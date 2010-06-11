%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%% Purpose : Updates variable list with variables depending on
%%%	      running Erlang system.

-module(ts_erl_config).


-export([variables/2]).

%% Returns a list of key, value pairs.

variables(Base0, OsType) ->
    Base1 = erl_include(Base0),
    Base2 = get_app_vars(fun erl_interface/2, Base1, OsType),
    Base3 = get_app_vars(fun ic/2, Base2, OsType),
    Base4 = get_app_vars(fun jinterface/2, Base3, OsType),
    Base5 = dl_vars(Base4, OsType),
    Base6 = emu_vars(Base5),
    Base7 = get_app_vars(fun ssl/2, Base6, OsType),
    Base8 = erts_lib(Base7, OsType),
    Base = separators(Base8, OsType),
    [{'EMULATOR', tl(code:objfile_extension())},
     {emu_threads, atom_to_list(erlang:system_info(threads))},
     {type_marker, case is_debug_build() of
		       true ->
			   ".debug";
		       false ->
			   ""
		   end}
     | Base].

get_app_vars(AppFun, Vars, OsType) ->
    case catch AppFun(Vars,OsType) of
	Res when is_list(Res) ->
	    Res;
	{cannot_find_app, App} ->
	    io:format("* WARNING: Cannot find ~p!~n", [App]),
	    Vars;
	{'EXIT', Reason} ->
	    exit(Reason);
	Garbage ->
	    exit({unexpected_internal_error, Garbage})
    end.

dl_vars(Vars, _) ->
    ShlibRules0 = ".SUFFIXES:\n" ++
 	".SUFFIXES: @dll@ @obj@ .c\n\n" ++
 	".c@dll@:\n" ++
 	"\t@CC@ -c @SHLIB_CFLAGS@ $(SHLIB_EXTRA_CFLAGS) -I@erl_include@ @DEFS@ $<\n" ++
	"\t@SHLIB_LD@ @CROSSLDFLAGS@ @SHLIB_LDFLAGS@ $(SHLIB_EXTRA_LDFLAGS) -o $@ $*@obj@ @SHLIB_LDLIBS@ $(SHLIB_EXTRA_LDLIBS)",

    ShlibRules = ts_lib:subst(ShlibRules0, Vars),
    [{'SHLIB_RULES', ShlibRules}|Vars].

erts_lib_name(multi_threaded, {win32, V}) ->
    link_library("erts_MD" ++ case is_debug_build() of
				  true -> "d";
				  false -> ""
			      end,
		 {win32, V});
erts_lib_name(single_threaded, {win32, V}) ->
    link_library("erts_ML" ++ case is_debug_build() of
				  true -> "d";
				  false -> ""
			      end,
		 {win32, V});
erts_lib_name(multi_threaded, OsType) ->
    link_library("erts_r", OsType);
erts_lib_name(single_threaded, OsType) ->
    link_library("erts", OsType).

erts_lib(Vars,OsType) ->
    {ErtsLibInclude,
     ErtsLibIncludeGenerated,
     ErtsLibIncludeInternal,
     ErtsLibIncludeInternalGenerated,
     ErtsLibPath,
     ErtsLibInternalPath}
	= case erl_root(Vars) of
	      {installed, _Root} ->
		  Erts = lib_dir(Vars, erts),
		  ErtsInclude = filename:join([Erts, "include"]),
		  ErtsIncludeInternal = filename:join([ErtsInclude, "internal"]),
		  ErtsLib = filename:join([Erts, "lib"]),
		  ErtsLibInternal = filename:join([ErtsLib, "internal"]),
		  {ErtsInclude,
		   ErtsInclude,
		   ErtsIncludeInternal,
		   ErtsIncludeInternal,
		   ErtsLib,
		   ErtsLibInternal};
	      {Type, Root, Target} when Type == clearcase; Type == srctree ->
		  Erts = filename:join([Root, "erts"]),
		  ErtsInclude = filename:join([Erts, "include"]),
		  ErtsIncludeTarget = filename:join([ErtsInclude, Target]),
		  ErtsIncludeInternal = filename:join([ErtsInclude,
						       "internal"]),
		  ErtsIncludeInternalTarget = filename:join([ErtsIncludeInternal,
							     Target]),
		  ErtsLib = filename:join([Erts, "lib", Target]),
		  ErtsLibInternal = filename:join([Erts,
						   "lib",
						   "internal",
						   Target]),
		  {ErtsInclude,
		   ErtsIncludeTarget,
		   ErtsIncludeInternal,
		   ErtsIncludeInternalTarget,
		   ErtsLib,
		   ErtsLibInternal}
	  end,
    [{erts_lib_include,
      filename:nativename(ErtsLibInclude)},
     {erts_lib_include_generated,
      filename:nativename(ErtsLibIncludeGenerated)},
     {erts_lib_include_internal,
      filename:nativename(ErtsLibIncludeInternal)},
     {erts_lib_include_internal_generated,
      filename:nativename(ErtsLibIncludeInternalGenerated)},
     {erts_lib_path, filename:nativename(ErtsLibPath)},
     {erts_lib_internal_path, filename:nativename(ErtsLibInternalPath)},
     {erts_lib_multi_threaded, erts_lib_name(multi_threaded, OsType)},
     {erts_lib_single_threaded, erts_lib_name(single_threaded, OsType)}
     | Vars].

erl_include(Vars) ->
    Include = 
	case erl_root(Vars) of
	    {installed, Root} ->
		filename:join([Root, "usr", "include"]);
	    {Type, Root, Target} when Type == clearcase; Type == srctree ->
		filename:join([Root, "erts", "emulator", "beam"])
		    ++ " -I" ++ filename:join([Root, "erts", "emulator"])
		    ++ system_include(Root, Vars)
		    ++ " -I" ++ filename:join([Root, "erts", "include"])
		    ++ " -I" ++ filename:join([Root, "erts", "include", Target])
	end,
    [{erl_include, filename:nativename(Include)}|Vars].


system_include(Root, Vars) ->
    SysDir =
	case ts_lib:var(os, Vars) of
	    "Windows" ++ _T -> "sys/win32";
	    "VxWorks" -> "sys.vxworks";
	    _ -> "sys/unix"
	end,
    " -I" ++ filename:nativename(filename:join([Root, "erts", "emulator", SysDir])).

erl_interface(Vars,OsType) ->
    {Incl, {LibPath, MkIncl}} =
	case lib_dir(Vars, erl_interface) of
	    {error, bad_name} ->
		throw({cannot_find_app, erl_interface});
	    Dir ->
		{filename:join(Dir, "include"),
		 case erl_root(Vars) of
		     {installed, _Root} ->
			 {filename:join(Dir, "lib"),
			  filename:join(Dir, "src")};
		     {srctree, _Root, _Target} when OsType =:= vxworks ->
			 {filename:join(Dir, "lib"),
			  filename:join([Dir, "src"])};
		     {Type, _Root, Target} when Type == clearcase; Type == srctree ->
			 {filename:join([Dir, "obj", Target]),
			  filename:join([Dir, "src", Target])}
		 end}
	end,
    Lib = link_library("erl_interface",OsType),
    Lib1 = link_library("ei",OsType),
    {LibDrv, Lib1Drv} =
	case erlang:system_info(threads) of
	    false ->
		case OsType of
		    {unix,_} ->
			{link_library("erl_interface_st",OsType),
			 link_library("ei_st",OsType)};
		    _ ->
			{Lib, Lib1}
		end;
	    true ->
		case OsType of
		    {win32, _} ->
			{link_library("erl_interface_md",OsType),
			 link_library("ei_md",OsType)};
		    _ ->
			{Lib, Lib1}
		end
	end,
    ThreadLib = case OsType of
		    % FIXME: FreeBSD uses gcc flag '-pthread' or linking with
		    % "libc_r". So it has to be last of libs. This is an
		    % temporary solution, should be configured elsewhere.
		    
		    % This temporary solution have now failed!
		    % A new temporary solution is installed ...
		    % {unix,freebsd} -> "-lc_r";
		    {unix,freebsd} ->
			"-lpthread";
		    {unix,_} ->
			"-lpthread";
		    _ -> 
			"" % VxWorks
		end,
    CrossCompile = case OsType of
		       vxworks -> "true";
		       ose ->     "true";
		       _ ->       "false"
		   end,
    [{erl_interface_libpath, filename:nativename(LibPath)},
     {erl_interface_sock_libs, sock_libraries(OsType)},
     {erl_interface_lib, Lib},
     {erl_interface_eilib, Lib1},
     {erl_interface_lib_drv, LibDrv},
     {erl_interface_eilib_drv, Lib1Drv},
     {erl_interface_threadlib, ThreadLib},
     {erl_interface_include, filename:nativename(Incl)},
     {erl_interface_mk_include, filename:nativename(MkIncl)},
     {erl_interface_cross_compile, CrossCompile} | Vars].

ic(Vars, OsType) ->
    {ClassPath, LibPath, Incl} =
	case lib_dir(Vars, ic) of
	    {error, bad_name} ->
		throw({cannot_find_app, ic});
	    Dir ->
		{filename:join([Dir, "priv", "ic.jar"]),
		 case erl_root(Vars) of
		     {installed, _Root} ->
			 filename:join([Dir, "priv", "lib"]);
		     {Type, _Root, Target} when Type == clearcase; Type == srctree ->
			 filename:join([Dir, "priv", "lib", Target])
		 end,
		 filename:join(Dir, "include")}
	end,
    [{ic_classpath, filename:nativename(ClassPath)},
     {ic_libpath, filename:nativename(LibPath)},
     {ic_lib, link_library("ic", OsType)},
     {ic_include_path, filename:nativename(Incl)}|Vars].

jinterface(Vars, _OsType) ->
    ClassPath =
	case lib_dir(Vars, jinterface) of
	    {error, bad_name} ->
		throw({cannot_find_app, jinterface});
	    Dir ->
		filename:join([Dir, "priv", "OtpErlang.jar"])
	end,
    [{jinterface_classpath, filename:nativename(ClassPath)}|Vars].

%% Unused!
% ig_vars(Vars) ->
%     {Lib0, Incl} = 
% 	case erl_root(Vars) of
% 	    {installed, Root} ->
% 		Base = filename:join([Root, "usr"]),
% 		{filename:join([Base, "lib"]), 
% 		 filename:join([Base, "include"])};
% 	    {Type, Root, Target} when Type == clearcase; Type == srctree ->
% 		{filename:join([Root, "lib", "ig", "obj", Target]),
% 		 filename:join([Root, "lib", "ig", "include"])}
% 	end,
%     [{ig_libdir, filename:nativename(Lib0)},
%      {ig_include, filename:nativename(Incl)}|Vars].

lib_dir(Vars, Lib) ->
    LibLibDir = case Lib of
		    erts ->
			filename:join([code:root_dir(),
				       "erts-" ++ erlang:system_info(version)]);
		    _ ->
			code:lib_dir(Lib)
		end,
    case {get_var(crossroot, Vars), LibLibDir} of
	{{error, _}, _} ->			%no crossroot
	    LibLibDir;
	{_, {error, _}} ->			%no lib
	    LibLibDir;
	{CrossRoot, _} ->
	    %% XXX: Ugly. So ugly I won't comment it
	    %% /Patrik
	    CLibDirList = case Lib of
			      erts ->
				  [CrossRoot, "erts"];
			      _ ->
				  [CrossRoot, "lib", atom_to_list(Lib)]
			  end,
	    CLibDir = filename:join(CLibDirList),
	    Cmd = "ls -d " ++ CLibDir ++ "*",
	    XLibDir = lists:last(string:tokens(os:cmd(Cmd),"\n")),
	    case file:list_dir(XLibDir) of
		{error, enoent} ->
		    [];
		_ ->
		    XLibDir
	    end
    end.

erl_root(Vars) ->
    Root = code:root_dir(),
    case ts_lib:erlang_type() of
	{clearcase, _Version} ->
	    Target = get_var(target, Vars),
	    {clearcase, Root, Target};
	{srctree, _Version} ->
	    Target = get_var(target, Vars),
	    {srctree, Root, Target};
	{_, _Version} ->
	    case get_var(crossroot,Vars) of
		{error, notfound} ->
		    {installed, Root};
		CrossRoot ->
		    {installed, CrossRoot}
	    end
    end.


get_var(Key, Vars) ->
    case lists:keysearch(Key, 1, Vars) of
	{value, {Key, Value}} ->
	    Value;
	_ ->
	    {error, notfound}
    end.


sock_libraries({win32, _}) ->
    "ws2_32.lib";
sock_libraries({unix, _}) ->
    "";	% Included in general libraries if needed.
sock_libraries(vxworks) ->
    "";
sock_libraries(ose) ->
    "".

link_library(LibName,{win32, _}) ->
    LibName ++ ".lib";
link_library(LibName,{unix, _}) ->
    "lib" ++ LibName ++ ".a";
link_library(LibName,vxworks) ->
    "lib" ++ LibName ++ ".a";
link_library(_LibName,ose) ->
    "";
link_library(_LibName,_Other) ->
    exit({link_library, not_supported}).

%% Returns emulator specific variables.
emu_vars(Vars) ->
    [{is_source_build, is_source_build()},
     {erl_name, atom_to_list(lib:progname())}|Vars].

is_source_build() ->
    string:str(erlang:system_info(system_version), "[source]") > 0.

is_debug_build() ->
    case catch string:str(erlang:system_info(system_version), "debug") of
	Int when is_integer(Int), Int > 0 ->
	    true;
	_ ->
	    false
    end.
%%
%% ssl_libdir
%%
ssl(Vars, _OsType) ->
    case lib_dir(Vars, ssl) of
	{error, bad_name} ->
	    throw({cannot_find_app, ssl});
	Dir ->
	    [{ssl_libdir, filename:nativename(Dir)}| Vars]
    end.

separators(Vars, {win32,_}) ->
    [{'DS',"\\"},{'PS',";"}|Vars];
separators(Vars, _) ->
    [{'DS',"/"},{'PS',":"}|Vars].
