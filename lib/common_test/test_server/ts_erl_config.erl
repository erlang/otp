%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
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
    Base5 = dl_vars(Base4, Base3, OsType),
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

dl_vars(Vars, Base3, OsType) ->
    ShlibRules0 = ".SUFFIXES:\n" ++
 	".SUFFIXES: @dll@ @obj@ .c\n\n" ++
 	".c@dll@:\n" ++
 	"\t@CC@ -c @SHLIB_CFLAGS@ $(SHLIB_EXTRA_CFLAGS) -I@erl_include@ @DEFS@ $<\n" ++
	"\t@SHLIB_LD@ @CROSSLDFLAGS@ @SHLIB_LDFLAGS@ $(SHLIB_EXTRA_LDFLAGS) -o $@ $*@obj@ @SHLIB_LDLIBS@ $(SHLIB_EXTRA_LDLIBS)",

    ShlibRules = ts_lib:subst(ShlibRules0, Vars),
    case get_app_vars2(fun jinterface/2, Base3, OsType) of
	{App, not_found} ->
	    [{'SHLIB_RULES', ShlibRules}, {App, "not_found"}|Vars];
	_ ->
	    [{'SHLIB_RULES', ShlibRules}|Vars]
    end.
get_app_vars2(AppFun, Vars, OsType) ->
    case catch AppFun(Vars,OsType) of
	Res when is_list(Res) ->
	    {jinterface, ok};
	{cannot_find_app, App} ->
	    {App, not_found};
	{'EXIT', Reason} ->
	    exit(Reason);
	Garbage ->
	    exit({unexpected_internal_error, Garbage})
    end.

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
     ErtsLibInternalPath,
     ErtsLibEthreadMake,
     ErtsLibInternalMake
    }
	= case erl_root(Vars) of
	      {installed, _Root} ->
		  Erts = lib_dir(Vars, erts),
		  ErtsInclude = filename:join([Erts, "include"]),
		  ErtsIncludeInternal = filename:join([ErtsInclude, "internal"]),
		  ErtsLib = filename:join([Erts, "lib"]),
		  ErtsLibInternal = filename:join([ErtsLib, "internal"]),
		  ErtsEthreadMake = filename:join([ErtsIncludeInternal, "ethread.mk"]),
		  ErtsInternalMake = filename:join([ErtsIncludeInternal, "erts_internal.mk"]),

		  {ErtsInclude,
		   ErtsInclude,
		   ErtsIncludeInternal,
		   ErtsIncludeInternal,
		   ErtsLib,
		   ErtsLibInternal,
		   ErtsEthreadMake,
		   ErtsInternalMake};
	      {srctree, Root, Target} ->
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
		  ErtsEthreadMake = filename:join([ErtsIncludeInternalTarget, "ethread.mk"]),
		  ErtsInternalMake = filename:join([ErtsIncludeInternalTarget, "erts_internal.mk"]),

		  {ErtsInclude,
		   ErtsIncludeTarget,
		   ErtsIncludeInternal,
		   ErtsIncludeInternalTarget,
		   ErtsLib,
		   ErtsLibInternal,
		   ErtsEthreadMake,
		   ErtsInternalMake}
	  end,
    [{erts_lib_include,
      quote(filename:nativename(ErtsLibInclude))},
     {erts_lib_include_generated,
      quote(filename:nativename(ErtsLibIncludeGenerated))},
     {erts_lib_include_internal,
      quote(filename:nativename(ErtsLibIncludeInternal))},
     {erts_lib_include_internal_generated,
      quote(filename:nativename(ErtsLibIncludeInternalGenerated))},
     {erts_lib_path, quote(filename:nativename(ErtsLibPath))},
     {erts_lib_internal_path, quote(filename:nativename(ErtsLibInternalPath))},
     {erts_lib_multi_threaded, erts_lib_name(multi_threaded, OsType)},
     {erts_lib_single_threaded, erts_lib_name(single_threaded, OsType)},
     {erts_lib_make_ethread, quote(ErtsLibEthreadMake)},
     {erts_lib_make_internal, quote(ErtsLibInternalMake)}
     | Vars].

erl_include(Vars) ->
    Include = 
	case erl_root(Vars) of
	    {installed, Root} ->
		quote(filename:join([Root, "usr", "include"]));
	    {srctree, Root, Target} ->
		quote(filename:join([Root, "erts", "emulator", "beam"]))
		    ++ " -I" ++ quote(filename:join([Root, "erts", "emulator"]))
		    ++ system_include(Root, Vars)
		    ++ " -I" ++ quote(filename:join([Root, "erts", "include"]))
		    ++ " -I" ++ quote(filename:join([Root, "erts", "include", Target]))
	end,
    [{erl_include, filename:nativename(Include)}|Vars].


system_include(Root, Vars) ->
    SysDir =
	case ts_lib:var(os, Vars) of
	    "Windows" ++ _T -> "sys/win32";
	    _ -> "sys/unix"
	end,
    " -I" ++ quote(filename:nativename(filename:join([Root, "erts", "emulator", SysDir]))).

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
			  filename:join([Dir, "src", "eidefs.mk"])};
		     {srctree, _Root, Target} ->
                         Obj = case is_debug_build() of
                                   true -> "obj.debug";
                                   false -> "obj"
                               end,
			 {filename:join([Dir, Obj, Target]),
			  filename:join([Dir, "src", Target, "eidefs.mk"])}
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
			""
		end,
    [{erl_interface_libpath, quote(filename:nativename(LibPath))},
     {erl_interface_sock_libs, sock_libraries(OsType)},
     {erl_interface_lib, quote(filename:join(LibPath, Lib))},
     {erl_interface_eilib, quote(filename:join(LibPath, Lib1))},
     {erl_interface_lib_drv, quote(filename:join(LibPath, LibDrv))},
     {erl_interface_eilib_drv, quote(filename:join(LibPath, Lib1Drv))},
     {erl_interface_threadlib, ThreadLib},
     {erl_interface_include, quote(filename:nativename(Incl))},
     {erl_interface_mk_include, quote(filename:nativename(MkIncl))}
     | Vars].

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
		     {srctree, _Root, Target} ->
			 filename:join([Dir, "priv", "lib", Target])
		 end,
		 filename:join(Dir, "include")}
	end,
    [{ic_classpath, quote(filename:nativename(ClassPath))},
     {ic_libpath, quote(filename:nativename(LibPath))},
     {ic_lib, quote(filename:join(filename:nativename(LibPath),link_library("ic", OsType)))},
     {ic_include_path, quote(filename:nativename(Incl))}|Vars].

jinterface(Vars, _OsType) ->
    ClassPath =
	case lib_dir(Vars, jinterface) of
	    {error, bad_name} ->
		throw({cannot_find_app, jinterface});
	    Dir ->
		filename:join([Dir, "priv", "OtpErlang.jar"])
	end,
    [{jinterface_classpath, quote(filename:nativename(ClassPath))}|Vars].

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
	    XLibDir = lists:last(string:lexemes(os:cmd(Cmd),"\n")),
	    case file:list_dir(XLibDir) of
		{error, enoent} ->
		    [];
		_ ->
		    XLibDir
	    end
    end.

erl_root(Vars) ->
    Root = case get_var(crossroot,Vars) of
	       {error, notfound} -> code:root_dir();
	       CrossRoot -> CrossRoot
	   end,
    case ts_lib:erlang_type(Root) of
	{srctree, _Version} ->
	    Target = get_var(target, Vars),
	    {srctree, Root, Target};
	{_, _Version} ->
	    {installed, Root}
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
    "".	% Included in general libraries if needed.

link_library(LibName,{win32, _}) ->
    LibName ++ ".lib";
link_library(LibName,{unix, _}) ->
    "lib" ++ LibName ++ ".a";
link_library(_LibName,_Other) ->
    exit({link_library, not_supported}).

%% Returns emulator specific variables.
emu_vars(Vars) ->
    [{is_source_build, is_source_build()},
     {erl_name, get_progname()}|Vars].

get_progname() ->
    case init:get_argument(progname) of
	{ok, [[Prog]]} ->
	    Prog;
	_Other ->
	    "no_prog_name"
    end.

is_source_build() ->
    string:find(erlang:system_info(system_version), "source") =/= nomatch.

is_debug_build() ->
    string:find(erlang:system_info(system_version), "debug") =/= nomatch.

%%
%% ssl_libdir
%%
ssl(Vars, _OsType) ->
    case lib_dir(Vars, ssl) of
	{error, bad_name} ->
	    throw({cannot_find_app, ssl});
	Dir ->
	    [{ssl_libdir, quote(filename:nativename(Dir))}| Vars]
    end.

separators(Vars, {win32,_}) ->
    [{'DS',"\\"},{'PS',";"}|Vars];
separators(Vars, _) ->
    [{'DS',"/"},{'PS',":"}|Vars].

quote(List) ->
    case lists:member($ , List) of
	false -> List;
	true -> make_quote(List)
    end.

make_quote(List) ->
    case os:type() of
	{win32, _} -> %% nmake"
	    [$"] ++ List ++ [$"];
	_ -> %% make
	    BackQuote = fun($ , Acc) -> [$\\ , $  |Acc];
			   (Char, Acc) -> [Char|Acc] end,
	    lists:foldr(BackQuote, [], List)
    end.
