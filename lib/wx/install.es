#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
%% Install wx to an erlang distribution.
%% 

-module('install_es'). %% Temp workaround for buggy escript
-mode(compile).

usage() ->
    io:format("install [Opts] [InstallDir]~n",[]),
    io:format("  -v verbose~n",[]),
    io:format("  -q quiet and direct no questions~n",[]),
    io:format("  --create_release To create a release~n~n",[]),
    ok.

main([]) ->
    IDir = filename:join(code:root_dir(),"lib"),
    install(IDir),
    ok;
main(Opts) ->
    IDir = options(Opts),
    install(IDir),
    ok.

install_files() ->
    [{[],          {files, ["README","TODO", "Notes", "license.terms"]}}, 
     {[src],       {list, "*.?rl"}}, 
     {[include],   {list, "*.hrl"}}, 
     {[src,gen],   {list, "*.?rl"}},
     {[test],      {list, "*.?rl"}},
     {[test],      {list, "*.bmp"}},
     {[test],      {files, ["wxt", "Readme"]}},
     {[samples],   {files, ["sample.xpm"]}},
     {[samples,menu], {files, ["copy.xpm"]}},
     {[samples,'*'],  {list, "*.?rl"}},
     {[demos,'*'],    {list, "*.?rl"}},
     {[demos,xrc,rc], {list, "*"}},
     {[doc,html],     {files, ["edoc-info", "erlang.png", "stylesheet.css"]}}
    ].

built_files(SrcDir, Type) ->
    Dirs = [{[doc],         {list, "*.html"}},
	    {[ebin],        {list, "*"}},
	    {[test],        {list, "*.beam"}},
	    {[demos,'*'],   {list, "*.beam"}},
	    {[samples,'*'], {list, "*.beam"}}],
    case Type of 
	install ->
	    [{[priv,'*'], {list, "*"}}|Dirs];
	_ -> %% Create Rel
	    Win32 = [{[priv,win32],  {list, "*"}}| Dirs],
	    case filelib:wildcard("i386-apple*", 
				  filename:join(SrcDir, "priv")) of
		[Mac] ->
		    [{[priv,list_to_atom(Mac)],  {list, "*"}}|Win32];
		[] ->
		    io:format("WARNING: Missing Mac driver!~n",[]),
		    Win32
	    end
    end.

release_files() ->
    [{[],          {files, ["configure.in","Makefile","wxwin.m4", "config.mk.in", "vsn.mk"]}},
     {[],          {files, ["configure", "install.es"]}},
     {[autoconf],  {files, ["config.guess","config.sub","install-sh"]}},
     {[src],       {files, ["Makefile"]}},
     {[c_src],     {files, ["Makefile.in"]}},
     {[c_src],     {list, "*.c*"}},
     {[c_src],     {list, "*.h"}},
     {[c_src,gen], {list, "*.cpp"}},
     {[c_src,gen], {list, "*.h"}},
     {[doc],       {files, ["overview.edoc"]}},
     {[doc,src],   {files, ["Makefile"]}},
     {[test],      {files, ["Makefile"]}},
%%      {[demos],     {files, ["Makefile"]}},
%%      {[demos,'*'], {files, ["Makefile"]}},
%%      {[samples],   {files, ["Makefile"]}},
%%      {[samples,'*'], {files, ["Makefile"]}},
     {[api_gen],   {list, "*.?rl"}},
     {[api_gen],   {list, "*.conf"}},
     {[api_gen],   {files, ["Makefile", "README"]}},
     {[api_gen, wx_extra], {list, "*"}}
    ].

options(["-v"|Os]) ->
    put(verbose,true),
    options(Os);
options(["-q"|Os]) ->
    put(quiet,true),
    options(Os);
options(["--help"|_]) ->
    usage(),
    halt(0);
options(["-h"|_]) ->
    usage(),
    halt(0);
options(["--create_release"|Os]) ->
    put(create_release,true),
    options(Os);
options(["-" ++_ |_Os]) ->
    usage(),
    halt(1);
options([Dir]) -> Dir;
options([]) -> 
    filename:join(code:root_dir(),"lib").
    
install(Dir) ->
    try
	case get(create_release) of
	    true ->  create_release();
	    _ ->     install2(Dir)
	end
    catch E:R ->
	    io:format("Error ~p:~p in ~n ~p~n",[E,R, erlang:get_stacktrace()])
    end.

install2(IDir) ->
    SrcD = get_src_dir(),
    Ver  = get_version(SrcD),
    case get(quiet) of
	true -> 
	    copy_files(SrcD, filename:append(IDir,Ver)),
	    ok;
	_ ->
	    io:format("Installing ~p~n From ~p to ~p ~n",[Ver,SrcD,IDir]),    
	    case is_ok('ok [y|n]? ',[y,n]) of
		y -> 
		    copy_files(SrcD, filename:append(IDir,Ver)),
		    io:format("Install complete~n",[]),
		    ok;
		n ->
		    usage(),
		    halt(1)
	    end
    end.

copy_files(FromD, ToD) ->
    Fs = install_files() ++ built_files(FromD, install),
    Copy = fun(File, From, To, Acc) ->
		   ensure_dir(To),
		   case file:copy(filename:join(From,File), 
				  filename:join(To,File)) of
		       {ok, _Bytes} ->
			   [File|Acc];
		       {error,_} ->
			   io:format("ERROR Could not install file: ~p in~n ~p~n",
				     [filename:join(From,File),
				      filename:join(To,File)]),
			   Acc
		   end
	   end,
    [expand_dirs(Dir,Files,FromD,ToD,Copy,[]) || {Dir,Files} <- Fs].

expand_dirs(['*'|Ds],Fs,From,To,Fun,Acc) ->
    All  = filelib:wildcard("[A-Za-z]*", From),
    Filter = fun(F) -> filelib:is_dir(filename:join(From,F)) end,
    Dirs = lists:filter(Filter, All),
    lists:foldl(fun(Dir,Nacc) -> 
			expand_dirs([Dir|Ds],Fs,From,To,Fun,Nacc) 
		end,
		Acc, Dirs);
expand_dirs([Dir|Ds],Fs,From,To,Fun,Acc) ->
    expand_dirs(Ds,Fs,filename:join(From,Dir),filename:join(To,Dir),Fun,Acc);
expand_dirs([],Fs,From,To,Fun,Acc) ->
    expand_files(Fs,From,To,Fun,Acc).

expand_files({files,Fs},From,To,Fun,Acc) ->
    case get(verbose) of
	true -> io:format(" Check dir ~p to ~p~n", [From,To]);
	_ -> ok
    end,
    expand_files2(Fs,From,To,Fun,Acc);
expand_files({list,WildCard},From,To,Fun,Acc) ->
    All = filelib:wildcard(WildCard, From),
    Filter = fun(F) -> filelib:is_regular(filename:join(From,F)) end,
    Fs  = lists:filter(Filter, All),
    case get(verbose) of
	true -> io:format(" Check dir ~p to ~p~n", [From,To]);
	_ -> ok
    end,
    expand_files2(Fs,From,To,Fun,Acc).

expand_files2(Fs,From,To,Fun,PrevAcc) ->
    lists:foldl(fun(File, Acc) -> Fun(File, From, To, Acc) end, PrevAcc, Fs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_release() ->
    All  = release_files() ++ install_files(),
    SrcD = get_src_dir(),
    Ver  = "wx-" ++ get_version(SrcD),
    io:format("Create release ~p~n From ~p ~n",[Ver,SrcD]),
    case is_ok('ok [y|n]? ',[y,n]) of
	y ->
	    List = fun(File, From, To, Acc) ->
			   case filelib:is_regular(filename:join(From,File)) of
			       true -> 
				   [{filename:join(To, File), 
				     filename:join(From,File)}|Acc];
			       false ->
				   io:format("Warning: File ~s/~s is missing~n",[From,File]),
				   Acc
			   end
		   end,
	    Expand = fun({Dir,Fs},Acc) -> 
			     expand_dirs(Dir,Fs,SrcD,Ver,List,Acc) 
		     end,
	    Files = lists:foldl(Expand,[], All),
	    ok = erl_tar:create(Ver ++ ".src.tar.gz", Files, [compressed]),
	    BuiltDir = built_files(SrcD, create_release),
	    BuiltFs = lists:foldl(Expand,[], BuiltDir),
	    ok = erl_tar:create(Ver ++ ".built.tar.gz", 
				Files ++ BuiltFs, 
				[compressed]),
	    ok;
	n ->
	    usage()
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_ok(Str, Answers) ->    
    {ok, [Ans]} = io:fread(Str, "~a"),
    case lists:member(Ans, Answers) of
	true -> Ans;
	false ->
	    io:format("Please answer ~p~n",[Answers]),
	    is_ok(Str,Answers)
    end.


ensure_dir(To) ->
    case filelib:ensure_dir(filename:join(To, "dummy")) of
	ok -> ok;
	Error -> 
	    io:format("Error ~p in ~p ~n",[Error, To]),
	    halt(1)   
    end.

get_version(Dir) ->
    %% Have we installed this from a release 
    %% get the directory name
    DateVariant = 
	fun() ->
		{_,Month,Day} = erlang:date(),
		Date = io_lib:format("~.2.0w~.2.0w",[Month,Day]),
		{ok, Bin} = file:read_file("vsn.mk"),
		Opt = [{capture, all_but_first, list}],
		case re:run(Bin, "WX_VSN\s*=\s*(.*)", Opt) of
		    {match, [Ver]} ->
			lists:flatten([Ver,"."|Date]);
		    _ ->
			lists:flatten(["wx-0.98."|Date])
		end
	end,
    case Dir of
	"." -> 
	    %% Installing from src without a release
	    DateVariant();
	Dir ->	    
	    Base = filename:basename(Dir),
	    case string:tokens(Base, "-.") of
		["wx", Ma, Mi|_] 
		when is_integer(Ma),is_integer(Mi) ->
		    Base;
		_ ->
		    DateVariant()
	    end
    end.

%% Get src dir of installation
get_src_dir() ->
    Escript = escript:script_name(),
    Abs = filename:dirname(filename:absname(Escript)),
    Test = filename:join(Abs, "README"),
    case file:read_file(Test) of
	{ok, _} -> ok;
	{error, enoent} ->
	    io:format("Error couldn't read ~s ~n",[Test]),
	    halt(1)
    end,
    Abs.


