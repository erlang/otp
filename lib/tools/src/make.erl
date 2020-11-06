%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
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
%% Purpose : Basic make facility

%% Compares date stamps of .erl and Object files - recompiles when
%% necessary.
%% Files to be checked are contained in a file 'Emakefile' 
%% If Emakefile is missing the current directory is used.
-module(make).

-export([all_or_nothing/0,all/0,all/1,files/1,files/2]).

-include_lib("kernel/include/file.hrl").

-define(MakeOpts,[noexec,load,netload,noload,emake]).

all_or_nothing() ->
    case all() of
        up_to_date ->
            up_to_date;
        error ->
            halt(1)
    end.

-spec all() -> 'up_to_date' | 'error'.

all() ->
    all([]).

-spec all(Options) -> 'up_to_date' | 'error' when
      Options :: [Option],
      Option :: 'noexec'
              | 'load'
              | 'netload'
              | {'emake', Emake}
              | compile:option(),
      Emake :: [EmakeElement],
      EmakeElement :: Modules | {Modules, [compile:option()]},
      Modules :: atom() | [atom()].

all(Options) ->
    run_emake(undefined, Options).

-spec files(ModFiles) -> 'up_to_date' | 'error' when
      ModFiles :: [(Module :: module()) | (File :: file:filename())].

files(Fs) ->
    files(Fs, []).

-spec files(ModFiles, Options) -> 'up_to_date' | 'error' when
      ModFiles :: [(Module :: module()) | (File :: file:filename())],
      Options :: [Option],
      Option :: 'noexec'
              | 'load'
              | 'netload'
              | compile:option().

files(Fs0, Options) ->
    Fs = [filename:rootname(F,".erl") || F <- Fs0],
    run_emake(Fs, Options).

run_emake(Mods, Options) ->
    {MakeOpts,CompileOpts} = sort_options(Options,[],[]),
    Emake = get_emake(Options),
    case normalize_emake(Emake, Mods, CompileOpts) of
	Files when is_list(Files) ->
	    do_make_files(Files,MakeOpts);
	error ->
	    error
    end.

do_make_files(Fs, Opts) ->
    process(Fs, lists:member(noexec, Opts), load_opt(Opts)).

sort_options([{emake, _}=H|T],Make,Comp) ->
  sort_options(T,[H|Make],Comp);

sort_options([H|T],Make,Comp) ->
    case lists:member(H,?MakeOpts) of
	true ->
	    sort_options(T,[H|Make],Comp);
	false ->
	    sort_options(T,Make,[H|Comp])
    end;
sort_options([],Make,Comp) ->
    {Make,lists:reverse(Comp)}.

%%% Reads the given Emakefile and returns a list of tuples: {Mods,Opts}
%%% Mods is a list of module names (strings)
%%% Opts is a list of options to be used when compiling Mods
%%%
%%% Emakefile can contain elements like this:
%%% Mod.
%%% {Mod,Opts}.
%%% Mod is a module name which might include '*' as wildcard
%%% or a list of such module names
%%%
%%% These elements are converted to [{ModList,OptList},...]
%%% ModList is a list of modulenames (strings)

normalize_emake(EmakeRaw, Mods, Opts) ->
    case EmakeRaw of
	{ok, Emake} when Mods =:= undefined ->
	    transform(Emake,Opts,[],[]);
	{ok, Emake} when is_list(Mods) ->
	    ModsOpts = transform(Emake,Opts,[],[]),
	    ModStrings = [coerce_2_list(M) || M <- Mods],
	    get_opts_from_emakefile(ModsOpts,ModStrings,Opts,[]); 
	{error,enoent} when Mods =:= undefined ->
	    %% No Emakefile found - return all modules in current 
	    %% directory and the options given at command line
	    CwdMods = [filename:rootname(F) ||  F <- filelib:wildcard("*.erl")],
	    [{CwdMods, Opts}];
	{error,enoent} when is_list(Mods) ->
	    [{Mods, Opts}];
	{error, Error} ->
	    io:format("make: Trouble reading 'Emakefile':~n~tp~n",[Error]),
	    error
    end.

get_emake(Opts) ->
    case proplists:get_value(emake, Opts, false) of
	false ->
	    file:consult('Emakefile');
	OptsEmake ->
	    {ok, OptsEmake}
    end.

transform([{Mod,ModOpts}|Emake],Opts,Files,Already) ->
    case expand(Mod,Already) of
	[] -> 
	    transform(Emake,Opts,Files,Already);
	Mods -> 
	    transform(Emake,Opts,[{Mods,ModOpts++Opts}|Files],Mods++Already)
    end;
transform([Mod|Emake],Opts,Files,Already) ->
    case expand(Mod,Already) of
	[] -> 
	    transform(Emake,Opts,Files,Already);
	Mods ->
	    transform(Emake,Opts,[{Mods,Opts}|Files],Mods++Already)
    end;
transform([],_Opts,Files,_Already) ->
    lists:reverse(Files).

expand(Mod,Already) when is_atom(Mod) ->
    expand(atom_to_list(Mod),Already);
expand(Mods,Already) when is_list(Mods), not is_integer(hd(Mods)) ->
    lists:concat([expand(Mod,Already) || Mod <- Mods]);
expand(Mod,Already) ->
    case lists:member($*,Mod) of
	true -> 
	    Fun = fun(F,Acc) -> 
			  M = filename:rootname(F),
			  case lists:member(M,Already) of
			      true -> Acc;
			      false -> [M|Acc]
			  end
		  end,
	    lists:foldl(Fun, [], filelib:wildcard(Mod++".erl"));
	false ->
	    Mod2 = filename:rootname(Mod, ".erl"),
	    case lists:member(Mod2,Already) of
		true -> [];
		false -> [Mod2]
	    end
    end.

%%% Reads the given Emake to see if there are any specific compile 
%%% options given for the modules.

get_opts_from_emakefile([{MakefileMods,O}|Rest],Mods,Opts,Result) ->
    case members(Mods,MakefileMods,[],Mods) of
	{[],_} -> 
	    get_opts_from_emakefile(Rest,Mods,Opts,Result);
	{I,RestOfMods} ->
	    get_opts_from_emakefile(Rest,RestOfMods,Opts,[{I,O}|Result])
    end;
get_opts_from_emakefile([],[],_Opts,Result) ->
    Result;
get_opts_from_emakefile([],RestOfMods,Opts,Result) ->
    [{RestOfMods,Opts}|Result].
    
members([H|T],MakefileMods,I,Rest) ->
    case lists:member(H,MakefileMods) of
	true ->
	    members(T,MakefileMods,[H|I],lists:delete(H,Rest));
	false ->
	    members(T,MakefileMods,I,Rest)
    end;
members([],_MakefileMods,I,Rest) ->
    {I,Rest}.


%% Any flags that are not recognixed as make flags are passed directly
%% to the compiler.
%% So for example make:all([load,debug_info]) will make everything
%% with the debug_info flag and load it.
	
load_opt(Opts) ->
    case lists:member(netload,Opts) of
	true -> 
	    netload;
	false ->
	    case lists:member(load,Opts) of
		true ->
		    load;
		_ ->
		    noload
	    end
    end.


process([{[],_Opts}|Rest], NoExec, Load) ->
    process(Rest, NoExec, Load);
process([{[H|T],Opts}|Rest], NoExec, Load) ->
    case recompilep(coerce_2_list(H), NoExec, Load, Opts) of
	error ->
	    error;
	_ ->
	    process([{T,Opts}|Rest], NoExec, Load)
    end;
process([], _NoExec, _Load) ->
    up_to_date.

recompilep(File, NoExec, Load, Opts) ->
    ObjName = lists:append(filename:basename(File),
			   code:objfile_extension()),
    ObjFile = case lists:keysearch(outdir,1,Opts) of
		  {value,{outdir,OutDir}} ->
		      filename:join(coerce_2_list(OutDir),ObjName);
		  false ->
		      ObjName
	      end,
    case exists(ObjFile) of
	true ->
	    recompilep1(File, NoExec, Load, Opts, ObjFile);
	false ->
	    recompile(File, NoExec, Load, Opts)
    end.
 
recompilep1(File, NoExec, Load, Opts, ObjFile) ->
    {ok, Erl} = file:read_file_info(lists:append(File, ".erl")),
    {ok, Obj} = file:read_file_info(ObjFile),
	 recompilep1(Erl, Obj, File, NoExec, Load, Opts).

recompilep1(#file_info{mtime=Te},
	    #file_info{mtime=To}, File, NoExec, Load, Opts) when Te>To ->
    recompile(File, NoExec, Load, Opts);
recompilep1(_Erl, #file_info{mtime=To}, File, NoExec, Load, Opts) ->
    recompile2(To, File, NoExec, Load, Opts).

%% recompile2(ObjMTime, File, NoExec, Load, Opts)
%% Check if file is of a later date than include files.
recompile2(ObjMTime, File, NoExec, Load, Opts) ->
    IncludePath = include_opt(Opts),
    case check_includes(lists:append(File, ".erl"), IncludePath, ObjMTime) of
	true ->
	    recompile(File, NoExec, Load, Opts);
	false ->
	    false
    end.

include_opt([{i,Path}|Rest]) ->
    [Path|include_opt(Rest)];
include_opt([_First|Rest]) ->
    include_opt(Rest);
include_opt([]) ->
    [].

%% recompile(File, NoExec, Load, Opts)
%% Actually recompile and load the file, depending on the flags.
%% Where load can be netload | load | noload

recompile(File, true, _Load, _Opts) ->
    io:format("Out of date: ~ts\n",[File]);
recompile(File, false, Load, Opts) ->
    io:format("Recompile: ~ts\n",[File]),
    case compile:file(File, [report_errors, report_warnings |Opts]) of
        Ok when is_tuple(Ok), element(1,Ok)==ok ->
            maybe_load(element(2,Ok), Load, Opts);
        _Error ->
            error
    end.

maybe_load(_Mod, noload, _Opts) ->
    ok;
maybe_load(Mod, Load, Opts) ->
    %% We have compiled File with options Opts. Find out where the
    %% output file went to, and load it.
    case compile:output_generated(Opts) of
	true ->
            Dir = proplists:get_value(outdir,Opts,"."),
            do_load(Dir, Mod, Load);
	false ->
	    io:format("** Warning: No object file created - nothing loaded **~n"),
	    ok
    end.

do_load(Dir, Mod, load) ->
    code:purge(Mod),
    case code:load_abs(filename:join(Dir, Mod),Mod) of
        {module,Mod} ->
            {ok,Mod};
        Other ->
            Other
    end;
do_load(Dir, Mod, netload) ->
    Obj = atom_to_list(Mod) ++ code:objfile_extension(),
    Fname = filename:join(Dir, Obj),
    case file:read_file(Fname) of
        {ok,Bin} ->
            rpc:eval_everywhere(code,load_binary,[Mod,Fname,Bin]),
            {ok,Mod};
        Other ->
            Other
    end.

exists(File) ->
    case file:read_file_info(File) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

coerce_2_list(X) when is_atom(X) ->
    atom_to_list(X);
coerce_2_list(X) ->
    X.

%%% If an include file is found with a modification
%%% time larger than the modification time of the object
%%% file, return true. Otherwise return false.
check_includes(File, IncludePath, ObjMTime) ->
    {ok,Cwd} = file:get_cwd(),
    Path = [Cwd,filename:dirname(File)|IncludePath],
    case epp:open(File, Path, []) of
	{ok, Epp} ->
	    check_includes2(Epp, File, ObjMTime);
	_Error ->
	    false
    end.
    
check_includes2(Epp, File, ObjMTime) ->
    A1 = erl_anno:new(1),
    case epp:parse_erl_form(Epp) of
	{ok, {attribute, A1, file, {File, A1}}} ->
	    check_includes2(Epp, File, ObjMTime);
	{ok, {attribute, A1, file, {IncFile, A1}}} ->
	    case file:read_file_info(IncFile) of
		{ok, #file_info{mtime=MTime}} when MTime>ObjMTime ->
		    epp:close(Epp),
		    true;
		_ ->
		    check_includes2(Epp, File, ObjMTime)
	    end;
	{ok, _} ->
	    check_includes2(Epp, File, ObjMTime);
	{eof, _} ->
	    epp:close(Epp),
	    false;
	{error, _Error} ->
	    check_includes2(Epp, File, ObjMTime);
	{warning, _Warning} ->
	    check_includes2(Epp, File, ObjMTime)
    end.
