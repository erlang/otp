%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
%%
%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the asn1 application
%%----------------------------------------------------------------------
-module(asn1_appup_test).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    Cases = 
	[
	 appup
	],
    {req, [], {conf, appup_init, Cases, appup_fin}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

appup_init(suite) -> [];
appup_init(doc) -> [];
appup_init(Config) when is_list(Config) ->
    AppFile   = file_name(asn1, ".app"),
    AppupFile = file_name(asn1, ".appup"),
    [{app_file, AppFile}, {appup_file, AppupFile}|Config].
    

file_name(App, Ext) ->
    LibDir = code:lib_dir(App),
    filename:join([LibDir, "ebin", atom_to_list(App) ++ Ext]).


appup_fin(suite) -> [];
appup_fin(doc) -> [];
appup_fin(Config) when is_list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

appup(suite) ->
    [];
appup(doc) ->
    "perform a simple check of the appup file";
appup(Config) when is_list(Config) ->
    AppupFile = key1search(appup_file, Config),
    AppFile   = key1search(app_file, Config),
    Modules   = modules(AppFile),
    check_appup(AppupFile, Modules).

modules(File) ->
    case file:consult(File) of
        {ok, [{application,asn1,Info}]} ->
            case lists:keysearch(modules,1,Info) of
                {value, {modules, Modules}} ->
                    Modules;
                false ->
                    fail({bad_appinfo, Info})
            end;
        Error ->
            fail({bad_appfile, Error})
    end.

    
check_appup(AppupFile, Modules) ->
    case file:consult(AppupFile) of
	{ok, [{V, UpFrom, DownTo}]} ->
	    io:format("V= ~p, UpFrom= ~p, DownTo= ~p, Modules= ~p~n",
		      [V, UpFrom, DownTo, Modules]),
	    check_appup(V, UpFrom, DownTo, Modules);
	Else ->
	    fail({bad_appupfile, Else})
    end.


check_appup(V, UpFrom, DownTo, Modules) ->
    check_version(V),
    check_depends(up,   UpFrom, Modules),
    check_depends(down, DownTo, Modules),
    ok.


check_depends(_, [], _) ->
    ok;
check_depends(UpDown, [Dep|Deps], Modules) ->
    check_depend(UpDown, Dep, Modules),
    check_depends(UpDown, Deps, Modules).


check_depend(up,I={add_application,_App},Modules) ->
    d("check_instructions(~w) -> entry with"
      "~n   Instruction:       ~p"
      "~n   Modules: ~p", [up,I , Modules]),
    ok;
check_depend(down,I={remove_application,_App},Modules) ->
    d("check_instructions(~w) -> entry with"
      "~n   Instruction:       ~p"
      "~n   Modules: ~p", [down,I , Modules]),
    ok;
check_depend(UpDown, {V, Instructions}, Modules) ->
    d("check_instructions(~w) -> entry with"
      "~n   V:       ~p"
      "~n   Modules: ~p", [UpDown, V, Modules]),
    check_version(V),
    case check_instructions(UpDown, 
			    Instructions, Instructions, [], [], Modules) of
	{_Good, []} ->
	    ok;
	{_, Bad} ->
	    fail({bad_instructions, Bad, UpDown})
    end.


check_instructions(_, [], _, Good, Bad, _) ->
    {lists:reverse(Good), lists:reverse(Bad)};
check_instructions(UpDown, [Instr|Instrs], AllInstr, Good, Bad, Modules) ->
    d("check_instructions(~w) -> entry with"
      "~n   Instr: ~p", [UpDown,Instr]),
    case (catch check_instruction(UpDown, Instr, AllInstr, Modules)) of
        ok ->
            check_instructions(UpDown, Instrs, AllInstr, 
			       [Instr|Good], Bad, Modules);
        {error, Reason} ->
	    d("check_instructions(~w) -> bad instruction: "
	      "~n   Reason: ~p", [UpDown,Reason]),
            check_instructions(UpDown, Instrs, AllInstr, Good, 
                               [{Instr, Reason}|Bad], Modules)
    end.

%% A new module is added
check_instruction(up, {add_module, Module}, _, Modules) 
  when is_atom(Module) ->
    d("check_instruction -> entry when up-add_module instruction with"
      "~n   Module: ~p", [Module]),
    check_module(Module, Modules);

%% An old module is re-added
check_instruction(down, {add_module, Module}, _, Modules) 
  when is_atom(Module) ->
    d("check_instruction -> entry when down-add_module instruction with"
      "~n   Module: ~p", [Module]),
    case (catch check_module(Module, Modules)) of
	{error, {unknown_module, Module, Modules}} ->
	    ok;
	ok ->
	    error({existing_readded_module, Module})
    end;

%% Removing a module on upgrade: 
%% - the module has been removed from the app-file.
%% - check that no module depends on this (removed) module
check_instruction(up, {remove, {Module, Pre, Post}}, _, Modules) 
  when is_atom(Module), is_atom(Pre), is_atom(Post) ->
    d("check_instruction -> entry when up-remove instruction with"
      "~n   Module: ~p"
      "~n   Pre:    ~p"
      "~n   Post:   ~p", [Module, Pre, Post]),
    case (catch check_module(Module, Modules)) of
	{error, {unknown_module, Module, Modules}} ->
	    check_purge(Pre),
	    check_purge(Post);
	ok ->
	    error({existing_removed_module, Module})
    end;

%% Removing a module on downgrade: the module exist
%% in the app-file.
check_instruction(down, {remove, {Module, Pre, Post}}, AllInstr, Modules) 
  when is_atom(Module), is_atom(Pre), is_atom(Post) ->
    d("check_instruction -> entry when down-remove instruction with"
      "~n   Module: ~p"
      "~n   Pre:    ~p"
      "~n   Post:   ~p", [Module, Pre, Post]),
    case (catch check_module(Module, Modules)) of
	ok ->
	    check_purge(Pre),
	    check_purge(Post),
	    check_no_remove_depends(Module, AllInstr);
	{error, {unknown_module, Module, Modules}} ->
	    error({nonexisting_removed_module, Module})
    end;

check_instruction(_, {load_module, Module, Pre, Post, Depend}, 
		  AllInstr, Modules) 
  when is_atom(Module), is_atom(Pre), is_atom(Post), is_list(Depend) ->
    d("check_instruction -> entry when load_module instruction with"
      "~n   Module: ~p"
      "~n   Pre:    ~p"
      "~n   Post:   ~p"
      "~n   Depend: ~p", [Module, Pre, Post, Depend]),
    check_module(Module, Modules),
    check_module_depend(Module, Depend, Modules),
    check_module_depend(Module, Depend, updated_modules(AllInstr, [])),
    check_purge(Pre),
    check_purge(Post);

check_instruction(_, {update, Module, Change, Pre, Post, Depend}, 
		  AllInstr, Modules) 
  when is_atom(Module), is_atom(Pre), is_atom(Post), is_list(Depend) ->
    d("check_instruction -> entry when update instruction with"
      "~n   Module: ~p"
      "~n   Change: ~p"
      "~n   Pre:    ~p"
      "~n   Post:   ~p"
      "~n   Depend: ~p", [Module, Change, Pre, Post, Depend]),
    check_module(Module, Modules),
    check_module_depend(Module, Depend, Modules),
    check_module_depend(Module, Depend, updated_modules(AllInstr, [])),
    check_change(Change),
    check_purge(Pre),
    check_purge(Post);

check_instruction(_, {apply, {Module, Function, Args}}, 
		  _AllInstr, Modules) 
  when is_atom(Module), is_atom(Function), is_list(Args) ->
    d("check_instruction -> entry when apply instruction with"
      "~n   Module: ~p"
      "~n   Function: ~p"
      "~n   Args:    ~p", [Module, Function, Args]),
    check_module(Module, Modules),
    check_apply(Module,Function,Args);

check_instruction(_, Instr, _AllInstr, _Modules) ->
    d("check_instruction -> entry when unknown instruction with"
      "~n   Instr: ~p", [Instr]),
    error({error, {unknown_instruction, Instr}}).


%% If Module X depends on Module Y, then module Y must have an update
%% instruction of some sort (otherwise the depend is faulty).
updated_modules([], Modules) ->
    d("update_modules -> entry when done with"
      "~n   Modules: ~p", [Modules]),
    Modules;
updated_modules([Instr|Instrs], Modules) ->
    d("update_modules -> entry with"
      "~n   Instr:   ~p"
      "~n   Modules: ~p", [Instr,Modules]),
    Module = instruction_module(Instr),
    d("update_modules -> Module: ~p", [Module]),
    updated_modules(Instrs, [Module|Modules]).
    
instruction_module({add_module, Module}) ->
    Module;
instruction_module({remove, {Module, _, _}}) ->
    Module;
instruction_module({load_module, Module, _, _, _}) ->
    Module;
instruction_module({update, Module, _, _, _, _}) ->
    Module;
instruction_module({apply, {Module, _, _}}) ->
    Module;
instruction_module(Instr) ->
    d("instruction_module -> entry when unknown instruction with"
      "~n   Instr: ~p", [Instr]),
    error({error, {unknown_instruction, Instr}}).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_version(V) when is_list(V) ->
    ok;
check_version(V) ->
    error({bad_version, V}).


check_module(M, Modules) when is_atom(M) ->
    case lists:member(M,Modules) of
        true ->
            ok;
        false ->
            error({unknown_module, M, Modules})
    end;
check_module(M, _) ->
    error({bad_module, M}).

check_apply(Module,Function,Args) ->
    case (catch Module:module_info()) of
	Info when is_list(Info) ->
	    check_exported(Function,Args,Info);
	{'EXIT',{undef,_}} ->
	    error({not_existing_module,Module})
    end.

check_exported(Function,Args,Info) ->
    case lists:keysearch(exports,1,Info) of
	{value,{exports,FunList}} ->
	    case lists:keysearch(Function,1,FunList) of
		{value,{_,Arity}} when Arity==length(Args) ->
		    ok;
		_ ->
		    error({not_exported_function,Function,length(Args)})
	    end;
	_ ->
	    error({bad_export,Info})
    end.

check_module_depend(M, [], _) when is_atom(M) ->
    d("check_module_depend -> entry with"
      "~n   M: ~p", [M]),    
    ok;
check_module_depend(M, Deps, Modules) when is_atom(M), is_list(Deps) ->
    d("check_module_depend -> entry with"
      "~n   M: ~p"
      "~n   Deps: ~p"
      "~n   Modules: ~p", [M, Deps, Modules]),    
    case [Dep || Dep <- Deps, lists:member(Dep, Modules) == false] of
        [] ->
            ok;
        Unknown ->
            error({unknown_depend_modules, Unknown})
    end;
check_module_depend(_M, D, _Modules) ->
    d("check_module_depend -> entry when bad depend with"
      "~n   D: ~p", [D]),    
    error({bad_depend, D}).


check_no_remove_depends(_Module, []) ->
    ok;
check_no_remove_depends(Module, [Instr|Instrs]) ->
    check_no_remove_depend(Module, Instr),
    check_no_remove_depends(Module, Instrs).

check_no_remove_depend(Module, {load_module, Mod, _Pre, _Post, Depend}) ->
    case lists:member(Module, Depend) of
	true ->
	    error({removed_module_in_depend, load_module, Mod, Module});
	false ->
	    ok
    end;
check_no_remove_depend(Module, {update, Mod, _Change, _Pre, _Post, Depend}) ->
    case lists:member(Module, Depend) of
	true ->
	    error({removed_module_in_depend, update, Mod, Module});
	false ->
	    ok
    end;
check_no_remove_depend(_, _) ->
    ok.
    

check_change(soft) ->
    ok;
check_change({advanced, _Something}) ->
    ok;
check_change(Change) ->
    error({bad_change, Change}).


check_purge(soft_purge) ->
    ok;
check_purge(brutal_purge) ->
    ok;
check_purge(Purge) ->
    error({bad_purge, Purge}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error(Reason) ->
    throw({error, Reason}).

fail(Reason) ->
    exit({suite_failed, Reason}).

key1search(Key, L) ->
    case lists:keysearch(Key, 1, L) of
	undefined ->
	    fail({not_found, Key, L});
	{value, {Key, Value}} ->
	    Value
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d(F, A) ->
    d(false, F, A).

d(true, F, A) ->
    io:format(F ++ "~n", A);
d(_, _, _) ->
    ok.

    
