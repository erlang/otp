%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2011. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the Megaco application
%%----------------------------------------------------------------------
-module(snmp_appup_test).

-export([
	 all/0,
	 groups/0, init_per_group/2, end_per_group/2, 
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2, 

	 appup_file/1

	]).

-compile({no_auto_import, [error/1]}).

-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    Cases = 
	[
	 appup_file
	],
    Cases.

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(suite) -> [];
init_per_suite(doc) -> [];
init_per_suite(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    TopDir  = filename:join(PrivDir, appup),
    case file:make_dir(TopDir) of
        ok ->
            ok;
        Error ->
            fail({failed_creating_subsuite_top_dir, Error})
    end,
    AppFile   = file_name(?APPLICATION, ".app"),
    AppupFile = file_name(?APPLICATION, ".appup"),
    [{app_file,     AppFile}, 
     {appup_file,   AppupFile},
     {appup_topdir, TopDir} | Config].
    

file_name(App, Ext) ->
    Env = init:get_arguments(),
    LibDir = 
	case lists:keysearch(clearcase, 1, Env) of
	    false ->
		code:lib_dir(App);
	    _ ->
		".."
	end,
    filename:join([LibDir, "ebin", atom_to_list(App) ++ Ext]).


end_per_suite(suite) -> [];
end_per_suite(doc) -> [];
end_per_suite(Config) when is_list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test server callbacks
init_per_testcase(_Case, Config) when is_list(Config) ->
    Config.

end_per_testcase(_Case, Config) when is_list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

appup_file(suite) ->
    [];
appup_file(doc) ->
    "Perform a simple check of the appup file";
appup_file(Config) when is_list(Config) ->
    AppupFile = key1search(appup_file, Config),
    AppFile   = key1search(app_file, Config),
    Modules   = modules(AppFile),
    check_appup(AppupFile, Modules).

modules(File) ->
    case file:consult(File) of
        {ok, [{application,snmp,Info}]} ->
            case lists:keysearch(modules,1,Info) of
                {value, {modules, Modules}} ->
                    Modules;
                false ->
                    fail({bad_appinfo, Info})
            end;
        Error ->
            fail({bad_appfile, Error, File})
    end.

    
check_appup(AppupFile, Modules) ->
    case file:consult(AppupFile) of
	{ok, [{V, UpFrom, DownTo}]} ->
	    check_appup(V, UpFrom, DownTo, Modules);
	Else ->
	    fail({bad_appupfile, Else})
    end.


check_appup(V, UpFrom, DownTo, Modules) ->
    check_version(V),
    check_depends(up,   UpFrom, Modules),
    check_depends(down, DownTo, Modules),
    check_module_subset(up,   UpFrom),
    check_module_subset(down, DownTo),
    ok.

check_depends(_, [], _) ->
    ok;
check_depends(UpDown, [Dep|Deps], Modules) ->
    check_depend(UpDown, Dep, Modules),
    check_depends(UpDown, Deps, Modules).


check_depend(up = UpDown, {add_application, ?APPLICATION} = Instr, Modules) ->
     d("check_instructions(~w) -> entry with"
       "~n   Instruction: ~p"
       "~n   Modules:     ~p", [UpDown, Instr, Modules]),
     ok;
check_depend(down = UpDown, {remove_application, ?APPLICATION} = Instr,
             Modules) ->
     d("check_instructions(~w) -> entry with"
       "~n   Instruction: ~p"
       "~n   Modules:     ~p", [UpDown, Instr, Modules]),
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
            check_instructions(UpDown, Instrs, AllInstr, Good, 
                               [{Instr, Reason}|Bad], Modules)
    end;
check_instructions(UpDown, Instructions, _, _, _, _) ->
    fail({bad_instructions, {UpDown, Instructions}}).

check_instruction(_, {restart_application, ?APPLICATION}, _, _Modules) ->
    d("check_instruction -> entry when restart_application instruction"),
    ok;

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

check_instruction(up, {delete_module, Module}, _, Modules) 
  when is_atom(Module) ->
    d("check_instruction -> entry when up-delete_module instruction with"
      "~n   Module: ~p", [Module]),
    case (catch check_module(Module, Modules)) of
	{error, {unknown_module, Module, Modules}} ->
	    ok;
	ok ->
	    error({module_cannot_be_deleted, Module})
    end;

%% An new module is deleted
check_instruction(down, {delete_module, Module}, _, Modules) 
  when is_atom(Module) ->
    d("check_instruction -> entry when down-delete_module instruction with"
      "~n   Module: ~p", [Module]),
    check_module(Module, Modules);

%% Removing a module on upgrade: 
%% - the module has been removed from the app-file.
%% - check that no module depends on this (removed) module
check_instruction(up, {remove, {Module, Pre, Post}}, _, Modules) 
  when is_atom(Module) and is_atom(Pre) and is_atom(Post) ->
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
  when is_atom(Module) and is_atom(Pre) and is_atom(Post) ->
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
  when is_atom(Module) and 
       is_atom(Pre)    and 
       is_atom(Post)   and 
       is_list(Depend) ->
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
  when is_atom(Module) and 
       is_atom(Pre)    and 
       is_atom(Post)   and 
       is_list(Depend) ->
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

check_instruction(_, {update, Module, supervisor}, _, Modules)
  when is_atom(Module) ->
    d("check_instruction -> entry when supervisor update instruction with"
      "~n   Module: ~p", [Module]),
    check_module(Module, Modules);

check_instruction(_, {apply, {Module, Function, Args}}, _, _Modules)
   when is_atom(Module) and is_atom(Function) and is_list(Args) ->
     d("check_instruction -> entry when apply instruction with"
       "~n   Module:   ~p"
       "~n   Function: ~p"
       "~n   Args:     ~p", [Module, Function, Args]),
     check_apply(Module, Function, Args);

check_instruction(_, Instr, _AllInstr, _Modules) ->
    error({error, {unknown_instruction, Instr}}).

%% If Module X depends on Module Y, then module Y must have an update
%% instruction of some sort (otherwise the depend is faulty).
updated_modules([], Modules) ->
    d("updated_modules -> entry when done with"
      "~n   Modules: ~p", [Modules]),
    Modules;
updated_modules([Instr|Instrs], Modules) ->
    d("updated_modules -> entry with"
      "~n   Instr:   ~p"
      "~n   Modules: ~p", [Instr,Modules]),
    case instruction_module(Instr) of
	{module, Module} ->
	    d("updated_modules -> Module: ~p", [Module]),
	    updated_modules(Instrs, [Module|Modules]);
	no_module ->
	    updated_modules(Instrs, Modules)
    end.

instruction_module({add_module, Module}) ->
    {module, Module};
instruction_module({delete_module, Module}) ->
    {module, Module};
instruction_module({remove, {Module, _, _}}) ->
    {module, Module};
instruction_module({load_module, Module, _, _, _}) ->
    {module, Module};
instruction_module({update, Module, _, _, _, _}) ->
    {module, Module};
instruction_module({update, Module, _}) ->
    {module, Module};
instruction_module({apply, {_, _, _}}) ->
    no_module;
instruction_module(Instr) ->
    d("instruction_module -> entry when unknown instruction with"
      "~n   Instr: ~p", [Instr]),
    error({error, {unknown_instruction, Instr}}).


%% Check that the modules handled in an instruction set for version X
%% is a subset of the instruction set for version X-1.
check_module_subset(Direction, Instructions) ->
    d("check_module_subset(~w) -> entry when"
      "~n   Instructions: ~p", [Direction,Instructions]),
    do_check_module_subset(modules_of(Instructions)).

do_check_module_subset([]) ->
    ok;
do_check_module_subset([_]) ->
    ok;
do_check_module_subset([{_V1, Mods1}|T]) ->
    d("do_check_module_subset -> entry with"
      "~n   V1:    ~s"
      "~n   Mods1: ~p", [_V1, Mods1]),
    {V2, Mods2} = hd(T),
    d("do_check_module_subset -> "
      "~n   V2:    ~s"
      "~n   Mods2: ~p", [V2, Mods2]),
    %% Check that the modules in V1 is a subset of V2
    case do_check_module_subset2(Mods1, Mods2) of
	ok ->
	    do_check_module_subset(T);
	{error, Modules} ->
	    fail({subset_missing_instructions, V2, Modules})
    end.

do_check_module_subset2(_Mods1, [{restart_application, ?APPLICATION}]) ->
    ok;
do_check_module_subset2(Mods1, Mods2) ->
    do_check_module_subset2(Mods1, Mods2, []).

do_check_module_subset2([], _, []) ->
    ok;
do_check_module_subset2([], _, Acc) ->
    {error, lists:reverse(Acc)};
do_check_module_subset2([Mod|Mods], Mods2, Acc) ->
    case lists:member(Mod, Mods2) of
	true ->
	    do_check_module_subset2(Mods, Mods2, Acc);
	false ->
	    do_check_module_subset2(Mods, Mods2, [Mod|Acc])
    end.
    

modules_of(Instructions) ->
    modules_of(Instructions, []).

modules_of([], Acc) ->
    lists:reverse(Acc);
modules_of([{_V,[{restart_application, ?APPLICATION}]}|T], Acc) ->
    modules_of(T, Acc);
modules_of([{V,Instructions}|T], Acc) ->
    Mods = modules_of2(Instructions, []),
    modules_of(T, [{V, Mods}|Acc]).

modules_of2([], Acc) ->
    lists:reverse(Acc);
modules_of2([Instr|Instructions], Acc) ->
    d("module_of -> entry with"
      "~n   Instr: ~p", [Instr]),
    case module_of(Instr) of
	{value, Mod} ->
	    d("module_of -> Mod: ~p", [Mod]),
	    modules_of2(Instructions, [Mod|Acc]);
	false ->
	    modules_of2(Instructions, Acc)
    end.

module_of({add_module, Module}) ->
    {value, Module};
module_of({delete_module, Module}) ->
    {value, Module};
module_of({remove, {Module, _Pre, _Post}}) ->
    {value, Module};
module_of({load_module, Module, _Pre, _Post, _Depend}) ->
    {value, Module};
module_of({update, Module, _Change, _Pre, _Post, _Depend}) ->
    {value, Module};
module_of({update, Module, supervisor}) ->
    {value, Module};
module_of(_) ->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_version(V) when is_list(V) ->
    ok;
check_version(V) ->
    error({bad_version, V}).


check_module(M, Modules) when is_atom(M) ->
    case lists:member(M, Modules) of
        true ->
            ok;
        false ->
            error({unknown_module, M, Modules})
    end;
check_module(M, _) ->
    error({bad_module, M}).


check_module_depend(M, [], _) when is_atom(M) ->
    ok;
check_module_depend(M, Deps, Modules) when is_atom(M) and is_list(Deps) ->
    case [Dep || Dep <- Deps, lists:member(Dep, Modules) == false] of
        [] ->
            ok;
        Unknown ->
            error({unknown_depend_modules, Unknown})
    end;
check_module_depend(_M, D, _Modules) ->
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

check_apply(Module, Function, Args) ->
     case (catch Module:module_info()) of
        Info when is_list(Info) ->
            check_exported(Function, Args, Info);
        {'EXIT', {undef, _}} ->
            error({not_existing_module, Module})
     end.

check_exported(Function, Args, Info) ->
     case lists:keysearch(exports, 1, Info) of
        {value, {exports, FuncList}} ->
             Arity   = length(Args),
             Arities = [A || {F, A} <- FuncList, F == Function],
             case lists:member(Arity, Arities) of
                 true ->
                     ok;
                 false ->
                     fail({not_exported_function, Function, Arity})
             end;
         _ ->
             error({bad_export, Info})
     end.


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

d(F) -> 
    d(F, []).

d(F, A) ->
    d(true, F, A).
 
d(true, F, A) ->
    io:format(F ++ "~n", A);
d(_, _, _) ->
    ok.
 
