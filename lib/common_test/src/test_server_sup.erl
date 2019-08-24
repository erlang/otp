%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2018. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% Purpose: Test server support functions.
%%%-------------------------------------------------------------------
-module(test_server_sup).
-export([timetrap/2, timetrap/3, timetrap/4,
	 timetrap_cancel/1, capture_get/1, messages_get/1,
	 timecall/3, call_crash/5, app_test/2, check_new_crash_dumps/0,
	 cleanup_crash_dumps/0, crash_dump_dir/0, tar_crash_dumps/0,
	 get_username/0, get_os_family/0, 
	 hostatom/0, hostatom/1, hoststr/0, hoststr/1,
	 framework_call/2,framework_call/3,framework_call/4,
	 format_loc/1,
	 util_start/0, util_stop/0, unique_name/0,
	 call_trace/1,
	 appup_test/1]).
-include("test_server_internal.hrl").
-define(crash_dump_tar,"crash_dumps.tar.gz").
-define(src_listing_ext, ".src.html").
-record(util_state, {starter, latest_name}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timetrap(Timeout,Scale,Pid) -> Handle
%% Handle = term()
%%
%% Creates a time trap, that will kill the given process if the 
%% trap is not cancelled with timetrap_cancel/1, within Timeout
%% milliseconds.
%% Scale says if the time should be scaled up to compensate for
%% delays during the test (e.g. if cover is running).

timetrap(Timeout0, Pid) ->
    timetrap(Timeout0, Timeout0, true, Pid).

timetrap(Timeout0, Scale, Pid) ->
    timetrap(Timeout0, Timeout0, Scale, Pid).

timetrap(Timeout0, ReportTVal, Scale, Pid) ->
    process_flag(priority, max),
    ct_util:mark_process(),
    Timeout = if not Scale -> Timeout0;
		 true -> test_server:timetrap_scale_factor() * Timeout0
	      end,
    TruncTO = trunc(Timeout),
    receive
    after TruncTO ->
	    kill_the_process(Pid, Timeout0, TruncTO, ReportTVal)
    end.

kill_the_process(Pid, Timeout0, TruncTO, ReportTVal) ->
    case is_process_alive(Pid) of
	true ->
	    TimeToReport = if Timeout0 == ReportTVal -> TruncTO;
			      true -> ReportTVal end,
	    MFLs = test_server:get_loc(Pid),
	    Mon = erlang:monitor(process, Pid),
	    Trap = {timetrap_timeout,TimeToReport,MFLs},
	    exit(Pid, Trap),
	    receive
		{'DOWN', Mon, process, Pid, _} ->
		    ok
	    after 10000 ->
		    %% Pid is probably trapping exits, hit it harder...
		    catch error_logger:warning_msg(
			    "Testcase process ~w not "
			    "responding to timetrap "
			    "timeout:~n"
			    "  ~tp.~n"
			    "Killing testcase...~n",
			    [Pid, Trap]),
		    exit(Pid, kill)
	    end;
	false ->
	    ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timetrap_cancel(Handle) -> ok
%% Handle = term()
%%
%% Cancels a time trap.
timetrap_cancel(Handle) ->
    unlink(Handle),
    MonRef = erlang:monitor(process, Handle),
    exit(Handle, kill),
    receive {'DOWN',MonRef,_,_,_} -> ok
    after
	2000 ->
	    erlang:demonitor(MonRef, [flush]),
	    ok
    end.

capture_get(Msgs) ->
    receive
	{captured,Msg} ->
	    capture_get([Msg|Msgs])
    after 0 ->
	    lists:reverse(Msgs)
    end.

messages_get(Msgs) ->
    receive
	Msg ->
	    messages_get([Msg|Msgs])
    after 0 ->
	    lists:reverse(Msgs)
    end.

timecall(M, F, A) ->
    {Elapsed, Val} = timer:tc(M, F, A),
    {Elapsed / 1000000, Val}.


call_crash(Time,Crash,M,F,A) ->
    OldTrapExit = process_flag(trap_exit,true),
    Pid = spawn_link(M,F,A),
    Answer =
	receive
	    {'EXIT',Crash} ->
		ok;
	    {'EXIT',Pid,Crash} ->
		ok;
	    {'EXIT',_Reason} when Crash==any ->
		ok;
	    {'EXIT',Pid,_Reason} when Crash==any ->
		ok;
	    {'EXIT',Reason} ->
		test_server:format(12, "Wrong crash reason. Wanted ~tp, got ~tp.",
		      [Crash, Reason]),
		exit({wrong_crash_reason,Reason});
	    {'EXIT',Pid,Reason} ->
		test_server:format(12, "Wrong crash reason. Wanted ~tp, got ~tp.",
		      [Crash, Reason]),
		exit({wrong_crash_reason,Reason});
	    {'EXIT',OtherPid,Reason} when OldTrapExit == false ->
		exit({'EXIT',OtherPid,Reason})
	after do_trunc(Time) ->
		exit(call_crash_timeout)
	end,
    process_flag(trap_exit,OldTrapExit),
    Answer.

do_trunc(infinity) -> infinity;
do_trunc(T) -> trunc(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% app_test/2
%%
%% Checks one applications .app file for obvious errors.
%% Checks..
%% * .. required fields
%% * .. that all modules specified actually exists
%% * .. that all requires applications exists
%% * .. that no module included in the application has export_all
%% * .. that all modules in the ebin/ dir is included
%%      (This only produce a warning, as all modules does not
%%       have to be included (If the `pedantic' option isn't used))
app_test(Application, Mode) ->
    case is_app(Application) of
	{ok, AppFile} ->
	    do_app_tests(AppFile, Application, Mode);
	Error ->
	    test_server:fail(Error)
    end.

is_app(Application) ->
    case file:consult(filename:join([code:lib_dir(Application),"ebin",
		   atom_to_list(Application)++".app"])) of
	{ok, [{application, Application, AppFile}] } ->
	    {ok, AppFile};
	_ ->
	    test_server:format(minor,
			       "Application (.app) file not found, "
			       "or it has very bad syntax.~n"),
	    {error, not_an_application}
    end.


do_app_tests(AppFile, AppName, Mode) ->
    DictList=
	[
	 {missing_fields, []},
	 {missing_mods, []},
	 {superfluous_mods_in_ebin, []},
	 {export_all_mods, []},
	 {missing_apps, []}
	],
    fill_dictionary(DictList),

    %% An appfile must (?) have some fields..
    check_fields([description, modules, registered, applications], AppFile),

    %% Check for missing and extra modules.
    {value, {modules, Mods}}=lists:keysearch(modules, 1, AppFile),
    EBinList=lists:sort(get_ebin_modnames(AppName)),
    {Missing, Extra} = common(lists:sort(Mods), EBinList),
    put(superfluous_mods_in_ebin, Extra),
    put(missing_mods, Missing),

    %% Check that no modules in the application has export_all.
    app_check_export_all(Mods),

    %% Check that all specified applications exists.
    {value, {applications, Apps}}=
	lists:keysearch(applications, 1, AppFile),
    check_apps(Apps),

    A=check_dict(missing_fields, "Inconsistent app file, "
	       "missing fields"),
    B=check_dict(missing_mods, "Inconsistent app file, "
	       "missing modules"),
    C=check_dict_tolerant(superfluous_mods_in_ebin, "Inconsistent app file, "
	       "Modules not included in app file.", Mode),
    D=check_dict(export_all_mods, "Inconsistent app file, "
	       "Modules have `export_all'."),
    E=check_dict(missing_apps, "Inconsistent app file, "
	       "missing applications."),

    erase_dictionary(DictList),
    case A+B+C+D+E of
	5 ->
	    ok;
	_ ->
	    test_server:fail()
    end.

app_check_export_all([]) ->
    ok;
app_check_export_all([Mod|Mods]) ->
    case catch apply(Mod, module_info, [compile]) of
	{'EXIT', {undef,_}} ->
	    app_check_export_all(Mods);
	COpts ->
	    case lists:keysearch(options, 1, COpts) of
		false ->
		    app_check_export_all(Mods);
		{value, {options, List}} ->
		    case lists:member(export_all, List) of
			true ->
			    put(export_all_mods, [Mod|get(export_all_mods)]),
			    app_check_export_all(Mods);
			false ->
			    app_check_export_all(Mods)
		    end
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% appup_test/1
%%
%% Checks one applications .appup file for obvious errors.
%% Checks..
%% * .. syntax
%% * .. that version in app file matches appup file version
%% * .. validity of appup instructions
%%
%% For library application this function checks that the proper
%% 'restart_application' upgrade and downgrade clauses exist.
appup_test(Application) ->
    case is_app(Application) of
        {ok, AppFile} ->
            case is_appup(Application, proplists:get_value(vsn, AppFile)) of
                {ok, Up, Down} ->
                    StartMod = proplists:get_value(mod, AppFile),
                    Modules = proplists:get_value(modules, AppFile),
                    do_appup_tests(StartMod, Application, Up, Down, Modules);
                Error ->
                    test_server:fail(Error)
            end;
        Error ->
            test_server:fail(Error)
    end.

is_appup(Application, Version) ->
    AppupFile = atom_to_list(Application) ++ ".appup",
    AppupPath = filename:join([code:lib_dir(Application), "ebin", AppupFile]),
    case file:consult(AppupPath) of
        {ok, [{Version, Up, Down}]} when is_list(Up), is_list(Down) ->
            {ok, Up, Down};
        _ ->
            test_server:format(
              minor,
              "Application upgrade (.appup) file not found, "
              "or it has very bad syntax.~n"),
            {error, appup_not_readable}
    end.

do_appup_tests(undefined, Application, Up, Down, _Modules) ->
    %% library application
    case Up of
        [{<<".*">>, [{restart_application, Application}]}] ->
            case Down of
                [{<<".*">>, [{restart_application, Application}]}] ->
                    ok;
                _ ->
                    test_server:format(
                      minor,
                      "Library application needs restart_application "
                      "downgrade instruction.~n"),
                    {error, library_downgrade_instruction_malformed}
            end;
        _ ->
            test_server:format(
              minor,
              "Library application needs restart_application "
              "upgrade instruction.~n"),
            {error, library_upgrade_instruction_malformed}
    end;
do_appup_tests(_, _Application, Up, Down, Modules) ->
    %% normal application
    case check_appup_clauses_plausible(Up, up, Modules) of
        ok ->
            case check_appup_clauses_plausible(Down, down, Modules) of
                ok ->
                    test_server:format(minor, "OK~n");
                Error ->
                    test_server:format(minor, "ERROR ~tp~n", [Error]),
                    test_server:fail(Error)
            end;
        Error ->
            test_server:format(minor, "ERROR ~tp~n", [Error]),
            test_server:fail(Error)
    end.
    
check_appup_clauses_plausible([], _Direction, _Modules) ->
    ok;
check_appup_clauses_plausible([{Re, Instrs} | Rest], Direction, Modules)
  when is_binary(Re) ->
    case re:compile(Re,[unicode]) of
        {ok, _} ->
            case check_appup_instructions(Instrs, Direction, Modules) of
                ok ->
                    check_appup_clauses_plausible(Rest, Direction, Modules);
                Error ->
                    Error
            end;
        {error, Error} ->
            {error, {version_regex_malformed, Re, Error}}
    end;
check_appup_clauses_plausible([{V, Instrs} | Rest], Direction, Modules)
  when is_list(V) ->
    case check_appup_instructions(Instrs, Direction, Modules) of
        ok ->
            check_appup_clauses_plausible(Rest, Direction, Modules);
        Error ->
            Error
    end;
check_appup_clauses_plausible(Clause, _Direction, _Modules) ->
    {error, {clause_malformed, Clause}}.

check_appup_instructions(Instrs, Direction, Modules) ->
    case check_instructions(Direction, Instrs, Instrs, [], [], Modules) of
        {_Good, []} ->
            ok;
        {_, Bad} ->
            {error, {bad_instructions, Bad}}
    end.

check_instructions(_, [], _, Good, Bad, _) ->
    {lists:reverse(Good), lists:reverse(Bad)};
check_instructions(UpDown, [Instr | Rest], All, Good, Bad, Modules) ->
    case catch check_instruction(UpDown, Instr, All, Modules) of
        ok ->
            check_instructions(UpDown, Rest, All, [Instr | Good], Bad, Modules);
        {error, Reason} ->
            NewBad = [{Instr, Reason} | Bad],
            check_instructions(UpDown, Rest, All, Good, NewBad, Modules)
    end.

check_instruction(up, {add_module, Module}, _, Modules) ->
    %% A new module is added
    check_module(Module, Modules);
check_instruction(down, {add_module, Module}, _, Modules) ->
    %% An old module is re-added
    case (catch check_module(Module, Modules)) of
        {error, {unknown_module, Module, Modules}} -> ok;
        ok -> throw({error, {existing_readded_module, Module}})
    end;
check_instruction(_, {load_module, Module}, _, Modules) ->
    check_module(Module, Modules);
check_instruction(_, {load_module, Module, DepMods}, _, Modules) ->
    check_module(Module, Modules),
    check_depend(DepMods);
check_instruction(_, {load_module, Module, Pre, Post, DepMods}, _, Modules) ->
    check_module(Module, Modules),
    check_depend(DepMods),
    check_purge(Pre),
    check_purge(Post);
check_instruction(up, {delete_module, Module}, _, Modules) ->
    case (catch check_module(Module, Modules)) of
        {error, {unknown_module, Module, Modules}} ->
            ok;
        ok ->
            throw({error,{existing_module_deleted, Module}})
    end;
check_instruction(down, {delete_module, Module}, _, Modules) ->
    check_module(Module, Modules);
check_instruction(_, {update, Module}, _, Modules) ->
    check_module(Module, Modules);
check_instruction(_, {update, Module, supervisor}, _, Modules) ->
    check_module(Module, Modules);
check_instruction(_, {update, Module, DepMods}, _, Modules)
  when is_list(DepMods) ->
    check_module(Module, Modules);
check_instruction(_, {update, Module, Change}, _, Modules) ->
    check_module(Module, Modules),
    check_change(Change);
check_instruction(_, {update, Module, Change, DepMods}, _, Modules) ->
    check_module(Module, Modules),
    check_change(Change),
    check_depend(DepMods);
check_instruction(_, {update, Module, Change, Pre, Post, DepMods}, _, Modules) ->
    check_module(Module, Modules),
    check_change(Change),
    check_purge(Pre),
    check_purge(Post),
    check_depend(DepMods);
check_instruction(_,
                  {update, Module, Timeout, Change, Pre, Post, DepMods},
                  _,
                  Modules) ->
    check_module(Module, Modules),
    check_timeout(Timeout),
    check_change(Change),
    check_purge(Pre),
    check_purge(Post),
    check_depend(DepMods);
check_instruction(_,
                  {update, Module, ModType, Timeout, Change, Pre, Post, DepMods},
                  _,
                  Modules) ->
    check_module(Module, Modules),
    check_mod_type(ModType),
    check_timeout(Timeout),
    check_change(Change),
    check_purge(Pre),
    check_purge(Post),
    check_depend(DepMods);
check_instruction(_, {restart_application, Application}, _, _) ->
    check_application(Application);
check_instruction(_, {remove_application, Application}, _, _) ->
    check_application(Application);
check_instruction(_, {add_application, Application}, _, _) ->
    check_application(Application);
check_instruction(_, {add_application, Application, Type}, _, _) ->
    check_application(Application),
    check_restart_type(Type);
check_instruction(_, Instr, _, _) ->
    throw({error, {low_level_or_invalid_instruction, Instr}}).

check_module(Module, Modules) ->
    case {is_atom(Module), lists:member(Module, Modules)} of
        {true, true}  -> ok;
        {true, false} -> throw({error, {unknown_module, Module}});
        {false, _}    -> throw({error, {bad_module, Module}})
    end.

check_application(App) ->
    case is_atom(App) of
        true  -> ok;
        false -> throw({error, {bad_application, App}})
    end.

check_depend(Dep) when is_list(Dep) -> ok;
check_depend(Dep)                   -> throw({error, {bad_depend, Dep}}).

check_restart_type(permanent) -> ok;
check_restart_type(transient) -> ok;
check_restart_type(temporary) -> ok;
check_restart_type(load)      -> ok;
check_restart_type(none)      -> ok;
check_restart_type(Type)      -> throw({error, {bad_restart_type, Type}}).

check_timeout(T) when is_integer(T), T > 0 -> ok;
check_timeout(default)                     -> ok;
check_timeout(infinity)                    -> ok;
check_timeout(T)                           -> throw({error, {bad_timeout, T}}).

check_mod_type(static)  -> ok;
check_mod_type(dynamic) -> ok;
check_mod_type(Type)    -> throw({error, {bad_mod_type, Type}}).

check_purge(soft_purge)   -> ok;
check_purge(brutal_purge) -> ok;
check_purge(Purge)        -> throw({error, {bad_purge, Purge}}).

check_change(soft)          -> ok;
check_change({advanced, _}) -> ok;
check_change(Change)        -> throw({error, {bad_change, Change}}).

%% Given two sorted lists, L1 and L2, returns {NotInL2, NotInL1},
%% NotInL2 is the elements of L1 which don't occurr in L2,
%% NotInL1 is the elements of L2 which don't ocurr in L1.

common(L1, L2) ->
    common(L1, L2, [], []).

common([X|Rest1], [X|Rest2], A1, A2) ->
    common(Rest1, Rest2, A1, A2);
common([X|Rest1], [Y|Rest2], A1, A2) when X < Y ->
    common(Rest1, [Y|Rest2], [X|A1], A2);
common([X|Rest1], [Y|Rest2], A1, A2) ->
    common([X|Rest1], Rest2, A1, [Y|A2]);
common([], L, A1, A2) ->
    {A1, L++A2};
common(L, [], A1, A2) ->
    {L++A1, A2}.

check_apps([]) ->
    ok;
check_apps([App|Apps]) ->
    case is_app(App) of
	{ok, _AppFile} ->
	    ok;
	{error, _} ->
	    put(missing_apps, [App|get(missing_apps)])
    end,
    check_apps(Apps).

check_fields([], _AppFile) ->
    ok;
check_fields([L|Ls], AppFile) ->
    check_field(L, AppFile),
    check_fields(Ls, AppFile).

check_field(FieldName, AppFile) ->
    case lists:keymember(FieldName, 1, AppFile) of
	true ->
	    ok;
	false ->
	    put(missing_fields, [FieldName|get(missing_fields)]),
	    ok
    end.

check_dict(Dict, Reason) ->
    case get(Dict) of
	[] ->
	    1;                         % All ok.
	List ->
	    io:format("** ~ts (~ts) ->~n~tp~n",[Reason, Dict, List]),
	    0
    end.

check_dict_tolerant(Dict, Reason, Mode) ->
    case get(Dict) of
	[] ->
	    1;                         % All ok.
	List ->
	    io:format("** ~ts (~ts) ->~n~tp~n",[Reason, Dict, List]),
	    case Mode of
		pedantic ->
		    0;
		_ ->
		    1
	    end
    end.

get_ebin_modnames(AppName) ->
    Wc=filename:join([code:lib_dir(AppName),"ebin",
		      "*"++code:objfile_extension()]),
    TheFun=fun(X, Acc) ->
		   [list_to_atom(filename:rootname(
				   filename:basename(X)))|Acc] end,
    _Files=lists:foldl(TheFun, [], filelib:wildcard(Wc)).

%%
%% This function removes any erl_crash_dump* files found in the
%% test server directory. Done only once when the test server
%% is started.
%%
cleanup_crash_dumps() ->
    Dir = crash_dump_dir(),
    Dumps = filelib:wildcard(filename:join(Dir, "erl_crash_dump*")),
    delete_files(Dumps).

crash_dump_dir() ->
    %% If no framework is known, then we use current working directory
    %% - in most cases that will be the same as the default log
    %% directory.
    {ok,Dir} = test_server_sup:framework_call(get_log_dir,[],file:get_cwd()),
    Dir.

tar_crash_dumps() ->
    Dir = crash_dump_dir(),
    case filelib:wildcard(filename:join(Dir, "erl_crash_dump*")) of
	[] -> {error,no_crash_dumps};
	Dumps ->
	    TarFileName = filename:join(Dir,?crash_dump_tar),
	    {ok,Tar} = erl_tar:open(TarFileName,[write,compressed]),
	    lists:foreach(
	      fun(File) -> 
		      ok = erl_tar:add(Tar,File,filename:basename(File),[])
	      end,
	      Dumps),
	    ok = erl_tar:close(Tar),
	    delete_files(Dumps),
	    {ok,TarFileName}
    end.


check_new_crash_dumps() ->
    Dir = crash_dump_dir(),
    Dumps = filelib:wildcard(filename:join(Dir, "erl_crash_dump*")),
    case length(Dumps) of
	0 ->
	    ok;
	Num ->
	    test_server_ctrl:format(minor,
				    "Found ~w crash dumps:~n", [Num]),
	    append_files_to_logfile(Dumps),
	    delete_files(Dumps)
    end.

append_files_to_logfile([]) -> ok;
append_files_to_logfile([File|Files]) ->
    NodeName=from($., File),
    test_server_ctrl:format(minor, "Crash dump from node ~tp:~n",[NodeName]),
    Fd=get(test_server_minor_fd),
    case file:read_file(File) of
	{ok, Bin} ->
	    case file:write(Fd, Bin) of
		ok ->
		    ok;
		{error,Error} ->
		    %% Write failed. The following io:format/3 will probably also
		    %% fail, but in that case it will throw an exception so that
		    %% we will be aware of the problem.
		    io:format(Fd, "Unable to write the crash dump "
			      "to this file: ~tp~n", [file:format_error(Error)])
	    end;
	_Error ->
	    io:format(Fd, "Failed to read: ~ts\n", [File])
    end,
    append_files_to_logfile(Files).

delete_files([]) -> ok;
delete_files([File|Files]) ->
    io:format("Deleting file: ~ts~n", [File]),
    case file:delete(File) of
	{error, _} ->
	    case file:rename(File, File++".old") of
		{error, Error} ->
		    io:format("Could neither delete nor rename file "
			      "~ts: ~ts.~n", [File, Error]);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    delete_files(Files).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% erase_dictionary(Vars) -> ok
%% Vars = [atom(),...]
%%
%% Takes a list of dictionary keys, KeyVals, erases
%% each key and returns ok.
erase_dictionary([{Var, _Val}|Vars]) ->
    erase(Var),
    erase_dictionary(Vars);
erase_dictionary([]) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fill_dictionary(KeyVals) -> void()
%% KeyVals = [{atom(),term()},...]
%%
%% Takes each Key-Value pair, and inserts it in the process dictionary.
fill_dictionary([{Var,Val}|Vars]) ->
    put(Var,Val),
    fill_dictionary(Vars);
fill_dictionary([]) ->
    [].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_username() -> UserName
%%
%% Returns the current user
get_username() ->
    getenv_any(["USER","USERNAME"]).
    
getenv_any([Key|Rest]) ->
    case catch os:getenv(Key) of
	String when is_list(String) -> String;
	false -> getenv_any(Rest)
    end;
getenv_any([]) -> "".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_os_family() -> OsFamily
%%
%% Returns the OS family
get_os_family() ->
    {OsFamily,_OsName} = os:type(),
    OsFamily.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% hostatom()/hostatom(Node) -> Host; atom()
%% hoststr() | hoststr(Node) -> Host; string()
%%
%% Returns the OS family
hostatom() ->
    hostatom(node()).
hostatom(Node) ->
    list_to_atom(hoststr(Node)).
hoststr() ->
    hoststr(node()).
hoststr(Node) when is_atom(Node) ->
    hoststr(atom_to_list(Node));
hoststr(Node) when is_list(Node) ->
    from($@, Node).
    
from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_H, []) -> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% framework_call(Callback,Func,Args,DefaultReturn) -> Return | DefaultReturn
%% 
%% Calls the given Func in Callback
framework_call(Func,Args) ->
    framework_call(Func,Args,ok).
framework_call(Func,Args,DefaultReturn) ->
    CB = os:getenv("TEST_SERVER_FRAMEWORK"),
    framework_call(CB,Func,Args,DefaultReturn).
framework_call(FW,_Func,_Args,DefaultReturn)  
  when FW =:= false; FW =:= "undefined" ->
    DefaultReturn;
framework_call(Callback,Func,Args,DefaultReturn) ->
    Mod = list_to_atom(Callback),
    _ = case code:is_loaded(Mod) of
	false -> code:load_file(Mod);
	_ -> ok
    end,
    case erlang:function_exported(Mod,Func,length(Args)) of
	true ->
	    EH = fun(Reason) -> exit({fw_error,{Mod,Func,Reason}}) end,
	    SetTcState = case Func of
			     end_tc -> true;
			     init_tc -> true;
			     _ -> false
			 end,
	    case SetTcState of
		true ->
		    test_server:set_tc_state({framework,{Mod,Func,Args}});
		false ->
		    ok
	    end,
            ct_util:mark_process(),
	    try apply(Mod,Func,Args) of
		Result ->
		    Result
	    catch
		exit:Why ->
		    EH(Why);
		error:Why:Stacktrace ->
		    EH({Why,Stacktrace});
		throw:Why ->
		    EH(Why)
	    end;
	false ->
	    DefaultReturn
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% format_loc(Loc) -> string()
%% 
%% Formats the printout of the line of code read from 
%% process dictionary (test_server_loc). Adds link to 
%% correct line in source code.
format_loc([{Mod,Func,Line}]) ->
    [format_loc1({Mod,Func,Line})];
format_loc([{Mod,Func,Line}|Rest]) ->
    ["[",format_loc1({Mod,Func,Line}),",\n"|format_loc1(Rest)];
format_loc([{Mod,LineOrFunc}]) ->
    format_loc({Mod,LineOrFunc});
format_loc({Mod,Func}) when is_atom(Func) -> 
    io_lib:format("{~w,~tw}",[Mod,Func]);
format_loc(Loc) ->
    io_lib:format("~tp",[Loc]).

format_loc1([{Mod,Func,Line}]) ->
    ["              ",format_loc1({Mod,Func,Line}),"]"];
format_loc1([{Mod,Func,Line}|Rest]) ->
    ["              ",format_loc1({Mod,Func,Line}),",\n"|format_loc1(Rest)];
format_loc1({Mod,Func,Line}) ->
    ModStr = atom_to_list(Mod),
    case {lists:member(no_src, get(test_server_logopts)),
	  lists:reverse(ModStr)} of
	{false,[$E,$T,$I,$U,$S,$_|_]}  ->
	    Link = if is_integer(Line) ->
			   integer_to_list(Line);
		      Line == last_expr ->
			   list_to_atom(atom_to_list(Func)++"-last_expr");
		      is_atom(Line) ->
			   atom_to_list(Line);
		      true ->
			   Line
		   end,
	    io_lib:format("{~w,~tw,<a href=\"~ts~ts#~ts\">~tw</a>}",
			  [Mod,Func,
			   test_server_ctrl:uri_encode(downcase(ModStr)),
			   ?src_listing_ext,Link,Line]);
	_ ->
	    io_lib:format("{~w,~tw,~tw}",[Mod,Func,Line])
    end.

downcase(S) -> downcase(S, []).
downcase([Uc|Rest], Result) when $A =< Uc, Uc =< $Z ->
    downcase(Rest, [Uc-$A+$a|Result]);
downcase([C|Rest], Result) ->
    downcase(Rest, [C|Result]);
downcase([], Result) ->
    lists:reverse(Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% util_start() -> ok
%%
%% Start local utility process
util_start() ->
    Starter = self(),
    case whereis(?MODULE) of
	undefined ->	
	    spawn_link(fun() ->
			       register(?MODULE, self()),
                               put(app, common_test),
			       util_loop(#util_state{starter=Starter})
		       end),
	    ok;
	_Pid ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% util_stop() -> ok
%%
%% Stop local utility process
util_stop() ->
    try (?MODULE ! {self(),stop}) of
	_ ->
	    receive {?MODULE,stopped} -> ok
	    after 5000 -> exit(whereis(?MODULE), kill)
	    end
    catch
	_:_ ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unique_name() -> string()
%%
unique_name() ->
    ?MODULE ! {self(),unique_name},
    receive {?MODULE,Name} -> Name
    after 5000 -> exit({?MODULE,no_util_process})
    end.
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% util_loop(State) -> ok
%%
util_loop(State) ->		       
    receive
	{From,unique_name} ->
	    Nr = erlang:unique_integer([positive]),
	    Name = integer_to_list(Nr),
	    if Name == State#util_state.latest_name ->
		    timer:sleep(1),
		    self() ! {From,unique_name},
		    util_loop(State);
	       true ->
		    From ! {?MODULE,Name},
		    util_loop(State#util_state{latest_name = Name})
	    end;
	{From,stop} ->
	    catch unlink(State#util_state.starter),
	    From ! {?MODULE,stopped},
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% call_trace(TraceSpecFile) -> ok
%%
%% Read terms on format {m,Mod} | {f,Mod,Func}
%% from TraceSpecFile and enable call trace for
%% specified functions.
call_trace(TraceSpec) ->
    case catch try_call_trace(TraceSpec) of
	{'EXIT',Reason} ->
	    erlang:display(Reason),
	    exit(Reason);
	Ok ->
	    Ok
    end.    

try_call_trace(TraceSpec) ->
    case file:consult(TraceSpec) of
	{ok,Terms} ->
	    dbg:tracer(),
	    %% dbg:p(self(), [p, m, sos, call]),
	    dbg:p(self(), [sos, call]),
	    lists:foreach(fun({m,M}) ->
				  case dbg:tpl(M,[{'_',[],[{return_trace}]}]) of
				      {error,What} -> exit({error,{tracing_failed,What}});
				      _ -> ok
				  end;			  
			     ({f,M,F}) ->
				  case dbg:tpl(M,F,[{'_',[],[{return_trace}]}]) of
				      {error,What} -> exit({error,{tracing_failed,What}});
				      _ -> ok
				  end;			  
			     (Huh) ->
				  exit({error,{unrecognized_trace_term,Huh}})
			  end, Terms),
	    ok;
	{_,Error} ->
	    exit({error,{tracing_failed,TraceSpec,Error}})
    end.
    
