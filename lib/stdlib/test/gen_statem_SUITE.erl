%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2024. All Rights Reserved.
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
-module(gen_statem_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).
-behaviour(gen_statem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,10}}].

all() ->
    [{group, start},
     {group, start_handle_event},
     {group, stop},
     {group, stop_handle_event},
     {group, abnormal},
     {group, abnormal_handle_event},
     shutdown, loop_start_fail, stop_and_reply, state_enter, event_order,
     state_timeout, timeout_cancel_and_update,
     event_types, generic_timers, code_change,
     {group, sys},
     hibernate, auto_hibernate, enter_loop, {group, undef_callbacks},
     undef_in_terminate, {group, format_log},
     reply_by_alias_with_payload,
     send_request_receive_reqid_collection, send_request_wait_reqid_collection,
     send_request_check_reqid_collection].

groups() ->
    [{start, [], tcs(start)},
     {start_handle_event, [], tcs(start)},
     {stop, [], tcs(stop)},
     {stop_handle_event, [], tcs(stop)},
     {abnormal, [], tcs(abnormal)},
     {abnormal_handle_event, [], tcs(abnormal)},
     {sys, [], tcs(sys)},
     {format_status, [], tcs(format_status)},
     {sys_handle_event, [], tcs(sys)},
     {undef_callbacks, [], tcs(undef_callbacks)},
     {format_log, [], tcs(format_log)}].

tcs(start) ->
    [start1, start2, start3, start4, start5a, start5b, start6, start7,
     start8, start9, start10, start11, start12, next_events];
tcs(stop) ->
    [stop1, stop2, stop3, stop4, stop5, stop6, stop7, stop8, stop9, stop10];
tcs(abnormal) ->
    [abnormal1, abnormal1clean, abnormal1dirty,
     abnormal2, abnormal3, abnormal4];
tcs(sys) ->
    [sys1, {group, format_status}, get_state, replace_state];
tcs(format_status) ->
    [call_format_status, error_format_status, terminate_crash_format,
     format_all_status];
tcs(undef_callbacks) ->
    [undef_code_change, undef_terminate1, undef_terminate2,
     pop_too_many];
tcs(format_log) ->
    [format_log_1, format_log_2, format_log_with_process_label].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(GroupName, Config)
  when GroupName =:= start_handle_event;
       GroupName =:= stop_handle_event;
       GroupName =:= abnormal_handle_event;
       GroupName =:= sys_handle_event ->
    [{callback_mode,handle_event_function}|Config];
init_per_group(undef_callbacks, Config) ->
    try compile_oc_statem(Config)
    catch Class : Reason : Stacktrace ->
             {fail,{Class,Reason,Stacktrace}}
    end,
    Config;
init_per_group(sys, Config) ->
    compile_format_status_statem(Config),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    flush(),
%%%    dbg:tracer(),
%%%    dbg:p(all, c),
%%%    dbg:tpl(gen_statem, cx),
%%%    dbg:tpl(gen_statem, loop_receive, cx),
%%%    dbg:tpl(gen_statem, loop_state_callback, cx),
%%%    dbg:tpl(gen_statem, loop_callback_mode_result, cx),
%%%    dbg:tpl(proc_lib, cx),
%%%    dbg:tpl(gen, cx),
%%%    dbg:tpl(sys, cx),
    Config.

end_per_testcase(_CaseName, Config) ->
%%%    dbg:stop(),
    Config.

compile_oc_statem(Config) ->
    DataDir = ?config(data_dir, Config),
    StatemPath = filename:join(DataDir, "oc_statem.erl"),
    {ok, oc_statem} = compile:file(StatemPath),
    ok.

compile_format_status_statem(Config) ->
    DataDir = ?config(data_dir, Config),
    StatemPath = filename:join(DataDir, "format_status_statem.erl"),
    {ok, format_status_statem} = compile:file(StatemPath),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(EXPECT_FAILURE(Code, Reason),
	try begin Code end of
	    Reason ->
		ct:fail({unexpected,Reason})
	catch
	    error:Reason -> Reason;
	    exit:Reason -> Reason
	end).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% anonymous
start1(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    {ok,Pid0} =
	gen_statem:start_link(?MODULE, start_arg(Config, []), [{debug,[trace]}]),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stop_it(Pid0),
%%    stopped = gen_statem:call(Pid0, stop),
%%    timeout =
%%	?EXPECT_FAILURE(gen_statem:call(Pid0, hej), Reason),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq(),

    {ok,{Pid1,Mon1}} = gen_statem:start_monitor(?MODULE, start_arg(Config, []), []),
    ok = do_func_test(Pid1),
    ok = do_sync_func_test(Pid1),
    stop_it(Pid1),
    receive
        {'DOWN', Mon1, process, Pid1, _Reason} ->
            ok
    end,
    ok = verify_empty_msgq().
    
%% anonymous w. shutdown
start2(Config) ->
    %% Dont link when shutdown
    {ok,Pid0} =
	gen_statem:start(?MODULE, start_arg(Config, []), []),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stopped = gen_statem:call(Pid0, {stop,shutdown}),
    check_stopped(Pid0),
    ok = verify_empty_msgq().

%% anonymous with timeout
start3(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    {ok,Pid0} =
	gen_statem:start(?MODULE, start_arg(Config, []), [{timeout,5}]),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stop_it(Pid0),

    {error,timeout} =
	gen_statem:start(
	  ?MODULE, start_arg(Config, sleep), [{timeout,5}]),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous with ignore
start4(Config) ->
    OldFl = process_flag(trap_exit, true),

    ignore = gen_statem:start(?MODULE, start_arg(Config, ignore), []),

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous with stop
start5a(Config) ->
    OldFl = process_flag(trap_exit, true),

    {error,stopped} = gen_statem:start(?MODULE, start_arg(Config, stop), []),

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous with shutdown
start5b(Config) ->
    OldFl = process_flag(trap_exit, true),

    {error, foobar} =
        gen_statem:start(?MODULE, start_arg(Config, {error, foobar}), []),

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous linked
start6(Config) ->
    {ok,Pid} = gen_statem:start_link(?MODULE, start_arg(Config, []), []),
    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    stop_it(Pid),

    ok = verify_empty_msgq().

%% global register linked & monitored
start7(Config) ->
    STM = {global,my_stm},

    {ok,Pid} =
	gen_statem:start_link(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start_link(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start_monitor(STM, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(STM),
    ok = do_sync_func_test(STM),
    stop_it(STM),

    ok = verify_empty_msgq(),

    {ok,{Pid1,Mon1}} =
	gen_statem:start_monitor(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid1}} =
	gen_statem:start_link(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid1}} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid1}} =
	gen_statem:start_monitor(STM, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid1),
    ok = do_sync_func_test(Pid1),
    ok = do_func_test(STM),
    ok = do_sync_func_test(STM),
    stop_it(STM),

    receive
        {'DOWN', Mon1, process, Pid1, _Reason} ->
            ok
    end,

    ok = verify_empty_msgq().


%% local register
start8(Config) ->
    %%OldFl = process_flag(trap_exit, true),
    Name = my_stm,
    STM = {local,Name},

    {ok,Pid} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(Name),
    ok = do_sync_func_test(Name),
    stop_it(Pid),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% local register linked & monitored
start9(Config) ->
    %%OldFl = process_flag(trap_exit, true),
    Name = my_stm,
    STM = {local,Name},

    {ok,Pid} =
	gen_statem:start_link(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(Name),
    ok = do_sync_func_test(Name),
    stop_it(Pid),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq(),

    {ok,{Pid1,Mon1}} =
	gen_statem:start_monitor(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid1}} =
	gen_statem:start_monitor(STM, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid1),
    ok = do_sync_func_test(Pid1),
    ok = do_func_test(Name),
    ok = do_sync_func_test(Name),
    stop_it(Pid1),

    receive
        {'DOWN', Mon1, process, Pid1, _Reason} ->
            ok
    end,

    ok = verify_empty_msgq().

%% global register
start10(Config) ->
    STM = {global,my_stm},

    {ok,Pid} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start_link(STM, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(STM),
    ok = do_sync_func_test(STM),
    stop_it(STM),

    ok = verify_empty_msgq().

%% Stop registered processes
start11(Config) ->
    Name = my_stm,
    LocalSTM = {local,Name},
    GlobalSTM = {global,Name},

    {ok,Pid} =
	gen_statem:start_link(LocalSTM, ?MODULE, start_arg(Config, []), []),
    stop_it(Pid),

    {ok,_Pid1} =
	gen_statem:start_link(LocalSTM, ?MODULE, start_arg(Config, []), []),
    stop_it(Name),

    {ok,Pid2} =
	gen_statem:start(GlobalSTM, ?MODULE, start_arg(Config, []), []),
    stop_it(Pid2),
    receive after 1 -> true end,
    Result =
	gen_statem:start(GlobalSTM, ?MODULE, start_arg(Config, []), []),
    ct:log("Result = ~p~n",[Result]),
    {ok,_Pid3} = Result,
    stop_it(GlobalSTM),

    ok = verify_empty_msgq().

%% Via register linked
start12(Config) ->
    dummy_via:reset(),
    VIA = {via,dummy_via,my_stm},

    {ok,Pid} =
	gen_statem:start_link(VIA, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start_link(VIA, ?MODULE, start_arg(Config, []), []),
    {error,{already_started,Pid}} =
	gen_statem:start(VIA, ?MODULE, start_arg(Config, []), []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(VIA),
    ok = do_sync_func_test(VIA),
    stop_it(VIA),

    ok = verify_empty_msgq().


%% Anonymous, reason 'normal'
stop1(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop(Pid),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(Pid), Reason).

%% Anonymous, other reason
stop2(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop(Pid, other_reason, infinity),
    false = erlang:is_process_alive(Pid),
    ok.

%% Anonymous, invalid timeout
stop3(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    _ =
	?EXPECT_FAILURE(
	   gen_statem:stop(Pid, other_reason, invalid_timeout),
	   Reason),
    true = erlang:is_process_alive(Pid),
    ok = gen_statem:stop(Pid),
    false = erlang:is_process_alive(Pid),
    ok.

%% Registered name
stop4(Config) ->
    {ok,Pid} =
	gen_statem:start(
	  {local,to_stop},?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop(to_stop),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(to_stop), Reason),
    ok.

%% Registered name and local node
stop5(Config) ->
    Name = to_stop,
    {ok,Pid} =
	gen_statem:start(
	  {local,Name},?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop({Name,node()}),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop({Name,node()}), Reason),
    ok.

%% Globally registered name
stop6(Config) ->
    STM = {global,to_stop},
    {ok,Pid} = gen_statem:start(STM, ?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop(STM),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason),
    ok.

%% 'via' registered name
stop7(Config) ->
    VIA = {via,dummy_via,to_stop},
    dummy_via:reset(),
    {ok,Pid} = gen_statem:start(VIA, ?MODULE, start_arg(Config, []), []),
    ok = gen_statem:stop(VIA),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(VIA), Reason),
    ok.

%% Anonymous on remote node
stop8(Config) ->
    {ok,Peer,NodeName} = ?CT_PEER(),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:block_call(NodeName, code, add_path, [Dir]),
    {ok,Pid} =
        rpc:block_call(
          NodeName, gen_statem,start,
          [?MODULE,start_arg(Config, []),[]]),
    ok = gen_statem:stop(Pid),
    false = rpc:block_call(NodeName, erlang, is_process_alive, [Pid]),
    noproc =
        ?EXPECT_FAILURE(gen_statem:stop(Pid), Reason1),

    peer:stop(Peer),
    {{nodedown,NodeName},{sys,terminate,_}} =
	?EXPECT_FAILURE(gen_statem:stop(Pid), Reason2),
    ok.

%% Registered name on remote node
stop9(Config) ->
    Name = to_stop,
    LocalSTM = {local,Name},
    {ok,Peer,NodeName} = ?CT_PEER(),

    STM = {Name,NodeName},
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:block_call(NodeName, code, add_path, [Dir]),
    {ok,Pid} =
        rpc:block_call(
          NodeName, gen_statem, start,
          [LocalSTM,?MODULE,start_arg(Config, []),[]]),
    ok = gen_statem:stop(STM),
    undefined = rpc:block_call(NodeName,erlang,whereis,[Name]),
    false = rpc:block_call(NodeName,erlang,is_process_alive,[Pid]),
    noproc =
        ?EXPECT_FAILURE(gen_statem:stop(STM), Reason1),
    peer:stop(Peer),

    {{nodedown,NodeName},{sys,terminate,_}} =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason2),
    ok.

%% Globally registered name on remote node
stop10(Config) ->
    STM = {global,to_stop},
    {ok,Peer,NodeName} = ?CT_PEER(),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:block_call(NodeName,code,add_path,[Dir]),
    {ok,Pid} =
        rpc:block_call(
          NodeName, gen_statem, start,
          [STM,?MODULE,start_arg(Config, []),[]]),
    global:sync(),
    ok = gen_statem:stop(STM),
    false = rpc:block_call(NodeName, erlang, is_process_alive, [Pid]),
    noproc =
        ?EXPECT_FAILURE(gen_statem:stop(STM), Reason1),
    peer:stop(Peer),

    noproc =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason2),
    ok.

%% Check that time outs in calls work
abnormal1(Config) ->
    Name = abnormal1,
    LocalSTM = {local,Name},

    {ok, _Pid} =
	gen_statem:start(LocalSTM, ?MODULE, start_arg(Config, []), []),

    %% timeout call.
    delayed = gen_statem:call(Name, {delayed_answer,100}, 2000),
    {timeout,_} =
	?EXPECT_FAILURE(
	   gen_statem:call(Name, {delayed_answer,2000}, 100),
	   Reason),
    ok = gen_statem:stop(Name),
    ct:sleep(1100),
    ok = verify_empty_msgq().

%% Check that time outs in calls work
abnormal1clean(Config) ->
    Name = abnormal1clean,
    LocalSTM = {local,Name},

    {ok, _Pid} =
	gen_statem:start(LocalSTM, ?MODULE, start_arg(Config, []), []),

    %% timeout call.
    delayed =
	gen_statem:call(Name, {delayed_answer,1}, {clean_timeout,100}),
    {timeout,_} =
	?EXPECT_FAILURE(
	   gen_statem:call(
	     Name, {delayed_answer,1000}, {clean_timeout,10}),
	   Reason),
    ok = gen_statem:stop(Name),
    ct:sleep(1100),
    ok = verify_empty_msgq().

%% Check that time outs in calls work
abnormal1dirty(Config) ->
    Name = abnormal1dirty,
    LocalSTM = {local,Name},

    {ok, _Pid} =
	gen_statem:start(LocalSTM, ?MODULE, start_arg(Config, []), []),

    %% timeout call.
    delayed =
	gen_statem:call(Name, {delayed_answer,1}, {dirty_timeout,100}),
    {timeout,_} =
	?EXPECT_FAILURE(
	   gen_statem:call(
	     Name, {delayed_answer,1000}, {dirty_timeout,10}),
	   Reason),
    ok = gen_statem:stop(Name),
    ct:sleep(1100),
    case flush() of
	[] -> ok
    end.

%% Check that bad return values makes the stm crash. Note that we must
%% trap exit since we must link to get the real bad_return_ error
abnormal2(Config) ->
    OldFl = process_flag(trap_exit, true),
    {ok,Pid} =
        gen_statem:start_link(
          ?MODULE, start_arg(Config, []), [{debug,[log]}]),

    %% bad return value in the gen_statem loop
    Cause = bad_return_from_state_function,
    {{{Cause,badreturn},_},_} =
	?EXPECT_FAILURE(gen_statem:call(Pid, badreturn), Reason),
    receive
	{'EXIT',Pid,{{Cause,badreturn},_}} -> ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% Check that bad return actions makes the stm crash. Note that we must
%% trap exit since we must link to get the real bad_return_ error
abnormal3(Config) ->
    OldFl = process_flag(trap_exit, true),
    {ok,Pid} =
        gen_statem:start_link(
          ?MODULE, start_arg(Config, []), [{debug,[log]}]),

    %% bad return value in the gen_statem loop
    Cause = bad_action_from_state_function,
    {{{Cause,badaction},_},_} =
	?EXPECT_FAILURE(gen_statem:call(Pid, badaction), Reason),
    receive
	{'EXIT',Pid,{{Cause,badaction},_}} -> ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% Check that bad timeout actions makes the stm crash. Note that we must
%% trap exit since we must link to get the real bad_return_ error
abnormal4(Config) ->
    OldFl = process_flag(trap_exit, true),
    {ok,Pid} =
        gen_statem:start_link(
          ?MODULE, start_arg(Config, []), [{debug,[log]}]),

    %% bad return value in the gen_statem loop
    BadTimeout = {badtimeout,4711,ouch},
    Cause = bad_action_from_state_function,
    {{{Cause,BadTimeout},_},_} =
	?EXPECT_FAILURE(gen_statem:call(Pid, {badtimeout,BadTimeout}), Reason),
    receive
	{'EXIT',Pid,{{Cause,BadTimeout},_}} -> ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

shutdown(Config) ->
    process_flag(trap_exit, true),

    {ok,Pid0} = gen_statem:start_link(?MODULE, start_arg(Config, []), []),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stopped = gen_statem:call(Pid0, {stop,{shutdown,reason}}),
    receive {'EXIT',Pid0,{shutdown,reason}} -> ok end,
    process_flag(trap_exit, false),

    {noproc,_} =
	?EXPECT_FAILURE(gen_statem:call(Pid0, hej), Reason),

    receive
	Any ->
	    ct:log("Unexpected: ~p", [Any]),
	    ct:fail({unexpected,Any})
    after 500 ->
	    ok
    end.


loop_start_fail(Config) ->
    _ = process_flag(trap_exit, true),
    loop_start_fail(
      Config,
      [{start, []}, {start, [link]},
       {start_link, []},
       {start_monitor, [link]}, {start_monitor, []}]).

loop_start_fail(_Config, []) ->
    ok;
loop_start_fail(Config, [{Start, Opts} | Start_Opts]) ->
    loop_start_fail(
      fun gen_statem:Start/3,
      {ets, {return, {stop, failed_to_start}}}, Opts,
      fun ({error, failed_to_start}) -> ok end),
    loop_start_fail(
      fun gen_statem:Start/3,
      {ets, {return, ignore}}, Opts,
      fun (ignore) -> ok end),
    loop_start_fail(
      fun gen_statem:Start/3,
      {ets, {return, 4711}}, Opts,
      fun ({error, {bad_return_from_init, 4711}}) -> ok end),
    loop_start_fail(
      fun gen_statem:Start/3,
      {ets, {crash, error, bailout}}, Opts,
      fun ({error, bailout}) -> ok end),
    loop_start_fail(
      fun gen_statem:Start/3,
      {ets, {crash, exit, bailout}}, Opts,
      fun ({error, bailout}) -> ok end),
    loop_start_fail(
      fun gen_statem:Start/3,
      {ets, {wait, 1000, void}}, [{timeout, 200} | Opts],
      fun ({error, timeout}) -> ok end),
    loop_start_fail(Config, Start_Opts).

loop_start_fail(GenStartFun, Arg, Opts, ValidateFun) ->
    loop_start_fail(GenStartFun, Arg, Opts, ValidateFun, 5).
%%
loop_start_fail(_GenStartFun, _Arg, _Opts, _ValidateFun, 0) ->
    ok;
loop_start_fail(GenStartFun, Arg, Opts, ValidateFun, N) ->
    ok = ValidateFun(GenStartFun(?MODULE, Arg, Opts)),
    loop_start_fail(GenStartFun, Arg, Opts, ValidateFun, N - 1).



stop_and_reply(_Config) ->
    process_flag(trap_exit, true),

    Machine =
	%% Abusing the internal format of From...
	#{init =>
	      fun () ->
		      {ok,start,undefined}
	      end,
	  start =>
	      fun (cast, {echo,From1,Reply1}, undefined) ->
		      {next_state,wait,{reply,From1,Reply1}}
	      end,
	  wait =>
	      fun (cast, {stop_and_reply,Reason,From2,Reply2},R1) ->
		      {stop_and_reply,Reason,
		       [R1,{reply,From2,Reply2}]}
	      end},
    {ok,STM} =
	gen_statem:start_link(?MODULE, {map_statem,Machine,[]}, []),

    Self = self(),
    Tag1 = make_ref(),
    gen_statem:cast(STM, {echo,{Self,Tag1},reply1}),
    Tag2 = make_ref(),
    gen_statem:cast(STM, {stop_and_reply,reason,{Self,Tag2},reply2}),
    case flush() of
	[{Tag1,reply1},{Tag2,reply2},{'EXIT',STM,reason}] ->
	    ok;
	Other1 ->
	    ct:fail({unexpected,Other1})
    end,

    {noproc,_} =
	?EXPECT_FAILURE(gen_statem:call(STM, hej), Reason),
    case flush() of
	[] ->
	    ok;
	Other2 ->
	    ct:fail({unexpected,Other2})
    end.



state_enter(_Config) ->
    process_flag(trap_exit, true),
    Self = self(),

    Machine =
	%% Abusing the internal format of From...
	#{init =>
	      fun () ->
		      {ok,start,1}
	      end,
	  start =>
	      fun (enter, Prev, N) ->
		      Self ! {N,enter,start,Prev},
		      {keep_state,N + 1};
		  (internal, Prev, N) ->
		      Self ! {N,internal,start,Prev},
		      {keep_state,N + 1};
                  (timeout, M, N) ->
                      {keep_state, N + 1,
                       {reply, {Self,N}, {timeout,M}}};
		  ({call,From}, repeat, N) ->
		      {repeat_state,N + 1,
		       [{reply,From,{N,repeat,start}}]};
		  ({call,From}, echo, N) ->
		      {next_state,wait,N + 1,
		       [{reply,From,{N,echo,start}},{timeout,0,N}]};
		  ({call,From}, {stop,Reason}, N) ->
		      {stop_and_reply,Reason,
		       [{reply,From,{N,stop}}],N + 1}
	      end,
	  wait =>
	      fun (enter, Prev, N) when N < 5 ->
		      {repeat_state,N + 1,
		       [{reply,{Self,N},{enter,Prev}},
                        {timeout,0,N},
                        {state_timeout,0,N}]};
		  (enter, Prev, N) ->
		      Self ! {N,enter,wait,Prev},
		      {keep_state,N + 1,
                       [{timeout,0,N},
                        {state_timeout,0,N}]};
                  (timeout, M, N) ->
                      {keep_state, N + 1,
                       {reply, {Self,N}, {timeout,M}}};
                  (state_timeout, M, N) ->
                      {keep_state, N + 1,
                       {reply, {Self,N}, {state_timeout,M}}};
		  ({call,From}, repeat, N) ->
		      {repeat_state_and_data,
		       [{reply,From,{N,repeat,wait}},
                        {timeout,0,N}]};
		  ({call,From}, echo, N) ->
		      {next_state,start,N + 1,
		       [{next_event,internal,wait},
			{reply,From,{N,echo,wait}}]}
	      end},
    {ok,STM} =
	gen_statem:start_link(
	  ?MODULE, {map_statem,Machine,[state_enter]},
          [{debug,[trace,{log,17}]}]),
    ok = sys:log(STM, false),
    ok = sys:log(STM, true),

    [{1,enter,start,start}] = flush(),
    {2,echo,start} = gen_statem:call(STM, echo),
    [{3,{enter,start}},
     {4,{enter,start}},
     {5,enter,wait,start},
     {6,{timeout,5}},
     {7,{state_timeout,5}}] = flush(),
    {wait,[8|_]} = sys:get_state(STM),
    {8,repeat,wait} = gen_statem:call(STM, repeat),
    [{8,enter,wait,wait},
     {9,{timeout,8}},
     {10,{state_timeout,8}}] = flush(),
    {11,echo,wait} = gen_statem:call(STM, echo),
    [{12,enter,start,wait},
     {13,internal,start,wait}] = flush(),
    {14,repeat,start} = gen_statem:call(STM, repeat),
    [{15,enter,start,start}] = flush(),

    {ok,Log} = sys:log(STM, get),
    io:format("sys:log ~p~n", [Log]),
    ok = sys:log(STM, print),

    {16,stop} = gen_statem:call(STM, {stop,bye}),
    [{'EXIT',STM,bye}] = flush(),

    {noproc,_} =
	?EXPECT_FAILURE(gen_statem:call(STM, hej), Reason),
    case flush() of
	[] ->
	    ok;
	Other2 ->
	    ct:fail({unexpected,Other2})
    end.



event_order(_Config) ->
    process_flag(trap_exit, true),

    Machine =
	%% Abusing the internal format of From...
	#{init =>
	      fun () ->
		      {ok,start,undefined}
	      end,
	  start =>
	      fun (cast, _, _) ->
		      {keep_state_and_data,postpone}; %% Handled in 'buffer'
		  ({call,From}, {buffer,Pid,[Tag3,Tag4,Tag5]},
		   undefined) ->
		      {next_state,buffer,[],
		       [{next_event,internal,{reply,{Pid,Tag3},ok3}},
			{next_event,internal,{reply,{Pid,Tag4},ok4}},
			{timeout,0,{reply,{Pid,Tag5},ok5}},
			%% The timeout should not happen since there
			%% are events that cancel it i.e next_event
			%% and postponed
			{reply,From,ok}]}
	      end,
	  buffer =>
	      fun (internal, Reply, Replies) ->
		      {keep_state,[Reply|Replies]};
		  (timeout, Reply, Replies) ->
		      {keep_state,[Reply|Replies]};
		  (cast, Reply, Replies) ->
		      {keep_state,[Reply|Replies]};
		  ({call,From}, {stop,Reason}, Replies) ->
		      {next_state,stop,undefined,
		       lists:reverse(
			 Replies,
			 [{reply,From,ok},
			  {next_event,internal,{stop,Reason}}])}
	      end,
	  stop =>
	      fun (internal, Result, undefined) ->
		      Result
	      end},

    {ok,STM} = gen_statem:start_link(?MODULE, {map_statem,Machine,[]}, []),
    Self = self(),
    Tag1 = make_ref(),
    gen_statem:cast(STM, {reply,{Self,Tag1},ok1}),
    Tag2 = make_ref(),
    gen_statem:cast(STM, {reply,{Self,Tag2},ok2}),
    Tag3 = make_ref(),
    Tag4 = make_ref(),
    Tag5 = make_ref(),
    ok = gen_statem:call(STM, {buffer,Self,[Tag3,Tag4,Tag5]}),
    ok = gen_statem:call(STM, {stop,reason}),
    case flush() of
	[{Tag3,ok3},{Tag4,ok4},{Tag1,ok1},{Tag2,ok2},
	 {'EXIT',STM,reason}] ->
	    ok;
	Other1 ->
	    ct:fail({unexpected,Other1})
    end,

    {noproc,_} =
	?EXPECT_FAILURE(gen_statem:call(STM, hej), Reason),
    case flush() of
	[] ->
	    ok;
	Other2 ->
	    ct:fail({unexpected,Other2})
    end.



state_timeout(_Config) ->
    process_flag(trap_exit, true),

    Machine =
	#{init =>
	      fun () ->
		      {ok,start,0}
	      end,
	  start =>
	      fun
		  ({call,From}, {go,Time}, 0)  ->
		      self() ! message_to_self,
		      {next_state, state1, {Time,From},
		       %% Verify that internal events goes before external
		       [{timeout,Time,1}, % Exercise different cancel code path
                        {state_timeout,Time,1},
			{next_event,internal,1}]}
	      end,
	  state1 =>
	      fun
		  (internal, 1, Data) ->
		      %% Verify that a state change cancels timeout 1
		      {next_state, state2, Data,
		       [{timeout,0,2},
			{state_timeout,0,2},
			{next_event,internal,2}]}
	      end,
	  state2 =>
	      fun
		  (internal, 2, Data) ->
		      %% Verify that {state_timeout,0,_}
		      %% comes after next_event and that
		      %% {timeout,0,_} is cancelled by
		      %% pending {state_timeout,0,_}
		      {keep_state, {ok,2,Data},
		       [{timeout,0,3}]};
		  (state_timeout, 2, {ok,2,Data}) ->
		      %% Verify that timeout 0's are processed
		      %% in order
		      {keep_state, {ok,3,Data},
		       [{timeout,0,4},{state_timeout,0,5}]};
		  (timeout, 4, {ok,3,Data}) ->
		      %% Verify that timeout 0 is cancelled by
		      %% a state_timeout 0 event and that
		      %% state_timeout 0 can be restarted
		      {keep_state, {ok,4,Data},
		       [{state_timeout,0,6},{timeout,0,7}]};
		  (state_timeout, 6, {ok,4,{Time,From}}) ->
		      {next_state, state3, 6,
		       [{reply,From,ok},
			{state_timeout,Time,8}]}
	      end,
	  state3 =>
	      fun
		  (info, message_to_self, 6) ->
		      {keep_state, 7};
		  ({call,From}, check, 7) ->
		      {keep_state, From};
		  (state_timeout, 8, From) ->
		      {stop_and_reply, normal,
		       {reply,From,ok}}
	      end},

    {ok,STM} =
        gen_statem:start_link(
          ?MODULE, {map_statem,Machine,[]}, [{debug,[trace]}]),
    TRef = erlang:start_timer(1000, self(), kull),
    ok = gen_statem:call(STM, {go,500}),
    ok = gen_statem:call(STM, check),
    receive
	{timeout,TRef,kull} ->
	    ct:fail(late_timeout)
    after 0 ->
	    receive
		{timeout,TRef,kull} ->
		    ok
	    after 1000 ->
		    ct:fail(no_check_timeout)
	    end
    end,
    receive
	{'EXIT',STM,normal} ->
	    ok
    after 500 ->
	    ct:fail(did_not_stop)
    end,

    verify_empty_msgq().



timeout_cancel_and_update(_Config) ->
    process_flag(trap_exit, true),
    %%
    Machine =
	#{init =>
	      fun () ->
		      {ok,start,0}
	      end,
	  start =>
	      fun
		  ({call,From}, test, 0)  ->
		      self() ! message_to_self,
		      {next_state, state1, From,
		       %% Verify that internal events goes before external
		       [{state_timeout,17,1},
			{next_event,internal,1}]}
	      end,
	  state1 =>
	      fun
		  (internal, 1, _) ->
                      {keep_state_and_data,
                       [{state_timeout,cancel},
                        {{timeout,a},17,1}]};
                  (info, message_to_self, _) ->
                      {keep_state_and_data,
                       [{{timeout,a},update,a}]};
                  ({timeout,a}, a, Data) ->
                      {next_state,state2,Data,
                       [{state_timeout,17,2},
                        {next_event,internal,2}]}
              end,
	  state2 =>
	      fun
		  (internal, 2, _) ->
                      receive after 50 -> ok end,
                      %% Now state_timeout 17 should have triggered
                      {keep_state_and_data,
                       [{state_timeout,update,b},
                        {timeout,17,2}]};
                  (state_timeout, b, From) ->
                      {next_state,state3,3,
                       [{reply,From,ok},
                        17000]}
	      end,
          state3 =>
              fun
                  ({call,From}, stop, 3) ->
                      {stop_and_reply, normal,
                       [{reply,From,ok}]}
              end
         },
    %%
    {ok,STM} =
        gen_statem:start_link(
          ?MODULE, {map_statem,Machine,[]}, [{debug,[trace]}]),
    ok = gen_statem:call(STM, test),
    {status, STM, {module,gen_statem}, Info} = sys:get_status(STM),
    ct:log("Status info: ~p~n", [Info]),
    {_,Timeouts} = dig_data_tuple(Info),
    {_, {1,[{timeout,17000}]}} = lists:keyfind("Time-outs", 1, Timeouts),
    %%
    ok = gen_statem:call(STM, stop),
    receive
	{'EXIT',STM,normal} ->
	    ok
    after 500 ->
	    ct:fail(did_not_stop)
    end,
    %%
    verify_empty_msgq().

dig_data_tuple([{data,_} = DataTuple|_]) -> DataTuple;
dig_data_tuple([H|T]) when is_list(H) ->
    case dig_data_tuple(H) of
        false -> dig_data_tuple(T);
        DataTuple -> DataTuple
    end;
dig_data_tuple([_|T]) -> dig_data_tuple(T);
dig_data_tuple([]) -> false.



%% Test that all event types can be sent with {next_event,EventType,_}
event_types(_Config) ->
    process_flag(trap_exit, true),

    Machine =
	%% Abusing the internal format of From...
	#{init =>
	      fun () ->
		      {ok, start1, undefined,
		       [{next_event,internal,0}]}
	      end,
	  start1 =>
	      fun (internal, 0, undefined) ->
		      {next_state, start2, undefined}
	      end,
	  start2 =>
	      fun ({call,_} = Call, Req, undefined) ->
		      {next_state, state1, undefined,
		       [{next_event,internal,1},
			{next_event,state_timeout,2},
			{next_event,timeout,3},
			{next_event,info,4},
			{next_event,cast,5},
			{next_event,{timeout,6}, 6},
			{next_event,Call,Req}]}
	      end,
	  state1 =>
	      fun (internal, 1, undefined) ->
		      {next_state, state2, undefined}
	      end,
	  state2 =>
	      fun (state_timeout, 2, undefined) ->
		      {next_state, state3, undefined}
	      end,
	  state3 =>
	      fun (timeout, 3, undefined) ->
		      {next_state, state4, undefined}
	      end,
	  state4 =>
	      fun (info, 4, undefined) ->
		      {next_state, state5, undefined}
	      end,
	  state5 =>
	      fun (cast, 5, undefined) ->
		      {next_state, state6, undefined}
	      end,
	  state6 =>
	      fun ({timeout,6}, 6, undefined) ->
		      {next_state, state7, undefined}
	      end,
	  state7 =>
	      fun ({call,From}, stop, undefined) ->
		      {stop_and_reply, shutdown,
		       [{reply,From,stopped}]}
	      end},
    {ok,STM} =
	gen_statem:start_link(
	  ?MODULE, {map_statem,Machine,[]}, [{debug,[trace]}]),

    stopped = gen_statem:call(STM, stop),
    receive
	{'EXIT',STM,shutdown} ->
	    ok
    after 500 ->
	    ct:fail(did_not_stop)
    end,

    {noproc,_} =
	?EXPECT_FAILURE(gen_statem:call(STM, hej), Reason),
    case flush() of
	[] ->
	    ok;
	Other2 ->
	    ct:fail({unexpected,Other2})
    end.



generic_timers(_Config) ->
    process_flag(trap_exit, true),

    Machine =
	%% Abusing the internal format of From...
	#{init =>
	      fun () ->
		      {ok, start, undefined}
	      end,
	  start =>
	      fun ({call,_} = Call, Req, undefined) ->
		      {next_state, state1, undefined,
		       [{{timeout,a},1500,1},
			{state_timeout,1500,1},
			{{timeout,b},1000,1},
			{next_event,Call,Req}]}
	      end,
	  state1 =>
	      fun ({call,_} = Call, Req, undefined) ->
		      T = erlang:monotonic_time(millisecond) + 500,
		      {next_state, state2, undefined,
		       [{{timeout,c},T,2,{abs,true}},
			{{timeout,d},0,2,[{abs,false}]},
			{timeout,0,2},
			{{timeout,b},infinity,2},
			{{timeout,a},1000,{Call,Req}}]}
	      end,
	  state2 =>
	      fun ({timeout,d}, 2, undefined) ->
		      {next_state, state3, undefined}
	      end,
	  state3 =>
	      fun ({timeout,c}, 2, undefined) ->
		      {next_state, state4, undefined}
	      end,
	  state4 =>
	      fun ({timeout,a}, {{call,From},stop}, undefined) ->
		      {stop_and_reply, shutdown,
		       [{reply,From,stopped}]}
	      end},
    {ok,STM} =
	gen_statem:start_link(
	  ?MODULE, {map_statem,Machine,[]}, [{debug,[trace]}]),

    stopped = gen_statem:call(STM, stop),
    receive
	{'EXIT',STM,shutdown} ->
	    ok
    after 500 ->
	    ct:fail(did_not_stop)
    end,

    {noproc,_} =
	?EXPECT_FAILURE(gen_statem:call(STM, hej), Reason),
    case flush() of
	[] ->
	    ok;
	Other2 ->
	    ct:fail({unexpected,Other2})
    end.



sys1(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    {status, Pid, {module,gen_statem}, Info} = sys:get_status(Pid),
    ct:log("Status info: ~p~n", [Info]),
    sys:suspend(Pid),
    Parent = self(),
    Tag = make_ref(),
    Caller =
	spawn(
	  fun () ->
		  Parent ! {Tag,gen_statem:call(Pid, hej)}
	  end),
    receive
	{Tag,_} ->
	    ct:fail(should_be_suspended)
    after 3000 ->
	    exit(Caller, ok)
    end,

    %% {timeout,_} =
    %% 	?EXPECT_FAILURE(gen_statem:call(Pid, hej), Reason),
    sys:resume(Pid),
    stop_it(Pid).

code_change(_Config) ->
    {ok,Pid} =
	gen_statem:start(
	  ?MODULE, {callback_mode,state_functions,[]}, []),
    {idle,data} = sys:get_state(Pid),
    sys:suspend(Pid),
    Mode = handle_event_function,
    sys:change_code(Pid, ?MODULE, old_vsn, Mode),
    sys:resume(Pid),
    {idle,{old_vsn,data,Mode}} = sys:get_state(Pid),
    Mode = gen_statem:call(Pid, get_callback_mode),
    stop_it(Pid).

call_format_status(Config) ->
    call_format_status(Config,?MODULE,format_status_called),
    call_format_status(Config, format_status_statem,
                       {data,[{"State",{format_status_called,format_data}}]}).
call_format_status(Config, Module, Match) ->
    {ok,Pid} = gen_statem:start(Module, start_arg(Config, []), []),
    Status = sys:get_status(Pid),
    {status,Pid,_Mod,[_PDict,running,_,_, Data]} = Status,
    [Match|_] = lists:reverse(Data),
    stop_it(Pid),

    %% check that format_status can handle a name being an atom (pid is
    %% already checked by the previous test)
    {ok, Pid2} =
	gen_statem:start(
	  {local, gstm}, Module, start_arg(Config, []), []),
    Status2 = sys:get_status(gstm),
    {status,Pid2,Mod,[_PDict2,running,_,_,Data2]} = Status2,
    [Match|_] = lists:reverse(Data2),
    stop_it(Pid2),

    %% check that format_status can handle a name being a term other than a
    %% pid or atom
    GlobalName1 = {global,"CallFormatStatus"},
    {ok,Pid3} =
	gen_statem:start(
	  GlobalName1, Module, start_arg(Config, []), []),
    Status3 = sys:get_status(GlobalName1),
    {status,Pid3,Mod,[_PDict3,running,_,_,Data3]} = Status3,
    [Match|_] = lists:reverse(Data3),
    stop_it(Pid3),
    GlobalName2 = {global,{name, "term"}},
    {ok,Pid4} =
	gen_statem:start(
	  GlobalName2, Module, start_arg(Config, []), []),
    Status4 = sys:get_status(GlobalName2),
    {status,Pid4,Mod,[_PDict4,running,_,_, Data4]} = Status4,
    [Match|_] = lists:reverse(Data4),
    stop_it(Pid4),

    %% check that format_status can handle a name being a term other than a
    %% pid or atom
    dummy_via:reset(),
    ViaName1 = {via,dummy_via,"CallFormatStatus"},
    {ok,Pid5} = gen_statem:start(ViaName1, Module, start_arg(Config, []), []),
    Status5 = sys:get_status(ViaName1),
    {status,Pid5,Mod, [_PDict5,running,_,_, Data5]} = Status5,
    [Match|_] = lists:reverse(Data5),
    stop_it(Pid5),
    ViaName2 = {via,dummy_via,{name,"term"}},
    {ok, Pid6} =
	gen_statem:start(
	  ViaName2, Module, start_arg(Config, []), []),
    Status6 = sys:get_status(ViaName2),
    {status,Pid6,Mod,[_PDict6,running,_,_,Data6]} = Status6,
    [Match|_] = lists:reverse(Data6),
    stop_it(Pid6).

error_format_status(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    try
        error_format_status(Config,?MODULE,{formatted,idle,"called format_status"}),
        error_format_status(Config,format_status_statem,
                            {{formatted,idle},{formatted,"called format_status"}})
    after
            process_flag(trap_exit, OldFl),
            error_logger_forwarder:unregister()
    end.
error_format_status(Config,Module,Match) ->
    Data = "called format_status",
    {ok,Pid} =
	gen_statem:start(
	  Module, start_arg(Config, {data,Data}), []),
    %% bad return value in the gen_statem loop
    {{{bad_return_from_state_function,badreturn},_},_} =
	?EXPECT_FAILURE(gen_statem:call(Pid, badreturn), Reason),
    receive
	{error,_,
	 {Pid,
	  "** State machine"++_,
	  [Pid,{{call,_},badreturn},
	   Match,
	   error,{bad_return_from_state_function,badreturn}|_]}} ->
	    ok;
	Other when is_tuple(Other), element(1, Other) =:= error ->
	    ct:fail({unexpected,Other})
    after 1000 ->
	    ct:fail({timeout,(fun F() -> receive M -> [M|F()] after 0 -> [] end end)()})
    end,

    receive
	%% Comes with SASL
	{error_report,_,{Pid,crash_report,_}} ->
	    ok
    after 500 ->
	    ok
    end,
    ok = verify_empty_msgq().

terminate_crash_format(Config) ->
    dbg:tracer(),
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    try
        terminate_crash_format(Config,?MODULE,{formatted,idle,crash_terminate}),
        terminate_crash_format(Config,format_status_statem,
                               {{formatted,idle},{formatted,crash_terminate}})
    after
        dbg:stop(),
        process_flag(trap_exit, OldFl),
        error_logger_forwarder:unregister()
    end.

terminate_crash_format(Config, Module, Match) ->
    Data = crash_terminate,
    {ok,Pid} =
	gen_statem:start(
	  Module, start_arg(Config, {data,Data}), []),
    stop_it(Pid),
    Self = self(),
    receive
	{error,_GroupLeader,
	 {Pid,
	  "** State machine"++_,
	  [Pid,
	   {{call,{Self,_}},stop},
	   Match,exit,{crash,terminate}|_]}} ->
	    ok;
	Other when is_tuple(Other), element(1, Other) =:= error ->
	    ct:fail({unexpected,Other})
    after 1000 ->
	    ct:fail({timeout,flush()})
    end,

    receive
	%% Comes with SASL
	{error_report,_,{Pid,crash_report,_}} ->
	    ok
    after 500 ->
	    ok
    end,
    ok = verify_empty_msgq().

%% We test that all of the different status items can be
%% formatted by the format_status/1 callback.
format_all_status(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),

    Data = fun(M) ->
                   maps:map(
                     fun(Key, Values) when Key =:= log;
                                           Key =:= queue;
                                           Key =:= postponed;
                                           Key =:= timeouts ->
                             [{Key, Value} || Value <- Values];
                        (Key, Value) ->
                             {Key, Value}
                     end, M)
           end,
    {ok,Pid} =
	gen_statem:start(
	  format_status_statem, start_arg(Config, {data,Data}), []),
    sys:log(Pid, true),
    ok = gen_statem:cast(Pid, postpone_event),
    ok = gen_statem:cast(Pid, {timeout, 100000}),

    {status,Pid, _, [_,_,_,_,Info]} = sys:get_status(Pid),
    [{header, _Hdr},
     {data, [_Status,_Parent,_Modules,
             {"Time-outs",{1,[{timeouts,_}]}},
             {"Logged Events",[{log,_}|_]},
             {"Postponed",[{postponed,_}]}]},
     {data, [{"State",{{state,idle},{data,Data}}}]}] = Info,

    %% bad return value in the gen_statem loop
    {{{bad_return_from_state_function,badreturn},_},_} =
	?EXPECT_FAILURE(gen_statem:call(Pid, badreturn), Reason),
    Self = self(),
    receive
	{error,_GroupLeader,
	 {Pid,
	  "** State machine"++_,
	  [Pid,
	   {queue,{{call,{Self,_}},badreturn}},
	   {{state,idle},{data,Data}},error,
           {reason,{bad_return_from_state_function,badreturn}},
           __Modules,_StateFunctions,
           [{postponed,{cast,postpone_event}}],
           [_|_] = _Stacktrace,
           {1,[{timeouts,{timeout,idle}}]},
           [{log,_}|_] |_]}} ->
	    ok;
	Other when is_tuple(Other), element(1, Other) =:= error ->
	    ct:fail({unexpected,Other})
    after 1000 ->
	    ct:fail({timeout,flush()})
    end,
    receive
        {error_report,_,_} -> ok
    end,
    error_logger_forwarder:unregister(),
    process_flag(trap_exit, OldFl),
    ok.

get_state(Config) ->
    State = self(),
    {ok,Pid} =
	gen_statem:start(
	  ?MODULE, start_arg(Config, {data,State}), []),
    {idle,State} = sys:get_state(Pid),
    {idle,State} = sys:get_state(Pid, 5000),
    stop_it(Pid),

    %% check that get_state can handle a name being an atom (pid is
    %% already checked by the previous test)
    {ok,Pid2} =
	gen_statem:start(
	  {local,gstm}, ?MODULE, start_arg(Config, {data,State}), []),
    {idle,State} = sys:get_state(gstm),
    {idle,State} = sys:get_state(gstm, 5000),
    stop_it(Pid2),

    %% check that get_state works when pid is sys suspended
    {ok,Pid3} =
	gen_statem:start(
	  ?MODULE, start_arg(Config, {data,State}), []),
    {idle,State} = sys:get_state(Pid3),
    ok = sys:suspend(Pid3),
    {idle,State} = sys:get_state(Pid3, 5000),
    ok = sys:resume(Pid3),
    stop_it(Pid3),
    ok = verify_empty_msgq().

replace_state(Config) ->
    State = self(),
    {ok, Pid} =
	gen_statem:start(
	  ?MODULE, start_arg(Config, {data,State}), []),
    {idle,State} = sys:get_state(Pid),
    NState1 = "replaced",
    Replace1 = fun({StateName, _}) -> {StateName,NState1} end,
    {idle,NState1} = sys:replace_state(Pid, Replace1),
    {idle,NState1} = sys:get_state(Pid),
    NState2 = "replaced again",
    Replace2 = fun({idle, _}) -> {state0,NState2} end,
    {state0,NState2} = sys:replace_state(Pid, Replace2, 5000),
    {state0,NState2} = sys:get_state(Pid),
    %% verify no change in state if replace function crashes
    Replace3 = fun(_) -> error(fail) end,
    {callback_failed,
     {gen_statem,system_replace_state},{error,fail}} =
	?EXPECT_FAILURE(sys:replace_state(Pid, Replace3), Reason),
    {state0, NState2} = sys:get_state(Pid),
    %% verify state replaced if process sys suspended
    ok = sys:suspend(Pid),
    Suffix2 = " and again",
    NState3 = NState2 ++ Suffix2,
    Replace4 = fun({StateName, _}) -> {StateName, NState3} end,
    {state0,NState3} = sys:replace_state(Pid, Replace4),
    ok = sys:resume(Pid),
    {state0,NState3} = sys:get_state(Pid, 5000),
    %% State 'error' does not exist but is never touched,
    %% just verify that sys handles it as a state, not as an error return
    {error,NState3} =
        sys:replace_state(Pid, fun ({state0, SD}) -> {error, SD} end),
    {error, NState3} = sys:get_state(Pid),
    {state0,NState3} =
        sys:replace_state(Pid, fun ({error, SD}) -> {state0, SD} end),
    stop_it(Pid),
    ok = verify_empty_msgq().

%% Hibernation
hibernate(Config) ->
    OldFl = process_flag(trap_exit, true),
    WaitHibernate = 500,

    {ok,Pid0} =
	gen_statem:start_link(
	  ?MODULE, start_arg(Config, hiber_now), []),
    wait_erlang_hibernate(Pid0, WaitHibernate),
    stop_it(Pid0),
    receive
	{'EXIT',Pid0,normal} -> ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    {ok,Pid} =
	gen_statem:start_link(?MODULE, start_arg(Config, hiber), []),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid,current_function)),
    hibernating = gen_statem:call(Pid, hibernate_sync),
    wait_erlang_hibernate(Pid, WaitHibernate),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid, WaitHibernate),
    hibernating = gen_statem:call(Pid, hibernate_sync),
    wait_erlang_hibernate(Pid, WaitHibernate),
    please_just_five_more = gen_statem:call(Pid, snooze_sync),
    wait_erlang_hibernate(Pid, WaitHibernate),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid, WaitHibernate),
    ok = gen_statem:cast(Pid, hibernate_async),
    wait_erlang_hibernate(Pid, WaitHibernate),
    ok = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid, WaitHibernate),
    ok = gen_statem:cast(Pid, hibernate_async),
    wait_erlang_hibernate(Pid, WaitHibernate),
    ok = gen_statem:cast(Pid, snooze_async),
    wait_erlang_hibernate(Pid, WaitHibernate),
    ok = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid, WaitHibernate),

    Pid ! {hibernate_later, WaitHibernate div 2},
    true =
	({current_function,{erlang,hibernate,3}} =/=
	     erlang:process_info(Pid, current_function)),
    wait_erlang_hibernate(Pid, WaitHibernate),

    'alive!' = gen_statem:call(Pid, 'alive?'),
    true =
	({current_function,{erlang,hibernate,3}} =/=
	     erlang:process_info(Pid, current_function)),
    Pid ! hibernate_now,
    wait_erlang_hibernate(Pid, WaitHibernate),

    'alive!' = gen_statem:call(Pid, 'alive?'),
    true =
	({current_function,{erlang,hibernate,3}} =/=
	     erlang:process_info(Pid, current_function)),

    hibernating = gen_statem:call(Pid, hibernate_sync),
    wait_erlang_hibernate(Pid, WaitHibernate),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid, WaitHibernate),
    hibernating = gen_statem:call(Pid, hibernate_sync),
    wait_erlang_hibernate(Pid, WaitHibernate),
    please_just_five_more = gen_statem:call(Pid, snooze_sync),
    wait_erlang_hibernate(Pid, WaitHibernate),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid, WaitHibernate),
    ok = gen_statem:cast(Pid, hibernate_async),
    wait_erlang_hibernate(Pid, WaitHibernate),
    ok  = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid, WaitHibernate),
    ok = gen_statem:cast(Pid, hibernate_async),
    wait_erlang_hibernate(Pid, WaitHibernate),
    ok = gen_statem:cast(Pid, snooze_async),
    wait_erlang_hibernate(Pid, WaitHibernate),
    ok = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid, WaitHibernate),

    hibernating = gen_statem:call(Pid, hibernate_sync),
    wait_erlang_hibernate(Pid, WaitHibernate),
    sys:suspend(Pid),
    wait_erlang_hibernate(Pid, WaitHibernate),
    sys:resume(Pid),
    wait_erlang_hibernate(Pid, WaitHibernate),
    receive after WaitHibernate -> ok end,
    wait_erlang_hibernate(Pid, WaitHibernate),

    good_morning  = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid, WaitHibernate),
    stop_it(Pid),
    process_flag(trap_exit, OldFl),
    receive
	{'EXIT',Pid,normal} -> ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,
    ok = verify_empty_msgq().

%% Auto-hibernation timeout
auto_hibernate(Config) ->
    OldFl = process_flag(trap_exit, true),
    HibernateAfterTimeout = 500,
    WaitTime = 1000,

    {ok,Pid} =
        gen_statem:start_link(
            ?MODULE, start_arg(Config, []),
          [{hibernate_after, HibernateAfterTimeout}]),
    %% After init test
    is_not_in_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    timer:sleep(HibernateAfterTimeout),
    wait_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    %% After info test
    Pid ! {hping, self()},
    receive
        {Pid, hpong} ->
            ok
    after WaitTime ->
        ct:fail(info)
    end,
    is_not_in_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    timer:sleep(HibernateAfterTimeout),
    wait_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    %% After cast test
    ok = gen_statem:cast(Pid, {hping, self()}),
    receive
        {Pid, hpong} ->
            ok
    after WaitTime ->
        ct:fail(cast)
    end,
    is_not_in_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    timer:sleep(HibernateAfterTimeout),
    wait_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    %% After call test
    hpong = gen_statem:call(Pid, hping),
    is_not_in_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    timer:sleep(HibernateAfterTimeout),
    wait_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    %% Timer test 1
    TimerTimeout1 = HibernateAfterTimeout div 2,
    ok = gen_statem:call(Pid, {start_htimer, self(), TimerTimeout1}),
    is_not_in_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    timer:sleep(TimerTimeout1),
    is_not_in_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    receive
        {Pid, htimer_timeout} ->
            ok
    after WaitTime ->
        ct:fail(timer1)
    end,
    is_not_in_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    timer:sleep(HibernateAfterTimeout),
    wait_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    %% Timer test 2
    TimerTimeout2 = HibernateAfterTimeout * 2,
    ok = gen_statem:call(Pid, {start_htimer, self(), TimerTimeout2}),
    is_not_in_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    timer:sleep(HibernateAfterTimeout),
    wait_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    receive
        {Pid, htimer_timeout} ->
            ok
    after TimerTimeout2 ->
        ct:fail(timer2)
    end,
    is_not_in_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    timer:sleep(HibernateAfterTimeout),
    wait_erlang_hibernate(Pid, 2 * HibernateAfterTimeout),
    stop_it(Pid),
    process_flag(trap_exit, OldFl),
    receive
        {'EXIT',Pid,normal} -> ok
    after 5000 ->
        ct:fail(gen_statem_did_not_die)
    end,
    ok = verify_empty_msgq().


wait_erlang_hibernate(Pid, Time) ->
    receive after 1 -> ok end,
    wait_erlang_hibernate_1(Pid, Time, Time div 100).

wait_erlang_hibernate_1(Pid, Time, _T) when Time =< 0 ->
    ct:log("~p\n", [erlang:process_info(Pid, current_function)]),
    ct:fail(should_be_in_erlang_hibernate_3);
wait_erlang_hibernate_1(Pid, Time, T) ->
    {current_function,MFA} = erlang:process_info(Pid, current_function),
    case MFA of
	{erlang,hibernate,3} ->
	    ok;
	_ ->
	    receive after T -> ok end,
	    wait_erlang_hibernate_1(Pid, Time - T, T)
    end.

is_not_in_erlang_hibernate(Pid, Time) ->
    receive after 1 -> ok end,
    is_not_in_erlang_hibernate_1(Pid, Time, Time div 100).

is_not_in_erlang_hibernate_1(_Pid, Time, _T) when Time =< 0 ->
    ct:fail(should_not_be_in_erlang_hibernate_3);
is_not_in_erlang_hibernate_1(Pid, Time, T) ->
    {current_function,MFA} = erlang:process_info(Pid, current_function),
    case MFA of
 	{erlang,hibernate,3} ->
	    receive after T -> ok end,
	    is_not_in_erlang_hibernate_1(Pid, Time - T, T);
 	_ ->
 	    ok
    end.


enter_loop(_Config) ->
    OldFlag = process_flag(trap_exit, true),

    dummy_via:reset(),

    %% Locally registered process + {local,Name}
    {ok,Pid1a} =
	proc_lib:start_link(
          ?MODULE, enter_loop, [local,local,[{debug,[{log,7}]}]]),
    yes = gen_statem:call(Pid1a, 'alive?'),
    stopped = gen_statem:call(Pid1a, stop),
    receive
	{'EXIT',Pid1a,normal} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Unregistered process + {local,Name}
    {ok,Pid1b} =
	proc_lib:start_link(
          ?MODULE, enter_loop, [anon,local,[{debug,[log]}]]),
    receive
	{'EXIT',Pid1b,process_not_registered} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Globally registered process + {global,Name}
    {ok,Pid2a} =
	proc_lib:start_link(?MODULE, enter_loop, [global,global]),
    yes = gen_statem:call(Pid2a, 'alive?'),
    stopped = gen_statem:call(Pid2a, stop),
    receive
	{'EXIT',Pid2a,normal} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Unregistered process + {global,Name}
    {ok,Pid2b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,global]),
    receive
	{'EXIT',Pid2b,process_not_registered_globally} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Unregistered process + no name
    {ok,Pid3} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,anon]),
    yes = gen_statem:call(Pid3, 'alive?'),
    stopped = gen_statem:call(Pid3, stop),
    receive
	{'EXIT',Pid3,normal} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Process not started using proc_lib
    Pid4 = spawn_link(gen_statem, enter_loop, [?MODULE,[],state0,[]]),
    receive
	{'EXIT',Pid4,process_was_not_started_by_proc_lib} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Make sure I am the parent, ie that ordering a shutdown will
    %% result in the process terminating with Reason==shutdown
    {ok,Pid5} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,anon]),
    yes = gen_statem:call(Pid5, 'alive?'),
    exit(Pid5, shutdown),
    receive
	{'EXIT',Pid5,shutdown} ->
	    ok
    after 5000 ->
	    ct:fail(gen_statem_did_not_die)
    end,

    %% Make sure gen_statem:enter_loop does not accept {local,Name}
    %% when it's another process than the calling one which is
    %% registered under that name
    register(armitage, self()),
    {ok,Pid6a} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,local]),
    receive
	{'EXIT',Pid6a,process_not_registered} ->
	    ok
    after 1000 ->
	    ct:fail(gen_statem_started)
    end,
    unregister(armitage),

    %% Make sure gen_statem:enter_loop does not accept {global,Name}
    %% when it's another process than the calling one which is
    %% registered under that name
    global:register_name(armitage, self()),
    {ok,Pid6b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,global]),
    receive
	{'EXIT',Pid6b,process_not_registered_globally} ->
	    ok
    after 1000 ->
	    ct:fail(gen_statem_started)
    end,
    global:unregister_name(armitage),

    dummy_via:register_name(armitage, self()),
    {ok,Pid6c} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,via]),
    receive
	{'EXIT',Pid6c,{process_not_registered_via,dummy_via}} ->
	    ok
    after 1000 ->
	    ct:fail(
	      {gen_statem_started,
	       process_info(self(), messages)})
    end,
    dummy_via:unregister_name(armitage),

    process_flag(trap_exit, OldFlag),
    ok = verify_empty_msgq().

enter_loop(Reg1, Reg2) ->
    enter_loop(Reg1, Reg2, []).
%%
enter_loop(Reg1, Reg2, Opts) ->
    process_flag(trap_exit, true),
    case Reg1 of
	local -> register(armitage, self());
	global -> global:register_name(armitage, self());
	via -> dummy_via:register_name(armitage, self());
	anon -> ignore
    end,
    proc_lib:init_ack({ok, self()}),
    case Reg2 of
	local ->
	    gen_statem:enter_loop(
	      ?MODULE, Opts, state0, [], {local,armitage});
	global ->
	    gen_statem:enter_loop(
	      ?MODULE, Opts, state0, [], {global,armitage});
	via ->
	    gen_statem:enter_loop(
	      ?MODULE, Opts, state0, [], {via, dummy_via, armitage});
	anon ->
	    gen_statem:enter_loop(?MODULE, Opts, state0, [])
    end.

undef_code_change(_Config) ->
    {ok, Statem} = gen_statem:start(oc_statem, [], [{debug, [trace]}]),
    {error, {'EXIT',
             {undef, [{oc_statem, code_change, [_, _, _, _], _}|_]}}}
        = fake_upgrade(Statem, oc_statem).

fake_upgrade(Pid, Mod) ->
    sys:suspend(Pid),
    sys:replace_state(Pid, fun(State) -> {new, State} end),
    Ret = sys:change_code(Pid, Mod, old_vsn, []),
    ok = sys:resume(Pid),
    Ret.

undef_terminate1(_Config) ->
    {ok, Statem} = gen_statem:start(oc_statem, [], [{debug,[trace]}]),
    MRef = monitor(process, Statem),
    ok = gen_statem:stop(Statem),
    verify_down(Statem, MRef, normal),
    ok.

undef_terminate2(_Config) ->
    Reason = {error, test},
    {ok, Statem} = oc_statem:start([{debug,[trace]}]),
    MRef = monitor(process, Statem),
    ok = gen_statem:stop(Statem, Reason, infinity),
    verify_down(Statem, MRef, Reason).

undef_in_terminate(_Config) ->
    Data =  {undef_in_terminate, {?MODULE, terminate}},
    {ok, Statem} =
        gen_statem:start(
          ?MODULE, {data, Data}, [{debug,[log]}]),
    try
        gen_statem:stop(Statem),
        ct:fail(should_crash)
    catch
        exit:{undef, [{?MODULE, terminate, _, _}|_]} ->
            ok
    end.

verify_down(Statem, MRef, Reason) ->
    receive
        {'DOWN', MRef, process, Statem, Reason} ->
            ok
    after 5000 ->
        ct:fail(default_terminate_failed)
    end.


pop_too_many(_Config) ->
    _ = process_flag(trap_exit, true),

    Machine =
	#{init =>
	      fun () ->
		      {ok,state_1,undefined}
	      end,
	  state_1 =>
	      fun (enter, state_2, undefined) ->
                      {keep_state, enter}; % OTP-18239, should not be called
                  ({call, From}, {change_callback_module, _Module} = Action,
                   undefined = Data) ->
                      {next_state, state_2, Data,
                       [Action,
                        {reply,From,ok}]};
                  ({call, From}, {verify, ?MODULE},
                   undefined = _Data) ->
		      {keep_state_and_data,
                       [{reply,From,ok}]};
                  ({call, From}, pop_callback_module = Action,
                   undefined = Data) ->
                      {next_state, state_2, Data,
                       [Action,
                        {reply,From,ok}]}
	      end},
    {ok, STM} =
	gen_statem:start_link(
          ?MODULE,
          {map_statem, Machine, []},
          [{debug, [trace]}]),

    ok    = gen_statem:call(STM, {change_callback_module, oc_statem}),
    enter = gen_statem:call(STM, get_data), % OTP-18239
    ok    = gen_statem:call(STM, {push_callback_module, ?MODULE}),
    ok    = gen_statem:call(STM, {verify, ?MODULE}),
    ok    = gen_statem:call(STM, pop_callback_module),
    BadAction = {bad_action_from_state_function, pop_callback_module},
    {{BadAction, _},
     {gen_statem,call,[STM,pop_callback_module,infinity]}} =
        ?EXPECT_FAILURE(gen_statem:call(STM, pop_callback_module), Reason),

    receive
        {'EXIT', STM, {BadAction, _}} ->
            ok;
        Other ->
            ct:fail({surprise, Other})
    end.


%% Test the order for multiple {next_event,T,C}
next_events(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    ok = gen_statem:cast(Pid, next_event),
    {state,next_events,[]} = gen_statem:call(Pid, get),
    ok = gen_statem:stop(Pid),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(Pid), Reason).


%% Test report callback for Logger handler error_logger
format_log_1(_Config) ->
    FD = application:get_env(kernel, error_logger_format_depth),
    application:unset_env(kernel, error_logger_format_depth),
    Term = lists:seq(1,15),
    Name = self(),
    Reason = {bad_reply_action_from_state_function,[]},
    Report1 = simple_report(Name, Term, Reason),
    Report2 = elaborate_report(Name, Term, Reason),

    {F1,A1} = gen_statem:format_log(Report1),
    ct:log("F1: ~ts~nA1: ~tp",[F1,A1]),
    FExpected1 = "** State machine ~tp terminating~n"
        "** When server state  = ~tp~n"
        "** Reason for termination = ~tp:~tp~n"
        "** Callback modules = ~tp~n"
        "** Callback mode = ~tp~n",
    FExpected1 = F1,
    [Name,Term,error,Reason,[?MODULE],state_functions] = A1,

    {F3,A3} = gen_statem:format_log(Report2),
    ct:log("F3: ~ts~nA3: ~tp",[F3,A3]),
    FExpected3 = "** State machine ~tp terminating~n"
        "** Last event = ~tp~n"
        "** When server state  = ~tp~n"
        "** Reason for termination = ~tp:~tp~n"
        "** Callback modules = ~tp~n"
        "** Callback mode = ~tp~n"
        "** Queued = ~tp~n"
        "** Postponed = ~tp~n"
        "** Stacktrace =~n**  ~tp~n"
        "** Time-outs: ~tp~n"
        "** Log =~n**  ~tp~n"
        "** Client ~tp stacktrace~n"
        "** ~tp~n",
    FExpected3 = F3,
    Stacktrace = stacktrace(),
    [Name,Term,Term,error,Reason,[?MODULE],[state_functions,state_enter],[Term],
     [{internal,Term}],Stacktrace,{1,[{timeout,message}]},[Term],Name,[]] = A3,

    Depth = 10,
    ok = application:set_env(kernel, error_logger_format_depth, Depth),
    Limited = [1,2,3,4,5,6,7,8,9,'...'],
    {F2,A2} = gen_statem:format_log(Report1),
    ct:log("F2: ~ts~nA2: ~tp",[F2,A2]),
    FExpected2 = "** State machine ~tP terminating~n"
        "** When server state  = ~tP~n"
        "** Reason for termination = ~tP:~tP~n"
        "** Callback modules = ~tP~n"
        "** Callback mode = ~tP~n",
    FExpected2 = F2,
    [Name,Depth,Limited,Depth,error,Depth,Reason,Depth,
     [?MODULE],Depth,state_functions,Depth] = A2,

    {F4,A4} = gen_statem:format_log(Report2),
    ct:log("F4: ~ts~nA4: ~tp",[F4,A4]),
    FExpected4 = "** State machine ~tP terminating~n"
        "** Last event = ~tP~n"
        "** When server state  = ~tP~n"
        "** Reason for termination = ~tP:~tP~n"
        "** Callback modules = ~tP~n"
        "** Callback mode = ~tP~n"
        "** Queued = ~tP~n"
        "** Postponed = ~tP~n"
        "** Stacktrace =~n**  ~tP~n"
        "** Time-outs: ~tP~n"
        "** Log =~n**  ~tP~n"
        "** Client ~tP stacktrace~n"
        "** ~tP~n",
    FExpected4 = F4,
    LimitedPostponed = [{internal,[1,2,3,4,5,6,'...']}],
    LimitedStacktrace = io_lib:limit_term(Stacktrace, Depth),
    LimitedQueue = io_lib:limit_term([Term], Depth),
    [Name,Depth,Limited,Depth,Limited,Depth,error,Depth,Reason,Depth,
     [?MODULE],Depth,[state_functions,state_enter],Depth,LimitedQueue,Depth,
     LimitedPostponed,Depth,LimitedStacktrace,Depth,{1,[{timeout,message}]},
     Depth,[Limited],Depth,Name,Depth,[],Depth] = A4,

    case FD of
        undefined ->
            application:unset_env(kernel, error_logger_format_depth);
        _ ->
            application:set_env(kernel, error_logger_format_depth, FD)
    end,
    ok.

%% Test report callback for any Logger handler
format_log_2(_Config) ->
    format_log_2_simple(),
    format_log_2_elaborate(),
    ok.

format_log_2_simple() ->
    FD = application:get_env(kernel, error_logger_format_depth),
    application:unset_env(kernel, error_logger_format_depth),

    Term = lists:seq(1,15),
    Name = self(),
    NameStr = pid_to_list(Name),
    Reason = {bad_reply_action_from_state_function,[]},
    Report = simple_report(Name, Term, Reason),

    FormatOpts1 = #{},
    Str1 = flatten_format_log(Report, FormatOpts1),
    L1 = length(Str1),
    Expected1 = "** State machine " ++ NameStr ++ " terminating\n"
        "** When server state  = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]\n"
        "** Reason for termination = "
           "error:{bad_reply_action_from_state_function,[]}\n"
        "** Callback modules = ["?MODULE_STRING"]\n"
        "** Callback mode = state_functions\n",
    ct:log("Str1: ~ts", [Str1]),
    ct:log("length(Str1): ~p", [L1]),
    Expected1 = Str1,

    Depth = 10,
    FormatOpts2 = #{depth=>Depth},
    Str2 = flatten_format_log(Report, FormatOpts2),
    L2 = length(Str2),
    Expected2 = "** State machine " ++ NameStr ++ " terminating\n"
        "** When server state  = [1,2,3,4,5,6,7,8,9|...]\n"
        "** Reason for termination = "
           "error:{bad_reply_action_from_state_function,[]}\n"
        "** Callback modules = ["?MODULE_STRING"]\n"
        "** Callback mode = state_functions\n",
    ct:log("Str2: ~ts", [Str2]),
    ct:log("length(Str2): ~p", [L2]),
    true = Expected2 =:= Str2,

    FormatOpts3 = #{chars_limit=>200},
    Str3 = flatten_format_log(Report, FormatOpts3),
    L3 = length(Str3),
    Expected3 = "** State machine " ++ NameStr ++ " terminating\n"
        "** When server state  = [",
    ct:log("Str3: ~ts", [Str3]),
    ct:log("length(Str3): ~p", [L3]),
    true = lists:prefix(Expected3, Str3),
    true = L3 < L1,

    FormatOpts4 = #{single_line=>true},
    Str4 = flatten_format_log(Report, FormatOpts4),
    L4 = length(Str4),
    Expected4 = "State machine " ++ NameStr ++ " terminating. "
        "Reason: {bad_reply_action_from_state_function,[]}. "
        "State: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15].",
    ct:log("Str4: ~ts", [Str4]),
    ct:log("length(Str4): ~p", [L4]),
    Expected4 = Str4,

    FormatOpts5 = #{single_line=>true, depth=>Depth},
    Str5 = flatten_format_log(Report, FormatOpts5),
    L5 = length(Str5),
    Expected5 = "State machine " ++ NameStr ++ " terminating. "
        "Reason: {bad_reply_action_from_state_function,[]}. "
        "State: [1,2,3,4,5,6,7,8,9|...].",
    ct:log("Str5: ~ts", [Str5]),
    ct:log("length(Str5): ~p", [L5]),
    Expected5 = Str5,

    FormatOpts6 = #{single_line=>true, chars_limit=>100},
    Str6 = flatten_format_log(Report, FormatOpts6),
    L6 = length(Str6),
    Expected6 = "State machine " ++ NameStr ++ " terminating. "
        "Reason: ",
    ct:log("Str6: ~ts", [Str6]),
    ct:log("length(Str6): ~p", [L6]),
    true = lists:prefix(Expected6, Str6),
    true = L6 < L4,

    case FD of
        undefined ->
            application:unset_env(kernel, error_logger_format_depth);
        _ ->
            application:set_env(kernel, error_logger_format_depth, FD)
    end,
    ok.

format_log_2_elaborate() ->
    FD = application:get_env(kernel, error_logger_format_depth),
    application:unset_env(kernel, error_logger_format_depth),

    Term = lists:seq(1,15),
    Name = self(),
    NameStr = pid_to_list(Name),
    Reason = {bad_reply_action_from_state_function,[]},
    Report = elaborate_report(Name, Term, Reason),
    FormatOpts1 = #{},
    Str1 = flatten_format_log(Report, FormatOpts1),
    L1 = length(Str1),
    Expected1 = "** State machine " ++ NameStr ++ " terminating\n"
        "** Last event = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]\n",
    ct:log("Str1: ~ts", [Str1]),
    ct:log("length(Str1): ~p", [L1]),
    true = lists:prefix(Expected1, Str1),

    Depth = 10,
    FormatOpts2 = #{depth=>Depth},
    Str2 = flatten_format_log(Report, FormatOpts2),
    L2 = length(Str2),
    Expected2 = "** State machine " ++ NameStr ++ " terminating\n"
        "** Last event = [1,2,3,4,5,6,7,8,9|...]\n"
        "** When server state  = [1,2,3,4,5,6,7,8,9|...]\n"
        "** Reason for termination = "
           "error:{bad_reply_action_from_state_function,[]}\n"
        "** Callback modules = ["?MODULE_STRING"]\n"
        "** Callback mode = [state_functions,state_enter]\n"
        "** Queued = [[1,2,3,4,5,6,7,8|...]]\n"
        "** Postponed = [{internal,[1,2,3,4,5,6|...]}]\n"
        "** Stacktrace =\n"
        "**  [{m,f,1,[1,2,3,4|...]}]\n"
        "** Time-outs: {1,[{timeout,message}]}\n"
        "** Log =\n"
        "**  [[1,2,3,4,5,6,7,8|...]]\n"
        "** Client "++NameStr ++ " stacktrace\n"
        "** []\n",
    ct:log("Str2: ~ts", [Str2]),
    ct:log("length(Str2): ~p", [L2]),
    Expected2 = Str2,

    FormatOpts3 = #{chars_limit=>300},
    Str3 = flatten_format_log(Report, FormatOpts3),
    L3 = length(Str3),
    Expected3 = "** State machine " ++ NameStr ++ " terminating\n"
        "** Last event = ",
    ct:log("Str3: ~ts", [Str3]),
    ct:log("length(Str3): ~p", [L3]),
    true = lists:prefix(Expected3, Str3),
    true = L3 < L1,

    FormatOpts4 = #{single_line=>true},
    Str4 = flatten_format_log(Report, FormatOpts4),
    L4 = length(Str4),
    Expected4 = "State machine " ++ NameStr ++ " terminating. "
        "Reason: {bad_reply_action_from_state_function,[]}. "
        "Stack: [{m,f,1,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]}]. "
        "Last event: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]. "
        "State: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]. "
        "Log: [[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]]. "
        "Client " ++ NameStr ++ " stacktrace: [].",
    ct:log("Str4: ~ts", [Str4]),
    ct:log("length(Str4): ~p", [L4]),
    Expected4 = Str4,

    FormatOpts5 = #{single_line=>true, depth=>Depth},
    Str5 = flatten_format_log(Report, FormatOpts5),
    L5 = length(Str5),
    Expected5 = "State machine " ++ NameStr ++ " terminating. "
        "Reason: {bad_reply_action_from_state_function,[]}. "
        "Stack: [{m,f,1,[1,2,3,4|...]}]. "
        "Last event: [1,2,3,4,5,6,7,8,9|...]. "
        "State: [1,2,3,4,5,6,7,8,9|...]. "
        "Log: [[1,2,3,4,5,6,7,8|...]]. "
        "Client " ++ NameStr ++ " stacktrace: [].",
    ct:log("Str5: ~ts", [Str5]),
    ct:log("length(Str5): ~p", [L5]),
    Expected5 = Str5,

    FormatOpts6 = #{single_line=>true, chars_limit=>300},
    Str6 = flatten_format_log(Report, FormatOpts6),
    L6 = length(Str6),
    Expected6 = "State machine " ++ NameStr ++ " terminating. "
        "Reason:",
    ct:log("Str6: ~ts", [Str6]),
    ct:log("length(Str6): ~p", [L6]),
    true = lists:prefix(Expected6, Str6),
    true = L6 < L4,

    case FD of
        undefined ->
            application:unset_env(kernel, error_logger_format_depth);
        _ ->
            application:set_env(kernel, error_logger_format_depth, FD)
    end,
    ok.

format_log_with_process_label(_Config) ->
    %% Previous test cases test with process_label set to undefined,
    %% so in this test case, test setting it, and test:
    %% * multiple and single line line
    %% * depth-limited and unlimited

    FD = application:get_env(kernel, error_logger_format_depth),
    application:unset_env(kernel, error_logger_format_depth),
    Term = lists:seq(1,15),
    Name = self(),
    NameStr = pid_to_list(Name),
    ProcessLabel = {some_id, #{term => Term}},
    Reason = dummy_reason,
    State = dummy_state,
    Report0 = simple_report(Name, State, Reason),
    Report = Report0#{process_label=>ProcessLabel},

    %% multiple and single line (unlimited depth)

    {F1,A1} = gen_statem:format_log(Report),
    ct:log("F1: ~ts~nA1: ~tp",[F1,A1]),
    FExpected1 = "** State machine ~tp terminating~n"
        "** Process label = ~tp~n"
        "** When server state  = ~tp~n"
        "** Reason for termination = ~tp:~tp~n"
        "** Callback modules = ~tp~n"
        "** Callback mode = ~tp~n",
    FExpected1 = F1,
    [Name,ProcessLabel,State,error,Reason,[?MODULE],state_functions] = A1,

    FormatOpts2 = #{single_line=>true},
    Str2 = flatten_format_log(Report, FormatOpts2),
    Expected2 = "State machine " ++ NameStr ++ " terminating. "
        "Label: {some_id,#{term => [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]}}. "
        "Reason: dummy_reason. "
        "State: dummy_state.",
    ct:log("Str2: ~ts", [Str2]),
    true = Expected2 =:= Str2,

    %% multiple and single line (depth-limited)

    Depth = 10,
    FormatOpts3 = #{depth=>Depth},
    Str3 = flatten_format_log(Report, FormatOpts3),
    Expected3 = "** State machine " ++ NameStr ++ " terminating\n"
        "** Process label = {some_id,#{term => [1,2,3,4,5,6|...]}}\n"
        "** When server state  = dummy_state\n"
        "** Reason for termination = "
           "error:dummy_reason\n"
        "** Callback modules = ["?MODULE_STRING"]\n"
        "** Callback mode = state_functions\n",
    ct:log("Str3: ~ts", [Str3]),
    true = Expected3 =:= Str3,

    FormatOpts4 = #{single_line=>true, depth=>Depth},
    Str4 = flatten_format_log(Report, FormatOpts4),
    Expected4 = "State machine " ++ NameStr ++ " terminating. "
        "Label: {some_id,#{term => [1,2,3,4,5,6|...]}}. "
        "Reason: dummy_reason. "
        "State: dummy_state.",
    ct:log("Str4: ~ts", [Str4]),
    true = Expected4 =:= Str4,

    case FD of
        undefined ->
            application:unset_env(kernel, error_logger_format_depth);
        _ ->
            application:set_env(kernel, error_logger_format_depth, FD)
    end,
    ok.

simple_report(Name, Term, Reason) ->
    #{label=>{gen_statem,terminate},
      name=>Name,
      queue=>[],
      postponed=>[],
      modules=>[?MODULE],
      callback_mode=>state_functions,
      state_enter=>false,
      state=>Term,
      timeouts=>{0,[]},
      log=>[],
      reason=>{error,Reason,[]},
      client_info=>undefined,
      process_label=>undefined}.

elaborate_report(Name, Term, Reason) ->
    #{label=>{gen_statem,terminate},
      name=>Name,
      queue=>[Term,Term],
      postponed=>[{internal,Term}],
      modules=>[?MODULE],
      callback_mode=>state_functions,
      state_enter=>true,
      state=>Term,
      timeouts=>{1,[{timeout,message}]},
      log=>[Term],
      reason=>{error,Reason,stacktrace()},
      client_info=>{self(),{self(),[]}},
      process_label=>undefined}.

stacktrace() ->
    [{m,f,1,lists:seq(1, 15)}].

flatten_format_log(Report, Format) ->
    lists:flatten(gen_statem:format_log(Report, Format)).

reply_by_alias_with_payload(Config) when is_list(Config) ->
    %% "Payload" version of tag not used yet, but make sure
    %% gen_statem:reply/2 works with it...
    %%
    %% Whitebox...
    Reply = make_ref(),
    Alias = alias(),
    Tag = [[alias|Alias], "payload"],
    spawn_link(fun () ->
                       gen_statem:reply({undefined, Tag},
                                        Reply)
               end),
    receive
        {[[alias|Alias]|_] = Tag, Reply} ->
            ok
    end.

send_request_receive_reqid_collection(Config) when is_list(Config) ->
    {ok,Pid1} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    {ok,Pid2} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    {ok,Pid3} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    send_request_receive_reqid_collection(Pid1, Pid2, Pid3),
    send_request_receive_reqid_collection_timeout(Pid1, Pid2, Pid3),
    send_request_receive_reqid_collection_error(Pid1, Pid2, Pid3),
    stopped = gen_statem:call(Pid1, {stop,shutdown}),
    stopped = gen_statem:call(Pid3, {stop,shutdown}),
    check_stopped(Pid1),
    check_stopped(Pid2),
    check_stopped(Pid3),
    ok.

send_request_receive_reqid_collection(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_statem:send_request(Pid1, 'alive?'),

    ReqIdC0 = gen_statem:reqids_new(),

    ReqId1 = gen_statem:send_request(Pid1, {delayed_answer,400}),
    ReqIdC1 = gen_statem:reqids_add(ReqId1, req1, ReqIdC0),
    1 = gen_statem:reqids_size(ReqIdC1),

    ReqIdC2 = gen_statem:send_request(Pid2, {delayed_answer,1}, req2, ReqIdC1),
    2 = gen_statem:reqids_size(ReqIdC2),

    ReqIdC3 = gen_statem:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_statem:reqids_size(ReqIdC3),
    
    {{reply, delayed}, req2, ReqIdC4} = gen_statem:receive_response(ReqIdC3, infinity, true),
    2 = gen_statem:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC5} = gen_statem:receive_response(ReqIdC4, 5678, true),
    1 = gen_statem:reqids_size(ReqIdC5),

    {{reply, delayed}, req1, ReqIdC6} = gen_statem:receive_response(ReqIdC5, 5000, true),
    0 = gen_statem:reqids_size(ReqIdC6),

    no_request = gen_statem:receive_response(ReqIdC6, 5000, true),

    {reply, yes} = gen_statem:receive_response(ReqId0, infinity),

    ok.

send_request_receive_reqid_collection_timeout(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_statem:send_request(Pid1, 'alive?'),

    ReqIdC0 = gen_statem:reqids_new(),

    ReqId1 = gen_statem:send_request(Pid1, {delayed_answer,1000}),
    ReqIdC1 = gen_statem:reqids_add(ReqId1, req1, ReqIdC0),

    ReqIdC2 = gen_statem:send_request(Pid2, {delayed_answer,1}, req2, ReqIdC1),

    ReqId3 = gen_statem:send_request(Pid3, {delayed_answer,500}),
    ReqIdC3 = gen_statem:reqids_add(ReqId3, req3, ReqIdC2),

    Deadline = erlang:monotonic_time(millisecond) + 100,
    
    {{reply, delayed}, req2, ReqIdC4} = gen_statem:receive_response(ReqIdC3, {abs, Deadline}, true),
    2 = gen_statem:reqids_size(ReqIdC4),

    timeout = gen_statem:receive_response(ReqIdC4, {abs, Deadline}, true),

    Abandoned = lists:sort([{ReqId1, req1}, {ReqId3, req3}]),
    Abandoned = lists:sort(gen_statem:reqids_to_list(ReqIdC4)),

    %% Make sure requests were abandoned...
    timeout = gen_statem:receive_response(ReqIdC4, {abs, Deadline+1000}, true),

    {reply, yes} = gen_statem:receive_response(ReqId0, infinity),

    ok.
    
send_request_receive_reqid_collection_error(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_statem:send_request(Pid1, 'alive?'),

    ReqIdC0 = gen_statem:reqids_new(),

    ReqId1 = gen_statem:send_request(Pid1, {delayed_answer,400}),
    ReqIdC1 = gen_statem:reqids_add(ReqId1, req1, ReqIdC0),
    try
        nope = gen_statem:reqids_add(ReqId1, req2, ReqIdC1)
    catch
        error:badarg -> ok
    end,
 
    unlink(Pid2),
    exit(Pid2, kill),
    ReqIdC2 = gen_statem:send_request(Pid2, {delayed_answer,1}, req2, ReqIdC1),
    ReqIdC3 = gen_statem:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_statem:reqids_size(ReqIdC3),
    
    {{error, {noproc, _}}, req2, ReqIdC4} = gen_statem:receive_response(ReqIdC3, 2000, true),
    2 = gen_statem:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC4} = gen_statem:receive_response(ReqIdC4, infinity, false),

    {{reply, delayed}, req1, ReqIdC4} = gen_statem:receive_response(ReqIdC4, infinity, false),

    {reply, yes} = gen_statem:receive_response(ReqId0, infinity),

    ok.

send_request_wait_reqid_collection(Config) when is_list(Config) ->
    {ok,Pid1} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    {ok,Pid2} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    {ok,Pid3} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    send_request_wait_reqid_collection(Pid1, Pid2, Pid3),
    send_request_wait_reqid_collection_timeout(Pid1, Pid2, Pid3),
    send_request_wait_reqid_collection_error(Pid1, Pid2, Pid3),
    stopped = gen_statem:call(Pid1, {stop,shutdown}),
    stopped = gen_statem:call(Pid3, {stop,shutdown}),
    check_stopped(Pid1),
    check_stopped(Pid2),
    check_stopped(Pid3),
    ok.

send_request_wait_reqid_collection(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_statem:send_request(Pid1, 'alive?'),

    ReqIdC0 = gen_statem:reqids_new(),

    ReqId1 = gen_statem:send_request(Pid1, {delayed_answer,400}),
    ReqIdC1 = gen_statem:reqids_add(ReqId1, req1, ReqIdC0),
    1 = gen_statem:reqids_size(ReqIdC1),

    ReqIdC2 = gen_statem:send_request(Pid2, {delayed_answer,1}, req2, ReqIdC1),
    2 = gen_statem:reqids_size(ReqIdC2),

    ReqIdC3 = gen_statem:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_statem:reqids_size(ReqIdC3),
    
    {{reply, delayed}, req2, ReqIdC4} = gen_statem:wait_response(ReqIdC3, infinity, true),
    2 = gen_statem:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC5} = gen_statem:wait_response(ReqIdC4, 5678, true),
    1 = gen_statem:reqids_size(ReqIdC5),

    {{reply, delayed}, req1, ReqIdC6} = gen_statem:wait_response(ReqIdC5, 5000, true),
    0 = gen_statem:reqids_size(ReqIdC6),

    no_request = gen_statem:wait_response(ReqIdC6, 5000, true),

    {reply, yes} = gen_statem:receive_response(ReqId0, infinity),

    ok.

send_request_wait_reqid_collection_timeout(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_statem:send_request(Pid1, 'alive?'),

    ReqIdC0 = gen_statem:reqids_new(),

    ReqId1 = gen_statem:send_request(Pid1, {delayed_answer,1000}),
    ReqIdC1 = gen_statem:reqids_add(ReqId1, req1, ReqIdC0),

    ReqIdC2 = gen_statem:send_request(Pid2, {delayed_answer,1}, req2, ReqIdC1),

    ReqId3 = gen_statem:send_request(Pid3, {delayed_answer,500}),
    ReqIdC3 = gen_statem:reqids_add(ReqId3, req3, ReqIdC2),

    Deadline = erlang:monotonic_time(millisecond) + 100,
    
    {{reply, delayed}, req2, ReqIdC4} = gen_statem:wait_response(ReqIdC3, {abs, Deadline}, true),
    2 = gen_statem:reqids_size(ReqIdC4),

    timeout = gen_statem:wait_response(ReqIdC4, {abs, Deadline}, true),

    Unhandled = lists:sort([{ReqId1, req1}, {ReqId3, req3}]),
    Unhandled = lists:sort(gen_statem:reqids_to_list(ReqIdC4)),

    %% Make sure requests were not abandoned...
    {{reply, delayed}, req3, ReqIdC4} = gen_statem:wait_response(ReqIdC4, {abs, Deadline+1500}, false),
    {{reply, delayed}, req1, ReqIdC4} = gen_statem:wait_response(ReqIdC4, {abs, Deadline+1500}, false),

    {reply, yes} = gen_statem:receive_response(ReqId0),

    ok.
    
send_request_wait_reqid_collection_error(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_statem:send_request(Pid1, 'alive?'),

    ReqIdC0 = gen_statem:reqids_new(),

    ReqId1 = gen_statem:send_request(Pid1, {delayed_answer,400}),
    ReqIdC1 = gen_statem:reqids_add(ReqId1, req1, ReqIdC0),
    try
        nope = gen_statem:reqids_add(ReqId1, req2, ReqIdC1)
    catch
        error:badarg -> ok
    end,
 
    unlink(Pid2),
    exit(Pid2, kill),
    ReqIdC2 = gen_statem:send_request(Pid2, {delayed_answer,1}, req2, ReqIdC1),
    ReqIdC3 = gen_statem:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_statem:reqids_size(ReqIdC3),
    
    {{error, {noproc, _}}, req2, ReqIdC4} = gen_statem:wait_response(ReqIdC3, 2000, true),
    2 = gen_statem:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC4} = gen_statem:wait_response(ReqIdC4, infinity, false),

    {{reply, delayed}, req1, ReqIdC4} = gen_statem:wait_response(ReqIdC4, infinity, false),

    {reply, yes} = gen_statem:receive_response(ReqId0, infinity),

    ok.

send_request_check_reqid_collection(Config) when is_list(Config) ->
    {ok,Pid1} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    {ok,Pid2} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    {ok,Pid3} = gen_statem:start(?MODULE, start_arg(Config, []), []),
    send_request_check_reqid_collection(Pid1, Pid2, Pid3),
    send_request_check_reqid_collection_error(Pid1, Pid2, Pid3),
    stopped = gen_statem:call(Pid1, {stop,shutdown}),
    stopped = gen_statem:call(Pid3, {stop,shutdown}),
    check_stopped(Pid1),
    check_stopped(Pid2),
    check_stopped(Pid3),
    ok.

send_request_check_reqid_collection(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_statem:send_request(Pid1, 'alive?'),

    receive after 100 -> ok end,

    ReqIdC0 = gen_statem:reqids_new(),

    ReqIdC1 = gen_statem:send_request(Pid1, {delayed_answer,400}, req1, ReqIdC0),
    1 = gen_statem:reqids_size(ReqIdC1),

    ReqId2 = gen_statem:send_request(Pid2, {delayed_answer,1}),
    ReqIdC2 = gen_statem:reqids_add(ReqId2, req2, ReqIdC1),
    2 = gen_statem:reqids_size(ReqIdC2),

    ReqIdC3 = gen_statem:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_statem:reqids_size(ReqIdC3),

    Msg0 = next_msg(),
    no_reply = gen_statem:check_response(Msg0, ReqIdC3, true),
    
    {{reply, delayed}, req2, ReqIdC4} = gen_statem:check_response(next_msg(), ReqIdC3, true),
    2 = gen_statem:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC5} = gen_statem:check_response(next_msg(), ReqIdC4, true),
    1 = gen_statem:reqids_size(ReqIdC5),

    {{reply, delayed}, req1, ReqIdC6} = gen_statem:check_response(next_msg(), ReqIdC5, true),
    0 = gen_statem:reqids_size(ReqIdC6),

    no_request = gen_statem:check_response(Msg0, ReqIdC6, true),

    {reply, yes} = gen_statem:check_response(Msg0, ReqId0),

    ok.
    
send_request_check_reqid_collection_error(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_statem:send_request(Pid1, 'alive?'),

    receive after 100 -> ok end,

    ReqIdC0 = gen_statem:reqids_new(),

    ReqId1 = gen_statem:send_request(Pid1, {delayed_answer,400}),
    ReqIdC1 = gen_statem:reqids_add(ReqId1, req1, ReqIdC0),
    try
        nope = gen_statem:reqids_add(ReqId1, req2, ReqIdC1)
    catch
        error:badarg -> ok
    end,

    unlink(Pid2),
    exit(Pid2, kill),
    ReqIdC2 = gen_statem:send_request(Pid2, {delayed_answer,1}, req2, ReqIdC1),

    ReqIdC3 = gen_statem:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_statem:reqids_size(ReqIdC3),

    Msg0 = next_msg(),

    no_reply = gen_statem:check_response(Msg0, ReqIdC3, true),
    
    {{error, {noproc, _}}, req2, ReqIdC4} = gen_statem:check_response(next_msg(), ReqIdC3, true),
    2 = gen_statem:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC4} = gen_statem:check_response(next_msg(), ReqIdC4, false),

    {{reply, delayed}, req1, ReqIdC4} = gen_statem:check_response(next_msg(), ReqIdC4, false),

    {reply, yes} = gen_statem:check_response(Msg0, ReqId0),

    ok.

next_msg() ->
    receive M -> M end.

%%
%% Functionality check
%%

wfor(Msg) ->
    receive
	Msg -> ok
    after 5000 ->
	    error(timeout)
    end.


stop_it(STM) ->
    stopped = gen_statem:call(STM, stop),
    check_stopped(STM).


check_stopped(STM) ->
    Call = there_you_are,
    {_,{gen_statem,call,[_,Call,infinity]}} =
	?EXPECT_FAILURE(gen_statem:call(STM, Call), Reason),
    ok.


do_func_test(STM) ->
    ok = gen_statem:cast(STM, {'alive?',self()}),
    wfor(yes),
    ok = do_connect(STM),
    ok = gen_statem:cast(STM, {'alive?',self()}),
    wfor(yes),
    test_server:do_times(3, ?MODULE, do_msg, [STM]),
    ok = gen_statem:cast(STM, {'alive?',self()}),
    wfor(yes),
    ok = do_disconnect(STM),
    ok = gen_statem:cast(STM, {'alive?',self()}),
    P0 = gen_statem:send_request(STM, 'alive?'),
    timeout = gen_statem:wait_response(P0, 0),
    wfor(yes),
    {reply, yes} = gen_statem:wait_response(P0, infinity),
    _ = flush(),
    P1 = gen_statem:send_request(STM, 'alive?'),
    receive Msg ->
            no_reply = gen_statem:check_response(Msg, P0),
            {reply, yes} = gen_statem:check_response(Msg, P1)
    after 1000 -> exit(timeout)
    end,
    ok.


do_connect(STM) ->
    check_state(STM, idle),
    gen_statem:cast(STM, {connect,self()}),
    wfor(accept),
    check_state(STM, wfor_conf),
    Tag = make_ref(),
    gen_statem:cast(STM, {ping,self(),Tag}),
    gen_statem:cast(STM, confirm),
    wfor({pong,Tag}),
    check_state(STM, connected),
    ok.

do_msg(STM) ->
    check_state(STM, connected),
    R = make_ref(),
    ok = gen_statem:cast(STM, {msg,self(),R}),
    wfor({ack,R}).


do_disconnect(STM) ->
    ok = gen_statem:cast(STM, disconnect),
    check_state(STM, idle).

check_state(STM, State) ->
    case gen_statem:call(STM, get) of
	{state, State, _} -> ok
    end.

do_sync_func_test(STM) ->
    yes = gen_statem:call(STM, 'alive?'),
    ok = do_sync_connect(STM),
    yes = gen_statem:call(STM, 'alive?'),
    test_server:do_times(3, ?MODULE, do_sync_msg, [STM]),
    yes = gen_statem:call(STM, 'alive?'),
    ok = do_sync_disconnect(STM),
    yes = gen_statem:call(STM, 'alive?'),
    check_state(STM, idle),
    ok = gen_statem:call(STM, {timeout,200}),
    yes = gen_statem:call(STM, 'alive?'),
    check_state(STM, idle),
    ok.


do_sync_connect(STM) ->
    check_state(STM, idle),
    accept = gen_statem:call(STM, connect),
    check_state(STM, wfor_conf),
    Tag = make_ref(),
    gen_statem:cast(STM, {ping,self(),Tag}),
    yes = gen_statem:call(STM, confirm),
    wfor({pong,Tag}),
    check_state(STM, connected),
    ok.

do_sync_msg(STM) ->
    check_state(STM, connected),
    R = make_ref(),
    {ack,R} = gen_statem:call(STM, {msg,R}),
    ok.

do_sync_disconnect(STM) ->
    yes = gen_statem:call(STM, disconnect),
    check_state(STM, idle).


verify_empty_msgq() ->
    [] = flush(),
    ok.

start_arg(Config, Arg) ->
    case lists:keyfind(callback_mode, 1, Config) of
	{_,CallbackMode} ->
	    {callback_mode,CallbackMode,Arg};
	false ->
	    Arg
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The State Machine
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(ignore) ->
    io:format("init(ignore)~n", []),
    ignore;
init(stop) ->
    io:format("init(stop)~n", []),
    {stop,stopped};
init({error, Reason}) ->
    io:format("init(error) -> Reason: ~p~n", [Reason]),
    {error, Reason};
init(stop_shutdown) ->
    io:format("init(stop_shutdown)~n", []),
    {stop,shutdown};
init(sleep) ->
    io:format("init(sleep)~n", []),
    ct:sleep(1000),
    init_sup({ok,idle,data});
init(hiber) ->
    io:format("init(hiber)~n", []),
    init_sup({ok,hiber_idle,[]});
init(hiber_now) ->
    io:format("init(hiber_now)~n", []),
    init_sup({ok,hiber_idle,[],[hibernate]});
init({data, Data}) ->
    io:format("init(data)~n", []),
    init_sup({ok,idle,Data});
init({callback_mode,CallbackMode,Arg}) ->
    io:format("init(callback_mode)~n", []),
    ets:new(?MODULE, [named_table,private]),
    ets:insert(?MODULE, {callback_mode,CallbackMode}),
    init(Arg);
init({map_statem,#{init := Init}=Machine,Modes}) ->
    io:format("init(map_statem)~n", []),
    ets:new(?MODULE, [named_table,private]),
    ets:insert(?MODULE, {callback_mode,[handle_event_function|Modes]}),
    case Init() of
	{ok,State,Data,Ops} ->
	    init_sup({ok,State,[Data|Machine],Ops});
	{ok,State,Data} ->
	    init_sup({ok,State,[Data|Machine]});
	Other ->
	    init_sup(Other)
    end;
init({ets, InitResult}) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    init_sup(
      case InitResult of
          {return, Value} ->
              Value;
          {crash, Class, Reason} ->
              erlang:Class(Reason);
          {wait, Time, Value} ->
              receive after Time -> Value end
      end);
init([]) ->
    io:format("init~n", []),
    init_sup({ok,idle,data}).

%% Supervise state machine parent i.e the test case, and if it dies
%% (fails due to some reason), kill the state machine,
%% just to not leak resources (process, name, ETS table, etc...)
%%
init_sup(Result) ->
    Parent = gen:get_parent(),
    Statem = self(),
    _Supervisor =
        spawn(
          fun () ->
                  StatemRef = monitor(process, Statem),
                  ParentRef = monitor(process, Parent),
                  receive
                      {'DOWN', StatemRef, _, _, Reason} ->
                          exit(Reason);
                      {'DOWN', ParentRef, _, _, _} ->
                          exit(Statem, kill)
                  end
          end),
    Result.

callback_mode() ->
    try ets:lookup(?MODULE, callback_mode) of
	[{callback_mode,CallbackMode}] ->
	    CallbackMode
    catch
	error:badarg ->
	    state_functions
    end.

terminate(_, _State, crash_terminate) ->
    exit({crash,terminate});
terminate(_, _State, {undef_in_terminate, {Mod, Fun}}) ->
    Mod:Fun(),
    ok;
terminate({From,stopped}, State, _Data) ->
    From ! {self(),{stopped,State}},
    ok;
terminate(_Reason, _State, _Data) ->
    ok.


%% State functions

idle(info, {hping,Pid}, _Data) ->
    Pid ! {self(), hpong},
    keep_state_and_data;
idle(cast, {hping,Pid}, Data) ->
    Pid ! {self(), hpong},
    {keep_state, Data};
idle({call, From}, hping, _Data) ->
    {keep_state_and_data, [{reply, From, hpong}]};
idle({call, From}, {start_htimer, Pid, Timeout}, _Data) ->
    {keep_state_and_data, [{reply, From, ok}, {timeout, Timeout, {htimer, Pid}}]};
idle(timeout, {htimer, Pid}, _Data) ->
    Pid ! {self(), htimer_timeout},
    keep_state_and_data;
idle(cast, {connect,Pid}, Data) ->
    Pid ! accept,
    {next_state,wfor_conf,Data,infinity}; % NoOp timeout just to test API
idle({call,From}, connect, Data) ->
    gen_statem:reply(From, accept),
    {next_state,wfor_conf,Data,infinity}; % NoOp timeout just to test API
idle({call,_From}, badreturn, _Data) ->
    badreturn;
idle({call,_From}, badaction, Data) ->
    {keep_state, Data, [badaction]};
idle({call,_From}, {badtimeout,BadTimeout}, Data) ->
    {keep_state, Data, BadTimeout};
idle({call,From}, {delayed_answer,T}, Data) ->
    receive
    after T ->
	    gen_statem:reply({reply,From,delayed}),
	    throw({keep_state,Data})
    end;
idle({call,From}, {timeout,Time}, _Data) ->
    AbsTime = erlang:monotonic_time(millisecond) + Time,
    {next_state,timeout,{From,Time},
     {timeout,AbsTime,idle,[{abs,true}]}};
idle(cast, {timeout,Time}, _Data) ->
    AbsTime = erlang:monotonic_time(millisecond) + Time,
    {keep_state_and_data,{timeout,AbsTime,idle,[{abs,true}]}};
idle(cast, postpone_event, _Data) ->
    {keep_state_and_data,postpone};
idle(cast, next_event, _Data) ->
    {next_state,next_events,[a,b,c],
     [{next_event,internal,a},
      {next_event,internal,b},
      {next_event,internal,c}]};
idle(Type, Content, Data) ->
    case handle_common_events(Type, Content, idle, Data) of
	undefined ->
	    case Type of
		{call,From} ->
		    throw({keep_state,Data,[{reply,From,'eh?'}]});
		_ ->
		    throw(
		      {stop,{unexpected,idle,Type,Content}})
	    end;
	Result ->
	    Result
    end.

timeout(timeout, idle, {From,Time}) ->
    TRef = erlang:start_timer(Time, self(), ok),
    {keep_state,{From,TRef},0}; % Immediate timeout 0
timeout(timeout, 0, {From,TRef}) ->
    {next_state,timeout2,{From,TRef},
     [{timeout,1,should_be_cancelled},
      postpone]}; % Should cancel state timeout
timeout(_, _, _) ->
    keep_state_and_data.

timeout2(timeout, 0, _) ->
    keep_state_and_data;
timeout2(timeout, Reason, _) ->
    {stop,Reason};
timeout2(info, {timeout,TRef,Result}, {From,TRef}) ->
    gen_statem:reply([{reply,From,Result}]),
    {next_state,idle,state};
timeout2(_, _, _) ->
    {keep_state_and_data,[]}.

wfor_conf({call,From}, confirm, Data) ->
    {next_state,connected,Data,
     {reply,From,yes}};
wfor_conf(cast, {ping,_,_}, _) ->
    {keep_state_and_data,[postpone]};
wfor_conf(cast, confirm, Data) ->
    {next_state,connected,Data};
wfor_conf(Type, Content, Data) ->
    case handle_common_events(Type, Content, wfor_conf, Data) of
	undefined ->
	    case Type of
		{call,From} ->
		    {next_state,idle,Data,
		     [{reply,From,'eh?'}]};
		_ ->
		    throw(keep_state_and_data)
	    end;
	Result ->
	    Result
    end.

connected({call,From}, {msg,Ref}, Data) ->
    {keep_state,Data,
     {reply,From,{ack,Ref}}};
connected(cast, {msg,From,Ref}, Data) ->
    From ! {ack,Ref},
    {keep_state,Data};
connected({call,From}, disconnect, Data) ->
    {next_state,idle,Data,
     [{reply,From,yes}]};
connected(cast, disconnect, Data) ->
    {next_state,idle,Data};
connected(cast, {ping,Pid,Tag}, Data) ->
    Pid ! {pong,Tag},
    {keep_state,Data};
connected(Type, Content, Data) ->
    case handle_common_events(Type, Content, connected, Data) of
	undefined ->
	    case Type of
		{call,From} ->
		    {keep_state,Data,
		     [{reply,From,'eh?'}]};
		_ ->
		    {keep_state,Data}
	    end;
	Result ->
	    Result
    end.

state0({call,From}, stop, Data) ->
    {stop_and_reply,normal,[{reply,From,stopped}],Data};
state0(Type, Content, Data) ->
    case handle_common_events(Type, Content, state0, Data) of
	undefined ->
	    {keep_state,Data};
	Result ->
	    Result
    end.

hiber_idle({call,From}, 'alive?', Data) ->
    {keep_state,Data,
     [{reply,From,'alive!'}]};
hiber_idle({call,From}, hibernate_sync, Data) ->
    {next_state,hiber_wakeup,Data,
     [{reply,From,hibernating},
      hibernate]};
hiber_idle(info, {hibernate_later, Time}, _) ->
    Tref = erlang:start_timer(Time, self(), hibernate),
    {keep_state,Tref};
hiber_idle(info, hibernate_now, Data) ->
    {keep_state,Data,
     [hibernate]};
hiber_idle(info, {timeout,Tref,hibernate}, Tref) ->
    {keep_state,[],
     [hibernate]};
hiber_idle(cast, hibernate_async, Data) ->
    {next_state,hiber_wakeup,Data,
     [hibernate]};
hiber_idle(Type, Content, Data) ->
    case handle_common_events(Type, Content, hiber_idle, Data) of
	undefined ->
	    {keep_state,Data};
	Result ->
	    Result
    end.

hiber_wakeup({call,From}, wakeup_sync, Data) ->
    {next_state,hiber_idle,Data,
     [{reply,From,good_morning}]};
hiber_wakeup({call,From}, snooze_sync, Data) ->
    {keep_state,Data,
     [{reply,From,please_just_five_more},
      hibernate]};
hiber_wakeup(cast, wakeup_async, Data) ->
    {next_state,hiber_idle,Data};
hiber_wakeup(cast, snooze_async, Data) ->
    {keep_state,Data,
     [hibernate]};
hiber_wakeup(Type, Content, Data) ->
    case handle_common_events(Type, Content, hiber_wakeup, Data) of
	undefined ->
	    {keep_state,Data};
	Result ->
	    Result
    end.

next_events(internal, Msg, [Msg|Msgs]) ->
    {keep_state,Msgs};
next_events(Type, Content, Data) ->
    case handle_common_events(Type, Content, next_events, Data) of
	undefined ->
	    {keep_state,Data};
	Result ->
	    Result
    end.


handle_common_events({call,From}, get_callback_mode, _, _) ->
    {keep_state_and_data,{reply,From,state_functions}};
handle_common_events({call,From}, get, State, Data) ->
    {keep_state,Data,
     [{reply,From,{state,State,Data}}]};
handle_common_events(cast, {get,Pid}, State, Data) ->
    Pid ! {state,State,Data},
    {keep_state,Data};
handle_common_events({call,From}, stop, _, Data) ->
    {stop_and_reply,normal,[{reply,From,stopped}],Data};
handle_common_events(cast, stop, _, _) ->
    stop;
handle_common_events({call,From}, {stop,Reason}, _, Data) ->
    {stop_and_reply,Reason,{reply,From,stopped},Data};
handle_common_events(cast, {stop,Reason}, _, _) ->
    {stop,Reason};
handle_common_events({call,From}, 'alive?', _, Data) ->
    {keep_state,Data,
     [{reply,From,yes}]};
handle_common_events(cast, {'alive?',Pid}, _, Data) ->
    Pid ! yes,
    {keep_state,Data};
handle_common_events(_, _, _, _) ->
    undefined.

handle_event({call,From}, get_callback_mode, _, _) ->
    {keep_state_and_data,{reply,From,handle_event_function}};
%% Wrapper state machine that uses a map state machine spec
handle_event(
  Type, Event, State, [Data|Machine])
  when is_map(Machine) ->
    #{State := HandleEvent} = Machine,
    case
	try HandleEvent(Type, Event, Data) of
	    Result ->
		Result
	catch
	    Result ->
		Result
	end of
	{stop,Reason,NewData} ->
	    {stop,Reason,[NewData|Machine]};
	{next_state,NewState,NewData} ->
	    {next_state,NewState,[NewData|Machine]};
	{next_state,NewState,NewData,Ops} ->
	    {next_state,NewState,[NewData|Machine],Ops};
	{keep_state,NewData} ->
	    {keep_state,[NewData|Machine]};
	{keep_state,NewData,Ops} ->
	    {keep_state,[NewData|Machine],Ops};
	{repeat_state,NewData} ->
	    {repeat_state,[NewData|Machine]};
	{repeat_state,NewData,Ops} ->
	    {repeat_state,[NewData|Machine],Ops};
	Other ->
	    Other
    end;
%%
%% Dispatcher to test callback_mode handle_event_function
%%
%% Wrap the state in a 1 element list just to test non-atom
%% states.  Note that the state from init/1 is not wrapped
%% so both atom and non-atom states are tested.
handle_event(Type, Event, State, Data) ->
    StateName = unwrap_state(State),
    try ?MODULE:StateName(Type, Event, Data) of
	Result ->
	    wrap_result(Result)
    catch
	throw:Result:Stacktrace ->
	    erlang:raise(
	      throw, wrap_result(Result), Stacktrace)
    end.

unwrap_state([State]) ->
    State;
unwrap_state(State) ->
    State.

wrap_result(Result) ->
    case Result of
	{next_state,NewState,NewData} ->
	    {next_state,[NewState],NewData};
	{next_state,NewState,NewData,StateOps} ->
	    {next_state,[NewState],NewData,StateOps};
	Other ->
	    Other
    end.



code_change(OldVsn, State, Data, CallbackMode) ->
    io:format(
      "code_change(~p, ~p, ~p, ~p)~n", [OldVsn,State,Data,CallbackMode]),
    ets:insert(?MODULE, {callback_mode,CallbackMode}),
    io:format(
      "code_change(~p, ~p, ~p, ~p)~n", [OldVsn,State,Data,CallbackMode]),
    {ok,State,{OldVsn,Data,CallbackMode}}.

format_status(terminate, [_Pdict,State,Data]) ->
    {formatted,State,Data};
format_status(normal, [_Pdict,_State,_Data]) ->
    [format_status_called].

flush() ->
    receive
	Msg ->
	    [Msg|flush()]
    after 500 ->
	    []
    end.
