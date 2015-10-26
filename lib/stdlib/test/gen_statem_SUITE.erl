%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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

-include_lib("test_server/include/test_server.hrl").

-compile(export_all).
-behaviour(gen_statem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group, start},
     {group, abnormal},
     shutdown,
     {group, sys}, hibernate, enter_loop].

groups() ->
    [{start, [],
      [start1, start2, start3, start4, start5, start6, start7,
       start8, start9, start10, start11, start12]},
     {stop, [],
      [stop1, stop2, stop3, stop4, stop5, stop6, stop7, stop8, stop9, stop10]},
     {abnormal, [], [abnormal1, abnormal2]},
     {sys, [],
      [sys1,
       call_format_status,
       error_format_status, terminate_crash_format,
       get_state, replace_state]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_CaseName, Config) ->
    ?t:messages_get(),
    Dog = ?t:timetrap(?t:minutes(1)),
%%%    dbg:tracer(),
%%%    dbg:p(all, c),
%%%    dbg:tpl(gen_statem, cx),
    [{watchdog, Dog} | Config].

end_per_testcase(_CaseName, Config) ->
%%%    dbg:stop(),
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(EXPECT_FAILURE(Code, Reason),
	try begin Code end of
	    _ ->
		?t:fail()
	catch
	    error:Reason -> Reason;
	    exit:Reason -> Reason
	end).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% anonymous
start1(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    {ok,Pid0} = gen_statem:start_link(?MODULE, [], []),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stop_it(Pid0),
%%    stopped = gen_statem:call(Pid0, stop),
%%    timeout =
%%	?EXPECT_FAILURE(gen_statem:call(Pid0, hej), Reason),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous w. shutdown
start2(Config) when is_list(Config) ->
    %% Dont link when shutdown
    {ok,Pid0} = gen_statem:start(?MODULE, [], []),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stopped = gen_statem:call(Pid0, {stop,shutdown}),
    check_stopped(Pid0),
    ok = verify_empty_msgq().

%% anonymous with timeout
start3(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    {ok,Pid0} = gen_statem:start(?MODULE, [], [{timeout,5}]),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stop_it(Pid0),

    {error,timeout} = gen_statem:start(?MODULE, sleep,
					   [{timeout,5}]),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous with ignore
start4(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    ignore = gen_statem:start(?MODULE, ignore, []),

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous with stop
start5(suite) -> [];
start5(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    {error,stopped} = gen_statem:start(?MODULE, stop, []),

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% anonymous linked
start6(Config) when is_list(Config) ->
    {ok,Pid} = gen_statem:start_link(?MODULE, [], []),
    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    stop_it(Pid),

    ok = verify_empty_msgq().

%% global register linked
start7(Config) when is_list(Config) ->
    STM = {global,my_stm},

    {ok,Pid} =
	gen_statem:start_link(STM, ?MODULE, [], []),
    {error,{already_started,Pid}} =
	gen_statem:start_link(STM, ?MODULE, [], []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, [], []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(STM),
    ok = do_sync_func_test(STM),
    stop_it(STM),

    ok = verify_empty_msgq().


%% local register
start8(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),
    Name = my_stm,
    STM = {local,Name},

    {ok,Pid} =
	gen_statem:start(STM, ?MODULE, [], []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, [], []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(Name),
    ok = do_sync_func_test(Name),
    stop_it(Pid),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% local register linked
start9(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),
    Name = my_stm,
    STM = {local,Name},

    {ok,Pid} =
	gen_statem:start_link(STM, ?MODULE, [], []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, [], []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(Name),
    ok = do_sync_func_test(Name),
    stop_it(Pid),

    %%process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

%% global register
start10(Config) when is_list(Config) ->
    STM = {global,my_stm},

    {ok,Pid} =
	gen_statem:start(STM, ?MODULE, [], []),
    {error,{already_started,Pid}} =
	gen_statem:start(STM, ?MODULE, [], []),
    {error,{already_started,Pid}} =
	gen_statem:start_link(STM, ?MODULE, [], []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(STM),
    ok = do_sync_func_test(STM),
    stop_it(STM),

    ok = verify_empty_msgq().

%% Stop registered processes
start11(Config) when is_list(Config) ->
    Name = my_stm,
    LocalSTM = {local,Name},
    GlobalSTM = {global,Name},

    {ok,Pid} =
	gen_statem:start_link(LocalSTM, ?MODULE, [], []),
    stop_it(Pid),

    {ok,_Pid1} =
	gen_statem:start_link(LocalSTM, ?MODULE, [], []),
    stop_it(Name),

    {ok,Pid2} =
	gen_statem:start(GlobalSTM, ?MODULE, [], []),
    stop_it(Pid2),
    receive after 1 -> true end,
    Result =
	gen_statem:start(GlobalSTM, ?MODULE, [], []),
    io:format("Result = ~p~n",[Result]),
    {ok,_Pid3} = Result,
    stop_it(GlobalSTM),

    ok = verify_empty_msgq().

%% Via register linked
start12(Config) when is_list(Config) ->
    dummy_via:reset(),
    VIA = {via,dummy_via,my_stm},

    {ok,Pid} =
	gen_statem:start_link(VIA, ?MODULE, [], []),
    {error,{already_started,Pid}} =
	gen_statem:start_link(VIA, ?MODULE, [], []),
    {error,{already_started,Pid}} =
	gen_statem:start(VIA, ?MODULE, [], []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(VIA),
    ok = do_sync_func_test(VIA),
    stop_it(VIA),

    ok = verify_empty_msgq().


%% Anonymous, reason 'normal'
stop1(_Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, [], []),
    ok = gen_statem:stop(Pid),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(Pid), Reason).

%% Anonymous, other reason
stop2(_Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, [], []),
    ok = gen_statem:stop(Pid, other_reason, infinity),
    false = erlang:is_process_alive(Pid),
    ok.

%% Anonymous, invalid timeout
stop3(_Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, [], []),
    _ =
	?EXPECT_FAILURE(
	   gen_statem:stop(Pid, other_reason, invalid_timeout),
	   Reason),
    true = erlang:is_process_alive(Pid),
    ok = gen_statem:stop(Pid),
    false = erlang:is_process_alive(Pid),
    ok.

%% Registered name
stop4(_Config) ->
    {ok,Pid} = gen_statem:start({local,to_stop},?MODULE, [], []),
    ok = gen_statem:stop(to_stop),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(to_stop), Reason),
    ok.

%% Registered name and local node
stop5(_Config) ->
    Name = to_stop,
    {ok,Pid} = gen_statem:start({local,Name},?MODULE, [], []),
    ok = gen_statem:stop({Name,node()}),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop({Name,node()}), Reason),
    ok.

%% Globally registered name
stop6(_Config) ->
    STM = {global,to_stop},
    {ok,Pid} = gen_statem:start(STM, ?MODULE, [], []),
    ok = gen_statem:stop(STM),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason),
    ok.

%% 'via' registered name
stop7(_Config) ->
    VIA = {via,dummy_via,to_stop},
    dummy_via:reset(),
    {ok,Pid} = gen_statem:start(VIA,
				  ?MODULE, [], []),
    ok = gen_statem:stop(VIA),
    false = erlang:is_process_alive(Pid),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(VIA), Reason),
    ok.

%% Anonymous on remote node
stop8(_Config) ->
    {ok,Node} = ?t:start_node(gen_statem_stop8, slave, []),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node, code, add_path, [Dir]),
    {ok,Pid} = rpc:call(Node, gen_statem,start, [?MODULE,[],[]]),
    ok = gen_statem:stop(Pid),
    false = rpc:call(Node, erlang, is_process_alive, [Pid]),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(Pid), Reason1),
    true = ?t:stop_node(Node),
    {nodedown,Node} =
	?EXPECT_FAILURE(gen_statem:stop(Pid), Reason2),
    ok.

%% Registered name on remote node
stop9(_Config) ->
    Name = to_stop,
    LocalSTM = {local,Name},
    {ok,Node} = ?t:start_node(gen_statem__stop9, slave, []),
    STM = {Name,Node},
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node, code, add_path, [Dir]),
    {ok,Pid} = rpc:call(Node, gen_statem, start, [LocalSTM,?MODULE,[],[]]),
    ok = gen_statem:stop(STM),
    undefined = rpc:call(Node,erlang,whereis,[Name]),
    false = rpc:call(Node,erlang,is_process_alive,[Pid]),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason1),
    true = ?t:stop_node(Node),
    {nodedown,Node} =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason2),
    ok.

%% Globally registered name on remote node
stop10(_Config) ->
    STM = {global,to_stop},
    {ok,Node} = ?t:start_node(gen_statem_stop10, slave, []),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node,code,add_path,[Dir]),
    {ok,Pid} = rpc:call(Node, gen_statem, start, [STM,?MODULE,[],[]]),
    global:sync(),
    ok = gen_statem:stop(STM),
    false = rpc:call(Node, erlang, is_process_alive, [Pid]),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason1),
    true = ?t:stop_node(Node),
    noproc =
	?EXPECT_FAILURE(gen_statem:stop(STM), Reason2),
    ok.

%% Check that time outs in calls work
abnormal1(Config) when is_list(Config) ->
    Name = abnormal1,
    LocalSTM = {local,Name},

    {ok, _Pid} = gen_statem:start(LocalSTM, ?MODULE, [], []),

    %% timeout call.
    delayed = gen_statem:call(Name, {delayed_answer,1}, 100),
    {timeout,_} =
	?EXPECT_FAILURE(
	   gen_statem:call(Name, {delayed_answer,1000}, 10),
	   Reason),
    ok = verify_empty_msgq().

%% Check that bad return values makes the stm crash. Note that we must
%% trap exit since we must link to get the real bad_return_ error
abnormal2(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),
    {ok,Pid} = gen_statem:start_link(?MODULE, [], []),

    %% bad return value in the gen_statem loop
    {{bad_return_value,badreturn},_} =
	?EXPECT_FAILURE(gen_statem:call(Pid, badreturn), Reason),
    receive
	{'EXIT',Pid,{bad_return_value,badreturn}} -> ok
    after 5000 ->
	    ?t:fail(gen_statem_did_not_die)
    end,

    process_flag(trap_exit, OldFl),
    ok = verify_empty_msgq().

shutdown(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    {ok,Pid0} = gen_statem:start_link(?MODULE, [], []),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stopped = gen_statem:call(Pid0, {stop,{shutdown,reason}}),
    receive {'EXIT',Pid0,{shutdown,reason}} -> ok end,
    process_flag(trap_exit, false),

    {noproc,_} =
	?EXPECT_FAILURE(gen_statem:call(Pid0, hej), Reason),

    receive
	Any ->
	    io:format("Unexpected: ~p", [Any]),
	    ?t:fail()
    after 500 ->
	    ok
    end.



sys1(Config) when is_list(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, [], []),
    {status, Pid, {module,gen_statem}, _} = sys:get_status(Pid),
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
	    ?t:fail()
    after 3000 ->
	    exit(Caller, ok)
    end,

    %% {timeout,_} =
    %% 	?EXPECT_FAILURE(gen_statem:call(Pid, hej), Reason),
    sys:resume(Pid),
    stop_it(Pid).

call_format_status(Config) when is_list(Config) ->
    {ok,Pid} = gen_statem:start(?MODULE, [], []),
    Status = sys:get_status(Pid),
    {status,Pid,_Mod,[_PDict,running,_,_, Data]} = Status,
    [format_status_called|_] = lists:reverse(Data),
    stop_it(Pid),

    %% check that format_status can handle a name being an atom (pid is
    %% already checked by the previous test)
    {ok, Pid2} = gen_statem:start({local, gstm}, ?MODULE, [], []),
    Status2 = sys:get_status(gstm),
    {status,Pid2,_Mod,[_PDict2,running,_,_,Data2]} = Status2,
    [format_status_called|_] = lists:reverse(Data2),
    stop_it(Pid2),

    %% check that format_status can handle a name being a term other than a
    %% pid or atom
    GlobalName1 = {global,"CallFormatStatus"},
    {ok,Pid3} = gen_statem:start(GlobalName1, ?MODULE, [], []),
    Status3 = sys:get_status(GlobalName1),
    {status,Pid3,_Mod,[_PDict3,running,_,_,Data3]} = Status3,
    [format_status_called|_] = lists:reverse(Data3),
    stop_it(Pid3),
    GlobalName2 = {global,{name, "term"}},
    {ok,Pid4} = gen_statem:start(GlobalName2, ?MODULE, [], []),
    Status4 = sys:get_status(GlobalName2),
    {status,Pid4,_Mod,[_PDict4,running,_,_, Data4]} = Status4,
    [format_status_called|_] = lists:reverse(Data4),
    stop_it(Pid4),

    %% check that format_status can handle a name being a term other than a
    %% pid or atom
    dummy_via:reset(),
    ViaName1 = {via,dummy_via,"CallFormatStatus"},
    {ok,Pid5} = gen_statem:start(ViaName1, ?MODULE, [], []),
    Status5 = sys:get_status(ViaName1),
    {status,Pid5,_Mod, [_PDict5,running,_,_, Data5]} = Status5,
    [format_status_called|_] = lists:reverse(Data5),
    stop_it(Pid5),
    ViaName2 = {via,dummy_via,{name,"term"}},
    {ok, Pid6} = gen_statem:start(ViaName2, ?MODULE, [], []),
    Status6 = sys:get_status(ViaName2),
    {status,Pid6,_Mod,[_PDict6,running,_,_,Data6]} = Status6,
    [format_status_called|_] = lists:reverse(Data6),
    stop_it(Pid6).



error_format_status(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    StateData = "called format_status",
    {ok,Pid} = gen_statem:start(?MODULE, {state_data,StateData}, []),
    %% bad return value in the gen_statem loop
    {{bad_return_value,badreturn},_} =
	?EXPECT_FAILURE(gen_statem:call(Pid, badreturn), Reason),
    receive
	{error,_,
	 {Pid,
	  "** State machine"++_,
	  [Pid,{{call,_},badreturn},
	   {formatted,idle,StateData},
	   exit,{bad_return_value,badreturn}|_]}} ->
	    ok;
	Other when is_tuple(Other), element(1, Other) =:= error ->
	    error_logger_forwarder:unregister(),
	    ?t:fail({unexpected,Other})
    after 1000 ->
	    error_logger_forwarder:unregister(),
	    ?t:fail()
    end,
    process_flag(trap_exit, OldFl),
    error_logger_forwarder:unregister(),
    receive
	%% Comes with SASL
	{error_report,_,{Pid,crash_report,_}} ->
	    ok
    after 500 ->
	    ok
    end,
    ok = verify_empty_msgq().

terminate_crash_format(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    StateData = crash_terminate,
    {ok,Pid} = gen_statem:start(?MODULE, {state_data,StateData}, []),
    stop_it(Pid),
    Self = self(),
    receive
	{error,_GroupLeader,
	 {Pid,
	  "** State machine"++_,
	  [Pid,
	   {{call,{Self,_}},stop},
	   {formatted,idle,StateData},
	   exit,{crash,terminate}|_]}} ->
	    ok;
	Other when is_tuple(Other), element(1, Other) =:= error ->
	    error_logger_forwarder:unregister(),
	    ?t:fail({unexpected,Other})
    after 1000 ->
	    error_logger_forwarder:unregister(),
	    ?t:fail()
    end,
    process_flag(trap_exit, OldFl),
    error_logger_forwarder:unregister(),
    receive
	%% Comes with SASL
	{error_report,_,{Pid,crash_report,_}} ->
	    ok
    after 500 ->
	    ok
    end,
    ok = verify_empty_msgq().


get_state(Config) when is_list(Config) ->
    State = self(),
    {ok,Pid} = gen_statem:start(?MODULE, {state_data,State}, []),
    {idle,State} = sys:get_state(Pid),
    {idle,State} = sys:get_state(Pid, 5000),
    stop_it(Pid),

    %% check that get_state can handle a name being an atom (pid is
    %% already checked by the previous test)
    {ok,Pid2} =
	gen_statem:start({local,gstm}, ?MODULE, {state_data,State}, []),
    {idle,State} = sys:get_state(gstm),
    {idle,State} = sys:get_state(gstm, 5000),
    stop_it(Pid2),

    %% check that get_state works when pid is sys suspended
    {ok,Pid3} = gen_statem:start(?MODULE, {state_data,State}, []),
    {idle,State} = sys:get_state(Pid3),
    ok = sys:suspend(Pid3),
    {idle,State} = sys:get_state(Pid3, 5000),
    ok = sys:resume(Pid3),
    stop_it(Pid3),
    ok = verify_empty_msgq().

replace_state(Config) when is_list(Config) ->
    State = self(),
    {ok, Pid} = gen_statem:start(?MODULE, {state_data,State}, []),
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
    stop_it(Pid),
    ok = verify_empty_msgq().

%% Hibernation
hibernate(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    {ok,Pid0} = gen_statem:start_link(?MODULE, hiber_now, []),
    is_in_erlang_hibernate(Pid0),
    stop_it(Pid0),
    receive
	{'EXIT',Pid0,normal} -> ok
    after 5000 ->
	    ?t:fail(gen_statem_did_not_die)
    end,

    {ok,Pid} = gen_statem:start_link(?MODULE, hiber, []),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid,current_function)),
    hibernating = gen_statem:call(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    hibernating = gen_statem:call(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    please_just_five_more = gen_statem:call(Pid, snooze_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, snooze_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),

    Pid ! hibernate_later,
    true =
	({current_function,{erlang,hibernate,3}} =/=
	     erlang:process_info(Pid, current_function)),
    is_in_erlang_hibernate(Pid),

    'alive!' = gen_statem:call(Pid, 'alive?'),
    true =
	({current_function,{erlang,hibernate,3}} =/=
	     erlang:process_info(Pid, current_function)),
    Pid ! hibernate_now,
    is_in_erlang_hibernate(Pid),

    'alive!' = gen_statem:call(Pid, 'alive?'),
    true =
	({current_function,{erlang,hibernate,3}} =/=
	     erlang:process_info(Pid, current_function)),

    hibernating = gen_statem:call(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    hibernating = gen_statem:call(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    please_just_five_more = gen_statem:call(Pid, snooze_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok  = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, snooze_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_statem:cast(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),

    hibernating = gen_statem:call(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    sys:suspend(Pid),
    is_in_erlang_hibernate(Pid),
    sys:resume(Pid),
    is_in_erlang_hibernate(Pid),
    receive after 1000 -> ok end,
    is_in_erlang_hibernate(Pid),

    good_morning  = gen_statem:call(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    stop_it(Pid),
    process_flag(trap_exit, OldFl),
    receive
	{'EXIT',Pid,normal} -> ok
    after 5000 ->
	    ?t:fail(gen_statem_did_not_die)
    end,
    ok = verify_empty_msgq().

is_in_erlang_hibernate(Pid) ->
    receive after 1 -> ok end,
    is_in_erlang_hibernate_1(200, Pid).

is_in_erlang_hibernate_1(0, Pid) ->
    io:format("~p\n", [erlang:process_info(Pid, current_function)]),
    ?t:fail(not_in_erlang_hibernate_3);
is_in_erlang_hibernate_1(N, Pid) ->
    {current_function,MFA} = erlang:process_info(Pid, current_function),
    case MFA of
	{erlang,hibernate,3} ->
	    ok;
	_ ->
	    receive after 10 -> ok end,
	    is_in_erlang_hibernate_1(N-1, Pid)
    end.

is_not_in_erlang_hibernate(Pid) ->
    receive after 1 -> ok end,
    is_not_in_erlang_hibernate_1(200, Pid).

is_not_in_erlang_hibernate_1(0, Pid) ->
    io:format("~p\n", [erlang:process_info(Pid, current_function)]),
    ?t:fail(not_in_erlang_hibernate_3);
is_not_in_erlang_hibernate_1(N, Pid) ->
    {current_function,MFA} = erlang:process_info(Pid, current_function),
    case MFA of
	{erlang,hibernate,3} ->
	    receive after 10 -> ok end,
	    is_not_in_erlang_hibernate_1(N-1, Pid);
	_ ->
	    ok
    end.

%%sys1(suite) -> [];
%%sys1(_) ->

enter_loop(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),

    dummy_via:reset(),

    %% Locally registered process + {local,Name}
    {ok,Pid1a} =
	proc_lib:start_link(?MODULE, enter_loop, [local,local]),
    yes = gen_statem:call(Pid1a, 'alive?'),
    stopped = gen_statem:call(Pid1a, stop),
    receive
	{'EXIT',Pid1a,normal} ->
	    ok
    after 5000 ->
	    ?t:fail(gen_statem_did_not_die)
    end,

    %% Unregistered process + {local,Name}
    {ok,Pid1b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,local]),
    receive
	{'EXIT',Pid1b,process_not_registered} ->
	    ok
    after 5000 ->
	    ?t:fail(gen_statem_did_not_die)
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
	    ?t:fail(gen_statem_did_not_die)
    end,

    %% Unregistered process + {global,Name}
    {ok,Pid2b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,global]),
    receive
	{'EXIT',Pid2b,process_not_registered_globally} ->
	    ok
    after 5000 ->
	    ?t:fail(gen_statem_did_not_die)
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
	    ?t:fail(gen_statem_did_not_die)
    end,

    %% Process not started using proc_lib
    Pid4 =
	spawn_link(gen_statem, enter_loop, [?MODULE,[],state0,[]]),
    receive
	{'EXIT',Pid4,process_was_not_started_by_proc_lib} ->
	    ok
    after 5000 ->
	    ?t:fail(gen_statem_did_not_die)
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
	    ?t:fail(gen_statem_did_not_die)
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
	    ?t:fail(gen_statem_started)
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
	    ?t:fail(gen_statem_started)
    end,
    global:unregister_name(armitage),

    dummy_via:register_name(armitage, self()),
    {ok,Pid6c} =
	proc_lib:start_link(?MODULE, enter_loop, [anon,via]),
    receive
	{'EXIT',Pid6c,{process_not_registered_via,dummy_via}} ->
	    ok
    after 1000 ->
	    ?t:fail(
	      {gen_statem_started,
	       process_info(self(), messages)})
    end,
    dummy_via:unregister_name(armitage),

    process_flag(trap_exit, OldFlag),
    ok = verify_empty_msgq().

enter_loop(Reg1, Reg2) ->
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
	    gen_statem:enter_loop(?MODULE, [], state0, [], {local,armitage});
	global ->
	    gen_statem:enter_loop(?MODULE, [], state0, [], {global,armitage});
	via ->
	    gen_statem:enter_loop(?MODULE, [], state0, [],
			       {via, dummy_via, armitage});
	anon ->
	    gen_statem:enter_loop(?MODULE, [], state0, [])
    end.

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
    ?t:do_times(3, ?MODULE, do_msg, [STM]),
    ok = gen_statem:cast(STM, {'alive?',self()}),
    wfor(yes),
    ok = do_disconnect(STM),
    ok = gen_statem:cast(STM, {'alive?',self()}),
    wfor(yes),
    ok.


do_connect(STM) ->
    check_state(STM, idle),
    gen_statem:cast(STM, {connect,self()}),
    wfor(accept),
    check_state(STM, wfor_conf),
    gen_statem:cast(STM, confirm),
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
    ?t:do_times(3, ?MODULE, do_sync_msg, [STM]),
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
    yes = gen_statem:call(STM, confirm),
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
    receive after 500 -> ok end,
    [] = ?t:messages_get(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The State Machine
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(ignore) ->
    ignore;
init(stop) ->
    {stop,stopped};
init(stop_shutdown) ->
    {stop,shutdown};
init(sleep) ->
    ?t:sleep(1000),
    {ok,idle,data};
init(hiber) ->
    {ok,hiber_idle,[]};
init(hiber_now) ->
    {ok,hiber_idle,[],[hibernate]};
init({state_data, StateData}) ->
    {ok,idle,StateData};
init(_) ->
    {ok,idle,state_data}.

terminate(_, _State, crash_terminate) ->
    exit({crash,terminate});
terminate({From,stopped}, State, _Data) ->
    From ! {self(),{stopped,State}},
    ok;
terminate(_Reason, _State, _Data) ->
    ok.


%% State functions

idle(cast, {connect,Pid}, _, _, Data) ->
    Pid ! accept,
    {wfor_conf,Data};
idle({call,From}, connect, _, _, Data) ->
    {wfor_conf,Data,[{reply,From,accept}]};
idle(cast, badreturn, _, _, _Data) ->
    badreturn;
idle({call,_From}, badreturn, _, _, _Data) ->
    badreturn;
idle({call,From}, {delayed_answer,T}, _, _, _Data) ->
    receive
    after T ->
	    [{reply,From,delayed}]
    end;
idle({call,From}, {timeout,Time}, _, State, _Data) ->
    {timeout, {From,Time}, [{timeout,Time,State}]};
idle(Type, Content, PrevState, State, Data) ->
    case handle_common_events(Type, Content, PrevState, State, Data) of
	[] ->
	    case Type of
		{call,From} ->
		    {State,Data,[{reply,From,'eh?'}]};
		_ ->
		    {State,Data,
		     [{stop,{unexpected,State,PrevState,Type,Content}}]}
	    end;
	Result ->
	    Result
    end.

timeout(timeout, idle, idle, timeout, {From,Time}) ->
    TRef2 = erlang:start_timer(Time, self(), ok),
    TRefC1 = erlang:start_timer(Time, self(), cancel1),
    TRefC2 = erlang:start_timer(Time, self(), cancel2),
    {timeout2,
     {From, Time, TRef2},
     [{cancel_timer, TRefC1},
      {insert_event,internal,{cancel_timer,TRefC2}}]};
timeout(_, _, _, _, Data) ->
    {Data}.

timeout2(
  internal, {cancel_timer,TRefC2}, timeout, _, {From,Time,TRef2}) ->
    Time4 = Time * 4,
    receive after Time4 -> ok end,
    {timeout3,{From,TRef2},[{cancel_timer,TRefC2}]};
timeout2(_, _, _, _, Data) ->
    {Data}.

timeout3(info, {timeout,TRef2,Result}, _, _, {From,TRef2}) ->
    {idle,state,[{reply,From,Result}]};
timeout3(_, _, _, _, Data) ->
    {Data}.

wfor_conf({call,From}, confirm, _, _, Data) ->
    {connected,Data,[{reply,From,yes}]};
wfor_conf(cast, confirm, _, _, Data) ->
    {connected,Data};
wfor_conf(Type, Content, PrevState, State, Data) ->
    case handle_common_events(Type, Content, PrevState, State, Data) of
	[] ->
	    case Type of
		{call,From} ->
		    {idle,Data,[{reply,From,'eh?'}]};
		_ ->
		    {Data}
	    end;
	Result ->
	    Result
    end.

connected({call,From}, {msg,Ref}, _, State, Data) ->
    {State,Data,[{reply,From,{ack,Ref}}]};
connected(cast, {msg,From,Ref}, _, _, _Data) ->
    From ! {ack,Ref},
    {};
connected({call,From}, disconnect, _, _, Data) ->
    {idle,Data,[{reply,From,yes}]};
connected(cast, disconnect, _, _, Data) ->
    {idle,Data};
connected(Type, Content, PrevState, State, Data) ->
    case handle_common_events(Type, Content, PrevState, State, Data) of
	[] ->
	    case Type of
		{call,From} ->
		    [{reply,From,'eh?'}];
		_ ->
		    {Data}
	    end;
	Result ->
	    Result
    end.

state0({call,From}, stop, _, State, Data) ->
    {State,Data,
     [{reply,From,stopped},
      {stop,normal}]};
state0(Type, Content, PrevState, State, Data) ->
    case handle_common_events(Type, Content, PrevState, State, Data) of
	[] ->
	    {Data};
	Result ->
	    Result
    end.

hiber_idle({call,From}, 'alive?', _, _, _) ->
    [{reply,From,'alive!'}];
hiber_idle({call,From}, hibernate_sync, _, _, Data) ->
    {hiber_wakeup,Data,
     [{reply,From,hibernating},
      hibernate]};
hiber_idle(info, hibernate_later, _, State, _) ->
    Tref = erlang:start_timer(1000, self(), hibernate),
    {State,Tref};
hiber_idle(info, hibernate_now, _, State, Data) ->
    {State,Data,[hibernate]};
hiber_idle(info, {timeout,Tref,hibernate}, _, State, Tref) ->
    {State,[],
     [hibernate]};
hiber_idle(cast, hibernate_async, _, _, Data) ->
    {hiber_wakeup,Data,
     [hibernate]};
hiber_idle(Type, Content, PrevState, State, Data) ->
    case handle_common_events(Type, Content, PrevState, State, Data) of
	[] ->
	    {Data};
	Result ->
	    Result
    end.

hiber_wakeup({call,From}, wakeup_sync, _, _, Data) ->
    {hiber_idle,Data,[{reply,From,good_morning}]};
hiber_wakeup({call,From}, snooze_sync, _, State, Data) ->
    {State,Data,
     [{reply,From,please_just_five_more},
      hibernate]};
hiber_wakeup(cast, wakeup_async, _, _, Data) ->
    {hiber_idle,Data};
hiber_wakeup(cast, snooze_async, _, _, _Data) ->
    [hibernate];
hiber_wakeup(Type, Content, PrevState, State, Data) ->
    case handle_common_events(Type, Content, PrevState, State, Data) of
	[] ->
	    {Data};
	Result ->
	    Result
    end.


handle_common_events({call,From}, get, _, State, Data) ->
    [{reply,From,{state,State,Data}}];
handle_common_events(cast, {get,Pid}, _, State, Data) ->
    Pid ! {state,State,Data},
    {};
handle_common_events({call,From}, stop, _, _, _) ->
    [{reply,From,stopped},
     {stop,normal}];
handle_common_events(cast, stop, _, State, Data) ->
    {State,Data,
     [{stop,normal}]};
handle_common_events({call,From}, {stop,Reason}, _, State, Data) ->
    {State,Data,
     [{reply,From,stopped},
      {stop,Reason}]};
handle_common_events(cast, {stop,Reason}, _, _, _) ->
     [{stop,Reason}];
handle_common_events({call,From}, 'alive?', _, State, Data) ->
    {State,Data,
     [{reply,From,yes}]};
handle_common_events(cast, {'alive?',Pid}, _, State, Data) ->
    Pid ! yes,
    {State,Data};
handle_common_events(_, _, _, _, _) ->
    [].

code_change(_OldVsn, State, StateData, _Extra) ->
    {ok,State,StateData}.

format_status(terminate, [_Pdict,State,StateData]) ->
    {formatted,State,StateData};
format_status(normal, [_Pdict,_State,_StateData]) ->
    [format_status_called].
