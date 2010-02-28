%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(gen_fsm_SUITE).

-include("test_server.hrl").

%% Test cases
-export([all/1]).

-export([start/1, start1/1, start2/1, start3/1, start4/1 , start5/1, start6/1,
	 start7/1, start8/1, start9/1, start10/1, start11/1]).

-export([abnormal/1, abnormal1/1, abnormal2/1]).

-export([shutdown/1]).

-export([sys/1, sys1/1, call_format_status/1, error_format_status/1]).

-export([hibernate/1,hiber_idle/3,hiber_wakeup/3,hiber_idle/2,hiber_wakeup/2]).

-export([enter_loop/1]).

%% Exports for apply
-export([do_msg/1, do_sync_msg/1]).
-export([enter_loop/2]).

% The gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, terminate/3,
	 handle_info/3, format_status/2]).
-export([idle/2,	idle/3,
	 timeout/2,
	 wfor_conf/2,	wfor_conf/3,
	 connected/2,	connected/3]).
-export([state0/3]).
	 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all(suite) ->
    [start, abnormal, shutdown, sys, hibernate, enter_loop].
    


start(suite) -> [start1, start2, start3, start4, start5, start6, start7,
		 start8, start9, start10, start11].

%% anonymous
start1(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    ?line {ok, Pid0} = gen_fsm:start_link(gen_fsm_SUITE, [], []),
    ?line ok = do_func_test(Pid0),
    ?line ok = do_sync_func_test(Pid0),
    stop_it(Pid0),
%%    ?line stopped = gen_fsm:sync_send_all_state_event(Pid0, stop),
%%    ?line {'EXIT', {timeout,_}} = 
%%	(catch gen_fsm:sync_send_event(Pid0, hej)),

    ?line test_server:messages_get(),
    %%process_flag(trap_exit, OldFl),
   ok.

%% anonymous w. shutdown
start2(Config) when is_list(Config) ->
    %% Dont link when shutdown
    ?line {ok, Pid0} = gen_fsm:start(gen_fsm_SUITE, [], []),
    ?line ok = do_func_test(Pid0),
    ?line ok = do_sync_func_test(Pid0),
    ?line shutdown_stopped = 
	gen_fsm:sync_send_all_state_event(Pid0, stop_shutdown),
    ?line {'EXIT', {noproc,_}} = 
	(catch gen_fsm:sync_send_event(Pid0, hej)),

    ?line test_server:messages_get(),
    ok.

%% anonymous with timeout
start3(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    ?line {ok, Pid0} = gen_fsm:start(gen_fsm_SUITE, [], [{timeout,5}]),
    ?line ok = do_func_test(Pid0),
    ?line ok = do_sync_func_test(Pid0),
    ?line stop_it(Pid0),
    
    ?line {error, timeout} = gen_fsm:start(gen_fsm_SUITE, sleep,
					   [{timeout,5}]),

    test_server:messages_get(),
    %%process_flag(trap_exit, OldFl),
    ok.

%% anonymous with ignore
start4(suite) -> [];
start4(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    ?line ignore = gen_fsm:start(gen_fsm_SUITE, ignore, []),

    test_server:messages_get(),
    process_flag(trap_exit, OldFl),
    ok.

%% anonymous with stop
start5(suite) -> [];
start5(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    ?line {error, stopped} = gen_fsm:start(gen_fsm_SUITE, stop, []),

    test_server:messages_get(),
    process_flag(trap_exit, OldFl),
    ok.

%% anonymous linked
start6(Config) when is_list(Config) ->
    ?line {ok, Pid} = gen_fsm:start_link(gen_fsm_SUITE, [], []),
    ?line ok = do_func_test(Pid),
    ?line ok = do_sync_func_test(Pid),
    ?line stop_it(Pid),

    test_server:messages_get(),

    ok.

%% global register linked
start7(Config) when is_list(Config) ->
    ?line {ok, Pid} = 
	gen_fsm:start_link({global, my_fsm}, gen_fsm_SUITE, [], []),
    ?line {error, {already_started, Pid}} =
	gen_fsm:start_link({global, my_fsm}, gen_fsm_SUITE, [], []),
    ?line {error, {already_started, Pid}} =
	gen_fsm:start({global, my_fsm}, gen_fsm_SUITE, [], []),
    
    ?line ok = do_func_test(Pid),
    ?line ok = do_sync_func_test(Pid),
    ?line ok = do_func_test({global, my_fsm}),
    ?line ok = do_sync_func_test({global, my_fsm}),
    ?line stop_it({global, my_fsm}),
    
    test_server:messages_get(),
    ok.


%% local register
start8(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    ?line {ok, Pid} = 
	gen_fsm:start({local, my_fsm}, gen_fsm_SUITE, [], []),
    ?line {error, {already_started, Pid}} =
	gen_fsm:start({local, my_fsm}, gen_fsm_SUITE, [], []),

    ?line ok = do_func_test(Pid),
    ?line ok = do_sync_func_test(Pid),
    ?line ok = do_func_test(my_fsm),
    ?line ok = do_sync_func_test(my_fsm),
    ?line stop_it(Pid),
    
    test_server:messages_get(),
    %%process_flag(trap_exit, OldFl),
    ok.

%% local register linked
start9(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    ?line {ok, Pid} = 
	gen_fsm:start_link({local, my_fsm}, gen_fsm_SUITE, [], []),
    ?line {error, {already_started, Pid}} =
	gen_fsm:start({local, my_fsm}, gen_fsm_SUITE, [], []),

    ?line ok = do_func_test(Pid),
    ?line ok = do_sync_func_test(Pid),
    ?line ok = do_func_test(my_fsm),
    ?line ok = do_sync_func_test(my_fsm),
    ?line stop_it(Pid),
    
    test_server:messages_get(),
    %%process_flag(trap_exit, OldFl),
    ok.

%% global register
start10(Config) when is_list(Config) ->
    ?line {ok, Pid} = 
	gen_fsm:start({global, my_fsm}, gen_fsm_SUITE, [], []),
    ?line {error, {already_started, Pid}} =
	gen_fsm:start({global, my_fsm}, gen_fsm_SUITE, [], []),
    ?line {error, {already_started, Pid}} =
	gen_fsm:start_link({global, my_fsm}, gen_fsm_SUITE, [], []),
    
    ?line ok = do_func_test(Pid),
    ?line ok = do_sync_func_test(Pid),
    ?line ok = do_func_test({global, my_fsm}),
    ?line ok = do_sync_func_test({global, my_fsm}),
    ?line stop_it({global, my_fsm}),
    
    test_server:messages_get(),
    ok.


%% Stop registered processes
start11(Config) when is_list(Config) ->
    ?line {ok, Pid} = 
	gen_fsm:start_link({local, my_fsm}, gen_fsm_SUITE, [], []),
    ?line stop_it(Pid),

    ?line {ok, _Pid1} = 
	gen_fsm:start_link({local, my_fsm}, gen_fsm_SUITE, [], []),
    ?line stop_it(my_fsm),
    
    ?line {ok, Pid2} = 
	gen_fsm:start({global, my_fsm}, gen_fsm_SUITE, [], []),
    ?line stop_it(Pid2),
    receive after 1 -> true end,
    ?line Result = 
	gen_fsm:start({global, my_fsm}, gen_fsm_SUITE, [], []),
    io:format("Result = ~p~n",[Result]),
    ?line {ok, _Pid3} = Result, 
    ?line stop_it({global, my_fsm}),

    test_server:messages_get(),
    ok.

abnormal(suite) -> [abnormal1, abnormal2].

%% Check that time outs in calls work
abnormal1(suite) -> [];
abnormal1(Config) when is_list(Config) ->
    ?line {ok, _Pid} = 
	gen_fsm:start({local, my_fsm}, gen_fsm_SUITE, [], []),

    %% timeout call.
    case os:type() of
	vxworks ->
	    %% timeout call for VxWorks must be in 16ms increments.
	    ?line delayed = gen_fsm:sync_send_event(my_fsm, {delayed_answer,1}, 17),
	    ?line {'EXIT',{timeout,_}} =
		(catch gen_fsm:sync_send_event(my_fsm, {delayed_answer,17}, 1));
	_ ->
	    ?line delayed = gen_fsm:sync_send_event(my_fsm, {delayed_answer,1}, 100),
	    ?line {'EXIT',{timeout,_}} =
		(catch gen_fsm:sync_send_event(my_fsm, {delayed_answer,10}, 1))
    end,
    test_server:messages_get(),
    ok.

%% Check that bad return values makes the fsm crash. Note that we must
%% trap exit since we must link to get the real bad_return_ error
abnormal2(suite) -> [];
abnormal2(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),
    ?line {ok, Pid} = 
	gen_fsm:start_link(gen_fsm_SUITE, [], []),

    %% bad return value in the gen_fsm loop
    ?line {'EXIT',{{bad_return_value, badreturn},_}} =
	(catch gen_fsm:sync_send_event(Pid, badreturn)),
    
    test_server:messages_get(),
    process_flag(trap_exit, OldFl),
    ok.

shutdown(Config) when is_list(Config) ->
    ?line error_logger_forwarder:register(),

    process_flag(trap_exit, true),

    ?line {ok,Pid0} = gen_fsm:start_link(gen_fsm_SUITE, [], []),
    ?line ok = do_func_test(Pid0),
    ?line ok = do_sync_func_test(Pid0),
    ?line {shutdown,reason} = 
	gen_fsm:sync_send_all_state_event(Pid0, stop_shutdown_reason),
    receive {'EXIT',Pid0,{shutdown,reason}} -> ok end,
    process_flag(trap_exit, false),

    ?line {'EXIT', {noproc,_}} = 
	(catch gen_fsm:sync_send_event(Pid0, hej)),

    receive
	Any ->
	    ?line io:format("Unexpected: ~p", [Any]),
	    ?line ?t:fail()
    after 500 ->
	    ok
    end,

    ok.


sys(suite) -> [sys1, call_format_status, error_format_status].

sys1(Config) when is_list(Config) ->
    ?line {ok, Pid} = 
	gen_fsm:start(gen_fsm_SUITE, [], []),
    ?line {status, Pid, {module,gen_fsm}, _} = sys:get_status(Pid),
    ?line sys:suspend(Pid),
    ?line {'EXIT', {timeout,_}} = 
	(catch gen_fsm:sync_send_event(Pid, hej)),
    ?line sys:resume(Pid),
    ?line stop_it(Pid).

call_format_status(Config) when is_list(Config) ->
    ?line {ok, Pid} = gen_fsm:start(gen_fsm_SUITE, [], []),
    ?line Status = sys:get_status(Pid),
    ?line {status, Pid, _Mod, [_PDict, running, _Parent, _, Data]} = Status,
    ?line [format_status_called | _] = lists:reverse(Data),
    ?line stop_it(Pid).

error_format_status(Config) when is_list(Config) ->
    ?line error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    StateData = "called format_status",
    ?line {ok, Pid} = gen_fsm:start(gen_fsm_SUITE, {state_data, StateData}, []),
    %% bad return value in the gen_fsm loop
    ?line {'EXIT',{{bad_return_value, badreturn},_}} =
	(catch gen_fsm:sync_send_event(Pid, badreturn)),
    receive
	{error,_GroupLeader,{Pid,
			     "** State machine"++_,
			     [Pid,{_,_,badreturn},idle,StateData,_]}} ->
	    ok;
	Other ->
	    ?line io:format("Unexpected: ~p", [Other]),
	    ?line ?t:fail()
    end,
    ?t:messages_get(),
    process_flag(trap_exit, OldFl),
    ok.


%% Hibernation
hibernate(suite) -> [];
hibernate(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    ?line {ok, Pid0} = gen_fsm:start_link(?MODULE, hiber_now, []),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid0,current_function),
    ?line stop_it(Pid0),
    test_server:messages_get(),


    ?line {ok, Pid} = gen_fsm:start_link(?MODULE, hiber, []),
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    ?line hibernating = gen_fsm:sync_send_event(Pid,hibernate_sync),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line good_morning  = gen_fsm:sync_send_event(Pid,wakeup_sync),
    ?line receive after 1000 -> ok end,
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    ?line hibernating = gen_fsm:sync_send_event(Pid,hibernate_sync),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line five_more  = gen_fsm:sync_send_event(Pid,snooze_sync),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line good_morning  = gen_fsm:sync_send_event(Pid,wakeup_sync),
    ?line receive after 1000 -> ok end,
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    ?line ok = gen_fsm:send_event(Pid,hibernate_async),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line ok  = gen_fsm:send_event(Pid,wakeup_async),
    ?line receive after 1000 -> ok end,
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    ?line ok = gen_fsm:send_event(Pid,hibernate_async),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line ok  = gen_fsm:send_event(Pid,snooze_async),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line ok = gen_fsm:send_event(Pid,wakeup_async),
    ?line receive after 1000 -> ok end,
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    ?line Pid ! hibernate_later,
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    ?line receive after 2000 -> ok end,
    ?line ({current_function,{erlang,hibernate,3}} = erlang:process_info(Pid,current_function)),
    ?line 'alive!' = gen_fsm:sync_send_event(Pid,'alive?'),
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    ?line Pid ! hibernate_now,
    ?line receive after 1000 -> ok end,
    ?line ({current_function,{erlang,hibernate,3}} = erlang:process_info(Pid,current_function)),
    ?line 'alive!' = gen_fsm:sync_send_event(Pid,'alive?'),
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    

    ?line hibernating = gen_fsm:sync_send_all_state_event(Pid,hibernate_sync),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line good_morning  = gen_fsm:sync_send_all_state_event(Pid,wakeup_sync),
    ?line receive after 1000 -> ok end,
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    ?line hibernating = gen_fsm:sync_send_all_state_event(Pid,hibernate_sync),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line five_more  = gen_fsm:sync_send_all_state_event(Pid,snooze_sync),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line good_morning  = gen_fsm:sync_send_all_state_event(Pid,wakeup_sync),
    ?line receive after 1000 -> ok end,
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    ?line ok = gen_fsm:send_all_state_event(Pid,hibernate_async),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line ok  = gen_fsm:send_all_state_event(Pid,wakeup_async),
    ?line receive after 1000 -> ok end,
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    ?line ok = gen_fsm:send_all_state_event(Pid,hibernate_async),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line ok  = gen_fsm:send_all_state_event(Pid,snooze_async),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line ok = gen_fsm:send_all_state_event(Pid,wakeup_async),
    ?line receive after 1000 -> ok end,
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),

    ?line hibernating = gen_fsm:sync_send_all_state_event(Pid,hibernate_sync),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line sys:suspend(Pid),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = erlang:process_info(Pid,current_function),
    ?line sys:resume(Pid),
    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = erlang:process_info(Pid,current_function),

    ?line receive after 1000 -> ok end,
    ?line {current_function,{erlang,hibernate,3}} = 
	erlang:process_info(Pid,current_function),
    ?line good_morning  = gen_fsm:sync_send_all_state_event(Pid,wakeup_sync),
    ?line receive after 1000 -> ok end,
    ?line true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),
    ?line stop_it(Pid),
    test_server:messages_get(),
    process_flag(trap_exit, OldFl),
    ok.



%%sys1(suite) -> [];
%%sys1(_) ->

enter_loop(suite) ->
    [];
enter_loop(doc) ->
    ["Test gen_fsm:enter_loop/4,5,6"];
enter_loop(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),

    %% Locally registered process + {local, Name}
    ?line {ok, Pid1a} =
	proc_lib:start_link(?MODULE, enter_loop, [local, local]),
    ?line yes = gen_fsm:sync_send_event(Pid1a, 'alive?'),
    ?line stopped = gen_fsm:sync_send_event(Pid1a, stop),
    receive
	{'EXIT', Pid1a, normal} ->
	    ok
    after 5000 ->
	    ?line test_server:fail(gen_fsm_did_not_die)
    end,

    %% Unregistered process + {local, Name}
    ?line {ok, Pid1b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, local]),
    receive
	{'EXIT', Pid1b, process_not_registered} ->
	    ok
    after 5000 ->
	    ?line test_server:fail(gen_fsm_did_not_die)
    end,

    %% Globally registered process + {global, Name}
    ?line {ok, Pid2a} =
	proc_lib:start_link(?MODULE, enter_loop, [global, global]),
    ?line yes = gen_fsm:sync_send_event(Pid2a, 'alive?'),
    ?line stopped = gen_fsm:sync_send_event(Pid2a, stop),
    receive
	{'EXIT', Pid2a, normal} ->
	    ok
    after 5000 ->
	    ?line test_server:fail(gen_fsm_did_not_die)
    end,

    %% Unregistered process + {global, Name}
    ?line {ok, Pid2b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, global]),
    receive
	{'EXIT', Pid2b, process_not_registered_globally} ->
	    ok
    after 5000 ->
	    ?line test_server:fail(gen_fsm_did_not_die)
    end,

    %% Unregistered process + no name
    ?line {ok, Pid3} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, anon]),
    ?line yes = gen_fsm:sync_send_event(Pid3, 'alive?'),
    ?line stopped = gen_fsm:sync_send_event(Pid3, stop),
    receive
	{'EXIT', Pid3, normal} ->
	    ok
    after 5000 ->
	    ?line test_server:fail(gen_fsm_did_not_die)
    end,

    %% Process not started using proc_lib
    ?line Pid4 =
	spawn_link(gen_fsm, enter_loop, [?MODULE, [], state0, []]),
    receive
	{'EXIT', Pid4, process_was_not_started_by_proc_lib} ->
	    ok
    after 5000 ->
	    ?line test_server:fail(gen_fsm_did_not_die)
    end,

    %% Make sure I am the parent, ie that ordering a shutdown will
    %% result in the process terminating with Reason==shutdown
    ?line {ok, Pid5} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, anon]),
    ?line yes = gen_fsm:sync_send_event(Pid5, 'alive?'),
    ?line exit(Pid5, shutdown),
    receive
	{'EXIT', Pid5, shutdown} ->
	    ok
    after 5000 ->
	    ?line test_server:fail(gen_fsm_did_not_die)
    end,

    %% Make sure gen_fsm:enter_loop does not accept {local,Name}
    %% when it's another process than the calling one which is
    %% registered under that name
    register(armitage, self()),
    ?line {ok, Pid6a} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, local]),
    receive
	{'EXIT', Pid6a, process_not_registered} ->
	    ok
    after 1000 ->
	    ?line test_server:fail(gen_fsm_started)
    end,
    unregister(armitage),

    %% Make sure gen_fsm:enter_loop does not accept {global,Name}
    %% when it's another process than the calling one which is
    %% registered under that name
    global:register_name(armitage, self()),
    ?line {ok, Pid6b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, global]),
    receive
	{'EXIT', Pid6b, process_not_registered_globally} ->
	    ok
    after 1000 ->
	    ?line test_server:fail(gen_server_started)
    end,
    global:unregister_name(armitage),

    process_flag(trap_exit, OldFlag),
    ok.

enter_loop(Reg1, Reg2) ->
    process_flag(trap_exit, true),
    case Reg1 of
	local -> register(armitage, self());
	global -> global:register_name(armitage, self());
	anon -> ignore
    end,
    proc_lib:init_ack({ok, self()}),
    case Reg2 of
	local ->
	    gen_fsm:enter_loop(?MODULE, [], state0, [], {local,armitage});
	global ->
	    gen_fsm:enter_loop(?MODULE, [], state0, [], {global,armitage});
	anon ->
	    gen_fsm:enter_loop(?MODULE, [], state0, [])
    end.

%%
%% Functionality check
%%

wfor(Msg) ->
    receive 
	Msg -> ok
    after 5000 -> 
	    throw(timeout)
    end.


stop_it(FSM) ->
    ?line stopped = gen_fsm:sync_send_all_state_event(FSM, stop),
    ?line {'EXIT',_} = 	(catch gen_fsm:sync_send_event(FSM, hej)),
    ok.



do_func_test(FSM) ->
    ok = gen_fsm:send_all_state_event(FSM, {'alive?', self()}),
    wfor(yes),
    ok = do_connect(FSM),
    ok = gen_fsm:send_all_state_event(FSM, {'alive?', self()}),
    wfor(yes),
    test_server:do_times(3, ?MODULE, do_msg, [FSM]),
    ok = gen_fsm:send_all_state_event(FSM, {'alive?', self()}),
    wfor(yes),
    ok = do_disconnect(FSM),
    ok = gen_fsm:send_all_state_event(FSM, {'alive?', self()}),
    wfor(yes),
    ok.


do_connect(FSM) ->
    check_state(FSM, idle),
    gen_fsm:send_event(FSM, {connect, self()}),
    wfor(accept),
    check_state(FSM, wfor_conf),
    gen_fsm:send_event(FSM, confirmation),
    check_state(FSM, connected),
    ok.

do_msg(FSM) ->
    check_state(FSM, connected),
    R = make_ref(),
    ok = gen_fsm:send_event(FSM, {msg, R, self(), hej_pa_dig_quasimodo}),
    wfor({ak, R}).


do_disconnect(FSM) ->
    ok = gen_fsm:send_event(FSM, disconnect),
    check_state(FSM, idle).

check_state(FSM, State) ->
    case gen_fsm:sync_send_all_state_event(FSM, {get, self()}) of
	{state, State, _} -> ok
    end.

do_sync_func_test(FSM) ->
    yes = gen_fsm:sync_send_all_state_event(FSM, 'alive?'),
    ok = do_sync_connect(FSM),
    yes = gen_fsm:sync_send_all_state_event(FSM, 'alive?'),
    test_server:do_times(3, ?MODULE, do_sync_msg, [FSM]),
    yes = gen_fsm:sync_send_all_state_event(FSM, 'alive?'),
    ok = do_sync_disconnect(FSM),
    yes = gen_fsm:sync_send_all_state_event(FSM, 'alive?'),
    check_state(FSM, idle),
    ok = gen_fsm:sync_send_event(FSM, {timeout,200}),
    yes = gen_fsm:sync_send_all_state_event(FSM, 'alive?'),
    check_state(FSM, idle),
    ok.


do_sync_connect(FSM) ->
    check_state(FSM, idle),
    accept = gen_fsm:sync_send_event(FSM, {connect, self()}),
    check_state(FSM, wfor_conf),
    yes = gen_fsm:sync_send_event(FSM, confirmation),
    check_state(FSM, connected),
    ok.

do_sync_msg(FSM) ->
    check_state(FSM, connected),
    R = make_ref(),
    Res = gen_fsm:sync_send_event(FSM, {msg, R, self(), hej_pa_dig_quasimodo}),
    if  Res == {ak, R} ->
	    ok
    end.

do_sync_disconnect(FSM) ->
    yes = gen_fsm:sync_send_event(FSM, disconnect),
    check_state(FSM, idle).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The Finite State Machine
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(ignore) ->
    ignore;
init(stop) ->
    {stop, stopped};
init(stop_shutdown) ->
    {stop, shutdown};
init(sleep) ->
    test_server:sleep(1000),
    {ok, idle, data};
init({timeout, T}) ->
    {ok, idle, state, T};
init(hiber) ->
    {ok, hiber_idle, []};
init(hiber_now) ->
    {ok, hiber_idle, [], hibernate};
init({state_data, StateData}) ->
    {ok, idle, StateData};
init(_) ->
    {ok, idle, state_data}.


terminate({From, stopped}, State, _Data) ->
    From ! {self(), {stopped, State}},
    ok;
terminate(_Reason, _State, _Data) ->
    ok.


idle({connect, Pid}, Data) ->
    Pid ! accept,
    {next_state, wfor_conf, Data};
idle(badreturn, _Data) ->
    badreturn;
idle(_, Data) ->
    {next_state, idle, Data}.

idle({connect, _Pid}, _From, Data) ->
    {reply, accept, wfor_conf, Data};
idle({delayed_answer, T}, _From, Data) ->
    test_server:sleep(T),
    {reply, delayed, idle, Data};
idle(badreturn, _From, _Data) ->
    badreturn;
idle({timeout,Time}, From, _Data) ->
    gen_fsm:send_event_after(Time, {timeout,Time}),
    {next_state, timeout, From};
idle(_, _From, Data) ->
    {reply, 'eh?', idle, Data}.

timeout({timeout,Time}, From) ->
    Ref = gen_fsm:start_timer(Time, {timeout,Time}),
    {next_state, timeout, {From,Ref}};
timeout({timeout,Ref,{timeout,Time}}, {From,Ref}) ->
    Ref2 = gen_fsm:start_timer(Time, ok),
    Cref = gen_fsm:start_timer(Time, cancel),
    Time4 = Time*4,
    receive after Time4 -> ok end,
    gen_fsm:cancel_timer(Cref),
    {next_state, timeout, {From,Ref2}};
timeout({timeout,Ref2,ok},{From,Ref2}) ->
    gen_fsm:reply(From, ok),
    {next_state, idle, state}.

wfor_conf(confirmation, Data) ->
    {next_state, connected, Data};
wfor_conf(_, Data) ->
    {next_state, idle, Data}.

wfor_conf(confirmation, _From, Data) ->
    {reply, yes, connected, Data};
wfor_conf(_, _From, Data) ->
    {reply, 'eh?', idle, Data}.

connected({msg, Ref, From, _Msg}, Data) ->
    From ! {ak, Ref},
    {next_state, connected, Data};
connected(disconnect, Data) ->
    {next_state, idle, Data};
connected(_, Data) ->
    {next_state, connected, Data}.

connected({msg, Ref, _From, _Msg}, _, Data) ->
    {reply, {ak, Ref}, connected, Data};
connected(disconnect, _From, Data) ->
    {reply, yes, idle, Data};
connected(_, _, Data) ->
    {reply, 'eh?', connected, Data}.

state0('alive?', _From, Data) ->
    {reply, yes, state0, Data};
state0(stop, _From, Data) ->
    {stop, normal, stopped, Data}.

hiber_idle('alive?', _From, Data) ->
    {reply, 'alive!', hiber_idle, Data};
hiber_idle(hibernate_sync, _From, Data) ->
    {reply, hibernating, hiber_wakeup, Data,hibernate}.
hiber_idle(timeout, hibernate_me) ->  % Arrive here from 
				              % handle_info(hibernate_later,...)
    {next_state, hiber_idle, [], hibernate};
hiber_idle(hibernate_async, Data) ->
    {next_state,hiber_wakeup, Data, hibernate}.

hiber_wakeup(wakeup_sync,_From,Data) ->
    {reply,good_morning,hiber_idle,Data};
hiber_wakeup(snooze_sync,_From,Data) ->
    {reply,five_more,hiber_wakeup,Data,hibernate}.
hiber_wakeup(wakeup_async,Data) ->
    {next_state,hiber_idle,Data};
hiber_wakeup(snooze_async,Data) ->
    {next_state,hiber_wakeup,Data,hibernate}.
    

handle_info(hibernate_now, _SName, _State) ->  % Arrive here from by direct ! from testcase
    {next_state, hiber_idle, [], hibernate};
handle_info(hibernate_later, _SName, _State) ->
    {next_state, hiber_idle, hibernate_me, 1000};

handle_info(Info, _State, Data) ->
    {stop, {unexpected,Info}, Data}.

handle_event(hibernate_async, hiber_idle, Data) ->
    {next_state,hiber_wakeup, Data, hibernate};
handle_event(wakeup_async,hiber_wakeup,Data) ->
    {next_state,hiber_idle,Data};
handle_event(snooze_async,hiber_wakeup,Data) ->
    {next_state,hiber_wakeup,Data,hibernate};
handle_event({get, Pid}, State, Data) ->
    Pid ! {state, State, Data},
    {next_state, State, Data};
handle_event(stop, _State, Data) ->
    {stop, normal, Data};
handle_event(stop_shutdown, _State, Data) ->
    {stop, shutdown, Data};
handle_event(stop_shutdown_reason, _State, Data) ->
    {stop, shutdown, Data};
handle_event({'alive?', Pid}, State, Data) ->
    Pid ! yes,
    {next_state, State, Data}.

handle_sync_event(hibernate_sync, _From, hiber_idle, Data) ->
    {reply, hibernating, hiber_wakeup, Data, hibernate};
handle_sync_event(wakeup_sync,_From,hiber_wakeup, Data) ->
    {reply,good_morning,hiber_idle,Data};
handle_sync_event(snooze_sync,_From,hiber_wakeup,Data) ->
    {reply,five_more,hiber_wakeup,Data,hibernate};
handle_sync_event('alive?', _From, State, Data) ->
    {reply, yes, State, Data};
handle_sync_event(stop, _From, _State, Data) ->
    {stop, normal, stopped, Data};
handle_sync_event(stop_shutdown, _From, _State, Data) ->
    {stop, shutdown, shutdown_stopped, Data};
handle_sync_event(stop_shutdown_reason, _From, _State, Data) ->
    {stop, {shutdown,reason}, {shutdown,reason}, Data};
handle_sync_event({get, _Pid}, _From, State, Data) ->
    {reply, {state, State, Data}, State, Data}.

format_status(terminate, [_Pdict, StateData]) ->
    StateData;
format_status(normal, [_Pdict, _StateData]) ->
    [format_status_called].
