%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(gen_fsm_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test cases
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).

-export([start1/1, start2/1, start3/1, start4/1, start5/1, start6/1,
	 start7/1, start8/1, start9/1, start10/1, start11/1, start12/1]).

-export([stop1/1, stop2/1, stop3/1, stop4/1, stop5/1, stop6/1, stop7/1,
	 stop8/1, stop9/1, stop10/1]).

-export([ abnormal1/1, abnormal2/1]).

-export([shutdown/1]).

-export([ sys1/1,
	  call_format_status/1, error_format_status/1, terminate_crash_format/1,
	  get_state/1, replace_state/1]).

-export([undef_handle_event/1, undef_handle_sync_event/1, undef_handle_info/1,
         undef_init/1, undef_code_change/1, undef_terminate1/1, undef_terminate2/1]).

-export([undef_in_handle_info/1, undef_in_terminate/1]).

-export([hibernate/1,auto_hibernate/1,hiber_idle/3,hiber_wakeup/3,hiber_idle/2,hiber_wakeup/2]).

-export([enter_loop/1]).

%% Exports for apply
-export([enter_loop/2]).

%% The gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, terminate/3,
	 handle_info/3, format_status/2, code_change/4]).
-export([idle/2,	idle/3,
	 timeout/2,
	 wfor_conf/2,	wfor_conf/3,
	 connected/2,	connected/3]).
-export([state0/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group, start}, {group, abnormal}, shutdown,
     {group, sys}, hibernate, auto_hibernate, enter_loop, {group, undef_callbacks},
     undef_in_handle_info, undef_in_terminate].

groups() ->
    [{start, [],
      [start1, start2, start3, start4, start5, start6, start7,
       start8, start9, start10, start11, start12]},
     {stop, [],
      [stop1, stop2, stop3, stop4, stop5, stop6, stop7, stop8, stop9, stop10]},
     {abnormal, [], [abnormal1, abnormal2]},
     {sys, [],
      [sys1, call_format_status, error_format_status, terminate_crash_format,
       get_state, replace_state]},
     {undef_callbacks, [],
      [undef_handle_event, undef_handle_sync_event, undef_handle_info,
       undef_init, undef_code_change, undef_terminate1, undef_terminate2]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(undef_callbacks, Config) ->
    DataDir = ?config(data_dir, Config),
    Server = filename:join(DataDir, "oc_fsm.erl"),
    {ok, oc_fsm} = compile:file(Server),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% anonymous
start1(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    {ok, Pid0} = gen_fsm:start_link(gen_fsm_SUITE, [], []),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stop_it(Pid0),
    %%    stopped = gen_fsm:sync_send_all_state_event(Pid0, stop),
    %%    {'EXIT', {timeout,_}} =
    %%	(catch gen_fsm:sync_send_event(Pid0, hej)),

    [] = get_messages(),
    %%process_flag(trap_exit, OldFl),
    ok.

%% anonymous w. shutdown
start2(Config) when is_list(Config) ->
    %% Dont link when shutdown
    {ok, Pid0} = gen_fsm:start(gen_fsm_SUITE, [], []),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    MRef = monitor(process,Pid0),
    shutdown_stopped =
	gen_fsm:sync_send_all_state_event(Pid0, stop_shutdown),
    receive {'DOWN',MRef,_,_,shutdown} -> ok end,
    {'EXIT', {noproc,_}} =
	(catch gen_fsm:sync_send_event(Pid0, hej)),

    [] = get_messages(),
    ok.

%% anonymous with timeout
start3(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    {ok, Pid0} = gen_fsm:start(gen_fsm_SUITE, [], [{timeout,5}]),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    stop_it(Pid0),

    {error, timeout} = gen_fsm:start(gen_fsm_SUITE, sleep,
				     [{timeout,5}]),

    [] = get_messages(),
    %%process_flag(trap_exit, OldFl),
    ok.

%% anonymous with ignore
start4(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    ignore = gen_fsm:start(gen_fsm_SUITE, ignore, []),

    [] = get_messages(),
    process_flag(trap_exit, OldFl),
    ok.

%% anonymous with stop
start5(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    {error, stopped} = gen_fsm:start(gen_fsm_SUITE, stop, []),

    [] = get_messages(),
    process_flag(trap_exit, OldFl),
    ok.

%% anonymous linked
start6(Config) when is_list(Config) ->
    {ok, Pid} = gen_fsm:start_link(gen_fsm_SUITE, [], []),
    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    stop_it(Pid),

    [] = get_messages(),

    ok.

%% global register linked
start7(Config) when is_list(Config) ->
    {ok, Pid} =
	gen_fsm:start_link({global, my_fsm}, gen_fsm_SUITE, [], []),
    {error, {already_started, Pid}} =
	gen_fsm:start_link({global, my_fsm}, gen_fsm_SUITE, [], []),
    {error, {already_started, Pid}} =
	gen_fsm:start({global, my_fsm}, gen_fsm_SUITE, [], []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test({global, my_fsm}),
    ok = do_sync_func_test({global, my_fsm}),
    stop_it({global, my_fsm}),

    [] = get_messages(),
    ok.


%% local register
start8(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    {ok, Pid} =
	gen_fsm:start({local, my_fsm}, gen_fsm_SUITE, [], []),
    {error, {already_started, Pid}} =
	gen_fsm:start({local, my_fsm}, gen_fsm_SUITE, [], []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(my_fsm),
    ok = do_sync_func_test(my_fsm),
    stop_it(Pid),

    [] = get_messages(),
    %%process_flag(trap_exit, OldFl),
    ok.

%% local register linked
start9(Config) when is_list(Config) ->
    %%OldFl = process_flag(trap_exit, true),

    {ok, Pid} =
	gen_fsm:start_link({local, my_fsm}, gen_fsm_SUITE, [], []),
    {error, {already_started, Pid}} =
	gen_fsm:start({local, my_fsm}, gen_fsm_SUITE, [], []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test(my_fsm),
    ok = do_sync_func_test(my_fsm),
    stop_it(Pid),

    [] = get_messages(),
    %%process_flag(trap_exit, OldFl),
    ok.

%% global register
start10(Config) when is_list(Config) ->
    {ok, Pid} =
	gen_fsm:start({global, my_fsm}, gen_fsm_SUITE, [], []),
    {error, {already_started, Pid}} =
	gen_fsm:start({global, my_fsm}, gen_fsm_SUITE, [], []),
    {error, {already_started, Pid}} =
	gen_fsm:start_link({global, my_fsm}, gen_fsm_SUITE, [], []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test({global, my_fsm}),
    ok = do_sync_func_test({global, my_fsm}),
    stop_it({global, my_fsm}),

    [] = get_messages(),
    ok.


%% Stop registered processes
start11(Config) when is_list(Config) ->
    {ok, Pid} =
	gen_fsm:start_link({local, my_fsm}, gen_fsm_SUITE, [], []),
    stop_it(Pid),

    {ok, _Pid1} =
	gen_fsm:start_link({local, my_fsm}, gen_fsm_SUITE, [], []),
    stop_it(my_fsm),

    {ok, Pid2} =
	gen_fsm:start({global, my_fsm}, gen_fsm_SUITE, [], []),
    stop_it(Pid2),
    receive after 1 -> true end,
    Result =
	gen_fsm:start({global, my_fsm}, gen_fsm_SUITE, [], []),
    io:format("Result = ~p~n",[Result]),
    {ok, _Pid3} = Result,
    stop_it({global, my_fsm}),

    [] = get_messages(),
    ok.

%% Via register linked
start12(Config) when is_list(Config) ->
    dummy_via:reset(),
    {ok, Pid} =
	gen_fsm:start_link({via, dummy_via, my_fsm}, gen_fsm_SUITE, [], []),
    {error, {already_started, Pid}} =
	gen_fsm:start_link({via, dummy_via, my_fsm}, gen_fsm_SUITE, [], []),
    {error, {already_started, Pid}} =
	gen_fsm:start({via, dummy_via, my_fsm}, gen_fsm_SUITE, [], []),

    ok = do_func_test(Pid),
    ok = do_sync_func_test(Pid),
    ok = do_func_test({via, dummy_via, my_fsm}),
    ok = do_sync_func_test({via, dummy_via, my_fsm}),
    stop_it({via, dummy_via, my_fsm}),

    [] = get_messages(),
    ok.


%% Anonymous, reason 'normal'
stop1(_Config) ->
    {ok, Pid} = gen_fsm:start(?MODULE, [], []),
    ok = gen_fsm:stop(Pid),
    false = erlang:is_process_alive(Pid),
    {'EXIT',noproc} = (catch gen_fsm:stop(Pid)),
    ok.

%% Anonymous, other reason
stop2(_Config) ->
    {ok,Pid} = gen_fsm:start(?MODULE, [], []),
    ok = gen_fsm:stop(Pid, other_reason, infinity),
    false = erlang:is_process_alive(Pid),
    ok.

%% Anonymous, invalid timeout
stop3(_Config) ->
    {ok,Pid} = gen_fsm:start(?MODULE, [], []),
    {'EXIT',_} = (catch gen_fsm:stop(Pid, other_reason, invalid_timeout)),
    true = erlang:is_process_alive(Pid),
    ok = gen_fsm:stop(Pid),
    false = erlang:is_process_alive(Pid),
    ok.

%% Registered name
stop4(_Config) ->
    {ok,Pid} = gen_fsm:start({local,to_stop},?MODULE, [], []),
    ok = gen_fsm:stop(to_stop),
    false = erlang:is_process_alive(Pid),
    {'EXIT',noproc} = (catch gen_fsm:stop(to_stop)),
    ok.

%% Registered name and local node
stop5(_Config) ->
    {ok,Pid} = gen_fsm:start({local,to_stop},?MODULE, [], []),
    ok = gen_fsm:stop({to_stop,node()}),
    false = erlang:is_process_alive(Pid),
    {'EXIT',noproc} = (catch gen_fsm:stop({to_stop,node()})),
    ok.

%% Globally registered name
stop6(_Config) ->
    {ok, Pid} = gen_fsm:start({global, to_stop}, ?MODULE, [], []),
    ok = gen_fsm:stop({global,to_stop}),
    false = erlang:is_process_alive(Pid),
    {'EXIT',noproc} = (catch gen_fsm:stop({global,to_stop})),
    ok.

%% 'via' registered name
stop7(_Config) ->
    dummy_via:reset(),
    {ok, Pid} = gen_fsm:start({via, dummy_via, to_stop},
			      ?MODULE, [], []),
    ok = gen_fsm:stop({via, dummy_via, to_stop}),
    false = erlang:is_process_alive(Pid),
    {'EXIT',noproc} = (catch gen_fsm:stop({via, dummy_via, to_stop})),
    ok.

%% Anonymous on remote node
stop8(_Config) ->
    {ok,Node} = test_server:start_node(gen_fsm_SUITE_stop8,slave,[]),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node,code,add_path,[Dir]),
    {ok, Pid} = rpc:call(Node,gen_fsm,start,[?MODULE,[],[]]),
    ok = gen_fsm:stop(Pid),
    false = rpc:call(Node,erlang,is_process_alive,[Pid]),
    {'EXIT',noproc} = (catch gen_fsm:stop(Pid)),
    true = test_server:stop_node(Node),
    {'EXIT',{{nodedown,Node},_}} = (catch gen_fsm:stop(Pid)),
    ok.

%% Registered name on remote node
stop9(_Config) ->
    {ok,Node} = test_server:start_node(gen_fsm_SUITE_stop9,slave,[]),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node,code,add_path,[Dir]),
    {ok, Pid} = rpc:call(Node,gen_fsm,start,[{local,to_stop},?MODULE,[],[]]),
    ok = gen_fsm:stop({to_stop,Node}),
    undefined = rpc:call(Node,erlang,whereis,[to_stop]),
    false = rpc:call(Node,erlang,is_process_alive,[Pid]),
    {'EXIT',noproc} = (catch gen_fsm:stop({to_stop,Node})),
    true = test_server:stop_node(Node),
    {'EXIT',{{nodedown,Node},_}} = (catch gen_fsm:stop({to_stop,Node})),
    ok.

%% Globally registered name on remote node
stop10(_Config) ->
    {ok,Node} = test_server:start_node(gen_fsm_SUITE_stop10,slave,[]),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node,code,add_path,[Dir]),
    {ok, Pid} = rpc:call(Node,gen_fsm,start,[{global,to_stop},?MODULE,[],[]]),
    ok = global:sync(),
    ok = gen_fsm:stop({global,to_stop}),
    false = rpc:call(Node,erlang,is_process_alive,[Pid]),
    {'EXIT',noproc} = (catch gen_fsm:stop({global,to_stop})),
    true = test_server:stop_node(Node),
    {'EXIT',noproc} = (catch gen_fsm:stop({global,to_stop})),
    ok.

%% Check that time outs in calls work
abnormal1(Config) when is_list(Config) ->
    {ok, _Pid} = gen_fsm:start({local, my_fsm}, gen_fsm_SUITE, [], []),

    %% timeout call.
    delayed = gen_fsm:sync_send_event(my_fsm, {delayed_answer,1}, 100),
    {'EXIT',{timeout,_}} =
	(catch gen_fsm:sync_send_event(my_fsm, {delayed_answer,10}, 1)),
    receive
	Msg ->
	    %% Ignore the delayed answer from the server.
	    io:format("Delayed message: ~p", [Msg])
    end,

    [] = get_messages(),
    ok.

%% Check that bad return values makes the fsm crash. Note that we must
%% trap exit since we must link to get the real bad_return_ error
abnormal2(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),
    {ok, Pid} =
	gen_fsm:start_link(gen_fsm_SUITE, [], []),

    %% bad return value in the gen_fsm loop
    {'EXIT',{{bad_return_value, badreturn},_}} =
	(catch gen_fsm:sync_send_event(Pid, badreturn)),

    [{'EXIT',Pid,{bad_return_value,badreturn}}] = get_messages(),
    process_flag(trap_exit, OldFl),
    ok.

shutdown(Config) when is_list(Config) ->
    error_logger_forwarder:register(),

    process_flag(trap_exit, true),

    {ok,Pid0} = gen_fsm:start_link(gen_fsm_SUITE, [], []),
    ok = do_func_test(Pid0),
    ok = do_sync_func_test(Pid0),
    {shutdown,reason} =
	gen_fsm:sync_send_all_state_event(Pid0, stop_shutdown_reason),
    receive {'EXIT',Pid0,{shutdown,reason}} -> ok end,
    process_flag(trap_exit, false),

    {'EXIT', {noproc,_}} =
	(catch gen_fsm:sync_send_event(Pid0, hej)),

    receive
	Any ->
	    io:format("Unexpected: ~p", [Any]),
	    ct:fail(failed)
    after 500 ->
	    ok
    end,

    ok.



sys1(Config) when is_list(Config) ->
    {ok, Pid} =
	gen_fsm:start(gen_fsm_SUITE, [], []),
    {status, Pid, {module,gen_fsm}, _} = sys:get_status(Pid),
    sys:suspend(Pid),
    {'EXIT', {timeout,_}} =
	(catch gen_fsm:sync_send_event(Pid, hej)),
    sys:resume(Pid),
    stop_it(Pid).

call_format_status(Config) when is_list(Config) ->
    {ok, Pid} = gen_fsm:start(gen_fsm_SUITE, [], []),
    Status = sys:get_status(Pid),
    {status, Pid, _Mod, [_PDict, running, _, _, Data]} = Status,
    [format_status_called | _] = lists:reverse(Data),
    stop_it(Pid),

    %% check that format_status can handle a name being an atom (pid is
    %% already checked by the previous test)
    {ok, Pid2} = gen_fsm:start({local, gfsm}, gen_fsm_SUITE, [], []),
    Status2 = sys:get_status(gfsm),
    {status, Pid2, _Mod, [_PDict2, running, _, _, Data2]} = Status2,
    [format_status_called | _] = lists:reverse(Data2),
    stop_it(Pid2),

    %% check that format_status can handle a name being a term other than a
    %% pid or atom
    GlobalName1 = {global, "CallFormatStatus"},
    {ok, Pid3} = gen_fsm:start(GlobalName1, gen_fsm_SUITE, [], []),
    Status3 = sys:get_status(GlobalName1),
    {status, Pid3, _Mod, [_PDict3, running, _, _, Data3]} = Status3,
    [format_status_called | _] = lists:reverse(Data3),
    stop_it(Pid3),
    GlobalName2 = {global, {name, "term"}},
    {ok, Pid4} = gen_fsm:start(GlobalName2, gen_fsm_SUITE, [], []),
    Status4 = sys:get_status(GlobalName2),
    {status, Pid4, _Mod, [_PDict4, running, _, _, Data4]} = Status4,
    [format_status_called | _] = lists:reverse(Data4),
    stop_it(Pid4),

    %% check that format_status can handle a name being a term other than a
    %% pid or atom
    dummy_via:reset(),
    ViaName1 = {via, dummy_via, "CallFormatStatus"},
    {ok, Pid5} = gen_fsm:start(ViaName1, gen_fsm_SUITE, [], []),
    Status5 = sys:get_status(ViaName1),
    {status, Pid5, _Mod, [_PDict5, running, _, _, Data5]} = Status5,
    [format_status_called | _] = lists:reverse(Data5),
    stop_it(Pid5),
    ViaName2 = {via, dummy_via, {name, "term"}},
    {ok, Pid6} = gen_fsm:start(ViaName2, gen_fsm_SUITE, [], []),
    Status6 = sys:get_status(ViaName2),
    {status, Pid6, _Mod, [_PDict6, running, _, _, Data6]} = Status6,
    [format_status_called | _] = lists:reverse(Data6),
    stop_it(Pid6).



error_format_status(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    StateData = "called format_status",
    Parent = self(),
    {ok, Pid} = gen_fsm:start(gen_fsm_SUITE, {state_data, StateData}, []),
    %% bad return value in the gen_fsm loop
    {'EXIT',{{bad_return_value, badreturn},_}} =
	(catch gen_fsm:sync_send_event(Pid, badreturn)),
    receive
	{error,_GroupLeader,{Pid,
			     "** State machine "++_,
			     [Pid,badreturn,Parent,idle,{formatted,StateData},
                              {bad_return_value,badreturn}|_]}} ->
	    ok;
	Other ->
	    io:format("Unexpected: ~p", [Other]),
	    ct:fail(failed)
    end,
    process_flag(trap_exit, OldFl),
    ok.

terminate_crash_format(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    StateData = crash_terminate,
    Parent = self(),
    {ok, Pid} = gen_fsm:start(gen_fsm_SUITE, {state_data, StateData}, []),
    stop_it(Pid),
    receive
	{error,_GroupLeader,{Pid,
			     "** State machine "++_,
			     [Pid,stop,Parent,idle,{formatted, StateData},
                              {crash,terminate}|_]}} ->
	    ok;
	Other ->
	    io:format("Unexpected: ~p", [Other]),
	    ct:fail(failed)
    after 5000 ->
	    io:format("Timeout: expected error logger msg", []),
	    ct:fail(failed)
    end,
    process_flag(trap_exit, OldFl),
    ok.


get_state(Config) when is_list(Config) ->
    State = self(),
    {ok, Pid} = gen_fsm:start(?MODULE, {state_data, State}, []),
    {idle, State} = sys:get_state(Pid),
    {idle, State} = sys:get_state(Pid, 5000),
    stop_it(Pid),

    %% check that get_state can handle a name being an atom (pid is
    %% already checked by the previous test)
    {ok, Pid2} = gen_fsm:start({local, gfsm}, gen_fsm_SUITE, {state_data, State}, []),
    {idle, State} = sys:get_state(gfsm),
    {idle, State} = sys:get_state(gfsm, 5000),
    stop_it(Pid2),

    %% check that get_state works when pid is sys suspended
    {ok, Pid3} = gen_fsm:start(gen_fsm_SUITE, {state_data, State}, []),
    {idle, State} = sys:get_state(Pid3),
    ok = sys:suspend(Pid3),
    {idle, State} = sys:get_state(Pid3, 5000),
    ok = sys:resume(Pid3),
    stop_it(Pid3),
    ok.

replace_state(Config) when is_list(Config) ->
    State = self(),
    {ok, Pid} = gen_fsm:start(?MODULE, {state_data, State}, []),
    {idle, State} = sys:get_state(Pid),
    NState1 = "replaced",
    Replace1 = fun({StateName, _}) -> {StateName, NState1} end,
    {idle, NState1} = sys:replace_state(Pid, Replace1),
    {idle, NState1} = sys:get_state(Pid),
    NState2 = "replaced again",
    Replace2 = fun({idle, _}) -> {state0, NState2} end,
    {state0, NState2} = sys:replace_state(Pid, Replace2, 5000),
    {state0, NState2} = sys:get_state(Pid),
    %% verify no change in state if replace function crashes
    Replace3 = fun(_) -> error(fail) end,
    {'EXIT',{{callback_failed,
	      {gen_fsm,system_replace_state},{error,fail}},_}} =
	(catch sys:replace_state(Pid, Replace3)),
    {state0, NState2} = sys:get_state(Pid),
    %% verify state replaced if process sys suspended
    ok = sys:suspend(Pid),
    Suffix2 = " and again",
    NState3 = NState2 ++ Suffix2,
    Replace4 = fun({StateName, _}) -> {StateName, NState3} end,
    {state0, NState3} = sys:replace_state(Pid, Replace4),
    ok = sys:resume(Pid),
    {state0, NState3} = sys:get_state(Pid, 5000),
    stop_it(Pid),
    ok.

%% Hibernation
hibernate(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    {ok, Pid0} = gen_fsm:start_link(?MODULE, hiber_now, []),
    is_in_erlang_hibernate(Pid0),
    stop_it(Pid0),
    receive
	{'EXIT',Pid0,normal} -> ok
    end,

    {ok, Pid} = gen_fsm:start_link(?MODULE, hiber, []),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid,current_function)),
    hibernating = gen_fsm:sync_send_event(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_fsm:sync_send_event(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    hibernating = gen_fsm:sync_send_event(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    five_more = gen_fsm:sync_send_event(Pid, snooze_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_fsm:sync_send_event(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_fsm:send_event(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_fsm:send_event(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_fsm:send_event(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_fsm:send_event(Pid, snooze_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_fsm:send_event(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),

    Pid ! hibernate_later,
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid, current_function)),
    is_in_erlang_hibernate(Pid),

    'alive!' = gen_fsm:sync_send_event(Pid,'alive?'),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid, current_function)),
    Pid ! hibernate_now,
    is_in_erlang_hibernate(Pid),

    'alive!' = gen_fsm:sync_send_event(Pid,'alive?'),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid, current_function)),

    hibernating = gen_fsm:sync_send_all_state_event(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_fsm:sync_send_all_state_event(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    hibernating = gen_fsm:sync_send_all_state_event(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    five_more = gen_fsm:sync_send_all_state_event(Pid, snooze_sync),
    is_in_erlang_hibernate(Pid),
    good_morning = gen_fsm:sync_send_all_state_event(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_fsm:send_all_state_event(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok  = gen_fsm:send_all_state_event(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),
    ok = gen_fsm:send_all_state_event(Pid, hibernate_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_fsm:send_all_state_event(Pid, snooze_async),
    is_in_erlang_hibernate(Pid),
    ok = gen_fsm:send_all_state_event(Pid, wakeup_async),
    is_not_in_erlang_hibernate(Pid),

    hibernating = gen_fsm:sync_send_all_state_event(Pid, hibernate_sync),
    is_in_erlang_hibernate(Pid),
    sys:suspend(Pid),
    is_in_erlang_hibernate(Pid),
    sys:resume(Pid),
    is_in_erlang_hibernate(Pid),
    receive after 1000 -> ok end,
    is_in_erlang_hibernate(Pid),

    good_morning  = gen_fsm:sync_send_all_state_event(Pid, wakeup_sync),
    is_not_in_erlang_hibernate(Pid),
    stop_it(Pid),
    receive
	{'EXIT',Pid,normal} -> ok
    end,

    [] = get_messages(),
    process_flag(trap_exit, OldFl),
    ok.

%% Auto hibernation
auto_hibernate(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),
    HibernateAfterTimeout = 100,
    State = {auto_hibernate_state},
    {ok, Pid} = gen_fsm:start_link({local, my_test_name_auto_hibernate}, ?MODULE, {state_data, State}, [{hibernate_after, HibernateAfterTimeout}]),
    %% After init test
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),
    %% Get state test
    {_, State} = sys:get_state(my_test_name_auto_hibernate),
    is_in_erlang_hibernate(Pid),
    %% Sync send event test
    'alive!' = gen_fsm:sync_send_event(Pid,'alive?'),
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),
    %% Send event test
    ok = gen_fsm:send_all_state_event(Pid,{'alive?', self()}),
    wfor(yes),
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),
    %% Info test
    Pid ! {self(), handle_info},
    wfor({Pid, handled_info}),
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),
    stop_it(Pid),
    receive
        {'EXIT',Pid,normal} -> ok
    end,
    process_flag(trap_exit, OldFl),
    ok.

is_in_erlang_hibernate(Pid) ->
    receive after 1 -> ok end,
    is_in_erlang_hibernate_1(200, Pid).

is_in_erlang_hibernate_1(0, Pid) ->
    io:format("~p\n", [erlang:process_info(Pid, current_function)]),
    ct:fail(not_in_erlang_hibernate_3);
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
    ct:fail(not_in_erlang_hibernate_3);
is_not_in_erlang_hibernate_1(N, Pid) ->
    {current_function,MFA} = erlang:process_info(Pid, current_function),
    case MFA of
	{erlang,hibernate,3} ->
	    receive after 10 -> ok end,
	    is_not_in_erlang_hibernate_1(N-1, Pid);
	_ ->
	    ok
    end.

%% Test gen_fsm:enter_loop/4,5,6.
enter_loop(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),

    dummy_via:reset(),

    %% Locally registered process + {local, Name}
    {ok, Pid1a} =
	proc_lib:start_link(?MODULE, enter_loop, [local, local]),
    yes = gen_fsm:sync_send_event(Pid1a, 'alive?'),
    stopped = gen_fsm:sync_send_event(Pid1a, stop),
    receive
	{'EXIT', Pid1a, normal} ->
	    ok
    after 5000 ->
	    ct:fail(gen_fsm_did_not_die)
    end,

    %% Unregistered process + {local, Name}
    {ok, Pid1b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, local]),
    receive
	{'EXIT', Pid1b, process_not_registered} ->
	    ok
    after 5000 ->
	    ct:fail(gen_fsm_did_not_die)
    end,

    %% Globally registered process + {global, Name}
    {ok, Pid2a} =
	proc_lib:start_link(?MODULE, enter_loop, [global, global]),
    yes = gen_fsm:sync_send_event(Pid2a, 'alive?'),
    stopped = gen_fsm:sync_send_event(Pid2a, stop),
    receive
	{'EXIT', Pid2a, normal} ->
	    ok
    after 5000 ->
	    ct:fail(gen_fsm_did_not_die)
    end,

    %% Unregistered process + {global, Name}
    {ok, Pid2b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, global]),
    receive
	{'EXIT', Pid2b, process_not_registered_globally} ->
	    ok
    after 5000 ->
	    ct:fail(gen_fsm_did_not_die)
    end,

    %% Unregistered process + no name
    {ok, Pid3} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, anon]),
    yes = gen_fsm:sync_send_event(Pid3, 'alive?'),
    stopped = gen_fsm:sync_send_event(Pid3, stop),
    receive
	{'EXIT', Pid3, normal} ->
	    ok
    after 5000 ->
	    ct:fail(gen_fsm_did_not_die)
    end,

    %% Process not started using proc_lib
    Pid4 =
	spawn_link(gen_fsm, enter_loop, [?MODULE, [], state0, []]),
    receive
	{'EXIT', Pid4, process_was_not_started_by_proc_lib} ->
	    ok
    after 5000 ->
	    ct:fail(gen_fsm_did_not_die)
    end,

    %% Make sure I am the parent, ie that ordering a shutdown will
    %% result in the process terminating with Reason==shutdown
    {ok, Pid5} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, anon]),
    yes = gen_fsm:sync_send_event(Pid5, 'alive?'),
    exit(Pid5, shutdown),
    receive
	{'EXIT', Pid5, shutdown} ->
	    ok
    after 5000 ->
	    ct:fail(gen_fsm_did_not_die)
    end,

    %% Make sure gen_fsm:enter_loop does not accept {local,Name}
    %% when it's another process than the calling one which is
    %% registered under that name
    register(armitage, self()),
    {ok, Pid6a} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, local]),
    receive
	{'EXIT', Pid6a, process_not_registered} ->
	    ok
    after 1000 ->
	    ct:fail(gen_fsm_started)
    end,
    unregister(armitage),

    %% Make sure gen_fsm:enter_loop does not accept {global,Name}
    %% when it's another process than the calling one which is
    %% registered under that name
    global:register_name(armitage, self()),
    {ok, Pid6b} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, global]),
    receive
	{'EXIT', Pid6b, process_not_registered_globally} ->
	    ok
    after 1000 ->
	    ct:fail(gen_fsm_started)
    end,
    global:unregister_name(armitage),

    dummy_via:register_name(armitage, self()),
    {ok, Pid6c} =
	proc_lib:start_link(?MODULE, enter_loop, [anon, via]),
    receive
	{'EXIT', Pid6c, {process_not_registered_via, dummy_via}} ->
	    ok
    after 1000 ->
	    ct:fail({gen_fsm_started, process_info(self(), messages)})
    end,
    dummy_via:unregister_name(armitage),

    process_flag(trap_exit, OldFlag),
    ok.

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
	    gen_fsm:enter_loop(?MODULE, [], state0, [], {local,armitage});
	global ->
	    gen_fsm:enter_loop(?MODULE, [], state0, [], {global,armitage});
	via ->
	    gen_fsm:enter_loop(?MODULE, [], state0, [],
			       {via, dummy_via, armitage});
	anon ->
	    gen_fsm:enter_loop(?MODULE, [], state0, [])
    end.

%% Start should return an undef error if init isn't implemented
undef_init(Config) when is_list(Config) ->
    {error, {undef, [{oc_init_fsm, init, [[]], []}|_]}}
        =  gen_fsm:start(oc_init_fsm, [], []),
    ok.

%% Test that the server crashes correctly if the handle_event callback is
%% not exported in the callback module
undef_handle_event(Config) when is_list(Config) ->
    {ok, FSM} = gen_fsm:start(oc_fsm, [], []),
    MRef = monitor(process, FSM),
    gen_fsm:send_all_state_event(FSM, state_name),
    ok = verify_undef_down(MRef, FSM, oc_fsm, handle_event).

%% Test that the server crashes correctly if the handle_sync_event callback is
%% not exported in the callback module
undef_handle_sync_event(Config) when is_list(Config) ->
    {ok, FSM} = gen_fsm:start(oc_fsm, [], []),
    try
        gen_fsm:sync_send_all_state_event(FSM, state_name),
        ct:fail(should_crash)
    catch exit:{{undef, [{oc_fsm, handle_sync_event, _, _}|_]},_} ->
        ok
    end.

%% The fsm should log but not crash if the handle_info callback is
%% calling an undefined function
undef_handle_info(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    {ok, FSM} = gen_fsm:start(oc_fsm, [], []),
    MRef = monitor(process, FSM),
    FSM ! hej,
    receive
        {'DOWN', MRef, process, FSM, _} ->
            ct:fail(should_not_crash)
    after 500 ->
        ok
    end,
    receive
        {warning_msg, _GroupLeader,
         {FSM, "** Undefined handle_info in " ++ _, [oc_fsm, hej]}} ->
            ok;
        Other ->
            io:format("Unexpected: ~p", [Other]),
            ct:fail(failed)
    end.

%% The upgrade should fail if code_change is expected in the callback module
%% but not exported, but the fsm should continue with the old code
undef_code_change(Config) when is_list(Config) ->
    {ok, FSM} = gen_fsm:start(oc_fsm, [], []),
    {error, {'EXIT', {undef, [{oc_fsm, code_change, [_, _, _, _], _}|_]}}}
        = fake_upgrade(FSM, oc_fsm),
    ok.

%% Test the default implementation of terminate with normal reason if the
%% callback module does not export it
undef_terminate1(Config) when is_list(Config) ->
    {ok, FSM} = gen_fsm:start(oc_fsm, [], []),
    MRef = monitor(process, FSM),
    ok = gen_fsm:stop(FSM),
    ok = verify_down_reason(MRef, FSM, normal).

%% Test the default implementation of terminate with error reason if the
%% callback module does not export it
undef_terminate2(Config) when is_list(Config) ->
    {ok, FSM} = gen_fsm:start(oc_fsm, [], []),
    MRef = monitor(process, FSM),
    ok = gen_fsm:stop(FSM, {error, test}, infinity),
    ok = verify_down_reason(MRef, FSM, {error, test}).

%% Test that the server crashes correctly if the handle_info callback is
%% calling an undefined function
undef_in_handle_info(Config) when is_list(Config) ->
    {ok, FSM} = gen_fsm:start(?MODULE, [], []),
    MRef = monitor(process, FSM),
    FSM ! {call_undef_fun, {?MODULE, handle_info}},
    verify_undef_down(MRef, FSM, ?MODULE, handle_info),
    ok.

%% Test that the server crashes correctly if the terminate callback is
%% calling an undefined function
undef_in_terminate(Config) when is_list(Config) ->
    State = {undef_in_terminate, {?MODULE, terminate}},
    {ok, FSM} = gen_fsm:start(?MODULE, {state_data, State}, []),
    try
        ok = gen_fsm:stop(FSM),
        ct:fail(failed)
    catch
        exit:{undef, [{?MODULE, terminate, _, _}|_]} ->
            ok
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
    stopped = gen_fsm:sync_send_all_state_event(FSM, stop),
    {'EXIT',_} = 	(catch gen_fsm:sync_send_event(FSM, hej)),
    ok.



do_func_test(FSM) ->
    ok = gen_fsm:send_all_state_event(FSM, {'alive?', self()}),
    wfor(yes),
    ok = do_connect(FSM),
    ok = gen_fsm:send_all_state_event(FSM, {'alive?', self()}),
    wfor(yes),
    _ = [do_msg(FSM) || _ <- lists:seq(1, 3)],
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
    _ = [do_sync_msg(FSM) || _ <- lists:seq(1, 3)],
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

verify_down_reason(MRef, Pid, Reason) ->
    receive
        {'DOWN', MRef, process, Pid, Reason} ->
            ok;
        {'DOWN', MRef, process, Pid, Other}->
            ct:fail({wrong_down_reason, Other})
    after 5000 ->
        ct:fail(should_shutdown)
    end.

verify_undef_down(MRef, Pid, Mod, Fun) ->
    ok = receive
        {'DOWN', MRef, process, Pid,
         {undef, [{Mod, Fun, _, _}|_]}} ->
            ok
    after 5000 ->
        ct:fail(should_crash)
    end.

fake_upgrade(Pid, Mod) ->
    sys:suspend(Pid),
    sys:replace_state(Pid, fun(State) -> {new, State} end),
    Ret = sys:change_code(Pid, Mod, old_vsn, []),
    ok = sys:resume(Pid),
    Ret.

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
    timer:sleep(1000),
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

terminate(_, _State, crash_terminate) ->
    exit({crash, terminate});
terminate(_, _, {undef_in_terminate, {Mod, Fun}}) ->
    Mod:Fun(),
    ok;
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
    timer:sleep(T),
    {reply, delayed, idle, Data};
idle(badreturn, _From, _Data) ->
    badreturn;
idle({timeout,Time}, From, _Data) ->
    gen_fsm:send_event_after(Time, {timeout,Time}),
    {next_state, timeout, From};
idle('alive?', _From, Data) ->
    {reply, 'alive!', idle, Data};
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
    _= gen_fsm:cancel_timer(Cref),
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
hiber_idle(timeout, hibernate_me) ->
    %% Arrive here from handle_info(hibernate_later,...)
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


handle_info(hibernate_now, _SName, _State) ->
    %% Arrive here from by direct ! from testcase
    {next_state, hiber_idle, [], hibernate};
handle_info(hibernate_later, _SName, _State) ->
    {next_state, hiber_idle, hibernate_me, 1000};
handle_info({call_undef_fun, {Mod, Fun}}, State, Data) ->
    Mod:Fun(),
    {next_state, State, Data};
handle_info({From, handle_info}, SName, State) ->
    From ! {self(), handled_info},
    {next_state, SName, State};
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
    {formatted, StateData};
format_status(normal, [_Pdict, _StateData]) ->
    [format_status_called].

code_change(_OldVsn, State,
            {idle, {undef_in_code_change, {Mod, Fun}}} = Data, _Extra) ->
    Mod:Fun(),
    {ok, State, Data};
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

get_messages() ->
    receive
	Msg -> [Msg|get_messages()]
    after 1 -> []
    end.
