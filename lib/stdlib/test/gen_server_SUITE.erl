%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-module(gen_server_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").

-export([init_per_testcase/2, end_per_testcase/2]).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([start/1, start_event_timeout/1, start_event_timeout_zero/1,
	 crash/1, loop_start_fail/1, call/1, send_request/1,
         send_request_receive_reqid_collection/1,
         send_request_wait_reqid_collection/1,
         send_request_check_reqid_collection/1,
         cast/1, cast_fast/1, continue/1, info/1, abcast/1,
	 handle_event_timeout/1, handle_event_timeout_zero/1,
	 handle_event_timeout_plain/1,
         multicall/1, multicall_down/1, multicall_remote/1,
         multicall_remote_old1/1, multicall_remote_old2/1,
         multicall_recv_opt_success/1,
         multicall_recv_opt_timeout/1,
         multicall_recv_opt_noconnection/1,
	 call_remote1/1, call_remote2/1, call_remote3/1, calling_self/1,
	 call_remote_n1/1, call_remote_n2/1, call_remote_n3/1, spec_init/1,
	 spec_init_local_registered_parent/1, 
	 spec_init_global_registered_parent/1,
	 otp_5854/1, hibernate/1, auto_hibernate/1, otp_7669/1, call_format_status/1,
	 error_format_status/1, terminate_crash_format/1, crash_in_format_status/1,
         throw_in_format_status/1, format_all_status/1,
	 get_state/1, replace_state/1, call_with_huge_message_queue/1,
	 undef_handle_call/1, undef_handle_cast/1, undef_handle_info/1,
	 undef_init/1, undef_code_change/1, undef_terminate1/1,
	 undef_terminate2/1, undef_in_terminate/1, undef_in_handle_info/1,
	 undef_handle_continue/1,

         format_log_1/1, format_log_2/1, format_log_with_process_label/1,
         reply_by_alias_with_payload/1
	]).

-export([stop1/1, stop2/1, stop3/1, stop4/1, stop5/1, stop6/1, stop7/1,
	 stop8/1, stop9/1, stop10/1]).

%% spawn export
-export([spec_init_local/2, spec_init_global/2, spec_init_via/2,
	 spec_init_default_timeout/2, spec_init_global_default_timeout/2,
         spec_init_anonymous/1,
	 spec_init_anonymous_default_timeout/1,
	 spec_init_not_proc_lib/1,
	 spec_init_action/2,
	 cast_fast_messup/0]).

%% Internal test specific exports
-export([multicall_srv_ctrlr/2, multicall_suspender/2]).

%% The gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
	 handle_info/2, code_change/3, terminate/2, format_status/2]).

%% This module needs to compile on old nodes...
-ifndef(CT_PEER).
-define(CT_PEER(), {ok, undefined, undefined}).
-define(CT_PEER(Opts), {ok, undefined, undefined}).
-endif.
-ifndef(CT_PEER_REL).
-define(CT_PEER_REL(Opts, Release, PrivDir), {ok, undefined, undefined}).
-endif.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [start, start_event_timeout, start_event_timeout_zero,
     {group,stop}, crash, loop_start_fail, call, send_request,
     send_request_receive_reqid_collection, send_request_wait_reqid_collection,
     send_request_check_reqid_collection, cast, cast_fast, info, abcast,
     continue, handle_event_timeout, handle_event_timeout_zero,
     handle_event_timeout_plain,
     {group, multi_call},
     call_remote1, call_remote2, calling_self,
     call_remote3, call_remote_n1, call_remote_n2,
     call_remote_n3, spec_init,
     spec_init_local_registered_parent,
     spec_init_global_registered_parent, otp_5854, hibernate, auto_hibernate,
     otp_7669,
     {group, format_status},
     get_state, replace_state,
     call_with_huge_message_queue, {group, undef_callbacks},
     undef_in_terminate, undef_in_handle_info,
     format_log_1, format_log_2, format_log_with_process_label,
     reply_by_alias_with_payload].

groups() -> 
    [{stop, [],
      [stop1, stop2, stop3, stop4, stop5, stop6, stop7, stop8, stop9, stop10]},
     {multi_call, [], [{group, multi_call_parallel}, {group, multi_call_sequence}]},
     {multi_call_parallel, [parallel],
      [multicall, multicall_down, multicall_remote, multicall_remote_old1,
       multicall_remote_old2]},
     {multi_call_sequence, [],
      [multicall_recv_opt_success, multicall_recv_opt_timeout,
       multicall_recv_opt_noconnection]},
     {format_status, [],
      [call_format_status, error_format_status, terminate_crash_format,
       crash_in_format_status, throw_in_format_status, format_all_status]},
     {undef_callbacks, [],
      [undef_handle_call, undef_handle_cast, undef_handle_info, undef_handle_continue,
       undef_init, undef_code_change, undef_terminate1, undef_terminate2]}].


init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    Server = filename:join(DataDir, "format_status_server.erl"),
    {ok, format_status_server} = compile:file(Server),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(undef_callbacks, Config) ->
    DataDir = ?config(data_dir, Config),
    Server = filename:join(DataDir, "oc_server.erl"),
    {ok, oc_server} = compile:file(Server),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Case, Config) when Case == call_remote1;
				     Case == call_remote2;
				     Case == call_remote3;
				     Case == call_remote_n1;
				     Case == call_remote_n2;
				     Case == call_remote_n3;
                                     Case == send_request ->
    {ok,Peer,Node} = ?CT_PEER(),
    ok = global:sync(),
    [{node,Node}, {peer, Peer} | Config];

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    case proplists:get_value(peer, Config) of
	undefined ->
	    ok;
	Peer ->
	    try peer:stop(Peer)
            catch exit : noproc ->
                    ok
            end
    end,
    ok.


%% --------------------------------------
%% Start and stop a gen_server.
%% --------------------------------------

start(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    %% anonymous
    io:format("anonymous~n", []),
    {ok, Pid0} = gen_server:start(gen_server_SUITE, [], []),
    ok = gen_server:call(Pid0, started_p),
    ok = gen_server:call(Pid0, stop),
    busy_wait_for_process(Pid0,600),
    {'EXIT', {noproc,_}} = (catch gen_server:call(Pid0, started_p, 1)),

    %% anonymous with timeout
    io:format("try init timeout~n", []),
    {ok, Pid00} = gen_server:start(gen_server_SUITE, [],
				   [{timeout,1000}]),
    ok = gen_server:call(Pid00, started_p),
    ok = gen_server:call(Pid00, stop),
    {error, timeout} = gen_server:start(gen_server_SUITE, sleep,
					[{timeout,100}]),

    %% anonymous with ignore
    io:format("try init ignore~n", []),
    ignore = gen_server:start(gen_server_SUITE, ignore, []),

    %% anonymous with shutdown
    io:format("try init shutdown~n", []),
    {error, foobar} =
        gen_server:start(gen_server_SUITE, {error, foobar}, []),

    %% anonymous with stop
    io:format("try init stop~n", []),
    {error, stopped} = gen_server:start(gen_server_SUITE, stop, []),

    %% anonymous linked
    {ok, Pid1} =
	gen_server:start_link(gen_server_SUITE, [], []),
    ok = gen_server:call(Pid1, started_p),
    ok = gen_server:call(Pid1, stop),
    receive
	{'EXIT', Pid1, stopped} ->
	    ok
    after 5000 ->
	    ct:fail(not_stopped)
    end,

    %% anonymous monitored
    {ok, {Pid1b, Mon1b}} =
	gen_server:start_monitor(gen_server_SUITE, [], []),
    ok = gen_server:call(Pid1b, started_p),
    ok = gen_server:call(Pid1b, stop),
    receive
	{'DOWN', Mon1b, process, Pid1b, stopped} ->
	    ok
    after 5000 ->
	    ct:fail(not_stopped)
    end,

    %% local register
    {ok, Pid2} =
	gen_server:start({local, my_test_name},
			 gen_server_SUITE, [], []),
    ok = gen_server:call(my_test_name, started_p),
    {error, {already_started, Pid2}} =
	gen_server:start({local, my_test_name},
			 gen_server_SUITE, [], []),
    ok = gen_server:call(my_test_name, stop),

    busy_wait_for_process(Pid2,600),

    {'EXIT', {noproc,_}} = (catch gen_server:call(Pid2, started_p, 10)),

    %% local register linked
    {ok, Pid3} =
	gen_server:start_link({local, my_test_name},
			      gen_server_SUITE, [], []), 
    ok = gen_server:call(my_test_name, started_p),
    {error, {already_started, Pid3}} =
	gen_server:start({local, my_test_name},
			 gen_server_SUITE, [], []),
    ok = gen_server:call(my_test_name, stop),
    receive
	{'EXIT', Pid3, stopped} ->
	    ok
    after 5000 ->
	    ct:fail(not_stopped)
    end,

    %% local register monitored
    {ok, {Pid3b, Mon3b}} =
	gen_server:start_monitor({local, my_test_name},
                                 gen_server_SUITE, [], []), 
    ok = gen_server:call(my_test_name, started_p),
    {error, {already_started, Pid3b}} =
	gen_server:start_monitor({local, my_test_name},
                                 gen_server_SUITE, [], []),
    ok = gen_server:call(my_test_name, stop),
    receive
	{'DOWN', Mon3b, process, Pid3b, stopped} ->
	    ok
    after 5000 ->
	    ct:fail(not_stopped)
    end,

    %% global register
    {ok, Pid4} =
	gen_server:start({global, my_test_name},
			 gen_server_SUITE, [], []),
    ok = gen_server:call({global, my_test_name}, started_p),
    {error, {already_started, Pid4}} =
	gen_server:start({global, my_test_name},
			 gen_server_SUITE, [], []),
    ok = gen_server:call({global, my_test_name}, stop),
    busy_wait_for_process(Pid4,600),
    {'EXIT', {noproc,_}} = (catch gen_server:call(Pid4, started_p, 10)),

    %% global register linked
    {ok, Pid5} =
	gen_server:start_link({global, my_test_name},
			      gen_server_SUITE, [], []), 
    ok = gen_server:call({global, my_test_name}, started_p),
    {error, {already_started, Pid5}} =
	gen_server:start({global, my_test_name},
			 gen_server_SUITE, [], []),
    ok = gen_server:call({global, my_test_name}, stop),
    receive
	{'EXIT', Pid5, stopped} ->
	    ok
    after 5000 ->
	    ct:fail(not_stopped)
    end,

    %% global register monitored
    {ok, {Pid5b, Mon5b}} =
	gen_server:start_monitor({global, my_test_name},
                                 gen_server_SUITE, [], []), 
    ok = gen_server:call({global, my_test_name}, started_p),
    {error, {already_started, Pid5b}} =
	gen_server:start_monitor({global, my_test_name},
                                 gen_server_SUITE, [], []),
    ok = gen_server:call({global, my_test_name}, stop),
    receive
	{'DOWN', Mon5b, process, Pid5b, stopped} ->
	    ok
    after 5000 ->
	    ct:fail(not_stopped)
    end,

    %% via register
    dummy_via:reset(),
    {ok, Pid6} =
	gen_server:start({via, dummy_via, my_test_name},
			 gen_server_SUITE, [], []),
    ok = gen_server:call({via, dummy_via, my_test_name}, started_p),
    {error, {already_started, Pid6}} =
	gen_server:start({via, dummy_via, my_test_name},
			 gen_server_SUITE, [], []),
    ok = gen_server:call({via, dummy_via, my_test_name}, stop),
    busy_wait_for_process(Pid6,600),
    {'EXIT', {noproc,_}} = (catch gen_server:call(Pid6, started_p, 10)),

    %% via register linked
    dummy_via:reset(),
    {ok, Pid7} =
	gen_server:start_link({via, dummy_via, my_test_name},
			      gen_server_SUITE, [], []),
    ok = gen_server:call({via, dummy_via, my_test_name}, started_p),
    {error, {already_started, Pid7}} =
	gen_server:start({via, dummy_via, my_test_name},
			 gen_server_SUITE, [], []),
    ok = gen_server:call({via, dummy_via, my_test_name}, stop),
    receive
	{'EXIT', Pid7, stopped} ->
	    ok
    after 5000 ->
	    ct:fail(not_stopped)
    end,
    receive
	Msg -> ct:fail({unexpected,Msg})
    after 1 -> ok
    end,

    process_flag(trap_exit, OldFl),
    ok.

start_event_timeout(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    lists:foreach(
	fun({StartFun, Arg, Interrupt}) ->
	    {ok, Pid} = StartFun(Arg),
	    sys:get_status(Pid),
	    case element(1, Arg) of
		timeout ->
		    is_not_in_erlang_hibernate(Pid);
		continue_timeout ->
		    is_not_in_erlang_hibernate(Pid);
		hibernate ->
		    is_in_erlang_hibernate(Pid);
		continue_hibernate ->
		    is_in_erlang_hibernate(Pid)
	    end,
	    case Interrupt of
		true ->
		    pong = gen_server:call(Pid, ping);
		false ->
		    ok
	    end,
	    receive
		{Pid, event_timeout} ->
		    not Interrupt orelse ct:fail(event_timeout_message_received)
	    after 1000 ->
		Interrupt orelse ct:fail(event_timeout_message_not_received)
	    end,
	    is_not_in_erlang_hibernate(Pid),
	    ok = gen_server:call(Pid, stop),
	    receive
		{'EXIT', Pid, _} ->
		    ok
	    after 5000 ->
		ct:fail(gen_server_did_not_die)
	    end
	end,
	[{StartFun, Arg, Interrupt} || StartFun <- [fun(X) -> start_link(spec_init_action, [[], X]) end,
						    fun(X) -> gen_server:start_link(gen_server_SUITE, X, []) end],
				       Arg <- [{timeout, 500, self()},
					       {timeout, 500, self(), relative},
					       {timeout, 500, self(), absolute},
					       {continue_timeout, 500, self()},
					       {continue_timeout, 500, self(), relative},
					       {continue_timeout, 500, self(), absolute},
					       {hibernate, 500, self()},
					       {hibernate, 500, self(), relative},
					       {hibernate, 500, self(), absolute},
					       {continue_hibernate, 500, self()},
					       {continue_hibernate, 500, self(), relative},
					       {continue_hibernate, 500, self(), absolute}],
				       Interrupt <- [false, true]]),

    process_flag(trap_exit, OldFl),
    ok.

start_event_timeout_zero(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    lists:foreach(
	fun({StartFun, Arg}) ->
	    {ok, Pid} = StartFun(Arg),
	    receive
		{Pid, after_event_timeout_zero} ->
		    ct:fail(after_event_timeout_zero_message_received);
		{Pid, event_timeout} ->
		    ok
	    after 1000 ->
		ct:fail(event_timeout_message_not_received)
	    end,
	    receive
		{Pid, after_event_timeout_zero} ->
		    ok
	    after 1000 ->
		ct:fail(after_event_timeout_zero_message_not_received)
	    end,
	    is_not_in_erlang_hibernate(Pid),
	    ok = gen_server:call(Pid, stop),
	    receive
		{'EXIT', Pid, _} ->
		    ok
	    after 5000 ->
		ct:fail(gen_server_did_not_die)
	    end
	end,
	[{StartFun, Arg} || StartFun <- [fun(X) -> start_link(spec_init_action, [[], X]) end,
					 fun(X) -> gen_server:start_link(gen_server_SUITE, X, []) end],
			    Arg <- [{timeout_zero, self()},
				    {continue_timeout_zero, self()},
				    {hibernate_zero, self()},
				    {continue_hibernate_zero, self()}]]),

    process_flag(trap_exit, OldFl),
    ok.

%% Anonymous, reason 'normal'
stop1(_Config) ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    ok = gen_server:stop(Pid),
    false = erlang:is_process_alive(Pid),
    {'EXIT',noproc} = (catch gen_server:stop(Pid)),
    ok.

%% Anonymous, other reason
stop2(_Config) ->
    {ok,Pid} = gen_server:start(?MODULE, [], []),
    ok = gen_server:stop(Pid, other_reason, infinity),
    false = erlang:is_process_alive(Pid),
    ok.

%% Anonymous, invalid timeout
stop3(_Config) ->
    {ok,Pid} = gen_server:start(?MODULE, [], []),
    {'EXIT',_} = (catch gen_server:stop(Pid, other_reason, invalid_timeout)),
    true = erlang:is_process_alive(Pid),
    ok = gen_server:stop(Pid),
    false = erlang:is_process_alive(Pid),
    ok.

%% Registered name
stop4(_Config) ->
    {ok,Pid} = gen_server:start({local,to_stop},?MODULE, [], []),
    ok = gen_server:stop(to_stop),
    false = erlang:is_process_alive(Pid),
    {'EXIT',noproc} = (catch gen_server:stop(to_stop)),
    ok.

%% Registered name and local node
stop5(_Config) ->
    {ok,Pid} = gen_server:start({local,to_stop},?MODULE, [], []),
    ok = gen_server:stop({to_stop,node()}),
    false = erlang:is_process_alive(Pid),
    {'EXIT',noproc} = (catch gen_server:stop({to_stop,node()})),
    ok.

%% Globally registered name
stop6(_Config) ->
    {ok, Pid} = gen_server:start({global, to_stop}, ?MODULE, [], []),
    ok = gen_server:stop({global,to_stop}),
    false = erlang:is_process_alive(Pid),
    {'EXIT',noproc} = (catch gen_server:stop({global,to_stop})),
    ok.

%% 'via' registered name
stop7(_Config) ->
    dummy_via:reset(),
    {ok, Pid} = gen_server:start({via, dummy_via, to_stop},
				 ?MODULE, [], []),
    ok = gen_server:stop({via, dummy_via, to_stop}),
    false = erlang:is_process_alive(Pid),
    {'EXIT',noproc} = (catch gen_server:stop({via, dummy_via, to_stop})),
    ok.

%% Anonymous on remote node
stop8(_Config) ->
    {ok,Peer,Node} = ?CT_PEER(),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node,code,add_path,[Dir]),
    {ok, Pid} = rpc:call(Node,gen_server,start,[?MODULE,[],[]]),
    ok = gen_server:stop(Pid),
    false = rpc:call(Node,erlang,is_process_alive,[Pid]),
    {'EXIT',noproc} = (catch gen_server:stop(Pid)),
    peer:stop(Peer),
    {'EXIT',{{nodedown,Node},_}} = (catch gen_server:stop(Pid)),
    ok.

%% Registered name on remote node
stop9(_Config) ->
    {ok,Peer,Node} = ?CT_PEER(),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node,code,add_path,[Dir]),
    {ok, Pid} = rpc:call(Node,gen_server,start,[{local,to_stop},?MODULE,[],[]]),
    ok = gen_server:stop({to_stop,Node}),
    undefined = rpc:call(Node,erlang,whereis,[to_stop]),
    false = rpc:call(Node,erlang,is_process_alive,[Pid]),
    {'EXIT',noproc} = (catch gen_server:stop({to_stop,Node})),
    peer:stop(Peer),
    {'EXIT',{{nodedown,Node},_}} = (catch gen_server:stop({to_stop,Node})),
    ok.

%% Globally registered name on remote node
stop10(_Config) ->
    {ok,Peer,Node} = ?CT_PEER(),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node,code,add_path,[Dir]),
    {ok, Pid} = rpc:call(Node,gen_server,start,[{global,to_stop},?MODULE,[],[]]),
    ok = global:sync(),
    ok = gen_server:stop({global,to_stop}),
    false = rpc:call(Node,erlang,is_process_alive,[Pid]),
    {'EXIT',noproc} = (catch gen_server:stop({global,to_stop})),
    peer:stop(Peer),
    {'EXIT',noproc} = (catch gen_server:stop({global,to_stop})),
    ok.

crash(Config) when is_list(Config) ->
    error_logger_forwarder:register(),

    process_flag(trap_exit, true),

    %% This crash should not generate a crash report.
    {ok,Pid0} = gen_server:start_link(?MODULE, [], []),
    {'EXIT',{{shutdown,reason},_}} =
 	(catch gen_server:call(Pid0, shutdown_reason)),
    receive {'EXIT',Pid0,{shutdown,reason}} -> ok end,

    %% This crash should not generate a crash report.
    {ok,Pid1} = gen_server:start_link(?MODULE, {state,state1}, []),
    {'EXIT',{{shutdown,stop_reason},_}} =
	(catch gen_server:call(Pid1, stop_shutdown_reason)),
    receive {'EXIT',Pid1,{shutdown,stop_reason}} -> ok end,

    %% This crash should not generate a crash report.
    {ok,Pid2} = gen_server:start_link(?MODULE, [], []),
    {'EXIT',{shutdown,_}} =
 	(catch gen_server:call(Pid2, exit_shutdown)),
    receive {'EXIT',Pid2,shutdown} -> ok end,

    %% This crash should not generate a crash report.
    {ok,Pid3} = gen_server:start_link(?MODULE, {state,state3}, []),
    {'EXIT',{shutdown,_}} =
	(catch gen_server:call(Pid3, stop_shutdown)),
    receive {'EXIT',Pid3,shutdown} -> ok end,

    process_flag(trap_exit, false),

    %% This crash should generate a crash report and a report
    %% from gen_server.
    {ok,Pid4} = gen_server:start(?MODULE, {state,state4}, []),
    {'EXIT',{crashed,_}} = (catch gen_server:call(Pid4, crash)),
    ClientPid = self(),
    receive
	{error,_GroupLeader4,{Pid4,
			      "** Generic server"++_,
			      [Pid4,crash,{formatted, state4},
			       {crashed,[{?MODULE,handle_call,3,_}
					 |_Stacktrace]},
			       ClientPid, [_|_] = _ClientStack]}} ->
	    ok;
	Other4a ->
	    io:format("Unexpected: ~p", [Other4a]),
	    ct:fail(failed)
    end,
    receive
	{error_report,_,{Pid4,crash_report,[List4|_]}} ->
	    {exit,crashed,[{?MODULE, handle_call, 3, _}|_]} = proplists:get_value(error_info, List4),
	    Pid4 = proplists:get_value(pid, List4);
	Other4 ->
	    io:format("Unexpected: ~p", [Other4]),
	    ct:fail(failed)
    end,

    receive
	Any ->
	    io:format("Unexpected: ~p", [Any]),
	    ct:fail(failed)
    after 500 ->
	    ok
    end,

    ok.


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
      fun gen_server:Start/3,
      {ets, {return, {stop, failed_to_start}}}, Opts,
      fun ({error, failed_to_start}) -> ok end),
    loop_start_fail(
      fun gen_server:Start/3,
      {ets, {return, ignore}}, Opts,
      fun (ignore) -> ok end),
    loop_start_fail(
      fun gen_server:Start/3,
      {ets, {return, 4711}}, Opts,
      fun ({error, {bad_return_value, 4711}}) -> ok end),
    loop_start_fail(
      fun gen_server:Start/3,
      {ets, {crash, error, bailout}}, Opts,
      fun ({error, {bailout, ST}}) when is_list(ST) -> ok end),
    loop_start_fail(
      fun gen_server:Start/3,
      {ets, {crash, exit, bailout}}, Opts,
      fun ({error, bailout}) -> ok end),
    loop_start_fail(
      fun gen_server:Start/3,
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



%% --------------------------------------
%% Test gen_server:call and handle_call.
%% Test all different return values from
%% handle_call.
%% --------------------------------------

call(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    {ok, _Pid} =
	gen_server:start_link({local, my_test_name},
			      gen_server_SUITE, [], []),

    ok = gen_server:call(my_test_name, started_p),
    delayed = gen_server:call(my_test_name, {delayed_answer,1}),

    %% two requests within a specified time.
    ok = gen_server:call(my_test_name, {call_within, 1000}),
    timer:sleep(500),
    ok = gen_server:call(my_test_name, next_call),
    ok = gen_server:call(my_test_name, {call_within, 1000}),
    timer:sleep(1500),
    false = gen_server:call(my_test_name, next_call),

    %% timeout call.
    delayed = gen_server:call(my_test_name, {delayed_answer,1}, 30),
    {'EXIT',{timeout,_}} =
	(catch gen_server:call(my_test_name, {delayed_answer,30}, 1)),

    %% bad return value in the gen_server loop from handle_call.
    {'EXIT',{{bad_return_value, badreturn},_}} =
	(catch gen_server:call(my_test_name, badreturn)),

    process_flag(trap_exit, OldFl),
    ok.

%% --------------------------------------
%% Test gen_server:send_request.
%% --------------------------------------

send_request(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    {ok, Pid} = gen_server:start_link({local, my_test_name},
                                      gen_server_SUITE, [], []),

    Async = fun(Process, Req) ->
                    try
                        Promise = gen_server:send_request(Process, Req),
                        gen_server:wait_response(Promise, infinity)
                    catch _:Reason:ST ->
                            {'did_exit', Reason, ST}
                    end
            end,
    {reply,ok} = Async(my_test_name, started_p),

    {reply,delayed} = Async(Pid, {delayed_answer,1}),

    %% two requests within a specified time.
    Promise1 = gen_server:send_request(my_test_name, {call_within, 1000}),
    Promise2 = gen_server:send_request(my_test_name, next_call),
    {reply, ok} = gen_server:wait_response(Promise1, infinity),
    {reply, ok} = gen_server:wait_response(Promise2, infinity),

    Promise3 = gen_server:send_request(my_test_name, {call_within, 1000}),
    no_reply = gen_server:check_response({foo, bar}, Promise3),
    receive {[alias|Ref],_} = Msg when is_reference(Ref) ->
            {reply, ok} = gen_server:check_response(Msg, Promise3)
    after 1000 ->
            %% Format changed which is ok. This test is just to make you
            %% aware that you have changed it
            exit(message_format_changed)
    end,
    timer:sleep(1500),

    {reply, false} = Async(my_test_name, next_call),

    %% timeout
    Promise5 = gen_server:send_request(my_test_name, {delayed_answer,50}),
    timeout = gen_server:wait_response(Promise5, 0),
    {reply, delayed} = gen_server:wait_response(Promise5, infinity),

    %% bad return value in the gen_server loop from handle_call.
    {error,{{bad_return_value, badreturn},_}} = Async(my_test_name, badreturn),

    %% Test other error cases
    {error, {noproc,_}} = Async(Pid, started_p),
    {error, {noproc,_}} = Async(my_test_name, started_p),
    {error, {noconnection, _}} = Async({my_test_name, foo@node}, started_p),

    {error, {noproc,_}} = Async({global, non_existing}, started_p),
    catch exit(whereis(dummy_via), foo),
    {'EXIT', {badarg,_}} =
        (catch gen_server:send_request({via, dummy_via, non_existing}, started_p)),

    %% Remote nodes
    Via = dummy_via:reset(),
    Remote = proplists:get_value(node,Config),
    {ok, RPid} = rpc:call(Remote, gen_server, start, [{global, remote}, ?MODULE, [], []]),
    dummy_via:register_name(remote, RPid),
    {reply, ok} = Async(RPid, started_p),
    {reply, ok} = Async({global, remote}, started_p),
    {reply, ok} = Async({via, dummy_via, remote}, started_p),
    {error, {shutdown, _}} = Async({global, remote}, stop_shutdown),
    {error, {noproc, _}} = Async({global, remote}, started_p),
    {error, {noproc, _}} = Async({via, dummy_via, remote}, started_p),
    {error, {noproc, _}} = Async({via, dummy_via, non_existing}, started_p),

    {ok, _} = rpc:call(Remote, gen_server, start, [{local, remote}, ?MODULE, [], []]),
    {reply, ok} = Async({remote, Remote}, started_p),
    {error, {shutdown, _}} = Async({remote, Remote}, stop_shutdown),
    {error, {noproc, _}} = Async({remote, Remote}, started_p),

    %% Cleanup
    catch exit(Via, foo2),
    receive {'EXIT', Via, foo2} -> ok end,
    process_flag(trap_exit, OldFl),
    ok.

send_request_receive_reqid_collection(Config) when is_list(Config) ->
    {ok, Pid1} = gen_server:start_link({local, my_test_name1},
                                       gen_server_SUITE, [], []),
    {ok, Pid2} = gen_server:start_link({local, my_test_name2},
                                       gen_server_SUITE, [], []),
    {ok, Pid3} = gen_server:start_link({local, my_test_name3},
                                       gen_server_SUITE, [], []),
    send_request_receive_reqid_collection(Pid1, Pid2, Pid3),
    send_request_receive_reqid_collection_timeout(Pid1, Pid2, Pid3),
    send_request_receive_reqid_collection_error(Pid1, Pid2, Pid3),
    unlink(Pid1),
    exit(Pid1, kill),
    unlink(Pid2),
    exit(Pid2, kill),
    unlink(Pid3),
    exit(Pid3, kill),
    false = is_process_alive(Pid1),
    false = is_process_alive(Pid2),
    false = is_process_alive(Pid3),
    ok.

send_request_receive_reqid_collection(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_server:send_request(Pid1, started_p),

    ReqIdC0 = gen_server:reqids_new(),

    ReqId1 = gen_server:send_request(Pid1, {delayed_answer,400}),
    ReqIdC1 = gen_server:reqids_add(ReqId1, req1, ReqIdC0),
    1 = gen_server:reqids_size(ReqIdC1),

    ReqIdC2 = gen_server:send_request(Pid2, {delayed_answer,1}, req2, ReqIdC1),
    2 = gen_server:reqids_size(ReqIdC2),

    ReqIdC3 = gen_server:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_server:reqids_size(ReqIdC3),
    
    {{reply, delayed}, req2, ReqIdC4} = gen_server:receive_response(ReqIdC3, infinity, true),
    2 = gen_server:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC5} = gen_server:receive_response(ReqIdC4, 5678, true),
    1 = gen_server:reqids_size(ReqIdC5),

    {{reply, delayed}, req1, ReqIdC6} = gen_server:receive_response(ReqIdC5, 5000, true),
    0 = gen_server:reqids_size(ReqIdC6),

    no_request = gen_server:receive_response(ReqIdC6, 5000, true),

    {reply, ok} = gen_server:receive_response(ReqId0, infinity),

    ok.

send_request_receive_reqid_collection_timeout(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_server:send_request(Pid1, started_p),

    ReqIdC0 = gen_server:reqids_new(),

    ReqId1 = gen_server:send_request(Pid1, {delayed_answer,1000}),
    ReqIdC1 = gen_server:reqids_add(ReqId1, req1, ReqIdC0),

    ReqIdC2 = gen_server:send_request(Pid2, {delayed_answer,1}, req2, ReqIdC1),

    ReqId3 = gen_server:send_request(Pid3, {delayed_answer,500}),
    ReqIdC3 = gen_server:reqids_add(ReqId3, req3, ReqIdC2),

    Deadline = erlang:monotonic_time(millisecond) + 100,
    
    {{reply, delayed}, req2, ReqIdC4} = gen_server:receive_response(ReqIdC3, {abs, Deadline}, true),
    2 = gen_server:reqids_size(ReqIdC4),

    timeout = gen_server:receive_response(ReqIdC4, {abs, Deadline}, true),

    Abandoned = lists:sort([{ReqId1, req1}, {ReqId3, req3}]),
    Abandoned = lists:sort(gen_server:reqids_to_list(ReqIdC4)),

    %% Make sure requests were abandoned...
    timeout = gen_server:receive_response(ReqIdC4, {abs, Deadline+1000}, true),

    {reply, ok} = gen_server:receive_response(ReqId0, infinity),

    ok.
    
send_request_receive_reqid_collection_error(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_server:send_request(Pid1, started_p),

    ReqIdC0 = gen_server:reqids_new(),

    ReqId1 = gen_server:send_request(Pid1, {delayed_answer,400}),
    ReqIdC1 = gen_server:reqids_add(ReqId1, req1, ReqIdC0),
    try
        nope = gen_server:reqids_add(ReqId1, req2, ReqIdC1)
    catch
        error:badarg -> ok
    end,
 
    unlink(Pid2),
    ReqIdC2 = gen_server:send_request(Pid2, stop_shutdown, req2, ReqIdC1),
    ReqIdC3 = gen_server:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_server:reqids_size(ReqIdC3),
    
    {{error, {shutdown, _}}, req2, ReqIdC4} = gen_server:receive_response(ReqIdC3, 2000, true),
    2 = gen_server:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC4} = gen_server:receive_response(ReqIdC4, infinity, false),

    {{reply, delayed}, req1, ReqIdC4} = gen_server:receive_response(ReqIdC4, infinity, false),

    {reply, ok} = gen_server:receive_response(ReqId0, infinity),

    ok.

send_request_wait_reqid_collection(Config) when is_list(Config) ->
    {ok, Pid1} = gen_server:start_link({local, my_test_name1},
                                       gen_server_SUITE, [], []),
    {ok, Pid2} = gen_server:start_link({local, my_test_name2},
                                       gen_server_SUITE, [], []),
    {ok, Pid3} = gen_server:start_link({local, my_test_name3},
                                       gen_server_SUITE, [], []),
    send_request_wait_reqid_collection(Pid1, Pid2, Pid3),
    send_request_wait_reqid_collection_timeout(Pid1, Pid2, Pid3),
    send_request_wait_reqid_collection_error(Pid1, Pid2, Pid3),
    unlink(Pid1),
    exit(Pid1, kill),
    unlink(Pid2),
    exit(Pid2, kill),
    unlink(Pid3),
    exit(Pid3, kill),
    false = is_process_alive(Pid1),
    false = is_process_alive(Pid2),
    false = is_process_alive(Pid3),
    ok.

send_request_wait_reqid_collection(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_server:send_request(Pid1, started_p),

    ReqIdC0 = gen_server:reqids_new(),

    ReqId1 = gen_server:send_request(Pid1, {delayed_answer,400}),
    ReqIdC1 = gen_server:reqids_add(ReqId1, req1, ReqIdC0),
    1 = gen_server:reqids_size(ReqIdC1),

    ReqIdC2 = gen_server:send_request(Pid2, {delayed_answer,1}, req2, ReqIdC1),
    2 = gen_server:reqids_size(ReqIdC2),

    ReqIdC3 = gen_server:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_server:reqids_size(ReqIdC3),
    
    {{reply, delayed}, req2, ReqIdC4} = gen_server:wait_response(ReqIdC3, infinity, true),
    2 = gen_server:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC5} = gen_server:wait_response(ReqIdC4, 5678, true),
    1 = gen_server:reqids_size(ReqIdC5),

    {{reply, delayed}, req1, ReqIdC6} = gen_server:wait_response(ReqIdC5, 5000, true),
    0 = gen_server:reqids_size(ReqIdC6),

    no_request = gen_server:wait_response(ReqIdC6, 5000, true),

    {reply, ok} = gen_server:wait_response(ReqId0, infinity),

    ok.

send_request_wait_reqid_collection_timeout(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_server:send_request(Pid1, started_p),

    ReqIdC0 = gen_server:reqids_new(),

    ReqId1 = gen_server:send_request(Pid1, {delayed_answer,1000}),
    ReqIdC1 = gen_server:reqids_add(ReqId1, req1, ReqIdC0),

    ReqIdC2 = gen_server:send_request(Pid2, {delayed_answer,1}, req2, ReqIdC1),

    ReqId3 = gen_server:send_request(Pid3, {delayed_answer,500}),
    ReqIdC3 = gen_server:reqids_add(ReqId3, req3, ReqIdC2),

    Deadline = erlang:monotonic_time(millisecond) + 100,
    
    {{reply, delayed}, req2, ReqIdC4} = gen_server:wait_response(ReqIdC3, {abs, Deadline}, true),
    2 = gen_server:reqids_size(ReqIdC4),

    timeout = gen_server:wait_response(ReqIdC4, {abs, Deadline}, true),

    Unhandled = lists:sort([{ReqId1, req1}, {ReqId3, req3}]),
    Unhandled = lists:sort(gen_server:reqids_to_list(ReqIdC4)),

    %% Make sure requests were not abandoned...
    {{reply, delayed}, req3, ReqIdC4} = gen_server:wait_response(ReqIdC4, {abs, Deadline+1500}, false),
    {{reply, delayed}, req1, ReqIdC4} = gen_server:wait_response(ReqIdC4, {abs, Deadline+1500}, false),

    {reply, ok} = gen_server:receive_response(ReqId0, infinity),

    ok.
    
send_request_wait_reqid_collection_error(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_server:send_request(Pid1, started_p),

    ReqIdC0 = gen_server:reqids_new(),

    ReqId1 = gen_server:send_request(Pid1, {delayed_answer,400}),
    ReqIdC1 = gen_server:reqids_add(ReqId1, req1, ReqIdC0),
    try
        nope = gen_server:reqids_add(ReqId1, req2, ReqIdC1)
    catch
        error:badarg -> ok
    end,
 
    unlink(Pid2),
    ReqIdC2 = gen_server:send_request(Pid2, stop_shutdown, req2, ReqIdC1),
    ReqIdC3 = gen_server:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_server:reqids_size(ReqIdC3),
    
    {{error, {shutdown, _}}, req2, ReqIdC4} = gen_server:wait_response(ReqIdC3, 2000, true),
    2 = gen_server:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC4} = gen_server:wait_response(ReqIdC4, infinity, false),

    {{reply, delayed}, req1, ReqIdC4} = gen_server:wait_response(ReqIdC4, infinity, false),

    {reply, ok} = gen_server:wait_response(ReqId0, infinity),

    ok.

send_request_check_reqid_collection(Config) when is_list(Config) ->
    {ok, Pid1} = gen_server:start_link({local, my_test_name1},
                                       gen_server_SUITE, [], []),
    {ok, Pid2} = gen_server:start_link({local, my_test_name2},
                                       gen_server_SUITE, [], []),
    {ok, Pid3} = gen_server:start_link({local, my_test_name3},
                                       gen_server_SUITE, [], []),
    send_request_check_reqid_collection(Pid1, Pid2, Pid3),
    send_request_check_reqid_collection_error(Pid1, Pid2, Pid3),
    unlink(Pid1),
    exit(Pid1, kill),
    unlink(Pid2),
    exit(Pid2, kill),
    unlink(Pid3),
    exit(Pid3, kill),
    false = is_process_alive(Pid1),
    false = is_process_alive(Pid2),
    false = is_process_alive(Pid3),
    ok.

send_request_check_reqid_collection(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_server:send_request(Pid1, started_p),

    receive after 100 -> ok end,

    ReqIdC0 = gen_server:reqids_new(),

    ReqIdC1 = gen_server:send_request(Pid1, {delayed_answer,400}, req1, ReqIdC0),
    1 = gen_server:reqids_size(ReqIdC1),

    ReqId2 = gen_server:send_request(Pid2, {delayed_answer,1}),
    ReqIdC2 = gen_server:reqids_add(ReqId2, req2, ReqIdC1),
    2 = gen_server:reqids_size(ReqIdC2),

    ReqIdC3 = gen_server:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_server:reqids_size(ReqIdC3),

    Msg0 = next_msg(),
    no_reply = gen_server:check_response(Msg0, ReqIdC3, true),
    
    {{reply, delayed}, req2, ReqIdC4} = gen_server:check_response(next_msg(), ReqIdC3, true),
    2 = gen_server:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC5} = gen_server:check_response(next_msg(), ReqIdC4, true),
    1 = gen_server:reqids_size(ReqIdC5),

    {{reply, delayed}, req1, ReqIdC6} = gen_server:check_response(next_msg(), ReqIdC5, true),
    0 = gen_server:reqids_size(ReqIdC6),

    no_request = gen_server:check_response(Msg0, ReqIdC6, true),

    {reply, ok} = gen_server:check_response(Msg0, ReqId0),

    ok.
    
send_request_check_reqid_collection_error(Pid1, Pid2, Pid3) ->

    ReqId0 = gen_server:send_request(Pid1, started_p),

    receive after 100 -> ok end,

    ReqIdC0 = gen_server:reqids_new(),

    ReqId1 = gen_server:send_request(Pid1, {delayed_answer,400}),
    ReqIdC1 = gen_server:reqids_add(ReqId1, req1, ReqIdC0),

    unlink(Pid2),
    ReqIdC2 = gen_server:send_request(Pid2, stop_shutdown, req2, ReqIdC1),

    ReqIdC3 = gen_server:send_request(Pid3, {delayed_answer,200}, req3, ReqIdC2),
    3 = gen_server:reqids_size(ReqIdC3),

    Msg0 = next_msg(),

    no_reply = gen_server:check_response(Msg0, ReqIdC3, true),
    
    {{error, {shutdown, _}}, req2, ReqIdC4} = gen_server:check_response(next_msg(), ReqIdC3, true),
    2 = gen_server:reqids_size(ReqIdC4),

    {{reply, delayed}, req3, ReqIdC4} = gen_server:check_response(next_msg(), ReqIdC4, false),

    {{reply, delayed}, req1, ReqIdC4} = gen_server:check_response(next_msg(), ReqIdC4, false),

    {reply, ok} = gen_server:check_response(Msg0, ReqId0),

    ok.

next_msg() ->
    receive M -> M end.

%% --------------------------------------
%% Test handle_continue.
%% --------------------------------------

continue(Config) when is_list(Config) ->
    {ok, Pid} = gen_server:start_link(gen_server_SUITE, {continue, self()}, []),
    [{Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    gen_server:call(Pid, {continue_reply, self()}),
    [{Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    gen_server:call(Pid, {continue_noreply, self()}),
    [{Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    gen_server:cast(Pid, {continue_noreply, self()}),
    [{Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    Pid ! {continue_noreply, self()},
    [{Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    Pid ! {continue_continue, self()},
    [{Pid, before_continue}, {Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    Ref = monitor(process, Pid),
    Pid ! continue_stop,
    verify_down_reason(Ref, Pid, normal).

read_replies(Pid) ->
    receive
	{Pid, ack} -> read_replies()
    after
	1000 -> ct:fail({continue, ack})
    end.

read_replies() ->
    receive
	Msg -> [Msg | read_replies()]
    after
	0 -> []
    end.

%% --------------------------------------
%% Test call to nonexisting processes on remote nodes
%% --------------------------------------

call_remote1(Config) when is_list(Config) ->
    N = hubba,
    Node = proplists:get_value(node,Config),
    {ok, Pid} = rpc:call(Node, gen_server, start,
			 [{global, N}, ?MODULE, [], []]),
    ok = (catch gen_server:call({global, N}, started_p, infinity)),
    exit(Pid, boom),
    {'EXIT', {Reason, _}} = (catch gen_server:call({global, N},
						   started_p, infinity)),
    true = (Reason == noproc) orelse (Reason == boom),
    ok.

call_remote2(Config) when is_list(Config) ->
    N = hubba,
    Node = proplists:get_value(node,Config),

    {ok, Pid} = rpc:call(Node, gen_server, start,
			 [{global, N}, ?MODULE, [], []]),
    ok = (catch gen_server:call(Pid, started_p, infinity)),
    exit(Pid, boom),
    {'EXIT', {Reason, _}} = (catch gen_server:call(Pid,
						   started_p, infinity)),
    true = (Reason == noproc) orelse (Reason == boom),
    ok.

call_remote3(Config) when is_list(Config) ->
    Node = proplists:get_value(node,Config),

    {ok, Pid} = rpc:call(Node, gen_server, start,
			 [{local, piller}, ?MODULE, [], []]),
    ok = (catch gen_server:call({piller, Node}, started_p, infinity)),
    exit(Pid, boom),
    {'EXIT', {Reason, _}} = (catch gen_server:call({piller, Node},
						   started_p, infinity)),
    true = (Reason == noproc) orelse (Reason == boom),
    ok.

%% --------------------------------------
%% Test call to nonexisting node
%% --------------------------------------

call_remote_n1(Config) when is_list(Config) ->
    N = hubba,
    Node = proplists:get_value(node,Config),
    {ok, _Pid} = rpc:call(Node, gen_server, start,
			  [{global, N}, ?MODULE, [], []]),
    peer:stop(proplists:get_value(peer,Config)),
    {'EXIT', {noproc, _}} =
	(catch gen_server:call({global, N}, started_p, infinity)),

    ok.

call_remote_n2(Config) when is_list(Config) ->
    N = hubba,
    Node = proplists:get_value(node,Config),

    {ok, Pid} = rpc:call(Node, gen_server, start,
			 [{global, N}, ?MODULE, [], []]),
    peer:stop(proplists:get_value(peer,Config)),
    {'EXIT', {{nodedown, Node}, _}} = (catch gen_server:call(Pid,
							     started_p, infinity)),

    ok.

call_remote_n3(Config) when is_list(Config) ->
    Node = proplists:get_value(node,Config),

    {ok, _Pid} = rpc:call(Node, gen_server, start,
			  [{local, piller}, ?MODULE, [], []]),
    peer:stop(proplists:get_value(peer,Config)),
    {'EXIT', {{nodedown, Node}, _}} = (catch gen_server:call({piller, Node},
							     started_p, infinity)),

    ok.

%% --------------------------------------
%% Other bad calls
%% --------------------------------------

calling_self(Config) when is_list(Config) ->
    {'EXIT', {calling_self, _}} = (catch gen_server:call(self(), oops)),
    {'EXIT', {calling_self, _}} = (catch gen_server:call(self(), oops, infinity)),
    ok.

%% --------------------------------------
%% Test gen_server:cast and handle_cast.
%% Test all different return values from
%% handle_cast.
%% --------------------------------------

cast(Config) when is_list(Config) ->
    {ok, Pid} =
	gen_server:start({local, my_test_name},
			 gen_server_SUITE, [], []),

    ok = gen_server:call(my_test_name, started_p),

    ok = gen_server:cast(my_test_name, {self(),handle_cast}),
    receive
	{Pid, handled_cast} ->
	    ok
    after 1000 ->
	    ct:fail(handle_cast)
    end,

    ok = gen_server:cast(my_test_name, {self(),delayed_cast,1}),
    receive
	{Pid, delayed} ->
	    ok
    after 1000 ->
	    ct:fail(delayed_cast)
    end,

    ok = gen_server:cast(my_test_name, {self(),stop}),
    receive
	{Pid, stopped} ->
	    ok
    after 1000 ->
	    ct:fail(stop)
    end,
    ok.

%% Test that cast really return immediately.
cast_fast(Config) when is_list(Config) ->
    {ok,Peer,Node} = ?CT_PEER(),
    %% After starting a slave, it takes a little while until global knows
    %% about it, even if nodes() includes it, so we make sure that global
    %% knows about it before registering something on all nodes.
    ok = global:sync(),
    {_,"@"++Host} = lists:splitwith(fun ($@) -> false; (_) -> true end,
				    atom_to_list(Node)),
    FalseNode = list_to_atom("hopp@"++Host),
    true = rpc:cast(Node, ?MODULE, cast_fast_messup, []),
    ct:sleep(1000),
    [Node] = nodes(),
    {Time,ok} = timer:tc(fun() ->
				 gen_server:cast({hopp,FalseNode}, hopp)
			 end),
    peer:stop(Peer),
    if Time > 1000000 ->       % Default listen timeout is about 7.0 s
	    ct:fail(hanging_cast);
       true ->
	    ok
    end.

cast_fast_messup() ->
    %% Register a false node: hopp@hostname
    unregister(erl_epmd),
    {ok, _} = erl_epmd:start_link(),
    {ok,S} = gen_tcp:listen(0, []),
    {ok,P} = inet:port(S),
    {ok,_Creation} = erl_epmd:register_node(hopp, P),
    receive after infinity -> ok end.

%% --------------------------------------
%% Test handle_info.
%% --------------------------------------

info(Config) when is_list(Config) ->
    {ok, Pid} =
	gen_server:start({local, my_test_name},
			 gen_server_SUITE, [], []),

    ok = gen_server:call(my_test_name, started_p),

    Pid ! {self(),handle_info},
    receive
	{Pid, handled_info} ->
	    ok
    after 1000 ->
	    ct:fail(handle_info)
    end,

    Pid ! {self(),delayed_info,1},
    receive
	{Pid, delayed_info} ->
	    ok
    after 1000 ->
	    ct:fail(delayed_info)
    end,

    Pid ! {self(),stop},
    receive
	{Pid, stopped_info} ->
	    ok
    after 1000 ->
	    ct:fail(stop_info)
    end,
    ok.

handle_event_timeout(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    {ok, Pid} =
        gen_server:start(gen_server_SUITE, [], []),
    pong = gen_server:call(Pid, ping),

    lists:foreach(
	fun({Cmd, Arg, Interrupt}) ->
	    Cmd(Pid, Arg),
	    sys:get_status(Pid),
	    case element(1, Arg) of
		timeout ->
		    is_not_in_erlang_hibernate(Pid);
		continue_timeout ->
		    is_not_in_erlang_hibernate(Pid);
		hibernate ->
		    is_in_erlang_hibernate(Pid);
		continue_hibernate ->
		    is_in_erlang_hibernate(Pid)
	    end,
	    case Interrupt of
		true ->
		    pong = gen_server:call(Pid, ping);
		false ->
		    ok
	    end,
	    receive
		{Pid, event_timeout} ->
		    not Interrupt orelse ct:fail(event_timeout_message_received)
	    after 1000 ->
		Interrupt orelse ct:fail(event_timeout_message_not_received)
	    end,
	    is_not_in_erlang_hibernate(Pid)
	end,
	[{Cmd, Arg, Interrupt} || Cmd <- [fun gen_server:call/2,
					  fun gen_server:cast/2,
					  fun erlang:send/2],
				  Arg <- [{timeout, 500, self()},
					  {timeout, 500, self(), relative},
					  {timeout, 500, self(), absolute},
					  {continue_timeout, 500, self()},
					  {continue_timeout, 500, self(), relative},
					  {continue_timeout, 500, self(), absolute},
					  {hibernate, 500, self()},
					  {hibernate, 500, self(), relative},
					  {hibernate, 500, self(), absolute},
					  {continue_hibernate, 500, self()},
					  {continue_hibernate, 500, self(), relative},
					  {continue_hibernate, 500, self(), absolute}],
				  Interrupt <- [false, true]]),
    ok = gen_server:call(Pid, stop),
    busy_wait_for_process(Pid, 600),
    {'EXIT', {noproc, _}} = (catch gen_server:call(Pid, started_p, 1)),

    process_flag(trap_exit, OldFl),
    ok.

handle_event_timeout_zero(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    {ok, Pid} =
        gen_server:start(gen_server_SUITE, [], []),
    pong = gen_server:call(Pid, ping),

    lists:foreach(
	fun({Cmd, Arg}) ->
	    Cmd(Pid, Arg),
	    receive
		{Pid, after_event_timeout_zero} ->
		    ct:fail(after_event_timeout_zero_message_received);
		{Pid, event_timeout} ->
		    ok
	    after 1000 ->
		ct:fail(event_timeout_message_not_received)
	    end,
	    receive
		{Pid, after_event_timeout_zero} ->
		    ok
	    after 1000 ->
		ct:fail(after_event_timeout_zero_message_not_received)
	    end
	end,
	[{Cmd, Arg} || Cmd <- [fun gen_server:call/2,
			       fun gen_server:cast/2,
			       fun erlang:send/2],
		       Arg <- [{timeout_zero, self()},
			       {continue_timeout_zero, self()},
			       {hibernate_zero, self()},
			       {continue_hibernate_zero, self()}]]),
    ok = gen_server:call(Pid, stop),
    busy_wait_for_process(Pid, 600),
    {'EXIT', {noproc, _}} = (catch gen_server:call(Pid, started_p, 1)),

    process_flag(trap_exit, OldFl),
    ok.

handle_event_timeout_plain(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    {ok, Pid} =
        gen_server:start(gen_server_SUITE, [], []),
    pong = gen_server:call(Pid, ping),

    %% a `system` message should not cancel a plain timeout
    ok = gen_server:cast(Pid, {self(), delayed_cast, 500}),
    sys:get_status(Pid),
    receive
	{Pid, delayed} ->
	    ok
    after 1000 ->
	ct:fail(delayed_cast_message_not_received)
    end,

    %% a request (or other message) should cancel a plain timeout
    ok = gen_server:cast(Pid, {self(), delayed_cast, 500}),
    pong = gen_server:call(Pid, ping),
    receive
	{Pid, delayed} ->
	    ct:fail(delayed_cast_message_received)
    after 1000 ->
	ok
    end,

    ok = gen_server:call(Pid, stop),
    busy_wait_for_process(Pid, 600),
    {'EXIT', {noproc, _}} = (catch gen_server:call(Pid, started_p, 1)),

    process_flag(trap_exit, OldFl),
    ok.

hibernate(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),
    {ok, Pid0} =
	gen_server:start_link({local, my_test_name_hibernate0},
			      gen_server_SUITE, hibernate, []),
    is_in_erlang_hibernate(Pid0),
    ok = gen_server:call(my_test_name_hibernate0, stop),
    receive 
	{'EXIT', Pid0, stopped} ->
 	    ok
    after 5000 ->
	    ct:fail(gen_server_did_not_die)
    end,

    {ok, Pid} =
	gen_server:start_link({local, my_test_name_hibernate},
			      gen_server_SUITE, [], []),

    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = gen_server:call(my_test_name_hibernate, hibernate),
    is_in_erlang_hibernate(Pid),
    Parent = self(),
    Fun = fun() ->
		  receive go -> ok end,
		  receive after 1000 -> ok end,
		  X = erlang:process_info(Pid, current_function),
 		  Pid ! continue,
 		  Parent ! {result,X}
 	  end,
    Pid2 = spawn_link(Fun),
    true = gen_server:call(my_test_name_hibernate, {hibernate_noreply,Pid2}),

    gen_server:cast(my_test_name_hibernate, hibernate_later),
    true = ({current_function,{gen_server, loop_hibernate, 4}} =/=
		erlang:process_info(Pid, current_function)),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{gen_server, loop_hibernate, 4}} =/=
		erlang:process_info(Pid, current_function)),

    gen_server:cast(my_test_name_hibernate, hibernate_now),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{gen_server, loop_hibernate, 4}} =/=
		erlang:process_info(Pid, current_function)),

    Pid ! hibernate_later,
    true = ({current_function,{gen_server, loop_hibernate, 4}} =/=
		erlang:process_info(Pid, current_function)),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{gen_server, loop_hibernate, 4}} =/=
		erlang:process_info(Pid, current_function)),

    Pid ! hibernate_now,
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{gen_server, loop_hibernate, 4}} =/=
		erlang:process_info(Pid, current_function)),
    receive
	{result,R} ->
	    {current_function,{gen_server, loop_hibernate, 4}} = R
    end,

    true = gen_server:call(my_test_name_hibernate, hibernate),
    is_in_erlang_hibernate(Pid),
    sys:suspend(my_test_name_hibernate),
    is_in_erlang_hibernate(Pid),
    sys:resume(my_test_name_hibernate),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{gen_server, loop_hibernate, 4}} =/= erlang:process_info(Pid,current_function)),

    ok = gen_server:call(my_test_name_hibernate, stop),
    receive 
	{'EXIT', Pid, stopped} ->
 	    ok
    after 5000 ->
	    ct:fail(gen_server_did_not_die)
    end,
    process_flag(trap_exit, OldFl),
    ok.

auto_hibernate(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),
    HibernateAfterTimeout = 100,
    State = {auto_hibernate_state},
    {ok, Pid} =
        gen_server:start_link({local, my_test_name_auto_hibernate},
            gen_server_SUITE, {state,State}, [{hibernate_after, HibernateAfterTimeout}]),
    %% After init test
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),
    %% Get state test
    State = sys:get_state(my_test_name_auto_hibernate),
    is_in_erlang_hibernate(Pid),
    %% Call test
    ok = gen_server:call(my_test_name_auto_hibernate, started_p),
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),
    %% Cast test
    ok = gen_server:cast(my_test_name_auto_hibernate, {self(),handle_cast}),
    receive
        {Pid, handled_cast} ->
            ok
    after 1000 ->
        ct:fail(cast)
    end,
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),
    %% Info test
    Pid ! {self(),handle_info},
    receive
        {Pid, handled_info} ->
            ok
    after 1000 ->
        ct:fail(info)
    end,
    is_not_in_erlang_hibernate(Pid),
    timer:sleep(HibernateAfterTimeout),
    is_in_erlang_hibernate(Pid),

    ok = gen_server:call(my_test_name_auto_hibernate, stop),
    receive
        {'EXIT', Pid, stopped} ->
            ok
    after 5000 ->
        ct:fail(gen_server_did_not_die)
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
	{gen_server, loop_hibernate, 4} ->
	    ok;
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
        {gen_server, loop_hibernate, 4} ->
            receive after 10 -> ok end,
            is_not_in_erlang_hibernate_1(N-1, Pid);
        {erlang,hibernate,3} ->
            receive after 10 -> ok end,
            is_not_in_erlang_hibernate_1(N-1, Pid);
        _ ->
            ok
    end.

%% --------------------------------------
%% Test gen_server:abcast and handle_cast.
%% Test all different return values from
%% handle_cast.
%% --------------------------------------

abcast(Config) when is_list(Config) ->
    {ok, Pid} =
	gen_server:start({local, my_test_name},
			 gen_server_SUITE, [], []),

    ok = gen_server:call(my_test_name, started_p),

    abcast = gen_server:abcast(my_test_name, {self(),handle_cast}),
    receive
	{Pid, handled_cast} ->
	    ok
    after 1000 ->
	    ct:fail(abcast)
    end,

    abcast = gen_server:abcast([node()], my_test_name,
			       {self(),delayed_cast,1}),
    receive
	{Pid, delayed} ->
	    ok
    after 1000 ->
	    ct:fail(delayed_abcast)
    end,

    abcast = gen_server:abcast(my_test_name, {self(),stop}),
    receive
	{Pid, stopped} ->
	    ok
    after 1000 ->
	    ct:fail(abcast_stop)
    end,
    ok.

%% --------------------------------------
%% Test gen_server:multicall and handle_call.
%% Test all different return values from
%% handle_call.
%% --------------------------------------

multicall(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    {ok, Pid} =
	gen_server:start_link({local, my_test_name},
			      gen_server_SUITE, [], []),

    ok = gen_server:call(my_test_name, started_p),
    Nodes = nodes(),
    Node = node(),
    {[{Node,delayed}],Nodes} =
	gen_server:multi_call(my_test_name, {delayed_answer,1}),

    %% two requests within a specified time.
    {[{Node,ok}],[]} =
	gen_server:multi_call([Node], my_test_name, {call_within, 1000}),
    timer:sleep(500),
    {[{Node,ok}],[]} =
	gen_server:multi_call([Node], my_test_name, next_call),
    {[{Node,ok}],[]} =
	gen_server:multi_call([Node], my_test_name, {call_within, 1000}),
    timer:sleep(1500),
    {[{Node,false}],[]} =
	gen_server:multi_call([Node],my_test_name, next_call),

    %% Stop the server.
    {[{Node,ok}],[]} =
	gen_server:multi_call([Node],my_test_name, stop),
    receive
	{'EXIT', Pid, stopped} -> ok
    after 1000 ->
	    ct:fail(multicall_stop)
    end,

    process_flag(trap_exit, OldFl),

    ok.

%% OTP-3587
multicall_down(Config) when is_list(Config) ->
    %% We need a named host which is inaccessible.
    Name = node@test01,

    %% We use 'global' as a gen_server to call.
    {Good, Bad} = gen_server:multi_call([Name, node()],
					global_name_server,
					info,
					3000),
    io:format("good = ~p, bad = ~p~n", [Good, Bad]),
    [Name] = Bad,
    ok.

busy_wait_for_process(Pid,N) ->
    case erlang:is_process_alive(Pid) of
	true ->
	    receive
	    after 100 ->
		    ok
	    end,
	    busy_wait_for_process(Pid,N-1);
	_ ->
	    ok
    end.

multicall_remote(Config) when is_list(Config) ->
    PNs = lists:map(fun (_) ->
                            {ok, P, N} = ?CT_PEER(),
                            {P, N}
                    end, lists:seq(1, 4)),
    multicall_remote_test(PNs, ?FUNCTION_NAME),
    ok.

multicall_remote_old1(Config) when is_list(Config) ->
    multicall_remote_old_test(Config, 1, ?FUNCTION_NAME).

multicall_remote_old2(Config) when is_list(Config) ->
    multicall_remote_old_test(Config, 2, ?FUNCTION_NAME).


multicall_remote_old_test(Config, OldN, Name) ->
    try
        {OldRelName, OldRel} = old_release(OldN),
        PD = proplists:get_value(priv_dir, Config),
        PNs = lists:map(fun (I) ->
                                Dir = atom_to_list(Name)++"-"++integer_to_list(I),
                                AbsDir = filename:join([PD, Dir]),
                                ok = file:make_dir(AbsDir),
                                case ?CT_PEER_REL(#{connection => 0}, OldRelName, AbsDir) of
                                    not_available ->
                                        throw({skipped, "No OTP "++OldRel++" available"});
                                    {ok, P, N} ->
                                        {P, N}
                                end
                        end, lists:seq(1, 4)),
        OldNodes = lists:map(fun ({_, N}) -> N end, PNs),
        %% Recompile on one old node and load this on all old nodes...
        SrcFile = filename:rootname(code:which(?MODULE)) ++ ".erl",
        {ok, ?MODULE, BeamCode} = erpc:call(hd(OldNodes), compile, file, [SrcFile, [binary]]),
        LoadResult = lists:duplicate(length(OldNodes), {ok, {module, ?MODULE}}),
        LoadResult = erpc:multicall(OldNodes, code, load_binary, [?MODULE, SrcFile, BeamCode]),
        multicall_remote_test(PNs, Name)
    catch
        throw:Res ->
            Res
    end.

multicall_remote_test([{Peer1, Node1},
                       {Peer2, Node2},
                       {Peer3, Node3},
                       {Peer4, Node4}],
                      Name) ->
    Tester = self(),
    ThisNode = node(),

    Nodes = [Node1, Node2, Node3, Node4, ThisNode],

    SrvList =
        lists:map(fun (Node) ->
                          Ctrl = spawn_link(Node, ?MODULE,
                                            multicall_srv_ctrlr,
                                            [Tester, Name]),
                          receive
                              {Ctrl, _Srv} = Procs ->
                                  {Node, Procs}
                          end
                  end, Nodes),
    SrvMap = maps:from_list(SrvList),

    Res0 = {lists:map(fun (Node) ->
                              {Node,ok}
                      end, Nodes), []},

    Res0 = gen_server:multi_call(Nodes, Name, started_p),

    true = try
               _ = gen_server:multi_call([Node1, Node2, Node3, node(), {Node4}],
                                         Name, {delayed_answer,1}),
               false
           catch
               _:_ ->
                   true
           end,

    Res1 = {lists:map(fun (Node) ->
                              {Node,delayed}
                      end, Nodes), []},

    Res1 = gen_server:multi_call(Nodes, Name, {delayed_answer,1}),

    Res2 = {[], Nodes},

    Start = erlang:monotonic_time(millisecond),
    Res2 = gen_server:multi_call(Nodes, Name, {delayed_answer,1000}, 100),
    End = erlang:monotonic_time(millisecond),
    Time = End-Start,
    ct:log("Time: ~p ms~n", [Time]),
    true = 200 >= Time,

    {Ctrl2, Srv2} = maps:get(Node2, SrvMap),
    unlink(Ctrl2),
    exit(Ctrl2, kill),
    wait_until(fun () ->
                       false == erpc:call(Node2, erlang,
                                          is_process_alive, [Srv2])
               end),

    {Ctrl3, _Srv3} = maps:get(Node3, SrvMap),
    unlink(Ctrl3),
    peer:stop(Peer3),

    {Ctrl4, Srv4} = maps:get(Node4, SrvMap),
    Spndr = spawn_link(Node4, ?MODULE, multicall_suspender, [Tester, Srv4]),

    Res3 = {[{Node1, delayed}, {ThisNode, delayed}],
            [Node2, Node3, Node4]},

    Res3 = gen_server:multi_call(Nodes, Name, {delayed_answer,1}, 1000),

    Spndr ! {Tester, resume_it},

    receive Msg -> ct:fail({unexpected_msg, Msg})
    after 1000 -> ok
    end,

    unlink(Ctrl4),

    {Ctrl1, _Srv1} = maps:get(Node1, SrvMap),

    unlink(Ctrl1),

    peer:stop(Peer1),
    peer:stop(Peer2),
    peer:stop(Peer4),

    ok.

multicall_srv_ctrlr(Tester, Name) ->
    {ok, Srv} = gen_server:start_link({local, Name},
                                      gen_server_SUITE, [], []),
    Tester ! {self(), Srv},
    receive after infinity -> ok end.

multicall_suspender(Tester, Suspendee) ->
    true = erlang:suspend_process(Suspendee),
    receive
        {Tester, resume_it} ->
            erlang:resume_process(Suspendee)
    end.

multicall_recv_opt_success(Config) when is_list(Config) ->
    multicall_recv_opt_test(success).

multicall_recv_opt_timeout(Config) when is_list(Config) ->
    multicall_recv_opt_test(timeout).

multicall_recv_opt_noconnection(Config) when is_list(Config) ->
    multicall_recv_opt_test(noconnection).

multicall_recv_opt_test(Type) ->
    Tester = self(),
    Name = ?FUNCTION_NAME,
    Loops = 1000,
    HugeMsgQ = 500000,
    process_flag(message_queue_data, off_heap),

    {ok, Peer1, Node1} = ?CT_PEER(),
    {ok, Peer2, Node2} = ?CT_PEER(),

    if Type == noconnection -> peer:stop(Peer2);
       true -> ok
    end,

    Nodes = [Node1, Node2],

    SrvList =
        lists:map(fun (Node) ->
                          Ctrl = spawn_link(Node, ?MODULE,
                                            multicall_srv_ctrlr,
                                            [Tester, Name]),
                          receive
                              {Ctrl, _Srv} = Procs ->
                                  {Node, Procs}
                          end
                  end,
                  if Type == noconnection -> [Node1];
                     true -> Nodes
                  end),

    {Req, ExpRes, Tmo} = case Type of
                             success ->
                                 {ping,
                                  {[{Node1, pong}, {Node2, pong}], []},
                                  infinity};
                             timeout ->
                                 {{delayed_answer,100},
                                  {[], Nodes},
                                  1};
                             noconnection ->
                                 {ping,
                                  {[{Node1, pong}], [Node2]},
                                  infinity}
                         end,

    _Warmup = time_multicall(ExpRes, Nodes, Name, Req, Tmo, Loops div 10),

    Empty = time_multicall(ExpRes, Nodes, Name, Req, Tmo, Loops),
    ct:log("Time with empty message queue: ~p microsecond~n",
           [erlang:convert_time_unit(Empty, native, microsecond)]),

    make_msgq(HugeMsgQ),

    Huge = time_multicall(ExpRes, Nodes, Name, Req, Tmo, Loops),
    ct:log("Time with huge message queue: ~p microsecond~n",
           [erlang:convert_time_unit(Huge, native, microsecond)]),

    lists:foreach(fun ({_Node, {Ctrl, _Srv}}) -> unlink(Ctrl) end, SrvList),

    peer:stop(Peer1),
    if Type == noconnection -> ok;
       true -> peer:stop(Peer2)
    end,

    Q = Huge / Empty,
    HugeMsgQ = flush_msgq(),
    case Q > 10 of
	true ->
	    ct:fail({ratio, Q});
	false ->
	    {comment, "Ratio: "++erlang:float_to_list(Q)}
    end.

time_multicall(Expect, Nodes, Name, Req, Tmo, Times) ->
    Start = erlang:monotonic_time(),
    ok = do_time_multicall(Expect, Nodes, Name, Req, Tmo, Times),
    erlang:monotonic_time() - Start.

do_time_multicall(_Expect, _Nodes, _Name, _Req, _Tmo, 0) ->
    ok;
do_time_multicall(Expect, Nodes, Name, Req, Tmo, N) ->
    Expect = gen_server:multi_call(Nodes, Name, Req, Tmo),
    do_time_multicall(Expect, Nodes, Name, Req, Tmo, N-1).

make_msgq(0) ->
    ok;
make_msgq(N) ->
    self() ! {a, msg},
    make_msgq(N-1).

%%--------------------------------------------------------------
%% Test gen_server:enter_loop/[3,4,5]. Used when you want to write
%% your own special init-phase.
spec_init(Config) when is_list(Config) ->

    OldFlag = process_flag(trap_exit, true),

    {ok, Pid0} = start_link(spec_init_local, [{ok, my_server}, []]),
    ok = gen_server:call(Pid0, started_p),
    ok = gen_server:call(Pid0, stop),
    receive 
	{'EXIT', Pid0, stopped} ->
 	    ok
    after 5000 ->
	    ct:fail(gen_server_did_not_die)
    end,

    {ok, Pid01} = start_link(spec_init_local, [{not_ok, my_server}, []]),
    receive 
 	{'EXIT', Pid01, process_not_registered} ->
 	    ok
    after 5000 ->
	    ct:fail(gen_server_did_not_die)
    end,

    {ok, Pid1} = start_link(spec_init_global, [{ok, my_server}, []]),
    ok = gen_server:call(Pid1, started_p),
    ok = gen_server:call(Pid1, stop),
    receive 
	{'EXIT', Pid1, stopped} ->
 	    ok
    after 5000 ->
	    ct:fail(gen_server_did_not_die)
    end,

    {ok, Pid11} =
	start_link(spec_init_global, [{not_ok, my_server}, []]),

    receive 
	{'EXIT', Pid11, process_not_registered_globally} ->
 	    ok
    after 5000 ->
	    ct:fail(gen_server_did_not_die)
    end,

    {ok, Pid2} = start_link(spec_init_anonymous, [[]]),
    ok = gen_server:call(Pid2, started_p),
    ok = gen_server:call(Pid2, stop),
    receive 
	{'EXIT', Pid2, stopped} ->
 	    ok
    after 5000 ->
	    ct:fail(gen_server_did_not_die)
    end,

    {ok, Pid3} = start_link(spec_init_anonymous_default_timeout, [[]]),
    ok = gen_server:call(Pid3, started_p),
    ok = gen_server:call(Pid3, stop),
    receive 
	{'EXIT', Pid3, stopped} ->
 	    ok
    after 5000 ->
	    ct:fail(gen_server_did_not_die)
    end,

    {ok, Pid4} =
	start_link(spec_init_default_timeout, [{ok, my_server}, []]),
    ok = gen_server:call(Pid4, started_p),
    ok = gen_server:call(Pid4, stop),
    receive 
	{'EXIT', Pid4, stopped} ->
 	    ok
    after 5000 ->
	    ct:fail(gen_server_did_not_die)
    end,

    %% Before the OTP-10130 fix this failed because a timeout message
    %% was generated as the spawned process crashed because a {global, Name}
    %% was matched as a timeout value instead of matching on scope.
    {ok, _PidHurra} =
	start_link(spec_init_global_default_timeout, [{ok, hurra}, []]),
    timer:sleep(1000),
    ok = gen_server:call(_PidHurra, started_p),

    Pid5 =
	erlang:spawn_link(?MODULE, spec_init_not_proc_lib, [[]]),
    receive 
	{'EXIT', Pid5, process_was_not_started_by_proc_lib} ->
 	    ok
    after 5000 ->
	    ct:fail(gen_server_did_not_die)
    end,
    process_flag(trap_exit, OldFlag),
    ok.

%%--------------------------------------------------------------
%% OTP-4820. Test that terminate is run when the parent is a locally
%% registered process.
spec_init_local_registered_parent(Config) when is_list(Config) ->

    register(foobar, self()),
    process_flag(trap_exit, true),

    {ok, Pid} = start_link(spec_init_local, [{ok, my_server}, []]),

    ok = gen_server:cast(my_server, {self(),stop}),
    receive
	{Pid, stopped} ->
	    ok
    after 1000 ->
	    ct:fail(stop)
    end,
    unregister(foobar),
    ok.

%%--------------------------------------------------------------
%% OTP-4820. Test that terminate is run when the parent is a global registered
%% process.
spec_init_global_registered_parent(Config) when is_list(Config) ->

    global:register_name(foobar, self()),
    process_flag(trap_exit, true),

    {ok, Pid} = start_link(spec_init_global, [{ok, my_server}, []]),

    ok = gen_server:call(Pid, started_p),
    ok = gen_server:cast(Pid, {self(),stop}),

    receive
	{Pid, stopped} ->
	    ok
    after 1000 ->
	    ct:fail(stop)
    end,
    global:unregister_name(foobar),
    ok.

%%--------------------------------------------------------------

%% Test check for registered name in enter_loop/3,4,5.
otp_5854(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),

    dummy_via:reset(),

    %% Make sure gen_server:enter_loop does not accept {local,Name}
    %% when it's another process than the calling one which is
    %% registered under that name
    register(armitage, self()),
    {ok, Pid1} =
	start_link(spec_init_local, [{not_ok, armitage}, []]),
    receive
	{'EXIT', Pid1, process_not_registered} ->
	    ok
    after 1000 ->
	    ct:fail(gen_server_started)
    end,
    unregister(armitage),

    %% Make sure gen_server:enter_loop does not accept {global,Name}
    %% when it's another process than the calling one which is
    %% registered under that name
    global:register_name(armitage, self()),
    {ok, Pid2} =
	start_link(spec_init_global, [{not_ok, armitage}, []]),
    receive
	{'EXIT', Pid2, process_not_registered_globally} ->
	    ok
    after 1000 ->
	    ct:fail(gen_server_started)
    end,
    global:unregister_name(armitage),

    %% (same for {via, Mod, Name})
    dummy_via:register_name(armitage, self()),
    {ok, Pid3} =
	start_link(spec_init_via, [{not_ok, armitage}, []]),
    receive
	{'EXIT', Pid3, {process_not_registered_via, dummy_via}} ->
	    ok
    after 1000 ->
	    ct:fail(gen_server_started)
    end,
    dummy_via:unregister_name(armitage),

    process_flag(trap_exit, OldFlag),
    ok.

%% If initialization fails (with ignore or {stop,Reason}),
%% make sure that the process is not registered when gen_server:start()
%% returns.

otp_7669(Config) when is_list(Config) ->
    do_times(100, fun do_otp_7669_local_ignore/0),
    do_times(100, fun do_otp_7669_global_ignore/0),
    do_times(10, fun do_otp_7669_stop/0),
    ok.    

do_times(0, _) ->
    ok;
do_times(N, Fun) ->
    Fun(),
    do_times(N-1, Fun).

do_otp_7669_local_ignore() ->
    %% The name should never be registered after the return
    %% from gen_server:start/3.
    ignore = gen_server:start({local,?MODULE}, ?MODULE, ignore, []),
    undefined = whereis(?MODULE),
    ignore = gen_server:start({local,?MODULE}, ?MODULE, ignore, []),
    undefined = whereis(?MODULE),
    ignore = gen_server:start_link({local,?MODULE}, ?MODULE, ignore, []),
    undefined = whereis(?MODULE).

do_otp_7669_global_ignore() ->
    ignore = gen_server:start({global,?MODULE}, ?MODULE, ignore, []),
    undefined = global:whereis_name(?MODULE),
    ignore = gen_server:start_link({global,?MODULE}, ?MODULE, ignore, []),
    undefined = global:whereis_name(?MODULE).

do_otp_7669_stop() ->
    %% The name should never be registered after the return
    %% from gen_server:start/3.
    {error,stopped} = gen_server:start({local,?MODULE},
				       ?MODULE, stop, []),
    undefined = whereis(?MODULE),

    {error,stopped} = gen_server:start({global,?MODULE},
				       ?MODULE, stop, []),
    undefined = global:whereis_name(?MODULE).

%% Verify that sys:get_status correctly calls our format_status/1,2 fun.
call_format_status(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),
    call_format_status(?MODULE, format_status_called),
    call_format_status(format_status_server,{data,[{"State",format_status_called}]}),
    process_flag(trap_exit, OldFl).
call_format_status(Module, Match) when is_atom(Module) ->

    Parent = self(),

    {ok, Pid} = gen_server:start_link({local, call_format_status},
				      Module, [], []),
    Status1 = sys:get_status(call_format_status),
    {status, Pid, Mod, [_Pdict1, running, Parent, _, Data1]} = Status1,
    [Match | _] = lists:reverse(Data1),
    Status2 = sys:get_status(call_format_status, 5000),
    {status, Pid, Mod, [_Pdict2, running, Parent, _, Data2]} = Status2,
    [Match | _] = lists:reverse(Data2),
    gen_server:call(Pid, stop),
    receive {'EXIT',_,_} -> ok end,

    %% check that format_status can handle a name being a pid (atom is
    %% already checked by the previous test)
    {ok, Pid3} = gen_server:start_link(Module, [], []),
    Status3 = sys:get_status(Pid3),
    {status, Pid3, Mod, [_PDict3, running, Parent, _, Data3]} = Status3,
    [Match | _] = lists:reverse(Data3),
    gen_server:call(Pid3, stop),
    receive {'EXIT',_,_} -> ok end,

    %% check that format_status can handle a name being a term other than a
    %% pid or atom
    GlobalName1 = {global, "CallFormatStatus"},
    {ok, Pid4} = gen_server:start_link(GlobalName1, Module, [], []),
    Status4 = sys:get_status(Pid4),
    {status, Pid4, Mod, [_PDict4, running, Parent, _, Data4]} = Status4,
    [Match | _] = lists:reverse(Data4),
    gen_server:call(Pid4, stop),
    receive {'EXIT',_,_} -> ok end,
    GlobalName2 = {global, {name, "term"}},
    {ok, Pid5} = gen_server:start_link(GlobalName2, Module, [], []),
    Status5 = sys:get_status(GlobalName2),
    {status, Pid5, Mod, [_PDict5, running, Parent, _, Data5]} = Status5,
    [Match | _] = lists:reverse(Data5),
    gen_server:call(Pid5, stop),
    receive {'EXIT',_,_} -> ok end,
    ok.

%% Verify that error termination correctly calls our format_status/1,2 fun.
error_format_status(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    error_format_status(?MODULE),
    error_format_status(format_status_server),
    process_flag(trap_exit, OldFl);
error_format_status(Module) when is_atom(Module) ->

    State = "called format_status",
    {ok, Pid} = gen_server:start_link(Module, {state, State}, []),
    {'EXIT',{crashed,_}} = (catch gen_server:call(Pid, crash)),
    receive
	{'EXIT', Pid, crashed} ->
	    ok
    end,
    ClientPid = self(),
    receive
	{error,_GroupLeader,{Pid,
			     "** Generic server"++_,
			     [Pid,crash,{formatted, State},
			      {crashed,[{?MODULE,handle_call,3,_}
					|_Stacktrace]},
			       ClientPid, [_|_] = _ClientStack]}} ->
	    ok;
	Other ->
	    ct:log("Unexpected: ~p", [Other]),
	    ct:fail(failed)
    end,
    receive
        {error_report,_,_} -> ok
    end,
    ok.

%% Verify that error when terminating correctly calls our format_status/1,2 fun
terminate_crash_format(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    terminate_crash_format(?MODULE),
    terminate_crash_format(format_status_server),
    process_flag(trap_exit, OldFl);
terminate_crash_format(Module) when is_atom(Module) ->
    State = crash_terminate,
    {ok, Pid} = gen_server:start_link(Module, {state, State}, []),
    gen_server:call(Pid, stop),
    receive {'EXIT', Pid, {crash, terminate}} -> ok end,
    ClientPid = self(),
    receive
	{error,_GroupLeader,{Pid,
			     "** Generic server"++_,
			     [Pid,stop, {formatted, State},
			      {{crash, terminate},
			       [{?MODULE,terminate,2,_}|_Stacktrace]},
			       ClientPid, [_|_] = _ClientStack]}} ->
	    ok;
	Other ->
	    io:format("Unexpected: ~p", [Other]),
	    ct:fail(failed)
    after 5000 ->
	    io:format("Timeout: expected error logger msg", []),
	    ct:fail(failed)
    end,
    receive
        {error_report,_,_} -> ok
    end,
    ok.

crash_in_format_status(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    crash_in_format_status(?MODULE, "gen_server_SUITE:format_status/2 crashed"),
    crash_in_format_status(format_status_server, "format_status_server:format_status/1 crashed"),
    process_flag(trap_exit, OldFl).
crash_in_format_status(Module, Match) when is_atom(Module) ->
    State = fun(_) -> exit({crash,format_status}) end,
    {ok, Pid} = gen_server:start_link(Module, {state, State}, []),

    {status,Pid, _, [_,_,_,_,Info]} = sys:get_status(Pid),
    {data,[{"State",Match}]} = lists:last(Info),

    gen_server:call(Pid, stop),
    receive {'EXIT', Pid, stopped} -> ok end,
    ClientPid = self(),
    receive
	{error,_GroupLeader,
         {Pid,
          "** Generic server"++_,
          [Pid, stop, Match, stopped,
           ClientPid, [_|_] = _ClientStack]}} ->
	    ok;
	Other ->
	    ct:log("Unexpected: ~p", [Other]),
	    ct:fail(failed)
    after 5000 ->
	    io:format("Timeout: expected error logger msg", []),
	    ct:fail(failed)
    end,
    receive
        {error_report,_,_} -> ok
    end,
    ok.

throw_in_format_status(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    throw_in_format_status(?MODULE,{throw,format_status}),
    throw_in_format_status(format_status_server,"format_status_server:format_status/1 crashed"),
    process_flag(trap_exit, OldFl).
throw_in_format_status(Module, Match) when is_atom(Module) ->
    State = fun(_) -> throw({throw,format_status}) end,
    {ok, Pid} = gen_server:start_link(Module, {state, State}, []),

    {status,Pid, _, [_,_,_,_,Info]} = sys:get_status(Pid),
    case lists:last(Info) of
        {data,[{"State",Match}]} ->
            ok;
        Match ->
            ok
    end,

    gen_server:call(Pid, stop),
    receive {'EXIT', Pid, stopped} -> ok end,
    ClientPid = self(),
    receive
	{error,_GroupLeader,
         {Pid, "** Generic server"++_,
          [Pid, stop, Match, stopped,
           ClientPid, [_|_] = _ClientStack]}} ->
	    ok;
	Other ->
	    ct:log("Unexpected: ~p", [Other]),
	    ct:fail(failed)
    after 5000 ->
	    io:format("Timeout: expected error logger msg", []),
	    ct:fail(failed)
    end,
    receive
        {error_report,_,_} -> ok
    end,
    ok.

%% Test that the state, reason, message format status calls works as they should
%% The test makes sure that both sys:get_status and the crash report works as they
%% should and can be used to strip data from both the reason, message, state and the
%% sys logger logs.

%%%% The sys logger log messages that should be matched
-define(LOG_MESSAGES,
        {log,{in,{_,_,started_p}}},
        {log,{out,ok,_,State}},
        {log,{in,{_,_,{delayed_answer,10}}}},
        {log,{noreply,{_,_,State}}},
        {log,{in,timeout}},
        {log,{noreply,State}}).

format_all_status(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),

    State = fun(M) ->
                    maps:map(
                      fun(log, Values) ->
                              [{log, Value} || Value <- Values];
                         (Key, Value) ->
                              {Key, Value}
                      end, M)
            end,
    {ok, Pid} = gen_server:start_link(format_status_server, {state, State}, []),
    sys:log(Pid, true),
    ok = gen_server:call(Pid, started_p),
    delayed = gen_server:call(Pid, {delayed_answer, 10}),

    {status,Pid, _, [_,_,_,_,Info]} = sys:get_status(Pid),
    [{header, _Hdr},
     {data, [_Status,_Parent,{"Logged events",LoggedEvents}]},
     {data, [{"State",{state,State}}]}] = Info,

    [?LOG_MESSAGES] = LoggedEvents,

    ok = gen_server:call(Pid, stop),
    receive {'EXIT', Pid, stopped} -> ok end,
    ClientPid = self(),
    receive
	{error,_GroupLeader,
         {Pid, "** Generic server"++_,
          [Pid, {message, stop}, {state,State}, {reason, stopped},
           ?LOG_MESSAGES, {log,{in,{_,_,stop}}},
           ClientPid, [_|_] = _ClientStack]}} ->
	    ok;
	Other ->
	    ct:log("Unexpected: ~p", [Other]),
	    ct:fail(failed)
    after 5000 ->
	    io:format("Timeout: expected error logger msg", []),
	    ct:fail(failed)
    end,
    receive
        {error_report,_,_} -> ok
    end,

    {ok, Pid2} = gen_server:start_link(format_status_server, {state, State}, []),
    catch gen_server:call(Pid2, crash),
    receive {'EXIT', Pid2, crashed} -> ok end,
    receive
	{error,_GroupLeader2,
         {Pid2, "** Generic server"++_,
          [Pid2, {message, crash}, {state,State},
           {{reason, crashed},[_|_] = _ServerStack},
           ClientPid, [_|_] = _ClientStack2]}} ->
	    ok;
	Other2 ->
	    ct:log("Unexpected: ~p", [Other2]),
	    ct:fail(failed)
    after 5000 ->
	    io:format("Timeout: expected error logger msg", []),
	    ct:fail(failed)
    end,
    receive
        {error_report,_,_} -> ok
    end,

    process_flag(trap_exit, OldFl).

%% Verify that sys:get_state correctly returns gen_server state
get_state(Config) when is_list(Config) ->
    State = self(),
    {ok, _Pid} = gen_server:start_link({local, get_state},
				       ?MODULE, {state,State}, []),
    State = sys:get_state(get_state),
    State = sys:get_state(get_state, 5000),
    {ok, Pid} = gen_server:start_link(?MODULE, {state,State}, []),
    State = sys:get_state(Pid),
    State = sys:get_state(Pid, 5000),
    ok = sys:suspend(Pid),
    State = sys:get_state(Pid),
    ok = sys:resume(Pid),
    ok.

%% Verify that sys:replace_state correctly replaces gen_server state
replace_state(Config) when is_list(Config) ->
    State = self(),
    {ok, _Pid} = gen_server:start_link({local, replace_state},
				       ?MODULE, {state,State}, []),
    State = sys:get_state(replace_state),
    NState1 = "replaced",
    Replace1 = fun(_) -> NState1 end,
    NState1 = sys:replace_state(replace_state, Replace1),
    NState1 = sys:get_state(replace_state),
    {ok, Pid} = gen_server:start_link(?MODULE, {state,NState1}, []),
    NState1 = sys:get_state(Pid),
    Suffix = " again",
    NState2 = NState1 ++ Suffix,
    Replace2 = fun(S) -> S ++ Suffix end,
    NState2 = sys:replace_state(Pid, Replace2, 5000),
    NState2 = sys:get_state(Pid, 5000),
    %% verify no change in state if replace function crashes
    Replace3 = fun(_) -> throw(fail) end,
    {'EXIT',{{callback_failed,
	      {gen_server,system_replace_state},{throw,fail}},_}} =
	(catch sys:replace_state(Pid, Replace3)),
    NState2 = sys:get_state(Pid, 5000),
    %% verify state replaced if process sys suspended
    ok = sys:suspend(Pid),
    Suffix2 = " and again",
    NState3 = NState2 ++ Suffix2,
    Replace4 = fun(S) -> S ++ Suffix2 end,
    NState3 = sys:replace_state(Pid, Replace4),
    ok = sys:resume(Pid),
    NState3 = sys:get_state(Pid, 5000),
    ok.

%% Test that the time for a huge message queue is not
%% significantly slower than with an empty message queue.
call_with_huge_message_queue(Config) when is_list(Config) ->
    Pid = spawn_link(fun echo_loop/0),

    {Time,ok} = tc(fun() -> calls(10000, Pid) end),

    _ = [self() ! {msg,N} || N <- lists:seq(1, 500000)],
    erlang:garbage_collect(),
    {NewTime,ok} = tc(fun() -> calls(10000, Pid) end),
    io:format("Time for empty message queue: ~p", [Time]),
    io:format("Time for huge message queue: ~p", [NewTime]),

    IsCover = test_server:is_cover(),
    case (NewTime+1) / (Time+1) of
	Q when Q < 10; IsCover ->
	    ok;
	Q ->
	    io:format("Q = ~p", [Q]),
	    ct:fail(failed)
    end,
    ok.

calls(0, _) -> ok;
calls(N, Pid) ->
    {ultimate_answer,42} = call(Pid, {ultimate_answer,42}),
    calls(N-1, Pid).

call(Pid, Msg) ->
    gen_server:call(Pid, Msg, infinity).

tc(Fun) ->
    timer:tc(erlang, apply, [Fun,[]]).

echo_loop() ->
    receive
	{'$gen_call',{Pid,Ref},Msg} ->
	    Pid ! {Ref,Msg},
	    echo_loop()
    end.

%% Test the default implementation of terminate if the callback module
%% does not export it
undef_terminate1(Config) when is_list(Config) ->
    {ok, Server} = oc_server:start(),
    MRef = monitor(process, Server),
    ok = gen_server:stop(Server),
    ok = verify_down_reason(MRef, Server, normal).

%% Test the default implementation of terminate if the callback module
%% does not export it
undef_terminate2(Config) when is_list(Config) ->
    {ok, Server} = oc_server:start(),
    MRef = monitor(process, Server),
    ok = gen_server:stop(Server, {error, test}, infinity),
    ok = verify_down_reason(MRef, Server, {error, test}).

%% Start should return an undef error if init isn't implemented
undef_init(_Config) ->
    {error, {undef, [{oc_init_server, init, [_], _}|_]}} =
        gen_server:start(oc_init_server, [], []),
    process_flag(trap_exit, true),
    {error, {undef, [{oc_init_server, init, [_], _}|_]}} =
        (catch gen_server:start_link(oc_init_server, [], [])),
    receive
        Msg ->
            ct:fail({unexpected_msg, Msg})
    after 500 ->
            ok
    end.

%% The upgrade should fail if code_change is expected in the callback module
%% but not exported, but the server should continue with the old code
undef_code_change(Config) when is_list(Config) ->
    {ok, Server} = oc_server:start(),
    {error, {'EXIT', {undef, [{oc_server, code_change, [_, _, _], _}|_]}}}
        = fake_upgrade(Server, ?MODULE),
    true = is_process_alive(Server).

%% The server should crash if the handle_call callback is
%% not exported in the callback module
undef_handle_call(_Config) ->
    {ok, Server} = oc_server:start(),
    try
        gen_server:call(Server, call_msg),
        ct:fail(should_crash)
    catch exit:{{undef, [{oc_server, handle_call, _, _}|_]},
                {gen_server, call, _}} ->
        ok
    end.

%% The server should crash if the handle_cast callback is
%% not exported in the callback module
undef_handle_cast(_Config) ->
    {ok, Server} = oc_server:start(),
    MRef = monitor(process, Server),
    gen_server:cast(Server, cast_msg),
    verify_undef_down(MRef, Server, oc_server, handle_cast),
    ok.

%% The server should crash if the handle_continue callback is
%% not exported in the callback module
undef_handle_continue(_Config) ->
    {ok, Server} = oc_server:start(continue),
    MRef = monitor(process, Server),
    verify_undef_down(MRef, Server, oc_server, handle_continue),
    ok.

%% The server should log but not crash if the handle_info callback is
%% calling an undefined function
undef_handle_info(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    {ok, Server} = oc_server:start(),
    Server ! hej,
    wait_until_processed(Server, hej, 10),
    true = is_process_alive(Server),
    receive
        {warning_msg, _GroupLeader,
         {Server, "** Undefined handle_info in " ++ _, [oc_server, hej]}} ->
            ok;
        Other ->
            io:format("Unexpected: ~p", [Other]),
            ct:fail(failed)
    end.

%% Test that the default implementation of terminate isn't catching the
%% wrong undef error
undef_in_terminate(Config) when is_list(Config) ->
    State = {undef_in_terminate, {oc_server, terminate}},
    {ok, Server} = gen_server:start(?MODULE, {state, State}, []),
    try
        ok = gen_server:stop(Server),
        ct:fail(failed)
    catch
        exit:{undef, [{oc_server, terminate, [], _}|_]} ->
            ok
    end.

%% Test that the default implementation of handle_info isn't catching the
%% wrong undef error
undef_in_handle_info(Config) when is_list(Config) ->
     {ok, Server} = gen_server:start(?MODULE, [], []),
     MRef = monitor(process, Server),
     Server ! {call_undef_fun, ?MODULE, handle_info},
     verify_undef_down(MRef, Server, ?MODULE, handle_info),
     ok.

verify_down_reason(MRef, Server, Reason) ->
    receive
        {'DOWN', MRef, process, Server, Reason} ->
            ok
    after 5000 ->
        ct:fail(failed)
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

wait_until_processed(_Pid, _Message, 0) ->
    ct:fail(not_processed);
wait_until_processed(Pid, Message, N) ->
    {messages, Messages} = erlang:process_info(Pid, messages),
    case lists:member(Message, Messages) of
        true ->
            timer:sleep(100),
            wait_until_processed(Pid, Message, N-1);
        false ->
            ok
    end.

%% Test report callback for Logger handler error_logger
format_log_1(_Config) ->
    FD = application:get_env(kernel,error_logger_format_depth),
    application:unset_env(kernel,error_logger_format_depth),
    Term = lists:seq(1,15),
    Name = self(),
    Report = #{label=>{gen_server,terminate},
               name=>Name,
               last_message=>Term,
               state=>Term,
               log=>[],
               reason=>Term,
               client_info=>{self(),{clientname,[]}},
               process_label=>undefined},
    {F1,A1} = gen_server:format_log(Report),
    FExpected1 = "** Generic server ~tp terminating \n"
        "** Last message in was ~tp~n"
        "** When Server state == ~tp~n"
        "** Reason for termination ==~n** ~tp~n"
        "** Client ~tp stacktrace~n"
        "** ~tp~n",
    ct:log("F1: ~ts~nA1: ~tp",[F1,A1]),
    FExpected1=F1,
    [Name,Term,Term,Term,clientname,[]] = A1,

    Warning = #{label=>{gen_server,no_handle_info},
                module=>?MODULE,
                message=>Term},
    {WF1,WA1} = gen_server:format_log(Warning),
    WFExpected1 = "** Undefined handle_info in ~p~n"
        "** Unhandled message: ~tp~n",
    ct:log("WF1: ~ts~nWA1: ~tp",[WF1,WA1]),
    WFExpected1=WF1,
    [?MODULE,Term] = WA1,

    Depth = 10,
    ok = application:set_env(kernel,error_logger_format_depth,Depth),
    Limited = [1,2,3,4,5,6,7,8,9,'...'],
    {F2,A2} = gen_server:format_log(#{label=>{gen_server,terminate},
                                      name=>Name,
                                      last_message=>Term,
                                      state=>Term,
                                      log=>[],
                                      reason=>Term,
                                      client_info=>{self(),{clientname,[]}},
                                      process_label=>undefined}),
    FExpected2 = "** Generic server ~tP terminating \n"
        "** Last message in was ~tP~n"
        "** When Server state == ~tP~n"
        "** Reason for termination ==~n** ~tP~n"
        "** Client ~tP stacktrace~n"
        "** ~tP~n",
    ct:log("F2: ~ts~nA2: ~tp",[F2,A2]),
    FExpected2=F2,
    [Name,Depth,Limited,Depth,Limited,Depth,Limited,Depth,
     clientname,Depth,[],Depth] = A2,

    {WF2,WA2} = gen_server:format_log(Warning),
    WFExpected2 = "** Undefined handle_info in ~p~n"
        "** Unhandled message: ~tP~n",
    ct:log("WF2: ~ts~nWA2: ~tp",[WF2,WA2]),
    WFExpected2=WF2,
    [?MODULE,Limited,Depth] = WA2,

    case FD of
        undefined ->
            application:unset_env(kernel,error_logger_format_depth);
        _ ->
            application:set_env(kernel,error_logger_format_depth,FD)
    end,
    ok.

%% Test report callback for any Logger handler
format_log_2(_Config) ->
    Term = lists:seq(1,15),
    Name = self(),
    NameStr = pid_to_list(Name),
    Report = #{label=>{gen_server,terminate},
               name=>Name,
               last_message=>Term,
               state=>Term,
               log=>[],
               reason=>Term,
               client_info=>{self(),{clientname,[]}},
               process_label=>undefined},
    FormatOpts1 = #{},
    Str1 = flatten_format_log(Report,FormatOpts1),
    L1 = length(Str1),
    Expected1 = "** Generic server "++NameStr++" terminating \n"
        "** Last message in was ",
    ct:log("Str1: ~ts",[Str1]),
    ct:log("length(Str1): ~p",[L1]),
    true = lists:prefix(Expected1,Str1),

    Warning = #{label=>{gen_server,no_handle_info},
                module=>?MODULE,
                message=>Term},
    WStr1 = flatten_format_log(Warning,FormatOpts1),
    WL1 = length(WStr1),
    WExpected1 = "** Undefined handle_info in gen_server_SUITE\n"
        "** Unhandled message: ",
    ct:log("WStr1: ~ts",[WStr1]),
    ct:log("length(WStr1): ~p",[WL1]),
    true = lists:prefix(WExpected1,WStr1),

    Depth = 10,
    FormatOpts2 = #{depth=>Depth},
    Str2 = flatten_format_log(Report,FormatOpts2),
    L2 = length(Str2),
    Expected2 = "** Generic server "++NameStr++" terminating \n"
        "** Last message in was ",
    ct:log("Str2: ~ts",[Str2]),
    ct:log("length(Str2): ~p",[L2]),
    true = lists:prefix(Expected2,Str2),
    true = L2<L1,

    WStr2 = flatten_format_log(Warning,FormatOpts2),
    WL2 = length(WStr2),
    WExpected2 = "** Undefined handle_info in gen_server_SUITE\n"
        "** Unhandled message: ",
    ct:log("WStr2: ~ts",[WStr2]),
    ct:log("length(WStr2): ~p",[WL2]),
    true = lists:prefix(WExpected2,WStr2),
    true = WL2<WL1,

    FormatOpts3 = #{chars_limit=>200},
    Str3 = flatten_format_log(Report,FormatOpts3),
    L3 = length(Str3),
    Expected3 = "** Generic server "++NameStr++" terminating \n"
        "** Last message in was ",
    ct:log("Str3: ~ts",[Str3]),
    ct:log("length(Str3): ~p",[L3]),
    true = lists:prefix(Expected3,Str3),
    true = L3<L1,

    WFormatOpts3 = #{chars_limit=>80},
    WStr3 = flatten_format_log(Warning,WFormatOpts3),
    WL3 = length(WStr3),
    WExpected3 = "** Undefined handle_info in gen_server_SUITE\n"
        "** Unhandled message: ",
    ct:log("WStr3: ~ts",[WStr3]),
    ct:log("length(WStr3): ~p",[WL3]),
    true = lists:prefix(WExpected3,WStr3),
    true = WL3<WL1,

    FormatOpts4 = #{single_line=>true},
    Str4 = flatten_format_log(Report,FormatOpts4),
    L4 = length(Str4),
    Expected4 = "Generic server "++NameStr++" terminating. Reason: ",
    ct:log("Str4: ~ts",[Str4]),
    ct:log("length(Str4): ~p",[L4]),
    true = lists:prefix(Expected4,Str4),
    true = L4<L1,

    WStr4 = flatten_format_log(Warning,FormatOpts4),
    WL4 = length(WStr4),
    WExpected4 = "Undefined handle_info in gen_server_SUITE. "
        "Unhandled message: ",
    ct:log("WStr4: ~ts",[WStr4]),
    ct:log("length(WStr4): ~p",[WL4]),
    true = lists:prefix(WExpected4,WStr4),
    true = WL4<WL1,

    FormatOpts5 = #{single_line=>true, depth=>Depth},
    Str5 = flatten_format_log(Report,FormatOpts5),
    L5 = length(Str5),
    Expected5 = "Generic server "++NameStr++" terminating. Reason: ",
    ct:log("Str5: ~ts",[Str5]),
    ct:log("length(Str5): ~p",[L5]),
    true = lists:prefix(Expected5,Str5),
    true = L5<L4,

    WStr5 = flatten_format_log(Warning,FormatOpts5),
    WL5 = length(WStr5),
    WExpected5 = "Undefined handle_info in gen_server_SUITE. "
        "Unhandled message: ",
    ct:log("WStr5: ~ts",[WStr5]),
    ct:log("length(WStr5): ~p",[WL5]),
    true = lists:prefix(WExpected5,WStr5),
    true = WL5<WL4,

    FormatOpts6 = #{single_line=>true, chars_limit=>200},
    Str6 = flatten_format_log(Report,FormatOpts6),
    L6 = length(Str6),
    Expected6 = "Generic server "++NameStr++" terminating. Reason: ",
    ct:log("Str6: ~ts",[Str6]),
    ct:log("length(Str6): ~p",[L6]),
    true = lists:prefix(Expected6,Str6),
    true = L6<L4,

    WFormatOpts6 = #{single_line=>true, chars_limit=>80},
    WStr6 = flatten_format_log(Warning,WFormatOpts6),
    WL6 = length(WStr6),
    WExpected6 = "Undefined handle_info in gen_server_SUITE. "
        "Unhandled message: ",
    ct:log("WStr6: ~ts",[WStr6]),
    ct:log("length(WStr6): ~p",[WL6]),
    true = lists:prefix(WExpected6,WStr6),
    true = WL6<WL4,

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
    LastMsg = dummy_msg,
    Reason = dummy_reason,
    State = dummy_state,
    Report = #{label=>{gen_server,terminate},
               name=>Name,
               last_message=>LastMsg,
               state=>State,
               log=>[],
               reason=>Reason,
               client_info=>{self(),{clientname,[]}},
               process_label=>ProcessLabel},

    %% multiple and single line (unlimited depth)

    {F1,A1} = gen_server:format_log(Report),
    FExpected1 = "** Generic server ~tp terminating \n"
        "** Process label == ~tp~n"
        "** Last message in was ~tp~n"
        "** When Server state == ~tp~n"
        "** Reason for termination ==~n** ~tp~n"
        "** Client ~tp stacktrace~n"
        "** ~tp~n",
    ct:log("F1: ~ts~nA1: ~tp",[F1,A1]),
    FExpected1=F1,
    [Name,ProcessLabel,LastMsg,State,Reason,clientname,[]] = A1,

    FormatOpts2 = #{single_line=>true},
    Str2 = flatten_format_log(Report, FormatOpts2),
    Expected2 = "Generic server "++NameStr++" terminating. "
        "Label: {some_id,#{term => [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]}}. "
        "Reason: dummy_reason. "
        "Last message: dummy_msg. "
        "State: dummy_state. "
        "Client clientname stacktrace: [].",
    ct:log("Str2: ~ts", [Str2]),
    true = Expected2 =:= Str2,

    %% multiple and single line (depth-limited)

    Depth = 10,
    FormatOpts3 = #{depth=>Depth},
    Str3 = flatten_format_log(Report, FormatOpts3),
    Expected3 = "** Generic server " ++ NameStr ++ " terminating \n"
        "** Process label == {some_id,#{term => [1,2,3,4,5,6|...]}}\n"
        "** Last message in was ",
    ct:log("Str3: ~ts",[Str3]),
    true = lists:prefix(Expected3,Str3),

    FormatOpts4 = #{single_line=>true, depth=>Depth},
    Str4 = flatten_format_log(Report, FormatOpts4),
    Expected4 = "Generic server "++NameStr++" terminating. "
        "Label: {some_id,#{term => [1,2,3,4,5,6|...]}}. "
        "Reason: ",
    ct:log("Str4: ~ts",[Str4]),
    true = lists:prefix(Expected4,Str4),

    case FD of
        undefined ->
            application:unset_env(kernel, error_logger_format_depth);
        _ ->
            application:set_env(kernel, error_logger_format_depth, FD)
    end,
    ok.

flatten_format_log(Report, Format) ->
    lists:flatten(gen_server:format_log(Report, Format)).

reply_by_alias_with_payload(Config) when is_list(Config) ->
    %% "Payload" version of tag not used yet, but make sure
    %% gen_server:reply/2 works with it...
    %%
    %% Whitebox...
    Reply = make_ref(),
    Alias = alias(),
    Tag = [[alias|Alias], "payload"],
    spawn_link(fun () ->
                       gen_server:reply({undefined, Tag},
                                        Reply)
               end),
    receive
        {[[alias|Alias]|_] = Tag, Reply} ->
            ok
    end,
    %% Check gen:reply/2 as well...
    Reply2 = make_ref(),
    Alias2 = alias(),
    Tag2 = [[alias|Alias2], "payload"],
    spawn_link(fun () ->
                       gen:reply({undefined, Tag2},
                                 Reply2)
               end),
    receive
        {[[alias|Alias2]|_] = Tag2, Reply2} ->
            ok
    end.

%%--------------------------------------------------------------
%% Help functions to spec_init_*
start_link(Init, Options) ->
    proc_lib:start_link(?MODULE, Init, Options).

spec_init_local({ok, Name}, Options) ->
    process_flag(trap_exit, true),
    register(Name, self()),
    proc_lib:init_ack({ok, self()}),
    %% Supervised init can occur here  ...
    gen_server:enter_loop(?MODULE, Options, {}, {local, Name}, infinity);

spec_init_local({not_ok, Name}, Options) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack({ok, self()}),
    %% Supervised init can occur here  ...
    gen_server:enter_loop(?MODULE, Options, {}, {local, Name}, infinity).

spec_init_global({ok, Name}, Options) ->
    process_flag(trap_exit, true),
    global:register_name(Name, self()),
    proc_lib:init_ack({ok, self()}),
    %% Supervised init can occur here  ...
    gen_server:enter_loop(?MODULE, Options, {}, {global, Name}, infinity);

spec_init_global({not_ok, Name}, Options) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack({ok, self()}),
    %% Supervised init can occur here  ...
    gen_server:enter_loop(?MODULE, Options, {}, {global, Name}, infinity).

spec_init_via({ok, Name}, Options) ->
    process_flag(trap_exit, true),
    dummy_via:register_name(Name, self()),
    proc_lib:init_ack({ok, self()}),
    %% Supervised init can occur here  ...
    gen_server:enter_loop(?MODULE, Options, {},
			  {via, dummy_via, Name}, infinity);

spec_init_via({not_ok, Name}, Options) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack({ok, self()}),
    %% Supervised init can occur here  ...
    gen_server:enter_loop(?MODULE, Options, {},
			  {via, dummy_via, Name}, infinity).

spec_init_default_timeout({ok, Name}, Options) ->
    process_flag(trap_exit, true),
    register(Name, self()),
    proc_lib:init_ack({ok, self()}),
    %% Supervised init can occur here  ...
    gen_server:enter_loop(?MODULE, Options, {}, {local, Name}).

%% OTP-10130, A bug was introduced where global scope was not matched when
%% enter_loop/4 was called (no timeout).
spec_init_global_default_timeout({ok, Name}, Options) ->
    process_flag(trap_exit, true),
    global:register_name(Name, self()),
    proc_lib:init_ack({ok, self()}),
    %% Supervised init can occur here  ...
    gen_server:enter_loop(?MODULE, Options, {}, {global, Name}).

spec_init_anonymous(Options) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack({ok, self()}),
    %% Supervised init can occur here  ...
    gen_server:enter_loop(?MODULE, Options, {}, infinity).

spec_init_anonymous_default_timeout(Options) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack({ok, self()}),
    %% Supervised init can occur here  ...
    gen_server:enter_loop(?MODULE, Options, {}).

spec_init_not_proc_lib(Options) ->
    gen_server:enter_loop(?MODULE, Options, {}, infinity).

spec_init_action(Options, Arg) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack({ok, self()}),
    {ok, State, Action} = init(Arg),
    gen_server:enter_loop(?MODULE, Options, State, Action).

%%% --------------------------------------------------------
%%% Here is the tested gen_server behaviour.
%%% --------------------------------------------------------

init([]) ->
    {ok, []};
init(ignore) ->
    io:format("init(ignore)~n"),
    ignore;
init({error, Reason}) ->
    io:format("init(error) -> ~w~n", [Reason]),
    {error, Reason};
init(stop) ->
    io:format("init(stop)~n"),
    {stop, stopped};
init({timeout, T, Pid}) ->
    {ok, [], {timeout, T, {event_timeout, Pid}}};
init({timeout, T, Pid, relative}) ->
    {ok, [], {timeout, T, {event_timeout, Pid}, [{abs, false}]}};
init({timeout, T, Pid, absolute}) ->
    {ok, [], {timeout, erlang:monotonic_time(millisecond) + T, {event_timeout, Pid}, [{abs, true}]}};
init({continue_timeout, T, Pid}) ->
    {ok, [], {continue, {timeout, T, Pid}}};
init({continue_timeout, T, Pid, Abs}) ->
    {ok, [], {continue, {timeout, T, Pid, Abs}}};
init({hibernate, T, Pid}) ->
    {ok, [], {hibernate, T, {event_timeout, Pid}}};
init({hibernate, T, Pid, relative}) ->
    {ok, [], {hibernate, T, {event_timeout, Pid}, [{abs, false}]}};
init({hibernate, T, Pid, absolute}) ->
    {ok, [], {hibernate, erlang:monotonic_time(millisecond) + T, {event_timeout, Pid}, [{abs, true}]}};
init({continue_hibernate, T, Pid}) ->
    {ok, [], {continue, {hibernate, T, Pid}}};
init({continue_hibernate, T, Pid, Abs}) ->
    {ok, [], {continue, {hibernate, T, Pid, Abs}}};
init({timeout_zero, Pid}) ->
    self() ! {after_event_timeout_zero, Pid},
    {ok, [], {timeout, 0, {event_timeout, Pid}}};
init({continue_timeout_zero, Pid}) ->
    {ok, [], {continue, {timeout_zero, Pid}}};
init({hibernate_zero, Pid}) ->
    self() ! {after_event_timeout_zero, Pid},
    {ok, [], {hibernate, 0, {event_timeout, Pid}}};
init({continue_hibernate_zero, Pid}) ->
    {ok, [], {continue, {hibernate_zero, Pid}}};
init(hibernate) ->
    io:format("init(hibernate)~n"),
    {ok,[],hibernate};
init(sleep) ->
    io:format("init(sleep)~n"),
    ct:sleep(1000),
    {ok, []};
init({continue, Pid}) ->
    io:format("init(continue) -> ~p~n", [Pid]),
    self() ! {after_continue, Pid},
    {ok, [], {continue, {message, Pid}}};
init({state,State}) ->
    io:format("init(state) -> ~p~n", [State]),
    {ok,State};
init({ets,InitResult}) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    case InitResult of
        {return, Value} ->
            Value;
        {crash, Class, Reason} ->
            erlang:Class(Reason);
        {wait, Time, Value} ->
            receive after Time -> Value end
    end.

handle_call(started_p, _From, State) ->
    io:format("FROZ"),
    {reply,ok,State};
handle_call(ping, _From, State) ->
    {reply,pong,State};
handle_call({timeout, T, Pid}, _From, State) ->
    {reply, ok, State, {timeout, T, {event_timeout, Pid}}};
handle_call({timeout, T, Pid, relative}, _From, State) ->
    {reply, ok, State, {timeout, T, {event_timeout, Pid}, [{abs, false}]}};
handle_call({timeout, T, Pid, absolute}, _From, State) ->
    {reply, ok, State, {timeout, erlang:monotonic_time(millisecond) + T, {event_timeout, Pid}, [{abs, true}]}};
handle_call({continue_timeout, T, Pid}, _From, State) ->
    {reply, ok, State, {continue, {timeout, T, Pid}}};
handle_call({continue_timeout, T, Pid, Abs}, _From, State) ->
    {reply, ok, State, {continue, {timeout, T, Pid, Abs}}};
handle_call({hibernate, T, Pid}, _From, State) ->
    {reply, ok, State, {hibernate, T, {event_timeout, Pid}}};
handle_call({hibernate, T, Pid, relative}, _From, State) ->
    {reply, ok, State, {hibernate, T, {event_timeout, Pid}, [{abs, false}]}};
handle_call({hibernate, T, Pid, absolute}, _From, State) ->
    {reply, ok, State, {hibernate, erlang:monotonic_time(millisecond) + T, {event_timeout, Pid}, [{abs, true}]}};
handle_call({continue_hibernate, T, Pid}, _From, State) ->
    {reply, ok, State, {continue, {hibernate, T, Pid}}};
handle_call({continue_hibernate, T, Pid, Abs}, _From, State) ->
    {reply, ok, State, {continue, {hibernate, T, Pid, Abs}}};
handle_call({timeout_zero, Pid}, _From, State) ->
    self() ! {after_event_timeout_zero, Pid},
    {reply, ok, State, {timeout, 0, {event_timeout, Pid}}};
handle_call({continue_timeout_zero, Pid}, _From, State) ->
    {reply, ok, State, {continue, {timeout_zero, Pid}}};
handle_call({hibernate_zero, Pid}, _From, State) ->
    self() ! {after_event_timeout_zero, Pid},
    {reply, ok, State, {hibernate, 0, {event_timeout, Pid}}};
handle_call({continue_hibernate_zero, Pid}, _From, State) ->
    {reply, ok, State, {continue, {hibernate_zero, Pid}}};
handle_call({delayed_answer, T}, From, State) ->
    {noreply,{reply_to,From,State},T};
handle_call({call_within, T}, _From, _) ->
    {reply,ok,call_within,T};
handle_call(next_call, _From, call_within) ->
    {reply,ok,[]};
handle_call(next_call, _From, State) ->
    io:format("handle_call(next_call) -> State: ~p~n", [State]),
    {reply,false,State};
handle_call(badreturn, _From, _State) ->
    badreturn;
handle_call(hibernate, _From, _State) ->
    {reply,true,[],hibernate};
handle_call({hibernate_noreply,Pid}, From, _State) ->
    Pid ! go,
    {noreply,From,hibernate};
handle_call(stop, _From, State) ->
    {stop,stopped,ok,State};
handle_call(crash, _From, _State) ->
    exit(crashed);
handle_call(exit_shutdown, _From, _State) ->
    exit(shutdown);
handle_call(stop_shutdown, _From, State) ->
    {stop,shutdown,State};
handle_call(shutdown_reason, _From, _State) ->
    exit({shutdown,reason});
handle_call({call_undef_fun, Mod, Fun}, _From, State) ->
    Mod:Fun(),
    {reply, ok, State};
handle_call({continue_reply, Pid}, _From, State) ->
    self() ! {after_continue, Pid},
    {reply, ok, State, {continue, {message, Pid}}};
handle_call({continue_noreply, Pid}, From, State) ->
    self() ! {after_continue, Pid},
    {noreply, State, {continue, {message, Pid, From}}};
handle_call(stop_shutdown_reason, _From, State) ->
    {stop,{shutdown,stop_reason},State}.

handle_cast({timeout, T, Pid}, State) ->
    {noreply, State, {timeout, T, {event_timeout, Pid}}};
handle_cast({timeout, T, Pid, relative}, State) ->
    {noreply, State, {timeout, T, {event_timeout, Pid}, [{abs, false}]}};
handle_cast({timeout, T, Pid, absolute}, State) ->
    {noreply, State, {timeout, erlang:monotonic_time(millisecond) + T, {event_timeout, Pid}, [{abs, true}]}};
handle_cast({continue_timeout, T, Pid}, State) ->
    {noreply, State, {continue, {timeout, T, Pid}}};
handle_cast({continue_timeout, T, Pid, Abs}, State) ->
    {noreply, State, {continue, {timeout, T, Pid, Abs}}};
handle_cast({hibernate, T, Pid}, State) ->
    {noreply, State, {hibernate, T, {event_timeout, Pid}}};
handle_cast({hibernate, T, Pid, relative}, State) ->
    {noreply, State, {hibernate, T, {event_timeout, Pid}, [{abs, false}]}};
handle_cast({hibernate, T, Pid, absolute}, State) ->
    {noreply, State, {hibernate, erlang:monotonic_time(millisecond) + T, {event_timeout, Pid}, [{abs, true}]}};
handle_cast({continue_hibernate, T, Pid}, State) ->
    {noreply, State, {continue, {hibernate, T, Pid}}};
handle_cast({continue_hibernate, T, Pid, Abs}, State) ->
    {noreply, State, {continue, {hibernate, T, Pid, Abs}}};
handle_cast({timeout_zero, Pid}, State) ->
    self() ! {after_event_timeout_zero, Pid},
    {noreply, State, {timeout, 0, {event_timeout, Pid}}};
handle_cast({continue_timeout_zero, Pid}, State) ->
    {noreply, State, {continue, {timeout_zero, Pid}}};
handle_cast({hibernate_zero, Pid}, State) ->
    self() ! {after_event_timeout_zero, Pid},
    {noreply, State, {hibernate, 0, {event_timeout, Pid}}};
handle_cast({continue_hibernate_zero, Pid}, State) ->
    {noreply, State, {continue, {hibernate_zero, Pid}}};
handle_cast({From,handle_cast}, State) ->
    From ! {self(), handled_cast},
    {noreply, State};
handle_cast({From,delayed_cast,T}, _State) ->
    {noreply, {delayed_cast,From}, T};
handle_cast(hibernate_now, _State) ->
    {noreply, [], hibernate};
handle_cast(hibernate_later, _State) ->
    {ok, _} = timer:send_after(1000,self(),hibernate_now),
    {noreply, []};
handle_cast({call_undef_fun, Mod, Fun}, State) ->
    Mod:Fun(),
    {noreply, State};
handle_cast({continue_noreply, Pid}, State) ->
    self() ! {after_continue, Pid},
    {noreply, State, {continue, {message, Pid}}};
handle_cast({From, stop}, State) ->
    io:format("BAZ"),
    {stop, {From,stopped}, State}.

handle_info({timeout, T, Pid}, State) ->
    {noreply, State, {timeout, T, {event_timeout, Pid}}};
handle_info({timeout, T, Pid, relative}, State) ->
    {noreply, State, {timeout, T, {event_timeout, Pid}, [{abs, false}]}};
handle_info({timeout, T, Pid, absolute}, State) ->
    {noreply, State, {timeout, erlang:monotonic_time(millisecond) + T, {event_timeout, Pid}, [{abs, true}]}};
handle_info({continue_timeout, T, Pid}, State) ->
    {noreply, State, {continue, {timeout, T, Pid}}};
handle_info({continue_timeout, T, Pid, Abs}, State) ->
    {noreply, State, {continue, {timeout, T, Pid, Abs}}};
handle_info({hibernate, T, Pid}, State) ->
    {noreply, State, {hibernate, T, {event_timeout, Pid}}};
handle_info({hibernate, T, Pid, relative}, State) ->
    {noreply, State, {hibernate, T, {event_timeout, Pid}, [{abs, false}]}};
handle_info({hibernate, T, Pid, absolute}, State) ->
    {noreply, State, {hibernate, erlang:monotonic_time(millisecond) + T, {event_timeout, Pid}, [{abs, true}]}};
handle_info({continue_hibernate, T, Pid}, State) ->
    {noreply, State, {continue, {hibernate, T, Pid}}};
handle_info({continue_hibernate, T, Pid, Abs}, State) ->
    {noreply, State, {continue, {hibernate, T, Pid, Abs}}};
handle_info({timeout_zero, Pid}, State) ->
    self() ! {after_event_timeout_zero, Pid},
    {noreply, State, {timeout, 0, {event_timeout, Pid}}};
handle_info({continue_timeout_zero, Pid}, State) ->
    {noreply, State, {continue, {timeout_zero, Pid}}};
handle_info({hibernate_zero, Pid}, State) ->
    self() ! {after_event_timeout_zero, Pid},
    {noreply, State, {hibernate, 0, {event_timeout, Pid}}};
handle_info({continue_hibernate_zero, Pid}, State) ->
    {noreply, State, {continue, {hibernate_zero, Pid}}};
handle_info({event_timeout, Pid}, State) ->
    Pid ! {self(), event_timeout},
    {noreply, State};
handle_info({after_event_timeout_zero, Pid}, State) ->
    Pid ! {self(), after_event_timeout_zero},
    {noreply, State};
handle_info(timeout, {reply_to, From, State}) ->
    gen_server:reply(From, delayed),
    {noreply, State};
handle_info(timeout, hibernate_me) -> % Arrive here from 
						% handle_info(hibernate_later,...)
    {noreply, [], hibernate};
handle_info(hibernate_now, _State) ->  % Arrive here from 
						% handle_cast({_,hibernate_later},...)
						% and by direct ! from testcase
    {noreply, [], hibernate};
handle_info(hibernate_later, _State) ->
    {noreply, hibernate_me, 1000};
handle_info(timeout, call_within) ->
    {noreply, []};
handle_info(timeout, {delayed_cast, From}) ->
    From ! {self(), delayed},
    {noreply, []};
handle_info(timeout, {delayed_info, From}) ->
    From ! {self(), delayed_info},
    {noreply, []};
handle_info({call_undef_fun, Mod, Fun}, State) ->
    Mod:Fun(),
    {noreply, State};
handle_info({From, handle_info}, _State) ->
    From ! {self(), handled_info},
    {noreply, []};
handle_info({From, delayed_info, T}, _State) ->
    {noreply, {delayed_info, From}, T};
handle_info(continue, From) ->
    gen_server:reply(From,true),
    {noreply, []};
handle_info({From, stop}, State) ->
    {stop, {From,stopped_info}, State};
handle_info({after_continue, Pid}, State) ->
    Pid ! {self(), after_continue},
    Pid ! {self(), ack},
    {noreply, State};
handle_info({continue_noreply, Pid}, State) ->
    self() ! {after_continue, Pid},
    {noreply, State, {continue, {message, Pid}}};
handle_info({continue_continue, Pid}, State) ->
    {noreply, State, {continue, {continue, Pid}}};
handle_info(continue_stop, State) ->
    {noreply, State, {continue, stop}};
handle_info(_Info, State) ->
    {noreply, State}.

handle_continue({timeout, T, Pid}, State) ->
    {noreply, State, {timeout, T, {event_timeout, Pid}}};
handle_continue({timeout, T, Pid, relative}, State) ->
    {noreply, State, {timeout, T, {event_timeout, Pid}, [{abs, false}]}};
handle_continue({timeout, T, Pid, absolute}, State) ->
    {noreply, State, {timeout, erlang:monotonic_time(millisecond) + T, {event_timeout, Pid}, [{abs, true}]}};
handle_continue({hibernate, T, Pid}, State) ->
    {noreply, State, {hibernate, T, {event_timeout, Pid}}};
handle_continue({hibernate, T, Pid, relative}, State) ->
    {noreply, State, {hibernate, T, {event_timeout, Pid}, [{abs, false}]}};
handle_continue({hibernate, T, Pid, absolute}, State) ->
    {noreply, State, {hibernate, erlang:monotonic_time(millisecond) + T, {event_timeout, Pid}, [{abs, true}]}};
handle_continue({timeout_zero, Pid}, State) ->
    self() ! {after_event_timeout_zero, Pid},
    {noreply, State, {timeout, 0, {event_timeout, Pid}}};
handle_continue({hibernate_zero, Pid}, State) ->
    self() ! {after_event_timeout_zero, Pid},
    {noreply, State, {hibernate, 0, {event_timeout, Pid}}};
handle_continue({continue, Pid}, State) ->
    Pid ! {self(), before_continue},
    self() ! {after_continue, Pid},
    {noreply, State, {continue, {message, Pid}}};
handle_continue(stop, State) ->
    {stop, normal, State};
handle_continue({message, Pid}, State) ->
    Pid ! {self(), continue},
    {noreply, State};
handle_continue({message, Pid, From}, State) ->
    Pid ! {self(), continue},
    gen_server:reply(From, ok),
    {noreply, State}.

code_change(_OldVsn,
            {new, {undef_in_code_change, {Mod, Fun}}} = State,
            _Extra) ->
    Mod:Fun(),
    {ok, State}.

terminate({From, stopped}, _State) ->
    io:format("FOOBAR"),
    From ! {self(), stopped},
    ok;
terminate({From, stopped_info}, _State) ->
    From ! {self(), stopped_info},
    ok;
terminate(_, crash_terminate) ->
    exit({crash, terminate});
terminate(_, {undef_in_terminate, {Mod, Fun}}) ->
    Mod:Fun(),
    ok;
terminate(_Reason, _State) ->
    ok.

format_status(_, [_PDict, Fun] = S) when is_function(Fun) ->
    Fun(S);
format_status(terminate, [_PDict, State]) ->
    {formatted, State};
format_status(normal, [_PDict, _State]) ->
    format_status_called.

%% Utils...

wait_until(Fun) ->
    case catch Fun() of
        true ->
            ok;
        _ ->
            receive after 100 -> ok end,
            wait_until(Fun)
    end.

old_release(N) ->
    OldRel = integer_to_list(list_to_integer(erlang:system_info(otp_release))-N),
    {OldRel++"_latest", OldRel}.

flush_msgq() ->
    flush_msgq(0).
flush_msgq(N) ->
    receive
	_ ->
	    flush_msgq(N+1)
    after 0 ->
	    N
    end.
