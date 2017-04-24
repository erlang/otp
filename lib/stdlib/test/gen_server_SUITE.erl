%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-export([start/1, crash/1, call/1, cast/1, cast_fast/1,
	 info/1, abcast/1, multicall/1, multicall_down/1,
	 call_remote1/1, call_remote2/1, call_remote3/1,
	 call_remote_n1/1, call_remote_n2/1, call_remote_n3/1, spec_init/1,
	 spec_init_local_registered_parent/1, 
	 spec_init_global_registered_parent/1,
	 otp_5854/1, hibernate/1, otp_7669/1, call_format_status/1,
	 error_format_status/1, terminate_crash_format/1,
	 get_state/1, replace_state/1, call_with_huge_message_queue/1,
	 undef_handle_call/1, undef_handle_cast/1, undef_handle_info/1,
	 undef_init/1, undef_code_change/1, undef_terminate1/1,
	 undef_terminate2/1, undef_in_terminate/1, undef_in_handle_info/1
	]).

-export([stop1/1, stop2/1, stop3/1, stop4/1, stop5/1, stop6/1, stop7/1,
	 stop8/1, stop9/1, stop10/1]).

%% spawn export
-export([spec_init_local/2, spec_init_global/2, spec_init_via/2,
	 spec_init_default_timeout/2, spec_init_global_default_timeout/2,
         spec_init_anonymous/1,
	 spec_init_anonymous_default_timeout/1,
	 spec_init_not_proc_lib/1, cast_fast_messup/0]).


%% The gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3, terminate/2, format_status/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [start, {group,stop}, crash, call, cast, cast_fast, info, abcast,
     multicall, multicall_down, call_remote1, call_remote2,
     call_remote3, call_remote_n1, call_remote_n2,
     call_remote_n3, spec_init,
     spec_init_local_registered_parent,
     spec_init_global_registered_parent, otp_5854, hibernate,
     otp_7669,
     call_format_status, error_format_status, terminate_crash_format,
     get_state, replace_state,
     call_with_huge_message_queue, {group, undef_callbacks},
     undef_in_terminate, undef_in_handle_info].

groups() -> 
    [{stop, [],
      [stop1, stop2, stop3, stop4, stop5, stop6, stop7, stop8, stop9, stop10]},
     {undef_callbacks, [],
      [undef_handle_call, undef_handle_cast, undef_handle_info,
       undef_init, undef_code_change, undef_terminate1, undef_terminate2]}].


init_per_suite(Config) ->
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
				     Case == call_remote_n3 ->
    {ok,N} = start_node(hubba),
    [{node,N} | Config];

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    case proplists:get_value(node, Config) of
	undefined ->
	    ok;
	N ->
	    test_server:stop_node(N)
    end,
    ok.


%% --------------------------------------
%% Start and stop a gen_server.
%% --------------------------------------

start(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    %% anonymous
    {ok, Pid0} = gen_server:start(gen_server_SUITE, [], []),
    ok = gen_server:call(Pid0, started_p),
    ok = gen_server:call(Pid0, stop),
    busy_wait_for_process(Pid0,600),
    {'EXIT', {noproc,_}} = (catch gen_server:call(Pid0, started_p, 1)),

    %% anonymous with timeout
    {ok, Pid00} = gen_server:start(gen_server_SUITE, [],
				   [{timeout,1000}]),
    ok = gen_server:call(Pid00, started_p),
    ok = gen_server:call(Pid00, stop),
    {error, timeout} = gen_server:start(gen_server_SUITE, sleep,
					[{timeout,100}]),

    %% anonymous with ignore
    ignore = gen_server:start(gen_server_SUITE, ignore, []),

    %% anonymous with stop
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
    {ok,Node} = test_server:start_node(gen_server_SUITE_stop8,slave,[]),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node,code,add_path,[Dir]),
    {ok, Pid} = rpc:call(Node,gen_server,start,[?MODULE,[],[]]),
    ok = gen_server:stop(Pid),
    false = rpc:call(Node,erlang,is_process_alive,[Pid]),
    {'EXIT',noproc} = (catch gen_server:stop(Pid)),
    true = test_server:stop_node(Node),
    {'EXIT',{{nodedown,Node},_}} = (catch gen_server:stop(Pid)),
    ok.

%% Registered name on remote node
stop9(_Config) ->
    {ok,Node} = test_server:start_node(gen_server_SUITE_stop9,slave,[]),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node,code,add_path,[Dir]),
    {ok, Pid} = rpc:call(Node,gen_server,start,[{local,to_stop},?MODULE,[],[]]),
    ok = gen_server:stop({to_stop,Node}),
    undefined = rpc:call(Node,erlang,whereis,[to_stop]),
    false = rpc:call(Node,erlang,is_process_alive,[Pid]),
    {'EXIT',noproc} = (catch gen_server:stop({to_stop,Node})),
    true = test_server:stop_node(Node),
    {'EXIT',{{nodedown,Node},_}} = (catch gen_server:stop({to_stop,Node})),
    ok.

%% Globally registered name on remote node
stop10(_Config) ->
    {ok,Node} = test_server:start_node(gen_server_SUITE_stop10,slave,[]),
    Dir = filename:dirname(code:which(?MODULE)),
    rpc:call(Node,code,add_path,[Dir]),
    {ok, Pid} = rpc:call(Node,gen_server,start,[{global,to_stop},?MODULE,[],[]]),
    global:sync(),
    ok = gen_server:stop({global,to_stop}),
    false = rpc:call(Node,erlang,is_process_alive,[Pid]),
    {'EXIT',noproc} = (catch gen_server:stop({global,to_stop})),
    true = test_server:stop_node(Node),
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
	    {exit,crashed,_} = proplists:get_value(error_info, List4),
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
%% Test call to nonexisting processes on remote nodes
%% --------------------------------------

start_node(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    N = test_server:start_node(Name, slave, [{args, " -pa " ++ Pa}]),
    %% After starting a slave, it takes a little while until global knows
    %% about it, even if nodes() includes it, so we make sure that global
    %% knows about it before registering something on all nodes.
    global:sync(),
    N.

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
    _ = test_server:stop_node(Node),
    {'EXIT', {noproc, _}} =
	(catch gen_server:call({global, N}, started_p, infinity)),

    ok.

call_remote_n2(Config) when is_list(Config) ->
    N = hubba,
    Node = proplists:get_value(node,Config),

    {ok, Pid} = rpc:call(Node, gen_server, start,
			 [{global, N}, ?MODULE, [], []]),
    _ = test_server:stop_node(Node),
    {'EXIT', {{nodedown, Node}, _}} = (catch gen_server:call(Pid,
							     started_p, infinity)),

    ok.

call_remote_n3(Config) when is_list(Config) ->
    Node = proplists:get_value(node,Config),

    {ok, _Pid} = rpc:call(Node, gen_server, start,
			  [{local, piller}, ?MODULE, [], []]),
    _ = test_server:stop_node(Node),
    {'EXIT', {{nodedown, Node}, _}} = (catch gen_server:call({piller, Node},
							     started_p, infinity)),

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
    {ok,Node} = start_node(hubba),
    {_,"@"++Host} = lists:splitwith(fun ($@) -> false; (_) -> true end,
				    atom_to_list(Node)),
    FalseNode = list_to_atom("hopp@"++Host),
    true = rpc:cast(Node, ?MODULE, cast_fast_messup, []),
    ct:sleep(1000),
    [Node] = nodes(),
    {Time,ok} = timer:tc(fun() ->
				 gen_server:cast({hopp,FalseNode}, hopp)
			 end),
    true = test_server:stop_node(Node),
    if Time > 1000000 ->       % Default listen timeout is about 7.0 s
	    ct:fail(hanging_cast);
       true ->
	    ok
    end.

cast_fast_messup() ->
    %% Register a false node: hopp@hostname
    unregister(erl_epmd),
    erl_epmd:start_link(),
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
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid, current_function)),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid, current_function)),

    gen_server:cast(my_test_name_hibernate, hibernate_now),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid, current_function)),

    Pid ! hibernate_later,
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid, current_function)),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid, current_function)),

    Pid ! hibernate_now,
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{erlang,hibernate,3}} =/=
		erlang:process_info(Pid, current_function)),
    receive
	{result,R} ->
	    {current_function,{erlang,hibernate,3}} = R
    end,

    true = gen_server:call(my_test_name_hibernate, hibernate),
    is_in_erlang_hibernate(Pid),
    sys:suspend(my_test_name_hibernate),
    is_in_erlang_hibernate(Pid),
    sys:resume(my_test_name_hibernate),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),

    ok = gen_server:call(my_test_name_hibernate, stop),
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
	{erlang,hibernate,3} ->
	    ok;
	_ ->
	    receive after 10 -> ok end,
	    is_in_erlang_hibernate_1(N-1, Pid)
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

%% Verify that sys:get_status correctly calls our format_status/2 fun.
call_format_status(Config) when is_list(Config) ->
    {ok, Pid} = gen_server:start_link({local, call_format_status},
				      ?MODULE, [], []),
    Status1 = sys:get_status(call_format_status),
    {status, Pid, _Mod, [_PDict, running, _Parent, _, Data1]} = Status1,
    [format_status_called | _] = lists:reverse(Data1),
    Status2 = sys:get_status(call_format_status, 5000),
    {status, Pid, _Mod, [_PDict, running, _Parent, _, Data2]} = Status2,
    [format_status_called | _] = lists:reverse(Data2),

    %% check that format_status can handle a name being a pid (atom is
    %% already checked by the previous test)
    {ok, Pid3} = gen_server:start_link(gen_server_SUITE, [], []),
    Status3 = sys:get_status(Pid3),
    {status, Pid3, _Mod, [_PDict3, running, _Parent, _, Data3]} = Status3,
    [format_status_called | _] = lists:reverse(Data3),

    %% check that format_status can handle a name being a term other than a
    %% pid or atom
    GlobalName1 = {global, "CallFormatStatus"},
    {ok, Pid4} = gen_server:start_link(GlobalName1,
				       gen_server_SUITE, [], []),
    Status4 = sys:get_status(Pid4),
    {status, Pid4, _Mod, [_PDict4, running, _Parent, _, Data4]} = Status4,
    [format_status_called | _] = lists:reverse(Data4),
    GlobalName2 = {global, {name, "term"}},
    {ok, Pid5} = gen_server:start_link(GlobalName2,
				       gen_server_SUITE, [], []),
    Status5 = sys:get_status(GlobalName2),
    {status, Pid5, _Mod, [_PDict5, running, _Parent, _, Data5]} = Status5,
    [format_status_called | _] = lists:reverse(Data5),
    ok.

%% Verify that error termination correctly calls our format_status/2 fun.
error_format_status(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    State = "called format_status",
    {ok, Pid} = gen_server:start_link(?MODULE, {state, State}, []),
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
	    io:format("Unexpected: ~p", [Other]),
	    ct:fail(failed)
    end,
    process_flag(trap_exit, OldFl),
    ok.

%% Verify that error when terminating correctly calls our format_status/2 fun
%%
terminate_crash_format(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    State = crash_terminate,
    {ok, Pid} = gen_server:start_link(?MODULE, {state, State}, []),
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
    process_flag(trap_exit, OldFl),
    ok.

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
    case test_server:is_native(gen) of
	true ->
	    {skip,
	     "gen is native - huge message queue optimization "
	     "is not implemented"};
	false ->
	    do_call_with_huge_message_queue()
    end.

do_call_with_huge_message_queue() ->
    Pid = spawn_link(fun echo_loop/0),

    {Time,ok} = tc(fun() -> calls(10000, Pid) end),

    [self() ! {msg,N} || N <- lists:seq(1, 500000)],
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
    {ok, Server} = gen_server:start(oc_server, [], []),
    MRef = monitor(process, Server),
    ok = gen_server:stop(Server),
    ok = verify_down_reason(MRef, Server, normal).

%% Test the default implementation of terminate if the callback module
%% does not export it
undef_terminate2(Config) when is_list(Config) ->
    {ok, Server} = gen_server:start(oc_server, [], []),
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
        {'EXIT', Server,
         {undef, [{oc_init_server, init, [_], _}|_]}} when is_pid(Server) ->
            ok
    after 1000 ->
        ct:fail(expected_exit_msg)
    end.

%% The upgrade should fail if code_change is expected in the callback module
%% but not exported, but the server should continue with the old code
undef_code_change(Config) when is_list(Config) ->
    {ok, Server} = gen_server:start(oc_server, [], []),
    {error, {'EXIT', {undef, [{oc_server, code_change, [_, _, _], _}|_]}}}
        = fake_upgrade(Server, ?MODULE),
    true = is_process_alive(Server).

%% The server should crash if the handle_call callback is
%% not exported in the callback module
undef_handle_call(_Config) ->
    {ok, Server} = gen_server:start(oc_server, [], []),
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
    {ok, Server} = gen_server:start(oc_server, [], []),
    MRef = monitor(process, Server),
    gen_server:cast(Server, cast_msg),
    verify_undef_down(MRef, Server, oc_server, handle_cast),
    ok.

%% The server should log but not crash if the handle_info callback is
%% calling an undefined function
undef_handle_info(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    {ok, Server} = gen_server:start(oc_server, [], []),
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
        gen_server:stop(Server),
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

%%% --------------------------------------------------------
%%% Here is the tested gen_server behaviour.
%%% --------------------------------------------------------

init([]) ->
    {ok, []};
init(ignore) ->
    ignore;
init(stop) ->
    {stop, stopped};
init(hibernate) ->
    {ok,[],hibernate};
init(sleep) ->
    ct:sleep(1000),
    {ok, []};
init({state,State}) ->
    {ok, State}.

handle_call(started_p, _From, State) ->
    io:format("FROZ"),
    {reply,ok,State};
handle_call({delayed_answer, T}, From, _State) ->
    {noreply,{reply_to,From},T};
handle_call({call_within, T}, _From, _) ->
    {reply,ok,call_within,T};
handle_call(next_call, _From, call_within) ->
    {reply,ok,[]};
handle_call(next_call, _From, State) ->
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
handle_call(stop_shutdown_reason, _From, State) ->
    {stop,{shutdown,stop_reason},State}.

handle_cast({From,handle_cast}, State) ->
    From ! {self(), handled_cast},
    {noreply, State};
handle_cast({From,delayed_cast,T}, _State) ->
    {noreply, {delayed_cast,From}, T};
handle_cast(hibernate_now, _State) ->
    {noreply, [], hibernate};
handle_cast(hibernate_later, _State) ->
    timer:send_after(1000,self(),hibernate_now),
    {noreply, []};
handle_cast({call_undef_fun, Mod, Fun}, State) ->
    Mod:Fun(),
    {noreply, State};
handle_cast({From, stop}, State) ->
    io:format("BAZ"),
    {stop, {From,stopped}, State}.

handle_info(timeout, {reply_to, From}) ->
    gen_server:reply(From, delayed),
    {noreply, []};
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
handle_info(_Info, State) ->
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

format_status(terminate, [_PDict, State]) ->
    {formatted, State};
format_status(normal, [_PDict, _State]) ->
    format_status_called.
