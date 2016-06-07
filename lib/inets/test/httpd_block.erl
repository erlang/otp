%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
%%
-module(httpd_block).

-include_lib("common_test/include/ct.hrl").

%% General testcases bodies called from httpd_SUITE
-export([block_disturbing_idle/4, block_non_disturbing_idle/4,
	 block_503/4, block_disturbing_active/4, 
	 block_non_disturbing_active/4, 
	 block_disturbing_active_timeout_not_released/4, 
	 block_disturbing_active_timeout_released/4, 
	 block_non_disturbing_active_timeout_not_released/4,
	 block_non_disturbing_active_timeout_released/4,
	 disturbing_blocker_dies/4,
	 non_disturbing_blocker_dies/4, restart_no_block/4,
	 restart_disturbing_block/4, restart_non_disturbing_block/4
	]).

%% Help functions 
-export([httpd_block/3, httpd_block/4, httpd_unblock/2, httpd_restart/2]).
-export([do_block_server/4, do_block_nd_server/5, do_long_poll/6]).

-define(report(Label, Content), 
	inets:report_event(20, Label, test_case, 
			   [{module, ?MODULE}, {line, ?LINE} | Content])).


%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
block_disturbing_idle(_Type, Port, Host, Node) ->
    io:format("block_disturbing_idle -> entry~n", []),
    validate_admin_state(Node, Host, Port, unblocked),
    block_server(Node, Host, Port),
    validate_admin_state(Node, Host, Port, blocked),
    unblock_server(Node, Host, Port),
    validate_admin_state(Node, Host, Port, unblocked),
    io:format("block_disturbing_idle -> done~n", []),
    ok.

%%--------------------------------------------------------------------
block_non_disturbing_idle(_Type, Port, Host, Node) ->
    unblocked = get_admin_state(Node, Host, Port),
    block_nd_server(Node, Host, Port),
    blocked = get_admin_state(Node, Host, Port),
    unblock_server(Node, Host, Port),
    unblocked = get_admin_state(Node, Host, Port),
    ok.

%%--------------------------------------------------------------------
block_503(Type, Port, Host, Node) ->
    Req = "GET / HTTP/1.0\r\ndummy-host.ericsson.se:\r\n\r\n",
    unblocked = get_admin_state(Node, Host, Port),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, Req, 
				  [{statuscode, 200},
				   {version, "HTTP/1.0"}]),
    ok = block_server(Node, Host, Port),
    blocked = get_admin_state(Node, Host, Port),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, Req,  
				  [{statuscode, 503},
				   {version, "HTTP/1.0"}]),
    ok = unblock_server(Node, Host, Port),
    unblocked = get_admin_state(Node, Host, Port),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, Req, 
				  [{statuscode, 200},
				   {version, "HTTP/1.0"}]).

%%--------------------------------------------------------------------
block_disturbing_active(Type, Port, Host, Node) ->
    process_flag(trap_exit, true),
    Pid = long_poll(Type, Host, Port, Node, 200, 60000),
    ct:sleep(15000),
    block_server(Node, Host, Port),
    await_suite_failed_process_exit(Pid, "poller", 60000,
				    connection_closed),
    blocked = get_admin_state(Node, Host, Port),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------
block_non_disturbing_active(Type, Port, Host, Node) ->
    process_flag(trap_exit, true),
    Poller = long_poll(Type, Host, Port, Node, 200, 60000),
    ct:sleep(15000),
    ok = block_nd_server(Node, Host, Port),
    await_normal_process_exit(Poller, "poller", 60000),
    blocked = get_admin_state(Node, Host, Port),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------
block_disturbing_active_timeout_not_released(Type, Port, Host, Node) ->
    process_flag(trap_exit, true),
    Poller = long_poll(Type, Host, Port, Node, 200, 60000),
    ct:sleep(15000),
    ok = httpd_block(undefined,  Port, disturbing, 50000),
    await_normal_process_exit(Poller, "poller", 30000),
    blocked = get_admin_state(Node, Host, Port),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------
block_disturbing_active_timeout_released(Type, Port, Host, Node) ->
    process_flag(trap_exit, true),
    Poller = long_poll(Type, Host, Port, Node, 200, 40000),
    ct:sleep(5000),
    ok = httpd_block(undefined,  Port, disturbing, 10000),
    await_suite_failed_process_exit(Poller, "poller", 40000, 
					  connection_closed),
    blocked = get_admin_state(Node, Host, Port),
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------
block_non_disturbing_active_timeout_not_released(Type, Port, Host, Node) ->
    process_flag(trap_exit, true),
    Poller = long_poll(Type, Host, Port, Node, 200, 60000),
    ct:sleep(5000),
    ok = block_nd_server(Node, Host, Port, 40000),
    await_normal_process_exit(Poller, "poller", 60000),
    blocked = get_admin_state(Node, Host, Port),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------
block_non_disturbing_active_timeout_released(Type, Port, Host, Node) ->
    process_flag(trap_exit, true),
    Poller = long_poll(Type, Host, Port, Node, 200, 45000),
    ct:sleep(5000),
    Blocker = blocker_nd(Node, Host, Port ,10000, {error,timeout}),
    await_normal_process_exit(Blocker, "blocker", 15000),
    await_normal_process_exit(Poller, "poller", 50000),
    unblocked = get_admin_state(Node, Host, Port),
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------
disturbing_blocker_dies(Type, Port, Host, Node) ->
    process_flag(trap_exit, true),
    Poller = long_poll(Type, Host, Port, Node, 200, 60000),
    ct:sleep(5000),
    Blocker = blocker(Node, Host, Port, 10000),
    ct:sleep(5000),
    exit(Blocker,simulate_blocker_crash),
    await_normal_process_exit(Poller, "poller", 60000),
    unblocked = get_admin_state(Node, Host, Port),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------
non_disturbing_blocker_dies(Type, Port, Host, Node) ->
    process_flag(trap_exit, true),
    Poller = long_poll(Type, Host, Port, Node, 200, 60000),
    ct:sleep(5000),  
    Blocker = blocker_nd(Node, Host, Port, 10000, ok),
    ct:sleep(5000),
    exit(Blocker, simulate_blocker_crash),
    await_normal_process_exit(Poller, "poller", 60000),
    unblocked = get_admin_state(Node, Host, Port),
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------
restart_no_block(_, Port, Host, Node) ->
    {error,_Reason} = restart_server(Node, Host, Port).

%%--------------------------------------------------------------------
restart_disturbing_block(_, Port, Host, Node) ->
    ?report("restart_disturbing_block - get_admin_state (unblocked)", []),
    unblocked = get_admin_state(Node, Host, Port),
    ?report("restart_disturbing_block - block_server", []),
    ok = block_server(Node, Host, Port),
    ?report("restart_disturbing_block - restart_server", []),
    ok = restart_server(Node, Host, Port),
    ?report("restart_disturbing_block - unblock_server", []),
    ok = unblock_server(Node, Host, Port),
    ?report("restart_disturbing_block - get_admin_state (unblocked)", []),
    unblocked = get_admin_state(Node, Host, Port).

%%--------------------------------------------------------------------
restart_non_disturbing_block(_, Port, Host, Node) ->
    ?report("restart_non_disturbing_block - get_admin_state (unblocked)", []),
    unblocked = get_admin_state(Node, Host, Port),
    ?report("restart_non_disturbing_block - block_nd_server", []),
    ok = block_nd_server(Node, Host, Port),
    ?report("restart_non_disturbing_block - restart_server", []),
    ok = restart_server(Node, Host, Port),
    ?report("restart_non_disturbing_block - unblock_server", []),
    ok = unblock_server(Node, Host, Port),
    ?report("restart_non_disturbing_block - get_admin_state (unblocked)", []),
    unblocked = get_admin_state(Node, Host, Port).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
blocker(Node, Host, Port, Timeout) ->
    spawn_link(?MODULE, do_block_server,[Node, Host, Port,Timeout]).

do_block_server(Node, Host, Port, Timeout) ->
    ok = block_server(Node, Host, Port, Timeout),
    exit(normal).

blocker_nd(Node, Host, Port, Timeout, Reply) ->
    spawn_link(?MODULE, do_block_nd_server,
	       [Node, Host, Port, Timeout, Reply]).

do_block_nd_server(Node, Host, Port, Timeout, Reply) ->
    Reply = block_nd_server(Node, Host, Port, Timeout),
    exit(normal).

restart_server(Node, _Host, Port) ->
    Addr = undefined, 
    rpc:call(Node, ?MODULE, httpd_restart, [Addr, Port]).


block_server(Node, _Host,  Port) ->
    io:format("block_server -> entry~n", []),    
    Addr = undefined, 
    rpc:call(Node, ?MODULE, httpd_block, [Addr, Port, disturbing]).


block_server(Node, _Host, Port, Timeout) ->
    Addr = undefined, 
    rpc:call(Node, ?MODULE, httpd_block, [Addr, Port, disturbing, Timeout]).


block_nd_server(Node, _Host, Port) ->
    Addr = undefined, 
    rpc:call(Node, ?MODULE, httpd_block, [Addr, Port, non_disturbing]).

block_nd_server(Node, _Host, Port, Timeout) ->
    Addr = undefined, 
    rpc:call(Node, ?MODULE, httpd_block, [Addr, Port, non_disturbing, Timeout]).

unblock_server(Node, _Host, Port) ->
    io:format("~p:~p:block_server -> entry~n", [node(),self()]),    
    Addr = undefined, 
    rpc:call(Node, ?MODULE, httpd_unblock, [Addr, Port]).


httpd_block(Addr, Port, Mode) ->
    io:format("~p:~p:httpd_block -> entry~n", [node(),self()]), 
    Name = make_name(Addr, Port),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:block(Pid, Mode);
	_ ->
	    {error, not_started}
    end.
    
httpd_block(Addr, Port, Mode, Timeout) ->
    Name = make_name(Addr, Port),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:block(Pid, Mode, Timeout);
	_ ->
	    {error, not_started}
    end.
    
httpd_unblock(Addr, Port) ->
    io:format("~p:~p:httpd_unblock -> entry~n", [node(),self()]), 
    Name = make_name(Addr, Port),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:unblock(Pid);
	_ ->
	    {error, not_started}
    end.
    
httpd_restart(Addr, Port) ->
    Name = make_name(Addr, Port),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:reload(Pid, undefined);
	_ ->
	    {error, not_started}
    end.
    
make_name(Addr, Port) ->
    httpd_util:make_name("httpd", Addr, Port, default).

get_admin_state(_, _Host, Port) ->
    Name = make_name(undefined, Port),
    {status, _, _, StatusInfo} = sys:get_status(whereis(Name)),
    [_, _,_, _, Prop] = StatusInfo,
    State = state(Prop),
    element(6, State).

validate_admin_state(Node, Host, Port, Expect) ->
    io:format("try validating server admin state: ~p~n", [Expect]),
    case get_admin_state(Node, Host, Port) of
	Expect ->
	    ok;
	Unexpected ->
	    io:format("failed validating server admin state: ~p~n", 
		      [Unexpected]),
	    exit({unexpected_admin_state, Unexpected, Expect})
    end.


await_normal_process_exit(Pid, Name, Timeout) ->
    receive
	{'EXIT', Pid, normal} ->
	    ok;
	{'EXIT', Pid, Reason} ->
	    Err = 
		lists:flatten(
		  io_lib:format("expected normal exit, "
				"unexpected exit of ~s process: ~p",
				[Name, Reason])),
	    ct:fail(Err)
    after Timeout ->
	    ct:fail("timeout while waiting for " ++ Name)
    end.


await_suite_failed_process_exit(Pid, Name, Timeout, Why) ->
    receive 
	{'EXIT', Pid, {test_failed, Why}} ->
	    ok;
	{'EXIT', Pid, Reason} ->
	    Err = 
		lists:flatten(
		  io_lib:format("expected connection_closed, "
				"unexpected exit of ~s process: ~p",
				[Name, Reason])),
	    ct:fail(Err)
    after Timeout ->
	    ct:fail("timeout while waiting for " ++ Name)
    end.
	  
long_poll(Type, Host, Port, Node, StatusCode, Timeout) ->
    spawn_link(?MODULE, do_long_poll, [Type, Host, Port, Node, 
				       StatusCode, Timeout]).

do_long_poll(Type, Host, Port, Node, StatusCode, Timeout) ->
    Mod  = "httpd_example",
    Func = "delay",
    Req  = lists:flatten(io_lib:format("GET /eval?" ++ Mod ++ ":" ++ Func ++ 
				       "(~p) HTTP/1.0\r\n\r\n",[30000])),
    case httpd_test_lib:verify_request(Type, Host, Port, Node, Req, 
			      [{statuscode, StatusCode},
			       {version, "HTTP/1.0"}], Timeout) of
	ok ->
	    exit(normal);
	Reason ->
	    exit({test_failed, Reason})
    end.


state([{data,[{"State", State}]} | _]) ->
    State;
state([{data,[{"StateData", State}]} | _]) ->
    State;
state([_ | Rest]) ->
    state(Rest).
