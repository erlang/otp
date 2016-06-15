%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(slave_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, t_start/1, t_start_link/1,
	 start_link_nodedown/1, errors/1]).

%% Internal exports.
-export([fun_init/1, test_errors/1]).
-export([timeout_test/1, auth_test/1, rsh_test/1, start_a_slave/3]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [t_start_link, start_link_nodedown, t_start, errors].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


t_start_link(Config) when is_list(Config) ->
    %% Define useful variables.

    Host = host(),
    Slave1 = node_name(Host, slave1),
    Slave2 = node_name(Host, slave2),

    %% Test slave:start_link() with one, two, and three arguments.

    ThisNode = node(),
    {error, {already_running, ThisNode}} = slave:start_link(Host),
    {ok, Slave1} = slave:start_link(Host, slave1),
    {ok, Slave2} = slave:start_link(Host, slave2, "-my_option 42"),
    {ok, [["42"]]} = rpc:call(Slave2, init, get_argument, [my_option]),

    %% Kill the two slave nodes and verify that they are dead.

    rpc:cast(Slave1, erlang, halt, []),
    rpc:cast(Slave2, erlang, halt, []),
    ct:sleep(250),
    is_dead(Slave1),
    is_dead(Slave2),

    %% Start two slave nodes from another process and verify that
    %% the slaves die when that process terminates.

    Parent = self(),
    Pid = fun_spawn(fun () ->
			    {ok, Slave1} = slave:start_link(Host, slave1),
			    {ok, Slave2} = slave:start_link(Host, slave2),
			    Parent ! slaves_started,
			    receive never -> ok end
		    end),
    receive slaves_started -> ok end,
    process_flag(trap_exit, true),
    wait_alive(Slave1),
    wait_alive(Slave2),
    exit(Pid, kill),
    receive {'EXIT', Pid, killed} -> ok end,
    ct:sleep(250),
    is_dead(Slave1),
    is_dead(Slave2),

    ok.

%% Test that slave:start_link() works when the master exits.

start_link_nodedown(Config) when is_list(Config) ->
    %% Define useful variables.

    Host = host(),
    Master = node_name(Host, my_master),
    Slave = node_name(Host, my_slave),

    Pa = "-pa " ++ filename:dirname(code:which(?MODULE)),
    {ok, Master} = slave:start_link(Host, my_master, Pa),
    spawn(Master, ?MODULE, start_a_slave, [self(), Host, my_slave]),
    {reply, {ok, _Node}} = receive Any -> Any end,

    rpc:call(Master, erlang, halt, []),
    receive after 200 -> ok end,
    pang = net_adm:ping(Slave),

    ok.

start_a_slave(ReplyTo, Host, Name) ->
    ReplyTo ! {reply, slave:start_link(Host, Name)},
    receive never -> ok end.

%% Test slave:start().

t_start(Config) when is_list(Config) ->
    %% Define useful variables.

    Host = host(),
    Slave1 = node_name(Host, slave1),
    Slave2 = node_name(Host, slave2),

    %% By running all tests from this master node which is linked
    %% to this test case, we ensure that all slaves are killed
    %% if this test case fails.  (If they are not, and therefore further
    %% test cases fail, there is a bug in slave.)

    {ok, Master} = slave:start_link(Host, master),

    %% Test slave:start() with one, two, and three arguments.

    ThisNode = node(),
    {error, {already_running, ThisNode}} = slave:start(Host),
    {ok, Slave1} = rpc:call(Master, slave, start, [Host, slave1]),
    {ok, Slave2} = rpc:call(Master, slave, start,
			    [Host, slave2, "-my_option 42"]),
    {ok, [["42"]]} = rpc:call(Slave2, init, get_argument, [my_option]),

    %% Test that a slave terminates when its master node terminates.

    ok = slave:stop(Slave2),
    is_dead(Slave2),
    {ok, Slave2} = rpc:call(Slave1, slave, start, [Host, slave2]),
    is_alive(Slave2),
    rpc:call(Slave1, erlang, halt, []),	% Kill master.
    receive after 1000 -> ok end,		% Make sure slaves have noticed
						% their dead master.
    is_dead(Slave1),
    is_dead(Slave2),			% Slave should be dead, too.

    %% Kill all slaves and verify that they are dead.

    ok = slave:stop(Slave1),
    ok = slave:stop(Slave2),
    is_dead(Slave1),
    is_dead(Slave2),

    ok.

%% Test the various error conditions in parallell (since the timeout
%% in slave is 32 seconds).

errors(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Pa = filename:dirname(code:which(?MODULE)),
    {ok, Master} = slave_start_link(host(), master,
				    "-rsh no_rsh_program -pa "++Pa++
					" -env ERL_CRASH_DUMP erl_crash_dump.master"),
    Pids = rpc:call(Master, ?MODULE, test_errors, [self()]),
    wait_for_result(Pids),

    ok.

wait_for_result([]) ->
    ok;
wait_for_result(Pids) ->
    receive
	{'EXIT', Pid, normal} ->
	    io:format("Process ~p terminated", [Pid]),
	    wait_for_result(lists:delete(Pid, Pids));
	{'EXIT', _, Reason} ->
	    exit(Reason)
    end.

show_process_info(Pid) ->
    io:format("~p: ~p", [Pid, catch process_info(Pid, initial_call)]).

test_errors(ResultTo) ->
    %% Sigh!  We use ordinary spawn instead of fun_spawn/1 to be able
    %% identify the processes by their initial call.
    P1 = spawn(?MODULE, timeout_test, [ResultTo]),
    P2 = spawn(?MODULE, auth_test, [ResultTo]),
    P3 = spawn(?MODULE, rsh_test, [ResultTo]),
    Pids =[P1, P2, P3],
    lists:foreach(fun show_process_info/1, Pids),
    Pids.

timeout_test(ResultTo) ->
    link(ResultTo),
    {error, timeout} = slave:start(host(), slave1, "-boot no_boot_script").

auth_test(ResultTo) ->
    link(ResultTo),
    {error, timeout} = slave:start(host(), slave2,
				   "-setcookie definitely_not_a_cookie").

rsh_test(ResultTo) ->
    link(ResultTo),
    {error, no_rsh} = slave:start(super, slave3).


%%% Utilities.    


wait_alive(Node) ->
    wait_alive_1(10, Node).

wait_alive_1(0, Node) ->
    ct:fail({still_not_alive,Node});
wait_alive_1(N, Node) ->
    case rpc:call(Node, init, get_status, []) of
	{started,_} ->
	    ok;
	{starting,_} ->
	    receive after 1 -> ok end,
	    wait_alive_1(N-1, Node)
    end.

is_alive(Node) ->
    {started, _} = rpc:call(Node, init, get_status, []).

is_dead(Node) ->
    {badrpc, nodedown} = rpc:call(Node, init, get_status, []).

node_name(Host, Name) ->
    list_to_atom(lists:concat([Name, "@", Host])).

host() ->
    from($@, atom_to_list(node())).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_H, []) -> [].

slave_start_link(Host, Name, Args) ->
    case slave:start_link(Host, Name, Args) of
	{ok, Node} ->
	    {ok, Node};
	Other ->
	    io:format("slave:start_link(~p, ~p, ~p) -> ~p",
		      [Host, Name, Args, Other])
    end.

fun_spawn(Fun) ->
    spawn_link(?MODULE, fun_init, [Fun]).

fun_init(Fun) ->
    Fun().
