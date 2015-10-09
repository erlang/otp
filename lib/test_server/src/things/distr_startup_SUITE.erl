%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(distr_startup_SUITE).
-compile([export_all]).
%%-define(line_trace,1).
-include("test_server.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) -> [reads,writes].

-define(iterations,10000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

app1() ->
    {application, app1,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {applications, [kernel, stdlib]},
      {mod, {ch_sup, {app1, 1, 3}}}]}.

app3() ->
    {application, app3,
     [{description, "ERTS  CXC 138 10"},
      {vsn, "2.0"},
      {applications, [kernel, stdlib]},
      {mod, {ch_sup, {app3, 7, 9}}}]}.


config(Fd,C1,C2,C3) ->
    io:format(Fd, 
	      "[{kernel, [{sync_nodes_optional, ['~s','~s','~s']},"
	      "{sync_nodes_timeout, 1},"
	      "{distributed, [{app1, ['~s', '~s', '~s']},"
	      "{app2, 10000, ['~s', '~s', '~s']},"
	      "{app3, 5000, [{'~s', '~s'}, '~s']}]}]}].~n",
	      [C1,C2,C3, C1,C2,C3, C1,C2,C3, C1,C2,C3]).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(H, []) -> [].

%%-----------------------------------------------------------------
%% Test suite for distributed applications, tests start, load
%% etc indirectly.
%% Should be started in a CC view with:
%% erl -sname master -rsh ctrsh
%%-----------------------------------------------------------------
start_nodes(Conf) ->
    % Write a config file
    ?line Nodes = ?config(nodes,Conf),
    ?line [C1,C2,C3|_] = Nodes, %% Need at least 3 nodes
    ?line Dir = ?config(priv_dir,Conf),
    ?line {ok, Fd} = file:open(Dir ++ "sys.config", write),
    ?line config(Fd,C1,C2,C3),
    ?line file:close(Fd),
    ?line Config = Dir ++ "sys",

    % Test [cp1, cp2, cp3]
    ?line {ok, Cp1} = start_node(lists:nth(1,Nodes), Config),
    ?line {ok, Cp2} = start_node(lists:nth(2,Nodes), Config),
    ?line {ok, Cp3} = start_node(lists:nth(3,Nodes), Config),
    % Start app1 and make sure cp1 starts it
    %%?line rpc:multicall([Cp1, Cp2, Cp3], application, load, [app1()]),
    %%?line rpc:multicall([Cp1, Cp2, Cp3], application, start,[app1,permanent]),
    ?line test_server:sleep(1000),
    {Cp1,Cp2,Cp3}.

stop_nodes({Cp1,Cp2,Cp3}) ->
    ?line stop_node(Cp1),
    ?line stop_node(Cp2),
    ?line stop_node(Cp3).

start_node(NodeAtHost, Config) ->
    ?line NodeAtHostStr = atom_to_list(NodeAtHost),
    ?line HostStr = from($@,NodeAtHostStr),
    ?line NodeStr = lists:reverse(from($@,lists:reverse(NodeAtHostStr))),
    ?line Host = list_to_atom(HostStr),
    ?line Node = list_to_atom(NodeStr),
    ?line io:format("Launching slave node ~p@~p ~p",[Node,Host,Config]),
    ?line slave:start(Host, Node, lists:concat(["-config ", Config])).

stop_node(Node) ->
    ?line rpc:cast(Node, erlang, halt, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_client_process(Cp,Mode,NodeNum) ->
    io:format("Starting client process at ~p in mode ~p",[Cp,Mode]),
    ?line case rpc:call(Cp, erlang, spawn,
			[?MODULE, client,
			 [Mode,NodeNum,self(),random:uniform(1000)]]) of
	      {badrpc,Reason} ->
		  ?line exit({badrpc,{Cp,Reason}});
	      Client ->
		  ?line Client
	  end.

start_clients(Mode,Conf) ->
    ?line random:seed(4711,0,0),
    ?line {Cp1,Cp2,Cp3} = start_nodes(Conf),
    ?line Client1 = start_client_process(Cp1,Mode,1),
    ?line Client2 = start_client_process(Cp2,Mode,2),
    ?line Client3 = start_client_process(Cp3,Mode,3),
    test_server:format(1,"All 3 nodes started, "
		       "power off client(s) any time...",[]),
    Client1 ! go,
    Client2 ! go,
    Client3 ! go,
    {{Cp1,Cp2,Cp3},{Client1,Client2,Client3}}.

stop_clients(Cps) ->
    test_server:format(1,"Test completed.",[]),
    ?line stop_nodes(Cps).

data() ->
    {{self(),foo,bar,[1,2,3,4,5,6,7],{{{{}}}},
      "We need pretty long packages, so that there is a big risk "
      "of cutting it in the middle when suddenly turning off "
      "the power or breaking the connection.  "
      "We don't check the contents of the data very much, but "
      "at least there is a magic cookie at the end (123456)."
      "If that one arrives correctly, the link is ok as far "
      "as we are concerned."},
     123456}.

reads(suite) -> [];
reads(Conf) ->
    ?line {Cps,_} = start_clients(w,Conf),
    ?line read_loop(?iterations,0),
    ?line stop_clients(Cps),
    ok.

read_loop(0,M) ->
    ok;
read_loop(N,M) ->
    ?line Dog = test_server:timetrap(test_server:seconds(0.5)),
    M2 =
	receive
	    {Node,Count,{_,123456}} ->
		?line setelement(Node,M,element(Node,M)+1);
	    {Node,Count,Data} ->
		?line exit({network_transmission_error,Data});
	    {nodedown,Node} ->
		?line test_server:format(1,"Node ~s went down",[Node]),
		?line M;
	    Other ->
		?line M
	after test_server:seconds(0.1) ->
		?line io:format("No message!"),
		?line M
	end,
    ?line test_server:timetrap_cancel(Dog),    
    ?line M3 =
	case N rem 100 of
	    0 -> io:format("~p reads to go (~w msgs)",[N,M2]),
		 {0,0,0};
	    _ -> M2
	  end,
    ?line read_loop(N-1,M3).

client(w,NodeNum,Pid,Seed) ->
    random:seed(Seed,0,0),
    receive
	go -> ok
    end,
    client_write_loop(Pid,0,NodeNum,data());
client(r,NodeNum,Pid,Seed) ->
    random:seed(Seed,0,0),
    receive
	go -> ok
    end,
    client_read_loop(0).

client_write_loop(Pid,N,NodeNum,Data) ->
    test_server:sleep(random:uniform(20)),
    Pid ! {NodeNum,N,Data},
    client_write_loop(Pid,N+1,NodeNum,Data).

writes(suite) -> [];
writes(Conf) ->
    ?line {Cps,{C1,C2,C3}} = start_clients(r,Conf),
    ?line write_loop(2*?iterations,{C1,C2,C3},data()),
    ?line stop_clients(Cps),
    ok.

write_loop(0,_,_) ->
    ok;
write_loop(N,Clients,Data) ->
    ?line Dog = test_server:timetrap(test_server:seconds(0.5)),
    ?line Client = element(random:uniform(size(Clients)),Clients),
    ?line Client ! {node(),N,Data},
    ?line test_server:timetrap_cancel(Dog),
    receive
	{nodedown,Node} ->
	    ?line test_server:format(1,"Node ~s went down",[Node])
    after 0 ->
	    ?line ok
    end,
    ?line case N rem 100 of
	      0 -> io:format("~p writes to go",[N]);
	      _ -> ok
	  end,
    ?line write_loop(N-1,Clients,Data).

client_read_loop(N) ->
    receive
	{Node,Count,{_,123456}} ->
	    ?line ok;
	{Node,Count,Data} ->
	    ?line io:format("~p(~p): transmission error from node ~p(~p): ~p",
			    [node(),N,Node,Count,Data]);
	Other ->
	    ?line io:format("~p(~p): got a strange message: ~p",
			    [node(),N,Other])
    end,
    client_read_loop(N+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


