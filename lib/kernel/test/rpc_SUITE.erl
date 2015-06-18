%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2011. All Rights Reserved.
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
-module(rpc_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([call/1, block_call/1, multicall/1, multicall_timeout/1, 
	 multicall_dies/1, multicall_node_dies/1,
	 called_dies/1, called_node_dies/1, 
	 called_throws/1, call_benchmark/1, async_call/1]).

-export([suicide/2, suicide/3, f/0, f2/0]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [call, block_call, multicall, multicall_timeout,
     multicall_dies, multicall_node_dies, called_dies,
     called_node_dies, called_throws, call_benchmark,
     async_call].

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



call(doc) -> "Test different rpc calls";
call(Config) when is_list(Config) ->
    Timetrap = ?t:timetrap(?t:seconds(30)),
    ?line PA = filename:dirname(code:which(?MODULE)),
    %% Note. First part of nodename sets response delay in seconds
    ?line {ok, N1} = ?t:start_node('3_rpc_SUITE_call', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N2} = ?t:start_node('1_rcp_SUITE_call', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N3} = ?t:start_node('4_rcp_SUITE_call', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N4} = ?t:start_node('8_rcp_SUITE_call', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line ok = io:format("~p~n", [[N1, N2, N3]]),
    ?line {hej,_,N1} = rpc:call(N1, ?MODULE, f, []),
    ?line {hej,_,N2} = rpc:call(N2, ?MODULE, f, [], 2000),
    ?line {badrpc,timeout} = rpc:call(N3, ?MODULE, f, [], 2000),
    ?line receive after 6000 -> ok end,
    ?line [] = flush([]),
    ?line {hej,_,N4} = rpc:call(N4, ?MODULE, f, []),
    ?line ?t:stop_node(N1),
    ?line ?t:stop_node(N2),
    ?line ?t:stop_node(N3),
    ?line ?t:stop_node(N4),
    ?t:timetrap_cancel(Timetrap),
    ok.

block_call(doc) -> "Test different rpc calls";
block_call(Config) when is_list(Config) ->
    Timetrap = ?t:timetrap(?t:seconds(30)),
    ?line PA = filename:dirname(code:which(?MODULE)),
    %% Note. First part of nodename sets response delay in seconds
    ?line {ok, N1} = ?t:start_node('3_rpc_SUITE_block_call', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N2} = ?t:start_node('1_rcp_SUITE_block_call', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N3} = ?t:start_node('4_rcp_SUITE_block_call', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N4} = ?t:start_node('8_rcp_SUITE_block_call', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line ok = io:format("~p~n", [[N1, N2, N3]]),
    ?line {hej,_,N1} = rpc:block_call(N1, ?MODULE, f, []),
    ?line {hej,_,N2} = rpc:block_call(N2, ?MODULE, f, [], 2000),
    ?line {badrpc,timeout} = rpc:block_call(N3, ?MODULE, f, [], 2000),
    ?line receive after 6000 -> ok end,
    ?line [] = flush([]),
    ?line {hej,_,N4} = rpc:block_call(N4, ?MODULE, f, []),
    ?line ?t:stop_node(N1),
    ?line ?t:stop_node(N2),
    ?line ?t:stop_node(N3),
    ?line ?t:stop_node(N4),
    ?t:timetrap_cancel(Timetrap),
    ok.


multicall(doc) ->
    "OTP-3449";
multicall(Config) when is_list(Config) ->
    Timetrap = ?t:timetrap(?t:seconds(20)),
    ?line PA = filename:dirname(code:which(?MODULE)),
    %% Note. First part of nodename sets response delay in seconds
    ?line {ok, N1} = ?t:start_node('3_rpc_SUITE_multicall', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N2} = ?t:start_node('1_rcp_SUITE_multicall', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line ok = io:format("~p~n", [[N1, N2]]),
    ?line {[{hej,_,N1},{hej,_,N2}],[]} = 
	   rpc:multicall([N1, N2], ?MODULE, f, []),
    ?line Msgs = flush([]),
    ?line [] = Msgs,
    ?line ?t:stop_node(N1),
    ?line ?t:stop_node(N2),
    ?t:timetrap_cancel(Timetrap),
    ok.

multicall_timeout(Config) when is_list(Config) ->
    Timetrap = ?t:timetrap(?t:seconds(30)),
    ?line PA = filename:dirname(code:which(?MODULE)),
    %% Note. First part of nodename sets response delay in seconds
    ?line {ok, N1} = ?t:start_node('11_rpc_SUITE_multicall', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N2} = ?t:start_node('8_rpc_SUITE_multicall', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N3} = ?t:start_node('5_rpc_SUITE_multicall', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N4} = ?t:start_node('2_rcp_SUITE_multicall', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line ok = io:format("~p~n", [[N1, N2]]),
    ?line {[{hej,_,N3},{hej,_,N4}],[N1, N2]} = 
	   rpc:multicall([N3, N1, N2, N4], ?MODULE, f, [], ?t:seconds(6)),
    ?t:sleep(?t:seconds(8)), %% Wait for late answers
    ?line Msgs = flush([]),
    ?line [] = Msgs,
    ?line ?t:stop_node(N1),
    ?line ?t:stop_node(N2),
    ?line ?t:stop_node(N3),
    ?line ?t:stop_node(N4),
    ?t:timetrap_cancel(Timetrap),
    ok.

multicall_dies(Config) when is_list(Config) ->
    Timetrap = ?t:timetrap(?t:seconds(30)),
    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok, N1} = ?t:start_node('rpc_SUITE_multicall_dies_1', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N2} = ?t:start_node('rcp_SUITE_multicall_dies_2', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line Nodes = [N1, N2],
    %%
    ?line {[{badrpc, {'EXIT', normal}}, {badrpc, {'EXIT', normal}}], []} = 
	do_multicall(Nodes, erlang, exit, [normal]),
    ?line {[{badrpc, {'EXIT', abnormal}}, {badrpc, {'EXIT', abnormal}}], []} = 
	do_multicall(Nodes, erlang, exit, [abnormal]),
    ?line {[{badrpc, {'EXIT', {badarith, _}}}, 
	    {badrpc, {'EXIT', {badarith, _}}}], 
	   []} = 
	do_multicall(Nodes, erlang, 'div', [1, 0]),
    ?line {[{badrpc, {'EXIT', {badarg, _}}}, 
	    {badrpc, {'EXIT', {badarg, _}}}], 
	   []} = 
	do_multicall(Nodes, erlang, atom_to_list, [1]),
    ?line {[{badrpc, {'EXIT', {undef, _}}}, 
	    {badrpc, {'EXIT', {undef, _}}}], 
	   []} = 
	do_multicall(Nodes, ?MODULE, suicide, []),
    ?line {[timeout, timeout], []} = 
	do_multicall(Nodes, ?MODULE, suicide, [link, normal]),
    ?line {[{badrpc, {'EXIT', abnormal}}, {badrpc, {'EXIT', abnormal}}], []} = 
	do_multicall(Nodes, ?MODULE, suicide, [link, abnormal]),
    ?line {[timeout, timeout], []} = 
	do_multicall(Nodes, ?MODULE, suicide, [exit, normal]),
    ?line {[{badrpc, {'EXIT', abnormal}}, {badrpc, {'EXIT', abnormal}}], []} = 
	do_multicall(Nodes, ?MODULE, suicide, [exit, abnormal]),
    ?line {[{badrpc, {'EXIT', killed}}, {badrpc, {'EXIT', killed}}], []} = 
	do_multicall(Nodes, ?MODULE, suicide, [exit, kill]),
    %%
    ?line ?t:stop_node(N1),
    ?line ?t:stop_node(N2),
    ?t:timetrap_cancel(Timetrap),
    ok.

do_multicall(Nodes, Mod, Func, Args) ->
    ?line ok = io:format("~p:~p~p~n", [Mod, Func, Args]),
    ?line Result = rpc:multicall(Nodes, Mod, Func, Args),
    ?line Msgs = flush([]),
    ?line [] = Msgs,
    Result.



multicall_node_dies(doc) ->    
    "";
multicall_node_dies(Config) when is_list(Config) ->
    Timetrap = ?t:timetrap(?t:seconds(60)),
    %%
    do_multicall_2_nodes_dies(?MODULE, suicide, [erlang, halt, []]),
    do_multicall_2_nodes_dies(?MODULE, suicide, [init, stop, []]),
    do_multicall_2_nodes_dies(?MODULE, suicide, [rpc, stop, []]),
    %%
    ?t:timetrap_cancel(Timetrap),
    ok.

do_multicall_2_nodes_dies(Mod, Func, Args) ->
    ?line ok = io:format("~p:~p~p~n", [Mod, Func, Args]),
    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok, N1} = ?t:start_node('rpc_SUITE_multicall_node_dies_1', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line {ok, N2} = ?t:start_node('rcp_SUITE_multicall_node_dies_2', slave, 
				   [{args, "-pa " ++ PA}]),
    ?line Nodes = [N1, N2],
    ?line {[], Nodes} = rpc:multicall(Nodes, Mod, Func, Args),
    ?line Msgs = flush([]),
    ?line [] = Msgs,
    ok.



called_dies(doc) ->
    "OTP-3766";
called_dies(Config) when is_list(Config) ->
    Timetrap = ?t:timetrap(?t:seconds(210)),
    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok, N} = ?t:start_node(rpc_SUITE_called_dies, slave, 
				  [{args, "-pa " ++ PA}]),
    %%
    ?line rep(fun (Tag, Call, Args) ->
		      {Tag,{badrpc,{'EXIT',normal}}} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, erlang, exit, [normal]),
    ?line rep(fun (Tag, Call, Args) ->
		      {Tag,{badrpc,{'EXIT',abnormal}}} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, erlang, exit, [abnormal]),
    ?line rep(fun (Tag, Call, Args) ->
		      {Tag,{badrpc,{'EXIT',{badarith,_}}}} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, erlang, 'div', [1,0]),
    ?line rep(fun (Tag, Call, Args) ->
		      {Tag,{badrpc,{'EXIT',{badarg,_}}}} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, erlang, atom_to_list, [1]),
    ?line rep(fun (Tag, Call, Args) ->
		      {Tag,{badrpc,{'EXIT',{undef,_}}}} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, ?MODULE, suicide, []),
    %%
    TrapExit = process_flag(trap_exit, true),
    %%
    ?line rep(fun (Tag, Call, Args=[Node|_]) when Node == node() ->
		      {Tag,timeout} = 
			  {Tag,apply(rpc, Call, Args)},
		      {Tag,flush,[{'EXIT',_,normal}]} = 
			  {Tag,flush,flush([])};
		  (Tag, Call, Args) ->
		      {Tag,timeout} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, ?MODULE, suicide, [link,normal]),
    ?line rep(fun (Tag, Call, Args=[Node|_]) when Node == node() ->
		      {Tag,timeout} = 
			  {Tag,apply(rpc, Call, Args)},
		      {Tag,flush,[{'EXIT',_,abnormal}]} = 
			  {Tag,flush,flush([])};
		  (Tag, block_call, Args) ->
		      {Tag,timeout} = 
			  {Tag,apply(rpc, block_call, Args)};
		  (Tag, Call, Args) ->
		      {Tag,{badrpc,{'EXIT',abnormal}}} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, ?MODULE, suicide, [link,abnormal]),
    ?line rep(fun (Tag, Call, Args=[Node|_]) when Node == node() ->
		      {Tag,timeout} = 
			  {Tag,apply(rpc, Call, Args)},
		      {Tag,flush,[{'EXIT',_,normal}]} = 
			  {Tag,flush,flush([])};
		  (Tag, Call, Args) ->
		      {Tag,timeout} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, ?MODULE, suicide, [exit,normal]),
    ?line rep(fun (Tag, Call, Args=[Node|_]) when Node == node() ->
		      {Tag,timeout} = 
			  {Tag,apply(rpc, Call, Args)},
		      {Tag,flush,[{'EXIT',_,abnormal}]} = 
			  {Tag,flush,flush([])};
		  (Tag, block_call, Args) ->
		      {Tag,timeout} = 
			  {Tag,apply(rpc, block_call, Args)};
		  (Tag, Call, Args) ->
		      {Tag,{badrpc,{'EXIT',abnormal}}} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, ?MODULE, suicide, [exit,abnormal]),
    %%
    process_flag(trap_exit, TrapExit),
    %%
    ?line rep(fun %% A local [exit,kill] would kill the test case process
		  (_Tag, _Call, [Node|_]) when Node == node() ->
		      ok;
		  %% A block_call [exit,kill] would kill the rpc server
		  (_Tag, block_call, _Args) -> ok;
		  (Tag, Call, Args) ->
		      {Tag,{badrpc,{'EXIT',killed}}} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, ?MODULE, suicide, [exit,kill]),
    %%
    ?line [] = flush([]),
    ?line ?t:stop_node(N),
    ?t:timetrap_cancel(Timetrap),
    ok.

rep(Fun, N, M, F, A) ->
    Fun(1, call, [node(), M, F, A]),
    Fun(2, call, [node(), M, F, A, infinity]),
    Fun(3, call, [N, M, F, A]),
    Fun(4, call, [N, M, F, A, infinity]),
    Fun(5, call, [N, M, F, A, 3000]),
    Fun(6, block_call, [node(), M, F, A]),
    Fun(7, block_call, [node(), M, F, A, infinity]),
    Fun(8, block_call, [N, M, F, A]),
    Fun(9, block_call, [N, M, F, A, infinity]),
    Fun(10, block_call, [N, M, F, A, 3000]),
    ok.
    

suicide(link, Reason) ->
    spawn_link(
      fun() ->
	      exit(Reason)
      end),
    receive after 2000 -> timeout end;
suicide(exit, Reason) ->
    Self = self(),
    spawn(
      fun() ->
	      exit(Self, Reason)
      end),
    receive after 2000 -> timeout end.

suicide(erlang, exit, [Name, Reason]) when is_atom(Name) ->
    case whereis(Name) of
	Pid when is_pid(Pid) -> suicide(erlang, exit, [Pid, Reason])
    end;
suicide(Mod, Func, Args) ->
    spawn_link(
      fun() ->
	      apply(Mod, Func, Args)
      end),
    receive after 10000 -> timeout end.



called_node_dies(doc) ->
    "";
called_node_dies(suite) -> [];
called_node_dies(Config) when is_list(Config) ->
    Timetrap = ?t:timetrap(?t:minutes(2)),
    ?line PA = filename:dirname(code:which(?MODULE)),
    %%
    ?line node_rep(
	    fun (Tag, Call, Args) ->
		    {Tag,{badrpc,nodedown}} = 
			{Tag,apply(rpc, Call, Args)}
	    end, "rpc_SUITE_called_node_dies_1", 
	    PA, ?MODULE, suicide, [erlang,halt,[]]),
    ?line node_rep(
	    fun (Tag, Call, Args) ->
		    {Tag,{badrpc,nodedown}} = 
			{Tag,apply(rpc, Call, Args)}
	    end, "rpc_SUITE_called_node_dies_2", 
	    PA, ?MODULE, suicide, [init,stop,[]]),
    ?line node_rep(
	    fun (Tag, Call, Args=[_|_]) ->
		    {Tag,{'EXIT',{killed,_}}} = 
			{Tag,catch {noexit,apply(rpc, Call, Args)}}
	    end, "rpc_SUITE_called_node_dies_3", 
	    PA, ?MODULE, suicide, [erlang,exit,[rex,kill]]),
    ?line node_rep(
	    fun %% Cannot block call rpc - will hang
		(_Tag, block_call, _Args) -> ok;
		(Tag, Call, Args=[_|_]) ->
		    {Tag,{'EXIT',{normal,_}}} = 
			{Tag,catch {noexit,apply(rpc, Call, Args)}}
	    end, "rpc_SUITE_called_node_dies_4", 
	    PA, ?MODULE, suicide, [rpc,stop,[]]),
    %%
    ?t:timetrap_cancel(Timetrap),
    ok.

node_rep(Fun, Name, PA, M, F, A) ->
    {ok, Na} = ?t:start_node(list_to_atom(Name++"_a"), slave, 
			      [{args, "-pa " ++ PA}]),
    Fun(a, call, [Na, M, F, A]),
    catch ?t:stop_node(Na),
    {ok, Nb} = ?t:start_node(list_to_atom(Name++"_b"), slave, 
			      [{args, "-pa " ++ PA}]),
    Fun(b, call, [Nb, M, F, A, infinity]),
    catch ?t:stop_node(Nb),
    {ok, Nc} = ?t:start_node(list_to_atom(Name++"_c"), slave, 
			      [{args, "-pa " ++ PA}]),
    Fun(c, call, [Nc, M, F, A, infinity]),
    catch ?t:stop_node(Nc),
    %%
    {ok, Nd} = ?t:start_node(list_to_atom(Name++"_d"), slave, 
			      [{args, "-pa " ++ PA}]),
    Fun(d, block_call, [Nd, M, F, A]),
    catch ?t:stop_node(Nd),
    {ok, Ne} = ?t:start_node(list_to_atom(Name++"_e"), slave, 
			      [{args, "-pa " ++ PA}]),
    Fun(e, block_call, [Ne, M, F, A, infinity]),
    catch ?t:stop_node(Ne),
    {ok, Nf} = ?t:start_node(list_to_atom(Name++"_f"), slave, 
			      [{args, "-pa " ++ PA}]),
    Fun(f, block_call, [Nf, M, F, A, infinity]),
    catch ?t:stop_node(Nf),
    ok.



called_throws(doc) ->
    "OTP-3766";
called_throws(Config) when is_list(Config) ->
    Timetrap = ?t:timetrap(?t:seconds(10)),
    ?line PA = filename:dirname(code:which(?MODULE)),
    %%
    ?line {ok, N} = ?t:start_node(rpc_SUITE_called_throws, slave, 
				  [{args, "-pa " ++ PA}]),
    %%
    ?line rep(fun (Tag, Call, Args) ->
		      {Tag,up} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, erlang, throw, [up]),
    ?line rep(fun (Tag, Call, Args) ->
		      {Tag,{badrpc,{'EXIT',reason}}} = 
			  {Tag,apply(rpc, Call, Args)}
	      end, N, erlang, throw, [{'EXIT',reason}]),
    %%
    ?line ?t:stop_node(N),
    ?t:timetrap_cancel(Timetrap),
    ok.



call_benchmark(Config) when is_list(Config) ->
    Timetrap = ?t:timetrap(?t:seconds(120)),
    PA = filename:dirname(code:which(?MODULE)),
    {ok, Node} = ?t:start_node(rpc_SUITE_call_benchmark, slave,
			       [{args, "-pa " ++ PA}]),
    Iter = case erlang:system_info(modified_timing_level) of
	       undefined -> 10000;
	       _ -> 500		     %Modified timing - spawn is slower
	   end,
    Res = do_call_benchmark(Node, Iter),
    ?t:stop_node(Node),
    ?t:timetrap_cancel(Timetrap),
    Res.

do_call_benchmark(Node, M) when is_integer(M), M > 0 ->
    {Micros,ok} = timer:tc(fun() ->
				   do_call_benchmark(Node, 0, M)
			   end),
    Calls = 3*M,
    S = io_lib:format("~p RPC calls/second", [Calls*1000000 div Micros]),
    {comment,lists:flatten(S)}.

do_call_benchmark(_Node, M, M) ->
    ok;
do_call_benchmark(Node, I, M) ->
    Node = rpc:call(Node, erlang, node, []),
    _ = rpc:call(Node, erlang, whereis, [rex]),
    3 = rpc:call(Node, erlang, '+', [1,2]),
    do_call_benchmark(Node, I+1, M).

async_call(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:seconds(120)),

    %% Note: First part of nodename sets response delay in seconds.
    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line NodeArgs = [{args,"-pa "++ PA}],
    ?line {ok,Node1} = ?t:start_node('1_rpc_SUITE_call', slave, NodeArgs),
    ?line {ok,Node2} = ?t:start_node('10_rpc_SUITE_call', slave, NodeArgs),
    ?line {ok,Node3} = ?t:start_node('20_rpc_SUITE_call', slave, NodeArgs),
    ?line Promise1 = rpc:async_call(Node1, ?MODULE, f, []),
    ?line Promise2 = rpc:async_call(Node2, ?MODULE, f, []),
    ?line Promise3 = rpc:async_call(Node3, ?MODULE, f, []),

    %% Test fast timeouts.
    ?line timeout = rpc:nb_yield(Promise2),
    ?line timeout = rpc:nb_yield(Promise2, 10),

    %% Let Node1 finish its work before yielding.
    ?t:sleep(?t:seconds(2)),
    ?line {hej,_,Node1} = rpc:yield(Promise1),

    %% Wait for the Node2 and Node3.
    ?line {value,{hej,_,Node2}} = rpc:nb_yield(Promise2, infinity),
    ?line {hej,_,Node3} = rpc:yield(Promise3),

    ?t:timetrap_cancel(Dog),
    ok.

%%%
%%% Utility functions.
%%%

flush(L) ->
    receive
	M ->
	    flush([M|L])
    after 0 ->
	    L
    end.

t() ->
    [N | _] = string:tokens(atom_to_list(node()), "_"),
    1000*list_to_integer(N).

f() ->
    timer:sleep(T=t()),
    spawn(?MODULE, f2, []),
    {hej,T,node()}.

f2() ->
    timer:sleep(500),
    halt().
