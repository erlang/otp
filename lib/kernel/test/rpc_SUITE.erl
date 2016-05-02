%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

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



%% Test different rpc calls.
call(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    %% Note. First part of nodename sets response delay in seconds
    {ok, N1} = test_server:start_node('3_rpc_SUITE_call', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N2} = test_server:start_node('1_rcp_SUITE_call', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N3} = test_server:start_node('4_rcp_SUITE_call', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N4} = test_server:start_node('8_rcp_SUITE_call', slave,
				      [{args, "-pa " ++ PA}]),
    ok = io:format("~p~n", [[N1, N2, N3]]),
    {hej,_,N1} = rpc:call(N1, ?MODULE, f, []),
    {hej,_,N2} = rpc:call(N2, ?MODULE, f, [], 2000),
    {badrpc,timeout} = rpc:call(N3, ?MODULE, f, [], 2000),
    receive after 6000 -> ok end,
    [] = flush([]),
    {hej,_,N4} = rpc:call(N4, ?MODULE, f, []),
    test_server:stop_node(N1),
    test_server:stop_node(N2),
    test_server:stop_node(N3),
    test_server:stop_node(N4),
    ok.

%% Test different rpc calls.
block_call(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    %% Note. First part of nodename sets response delay in seconds
    {ok, N1} = test_server:start_node('3_rpc_SUITE_block_call', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N2} = test_server:start_node('1_rcp_SUITE_block_call', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N3} = test_server:start_node('4_rcp_SUITE_block_call', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N4} = test_server:start_node('8_rcp_SUITE_block_call', slave,
				      [{args, "-pa " ++ PA}]),
    ok = io:format("~p~n", [[N1, N2, N3]]),
    {hej,_,N1} = rpc:block_call(N1, ?MODULE, f, []),
    {hej,_,N2} = rpc:block_call(N2, ?MODULE, f, [], 2000),
    {badrpc,timeout} = rpc:block_call(N3, ?MODULE, f, [], 2000),
    receive after 6000 -> ok end,
    [] = flush([]),
    {hej,_,N4} = rpc:block_call(N4, ?MODULE, f, []),
    test_server:stop_node(N1),
    test_server:stop_node(N2),
    test_server:stop_node(N3),
    test_server:stop_node(N4),
    ok.


%% OTP-3449.
multicall(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    %% Note. First part of nodename sets response delay in seconds
    {ok, N1} = test_server:start_node('3_rpc_SUITE_multicall', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N2} = test_server:start_node('1_rcp_SUITE_multicall', slave,
				      [{args, "-pa " ++ PA}]),
    ok = io:format("~p~n", [[N1, N2]]),
    {[{hej,_,N1},{hej,_,N2}],[]} =
	rpc:multicall([N1, N2], ?MODULE, f, []),
    Msgs = flush([]),
    [] = Msgs,
    test_server:stop_node(N1),
    test_server:stop_node(N2),
    ok.

multicall_timeout(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    %% Note. First part of nodename sets response delay in seconds
    {ok, N1} = test_server:start_node('11_rpc_SUITE_multicall', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N2} = test_server:start_node('8_rpc_SUITE_multicall', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N3} = test_server:start_node('5_rpc_SUITE_multicall', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N4} = test_server:start_node('2_rcp_SUITE_multicall', slave,
				      [{args, "-pa " ++ PA}]),
    ok = io:format("~p~n", [[N1, N2]]),
    {[{hej,_,N3},{hej,_,N4}],[N1, N2]} =
	rpc:multicall([N3, N1, N2, N4], ?MODULE, f, [], 6000),
    ct:sleep({seconds,8}),			%Wait for late answers
    Msgs = flush([]),
    [] = Msgs,
    test_server:stop_node(N1),
    test_server:stop_node(N2),
    test_server:stop_node(N3),
    test_server:stop_node(N4),
    ok.

multicall_dies(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    {ok, N1} = test_server:start_node('rpc_SUITE_multicall_dies_1', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N2} = test_server:start_node('rcp_SUITE_multicall_dies_2', slave,
				      [{args, "-pa " ++ PA}]),
    Nodes = [N1, N2],
    %%
    {[{badrpc, {'EXIT', normal}}, {badrpc, {'EXIT', normal}}], []} =
	do_multicall(Nodes, erlang, exit, [normal]),
    {[{badrpc, {'EXIT', abnormal}}, {badrpc, {'EXIT', abnormal}}], []} =
	do_multicall(Nodes, erlang, exit, [abnormal]),
    {[{badrpc, {'EXIT', {badarith, _}}},
      {badrpc, {'EXIT', {badarith, _}}}],
     []} =
	do_multicall(Nodes, erlang, 'div', [1, 0]),
    {[{badrpc, {'EXIT', {badarg, _}}},
      {badrpc, {'EXIT', {badarg, _}}}],
     []} =
	do_multicall(Nodes, erlang, atom_to_list, [1]),
    {[{badrpc, {'EXIT', {undef, _}}},
      {badrpc, {'EXIT', {undef, _}}}],
     []} =
	do_multicall(Nodes, ?MODULE, suicide, []),
    {[timeout, timeout], []} =
	do_multicall(Nodes, ?MODULE, suicide, [link, normal]),
    {[{badrpc, {'EXIT', abnormal}}, {badrpc, {'EXIT', abnormal}}], []} =
	do_multicall(Nodes, ?MODULE, suicide, [link, abnormal]),
    {[timeout, timeout], []} =
	do_multicall(Nodes, ?MODULE, suicide, [exit, normal]),
    {[{badrpc, {'EXIT', abnormal}}, {badrpc, {'EXIT', abnormal}}], []} =
	do_multicall(Nodes, ?MODULE, suicide, [exit, abnormal]),
    {[{badrpc, {'EXIT', killed}}, {badrpc, {'EXIT', killed}}], []} =
	do_multicall(Nodes, ?MODULE, suicide, [exit, kill]),
    %%
    test_server:stop_node(N1),
    test_server:stop_node(N2),
    ok.

do_multicall(Nodes, Mod, Func, Args) ->
    ok = io:format("~p:~p~p~n", [Mod, Func, Args]),
    Result = rpc:multicall(Nodes, Mod, Func, Args),
    Msgs = flush([]),
    [] = Msgs,
    Result.



multicall_node_dies(Config) when is_list(Config) ->
    do_multicall_2_nodes_dies(?MODULE, suicide, [erlang, halt, []]),
    do_multicall_2_nodes_dies(?MODULE, suicide, [init, stop, []]),
    do_multicall_2_nodes_dies(?MODULE, suicide, [rpc, stop, []]),
    ok.

do_multicall_2_nodes_dies(Mod, Func, Args) ->
    ok = io:format("~p:~p~p~n", [Mod, Func, Args]),
    PA = filename:dirname(code:which(?MODULE)),
    {ok, N1} = test_server:start_node('rpc_SUITE_multicall_node_dies_1', slave,
				      [{args, "-pa " ++ PA}]),
    {ok, N2} = test_server:start_node('rcp_SUITE_multicall_node_dies_2', slave,
				      [{args, "-pa " ++ PA}]),
    Nodes = [N1, N2],
    {[], Nodes} = rpc:multicall(Nodes, Mod, Func, Args),
    Msgs = flush([]),
    [] = Msgs,
    ok.



%% OTP-3766.
called_dies(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    {ok, N} = test_server:start_node(rpc_SUITE_called_dies, slave,
				     [{args, "-pa " ++ PA}]),
    %%
    rep(fun (Tag, Call, Args) ->
		{Tag,{badrpc,{'EXIT',normal}}} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, erlang, exit, [normal]),
    rep(fun (Tag, Call, Args) ->
		{Tag,{badrpc,{'EXIT',abnormal}}} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, erlang, exit, [abnormal]),
    rep(fun (Tag, Call, Args) ->
		{Tag,{badrpc,{'EXIT',{badarith,_}}}} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, erlang, 'div', [1,0]),
    rep(fun (Tag, Call, Args) ->
		{Tag,{badrpc,{'EXIT',{badarg,_}}}} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, erlang, atom_to_list, [1]),
    rep(fun (Tag, Call, Args) ->
		{Tag,{badrpc,{'EXIT',{undef,_}}}} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, ?MODULE, suicide, []),
    %%
    TrapExit = process_flag(trap_exit, true),
    %%
    rep(fun (Tag, Call, Args=[Node|_]) when Node == node() ->
		{Tag,timeout} =
		    {Tag,apply(rpc, Call, Args)},
		{Tag,flush,[{'EXIT',_,normal}]} =
		    {Tag,flush,flush([])};
	    (Tag, Call, Args) ->
		{Tag,timeout} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, ?MODULE, suicide, [link,normal]),
    rep(fun (Tag, Call, Args=[Node|_]) when Node == node() ->
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
    rep(fun (Tag, Call, Args=[Node|_]) when Node == node() ->
		{Tag,timeout} =
		    {Tag,apply(rpc, Call, Args)},
		{Tag,flush,[{'EXIT',_,normal}]} =
		    {Tag,flush,flush([])};
	    (Tag, Call, Args) ->
		{Tag,timeout} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, ?MODULE, suicide, [exit,normal]),
    rep(fun (Tag, Call, Args=[Node|_]) when Node == node() ->
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
    rep(fun %% A local [exit,kill] would kill the test case process
	    (_Tag, _Call, [Node|_]) when Node == node() ->
	       ok;
	    %% A block_call [exit,kill] would kill the rpc server
	    (_Tag, block_call, _Args) -> ok;
	    (Tag, Call, Args) ->
	       {Tag,{badrpc,{'EXIT',killed}}} =
		   {Tag,apply(rpc, Call, Args)}
       end, N, ?MODULE, suicide, [exit,kill]),
    %%
    [] = flush([]),
    test_server:stop_node(N),
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



called_node_dies(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),

    node_rep(
      fun (Call, Args) ->
	      {badrpc,nodedown} = apply(rpc, Call, Args)
      end, "rpc_SUITE_called_node_dies_1",
      PA, ?MODULE, suicide, [erlang,halt,[]]),

    node_rep(
      fun (Call, Args) ->
	      {badrpc,nodedown} = apply(rpc, Call, Args)
      end, "rpc_SUITE_called_node_dies_2",
      PA, ?MODULE, suicide, [init,stop,[]]),

    node_rep(
      fun (Call, Args=[_|_]) ->
	      {badrpc,{'EXIT',{killed,_}}} = apply(rpc, Call, Args)
      end, "rpc_SUITE_called_node_dies_3",
      PA, ?MODULE, suicide, [erlang,exit,[rex,kill]]),

    node_rep(
      fun (block_call, _Args) ->
	      %% Cannot block call rpc - will hang
	      ok;
	  (Call, Args=[_|_]) ->
	      {badrpc,{'EXIT',{normal,_}}} = apply(rpc, Call, Args)
      end, "rpc_SUITE_called_node_dies_4",
      PA, ?MODULE, suicide, [rpc,stop,[]]),

    ok.

node_rep(Fun, Name, PA, M, F, A) ->
    node_rep_call(a, call, [M,F,A], Fun, Name, PA),
    node_rep_call(b, call, [M,F,A,infinity], Fun, Name, PA),
    node_rep_call(c, block_call, [M,F,A], Fun, Name, PA),
    node_rep_call(d, block_call, [M,F,A,infinity], Fun, Name, PA).

node_rep_call(Tag, Call, Args, Fun, Name0, PA) ->
    Name = list_to_atom(Name0 ++ "_" ++ atom_to_list(Tag)),
    {ok, N} = test_server:start_node(Name, slave,
				     [{args, "-pa " ++ PA}]),
    Fun(Call, [N|Args]),
    catch test_server:stop_node(N),
    ok.

%% OTP-3766.
called_throws(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    %%
    {ok, N} = test_server:start_node(rpc_SUITE_called_throws, slave,
				     [{args, "-pa " ++ PA}]),
    %%
    rep(fun (Tag, Call, Args) ->
		{Tag,up} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, erlang, throw, [up]),
    rep(fun (Tag, Call, Args) ->
		{Tag,{badrpc,{'EXIT',reason}}} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, erlang, throw, [{'EXIT',reason}]),
    %%
    test_server:stop_node(N),
    ok.



call_benchmark(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    {ok, Node} = test_server:start_node(rpc_SUITE_call_benchmark, slave,
					[{args, "-pa " ++ PA}]),
    Iter = case erlang:system_info(modified_timing_level) of
	       undefined -> 10000;
	       _ -> 500		     %Modified timing - spawn is slower
	   end,
    Res = do_call_benchmark(Node, Iter),
    test_server:stop_node(Node),
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
    %% Note: First part of nodename sets response delay in seconds.
    PA = filename:dirname(code:which(?MODULE)),
    NodeArgs = [{args,"-pa "++ PA}],
    {ok,Node1} = test_server:start_node('1_rpc_SUITE_call', slave, NodeArgs),
    {ok,Node2} = test_server:start_node('10_rpc_SUITE_call', slave, NodeArgs),
    {ok,Node3} = test_server:start_node('20_rpc_SUITE_call', slave, NodeArgs),
    Promise1 = rpc:async_call(Node1, ?MODULE, f, []),
    Promise2 = rpc:async_call(Node2, ?MODULE, f, []),
    Promise3 = rpc:async_call(Node3, ?MODULE, f, []),

    %% Test fast timeouts.
    timeout = rpc:nb_yield(Promise2),
    timeout = rpc:nb_yield(Promise2, 10),

    %% Let Node1 finish its work before yielding.
    ct:sleep({seconds,2}),
    {hej,_,Node1} = rpc:yield(Promise1),

    %% Wait for the Node2 and Node3.
    {value,{hej,_,Node2}} = rpc:nb_yield(Promise2, infinity),
    {hej,_,Node3} = rpc:yield(Promise3),

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
