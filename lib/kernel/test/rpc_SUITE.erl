%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2022. All Rights Reserved.
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
-export([off_heap/1,
         call/1, call_reqtmo/1, block_call/1, multicall/1,
         multicall_timeout/1, multicall_reqtmo/1, multicall_dies/1,
         multicall_node_dies/1, called_dies/1, called_node_dies/1, 
	 called_throws/1, call_benchmark/1, async_call/1,
         call_against_old_node/1,
         multicall_mix/1,
         timeout_limit/1,
         call_old_against_new/1,
         multicall_old_against_new/1,
         cast_old_against_new/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([call_func1/1]).

-export([suicide/2, suicide/3, f/0, f2/0]).

-export([call_old_against_new_test/2]).
-export([multicall_old_against_new_test/2]).
-export([cast_old_against_new_test/2]).

-include_lib("common_test/include/ct.hrl").

-ifndef(CT_PEER).
%% This module needs to compile on old nodes...
-define(CT_PEER(), {ok, undefined, undefined}).
-define(CT_PEER(Opts), {ok, undefined, undefined}).
-define(CT_PEER_REL(Opts, Release, PrivDir), {ok, undefined, undefined}).
-endif.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [off_heap, call, call_reqtmo, block_call, multicall,
     multicall_timeout, call_reqtmo, multicall_dies,
     multicall_node_dies, called_dies, called_node_dies,
     called_throws, call_benchmark, async_call,
     call_against_old_node,
     multicall_mix, timeout_limit, call_old_against_new,
     multicall_old_against_new, cast_old_against_new].

groups() -> 
    [].

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    [{testcase, Func}|Config].

end_per_testcase(_Func, _Config) ->
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

off_heap(_Config) ->
    %% The rex server process may receive a huge amount of
    %% messages. Make sure that they are stored off heap to
    %% avoid exessive GCs.
    MQD = message_queue_data,
    {MQD,off_heap} = process_info(whereis(rex), MQD),
    ok.


%% Test different rpc calls.
call(Config) when is_list(Config) ->
    {ok, _P1, N1} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "3"]),
    {ok, _P2, N2} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "1"]),
    {ok, _P3, N3} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "4"]),
    {ok, _P4, N4} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "8"]),
    ok = io:format("~p~n", [[N1, N2, N3, N4]]),
    {hej,_,N1} = rpc:call(N1, ?MODULE, f, []),
    {hej,_,N2} = rpc:call(N2, ?MODULE, f, [], 2000),
    {badrpc,timeout} = rpc:call(N3, ?MODULE, f, [], 2000),
    receive after 6000 -> ok end,
    [] = flush([]),
    {hej,_,N4} = rpc:call(N4, ?MODULE, f, []),
    {badrpc, nodedown} = rpc:call(gurka, ?MODULE, f, []),
    ok.


call_reqtmo(Config) when is_list(Config) ->
    Fun = fun (Node, SendMe, Timeout) ->
                  {badrpc, timeout} = rpc:call(Node, erlang, send,
                                               [self(), SendMe],
                                               Timeout)
          end,
    reqtmo_test(Fun).

reqtmo_test(Test) ->
    %% Tests that we time out in time also when the request itself
    %% does not get through. A typical issue we have had
    %% in the past, is that the timeout has not triggered until
    %% the request has gotten through...
    
    Timeout = 500,
    WaitBlock = 100,
    BlockTime = 1000,

    {ok, Peer, Node} = ?CT_PEER(),

    erpc:call(Node, erts_debug, set_internal_state, [available_internal_state,
                                                     true]),
    
    SendMe = make_ref(),

    erpc:cast(Node, erts_debug, set_internal_state, [block, BlockTime]),
    receive after WaitBlock -> ok end,
    
    Start = erlang:monotonic_time(),

    Test(Node, SendMe, Timeout),

    Stop = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(Stop-Start, native, millisecond),
    io:format("Actual time: ~p ms~n", [Time]),
    true = Time >= Timeout,
    true = Time =< Timeout + 200,
    
    receive SendMe -> ok end,
    
    receive UnexpectedMsg -> ct:fail({unexpected_message, UnexpectedMsg})
    after 0 -> ok
    end,
    
    peer:stop(Peer),
    
    {comment,
     "Timeout = " ++ integer_to_list(Timeout)
     ++ " Actual = " ++ integer_to_list(Time)}.


%% Test different rpc calls.
block_call(Config) when is_list(Config) ->
    {ok, _P1, N1} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "3"]),
    {ok, _P2, N2} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "1"]),
    {ok, _P3, N3} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "4"]),
    {ok, _P4, N4} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "8"]),
    ok = io:format("~p~n", [[N1, N2, N3]]),
    {hej,_,N1} = rpc:block_call(N1, ?MODULE, f, []),
    {hej,_,N2} = rpc:block_call(N2, ?MODULE, f, [], 2000),
    {badrpc,timeout} = rpc:block_call(N3, ?MODULE, f, [], 2000),
    receive after 6000 -> ok end,
    [] = flush([]),
    {hej,_,N4} = rpc:block_call(N4, ?MODULE, f, []),
    ok.


%% OTP-3449.
multicall(Config) when is_list(Config) ->
    {ok, _P1, N1} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "3"]),
    {ok, _P2, N2} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "1"]),
    ok = io:format("~p~n", [[N1, N2]]),
    {[{hej,_,N1},{hej,_,N2}],[]} =
	rpc:multicall([N1, N2], ?MODULE, f, []),
    Msgs = flush([]),
    [] = Msgs,
    ok.

multicall_timeout(Config) when is_list(Config) ->
    {ok, _P1, N1} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "11"]),
    {ok, _P2, N2} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "8"]),
    {ok, _P3, N3} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "5"]),
    {ok, _P4, N4} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "2"]),
    ok = io:format("~p~n", [[N1, N2, N3, N4]]),
    {[{hej,_,N3},{hej,_,N4}],[N1, N2]} =
	rpc:multicall([N3, N1, N2, N4], ?MODULE, f, [], 6000),
    ct:sleep({seconds,8}),			%Wait for late answers
    Msgs = flush([]),
    [] = Msgs,
    ok.

multicall_reqtmo(Config) when is_list(Config) ->
    {ok, Peer1, QuickNode1} = ?CT_PEER(),
    {ok, Peer2, QuickNode2} = ?CT_PEER(),
    Fun = fun (Node, SendMe, Timeout) ->
                  Me = self(),
                  SlowSend = fun () ->
                                     if node() == Node ->
                                             Me ! SendMe,
                                             done;
                                        true ->
                                             done
                                     end
                             end,
                  {[done, done], [Node]}
                      = rpc:multicall([QuickNode1, Node, QuickNode2],
                                      erlang, apply, [SlowSend, []],
                                      Timeout)
          end,
    Res = reqtmo_test(Fun),
    peer:stop(Peer1),
    peer:stop(Peer2),
    Res.


multicall_dies(Config) when is_list(Config) ->
    {ok, P1, N1} = ?CT_PEER(["-connect_all", "false"]),
    {ok, P2, N2} = ?CT_PEER(["-connect_all", "false"]),
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
    peer:stop(P1),
    peer:stop(P2),
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
    {ok, _P1, N1} = ?CT_PEER(#{connection => 0, args => ["-connect_all", "false"]}),
    {ok, _P2, N2} = ?CT_PEER(#{connection => 0, args => ["-connect_all", "false"]}),
    Nodes = [N1, N2],
    case {Mod, Func, rpc:multicall(Nodes, Mod, Func, Args)} of
        {_, _, {[], Nodes}} ->
            ok;
        {init, stop, {OkNs, ErrNs}} ->
            %% The killed reason might reach us before the nodedown...
            Killed = {badrpc, {'EXIT', killed}},
            case length(ErrNs) of
                1 -> [Killed] = OkNs;
                0 -> [Killed, Killed] = OkNs
            end
    end,
        
    Msgs = flush([]),
    [] = Msgs,
    ok.


%% OTP-3766.
called_dies(Config) when is_list(Config) ->
    {ok, P, N} = ?CT_PEER(["-connect_all", "false"]),
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
    rep(fun (Tag, call, Args=[Node|_]) when Node == node() ->
		{Tag,timeout} =
		    {Tag,apply(rpc, call, Args)},
		{Tag,flush,[{'EXIT',_,normal}]} =
		    {Tag,flush,flush([])};
	    (Tag, Call, Args) ->
		{Tag,timeout} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, ?MODULE, suicide, [link,normal]),
    rep(fun (Tag, call, Args=[Node|_]) when Node == node() ->
		{Tag,timeout} =
		    {Tag,apply(rpc, call, Args)},
		{Tag,flush,[{'EXIT',_,abnormal}]} =
		    {Tag,flush,flush([])};
	    (Tag, Call, Args=[Node|_]) when Node == node() ->
		{Tag,timeout} =
		    {Tag,apply(rpc, Call, Args)};
	    (Tag, block_call, Args) ->
		{Tag,timeout} =
		    {Tag,apply(rpc, block_call, Args)};
	    (Tag, Call, Args) ->
		{Tag,{badrpc,{'EXIT',abnormal}}} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, ?MODULE, suicide, [link,abnormal]),
    rep(fun (Tag, call, Args=[Node|_]) when Node == node() ->
		{Tag,timeout} =
		    {Tag,apply(rpc, call, Args)},
		{Tag,flush,[{'EXIT',_,normal}]} =
		    {Tag,flush,flush([])};
	    (Tag, Call, Args) ->
		{Tag,timeout} =
		    {Tag,apply(rpc, Call, Args)}
	end, N, ?MODULE, suicide, [exit,normal]),
    rep(fun (Tag, call, Args=[Node|_]) when Node == node() ->
		{Tag,timeout} =
		    {Tag,apply(rpc, call, Args)},
		{Tag,flush,[{'EXIT',_,abnormal}]} =
		    {Tag,flush,flush([])};
	    (Tag, Call, Args=[Node|_]) when Node == node() ->
		{Tag,timeout} =
		    {Tag,apply(rpc, Call, Args)};
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
    peer:stop(P),
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
    node_rep(
      fun (Call, Args) ->
	      {badrpc,nodedown} = apply(rpc, Call, Args)
      end, ?MODULE, suicide, [erlang,halt,[]]),

    node_rep(
      fun (Call, Args) ->
	      {badrpc,nodedown} = apply(rpc, Call, Args)
      end, ?MODULE, suicide, [init,stop,[]]),

    node_rep(
      fun (block_call, Args=[_|_]) ->
	      {badrpc,{'EXIT',{killed,_}}} = apply(rpc, block_call, Args);
          (call, Args=[_|_]) ->
	      {badrpc,nodedown} = apply(rpc, call, Args)
      end, ?MODULE, suicide, [erlang,exit,[rex,kill]]),

    node_rep(
      fun (block_call, _Args) ->
	      %% Cannot block call rpc - will hang
	      ok;
	  (Call, Args=[_|_]) ->
	      {badrpc,nodedown} = apply(rpc, Call, Args)
      end, ?MODULE, suicide, [rpc,stop,[]]),

    ok.

node_rep(Fun, M, F, A) ->
    node_rep_call(call, [M,F,A], Fun),
    node_rep_call(call, [M,F,A,infinity], Fun),
    node_rep_call(block_call, [M,F,A], Fun),
    node_rep_call(block_call, [M,F,A,infinity], Fun).

node_rep_call(Call, Args, Fun) ->
    {ok, _P, N} = ?CT_PEER(#{connection => 0, args => ["-connect_all", "false"]}),
    Fun(Call, [N|Args]),
    ok.

%% OTP-3766.
called_throws(Config) when is_list(Config) ->
    {ok, P, N} = ?CT_PEER(["-connect_all", "false"]),
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
    peer:stop(P),
    ok.



call_benchmark(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(["-connect_all", "false"]),
    Iter = case erlang:system_info(modified_timing_level) of
	       undefined -> 10000;
	       _ -> 500		     %Modified timing - spawn is slower
	   end,
    Res = do_call_benchmark(Node, Iter),
    peer:stop(Peer),
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
    {ok, _Peer1, Node1} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "1"]),
    {ok, _Peer2, Node2} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "10"]),
    {ok, _Peer3, Node3} = ?CT_PEER(["-connect_all", "false", "-rpc_test_delay", "20"]),
    %% Note: First part of nodename sets response delay in seconds.
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

call_against_old_node(Config) ->
    {OldRelName, OldRel} = old_release(),
    case ?CT_PEER_REL(#{connection => 0},
                      OldRelName,
                      proplists:get_value(priv_dir, Config)) of
        not_available ->
	    {skipped, "No OTP "++OldRel++" available"};
        {ok, PeerOld, NodeOld} ->
            NodeOld = rpc:call(NodeOld, erlang, node, []),
            peer:stop(PeerOld),
            ok
    end.

multicall_mix(Config) ->
    {ok, _Peer1, Node1} = ?CT_PEER(#{connection => 0}),
    {ok, Peer2, Node2} = ?CT_PEER(#{connection => 0}),
    {OldRelName, OldRel} = old_release(),
    {{ok, _Peer3, Node3}, OldNodeTest}
        = case ?CT_PEER_REL(#{connection => 0},
                            OldRelName,
                            proplists:get_value(priv_dir, Config)) of
              {ok, _, _} = OldNRes ->
                  {OldNRes, true};
              not_available ->
                  {?CT_PEER(#{connection => 0}), false}
          end,
    {ok, _Peer4, Node4} = ?CT_PEER(#{connection => 0}),
    {ok, _Peer5, Node5} = ?CT_PEER(#{connection => 0}),
    peer:stop(Peer2),
    
    [] = flush([]),

    ThisNode = node(),
    Nodes = [ThisNode, Node1, Node2, Node3, Node4, Node5],
    
    {[ThisNode,
      Node1,
      Node3,
      Node4,
      Node5],
     [Node2]}
        = rpc:multicall(Nodes, erlang, node, []),
    
    [] = flush([]),

    {[Node5,
      ThisNode,
      Node1,
      Node3,
      Node4,
      Node5,
      Node1,
      ThisNode],
     [Node2]}
        = rpc:multicall([Node5|Nodes]++[Node1, ThisNode], erlang, node, []),
    
    [] = flush([]),

    {[BlingError,
      BlingError,
      {badrpc, {'EXIT', _}},
      BlingError,
      BlingError],
     [Node2]}
        = rpc:multicall(Nodes, ?MODULE, call_func1, [bling]),

    [] = flush([]),

    {badrpc, {'EXIT',
              {bling,
               [{?MODULE, call_func2, A, _},
                {?MODULE, call_func1, 1, _}]}}} = BlingError,
    true = (A == 1) orelse (A == [bling]),

    {[], Nodes}
        = rpc:multicall(Nodes, timer, sleep, [100], 50),

    OtherNodes = Nodes -- [ThisNode],

    [] = flush([]),

    
    {[ThisNode,
      Node1,
      Node3,
      Node4,
      Node5],
     [Node2, badnodename]}
        = rpc:multicall(Nodes ++ [badnodename], erlang, node, []),

    try
        rpc:multicall(Nodes ++ [<<"badnodename">>], erlang, node, []),
        ct:fail(unexpected)
    catch
        error:_ ->
            ok
    end,

    [] = flush([]),

    try
        rpc:multicall([Node1, Node2, Node3, Node4, Node5 | ThisNode], erlang, node, []),
        ct:fail(unexpected)
    catch
        error:_ ->
            ok
    end,

    [] = flush([]),

    try
        rpc:multicall(Nodes, erlang, node, [], (1 bsl 32)),
        ct:fail(unexpected)
    catch
        error:_ ->
            ok
    end,
    
    [] = flush([]),

    {[], OtherNodes}
        = rpc:multicall(OtherNodes, erlang, halt, []),

    [] = flush([]),

    case OldNodeTest of
        true -> {comment, "Test with OTP "++OldRel++" node as well"};
        false -> {comment, "Test without OTP "++OldRel}
    end.

call_func1(X) ->
    call_func2(X),
    ok.

call_func2(X) ->
    erlang:error(X, [X]).

timeout_limit(Config) when is_list(Config) ->
    Node = node(),
    MaxTmo = (1 bsl 32) - 1,
    erlang:send_after(100, self(), dummy_message),
    try
        receive
            M ->
                M
        after MaxTmo + 1 ->
                ok
        end,
        ct:fail("The ?MAX_INT_TIMEOUT define in rpc.erl needs "
                "to be updated to reflect max timeout value "
                "in a receive/after...")
    catch
        error:timeout_value ->
            ok
    end,
    Node = rpc:call(Node, erlang, node, [], MaxTmo),
    try
        {badrpc, _} = rpc:call(Node, erlang, node, [], MaxTmo+1),
        ct:fail(unexpected)
    catch
        error:_ ->
            ok
    end,
    Node = rpc:block_call(Node, erlang, node, [], MaxTmo),
    try
        {badrpc, _} = rpc:block_call(Node, erlang, node, [], MaxTmo+1),
        ct:fail(unexpected)
    catch
        error:_ ->
            ok
    end,
    {[Node],[]} = rpc:multicall([Node], erlang, node, [], MaxTmo),
    try
        rpc:multicall([Node], erlang, node, [], MaxTmo+1),
        ct:fail(unexpected)
    catch
        error:_ ->
            ok
    end,
    ok.
    

call_old_against_new(Config) ->
    try
        test_on_old_node(Config, call_old_against_new_test, 1, 1)
    catch
        throw:{skipped, _} = Skipped ->
            Skipped
    end.

call_old_against_new_test([NodeOld], [NodeCurr]) ->
    %% Executed on an OTP old node

    NodeOld = rpc:call(NodeOld, erlang, node, []),
    NodeCurr = rpc:call(NodeCurr, erlang, node, []),

    {badrpc, {'EXIT', bang}} = rpc:call(NodeOld, erlang, exit, [bang]),
    {badrpc, {'EXIT', bang}} = rpc:call(NodeCurr, erlang, exit, [bang]),

    {badrpc, {'EXIT', {blong, _}}} = rpc:call(NodeOld, erlang, error, [blong]),
    {badrpc, {'EXIT', {blong, _}}} = rpc:call(NodeCurr, erlang, error, [blong]),

    bling = rpc:call(NodeOld, erlang, throw, [bling]),
    bling = rpc:call(NodeCurr, erlang, throw, [bling]),

    {badrpc, timeout} = rpc:call(NodeOld, timer, sleep, [1000], 100),
    {badrpc, timeout} = rpc:call(NodeCurr, timer, sleep, [1000], 100),

    {badrpc, nodedown} = rpc:call(NodeOld, erlang, halt, []),
    {badrpc, nodedown} = rpc:call(NodeCurr, erlang, halt, []),

    {badrpc, nodedown} = rpc:call(NodeOld, erlang, node, []),
    {badrpc, nodedown} = rpc:call(NodeCurr, erlang, node, []),

    ok.
    
multicall_old_against_new(Config) ->
    try
        test_on_old_node(Config, multicall_old_against_new_test, 2, 2)
    catch
        throw:{skipped, _} = Skipped ->
            Skipped
    end.

multicall_old_against_new_test([NodeOldA, NodeOldB], [NodeCurrA, NodeCurrB]) ->
    %% Executed on an OTP old node

    AllNodes = [NodeCurrA, NodeOldA, NodeCurrB, NodeOldB],
    NoNodes = length(AllNodes),

    {AllNodes, []} = rpc:multicall(AllNodes, erlang, node, []),

    Bang = lists:duplicate(NoNodes, {badrpc, {'EXIT', bang}}),
    {Bang, []} = rpc:multicall(AllNodes, erlang, exit, [bang]),

    {[{badrpc, {'EXIT', {blong, _}}},
      {badrpc, {'EXIT', {blong, _}}},
      {badrpc, {'EXIT', {blong, _}}},
      {badrpc, {'EXIT', {blong, _}}}], []} = rpc:multicall(AllNodes, erlang, error, [blong]),

    Bling = lists:duplicate(NoNodes, bling),
    {Bling, []} = rpc:multicall(AllNodes, erlang, throw, [bling]),

    {[], AllNodes} = rpc:multicall(AllNodes, timer, sleep, [1000], 100),

    {AllNodes, []} = rpc:multicall(AllNodes, erlang, node, []),

    {[], AllNodes} = rpc:multicall(AllNodes, erlang, halt, []),

    {[], AllNodes} = rpc:multicall(AllNodes, erlang, node, []),

    ok.

cast_old_against_new(Config) ->
    try
        test_on_old_node(Config, cast_old_against_new_test, 1, 1)
    catch
        throw:{skipped, _} = Skipped ->
            Skipped
    end.

cast_old_against_new_test([NodeOld], [NodeCurr]) ->
    %% Executed on an OTP old node

    Me = self(),
    Ref = make_ref(),
    true = rpc:cast(NodeOld, erlang, send, [Me, {Ref, 1}]),
    receive {Ref, 1} -> ok end,
    true = rpc:cast(NodeCurr, erlang, send, [Me, {Ref, 2}]),
    receive {Ref, 2} -> ok end,

    true = rpc:cast(NodeOld, erlang, halt, []),
    true = rpc:cast(NodeCurr, erlang, halt, []),

    monitor_node(NodeOld, true),
    receive {nodedown, NodeOld} -> ok end,
    monitor_node(NodeCurr, true),
    receive {nodedown, NodeCurr} -> ok end,

    true = rpc:cast(NodeOld, erlang, send, [Me, {Ref, 3}]),
    true = rpc:cast(NodeCurr, erlang, send, [Me, {Ref, 4}]),

    receive Msg -> error({unexcpected_message, Msg})
    after 1000 -> ok
    end.

%%%
%%% Utility functions.
%%%

old_release() ->
    OldRel = integer_to_list(list_to_integer(erlang:system_info(otp_release))-2),
    {OldRel++"_latest", OldRel}.

flush(L) ->
    receive
	M ->
	    flush([M|L])
    after 0 ->
	    L
    end.

t() ->
    Delay = case init:get_argument(rpc_test_delay) of
                {ok,[[D]]} -> list_to_integer(D);
                _ -> 0
            end,
    1000*Delay.

f() ->
    timer:sleep(T=t()),
    spawn(?MODULE, f2, []),
    {hej,T,node()}.

f2() ->
    timer:sleep(500),
    halt().

test_on_old_node(Config, Test, NoOld, NoCurr) ->
    {OldRelName, OldRel} = old_release(),
    NodesOldRes = lists:map(fun (_) ->
                                    case ?CT_PEER_REL(#{connection => 0},
                                                      OldRelName,
                                                      proplists:get_value(priv_dir, Config)) of
                                        not_available ->
                                            throw({skipped, "No OTP "++OldRel++" available"});
                                        {ok, _, _} = Ok ->
                                            Ok
                                    end
                            end,
                            lists:seq(1, NoOld+1)),
    NodesCurrRes = lists:map(fun (_) ->
                                     {ok, _, _} = ?CT_PEER()
                             end,
                             lists:seq(1, NoCurr)),
    NFun = fun ({ok, _Peer, Node}) -> Node end,
    NodesOld = lists:map(NFun, NodesOldRes),
    NodesCurr = lists:map(NFun, NodesCurrRes),

    %% Recompile rpc_SUITE on old node and load it on all old nodes...
    SrcFile = filename:rootname(code:which(?MODULE)) ++ ".erl",
    {ok, ?MODULE, BeamCode} = rpc:call(hd(NodesOld), compile, file, [SrcFile, [binary]]),
    LoadResult = lists:duplicate(length(NodesOld), {module, ?MODULE}),
    {LoadResult, []} = rpc:multicall(NodesOld, code, load_binary, [?MODULE, SrcFile, BeamCode]),
    %% Execute test on first old node...
    Pid = spawn_link(hd(NodesOld), ?MODULE, Test, [tl(NodesOld), NodesCurr]),
    Mon = erlang:monitor(process, Pid),
    receive
        {'DOWN', Mon, process, Pid, Reason} when Reason == normal; Reason == noproc ->
            ok
    end,
    ok.
