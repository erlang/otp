%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2022. All Rights Reserved.
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
-module(erpc_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([call/1, call_against_old_node/1,
         call_from_old_node/1,
         call_reqtmo/1, call_against_ei_node/1, cast/1,
         send_request/1, send_request_fun/1,
         send_request_receive_reqtmo/1,
         send_request_wait_reqtmo/1,
         send_request_check_reqtmo/1,
         send_request_against_ei_node/1,
         send_request_receive_reqid_collection/1,
         send_request_wait_reqid_collection/1,
         send_request_check_reqid_collection/1,
         multicall/1, multicall_reqtmo/1,
         multicall_recv_opt/1,
         multicall_recv_opt2/1,
         multicall_recv_opt3/1,
         multicast/1,
         timeout_limit/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([call_test/4]).

-export([call_func1/1, call_func2/1, call_func4/4]).

-export([f/0, f2/0]).

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
    [call,
     call_against_old_node,
     call_from_old_node,
     call_reqtmo,
     call_against_ei_node,
     cast,
     send_request,
     send_request_fun,
     send_request_receive_reqtmo,
     send_request_wait_reqtmo,
     send_request_check_reqtmo,
     send_request_against_ei_node,
     send_request_receive_reqid_collection,
     send_request_wait_reqid_collection,
     send_request_check_reqid_collection,
     multicall,
     multicall_reqtmo,
     multicall_recv_opt,
     multicall_recv_opt2,
     multicall_recv_opt3,
     multicast,
     timeout_limit].

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

call(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    call_test(Peer, Node, true, false).

call_against_old_node(Config) when is_list(Config) ->
    {OldRelName, OldRel} = old_release(),
    case ?CT_PEER_REL(#{connection => 0},
                      OldRelName,
                      proplists:get_value(priv_dir, Config)) of
        not_available ->
            {skipped, "Not able to start an OTP "++OldRel++" node"};
        {ok, Peer, Node} ->
            compile_and_load_on_node(Node),
            call_test(Peer, Node, false, false),
            ok
    end.

call_from_old_node(Config) when is_list(Config) ->
    {OldRelName, OldRel} = old_release(),
    case ?CT_PEER_REL(#{connection => 0},
                      OldRelName,
                      proplists:get_value(priv_dir, Config)) of
        not_available ->
            {skipped, "Not able to start an OTP "++OldRel++" node"};
        {ok, Peer, Node} ->
            try
                compile_and_load_on_node(Node),
                ok = erpc:call(Node, ?MODULE, call_test, [undefined, node(), false, true])
            after
                peer:stop(Peer)
            end
    end.

call_test(Peer, Node, NodesOfSameRelease, ToTestServer) ->
    if NodesOfSameRelease ->
            call_test(Peer, node(), 10000, NodesOfSameRelease, ToTestServer),
            call_test(Peer, node(), infinity, NodesOfSameRelease, ToTestServer),
            try
                erpc:call(node(), timer, sleep, [100], 10),
                ct:fail(unexpected)
            catch
                error:{erpc, timeout} ->
                    ok
            end,
            try
                erpc:call(node(), fun () -> timer:sleep(100) end, 10),
                ct:fail(unexpected)
            catch
                error:{erpc, timeout} ->
                    ok
            end;
       true ->
            ok
    end,
    call_test(Peer, Node, 10000, NodesOfSameRelease, ToTestServer),
    call_test(Peer, Node, infinity, NodesOfSameRelease, ToTestServer),
    try
        erpc:call(Node, timer, sleep, [100], 10),
        ct:fail(unexpected)
    catch
        error:{erpc, timeout} ->
            ok
    end,
    if NodesOfSameRelease ->
            try
                erpc:call(Node, fun () -> timer:sleep(100) end, 10),
                ct:fail(unexpected)
            catch
                error:{erpc, timeout} ->
                    ok
            end;
       true ->
            ok
    end,
    if not ToTestServer ->
            try
                erpc:call(Node, erlang, halt, []),
                ct:fail(unexpected)
            catch
                error:{erpc, noconnection} ->
                    ok
            end,
            if NodesOfSameRelease ->
                    try
                        erpc:call(Node, fun () -> erlang:node() end),
                        ct:fail(unexpected)
                    catch
                        error:{erpc, noconnection} ->
                            ok
                    end;
               true ->
                    ok
            end,
            try
                erpc:call(Node, erlang, node, []),
                ct:fail(unexpected)
            catch
                error:{erpc, noconnection} ->
                    ok
            end;
       true ->
            ok
    end,
    try
        erpc:call(badnodename, erlang, node, []),
        ct:fail(unexpected)
    catch
        error:{erpc, noconnection} ->
            ok
    end,

    receive after 1000 -> ok end,
    [] = flush([]),
    ok.

call_test(Peer, Node, Timeout, NodesOfSameRelease, ToTestServer) ->
    io:format("call_test(~p, ~p, ~p, ~p, ~p)~n",
              [Peer, Node, Timeout, NodesOfSameRelease, ToTestServer]),
    Node = erpc:call(Node, erlang, node, [], Timeout),
    try
        erpc:call(Node, erlang, error, [oops|invalid], Timeout),
        ct:fail(unexpected)
    catch
        error:{exception, badarg, [{erlang,apply,[erlang,error,[oops|invalid]],_}]} ->
            ok
    end,
    try
        erpc:call(Node, erlang, error, [oops], Timeout),
        ct:fail(unexpected)
    catch
        error:{exception, oops, [{erlang,error,[oops],_}]} ->
            ok
    end,
    try
        erpc:call(Node, erlang, exit, [oops], Timeout),
        ct:fail(unexpected)
    catch
        exit:{exception, oops} ->
            ok
    end,
    if NodesOfSameRelease ->
            try
                erpc:call(Node, fun () -> erlang:exit(oops) end, Timeout),
                ct:fail(unexpected)
            catch
                exit:{exception, oops} ->
                    ok
            end;
       true ->
            ok
    end,
    try
        erpc:call(Node, erlang, throw, [oops], Timeout),
        ct:fail(unexpected)
    catch
        throw:oops ->
            ok
    end,
    if NodesOfSameRelease ->
            try
                erpc:call(Node, fun () -> erlang:throw(oops) end, Timeout),
                ct:fail(unexpected)
            catch
                throw:oops ->
                    ok
            end,
            case {node() == Node, Timeout == infinity} of
                {true, true} ->
                    %% This would kill the test since local calls
                    %% without timeout is optimized to execute in
                    %% calling process itself...
                    ok;
                _ ->
                    ExitSignal = fun () ->
                                         exit(self(), oops),
                                         receive after infinity -> ok end
                                 end,
                    try
                        erpc:call(Node, ExitSignal, Timeout),
                        ct:fail(unexpected)
                    catch
                        exit:{signal, oops} ->
                            ok
                    end,
                    try
                        erpc:call(Node, erlang, apply, [ExitSignal, []], Timeout),
                        ct:fail(unexpected)
                    catch
                        exit:{signal, oops} ->
                            ok
                    end
            end;
       true ->
            ok
    end,
    try
        erpc:call(Node, ?MODULE, call_func1, [boom], Timeout),
        ct:fail(unexpected)
    catch
        error:{exception,
               {exception,
                boom,
                [{?MODULE, call_func3, A2, _},
                 {?MODULE, call_func2, 1, _}]},
               [{erpc, call, A1, _},
                {?MODULE, call_func1, 1, _}]}
          when ((A1 == 5)
                orelse (A1 == [Node, ?MODULE, call_func2, [boom]]))
               andalso ((A2 == 1)
                        orelse (A2 == [boom])) ->
            ok
    end,
    if NodesOfSameRelease ->
            try
                erpc:call(Node, fun () -> ?MODULE:call_func1(boom) end, Timeout),
                ct:fail(unexpected)
            catch
                error:{exception,
                       {exception,
                        boom,
                        [{?MODULE, call_func3, A4, _},
                         {?MODULE, call_func2, 1, _}]},
                       [{erpc, call, A3, _},
                        {?MODULE, call_func1, 1, _},
                        {erlang, apply, 2, _}]}
                  when ((A3 == 5)
                        orelse (A3 == [Node, ?MODULE, call_func2, [boom]]))
                       andalso ((A4 == 1)
                                orelse (A4 == [boom])) ->
                    ok
            end;
       true ->
            ok
    end,
    try
        call_func4(Node, node(), 10, Timeout),
        ct:fail(unexpected)
    catch
        error:Error1 ->
%%%            io:format("Error1=~p~n", [Error1]),
            check_call_func4_error(Error1, 10)
    end,
    try
        call_func4(node(), Node, 5, Timeout),
        ct:fail(unexpected)
    catch
        error:Error2 ->
%%%            io:format("Error2=~p~n", [Error2]),
            check_call_func4_error(Error2, 5)
    end,
    ok.

check_call_func4_error({exception,
                         badarg,
                         [{?MODULE, call_func5, _, _},
                          {?MODULE, call_func4, _, _}]},
                         0) ->
    ok;
check_call_func4_error({exception,
                        Exception,
                        [{erpc, call, _, _},
                         {?MODULE, call_func5, _, _},
                         {?MODULE, call_func4, _, _}]},
                       N) ->
    check_call_func4_error(Exception, N-1).
    
call_func1(X) ->
    erpc:call(node(), ?MODULE, call_func2, [X]),
    ok.

call_func2(X) ->
    call_func3(X),
    ok.

call_func3(X) ->
    erlang:error(X, [X]).
    
call_func4(A, B, N, T) ->
    call_func5(A, B, N, T),
    ok.

call_func5(A, B, N, T) when N >= 0 ->
    erpc:call(A, ?MODULE, call_func4, [B, A, N-1, T], T),
    ok;
call_func5(_A, _B, _N, _T) ->
    erlang:error(badarg).

call_against_ei_node(Config) when is_list(Config) ->
    {ok, Node} = start_ei_node(Config),
    %% Once when erpc:call() brings up the connection
    %% and once when the connection is up...
    disconnect(Node),
    try
        erpc:call(Node, erlang, node, []),
        ct:fail(unexpected)
    catch
        error:{erpc, notsup} ->
            ok
    end,
    true = lists:member(Node, nodes(hidden)),
    try
        erpc:call(Node, erlang, node, []),
        ct:fail(unexpected)
    catch
        error:{erpc, notsup} ->
            ok
    end,
    ok = stop_ei_node(Node).

call_reqtmo(Config) when is_list(Config) ->
    Fun = fun (Node, SendMe, Timeout) ->
                  try
                      erpc:call(Node, erlang, send,
                                [self(), SendMe], Timeout),
                      ct:fail(unexpected)
                  catch
                      error:{erpc, timeout} -> ok
                  end
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

cast(Config) when is_list(Config) ->
    %% silently fail
    ok = erpc:cast(badnodename, erlang, send, [hej]),

    try
        erpc:cast(<<"node">>, erlang, send, [hej]),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} -> ok
    end,
    try
        erpc:cast(node(), erlang, send, hej),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} -> ok
    end,
    try
        erpc:cast(node(), "erlang", send, [hej]),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} -> ok
    end,
    try
        erpc:cast(node(), erlang, make_ref(), [hej]),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} -> ok
    end,

    erpc:cast(node(), erlang, send, [self()|blupp]), %% silent remote error...

    Me = self(),
    Ok1 = make_ref(),
    ok = erpc:cast(node(), erlang, send, [Me, {mfa, Ok1}]),
    receive
        {mfa, Ok1} -> ok
    end,
    ok = erpc:cast(node(), fun () -> Me ! {a_fun, Ok1} end),
    receive
        {a_fun, Ok1} -> ok
    end,
    {ok, _Peer, Node} = ?CT_PEER(),
    Ok2 = make_ref(),
    ok = erpc:cast(Node, erlang, send, [Me, {mfa, Ok2}]),
    receive
        {mfa, Ok2} -> ok
    end,
    ok = erpc:cast(Node, fun () -> Me ! {a_fun, Ok2} end),
    receive
        {a_fun, Ok2} -> ok
    end,

    ok = erpc:cast(Node, erlang, halt, []),

    monitor_node(Node, true),
    receive {nodedown, Node} -> ok end,

    ok = erpc:cast(Node, erlang, send, [Me, wont_reach_me]),

    receive after 1000 -> ok end,
    [] = flush([]),

    {ok, EiNode} = start_ei_node(Config),
    %% Both when erpc:cast() brings up the connection
    %% and when the connection is up...
    disconnect(EiNode),

    ok = erpc:cast(EiNode, erlang, send, [Me, wont_reach_me]),
    ok = erpc:cast(EiNode, fun () -> Me ! wont_reach_me end),

    wait_until(fun () -> lists:member(EiNode, nodes(hidden)) end),
    
    ok = erpc:cast(EiNode, erlang, send, [Me, wont_reach_me]),
    ok = erpc:cast(EiNode, fun () -> Me ! wont_reach_me end),

    receive
        Msg -> ct:fail({unexpected_message, Msg})
    after
        2000 -> ok
    end,
    ok = stop_ei_node(EiNode),

    {OldRelName, OldRel} = old_release(),
    case ?CT_PEER_REL(#{connection => 0},
                      OldRelName,
                      proplists:get_value(priv_dir, Config)) of
        not_available ->
            {comment, "Not tested against OTP "++OldRel++" node"};
        {ok, _OldPeer, OldNode} ->
            Ok3 = make_ref(),
            ok = erpc:cast(OldNode, erlang, send, [Me, {mfa, Ok3}]),
            receive
                {mfa, Ok3} -> ok
            end,
            ok = erpc:cast(OldNode, erlang, halt, []),

            ok = erpc:cast(OldNode, erlang, send, [Me, wont_reach_me]),

            receive after 1000 -> ok end,
            [] = flush([]),
            {comment, "Also tested against OTP "++OldRel++" node"}
    end.

send_request(Config) when is_list(Config) ->
    %% Note: First part of nodename sets response delay in seconds.
    {ok, _Peer1, Node1} = ?CT_PEER(["-erpc_test_delay", "1"]),
    {ok, _Peer2, Node2} = ?CT_PEER(["-erpc_test_delay", "10"]),
    {ok, _Peer3, Node3} = ?CT_PEER(["-erpc_test_delay", "20"]),
    ReqId1 = erpc:send_request(Node1, ?MODULE, f, []),
    ReqId2 = erpc:send_request(Node2, ?MODULE, f, []),
    ReqId3 = erpc:send_request(Node3, ?MODULE, f, []),
    ReqId4 = erpc:send_request(Node1, erlang, error, [bang]),
    ReqId5 = erpc:send_request(Node1, ?MODULE, f, []),

    try
        erpc:receive_response(ReqId4, 1000)
    catch
        error:{exception, bang, [{erlang,error,[bang],_}]} ->
            ok
    end,
    try
        erpc:receive_response(ReqId5, 10)
    catch
        error:{erpc, timeout} ->
            ok
    end,

    %% Test fast timeouts.
    no_response = erpc:wait_response(ReqId2),
    no_response = erpc:wait_response(ReqId2, 10),

    %% Let Node1 finish its work before yielding.
    ct:sleep({seconds,2}),
    {hej,_,Node1} = erpc:receive_response(ReqId1),

    %% Wait for the Node2 and Node3.
    {response,{hej,_,Node2}} = erpc:wait_response(ReqId2, infinity),
    {hej,_,Node3} = erpc:receive_response(ReqId3),

    try
        erpc:receive_response(ReqId5, 10),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
            ok
    end,

    receive
        Msg0 -> ct:fail(Msg0)
    after 0 -> ok
    end,

    [] = flush([]),

    {ok, _Peer4, Node4} = ?CT_PEER(),

    ReqId6 = erpc:send_request(Node4, erlang, node, []),
    
    no_response = erpc:check_response({response, Node1}, ReqId6),
    no_response = erpc:check_response(ReqId6, ReqId6),
    receive
        Msg1 ->
            {response, Node4} = erpc:check_response(Msg1, ReqId6)
    end,

    ReqId7 = erpc:send_request(Node4, erlang, halt, []),
    try
        erpc:receive_response(ReqId7),
        ct:fail(unexpected)
    catch
        error:{erpc, noconnection} -> ok
    end,

    ReqId8 = erpc:send_request(Node4, erlang, node, []),
    receive
        Msg2 ->
            try
                erpc:check_response(Msg2, ReqId8),
                ct:fail(unexpected)
            catch
                error:{erpc, noconnection} ->
                    ok
            end
    end,

    [] = flush([]),

    {ok, Node5} = start_ei_node(Config),
    %% Once when erpc:send_request() brings up the connection
    %% and once when the connection is up...
    disconnect(Node5),
    ReqId14 = erpc:send_request(Node5, erlang, node, []),
    try
        erpc:receive_response(ReqId14),
        ct:fail(unexpected)
    catch
        error:{erpc, notsup} -> ok
    end,
    true = lists:member(Node5, nodes(hidden)),
    ReqId15 = erpc:send_request(Node5, erlang, node, []),
    try
        erpc:receive_response(ReqId15),
        ct:fail(unexpected)
    catch
        error:{erpc, notsup} -> ok
    end,

    ok = stop_ei_node(Node5),

    [] = flush([]),

    {OldRelName, OldRel} = old_release(),
    case ?CT_PEER_REL(#{connection => 0},
                      OldRelName,
                      proplists:get_value(priv_dir, Config)) of
        not_available ->
            {comment, "Not tested against OTP "++OldRel++" node"};
        {ok, _OldPeer, OldNode} ->

            ReqId9 = erpc:send_request(OldNode, erlang, node, []),
    
            no_response = erpc:check_response({response, OldNode}, ReqId9),
            no_response = erpc:check_response(ReqId6, ReqId9),
            receive
                Msg3 ->
                    {response, OldNode} = erpc:check_response(Msg3, ReqId9)
            end,

            ReqId10 = erpc:send_request(OldNode, erlang, node, []),
                        
            OldNode = erpc:receive_response(ReqId10),

            ReqId11 = erpc:send_request(OldNode, erlang, node, []),
            {response, OldNode} = erpc:wait_response(ReqId11, infinity),

            ReqId12 = erpc:send_request(OldNode, erlang, halt, []),
            try
                erpc:receive_response(ReqId12),
                ct:fail(unexpected)
            catch
                error:{erpc, noconnection} -> ok
            end,

            ReqId13 = erpc:send_request(OldNode, erlang, node, []),
            receive
                Msg4 ->
                    try
                        erpc:check_response(Msg4, ReqId13),
                        ct:fail(unexpected)
                    catch
                        error:{erpc, noconnection} ->
                            ok
                    end
            end,

            [] = flush([]),
            {comment, "Also tested against OTP "++OldRel++" node"}
    end.

send_request_fun(Config) when is_list(Config) ->
    %% Note: First part of nodename sets response delay in seconds.
    {ok, _Peer1, Node1} = ?CT_PEER(["-erpc_test_delay", "1"]),
    {ok, _Peer2, Node2} = ?CT_PEER(["-erpc_test_delay", "10"]),
    {ok, _Peer3, Node3} = ?CT_PEER(["-erpc_test_delay", "20"]),
    ReqId1 = erpc:send_request(Node1, fun () -> ?MODULE:f() end),
    ReqId2 = erpc:send_request(Node2, fun () -> ?MODULE:f() end),
    ReqId3 = erpc:send_request(Node3, fun () -> ?MODULE:f() end),
    ReqId4 = erpc:send_request(Node1, fun () -> erlang:error(bang) end),
    ReqId5 = erpc:send_request(Node1, fun () -> ?MODULE:f() end),

    try
        erpc:receive_response(ReqId4, 1000)
    catch
        error:{exception, bang, [{?MODULE, _, _, _},
                                 {erlang,apply,2,_}]} ->
            ok
    end,
    try
        erpc:receive_response(ReqId5, 10)
    catch
        error:{erpc, timeout} ->
            ok
    end,

    %% Test fast timeouts.
    no_response = erpc:wait_response(ReqId2),
    no_response = erpc:wait_response(ReqId2, 10),

    %% Let Node1 finish its work before yielding.
    ct:sleep({seconds,2}),
    {hej,_,Node1} = erpc:receive_response(ReqId1),

    %% Wait for the Node2 and Node3.
    {response,{hej,_,Node2}} = erpc:wait_response(ReqId2, infinity),
    {hej,_,Node3} = erpc:receive_response(ReqId3),

    try
        erpc:receive_response(ReqId5, 10),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
            ok
    end,

    receive
        Msg0 -> ct:fail(Msg0)
    after 0 -> ok
    end,

    {ok, _Peer4, Node4} = ?CT_PEER(),

    ReqId6 = erpc:send_request(Node4, fun () -> erlang:node() end),
    
    no_response = erpc:check_response({response, Node1}, ReqId6),
    no_response = erpc:check_response(ReqId6, ReqId6),
    receive
        Msg1 ->
            {response, Node4} = erpc:check_response(Msg1, ReqId6)
    end,

    ReqId7 = erpc:send_request(Node4, erlang, halt, []),
    try
        erpc:receive_response(ReqId7),
        ct:fail(unexpected)
    catch
        error:{erpc, noconnection} -> ok
    end,

    ReqId8 = erpc:send_request(Node4, erlang, node, []),
    receive
        Msg2 ->
            try
                erpc:check_response(Msg2, ReqId8),
                ct:fail(unexpected)
            catch
                error:{erpc, noconnection} ->
                    ok
            end
    end,

    [] = flush([]),

    {ok, Node5} = start_ei_node(Config),
    %% Once when erpc:send_request() brings up the connection
    %% and once when the connection is up...
    disconnect(Node5),
    ReqId9 = erpc:send_request(Node5, fun () -> erlang:node() end),
    try
        erpc:receive_response(ReqId9),
        ct:fail(unexpected)
    catch
        error:{erpc, notsup} -> ok
    end,
    true = lists:member(Node5, nodes(hidden)),
    ReqId10 = erpc:send_request(Node5, fun () -> erlang:node() end),
    try
        erpc:receive_response(ReqId10),
        ct:fail(unexpected)
    catch
        error:{erpc, notsup} -> ok
    end,

    ok = stop_ei_node(Node5).


send_request_receive_reqtmo(Config) when is_list(Config) ->
    Fun = fun (Node, SendMe, Timeout) ->
                  RID = erpc:send_request(Node, erlang, send,
                                          [self(), SendMe]),
                  try
                      erpc:receive_response(RID, Timeout),
                      ct:fail(unexpected)
                  catch
                      error:{erpc, timeout} -> ok
                  end
          end,
    reqtmo_test(Fun).

send_request_wait_reqtmo(Config) when is_list(Config) ->
    Fun = fun (Node, SendMe, Timeout) ->
                  RID = erpc:send_request(Node, erlang, send,
                                          [self(), SendMe]),
                  no_response = erpc:wait_response(RID, 0),
                  no_response = erpc:wait_response(RID, Timeout),
                  %% Cleanup...
                  try
                      erpc:receive_response(RID, 0),
                      ct:fail(unexpected)
                  catch
                      error:{erpc, timeout} -> ok
                  end
          end,
    reqtmo_test(Fun).

send_request_check_reqtmo(Config) when is_list(Config) ->
    Fun = fun (Node, SendMe, Timeout) ->
                  RID = erpc:send_request(Node, erlang, send,
                                          [self(), SendMe]),
                  receive Msg -> erpc:check_response(Msg, RID)
                  after Timeout -> ok
                  end,
                  %% Cleanup...
                  try
                      erpc:receive_response(RID, 0),
                      ct:fail(unexpected)
                  catch
                      error:{erpc, timeout} -> ok
                  end
          end,
    reqtmo_test(Fun).

send_request_against_ei_node(Config) when is_list(Config) ->
    {ok, EiNode} = start_ei_node(Config),
    %% Once when erpc:send_request() brings up the connection
    %% and once when the connection is up...
    disconnect(EiNode),
    RID1 = erpc:send_request(EiNode, erlang, node, []),
    RID2 = erpc:send_request(EiNode, erlang, node, []),
    RID3 = erpc:send_request(EiNode, erlang, node, []),
    try
        erpc:receive_response(RID1),
        ct:fail(unexpected)
    catch
        error:{erpc, notsup} ->
            ok
    end,
    try
        erpc:wait_response(RID2, infinity),
        ct:fail(unexpected)
    catch
        error:{erpc, notsup} ->
            ok
    end,
    try
        receive
            Msg ->
                erpc:check_response(Msg, RID3),
                ct:fail(unexpected)
        end
    catch
        error:{erpc, notsup} ->
            ok
    end,

    true = lists:member(EiNode, nodes(hidden)),

    RID1u = erpc:send_request(EiNode, erlang, node, []),
    RID2u = erpc:send_request(EiNode, erlang, node, []),
    RID3u = erpc:send_request(EiNode, erlang, node, []),
    try
        erpc:receive_response(RID1u),
        ct:fail(unexpected)
    catch
        error:{erpc, notsup} ->
            ok
    end,
    try
        erpc:wait_response(RID2u, infinity),
        ct:fail(unexpected)
    catch
        error:{erpc, notsup} ->
            ok
    end,
    try
        receive
            Msgu ->
                erpc:check_response(Msgu, RID3u),
                ct:fail(unexpected)
        end
    catch
        error:{erpc, notsup} ->
            ok
    end,
    
    ok = stop_ei_node(EiNode).

send_request_receive_reqid_collection(Config) when is_list(Config) ->
    {ok, _P, N} = ?CT_PEER(#{connection => 0}),
    send_request_receive_reqid_collection_success(N),
    send_request_receive_reqid_collection_timeout(N),
    send_request_receive_reqid_collection_error(N),
    ok.

send_request_receive_reqid_collection_success(N) ->

    ReqId0 = erpc:send_request(N, fun () -> 0 end),

    ReqIdC0 = erpc:reqids_new(),

    ReqId1 = erpc:send_request(N, fun () -> receive after 400 -> 400 end end),
    ReqIdC1 = erpc:reqids_add(ReqId1, req1, ReqIdC0),
    1 = erpc:reqids_size(ReqIdC1),

    ReqIdC2 = erpc:send_request(N, fun () -> receive after 1 -> 1 end end, req2, ReqIdC1),
    2 = erpc:reqids_size(ReqIdC2),

    ReqIdC3 = erpc:send_request(N, fun () -> receive after 200 -> 200 end end, req3, ReqIdC2),
    3 = erpc:reqids_size(ReqIdC3),
    
    {1, req2, ReqIdC4} = erpc:receive_response(ReqIdC3, infinity, true),
    2 = erpc:reqids_size(ReqIdC4),

    {200, req3, ReqIdC5} = erpc:receive_response(ReqIdC4, 7654, true),
    1 = erpc:reqids_size(ReqIdC5),

    {400, req1, ReqIdC6} = erpc:receive_response(ReqIdC5, 5000, true),
    0 = erpc:reqids_size(ReqIdC6),

    no_request = erpc:receive_response(ReqIdC6, 5000, true),

    0 = erpc:receive_response(ReqId0),

    ok.

send_request_receive_reqid_collection_timeout(N) ->

    ReqId0 = erpc:send_request(N, fun () -> 0 end),

    ReqIdC0 = erpc:reqids_new(),

    ReqId1 = erpc:send_request(N, fun () -> receive after 1000 -> 1000 end end),
    ReqIdC1 = erpc:reqids_add(ReqId1, req1, ReqIdC0),

    ReqIdC2 = erpc:send_request(N, fun () -> receive after 1 -> 1 end end, req2, ReqIdC1),

    ReqId3 = erpc:send_request(N, fun () -> receive after 500 -> 500 end end),
    ReqIdC3 = erpc:reqids_add(ReqId3, req3, ReqIdC2),

    Deadline = erlang:monotonic_time(millisecond) + 100,
    
    {1, req2, ReqIdC4} = erpc:receive_response(ReqIdC3, {abs, Deadline}, true),
    2 = erpc:reqids_size(ReqIdC4),

    try not_valid = erpc:receive_response(ReqIdC4, {abs, Deadline}, true)
    catch error:{erpc, timeout} -> ok
    end,

    Abandoned = lists:sort([{ReqId1, req1}, {ReqId3, req3}]),
    Abandoned = lists:sort(erpc:reqids_to_list(ReqIdC4)),

    %% Make sure requests were abandoned...
    try not_valid = erpc:receive_response(ReqIdC4, {abs, Deadline+1000}, false)
    catch error:{erpc, timeout} -> ok
    end,

    0 = erpc:receive_response(ReqId0, infinity),

    ok.
    
send_request_receive_reqid_collection_error(N) ->

    ReqId0 = erpc:send_request(N, fun () -> 0 end),

    ReqIdC0 = erpc:reqids_new(),

    ReqId1 = erpc:send_request(N, fun () -> receive after 600 -> 600 end end),
    ReqIdC1 = erpc:reqids_add(ReqId1, req1, ReqIdC0),
    try
        nope = erpc:reqids_add(ReqId1, req2, ReqIdC1)
    catch
        error:{erpc, badarg} -> ok
    end,

    ReqIdC2 = erpc:send_request(N, fun () -> receive after 800 -> erlang:halt() end end, req2, ReqIdC1),
    ReqIdC3 = erpc:send_request(N, fun () -> receive after 200 -> error(errored) end end, req3, ReqIdC2),
    ReqIdC4 = erpc:send_request(N, fun () -> exit(exited) end, req4, ReqIdC3),
    ReqIdC5 = erpc:send_request(N, fun () -> receive after 400 -> throw(thrown) end end, req5, ReqIdC4),

    5 = erpc:reqids_size(ReqIdC5),

    ReqIdC6 = try not_valid = erpc:receive_response(ReqIdC5, infinity, true)
              catch exit:{{exception, exited}, req4, RIC6} -> RIC6
              end,

    4 = erpc:reqids_size(ReqIdC6),

    try not_valid = erpc:receive_response(ReqIdC6, 2000, false)
    catch error:{{exception, errored, _Stk}, req3, _} -> ok
    end,

    try not_valid = erpc:receive_response(ReqIdC6, infinity, false)
    catch throw:{thrown, req5, _} -> ok
    end,
    
    {600, req1, ReqIdC6} = erpc:receive_response(ReqIdC6, infinity, false),

    try not_valid = erpc:receive_response(ReqIdC6, 5000, false)
    catch error:{{erpc, noconnection}, req2, ReqIdC6} -> ok
    end,

    0 = erpc:receive_response(ReqId0),

    ok.

send_request_wait_reqid_collection(Config) when is_list(Config) ->
    {ok, _P, N} = ?CT_PEER(#{connection => 0}),
    send_request_wait_reqid_collection_success(N),
    send_request_wait_reqid_collection_timeout(N),
    send_request_wait_reqid_collection_error(N),
    ok.

send_request_wait_reqid_collection_success(N) ->

    ReqId0 = erpc:send_request(N, fun () -> 0 end),

    ReqIdC0 = erpc:reqids_new(),

    ReqId1 = erpc:send_request(N, fun () -> receive after 400 -> 400 end end),
    ReqIdC1 = erpc:reqids_add(ReqId1, req1, ReqIdC0),
    1 = erpc:reqids_size(ReqIdC1),

    ReqIdC2 = erpc:send_request(N, fun () -> receive after 1 -> 1 end end, req2, ReqIdC1),
    2 = erpc:reqids_size(ReqIdC2),

    ReqIdC3 = erpc:send_request(N, fun () -> receive after 200 -> 200 end end, req3, ReqIdC2),
    3 = erpc:reqids_size(ReqIdC3),
    
    {{response, 1}, req2, ReqIdC4} = erpc:wait_response(ReqIdC3, infinity, true),
    2 = erpc:reqids_size(ReqIdC4),

    {{response, 200}, req3, ReqIdC5} = erpc:wait_response(ReqIdC4, 7654, true),
    1 = erpc:reqids_size(ReqIdC5),

    {{response, 400}, req1, ReqIdC6} = erpc:wait_response(ReqIdC5, 5000, true),
    0 = erpc:reqids_size(ReqIdC6),

    no_request = erpc:wait_response(ReqIdC6, 5000, true),

    {response, 0} = erpc:wait_response(ReqId0),

    ok.

send_request_wait_reqid_collection_timeout(N) ->

    ReqId0 = erpc:send_request(N, fun () -> 0 end),

    ReqIdC0 = erpc:reqids_new(),

    ReqIdC0 = erpc:reqids_new(),

    ReqId1 = erpc:send_request(N, fun () -> receive after 1000 -> 1000 end end),
    ReqIdC1 = erpc:reqids_add(ReqId1, req1, ReqIdC0),

    ReqIdC2 = erpc:send_request(N, fun () -> receive after 1 -> 1 end end, req2, ReqIdC1),

    ReqId3 = erpc:send_request(N, fun () -> receive after 500 -> 500 end end),
    ReqIdC3 = erpc:reqids_add(ReqId3, req3, ReqIdC2),

    Deadline = erlang:monotonic_time(millisecond) + 100,
    
    {{response, 1}, req2, ReqIdC4} = erpc:wait_response(ReqIdC3, {abs, Deadline}, true),
    2 = erpc:reqids_size(ReqIdC4),

    no_response = erpc:wait_response(ReqIdC4, {abs, Deadline}, true),

    Unhandled = lists:sort([{ReqId1, req1}, {ReqId3, req3}]),
    Unhandled = lists:sort(erpc:reqids_to_list(ReqIdC4)),

    %% Make sure requests were not abandoned...
    {{response, 500}, req3, ReqIdC4} = erpc:wait_response(ReqIdC4, {abs, Deadline+1500}, false),
    {{response, 1000}, req1, ReqIdC4} = erpc:wait_response(ReqIdC4, {abs, Deadline+1500}, false),

    {response, 0} = erpc:wait_response(ReqId0, infinity),

    ok.
    
send_request_wait_reqid_collection_error(N) ->

    ReqId0 = erpc:send_request(N, fun () -> 0 end),

    ReqIdC0 = erpc:reqids_new(),

    ReqId1 = erpc:send_request(N, fun () -> receive after 600 -> 600 end end),
    ReqIdC1 = erpc:reqids_add(ReqId1, req1, ReqIdC0),
    try
        nope = erpc:reqids_add(ReqId1, req2, ReqIdC1)
    catch
        error:{erpc, badarg} -> ok
    end,

    ReqIdC2 = erpc:send_request(N, fun () -> receive after 800 -> erlang:halt() end end, req2, ReqIdC1),
    ReqIdC3 = erpc:send_request(N, fun () -> receive after 200 -> error(errored) end end, req3, ReqIdC2),
    ReqIdC4 = erpc:send_request(N, fun () -> exit(exited) end, req4, ReqIdC3),
    ReqIdC5 = erpc:send_request(N, fun () -> receive after 400 -> throw(thrown) end end, req5, ReqIdC4),

    5 = erpc:reqids_size(ReqIdC5),

    ReqIdC6 = try not_valid = erpc:wait_response(ReqIdC5, infinity, true)
              catch exit:{{exception, exited}, req4, RIC6} -> RIC6
              end,

    4 = erpc:reqids_size(ReqIdC6),

    try not_valid = erpc:wait_response(ReqIdC6, 2000, false)
    catch error:{{exception, errored, _Stk}, req3, _} -> ok
    end,

    try not_valid = erpc:wait_response(ReqIdC6, infinity, false)
    catch throw:{thrown, req5, _} -> ok
    end,
    
    {{response, 600}, req1, ReqIdC6} = erpc:wait_response(ReqIdC6, infinity, false),

    try not_valid = erpc:wait_response(ReqIdC6, 5000, false)
    catch error:{{erpc, noconnection}, req2, ReqIdC6} -> ok
    end,

    {response, 0} = erpc:wait_response(ReqId0),

    ok.

send_request_check_reqid_collection(Config) when is_list(Config) ->
    {ok, _P, N} = ?CT_PEER(#{connection => 0}),
    send_request_check_reqid_collection_success(N),
    send_request_check_reqid_collection_error(N),
    ok.

send_request_check_reqid_collection_success(N) ->

    ReqId0 = erpc:send_request(N, fun () -> 0 end),

    ReqIdC0 = erpc:reqids_new(),

    ReqIdC1 = erpc:send_request(N, fun () -> receive after 600 -> 600 end end, req1, ReqIdC0),
    1 = erpc:reqids_size(ReqIdC1),

    ReqId2 = erpc:send_request(N, fun () -> receive after 200 -> 200 end end),
    ReqIdC2 = erpc:reqids_add(ReqId2, req2, ReqIdC1),
    2 = erpc:reqids_size(ReqIdC2),

    ReqIdC3 = erpc:send_request(N, fun () -> receive after 400 -> 400 end end, req3, ReqIdC2),
    3 = erpc:reqids_size(ReqIdC3),

    Msg0 = next_msg(),

    no_response = erpc:check_response(Msg0, ReqIdC3, true),
    
    {{response, 200}, req2, ReqIdC4} = erpc:check_response(next_msg(), ReqIdC3, true),
    2 = erpc:reqids_size(ReqIdC4),

    {{response, 400}, req3, ReqIdC5} = erpc:check_response(next_msg(), ReqIdC4, true),
    1 = erpc:reqids_size(ReqIdC5),

    {{response, 600}, req1, ReqIdC6} = erpc:check_response(next_msg(), ReqIdC5, true),
    0 = erpc:reqids_size(ReqIdC6),

    no_request = erpc:check_response(Msg0, ReqIdC6, true),

    {response, 0} = erpc:check_response(Msg0, ReqId0),

    ok.
    
send_request_check_reqid_collection_error(N) ->

    ReqId0 = erpc:send_request(N, fun () -> 0 end),

    ReqIdC0 = erpc:reqids_new(),

    ReqId1 = erpc:send_request(N, fun () -> receive after 600 -> 600 end end),
    ReqIdC1 = erpc:reqids_add(ReqId1, req1, ReqIdC0),

    ReqIdC2 = erpc:send_request(N, fun () -> receive after 800 -> erlang:halt() end end, req2, ReqIdC1),

    ReqIdC3 = erpc:send_request(N, fun () -> receive after 200 -> error(errored) end end, req3, ReqIdC2),

    ReqIdC4 = erpc:send_request(N, fun () -> exit(exited) end, req4, ReqIdC3),

    ReqIdC5 = erpc:send_request(N, fun () -> receive after 400 -> throw(thrown) end end, req5, ReqIdC4),

    5 = erpc:reqids_size(ReqIdC5),

    Msg0 = next_msg(),

    no_response = erpc:check_response(Msg0, ReqIdC5, true),

    ReqIdC6 = try not_valid = erpc:check_response(next_msg(), ReqIdC5, true)
              catch exit:{{exception, exited}, req4, RIC6} -> RIC6
              end,
    
    try not_valid = erpc:check_response(next_msg(), ReqIdC6, false)
    catch error:{{exception, errored, _Stk}, req3, ReqIdC6} -> ok
    end,

    try not_valid = erpc:check_response(next_msg(), ReqIdC6, false)
    catch throw:{thrown, req5, ReqIdC6} -> ok
    end,
    
    {{response, 600}, req1, ReqIdC6} = erpc:check_response(next_msg(), ReqIdC6, false),

    try not_valid = erpc:check_response(next_msg(), ReqIdC6, false)
    catch error:{{erpc, noconnection}, req2, ReqIdC6} -> ok
    end,

    {response, 0} = erpc:check_response(Msg0, ReqId0),

    ok.

multicall(Config) ->
    {ok, _Peer1, Node1} = ?CT_PEER(#{connection => 0}),
    {ok, Peer2, Node2} = ?CT_PEER(#{connection => 0}),
    {ok, Node3} = start_ei_node(Config),
    %% Test once when erpc:multicall() brings up the connection...
    disconnect(Node3),
    Node3Res = {error, {erpc, notsup}},
    {ok, _Peer4, Node4} = ?CT_PEER(#{connection => 0}),
    {ok, _Peer5, Node5} = ?CT_PEER(#{connection => 0}),
    {OldRelName, OldRel} = old_release(),
    {OldTested, {ok, _Peer6, Node6}}
        = case ?CT_PEER_REL(#{connection => 0},
                            OldRelName,
                            proplists:get_value(priv_dir, Config)) of
              not_available ->
                  {false, ?CT_PEER(#{connection => 0})};
              {ok, _OldPeer, OldNode} = OldNodeRes ->
                  compile_and_load_on_node(OldNode),
                  {true, OldNodeRes}
          end,
    peer:stop(Peer2),
    io:format("Node1=~p~nNode2=~p~nNode3=~p~nNode4=~p~nNode5=~p~nNode6=~p~n",
              [Node1, Node2, Node3, Node4, Node5, Node6]),

    ThisNode = node(),
    Nodes = [ThisNode, Node1, Node2, Node3, Node4, Node5],
    
    [{ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5},
     {error, {erpc, noconnection}},
     {ok, Node6}]
        = erpc:multicall(Nodes ++ [badnodename] ++ [Node6], erlang, node, []),

    [{ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5},
     {error, {erpc, noconnection}}]
        = erpc:multicall(Nodes ++ [badnodename], fun () -> erlang:node() end),

    try
        erpc:multicall(Nodes ++ [<<"badnodename">>], erlang, node, []),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
            ok
    end,
    
    try
        erpc:multicall([Node1, Node2, Node3, Node4, Node5 | node()], erlang, node, []),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
            ok
    end,
    
    
    try
        erpc:multicall(Nodes ++ [<<"badnodename">>], fun () -> erlang:node() end),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
            ok
    end,

    try
        erpc:multicall(Nodes, fun (X) -> X end),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
            ok
    end,

    [{ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5},
     {ok, Node6}]
        = erpc:multicall(Nodes ++ [Node6], erlang, node, []),

    [{ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5},
     {ok, Node6}]
        = erpc:multicall(Nodes ++ [Node6], erlang, node, [], 60000),

    [{throw, ThisNode},
     {throw, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {throw, Node4},
     {throw, Node5}]
        = erpc:multicall(Nodes, fun () -> throw(erlang:node()) end),

    [{throw, ThisNode},
     {throw, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {throw, Node4},
     {throw, Node5}]
        = erpc:multicall(Nodes,  fun () -> throw(erlang:node()) end, 60000),

    S0 = erlang:monotonic_time(millisecond),
    [{error, {erpc, timeout}},
     {error, {erpc, timeout}},
     {error, {erpc, noconnection}},
     Node3Res,
     {error, {erpc, timeout}},
     {error, {erpc, timeout}},
     {error, {erpc, timeout}},

     {error, {erpc, timeout}},
     {error, {erpc, timeout}},
     {error, {erpc, noconnection}},
     Node3Res,
     {error, {erpc, timeout}},
     {error, {erpc, timeout}},
     {error, {erpc, timeout}}]
        = erpc:multicall(Nodes++[Node6]++Nodes++[Node6], timer, sleep, [2000], 500),
    E0 = erlang:monotonic_time(millisecond),
    T0 = E0 - S0,
    io:format("T0=~p~n", [T0]),
    true = T0 < 1000,

    S1 = erlang:monotonic_time(millisecond),
    [{ok, ok},
     {ok, ok},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, ok},
     {ok, ok},
     {ok, ok},

     {ok, ok},
     {ok, ok},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, ok},
     {ok, ok},
     {ok, ok}]
        = erpc:multicall(Nodes++[Node6]++Nodes++[Node6], timer, sleep, [2000]),
    E1 = erlang:monotonic_time(millisecond),
    T1 = E1 - S1,
    io:format("T1=~p~n", [T1]),
    true = T1 < 3000,

    S2 = erlang:monotonic_time(millisecond),
    [{ok, ok},
     {ok, ok},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, ok},
     {ok, ok},
     {ok, ok},

     {ok, ok},
     {ok, ok},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, ok},
     {ok, ok},
     {ok, ok}]
        = erpc:multicall(Nodes++[Node6]++Nodes++[Node6], timer, sleep, [2000], 3000),
    E2 = erlang:monotonic_time(millisecond),
    T2 = E2 - S2,
    io:format("T2=~p~n", [T2]),
    true = T2 < 3000,

    [{ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5},
     {ok, Node6},

     {ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5},
     {ok, Node6}]
        = erpc:multicall(Nodes++[Node6]++Nodes++[Node6], erlang, node, []),

    [{ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5},
     {ok, Node6},

     {ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5},
     {ok, Node6}]
        = erpc:multicall(Nodes++[Node6]++Nodes++[Node6], erlang, node, [], 60000),

    [{ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5}]
        = erpc:multicall(Nodes, fun () -> erlang:node() end),
    
    [BlingError,
     BlingError,
     {error, {erpc, noconnection}},
     Node3Res,
     BlingError,
     BlingError,
     OldBlingError]
        = erpc:multicall(Nodes++[Node6], ?MODULE, call_func2, [bling]),

    [BlingError,
     BlingError,
     {error, {erpc, noconnection}},
     Node3Res,
     BlingError,
     BlingError,
     OldBlingError]
        = erpc:multicall(Nodes++[Node6], ?MODULE, call_func2, [bling], 60000),

    {error, {exception,
             bling,
             [{?MODULE, call_func3, A, _},
              {?MODULE, call_func2, 1, _}]}} = BlingError,
    true = (A == 1) orelse (A == [bling]),

    {error, {exception,
             bling,
             [{?MODULE, call_func3, OldA, _},
              {?MODULE, call_func2, 1, _}]}} = OldBlingError,
    true = (OldA == 1) orelse (OldA == [bling]),

    [{error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {erpc, noconnection}},
     Node3Res,
     {error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {exception, blong, [{erlang, error, [blong], _}]}}]
        = erpc:multicall(Nodes++[Node6], erlang, error, [blong]),

    [{error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {erpc, noconnection}},
     Node3Res,
     {error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {exception, blong, [{erlang, error, [blong], _}]}}]
        = erpc:multicall(Nodes++[Node6], erlang, error, [blong], 60000),

    SlowNode4 = fun () ->
                        case node() of
                            Node4 ->
                                receive after 1000 -> ok end,
                                slow;
                            ThisNode ->
                                throw(fast);
                            _ ->
                                fast
                           end
                end,

    Start1 = erlang:monotonic_time(),
    [{throw, fast},
     {ok, fast},
     {error, {erpc, noconnection}},
     Node3Res,
     {error, {erpc, timeout}},
     {ok, fast}]
        = erpc:multicall(Nodes, erlang, apply, [SlowNode4, []], 500),

    End1 = erlang:monotonic_time(),

    Time1 = erlang:convert_time_unit(End1-Start1, native, millisecond),
    io:format("Time1 = ~p~n",[Time1]),
    true = Time1 >= 500,
    true = Time1 =< 1000,

    SlowThisNode = fun () ->
                           case node() of
                               ThisNode ->
                                   receive after 1000 -> ok end,
                                   slow;
                               Node4 ->
                                   throw(fast);
                               Node5 ->
                                   exit(fast);
                               _ ->
                                   fast
                           end
                   end,

    Start2 = erlang:monotonic_time(),
    [{error, {erpc, timeout}},
     {ok, fast},
     {error, {erpc, noconnection}},
     Node3Res,
     {throw, fast},
     {exit, {exception, fast}}] = erpc:multicall(Nodes, SlowThisNode, 500),

    End2 = erlang:monotonic_time(),

    Time2 = erlang:convert_time_unit(End2-Start2, native, millisecond),
    io:format("Time2 = ~p~n",[Time2]),
    true = Time2 >= 500,
    true = Time2 =< 1000,

    %% We should not get any stray messages due to timed out operations...
    receive Msg -> ct:fail({unexpected, Msg})
    after 1000 -> ok
    end,

    [{error, {erpc, noconnection}},
     Node3Res,
     {error, {erpc, noconnection}},
     {error, {erpc, noconnection}},
     {error, {erpc, noconnection}}]
        = erpc:multicall([Node2, Node3, Node4, Node5, Node6], erlang, halt, []),

    [] = flush([]),

    ok = stop_ei_node(Node3),
    case OldTested of
        false ->
            {comment, "Not tested against OTP "++OldRel++" node"};
        true ->
            {comment, "Also tested against OTP "++OldRel++" node"}
    end.

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
                  [{ok, done},{error,{erpc,timeout}},{ok, done}]
                      = erpc:multicall([QuickNode1, Node, QuickNode2],
                                        erlang, apply, [SlowSend, []],
                                        Timeout)
          end,
    Res = reqtmo_test(Fun),
    peer:stop(Peer1),
    peer:stop(Peer2),
    Res.

multicall_recv_opt(Config) when is_list(Config) ->
    Loops = 1000,
    HugeMsgQ = 500000,
    process_flag(message_queue_data, off_heap),
    {ok, Peer1, Node1} = ?CT_PEER(),
    {ok, Peer2, Node2} = ?CT_PEER(),
    ExpRes = [{ok, node()}, {ok, Node1}, {ok, Node2}],
    Nodes = [node(), Node1, Node2],
    Fun = fun () -> erlang:node() end,
    _Warmup = time_multicall(ExpRes, Nodes, Fun, infinity, Loops div 10),
    Empty = time_multicall(ExpRes, Nodes, Fun, infinity, Loops),
    io:format("Time with empty message queue: ~p microsecond~n",
	      [erlang:convert_time_unit(Empty, native, microsecond)]),
    _ = [self() ! {msg,N} || N <- lists:seq(1, HugeMsgQ)],
    Huge = time_multicall(ExpRes, Nodes, Fun, infinity, Loops),
    io:format("Time with huge message queue: ~p microsecond~n",
	      [erlang:convert_time_unit(Huge, native, microsecond)]),
    peer:stop(Peer1),
    peer:stop(Peer2),
    Q = Huge / Empty,
    HugeMsgQ = flush_msgq(),
    case Q > 10 of
	true ->
	    ct:fail({ratio, Q});
	false ->
	    {comment, "Ratio: "++erlang:float_to_list(Q)}
    end.

multicall_recv_opt2(Config) when is_list(Config) ->
    Loops = 1000,
    HugeMsgQ = 500000,
    process_flag(message_queue_data, off_heap),
    {ok, Peer1, Node1} = ?CT_PEER(),
    peer:stop(Peer1),
    {ok, Peer2, Node2} = ?CT_PEER(),
    ExpRes = [{ok, node()}, {error, {erpc, noconnection}}, {ok, Node2}],
    Nodes = [node(), Node1, Node2],
    Fun = fun () -> erlang:node() end,
    _Warmup = time_multicall(ExpRes, Nodes, Fun, infinity, Loops div 10),
    Empty = time_multicall(ExpRes, Nodes, Fun, infinity, Loops),
    io:format("Time with empty message queue: ~p microsecond~n",
	      [erlang:convert_time_unit(Empty, native, microsecond)]),
    _ = [self() ! {msg,N} || N <- lists:seq(1, HugeMsgQ)],
    Huge = time_multicall(ExpRes, Nodes, Fun, infinity, Loops),
    io:format("Time with huge message queue: ~p microsecond~n",
	      [erlang:convert_time_unit(Huge, native, microsecond)]),
    peer:stop(Peer2),
    Q = Huge / Empty,
    HugeMsgQ = flush_msgq(),
    case Q > 10 of
	true ->
	    ct:fail({ratio, Q});
	false ->
	    {comment, "Ratio: "++erlang:float_to_list(Q)}
    end.

multicall_recv_opt3(Config) when is_list(Config) ->
    Loops = 1000,
    HugeMsgQ = 500000,
    process_flag(message_queue_data, off_heap),
    {ok, Peer1, Node1} = ?CT_PEER(),
    peer:stop(Peer1),
    {ok, Peer2, Node2} = ?CT_PEER(),
    Nodes = [node(), Node1, Node2],
    Fun = fun () -> erlang:node() end,
    _Warmup = time_multicall(undefined, Nodes, Fun, infinity, Loops div 10),
    Empty = time_multicall(undefined, Nodes, Fun, infinity, Loops),
    io:format("Time with empty message queue: ~p microsecond~n",
	      [erlang:convert_time_unit(Empty, native, microsecond)]),
    _ = [self() ! {msg,N} || N <- lists:seq(1, HugeMsgQ)],
    Huge = time_multicall(undefined, Nodes, Fun, 0, Loops),
    io:format("Time with huge message queue: ~p microsecond~n",
	      [erlang:convert_time_unit(Huge, native, microsecond)]),
    peer:stop(Peer2),
    Q = Huge / Empty,
    HugeMsgQ = flush_msgq(),
    case Q > 10 of
	true ->
	    ct:fail({ratio, Q});
	false ->
	    {comment, "Ratio: "++erlang:float_to_list(Q)}
    end.

time_multicall(Expect, Nodes, Fun, Tmo, Times) ->
    Start = erlang:monotonic_time(),
    ok = do_time_multicall(Expect, Nodes, Fun, Tmo, Times),
    erlang:monotonic_time() - Start.

do_time_multicall(_Expect, _Nodes, _Fun, _Tmo, 0) ->
    ok;
do_time_multicall(undefined, Nodes, Fun, Tmo, N) ->
    _ = erpc:multicall(Nodes, Fun, Tmo),
    do_time_multicall(undefined, Nodes, Fun, Tmo, N-1);
do_time_multicall(Expect, Nodes, Fun, Tmo, N) ->
    Expect = erpc:multicall(Nodes, Fun, Tmo),
    do_time_multicall(Expect, Nodes, Fun, Tmo, N-1).

multicast(Config) when is_list(Config) ->
    {ok, _Peer, Node} = ?CT_PEER(),
    {OldRelName, OldRel} = old_release(),
    {OldTested, {ok, _OldPeer, OldNode}}
        = case ?CT_PEER_REL(#{connection => 0},
                            OldRelName,
                            proplists:get_value(priv_dir, Config)) of
              not_available ->
                  {false, ?CT_PEER()};
              {ok, _, _} = OldNodeRes ->
                  {true, OldNodeRes}
          end,
    Nodes = [node(), Node],
    try
        erpc:multicast(Nodes++[OldNode], erlang, send, hej),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} -> ok
    end,
    try
        erpc:multicast(node(), erlang, send, [hej]),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} -> ok
    end,
    try
        erpc:multicast([<<"node">>, Node], erlang, send, [hej]),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} -> ok
    end,
    try
        erpc:multicast(Nodes, "erlang", send, [hej]),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} -> ok
    end,
    try
        erpc:multicast(Nodes, erlang, make_ref(), [hej]),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} -> ok
    end,

    %% Silently fail...
    erpc:multicast([badnodename], erlang, send, [self(), blupp]),
    %% silent remote error...
    erpc:multicast(Nodes, erlang, send, [self()|blupp]),

    Me = self(),
    Ok1 = make_ref(),
    ok = erpc:multicast(Nodes++[OldNode], erlang, send, [Me, {mfa, Ok1}]),
    receive
        {mfa, Ok1} -> ok
    end,
    receive
        {mfa, Ok1} -> ok
    end,
    receive
        {mfa, Ok1} -> ok
    end,
    ok = erpc:multicast(Nodes, fun () -> Me ! {a_fun, Ok1} end),
    receive
        {a_fun, Ok1} -> ok
    end,
    receive
        {a_fun, Ok1} -> ok
    end,

    ok = erpc:multicast([Node, OldNode], erlang, halt, []),

    monitor_node(Node, true),
    receive {nodedown, Node} -> ok end,
    monitor_node(OldNode, true),
    receive {nodedown, OldNode} -> ok end,

    ok = erpc:multicast([Node, OldNode], erlang, send, [Me, wont_reach_me]),

    receive after 1000 -> ok end,

    [] = flush([]),

    {ok, EiNode} = start_ei_node(Config),
    %% Test once when erpc:multicast() brings up the connection
    %% and once when the connection is up...
    disconnect(EiNode),

    ok = erpc:multicast([EiNode], erlang, send, [Me, wont_reach_me]),
    ok = erpc:multicast([EiNode], fun () -> Me ! wont_reach_me end),

    wait_until(fun () -> lists:member(EiNode, nodes(hidden)) end),

    ok = erpc:multicast([EiNode], erlang, send, [Me, wont_reach_me]),
    ok = erpc:multicast([EiNode], fun () -> Me ! wont_reach_me end),

    receive
        Msg -> ct:fail({unexpected_message, Msg})
    after
        2000 -> ok
    end,
    ok = stop_ei_node(EiNode),

    case OldTested of
        false ->
            {comment, "Not tested against OTP "++OldRel++" node"};
        true ->
            {comment, "Also tested against OTP "++OldRel++" node"}
    end.


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
        ct:fail("The ?MAX_INT_TIMEOUT define in erpc.erl needs "
                "to be updated to reflect max timeout value "
                "in a receive/after...")
    catch
        error:timeout_value ->
            ok
    end,
    Node = erpc:call(Node, erlang, node, [], MaxTmo),
    try
        erpc:call(node(), erlang, node, [], MaxTmo+1),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
            ok
    end,
    [{ok,Node}] = erpc:multicall([Node], erlang, node, [], MaxTmo),
    try
        erpc:multicall([Node], erlang, node, [], MaxTmo+1),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
            ok
    end,
    ReqId1 = erpc:send_request(Node, erlang, node, []),
    Node = erpc:receive_response(ReqId1, MaxTmo),
    ReqId2 = erpc:send_request(Node, erlang, node, []),
    try
        erpc:receive_response(ReqId2, MaxTmo+1),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
            ok
    end,
    ReqId3 = erpc:send_request(Node, erlang, node, []),
    Node = erpc:receive_response(ReqId3, MaxTmo),
    ReqId4 = erpc:send_request(Node, erlang, node, []),
    try
        erpc:receive_response(ReqId4, MaxTmo+1),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
            ok
    end,
    ok.
    

%%%
%%% Utility functions.
%%%

wait_until(Fun) ->
    case (catch Fun()) of
        true ->
            ok;
        _ ->
            receive after 100 -> ok end,
            wait_until(Fun)
    end.

old_release() ->
    OldRel = integer_to_list(list_to_integer(erlang:system_info(otp_release))-2),
    {OldRel++"_latest", OldRel}.

compile_and_load_on_node(Node) ->
    %% Recompile erpc_SUITE on the old node and load it...
    SrcFile = filename:rootname(code:which(?MODULE)) ++ ".erl",
    {ok, ?MODULE, BeamCode} = erpc:call(Node, compile, file,
                                        [SrcFile, [binary]]),
    {module, ?MODULE} = erpc:call(Node, code, load_binary,
                                  [?MODULE, SrcFile, BeamCode]).

start_ei_node(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Suffix = case os:type() of
                 {win32, _} -> ".exe";
                 _ -> ""
             end,
    FwdNodeExe = filename:join(DataDir, "fwd_node"++Suffix),
    Name = atom_to_list(?MODULE)
        ++ "-" ++ "ei_node"
        ++ "-" ++ integer_to_list(erlang:system_time(second))
        ++ "-" ++ integer_to_list(erlang:unique_integer([positive])),
    Cookie = atom_to_list(erlang:get_cookie()),
    HostName = get_hostname(),
    Node = list_to_atom(Name++"@"++HostName),
    Creation = integer_to_list(rand:uniform((1 bsl 15) - 4) + 3),
    Parent = self(),
    Pid = spawn_link(fun () ->
                             register(cnode_forward_receiver, self()),
                             process_flag(trap_exit, true),
                             Args = ["-sname", Name,
                                     "-cookie", Cookie,
                                     "-creation", Creation],
                             io:format("Starting ei_node: ~p ~p~n",
                                       [FwdNodeExe, Args]),
                             Port = erlang:open_port({spawn_executable, FwdNodeExe},
                                                     [use_stdio, {args, Args}]),
                             receive
                                 {Port, {data, "accepting"}} -> ok
                             end,
                             ei_node_handler_loop(Node, Parent, Port)
                     end),
    put({ei_node_handler, Node}, Pid),
    case check_ei_node(Node) of
        ok -> {ok, Node};
        Error -> Error
    end.

check_ei_node(Node) ->
    Key = {ei_node_handler, Node},
    case get(Key) of
        undefined ->
            {error, no_handler};
        Pid when is_pid(Pid) ->
            Pid ! {check_node, self()},
            receive
                {check_node, Pid, Res} ->
                    Res
            after 3000 ->
                    {error, no_handler_response}
            end
    end.

stop_ei_node(Node) ->
    case check_ei_node(Node) of
        ok ->
            Key = {ei_node_handler, Node},
            case get(Key) of
                undefined ->
                    {error, no_handler};
                Pid when is_pid(Pid) ->
                    Pid ! {stop_node, self()},
                    receive
                        {stop_node, Pid} ->
                            put(Key, undefined),
                            ok
                    after 2000 ->
                            {error, no_handler_response}
                    end
            end;
        Error ->
            Error
    end.

ei_node_handler_loop(Node, Parent, Port) ->
    receive
        {'EXIT', Parent, Reason} ->
            erlang:disconnect_node(Node),
            (catch port_close(Port)),
            exit(Reason);
        {stop_node, Parent} ->
            erlang:disconnect_node(Node),
            (catch port_close(Port)),
            Parent ! {stop_node, self()},
            exit(normal);
        {check_node, Parent} ->
            Ref = make_ref(),
            {a_name, Node} ! Ref,
            receive
                Ref ->
                    Parent ! {check_node, self(), ok}
            after
                2000 ->
                    Parent ! {check_node, self(), {error, no_node_response}}
            end;
        Msg ->
            Msgs = fetch_all_messages([Msg]),
            erlang:disconnect_node(Node),
            (catch port_close(Port)),
            exit({ei_node_handler, Node, unexpected_messages, Msgs})
    end,
    ei_node_handler_loop(Node, Parent, Port).

get_hostname() ->
    get_hostname(atom_to_list(node())).

get_hostname([$@ | HostName]) ->
    HostName;
get_hostname([_ | Rest]) ->
    get_hostname(Rest).

flush(L) ->
    receive
	M ->
	    flush([M|L])
    after 0 ->
	    L
    end.

disconnect(Node) ->
    case lists:member(Node, nodes([visible, hidden])) of
        false ->
            ok;
        true ->
            monitor_node(Node, true),
            true = erlang:disconnect_node(Node),
            receive {nodedown, Node} -> ok end,
            false = lists:member(Node, nodes([visible, hidden])),                
            ok
    end.

t() ->
    Delay = case init:get_argument(erpc_test_delay) of
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

next_msg() ->
    receive M -> M end.

flush_msgq() ->
    flush_msgq(0).
flush_msgq(N) ->
    receive
	_ ->
	    flush_msgq(N+1)
    after 0 ->
	    N
    end.

fetch_all_messages(Msgs) ->
    receive
        Msg ->
            fetch_all_messages([Msg|Msgs])
    after
        0 ->
            Msgs
    end.
