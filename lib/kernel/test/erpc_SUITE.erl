%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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
-export([call/1, call_reqtmo/1, call_against_old_node/1, cast/1,
         send_request/1, send_request_fun/1,
         send_request_receive_reqtmo/1,
         send_request_wait_reqtmo/1,
         send_request_check_reqtmo/1,
         send_request_against_old_node/1,
         multicall/1, multicall_reqtmo/1,
         multicast/1,
         timeout_limit/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([call_func1/1, call_func2/1, call_func4/4]).

-export([f/0, f2/0]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [call,
     call_reqtmo,
     call_against_old_node,
     cast,
     send_request,
     send_request_fun,
     send_request_receive_reqtmo,
     send_request_wait_reqtmo,
     send_request_check_reqtmo,
     send_request_against_old_node,
     multicall,
     multicall_reqtmo,
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
    call_test(Config).

call_test(Config) ->
    call_test(node(), 10000),
    call_test(node(), infinity),
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
    end,
    {ok, Node} = start_node(Config),
    call_test(Node, 10000),
    call_test(Node, infinity),
    try
        erpc:call(Node, timer, sleep, [100], 10),
        ct:fail(unexpected)
    catch
        error:{erpc, timeout} ->
            ok
    end,
    try
        erpc:call(Node, fun () -> timer:sleep(100) end, 10),
        ct:fail(unexpected)
    catch
        error:{erpc, timeout} ->
            ok
    end,
    try
        erpc:call(Node, erlang, halt, []),
        ct:fail(unexpected)
    catch
        error:{erpc, noconnection} ->
            ok
    end,
    try
        erpc:call(Node, fun () -> erlang:node() end),
        ct:fail(unexpected)
    catch
        error:{erpc, noconnection} ->
            ok
    end,
    try
        erpc:call(Node, erlang, node, []),
        ct:fail(unexpected)
    catch
        error:{erpc, noconnection} ->
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

call_test(Node, Timeout) ->
    io:format("call_test(~p, ~p)~n", [Node, Timeout]),
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
    try
        erpc:call(Node, fun () -> erlang:exit(oops) end, Timeout),
        ct:fail(unexpected)
    catch
        exit:{exception, oops} ->
            ok
    end,
    try
        erpc:call(Node, erlang, throw, [oops], Timeout),
        ct:fail(unexpected)
    catch
        throw:oops ->
            ok
    end,
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

call_against_old_node(Config) ->
    case start_22_node(Config) of
        {ok, Node22} ->
            try
                erpc:call(Node22, erlang, node, []),
                ct:fail(unexpected)
            catch
                error:{erpc, notsup} ->
                    ok
            end,
            stop_node(Node22),
            ok;
        _ ->
	    {skipped, "No OTP 22 available"}
    end.

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
    reqtmo_test(Config, Fun).

reqtmo_test(Config, Test) ->
    %% Tests that we time out in time also when the request itself
    %% does not get through. A typical issue we have had
    %% in the past, is that the timeout has not triggered until
    %% the request has gotten through...
    
    Timeout = 500,
    WaitBlock = 100,
    BlockTime = 1000,

    {ok, Node} = start_node(Config),

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
    
    stop_node(Node),
    
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
    {ok, Node} = start_node(Config),
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

    case start_22_node(Config) of
        {ok, Node22} ->
            ok = erpc:cast(Node, erlang, send, [Me, wont_reach_me]),
            ok = erpc:cast(Node, fun () -> Me ! wont_reach_me end),
            receive
                Msg -> ct:fail({unexpected_message, Msg})
            after
                2000 -> ok
            end,
            stop_node(Node22),
            {comment, "Tested against OTP 22 as well"};
        _ ->
            {comment, "No tested against OTP 22"}
    end.

send_request(Config) when is_list(Config) ->
    %% Note: First part of nodename sets response delay in seconds.
    PA = filename:dirname(code:which(?MODULE)),
    NodeArgs = [{args,"-pa "++ PA}],
    {ok,Node1} = test_server:start_node('1_erpc_SUITE_call', slave, NodeArgs),
    {ok,Node2} = test_server:start_node('10_erpc_SUITE_call', slave, NodeArgs),
    {ok,Node3} = test_server:start_node('20_erpc_SUITE_call', slave, NodeArgs),
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

    stop_node(Node1),
    stop_node(Node2),
    stop_node(Node3),

    [] = flush([]),

    {ok, Node4} = start_node(Config),

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

    case start_22_node(Config) of
        {ok, Node5} ->
            ReqId9 = erpc:send_request(Node5, erlang, node, []),
            try
                erpc:receive_response(ReqId9),
                ct:fail(unexpected)
            catch
                error:{erpc, notsup} -> ok
            end,

            stop_node(Node5),

            [] = flush([]),

            ok;
        _ ->
            {comment, "No test against OTP 22 node performed"}
    end.

send_request_fun(Config) when is_list(Config) ->
    %% Note: First part of nodename sets response delay in seconds.
    PA = filename:dirname(code:which(?MODULE)),
    NodeArgs = [{args,"-pa "++ PA}],
    {ok,Node1} = test_server:start_node('1_erpc_SUITE_call', slave, NodeArgs),
    {ok,Node2} = test_server:start_node('10_erpc_SUITE_call', slave, NodeArgs),
    {ok,Node3} = test_server:start_node('20_erpc_SUITE_call', slave, NodeArgs),
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

    stop_node(Node1),
    stop_node(Node2),
    stop_node(Node3),

    {ok, Node4} = start_node(Config),

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

    case start_22_node(Config) of
        {ok, Node5} ->
            ReqId9 = erpc:send_request(Node5, fun () -> erlang:node() end),
            try
                erpc:receive_response(ReqId9),
                ct:fail(unexpected)
            catch
                error:{erpc, notsup} -> ok
            end,

            stop_node(Node5),

            [] = flush([]),

            ok;
        _ ->
            {comment, "No test against OTP 22 node performed"}
    end.


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
    reqtmo_test(Config, Fun).

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
    reqtmo_test(Config, Fun).

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
    reqtmo_test(Config, Fun).

send_request_against_old_node(Config) when is_list(Config) ->
    case start_22_node(Config) of
        {ok, Node22} ->
            RID1 = erpc:send_request(Node22, erlang, node, []),
            RID2 = erpc:send_request(Node22, erlang, node, []),
            RID3 = erpc:send_request(Node22, erlang, node, []),
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
            stop_node(Node22),
            ok;
        _ ->
	    {skipped, "No OTP 22 available"}
    end.

multicall(Config) ->
    {ok, Node1} = start_node(Config),
    {ok, Node2} = start_node(Config),
    {Node3, Node3Res} = case start_22_node(Config) of
                            {ok, N3} ->
                                {N3, {error, {erpc, notsup}}};
                            _ ->
                                {ok, N3} = start_node(Config),
                                stop_node(N3),
                                {N3, {error, {erpc, noconnection}}}
                        end,
    {ok, Node4} = start_node(Config),
    {ok, Node5} = start_node(Config),
    stop_node(Node2),
    
    ThisNode = node(),
    Nodes = [ThisNode, Node1, Node2, Node3, Node4, Node5],
    
    [{ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5},
     {error, {erpc, noconnection}}]
        = erpc:multicall(Nodes ++ [badnodename], erlang, node, []),

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
     {ok, Node5}]
        = erpc:multicall(Nodes, erlang, node, []),

    [{ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5}]
        = erpc:multicall(Nodes, erlang, node, [], 60000),

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
     {error, {erpc, noconnection}},
     Node3Res,
     {error, {erpc, timeout}},
     {error, {erpc, timeout}}]
        = erpc:multicall(Nodes++Nodes, timer, sleep, [2000], 500),
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
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, ok},
     {ok, ok}]
        = erpc:multicall(Nodes++Nodes, timer, sleep, [2000]),
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
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, ok},
     {ok, ok}]
        = erpc:multicall(Nodes++Nodes, timer, sleep, [2000], 3000),
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
     {ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5}]
        = erpc:multicall(Nodes++Nodes, erlang, node, []),

    [{ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5},
     {ok, ThisNode},
     {ok, Node1},
     {error, {erpc, noconnection}},
     Node3Res,
     {ok, Node4},
     {ok, Node5}]
        = erpc:multicall(Nodes++Nodes, erlang, node, [], 60000),

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
     BlingError]
        = erpc:multicall(Nodes, ?MODULE, call_func2, [bling]),

    [BlingError,
     BlingError,
     {error, {erpc, noconnection}},
     Node3Res,
     BlingError,
     BlingError]
        = erpc:multicall(Nodes, ?MODULE, call_func2, [bling], 60000),

    {error, {exception,
             bling,
             [{?MODULE, call_func3, A, _},
              {?MODULE, call_func2, 1, _}]}} = BlingError,
    true = (A == 1) orelse (A == [bling]),

    [{error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {erpc, noconnection}},
     Node3Res,
     {error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {exception, blong, [{erlang, error, [blong], _}]}}]
        = erpc:multicall(Nodes, erlang, error, [blong]),

    [{error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {erpc, noconnection}},
     Node3Res,
     {error, {exception, blong, [{erlang, error, [blong], _}]}},
     {error, {exception, blong, [{erlang, error, [blong], _}]}}]
        = erpc:multicall(Nodes, erlang, error, [blong], 60000),

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
     {error, {erpc, noconnection}}]
        = erpc:multicall([Node2, Node3, Node4, Node5], erlang, halt, []),

    [] = flush([]),

    stop_node(Node3),
    case Node3Res of
        {error, {erpc, notsup}} ->
            {comment, "Tested against an OTP 22 node as well"};
        _ ->
            {comment, "No OTP 22 node available; i.e., only testing against current release"}
    end.

multicall_reqtmo(Config) when is_list(Config) ->
    {ok, QuickNode1} = start_node(Config),
    {ok, QuickNode2} = start_node(Config),
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
    Res = reqtmo_test(Config, Fun),
    stop_node(QuickNode1),
    stop_node(QuickNode2),
    Res.


multicast(Config) when is_list(Config) ->
    {ok, Node} = start_node(Config),
    Nodes = [node(), Node],
    try
        erpc:multicast(Nodes, erlang, send, hej),
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
    ok = erpc:multicast(Nodes, erlang, send, [Me, {mfa, Ok1}]),
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

    ok = erpc:multicast([Node], erlang, halt, []),

    monitor_node(Node, true),
    receive {nodedown, Node} -> ok end,

    ok = erpc:multicast([Node], erlang, send, [Me, wont_reach_me]),

    receive after 1000 -> ok end,

    [] = flush([]),
    case start_22_node(Config) of
        {ok, Node22} ->
            ok = erpc:multicast([Node], erlang, send, [Me, wont_reach_me]),
            ok = erpc:multicast([Node], fun () -> Me ! wont_reach_me end),
            receive
                Msg -> ct:fail({unexpected_message, Msg})
            after
                2000 -> ok
            end,
            stop_node(Node22),
            {comment, "Tested against OTP 22 as well"};
        _ ->
            {comment, "No tested against OTP 22"}
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

start_node(Config) ->
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-" ++ atom_to_list(proplists:get_value(testcase, Config))
			++ "-" ++ integer_to_list(erlang:system_time(second))
			++ "-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args,  "-pa " ++ Pa}]).

start_22_node(Config) ->
    Rel = "22_latest",
    case test_server:is_release_available(Rel) of
	false ->
            notsup;
        true ->
            Cookie = atom_to_list(erlang:get_cookie()),
            Name = list_to_atom(atom_to_list(?MODULE)
                                ++ "-" ++ atom_to_list(proplists:get_value(testcase, Config))
                                ++ "-" ++ integer_to_list(erlang:system_time(second))
                                ++ "-" ++ integer_to_list(erlang:unique_integer([positive]))),
            Pa = filename:dirname(code:which(?MODULE)),
	    test_server:start_node(Name,
                                   peer,
                                   [{args, "-pa " ++ Pa ++ " -setcookie "++Cookie},
                                    {erl, [{release, Rel}]}])
    end.

stop_node(Node) ->
    test_server:stop_node(Node).

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
