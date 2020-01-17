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
         send_request/1, send_request_receive_reqtmo/1,
         send_request_wait_reqtmo/1,
         send_request_check_reqtmo/1,
         send_request_against_old_node/1,
         multicall/1, multicall_reqtmo/1,
         timeout_limit/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([call_func1/1, call_func2/1, call_func4/4]).

-export([f/0, f2/0]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [call, call_reqtmo, call_against_old_node, cast,
     send_request, send_request_receive_reqtmo,
     send_request_wait_reqtmo, send_request_check_reqtmo,
     send_request_against_old_node,
     multicall, multicall_reqtmo,
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
        erpc:call(node(), erlang, node, [], 0),
        ct:fail(unexpected)
    catch
        error:{erpc, timeout} ->
            ok
    end,
    {ok, Node} = start_node(Config),
    call_test(Node, 10000),
    call_test(Node, infinity),
    try
        erpc:call(Node, erlang, node, [], 0),
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
        erpc:call(Node, erlang, node, []),
        ct:fail(unexpected)
    catch
        error:{erpc, noconnection} ->
            ok
    end.

call_test(Node, Timeout) ->
    io:format("call_test(~p, ~p)~n", [Node, Timeout]),
    Node = erpc:call(Node, erlang, node, [], Timeout),
    try
        erpc:call(Node, erlang, error, [oops|invalid], Timeout),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
            ok
    end,
    try
        erpc:call(Node, erlang, error, [oops|invalid], Timeout),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} ->
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
        erpc:call(Node, erlang, throw, [oops], Timeout),
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
    try
        erpc:cast(node, erlang, send, [hej]),
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
    try
        erpc:cast(node(), erlang, send, [hej|hopp]),
        ct:fail(unexpected)
    catch
        error:{erpc, badarg} -> ok
    end,
    Me = self(),
    Ok1 = make_ref(),
    ok = erpc:cast(node(), erlang, send, [Me, Ok1]),
    receive
        Ok1 -> ok
    end,
    {ok, Node} = start_node(Config),
    Ok2 = make_ref(),
    ok = erpc:cast(Node, erlang, send, [Me, Ok2]),
    receive
        Ok2 -> ok
    end,
    stop_node(Node),
    case start_22_node(Config) of
        {ok, Node22} ->
            ok = erpc:cast(Node, erlang, send, [Me, wont_reach_me]),
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
        erpc:receive_response(ReqId4, 10)
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
     {ok, Node5}]
        = erpc:multicall(Nodes, erlang, node, []),
    
    [BlingError,
     BlingError,
     {error, {erpc, noconnection}},
     Node3Res,
     BlingError,
     BlingError]
        = erpc:multicall(Nodes, ?MODULE, call_func2, [bling]),

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
     {exit, {exception, fast}}]
        = erpc:multicall(Nodes, erlang, apply, [SlowThisNode, []], 500),

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
