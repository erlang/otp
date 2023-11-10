%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022-2023. All Rights Reserved.
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

-module(ssl_trace_SUITE).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ssl/src/ssl_api.hrl").

-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([tc_basic/0,
         tc_basic/1,
         tc_no_trace/0,
         tc_no_trace/1,
         tc_api_profile/0,
         tc_api_profile/1,
         tc_rle_profile/0,
         tc_rle_profile/1,
         tc_budget_option/0,
         tc_budget_option/1,
         tc_file_option/0,
         tc_file_option/1,
         tc_write/0,
         tc_write/1,
         tc_check_profiles/0,
         tc_check_profiles/1]).
-define(TRACE_FILE, "ssl_trace.txt").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() -> [{timetrap,{seconds,60}}].

all() -> [tc_basic, tc_no_trace, tc_api_profile, tc_rle_profile,
          tc_budget_option, tc_write, tc_file_option, tc_check_profiles].

init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
            ssl_test_lib:make_rsa_cert(Config)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, Config) ->
    ssl_trace:stop(),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
tc_basic() ->
    [{doc, "Basic test of ssl_trace API"}].
tc_basic(_Config) ->
    {ok, L0} = ssl_trace:start(),
    true = is_pid(whereis(ssl_trace)),
    true = is_list(L0),
    {ok,L0} = ssl_trace:on(),
    {ok,L0} = ssl_trace:on(),
    L0 = ssl_trace:is_on(),
    [] = ssl_trace:is_off(),

    L1 = [hd(L0)],
    L2 = tl(L0),
    {ok,L1} = ssl_trace:off(L2),

    L1 = ssl_trace:is_on(),
    L2 = ssl_trace:is_off(),

    {ok,[]} = ssl_trace:off(),
    {ok,[]} = ssl_trace:off(),

    [] = ssl_trace:is_on(),
    L0 = ssl_trace:is_off(),
    ok = ssl_trace:stop(),
    undefined = whereis(ssl_trace),

    {ok, EnabledProfiles} = ssl_trace:start(),
    [true = lists:member(ExpectedProfile, EnabledProfiles) ||
        ExpectedProfile <- [api, crt, csp, hbn, kdt, rle, ssn]],
    {ok, [api]} = ssl_trace:on(api),
    {ok, []} = ssl_trace:off(api),
    ok = ssl_trace:stop(),
    ok.

tc_no_trace() ->
    [{doc, "Verify there are no traces if not enabled"}].
tc_no_trace(Config) ->
    Ref = ssl_trace_start(),
    [Server, Client] = ssl_connect(Config),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ExpectedTraces =
        #{call => [], processed => [], exception_from => [], return_from => []},
    ExpectedTraces = receive_map(Ref),
    ok = ssl_trace:stop(),
    ok.

tc_api_profile() ->
    [{doc, "Verify traces for 'api' trace profile"}].
tc_api_profile(Config) ->
    On = [api, rle],
    Off = [],
    TracesAfterConnect =
        #{
          call =>
              [{"    (server) -> ssl:handshake/2", ssl, handshake},
               {"    (server) -> tls_server_connection:initial_hello/3",
                tls_server_connection, initial_hello},
               {"    (client) -> tls_client_connection:initial_hello/3",
                tls_client_connection, initial_hello}],
         return_from =>
              [{"    (server) <- ssl:listen/2 returned", ssl, listen},
               {"    (server) <- tls_server_connection:initial_hello/3 returned",
                tls_server_connection, initial_hello},
               {"    (client) <- tls_client_connection:initial_hello/3 returned",
                tls_client_connection, initial_hello},
               {"    (client) <- ssl_gen_statem:connect/8 returned",
                ssl_gen_statem, connect},
               {"    (client) <- ssl:connect/3 returned", ssl, connect},
               {"    (server) <- ssl:handshake/2 returned", ssl, handshake},
               {"    (client) <- tls_sender:init/3 returned", tls_sender, init},
               {"    (server) <- tls_sender:init/3 returned", tls_sender, init}],
          processed =>
              ["rle ('?') -> ssl_gen_statem:init/1 (*client)",
               "rle ('?') -> ssl_gen_statem:init/1 (*server)",
               "rle ('?') -> ssl:listen/2 (*server) Args",
               "rle ('?') -> ssl:connect/3 (*client) Args",
               "rle ('?') -> tls_sender:init/3 (*server)",
               "rle ('?') -> tls_sender:init/3 (*client)",
               "api (client) -> ssl_gen_statem:connect/8"]},
    TracesAfterDisconnect =
        #{
          call =>
              [{"    (client) -> ssl:close/1", ssl, close},
               {"    (client) -> ssl:close/1", ssl, close},
               {"    (client) -> ssl_gen_statem:close/2", ssl_gen_statem, close},
               {"    (client) -> ssl_gen_statem:terminate_alert/1",
                ssl_gen_statem, terminate_alert},
               {"    (server) -> ssl:close/1", ssl, close},
               {"    (server) -> ssl_gen_statem:close/2", ssl_gen_statem, close},
               {"    (server) -> ssl_gen_statem:terminate_alert/1",
                ssl_gen_statem, terminate_alert}],
         return_from =>
              [{"    (client) <- ssl:close/1 returned", ssl, close},
               {"    (client) <- ssl:close/1 returned", ssl, close},
               {"    (client) <- ssl_gen_statem:close/2 returned",
                ssl_gen_statem, close},
               {"    (client) <- ssl_gen_statem:terminate_alert/1 returned",
                ssl_gen_statem, terminate_alert},
               {"    (server) <- ssl:close/1 returned", ssl, close},
               {"    (server) <- ssl_gen_statem:close/2 returned",
                ssl_gen_statem, close},
               {"    (server) <- ssl_gen_statem:terminate_alert/1 returned",
                ssl_gen_statem, terminate_alert}],
          exception_from =>
              [{"    (server) exception_from ssl_gen_statem:init/1  {exit,{shutdown,normal}}",
                ssl_gen_statem, init},
               {"    (client) exception_from ssl_gen_statem:init/1  {exit,{shutdown,normal}}",
                ssl_gen_statem, init}]},
    Ref = ssl_trace_start(),
    {ok, On} = ssl_trace:on(On),
    Delta = On -- Off,
    {ok, Delta} = ssl_trace:off(Off),
    [Server, Client] = ssl_connect(Config),
    UnhandledTraceCnt1 =
        #{call => 2, processed => 0, exception_from => no_trace_received,
          return_from => 2},
    check_trace_map(Ref, TracesAfterConnect, UnhandledTraceCnt1),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    UnhandledTraceCnt2 =
        #{call => 0, processed => no_trace_received, exception_from => 0,
          return_from => 0},
    check_trace_map(Ref, TracesAfterDisconnect, UnhandledTraceCnt2),
    ssl_trace:stop(),
    ok.

tc_rle_profile() ->
    [{doc, "Verify traces for 'rle' trace profile"}].
tc_rle_profile(Config) ->
    On = [rle],
    ExpectedTraces =
        #{
          call =>
              [],
         return_from =>
              [{"    (client) <- ssl:connect/3 returned", ssl, connect},
               {"    (server) <- ssl:listen/2 returned", ssl, listen},
               {"    (client) <- tls_sender:init/3 returned", tls_sender, init},
               {"    (server) <- tls_sender:init/3 returned", tls_sender, init}],
          processed =>
              ["rle ('?') -> ssl:listen/2 (*server) Args =",
               "rle ('?') -> ssl:connect/3 (*client) Args",
               "rle ('?') -> ssl_gen_statem:init/1 (*server) Args = [[server",
               "rle ('?') -> ssl_gen_statem:init/1 (*client) Args = [[client",
               "rle ('?') -> tls_sender:init/3 (*server)",
               "rle ('?') -> tls_sender:init/3 (*client)"]},
    Ref = ssl_trace_start(),
    {ok, On} = ssl_trace:on(On),
    [Server, Client] = ssl_connect(Config),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    UnhandledTraceCnt =
        #{call => no_trace_received, processed => 0, exception_from => 2,
          return_from => 0},
    check_trace_map(Ref, ExpectedTraces, UnhandledTraceCnt),
    ssl_trace:stop(),
    ok.

tc_budget_option() ->
    [{doc, "Verify that budget option limits amount of traces"}].
tc_budget_option(Config) ->
    Ref = ssl_trace_start(make_ref(), [{budget, 10}]),
    {ok, [api,rle]} = ssl_trace:on([api,rle]),
    ssl_trace:write("Not a trace from dbg - not included in budget", []),
    [Server, Client] = ssl_connect(Config),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    CountReceived = fun(Reference) ->
                            ReceiveStats = check_trace_map(Reference, #{}),
                            ReceivedNumbers =
                                lists:filter(fun is_number/1,
                                             maps:values(ReceiveStats)),
                            lists:sum(ReceivedNumbers)
                    end,
    ssl_trace:stop(),
    ExpectedTraceCnt = 10,
    ActualTraceCnt = CountReceived(Ref),
    case ExpectedTraceCnt == ActualTraceCnt of
        true ->
            ok;
        _ ->
            ?CT_FAIL("Expected ~w traces, but found ~w",
                  [ExpectedTraceCnt, ActualTraceCnt])
    end.

tc_file_option() ->
    [{doc, "Verify that file option redirects traces to file"}].
tc_file_option(Config) ->
    _Ref = ssl_trace_start(make_ref(), [{budget, 10}, file]),
    {ok, [api,rle]} = ssl_trace:on([api,rle]),
    [Server, Client] = ssl_connect(Config),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ActualTraceCnt = count_line(?TRACE_FILE),
    ExpectedTraceCnt = 11, %% budget + 1 message about end of budget
    ssl_trace:stop(),
    case ExpectedTraceCnt == ActualTraceCnt of
        true ->
            ok;
        _ ->
            ?CT_FAIL("Expected ~w traces, but found ~w",
                  [ExpectedTraceCnt, ActualTraceCnt])
    end.

tc_write() ->
    [{doc, "Verify that custom messages can be written"}].
tc_write(_Config) ->
    _Ref = ssl_trace_start(make_ref(), [{budget, 10}, file]),
    {ok, [api,rle]} = ssl_trace:on([api,rle]),
    ssl_trace:write("Custom trace message ~w", [msg]),
    ActualTraceCnt = count_line(?TRACE_FILE),
    ExpectedTraceCnt = 1,
    ssl_trace:stop(),
    case ExpectedTraceCnt == ActualTraceCnt of
        true ->
            ok;
        _ ->
            ?CT_FAIL("Expected ~w traces, but found ~w",
                  [ExpectedTraceCnt, ActualTraceCnt])
    end.

tc_check_profiles() ->
    [{doc, "Verify that trace profile contain valid functions"}].
tc_check_profiles(_Config) ->
    CheckFun =
        fun(Profile, Module, Fun, DefinedFunctions) ->
                case lists:member(Fun, DefinedFunctions) of
                    true -> ok;
                    _ ->
                        {F, A} = Fun,
                        ct:fail("~w:~w/~w from '~w' trace profile not found",
                                [Module, F, A, Profile])
                end
        end,
    CheckModule =
        fun(Profile, {Module, Funs}) ->
                DefinedFunctions = Module:module_info(functions),
                [CheckFun(Profile, Module, F, DefinedFunctions) ||
                    F <- Funs]
        end,
    CheckTProfile =
        fun({Profile, _, _, ModFunsTuples}) ->
                [CheckModule(Profile, MFTuple) ||
                    MFTuple <- ModFunsTuples]
        end,
    [CheckTProfile(P) || P <- ssl_trace:trace_profiles()],
    ok.

%%%----------------------------------------------------------------
ssl_trace_start() ->
    ssl_trace_start(make_ref(), []).

ssl_trace_start(Ref, TraceOpts) ->
    TestProcess = self(),
    {ok, [_|_]} = ssl_trace:start(fun(Format,Args) ->
                                          ct:log(Format, Args),
                                          TestProcess ! {Ref, Args}
                                  end,
                                  TraceOpts),
    Ref.

receive_map(Ref) ->
    Empty = #{call => [], return_from => [], exception_from => [],
              processed => []},
    receive_map(Ref, Empty).

receive_map(Ref,
               Map = #{call := Call, return_from := Return,
                       exception_from := Exception, processed := Processed}) ->
    receive
        {Ref, Msg = [_, {call, {_, _, _}}, _]} ->
            receive_map(Ref, Map#{call => [Msg|Call]});
        {Ref, Msg = [_, {return_from, {_, _, _}, _}, _]} ->
            receive_map(Ref, Map#{return_from => [Msg|Return]});
        {Ref, Msg = [_, {exception_from, {_, _, _}, _}, _]} ->
            receive_map(Ref, Map#{exception_from => [Msg|Exception]});
        {Ref, Msg = [_Timestamp, _Pid, _ExpectString]} ->
            %% processed means a trace was processed by Module:handle_trace
            %% function and is not received as a trace tuple
            receive_map(Ref, Map#{processed => [Msg|Processed]})
    after 5000 ->
            Map
    end.

check_trace_map(Ref, ExpectedTraces) ->
    Received = receive_map(Ref),
    L = [check_key(Type, ExpectedTraces, maps:get(Type, Received)) ||
            Type <- maps:keys(Received)],
    maps:from_list(L).

check_trace_map(Ref, ExpectedTraces, ExpectedRemainders) ->
    ActualRemainders = check_trace_map(Ref, ExpectedTraces),
    case ExpectedRemainders == ActualRemainders of
        true ->
            ok;
        _ ->
            ?CT_FAIL("Expected trace remainders = ~w ~n"
                 "Actual trace remainders = ~w",
                 [ExpectedRemainders, ActualRemainders])
    end.

check_key(Type, ExpectedTraces, ReceivedPerType) ->
    ReceivedPerTypeCnt = length(ReceivedPerType),
    ?CT_LOG("Received Type = ~w Messages# = ~w", [Type, ReceivedPerTypeCnt]),
    case ReceivedPerTypeCnt > 0 of
        true ->
            ExpectedPerType = maps:get(Type, ExpectedTraces, []),
            ExpectedPerTypeCnt = length(ExpectedPerType),
            check_trace(Type, ExpectedPerType, ReceivedPerType),
            {Type, ReceivedPerTypeCnt - ExpectedPerTypeCnt};
        _ ->
            {Type, no_trace_received}
    end.

-define(CHECK_TRACE(PATTERN, Expected),
        fun({ExpectedString, Module, Function}) ->
                P2 = fun(Received) ->
                             PATTERN = Received,
                             SearchResult =
                                 string:str(lists:flatten(Txt), ExpectedString),
                             case {Module == M, Function == F, SearchResult > 0} of
                                 {true, true, true} ->
                                     true;
                                 _ -> false
                             end
                     end,
                Result = lists:any(P2, ReceivedPerType),
                case Result of
                    false ->
                        F = "Trace not found: {~s, ~w, ~w}",
                        ?CT_FAIL(F, [ExpectedString, Module, Function]);
                    _ -> ok
                end,
                Result
        end).

-define(CHECK_PROCESSED_TRACE(PATTERN, Expected),
        fun(ExpectedString) ->
                P2 = fun(Received) ->
                             PATTERN = Received,
                             SearchResult =
                                 string:str(lists:flatten(Txt), ExpectedString),
                             SearchResult > 0
                     end,
                Result = lists:any(P2, ReceivedPerType),
                case Result of
                    false ->
                        F = "Processed trace not found: ~s",
                        ?CT_FAIL(F, [ExpectedString]);
                    _ -> ok
                end,
                Result
        end).

check_trace(call, ExpectedPerType, ReceivedPerType) ->
    P1 = ?CHECK_TRACE([Txt, {call, {M, F, _Args}}, _], Expected),
    true = lists:all(P1, ExpectedPerType);
check_trace(return_from, ExpectedPerType, ReceivedPerType) ->
    P1 = ?CHECK_TRACE([Txt, {return_from, {M, F, _Args}, _Return}, _], Expected),
    true = lists:all(P1, ExpectedPerType);
check_trace(exception_from, ExpectedPerType, ReceivedPerType) ->
    P1 = ?CHECK_TRACE([Txt, {exception_from, {M, F, _Args}, _Return}, _], Expected),
    true = lists:all(P1, ExpectedPerType);
check_trace(processed, ExpectedPerType, ReceivedPerType) ->
    P1 = ?CHECK_PROCESSED_TRACE([_Timestamp, _Pid, Txt], Expected),
    true = lists:all(P1, ExpectedPerType);
check_trace(Type, _ExpectedPerType, _ReceivedPerType) ->
    ?CT_FAIL("Type = ~w not checked", [Type]),
    ok.

count_line(Filename) ->
    case file:open(Filename, [read]) of
        {ok, IoDevice} ->
            Count = count_line(IoDevice, 0),
            file:close(IoDevice),
            Count;
        {error, Reason} ->
            ?CT_PAL("~s open error  reason:~s~n", [Filename, Reason]),
            ct:fail(Reason)
    end.

count_line(IoDevice, Count) ->
    case file:read_line(IoDevice) of
        {ok, _} -> count_line(IoDevice, Count+1);
        eof     -> Count
    end.

ssl_connect(Config) when is_list(Config) ->
    ?CT_LOG("Establishing connection for producing traces", []),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{keepalive, true},{active, false}
					       | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{keepalive, true},{active, false}
					      | ClientOpts]}]),
    ?CT_LOG("Testcase ~p, Client ~p  Server ~p ~n", [self(), Client, Server]),
    [Server, Client].
