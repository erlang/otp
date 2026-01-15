%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

%% Variable that controls "verbosity" of the test case(s):
%%
%%         ESOCK_TEST_QUIET: true (default) | false
%%

%% Run the entire test suite: 
%% ts:run(kernel, socket_traffic_SUITE, [batch]).
%%
%% Run a specific group:
%% ts:run(kernel, socket_traffic_SUITE, {group, foo}, [batch]).
%%
%% Run a specific test case:
%% ts:run(kernel, socket_traffic_SUITE, foo, [batch]).
%%
%% (cd /mnt/c/$LOCAL_TESTS/26/kernel_test/ && $ERL_TOP/bin/win32/erl.exe -sname kernel-26-tester -pa c:$LOCAL_TESTS/26/test_server)
%% application:set_env(kernel, test_inet_backends, true).
%%
%% S = fun() -> ts:run(kernel, socket_SUITE, [batch]) end.
%% S = fun(SUITE) -> ts:run(kernel, SUITE, [batch]) end.
%% G = fun(GROUP) -> ts:run(kernel, socket_SUITE, {group, GROUP}, [batch]) end.
%% G = fun(SUITE, GROUP) -> ts:run(kernel, SUITE, {group, GROUP}, [batch]) end.
%% T = fun(TC) -> ts:run(kernel, socket_SUITE, TC, [batch]) end.
%%
%% S = fun() -> ct:run_test([{suite, socket_SUITE}]) end.
%% S = fun(SUITE) -> ct:run_test([{suite, SUITE}]) end.
%% G = fun(GROUP) -> ct:run_test([{suite, socket_SUITE}, {group, GROUP}]) end.
%% G = fun(SUITE, GROUP) -> ct:run_test([{suite, SUITE}, {group, GROUP}]) end.
%% T = fun(TC) -> ct:run_test([{suite, socket_SUITE}, {testcase, TC}]) end.
%% T = fun(S, TC) -> ct:run_test([{suite, S}, {testcase, TC}]) end.
%% T = fun(S, G, TC) -> ct:run_test([{suite, S}, {group, G}, {testcase, TC}]) end.
%%
%% Some official info about AF_UNIX
%% https://devblogs.microsoft.com/commandline/windowswsl-interop-with-af_unix/



-module(socket_traffic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include("socket_test_evaluator.hrl").
-include("kernel_test_lib.hrl").

%% Suite exports
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
         %% *** Counters ***
         traffic_send_and_recv_counters_tcp4/1,
         traffic_send_and_recv_counters_tcp6/1,
         traffic_send_and_recv_counters_tcpL/1,
         traffic_send_and_recv_counters_sctp4/1,
         traffic_send_and_recv_counters_sctp6/1,
         traffic_sendmsg_and_recvmsg_counters_tcp4/1,
         traffic_sendmsg_and_recvmsg_counters_tcp6/1,
         traffic_sendmsg_and_recvmsg_counters_tcpL/1,
         traffic_sendmsg_and_recvmsg_counters_sctp4/1,
         traffic_sendmsg_and_recvmsg_counters_sctp6/1,
         traffic_sendto_and_recvfrom_counters_udp4/1,
         traffic_sendto_and_recvfrom_counters_udp6/1,
         traffic_sendto_and_recvfrom_counters_udpL/1,
         traffic_sendmsg_and_recvmsg_counters_udp4/1,
         traffic_sendmsg_and_recvmsg_counters_udp6/1,
         traffic_sendmsg_and_recvmsg_counters_udpL/1,

         %% *** Chunks ***
         traffic_send_and_recv_chunks_tcp4/1,
         traffic_send_and_recv_chunks_tcp6/1,
         traffic_send_and_recv_chunks_tcpL/1,
         traffic_send_and_recv_chunks_sctp4/1,
         traffic_send_and_recv_chunks_sctp6/1,

         %% *** Ping Pong ***
         traffic_ping_pong_small_send_and_recv_tcp4/1,
         traffic_ping_pong_small_send_and_recv_tcp6/1,
         traffic_ping_pong_small_send_and_recv_tcpL/1,
         traffic_ping_pong_small_send_and_recv_sctp4/1,
         traffic_ping_pong_small_send_and_recv_sctp6/1,
         traffic_ping_pong_medium_send_and_recv_tcp4/1,
         traffic_ping_pong_medium_send_and_recv_tcp6/1,
         traffic_ping_pong_medium_send_and_recv_tcpL/1,
         traffic_ping_pong_medium_send_and_recv_sctp4/1,
         traffic_ping_pong_medium_send_and_recv_sctp6/1,
         traffic_ping_pong_large_send_and_recv_tcp4/1,
         traffic_ping_pong_large_send_and_recv_tcp6/1,
         traffic_ping_pong_large_send_and_recv_tcpL/1,
         traffic_ping_pong_large_send_and_recv_sctp4/1,
         traffic_ping_pong_large_send_and_recv_sctp6/1,

         traffic_ping_pong_small_sendto_and_recvfrom_udp4/1,
         traffic_ping_pong_small_sendto_and_recvfrom_udp6/1,
         traffic_ping_pong_small_sendto_and_recvfrom_udpL/1,
         traffic_ping_pong_medium_sendto_and_recvfrom_udp4/1,
         traffic_ping_pong_medium_sendto_and_recvfrom_udp6/1,
         traffic_ping_pong_medium_sendto_and_recvfrom_udpL/1,

         traffic_ping_pong_small_sendmsg_and_recvmsg_tcp4/1,
         traffic_ping_pong_small_sendmsg_and_recvmsg_tcp6/1,
         traffic_ping_pong_small_sendmsg_and_recvmsg_tcpL/1,
         traffic_ping_pong_small_sendmsg_and_recvmsg_sctp4/1,
         traffic_ping_pong_small_sendmsg_and_recvmsg_sctp6/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp4/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp6/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_tcpL/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_sctp4/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_sctp6/1,
         traffic_ping_pong_large_sendmsg_and_recvmsg_tcp4/1,
         traffic_ping_pong_large_sendmsg_and_recvmsg_tcp6/1,
         traffic_ping_pong_large_sendmsg_and_recvmsg_tcpL/1,
         traffic_ping_pong_large_sendmsg_and_recvmsg_sctp4/1,
         traffic_ping_pong_large_sendmsg_and_recvmsg_sctp6/1,

         traffic_ping_pong_small_sendmsg_and_recvmsg_udp4/1,
         traffic_ping_pong_small_sendmsg_and_recvmsg_udp6/1,
         traffic_ping_pong_small_sendmsg_and_recvmsg_udpL/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_udp4/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_udp6/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_udpL/1,

         %% *** Bench ***
         traffic_bench_sendv_and_recv_tcp4/1,
         traffic_bench_send_and_recv_tcp4/1,
         traffic_bench_sendv_and_recv_tcp6/1,
         traffic_bench_send_and_recv_tcp6/1,
         traffic_bench_sendv_and_recv_tcpL/1,
         traffic_bench_send_and_recv_tcpL/1
        ]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SLIB,       socket_test_lib).
-define(KLIB,       kernel_test_lib).
-define(LOGGER,     socket_test_logger).

-define(DATA,       <<"HOPPSAN">>). % Temporary
-define(FAIL(R),    exit(R)).

-define(TPP_SMALL,  lists:seq(1, 8)).
-define(TPP_MEDIUM, lists:flatten(lists:duplicate(100, ?TPP_SMALL))).
-define(TPP_LARGE,  lists:flatten(lists:duplicate(100, ?TPP_MEDIUM))).

-define(TPP_SMALL_NUM,  5000).
-define(TPP_MEDIUM_NUM, 500).
-define(TPP_LARGE_NUM,  50).
-define(TPP_NUM(Config, Base), (Base) div lookup(kernel_factor, 1, Config)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes,1}}].

all() -> 
    %% Groups = [
    %%           {counters,  "ESOCK_TEST_TRAFFIC_COUNTERS",  include},
    %%           {chunks,    "ESOCK_TEST_TRAFFIC_CHUNKS",    include},
    %%           {ping_pong, "ESOCK_TEST_TRAFFIC_PING_PONG", include},
    %%           {tbench,    "ESOCK_TEST_TRAFFIC_BENCH",     include}
    %%          ],
    %% [use_group(Group, Env, Default) || {Group, Env, Default} <- Groups].
    [{group, standard}].

%% use_group(_Group, undefined, exclude) ->
%%     [];
%% use_group(Group, undefined, _Default) ->
%%     [{group, Group}];
%% use_group(Group, Env, Default) ->
%% 	case os:getenv(Env) of
%% 	    false when (Default =:= include) ->
%% 		[{group, Group}];
%% 	    false ->
%% 		[];
%% 	    Val ->
%% 		case list_to_atom(string:to_lower(Val)) of
%% 		    Use when (Use =:= include) orelse 
%% 			     (Use =:= enable) orelse 
%% 			     (Use =:= true) ->
%% 			[{group, Group}];
%% 		    _ ->
%% 			[]
%% 		end
%% 	end.
    

groups() -> 
    [
     %% Top level "wrapper" groups
     %% A normal (standard) test run will be running the 'suite'.
     %% Which will run the 'standard' group, with all test cases but
     %% the 'tbench' group of test cases will run with a short
     %% run time.
     %% A benchmark test run will run the 'bench' group directly,
     %% with an "extended" run time.
     %%
     {standard,            [], standard_cases()},
     {bench,               [], bench_cases()},

     {counters,            [], traffic_counters_cases()},
     {chunks,              [], traffic_chunks_cases()},
     {ping_pong,           [], traffic_ping_pong_cases()},
     {tbench,              [], traffic_bench_cases()},
     {pp_send_recv,        [], traffic_pp_send_recv_cases()},
     {pp_sendto_recvfrom,  [], traffic_pp_sendto_recvfrom_cases()},
     {pp_sendmsg_recvmsg,  [], traffic_pp_sendmsg_recvmsg_cases()}
    ].

standard_cases() ->
    [
     {group, counters},
     {group, chunks},
     {group, ping_pong},
     {group, tbench}
    ].

bench_cases() ->
    [
     {group, tbench}
    ].

traffic_counters_cases() ->
    [
     traffic_send_and_recv_counters_tcp4,
     traffic_send_and_recv_counters_tcp6,
     traffic_send_and_recv_counters_tcpL,
     traffic_send_and_recv_counters_sctp4,
     traffic_send_and_recv_counters_sctp6,
     traffic_sendmsg_and_recvmsg_counters_tcp4,
     traffic_sendmsg_and_recvmsg_counters_tcp6,
     traffic_sendmsg_and_recvmsg_counters_tcpL,
     traffic_sendmsg_and_recvmsg_counters_sctp4,
     traffic_sendmsg_and_recvmsg_counters_sctp6,
     traffic_sendto_and_recvfrom_counters_udp4,
     traffic_sendto_and_recvfrom_counters_udp6,
     traffic_sendto_and_recvfrom_counters_udpL,
     traffic_sendmsg_and_recvmsg_counters_udp4,
     traffic_sendmsg_and_recvmsg_counters_udp6,
     traffic_sendmsg_and_recvmsg_counters_udpL
    ].

traffic_chunks_cases() ->
    [
     traffic_send_and_recv_chunks_tcp4,
     traffic_send_and_recv_chunks_tcp6,
     traffic_send_and_recv_chunks_tcpL,
     traffic_send_and_recv_chunks_sctp4,
     traffic_send_and_recv_chunks_sctp6
    ].

traffic_ping_pong_cases() ->
    [
     {group, pp_send_recv},
     {group, pp_sendto_recvfrom},
     {group, pp_sendmsg_recvmsg}
    ].

traffic_bench_cases() ->
    [
     traffic_bench_sendv_and_recv_tcp4,
     traffic_bench_send_and_recv_tcp4,
     traffic_bench_sendv_and_recv_tcp6,
     traffic_bench_send_and_recv_tcp6,
     traffic_bench_sendv_and_recv_tcpL,
     traffic_bench_send_and_recv_tcpL
    ].

traffic_pp_send_recv_cases() ->
    [
     traffic_ping_pong_small_send_and_recv_tcp4,
     traffic_ping_pong_small_send_and_recv_tcp6,
     traffic_ping_pong_small_send_and_recv_tcpL,
     traffic_ping_pong_small_send_and_recv_sctp4,
     traffic_ping_pong_small_send_and_recv_sctp6,
     traffic_ping_pong_medium_send_and_recv_tcp4,
     traffic_ping_pong_medium_send_and_recv_tcp6,
     traffic_ping_pong_medium_send_and_recv_tcpL,
     traffic_ping_pong_medium_send_and_recv_sctp4,
     traffic_ping_pong_medium_send_and_recv_sctp6,
     traffic_ping_pong_large_send_and_recv_tcp4,
     traffic_ping_pong_large_send_and_recv_tcp6,
     traffic_ping_pong_large_send_and_recv_tcpL,
     traffic_ping_pong_large_send_and_recv_sctp4,
     traffic_ping_pong_large_send_and_recv_sctp6
    ].    

traffic_pp_sendto_recvfrom_cases() ->
    [
     traffic_ping_pong_small_sendto_and_recvfrom_udp4,
     traffic_ping_pong_small_sendto_and_recvfrom_udp6,
     traffic_ping_pong_small_sendto_and_recvfrom_udpL,
     traffic_ping_pong_medium_sendto_and_recvfrom_udp4,
     traffic_ping_pong_medium_sendto_and_recvfrom_udp6,
     traffic_ping_pong_medium_sendto_and_recvfrom_udpL
    ].

traffic_pp_sendmsg_recvmsg_cases() ->
    [
     traffic_ping_pong_small_sendmsg_and_recvmsg_tcp4,
     traffic_ping_pong_small_sendmsg_and_recvmsg_tcp6,
     traffic_ping_pong_small_sendmsg_and_recvmsg_tcpL,
     traffic_ping_pong_small_sendmsg_and_recvmsg_sctp4,
     traffic_ping_pong_small_sendmsg_and_recvmsg_sctp6,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp4,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp6,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_tcpL,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_sctp4,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_sctp6,
     traffic_ping_pong_large_sendmsg_and_recvmsg_tcp4,
     traffic_ping_pong_large_sendmsg_and_recvmsg_tcp6,
     traffic_ping_pong_large_sendmsg_and_recvmsg_tcpL,
     traffic_ping_pong_large_sendmsg_and_recvmsg_sctp4,
     traffic_ping_pong_large_sendmsg_and_recvmsg_sctp6,

     traffic_ping_pong_small_sendmsg_and_recvmsg_udp4,
     traffic_ping_pong_small_sendmsg_and_recvmsg_udp6,
     traffic_ping_pong_small_sendmsg_and_recvmsg_udpL,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_udp4,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_udp6,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_udpL
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config0) ->
    ?P("init_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),
    
    try socket:info() of
        #{} ->
            case ?KLIB:init_per_suite(Config0) of
                {skip, _} = SKIP ->
                    SKIP;

                Config1 when is_list(Config1) ->

                    ?P("init_per_suite -> end when "
                       "~n      Config: ~p", [Config1]),

                    %% We need a monitor on this node also
                    kernel_test_sys_monitor:start(),

                    socket:use_registry(false),
                    case quiet_mode(Config1) of
                        default ->
                            case ?LOGGER:start() of
                                ok ->
                                    Config1;
                                {error, Reason} ->
                                    ?P("init_per_suite -> "
                                       "Failed starting logger"
                                       "~n   Reason: ~p"
                                       "~n", [Reason]),
                                    {skip, "Failed starting logger"}
                            end;
                        Quiet ->
                            case ?LOGGER:start(Quiet) of
                                ok ->
                                    [{esock_test_quiet, Quiet} | Config1];
                                {error, Reason} ->
                                    ?P("init_per_suite -> "
                                       "Failed starting logger"
                                       "~n   Reason: ~p"
                                       "~n", [Reason]),
                                    {skip, "Failed starting logger"}
                            end
                    end
            end
    catch
        error : notsup ->
            {skip, "esock not supported"};
        error : undef ->
            {skip, "esock not configured"}
    end.

end_per_suite(Config0) ->

    ?P("end_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    %% Stop the local monitor
    kernel_test_sys_monitor:stop(),

    (catch ?LOGGER:stop()),

    Config1 = ?KLIB:end_per_suite(Config0),

    ?P("end_per_suite -> "
       "~n      Nodes: ~p", [erlang:nodes()]),

    Config1.


init_per_group(standard = GroupName, Config) ->
    ?P("init_per_group -> entry with"
       "~n      GroupName: ~p"
       "~n      Config:    ~p"
       "~n   when"
       "~n      Nodes:     ~p", [GroupName, Config, erlang:nodes()]),
    [{category, GroupName} | Config];
init_per_group(bench = GroupName, Config) ->
    ?P("init_per_group -> entry with"
       "~n      GroupName: ~p"
       "~n      Config:    ~p"
       "~n   when"
       "~n      Nodes:     ~p", [GroupName, Config, erlang:nodes()]),
    case proplists:get_value(category, Config, undefined) of
        undefined ->
            [{category, GroupName} | Config];
        _ ->
            Config
    end;
init_per_group(_GroupName, Config) ->
    ?P("init_per_group -> entry with"
       "~n      GroupName: ~p"
       "~n      Config:    ~p"
       "~n   when"
       "~n      Nodes:     ~p", [_GroupName, Config, erlang:nodes()]),
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_TC, Config) ->
    io:format("init_per_testcase(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [_TC, Config]),
    Config.

end_per_testcase(_TC, Config) ->
    Config.


quiet_mode(Config) ->
    case lists:keysearch(esock_test_quiet, 1, Config) of
        {value, {esock_test_quiet, Quiet}} ->
            Quiet;
        false ->
            case os:getenv("ESOCK_TEST_QUIET") of
                "true"  -> true;
                "false" -> false;
                _       -> default
            end
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test that the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use TCP on IPv4.

traffic_send_and_recv_counters_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(traffic_send_and_recv_counters_tcp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 recv   => fun(S)    -> socket:recv(S)    end,
                                 send   => fun(S, D) -> socket:send(S, D) end},
                   ok = traffic_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test that the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use TCP on IPv6.

traffic_send_and_recv_counters_tcp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => tcp,
                                 recv   => fun(S)    -> socket:recv(S)    end,
                                 send   => fun(S, D) -> socket:send(S, D) end},
                   ok = traffic_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test that the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use default (TCP) on local.

traffic_send_and_recv_counters_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   InitState = #{domain => local,
                                 proto  => default,
                                 recv   => fun(S)    -> socket:recv(S)    end,
                                 send   => fun(S, D) -> socket:send(S, D) end},
                   ok = traffic_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test that the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use SCTP on IPv4.

traffic_send_and_recv_counters_sctp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   has_support_ipv4(),
                   has_support_sctp()
           end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => sctp,
                                 recv   => fun(S)    -> socket:recv(S)    end,
                                 send   => fun(S, D) -> socket:send(S, D) end},
                   ok = traffic_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test that the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use SCTP on IPv6.

traffic_send_and_recv_counters_sctp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   has_support_ipv6(),
                   has_support_sctp()
           end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => sctp,
                                 recv   => fun(S)    -> socket:recv(S)    end,
                                 send   => fun(S, D) -> socket:send(S, D) end},
                   ok = traffic_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test that the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use TCP on IPv4.

traffic_sendmsg_and_recvmsg_counters_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
                   has_support_ipv4()
           end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 recv   => fun(S) ->
                                                   case socket:recvmsg(S) of
                                                       {ok, #{iov  := [Data]}} ->
                                                           {ok, Data};
                                                       {error, _} = ERROR ->
                                                           ERROR
                                                   end
                                           end,
                                 send   => fun(S, Data) ->
                                                   Msg = #{iov => [Data]},
                                                   socket:sendmsg(S, Msg)
                                           end},
                   ok = traffic_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test that the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use TCP on IPv6.

traffic_sendmsg_and_recvmsg_counters_tcp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
                   has_support_ipv6()
           end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => tcp,
                                 recv   => fun(S) ->
                                                   case socket:recvmsg(S) of
                                                       {ok, #{iov  := [Data]}} ->
                                                           {ok, Data};
                                                       {error, _} = ERROR ->
                                                           ERROR
                                                   end
                                           end,
                                 send   => fun(S, Data) ->
                                                   Msg = #{iov => [Data]},
                                                   socket:sendmsg(S, Msg)
                                           end},
                   ok = traffic_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test that the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use default (TCP) on local.

traffic_sendmsg_and_recvmsg_counters_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   InitState = #{domain => local,
                                 proto  => default,
                                 recv   => fun(S) ->
                                                   case socket:recvmsg(S) of
                                                       {ok, #{iov  := [Data]}} ->
                                                           {ok, Data};
                                                       {error, _} = ERROR ->
                                                           ERROR
                                                   end
                                           end,
                                 send   => fun(S, Data) ->
                                                   Msg = #{iov => [Data]},
                                                   socket:sendmsg(S, Msg)
                                           end},
                   ok = traffic_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test that the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use SCTP on IPv4.

traffic_sendmsg_and_recvmsg_counters_sctp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   is_not_windows(),
                   has_support_ipv4(),
                   has_support_sctp()
           end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => sctp,
                                 recv   => fun(S) ->
                                                   case socket:recvmsg(S) of
                                                       {ok, #{iov  := [Data]}} ->
                                                           {ok, Data};
                                                       {error, _} = ERROR ->
                                                           ERROR
                                                   end
                                           end,
                                 send   => fun(S, Data) ->
                                                   Msg = #{iov => [Data]},
                                                   socket:sendmsg(S, Msg)
                                           end},
                   ok = traffic_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test that the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use SCTP on IPv6.

traffic_sendmsg_and_recvmsg_counters_sctp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   is_not_windows(),
                   has_support_ipv6(),
                   has_support_sctp()
           end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => sctp,
                                 recv   => fun(S) ->
                                                   case socket:recvmsg(S) of
                                                       {ok, #{iov  := [Data]}} ->
                                                           {ok, Data};
                                                       {error, _} = ERROR ->
                                                           ERROR
                                                   end
                                           end,
                                 send   => fun(S, Data) ->
                                                   Msg = #{iov => [Data]},
                                                   socket:sendmsg(S, Msg)
                                           end},
                   ok = traffic_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

traffic_send_and_recv_stream(InitState) ->
    ServerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "create listen socket",
           cmd  => fun(#{domain := Domain, proto := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, eafnosupport = Reason} ->
                                   {skip, Reason};
                               {error, eprotonosupport = Reason} ->
                                   {skip, Reason};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{domain   := local,
                         lsock    := LSock,
                         local_sa := LSA} = _State) ->
                           case socket:bind(LSock, LSA) of
                               ok ->
                                   ok; % We do not care about the port for local
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{lsock    := LSock,
                         local_sa := LSA} = State) ->
                           case sock_bind(LSock, LSA) of
                               ok ->
                                   Port = sock_port(LSock),
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},
         #{desc => "initial (listen socket) counter validation (=zero)",
           cmd  => fun(#{lsock := LSock} = _State) ->
                           try socket:info(LSock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("Validate initial listen socket counters: "
                                               "~s", [format_counters(listen,
                                                                      Counters)]),
                                   traffic_sar_counters_validation(Counters)
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{domain   := local,
                         tester   := Tester,
                         local_sa := #{path := Path}}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Path),
                           ok;
                      (#{tester   := Tester,
                         lport    := Port}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, CSock} ->
                                   #{counters := LCnts} = socket:info(LSock),
                                   ?SEV_IPRINT("Validate listen counters: "
                                               "~s", [format_counters(listen,
                                                                      LCnts)]),
                                   traffic_sar_counters_validation(
                                     LCnts,
                                     [{acc_success, 1},
                                      {acc_fails,   0},
                                      {acc_tries,   1},
                                      {acc_waits,   0}]),
                                   #{counters := CCnts} = socket:info(CSock),
                                   ?SEV_IPRINT("Validate initial accept counters: "
                                               "~s", [format_counters(CCnts)]),
                                   traffic_sar_counters_validation(CCnts),
                                   {ok, State#{csock => CSock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "initial counter validation (=zero)",
           cmd  => fun(#{csock := Sock} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("Validate initial counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(Counters)
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         #{desc => "await continue (recv_and_validate 1)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv_and_validate)
                   end},
         #{desc => "recv (1)",
           cmd  => fun(#{csock := Sock,
                         recv  := Recv} = State) ->
                           case Recv(Sock) of
                               {ok, Data} ->
                                   ?SEV_IPRINT("recv ~p bytes", [size(Data)]),
                                   {ok, State#{read_pkg  => 1,
                                               read_byte => size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (recv 1)",
           cmd  => fun(#{csock     := Sock,
                         read_pkg  := Pkg,
                         read_byte := Byte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,     Pkg},
                                      {read_byte,    Byte},
                                      {read_tries,   any},
                                      {read_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (recv_and_validate 1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_and_validate),
                           ok
                   end},

         #{desc => "await continue (send_and_validate 1)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_and_validate)
                   end},
         #{desc => "send (1)",
           cmd  => fun(#{csock := Sock,
                         send  := Send} = State) ->
                           Data = ?DATA,
                           case Send(Sock, Data) of
                               ok ->
                                   ?SEV_IPRINT("sent ~p bytes", [size(Data)]),
                                   {ok, State#{write_pkg  => 1,
                                               write_byte => size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (send 1)",
           cmd  => fun(#{csock      := Sock,
                         read_pkg   := RPkg,
                         read_byte  := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,      RPkg},
                                      {read_byte,     RByte},
                                      {write_pkg,     SPkg},
                                      {write_byte,    SByte},
                                      {read_tries,    any},
                                      {write_tries,   any},
                                      {read_pkg_max,  any},
                                      {write_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (send_and_validate 1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_and_validate),
                           ok
                   end},

         #{desc => "await continue (recv_and_validate 2)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv_and_validate)
                   end},
         #{desc => "recv (2)",
           cmd  => fun(#{csock     := Sock,
                         recv      := Recv,
                         read_pkg  := Pkg,
                         read_byte := Byte} = State) ->
                           case Recv(Sock) of
                               {ok, Data} ->
                                   ?SEV_IPRINT("recv ~p bytes", [size(Data)]),
                                   {ok, State#{read_pkg  => Pkg + 1,
                                               read_byte => Byte + size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (recv 2)",
           cmd  => fun(#{csock      := Sock,
                         read_pkg   := RPkg,
                         read_byte  := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,      RPkg},
                                      {read_byte,     RByte},
                                      {write_pkg,     SPkg},
                                      {write_byte,    SByte},
                                      {read_tries,    any},
                                      {write_tries,   any},
                                      {read_pkg_max,  any},
                                      {write_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (recv_and_validate 2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_and_validate),
                           ok
                   end},

         #{desc => "await continue (send_and_validate 2)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_and_validate)
                   end},
         #{desc => "send (2)",
           cmd  => fun(#{csock      := Sock,
                         send       := Send,
                         write_pkg  := Pkg,
                         write_byte := Byte} = State) ->
                           Data = ?DATA,
                           case Send(Sock, Data) of
                               ok ->
                                   ?SEV_IPRINT("sent ~p bytes", [size(Data)]),
                                   {ok, State#{write_pkg  => Pkg + 1,
                                               write_byte => Byte + size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (send 2)",
           cmd  => fun(#{csock      := Sock,
                         read_pkg   := RPkg,
                         read_byte  := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,      RPkg},
                                      {read_byte,     RByte},
                                      {write_pkg,     SPkg},
                                      {write_byte,    SByte},
                                      {read_tries,    any},
                                      {write_tries,   any},
                                      {read_pkg_max,  any},
                                      {write_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (send_and_validate 2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_and_validate),
                           ok
                   end},


         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close connection socket (just in case)",
           cmd  => fun(#{csock := Sock} = State) ->
                           (catch socket:close(Sock)),
                           {ok, maps:remove(csock, State)}
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{domain   := local,
                         lsock    := Sock,
                         local_sa := #{path := Path}} = State) ->
                           ok = socket:close(Sock),
                           State1 =
                               unlink_path(Path,
                                           fun() ->
                                                   maps:remove(local_sa, State)
                                           end,
                                           fun() -> State end),
                           {ok, maps:remove(lsock, State1)};
                      (#{lsock := Sock} = State) ->
                           (catch socket:close(Sock)),
                           {ok, maps:remove(lsock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(#{domain := local} = State) ->
                           {Tester, Path} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, server_path => Path}};
                      (State) ->
                           {Tester, Port} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, server_port => Port}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which server (local) address",
           cmd  => fun(#{domain      := local = Domain,
                         server_path := Path} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           SSA = #{family => Domain, path => Path},
                           {ok, State#{local_sa => LSA, server_sa => SSA}};
                      (#{domain := Domain, server_port := Port} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           SSA = LSA#{port => Port},
                           {ok, State#{local_sa => LSA, server_sa => SSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain := Domain,
                         proto  := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, eafnosupport = Reason} ->
                                   {skip, Reason};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, local_sa := LSA} = _State) ->
                           case sock_bind(Sock, LSA) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect),
                           ok
                   end},
         #{desc => "connect to server",
           cmd  => fun(#{sock := Sock, server_sa := SSA}) ->
                           socket:connect(Sock, SSA)
                   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},

         #{desc => "await continue (send_and_validate 1)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_and_validate)
                   end},
         #{desc => "send (1)",
           cmd  => fun(#{sock := Sock,
                         send := Send} = State) ->
                           Data = ?DATA,
                           case Send(Sock, Data) of
                               ok ->
                                   ?SEV_IPRINT("sent ~p bytes", [size(Data)]),
                                   {ok, State#{write_pkg  => 1,
                                               write_byte => size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (send 1)",
           cmd  => fun(#{sock      := Sock,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{write_pkg,     SPkg},
                                      {write_byte,    SByte},
                                      {write_tries,   any},
                                      {write_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (send_and_validate 1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_and_validate),
                           ok
                   end},

         #{desc => "await continue (recv_and_validate 1)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv_and_validate)
                   end},
         #{desc => "recv (1)",
           cmd  => fun(#{sock := Sock,
                         recv := Recv} = State) ->
                           case Recv(Sock) of
                               {ok, Data} ->
                                   ?SEV_IPRINT("recv ~p bytes", [size(Data)]),
                                   {ok, State#{read_pkg  => 1,
                                               read_byte => size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (recv 1)",
           cmd  => fun(#{sock       := Sock,
                         read_pkg   := RPkg,
                         read_byte  := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,      RPkg},
                                      {read_byte,     RByte},
                                      {write_pkg,     SPkg},
                                      {write_byte,    SByte},
                                      {read_tries,    any},
                                      {write_tries,   any},
                                      {read_pkg_max,  any},
                                      {write_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (recv_and_validate 1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_and_validate),
                           ok
                   end},

         #{desc => "await continue (send_and_validate 2)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_and_validate)
                   end},
         #{desc => "send (2)",
           cmd  => fun(#{sock       := Sock,
                         send       := Send,
                         write_pkg  := SPkg,
                         write_byte := SByte} = State) ->
                           Data = ?DATA,
                           case Send(Sock, Data) of
                               ok ->
                                   ?SEV_IPRINT("sent ~p bytes", [size(Data)]),
                                   {ok, State#{write_pkg  => SPkg + 1,
                                               write_byte => SByte + size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (send 2)",
           cmd  => fun(#{sock      := Sock,
                         read_pkg  := RPkg,
                         read_byte := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,      RPkg},
                                      {read_byte,     RByte},
                                      {write_pkg,     SPkg},
                                      {write_byte,    SByte},
                                      {read_tries,    any},
                                      {write_tries,   any},
                                      {read_pkg_max,  any},
                                      {write_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (send_and_validate 2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_and_validate),
                           ok
                   end},

         #{desc => "await continue (recv_and_validate 2)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv_and_validate)
                   end},
         #{desc => "recv (2)",
           cmd  => fun(#{sock      := Sock,
                         recv      := Recv,
                         read_pkg  := RPkg,
                         read_byte := RByte} = State) ->
                           case Recv(Sock) of
                               {ok, Data} ->
                                   ?SEV_IPRINT("recv ~p bytes", [size(Data)]),
                                   {ok, State#{read_pkg  => RPkg + 1,
                                               read_byte => RByte + size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (recv 2)",
           cmd  => fun(#{sock      := Sock,
                         read_pkg  := RPkg,
                         read_byte := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,      RPkg},
                                      {read_byte,     RByte},
                                      {write_pkg,     SPkg},
                                      {write_byte,    SByte},
                                      {read_tries,    any},
                                      {write_tries,   any},
                                      {read_pkg_max,  any},
                                      {write_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (recv_and_validate 2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_and_validate),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close connection socket",
           cmd  => fun(#{domain   := local,
                         sock     := Sock,
                         local_sa := #{path := Path}} = State) ->
                           ok = socket:close(Sock),
                           State1 =
                               unlink_path(Path,
                                           fun() ->
                                                   maps:remove(local_sa, State)
                                           end,
                                           fun() -> State end),
                           {ok, maps:remove(sock, State1)};
                      (#{sock := Sock} = State) ->
                           socket:close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the server
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{domain := local,
                         server := Pid} = State) ->
                           {ok, Path} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{path => Path}};
                      (#{server := Pid} = State) ->
                           {ok, Port} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{port => Port}}
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{domain := local,
                         client := Pid,
                         path   := Path} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Path),
                           ok;
                      (#{client := Pid,
                         port   := Port} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Port),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, init)
                   end},

         %% *** The actual test ***

         #{desc => "order server to continue (with accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, accept),
                           ok
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "order client to continue (with connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect),
                           ok
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, connect)
                   end},
         #{desc => "await server ready (accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept)
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "order server to continue (recv_and_validate 1)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, recv_and_validate),
                           ok
                   end},
         #{desc => "order client to continue (send_and_validate 1)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send_and_validate),
                           ok
                   end},
         #{desc => "await client ready (send_and_validate 1)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, send_and_validate)
                   end},
         #{desc => "await server ready (recv_and_validate 1)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv_and_validate)
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "order client to continue (recv_and_validate 1)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, recv_and_validate),
                           ok
                   end},
         #{desc => "order server to continue (send_and_validate 1)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, send_and_validate),
                           ok
                   end},
         #{desc => "await server ready (send_and_validate 1)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, send_and_validate)
                   end},
         #{desc => "await client ready (recv_and_validate 1)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, recv_and_validate)
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "order server to continue (recv_and_validate 2)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, recv_and_validate),
                           ok
                   end},
         #{desc => "order client to continue (send_and_validate 2)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send_and_validate),
                           ok
                   end},
         #{desc => "await client ready (send_and_validate 2)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, send_and_validate)
                   end},
         #{desc => "await server ready (recv_and_validate 2)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv_and_validate)
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "order client to continue (recv_and_validate 2)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, recv_and_validate),
                           ok
                   end},
         #{desc => "order server to continue (send_and_validate 2)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, send_and_validate),
                           ok
                   end},
         #{desc => "await server ready (send_and_validate 2)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, send_and_validate)
                   end},
         #{desc => "await client ready (recv_and_validate 2)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, recv_and_validate)
                   end},

         %% *** Termination ***
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           State1 = maps:remove(client, State),
                           {ok, State1}
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Server} = State) ->
                           ?SEV_AWAIT_TERMINATION(Server),
                           State1 = maps:remove(server, State),
                           {ok, State1}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start server evaluator"),
    ServerInitState = InitState#{host => local_host()},
    Server = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator(s)"),
    ClientInitState = InitState#{host => local_host()},
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{server => Server#ev.pid,
                        client => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).


format_counters(Counters) ->
    format_counters(traffic, Counters).

format_counters(Type, Counters) when (Type =:= listen) orelse (Type =:= traffic) ->
    format_counters("   ", Type, Counters).

format_counters(Prefix, traffic, Counters) ->
    ReadByte    = maps:get(read_byte,     Counters, -1),
    ReadFails   = maps:get(read_fails,    Counters, -1),
    ReadPkg     = maps:get(read_pkg,      Counters, -1),
    ReadPkgMax  = maps:get(read_pkg_max,  Counters, -1),
    ReadTries   = maps:get(read_tries,    Counters, -1),
    ReadWaits   = maps:get(read_waits,    Counters, -1),
    WriteByte   = maps:get(write_byte,    Counters, -1),
    WriteFails  = maps:get(write_fails,   Counters, -1),
    WritePkg    = maps:get(write_pkg,     Counters, -1),
    WritePkgMax = maps:get(write_pkg_max, Counters, -1),
    WriteTries  = maps:get(write_tries,   Counters, -1),
    WriteWaits  = maps:get(write_waits,   Counters, -1),
    ?F("~n~sNumber Of Read Bytes:     ~p"
       "~n~sNumber Of Read Fails:     ~p"
       "~n~sNumber Of Read Packages:  ~p"
       "~n~sNumber Of Read Tries:     ~p"
       "~n~sNumber Of Read Waits:     ~p"
       "~n~sMax Read Package Size:    ~p"
       "~n~sNumber Of Write Bytes:    ~p"
       "~n~sNumber Of Write Fails:    ~p"
       "~n~sNumber Of Write Packages: ~p"
       "~n~sNumber Of Write Tries:    ~p"
       "~n~sNumber Of Write Waits:    ~p"
       "~n~sMax Write Package Size:   ~p",
       [Prefix, ReadByte,
        Prefix, ReadFails,
        Prefix, ReadPkg,
        Prefix, ReadTries,
        Prefix, ReadWaits,
        Prefix, ReadPkgMax,
        Prefix, WriteByte,
        Prefix, WriteFails,
        Prefix, WritePkg,
        Prefix, WriteTries,
        Prefix, WriteWaits,
        Prefix, WritePkgMax]);

format_counters(Prefix, listen, Counters) ->
    AccSuccess = maps:get(acc_success, Counters, -1),
    AccFails   = maps:get(acc_fails,   Counters, -1),
    AccTries   = maps:get(acc_tries,   Counters, -1),
    AccWaits   = maps:get(acc_waits,   Counters, -1),
    ?F("~n~sNumber Of Successful Accepts: ~p"
       "~n~sNumber Of Failed Accepts:     ~p"
       "~n~sNumber Of Accept Attempts:    ~p"
       "~n~sNumber Of Accept Waits:       ~p",
       [Prefix, AccSuccess,
        Prefix, AccFails,
        Prefix, AccTries,
        Prefix, AccWaits]).

all_counters() ->
    [
     read_byte,
     read_fails,
     read_pkg,
     read_pkg_max,
     read_tries,
     read_waits,
     write_byte,
     write_fails,
     write_pkg,
     write_pkg_max,
     write_tries,
     write_waits,
     acc_success,
     acc_fails,
     acc_tries,
     acc_waits
    ].

zero_counters() ->
    [{Cnt, 0} || Cnt <- all_counters()].

any_counters() ->
    [{Cnt, any} || Cnt <- all_counters()].


%% This function ensures that we have a list of "validate counters"
%% that have an entry for each existing counter.

ensure_counters(Counters) ->
    ensure_counters(any_counters(), Counters, []).

ensure_counters([], [], Acc) ->
    lists:reverse(Acc);
ensure_counters([{Cnt, Val}|DefCounters], Counters, Acc) ->
    case lists:keysearch(Cnt, 1, Counters) of
        {value, {Cnt, _} = T} ->
            Counters2 = lists:keydelete(Cnt, 1, Counters),
            ensure_counters(DefCounters, Counters2, [T|Acc]);
        false ->
            ensure_counters(DefCounters, Counters, [{Cnt, Val}|Acc])
    end.

traffic_sar_counters_validation(Counters) ->
    %% ?SEV_IPRINT("traffic_sar_counters_validation -> entry with"
    %%             "~n   Counters: ~p", [Counters]),
    traffic_sar_counters_validation2(maps:to_list(Counters),
                                     zero_counters()).

traffic_sar_counters_validation(Counters, ValidateCounters) ->
    %% ?SEV_IPRINT("traffic_sar_counters_validation -> entry with"
    %%             "~n   Counters:          ~p"
    %%             "~n   Validate Counters: ~p", [Counters, ValidateCounters]),
    traffic_sar_counters_validation2(maps:to_list(Counters),
                                     ensure_counters(ValidateCounters)).

traffic_sar_counters_validation2(Counters, []) ->
    %% ?SEV_IPRINT("traffic_sar_counters_validation2 -> Remaining Counters: "
    %%             "~n   ~p", [Counters]),
    (catch lists:foreach(
             fun({_Cnt, 0})   -> ok;
                ({Cnt,  Val}) ->
                     throw({error, {invalid_counter, Cnt, Val}})
             end,
             Counters));
traffic_sar_counters_validation2(Counters, [{Cnt, Val}|ValidateCounters]) ->
    %% ?SEV_IPRINT("traffic_sar_counters_validation2 -> try validate ~w when"
    %%             "~n   Counters:         ~p"
    %%             "~n   ValidateCounters: ~p", [Cnt, Counters, ValidateCounters]),
    case lists:keysearch(Cnt, 1, Counters) of
        {value, {Cnt, Val}} ->
            %% ?SEV_IPRINT("traffic_sar_counters_validation2 -> ~w validated", [Cnt]),
            Counters2 = lists:keydelete(Cnt, 1, Counters),
            traffic_sar_counters_validation2(Counters2, ValidateCounters);
        {value, {Cnt, _Val}} when (Val =:= any) ->
            %% ?SEV_IPRINT("traffic_sar_counters_validation2 -> "
            %%             "~w validated (any) when"
            %%             "~n   Counters: ~p", [Cnt, Counters]),
            Counters2 = lists:keydelete(Cnt, 1, Counters),
            traffic_sar_counters_validation2(Counters2, ValidateCounters);
        {value, {Cnt, InvVal}} ->
            ?SEV_EPRINT("traffic_sar_counters_validation2 -> "
                        "~w validation failed: "
                        "~n   Expected Value: ~p"
                        "~n   Actual Value:   ~p", [Cnt, Val, InvVal]),
            {error, {invalid_counter, Cnt, InvVal, Val}};
        false ->
            ?SEV_EPRINT("traffic_sar_counters_validation2 -> "
                        "~w validation failed: Unknown", [Cnt]),
            {error, {unknown_counter, Cnt, Counters}}
    end.

                          

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use UDP on IPv4.

traffic_sendto_and_recvfrom_counters_udp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(traffic_sendto_and_recvfrom_counters_udp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => udp,
                                 recv   => fun(S) ->
                                                   socket:recvfrom(S)
                                           end,
                                 send   => fun(S, Data, Dest) ->
                                                   socket:sendto(S, Data, Dest)
                                           end},
                   ok = traffic_send_and_recv_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use UDP on IPv6.

traffic_sendto_and_recvfrom_counters_udp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(traffic_sendto_and_recvfrom_counters_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => udp,
                                 recv   => fun(S) ->
                                                   socket:recvfrom(S)
                                           end,
                                 send   => fun(S, Data, Dest) ->
                                                   socket:sendto(S, Data, Dest)
                                           end},
                   ok = traffic_send_and_recv_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use default (UDP) on local.

traffic_sendto_and_recvfrom_counters_udpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(traffic_sendto_and_recvfrom_counters_udp4,
           fun() ->
		   has_support_unix_domain_socket(),
		   is_not_windows()
	   end,
           fun() ->
                   InitState = #{domain => local,
                                 proto  => default,
                                 recv   => fun(S) ->
                                                   socket:recvfrom(S)
                                           end,
                                 send   => fun(S, Data, Dest) ->
                                                   socket:sendto(S, Data, Dest)
                                           end},
                   ok = traffic_send_and_recv_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use UDP on IPv4.

traffic_sendmsg_and_recvmsg_counters_udp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(traffic_sendmsg_and_recvmsg_counters_udp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => udp,
                                 recv   => fun(S) ->
                                                   case socket:recvmsg(S) of
                                                       {ok, #{addr  := Source,
                                                              iov   := [Data]}} ->
                                                           {ok, {Source, Data}};
                                                       {error, _} = ERROR ->
                                                           ERROR
                                                   end
                                           end,
                                 send   => fun(S, Data, Dest) ->
                                                   Msg = #{addr => Dest,
                                                              iov  => [Data]},
                                                   socket:sendmsg(S, Msg)
                                           end},
                   ok = traffic_send_and_recv_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use UDP on IPv6.

traffic_sendmsg_and_recvmsg_counters_udp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(traffic_sendmsg_and_recvmsg_counters_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => udp,
                                 recv   => fun(S) ->
                                                   case socket:recvmsg(S) of
                                                       {ok, #{addr  := Source,
                                                              iov   := [Data]}} ->
                                                           {ok, {Source, Data}};
                                                       {error, _} = ERROR ->
                                                           ERROR
                                                   end
                                           end,
                                 send   => fun(S, Data, Dest) ->
                                                   Msg = #{addr => Dest,
                                                              iov  => [Data]},
                                                   socket:sendmsg(S, Msg)
                                           end},
                   ok = traffic_send_and_recv_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test the counters
%% for both read and write.
%% So that its easy to extend, we use fun's for read and write.
%% We use default (UDP) on local.

traffic_sendmsg_and_recvmsg_counters_udpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(traffic_sendmsg_and_recvmsg_counters_udpL,
           fun() ->
		   has_support_unix_domain_socket(),
		   is_not_windows()
	   end,
           fun() ->
                   InitState = #{domain => local,
                                 proto  => default,
                                 recv   => fun(S) ->
                                                   case socket:recvmsg(S) of
                                                       {ok, #{addr  := Source,
                                                              iov   := [Data]}} ->
                                                           {ok, {Source, Data}};
                                                       {error, _} = ERROR ->
                                                           ERROR
                                                   end
                                           end,
                                 send   => fun(S, Data, Dest) ->
                                                   Msg = #{addr => Dest,
                                                              iov  => [Data]},
                                                   socket:sendmsg(S, Msg)
                                           end},
                   ok = traffic_send_and_recv_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

traffic_send_and_recv_udp(InitState) ->
    ServerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain := Domain, proto := Proto} = State) ->
                           case socket:open(Domain, dgram, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{domain   := local,
                         sock     := Sock,
                         local_sa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               ok ->
                                   ok; % We do not care about the port for local
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{sock     := LSock,
                         local_sa := LSA} = State) ->
                           case sock_bind(LSock, LSA) of
                               ok ->
                                   Port = sock_port(LSock),
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "initial counter validation (=zero)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("Validate initial counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(Counters)
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{domain   := local,
                         tester   := Tester,
                         local_sa := #{path := Path}}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Path),
                           ok;
                      (#{tester   := Tester,
                         lport    := Port}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (recv_and_validate 1)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv_and_validate)
                   end},
         #{desc => "recv (1)",
           cmd  => fun(#{sock := Sock,
                         recv := Recv} = State) ->
                           case Recv(Sock) of
                               {ok, {ClientSA, Data}} ->
                                   ?SEV_IPRINT("recv ~p bytes", [size(Data)]),
                                   {ok, State#{client_sa => ClientSA,
                                               read_pkg  => 1,
                                               read_byte => size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (recv 1)",
           cmd  => fun(#{sock      := Sock,
                         read_pkg  := Pkg,
                         read_byte := Byte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,   Pkg},
                                      {read_byte,  Byte},
                                      {read_tries, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (recv_and_validate 1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_and_validate),
                           ok
                   end},

         #{desc => "await continue (send_and_validate 1)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_and_validate)
                   end},
         #{desc => "send (1)",
           cmd  => fun(#{sock      := Sock,
                         send      := Send,
                         client_sa := ClientSA} = State) ->
                           Data = ?DATA,
                           case Send(Sock, Data, ClientSA) of
                               ok ->
                                   ?SEV_IPRINT("sent ~p bytes", [size(Data)]),
                                   {ok, State#{write_pkg  => 1,
                                               write_byte => size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (send 1)",
           cmd  => fun(#{sock       := Sock,
                         read_pkg   := RPkg,
                         read_byte  := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,    RPkg},
                                      {read_byte,   RByte},
                                      {write_pkg,   SPkg},
                                      {write_byte,  SByte},
                                      {read_tries,  any},
                                      {write_tries, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (send_and_validate 1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_and_validate),
                           ok
                   end},

         #{desc => "await continue (recv_and_validate 2)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv_and_validate)
                   end},
         #{desc => "recv (2)",
           cmd  => fun(#{sock      := Sock,
                         recv      := Recv,
                         read_pkg  := Pkg,
                         read_byte := Byte} = State) ->
                           case Recv(Sock) of
                               {ok, {Source, Data}} ->
                                   ?SEV_IPRINT("recv ~p bytes", [size(Data)]),
                                   {ok, State#{client_sa => Source,
                                               read_pkg  => Pkg + 1,
                                               read_byte => Byte + size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (recv 2)",
           cmd  => fun(#{sock       := Sock,
                         read_pkg   := RPkg,
                         read_byte  := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,    RPkg},
                                      {read_byte,   RByte},
                                      {write_pkg,   SPkg},
                                      {write_byte,  SByte},
                                      {read_tries,  any},
                                      {write_tries, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (recv_and_validate 2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_and_validate),
                           ok
                   end},

         #{desc => "await continue (send_and_validate 2)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_and_validate)
                   end},
         #{desc => "send (2)",
           cmd  => fun(#{sock       := Sock,
                         client_sa  := ClientSA,
                         send       := Send,
                         write_pkg  := Pkg,
                         write_byte := Byte} = State) ->
                           Data = ?DATA,
                           case Send(Sock, Data, ClientSA) of
                               ok ->
                                   ?SEV_IPRINT("sent ~p bytes", [size(Data)]),
                                   {ok, State#{write_pkg  => Pkg + 1,
                                               write_byte => Byte + size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (send 2)",
           cmd  => fun(#{sock       := Sock,
                         read_pkg   := RPkg,
                         read_byte  := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,    RPkg},
                                      {read_byte,   RByte},
                                      {write_pkg,   SPkg},
                                      {write_byte,  SByte},
                                      {read_tries,  any},
                                      {write_tries, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (send_and_validate 2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_and_validate),
                           ok
                   end},


         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket (just in case)",
           cmd  => fun(#{domain   := local,
                         sock     := Sock,
                         local_sa := #{path := Path}} = State) ->
                           ok = socket:close(Sock),
                           State1 =
                               unlink_path(Path,
                                           fun() ->
                                                   maps:remove(local_sa, State)
                                           end,
                                           fun() -> State end),
                           {ok, maps:remove(lsock, State1)};
                      (#{sock := Sock} = State) ->
                           (catch socket:close(Sock)),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(#{domain := local} = State) ->
                           {Tester, Path} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, server_path => Path}};
                      (State) ->
                           {Tester, Port} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, server_port => Port}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which server (local) address",
           cmd  => fun(#{domain      := local = Domain,
                         server_path := Path} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           SSA = #{family => Domain, path => Path},
                           {ok, State#{local_sa => LSA, server_sa => SSA}};
                      (#{domain := Domain, server_port := Port} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           SSA = LSA#{port => Port},
                           {ok, State#{local_sa => LSA, server_sa => SSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain := Domain,
                         proto  := Proto} = State) ->
                           case socket:open(Domain, dgram, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, local_sa := LSA} = _State) ->
                           case sock_bind(Sock, LSA) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "initial counter validation (=zero)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("Validate initial counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(Counters)
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (send_and_validate 1)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_and_validate)
                   end},
         #{desc => "send (1)",
           cmd  => fun(#{sock      := Sock,
                         send      := Send,
                         server_sa := ServerSA} = State) ->
                           Data = ?DATA,
                           case Send(Sock, Data, ServerSA) of
                               ok ->
                                   ?SEV_IPRINT("sent ~p bytes", [size(Data)]),
                                   {ok, State#{write_pkg  => 1,
                                               write_byte => size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (send 1)",
           cmd  => fun(#{sock       := Sock,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{write_pkg,     SPkg},
                                      {write_byte,    SByte},
                                      {write_tries,   any},
                                      {write_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (send_and_validate 1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_and_validate),
                           ok
                   end},

         #{desc => "await continue (recv_and_validate 1)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv_and_validate)
                   end},
         #{desc => "recv (1)",
           cmd  => fun(#{sock      := Sock,
                         recv      := Recv,
                         server_sa := #{family := local} = ServerSA} = State) ->
                           case Recv(Sock) of
                               {ok, {ServerSA, Data}} ->
                                   ?SEV_IPRINT("recv ~p bytes", [size(Data)]),
                                   {ok, State#{read_pkg  => 1,
                                               read_byte => size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{sock      := Sock,
                         recv      := Recv,
                         server_sa := #{addr := Addr, port := Port}} = State) ->
                           case Recv(Sock) of
                               {ok, {#{addr := Addr, port := Port}, Data}} ->
                                   ?SEV_IPRINT("recv ~p bytes", [size(Data)]),
                                   {ok, State#{read_pkg  => 1,
                                               read_byte => size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (recv 1)",
           cmd  => fun(#{sock      := Sock,
                         read_pkg  := RPkg,
                         read_byte := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,      RPkg},
                                      {read_byte,     RByte},
                                      {write_pkg,     SPkg},
                                      {write_byte,    SByte},
                                      {read_tries,    any},
                                      {write_tries,   any},
                                      {read_pkg_max,  any},
                                      {write_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (recv_and_validate 1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_and_validate),
                           ok
                   end},

         #{desc => "await continue (send_and_validate 2)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_and_validate)
                   end},
         #{desc => "send (2)",
           cmd  => fun(#{sock       := Sock,
                         send       := Send,
                         server_sa  := ServerSA,
                         write_pkg  := SPkg,
                         write_byte := SByte} = State) ->
                           Data = ?DATA,
                           case Send(Sock, Data, ServerSA) of
                               ok ->
                                   ?SEV_IPRINT("sent ~p bytes", [size(Data)]),
                                   {ok, State#{write_pkg  => SPkg + 1,
                                               write_byte => SByte + size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (send 2)",
           cmd  => fun(#{sock       := Sock,
                         read_pkg   := RPkg,
                         read_byte  := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,      RPkg},
                                      {read_byte,     RByte},
                                      {write_pkg,     SPkg},
                                      {write_byte,    SByte},
                                      {read_tries,    any},
                                      {write_tries,   any},
                                      {read_pkg_max,  any},
                                      {write_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (send_and_validate 2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_and_validate),
                           ok
                   end},

         #{desc => "await continue (recv_and_validate 2)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv_and_validate)
                   end},
         #{desc => "recv (2)",
           cmd  => fun(#{sock      := Sock,
                         server_sa := #{family := local} = ServerSA,
                         recv      := Recv,
                         read_pkg  := RPkg,
                         read_byte := RByte} = State) ->
                           case Recv(Sock) of
                               {ok, {ServerSA, Data}} ->
                                   ?SEV_IPRINT("recv ~p bytes", [size(Data)]),
                                   {ok, State#{read_pkg  => RPkg + 1,
                                               read_byte => RByte + size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end;
		      (#{sock      := Sock,
                         server_sa := #{addr := Addr, port := Port},
                         recv      := Recv,
                         read_pkg  := RPkg,
                         read_byte := RByte} = State) ->
                           case Recv(Sock) of
                               {ok, {#{addr := Addr, port := Port}, Data}} ->
                                   ?SEV_IPRINT("recv ~p bytes", [size(Data)]),
                                   {ok, State#{read_pkg  => RPkg + 1,
                                               read_byte => RByte + size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate (recv 2)",
           cmd  => fun(#{sock      := Sock,
                         read_pkg  := RPkg,
                         read_byte := RByte,
                         write_pkg  := SPkg,
                         write_byte := SByte} = _State) ->
                           try socket:info(Sock) of
                               #{counters := Counters} ->
                                   ?SEV_IPRINT("validate counters: "
                                               "~s", [format_counters(Counters)]),
                                   traffic_sar_counters_validation(
                                     Counters,
                                     [{read_pkg,      RPkg},
                                      {read_byte,     RByte},
                                      {write_pkg,     SPkg},
                                      {write_byte,    SByte},
                                      {read_tries,    any},
                                      {write_tries,   any},
                                      {read_pkg_max,  any},
                                      {write_pkg_max, any}])
                           catch
                               C:E:S ->
                                   ?SEV_EPRINT("Failed get socket info: "
                                               "~n   Class: ~p"
                                               "~n   Error: ~p"
                                               "~n   Stack: ~p", [C, E, S]),
                                   {error, {socket_info_failed, {C, E, S}}}
                           end
                   end},
         #{desc => "announce ready (recv_and_validate 2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_and_validate),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close connection socket",
           cmd  => fun(#{domain   := local,
                         sock     := Sock,
                         local_sa := #{path := Path}} = State) ->
                           ok = socket:close(Sock),
                           State1 =
                               unlink_path(Path,
                                           fun() ->
                                                   maps:remove(local_sa, State)
                                           end,
                                           fun() -> State end),
                           {ok, maps:remove(sock, State1)};
                      (#{sock := Sock} = State) ->
                           socket:close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the server
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{domain := local,
                         server := Pid} = State) ->
                           {ok, Path} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{path => Path}};
                      (#{server := Pid} = State) ->
                           {ok, Port} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{port => Port}}
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{domain := local,
                         client := Pid,
                         path   := Path} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Path),
                           ok;
                      (#{client := Pid,
                         port   := Port} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Port),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, init)
                   end},

         %% *** The actual test ***

         #{desc => "order server to continue (recv_and_validate 1)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, recv_and_validate),
                           ok
                   end},
         #{desc => "order client to continue (send_and_validate 1)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send_and_validate),
                           ok
                   end},
         #{desc => "await client ready (send_and_validate 1)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, send_and_validate)
                   end},
         #{desc => "await server ready (recv_and_validate 1)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv_and_validate)
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "order client to continue (recv_and_validate 1)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, recv_and_validate),
                           ok
                   end},
         #{desc => "order server to continue (send_and_validate 1)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, send_and_validate),
                           ok
                   end},
         #{desc => "await server ready (send_and_validate 1)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, send_and_validate)
                   end},
         #{desc => "await client ready (recv_and_validate 1)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, recv_and_validate)
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "order server to continue (recv_and_validate 2)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, recv_and_validate),
                           ok
                   end},
         #{desc => "order client to continue (send_and_validate 2)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send_and_validate),
                           ok
                   end},
         #{desc => "await client ready (send_and_validate 2)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, send_and_validate)
                   end},
         #{desc => "await server ready (recv_and_validate 2)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv_and_validate)
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "order client to continue (recv_and_validate 2)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, recv_and_validate),
                           ok
                   end},
         #{desc => "order server to continue (send_and_validate 2)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, send_and_validate),
                           ok
                   end},
         #{desc => "await server ready (send_and_validate 2)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, send_and_validate)
                   end},
         #{desc => "await client ready (recv_and_validate 2)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, recv_and_validate)
                   end},

         %% *** Termination ***
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           State1 = maps:remove(client, State),
                           {ok, State1}
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Server} = State) ->
                           ?SEV_AWAIT_TERMINATION(Server),
                           State1 = maps:remove(server, State),
                           {ok, State1}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start server evaluator"),
    ServerInitState = InitState#{host => local_host()},
    Server = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator(s)"),
    ClientInitState = InitState#{host => local_host()},
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{server => Server#ev.pid,
                        client => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% behave as expected when sending and/or reading chunks.
%% First send data in one "big" chunk, and read it in "small" chunks.
%% Second, send in a bunch of "small" chunks, and read in one "big" chunk.
%% Protocol is tcp and Domain is IPv4.

traffic_send_and_recv_chunks_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => tcp},
                   ok = traffic_send_and_recv_chunks_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% behave as expected when sending and/or reading chunks.
%% First send data in one "big" chunk, and read it in "small" chunks.
%% Second, send in a bunch of "small" chunks, and read in one "big" chunk.
%% Protocol is tcp and Domain is IPv6.

traffic_send_and_recv_chunks_tcp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => tcp},
                   ok = traffic_send_and_recv_chunks_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% behave as expected when sending and/or reading chunks.
%% First send data in one "big" chunk, and read it in "small" chunks.
%% Second, send in a bunch of "small" chunks, and read in one "big" chunk.
%% Protocol is 'default' and Domain is UNix Domain (Stream).

traffic_send_and_recv_chunks_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   InitState = #{domain => local,
                                 proto  => default},
                   ok = traffic_send_and_recv_chunks_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% behave as expected when sending and/or reading chunks.
%% First send data in one "big" chunk, and read it in "small" chunks.
%% Second, send in a bunch of "small" chunks, and read in one "big" chunk.
%% Protocol is SCTP and Domain is IPv4.

traffic_send_and_recv_chunks_sctp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   has_support_ipv4(),
                   has_support_sctp()
           end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => sctp},
                   ok = traffic_send_and_recv_chunks_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% behave as expected when sending and/or reading chunks.
%% First send data in one "big" chunk, and read it in "small" chunks.
%% Second, send in a bunch of "small" chunks, and read in one "big" chunk.
%% Protocol is SCTP and Domain is IPv6.

traffic_send_and_recv_chunks_sctp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   has_support_ipv6(),
                   has_support_sctp()
           end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => sctp},
                   ok = traffic_send_and_recv_chunks_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

traffic_send_and_recv_chunks_stream(InitState) ->
    ServerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
			   put(sname, server),
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "create listen socket",
           cmd  => fun(#{domain := Domain, proto := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, eprotonosupport = Reason} ->
                                   {skip, Reason};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{domain   := local,
                         lsock    := LSock,
                         local_sa := LSA} = _State) ->
                           case socket:bind(LSock, LSA) of
                               ok ->
                                   ok; % We do not care about the port for local
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{lsock    := LSock,
                         local_sa := LSA} = State) ->
                           case sock_bind(LSock, LSA) of
                               ok ->
                                   Port = sock_port(LSock),
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{domain   := local,
                         tester   := Tester,
                         local_sa := LSA}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, LSA),
                           ok;
                      (#{tester   := Tester,
                         local_sa := LSA,
                         lport    := Port}) ->
                           ServerSA = LSA#{port => Port},
                           ?SEV_ANNOUNCE_READY(Tester, init, ServerSA),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   {ok, State#{csock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         #{desc => "await continue (recv-many-small)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv_many_small)
                   end},
         #{desc => "recv chunk 1",
           cmd  => fun(#{csock := Sock} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} when byte_size(Chunk) =:= 100 ->
                                   ?SEV_IPRINT(
				      "recv chunk 1 (~p bytes)",
				      [byte_size(Chunk)]),
                                   {ok, State#{chunks => [b2l(Chunk)]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 2",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} when byte_size(Chunk) =:= 100 ->
                                   ?SEV_IPRINT(
				      "recv chunk 2 (~p bytes):"
				      "~n   (Acced) Chunks: ~p bytes",
				      [byte_size(Chunk),
				       lists:flatlength(Chunks)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 3",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} when byte_size(Chunk) =:= 100 ->
                                   ?SEV_IPRINT(
				      "recv chunk 3 (~p bytes):"
				      "~n   (Acced) Chunks: ~p bytes",
				      [byte_size(Chunk),
				       lists:flatlength(Chunks)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 4",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} when byte_size(Chunk) =:= 100 ->
                                   ?SEV_IPRINT(
				      "recv chunk 4 (~p bytes):"
				      "~n   (Acced) Chunks: ~p bytes",
				      [byte_size(Chunk),
				       lists:flatlength(Chunks)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 5",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} when byte_size(Chunk) =:= 100 ->
                                   ?SEV_IPRINT(
				      "recv chunk 5 (~p bytes):"
				      "~n   (Acced) Chunks: ~p bytes",
				      [byte_size(Chunk),
				       lists:flatlength(Chunks)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 6",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} when byte_size(Chunk) =:= 100 ->
                                   ?SEV_IPRINT(
				      "recv chunk 6 (~p bytes):"
				      "~n   (Acced) Chunks: ~p bytes",
				      [byte_size(Chunk),
				       lists:flatlength(Chunks)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 7",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} when byte_size(Chunk) =:= 100 ->
                                   ?SEV_IPRINT(
				      "recv chunk 7 (~p bytes):"
				      "~n   (Acced) Chunks: ~p bytes",
				      [byte_size(Chunk),
				       lists:flatlength(Chunks)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 8",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} when byte_size(Chunk) =:= 100 ->
                                   ?SEV_IPRINT(
				      "recv chunk 8 (~p bytes):"
				      "~n   (Acced) Chunks: ~p bytes",
				      [byte_size(Chunk),
				       lists:flatlength(Chunks)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 9",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} when byte_size(Chunk) =:= 100 ->
                                   ?SEV_IPRINT(
				      "recv chunk 9 (~p bytes):"
				      "~n   (Acced) Chunks: ~p bytes",
				      [byte_size(Chunk),
				       lists:flatlength(Chunks)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 10",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} when byte_size(Chunk) =:= 100 ->
                                   ?SEV_IPRINT(
				      "recv (final) chunk 10 (~p bytes):"
				      "~n   (Acced) Chunks: ~p",
				      [byte_size(Chunk),
				       lists:flatlength(Chunks)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv-many-small)",
           cmd  => fun(#{tester := Tester,
                         chunks := Chunks} = State) ->
                           Data = lists:flatten(lists:reverse(Chunks)),
                           ?SEV_ANNOUNCE_READY(Tester, recv_many_small, Data),
                           {ok, maps:remove(chunks, State)}
                   end},

         #{desc => "await continue (recv-one-big)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, recv_one_big) of
                               {ok, Size} ->
                                   {ok, State#{size => Size}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv (one big)",
           cmd  => fun(#{tester := Tester, csock := Sock, size := Size} = _State) ->
                           %% socket:setopt(Sock, otp, debug, true),
			   ?SEV_IPRINT("try recv ~w bytes", [Size]),
                           case socket:recv(Sock, Size) of
                               {ok, Data} ->
				   ?SEV_IPRINT("recv ~w bytes",
					       [byte_size(Data)]),
                                   ?SEV_ANNOUNCE_READY(Tester,
                                                       recv_one_big,
                                                       b2l(Data)),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close connection socket (just in case)",
           cmd  => fun(#{csock := Sock} = State) ->
                           (catch socket:close(Sock)),
                           {ok, maps:remove(csock, State)}
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{domain   := local,
                         lsock    := Sock,
                         local_sa := #{path := Path}} = State) ->
                           ok = socket:close(Sock),
                           State1 =
                               unlink_path(Path,
                                           fun() ->
                                                   maps:remove(local_sa, State)
                                           end,
                                           fun() -> State end),
                           {ok, maps:remove(lsock, State1)};
                      (#{lsock := Sock} = State) ->
                           (catch socket:close(Sock)),
                           {ok, maps:remove(lsock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
			   put(sname, client),
                           {Tester, ServerSA} = ?SEV_AWAIT_START(),
                           {ok, State#{tester    => Tester,
                                       server_sa => ServerSA}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create node",
           cmd  => fun(State) ->
                           {Peer, Node} = start_node("client"),
                           {ok, State#{peer => Peer, node => Node}}
                   end},
         #{desc => "monitor client node",
           cmd  => fun(#{node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "start remote client",
           cmd  => fun(#{node := Node} = State) ->
                           Pid = traffic_snr_tcp_client_start(Node),
                           ?SEV_IPRINT("client ~p started", [Pid]),
                           {ok, State#{rclient => Pid}}
                   end},
         #{desc => "monitor remote client",
           cmd  => fun(#{rclient := Pid}) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order remote client to start",
           cmd  => fun(#{rclient   := Client,
                         server_sa := ServerSA,
                         proto     := Proto}) ->
                           ?SEV_ANNOUNCE_START(Client, {ServerSA, Proto}),
                           ok
                   end},
         #{desc => "await remote client ready",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, init,
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect,
                                               [{rclient, Client}]),
                           ok
                   end},
         #{desc => "order remote client to continue (connect)",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect),
                           ok
                   end},
         #{desc => "await client process ready (connect)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, connect,
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},

         #{desc => "await continue (send-one-big)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester,
                                                    send_one_big,
                                                    [{rclient, Client}]) of
                               {ok, Data} ->
                                   {ok, State#{data => Data}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send)",
           cmd  => fun(#{rclient := Client, data := Data}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Data),
                           ok
                   end},
         #{desc => "await client process ready (send)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send,
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (send-one-big)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_one_big),
                           ok
                   end},

         #{desc => "await continue (send-many-small)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester,
                                                    send_many_small,
                                                    [{rclient, Client}]) of
                               {ok, Data} ->
                                   {ok, State#{data => Data}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send chunk 1)",
           cmd  => fun(#{rclient := Client,
                         data    := Data} = State) ->
                           {Chunk, RestData} = lists:split(100, Data),
                           %% ?SEV_IPRINT("order send of chunk 1: "
                           %%             "~n   Size: ~p"
                           %%             "~n   ~p", [length(Chunk), Chunk]),
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Chunk),
                           {ok, State#{data => RestData}}
                   end},
         #{desc => "await client process ready (send chunk 1)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send chunk 2)",
           cmd  => fun(#{rclient := Client,
                         data    := Data} = State) ->
                           {Chunk, RestData} = lists:split(100, Data),
                           %% ?SEV_IPRINT("order send of chunk 2: "
                           %%             "~n   Size: ~p"
                           %%             "~n   ~p", [length(Chunk), Chunk]),
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Chunk),
                           {ok, State#{data => RestData}}
                   end},
         #{desc => "await client process ready (send chunk 2)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send chunk 3)",
           cmd  => fun(#{rclient := Client,
                         data    := Data} = State) ->
                           {Chunk, RestData} = lists:split(100, Data),
                           %% ?SEV_IPRINT("order send of chunk 3: "
                           %%             "~n   Size: ~p"
                           %%             "~n   ~p", [length(Chunk), Chunk]),
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Chunk),
                           {ok, State#{data => RestData}}
                   end},
         #{desc => "await client process ready (send chunk 3)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send chunk 4)",
           cmd  => fun(#{rclient := Client,
                         data    := Data} = State) ->
                           {Chunk, RestData} = lists:split(100, Data),
                           %% ?SEV_IPRINT("order send of chunk 4: "
                           %%             "~n   Size: ~p"
                           %%             "~n   ~p", [length(Chunk), Chunk]),
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Chunk),
                           {ok, State#{data => RestData}}
                   end},
         #{desc => "await client process ready (send chunk 4)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send chunk 5)",
           cmd  => fun(#{rclient := Client,
                         data    := Data} = State) ->
                           {Chunk, RestData} = lists:split(100, Data),
                           %% ?SEV_IPRINT("order send of chunk 5: "
                           %%             "~n   Size: ~p"
                           %%             "~n   ~p", [length(Chunk), Chunk]),
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Chunk),
                           {ok, State#{data => RestData}}
                   end},
         #{desc => "await client process ready (send chunk 5)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send chunk 6)",
           cmd  => fun(#{rclient := Client,
                         data    := Data} = State) ->
                           {Chunk, RestData} = lists:split(100, Data),
                           %% ?SEV_IPRINT("order send of chunk 6: "
                           %%             "~n   Size: ~p"
                           %%             "~n   ~p", [length(Chunk), Chunk]),
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Chunk),
                           {ok, State#{data => RestData}}
                   end},
         #{desc => "await client process ready (send chunk 6)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send chunk 7)",
           cmd  => fun(#{rclient := Client,
                         data    := Data} = State) ->
                           {Chunk, RestData} = lists:split(100, Data),
                           %% ?SEV_IPRINT("order send of chunk 7: "
                           %%             "~n   Size: ~p"
                           %%             "~n   ~p", [length(Chunk), Chunk]),
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Chunk),
                           {ok, State#{data => RestData}}
                   end},
         #{desc => "await client process ready (send chunk 7)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send chunk 8)",
           cmd  => fun(#{rclient := Client,
                         data    := Data} = State) ->
                           {Chunk, RestData} = lists:split(100, Data),
                           %% ?SEV_IPRINT("order send of chunk 8: "
                           %%             "~n   Size: ~p"
                           %%             "~n   ~p", [length(Chunk), Chunk]),
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Chunk),
                           {ok, State#{data => RestData}}
                   end},
         #{desc => "await client process ready (send chunk 8)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send chunk 9)",
           cmd  => fun(#{rclient := Client,
                         data    := Data} = State) ->
                           {Chunk, RestData} = lists:split(100, Data),
                           %% ?SEV_IPRINT("order send of chunk 9: "
                           %%             "~n   Size: ~p"
                           %%             "~n   ~p", [length(Chunk), Chunk]),
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Chunk),
                           {ok, State#{data => RestData}}
                   end},
         #{desc => "await client process ready (send chunk 9)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send chunk 10)",
           cmd  => fun(#{rclient := Client,
                         data    := Data} = State) ->
                           {Chunk, []} = lists:split(100, Data),
                           %% ?SEV_IPRINT("order send of chunk 10: "
                           %%             "~n   Size: ~p"
                           %%             "~n   ~p", [length(Chunk), Chunk]),
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Chunk),
                           {ok, maps:remove(data, State)}
                   end},
         #{desc => "await client process ready (send chunk 10)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (send stop)",
           cmd  => fun(#{rclient := Client} = State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, stop),
                           {ok, maps:remove(data, State)}
                   end},
         #{desc => "await client process ready (send stop)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (send-many-small)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_many_small),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester  := Tester, 
                         rclient := Client} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester,
                                                     [{rclient, Client}]) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "kill remote client",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await remote client termination",
           cmd  => fun(#{rclient := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           State1 = maps:remove(rclient, State),
                           {ok, State1}
                   end},
         #{desc => "stop client node",
           cmd  => fun(#{peer := Peer} = State) ->
                           {ok,
                            try peer:stop(Peer) of
                                ok ->
                                    State#{node_stop => ok};
                                {error, Reason} ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   ~p", [Reason]),
                                    State#{node_stop => error}
                            catch
                                C:E:S ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   Class: ~p"
                                                "~n   Error: ~p"
                                                "~n   Stack: ~p",[C, E, S]),
                                    State#{node_stop => error}
                            end}
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node, node_stop := ok} = State) ->
                           ?SEV_IPRINT("Success node stop - await nodedown"),
                           receive
                               {nodedown, Node} ->
                                   ?SEV_IPRINT("nodedown received - cleanup"),
                                   State1 = maps:remove(node, State),
                                   {ok, State1}
                           end;
                      (#{node_stop := error} = State) ->
                           ?SEV_IPRINT("Failed node stop - cleanup"),
                           State1 = maps:remove(node, State),
                           {ok, State1}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Pid} = _State) ->
			   put(sname, tester),
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the server
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Pid} = State) ->
                           {ok, ServerSA} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{server_sa => ServerSA}}
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client    := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, ServerSA),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, init)
                   end},
 
         %% The actual test
         #{desc => "order server continue (accept)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client continue (connect)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await server ready (accept)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept,
                                            [{client, Client}]),
                           ok
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, connect, 
                                            [{server, Server}])
                   end},

         #{desc => "generate data",
           cmd  => fun(State) ->
                           D1 = lists:seq(1,250),
                           D2 = lists:duplicate(4, D1),
                           D3 = lists:flatten(D2),
                           {ok, State#{data => D3}}
                   end},

         %% (client) Send one big and (server) recv may small
         #{desc => "order server continue (recv-many-small)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv_many_small),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client continue (send-one-big)",
           cmd  => fun(#{client := Pid, data := Data} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, send_one_big, Data),
                           ok
                   end},
         #{desc => "await client ready (send-one-big)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ok = ?SEV_AWAIT_READY(Client, client, send_one_big, 
                                                 [{server, Server}])
                   end},
         #{desc => "await server ready (recv-many-small)",
           cmd  => fun(#{server := Server,
                         client := Client, 
                         data   := Data} = _State) ->
                           case ?SEV_AWAIT_READY(Server, server, recv_many_small,
                                                 [{client, Client}]) of
                               {ok, Data} ->
                                   ok;
                               {ok, OtherData} ->
                                   {error, {mismatched_data, Data, OtherData}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order server continue (recv-one-big)",
           cmd  => fun(#{server := Pid, data := Data} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv_one_big, length(Data)),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client continue (send-many-small)",
           cmd  => fun(#{client := Pid, data := Data} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, send_many_small, Data),
                           ok
                   end},
         #{desc => "await client ready (send-many-small)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ok = ?SEV_AWAIT_READY(Client, client, send_many_small, 
                                                 [{server, Server}])
                   end},
         #{desc => "await server ready (recv-one-big)",
           cmd  => fun(#{server := Server,
                         client := Client, 
                         data   := Data} = State) ->
                           case ?SEV_AWAIT_READY(Server, server, recv_one_big,
                                                 [{client, Client}]) of
                               {ok, Data} ->
                                   {ok, maps:remove(data, State)};
                               {ok, OtherData} ->
                                   {error, {mismatched_data, Data, OtherData}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% Terminations
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(server, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start server evaluator"),
    ServerInitState = InitState,
    Server = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator(s)"),
    ClientInitState = InitState#{host => local_host()},
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{server => Server#ev.pid,
                        client => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).
    


traffic_snr_tcp_client_start(Node) ->
    Self = self(),
    Fun  = fun() -> traffic_snr_tcp_client(Self) end,
    erlang:spawn(Node, Fun).

traffic_snr_tcp_client(Parent) ->
    {Sock, ServerSA, Path} = traffic_snr_tcp_client_init(Parent),
    traffic_snr_tcp_client_announce_ready(Parent, init),
    traffic_snr_tcp_client_await_continue(Parent, connect),
    traffic_snr_tcp_client_connect(Sock, ServerSA),
    traffic_snr_tcp_client_announce_ready(Parent, connect),
    traffic_snr_tcp_client_send_loop(Parent, Sock),
    Reason = traffic_snr_tcp_client_await_terminate(Parent),
    traffic_snr_tcp_client_close(Sock, Path),
    exit(Reason).


traffic_snr_tcp_client_send_loop(Parent, Sock) ->
    case ?SEV_AWAIT_CONTINUE(Parent, parent, send) of
        {ok, stop} -> % Breaks the loop
            ?SEV_ANNOUNCE_READY(Parent, send, ok),
            ok;
        {ok, Data} ->
            case socket:send(Sock, Data) of
                ok ->
                    ?SEV_ANNOUNCE_READY(Parent, send, ok),
                    traffic_snr_tcp_client_send_loop(Parent, Sock);
                {error, Reason} = ERROR ->
                    ?SEV_ANNOUNCE_READY(Parent, send, ERROR),
                    exit({send, Reason})
            end;
        {error, Reason} ->
            exit({await_continue, Reason})
    end.

traffic_snr_tcp_client_init(Parent) ->
    put(sname, "rclient"),
    ?SEV_IPRINT("init"),
    _MRef = erlang:monitor(process, Parent),
    {ServerSA, Proto} = traffic_snr_tcp_client_await_start(Parent),
    Domain   = maps:get(family, ServerSA),
    Sock     = traffic_snr_tcp_client_create(Domain, Proto),
    Path     = traffic_snr_tcp_client_bind(Sock, Domain),
    {Sock, ServerSA, Path}.

traffic_snr_tcp_client_await_start(Parent) ->
    i("traffic_snr_tcp_client_await_start -> entry"),
    ?SEV_AWAIT_START(Parent).

traffic_snr_tcp_client_create(Domain, Proto) ->
    i("traffic_snr_tcp_client_create -> entry"),
    case socket:open(Domain, stream, Proto) of
        {ok, Sock} ->
            Sock;
        {error, Reason} ->
            exit({open_failed, Reason})
    end.

traffic_snr_tcp_client_bind(Sock, Domain) ->
    i("traffic_snr_tcp_client_bind -> entry"),
    LSA = which_local_socket_addr(Domain),
    case socket:bind(Sock, LSA) of
        ok ->
            case socket:sockname(Sock) of
                {ok, #{family := local, path := Path}} ->
                    Path;
                {ok, _} ->
                    undefined;
                {error, Reason1} ->
                    exit({sockname, Reason1})
            end;
        {error, Reason} ->
            exit({bind, Reason})
    end.

traffic_snr_tcp_client_announce_ready(Parent, Slogan) ->
    ?SEV_ANNOUNCE_READY(Parent, Slogan).

traffic_snr_tcp_client_await_continue(Parent, Slogan) ->
    i("traffic_snr_tcp_client_await_continue -> entry"),
    ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan).

traffic_snr_tcp_client_connect(Sock, ServerSA) ->
    i("traffic_snr_tcp_client_connect -> entry"),
    case socket:connect(Sock, ServerSA) of
        ok ->
            ok;
        {error, Reason} ->
            exit({connect, Reason})
    end.

traffic_snr_tcp_client_close(Sock, Path) ->
    i("traffic_snr_tcp_client_close -> entry"),
    case socket:close(Sock) of
        ok ->
            unlink_path(Path),
            ok;
        {error, Reason} ->
            ?SEV_EPRINT("failed closing: "
                        "~n   Reason: ~p", [Reason]),
            unlink_path(Path),
            {error, {close, Reason}}
    end.

traffic_snr_tcp_client_await_terminate(Parent) ->
    i("traffic_snr_tcp_client_await_terminate -> entry"),
    case ?SEV_AWAIT_TERMINATE(Parent, parent) of
        ok ->
            ok;
        {error, Reason} ->
            Reason
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case,
%% for TCP and IPv4.

traffic_ping_pong_small_send_and_recv_tcp4(Config) when is_list(Config) ->
    ?TT(?SECS(15)),
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case,
%% for TCP and IPv6.

traffic_ping_pong_small_send_and_recv_tcp6(Config) when is_list(Config) ->
    ?TT(?SECS(15)),
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case,
%% for TCP (default) and Unix Domain (stream) socket.

traffic_ping_pong_small_send_and_recv_tcpL(Config) when is_list(Config) ->
    ?TT(?SECS(15)),
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   InitState = #{domain => local,
                                 proto  => default,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case,
%% for SCTP and IPv4.

traffic_ping_pong_small_send_and_recv_sctp4(Config) when is_list(Config) ->
    ?TT(?SECS(15)),
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   has_support_ipv4(),
                   has_support_sctp()
           end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case,
%% for SCTP and IPv6.

traffic_ping_pong_small_send_and_recv_sctp6(Config) when is_list(Config) ->
    ?TT(?SECS(15)),
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   has_support_ipv6(),
                   has_support_sctp()
           end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case,
%% for TCP and IPv4.

traffic_ping_pong_medium_send_and_recv_tcp4(Config) when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case,
%% for TCP and IPv6.

traffic_ping_pong_medium_send_and_recv_tcp6(Config) when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet6,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case,
%% for TCP (default) and Unix Domain (stream) socket.

traffic_ping_pong_medium_send_and_recv_tcpL(Config) when is_list(Config) ->
    ?TT(?SECS(30)),
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(traffic_ping_pong_medium_send_and_recv_tcpL,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   InitState = #{domain => local,
                                 proto  => default,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case,
%% for SCTP and IPv4.

traffic_ping_pong_medium_send_and_recv_sctp4(Config) when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   has_support_ipv4(),
                   has_support_sctp()
           end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case,
%% for SCTP and IPv6.

traffic_ping_pong_medium_send_and_recv_sctp6(Config) when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   has_support_ipv6(),
                   has_support_sctp()
           end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet6,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case,
%% for TCP and IPv4.

traffic_ping_pong_large_send_and_recv_tcp4(Config) when is_list(Config) ->
    ?TT(?SECS(60)),
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_NUM(Config, ?TPP_LARGE_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_ipv4(),
                   is_old_fedora16(),
                   is_slow_ubuntu(Config)
           end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case,
%% for TCP and IPv6.

traffic_ping_pong_large_send_and_recv_tcp6(Config) when is_list(Config) ->
    ?TT(?SECS(60)),
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_NUM(Config, ?TPP_LARGE_NUM),
    tc_try(?FUNCTION_NAME,
           fun() -> is_old_fedora16(),
                    has_support_ipv6(),
		    is_slow_ubuntu(Config) end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case,
%% for TCP (default) and UNix Domain (stream) socket.

traffic_ping_pong_large_send_and_recv_tcpL(Config) when is_list(Config) ->
    ?TT(?SECS(60)),
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_NUM(Config, ?TPP_LARGE_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_unix_domain_socket(),
                   traffic_ping_pong_large_host_cond()
           end,
           fun() ->
                   InitState = #{domain => local,
                                 proto  => default,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case,
%% for SCTP and IPv4.

traffic_ping_pong_large_send_and_recv_sctp4(Config) when is_list(Config) ->
    ?TT(?SECS(60)),
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_NUM(Config, ?TPP_LARGE_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   is_old_fedora16(),
                   is_slow_ubuntu(Config),
                   has_support_ipv4(),
                   has_support_sctp(),
                   traffic_ping_pong_large_host_cond()
           end,
           fun() ->
                   InitState = #{domain => inet,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case,
%% for SCTP and IPv6.

traffic_ping_pong_large_send_and_recv_sctp6(Config) when is_list(Config) ->
    ?TT(?SECS(60)),
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_NUM(Config, ?TPP_LARGE_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   is_old_fedora16(),
                   is_slow_ubuntu(Config),
                   has_support_ipv6(),
                   has_support_sctp()
           end,
           fun() ->
                   InitState = #{domain => inet6,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_stream(InitState)
           end).



%% This test case is a bit extreme and fails on some hosts
%% (e.g. OpenIndiana Hipster), so exclude them.
traffic_ping_pong_large_host_cond() ->
    traffic_ping_pong_large_host_cond(os:type(), os:version()).

traffic_ping_pong_large_host_cond({unix, sunos}, _) ->
    skip("TC does not work on platform");
traffic_ping_pong_large_host_cond({unix, linux}, _) ->
    traffic_ping_pong_large_host_cond2(string:trim(os:cmd("cat /etc/issue")));
traffic_ping_pong_large_host_cond(_, _) ->
    ok.

traffic_ping_pong_large_host_cond2("Welcome to SUSE Linux Enterprise Server 10 SP1 (i586)" ++ _) ->
    skip("TC does not work on platform");
traffic_ping_pong_large_host_cond2("Fedora release 16 " ++ _) ->
    skip("Very slow VM");
traffic_ping_pong_large_host_cond2(_) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendto and recvfrom 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for two different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case, for IPv4.

traffic_ping_pong_small_sendto_and_recvfrom_udp4(Config) when is_list(Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(traffic_ping_pong_small_sendto_and_recvfrom_udp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => inet,
                                 proto  => udp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendto_and_recvfrom_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendto and recvfrom 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for two different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case, for IPv6.

traffic_ping_pong_small_sendto_and_recvfrom_udp6(Config) when is_list(Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(traffic_ping_pong_small_sendto_and_recvfrom_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => inet6,
                                 proto  => udp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendto_and_recvfrom_udp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendto and recvfrom 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for two different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case, for Unix Domain (dgram) socket.

traffic_ping_pong_small_sendto_and_recvfrom_udpL(Config) when is_list(Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(traffic_ping_pong_small_sendto_and_recvfrom_udpL,
           fun() ->
		   has_support_unix_domain_socket(),
		   is_not_windows()
	   end,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => local,
                                 proto  => default,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendto_and_recvfrom_udp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendto and recvfrom 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for two different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case, for IPv4.

traffic_ping_pong_medium_sendto_and_recvfrom_udp4(Config) when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(traffic_ping_pong_medium_sendto_and_recvfrom_udp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => inet,
                                 proto  => udp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendto_and_recvfrom_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendto and recvfrom 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for two different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case, for IPv6.

traffic_ping_pong_medium_sendto_and_recvfrom_udp6(Config) when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(traffic_ping_pong_medium_sendto_and_recvfrom_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => inet6,
                                 proto  => udp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendto_and_recvfrom_udp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendto and recvfrom 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for two different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case, for Unix Domain (dgram) socket.

traffic_ping_pong_medium_sendto_and_recvfrom_udpL(Config) when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(traffic_ping_pong_medium_sendto_and_recvfrom_udpL,
           fun() ->
		   has_support_unix_domain_socket(),
		   is_not_windows()
	   end,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => local,
                                 proto  => default,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendto_and_recvfrom_udp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case,
%% for TCP and IPv4.

traffic_ping_pong_small_sendmsg_and_recvmsg_tcp4(Config) when is_list(Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(traffic_ping_pong_small_sendmsg_and_recvmsg_tcp4,
           fun() ->
                   is_not_windows(),
                   has_support_ipv4()
           end,
           fun() ->
                   ?TT(?SECS(20)),
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case,
%% for TCP and IPv6.

traffic_ping_pong_small_sendmsg_and_recvmsg_tcp6(Config) when is_list(Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
                   has_support_ipv6()
           end,
           fun() ->
                   ?TT(?SECS(20)),
                   InitState = #{domain => inet6,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case,
%% for TCP (default) and Unix Domain (stream) socket.

traffic_ping_pong_small_sendmsg_and_recvmsg_tcpL(Config) when is_list(Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
		   has_support_unix_domain_socket()
	   end,
           fun() ->
                   ?TT(?SECS(20)),
                   InitState = #{domain => local,
                                 proto  => default,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case,
%% for SCTP and IPv4.

traffic_ping_pong_small_sendmsg_and_recvmsg_sctp4(Config)
  when is_list(Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   is_not_windows(),
                   has_support_ipv4(),
                   has_support_sctp()
           end,
           fun() ->
                   ?TT(?SECS(20)),
                   InitState = #{domain => inet,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case,
%% for SCTP and IPv6.

traffic_ping_pong_small_sendmsg_and_recvmsg_sctp6(Config)
  when is_list(Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   is_not_windows(),
                   has_support_ipv6(),
                   has_support_sctp()
           end,
           fun() ->
                   ?TT(?SECS(20)),
                   InitState = #{domain => inet6,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case,
%% for TCP and IPv4.

traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp4(Config) when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
                   has_support_ipv4()
           end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case,
%% for TCP and IPv6.

traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp6(Config)
  when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
                   has_support_ipv6()
           end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet6,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case,
%% for TCP (default) and Unix Domain (stream) socket.

traffic_ping_pong_medium_sendmsg_and_recvmsg_tcpL(Config)
  when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
		   has_support_unix_domain_socket()
	   end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => local,
                                 proto  => default,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case,
%% for SCTP and IPv4.

traffic_ping_pong_medium_sendmsg_and_recvmsg_sctp4(Config)
  when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   is_not_windows(),
                   has_support_ipv4(),
                   has_support_sctp()
           end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case,
%% for SCTP and IPv4.

traffic_ping_pong_medium_sendmsg_and_recvmsg_sctp6(Config)
  when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_flakey_os(),
                   is_not_windows(),
                   has_support_ipv6(),
                   has_support_sctp()
           end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet6,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case,
%% for TCP and IPv4.

traffic_ping_pong_large_sendmsg_and_recvmsg_tcp4(Config) when is_list(Config) ->
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_NUM(Config, ?TPP_LARGE_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
                   has_support_ipv4(),
                   traffic_ping_pong_large_sendmsg_and_recvmsg_cond()
           end,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


traffic_ping_pong_large_sendmsg_and_recvmsg_cond() ->
    traffic_ping_pong_large_sendmsg_and_recvmsg_cond(os:type(), os:version()).

traffic_ping_pong_large_sendmsg_and_recvmsg_cond({unix, linux}, {M, _, _})
  when (M < 3) ->
    skip("TC may not work on this version");
traffic_ping_pong_large_sendmsg_and_recvmsg_cond(_, _) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case,
%% for TCP and IPv6.

traffic_ping_pong_large_sendmsg_and_recvmsg_tcp6(Config) when is_list(Config) ->
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_NUM(Config, ?TPP_LARGE_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
                   has_support_ipv6(),
                   traffic_ping_pong_large_sendmsg_and_recvmsg_cond()
           end,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => inet6,
                                 proto  => tcp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case,
%% for TCP (default) and Unix Domain (stream) socket.

traffic_ping_pong_large_sendmsg_and_recvmsg_tcpL(Config) when is_list(Config) ->
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_NUM(Config, ?TPP_LARGE_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
		   has_support_unix_domain_socket()
	   end,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => local,
                                 proto  => default,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case,
%% for SCTP and IPv4.

traffic_ping_pong_large_sendmsg_and_recvmsg_sctp4(Config)
  when is_list(Config) ->
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_NUM(Config, ?TPP_LARGE_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
                   is_not_flakey_os(),
                   has_support_ipv4(),
                   has_support_sctp(),
                   traffic_ping_pong_large_sendmsg_and_recvmsg_cond()
           end,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => inet,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case,
%% for SCTP and IPv6.

traffic_ping_pong_large_sendmsg_and_recvmsg_sctp6(Config)
  when is_list(Config) ->
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_NUM(Config, ?TPP_LARGE_NUM),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
                   is_not_flakey_os(),
                   has_support_ipv6(),
                   has_support_sctp(),
                   traffic_ping_pong_large_sendmsg_and_recvmsg_cond()
           end,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => inet6,
                                 proto  => sctp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case, for IPv4.

traffic_ping_pong_small_sendmsg_and_recvmsg_udp4(Config) when is_list(Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(traffic_ping_pong_small_sendmsg_and_recvmsg_udp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => inet,
                                 proto  => udp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case, for IPv6.

traffic_ping_pong_small_sendmsg_and_recvmsg_udp6(Config) when is_list(Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(traffic_ping_pong_small_sendmsg_and_recvmsg_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => inet6,
                                 proto  => udp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case, for Unix Domain (dgram) socket.

traffic_ping_pong_small_sendmsg_and_recvmsg_udpL(Config) when is_list(Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_NUM(Config, ?TPP_SMALL_NUM),
    tc_try(traffic_ping_pong_small_sendmsg_and_recvmsg_udpL,
           fun() ->
		   has_support_unix_domain_socket(),
		   is_not_windows()
	   end,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => local,
                                 proto  => default,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case, for IPv4.

traffic_ping_pong_medium_sendmsg_and_recvmsg_udp4(Config) when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(traffic_ping_pong_medium_sendmsg_and_recvmsg_udp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => inet,
                                 proto  => udp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case, for IPv6.

traffic_ping_pong_medium_sendmsg_and_recvmsg_udp6(Config) when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(traffic_ping_pong_medium_sendmsg_and_recvmsg_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => inet6,
                                 proto  => udp,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_udp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case, for Unix Domain (dgram) socket.

traffic_ping_pong_medium_sendmsg_and_recvmsg_udpL(Config) when is_list(Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_NUM(Config, ?TPP_MEDIUM_NUM),
    tc_try(traffic_ping_pong_medium_sendmsg_and_recvmsg_udpL,
           fun() ->
		   has_support_unix_domain_socket(),
		   is_not_windows()
	   end,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => local,
                                 proto  => default,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_udp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ping-Pong for TCP

traffic_ping_pong_send_and_recv_stream(InitState) ->
    Send = fun(Sock, Data) -> socket:send(Sock, Data) end,
    Recv = fun(Sock, Sz)   -> socket:recv(Sock, Sz) end,
    InitState2 = InitState#{send => Send, % Send function
                            recv => Recv  % Receive function
                           },
    traffic_ping_pong_send_and_receive_stream(InitState2).

traffic_ping_pong_sendmsg_and_recvmsg_stream(#{domain := local} = InitState) ->
    Recv = fun(Sock, Sz)   -> 
                   case socket:recvmsg(Sock, Sz, 0) of
                       %% On some platforms, the address
                       %% *is* provided (e.g. linux)
                       {ok, #{addr  := #{family := local},
                              iov   := [Data]}} ->
                           {ok, Data};
                       {ok, #{addr := _} = Msg} ->
                           {error, {msg, Msg}};
                       %% On some platforms, the address
                       %% is *not* provided (e.g. FreeBSD)
                       {ok, #{iov   := [Data]}} ->
                           {ok, Data};
                       {ok, Msg} ->
                           {error, {msg, Msg}};
                       {error, _} = ERROR ->
                           ERROR
                   end
           end,
    InitState2 = InitState#{recv => Recv},  % Receive function
    traffic_ping_pong_sendmsg_and_recvmsg_stream2(InitState2);
traffic_ping_pong_sendmsg_and_recvmsg_stream(InitState) ->
    Recv = fun(Sock, Sz)   -> 
                   case socket:recvmsg(Sock, Sz, 0) of
                       {ok, #{iov   := [Data]}} ->
                           {ok, Data};
                       {ok, Msg} ->
                           {error, {msg, Msg}};
                       {error, _} = ERROR ->
                           ERROR
                   end
           end,
    InitState2 = InitState#{recv => Recv},  % Receive function
    traffic_ping_pong_sendmsg_and_recvmsg_stream2(InitState2).

traffic_ping_pong_sendmsg_and_recvmsg_stream2(InitState) ->
    Send = fun(Sock, Data) when is_binary(Data) ->
                   Msg = #{iov => [Data]},
                   socket:sendmsg(Sock, Msg);
              (Sock, Data) when is_list(Data) -> %% We assume iovec...
                   Msg = #{iov => Data},
                   socket:sendmsg(Sock, Msg)
           end,
    InitState2 = InitState#{send => Send}, % Send function
    traffic_ping_pong_send_and_receive_stream(InitState2).


traffic_ping_pong_send_and_receive_stream(#{msg := Msg} = InitState) ->
    Fun = fun(Sock) -> 
                  {ok, RcvSz} = socket:getopt(Sock, socket, rcvbuf),
		  ?SEV_IPRINT("RcvBuf is ~p (needs at least ~p)", 
			      [RcvSz, 16+size(Msg)]),
                  if (RcvSz < size(Msg)) ->
                          NewRcvSz = 1024+size(Msg),
                          case socket:setopt(Sock, socket, rcvbuf, NewRcvSz) of
			      ok ->
				  ok;
			      {error, enobufs} ->
				  skip(?F("Change ~w buffer size (to ~w) "
                                          "not allowed", 
                                          [rcvbuf, NewRcvSz]));
			      {error, Reason1} ->
				  ?FAIL({rcvbuf, Reason1})
			  end;
                     true ->
                          ok
                  end,
                  {ok, SndSz} = socket:getopt(Sock, socket, sndbuf),
		  ?SEV_IPRINT("SndBuf is ~p (needs at least ~p)", 
			      [SndSz, 16+size(Msg)]),
                  if (SndSz < size(Msg)) ->
                          NewSndSz = 1024+size(Msg),
                          case socket:setopt(Sock, socket, sndbuf, NewSndSz) of
			      ok ->
				  ok;
			      {error, enobufs} ->
				  skip(?F("Change ~w buffer size (to ~w) "
                                          "not allowed", 
                                          [sndbuf, NewSndSz]));
			      {error, Reason2} ->
				  ?FAIL({sndbuf, Reason2})
			  end;
                     true ->
                          ok
                  end,
                  case os:type() of
                      {win32, nt} ->
                          ok = socket:setopt(Sock, otp, rcvbuf, 12*1024);
                      _ ->
                          ok = socket:setopt(Sock, otp, rcvbuf, {12, 1024})
                  end
          end,
    traffic_ping_pong_send_and_receive_stream2(InitState#{buf_init => Fun}).

traffic_ping_pong_send_and_receive_stream2(InitState) ->
    ServerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "create listen socket",
           cmd  => fun(#{domain := Domain, proto := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, eprotonosupport = Reason} ->
                                   {skip, Reason};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{domain := local,
                         lsock  := LSock,
                         lsa    := LSA} = _State) ->
                           case socket:bind(LSock, LSA) of
                               ok ->
                                   ok; % We do not care about the port for local
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{lsock := LSock, local_sa := LSA} = State) ->
                           case sock_bind(LSock, LSA) of
                               ok ->
                                   Port = sock_port(LSock),
                                   ?SEV_IPRINT("bound to port: ~w", [Port]),
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "maybe init buffers",
           cmd  => fun(#{lsock := LSock, buf_init := BufInit} = _State) ->
                           BufInit(LSock)
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{domain := local,
                         tester := Tester, local_sa := LSA}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, LSA),
                           ok;
                      (#{tester := Tester, local_sa := LSA, lport := Port}) ->
                           ServerSA = LSA#{port => Port},
                           ?SEV_ANNOUNCE_READY(Tester, init, ServerSA),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   {ok, State#{csock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create handler",
           cmd  => fun(State) ->
                           Handler = tpp_tcp_handler_create(),
                           ?SEV_IPRINT("handler created: ~p", [Handler]),
                           {ok, State#{handler => Handler}}
                   end},
         #{desc => "monitor handler",
           cmd  => fun(#{handler := Handler} = _State) ->
                           _MRef = erlang:monitor(process, Handler),
                           ok
                   end},
         #{desc => "transfer connection socket ownership to handler",
           cmd  => fun(#{handler := Handler, csock := Sock} = _State) ->
                           socket:setopt(Sock, otp, controlling_process, Handler)
                   end},
         #{desc => "start handler",
           cmd  => fun(#{handler  := Handler,
                         csock    := Sock,
                         send     := Send,
                         recv     := Recv} = _State) ->
                           ?SEV_ANNOUNCE_START(Handler, {Sock, Send, Recv}),
                           ok
                   end},
         #{desc => "await handler ready (init)",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = State) ->
                           case ?SEV_AWAIT_READY(Handler, handler, init, 
                                                 [{tester, Tester}]) of
                               ok ->
                                   {ok, maps:remove(csock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},
         #{desc => "await continue (recv)",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv, 
                                               [{handler, Handler}])
                   end},
         #{desc => "order handler to recv",
           cmd  => fun(#{handler := Handler} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Handler, recv),
                           ok
                   end},
         #{desc => "await handler ready (recv)",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = State) ->
                           case ?SEV_AWAIT_READY(Handler, handler, recv, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   %% ?SEV_IPRINT("Result: ~p", [Result]),
                                   {ok, State#{result => Result}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv)",
           cmd  => fun(#{tester := Tester, 
                         result := Result} = State) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv, Result),
                           {ok, maps:remove(result, State)}
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "stop handler",
           cmd  => fun(#{handler := Handler}) ->
                           ?SEV_ANNOUNCE_TERMINATE(Handler),
                           ok
                   end},
         #{desc => "await handler termination",
           cmd  => fun(#{handler := Handler} = State) ->
                           ?SEV_AWAIT_TERMINATION(Handler),
                           State1 = maps:remove(handler, State),
                           {ok, State1}
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{domain   := local,
                         lsock    := Sock,
                         local_sa := #{path := Path}} = State) ->
                           (catch socket:close(Sock)),
                           State1 =
                               unlink_path(Path,
                                           fun() -> 
                                                   maps:remove(local_sa, State)
                                           end,
                                           fun() -> State end),
                           {ok, maps:remove(lsock, State1)};
                      (#{lsock := Sock} = State) ->
                           (catch socket:close(Sock)),
                           {ok, maps:remove(lsock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, ServerSA} = ?SEV_AWAIT_START(),
                           {ok, State#{tester    => Tester, 
                                       server_sa => ServerSA}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create node",
           cmd  => fun(State) ->
                           {Peer, Node} = start_node("client"),
                           {ok, State#{peer => Peer, node => Node}}
                   end},
         #{desc => "monitor client node",
           cmd  => fun(#{node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "create remote client",
           cmd  => fun(#{node := Node} = State) ->
                           Pid = tpp_tcp_client_create(Node),
                           ?SEV_IPRINT("remote client created: ~p", [Pid]),
                           {ok, State#{rclient => Pid}}
                   end},
         #{desc => "monitor remote client",
           cmd  => fun(#{rclient := Pid}) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order remote client to start",
           cmd  => fun(#{rclient   := RClient,
                         proto     := Proto,
                         server_sa := ServerSA,
                         buf_init  := BufInit,
                         send      := Send,
                         recv      := Recv}) ->
                           ?SEV_ANNOUNCE_START(RClient, 
                                               {ServerSA, Proto, BufInit,
                                                Send, Recv}),
                           ok
                   end},
         #{desc => "await remote client ready",
           cmd  => fun(#{tester  := Tester,
                         rclient := RClient} = _State) ->
                           case ?SEV_AWAIT_READY(RClient, rclient, init, 
                                                 [{tester, Tester}]) of
                               ok ->
                                   ?SEV_IPRINT("remote client started"),
                                   ok;
                               {error, {unexpected_exit, _, {bind, eaddrnotavail = Reason}}} ->
                                   ?SEV_IPRINT("remote client bind failure:"
                                               "~n   ~p", [Reason]),
                                   {skip, Reason};
                               {error, no_address = Reason} ->
                                   ?SEV_IPRINT("remote valid address"),
                                   {skip, Reason};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("remote client failure:"
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester  := Tester,
                         rclient := RClient} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect, 
                                               [{rclient, RClient}]),
                           ok
                   end},
         #{desc => "order remote client to continue (connect)",
           cmd  => fun(#{rclient := RClient}) ->
                           ?SEV_ANNOUNCE_CONTINUE(RClient, connect),
                           ok
                   end},
         #{desc => "await remote client ready (connect)",
           cmd  => fun(#{tester  := Tester,
                         rclient := RClient} = _State) ->
                           ?SEV_AWAIT_READY(RClient, rclient, connect, 
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},
         #{desc => "await continue (send)",
           cmd  => fun(#{tester  := Tester,
                         rclient := RClient} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, 
                                               send, 
                                               [{rclient, RClient}])
                   end},
         #{desc => "order remote client to continue (send)",
           cmd  => fun(#{rclient := RClient,
                         msg     := Msg,
                         num     := Num} = State) ->
                           Data = {Msg, Num},
                           ?SEV_ANNOUNCE_CONTINUE(RClient, send, Data),
                           {ok, maps:remove(data, State)}
                   end},
         #{desc => "await remote client ready (send)",
           cmd  => fun(#{tester  := Tester,
                         rclient := RClient} = State) ->
                           case ?SEV_AWAIT_READY(RClient, rclient, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   {ok, State#{result => Result}};
                               {error,
                                {unexpected_exit, rclient, noconnection}} ->
                                   %% One guess is that the message is so
                                   %% "large" that the client node died on us.
                                   %% Or so "large" that the connection (to
                                   %% the node) fails/dies.
                                   %% Either way, we assume this is not actually
                                   %% related to what we are testing => skip
                                   ?SEV_IPRINT("lost connection "
                                               "to remote client node => SKIP"),
                                   {skip, {rclient, noconnection}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (send)",
           cmd  => fun(#{tester := Tester, result := Result} = State) ->
                           ?SEV_ANNOUNCE_READY(Tester, send, Result),
                           {ok, maps:remove(result, State)}
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester  := Tester, 
                         rclient := RClient} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester,
                                                     [{rclient, RClient}]) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "stop remote client",
           cmd  => fun(#{rclient := RClient}) ->
                           ?SEV_ANNOUNCE_TERMINATE(RClient),
                           ok
                   end},
         #{desc => "await remote client termination",
           cmd  => fun(#{rclient := RClient} = State) ->
                           ?SEV_AWAIT_TERMINATION(RClient),
                           State1 = maps:remove(rclient, State),
                           {ok, State1}
                   end},
         #{desc => "stop client node",
           cmd  => fun(#{peer := Peer} = State) ->
                           {ok,
                            try peer:stop(Peer) of
                                ok ->
                                    State#{node_stop => ok};
                                {error, Reason} ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   ~p", [Reason]),
                                    State#{node_stop => error}
                            catch
                                C:E:S ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   Class: ~p"
                                                "~n   Error: ~p"
                                                "~n   Stack: ~p",[C, E, S]),
                                    State#{node_stop => error}
                            end}
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node, node_stop := ok} = State) ->
                           ?SEV_IPRINT("Success node stop - await nodedown"),
                           receive
                               {nodedown, Node} ->
                                   ?SEV_IPRINT("nodedown received - cleanup"),
                                   State1 = maps:remove(node, State),
                                   {ok, State1}
                           end;
                      (#{node_stop := error} = State) ->
                           ?SEV_IPRINT("Failed node stop - cleanup"),
                           State1 = maps:remove(node, State),
                           {ok, State1}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the server
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Pid} = State) ->
                           {ok, ServerSA} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{server_sa => ServerSA}}
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client    := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, ServerSA),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, init)
                   end},
 
         %% The actual test
         #{desc => "order server continue (accept)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client continue (connect)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await server ready (accept)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept,
                                            [{client, Client}]),
                           ok
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, connect, 
                                            [{server, Server}])
                   end},
         #{desc => "order server continue (recv)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client continue (send)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, send),
                           ok
                   end},
         #{desc => "await client ready (send)",
           cmd  => fun(#{server := Server,
                         client := Client} = State) ->
                           case ?SEV_AWAIT_READY(Client, client, send, 
                                                 [{server, Server}]) of
                               {ok, {_, _, _, _, _} = Result} ->
                                   ?SEV_IPRINT("client result: "
                                               "~n   ~p", [Result]),
                                   {ok, State#{client_result => Result}};
                               {ok, BadResult} ->
                                   ?SEV_EPRINT("client result: "
                                               "~n   ~p", [BadResult]),
                                   {error, {invalid_client_result, BadResult}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await server ready (recv)",
           cmd  => fun(#{server := Server,
                         client := Client,
                         num    := Num} = State) ->
                           case ?SEV_AWAIT_READY(Server, server, recv,
                                                 [{client, Client}]) of
                               {ok, {Num, _, _, _, _} = Result} ->
                                   ?SEV_IPRINT("server result: "
                                               "~n   ~p", [Result]),
                                   Result2 = erlang:delete_element(1, Result),
                                   {ok, State#{server_result => Result2}};
                               {ok, BadResult} ->
                                   ?SEV_EPRINT("bad server result: "
                                               "~n   ~p", [BadResult]),
                                   {error, {invalid_server_result, BadResult}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "present result",
           cmd  => fun(#{server_result := SRes,
                         client_result := CRes,
                         num           := Num} = State) ->
                           {SSent, SReceived, SStart, SStop} = SRes,
                           {CSent, CReceived, _, CStart, CStop} = CRes,
                           STime = tdiff(SStart, SStop),
                           CTime = tdiff(CStart, CStop),
                           ?SEV_IPRINT("process result data:"
                                       "~n   Num:          ~p"
                                       "~n   Server Sent:  ~p"
                                       "~n   Server Recv:  ~p"
                                       "~n   Server Start: ~p"
                                       "~n   Server Stop:  ~p"
                                       "~n   Server Time:  ~p"
                                       "~n   Client Sent:  ~p"
                                       "~n   Client Recv:  ~p"
                                       "~n   Client Start: ~p"
                                       "~n   Client Stop:  ~p"
                                       "~n   Client Time:  ~p",
                                       [Num,
                                        SSent, SReceived, SStart, SStop,
                                        STime,
                                        CSent, CReceived, CStart, CStop,
                                        CTime]),
                           if
                               (STime =:= 0) orelse
                               (CTime =:= 0) ->
                                   {skip,
                                    ?F("Invalid exec time(s): ~w , ~w",
                                       [STime, CTime])};
                               true ->
                                   %% Note that the sizes we are counting is 
                                   %% only the "data" part of the messages.
                                   %% There is also fixed header for each
                                   %% message, which of course is small for
                                   %% the large messages, but comparatively
                                   %% big for the small messages!
                                   ?SEV_IPRINT(
                                      "Results: ~w messages exchanged"
                                      "~n   Server: ~w msec"
                                      "~n      ~.2f msec/message (roundtrip)"
                                      "~n      ~.2f messages/msec (roundtrip)"
                                      "~n      ~w bytes/msec sent"
                                      "~n      ~w bytes/msec received"
                                      "~n   Client: ~w msec"
                                      "~n      ~.2f msec/message (roundtrip)"
                                      "~n      ~.2f messages/msec (roundtrip)"
                                      "~n      ~w bytes/msec sent"
                                      "~n      ~w bytes/msec received",
                                      [Num,
                                       STime,
                                       STime / Num,
                                       Num / STime,
                                       SSent div STime,
                                       SReceived div STime,
                                       CTime,
                                       CTime / Num,
                                       Num / CTime,
                                       CSent div CTime,
                                       CReceived div CTime]),
                                   State1 = maps:remove(server_result, State),
                                   State2 = maps:remove(client_result, State1),
                                   {ok, State2}
                           end
                   end},

         %% Terminations
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(server, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start server evaluator"),
    ServerInitState = #{domain   => maps:get(domain,   InitState),
                        proto    => maps:get(proto,    InitState),
                        recv     => maps:get(recv,     InitState),
                        send     => maps:get(send,     InitState),
                        buf_init => maps:get(buf_init, InitState)},
    Server = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator(s)"),
    ClientInitState = InitState#{host => local_host()},
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{server => Server#ev.pid,
                        client => Client#ev.pid,
                        num    => maps:get(num, InitState)},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).
    

tpp_tcp_handler_create() ->
    Self = self(),
    erlang:spawn(fun() -> tpp_tcp_handler(Self) end).

tpp_tcp_handler(Parent) ->
    tpp_tcp_handler_init(Parent),
    {Sock, Send, Recv} = tpp_tcp_handler_await_start(Parent),
    tpp_tcp_handler_announce_ready(Parent, init),
    tpp_tcp_handler_await_continue(Parent, recv),
    Result = tpp_tcp_handler_msg_exchange(Sock, Send, Recv),
    tpp_tcp_handler_announce_ready(Parent, recv, Result),
    Reason = tpp_tcp_handler_await_terminate(Parent),
    ?SEV_IPRINT("terminating"),
    exit(Reason).

tpp_tcp_handler_init(Parent) ->
    put(sname, "handler"),
    ?SEV_IPRINT("init"),
    _MRef = erlang:monitor(process, Parent),
    ok.

tpp_tcp_handler_await_start(Parent) ->
    ?SEV_IPRINT("await start"),
    ?SEV_AWAIT_START(Parent).

tpp_tcp_handler_announce_ready(Parent, Slogan) ->
    ?SEV_IPRINT("announce ready (~p)", [Slogan]),
    ?SEV_ANNOUNCE_READY(Parent, Slogan).
tpp_tcp_handler_announce_ready(Parent, Slogan, Extra) ->
    ?SEV_IPRINT("announce ready (~p)", [Slogan]),
    ?SEV_ANNOUNCE_READY(Parent, Slogan, Extra).

tpp_tcp_handler_await_continue(Parent, Slogan) ->
    ?SEV_IPRINT("await continue (~p)", [Slogan]),
    case ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan) of
        ok ->
            %% ?SEV_IPRINT("continue (~p): ok", [Slogan]),
            ok;
        {error, Reason} ->
            ?SEV_EPRINT("continue (~p): error"
                        "~n   ~p", [Slogan, Reason]),
            exit({continue, Slogan, Reason})
    end.

tpp_tcp_handler_await_terminate(Parent) ->
    ?SEV_IPRINT("await terminate"),
    case ?SEV_AWAIT_TERMINATE(Parent, parent) of
        ok ->
            ok;
        {error, Reason} ->
            Reason
    end.

tpp_tcp_handler_msg_exchange(Sock, Send, Recv) ->
    tpp_tcp_handler_msg_exchange_loop(Sock, Send, Recv, 0, 0, 0, undefined).

tpp_tcp_handler_msg_exchange_loop(Sock, Send, Recv, N, Sent, Received, Start) ->
    %% ?SEV_IPRINT("[~w] try receive", [N]),
    case tpp_tcp_recv_req(Sock, Recv) of
        {ok, Msg, RecvSz} ->
            NewStart = if (Start =:= undefined) -> ?SLIB:timestamp(); 
                          true -> Start end,
            %% ?SEV_IPRINT("[~w] received - now try send", [N]),
            case tpp_tcp_send_rep(Sock, Send, Msg) of
                {ok, SendSz} ->
                    tpp_tcp_handler_msg_exchange_loop(Sock, Send, Recv,
                                                      N+1,
                                                      Sent+SendSz,
                                                      Received+RecvSz,
                                                      NewStart);
                {error, SReason} ->
                    ?SEV_EPRINT("send (~w): ~p", [N, SReason]),
                    exit({send, SReason, N})
            end;
        {error, closed} ->
            ?SEV_IPRINT("closed - we are done: ~w, ~w, ~w", [N, Sent, Received]),
            Stop = ?SLIB:timestamp(),
            {N, Sent, Received, Start, Stop};
        {error, RReason} ->
            ?SEV_EPRINT("recv (~w): ~p", [N, RReason]),
            exit({recv, RReason, N})
    end.
            
%% The (remote) client process

tpp_tcp_client_create(Node) ->
    Self = self(),
    Fun  = fun() -> tpp_tcp_client(Self) end,
    erlang:spawn(Node, Fun).

tpp_tcp_client(Parent) ->
    tpp_tcp_client_init(Parent),
    {ServerSA, Proto, BufInit, Send, Recv} = tpp_tcp_client_await_start(Parent),
    Domain   = maps:get(family, ServerSA),
    Sock     = tpp_tcp_client_sock_open(Domain, Proto, BufInit),
    Path     = tpp_tcp_client_sock_bind(Sock, Domain),
    tpp_tcp_client_announce_ready(Parent, init),
    tpp_tcp_client_await_continue(Parent, connect),
    tpp_tcp_client_sock_connect(Sock, ServerSA),
    tpp_tcp_client_announce_ready(Parent, connect),
    {InitMsg, Num} = tpp_tcp_client_await_continue(Parent, send),
    Result = tpp_tcp_client_msg_exchange(Sock, Send, Recv, InitMsg, Num),
    tpp_tcp_client_announce_ready(Parent, send, Result),
    Reason = tpp_tcp_client_await_terminate(Parent),
    tpp_tcp_client_sock_close(Sock, Path),
    ?SEV_IPRINT("terminating"),
    exit(Reason).

tpp_tcp_client_init(Parent) ->
    put(sname, "rclient"),
    ?SEV_IPRINT("init"),
    _MRef = erlang:monitor(process, Parent),
    ok.

tpp_tcp_client_await_start(Parent) ->
    ?SEV_IPRINT("await start"),
    ?SEV_AWAIT_START(Parent).

tpp_tcp_client_announce_ready(Parent, Slogan) ->
    ?SEV_IPRINT("announce ready (~p)", [Slogan]),
    ?SEV_ANNOUNCE_READY(Parent, Slogan).
tpp_tcp_client_announce_ready(Parent, Slogan, Extra) ->
    ?SEV_IPRINT("announce ready (~p): ~p", [Slogan, Extra]),
    ?SEV_ANNOUNCE_READY(Parent, Slogan, Extra).

tpp_tcp_client_await_continue(Parent, Slogan) ->
    ?SEV_IPRINT("await continue (~p)", [Slogan]),
    case ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan) of
        ok ->
            ?SEV_IPRINT("continue (~p): ok", [Slogan]),
            ok;
        {ok, Data} ->
            ?SEV_IPRINT("continue (~p): ok with data", [Slogan]),
            Data;
        {error, Reason} ->
            ?SEV_EPRINT("continue (~p): error"
                        "~n   ~p", [Slogan, Reason]),
            exit({continue, Slogan, Reason})
    end.

tpp_tcp_client_await_terminate(Parent) ->
    ?SEV_IPRINT("await terminate"),
    case ?SEV_AWAIT_TERMINATE(Parent, parent) of
        ok ->
            ?SEV_IPRINT("termination received: normal"),
            normal;
        {error, Reason} ->
            ?SEV_IPRINT("termination received: ~w", [Reason]),
            Reason
    end.

tpp_tcp_client_msg_exchange(Sock, Send, Recv, InitMsg, Num) ->
    Start = ?SLIB:timestamp(),
    tpp_tcp_client_msg_exchange_loop(Sock, Send, Recv, InitMsg, 
                                     Num, 0, 0, 0, Start).

tpp_tcp_client_msg_exchange_loop(Sock, _Send, _Recv, _Msg,
                                 Num, Num, Sent, Received,
                                 Start) ->
    Stop = ?SLIB:timestamp(),
    Info = socket:info(Sock),
    case socket:close(Sock) of
        ok ->
            {Sent, Received, Info, Start, Stop};
        {error, Reason} ->
            exit({failed_closing, Reason})
    end;
tpp_tcp_client_msg_exchange_loop(Sock, Send, Recv, Data, 
                                 Num, N, Sent, Received, Start) ->
    %% d("tpp_tcp_client_msg_exchange_loop(~w,~w) try send ~w", [Num,N,size(Data)]),
    case tpp_tcp_send_req(Sock, Send, Data) of
        {ok, SendSz} ->
            %% d("tpp_tcp_client_msg_exchange_loop(~w,~w) sent - "
            %%   "now try recv", [Num,N]),
            case tpp_tcp_recv_rep(Sock, Recv) of
                {ok, NewData, RecvSz} ->
                    tpp_tcp_client_msg_exchange_loop(Sock, Send, Recv,
                                                     NewData, Num, N+1,
                                                     Sent+SendSz, 
                                                     Received+RecvSz, 
                                                     Start);
                {error, RReason} ->
                    ?SEV_EPRINT("recv (~w of ~w): ~p: "
                                "~n   ~p", [N, Num, RReason, ?MQ()]),
                    exit({recv, RReason, N})
            end;
        {error, SReason} ->
            ?SEV_EPRINT("send (~w of ~w): ~p"
                        "~n   ~p", [N, Num, SReason, ?MQ()]),
            case SReason of
                emsgsize ->
                    exit({send, SReason, N, byte_size(Data)});
                _ ->
                    exit({send, SReason, N})
            end
    end.

tpp_tcp_client_sock_open(Domain, Proto, BufInit) ->
    case socket:open(Domain, stream, Proto) of
        {ok, Sock} ->
            ok = BufInit(Sock),
            Sock;
        {error, Reason} ->
            exit({open_failed, Reason})
    end.

tpp_tcp_client_sock_bind(Sock, Domain) ->
    LSA = which_local_socket_addr(Domain),
    case socket:bind(Sock, LSA) of
        ok ->
            case socket:sockname(Sock) of
                {ok, #{family := local, path := Path}} ->
                    Path;
                {ok, _} ->
                    undefined;
                {error, Reason1} ->
                    exit({sockname, Reason1})
            end;
        {error, Reason2} ->
            exit({bind, Reason2})
    end.

tpp_tcp_client_sock_connect(Sock, ServerSA) ->
    case socket:connect(Sock, ServerSA) of
        ok ->
            ok;
        {error, Reason} ->
            exit({connect, Reason})
    end.

tpp_tcp_client_sock_close(Sock, Path) ->
    case socket:close(Sock) of
        ok ->
            unlink_path(Path),
            ok;
        {error, closed} ->
            ok;
        {error, Reason} ->
            ?SEV_EPRINT("failed closing: "
                        "~n   Reason: ~p", [Reason]),
            unlink_path(Path),
            {error, {close, Reason}}
    end.

    
    
-define(TPP_REQUEST, 1).
-define(TPP_REPLY,   2).

tpp_tcp_recv_req(Sock, Recv) ->
    tpp_tcp_recv(Sock, Recv, ?TPP_REQUEST).

tpp_tcp_recv_rep(Sock, Recv) ->
    tpp_tcp_recv(Sock, Recv, ?TPP_REPLY).

tpp_tcp_recv(Sock, Recv, Tag) ->
    case Recv(Sock, 0) of
        {ok, <<Tag:32/integer, Sz:32/integer, Data/binary>> = Msg} 
          when (Sz =:= size(Data)) ->
            %% We got it all
            {ok, Data, size(Msg)};
        {ok, <<Tag:32/integer, Sz:32/integer, Data/binary>> = Msg} ->
            Remains = Sz - size(Data),
            tpp_tcp_recv(Sock, Recv, Tag, Remains, size(Msg), [Data]);
        {ok, <<Tag:32/integer, _/binary>>} ->
            {error, {invalid_msg_tag, Tag}};
        {error, _R} = ERROR ->
            ERROR
    end.

tpp_tcp_recv(Sock, Recv, Tag, Remaining, AccSz, Acc) ->
    case Recv(Sock, Remaining) of
        {ok, Data} when (Remaining =:= size(Data)) ->
            %% We got the rest
            TotSz = AccSz + size(Data),
            {ok, erlang:iolist_to_binary(lists:reverse([Data | Acc])), TotSz};
        {ok, Data} when (Remaining > size(Data)) ->
            tpp_tcp_recv(Sock, Recv, Tag, 
                         Remaining - size(Data), AccSz + size(Data),     
                         [Data | Acc]);
        {error, _R} = ERROR ->
            ERROR
    end.
                                                         
            
tpp_tcp_send_req(Sock, Send, Data) ->
    tpp_tcp_send(Sock, Send, ?TPP_REQUEST, Data).

tpp_tcp_send_rep(Sock, Send, Data) ->
    tpp_tcp_send(Sock, Send, ?TPP_REPLY, Data).

tpp_tcp_send(Sock, Send, Tag, Data) ->
    DataSz = size(Data),
    Msg    = <<Tag:32/integer, DataSz:32/integer, Data/binary>>,
    tpp_tcp_send_msg(Sock, Send, Msg, 0).

tpp_tcp_send_msg(Sock, Send, Msg, AccSz) when is_binary(Msg) ->
    case Send(Sock, Msg) of
        ok ->
            {ok, AccSz+size(Msg)};
        {ok, Rest} -> % This is an IOVec
            RestBin = list_to_binary(Rest),
            tpp_tcp_send_msg(Sock, Send, RestBin, AccSz+(size(Msg)-size(RestBin)));
        {error, _} = ERROR ->
            ERROR
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ping-Pong for UDP

traffic_ping_pong_sendto_and_recvfrom_udp(InitState) ->
    Send = fun(Sock, Data, Dest) ->
                   socket:sendto(Sock, Data, Dest)
           end,
    Recv = fun(Sock, Sz)         ->
                   socket:recvfrom(Sock, Sz)
           end,
    InitState2 = InitState#{send => Send, % Send function
                            recv => Recv  % Receive function
                           },
    traffic_ping_pong_send_and_receive_udp(InitState2).

traffic_ping_pong_sendmsg_and_recvmsg_udp(InitState) ->
    Send = fun(Sock, Data, Dest) when is_binary(Data) ->
                   Msg = #{addr => Dest, iov => [Data]},
                   socket:sendmsg(Sock, Msg);
              (Sock, Data, Dest) when is_list(Data) -> %% We assume iovec...
                   Msg = #{addr => Dest, iov => Data},
                   socket:sendmsg(Sock, Msg)
           end,
    Recv = fun(Sock, Sz)   ->
                   case socket:recvmsg(Sock, Sz, 0) of
                       {ok, #{addr  := Source,
                              iov   := [Data]}} ->
                           {ok, {Source, Data}};
                       {error, _} = ERROR ->
                           ERROR
                   end
           end,
    InitState2 = InitState#{send => Send, % Send function
                            recv => Recv  % Receive function
                           },
    traffic_ping_pong_send_and_receive_udp(InitState2).


traffic_ping_pong_send_and_receive_udp(#{msg := Msg} = InitState) ->
    Fun = fun(Sock) -> 
                  {ok, RcvSz} = socket:getopt(Sock, socket, rcvbuf),
                  if (RcvSz =< (8+size(Msg))) ->
                          i("adjust socket rcvbuf buffer size"),
                          ok = socket:setopt(Sock, socket, rcvbuf, 1024+size(Msg));
                     true ->
                          ok
                  end,
                  {ok, SndSz} = socket:getopt(Sock, socket, sndbuf),
                  if (SndSz =< (8+size(Msg))) ->
                          i("adjust socket sndbuf buffer size"),
                          ok = socket:setopt(Sock, socket, sndbuf, 1024+size(Msg));
                     true ->
                          ok
                  end,
                  {ok, OtpRcvBuf} = socket:getopt(Sock, otp, rcvbuf),
                  if
                      (OtpRcvBuf =< (8+size(Msg))) ->
                          i("adjust otp rcvbuf buffer size"),
                          ok = socket:setopt(Sock, otp, rcvbuf, 1024+size(Msg));
                      true ->
                          ok
                  end
          end,
    traffic_ping_pong_send_and_receive_udp2(InitState#{buf_init => Fun}).

traffic_ping_pong_send_and_receive_udp2(InitState) ->
    ServerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain := Domain, proto := Proto} = State) ->
                           case socket:open(Domain, dgram, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{domain := local,
                         sock := Sock, local_sa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{sock := Sock, local_sa := LSA} = State) ->
                           case sock_bind(Sock, LSA) of
                               ok ->
                                   Port = sock_port(Sock),
                                   {ok, State#{port => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "maybe init buffers",
           cmd  => fun(#{sock := Sock, buf_init := BufInit} = _State) ->
                           BufInit(Sock)
                   end},
         #{desc => "create handler",
           cmd  => fun(State) ->
                           Handler = tpp_udp_server_handler_create(),
                           ?SEV_IPRINT("handler created: ~p", [Handler]),
                           {ok, State#{handler => Handler}}
                   end},
         #{desc => "monitor handler",
           cmd  => fun(#{handler := Handler} = _State) ->
                           _MRef = erlang:monitor(process, Handler),
                           ok
                   end},
         #{desc => "start handler",
           cmd  => fun(#{handler := Handler,
                         sock    := Sock,
                         send    := Send,
                         recv    := Recv} = _State) ->
                           ?SEV_ANNOUNCE_START(Handler, {Sock, Send, Recv}),
                           ok
                   end},
         #{desc => "await handler ready (init)",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = State) ->
                           case ?SEV_AWAIT_READY(Handler, handler, init, 
                                                 [{tester, Tester}]) of
                               ok ->
                                   {ok, maps:remove(csock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{domain := local,
                         tester := Tester, local_sa := LSA}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, LSA),
                           ok;
                      (#{tester := Tester, local_sa := LSA, port := Port}) ->
                           ServerSA = LSA#{port => Port},
                           ?SEV_ANNOUNCE_READY(Tester, init, ServerSA),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (recv)",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv, 
                                               [{handler, Handler}])
                   end},
         #{desc => "order handler to recv",
           cmd  => fun(#{handler := Handler,
                         sock    := _Sock} = _State) ->
                           %% socket:setopt(Sock, otp, debug, true),
                           ?SEV_ANNOUNCE_CONTINUE(Handler, recv),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close, 
                                               [{handler, Handler}])
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           %% socket:setopt(Sock, otp, debug, true),
                           case socket:close(Sock) of
                               ok ->
                                   {ok, maps:remove(sock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "(maybe) unlink socket",
           cmd  => fun(#{domain   := local,
                         local_sa := #{path := Path}} = State) ->
                           unlink_path(Path,
                                       fun() ->
                                               {ok, maps:remove(local_sa, State)}
                                       end,
                                       fun() ->
                                               ok
                                       end);
                      (_) ->
                           ok
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},
         #{desc => "await handler ready (recv)",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = State) ->
                           case ?SEV_AWAIT_READY(Handler, handler, recv, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   %% ?SEV_IPRINT("Result: ~p", [Result]),
                                   {ok, State#{result => Result}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv)",
           cmd  => fun(#{tester := Tester, 
                         result := Result} = State) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv, Result),
                           {ok, maps:remove(result, State)}
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "stop handler",
           cmd  => fun(#{handler := Handler}) ->
                           ?SEV_ANNOUNCE_TERMINATE(Handler),
                           ok
                   end},
         #{desc => "await handler termination",
           cmd  => fun(#{handler := Handler} = State) ->
                           ?SEV_AWAIT_TERMINATION(Handler),
                           State1 = maps:remove(handler, State),
                           {ok, State1}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, ServerSA} = ?SEV_AWAIT_START(),
                           {ok, State#{tester    => Tester, 
                                       server_sa => ServerSA}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create node",
           cmd  => fun(State) ->
                           {Peer, Node} = start_node("client"),
                           {ok, State#{peer => Peer, node => Node}}
                   end},
         #{desc => "monitor client node",
           cmd  => fun(#{node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "create (remote) handler",
           cmd  => fun(#{node := Node} = State) ->
                           Pid = tpp_udp_client_handler_create(Node),
                           ?SEV_IPRINT("handler created: ~p", [Pid]),
                           {ok, State#{handler => Pid}}
                   end},
         #{desc => "monitor remote handler",
           cmd  => fun(#{handler := Pid}) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order remote handler to start",
           cmd  => fun(#{handler   := Handler,
                         server_sa := ServerSA,
                         proto     := Proto,
                         buf_init  := BufInit,
                         send      := Send,
                         recv      := Recv}) ->
                           ?SEV_ANNOUNCE_START(Handler, 
                                               {ServerSA, Proto, BufInit,
                                                Send, Recv}),
                           ok
                   end},
         #{desc => "await (remote) handler ready",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = _State) ->
                           %% If this fails then we should really skip
                           %% as it has actually nothing to do with the
                           %% test. But for now we just let it crash...
                           %% case ?SEV_AWAIT_READY(Handler, handler, init, 
                           %%                       [{tester, Tester}]) of
                           %%     ok ->
                           %%         ok;
                           %%     {error, Reason} ->
                           %%         ?SEV_EPRINT("Handler failed initiate: "
                           %%                     "~n   ~p", [Reason]),
                           %%         {skip, Reason}
                           %% end
                           ?SEV_AWAIT_READY(Handler, handler, init, 
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (send)",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, 
                                               send, 
                                               [{handler, Handler}])
                   end},
         #{desc => "order handler to continue (send)",
           cmd  => fun(#{handler := Handler,
                         msg     := Msg,
                         num     := Num} = State) ->
                           Data = {Msg, Num},
                           ?SEV_ANNOUNCE_CONTINUE(Handler, send, Data),
                           {ok, maps:remove(data, State)}
                   end},
         #{desc => "await remote handler ready (send)",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = State) ->
                           case ?SEV_AWAIT_READY(Handler, handler, send, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   %% ?SEV_IPRINT("remote client result: "
                                   %%             "~n   ~p", [Result]),
                                   {ok, State#{result => Result}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (send)",
           cmd  => fun(#{tester := Tester, result := Result} = State) ->
                           ?SEV_ANNOUNCE_READY(Tester, send, Result),
                           {ok, maps:remove(result, State)}
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester  := Tester, 
                         handler := Handler} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester,
                                                     [{handler, Handler}]) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "stop (remote) handler",
           cmd  => fun(#{handler := Handler}) ->
                           ?SEV_ANNOUNCE_TERMINATE(Handler),
                           ok
                   end},
         #{desc => "await (remote) handler termination",
           cmd  => fun(#{handler := Handler} = State) ->
                           ?SEV_AWAIT_TERMINATION(Handler),
                           State1 = maps:remove(handler, State),
                           {ok, State1}
                   end},
         #{desc => "stop client node",
           cmd  => fun(#{peer := Peer} = State) ->
                           {ok,
                            try peer:stop(Peer) of
                                ok ->
                                    State#{node_stop => ok};
                                {error, Reason} ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   ~p", [Reason]),
                                    State#{node_stop => error}
                            catch
                                C:E:S ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   Class: ~p"
                                                "~n   Error: ~p"
                                                "~n   Stack: ~p",[C, E, S]),
                                    State#{node_stop => error}
                            end}
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node} = State) ->
                           ?SEV_IPRINT("Success node stop - await nodedown"),
                           receive
                               {nodedown, Node} ->
                                   ?SEV_IPRINT("nodedown received - cleanup"),
                                   State1 = maps:remove(node, State),
                                   {ok, State1}
                           end;
                      (#{node_stop := error} = State) ->
                           ?SEV_IPRINT("Failed node stop - cleanup"),
                           State1 = maps:remove(node, State),
                           {ok, State1}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the server
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Pid} = State) ->
                           {ok, ServerSA} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{server_sa => ServerSA}}
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client    := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, ServerSA),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, init)
                   end},
 
         %% The actual test
         #{desc => "order server continue (recv)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client continue (send)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, send),
                           ok
                   end},
         #{desc => "await client ready (send)",
           cmd  => fun(#{server := Server,
                         client := Client} = State) ->
                           case ?SEV_AWAIT_READY(Client, client, send, 
                                                 [{server, Server}]) of
                               {ok, {_, _, _, _} = Result} ->
                                   ?SEV_IPRINT("client result: "
                                               "~n   ~p", [Result]),
                                   {ok, State#{client_result => Result}};
                               {ok, BadResult} ->
                                   ?SEV_EPRINT("client result: "
                                               "~n   ~p", [BadResult]),
                                   {error, {invalid_client_result, BadResult}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order server continue (close)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await server ready (close)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, server, close)
                   end},
         %% Because of the way we control the server, there is no real 
         %% point in collecting statistics from it (the time will include
         %% our communication with it).
         #{desc => "await server ready (recv)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Server, server, recv,
                                                 [{client, Client}]) of
                               {ok, _Result} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "present result",
           cmd  => fun(#{client_result := CRes,
                         num           := Num} = State) ->
                           {CSent, CReceived, CStart, CStop} = CRes,
                           CTime = tdiff(CStart, CStop),
                           if
                               (CTime =:= 0) ->
                                   {skip,
                                    ?F("Invalid exec time: ~w ", [CTime])};
                               true ->
                                   %% Note that the sizes we are counting is
                                   %% only the "data" part of the messages.
                                   %% There is also fixed header for each
                                   %% message, which of course is small for
                                   %% the large messages, but comparatively
                                   %% big for the small messages!
                                   ?SEV_IPRINT(
                                      "Results: ~w messages exchanged"
                                      "~n   Client: ~w msec"
                                      "~n      ~.2f msec/message (roundtrip)"
                                      "~n      ~.2f messages/msec (roundtrip)"
                                      "~n      ~w bytes/msec sent"
                                      "~n      ~w bytes/msec received",
                                      [Num,
                                       CTime,
                                       CTime / Num,
                                       Num / CTime,
                                       CSent div CTime,
                                       CReceived div CTime]),
                                   State1 = maps:remove(client_result, State),
                                   {ok, State1}
                           end
                   end},

         %% Terminations
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(server, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    i("start server evaluator"),
    ServerInitState = #{domain   => maps:get(domain,   InitState),
                        proto    => maps:get(proto,    InitState),
                        recv     => maps:get(recv,     InitState),
                        send     => maps:get(send,     InitState),
                        buf_init => maps:get(buf_init, InitState)},
    Server = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator(s)"),
    ClientInitState = InitState#{host => local_host()},
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{server => Server#ev.pid,
                        client => Client#ev.pid,
                        num    => maps:get(num, InitState)},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).



%% Server side handler process
%% We don't actually need a separate process for this socket, 
%% but we do it anyway to simplify the sequence.
tpp_udp_server_handler_create() ->
    Self = self(),
    erlang:spawn(fun() -> tpp_udp_server_handler(Self) end).

tpp_udp_server_handler(Parent) ->
    tpp_udp_server_handler_init(Parent),
    {Sock, Send, Recv} = tpp_udp_handler_await_start(Parent),
    tpp_udp_handler_announce_ready(Parent, init),
    tpp_udp_handler_await_continue(Parent, recv),
    Result = tpp_udp_server_handler_msg_exchange(Sock, Send, Recv),
    tpp_udp_handler_announce_ready(Parent, recv, Result),
    Reason = tpp_udp_handler_await_terminate(Parent),
    ?SEV_IPRINT("terminating"),
    exit(Reason).

tpp_udp_server_handler_init(Parent) ->
    put(sname, "shandler"),
    ?SEV_IPRINT("init"),
    _MRef = erlang:monitor(process, Parent),
    ok.

tpp_udp_server_handler_msg_exchange(Sock, Send, Recv) ->
    tpp_udp_server_handler_msg_exchange_loop(Sock, Send, Recv,
                                             0, 0, 0, undefined).

tpp_udp_server_handler_msg_exchange_loop(Sock, Send, Recv, 
                                         N, Sent, Received, Start) ->
    %% ?SEV_IPRINT("[~w] try receive", [N]),
    %% if 
    %%     (N =:= (?TPP_SMALL_NUM-2)) -> 
    %%         ?SEV_IPRINT("[~w] try receive", [N]),
    %%         socket:setopt(Sock, otp, debug, true); 
    %%     true -> ok
    %% end,
    try tpp_udp_recv_req(Sock, Recv) of
        {ok, Msg, RecvSz, From} ->
            NewStart = if (Start =:= undefined) -> ?SLIB:timestamp(); 
                          true -> Start end,
            %% ?SEV_IPRINT("[~w] received - now try send", [N]),
            try tpp_udp_send_rep(Sock, Send, Msg, From) of
                {ok, SendSz} ->
                    tpp_udp_server_handler_msg_exchange_loop(Sock, Send, Recv,
                                                             N+1,
                                                             Sent+SendSz,
                                                             Received+RecvSz,
                                                             NewStart);
                {error, SReason} ->
                    ?SEV_EPRINT("send (~w): ~p", [N, SReason]),
                    exit({send, SReason, N})
	    catch
		SC:SE:SS ->
		    exit({send, {SC, SE, SS}, N})
            end;
        {error, closed} ->
            ?SEV_IPRINT("closed - we are done: ~w, ~w, ~w",
                        [N, Sent, Received]),
            Stop = ?SLIB:timestamp(),
            {N, Sent, Received, Start, Stop};
        {error, RReason} ->
            ?SEV_EPRINT("recv (~w): ~p", [N, RReason]),
            exit({recv, RReason, N})
    catch
	RC:RE:RS ->
	    exit({recv, {RC, RE, RS}, N})	
    end.
  

%% The (remote) client side handler process

tpp_udp_client_handler_create(Node) ->
    Self = self(),
    Fun  = fun() -> put(sname, "chandler"), tpp_udp_client_handler(Self) end,
    erlang:spawn(Node, Fun).

tpp_udp_client_handler(Parent) ->
    tpp_udp_client_handler_init(Parent),
    ?SEV_IPRINT("await start command"),
    {ServerSA, Proto, BufInit, Send, Recv} =
        tpp_udp_handler_await_start(Parent),
    ?SEV_IPRINT("start command with"
                "~n   ServerSA: ~p", [ServerSA]),
    Domain   = maps:get(family, ServerSA),
    ?SEV_IPRINT("try create (domain ~w) socket", [Domain]),
    Sock     = tpp_udp_sock_open(Domain, Proto, BufInit),
    ?SEV_IPRINT("try bind socket"),
    Path     = tpp_udp_sock_bind(Sock, Domain),
    ?SEV_IPRINT("announce ready", []),
    tpp_udp_handler_announce_ready(Parent, init),
    {InitMsg, Num} = tpp_udp_handler_await_continue(Parent, send),
    ?SEV_IPRINT("received continue with"
                "~n   Num: ~p", [Num]),
    Result = tpp_udp_client_handler_msg_exchange(Sock, ServerSA, 
                                                 Send, Recv, InitMsg, Num),
    ?SEV_IPRINT("ready"),
    tpp_udp_handler_announce_ready(Parent, send, Result),
    ?SEV_IPRINT("await terminate"),
    Reason = tpp_udp_handler_await_terminate(Parent),
    ?SEV_IPRINT("terminate with ~p", [Reason]),
    tpp_udp_sock_close(Sock, Path),
    ?SEV_IPRINT("terminating"),
    exit(Reason).

tpp_udp_client_handler_init(Parent) ->
    put(sname, "chandler"),
    ?SEV_IPRINT("init"),
    _MRef = erlang:monitor(process, Parent),
    ok.

tpp_udp_client_handler_msg_exchange(Sock, ServerSA,
                                    Send, Recv, InitMsg, Num) ->
    Start = ?SLIB:timestamp(),
    tpp_udp_client_handler_msg_exchange_loop(Sock, ServerSA,
                                             Send, Recv, InitMsg,
                                             Num, 0, 0, 0, Start).

tpp_udp_client_handler_msg_exchange_loop(_Sock, _Dest, _Send, _Recv, _Msg,
                                         Num, Num, Sent, Received,
                                         Start) ->
    Stop = ?SLIB:timestamp(),
    {Sent, Received, Start, Stop};
tpp_udp_client_handler_msg_exchange_loop(Sock,
					 #{family := local} = Dest,
					 Send, Recv, Data,
                                         Num, N, Sent, Received, Start) ->
    case tpp_udp_send_req(Sock, Send, Data, Dest) of
        {ok, SendSz} ->
            case tpp_udp_recv_rep(Sock, Recv) of
                {ok, NewData, RecvSz, Dest} ->
                    tpp_udp_client_handler_msg_exchange_loop(Sock, Dest,
                                                             Send, Recv,
                                                             NewData, Num, N+1,
                                                             Sent+SendSz, 
                                                             Received+RecvSz, 
                                                             Start);
                {error, RReason} ->
                    ?SEV_EPRINT("recv (~w of ~w): ~p", [N, Num, RReason]),
                    exit({recv, RReason, N})
            end;
        {error, SReason} ->
            ?SEV_EPRINT("send (~w of ~w): ~p", [N, Num, SReason]),
            exit({send, SReason, N})
    end;
tpp_udp_client_handler_msg_exchange_loop(Sock,
					 #{addr := Addr, port := Port} = Dest0,
					 Send, Recv, Data,
                                         Num, N, Sent, Received, Start) ->
    case tpp_udp_send_req(Sock, Send, Data, Dest0) of
        {ok, SendSz} ->
            case tpp_udp_recv_rep(Sock, Recv) of
                {ok, NewData, RecvSz, #{addr := Addr, port := Port} = Dest1} ->
                    tpp_udp_client_handler_msg_exchange_loop(Sock, Dest1,
                                                             Send, Recv,
                                                             NewData, Num, N+1,
                                                             Sent+SendSz, 
                                                             Received+RecvSz, 
                                                             Start);
                {error, RReason} ->
                    ?SEV_EPRINT("recv (~w of ~w): ~p", [N, Num, RReason]),
                    exit({recv, RReason, N})
            end;
        {error, SReason} ->
            ?SEV_EPRINT("send (~w of ~w): ~p", [N, Num, SReason]),
            exit({send, SReason, N})
    end.


tpp_udp_recv_req(Sock, Recv) ->
    tpp_udp_recv(Sock, Recv, ?TPP_REQUEST).

tpp_udp_recv_rep(Sock, Recv) ->
    tpp_udp_recv(Sock, Recv, ?TPP_REPLY).

tpp_udp_recv(Sock, Recv, Tag) ->
    %% ok = socket:setopt(Sock, otp, debug, true),
    try Recv(Sock, 0) of
        {ok, {Source, <<Tag:32/integer, Sz:32/integer, Data/binary>> = Msg}} 
          when (Sz =:= size(Data)) ->
	    %% ok = socket:setopt(Sock, otp, debug, false),
            %% We got it all
            %% ?SEV_IPRINT("tpp_udp_recv -> got all: "
            %%             "~n   Source:     ~p"
            %%             "~n   Tag:        ~p"
            %%             "~n   Sz:         ~p"
            %%             "~n   size(Data): ~p",
            %%             [Source, Tag, Sz, size(Data)]),
            {ok, Data, size(Msg), Source};
        {ok, {_Source, <<Tag:32/integer, Sz:32/integer, Data/binary>>}} ->
	    %% ok = socket:setopt(Sock, otp, debug, false),
            {error, {invalid_msg, Sz, size(Data)}};
        {ok, {_, <<Tag:32/integer, _/binary>>}} ->
	    %% ok = socket:setopt(Sock, otp, debug, false),
            {error, {invalid_msg_tag, Tag}};
        {error, _} = ERROR ->
	    %% ok = socket:setopt(Sock, otp, debug, false),
            ERROR
    catch
	C:E:S ->
	    {error, {caught, C, E, S}}
    end.

tpp_udp_send_req(Sock, Send, Data, Dest) ->
    tpp_udp_send(Sock, Send, ?TPP_REQUEST, Data, Dest).

tpp_udp_send_rep(Sock, Send, Data, Dest) ->
    tpp_udp_send(Sock, Send, ?TPP_REPLY, Data, Dest).

tpp_udp_send(Sock, Send, Tag, Data, Dest) ->
    DataSz = size(Data),
    Msg    = <<Tag:32/integer, DataSz:32/integer, Data/binary>>,
    tpp_udp_send_msg(Sock, Send, Msg, Dest, 0).

tpp_udp_send_msg(Sock, Send, Msg, Dest, AccSz) when is_binary(Msg) ->
    case Send(Sock, Msg, Dest) of
        ok ->
            {ok, AccSz+size(Msg)};
        {ok, Rest} -> % This is an IOVec
            RestBin = list_to_binary(Rest),
            tpp_udp_send_msg(Sock, Send, RestBin, Dest,
                             AccSz+(size(Msg)-size(RestBin)));
        {error, _} = ERROR ->
            ERROR
    end.
    

tpp_udp_handler_await_start(Parent) ->
    ?SEV_IPRINT("await start"),
    ?SEV_AWAIT_START(Parent).

tpp_udp_handler_announce_ready(Parent, Slogan) ->
    ?SEV_IPRINT("announce ready (~p)", [Slogan]),
    ?SEV_ANNOUNCE_READY(Parent, Slogan).
tpp_udp_handler_announce_ready(Parent, Slogan, Extra) ->
    ?SEV_IPRINT("announce ready (~p)", [Slogan]),
    ?SEV_ANNOUNCE_READY(Parent, Slogan, Extra).

tpp_udp_handler_await_continue(Parent, Slogan) ->
    ?SEV_IPRINT("await continue (~p)", [Slogan]),
    case ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan) of
        ok ->
            ?SEV_IPRINT("continue (~p): ok", [Slogan]),
            ok;
        {ok, Data} ->
            ?SEV_IPRINT("continue (~p): ok with data", [Slogan]),
            Data;
        {error, Reason} ->
            ?SEV_EPRINT("continue (~p): error"
                        "~n   ~p", [Slogan, Reason]),
            exit({continue, Slogan, Reason})
    end.

tpp_udp_handler_await_terminate(Parent) ->
    ?SEV_IPRINT("await terminate"),
    case ?SEV_AWAIT_TERMINATE(Parent, parent) of
        ok ->
            ok;
        {error, Reason} ->
            Reason
    end.


tpp_udp_sock_open(Domain, Proto, BufInit) ->
    case socket:open(Domain, dgram, Proto) of
        {ok, Sock} ->
            ok = BufInit(Sock),
            Sock;
        {error, Reason} ->
            exit({open_failed, Reason})
    end.

tpp_udp_sock_bind(Sock, Domain) ->
    LSA = which_local_socket_addr(Domain),
    case socket:bind(Sock, LSA) of
        ok ->
            ok;
        {error, Reason} ->
            exit({bind, Reason})
    end.

tpp_udp_sock_close(Sock, Path) ->
    case socket:close(Sock) of
        ok ->
            unlink_path(Path),
            ok;
        {error, Reason} ->
            ?SEV_EPRINT("Failed closing socket: "
                        "~n   ~p", [Reason]),
            unlink_path(Path),
            {error, {close, Reason}}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Benchmark test cases
%% This test is (currently) very simple. Both parties of the test
%% (server and client) runs in the same (this) node.

-define(TB_IOV_CHUNK(Sz,V), list_to_binary(lists:duplicate((Sz), (V)))).
tb_iov() ->
    IOV0 = 
        [
         ?TB_IOV_CHUNK(8,       16#01),
         ?TB_IOV_CHUNK(16*1024, 16#02),
         ?TB_IOV_CHUNK(256,     16#03),
         ?TB_IOV_CHUNK(8*1024,  16#04),
         ?TB_IOV_CHUNK(512,     16#05),
         ?TB_IOV_CHUNK(1*1024,  16#06),
         ?TB_IOV_CHUNK(1*1024,  16#07),
         ?TB_IOV_CHUNK(1*1024,  16#08),
         ?TB_IOV_CHUNK(1*1024,  16#09),
         ?TB_IOV_CHUNK(1*1024,  16#0A),
         ?TB_IOV_CHUNK(1*1024,  16#0B),
         ?TB_IOV_CHUNK(1*1024,  16#0C),
         ?TB_IOV_CHUNK(16,      16#0D),
         ?TB_IOV_CHUNK(256,     16#0E),
         ?TB_IOV_CHUNK(16*1024, 16#0F),
         ?TB_IOV_CHUNK(32,      16#10),
         ?TB_IOV_CHUNK(8,       16#11),
         ?TB_IOV_CHUNK(128,     16#12),
         ?TB_IOV_CHUNK(2*1024,  16#13),
         ?TB_IOV_CHUNK(16,      16#14),
         ?TB_IOV_CHUNK(32,      16#15),
         ?TB_IOV_CHUNK(64,      16#16),
         ?TB_IOV_CHUNK(4*1024,  16#17)     
        ],
    IOV1 = lists:flatten([begin
                              Sz = byte_size(B),
                              [<<Sz:32/integer>>, B]
                          end || B <- IOV0]),
    TSz = iolist_size(IOV1),
    [<<TSz:32/integer>> | IOV1].

tb_runtime(Config) ->
    ?SEV_IPRINT("~w -> entry with"
                "~n   Config: ~p", [?FUNCTION_NAME, Config]),
    case proplists:get_value(category, Config, standard) of
        bench ->
            ?MINS(1);
        standard ->
            ?SECS(10)
    end.

traffic_bench_sendv_and_recv_tcp4(Config) when is_list(Config) ->
    RunTime = tb_runtime(Config),
    ?TT(RunTime + ?MINS(1)),
    IOV = tb_iov(),
    Send = fun(S, Data) when is_list(Data) ->
                   socket:sendv(S, Data)
           end,
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{bname    => sendv_inet4,
                                 domain   => inet,
                                 send     => Send,
                                 iov      => IOV,
                                 run_time => RunTime},
                   do_traffic_bench_send_and_recv(InitState)
           end).

traffic_bench_send_and_recv_tcp4(Config) when is_list(Config) ->
    RunTime = tb_runtime(Config),
    ?TT(RunTime + ?MINS(1)),
    IOV = tb_iov(),
    Send = fun(S, Data) when is_list(Data) ->
                   socket:send(S, iolist_to_binary(Data))
           end,
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{bname    => send_inet4,
                                 domain   => inet,
                                 send     => Send,
                                 iov      => IOV,
                                 run_time => RunTime},
                   do_traffic_bench_send_and_recv(InitState)
           end).

traffic_bench_sendv_and_recv_tcp6(Config) when is_list(Config) ->
    RunTime = tb_runtime(Config),
    ?TT(RunTime + ?MINS(1)),
    IOV = tb_iov(),
    Send = fun(S, Data) when is_list(Data) ->
                   socket:sendv(S, Data)
           end,
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{bname    => sendv_inet6,
                                 domain   => inet6,
                                 send     => Send,
                                 iov      => IOV,
                                 run_time => RunTime},
                   do_traffic_bench_send_and_recv(InitState)
           end).

traffic_bench_send_and_recv_tcp6(Config) when is_list(Config) ->
    RunTime = tb_runtime(Config),
    ?TT(RunTime + ?MINS(1)),
    IOV = tb_iov(),
    Send = fun(S, Data) when is_list(Data) ->
                   socket:send(S, iolist_to_binary(Data))
           end,
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{bname    => send_inet6,
                                 domain   => inet6,
                                 send     => Send,
                                 iov      => IOV,
                                 run_time => RunTime},
                   do_traffic_bench_send_and_recv(InitState)
           end).

traffic_bench_sendv_and_recv_tcpL(Config) when is_list(Config) ->
    RunTime = tb_runtime(Config),
    ?TT(RunTime + ?MINS(1)),
    IOV = tb_iov(),
    Send = fun(S, Data) when is_list(Data) ->
                   socket:sendv(S, Data)
           end,
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   InitState = #{bname    => sendv_local,
                                 domain   => local,
                                 send     => Send,
                                 iov      => IOV,
                                 run_time => RunTime},
                   do_traffic_bench_send_and_recv(InitState)
           end).

traffic_bench_send_and_recv_tcpL(Config) when is_list(Config) ->
    RunTime = tb_runtime(Config),
    ?TT(RunTime + ?MINS(1)),
    IOV = tb_iov(),
    Send = fun(S, Data) when is_list(Data) ->
                   socket:send(S, iolist_to_binary(Data))
           end,
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   InitState = #{bname    => send_local,
                                 domain   => local,
                                 send     => Send,
                                 iov      => IOV,
                                 run_time => RunTime},
                   do_traffic_bench_send_and_recv(InitState)
           end).

do_traffic_bench_send_and_recv(#{bname    := BName,
                                 run_time := RTime} = InitState) ->
    ?SEV_IPRINT("[ctrl] start server"),
    {PathOrPort, Server} = tb_server_start(InitState),
    ?SEV_IPRINT("[ctrl] start client"),
    Client               = tb_client_start(InitState, PathOrPort),
    TRef = erlang:start_timer(RTime, self(), tb_timeout),
    ?SEV_IPRINT("[ctrl] await completion"),
    tb_await_completion(BName, Server, Client, TRef).

tb_await_completion(BName,
                    {ServerPid, ServerMRef} = Server,
                    {ClientPid, ClientMRef} = Client,
                    TRef) ->
    receive
        {timeout, TRef, tb_timeout} ->
            ?SEV_IPRINT("[ctrl] bench timeout received - begin termination"),
            ClientPid ! {self(), stop},
            tb_await_termination(BName, Server, Client);
        {'DOWN', ClientMRef, process, ClientPid, ClientReason} ->
            ?SEV_EPRINT("[ctrl] received unexpected client down: "
                        "~n   ~p", [ClientReason]),
            erlang:cancel_timer(TRef),
            exit(ClientPid, kill),
            exit(ClientReason);
        {'DOWN', ServerMRef, process, ServerPid, ServerReason} ->
            ?SEV_EPRINT("[ctrl] received unexpected server down: "
                        "~n   ~p", [ServerReason]),
            erlang:cancel_timer(TRef),
            exit(ClientPid, kill),
            exit(ServerReason)
    end.

tb_await_termination(BName, Server, Client) ->
    tb_await_termination(BName, Server, Client, undefined).

-define(BENCH_EVENT(__N__, __V__),
        #event{name = (__N__),
               data = [{suite, atom_to_list(?MODULE)},
                       {value, (__V__)}]}).

tb_await_termination(BName,
                     {ServerPid, ServerMRef} = Server,
                     {ClientPid, ClientMRef} = Client,
                     undefined = Comment) ->
    %% ?SEV_IPRINT("[ctrl] await client and server down"),
    receive
        {'DOWN', ClientMRef, process, ClientPid, {done, {Exchange, UnitStr}}} ->
            ?SEV_IPRINT("[ctrl] "
                        "received (expected) down from client with result"),
            ct_event:notify( ?BENCH_EVENT(BName, Exchange) ),
            ?SEV_IPRINT("[ctrl] await server termination"),
            NewComment = {comment, ?F("~p ~s", [Exchange, UnitStr])},
            tb_await_termination(BName,
                                 Server, undefined, NewComment);
        {'DOWN', ClientMRef, process, ClientPid, ClientReason} ->
            ?SEV_EPRINT("[ctrl] unexpected termination from client: "
                        "~n   ~p", [ClientReason]),
            exit(ServerPid, kill),
            exit(ClientReason);
        {'DOWN', ServerMRef, process, ServerPid, ServerReason} ->
            ?SEV_IPRINT("[ctrl] received down from server: "
                        "~n   ~p", [ServerReason]),
            tb_await_termination(BName,
                                 undefined, Client, Comment)
    %% after 1000 ->
    %%         ?SEV_IPRINT("[ctrl] timeout waiting for client and/or server exit:"
    %%                     "~n   MQueue: ~p", [?SLIB:pi(messages)]),
    %%         tb_await_termination(BName, Server, Client, Comment)
    end;
tb_await_termination(_BName,
                     {ServerPid, ServerMRef} = _Server,
                     undefined,
                     Result) ->
    %% ?SEV_IPRINT("[ctrl] await server down"),
    receive
        {'DOWN', ServerMRef, process, ServerPid, _} ->
            ?SEV_IPRINT("[ctrl] received (expected) down from server - "
                        "we are done"),
            Result
    end;
tb_await_termination(BName,
                     undefined,
                     {ClientPid, ClientMRef} = _Client,
                     undefined = _Result) ->
    %% ?SEV_IPRINT("[ctrl] await client down (with result)"),
    receive
        {'DOWN', ClientMRef, process, ClientPid, {done, {Exchange, UnitStr}}} ->
            ?SEV_IPRINT("[ctrl] received down from client - we are done"),
            ct_event:notify( ?BENCH_EVENT(BName, Exchange) ),
            {comment, ?F("~p ~s", [Exchange, UnitStr])};
        {'DOWN', ClientMRef, process, ClientPid, ClientReason} ->
            ?SEV_EPRINT("[ctrl] unexpected termination from client: "
                        "~n   ~p", [ClientReason]),
            exit(ClientReason)
    end.
    

tb_server_start(#{domain := Fam,
                  send   := Send}) ->
    Self = self(),
    Server = {Pid, MRef} =
        spawn_monitor(fun() ->
                              tb_server_init(#{parent => Self,
                                               domain => Fam,
                                               send   => Send})
                      end),
    receive
        {Pid, PathOrPort} ->
            ?SEV_IPRINT("[ctrl] server started: ~p", [PathOrPort]),
            {PathOrPort, Server};
        {'DOWN', MRef, process, Pid, Info} ->
            ?SEV_EPRINT("[ctrl] server start failure: "
                        "~n   ~p", [Info]),
            exit({tb_server_start, Info})
    end.

tb_decode(Bin) when is_binary(Bin) ->
    tb_decode(Bin, []).

tb_decode(<<>>, Acc) ->
    lists:reverse(Acc);
tb_decode(<<Sz:32/integer, Data:Sz/binary, Rest/binary>>, Acc) ->
    tb_decode(Rest, [Data, <<Sz:32/integer>> | Acc]).

tb_server_init(#{parent := Pid, domain := Fam} = State) ->
    ?SEV_IPRINT("[server] initiate"),
    SA                    = which_local_socket_addr(Fam),
    {ok, LS}              = socket:open(Fam, stream),
    ok                    = socket:bind(LS, SA),
    ok                    = socket:listen(LS),
    case SA of
         #{path := Path} ->
            Pid ! {self(), {path, Path}};
        _ ->
            {ok, #{port := Port}} = socket:sockname(LS),
            Pid ! {self(), {port, Port}}
    end,
    ?SEV_IPRINT("[server] ready for client connect"),
    {ok, AS}              = socket:accept(LS),
    ?SEV_IPRINT("[server] client connected - test started"),
    tb_server_loop(State#{listen => LS,
                          accept => AS}).

%% Make it simple: The data begins with a 4 byte size, so read that,
%% and then that amount of data.
tb_server_loop(#{listen := LS, accept := AS, send := Send} = State) ->
    case socket:recv(AS, 4) of
        {ok, <<Sz:32/integer>> = SzBin} ->
            case socket:recv(AS, Sz) of
                {ok, Data} ->
                    IOV = tb_decode(Data),
                    case Send(AS, [SzBin | IOV]) of
                        ok ->
                            tb_server_loop(State);
                        {error, SReason} ->
                            ?SEV_EPRINT("[server] unexpected send error:"
                                        "~n   ~p", [SReason]),
                            (catch socket:close(LS)),
                            (catch socket:close(AS)),
                            exit({tb_server_send, SReason})
                    end;
                {error, R2Reason} ->
                    ?SEV_EPRINT("[server] unexpected read (data) error:"
                                "~n   ~p", [R2Reason]),
                    (catch socket:close(LS)),
                    (catch socket:close(AS)),
                    exit({tb_server_recv2, R2Reason})
            end;
        {error, closed} ->
            ?SEV_IPRINT("[server] socket closed => terminate"),
            (catch socket:close(LS)),
            (catch socket:close(AS)),
            exit(normal);
        {error, R1Reason} ->
            ?SEV_EPRINT("[server] unexpected read (sz) error:"
                        "~n   ~p", [R1Reason]),
            (catch socket:close(LS)),
            (catch socket:close(AS)),
            exit({tb_server_recv1, R1Reason})
    end.                    


tb_client_start(#{domain := Fam,
                  send   := Send,
                  iov    := IOV}, PathOrPort) ->
    Self = self(),
    Client = {Pid, MRef} =
        spawn_monitor(fun() ->
                              tb_client_init(#{parent       => Self,
                                               domain       => Fam,
                                               send         => Send,
                                               iov          => IOV,
                                               path_or_port => PathOrPort})
                      end),
    receive
        {Pid, ok} ->
            Client;
        {'DOWN', MRef, process, Pid, Info} ->
            ?SEV_EPRINT("client start failure: "
                        "~n   ~p", [Info]),
            exit({tb_client_start, Info})
    end.

tb_client_init(#{parent       := Pid,
                 domain       := Fam,
                 path_or_port := PathOrPort,
                 send         := Send,
                 iov          := IOV}) ->
    ?SEV_IPRINT("[client] initiate"),
    SA       = which_local_socket_addr(Fam),
    {ok, CS} = socket:open(Fam, stream),
    ok       = socket:bind(CS, SA),
    SSA = case PathOrPort of
              {path, Path} ->
                  SA#{path => Path};
              {port, Port} ->
                  SA#{port => Port}
          end,
    ok       = socket:connect(CS, SSA),
    Pid ! {self(), ok},
    ?SEV_IPRINT("[client] connected to server - begin test"),
    tb_client_loop(Pid, CS, Send, IOV, ts(), 0, 0).

tb_client_loop(Pid, Sock, Send, Data0, TStart, ARcv0, N0) ->
    case Send(Sock, Data0) of
        ok ->
            case socket:recv(Sock, 4) of
                {ok, <<Sz:32/integer>> = SzBin} ->
                    case socket:recv(Sock, Sz) of
                        {ok, Data} ->
                            IOV0 = tb_decode(Data),
                            case tb_client_is_done(Pid) of
                                true ->
                                    TStop = ts(),
                                    TDiff = TStop - TStart,
                                    ARcv  = ARcv0 + 4 + byte_size(Data),
                                    {Exchange, UnitStr} = Res =
                                        case ARcv div TDiff of
                                            E when (E > 1024) ->
                                                {E div 1024,
                                                 "kb/msec"};
                                            E ->
                                                {E, "b/msec"}
                                        end,
                                    N     = N0 + 1,
                                    ?SEV_IPRINT("[client] test result:"
                                                "~n   TDiff:      ~w msec"
                                                "~n   Data:       ~w bytes"
                                                "~n   Exchange:   ~w ~s"
                                                "~n   Iterations: ~w",
                                                [TDiff, ARcv,
                                                 Exchange, UnitStr,
                                                 N]),
                                    (catch socket:close(Sock)),
                                    exit({done, Res});
                                false ->
                                    IOV = [SzBin | IOV0],
                                    tb_client_loop(Pid,
                                                   Sock, Send, IOV,
                                                   TStart,
                                                   ARcv0 + 4 + byte_size(Data),
                                                   N0+1)
                            end;
                        {error, R2Reason} ->
                            ?SEV_EPRINT("[client] unexpected read (data) error:"
                                        "~n   ~p", [R2Reason]),
                            (catch socket:close(Sock)),
                            exit({tb_client_recv2, R2Reason})
                    end;
                {error, R1Reason} ->
                    ?SEV_EPRINT("[client] unexpected read (sz) error:"
                                "~n   ~p", [R1Reason]),
                    (catch socket:close(Sock)),
                    exit({tb_client_recv1, R1Reason})
            end;
        {error, SReason} ->
            ?SEV_EPRINT("[client] unexpected send error:"
                        "~n   ~p", [SReason]),
            (catch socket:close(Sock)),
            exit({tb_client_send, SReason})
    end.                    

tb_client_is_done(Pid) ->
    receive
        {Pid, stop} ->
            ?SEV_IPRINT("[client] received stop command - test is over"),
            true
    after 0 ->
            false
end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sock_bind(Sock, LSA) ->
    try socket:bind(Sock, LSA) of
        ok = OK ->
            OK;
        {error, eaddrnotavail = Reason} ->
            ?SEV_IPRINT("Address not available"),
            throw({skip, Reason});
        {error, _} = ERROR ->
            ERROR
    catch
        C:E:S ->
            ?FAIL({bind, C, E, S})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local_host() ->
    try net_adm:localhost() of
        Host when is_list(Host) ->
	    %% Convert to shortname if long
	    case string:tokens(Host, [$.]) of
		[H|_] ->
		    list_to_atom(H)
	    end
    catch
        C:E:S ->
            erlang:raise(C, E, S)
    end.


%% The point of this is to "ensure" that paths from different test runs
%% don't clash.

mk_unique_path() ->
    ?SLIB:mk_unique_path().


which_local_socket_addr(local = Domain) ->
    #{family => Domain,
      path   => mk_unique_path()};

%% This gets the local socket address (not 127.0...)
%% We should really implement this using the (new) net module,
%% but until that gets the necessary functionality...
which_local_socket_addr(Domain) ->
    case ?KLIB:which_local_host_info(Domain) of
        {ok, [#{addr := Addr}|_]} ->
            #{family => Domain,
              addr   => Addr};
        {error, no_address = Reason} ->
            ?SEV_IPRINT("failed get (valid) local host address: ~w"
                        "~n   Host Info: "
                        "~n      ~p",
                        [case net:getifaddrs(Domain) of
                             {ok, Info} ->
                                 Info;
                             _ ->
                                 undefined
                         end]),
            ?FAIL(Reason);
        {error, Reason} ->
            ?FAIL(Reason)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Here are all the *general* test case condition functions.

etc_issue() ->
    string:trim(os:cmd("cat /etc/issue")).

is_old_fedora16() ->
    is_old_fedora16( etc_issue() ).

%% We actually only have one host running this, a slow VM.
is_old_fedora16("Fedora release 16 " ++ _) ->
    skip("Very slow VM");
is_old_fedora16(_) ->
    ok.


%% This is a bit subjective, but...
%% ..we have some WMs that is not "fast enough", but the only
%% thing we can test on is 'esock factor' (other than host name).
%% This means we actually skip this on platforms where its
%% not actually needed.
%% The host in question is a Ubuntu 20.04...
is_slow_ubuntu(Config) ->
    case lookup(kernel_factor, 1, Config) of
	F when is_integer(F) andalso (F > 1) ->
	    case os:type() of
		{unix, linux} ->
		    case etc_issue() of
			"Ubuntu 20.04" ++ _ ->
			    skip("Slow Ubuntu host");
			_ ->
			    ok
		    end;
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end.


is_not_windows() ->
    case os:type() of
        {win32, nt} ->
            skip("This does not work on Windows");
        _ ->
            ok
    end.

is_not_flakey_os() ->
    is_not_flakey_os(erlang:system_info(system_architecture)).

is_not_flakey_os("sparc-sun-solaris2.10") ->
    skip('flakey os');
is_not_flakey_os("x86_64-pc-solaris2.11") ->
    skip('flakey os');
is_not_flakey_os(_) ->
    ok.


has_support_unix_domain_socket() ->
    case socket:is_supported(local) of
	true ->
	    ok;
	false ->
	    skip("Not supported")
    end.

has_support_sctp() ->
    case os:type() of
        {win32, _} ->
            skip("Not supported");
        {unix, netbsd} ->
            %% XYZ We will have to investigate this later...
            skip("Not supported");
        _ ->
            case socket:is_supported(sctp) of
                true ->
                    ok;
                false ->
                    skip("Not supported")
            end
    end.


%% The idea is that this function shall test if the test host has 
%% support for IPv4 or IPv6. If not, there is no point in running corresponding tests.
%% Currently we just skip.
has_support_ipv4() ->
    ?KLIB:has_support_ipv4().

has_support_ipv6() ->
    ?KLIB:has_support_ipv6().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unlink_path(Path) ->
    unlink_path(Path, fun() -> ok end, fun() -> ok end).

unlink_path(Path, Success, Failure)
  when is_function(Success, 0), is_function(Failure, 0) ->
    case Path of
        undefined ->
            ?SEV_IPRINT("not a path to unlink"),
                    Success();
        _ ->
            ?SEV_IPRINT("try unlink path: "
                        "~n   ~s", [Path]),
            case file:delete(Path) of
                ok ->
                    ?SEV_IPRINT("path unlinked: "
                                "~n   Path: ~s", [Path]),
                    Success();
                Error ->
                    ?SEV_EPRINT("unlink failed: "
                                "~n   Path: ~s"
                                "~n   Res:  ~p", [Path, Error]),
                    Failure()
            end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% not_supported(What) ->
%%     skip({not_supported, What}).

%% not_yet_implemented() ->
%%     skip("not yet implemented").

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup(Key, Default, Config) ->
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Value}} ->
            Value;
        _ ->
            Default
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tdiff({A1, B1, C1} = _T1x, {A2, B2, C2} = _T2x) ->
    T1 = A1*1000000000+B1*1000+(C1 div 1000), 
    T2 = A2*1000000000+B2*1000+(C2 div 1000), 
    T2 - T1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tc_try(Case, TCCondFun, TCFun) ->
    ?TC_TRY(Case, TCCondFun, TCFun).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_node(Name) ->
    start_node(Name, 5000).

start_node(Name, Timeout) when is_integer(Timeout) andalso (Timeout > 0) ->
    Pa   = filename:dirname(code:which(?MODULE)),
    Args = ["-pa", Pa,
            "-s", atom_to_list(?PROXY), "start", atom_to_list(node()),
            "-s", "global", "sync"],
    try ?CT_PEER(#{name      => Name,
                   wait_boot => Timeout,
                   args      => Args}) of
        {ok, Peer, Node} ->
            ?SEV_IPRINT("Started node ~p - now (global) sync", [Name]),
            global:sync(), % Again, just in case...
            ?SEV_IPRINT("ping proxy"),
            pong = ?PPING(Node),
            {Peer, Node};
        {error, Reason} ->
            ?SEV_EPRINT("failed starting node ~p (=> SKIP):"
                        "~n   ~p", [Name, Reason]),
            skip(Reason)
    catch
        Class:Reason:Stack ->
            ?SEV_EPRINT("Failed starting node: "
                        "~n   Class:  ~p"
                        "~n   Reason: ~p"
                        "~n   Stack:  ~p",
                        [Class, Reason, Stack]),
            skip({node_start, Class, Reason})
    end.

            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sock_port(S) ->
    case socket:sockname(S) of
        {ok, #{port := Port}} -> Port;
        {ok, #{}}             -> undefined
    end.

l2b(L) when is_list(L) ->
    list_to_binary(L).

b2l(B) when is_binary(B) ->
    binary_to_list(B).

i(F) ->
    i(F, []).

i(F, A) ->
    FStr = ?F("[~s] " ++ F, [?FTS()|A]),
    io:format(user, FStr ++ "~n", []),
    io:format(FStr, []).

ts() ->
    erlang:system_time(millisecond).
