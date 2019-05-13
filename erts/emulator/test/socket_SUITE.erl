%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2019. All Rights Reserved.
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

%% There are some environment variables that can be used to "manipulate"
%% the test suite: 
%%
%% Variable that controls which 'groups' are to run (with default values)
%%
%%         ESOCK_TEST_API:         include
%%         ESOCK_TEST_SOCK_CLOSE:  include
%%         ESOCK_TEST_TRAFFIC:     include
%%         ESOCK_TEST_TTEST:       exclude
%%
%% Variable that controls "verbosity" of the test case(s):
%%
%%         ESOCK_TEST_QUIET: true (default) | false
%%
%% Defines the runtime of the ttest cases
%% (This is the time during which "measurement" is performed. 
%%  the actual time it takes for the test case to complete
%%  will be longer; setup, completion, ...)
%%
%%          ESOCK_TEST_TTEST_RUNTIME: 10 seconds
%%              Format of values: <integer>[<unit>]
%%              Where unit is: ms | s | m
%%                 ms - milli seconds
%%                 s  - seconds (default)
%%                 m  - minutes
%%

%% Run the entire test suite: 
%% ts:run(emulator, socket_SUITE, [batch]).
%%
%% Run a specific group:
%% ts:run(emulator, socket_SUITE, {group, foo}, [batch]).
%%
%% Run a specific test case:
%% ts:run(emulator, socket_SUITE, foo, [batch]).

-module(socket_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

%% Suite exports
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
         %% *** API Basic ***
         api_b_open_and_close_udp4/1,
         api_b_open_and_close_tcp4/1,
         api_b_open_and_close_udpL/1,
         api_b_open_and_close_tcpL/1,
         api_b_sendto_and_recvfrom_udp4/1,
         api_b_sendto_and_recvfrom_udpL/1,
         api_b_sendmsg_and_recvmsg_udp4/1,
         api_b_send_and_recv_tcp4/1,
         api_b_send_and_recv_tcpL/1,
         api_b_sendmsg_and_recvmsg_tcp4/1,
         api_b_sendmsg_and_recvmsg_tcpL/1,

         %% *** API Options ***
         api_opt_simple_otp_options/1,
         api_opt_simple_otp_rcvbuf_option/1,
         api_opt_simple_otp_controlling_process/1,

         %% *** API Operation Timeout ***
         api_to_connect_tcp4/1,
         api_to_connect_tcp6/1,
         api_to_accept_tcp4/1,
         api_to_accept_tcp6/1,
         api_to_maccept_tcp4/1,
         api_to_maccept_tcp6/1,
         api_to_send_tcp4/1,
         api_to_send_tcp6/1,
         api_to_sendto_udp4/1,
         api_to_sendto_udp6/1,
         api_to_sendmsg_tcp4/1,
         api_to_sendmsg_tcp6/1,
         api_to_recv_udp4/1,
         api_to_recv_udp6/1,
         api_to_recv_tcp4/1,
         api_to_recv_tcp6/1,
         api_to_recvfrom_udp4/1,
         api_to_recvfrom_udp6/1,
         api_to_recvmsg_udp4/1,
         api_to_recvmsg_udp6/1,
         api_to_recvmsg_tcp4/1,
         api_to_recvmsg_tcp6/1,

         %% *** Socket Closure ***
         sc_cpe_socket_cleanup_tcp4/1,
         sc_cpe_socket_cleanup_tcp6/1,
         sc_cpe_socket_cleanup_udp4/1,
         sc_cpe_socket_cleanup_udp6/1,

         sc_lc_recv_response_tcp4/1,
         sc_lc_recv_response_tcp6/1,
         sc_lc_recvfrom_response_udp4/1,
         sc_lc_recvfrom_response_udp6/1,
         sc_lc_recvmsg_response_tcp4/1,
         sc_lc_recvmsg_response_tcp6/1,
         sc_lc_recvmsg_response_udp4/1,
         sc_lc_recvmsg_response_udp6/1,
         sc_lc_acceptor_response_tcp4/1,
         sc_lc_acceptor_response_tcp6/1,

         sc_rc_recv_response_tcp4/1,
         sc_rc_recv_response_tcp6/1,
         sc_rc_recvmsg_response_tcp4/1,
         sc_rc_recvmsg_response_tcp6/1,

         sc_rs_recv_send_shutdown_receive_tcp4/1,
         sc_rs_recv_send_shutdown_receive_tcp6/1,
         sc_rs_recvmsg_send_shutdown_receive_tcp4/1,
         sc_rs_recvmsg_send_shutdown_receive_tcp6/1,

         %% *** Traffic ***
         traffic_send_and_recv_chunks_tcp4/1,
         traffic_send_and_recv_chunks_tcp6/1,

         traffic_ping_pong_small_send_and_recv_tcp4/1,
         traffic_ping_pong_small_send_and_recv_tcp6/1,
         traffic_ping_pong_medium_send_and_recv_tcp4/1,
         traffic_ping_pong_medium_send_and_recv_tcp6/1,
         traffic_ping_pong_large_send_and_recv_tcp4/1,
         traffic_ping_pong_large_send_and_recv_tcp6/1,

         traffic_ping_pong_small_sendto_and_recvfrom_udp4/1,
         traffic_ping_pong_small_sendto_and_recvfrom_udp6/1,
         traffic_ping_pong_medium_sendto_and_recvfrom_udp4/1,
         traffic_ping_pong_medium_sendto_and_recvfrom_udp6/1,

         traffic_ping_pong_small_sendmsg_and_recvmsg_tcp4/1,
         traffic_ping_pong_small_sendmsg_and_recvmsg_tcp6/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp4/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp6/1,
         traffic_ping_pong_large_sendmsg_and_recvmsg_tcp4/1,
         traffic_ping_pong_large_sendmsg_and_recvmsg_tcp6/1,

         traffic_ping_pong_small_sendmsg_and_recvmsg_udp4/1,
         traffic_ping_pong_small_sendmsg_and_recvmsg_udp6/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_udp4/1,
         traffic_ping_pong_medium_sendmsg_and_recvmsg_udp6/1,

         %% *** Time Test ***
         %% Server: transport = gen_tcp, active = false
         %% Client: transport = gen_tcp
         ttest_sgenf_cgenf_small_tcp4/1,
         ttest_sgenf_cgenf_small_tcp6/1,
         ttest_sgenf_cgenf_medium_tcp4/1,
         ttest_sgenf_cgenf_medium_tcp6/1,
         ttest_sgenf_cgenf_large_tcp4/1,
         ttest_sgenf_cgenf_large_tcp6/1,

         ttest_sgenf_cgeno_small_tcp4/1,
         ttest_sgenf_cgeno_small_tcp6/1,
         ttest_sgenf_cgeno_medium_tcp4/1,
         ttest_sgenf_cgeno_medium_tcp6/1,
         ttest_sgenf_cgeno_large_tcp4/1,
         ttest_sgenf_cgeno_large_tcp6/1,

         ttest_sgenf_cgent_small_tcp4/1,
         ttest_sgenf_cgent_small_tcp6/1,
         ttest_sgenf_cgent_medium_tcp4/1,
         ttest_sgenf_cgent_medium_tcp6/1,
         ttest_sgenf_cgent_large_tcp4/1,
         ttest_sgenf_cgent_large_tcp6/1,

         %% Server: transport = gen_tcp, active = false
         %% Client: transport = socket(tcp)
         ttest_sgenf_csockf_small_tcp4/1,
         ttest_sgenf_csockf_small_tcp6/1,
         ttest_sgenf_csockf_medium_tcp4/1,
         ttest_sgenf_csockf_medium_tcp6/1,
         ttest_sgenf_csockf_large_tcp4/1,
         ttest_sgenf_csockf_large_tcp6/1,

         ttest_sgenf_csocko_small_tcp4/1,
         ttest_sgenf_csocko_small_tcp6/1,
         ttest_sgenf_csocko_medium_tcp4/1,
         ttest_sgenf_csocko_medium_tcp6/1,
         ttest_sgenf_csocko_large_tcp4/1,
         ttest_sgenf_csocko_large_tcp6/1,

         ttest_sgenf_csockt_small_tcp4/1,
         ttest_sgenf_csockt_small_tcp6/1,
         ttest_sgenf_csockt_medium_tcp4/1,
         ttest_sgenf_csockt_medium_tcp6/1,
         ttest_sgenf_csockt_large_tcp4/1,
         ttest_sgenf_csockt_large_tcp6/1,

         %% Server: transport = gen_tcp, active = once
         %% Client: transport = gen_tcp
         ttest_sgeno_cgenf_small_tcp4/1,
         ttest_sgeno_cgenf_small_tcp6/1,
         ttest_sgeno_cgenf_medium_tcp4/1,
         ttest_sgeno_cgenf_medium_tcp6/1,
         ttest_sgeno_cgenf_large_tcp4/1,
         ttest_sgeno_cgenf_large_tcp6/1,

         ttest_sgeno_cgeno_small_tcp4/1,
         ttest_sgeno_cgeno_small_tcp6/1,
         ttest_sgeno_cgeno_medium_tcp4/1,
         ttest_sgeno_cgeno_medium_tcp6/1,
         ttest_sgeno_cgeno_large_tcp4/1,
         ttest_sgeno_cgeno_large_tcp6/1,

         ttest_sgeno_cgent_small_tcp4/1,
         ttest_sgeno_cgent_small_tcp6/1,
         ttest_sgeno_cgent_medium_tcp4/1,
         ttest_sgeno_cgent_medium_tcp6/1,
         ttest_sgeno_cgent_large_tcp4/1,
         ttest_sgeno_cgent_large_tcp6/1,

         %% Server: transport = gen_tcp, active = once
         %% Client: transport = socket(tcp)
         ttest_sgeno_csockf_small_tcp4/1,
         ttest_sgeno_csockf_small_tcp6/1,
         ttest_sgeno_csockf_medium_tcp4/1,
         ttest_sgeno_csockf_medium_tcp6/1,
         ttest_sgeno_csockf_large_tcp4/1,
         ttest_sgeno_csockf_large_tcp6/1,

         ttest_sgeno_csocko_small_tcp4/1,
         ttest_sgeno_csocko_small_tcp6/1,
         ttest_sgeno_csocko_medium_tcp4/1,
         ttest_sgeno_csocko_medium_tcp6/1,
         ttest_sgeno_csocko_large_tcp4/1,
         ttest_sgeno_csocko_large_tcp6/1,

         ttest_sgeno_csockt_small_tcp4/1,
         ttest_sgeno_csockt_small_tcp6/1,
         ttest_sgeno_csockt_medium_tcp4/1,
         ttest_sgeno_csockt_medium_tcp6/1,
         ttest_sgeno_csockt_large_tcp4/1,
         ttest_sgeno_csockt_large_tcp6/1,

         %% Server: transport = gen_tcp, active = true
         %% Client: transport = gen_tcp
         ttest_sgent_cgenf_small_tcp4/1,
         ttest_sgent_cgenf_small_tcp6/1,
         ttest_sgent_cgenf_medium_tcp4/1,
         ttest_sgent_cgenf_medium_tcp6/1,
         ttest_sgent_cgenf_large_tcp4/1,
         ttest_sgent_cgenf_large_tcp6/1,

         ttest_sgent_cgeno_small_tcp4/1,
         ttest_sgent_cgeno_small_tcp6/1,
         ttest_sgent_cgeno_medium_tcp4/1,
         ttest_sgent_cgeno_medium_tcp6/1,
         ttest_sgent_cgeno_large_tcp4/1,
         ttest_sgent_cgeno_large_tcp6/1,

         ttest_sgent_cgent_small_tcp4/1,
         ttest_sgent_cgent_small_tcp6/1,
         ttest_sgent_cgent_medium_tcp4/1,
         ttest_sgent_cgent_medium_tcp6/1,
         ttest_sgent_cgent_large_tcp4/1,
         ttest_sgent_cgent_large_tcp6/1,

         %% Server: transport = gen_tcp, active = true
         %% Client: transport = socket(tcp)
         ttest_sgent_csockf_small_tcp4/1,
         ttest_sgent_csockf_small_tcp6/1,
         ttest_sgent_csockf_medium_tcp4/1,
         ttest_sgent_csockf_medium_tcp6/1,
         ttest_sgent_csockf_large_tcp4/1,
         ttest_sgent_csockf_large_tcp6/1,

         ttest_sgent_csocko_small_tcp4/1,
         ttest_sgent_csocko_small_tcp6/1,
         ttest_sgent_csocko_medium_tcp4/1,
         ttest_sgent_csocko_medium_tcp6/1,
         ttest_sgent_csocko_large_tcp4/1,
         ttest_sgent_csocko_large_tcp6/1,

         ttest_sgent_csockt_small_tcp4/1,
         ttest_sgent_csockt_small_tcp6/1,
         ttest_sgent_csockt_medium_tcp4/1,
         ttest_sgent_csockt_medium_tcp6/1,
         ttest_sgent_csockt_large_tcp4/1,
         ttest_sgent_csockt_large_tcp6/1,

         %% Server: transport = socket(tcp), active = false
         %% Client: transport = gen_tcp
         ttest_ssockf_cgenf_small_tcp4/1,
         ttest_ssockf_cgenf_small_tcp6/1,
         ttest_ssockf_cgenf_medium_tcp4/1,
         ttest_ssockf_cgenf_medium_tcp6/1,
         ttest_ssockf_cgenf_large_tcp4/1,
         ttest_ssockf_cgenf_large_tcp6/1,

         ttest_ssockf_cgeno_small_tcp4/1,
         ttest_ssockf_cgeno_small_tcp6/1,
         ttest_ssockf_cgeno_medium_tcp4/1,
         ttest_ssockf_cgeno_medium_tcp6/1,
         ttest_ssockf_cgeno_large_tcp4/1,
         ttest_ssockf_cgeno_large_tcp6/1,

         ttest_ssockf_cgent_small_tcp4/1,
         ttest_ssockf_cgent_small_tcp6/1,
         ttest_ssockf_cgent_medium_tcp4/1,
         ttest_ssockf_cgent_medium_tcp6/1,
         ttest_ssockf_cgent_large_tcp4/1,
         ttest_ssockf_cgent_large_tcp6/1,

         %% Server: transport = socket(tcp), active = false
         %% Client: transport = socket(tcp)
         ttest_ssockf_csockf_small_tcp4/1,
         ttest_ssockf_csockf_small_tcp6/1,
         ttest_ssockf_csockf_medium_tcp4/1,
         ttest_ssockf_csockf_medium_tcp6/1,
         ttest_ssockf_csockf_large_tcp4/1,
         ttest_ssockf_csockf_large_tcp6/1,

         ttest_ssockf_csocko_small_tcp4/1,
         ttest_ssockf_csocko_small_tcp6/1,
         ttest_ssockf_csocko_medium_tcp4/1,
         ttest_ssockf_csocko_medium_tcp6/1,
         ttest_ssockf_csocko_large_tcp4/1,
         ttest_ssockf_csocko_large_tcp6/1,

         ttest_ssockf_csockt_small_tcp4/1,
         ttest_ssockf_csockt_small_tcp6/1,
         ttest_ssockf_csockt_medium_tcp4/1,
         ttest_ssockf_csockt_medium_tcp6/1,
         ttest_ssockf_csockt_large_tcp4/1,
         ttest_ssockf_csockt_large_tcp6/1,

         %% Server: transport = socket(tcp), active = once
         %% Client: transport = gen_tcp
         ttest_ssocko_cgenf_small_tcp4/1,
         ttest_ssocko_cgenf_small_tcp6/1,
         ttest_ssocko_cgenf_medium_tcp4/1,
         ttest_ssocko_cgenf_medium_tcp6/1,
         ttest_ssocko_cgenf_large_tcp4/1,
         ttest_ssocko_cgenf_large_tcp6/1,

         ttest_ssocko_cgeno_small_tcp4/1,
         ttest_ssocko_cgeno_small_tcp6/1,
         ttest_ssocko_cgeno_medium_tcp4/1,
         ttest_ssocko_cgeno_medium_tcp6/1,
         ttest_ssocko_cgeno_large_tcp4/1,
         ttest_ssocko_cgeno_large_tcp6/1,

         ttest_ssocko_cgent_small_tcp4/1,
         ttest_ssocko_cgent_small_tcp6/1,
         ttest_ssocko_cgent_medium_tcp4/1,
         ttest_ssocko_cgent_medium_tcp6/1,
         ttest_ssocko_cgent_large_tcp4/1,
         ttest_ssocko_cgent_large_tcp6/1,

         %% Server: transport = socket(tcp), active = once
         %% Client: transport = socket(tcp)
         ttest_ssocko_csockf_small_tcp4/1,
         ttest_ssocko_csockf_small_tcp6/1,
         ttest_ssocko_csockf_medium_tcp4/1,
         ttest_ssocko_csockf_medium_tcp6/1,
         ttest_ssocko_csockf_large_tcp4/1,
         ttest_ssocko_csockf_large_tcp6/1,

         ttest_ssocko_csocko_small_tcp4/1,
         ttest_ssocko_csocko_small_tcp6/1,
         ttest_ssocko_csocko_medium_tcp4/1,
         ttest_ssocko_csocko_medium_tcp6/1,
         ttest_ssocko_csocko_large_tcp4/1,
         ttest_ssocko_csocko_large_tcp6/1,

         ttest_ssocko_csockt_small_tcp4/1,
         ttest_ssocko_csockt_small_tcp6/1,
         ttest_ssocko_csockt_medium_tcp4/1,
         ttest_ssocko_csockt_medium_tcp6/1,
         ttest_ssocko_csockt_large_tcp4/1,
         ttest_ssocko_csockt_large_tcp6/1,

         %% Server: transport = socket(tcp), active = true
         %% Client: transport = gen_tcp
         ttest_ssockt_cgenf_small_tcp4/1,
         ttest_ssockt_cgenf_small_tcp6/1,
         ttest_ssockt_cgenf_medium_tcp4/1,
         ttest_ssockt_cgenf_medium_tcp6/1,
         ttest_ssockt_cgenf_large_tcp4/1,
         ttest_ssockt_cgenf_large_tcp6/1,

         ttest_ssockt_cgeno_small_tcp4/1,
         ttest_ssockt_cgeno_small_tcp6/1,
         ttest_ssockt_cgeno_medium_tcp4/1,
         ttest_ssockt_cgeno_medium_tcp6/1,
         ttest_ssockt_cgeno_large_tcp4/1,
         ttest_ssockt_cgeno_large_tcp6/1,

         ttest_ssockt_cgent_small_tcp4/1,
         ttest_ssockt_cgent_small_tcp6/1,
         ttest_ssockt_cgent_medium_tcp4/1,
         ttest_ssockt_cgent_medium_tcp6/1,
         ttest_ssockt_cgent_large_tcp4/1,
         ttest_ssockt_cgent_large_tcp6/1,

         %% Server: transport = socket(tcp), active = true
         %% Client: transport = socket(tcp)
         ttest_ssockt_csockf_small_tcp4/1,
         ttest_ssockt_csockf_small_tcp6/1,
         ttest_ssockt_csockf_medium_tcp4/1,
         ttest_ssockt_csockf_medium_tcp6/1,
         ttest_ssockt_csockf_large_tcp4/1,
         ttest_ssockt_csockf_large_tcp6/1,

         ttest_ssockt_csocko_small_tcp4/1,
         ttest_ssockt_csocko_small_tcp6/1,
         ttest_ssockt_csocko_medium_tcp4/1,
         ttest_ssockt_csocko_medium_tcp6/1,
         ttest_ssockt_csocko_large_tcp4/1,
         ttest_ssockt_csocko_large_tcp6/1,

         ttest_ssockt_csockt_small_tcp4/1,
         ttest_ssockt_csockt_small_tcp6/1,
         ttest_ssockt_csockt_medium_tcp4/1,
         ttest_ssockt_csockt_medium_tcp6/1,
         ttest_ssockt_csockt_large_tcp4/1,
         ttest_ssockt_csockt_large_tcp6/1

         %% Tickets
        ]).


-include("socket_test_evaluator.hrl").

%% Internal exports
%% -export([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(BASIC_REQ, <<"hejsan">>).
-define(BASIC_REP, <<"hoppsan">>).

-define(DATA,      <<"HOPPSAN">>). % Temporary
-define(FAIL(R), exit(R)).

-define(SLEEP(T), receive after T -> ok end).

-define(MINS(M), timer:minutes(M)).
-define(SECS(S), timer:seconds(S)).

-define(TT(T),   ct:timetrap(T)).

-define(LIB,       socket_test_lib).
-define(TTEST_LIB, socket_test_ttest_lib).
-define(LOGGER,    socket_test_logger).

-define(TPP_SMALL,  lists:seq(1, 8)).
-define(TPP_MEDIUM, lists:flatten(lists:duplicate(1024, ?TPP_SMALL))).
-define(TPP_LARGE,  lists:flatten(lists:duplicate(1024, ?TPP_MEDIUM))).

-define(TPP_SMALL_NUM,  10000).
-define(TPP_MEDIUM_NUM, 1000).
-define(TPP_LARGE_NUM,  100).

-define(TTEST_RUNTIME,  ?SECS(10)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    Groups = [{api,            "ESOCK_TEST_API",        include},
	      {socket_closure, "ESOCK_TEST_SOCK_CLOSE", include},
	      {traffic,        "ESOCK_TEST_TRAFFIC",    include},
	      {ttest,          "ESOCK_TEST_TTEST",      exclude}],
    [use_group(Group, Env, Default) || {Group, Env, Default} <- Groups].

use_group(Group, Env, Default) ->
	case os:getenv(Env) of
	    false when (Default =:= include) ->
		[{group, Group}];
	    false ->
		[];
	    Val ->
		case list_to_atom(string:to_lower(Val)) of
		    Use when (Use =:= include) orelse 
			     (Use =:= enable) orelse 
			     (Use =:= true) ->
			[{group, Group}];
		    _ ->
			[]
		end
	end.
    

groups() -> 
    [{api,                 [], api_cases()},
     {api_basic,           [], api_basic_cases()},
     {api_options,         [], api_options_cases()},
     {api_op_with_timeout, [], api_op_with_timeout_cases()},
     {socket_closure,      [], socket_closure_cases()},
     {sc_ctrl_proc_exit,   [], sc_cp_exit_cases()},
     {sc_local_close,      [], sc_lc_cases()},
     {sc_remote_close,     [], sc_rc_cases()},
     {sc_remote_shutdown,  [], sc_rs_cases()},
     {traffic,             [], traffic_cases()},
     {ttest,               [], ttest_cases()},
     {ttest_sgenf,         [], ttest_sgenf_cases()},
     {ttest_sgenf_cgen,    [], ttest_sgenf_cgen_cases()},
     {ttest_sgenf_cgenf,   [], ttest_sgenf_cgenf_cases()},
     {ttest_sgenf_cgeno,   [], ttest_sgenf_cgeno_cases()},
     {ttest_sgenf_cgent,   [], ttest_sgenf_cgent_cases()},
     {ttest_sgenf_csock,   [], ttest_sgenf_csock_cases()},
     {ttest_sgenf_csockf,  [], ttest_sgenf_csockf_cases()},
     {ttest_sgenf_csocko,  [], ttest_sgenf_csocko_cases()},
     {ttest_sgenf_csockt,  [], ttest_sgenf_csockt_cases()},
     {ttest_sgeno,         [], ttest_sgeno_cases()},
     {ttest_sgeno_cgen,    [], ttest_sgeno_cgen_cases()},
     {ttest_sgeno_cgenf,   [], ttest_sgeno_cgenf_cases()},
     {ttest_sgeno_cgeno,   [], ttest_sgeno_cgeno_cases()},
     {ttest_sgeno_cgent,   [], ttest_sgeno_cgent_cases()},
     {ttest_sgeno_csock,   [], ttest_sgeno_csock_cases()},
     {ttest_sgeno_csockf,  [], ttest_sgeno_csockf_cases()},
     {ttest_sgeno_csocko,  [], ttest_sgeno_csocko_cases()},
     {ttest_sgeno_csockt,  [], ttest_sgeno_csockt_cases()},
     {ttest_sgent,         [], ttest_sgent_cases()},
     {ttest_sgent_cgen,    [], ttest_sgent_cgen_cases()},
     {ttest_sgent_cgenf,   [], ttest_sgent_cgenf_cases()},
     {ttest_sgent_cgeno,   [], ttest_sgent_cgeno_cases()},
     {ttest_sgent_cgent,   [], ttest_sgent_cgent_cases()},
     {ttest_sgent_csock,   [], ttest_sgent_csock_cases()},
     {ttest_sgent_csockf,  [], ttest_sgent_csockf_cases()},
     {ttest_sgent_csocko,  [], ttest_sgent_csocko_cases()},
     {ttest_sgent_csockt,  [], ttest_sgent_csockt_cases()},
     {ttest_ssockf,        [], ttest_ssockf_cases()},
     {ttest_ssockf_cgen,   [], ttest_ssockf_cgen_cases()},
     {ttest_ssockf_cgenf,  [], ttest_ssockf_cgenf_cases()},
     {ttest_ssockf_cgeno,  [], ttest_ssockf_cgeno_cases()},
     {ttest_ssockf_cgent,  [], ttest_ssockf_cgent_cases()},
     {ttest_ssockf_csock,  [], ttest_ssockf_csock_cases()},
     {ttest_ssockf_csockf, [], ttest_ssockf_csockf_cases()},
     {ttest_ssockf_csocko, [], ttest_ssockf_csocko_cases()},
     {ttest_ssockf_csockt, [], ttest_ssockf_csockt_cases()},
     {ttest_ssocko,        [], ttest_ssocko_cases()},
     {ttest_ssocko_cgen,   [], ttest_ssocko_cgen_cases()},
     {ttest_ssocko_cgenf,  [], ttest_ssocko_cgenf_cases()},
     {ttest_ssocko_cgeno,  [], ttest_ssocko_cgeno_cases()},
     {ttest_ssocko_cgent,  [], ttest_ssocko_cgent_cases()},
     {ttest_ssocko_csock,  [], ttest_ssocko_csock_cases()},
     {ttest_ssocko_csockf, [], ttest_ssocko_csockf_cases()},
     {ttest_ssocko_csocko, [], ttest_ssocko_csocko_cases()},
     {ttest_ssocko_csockt, [], ttest_ssocko_csockt_cases()},
     {ttest_ssockt,        [], ttest_ssockt_cases()},
     {ttest_ssockt_cgen,   [], ttest_ssockt_cgen_cases()},
     {ttest_ssockt_cgenf,  [], ttest_ssockt_cgenf_cases()},
     {ttest_ssockt_cgeno,  [], ttest_ssockt_cgeno_cases()},
     {ttest_ssockt_cgent,  [], ttest_ssockt_cgent_cases()},
     {ttest_ssockt_csock,  [], ttest_ssockt_csock_cases()},
     {ttest_ssockt_csockf, [], ttest_ssockt_csockf_cases()},
     {ttest_ssockt_csocko, [], ttest_ssockt_csocko_cases()},
     {ttest_ssockt_csockt, [], ttest_ssockt_csockt_cases()}

     %% {tickets,             [], ticket_cases()}
    ].
     
api_cases() ->
    [
     {group, api_basic},
     {group, api_options},
     {group, api_op_with_timeout}
    ].

api_basic_cases() ->
    [
     api_b_open_and_close_udp4,
     api_b_open_and_close_tcp4,
     api_b_open_and_close_udpL,
     api_b_open_and_close_tcpL,
     api_b_sendto_and_recvfrom_udp4,
     api_b_sendto_and_recvfrom_udpL,
     api_b_sendmsg_and_recvmsg_udp4,
     api_b_send_and_recv_tcp4,
     api_b_send_and_recv_tcpL,
     api_b_sendmsg_and_recvmsg_tcp4,
     api_b_sendmsg_and_recvmsg_tcpL
    ].

api_options_cases() ->
    [
     api_opt_simple_otp_options,
     api_opt_simple_otp_rcvbuf_option,
     api_opt_simple_otp_controlling_process
    ].

api_op_with_timeout_cases() ->
    [
     api_to_connect_tcp4,
     api_to_connect_tcp6,
     api_to_accept_tcp4,
     api_to_accept_tcp6,
     api_to_maccept_tcp4,
     api_to_maccept_tcp6,
     api_to_send_tcp4,
     api_to_send_tcp6,
     api_to_sendto_udp4,
     api_to_sendto_udp6,
     api_to_sendmsg_tcp4,
     api_to_sendmsg_tcp6,
     api_to_recv_udp4,
     api_to_recv_udp6,
     api_to_recv_tcp4,
     api_to_recv_tcp6,
     api_to_recvfrom_udp4,
     api_to_recvfrom_udp6,
     api_to_recvmsg_udp4,
     api_to_recvmsg_udp6,
     api_to_recvmsg_tcp4,
     api_to_recvmsg_tcp6
    ].

%% These cases tests what happens when the socket is closed/shutdown,
%% locally or remotely.
socket_closure_cases() ->
    [
     {group, sc_ctrl_proc_exit},
     {group, sc_local_close},
     {group, sc_remote_close},
     {group, sc_remote_shutdown}
    ].

%% These cases are all about socket cleanup after the controlling process
%% exits *without* calling socket:close/1.
sc_cp_exit_cases() ->
    [
     sc_cpe_socket_cleanup_tcp4,
     sc_cpe_socket_cleanup_tcp6,
     sc_cpe_socket_cleanup_udp4,
     sc_cpe_socket_cleanup_udp6
    ].

%% These cases tests what happens when the socket is closed locally.
sc_lc_cases() ->
    [
     sc_lc_recv_response_tcp4,
     sc_lc_recv_response_tcp6,

     sc_lc_recvfrom_response_udp4,
     sc_lc_recvfrom_response_udp6,

     sc_lc_recvmsg_response_tcp4,
     sc_lc_recvmsg_response_tcp6,
     sc_lc_recvmsg_response_udp4,
     sc_lc_recvmsg_response_udp6,

     sc_lc_acceptor_response_tcp4,
     sc_lc_acceptor_response_tcp6
    ].

%% These cases tests what happens when the socket is closed remotely.
sc_rc_cases() ->
    [
     sc_rc_recv_response_tcp4,
     sc_rc_recv_response_tcp6,

     sc_rc_recvmsg_response_tcp4,
     sc_rc_recvmsg_response_tcp6
    ].

%% These cases tests what happens when the socket is shutdown/closed remotely
%% after writing and reading is ongoing.
sc_rs_cases() ->
    [
     sc_rs_recv_send_shutdown_receive_tcp4,
     sc_rs_recv_send_shutdown_receive_tcp6,

     sc_rs_recvmsg_send_shutdown_receive_tcp4,
     sc_rs_recvmsg_send_shutdown_receive_tcp6
    ].


traffic_cases() ->
    [
     traffic_send_and_recv_chunks_tcp4,
     traffic_send_and_recv_chunks_tcp6,

     traffic_ping_pong_small_send_and_recv_tcp4,
     traffic_ping_pong_small_send_and_recv_tcp6,
     traffic_ping_pong_medium_send_and_recv_tcp4,
     traffic_ping_pong_medium_send_and_recv_tcp6,
     traffic_ping_pong_large_send_and_recv_tcp4,
     traffic_ping_pong_large_send_and_recv_tcp6,

     traffic_ping_pong_small_sendto_and_recvfrom_udp4,
     traffic_ping_pong_small_sendto_and_recvfrom_udp6,
     traffic_ping_pong_medium_sendto_and_recvfrom_udp4,
     traffic_ping_pong_medium_sendto_and_recvfrom_udp6,

     traffic_ping_pong_small_sendmsg_and_recvmsg_tcp4,
     traffic_ping_pong_small_sendmsg_and_recvmsg_tcp6,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp4,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp6,
     traffic_ping_pong_large_sendmsg_and_recvmsg_tcp4,
     traffic_ping_pong_large_sendmsg_and_recvmsg_tcp6,

     traffic_ping_pong_small_sendmsg_and_recvmsg_udp4,
     traffic_ping_pong_small_sendmsg_and_recvmsg_udp6,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_udp4,
     traffic_ping_pong_medium_sendmsg_and_recvmsg_udp6
    ].


ttest_cases() ->
    [
     %% Server: transport = gen_tcp, active = false
     {group, ttest_sgenf},

     %% Server: transport = gen_tcp, active = once
     {group, ttest_sgeno},

     %% Server: transport = gen_tcp, active = true
     {group, ttest_sgent},

     %% Server: transport = socket(tcp), active = false
     {group, ttest_ssockf},

     %% Server: transport = socket(tcp), active = once
     {group, ttest_ssocko},

     %% Server: transport = socket(tcp), active = true
     {group, ttest_ssockt}

    ].


%% Server: transport = gen_tcp, active = false
ttest_sgenf_cases() ->
    [
     {group, ttest_sgenf_cgen},
     {group, ttest_sgenf_csock}
    ].

%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp
ttest_sgenf_cgen_cases() ->
    [
     {group, ttest_sgenf_cgenf},
     {group, ttest_sgenf_cgeno},
     {group, ttest_sgenf_cgent}
    ].

%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp, active = false
ttest_sgenf_cgenf_cases() ->
    [
     ttest_sgenf_cgenf_small_tcp4,
     ttest_sgenf_cgenf_small_tcp6,

     ttest_sgenf_cgenf_medium_tcp4,
     ttest_sgenf_cgenf_medium_tcp6,

     ttest_sgenf_cgenf_large_tcp4,
     ttest_sgenf_cgenf_large_tcp6
    ].

%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp, active = once
ttest_sgenf_cgeno_cases() ->
    [
     ttest_sgenf_cgeno_small_tcp4,
     ttest_sgenf_cgeno_small_tcp6,

     ttest_sgenf_cgeno_medium_tcp4,
     ttest_sgenf_cgeno_medium_tcp6,

     ttest_sgenf_cgeno_large_tcp4,
     ttest_sgenf_cgeno_large_tcp6
    ].

%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp, active = true
ttest_sgenf_cgent_cases() ->
    [
     ttest_sgenf_cgent_small_tcp4,
     ttest_sgenf_cgent_small_tcp6,

     ttest_sgenf_cgent_medium_tcp4,
     ttest_sgenf_cgent_medium_tcp6,

     ttest_sgenf_cgent_large_tcp4,
     ttest_sgenf_cgent_large_tcp6
    ].

%% Server: transport = gen_tcp, active = false
%% Client: transport = socket(tcp)
ttest_sgenf_csock_cases() ->
    [
     {group, ttest_sgenf_csockf},
     {group, ttest_sgenf_csocko},
     {group, ttest_sgenf_csockt}
    ].

ttest_sgenf_csockf_cases() ->
    [
     ttest_sgenf_csockf_small_tcp4,
     ttest_sgenf_csockf_small_tcp6,

     ttest_sgenf_csockf_medium_tcp4,
     ttest_sgenf_csockf_medium_tcp6,

     ttest_sgenf_csockf_large_tcp4,
     ttest_sgenf_csockf_large_tcp6
    ].

ttest_sgenf_csocko_cases() ->
    [
     ttest_sgenf_csocko_small_tcp4,
     ttest_sgenf_csocko_small_tcp6,

     ttest_sgenf_csocko_medium_tcp4,
     ttest_sgenf_csocko_medium_tcp6,

     ttest_sgenf_csocko_large_tcp4,
     ttest_sgenf_csocko_large_tcp6
    ].

ttest_sgenf_csockt_cases() ->
    [
     ttest_sgenf_csockt_small_tcp4,
     ttest_sgenf_csockt_small_tcp6,

     ttest_sgenf_csockt_medium_tcp4,
     ttest_sgenf_csockt_medium_tcp6,

     ttest_sgenf_csockt_large_tcp4,
     ttest_sgenf_csockt_large_tcp6
    ].

%% Server: transport = gen_tcp, active = once
ttest_sgeno_cases() ->
    [
     {group, ttest_sgeno_cgen},
     {group, ttest_sgeno_csock}
    ].

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp
ttest_sgeno_cgen_cases() ->
    [
     {group, ttest_sgeno_cgenf},
     {group, ttest_sgeno_cgeno},
     {group, ttest_sgeno_cgent}
    ].

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp, active = false
ttest_sgeno_cgenf_cases() ->
    [
     ttest_sgeno_cgenf_small_tcp4,
     ttest_sgeno_cgenf_small_tcp6,

     ttest_sgeno_cgenf_medium_tcp4,
     ttest_sgeno_cgenf_medium_tcp6,

     ttest_sgeno_cgenf_large_tcp4,
     ttest_sgeno_cgenf_large_tcp6
    ].

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp, active = once
ttest_sgeno_cgeno_cases() ->
    [
     ttest_sgeno_cgeno_small_tcp4,
     ttest_sgeno_cgeno_small_tcp6,

     ttest_sgeno_cgeno_medium_tcp4,
     ttest_sgeno_cgeno_medium_tcp6,

     ttest_sgeno_cgeno_large_tcp4,
     ttest_sgeno_cgeno_large_tcp6
    ].

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp, active = true
ttest_sgeno_cgent_cases() ->
    [
     ttest_sgeno_cgent_small_tcp4,
     ttest_sgeno_cgent_small_tcp6,

     ttest_sgeno_cgent_medium_tcp4,
     ttest_sgeno_cgent_medium_tcp6,

     ttest_sgeno_cgent_large_tcp4,
     ttest_sgeno_cgent_large_tcp6
    ].

%% Server: transport = gen_tcp, active = once
%% Client: transport = socket(tcp)
ttest_sgeno_csock_cases() ->
    [
     {group, ttest_sgeno_csockf},
     {group, ttest_sgeno_csocko},
     {group, ttest_sgeno_csockt}
    ].

ttest_sgeno_csockf_cases() ->
    [
     ttest_sgeno_csockf_small_tcp4,
     ttest_sgeno_csockf_small_tcp6,

     ttest_sgeno_csockf_medium_tcp4,
     ttest_sgeno_csockf_medium_tcp6,

     ttest_sgeno_csockf_large_tcp4,
     ttest_sgeno_csockf_large_tcp6
    ].

ttest_sgeno_csocko_cases() ->
    [
     ttest_sgeno_csocko_small_tcp4,
     ttest_sgeno_csocko_small_tcp6,

     ttest_sgeno_csocko_medium_tcp4,
     ttest_sgeno_csocko_medium_tcp6,

     ttest_sgeno_csocko_large_tcp4,
     ttest_sgeno_csocko_large_tcp6
    ].

ttest_sgeno_csockt_cases() ->
    [
     ttest_sgeno_csockt_small_tcp4,
     ttest_sgeno_csockt_small_tcp6,

     ttest_sgeno_csockt_medium_tcp4,
     ttest_sgeno_csockt_medium_tcp6,

     ttest_sgeno_csockt_large_tcp4,
     ttest_sgeno_csockt_large_tcp6
    ].

%% Server: transport = gen_tcp, active = true
ttest_sgent_cases() ->
    [
     {group, ttest_sgent_cgen},
     {group, ttest_sgent_csock}
    ].

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp
ttest_sgent_cgen_cases() ->
    [
     {group, ttest_sgent_cgenf},
     {group, ttest_sgent_cgeno},
     {group, ttest_sgent_cgent}
    ].

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp, active = false
ttest_sgent_cgenf_cases() ->
    [
     ttest_sgent_cgenf_small_tcp4,
     ttest_sgent_cgenf_small_tcp6,

     ttest_sgent_cgenf_medium_tcp4,
     ttest_sgent_cgenf_medium_tcp6,

     ttest_sgent_cgenf_large_tcp4,
     ttest_sgent_cgenf_large_tcp6
    ].

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp, active = once
ttest_sgent_cgeno_cases() ->
    [
     ttest_sgent_cgeno_small_tcp4,
     ttest_sgent_cgeno_small_tcp6,

     ttest_sgent_cgeno_medium_tcp4,
     ttest_sgent_cgeno_medium_tcp6,

     ttest_sgent_cgeno_large_tcp4,
     ttest_sgent_cgeno_large_tcp6
    ].

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp, active = true
ttest_sgent_cgent_cases() ->
    [
     ttest_sgent_cgent_small_tcp4,
     ttest_sgent_cgent_small_tcp6,

     ttest_sgent_cgent_medium_tcp4,
     ttest_sgent_cgent_medium_tcp6,

     ttest_sgent_cgent_large_tcp4,
     ttest_sgent_cgent_large_tcp6
    ].

%% Server: transport = gen_tcp, active = true
%% Client: transport = socket(tcp)
ttest_sgent_csock_cases() ->
    [
     {group, ttest_sgent_csockf},
     {group, ttest_sgent_csocko},
     {group, ttest_sgent_csockt}
    ].

ttest_sgent_csockf_cases() ->
    [
     ttest_sgent_csockf_small_tcp4,
     ttest_sgent_csockf_small_tcp6,

     ttest_sgent_csockf_medium_tcp4,
     ttest_sgent_csockf_medium_tcp6,

     ttest_sgent_csockf_large_tcp4,
     ttest_sgent_csockf_large_tcp6
    ].

ttest_sgent_csocko_cases() ->
    [
     ttest_sgent_csocko_small_tcp4,
     ttest_sgent_csocko_small_tcp6,

     ttest_sgent_csocko_medium_tcp4,
     ttest_sgent_csocko_medium_tcp6,

     ttest_sgent_csocko_large_tcp4,
     ttest_sgent_csocko_large_tcp6
    ].

ttest_sgent_csockt_cases() ->
    [
     ttest_sgent_csockt_small_tcp4,
     ttest_sgent_csockt_small_tcp6,

     ttest_sgent_csockt_medium_tcp4,
     ttest_sgent_csockt_medium_tcp6,

     ttest_sgent_csockt_large_tcp4,
     ttest_sgent_csockt_large_tcp6
    ].

%% Server: transport = socket(tcp), active = false
ttest_ssockf_cases() ->
    [
     {group, ttest_ssockf_cgen},
     {group, ttest_ssockf_csock}
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp
ttest_ssockf_cgen_cases() ->
    [
     {group, ttest_ssockf_cgenf},
     {group, ttest_ssockf_cgeno},
     {group, ttest_ssockf_cgent}
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp, active = false
ttest_ssockf_cgenf_cases() ->
    [
     ttest_ssockf_cgenf_small_tcp4,
     ttest_ssockf_cgenf_small_tcp6,

     ttest_ssockf_cgenf_medium_tcp4,
     ttest_ssockf_cgenf_medium_tcp6,

     ttest_ssockf_cgenf_large_tcp4,
     ttest_ssockf_cgenf_large_tcp6
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp, active = once
ttest_ssockf_cgeno_cases() ->
    [
     ttest_ssockf_cgeno_small_tcp4,
     ttest_ssockf_cgeno_small_tcp6,

     ttest_ssockf_cgeno_medium_tcp4,
     ttest_ssockf_cgeno_medium_tcp6,

     ttest_ssockf_cgeno_large_tcp4,
     ttest_ssockf_cgeno_large_tcp6
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp, active = true
ttest_ssockf_cgent_cases() ->
    [
     ttest_ssockf_cgent_small_tcp4,
     ttest_ssockf_cgent_small_tcp6,

     ttest_ssockf_cgent_medium_tcp4,
     ttest_ssockf_cgent_medium_tcp6,

     ttest_ssockf_cgent_large_tcp4,
     ttest_ssockf_cgent_large_tcp6
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = socket(tcp)
ttest_ssockf_csock_cases() ->
    [
     {group, ttest_ssockf_csockf},
     {group, ttest_ssockf_csocko},
     {group, ttest_ssockf_csockt}
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = socket(tcp), active = false
ttest_ssockf_csockf_cases() ->
    [
     ttest_ssockf_csockf_small_tcp4,
     ttest_ssockf_csockf_small_tcp6,

     ttest_ssockf_csockf_medium_tcp4,
     ttest_ssockf_csockf_medium_tcp6,

     ttest_ssockf_csockf_large_tcp4,
     ttest_ssockf_csockf_large_tcp6
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = socket(tcp), active = once
ttest_ssockf_csocko_cases() ->
    [
     ttest_ssockf_csocko_small_tcp4,
     ttest_ssockf_csocko_small_tcp6,

     ttest_ssockf_csocko_medium_tcp4,
     ttest_ssockf_csocko_medium_tcp6,

     ttest_ssockf_csocko_large_tcp4,
     ttest_ssockf_csocko_large_tcp6
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = socket(tcp), active = true
ttest_ssockf_csockt_cases() ->
    [
     ttest_ssockf_csockt_small_tcp4,
     ttest_ssockf_csockt_small_tcp6,

     ttest_ssockf_csockt_medium_tcp4,
     ttest_ssockf_csockt_medium_tcp6,

     ttest_ssockf_csockt_large_tcp4,
     ttest_ssockf_csockt_large_tcp6
    ].

%% Server: transport = socket(tcp), active = once
ttest_ssocko_cases() ->
    [
     {group, ttest_ssocko_cgen},
     {group, ttest_ssocko_csock}
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp
ttest_ssocko_cgen_cases() ->
    [
     {group, ttest_ssocko_cgenf},
     {group, ttest_ssocko_cgeno},
     {group, ttest_ssocko_cgent}
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp, active = false
ttest_ssocko_cgenf_cases() ->
    [
     ttest_ssocko_cgenf_small_tcp4,
     ttest_ssocko_cgenf_small_tcp6,

     ttest_ssocko_cgenf_medium_tcp4,
     ttest_ssocko_cgenf_medium_tcp6,

     ttest_ssocko_cgenf_large_tcp4,
     ttest_ssocko_cgenf_large_tcp6
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp, active = once
ttest_ssocko_cgeno_cases() ->
    [
     ttest_ssocko_cgeno_small_tcp4,
     ttest_ssocko_cgeno_small_tcp6,

     ttest_ssocko_cgeno_medium_tcp4,
     ttest_ssocko_cgeno_medium_tcp6,

     ttest_ssocko_cgeno_large_tcp4,
     ttest_ssocko_cgeno_large_tcp6
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp, active = true
ttest_ssocko_cgent_cases() ->
    [
     ttest_ssocko_cgent_small_tcp4,
     ttest_ssocko_cgent_small_tcp6,

     ttest_ssocko_cgent_medium_tcp4,
     ttest_ssocko_cgent_medium_tcp6,

     ttest_ssocko_cgent_large_tcp4,
     ttest_ssocko_cgent_large_tcp6
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = socket(tcp)
ttest_ssocko_csock_cases() ->
    [
     {group, ttest_ssocko_csockf},
     {group, ttest_ssocko_csocko},
     {group, ttest_ssocko_csockt}
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = socket(tcp), active = false
ttest_ssocko_csockf_cases() ->
    [
     ttest_ssocko_csockf_small_tcp4,
     ttest_ssocko_csockf_small_tcp6,

     ttest_ssocko_csockf_medium_tcp4,
     ttest_ssocko_csockf_medium_tcp6,

     ttest_ssocko_csockf_large_tcp4,
     ttest_ssocko_csockf_large_tcp6
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = socket(tcp), active = once
ttest_ssocko_csocko_cases() ->
    [
     ttest_ssocko_csocko_small_tcp4,
     ttest_ssocko_csocko_small_tcp6,

     ttest_ssocko_csocko_medium_tcp4,
     ttest_ssocko_csocko_medium_tcp6,

     ttest_ssocko_csocko_large_tcp4,
     ttest_ssocko_csocko_large_tcp6
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = socket(tcp), active = true
ttest_ssocko_csockt_cases() ->
    [
     ttest_ssocko_csockt_small_tcp4,
     ttest_ssocko_csockt_small_tcp6,

     ttest_ssocko_csockt_medium_tcp4,
     ttest_ssocko_csockt_medium_tcp6,

     ttest_ssocko_csockt_large_tcp4,
     ttest_ssocko_csockt_large_tcp6
    ].

%% Server: transport = socket(tcp), active = true
ttest_ssockt_cases() ->
    [
     {group, ttest_ssockt_cgen},
     {group, ttest_ssockt_csock}
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp
ttest_ssockt_cgen_cases() ->
    [
     {group, ttest_ssockt_cgenf},
     {group, ttest_ssockt_cgeno},
     {group, ttest_ssockt_cgent}
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp, active = false
ttest_ssockt_cgenf_cases() ->
    [
     ttest_ssockt_cgenf_small_tcp4,
     ttest_ssockt_cgenf_small_tcp6,

     ttest_ssockt_cgenf_medium_tcp4,
     ttest_ssockt_cgenf_medium_tcp6,

     ttest_ssockt_cgenf_large_tcp4,
     ttest_ssockt_cgenf_large_tcp6
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp, active = once
ttest_ssockt_cgeno_cases() ->
    [
     ttest_ssockt_cgeno_small_tcp4,
     ttest_ssockt_cgeno_small_tcp6,

     ttest_ssockt_cgeno_medium_tcp4,
     ttest_ssockt_cgeno_medium_tcp6,

     ttest_ssockt_cgeno_large_tcp4,
     ttest_ssockt_cgeno_large_tcp6
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp, active = true
ttest_ssockt_cgent_cases() ->
    [
     ttest_ssockt_cgent_small_tcp4,
     ttest_ssockt_cgent_small_tcp6,

     ttest_ssockt_cgent_medium_tcp4,
     ttest_ssockt_cgent_medium_tcp6,

     ttest_ssockt_cgent_large_tcp4,
     ttest_ssockt_cgent_large_tcp6
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = socket(tcp)
ttest_ssockt_csock_cases() ->
    [
     {group, ttest_ssockt_csockf},
     {group, ttest_ssockt_csocko},
     {group, ttest_ssockt_csockt}
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = socket(tcp), active = false
ttest_ssockt_csockf_cases() ->
    [
     ttest_ssockt_csockf_small_tcp4,
     ttest_ssockt_csockf_small_tcp6,

     ttest_ssockt_csockf_medium_tcp4,
     ttest_ssockt_csockf_medium_tcp6,

     ttest_ssockt_csockf_large_tcp4,
     ttest_ssockt_csockf_large_tcp6
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = socket(tcp), active = once
ttest_ssockt_csocko_cases() ->
    [
     ttest_ssockt_csocko_small_tcp4,
     ttest_ssockt_csocko_small_tcp6,

     ttest_ssockt_csocko_medium_tcp4,
     ttest_ssockt_csocko_medium_tcp6,

     ttest_ssockt_csocko_large_tcp4,
     ttest_ssockt_csocko_large_tcp6
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = socket(tcp), active = true
ttest_ssockt_csockt_cases() ->
    [
     ttest_ssockt_csockt_small_tcp4,
     ttest_ssockt_csockt_small_tcp6,

     ttest_ssockt_csockt_medium_tcp4,
     ttest_ssockt_csockt_medium_tcp6,

     ttest_ssockt_csockt_large_tcp4,
     ttest_ssockt_csockt_large_tcp6
    ].

%% ticket_cases() ->
%%     [].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config) ->
    case lists:member(socket, erlang:loaded()) of
        true ->
            case os:type() of
                {win32, _} ->
                    (catch not_yet_implemented());
                _ ->
                    case quiet_mode(Config) of
                        default ->
                            ?LOGGER:start(),
                            Config;
                        Quiet ->
                            ?LOGGER:start(Quiet),
                            [{esock_test_quiet, Quiet}|Config]
                    end
            end;
        false ->
            {skip, "esock disabled"}
    end.

end_per_suite(_) ->
    (catch ?LOGGER:stop()),
    ok.


init_per_group(ttest = _GroupName, Config) ->
    io:format("init_per_group(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [_GroupName, Config]),
    case lists:keysearch(esock_test_ttest_runtime, 1, Config) of
        {value, _} ->
            Config;
        false ->
            [{esock_test_ttest_runtime, which_ttest_runtime_env()}|Config]
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(ttest = _GroupName, Config) ->
    io:format("init_per_group(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [_GroupName, Config]),
    lists:keydelete(esock_test_ttest_runtime, 1, Config);
end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_TC, Config) ->
    io:format("init_per_testcase(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [_TC, Config]),
    case quiet_mode(Config) of
        default ->
            ?LOGGER:start();
        Quiet ->
            ?LOGGER:start(Quiet)
    end,
    Config.

end_per_testcase(_TC, Config) ->
    ?LOGGER:stop(),
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                           API BASIC                                 %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and close an IPv4 UDP (dgram) socket.
%% With some extra checks...
api_b_open_and_close_udp4(suite) ->
    [];
api_b_open_and_close_udp4(doc) ->
    [];
api_b_open_and_close_udp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_b_open_and_close_udp4,
           fun() ->
                   InitState = #{domain   => inet,
                                 type     => dgram,
                                 protocol => udp},
                   ok = api_b_open_and_close(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and close an IPv4 TCP (stream) socket.
%% With some extra checks...
api_b_open_and_close_tcp4(suite) ->
    [];
api_b_open_and_close_tcp4(doc) ->
    [];
api_b_open_and_close_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_b_open_and_close_tcp4,
           fun() ->
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = api_b_open_and_close(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and close an Unix Domain dgram (UDP) socket.
%% With some extra checks...
api_b_open_and_close_udpL(suite) ->
    [];
api_b_open_and_close_udpL(doc) ->
    [];
api_b_open_and_close_udpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_b_open_and_close_udpL,
           fun() -> supports_unix_domain_socket() end,
           fun() ->
                   InitState = #{domain   => local,
                                 type     => dgram,
                                 protocol => default},
                   ok = api_b_open_and_close(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and close an Unix Domain stream (TCP) socket.
%% With some extra checks...
api_b_open_and_close_tcpL(suite) ->
    [];
api_b_open_and_close_tcpL(doc) ->
    [];
api_b_open_and_close_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_b_open_and_close_tcpL,
           fun() -> supports_unix_domain_socket() end,
           fun() ->
                   InitState = #{domain   => local,
                                 type     => stream,
                                 protocol => default},
                   ok = api_b_open_and_close(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_b_open_and_close(InitState) ->
    Seq = 
        [
         #{desc => "open",
           cmd  => fun(#{domain   := Domain,
                         type     := Type,
                         protocol := Protocol} = S) -> 
                           Res = socket:open(Domain, Type, Protocol), 
                           {ok, {S, Res}} 
                   end},
         #{desc => "validate open",
           cmd  => fun({S, {ok, Sock}}) -> 
                           NewS = S#{socket => Sock},
                           {ok, NewS};
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},
         #{desc => "get domain (maybe)",
           cmd  => fun(#{socket := Sock} = S) ->
                           Res = socket:getopt(Sock, socket, domain),
                           {ok, {S, Res}}
                   end},
         #{desc => "validate domain (maybe)",
           cmd  => fun({#{domain := Domain} = S, {ok, Domain}}) -> 
                           {ok, S};
                      ({#{domain := ExpDomain}, {ok, Domain}}) ->
                           {error, {unexpected_domain, ExpDomain, Domain}};
                      %% Some platforms do not support this option
                      ({S, {error, einval}}) ->
                           {ok, S};
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},
         #{desc => "get type",
           cmd  => fun(#{socket := Sock} = State) ->
                           Res = socket:getopt(Sock, socket, type), 
                           {ok, {State, Res}}
                   end},
         #{desc => "validate type",
           cmd  => fun({#{type := Type} = State, {ok, Type}}) ->
                           {ok, State};
                      ({#{type := ExpType}, {ok, Type}}) ->
                           {error, {unexpected_type, ExpType, Type}};
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},
         #{desc => "get protocol",
           cmd  => fun(#{socket := Sock} = State) ->
			   case socket:supports(options, socket, protocol) of
			       true ->
				   Res = socket:getopt(Sock, socket, protocol),
				   {ok, {State, Res}};
			       false ->
				   {ok, {State, not_supported}}
			   end
                   end},
         #{desc => "validate protocol",
           cmd  => fun({State, not_supported}) ->
			   ?SEV_IPRINT("socket option 'protocol' "
				       "not supported"),
                           {ok, State};
                      ({#{protocol := Protocol} = State, {ok, Protocol}}) ->
                           {ok, State};
                      ({#{protocol := ExpProtocol}, {ok, Protocol}}) ->
                           {error, {unexpected_type, ExpProtocol, Protocol}};
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},
         #{desc => "get controlling-process",
           cmd  => fun(#{socket := Sock} = State) ->
                           Res = socket:getopt(Sock, otp, controlling_process),
                           {ok, {State, Res}}
                   end},
         #{desc => "validate controlling-process",
           cmd  => fun({State, {ok, Pid}}) ->
                           case self() of
                               Pid ->
                                   {ok, State};
                               _ ->
                                   {error, {unexpected_owner, Pid}}
                           end;
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},
         #{desc => "close socket",
           cmd  => fun(#{socket := Sock} = State) ->
                           Res = socket:close(Sock),
                           {ok, {State, Res}}
                   end},
         #{desc => "validate socket close",
           cmd  => fun({_, ok}) ->
                           ok;
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    Evaluator = ?SEV_START("tester", Seq, InitState),
    ok = ?SEV_AWAIT_FINISH([Evaluator]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive on an IPv4 UDP (dgram) socket using
%% sendto and recvfrom..
api_b_sendto_and_recvfrom_udp4(suite) ->
    [];
api_b_sendto_and_recvfrom_udp4(doc) ->
    [];
api_b_sendto_and_recvfrom_udp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_b_sendto_and_recvfrom_udp4,
           fun() ->
                   Send = fun(Sock, Data, Dest) ->
                                  socket:sendto(Sock, Data, Dest)
                          end,
                   Recv = fun(Sock) ->
                                  socket:recvfrom(Sock)
                          end,
                   InitState = #{domain => inet,
                                 proto  => udp,
                                 send   => Send,
                                 recv   => Recv},
                   ok = api_b_send_and_recv_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive on an IPv4 UDP (dgram) socket using
%% sendto and recvfrom..
api_b_sendto_and_recvfrom_udpL(suite) ->
    [];
api_b_sendto_and_recvfrom_udpL(doc) ->
    [];
api_b_sendto_and_recvfrom_udpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_b_sendto_and_recvfrom_udpL,
           fun() -> supports_unix_domain_socket() end,
           fun() ->
                   Send = fun(Sock, Data, Dest) ->
                                  socket:sendto(Sock, Data, Dest)
                          end,
                   Recv = fun(Sock) ->
                                  socket:recvfrom(Sock)
                          end,
                   InitState = #{domain => local,
                                 proto  => default,
                                 send   => Send,
                                 recv   => Recv},
                   ok = api_b_send_and_recv_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive on an IPv4 UDP (dgram) socket
%% using sendmsg and recvmsg.
api_b_sendmsg_and_recvmsg_udp4(suite) ->
    [];
api_b_sendmsg_and_recvmsg_udp4(doc) ->
    [];
api_b_sendmsg_and_recvmsg_udp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_b_sendmsg_and_recvmsg_udp4,
           fun() ->
                   Send = fun(Sock, Data, Dest) ->
                                  %% We need tests for this,
                                  %% but this is not the place it.
                                  %% CMsgHdr  = #{level => ip,
                                  %%              type  => tos,
                                  %%              data  => reliability},
                                  %% CMsgHdrs = [CMsgHdr],
                                  MsgHdr = #{addr => Dest,
                                             %% ctrl => CMsgHdrs,
                                             iov  => [Data]},
                                  socket:sendmsg(Sock, MsgHdr)
                          end,
                   Recv = fun(Sock) ->
                                  %% We have some issues on old darwing...
                                  %% socket:setopt(Sock, otp, debug, true),
                                  case socket:recvmsg(Sock) of
                                      {ok, #{addr  := Source,
                                             iov   := [Data]}} ->
                                          %% socket:setopt(Sock, otp, debug, false),
                                          {ok, {Source, Data}};
                                      {error, _} = ERROR ->
                                          ERROR
                                  end
                          end,
                   InitState = #{domain => inet,
                                 proto  => udp,
                                 send   => Send,
                                 recv   => Recv},
                   ok = api_b_send_and_recv_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_b_send_and_recv_udp(InitState) ->
    Seq = 
        [
         #{desc => "local address",
           cmd  => fun(#{domain := local = Domain} = State) ->
                           LSASrc = which_local_socket_addr(Domain),
                           LSADst = which_local_socket_addr(Domain),
                           {ok, State#{lsa_src => LSASrc,
                                       lsa_dst => LSADst}};
                      (#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{lsa_src => LSA,
                                       lsa_dst => LSA}}
                   end},

         #{desc => "open src socket",
           cmd  => fun(#{domain := Domain,
                         proto  := Proto} = State) ->
                           Sock = sock_open(Domain, dgram, Proto),
                           {ok, State#{sock_src => Sock}}
                   end},
         #{desc => "bind src",
           cmd  => fun(#{sock_src := Sock, lsa_src := LSA}) ->
                           sock_bind(Sock, LSA),
                           ok
                   end},
         #{desc => "sockname src socket",
           cmd  => fun(#{sock_src := Sock} = State) ->
                           SASrc = sock_sockname(Sock),
                           ?SEV_IPRINT("src sockaddr: "
                                       "~n   ~p", [SASrc]),
                           {ok, State#{sa_src => SASrc}}
                   end},

         #{desc => "open dst socket",
           cmd  => fun(#{domain := Domain,
                         proto  := Proto} = State) ->
                           Sock = sock_open(Domain, dgram, Proto),
                           {ok, State#{sock_dst => Sock}}
                   end},
         #{desc => "bind dst",
           cmd  => fun(#{sock_dst := Sock, lsa_dst := LSA}) ->
                           sock_bind(Sock, LSA),
                           ok
                   end},
         #{desc => "sockname dst socket",
           cmd  => fun(#{sock_dst := Sock} = State) ->
                           SADst = sock_sockname(Sock),
                           ?SEV_IPRINT("dst sockaddr: "
                                       "~n   ~p", [SADst]),
                           {ok, State#{sa_dst => SADst}}
                   end},
         #{desc => "send req (to dst)",
           cmd  => fun(#{sock_src := Sock, sa_dst := Dst, send := Send}) ->
                           Send(Sock, ?BASIC_REQ, Dst)
                   end},
         #{desc => "recv req (from src)",
           cmd  => fun(#{sock_dst := Sock, sa_src := Src, recv := Recv}) ->
                           case Recv(Sock) of
                               {ok, {Src, ?BASIC_REQ}} ->
                                   ok;
                               {ok, UnexpData} ->
                                   {error, {unexpected_data, UnexpData}};
                               {error, _} = ERROR ->
                                   %% At the moment there is no way to get
                                   %% status or state for the socket...
                                   ERROR
                           end
                   end},
         #{desc => "send rep (to src)",
           cmd  => fun(#{sock_dst := Sock, sa_src := Src, send := Send}) ->
                           Send(Sock, ?BASIC_REP, Src)
                   end},
         #{desc => "recv rep (from dst)",
           cmd  => fun(#{sock_src := Sock, sa_dst := Dst, recv := Recv}) ->
                           case Recv(Sock) of
                               {ok, {Dst, ?BASIC_REP}} ->
                                   ok;
                               {ok, UnexpData} ->
                                   {error, {unexpected_data, UnexpData}};
                               {error, _} = ERROR ->
                                   %% At the moment there is no way to get
                                   %% status or state for the socket...
                                   ERROR
                           end
                   end},
         #{desc => "close src socket",
           cmd  => fun(#{domain   := local,
                         sock_src := Sock,
                         lsa_src  := #{path := Path}}) ->
                           ok = socket:close(Sock),
                           case os:cmd("unlink " ++ Path) of
                               "" ->
                                   ok;
                               Result ->
                                   ?SEV_IPRINT("unlink result: "
                                               "~n   ~s", [Result]),
                                   ok
                           end,
                           ok;
                      (#{sock_src := Sock}) ->
                           ok = socket:close(Sock)
                   end},
         #{desc => "close dst socket",
           cmd  => fun(#{domain   := local,
                         sock_dst := Sock,
                         lsa_dst  := #{path := Path}}) ->
                           ok = socket:close(Sock),
                           case os:cmd("unlink " ++ Path) of
                               "" ->
                                   ok;
                               Result ->
                                   ?SEV_IPRINT("unlink result: "
                                               "~n   ~s", [Result]),
                                   ok
                           end,
                           ok;
                      (#{sock_dst := Sock}) ->
                           ok = socket:close(Sock)
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    Evaluator = ?SEV_START("tester", Seq, InitState),
    ok = ?SEV_AWAIT_FINISH([Evaluator]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive using the "common" functions (send and recv)
%% on an IPv4 TCP (stream) socket.
api_b_send_and_recv_tcp4(suite) ->
    [];
api_b_send_and_recv_tcp4(doc) ->
    [];
api_b_send_and_recv_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(api_b_send_and_recv_tcp4,
           fun() ->
                   Send = fun(Sock, Data) ->
                                  socket:send(Sock, Data)
                          end,
                   Recv = fun(Sock) ->
                                  socket:recv(Sock)
                          end,
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 send   => Send,
                                 recv   => Recv},
                   ok = api_b_send_and_recv_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive using the "common" functions (send and recv)
%% on an Unix Domain (stream) socket (TCP).
api_b_send_and_recv_tcpL(suite) ->
    [];
api_b_send_and_recv_tcpL(doc) ->
    [];
api_b_send_and_recv_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(api_b_send_and_recv_tcpL,
           fun() -> supports_unix_domain_socket() end,
           fun() ->
                   Send = fun(Sock, Data) ->
                                  socket:send(Sock, Data)
                          end,
                   Recv = fun(Sock) ->
                                  socket:recv(Sock)
                          end,
                   InitState = #{domain => local,
                                 proto  => default,
                                 send   => Send,
                                 recv   => Recv},
                   ok = api_b_send_and_recv_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive using the msg functions (sendmsg and recvmsg)
%% on an IPv4 TCP (stream) socket.
api_b_sendmsg_and_recvmsg_tcp4(suite) ->
    [];
api_b_sendmsg_and_recvmsg_tcp4(doc) ->
    [];
api_b_sendmsg_and_recvmsg_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(api_b_sendmsg_and_recvmsg_tcp4,
           fun() ->
                   Send = fun(Sock, Data) ->
                                  MsgHdr = #{iov => [Data]},
                                  socket:sendmsg(Sock, MsgHdr)
                          end,
                   Recv = fun(Sock) ->
                                  case socket:recvmsg(Sock) of
                                      {ok, #{addr  := undefined,
                                             iov   := [Data]}} ->
                                          {ok, Data};
                                      {error, _} = ERROR ->
                                          ERROR
                                  end
                          end,
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 send   => Send,
                                 recv   => Recv},
                   ok = api_b_send_and_recv_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive using the msg functions (sendmsg and recvmsg)
%% on an Unix Domain (stream) socket (TCP).
api_b_sendmsg_and_recvmsg_tcpL(suite) ->
    [];
api_b_sendmsg_and_recvmsg_tcpL(doc) ->
    [];
api_b_sendmsg_and_recvmsg_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(api_b_sendmsg_and_recvmsg_tcpL,
           fun() -> supports_unix_domain_socket() end,
           fun() ->
                   Send = fun(Sock, Data) ->
                                  MsgHdr = #{iov => [Data]},
                                  socket:sendmsg(Sock, MsgHdr)
                          end,
                   Recv = fun(Sock) ->
                                  case socket:recvmsg(Sock) of
                                      {ok, #{addr  := #{family := local},
                                             iov   := [Data]}} ->
                                          {ok, Data};
                                      {error, _} = ERROR ->
                                          ERROR
                                  end
                          end,
                   InitState = #{domain => local,
                                 proto  => default,
                                 send   => Send,
                                 recv   => Recv},
                   ok = api_b_send_and_recv_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_b_send_and_recv_tcp(InitState) ->
    process_flag(trap_exit, true),
    ServerSeq = 
        [
         %% *** Wait for start order ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester}) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create listen socket",
           cmd  => fun(#{domain := Domain,
                         proto  := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{domain := local,
                         lsock  := LSock,
                         lsa    := LSA} = _State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, _Port} ->
                                   ok; % We do not care about the port for local
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{lsock := LSock, lsa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
                                   ?SEV_IPRINT("bound to port: ~w", [Port]),
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
           cmd  => fun(#{domain := local,
                         tester := Tester, lsa := #{path := Path}}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Path),
                           ok;
                      (#{tester := Tester, lport := Port}) ->
                           %% This is actually not used for unix domain socket
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "await connection",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: ~n   ~p", [Sock]),
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
         #{desc => "await (recv) request",
           cmd  => fun(#{csock := Sock, recv := Recv}) ->
                           case Recv(Sock) of
                               {ok, ?BASIC_REQ} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv request)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_req),
                           ok
                   end},
         #{desc => "await continue (with send reply)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_reply)
                   end},
         #{desc => "send reply",
           cmd  => fun(#{csock := Sock, send := Send}) ->
                           Send(Sock, ?BASIC_REP)
                   end},
         #{desc => "announce ready (send reply)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_reply),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close connection socket",
           cmd  => fun(#{domain := local,
                         csock  := Sock,
                         lsa    := #{path := Path}}) ->
                           ok = socket:close(Sock),
                           case os:cmd("unlink " ++ Path) of
                               "" ->
                                   ok;
                               Result ->
                                   ?SEV_IPRINT("unlink result: "
                                               "~n   ~s", [Result]),
                                   ok
                           end,
                           ok;
                      (#{csock := Sock}) ->
                           socket:close(Sock)
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{lsock := Sock}) ->
                           socket:close(Sock)
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq = 
        [
         %% *** Wait for start order ***
         #{desc => "await start (from tester)",
           cmd  => fun(#{domain := local} = State) ->
                           {Tester, Path} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, server_path => Path}};
                      (State) ->
                           {Tester, Port} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, server_port => Port}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester}) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** The init part ***
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
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, local_sa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _Port} ->
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

         %% *** The actual test ***
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect)
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
         #{desc => "await continue (send request)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send_req)
                   end},
         #{desc => "send request (to server)",
           cmd  => fun(#{sock := Sock, send := Send}) ->
                           Send(Sock, ?BASIC_REQ)
                   end},
         #{desc => "announce ready (send request)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send_req),
                           ok
                   end},
         #{desc => "await recv reply (from server)",
           cmd  => fun(#{sock := Sock, recv := Recv}) ->
                           {ok, ?BASIC_REP} = Recv(Sock),
                           ok
                   end},
         #{desc => "announce ready (recv reply)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_reply),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{domain   := local,
                         sock     := Sock,
                         local_sa := #{path := Path}}) ->
                           ok = socket:close(Sock),
                           case os:cmd("unlink " ++ Path) of
                               "" ->
                                   ok;
                               Result ->
                                   ?SEV_IPRINT("unlink result:"
                                               "~n   ~s", [Result]),
                                   ok
                           end,
                           ok;
                      (#{sock := Sock} = _S) ->
                           socket:close(Sock)
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
                           {ok, Port} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{server_port => Port}}
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client := Pid, server_port := Port} = _State) ->
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
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
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
         #{desc => "order client to continue (with send request)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send_req),
                           ok
                   end},
         #{desc => "await client ready (with send request)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, send_req)
                   end},
         #{desc => "await server ready (request recv)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv_req)
                   end},
         #{desc => "order server to continue (with send reply)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, send_reply),
                           ok
                   end},
         #{desc => "await server ready (with reply sent)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, send_reply)
                   end},
         #{desc => "await client ready (reply recv)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, recv_reply)
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
    Server = ?SEV_START("server", ServerSeq, InitState),

    i("start client evaluator"),
    Client = ?SEV_START("client", ClientSeq, InitState),
    i("await evaluator(s)"),

    i("start tester evaluator"),
    TesterInitState = #{server => Server#ev.pid,
                        client => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                           API OPTIONS                               %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Perform some simple getopt and setopt with the level = otp options
api_opt_simple_otp_options(suite) ->
    [];
api_opt_simple_otp_options(doc) ->
    [];
api_opt_simple_otp_options(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_opt_simple_otp_options,
           fun() -> api_opt_simple_otp_options() end).

api_opt_simple_otp_options() ->
    Get = fun(S, Key) ->
                  socket:getopt(S, otp, Key)
          end,
    Set = fun(S, Key, Val) ->
                  socket:setopt(S, otp, Key, Val)
          end,

    Seq = 
        [
         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type,
                         protocol := Protocol} = State) ->
                           Sock = sock_open(Domain, Type, Protocol),
                           {ok, State#{sock => Sock}}
                   end},
         #{desc => "create dummy process",
           cmd  => fun(State) ->
                           Pid =  spawn_link(fun() -> 
                                                     put(sname, "dummy"),
                                                     receive
                                                         die -> 
                                                             exit(normal) 
                                                     end 
                                             end),
                           {ok, State#{dummy => Pid}}
                   end},

         %% *** Check iow part ***
         #{desc => "get iow",
           cmd  => fun(#{sock := Sock} = State) ->
                           case Get(Sock, iow) of
                               {ok, IOW} when is_boolean(IOW) ->
                                   {ok, State#{iow => IOW}};
                               {ok, InvalidIOW} ->
                                   {error, {invalid, InvalidIOW}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% #{desc => "enable debug",
         %%   cmd  => fun(#{sock := Sock}) ->
         %%                   ok = socket:setopt(Sock, otp, debug, true)
         %%           end},

         #{desc => "set (new) iow",
           cmd  => fun(#{sock := Sock, iow := OldIOW} = State) ->
                           NewIOW = not OldIOW,
                           case Set(Sock, iow, NewIOW) of
                               ok ->
                                   {ok, State#{iow => NewIOW}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "get (new) iow",
           cmd  => fun(#{sock := Sock, iow := IOW}) ->
                           case Get(Sock, iow) of
                               {ok, IOW} ->
                                   ok;
                               {ok, InvalidIOW} ->
                                   {error, {invalid, InvalidIOW}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** Check rcvbuf part ***
         #{desc => "get rcvbuf",
           cmd  => fun(#{sock := Sock} = State) ->
                           case Get(Sock, rcvbuf) of
                               {ok, RcvBuf} when is_integer(RcvBuf) ->
                                   {ok, State#{rcvbuf => RcvBuf}};
                               {ok, {N, RcvBuf} = V} when is_integer(N) andalso 
                                                          is_integer(RcvBuf) ->
                                   {ok, State#{rcvbuf => V}};
                               {ok, InvalidRcvBuf} ->
                                   {error, {invalid, InvalidRcvBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "set (new) rcvbuf",
           cmd  => fun(#{sock := Sock, rcvbuf := {OldN, OldRcvBuf}} = State) ->
                           NewRcvBuf = {OldN+2, OldRcvBuf + 1024},
                           case Set(Sock, rcvbuf, NewRcvBuf) of
                               ok ->
                                   {ok, State#{rcvbuf => NewRcvBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{sock := Sock, rcvbuf := OldRcvBuf} = State) when is_integer(OldRcvBuf) ->
                           NewRcvBuf = 2 * OldRcvBuf,
                           case Set(Sock, rcvbuf, NewRcvBuf) of
                               ok ->
                                   {ok, State#{rcvbuf => NewRcvBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{sock := Sock, rcvbuf := OldRcvBuf,
                         type := stream,
                         protocol := tcp} = State) when is_integer(OldRcvBuf) ->
                           NewRcvBuf = {2, OldRcvBuf},
                           case Set(Sock, rcvbuf, NewRcvBuf) of
                               ok ->
                                   {ok, State#{rcvbuf => NewRcvBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "get (new) rcvbuf",
           cmd  => fun(#{sock := Sock, rcvbuf := RcvBuf}) ->
                           case Get(Sock, rcvbuf) of
                               {ok, RcvBuf} ->
                                   ok;
                               {ok, InvalidRcvBuf} ->
                                   {error, {invalid, InvalidRcvBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** Check rcvctrlbuf part ***
         #{desc => "get rcvctrlbuf",
           cmd  => fun(#{sock := Sock} = State) ->
                           case Get(Sock, rcvctrlbuf) of
                               {ok, RcvCtrlBuf} when is_integer(RcvCtrlBuf) ->
                                   {ok, State#{rcvctrlbuf => RcvCtrlBuf}};
                               {ok, InvalidRcvCtrlBuf} ->
                                   {error, {invalid, InvalidRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "set (new) rcvctrlbuf",
           cmd  => fun(#{sock := Sock, rcvctrlbuf := OldRcvCtrlBuf} = State) ->
                           NewRcvCtrlBuf = 2 * OldRcvCtrlBuf,
                           case Set(Sock, rcvctrlbuf, NewRcvCtrlBuf) of
                               ok ->
                                   {ok, State#{rcvctrlbuf => NewRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "get (new) rcvctrlbuf",
           cmd  => fun(#{sock := Sock, rcvctrlbuf := RcvCtrlBuf}) ->
                           case Get(Sock, rcvctrlbuf) of
                               {ok, RcvCtrlBuf} ->
                                   ok;
                               {ok, InvalidRcvCtrlBuf} ->
                                   {error, {invalid, InvalidRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         %% *** Check rcvctrlbuf part ***
         #{desc => "get rcvctrlbuf",
           cmd  => fun(#{sock := Sock} = State) ->
                           case Get(Sock, rcvctrlbuf) of
                               {ok, RcvCtrlBuf} when is_integer(RcvCtrlBuf) ->
                                   {ok, State#{rcvctrlbuf => RcvCtrlBuf}};
                               {ok, InvalidRcvCtrlBuf} ->
                                   {error, {invalid, InvalidRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "set (new) rcvctrlbuf",
           cmd  => fun(#{sock := Sock, rcvctrlbuf := OldRcvCtrlBuf} = State) ->
                           NewRcvCtrlBuf = 2 * OldRcvCtrlBuf,
                           case Set(Sock, rcvctrlbuf, NewRcvCtrlBuf) of
                               ok ->
                                   {ok, State#{rcvctrlbuf => NewRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "get (new) rcvctrlbuf",
           cmd  => fun(#{sock := Sock, rcvctrlbuf := RcvCtrlBuf}) ->
                           case Get(Sock, rcvctrlbuf) of
                               {ok, RcvCtrlBuf} ->
                                   ok;
                               {ok, InvalidRcvCtrlBuf} ->
                                   {error, {invalid, InvalidRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** Check sndctrlbuf part ***
         #{desc => "get sndctrlbuf",
           cmd  => fun(#{sock := Sock} = State) ->
                           case Get(Sock, sndctrlbuf) of
                               {ok, SndCtrlBuf} when is_integer(SndCtrlBuf) ->
                                   {ok, State#{sndctrlbuf => SndCtrlBuf}};
                               {ok, InvalidSndCtrlBuf} ->
                                   {error, {invalid, InvalidSndCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "set (new) sndctrlbuf",
           cmd  => fun(#{sock := Sock, sndctrlbuf := OldSndCtrlBuf} = State) ->
                           NewSndCtrlBuf = 2 * OldSndCtrlBuf,
                           case Set(Sock, sndctrlbuf, NewSndCtrlBuf) of
                               ok ->
                                   {ok, State#{sndctrlbuf => NewSndCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "get (new) sndctrlbuf",
           cmd  => fun(#{sock := Sock, sndctrlbuf := SndCtrlBuf}) ->
                           case Get(Sock, sndctrlbuf) of
                               {ok, SndCtrlBuf} ->
                                   ok;
                               {ok, InvalidSndCtrlBuf} ->
                                   {error, {invalid, InvalidSndCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** Check controlling-process part ***
         #{desc => "verify self as controlling-process",
           cmd  => fun(#{sock := Sock}) ->
                           Self = self(),
                           case Get(Sock, controlling_process) of
                               {ok, Self} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "set dummy as controlling-process",
           cmd  => fun(#{sock := Sock, dummy := Dummy}) ->
                           Set(Sock, controlling_process, Dummy)
                   end},
         #{desc => "verify dummy as controlling-process",
           cmd  => fun(#{sock := Sock, dummy := Dummy}) ->
                           case Get(Sock, controlling_process) of
                               {ok, Dummy} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    i("start tcp (stream) evaluator"),
    InitState1 = #{domain => inet, type => stream, protocol => tcp},
    Tester1 = ?SEV_START("tcp-tester", Seq, InitState1),
    i("await tcp evaluator"),
    ok = ?SEV_AWAIT_FINISH([Tester1]),

    i("start udp (dgram) socket"),
    InitState2 = #{domain => inet, type => dgram, protocol => udp},
    Tester2 = ?SEV_START("udp-tester", Seq, InitState2),
    i("await udp evaluator"),
    ok = ?SEV_AWAIT_FINISH([Tester2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Perform some simple operations with the rcvbuf otp option
%% The operations we test here are only for type = stream and
%% protocol = tcp.
api_opt_simple_otp_rcvbuf_option(suite) ->
    [];
api_opt_simple_otp_rcvbuf_option(doc) ->
    [];
api_opt_simple_otp_rcvbuf_option(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    tc_try(api_opt_simple_otp_rcvbuf_option,
           fun() -> api_opt_simple_otp_rcvbuf_option() end).

api_opt_simple_otp_rcvbuf_option() ->
    Get = fun(S) ->
                  socket:getopt(S, otp, rcvbuf)
          end,
    Set = fun(S, Val) ->
                  socket:setopt(S, otp, rcvbuf, Val)
          end,

    ServerSeq = 
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
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
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, local_sa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
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
           cmd  => fun(#{tester   := Tester,
                         local_sa := LocalSA,
                         lport    := Port}) ->
                           ServerSA = LocalSA#{port => Port},
                           ?SEV_ANNOUNCE_READY(Tester, init, ServerSA),
                           ok
                   end},


         %% *** The actual test part ***
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "attempt to accept",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         %% Recv with default size for (otp) rcvbuf
         #{desc => "await continue (recv initial)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, recv) of
                               {ok, MsgSz} ->
                                   ?SEV_IPRINT("MsgSz: ~p", [MsgSz]),
                                   {ok, State#{msg_sz => MsgSz}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt to recv",
           cmd  => fun(#{sock := Sock, msg_sz := MsgSz} = _State) ->
                           ?SEV_IPRINT("try recv ~w bytes when rcvbuf is ~s", 
                                       [MsgSz,
                                        case Get(Sock) of
                                            {ok, RcvBuf} -> f("~w", [RcvBuf]);
                                            {error, _}   -> "-"
                                        end]),
                           case socket:recv(Sock) of
                               {ok, Data} when (size(Data) =:= MsgSz) ->
                                   ok;
                               {ok, Data} ->
                                   {error, {invalid_msg_sz, MsgSz, size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv initial)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},

         %% Recv with new size (1) for (otp) rcvbuf
         #{desc => "await continue (recv 1)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, recv) of
                               {ok, NewRcvBuf} ->
                                   ?SEV_IPRINT("set new rcvbuf: ~p", [NewRcvBuf]),
                                   {ok, State#{rcvbuf => NewRcvBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt to setopt rcvbuf",
           cmd  => fun(#{sock := Sock, rcvbuf := NewRcvBuf} = _State) ->
                           case Set(Sock, NewRcvBuf) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt to recv",
           cmd  => fun(#{sock := Sock, msg_sz := MsgSz} = _State) ->
                           case socket:recv(Sock) of
                               {ok, Data} when (size(Data) =:= MsgSz) ->
                                   ok;
                               {ok, Data} ->
                                   {error, {invalid_msg_sz, MsgSz, size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv 1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},

         %% Recv with new size (2) for (otp) rcvbuf
         #{desc => "await continue (recv 2)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, recv) of
                               {ok, NewRcvBuf} ->
                                   ?SEV_IPRINT("set new rcvbuf: ~p", [NewRcvBuf]),
                                   {ok, State#{rcvbuf => NewRcvBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt to setopt rcvbuf",
           cmd  => fun(#{sock := Sock, rcvbuf := NewRcvBuf} = _State) ->
                           case Set(Sock, NewRcvBuf) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt to recv",
           cmd  => fun(#{sock := Sock, msg_sz := MsgSz} = _State) ->
                           case socket:recv(Sock) of
                               {ok, Data} when (size(Data) =:= MsgSz) ->
                                   ok;
                               {ok, Data} ->
                                   {error, {invalid_msg_sz, MsgSz, size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv 2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},

         %% Recv with new size (3) for (otp) rcvbuf
         #{desc => "await continue (recv 3, truncated)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, recv) of
                               {ok, {ExpSz, NewRcvBuf}} ->
                                   {ok, State#{msg_sz => ExpSz,
                                               rcvbuf => NewRcvBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt to setopt rcvbuf",
           cmd  => fun(#{sock := Sock, rcvbuf := NewRcvBuf} = _State) ->
                           case Set(Sock, NewRcvBuf) of
                               ok ->
                                   ?SEV_IPRINT("set new rcvbuf: ~p", [NewRcvBuf]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt to recv",
           cmd  => fun(#{sock := Sock, msg_sz := MsgSz} = _State) ->
                           ?SEV_IPRINT("try recv ~w bytes of data", [MsgSz]),
                           case socket:recv(Sock) of
                               {ok, Data} when (size(Data) =:= MsgSz) ->
                                   ok;
                               {ok, Data} ->
                                   {error, {invalid_msg_sz, MsgSz, size(Data)}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},


         %% Termination
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket(s)",
           cmd  => fun(#{lsock := LSock, sock := Sock} = State) ->
                           sock_close(Sock),
                           sock_close(LSock),
                           State1 = maps:remove(sock,  State),
                           State2 = maps:remove(lport, State1),
                           State3 = maps:remove(lsock, State2),
                           {ok, State3}
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
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, local_sa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _Port} ->
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
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect)
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

         #{desc => "await continue (send initial)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, send) of
                               {ok, Data} ->
                                   {ok, State#{data => Data}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "send (initial) data to server",
           cmd  => fun(#{sock := Sock, data := Data} = _State) ->
                           ?SEV_IPRINT("try send ~w bytes", [size(Data)]),
                           socket:send(Sock, Data)
                   end},
         #{desc => "announce ready (send initial)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send),
                           ok
                   end},

         #{desc => "await continue (send 1)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send)
                   end},
         #{desc => "send (1) data to server",
           cmd  => fun(#{sock := Sock, data := Data}) ->
                           ?SEV_IPRINT("try send ~w bytes", [size(Data)]),
                           socket:send(Sock, Data)
                   end},
         #{desc => "announce ready (send 1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send),
                           ok
                   end},

         #{desc => "await continue (send 2)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send)
                   end},
         #{desc => "send (2) data to server",
           cmd  => fun(#{sock := Sock, data := Data}) ->
                           ?SEV_IPRINT("try send ~w bytes", [size(Data)]),
                           socket:send(Sock, Data)
                   end},
         #{desc => "announce ready (send 2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send),
                           ok
                   end},

         #{desc => "await continue (send 3)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send)
                   end},
         #{desc => "send (3) data to server",
           cmd  => fun(#{sock := Sock, data := Data}) ->
                           ?SEV_IPRINT("try send ~w bytes", [size(Data)]),
                           socket:send(Sock, Data)
                   end},
         #{desc => "announce ready (send 3)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send),
                           ok
                   end},


         %% Termination
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock}) ->
                           socket:close(Sock)
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Server} = _State) ->
                           _MRef = erlang:monitor(process, Server),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Client} = _State) ->
                           _MRef = erlang:monitor(process, Client),
                           ok
                   end},
         #{desc => "order server start",
           cmd  => fun(#{server := Server}) ->
                           ?SEV_ANNOUNCE_START(Server)
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Server} = State) ->
                           {ok, ServerSA} = ?SEV_AWAIT_READY(Server, server, init),
                           {ok, State#{server_sa => ServerSA}}
                   end},
         #{desc => "order client start",
           cmd  => fun(#{client    := Client,
                         server_sa := ServerSA}) ->
                           ?SEV_ANNOUNCE_START(Client, ServerSA),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, init)
                   end},


         %% The actual test (connecting)
         #{desc => "order server accept (accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client continue (connect)",
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

         %% The actual test (initial part)
         #{desc => "order client continue (send initial)",
           cmd  => fun(#{client := Client, data := Data} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Data),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order server continue (recv initial)",
           cmd  => fun(#{server := Server, data := Data} = _State) ->
                           ExpMsgSz = size(Data),
                           ?SEV_ANNOUNCE_CONTINUE(Server, recv, ExpMsgSz),
                           ok
                   end},
         #{desc => "await client ready (send initial)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, client, send,
                                                 [{server, Server}]) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await server ready (recv initial)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Server, client, recv,
                                                 [{client, Client}]) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% The actual test (part 1)
         #{desc => "order client continue (send 1)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order server continue (recv 1)",
           cmd  => fun(#{server := Server, data := Data} = _State) ->
                           MsgSz     = size(Data),
                           NewRcvBuf = {2 + (MsgSz div 1024), 1024},
                           ?SEV_ANNOUNCE_CONTINUE(Server, recv, NewRcvBuf),
                           ok
                   end},
         #{desc => "await client ready (send 1)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, client, send,
                                                 [{server, Server}]) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await server ready (recv 1)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Server, client, recv,
                                                 [{client, Client}]) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% The actual test (part 2)
         #{desc => "order client continue (send 2)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order server continue (recv 2)",
           cmd  => fun(#{server := Server, data := Data} = _State) ->
                           MsgSz     = size(Data),
                           NewRcvBuf = {2 + (MsgSz div 2048), 2048},
                           ?SEV_ANNOUNCE_CONTINUE(Server, recv, NewRcvBuf),
                           ok
                   end},
         #{desc => "await client ready (send 2)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, client, send,
                                                 [{server, Server}]) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await server ready (recv 2)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Server, client, recv,
                                                 [{client, Client}]) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% The actual test (part 3)
         #{desc => "order client continue (send 3)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order server continue (recv 3)",
           cmd  => fun(#{server := Server, data := Data} = _State) ->
                           MsgSz     = size(Data),
                           BufSz     = 2048,
                           N         = MsgSz div BufSz - 1,
                           NewRcvBuf = {N, BufSz},
                           ?SEV_ANNOUNCE_CONTINUE(Server, recv,
                                                  {N*BufSz, NewRcvBuf})
                   end},
         #{desc => "await client ready (send 3)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, client, send,
                                                 [{server, Server}]) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await server ready (recv 3)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Server, client, recv,
                                                 [{client, Client}]) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         ?SEV_SLEEP(?SECS(1)),

         %% *** Terminate server ***
         #{desc => "order client terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client down",
           cmd  => fun(#{client := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           State1 = maps:remove(client,    State),
                           {ok, State1}
                   end},
         #{desc => "order server terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server down",
           cmd  => fun(#{server := Server} = State) ->
                           ?SEV_AWAIT_TERMINATION(Server),
                           State1 = maps:remove(server,    State),
                           State2 = maps:remove(server_sa, State1),
                           {ok, State2}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    %% Create a data binary of 6*1024 bytes
    Data      = list_to_binary(lists:duplicate(6*4, lists:seq(0, 255))),
    InitState = #{domain => inet,
                  data   => Data},

    i("create server evaluator"),
    ServerInitState = #{domain => maps:get(domain, InitState)},
    Server          = ?SEV_START("server", ServerSeq, ServerInitState),

    i("create client evaluator"),
    ClientInitState = #{host   => local_host(),
                        domain => maps:get(domain, InitState)},
    Client          = ?SEV_START("client", ClientSeq, ClientInitState),

    i("create tester evaluator"),
    TesterInitState = InitState#{server => Server#ev.pid,
                                 client => Client#ev.pid},
    Tester          = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Perform some simple getopt and setopt with the level = otp options
api_opt_simple_otp_controlling_process(suite) ->
    [];
api_opt_simple_otp_controlling_process(doc) ->
    [];
api_opt_simple_otp_controlling_process(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_opt_simple_otp_controlling_process,
           fun() -> api_opt_simple_otp_controlling_process() end).

api_opt_simple_otp_controlling_process() ->
    Get = fun(S, Key) ->
                  socket:getopt(S, otp, Key)
          end,
    Set = fun(S, Key, Val) ->
                  socket:setopt(S, otp, Key, Val)
          end,

    ClientSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, Sock} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester,
                                       sock   => Sock}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester}) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "verify tester as controlling-process",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           case Get(Sock, controlling_process) of
                               {ok, Tester} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt invalid controlling-process transfer (to self)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           case Set(Sock, controlling_process, self()) of
                               {error, not_owner} ->
                                   ok;
                               ok ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (not owner)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, not_owner),
                           ok
                   end},
         #{desc => "await continue (owner)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, owner)
                   end},
         #{desc => "verify self as controlling-process",
           cmd  => fun(#{sock := Sock} = _State) ->
                           Self = self(),
                           case Get(Sock, controlling_process) of
                               {ok, Self} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt controlling-process transfer to tester",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           Set(Sock, controlling_process, Tester)
                   end},
         #{desc => "attempt invalid controlling-process transfer (to self)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           case Set(Sock, controlling_process, self()) of
                               {error, not_owner} ->
                                   ok;
                               ok ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (owner)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, owner),
                           ok

                   end},
         
         %% *** Termination ***
         #{desc => "await termination",
           cmd  => fun(#{tester := Tester} = State) ->
                           ?SEV_AWAIT_TERMINATE(Tester, tester),
                           State1 = maps:remove(tester, State),
                           State2 = maps:remove(sock, State1),
                           {ok, State2}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type,
                         protocol := Protocol} = State) ->
                           Sock = sock_open(Domain, Type, Protocol),
                           {ok, State#{sock => Sock}}
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Client} = _State) ->
                           _MRef = erlang:monitor(process, Client),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "verify self as controlling-process",
           cmd  => fun(#{sock := Sock} = _State) ->
                           Self = self(),
                           case Get(Sock, controlling_process) of
                               {ok, Self} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order (client) start",
           cmd  => fun(#{client := Client, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Client, Sock),
                           ok
                   end},
         #{desc => "await (client) ready (not owner)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, not_owner)
                   end},
         #{desc => "attempt controlling-process transfer to client",
           cmd  => fun(#{client := Client, sock := Sock} = _State) ->
                           Set(Sock, controlling_process, Client)
                   end},
         #{desc => "verify client as controlling-process",
           cmd  => fun(#{client := Client, sock := Sock} = _State) ->
                           case Get(Sock, controlling_process) of
                               {ok, Client} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt invalid controlling-process transfer (to self)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           case Set(Sock, controlling_process, self()) of
                               {error, not_owner} ->
                                   ok;
                               ok ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order (client) continue (owner)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, owner),
                           ok
                   end},
         #{desc => "await (client) ready (2)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, owner),
                           ok
                   end},
         #{desc => "verify self as controlling-process",
           cmd  => fun(#{sock := Sock} = _State) ->
                           Self = self(),
                           case Get(Sock, controlling_process) of
                               {ok, Self} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** Termination ***
         #{desc => "order (client) terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           {ok, maps:remove(client, State)}
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start tcp (stream) client evaluator"),
    ClientInitState1 = #{},
    Client1 = ?SEV_START("tcp-client", ClientSeq, ClientInitState1),

    i("start tcp (stream) tester evaluator"),
    TesterInitState1 = #{domain   => inet, 
                         type     => stream, 
                         protocol => tcp,
                         client   => Client1#ev.pid},
    Tester1 = ?SEV_START("tcp-tester", TesterSeq, TesterInitState1),

    i("await tcp evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([Tester1, Client1]),

    i("start udp (dgram) client evaluator"),
    ClientInitState2 = #{},
    Client2 = ?SEV_START("udp-client", ClientSeq, ClientInitState2),

    i("start udp (dgram) tester evaluator"),
    TesterInitState2 = #{domain   => inet, 
                         type     => dgram, 
                         protocol => udp,
                         client   => Client2#ev.pid},
    Tester2 = ?SEV_START("udp-tester", TesterSeq, TesterInitState2),

    i("await udp evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([Tester2, Client2]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                  API OPERATIONS WITH TIMEOUT                        %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the connect timeout option
%% on an IPv4 TCP (stream) socket.
api_to_connect_tcp4(suite) ->
    [];
api_to_connect_tcp4(doc) ->
    [];
api_to_connect_tcp4(_Config) when is_list(_Config) ->
    Cond = fun() -> api_to_connect_cond() end,
    tc_try(api_to_connect_tcp4,
           Cond,
           fun() ->
                   ?TT(?SECS(10)),
                   InitState = #{domain        => inet,
                                 backlog       => 1,
                                 timeout       => 5000,
                                 connect_limit => 3},
                   ok = api_to_connect_tcp(InitState)
           end).

api_to_connect_cond() ->
    api_to_connect_cond(os:type(), os:version()).

%% I don't know exactly at which version this starts to work.
%% I know it does not work for 4.4.*, but is does for 4.15.
%% So, just to simplify, we require atleast 4.15
api_to_connect_cond({unix, linux}, {Maj, Min, _Rev}) ->
    if
        ((Maj >= 4) andalso (Min >= 15)) ->
            ok;
        true ->
            skip("TC does not work")
    end;
%% Only test on one machine, which has version 6.3, and there it does
%% not work, so disable for all.
api_to_connect_cond({unix, openbsd}, _) ->
    skip("TC does not work");
api_to_connect_cond({unix, freebsd}, {Maj, Min, _Rev}) ->
    if
        ((Maj >= 10) andalso (Min >= 4)) ->
            ok;
        true ->
            skip("TC may not work")
    end;
api_to_connect_cond({unix, sunos}, {Maj, Min, _Rev}) ->
    if
        ((Maj >= 5) andalso (Min >= 10)) ->
            ok;
        true ->
            skip("TC may not work")
    end;
api_to_connect_cond(_, _) ->
    skip("TC may not work").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the connect timeout option
%% on an IPv6 TCP (stream) socket.
api_to_connect_tcp6(suite) ->
    [];
api_to_connect_tcp6(doc) ->
    [];
api_to_connect_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_connect_tcp6,
           fun() -> has_support_ipv6(), api_to_connect_cond() end,
           fun() ->
                   ?TT(?SECS(10)),
                   InitState = #{domain        => inet6,
                                 backlog       => 1,
                                 timeout       => 5000,
                                 connect_limit => 3},
                   ok = api_to_connect_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% We use the backlog (listen) argument to test this.
%% Note that the behaviour of the TCP "server side" can vary when 
%% a client connect to a "busy" server (full backlog).
%% For instance, on FreeBSD (11.2) the reponse when the backlog is full
%% is a econreset.

api_to_connect_tcp(InitState) ->
    process_flag(trap_exit, true),

    ServerSeq = 
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           {Tester, Backlog} = ?SEV_AWAIT_START(),
                           {ok, State#{tester  => Tester,
                                       backlog => Backlog}}
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
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, local_sa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket (with backlog = 1)",
           cmd  => fun(#{lsock := LSock, backlog := Backlog}) ->
                           socket:listen(LSock, Backlog)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, lport := Port}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{lsock := Sock} = State) ->
                           sock_close(Sock),
                           State1 = maps:remove(lport, State),
                           State2 = maps:remove(sock,  State1),
                           {ok, State2}
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
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "create node",
           cmd  => fun(#{host := Host} = State) ->
			   ?SEV_IPRINT("try create node on ~p", [Host]),
                           case start_node(Host, client) of
                               {ok, Node} ->
                                   ?SEV_IPRINT("client node ~p started",
                                               [Node]),
                                   {ok, State#{node => Node}};
                               {error, Reason} ->
                                   {skip, Reason}
                           end
                   end},
         #{desc => "monitor client node",
           cmd  => fun(#{node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "start remote client on client node",
           cmd  => fun(#{node := Node} = State) ->
                           Pid = api_toc_tcp_client_start(Node),
                           ?SEV_IPRINT("remote client ~p started", [Pid]),
                           {ok, State#{rclient => Pid}}
                   end},
         #{desc => "monitor remote client",
           cmd  => fun(#{rclient := Pid}) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order remote client to start",
           cmd  => fun(#{rclient   := Client,
                         server_sa := ServerSA}) ->
                           ?SEV_ANNOUNCE_START(Client, ServerSA),
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
                         rclient := Client} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, connect,
                                                    [{rclient, Client}]) of
                               {ok, {ConTimeout, ConLimit}} ->
                                   {ok, State#{connect_timeout => ConTimeout,
                                               connect_limit   => ConLimit}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to continue (connect)",
           cmd  => fun(#{rclient         := RClient,
                         connect_timeout := ConTimeout,
                         connect_limit   := ConLimit}) ->
                           ?SEV_ANNOUNCE_CONTINUE(RClient, connect,
                                                  {ConTimeout, ConLimit}),
                           ok
                   end},
         #{desc => "await remote client ready (connect)",
           cmd  => fun(#{tester  := Tester,
                         rclient := RClient} = State) ->
                           case ?SEV_AWAIT_READY(RClient, rclient, connect,
                                                 [{tester, Tester}]) of
                               {ok, ok = _Result} ->
                                   {ok, maps:remove(connect_limit, State)};
                               {ok, {error, {connect_limit_reached,R,L}}} ->
                                   {skip,
                                    ?LIB:f("Connect limit reached ~w: ~w",
                                           [L, R])};
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
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
           cmd  => fun(#{node := Node} = _State) ->
                           stop_node(Node)
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node} = State) ->
                           receive
                               {nodedown, Node} ->
                                   State1 = maps:remove(node_id, State),
                                   State2 = maps:remove(node,    State1),
                                   {ok, State2}
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Server} = _State) ->
                           _MRef = erlang:monitor(process, Server),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Client} = _State) ->
                           _MRef = erlang:monitor(process, Client),
                           ok
                   end},
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "order server start",
           cmd  => fun(#{server  := Server,
                         backlog := Backlog}) ->
                           ?SEV_ANNOUNCE_START(Server, Backlog),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Server, local_sa := LSA} = State) ->
                           {ok, Port} = ?SEV_AWAIT_READY(Server, server, init),
                           ServerSA = LSA#{port => Port},
                           {ok, State#{server_sa => ServerSA}}
                   end},
         #{desc => "order client start",
           cmd  => fun(#{client    := Client,
                         server_sa := ServerSA}) ->
                           ?SEV_ANNOUNCE_START(Client, ServerSA),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, init),
                           ok
                   end},

         %% The actual test
         %% The server does nothing (this is the point), no accept,
         %% the client tries to connect.
         #{desc => "order client continue (connect)",
           cmd  => fun(#{client        := Client,
                         timeout       := Timeout,
                         connect_limit := ConLimit} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect,
                                                  {Timeout, ConLimit}),
                           ok
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           case ?SEV_AWAIT_READY(Client, client, connect,
                                                 [{server, Server}]) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** Terminate server ***
         #{desc => "order client terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client down",
           cmd  => fun(#{client := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           State1 = maps:remove(client,    State),
                           {ok, State1}
                   end},
         #{desc => "order server terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server down",
           cmd  => fun(#{server := Server} = State) ->
                           ?SEV_AWAIT_TERMINATION(Server),
                           State1 = maps:remove(server,    State),
                           State2 = maps:remove(server_sa, State1),
                           {ok, State2}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("create server evaluator"),
    ServerInitState = #{domain => maps:get(domain, InitState)},
    Server          = ?SEV_START("server", ServerSeq, ServerInitState),

    i("create client evaluator"),
    ClientInitState = #{host   => local_host(),
                        domain => maps:get(domain, InitState)},
    Client          = ?SEV_START("client", ClientSeq, ClientInitState),

    i("create tester evaluator"),
    TesterInitState = InitState#{server => Server#ev.pid,
                                 client => Client#ev.pid},
    Tester          = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).


api_toc_tcp_client_start(Node) ->
    Self = self(),
    Fun  = fun() -> api_toc_tcp_client(Self) end,
    erlang:spawn(Node, Fun).

api_toc_tcp_client(Parent) ->
    api_toc_tcp_client_init(Parent),
    ServerSA = api_toc_tcp_client_await_start(Parent),
    Domain   = maps:get(family, ServerSA),
    api_toc_tcp_client_announce_ready(Parent, init),
    {To, ConLimit} = api_toc_tcp_client_await_continue(Parent, connect),
    Result = api_to_connect_tcp_await_timeout(To, ServerSA, Domain, ConLimit),
    ?SEV_IPRINT("result: ~p", [Result]),
    api_toc_tcp_client_announce_ready(Parent, connect, Result),
    Reason = api_toc_tcp_client_await_terminate(Parent),
    exit(Reason).

api_toc_tcp_client_init(Parent) ->
    put(sname, "rclient"),
    %% i("api_toc_tcp_client_init -> entry"),
    _MRef = erlang:monitor(process, Parent),
    ok.

api_toc_tcp_client_await_start(Parent) ->
    %% i("api_toc_tcp_client_await_start -> entry"),
    ?SEV_AWAIT_START(Parent).

api_toc_tcp_client_announce_ready(Parent, Slogan) ->
    ?SEV_ANNOUNCE_READY(Parent, Slogan).
api_toc_tcp_client_announce_ready(Parent, Slogan, Result) ->
    ?SEV_ANNOUNCE_READY(Parent, Slogan, Result).

api_toc_tcp_client_await_continue(Parent, Slogan) ->
    %% i("api_toc_tcp_client_await_continue -> entry"),
    case ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan) of
        ok ->
            ok;
        {ok, Extra} ->
            Extra;
        {error, Reason} ->
            exit({await_continue, Slogan, Reason})
    end.

api_toc_tcp_client_await_terminate(Parent) ->
    %% i("api_toc_tcp_client_await_terminate -> entry"),
    case ?SEV_AWAIT_TERMINATE(Parent, parent) of
        ok ->
            ok;
        {error, Reason} ->
            Reason
    end.

api_to_connect_tcp_await_timeout(To, ServerSA, Domain, ConLimit) ->
    LSA = which_local_socket_addr(Domain),
    NewSock = fun() ->
                      S = case socket:open(Domain, stream, tcp) of
                              {ok, Sock} ->
                                  Sock;
                              {error, OReason} ->
                                  ?FAIL({open, OReason})
                          end,
                      case socket:bind(S, LSA) of
                          {ok, _} ->
                              S;
                          {error, BReason} ->
                              ?FAIL({bind, BReason})
                      end
              end,
    api_to_connect_tcp_await_timeout(1, ConLimit, To, ServerSA, NewSock, []).

api_to_connect_tcp_await_timeout(ID, ConLimit, _To, _ServerSA, _NewSock, Acc)
  when (ID > ConLimit) ->
    api_to_connect_tcp_await_timeout3(Acc),
    {error, {connect_limit_reached, ID, ConLimit}};
api_to_connect_tcp_await_timeout(ID, ConLimit, To, ServerSA, NewSock, Acc) ->
    case api_to_connect_tcp_await_timeout2(ID, To, ServerSA, NewSock) of
        ok ->
            %% ?SEV_IPRINT("success when number of socks: ~w", [length(Acc)]),
            api_to_connect_tcp_await_timeout3(Acc),
            ok;
        {ok, Sock} ->
            %% ?SEV_IPRINT("~w: unexpected success (connect)", [ID]),
            api_to_connect_tcp_await_timeout(ID+1, ConLimit,
                                             To, ServerSA, NewSock,
                                             [Sock|Acc]);
        {error, _} = ERROR ->
            ERROR
    end.

api_to_connect_tcp_await_timeout2(_ID, To, ServerSA, NewSock) ->
    Sock = NewSock(),
    %% ?SEV_IPRINT("~w: try connect", [ID]),
    Start = t(),
    case socket:connect(Sock, ServerSA, To) of
        {error, timeout} ->
            Stop  = t(),
            TDiff = Stop - Start,
            if
                (TDiff >= To) ->
                    (catch socket:close(Sock)),
                    ok;
                true ->
                    (catch socket:close(Sock)),
                    ?FAIL({unexpected_timeout, TDiff, To})
            end;
        {error, econnreset = _Reason} ->
            (catch socket:close(Sock)),
            ok;
        {error, Reason} ->
            (catch socket:close(Sock)),
            ?FAIL({connect, Reason});
        ok ->
            {ok, Sock}
    end.

api_to_connect_tcp_await_timeout3([]) ->
    ok;
api_to_connect_tcp_await_timeout3([Sock|Socka]) ->
    (catch socket:close(Sock)),
    api_to_connect_tcp_await_timeout3(Socka).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the accept timeout option
%% on an IPv4 TCP (stream) socket.
api_to_accept_tcp4(suite) ->
    [];
api_to_accept_tcp4(doc) ->
    [];
api_to_accept_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_accept_tcp4,
           fun() ->
                   ?TT(?SECS(10)),
                   InitState = #{domain => inet, timeout => 5000},
                   ok = api_to_accept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the accept timeout option
%% on an IPv6 TCP (stream) socket.
api_to_accept_tcp6(suite) ->
    [];
api_to_accept_tcp6(doc) ->
    [];
api_to_accept_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_accept_tcp4,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   InitState = #{domain => inet6, timeout => 5000},
                   ok = api_to_accept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_accept_tcp(InitState) ->
    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, lsa := LSA} = _State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},

         %% *** The actual test part ***
         #{desc => "attempt to accept (without success)",
           cmd  => fun(#{lsock := LSock, timeout := To} = State) ->
                           Start = t(),
                           case socket:accept(LSock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start, stop => t()}};
                               {ok, Sock} ->
                                   (catch socket:close(Sock)),
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate timeout time",
           cmd  => fun(#{start := Start, stop := Stop, timeout := To} = _State) ->
                           TDiff  = Stop - Start,
                           if
                               (TDiff >= To) ->
                                   ok;
                               true ->
                                   {error, {unexpected_timeout, TDiff, To}}
                           end
                   end},

         %% *** Close (listen) socket ***
         #{desc => "close (listen) socket",
           cmd  => fun(#{lsock := LSock} = State) ->
                           sock_close(LSock),
                           {ok, maps:remove(sock3, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("create tester evaluator"),
    Tester = ?SEV_START("tester", TesterSeq, InitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the multi accept timeout option
%% on an IPv4 TCP (stream) socket with multiple acceptor processes 
%% (three in this case).
api_to_maccept_tcp4(suite) ->
    [];
api_to_maccept_tcp4(doc) ->
    [];
api_to_maccept_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(20)),
    tc_try(api_to_maccept_tcp4,
           fun() ->
                   InitState = #{domain => inet, timeout => 5000},
                   ok = api_to_maccept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the accept timeout option
%% on an IPv6 TCP (stream) socket.
api_to_maccept_tcp6(suite) ->
    [];
api_to_maccept_tcp6(doc) ->
    [];
api_to_maccept_tcp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(20)),
    tc_try(api_to_maccept_tcp4,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain => inet6, timeout => 5000},
                   ok = api_to_maccept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_maccept_tcp(InitState) ->
    PrimAcceptorSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, lsa := LSA} = _State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{lsock := LSock, tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, LSock),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "attempt to accept (without success)",
           cmd  => fun(#{lsock := LSock, timeout := To} = State) ->
                           Start = t(),
                           case socket:accept(LSock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start, stop => t()}};
                               {ok, Sock} ->
                                   ?SEV_EPRINT("Unexpected accept success: "
                                               "~n   ~p", [Sock]),
                                   (catch socket:close(Sock)),
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate timeout time",
           cmd  => fun(#{start := Start, stop := Stop, timeout := To} = _State) ->
                           TDiff  = Stop - Start,
                           if
                               (TDiff >= To) ->
                                   ok;
                               true ->
                                   {error, {unexpected_timeout, TDiff, To}}
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_TERMINATE(Tester, tester),
                           ok
                   end},
         %% *** Close (listen) socket ***
         #{desc => "close (listen) socket",
           cmd  => fun(#{lsock := LSock} = State) ->
                           sock_close(LSock),
                           {ok, maps:remove(lsock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    SecAcceptorSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, LSock} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester,
                                       lsock  => LSock}}
                           
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% *** The actual test part ***
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "attempt to accept (without success)",
           cmd  => fun(#{lsock := LSock, timeout := To} = State) ->
                           Start = t(),
                           case socket:accept(LSock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start, stop => t()}};
                               {ok, Sock} ->
                                   (catch socket:close(Sock)),
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate timeout time",
           cmd  => fun(#{start := Start, stop := Stop, timeout := To} = State) ->
                           TDiff  = Stop - Start,
                           if
                               (TDiff >= To) ->
                                   State1 = maps:remove(start, State),
                                   State2 = maps:remove(stop,  State1),
                                   {ok, State2};
                               true ->
                                   {error, {unexpected_timeout, TDiff, To}}
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    TesterSeq =
        [
         %% Init part
         #{desc => "monitor prim-acceptor",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},


         %% Start the prim-acceptor
         #{desc => "start prim-acceptor",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await prim-acceptor ready (init)",
           cmd  => fun(#{prim_acceptor := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, prim_acceptor, init),
                           {ok, State#{lsock => Sock}}
                   end},

         %% Start sec-acceptor-1
         #{desc => "start sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid, lsock := LSock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, LSock),
                           ok
                   end},
         #{desc => "await sec-acceptor 1 ready (init)",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor1, init)
                   end},

         %% Start sec-acceptor-2
         #{desc => "start sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid, lsock := LSock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, LSock),
                           ok
                   end},
         #{desc => "await sec-acceptor 2 ready (init)",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor2, init)
                   end},

         %% Activate the acceptor(s)
         #{desc => "active prim-acceptor",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         #{desc => "active sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         #{desc => "active sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},

         %% Await acceptor(s) completions
         #{desc => "await prim-acceptor ready (accept)",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, prim_acceptor, accept)
                   end},
         #{desc => "await sec-acceptor 1 ready (accept)",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor1, accept)
                   end},
         #{desc => "await sec-acceptor 2 ready (accept)",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor2, accept)
                   end},

         %% Terminate
         #{desc => "order prim-acceptor to terminate",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await prim-acceptor termination",
           cmd  => fun(#{prim_acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(prim_acceptor, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order sec-acceptor 1 to terminate",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await sec-acceptor 1 termination",
           cmd  => fun(#{sec_acceptor1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(sec_acceptor1, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order sec-acceptor 2 to terminate",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await sec-acceptor 2 termination",
           cmd  => fun(#{sec_acceptor2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(sec_acceptor2, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         
         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("create prim-acceptor evaluator"),
    PrimAInitState = InitState,
    PrimAcceptor = ?SEV_START("prim-acceptor", PrimAcceptorSeq, PrimAInitState),

    i("create sec-acceptor 1 evaluator"),
    SecAInitState1 = maps:remove(domain, InitState),
    SecAcceptor1 = ?SEV_START("sec-acceptor-1", SecAcceptorSeq, SecAInitState1),
    
    i("create sec-acceptor 2 evaluator"),
    SecAInitState2 = SecAInitState1,
    SecAcceptor2 = ?SEV_START("sec-acceptor-2", SecAcceptorSeq, SecAInitState2),

    i("create tester evaluator"),
    TesterInitState = #{prim_acceptor => PrimAcceptor#ev.pid,
                        sec_acceptor1 => SecAcceptor1#ev.pid,
                        sec_acceptor2 => SecAcceptor2#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([PrimAcceptor, SecAcceptor1, SecAcceptor2, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the send timeout option
%% on an IPv4 TCP (stream) socket.
api_to_send_tcp4(suite) ->
    [];
api_to_send_tcp4(doc) ->
    [];
api_to_send_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_send_tcp4,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_send_tcp(inet)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the send timeout option
%% on an IPv6 TCP (stream) socket.
api_to_send_tcp6(suite) ->
    [];
api_to_send_tcp6(doc) ->
    [];
api_to_send_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_send_tcp6,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_send_tcp(inet6)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendto timeout option
%% on an IPv4 UDP (dgram) socket.
api_to_sendto_udp4(suite) ->
    [];
api_to_sendto_udp4(doc) ->
    [];
api_to_sendto_udp4(_Config) when is_list(_Config) ->
    tc_try(api_to_sendto_udp4,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_sendto_to_udp(inet)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendto timeout option
%% on an IPv6 UDP (dgram) socket.
api_to_sendto_udp6(suite) ->
    [];
api_to_sendto_udp6(doc) ->
    [];
api_to_sendto_udp6(_Config) when is_list(_Config) ->
    tc_try(api_to_sendto_udp6,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_sendto_to_udp(inet6)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendmsg timeout option
%% on an IPv4 TCP (stream) socket.
api_to_sendmsg_tcp4(suite) ->
    [];
api_to_sendmsg_tcp4(doc) ->
    [];
api_to_sendmsg_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_sendmsg_tcp4,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_sendmsg_tcp(inet)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendmsg timeout option
%% on an IPv6 TCP (stream) socket.
api_to_sendmsg_tcp6(suite) ->
    [];
api_to_sendmsg_tcp6(doc) ->
    [];
api_to_sendmsg_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_sendmsg_tcp6,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_sendmsg_tcp(inet6)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv4 UDP (dgram) socket. To test this we must connect
%% the socket.
api_to_recv_udp4(suite) ->
    [];
api_to_recv_udp4(doc) ->
    [];
api_to_recv_udp4(_Config) when is_list(_Config) ->
    tc_try(api_to_recv_udp4,
           fun() ->
                   not_yet_implemented()%%,
                   %%ok = api_to_recv_udp(inet)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv6 UDP (dgram) socket. To test this we must connect
%% the socket.
api_to_recv_udp6(suite) ->
    [];
api_to_recv_udp6(doc) ->
    [];
api_to_recv_udp6(_Config) when is_list(_Config) ->
    tc_try(api_to_recv_udp6,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_recv_udp(inet6)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv4 TCP (stream) socket.
api_to_recv_tcp4(suite) ->
    [];
api_to_recv_tcp4(doc) ->
    [];
api_to_recv_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_recv_tcp4,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv = fun(Sock, To) -> socket:recv(Sock, 0, To) end,
                   InitState = #{domain  => inet,
                                 recv    => Recv,
                                 timeout => 2000},
                   ok = api_to_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv6 TCP (stream) socket.
api_to_recv_tcp6(suite) ->
    [];
api_to_recv_tcp6(doc) ->
    [];
api_to_recv_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_recv_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   case socket:supports(ipv6) of
                       true ->
                           ?TT(?SECS(10)),
                           Recv = fun(Sock, To) -> 
                                          socket:recv(Sock, 0, To)
                                  end,
                           InitState = #{domain  => inet6,
                                         recv    => Recv,
                                         timeout => 2000},
                           ok = api_to_receive_tcp(InitState);
                       false ->
                           skip("ipv6 not supported")
                   end
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_receive_tcp(InitState) ->
    process_flag(trap_exit, true),

    ServerSeq = 
        [
         %% *** Wait for start order ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester}) ->
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
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, local_sa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket (with backlog = 1)",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock, 1)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, lport := Port}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (accept and recv)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept_recv)
                   end},
         #{desc => "attempt accept",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt to recv (without success)",
           cmd  => fun(#{sock := Sock, recv := Recv, timeout := To} = State) ->
                           Start = t(),
                           case Recv(Sock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start, stop => t()}};
                               {ok, _Data} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate timeout time",
           cmd  => fun(#{start := Start, stop := Stop, timeout := To} = State) ->
                           TDiff  = Stop - Start,
                           if
                               (TDiff >= To) ->
                                   State1 = maps:remove(start, State),
                                   State2 = maps:remove(stop,  State1),
                                   {ok, State2};
                               true ->
                                   {error, {unexpected_timeout, TDiff, To}}
                           end
                   end},
         #{desc => "announce ready (recv timeout success)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept_recv),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close (traffic) socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},
         #{desc => "close (listen) socket",
           cmd  => fun(#{lsock := LSock} = State) ->
                           sock_close(LSock),
                           {ok, maps:remove(lsock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           {Tester, Port} = ?SEV_AWAIT_START(),
                           {ok, State#{tester      => Tester,
                                       server_port => Port}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain, server_port := Port} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           SSA = LSA#{port => Port},
                           {ok, State#{local_sa => LSA, server_sa => SSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, local_sa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (with connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect)
                   end},
         #{desc => "connect",
           cmd  => fun(#{sock := Sock, server_sa := SSA}) ->
                           sock_connect(Sock, SSA),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Server} = _State) ->
                           _MRef = erlang:monitor(process, Server),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Client} = _State) ->
                           _MRef = erlang:monitor(process, Client),
                           ok
                   end},

         %% *** Activate server ***
         #{desc => "start server",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_START(Server),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Server} = State) ->
                           {ok, Port} = ?SEV_AWAIT_READY(Server, server, init),
                           {ok, State#{server_port => Port}}
                   end},
         #{desc => "order server to continue (with accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, accept_recv),
                           ok
                   end},

         %% *** Activate client ***
         #{desc => "start client",
           cmd  => fun(#{client := Client, server_port := Port} = _State) ->
                           ?SEV_ANNOUNCE_START(Client, Port),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, init)
                   end},

         %% *** The actual test ***
         #{desc => "order client to continue (with connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect),
                           ok
                   end},
         #{desc => "await server ready (accept/recv)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept_recv)
                   end},

         %% *** Termination ***
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Client) of
                               ok ->
                                   State1 = maps:remove(client, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Server} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Server) of
                               ok ->
                                   State1 = maps:remove(server, State),
                                   State2 = maps:remove(server_port, State1),
                                   {ok, State2};
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

    i("start client evaluator"),
    ClientInitState = InitState,
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start tester evaluator"),
    TesterInitState = #{server => Server#ev.pid, 
                        client => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvfrom timeout option
%% on an IPv4 UDP (dgram) socket.
api_to_recvfrom_udp4(suite) ->
    [];
api_to_recvfrom_udp4(doc) ->
    [];
api_to_recvfrom_udp4(_Config) when is_list(_Config) ->
    tc_try(api_to_recvfrom_udp4,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv = fun(Sock, To) -> socket:recvfrom(Sock, 0, To) end,
                   InitState = #{domain  => inet,
                                 recv    => Recv,
                                 timeout => 2000},
                   ok = api_to_receive_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvfrom timeout option
%% on an IPv6 UDP (dgram) socket.
api_to_recvfrom_udp6(suite) ->
    [];
api_to_recvfrom_udp6(doc) ->
    [];
api_to_recvfrom_udp6(_Config) when is_list(_Config) ->
    tc_try(api_to_recvfrom_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv = fun(Sock, To) -> socket:recvfrom(Sock, 0, To) end,
                   InitState = #{domain  => inet6,
                                 recv    => Recv,
                                 timeout => 2000},
                   ok = api_to_receive_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_receive_udp(InitState) ->
    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, dgram, udp) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, lsa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _Port} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** The actual test ***
         #{desc => "attempt to read (without success)",
           cmd  => fun(#{sock := Sock, recv := Recv, timeout := To} = State) ->
                           Start = t(),
                           case Recv(Sock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start,
                                               stop => t()}};
                               {ok, _} ->
                                   {error, unexpected_sucsess};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate timeout time",
           cmd  => fun(#{start := Start, stop := Stop, timeout := To} = _State) ->
                           TDiff  = Stop - Start,
                           if
                               (TDiff >= To) ->
                                   ok;
                               true ->
                                   {error, {unexpected_timeout, TDiff, To}}
                           end
                   end},
         
         %% *** Termination ***
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = _State) ->
                           %% socket:setopt(Sock, otp, debug, true),
                           sock_close(Sock),
                           ok
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start tester evaluator"),
    Tester = ?SEV_START("tester", TesterSeq, InitState),
    
    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv4 UDP (dgram) socket.
api_to_recvmsg_udp4(suite) ->
    [];
api_to_recvmsg_udp4(doc) ->
    [];
api_to_recvmsg_udp4(_Config) when is_list(_Config) ->
    tc_try(api_to_recvmsg_udp4,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain  => inet,
                                 recv    => Recv,
                                 timeout => 2000},
                   ok = api_to_receive_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv6 UDP (dgram) socket.
api_to_recvmsg_udp6(suite) ->
    [];
api_to_recvmsg_udp6(doc) ->
    [];
api_to_recvmsg_udp6(_Config) when is_list(_Config) ->
    tc_try(api_to_recvmsg_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain  => inet6,
                                 recv    => Recv,
                                 timeout => 2000},
                   ok = api_to_receive_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv4 TCP (stream) socket.
api_to_recvmsg_tcp4(suite) ->
    [];
api_to_recvmsg_tcp4(doc) ->
    [];
api_to_recvmsg_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_recvmsg_tcp4,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain  => inet,
                                 recv    => Recv,
                                 timeout => 2000},
                   ok = api_to_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv6 TCP (stream) socket.
api_to_recvmsg_tcp6(suite) ->
    [];
api_to_recvmsg_tcp6(doc) ->
    [];
api_to_recvmsg_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_recvmsg_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain  => inet6,
                                 recv    => Recv,
                                 timeout => 2000},
                   ok = api_to_receive_tcp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                         SOCKET CLOSURE                              %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% ("removed") when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv4 TCP (stream) socket.

sc_cpe_socket_cleanup_tcp4(suite) ->
    [];
sc_cpe_socket_cleanup_tcp4(doc) ->
    [];
sc_cpe_socket_cleanup_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_cpe_socket_cleanup_tcp4,
           fun() ->
                   ?TT(?SECS(5)),
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% ("removed") when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv6 TCP (stream) socket.

sc_cpe_socket_cleanup_tcp6(suite) ->
    [];
sc_cpe_socket_cleanup_tcp6(doc) ->
    [];
sc_cpe_socket_cleanup_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_cpe_socket_cleanup_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(5)),
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% ("removed") when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv4 UDP (dgram) socket.

sc_cpe_socket_cleanup_udp4(suite) ->
    [];
sc_cpe_socket_cleanup_udp4(doc) ->
    [];
sc_cpe_socket_cleanup_udp4(_Config) when is_list(_Config) ->
    tc_try(sc_cpe_socket_cleanup_udp4,
           fun() ->
                   ?TT(?SECS(5)),
                   InitState = #{domain   => inet,
                                 type     => dgram,
                                 protocol => udp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% (removed) when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv6 UDP (dgram) socket.

sc_cpe_socket_cleanup_udp6(suite) ->
    [];
sc_cpe_socket_cleanup_udp6(doc) ->
    [];
sc_cpe_socket_cleanup_udp6(_Config) when is_list(_Config) ->
    tc_try(sc_cpe_socket_cleanup_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(5)),
                   InitState = #{domain   => inet6,
                                 type     => dgram,
                                 protocol => udp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_cpe_socket_cleanup(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
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
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% *** The actual test ***
         %% We *intentially* leave the socket "as is", no explicit close
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor owner",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock => Sock}}
                   end},
         #{desc => "verify owner as controlling-process",
           cmd  => fun(#{owner := Pid, sock := Sock} = _State) ->
                           case socket:getopt(Sock, otp, controlling_process) of
                               {ok, Pid} ->
                                   ok;
                               {ok, Other} ->
                                   {error, {unexpected_owner, Other}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = _State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         %% The reason we get closed, is that as long as there is a ref to 
         %% the resource (socket), then it will not be garbage collected.
         #{desc => "verify no socket (closed)",
           cmd  => fun(#{owner := Pid, sock := Sock} = _State) ->
                           case socket:getopt(Sock, otp, controlling_process) of
                               {ok, OtherPid} ->
                                   {error, {unexpected_success, Pid, OtherPid}};
                               {error, closed} ->
                                   ok;
                               {error, Reason} ->
                                   ?SEV_IPRINT("expected failure: ~p", [Reason]),
                                   {error, {unexpected_failure, Reason}}
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner => Owner#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while a process is calling the recv function.
%% Socket is IPv4.
%% 
%% <KOLLA>
%% 
%% We should really have a similar test cases for when the controlling
%% process exits and there are other processes in recv, accept, and 
%% all the other functions.
%% 
%% </KOLLA>

sc_lc_recv_response_tcp4(suite) ->
    [];
sc_lc_recv_response_tcp4(doc) ->
    [];
sc_lc_recv_response_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recv_response_tcp4,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recv function.
%% Socket is IPv6.

sc_lc_recv_response_tcp6(suite) ->
    [];
sc_lc_recv_response_tcp6(doc) ->
    [];
sc_lc_recv_response_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recv_response_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_lc_receive_response_tcp(InitState) ->
    %% This (acceptor) is the server that accepts connections.
    %% But it is also suppose to close the connection socket, 
    %% and trigger the read failure (=closed) for the handler process.
    AcceptorSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
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
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, local_sa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
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
           cmd  => fun(#{tester := Tester, lport := Port}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},
                           
         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, accept) of
                               {ok, {H1, H2, H3}} ->
                                   {ok, State#{handler1 => H1,
                                               handler2 => H2,
                                               handler3 => H3}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await accept",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("connection accepted"),
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
         #{desc => "transfer connection to handler 1",
           cmd  => fun(#{handler1 := Handler, csock := Sock}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Handler, transfer, Sock),
                           ok
                   end},
         #{desc => "transfer connection to handler 2",
           cmd  => fun(#{handler2 := Handler, csock := Sock}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Handler, transfer, Sock),
                           ok
                   end},
         #{desc => "transfer connection to handler 3",
           cmd  => fun(#{handler3 := Handler, csock := Sock}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Handler, transfer, Sock),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close),
                           ok
                   end},
         #{desc => "close the connection socket",
           cmd  => fun(#{csock := Sock} = State) ->
                           %% ok = socket:setopt(Sock, otp, debug, true),
                           case socket:close(Sock) of
                               ok ->
                                   {ok, maps:remove(csock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{lsock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   State1 = maps:remove(lsock, State),
                                   State2 = maps:remove(lport, State1),
                                   {ok, State2};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    %% The point of this is to perform the recv for which
    %% we are testing the reponse.
    HandlerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           {Tester, Acceptor} = ?SEV_AWAIT_START(),
                           {ok, State#{tester  => Tester, 
                                      acceptor => Acceptor}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "monitor acceptor",
           cmd  => fun(#{acceptor := Acceptor} = _State) ->
                           _MRef = erlang:monitor(process, Acceptor),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (transfer)",
           cmd  => fun(#{acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Pid, acceptor, transfer) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (transfer)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, transfer),
                           ok
                   end},
         #{desc => "attempt recv (=> closed)",
           cmd  => fun(#{sock := Sock, recv := Recv} = State) ->
                           %% ok = socket:setopt(Sock, otp, debug, true),
                           case Recv(Sock) of
                               {ok, _Data} ->
                                   ?SEV_EPRINT("Unexpected data received"),
                                   {error, unexpected_success};
                               {error, closed} ->
                                   ?SEV_IPRINT("received expected 'closed' "
                                               "result"),
                                   State1 = maps:remove(sock, State),
                                   {ok, State1};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Unexpected read failure: "
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv closed)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_closed),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    %% The point of this is basically just to create the connection.
    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
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
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind socket to local address",
           cmd  => fun(#{sock := Sock, local_sa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester := Tester, local_sa := LSA} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, connect) of
                               {ok, Port} ->
                                   ServerSA = LSA#{port => Port},
                                   {ok, State#{server_sa => ServerSA}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "connect to server",
           cmd  => fun(#{sock := Sock, server_sa := ServerSA}) ->
                           socket:connect(Sock, ServerSA)
                   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end                           
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor acceptor",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor handler 1",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor handler 2",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor handler 3",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the acceptor
         #{desc => "order acceptor start",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await acceptor ready (init)",
           cmd  => fun(#{acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_READY(Pid, acceptor, init) of
                               {ok, Port} ->
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% Start the handler(s)
         #{desc => "order handler 1 start",
           cmd  => fun(#{acceptor := Acceptor, handler1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Acceptor),
                           ok
                   end},
         #{desc => "await handler 1 ready (init)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, handler1, init)
                   end},
         #{desc => "order handler 2 start",
           cmd  => fun(#{acceptor := Acceptor, handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Acceptor),
                           ok
                   end},
         #{desc => "await handler 2 ready (init)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, handler2, init)
                   end},
         #{desc => "order handler 3 start",
           cmd  => fun(#{acceptor := Acceptor, handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Acceptor),
                           ok
                   end},
         #{desc => "await handler 3 ready (init)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, handler3, init)
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, client, init)
                   end},

         %% The actual test
         #{desc => "order acceptor to continue (accept)",
           cmd  => fun(#{acceptor := Pid, 
                         handler1 := H1, 
                         handler2 := H2, 
                         handler3 := H3} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept, {H1, H2, H3}),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to continue (connect)",
           cmd  => fun(#{client := Pid, lport := Port} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect, Port),
                           ok
                   end},
         #{desc => "await acceptor ready (accept)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, acceptor, accept)
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, connect)
                   end},
         #{desc => "await handler 1 ready (transfer)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler1, transfer)
                   end},
         #{desc => "await handler 2 ready (transfer)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler2, transfer)
                   end},
         #{desc => "await handler 3 ready (transfer)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler3, transfer)
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order acceptor to continue (close connection socket)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await acceptor ready (close)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, acceptor, close)
                   end},
         #{desc => "await handler 1 ready (recv closed)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler1, recv_closed)
                   end},
         #{desc => "await handler 2 ready (recv closed)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler2, recv_closed)
                   end},
         #{desc => "await handler 3 ready (recv closed)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler3, recv_closed)
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
                                   {ok, maps:remove(client, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler 1 to terminate",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 1 termination",
           cmd  => fun(#{handler1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(handler1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler 2 to terminate",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 2 termination",
           cmd  => fun(#{handler2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(handler2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler 3 to terminate",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 3 termination",
           cmd  => fun(#{handler3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(handler3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order acceptor to terminate",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await acceptor termination",
           cmd  => fun(#{acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(acceptor, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start acceptor evaluator"),
    AccInitState = InitState,
    Acceptor = ?SEV_START("acceptor", AcceptorSeq, AccInitState),

    i("start handler 1 evaluator"),
    HandlerInitState = #{recv => maps:get(recv, InitState)},
    Handler1 = ?SEV_START("handler-1", HandlerSeq, HandlerInitState),

    i("start handler 2 evaluator"),
    Handler2 = ?SEV_START("handler-2", HandlerSeq, HandlerInitState),

    i("start handler 3 evaluator"),
    Handler3 = ?SEV_START("handler-3", HandlerSeq, HandlerInitState),

    i("start client evaluator"),
    ClientInitState = InitState,
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start tester evaluator"),
    TesterInitState = #{acceptor => Acceptor#ev.pid,
                        handler1 => Handler1#ev.pid,
                        handler2 => Handler2#ev.pid,
                        handler3 => Handler3#ev.pid,
                        client   => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Acceptor, 
                            Handler1, Handler2, Handler3, 
                            Client, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while a process is calling the recvfrom function.
%% Socket is IPv4.
%% 

sc_lc_recvfrom_response_udp4(suite) ->
    [];
sc_lc_recvfrom_response_udp4(doc) ->
    [];
sc_lc_recvfrom_response_udp4(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recvfrom_response_udp4,
           fun() ->
                   ?TT(?SECS(30)),
                   Recv      = fun(Sock, To) -> socket:recvfrom(Sock, [], To) end,
                   InitState = #{domain   => inet,
                                 type     => dgram,
                                 protocol => udp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recv function.
%% Socket is IPv6.

sc_lc_recvfrom_response_udp6(suite) ->
    [];
sc_lc_recvfrom_response_udp6(doc) ->
    [];
sc_lc_recvfrom_response_udp6(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recvfrom_response_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(30)),
                   Recv      = fun(Sock, To) -> socket:recvfrom(Sock, [], To) end,
                   InitState = #{domain => inet6,
                                 recv   => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_lc_receive_response_udp(InitState) ->
    PrimServerSeq =
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
         #{desc => "local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "open socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           Sock = sock_open(Domain, dgram, udp),
                           SA   = sock_sockname(Sock),
                           {ok, State#{sock => Sock, sa => SA}}
                   end},
         #{desc => "bind socket",
           cmd  => fun(#{sock := Sock, local_sa := LSA}) ->
                           sock_bind(Sock, LSA),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (recv, with timeout)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, recv) of
                               {ok, Timeout} ->
                                   {ok, State#{timeout => Timeout}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "receive, with timeout",
           cmd  => fun(#{sock := Sock, recv := Recv, timeout := Timeout}) ->
                           case Recv(Sock, Timeout) of
                               {error, timeout} ->
                                   ok;
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv, with timeout)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, close)
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   {ok, maps:remove(sock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, terminate) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    SecServerSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, Sock} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, sock => Sock}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (recv)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                           
                   end},
         #{desc => "receive",
           cmd  => fun(#{sock := Sock, recv := Recv} = State) ->
                           case Recv(Sock, infinity) of
                               {error, closed} ->
                                   {ok, maps:remove(sock, State)};
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv closed)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_closed),
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

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor primary server",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary server 1",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary server 2",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary server 3",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the primary server
         #{desc => "order 'primary server' start",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await 'primary server' ready (init)",
           cmd  => fun(#{prim_server := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, prim_server, init),
                           {ok, State#{sock => Sock}}
                   end},

         %% Start the secondary server 1
         #{desc => "order 'secondary server 1' start",
           cmd  => fun(#{sec_server1 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary server 1' ready (init)",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_server1, init)
                   end},

         %% Start the secondary server 2
         #{desc => "order 'secondary server 2' start",
           cmd  => fun(#{sec_server2 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary server 2' ready (init)",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_server2, init)
                   end},

         %% Start the secondary server 3
         #{desc => "order 'secondary server 3' start",
           cmd  => fun(#{sec_server3 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary server 3' ready (init)",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_server3, init)
                   end},


         %% The actual test
         %% Make all the seondary servers continue, with an infinit recvfrom
         %% and then the prim-server with a timed recvfrom.
         %% After the prim server notifies us (about the timeout) we order it
         %% to close the socket, which should cause the all the secondary 
         %% server to return with error-closed.

         #{desc => "order 'secondary server 1' to continue (recv)",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary server 2' to continue (recv)",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary server 3' to continue (recv)",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'primary server' to continue (recv, with timeout)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv, ?SECS(5)),
                           ok
                   end},
         #{desc => "await 'primary server' ready (recv, with timeout)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, prim_server, recv)
                   end},
         #{desc => "order 'primary server' to continue (close)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await 'primary server' ready (close)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, prim_server, close)
                   end},
         #{desc => "await 'secondary server 1' ready (closed)",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_server1, recv_closed)
                   end},
         #{desc => "await 'secondary server 2' ready (closed)",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_server2, recv_closed)
                   end},
         #{desc => "await 'secondary server 3' ready (closed)",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_server3, recv_closed)
                   end},

         %% Terminations
         #{desc => "order 'secondary server 3' to terminate",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary server 3' termination",
           cmd  => fun(#{sec_server3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_server3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary server 2' to terminate",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary server 2' termination",
           cmd  => fun(#{sec_server2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_server2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary server 1' to terminate",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary server 1' termination",
           cmd  => fun(#{sec_server1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_server1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'primary server' to terminate",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'primary server' termination",
           cmd  => fun(#{prim_server := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(prim_server, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    

    i("start 'primary server' evaluator"),
    PrimSrvInitState = InitState,
    PrimServer = ?SEV_START("prim-server", PrimServerSeq, PrimSrvInitState),

    i("start 'secondary server 1' evaluator"),
    SecSrvInitState = #{recv => maps:get(recv, InitState)},
    SecServer1 = ?SEV_START("sec-server-1", SecServerSeq, SecSrvInitState),

    i("start 'secondary server 2' evaluator"),
    SecServer2 = ?SEV_START("sec-server-2", SecServerSeq, SecSrvInitState),

    i("start 'secondary server 3' evaluator"),
    SecServer3 = ?SEV_START("sec-server-3", SecServerSeq, SecSrvInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{prim_server => PrimServer#ev.pid,
                        sec_server1  => SecServer1#ev.pid,
                        sec_server2  => SecServer2#ev.pid,
                        sec_server3  => SecServer3#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([PrimServer, 
                            SecServer1, SecServer2, SecServer3,
                            Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv4.

sc_lc_recvmsg_response_tcp4(suite) ->
    [];
sc_lc_recvmsg_response_tcp4(doc) ->
    [];
sc_lc_recvmsg_response_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recvmsg_response_tcp4,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv6.

sc_lc_recvmsg_response_tcp6(suite) ->
    [];
sc_lc_recvmsg_response_tcp6(doc) ->
    [];
sc_lc_recvmsg_response_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_recvmsg_response_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv4.

sc_lc_recvmsg_response_udp4(suite) ->
    [];
sc_lc_recvmsg_response_udp4(doc) ->
    [];
sc_lc_recvmsg_response_udp4(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recvmsg_response_udp4,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain => inet,
                                 recv   => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv6.

sc_lc_recvmsg_response_udp6(suite) ->
    [];
sc_lc_recvmsg_response_udp6(doc) ->
    [];
sc_lc_recvmsg_response_udp6(_Config) when is_list(_Config) ->
    tc_try(sc_recvmsg_response_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain => inet6,
                                 recv   => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the accept function.
%% We test what happens with a non-controlling_process also, since we 
%% git the setup anyway.
%% Socket is IPv4.

sc_lc_acceptor_response_tcp4(suite) ->
    [];
sc_lc_acceptor_response_tcp4(doc) ->
    [];
sc_lc_acceptor_response_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_lc_acceptor_response_tcp4,
           fun() ->
                   ?TT(?SECS(10)),
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_lc_acceptor_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the accept function.
%% We test what happens with a non-controlling_process also, since we 
%% git the setup anyway.
%% Socket is IPv6.

sc_lc_acceptor_response_tcp6(suite) ->
    [];
sc_lc_acceptor_response_tcp6(doc) ->
    [];
sc_lc_acceptor_response_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_lc_acceptor_response_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_lc_acceptor_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_lc_acceptor_response_tcp(InitState) ->
    PrimAcceptorSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
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
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, lsa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _Port} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{sock := Sock}) ->
                           socket:listen(Sock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, accept) of
                               {ok, Timeout} ->
                                   {ok, State#{timeout => Timeout}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await connection",
           cmd  => fun(#{sock := Sock, timeout := Timeout} = _State) ->
                           case socket:accept(Sock, Timeout) of
                               {error, timeout} ->
                                   ok;
                               {ok, Sock} ->
                                   ?SEV_EPRINT("unexpected success"),
                                   (catch socket:close(Sock)),
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept timeout)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept_timeout),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, close)
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   {ok, maps:remove(sock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

                                                % Termination
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    SecAcceptorSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, Sock} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, sock => Sock}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init)
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept",
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:accept(Sock) of
                               {error, closed} ->
                                   {ok, maps:remove(sock, State)};
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept closed)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept_closed)
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

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor 'primary acceptor'",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor 'secondary acceptor 1'",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary acceptor 2",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary acceptor 3",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the primary server
         #{desc => "order 'primary acceptor' start",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (init)",
           cmd  => fun(#{prim_acc := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, prim_acc, init),
                           {ok, State#{sock => Sock}}
                   end},

         %% Start the secondary acceptor 1
         #{desc => "order 'secondary acceptor 1' start",
           cmd  => fun(#{sec_acc1 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 1' ready (init)",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc1, init)
                   end},

         %% Start the secondary acceptor 2
         #{desc => "order 'secondary acceptor 2' start",
           cmd  => fun(#{sec_acc2 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 2' ready (init)",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc2, init)
                   end},

         %% Start the secondary acceptor 3
         #{desc => "order 'secondary acceptor 3' start",
           cmd  => fun(#{sec_acc3 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 3' ready (init)",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc3, init)
                   end},


         %% The actual test
         %% Make all the seondary servers continue, with an infinit recvfrom
         %% and then the prim-server with a timed recvfrom.
         %% After the prim server notifies us (about the timeout) we order it
         %% to close the socket, which should cause the all the secondary 
         %% server to return with error-closed.

         #{desc => "order 'secondary acceptor 1' to continue (accept)",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary acceptor 2' to continue (accept)",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary acceptor 3' to continue (accept)",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'primary acceptor' to continue",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept, ?SECS(5)),
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (accept timeout)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, prim_acc, accept_timeout)
                   end},
         #{desc => "order 'primary acceptor' to continue (close)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (close)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, prim_acc, close)
                   end},
         #{desc => "await 'secondary acceptor 1' ready (accept closed)",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc1, accept_closed)
                   end},
         #{desc => "await 'secondary acceptor 2' ready (accept closed)",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc2, accept_closed)
                   end},
         #{desc => "await 'secondary acceptor 3' ready (accept closed)",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc3, accept_closed)
                   end},


         %% Terminations
         #{desc => "order 'secondary acceptor 3' to terminate",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 3' termination",
           cmd  => fun(#{sec_acc3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_acc3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary acceptor 2' to terminate",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 2' termination",
           cmd  => fun(#{sec_acc2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_acc2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary acceptor 1' to terminate",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 1' termination",
           cmd  => fun(#{sec_acc1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_acc1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'primary acceptor' to terminate",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'primary acceptor' termination",
           cmd  => fun(#{prim_acc := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(prim_acc, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    i("start 'primary acceptor' evaluator"),
    PrimAccInitState = InitState,
    PrimAcc = ?SEV_START("prim-acceptor", PrimAcceptorSeq, PrimAccInitState),

    i("start 'secondary acceptor 1' evaluator"),
    SecAccInitState = #{},
    SecAcc1 = ?SEV_START("sec-acceptor-1", SecAcceptorSeq, SecAccInitState),

    i("start 'secondary acceptor 2' evaluator"),
    SecAcc2 = ?SEV_START("sec-acceptor-2", SecAcceptorSeq, SecAccInitState),

    i("start 'secondary acceptor 3' evaluator"),
    SecAcc3 = ?SEV_START("sec-acceptor-3", SecAcceptorSeq, SecAccInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{prim_acc => PrimAcc#ev.pid,
                        sec_acc1 => SecAcc1#ev.pid,
                        sec_acc2 => SecAcc2#ev.pid,
                        sec_acc3 => SecAcc3#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([PrimAcc, SecAcc1, SecAcc2, SecAcc3, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recv function.
%% Socket is IPv4.
%%
%% To minimize the chance of "weirdness", we should really have test cases
%% where the two sides of the connection is on different machines. But for
%% now, we will make do with different VMs on the same host.
%%

sc_rc_recv_response_tcp4(suite) ->
    [];
sc_rc_recv_response_tcp4(doc) ->
    [];
sc_rc_recv_response_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_rc_recv_response_tcp4,
           fun() ->
                   ?TT(?SECS(30)),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recv function.
%% Socket is IPv6.

sc_rc_recv_response_tcp6(suite) ->
    [];
sc_rc_recv_response_tcp6(doc) ->
    [];
sc_rc_recv_response_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_rc_recv_response_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_rc_receive_response_tcp(InitState) ->
    %% Each connection are handled by handler processes.
    %% These are created (on the fly) and handled internally 
    %% by the server!
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
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, local_sa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
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
           cmd  => fun(#{tester := Tester, local_sa := LSA, lport := Port}) ->
                           ServerSA = LSA#{port => Port},
                           ?SEV_ANNOUNCE_READY(Tester, init, ServerSA),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept all three connections)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept 1",
           cmd  => fun(#{lsock := LSock, recv := Recv} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: try start handler"),
                                   Handler = sc_rc_tcp_handler_start(1, Recv, Sock),
                                   ?SEV_IPRINT("handler started"),
                                   {ok, State#{csock1   => Sock,
                                               handler1 => Handler}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await handler 1 ready (init)",
           cmd  => fun(#{tester   := Tester, 
                         handler1 := Handler1} = _State) ->
                           ?SEV_AWAIT_READY(Handler1, handler1, init, 
                                            [{tester, Tester}])
                   end},
         #{desc => "accept 2",
           cmd  => fun(#{lsock := LSock, recv := Recv} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: try start handler"),
                                   Handler = sc_rc_tcp_handler_start(2, Recv, Sock),
                                   ?SEV_IPRINT("handler started"),
                                   {ok, State#{csock2   => Sock,
                                               handler2 => Handler}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await handler 2 ready (init)",
           cmd  => fun(#{tester   := Tester, 
                         handler1 := Handler1, 
                         handler2 := Handler2} = _State) ->
                           ?SEV_AWAIT_READY(Handler2, handler2, init, 
                                            [{tester,   Tester},
                                             {handler1, Handler1}])
                   end},
         #{desc => "accept 3",
           cmd  => fun(#{lsock := LSock, recv := Recv} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: try start handler"),
                                   Handler = sc_rc_tcp_handler_start(3, Recv, Sock),
                                   ?SEV_IPRINT("handler started"),
                                   {ok, State#{csock3   => Sock,
                                               handler3 => Handler}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await handler 3 ready (init)",
           cmd  => fun(#{tester   := Tester, 
                         handler1 := Handler1, 
                         handler2 := Handler2, 
                         handler3 := Handler3} = _State) ->
                           ?SEV_AWAIT_READY(Handler3, handler3, init, 
                                            [{tester,   Tester},
                                             {handler1, Handler1},
                                             {handler2, Handler2}])
                   end},
         #{desc => "announce ready (accept all three connections)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},
         #{desc => "await continue (recv)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                   end},
         #{desc => "order handler 1 to receive",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "order handler 2 to receive",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "order handler 3 to receive",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "await ready from handler 1 (recv)",
           cmd  => fun(#{tester := Tester, handler1 := Pid} = _State) ->
                           case ?SEV_AWAIT_READY(Pid, handler1, recv, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await ready from handler 2 (recv)",
           cmd  => fun(#{tester := Tester, handler2 := Pid} = _State) ->
                           case ?SEV_AWAIT_READY(Pid, handler2, recv, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await ready from handler 3 (recv)",
           cmd  => fun(#{tester := Tester, handler3 := Pid} = _State) ->
                           case ?SEV_AWAIT_READY(Pid, handler3, recv, 
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv closed from all handlers)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_closed),
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
         #{desc => "order handler 1 to terminate",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           %% Pid ! {terminate, self(), ok},
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 1 termination",
           cmd  => fun(#{handler1 := Pid} = State) ->
                           ?SEV_AWAIT_TERMINATION(Pid),
                           State1 = maps:remove(csock1,   State),
                           State2 = maps:remove(handler1, State1),
                           {ok, State2}
                   end},
         #{desc => "order handler 2 to terminate",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 2 termination",
           cmd  => fun(#{handler2 := Pid} = State) ->
                           ?SEV_AWAIT_TERMINATION(Pid),
                           State1 = maps:remove(csock2,   State),
                           State2 = maps:remove(handler2, State1),
                           {ok, State2}
                   end},
         #{desc => "order handler 3 to terminate",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 3 termination",
           cmd  => fun(#{handler3 := Pid} = State) ->
                           ?SEV_AWAIT_TERMINATION(Pid),
                           State1 = maps:remove(csock3,   State),
                           State2 = maps:remove(handler3, State1),
                           {ok, State2}
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:close(LSock) of
                               ok ->
                                   {ok, maps:remove(lsock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, {NodeID, ServerSA}} = ?SEV_AWAIT_START(),
                           {ok, State#{tester    => Tester, 
                                       node_id   => NodeID, 
                                       server_sa => ServerSA}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create node",
           cmd  => fun(#{host := Host, node_id := NodeID} = State) ->
                           case start_node(Host, l2a(f("client_~w", [NodeID]))) of
                               {ok, Node} ->
                                   ?SEV_IPRINT("client node ~p started", [Node]),
                                   {ok, State#{node => Node}};
                               {error, Reason} ->
                                   {skip, Reason}
                           end
                   end},
         #{desc => "monitor client node 1",
           cmd  => fun(#{node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "start remote client on client node",
           cmd  => fun(#{node := Node} = State) ->
                           Pid = sc_rc_tcp_client_start(Node),
                           ?SEV_IPRINT("client ~p started", [Pid]),
                           {ok, State#{rclient => Pid}}
                   end},
         #{desc => "monitor remote client",
           cmd  => fun(#{rclient := Pid}) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order remote client to start",
           cmd  => fun(#{rclient := Client, server_sa := ServerSA}) ->
                           ?SEV_ANNOUNCE_START(Client, ServerSA),
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
         #{desc => "announce ready (connected)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close, 
                                               [{rclient, Client}]),
                           ok
                   end},
         #{desc => "order remote client to close",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, close),
                           ok
                   end},
         #{desc => "await remote client ready (closed)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, close, 
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
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
           cmd  => fun(#{node := Node} = _State) ->
                           stop_node(Node)
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node} = State) ->
                           receive
                               {nodedown, Node} ->
                                   State1 = maps:remove(node_id, State),
                                   State2 = maps:remove(node,    State1),
                                   {ok, State2}
                           end
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
         #{desc => "monitor client 1",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client 2",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client 3",
           cmd  => fun(#{client3 := Pid} = _State) ->
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

         %% Start the client(s)
         #{desc => "order client 1 start",
           cmd  => fun(#{client1   := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, {1, ServerSA}),
                           ok
                   end},
         #{desc => "await client 1 ready (init)",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, init)
                   end},
         #{desc => "order client 2 start",
           cmd  => fun(#{client2   := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, {2, ServerSA}),
                           ok
                   end},
         #{desc => "await client 2 ready (init)",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, init)
                   end},
         #{desc => "order client 3 start",
           cmd  => fun(#{client3   := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, {3, ServerSA}),
                           ok
                   end},
         #{desc => "await client 3 ready (init)",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, init)
                   end},

         %% The actual test
         #{desc => "order server continue (accept)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client 1 continue (connect)",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await client 1 ready (connect)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client1, client1, connect, 
                                            [{server,  Server},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 2 continue (connect)",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await client 2 ready (connect)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client2, client2, connect, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 3 continue (connect)",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await client 3 ready (connect)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client3, client3, connect, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client2, Client2}]),
                           ok
                   end},
         #{desc => "await server ready (accept from all connections)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept,
                                            [{client1, Client1},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order server continue (recv for all connections)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client 1 continue (close)",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await client 1 ready (close)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client1, client1, close, 
                                            [{server,  Server},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 2 continue (close)",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await client 2 ready (close)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client2, client2, close, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 3 continue (close)",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await client 3 ready (close)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client3, client1, close, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client2, Client2}]),
                           ok
                   end},
         #{desc => "await server ready (close for all connections)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv_closed,
                                            [{client1, Client1},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},

         %% Terminations
         #{desc => "order client 1 to terminate",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 1 termination",
           cmd  => fun(#{client1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client1, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order client 2 to terminate",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 2 termination",
           cmd  => fun(#{client2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client2, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order client 3 to terminate",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 3 termination",
           cmd  => fun(#{client3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client3, State),
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
    Client1 = ?SEV_START("client-1", ClientSeq, ClientInitState),
    Client2 = ?SEV_START("client-2", ClientSeq, ClientInitState),
    Client3 = ?SEV_START("client-3", ClientSeq, ClientInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{server  => Server#ev.pid,
                        client1 => Client1#ev.pid,
                        client2 => Client2#ev.pid,
                        client3 => Client3#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Server,
                            Client1, Client2, Client3,
                            Tester]).


sc_rc_tcp_client_start(Node) ->
    Self = self(),
    Fun  = fun() -> sc_rc_tcp_client(Self) end,
    erlang:spawn(Node, Fun).


sc_rc_tcp_client(Parent) ->
    sc_rc_tcp_client_init(Parent),
    ServerSA = sc_rc_tcp_client_await_start(Parent),
    Domain   = maps:get(family, ServerSA),
    Sock     = sc_rc_tcp_client_create(Domain),
    sc_rc_tcp_client_bind(Sock, Domain),
    sc_rc_tcp_client_announce_ready(Parent, init),
    sc_rc_tcp_client_await_continue(Parent, connect),
    sc_rc_tcp_client_connect(Sock, ServerSA),
    sc_rc_tcp_client_announce_ready(Parent, connect),
    sc_rc_tcp_client_await_continue(Parent, close),
    sc_rc_tcp_client_close(Sock),
    sc_rc_tcp_client_announce_ready(Parent, close),
    Reason = sc_rc_tcp_client_await_terminate(Parent),
    ?SEV_IPRINT("terminate"),
    exit(Reason).

sc_rc_tcp_client_init(Parent) ->
    put(sname, "rclient"),
    ?SEV_IPRINT("init"),
    _MRef = erlang:monitor(process, Parent),
    ok.

sc_rc_tcp_client_await_start(Parent) ->
    i("sc_rc_tcp_client_await_start -> entry"),
    ?SEV_AWAIT_START(Parent).

sc_rc_tcp_client_create(Domain) ->
    i("sc_rc_tcp_client_create -> entry"),
    case socket:open(Domain, stream, tcp) of
        {ok, Sock} ->
            case socket:getopt(Sock, otp, fd) of
                {ok, FD} ->
                    put(sname, f("rclient-~w", [FD])); % Update SName
                _ ->
                    ok
            end,
            Sock;
        {error, Reason} ->
            exit({open_failed, Reason})
    end.

sc_rc_tcp_client_bind(Sock, Domain) ->
    i("sc_rc_tcp_client_bind -> entry"),
    LSA = which_local_socket_addr(Domain),
    case socket:bind(Sock, LSA) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            exit({bind, Reason})
    end.

sc_rc_tcp_client_announce_ready(Parent, Slogan) ->
    ?SEV_IPRINT("ready ~w", [Slogan]),
    ?SEV_ANNOUNCE_READY(Parent, Slogan).

sc_rc_tcp_client_await_continue(Parent, Slogan) ->
    ?SEV_IPRINT("await ~w continue", [Slogan]),
    ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan).

sc_rc_tcp_client_connect(Sock, ServerSA) ->
    i("sc_rc_tcp_client_connect -> entry"),
    case socket:connect(Sock, ServerSA) of
        ok ->
            ok;
        {error, Reason} ->
            exit({connect, Reason})
    end.

sc_rc_tcp_client_close(Sock) ->
    i("sc_rc_tcp_client_close -> entry"),
    case socket:close(Sock) of
        ok ->
            ok;
        {error, Reason} ->
            exit({close, Reason})
    end.

sc_rc_tcp_client_await_terminate(Parent) ->
    i("sc_rc_tcp_client_await_terminate -> entry"),
    case ?SEV_AWAIT_TERMINATE(Parent, parent) of
        ok ->
            ok;
        {error, Reason} ->
            Reason
    end.


%% The handlers run on the same node as the server (the local node).

sc_rc_tcp_handler_start(ID, Recv, Sock) ->
    Self     = self(),
    Fun      = fun() -> sc_rc_tcp_handler(ID, Self, Recv, Sock) end,
    {Pid, _} = erlang:spawn_monitor(Fun),
    Pid.

sc_rc_tcp_handler(ID, Parent, Recv, Sock) ->
    sc_rc_tcp_handler_init(ID, socket:getopt(Sock, otp, fd), Parent),
    sc_rc_tcp_handler_await(Parent, recv),
    RecvRes = sc_rc_tcp_handler_recv(Recv, Sock),
    sc_rc_tcp_handler_announce_ready(Parent, recv, RecvRes),
    Reason = sc_rc_tcp_handler_await(Parent, terminate),
    exit(Reason).

sc_rc_tcp_handler_init(ID, {ok, FD}, Parent) ->
    put(sname, f("handler-~w:~w", [ID, FD])),
    _MRef = erlang:monitor(process, Parent),
    ?SEV_IPRINT("started"),
    ?SEV_ANNOUNCE_READY(Parent, init),
    ok.

sc_rc_tcp_handler_await(Parent, terminate) ->
    ?SEV_IPRINT("await terminate"),
    ?SEV_AWAIT_TERMINATE(Parent, tester);
sc_rc_tcp_handler_await(Parent, Slogan) ->
    ?SEV_IPRINT("await ~w", [Slogan]),
    ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan).

sc_rc_tcp_handler_recv(Recv, Sock) ->
    ?SEV_IPRINT("recv"),
    try Recv(Sock) of
        {error, closed} ->
            ok;
        {ok, _} ->
            ?SEV_IPRINT("unexpected success"),
            {error, unexpected_success};
        {error, Reason} = ERROR ->
            ?SEV_IPRINT("receive error: "
                        "~n   ~p", [Reason]),
            ERROR
    catch
        C:E:S ->
            ?SEV_IPRINT("receive failure: "
                        "~n   Class: ~p"
                        "~n   Error: ~p"
                        "~n   Stack: ~p", [C, E, S]),
            {error, {recv, C, E, S}}
    end.

sc_rc_tcp_handler_announce_ready(Parent, Slogan, Result) ->
    ?SEV_IPRINT("announce ready"),
    ?SEV_ANNOUNCE_READY(Parent, Slogan, Result),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recvmsg function.
%% Socket is IPv4.

sc_rc_recvmsg_response_tcp4(suite) ->
    [];
sc_rc_recvmsg_response_tcp4(doc) ->
    [];
sc_rc_recvmsg_response_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_rc_recvmsg_response_tcp4,
           fun() ->
                   ?TT(?SECS(30)),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recvmsg function.
%% Socket is IPv6.

sc_rc_recvmsg_response_tcp6(suite) ->
    [];
sc_rc_recvmsg_response_tcp6(doc) ->
    [];
sc_rc_recvmsg_response_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_rc_recvmsg_response_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is
%% remotely closed while the process is calling the recv function.
%% The remote client sends data, then shutdown(write) and then the
%% reader attempts a recv.
%% Socket is IPv4.
%%
%% To minimize the chance of "weirdness", we should really have test cases
%% where the two sides of the connection is on different machines. But for
%% now, we will make do with different VMs on the same host.
%%

sc_rs_recv_send_shutdown_receive_tcp4(suite) ->
    [];
sc_rs_recv_send_shutdown_receive_tcp4(doc) ->
    [];
sc_rs_recv_send_shutdown_receive_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_rs_recv_send_shutdown_receive_tcp4,
           fun() ->
                   ?TT(?SECS(30)),
                   MsgData   = ?DATA,
                   Recv      = fun(Sock) ->
                                       socket:recv(Sock)
                               end,
                   Send      = fun(Sock, Data) ->
                                       socket:send(Sock, Data)
                               end,
                   InitState = #{domain => inet,
                                 recv   => Recv,
                                 send   => Send,
                                 data   => MsgData},
                   ok = sc_rs_send_shutdown_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is
%% remotely closed while the process is calling the recv function.
%% The remote client sends data, then shutdown(write) and then the
%% reader attempts a recv.
%% Socket is IPv6.

sc_rs_recv_send_shutdown_receive_tcp6(suite) ->
    [];
sc_rs_recv_send_shutdown_receive_tcp6(doc) ->
    [];
sc_rs_recv_send_shutdown_receive_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_rs_recv_send_shutdown_receive_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   MsgData   = ?DATA,
                   Recv      = fun(Sock) ->
                                       socket:recv(Sock)
                               end,
                   Send      = fun(Sock, Data) ->
                                       socket:send(Sock, Data)
                               end,
                   InitState = #{domain => inet6,
                                 recv   => Recv,
                                 send   => Send,
                                 data   => MsgData},
                   ok = sc_rs_send_shutdown_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_rs_send_shutdown_receive_tcp(InitState) ->
    %% The connection is handled by a handler processes.
    %% This are created (on the fly) and handled internally
    %% by the server!
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
                           i("get local address for ~p", [Domain]),
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "create listen socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, local_sa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
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
           cmd  => fun(#{tester := Tester, local_sa := LSA, lport := Port}) ->
                           ServerSA = LSA#{port => Port},
                           ?SEV_ANNOUNCE_READY(Tester, init, ServerSA),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept",
           cmd  => fun(#{lsock := LSock, recv := Recv} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: try start handler"),
                                   Handler =
                                       sc_rs_tcp_handler_start(Recv, Sock),
                                   ?SEV_IPRINT("handler started"),
                                   {ok, State#{csock   => Sock,
                                               handler => Handler}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await handler ready (init)",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = _State) ->
                           ?SEV_AWAIT_READY(Handler, handler, init,
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         #{desc => "await continue (first recv)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                   end},
         #{desc => "order handler to receive (first)",
           cmd  => fun(#{handler := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "await ready from handler (first recv)",
           cmd  => fun(#{tester := Tester, handler := Pid} = _State) ->
                           case ?SEV_AWAIT_READY(Pid, handler, recv,
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   ?SEV_IPRINT("first recv: ~p", [Result]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (first recv)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},
         #{desc => "await continue (second recv)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                   end},
         #{desc => "order handler to receive (second)",
           cmd  => fun(#{handler := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "await ready from handler (second recv)",
           cmd  => fun(#{tester := Tester, handler := Pid} = _State) ->
                           case ?SEV_AWAIT_READY(Pid, handler, recv,
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   ?SEV_IPRINT("second recv: ~p", [Result]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (second recv)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv),
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
         #{desc => "order handler to terminate",
           cmd  => fun(#{handler := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler termination",
           cmd  => fun(#{handler := Pid} = State) ->
                           ?SEV_AWAIT_TERMINATION(Pid),
                           State1 = maps:remove(csock,   State),
                           State2 = maps:remove(handler, State1),
                           {ok, State2}
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:close(LSock) of
                               ok ->
                                   {ok, maps:remove(lsock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
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
           cmd  => fun(#{host := Host} = State) ->
                           case start_node(Host, client) of
                               {ok, Node} ->
                                   ?SEV_IPRINT("client node ~p started",
                                               [Node]),
                                   {ok, State#{node => Node}};
                               {error, Reason} ->
                                   {skip, Reason}
                           end
                   end},
         #{desc => "monitor client node",
           cmd  => fun(#{node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "start remote client on client node",
           cmd  => fun(#{node := Node,
                         send := Send} = State) ->
                           Pid = sc_rs_tcp_client_start(Node, Send),
                           ?SEV_IPRINT("client ~p started", [Pid]),
                           {ok, State#{rclient => Pid}}
                   end},
         #{desc => "monitor remote client",
           cmd  => fun(#{rclient := Pid}) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order remote client to start",
           cmd  => fun(#{rclient := Client, server_sa := ServerSA}) ->
                           ?SEV_ANNOUNCE_START(Client, ServerSA),
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

         #{desc => "await continue (send)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, send,
                                                    [{rclient, Client}]) of
                               {ok, Data} ->
                                   {ok, State#{rclient_data => Data}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to send",
           cmd  => fun(#{rclient      := Client,
                         rclient_data := Data}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Data),
                           ok
                   end},
         #{desc => "await remote client ready (closed)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, send,
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (send)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send),
                           ok
                   end},


         #{desc => "await continue (shutdown)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, shutdown,
                                               [{rclient, Client}]),
                           ok
                   end},
         #{desc => "order remote client to shutdown",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, shutdown),
                           ok
                   end},
         #{desc => "await remote client ready (shiutdown)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, shutdown,
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (shutdown)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, shutdown),
                           ok
                   end},

         #{desc => "await continue (close)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close,
                                               [{rclient, Client}]),
                           ok
                   end},
         #{desc => "order remote client to close",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, close),
                           ok
                   end},
         #{desc => "await remote client ready (closed)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, close,
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
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
           cmd  => fun(#{node := Node} = _State) ->
                           stop_node(Node)
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node} = State) ->
                           receive
                               {nodedown, Node} ->
                                   State1 = maps:remove(node_id, State),
                                   State2 = maps:remove(node,    State1),
                                   {ok, State2}
                           end
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

         %% Start the client(s)
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
         #{desc => "await client ready (connect)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, connect,
                                            [{server, Server}]),
                           ok
                   end},
         #{desc => "await server ready (accept)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept,
                                            [{client, Client}]),
                           ok
                   end},

         #{desc => "order client continue (send)",
           cmd  => fun(#{client := Pid,
                         data   := Data} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, send, Data),
                           ok
                   end},
         #{desc => "await client ready (send)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, send,
                                            [{server, Server}]),
                           ok
                   end},

         #{desc => "order client continue (shutdown)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, shutdown),
                           ok
                   end},
         #{desc => "await client ready (shutdown)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, shutdown,
                                            [{server, Server}]),
                           ok
                   end},

         #{desc => "order server continue (first recv)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "await server ready (first recv)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv,
                                            [{client, Client}]),
                           ok
                   end},

         #{desc => "order server continue (second recv)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "await server ready (second recv)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv,
                                            [{client, Client}]),
                           ok
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "order client continue (close)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await client ready (close)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, close,
                                            [{server, Server}]),
                           ok
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
    ServerInitState = #{domain => maps:get(domain, InitState),
                        recv   => maps:get(recv,   InitState)},
    Server = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator"),
    ClientInitState = #{host   => local_host(),
                        domain => maps:get(domain, InitState),
                        send   => maps:get(send, InitState)},
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{server => Server#ev.pid,
                        client => Client#ev.pid,
                        data   => maps:get(data, InitState)},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).


sc_rs_tcp_client_start(Node, Send) ->
    Self = self(),
    Fun  = fun() -> sc_rs_tcp_client(Self, Send) end,
    erlang:spawn(Node, Fun).


sc_rs_tcp_client(Parent, Send) ->
    sc_rs_tcp_client_init(Parent),
    ServerSA = sc_rs_tcp_client_await_start(Parent),
    Domain   = maps:get(family, ServerSA),
    Sock     = sc_rs_tcp_client_create(Domain),
    sc_rs_tcp_client_bind(Sock, Domain),
    sc_rs_tcp_client_announce_ready(Parent, init),
    sc_rs_tcp_client_await_continue(Parent, connect),
    sc_rs_tcp_client_connect(Sock, ServerSA),
    sc_rs_tcp_client_announce_ready(Parent, connect),
    Data = sc_rs_tcp_client_await_continue(Parent, send),
    sc_rs_tcp_client_send(Sock, Send, Data),
    sc_rs_tcp_client_announce_ready(Parent, send),
    sc_rs_tcp_client_await_continue(Parent, shutdown),
    sc_rs_tcp_client_shutdown(Sock),
    sc_rs_tcp_client_announce_ready(Parent, shutdown),
    sc_rs_tcp_client_await_continue(Parent, close),
    sc_rs_tcp_client_close(Sock),
    sc_rs_tcp_client_announce_ready(Parent, close),
    Reason = sc_rs_tcp_client_await_terminate(Parent),
    ?SEV_IPRINT("terminate"),
    exit(Reason).

sc_rs_tcp_client_init(Parent) ->
    put(sname, "rclient"),
    ?SEV_IPRINT("init"),
    _MRef = erlang:monitor(process, Parent),
    ok.

sc_rs_tcp_client_await_start(Parent) ->
    i("sc_rs_tcp_client_await_start -> entry"),
    ?SEV_AWAIT_START(Parent).

sc_rs_tcp_client_create(Domain) ->
    i("sc_rs_tcp_client_create -> entry"),
    case socket:open(Domain, stream, tcp) of
        {ok, Sock} ->
            Sock;
        {error, Reason} ->
            exit({open_failed, Reason})
    end.

sc_rs_tcp_client_bind(Sock, Domain) ->
    i("sc_rs_tcp_client_bind -> entry"),
    LSA = which_local_socket_addr(Domain),
    case socket:bind(Sock, LSA) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            exit({bind, Reason})
    end.

sc_rs_tcp_client_announce_ready(Parent, Slogan) ->
    ?SEV_ANNOUNCE_READY(Parent, Slogan).

sc_rs_tcp_client_await_continue(Parent, Slogan) ->
    i("sc_rs_tcp_client_await_continue -> entry"),
    case ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan) of
        ok ->
            ok;
        {ok, Extra} ->
            Extra;
        {error, Reason} ->
            exit({await_continue, Slogan, Reason})
    end.


sc_rs_tcp_client_connect(Sock, ServerSA) ->
    i("sc_rs_tcp_client_connect -> entry"),
    case socket:connect(Sock, ServerSA) of
        ok ->
            ok;
        {error, Reason} ->
            exit({connect, Reason})
    end.

sc_rs_tcp_client_send(Sock, Send, Data) ->
    i("sc_rs_tcp_client_send -> entry"),
    case Send(Sock, Data) of
        ok ->
            ok;
        {error, Reason} ->
            exit({send, Reason})
    end.

sc_rs_tcp_client_shutdown(Sock) ->
    i("sc_rs_tcp_client_shutdown -> entry"),
    case socket:shutdown(Sock, write) of
        ok ->
            ok;
        {error, Reason} ->
            exit({shutdown, Reason})
    end.

sc_rs_tcp_client_close(Sock) ->
    i("sc_rs_tcp_client_close -> entry"),
    case socket:close(Sock) of
        ok ->
            ok;
        {error, Reason} ->
            exit({close, Reason})
    end.

sc_rs_tcp_client_await_terminate(Parent) ->
    i("sc_rs_tcp_client_await_terminate -> entry"),
    case ?SEV_AWAIT_TERMINATE(Parent, parent) of
        ok ->
            ok;
        {error, Reason} ->
            Reason
    end.


%% The handlers run on the same node as the server (the local node).

sc_rs_tcp_handler_start(Recv, Sock) ->
    Self     = self(),
    Fun      = fun() -> sc_rs_tcp_handler(Self, Recv, Sock) end,
    {Pid, _} = erlang:spawn_monitor(Fun),
    Pid.

sc_rs_tcp_handler(Parent, Recv, Sock) ->
    sc_rs_tcp_handler_init(Parent),
    sc_rs_tcp_handler_await(Parent, recv),
    ok = sc_rs_tcp_handler_recv(Recv, Sock, true),
    sc_rs_tcp_handler_announce_ready(Parent, recv, received),
    sc_rs_tcp_handler_await(Parent, recv),
    ok = sc_rs_tcp_handler_recv(Recv, Sock, false),
    sc_rs_tcp_handler_announce_ready(Parent, recv, closed),
    Reason = sc_rs_tcp_handler_await(Parent, terminate),
    exit(Reason).

sc_rs_tcp_handler_init(Parent) ->
    put(sname, "handler"),
    _MRef = erlang:monitor(process, Parent),
    ?SEV_IPRINT("started"),
    ?SEV_ANNOUNCE_READY(Parent, init),
    ok.

sc_rs_tcp_handler_await(Parent, terminate) ->
    ?SEV_IPRINT("await terminate"),
    ?SEV_AWAIT_TERMINATE(Parent, tester);
sc_rs_tcp_handler_await(Parent, Slogan) ->
    ?SEV_IPRINT("await ~w", [Slogan]),
    ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan).

%% This hould actually work - we leave it for now
sc_rs_tcp_handler_recv(Recv, Sock, First) ->
    ?SEV_IPRINT("recv"),
    try Recv(Sock) of
        {ok, _} when (First =:= true) ->
            ok;
        {error, closed} when (First =:= false) ->
            ok;
        {ok, _} ->
            ?SEV_IPRINT("unexpected success"),
            {error, unexpected_success};
        {error, Reason} = ERROR ->
            ?SEV_IPRINT("receive error: "
                        "~n   ~p", [Reason]),
            ERROR
    catch
        C:E:S ->
            ?SEV_IPRINT("receive failure: "
                        "~n   Class: ~p"
                        "~n   Error: ~p"
                        "~n   Stack: ~p", [C, E, S]),
            {error, {recv, C, E, S}}
    end.

sc_rs_tcp_handler_announce_ready(Parent, Slogan, Result) ->
    ?SEV_IPRINT("announce ready"),
    ?SEV_ANNOUNCE_READY(Parent, Slogan, Result),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is
%% remotely closed while the process is calling the recvmsg function.
%% The remote client sends data, then shutdown(write) and then the
%% reader attempts a recv.
%% Socket is IPv4.

sc_rs_recvmsg_send_shutdown_receive_tcp4(suite) ->
    [];
sc_rs_recvmsg_send_shutdown_receive_tcp4(doc) ->
    [];
sc_rs_recvmsg_send_shutdown_receive_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_rs_recvmsg_send_shutdown_receive_tcp4,
           fun() ->
                   ?TT(?SECS(30)),
                   MsgData   = ?DATA,
                   Recv      = fun(Sock) ->
                                       case socket:recvmsg(Sock) of
                                           {ok, #{addr  := undefined,
                                                  iov   := [Data]}} ->
                                               {ok, Data};
                                           {error, _} = ERROR ->
                                               ERROR
                                       end
                               end,
                   Send      = fun(Sock, Data) when is_binary(Data) ->
                                  MsgHdr = #{iov => [Data]},
                                  socket:sendmsg(Sock, MsgHdr)
                               end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv,
                                 send     => Send,
                                 data     => MsgData},
                   ok = sc_rs_send_shutdown_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is
%% remotely closed while the process is calling the recvmsg function.
%% The remote client sends data, then shutdown(write) and then the
%% reader attempts a recv.
%% Socket is IPv6.

sc_rs_recvmsg_send_shutdown_receive_tcp6(suite) ->
    [];
sc_rs_recvmsg_send_shutdown_receive_tcp6(doc) ->
    [];
sc_rs_recvmsg_send_shutdown_receive_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_rs_recvmsg_send_shutdown_receive_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   MsgData   = ?DATA,
                   Recv      = fun(Sock) ->
                                       case socket:recvmsg(Sock) of
                                           {ok, #{addr  := undefined,
                                                  iov   := [Data]}} ->
                                               {ok, Data};
                                           {error, _} = ERROR ->
                                               ERROR
                                       end
                               end,
                   Send      = fun(Sock, Data) when is_binary(Data) ->
                                  MsgHdr = #{iov => [Data]},
                                  socket:sendmsg(Sock, MsgHdr)
                               end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv,
                                 send     => Send,
                                 data     => MsgData},
                   ok = sc_rs_send_shutdown_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% behave as expected when sending and/or reading chunks.
%% First send data in one "big" chunk, and read it in "small" chunks.
%% Second, send in a bunch of "small" chunks, and read in one "big" chunk.
%% Socket is IPv4.

traffic_send_and_recv_chunks_tcp4(suite) ->
    [];
traffic_send_and_recv_chunks_tcp4(doc) ->
    [];
traffic_send_and_recv_chunks_tcp4(_Config) when is_list(_Config) ->
    tc_try(traffic_send_and_recv_chunks_tcp4,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet},
                   ok = traffic_send_and_recv_chunks_tcp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% behave as expected when sending and/or reading chunks.
%% First send data in one "big" chunk, and read it in "small" chunks.
%% Second, send in a bunch of "small" chunks, and read in one "big" chunk.
%% Socket is IPv6.

traffic_send_and_recv_chunks_tcp6(suite) ->
    [];
traffic_send_and_recv_chunks_tcp6(doc) ->
    [];
traffic_send_and_recv_chunks_tcp6(_Config) when is_list(_Config) ->
    tc_try(traffic_send_and_recv_chunks_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet6},
                   ok = traffic_send_and_recv_chunks_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

traffic_send_and_recv_chunks_tcp(InitState) ->
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
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, local_sa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
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
           cmd  => fun(#{tester := Tester, local_sa := LSA, lport := Port}) ->
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
                               {ok, Chunk} ->
                                   ?SEV_IPRINT("recv of chunk 1 of ~p bytes",
                                               [size(Chunk)]),
                                   {ok, State#{chunks => [b2l(Chunk)]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 2",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} ->
                                   ?SEV_IPRINT("recv of chunk 2 of ~p bytes",
                                               [size(Chunk)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 3",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} ->
                                   ?SEV_IPRINT("recv of chunk 3 of ~p bytes",
                                               [size(Chunk)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 4",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} ->
                                   ?SEV_IPRINT("recv of chunk 4 of ~p bytes",
                                               [size(Chunk)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 5",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} ->
                                   ?SEV_IPRINT("recv of chunk 5 of ~p bytes",
                                               [size(Chunk)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 6",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} ->
                                   ?SEV_IPRINT("recv of chunk 6 of ~p bytes",
                                               [size(Chunk)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 7",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} ->
                                   ?SEV_IPRINT("recv of chunk 7 of ~p bytes",
                                               [size(Chunk)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 8",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} ->
                                   ?SEV_IPRINT("recv of chunk 8 of ~p bytes",
                                               [size(Chunk)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 9",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} ->
                                   ?SEV_IPRINT("recv of chunk 9 of ~p bytes",
                                               [size(Chunk)]),
                                   {ok, State#{chunks => [b2l(Chunk)|Chunks]}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "recv chunk 10",
           cmd  => fun(#{csock  := Sock,
                         chunks := Chunks} = State) ->
                           case socket:recv(Sock, 100) of
                               {ok, Chunk} ->
                                   ?SEV_IPRINT("recv of chunk 10 of ~p bytes",
                                               [size(Chunk)]),
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
                           case socket:recv(Sock, Size) of
                               {ok, Data} ->
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
           cmd  => fun(#{lsock := Sock} = State) ->
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
           cmd  => fun(#{host := Host} = State) ->
                           case start_node(Host, client) of
                               {ok, Node} ->
                                   ?SEV_IPRINT("(remote) client node ~p started",
                                               [Node]),
                                   {ok, State#{node => Node}};
                               {error, Reason} ->
                                   {skip, Reason}
                           end
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
           cmd  => fun(#{rclient := Client, server_sa := ServerSA}) ->
                           ?SEV_ANNOUNCE_START(Client, ServerSA),
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
           cmd  => fun(#{node := Node} = _State) ->
                           stop_node(Node)
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node} = State) ->
                           receive
                               {nodedown, Node} ->
                                   {ok, maps:remove(node, State)}
                           end
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
    {Sock, ServerSA} = traffic_snr_tcp_client_init(Parent),
    traffic_snr_tcp_client_announce_ready(Parent, init),
    traffic_snr_tcp_client_await_continue(Parent, connect),
    traffic_snr_tcp_client_connect(Sock, ServerSA),
    traffic_snr_tcp_client_announce_ready(Parent, connect),
    traffic_snr_tcp_client_send_loop(Parent, Sock),
    Reason = traffic_snr_tcp_client_await_terminate(Parent),
    traffic_snr_tcp_client_close(Sock),
    exit(Reason).


traffic_snr_tcp_client_send_loop(Parent, Sock) ->
    case ?SEV_AWAIT_CONTINUE(Parent, parent, send) of
        {ok, stop} -> % Breakes the loop
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
    ServerSA = traffic_snr_tcp_client_await_start(Parent),
    Domain   = maps:get(family, ServerSA),
    Sock     = traffic_snr_tcp_client_create(Domain),
    traffic_snr_tcp_client_bind(Sock, Domain),
    {Sock, ServerSA}.

traffic_snr_tcp_client_await_start(Parent) ->
    i("traffic_snr_tcp_client_await_start -> entry"),
    ?SEV_AWAIT_START(Parent).

traffic_snr_tcp_client_create(Domain) ->
    i("traffic_snr_tcp_client_create -> entry"),
    case socket:open(Domain, stream, tcp) of
        {ok, Sock} ->
            Sock;
        {error, Reason} ->
            exit({open_failed, Reason})
    end.

traffic_snr_tcp_client_bind(Sock, Domain) ->
    i("traffic_snr_tcp_client_bind -> entry"),
    LSA = which_local_socket_addr(Domain),
    case socket:bind(Sock, LSA) of
        {ok, _} ->
            ok;
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

traffic_snr_tcp_client_close(Sock) ->
    i("traffic_snr_tcp_client_close -> entry"),
    case socket:close(Sock) of
        ok ->
            ok;
        {error, Reason} ->
            exit({close, Reason})
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
%% This is the 'small' message test case, for IPv4.

traffic_ping_pong_small_send_and_recv_tcp4(suite) ->
    [];
traffic_ping_pong_small_send_and_recv_tcp4(doc) ->
    [];
traffic_ping_pong_small_send_and_recv_tcp4(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_SMALL_NUM,
    tc_try(traffic_ping_pong_small_send_and_recv_tcp4,
           fun() ->
                   ?TT(?SECS(15)),
                   InitState = #{domain => inet,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_tcp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case, for IPv6.

traffic_ping_pong_small_send_and_recv_tcp6(suite) ->
    [];
traffic_ping_pong_small_send_and_recv_tcp6(doc) ->
    [];
traffic_ping_pong_small_send_and_recv_tcp6(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_SMALL_NUM,
    tc_try(traffic_ping_pong_small_send_and_recv_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(15)),
                   InitState = #{domain => inet6,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case, for IPv4.

traffic_ping_pong_medium_send_and_recv_tcp4(suite) ->
    [];
traffic_ping_pong_medium_send_and_recv_tcp4(doc) ->
    [];
traffic_ping_pong_medium_send_and_recv_tcp4(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_MEDIUM_NUM,
    tc_try(traffic_ping_pong_medium_send_and_recv_tcp4,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case, for IPv6.

traffic_ping_pong_medium_send_and_recv_tcp6(suite) ->
    [];
traffic_ping_pong_medium_send_and_recv_tcp6(doc) ->
    [];
traffic_ping_pong_medium_send_and_recv_tcp6(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_MEDIUM_NUM,
    tc_try(traffic_ping_pong_medium_send_and_recv_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet6,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_tcp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case, for IPv4.

traffic_ping_pong_large_send_and_recv_tcp4(suite) ->
    [];
traffic_ping_pong_large_send_and_recv_tcp4(doc) ->
    [];
traffic_ping_pong_large_send_and_recv_tcp4(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_LARGE_NUM,
    tc_try(traffic_ping_pong_large_send_and_recv_tcp4,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => inet,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the send and recv functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case, for IPv6.

traffic_ping_pong_large_send_and_recv_tcp6(suite) ->
    [];
traffic_ping_pong_large_send_and_recv_tcp6(doc) ->
    [];
traffic_ping_pong_large_send_and_recv_tcp6(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_LARGE_NUM,
    tc_try(traffic_ping_pong_large_send_and_recv_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => inet6,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_send_and_recv_tcp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendto and recvfrom 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for two different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case, for IPv4.

traffic_ping_pong_small_sendto_and_recvfrom_udp4(suite) ->
    [];
traffic_ping_pong_small_sendto_and_recvfrom_udp4(doc) ->
    [];
traffic_ping_pong_small_sendto_and_recvfrom_udp4(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_SMALL_NUM,
    tc_try(traffic_ping_pong_small_sendto_and_recvfrom_udp4,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => inet,
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

traffic_ping_pong_small_sendto_and_recvfrom_udp6(suite) ->
    [];
traffic_ping_pong_small_sendto_and_recvfrom_udp6(doc) ->
    [];
traffic_ping_pong_small_sendto_and_recvfrom_udp6(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_SMALL_NUM,
    tc_try(traffic_ping_pong_small_sendto_and_recvfrom_udp6,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => inet,
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

traffic_ping_pong_medium_sendto_and_recvfrom_udp4(suite) ->
    [];
traffic_ping_pong_medium_sendto_and_recvfrom_udp4(doc) ->
    [];
traffic_ping_pong_medium_sendto_and_recvfrom_udp4(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_MEDIUM_NUM,
    tc_try(traffic_ping_pong_medium_sendto_and_recvfrom_udp4,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => inet,
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

traffic_ping_pong_medium_sendto_and_recvfrom_udp6(suite) ->
    [];
traffic_ping_pong_medium_sendto_and_recvfrom_udp6(doc) ->
    [];
traffic_ping_pong_medium_sendto_and_recvfrom_udp6(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_MEDIUM_NUM,
    tc_try(traffic_ping_pong_medium_sendto_and_recvfrom_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(45)),
                   InitState = #{domain => inet6,
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
%% This is the 'small' message test case, for IPv4.

traffic_ping_pong_small_sendmsg_and_recvmsg_tcp4(suite) ->
    [];
traffic_ping_pong_small_sendmsg_and_recvmsg_tcp4(doc) ->
    [];
traffic_ping_pong_small_sendmsg_and_recvmsg_tcp4(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_SMALL_NUM,
    tc_try(traffic_ping_pong_small_sendmsg_and_recvmsg_tcp4,
           fun() ->
                   ?TT(?SECS(20)),
                   InitState = #{domain => inet,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case, for IPv6.

traffic_ping_pong_small_sendmsg_and_recvmsg_tcp6(suite) ->
    [];
traffic_ping_pong_small_sendmsg_and_recvmsg_tcp6(doc) ->
    [];
traffic_ping_pong_small_sendmsg_and_recvmsg_tcp6(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_SMALL_NUM,
    tc_try(traffic_ping_pong_small_sendmsg_and_recvmsg_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(20)),
                   InitState = #{domain => inet6,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case, for IPv4.

traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp4(suite) ->
    [];
traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp4(doc) ->
    [];
traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp4(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_MEDIUM_NUM,
    tc_try(traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp4,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'medium' message test case, for IPv6.

traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp6(suite) ->
    [];
traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp6(doc) ->
    [];
traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp6(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_MEDIUM_NUM,
    tc_try(traffic_ping_pong_medium_sendmsg_and_recvmsg_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(20)),
                   InitState = #{domain => ine6,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case, for IPv4.

traffic_ping_pong_large_sendmsg_and_recvmsg_tcp4(suite) ->
    [];
traffic_ping_pong_large_sendmsg_and_recvmsg_tcp4(doc) ->
    [];
traffic_ping_pong_large_sendmsg_and_recvmsg_tcp4(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_LARGE_NUM,
    tc_try(traffic_ping_pong_large_sendmsg_and_recvmsg_tcp4,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg functions
%% by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes), medium (8K) and large (8M).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'large' message test case, for IPv6.

traffic_ping_pong_large_sendmsg_and_recvmsg_tcp6(suite) ->
    [];
traffic_ping_pong_large_sendmsg_and_recvmsg_tcp6(doc) ->
    [];
traffic_ping_pong_large_sendmsg_and_recvmsg_tcp6(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_LARGE),
    Num = ?TPP_LARGE_NUM,
    tc_try(traffic_ping_pong_large_sendmsg_and_recvmsg_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet6,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_tcp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sendmsg and recvmsg 
%% functions by repeatedly sending a meassage between two entities.
%% The same basic test case is used for three different message sizes; 
%% small (8 bytes) and medium (8K).
%% The message is sent from A to B and then back again. This is 
%% repeated a set number of times (more times the small the message).
%% This is the 'small' message test case, for IPv4.

traffic_ping_pong_small_sendmsg_and_recvmsg_udp4(suite) ->
    [];
traffic_ping_pong_small_sendmsg_and_recvmsg_udp4(doc) ->
    [];
traffic_ping_pong_small_sendmsg_and_recvmsg_udp4(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_SMALL_NUM,
    tc_try(traffic_ping_pong_small_sendmsg_and_recvmsg_udp4,
           fun() ->
                   ?TT(?SECS(60)),
                   InitState = #{domain => inet,
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

traffic_ping_pong_small_sendmsg_and_recvmsg_udp6(suite) ->
    [];
traffic_ping_pong_small_sendmsg_and_recvmsg_udp6(doc) ->
    [];
traffic_ping_pong_small_sendmsg_and_recvmsg_udp6(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_SMALL),
    Num = ?TPP_SMALL_NUM,
    tc_try(traffic_ping_pong_small_sendmsg_and_recvmsg_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet,
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

traffic_ping_pong_medium_sendmsg_and_recvmsg_udp4(suite) ->
    [];
traffic_ping_pong_medium_sendmsg_and_recvmsg_udp4(doc) ->
    [];
traffic_ping_pong_medium_sendmsg_and_recvmsg_udp4(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_MEDIUM_NUM,
    tc_try(traffic_ping_pong_medium_sendmsg_and_recvmsg_udp4,
           fun() ->
                   ?TT(?SECS(30)),
                   InitState = #{domain => inet,
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

traffic_ping_pong_medium_sendmsg_and_recvmsg_udp6(suite) ->
    [];
traffic_ping_pong_medium_sendmsg_and_recvmsg_udp6(doc) ->
    [];
traffic_ping_pong_medium_sendmsg_and_recvmsg_udp6(_Config) when is_list(_Config) ->
    Msg = l2b(?TPP_MEDIUM),
    Num = ?TPP_MEDIUM_NUM,
    tc_try(traffic_ping_pong_medium_sendmsg_and_recvmsg_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(20)),
                   InitState = #{domain => ine6,
                                 msg    => Msg,
                                 num    => Num},
                   ok = traffic_ping_pong_sendmsg_and_recvmsg_udp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ping-Pong for TCP

traffic_ping_pong_send_and_recv_tcp(InitState) ->
    Send = fun(Sock, Data) -> socket:send(Sock, Data) end,
    Recv = fun(Sock, Sz)   -> socket:recv(Sock, Sz) end,
    InitState2 = InitState#{send => Send, % Send function
                            recv => Recv  % Receive function
                           },
    traffic_ping_pong_send_and_receive_tcp(InitState2).

traffic_ping_pong_sendmsg_and_recvmsg_tcp(InitState) ->
    Send = fun(Sock, Data) when is_binary(Data) ->
                   MsgHdr = #{iov => [Data]},
                   socket:sendmsg(Sock, MsgHdr);
              (Sock, Data) when is_list(Data) -> %% We assume iovec...
                   MsgHdr = #{iov => Data},
                   socket:sendmsg(Sock, MsgHdr)
           end,
    Recv = fun(Sock, Sz)   -> 
                   case socket:recvmsg(Sock, Sz, 0) of
                       {ok, #{addr  := undefined,
                              iov   := [Data]}} ->
                           {ok, Data};
                       {error, _} = ERROR ->
                           ERROR
                   end
           end,
    InitState2 = InitState#{send => Send, % Send function
                            recv => Recv  % Receive function
                           },
    traffic_ping_pong_send_and_receive_tcp(InitState2).


traffic_ping_pong_send_and_receive_tcp(#{msg := Msg} = InitState) ->
    Fun = fun(Sock) -> 
                  {ok, RcvSz} = socket:getopt(Sock, socket, rcvbuf),
		  ?SEV_IPRINT("RcvBuf is ~p (needs atleast ~p)", 
			      [RcvSz, 16+size(Msg)]),
                  if (RcvSz < size(Msg)) ->
                          case socket:setopt(Sock,
					     socket, rcvbuf, 1024+size(Msg)) of
			      ok ->
				  ok;
			      {error, enobufs} ->
				  skip({failed_change, rcvbuf});
			      {error, Reason1} ->
				  ?FAIL({rcvbuf, Reason1})
			  end;
                     true ->
                          ok
                  end,
                  {ok, SndSz} = socket:getopt(Sock, socket, sndbuf),
		  ?SEV_IPRINT("SndBuf is ~p (needs atleast ~p)", 
			      [SndSz, 16+size(Msg)]),
                  if (SndSz < size(Msg)) ->
                          case socket:setopt(Sock,
					     socket, sndbuf, 1024+size(Msg)) of
			      ok ->
				  ok;
			      {error, enobufs} ->
				  skip({failed_change, sndbuf});
			      {error, Reason2} ->
				  ?FAIL({sndbuf, Reason2})
			  end;
                     true ->
                          ok
                  end,
                  ok = socket:setopt(Sock, otp, rcvbuf, {12, 1024})
          end,
    traffic_ping_pong_send_and_receive_tcp2(InitState#{buf_init => Fun}).

traffic_ping_pong_send_and_receive_tcp2(InitState) ->
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
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, local_sa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
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
           cmd  => fun(#{tester := Tester, local_sa := LSA, lport := Port}) ->
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
           cmd  => fun(#{lsock := Sock} = State) ->
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
           cmd  => fun(#{host := Host} = State) ->
                           case start_node(Host, client) of
                               {ok, Node} ->
                                   ?SEV_IPRINT("(remote) client node ~p started", 
                                               [Node]),
                                   {ok, State#{node => Node}};
                               {error, Reason} ->
                                   {skip, Reason}
                           end
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
                         server_sa := ServerSA,
                         buf_init  := BufInit,
                         send      := Send,
                         recv      := Recv}) ->
                           ?SEV_ANNOUNCE_START(RClient, 
                                               {ServerSA, BufInit, Send, Recv}),
                           ok
                   end},
         #{desc => "await remote client ready",
           cmd  => fun(#{tester  := Tester,
                         rclient := RClient} = _State) ->
                           ?SEV_AWAIT_READY(RClient, rclient, init, 
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
           cmd  => fun(#{node := Node} = _State) ->
                           stop_node(Node)
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node} = State) ->
                           receive
                               {nodedown, Node} ->
                                   {ok, maps:remove(node, State)}
                           end
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
                           {CSent, CReceived, CStart, CStop} = CRes,
                           STime = tdiff(SStart, SStop),
                           CTime = tdiff(CStart, CStop),
                           %% Note that the sizes we are counting is only 
                           %% the "data" part of the messages. There is also
                           %% fixed header for each message, which of cource
                           %% is small for the large messages, but comparatively
                           %% big for the small messages!
                           ?SEV_IPRINT("Results: ~w messages exchanged"
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
            NewStart = if (Start =:= undefined) -> ?LIB:timestamp(); 
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
        %% {error, timeout} ->
        %%     ?SEV_IPRINT("timeout(~w) - try again", [N]),
        %%     case Send(Sock, list_to_binary("ping")) of
        %%         ok ->
        %%             exit({'ping-send', ok, N});
        %%         {error, Reason} ->
        %%             exit({'ping-send', Reason, N})
        %%     end;
        {error, closed} ->
            ?SEV_IPRINT("closed - we are done: ~w, ~w, ~w", [N, Sent, Received]),
            Stop = ?LIB:timestamp(),
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
    {ServerSA, BufInit, Send, Recv} = tpp_tcp_client_await_start(Parent),
    Domain   = maps:get(family, ServerSA),
    Sock     = tpp_tcp_client_sock_open(Domain, BufInit),
    tpp_tcp_client_sock_bind(Sock, Domain),
    tpp_tcp_client_announce_ready(Parent, init),
    tpp_tcp_client_await_continue(Parent, connect),
    tpp_tcp_client_sock_connect(Sock, ServerSA),
    tpp_tcp_client_announce_ready(Parent, connect),
    {InitMsg, Num} = tpp_tcp_client_await_continue(Parent, send),
    Result = tpp_tcp_client_msg_exchange(Sock, Send, Recv, InitMsg, Num),
    tpp_tcp_client_announce_ready(Parent, send, Result),
    Reason = tpp_tcp_client_await_terminate(Parent),
    tpp_tcp_client_sock_close(Sock),
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
            ok;
        {error, Reason} ->
            Reason
    end.

tpp_tcp_client_msg_exchange(Sock, Send, Recv, InitMsg, Num) ->
    Start = ?LIB:timestamp(),
    tpp_tcp_client_msg_exchange_loop(Sock, Send, Recv, InitMsg, 
                                     Num, 0, 0, 0, Start).

tpp_tcp_client_msg_exchange_loop(Sock, _Send, _Recv, _Msg,
                                 Num, Num, Sent, Received,
                                 Start) ->
    Stop = ?LIB:timestamp(),
    case socket:close(Sock) of
        ok ->
            {Sent, Received, Start, Stop};
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
                                "~n   ~p", [N, Num, RReason, mq()]),
                    exit({recv, RReason, N})
            end;
        {error, SReason} ->
            ?SEV_EPRINT("send (~w of ~w): ~p"
                        "~n   ~p", [N, Num, SReason, mq()]),
            exit({send, SReason, N})
    end.

tpp_tcp_client_sock_open(Domain, BufInit) ->
    case socket:open(Domain, stream, tcp) of
        {ok, Sock} ->
            ok = BufInit(Sock),
            Sock;
        {error, Reason} ->
            exit({open_failed, Reason})
    end.

tpp_tcp_client_sock_bind(Sock, Domain) ->
    LSA = which_local_socket_addr(Domain),
    case socket:bind(Sock, LSA) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            exit({bind, Reason})
    end.

tpp_tcp_client_sock_connect(Sock, ServerSA) ->
    case socket:connect(Sock, ServerSA) of
        ok ->
            ok;
        {error, Reason} ->
            exit({connect, Reason})
    end.

tpp_tcp_client_sock_close(Sock) ->
    case socket:close(Sock) of
        ok ->
            ok;
        {error, Reason} ->
            exit({close, Reason})
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
    

%% size_of_data(Data) when is_binary(Data) ->
%%     size(Data);
%% size_of_data(Data) when is_list(Data) ->
%%     size_of_iovec(Data, 0).

%% size_of_iovec([], Sz) ->
%%     Sz;
%% size_of_iovec([B|IOVec], Sz) ->
%%     size_of_iovec(IOVec, Sz+size(B)).

mq() ->
    mq(self()).

mq(Pid) when is_pid(Pid) ->
    Tag = messages,
    {Tag, Msgs} = process_info(Pid, Tag),
    Msgs.


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
                   MsgHdr = #{addr => Dest, iov => [Data]},
                   socket:sendmsg(Sock, MsgHdr);
              (Sock, Data, Dest) when is_list(Data) -> %% We assume iovec...
                   MsgHdr = #{addr => Dest, iov => Data},
                   socket:sendmsg(Sock, MsgHdr)
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
         #{desc => "create listen socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, dgram, udp) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, local_sa := LSA} = State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, Port} ->
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
           cmd  => fun(#{tester := Tester, local_sa := LSA, port := Port}) ->
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
           cmd  => fun(#{host := Host} = State) ->
                           case start_node(Host, client) of
                               {ok, Node} ->
                                   ?SEV_IPRINT("(remote) client node ~p started", 
                                               [Node]),
                                   {ok, State#{node => Node}};
                               {error, Reason} ->
                                   {skip, Reason}
                           end
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
                         buf_init  := BufInit,
                         send      := Send,
                         recv      := Recv}) ->
                           ?SEV_ANNOUNCE_START(Handler, 
                                               {ServerSA, BufInit, Send, Recv}),
                           ok
                   end},
         #{desc => "await (remote) handler ready",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = _State) ->
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
           cmd  => fun(#{node := Node} = _State) ->
                           stop_node(Node)
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node} = State) ->
                           receive
                               {nodedown, Node} ->
                                   {ok, maps:remove(node, State)}
                           end
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
                           %% Note that the sizes we are counting is only 
                           %% the "data" part of the messages. There is also
                           %% fixed header for each message, which of cource
                           %% is small for the large messages, but comparatively
                           %% big for the small messages!
                           ?SEV_IPRINT("Results: ~w messages exchanged"
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
            NewStart = if (Start =:= undefined) -> ?LIB:timestamp(); 
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
            Stop = ?LIB:timestamp(),
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
    {ServerSA, BufInit, Send, Recv} = tpp_udp_handler_await_start(Parent),
    ?SEV_IPRINT("start command with"
                "~n   ServerSA: ~p", [ServerSA]),
    Domain   = maps:get(family, ServerSA),
    Sock     = tpp_udp_sock_open(Domain, BufInit),
    tpp_udp_sock_bind(Sock, Domain),
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
    tpp_udp_sock_close(Sock),
    ?SEV_IPRINT("terminating"),
    exit(Reason).

tpp_udp_client_handler_init(Parent) ->
    put(sname, "chandler"),
    ?SEV_IPRINT("init"),
    _MRef = erlang:monitor(process, Parent),
    ok.

tpp_udp_client_handler_msg_exchange(Sock, ServerSA,
                                    Send, Recv, InitMsg, Num) ->
    Start = ?LIB:timestamp(),
    tpp_udp_client_handler_msg_exchange_loop(Sock, ServerSA,
                                             Send, Recv, InitMsg,
                                             Num, 0, 0, 0, Start).

tpp_udp_client_handler_msg_exchange_loop(_Sock, _Dest, _Send, _Recv, _Msg,
                                         Num, Num, Sent, Received,
                                         Start) ->
    Stop = ?LIB:timestamp(),
    {Sent, Received, Start, Stop};
tpp_udp_client_handler_msg_exchange_loop(Sock, Dest, Send, Recv, Data,
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
	    {error, {catched, C, E, S}}
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


tpp_udp_sock_open(Domain, BufInit) ->
    case socket:open(Domain, dgram, udp) of
        {ok, Sock} ->
            ok = BufInit(Sock),
            Sock;
        {error, Reason} ->
            exit({open_failed, Reason})
    end.

tpp_udp_sock_bind(Sock, Domain) ->
    LSA = which_local_socket_addr(Domain),
    case socket:bind(Sock, LSA) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            exit({bind, Reason})
    end.

tpp_udp_sock_close(Sock) ->
    case socket:close(Sock) of
        ok ->
            ok;
        {error, Reason} ->
            exit({close, Reason})
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_cgenf_small_tcp4(suite) ->
    [];
ttest_sgenf_cgenf_small_tcp4(doc) ->
    [];
ttest_sgenf_cgenf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgenf_small_tcp4,
              Runtime,
              inet,
              gen, false,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_cgenf_small_tcp6(suite) ->
    [];
ttest_sgenf_cgenf_small_tcp6(doc) ->
    [];
ttest_sgenf_cgenf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgenf_small_tcp6,
              Runtime,
              inet6,
              gen, false,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_cgenf_medium_tcp4(suite) ->
    [];
ttest_sgenf_cgenf_medium_tcp4(doc) ->
    [];
ttest_sgenf_cgenf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgenf_medium_tcp4,
              Runtime,
              inet,
              gen, false,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_cgenf_medium_tcp6(suite) ->
    [];
ttest_sgenf_cgenf_medium_tcp6(doc) ->
    [];
ttest_sgenf_cgenf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgenf_medium_tcp6,
              Runtime,
              inet6,
              gen, false,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_cgenf_large_tcp4(suite) ->
    [];
ttest_sgenf_cgenf_large_tcp4(doc) ->
    [];
ttest_sgenf_cgenf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgenf_large_tcp4,
              Runtime,
              inet,
              gen, false,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_cgenf_large_tcp6(suite) ->
    [];
ttest_sgenf_cgenf_large_tcp6(doc) ->
    [];
ttest_sgenf_cgenf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgenf_large_tcp6,
              Runtime,
              inet6,
              gen, false,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_cgeno_small_tcp4(suite) ->
    [];
ttest_sgenf_cgeno_small_tcp4(doc) ->
    [];
ttest_sgenf_cgeno_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgeno_small_tcp4,
              Runtime,
              inet,
              gen, false,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_cgeno_small_tcp6(suite) ->
    [];
ttest_sgenf_cgeno_small_tcp6(doc) ->
    [];
ttest_sgenf_cgeno_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgeno_small_tcp6,
              Runtime,
              inet6,
              gen, false,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_cgeno_medium_tcp4(suite) ->
    [];
ttest_sgenf_cgeno_medium_tcp4(doc) ->
    [];
ttest_sgenf_cgeno_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgeno_medium_tcp4,
              Runtime,
              inet,
              gen, false,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_cgeno_medium_tcp6(suite) ->
    [];
ttest_sgenf_cgeno_medium_tcp6(doc) ->
    [];
ttest_sgenf_cgeno_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgeno_medium_tcp6,
              Runtime,
              inet6,
              gen, false,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_cgeno_large_tcp4(suite) ->
    [];
ttest_sgenf_cgeno_large_tcp4(doc) ->
    [];
ttest_sgenf_cgeno_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgeno_large_tcp4,
              Runtime,
              inet,
              gen, false,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_cgeno_large_tcp6(suite) ->
    [];
ttest_sgenf_cgeno_large_tcp6(doc) ->
    [];
ttest_sgenf_cgeno_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgeno_large_tcp6,
              Runtime,
              inet6,
              gen, false,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_cgent_small_tcp4(suite) ->
    [];
ttest_sgenf_cgent_small_tcp4(doc) ->
    [];
ttest_sgenf_cgent_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgent_small_tcp4,
              Runtime,
              inet,
              gen, false,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_cgent_small_tcp6(suite) ->
    [];
ttest_sgenf_cgent_small_tcp6(doc) ->
    [];
ttest_sgenf_cgent_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgeno_small_tcp6,
              Runtime,
              inet6,
              gen, false,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_cgent_medium_tcp4(suite) ->
    [];
ttest_sgenf_cgent_medium_tcp4(doc) ->
    [];
ttest_sgenf_cgent_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgent_medium_tcp4,
              Runtime,
              inet,
              gen, false,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_cgent_medium_tcp6(suite) ->
    [];
ttest_sgenf_cgent_medium_tcp6(doc) ->
    [];
ttest_sgenf_cgent_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgent_medium_tcp6,
              Runtime,
              inet6,
              gen, false,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_cgent_large_tcp4(suite) ->
    [];
ttest_sgenf_cgent_large_tcp4(doc) ->
    [];
ttest_sgenf_cgent_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgent_large_tcp4,
              Runtime,
              inet,
              gen, false,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_cgent_large_tcp6(suite) ->
    [];
ttest_sgenf_cgent_large_tcp6(doc) ->
    [];
ttest_sgenf_cgent_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_cgent_large_tcp6,
              Runtime,
              inet6,
              gen, false,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_csockf_small_tcp4(suite) ->
    [];
ttest_sgenf_csockf_small_tcp4(doc) ->
    [];
ttest_sgenf_csockf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csockf_small_tcp4,
              Runtime,
              inet,
              gen, false,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_csockf_small_tcp6(suite) ->
    [];
ttest_sgenf_csockf_small_tcp6(doc) ->
    [];
ttest_sgenf_csockf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csockf_small_tcp6,
              Runtime,
              inet6,
              gen, false,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_csockf_medium_tcp4(suite) ->
    [];
ttest_sgenf_csockf_medium_tcp4(doc) ->
    [];
ttest_sgenf_csockf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csockf_medium_tcp4,
              Runtime,
              inet,
              gen, false,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_csockf_medium_tcp6(suite) ->
    [];
ttest_sgenf_csockf_medium_tcp6(doc) ->
    [];
ttest_sgenf_csockf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csockf_medium_tcp6,
              Runtime,
              inet6,
              gen, false,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_csockf_large_tcp4(suite) ->
    [];
ttest_sgenf_csockf_large_tcp4(doc) ->
    [];
ttest_sgenf_csockf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csockf_large_tcp4,
              Runtime,
              inet,
              gen, false,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_csockf_large_tcp6(suite) ->
    [];
ttest_sgenf_csockf_large_tcp6(doc) ->
    [];
ttest_sgenf_csockf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csockf_large_tcp6,
              Runtime,
              inet6,
              gen, false,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_csocko_small_tcp4(suite) ->
    [];
ttest_sgenf_csocko_small_tcp4(doc) ->
    [];
ttest_sgenf_csocko_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csocko_small_tcp4,
              Runtime,
              inet,
              gen, false,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_csocko_small_tcp6(suite) ->
    [];
ttest_sgenf_csocko_small_tcp6(doc) ->
    [];
ttest_sgenf_csocko_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csocko_small_tcp6,
              Runtime,
              inet6,
              gen, false,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_csocko_medium_tcp4(suite) ->
    [];
ttest_sgenf_csocko_medium_tcp4(doc) ->
    [];
ttest_sgenf_csocko_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csocko_medium_tcp4,
              Runtime,
              inet,
              gen, false,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_csocko_medium_tcp6(suite) ->
    [];
ttest_sgenf_csocko_medium_tcp6(doc) ->
    [];
ttest_sgenf_csocko_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csocko_medium_tcp6,
              Runtime,
              inet6,
              gen, false,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_csocko_large_tcp4(suite) ->
    [];
ttest_sgenf_csocko_large_tcp4(doc) ->
    [];
ttest_sgenf_csocko_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csocko_large_tcp4,
              Runtime,
              inet,
              gen, false,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_csocko_large_tcp6(suite) ->
    [];
ttest_sgenf_csocko_large_tcp6(doc) ->
    [];
ttest_sgenf_csocko_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csocko_large_tcp6,
              Runtime,
              inet6,
              gen, false,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_csockt_small_tcp4(suite) ->
    [];
ttest_sgenf_csockt_small_tcp4(doc) ->
    [];
ttest_sgenf_csockt_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csockt_small_tcp4,
              Runtime,
              inet,
              gen, false,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_csockt_small_tcp6(suite) ->
    [];
ttest_sgenf_csockt_small_tcp6(doc) ->
    [];
ttest_sgenf_csockt_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csocko_small_tcp6,
              Runtime,
              inet6,
              gen, false,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_csockt_medium_tcp4(suite) ->
    [];
ttest_sgenf_csockt_medium_tcp4(doc) ->
    [];
ttest_sgenf_csockt_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csockt_medium_tcp4,
              Runtime,
              inet,
              gen, false,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_csockt_medium_tcp6(suite) ->
    [];
ttest_sgenf_csockt_medium_tcp6(doc) ->
    [];
ttest_sgenf_csockt_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csockt_medium_tcp6,
              Runtime,
              inet6,
              gen, false,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_csockt_large_tcp4(suite) ->
    [];
ttest_sgenf_csockt_large_tcp4(doc) ->
    [];
ttest_sgenf_csockt_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csockt_large_tcp4,
              Runtime,
              inet,
              gen, false,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_csockt_large_tcp6(suite) ->
    [];
ttest_sgenf_csockt_large_tcp6(doc) ->
    [];
ttest_sgenf_csockt_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgenf_csockt_large_tcp6,
              Runtime,
              inet6,
              gen, false,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_cgenf_small_tcp4(suite) ->
    [];
ttest_sgeno_cgenf_small_tcp4(doc) ->
    [];
ttest_sgeno_cgenf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgenf_small_tcp4,
              Runtime,
              inet,
              gen, once,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_cgenf_small_tcp6(suite) ->
    [];
ttest_sgeno_cgenf_small_tcp6(doc) ->
    [];
ttest_sgeno_cgenf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgenf_small_tcp6,
              Runtime,
              inet6,
              gen, once,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_cgenf_medium_tcp4(suite) ->
    [];
ttest_sgeno_cgenf_medium_tcp4(doc) ->
    [];
ttest_sgeno_cgenf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgenf_medium_tcp4,
              Runtime,
              inet,
              gen, once,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_cgenf_medium_tcp6(suite) ->
    [];
ttest_sgeno_cgenf_medium_tcp6(doc) ->
    [];
ttest_sgeno_cgenf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgenf_medium_tcp6,
              Runtime,
              inet6,
              gen, once,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_cgenf_large_tcp4(suite) ->
    [];
ttest_sgeno_cgenf_large_tcp4(doc) ->
    [];
ttest_sgeno_cgenf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgenf_large_tcp4,
              Runtime,
              inet,
              gen, once,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_cgenf_large_tcp6(suite) ->
    [];
ttest_sgeno_cgenf_large_tcp6(doc) ->
    [];
ttest_sgeno_cgenf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgenf_large_tcp6,
              Runtime,
              inet6,
              gen, once,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_cgeno_small_tcp4(suite) ->
    [];
ttest_sgeno_cgeno_small_tcp4(doc) ->
    [];
ttest_sgeno_cgeno_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgeno_small_tcp4,
              Runtime,
              inet,
              gen, once,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_cgeno_small_tcp6(suite) ->
    [];
ttest_sgeno_cgeno_small_tcp6(doc) ->
    [];
ttest_sgeno_cgeno_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgeno_small_tcp6,
              Runtime,
              inet6,
              gen, once,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_cgeno_medium_tcp4(suite) ->
    [];
ttest_sgeno_cgeno_medium_tcp4(doc) ->
    [];
ttest_sgeno_cgeno_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgeno_medium_tcp4,
              Runtime,
              inet,
              gen, once,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_cgeno_medium_tcp6(suite) ->
    [];
ttest_sgeno_cgeno_medium_tcp6(doc) ->
    [];
ttest_sgeno_cgeno_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgeno_medium_tcp6,
              Runtime,
              inet6,
              gen, once,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_cgeno_large_tcp4(suite) ->
    [];
ttest_sgeno_cgeno_large_tcp4(doc) ->
    [];
ttest_sgeno_cgeno_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgeno_large_tcp4,
              Runtime,
              inet,
              gen, once,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_cgeno_large_tcp6(suite) ->
    [];
ttest_sgeno_cgeno_large_tcp6(doc) ->
    [];
ttest_sgeno_cgeno_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgeno_large_tcp6,
              Runtime,
              inet6,
              gen, once,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_cgent_small_tcp4(suite) ->
    [];
ttest_sgeno_cgent_small_tcp4(doc) ->
    [];
ttest_sgeno_cgent_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgent_small_tcp4,
              Runtime,
              inet,
              gen, once,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_cgent_small_tcp6(suite) ->
    [];
ttest_sgeno_cgent_small_tcp6(doc) ->
    [];
ttest_sgeno_cgent_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgeno_small_tcp6,
              Runtime,
              inet6,
              gen, once,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_cgent_medium_tcp4(suite) ->
    [];
ttest_sgeno_cgent_medium_tcp4(doc) ->
    [];
ttest_sgeno_cgent_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgent_medium_tcp4,
              Runtime,
              inet,
              gen, once,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_cgent_medium_tcp6(suite) ->
    [];
ttest_sgeno_cgent_medium_tcp6(doc) ->
    [];
ttest_sgeno_cgent_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgent_medium_tcp6,
              Runtime,
              inet6,
              gen, once,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_cgent_large_tcp4(suite) ->
    [];
ttest_sgeno_cgent_large_tcp4(doc) ->
    [];
ttest_sgeno_cgent_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgent_large_tcp4,
              Runtime,
              inet,
              gen, once,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_cgent_large_tcp6(suite) ->
    [];
ttest_sgeno_cgent_large_tcp6(doc) ->
    [];
ttest_sgeno_cgent_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_cgent_large_tcp6,
              Runtime,
              inet6,
              gen, once,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_csockf_small_tcp4(suite) ->
    [];
ttest_sgeno_csockf_small_tcp4(doc) ->
    [];
ttest_sgeno_csockf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csockf_small_tcp4,
              Runtime,
              inet,
              gen, once,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_csockf_small_tcp6(suite) ->
    [];
ttest_sgeno_csockf_small_tcp6(doc) ->
    [];
ttest_sgeno_csockf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csockf_small_tcp6,
              Runtime,
              inet6,
              gen, once,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_csockf_medium_tcp4(suite) ->
    [];
ttest_sgeno_csockf_medium_tcp4(doc) ->
    [];
ttest_sgeno_csockf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csockf_medium_tcp4,
              Runtime,
              inet,
              gen, once,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_csockf_medium_tcp6(suite) ->
    [];
ttest_sgeno_csockf_medium_tcp6(doc) ->
    [];
ttest_sgeno_csockf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csockf_medium_tcp6,
              Runtime,
              inet6,
              gen, once,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_csockf_large_tcp4(suite) ->
    [];
ttest_sgeno_csockf_large_tcp4(doc) ->
    [];
ttest_sgeno_csockf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csockf_large_tcp4,
              Runtime,
              inet,
              gen, once,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_csockf_large_tcp6(suite) ->
    [];
ttest_sgeno_csockf_large_tcp6(doc) ->
    [];
ttest_sgeno_csockf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csockf_large_tcp6,
              Runtime,
              inet6,
              gen, once,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_csocko_small_tcp4(suite) ->
    [];
ttest_sgeno_csocko_small_tcp4(doc) ->
    [];
ttest_sgeno_csocko_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csocko_small_tcp4,
              Runtime,
              inet,
              gen, once,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_csocko_small_tcp6(suite) ->
    [];
ttest_sgeno_csocko_small_tcp6(doc) ->
    [];
ttest_sgeno_csocko_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csocko_small_tcp6,
              Runtime,
              inet6,
              gen, once,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_csocko_medium_tcp4(suite) ->
    [];
ttest_sgeno_csocko_medium_tcp4(doc) ->
    [];
ttest_sgeno_csocko_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csocko_medium_tcp4,
              Runtime,
              inet,
              gen, once,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_csocko_medium_tcp6(suite) ->
    [];
ttest_sgeno_csocko_medium_tcp6(doc) ->
    [];
ttest_sgeno_csocko_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csocko_medium_tcp6,
              Runtime,
              inet6,
              gen, once,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_csocko_large_tcp4(suite) ->
    [];
ttest_sgeno_csocko_large_tcp4(doc) ->
    [];
ttest_sgeno_csocko_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csocko_large_tcp4,
              Runtime,
              inet,
              gen, once,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_csocko_large_tcp6(suite) ->
    [];
ttest_sgeno_csocko_large_tcp6(doc) ->
    [];
ttest_sgeno_csocko_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csocko_large_tcp6,
              Runtime,
              inet6,
              gen, once,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_csockt_small_tcp4(suite) ->
    [];
ttest_sgeno_csockt_small_tcp4(doc) ->
    [];
ttest_sgeno_csockt_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csockt_small_tcp4,
              Runtime,
              inet,
              gen, once,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_csockt_small_tcp6(suite) ->
    [];
ttest_sgeno_csockt_small_tcp6(doc) ->
    [];
ttest_sgeno_csockt_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csocko_small_tcp6,
              Runtime,
              inet6,
              gen, once,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_csockt_medium_tcp4(suite) ->
    [];
ttest_sgeno_csockt_medium_tcp4(doc) ->
    [];
ttest_sgeno_csockt_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csockt_medium_tcp4,
              Runtime,
              inet,
              gen, once,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_csockt_medium_tcp6(suite) ->
    [];
ttest_sgeno_csockt_medium_tcp6(doc) ->
    [];
ttest_sgeno_csockt_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csockt_medium_tcp6,
              Runtime,
              inet6,
              gen, once,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_csockt_large_tcp4(suite) ->
    [];
ttest_sgeno_csockt_large_tcp4(doc) ->
    [];
ttest_sgeno_csockt_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csockt_large_tcp4,
              Runtime,
              inet,
              gen, once,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_csockt_large_tcp6(suite) ->
    [];
ttest_sgeno_csockt_large_tcp6(doc) ->
    [];
ttest_sgeno_csockt_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgeno_csockt_large_tcp6,
              Runtime,
              inet6,
              gen, once,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_cgenf_small_tcp4(suite) ->
    [];
ttest_sgent_cgenf_small_tcp4(doc) ->
    [];
ttest_sgent_cgenf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgenf_small_tcp4,
              Runtime,
              inet,
              gen, true,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_cgenf_small_tcp6(suite) ->
    [];
ttest_sgent_cgenf_small_tcp6(doc) ->
    [];
ttest_sgent_cgenf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgenf_small_tcp6,
              Runtime,
              inet6,
              gen, true,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_cgenf_medium_tcp4(suite) ->
    [];
ttest_sgent_cgenf_medium_tcp4(doc) ->
    [];
ttest_sgent_cgenf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgenf_medium_tcp4,
              Runtime,
              inet,
              gen, true,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_cgenf_medium_tcp6(suite) ->
    [];
ttest_sgent_cgenf_medium_tcp6(doc) ->
    [];
ttest_sgent_cgenf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgenf_medium_tcp6,
              Runtime,
              inet6,
              gen, true,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_cgenf_large_tcp4(suite) ->
    [];
ttest_sgent_cgenf_large_tcp4(doc) ->
    [];
ttest_sgent_cgenf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgenf_large_tcp4,
              Runtime,
              inet,
              gen, true,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_cgenf_large_tcp6(suite) ->
    [];
ttest_sgent_cgenf_large_tcp6(doc) ->
    [];
ttest_sgent_cgenf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgenf_large_tcp6,
              Runtime,
              inet6,
              gen, true,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_cgeno_small_tcp4(suite) ->
    [];
ttest_sgent_cgeno_small_tcp4(doc) ->
    [];
ttest_sgent_cgeno_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgeno_small_tcp4,
              Runtime,
              inet,
              gen, true,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_cgeno_small_tcp6(suite) ->
    [];
ttest_sgent_cgeno_small_tcp6(doc) ->
    [];
ttest_sgent_cgeno_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgeno_small_tcp6,
              Runtime,
              inet6,
              gen, true,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_cgeno_medium_tcp4(suite) ->
    [];
ttest_sgent_cgeno_medium_tcp4(doc) ->
    [];
ttest_sgent_cgeno_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgeno_medium_tcp4,
              Runtime,
              inet,
              gen, true,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_cgeno_medium_tcp6(suite) ->
    [];
ttest_sgent_cgeno_medium_tcp6(doc) ->
    [];
ttest_sgent_cgeno_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgeno_medium_tcp6,
              Runtime,
              inet6,
              gen, true,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_cgeno_large_tcp4(suite) ->
    [];
ttest_sgent_cgeno_large_tcp4(doc) ->
    [];
ttest_sgent_cgeno_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgeno_large_tcp4,
              Runtime,
              inet,
              gen, true,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_cgeno_large_tcp6(suite) ->
    [];
ttest_sgent_cgeno_large_tcp6(doc) ->
    [];
ttest_sgent_cgeno_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgeno_large_tcp6,
              Runtime,
              inet6,
              gen, true,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_cgent_small_tcp4(suite) ->
    [];
ttest_sgent_cgent_small_tcp4(doc) ->
    [];
ttest_sgent_cgent_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgent_small_tcp4,
              Runtime,
              inet,
              gen, true,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_cgent_small_tcp6(suite) ->
    [];
ttest_sgent_cgent_small_tcp6(doc) ->
    [];
ttest_sgent_cgent_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgeno_small_tcp6,
              Runtime,
              inet6,
              gen, true,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_cgent_medium_tcp4(suite) ->
    [];
ttest_sgent_cgent_medium_tcp4(doc) ->
    ["Server(gen,true), Client(gen,true), Domain=inet, msg=medium"];
ttest_sgent_cgent_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgent_medium_tcp4,
              Runtime,
              inet,
              gen, true,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_cgent_medium_tcp6(suite) ->
    [];
ttest_sgent_cgent_medium_tcp6(doc) ->
    ["Server(gen,true), Client(gen,true), Domain=inet6, msg=medium"];
ttest_sgent_cgent_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgent_medium_tcp6,
              Runtime,
              inet6,
              gen, true,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_cgent_large_tcp4(suite) ->
    [];
ttest_sgent_cgent_large_tcp4(doc) ->
    ["Server(gen,true), Client(gen,true), Domain=inet, msg=large"];
ttest_sgent_cgent_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgent_large_tcp4,
              Runtime,
              inet,
              gen, true,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_cgent_large_tcp6(suite) ->
    [];
ttest_sgent_cgent_large_tcp6(doc) ->
    ["Server(gen,true), Client(gen,true), Domain=inet6, msg=large"];
ttest_sgent_cgent_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_cgent_large_tcp6,
              Runtime,
              inet6,
              gen, true,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_csockf_small_tcp4(suite) ->
    [];
ttest_sgent_csockf_small_tcp4(doc) ->
    [];
ttest_sgent_csockf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csockf_small_tcp4,
              Runtime,
              inet,
              gen, true,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_csockf_small_tcp6(suite) ->
    [];
ttest_sgent_csockf_small_tcp6(doc) ->
    [];
ttest_sgent_csockf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csockf_small_tcp6,
              Runtime,
              inet6,
              gen, true,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_csockf_medium_tcp4(suite) ->
    [];
ttest_sgent_csockf_medium_tcp4(doc) ->
    [];
ttest_sgent_csockf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csockf_medium_tcp4,
              Runtime,
              inet,
              gen, true,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_csockf_medium_tcp6(suite) ->
    [];
ttest_sgent_csockf_medium_tcp6(doc) ->
    [];
ttest_sgent_csockf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csockf_medium_tcp6,
              Runtime,
              inet6,
              gen, true,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_csockf_large_tcp4(suite) ->
    [];
ttest_sgent_csockf_large_tcp4(doc) ->
    [];
ttest_sgent_csockf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csockf_large_tcp4,
              Runtime,
              inet,
              gen, true,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_csockf_large_tcp6(suite) ->
    [];
ttest_sgent_csockf_large_tcp6(doc) ->
    [];
ttest_sgent_csockf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csockf_large_tcp6,
              Runtime,
              inet6,
              gen, true,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_csocko_small_tcp4(suite) ->
    [];
ttest_sgent_csocko_small_tcp4(doc) ->
    [];
ttest_sgent_csocko_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csocko_small_tcp4,
              Runtime,
              inet,
              gen, true,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_csocko_small_tcp6(suite) ->
    [];
ttest_sgent_csocko_small_tcp6(doc) ->
    [];
ttest_sgent_csocko_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csocko_small_tcp6,
              Runtime,
              inet6,
              gen, true,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_csocko_medium_tcp4(suite) ->
    [];
ttest_sgent_csocko_medium_tcp4(doc) ->
    [];
ttest_sgent_csocko_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csocko_medium_tcp4,
              Runtime,
              inet,
              gen, true,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_csocko_medium_tcp6(suite) ->
    [];
ttest_sgent_csocko_medium_tcp6(doc) ->
    [];
ttest_sgent_csocko_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csocko_medium_tcp6,
              Runtime,
              inet6,
              gen, true,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_csocko_large_tcp4(suite) ->
    [];
ttest_sgent_csocko_large_tcp4(doc) ->
    [];
ttest_sgent_csocko_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csocko_large_tcp4,
              Runtime,
              inet,
              gen, true,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_csocko_large_tcp6(suite) ->
    [];
ttest_sgent_csocko_large_tcp6(doc) ->
    [];
ttest_sgent_csocko_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csocko_large_tcp6,
              Runtime,
              inet6,
              gen, true,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_csockt_small_tcp4(suite) ->
    [];
ttest_sgent_csockt_small_tcp4(doc) ->
    [];
ttest_sgent_csockt_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csockt_small_tcp4,
              Runtime,
              inet,
              gen, true,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_csockt_small_tcp6(suite) ->
    [];
ttest_sgent_csockt_small_tcp6(doc) ->
    [];
ttest_sgent_csockt_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csocko_small_tcp6,
              Runtime,
              inet6,
              gen, true,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_csockt_medium_tcp4(suite) ->
    [];
ttest_sgent_csockt_medium_tcp4(doc) ->
    [];
ttest_sgent_csockt_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csockt_medium_tcp4,
              Runtime,
              inet,
              gen, true,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_csockt_medium_tcp6(suite) ->
    [];
ttest_sgent_csockt_medium_tcp6(doc) ->
    [];
ttest_sgent_csockt_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csockt_medium_tcp6,
              Runtime,
              inet6,
              gen, true,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_csockt_large_tcp4(suite) ->
    [];
ttest_sgent_csockt_large_tcp4(doc) ->
    [];
ttest_sgent_csockt_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csockt_large_tcp4,
              Runtime,
              inet,
              gen, true,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_csockt_large_tcp6(suite) ->
    [];
ttest_sgent_csockt_large_tcp6(doc) ->
    [];
ttest_sgent_csockt_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_sgent_csockt_large_tcp6,
              Runtime,
              inet6,
              gen, true,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_cgenf_small_tcp4(suite) ->
    [];
ttest_ssockf_cgenf_small_tcp4(doc) ->
    [];
ttest_ssockf_cgenf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgenf_small_tcp4,
              Runtime,
              inet,
              sock, false,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_cgenf_small_tcp6(suite) ->
    [];
ttest_ssockf_cgenf_small_tcp6(doc) ->
    [];
ttest_ssockf_cgenf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgenf_small_tcp6,
              Runtime,
              inet6,
              sock, false,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_cgenf_medium_tcp4(suite) ->
    [];
ttest_ssockf_cgenf_medium_tcp4(doc) ->
    [];
ttest_ssockf_cgenf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgenf_medium_tcp4,
              Runtime,
              inet,
              sock, false,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_cgenf_medium_tcp6(suite) ->
    [];
ttest_ssockf_cgenf_medium_tcp6(doc) ->
    [];
ttest_ssockf_cgenf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgenf_medium_tcp6,
              Runtime,
              inet6,
              sock, false,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_cgenf_large_tcp4(suite) ->
    [];
ttest_ssockf_cgenf_large_tcp4(doc) ->
    [];
ttest_ssockf_cgenf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgenf_large_tcp4,
              Runtime,
              inet,
              sock, false,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_cgenf_large_tcp6(suite) ->
    [];
ttest_ssockf_cgenf_large_tcp6(doc) ->
    [];
ttest_ssockf_cgenf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgenf_large_tcp6,
              Runtime,
              inet6,
              sock, false,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_cgeno_small_tcp4(suite) ->
    [];
ttest_ssockf_cgeno_small_tcp4(doc) ->
    [];
ttest_ssockf_cgeno_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgeno_small_tcp4,
              Runtime,
              inet,
              sock, false,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_cgeno_small_tcp6(suite) ->
    [];
ttest_ssockf_cgeno_small_tcp6(doc) ->
    [];
ttest_ssockf_cgeno_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgeno_small_tcp6,
              Runtime,
              inet6,
              sock, false,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_cgeno_medium_tcp4(suite) ->
    [];
ttest_ssockf_cgeno_medium_tcp4(doc) ->
    [];
ttest_ssockf_cgeno_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgeno_medium_tcp4,
              Runtime,
              inet,
              sock, false,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_cgeno_medium_tcp6(suite) ->
    [];
ttest_ssockf_cgeno_medium_tcp6(doc) ->
    [];
ttest_ssockf_cgeno_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgeno_medium_tcp6,
              Runtime,
              inet6,
              sock, false,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_cgeno_large_tcp4(suite) ->
    [];
ttest_ssockf_cgeno_large_tcp4(doc) ->
    [];
ttest_ssockf_cgeno_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgeno_large_tcp4,
              Runtime,
              inet,
              sock, false,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_cgeno_large_tcp6(suite) ->
    [];
ttest_ssockf_cgeno_large_tcp6(doc) ->
    [];
ttest_ssockf_cgeno_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgeno_large_tcp6,
              Runtime,
              inet6,
              sock, false,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_cgent_small_tcp4(suite) ->
    [];
ttest_ssockf_cgent_small_tcp4(doc) ->
    [];
ttest_ssockf_cgent_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgent_small_tcp4,
              Runtime,
              inet,
              sock, false,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_cgent_small_tcp6(suite) ->
    [];
ttest_ssockf_cgent_small_tcp6(doc) ->
    [];
ttest_ssockf_cgent_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgeno_small_tcp6,
              Runtime,
              inet6,
              sock, false,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_cgent_medium_tcp4(suite) ->
    [];
ttest_ssockf_cgent_medium_tcp4(doc) ->
    [];
ttest_ssockf_cgent_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgent_medium_tcp4,
              Runtime,
              inet,
              sock, false,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_cgent_medium_tcp6(suite) ->
    [];
ttest_ssockf_cgent_medium_tcp6(doc) ->
    [];
ttest_ssockf_cgent_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgent_medium_tcp6,
              Runtime,
              inet6,
              sock, false,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_cgent_large_tcp4(suite) ->
    [];
ttest_ssockf_cgent_large_tcp4(doc) ->
    [];
ttest_ssockf_cgent_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgent_large_tcp4,
              Runtime,
              inet,
              sock, false,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_cgent_large_tcp6(suite) ->
    [];
ttest_ssockf_cgent_large_tcp6(doc) ->
    [];
ttest_ssockf_cgent_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_cgent_large_tcp6,
              Runtime,
              inet6,
              sock, false,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_csockf_small_tcp4(suite) ->
    [];
ttest_ssockf_csockf_small_tcp4(doc) ->
    [];
ttest_ssockf_csockf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csockf_small_tcp4,
              Runtime,
              inet,
              sock, false,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_csockf_small_tcp6(suite) ->
    [];
ttest_ssockf_csockf_small_tcp6(doc) ->
    [];
ttest_ssockf_csockf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csockf_small_tcp6,
              Runtime,
              inet6,
              sock, false,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_csockf_medium_tcp4(suite) ->
    [];
ttest_ssockf_csockf_medium_tcp4(doc) ->
    [];
ttest_ssockf_csockf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csockf_medium_tcp4,
              Runtime,
              inet,
              sock, false,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_csockf_medium_tcp6(suite) ->
    [];
ttest_ssockf_csockf_medium_tcp6(doc) ->
    [];
ttest_ssockf_csockf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csockf_medium_tcp6,
              Runtime,
              inet6,
              sock, false,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_csockf_large_tcp4(suite) ->
    [];
ttest_ssockf_csockf_large_tcp4(doc) ->
    [];
ttest_ssockf_csockf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csockf_large_tcp4,
              Runtime,
              inet,
              sock, false,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_csockf_large_tcp6(suite) ->
    [];
ttest_ssockf_csockf_large_tcp6(doc) ->
    [];
ttest_ssockf_csockf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csockf_large_tcp6,
              Runtime,
              inet6,
              sock, false,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_csocko_small_tcp4(suite) ->
    [];
ttest_ssockf_csocko_small_tcp4(doc) ->
    [];
ttest_ssockf_csocko_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csocko_small_tcp4,
              Runtime,
              inet,
              sock, false,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_csocko_small_tcp6(suite) ->
    [];
ttest_ssockf_csocko_small_tcp6(doc) ->
    [];
ttest_ssockf_csocko_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csocko_small_tcp6,
              Runtime,
              inet6,
              sock, false,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_csocko_medium_tcp4(suite) ->
    [];
ttest_ssockf_csocko_medium_tcp4(doc) ->
    [];
ttest_ssockf_csocko_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csocko_medium_tcp4,
              Runtime,
              inet,
              sock, false,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_csocko_medium_tcp6(suite) ->
    [];
ttest_ssockf_csocko_medium_tcp6(doc) ->
    [];
ttest_ssockf_csocko_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csocko_medium_tcp6,
              Runtime,
              inet6,
              sock, false,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_csocko_large_tcp4(suite) ->
    [];
ttest_ssockf_csocko_large_tcp4(doc) ->
    [];
ttest_ssockf_csocko_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csocko_large_tcp4,
              Runtime,
              inet,
              sock, false,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_csocko_large_tcp6(suite) ->
    [];
ttest_ssockf_csocko_large_tcp6(doc) ->
    [];
ttest_ssockf_csocko_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csocko_large_tcp6,
              Runtime,
              inet6,
              sock, false,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_csockt_small_tcp4(suite) ->
    [];
ttest_ssockf_csockt_small_tcp4(doc) ->
    [];
ttest_ssockf_csockt_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csockt_small_tcp4,
              Runtime,
              inet,
              sock, false,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_csockt_small_tcp6(suite) ->
    [];
ttest_ssockf_csockt_small_tcp6(doc) ->
    [];
ttest_ssockf_csockt_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csocko_small_tcp6,
              Runtime,
              inet6,
              sock, false,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_csockt_medium_tcp4(suite) ->
    [];
ttest_ssockf_csockt_medium_tcp4(doc) ->
    [];
ttest_ssockf_csockt_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csockt_medium_tcp4,
              Runtime,
              inet,
              sock, false,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_csockt_medium_tcp6(suite) ->
    [];
ttest_ssockf_csockt_medium_tcp6(doc) ->
    [];
ttest_ssockf_csockt_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csockt_medium_tcp6,
              Runtime,
              inet6,
              sock, false,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_csockt_large_tcp4(suite) ->
    [];
ttest_ssockf_csockt_large_tcp4(doc) ->
    [];
ttest_ssockf_csockt_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csockt_large_tcp4,
              Runtime,
              inet,
              sock, false,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_csockt_large_tcp6(suite) ->
    [];
ttest_ssockf_csockt_large_tcp6(doc) ->
    [];
ttest_ssockf_csockt_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockf_csockt_large_tcp6,
              Runtime,
              inet6,
              sock, false,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_cgenf_small_tcp4(suite) ->
    [];
ttest_ssocko_cgenf_small_tcp4(doc) ->
    [];
ttest_ssocko_cgenf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgenf_small_tcp4,
              Runtime,
              inet,
              sock, once,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_cgenf_small_tcp6(suite) ->
    [];
ttest_ssocko_cgenf_small_tcp6(doc) ->
    [];
ttest_ssocko_cgenf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgenf_small_tcp6,
              Runtime,
              inet6,
              sock, once,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_cgenf_medium_tcp4(suite) ->
    [];
ttest_ssocko_cgenf_medium_tcp4(doc) ->
    [];
ttest_ssocko_cgenf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgenf_medium_tcp4,
              Runtime,
              inet,
              sock, once,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_cgenf_medium_tcp6(suite) ->
    [];
ttest_ssocko_cgenf_medium_tcp6(doc) ->
    [];
ttest_ssocko_cgenf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgenf_medium_tcp6,
              Runtime,
              inet6,
              sock, once,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_cgenf_large_tcp4(suite) ->
    [];
ttest_ssocko_cgenf_large_tcp4(doc) ->
    [];
ttest_ssocko_cgenf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgenf_large_tcp4,
              Runtime,
              inet,
              sock, once,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_cgenf_large_tcp6(suite) ->
    [];
ttest_ssocko_cgenf_large_tcp6(doc) ->
    [];
ttest_ssocko_cgenf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgenf_large_tcp6,
              Runtime,
              inet6,
              sock, once,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_cgeno_small_tcp4(suite) ->
    [];
ttest_ssocko_cgeno_small_tcp4(doc) ->
    [];
ttest_ssocko_cgeno_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgeno_small_tcp4,
              Runtime,
              inet,
              sock, once,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_cgeno_small_tcp6(suite) ->
    [];
ttest_ssocko_cgeno_small_tcp6(doc) ->
    [];
ttest_ssocko_cgeno_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgeno_small_tcp6,
              Runtime,
              inet6,
              sock, once,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_cgeno_medium_tcp4(suite) ->
    [];
ttest_ssocko_cgeno_medium_tcp4(doc) ->
    [];
ttest_ssocko_cgeno_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgeno_medium_tcp4,
              Runtime,
              inet,
              sock, once,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_cgeno_medium_tcp6(suite) ->
    [];
ttest_ssocko_cgeno_medium_tcp6(doc) ->
    [];
ttest_ssocko_cgeno_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgeno_medium_tcp6,
              Runtime,
              inet6,
              sock, once,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_cgeno_large_tcp4(suite) ->
    [];
ttest_ssocko_cgeno_large_tcp4(doc) ->
    [];
ttest_ssocko_cgeno_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgeno_large_tcp4,
              Runtime,
              inet,
              sock, once,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_cgeno_large_tcp6(suite) ->
    [];
ttest_ssocko_cgeno_large_tcp6(doc) ->
    [];
ttest_ssocko_cgeno_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgeno_large_tcp6,
              Runtime,
              inet6,
              sock, once,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_cgent_small_tcp4(suite) ->
    [];
ttest_ssocko_cgent_small_tcp4(doc) ->
    [];
ttest_ssocko_cgent_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgent_small_tcp4,
              Runtime,
              inet,
              sock, once,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_cgent_small_tcp6(suite) ->
    [];
ttest_ssocko_cgent_small_tcp6(doc) ->
    [];
ttest_ssocko_cgent_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgent_small_tcp6,
              Runtime,
              inet6,
              sock, once,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_cgent_medium_tcp4(suite) ->
    [];
ttest_ssocko_cgent_medium_tcp4(doc) ->
    [];
ttest_ssocko_cgent_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgent_medium_tcp4,
              Runtime,
              inet,
              sock, once,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_cgent_medium_tcp6(suite) ->
    [];
ttest_ssocko_cgent_medium_tcp6(doc) ->
    [];
ttest_ssocko_cgent_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgent_medium_tcp6,
              Runtime,
              inet6,
              sock, once,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_cgent_large_tcp4(suite) ->
    [];
ttest_ssocko_cgent_large_tcp4(doc) ->
    [];
ttest_ssocko_cgent_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgent_large_tcp4,
              Runtime,
              inet,
              sock, once,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_cgent_large_tcp6(suite) ->
    [];
ttest_ssocko_cgent_large_tcp6(doc) ->
    [];
ttest_ssocko_cgent_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_cgent_large_tcp6,
              Runtime,
              inet6,
              sock, once,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_csockf_small_tcp4(suite) ->
    [];
ttest_ssocko_csockf_small_tcp4(doc) ->
    [];
ttest_ssocko_csockf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csockf_small_tcp4,
              Runtime,
              inet,
              sock, once,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_csockf_small_tcp6(suite) ->
    [];
ttest_ssocko_csockf_small_tcp6(doc) ->
    [];
ttest_ssocko_csockf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csockf_small_tcp6,
              Runtime,
              inet6,
              sock, once,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_csockf_medium_tcp4(suite) ->
    [];
ttest_ssocko_csockf_medium_tcp4(doc) ->
    [];
ttest_ssocko_csockf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csockf_medium_tcp4,
              Runtime,
              inet,
              sock, once,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_csockf_medium_tcp6(suite) ->
    [];
ttest_ssocko_csockf_medium_tcp6(doc) ->
    [];
ttest_ssocko_csockf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csockf_medium_tcp6,
              Runtime,
              inet6,
              sock, once,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_csockf_large_tcp4(suite) ->
    [];
ttest_ssocko_csockf_large_tcp4(doc) ->
    [];
ttest_ssocko_csockf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csockf_large_tcp4,
              Runtime,
              inet,
              sock, once,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_csockf_large_tcp6(suite) ->
    [];
ttest_ssocko_csockf_large_tcp6(doc) ->
    [];
ttest_ssocko_csockf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csockf_large_tcp6,
              Runtime,
              inet6,
              sock, once,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_csocko_small_tcp4(suite) ->
    [];
ttest_ssocko_csocko_small_tcp4(doc) ->
    [];
ttest_ssocko_csocko_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csocko_small_tcp4,
              Runtime,
              inet,
              sock, once,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_csocko_small_tcp6(suite) ->
    [];
ttest_ssocko_csocko_small_tcp6(doc) ->
    [];
ttest_ssocko_csocko_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csocko_small_tcp6,
              Runtime,
              inet6,
              sock, once,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_csocko_medium_tcp4(suite) ->
    [];
ttest_ssocko_csocko_medium_tcp4(doc) ->
    [];
ttest_ssocko_csocko_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csocko_medium_tcp4,
              Runtime,
              inet,
              sock, once,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_csocko_medium_tcp6(suite) ->
    [];
ttest_ssocko_csocko_medium_tcp6(doc) ->
    [];
ttest_ssocko_csocko_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csocko_medium_tcp6,
              Runtime,
              inet6,
              sock, once,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_csocko_large_tcp4(suite) ->
    [];
ttest_ssocko_csocko_large_tcp4(doc) ->
    [];
ttest_ssocko_csocko_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csocko_large_tcp4,
              Runtime,
              inet,
              sock, once,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_csocko_large_tcp6(suite) ->
    [];
ttest_ssocko_csocko_large_tcp6(doc) ->
    [];
ttest_ssocko_csocko_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csocko_large_tcp6,
              Runtime,
              inet6,
              sock, once,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_csockt_small_tcp4(suite) ->
    [];
ttest_ssocko_csockt_small_tcp4(doc) ->
    [];
ttest_ssocko_csockt_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csockt_small_tcp4,
              Runtime,
              inet,
              sock, once,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_csockt_small_tcp6(suite) ->
    [];
ttest_ssocko_csockt_small_tcp6(doc) ->
    [];
ttest_ssocko_csockt_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csocko_small_tcp6,
              Runtime,
              inet6,
              sock, once,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_csockt_medium_tcp4(suite) ->
    [];
ttest_ssocko_csockt_medium_tcp4(doc) ->
    [];
ttest_ssocko_csockt_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csockt_medium_tcp4,
              Runtime,
              inet,
              sock, once,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_csockt_medium_tcp6(suite) ->
    [];
ttest_ssocko_csockt_medium_tcp6(doc) ->
    [];
ttest_ssocko_csockt_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csockt_medium_tcp6,
              Runtime,
              inet6,
              sock, once,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_csockt_large_tcp4(suite) ->
    [];
ttest_ssocko_csockt_large_tcp4(doc) ->
    [];
ttest_ssocko_csockt_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csockt_large_tcp4,
              Runtime,
              inet,
              sock, once,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_csockt_large_tcp6(suite) ->
    [];
ttest_ssocko_csockt_large_tcp6(doc) ->
    [];
ttest_ssocko_csockt_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssocko_csockt_large_tcp6,
              Runtime,
              inet6,
              sock, once,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_cgenf_small_tcp4(suite) ->
    [];
ttest_ssockt_cgenf_small_tcp4(doc) ->
    [];
ttest_ssockt_cgenf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgenf_small_tcp4,
              Runtime,
              inet,
              sock, true,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_cgenf_small_tcp6(suite) ->
    [];
ttest_ssockt_cgenf_small_tcp6(doc) ->
    [];
ttest_ssockt_cgenf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgenf_small_tcp6,
              Runtime,
              inet6,
              sock, true,
              gen, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_cgenf_medium_tcp4(suite) ->
    [];
ttest_ssockt_cgenf_medium_tcp4(doc) ->
    [];
ttest_ssockt_cgenf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgenf_medium_tcp4,
              Runtime,
              inet,
              sock, true,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_cgenf_medium_tcp6(suite) ->
    [];
ttest_ssockt_cgenf_medium_tcp6(doc) ->
    [];
ttest_ssockt_cgenf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgenf_medium_tcp6,
              Runtime,
              inet6,
              sock, true,
              gen, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_cgenf_large_tcp4(suite) ->
    [];
ttest_ssockt_cgenf_large_tcp4(doc) ->
    [];
ttest_ssockt_cgenf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgenf_large_tcp4,
              Runtime,
              inet,
              sock, true,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_cgenf_large_tcp6(suite) ->
    [];
ttest_ssockt_cgenf_large_tcp6(doc) ->
    [];
ttest_ssockt_cgenf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgenf_large_tcp6,
              Runtime,
              inet6,
              sock, true,
              gen, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_cgeno_small_tcp4(suite) ->
    [];
ttest_ssockt_cgeno_small_tcp4(doc) ->
    [];
ttest_ssockt_cgeno_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgeno_small_tcp4,
              Runtime,
              inet,
              sock, true,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_cgeno_small_tcp6(suite) ->
    [];
ttest_ssockt_cgeno_small_tcp6(doc) ->
    [];
ttest_ssockt_cgeno_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgeno_small_tcp6,
              Runtime,
              inet6,
              sock, true,
              gen, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_cgeno_medium_tcp4(suite) ->
    [];
ttest_ssockt_cgeno_medium_tcp4(doc) ->
    [];
ttest_ssockt_cgeno_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgeno_medium_tcp4,
              Runtime,
              inet,
              sock, true,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_cgeno_medium_tcp6(suite) ->
    [];
ttest_ssockt_cgeno_medium_tcp6(doc) ->
    [];
ttest_ssockt_cgeno_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgeno_medium_tcp6,
              Runtime,
              inet6,
              sock, true,
              gen, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_cgeno_large_tcp4(suite) ->
    [];
ttest_ssockt_cgeno_large_tcp4(doc) ->
    [];
ttest_ssockt_cgeno_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgeno_large_tcp4,
              Runtime,
              inet,
              sock, true,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_cgeno_large_tcp6(suite) ->
    [];
ttest_ssockt_cgeno_large_tcp6(doc) ->
    [];
ttest_ssockt_cgeno_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgeno_large_tcp6,
              Runtime,
              inet6,
              sock, true,
              gen, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_cgent_small_tcp4(suite) ->
    [];
ttest_ssockt_cgent_small_tcp4(doc) ->
    [];
ttest_ssockt_cgent_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgent_small_tcp4,
              Runtime,
              inet,
              sock, true,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_cgent_small_tcp6(suite) ->
    [];
ttest_ssockt_cgent_small_tcp6(doc) ->
    [];
ttest_ssockt_cgent_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgent_small_tcp6,
              Runtime,
              inet6,
              sock, true,
              gen, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_cgent_medium_tcp4(suite) ->
    [];
ttest_ssockt_cgent_medium_tcp4(doc) ->
    [];
ttest_ssockt_cgent_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgent_medium_tcp4,
              Runtime,
              inet,
              sock, true,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_cgent_medium_tcp6(suite) ->
    [];
ttest_ssockt_cgent_medium_tcp6(doc) ->
    [];
ttest_ssockt_cgent_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgent_medium_tcp6,
              Runtime,
              inet6,
              sock, true,
              gen, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_cgent_large_tcp4(suite) ->
    [];
ttest_ssockt_cgent_large_tcp4(doc) ->
    [];
ttest_ssockt_cgent_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgent_large_tcp4,
              Runtime,
              inet,
              sock, true,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_cgent_large_tcp6(suite) ->
    [];
ttest_ssockt_cgent_large_tcp6(doc) ->
    [];
ttest_ssockt_cgent_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_cgent_large_tcp6,
              Runtime,
              inet6,
              sock, true,
              gen, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_csockf_small_tcp4(suite) ->
    [];
ttest_ssockt_csockf_small_tcp4(doc) ->
    [];
ttest_ssockt_csockf_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csockf_small_tcp4,
              Runtime,
              inet,
              sock, true,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_csockf_small_tcp6(suite) ->
    [];
ttest_ssockt_csockf_small_tcp6(doc) ->
    [];
ttest_ssockt_csockf_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csockf_small_tcp6,
              Runtime,
              inet6,
              sock, true,
              sock, false,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_csockf_medium_tcp4(suite) ->
    [];
ttest_ssockt_csockf_medium_tcp4(doc) ->
    [];
ttest_ssockt_csockf_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csockf_medium_tcp4,
              Runtime,
              inet,
              sock, true,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_csockf_medium_tcp6(suite) ->
    [];
ttest_ssockt_csockf_medium_tcp6(doc) ->
    [];
ttest_ssockt_csockf_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csockf_medium_tcp6,
              Runtime,
              inet6,
              sock, true,
              sock, false,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_csockf_large_tcp4(suite) ->
    [];
ttest_ssockt_csockf_large_tcp4(doc) ->
    [];
ttest_ssockt_csockf_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csockf_large_tcp4,
              Runtime,
              inet,
              sock, true,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_csockf_large_tcp6(suite) ->
    [];
ttest_ssockt_csockf_large_tcp6(doc) ->
    [];
ttest_ssockt_csockf_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csockf_large_tcp6,
              Runtime,
              inet6,
              sock, true,
              sock, false,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_csocko_small_tcp4(suite) ->
    [];
ttest_ssockt_csocko_small_tcp4(doc) ->
    [];
ttest_ssockt_csocko_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csocko_small_tcp4,
              Runtime,
              inet,
              sock, true,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_csocko_small_tcp6(suite) ->
    [];
ttest_ssockt_csocko_small_tcp6(doc) ->
    [];
ttest_ssockt_csocko_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csocko_small_tcp6,
              Runtime,
              inet6,
              sock, true,
              sock, once,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_csocko_medium_tcp4(suite) ->
    [];
ttest_ssockt_csocko_medium_tcp4(doc) ->
    [];
ttest_ssockt_csocko_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csocko_medium_tcp4,
              Runtime,
              inet,
              sock, true,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_csocko_medium_tcp6(suite) ->
    [];
ttest_ssockt_csocko_medium_tcp6(doc) ->
    [];
ttest_ssockt_csocko_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csocko_medium_tcp6,
              Runtime,
              inet6,
              sock, true,
              sock, once,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_csocko_large_tcp4(suite) ->
    [];
ttest_ssockt_csocko_large_tcp4(doc) ->
    [];
ttest_ssockt_csocko_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csocko_large_tcp4,
              Runtime,
              inet,
              sock, true,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_csocko_large_tcp6(suite) ->
    [];
ttest_ssockt_csocko_large_tcp6(doc) ->
    [];
ttest_ssockt_csocko_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csocko_large_tcp6,
              Runtime,
              inet6,
              sock, true,
              sock, once,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_csockt_small_tcp4(suite) ->
    [];
ttest_ssockt_csockt_small_tcp4(doc) ->
    [];
ttest_ssockt_csockt_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csockt_small_tcp4,
              Runtime,
              inet,
              sock, true,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_csockt_small_tcp6(suite) ->
    [];
ttest_ssockt_csockt_small_tcp6(doc) ->
    [];
ttest_ssockt_csockt_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csocko_small_tcp6,
              Runtime,
              inet6,
              sock, true,
              sock, true,
              1, 200).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_csockt_medium_tcp4(suite) ->
    [];
ttest_ssockt_csockt_medium_tcp4(doc) ->
    [];
ttest_ssockt_csockt_medium_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csockt_medium_tcp4,
              Runtime,
              inet,
              sock, true,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_csockt_medium_tcp6(suite) ->
    [];
ttest_ssockt_csockt_medium_tcp6(doc) ->
    [];
ttest_ssockt_csockt_medium_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csockt_medium_tcp6,
              Runtime,
              inet6,
              sock, true,
              sock, true,
              2, 20).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_csockt_large_tcp4(suite) ->
    [];
ttest_ssockt_csockt_large_tcp4(doc) ->
    [];
ttest_ssockt_csockt_large_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csockt_large_tcp4,
              Runtime,
              inet,
              sock, true,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_csockt_large_tcp6(suite) ->
    [];
ttest_ssockt_csockt_large_tcp6(doc) ->
    [];
ttest_ssockt_csockt_large_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(ttest_ssockt_csockt_large_tcp6,
              Runtime,
              inet6,
              sock, true,
              sock, true,
              3, 2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

which_ttest_runtime(Config) when is_list(Config) ->
    case lists:keysearch(esock_test_ttest_runtime, 1, Config) of
        {value, {esock_test_ttest_runtime, Runtime}} ->
            Runtime;
        false ->
            which_ttest_runtime_env()
    end.

which_ttest_runtime_env() ->
    which_ttest_runtime_env(os:getenv("ESOCK_TEST_TTEST_RUNTIME")).

which_ttest_runtime_env(TStr) when is_list(TStr) ->
    which_ttest_runtime_env2(lists:reverse(TStr));
which_ttest_runtime_env(false) ->
    ?TTEST_RUNTIME.


%% The format is: <int>[unit]
%% where the optional unit can be:
%% ms: milliseconds
%% s:  seconds (default)
%% m:  minutes
which_ttest_runtime_env2([$m, $s | MS]) when (length(MS) > 0) ->
    convert_time(MS, fun(X) -> X end);
which_ttest_runtime_env2([$m | M]) when (length(M) > 0) ->
    convert_time(M, fun(X) -> ?MINS(X) end);
which_ttest_runtime_env2([$s | S]) when (length(S) > 0) ->
    convert_time(S, fun(X) -> ?SECS(X) end);
which_ttest_runtime_env2(S) ->
    convert_time(S, fun(X) -> ?SECS(X) end).

convert_time(TStrRev, Convert) ->
    try list_to_integer(lists:reverse(TStrRev)) of
        I -> Convert(I)
    catch
        _:_ ->
            ?TTEST_RUNTIME
    end.

%% ttest_tcp(TC,
%%           Domain,
%%           ServerMod, ServerActive,
%%           ClientMod, ClientActive,
%%           MsgID, MaxOutstanding) ->
%%     ttest_tcp(TC,
%%               ?TTEST_RUNTIME,
%%               Domain,
%%               ServerMod, ServerActive,
%%               ClientMod, ClientActive,
%%               MsgID, MaxOutstanding).
ttest_tcp(TC,
          Runtime,
          Domain,
          ServerMod, ServerActive,
          ClientMod, ClientActive,
          MsgID, MaxOutstanding) ->
    tc_try(TC,
           fun() ->
                   if
                       (Domain =/= inet) -> has_support_ipv6(); 
                       true -> ok 
                   end
           end,
           fun() ->
                   %% This may be overkill, depending on the runtime,
                   %% but better safe then sorry...
                   ?TT(Runtime + ?SECS(60)),
                   InitState = #{domain          => Domain,
                                 msg_id          => MsgID,
                                 max_outstanding => MaxOutstanding,
                                 runtime         => Runtime,
                                 server_mod      => ServerMod,
                                 server_active   => ServerActive,
                                 client_mod      => ClientMod,
                                 client_active   => ClientActive},
                   ok = ttest_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ttest_tcp(InitState) ->
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
         #{desc => "create node",
           cmd  => fun(#{host := Host} = State) ->
                           case start_node(Host, server) of
                               {ok, Node} ->
                                   {ok, State#{node => Node}};
                               {error, Reason} ->
                                   {skip, Reason}
                           end
                   end},
         #{desc => "monitor server node",
           cmd  => fun(#{node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "start ttest (remote) server",
           cmd  => fun(#{mod    := Mod,
                         active := Active,
                         node   := Node} = State) ->
                           case ttest_tcp_server_start(Node, Mod, Active) of
                               {ok, {{Pid, _MRef}, {Addr, Port}}} ->
                                   {ok, State#{rserver => Pid,
                                               addr    => Addr,
                                               port    => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester,
                         addr   := Addr,
                         port   := Port}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, {Addr, Port}),
                           ok
                   end},


         %% *** Termination ***
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester  := Tester, 
                         rserver := RServer} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester,
                                                     [{rserver, RServer}]) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         %% The remote server is in a accept, with a timeout of 5 seconds,
         %% so may have to wait a bit...
         #{desc => "order (remote) ttest server terminate",
           cmd  => fun(#{node    := _Node,
                         rserver := RServer}) ->
                           ttest_tcp_server_stop(RServer),
                           ok
                   end},
         #{desc => "await ttest (remote) server termination",
           cmd  => fun(#{rserver := RServer} = State) ->
                           ?SEV_AWAIT_TERMINATION(RServer),
                           State1 = maps:remove(rserver, State),
                           {ok, State1}
                   end},
         #{desc => "stop (server) node",
           cmd  => fun(#{node := Node} = _State) ->
                           stop_node(Node)
                   end},
         #{desc => "await (server) node termination",
           cmd  => fun(#{node := Node} = State) ->
                           receive
                               {nodedown, Node} ->
                                   {ok, maps:remove(node, State)}
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, {ServerAddr, ServerPort}} = ?SEV_AWAIT_START(),
                           {ok, State#{tester      => Tester,
                                       server_addr => ServerAddr,
                                       server_port => ServerPort}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},


         %% *** Init part ***
         #{desc => "create node",
           cmd  => fun(#{host := Host} = State) ->
                           case start_node(Host, client) of
                               {ok, Node} ->
                                   {ok, State#{node => Node}};
                               {error, Reason} ->
                                   {skip, Reason}
                           end
                   end},
         #{desc => "monitor client node",
           cmd  => fun(#{node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},


         %% The actual test
         #{desc => "await continue (ttest)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, ttest),
                           ok
                   end},
         #{desc => "start ttest (remote) client",
           cmd  => fun(#{node            := Node,
                         mod             := Mod,
                         active          := Active,
                         msg_id          := MsgID,
                         max_outstanding := MaxOutstanding,
                         runtime         := RunTime,
                         server_addr     := Addr,
                         server_port     := Port} = State) ->
                           Self   = self(),
                           Notify =
                               fun(Result) ->
                                       ?SEV_ANNOUNCE_READY(Self, ttest, Result)
                               end,                           
                           case ttest_tcp_client_start(Node, Notify,
                                                       Mod, Active,
                                                       Addr, Port,
                                                       MsgID, MaxOutstanding,
                                                       RunTime) of
                               {ok, {Pid, _MRef}} ->
                                   {ok, State#{rclient => Pid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await ttest ready",
           cmd  => fun(#{tester  := Tester,
                         rclient := RClient} = State) ->
                           %% TTestResult = ?SEV_AWAIT_READY(RClient, rclient, ttest, 
                           %%                                [{tester, Tester}]),
                           case ?SEV_AWAIT_READY(RClient, rclient, ttest, 
                                                 [{tester, Tester}]) of
                             {ok, Result} ->
                                 {ok, State#{result => Result}};
                             {error, _} = ERROR ->
                                 ERROR
                         end
                   end},
         #{desc => "await ttest (remote) client termination",
           cmd  => fun(#{rclient := RClient} = State) ->
                           ?SEV_AWAIT_TERMINATION(RClient),
                           State1 = maps:remove(rclient, State),
                           {ok, State1}
                   end},
         #{desc => "announce ready (ttest)",
           cmd  => fun(#{tester := Tester,
                         result := Result} = State) ->
                           ?SEV_ANNOUNCE_READY(Tester, ttest, Result),
                           {ok, maps:remove(result, State)}
                   end},


         %% *** Termination ***
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "stop (client) node",
           cmd  => fun(#{node := Node} = _State) ->
                           stop_node(Node)
                   end},
         #{desc => "await (client) node termination",
           cmd  => fun(#{node := Node} = State) ->
                           receive
                               {nodedown, Node} ->
                                   {ok, maps:remove(node, State)}
                           end
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
                           {ok, {Addr, Port}} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{server_addr => Addr,
                                       server_port => Port}}
                   end},


         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client      := Pid,
                         server_addr := Addr,
                         server_port := Port} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, {Addr, Port}),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Client} = _State) ->
                           ok = ?SEV_AWAIT_READY(Client, client, init)
                   end},
 
         %% The actual test
         #{desc => "order client continue (ttest)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, ttest),
                           ok
                   end},
         #{desc => "await client ready (ttest)",
           cmd  => fun(#{server := Server,
                         client := Client} = State) ->
                           case ?SEV_AWAIT_READY(Client, client, ttest,
                                                 [{server, Server}]) of
                               {ok, Result} ->
                                   {ok, State#{result => Result}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** Terminate server ***
         #{desc => "order client terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client down",
           cmd  => fun(#{client := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           State1 = maps:remove(client,    State),
                           {ok, State1}
                   end},
         #{desc => "order server terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server down",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_TERMINATION(Server),
                           ok
                   end},

         
         %% Present the results
         #{desc => "present the results",
           cmd  => fun(#{result := Result} = State) ->
                           case Result of
                               #{status  := ok,
                                 runtime := RunTime,
                                 cnt     := Cnt,
                                 bcnt    := BCnt} ->
                                   ?SEV_IPRINT(
                                      "TTest results: "
                                      "~n   Run Time:                    ~s"
                                      "~n   Byte Count:                  ~s"
                                      "~n   Number of message exchanges: ~s"
                                      "~n~n",
                                      [
                                       ?TTEST_LIB:format_time(RunTime),
                                       if ((BCnt =:= 0) orelse (RunTime =:= 0)) ->
                                               ?TTEST_LIB:format("~w, ~w",
                                                                 [BCnt, RunTime]);
                                          true ->
                                               ?TTEST_LIB:format("~p => ~p byte / ms",
                                                                 [BCnt, BCnt div RunTime])
                                       end,
                                       if (RunTime =:= 0) ->
                                               "-";
                                          true ->
                                               ?TTEST_LIB:format("~p => ~p iterations / ms",
                                                                 [Cnt, Cnt div RunTime])
                                       end
                                      ]),
                                   {ok, maps:remove(result, State)};

                               #{status  := Failure,
                                 runtime := RunTime,
                                 sid     := SID,
                                 rid     := RID,
                                 scnt    := SCnt,
                                 rcnt    := RCnt,
                                 bcnt    := BCnt,
                                 num     := Num} ->
                                   ?SEV_EPRINT("Time Test failed: "
                                               "~n   ~p"
                                               "~n"
                                               "~nwhen"
                                               "~n"
                                               "~n   Run Time:       ~s"
                                               "~n   Send ID:        ~p"
                                               "~n   Recv ID:        ~p"
                                               "~n   Send Count:     ~p"
                                               "~n   Recv Count:     ~p"
                                               "~n   Byte Count:     ~p"
                                               "~n   Num Iterations: ~p",
                                               [Failure,
                                                ?TTEST_LIB:format_time(RunTime),
                                                SID, RID, SCnt, RCnt, BCnt, Num]),
                                   {error, Failure}
                           end
                   end},

         %% This is just so that the printout above shall have time to come
         %% out before then end of the test case.
         ?SEV_SLEEP(?SECS(1)),

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start server evaluator"),
    ServerInitState = #{host   => local_host(),
                        domain => maps:get(domain,        InitState),
                        mod    => maps:get(server_mod,    InitState),
                        active => maps:get(server_active, InitState)},
    Server          = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator"),
    ClientInitState = #{host            => local_host(),
                        domain          => maps:get(domain,          InitState),
                        mod             => maps:get(client_mod,      InitState),
                        active          => maps:get(client_active,   InitState),
                        msg_id          => maps:get(msg_id,          InitState),
                        max_outstanding => maps:get(max_outstanding, InitState),
                        runtime         => maps:get(runtime,         InitState)},
    Client          = ?SEV_START("client", ClientSeq, ClientInitState),
    
    i("start 'tester' evaluator"),
    TesterInitState = #{server => Server#ev.pid,
                        client => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).



ttest_tcp_server_start(Node, gen, Active) ->
    Transport = socket_test_ttest_tcp_gen,
    socket_test_ttest_tcp_server:start_monitor(Node, Transport, Active);
ttest_tcp_server_start(Node, sock, Active) ->
    TransportMod = socket_test_ttest_tcp_socket,
    Transport    = {TransportMod, #{method => plain}},
    socket_test_ttest_tcp_server:start_monitor(Node, Transport, Active).

ttest_tcp_server_stop(Pid) ->
    socket_test_ttest_tcp_server:stop(Pid).

ttest_tcp_client_start(Node,
                       Notify,
                       gen,
                       Active, Addr, Port, MsgID, MaxOutstanding, RunTime) ->
    Transport = socket_test_ttest_tcp_gen,
    socket_test_ttest_tcp_client:start_monitor(Node,
                                               Notify,
                                               Transport,
                                               Active,
                                               Addr, Port,
                                               MsgID, MaxOutstanding, RunTime);
ttest_tcp_client_start(Node,
                       Notify,
                       sock,
                       Active, Addr, Port, MsgID, MaxOutstanding, RunTime) ->
    TransportMod = socket_test_ttest_tcp_socket,
    Transport    = {TransportMod, #{method => plain}},
    socket_test_ttest_tcp_client:start_monitor(Node,
                                               Notify,
                                               Transport,
                                               Active,
                                               Addr, Port,
                                               MsgID, MaxOutstanding, RunTime).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This mechanism has only one purpose: So that we are able to kill
%% the node-starter process if it takes to long. The node-starter
%% runs on the local node.
%% This crapola is hopefully temporary, but we have seen that on
%% some platforms the ct_slave:start simply hangs.
-define(NODE_START_TIMEOUT, 10000).
start_node(Host, NodeName) ->
    start_node(Host, NodeName, ?NODE_START_TIMEOUT).

start_node(Host, NodeName, Timeout) ->
    {NodeStarter, _} =
        spawn_monitor(fun() -> exit(start_unique_node(Host, NodeName)) end),
    receive
        {'DOWN', _, process, NodeStarter, Result} ->
            %% i("Node Starter (~p) reported: ~p", [NodeStarter, Result]),
            Result
    after Timeout ->
            exit(NodeStarter, kill),
            {error, {failed_starting_node, NodeName, timeout}}
    end.

start_unique_node(Host, NodeName) ->
    UniqueNodeName = f("~w_~w", [NodeName, erlang:system_time(millisecond)]),
    case do_start_node(Host, UniqueNodeName) of
        {ok, _} = OK ->
            global:sync(),
            %% i("Node ~p started: "
            %%    "~n   Nodes:        ~p"
            %%    "~n   Logger:       ~p"
            %%    "~n   Global Names: ~p",
            %%    [NodeName, nodes(),
            %%     global:whereis_name(socket_test_logger),
            %%     global:registered_names()]),
            OK;
        {error, Reason, _} ->
            {error, Reason}
    end.

do_start_node(Host, NodeName) when is_list(NodeName) ->
    do_start_node(Host, list_to_atom(NodeName));
do_start_node(Host, NodeName) when is_atom(NodeName) ->
    Dir   = filename:dirname(code:which(?MODULE)),
    Flags = "-pa " ++ Dir,
    Opts  = [{monitor_master, true}, {erl_flags, Flags}],
    ct_slave:start(Host, NodeName, Opts).


stop_node(Node) ->
    case ct_slave:stop(Node) of
        {ok, _} ->
            ok;
        {error, _} = ERROR ->
            ERROR
    end.

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sock_open(Domain, Type, Proto) ->
    try socket:open(Domain, Type, Proto) of
        {ok, Socket} ->
            Socket;
        {error, Reason} ->
            ?FAIL({open, Reason})
    catch
        C:E:S ->
            ?FAIL({open, C, E, S})
    end.


sock_bind(Sock, SockAddr) ->
    try socket:bind(Sock, SockAddr) of
        {ok, Port} ->
            Port;
        {error, Reason} ->
            i("sock_bind -> error: ~p", [Reason]),
            ?FAIL({bind, Reason})
    catch
        C:E:S ->
            i("sock_bind -> failed: ~p, ~p, ~p", [C, E, S]),
            ?FAIL({bind, C, E, S})
    end.

sock_connect(Sock, SockAddr) ->
    try socket:connect(Sock, SockAddr) of
        ok ->
            ok;
        {error, Reason} ->
            ?FAIL({connect, Reason})
    catch
        C:E:S ->
            ?FAIL({connect, C, E, S})
    end.
    
sock_sockname(Sock) ->
    try socket:sockname(Sock) of
        {ok, SockAddr} ->
            SockAddr;
        {error, Reason} ->
            ?FAIL({sockname, Reason})
    catch
        C:E:S ->
            ?FAIL({sockname, C, E, S})
    end.
    

%% sock_listen(Sock) ->
%%     sock_listen2(fun() -> socket:listen(Sock) end).

%% sock_listen(Sock, BackLog) ->
%%     sock_listen2(fun() -> socket:listen(Sock, BackLog) end).

%% sock_listen2(Listen) ->
%%     try Listen() of
%%         ok ->
%%             ok;
%%         {error, Reason} ->
%%             ?FAIL({listen, Reason})
%%     catch
%%         C:E:S ->
%%             ?FAIL({listen, C, E, S})
%%     end.


%% sock_accept(LSock) ->
%%     try socket:accept(LSock) of
%%         {ok, Sock} ->
%%             Sock;
%%         {error, Reason} ->
%%             i("sock_accept -> error: ~p", [Reason]),
%%             ?FAIL({accept, Reason})
%%     catch
%%         C:E:S ->
%%             i("sock_accept -> failed: ~p, ~p, ~p", [C, E, S]),
%%             ?FAIL({accept, C, E, S})
%%     end.


sock_close(Sock) ->
    try socket:close(Sock) of
        ok ->
            ok;
        {error, Reason} ->
            i("sock_close -> error: ~p", [Reason]),
            ?FAIL({close, Reason})
    catch
        C:E:S ->
            i("sock_close -> failed: ~p, ~p, ~p", [C, E, S]),
            ?FAIL({close, C, E, S})
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


which_local_socket_addr(local = Domain) ->
    #{family => Domain,
      path   => ?LIB:f("/tmp/socket_~w", [erlang:unique_integer()])};

%% This gets the local address (not 127.0...)
%% We should really implement this using the (new) net module,
%% but until that gets the necessary functionality...
which_local_socket_addr(Domain) ->
    case inet:getifaddrs() of
        {ok, IFL} ->
            Addr = which_addr(Domain, IFL),
            #{family => Domain,
              addr   => Addr};
        {error, Reason} ->
            ?FAIL({inet, getifaddrs, Reason})
    end.

which_addr(_Domain, []) ->
    ?FAIL(no_address);
which_addr(Domain, [{"lo" ++ _, _}|IFL]) ->
    which_addr(Domain, IFL);
which_addr(Domain, [{_Name, IFO}|IFL]) ->
    case which_addr2(Domain, IFO) of
        {ok, Addr} ->
            Addr;
        {error, no_address} ->
            which_addr(Domain, IFL)
    end;
which_addr(Domain, [_|IFL]) ->
    which_addr(Domain, IFL).

which_addr2(_Domain, []) ->
    {error, no_address};
which_addr2(inet = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 4) ->
    {ok, Addr};
which_addr2(inet6 = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 8) ->
    {ok, Addr};
which_addr2(Domain, [_|IFO]) ->
    which_addr2(Domain, IFO).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Here are all the *general* test vase condition functions.

supports_unix_domain_socket() ->
    case os:type() of
        {win32, _} ->
            {skip, "Not supported"};
        _ ->
            ok
    end.


%% The idea is that this function shall test if the test host has 
%% support for IPv6. If not, there is no point in running IPv6 tests.
%% Currently we just skip.
has_support_ipv6() ->
    %% case socket:supports(ipv6) of
    %%     true ->
    %%         ok;
    %%     false ->
    %%         {error, not_supported}
    %% end.
    not_yet_implemented().



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_yet_implemented() ->
    skip("not yet implemented").

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t() ->
    ts(ms).

ts(ms) ->
    erlang:monotonic_time(milli_seconds).

tdiff({A1, B1, C1} = _T1x, {A2, B2, C2} = _T2x) ->
    T1 = A1*1000000000+B1*1000+(C1 div 1000), 
    T2 = A2*1000000000+B2*1000+(C2 div 1000), 
    T2 - T1.


formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, _N3} = TS) ->
    {_Date, Time}   = calendar:now_to_local_time(TS),
    %% {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    %% FormatTS = 
    %%     io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~w",
    %%                   [YYYY, MM, DD, Hour, Min, Sec, N3]),  
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w", [Hour, Min, Sec]),  
    lists:flatten(FormatTS).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_tc_name(N) when is_atom(N) ->
    set_tc_name(atom_to_list(N));
set_tc_name(N) when is_list(N) ->
    put(tc_name, N).

%% get_tc_name() ->
%%     get(tc_name).

tc_begin(TC) ->
    OldVal = process_flag(trap_exit, true),
    put(old_trap_exit, OldVal),
    set_tc_name(TC),
    tc_print("begin ***",
             "~n----------------------------------------------------~n", "").
    
tc_end(Result) when is_list(Result) ->
    OldVal = erase(old_trap_exit),
    process_flag(trap_exit, OldVal),
    tc_print("done: ~s", [Result], 
             "", "----------------------------------------------------~n~n"),
    ok.

%% *** tc_try/2,3 ***
%% Case:      Basically the test case name
%% TCCondFun: A fun that is evaluated before the actual test case
%%            The point of this is that it can performs checks to
%%            see if we shall run the test case at all.
%%            For instance, the test case may only work in specific
%%            conditions.
%% FCFun:     The test case fun
tc_try(Case, TCFun) ->
    TCCondFun = fun() -> ok end,
    tc_try(Case, TCCondFun, TCFun).

tc_try(Case, TCCondFun, TCFun) 
  when is_atom(Case) andalso
       is_function(TCCondFun, 0) andalso
       is_function(TCFun, 0) ->
    tc_begin(Case),
    try TCCondFun() of
        ok ->
            try 
                begin
                    TCFun(),
                    ?SLEEP(?SECS(1)),
                    tc_end("ok")
                end
            catch
                C:{skip, _} = SKIP when ((C =:= throw) orelse (C =:= exit)) ->
                    %% i("catched[tc] (skip): "
                    %%   "~n   C:    ~p"
                    %%   "~n   SKIP: ~p"
                    %%   "~n", [C, SKIP]),
                    tc_end( f("skipping(catched,~w,tc)", [C]) ),
                    SKIP;
                C:E:S ->
                    %% i("catched[tc]: "
                    %%   "~n   C: ~p"
                    %%   "~n   E: ~p"
                    %%   "~n   S: ~p"
                    %%    "~n", [C, E, S]),
                    tc_end( f("failed(catched,~w,tc)", [C]) ),
                    erlang:raise(C, E, S)
            end;
        {skip, _} = SKIP ->
            tc_end("skipping(tc)"),
            SKIP;
        {error, Reason} ->
            tc_end("failed(tc)"),
            exit({tc_cond_failed, Reason})
    catch
        C:{skip, _} = SKIP when ((C =:= throw) orelse (C =:= exit)) ->
            %% i("catched[cond] (skip): "
            %%   "~n   C:    ~p"
            %%   "~n   SKIP: ~p"
            %%   "~n", [C, SKIP]),
            tc_end( f("skipping(catched,~w,cond)", [C]) ),
            SKIP;
        C:E:S ->
            %% i("catched[cond]: "
            %%   "~n   C: ~p"
            %%   "~n   E: ~p"
            %%   "~n   S: ~p"
            %%   "~n", [C, E, S]),
            tc_end( f("failed(catched,~w,cond)", [C]) ),
            erlang:raise(C, E, S)
    end.


tc_print(F, Before, After) ->
    tc_print(F, [], Before, After).

tc_print(F, A, Before, After) ->
    Name = tc_which_name(),
    FStr = f("*** [~s][~s][~p] " ++ F ++ "~n", 
             [formated_timestamp(),Name,self()|A]),
    io:format(user, Before ++ FStr ++ After, []).

tc_which_name() ->
    case get(tc_name) of
        undefined ->
            case get(sname) of
                undefined ->
                    "";
                SName when is_list(SName) ->
                    SName
            end;
        Name when is_list(Name) ->
            Name
    end.
    
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

l2a(S) when is_list(S) ->
    list_to_atom(S).

l2b(L) when is_list(L) ->
    list_to_binary(L).

b2l(B) when is_binary(B) ->
    binary_to_list(B).

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

%% p(F) ->
%%     p(F, []).

%% p(F, A) ->
%%     p(F, A, "", "").

%% p(F, A, Before, After) when is_list(Before) andalso is_list(After) ->
%%     TcName = 
%%         case get(tc_name) of
%%             undefined ->
%%                 case get(sname) of
%%                     undefined ->
%%                         "";
%%                     SName when is_list(SName) ->
%%                         SName
%%                 end;
%%             Name when is_list(Name) ->
%%                 Name
%%         end,
%%     FStr = f("*** [~s][~s][~p] " ++ F ++ "~n", 
%%              [formated_timestamp(),TcName,self()|A]),
%%     i(Before ++ FStr ++ After, []).


%% d(F, A) ->
%%     d(get(dbg_fd), F, A).

%% d(undefined, F, A) ->
%%     [NodeNameStr|_] = string:split(atom_to_list(node()), [$@]),
%%     DbgFileName = f("~s-dbg.txt", [NodeNameStr]),
%%     case file:open(DbgFileName, [write]) of
%%         {ok, FD} ->
%%             put(dbg_fd, FD),
%%             d(FD, F, A);
%%         {error, Reason} ->
%%             exit({failed_open_dbg_file, Reason})
%%     end;
%% d(FD, F, A) ->
%%     io:format(FD, "~s~n", [f("[~s] " ++ F, [formated_timestamp()|A])]).

i(F) ->
    i(F, []).

i(F, A) ->
    FStr = f("[~s] " ++ F, [formated_timestamp()|A]),
    io:format(user, FStr ++ "~n", []),
    io:format(FStr, []).

