%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2023. All Rights Reserved.
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
-module(tls_server_session_ticket_SUITE).
-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssl/src/ssl_alert.hrl").
-include_lib("ssl/src/ssl_cipher.hrl").
-include_lib("ssl/src/ssl_internal.hrl").
-include_lib("ssl/src/tls_handshake_1_3.hrl").
-include("ssl_record.hrl").

%% Callback functions
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).
%% Testcases
-export([expired_ticket_test/0,
         expired_ticket_test/1,
         invalid_ticket_test/0,
         invalid_ticket_test/1,
         main_test/0,
         main_test/1,
         misc_test/0,
         misc_test/1,
         valid_ticket_older_than_windowsize_test/0,
         valid_ticket_older_than_windowsize_test/1,
         certificate_encoding_test/0,
         certificate_encoding_test/1]).

-define(LIFETIME, 3). % tickets expire after 3s
-define(TICKET_STORE_SIZE, 1).
-define(MASTER_SECRET, "master_secret").
-define(PRF, sha).
-define(VERSION, ?TLS_1_3).
-define(PSK, <<15,168,18,43,216,33,227,142,114,190,70,183,137,57,64,64,66,152,115,94>>).
-define(WINDOW_SIZE, 1).
-define(SEED, <<1,2,3,4,5>>).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [{group, stateful},
     {group, stateful_with_cert},
     {group, stateless},
     {group, stateless_with_cert},
     {group, stateless_antireplay}
    ].

groups() ->
    [{stateful, [], [main_test, expired_ticket_test, invalid_ticket_test]},
     {stateful_with_cert, [], [main_test, expired_ticket_test, invalid_ticket_test]},
     {stateless, [], [expired_ticket_test, invalid_ticket_test, main_test, certificate_encoding_test]},
     {stateless_with_cert, [], [expired_ticket_test, invalid_ticket_test, main_test, certificate_encoding_test]},
     {stateless_antireplay, [], [main_test, misc_test, valid_ticket_older_than_windowsize_test, certificate_encoding_test]}
    ].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            ssl_test_lib:clean_start(),
            Config0
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(stateless_antireplay, Config) ->
    check_environment([{server_session_tickets, stateless},
                       {anti_replay, {?WINDOW_SIZE, 20, 30}}]
                      ++ Config);
init_per_group(Group, Config)
    when Group == stateless orelse Group == stateless_with_cert ->
    check_environment([{server_session_tickets, Group} | Config]);
init_per_group(Group, Config)
    when Group == stateful orelse Group == stateful_with_cert ->
    [{server_session_tickets, Group} | Config].

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestCase, Config)  ->
    {ok, ListenSocket} = gen_tcp:listen(0, [{active, false}]),
    AntiReplay = ?config(anti_replay, Config),
    {ok, Pid} = tls_server_session_ticket:start_link(
                  ListenSocket, ?config(server_session_tickets, Config),
                  ?LIFETIME, ?TICKET_STORE_SIZE, _MaxEarlyDataSize = 100,
                  AntiReplay, ?SEED),
    % For all anti-replay test-cases we will sleep longer than the warmup period
    case AntiReplay of
        undefined -> undefined;
        _ ->
            ct:sleep({seconds, 2 * ?LIFETIME})
    end,
    [{server_pid, Pid}, {listen_socket, ListenSocket} | Config].

end_per_testcase(_TestCase, Config) ->
    Pid = ?config(server_pid, Config),
    exit(Pid, normal),
    ListenSocket = ?config(listen_socket, Config),
    ok = gen_tcp:close(ListenSocket),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
main_test() ->
    [{doc, "Ticket main scenario"}].
main_test(Config) when is_list(Config) ->
    Pid = ?config(server_pid, Config),
    % Fill in GB tree store for stateful setup
    tls_server_session_ticket:new(Pid, ?PRF, ?MASTER_SECRET, undefined),
    % Reach ticket store size limit - force GB tree pruning
    SessionTicket = #new_session_ticket{} =
        tls_server_session_ticket:new(Pid, ?PRF, ?MASTER_SECRET, undefined),
    TicketRecvTime = erlang:system_time(millisecond),
    %% Sleep more than the ticket lifetime (which is in seconds) in
    %% milliseconds, to confirm that the client reported age (which is in
    %% milliseconds) is compared correctly with the lifetime
    ct:sleep(5 * ?LIFETIME),
    {HandshakeHist, OferredPsks} = get_handshake_hist(SessionTicket, TicketRecvTime, ?PSK),
    AcceptResponse = {ok, {0, ?PSK, undefined}},
    AcceptResponse = tls_server_session_ticket:use(Pid, OferredPsks, ?PRF,
                                      [iolist_to_binary(HandshakeHist)]),
    % check replay attempt result
    ExpReplyResult = get_replay_expected_result(Config, AcceptResponse),
    ExpReplyResult = tls_server_session_ticket:use(Pid, OferredPsks, ?PRF,
                                      [iolist_to_binary(HandshakeHist)]),
    true = is_process_alive(Pid).

invalid_ticket_test() ->
    [{doc, "Verify invalid tickets handling"}].
invalid_ticket_test(Config) when is_list(Config) ->
    Pid = ?config(server_pid, Config),
    #new_session_ticket{ticket=Ticket} =
        tls_server_session_ticket:new(Pid, ?PRF, ?MASTER_SECRET, undefined),
    Ids = [#psk_identity{identity = <<"wrongidentity">>,
                         obfuscated_ticket_age = 0},
           #psk_identity{identity = Ticket,
                         obfuscated_ticket_age = 0}],
    OfferedPSKs = #offered_psks{identities = Ids,
                                binders = [<<"wrongbinder">>, <<"wrongbinder">>]},
    HandshakeHist = tls_handshake:encode_handshake(
                      get_client_hello(OfferedPSKs), ?VERSION),
    ExpectedReason = get_alert_reason(Config),
    {error, #alert{level = ?FATAL,
                   description = ?ILLEGAL_PARAMETER,
                   role = undefined,
                   reason = ExpectedReason}} =
        tls_server_session_ticket:use(Pid, OfferedPSKs, ?PRF,
                                      [iolist_to_binary(HandshakeHist)]),
    true = is_process_alive(Pid).

expired_ticket_test() ->
    [{doc, "Expired ticket scenario"}].
expired_ticket_test(Config) when is_list(Config) ->
    Pid = ?config(server_pid, Config),
    SessionTicket = tls_server_session_ticket:new(Pid, ?PRF, ?MASTER_SECRET, undefined),
    TicketRecvTime = erlang:system_time(millisecond),
    ct:sleep({seconds, 2 * ?LIFETIME}),
    {HandshakeHist, OFPSKs} = get_handshake_hist(SessionTicket, TicketRecvTime, ?PSK),
    {ok, undefined} = tls_server_session_ticket:use(Pid, OFPSKs, ?PRF,
                                      [iolist_to_binary(HandshakeHist)]),
    true = is_process_alive(Pid).

valid_ticket_older_than_windowsize_test() ->
    [{doc, "Verify valid ticket handling of tickets older than WindowSize"}].

valid_ticket_older_than_windowsize_test(Config) when is_list(Config) ->
    Pid = ?config(server_pid, Config),
    % Fill in GB tree store for stateful setup (Stateless tests also fail without this)
    tls_server_session_ticket:new(Pid, ?PRF, ?MASTER_SECRET, undefined),
    % Reach ticket store size limit - force GB tree pruning
    SessionTicket = #new_session_ticket{} =
        tls_server_session_ticket:new(Pid, ?PRF, ?MASTER_SECRET, undefined),
    TicketRecvTime = erlang:system_time(millisecond),
    %% Sleep more than the window length (which is in seconds)
    ct:sleep({seconds, 2 * ?WINDOW_SIZE}),
    {HandshakeHist, OferredPsks} = get_handshake_hist(SessionTicket, TicketRecvTime, ?PSK),
    AcceptResponse = {ok, {0, ?PSK, undefined}},
    AcceptResponse = tls_server_session_ticket:use(Pid, OferredPsks, ?PRF,
                                      [iolist_to_binary(HandshakeHist)]),
    % check replay attempt result
    ExpReplyResult = get_replay_expected_result(Config, AcceptResponse),
    ExpReplyResult = tls_server_session_ticket:use(Pid, OferredPsks, ?PRF,
                                      [iolist_to_binary(HandshakeHist)]),
    true = is_process_alive(Pid).

misc_test() ->
    [{doc, "Miscellaneous functionality"}].
misc_test(Config) when is_list(Config) ->
    Pid = ?config(server_pid, Config),
    ok = gen_server:cast(Pid, some_request),
    Pid ! rotate_bloom_filters,
    Pid ! general_handle_info,
    {ok, state} = tls_server_session_ticket:code_change(old_version, state, extra),
    Pid = tls_server_session_ticket:format_status(not_relevant, Pid),
    true = is_process_alive(Pid).

certificate_encoding_test() ->
    [{doc, "Verify certifcate encoding/decoding in ticket"}].

certificate_encoding_test(Config) when is_list(Config) ->
    Pid = ?config(server_pid, Config),
    tls_server_session_ticket:new(Pid, ?PRF, ?MASTER_SECRET, undefined),
    Certificate = crypto:strong_rand_bytes(100),
    SessionTicket = #new_session_ticket{} =
        tls_server_session_ticket:new(Pid, ?PRF, ?MASTER_SECRET, Certificate),
    TicketRecvTime = erlang:system_time(millisecond),
    {HandshakeHist, OferredPsks} = get_handshake_hist(SessionTicket, TicketRecvTime, ?PSK),
    AcceptResponse = {ok, {0, ?PSK, Certificate}},
    AcceptResponse = tls_server_session_ticket:use(Pid, OferredPsks, ?PRF,
                                      [iolist_to_binary(HandshakeHist)]),
    true = is_process_alive(Pid).

%%--------------------------------------------------------------------
%% Helpers -----------------------------------------------------------
%%--------------------------------------------------------------------
get_handshake_hist(#new_session_ticket{} = T, TicketRecvTime, PSK0) ->
    M = #{cipher_suite => {nothing, ?PRF},
          sni => nothing,
          psk => PSK0,
          timestamp => TicketRecvTime,
          ticket => T},
    TicketData = tls_handshake_1_3:get_ticket_data(self(), manual, [M]),
    [#ticket_data{identity = Identity}] = TicketData,
    SomeBinder = <<159, 187, 86, 6, 55, 20, 149, 208, 3, 221, 78, 126, 254, 101,
                   123, 251, 151, 189, 17, 53>>,
    OfferedPSKs0 = #offered_psks{identities = [Identity], binders = [SomeBinder]},
    Hello0 = get_client_hello(OfferedPSKs0),
    Hello1 = tls_handshake_1_3:maybe_add_binders(Hello0, TicketData, ?VERSION),
    PSK1 =  maps:get(pre_shared_key, Hello1#client_hello.extensions),
    OfferedPSKs1 = PSK1#pre_shared_key_client_hello.offered_psks,
    {tls_handshake:encode_handshake(Hello1, ?VERSION), OfferedPSKs1}.

get_client_hello(OfferedPSKs) ->
    PreSharedKey = #pre_shared_key_client_hello{offered_psks = OfferedPSKs},
    Ext0 = ssl_handshake:empty_extensions(?VERSION, client_hello),
    #client_hello{
       client_version = ?VERSION,
       random = <<1:256>>,
       session_id = <<>>,
       cipher_suites = [?TLS_AES_256_GCM_SHA384],
       extensions = Ext0#{pre_shared_key => PreSharedKey}}.

get_replay_expected_result(Config, AcceptResponse) ->
    case get_group(Config) of
        stateless ->
            % no protection - replayed ticket is accepted
            AcceptResponse;
        stateless_with_cert ->
            % no protection - replayed ticket is accepted
            AcceptResponse;
        _ ->
            {ok, undefined}
    end.

get_alert_reason(Config) ->
    case get_group(Config) of
        stateful ->
            stateful;
        stateful_with_cert ->
            stateful;
        _ ->
            stateless
    end.

get_group(Config) ->
    proplists:get_value(name, ?config(tc_group_properties, Config)).

check_environment(T) ->
    case ssl_test_lib:sufficient_crypto_support('tlsv1.3') of
        true ->
            T;
        _ ->
            {skip, insufficient_environment}
    end.
