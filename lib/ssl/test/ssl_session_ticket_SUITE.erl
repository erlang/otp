%%
%% Copyright Ericsson AB 2007-2022. All Rights Reserved.
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

-module(ssl_session_ticket_SUITE).

-behaviour(ct_suite).

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
-export([basic/0,
         basic/1,
         ticketage_smaller_than_windowsize_anti_replay/0,
         ticketage_smaller_than_windowsize_anti_replay/1,
         ticketage_bigger_than_windowsize_anti_replay/0,
         ticketage_bigger_than_windowsize_anti_replay/1,
         ticketage_out_of_lifetime_anti_replay/0,
         ticketage_out_of_lifetime_anti_replay/1,
         ticket_reuse_anti_replay/0,
         ticket_reuse_anti_replay/1,
         ticket_reuse_anti_replay_server_restart/0,
         ticket_reuse_anti_replay_server_restart/1,
         ticket_reuse_anti_replay_server_restart_reused_seed/0,
         ticket_reuse_anti_replay_server_restart_reused_seed/1,
         basic_stateful_stateless/0,
         basic_stateful_stateless/1,
         basic_stateless_stateful/0,
         basic_stateless_stateful/1,
         basic_stateful_stateless_anti_replay/0,
         basic_stateful_stateless_anti_replay/1,
         basic_stateless_stateful_anti_replay/0,
         basic_stateless_stateful_anti_replay/1,
         basic_stateful_stateless_faulty_ticket/0,
         basic_stateful_stateless_faulty_ticket/1,
         basic_stateless_stateful_faulty_ticket/0,
         basic_stateless_stateful_faulty_ticket/1,
         hello_retry_request/0,
         hello_retry_request/1,
         multiple_tickets/0,
         multiple_tickets/1,
         multiple_tickets_2hash/0,
         multiple_tickets_2hash/1,
         early_data_client_too_much_data/0,
         early_data_client_too_much_data/1,
         early_data_trial_decryption/0,
         early_data_trial_decryption/1,
         early_data_trial_decryption_failure/0,
         early_data_trial_decryption_failure/1,
         early_data_decryption_failure/0,
         early_data_decryption_failure/1,
         early_data_disabled_small_limit/0,
         early_data_disabled_small_limit/1,
         early_data_enabled_small_limit/0,
         early_data_enabled_small_limit/1,
         early_data_basic/0,
         early_data_basic/1,
         early_data_basic_auth/0,
         early_data_basic_auth/1,
         stateless_multiple_servers/0,
         stateless_multiple_servers/1]).

-include("ssl_test_lib.hrl").
-include("tls_handshake.hrl").
-include_lib("common_test/include/ct.hrl").

-define(SLEEP, 500).


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [
     {group, 'tlsv1.3'}
    ].

groups() ->
    [{'tlsv1.3', [], [{group, stateful},
                      {group, stateless},
                      {group, stateful_with_cert},
                      {group, stateless_with_cert},
                      {group, mixed}]},
     {stateful, [], session_tests()},
     {stateless, [], session_tests() ++ anti_replay_tests()},
     {stateful_with_cert, [], session_tests()},
     {stateless_with_cert, [], session_tests() ++ anti_replay_tests()},
     {mixed, [], mixed_tests()}].

session_tests() ->
    [basic,
     hello_retry_request,
     multiple_tickets,
     multiple_tickets_2hash,
     early_data_client_too_much_data,
     early_data_trial_decryption,
     early_data_trial_decryption_failure,
     early_data_decryption_failure,
     early_data_disabled_small_limit,
     early_data_enabled_small_limit,
     early_data_basic,
     early_data_basic_auth].

anti_replay_tests() ->
    [
     ticketage_smaller_than_windowsize_anti_replay,
     ticketage_bigger_than_windowsize_anti_replay,
     ticketage_out_of_lifetime_anti_replay, ticket_reuse_anti_replay,
     ticket_reuse_anti_replay_server_restart,
     ticket_reuse_anti_replay_server_restart_reused_seed,
     stateless_multiple_servers
    ].

mixed_tests() ->
    [
     basic_stateful_stateless,
     basic_stateless_stateful,
     basic_stateful_stateless_anti_replay,
     basic_stateless_stateful_anti_replay,
     basic_stateful_stateless_faulty_ticket,
     basic_stateless_stateful_faulty_ticket
    ].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
            ssl_test_lib:make_rsa_cert(Config0)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(GroupName, Config)
    when GroupName == stateful
         orelse GroupName == stateless
         orelse GroupName == stateful_with_cert
         orelse GroupName == stateless_with_cert ->
    [{server_ticket_mode, GroupName} | proplists:delete(server_ticket_mode, Config)];
init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(_, Config)  ->
    ssl:stop(),
    application:load(ssl),
    ssl:start(),
    ct:timetrap({seconds, 15}),
    Config.

end_per_testcase(_TestCase, Config) ->
    application:unset_env(ssl, server_session_ticket_max_early_data),
    application:unset_env(ssl, server_session_ticket_lifetime),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

basic() ->
    [{doc,"Test session resumption with session tickets (erlang client - erlang server)"}].
basic(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ServerOpts = [{session_tickets, ServerTicketMode},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Full handshake
                                                verify_active_session_resumption,
                                                [false]}},
                                         {from, self()}, {options, ClientOpts}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! get_socket,
    SSocket0 =
        receive
            {Server0, {socket, Socket0}} ->
                Socket0
        end,

    {ok, ClientCert} = ssl:peercert(SSocket0),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [true]}}},

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Short handshake
                                                verify_active_session_resumption,
                                                [true]}},
                                         {from, self()}, {options, ClientOpts}]),
    ssl_test_lib:check_result(Server0, ok, Client1, ok),

    Server0 ! get_socket,
    SSocket1 =
        receive
            {Server0, {socket, Socket1}} ->
                Socket1
        end,

    ExpectedPeercert = case ServerTicketMode of
                           stateful_with_cert -> {ok, ClientCert};
                           stateless_with_cert -> {ok, ClientCert};
                           _ -> {error, no_peercert}
                       end,

    ExpectedPeercert = ssl:peercert(SSocket1),

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client1).

ticketage_smaller_than_windowsize_anti_replay() ->
    [{doc, "Session resumption with stateless tickets and anti_replay enabled."
      "ClientHello with DeltaAge smaller than Bloom filter window size - fresh ClientHello."
      "DeltaAge treated as particular ClientHello adjective, calculated "
      "as a difference between RealAge and ReportedAge of a ticket used in ClientHello."
      "Ticket age smaller than windowsize."
      "(Erlang client - Erlang server)"}].
ticketage_smaller_than_windowsize_anti_replay(Config) when is_list(Config) ->
    {Server0, Client0, Port0, ClientNode, Hostname, ClientOpts} =
        anti_replay_helper_init(Config, auto, 10),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),
    Client1 = anti_replay_helper_connect(Server0, Client0, Port0, ClientNode,
                                         Hostname, ClientOpts, 0, true),
    process_flag(trap_exit, false),
    [ssl_test_lib:close(A) || A <- [Server0, Client0, Client1]].

ticketage_bigger_than_windowsize_anti_replay() ->
    [{doc, "Session resumption with stateless tickets and anti_replay enabled."
      "Fresh ClientHellos."
      "Ticket age bigger than windowsize. 0-RTT is expected to succeed."
      "(Erlang client - Erlang server)"}].
ticketage_bigger_than_windowsize_anti_replay(Config) when is_list(Config) ->
    WindowSize = 3,
    {Server0, Client0, Port0, ClientNode, Hostname, ClientOpts} =
        anti_replay_helper_init(Config, auto, WindowSize),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),
    Client1 = anti_replay_helper_connect(Server0, Client0, Port0, ClientNode,
                                         Hostname, ClientOpts,
                                         {seconds, WindowSize + 2}, true),
    Client2 = anti_replay_helper_connect(Server0, Client0, Port0, ClientNode,
                                         Hostname, ClientOpts,
                                         {seconds, 2*WindowSize + 2}, true),
    process_flag(trap_exit, false),
    [ssl_test_lib:close(A) || A <- [Server0, Client0, Client1, Client2]].

ticketage_out_of_lifetime_anti_replay() ->
    [{doc, "Session resumption with stateless tickets and anti_replay enabled."
      "Fresh ClientHello."
      "Ticket age beyond its lifetime. 0-RTT is expected to fail."
      "(Erlang client - Erlang server)"}].
ticketage_out_of_lifetime_anti_replay(Config) when is_list(Config) ->
    Lifetime = 4,
    WindowSize = 2,
    {Server0, Client0, Port0, ClientNode, Hostname, ClientOpts} =
        anti_replay_helper_init(Config, auto, WindowSize, Lifetime),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),
    Client1 = anti_replay_helper_connect(Server0, Client0, Port0, ClientNode,
                                         Hostname, ClientOpts,
                                         {seconds, Lifetime + 2}, false),
    process_flag(trap_exit, false),
    [ssl_test_lib:close(A) || A <- [Server0, Client0, Client1]].

ticket_reuse_anti_replay() ->
    [{doc, "Verify that 2 connection attempts with same stateless tickets "
      "are successful."
      "Fresh ClientHellos."
      "Ticket age smaller than windowsize."
      "Anti_replay allows it because both "
      "ClientHellos are unique, look fresh(small enough DeltaAge) "
      "and do not look like a replay attempt."
      "(Erlang client - Erlang server)"}].
ticket_reuse_anti_replay(Config) when is_list(Config) ->
    {Server0, Client0, Port0, ClientNode, Hostname, ClientOpts0} =
        anti_replay_helper_init(Config, manual, 10),
    [Ticket] = ssl_test_lib:check_tickets(Client0),
    ssl_test_lib:check_result(Server0, ok),
    ClientOpts1 = [{use_ticket, [Ticket]} | ClientOpts0],
    Client1 = anti_replay_helper_connect(Server0, Client0, Port0, ClientNode,
                                         Hostname, ClientOpts1, 0, true),
    Client2 = anti_replay_helper_connect(Server0, Client1, Port0, ClientNode,
                                         Hostname, ClientOpts1, 0, true),
    process_flag(trap_exit, false),
    [ssl_test_lib:close(A) || A <- [Server0, Client0, Client2]].

ticket_reuse_anti_replay_server_restart() ->
    [{doc, "Verify 2 connection attempts with same stateless tickets "
      "and server restart between. Second attempt is expected to fail as long as "
      "Bloom filter window overlaps with startup time."
      "Fresh ClientHellos."
      "(Erlang client - Erlang server)"

      "RFC8446 8.2: When implementations are freshly started, they SHOULD reject "
      "0-RTT as long as any portion of their recording window overlaps the startup "
      "time. Otherwise, they run the risk of accepting replays which were "
      "originally sent during that period."
     }].
ticket_reuse_anti_replay_server_restart(Config) when is_list(Config) ->
    WindowSize = 10,
    {Server0, Client0, Port0, ClientNode, Hostname, ClientOpts0} =
        anti_replay_helper_init(Config, manual, WindowSize),
    [Ticket] = ssl_test_lib:check_tickets(Client0),
    ssl_test_lib:check_result(Server0, ok),
    ClientOpts1 = [{use_ticket, [Ticket]} | ClientOpts0],
    Client1 = anti_replay_helper_connect(Server0, Client0, Port0, ClientNode,
                                         Hostname, ClientOpts1, 0, true),
    {Server1, Port1} = anti_replay_helper_start_server(Config, WindowSize),
    Client2 = anti_replay_helper_connect(Server1, Client1, Port1, ClientNode,
                                         Hostname, ClientOpts1, 0, false, false),
    process_flag(trap_exit, false),
    [ssl_test_lib:close(A) || A <- [Server0, Client2, Server1]].

ticket_reuse_anti_replay_server_restart_reused_seed() ->
    [{doc, "Verify 2 connection attempts with same stateless tickets "
      "and server restart between, with the server using the same session "
      "ticket encryption seed between restarts. Second attempt is expected to "
      "fail as long as the Bloom filter window overlaps with startup time."
     }].
ticket_reuse_anti_replay_server_restart_reused_seed(Config) when is_list(Config) ->
    WindowSize = 10,
    Seed = crypto:strong_rand_bytes(32),
    Config1 = [{server_ticket_seed, Seed} | Config],
    {Server1 , Port1} = anti_replay_helper_start_server(Config1, WindowSize),
    {ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ClientOpts1 = [{session_tickets, manual},
                  {versions, ['tlsv1.2','tlsv1.3']} | ClientOpts0],
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port1}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% full handshake
                                                verify_active_session_resumption,
                                                [false, wait_reply, {tickets, 1}]}},
                                         {from, self()}, {options, ClientOpts1}]),
    [Ticket] = ssl_test_lib:check_tickets(Client1),
    ssl_test_lib:check_result(Server1, ok),
    ClientOpts2 = [{use_ticket, [Ticket]} | ClientOpts1],
    {Server2, Port2} = anti_replay_helper_start_server(Config1, WindowSize),
    Client2 = anti_replay_helper_connect(Server2, Client1, Port2, ClientNode,
                                         Hostname, ClientOpts2, 0, false, false),
    process_flag(trap_exit, false),
    [ssl_test_lib:close(A) || A <- [Server1, Client2, Server2]].

anti_replay_helper_init(Config, Mode, WindowSize) ->
    DefaultLifetime = ssl_config:get_ticket_lifetime(),
    anti_replay_helper_init(Config, Mode, WindowSize, DefaultLifetime).

anti_replay_helper_init(Config, Mode, WindowSize, Lifetime) ->
    application:set_env(ssl, server_session_ticket_lifetime, Lifetime),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    {ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %% Configure session tickets
    ClientOpts = [{session_tickets, Mode},
                  {versions, ['tlsv1.2','tlsv1.3']} | ClientOpts0],

    {Server0 , Port0} = anti_replay_helper_start_server(Config, WindowSize),
    MFA = case Mode of
              auto ->
                  {ssl_test_lib,
                   verify_active_session_resumption,
                   [false]};
              manual ->
                  {ssl_test_lib,
                   verify_active_session_resumption,
                   [false, wait_reply, {tickets, 1}]}
          end,

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, MFA},
                                         {from, self()}, {options, ClientOpts}]),
    {Server0, Client0, Port0, ClientNode, Hostname, ClientOpts}.

anti_replay_helper_start_server(Config, WindowSize) ->
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),
    ServerTicketSeed =
        case proplists:get_value(server_ticket_seed, Config) of
            undefined ->
                [];
            Seed ->
                [{stateless_tickets_seed, Seed}]
        end,
    ServerOpts = [{session_tickets, ServerTicketMode},
                  {anti_replay, {WindowSize, 5, 72985}},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0
                 ] ++ ServerTicketSeed,

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server),
    {Server, Port0}.

anti_replay_helper_connect(Server, Client0, Port0, ClientNode, Hostname,
                           ClientOpts, Delay, ExpectedResumption) ->
    anti_replay_helper_connect(Server, Client0, Port0, ClientNode, Hostname,
                               ClientOpts, Delay, ExpectedResumption, true).

anti_replay_helper_connect(Server, Client0, Port0, ClientNode, Hostname,
                           ClientOpts, Delay, ExpectedResumption, SendListen) ->
    case SendListen of
        true ->
            Server ! {listen, {mfa, {ssl_test_lib,
                                     verify_active_session_resumption,
                                     [ExpectedResumption]}}};
        _ ->
            ok
    end,

    %% Wait for session ticket
    ct:sleep(100),
    ssl_test_lib:close(Client0),
    ct:sleep(Delay),
    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Short handshake
                                                verify_active_session_resumption,
                                                [ExpectedResumption]}},
                                         {from, self()}, {options, ClientOpts}]),
    ssl_test_lib:check_result(Server, ok, Client1, ok),
    Client1.

basic_stateful_stateless() ->
    [{doc,"Test session resumption with session tickets (erlang client - erlang server)"}].
basic_stateful_stateless(Config) when is_list(Config) ->
    do_test_mixed(Config,
                  [{session_tickets, auto},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateful},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateless},
                   {versions, ['tlsv1.2','tlsv1.3']}]).

basic_stateless_stateful() ->
    [{doc,"Test session resumption with session tickets (erlang client - erlang server)"}].
basic_stateless_stateful(Config) when is_list(Config) ->
    do_test_mixed(Config,
                  [{session_tickets, auto},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateless},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateful},
                   {versions, ['tlsv1.2','tlsv1.3']}]).

basic_stateful_stateless_anti_replay() ->
    [{doc,"Test session resumption with session tickets (erlang client - erlang server)"}].
basic_stateful_stateless_anti_replay(Config) when is_list(Config) ->
    do_test_mixed(Config,
                  [{session_tickets, auto},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateful},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateless},
                   {anti_replay, '10k'},
                   {versions, ['tlsv1.2','tlsv1.3']}]).

basic_stateless_stateful_anti_replay() ->
    [{doc,"Test session resumption with session tickets (erlang client - erlang server)"}].
basic_stateless_stateful_anti_replay(Config) when is_list(Config) ->
    do_test_mixed(Config,
                  [{session_tickets, auto},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateless},
                   {anti_replay, '10k'},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateful},
                   {versions, ['tlsv1.2','tlsv1.3']}]).

basic_stateful_stateless_faulty_ticket() ->
    [{doc,"Test session resumption with session tickets (erlang client - erlang server)"}].
basic_stateful_stateless_faulty_ticket(Config) when is_list(Config) ->
    do_test_mixed(Config,
                  [{session_tickets, auto},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, manual},
                   {use_ticket, [<<131,100,0,12,"faultyticket">>,
                                 <<"faulty ticket">>]},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateless},
                   {anti_replay, '10k'},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateful},
                   {versions, ['tlsv1.2','tlsv1.3']}]).

basic_stateless_stateful_faulty_ticket() ->
    [{doc,"Test session resumption with session tickets (erlang client - erlang server)"}].
basic_stateless_stateful_faulty_ticket(Config) when is_list(Config) ->
    do_test_mixed(Config,
                  [{session_tickets, auto},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, manual},
                   {use_ticket, [<<"faulty ticket">>,
                                 <<131,100,0,12,"faultyticket">>]},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateless},
                   {anti_replay, '10k'},
                   {versions, ['tlsv1.2','tlsv1.3']}],
                  [{session_tickets, stateful},
                   {versions, ['tlsv1.2','tlsv1.3']}]).

hello_retry_request() ->
    [{doc,"Test session resumption with session tickets and hello_retry_request (erlang client - erlang server)"}].
hello_retry_request(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),
    
    %% Configure session tickets
    ClientOpts = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups,[secp256r1, x25519]}|ClientOpts0],
    ServerOpts = [{session_tickets, ServerTicketMode},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups, [x448, x25519]}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false]}},
                                         {from, self()},  {options, ClientOpts}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [true]}}},

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [true]}},
                                         {from, self()}, {options, ClientOpts}]),
    ssl_test_lib:check_result(Server0, ok, Client1, ok),

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client1).

multiple_tickets() ->
    [{doc,"Test session resumption with multiple session tickets (erlang client - erlang server)"}].
multiple_tickets(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts = [{session_tickets, manual},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ServerOpts = [{session_tickets, ServerTicketMode},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, wait_reply, {tickets, 3}]}},
                                         {from, self()},  {options, ClientOpts}]),

    Tickets0 = ssl_test_lib:check_tickets(Client0),

    ?CT_LOG("Received tickets: ~p~n", [Tickets0]),

    ssl_test_lib:check_result(Server0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [true]}}},

    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [true, wait_reply, no_tickets]}},
                                         {from, self()},
                                         {options, [{use_ticket, Tickets0}|ClientOpts]}]),

    ssl_test_lib:check_result(Server0, ok, Client1, ok),

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client1).

multiple_tickets_2hash() ->
    [{doc,"Test session resumption with multiple session tickets with 2 different hash algorithms (erlang client - erlang server)"}].
multiple_tickets_2hash(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),
    
    %% Configure session tickets
    ClientOpts = [{session_tickets, manual},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ServerOpts = [{session_tickets, ServerTicketMode},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Get tickets using sha256
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, wait_reply, {tickets, 3}]}},
                                         {from, self()},
                                         {options, [{ciphers, [#{key_exchange => any,
                                                                 cipher => aes_128_gcm,
                                                                 mac => aead,
                                                                 prf => sha256}]}| ClientOpts]}]),

    Tickets0 = ssl_test_lib:check_tickets(Client0),

    ?CT_LOG("Received tickets: ~p~n", [Tickets0]),

    ssl_test_lib:check_result(Server0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [false]}}},

    ssl_test_lib:close(Client0),

    %% Get tickets using sha384
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, wait_reply, {tickets, 3}]}},
                                         {from, self()},
                                         {options, [{ciphers, [#{key_exchange => any,
                                                                 cipher => aes_256_gcm,
                                                                 mac => aead,
                                                                 prf => sha384}]}| ClientOpts]}]),

    Tickets1 = ssl_test_lib:check_tickets(Client1),

    ?CT_LOG("Received tickets: ~p~n", [Tickets1]),

    ssl_test_lib:check_result(Server0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [true]}}},

    ssl_test_lib:close(Client1),

    %% Use tickets for handshake (server chooses TLS_AES_256_GCM_SHA384 cipher suite)
    Client2 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Short handshake
                                                verify_active_session_resumption,
                                                [true]}},
                                         {from, self()},
                                         {options, [{use_ticket, Tickets0 ++ Tickets1}|ClientOpts]}]),
    ssl_test_lib:check_result(Server0, ok, Client2, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [true]}}},

    ssl_test_lib:close(Client2),

    %% Use tickets for handshake (client chooses TLS_CHACHA20_POLY1305_SHA256 cipher suite)
    Client3 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Short handshake
                                                verify_active_session_resumption,
                                                [true]}},
                                         {from, self()},
                                         {options, [{ciphers, [#{key_exchange => any,
                                                                 cipher => chacha20_poly1305,
                                                                 mac => aead,
                                                                 prf => sha256}]},
                                                    {use_ticket, Tickets0 ++ Tickets1}|ClientOpts]}]),
    ssl_test_lib:check_result(Server0, ok, Client3, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [false]}}},

    ssl_test_lib:close(Client3),

    %% Use tickets (created using sha384) for handshake (client chooses
    %% TLS_CHACHA20_POLY1305_SHA256 cipher suite).
    %% Session resumption should fail as chosen cipher suite uses different hash algorithms
    %% than those supplied by the selected tickets.
    Client4 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Short handshake
                                                verify_active_session_resumption,
                                                [false]}},
                                         {from, self()},
                                         {options, [{ciphers, [#{key_exchange => any,
                                                                 cipher => chacha20_poly1305,
                                                                 mac => aead,
                                                                 prf => sha256}]},
                                                    {use_ticket, Tickets1}|ClientOpts]}]),
    ssl_test_lib:check_result(Server0, ok, Client4, ok),

    ssl_test_lib:close(Client4),

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server0).

early_data_trial_decryption() ->
    [{doc,"Test trial decryption when server rejects early data (erlang client - erlang server)"}].
early_data_trial_decryption(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    %% Send maximum sized early data to verify calculation of plain text size
    %% in the server.
    ClientOpts2 = [{early_data, binary:copy(<<"F">>, 16384)}|ClientOpts1],

    %% Disabled early data triggers trial decryption upon receiving early data
    ServerOpts = [{session_tickets, ServerTicketMode}, {early_data, disabled},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Full handshake
                                                verify_active_session_resumption,
                                                [false]}},
                                         {from, self()}, {options, ClientOpts1}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [true]}}},

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Short handshake
                                                verify_active_session_resumption,
                                                [true]}},
                                         {from, self()}, {options, ClientOpts2}]),
    ssl_test_lib:check_result(Server0, ok, Client1, ok),

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client1).

early_data_client_too_much_data() ->
    [{doc,"Client sending too much early data (erlang client - erlang server)"}].
early_data_client_too_much_data(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, manual},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    %% Send more early data than max_early_data_size to verify calculation
    %% of plain text size in the server.
    MaxEarlyDataSize = 10000,
    ClientOpts2 = [{early_data, binary:copy(<<"F">>, 16384)}|ClientOpts1],

    ServerOpts = [{session_tickets, ServerTicketMode}, {early_data, disabled},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    application:set_env(ssl, server_session_ticket_max_early_data, MaxEarlyDataSize),
    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    application:unset_env(ssl, server_session_ticket_max_early_data),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Full handshake
                                                verify_active_session_resumption,
                                                [false, no_reply, {tickets, 1}]}},
                                         {from, self()}, {options, ClientOpts1}]),
    Tickets0 = ssl_test_lib:check_tickets(Client0),
    ssl_test_lib:verify_session_ticket_extension(Tickets0, MaxEarlyDataSize),
    %% ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [false, no_reply]}}},

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client_error([{node, ClientNode},
                                               {port, Port0}, {host, Hostname},
                                               {mfa, {ssl_test_lib,  %% Short handshake
                                                      verify_active_session_resumption,
                                                      [false, no_reply, no_tickets]}},
                                               {from, self()}, {options, [{use_ticket, Tickets0}|ClientOpts2]}]),
    ssl_test_lib:check_client_alert(Client1, illegal_parameter),
    process_flag(trap_exit, false),
    ssl_test_lib:close(Server0).

early_data_trial_decryption_failure() ->
    [{doc,"Emulate faulty client that sends too much early data (erlang client - erlang server)"}].
early_data_trial_decryption_failure(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, manual},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    %% Send more early data than max_early_data_size to verify calculation
    %% of plain text size in the server.
    MaxEarlyDataSize = 10000,
    ClientOpts2 = [{early_data, binary:copy(<<"F">>, 16385)}|ClientOpts1],

    %% Disabled early data triggers trial decryption upon receiving early data
    %% up to the configured amount. If more data is received the server triggers
    %% a bad_record_mac alert.
    %% It is not possible to trigger this condition in normal use cases:
    %%  - The ssl client in auto mode has a built in protection against sending
    %%    too much early data. It will not send any early data.
    %%  - The ssl client in manual mode can only send the mount that is received
    %%    in the ticket used for the 0-RTT handshake. If more data is sent the
    %%    client will trigger an illegal_parameter alert (too_much_early_data).
    ServerOpts = [{session_tickets, ServerTicketMode}, {early_data, disabled},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    application:set_env(ssl, server_session_ticket_max_early_data, MaxEarlyDataSize),
    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Full handshake
                                                verify_active_session_resumption,
                                                [false, no_reply, {tickets, 1}]}},
                                         {from, self()}, {options, ClientOpts1}]),
    Tickets0 = ssl_test_lib:check_tickets(Client0),
    %% Simulate a faulty client by updating the max_early_data_size extension in
    %% the received session ticket
    Tickets1 = ssl_test_lib:update_session_ticket_extension(Tickets0, 16385),
    %% ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [false, no_reply]}}},

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    %% Use ticket
    _Client1 = ssl_test_lib:start_client_error([{node, ClientNode},
                                               {port, Port0}, {host, Hostname},
                                               {mfa, {ssl_test_lib,  %% Short handshake
                                                      verify_active_session_resumption,
                                                      [false, no_reply, no_tickets]}},
                                               {from, self()}, {options, [{use_ticket, Tickets1}|ClientOpts2]}]),
    ssl_test_lib:check_server_alert(Server0, bad_record_mac),
    process_flag(trap_exit, false),
    application:unset_env(ssl, server_session_ticket_max_early_data),
    ssl_test_lib:close(Server0).

early_data_decryption_failure() ->
    [{doc,"Emulate faulty client that sends too much early data - server early_data enabled (erlang client - erlang server)"}].
early_data_decryption_failure(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, manual},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    %% Send more early data than max_early_data_size to verify calculation
    %% of plain text size in the server.
    MaxEarlyDataSize = 10000,
    ClientOpts2 = [{early_data, binary:copy(<<"F">>, 16385)}|ClientOpts1],

    ServerOpts = [{session_tickets, ServerTicketMode}, {early_data, enabled},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    application:set_env(ssl, server_session_ticket_max_early_data, MaxEarlyDataSize),
    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Full handshake
                                                verify_active_session_resumption,
                                                [false, no_reply, {tickets, 1}]}},
                                         {from, self()}, {options, ClientOpts1}]),
    Tickets0 = ssl_test_lib:check_tickets(Client0),
    %% Simulate a faulty client by updating the max_early_data_size extension in
    %% the received session ticket
    Tickets1 = ssl_test_lib:update_session_ticket_extension(Tickets0, 16385),
    %% ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [false, no_reply]}}},

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    %% Use ticket
    _Client1 = ssl_test_lib:start_client_error([{node, ClientNode},
                                               {port, Port0}, {host, Hostname},
                                               {mfa, {ssl_test_lib,  %% Short handshake
                                                      verify_active_session_resumption,
                                                      [false, no_reply, no_tickets]}},
                                               {from, self()}, {options, [{use_ticket, Tickets1}|ClientOpts2]}]),
    ssl_test_lib:check_server_alert(Server0, unexpected_message),
    process_flag(trap_exit, false),
    application:unset_env(ssl, server_session_ticket_max_early_data),
    ssl_test_lib:close(Server0).

early_data_disabled_small_limit() ->
    [{doc,"Test trial decryption when server rejects early data (erlang client - erlang server)"}].
early_data_disabled_small_limit(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    %% Send maximum sized early data to verify calculation of plain text size
    %% in the server.
    MaxEarlyDataSize = 5,
    ClientOpts2 = [{early_data, binary:copy(<<"F">>, 4)}|ClientOpts1],

    %% Disabled early data triggers trial decryption upon receiving early data
    ServerOpts = [{session_tickets, ServerTicketMode}, {early_data, disabled},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],
    application:set_env(ssl, server_session_ticket_max_early_data, MaxEarlyDataSize),
    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Full handshake
                                                verify_active_session_resumption,
                                                [false]}},
                                         {from, self()}, {options, ClientOpts1}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [true]}}},

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Short handshake
                                                verify_active_session_resumption,
                                                [true]}},
                                         {from, self()}, {options, ClientOpts2}]),
    ssl_test_lib:check_result(Server0, ok, Client1, ok),

    process_flag(trap_exit, false),
    application:unset_env(ssl, server_session_ticket_max_early_data),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client1).

early_data_enabled_small_limit() ->
    [{doc,"Test decryption when server accepts early data (erlang client - erlang server)"}].
early_data_enabled_small_limit(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    %% Send maximum sized early data to verify calculation of plain text size
    %% in the server.
    MaxEarlyDataSize = 5,
    ClientOpts2 = [{early_data, binary:copy(<<"F">>, 4)}|ClientOpts1],

    %% Disabled early data triggers trial decryption upon receiving early data
    ServerOpts = [{session_tickets, ServerTicketMode}, {early_data, enabled},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],
    application:set_env(ssl, server_session_ticket_max_early_data, MaxEarlyDataSize),
    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Full handshake
                                                verify_active_session_resumption,
                                                [false]}},
                                         {from, self()}, {options, ClientOpts1}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [true]}}},

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Short handshake
                                                verify_active_session_resumption,
                                                [true]}},
                                         {from, self()}, {options, ClientOpts2}]),
    ssl_test_lib:check_result(Server0, ok, Client1, ok),

    process_flag(trap_exit, false),
    application:unset_env(ssl, server_session_ticket_max_early_data),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client1).

early_data_basic() ->
    [{doc,"Test early data when client is not authenticated (erlang client - erlang server)"}].
early_data_basic(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    %% Send maximum sized early data to verify calculation of plain text size
    %% in the server.
    EarlyData = binary:copy(<<"F">>, 16384),
    ClientOpts2 = [{early_data, binary:copy(<<"F">>, 16384)}|ClientOpts1],

    %% Disabled early data triggers trial decryption upon receiving early data
    ServerOpts = [{session_tickets, ServerTicketMode}, {early_data, enabled},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Full handshake
                                                verify_active_session_resumption,
                                                [false]}},
                                         {from, self()}, {options, ClientOpts1}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_server_early_data,
                              [wait_reply, EarlyData]}}},

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Short handshake
                                                verify_active_session_resumption,
                                                [true]}},
                                         {from, self()}, {options, ClientOpts2}]),
    ssl_test_lib:check_result(Server0, ok, Client1, ok),

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client1).

early_data_basic_auth() ->
    [{doc,"Test early data when client is authenticated (erlang client - erlang server)"}].
early_data_basic_auth(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    %% Send maximum sized early data to verify calculation of plain text size
    %% in the server.
    EarlyData = binary:copy(<<"F">>, 16384),
    ClientOpts2 = [{early_data, binary:copy(<<"F">>, 16384)}|ClientOpts1],

    %% Disabled early data triggers trial decryption upon receiving early data
    ServerOpts = [{session_tickets, ServerTicketMode}, {early_data, enabled},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Full handshake
                                                verify_active_session_resumption,
                                                [false]}},
                                         {from, self()}, {options, ClientOpts1}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_server_early_data,
                              [wait_reply, EarlyData]}}},

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    %% TODO This test should fail!
    %% State transition is not implemented from wait_eoed to wait_cert!
    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Short handshake
                                                verify_active_session_resumption,
                                                [true]}},
                                         {from, self()},
                                         {options,
                                          proplists:delete(keyfile,
                                                           proplists:delete(certfile, ClientOpts2))}]),
    ssl_test_lib:check_result(Server0, ok, Client1, ok),

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client1).

stateless_multiple_servers() ->
    [{doc, "Test session resumption with session tickets, resuming on different server"}].
stateless_multiple_servers(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Seed = crypto:strong_rand_bytes(64),

    %% Configure session tickets
    ClientOpts = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']} | ClientOpts0],
    ServerOpts = [{session_tickets, stateless},
                  {stateless_tickets_seed, Seed},
                  {versions, ['tlsv1.2','tlsv1.3']} | ServerOpts0],

    Server0 =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
                                   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    Server1 =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {mfa, {ssl_test_lib,  %% Short handshake
                                          verify_active_session_resumption,
                                          [true]}},
                                   {options, ServerOpts}]),
    Port1 = ssl_test_lib:inet_port(Server1),

    %% Store ticket from first connection to server 0
    Client0 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port0}, {host, Hostname},
                                   {mfa, {ssl_test_lib,  %% Full handshake
                                          verify_active_session_resumption,
                                          [false]}},
                                   {from, self()}, {options, ClientOpts}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    %% Use ticket when connecting to server 1
    Client1 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port1}, {host, Hostname},
                                   {mfa, {ssl_test_lib,  %% Short handshake
                                          verify_active_session_resumption,
                                          [true]}},
                                   {from, self()}, {options, ClientOpts}]),
    ssl_test_lib:check_result(Server1, ok, Client1, ok),

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

do_test_mixed(Config, COpts, SOpts1, SOpts2) when is_list(Config) ->
    do_test_mixed(Config, COpts, COpts, SOpts1, SOpts2).
%%
do_test_mixed(Config, COpts1, COpts2, SOpts1, SOpts2) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    %% Configure session tickets
    ClientOpts1 = COpts1 ++ ClientOpts0,
    ServerOpts1 = SOpts1 ++ ServerOpts0,

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts1}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Full handshake
                                                verify_active_session_resumption,
                                                [false]}},
                                         {from, self()}, {options, ClientOpts1}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Server0),

    ClientOpts2 = COpts2 ++ ClientOpts0,
    ServerOpts2 = SOpts2 ++ ServerOpts0,

    Server1 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts2}]),
    Port1 = ssl_test_lib:inet_port(Server1),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port1}, {host, Hostname},
                                         {mfa, {ssl_test_lib,  %% Short handshake
                                                verify_active_session_resumption,
                                                [false]}},
                                         {from, self()}, {options, ClientOpts2}]),
    ssl_test_lib:check_result(Server1, ok, Client1, ok),

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).
