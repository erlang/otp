%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2019. All Rights Reserved.
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
-module(ssl_session_ticket_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

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
    [{'tlsv1.3', [], [{group, stateful}, {group, stateless}, {group, openssl_server}]},
     {openssl_server, [], [erlang_client_openssl_server_basic,
                           erlang_client_openssl_server_hrr,
                           erlang_client_openssl_server_hrr_multiple_tickets
                          ]},
     {stateful, [], session_tests()},
     {stateless, [], session_tests()}].

session_tests() ->
    [erlang_client_erlang_server_basic,
     openssl_client_erlang_server_basic,
     erlang_client_erlang_server_hrr,
     openssl_client_erlang_server_hrr,
     erlang_client_erlang_server_multiple_tickets,
     erlang_client_erlang_server_multiple_tickets_2hash].

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

init_per_group(stateful, Config) ->
    [{server_ticket_mode, stateful} | proplists:delete(server_ticket_mode, Config)];
init_per_group(stateless, Config) ->
    [{server_ticket_mode, stateless} | proplists:delete(server_ticket_mode, Config)];
init_per_group(GroupName, Config) ->
    ssl_test_lib:clean_tls_version(Config),
    case ssl_test_lib:is_tls_version(GroupName) andalso ssl_test_lib:sufficient_crypto_support(GroupName) of
	true ->
	    ssl_test_lib:init_tls_version(GroupName, Config);
	_ ->
	    case ssl_test_lib:sufficient_crypto_support(GroupName) of
		true ->
		    ssl:start(),
		    Config;
		false ->
		    {skip, "Missing crypto support"}
	    end
    end.

end_per_group(GroupName, Config) ->
  case ssl_test_lib:is_tls_version(GroupName) of
      true ->
          ssl_test_lib:clean_tls_version(Config);
      false ->
          Config
  end.

init_per_testcase(_, Config)  ->
    ssl:stop(),
    application:load(ssl),
    ssl:start(),
    ct:timetrap({seconds, 15}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------


erlang_client_erlang_server_basic() ->
    [{doc,"Test session resumption with session tickets (erlang client - erlang server)"}].
erlang_client_erlang_server_basic(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts = [{session_tickets, auto}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ServerOpts = [{session_tickets, ServerTicketMode}, {log_level, debug},
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

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client1).


erlang_client_openssl_server_basic() ->
    [{doc,"Test session resumption with session tickets (erlang client - openssl server)"}].
erlang_client_openssl_server_basic(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Version = 'tlsv1.3',
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    CACertFile = proplists:get_value(cacertfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),

    %% Configure session tickets
    ClientOpts = [{session_tickets, auto}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],

    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port), ssl_test_lib:version_flag(Version),
            "-cert", CertFile,"-key", KeyFile, "-CAfile", CACertFile, "-msg", "-debug"],

    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),

    ssl_test_lib:wait_for_openssl_server(Port,  proplists:get_value(protocol, Config)),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, no_reply]}},
                                         {from, self()},  {options, ClientOpts}]),
    %% Wait for session ticket
    ct:sleep(100),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [true, no_reply]}},
                                         {from, self()},
                                         {options, ClientOpts}]),
    process_flag(trap_exit, false),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client1).


openssl_client_erlang_server_basic() ->
    [{doc,"Test session resumption with session tickets (openssl client - erlang server)"}].
openssl_client_erlang_server_basic(Config) when is_list(Config) ->
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TicketFile0 = filename:join([proplists:get_value(priv_dir, Config), "session_ticket0"]),
    TicketFile1 = filename:join([proplists:get_value(priv_dir, Config), "session_ticket1"]),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    Data = "Hello world",

    %% Configure session tickets
    ServerOpts = [{session_tickets, ServerTicketMode}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),

    Version = 'tlsv1.3',
    Port0 = ssl_test_lib:inet_port(Server0),

    Exe = "openssl",
    Args0 = ["s_client", "-connect", ssl_test_lib:hostname_format(Hostname)
            ++ ":" ++ integer_to_list(Port0),
            ssl_test_lib:version_flag(Version),
            "-sess_out", TicketFile0],

    OpenSslPort0 =  ssl_test_lib:portable_open_port(Exe, Args0),

    true = port_command(OpenSslPort0, Data),

    ssl_test_lib:check_result(Server0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [true]}}},

    %% Wait for session ticket
    ct:sleep(100),

    Args1 = ["s_client", "-connect", ssl_test_lib:hostname_format(Hostname)
            ++ ":" ++ integer_to_list(Port0),
            ssl_test_lib:version_flag(Version),
            "-sess_in", TicketFile0,
            "-sess_out", TicketFile1],

    OpenSslPort1 =  ssl_test_lib:portable_open_port(Exe, Args1),

    true = port_command(OpenSslPort1, Data),

    ssl_test_lib:check_result(Server0, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server0),
    ssl_test_lib:close_port(OpenSslPort0),
    ssl_test_lib:close_port(OpenSslPort1).


erlang_client_erlang_server_hrr() ->
    [{doc,"Test session resumption with session tickets and hello_retry_request (erlang client - erlang server)"}].
erlang_client_erlang_server_hrr(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),
    
    %% Configure session tickets
    ClientOpts = [{session_tickets, auto}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups,[secp256r1, x25519]}|ClientOpts0],
    ServerOpts = [{session_tickets, ServerTicketMode}, {log_level, debug},
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


erlang_client_openssl_server_hrr() ->
    [{doc,"Test session resumption with session tickets and hello_retry_request (erlang client - openssl server)"}].
erlang_client_openssl_server_hrr(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Version = 'tlsv1.3',
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    CACertFile = proplists:get_value(cacertfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),

    %% Configure session tickets
    ClientOpts = [{session_tickets, auto}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups,[secp256r1, x25519]}|ClientOpts0],

    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port), ssl_test_lib:version_flag(Version),
            "-cert", CertFile,
            "-key", KeyFile,
            "-CAfile", CACertFile,
            "-groups", "X448:X25519",
            "-msg", "-debug"],

    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),

    ssl_test_lib:wait_for_openssl_server(Port,  proplists:get_value(protocol, Config)),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, no_reply]}},
                                         {from, self()}, {options, ClientOpts}]),
    %% Wait for session ticket
    ct:sleep(100),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [true, no_reply]}},
                                         {from, self()},
                                         {options, ClientOpts}]),
    process_flag(trap_exit, false),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client1).


openssl_client_erlang_server_hrr() ->
    [{doc,"Test session resumption with session tickets and hello_retry_request (openssl client - erlang server)"}].
openssl_client_erlang_server_hrr(Config) when is_list(Config) ->
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TicketFile0 = filename:join([proplists:get_value(priv_dir, Config), "session_ticket0"]),
    TicketFile1 = filename:join([proplists:get_value(priv_dir, Config), "session_ticket1"]),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    Data = "Hello world",

    %% Configure session tickets
    ServerOpts = [{session_tickets, ServerTicketMode}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups,[x448, x25519]}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),

    Version = 'tlsv1.3',
    Port0 = ssl_test_lib:inet_port(Server0),

    Exe = "openssl",
    Args0 = ["s_client", "-connect", ssl_test_lib:hostname_format(Hostname)
            ++ ":" ++ integer_to_list(Port0),
            ssl_test_lib:version_flag(Version),
            "-groups", "P-256:X25519",
            "-sess_out", TicketFile0],

    OpenSslPort0 =  ssl_test_lib:portable_open_port(Exe, Args0),

    true = port_command(OpenSslPort0, Data),

    ssl_test_lib:check_result(Server0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [true]}}},

    %% Wait for session ticket
    ct:sleep(100),

    Args1 = ["s_client", "-connect", ssl_test_lib:hostname_format(Hostname)
            ++ ":" ++ integer_to_list(Port0),
            ssl_test_lib:version_flag(Version),
            "-groups", "P-256:X25519",
            "-sess_in", TicketFile0,
            "-sess_out", TicketFile1],

    OpenSslPort1 =  ssl_test_lib:portable_open_port(Exe, Args1),

    true = port_command(OpenSslPort1, Data),

    ssl_test_lib:check_result(Server0, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server0),
    ssl_test_lib:close_port(OpenSslPort0),
    ssl_test_lib:close_port(OpenSslPort1).


erlang_client_erlang_server_multiple_tickets() ->
    [{doc,"Test session resumption with multiple session tickets (erlang client - erlang server)"}].
erlang_client_erlang_server_multiple_tickets(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    %% Configure session tickets
    ClientOpts = [{session_tickets, enabled}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ServerOpts = [{session_tickets, ServerTicketMode}, {log_level, debug},
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

    ct:pal("Received tickets: ~p~n", [Tickets0]),

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


erlang_client_openssl_server_hrr_multiple_tickets() ->
    [{doc,"Test session resumption with multiple session tickets and hello_retry_request (erlang client - openssl server)"}].
erlang_client_openssl_server_hrr_multiple_tickets(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Version = 'tlsv1.3',
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    CACertFile = proplists:get_value(cacertfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),

    %% Configure session tickets
    ClientOpts = [{session_tickets, enabled}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups,[secp256r1, x25519]}|ClientOpts0],

    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port), ssl_test_lib:version_flag(Version),
            "-cert", CertFile,
            "-key", KeyFile,
            "-CAfile", CACertFile,
            "-groups", "X448:X25519",
            "-msg", "-debug"],

    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),

    ssl_test_lib:wait_for_openssl_server(Port,  proplists:get_value(protocol, Config)),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, no_reply, {tickets, 2}]}},
                                         {from, self()},  {options, ClientOpts}]),

    Tickets0 = ssl_test_lib:check_tickets(Client0),

    ct:pal("Received tickets: ~p~n", [Tickets0]),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Use tickets
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [true, no_reply, no_tickets]}},
                                         {from, self()},
                                         {options, [{use_ticket, Tickets0}|ClientOpts]}]),

    process_flag(trap_exit, false),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client1).


erlang_client_erlang_server_multiple_tickets_2hash() ->
    [{doc,"Test session resumption with multiple session tickets with 2 different hash algorithms (erlang client - erlang server)"}].
erlang_client_erlang_server_multiple_tickets_2hash(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),
    
    %% Configure session tickets
    ClientOpts = [{session_tickets, enabled}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ServerOpts = [{session_tickets, ServerTicketMode}, {log_level, debug},
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

    ct:pal("Received tickets: ~p~n", [Tickets0]),

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

    ct:pal("Received tickets: ~p~n", [Tickets1]),

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


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
