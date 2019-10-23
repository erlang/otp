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
    [{'tlsv1.3', [], session_tests()}].

session_tests() ->
    [erlang_client_erlang_server_basic,
     erlang_client_openssl_server_basic,
     openssl_client_erlang_server_basic,
     erlang_client_erlang_server_hrr,
     erlang_client_openssl_server_hrr,
     openssl_client_erlang_server_hrr,
     erlang_client_erlang_server_multiple_tickets,
     erlang_client_openssl_server_hrr_multiple_tickets].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
            Config = ssl_test_lib:make_rsa_cert(Config0),
            ssl_test_lib:make_dsa_cert(Config)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

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
    %% Prototype
    ets:new(tls13_session_ticket_db, [public, named_table, ordered_set]),
    ets:new(tls13_server_state, [public, named_table, ordered_set]),
    Config.

end_per_testcase(_TestCase, Config) ->
    ets:delete(tls13_session_ticket_db),
    ets:delete(tls13_server_state),
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

    %% Configure session tickets
    ClientOpts = [{session_tickets, true}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ServerOpts = [{session_tickets, stateless}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                         {from, self()},  {options, ClientOpts}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! listen,

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    TicketId = ets:last(tls13_session_ticket_db),  %% Prototype

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                         {from, self()},
                                         {options, [{use_ticket, [TicketId]}|ClientOpts]}]),
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
    ClientOpts = [{session_tickets, true}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],

    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port), ssl_test_lib:version_flag(Version),
            "-cert", CertFile,"-key", KeyFile, "-CAfile", CACertFile, "-msg", "-debug"],

    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),

    ssl_test_lib:wait_for_openssl_server(Port,  proplists:get_value(protocol, Config)),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},  {options, ClientOpts}]),

    SID = receive
              {Client0, Id0} ->
                  Id0
          end,

    %% Wait for session ticket
    ct:sleep(100),

    TicketId = ets:last(tls13_session_ticket_db),  %% Prototype
    ct:pal("TicketId = ~p~n", [TicketId]),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},
                                         {options, [{use_ticket, [TicketId]}|ClientOpts]}]),

    receive
        {Client1, SID} ->
            ok
    after ?SLEEP ->
              ct:fail(session_not_reused)
    end,

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

    Data = "From openssl to erlang",

    %% Configure session tickets
    ServerOpts = [{session_tickets, stateless}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, active_recv, [length(Data)]}},
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

    ssl_test_lib:check_result(Server0, Data),

    Server0 ! listen,

    %% Wait for session ticket
    ct:sleep(100),

    Args1 = ["s_client", "-connect", ssl_test_lib:hostname_format(Hostname)
            ++ ":" ++ integer_to_list(Port0),
            ssl_test_lib:version_flag(Version),
            "-sess_in", TicketFile0,
            "-sess_out", TicketFile1],

    OpenSslPort1 =  ssl_test_lib:portable_open_port(Exe, Args1),

    true = port_command(OpenSslPort1, Data),

    ssl_test_lib:check_result(Server0, Data),

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

    %% Configure session tickets
    ClientOpts = [{session_tickets, true}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups,[secp256r1, x25519]}|ClientOpts0],
    ServerOpts = [{session_tickets, stateless}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups, [x448, x25519]}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                         {from, self()},  {options, ClientOpts}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! listen,

    %% Wait for session ticket
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    TicketId = ets:last(tls13_session_ticket_db),  %% Prototype

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                         {from, self()},
                                         {options, [{use_ticket, [TicketId]}|ClientOpts]}]),
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
    ClientOpts = [{session_tickets, true}, {log_level, debug},
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
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},  {options, ClientOpts}]),

    SID = receive
              {Client0, Id0} ->
                  Id0
          end,

    %% Wait for session ticket
    ct:sleep(100),

    TicketId = ets:last(tls13_session_ticket_db),  %% Prototype
    ct:pal("TicketId = ~p~n", [TicketId]),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},
                                         {options, [{use_ticket, [TicketId]}|ClientOpts]}]),

    receive
        {Client1, SID} ->
            ok
    after ?SLEEP ->
              ct:fail(session_not_reused)
    end,

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

    Data = "From openssl to erlang",

    %% Configure session tickets
    ServerOpts = [{session_tickets, stateless}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups,[x448, x25519]}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, active_recv, [length(Data)]}},
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

    ssl_test_lib:check_result(Server0, Data),

    Server0 ! listen,

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

    ssl_test_lib:check_result(Server0, Data),

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

    %% Configure session tickets
    ClientOpts = [{session_tickets, true}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ServerOpts = [{session_tickets, stateless}, {log_level, debug},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                         {from, self()},  {options, ClientOpts}]),
    ssl_test_lib:check_result(Server0, ok, Client0, ok),

    Server0 ! listen,

    %% Wait for session tickets
    ct:sleep(100),

    ssl_test_lib:close(Client0),

    TicketId0 = ets:first(tls13_session_ticket_db),  %% Prototype
    TicketId1 = ets:last(tls13_session_ticket_db),  %% Prototype

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                         {from, self()},
                                         {options, [{use_ticket, [TicketId0, TicketId1]}|ClientOpts]}]),
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
    ClientOpts = [{session_tickets, true}, {log_level, debug},
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
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},  {options, ClientOpts}]),

    SID = receive
              {Client0, Id0} ->
                  Id0
          end,

    %% Wait for session ticket
    ct:sleep(100),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Store ticket from second connection
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},  {options, ClientOpts}]),

    SID = receive
              {Client1, Id1} ->
                  Id1
          end,

    %% Wait for session ticket
    ct:sleep(100),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client1),

    TicketId0 = ets:first(tls13_session_ticket_db),  %% Prototype
    TicketId1 = ets:last(tls13_session_ticket_db),  %% Prototype

    %% Use ticket
    Client2 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},
                                         {options, [{use_ticket, [TicketId0, TicketId1]}|ClientOpts]}]),

    receive
        {Client2, SID} ->
            ok
    after ?SLEEP ->
              ct:fail(session_not_reused)
    end,

    process_flag(trap_exit, false),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client2).


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
