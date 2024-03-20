%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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

-module(openssl_session_ticket_SUITE).

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
-export([openssl_server_basic/0,
         openssl_server_basic/1,
         openssl_server_hrr/0,
         openssl_server_hrr/1,
         openssl_server_hrr_multiple_tickets/0,
         openssl_server_hrr_multiple_tickets/1,
         openssl_client_basic/0,
         openssl_client_basic/1,
         openssl_client_hrr/0,
         openssl_client_hrr/1,
         openssl_server_early_data_basic/0,
         openssl_server_early_data_basic/1,
         openssl_server_early_data_big/0,
         openssl_server_early_data_big/1,
         openssl_server_early_data_manual/0,
         openssl_server_early_data_manual/1,
         openssl_server_early_data_manual_big/0,
         openssl_server_early_data_manual_big/1,
         openssl_server_early_data_manual_2_tickets/0,
         openssl_server_early_data_manual_2_tickets/1,
         openssl_server_early_data_manual_2_chacha_tickets/0,
         openssl_server_early_data_manual_2_chacha_tickets/1,
         openssl_client_early_data_basic/0,
         openssl_client_early_data_basic/1]).

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
                      {group, openssl_server}]},
     {openssl_server, [], [openssl_server_basic,
                           openssl_server_hrr,
                           openssl_server_hrr_multiple_tickets,
                           openssl_server_early_data_basic,
                           openssl_server_early_data_big,
                           openssl_server_early_data_manual,
                           openssl_server_early_data_manual_big,
                           openssl_server_early_data_manual_2_tickets,
                           openssl_server_early_data_manual_2_chacha_tickets
                          ]},
     {stateful, [], session_tests()},
     {stateless, [], session_tests()}].

session_tests() ->
    [openssl_client_basic,
     openssl_client_hrr,
     openssl_client_early_data_basic].

init_per_suite(Config0) ->
    Config = ssl_test_lib:init_per_suite(Config0, openssl),
    ssl_test_lib:make_rsa_cert(Config).

end_per_suite(Config) ->
    ssl_test_lib:end_per_suite(Config).

init_per_group(stateful, Config) ->
    [{server_ticket_mode, stateful} | proplists:delete(server_ticket_mode, Config)];
init_per_group(stateless, Config) ->
    [{server_ticket_mode, stateless} | proplists:delete(server_ticket_mode, Config)];
init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group_openssl(GroupName, Config).

end_per_group(GroupName, Config) ->
  ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(_TestCase, Config) ->
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

openssl_server_basic() ->
    [{doc,"Test session resumption with session tickets (erlang client - openssl server)"}].
openssl_server_basic(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    %% Configure session tickets
    ClientOpts = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],

    Server = ssl_test_lib:start_server(openssl, [], 
                                       [{server_opts, ServerOpts} | Config]),
    
    Port = ssl_test_lib:inet_port(Server),

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
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client1).

openssl_client_basic() ->
    [{doc,"Test session resumption with session tickets (openssl client - erlang server)"}].
openssl_client_basic(Config) when is_list(Config) ->
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = proplists:get_value(client_rsa_opts, Config),

    {_, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),
    TicketFile0 = filename:join([proplists:get_value(priv_dir, Config), "session_ticket0"]),
    TicketFile1 = filename:join([proplists:get_value(priv_dir, Config), "session_ticket1"]),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    Data = "Hello world",

    %% Configure session tickets
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

    Client0 = ssl_test_lib:start_client(openssl, [{port, Port0}, 
                                                  {options, ClientOpts},
                                                  {session_args, ["-sess_out", TicketFile0]}], Config),

    ssl_test_lib:send(Client0, Data),

    ssl_test_lib:check_result(Server0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                               verify_active_session_resumption,
                              [true]}}},
    ssl_test_lib:close(Client0),
    %% %% Wait for session ticket
    ct:sleep(100),
    
    Client1 = ssl_test_lib:start_client(openssl, [{port, Port0},
                                                  {options, ClientOpts},
                                                  {session_args, ["-sess_in", TicketFile0,
                                                                  "-sess_out", TicketFile1]}], Config),
    

    ssl_test_lib:send(Client1, Data),
    ssl_test_lib:check_result(Server0, ok),
    ssl_test_lib:close(Server0),    
    ssl_test_lib:close(Client1).

openssl_server_hrr() ->
    [{doc,"Test session resumption with session tickets and hello_retry_request (erlang client - openssl server)"}].
openssl_server_hrr(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    %% Configure session tickets
    ClientOpts = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups,[secp256r1, x25519]}|ClientOpts0],

    
    Server = ssl_test_lib:start_server(openssl, [{groups, "X448:X25519"}], 
                                       [{server_opts, ServerOpts} | Config]),
    
    Port = ssl_test_lib:inet_port(Server),
  

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
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client1).

openssl_client_hrr() ->
    [{doc,"Test session resumption with session tickets and hello_retry_request (openssl client - erlang server)"}].
openssl_client_hrr(Config) when is_list(Config) ->
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = proplists:get_value(client_rsa_opts, Config),
    {_, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),
    TicketFile0 = filename:join([proplists:get_value(priv_dir, Config), "session_ticket0"]),
    TicketFile1 = filename:join([proplists:get_value(priv_dir, Config), "session_ticket1"]),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),

    Data = "Hello world",

    %% Configure session tickets
    ServerOpts = [{session_tickets, ServerTicketMode},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups,[x448, x25519]}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),

    Port0 = ssl_test_lib:inet_port(Server0),
    

    Client0 = ssl_test_lib:start_client(openssl, [{port, Port0}, 
                                                  {options, ClientOpts},
                                                  {groups,  "P-256:X25519"},
                                                  {session_args, ["-sess_out", TicketFile0]}], Config),

    ssl_test_lib:send(Client0, Data),

    ssl_test_lib:check_result(Server0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_active_session_resumption,
                              [true]}}},

    %% Wait for session ticket
    ssl_test_lib:close(Client0),
    ct:sleep(100),

    Client1 = ssl_test_lib:start_client(openssl, [{port, Port0}, 
                                                  {options, ClientOpts},
                                                  {groups,  "P-256:X25519"},
                                                  {session_args, ["-sess_in", TicketFile0,
                                                                  "-sess_out", TicketFile1]}], Config),
    ssl_test_lib:send(Client1, Data),

    ssl_test_lib:check_result(Server0, ok),

    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client1).

openssl_server_hrr_multiple_tickets() ->
    [{doc,"Test session resumption with multiple session tickets and hello_retry_request "
      "(erlang client - openssl server)"}].
openssl_server_hrr_multiple_tickets(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    %% Configure session tickets
    ClientOpts = [{session_tickets, manual},
                  {versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups,[secp256r1, x25519]}|ClientOpts0],


    Server = ssl_test_lib:start_server(openssl, [{groups, "X448:X25519"}], 
                                       [{server_opts, ServerOpts} | Config]),
    
    Port = ssl_test_lib:inet_port(Server),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, no_reply, {tickets, 2}]}},
                                         {from, self()},  {options, ClientOpts}]),

    Tickets0 = ssl_test_lib:check_tickets(Client0),

    ?CT_LOG("Received tickets: ~p~n", [Tickets0]),

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

    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Server).

openssl_server_early_data_basic() ->
    [{doc,"Test early data (erlang client - openssl server)"}].
openssl_server_early_data_basic(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ClientOpts2 = [{early_data, <<"SampleData">>}|ClientOpts1],

    Server = ssl_test_lib:start_server(openssl, [{early_data, 16384}],
                                       [{server_opts, ServerOpts} | Config]),

    Port = ssl_test_lib:inet_port(Server),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, no_reply]}},
                                         {from, self()},  {options, ClientOpts1}]),
    %% Wait for session ticket
    ct:sleep(100),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Use ticket
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [true, no_reply, no_tickets,
                                                 {verify_early_data, accepted}]}},
                                         {from, self()},
                                         {options, ClientOpts2}]),
    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Server).

openssl_server_early_data_big() ->
    [{doc,"Send more early data than the max_early_data_size (erlang client - openssl server)"}].
openssl_server_early_data_big(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, auto},
                  {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ClientOpts2 = [{early_data, <<"SampleData">>}|ClientOpts1],

    Server = ssl_test_lib:start_server(openssl, [{early_data, 5}],
                                       [{server_opts, ServerOpts} | Config]),

    Port = ssl_test_lib:inet_port(Server),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, no_reply]}},
                                         {from, self()},  {options, ClientOpts1}]),
    %% Wait for session ticket
    ct:sleep(100),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Use ticket
    %% The tickets received cannot be used for sending more early data than the
    %% max_early_data_size. They are filtered by the automatic ticket handling
    %% mechanism and there will be no session resumption.
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, no_reply, no_tickets]}},
                                         {from, self()},
                                         {options, ClientOpts2}]),
    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Server).

openssl_server_early_data_manual() ->
    [{doc,"Test sending early data - manual ticket handling (erlang client - openssl server)"}].
openssl_server_early_data_manual(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, manual},
                   {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ClientOpts2 = [{early_data, <<"SampleData">>}|ClientOpts1],

    Server = ssl_test_lib:start_server(openssl, [{early_data, 16384}],
                                       [{server_opts, ServerOpts} | Config]),

    Port = ssl_test_lib:inet_port(Server),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, no_reply, {tickets, 1}]}},
                                         {from, self()},  {options, ClientOpts1}]),

    Tickets0 = ssl_test_lib:check_tickets(Client0),

    ?CT_LOG("Received tickets: ~p~n", [Tickets0]),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Use tickets
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [true, no_reply, no_tickets,
                                                 {verify_early_data, accepted}]}},
                                         {from, self()},
                                         {options, [{use_ticket, Tickets0}|ClientOpts2]}]),

    process_flag(trap_exit, false),

    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Server).

openssl_server_early_data_manual_big() ->
    [{doc,"Test sending more early data than the max_early_data_size - manual ticket handling "
      "(erlang client - openssl server)"}].
openssl_server_early_data_manual_big(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, manual},
                   {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ClientOpts2 = [{early_data, <<"SampleData">>}|ClientOpts1],

    Server = ssl_test_lib:start_server(openssl, [{early_data, 5}],
                                       [{server_opts, ServerOpts} | Config]),

    Port = ssl_test_lib:inet_port(Server),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, no_reply, {tickets, 1}]}},
                                         {from, self()},  {options, ClientOpts1}]),

    Tickets0 = ssl_test_lib:check_tickets(Client0),

    ?CT_LOG("Received tickets: ~p~n", [Tickets0]),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Use tickets
    Client1 = ssl_test_lib:start_client_error([{node, ClientNode},
                                               {port, Port}, {host, Hostname},
                                               {mfa, {ssl_test_lib,
                                                      verify_active_session_resumption,
                                                      [true, no_reply, no_tickets]}},
                                               {from, self()},
                                               {options, [{use_ticket, Tickets0}|ClientOpts2]}]),
    ssl_test_lib:check_client_alert(Client1, illegal_parameter),
    process_flag(trap_exit, false),

    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Server).

openssl_server_early_data_manual_2_tickets() ->
    [{doc,"Test sending early data - manual ticket handling, 2 tickets (erlang client - openssl server)"}].
openssl_server_early_data_manual_2_tickets(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, manual},
                   {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ClientOpts2 = [{early_data, <<"SampleData">>}|ClientOpts1],

    Server = ssl_test_lib:start_server(openssl, [{early_data, 16384}],
                                       [{server_opts, ServerOpts} | Config]),

    Port = ssl_test_lib:inet_port(Server),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, no_reply, {tickets, 2}]}},
                                         {from, self()},  {options, ClientOpts1}]),

    Tickets0 = ssl_test_lib:check_tickets(Client0),

    ?CT_LOG("Received tickets: ~p~n", [Tickets0]),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Use tickets
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [true, no_reply, no_tickets,
                                                 {verify_early_data, accepted}]}},
                                         {from, self()},
                                         {options, [{use_ticket, Tickets0}|ClientOpts2]}]),
    process_flag(trap_exit, false),

    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Server).

openssl_server_early_data_manual_2_chacha_tickets() ->
    [{doc,"Test sending early data - manual ticket handling, 2 tickets - chacha (erlang client - openssl server)"}].
openssl_server_early_data_manual_2_chacha_tickets(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    %% Configure session tickets
    ClientOpts1 = [{session_tickets, manual},
                   {ciphers, ["TLS_CHACHA20_POLY1305_SHA256"]},
                   {versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ClientOpts2 = [{early_data, <<"SampleData">>}|ClientOpts1],

    %% openssl s_server seems to select a cipher_suite that satisfies the requirements
    %% for early_data.
    Server = ssl_test_lib:start_server(openssl, [{early_data, 16384}],
                                       [{server_opts, ServerOpts} | Config]),

    Port = ssl_test_lib:inet_port(Server),

    %% Store ticket from first connection
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [false, no_reply, {tickets, 2}]}},
                                         {from, self()},  {options, ClientOpts1}]),

    %% Receive 2 tickets that used Chacha20-Poly1305 and sha256
    Tickets0 = ssl_test_lib:check_tickets(Client0),

    ?CT_LOG("Received tickets: ~p~n", [Tickets0]),

    %% Close previous connection as s_server can only handle one at a time
    ssl_test_lib:close(Client0),

    %% Use tickets
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib,
                                                verify_active_session_resumption,
                                                [true, no_reply, no_tickets,
                                                 {verify_early_data, accepted}]}},
                                         {from, self()},
                                         {options, [{use_ticket, Tickets0}|ClientOpts2]}]),
    process_flag(trap_exit, false),

    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Server).

openssl_client_early_data_basic() ->
    [{doc,"Test early data (openssl client - erlang server)"}].
openssl_client_early_data_basic(Config) when is_list(Config) ->
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = proplists:get_value(client_rsa_opts, Config),

    {_, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),
    TicketFile0 = filename:join([proplists:get_value(priv_dir, Config), "session_ticket0"]),
    TicketFile1 = filename:join([proplists:get_value(priv_dir, Config), "session_ticket1"]),
    RequestFile = filename:join([proplists:get_value(priv_dir, Config), "request"]),
    ServerTicketMode = proplists:get_value(server_ticket_mode, Config),
    %% Create request file to be used with early data
    EarlyData = <<"HEAD / HTTP/1.1\nHost: \nConnection: close\n\n", 0, 0>>,
    create_request(RequestFile, EarlyData),

    %% Configure session tickets
    ServerOpts = [{session_tickets, ServerTicketMode},
                  {early_data, enabled},
                  {versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],

    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib,
                                          verify_active_session_resumption,
                                          [false]}},
				   {options, ServerOpts}]),

    Port0 = ssl_test_lib:inet_port(Server0),

    Client0 = ssl_test_lib:start_client(openssl, [{port, Port0},
                                                  {options, ClientOpts},
                                                  {session_args, ["-sess_out", TicketFile0]}], Config),
    ssl_test_lib:check_result(Server0, ok),

    Server0 ! {listen, {mfa, {ssl_test_lib,
                              verify_server_early_data,
                              [wait_reply, EarlyData]}}},

    %% %% Wait for session ticket
    ct:sleep(100),
    ssl_test_lib:close(Client0),

    Client1 = ssl_test_lib:start_client(openssl, [{port, Port0},
                                                  {options, ClientOpts},
                                                  {session_args, ["-sess_in", TicketFile0,
                                                                  "-sess_out", TicketFile1,
                                                                  "-early_data", RequestFile]}],
                                        Config),

    ssl_test_lib:check_result(Server0, ok),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

create_request(File, EarlyData) ->
    {ok, S} = file:open(File, [write]),
    io:format(S, "~s", [binary_to_list(EarlyData)]),
    file:close(S).

