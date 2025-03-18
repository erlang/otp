%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2025. All Rights Reserved.
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
-module(tls_api_SUITE).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("ssl/src/ssl_record.hrl").
-include_lib("ssl/src/ssl_internal.hrl").
-include_lib("ssl/src/ssl_api.hrl").
-include_lib("ssl/src/ssl_connection.hrl").
-include_lib("ssl/src/ssl_alert.hrl").
-include_lib("ssl/src/ssl_cipher.hrl").

%% Common test
-export([all/0,
         groups/0,
         init_per_suite/1,
         init_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,
         end_per_suite/1,
         end_per_group/2
        ]).

%% Test cases
-export([tls_upgrade/0,
         tls_upgrade/1,
         tls_upgrade_new_opts/0,
         tls_upgrade_new_opts/1,
         tls_upgrade_new_opts_with_sni_fun/0,
         tls_upgrade_new_opts_with_sni_fun/1,
         tls_upgrade_with_timeout/0,
         tls_upgrade_with_timeout/1,
         tls_upgrade_with_client_timeout/0,
         tls_upgrade_with_client_timeout/1,
         tls_downgrade/0,
         tls_downgrade/1,
         tls_shutdown/0,
         tls_shutdown/1,
         tls_shutdown_write/0,
         tls_shutdown_write/1,
         tls_shutdown_both/0,
         tls_shutdown_both/1,
         tls_shutdown_error/0,
         tls_shutdown_error/1,
         tls_sup_shutdown/0,
         tls_sup_shutdown/1,
         tls_client_closes_socket/0,
         tls_client_closes_socket/1,
         tls_closed_in_active_once/0,
         tls_closed_in_active_once/1,
         tls_password_incorrect/0,
         tls_password_incorrect/1,
         tls_password_correct/0,
         tls_password_correct/1,
         tls_password_badarg/0,
         tls_password_badarg/1,
         tls_reset_in_active_once/0,
         tls_reset_in_active_once/1,
         tls_monitor_listener/0,
         tls_monitor_listener/1,
         tls_tcp_msg/0,
         tls_tcp_msg/1,
         tls_tcp_msg_big/0,
         tls_tcp_msg_big/1,
         tls_dont_crash_on_handshake_garbage/0,
         tls_dont_crash_on_handshake_garbage/1,
         tls_tcp_error_propagation_in_active_mode/0,
         tls_tcp_error_propagation_in_active_mode/1,
         tls_reject_warning_alert_in_initial_hs/0,
         tls_reject_warning_alert_in_initial_hs/1,
         tls_reject_fake_warning_alert_in_initial_hs/0,
         tls_reject_fake_warning_alert_in_initial_hs/1,
         tls_app_data_in_initial_hs_state/0,
         tls_app_data_in_initial_hs_state/1,
         tls_13_reject_change_cipher_spec_as_first_msg/0,
         tls_13_reject_change_cipher_spec_as_first_msg/1,
         tls_13_middlebox_reject_change_cipher_spec_as_first_msg/0,
         tls_13_middlebox_reject_change_cipher_spec_as_first_msg/1,
         peername/0,
         peername/1,
         sockname/0,
         sockname/1,
         tls_server_handshake_timeout/0,
         tls_server_handshake_timeout/1,
         transport_close/0,
         transport_close/1,
         transport_close_in_inital_hello/0,
         transport_close_in_inital_hello/1,
         emulated_options/0,
         emulated_options/1,
         accept_pool/0,
         accept_pool/1,
         reuseaddr/0,
         reuseaddr/1,
         signature_algs/0,
         signature_algs/1
        ]).

%% Apply export
-export([upgrade_result/1,
         upgrade_result_new_opts/1,
         tls_downgrade_result/2,
         tls_shutdown_result/2,
         tls_shutdown_write_result/2,
         tls_shutdown_both_result/2,
         tls_socket_options_result/5,
         receive_msg/1
        ]).

-define(SLEEP, 500).
-define(CORRECT_PASSWORD, "hello test").
-define(INCORRECT_PASSWORD, "hello").
-define(BADARG_PASSWORD, hello).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [
     {group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, transport_socket}
    ].

groups() ->
    [
     {'tlsv1.3', [], [{group, api_tests},
                      tls_13_reject_change_cipher_spec_as_first_msg,
                      tls_13_middlebox_reject_change_cipher_spec_as_first_msg
                     ] ++ seq_test()},
     {'tlsv1.2', [], [{group, api_tests}] ++ seq_test()},
     {'tlsv1.1', [], [{group, api_tests}] ++ seq_test()},
     {'tlsv1', [],  [{group, api_tests}] ++ seq_test()},
     {transport_socket, [], [{group, api_tests}] ++ seq_test()},
     {api_tests, [parallel], api_tests()}
    ].

api_tests() ->
    [
     tls_upgrade,
     tls_upgrade_new_opts,
     tls_upgrade_new_opts_with_sni_fun,
     tls_upgrade_with_timeout,
     tls_upgrade_with_client_timeout,
     tls_downgrade,
     tls_shutdown,
     tls_shutdown_write,
     tls_shutdown_both,
     tls_shutdown_error,
     tls_sup_shutdown,
     tls_password_correct,
     tls_password_incorrect,
     tls_password_badarg,
     tls_client_closes_socket,
     tls_closed_in_active_once,
     tls_reset_in_active_once,
     tls_tcp_msg,
     tls_tcp_msg_big,
     tls_dont_crash_on_handshake_garbage,
     tls_tcp_error_propagation_in_active_mode,
     tls_reject_warning_alert_in_initial_hs,
     tls_reject_fake_warning_alert_in_initial_hs,
     tls_app_data_in_initial_hs_state,
     peername,
     sockname,
     tls_server_handshake_timeout,
     transport_close,
     emulated_options,
     accept_pool,
     reuseaddr
    ].

seq_test() ->
    [transport_close_in_inital_hello,tls_monitor_listener].


init_per_suite(Config0) ->
    catch application:stop(crypto),
    try application:start(crypto) of
	ok ->
	    ssl_test_lib:clean_start(),
	    Config1 = ssl_test_lib:make_rsa_cert_with_protected_keyfile(Config0,
                                                                        ?CORRECT_PASSWORD),
            ssl_test_lib:make_ecdsa_cert(Config1)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:unload(ssl),
    application:stop(crypto).

init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group(GroupName, Config).

end_per_group(GroupName, Config) ->
  ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(Testcase, Config) when Testcase == tls_server_handshake_timeout;
                                         Testcase == tls_upgrade_with_timeout ->
    ct:timetrap({seconds, 10}),
    Config;
init_per_testcase(_, Config) ->
    ct:timetrap({seconds, 5}),
    Config.
end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
tls_upgrade() ->
    [{doc,"Test that you can upgrade an tcp connection to an ssl connection"}].

tls_upgrade(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server([{node, ServerNode}, {port, 0},
						{from, self()},
						{mfa, {?MODULE,
						       upgrade_result, []}},
                                                {transport, proplists:get_value(transport, Config)},
                                                {tcp_options,[{active, false} | TcpOpts]},
						{ssl_options, [{verify, verify_peer} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_upgrade_client([{node, ClientNode},
						{port, Port},
                                                {host, Hostname},
                                                {from, self()},
                                                {mfa, {?MODULE, upgrade_result, []}},
                                                {transport, proplists:get_value(transport, Config)},
                                                {tcp_options, [binary]},
                                                {ssl_options,  [{verify, verify_peer},
                                                                {server_name_indication, Hostname} | ClientOpts]}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tls_upgrade_new_opts() ->
    [{doc,"Test that you can upgrade an tcp connection to an ssl connection and give new socket opts"}].

tls_upgrade_new_opts(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server([{node, ServerNode}, {port, 0},
						{from, self()},
						{mfa, {?MODULE,upgrade_result_new_opts, []}},
						{tcp_options,[{active, false} | TcpOpts]},
                                                {transport, proplists:get_value(transport, Config)},
						{ssl_options, [{verify, verify_peer},
                                                               {mode, list} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_upgrade_client([{node, ClientNode},
						{port, Port},
                                                {host, Hostname},
                                                {from, self()},
                                                {mfa, {?MODULE, upgrade_result_new_opts, []}},
                                                {tcp_options, [binary]},
                                                {transport, proplists:get_value(transport, Config)},
                                                {ssl_options,  [{verify, verify_peer},
                                                                {mode, list},
                                                                {server_name_indication, Hostname} | ClientOpts]}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tls_upgrade_new_opts_with_sni_fun() ->
    [{doc,"Test that you can upgrade an tcp connection to an ssl connection with new versions option provided by sni_fun"}].

tls_upgrade_new_opts_with_sni_fun(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerEcdsaOpts = ssl_test_lib:ssl_options(server_ecdsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],
    Version = ssl_test_lib:protocol_version(Config),
    NewVersions = new_versions(Version),
    Ciphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, Version),
                                       [{key_exchange, fun(srp_rsa) -> false;
                                                          (srp_dss) -> false;
                                                          (_) -> true
                                                       end}]),

    NewOpts = [{versions, NewVersions},
               {ciphers, Ciphers},
               {verify, verify_peer}],

    Server = ssl_test_lib:start_upgrade_server(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {mfa, {?MODULE, upgrade_result, []}},
                {tcp_options,[{active, false} | TcpOpts]},
                {transport, proplists:get_value(transport, Config)},
                {ssl_options, [{versions, [Version |NewVersions]},
                               {sni_fun, fun(_SNI) -> ServerOpts ++ NewOpts end}
                              | proplists:delete(versions, ServerEcdsaOpts)]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_upgrade_client(
               [{node, ClientNode},
                {port, Port},
                {host, Hostname},
                {from, self()},
                {mfa, {?MODULE, upgrade_result, []}},
                {tcp_options, [binary]},
                {transport, proplists:get_value(transport, Config)},
                {ssl_options,  [{versions,  [Version |NewVersions]},
                                {ciphers, Ciphers},
                                {server_name_indication, Hostname} |
                                proplists:delete(versions, ClientOpts)]}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).



%%--------------------------------------------------------------------
tls_upgrade_with_timeout() ->
    [{doc,"Test handshake/3"}].

tls_upgrade_with_timeout(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server([{node, ServerNode}, {port, 0},
						{from, self()},
						{timeout, 5000},
						{mfa, {?MODULE, upgrade_result, []}},
						{tcp_options, [{active, false} | TcpOpts]},
                                                {transport, proplists:get_value(transport, Config)},
						{ssl_options, [{verify, verify_peer} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_upgrade_client([{node, ClientNode},
						{port, Port},
						{host, Hostname},
						{from, self()},
						{mfa, {?MODULE, upgrade_result, []}},
						{tcp_options, TcpOpts},
                                                {transport, proplists:get_value(transport, Config)},
						{ssl_options, [{verify, verify_peer},
                                                               {server_name_indication, Hostname}
                                                              | ClientOpts]}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

tls_upgrade_with_client_timeout() ->
    [{doc,"Test upgrade with connect/3 and a timeout value"}].

tls_upgrade_with_client_timeout(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {mfa, {?MODULE, upgrade_result, []}},
                {tcp_options, [{active, false} | TcpOpts]},
                {transport, proplists:get_value(transport, Config)},
                {ssl_options, [{verify, verify_peer} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_upgrade_client_error(
               [{node, ClientNode},
                {port, Port},
                {host, Hostname},
                {from, self()},
                {timeout, 0},
                {mfa, {?MODULE, upgrade_result, []}},
                {tcp_options, [binary]},
                {transport, proplists:get_value(transport, Config)},
                {ssl_options,  [{verify, verify_peer},
                                {server_name_indication, Hostname} | ClientOpts]}]),

    ?CT_LOG("Client ~p  Server ~p", [Client, Server]),
    ok = ssl_test_lib:check_result(Client, {error, timeout}),
    ssl_test_lib:close(Server).

%%--------------------------------------------------------------------
tls_downgrade() ->
      [{doc,"Test that you can downgarde an ssl connection to an tcp connection"}].
tls_downgrade(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, tls_downgrade_result, [self()]}},
					{options, [{active, false}, {verify, verify_peer} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, tls_downgrade_result, [self()]}},
					{options, [{active, false}, {verify, verify_peer} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ready, Client, ready),

    Server ! go,
    Client ! go,

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
tls_shutdown() ->
    [{doc,"Test API function ssl:shutdown/2"}].
tls_shutdown(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {?MODULE, tls_shutdown_result, [server]}},
                                        {options, [{exit_on_close, false},
                                                   {active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa,
					 {?MODULE, tls_shutdown_result, [client]}},
					{options,
					 [{exit_on_close, false},
					  {active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tls_shutdown_write() ->
    [{doc,"Test API function ssl:shutdown/2 with option write."}].
tls_shutdown_write(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, tls_shutdown_write_result, [server]}},
			   {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {?MODULE, tls_shutdown_write_result, [client]}},
			   {options, [{active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, {error, closed}).

%%--------------------------------------------------------------------
tls_shutdown_both() ->
    [{doc,"Test API function ssl:shutdown/2 with option both."}].
tls_shutdown_both(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, tls_shutdown_both_result, [server]}},
			   {options, [{active, false} | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {?MODULE, tls_shutdown_both_result, [client]}},
			   {options, [{active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, {error, closed}).

%%--------------------------------------------------------------------
tls_shutdown_error() ->
    [{doc,"Test ssl:shutdown/2 error handling"}].
tls_shutdown_error(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    Port = ssl_test_lib:inet_port(node()),
    {ok, Listen} = ssl:listen(Port, ServerOpts),
    {error, enotconn} = ssl:shutdown(Listen, read_write),
    ok = ssl:close(Listen),
    {error, _} = ssl:shutdown(Listen, read_write).

%%--------------------------------------------------------------------
tls_client_closes_socket() ->
    [{doc,"Test what happens when client closes socket before handshake is completed"}].

tls_client_closes_socket(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server_error(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {tcp_options, TcpOpts},
                {transport, proplists:get_value(transport, Config)},
                {ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Connect = fun() ->
		      {ok, _Socket} = rpc:call(ClientNode, gen_tcp, connect,
                                               [Hostname, Port, [binary]]),
		      %% Make sure that handshake is called before
		      %% client process ends and closes socket.
		      ct:sleep(?SLEEP)
	      end,

    _Client = spawn_link(Connect),

    ssl_test_lib:check_result(Server, {error,closed}).

%%--------------------------------------------------------------------
tls_reset_in_active_once() ->
    [{doc, "Test that ssl_closed is delivered in active once with non-empty buffer, check ERL-420."}].

tls_reset_in_active_once(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],
    Port = ssl_test_lib:inet_port(node()),
    Server = fun() ->
                     case proplists:get_value(transport, Config) of
                         socket ->
                             {ok, Listen} = tls_socket_tcp:listen(Port, TcpOpts),
                             {ok, TcpServerSocket} = socket:accept(Listen),
                             {ok, ServerSocket} = ssl:handshake(TcpServerSocket, ServerOpts),
                             lists:foreach(
                               fun(_) ->
                                       ssl:send(ServerSocket, "some random message\r\n")
                               end, lists:seq(1, 20)),
                             %% Close TCP instead of SSL socket to trigger the bug:
                             socket:close(TcpServerSocket),
                             socket:close(Listen);
                         _ ->
                             {ok, Listen} = gen_tcp:listen(Port, TcpOpts),
                             {ok, TcpServerSocket} = gen_tcp:accept(Listen),
                             {ok, ServerSocket} = ssl:handshake(TcpServerSocket, ServerOpts),
                             lists:foreach(
                               fun(_) ->
                                       ssl:send(ServerSocket, "some random message\r\n")
                               end, lists:seq(1, 20)),
                             %% Close TCP instead of SSL socket to trigger the bug:
                             gen_tcp:close(TcpServerSocket),
                             gen_tcp:close(Listen)
                     end
	     end,
    spawn_link(Server),
    {ok, Socket} = ssl:connect(Hostname, Port, [{active, false} | ClientOpts]),
    Result = tls_closed_in_active_once_loop(Socket),
    ssl:close(Socket),
    case Result of
	ok -> ok;
	_ -> ct:fail(Result)
    end.

%%--------------------------------------------------------------------
tls_closed_in_active_once() ->
    [{doc, "Test that active once can be used to deliver not only all data"
      " but even the close message, see ERL-1409, in normal operation."
      " This is also test, with slightly different circumstances in"
      " the old tls_closed_in_active_once test"
      " renamed tls_reset_in_active_once"}].

tls_closed_in_active_once(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = proplists:delete(cb_info, ServerOpts0),
    {_ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],
    Port = ssl_test_lib:inet_port(node()),
    Server = fun() ->
		     {ok, Listen} = gen_tcp:listen(Port, TcpOpts),
		     {ok, TcpServerSocket} = gen_tcp:accept(Listen),
		     {ok, ServerSocket} = ssl:handshake(TcpServerSocket, ServerOpts),
		     lists:foreach(
		       fun(_) ->
			       ssl:send(ServerSocket, "some random message\r\n")
		       end, lists:seq(1, 20)),
		     ssl:close(ServerSocket)
	     end,
    spawn_link(Server),
    {ok, Socket} = ssl:connect(Hostname, Port, [{active, false} | ClientOpts]),
    Result = tls_closed_in_active_once_loop(Socket),
    ssl:close(Socket),
    case Result of
	ok -> ok;
	_ -> ct:fail(Result)
    end.

%%--------------------------------------------------------------------
tls_monitor_listener() ->
    [{doc, "Check that TLS server processes are shutdown when listener socket is closed."
      "Note that individual already established connections may live longer."}].

tls_monitor_listener(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    Version = ssl_test_lib:protocol_version(Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server1 = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                         {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                         {options, tls_monitor_listen_opts(Version, ServerOpts)}]),
    Port1 = ssl_test_lib:inet_port(Server1),

    Server2 = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                         {options, tls_monitor_listen_opts(Version, ServerOpts)}]),
    _Port2 = ssl_test_lib:inet_port(Server2),

    2 = count_children(workers, ssl_listen_tracker_sup),

    Sessions = session_info(Version),

    true = (Sessions == 2),

    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port1},
                                         {host, Hostname},
                                         {from, self()},
                                         {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                         {options, tls_monitor_client_opts(Version, ClientOpts)}
					]),

    ssl_test_lib:check_result(Server1, ok, Client1, ok),
    Monitor = erlang:monitor(process, Server1),
    ssl_test_lib:close(Server1),
    receive
        {'DOWN', Monitor, _, _, _} ->
            ct:sleep(1000)
    end,


    Sessions1 = session_info(Version),
    true = (Sessions1 == 1),

    1 = count_children(workers, ssl_listen_tracker_sup).

%%--------------------------------------------------------------------
tls_tcp_msg() ->
    [{doc,"Test what happens when a tcp tries to connect, i,e. a bad (ssl) packet is sent first"}].

tls_tcp_msg(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}, {active, false}],

    Server = ssl_test_lib:start_upgrade_server_error(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {timeout, 5000},
                {mfa, {?MODULE, dummy, []}},
                {transport, proplists:get_value(transport, Config)},
                {tcp_options, TcpOpts},
                {ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {packet, 0}]),
    ?CT_LOG("connected to Server ~p ~n", [Server]),
    gen_tcp:send(Socket, "<SOME GARBLED NON SSL MESSAGE>"),

    receive
	{tcp_closed, Socket} ->
	    receive
		{Server, {error, Error}} ->
		    ?CT_LOG("Error ~p", [Error])
	    end
    end.
%%--------------------------------------------------------------------
tls_tcp_msg_big() ->
    [{doc,"Test what happens when a tcp tries to connect, i,e. a bad big (ssl) packet is sent first"}].

tls_tcp_msg_big(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Rand = crypto:strong_rand_bytes(?MAX_CIPHER_TEXT_LENGTH+1),
    Server = ssl_test_lib:start_upgrade_server_error([{node, ServerNode}, {port, 0},
						      {from, self()},
						      {timeout, 5000},
						      {mfa, {?MODULE, dummy, []}},
						      {tcp_options, TcpOpts},
                                                      {transport, proplists:get_value(transport, Config)},
						      {ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {packet, 0}]),
    ?CT_LOG("connected to Server ~p ~n", [Server]),

    gen_tcp:send(Socket, <<?BYTE(0),
			   ?BYTE(3), ?BYTE(1), ?UINT16(?MAX_CIPHER_TEXT_LENGTH), Rand/binary>>),

    receive
	{tcp_closed, Socket} ->
	    receive
		{Server, {error, timeout}} ->
		    ct:fail("hangs");
		{Server, {error, Error}} ->
		    ?CT_LOG("Error ~p", [Error]);
		{'EXIT', Server, _} ->
		    ok
	    end
    end.

%%--------------------------------------------------------------------
tls_dont_crash_on_handshake_garbage() ->
    [{doc, "Ensure SSL server worker thows an alert on garbage during handshake "
      "instead of crashing and exposing state to user code"}].

tls_dont_crash_on_handshake_garbage(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    Version = ssl_test_lib:protocol_version(Config),
    {_ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Ciphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, Version),
                                       [{key_exchange, fun(srp_rsa) -> false;
                                                          (srp_dss) -> false;
                                                          (_) -> true
                                                       end}]),


    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, ssl_test_lib, no_result},
                                        {options, [{versions, [Version]}, {ciphers, Ciphers} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {active, false}]),

    %% Send hello and garbage record
    ok = gen_tcp:send(Socket,
                      [<<22, 3,3, 49:16, 1, 45:24, 3,3, % client_hello
                         16#deadbeef:256, % 32 'random' bytes = 256 bits
                         0, 6:16, 0,255, 0,61, 0,57, 1, 0 >>, % some hello values
                       <<22, 3,3, 5:16, 92,64,37,228,209>> % garbage
                      ]),
    %% Send unexpected change_cipher_spec
    ok = gen_tcp:send(Socket, <<20, 3,3, 12:16, 111,40,244,7,137,224,16,109,197,110,249,152>>),
    gen_tcp:close(Socket),
    % Ensure we receive an alert, not sudden disconnect
    case Version of
        'tlsv1.3' ->
            ssl_test_lib:check_server_alert(Server, protocol_version);
        _ ->
            ssl_test_lib:check_server_alert(Server, handshake_failure)
    end.

%%--------------------------------------------------------------------
tls_tcp_error_propagation_in_active_mode() ->
    [{doc,"Test that process receives {ssl_error, Socket, closed} when tcp error ocurres"}].
tls_tcp_error_propagation_in_active_mode(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					 {from, self()},
					 {mfa, {ssl_test_lib, no_result, []}},
					 {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {Client, SslSocket} = ssl_test_lib:start_client([return_socket,
                                                     {node, ClientNode}, {port, Port},
                                                     {host, Hostname},
                                                     {from, self()},
                                                     {mfa, {?MODULE, receive_msg, []}},
                                                     {options, ClientOpts}]),
    #sslsocket{connection_handler = Pid} = SslSocket,

    {status, _, _, StatusInfo} = sys:get_status(Pid),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    StaticEnv = State#state.static_env,
    Socket = StaticEnv#static_env.socket,
    %% Fake tcp error
    case proplists:get_value(transport, Config) of
        socket ->
            Pid ! {'$socket', Socket, abort, etimedout};
        _ ->
            Pid ! {tcp_error, Socket, etimedout}
    end,

    ssl_test_lib:check_result(Client, {ssl_closed, SslSocket}).

%%--------------------------------------------------------------------
tls_sup_shutdown() ->
    [{doc,"Test that terminate behaves correctly for exit(shutdown) as done by supervisor at application shutdown"}].
tls_sup_shutdown(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					 {from, self()},
					 {mfa, {?MODULE, receive_msg, []}},
					 {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {_, #sslsocket{connection_handler = Pid}} = ssl_test_lib:start_client([return_socket,
                                                                   {node, ClientNode}, {port, Port},
                                                                   {host, Hostname},
                                                                   {from, self()},
                                                                   {mfa, {ssl_test_lib, no_result, []}},
                                                                   {options, [{active, false} | ClientOpts]}]),
    exit(Pid, shutdown),

    receive
        {Server, {ssl_closed, _}} ->
            ok;
        Msg ->
          ct:fail(Msg)
    end.

%%--------------------------------------------------------------------
tls_reject_warning_alert_in_initial_hs() ->
    [{doc,"Test sending warning ALERT instead of client hello"}].
tls_reject_warning_alert_in_initial_hs(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_Clientnode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),
    {Major, Minor} = case ssl_test_lib:protocol_version(Config, tuple) of
                         ?TLS_1_3 ->
                             ?TLS_1_2;
                         Other ->
                             Other
                     end,

    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					 {from, self()},
					 {mfa, {ssl_test_lib, no_result, []}},
					 {options, [{versions, [ssl_test_lib:protocol_version(Config)]} |
                                                    proplists:delete(versions, ServerOpts)]}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, Socket}  = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
    NoRenegotiateAlert = <<?BYTE(?ALERT), ?BYTE(Major), ?BYTE(Minor), ?UINT16(2), ?BYTE(?WARNING), ?BYTE(?NO_RENEGOTIATION)>>,
    gen_tcp:send(Socket, NoRenegotiateAlert),
    UnexpectedMsgAlert = <<?BYTE(?ALERT), ?BYTE(Major), ?BYTE(Minor), ?UINT16(2), ?BYTE(?FATAL), ?BYTE(?UNEXPECTED_MESSAGE)>>,
    {ok, UnexpectedMsgAlert} = gen_tcp:recv(Socket, 7),
    {error, closed} = gen_tcp:recv(Socket, 0).

%%--------------------------------------------------------------------
tls_reject_fake_warning_alert_in_initial_hs() ->
    [{doc,"Test sending 'fake' warning ALERT covers different function clause pre TLS-1.3"}].
tls_reject_fake_warning_alert_in_initial_hs(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),
    {Major, Minor} = case ssl_test_lib:protocol_version(Config, tuple) of
                         ?TLS_1_3 ->
                             ?TLS_1_2;
                         Other ->
                             Other
                     end,
    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					 {from, self()},
					 {mfa, {ssl_test_lib, no_result, []}},
					 {options, [{versions, [ssl_test_lib:protocol_version(Config)]} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, Socket}  = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
    NoRenegotiateAlert = <<?BYTE(?ALERT), ?BYTE(Major), ?BYTE(Minor), ?UINT16(2), ?BYTE(?WARNING), ?BYTE(?UNEXPECTED_MESSAGE)>>,
    gen_tcp:send(Socket, NoRenegotiateAlert),
    UnexpectedMsgAlert = <<?BYTE(?ALERT), ?BYTE(Major), ?BYTE(Minor), ?UINT16(2), ?BYTE(?FATAL), ?BYTE(?UNEXPECTED_MESSAGE)>>,
    {ok, UnexpectedMsgAlert} = gen_tcp:recv(Socket, 7),
    {error, closed} = gen_tcp:recv(Socket, 0).

%%--------------------------------------------------------------------
tls_app_data_in_initial_hs_state() ->
     [{doc,"Test sending application data instead of initial client hello. In TLS-1.3 application data can be sent",
       "in first round trip but not before client hello."}].
tls_app_data_in_initial_hs_state(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),
    Version = ssl_test_lib:protocol_version(Config, tuple),
    {Major, Minor} = case Version of
                         ?TLS_1_3 ->
                             ?TLS_1_2;
                         Other ->
                             Other
                     end,
    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					 {from, self()},
					 {mfa, {ssl_test_lib, no_result, []}},
					 {options, [{versions, [ssl_test_lib:protocol_version(Config)]} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, Socket}  = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
    AppData = case Version of
                  ?TLS_1_3 ->
                      <<?BYTE(?APPLICATION_DATA), ?BYTE(3), ?BYTE(3), ?UINT16(4), ?BYTE($F), 
                        ?BYTE($O), ?BYTE($O), ?BYTE(?APPLICATION_DATA)>>;
                  _ ->
                     <<?BYTE(?APPLICATION_DATA), ?BYTE(Major), ?BYTE(Minor), 
                       ?UINT16(3), ?BYTE($F), ?BYTE($O), ?BYTE($O)>>
              end,
    gen_tcp:send(Socket, AppData),
    UnexpectedMsgAlert = <<?BYTE(?ALERT), ?BYTE(Major), ?BYTE(Minor), ?UINT16(2), 
                           ?BYTE(?FATAL), ?BYTE(?UNEXPECTED_MESSAGE)>>,
    {ok, UnexpectedMsgAlert} = gen_tcp:recv(Socket, 7),
    {error, closed} = gen_tcp:recv(Socket, 0).
%%--------------------------------------------------------------------
tls_13_reject_change_cipher_spec_as_first_msg() ->
     [{doc,"change_cipher_spec messages can be sent in TLS-1.3 middlebox_comp_mode, but can not be sent as first msg"}].
tls_13_reject_change_cipher_spec_as_first_msg(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),
    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                         {from, self()},
                                         {mfa, {ssl_test_lib, no_result, []}},
                                         {options, [{versions, [ssl_test_lib:protocol_version(Config)]} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, Socket}  = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
    ChangeCipherSpec = <<?BYTE(?CHANGE_CIPHER_SPEC), ?BYTE(3), ?BYTE(3),
                         ?UINT16(1), ?BYTE(?CHANGE_CIPHER_SPEC_PROTO)>>,
    gen_tcp:send(Socket, ChangeCipherSpec),
    UnexpectedMsgAlert = <<?BYTE(?ALERT), ?BYTE(3), ?BYTE(3), ?UINT16(2),
                           ?BYTE(?FATAL), ?BYTE(?UNEXPECTED_MESSAGE)>>,
    {ok, UnexpectedMsgAlert} = gen_tcp:recv(Socket, 7),
    {error, closed} = gen_tcp:recv(Socket, 0).

%%--------------------------------------------------------------------
tls_13_middlebox_reject_change_cipher_spec_as_first_msg() ->
     [{doc,"change_cipher_spec messages can be sent in TLS-1.3 middlebox_comp_mode, but can not be sent as first msg"}].
tls_13_middlebox_reject_change_cipher_spec_as_first_msg(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),
    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                         {from, self()},
                                         {mfa, {ssl_test_lib, no_result, []}},
                                         {options, [{middlebox_comp_mode, false},
                                                    {versions, [ssl_test_lib:protocol_version(Config)]}
                                                   | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, Socket}  = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
    ChangeCipherSpec = <<?BYTE(?CHANGE_CIPHER_SPEC), ?BYTE(3), ?BYTE(3),
                         ?UINT16(1), ?BYTE(?CHANGE_CIPHER_SPEC_PROTO)>>,
    gen_tcp:send(Socket, ChangeCipherSpec),
    UnexpectedMsgAlert = <<?BYTE(?ALERT), ?BYTE(3), ?BYTE(3), ?UINT16(2),
                           ?BYTE(?FATAL), ?BYTE(?UNEXPECTED_MESSAGE)>>,
    {ok, UnexpectedMsgAlert} = gen_tcp:recv(Socket, 7),
    {error, closed} = gen_tcp:recv(Socket, 0).
%%--------------------------------------------------------------------
peername() ->
    [{doc,"Test API function peername/1"}].

peername(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {ssl, peername, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl, peername, []}},
					{options, [{port, 0} | ClientOpts]}]),

    ClientPort = ssl_test_lib:inet_port(Client),
    ServerIp = ssl_test_lib:node_to_hostip(ServerNode, server),
    ClientIp = ssl_test_lib:node_to_hostip(ClientNode, client),
    ServerMsg = {ok, {ClientIp, ClientPort}},
    ClientMsg = {ok, {ServerIp, Port}},

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
sockname() ->
    [{doc,"Test API function sockname/1"}].
sockname(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {ssl, sockname, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl, sockname, []}},
                                        {options, [{port, 0} | ClientOpts]}]),

    ClientPort = ssl_test_lib:inet_port(Client),
    ServerIp = ssl_test_lib:node_to_hostip(ServerNode, server),
    ClientIp = ssl_test_lib:node_to_hostip(ClientNode, client),
    ServerMsg = {ok, {ServerIp, Port}},
    ClientMsg = {ok, {ClientIp, ClientPort}},

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
tls_server_handshake_timeout() ->
    [{doc,"Test server handshake timeout"}].

tls_server_handshake_timeout(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{timeout, 5000},
					{mfa, {ssl_test_lib,
					       no_result_msg, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, CSocket} = gen_tcp:connect(Hostname, Port, [binary, {active, true}]),

    receive
	{tcp_closed, CSocket} ->
	    ssl_test_lib:check_result(Server, {error, timeout}),
	    receive
		{'EXIT', Server, _} ->
		    %% Make sure supervisor had time to react on process exit
		    %% Could we come up with a better solution to this?
		    ct:sleep(500),
		    [] = supervisor:which_children(tls_connection_sup)
	    end
    end.

%%--------------------------------------------------------------------
transport_close() ->
    [{doc, "Test what happens if socket is closed on TCP level after a while of normal operation"}].
transport_close(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = case proplists:get_value(transport, Config) of
                       socket ->
                           rpc:call(ClientNode, tls_socket_tcp, connect,
                                    [Hostname,Port,[binary, {active, false}], infinity]);
                       _ ->
                           rpc:call(ClientNode, gen_tcp, connect,
                                    [Hostname,Port,[binary, {active, false}]])
                   end,

    {ok, SslS} = rpc:call(ClientNode, ssl, connect,
			  [Socket,[{active, false}|ClientOpts]]),

    ?CT_LOG("Server ~p ~n", [Server]),
    ok = ssl:send(SslS, "Hello world"),
    {ok,<<"Hello world">>} = ssl:recv(SslS, 11),
    case proplists:get_value(transport, Config) of
        socket ->
            socket:close(Socket);
        _ ->
            gen_tcp:close(Socket)
    end,
    {error, _} = ssl:send(SslS, "Hello world").

%%--------------------------------------------------------------------
transport_close_in_inital_hello() ->
    [{doc, "Test what happens if server dies after calling transport_accept but before initiating handshake."}].
transport_close_in_inital_hello(Config) when is_list(Config) ->
    _ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_, _, Hostname} = ssl_test_lib:run_where(Config),
    process_flag(trap_exit, true),

    Testcase = self(),

    Acceptor = spawn_link(fun() ->
                                  {ok, Listen} = ssl:listen(0, ServerOpts),
                                  {ok, {_, Port}} = ssl:sockname(Listen),
                                  Testcase ! {port, Port},
                                  {ok, _Accept} = ssl:transport_accept(Listen),
                                  receive
                                      die -> ok
                                  end
                          end),
    Port =  receive
                {port, Port0} ->
                    Port0
            end,

    Connector = spawn_link(fun() ->
                                   {ok, _} = ssl:connect(Hostname, Port,
                                                         [{verify, verify_none}],
                                                         infinity
                                                        )
                           end),


    Sup = (whereis(tls_connection_sup)),

    check_connection_processes(Sup, 2),

    Acceptor ! die,

    receive
         {'EXIT', Acceptor, _} ->
            ok
    end,
    receive
        {'EXIT', Connector, _} ->
            ok
     end,
    check_connection_processes(Sup, 0).

check_connection_processes(Sup, N) ->
    check_connection_processes(Sup, N, 5).

check_connection_processes(Sup, N, 0) ->
    N = count_children(supervisors, Sup);
check_connection_processes(Sup, N, M) ->
    case count_children(supervisors, Sup) of
        N ->
            ok;
        _ ->
            ct:sleep(500),
            check_connection_processes(Sup, N, M-1)
    end.

%%--------------------------------------------------------------------
emulated_options() ->
    [{doc,"Test API function getopts/2 and setopts/2"}].

emulated_options(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Values = [{mode, list}, {packet, 0}, {header, 0},
		      {active, true}],
    %% Shall be the reverse order of Values!
    Options = [active, header, packet, mode],

    NewValues = [{mode, binary}, {active, once}],
    %% Shall be the reverse order of NewValues!
    NewOptions = [active, mode],

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, tls_socket_options_result,
				  [Options, Values, NewOptions, NewValues]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {?MODULE, tls_socket_options_result,
				  [Options, Values, NewOptions, NewValues]}},
			   {options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),

    {ok, Listen} = ssl:listen(0, ServerOpts),
    {ok,[{mode,list}]} = ssl:getopts(Listen, [mode]),
    ok = ssl:setopts(Listen, [{mode, binary}]),
    {ok,[{mode, binary}]} = ssl:getopts(Listen, [mode]),
    {ok,[{recbuf, _}]} = ssl:getopts(Listen, [recbuf]),
    ssl:close(Listen).
accept_pool() ->
    [{doc,"Test having an accept pool."}].
accept_pool(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server0 = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{accepters, 3},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server0),
    [Server1, Server2] = ssl_test_lib:accepters(2),

    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					 {host, Hostname},
					 {from, self()},
					 {mfa, {ssl_test_lib, send_recv_result_active, []}},
					 {options, ClientOpts}
					]),

    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					 {host, Hostname},
					 {from, self()},
					 {mfa, {ssl_test_lib, send_recv_result_active, []}},
					 {options, ClientOpts}
					]),

    Client2 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					 {host, Hostname},
					 {from, self()},
					 {mfa, {ssl_test_lib, send_recv_result_active, []}},
					 {options, ClientOpts}
					]),

    ssl_test_lib:check_ok([Server0, Server1, Server2, Client0, Client1, Client2]),

    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Server2),
    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Client2).

%%--------------------------------------------------------------------
reuseaddr() ->
    [{doc,"Test reuseaddr option"}].

reuseaddr(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options,  [{active, false}, {reuseaddr, true}| ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, [{active, false} | ClientOpts]}]),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),

    Server1 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, Port},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{active, false}, {reuseaddr, true} | ServerOpts]}]),
    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server1, ok, Client1, ok),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
tls_password_correct() ->
    [{doc, "Test that connection is possible with a correct password"}].
tls_password_correct(Config) when is_list(Config) ->
    F = fun (P) ->
                ProtectedClientOpts = ssl_test_lib:ssl_options(client_protected_rsa_opts, Config),
                ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

                {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
                Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                                    {from, self()},
                                                    {mfa, {ssl_test_lib, send_recv_result, []}},
                                                    {options, [{active, false} | ServerOpts]}]),
                Port = ssl_test_lib:inet_port(Server),
                Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                                    {host, Hostname},
                                                    {from, self()},
                                                    {mfa,
                                                     {ssl_test_lib, send_recv_result, []}},
                                                    {options,
                                                     [{verify, verify_none},
                                                      {active, false},
                                                      {password, P} | ProtectedClientOpts]}]),
                ssl_test_lib:check_result(Server, ok, Client, ok),
                ssl_test_lib:close(Server),
                ssl_test_lib:close(Client)
        end,
    F(?CORRECT_PASSWORD),
    F(fun() -> ?CORRECT_PASSWORD end).

%%--------------------------------------------------------------------
tls_password_incorrect() ->
    [{doc, "Test that connection is not possible with wrong password"}].
tls_password_incorrect(Config) when is_list(Config) ->
    F = fun (P) ->
                ProtectedClientOpts = ssl_test_lib:ssl_options(client_protected_rsa_opts, Config),
                ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

                {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
                Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
                                                          {from, self()},
                                                          {mfa, ssl_test_lib, no_result},
                                                          {options, [{active, false} | ServerOpts]}]),
                Port = ssl_test_lib:inet_port(Server),
                Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
                                                          {host, Hostname},
                                                          {from, self()},
                                                          {mfa, ssl_test_lib, no_result},
                                                          {options,
                                                           [{active, false},
                                                            {verify, verify_none},
                                                            {password, P} | ProtectedClientOpts]}]),
                Results = ssl_test_lib:get_result([Server, Client]),
                Pred = fun({Pid, {error, closed}}) ->
                               Server == Pid;
                          ({Pid, {error, {options, {keyfile, _, {error, _}}}}}) ->
                               Client == Pid;
                          (_) -> false
                       end,
                true = lists:all(Pred, Results)
        end,
    F(?INCORRECT_PASSWORD),
    F(fun() -> ?INCORRECT_PASSWORD end).

%%--------------------------------------------------------------------
tls_password_badarg() ->
    [{doc, "Test that connection is not possible with badarg password"}].
tls_password_badarg(Config) when is_list(Config) ->
    F = fun (P, ServerError, ClientError) ->
                ProtectedClientOpts = ssl_test_lib:ssl_options(client_protected_rsa_opts, Config),
                ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
                {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
                Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
                                                          {from, self()},
                                                          {mfa, ssl_test_lib, no_result},
                                                          {timeout, 100},
                                                          {options, [{active, false} | ServerOpts]}]),
                Port = ssl_test_lib:inet_port(Server),
                Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
                                                          {host, Hostname},
                                                          {from, self()},
                                                          {mfa, ssl_test_lib, no_result},
                                                          {options,
                                                           [{active, false},
                                                            {verify, verify_none},
                                                            {password, P} | ProtectedClientOpts]}]),
                ssl_test_lib:check_result(Server, ServerError, Client, ClientError)
        end,
    %% {options error comes from ssl app
    F(?BADARG_PASSWORD, {error, timeout},
      {error, {options, {password, ?BADARG_PASSWORD}}}),
    %% {keyfile, badarg} error comes from crypto:macN, also handhsake is initiated
    %% so different server error is observed
    F(fun() -> ?BADARG_PASSWORD end, {error, closed},
      {error, {keyfile,badarg}}).

%%--------------------------------------------------------------------
signature_algs() ->
    [{doc, "Check that listing of signature algorithms for different version and configure combinations"}].
signature_algs(Config) when is_list(Config) ->
    true = [] =/= [Alg || Alg <- ssl:signature_algs(default, 'tlsv1.3'), is_tuple(Alg)],
    true = [] == [Alg || Alg <- ssl:signature_algs(exclusive, 'tlsv1.3'), is_tuple(Alg)],
    true = ssl:signature_algs(exclusive, 'tlsv1.3') =/=  ssl:signature_algs(exclusive, 'tlsv1.2'),
    true = length(ssl:signature_algs(default, 'tlsv1.2')) <
        length(ssl:signature_algs(all, 'tlsv1.2')),
    TLS_1_3_All = ssl:signature_algs(all, 'tlsv1.3'),
    true = lists:member(rsa_pkcs1_sha512, TLS_1_3_All) andalso (not lists:member({sha512, rsa}, TLS_1_3_All)),
    true = lists:member(rsa_pkcs1_sha384, TLS_1_3_All) andalso (not lists:member({sha384, rsa}, TLS_1_3_All)),
    true = lists:member(rsa_pkcs1_sha256, TLS_1_3_All) andalso (not lists:member({sha256, rsa}, TLS_1_3_All)),
    true = lists:member(rsa_pkcs1_sha, TLS_1_3_All) andalso (not lists:member({sha, rsa}, TLS_1_3_All)),
    true = lists:member(ecdsa_sha1, TLS_1_3_All) andalso (not lists:member({sha, ecdsa}, TLS_1_3_All)),
    TLS_1_2_All = ssl:signature_algs(all, 'tlsv1.2'),
    true = (not lists:member(rsa_pkcs1_sha512, TLS_1_2_All)) andalso lists:member({sha512, rsa}, TLS_1_2_All),
    true = (not lists:member(rsa_pkcs1_sha384, TLS_1_2_All)) andalso lists:member({sha384, rsa}, TLS_1_2_All),
    true = (not lists:member(rsa_pkcs1_sha256, TLS_1_2_All)) andalso lists:member({sha256, rsa}, TLS_1_2_All),
    true = (not lists:member(rsa_pkcs1_sha, TLS_1_2_All)) andalso lists:member({sha, rsa}, TLS_1_2_All),
    true = (not lists:member(ecdsa_sha1, TLS_1_2_All)) andalso lists:member({sha, ecdsa}, TLS_1_2_All),
    All = ssl_cipher:signature_schemes_1_2(ssl:signature_algs(default, 'tlsv1.3')),
    true = length(All) == length(lists:uniq(All)).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

upgrade_result(Socket) ->
    ssl:setopts(Socket, [{active, true}]),
    ok = ssl:send(Socket, "Hello world"),
    %% Make sure binary is inherited from tcp socket and that we do
    %% not get the list default!
    <<"Hello world">> = ssl_test_lib:active_recv(Socket, length("Hello world")),
    ok.

upgrade_result_new_opts(Socket) ->
    ssl:setopts(Socket, [{active, true}]),
    ok = ssl:send(Socket, "Hello world"),
    %% Make sure list option set in ssl:connect/handshake overrides
    %% previous gen_tcp socket option that was set to binary.
    "Hello world" =  ssl_test_lib:active_recv(Socket, length("Hello world")),
    ok.

tls_downgrade_result(Socket, Pid) ->
    ok = ssl_test_lib:send_recv_result(Socket),
    Pid ! {self(), ready},
    receive
        go ->
            ok
    end,
    case ssl:close(Socket, {self(), 10000})  of
        {ok, {'$socket', _} = TCPSocket} ->
            ok = socket:send(TCPSocket, <<"Downgraded">>),
            {ok, <<"Downgraded">>} = socket:recv(TCPSocket, length("Downgraded")),
            ok;
        {ok, {'$socket', _} = TCPSocket, Bin} ->
            ok = socket:send(TCPSocket, <<"Downgraded">>),
            <<"Downgraded">> = Bin,
            ok;
	{ok, TCPSocket} ->
            inet:setopts(TCPSocket, [{active, true}]),
	    gen_tcp:send(TCPSocket, "Downgraded"),
            <<"Downgraded">> = active_tcp_recv(TCPSocket, length("Downgraded")),
            ok;
	{ok, TCPSocket, Bin} ->
	    gen_tcp:send(TCPSocket, "Downgraded"),
            <<"Downgraded">> = Bin,
            ok;
	{error, timeout} ->
	    ct:comment("Timed out, downgrade aborted"),
	    ok;
	Fail ->
            ct:fail(Fail)
    end.

tls_shutdown_result(Socket, server) ->
    ok = ssl:send(Socket, "Hej"),
    ok = ssl:shutdown(Socket, write),
    {ok, "Hej hopp"} = ssl:recv(Socket, 8),
    ok;

tls_shutdown_result(Socket, client) ->
    ok = ssl:send(Socket, "Hej hopp"),
    ok = ssl:shutdown(Socket, write),
    {ok, "Hej"} = ssl:recv(Socket, 3),
    ok.

tls_shutdown_write_result(Socket, server) ->
    ct:sleep(?SLEEP),
    ssl:shutdown(Socket, write);
tls_shutdown_write_result(Socket, client) ->
    ssl:recv(Socket, 0).

tls_shutdown_both_result(Socket, server) ->
    ct:sleep(?SLEEP),
    ssl:shutdown(Socket, read_write);
tls_shutdown_both_result(Socket, client) ->
    ssl:recv(Socket, 0).

tls_closed_in_active_once_loop(Socket) ->
    case ssl:setopts(Socket, [{active, once}]) of
        ok ->
            receive
                {ssl, Socket, _} ->
                    tls_closed_in_active_once_loop(Socket);
                {ssl_closed, Socket} ->
                    ok
            end;
        {error, closed} ->
            {error, ssl_setopt_failed}
    end.

receive_msg(_) ->
    receive
	Msg ->
	   Msg
    end.

tls_socket_options_result(Socket, Options, DefaultValues, NewOptions, NewValues) ->
    %% Test get/set emulated opts
    {ok, DefaultValues} = ssl:getopts(Socket, Options),
    ssl:setopts(Socket, NewValues),
    {ok, NewValues} = ssl:getopts(Socket, NewOptions),
    %% Test get/set inet opts
    {ok,[{nodelay,false}]} = ssl:getopts(Socket, [nodelay]),
    ssl:setopts(Socket, [{nodelay, true}]),
    {ok,[{nodelay, true}]} = ssl:getopts(Socket, [nodelay]),
    {ok, All} = ssl:getopts(Socket, []),
    ?CT_LOG("All opts ~p~n", [All]),
    ok.

active_tcp_recv(Socket, N) ->
    active_tcp_recv(Socket, N, []).

active_tcp_recv(_Socket, 0, Acc) ->
    Acc;
active_tcp_recv(Socket, N, Acc) ->
    receive
	{tcp, Socket, Bytes} ->
            active_tcp_recv(Socket, N-size(Bytes),  Acc ++ Bytes)
    end.

tls_monitor_listen_opts('tlsv1.3', Opts) ->
    [{session_tickets, stateful} | Opts];
tls_monitor_listen_opts(_, Opts) ->
    Opts.

tls_monitor_client_opts('tlsv1.3', Opts) ->
    [{session_tickets, auto} | Opts];
tls_monitor_client_opts(_, Opts) ->
    Opts.

session_info('tlsv1.3') ->
    count_children(workers, tls_server_session_ticket_sup);
session_info(_) ->
    count_children(workers, ssl_server_session_cache_sup).

count_children(ChildType, SupRef) ->
    proplists:get_value(ChildType, supervisor:count_children(SupRef)).


new_versions('tlsv1.3') ->
    ['tlsv1.2'];
new_versions('tlsv1.2') ->
    ['tlsv1.1'];
new_versions('tlsv1.1') ->
    ['tlsv1'];
new_versions('tlsv1') ->
    ['tlsv1'].
