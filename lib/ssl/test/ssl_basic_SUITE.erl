%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.2
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

-module(ssl_basic_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

-include("ssl_api.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("tls_record.hrl").
-include("tls_handshake.hrl").

-define('24H_in_sec', 86400).  
-define(TIMEOUT, 60000).
-define(LONG_TIMEOUT, 600000).
-define(EXPIRE, 10).
-define(SLEEP, 500).
-define(RENEGOTIATION_DISABLE_TIME, 12000).
-define(CLEAN_SESSION_DB, 60000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [
     {group, basic},
     {group, options},
     {group, session},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'sslv3'}
    ].

groups() ->
    [{basic, [], basic_tests()},
     {options, [], options_tests()},
     {'tlsv1.2', [], all_versions_groups()},
     {'tlsv1.1', [], all_versions_groups()},
     {'tlsv1', [], all_versions_groups() ++ rizzo_tests()},
     {'sslv3', [], all_versions_groups() ++ rizzo_tests()},
     {api,[], api_tests()},
     {session, [], session_tests()},
     {renegotiate, [], renegotiate_tests()},
     {ciphers, [], cipher_tests()},
     {ciphers_ec, [], cipher_tests_ec()},
     {error_handling_tests, [], error_handling_tests()}
    ].

all_versions_groups ()->
    [{group, api},
     {group, renegotiate},
     {group, ciphers},
     {group, ciphers_ec},
     {group, error_handling_tests}].


basic_tests() ->
    [app,
     alerts,
     send_close,
     connect_twice,
     connect_dist,
     clear_pem_cache
    ].

options_tests() ->
    [der_input,
     misc_ssl_options,
     socket_options,
     invalid_inet_get_option,
     invalid_inet_get_option_not_list,
     invalid_inet_get_option_improper_list,
     invalid_inet_set_option,
     invalid_inet_set_option_not_list,
     invalid_inet_set_option_improper_list,
     dh_params,
     invalid_certfile,
     invalid_cacertfile,
     invalid_keyfile,
     invalid_options,
     protocol_versions,
     empty_protocol_versions,
     ipv6,
     reuseaddr,
     tcp_reuseaddr].

api_tests() ->
    [connection_info,
     peername,
     peercert,
     peercert_with_client_cert,
     sockname,
     versions,
     controlling_process,
     upgrade,
     upgrade_with_timeout,
     shutdown,
     shutdown_write,
     shutdown_both,
     shutdown_error,
     hibernate,
     listen_socket,
     ssl_accept_timeout,
     ssl_recv_timeout,
     versions_option
    ].

session_tests() ->
    [reuse_session,
     reuse_session_expired,
     server_does_not_want_to_reuse_session,
     no_reuses_session_server_restart_new_cert,
     no_reuses_session_server_restart_new_cert_file].

renegotiate_tests() ->
    [client_renegotiate,
     server_renegotiate,
     client_renegotiate_reused_session,
     server_renegotiate_reused_session,
     client_no_wrap_sequence_number,
     server_no_wrap_sequence_number,
     renegotiate_dos_mitigate_active,
     renegotiate_dos_mitigate_passive].

cipher_tests() ->
    [cipher_suites,
     ciphers_rsa_signed_certs,
     ciphers_rsa_signed_certs_openssl_names,
     ciphers_dsa_signed_certs,
     ciphers_dsa_signed_certs_openssl_names,
     anonymous_cipher_suites,
     psk_cipher_suites,
     psk_with_hint_cipher_suites,
     psk_anon_cipher_suites,
     psk_anon_with_hint_cipher_suites,
     srp_cipher_suites,
     srp_anon_cipher_suites,
     srp_dsa_cipher_suites,
     default_reject_anonymous].

cipher_tests_ec() ->
    [ciphers_ecdsa_signed_certs,
     ciphers_ecdsa_signed_certs_openssl_names,
     ciphers_ecdh_rsa_signed_certs,
     ciphers_ecdh_rsa_signed_certs_openssl_names].

error_handling_tests()->
    [controller_dies,
     client_closes_socket,
     tcp_error_propagation_in_active_mode,
     tcp_connect,
     tcp_connect_big,
     close_transport_accept
    ].

rizzo_tests() ->
    [rizzo,
     no_rizzo_rc4].

%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    Dog = ct:timetrap(?LONG_TIMEOUT *2),
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl:start(),
	    %% make rsa certs using oppenssl
	    Result =
		(catch make_certs:all(?config(data_dir, Config0),
				      ?config(priv_dir, Config0))),
	    ct:log("Make certs  ~p~n", [Result]),

	    Config1 = ssl_test_lib:make_dsa_cert(Config0),
	    Config2 = ssl_test_lib:make_ecdsa_cert(Config1),
	    Config3 = ssl_test_lib:make_ecdh_rsa_cert(Config2),
	    Config = ssl_test_lib:cert_options(Config3),
	    [{watchdog, Dog} | Config]
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

%%--------------------------------------------------------------------
init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) andalso ssl_test_lib:sufficient_crypto_support(GroupName) of
	true ->
	    ssl_test_lib:init_tls_version(GroupName),
	    Config;
	_ ->
	    case ssl_test_lib:sufficient_crypto_support(GroupName) of
		true ->
		    ssl:start(),
		    Config;
		false ->
		    {skip, "Missing crypto support"}
	    end
    end.

end_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
init_per_testcase(no_authority_key_identifier, Config) ->
    %% Clear cach so that root cert will not
    %% be found.
    ssl:clear_pem_cache(),
    Config;

init_per_testcase(protocol_versions, Config)  ->
    ssl:stop(),
    application:load(ssl),
    %% For backwards compatibility sslv2 should be filtered out.
    application:set_env(ssl, protocol_version, [sslv2, sslv3, tlsv1]),
    ssl:start(),
    Config;

init_per_testcase(reuse_session_expired, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_lifetime, ?EXPIRE),
    application:set_env(ssl, session_delay_cleanup_time, 500),
    ssl:start(),
    Config;

init_per_testcase(empty_protocol_versions, Config)  ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, protocol_version, []),
    ssl:start(),
    Config;

%% init_per_testcase(different_ca_peer_sign, Config0) ->
%%     ssl_test_lib:make_mix_cert(Config0);

init_per_testcase(_TestCase, Config0) ->
    ct:log("TLS/SSL version ~p~n ", [tls_record:supported_protocol_versions()]),
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ct:timetrap(?TIMEOUT),
   [{watchdog, Dog} | Config].

end_per_testcase(reuse_session_expired, Config) ->
    application:unset_env(ssl, session_lifetime),
    application:unset_env(ssl, session_delay_cleanup_time),
    end_per_testcase(default_action, Config);

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
app() ->
    [{doc, "Test that the ssl app file is ok"}].
app(Config) when is_list(Config) ->
    ok = ?t:app_test(ssl).
%%--------------------------------------------------------------------
alerts() ->
    [{doc, "Test ssl_alert:alert_txt/1"}].
alerts(Config) when is_list(Config) ->
    Descriptions = [?CLOSE_NOTIFY, ?UNEXPECTED_MESSAGE, ?BAD_RECORD_MAC,
		    ?DECRYPTION_FAILED, ?RECORD_OVERFLOW, ?DECOMPRESSION_FAILURE,
		    ?HANDSHAKE_FAILURE, ?BAD_CERTIFICATE, ?UNSUPPORTED_CERTIFICATE,
		    ?CERTIFICATE_REVOKED,?CERTIFICATE_EXPIRED, ?CERTIFICATE_UNKNOWN,
		    ?ILLEGAL_PARAMETER, ?UNKNOWN_CA, ?ACCESS_DENIED, ?DECODE_ERROR,
		    ?DECRYPT_ERROR, ?EXPORT_RESTRICTION, ?PROTOCOL_VERSION, 
		    ?INSUFFICIENT_SECURITY, ?INTERNAL_ERROR, ?USER_CANCELED,
		    ?NO_RENEGOTIATION],
    Alerts = [?ALERT_REC(?WARNING, ?CLOSE_NOTIFY) | 
	      [?ALERT_REC(?FATAL, Desc) || Desc <- Descriptions]],
    lists:foreach(fun(Alert) ->
			case ssl_alert:alert_txt(Alert) of
			    Txt when is_list(Txt) ->
				ok;
			    Other ->
				ct:fail({unexpected, Other})
			end 
		  end, Alerts).
%%--------------------------------------------------------------------
connection_info() ->
    [{doc,"Test the API function ssl:connection_info/1"}].
connection_info(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, connection_info_result, []}},
					{options, ServerOpts}]),
    
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, connection_info_result, []}},
			   {options, 
			    [{ciphers,[{rsa,rc4_128,sha,no_export}]} | 
			     ClientOpts]}]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    Version = 
	tls_record:protocol_version(tls_record:highest_protocol_version([])),
    
    ServerMsg = ClientMsg = {ok, {Version, {rsa,rc4_128,sha}}},
			   
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
protocol_versions() ->
    [{doc,"Test to set a list of protocol versions in app environment."}].

protocol_versions(Config) when is_list(Config) -> 
    basic_test(Config).
%%--------------------------------------------------------------------
empty_protocol_versions() ->
    [{doc,"Test to set an empty list of protocol versions in app environment."}].

empty_protocol_versions(Config) when is_list(Config) -> 
    basic_test(Config).

%%--------------------------------------------------------------------

controlling_process() ->
    [{doc,"Test API function controlling_process/2"}].

controlling_process(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientMsg = "Server hello",
    ServerMsg = "Client hello",
   
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, 
				  controlling_process_result, [self(),
							       ServerMsg]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
		   {from, self()}, 
			   {mfa, {?MODULE, 
				  controlling_process_result, [self(),
							       ClientMsg]}},
			   {options, ClientOpts}]),
   
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),
    
    receive 
	{ssl, _, "S"} ->
	    receive_s_rizzo_duong_beast();
	{ssl, _, ServerMsg} ->
	    receive 
		{ssl, _, ClientMsg} ->
		    ok
	    end;
	{ssl, _, "C"} ->
	    receive_c_rizzo_duong_beast();
	{ssl, _, ClientMsg} ->
	      receive 
		  {ssl, _, ServerMsg} ->
		      ok
	      end;
	Unexpected ->
	    ct:fail(Unexpected)
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
controller_dies() ->
    [{doc,"Test that the socket is closed after controlling process dies"}].
controller_dies(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientMsg = "Hello server",
    ServerMsg = "Hello client",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, 
					       controller_dies_result, [self(),
									ServerMsg]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       controller_dies_result, [self(),
									    ClientMsg]}},
					{options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n", [self(), Client, Server]),
    ct:sleep(?SLEEP), %% so that they are connected
    
    process_flag(trap_exit, true),

    %% Test that clients die
    exit(Client, killed),
    get_close(Client, ?LINE),

    %% Test that clients die when process disappear
    Server ! listen, 
    Tester = self(),
    Connect = fun(Pid) ->
		      {ok, Socket} = ssl:connect(Hostname, Port, 
						  [{reuseaddr,true},{ssl_imp,new}]),
		      %% Make sure server finishes and verification
		      %% and is in coonection state before
		      %% killing client
		      ct:sleep(?SLEEP),
		      Pid ! {self(), connected, Socket},
		      receive die_nice -> normal end
	      end,
    Client2 = spawn_link(fun() -> Connect(Tester) end),
    receive {Client2, connected, _Socket} ->  Client2 ! die_nice end,
    
    get_close(Client2, ?LINE),
    
    %% Test that clients die when the controlling process have changed 
    Server ! listen, 

    Client3 = spawn_link(fun() -> Connect(Tester) end),
    Controller = spawn_link(fun() -> receive die_nice -> normal end end),
    receive 
	{Client3, connected, Socket} ->  
	    ok = ssl:controlling_process(Socket, Controller),
	    Client3 ! die_nice 
    end,

    ct:log("Wating on exit ~p~n",[Client3]),
    receive {'EXIT', Client3, normal} -> ok end,
    
    receive   %% Client3 is dead but that doesn't matter, socket should not be closed.
	Unexpected ->
	    ct:log("Unexpected ~p~n",[Unexpected]),
	    ct:fail({line, ?LINE-1})
    after 1000 ->
	    ok
    end,
    Controller ! die_nice,
    get_close(Controller, ?LINE),
    
    %% Test that servers die
    Server ! listen, 
    LastClient = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					    {host, Hostname},
					    {from, self()}, 
					    {mfa, {?MODULE, 
						   controller_dies_result, [self(),
									    ClientMsg]}},
					    {options, [{reuseaddr,true}|ClientOpts]}]),
    ct:sleep(?SLEEP), %% so that they are connected
    
    exit(Server, killed),
    get_close(Server, ?LINE),
    process_flag(trap_exit, false),
    ssl_test_lib:close(LastClient).

%%--------------------------------------------------------------------
client_closes_socket() ->
    [{doc,"Test what happens when client closes socket before handshake is compleated"}].

client_closes_socket(Config) when is_list(Config) -> 
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],
    
    Server = ssl_test_lib:start_upgrade_server_error([{node, ServerNode}, {port, 0}, 
						      {from, self()}, 
						      {tcp_options, TcpOpts},
						      {ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Connect = fun() ->
		      {ok, _Socket} = rpc:call(ClientNode, gen_tcp, connect, 
					      [Hostname, Port, TcpOpts]),	      
		      %% Make sure that ssl_accept is called before 
		      %% client process ends and closes socket.
		      ct:sleep(?SLEEP)
	      end,
    
    _Client = spawn_link(Connect),

    ssl_test_lib:check_result(Server, {error,closed}).

%%--------------------------------------------------------------------
connect_dist() ->
    [{doc,"Test a simple connect as is used by distribution"}].

connect_dist(Config) when is_list(Config) -> 
    ClientOpts0 = ?config(client_kc_opts, Config),
    ClientOpts = [{ssl_imp, new},{active, false}, {packet,4}|ClientOpts0],
    ServerOpts0 = ?config(server_kc_opts, Config),
    ServerOpts = [{ssl_imp, new},{active, false}, {packet,4}|ServerOpts0],

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, connect_dist_s, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, connect_dist_c, []}},
					{options, ClientOpts}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

clear_pem_cache() ->
    [{doc,"Test that internal reference tabel is cleaned properly even when "
     " the PEM cache is cleared" }].
clear_pem_cache(Config) when is_list(Config) -> 
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    [_,FilRefDb, _] = element(5, State),
    {Server, Client} = basic_verify_test_no_close(Config),
    2 = ets:info(FilRefDb, size), 
    ssl:clear_pem_cache(),
    _ = sys:get_status(whereis(ssl_manager)),
    {Server1, Client1} = basic_verify_test_no_close(Config),
    4 = ets:info(FilRefDb, size), 
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ct:sleep(5000),
    _ = sys:get_status(whereis(ssl_manager)),
    2 = ets:info(FilRefDb, size),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1),
    ct:sleep(5000),
    _ = sys:get_status(whereis(ssl_manager)),
    0 = ets:info(FilRefDb, size).

%%--------------------------------------------------------------------
peername() ->
    [{doc,"Test API function peername/1"}].

peername(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, peername_result, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, peername_result, []}},
					{options, [{port, 0} | ClientOpts]}]),
    
    ClientPort = ssl_test_lib:inet_port(Client),
    ServerIp = ssl_test_lib:node_to_hostip(ServerNode),
    ClientIp = ssl_test_lib:node_to_hostip(ClientNode),
    ServerMsg = {ok, {ClientIp, ClientPort}},
    ClientMsg = {ok, {ServerIp, Port}},
	
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),
		   
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
peercert() ->
    [{doc,"Test API function peercert/1"}].
peercert(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, peercert_result, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {?MODULE, peercert_result, []}},
			   {options, ClientOpts}]),

    CertFile = proplists:get_value(certfile, ServerOpts),
    [{'Certificate', BinCert, _}]= ssl_test_lib:pem_to_der(CertFile),

    ServerMsg = {error, no_peercert},
    ClientMsg = {ok, BinCert},

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

peercert_result(Socket) ->
    ssl:peercert(Socket).
%%--------------------------------------------------------------------

peercert_with_client_cert() ->
    [{doc,"Test API function peercert/1"}].
peercert_with_client_cert(Config) when is_list(Config) ->
    ClientOpts = ?config(client_dsa_opts, Config),
    ServerOpts = ?config(server_dsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, peercert_result, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {?MODULE, peercert_result, []}},
			   {options, ClientOpts}]),

    ServerCertFile = proplists:get_value(certfile, ServerOpts),
    [{'Certificate', ServerBinCert, _}]= ssl_test_lib:pem_to_der(ServerCertFile),
     ClientCertFile = proplists:get_value(certfile, ClientOpts),
    [{'Certificate', ClientBinCert, _}]= ssl_test_lib:pem_to_der(ClientCertFile),

    ServerMsg = {ok, ClientBinCert},
    ClientMsg = {ok, ServerBinCert},

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
sockname() ->
    [{doc,"Test API function sockname/1"}].
sockname(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, sockname_result, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, sockname_result, []}},
			   {options, [{port, 0} | ClientOpts]}]),

    ClientPort = ssl_test_lib:inet_port(Client),
    ServerIp = ssl_test_lib:node_to_hostip(ServerNode),
    ClientIp = ssl_test_lib:node_to_hostip(ClientNode),
    ServerMsg = {ok, {ServerIp, Port}},
    ClientMsg = {ok, {ClientIp, ClientPort}},
			   
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
			 [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

sockname_result(S) ->
    ssl:sockname(S).

%%--------------------------------------------------------------------
cipher_suites() ->
    [{doc,"Test API function cipher_suites/0"}].

cipher_suites(Config) when is_list(Config) -> 
    MandatoryCipherSuite = {rsa,'3des_ede_cbc',sha},
    [_|_] = Suites = ssl:cipher_suites(),
    true = lists:member(MandatoryCipherSuite, Suites),
    Suites = ssl:cipher_suites(erlang),
    [_|_] =ssl:cipher_suites(openssl).

%%--------------------------------------------------------------------
socket_options() ->
    [{doc,"Test API function getopts/2 and setopts/2"}].

socket_options(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
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
			   {mfa, {?MODULE, socket_options_result, 
				  [Options, Values, NewOptions, NewValues]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, socket_options_result, 
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

socket_options_result(Socket, Options, DefaultValues, NewOptions, NewValues) ->
    %% Test get/set emulated opts
    {ok, DefaultValues} = ssl:getopts(Socket, Options), 
    ssl:setopts(Socket, NewValues),
    {ok, NewValues} = ssl:getopts(Socket, NewOptions),
    %% Test get/set inet opts
    {ok,[{nodelay,false}]} = ssl:getopts(Socket, [nodelay]),  
    ssl:setopts(Socket, [{nodelay, true}]),
    {ok,[{nodelay, true}]} = ssl:getopts(Socket, [nodelay]),
    {ok, All} = ssl:getopts(Socket, []),
    ct:log("All opts ~p~n", [All]),
    ok.


%%--------------------------------------------------------------------
invalid_inet_get_option() ->
    [{doc,"Test handling of invalid inet options in getopts"}].

invalid_inet_get_option(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, get_invalid_inet_option, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
invalid_inet_get_option_not_list() ->
    [{doc,"Test handling of invalid type in getopts"}].

invalid_inet_get_option_not_list(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, get_invalid_inet_option_not_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


get_invalid_inet_option_not_list(Socket) ->
    {error, {options, {socket_options, some_invalid_atom_here}}}
     = ssl:getopts(Socket, some_invalid_atom_here),
     ok.

%%--------------------------------------------------------------------
invalid_inet_get_option_improper_list() ->
    [{doc,"Test handling of invalid type in getopts"}].

invalid_inet_get_option_improper_list(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, get_invalid_inet_option_improper_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


get_invalid_inet_option_improper_list(Socket) ->
    {error, {options, {socket_options, foo,_}}} = ssl:getopts(Socket, [packet | foo]),
    ok.

%%--------------------------------------------------------------------
invalid_inet_set_option() ->
    [{doc,"Test handling of invalid inet options in setopts"}].

invalid_inet_set_option(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, set_invalid_inet_option, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

set_invalid_inet_option(Socket) ->
    {error, {options, {socket_options, {packet, foo}}}} = ssl:setopts(Socket, [{packet, foo}]),
    {error, {options, {socket_options, {header, foo}}}} = ssl:setopts(Socket, [{header, foo}]),
    {error, {options, {socket_options, {active, foo}}}} = ssl:setopts(Socket, [{active, foo}]),
    {error, {options, {socket_options, {mode, foo}}}}   = ssl:setopts(Socket, [{mode, foo}]),
    ok.
%%--------------------------------------------------------------------
invalid_inet_set_option_not_list() ->
    [{doc,"Test handling of invalid type in setopts"}].

invalid_inet_set_option_not_list(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, set_invalid_inet_option_not_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


set_invalid_inet_option_not_list(Socket) ->
    {error, {options, {not_a_proplist, some_invalid_atom_here}}}
	= ssl:setopts(Socket, some_invalid_atom_here),
    ok.

%%--------------------------------------------------------------------
invalid_inet_set_option_improper_list() ->
    [{doc,"Test handling of invalid tye in setopts"}].

invalid_inet_set_option_improper_list(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, set_invalid_inet_option_improper_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

set_invalid_inet_option_improper_list(Socket) ->
    {error, {options, {not_a_proplist, [{packet, 0} | {foo, 2}]}}} =
	ssl:setopts(Socket, [{packet, 0} | {foo, 2}]),
    ok.

%%--------------------------------------------------------------------
misc_ssl_options() ->
    [{doc,"Test what happens when we give valid options"}].

misc_ssl_options(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    %% Chek that ssl options not tested elsewhere are filtered away e.i. not passed to inet.
    TestOpts = [{depth, 1}, 
		{key, undefined}, 
		{password, []},
		{reuse_session, fun(_,_,_,_) -> true end},
		{cb_info, {gen_tcp, tcp, tcp_closed, tcp_error}}],
    
   Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options,  TestOpts ++ ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options, TestOpts ++ ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
			 [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
versions() ->
    [{doc,"Test API function versions/0"}].

versions(Config) when is_list(Config) -> 
    [_|_] = Versions = ssl:versions(),
    ct:log("~p~n", [Versions]).

%%--------------------------------------------------------------------
send_recv() ->
    [{doc,""}].
send_recv(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{active, false} | ClientOpts]}]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
			 [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
send_close() ->
    [{doc,""}].
send_close(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, TcpS} = rpc:call(ClientNode, gen_tcp, connect, 
			  [Hostname,Port,[binary, {active, false}, {reuseaddr, true}]]),
    {ok, SslS} = rpc:call(ClientNode, ssl, connect, 
			  [TcpS,[{active, false}|ClientOpts]]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), self(), Server]),
    ok = ssl:send(SslS, "Hello world"),      
    {ok,<<"Hello world">>} = ssl:recv(SslS, 11),    
    gen_tcp:close(TcpS),    
    {error, _} = ssl:send(SslS, "Hello world").

%%--------------------------------------------------------------------
close_transport_accept() ->
    [{doc,"Tests closing ssl socket when waiting on ssl:transport_accept/1"}].

close_transport_accept(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Port = 0,
    Opts = [{active, false} | ServerOpts],
    {ok, ListenSocket} = rpc:call(ServerNode, ssl, listen, [Port, Opts]),
    spawn_link(fun() ->
			ct:sleep(?SLEEP),
			rpc:call(ServerNode, ssl, close, [ListenSocket])
	       end),
    case rpc:call(ServerNode, ssl, transport_accept, [ListenSocket]) of
	{error, closed} ->
	    ok;
	Other ->
	    exit({?LINE, Other})
    end.

%%--------------------------------------------------------------------
dh_params() ->
    [{doc,"Test to specify DH-params file in server."}].

dh_params(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    DataDir = ?config(data_dir, Config),
    DHParamFile = filename:join(DataDir, "dHParam.pem"),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {ssl_test_lib, send_recv_result_active, []}},
			   {options, [{dhfile, DHParamFile} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {ssl_test_lib, send_recv_result_active, []}},
			   {options,
			    [{ciphers,[{dhe_rsa,aes_256_cbc,sha,ignore}]} | 
				       ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
upgrade() ->
    [{doc,"Test that you can upgrade an tcp connection to an ssl connection"}].

upgrade(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server([{node, ServerNode}, {port, 0}, 
						{from, self()}, 
						{mfa, {?MODULE, 
						       upgrade_result, []}},
						{tcp_options, 
						 [{active, false} | TcpOpts]},
						{ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_upgrade_client([{node, ClientNode}, 
						{port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {?MODULE, upgrade_result, []}},
				   {tcp_options, TcpOpts},
				   {ssl_options, ClientOpts}]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

upgrade_result(Socket) ->
    ssl:setopts(Socket, [{active, true}]),
    ok = ssl:send(Socket, "Hello world"),
    %% Make sure binary is inherited from tcp socket and that we do
    %% not get the list default!
    receive 
	{ssl, _, <<"H">>} ->
	    receive 
		{ssl, _, <<"ello world">>} ->
		    ok
	    end;
	{ssl, _, <<"Hello world">>}  ->
	    ok
    end.

%%--------------------------------------------------------------------
upgrade_with_timeout() ->
    [{doc,"Test ssl_accept/3"}].

upgrade_with_timeout(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server([{node, ServerNode}, {port, 0}, 
						{from, self()}, 
						{timeout, 5000},
						{mfa, {?MODULE, 
						       upgrade_result, []}},
						{tcp_options, 
						 [{active, false} | TcpOpts]},
						{ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_upgrade_client([{node, ClientNode}, 
						{port, Port}, 
						{host, Hostname},
						{from, self()}, 
						{mfa, {?MODULE, upgrade_result, []}},
						{tcp_options, TcpOpts},
						{ssl_options, ClientOpts}]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tcp_connect() ->
    [{doc,"Test what happens when a tcp tries to connect, i,e. a bad (ssl) packet is sent first"}].

tcp_connect(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server_error([{node, ServerNode}, {port, 0},
						      {from, self()},
						      {timeout, 5000},
						      {mfa, {?MODULE, dummy, []}},
						      {tcp_options, TcpOpts},
						      {ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {packet, 0}]),
    ct:log("Testcase ~p connected to Server ~p ~n", [self(), Server]),
    gen_tcp:send(Socket, "<SOME GARBLED NON SSL MESSAGE>"),

    receive 
	{tcp_closed, Socket} ->
	    receive 
		{Server, {error, Error}} ->
		    ct:log("Error ~p", [Error])
	    end
    end.
%%--------------------------------------------------------------------
tcp_connect_big() ->
    [{doc,"Test what happens when a tcp tries to connect, i,e. a bad big (ssl) packet is sent first"}].

tcp_connect_big(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server_error([{node, ServerNode}, {port, 0},
						      {from, self()},
						      {timeout, 5000},
						      {mfa, {?MODULE, dummy, []}},
						      {tcp_options, TcpOpts},
						      {ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {packet, 0}]),
    ct:log("Testcase ~p connected to Server ~p ~n", [self(), Server]),

    Rand = crypto:rand_bytes(?MAX_CIPHER_TEXT_LENGTH+1),
    gen_tcp:send(Socket, <<?BYTE(0),
			   ?BYTE(3), ?BYTE(1), ?UINT16(?MAX_CIPHER_TEXT_LENGTH), Rand/binary>>),

    receive
	{tcp_closed, Socket} ->
	    receive
		{Server, {error, timeout}} ->
		    ct:fail("hangs");
		{Server, {error, Error}} ->
		    ct:log("Error ~p", [Error])
	    end
    end.

%%--------------------------------------------------------------------
ipv6() ->
    [{require, ipv6_hosts},
     {doc,"Test ipv6."}].
ipv6(Config) when is_list(Config) ->
    {ok, Hostname0} = inet:gethostname(),
    
    case lists:member(list_to_atom(Hostname0), ct:get_config(ipv6_hosts)) of
	true ->
	    ClientOpts = ?config(client_opts, Config),
	    ServerOpts = ?config(server_opts, Config),
	    {ClientNode, ServerNode, Hostname} = 
		ssl_test_lib:run_where(Config, ipv6),
	    Server = ssl_test_lib:start_server([{node, ServerNode}, 
				   {port, 0}, {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  
				    [inet6, {active, false} | ServerOpts]}]),
	    Port = ssl_test_lib:inet_port(Server), 
	    Client = ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port}, {host, Hostname},
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, 
				    [inet6, {active, false} | ClientOpts]}]),
	    
	    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
			       [self(), Client, Server]),
	    
	    ssl_test_lib:check_result(Server, ok, Client, ok),
	    
	    ssl_test_lib:close(Server),
	    ssl_test_lib:close(Client);
	false ->
	    {skip, "Host does not support IPv6"}
    end.

%%--------------------------------------------------------------------

invalid_keyfile() ->
    [{doc,"Test what happens with an invalid key file"}].
invalid_keyfile(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    BadOpts = ?config(server_bad_key, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = 
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
					 {from, self()},
			    {options, BadOpts}]),

    Port = ssl_test_lib:inet_port(Server),

    Client =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
			    {port, Port}, {host, Hostname},
			    {from, self()},  {options, ClientOpts}]),

    File = proplists:get_value(keyfile,BadOpts),    
    ssl_test_lib:check_result(Server, {error,{options, {keyfile, File, {error,enoent}}}}, Client,  
				       {error, closed}).

%%--------------------------------------------------------------------

invalid_certfile() ->
    [{doc,"Test what happens with an invalid cert file"}].

invalid_certfile(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerBadOpts = ?config(server_bad_cert, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = 
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
					 {from, self()},
					 {options, ServerBadOpts}]),

    Port = ssl_test_lib:inet_port(Server),

    Client =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
					 {port, Port}, {host, Hostname},
					 {from, self()}, 
					 {options, ClientOpts}]),
    File = proplists:get_value(certfile, ServerBadOpts),
    ssl_test_lib:check_result(Server, {error,{options, {certfile, File, {error,enoent}}}}, 
			      Client, {error, closed}).
    

%%--------------------------------------------------------------------
invalid_cacertfile() ->
    [{doc,"Test what happens with an invalid cacert file"}].

invalid_cacertfile(Config) when is_list(Config) ->
    ClientOpts    = [{reuseaddr, true}|?config(client_opts, Config)],
    ServerBadOpts = [{reuseaddr, true}|?config(server_bad_ca, Config)],
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server0  = 
	ssl_test_lib:start_server_error([{node, ServerNode}, 
					 {port, 0}, {from, self()},
					 {options, ServerBadOpts}]),

    Port0 = ssl_test_lib:inet_port(Server0),
    

    Client0 =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
					 {port, Port0}, {host, Hostname},
					 {from, self()}, 
					 {options, ClientOpts}]),

    File0 = proplists:get_value(cacertfile, ServerBadOpts),
    
    ssl_test_lib:check_result(Server0, {error, {options, {cacertfile, File0,{error,enoent}}}},
			      Client0, {error, closed}),
    
    File = File0 ++ "do_not_exit.pem",
    ServerBadOpts1 = [{cacertfile, File}|proplists:delete(cacertfile, ServerBadOpts)],
            
    Server1  = 
	ssl_test_lib:start_server_error([{node, ServerNode}, 
					 {port, 0}, {from, self()},
					 {options, ServerBadOpts1}]),

    Port1 = ssl_test_lib:inet_port(Server1),
    
    Client1 =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
					 {port, Port1}, {host, Hostname},
					 {from, self()}, 
					 {options, ClientOpts}]),


    ssl_test_lib:check_result(Server1, {error, {options, {cacertfile, File,{error,enoent}}}},
			      Client1, {error, closed}),
    ok.
    
    

%%--------------------------------------------------------------------
invalid_options() ->
    [{doc,"Test what happens when we give invalid options"}].
       
invalid_options(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Check = fun(Client, Server, {versions, [sslv2, sslv3]} = Option) ->
		    ssl_test_lib:check_result(Server, 
					      {error, {options, {sslv2, Option}}}, 
					      Client,
					      {error, {options, {sslv2, Option}}});
	       (Client, Server, Option) ->
		    ssl_test_lib:check_result(Server, 
					      {error, {options, Option}}, 
					      Client,
					      {error, {options, Option}})
	    end,

    TestOpts = [{versions, [sslv2, sslv3]}, 
		{verify, 4}, 
		{verify_fun, function},
		{fail_if_no_peer_cert, 0}, 
		{verify_client_once, 1},
		{depth, four}, 
		{certfile, 'cert.pem'}, 
		{keyfile,'key.pem' }, 
		{password, foo},
		{cacertfile, ""}, 
		{dhfile,'dh.pem' },
		{ciphers, [{foo, bar, sha, ignore}]},
		{reuse_session, foo},
		{reuse_sessions, 0},
		{renegotiate_at, "10"},
		{mode, depech},
		{packet, 8.0},
		{packet_size, "2"},
		{header, a},
		{active, trice},
		{key, 'key.pem' }],

    [begin
	 Server =
	     ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					{from, self()},
					{options, [TestOpt | ServerOpts]}]),
	 %% Will never reach a point where port is used.
	 Client =
	     ssl_test_lib:start_client_error([{node, ClientNode}, {port, 0},
					      {host, Hostname}, {from, self()},
					      {options, [TestOpt | ClientOpts]}]),
	 Check(Client, Server, TestOpt),
	 ok
     end || TestOpt <- TestOpts],
    ok.

%%--------------------------------------------------------------------
shutdown() ->
    [{doc,"Test API function ssl:shutdown/2"}].
shutdown(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, shutdown_result, [server]}},
			   {options, [{exit_on_close, false},
				      {active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, shutdown_result, [client]}},
					{options, 
					 [{exit_on_close, false},
					  {active, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
shutdown_write() ->
    [{doc,"Test API function ssl:shutdown/2 with option write."}].
shutdown_write(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, shutdown_write_result, [server]}},
			   {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, shutdown_write_result, [client]}},
			   {options, [{active, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, {error, closed}).

%%--------------------------------------------------------------------
shutdown_both() ->
    [{doc,"Test API function ssl:shutdown/2 with option both."}].
shutdown_both(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, shutdown_both_result, [server]}},
			   {options, [{active, false} | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, shutdown_both_result, [client]}},
			   {options, [{active, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, {error, closed}).

%%--------------------------------------------------------------------
shutdown_error() ->
    [{doc,"Test ssl:shutdown/2 error handling"}].
shutdown_error(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    Port = ssl_test_lib:inet_port(node()),
    {ok, Listen} = ssl:listen(Port, ServerOpts),
    {error, enotconn} = ssl:shutdown(Listen, read_write),
    ok = ssl:close(Listen),
    {error, closed} = ssl:shutdown(Listen, read_write).

%%-------------------------------------------------------------------
ciphers_rsa_signed_certs() ->
    [{doc,"Test all rsa ssl cipher suites in highest support ssl/tls version"}].
       
ciphers_rsa_signed_certs(Config) when is_list(Config) ->
    Version = 
	tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:rsa_suites(crypto),
    ct:log("~p erlang cipher suites ~p~n", [Version, Ciphers]),
    run_suites(Ciphers, Version, Config, rsa).
%%-------------------------------------------------------------------
ciphers_rsa_signed_certs_openssl_names() ->
    [{doc,"Test all rsa ssl cipher suites in highest support ssl/tls version"}].
       
ciphers_rsa_signed_certs_openssl_names(Config) when is_list(Config) ->
    Version = 
	tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:openssl_rsa_suites(crypto),  
    ct:log("tls1 openssl cipher suites ~p~n", [Ciphers]),
    run_suites(Ciphers, Version, Config, rsa).

%%-------------------------------------------------------------------
ciphers_dsa_signed_certs() ->
    [{doc,"Test all dsa ssl cipher suites in highest support ssl/tls version"}].
       
ciphers_dsa_signed_certs(Config) when is_list(Config) ->
    Version = 
	tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:dsa_suites(),
    ct:log("~p erlang cipher suites ~p~n", [Version, Ciphers]),
    run_suites(Ciphers, Version, Config, dsa).
%%-------------------------------------------------------------------
ciphers_dsa_signed_certs_openssl_names() ->
    [{doc,"Test all dsa ssl cipher suites in highest support ssl/tls version"}].
       
ciphers_dsa_signed_certs_openssl_names(Config) when is_list(Config) ->
    Version = 
	tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:openssl_dsa_suites(),
    ct:log("tls1 openssl cipher suites ~p~n", [Ciphers]),
    run_suites(Ciphers, Version, Config, dsa).
%%-------------------------------------------------------------------
anonymous_cipher_suites()->
    [{doc,"Test the anonymous ciphersuites"}].
anonymous_cipher_suites(Config) when is_list(Config) ->
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:anonymous_suites(),
    run_suites(Ciphers, Version, Config, anonymous).
%%-------------------------------------------------------------------
psk_cipher_suites() ->
    [{doc, "Test the PSK ciphersuites WITHOUT server supplied identity hint"}].
psk_cipher_suites(Config) when is_list(Config) ->
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:psk_suites(),
    run_suites(Ciphers, Version, Config, psk).
%%-------------------------------------------------------------------
psk_with_hint_cipher_suites()->
    [{doc, "Test the PSK ciphersuites WITH server supplied identity hint"}].
psk_with_hint_cipher_suites(Config) when is_list(Config) ->
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:psk_suites(),
    run_suites(Ciphers, Version, Config, psk_with_hint).
%%-------------------------------------------------------------------
psk_anon_cipher_suites() ->
    [{doc, "Test the anonymous PSK ciphersuites WITHOUT server supplied identity hint"}].
psk_anon_cipher_suites(Config) when is_list(Config) ->
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:psk_anon_suites(),
    run_suites(Ciphers, Version, Config, psk_anon).
%%-------------------------------------------------------------------
psk_anon_with_hint_cipher_suites()->
    [{doc, "Test the anonymous PSK ciphersuites WITH server supplied identity hint"}].
psk_anon_with_hint_cipher_suites(Config) when is_list(Config) ->
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:psk_anon_suites(),
    run_suites(Ciphers, Version, Config, psk_anon_with_hint).
%%-------------------------------------------------------------------
srp_cipher_suites()->
    [{doc, "Test the SRP ciphersuites"}].
srp_cipher_suites(Config) when is_list(Config) ->
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:srp_suites(),
    run_suites(Ciphers, Version, Config, srp).
%%-------------------------------------------------------------------
srp_anon_cipher_suites()->
    [{doc, "Test the anonymous SRP ciphersuites"}].
srp_anon_cipher_suites(Config) when is_list(Config) ->
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:srp_anon_suites(),
    run_suites(Ciphers, Version, Config, srp_anon).
%%-------------------------------------------------------------------
srp_dsa_cipher_suites()->
    [{doc, "Test the SRP DSA ciphersuites"}].
srp_dsa_cipher_suites(Config) when is_list(Config) ->
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:srp_dss_suites(),
    run_suites(Ciphers, Version, Config, srp_dsa).
%%--------------------------------------------------------------------
default_reject_anonymous()->
    [{doc,"Test that by default anonymous cipher suites are rejected "}].
default_reject_anonymous(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),

    [Cipher | _] = ssl_test_lib:anonymous_suites(),

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {options,
			    [{ciphers,[Cipher]} |
			     ClientOpts]}]),

    ssl_test_lib:check_result(Server, {error, {tls_alert, "insufficient security"}},
			      Client, {error, {tls_alert, "insufficient security"}}).

%%--------------------------------------------------------------------
ciphers_ecdsa_signed_certs() ->
    [{doc, "Test all ecdsa ssl cipher suites in highest support ssl/tls version"}].

ciphers_ecdsa_signed_certs(Config) when is_list(Config) ->
    Version =
       tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:ecdsa_suites(),
    ct:log("~p erlang cipher suites ~p~n", [Version, Ciphers]),
    run_suites(Ciphers, Version, Config, ecdsa).
%%--------------------------------------------------------------------
ciphers_ecdsa_signed_certs_openssl_names() ->
    [{doc, "Test all ecdsa ssl cipher suites in highest support ssl/tls version"}].

ciphers_ecdsa_signed_certs_openssl_names(Config) when is_list(Config) ->
    Version =
       tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:openssl_ecdsa_suites(),
    ct:log("tls1 openssl cipher suites ~p~n", [Ciphers]),
    run_suites(Ciphers, Version, Config, ecdsa).
%%--------------------------------------------------------------------
ciphers_ecdh_rsa_signed_certs() ->
    [{doc, "Test all ecdh_rsa ssl cipher suites in highest support ssl/tls version"}].

ciphers_ecdh_rsa_signed_certs(Config) when is_list(Config) ->
    Version =
       tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:ecdh_rsa_suites(),
    ct:log("~p erlang cipher suites ~p~n", [Version, Ciphers]),
    run_suites(Ciphers, Version, Config, ecdh_rsa).
%%--------------------------------------------------------------------
ciphers_ecdh_rsa_signed_certs_openssl_names() ->
    [{doc, "Test all ecdh_rsa ssl cipher suites in highest support ssl/tls version"}].

ciphers_ecdh_rsa_signed_certs_openssl_names(Config) when is_list(Config) ->
    Version =
       tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:openssl_ecdh_rsa_suites(),
    ct:log("tls1 openssl cipher suites ~p~n", [Ciphers]),
    run_suites(Ciphers, Version, Config, ecdh_rsa).
%%--------------------------------------------------------------------
reuse_session() ->
    [{doc,"Test reuse of sessions (short handshake)"}].
reuse_session(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()},
				   {mfa, {ssl_test_lib, session_info_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client0 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, no_result, []}},
		      {from, self()},  {options, ClientOpts}]),   
    SessionInfo = 
	receive
	    {Server, Info} ->
		Info
	end,
       
    Server ! {listen, {mfa, {ssl_test_lib, no_result, []}}},
    
    %% Make sure session is registered
    ct:sleep(?SLEEP),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode},
				   {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]),
    receive
	{Client1, SessionInfo} ->
	    ok;
	{Client1, Other} ->
	    ct:log("Expected: ~p,  Unexpected: ~p~n",
			       [SessionInfo, Other]),
	    ct:fail(session_not_reused)
    end,
    
    Server !  {listen, {mfa, {ssl_test_lib, no_result, []}}},
    
    Client2 =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
			    {mfa, {ssl_test_lib, session_info_result, []}},
		      {from, self()},  {options, [{reuse_sessions, false}
						  | ClientOpts]}]),   
    receive
	{Client2, SessionInfo} ->
	    ct:fail(
	      session_reused_when_session_reuse_disabled_by_client);
	{Client2, _} ->
	    ok
    end,
    
    ssl_test_lib:close(Server),

    Server1 = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()},
		      {mfa, {ssl_test_lib, session_info_result, []}},
		      {options, [{reuse_sessions, false} | ServerOpts]}]),
    
    Port1 = ssl_test_lib:inet_port(Server1),
    Client3 =
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port1}, {host, Hostname},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {from, self()},  {options, ClientOpts}]), 

    SessionInfo1 = 
	receive
	    {Server1, Info1} ->
		Info1
	end,
       
    Server1 ! {listen, {mfa, {ssl_test_lib, no_result, []}}},
    
    %% Make sure session is registered
    ct:sleep(?SLEEP),

    Client4 = 
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port1}, {host, Hostname},
				   {mfa, {ssl_test_lib, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]),
    
    receive
	{Client4, SessionInfo1} ->
	    ct:fail(
	      session_reused_when_session_reuse_disabled_by_server);
	{Client4, _Other} ->
	    ct:log("OTHER: ~p ~n", [_Other]),
	    ok
    end,

    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Client2),
    ssl_test_lib:close(Client3),
    ssl_test_lib:close(Client4).

%%--------------------------------------------------------------------
reuse_session_expired() ->
    [{doc,"Test sessions is not reused when it has expired"}].
reuse_session_expired(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()},
		      {mfa, {ssl_test_lib, session_info_result, []}},
		      {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client0 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
			    {mfa, {ssl_test_lib, no_result, []}},
		      {from, self()},  {options, ClientOpts}]),   
    SessionInfo = 
	receive
	    {Server, Info} ->
		Info
	end,

    Server ! {listen, {mfa, {ssl_test_lib, no_result, []}}},
    
    %% Make sure session is registered
    ct:sleep(?SLEEP),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
		      {mfa, {ssl_test_lib, session_info_result, []}},
		      {from, self()},  {options, ClientOpts}]),    
    receive
	{Client1, SessionInfo} ->
	    ok;
	{Client1, Other} ->
	    ct:log("Expected: ~p,  Unexpected: ~p~n",
			       [SessionInfo, Other]),
	    ct:fail(session_not_reused)
    end,
    
    Server ! listen,

    %% Make sure session is unregistered due to expiration
    ct:sleep((?EXPIRE+1)),
    [{session_id, Id} |_] = SessionInfo,

    make_sure_expired(Hostname, Port, Id),
    
    Client2 =
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, session_info_result, []}},
				   {from, self()}, {options, ClientOpts}]),   
    receive
	{Client2, SessionInfo} ->
	    ct:fail(session_reused_when_session_expired);
	{Client2, _} ->
	    ok
    end,
    process_flag(trap_exit, false),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Client2).

make_sure_expired(Host, Port, Id) ->
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    Cache = element(2, State),

    case ssl_session_cache:lookup(Cache, {{Host,  Port}, Id}) of
	undefined ->
	   ok;
	#session{is_resumable = false} ->
	   ok;
	_ ->
	    ct:sleep(?SLEEP),
	    make_sure_expired(Host, Port, Id)
    end.     

%%--------------------------------------------------------------------
server_does_not_want_to_reuse_session() ->
    [{doc,"Test reuse of sessions (short handshake)"}].
server_does_not_want_to_reuse_session(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()},
		      {mfa, {ssl_test_lib, session_info_result, []}},
				   {options, [{reuse_session, fun(_,_,_,_) ->
								      false
							      end} | 
					      ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client0 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
			    {mfa, {ssl_test_lib, no_result, []}},
		      {from, self()},  {options, ClientOpts}]),   
    SessionInfo = 
	receive
	    {Server, Info} ->
		Info
	end,
       
    Server ! {listen, {mfa, {ssl_test_lib, no_result, []}}},
    
    %% Make sure session is registered
    ct:sleep(?SLEEP),
    ssl_test_lib:close(Client0),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
		      {mfa, {ssl_test_lib, session_info_result, []}},
		      {from, self()},  {options, ClientOpts}]),    
    receive
	{Client1, SessionInfo} ->
	    ct:fail(session_reused_when_server_does_not_want_to);
	{Client1, _Other} ->
	   ok
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
client_renegotiate() ->
    [{doc,"Test ssl:renegotiate/1 on client."}].
client_renegotiate(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate, [Data]}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
server_renegotiate() ->
    [{doc,"Test ssl:renegotiate/1 on server."}].
server_renegotiate(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE,
					       renegotiate, [Data]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, erlang_ssl_receive, [Data]}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
client_renegotiate_reused_session() ->
    [{doc,"Test ssl:renegotiate/1 on client when the ssl session will be reused."}].
client_renegotiate_reused_session(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate_reuse_session, [Data]}},
					{options, [{reuse_sessions, true} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
server_renegotiate_reused_session() ->
    [{doc,"Test ssl:renegotiate/1 on server when the ssl session will be reused."}].
server_renegotiate_reused_session(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From erlang to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate_reuse_session, [Data]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, erlang_ssl_receive, [Data]}},
					{options, [{reuse_sessions, true} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
client_no_wrap_sequence_number() ->
    [{doc,"Test that erlang client will renegotiate session when",
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at"
     " to lower treashold substantially."}].

client_no_wrap_sequence_number(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    ErlData = "From erlang to erlang",
    N = 10,

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Version = tls_record:highest_protocol_version(tls_record:supported_protocol_versions()),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib,
					       trigger_renegotiate, [[ErlData, treashold(N, Version)]]}},
					{options, [{reuse_sessions, false},
						   {renegotiate_at, N} | ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
server_no_wrap_sequence_number() ->
    [{doc, "Test that erlang server will renegotiate session when",
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at"
     " to lower treashold substantially."}].

server_no_wrap_sequence_number(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",
    N = 10,

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib,
					       trigger_renegotiate, [[Data, N+2]]}},
					{options, [{renegotiate_at, N} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, no_result, []}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
der_input() ->
    [{doc,"Test to input certs and key as der"}].

der_input(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    DHParamFile = filename:join(DataDir, "dHParam.pem"),

    SeverVerifyOpts = ?config(server_verification_opts, Config),
    {ServerCert, ServerKey, ServerCaCerts, DHParams} = der_input_opts([{dhfile, DHParamFile} |
								       SeverVerifyOpts]),
    ClientVerifyOpts = ?config(client_verification_opts, Config),
    {ClientCert, ClientKey, ClientCaCerts, DHParams} = der_input_opts([{dhfile, DHParamFile} |
								       ClientVerifyOpts]),
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true},
		  {dh, DHParams},
		  {cert, ServerCert}, {key, ServerKey}, {cacerts, ServerCaCerts}],
    ClientOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true},
		  {dh, DHParams},
		  {cert, ClientCert}, {key, ClientKey}, {cacerts, ClientCaCerts}],
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result, []}},
					{options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result, []}},
					{options, [{active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
der_input_opts(Opts) ->
    Certfile = proplists:get_value(certfile, Opts),
    CaCertsfile = proplists:get_value(cacertfile, Opts),
    Keyfile = proplists:get_value(keyfile, Opts),
    Dhfile = proplists:get_value(dhfile, Opts),
    [{_, Cert, _}] = ssl_test_lib:pem_to_der(Certfile),
    [{Asn1Type, Key, _}]  = ssl_test_lib:pem_to_der(Keyfile),
    [{_, DHParams, _}]  = ssl_test_lib:pem_to_der(Dhfile),
    CaCerts =
	lists:map(fun(Entry) ->
			  {_, CaCert, _} = Entry,
			  CaCert
		  end, ssl_test_lib:pem_to_der(CaCertsfile)),
    {Cert, {Asn1Type, Key}, CaCerts, DHParams}.

%%--------------------------------------------------------------------
%% different_ca_peer_sign() ->
%%     ["Check that a CA can have a different signature algorithm than the peer cert."];

%% different_ca_peer_sign(Config) when is_list(Config) ->
%%     ClientOpts =  ?config(client_mix_opts, Config),
%%     ServerOpts =  ?config(server_mix_verify_opts, Config),

%%     {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
%%     Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
%% 					{from, self()},
%% 			   {mfa, {ssl_test_lib, send_recv_result_active_once, []}},
%% 			   {options, [{active, once},
%% 				      {verify, verify_peer} | ServerOpts]}]),
%%     Port  = ssl_test_lib:inet_port(Server),

%%     Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
%% 					{host, Hostname},
%% 					{from, self()},
%% 					{mfa, {ssl_test_lib,
%% 					       send_recv_result_active_once,
%% 					       []}},
%% 					{options, [{active, once},
%% 						   {verify, verify_peer}
%% 						   | ClientOpts]}]),

%%     ssl_test_lib:check_result(Server, ok, Client, ok),
%%     ssl_test_lib:close(Server),
%%     ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
no_reuses_session_server_restart_new_cert() ->
    [{doc,"Check that a session is not reused if the server is restarted with a new cert."}].
no_reuses_session_server_restart_new_cert(Config) when is_list(Config) ->

    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    DsaServerOpts = ?config(server_dsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
		      {mfa, {ssl_test_lib, session_info_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client0 =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
			    {mfa, {ssl_test_lib, no_result, []}},
		      {from, self()},  {options, ClientOpts}]),
    SessionInfo =
	receive
	    {Server, Info} ->
		Info
	end,

    %% Make sure session is registered
    ct:sleep(?SLEEP),
    Monitor = erlang:monitor(process, Server),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client0),
    receive
	{'DOWN', Monitor, _, _, _} ->
	    ok
    end,
    
    Server1 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, Port},
				   {from, self()},
		      {mfa, {ssl_test_lib, no_result, []}},
				   {options, DsaServerOpts}]),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
		      {mfa, {ssl_test_lib, session_info_result, []}},
		      {from, self()},  {options, ClientOpts}]),
    receive
	{Client1, SessionInfo} ->
	    ct:fail(session_reused_when_server_has_new_cert);
	{Client1, _Other} ->
	   ok
    end,
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
no_reuses_session_server_restart_new_cert_file() ->
    [{doc,"Check that a session is not reused if a server is restarted with a new "
      "cert contained in a file with the same name as the old cert."}].

no_reuses_session_server_restart_new_cert_file(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),
    DsaServerOpts = ?config(server_dsa_opts, Config),
    PrivDir =  ?config(priv_dir, Config),

    NewServerOpts = new_config(PrivDir, ServerOpts),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
		      {mfa, {ssl_test_lib, session_info_result, []}},
				   {options, NewServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client0 =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
			    {mfa, {ssl_test_lib, no_result, []}},
		      {from, self()},  {options, ClientOpts}]),
    SessionInfo =
	receive
	    {Server, Info} ->
		Info
	end,

    %% Make sure session is registered and we get
    %% new file time stamp when calling new_config!
    ct:sleep(?SLEEP* 2),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client0),

    ssl:clear_pem_cache(),

    NewServerOpts = new_config(PrivDir, DsaServerOpts),

    Server1 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, Port},
				   {from, self()},
		      {mfa, {ssl_test_lib, no_result, []}},
				   {options, NewServerOpts}]),
    Client1 =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
		      {mfa, {ssl_test_lib, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]),
    receive
	{Client1, SessionInfo} ->
	    ct:fail(session_reused_when_server_has_new_cert);
	{Client1, _Other} ->
	   ok
    end,
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
reuseaddr() ->
    [{doc,"Test reuseaddr option"}].

reuseaddr(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options,  [{active, false} | ServerOpts]}]),
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
				   {options,  [{active, false} | ServerOpts]}]),
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
tcp_reuseaddr() ->
    [{doc, "Reference test case."}].
tcp_reuseaddr(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {transport, gen_tcp},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options,  [{active, false}, {reuseaddr, true}]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
                                   {transport, gen_tcp},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, [{active, false}]}]),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    
    Server1 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, Port},
				   {from, self()},
				   {transport, gen_tcp},
				   {mfa, {?MODULE, tcp_send_recv_result, []}},
				   {options,  [{active, false}, {reuseaddr, true}]}]),
    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
                                   {transport, gen_tcp}, 
				   {mfa, {?MODULE, tcp_send_recv_result, []}},
				   {options, [{active, false}]}]),

    ssl_test_lib:check_result(Server1, ok, Client1, ok),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------

hibernate() ->
    [{doc,"Check that an SSL connection that is started with option "
      "{hibernate_after, 1000} indeed hibernates after 1000ms of "
      "inactivity"}].

hibernate(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {Client, #sslsocket{pid=Pid}} = ssl_test_lib:start_client([return_socket,
                    {node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, [{hibernate_after, 1000}|ClientOpts]}]),
    {current_function, _} =
        process_info(Pid, current_function),

    timer:sleep(1100),

    {current_function, {erlang, hibernate, 3}} =
	process_info(Pid, current_function),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
listen_socket() ->
    [{doc,"Check error handling and inet compliance when calling API functions with listen sockets."}].

listen_socket(Config) ->
    ServerOpts = ?config(server_opts, Config),
    {ok, ListenSocket} = ssl:listen(0, ServerOpts),

    %% This can be a valid thing to do as
    %% options are inherited by the accept socket
    ok = ssl:controlling_process(ListenSocket, self()),

    {ok, _} = ssl:sockname(ListenSocket),

    {error, enotconn} = ssl:send(ListenSocket, <<"data">>),
    {error, enotconn} = ssl:recv(ListenSocket, 0),
    {error, enotconn} = ssl:connection_info(ListenSocket),
    {error, enotconn} = ssl:peername(ListenSocket),
    {error, enotconn} = ssl:peercert(ListenSocket),
    {error, enotconn} = ssl:session_info(ListenSocket),
    {error, enotconn} = ssl:renegotiate(ListenSocket),
    {error, enotconn} = ssl:prf(ListenSocket, 'master_secret', <<"Label">>, client_random, 256),
    {error, enotconn} = ssl:shutdown(ListenSocket, read_write),

    ok = ssl:close(ListenSocket).
%%--------------------------------------------------------------------
ssl_accept_timeout() ->
    [{doc,"Test ssl:ssl_accept timeout"}].

ssl_accept_timeout(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),
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
ssl_recv_timeout() ->
    [{doc,"Test ssl:ssl_accept timeout"}].

ssl_recv_timeout(Config) ->
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, send_recv_result_timeout_server, []}},
				   {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       send_recv_result_timeout_client, []}},
					{options, [{active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
connect_twice() ->
    [{doc,""}].
connect_twice(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),

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
    Server ! listen,

    {Client1, #sslsocket{}} =
	ssl_test_lib:start_client([return_socket,
				   {node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{keepalive, true},{active, false}
					      | ClientOpts]}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
			 [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:check_result(Server, ok, Client1, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
renegotiate_dos_mitigate_active() ->
    [{doc, "Mitigate DOS computational attack by not allowing client to renegotiate many times in a row",
      "immediately after each other"}].
renegotiate_dos_mitigate_active(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options, [ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate_immediately, []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
renegotiate_dos_mitigate_passive() ->
    [{doc, "Mitigate DOS computational attack by not allowing client to renegotiate many times in a row",
      "immediately after each other"}].
renegotiate_dos_mitigate_passive(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
 
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       renegotiate_immediately, []}},
					{options, ClientOpts}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok), 
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tcp_error_propagation_in_active_mode() ->
    [{doc,"Test that process recives {ssl_error, Socket, closed} when tcp error ocurres"}].
tcp_error_propagation_in_active_mode(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					 {from, self()},
					 {mfa, {ssl_test_lib, no_result, []}},
					 {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {Client, #sslsocket{pid=Pid} = SslSocket} = ssl_test_lib:start_client([return_socket,
									   {node, ClientNode}, {port, Port},
									   {host, Hostname},
									   {from, self()},
									   {mfa, {?MODULE, receive_msg, []}},
									   {options, ClientOpts}]),

    {status, _, _, StatusInfo} = sys:get_status(Pid),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    Socket = element(11, State),

    %% Fake tcp error
    Pid ! {tcp_error, Socket, etimedout},

    ssl_test_lib:check_result(Client, {ssl_closed, SslSocket}).

%%--------------------------------------------------------------------
recv_error_handling() ->
    [{doc,"Special case of call error handling"}].
recv_error_handling(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					  {from, self()},
					  {mfa, {?MODULE, recv_close, []}},
					 {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, #sslsocket{} = SslSocket} = ssl_test_lib:start_client([return_socket,
									   {node, ClientNode}, {port, Port},
									   {host, Hostname},
									   {from, self()},
									   {mfa, {ssl_test_lib, no_result, []}},
									   {options, ClientOpts}]),
    ssl:close(SslSocket),
    ssl_test_lib:check_result(Server, ok).

%%--------------------------------------------------------------------

rizzo() ->
    [{doc, "Test that there is a 1/n-1-split for non RC4 in 'TLS < 1.1' as it is
    vunrable to Rizzo/Dungon attack"}].

rizzo(Config) when is_list(Config) ->
    Ciphers  = [X || X ={_,Y,_} <- ssl:cipher_suites(), Y  =/= rc4_128],
    Prop = ?config(tc_group_properties, Config),
    Version = proplists:get_value(name, Prop),
    run_send_recv_rizzo(Ciphers, Config, Version,
			 {?MODULE, send_recv_result_active_rizzo, []}).
%%--------------------------------------------------------------------
no_rizzo_rc4() ->
    [{doc,"Test that there is no 1/n-1-split for RC4 as it is not vunrable to Rizzo/Dungon attack"}].

no_rizzo_rc4(Config) when is_list(Config) ->
    Ciphers = [X || X ={_,Y,_} <- ssl:cipher_suites(),Y == rc4_128],
    Prop = ?config(tc_group_properties, Config),
    Version = proplists:get_value(name, Prop),
    run_send_recv_rizzo(Ciphers, Config, Version,
			{?MODULE, send_recv_result_active_no_rizzo, []}).

%%--------------------------------------------------------------------
new_server_wants_peer_cert() ->
    [{doc, "Test that server configured to do client certification does"
      " not reuse session without a client certificate."}].
new_server_wants_peer_cert(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    VServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}
		  | ?config(server_verification_opts, Config)],
    ClientOpts = ?config(client_verification_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, peercert_result, []}},
				   {options,  [ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ClientOpts}]),

    Monitor = erlang:monitor(process, Server),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    receive
	{'DOWN', Monitor, _, _, _} ->
	    ok
    end,
    
    Server1 = ssl_test_lib:start_server([{node, ServerNode}, {port, Port},
					 {from, self()},
					 {mfa, {?MODULE, peercert_result, []}},
					 {options,  VServerOpts}]), 
    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, [ClientOpts]}]),

    CertFile = proplists:get_value(certfile, ClientOpts),
    [{'Certificate', BinCert, _}]= ssl_test_lib:pem_to_der(CertFile),

    ServerMsg = {error, no_peercert},
    Sever1Msg = {ok, BinCert},
   
    ssl_test_lib:check_result(Server, ServerMsg, Server1, Sever1Msg),

    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
session_cache_process_list() ->
    [{doc,"Test reuse of sessions (short handshake)"}].
session_cache_process_list(Config) when is_list(Config) ->
    session_cache_process(list,Config).
%%--------------------------------------------------------------------
session_cache_process_mnesia() ->
    [{doc,"Test reuse of sessions (short handshake)"}].
session_cache_process_mnesia(Config) when is_list(Config) ->
    session_cache_process(mnesia,Config).

%%--------------------------------------------------------------------

versions_option() ->
    [{doc,"Test API versions option to connect/listen."}].
versions_option(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),  

    Supported = proplists:get_value(supported, ssl:versions()),
    Available = proplists:get_value(available, ssl:versions()),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, [{versions, Supported} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ClientOpts}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    Server ! listen,				       
    
    ErrClient = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
						 {host, Hostname},
						 {from, self()},
						 {options, [{versions , Available -- Supported} | ClientOpts]}]),
    receive
	{Server, _} ->
	    ok
    end,	    
   
    ssl_test_lib:check_result(ErrClient, {error, {tls_alert, "protocol version"}}).
%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
send_recv_result(Socket) ->
    ssl:send(Socket, "Hello world"),
    {ok,"Hello world"} = ssl:recv(Socket, 11),
    ok.
tcp_send_recv_result(Socket) ->
    gen_tcp:send(Socket, "Hello world"),
    {ok,"Hello world"} = gen_tcp:recv(Socket, 11),
    ok.

basic_verify_test_no_close(Config) ->
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    {Server, Client}.

basic_test(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

send_recv_result_timeout_client(Socket) ->
    {error, timeout} = ssl:recv(Socket, 11, 500),
    ssl:send(Socket, "Hello world"),
    receive
	Msg ->
	    io:format("Msg ~p~n",[Msg])
    after 500 ->
	    ok
    end,
    {ok, "Hello world"} = ssl:recv(Socket, 11, 500),
    ok.
send_recv_result_timeout_server(Socket) ->
    ssl:send(Socket, "Hello"),
    {ok, "Hello world"} = ssl:recv(Socket, 11),
    ssl:send(Socket, " world"),
    ok.

recv_close(Socket) ->
    {error, closed} = ssl:recv(Socket, 11),
    receive
	{_,{error,closed}} ->
	    error_extra_close_sent_to_user_process
    after 500 ->
	    ok
    end.


send_recv_result_active_rizzo(Socket) ->
    ssl:send(Socket, "Hello world"),
    receive 
	{ssl, Socket, "H"} ->
	    receive 
		{ssl, Socket, "ello world"} ->
		    ok
	    end
    end.

send_recv_result_active_no_rizzo(Socket) ->
    ssl:send(Socket, "Hello world"),
    receive 
	{ssl, Socket, "Hello world"} ->
	    ok
    end.

result_ok(_Socket) ->
    ok.

renegotiate(Socket, Data) ->
    ct:log("Renegotiating ~n", []),
    Result = ssl:renegotiate(Socket),
    ct:log("Result ~p~n", [Result]),
    ssl:send(Socket, Data),
    case Result of
	ok ->
	    ok;
	Other ->
	    Other
    end.

renegotiate_reuse_session(Socket, Data) ->
    %% Make sure session is registered
    ct:sleep(?SLEEP),
    renegotiate(Socket, Data).

renegotiate_immediately(Socket) ->
    receive 
	{ssl, Socket, "Hello world"} ->
	    ok;
	%% Handle 1/n-1 splitting countermeasure Rizzo/Duong-Beast
	{ssl, Socket, "H"} ->
	    receive 
		{ssl, Socket, "ello world"} ->
		    ok
	    end
    end,
    ok = ssl:renegotiate(Socket),  
    {error, renegotiation_rejected} = ssl:renegotiate(Socket),
    ct:sleep(?RENEGOTIATION_DISABLE_TIME +1),
    ok = ssl:renegotiate(Socket),
    ct:log("Renegotiated again"),
    ssl:send(Socket, "Hello world"),
    ok.
    
new_config(PrivDir, ServerOpts0) ->
    CaCertFile = proplists:get_value(cacertfile, ServerOpts0),
    CertFile = proplists:get_value(certfile, ServerOpts0),
    KeyFile = proplists:get_value(keyfile, ServerOpts0),
    NewCaCertFile = filename:join(PrivDir, "new_ca.pem"),
    NewCertFile = filename:join(PrivDir, "new_cert.pem"),
    NewKeyFile = filename:join(PrivDir, "new_key.pem"),
    file:copy(CaCertFile, NewCaCertFile),
    file:copy(CertFile, NewCertFile),
    file:copy(KeyFile, NewKeyFile),
    ServerOpts1 = proplists:delete(cacertfile, ServerOpts0),
    ServerOpts2 = proplists:delete(certfile, ServerOpts1),
    ServerOpts = proplists:delete(keyfile, ServerOpts2),

    {ok, PEM} = file:read_file(NewCaCertFile),
    ct:log("CA file content: ~p~n", [public_key:pem_decode(PEM)]),

    [{cacertfile, NewCaCertFile}, {certfile, NewCertFile},
     {keyfile, NewKeyFile} | ServerOpts].

session_cache_process(_Type,Config) when is_list(Config) ->
    reuse_session(Config).

init([Type]) ->
    ets:new(ssl_test, [named_table, public, set]),
    ets:insert(ssl_test, {type, Type}),
    case Type of
	list ->
	    spawn(fun() -> session_loop([]) end);
	mnesia ->
	    mnesia:start(),
	    {atomic,ok} = mnesia:create_table(sess_cache, []),
	    sess_cache
    end.

session_cb() ->
    [{type, Type}] = ets:lookup(ssl_test, type),
    Type.

terminate(Cache) ->
    case session_cb() of
	list ->
	    Cache ! terminate;
	mnesia ->
	    catch {atomic,ok} = 
		mnesia:delete_table(sess_cache)
    end.

lookup(Cache, Key) ->        
    case session_cb() of
	list ->
	    Cache ! {self(), lookup, Key},
	    receive {Cache, Res} -> Res end;
	mnesia ->
	    case mnesia:transaction(fun() -> 
					    mnesia:read(sess_cache, 
							Key, read) 
				    end) of
		{atomic, [{sess_cache, Key, Value}]} -> 
		    Value;
		_ -> 
		    undefined
	    end
	end.

update(Cache, Key, Value) ->
    case session_cb() of
	list ->
	    Cache ! {update, Key, Value};
	mnesia ->
	    {atomic, ok} = 
		mnesia:transaction(fun() -> 
					   mnesia:write(sess_cache, 
							{sess_cache, Key, Value}, write) 
				   end)
    end.

delete(Cache, Key) -> 
    case session_cb() of
	list ->
	    Cache ! {delete, Key};
	mnesia ->
	    {atomic, ok} = 
		mnesia:transaction(fun() -> 
					   mnesia:delete(sess_cache, Key) 
				   end)
    end.

foldl(Fun, Acc, Cache) -> 
    case session_cb() of
	list ->
	    Cache ! {self(),foldl,Fun,Acc},
	    receive {Cache, Res} -> Res end;
	mnesia ->
	    Foldl = fun() ->
			    mnesia:foldl(Fun, Acc, sess_cache)
		    end,
	    {atomic, Res} = mnesia:transaction(Foldl),
	    Res
    end.
    
select_session(Cache, PartialKey) ->
    case session_cb() of
	list ->
	    Cache ! {self(),select_session, PartialKey},
	    receive 
		{Cache, Res} -> 
		    Res 
	    end;
	mnesia ->
	    Sel = fun() ->
			  mnesia:select(Cache,
					[{{sess_cache,{PartialKey,'$1'}, '$2'},
					  [],['$$']}])
		  end,
	    {atomic, Res} = mnesia:transaction(Sel),
	    Res
    end.

session_loop(Sess) ->
    receive 
	terminate ->
	    ok;
	{Pid, lookup, Key} ->
	    case lists:keysearch(Key,1,Sess) of
		{value, {Key,Value}} ->
		    Pid ! {self(), Value};
		_ -> 
		    Pid ! {self(), undefined}
	    end,
	    session_loop(Sess);
	{update, Key, Value} ->
	    NewSess = [{Key,Value}| lists:keydelete(Key,1,Sess)],
	    session_loop(NewSess);
	{delete, Key} ->
	    session_loop(lists:keydelete(Key,1,Sess));
	{Pid,foldl,Fun,Acc} ->
	    Res = lists:foldl(Fun, Acc,Sess),
	    Pid ! {self(), Res},
	    session_loop(Sess);
	{Pid,select_session,PKey} ->
	    Sel = fun({{PKey0, Id},Session}, Acc) when PKey == PKey0 -> 
			  [[Id, Session]|Acc];
		     (_,Acc) -> 
			  Acc
		  end, 
	    Sessions = lists:foldl(Sel, [], Sess),
	    Pid ! {self(), Sessions},
	    session_loop(Sess)
    end.
	    

erlang_ssl_receive(Socket, Data) ->
    receive
	{ssl, Socket, Data} ->
	    io:format("Received ~p~n",[Data]),
	    ok;
	{ssl, Socket, Byte} when length(Byte) == 1 ->  %% Handle 1/n-1 splitting countermeasure Rizzo/Duong-Beast
	    io:format("Received ~p~n",[Byte]),
	    erlang_ssl_receive(Socket, tl(Data));
	Other ->
	    ct:fail({unexpected_message, Other})
    after ?SLEEP * 3 *  test_server:timetrap_scale_factor() ->
	    ct:fail({did_not_get, Data})
    end.

receive_msg(_) ->
    receive
	Msg ->
	   Msg
    end.

controlling_process_result(Socket, Pid, Msg) ->
    ok = ssl:controlling_process(Socket, Pid),
    %% Make sure other side has evaluated controlling_process
    %% before message is sent
    ct:sleep(?SLEEP),
    ssl:send(Socket, Msg),
    no_result_msg.

receive_s_rizzo_duong_beast() ->
    receive
	{ssl, _, "erver hello"} ->
	    receive
		{ssl, _, "C"} ->
		    receive
			{ssl, _, "lient hello"} ->
			    ok
		    end
	    end
    end.
receive_c_rizzo_duong_beast() ->
    receive
	{ssl, _, "lient hello"} ->
	    receive
		{ssl, _, "S"} ->
		    receive
			{ssl, _, "erver hello"} ->
			    ok
		    end
	    end
    end.

controller_dies_result(_Socket, _Pid, _Msg) ->
    receive Result -> Result end.

get_close(Pid, Where) ->
    receive
	{'EXIT', Pid, _Reason} ->
	    receive
		{_, {ssl_closed, Socket}} ->
		    ct:log("Socket closed ~p~n",[Socket]);
		Unexpected ->
		    ct:log("Unexpected ~p~n",[Unexpected]),
		    ct:fail({line, ?LINE-1})
	    after 5000 ->
		    ct:fail({timeout, {line, ?LINE, Where}})
	    end;
	Unexpected ->
	    ct:log("Unexpected ~p~n",[Unexpected]),
	    ct:fail({line, ?LINE-1})
    after 5000 ->
	    ct:fail({timeout, {line, ?LINE, Where}})
    end.

run_send_recv_rizzo(Ciphers, Config, Version, Mfa) ->
    Result =  lists:map(fun(Cipher) ->
				rizzo_test(Cipher, Config, Version, Mfa) end,
			Ciphers),
    case lists:flatten(Result) of
	[] ->
	    ok;
	Error ->
	    ct:log("Cipher suite errors: ~p~n", [Error]),
	    ct:fail(cipher_suite_failed_see_test_case_log)
    end.

rizzo_test(Cipher, Config, Version, Mfa) ->
   {ClientOpts, ServerOpts} = client_server_opts(Cipher, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, Mfa},
			   {options, [{active, true}, {ciphers, [Cipher]},
				       {versions, [Version]}
				      | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, Mfa},
			   {options, [{active, true} | ClientOpts]}]),

    Result = ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    case Result of
	ok ->
	    [];
	Error ->
	    [{Cipher, Error}]
    end.

client_server_opts({KeyAlgo,_,_}, Config)
  when KeyAlgo == rsa orelse
       KeyAlgo == dhe_rsa orelse
       KeyAlgo == ecdhe_rsa ->
    {?config(client_opts, Config),
     ?config(server_opts, Config)};
client_server_opts({KeyAlgo,_,_}, Config) when KeyAlgo == dss orelse KeyAlgo == dhe_dss ->
    {?config(client_dsa_opts, Config),
     ?config(server_dsa_opts, Config)};
client_server_opts({KeyAlgo,_,_}, Config) when KeyAlgo == ecdh_ecdsa orelse KeyAlgo == ecdhe_ecdsa ->
    {?config(client_opts, Config),
     ?config(server_ecdsa_opts, Config)};
client_server_opts({KeyAlgo,_,_}, Config) when KeyAlgo == ecdh_rsa ->
    {?config(client_opts, Config),
     ?config(server_ecdh_rsa_opts, Config)}.

run_suites(Ciphers, Version, Config, Type) ->
    {ClientOpts, ServerOpts} =
	case Type of
	    rsa ->
		{?config(client_opts, Config),
		 ?config(server_opts, Config)};
	    dsa ->
		{?config(client_opts, Config),
		 ?config(server_dsa_opts, Config)};
	    anonymous ->
		%% No certs in opts!
		{?config(client_opts, Config),
		 ?config(server_anon, Config)};
	    psk ->
		{?config(client_psk, Config),
		 ?config(server_psk, Config)};
	    psk_with_hint ->
		{?config(client_psk, Config),
		 ?config(server_psk_hint, Config)};
	    psk_anon ->
		{?config(client_psk, Config),
		 ?config(server_psk_anon, Config)};
	    psk_anon_with_hint ->
		{?config(client_psk, Config),
		 ?config(server_psk_anon_hint, Config)};
	    srp ->
		{?config(client_srp, Config),
		 ?config(server_srp, Config)};
	    srp_anon ->
		{?config(client_srp, Config),
		 ?config(server_srp_anon, Config)};
	    srp_dsa ->
		{?config(client_srp_dsa, Config),
		 ?config(server_srp_dsa, Config)};
	    ecdsa ->
		{?config(client_opts, Config),
		 ?config(server_ecdsa_opts, Config)};
	    ecdh_rsa ->
		{?config(client_opts, Config),
		 ?config(server_ecdh_rsa_opts, Config)}
	    end,

    Result =  lists:map(fun(Cipher) ->
				cipher(Cipher, Version, Config, ClientOpts, ServerOpts) end,
			Ciphers),
    case lists:flatten(Result) of
	[] ->
	    ok;
	Error ->
	    ct:log("Cipher suite errors: ~p~n", [Error]),
	    ct:fail(cipher_suite_failed_see_test_case_log)
    end.

erlang_cipher_suite(Suite) when is_list(Suite)->
    ssl:suite_definition(ssl_cipher:openssl_suite(Suite));
erlang_cipher_suite(Suite) ->
    Suite.

cipher(CipherSuite, Version, Config, ClientOpts, ServerOpts) ->
    %% process_flag(trap_exit, true),
    ct:log("Testing CipherSuite ~p~n", [CipherSuite]),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    ErlangCipherSuite = erlang_cipher_suite(CipherSuite),

    ConnectionInfo = {ok, {Version, ErlangCipherSuite}},

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {ssl_test_lib, cipher_result, [ConnectionInfo]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, cipher_result, [ConnectionInfo]}},
			   {options,
			    [{ciphers,[CipherSuite]} |
			     ClientOpts]}]),

    Result = ssl_test_lib:wait_for_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),

    case Result of
	ok ->
	    [];
	Error ->
	    [{ErlangCipherSuite, Error}]
    end.

connection_info_result(Socket) ->
    ssl:connection_info(Socket).

connect_dist_s(S) ->
    Msg = term_to_binary({erlang,term}),
    ok = ssl:send(S, Msg).

connect_dist_c(S) ->
    Test = binary_to_list(term_to_binary({erlang,term})),
    {ok, Test} = ssl:recv(S, 0, 10000),
    ok.

 %% First two clauses handles 1/n-1 splitting countermeasure Rizzo/Duong-Beast
treashold(N, {3,0}) ->
    (N div 2) + 1;
treashold(N, {3,1}) ->
    (N div 2) + 1;
treashold(N, _) ->
    N + 1.

get_invalid_inet_option(Socket) ->
    {error, {options, {socket_options, foo, _}}} = ssl:getopts(Socket, [foo]),
    ok.

shutdown_result(Socket, server) ->
    ssl:send(Socket, "Hej"),
    ssl:shutdown(Socket, write),
    {ok, "Hej hopp"} = ssl:recv(Socket, 8),
    ok;

shutdown_result(Socket, client) ->
    {ok, "Hej"} = ssl:recv(Socket, 3),
    ssl:send(Socket, "Hej hopp"),
    ssl:shutdown(Socket, write),
    ok.

shutdown_write_result(Socket, server) ->
    ct:sleep(?SLEEP),
    ssl:shutdown(Socket, write);
shutdown_write_result(Socket, client) ->
    ssl:recv(Socket, 0).

dummy(_Socket) ->
    %% Should not happen as the ssl connection will not be established
    %% due to fatal handshake failiure
    exit(kill).

shutdown_both_result(Socket, server) ->
    ct:sleep(?SLEEP),
    ssl:shutdown(Socket, read_write);
shutdown_both_result(Socket, client) ->
    ssl:recv(Socket, 0).

peername_result(S) ->
    ssl:peername(S).
