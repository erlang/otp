%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2012. All Rights Reserved.
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

-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("ssl_handshake.hrl").

-define('24H_in_sec', 86400).  
-define(TIMEOUT, 60000).
-define(LONG_TIMEOUT, 600000).
-define(EXPIRE, 10).
-define(SLEEP, 500).
-define(RENEGOTIATION_DISABLE_TIME, 12000).
-define(CLEAN_SESSION_DB, 60000).

%% Test server callback functions
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initialization before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    Dog = ssl_test_lib:timetrap(?LONG_TIMEOUT *2),
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    application:start(public_key),

	    %% make rsa certs using oppenssl
	    Result =
		(catch make_certs:all(?config(data_dir, Config0),
				      ?config(priv_dir, Config0))),
	    test_server:format("Make certs  ~p~n", [Result]),

	    Config1 = ssl_test_lib:make_dsa_cert(Config0),
	    Config = ssl_test_lib:cert_options(Config1),
	    [{watchdog, Dog} | Config]
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Description: Initialization before each test case
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
    test_server:format("TLS/SSL version ~p~n ", [ssl_record:supported_protocol_versions()]),
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = test_server:timetrap(?TIMEOUT),
   [{watchdog, Dog} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(reuse_session_expired, Config) ->
    application:unset_env(ssl, session_lifetime),
    application:unset_env(ssl, session_delay_cleanup_time),
    end_per_testcase(default_action, Config);

end_per_testcase(_TestCase, Config) ->
    Dog = ?config(watchdog, Config),
    case Dog of 
	undefined ->
	    ok;
	_ ->
	    test_server:timetrap_cancel(Dog)
    end.

%%--------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case] 
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
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
     {certificate_verify, [], certificate_verify_tests()},
     {session, [], session_tests()},
     {renegotiate, [], renegotiate_tests()},
     {ciphers, [], cipher_tests()},
     {error_handling_tests, [], error_handling_tests()}
    ].

all_versions_groups ()->
    [{group, api},
     {group, certificate_verify},
     {group, renegotiate},
     {group, ciphers},
     {group, error_handling_tests}].

init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
	true ->
	    case ssl_test_lib:sufficient_crypto_support(GroupName) of
		true ->
		    ssl_test_lib:init_tls_version(GroupName),
		    Config;
		false ->
		    {skip, "Missing crypto support"}
	    end;
	_ ->
	    ssl:start(),
	    Config
    end.


end_per_group(_GroupName, Config) ->
    Config.

basic_tests() ->
    [app,
     alerts,
     send_close,
     connect_twice,
     connect_dist
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
     ecertfile,
     ecacertfile,
     ekeyfile,
     eoptions,
     protocol_versions,
     empty_protocol_versions,
     ipv6,
     reuseaddr].

api_tests() ->
    [connection_info,
     peername,
     peercert,
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
     ssl_accept_timeout,
     ssl_recv_timeout
    ].

certificate_verify_tests() ->
    [server_verify_peer_passive,
     server_verify_peer_active,
     server_verify_peer_active_once,
     server_verify_none_passive,
     server_verify_none_active,
     server_verify_none_active_once,
     server_verify_no_cacerts,
     server_require_peer_cert_ok,
     server_require_peer_cert_fail,
     server_verify_client_once_passive,
     server_verify_client_once_active,
     server_verify_client_once_active_once,
     client_verify_none_passive,
     client_verify_none_active,
     client_verify_none_active_once,
     extended_key_usage_verify_peer,
     extended_key_usage_verify_none,
     invalid_signature_client,
     invalid_signature_server,
     cert_expired,
     client_with_cert_cipher_suites_handshake,
     verify_fun_always_run_client,
     verify_fun_always_run_server,
     unknown_server_ca_fail,
     unknown_server_ca_accept_verify_none,
     unknown_server_ca_accept_verify_peer,
     unknown_server_ca_accept_backwardscompatibility,
     no_authority_key_identifier
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
     default_reject_anonymous].

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

%% Test cases starts here.
%%--------------------------------------------------------------------
app(doc) ->
    "Test that the ssl app file is ok";
app(suite) ->
    [];
app(Config) when is_list(Config) ->
    ok = test_server:app_test(ssl).
%%--------------------------------------------------------------------
alerts(doc) ->
    "Test ssl_alert:alert_txt/1";
alerts(suite) ->
    [];
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
				test_server:fail({unexpected, Other})
			end 
		  end, Alerts).
%%--------------------------------------------------------------------
connection_info(doc) -> 
    ["Test the API function ssl:connection_info/1"];
connection_info(suite) -> 
    [];
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
    
    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", 
		       [self(), Client, Server]),

    Version = 
	ssl_record:protocol_version(ssl_record:highest_protocol_version([])),
    
    ServerMsg = ClientMsg = {ok, {Version, {rsa,rc4_128,sha}}},
			   
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

connection_info_result(Socket) ->                                            
    ssl:connection_info(Socket).

%%--------------------------------------------------------------------

protocol_versions(doc) -> 
    ["Test to set a list of protocol versions in app environment."];

protocol_versions(suite) -> 
    [];

protocol_versions(Config) when is_list(Config) -> 
    basic_test(Config).

empty_protocol_versions(doc) -> 
    ["Test to set an empty list of protocol versions in app environment."];

empty_protocol_versions(suite) -> 
    [];

empty_protocol_versions(Config) when is_list(Config) -> 
    basic_test(Config).


basic_test(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, send_recv_result_active, []}},
					{options, ClientOpts}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------

controlling_process(doc) -> 
    ["Test API function controlling_process/2"];

controlling_process(suite) -> 
    [];

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
   
    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", 
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
	    test_server:fail(Unexpected)
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

controlling_process_result(Socket, Pid, Msg) ->
    ok = ssl:controlling_process(Socket, Pid),
    %% Make sure other side has evaluated controlling_process
    %% before message is sent
    test_server:sleep(?SLEEP),
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
%%--------------------------------------------------------------------
controller_dies(doc) -> 
    ["Test that the socket is closed after controlling process dies"];
controller_dies(suite) -> [];
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

    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", [self(), Client, Server]),
    test_server:sleep(?SLEEP), %% so that they are connected
    
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
		      test_server:sleep(?SLEEP),
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

    test_server:format("Wating on exit ~p~n",[Client3]),
    receive {'EXIT', Client3, normal} -> ok end,
    
    receive   %% Client3 is dead but that doesn't matter, socket should not be closed.
	Unexpected ->
	    test_server:format("Unexpected ~p~n",[Unexpected]),
	    test_server:fail({line, ?LINE-1})
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
    test_server:sleep(?SLEEP), %% so that they are connected
    
    exit(Server, killed),
    get_close(Server, ?LINE),
    process_flag(trap_exit, false),
    ssl_test_lib:close(LastClient).

controller_dies_result(_Socket, _Pid, _Msg) ->
    receive Result -> Result end.

get_close(Pid, Where) ->
    receive 
	{'EXIT', Pid, _Reason} ->
	    receive 
		{_, {ssl_closed, Socket}} ->
		    test_server:format("Socket closed ~p~n",[Socket]);
		Unexpected ->
		    test_server:format("Unexpected ~p~n",[Unexpected]),
		    test_server:fail({line, ?LINE-1})
	    after 5000 ->
		    test_server:fail({timeout, {line, ?LINE, Where}})
	    end;
	Unexpected ->
	    test_server:format("Unexpected ~p~n",[Unexpected]),
	    test_server:fail({line, ?LINE-1})
    after 5000 ->
	    test_server:fail({timeout, {line, ?LINE, Where}})
    end.  

%%--------------------------------------------------------------------
client_closes_socket(doc) -> 
    ["Test what happens when client closes socket before handshake is compleated"];
client_closes_socket(suite) -> [];
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
		      test_server:sleep(?SLEEP)
	      end,
    
    _Client = spawn_link(Connect),

    ssl_test_lib:check_result(Server, {error,closed}).

%%--------------------------------------------------------------------
connect_dist(doc) -> 
    ["Test a simple connect as is used by distribution"];

connect_dist(suite) -> 
    [];

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

connect_dist_s(S) ->
    Msg = term_to_binary({erlang,term}),
    ok = ssl:send(S, Msg).

connect_dist_c(S) ->
    Test = binary_to_list(term_to_binary({erlang,term})),
    {ok, Test} = ssl:recv(S, 0, 10000),
    ok.


%%--------------------------------------------------------------------
peername(doc) -> 
    ["Test API function peername/1"];

peername(suite) -> 
    [];

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
	
    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", 
		       [self(), Client, Server]),
		   
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

peername_result(S) ->
    ssl:peername(S).

%%--------------------------------------------------------------------
peercert(doc) ->
    [""];
peercert(suite) ->
    [];
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

    test_server:format("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

peercert_result(Socket) ->
    ssl:peercert(Socket).

%%--------------------------------------------------------------------
sockname(doc) -> 
    ["Test API function sockname/1"];

sockname(suite) -> 
    [];

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
			   
    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", 
			 [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

sockname_result(S) ->
    ssl:sockname(S).

%%--------------------------------------------------------------------
cipher_suites(doc) -> 
    ["Test API function cipher_suites/0"];

cipher_suites(suite) -> 
    [];

cipher_suites(Config) when is_list(Config) -> 
    MandatoryCipherSuite = {rsa,'3des_ede_cbc',sha},
    [_|_] = Suites = ssl:cipher_suites(),
    true = lists:member(MandatoryCipherSuite, Suites),
    Suites = ssl:cipher_suites(erlang),
    [_|_] =ssl:cipher_suites(openssl).

%%--------------------------------------------------------------------
socket_options(doc) -> 
    ["Test API function getopts/2 and setopts/2"];

socket_options(suite) -> 
    [];

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
    test_server:format("All opts ~p~n", [All]),
    ok.


%%--------------------------------------------------------------------
invalid_inet_get_option(doc) ->
    ["Test handling of invalid inet options in getopts"];

invalid_inet_get_option(suite) ->
    [];

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

    test_server:format("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


get_invalid_inet_option(Socket) ->
    {error, {eoptions, {inet_option, foo, _}}} = ssl:getopts(Socket, [foo]),
    ok.

%%--------------------------------------------------------------------
invalid_inet_get_option_not_list(doc) ->
    ["Test handling of invalid type in getopts"];

invalid_inet_get_option_not_list(suite) ->
    [];

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

    test_server:format("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


get_invalid_inet_option_not_list(Socket) ->
    {error, {eoptions, {inet_options, some_invalid_atom_here}}}
     = ssl:getopts(Socket, some_invalid_atom_here),
     ok.

%%--------------------------------------------------------------------
invalid_inet_get_option_improper_list(doc) ->
    ["Test handling of invalid type in getopts"];

invalid_inet_get_option_improper_list(suite) ->
    [];

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

    test_server:format("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


get_invalid_inet_option_improper_list(Socket) ->
    {error, {eoptions, {inet_option, foo,_}}} = ssl:getopts(Socket, [packet | foo]),
    ok.

%%--------------------------------------------------------------------
invalid_inet_set_option(doc) ->
    ["Test handling of invalid inet options in setopts"];

invalid_inet_set_option(suite) ->
    [];

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

    test_server:format("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

set_invalid_inet_option(Socket) ->
    {error, {eoptions, {inet_opt, {packet, foo}}}} = ssl:setopts(Socket, [{packet, foo}]),
    {error, {eoptions, {inet_opt, {header, foo}}}} = ssl:setopts(Socket, [{header, foo}]),
    {error, {eoptions, {inet_opt, {active, foo}}}} = ssl:setopts(Socket, [{active, foo}]),
    {error, {eoptions, {inet_opt, {mode, foo}}}}   = ssl:setopts(Socket, [{mode, foo}]),
    ok.
%%--------------------------------------------------------------------
invalid_inet_set_option_not_list(doc) ->
    ["Test handling of invalid type in setopts"];

invalid_inet_set_option_not_list(suite) ->
    [];

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

    test_server:format("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


set_invalid_inet_option_not_list(Socket) ->
    {error, {eoptions, {not_a_proplist, some_invalid_atom_here}}}
	= ssl:setopts(Socket, some_invalid_atom_here),
    ok.

%%--------------------------------------------------------------------
invalid_inet_set_option_improper_list(doc) ->
    ["Test handling of invalid tye in setopts"];

invalid_inet_set_option_improper_list(suite) ->
    [];

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

    test_server:format("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

set_invalid_inet_option_improper_list(Socket) ->
    {error, {eoptions, {not_a_proplist, [{packet, 0} | {foo, 2}]}}} =
	ssl:setopts(Socket, [{packet, 0} | {foo, 2}]),
    ok.

%%--------------------------------------------------------------------
misc_ssl_options(doc) ->
    ["Test what happens when we give valid options"];

misc_ssl_options(suite) ->
    [];

misc_ssl_options(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    %% Chek that ssl options not tested elsewhere are filtered away e.i. not passed to inet.
    TestOpts = [{depth, 1}, 
		{key, undefined}, 
		{password, []},
		{reuse_session, fun(_,_,_,_) -> true end},
		{debug, []}, 
		{cb_info, {gen_tcp, tcp, tcp_closed, tcp_error}}],
    
   Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {?MODULE, send_recv_result_active, []}},
				   {options,  TestOpts ++ ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {?MODULE, send_recv_result_active, []}},
				   {options, TestOpts ++ ClientOpts}]),

    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", 
			 [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
versions(doc) -> 
    ["Test API function versions/0"];

versions(suite) -> 
    [];

versions(Config) when is_list(Config) -> 
    [_|_] = Versions = ssl:versions(),
    test_server:format("~p~n", [Versions]).

%%--------------------------------------------------------------------
send_recv(doc) -> 
    [""];

send_recv(suite) -> 
    [];

send_recv(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {?MODULE, send_recv_result, []}},
				   {options,  [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {?MODULE, send_recv_result, []}},
				   {options, [{active, false} | ClientOpts]}]),
    
    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", 
			 [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
send_close(doc) -> 
    [""];

send_close(suite) -> 
    [];

send_close(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {?MODULE, send_recv_result, []}},
				   {options,  [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, TcpS} = rpc:call(ClientNode, gen_tcp, connect, 
			  [Hostname,Port,[binary, {active, false}, {reuseaddr, true}]]),
    {ok, SslS} = rpc:call(ClientNode, ssl, connect, 
			  [TcpS,[{active, false}|ClientOpts]]),
    
    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", 
		       [self(), self(), Server]),
    ok = ssl:send(SslS, "Hello world"),      
    {ok,<<"Hello world">>} = ssl:recv(SslS, 11),    
    gen_tcp:close(TcpS),    
    {error, _} = ssl:send(SslS, "Hello world").

%%--------------------------------------------------------------------
close_transport_accept(doc) ->
    ["Tests closing ssl socket when waiting on ssl:transport_accept/1"];

close_transport_accept(suite) ->
    [];

close_transport_accept(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Port = 0,
    Opts = [{active, false} | ServerOpts],
    {ok, ListenSocket} = rpc:call(ServerNode, ssl, listen, [Port, Opts]),
    spawn_link(fun() ->
			test_server:sleep(?SLEEP),
			rpc:call(ServerNode, ssl, close, [ListenSocket])
	       end),
    case rpc:call(ServerNode, ssl, transport_accept, [ListenSocket]) of
	{error, closed} ->
	    ok;
	Other ->
	    exit({?LINE, Other})
    end.

%%--------------------------------------------------------------------
dh_params(doc) -> 
    ["Test to specify DH-params file in server."];

dh_params(suite) -> 
    [];

dh_params(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    DataDir = ?config(data_dir, Config),
    DHParamFile = filename:join(DataDir, "dHParam.pem"),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active, []}},
			   {options, [{dhfile, DHParamFile} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active, []}},
			   {options,
			    [{ciphers,[{dhe_rsa,aes_256_cbc,sha,ignore}]} | 
				       ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
upgrade(doc) -> 
    ["Test that you can upgrade an tcp connection to an ssl connection"];

upgrade(suite) -> 
    [];

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
    
    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", 
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
upgrade_with_timeout(doc) -> 
    ["Test ssl_accept/3"];

upgrade_with_timeout(suite) -> 
    [];

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
    
    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", 
		       [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tcp_connect(doc) ->
    ["Test what happens when a tcp tries to connect, i,e. a bad (ssl) packet is sent first"];

tcp_connect(suite) ->
    [];

tcp_connect(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server([{node, ServerNode}, {port, 0},
						{from, self()},
						{timeout, 5000},
						{mfa, {?MODULE, dummy, []}},
						{tcp_options, TcpOpts},
						{ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {packet, 0}]),
    test_server:format("Testcase ~p connected to Server ~p ~n", [self(), Server]),
    gen_tcp:send(Socket, "<SOME GARBLED NON SSL MESSAGE>"),

    receive 
	{tcp_closed, Socket} ->
	    receive 
		{Server, {error, Error}} ->
		    test_server:format("Error ~p", [Error])
	    end
    end.

tcp_connect_big(doc) ->
    ["Test what happens when a tcp tries to connect, i,e. a bad big (ssl) packet is sent first"];

tcp_connect_big(suite) ->
    [];

tcp_connect_big(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server([{node, ServerNode}, {port, 0},
						{from, self()},
						{timeout, 5000},
						{mfa, {?MODULE, dummy, []}},
						{tcp_options, TcpOpts},
						{ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {packet, 0}]),
    test_server:format("Testcase ~p connected to Server ~p ~n", [self(), Server]),

    Rand = crypto:rand_bytes(?MAX_CIPHER_TEXT_LENGTH+1),
    gen_tcp:send(Socket, <<?BYTE(0),
			   ?BYTE(3), ?BYTE(1), ?UINT16(?MAX_CIPHER_TEXT_LENGTH), Rand/binary>>),

    receive
	{tcp_closed, Socket} ->
	    receive
		{Server, {error, timeout}} ->
		    test_server:fail("hangs");
		{Server, {error, Error}} ->
		    test_server:format("Error ~p", [Error])
	    end
    end.

dummy(_Socket) ->
    %% Should not happen as the ssl connection will not be established
    %% due to fatal handshake failiure
    exit(kill).

%%--------------------------------------------------------------------
ipv6() ->
    [{require, ipv6_hosts}].
ipv6(doc) ->
    ["Test ipv6."];
ipv6(suite) ->
    [];
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
				   {mfa, {?MODULE, send_recv_result, []}},
				   {options,  
				    [inet6, {active, false} | ServerOpts]}]),
	    Port = ssl_test_lib:inet_port(Server), 
	    Client = ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port}, {host, Hostname},
				   {from, self()}, 
				   {mfa, {?MODULE, send_recv_result, []}},
				   {options, 
				    [inet6, {active, false} | ClientOpts]}]),
	    
	    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", 
			       [self(), Client, Server]),
	    
	    ssl_test_lib:check_result(Server, ok, Client, ok),
	    
	    ssl_test_lib:close(Server),
	    ssl_test_lib:close(Client);
	false ->
	    {skip, "Host does not support IPv6"}
    end.

%%--------------------------------------------------------------------

ekeyfile(doc) -> 
    ["Test what happens with an invalid key file"];

ekeyfile(suite) -> 
    [];

ekeyfile(Config) when is_list(Config) -> 
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
    
    ssl_test_lib:check_result(Server, {error, ekeyfile}, Client,  
			      {error, closed}).

%%--------------------------------------------------------------------

ecertfile(doc) -> 
    ["Test what happens with an invalid cert file"];

ecertfile(suite) -> 
    [];

ecertfile(Config) when is_list(Config) -> 
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
    
    ssl_test_lib:check_result(Server, {error, ecertfile}, Client, 
			      {error, closed}).
    

%%--------------------------------------------------------------------
ecacertfile(doc) ->
    ["Test what happens with an invalid cacert file"];

ecacertfile(suite) ->
    [];

ecacertfile(Config) when is_list(Config) ->
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
    
    ssl_test_lib:check_result(Server0, {error, ecacertfile},
			      Client0, {error, closed}),
    
    File0 = proplists:get_value(cacertfile, ServerBadOpts),
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

    ssl_test_lib:check_result(Server1, {error, ecacertfile},
			      Client1, {error, closed}),
    ok.
    
    

%%--------------------------------------------------------------------
eoptions(doc) -> 
    ["Test what happens when we give invalid options"];
       
eoptions(suite) -> 
    [];

eoptions(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Check = fun(Client, Server, {versions, [sslv2, sslv3]} = Option) ->
		    ssl_test_lib:check_result(Server, 
					      {error, {eoptions, {sslv2, Option}}}, 
					      Client,
					      {error, {eoptions, {sslv2, Option}}});
	       (Client, Server, Option) ->
		    ssl_test_lib:check_result(Server, 
					      {error, {eoptions, Option}}, 
					      Client,
					      {error, {eoptions, Option}})
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
		{debug, 1},
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
shutdown(doc) -> 
    [""];
       
shutdown(suite) -> 
    [];

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

%%--------------------------------------------------------------------
shutdown_write(doc) -> 
    [""];
       
shutdown_write(suite) -> 
    [];

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
    
shutdown_write_result(Socket, server) ->
    test_server:sleep(?SLEEP),
    ssl:shutdown(Socket, write);
shutdown_write_result(Socket, client) ->    
    ssl:recv(Socket, 0).

%%--------------------------------------------------------------------
shutdown_both(doc) -> 
    [""];
       
shutdown_both(suite) -> 
    [];

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

shutdown_both_result(Socket, server) ->
    test_server:sleep(?SLEEP),
    ssl:shutdown(Socket, read_write);
shutdown_both_result(Socket, client) ->    
    ssl:recv(Socket, 0).

%%--------------------------------------------------------------------
shutdown_error(doc) -> 
    [""];
       
shutdown_error(suite) -> 
    [];

shutdown_error(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    Port = ssl_test_lib:inet_port(node()),
    {ok, Listen} = ssl:listen(Port, ServerOpts),
    {error, enotconn} = ssl:shutdown(Listen, read_write),
    ok = ssl:close(Listen),
    {error, closed} = ssl:shutdown(Listen, read_write).

%%-------------------------------------------------------------------
ciphers_rsa_signed_certs(doc) -> 
    ["Test all rsa ssl cipher suites in highest support ssl/tls version"];
       
ciphers_rsa_signed_certs(suite) -> 
    [];

ciphers_rsa_signed_certs(Config) when is_list(Config) ->
    Version = 
	ssl_record:protocol_version(ssl_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:rsa_suites(),
    test_server:format("~p erlang cipher suites ~p~n", [Version, Ciphers]),
    run_suites(Ciphers, Version, Config, rsa).

ciphers_rsa_signed_certs_openssl_names(doc) -> 
    ["Test all rsa ssl cipher suites in highest support ssl/tls version"];
       
ciphers_rsa_signed_certs_openssl_names(suite) -> 
    [];

ciphers_rsa_signed_certs_openssl_names(Config) when is_list(Config) ->
    Version = 
	ssl_record:protocol_version(ssl_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:openssl_rsa_suites(),  
    test_server:format("tls1 openssl cipher suites ~p~n", [Ciphers]),
    run_suites(Ciphers, Version, Config, rsa).


ciphers_dsa_signed_certs(doc) -> 
    ["Test all dsa ssl cipher suites in highest support ssl/tls version"];
       
ciphers_dsa_signed_certs(suite) -> 
    [];

ciphers_dsa_signed_certs(Config) when is_list(Config) ->
    Version = 
	ssl_record:protocol_version(ssl_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:dsa_suites(),
    test_server:format("~p erlang cipher suites ~p~n", [Version, Ciphers]),
    run_suites(Ciphers, Version, Config, dsa).

ciphers_dsa_signed_certs_openssl_names(doc) -> 
    ["Test all dsa ssl cipher suites in highest support ssl/tls version"];
       
ciphers_dsa_signed_certs_openssl_names(suite) -> 
    [];

ciphers_dsa_signed_certs_openssl_names(Config) when is_list(Config) ->
    Version = 
	ssl_record:protocol_version(ssl_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:openssl_dsa_suites(),
    test_server:format("tls1 openssl cipher suites ~p~n", [Ciphers]),
    run_suites(Ciphers, Version, Config, dsa).

anonymous_cipher_suites(doc)->
    ["Test the anonymous ciphersuites"];
anonymous_cipher_suites(suite) ->
    [];
anonymous_cipher_suites(Config) when is_list(Config) ->
    Version = ssl_record:protocol_version(ssl_record:highest_protocol_version([])),
    Ciphers = ssl_test_lib:anonymous_suites(),
    run_suites(Ciphers, Version, Config, anonymous).

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
		 ?config(server_anon, Config)}
	    end,
    
    Result =  lists:map(fun(Cipher) -> 
				cipher(Cipher, Version, Config, ClientOpts, ServerOpts) end,
			Ciphers),
    case lists:flatten(Result) of
	[] ->
	    ok;
	Error ->
	    test_server:format("Cipher suite errors: ~p~n", [Error]),
	    test_server:fail(cipher_suite_failed_see_test_case_log) 
    end.

erlang_cipher_suite(Suite) when is_list(Suite)->
    ssl:suite_definition(ssl_cipher:openssl_suite(Suite));
erlang_cipher_suite(Suite) ->
    Suite.

cipher(CipherSuite, Version, Config, ClientOpts, ServerOpts) ->   
    %% process_flag(trap_exit, true),
    test_server:format("Testing CipherSuite ~p~n", [CipherSuite]),
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

%%--------------------------------------------------------------------
default_reject_anonymous(doc)->
    ["Test that by default anonymous cipher suites are rejected "];
default_reject_anonymous(suite) ->
    [];
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

    ssl_test_lib:check_result(Server, {error, "insufficient security"},
			      Client, {error, "insufficient security"}).

%%--------------------------------------------------------------------
reuse_session(doc) -> 
    ["Test reuse of sessions (short handshake)"];

reuse_session(suite) -> 
    [];

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
    test_server:sleep(?SLEEP),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode},
				   {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]),
    receive
	{Client1, SessionInfo} ->
	    ok;
	{Client1, Other} ->
	    test_server:format("Expected: ~p,  Unexpected: ~p~n", 
			       [SessionInfo, Other]),
	    test_server:fail(session_not_reused)
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
	    test_server:fail(
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
    test_server:sleep(?SLEEP),

    Client4 = 
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port1}, {host, Hostname},
				   {mfa, {ssl_test_lib, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]),
    
    receive
	{Client4, SessionInfo1} ->
	    test_server:fail(
	      session_reused_when_session_reuse_disabled_by_server);
	{Client4, _Other} ->
	    test_server:format("OTHER: ~p ~n", [_Other]),
	    ok
    end,

    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Client2),
    ssl_test_lib:close(Client3),
    ssl_test_lib:close(Client4).

%%--------------------------------------------------------------------
reuse_session_expired(doc) -> 
    ["Test sessions is not reused when it has expired"];

reuse_session_expired(suite) -> 
    [];

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
    test_server:sleep(?SLEEP),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
		      {mfa, {ssl_test_lib, session_info_result, []}},
		      {from, self()},  {options, ClientOpts}]),    
    receive
	{Client1, SessionInfo} ->
	    ok;
	{Client1, Other} ->
	    test_server:format("Expected: ~p,  Unexpected: ~p~n", 
			       [SessionInfo, Other]),
	    test_server:fail(session_not_reused)
    end,
    
    Server ! listen,

    %% Make sure session is unregistered due to expiration
    test_server:sleep((?EXPIRE+1)),
    [{session_id, Id} |_] = SessionInfo,

    make_sure_expired(Hostname, Port, Id),
    
    Client2 =
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, session_info_result, []}},
				   {from, self()}, {options, ClientOpts}]),   
    receive
	{Client2, SessionInfo} ->
	    test_server:fail(session_reused_when_session_expired);
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
	    test_server:sleep(?SLEEP),
	    make_sure_expired(Host, Port, Id)
    end.     

%%--------------------------------------------------------------------
server_does_not_want_to_reuse_session(doc) -> 
    ["Test reuse of sessions (short handshake)"];

server_does_not_want_to_reuse_session(suite) -> 
    [];

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
    test_server:sleep(?SLEEP),
    ssl_test_lib:close(Client0),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
		      {mfa, {ssl_test_lib, session_info_result, []}},
		      {from, self()},  {options, ClientOpts}]),    
    receive
	{Client1, SessionInfo} ->
	    test_server:fail(session_reused_when_server_does_not_want_to);
	{Client1, _Other} ->
	   ok
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------

server_verify_peer_passive(doc) -> 
    ["Test server option verify_peer"];

server_verify_peer_passive(suite) -> 
    [];

server_verify_peer_passive(Config) when is_list(Config) ->
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_recv_result, []}},
			   {options, [{active, false}, {verify, verify_peer} 
				      | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, send_recv_result, []}},
			   {options, [{active, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

server_verify_peer_active(doc) -> 
    ["Test server option verify_peer"];

server_verify_peer_active(suite) -> 
    [];

server_verify_peer_active(Config) when is_list(Config) ->
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active, []}},
			   {options, [{active, true}, {verify, verify_peer} 
				      | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active, []}},
			   {options, [{active, true} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
server_verify_peer_active_once(doc) -> 
    ["Test server option verify_peer"];

server_verify_peer_active_once(suite) -> 
    [];

server_verify_peer_active_once(Config) when is_list(Config) ->
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active_once, []}},
			   {options, [{active, once}, {verify, verify_peer} 
				      | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active_once, []}},
			   {options, [{active, once} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

server_verify_none_passive(doc) -> 
    ["Test server option verify_none"];

server_verify_none_passive(suite) -> 
    [];

server_verify_none_passive(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_recv_result, []}},
			   {options, [{active, false}, {verify, verify_none} 
				      | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, send_recv_result, []}},
			   {options, [{active, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

server_verify_none_active(doc) -> 
    ["Test server option verify_none"];

server_verify_none_active(suite) -> 
    [];

server_verify_none_active(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active, []}},
			   {options, [{active, true}, {verify, verify_none} | 
				      ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active, []}},
			   {options, [{active, true} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
server_verify_none_active_once(doc) -> 
    ["Test server option verify_none"];

server_verify_none_active_once(suite) -> 
    [];

server_verify_none_active_once(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active_once, []}},
			   {options, [{active, once}, {verify, verify_none} 
				      | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active_once, []}},
			   {options, [{active, once} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

server_verify_client_once_passive(doc) -> 
    ["Test server option verify_client_once"];

server_verify_client_once_passive(suite) -> 
    [];

server_verify_client_once_passive(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_verification_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_recv_result, []}},
					{options, [{active, false}, {verify, verify_peer},
						   {verify_client_once, true}
						   | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, send_recv_result, []}},
					{options, [{active, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client0, ok),
    Server ! {listen, {mfa, {ssl_test_lib, no_result, []}}},
    ssl_test_lib:close(Client0),
    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, result_ok, []}},
					{options, [{active, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client1, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------

server_verify_client_once_active(doc) -> 
    ["Test server option verify_client_once"];

server_verify_client_once_active(suite) -> 
    [];

server_verify_client_once_active(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_verification_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_recv_result_active, []}},
					{options, [{active, true}, {verify, verify_peer},
						   {verify_client_once, true}
						   | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					 {host, Hostname},
					 {from, self()}, 
					 {mfa, {?MODULE, send_recv_result_active, []}},
					 {options, [{active, true} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client0, ok),
    Server ! {listen, {mfa, {ssl_test_lib, no_result, []}}},
    ssl_test_lib:close(Client0),
    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, result_ok, []}},
					{options, [{active, true} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client1, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client1).
    
%%--------------------------------------------------------------------

server_verify_client_once_active_once(doc) -> 
    ["Test server option verify_client_once"];

server_verify_client_once_active_once(suite) -> 
    [];

server_verify_client_once_active_once(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_verification_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_recv_result_active_once, []}},
					{options, [{active, once}, {verify, verify_peer},
						   {verify_client_once, true}
						   | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, send_recv_result_active_once, []}},
					{options, [{active, once} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client0, ok),
    Server ! {listen, {mfa, {ssl_test_lib, no_result, []}}},
    ssl_test_lib:close(Client0),
    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					 {host, Hostname},
					 {from, self()},
					 {mfa, {?MODULE, result_ok, []}},
					 {options, [{active, once} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client1, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------

server_verify_no_cacerts(doc) -> 
    ["Test server must have cacerts if it wants to verify client"];

server_verify_no_cacerts(suite) -> 
    [];
server_verify_no_cacerts(Config) when is_list(Config) ->
    ServerOpts =  ?config(server_opts, Config),
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
					      {from, self()}, 
					      {options, [{verify, verify_peer} 
							 | ServerOpts]}]),
        
    ssl_test_lib:check_result(Server, {error, {eoptions, {cacertfile, ""}}}).

%%--------------------------------------------------------------------

server_require_peer_cert_ok(doc) ->
    ["Test server option fail_if_no_peer_cert when peer sends cert"];

server_require_peer_cert_ok(suite) ->
    [];

server_require_peer_cert_ok(Config) when is_list(Config) ->
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}
		  | ?config(server_verification_opts, Config)],
    ClientOpts = ?config(client_verification_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, send_recv_result, []}},
			   {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {?MODULE, send_recv_result, []}},
			   {options, [{active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

server_require_peer_cert_fail(doc) ->
    ["Test server option fail_if_no_peer_cert when peer doesn't send cert"];

server_require_peer_cert_fail(suite) ->
    [];

server_require_peer_cert_fail(Config) when is_list(Config) ->
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}
		  | ?config(server_verification_opts, Config)],
    BadClientOpts = ?config(client_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
			   {options, [{active, false} | ServerOpts]}]),

    Port  = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
					      {options, [{active, false} | BadClientOpts]}]),
    
    ssl_test_lib:check_result(Server, {error, esslaccept},
			      Client, {error, esslconnect}).

%%--------------------------------------------------------------------

client_verify_none_passive(doc) -> 
    ["Test client option verify_none"];

client_verify_none_passive(suite) -> 
    [];

client_verify_none_passive(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_recv_result, []}},
					{options, [{active, false} 
						   | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, send_recv_result, []}},
					{options, [{active, false}, 
						   {verify, verify_none}
						   | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

client_verify_none_active(doc) -> 
    ["Test client option verify_none"];

client_verify_none_active(suite) -> 
    [];

client_verify_none_active(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_opts, Config),  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, 
					       send_recv_result_active, []}},
					{options, [{active, true} 
						   | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       send_recv_result_active, []}},
					{options, [{active, true}, 
						   {verify, verify_none}
						   | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
client_verify_none_active_once(doc) -> 
    ["Test client option verify_none"];

client_verify_none_active_once(suite) -> 
    [];

client_verify_none_active_once(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_opts, Config),  

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active_once, []}},
			   {options, [{active, once} | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       send_recv_result_active_once, 
					       []}},
					{options, [{active, once}, 
						   {verify, verify_none}
						   | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
client_renegotiate(doc) -> 
    ["Test ssl:renegotiate/1 on client."];

client_renegotiate(suite) -> 
    [];

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
server_renegotiate(doc) -> 
    ["Test ssl:renegotiate/1 on server."];

server_renegotiate(suite) -> 
    [];

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
client_renegotiate_reused_session(doc) -> 
    ["Test ssl:renegotiate/1 on client when the ssl session will be reused."];

client_renegotiate_reused_session(suite) -> 
    [];

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
server_renegotiate_reused_session(doc) -> 
    ["Test ssl:renegotiate/1 on server when the ssl session will be reused."];

server_renegotiate_reused_session(suite) -> 
    [];

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
client_no_wrap_sequence_number(doc) -> 
    ["Test that erlang client will renegotiate session when",  
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at" 
     " to lower treashold substantially."];

client_no_wrap_sequence_number(suite) -> 
    [];

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
 
    Version = ssl_record:highest_protocol_version(ssl_record:supported_protocol_versions()),

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

 %% First two clauses handles 1/n-1 splitting countermeasure Rizzo/Duong-Beast
treashold(N, {3,0}) ->
    (N div 2) + 1;
treashold(N, {3,1}) ->
    (N div 2) + 1;
treashold(N, _) ->
    N + 1.
    
%%--------------------------------------------------------------------
server_no_wrap_sequence_number(doc) -> 
    ["Test that erlang server will renegotiate session when",  
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at" 
     " to lower treashold substantially."];

server_no_wrap_sequence_number(suite) -> 
    [];

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
extended_key_usage_verify_peer(doc) ->
    ["Test cert that has a critical extended_key_usage extension in verify_peer mode"];

extended_key_usage_verify_peer(suite) ->
    [];

extended_key_usage_verify_peer(Config) when is_list(Config) ->
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),
    PrivDir = ?config(priv_dir, Config),
   
    KeyFile = filename:join(PrivDir, "otpCA/private/key.pem"), 
    [KeyEntry] = ssl_test_lib:pem_to_der(KeyFile),
    Key = ssl_test_lib:public_key(public_key:pem_entry_decode(KeyEntry)),

    ServerCertFile = proplists:get_value(certfile, ServerOpts),
    NewServerCertFile = filename:join(PrivDir, "server/new_cert.pem"),
    [{'Certificate', ServerDerCert, _}] = ssl_test_lib:pem_to_der(ServerCertFile),
    ServerOTPCert = public_key:pkix_decode_cert(ServerDerCert, otp),
    ServerExtKeyUsageExt = {'Extension', ?'id-ce-extKeyUsage', true, [?'id-kp-serverAuth']},
    ServerOTPTbsCert = ServerOTPCert#'OTPCertificate'.tbsCertificate,
    ServerExtensions =  ServerOTPTbsCert#'OTPTBSCertificate'.extensions,
    NewServerOTPTbsCert = ServerOTPTbsCert#'OTPTBSCertificate'{extensions =
							       [ServerExtKeyUsageExt |
								ServerExtensions]},
    NewServerDerCert = public_key:pkix_sign(NewServerOTPTbsCert, Key),
    ssl_test_lib:der_to_pem(NewServerCertFile, [{'Certificate', NewServerDerCert, not_encrypted}]),
    NewServerOpts = [{certfile, NewServerCertFile} | proplists:delete(certfile, ServerOpts)],

    ClientCertFile = proplists:get_value(certfile, ClientOpts),
    NewClientCertFile = filename:join(PrivDir, "client/new_cert.pem"),
    [{'Certificate', ClientDerCert, _}] = ssl_test_lib:pem_to_der(ClientCertFile),
    ClientOTPCert = public_key:pkix_decode_cert(ClientDerCert, otp),
    ClientExtKeyUsageExt = {'Extension', ?'id-ce-extKeyUsage', true, [?'id-kp-clientAuth']},
    ClientOTPTbsCert = ClientOTPCert#'OTPCertificate'.tbsCertificate,
    ClientExtensions =  ClientOTPTbsCert#'OTPTBSCertificate'.extensions,
    NewClientOTPTbsCert = ClientOTPTbsCert#'OTPTBSCertificate'{extensions =
 							       [ClientExtKeyUsageExt |
 								ClientExtensions]},
    NewClientDerCert = public_key:pkix_sign(NewClientOTPTbsCert, Key),
    ssl_test_lib:der_to_pem(NewClientCertFile, [{'Certificate', NewClientDerCert, not_encrypted}]),
    NewClientOpts = [{certfile, NewClientCertFile} | proplists:delete(certfile, ClientOpts)],

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, send_recv_result_active, []}},
			   {options, [{verify, verify_peer} | NewServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {?MODULE, send_recv_result_active, []}},
					{options, [{verify, verify_peer} | NewClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
extended_key_usage_verify_none(doc) ->
    ["Test cert that has a critical extended_key_usage extension in verify_none mode"];

extended_key_usage_verify_none(suite) ->
    [];

extended_key_usage_verify_none(Config) when is_list(Config) ->
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),
    PrivDir = ?config(priv_dir, Config),

    KeyFile = filename:join(PrivDir, "otpCA/private/key.pem"),
    [KeyEntry] = ssl_test_lib:pem_to_der(KeyFile),
    Key = ssl_test_lib:public_key(public_key:pem_entry_decode(KeyEntry)),

    ServerCertFile = proplists:get_value(certfile, ServerOpts),
    NewServerCertFile = filename:join(PrivDir, "server/new_cert.pem"),
    [{'Certificate', ServerDerCert, _}] = ssl_test_lib:pem_to_der(ServerCertFile),
    ServerOTPCert = public_key:pkix_decode_cert(ServerDerCert, otp),
    ServerExtKeyUsageExt = {'Extension', ?'id-ce-extKeyUsage', true, [?'id-kp-serverAuth']},
    ServerOTPTbsCert = ServerOTPCert#'OTPCertificate'.tbsCertificate,
    ServerExtensions =  ServerOTPTbsCert#'OTPTBSCertificate'.extensions,
    NewServerOTPTbsCert = ServerOTPTbsCert#'OTPTBSCertificate'{extensions =
							       [ServerExtKeyUsageExt |
								ServerExtensions]},
    NewServerDerCert = public_key:pkix_sign(NewServerOTPTbsCert, Key),
    ssl_test_lib:der_to_pem(NewServerCertFile, [{'Certificate', NewServerDerCert, not_encrypted}]),
    NewServerOpts = [{certfile, NewServerCertFile} | proplists:delete(certfile, ServerOpts)],

    ClientCertFile = proplists:get_value(certfile, ClientOpts),
    NewClientCertFile = filename:join(PrivDir, "client/new_cert.pem"),
    [{'Certificate', ClientDerCert, _}] = ssl_test_lib:pem_to_der(ClientCertFile),
    ClientOTPCert = public_key:pkix_decode_cert(ClientDerCert, otp),
    ClientExtKeyUsageExt = {'Extension', ?'id-ce-extKeyUsage', true, [?'id-kp-clientAuth']},
    ClientOTPTbsCert = ClientOTPCert#'OTPCertificate'.tbsCertificate,
    ClientExtensions =  ClientOTPTbsCert#'OTPTBSCertificate'.extensions,
    NewClientOTPTbsCert = ClientOTPTbsCert#'OTPTBSCertificate'{extensions =
								   [ClientExtKeyUsageExt |
								    ClientExtensions]},
    NewClientDerCert = public_key:pkix_sign(NewClientOTPTbsCert, Key),
    ssl_test_lib:der_to_pem(NewClientCertFile, [{'Certificate', NewClientDerCert, not_encrypted}]),
    NewClientOpts = [{certfile, NewClientCertFile} | proplists:delete(certfile, ClientOpts)],

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, send_recv_result_active, []}},
			   {options, [{verify, verify_none} | NewServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {?MODULE, send_recv_result_active, []}},
					{options, [{verify, verify_none} | NewClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
no_authority_key_identifier(doc) -> 
    ["Test cert that does not have authorityKeyIdentifier extension"
     " but are present in trusted certs db."];

no_authority_key_identifier(suite) -> 
    [];
no_authority_key_identifier(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    PrivDir = ?config(priv_dir, Config),
   
    KeyFile = filename:join(PrivDir, "otpCA/private/key.pem"),
    [KeyEntry] = ssl_test_lib:pem_to_der(KeyFile),
    Key = ssl_test_lib:public_key(public_key:pem_entry_decode(KeyEntry)),

    CertFile = proplists:get_value(certfile, ServerOpts),
    NewCertFile = filename:join(PrivDir, "server/new_cert.pem"),
    [{'Certificate', DerCert, _}] = ssl_test_lib:pem_to_der(CertFile),
    OTPCert = public_key:pkix_decode_cert(DerCert, otp),
    OTPTbsCert = OTPCert#'OTPCertificate'.tbsCertificate,
    Extensions =  OTPTbsCert#'OTPTBSCertificate'.extensions,
    NewExtensions =  delete_authority_key_extension(Extensions, []),
    NewOTPTbsCert =  OTPTbsCert#'OTPTBSCertificate'{extensions = NewExtensions},

    test_server:format("Extensions ~p~n, NewExtensions: ~p~n", [Extensions, NewExtensions]),

    NewDerCert = public_key:pkix_sign(NewOTPTbsCert, Key), 
    ssl_test_lib:der_to_pem(NewCertFile, [{'Certificate', NewDerCert, not_encrypted}]),
    NewServerOpts = [{certfile, NewCertFile} | proplists:delete(certfile, ServerOpts)],

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active, []}},
			   {options, NewServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, send_recv_result_active, []}},
			   {options, [{verify, verify_peer} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

delete_authority_key_extension([], Acc) ->
    lists:reverse(Acc);
delete_authority_key_extension([#'Extension'{extnID = ?'id-ce-authorityKeyIdentifier'} | Rest], 
			       Acc) ->
    delete_authority_key_extension(Rest, Acc);
delete_authority_key_extension([Head | Rest], Acc) ->
    delete_authority_key_extension(Rest, [Head | Acc]).

%%--------------------------------------------------------------------

invalid_signature_server(doc) -> 
    ["Test server with invalid signature"];

invalid_signature_server(suite) -> 
    [];

invalid_signature_server(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),
    PrivDir = ?config(priv_dir, Config),
   
    KeyFile = filename:join(PrivDir, "server/key.pem"),
    [KeyEntry] = ssl_test_lib:pem_to_der(KeyFile),
    Key = ssl_test_lib:public_key(public_key:pem_entry_decode(KeyEntry)),

    ServerCertFile = proplists:get_value(certfile, ServerOpts),
    NewServerCertFile = filename:join(PrivDir, "server/invalid_cert.pem"),
    [{'Certificate', ServerDerCert, _}] = ssl_test_lib:pem_to_der(ServerCertFile),
    ServerOTPCert = public_key:pkix_decode_cert(ServerDerCert, otp),
    ServerOTPTbsCert = ServerOTPCert#'OTPCertificate'.tbsCertificate,
    NewServerDerCert = public_key:pkix_sign(ServerOTPTbsCert, Key), 
    ssl_test_lib:der_to_pem(NewServerCertFile, [{'Certificate', NewServerDerCert, not_encrypted}]),
    NewServerOpts = [{certfile, NewServerCertFile} | proplists:delete(certfile, ServerOpts)],
    
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
					      {from, self()}, 
					      {options, NewServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
					      {host, Hostname},
					      {from, self()}, 
					      {options, [{verify, verify_peer} | ClientOpts]}]),
    
    tcp_delivery_workaround(Server, {error, "bad certificate"},
			    Client, {error,"bad certificate"}).
    
%%--------------------------------------------------------------------

invalid_signature_client(doc) -> 
    ["Test server with invalid signature"];

invalid_signature_client(suite) -> 
    [];

invalid_signature_client(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),
    PrivDir = ?config(priv_dir, Config),
   
    KeyFile = filename:join(PrivDir, "client/key.pem"),
    [KeyEntry] = ssl_test_lib:pem_to_der(KeyFile),
    Key = ssl_test_lib:public_key(public_key:pem_entry_decode(KeyEntry)),

    ClientCertFile = proplists:get_value(certfile, ClientOpts),
    NewClientCertFile = filename:join(PrivDir, "client/invalid_cert.pem"),
    [{'Certificate', ClientDerCert, _}] = ssl_test_lib:pem_to_der(ClientCertFile),
    ClientOTPCert = public_key:pkix_decode_cert(ClientDerCert, otp),
    ClientOTPTbsCert = ClientOTPCert#'OTPCertificate'.tbsCertificate,
    NewClientDerCert = public_key:pkix_sign(ClientOTPTbsCert, Key), 
    ssl_test_lib:der_to_pem(NewClientCertFile, [{'Certificate', NewClientDerCert, not_encrypted}]),
    NewClientOpts = [{certfile, NewClientCertFile} | proplists:delete(certfile, ClientOpts)],

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{options, [{verify, verify_peer} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
					      {host, Hostname},
					      {from, self()}, 
					      {options, NewClientOpts}]),

    tcp_delivery_workaround(Server, {error, "bad certificate"},
			    Client, {error,"bad certificate"}).

tcp_delivery_workaround(Server, ServerMsg, Client, ClientMsg) ->
    receive 
	{Server, ServerMsg} ->
	    client_msg(Client, ClientMsg);
	{Client, ClientMsg} ->
	    server_msg(Server, ServerMsg);
       	{Client, {error,closed}} ->
	    server_msg(Server, ServerMsg);
	{Server, {error,closed}} ->
	    client_msg(Client, ClientMsg);
	{Client, {error, esslconnect}} ->
	    server_msg(Server, ServerMsg);
	{Server, {error, esslaccept}} ->
	    client_msg(Client, ClientMsg)
    end.

client_msg(Client, ClientMsg) ->
    receive
	{Client, ClientMsg} ->
	    ok;
	{Client, {error,closed}} ->
	    test_server:format("client got close"),
	    ok;
	{Client, {error, esslconnect}} ->
	    test_server:format("client got econnaborted"),
	    ok;
	Unexpected ->
	    test_server:fail(Unexpected)
    end.

server_msg(Server, ServerMsg) ->
    receive
	{Server, ServerMsg} ->
	    ok;
	{Server, {error,closed}} ->
	    test_server:format("server got close"),
	    ok;
	{Server, {error, esslaccept}} ->
	    test_server:format("server got econnaborted"),
	    ok;
	Unexpected ->
	    test_server:fail(Unexpected)
    end.

%%--------------------------------------------------------------------
cert_expired(doc) -> 
    ["Test server with invalid signature"];

cert_expired(suite) -> 
    [];

cert_expired(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),
    PrivDir = ?config(priv_dir, Config),
   
    KeyFile = filename:join(PrivDir, "otpCA/private/key.pem"),
    [KeyEntry] = ssl_test_lib:pem_to_der(KeyFile),
    Key = ssl_test_lib:public_key(public_key:pem_entry_decode(KeyEntry)),

    ServerCertFile = proplists:get_value(certfile, ServerOpts),
    NewServerCertFile = filename:join(PrivDir, "server/expired_cert.pem"),
    [{'Certificate', DerCert, _}] = ssl_test_lib:pem_to_der(ServerCertFile),
    OTPCert = public_key:pkix_decode_cert(DerCert, otp),
    OTPTbsCert = OTPCert#'OTPCertificate'.tbsCertificate,

    {Year, Month, Day} = date(),
    {Hours, Min, Sec} = time(),
    NotBeforeStr = lists:flatten(io_lib:format("~p~s~s~s~s~sZ",[Year-2, 
								two_digits_str(Month), 
								two_digits_str(Day), 
								two_digits_str(Hours), 
								two_digits_str(Min), 
								two_digits_str(Sec)])),
    NotAfterStr = lists:flatten(io_lib:format("~p~s~s~s~s~sZ",[Year-1, 
							       two_digits_str(Month), 
							       two_digits_str(Day), 
							       two_digits_str(Hours), 
							       two_digits_str(Min), 
							       two_digits_str(Sec)])),	
    NewValidity = {'Validity', {generalTime, NotBeforeStr}, {generalTime, NotAfterStr}}, 

    test_server:format("Validity: ~p ~n NewValidity: ~p ~n", 
		       [OTPTbsCert#'OTPTBSCertificate'.validity, NewValidity]),

    NewOTPTbsCert =  OTPTbsCert#'OTPTBSCertificate'{validity = NewValidity},
    NewServerDerCert = public_key:pkix_sign(NewOTPTbsCert, Key), 
    ssl_test_lib:der_to_pem(NewServerCertFile, [{'Certificate', NewServerDerCert, not_encrypted}]),
    NewServerOpts = [{certfile, NewServerCertFile} | proplists:delete(certfile, ServerOpts)],
    
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
					      {from, self()}, 
					      {options, NewServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
					      {host, Hostname},
					      {from, self()}, 
					      {options, [{verify, verify_peer} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, {error, "certificate expired"}, 
			      Client, {error, "certificate expired"}).

two_digits_str(N) when N < 10 ->
    lists:flatten(io_lib:format("0~p", [N]));
two_digits_str(N) ->
    lists:flatten(io_lib:format("~p", [N])).

%%--------------------------------------------------------------------

client_with_cert_cipher_suites_handshake(doc) ->
    ["Test that client with a certificate without keyEncipherment usage "
    " extension can connect to a server with restricted cipher suites "];

client_with_cert_cipher_suites_handshake(suite) ->
    [];

client_with_cert_cipher_suites_handshake(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_verification_opts_digital_signature_only, Config),
    ServerOpts =  ?config(server_verification_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE,
					       send_recv_result_active, []}},
					{options, [{active, true},
						   {ciphers, ssl_test_lib:rsa_non_signed_suites()}
						   | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       send_recv_result_active, []}},
					{options, [{active, true}
						   | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
verify_fun_always_run_client(doc) ->
    ["Verify that user verify_fun is always run (for valid and valid_peer not only unknown_extension)"];
verify_fun_always_run_client(suite) ->
    [];
verify_fun_always_run_client(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_verification_opts, Config),
    ServerOpts =  ?config(server_verification_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options, ServerOpts}]),
    Port  = ssl_test_lib:inet_port(Server),

    %% If user verify fun is called correctly we fail the connection.
    %% otherwise we can not tell this case apart form where we miss
    %% to call users verify fun
    FunAndState =  {fun(_,{extension, _}, UserState) ->
			    {unknown, UserState};
		       (_, valid, [ChainLen]) ->
			    {valid, [ChainLen + 1]};
		       (_, valid_peer, [2]) ->
			    {fail, "verify_fun_was_always_run"};
		       (_, valid_peer, UserState) ->
			    {valid, UserState}
		    end, [0]},

    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options,
					       [{verify, verify_peer},
						{verify_fun, FunAndState}
						| ClientOpts]}]),
    %% Server error may be esslaccept or closed depending on timing
    %% this is not a bug it is a circumstance of how tcp works!
    receive
	{Server, ServerError} ->
	    test_server:format("Server Error ~p~n", [ServerError])
    end,

    ssl_test_lib:check_result(Client, {error, esslconnect}).

%%--------------------------------------------------------------------
verify_fun_always_run_server(doc) ->
    ["Verify that user verify_fun is always run (for valid and valid_peer not only unknown_extension)"];
verify_fun_always_run_server(suite) ->
    [];
verify_fun_always_run_server(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_verification_opts, Config),
    ServerOpts =  ?config(server_verification_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    %% If user verify fun is called correctly we fail the connection.
    %% otherwise we can not tell this case apart form where we miss
    %% to call users verify fun
    FunAndState =  {fun(_,{extension, _}, UserState) ->
			    {unknown, UserState};
		       (_, valid, [ChainLen]) ->
			    {valid, [ChainLen + 1]};
		       (_, valid_peer, [2]) ->
			    {fail, "verify_fun_was_always_run"};
		       (_, valid_peer, UserState) ->
			    {valid, UserState}
		    end, [0]},

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options,
					       [{verify, verify_peer},
						{verify_fun, FunAndState} |
						ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options,
					       [{verify, verify_peer}
						| ClientOpts]}]),

    %% Client error may be esslconnect or closed depending on timing
    %% this is not a bug it is a circumstance of how tcp works!
    receive
	{Client, ClientError} ->
	    test_server:format("Client Error ~p~n", [ClientError])
    end,

    ssl_test_lib:check_result(Server, {error, esslaccept}).

%%--------------------------------------------------------------------
unknown_server_ca_fail(doc) ->
    ["Test that the client fails if the ca is unknown in verify_peer mode"];
unknown_server_ca_fail(suite) ->
    [];
unknown_server_ca_fail(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options, ServerOpts}]),
    Port  = ssl_test_lib:inet_port(Server),

    FunAndState =  {fun(_,{bad_cert, unknown_ca} = Reason, _) ->
			    {fail, Reason};
		       (_,{extension, _}, UserState) ->
			    {unknown, UserState};
		       (_, valid, UserState) ->
			    {valid, [test_to_update_user_state | UserState]};
		       (_, valid_peer, UserState) ->
			    {valid, UserState}
		    end, []},

    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options,
					       [{verify, verify_peer},
						{verify_fun, FunAndState}
						| ClientOpts]}]),

    ssl_test_lib:check_result(Server, {error,"unknown ca"},
			      Client, {error, "unknown ca"}).

%%--------------------------------------------------------------------
unknown_server_ca_accept_verify_none(doc) ->
    ["Test that the client succeds if the ca is unknown in verify_none mode"];
unknown_server_ca_accept_verify_none(suite) ->
    [];
unknown_server_ca_accept_verify_none(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE,
					       send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       send_recv_result_active, []}},
					{options,
					 [{verify, verify_none}| ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
unknown_server_ca_accept_verify_peer(doc) ->
    ["Test that the client succeds if the ca is unknown in verify_peer mode"
     " with a verify_fun that accepts the unknown ca error"];
unknown_server_ca_accept_verify_peer(suite) ->
    [];
unknown_server_ca_accept_verify_peer(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE,
					       send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port  = ssl_test_lib:inet_port(Server),

    FunAndState =  {fun(_,{bad_cert, unknown_ca}, UserState) ->
			    {valid, UserState};
		       (_,{bad_cert, _} = Reason, _) ->
			    {fail, Reason};
		       (_,{extension, _}, UserState) ->
			    {unknown, UserState};
		       (_, valid, UserState) ->
			    {valid, UserState};
		       (_, valid_peer, UserState) ->
			    {valid, UserState}
		    end, []},

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       send_recv_result_active, []}},
					{options,
					 [{verify, verify_peer},
					  {verify_fun, FunAndState}| ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
unknown_server_ca_accept_backwardscompatibility(doc) ->
    ["Test that old style verify_funs will work"];
unknown_server_ca_accept_backwardscompatibility(suite) ->
    [];
unknown_server_ca_accept_backwardscompatibility(Config) when is_list(Config) ->
    ClientOpts =  ?config(client_opts, Config),
    ServerOpts =  ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE,
					       send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port  = ssl_test_lib:inet_port(Server),

    AcceptBadCa = fun({bad_cert,unknown_ca}, Acc) ->  Acc;
		     (Other, Acc) -> [Other | Acc]
		  end,
    VerifyFun =
	fun(ErrorList) ->
		case lists:foldl(AcceptBadCa, [], ErrorList) of
		    [] ->    true;
		    [_|_] -> false
		end
	end,

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       send_recv_result_active, []}},
					{options,
					 [{verify, verify_peer},
					  {verify_fun, VerifyFun}| ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
der_input(doc) ->
    ["Test to input certs and key as der"];

der_input(suite) ->
    [];

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
					{mfa, {?MODULE, send_recv_result, []}},
					{options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, send_recv_result, []}},
					{options, [{active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

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
%% different_ca_peer_sign(doc) ->
%%     ["Check that a CA can have a different signature algorithm than the peer cert."];

%% different_ca_peer_sign(suite) ->
%%     [];

%% different_ca_peer_sign(Config) when is_list(Config) ->
%%     ClientOpts =  ?config(client_mix_opts, Config),
%%     ServerOpts =  ?config(server_mix_verify_opts, Config),

%%     {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
%%     Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
%% 					{from, self()},
%% 			   {mfa, {?MODULE, send_recv_result_active_once, []}},
%% 			   {options, [{active, once},
%% 				      {verify, verify_peer} | ServerOpts]}]),
%%     Port  = ssl_test_lib:inet_port(Server),

%%     Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
%% 					{host, Hostname},
%% 					{from, self()},
%% 					{mfa, {?MODULE,
%% 					       send_recv_result_active_once,
%% 					       []}},
%% 					{options, [{active, once},
%% 						   {verify, verify_peer}
%% 						   | ClientOpts]}]),

%%     ssl_test_lib:check_result(Server, ok, Client, ok),
%%     ssl_test_lib:close(Server),
%%     ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
no_reuses_session_server_restart_new_cert(doc) ->
    ["Check that a session is not reused if the server is restarted with a new cert."];

no_reuses_session_server_restart_new_cert(suite) ->
    [];

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
    test_server:sleep(?SLEEP),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client0),

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
	    test_server:fail(session_reused_when_server_has_new_cert);
	{Client1, _Other} ->
	   ok
    end,
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
no_reuses_session_server_restart_new_cert_file(doc) ->
    ["Check that a session is not reused if a server is restarted with a new "
     "cert contained in a file with the same name as the old cert."];

no_reuses_session_server_restart_new_cert_file(suite) ->
    [];

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
    test_server:sleep(?SLEEP* 2),
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
	    test_server:fail(session_reused_when_server_has_new_cert);
	{Client1, _Other} ->
	   ok
    end,
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
reuseaddr(doc) ->
    [""];

reuseaddr(suite) ->
    [];

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
    test_server:sleep(?SLEEP),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),

    Server1 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, Port},
				   {from, self()},
				   {mfa, {?MODULE, send_recv_result, []}},
				   {options,  [{active, false} | ServerOpts]}]),
    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {?MODULE, send_recv_result, []}},
				   {options, [{active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server1, ok, Client1, ok),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------

hibernate(doc) -> 
    ["Check that an SSL connection that is started with option "
     "{hibernate_after, 1000} indeed hibernates after 1000ms of "
     "inactivity"];

hibernate(suite) ->
    [];

hibernate(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {Client, #sslsocket{pid=Pid}} = ssl_test_lib:start_client([return_socket,
                    {node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, send_recv_result_active, []}},
					{options, [{hibernate_after, 1000}|ClientOpts]}]),
    {current_function, _} =
        process_info(Pid, current_function),

    timer:sleep(1100),

    {current_function, {erlang, hibernate, 3}} =
	process_info(Pid, current_function),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
ssl_accept_timeout(doc) ->
    ["Test ssl:ssl_accept timeout"];
ssl_accept_timeout(suite) ->
    [];
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
		    [] = supervisor:which_children(ssl_connection_sup)
	    end
    end.

%%--------------------------------------------------------------------
ssl_recv_timeout(doc) ->
    ["Test ssl:ssl_accept timeout"];
ssl_recv_timeout(suite) ->
    [];
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

connect_twice(doc) ->
    [""];
connect_twice(suite) ->
    [];
connect_twice(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, send_recv_result, []}},
				   {options,  [{keepalive, true},{active, false}
					       | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {?MODULE, send_recv_result, []}},
				   {options, [{keepalive, true},{active, false}
					      | ClientOpts]}]),
    Server ! listen,

    {Client1, #sslsocket{}} =
	ssl_test_lib:start_client([return_socket,
				   {node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {?MODULE, send_recv_result, []}},
				   {options, [{keepalive, true},{active, false}
					      | ClientOpts]}]),

    test_server:format("Testcase ~p, Client ~p  Server ~p ~n",
			 [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:check_result(Server, ok, Client1, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
renegotiate_dos_mitigate_active(doc) ->
    ["Mitigate DOS computational attack by not allowing client to renegotiate many times in a row",
    "immediately after each other"];

renegotiate_dos_mitigate_active(suite) ->
    [];

renegotiate_dos_mitigate_active(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, send_recv_result_active, []}},
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
renegotiate_dos_mitigate_passive(doc) ->
    ["Mitigate DOS computational attack by not allowing client to renegotiate many times in a row",
    "immediately after each other"];

renegotiate_dos_mitigate_passive(suite) ->
    [];

renegotiate_dos_mitigate_passive(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, send_recv_result, []}},
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
tcp_error_propagation_in_active_mode(doc) ->
    ["Test that process recives {ssl_error, Socket, closed} when tcp error ocurres"];
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
    Socket = element(10, State),

    %% Fake tcp error
    Pid ! {tcp_error, Socket, etimedout},

    ssl_test_lib:check_result(Client, {ssl_closed, SslSocket}).


%%--------------------------------------------------------------------

recv_error_handling(doc) ->
    ["Special case of call error handling"];
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

rizzo(doc) -> ["Test that there is a 1/n-1-split for non RC4 in 'TLS < 1.1' as it is
    vunrable to Rizzo/Dungon attack"];

rizzo(Config) when is_list(Config) ->
    Ciphers  = [X || X ={_,Y,_} <- ssl:cipher_suites(), Y  =/= rc4_128],
    Prop = ?config(tc_group_properties, Config),
    Version = proplists:get_value(name, Prop),
    run_send_recv_rizzo(Ciphers, Config, Version,
			 {?MODULE, send_recv_result_active_rizzo, []}).
%%--------------------------------------------------------------------
no_rizzo_rc4(doc) -> 
    ["Test that there is no 1/n-1-split for RC4 as it is not vunrable to Rizzo/Dungon attack"];

no_rizzo_rc4(Config) when is_list(Config) ->
    Ciphers = [X || X ={_,Y,_} <- ssl:cipher_suites(),Y == rc4_128],
    Prop = ?config(tc_group_properties, Config),
    Version = proplists:get_value(name, Prop),
    run_send_recv_rizzo(Ciphers, Config, Version,
			{?MODULE, send_recv_result_active_no_rizzo, []}).

%%--------------------------------------------------------------------
run_send_recv_rizzo(Ciphers, Config, Version, Mfa) ->
    Result =  lists:map(fun(Cipher) -> 
				rizzo_test(Cipher, Config, Version, Mfa) end,
			Ciphers),
    case lists:flatten(Result) of
	[] ->
	    ok;
	Error ->
	    test_server:format("Cipher suite errors: ~p~n", [Error]),
	    test_server:fail(cipher_suite_failed_see_test_case_log) 
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

client_server_opts({KeyAlgo,_,_}, Config) when KeyAlgo == rsa orelse KeyAlgo == dhe_rsa ->
    {?config(client_opts, Config),
     ?config(server_opts, Config)};   
client_server_opts({KeyAlgo,_,_}, Config) when KeyAlgo == dss orelse KeyAlgo == dhe_dss ->
    {?config(client_dsa_opts, Config),
     ?config(server_dsa_opts, Config)}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
send_recv_result(Socket) ->
    ssl:send(Socket, "Hello world"),
    {ok,"Hello world"} = ssl:recv(Socket, 11),
    ok.

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

send_recv_result_active(Socket) ->
    ssl:send(Socket, "Hello world"),
    receive 
	{ssl, Socket, "H"} ->
	    receive 
		{ssl, Socket, "ello world"} ->
		    ok
	    end;
	{ssl, Socket, "Hello world"} ->
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

send_recv_result_active_once(Socket) ->
    ssl:send(Socket, "Hello world"),
    receive 
	{ssl, Socket, "H"} ->
	    ssl:setopts(Socket, [{active, once}]),
	    receive 
		{ssl, Socket, "ello world"} ->
		    ok
	    end;
	{ssl, Socket, "Hello world"} ->
	    ok
    end.

result_ok(_Socket) ->
    ok.

renegotiate(Socket, Data) ->
    test_server:format("Renegotiating ~n", []),
    Result = ssl:renegotiate(Socket),
    test_server:format("Result ~p~n", [Result]),
    ssl:send(Socket, Data),
    case Result of
	ok ->
	    ok;
	Other ->
	    Other
    end.

renegotiate_reuse_session(Socket, Data) ->
    %% Make sure session is registered
    test_server:sleep(?SLEEP),
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
    test_server:sleep(?RENEGOTIATION_DISABLE_TIME +1),
    ok = ssl:renegotiate(Socket),
    test_server:format("Renegotiated again"),
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
    test_server:format("CA file content: ~p~n", [public_key:pem_decode(PEM)]),

    [{cacertfile, NewCaCertFile}, {certfile, NewCertFile},
     {keyfile, NewKeyFile} | ServerOpts].

session_cache_process_list(doc) -> 
    ["Test reuse of sessions (short handshake)"];

session_cache_process_list(suite) -> 
    [];
session_cache_process_list(Config) when is_list(Config) -> 
    session_cache_process(list,Config).

session_cache_process_mnesia(doc) -> 
    ["Test reuse of sessions (short handshake)"];

session_cache_process_mnesia(suite) -> 
    [];
session_cache_process_mnesia(Config) when is_list(Config) -> 
    session_cache_process(mnesia,Config).

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
	    test_server:fail({unexpected_message, Other})
    after ?SLEEP * 3 ->
	    test_server:fail({did_not_get, Data})
    end.

receive_msg(_) ->
    receive
	Msg ->
	   Msg
    end.
