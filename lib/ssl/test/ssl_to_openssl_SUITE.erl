%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

-module(ssl_to_openssl_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TIMEOUT, 120000).
-define(LONG_TIMEOUT, 600000).
-define(SLEEP, 1000).
-define(OPENSSL_RENEGOTIATE, "r\n").
-define(OPENSSL_QUIT, "Q\n").
-define(OPENSSL_GARBAGE, "P\n").
-define(EXPIRE, 10).

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
    case os:find_executable("openssl") of
	false ->
	    {skip, "Openssl not found"};
	_ ->
	    try crypto:start() of
		ok ->
		    application:start(public_key),
		    ssl:start(),
		    Result =
			(catch make_certs:all(?config(data_dir, Config0),
					      ?config(priv_dir, Config0))),
		    test_server:format("Make certs  ~p~n", [Result]),
		    Config1 = ssl_test_lib:make_dsa_cert(Config0),
		    Config = ssl_test_lib:cert_options(Config1),
		    [{watchdog, Dog} | Config]
		catch _:_  ->
		    {skip, "Crypto did not start"}
	    end
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
init_per_testcase(expired_session, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ssl_test_lib:timetrap(?EXPIRE * 1000 * 5),
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_lifetime, ?EXPIRE),
    ssl:start(),
    [{watchdog, Dog} | Config];

init_per_testcase(TestCase, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ssl_test_lib:timetrap(?TIMEOUT),
    special_init(TestCase, [{watchdog, Dog} | Config]).

special_init(TestCase, Config) 
  when TestCase == erlang_client_openssl_server_renegotiate;
       TestCase == erlang_client_openssl_server_no_wrap_sequence_number;
       TestCase == erlang_server_openssl_client_no_wrap_sequence_number ->
    check_sane_openssl_renegotaite(Config);

special_init(_, Config) ->
    Config.
    
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
    end_per_testcase(default_action, Config);

end_per_testcase(default_action, Config) ->
    Dog = ?config(watchdog, Config),
    case Dog of 
	undefined ->
	    ok;
	_ ->
	    test_server:timetrap_cancel(Dog)
    end;
end_per_testcase(_, Config) ->
    end_per_testcase(default_action, Config).

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
    [erlang_client_openssl_server,
     erlang_server_openssl_client,
     tls1_erlang_client_openssl_server_dsa_cert,
     tls1_erlang_server_openssl_client_dsa_cert,
     ssl3_erlang_client_openssl_server_dsa_cert,
     ssl3_erlang_server_openssl_client_dsa_cert,
     erlang_server_openssl_client_reuse_session,
     erlang_client_openssl_server_renegotiate,
     erlang_client_openssl_server_no_wrap_sequence_number,
     erlang_server_openssl_client_no_wrap_sequence_number,
     erlang_client_openssl_server_no_server_ca_cert,
     ssl3_erlang_client_openssl_server,
     ssl3_erlang_server_openssl_client,
     ssl3_erlang_client_openssl_server_client_cert,
     ssl3_erlang_server_openssl_client_client_cert,
     ssl3_erlang_server_erlang_client_client_cert,
     tls1_erlang_client_openssl_server,
     tls1_erlang_server_openssl_client,
     tls1_erlang_client_openssl_server_client_cert,
     tls1_erlang_server_openssl_client_client_cert,
     tls1_erlang_server_erlang_client_client_cert,
     ciphers_rsa_signed_certs, ciphers_dsa_signed_certs,
     erlang_client_bad_openssl_server, expired_session,
     ssl2_erlang_server_openssl_client].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Test cases starts here.
%%--------------------------------------------------------------------

erlang_client_openssl_server(doc) ->
    ["Test erlang client with openssl server"];
erlang_client_openssl_server(suite) ->
    [];
erlang_client_openssl_server(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  
    ClientOpts = ?config(client_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",
    
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile  ++ " -key " ++ KeyFile, 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, ClientOpts}]),
    port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok),
  
    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.


%%--------------------------------------------------------------------    
erlang_server_openssl_client(doc) ->
    ["Test erlang server with openssl client"];
erlang_server_openssl_client(suite) ->
    [];
erlang_server_openssl_client(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ 
	" -host localhost",

    test_server:format("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),
    
    ssl_test_lib:close(Server),

    close_port(OpenSslPort),
    process_flag(trap_exit, false),
    ok.

%%-------------------------------------------------------------------- 

tls1_erlang_client_openssl_server_dsa_cert(doc) ->
    ["Test erlang server with openssl client"];
tls1_erlang_client_openssl_server_dsa_cert(suite) ->
    [];
tls1_erlang_client_openssl_server_dsa_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = ?config(client_dsa_opts, Config),  
    ServerOpts = ?config(server_dsa_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
  
    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CaCertFile =  proplists:get_value(cacertfile, ServerOpts),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -CAfile " ++ CaCertFile  
	++ " -key " ++ KeyFile ++ " -Verify 2 -tls1 -msg", 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, ClientOpts}]),

    port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok), 
   
    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.

%%-------------------------------------------------------------------- 

tls1_erlang_server_openssl_client_dsa_cert(doc) ->
    ["Test erlang server with openssl client"];
tls1_erlang_server_openssl_client_dsa_cert(suite) ->
    [];
tls1_erlang_server_openssl_client_dsa_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = ?config(client_dsa_opts, Config),
    ServerOpts = ?config(server_dsa_verify_opts, Config),  

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",
    CaCertFile =  proplists:get_value(cacertfile, ClientOpts),
    CertFile = proplists:get_value(certfile, ClientOpts),
    KeyFile = proplists:get_value(keyfile, ClientOpts),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ 
	" -host localhost " ++	" -cert " ++ CertFile ++ " -CAfile " ++ CaCertFile  
	++ " -key " ++ KeyFile ++ " -tls1 -msg",

    test_server:format("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),
    
    ssl_test_lib:close(Server),

    close_port(OpenSslPort),
    process_flag(trap_exit, false),
    ok.

%%-------------------------------------------------------------------- 

ssl3_erlang_client_openssl_server_dsa_cert(doc) ->
    ["Test erlang server with openssl client"];
ssl3_erlang_client_openssl_server_dsa_cert(suite) ->
    [];
ssl3_erlang_client_openssl_server_dsa_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = ?config(client_dsa_opts, Config),  
    ServerOpts = ?config(server_dsa_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
  
    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CaCertFile =  proplists:get_value(cacertfile, ServerOpts),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -CAfile " ++ CaCertFile  
	++ " -key " ++ KeyFile ++ " -Verify 2 -ssl3 -msg", 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, ClientOpts}]),

    port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok), 
   
    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.

%%-------------------------------------------------------------------- 

ssl3_erlang_server_openssl_client_dsa_cert(doc) ->
    ["Test erlang server with openssl client"];
ssl3_erlang_server_openssl_client_dsa_cert(suite) ->
    [];
ssl3_erlang_server_openssl_client_dsa_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = ?config(client_dsa_opts, Config),
    ServerOpts = ?config(server_dsa_verify_opts, Config),  

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",
    CaCertFile =  proplists:get_value(cacertfile, ClientOpts),
    CertFile = proplists:get_value(certfile, ClientOpts),
    KeyFile = proplists:get_value(keyfile, ClientOpts),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ 
	" -host localhost " ++	" -cert " ++ CertFile ++ " -CAfile " ++ CaCertFile  
	++ " -key " ++ KeyFile ++ " -ssl3 -msg",

    test_server:format("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),
    
    ssl_test_lib:close(Server),

    close_port(OpenSslPort),
    process_flag(trap_exit, false),
    ok.


%%-------------------------------------------------------------------- 

erlang_server_openssl_client_reuse_session(doc) ->
    ["Test erlang server with openssl client that reconnects with the"
     "same session id, to test reusing of sessions."];
erlang_server_openssl_client_reuse_session(suite) ->
    [];
erlang_server_openssl_client_reuse_session(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
			   {reconnect_times, 5},		
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ 
	" -host localhost -reconnect",

    test_server:format("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),
    
    ssl_test_lib:close(Server),

    close_port(OpenSslPort),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

erlang_client_openssl_server_renegotiate(doc) ->
    ["Test erlang client when openssl server issuses a renegotiate"];
erlang_client_openssl_server_renegotiate(suite) ->
    [];
erlang_client_openssl_server_renegotiate(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  
    ClientOpts = ?config(client_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    ErlData = "From erlang to openssl",
    OpenSslData = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ " -msg", 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       delayed_send, [[ErlData, OpenSslData]]}},
					{options, ClientOpts}]),

    port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
    test_server:sleep(?SLEEP),
    port_command(OpensslPort, OpenSslData),
    
    ssl_test_lib:check_result(Client, ok), 
   
    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

erlang_client_openssl_server_no_wrap_sequence_number(doc) ->
    ["Test that erlang client will renegotiate session when",  
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at" 
     " to lower treashold substantially."];
erlang_client_openssl_server_no_wrap_sequence_number(suite) ->
    [];
erlang_client_openssl_server_no_wrap_sequence_number(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  
    ClientOpts = ?config(client_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    ErlData = "From erlang to openssl\n",
    N = 10,

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ " -msg", 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {ssl_test_lib, 
					       trigger_renegotiate, [[ErlData, N+2]]}},
					{options, [{reuse_sessions, false},
						   {renegotiate_at, N} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok), 

    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------
erlang_server_openssl_client_no_wrap_sequence_number(doc) ->
    ["Test that erlang client will renegotiate session when",  
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at" 
     " to lower treashold substantially."];

erlang_server_openssl_client_no_wrap_sequence_number(suite) ->
    [];
erlang_server_openssl_client_no_wrap_sequence_number(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",
    
    N = 10,

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {ssl_test_lib, 
					       trigger_renegotiate, [[Data, N+2]]}},
					{options, [{renegotiate_at, N} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ 
	" -host localhost -msg",

    test_server:format("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),
    
    ssl_test_lib:close(Server),

    close_port(OpenSslPort),
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------

erlang_client_openssl_server_no_server_ca_cert(doc) ->
    ["Test erlang client when openssl server sends a cert chain not"
     "including the ca cert. Explicitly test this even if it is"
     "implicitly tested eleswhere."];
erlang_client_openssl_server_no_server_ca_cert(suite) ->
    [];
erlang_client_openssl_server_no_server_ca_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  
    ClientOpts = ?config(client_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ " -msg", 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, ClientOpts}]),

    port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok), 

    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------
ssl3_erlang_client_openssl_server(doc) ->
    ["Test erlang client with openssl server"];
ssl3_erlang_client_openssl_server(suite) ->
    [];
ssl3_erlang_client_openssl_server(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),  
    ClientOpts = ?config(client_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ " -ssl3", 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       connection_info, [sslv3]}},
					{options, 
					 [{versions, [sslv3]} | ClientOpts]}]),
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Client),
    %% Clean close down!
    close_port(OpensslPort),
    test_server:sleep(?SLEEP),
    ok.

%%--------------------------------------------------------------------
 
ssl3_erlang_server_openssl_client(doc) ->
    ["Test erlang server with openssl client"];
ssl3_erlang_server_openssl_client(suite) ->
    [];
ssl3_erlang_server_openssl_client(Config) when is_list(Config) ->
    ServerOpts = ?config(server_opts, Config),  
    
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, 
					 {?MODULE, connection_info, [sslv3]}},
					{options, 
					 [{versions, [sslv3]} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ 
	" -host localhost -ssl3",

    test_server:format("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    ssl_test_lib:check_result(Server, ok),
    
    close_port(OpenSslPort), %% openssl server first
    ssl_test_lib:close(Server),
    test_server:sleep(?SLEEP),
    ok.

%%--------------------------------------------------------------------
ssl3_erlang_client_openssl_server_client_cert(doc) ->
    ["Test erlang client with openssl server when client sends cert"];
ssl3_erlang_client_openssl_server_client_cert(suite) ->
    [];
ssl3_erlang_client_openssl_server_client_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_verification_opts, Config),  
    ClientOpts = ?config(client_verification_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",
    
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    CaCertFile = proplists:get_value(cacertfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -CAfile " ++ CaCertFile 
	++ " -key " ++ KeyFile ++ " -Verify 2 -ssl3", 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, ClientOpts}]),
    port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok),
  
    %% Clean close down!
    close_port(OpensslPort), 
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------  

ssl3_erlang_server_openssl_client_client_cert(doc) ->
    ["Test erlang server with openssl client when client sends cert"];
ssl3_erlang_server_openssl_client_client_cert(suite) ->
    [];
ssl3_erlang_server_openssl_client_client_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_verification_opts, Config),  
    ClientOpts = ?config(client_verification_opts, Config),  

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, 
					 [{verify , verify_peer} 
					  | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    
    CaCertFile = proplists:get_value(cacertfile, ClientOpts),
    CertFile = proplists:get_value(certfile, ClientOpts),
    KeyFile = proplists:get_value(keyfile, ClientOpts),
    
    Cmd = "openssl s_client -cert " ++ CertFile  ++ " -CAfile " ++ CaCertFile 
	++ " -key " ++ KeyFile ++ " -port " ++ integer_to_list(Port)  ++ 
	" -host localhost -ssl3",

    test_server:format("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),
    
    close_port(OpenSslPort), %% openssl server first
    ssl_test_lib:close(Server),
    %% Clean close down!
    process_flag(trap_exit, false),
    ok.


%%--------------------------------------------------------------------  

ssl3_erlang_server_erlang_client_client_cert(doc) ->
    ["Test erlang server with erlang client when client sends cert"];
ssl3_erlang_server_erlang_client_client_cert(suite) ->
    [];
ssl3_erlang_server_erlang_client_client_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_verification_opts, Config),  
    ClientOpts = ?config(client_verification_opts, Config),  

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From erlang to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, 
					 [{verify , verify_peer} 
					  | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {ssl, send, [Data]}},
					{options, 
					 [{versions, [sslv3]} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

tls1_erlang_client_openssl_server(doc) ->
    ["Test erlang client with openssl server"];
tls1_erlang_client_openssl_server(suite) ->
    [];
tls1_erlang_client_openssl_server(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  
    ClientOpts = ?config(client_opts, Config),  


    test_server:format("Server Opts", [ServerOpts]),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),

    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ " -tls1", 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),
    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       connection_info, [tlsv1]}},
					{options, 
					 [{versions, [tlsv1]} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Client),
    %% Clean close down!
    close_port(OpensslPort),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

tls1_erlang_server_openssl_client(doc) ->
    ["Test erlang server with openssl client"];
tls1_erlang_server_openssl_client(suite) ->
    [];
tls1_erlang_server_openssl_client(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, 
					 {?MODULE, connection_info, [tlsv1]}},
					{options, 
					 [{versions, [tlsv1]} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ 
	" -host localhost -tls1",

    test_server:format("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
     
    ssl_test_lib:check_result(Server, ok),
    
    %% Clean close down!
    close_port(OpenSslPort),
    ssl_test_lib:close(Server),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

tls1_erlang_client_openssl_server_client_cert(doc) ->
    ["Test erlang client with openssl server when client sends cert"];
tls1_erlang_client_openssl_server_client_cert(suite) ->
    [];
tls1_erlang_client_openssl_server_client_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_verification_opts, Config),  
    ClientOpts = ?config(client_verification_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",
    
    Port = ssl_test_lib:inet_port(node()),
    CaCertFile = proplists:get_value(cacertfile, ServerOpts),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -CAfile " ++ CaCertFile 
	++ " -key " ++ KeyFile ++ " -Verify 2 -tls1", 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, ClientOpts}]),
    port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok),
  
    %% Clean close down!
    close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------  

tls1_erlang_server_openssl_client_client_cert(doc) ->
    ["Test erlang server with openssl client when client sends cert"];
tls1_erlang_server_openssl_client_client_cert(suite) ->
    [];
tls1_erlang_server_openssl_client_client_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_verification_opts, Config),  
    ClientOpts = ?config(client_verification_opts, Config),  

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, 
					 [{verify , verify_peer} 
					  | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    
    CaCertFile = proplists:get_value(cacertfile, ClientOpts),
    CertFile = proplists:get_value(certfile, ClientOpts),
    KeyFile = proplists:get_value(keyfile, ClientOpts),
    
    Cmd = "openssl s_client -cert " ++ CertFile ++ " -CAfile " ++ CaCertFile 
	++ " -key " ++ KeyFile ++ " -port " ++ integer_to_list(Port)  ++ 
	" -host localhost -tls1",

    test_server:format("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),
    
    %% Clean close down!
    close_port(OpenSslPort),
    ssl_test_lib:close(Server),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------
tls1_erlang_server_erlang_client_client_cert(doc) ->
    ["Test erlang server with erlang client when client sends cert"];
tls1_erlang_server_erlang_client_client_cert(suite) ->
    [];
tls1_erlang_server_erlang_client_client_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_verification_opts, Config),  
    ClientOpts = ?config(client_verification_opts, Config),  

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From erlang to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, 
					 [{verify , verify_peer} 
					  | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {ssl, send, [Data]}},
					{options, 
					 [{versions, [tlsv1]} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    %% Clean close down!
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------

ciphers_rsa_signed_certs(doc) -> 
    ["Test cipher suites that uses rsa certs"];
       
ciphers_rsa_signed_certs(suite) -> 
    [];

ciphers_rsa_signed_certs(Config) when is_list(Config) ->
    Version = 
	ssl_record:protocol_version(ssl_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:rsa_suites(),
    run_suites(Ciphers, Version, Config, rsa).


ciphers_dsa_signed_certs(doc) -> 
    ["Test cipher suites that uses dsa certs"];
       
ciphers_dsa_signed_certs(suite) -> 
    [];

ciphers_dsa_signed_certs(Config) when is_list(Config) ->
    Version = 
	ssl_record:protocol_version(ssl_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:dsa_suites(),
    run_suites(Ciphers, Version, Config, dsa).

run_suites(Ciphers, Version, Config, Type) ->
    {ClientOpts, ServerOpts} =
	case Type of 
	    rsa ->
		{?config(client_opts, Config),
		 ?config(server_opts, Config)};
	    dsa ->
		{?config(client_opts, Config),
		 ?config(server_dsa_opts, Config)}
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

cipher(CipherSuite, Version, Config, ClientOpts, ServerOpts) ->   
    process_flag(trap_exit, true),
    test_server:format("Testing CipherSuite ~p~n", [CipherSuite]),
    {ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ "",

    test_server:format("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    
    wait_for_openssl_server(),

    ConnectionInfo = {ok, {Version, CipherSuite}},

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, cipher_result, [ConnectionInfo]}},
					{options,
					 [{ciphers,[CipherSuite]} |
			     ClientOpts]}]),

    port_command(OpenSslPort, "Hello\n"),
    
    receive
	{Port, {data, _}} when is_port(Port) ->
	    ok
    after 500 ->
	    test_server:format("Time out on openssl port, check that"
			       " the messages Hello and world are received"
			       " during close of port" , []),
	    ok
    end,

    port_command(OpenSslPort, " world\n"),
    
    Result = ssl_test_lib:wait_for_result(Client, ok),    

    close_port(OpenSslPort),
    %% Clean close down!
    ssl_test_lib:close(Client),
    
    Return = case Result of
		 ok ->
		     [];
		 Error ->
		     [{CipherSuite, Error}]
	     end,
    process_flag(trap_exit, false),
    Return.

%%--------------------------------------------------------------------
erlang_client_bad_openssl_server(doc) ->
    [""];
erlang_client_bad_openssl_server(suite) ->
    [];
erlang_client_bad_openssl_server(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_verification_opts, Config),  
    ClientOpts = ?config(client_verification_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ "", 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),
    
    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, server_sent_garbage, []}},
					{options, 
					 [{versions, [tlsv1]} | ClientOpts]}]),
    
    %% Send garbage
    port_command(OpensslPort, ?OPENSSL_GARBAGE),
    
    test_server:sleep(?SLEEP),

    Client0 ! server_sent_garbage,
    
    ssl_test_lib:check_result(Client0, true),
    
    ssl_test_lib:close(Client0),
    
    %% Make sure openssl does not hang and leave zombie process
    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					 {host, Hostname},
					 {from, self()}, 
					 {mfa, {ssl_test_lib, no_result_msg, []}},
					 {options, 
					  [{versions, [tlsv1]} | ClientOpts]}]),
    
    ssl_test_lib:close(Client1),
    
    %% Clean close down!
    close_port(OpensslPort),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

expired_session(doc) -> 
    ["Test our ssl client handling of expired sessions. Will make"
    "better code coverage of the ssl_manager module"];

expired_session(suite) -> 
    [];

expired_session(Config) when is_list(Config) -> 
    process_flag(trap_exit, true),
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
   
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ 
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ "", 
    
    test_server:format("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    wait_for_openssl_server(),
    
    Client0 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
			    {mfa, {ssl_test_lib, no_result, []}},
		      {from, self()},  {options, ClientOpts}]),   
         
    ssl_test_lib:close(Client0),

    %% Make sure session is registered
    test_server:sleep(?SLEEP),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {from, self()},  {options, ClientOpts}]),  
    
    ssl_test_lib:close(Client1),
    %% Make sure session is unregistered due to expiration
    test_server:sleep((?EXPIRE+1) * 1000),
    
    Client2 =
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {from, self()},  {options, ClientOpts}]),  

    close_port(OpensslPort),
    ssl_test_lib:close(Client2),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------
ssl2_erlang_server_openssl_client(doc) ->
    ["Test that ssl v2 clients are rejected"];
ssl2_erlang_server_openssl_client(suite) ->
    [];
ssl2_erlang_server_openssl_client(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ 
	" -host localhost -ssl2 -msg",

    test_server:format("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, {error,"protocol version"}),
    
    ssl_test_lib:close(Server),

    close_port(OpenSslPort),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

erlang_ssl_receive(Socket, Data) ->
    test_server:format("Connection info: ~p~n", 
		       [ssl:connection_info(Socket)]),
    receive
	{ssl, Socket, Data} ->
	    io:format("Received ~p~n",[Data]),
	    %% open_ssl server sometimes hangs waiting in blocking read
	    ssl:send(Socket, "Got it"), 
	    ok;
	{Port, {data,Debug}} when is_port(Port) ->
	    io:format("openssl ~s~n",[Debug]),
	    erlang_ssl_receive(Socket,Data);
	Other ->
	    test_server:fail({unexpected_message, Other})
    after 4000 ->
	    test_server:fail({did_not_get, Data})
    end.
 
connection_info(Socket, Version) ->
    case ssl:connection_info(Socket) of
	{ok, {Version, _} = Info} ->
	    test_server:format("Connection info: ~p~n", [Info]),
	    ok;
	{ok, {OtherVersion, _}} ->
	    {wrong_version, OtherVersion}
    end.

connection_info_result(Socket) ->                                            
    ssl:connection_info(Socket).


delayed_send(Socket, [ErlData, OpenSslData]) ->
    test_server:sleep(?SLEEP),
    ssl:send(Socket, ErlData),
    erlang_ssl_receive(Socket, OpenSslData).

close_port(Port) ->
    catch port_command(Port, ?OPENSSL_QUIT),
    close_loop(Port, 500, false).

close_loop(Port, Time, SentClose) ->
    receive 
	{Port, {data,Debug}} when is_port(Port) ->
	    io:format("openssl ~s~n",[Debug]),
	    close_loop(Port, Time, SentClose);	
	{ssl,_,Msg} ->
	    io:format("ssl Msg ~s~n",[Msg]),
	    close_loop(Port, Time, SentClose);	
	{Port, closed} -> 
	    io:format("Port Closed~n",[]),
	    ok;
	{'EXIT', Port, Reason} ->
	    io:format("Port Closed ~p~n",[Reason]),
	    ok;
	Msg ->
	    io:format("Port Msg ~p~n",[Msg]),
	    close_loop(Port, Time, SentClose)
    after Time ->
	    case SentClose of
		false -> 
		    io:format("Closing port ~n",[]),
		    catch erlang:port_close(Port),
		    close_loop(Port, Time, true);
		true ->
		    io:format("Timeout~n",[])
	    end
    end.


server_sent_garbage(Socket) ->
    receive 
	server_sent_garbage ->
	    {error, closed} == ssl:send(Socket, "data")
	    
    end.
    
wait_for_openssl_server() ->
    receive
     	{Port, {data, Debug}} when is_port(Port) ->
 	    io:format("openssl ~s~n",[Debug]),
	    %% openssl has started make sure
	    %% it will be in accept. Parsing
	    %% output is too error prone. (Even 
	    %% more so than sleep!)
	    test_server:sleep(?SLEEP)
    end.
	    
check_sane_openssl_renegotaite(Config) ->
    case os:cmd("openssl version") of
	"OpenSSL 0.9.8" ++ _ ->
	    {skip, "Known renegotiation bug in OppenSSL"};
	"OpenSSL 0.9.7" ++ _ ->
	    {skip, "Known renegotiation bug in OppenSSL"};
	_ ->
	    Config
    end.
