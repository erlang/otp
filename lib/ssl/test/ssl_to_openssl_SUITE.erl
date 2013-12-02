%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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
-define(OPENSSL_RENEGOTIATE, "R\n").
-define(OPENSSL_QUIT, "Q\n").
-define(OPENSSL_GARBAGE, "P\n").
-define(EXPIRE, 10).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [
     {group, basic},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'sslv3'}
    ].

groups() ->
    [{basic, [], basic_tests()},
     {'tlsv1.2', [], all_versions_tests() ++ npn_tests()},
     {'tlsv1.1', [], all_versions_tests() ++ npn_tests()},
     {'tlsv1', [], all_versions_tests()++ npn_tests()},
     {'sslv3', [], all_versions_tests()}].

basic_tests() ->
    [basic_erlang_client_openssl_server,
     basic_erlang_server_openssl_client,
     expired_session].

all_versions_tests() ->
    [
     erlang_client_openssl_server,
     erlang_server_openssl_client,
     erlang_client_openssl_server_dsa_cert,
     erlang_server_openssl_client_dsa_cert,
     erlang_server_openssl_client_reuse_session,
     erlang_client_openssl_server_renegotiate,
     erlang_client_openssl_server_nowrap_seqnum,
     erlang_server_openssl_client_nowrap_seqnum,
     erlang_client_openssl_server_no_server_ca_cert,
     erlang_client_openssl_server_client_cert,
     erlang_server_openssl_client_client_cert,
     ciphers_rsa_signed_certs,
     ciphers_dsa_signed_certs,
     erlang_client_bad_openssl_server,
     expired_session,
     ssl2_erlang_server_openssl_client].

npn_tests() ->
    [erlang_client_openssl_server_npn,
     erlang_server_openssl_client_npn,
     erlang_server_openssl_client_npn_renegotiate,
     erlang_client_openssl_server_npn_renegotiate,
     erlang_server_openssl_client_npn_only_client,
     erlang_server_openssl_client_npn_only_server,
     erlang_client_openssl_server_npn_only_client,
     erlang_client_openssl_server_npn_only_server].


init_per_suite(Config0) ->
    Dog = ct:timetrap(?LONG_TIMEOUT *2),
    case os:find_executable("openssl") of
	false ->
	    {skip, "Openssl not found"};
	_ ->
	    catch crypto:stop(),
	    try crypto:start() of
		ok ->
		    ssl:start(),
		    Result =
			(catch make_certs:all(?config(data_dir, Config0),
					      ?config(priv_dir, Config0))),
		    ct:log("Make certs  ~p~n", [Result]),
		    Config1 = ssl_test_lib:make_dsa_cert(Config0),
		    Config2 = ssl_test_lib:cert_options(Config1),
		    Config = [{watchdog, Dog} | Config2],
		    ssl_test_lib:cipher_restriction(Config)
		catch _:_  ->
		    {skip, "Crypto did not start"}
	    end
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
	true ->
	    case ssl_test_lib:check_sane_openssl_version(GroupName) of
		true ->
		    ssl_test_lib:init_tls_version(GroupName),
		    Config;
		false ->
		    {skip, openssl_does_not_support_version}
	    end;
	_ ->
	    ssl:start(),
	    Config
    end.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(expired_session, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ct:timetrap(?EXPIRE * 1000 * 5),
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_lifetime, ?EXPIRE),
    ssl:start(),
    [{watchdog, Dog} | Config];

init_per_testcase(TestCase, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ct:timetrap(?TIMEOUT),
    special_init(TestCase, [{watchdog, Dog} | Config]).

special_init(TestCase, Config)
  when TestCase == erlang_client_openssl_server_renegotiate;
       TestCase == erlang_client_openssl_server_nowrap_seqnum;
       TestCase == erlang_server_openssl_client_nowrap_seqnum
       ->
    check_sane_openssl_renegotaite(Config);

special_init(ssl2_erlang_server_openssl_client, Config) ->
    check_sane_openssl_sslv2(Config);

special_init(TestCase, Config)
    when TestCase == erlang_client_openssl_server_npn;
         TestCase == erlang_server_openssl_client_npn;
         TestCase == erlang_server_openssl_client_npn_renegotiate;
         TestCase == erlang_client_openssl_server_npn_renegotiate;
         TestCase == erlang_server_openssl_client_npn_only_server;
         TestCase == erlang_server_openssl_client_npn_only_client;
         TestCase == erlang_client_openssl_server_npn_only_client;
         TestCase == erlang_client_openssl_server_npn_only_server ->
    check_openssl_npn_support(Config);

special_init(_, Config) ->
    Config.

end_per_testcase(reuse_session_expired, Config) ->
    application:unset_env(ssl, session_lifetime),
    Config;
end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
basic_erlang_client_openssl_server() ->
    [{doc,"Test erlang client with openssl server"}].
basic_erlang_client_openssl_server(Config) when is_list(Config) ->
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
    
    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    ssl_test_lib:wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, ClientOpts}]),
    true = port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok),
  
    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------    
basic_erlang_server_openssl_client() ->
    [{doc,"Test erlang server with openssl client"}].
basic_erlang_server_openssl_client(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Cmd = "openssl s_client -port " ++ integer_to_list(Port) ++
	" -host localhost",

    ct:log("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    true = port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    close_port(OpenSslPort),
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------
erlang_client_openssl_server() ->
    [{doc,"Test erlang client with openssl server"}].
erlang_client_openssl_server(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),
    ClientOpts = ?config(client_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++  ssl_test_lib:version_flag(Version) ++
	" -cert " ++ CertFile  ++ " -key " ++ KeyFile,
    
    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    ssl_test_lib:wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, ClientOpts}]),
    true = port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok),

    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------
erlang_server_openssl_client() ->
    [{doc,"Test erlang server with openssl client"}].
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
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ ssl_test_lib:version_flag(Version) ++
	" -host localhost",

    ct:log("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    true = port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    close_port(OpenSslPort),
    process_flag(trap_exit, false).

%%-------------------------------------------------------------------- 

erlang_client_openssl_server_dsa_cert() ->
    [{doc,"Test erlang server with openssl client"}].
erlang_client_openssl_server_dsa_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = ?config(client_dsa_opts, Config),  
    ServerOpts = ?config(server_dsa_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
  
    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CaCertFile =  proplists:get_value(cacertfile, ServerOpts),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++  ssl_test_lib:version_flag(Version) ++
	" -cert " ++ CertFile ++ " -CAfile " ++ CaCertFile  
	++ " -key " ++ KeyFile ++ " -Verify 2 -msg",
    
    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    ssl_test_lib:wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, ClientOpts}]),

    true = port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok), 
   
    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------
erlang_server_openssl_client_dsa_cert() ->
    [{doc,"Test erlang server with openssl client"}].
erlang_server_openssl_client_dsa_cert(Config) when is_list(Config) ->
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
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ ssl_test_lib:version_flag(Version) ++
       " -host localhost " ++  " -cert " ++ CertFile ++ " -CAfile " ++ CaCertFile
       ++ " -key " ++ KeyFile ++ " -msg",

    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]),
    true = port_command(OpenSslPort, Data),

    ssl_test_lib:check_result(Server, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    close_port(OpenSslPort),
    process_flag(trap_exit, false).

%%-------------------------------------------------------------------- 

erlang_server_openssl_client_reuse_session() ->
    [{doc, "Test erlang server with openssl client that reconnects with the"
     "same session id, to test reusing of sessions."}].
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
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ ssl_test_lib:version_flag(Version) ++
	" -host localhost -reconnect",

    ct:log("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    true = port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),
    
    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    close_port(OpenSslPort),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

erlang_client_openssl_server_renegotiate() ->
    [{doc,"Test erlang client when openssl server issuses a renegotiate"}].
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
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ ssl_test_lib:version_flag(Version) ++
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ " -msg", 
    
    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    ssl_test_lib:wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       delayed_send, [[ErlData, OpenSslData]]}},
					{options, ClientOpts}]),

    true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
    ct:sleep(?SLEEP),
    true = port_command(OpensslPort, OpenSslData),
    
    ssl_test_lib:check_result(Client, ok), 
   
    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

erlang_client_openssl_server_nowrap_seqnum() ->
    [{doc, "Test that erlang client will renegotiate session when",
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at" 
     " to lower treashold substantially."}].
erlang_client_openssl_server_nowrap_seqnum(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  
    ClientOpts = ?config(client_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    ErlData = "From erlang to openssl\n",
    N = 10,

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ ssl_test_lib:version_flag(Version) ++
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ " -msg", 
    
    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    ssl_test_lib:wait_for_openssl_server(),

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
    process_flag(trap_exit, false).
%%--------------------------------------------------------------------
erlang_server_openssl_client_nowrap_seqnum() ->
    [{doc, "Test that erlang client will renegotiate session when",
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at" 
     " to lower treashold substantially."}].
erlang_server_openssl_client_nowrap_seqnum(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",
    
    N = 10,

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {ssl_test_lib, 
					       trigger_renegotiate, [[Data, N+2]]}},
					{options, [{renegotiate_at, N}, {reuse_sessions, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_client -port " ++ integer_to_list(Port)  ++ ssl_test_lib:version_flag(Version) ++
	" -host localhost -msg",

    ct:log("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    true = port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),
    
    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    close_port(OpenSslPort),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------

erlang_client_openssl_server_no_server_ca_cert() ->
    [{doc, "Test erlang client when openssl server sends a cert chain not"
     "including the ca cert. Explicitly test this even if it is"
     "implicitly tested eleswhere."}].
erlang_client_openssl_server_no_server_ca_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),  
    ClientOpts = ?config(client_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ ssl_test_lib:version_flag(Version) ++
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ " -msg", 
    
    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    ssl_test_lib:wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, ClientOpts}]),

    true = port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok), 

    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------
erlang_client_openssl_server_client_cert() ->
    [{doc,"Test erlang client with openssl server when client sends cert"}].
erlang_client_openssl_server_client_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_verification_opts, Config),  
    ClientOpts = ?config(client_verification_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",
    
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    CaCertFile = proplists:get_value(cacertfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++ ssl_test_lib:version_flag(Version) ++
	" -cert " ++ CertFile ++ " -CAfile " ++ CaCertFile 
	++ " -key " ++ KeyFile ++ " -Verify 2",
    
    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    ssl_test_lib:wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, [Data]}},
					{options, ClientOpts}]),
    true = port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok),
  
    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort), 
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------  

erlang_server_openssl_client_client_cert() ->
    [{doc,"Test erlang server with openssl client when client sends cert"}].
erlang_server_openssl_client_client_cert(Config) when is_list(Config) ->
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
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_client -cert " ++ CertFile  ++ " -CAfile " ++ CaCertFile 
	++ " -key " ++ KeyFile ++ " -port " ++ integer_to_list(Port)  ++ ssl_test_lib:version_flag(Version) ++
	" -host localhost",

    ct:log("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    true = port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),
    
    %% Clean close down!   Server needs to be closed first !!
    close_port(OpenSslPort),
    ssl_test_lib:close(Server),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------  

erlang_server_erlang_client_client_cert() ->
    [{doc,"Test erlang server with erlang client when client sends cert"}].
erlang_server_erlang_client_client_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_verification_opts, Config),  
    ClientOpts = ?config(client_verification_opts, Config),  
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From erlang to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, 
					       erlang_ssl_receive, 
					       %% Due to 1/n-1 splitting countermeasure Rizzo/Duong-Beast
					       [Data]}},
					{options, 
					 [{verify , verify_peer} 
					  | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					%% Due to 1/n-1 splitting countermeasure Rizzo/Duong-Beast
					{mfa, {ssl, send, [Data]}},
					{options, 
					 [{versions, [Version]} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------

ciphers_rsa_signed_certs() ->
    [{doc,"Test cipher suites that uses rsa certs"}].
ciphers_rsa_signed_certs(Config) when is_list(Config) ->
    Version = 
	tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:rsa_suites(openssl),
    run_suites(Ciphers, Version, Config, rsa).
%%--------------------------------------------------------------------

ciphers_dsa_signed_certs() ->
    [{doc,"Test cipher suites that uses dsa certs"}].
ciphers_dsa_signed_certs(Config) when is_list(Config) ->
    Version = 
	tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Ciphers = ssl_test_lib:dsa_suites(),
    run_suites(Ciphers, Version, Config, dsa).

%%--------------------------------------------------------------------
erlang_client_bad_openssl_server() ->
    [{doc,"Test what happens if openssl server sends garbage to erlang ssl client"}].
erlang_client_bad_openssl_server(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_verification_opts, Config),  
    ClientOpts = ?config(client_verification_opts, Config),  

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++  ssl_test_lib:version_flag(Version) ++
 	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ "",

    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    
    ssl_test_lib:wait_for_openssl_server(),
    
    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					 {host, Hostname},
					 {from, self()}, 
 					{mfa, {?MODULE, server_sent_garbage, []}},
					 {options,
					  [{versions, [Version]} | ClientOpts]}]),
    
    %% Send garbage
    true = port_command(OpensslPort, ?OPENSSL_GARBAGE),

    ct:sleep(?SLEEP),

    Client0 ! server_sent_garbage,
    
    ssl_test_lib:check_result(Client0, true),
    
    ssl_test_lib:close(Client0),
    
    %% Make sure openssl does not hang and leave zombie process
    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
 					 {host, Hostname},
 					 {from, self()},
 					 {mfa, {ssl_test_lib, no_result_msg, []}},
 					 {options,
					  [{versions, [Version]} | ClientOpts]}]),

    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),
    ssl_test_lib:close(Client1),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

expired_session() ->
    [{doc, "Test our ssl client handling of expired sessions. Will make"
      "better code coverage of the ssl_manager module"}].
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
    
    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 

    ssl_test_lib:wait_for_openssl_server(),
    
    Client0 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
			    {mfa, {ssl_test_lib, no_result, []}},
		      {from, self()},  {options, ClientOpts}]),   
         
    ssl_test_lib:close(Client0),

    %% Make sure session is registered
    ct:sleep(?SLEEP),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {from, self()},  {options, ClientOpts}]),  
    
    ssl_test_lib:close(Client1),
    %% Make sure session is unregistered due to expiration
    ct:sleep((?EXPIRE+1) * 1000),
    
    Client2 =
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {from, self()},  {options, ClientOpts}]),  

    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),
    ssl_test_lib:close(Client2),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------
ssl2_erlang_server_openssl_client() ->
    [{doc,"Test that ssl v2 clients are rejected"}].

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

    ct:log("openssl cmd: ~p~n", [Cmd]),
    
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]), 
    true = port_command(OpenSslPort, Data),
    
    ct:log("Ports ~p~n", [[erlang:port_info(P) || P <- erlang:ports()]]), 
    receive
	{'EXIT', OpenSslPort, _} = Exit ->
	    ct:log("Received: ~p ~n", [Exit]),
	    ok

    end,
    ssl_test_lib:check_result(Server, {error, {tls_alert, "protocol version"}}),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------
erlang_client_openssl_server_npn() ->
    [{doc,"Test erlang client with openssl server doing npn negotiation"}].

erlang_client_openssl_server_npn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_npn_negotiation(Config, Data, fun(Client, OpensslPort) ->
        true = port_command(OpensslPort, Data),

        ssl_test_lib:check_result(Client, ok)
    end),
    ok.

%%--------------------------------------------------------------------
erlang_client_openssl_server_npn_renegotiate() ->
    [{doc,"Test erlang client with openssl server doing npn negotiation and renegotiate"}].

erlang_client_openssl_server_npn_renegotiate(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_npn_negotiation(Config, Data, fun(Client, OpensslPort) ->
        true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
        ct:sleep(?SLEEP),
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Client, ok)
    end),
    ok.
%%--------------------------------------------------------------------------
erlang_server_openssl_client_npn() ->
    [{doc,"Test erlang server with openssl client and npn negotiation"}].

erlang_server_openssl_client_npn(Config) when is_list(Config) ->

    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_npn_negotiation(Config, Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

%%--------------------------------------------------------------------------
erlang_server_openssl_client_npn_renegotiate() ->
    [{doc,"Test erlang server with openssl client and npn negotiation with renegotiation"}].

erlang_server_openssl_client_npn_renegotiate(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_npn_negotiation(Config, Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
        ct:sleep(?SLEEP),
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.
%%--------------------------------------------------------------------------
erlang_client_openssl_server_npn_only_server(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_with_opts(Config, [],
						     "-nextprotoneg spdy/2", Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

%%--------------------------------------------------------------------------

erlang_client_openssl_server_npn_only_client(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_with_opts(Config,
						     [{client_preferred_next_protocols,
						       {client, [<<"spdy/2">>], <<"http/1.1">>}}], "",
						     Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

%%--------------------------------------------------------------------------
erlang_server_openssl_client_npn_only_server(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_with_opts(Config, [{next_protocols_advertised, [<<"spdy/2">>]}], "",
						     Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

erlang_server_openssl_client_npn_only_client(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_with_opts(Config, [], "-nextprotoneg spdy/2",
						     Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
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
	    ct:log("Cipher suite errors: ~p~n", [Error]),
	    ct:fail(cipher_suite_failed_see_test_case_log)
    end.

cipher(CipherSuite, Version, Config, ClientOpts, ServerOpts) ->
    process_flag(trap_exit, true),
    ct:log("Testing CipherSuite ~p~n", [CipherSuite]),
    {ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),

    Cmd = "openssl s_server -accept " ++ integer_to_list(Port)  ++  ssl_test_lib:version_flag(Version) ++
	" -cert " ++ CertFile ++ " -key " ++ KeyFile ++ "",

    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]),

    ssl_test_lib:wait_for_openssl_server(),

    ConnectionInfo = {ok, {Version, CipherSuite}},

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, cipher_result, [ConnectionInfo]}},
					{options,
					 [{ciphers,[CipherSuite]} |
			     ClientOpts]}]),

    true = port_command(OpenSslPort, "Hello\n"),

    receive
	{Port, {data, _}} when is_port(Port) ->
	    ok
    after 500 ->
	    ct:log("Time out on openssl port, check that"
			       " the messages Hello and world are received"
			       " during close of port" , []),
	    ok
    end,

    true = port_command(OpenSslPort, " world\n"),

    Result = ssl_test_lib:wait_for_result(Client, ok),

    %% Clean close down!   Server needs to be closed first !!
    close_port(OpenSslPort),
    ssl_test_lib:close(Client),

    Return = case Result of
		 ok ->
		     [];
		 Error ->
		     [{CipherSuite, Error}]
	     end,
    process_flag(trap_exit, false),
    Return.

start_erlang_client_and_openssl_server_with_opts(Config, ErlangClientOpts, OpensslServerOpts, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),
    ClientOpts0 = ?config(client_opts, Config),
    ClientOpts = ErlangClientOpts ++ ClientOpts0,

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Cmd = "openssl s_server " ++ OpensslServerOpts ++ "  -accept " ++ 
	integer_to_list(Port) ++  ssl_test_lib:version_flag(Version) ++
	" -cert " ++ CertFile  ++ " -key " ++ KeyFile,

    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]),

    ssl_test_lib:wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                    {host, Hostname},
                    {from, self()},
                    {mfa, {?MODULE,
                           erlang_ssl_receive, [Data]}},
                    {options, ClientOpts}]),

    Callback(Client, OpensslPort),

    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

start_erlang_client_and_openssl_server_for_npn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts = ?config(server_opts, Config),
    ClientOpts0 = ?config(client_opts, Config),
    ClientOpts = [{client_preferred_next_protocols, {client, [<<"spdy/2">>], <<"http/1.1">>}} | ClientOpts0],

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),

    Cmd = "openssl s_server -msg -nextprotoneg http/1.1,spdy/2  -accept " ++ integer_to_list(Port)  ++  ssl_test_lib:version_flag(Version) ++
    " -cert " ++ CertFile  ++ " -key " ++ KeyFile,

    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]),

    ssl_test_lib:wait_for_openssl_server(),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                    {host, Hostname},
                    {from, self()},
                    {mfa, {?MODULE,
                           erlang_ssl_receive_and_assert_npn, [<<"spdy/2">>, Data]}},
                    {options, ClientOpts}]),

    Callback(Client, OpensslPort),

    %% Clean close down!   Server needs to be closed first !!
    close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

start_erlang_server_and_openssl_client_for_npn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts0 = ?config(server_opts, Config),
    ServerOpts = [{next_protocols_advertised, [<<"spdy/2">>]}, ServerOpts0],

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),


    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, erlang_ssl_receive_and_assert_npn, [<<"spdy/2">>, Data]}},
                    {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_client -nextprotoneg http/1.0,spdy/2 -msg -port " ++ integer_to_list(Port)  ++ ssl_test_lib:version_flag(Version) ++
    " -host localhost",

    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]),

    Callback(Server, OpenSslPort),

    ssl_test_lib:close(Server),

    close_port(OpenSslPort),
    process_flag(trap_exit, false).


start_erlang_server_and_openssl_client_with_opts(Config, ErlangServerOpts, OpenSSLClientOpts, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts0 = ?config(server_opts, Config),
    ServerOpts = ErlangServerOpts ++  ServerOpts0,

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),


    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
                    {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_client " ++ OpenSSLClientOpts ++ " -msg -port " ++ integer_to_list(Port)  ++ ssl_test_lib:version_flag(Version) ++
    " -host localhost",

    ct:log("openssl cmd: ~p~n", [Cmd]),

    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]),

    Callback(Server, OpenSslPort),

    ssl_test_lib:close(Server),

    close_port(OpenSslPort),
    process_flag(trap_exit, false).


erlang_ssl_receive_and_assert_npn(Socket, Protocol, Data) ->
    {ok, Protocol} = ssl:negotiated_next_protocol(Socket),
    erlang_ssl_receive(Socket, Data),
    {ok, Protocol} = ssl:negotiated_next_protocol(Socket),
    ok.

erlang_ssl_receive(Socket, Data) ->
    ct:log("Connection info: ~p~n",
		       [ssl:connection_info(Socket)]),
    receive
	{ssl, Socket, Data} ->
	    io:format("Received ~p~n",[Data]),
	    %% open_ssl server sometimes hangs waiting in blocking read
	    ssl:send(Socket, "Got it"), 
	    ok;
	{ssl, Socket, Byte} when length(Byte) == 1 ->
	    erlang_ssl_receive(Socket, tl(Data));
	{Port, {data,Debug}} when is_port(Port) ->
	    io:format("openssl ~s~n",[Debug]),
	    erlang_ssl_receive(Socket,Data);
	Other ->
	    ct:fail({unexpected_message, Other})
    after 4000 ->
	    ct:fail({did_not_get, Data})
    end.
 
connection_info(Socket, Version) ->
    case ssl:connection_info(Socket) of
	{ok, {Version, _} = Info} ->
	    ct:log("Connection info: ~p~n", [Info]),
	    ok;
	{ok, {OtherVersion, _}} ->
	    {wrong_version, OtherVersion}
    end.

connection_info_result(Socket) ->                                            
    ssl:connection_info(Socket).


delayed_send(Socket, [ErlData, OpenSslData]) ->
    ct:sleep(?SLEEP),
    ssl:send(Socket, ErlData),
    erlang_ssl_receive(Socket, OpenSslData).

close_port(Port) ->
    catch port_command(Port, ?OPENSSL_QUIT),
    close_loop(Port, 500, false).

close_loop(Port, Time, SentClose) ->
    receive 
	{Port, {data,Debug}} when is_port(Port) ->
	    ct:log("openssl ~s~n",[Debug]),
	    close_loop(Port, Time, SentClose);	
	{ssl,_,Msg} ->
	    ct:log("ssl Msg ~s~n",[Msg]),
	    close_loop(Port, Time, SentClose);	
	{Port, closed} -> 
	    ct:log("Port Closed~n",[]),
	    ok;
	{'EXIT', Port, Reason} ->
	    ct:log("Port Closed ~p~n",[Reason]),
	    ok;
	Msg ->
	    ct:log("Port Msg ~p~n",[Msg]),
	    close_loop(Port, Time, SentClose)
    after Time ->
	    case SentClose of
		false -> 
		    ct:log("Closing port ~n",[]),
		    catch erlang:port_close(Port),
		    close_loop(Port, Time, true);
		true ->
		    ct:log("Timeout~n",[])
	    end
    end.


server_sent_garbage(Socket) ->
    receive 
	server_sent_garbage ->
	    {error, closed} == ssl:send(Socket, "data")
	    
    end.
    

check_openssl_npn_support(Config) ->
    HelpText = os:cmd("openssl s_client --help"),
    case string:str(HelpText, "nextprotoneg") of
        0 ->
            {skip, "Openssl not compiled with nextprotoneg support"};
        _ ->
            Config
    end.

check_sane_openssl_renegotaite(Config) ->
    case os:cmd("openssl version") of
	"OpenSSL 0.9.8" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	"OpenSSL 0.9.7" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	_ ->
	    Config
    end.

check_sane_openssl_sslv2(Config) ->
    Port = open_port({spawn, "openssl s_client  -ssl2 "}, [stderr_to_stdout]),
    case supports_sslv2(Port) of
	true ->
	    Config;
	false ->
	    {skip, "sslv2 not supported by openssl"}
    end.

supports_sslv2(Port) ->
    receive 
	{Port, {data, "unknown option -ssl2" ++ _}} -> 
	    false;
	{Port, {data, Data}} ->
	    case lists:member("error", string:tokens(Data, ":")) of
		true ->
		    false;
		false ->
		    supports_sslv2(Port)
	    end
    after 500 ->
	    true
    end.

