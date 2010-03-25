%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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

-module(ssl_basic_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").
-include("test_server_line.hrl").

-define('24H_in_sec', 86400).  
-define(TIMEOUT, 60000).
-define(EXPIRE, 10).
-define(SLEEP, 500).

-behaviour(ssl_session_cache_api).

%% For the session cache tests
-export([init/0, terminate/1, lookup/2, update/3, 
	 delete/2, foldl/3, select_session/2]).

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
init_per_suite(Config) ->
    crypto:start(),
    ssl:start(),
    Result = 
	(catch make_certs:all(?config(data_dir, Config), 
			      ?config(priv_dir, Config))),
    test_server:format("Make certs  ~p~n", [Result]),
    ssl_test_lib:cert_options(Config).

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ssl:stop(),
    crypto:stop().

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
init_per_testcase(session_cache_process_list, Config) ->
    init_customized_session_cache(Config);

init_per_testcase(session_cache_process_mnesia, Config) ->
    mnesia:start(),
    init_customized_session_cache(Config);

init_per_testcase(reuse_session_expired, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ssl_test_lib:timetrap(?EXPIRE * 1000 * 5),
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_lifetime, ?EXPIRE),
    ssl:start(),
    [{watchdog, Dog} | Config];

init_per_testcase(_TestCase, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = test_server:timetrap(?TIMEOUT),
    [{watchdog, Dog} | Config].

init_customized_session_cache(Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = test_server:timetrap(?TIMEOUT),
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_cb, ?MODULE),
    ssl:start(),
    [{watchdog, Dog} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(session_cache_process_list, Config) ->
    application:unset_env(ssl, session_cb),
    end_per_testcase(default_action, Config);
end_per_testcase(session_cache_process_mnesia, Config) ->
    application:unset_env(ssl, session_cb),
    mnesia:stop(),
    end_per_testcase(default_action, Config);
end_per_testcase(reuse_session_expired, Config) ->
    application:unset_env(ssl, session_lifetime),
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
all(doc) -> 
    ["Test the basic ssl functionality"];

all(suite) -> 
    [app, connection_info, controlling_process, controller_dies, 
     peercert, connect_dist,
     peername, sockname, socket_options, versions, cipher_suites,
     upgrade, upgrade_with_timeout, tcp_connect,
     ipv6, ekeyfile, ecertfile, ecacertfile, eoptions, shutdown,
     shutdown_write, shutdown_both, shutdown_error, ciphers, 
     send_close, 
     server_verify_peer_passive,
     server_verify_peer_active, server_verify_peer_active_once,
     server_verify_none_passive, server_verify_none_active, 
     server_verify_none_active_once,
     server_verify_no_cacerts, client_verify_none_passive, 
     client_verify_none_active, client_verify_none_active_once
     %%, session_cache_process_list, session_cache_process_mnesia
     ,reuse_session, reuse_session_expired, server_does_not_want_to_reuse_session,
     client_renegotiate, server_renegotiate,
     client_no_wrap_sequence_number, server_no_wrap_sequence_number
    ].

%% Test cases starts here.
%%--------------------------------------------------------------------
app(doc) ->
    "Test that the ssl app file is ok";
app(suite) ->
    [];
app(Config) when is_list(Config) ->
    ok = test_server:app_test(ssl).

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
    
    ServerMsg = ClientMsg = {ok, {Version, {rsa,rc4_128,sha,no_export}}},
			   
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

connection_info_result(Socket) ->                                            
    ssl:connection_info(Socket).

%%--------------------------------------------------------------------

controlling_process(doc) -> 
    ["Test API function controlling_process/2"];

controlling_process(suite) -> 
    [];

controlling_process(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientMsg = "Hello server",
    ServerMsg = "Hello client",
   
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
	{ssl, _, ServerMsg} ->
	    receive 
		{ssl, _, ClientMsg} ->
		    ok
	    end;
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
    Server ! listen, test_server:sleep(?SLEEP),
    Tester = self(),
    Connect = fun(Pid) ->
		      {ok, Socket} = ssl:connect(Hostname, Port, 
						  [{reuseaddr,true},{ssl_imp,new}]),
		      Pid ! {self(), connected, Socket},
		      receive die_nice -> normal end
	      end,
    Client2 = spawn_link(fun() -> Connect(Tester) end),
    receive {Client2, connected, _Socket} ->  Client2 ! die_nice end,
    
    get_close(Client2, ?LINE),
    
    %% Test that clients die when the controlling process have changed 
    Server ! listen, test_server:sleep(?SLEEP),

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
    Server ! listen, test_server:sleep(?SLEEP),
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
    {ok, [{cert, BinCert, _}]} = public_key:pem_to_der(CertFile),
    {ok, ErlCert} = public_key:pkix_decode_cert(BinCert, otp),
       
    ServerMsg = {{error, no_peercert}, {error, no_peercert}},
    ClientMsg = {{ok, BinCert}, {ok, ErlCert}},
    
    test_server:format("Testcase ~p, Client ~p  Server ~p ~n", 
		       [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

peercert_result(Socket) ->
    Result1 = ssl:peercert(Socket),
    Result2 = ssl:peercert(Socket, [ssl]), 
    {Result1, Result2}.

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
    MandatoryCipherSuite = {rsa,'3des_ede_cbc',sha,no_export},
    [_|_] = Suites = ssl:cipher_suites(),
    true = lists:member(MandatoryCipherSuite, Suites).
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
    ssl_test_lib:close(Client).

socket_options_result(Socket, Options, DefaultValues, NewOptions, NewValues) ->
    %% Test get/set emulated opts
    {ok, DefaultValues} = ssl:getopts(Socket, Options), 
    ssl:setopts(Socket, NewValues),
    {ok, NewValues} = ssl:getopts(Socket, NewOptions),
    %% Test get/set inet opts
    {ok,[{nodelay,false}]} = ssl:getopts(Socket, [nodelay]),  
    ok.
    
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
    {error, _} = ssl:send(SslS, "Hello world"),    
    ssl_test_lib:close(Server).

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
				   {tcp_options, TcpOpts},
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
    ok = ssl:send(Socket, "Hello world"),
    %% Make sure binary is inherited from tcp socket and that we do
    %% not get the list default!
    receive 
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
						{tcp_options, TcpOpts},
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
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server([{node, ServerNode}, {port, 0},
						{from, self()},
						{timeout, 5000},
						{mfa, {?MODULE, should_close, []}},
						{tcp_options, TcpOpts},
						{ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {packet, 0}]),
    test_server:format("Testcase ~p connected to Server ~p ~n", [self(), Server]),
    gen_tcp:send(Socket, "<SOME GARBLED NON SSL MESSAGE>"),

    ssl_test_lib:check_result(Server, {error,esslerrssl}, tcp_closed, Socket),

    ssl_test_lib:close(Server).


should_close(Socket) ->
    receive
	{ssl, Socket, closed} ->
	    server_closed;
	Other ->
	    exit({?LINE, Other})
    end.

%%--------------------------------------------------------------------
ipv6(doc) ->
    ["Test ipv6."];
ipv6(suite) ->
    [];
ipv6(Config) when is_list(Config) ->
    {ok, Hostname0} = inet:gethostname(),
    
    case lists:member(list_to_atom(Hostname0), ?config(ipv6_hosts, Config)) of
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
    Port = ssl_test_lib:inet_port(ServerNode),
  
    Server = 
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, BadOpts}]),
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
    Port = ssl_test_lib:inet_port(ServerNode),
    
    Server0 = 
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, ServerBadOpts}]),
    Client0 =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
			    {port, Port}, {host, Hostname},
					 {from, self()}, 
					 {options, ClientOpts}]),
    
    ssl_test_lib:check_result(Server0, {error, ecertfile}, Client0, 
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
    Port = ssl_test_lib:inet_port(ServerNode),
    
    Server0  = 
	ssl_test_lib:start_server_error([{node, ServerNode}, 
					 {port, Port}, {from, self()},
					 {options, ServerBadOpts}]),
    Client0 =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
					 {port, Port}, {host, Hostname},
					 {from, self()}, 
					 {options, ClientOpts}]),
    
    ssl_test_lib:check_result(Server0, {error, ecacertfile},
			      Client0, {error, closed}),
    
    File0 = proplists:get_value(cacertfile, ServerBadOpts),
    File = File0 ++ "do_not_exit.pem",
    ServerBadOpts1 = [{cacertfile, File}|proplists:delete(cacertfile, ServerBadOpts)],
            
    Server1  = 
	ssl_test_lib:start_server_error([{node, ServerNode}, 
					 {port, Port}, {from, self()},
					 {options, ServerBadOpts1}]),
    Client1 =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
					 {port, Port}, {host, Hostname},
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
    Port = ssl_test_lib:inet_port(ServerNode),

    %% Emulated opts
    Server0 =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port},
					 {from, self()},
			    {options, [{active, trice} | ServerOpts]}]),
    Client0 =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
			    {port, Port}, {host, Hostname},
			    {from, self()}, 
			    {options, [{active, trice} | ClientOpts]}]),    
    ssl_test_lib:check_result(Server0, {error, {eoptions, {active,trice}}}, 
		    Client0, {error, {eoptions, {active,trice}}}),

    test_server:sleep(?SLEEP),
    
    Server1 =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, [{header, a} | ServerOpts]}]),
    Client1 = 
	ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
			    {host, Hostname},
			    {from, self()},  
			    {options, [{header, a} | ClientOpts]}]), 
    ssl_test_lib:check_result(Server1, {error, {eoptions, {header, a}}}, 
		    Client1, {error, {eoptions, {header, a}}}),

    test_server:sleep(?SLEEP),

    
    Server2 =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port},
					 {from, self()},
			    {options, [{mode, a} | ServerOpts]}]),
    
    Client2 =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
			    {port, Port}, {host, Hostname},
			    {from, self()}, 
			    {options, [{mode, a} | ClientOpts]}]),    
    ssl_test_lib:check_result(Server2, {error, {eoptions, {mode, a}}}, 
		    Client2, {error, {eoptions, {mode, a}}}),


    test_server:sleep(?SLEEP),

    Server3 =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, [{packet, 8.0} | ServerOpts]}]),
    Client3 =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
			    {port, Port}, {host, Hostname},
			    {from, self()}, 
			    {options, [{packet, 8.0} | ClientOpts]}]),
    ssl_test_lib:check_result(Server3, {error, {eoptions, {packet, 8.0}}}, 
			      Client3, {error, {eoptions, {packet, 8.0}}}),
   
    test_server:sleep(?SLEEP),

    %% ssl  
    Server4 =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, [{verify, 4} | ServerOpts]}]),
    Client4 =
	ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
			    {host, Hostname},
			    {from, self()}, 
			    {options, [{verify, 4} | ClientOpts]}]),
    ssl_test_lib:check_result(Server4, {error, {eoptions, {verify, 4}}}, 
		    Client4, {error, {eoptions, {verify, 4}}}),

    test_server:sleep(?SLEEP),
   
    Server5 =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, [{depth, four} | ServerOpts]}]),
    Client5 =
	ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
			    {host, Hostname},
			    {from, self()}, 
			    {options, [{depth, four} | ClientOpts]}]),
    ssl_test_lib:check_result(Server5, {error, {eoptions, {depth, four}}}, 
		    Client5, {error, {eoptions, {depth, four}}}),

    test_server:sleep(?SLEEP),
    
    Server6 =  
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, [{cacertfile, ""} | ServerOpts]}]),
    Client6 =
	ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
			    {host, Hostname},
			    {from, self()}, 
			    {options, [{cacertfile, ""} | ClientOpts]}]),   
    ssl_test_lib:check_result(Server6, {error, {eoptions, {cacertfile, ""}}}, 
		    Client6, {error, {eoptions, {cacertfile, ""}}}),


    test_server:sleep(?SLEEP),
    
    Server7 =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, [{certfile, 'cert.pem'} | ServerOpts]}]),
    Client7 =
	ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
			    {host, Hostname},
			    {from, self()}, 
			    {options, [{certfile, 'cert.pem'} | ClientOpts]}]),
    ssl_test_lib:check_result(Server7, 
			      {error, {eoptions, {certfile, 'cert.pem'}}}, 
		    Client7, {error, {eoptions, {certfile, 'cert.pem'}}}),
    
    test_server:sleep(?SLEEP),

    Server8 =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, [{keyfile,'key.pem' } | ServerOpts]}]),
    Client8 =
	ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
			    {host, Hostname},
			    {from, self()}, {options, [{keyfile, 'key.pem'} 
						       | ClientOpts]}]),
    ssl_test_lib:check_result(Server8, 
			      {error, {eoptions, {keyfile, 'key.pem'}}}, 
		    Client8, {error, {eoptions, {keyfile, 'key.pem'}}}),

    test_server:sleep(?SLEEP),
    
    Server9 =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, [{key, 'key.pem' } | ServerOpts]}]),
    Client9 =
	ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
			    {host, Hostname},
			    {from, self()}, {options, [{key, 'key.pem'} 
						       | ClientOpts]}]),
    ssl_test_lib:check_result(Server9, {error, {eoptions, {key, 'key.pem'}}}, 
		    Client9, {error, {eoptions, {key, 'key.pem'}}}),
    

    test_server:sleep(?SLEEP),

    Server10 =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, [{password, foo} | ServerOpts]}]),
    Client10 = 
	ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
			    {host, Hostname},
			    {from, self()}, 
			    {options, [{password, foo} | ClientOpts]}]),
    ssl_test_lib:check_result(Server10, {error, {eoptions, {password, foo}}}, 
		    Client10, {error, {eoptions, {password, foo}}}),
    
    test_server:sleep(?SLEEP),
    
    %% Misc
    Server11 =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
			    {options, [{ssl_imp, cool} | ServerOpts]}]),
    Client11 =
	ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
			    {host, Hostname},
			    {from, self()}, 
			    {options, [{ssl_imp, cool} | ClientOpts]}]),
    ssl_test_lib:check_result(Server11, {error, {eoptions, {ssl_imp, cool}}}, 
		    Client11, {error, {eoptions, {ssl_imp, cool}}}),


    test_server:sleep(?SLEEP),
    
    Server12 = 
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, Port}, 
					 {from, self()},
		      {options, [{debug, cool} | ServerOpts]}]),
    Client12 =
	ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
			    {host, Hostname},
			    {from, self()}, 
			    {options, [{debug, cool} | ClientOpts]}]),
    ssl_test_lib:check_result(Server12, {error, {eoptions, {debug, cool}}}, 
		    Client12, {error, {eoptions, {debug, cool}}}).

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

%%--------------------------------------------------------------------
ciphers(doc) -> 
    [""];
       
ciphers(suite) -> 
    [];

ciphers(Config) when is_list(Config) ->
    Version = 
	ssl_record:protocol_version(ssl_record:highest_protocol_version([])),

    Ciphers = ssl:cipher_suites(),
    Result =  lists:map(fun(Cipher) -> 
				cipher(Cipher, Version, Config) end,
			Ciphers),
    case lists:flatten(Result) of
	[] ->
	    ok;
	Error ->
	    test_server:format("Cipher suite errors: ~p~n", [Error]),
	    test_server:fail(cipher_suite_failed_see_test_case_log) 
    end.
    
cipher(CipherSuite, Version, Config) ->   
    process_flag(trap_exit, true),
    test_server:format("Testing CipherSuite ~p~n", [CipherSuite]),
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
			    [{ciphers,[CipherSuite]} | 
			     ClientOpts]}]), 
   
    ServerMsg = ClientMsg = {ok, {Version, CipherSuite}},
			   
    Result = ssl_test_lib:wait_for_result(Server, ServerMsg, 
					  Client, ClientMsg),    
    ssl_test_lib:close(Server),
    receive 
	{'EXIT', Server, normal} ->
	    ok
     end,
    ssl_test_lib:close(Client),
    receive 
	{'EXIT', Client, normal} ->
	    ok
    end,
    process_flag(trap_exit, false),
    case Result of
	ok ->
	    [];
	Error ->
	    [{CipherSuite, Error}]
    end.

%%--------------------------------------------------------------------
reuse_session(doc) -> 
    ["Test reuse of sessions (short handshake)"];

reuse_session(suite) -> 
    [];

reuse_session(Config) when is_list(Config) -> 
    process_flag(trap_exit, true),
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()},
		      {mfa, {?MODULE, session_info_result, []}},
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
       
    Server ! listen,
    
    %% Make sure session is registered
    test_server:sleep(?SLEEP),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
		      {mfa, {?MODULE, session_info_result, []}},
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
    
    Client2 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
			    {mfa, {?MODULE, session_info_result, []}},
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
    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Client2),


    Server1 = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()},
		      {mfa, {?MODULE, session_info_result, []}},
		      {options, [{reuse_sessions, false} | ServerOpts]}]),
    
    Port1 = ssl_test_lib:inet_port(Server1),
    Client3 =
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port1}, {host, Hostname},
				   {mfa, {?MODULE, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]), 

    SessionInfo1 = 
	receive
	    {Server1, Info1} ->
		Info1
	end,
       
    Server1 ! listen,
    
    %% Make sure session is registered
    test_server:sleep(?SLEEP),

    Client4 = 
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port1}, {host, Hostname},
				   {mfa, {?MODULE, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]),
    
    receive
	{Client4, SessionInfo1} ->
	    test_server:fail(
	      session_reused_when_session_reuse_disabled_by_server);
	{Client4, _Other} ->
	    ok
    end,
    
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client3),
    ssl_test_lib:close(Client4),
    process_flag(trap_exit, false).


session_info_result(Socket) ->                                            
    ssl:session_info(Socket).

%%--------------------------------------------------------------------
reuse_session_expired(doc) -> 
    ["Test sessions is not reused when it has expired"];

reuse_session_expired(suite) -> 
    [];

reuse_session_expired(Config) when is_list(Config) -> 
    process_flag(trap_exit, true),
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()},
		      {mfa, {?MODULE, session_info_result, []}},
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
       
    Server ! listen,
    
    %% Make sure session is registered
    test_server:sleep(?SLEEP),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
		      {mfa, {?MODULE, session_info_result, []}},
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
    test_server:sleep((?EXPIRE+1) * 1000),
    
    Client2 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
				   {mfa, {?MODULE, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]),   
    receive
	{Client2, SessionInfo} ->
	    test_server:fail(session_reused_when_session_expired);
	{Client2, _} ->
	    ok
    end,
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Client2),
    process_flag(trap_exit, false).
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
		      {mfa, {?MODULE, session_info_result, []}},
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
       
    Server ! listen,
    
    %% Make sure session is registered
    test_server:sleep(?SLEEP),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
		      {mfa, {?MODULE, session_info_result, []}},
		      {from, self()},  {options, ClientOpts}]),    
    receive
	{Client1, SessionInfo} ->
	    test_server:fail(session_reused_when_server_does_not_want_to);
	{Client1, _Other} ->
	   ok
    end,
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Client1),
    process_flag(trap_exit, false).

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

server_verify_no_cacerts(doc) -> 
    ["Test server must have cacerts if it wants to verify client"];

server_verify_no_cacerts(suite) -> 
    [];

server_verify_no_cacerts(Config) when is_list(Config) ->
    ServerOpts =  ServerOpts =  ?config(server_opts, Config), 
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
					      {from, self()}, 
					      {options, [{verify, verify_peer} 
							 | ServerOpts]}]),
        
    ssl_test_lib:check_result(Server, {error, {eoptions, {cacertfile, ""}}}).
    
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
    %% TODO: send message to test process to make sure
    %% verifyfun has beeen run as it has the same behavior as
    %% the default fun
    VerifyFun =  fun([{bad_cert, unknown_ca}]) ->
 			 true;
 		    (_) ->
 			 false
 		 end,
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       send_recv_result_active_once, 
					       []}},
					{options, [{active, once}, 
						   {verify, verify_none},
						   {verify_fun, VerifyFun}
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
    process_flag(trap_exit, true),
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
 
    test_server:sleep(?SLEEP),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       renegotiate, [Data]}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok), 

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------
server_renegotiate(doc) -> 
    ["Test ssl:renegotiate/1 on server."];

server_renegotiate(suite) -> 
    [];

server_renegotiate(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
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
    
    test_server:sleep(?SLEEP),
   
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, erlang_ssl_receive, [Data]}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ok.

%%--------------------------------------------------------------------
client_no_wrap_sequence_number(doc) -> 
    ["Test that erlang client will renegotiate session when",  
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at" 
     " to lower treashold substantially."];

client_no_wrap_sequence_number(suite) -> 
    [];

client_no_wrap_sequence_number(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
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
 
    test_server:sleep(?SLEEP),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {ssl_test_lib, 
					       trigger_renegotiate, [[ErlData, N+2]]}},
					{options, [{reuse_sessions, false},
						   {renegotiate_at, N} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok), 

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------
server_no_wrap_sequence_number(doc) -> 
    ["Test that erlang server will renegotiate session when",  
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at" 
     " to lower treashold substantially."];

server_no_wrap_sequence_number(suite) -> 
    [];

server_no_wrap_sequence_number(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
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
    
    test_server:sleep(?SLEEP),
   
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {ssl_test_lib, no_result, []}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
send_recv_result(Socket) ->
    ssl:send(Socket, "Hello world"),
    test_server:sleep(?SLEEP),
    {ok,"Hello world"} = ssl:recv(Socket, 11),
    ok.

send_recv_result_active(Socket) ->
    ssl:send(Socket, "Hello world"),
    test_server:sleep(?SLEEP),
    receive 
	{ssl, Socket, "Hello world"} ->
	    ok
    end.

send_recv_result_active_once(Socket) ->
    ssl:send(Socket, "Hello world"),
    test_server:sleep(?SLEEP),
    receive 
	{ssl, Socket, "Hello world"} ->
	    ok
    end.


renegotiate(Socket, Data) ->
    [{session_id, Id} | _ ] = ssl:session_info(Socket),
    ssl:renegotiate(Socket),
    ssl:send(Socket, Data),
    test_server:sleep(1000),
    case  ssl:session_info(Socket) of
	 [{session_id, Id} | _ ] ->
	    fail_session_not_renegotiated;
	_ ->
	    ok
    end.
 

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

session_cache_process(Type,Config) when is_list(Config) -> 
    process_flag(trap_exit, true),
    setup_session_cb(Type),

    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()},
				   {mfa, {?MODULE, session_info_result, []}},
				   {options, 
				    [{session_cache_cb, ?MODULE}| 
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

    Server ! listen,
    
    %% Make sure session is registered
    test_server:sleep(?SLEEP),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port}, {host, Hostname},
				   {mfa, {?MODULE, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]),   
    receive
	{Client1, SessionInfo} ->
	    ok;
	{Client1, Other} ->
	    test_server:format("Expected: ~p,  Unexpected: ~p~n", 
			       [SessionInfo, Other]),
	    test_server:fail(session_not_reused)
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Client1),

    Server1 = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()},
				   {mfa, {?MODULE, session_info_result, []}},
				   {options, 
				    [{reuse_sessions, false} | ServerOpts]}]),
    Port1 = ssl_test_lib:inet_port(Server1),

    Client3 =
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port1}, {host, Hostname},
				   {mfa, {?MODULE, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]), 

    SessionInfo1 = 
	receive
	    {Server1, Info1} ->
		Info1
	end,

    Server1 ! listen,

    %% Make sure session is registered
    test_server:sleep(?SLEEP),

    Client4 = 
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port1}, {host, Hostname},
				   {mfa, {?MODULE, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]),

    receive
	{Client4, SessionInfo1} ->
	    test_server:fail(
	      session_reused_when_session_reuse_disabled_by_server);
	{Client4, _Other} ->
	    ok
    end,

    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client3),
    ssl_test_lib:close(Client4),
    process_flag(trap_exit, false).

setup_session_cb(Type) ->
    ssl_test = ets:new(ssl_test,[named_table, set,public]),
    ets:insert(ssl_test, {type,Type}).

session_cb() ->
    [{type,Type}] = ets:lookup(ssl_test, type),
    Type.

init() ->
    io:format("~p~n",[?LINE]),
    case session_cb() of
	list ->
	    spawn(fun() -> session_loop([]) end);
	mnesia ->
	    mnesia:start(),
	    {atomic,ok} = mnesia:create_table(sess_cache, [])
    end.

terminate(Cache) ->
    io:format("~p~n",[?LINE]),
    case session_cb() of
	list ->
	    Cache ! terminate;
	mnesia ->
	    {atomic,ok} = mnesia:delete_table(sess_cache, [])
    end.

lookup(Cache, Key) ->
    io:format("~p~n",[?LINE]),
    case session_cb() of
	list ->
	    Cache ! {self(), lookup, Key},
	    receive {Cache, Res} -> Res end;
	mnesia ->
	    case mnesia:transaction(fun() -> 
					    mnesia:read(sess_cache, 
							Key, read) 
				    end) of
		{atomic, [Session]} -> Session;
		_ -> undefined
	    end
    end.

update(Cache, Key, Value) ->
    io:format("~p~n",[?LINE]),
    case session_cb() of
	list ->
	    Cache ! {update, Key, Value};
	mnesia ->
	    {atomic, ok} = 
		mnesia:transaction(fun() -> 
					   mnesia:write(sess_cache, 
							Key, Value) 
				   end)
    end.

delete(Cache, Key) -> 
    io:format("~p~n",[?LINE]),
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
    io:format("~p~n",[?LINE]),
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
    io:format("~p~n",[?LINE]),
    case session_cb() of
	list ->
	    Cache ! {self(),select_session, PartialKey},
	    receive {Cache, Res} -> Res end;
	mnesia ->
	    Sel = fun() ->
			  mnesia:select(Cache,
					[{{{PartialKey,'$1'}, '$2'},
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
	    session_loop([{Key,Value}|Sess]);
	{delete, Key} ->
	    session_loop(lists:keydelete(Key,1,Sess));
	{Pid,foldl,Fun,Acc} ->
	    Res = lists:foldl(Fun, Acc,Sess),
	    Pid ! {self(), Res},
	    session_loop(Sess);
	{Pid,select_session,PKey} ->
	    Sel = fun({{Head, _},Session}, Acc) when Head =:= PKey -> 
			  [Session|Acc];
		     (_,Acc) -> 
			  Acc
		  end,
	    Pid ! {self(), lists:foldl(Sel, [], Sess)},
	    session_loop(Sess)
    end.
	    
erlang_ssl_receive(Socket, Data) ->
    receive
	{ssl, Socket, Data} ->
	    io:format("Received ~p~n",[Data]),
	    ok;
	Other ->
	    test_server:fail({unexpected_message, Other})
    after 4000 ->
	    test_server:fail({did_not_get, Data})
    end.
 
