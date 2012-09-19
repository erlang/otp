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
-module(ssl_test_lib).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-record(sslsocket, { fd = nil, pid = nil}).

%% For now always run locally
run_where(_) ->
    ClientNode = node(),
    ServerNode = node(),
    {ok, Host} = rpc:call(ServerNode, inet, gethostname, []),
    {ClientNode, ServerNode, Host}.

run_where(_, ipv6) ->
    ClientNode = node(),
    ServerNode = node(),
    {ok, Host} = rpc:call(ServerNode, inet, gethostname, []),
    {ClientNode, ServerNode, Host}.

node_to_hostip(Node) ->
    [_ , Host] = string:tokens(atom_to_list(Node), "@"),
    {ok, Address} = inet:getaddr(Host, inet),
    Address.

start_server(Args) ->
    Result = spawn_link(?MODULE, run_server, [Args]),
    receive
	{listen, up} ->
	    Result
    end.

run_server(Opts) ->
    Node = proplists:get_value(node, Opts),
    Port = proplists:get_value(port, Opts),
    Options = proplists:get_value(options, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport =  proplists:get_value(transport, Opts, ssl),
    ct:print("ssl:listen(~p, ~p)~n", [Port, Options]),
    {ok, ListenSocket} = rpc:call(Node, Transport, listen, [Port, Options]),
    Pid ! {listen, up},
    send_selected_port(Pid, Port, ListenSocket),
    run_server(ListenSocket, Opts).

run_server(ListenSocket, Opts) ->
    do_run_server(ListenSocket, connect(ListenSocket, Opts), Opts).

do_run_server(_, {error, timeout} = Result, Opts)  ->
    Pid = proplists:get_value(from, Opts),
    Pid ! {self(), Result};

do_run_server(ListenSocket, AcceptSocket, Opts) ->
    Node = proplists:get_value(node, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport = proplists:get_value(transport, Opts, ssl),
    {Module, Function, Args} = proplists:get_value(mfa, Opts),
    ct:print("Server: apply(~p,~p,~p)~n",
		       [Module, Function, [AcceptSocket | Args]]),
    case rpc:call(Node, Module, Function, [AcceptSocket | Args]) of
	no_result_msg ->
	    ok;
	Msg ->
	    ct:print("Server Msg: ~p ~n", [Msg]),
	    Pid ! {self(), Msg}
    end,
    receive
	listen ->
	    run_server(ListenSocket, Opts);
	{listen, MFA} ->
	    run_server(ListenSocket, [MFA | proplists:delete(mfa, Opts)]);
	close ->
	    ct:print("Server closing  ~p ~n", [self()]),
	    Result = rpc:call(Node, Transport, close, [AcceptSocket], 500),
	    Result1 = rpc:call(Node, Transport, close, [ListenSocket], 500),
	    ct:print("Result ~p : ~p ~n", [Result, Result1]);
	{ssl_closed, _} ->
	    ok
    end.

%%% To enable to test with s_client -reconnect
connect(#sslsocket{} = ListenSocket, Opts) ->
    Node = proplists:get_value(node, Opts),
    ReconnectTimes =  proplists:get_value(reconnect_times, Opts, 0),
    Timeout = proplists:get_value(timeout, Opts, infinity),
    AcceptSocket = connect(ListenSocket, Node, 1 + ReconnectTimes, dummy, Timeout),
    case ReconnectTimes of
	0 ->
	    AcceptSocket;
	_ ->
	  remove_close_msg(ReconnectTimes),
	  AcceptSocket
    end;
connect(ListenSocket, Opts) ->
    Node = proplists:get_value(node, Opts),
    ct:print("gen_tcp:accept(~p)~n", [ListenSocket]),
    {ok, AcceptSocket} = rpc:call(Node, gen_tcp, accept, 
				  [ListenSocket]),
    AcceptSocket.

connect(_, _, 0, AcceptSocket, _) ->
    AcceptSocket;
connect(ListenSocket, Node, N, _, Timeout) ->
    ct:print("ssl:transport_accept(~p)~n", [ListenSocket]),
    {ok, AcceptSocket} = rpc:call(Node, ssl, transport_accept, 
				  [ListenSocket]),    
    ct:print("ssl:ssl_accept(~p, ~p)~n", [AcceptSocket, Timeout]),

    case rpc:call(Node, ssl, ssl_accept, [AcceptSocket, Timeout]) of
	ok ->
	    connect(ListenSocket, Node, N-1, AcceptSocket, Timeout);
	Result ->
	    Result
    end.

  
remove_close_msg(0) ->
    ok;
remove_close_msg(ReconnectTimes) ->
    receive
	{ssl_closed, _} ->
	   remove_close_msg(ReconnectTimes -1)
    end.
	    
start_client(Args) ->
    Result = spawn_link(?MODULE, run_client, [lists:delete(return_socket, Args)]),
    receive 
	{ connected, Socket } ->
        case lists:member(return_socket, Args) of
            true -> { Result, Socket };
            false -> Result
        end
    end.

run_client(Opts) ->
    Node = proplists:get_value(node, Opts),
    Host = proplists:get_value(host, Opts),
    Port = proplists:get_value(port, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport =  proplists:get_value(transport, Opts, ssl),
    Options = proplists:get_value(options, Opts),
    ct:print("ssl:connect(~p, ~p, ~p)~n", [Host, Port, Options]),
    case rpc:call(Node, Transport, connect, [Host, Port, Options]) of
	{ok, Socket} ->
	    Pid ! { connected, Socket },
	    ct:print("Client: connected~n", []),
	    %% In special cases we want to know the client port, it will
	    %% be indicated by sending {port, 0} in options list!
	    send_selected_port(Pid,  proplists:get_value(port, Options), Socket),
	    {Module, Function, Args} = proplists:get_value(mfa, Opts),
	    ct:print("Client: apply(~p,~p,~p)~n",
			       [Module, Function, [Socket | Args]]),
	    case rpc:call(Node, Module, Function, [Socket | Args]) of
		no_result_msg ->
		    ok;
		Msg ->
		    ct:print("Client Msg: ~p ~n", [Msg]),
		    Pid ! {self(), Msg}
	    end,
	    receive
		close ->
		    ct:print("Client closing~n", []),
		    rpc:call(Node, Transport, close, [Socket]);
		{ssl_closed, Socket} ->
		    ok;
		{gen_tcp, closed} ->
		    ok
	    end;
	{error, Reason} ->
	    ct:print("Client: connection failed: ~p ~n", [Reason]),
	       Pid ! {self(), {error, Reason}}
    end.

close(Pid) ->
    ct:print("Close ~p ~n", [Pid]),
    Monitor = erlang:monitor(process, Pid),
    Pid ! close,
    receive
	{'DOWN', Monitor, process, Pid, Reason} ->
	    erlang:demonitor(Monitor),
	    ct:print("Pid: ~p down due to:~p ~n", [Pid, Reason])
    end.

check_result(Server, ServerMsg, Client, ClientMsg) -> 
    receive 
	{Server, ServerMsg} -> 
	    receive 
		{Client, ClientMsg} ->
		    ok;
		Unexpected ->
		    Reason = {{expected, {Client, ClientMsg}}, 
			      {got, Unexpected}},
		    ct:fail(Reason)
	    end;
	{Client, ClientMsg} -> 
	    receive 
		{Server, ServerMsg} ->
		    ok;
		Unexpected ->
		    Reason = {{expected, {Server, ClientMsg}}, 
			      {got, Unexpected}},
		    ct:fail(Reason)
	    end;
	{Port, {data,Debug}} when is_port(Port) ->
	    io:format("openssl ~s~n",[Debug]),
	    check_result(Server, ServerMsg, Client, ClientMsg);

	Unexpected ->
	    Reason = {{expected, {Client, ClientMsg}},
		      {expected, {Server, ServerMsg}}, {got, Unexpected}},
	    ct:fail(Reason)
    end.

check_result(Pid, Msg) -> 
    receive 
	{Pid, Msg} -> 
	    ok;
	{Port, {data,Debug}} when is_port(Port) ->
	    io:format("openssl ~s~n",[Debug]),
	    check_result(Pid,Msg);
	Unexpected ->
	    Reason = {{expected, {Pid, Msg}}, 
		      {got, Unexpected}},
	    ct:fail(Reason)
    end.

wait_for_result(Server, ServerMsg, Client, ClientMsg) -> 
    receive 
	{Server, ServerMsg} -> 
	    receive 
		{Client, ClientMsg} ->
		    ok
		%% Unexpected ->
		%%     Unexpected
	    end;
	{Client, ClientMsg} -> 
	    receive 
		{Server, ServerMsg} ->
		    ok
		%% Unexpected ->
		%%     Unexpected
	    end;
	{Port, {data,Debug}} when is_port(Port) ->
	    io:format("openssl ~s~n",[Debug]),
	    wait_for_result(Server, ServerMsg, Client, ClientMsg)
	%% Unexpected ->
	%%     Unexpected
    end.


wait_for_result(Pid, Msg) -> 
    receive 
	{Pid, Msg} -> 
	    ok;
	{Port, {data,Debug}} when is_port(Port) ->
	    io:format("openssl ~s~n",[Debug]),
	    wait_for_result(Pid,Msg)
	%% Unexpected ->
	%%     Unexpected
    end.

user_lookup(psk, _Identity, UserState) ->
    {ok, UserState};
user_lookup(srp, Username, _UserState) ->
    Salt = ssl:random_bytes(16),
    UserPassHash = crypto:sha([Salt, crypto:sha([Username, <<$:>>, <<"secret">>])]),
    {ok, {srp_1024, Salt, UserPassHash}}.

cert_options(Config) ->
    ClientCaCertFile = filename:join([?config(priv_dir, Config), 
				      "client", "cacerts.pem"]),
    ClientCertFile = filename:join([?config(priv_dir, Config), 
				    "client", "cert.pem"]),
    ClientCertFileDigitalSignatureOnly = filename:join([?config(priv_dir, Config),
				    "client", "digital_signature_only_cert.pem"]),
    ServerCaCertFile = filename:join([?config(priv_dir, Config), 
				      "server", "cacerts.pem"]),
    ServerCertFile = filename:join([?config(priv_dir, Config), 
				    "server", "cert.pem"]),
    ServerKeyFile = filename:join([?config(priv_dir, Config), 
			     "server", "key.pem"]),
    ClientKeyFile = filename:join([?config(priv_dir, Config), 
			     "client", "key.pem"]),
    ServerKeyCertFile = filename:join([?config(priv_dir, Config), 
				       "server", "keycert.pem"]),
    ClientKeyCertFile = filename:join([?config(priv_dir, Config), 
				       "client", "keycert.pem"]),

    BadCaCertFile = filename:join([?config(priv_dir, Config), 
				   "badcacert.pem"]),
    BadCertFile = filename:join([?config(priv_dir, Config), 
				   "badcert.pem"]),
    BadKeyFile = filename:join([?config(priv_dir, Config), 
			      "badkey.pem"]),
    PskSharedSecret = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>,
    [{client_opts, [{ssl_imp, new},{reuseaddr, true}]}, 
     {client_verification_opts, [{cacertfile, ClientCaCertFile}, 
				{certfile, ClientCertFile},  
				{keyfile, ClientKeyFile},
				{ssl_imp, new}]}, 
     {client_verification_opts_digital_signature_only, [{cacertfile, ClientCaCertFile},
				{certfile, ClientCertFileDigitalSignatureOnly},
				{keyfile, ClientKeyFile},
				{ssl_imp, new}]},
     {server_opts, [{ssl_imp, new},{reuseaddr, true}, 
		    {certfile, ServerCertFile}, {keyfile, ServerKeyFile}]},
     {server_anon, [{ssl_imp, new},{reuseaddr, true}, {ciphers, anonymous_suites()}]},
     {client_psk, [{ssl_imp, new},{reuseaddr, true},
		   {psk_identity, "Test-User"},
		   {user_lookup_fun, {fun user_lookup/3, PskSharedSecret}}]},
     {server_psk, [{ssl_imp, new},{reuseaddr, true},
		   {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
		   {user_lookup_fun, {fun user_lookup/3, PskSharedSecret}},
		   {ciphers, psk_suites()}]},
     {server_psk_hint, [{ssl_imp, new},{reuseaddr, true},
			{certfile, ServerCertFile}, {keyfile, ServerKeyFile},
			{psk_identity, "HINT"},
			{user_lookup_fun, {fun user_lookup/3, PskSharedSecret}},
			{ciphers, psk_suites()}]},
     {client_srp, [{ssl_imp, new},{reuseaddr, true},
		   {srp_identity, {"Test-User", "secret"}}]},
     {server_srp, [{ssl_imp, new},{reuseaddr, true},
		   {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
		   {user_lookup_fun, {fun user_lookup/3, undefined}},
		   {ciphers, srp_suites()}]},
     {server_verification_opts, [{ssl_imp, new},{reuseaddr, true}, 
		    {cacertfile, ServerCaCertFile},
		    {certfile, ServerCertFile}, {keyfile, ServerKeyFile}]},
     {client_kc_opts, [{certfile, ClientKeyCertFile},  {ssl_imp, new}]}, 
     {server_kc_opts, [{ssl_imp, new},{reuseaddr, true}, 
		       {certfile, ServerKeyCertFile}]},
     {client_bad_ca, [{cacertfile, BadCaCertFile}, 
		      {certfile, ClientCertFile},
		      {keyfile, ClientKeyFile},
		      {ssl_imp, new}]},
     {client_bad_cert, [{cacertfile, ClientCaCertFile},	    
			{certfile, BadCertFile},  
			{keyfile, ClientKeyFile},
			{ssl_imp, new}]},
     {server_bad_ca, [{ssl_imp, new},{cacertfile, BadCaCertFile},
		      {certfile, ServerCertFile}, 
		      {keyfile, ServerKeyFile}]},
     {server_bad_cert, [{ssl_imp, new},{cacertfile, ServerCaCertFile},
			{certfile, BadCertFile}, {keyfile, ServerKeyFile}]},
     {server_bad_key, [{ssl_imp, new},{cacertfile, ServerCaCertFile},
		       {certfile, ServerCertFile}, {keyfile, BadKeyFile}]}
     | Config].


make_dsa_cert(Config) ->
    
    {ServerCaCertFile, ServerCertFile, ServerKeyFile} = make_cert_files("server", Config, dsa, dsa, ""),
    {ClientCaCertFile, ClientCertFile, ClientKeyFile} = make_cert_files("client", Config, dsa, dsa, ""),
    [{server_dsa_opts, [{ssl_imp, new},{reuseaddr, true}, 
				 {cacertfile, ServerCaCertFile},
				 {certfile, ServerCertFile}, {keyfile, ServerKeyFile}]},
     {server_dsa_verify_opts, [{ssl_imp, new},{reuseaddr, true}, 
			       {cacertfile, ClientCaCertFile},
			       {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
			       {verify, verify_peer}]},
     {client_dsa_opts, [{ssl_imp, new},{reuseaddr, true}, 
			{cacertfile, ClientCaCertFile},
			{certfile, ClientCertFile}, {keyfile, ClientKeyFile}]},
     {server_srp_dsa, [{ssl_imp, new},{reuseaddr, true}, 
		       {cacertfile, ServerCaCertFile},
		       {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
		       {user_lookup_fun, {fun user_lookup/3, undefined}},
		       {ciphers, srp_dss_suites()}]},
     {client_srp_dsa, [{ssl_imp, new},{reuseaddr, true}, 
		       {srp_identity, {"Test-User", "secret"}},
		       {cacertfile, ClientCaCertFile},
		       {certfile, ClientCertFile}, {keyfile, ClientKeyFile}]}
     | Config].


make_mix_cert(Config) ->
    {ServerCaCertFile, ServerCertFile, ServerKeyFile} = make_cert_files("server", Config, dsa,
									rsa, "mix"),
    {ClientCaCertFile, ClientCertFile, ClientKeyFile} = make_cert_files("client", Config, dsa,
									rsa, "mix"),
    [{server_mix_opts, [{ssl_imp, new},{reuseaddr, true},
				 {cacertfile, ServerCaCertFile},
				 {certfile, ServerCertFile}, {keyfile, ServerKeyFile}]},
     {server_mix_verify_opts, [{ssl_imp, new},{reuseaddr, true},
			       {cacertfile, ClientCaCertFile},
			       {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
			       {verify, verify_peer}]},
     {client_mix_opts, [{ssl_imp, new},{reuseaddr, true},
			{cacertfile, ClientCaCertFile},
			{certfile, ClientCertFile}, {keyfile, ClientKeyFile}]}
     | Config].

make_cert_files(RoleStr, Config, Alg1, Alg2, Prefix) ->
    Alg1Str = atom_to_list(Alg1),
    Alg2Str = atom_to_list(Alg2),
    CaInfo = {CaCert, _} = erl_make_certs:make_cert([{key, Alg1}]),
    {Cert, CertKey} = erl_make_certs:make_cert([{key, Alg2}, {issuer, CaInfo}]),
    CaCertFile = filename:join([?config(priv_dir, Config), 
				RoleStr, Prefix ++ Alg1Str ++ "_cacerts.pem"]),
    CertFile = filename:join([?config(priv_dir, Config), 
			      RoleStr, Prefix ++ Alg2Str ++ "_cert.pem"]),
    KeyFile = filename:join([?config(priv_dir, Config), 
				   RoleStr, Prefix ++ Alg2Str ++ "_key.pem"]),
    
    der_to_pem(CaCertFile, [{'Certificate', CaCert, not_encrypted}]),
    der_to_pem(CertFile, [{'Certificate', Cert, not_encrypted}]),
    der_to_pem(KeyFile, [CertKey]),
    {CaCertFile, CertFile, KeyFile}.


start_upgrade_server(Args) ->
    Result = spawn_link(?MODULE, run_upgrade_server, [Args]),
    receive
	{listen, up} ->
	    Result
    end.

run_upgrade_server(Opts) ->
    Node = proplists:get_value(node, Opts),
    Port = proplists:get_value(port, Opts),
    TimeOut = proplists:get_value(timeout, Opts, infinity),
    TcpOptions = proplists:get_value(tcp_options, Opts),
    SslOptions = proplists:get_value(ssl_options, Opts),
    Pid = proplists:get_value(from, Opts),

    ct:print("gen_tcp:listen(~p, ~p)~n", [Port, TcpOptions]),
    {ok, ListenSocket} = rpc:call(Node, gen_tcp, listen, [Port, TcpOptions]),
    Pid ! {listen, up},
    send_selected_port(Pid, Port, ListenSocket),
    ct:print("gen_tcp:accept(~p)~n", [ListenSocket]),
    {ok, AcceptSocket} = rpc:call(Node, gen_tcp, accept, [ListenSocket]),

    try
	{ok, SslAcceptSocket} = case TimeOut of
				    infinity ->
					ct:print("ssl:ssl_accept(~p, ~p)~n",
							   [AcceptSocket, SslOptions]),
					rpc:call(Node, ssl, ssl_accept,
						 [AcceptSocket, SslOptions]);
				    _ ->
					ct:print("ssl:ssl_accept(~p, ~p, ~p)~n",
							   [AcceptSocket, SslOptions, TimeOut]),
					rpc:call(Node, ssl, ssl_accept,
						 [AcceptSocket, SslOptions, TimeOut])
				end,
	{Module, Function, Args} = proplists:get_value(mfa, Opts),
	Msg = rpc:call(Node, Module, Function, [SslAcceptSocket | Args]),
	ct:print("Upgrade Server Msg: ~p ~n", [Msg]),
	Pid ! {self(), Msg},
	receive
	    close ->
		ct:print("Upgrade Server closing~n", []),
		rpc:call(Node, ssl, close, [SslAcceptSocket])
	end
    catch error:{badmatch, Error} ->
	    Pid ! {self(), Error}
    end.

start_upgrade_client(Args) ->
    spawn_link(?MODULE, run_upgrade_client, [Args]).

run_upgrade_client(Opts) ->
    Node = proplists:get_value(node, Opts),
    Host = proplists:get_value(host, Opts),
    Port = proplists:get_value(port, Opts),
    Pid = proplists:get_value(from, Opts),
    TcpOptions = proplists:get_value(tcp_options, Opts),
    SslOptions = proplists:get_value(ssl_options, Opts),
    
    ct:print("gen_tcp:connect(~p, ~p, ~p)~n",
		       [Host, Port, TcpOptions]),
    {ok, Socket} = rpc:call(Node, gen_tcp, connect, [Host, Port, TcpOptions]),

    send_selected_port(Pid, Port, Socket),

    ct:print("ssl:connect(~p, ~p)~n", [Socket, SslOptions]),
    {ok, SslSocket} = rpc:call(Node, ssl, connect, [Socket, SslOptions]),

    {Module, Function, Args} = proplists:get_value(mfa, Opts),
    ct:print("apply(~p, ~p, ~p)~n",
		       [Module, Function, [SslSocket | Args]]),
    Msg = rpc:call(Node, Module, Function, [SslSocket | Args]),
    ct:print("Upgrade Client Msg: ~p ~n", [Msg]),
    Pid ! {self(), Msg},
    receive 
	close ->
	    ct:print("Upgrade Client closing~n", []),
	    rpc:call(Node, ssl, close, [SslSocket])
    end.

start_upgrade_server_error(Args) ->
    Result = spawn_link(?MODULE, run_upgrade_server_error, [Args]),
    receive
	{listen, up} ->
	    Result
    end.

run_upgrade_server_error(Opts) ->
    Node = proplists:get_value(node, Opts),
    Port = proplists:get_value(port, Opts),
    TimeOut = proplists:get_value(timeout, Opts, infinity),
    TcpOptions = proplists:get_value(tcp_options, Opts),
    SslOptions = proplists:get_value(ssl_options, Opts),
    Pid = proplists:get_value(from, Opts),

    ct:print("gen_tcp:listen(~p, ~p)~n", [Port, TcpOptions]),
    {ok, ListenSocket} = rpc:call(Node, gen_tcp, listen, [Port, TcpOptions]),
    Pid ! {listen, up},
    send_selected_port(Pid, Port, ListenSocket),
    ct:print("gen_tcp:accept(~p)~n", [ListenSocket]),
    {ok, AcceptSocket} = rpc:call(Node, gen_tcp, accept, [ListenSocket]),
    Error = case TimeOut of
		infinity ->
		    ct:print("ssl:ssl_accept(~p, ~p)~n",
				       [AcceptSocket, SslOptions]),
		    rpc:call(Node, ssl, ssl_accept,
			     [AcceptSocket, SslOptions]);
		_ ->
		    ct:print("ssl:ssl_accept(~p, ~p, ~p)~n",
				       [AcceptSocket, SslOptions, TimeOut]),
		    rpc:call(Node, ssl, ssl_accept,
			     [AcceptSocket, SslOptions, TimeOut])
	    end,
    Pid ! {self(), Error}.

start_server_error(Args) ->
    Result = spawn_link(?MODULE, run_server_error, [Args]),
    receive 
	{listen, up} ->
	    Result
    end.

run_server_error(Opts) ->
    Node = proplists:get_value(node, Opts),
    Port = proplists:get_value(port, Opts),
    Options = proplists:get_value(options, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport =  proplists:get_value(transport, Opts, ssl),
    ct:print("ssl:listen(~p, ~p)~n", [Port, Options]),
    case rpc:call(Node, Transport, listen, [Port, Options]) of
	{ok, #sslsocket{} = ListenSocket} ->
	    %% To make sure error_client will
	    %% get {error, closed} and not {error, connection_refused}
	    Pid ! {listen, up},
	    send_selected_port(Pid, Port, ListenSocket),
	    ct:print("ssl:transport_accept(~p)~n", [ListenSocket]),
	    case rpc:call(Node, Transport, transport_accept, [ListenSocket]) of
		{error, _} = Error ->
		    Pid ! {self(), Error};
		{ok, AcceptSocket} ->
		    ct:print("ssl:ssl_accept(~p)~n", [AcceptSocket]),
		    Error = rpc:call(Node, ssl, ssl_accept, [AcceptSocket]),
		    Pid ! {self(), Error}
	    end;
	{ok, ListenSocket} ->
	    Pid ! {listen, up},
	    send_selected_port(Pid, Port, ListenSocket),
	    ct:print("~p:accept(~p)~n", [Transport, ListenSocket]),
	     case rpc:call(Node, Transport, accept, [ListenSocket]) of
		{error, _} = Error ->
		     Pid ! {self(), Error}
	     end;
	Error ->
	    %% Not really true but as this is an error test 
	    %% this is what we want.
	    Pid ! {listen, up},
	    Pid ! {self(), Error}
    end.

start_client_error(Args) ->
    spawn_link(?MODULE, run_client_error, [Args]).

run_client_error(Opts) ->
    Node = proplists:get_value(node, Opts),
    Host = proplists:get_value(host, Opts),
    Port = proplists:get_value(port, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport = proplists:get_value(transport, Opts, ssl),
    Options = proplists:get_value(options, Opts),
    ct:print("ssl:connect(~p, ~p, ~p)~n", [Host, Port, Options]),
    Error = rpc:call(Node, Transport, connect, [Host, Port, Options]),
    Pid ! {self(), Error}.

inet_port(Pid) when is_pid(Pid)->
    receive
	{Pid, {port, Port}} ->
	    Port
    end;

inet_port(Node) ->
    {Port, Socket} = do_inet_port(Node),
     rpc:call(Node, gen_tcp, close, [Socket]),
     Port.

do_inet_port(Node) ->
    {ok, Socket} = rpc:call(Node, gen_tcp, listen, [0, [{reuseaddr, true}]]),
    {ok, Port} = rpc:call(Node, inet, port, [Socket]),
    {Port, Socket}.

no_result(_) ->
    no_result_msg.

trigger_renegotiate(Socket, [ErlData, N]) ->
    [{session_id, Id} | _ ] = ssl:session_info(Socket),
    trigger_renegotiate(Socket, ErlData, N, Id).

trigger_renegotiate(Socket, _, 0, Id) ->
    ct:sleep(1000),
    case ssl:session_info(Socket) of
	[{session_id, Id} | _ ] ->
	    fail_session_not_renegotiated;
	%% Tests that uses this function will not reuse
	%% sessions so if we get a new session id the
	%% renegotiation has succeeded.
       	[{session_id, _} | _ ] -> 
	    ok;
	{error, closed} ->
	    fail_session_fatal_alert_during_renegotiation;
	{error, timeout} ->
	    fail_timeout
    end;

trigger_renegotiate(Socket, ErlData, N, Id) ->
    ssl:send(Socket, ErlData),
    trigger_renegotiate(Socket, ErlData, N-1, Id).				   
    

send_selected_port(Pid, 0, #sslsocket{} = Socket) ->
    {ok, {_, NewPort}} = ssl:sockname(Socket),	 
    Pid ! {self(), {port, NewPort}};
send_selected_port(Pid, 0, Socket) ->
    {ok, {_, NewPort}} = inet:sockname(Socket),	 
    Pid ! {self(), {port, NewPort}};
send_selected_port(_,_,_) ->
    ok.

rsa_suites() ->
    lists:filter(fun({dhe_dss, _, _}) ->
			 false;
		    (_) ->
			 true
		 end,
		 ssl:cipher_suites()).

rsa_non_signed_suites() ->
    lists:filter(fun({rsa, _, _}) ->
			 true;
		    (_) ->
			 false
		 end,
		 ssl:cipher_suites()).

dsa_suites() ->
     lists:filter(fun({dhe_dss, _, _}) ->
			 true;
		    (_) ->
			 false
		 end,
		 ssl:cipher_suites()).


openssl_rsa_suites() ->
    Ciphers = ssl:cipher_suites(openssl),
    lists:filter(fun(Str) ->
			 case re:run(Str,"DSS",[]) of
			     nomatch ->
				 true;
			     _ ->
				 false
			 end 
		 end, Ciphers).

openssl_dsa_suites() ->
    Ciphers = ssl:cipher_suites(openssl),
    lists:filter(fun(Str) ->
			 case re:run(Str,"DSS",[]) of
			     nomatch ->
				 false;
			     _ ->
				 true
			 end 
		 end, Ciphers).

anonymous_suites() ->
    [{dh_anon, rc4_128, md5},
     {dh_anon, des_cbc, sha},
     {dh_anon, '3des_ede_cbc', sha},
     {dh_anon, aes_128_cbc, sha},
     {dh_anon, aes_256_cbc, sha}].

psk_suites() ->
    [{psk, rc4_128, sha},
     {psk, '3des_ede_cbc', sha},
     {psk, aes_128_cbc, sha},
     {psk, aes_256_cbc, sha},
     {dhe_psk, rc4_128, sha},
     {dhe_psk, '3des_ede_cbc', sha},
     {dhe_psk, aes_128_cbc, sha},
     {dhe_psk, aes_256_cbc, sha},
     {rsa_psk, rc4_128, sha},
     {rsa_psk, '3des_ede_cbc', sha},
     {rsa_psk, aes_128_cbc, sha},
     {rsa_psk, aes_256_cbc, sha}].

srp_suites() ->
    [{srp_anon, '3des_ede_cbc', sha},
     {srp_rsa, '3des_ede_cbc', sha},
     {srp_anon, aes_128_cbc, sha},
     {srp_rsa, aes_128_cbc, sha},
     {srp_anon, aes_256_cbc, sha},
     {srp_rsa, aes_256_cbc, sha}].

srp_dss_suites() ->
    [{srp_dss, '3des_ede_cbc', sha},
     {srp_dss, aes_128_cbc, sha},
     {srp_dss, aes_256_cbc, sha}].

pem_to_der(File) ->
    {ok, PemBin} = file:read_file(File),
    public_key:pem_decode(PemBin).

der_to_pem(File, Entries) ->
    PemBin = public_key:pem_encode(Entries),
    file:write_file(File, PemBin).

cipher_result(Socket, Result) ->
    Result = ssl:connection_info(Socket),
    ct:print("Successfull connect: ~p~n", [Result]),
    %% Importante to send two packets here
    %% to properly test "cipher state" handling
    ssl:send(Socket, "Hello\n"),
    receive 
	{ssl, Socket, "H"} ->
	    ssl:send(Socket, " world\n"),
	    receive_rizzo_duong_beast();
	{ssl, Socket, "Hello\n"} ->
	    ssl:send(Socket, " world\n"),
	    receive
		{ssl, Socket, " world\n"} ->
		    ok
	    end;       
	Other ->
	    {unexpected, Other}
    end.

session_info_result(Socket) ->
    ssl:session_info(Socket).


public_key(#'PrivateKeyInfo'{privateKeyAlgorithm =
				 #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?rsaEncryption},
			     privateKey = Key}) ->
    public_key:der_decode('RSAPrivateKey', iolist_to_binary(Key));

public_key(#'PrivateKeyInfo'{privateKeyAlgorithm =
				 #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-dsa'},
			     privateKey = Key}) ->
    public_key:der_decode('DSAPrivateKey', iolist_to_binary(Key));
public_key(Key) ->
    Key.
receive_rizzo_duong_beast() ->
    receive 
	{ssl, _, "ello\n"} ->
	    receive 
		{ssl, _, " "} ->
		    receive
			{ssl, _, "world\n"} ->
			    ok
		    end
	    end
    end.

state([{data,[{"State", State}]} | _]) ->
    State;
state([{data,[{"StateData", State}]} | _]) ->
    State;
state([_ | Rest]) ->
    state(Rest).

is_tls_version('tlsv1.2') ->
    true;
is_tls_version('tlsv1.1') ->
    true;
is_tls_version('tlsv1') ->
    true;
is_tls_version('sslv3') ->
    true;
is_tls_version(_) ->
    false.

init_tls_version(Version) ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, protocol_version, Version),
    ssl:start().

sufficient_crypto_support('tlsv1.2') ->
    Data = "Sampl",
    Data2 = "e #1",
    Key = <<0,1,2,3,16,17,18,19,32,33,34,35,48,49,50,51,4,5,6,7,20,21,22,23,36,37,38,39,
	    52,53,54,55,8,9,10,11,24,25,26,27,40,41,42,43,56,57,58,59>>,
    try
	crypto:sha256_mac(Key, lists:flatten([Data, Data2])),
	true
    catch _:_ -> false
    end;
sufficient_crypto_support(_) ->
    true.

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

send_recv_result(Socket) ->
    ssl:send(Socket, "Hello world"),
    {ok,"Hello world"} = ssl:recv(Socket, 11),
    ok.

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
