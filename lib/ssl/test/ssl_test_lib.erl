%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
-module(ssl_test_lib).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-record(sslsocket, { fd = nil, pid = nil}).
-define(SLEEP, 1000).

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
    ct:log("~p:~p~nssl:listen(~p, ~p)~n", [?MODULE,?LINE, Port, Options]),
    {ok, ListenSocket} = rpc:call(Node, Transport, listen, [Port, Options]),
    Pid ! {listen, up},
    send_selected_port(Pid, Port, ListenSocket),
    run_server(ListenSocket, Opts).

run_server(ListenSocket, Opts) ->
    Accepters = proplists:get_value(accepters, Opts, 1),
    run_server(ListenSocket, Opts, Accepters). 
    
run_server(ListenSocket, Opts, 1) ->
    do_run_server(ListenSocket, connect(ListenSocket, Opts), Opts);
run_server(ListenSocket, Opts, N) ->
    Pid = proplists:get_value(from, Opts),
    Server = spawn(?MODULE, run_server, [ListenSocket, Opts, 1]),
    Pid ! {accepter, N, Server},
    run_server(ListenSocket, Opts, N-1).

do_run_server(_, {error, timeout} = Result, Opts)  ->
    Pid = proplists:get_value(from, Opts),
    Pid ! {self(), Result};

do_run_server(ListenSocket, AcceptSocket, Opts) ->
    Node = proplists:get_value(node, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport = proplists:get_value(transport, Opts, ssl),
    {Module, Function, Args} = proplists:get_value(mfa, Opts),
    ct:log("~p:~p~nServer: apply(~p,~p,~p)~n",
		       [?MODULE,?LINE, Module, Function, [AcceptSocket | Args]]),
    case rpc:call(Node, Module, Function, [AcceptSocket | Args]) of
	no_result_msg ->
	    ok;
	Msg ->
	    ct:log("~p:~p~nServer Msg: ~p ~n", [?MODULE,?LINE, Msg]),
	    Pid ! {self(), Msg}
    end,
    receive
	listen ->
	    run_server(ListenSocket, Opts);
	{listen, MFA} ->
	    run_server(ListenSocket, [MFA | proplists:delete(mfa, Opts)]);
	close ->
	    ct:log("~p:~p~nServer closing  ~p ~n", [?MODULE,?LINE, self()]),
	    Result = rpc:call(Node, Transport, close, [AcceptSocket], 500),
	    Result1 = rpc:call(Node, Transport, close, [ListenSocket], 500),
	    ct:log("~p:~p~nResult ~p : ~p ~n", [?MODULE,?LINE, Result, Result1]);
	{ssl_closed, _} ->
	    ok
    end.

%%% To enable to test with s_client -reconnect
connect(#sslsocket{} = ListenSocket, Opts) ->
    Node = proplists:get_value(node, Opts),
    ReconnectTimes =  proplists:get_value(reconnect_times, Opts, 0),
    Timeout = proplists:get_value(timeout, Opts, infinity),
    SslOpts = proplists:get_value(ssl_extra_opts, Opts, []),
    AcceptSocket = connect(ListenSocket, Node, 1 + ReconnectTimes, dummy, Timeout, SslOpts),
    case ReconnectTimes of
	0 ->
	    AcceptSocket;
	_ ->
	  remove_close_msg(ReconnectTimes),
	  AcceptSocket
    end;
connect(ListenSocket, Opts) ->
    Node = proplists:get_value(node, Opts),
    ct:log("~p:~p~ngen_tcp:accept(~p)~n", [?MODULE,?LINE, ListenSocket]),
    {ok, AcceptSocket} = rpc:call(Node, gen_tcp, accept, 
				  [ListenSocket]),
    AcceptSocket.

connect(_, _, 0, AcceptSocket, _, _) ->
    AcceptSocket;

connect(ListenSocket, Node, N, _, Timeout, []) ->
    ct:log("ssl:transport_accept(~p)~n", [ListenSocket]),
    {ok, AcceptSocket} = rpc:call(Node, ssl, transport_accept, 
				  [ListenSocket]),    
    ct:log("~p:~p~nssl:ssl_accept(~p, ~p)~n", [?MODULE,?LINE, AcceptSocket, Timeout]),

    case rpc:call(Node, ssl, ssl_accept, [AcceptSocket, Timeout]) of
	ok ->
	    connect(ListenSocket, Node, N-1, AcceptSocket, Timeout, []);
	Result ->
	    ct:log("~p:~p~nssl:ssl_accept@~p ret ~p",[?MODULE,?LINE, Node,Result]),
	    Result
    end;
connect(ListenSocket, Node, _, _, Timeout, Opts) ->
    ct:log("ssl:transport_accept(~p)~n", [ListenSocket]),
    {ok, AcceptSocket} = rpc:call(Node, ssl, transport_accept, 
				  [ListenSocket]),    
    ct:log("ssl:ssl_accept(~p,~p, ~p)~n", [AcceptSocket, Opts, Timeout]),
    rpc:call(Node, ssl, ssl_accept, [AcceptSocket, Opts, Timeout]),
    AcceptSocket.

remove_close_msg(0) ->
    ok;
remove_close_msg(ReconnectTimes) ->
    receive
	{ssl_closed, _} ->
	   remove_close_msg(ReconnectTimes -1)
    end.
	    
start_client(Args) ->
    Result = spawn_link(?MODULE, run_client_init, [lists:delete(return_socket, Args)]),
    receive 
	{connected, Socket} ->
	    case lists:member(return_socket, Args) of
		true -> {Result, Socket};
		false -> Result
	    end;
	{connect_failed, Reason} ->
	    {connect_failed, Reason}
    end.

run_client_init(Opts) ->
    put(retries, 0),
    run_client(Opts).

run_client(Opts) ->
    Node = proplists:get_value(node, Opts),
    Host = proplists:get_value(host, Opts),
    Port = proplists:get_value(port, Opts),
    Pid = proplists:get_value(from, Opts),
    Transport =  proplists:get_value(transport, Opts, ssl),
    Options = proplists:get_value(options, Opts),
    ct:log("~p:~p~n~p:connect(~p, ~p)@~p~n", [?MODULE,?LINE, Transport, Host, Port, Node]),
    ct:log("SSLOpts: ~p", [Options]),
    case rpc:call(Node, Transport, connect, [Host, Port, Options]) of
	{ok, Socket} ->
	    Pid ! {connected, Socket},
	    ct:log("~p:~p~nClient: connected~n", [?MODULE,?LINE]),
	    %% In special cases we want to know the client port, it will
	    %% be indicated by sending {port, 0} in options list!
	    send_selected_port(Pid,  proplists:get_value(port, Options), Socket),
	    {Module, Function, Args} = proplists:get_value(mfa, Opts),
	    ct:log("~p:~p~nClient: apply(~p,~p,~p)~n",
			       [?MODULE,?LINE, Module, Function, [Socket | Args]]),
	    case rpc:call(Node, Module, Function, [Socket | Args]) of
		no_result_msg ->
		    ok;
		Msg ->
		    ct:log("~p:~p~nClient Msg: ~p ~n", [?MODULE,?LINE, Msg]),
		    Pid ! {self(), Msg}
	    end,
	    receive
		close ->
		    ct:log("~p:~p~nClient closing~n", [?MODULE,?LINE]),
		    rpc:call(Node, Transport, close, [Socket]);
		{ssl_closed, Socket} ->
		    ok;
		{gen_tcp, closed} ->
		    ok
	    end;
	{error, econnrefused = Reason} ->
	    case get(retries) of
		N when N < 5 ->
		    ct:log("~p:~p~neconnrefused retries=~p sleep ~p",[?MODULE,?LINE, N,?SLEEP]),
		    put(retries, N+1),
		    ct:sleep(?SLEEP),
		    run_client(Opts);
	       _ ->
		    ct:log("~p:~p~nClient faild several times: connection failed: ~p ~n", [?MODULE,?LINE, Reason]),
		    Pid ! {self(), {error, Reason}}
	    end;
	{error, econnreset = Reason} ->
	      case get(retries) of
		N when N < 5 ->
		    ct:log("~p:~p~neconnreset retries=~p sleep ~p",[?MODULE,?LINE, N,?SLEEP]),
		    put(retries, N+1),
		    ct:sleep(?SLEEP),
		    run_client(Opts);
	       _ ->
		    ct:log("~p:~p~nClient faild several times: connection failed: ~p ~n", [?MODULE,?LINE, Reason]),
		    Pid ! {self(), {error, Reason}}
	    end;
	{error, Reason} ->
	    ct:log("~p:~p~nClient: connection failed: ~p ~n", [?MODULE,?LINE, Reason]),
	    Pid ! {connect_failed, Reason};
	{badrpc,BadRPC} ->
            ct:log("~p:~p~nBad rpc: ~p",[?MODULE,?LINE, BadRPC]),
            Pid ! {connect_failed, {badrpc,BadRPC}}
    end.

close(Pid) ->
    ct:log("~p:~p~nClose ~p ~n", [?MODULE,?LINE, Pid]),
    Monitor = erlang:monitor(process, Pid),
    Pid ! close,
    receive
	{'DOWN', Monitor, process, Pid, Reason} ->
	    erlang:demonitor(Monitor),
	    ct:log("~p:~p~nPid: ~p down due to:~p ~n", [?MODULE,?LINE, Pid, Reason]) 
		
    end.

close(Pid, Timeout) ->
    ct:log("~p:~p~n Close ~p ~n", [?MODULE,?LINE, Pid]),
    Monitor = erlang:monitor(process, Pid),
    Pid ! close,
    receive
	{'DOWN', Monitor, process, Pid, Reason} ->
	    erlang:demonitor(Monitor),
	    ct:log("~p:~p~nPid: ~p down due to:~p ~n", [?MODULE,?LINE, Pid, Reason]) 
    after 
	Timeout ->
	    exit(Pid, kill)
    end.

check_result(Server, ServerMsg, Client, ClientMsg) -> 
    receive 
	{Server, ServerMsg} ->
	    check_result(Client, ClientMsg);

	{Client, ClientMsg} ->
	    check_result(Server, ServerMsg);

	{Port, {data,Debug}} when is_port(Port) ->
	    ct:log("~p:~p~nopenssl ~s~n",[?MODULE,?LINE, Debug]),
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
	    ct:log("~p:~p~nopenssl ~s~n",[?MODULE,?LINE, Debug]),
	    check_result(Pid,Msg);
	%% {Port, {exit_status, Status}} when is_port(Port) ->
	%%     ct:log("~p:~p Exit status: ~p~n",[?MODULE,?LINE, Status]),
	%%    check_result(Pid, Msg);
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
	    ct:log("~p:~p~nopenssl ~s~n",[?MODULE,?LINE, Debug]),
	    wait_for_result(Server, ServerMsg, Client, ClientMsg)
	%% Unexpected ->
	%%     Unexpected
    end.

check_ok([]) ->
    ok;
check_ok(Pids) ->
    receive 
	{Pid, ok} ->
	    check_ok(lists:delete(Pid, Pids));
	Other ->
	    ct:fail({expected, {"pid()", ok}, got, Other})
    end.
	
wait_for_result(Pid, Msg) -> 
    receive 
	{Pid, Msg} -> 
	    ok;
	{Port, {data,Debug}} when is_port(Port) ->
	    ct:log("~p:~p~nopenssl ~s~n",[?MODULE,?LINE, Debug]),
	    wait_for_result(Pid,Msg)
	%% Unexpected ->
	%%     Unexpected
    end.

user_lookup(psk, _Identity, UserState) ->
    {ok, UserState};
user_lookup(srp, Username, _UserState) ->
    Salt = ssl_cipher:random_bytes(16),
    UserPassHash = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, <<"secret">>])]),
    {ok, {srp_1024, Salt, UserPassHash}}.

cert_options(Config) ->
    ClientCaCertFile = filename:join([proplists:get_value(priv_dir, Config), 
				      "client", "cacerts.pem"]),
    ClientCertFile = filename:join([proplists:get_value(priv_dir, Config), 
				    "client", "cert.pem"]),
    ClientCertFileDigitalSignatureOnly = filename:join([proplists:get_value(priv_dir, Config),
				    "client", "digital_signature_only_cert.pem"]),
    ServerCaCertFile = filename:join([proplists:get_value(priv_dir, Config), 
				      "server", "cacerts.pem"]),
    ServerCertFile = filename:join([proplists:get_value(priv_dir, Config), 
				    "server", "cert.pem"]),
    ServerKeyFile = filename:join([proplists:get_value(priv_dir, Config), 
			     "server", "key.pem"]),
    ClientKeyFile = filename:join([proplists:get_value(priv_dir, Config), 
			     "client", "key.pem"]),
    ServerKeyCertFile = filename:join([proplists:get_value(priv_dir, Config), 
				       "server", "keycert.pem"]),
    ClientKeyCertFile = filename:join([proplists:get_value(priv_dir, Config), 
				       "client", "keycert.pem"]),

    BadCaCertFile = filename:join([proplists:get_value(priv_dir, Config), 
				   "badcacert.pem"]),
    BadCertFile = filename:join([proplists:get_value(priv_dir, Config), 
				   "badcert.pem"]),
    BadKeyFile = filename:join([proplists:get_value(priv_dir, Config), 
			      "badkey.pem"]),
    PskSharedSecret = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>,

    SNIServerACertFile = filename:join([proplists:get_value(priv_dir, Config), "a.server", "cert.pem"]),
    SNIServerAKeyFile = filename:join([proplists:get_value(priv_dir, Config), "a.server", "key.pem"]),
    SNIServerBCertFile = filename:join([proplists:get_value(priv_dir, Config), "b.server", "cert.pem"]),
    SNIServerBKeyFile = filename:join([proplists:get_value(priv_dir, Config), "b.server", "key.pem"]),
    [{client_opts, [{cacertfile, ClientCaCertFile}, 
		    {certfile, ClientCertFile},  
		    {keyfile, ClientKeyFile}]}, 
     {client_verification_opts, [{cacertfile, ServerCaCertFile}, 
				{certfile, ClientCertFile},  
				{keyfile, ClientKeyFile},
				{ssl_imp, new}]}, 
     {client_verification_opts_digital_signature_only, [{cacertfile, ServerCaCertFile},
				{certfile, ClientCertFileDigitalSignatureOnly},
				{keyfile, ClientKeyFile},
				{ssl_imp, new}]},
     {server_opts, [{ssl_imp, new},{reuseaddr, true}, {cacertfile, ServerCaCertFile}, 
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
     {server_psk_anon, [{ssl_imp, new},{reuseaddr, true},
			{user_lookup_fun, {fun user_lookup/3, PskSharedSecret}},
			{ciphers, psk_anon_suites()}]},
     {server_psk_anon_hint, [{ssl_imp, new},{reuseaddr, true},
			     {psk_identity, "HINT"},
			     {user_lookup_fun, {fun user_lookup/3, PskSharedSecret}},
			     {ciphers, psk_anon_suites()}]},
     {client_srp, [{ssl_imp, new},{reuseaddr, true},
		   {srp_identity, {"Test-User", "secret"}}]},
     {server_srp, [{ssl_imp, new},{reuseaddr, true},
		   {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
		   {user_lookup_fun, {fun user_lookup/3, undefined}},
		   {ciphers, srp_suites()}]},
     {server_srp_anon, [{ssl_imp, new},{reuseaddr, true},
			{user_lookup_fun, {fun user_lookup/3, undefined}},
			{ciphers, srp_anon_suites()}]},
     {server_verification_opts, [{ssl_imp, new},{reuseaddr, true}, 
		    {cacertfile, ClientCaCertFile},
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
		       {certfile, ServerCertFile}, {keyfile, BadKeyFile}]},
     {sni_server_opts, [{sni_hosts, [
                                     {"a.server", [
                                                   {certfile, SNIServerACertFile},
                                                   {keyfile, SNIServerAKeyFile}
                                                  ]},
                                     {"b.server", [
                                                   {certfile, SNIServerBCertFile},
                                                   {keyfile, SNIServerBKeyFile}
                                                  ]}
                                    ]}]}
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

make_ecdsa_cert(Config) ->
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(ecdsa, proplists:get_value(public_keys, CryptoSupport)) of
	    true ->
	    {ServerCaCertFile, ServerCertFile, ServerKeyFile} = make_cert_files("server", Config, ec, ec, ""),
	    {ClientCaCertFile, ClientCertFile, ClientKeyFile} = make_cert_files("client", Config, ec, ec, ""),
	    [{server_ecdsa_opts, [{ssl_imp, new},{reuseaddr, true},
				  {cacertfile, ServerCaCertFile},
				  {certfile, ServerCertFile}, {keyfile, ServerKeyFile}]},
	     {server_ecdsa_verify_opts, [{ssl_imp, new},{reuseaddr, true},
					 {cacertfile, ClientCaCertFile},
					 {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
					 {verify, verify_peer}]},
	     {client_ecdsa_opts, [{ssl_imp, new},{reuseaddr, true},
				  {cacertfile, ClientCaCertFile},
				  {certfile, ClientCertFile}, {keyfile, ClientKeyFile}]}
	     | Config];
	_ ->
	    Config
    end.

%% RFC 4492, Sect. 2.3.  ECDH_RSA
%%
%%    This key exchange algorithm is the same as ECDH_ECDSA except that the
%%    server's certificate MUST be signed with RSA rather than ECDSA.
make_ecdh_rsa_cert(Config) ->
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(ecdh, proplists:get_value(public_keys, CryptoSupport)) of
	true ->
	    {ServerCaCertFile, ServerCertFile, ServerKeyFile} = make_cert_files("server", Config, rsa, ec, "rsa_"),
	    {ClientCaCertFile, ClientCertFile, ClientKeyFile} = make_cert_files("client", Config, rsa, ec, "rsa_"),
	    [{server_ecdh_rsa_opts, [{ssl_imp, new},{reuseaddr, true},
				     {cacertfile, ServerCaCertFile},
				     {certfile, ServerCertFile}, {keyfile, ServerKeyFile}]},
	     {server_ecdh_rsa_verify_opts, [{ssl_imp, new},{reuseaddr, true},
					    {cacertfile, ClientCaCertFile},
					    {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
					    {verify, verify_peer}]},
	     {client_ecdh_rsa_opts, [{ssl_imp, new},{reuseaddr, true},
				     {cacertfile, ClientCaCertFile},
				     {certfile, ClientCertFile}, {keyfile, ClientKeyFile}]}
	     | Config];
	_ ->
	    Config
    end.

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
    CaCertFile = filename:join([proplists:get_value(priv_dir, Config), 
				RoleStr, Prefix ++ Alg1Str ++ "_cacerts.pem"]),
    CertFile = filename:join([proplists:get_value(priv_dir, Config), 
			      RoleStr, Prefix ++ Alg2Str ++ "_cert.pem"]),
    KeyFile = filename:join([proplists:get_value(priv_dir, Config), 
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

    ct:log("~p:~p~ngen_tcp:listen(~p, ~p)~n", [?MODULE,?LINE, Port, TcpOptions]),
    {ok, ListenSocket} = rpc:call(Node, gen_tcp, listen, [Port, TcpOptions]),
    Pid ! {listen, up},
    send_selected_port(Pid, Port, ListenSocket),
    ct:log("~p:~p~ngen_tcp:accept(~p)~n", [?MODULE,?LINE, ListenSocket]),
    {ok, AcceptSocket} = rpc:call(Node, gen_tcp, accept, [ListenSocket]),

    try
	{ok, SslAcceptSocket} = case TimeOut of
				    infinity ->
					ct:log("~p:~p~nssl:ssl_accept(~p, ~p)~n",
							   [?MODULE,?LINE, AcceptSocket, SslOptions]),
					rpc:call(Node, ssl, ssl_accept,
						 [AcceptSocket, SslOptions]);
				    _ ->
					ct:log("~p:~p~nssl:ssl_accept(~p, ~p, ~p)~n",
							   [?MODULE,?LINE, AcceptSocket, SslOptions, TimeOut]),
					rpc:call(Node, ssl, ssl_accept,
						 [AcceptSocket, SslOptions, TimeOut])
				end,
	{Module, Function, Args} = proplists:get_value(mfa, Opts),
	Msg = rpc:call(Node, Module, Function, [SslAcceptSocket | Args]),
	ct:log("~p:~p~nUpgrade Server Msg: ~p ~n", [?MODULE,?LINE, Msg]),
	Pid ! {self(), Msg},
	receive
	    close ->
		ct:log("~p:~p~nUpgrade Server closing~n", [?MODULE,?LINE]),
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
    
    ct:log("~p:~p~ngen_tcp:connect(~p, ~p, ~p)~n",
		       [?MODULE,?LINE, Host, Port, TcpOptions]),
    {ok, Socket} = rpc:call(Node, gen_tcp, connect, [Host, Port, TcpOptions]),

    send_selected_port(Pid, Port, Socket),

    ct:log("~p:~p~nssl:connect(~p, ~p)~n", [?MODULE,?LINE, Socket, SslOptions]),
    {ok, SslSocket} = rpc:call(Node, ssl, connect, [Socket, SslOptions]),

    {Module, Function, Args} = proplists:get_value(mfa, Opts),
    ct:log("~p:~p~napply(~p, ~p, ~p)~n",
		       [?MODULE,?LINE, Module, Function, [SslSocket | Args]]),
    Msg = rpc:call(Node, Module, Function, [SslSocket | Args]),
    ct:log("~p:~p~nUpgrade Client Msg: ~p ~n", [?MODULE,?LINE, Msg]),
    Pid ! {self(), Msg},
    receive 
	close ->
	    ct:log("~p:~p~nUpgrade Client closing~n", [?MODULE,?LINE]),
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

    ct:log("~p:~p~ngen_tcp:listen(~p, ~p)~n", [?MODULE,?LINE, Port, TcpOptions]),
    {ok, ListenSocket} = rpc:call(Node, gen_tcp, listen, [Port, TcpOptions]),
    Pid ! {listen, up},
    send_selected_port(Pid, Port, ListenSocket),
    ct:log("~p:~p~ngen_tcp:accept(~p)~n", [?MODULE,?LINE, ListenSocket]),
    {ok, AcceptSocket} = rpc:call(Node, gen_tcp, accept, [ListenSocket]),
    Error = case TimeOut of
		infinity ->
		    ct:log("~p:~p~nssl:ssl_accept(~p, ~p)~n",
				       [?MODULE,?LINE, AcceptSocket, SslOptions]),
		    rpc:call(Node, ssl, ssl_accept,
			     [AcceptSocket, SslOptions]);
		_ ->
		    ct:log("~p:~p~nssl:ssl_accept(~p, ~p, ~p)~n",
				       [?MODULE,?LINE, AcceptSocket, SslOptions, TimeOut]),
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
    ct:log("~p:~p~nssl:listen(~p, ~p)~n", [?MODULE,?LINE, Port, Options]),
    case rpc:call(Node, Transport, listen, [Port, Options]) of
	{ok, #sslsocket{} = ListenSocket} ->
	    %% To make sure error_client will
	    %% get {error, closed} and not {error, connection_refused}
	    Pid ! {listen, up},
	    send_selected_port(Pid, Port, ListenSocket),
	    ct:log("~p:~p~nssl:transport_accept(~p)~n", [?MODULE,?LINE, ListenSocket]),
	    case rpc:call(Node, Transport, transport_accept, [ListenSocket]) of
		{error, _} = Error ->
		    Pid ! {self(), Error};
		{ok, AcceptSocket} ->
		    ct:log("~p:~p~nssl:ssl_accept(~p)~n", [?MODULE,?LINE, AcceptSocket]),
		    Error = rpc:call(Node, ssl, ssl_accept, [AcceptSocket]),
		    Pid ! {self(), Error}
	    end;
	{ok, ListenSocket} ->
	    Pid ! {listen, up},
	    send_selected_port(Pid, Port, ListenSocket),
	    ct:log("~p:~p~n~p:accept(~p)~n", [?MODULE,?LINE, Transport, ListenSocket]),
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
    ct:log("~p:~p~nssl:connect(~p, ~p, ~p)~n", [?MODULE,?LINE, Host, Port, Options]),
    Error = rpc:call(Node, Transport, connect, [Host, Port, Options]),
    Pid ! {self(), Error}.

accepters(N) ->
    accepters([], N).

accepters(Acc, 0) ->
    Acc;
accepters(Acc, N) ->
    receive 
	{accepter, _, Server} ->
	    accepters([Server| Acc], N-1)
    end.

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

rsa_suites(CounterPart) ->
    ECC = is_sane_ecc(CounterPart),
    FIPS = is_fips(CounterPart),
    lists:filter(fun({rsa, des_cbc, sha}) when FIPS == true ->
			 false;
		    ({dhe_rsa, des_cbc, sha}) when FIPS == true ->
			 false;
		    ({rsa, _, _}) ->
			 true;
		    ({dhe_rsa, _, _}) ->
			 true;
		    ({ecdhe_rsa, _, _}) when ECC == true ->
			 true;
		    ({rsa, _, _, _}) ->
			 true;
		    ({dhe_rsa, _, _,_}) ->
			 true;
		    ({ecdhe_rsa, _, _,_}) when ECC == true ->
			 true;
		    (_) ->
			 false
		 end,
                 common_ciphers(CounterPart)).

common_ciphers(crypto) ->
    ssl:cipher_suites();
common_ciphers(openssl) ->
    OpenSslSuites =
        string:tokens(string:strip(os:cmd("openssl ciphers"), right, $\n), ":"),
    [ssl_cipher:erl_suite_definition(S)
     || S <- ssl_cipher:suites(tls_record:highest_protocol_version([])),
        lists:member(ssl_cipher:openssl_suite_name(S), OpenSslSuites)
    ].

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

ecdsa_suites() ->
     lists:filter(fun({ecdhe_ecdsa, _, _}) ->
			 true;
		    (_) ->
			 false
		 end,
		 ssl:cipher_suites()).

ecdh_rsa_suites() ->
     lists:filter(fun({ecdh_rsa, _, _}) ->
			 true;
		    (_) ->
			 false
		 end,
		 ssl:cipher_suites()).

openssl_rsa_suites(CounterPart) ->
    Ciphers = ssl:cipher_suites(openssl),
    Names = case is_sane_ecc(CounterPart) of
		true ->
		    "DSS | ECDSA";
		false ->
		    "DSS | ECDHE | ECDH"
		end,
    lists:filter(fun(Str) -> string_regex_filter(Str, Names)
		 end, Ciphers).

openssl_dsa_suites() ->
    Ciphers = ssl:cipher_suites(openssl),
    lists:filter(fun(Str) -> string_regex_filter(Str, "DSS")
		 end, Ciphers).

openssl_ecdsa_suites() ->
    Ciphers = ssl:cipher_suites(openssl),
    lists:filter(fun(Str) -> string_regex_filter(Str, "ECDHE-ECDSA")
		 end, Ciphers).

openssl_ecdh_rsa_suites() ->
    Ciphers = ssl:cipher_suites(openssl),
    lists:filter(fun(Str) -> string_regex_filter(Str, "ECDH-RSA")
		 end, Ciphers).

string_regex_filter(Str, Search) when is_list(Str) ->
    case re:run(Str, Search, []) of
	nomatch ->
	    false;
	_ ->
	    true
    end;
string_regex_filter(_Str, _Search) ->
    false.

anonymous_suites() ->
    Suites =
	[{dh_anon, rc4_128, md5},
	 {dh_anon, des_cbc, sha},
	 {dh_anon, '3des_ede_cbc', sha},
	 {dh_anon, aes_128_cbc, sha},
	 {dh_anon, aes_256_cbc, sha},
	 {dh_anon, aes_128_gcm, null, sha256},
	 {dh_anon, aes_256_gcm, null, sha384},
	 {ecdh_anon,rc4_128,sha},
	 {ecdh_anon,'3des_ede_cbc',sha},
	 {ecdh_anon,aes_128_cbc,sha},
	 {ecdh_anon,aes_256_cbc,sha}],
    ssl_cipher:filter_suites(Suites).

psk_suites() ->
    Suites =
	[{psk, rc4_128, sha},
	 {psk, '3des_ede_cbc', sha},
	 {psk, aes_128_cbc, sha},
	 {psk, aes_256_cbc, sha},
	 {psk, aes_128_cbc, sha256},
	 {psk, aes_256_cbc, sha384},
	 {dhe_psk, rc4_128, sha},
	 {dhe_psk, '3des_ede_cbc', sha},
	 {dhe_psk, aes_128_cbc, sha},
	 {dhe_psk, aes_256_cbc, sha},
	 {dhe_psk, aes_128_cbc, sha256},
	 {dhe_psk, aes_256_cbc, sha384},
	 {rsa_psk, rc4_128, sha},
	 {rsa_psk, '3des_ede_cbc', sha},
	 {rsa_psk, aes_128_cbc, sha},
	 {rsa_psk, aes_256_cbc, sha},
	 {rsa_psk, aes_128_cbc, sha256},
	 {rsa_psk, aes_256_cbc, sha384},
	 {psk, aes_128_gcm, null, sha256},
	 {psk, aes_256_gcm, null, sha384},
	 {dhe_psk, aes_128_gcm, null, sha256},
	 {dhe_psk, aes_256_gcm, null, sha384},
	 {rsa_psk, aes_128_gcm, null, sha256},
	 {rsa_psk, aes_256_gcm, null, sha384}],
    ssl_cipher:filter_suites(Suites).

psk_anon_suites() ->
    Suites =
	[{psk, rc4_128, sha},
	 {psk, '3des_ede_cbc', sha},
	 {psk, aes_128_cbc, sha},
	 {psk, aes_256_cbc, sha},
	 {dhe_psk, rc4_128, sha},
	 {dhe_psk, '3des_ede_cbc', sha},
	 {dhe_psk, aes_128_cbc, sha},
	 {dhe_psk, aes_256_cbc, sha}],
    ssl_cipher:filter_suites(Suites).

srp_suites() ->
    Suites =
	[{srp_anon, '3des_ede_cbc', sha},
	 {srp_rsa, '3des_ede_cbc', sha},
	 {srp_anon, aes_128_cbc, sha},
	 {srp_rsa, aes_128_cbc, sha},
	 {srp_anon, aes_256_cbc, sha},
	 {srp_rsa, aes_256_cbc, sha}],
    ssl_cipher:filter_suites(Suites).

srp_anon_suites() ->
    Suites =
	[{srp_anon, '3des_ede_cbc', sha},
	 {srp_anon, aes_128_cbc, sha},
	 {srp_anon, aes_256_cbc, sha}],
    ssl_cipher:filter_suites(Suites).

srp_dss_suites() ->
    Suites =
	[{srp_dss, '3des_ede_cbc', sha},
	 {srp_dss, aes_128_cbc, sha},
	 {srp_dss, aes_256_cbc, sha}],
    ssl_cipher:filter_suites(Suites).

rc4_suites(Version) ->
    Suites = ssl_cipher:rc4_suites(Version),
    ssl_cipher:filter_suites(Suites).

des_suites(Version) ->
    Suites = ssl_cipher:des_suites(Version),
    ssl_cipher:filter_suites(Suites).

pem_to_der(File) ->
    {ok, PemBin} = file:read_file(File),
    public_key:pem_decode(PemBin).

der_to_pem(File, Entries) ->
    PemBin = public_key:pem_encode(Entries),
    file:write_file(File, PemBin).

cipher_result(Socket, Result) ->
    {ok, Info} = ssl:connection_information(Socket),
    Result = {ok, {proplists:get_value(protocol, Info), proplists:get_value(cipher_suite, Info)}},
    ct:log("~p:~p~nSuccessfull connect: ~p~n", [?MODULE,?LINE, Result]),
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


state([{data,[{"State", {_StateName, StateData}}]} | _]) -> %% gen_statem
    StateData;
state([{data,[{"State", State}]} | _]) -> %% gen_server
    State;
state([{data,[{"StateData", State}]} | _]) -> %% gen_fsm
     State;
state([_ | Rest]) ->
    state(Rest).

is_tls_version('dtlsv1.2') ->
    true;
is_tls_version('dtlsv1') ->
    true;
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

init_tls_version(Version, Config)
  when Version == 'dtlsv1.2'; Version == 'dtlsv1' ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, dtls_protocol_version, Version),
    ssl:start(),
    [{protocol, dtls}, {protocol_opts, [{protocol, dtls}]}|Config];

init_tls_version(Version, Config) ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, protocol_version, Version),
    ssl:start(),
    [{protocol, tls}|Config].

sufficient_crypto_support(Version)
  when Version == 'tlsv1.2'; Version == 'dtlsv1.2' ->
    CryptoSupport = crypto:supports(),
    proplists:get_bool(sha256, proplists:get_value(hashs, CryptoSupport));
sufficient_crypto_support(Group) when Group == ciphers_ec;     %% From ssl_basic_SUITE
				      Group == erlang_server;  %% From ssl_ECC_SUITE
				      Group == erlang_client;  %% From ssl_ECC_SUITE
				      Group == erlang ->       %% From ssl_ECC_SUITE
    CryptoSupport = crypto:supports(),
    proplists:get_bool(ecdh, proplists:get_value(public_keys, CryptoSupport));
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

is_sane_ecc(openssl) ->
    case os:cmd("openssl version") of
	"OpenSSL 1.0.0a" ++ _ -> % Known bug in openssl
	    %% manifests as SSL_CHECK_SERVERHELLO_TLSEXT:tls invalid ecpointformat list
	    false;
	"OpenSSL 1.0.0" ++ _ ->  % Known bug in openssl
	    %% manifests as SSL_CHECK_SERVERHELLO_TLSEXT:tls invalid ecpointformat list
	    false;
	"OpenSSL 1.0.1l" ++ _ ->  
	    %% Breaks signature verification 
	    false;
	"OpenSSL 0.9.8" ++ _ -> % Does not support ECC
	    false;
	"OpenSSL 0.9.7" ++ _ -> % Does not support ECC
	    false;
	_ ->
	    true
    end;
is_sane_ecc(crypto) ->
    [{_,_, Bin}]  = crypto:info_lib(), 
    case binary_to_list(Bin) of
	"OpenSSL 0.9.8" ++ _ -> % Does not support ECC
	    false;
	"OpenSSL 0.9.7" ++ _ -> % Does not support ECC
	    false;
	_ ->
	    true
    end;
is_sane_ecc(_) ->
    true.

is_fips(openssl) ->
    VersionStr = os:cmd("openssl version"),
    case re:split(VersionStr, "fips") of
	[_] ->
	    false;
	_ ->
	    true
    end;
is_fips(crypto) ->
    [{_,_, Bin}]  = crypto:info_lib(),
    case re:split(Bin, <<"fips">>) of
	[_] ->
	    false;
	_ ->
	    true
    end;
is_fips(_) ->
    false.

cipher_restriction(Config0) ->
    case is_sane_ecc(openssl) of
	false ->
	    Opts = proplists:get_value(server_opts, Config0),
	    Config1 = proplists:delete(server_opts, Config0),
	    VerOpts = proplists:get_value(server_verification_opts, Config1),
	    Config = proplists:delete(server_verification_opts, Config1),
	    Restricted0 = ssl:cipher_suites() -- ecdsa_suites(),
            Restricted  = Restricted0 -- ecdh_rsa_suites(),
	    [{server_opts, [{ciphers, Restricted} | Opts]}, {server_verification_opts, [{ciphers, Restricted} | VerOpts] } | Config];
	true ->
	    Config0
    end.

check_sane_openssl_version(Version) ->
    case supports_ssl_tls_version(Version) of 
	true ->
	    case {Version, os:cmd("openssl version")} of
		{_, "OpenSSL 1.0.2" ++ _} ->
		    true;
		{_, "OpenSSL 1.0.1" ++ _} ->
		    true;
		{'tlsv1.2', "OpenSSL 1.0" ++ _} ->
		    false;
		{'tlsv1.1', "OpenSSL 1.0" ++ _} ->
		    false;
		{'tlsv1.2', "OpenSSL 0" ++ _} ->
		    false;
		{'tlsv1.1', "OpenSSL 0" ++ _} ->
		    false;
		{_, _} ->
		    true
	    end;
	false ->
	    false
    end.
enough_openssl_crl_support("OpenSSL 0." ++ _) -> false;
enough_openssl_crl_support(_) -> true.

wait_for_openssl_server(Port) ->
    wait_for_openssl_server(Port, 10).
wait_for_openssl_server(_, 0) ->
    exit(failed_to_connect_to_openssl);
wait_for_openssl_server(Port, N) ->
    case gen_tcp:connect("localhost", Port, []) of
	{ok, S} ->
	    gen_tcp:close(S);
	_  ->
	    ct:sleep(?SLEEP),
	    wait_for_openssl_server(Port, N-1)
    end.

version_flag(tlsv1) ->
    "-tls1";
version_flag('tlsv1.1') ->
    "-tls1_1";
version_flag('tlsv1.2') ->
    "-tls1_2";
version_flag(sslv3) ->
    "-ssl3";
version_flag(sslv2) ->
    "-ssl2".

filter_suites(Ciphers0) ->
    Version = tls_record:highest_protocol_version([]),
    Supported0 = ssl_cipher:suites(Version)
	++ ssl_cipher:anonymous_suites(Version)
	++ ssl_cipher:psk_suites(Version)
	++ ssl_cipher:srp_suites() 
	++ ssl_cipher:rc4_suites(Version),
    Supported1 = ssl_cipher:filter_suites(Supported0),
    Supported2 = [ssl_cipher:erl_suite_definition(S) || S <- Supported1],
    [Cipher || Cipher <- Ciphers0, lists:member(Cipher, Supported2)].

-define(OPENSSL_QUIT, "Q\n").
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

portable_open_port(Exe, Args) ->
    AbsPath = os:find_executable(Exe),
    ct:pal("open_port({spawn_executable, ~p}, [{args, ~p}, stderr_to_stdout]).", [AbsPath, Args]),
    open_port({spawn_executable, AbsPath}, 
	      [{args, Args}, stderr_to_stdout]). 

supports_ssl_tls_version(Version) ->
    VersionFlag = version_flag(Version),
    Exe = "openssl",
    Args = ["s_client", VersionFlag],
    Port = ssl_test_lib:portable_open_port(Exe, Args),
    do_supports_ssl_tls_version(Port).

do_supports_ssl_tls_version(Port) ->
    receive 
	{Port, {data, "unknown option"  ++ _}} -> 
	    false;
	{Port, {data, Data}} ->
	    case lists:member("error", string:tokens(Data, ":")) of
		true ->
		    false;
		false ->
		    do_supports_ssl_tls_version(Port)
	    end
    after 500 ->
	    true
    end.

ssl_options(Option, Config) ->
    ProtocolOpts = proplists:get_value(protocol_opts, Config, []),
    Opts = proplists:get_value(Option, Config, []),
    Opts ++ ProtocolOpts.

protocol_version(Config) ->
   protocol_version(Config, atom).

protocol_version(Config, tuple) ->
    case proplists:get_value(protocol, Config) of
	dtls ->
	    dtls_record:protocol_version(dtls_record:highest_protocol_version([]));
	_ ->
	    tls_record:highest_protocol_version(tls_record:supported_protocol_versions())
   end;

protocol_version(Config, atom) ->
    case proplists:get_value(protocol, Config) of
	dtls ->
	   dtls_record:protocol_version(protocol_version(Config, tuple));
	_ ->							 
           tls_record:protocol_version(protocol_version(Config, tuple))	
   end.

protocol_options(Config, Options) ->
    Protocol = proplists:get_value(protocol, Config, tls),
    {Protocol, Opts} = lists:keyfind(Protocol, 1, Options),
    Opts.

ct_log_supported_protocol_versions(Config) ->
    case proplists:get_value(protocol, Config) of
	dtls ->
	    ct:log("DTLS version ~p~n ", [dtls_record:supported_protocol_versions()]);
	_ ->
	    ct:log("TLS/SSL version ~p~n ", [tls_record:supported_protocol_versions()])
    end.
