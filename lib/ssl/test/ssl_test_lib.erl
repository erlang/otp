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
	    ct:log("~p:~p~n Openssl ~s~n",[?MODULE,?LINE, Debug]),
	    check_result(Server, ServerMsg, Client, ClientMsg);
        {Port,closed} when is_port(Port) ->
            ct:log("~p:~p~n Openssl port ~n",[?MODULE,?LINE]),
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
	    ct:log("~p:~p~n Openssl ~s~n",[?MODULE,?LINE, Debug]),
	    check_result(Pid,Msg);
        {Port,closed} when is_port(Port)->
            ct:log("~p:~p Openssl port closed ~n",[?MODULE,?LINE]),
            check_result(Pid, Msg);
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
     {client_psk, [{ssl_imp, new},
		   {psk_identity, "Test-User"},
		   {user_lookup_fun, {fun user_lookup/3, PskSharedSecret}}]},
     {server_psk, [{ssl_imp, new},{reuseaddr, true},
		   {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
		   {user_lookup_fun, {fun user_lookup/3, PskSharedSecret}}]},
     {server_psk_hint, [{ssl_imp, new},{reuseaddr, true},
			{certfile, ServerCertFile}, {keyfile, ServerKeyFile},
			{psk_identity, "HINT"},
			{user_lookup_fun, {fun user_lookup/3, PskSharedSecret}}]},
     {server_psk_anon, [{ssl_imp, new},{reuseaddr, true},
			{user_lookup_fun, {fun user_lookup/3, PskSharedSecret}}]},
     {server_psk_anon_hint, [{ssl_imp, new},{reuseaddr, true},
			     {psk_identity, "HINT"},
			     {user_lookup_fun, {fun user_lookup/3, PskSharedSecret}}]},
     {client_srp, [{ssl_imp, new},
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
    {ServerCaCertFile, ServerCertFile, ServerKeyFile} = 
	make_cert_files("server", Config, dsa, dsa, "", []),
    {ClientCaCertFile, ClientCertFile, ClientKeyFile} = 
	make_cert_files("client", Config, dsa, dsa, "", []),
    [{server_dsa_opts, [{ssl_imp, new},{reuseaddr, true}, 
				 {cacertfile, ServerCaCertFile},
				 {certfile, ServerCertFile}, {keyfile, ServerKeyFile}]},
     {server_dsa_verify_opts, [{ssl_imp, new},{reuseaddr, true}, 
			       {cacertfile, ClientCaCertFile},
			       {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
			       {verify, verify_peer}]},
     {client_dsa_opts, [{ssl_imp, new},
			{cacertfile, ClientCaCertFile},
			{certfile, ClientCertFile}, {keyfile, ClientKeyFile}]},
     {server_srp_dsa, [{ssl_imp, new},{reuseaddr, true}, 
		       {cacertfile, ServerCaCertFile},
		       {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
		       {user_lookup_fun, {fun user_lookup/3, undefined}},
		       {ciphers, srp_dss_suites()}]},
     {client_srp_dsa, [{ssl_imp, new},
		       {srp_identity, {"Test-User", "secret"}},
		       {cacertfile, ClientCaCertFile},
		       {certfile, ClientCertFile}, {keyfile, ClientKeyFile}]}
     | Config].

make_rsa_cert_chains(ChainConf, Config, Suffix) ->
   CryptoSupport = crypto:supports(),
    KeyGenSpec = key_gen_info(rsa, rsa),
    ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "rsa" ++ Suffix]),
    ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "rsa" ++ Suffix]),
    GenCertData = x509_test:gen_test_certs([{digest, appropriate_sha(CryptoSupport)} | KeyGenSpec] ++ ChainConf),
    [{server_config, ServerConf}, 
     {client_config, ClientConf}] = 
        x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
    {[{verify, verify_peer} | ClientConf],
     [{reuseaddr, true}, {verify, verify_peer} | ServerConf]
    }.

make_ec_cert_chains(ClientChainType, ServerChainType, Config) ->
    CryptoSupport = crypto:supports(),
    KeyGenSpec = key_gen_info(ClientChainType, ServerChainType),
    ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), atom_to_list(ClientChainType)]),
    ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), atom_to_list(ServerChainType)]),
    GenCertData = x509_test:gen_test_certs([{digest, appropriate_sha(CryptoSupport)} | KeyGenSpec]),
    [{server_config, ServerConf}, 
     {client_config, ClientConf}] = 
        x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
    {[{verify, verify_peer} | ClientConf],
     [{reuseaddr, true}, {verify, verify_peer} | ServerConf]
    }.

key_gen_info(ClientChainType, ServerChainType) ->
    key_gen_spec("client", ClientChainType) ++ key_gen_spec("server", ServerChainType).

key_gen_spec(Role, ecdh_rsa) ->
    CurveOid = hd(tls_v1:ecc_curves(0)),
    [{list_to_atom(Role ++ "_key_gen"),  {namedCurve, CurveOid}},
     {list_to_atom(Role ++ "_key_gen_chain"),  [hardcode_rsa_key(1),
                                                {namedCurve, CurveOid}]}
    ];
key_gen_spec(Role, ecdhe_ecdsa) ->
    CurveOid = hd(tls_v1:ecc_curves(0)),
     [{list_to_atom(Role ++ "_key_gen"),  {namedCurve, CurveOid}},
      {list_to_atom(Role ++ "_key_gen_chain"),  [{namedCurve, CurveOid},
                                                 {namedCurve, CurveOid}]}
    ];
key_gen_spec(Role, ecdh_ecdsa) ->
    CurveOid = hd(tls_v1:ecc_curves(0)),
    [{list_to_atom(Role ++ "_key_gen"),  {namedCurve, CurveOid}},
     {list_to_atom(Role ++ "_key_gen_chain"),  [{namedCurve, CurveOid},
                                                {namedCurve, CurveOid}]}
    ];
key_gen_spec(Role, ecdhe_rsa) ->
    [{list_to_atom(Role ++ "_key_gen"),  hardcode_rsa_key(1)},
     {list_to_atom(Role ++ "_key_gen_chain"),  [hardcode_rsa_key(2),
                                                hardcode_rsa_key(3)]}
    ];
key_gen_spec(Role, rsa) ->
    [{list_to_atom(Role ++ "_key_gen"),  hardcode_rsa_key(1)},
     {list_to_atom(Role ++ "_key_gen_chain"),  [hardcode_rsa_key(2),
                                                hardcode_rsa_key(3)]}
    ].
make_ecdsa_cert(Config) ->
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(ecdsa, proplists:get_value(public_keys, CryptoSupport)) of
        true ->
            ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "ecdsa"]),
            ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "ecdsa"]),
            CurveOid = hd(tls_v1:ecc_curves(0)),
            GenCertData = x509_test:gen_test_certs([{server_key_gen, {namedCurve, CurveOid}}, 
                                                    {client_key_gen, {namedCurve, CurveOid}},
                                                    {server_key_gen_chain, [{namedCurve, CurveOid},
                                                                            {namedCurve, CurveOid}]},
                                                    {client_key_gen_chain, [{namedCurve, CurveOid},
                                                                            {namedCurve, CurveOid}]},
                                                    {digest, appropriate_sha(CryptoSupport)}]),
            [{server_config, ServerConf}, 
             {client_config, ClientConf}] = 
                x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
	    [{server_ecdsa_opts, [{ssl_imp, new},{reuseaddr, true} | ServerConf]},
             
	     {server_ecdsa_verify_opts, [{ssl_imp, new}, {reuseaddr, true},
					 {verify, verify_peer} | ServerConf]},
	     {client_ecdsa_opts, ClientConf}
	     | Config];
	false ->
	    Config
    end.
make_rsa_cert(Config) ->
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(rsa, proplists:get_value(public_keys, CryptoSupport)) of
        true ->
            ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "rsa"]),
            ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "rsa"]),
            GenCertData = x509_test:gen_test_certs([{server_key_gen, hardcode_rsa_key(1)}, 
                                                    {client_key_gen, hardcode_rsa_key(2)},
                                                    {server_key_gen_chain, [hardcode_rsa_key(3),
                                                                            hardcode_rsa_key(4)]},
                                                    {client_key_gen_chain, [hardcode_rsa_key(5),
                                                                            hardcode_rsa_key(6)]},
                                                    {digest, appropriate_sha(CryptoSupport)}]),
            [{server_config, ServerConf}, 
             {client_config, ClientConf}] = 
                x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),               
	    [{server_rsa_opts, [{ssl_imp, new},{reuseaddr, true} | ServerConf]},
             
	     {server_rsa_verify_opts, [{ssl_imp, new}, {reuseaddr, true},
					 {verify, verify_peer} | ServerConf]},
	     {client_rsa_opts, ClientConf},
             {client_rsa_verify_opts,  [{verify, verify_peer} |ClientConf]}
	     | Config];
	false ->
	    Config
    end.
appropriate_sha(CryptoSupport) ->
    case proplists:get_bool(sha256, CryptoSupport) of
	true ->
	    sha256;
	false ->
	    sha1
    end.

%% RFC 4492, Sect. 2.3.  ECDH_RSA
%%
%%    This key exchange algorithm is the same as ECDH_ECDSA except that the
%%    server's certificate MUST be signed with RSA rather than ECDSA.
make_ecdh_rsa_cert(Config) ->
    CryptoSupport = crypto:supports(),
    case proplists:get_bool(ecdh, proplists:get_value(public_keys, CryptoSupport)) of
	true ->
            ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "ecdh_rsa"]),
            ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "ecdh_rsa"]),
            CurveOid = hd(tls_v1:ecc_curves(0)),
            GenCertData = x509_test:gen_test_certs([{server_key_gen, {namedCurve, CurveOid}}, 
                                                    {client_key_gen, {namedCurve, CurveOid}},
                                                    {server_key_gen_chain, [hardcode_rsa_key(1),
                                                                            {namedCurve, CurveOid}
                                                                           ]},
                                                    {client_key_gen_chain, [hardcode_rsa_key(2),
                                                                            {namedCurve, CurveOid}
                                                                           ]},
                                                    {digest, appropriate_sha(CryptoSupport)}]),
            [{server_config, ServerConf}, 
             {client_config, ClientConf}] = 
                x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),

	    [{server_ecdh_rsa_opts, [{ssl_imp, new},{reuseaddr, true} | ServerConf]},
				    
	     {server_ecdh_rsa_verify_opts, [{ssl_imp, new},{reuseaddr, true}, 	 
                                            {verify, verify_peer} | ServerConf]},
					
	     {client_ecdh_rsa_opts, ClientConf}
				   
             | Config];
	_ ->
	    Config
    end.

make_mix_cert(Config) ->
    {ServerCaCertFile, ServerCertFile, ServerKeyFile} = make_cert_files("server", Config, dsa,
									rsa, "mix", []),
    {ClientCaCertFile, ClientCertFile, ClientKeyFile} = make_cert_files("client", Config, dsa,
									rsa, "mix", []),
    [{server_mix_opts, [{ssl_imp, new},{reuseaddr, true},
				 {cacertfile, ServerCaCertFile},
				 {certfile, ServerCertFile}, {keyfile, ServerKeyFile}]},
     {server_mix_verify_opts, [{ssl_imp, new},{reuseaddr, true},
			       {cacertfile, ClientCaCertFile},
			       {certfile, ServerCertFile}, {keyfile, ServerKeyFile},
			       {verify, verify_peer}]},
     {client_mix_opts, [{ssl_imp, new},
			{cacertfile, ClientCaCertFile},
			{certfile, ClientCertFile}, {keyfile, ClientKeyFile}]}
     | Config].

make_cert_files(RoleStr, Config, Alg1, Alg2, Prefix, Opts) ->
    Alg1Str = atom_to_list(Alg1),
    Alg2Str = atom_to_list(Alg2),
    CaInfo = {CaCert, _} = erl_make_certs:make_cert([{key, Alg1}| Opts]),
    {Cert, CertKey} = erl_make_certs:make_cert([{key, Alg2}, {issuer, CaInfo} | Opts]),
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
     {ok, [{session_id, Id}]} = ssl:connection_information(Socket,  [session_id]),
    trigger_renegotiate(Socket, ErlData, N, Id).

trigger_renegotiate(Socket, _, 0, Id) ->
    ct:sleep(1000),
    case ssl:connection_information(Socket,  [session_id]) of
        {ok, [{session_id, Id}]} ->
	    fail_session_not_renegotiated;
	%% Tests that uses this function will not reuse
	%% sessions so if we get a new session id the
	%% renegotiation has succeeded.
        {ok, [{session_id, _}]} -> 
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
    CryptoSupport = crypto:supports(),
    Ciphers = proplists:get_value(ciphers, CryptoSupport),
    lists:filter(fun({rsa, des_cbc, sha}) when FIPS == true ->
			 false;
		    ({dhe_rsa, des_cbc, sha}) when FIPS == true ->
			 false;
		    ({rsa, Cipher, _}) ->
			 lists:member(cipher_atom(Cipher), Ciphers);
		    ({dhe_rsa, Cipher, _}) ->
			 lists:member(cipher_atom(Cipher), Ciphers);
		    ({ecdhe_rsa, Cipher, _}) when ECC == true ->
			 lists:member(cipher_atom(Cipher), Ciphers);
		    ({rsa, Cipher, _, _}) ->
			 lists:member(cipher_atom(Cipher), Ciphers);
		    ({dhe_rsa, Cipher, _,_}) ->
			 lists:member(cipher_atom(Cipher), Ciphers);
		    ({ecdhe_rsa, Cipher, _,_}) when ECC == true ->
			 lists:member(cipher_atom(Cipher), Ciphers);
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

available_suites(Version) ->
    [ssl_cipher:erl_suite_definition(Suite) || 
	Suite  <-  ssl_cipher:filter_suites(ssl_cipher:suites(Version))].


rsa_non_signed_suites(Version) ->
    lists:filter(fun({rsa, _, _}) ->
			 false;
		    (_) ->
			 true
		 end,
		 available_suites(Version)).

dsa_suites(Version) ->
     lists:filter(fun({dhe_dss, _, _}) ->
			 true;
		    (_) ->
			 false
		 end,
		 available_suites(Version)).

ecdsa_suites(Version) ->
     lists:filter(fun({ecdhe_ecdsa, _, _}) ->
			 true;
		    (_) ->
			 false
		 end,
		 available_suites(Version)).

ecdh_rsa_suites(Version) ->
     lists:filter(fun({ecdh_rsa, _, _}) ->
			 true;
		    (_) ->
			 false
		 end,
		 available_suites(Version)).

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

anonymous_suites(Version) ->
    Suites = ssl_cipher:anonymous_suites(Version),
    ssl_cipher:filter_suites(Suites).

psk_suites(Version) ->
    Suites = ssl_cipher:psk_suites(Version),
    ssl_cipher:filter_suites(Suites).

psk_anon_suites(Version) ->
    Suites = [Suite || Suite <- psk_suites(Version), is_psk_anon_suite(Suite)],
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
    {ok, Info} = ssl:connection_information(Socket,  [session_id, cipher_suite]),
    Info.

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
    NewConfig = proplists:delete(protocol_opts, proplists:delete(protocol, Config)),
    [{protocol, dtls}, {protocol_opts, [{protocol, dtls}]} | NewConfig];

init_tls_version(Version, Config) ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, protocol_version, Version),
    ssl:start(),
    NewConfig = proplists:delete(protocol_opts, proplists:delete(protocol, Config)),
    [{protocol, tls} | NewConfig].

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
    Version = tls_record:protocol_version(protocol_version(Config0)),
    case is_sane_ecc(openssl) of
	false ->
	    Opts = proplists:get_value(server_opts, Config0),
	    Config1 = proplists:delete(server_opts, Config0),
	    VerOpts = proplists:get_value(server_verification_opts, Config1),
	    Config = proplists:delete(server_verification_opts, Config1),
	    Restricted0 = ssl:cipher_suites() -- ecdsa_suites(Version),
            Restricted  = Restricted0 -- ecdh_rsa_suites(Version),
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
                {'dtlsv1', "OpenSSL 0" ++ _} ->
		    false;
		{'dtlsv1.2', "OpenSSL 0" ++ _} ->
		    false;
		{_, _} ->
		    true
	    end;
	false ->
	    false
    end.
enough_openssl_crl_support("OpenSSL 0." ++ _) -> false;
enough_openssl_crl_support(_) -> true.

wait_for_openssl_server(Port, tls) ->
    do_wait_for_openssl_tls_server(Port, 10);
wait_for_openssl_server(Port, dtls) ->
    do_wait_for_openssl_dtls_server(Port, 10).

do_wait_for_openssl_tls_server(_, 0) ->
    exit(failed_to_connect_to_openssl);
do_wait_for_openssl_tls_server(Port, N) ->
    case gen_tcp:connect("localhost", Port, []) of
	{ok, S} ->
	    gen_tcp:close(S);
	_  ->
	    ct:sleep(?SLEEP),
	    do_wait_for_openssl_tls_server(Port, N-1)
    end.

do_wait_for_openssl_dtls_server(_, 0) ->
    %%exit(failed_to_connect_to_openssl);
    ok;
do_wait_for_openssl_dtls_server(Port, N) ->
    %% case gen_udp:open(0) of
    %%     {ok, S} ->
    %%         gen_udp:connect(S, "localhost", Port),
    %%         gen_udp:close(S);
    %%     _  ->
    %%         ct:sleep(?SLEEP),
    %%         do_wait_for_openssl_dtls_server(Port, N-1)
    %% end.
    ct:sleep(500),
    do_wait_for_openssl_dtls_server(Port, N-1).

version_flag(tlsv1) ->
    "-tls1";
version_flag('tlsv1.1') ->
    "-tls1_1";
version_flag('tlsv1.2') ->
    "-tls1_2";
version_flag(sslv3) ->
    "-ssl3";
version_flag(sslv2) ->
    "-ssl2";
version_flag('dtlsv1.2') ->
    "-dtls1_2";
version_flag('dtlsv1') ->
    "-dtls1".

filter_suites(Ciphers0, AtomVersion) ->
    Version = tls_version(AtomVersion),
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
    after 1000 ->
	    true
    end.

ssl_options(Option, Config) when is_atom(Option) ->
    ProtocolOpts = proplists:get_value(protocol_opts, Config, []),
    Opts = proplists:get_value(Option, Config, []),
    Opts ++ ProtocolOpts;
ssl_options(Options, Config) ->
    ProtocolOpts = proplists:get_value(protocol_opts, Config, []),
    Options ++ ProtocolOpts.

protocol_version(Config) ->
   protocol_version(Config, atom).

protocol_version(Config, tuple) ->
    case proplists:get_value(protocol, Config) of
	dtls ->
	    dtls_record:highest_protocol_version(dtls_record:supported_protocol_versions());
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

clean_env() ->
    application:unset_env(ssl, protocol_version),
    application:unset_env(ssl, session_lifetime),
    application:unset_env(ssl, session_cb),
    application:unset_env(ssl, session_cb_init_args),
    application:unset_env(ssl, session_cache_client_max),
    application:unset_env(ssl, session_cache_server_max),
    application:unset_env(ssl, ssl_pem_cache_clean),
    application:unset_env(ssl, bypass_pem_cache),
    application:unset_env(ssl, alert_timeout).

clean_start() ->
    ssl:stop(),
    application:load(ssl),
    clean_env(),
    ssl:start().

is_psk_anon_suite({psk, _,_}) ->
    true;
is_psk_anon_suite({dhe_psk,_,_}) ->
    true;
is_psk_anon_suite({psk, _,_,_}) ->
    true;
is_psk_anon_suite({dhe_psk, _,_,_}) ->
    true;
is_psk_anon_suite(_) ->
    false.

cipher_atom(aes_256_cbc) ->
    aes_cbc256;
cipher_atom(aes_128_cbc) ->
    aes_cbc128;
cipher_atom('3des_ede_cbc') ->
    des_ede3;
cipher_atom(Atom) ->
    Atom.
tls_version('dtlsv1' = Atom) ->
    dtls_v1:corresponding_tls_version(dtls_record:protocol_version(Atom));
tls_version('dtlsv1.2' = Atom) ->
    dtls_v1:corresponding_tls_version(dtls_record:protocol_version(Atom));
tls_version(Atom) ->
    tls_record:protocol_version(Atom).

hardcode_rsa_key(1) ->
    {'RSAPrivateKey',0,
                 23995666614853919027835084074500048897452890537492185072956789802729257783422306095699263934587064480357348855732149402060270996295002843755712064937715826848741191927820899197493902093529581182351132392364214171173881547273475904587683433713767834856230531387991145055273426806331200574039205571401702219159773947658558490957010003143162250693492642996408861265758000254664396313741422909188635443907373976005987612936763564996605457102336549804831742940035613780926178523017685712710473543251580072875247250504243621640157403744718833162626193206685233710319205099867303242759099560438381385658382486042995679707669,
                 17,
                 11292078406990079542510627799764728892919007311761028269626724613049062486316379339152594792746853873109340637991599718616598115903530750002688030558925094987642913848386305504703012749896273497577003478759630198199473669305165131570674557041773098755873191241407597673069847908861741446606684974777271632545629600685952292605647052193819136445675100211504432575554351515262198132231537860917084269870590492135731720141577986787033006338680118008484613510063003323516659048210893001173583018220214626635609151105287049126443102976056146630518124476470236027123782297108342869049542023328584384300970694412006494684657,
                 169371138592582642967021557955633494538845517070305333860805485424261447791289944610138334410987654265476540480228705481960508520379619587635662291973699651583489223555422528867090299996446070521801757353675026048850480903160224210802452555900007597342687137394192939372218903554801584969667104937092080815197,
                 141675062317286527042995673340952251894209529891636708844197799307963834958115010129693036021381525952081167155681637592199810112261679449166276939178032066869788822014115556349519329537177920752776047051833616197615329017439297361972726138285974555338480581117881706656603857310337984049152655480389797687577,
                 119556097830058336212015217380447172615655659108450823901745048534772786676204666783627059584226579481512852103690850928442711896738555003036938088452023283470698275450886490965004917644550167427154181661417665446247398284583687678213495921811770068712485038160606780733330990744565824684470897602653233516609,
                 41669135975672507953822256864985956439473391144599032012999352737636422046504414744027363535700448809435637398729893409470532385959317485048904982111185902020526124121798693043976273393287623750816484427009887116945685005129205106462566511260580751570141347387612266663707016855981760014456663376585234613993,
                 76837684977089699359024365285678488693966186052769523357232308621548155587515525857011429902602352279058920284048929101483304120686557782043616693940283344235057989514310975192908256494992960578961614059245280827077951132083993754797053182279229469590276271658395444955906108899267024101096069475145863928441,
                 asn1_NOVALUE};

hardcode_rsa_key(2) ->
{'RSAPrivateKey',0,
                 21343679768589700771839799834197557895311746244621307033143551583788179817796325695589283169969489517156931770973490560582341832744966317712674900833543896521418422508485833901274928542544381247956820115082240721897193055368570146764204557110415281995205343662628196075590438954399631753508888358737971039058298703003743872818150364935790613286541190842600031570570099801682794056444451081563070538409720109449780410837763602317050353477918147758267825417201591905091231778937606362076129350476690460157227101296599527319242747999737801698427160817755293383890373574621116766934110792127739174475029121017282777887777,
                 17,
                 18832658619343853622211588088997845201745658451136447382185486691577805721584993260814073385267196632785528033211903435807948675951440868570007265441362261636545666919252206383477878125774454042314841278013741813438699754736973658909592256273895837054592950290554290654932740253882028017801960316533503857992358685308186680144968293076156011747178275038098868263178095174694099811498968993700538293188879611375604635940554394589807673542938082281934965292051746326331046224291377703201248790910007232374006151098976879987912446997911775904329728563222485791845480864283470332826504617837402078265424772379987120023773,
                 146807662748886761089048448970170315054939768171908279335181627815919052012991509112344782731265837727551849787333310044397991034789843793140419387740928103541736452627413492093463231242466386868459637115999163097726153692593711599245170083315894262154838974616739452594203727376460632750934355508361223110419,
                 145385325050081892763917667176962991350872697916072592966410309213561884732628046256782356731057378829876640317801978404203665761131810712267778698468684631707642938779964806354584156202882543264893826268426566901882487709510744074274965029453915224310656287149777603803201831202222853023280023478269485417083,
                 51814469205489445090252393754177758254684624060673510353593515699736136004585238510239335081623236845018299924941168250963996835808180162284853901555621683602965806809675350150634081614988136541809283687999704622726877773856604093851236499993845033701707873394143336209718962603456693912094478414715725803677,
                 51312467664734785681382706062457526359131540440966797517556579722433606376221663384746714140373192528191755406283051201483646739222992016094510128871300458249756331334105225772206172777487956446433115153562317730076172132768497908567634716277852432109643395464627389577600646306666889302334125933506877206029,
                 30504662229874176232343608562807118278893368758027179776313787938167236952567905398252901545019583024374163153775359371298239336609182249464886717948407152570850677549297935773605431024166978281486607154204888016179709037883348099374995148481968169438302456074511782717758301581202874062062542434218011141540,
 asn1_NOVALUE};

hardcode_rsa_key(3) -> 
{'RSAPrivateKey',0,
                 25089040456112869869472694987833070928503703615633809313972554887193090845137746668197820419383804666271752525807484521370419854590682661809972833718476098189250708650325307850184923546875260207894844301992963978994451844985784504212035958130279304082438876764367292331581532569155681984449177635856426023931875082020262146075451989132180409962870105455517050416234175675478291534563995772675388370042873175344937421148321291640477650173765084699931690748536036544188863178325887393475703801759010864779559318631816411493486934507417755306337476945299570726975433250753415110141783026008347194577506976486290259135429,
                 17,
                 8854955455098659953931539407470495621824836570223697404931489960185796768872145882893348383311931058684147950284994536954265831032005645344696294253579799360912014817761873358888796545955974191021709753644575521998041827642041589721895044045980930852625485916835514940558187965584358347452650930302268008446431977397918214293502821599497633970075862760001650736520566952260001423171553461362588848929781360590057040212831994258783694027013289053834376791974167294527043946669963760259975273650548116897900664646809242902841107022557239712438496384819445301703021164043324282687280801738470244471443835900160721870265,
                 171641816401041100605063917111691927706183918906535463031548413586331728772311589438043965564336865070070922328258143588739626712299625805650832695450270566547004154065267940032684307994238248203186986569945677705100224518137694769557564475390859269797990555863306972197736879644001860925483629009305104925823,
                 146170909759497809922264016492088453282310383272504533061020897155289106805616042710009332510822455269704884883705830985184223718261139908416790475825625309815234508695722132706422885088219618698987115562577878897003573425367881351537506046253616435685549396767356003663417208105346307649599145759863108910523,
                 60579464612132153154728441333538327425711971378777222246428851853999433684345266860486105493295364142377972586444050678378691780811632637288529186629507258781295583787741625893888579292084087601124818789392592131211843947578009918667375697196773859928702549128225990187436545756706539150170692591519448797349,
                 137572620950115585809189662580789132500998007785886619351549079675566218169991569609420548245479957900898715184664311515467504676010484619686391036071176762179044243478326713135456833024206699951987873470661533079532774988581535389682358631768109586527575902839864474036157372334443583670210960715165278974609,
                 15068630434698373319269196003209754243798959461311186548759287649485250508074064775263867418602372588394608558985183294561315208336731894947137343239541687540387209051236354318837334154993136528453613256169847839789803932725339395739618592522865156272771578671216082079933457043120923342632744996962853951612,
 asn1_NOVALUE};
hardcode_rsa_key(4) -> 
{'RSAPrivateKey',0,
                 28617237755030755643854803617273584643843067580642149032833640135949799721163782522787597288521902619948688786051081993247908700824196122780349730169173433743054172191054872553484065655968335396052034378669869864779940355219732200954630251223541048434478476115391643898092650304645086338265930608997389611376417609043761464100338332976874588396803891301015812818307951159858145399281035705713082131199940309445719678087542976246147777388465712394062188801177717719764254900022006288880246925156931391594131839991579403409541227225173269459173129377291869028712271737734702830877034334838181789916127814298794576266389,
                 17,
                 26933870828264240605980991639786903194205240075898493207372837775011576208154148256741268036255908348187001210401018346586267012540419880263858569570986761169933338532757527109161473558558433313931326474042230460969355628442100895016122589386862163232450330461545076609969553227901257730132640573174013751883368376011370428995523268034111482031427024082719896108094847702954695363285832195666458915142143884210891427766607838346722974883433132513540317964796373298134261669479023445911856492129270184781873446960437310543998533283339488055776892320162032014809906169940882070478200435536171854883284366514852906334641,
                 177342190816702392178883147766999616783253285436834252111702533617098994535049411784501174309695427674025956656849179054202187436663487378682303508229883753383891163725167367039879190685255046547908384208614573353917213168937832054054779266431207529839577747601879940934691505396807977946728204814969824442867,
                 161367340863680900415977542864139121629424927689088951345472941851682581254789586032968359551717004797621579428672968948552429138154521719743297455351687337112710712475376510559020211584326773715482918387500187602625572442687231345855402020688502483137168684570635690059254866684191216155909970061793538842967,
                 62591361464718491357252875682470452982324688977706206627659717747211409835899792394529826226951327414362102349476180842659595565881230839534930649963488383547255704844176717778780890830090016428673547367746320007264898765507470136725216211681602657590439205035957626212244060728285168687080542875871702744541,
                 28476589564178982426348978152495139111074987239250991413906989738532220221433456358759122273832412611344984605059935696803369847909621479954699550944415412431654831613301737157474154985469430655673456186029444871051571607533040825739188591886206320553618003159523945304574388238386685203984112363845918619347,
                 34340318160575773065401929915821192439103777558577109939078671096408836197675640654693301707202885840826672396546056002756167635035389371579540325327619480512374920136684787633921441576901246290213545161954865184290700344352088099063404416346968182170720521708773285279884132629954461545103181082503707725012,
 asn1_NOVALUE};
hardcode_rsa_key(5) -> 
{'RSAPrivateKey',0,
                 26363170152814518327068346871197765236382539835597898797762992537312221863402655353436079974302838986536256364057947538018476963115004626096654613827403121905035011992899481598437933532388248462251770039307078647864188314916665766359828262009578648593031111569685489178543405615478739906285223620987558499488359880003693226535420421293716164794046859453204135383236667988765227190694994861629971618548127529849059769249520775574008363789050621665120207265361610436965088511042779948238320901918522125988916609088415989475825860046571847719492980547438560049874493788767083330042728150253120940100665370844282489982633,
                 17,
                 10855423004100095781734025182257903332628104638187370093196526338893267826106975733767797636477639582691399679317978398007608161282648963686857782164224814902073240232370374775827384395689278778574258251479385325591136364965685903795223402003944149420659869469870495544106108194608892902588033255700759382142132115013969680562678811046675523365751498355532768935784747314021422035957153013494814430893022253205880275287307995039363642554998244274484818208792520243113824379110193356010059999642946040953102866271737127640405568982049887176990990501963784502429481034227543991366980671390566584211881030995602076468001,
                 163564135568104310461344551909369650951960301778977149705601170951529791054750122905880591964737953456660497440730575925978769763154927541340839715938951226089095007207042122512586007411328664679011914120351043948122025612160733403945093961374276707993674792189646478659304624413958625254578122842556295400709,
                 161179405627326572739107057023381254841260287988433675196680483761672455172873134522398837271764104320975746111042211695289319249471386600030523328069395763313848583139553961129874895374324504709512019736703349829576024049432816885712623938437949550266365056310544300920756181033500610331519029869549723159637,
                 115457036871603042678596154288966812436677860079277988027483179495197499568058910286503947269226790675289762899339230065396778656344654735064122152427494983121714122734382674714766593466820233891067233496718383963380253373289929461608301619793607087995535147427985749641862087821617853120878674947686796753441,
                 142217122612346975946270932667689342506994371754500301644129838613240401623123353990351915239791856753802128921507833848784693455415929352968108818884760967629866396887841730408713142977345151214275311532385308673155315337734838428569962298621720191411498579097539089047726042088382891468987379296661520434973,
                 40624877259097915043489529504071755460170951428490878553842519165800720914888257733191322215286203357356050737713125202129282154441426952501134581314792133018830748896123382106683994268028624341502298766844710276939303555637478596035491641473828661569958212421472263269629366559343208764012473880251174832392,
 asn1_NOVALUE};
hardcode_rsa_key(6) -> 
{'RSAPrivateKey',0,
                 22748888494866396715768692484866595111939200209856056370972713870125588774286266397044592487895293134537316190976192161177144143633669641697309689280475257429554879273045671863645233402796222694405634510241820106743648116753479926387434021380537483429927516962909367257212902212159798399531316965145618774905828756510318897899298783143203190245236381440043169622358239226123652592179006905016804587837199618842875361941208299410035232803124113612082221121192550063791073372276763648926636149384299189072950588522522800393261949880796214514243704858378436010975184294077063518776479282353562934591448646412389762167039,
                 17,
                 6690849557313646092873144848490175032923294179369428344403739373566349639495960705013115437616262686628622409110644753287395336362844012263914614494257428655751435080307550548130951000822418439531068973600535325512837681398082331290421770994275730420566916753796872722709677121223470117509210872101652580854566448661533030419787125312956120661097410038933324613372774190658239039998357548275441758790939430824924502690997433186652165055694361752689819209062683281242276039100201318203707142383491769671330743466041394101421674581185260900666085723130684175548215193875544802254923825103844262661010117443222587769713,
                 164748737139489923768181260808494855987398781964531448608652166632780898215212977127034263859971474195908846263894581556691971503119888726148555271179103885786024920582830105413607436718060544856016793981261118694063993837665813285582095833772675610567592660039821387740255651489996976698808018635344299728063,
                 138082323967104548254375818343885141517788525705334488282154811252858957969378263753268344088034079842223206527922445018725900110643394926788280539200323021781309918753249061620424428562366627334409266756720941754364262467100514166396917565961434203543659974860389803369482625510495464845206228470088664021953,
                 19382204369351755737433089506881747763223386113474288071606137250915399790025056132592266336467232258342217207517009594904937823896457497193947678962247515974826461245038835931012639613889475865413740468383661022831058098548919210068481862796785365949128548239978986792971253116470232552800943368864035262125,
                 48734937870742781736838524121371226418043009072470995864289933383361985165662916618800592031070851709019955245149098241903258862580021738866451955011878713569874088971734962924855680669070574353320917678842685325069739694270769705787147376221682660074232932303666989424523279591939575827719845342384234360689,
                 81173034184183681160439870161505779100040258708276674532866007896310418779840630960490793104541748007902477778658270784073595697910785917474138815202903114440800310078464142273778315781957021015333260021813037604142367434117205299831740956310682461174553260184078272196958146289378701001596552915990080834227,
                 asn1_NOVALUE}.


dtls_hello() ->
    [1,
     <<0,1,4>>,
     <<0,0>>,
     <<0,0,0>>,
     <<0,1,4>>,
     <<254,253,88,
       156,129,61,
       131,216,15,
       131,194,242,
       46,154,190,
       20,228,234,
       234,150,44,
       62,96,96,103,
       127,95,103,
       23,24,42,138,
       13,142,32,57,
       230,177,32,
       210,154,152,
       188,121,134,
       136,53,105,
       118,96,106,
       103,231,223,
       133,10,165,
       50,32,211,
       227,193,14,
       181,143,48,
       66,0,0,100,0,
       255,192,44,
       192,48,192,
       36,192,40,
       192,46,192,
       50,192,38,
       192,42,0,159,
       0,163,0,107,
       0,106,0,157,
       0,61,192,43,
       192,47,192,
       35,192,39,
       192,45,192,
       49,192,37,
       192,41,0,158,
       0,162,0,103,
       0,64,0,156,0,
       60,192,10,
       192,20,0,57,
       0,56,192,5,
       192,15,0,53,
       192,8,192,18,
       0,22,0,19,
       192,3,192,13,
       0,10,192,9,
       192,19,0,51,
       0,50,192,4,
       192,14,0,47,
       1,0,0,86,0,0,
       0,14,0,12,0,
       0,9,108,111,
       99,97,108,
       104,111,115,
       116,0,10,0,
       58,0,56,0,14,
       0,13,0,25,0,
       28,0,11,0,12,
       0,27,0,24,0,
       9,0,10,0,26,
       0,22,0,23,0,
       8,0,6,0,7,0,
       20,0,21,0,4,
       0,5,0,18,0,
       19,0,1,0,2,0,
       3,0,15,0,16,
       0,17,0,11,0,
       2,1,0>>].

