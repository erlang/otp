%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-module(ssl_ECC_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'}
    ].

groups() ->
    [
     {'tlsv1.2', [], all_versions_groups()},
     {'tlsv1.1', [], all_versions_groups()},
     {'tlsv1', [], all_versions_groups()},
     {'erlang_server', [], key_cert_combinations()},
     {'erlang_client', [], key_cert_combinations()},
     {'erlang', [], key_cert_combinations() ++ misc()}
    ].

all_versions_groups ()->
    [{group, 'erlang_server'},
     {group, 'erlang_client'},
     {group, 'erlang'}
    ].

key_cert_combinations() ->
    [client_ecdh_server_ecdh,
     client_rsa_server_ecdh,
     client_ecdh_server_rsa,
     client_rsa_server_rsa,
     client_ecdsa_server_ecdsa,
     client_ecdsa_server_rsa,
     client_rsa_server_ecdsa
    ].

misc()->
    [client_ecdsa_server_ecdsa_with_raw_key].

%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    end_per_suite(Config0),
    try crypto:start() of
	ok ->
	    %% make rsa certs using oppenssl
	    {ok, _} = make_certs:all(proplists:get_value(data_dir, Config0),
				     proplists:get_value(priv_dir, Config0)),
	    Config1 = ssl_test_lib:make_ecdsa_cert(Config0),
	    Config2 = ssl_test_lib:make_ecdh_rsa_cert(Config1),
	    ssl_test_lib:cert_options(Config2)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(ssl),
    application:stop(crypto).

%%--------------------------------------------------------------------
init_per_group(erlang_client = Group, Config) ->
    case ssl_test_lib:is_sane_ecc(openssl) of
	true ->
	    common_init_per_group(Group, [{server_type, openssl},
					  {client_type, erlang} | Config]);
	false ->
	    {skip, "Known ECC bug in openssl"}
    end;

init_per_group(erlang_server = Group, Config) ->
    case ssl_test_lib:is_sane_ecc(openssl) of 
	true ->
	    common_init_per_group(Group, [{server_type, erlang},
					  {client_type, openssl} | Config]);
	false ->
	    {skip, "Known ECC bug in openssl"}
    end;
	
init_per_group(erlang = Group, Config) ->
     case ssl_test_lib:sufficient_crypto_support(Group) of
	 true ->
	     common_init_per_group(Group, [{server_type, erlang},
					   {client_type, erlang} | Config]);
	 false ->
	      {skip, "Crypto does not support ECC"}
     end;

init_per_group(openssl = Group, Config) ->
     case ssl_test_lib:sufficient_crypto_support(Group) of
	 true ->
	     common_init_per_group(Group, [{server_type, openssl},
					   {client_type, openssl} | Config]);
	 false ->
	      {skip, "Crypto does not support ECC"}
     end;
		 
init_per_group(Group, Config) ->
    common_init_per_group(Group, Config).

common_init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
	true ->
	    Config0 = ssl_test_lib:init_tls_version(GroupName, Config),
	    [{tls_version, GroupName} | Config0];
	_ ->
	   openssl_check(GroupName, Config)
    end.

end_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------

init_per_testcase(TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:log("Ciphers: ~p~n ", [ ssl:cipher_suites()]),
    end_per_testcase(TestCase, Config),
    ssl:start(),	
    ct:timetrap({seconds, 15}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    application:stop(ssl),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

client_ecdh_server_ecdh(Config) when is_list(Config) ->
    COpts =  proplists:get_value(client_ecdh_rsa_opts, Config),
    SOpts = proplists:get_value(server_ecdh_rsa_opts, Config),
    basic_test(COpts, SOpts, Config).
    
client_ecdh_server_rsa(Config)  when is_list(Config) ->
    COpts =  proplists:get_value(client_ecdh_rsa_opts, Config),
    SOpts = proplists:get_value(server_opts, Config),
    basic_test(COpts, SOpts, Config).
  
client_rsa_server_ecdh(Config)  when is_list(Config) ->
    COpts =  proplists:get_value(client_opts, Config),
    SOpts = proplists:get_value(server_ecdh_rsa_opts, Config),
    basic_test(COpts, SOpts, Config).
   
client_rsa_server_rsa(Config)  when is_list(Config) ->
    COpts =  proplists:get_value(client_opts, Config),
    SOpts = proplists:get_value(server_opts, Config),
    basic_test(COpts, SOpts, Config).
   
client_ecdsa_server_ecdsa(Config)  when is_list(Config) ->
    COpts =  proplists:get_value(client_ecdsa_opts, Config),
    SOpts = proplists:get_value(server_ecdsa_opts, Config),
    basic_test(COpts, SOpts, Config).

client_ecdsa_server_rsa(Config)  when is_list(Config) ->
    COpts =  proplists:get_value(client_ecdsa_opts, Config),
    SOpts = proplists:get_value(server_opts, Config),
    basic_test(COpts, SOpts, Config).

client_rsa_server_ecdsa(Config)  when is_list(Config) ->
    COpts =  proplists:get_value(client_opts, Config),
    SOpts = proplists:get_value(server_ecdsa_opts, Config),
    basic_test(COpts, SOpts, Config).

client_ecdsa_server_ecdsa_with_raw_key(Config)  when is_list(Config) ->
    COpts =  proplists:get_value(client_ecdsa_opts, Config),
    SOpts = proplists:get_value(server_ecdsa_opts, Config),
    ServerCert = proplists:get_value(certfile, SOpts),
    ServerKeyFile = proplists:get_value(keyfile, SOpts),
    {ok, PemBin} = file:read_file(ServerKeyFile),
    PemEntries = public_key:pem_decode(PemBin),
    {'ECPrivateKey', Key, not_encrypted} = proplists:lookup('ECPrivateKey', PemEntries),
    ServerKey = {'ECPrivateKey', Key},
    ServerCA = proplists:get_value(cacertfile, SOpts),
    ClientCert = proplists:get_value(certfile, COpts),
    ClientKey = proplists:get_value(keyfile, COpts),
    ClientCA = proplists:get_value(cacertfile, COpts),
    SType = proplists:get_value(server_type, Config),
    CType = proplists:get_value(client_type, Config),
    {Server, Port} = start_server_with_raw_key(SType,
				  ClientCA, ServerCA,
				  ServerCert,
				  ServerKey,
				  Config),
    Client = start_client(CType, Port, ServerCA, ClientCA,
			  ClientCert,
			  ClientKey, Config),
    check_result(Server, SType, Client, CType),
    close(Server, Client).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
basic_test(COpts, SOpts, Config) ->
    basic_test(proplists:get_value(certfile, COpts), 
	       proplists:get_value(keyfile, COpts), 
	       proplists:get_value(cacertfile, COpts), 
	       proplists:get_value(certfile, SOpts), 
	       proplists:get_value(keyfile, SOpts), 
	       proplists:get_value(cacertfile, SOpts), 
	       Config).
    
basic_test(ClientCert, ClientKey, ClientCA, ServerCert, ServerKey, ServerCA, Config) ->
    SType = proplists:get_value(server_type, Config),
    CType = proplists:get_value(client_type, Config),
    {Server, Port} = start_server(SType,
				  ClientCA, ServerCA,
				  ServerCert,
				  ServerKey,
				  Config),
    Client = start_client(CType, Port, ServerCA, ClientCA,
			  ClientCert,
			  ClientKey, Config),
    check_result(Server, SType, Client, CType),
    close(Server, Client).    

start_client(openssl, Port, PeerCA, OwnCa, Cert, Key, _Config) ->
    CA = new_openssl_ca("openssl_client_ca", PeerCA, OwnCa),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Exe = "openssl",
    Args = ["s_client", "-verify", "2", "-port", integer_to_list(Port),
	    ssl_test_lib:version_flag(Version),
	    "-cert", Cert, "-CAfile", CA,
	    "-key", Key, "-host","localhost", "-msg", "-debug"],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args), 
    true = port_command(OpenSslPort, "Hello world"),
    OpenSslPort;
start_client(erlang, Port, PeerCA, OwnCa, Cert, Key, Config) ->
    CA = new_ca("erlang_client_ca", PeerCA, OwnCa),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
			       {host, Hostname},
			       {from, self()},
			       {mfa, {ssl_test_lib, send_recv_result_active, []}},
			       {options, [{verify, verify_peer}, 
					  {cacertfile, CA},
					  {certfile, Cert}, {keyfile, Key}]}]).

start_server(openssl, PeerCA, OwnCa, Cert, Key, _Config) ->
    CA = new_openssl_ca("openssl_server_ca", PeerCA, OwnCa),
    Port = ssl_test_lib:inet_port(node()),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port), ssl_test_lib:version_flag(Version),
	    "-verify", "2", "-cert", Cert, "-CAfile", CA,
	    "-key", Key, "-msg", "-debug"],
    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args),
    true = port_command(OpenSslPort, "Hello world"),
    {OpenSslPort, Port};
start_server(erlang, PeerCA, OwnCa, Cert, Key, Config) ->
    CA = new_ca("erlang_server_ca", PeerCA, OwnCa),
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
			       {from, self()},
			       {mfa, {ssl_test_lib,
				      send_recv_result_active,
				      []}},
			       {options,
				[{verify, verify_peer}, {cacertfile, CA},
				 {certfile, Cert}, {keyfile, Key}]}]),
    {Server, ssl_test_lib:inet_port(Server)}.
start_server_with_raw_key(erlang, PeerCA, OwnCa, Cert, Key, Config) ->
     CA = new_ca("erlang_server_ca", PeerCA, OwnCa),
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
			       {from, self()},
			       {mfa, {ssl_test_lib,
				      send_recv_result_active,
				      []}},
			       {options,
				[{verify, verify_peer}, {cacertfile, CA},
				 {certfile, Cert}, {key, Key}]}]),
    {Server, ssl_test_lib:inet_port(Server)}.

check_result(Server, erlang, Client, erlang) ->
    ssl_test_lib:check_result(Server, ok, Client, ok);
check_result(Server, erlang, _, _) ->
    ssl_test_lib:check_result(Server, ok);
check_result(_, _, Client, erlang) ->
    ssl_test_lib:check_result(Client, ok);
check_result(_,openssl, _, openssl) ->
    ok.

openssl_check(erlang, Config) ->
    Config;
openssl_check(_, Config) ->
    TLSVersion = proplists:get_value(tls_version, Config),
    case ssl_test_lib:check_sane_openssl_version(TLSVersion) of
	true ->
	    Config;
	false ->
	    {skip, "TLS version not supported by openssl"}
    end.

close(Port1, Port2) when is_port(Port1), is_port(Port2) ->
    ssl_test_lib:close_port(Port1),
    ssl_test_lib:close_port(Port2);
close(Port, Pid) when is_port(Port) ->
    ssl_test_lib:close_port(Port),
    ssl_test_lib:close(Pid);
close(Pid, Port) when is_port(Port) ->
    ssl_test_lib:close_port(Port),
    ssl_test_lib:close(Pid);
close(Client, Server)  ->
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

new_ca(FileName, CA, OwnCa) ->
    {ok, P1} = file:read_file(CA),
    E1 = public_key:pem_decode(P1),
    {ok, P2} = file:read_file(OwnCa),
    E2 = public_key:pem_decode(P2),
    Pem = public_key:pem_encode(E1 ++E2),
    file:write_file(FileName,  Pem),
    FileName.

new_openssl_ca(FileName, CA, OwnCa) ->
    {ok, P1} = file:read_file(CA),
    E1 = public_key:pem_decode(P1),
    {ok, P2} = file:read_file(OwnCa),
    E2 = public_key:pem_decode(P2),
    case os:cmd("openssl version") of 
	"OpenSSL 1.0.1p-freebsd" ++ _ ->
	    Pem = public_key:pem_encode(E1 ++E2),
	    file:write_file(FileName,  Pem);
	"LibreSSL" ++ _ ->
	    Pem = public_key:pem_encode(E1 ++E2),
	    file:write_file(FileName,  Pem);
	_ ->
	    Pem = public_key:pem_encode(E2 ++E1),
	    file:write_file(FileName,  Pem)
    end,  
    FileName.
