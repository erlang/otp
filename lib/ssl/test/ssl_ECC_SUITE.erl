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

-module(ssl_ECC_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

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
     {'erlang', [], key_cert_combinations()}
    ].

all_versions_groups ()->
    [{group, 'erlang_server'},
     {group, 'erlang_client'},
     {group, 'erlang'}
    ].

key_cert_combinations() ->
    [client_ec_server_ec,
     client_rsa_server_ec,
     client_ec_server_rsa,
     client_rsa_server_rsa].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl:start(),
	    Config
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

%%--------------------------------------------------------------------
init_per_group(erlang_client, Config) ->
    case ssl_test_lib:is_sane_ecc(openssl) of
	true ->
	    common_init_per_group(erlang_client, [{server_type, openssl},
						  {client_type, erlang} | Config]);
	false ->
	    {skip, "Known ECC bug in openssl"}
    end;

init_per_group(erlang_server, Config) ->
    case ssl_test_lib:is_sane_ecc(openssl) of 
	true ->
	    common_init_per_group(erlang_client, [{server_type, erlang},
						  {client_type, openssl} | Config]);
	false ->
	    {skip, "Known ECC bug in openssl"}
    end;
	
init_per_group(erlang = Group, Config) ->
     case ssl_test_lib:sufficient_crypto_support(Group) of
	 true ->
	     common_init_per_group(erlang, [{server_type, erlang},
					    {client_type, erlang} | Config]);
	 false ->
	      {skip, "Crypto does not support ECC"}
     end;		 
init_per_group(Group, Config) ->
    common_init_per_group(Group, Config).

common_init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
	true ->
	    ssl_test_lib:init_tls_version(GroupName),
	    [{tls_version, GroupName} | Config];
	_ ->
	   openssl_check(GroupName, Config)
    end.

end_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------

init_per_testcase(_TestCase, Config) ->
    ct:log("TLS/SSL version ~p~n ", [tls_record:supported_protocol_versions()]),
    ct:log("Ciphers: ~p~n ", [ ssl:cipher_suites()]),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

client_ec_server_ec(Config) when is_list(Config) ->
    basic_test("ec1.crt", "ec1.key", "ec2.crt", "ec2.key", Config).

client_ec_server_rsa(Config)  when is_list(Config) ->
    basic_test("ec1.crt", "ec1.key", "rsa1.crt", "rsa1.key", Config).

client_rsa_server_ec(Config)  when is_list(Config) ->
    basic_test("rsa1.crt", "rsa1.key", "ec2.crt", "ec2.key", Config).

client_rsa_server_rsa(Config)  when is_list(Config) ->
    basic_test("rsa1.crt", "rsa1.key", "rsa2.crt", "rsa2.key", Config).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
basic_test(ClientCert, ClientKey, ServerCert, ServerKey, Config) ->
    DataDir = ?config(data_dir, Config),
    SType = ?config(server_type, Config),
    CType = ?config(client_type, Config),
    {Server, Port} = start_server(SType,
				  filename:join(DataDir, "CA.pem"),
				  filename:join(DataDir, ServerCert),
				  filename:join(DataDir, ServerKey),
				  Config),
    Client = start_client(CType, Port, filename:join(DataDir, "CA.pem"),
			  filename:join(DataDir, ClientCert),
			  filename:join(DataDir, ClientKey), Config),
    check_result(Server, SType, Client, CType).

start_client(openssl, Port, CA, Cert, Key, _) ->
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_client -port " ++ integer_to_list(Port) ++  ssl_test_lib:version_flag(Version) ++
	" -cert " ++ Cert ++ " -CAfile " ++ CA
	++ " -key " ++ Key ++ " -host localhost -msg",
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]),
    true = port_command(OpenSslPort, "Hello world"),
    OpenSslPort;
start_client(erlang, Port, CA, Cert, Key, Config) ->
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
			       {host, Hostname},
			       {from, self()},
			       {mfa, {ssl_test_lib, send_recv_result_active, []}},
			       {options, [{verify, verify_peer}, {cacertfile, CA},
					  {certfile, Cert}, {keyfile, Key}]}]).

start_server(openssl, CA, Cert, Key, _) ->
    Port = ssl_test_lib:inet_port(node()),
    Version = tls_record:protocol_version(tls_record:highest_protocol_version([])),
    Cmd = "openssl s_server -accept " ++ integer_to_list(Port) ++ ssl_test_lib:version_flag(Version) ++
	" -cert " ++ Cert ++ " -CAfile " ++ CA
	++ " -key " ++ Key ++ " -Verify 2 -msg",
    OpenSslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]),
    ssl_test_lib:wait_for_openssl_server(),
    true = port_command(OpenSslPort, "Hello world"),
    {OpenSslPort, Port};

start_server(erlang, CA, Cert, Key, Config) ->
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
    TLSVersion = ?config(tls_version, Config),
    case ssl_test_lib:check_sane_openssl_version(TLSVersion) of
	true ->
	    ssl:start(),
	    Config;
	false ->
	    {skip, "TLS version not supported by openssl"}
    end.

