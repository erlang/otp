%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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
-module(ssl_certificate_verify_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("tls_record.hrl").
-include("tls_handshake.hrl").

-define(LONG_TIMEOUT, 600000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [
     {group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'sslv3'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'tlsv1.3', [], all_protocol_groups()},
     {'tlsv1.2', [], all_protocol_groups()},
     {'tlsv1.1', [], all_protocol_groups()},
     {'tlsv1', [], all_protocol_groups()},
     {'sslv3', [], all_protocol_groups()},
     {'dtlsv1.2', [], all_protocol_groups()},
     {'dtlsv1', [], all_protocol_groups()},
     {active, [], tests()},
     {active_once, [], tests()},
     {passive, [], tests()},
     {error_handling, [],error_handling_tests()}
    ].

all_protocol_groups() ->
    [{group, active},
     {group, passive},
     {group, active_once},
     {group, error_handling}].

tests() ->
    [cert_expired,
     %invalid_signature_client,
     %%invalid_signature_server,
     extended_key_usage_verify_both,
     extended_key_usage_verify_server,
     critical_extension_verify_client,
     critical_extension_verify_server,
     critical_extension_verify_none,
     long_chain
    ].

error_handling_tests()->
    [client_with_cert_cipher_suites_handshake,
     %%unknown_server_ca_accept_backwardscompatibility,
     no_authority_key_identifier,
     no_authority_key_identifier_keyEncipherment].

init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            ssl_test_lib:clean_start(), 
            ssl_test_lib:make_rsa_cert(Config)            
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(active, Config) ->
    [{active, true}, {receive_function, send_recv_result_active} | Config];
init_per_group(active_once, Config) ->
    [{active, once}, {receive_function, send_recv_result_active_once} | Config];
init_per_group(passive, Config) ->
    [{active, false}, {receive_function, send_recv_result} | Config];
init_per_group(error_handling, Config) ->
    [{active, false}, {receive_function, send_recv_result} | Config];
init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
	true ->
	    case ssl_test_lib:sufficient_crypto_support(GroupName) of
		true ->
		    [{version, GroupName} | ssl_test_lib:init_tls_version(GroupName, Config)];
		false ->
		    {skip, "Missing crypto support"}
	    end
    end.

end_per_group(GroupName, Config) ->
       case ssl_test_lib:is_tls_version(GroupName) of
        true ->
            ssl_test_lib:clean_tls_version(Config);
        false ->
            Config
    end.

init_per_testcase(_TestCase, Config) ->
    ssl:stop(),
    ssl:start(),
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:pal(" ~p", [ dtls_record:supported_protocol_versions()]),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
server_verify_client_once() ->
    [{doc,"Test server option verify_client_once"}].

server_verify_client_once(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, []),
    ServerOpts =  ssl_test_lib:ssl_options(server_rsa_opts, Config),
    Active = proplists:get_value(active, Config),
    ReceiveFunction =  proplists:get_value(receive_function, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, ReceiveFunction, []}},
					{options, [{active, Active}, {verify, verify_peer},
						   {verify_client_once, true}
						   | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, ReceiveFunction, []}},
                                         {options, [{active, Active} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client0, ok),
    Server ! {listen, {mfa, {ssl_test_lib, no_result, []}}},
    ssl_test_lib:close(Client0),
    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, result_ok, []}},
					{options, [{active, Active} | ClientOpts]}]),

    ssl_test_lib:check_result(Client1, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------

cert_expired() ->
    [{doc,"Test server with expired certificate"}].

cert_expired(Config) when is_list(Config) ->
    {Year, Month, Day} = date(),
    Active = proplists:get_value(active, Config),
    {ClientOpts0, ServerOpts0} = ssl_test_lib:make_rsa_cert_chains([{server_chain,
                                                                     [[], 
                                                                      [{validity, {{Year-2, Month, Day}, 
                                                                                   {Year-1, Month, Day}}}],
                                                                      []
                                                                     ]}], 
                                                                   Config, "_expired"),
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),                                                     
    
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {options, [{active, Active}| ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
					      {options, [{verify, verify_peer}, {active, Active}  | ClientOpts]}]),    
    
    ssl_test_lib:check_client_alert(Server, Client, certificate_expired).

two_digits_str(N) when N < 10 ->
    lists:flatten(io_lib:format("0~p", [N]));
two_digits_str(N) ->
    lists:flatten(io_lib:format("~p", [N])).

%%--------------------------------------------------------------------
extended_key_usage_verify_server() ->
    [{doc,"Test cert that has a critical extended_key_usage extension in server cert"}].

extended_key_usage_verify_server(Config) when is_list(Config) -> 
    Ext = x509_test:extensions([{?'id-ce-extKeyUsage',
                                 [?'id-kp-serverAuth'], true}]),
    {ClientOpts0, ServerOpts0} = ssl_test_lib:make_rsa_cert_chains([{server_chain, 
                                                                     [[],[], [{extensions, Ext}]]}], Config, 
                                                                   "_keyusage_server"),
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),                                                     
    Active = proplists:get_value(active, Config),
    ReceiveFunction =  proplists:get_value(receive_function, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {ssl_test_lib,  ReceiveFunction, []}},
			   {options, [{verify, verify_none}, {active, Active} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, ReceiveFunction, []}},
					{options, [{verify, verify_peer}, {active, Active} |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
extended_key_usage_verify_both() ->
    [{doc,"Test cert that has a critical extended_key_usage extension in client verify_peer mode"}].

extended_key_usage_verify_both(Config) when is_list(Config) ->
    ServerExt = x509_test:extensions([{?'id-ce-extKeyUsage',
                                       [?'id-kp-serverAuth'], true}]),
    ClientExt = x509_test:extensions([{?'id-ce-extKeyUsage',
                                       [?'id-kp-clientAuth'], true}]),
    {ClientOpts0, ServerOpts0} = ssl_test_lib:make_rsa_cert_chains([{client_chain, [[],[],[{extensions, ClientExt}]]},
                                                                    {server_chain, [[],[],[{extensions, ServerExt}]]}], 
                                                                   Config, "_keyusage_both"),
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),        
    Active = proplists:get_value(active, Config),
    ReceiveFunction =  proplists:get_value(receive_function, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {ssl_test_lib, ReceiveFunction, []}},
			   {options, [{verify, verify_peer}, {active, Active} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, ReceiveFunction, []}},
					{options, [{verify, verify_peer}, {active, Active} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
critical_extension_verify_server() ->
    [{doc,"Test cert that has a critical unknown extension in verify_peer mode"}].

critical_extension_verify_server(Config) when is_list(Config) ->
    Ext = x509_test:extensions([{{2,16,840,1,113730,1,1}, <<3,2,6,192>>, true}]),
    {ClientOpts0, ServerOpts0} = ssl_test_lib:make_rsa_cert_chains([{client_chain, 
                                                                     [[],[], [{extensions, Ext}]]}], 
                                                                   Config, "_client_unknown_extension"),
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),              
    Active = proplists:get_value(active, Config),
    ReceiveFunction =  proplists:get_value(receive_function, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {mfa, {ssl_test_lib,  ReceiveFunction, []}},
                {options, [{verify, verify_peer}, {active, Active} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error(
               [{node, ClientNode}, {port, Port},
                {host, Hostname},
                {from, self()},
                {mfa, {ssl_test_lib, ReceiveFunction, []}},
                {options, [{verify, verify_none}, {active, Active} | ClientOpts]}]),

    %% This certificate has a critical extension that we don't
    %% understand.  Therefore, verification should fail.          
    ssl_test_lib:check_server_alert(Server, Client, unsupported_certificate).
%%--------------------------------------------------------------------

critical_extension_verify_client() ->
    [{doc,"Test cert that has a critical unknown extension in verify_peer mode"}].

critical_extension_verify_client(Config) when is_list(Config) ->
    Ext = x509_test:extensions([{{2,16,840,1,113730,1,1}, <<3,2,6,192>>, true}]),
    {ClientOpts0, ServerOpts0} = ssl_test_lib:make_rsa_cert_chains([{server_chain, 
                                                                     [[],[],[{extensions, Ext}]]}], 
                                                                   Config, "_server_unknown_extensions"),
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),              
    Active = proplists:get_value(active, Config),
    ReceiveFunction =  proplists:get_value(receive_function, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {mfa, {ssl_test_lib,  ReceiveFunction, []}},
                {options, [{verify, verify_none}, {active, Active} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error(
               [{node, ClientNode}, {port, Port},
                {host, Hostname},
                {from, self()},
                {mfa, {ssl_test_lib, ReceiveFunction, []}},
                {options, [{verify, verify_peer}, {active, Active} | ClientOpts]}]),

    ssl_test_lib:check_client_alert(Server, Client, unsupported_certificate).

%%--------------------------------------------------------------------
critical_extension_verify_none() ->
    [{doc,"Test cert that has a critical unknown extension in verify_none mode"}].

critical_extension_verify_none(Config) when is_list(Config) ->
    Ext = x509_test:extensions([{{2,16,840,1,113730,1,1}, <<3,2,6,192>>, true}]),
    {ClientOpts0, ServerOpts0} = ssl_test_lib:make_rsa_cert_chains([{server_chain, 
                                                                     [[],[], [{extensions, Ext}]]}], 
                                                                   Config, "_unknown_extensions"),
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),             
    Active = proplists:get_value(active, Config),
    ReceiveFunction =  proplists:get_value(receive_function, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {mfa, {ssl_test_lib, ReceiveFunction, []}},
                {options, [{verify, verify_none}, {active, Active} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client(
               [{node, ClientNode}, {port, Port},
                {host, Hostname},
                {from, self()},
                {mfa, {ssl_test_lib, ReceiveFunction, []}},
                {options, [{verify, verify_none}, {active, Active} | ClientOpts]}]),

    %% This certificate has a critical extension that we don't
    %% understand.  But we're using `verify_none', so verification
    %% shouldn't fail.
    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
no_authority_key_identifier() ->
    [{doc, "Test cert that does not have authorityKeyIdentifier extension"
      " but are present in trusted certs db."}].

no_authority_key_identifier(Config) when is_list(Config) ->
   {ClientOpts0, ServerOpts0} = ssl_test_lib:make_rsa_cert_chains([], Config, "_peer_no_auth_key_id"),
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),        

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

no_authority_key_identifier_keyEncipherment() ->
    [{doc, "Test cert with keyEncipherment key_usage an no"
      " authorityKeyIdentifier extension, but are present in trusted certs db."}].

no_authority_key_identifier_keyEncipherment(Config) when is_list(Config) ->
    ClientExt = x509_test:extensions([{key_usage, [digitalSignature, keyEncipherment]}]),
    {ClientOpts0, ServerOpts0} = ssl_test_lib:make_rsa_cert_chains([{client_chain, 
                                                                     [[],[],[{extensions, ClientExt}]]}], 
                                                                   Config, "_peer_keyEncipherment"),
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),        
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib,
					       send_recv_result_active, []}},
					{options, [{active, true} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib,
					       send_recv_result_active, []}},
					{options, [{verify, verify_peer} | ClientOpts]}]),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------

invalid_signature_server() ->
    [{doc,"Test client with invalid signature"}].

invalid_signature_server(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    PrivDir = proplists:get_value(priv_dir, Config),

    KeyFile =  proplists:get_value(keyfile, ServerOpts),
    [KeyEntry] = ssl_test_lib:pem_to_der(KeyFile),
    Key = ssl_test_lib:public_key(public_key:pem_entry_decode(KeyEntry)),

    ServerCertFile = proplists:get_value(certfile, ServerOpts),
    NewServerCertFile = filename:join(PrivDir, "server_invalid_cert.pem"),
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
    ssl_test_lib:check_server_alert(Server, Client, unknown_ca).

%%--------------------------------------------------------------------

invalid_signature_client() ->
    [{doc,"Test server with invalid signature"}].

invalid_signature_client(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    PrivDir = proplists:get_value(priv_dir, Config),

    KeyFile =  proplists:get_value(keyfile, ClientOpts),
    [KeyEntry] = ssl_test_lib:pem_to_der(KeyFile),
    Key = ssl_test_lib:public_key(public_key:pem_entry_decode(KeyEntry)),

    ClientCertFile = proplists:get_value(certfile, ClientOpts),
    NewClientCertFile = filename:join(PrivDir, "client_invalid_cert.pem"),
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

    ssl_test_lib:check_client_alert(Server, Client, unknown_ca).

%%--------------------------------------------------------------------

client_with_cert_cipher_suites_handshake() ->
    [{doc, "Test that client with a certificate without keyEncipherment usage "
    " extension can connect to a server with restricted cipher suites "}].
client_with_cert_cipher_suites_handshake(Config) when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [digitalSignature]}]),
  {ClientOpts0, ServerOpts0} = ssl_test_lib:make_rsa_cert_chains([{client_chain, 
                                                                     [[], [], [{extensions, Ext}]]}], 
                                                                 Config, "_sign_only_extensions"),
    ClientOpts =  ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts =  ssl_test_lib:ssl_options(ServerOpts0, Config),
    TLSVersion = ssl_test_lib:protocol_version(Config, tuple),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib,
					       send_recv_result_active, []}},
					{options, [{active, true},
						   {ciphers, 
						    ssl_test_lib:rsa_non_signed_suites(TLSVersion)}
						   | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib,
					       send_recv_result_active, []}},
					{options, [{active, true}
						   | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------


long_chain() ->
    [{doc,"Test option verify_peer"}].
long_chain(Config) when is_list(Config) ->      
    #{server_config := ServerConf,
      client_config := ClientConf} = public_key:pkix_test_data(#{server_chain => #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                                                                                  intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}], 
                                                                                                    [{key, ssl_test_lib:hardcode_rsa_key(3)}],
                                                                                                    [{key, ssl_test_lib:hardcode_rsa_key(4)}]],
                                                                                  peer => [{key, ssl_test_lib:hardcode_rsa_key(5)}]},
                                                                 client_chain => #{root => [{key, ssl_test_lib:hardcode_rsa_key(3)}], 
                                                                                  intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                                                                  peer => [{key, ssl_test_lib:hardcode_rsa_key(1)}]}}), 
    [ServerRoot| _] = ServerCas = proplists:get_value(cacerts, ServerConf),
    ClientCas = proplists:get_value(cacerts, ClientConf),
    
    Active = proplists:get_value(active, Config),
    ReceiveFunction =  proplists:get_value(receive_function, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, ReceiveFunction, []}},
                                        {options, [{active, Active}, {verify, verify_peer},
                                                   {cacerts, [ServerRoot]} |  
                                                   proplists:delete(cacerts, ServerConf)]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, ReceiveFunction, []}},
                                        {options, [{active, Active}, 
                                                   {verify, verify_peer},
                                                   {depth, 5},
                                                   {cacerts,  ServerCas ++ ClientCas} | 
                                                   proplists:delete(cacerts, ClientConf)]}]),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

