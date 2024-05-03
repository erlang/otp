%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2024. All Rights Reserved.
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

-module(tls_1_3_version_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
%% Common test
-export([all/0,
         groups/0,
         init_per_suite/1,
         init_per_group/2,
         end_per_suite/1,
         end_per_group/2
        ]).

%% Test cases
-export([tls13_client_tls12_server/0,
         tls13_client_tls12_server/1,
         tls13_client_with_ext_tls12_server/0,
         tls13_client_with_ext_tls12_server/1,
         tls12_client_tls13_server/0,
         tls12_client_tls13_server/1,
         tls_client_tls10_server/0,
         tls_client_tls10_server/1,
         tls_client_tls11_server/0,
         tls_client_tls11_server/1,
         tls_client_tls12_server/0,
         tls_client_tls12_server/1,
         tls10_client_tls_server/0,
         tls10_client_tls_server/1,
         tls11_client_tls_server/0,
         tls11_client_tls_server/1,
         tls12_client_tls_server/0,
         tls12_client_tls_server/1,
         legacy_tls12_client_tls_server/0,
         legacy_tls12_client_tls_server/1,
         legacy_tls12_server_tls_client/0,
         legacy_tls12_server_tls_client/1,
         middle_box_tls13_client/0,
         middle_box_tls13_client/1,
         middle_box_tls12_enabled_client/0,
         middle_box_tls12_enabled_client/1,
         middle_box_client_tls_v2_session_reused/0,
         middle_box_client_tls_v2_session_reused/1,
         renegotiate_error/0,
         renegotiate_error/1
        ]).


%% Test callback
-export([check_session_id/2]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [
     cert_groups()
    ].

groups() ->
    [
     {rsa, [], tls_1_3_1_2_tests() ++ legacy_tests()},
     {ecdsa, [], tls_1_3_1_2_tests()}
    ].

cert_groups() ->
    [{group, rsa},
     {group, ecdsa}].

tls_1_3_1_2_tests() ->
    [tls13_client_tls12_server,
     tls13_client_with_ext_tls12_server,
     tls12_client_tls13_server,
     tls_client_tls12_server,
     tls12_client_tls_server,
     legacy_tls12_client_tls_server,
     legacy_tls12_server_tls_client,
     middle_box_tls13_client,
     middle_box_tls12_enabled_client,
     middle_box_client_tls_v2_session_reused,
     renegotiate_error
    ].
legacy_tests() ->
    [tls_client_tls10_server,
     tls_client_tls11_server,
     tls_client_tls12_server,
     tls10_client_tls_server,
     tls11_client_tls_server,
     tls12_client_tls_server].

init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            case ssl_test_lib:sufficient_crypto_support('tlsv1.3') of                
                true ->
                    ssl_test_lib:clean_start(),
                    [{client_type, erlang}, {server_type, erlang} | 
                     Config];
                false ->
                    {skip, "Insufficient crypto support for TLS-1.3"}
            end
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(rsa, Config0) ->
    Config = ssl_test_lib:make_rsa_cert(Config0),
    COpts = proplists:get_value(client_rsa_opts, Config),
    SOpts = proplists:get_value(server_rsa_opts, Config),
    [{client_type, erlang},
     {server_type, erlang},{client_cert_opts, COpts}, {server_cert_opts, SOpts} | 
     lists:delete(server_cert_opts, lists:delete(client_cert_opts, Config))];
init_per_group(ecdsa, Config0) ->
    PKAlg = crypto:supports(public_keys),
    case lists:member(ecdsa, PKAlg) andalso 
        (lists:member(ecdh, PKAlg) orelse lists:member(dh, PKAlg)) of
        true ->
            Config = ssl_test_lib:make_ecdsa_cert(Config0),
            COpts = proplists:get_value(client_ecdsa_opts, Config),
            SOpts = proplists:get_value(server_ecdsa_opts, Config),
            [{client_type, erlang},
             {server_type, erlang},{client_cert_opts, COpts}, {server_cert_opts, SOpts} | 
             lists:delete(server_cert_opts, lists:delete(client_cert_opts, Config))];
        false ->
            {skip, "Missing EC crypto support"}
    end.

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).
%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

tls13_client_tls12_server() ->
    [{doc,"Test that a TLS 1.3 client can connect to a TLS 1.2 server."}].

tls13_client_tls12_server(Config) when is_list(Config) ->
    ClientOpts = [{versions, ['tlsv1.3', 'tlsv1.2']} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.1', 'tlsv1.2']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true}
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).


    
tls13_client_with_ext_tls12_server() ->
     [{doc,"Test basic connection between TLS 1.2 server and TLS 1.3 client when " 
       "client has TLS 1.3 specific extensions"}].

tls13_client_with_ext_tls12_server(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),

    ServerOpts = [{versions, ['tlsv1.2']},
                  {verify, verify_peer}, {fail_if_no_peer_cert, true},
                  {signature_algs, [rsa_pss_rsae_sha256,
                                    {sha256, rsa},
                                    {sha256, ecdsa},
                                    {sha, ecdsa}]}| ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {signature_algs_cert, [ecdsa_secp384r1_sha384,
                                         ecdsa_secp256r1_sha256,
                                         rsa_pss_rsae_sha256,
                                         rsa_pkcs1_sha256,
                                         ecdsa_sha1]}|ClientOpts0],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
   
tls12_client_tls13_server() ->
    [{doc,"Test that a TLS 1.2 client can connect to a TLS 1.3 server."}].

tls12_client_tls13_server(Config) when is_list(Config) ->    
    ClientOpts = [{versions, ['tlsv1.1', 'tlsv1.2']} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.3', 'tlsv1.2']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true}
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls_client_tls10_server() ->
    [{doc,"Test that a TLS 1.0-1.3 client can connect to a TLS 1.0 server."}].
tls_client_tls10_server(Config) when is_list(Config) ->
    CCiphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.3'),
                                        [{key_exchange, fun(srp_rsa)  -> false;
                                                           (srp_anon) -> false;
                                                           (srp_dss) -> false;
                                                           (_) -> true end}]),        
    ClientOpts = [{versions, ['tlsv1', 'tlsv1.1', 'tlsv1.2', 'tlsv1.3']},
                  {ciphers, CCiphers} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true},
                   {ciphers, ssl:cipher_suites(all, 'tlsv1')}
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls_client_tls11_server() ->
    [{doc,"Test that a TLS 1.0-1.3 client can connect to a TLS 1.1 server."}].
tls_client_tls11_server(Config) when is_list(Config) ->
    CCiphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.3'),
                                        [{key_exchange, fun(srp_rsa)  -> false;
                                                           (srp_anon) -> false;
                                                           (srp_dss) -> false;
                                                           (_) -> true end}]),    
    ClientOpts = [{versions,
                   ['tlsv1', 'tlsv1.1', 'tlsv1.2', 'tlsv1.3']},
                  {ciphers, CCiphers} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,['tlsv1.1']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true},
                   {ciphers, ssl:cipher_suites(all, 'tlsv1.1')}  
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls_client_tls12_server() ->
    [{doc,"Test that a TLS 1.0-1.3 client can connect to a TLS 1.2 server."}].
tls_client_tls12_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1', 'tlsv1.1', 'tlsv1.2', 'tlsv1.3']} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.2']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true}
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls10_client_tls_server() ->
    [{doc,"Test that a TLS 1.0 client can connect to a TLS 1.0-1.3 server."}].
tls10_client_tls_server(Config) when is_list(Config) ->
    SCiphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.3'),
                                        [{key_exchange, fun(srp_rsa)  -> false;
                                                           (srp_anon) -> false;
                                                           (srp_dss) -> false;
                                                           (_) -> true end}]),    
    ClientOpts = [{versions, ['tlsv1']},
                  {ciphers, ssl:cipher_suites(all, 'tlsv1')} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1','tlsv1.1', 'tlsv1.2', 'tlsv1.3']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true},
                   {ciphers, SCiphers} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls11_client_tls_server() ->
    [{doc,"Test that a TLS 1.1 client can connect to a TLS 1.0-1.3 server."}].
tls11_client_tls_server(Config) when is_list(Config) ->
    SCiphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.3'),
                                        [{key_exchange, fun(srp_rsa)  -> false;
                                                           (srp_anon) -> false;
                                                           (srp_dss) -> false;
                                                           (_) -> true end}]),
    
    ClientOpts = [{versions, ['tlsv1.1']},
                  {ciphers, ssl:cipher_suites(all, 'tlsv1.1')} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1','tlsv1.1', 'tlsv1.2', 'tlsv1.3']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true},
                   {ciphers, SCiphers} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls12_client_tls_server() ->
    [{doc,"Test that a TLS 1.2 client can connect to a TLS 1.0-1.3 server."}].
tls12_client_tls_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1.2']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1','tlsv1.1', 'tlsv1.2', 'tlsv1.3']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

legacy_tls12_client_tls_server() ->
    [{doc,"Test that a TLS 1.2 client can connect to a TLS 1.3 server."}].

legacy_tls12_client_tls_server(Config) when is_list(Config) ->
    SHA = ssl_test_lib:appropriate_sha(crypto:supports()),
    ClientOpts = [{versions, ['tlsv1.1', 'tlsv1.2']}, {signature_algs, [{SHA, rsa}, {SHA, ecdsa}]} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.3', 'tlsv1.2']},
                   {signature_algs, ssl:signature_algs(default, 'tlsv1.3')},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true}
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

legacy_tls12_server_tls_client() ->
    [{doc,"Test that a TLS 1.3 enabled client can connect to legacy TLS-1.2 server."}].

legacy_tls12_server_tls_client(Config) when is_list(Config) ->
    SHA = sha384,
    Prop = proplists:get_value(tc_group_properties, Config),
    Alg = proplists:get_value(name, Prop),
    #{client_config := ClientOpts0, 
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(Alg, [{server_chain,
                                                                               [[{digest, SHA}],
                                                                                [{digest, SHA}],
                                                                                [{digest, SHA}]]},
                                                                              {client_chain,
                                                                               [[{digest, SHA}],
                                                                                [{digest, SHA}],
                                                                                [{digest, SHA}]]}
                                                                             ]),

    ClientOpts = [{versions, ['tlsv1.3', 'tlsv1.2']} | ClientOpts0],
    ServerOpts =  [{versions, ['tlsv1.2']}, {verify, verify_peer}, {fail_if_no_peer_cert, true},
                   {signature_algs, [{SHA, Alg}]}
                  | ServerOpts0],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

middle_box_tls13_client() ->
    [{doc,"Test that a TLS 1.3 client can connect to a 1.3 server with and without middle box compatible mode."}].
middle_box_tls13_client(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1.3']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.3']} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    middlebox_test(true, not_empty, ClientOpts, ServerOpts, Config),
    middlebox_test(false, empty, ClientOpts, ServerOpts, Config).

middle_box_tls12_enabled_client() ->
    [{doc,"Test that a TLS 1.2 enabled client can connect to a TLS 1.3 server with and without middle box compatible mode."}].
middle_box_tls12_enabled_client(Config) when is_list(Config) ->
    ClientOpts = [{versions, ['tlsv1.2', 'tlsv1.3']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.3']} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    middlebox_test(true, not_empty, ClientOpts, ServerOpts, Config),
    middlebox_test(false, empty, ClientOpts, ServerOpts, Config).

middle_box_client_tls_v2_session_reused() ->
    [{doc, "Test that TLS-1.3 middlebox enabled client can reuse TLS-1.2 session when talking to TLS-1.2 server"}].
middle_box_client_tls_v2_session_reused(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientOpts = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_cert_opts, Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, [{versions, ['tlsv1.2']} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, CSock} = ssl_test_lib:start_client([return_socket, {node, ClientNode}, {port, Port},
                                                  {host, Hostname},
                                                  {from, self()},
                                                  {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                                  {options,
                                                   [{versions, ['tlsv1.2']}, {reuse_sessions, save}| ClientOpts]}]),
    Server ! listen,
    {ok,[{session_id, SessionId}, {session_data, SessData}]} = ssl:connection_information(CSock, [session_id, session_data]),
    {_Client1, CSock1}  = ssl_test_lib:start_client([return_socket,
                                                     {node, ClientNode}, {port, Port},
                                                     {host, Hostname},
                                                     {from, self()},
                                                     {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                                     {options,
                                                      [{versions, ['tlsv1.3', 'tlsv1.2']},
                                                       {middlebox_comp_mode, true},
                                                       {reuse_session, {SessionId, SessData}} | ClientOpts]}]),
    {ok,[{session_id, SessionId}]}  = ssl:connection_information(CSock1, [session_id]).

renegotiate_error() ->
    [{doc, "Test that an error is returned when ssl:renegotiate/1 is called on a connection running TLS-1.3"}].
renegotiate_error(Config) when is_list(Config) ->
    {_ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientOpts = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_cert_opts, Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, [{versions, ['tlsv1.3']} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Options = [{versions, ['tlsv1.3']} | ClientOpts],
    case ssl:connect(Hostname, Port, Options) of
        {ok, Socket} ->
            {error, notsup} = ssl:renegotiate(Socket);
        {error, Reason} ->
            ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% Internal functions and callbacks -----------------------------------
%%--------------------------------------------------------------------

middlebox_test(Mode, Expected, ClientOpts, ServerOpts, Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {?MODULE, check_session_id, [Expected]}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE, check_session_id, [Expected]}},
                                        {options,
                                         [{middlebox_comp_mode, Mode}| ClientOpts]}]),
    ssl_test_lib:check_result(Server, ok, Client, ok).

check_session_id(Socket, Expected) ->
   {ok, [{session_id, SessionId}]} = ssl:connection_information(Socket, [session_id]),
    case {Expected, SessionId} of
        {empty, <<>>} ->
            ok;
        {not_empty, SessionId} when SessionId =/= <<>> ->
            ok;
        _ ->
            {nok, {{expected, Expected}, {got, SessionId}}}
    end.


