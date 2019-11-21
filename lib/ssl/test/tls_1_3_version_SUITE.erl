%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [
     {group, 'tlsv1.3'}
    ].

groups() ->
    [
     {'tlsv1.3', [], cert_groups()},
     {rsa, [], tests()},
     {ecdsa, [], tests()}
    ].

cert_groups() ->
    [{group, rsa},
     {group, ecdsa}].

tests() ->
    [tls13_client_tls12_server,
     tls13_client_with_ext_tls12_server,
     tls12_client_tls13_server,
     tls_client_tls10_server,
     tls_client_tls11_server,
     tls_client_tls12_server,
     tls10_client_tls_server,
     tls11_client_tls_server,
     tls12_client_tls_server].

init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            ssl_test_lib:clean_start(),
	    [{client_type, erlang}, {server_type, erlang} | 
             Config]
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
    [{client_cert_opts, COpts}, {server_cert_opts, SOpts} | 
     lists:delete(server_cert_opts, lists:delete(client_cert_opts, Config))];
init_per_group(ecdsa, Config0) ->
    PKAlg = crypto:supports(public_keys),
    case lists:member(ecdsa, PKAlg) andalso 
        (lists:member(ecdh, PKAlg) orelse lists:member(dh, PKAlg)) of
        true ->
            Config = ssl_test_lib:make_ecdsa_cert(Config0),
            COpts = proplists:get_value(client_ecdsa_opts, Config),
            SOpts = proplists:get_value(server_ecdsa_opts, Config),
            [{client_cert_opts, COpts}, {server_cert_opts, SOpts} | 
             lists:delete(server_cert_opts, lists:delete(client_cert_opts, Config))];
        false ->
            {skip, "Missing EC crypto support"}
    end;
init_per_group(GroupName, Config) ->
    ssl_test_lib:clean_tls_version(Config),                          
    case ssl_test_lib:is_tls_version(GroupName) andalso 
        ssl_test_lib:sufficient_crypto_support(GroupName) of
	true ->
	    ssl_test_lib:init_tls_version(GroupName, Config);
	_ ->
	    case ssl_test_lib:sufficient_crypto_support(GroupName) of
		true ->
		    ssl:start(),
		    Config;
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

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

tls13_client_tls12_server() ->
    [{doc,"Test that a TLS 1.3 client can connect to a TLS 1.2 server."}].

tls13_client_tls12_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1.3', 'tlsv1.2']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1.1', 'tlsv1.2']} | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
    
tls13_client_with_ext_tls12_server() ->
     [{doc,"Test basic connection between TLS 1.2 server and TLS 1.3 client when " 
       "client has TLS 1.3 specsific extensions"}].

tls13_client_with_ext_tls12_server(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
  
    ServerOpts = [{versions, ['tlsv1.2']}|ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {signature_algs_cert, [ecdsa_secp384r1_sha384,
                                         ecdsa_secp256r1_sha256,
                                         rsa_pss_rsae_sha256,
                                         rsa_pkcs1_sha256,
                                         {sha256,rsa},{sha256,dsa}]}|ClientOpts0],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
   
tls12_client_tls13_server() ->
    [{doc,"Test that a TLS 1.2 client can connect to a TLS 1.3 server."}].

tls12_client_tls13_server(Config) when is_list(Config) ->    
    ClientOpts = [{versions,
                   ['tlsv1.1', 'tlsv1.2']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1.3', 'tlsv1.2']} | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls_client_tls10_server() ->
    [{doc,"Test that a TLS 1.0-1.3 client can connect to a TLS 1.0 server."}].
tls_client_tls10_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1', 'tlsv1.1', 'tlsv1.2', 'tlsv1.3']} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1']} | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls_client_tls11_server() ->
    [{doc,"Test that a TLS 1.0-1.3 client can connect to a TLS 1.1 server."}].
tls_client_tls11_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1', 'tlsv1.1', 'tlsv1.2', 'tlsv1.3']} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1.1']} | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls_client_tls12_server() ->
    [{doc,"Test that a TLS 1.0-1.3 client can connect to a TLS 1.2 server."}].
tls_client_tls12_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1', 'tlsv1.1', 'tlsv1.2', 'tlsv1.3']} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1.2']} | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls10_client_tls_server() ->
    [{doc,"Test that a TLS 1.0 client can connect to a TLS 1.0-1.3 server."}].
tls10_client_tls_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1','tlsv1.1', 'tlsv1.2', 'tlsv1.3']} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls11_client_tls_server() ->
    [{doc,"Test that a TLS 1.1 client can connect to a TLS 1.0-1.3 server."}].
tls11_client_tls_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1.1']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1','tlsv1.1', 'tlsv1.2', 'tlsv1.3']} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls12_client_tls_server() ->
    [{doc,"Test that a TLS 1.2 client can connect to a TLS 1.0-1.3 server."}].
tls12_client_tls_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1.2']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1','tlsv1.1', 'tlsv1.2', 'tlsv1.3']} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
