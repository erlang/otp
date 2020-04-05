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

-module(openssl_tls_1_3_version_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [
     %%{group,  openssl_server},
     {group,  openssl_client}
    ].

groups() ->
    [
     %%{openssl_server, [{group, 'tlsv1.3'}]},
     {openssl_client, [{group, 'tlsv1.3'}]},
     {'tlsv1.3', [], cert_groups()},
     {rsa, [], tests()},
     {ecdsa, [], tests()}
    ].

cert_groups() ->
    [{group, rsa},
     {group, ecdsa}].

tests() ->
    [%%tls13_client_tls12_server, %% Not testable with current openssl s_client
     %%tls13_client_with_ext_tls12_server,
     tls12_client_tls13_server].
    
init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            case ssl_test_lib:check_sane_openssl_version('tlsv1.3') of
                true ->
                    ssl_test_lib:clean_start(),
                    Config;
                false ->
                    {skip, openssl_does_not_support_version}
            end
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(openssl_client, Config0) ->
    Config = proplists:delete(server_type, proplists:delete(client_type, Config0)),
    [{client_type, openssl}, {server_type, erlang} | Config];
init_per_group(openssl_server, Config0) ->
    Config = proplists:delete(server_type, proplists:delete(client_type, Config0)),
    [{client_type, erlang}, {server_type, openssl} | Config];    
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

%% openssl s_client cannot be configured to support both TLS 1.3 and TLS 1.2.
%% In its ClientHello the supported_versions extension contains only one element
%% [{3,4}] that the server does not accept if it is configured to not support
%% TLS 1.3.
tls13_client_tls12_server() ->
    [{doc,"Test that a TLS 1.3 client can connect to a TLS 1.2 server."}].

tls13_client_tls12_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1.3', 'tlsv1.2']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1.1', 'tlsv1.2']} | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
    
%% tls13_client_with_ext_tls12_server() ->
%%      [{doc,"Test basic connection between TLS 1.2 server and TLS 1.3 client when " 
%%        "client has TLS 1.3 specsific extensions"}].

%% tls13_client_with_ext_tls12_server(Config) ->
%%     ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
%%     ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
  
%%     {ServerOpts, ClientOpts} =
%%         case proplists:get_value(client_type) of
%%             erlang ->
%%                 {[{versions, ['tlsv1.2']}|ServerOpts0],
%%                  [{versions, ['tlsv1.2','tlsv1.3']},
%%                   {signature_algs_cert, [ecdsa_secp384r1_sha384,
%%                                          ecdsa_secp256r1_sha256,
%%                                          rsa_pss_rsae_sha256,
%%                                          rsa_pkcs1_sha256,
%%                                          {sha256,rsa},{sha256,dsa}]}|ClientOpts0]};
%%             openssl ->
                
            
%%     ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).


%% TODO: wrong version of TLS is configured for the client
tls12_client_tls13_server() ->
    [{doc,"Test that a TLS 1.2 client can connect to a TLS 1.3 server."}].

tls12_client_tls13_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1.1', 'tlsv1.2']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1.3', 'tlsv1.2']} | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
   
