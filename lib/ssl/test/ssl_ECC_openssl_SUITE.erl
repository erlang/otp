%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-module(ssl_ECC_openssl_SUITE).

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
     {group, 'tlsv1'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'tlsv1.2', [], test_cases()},
     {'tlsv1.1', [], test_cases()},
     {'tlsv1', [], test_cases()},
     {'dtlsv1.2', [], test_cases()},
     {'dtlsv1', [], test_cases()}     
    ].

test_cases()->
   %% cert_combinations().
    server_ecdh_rsa(). 
cert_combinations() ->
     lists:append(lists:filtermap(fun({Name, Suites}) -> 
                             case ssl_test_lib:openssl_filter(Name) of
                                [] ->
                                     false;
                                [_|_] ->
                                     {true, Suites}
                             end
                    end, [{"ECDH-RSA", server_ecdh_rsa()},
                           {"ECDHE-RSA", server_ecdhe_rsa()},
                          {"ECDH-ECDSA", server_ecdh_ecdsa()},
                          {"ECDHE-ECDSA", server_ecdhe_ecdsa()}
                         ])).
server_ecdh_rsa() ->
    [client_ecdh_rsa_server_ecdh_rsa,
     client_ecdhe_rsa_server_ecdh_rsa,     
     client_ecdhe_ecdsa_server_ecdh_rsa].

server_ecdhe_rsa() ->
    [client_ecdh_rsa_server_ecdhe_rsa,
     client_ecdhe_rsa_server_ecdhe_rsa,
     client_ecdhe_ecdsa_server_ecdhe_rsa].

server_ecdh_ecdsa() ->
    [client_ecdh_ecdsa_server_ecdh_ecdsa,
     client_ecdhe_rsa_server_ecdh_ecdsa,
     client_ecdhe_ecdsa_server_ecdh_ecdsa].

server_ecdhe_ecdsa() ->
    [client_ecdh_rsa_server_ecdhe_ecdsa,
     client_ecdh_ecdsa_server_ecdhe_ecdsa,
     client_ecdhe_ecdsa_server_ecdhe_ecdsa].

%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    end_per_suite(Config0),
    try crypto:start() of
	ok ->
            case ssl_test_lib:sufficient_crypto_support(cipher_ec) of
                true ->
                    Config0;
                false ->
                    {skip, "Crypto does not support ECC"}
            end
    catch _:_ ->
            {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(ssl),
    application:stop(crypto).

%%--------------------------------------------------------------------
init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
	true ->
            case ssl_test_lib:check_sane_openssl_version(GroupName) of
                true ->
                    [{tls_version, GroupName},
                     {server_type, erlang},
                     {client_type, openssl} | ssl_test_lib:init_tls_version(GroupName, Config)];
                false ->
                    {skip, openssl_does_not_support_version}
            end;
        _ ->
            Config
    end.

end_per_group(GroupName, Config0) ->
  case ssl_test_lib:is_tls_version(GroupName) of
      true ->
          Config = ssl_test_lib:clean_tls_version(Config0),
          proplists:delete(tls_version, Config);
      false ->
          Config0
  end.

%%--------------------------------------------------------------------

init_per_testcase(TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    Version = proplists:get_value(tls_version, Config),
    ct:log("Ciphers: ~p~n ", [ssl:cipher_suites(default, Version)]),
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

%% Test diffrent certificate chain types, note that it is the servers
%% chain that affect what cipher suit that will be choosen

%% ECDH_RSA 
client_ecdh_rsa_server_ecdh_rsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdh_rsa_server_ecdh_rsa(Config).
client_ecdhe_rsa_server_ecdh_rsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdhe_rsa_server_ecdh_rsa(Config).
client_ecdhe_ecdsa_server_ecdh_rsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdhe_ecdsa_server_ecdh_rsa(Config).
%% ECDHE_RSA    
client_ecdh_rsa_server_ecdhe_rsa(Config)  when is_list(Config) ->
    ssl_ECC:client_ecdh_rsa_server_ecdhe_rsa(Config).
client_ecdhe_rsa_server_ecdhe_rsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdhe_rsa_server_ecdhe_rsa(Config).
client_ecdhe_ecdsa_server_ecdhe_rsa(Config) when is_list(Config) ->
   ssl_ECC:client_ecdhe_ecdsa_server_ecdhe_rsa(Config).
%% ECDH_ECDSA
client_ecdh_ecdsa_server_ecdh_ecdsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdh_ecdsa_server_ecdh_ecdsa(Config).
client_ecdhe_rsa_server_ecdh_ecdsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdhe_rsa_server_ecdh_ecdsa(Config).
client_ecdhe_ecdsa_server_ecdh_ecdsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdhe_ecdsa_server_ecdh_ecdsa(Config).
%% ECDHE_ECDSA
client_ecdh_rsa_server_ecdhe_ecdsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdh_rsa_server_ecdhe_ecdsa(Config).
client_ecdh_ecdsa_server_ecdhe_ecdsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdh_ecdsa_server_ecdhe_ecdsa(Config).
client_ecdhe_ecdsa_server_ecdhe_ecdsa(Config) when is_list(Config) ->
     ssl_ECC:client_ecdhe_ecdsa_server_ecdhe_ecdsa(Config).
%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
