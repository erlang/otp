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
    case ssl_test_lib:openssl_sane_dtls() of 
        true ->
            [{group, 'tlsv1.2'},
             {group, 'dtlsv1.2'}];   
        false ->
            [{group, 'tlsv1.2'}]
    end.

groups() ->
    case ssl_test_lib:openssl_sane_dtls() of 
        true ->
            [{'tlsv1.2', [], [mix_sign]},
             {'dtlsv1.2', [],  [mix_sign]}];
        false ->
            [{'tlsv1.2', [], [mix_sign]}]
    end.
  
%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    end_per_suite(Config0),
    try crypto:start() of
	ok ->
            case  ssl_test_lib:sufficient_crypto_support(cipher_ec) of
                true ->
                    Config0;
                false ->
                    {skip, "Openssl does not support ECC"}
            end
    catch _:_ ->
            {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(ssl),
    application:stop(crypto),
    ssl_test_lib:kill_openssl().

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
init_per_testcase(skip, Config) ->
    Config;
init_per_testcase(TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    Version = proplists:get_value(tls_version, Config),
    ct:log("Ciphers: ~p~n ", [ssl:cipher_suites(default, Version)]),
    end_per_testcase(TestCase, Config),
    ssl:start(),
    ct:timetrap({seconds, 30}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    application:stop(ssl),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

skip(Config) when is_list(Config) ->
    {skip, openssl_does_not_support_ECC}.

mix_sign(Config) ->
    {COpts0, SOpts0} = ssl_test_lib:make_mix_cert(Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECDHE_ECDSA =
        ssl:filter_cipher_suites(ssl:cipher_suites(default, 'tlsv1.2'), 
                                 [{key_exchange, fun(ecdhe_ecdsa) -> true; (_) -> false end}]),
    ssl_test_lib:basic_test(COpts, [{ciphers, ECDHE_ECDSA} | SOpts], [{client_type, erlang},
                                                                      {server_type, openssl} | Config]).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
