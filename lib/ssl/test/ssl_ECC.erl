
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

-module(ssl_ECC).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Test diffrent certificate chain types, note that it is the servers
%% chain that affect what cipher suit that will be choosen

%% ECDH_RSA 
client_ecdh_rsa_server_ecdh_rsa(Config) when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [keyAgreement]}]),
    Suites = all_rsa_suites(Config),
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain,
                                                        [[], [], [{extensions, Ext}]]},
                                                       {client_chain, Default}],
                                                      ecdh_rsa, ecdh_rsa, Config),
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
               ssl_test_lib:ssl_options(SOpts, Config),
               [{check_keyex, ecdh_rsa}, {ciphers, Suites} | proplists:delete(check_keyex, Config)]).
client_ecdhe_rsa_server_ecdh_rsa(Config)  when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [keyAgreement]}]),
    Suites = all_rsa_suites(Config),
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain,
                                                        [[], [], [{extensions, Ext}]]},
                                                       {client_chain, Default}], 
                                                      ecdhe_rsa, ecdh_rsa, Config),
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
               ssl_test_lib:ssl_options(SOpts, Config),  
               [{check_keyex, ecdh_rsa}, {ciphers, Suites} | proplists:delete(check_keyex, Config)]).
client_ecdhe_ecdsa_server_ecdh_rsa(Config)  when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [keyAgreement]}]),
    Suites = all_rsa_suites(Config),
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain,
                                                        [[], [], [{extensions, Ext}]]},
                                                       {client_chain, Default}],
                                                      ecdhe_ecdsa, ecdh_rsa, Config),
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
                            ssl_test_lib:ssl_options(SOpts, Config),
                            [{check_keyex, ecdh_rsa}, {ciphers, Suites} | proplists:delete(check_keyex, Config)]).

%% ECDHE_RSA    
client_ecdh_rsa_server_ecdhe_rsa(Config)  when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [digitalSignature]}]),
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain,
                                                        [[], [], [{extensions, Ext}]]},
                                                       {client_chain, Default}], 
                                                      ecdh_rsa, ecdhe_rsa, Config),
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
               ssl_test_lib:ssl_options(SOpts, Config), 
               [{check_keyex, ecdhe_rsa} | proplists:delete(check_keyex, Config)]).
client_ecdhe_rsa_server_ecdhe_rsa(Config)  when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [digitalSignature]}]),
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain,
                                                        [[], [], [{extensions, Ext}]]},
                                                       {client_chain, Default}], 
                                                      ecdhe_rsa, ecdhe_rsa, Config),
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
                            ssl_test_lib:ssl_options(SOpts, Config),
               [{check_keyex, ecdhe_rsa} | proplists:delete(check_keyex, Config)]).
client_ecdhe_ecdsa_server_ecdhe_rsa(Config)  when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [digitalSignature]}]),
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain,
                                                        [[], [], [{extensions, Ext}]]},
                                                       {client_chain, Default}],
                                                      ecdh_ecdsa, ecdhe_rsa, Config),
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
               ssl_test_lib:ssl_options(SOpts, Config),
               [{check_keyex, ecdhe_rsa} | proplists:delete(check_keyex, Config)]).

%% ECDH_ECDSA
client_ecdh_ecdsa_server_ecdh_ecdsa(Config)  when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [keyAgreement]}]),
    {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain, 
                                                        [[], [], [{extensions, Ext}]]},
                                                       {client_chain,
                                                         ssl_test_lib:default_cert_chain_conf()}],
                                                      ecdh_ecdsa, ecdh_ecdsa, Config),
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
               ssl_test_lib:ssl_options(SOpts, Config),
               [{check_keyex, ecdh_ecdsa} | proplists:delete(check_keyex, Config)]).
client_ecdhe_rsa_server_ecdh_ecdsa(Config)  when is_list(Config) ->
     Ext = x509_test:extensions([{key_usage, [keyAgreement]}]),
     {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain, 
                                                         [[], [], [{extensions, Ext}]]},
                                                        {client_chain,
                                                         ssl_test_lib:default_cert_chain_conf()}],
                                                       ecdhe_rsa, ecdh_ecdsa, Config),
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
               ssl_test_lib:ssl_options(SOpts, Config),
               [{check_keyex, ecdh_ecdsa} | proplists:delete(check_keyex, Config)]).

client_ecdhe_ecdsa_server_ecdh_ecdsa(Config)  when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [keyAgreement]}]),
    {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain, 
                                                        [[], [], [{extensions, Ext}]]},
                                                       {client_chain,
                                                        ssl_test_lib:default_cert_chain_conf()}],
                                                       ecdhe_ecdsa, ecdh_ecdsa, Config),
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
               ssl_test_lib:ssl_options(SOpts, Config),
               [{check_keyex, ecdh_ecdsa} | proplists:delete(check_keyex, Config)]).

%% ECDHE_ECDSA
client_ecdh_rsa_server_ecdhe_ecdsa(Config)  when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [digitalSignature]}]),
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain,
                                                        [[], [], [{extensions, Ext}]]},
                                                       {client_chain, Default}], 
                                                      ecdh_rsa, ecdhe_ecdsa, Config), 
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
               ssl_test_lib:ssl_options(SOpts, Config), 
               [{check_keyex, ecdhe_ecdsa} | proplists:delete(check_keyex, Config)]).
client_ecdh_ecdsa_server_ecdhe_ecdsa(Config)  when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [digitalSignature]}]),
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain,
                                                        [[], [], [{extensions, Ext}]]},
                                                       {client_chain, Default}], 
                                                      ecdh_ecdsa, ecdhe_ecdsa, Config), 
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
               ssl_test_lib:ssl_options(SOpts, Config),
               [{check_keyex, ecdhe_ecdsa} | proplists:delete(check_keyex, Config)]).
client_ecdhe_ecdsa_server_ecdhe_ecdsa(Config)  when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [digitalSignature]}]),
    Default = ssl_test_lib:default_cert_chain_conf(),
     {COpts, SOpts} = ssl_test_lib:make_ec_cert_chains([{server_chain,
                                                         [[], [], [{extensions, Ext}]]},
                                                        {client_chain, Default}], 
                                                       ecdhe_ecdsa, ecdhe_ecdsa, Config),
    ssl_test_lib:basic_test(ssl_test_lib:ssl_options(COpts, Config), 
               ssl_test_lib:ssl_options(SOpts, Config),
               [{check_keyex, ecdhe_ecdsa} | proplists:delete(check_keyex, Config)]).

all_rsa_suites(Config) ->
    Version = proplists:get_value(tls_version, Config),
    All = ssl:cipher_suites(all, Version),
    Default = ssl:cipher_suites(default, Version),
    RSASuites = ssl:filter_cipher_suites(All,[{key_exchange, fun(rsa) -> true;(_) -> false end}]),
    ssl:append_cipher_suites(RSASuites, Default).
