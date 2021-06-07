%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2015-2020. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%

-module(ssl_eqc_SUITE).

-behaviour(ct_suite).

%% Common test
-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_suite/1,
         end_per_testcase/2
        ]).

%% Test cases
-export([tls_handshake_encoding/1,
         tls_cipher_suite_names/1,
         tls_cipher_openssl_suite_names/1,
         tls_anon_cipher_suite_names/1,
         tls_anon_cipher_openssl_suite_names/1,
         tls_unorded_chains/1,
         tls_extraneous_chain/1,
         tls_extraneous_chains/1,
         tls_extraneous_and_unorder_chains/1,
         tls_client_cert_auth/1
         ]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() -> 
    [
     tls_handshake_encoding,
     tls_cipher_suite_names,
     tls_cipher_openssl_suite_names,
     tls_anon_cipher_suite_names,
     tls_anon_cipher_openssl_suite_names,
     tls_unorded_chains,
     tls_extraneous_chain,
     tls_extraneous_chains,
     tls_extraneous_and_unorder_chains,
     tls_client_cert_auth
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).
end_per_suite(Config) ->
    Config.

init_per_testcase(_, Config0) ->
    Config0.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

tls_handshake_encoding(Config) when is_list(Config) ->
    %% manual test:  proper:quickcheck(ssl_eqc_handshake:prop_tls_hs_encode_decode()).
    true =  ct_property_test:quickcheck(ssl_eqc_handshake:prop_tls_hs_encode_decode(),
                                        Config).

tls_cipher_suite_names(Config) when is_list(Config) ->
    %% manual test:  proper:quickcheck(ssl_eqc_cipher_format:prop_tls_cipher_suite_rfc_name()).
    true =  ct_property_test:quickcheck(ssl_eqc_cipher_format:prop_tls_cipher_suite_rfc_name(),
                                        Config).

tls_cipher_openssl_suite_names(Config) when is_list(Config) ->
    %% manual test:  proper:quickcheck(ssl_eqc_handshake:prop_tls_cipher_suite_openssl_name()).
    true =  ct_property_test:quickcheck(ssl_eqc_cipher_format:prop_tls_cipher_suite_openssl_name(),
                                        Config).
tls_anon_cipher_suite_names(Config) when is_list(Config) ->
    %% manual test:  proper:quickcheck(ssl_eqc_cipher_format:prop_tls_cipher_suite_rfc_name()).
    true =  ct_property_test:quickcheck(ssl_eqc_cipher_format:prop_tls_anon_cipher_suite_rfc_name(),
                                        Config).

tls_anon_cipher_openssl_suite_names(Config) when is_list(Config) ->
    %% manual test:  proper:quickcheck(ssl_eqc_handshake:prop_tls_cipher_suite_openssl_name()).
    true =  ct_property_test:quickcheck(ssl_eqc_cipher_format:prop_tls_anon_cipher_suite_openssl_name(),
                                        Config).

tls_unorded_chains(Config) when is_list(Config) ->
    %% manual test:  proper:quickcheck(ssl_eqc_chain:prop_tls_ordered_path("/tmp")
    ssl:start(),
    PrivDir = proplists:get_value(priv_dir, Config),
    true =  ct_property_test:quickcheck(ssl_eqc_chain:prop_tls_unordered_path(PrivDir),
                                        Config).

tls_extraneous_chain(Config) when is_list(Config) ->
    %% manual test:  proper:quickcheck(ssl_eqc_chain:prop_tls_ordered_path("/tmp")
    ssl:start(),
    PrivDir = proplists:get_value(priv_dir, Config),
    true = ct_property_test:quickcheck(ssl_eqc_chain:prop_tls_extraneous_path(PrivDir),
                                        Config).

tls_extraneous_chains(Config) when is_list(Config) ->
    %% manual test:  proper:quickcheck(ssl_eqc_chain:prop_tls_ordered_path()
    ssl:start(),
    true = ct_property_test:quickcheck(ssl_eqc_chain:prop_tls_extraneous_paths(),
                                       Config).
tls_extraneous_and_unorder_chains(Config) when is_list(Config) ->
    %% manual test:  proper:quickcheck(ssl_eqc_chain:prop_tls_ordered_path()
    ssl:start(),
    true = ct_property_test:quickcheck(ssl_eqc_chain:prop_tls_extraneous_and_unordered_path(),
                                       Config).

tls_client_cert_auth(Config) when is_list(Config) ->
    %% manual test:  proper:quickcheck(ssl_eqc_chain:prop_client_cert_auth()
    ssl:start(),
    true = ct_property_test:quickcheck(ssl_eqc_chain:prop_client_cert_auth(),
                                       Config).
