%
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
%%----------------------------------------------------------------------
%% Purpose: Convert between diffrent cipher suite representations
%% 
%%----------------------------------------------------------------------
-module(ssl_cipher_format).

-include("ssl_api.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include_lib("public_key/include/public_key.hrl").

-export_type([old_erl_cipher_suite/0, openssl_cipher_suite/0, cipher_suite/0]).

-type internal_cipher()            :: null | ssl:cipher().
-type internal_hash()              :: null | ssl:hash().
-type internal_kex_algo()          :: null | ssl:kex_algo().
-type internal_erl_cipher_suite()  :: #{key_exchange := internal_kex_algo(),
                               cipher := internal_cipher(),
                               mac    := internal_hash() | aead,
                               prf    := internal_hash() | default_prf %% Old cipher suites, version dependent
                              }.  
-type old_erl_cipher_suite() :: {ssl:kex_algo(), internal_cipher(), internal_hash()} % Pre TLS 1.2 
                                %% TLS 1.2, internally PRE TLS 1.2 will use default_prf
                              | {ssl:kex_algo(), internal_cipher(), internal_hash(), 
                                 internal_hash() | default_prf}. 
-type cipher_suite()      :: binary().
-type openssl_cipher_suite()  :: string().


-export([suite_map_to_bin/1,            %% Binary format  
         suite_bin_to_map/1, %% Erlang API format
         suite_map_to_str/1,     %% RFC string
         suite_str_to_map/1,
         suite_map_to_openssl_str/1, %% OpenSSL name
         suite_openssl_str_to_map/1,
         suite_legacy/1 %% Erlang legacy format
        ]).

%%--------------------------------------------------------------------
-spec suite_map_to_str(internal_erl_cipher_suite()) -> string().
%%
%% Description: Return the string representation of a cipher suite.
%%--------------------------------------------------------------------
suite_map_to_str(#{key_exchange := null,
               cipher := null,
               mac := null,
               prf := null}) ->
    "TLS_EMPTY_RENEGOTIATION_INFO_SCSV";
suite_map_to_str(#{key_exchange := any,
               cipher := Cipher,
               mac := aead,
               prf := PRF}) ->
    "TLS_" ++ string:to_upper(atom_to_list(Cipher)) ++
        "_" ++ string:to_upper(atom_to_list(PRF));
suite_map_to_str(#{key_exchange := Kex,
               cipher := Cipher,
               mac := aead,
               prf := PRF}) ->
    "TLS_" ++ string:to_upper(atom_to_list(Kex)) ++
        "_WITH_" ++  string:to_upper(atom_to_list(Cipher)) ++
        "_" ++ string:to_upper(atom_to_list(PRF));
suite_map_to_str(#{key_exchange := Kex,
               cipher := Cipher,
               mac := Mac}) ->
    "TLS_" ++ string:to_upper(atom_to_list(Kex)) ++
        "_WITH_" ++  string:to_upper(atom_to_list(Cipher)) ++
        "_" ++ string:to_upper(atom_to_list(Mac)).

suite_str_to_map("TLS_EMPTY_RENEGOTIATION_INFO_SCSV") ->
    #{key_exchange => null,
      cipher => null,
      mac => null,
      prf => null};
suite_str_to_map(SuiteStr)->
    Str0 = string:prefix(SuiteStr, "TLS_"),
    case string:split(Str0, "_WITH_") of
        [Rest] ->
            tls_1_3_suite_str_to_map(Rest);
        [Prefix, Kex | Rest] when Prefix == "SPR"; 
                                  Prefix == "PSK";                                  
                                  Prefix == "DHE"; 
                                  Prefix == "ECDHE" 
                                  ->
            pre_tls_1_3_suite_str_to_map(Prefix ++ "_" ++ Kex, Rest);
        [Kex| Rest] ->
            pre_tls_1_3_suite_str_to_map(Kex, Rest)
    end.

suite_map_to_openssl_str(#{key_exchange := any,
                       mac := aead} = Suite) ->
    %% TLS 1.3 OpenSSL finally use RFC names
    suite_map_to_str(Suite);
suite_map_to_openssl_str(#{key_exchange := null} = Suite) ->
    %% TLS_EMPTY_RENEGOTIATION_INFO_SCSV
    suite_map_to_str(Suite);
suite_map_to_openssl_str(#{key_exchange := rsa = Kex,
                           cipher := Cipher,
                           mac := Mac}) when Cipher == "des_cbc";
                                             Cipher == "3des_ede_cbc" ->
    openssl_cipher_name(Kex, string:to_upper(atom_to_list(Cipher))) ++
        "-" ++ string:to_upper(atom_to_list(Mac));
suite_map_to_openssl_str(#{key_exchange := Kex,
                           cipher := chacha20_poly1305 = Cipher,
                           mac := aead}) ->
    openssl_suite_start(string:to_upper(atom_to_list(Kex))) 
        ++  openssl_cipher_name(Kex, string:to_upper(atom_to_list(Cipher)));
suite_map_to_openssl_str(#{key_exchange := Kex,
                       cipher := Cipher,
                       mac := aead,
                       prf := PRF}) ->
    openssl_suite_start(string:to_upper(atom_to_list(Kex))) 
        ++  openssl_cipher_name(Kex, string:to_upper(atom_to_list(Cipher))) ++
        "-" ++ string:to_upper(atom_to_list(PRF));
suite_map_to_openssl_str(#{key_exchange := Kex,
                       cipher := Cipher,
                       mac := Mac}) ->
    openssl_suite_start(string:to_upper(atom_to_list(Kex))) 
        ++  openssl_cipher_name(Kex, string:to_upper(atom_to_list(Cipher))) ++
        "-" ++ string:to_upper(atom_to_list(Mac)).


suite_openssl_str_to_map("TLS_" ++ _ = SuiteStr) ->
    suite_str_to_map(SuiteStr);
suite_openssl_str_to_map("DES-CBC-SHA") ->
    suite_str_to_map("TLS_RSA_WITH_DES_CBC_SHA");
suite_openssl_str_to_map("DES-CBC3-SHA") ->
    suite_str_to_map("TLS_RSA_WITH_3DES_EDE_CBC_SHA");
suite_openssl_str_to_map("SRP-DSS-DES-CBC3-SHA") ->
    suite_str_to_map("TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA");
suite_openssl_str_to_map("DHE-RSA-" ++ Rest) ->
    suite_openssl_str_to_map("DHE-RSA", Rest);
suite_openssl_str_to_map("DHE-DSS-" ++ Rest) ->
    suite_openssl_str_to_map("DHE-DSS", Rest);
suite_openssl_str_to_map("EDH-RSA-" ++ Rest) ->
    suite_openssl_str_to_map("DHE-RSA", Rest);
suite_openssl_str_to_map("EDH-DSS-" ++ Rest) ->
    suite_openssl_str_to_map("DHE-DSS", Rest);
suite_openssl_str_to_map("DES" ++ _ = Rest) ->
    suite_openssl_str_to_map("RSA", Rest);
suite_openssl_str_to_map("AES" ++ _ = Rest) ->
    suite_openssl_str_to_map("RSA", Rest);
suite_openssl_str_to_map("RC4" ++ _ = Rest) ->
    suite_openssl_str_to_map("RSA", Rest);
suite_openssl_str_to_map("ECDH-RSA-" ++ Rest) ->
    suite_openssl_str_to_map("ECDH-RSA", Rest);
suite_openssl_str_to_map("ECDH-ECDSA-" ++ Rest) ->
    suite_openssl_str_to_map("ECDH-ECDSA", Rest);
suite_openssl_str_to_map("ECDHE-RSA-" ++ Rest) ->
    suite_openssl_str_to_map("ECDHE-RSA", Rest);
suite_openssl_str_to_map("ECDHE-ECDSA-" ++ Rest) ->
    suite_openssl_str_to_map("ECDHE-ECDSA", Rest);
suite_openssl_str_to_map("RSA-PSK-" ++ Rest) ->
    suite_openssl_str_to_map("RSA-PSK", Rest);
suite_openssl_str_to_map("RSA-" ++ Rest) ->
    suite_openssl_str_to_map("RSA", Rest);
suite_openssl_str_to_map("DHE-PSK-" ++ Rest) ->
    suite_openssl_str_to_map("DHE-PSK", Rest);
suite_openssl_str_to_map("ECDHE-PSK-" ++ Rest) ->
    suite_openssl_str_to_map("ECDHE-PSK", Rest);
suite_openssl_str_to_map("PSK-" ++ Rest) ->
    suite_openssl_str_to_map("PSK", Rest);
suite_openssl_str_to_map("SRP-RSA-" ++ Rest) ->
    suite_openssl_str_to_map("SRP-RSA", Rest);
suite_openssl_str_to_map("SRP-DSS-" ++ Rest) ->
    suite_openssl_str_to_map("SRP-DSS", Rest);
suite_openssl_str_to_map("SRP-" ++ Rest) ->
    suite_openssl_str_to_map("SRP", Rest).

%%--------------------------------------------------------------------
-spec suite_bin_to_map(cipher_suite()) -> internal_erl_cipher_suite().
%%
%% Description: Return erlang cipher suite definition.
%% Note: Currently not supported suites are commented away.
%% They should be supported or removed in the future.
%%-------------------------------------------------------------------
%% TLS v1.1 suites
suite_bin_to_map(?TLS_NULL_WITH_NULL_NULL) ->
    #{key_exchange => null,
      cipher => null, 
      mac => null, 
      prf => null};
%% RFC 5746 - Not a real cipher suite used to signal empty "renegotiation_info" extension
%% to avoid handshake failure from old servers that do not ignore
%% hello extension data as they should.
suite_bin_to_map(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV) ->
    #{key_exchange => null,
      cipher => null, 
      mac => null,
      prf => null};
suite_bin_to_map(?TLS_RSA_WITH_RC4_128_MD5) ->	
    #{key_exchange => rsa,
      cipher => rc4_128, 
      mac => md5, 
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_WITH_RC4_128_SHA) ->
    #{key_exchange => rsa,
      cipher => rc4_128,
      mac => sha,
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_WITH_DES_CBC_SHA) ->
    #{key_exchange => rsa,
      cipher => des_cbc,
      mac => sha,
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => rsa,
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_DSS_WITH_DES_CBC_SHA) ->
    #{key_exchange => dhe_dss, 
      cipher => des_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => dhe_dss, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_RSA_WITH_DES_CBC_SHA) ->
    #{key_exchange => dhe_rsa, 
      cipher => des_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => dhe_rsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
%%% TSL V1.1 AES suites
suite_bin_to_map(?TLS_RSA_WITH_AES_128_CBC_SHA) -> 
    #{key_exchange => rsa,
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_DSS_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_RSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => dhe_rsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_WITH_AES_256_CBC_SHA) -> 
    #{key_exchange => rsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_DSS_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_RSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => dhe_rsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
%% TLS v1.2 suites
%% suite_bin_to_map(?TLS_RSA_WITH_NULL_SHA) ->
%%     {rsa, null, sha, default_prf};
suite_bin_to_map(?TLS_RSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => rsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_WITH_AES_256_CBC_SHA256) ->
    #{key_exchange => rsa, 
      cipher => aes_256_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_DSS_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_RSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => dhe_rsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_DSS_WITH_AES_256_CBC_SHA256) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_256_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_RSA_WITH_AES_256_CBC_SHA256) ->
    #{key_exchange => dhe_rsa, 
      cipher => aes_256_cbc, 
      mac => sha256, 
      prf => default_prf};
%% not defined YET:
%%   TLS_DH_DSS_WITH_AES_128_CBC_SHA256      DH_DSS       AES_128_CBC  SHA256
%%   TLS_DH_RSA_WITH_AES_128_CBC_SHA256      DH_RSA       AES_128_CBC  SHA256
%%   TLS_DH_DSS_WITH_AES_256_CBC_SHA256      DH_DSS       AES_256_CBC  SHA256
%%   TLS_DH_RSA_WITH_AES_256_CBC_SHA256      DH_RSA       AES_256_CBC  SHA256
%%% DH-ANON deprecated by TLS spec and not available
%%% by default, but good for testing purposes.
suite_bin_to_map(?TLS_DH_anon_WITH_RC4_128_MD5) ->
    #{key_exchange => dh_anon, 
      cipher => rc4_128, 
      mac => md5, 
      prf => default_prf};
suite_bin_to_map(?TLS_DH_anon_WITH_DES_CBC_SHA) ->
    #{key_exchange => dh_anon, 
      cipher => des_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DH_anon_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => dh_anon, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DH_anon_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => dh_anon, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DH_anon_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => dh_anon, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DH_anon_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => dh_anon, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_DH_anon_WITH_AES_256_CBC_SHA256) ->
    #{key_exchange => dh_anon, 
      cipher => aes_256_cbc, 
      mac => sha256, 
      prf => default_prf};
%%% PSK Cipher Suites RFC 4279
suite_bin_to_map(?TLS_PSK_WITH_RC4_128_SHA) ->
    #{key_exchange => psk, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_PSK_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => psk, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_PSK_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => psk, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_PSK_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => psk, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_PSK_WITH_RC4_128_SHA) ->
    #{key_exchange => dhe_psk, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => dhe_psk, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_PSK_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_PSK_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_PSK_WITH_RC4_128_SHA) ->
    #{key_exchange => rsa_psk, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => rsa_psk, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_PSK_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_PSK_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
%%% PSK NULL Cipher Suites RFC 4785
suite_bin_to_map(?TLS_PSK_WITH_NULL_SHA) ->
    #{key_exchange => psk, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_PSK_WITH_NULL_SHA) ->
    #{key_exchange => dhe_psk,
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_PSK_WITH_NULL_SHA) ->
    #{key_exchange => rsa_psk,
      cipher => null, 
      mac => sha, 
      prf => default_prf};  
%%% TLS 1.2 PSK Cipher Suites RFC 5487
suite_bin_to_map(?TLS_PSK_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => psk, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_PSK_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => psk, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_DHE_PSK_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_DHE_PSK_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_RSA_PSK_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_RSA_PSK_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_PSK_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => psk, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_PSK_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => psk, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
suite_bin_to_map(?TLS_DHE_PSK_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_PSK_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
suite_bin_to_map(?TLS_RSA_PSK_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_PSK_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
suite_bin_to_map(?TLS_PSK_WITH_NULL_SHA256) ->
    #{key_exchange => psk, 
      cipher => null, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_PSK_WITH_NULL_SHA384) ->
    #{key_exchange => psk, 
      cipher => null, 
      mac => sha384, 
      prf => sha384};
suite_bin_to_map(?TLS_DHE_PSK_WITH_NULL_SHA256) ->
    #{key_exchange => dhe_psk, 
      cipher => null, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_DHE_PSK_WITH_NULL_SHA384) ->
    #{key_exchange => dhe_psk, 
      cipher => null, 
      mac => sha384, 
      prf => sha384};
suite_bin_to_map(?TLS_RSA_PSK_WITH_NULL_SHA256) ->
    #{key_exchange => rsa_psk, 
      cipher => null, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_RSA_PSK_WITH_NULL_SHA384) ->
    #{key_exchange => rsa_psk, 
      cipher => null, 
      mac => sha384, 
      prf => sha384};
%%% ECDHE PSK Cipher Suites RFC 5489
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdhe_psk, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdhe_psk, 
      cipher => '3des_ede_cbc',
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_NULL_SHA256) ->
    #{key_exchange => ecdhe_psk, 
      cipher => null, 
      mac => sha256, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_NULL_SHA384) ->
    #{key_exchange => ecdhe_psk, 
      cipher => null, mac => sha384, 
      prf => sha384};
%%% ECDHE_PSK with AES-GCM and AES-CCM Cipher Suites, draft-ietf-tls-ecdhe-psk-aead-05
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_128_gcm, 
      mac => null, 
      prf => sha256};
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_256_gcm, 
      mac => null, 
      prf => sha384};
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_AES_128_CCM_SHA256) ->
     #{key_exchange => ecdhe_psk, 
      cipher => aes_128_ccm, 
       mac => null, 
       prf => sha256};
suite_bin_to_map(?TLS_ECDHE_PSK_WITH_AES_128_CCM_8_SHA256) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_128_ccm_8, 
      mac => null, 
      prf => sha256};
%%% SRP Cipher Suites RFC 5054
suite_bin_to_map(?TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => srp_anon, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => srp_rsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => srp_dss, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_SRP_SHA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => srp_anon, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => srp_rsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => srp_dss, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_SRP_SHA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => srp_anon, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => srp_rsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => srp_dss, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
%% RFC 4492 EC TLS suites
suite_bin_to_map(?TLS_ECDH_ECDSA_WITH_NULL_SHA) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_ECDSA_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_ECDSA_WITH_NULL_SHA) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_ECDSA_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_RSA_WITH_NULL_SHA) ->
    #{key_exchange => ecdh_rsa, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_RSA_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdh_rsa, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdh_rsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_RSA_WITH_NULL_SHA) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_RSA_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_anon_WITH_NULL_SHA) ->
    #{key_exchange => ecdh_anon, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_anon_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdh_anon, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdh_anon, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_anon_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdh_anon, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_bin_to_map(?TLS_ECDH_anon_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdh_anon, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
%% RFC 5289 EC TLS suites
suite_bin_to_map(?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => sha256};
suite_bin_to_map(?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
suite_bin_to_map(?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => sha256};
suite_bin_to_map(?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
suite_bin_to_map(?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => sha256};
suite_bin_to_map(?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
suite_bin_to_map(?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => sha256};
suite_bin_to_map(?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
%% RFC 5288 AES-GCM Cipher Suites
suite_bin_to_map(?TLS_RSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => rsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_RSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => rsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_DHE_RSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dhe_rsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_DHE_RSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dhe_rsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_DH_RSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dh_rsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_DH_RSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dh_rsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_DHE_DSS_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_DHE_DSS_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_DH_DSS_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dh_dss, 
      cipher => aes_128_gcm, 
      mac => null, 
      prf => sha256};
suite_bin_to_map(?TLS_DH_DSS_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dh_dss, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_DH_anon_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dh_anon, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_DH_anon_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dh_anon, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
%% RFC 5289 ECC AES-GCM Cipher Suites
suite_bin_to_map(?TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_ECDH_ECDSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_ECDH_ECDSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_ECDH_RSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_ECDH_RSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_bin_to_map(?TLS_PSK_WITH_AES_128_CCM) ->
    #{key_exchange => psk,
      cipher => aes_128_ccm,
      mac => aead,
      prf => sha256};
suite_bin_to_map(?TLS_PSK_WITH_AES_256_CCM) -> 
    #{key_exchange => psk,
      cipher => aes_256_ccm,
      mac => aead,
      prf => sha256};
suite_bin_to_map(?TLS_DHE_PSK_WITH_AES_128_CCM) ->
    #{key_exchange => dhe_psk,
      cipher => aes_128_ccm,
      mac => aead,
      prf => sha256};
suite_bin_to_map(?TLS_DHE_PSK_WITH_AES_256_CCM) ->
    #{key_exchange => dhe_psk,
      cipher => aes_256_ccm,
      mac => aead,
      prf => sha256};
suite_bin_to_map(?TLS_PSK_WITH_AES_128_CCM_8) ->
    #{key_exchange => psk,
      cipher => aes_128_ccm_8,
      mac => aead,
      prf => sha256};
suite_bin_to_map(?TLS_PSK_WITH_AES_256_CCM_8) ->
    #{key_exchange => psk,
      cipher => aes_256_ccm_8,
      mac => aead,
      prf => sha256};
suite_bin_to_map(?TLS_PSK_DHE_WITH_AES_128_CCM_8) ->
  #{key_exchange => dhe_psk,
    cipher => aes_128_ccm_8,
    mac => aead,
    prf => sha256};
suite_bin_to_map(?TLS_PSK_DHE_WITH_AES_256_CCM_8) ->
  #{key_exchange => dhe_psk,
    cipher => aes_256_ccm_8,
    mac => aead,
    prf => sha256};
suite_bin_to_map(#{key_exchange := psk_dhe,
                   cipher := aes_256_ccm_8,
                   mac := aead,
                   prf := sha256}) ->
    ?TLS_PSK_DHE_WITH_AES_256_CCM_8;

% draft-agl-tls-chacha20poly1305-04 Chacha20/Poly1305 Suites
suite_bin_to_map(?TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => chacha20_poly1305, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => chacha20_poly1305, 
      mac => aead, 
      prf => sha256};
suite_bin_to_map(?TLS_DHE_RSA_WITH_CHACHA20_POLY1305_SHA256) ->
    #{key_exchange => dhe_rsa, 
      cipher => chacha20_poly1305, 
      mac => aead, 
      prf => sha256};
%% TLS 1.3 Cipher Suites RFC8446
suite_bin_to_map(?TLS_AES_128_GCM_SHA256) ->
    #{key_exchange => any,
      cipher => aes_128_gcm,
      mac => aead,
      prf => sha256};
suite_bin_to_map(?TLS_AES_256_GCM_SHA384) ->
    #{key_exchange => any,
      cipher => aes_256_gcm,
      mac => aead,
      prf => sha384};
suite_bin_to_map(?TLS_CHACHA20_POLY1305_SHA256) ->
    #{key_exchange => any,
      cipher => chacha20_poly1305,
      mac => aead,
      prf => sha256};
suite_bin_to_map(?TLS_AES_128_CCM_SHA256) ->
     #{key_exchange => any,
       cipher => aes_128_ccm,
       mac => aead,
       prf => sha256}.
%% suite_bin_to_map(?TLS_AES_128_CCM_8_SHA256) ->
%%      #{key_exchange => any,
%%       cipher => aes_128_ccm_8,
%%        mac => aead,
%%        prf => sha256}.

%%--------------------------------------------------------------------
-spec suite_legacy(cipher_suite() | internal_erl_cipher_suite()) -> old_erl_cipher_suite().
%%
%% Description: Return erlang cipher suite definition. Filters last value
%% for now (compatibility reasons).
%%--------------------------------------------------------------------
suite_legacy(Bin) when is_binary(Bin) ->
    suite_legacy(suite_bin_to_map(Bin));    
suite_legacy(#{key_exchange := KeyExchange, cipher := Cipher,
                       mac := Hash, prf := Prf}) ->
    case Prf of
        default_prf ->
	    {KeyExchange, Cipher, Hash};
	_ ->
            {KeyExchange, Cipher, Hash, Prf}
    end.

%%--------------------------------------------------------------------
-spec suite_map_to_bin(internal_erl_cipher_suite()) -> cipher_suite().
%%
%% Description: Return TLS cipher suite definition.
%%--------------------------------------------------------------------
%% TLS v1.1 suites
suite_map_to_bin(#{key_exchange := rsa, 
        cipher := rc4_128, 
        mac := md5}) ->
    ?TLS_RSA_WITH_RC4_128_MD5;
suite_map_to_bin(#{key_exchange := rsa, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_RSA_WITH_RC4_128_SHA;
suite_map_to_bin(#{key_exchange := rsa, 
        cipher := des_cbc, 
        mac := sha}) ->
    ?TLS_RSA_WITH_DES_CBC_SHA; 
suite_map_to_bin(#{key_exchange := rsa, 
        cipher :='3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_RSA_WITH_3DES_EDE_CBC_SHA; 
suite_map_to_bin(#{key_exchange := dhe_dss,  
        cipher:= des_cbc, 
        mac := sha}) ->
    ?TLS_DHE_DSS_WITH_DES_CBC_SHA;
suite_map_to_bin(#{key_exchange := dhe_dss, 
        cipher:= '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := dhe_rsa, 
        cipher:= des_cbc,
        mac := sha}) ->
    ?TLS_DHE_RSA_WITH_DES_CBC_SHA;
suite_map_to_bin(#{key_exchange := dhe_rsa, 
        cipher:= '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA; 
suite_map_to_bin(#{key_exchange := dh_anon, 
        cipher:= rc4_128,
        mac := md5}) ->
    ?TLS_DH_anon_WITH_RC4_128_MD5;
suite_map_to_bin(#{key_exchange := dh_anon, 
        cipher:= des_cbc,  
        mac := sha}) ->
    ?TLS_DH_anon_WITH_DES_CBC_SHA;
suite_map_to_bin(#{key_exchange := dh_anon, 
        cipher:= '3des_ede_cbc',
        mac := sha}) ->
    ?TLS_DH_anon_WITH_3DES_EDE_CBC_SHA;
%%% TSL V1.1 AES suites
suite_map_to_bin(#{key_exchange := rsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_RSA_WITH_AES_128_CBC_SHA; 
suite_map_to_bin(#{key_exchange := dhe_dss, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA; 
suite_map_to_bin(#{key_exchange := dhe_rsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := dh_anon, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_DH_anon_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := rsa, 
        cipher := aes_256_cbc,  
        mac := sha}) ->
    ?TLS_RSA_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := dhe_dss, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := dhe_rsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := dh_anon, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_DH_anon_WITH_AES_256_CBC_SHA;
%% TLS v1.2 suites
suite_map_to_bin(#{key_exchange := rsa, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_RSA_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := rsa, 
        cipher := aes_256_cbc, 
        mac := sha256}) ->
    ?TLS_RSA_WITH_AES_256_CBC_SHA256;
suite_map_to_bin(#{key_exchange := dhe_dss, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := dhe_rsa, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := dhe_dss, 
        cipher := aes_256_cbc, 
        mac := sha256}) ->
    ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA256;
suite_map_to_bin(#{key_exchange := dhe_rsa, 
        cipher := aes_256_cbc, 
        mac := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA256;
suite_map_to_bin(#{key_exchange := dh_anon, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_DH_anon_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := dh_anon, 
        cipher := aes_256_cbc, 
        mac := sha256}) ->
    ?TLS_DH_anon_WITH_AES_256_CBC_SHA256;
%%% PSK Cipher Suites RFC 4279
suite_map_to_bin(#{key_exchange := psk, 
        cipher := rc4_128,
        mac := sha}) ->
    ?TLS_PSK_WITH_RC4_128_SHA;
suite_map_to_bin(#{key_exchange := psk, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_PSK_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := psk, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_PSK_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := psk, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_PSK_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := dhe_psk, 
        cipher := rc4_128, 
        mac := sha})  ->
    ?TLS_DHE_PSK_WITH_RC4_128_SHA;
suite_map_to_bin(#{key_exchange := dhe_psk, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := dhe_psk, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_DHE_PSK_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := dhe_psk, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_DHE_PSK_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := rsa_psk, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_RSA_PSK_WITH_RC4_128_SHA;
suite_map_to_bin(#{key_exchange := rsa_psk, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := rsa_psk, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_RSA_PSK_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := rsa_psk, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_RSA_PSK_WITH_AES_256_CBC_SHA;
%%% PSK NULL Cipher Suites RFC 4785
suite_map_to_bin(#{key_exchange := psk, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_PSK_WITH_NULL_SHA;
suite_map_to_bin(#{key_exchange := dhe_psk, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_DHE_PSK_WITH_NULL_SHA;
suite_map_to_bin(#{key_exchange := rsa_psk, 
       cipher := null, 
       mac := sha}) ->
    ?TLS_RSA_PSK_WITH_NULL_SHA;
%%% TLS 1.2 PSK Cipher Suites RFC 5487
suite_map_to_bin(#{key_exchange := psk, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_PSK_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := psk, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_PSK_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := dhe_psk, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DHE_PSK_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := dhe_psk, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DHE_PSK_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := rsa_psk, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_RSA_PSK_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := rsa_psk, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_RSA_PSK_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := psk, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_PSK_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := psk, 
        cipher := aes_256_cbc, 
        mac := sha384}) ->
    ?TLS_PSK_WITH_AES_256_CBC_SHA384;
suite_map_to_bin(#{key_exchange := dhe_psk, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_DHE_PSK_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := dhe_psk, 
        cipher := aes_256_cbc, 
        mac := sha384}) ->
    ?TLS_DHE_PSK_WITH_AES_256_CBC_SHA384;
suite_map_to_bin(#{key_exchange := rsa_psk, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_RSA_PSK_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := rsa_psk, 
        cipher := aes_256_cbc, 
        mac := sha384}) ->
    ?TLS_RSA_PSK_WITH_AES_256_CBC_SHA384;
suite_map_to_bin(#{key_exchange := psk, 
        cipher := null, 
        mac := sha256}) ->
    ?TLS_PSK_WITH_NULL_SHA256;
suite_map_to_bin(#{key_exchange := psk, 
        cipher := null,
        mac := sha384}) ->
    ?TLS_PSK_WITH_NULL_SHA384;
suite_map_to_bin(#{key_exchange := dhe_psk, 
        cipher := null, 
        mac := sha256}) ->
    ?TLS_DHE_PSK_WITH_NULL_SHA256;
suite_map_to_bin(#{key_exchange := dhe_psk, 
        cipher := null, 
        mac := sha384}) ->
    ?TLS_DHE_PSK_WITH_NULL_SHA384;
suite_map_to_bin(#{key_exchange := rsa_psk, 
        cipher := null,  
        mac := sha256}) ->
    ?TLS_RSA_PSK_WITH_NULL_SHA256;
suite_map_to_bin(#{key_exchange := rsa_psk, 
       cipher := null, 
       mac := sha384}) ->
    ?TLS_RSA_PSK_WITH_NULL_SHA384;
%%% ECDHE PSK Cipher Suites RFC 5489
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
        cipher := rc4_128,
        mac := sha}) ->
    ?TLS_ECDHE_PSK_WITH_RC4_128_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
        cipher :='3des_ede_cbc',
        mac := sha}) ->
    ?TLS_ECDHE_PSK_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
        cipher := aes_128_cbc,
        mac := sha}) ->
    ?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
        cipher := aes_256_cbc,
        mac := sha}) ->
    ?TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
       cipher := aes_128_cbc, 
       mac := sha256}) ->
    ?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
       cipher := aes_256_cbc, 
       mac := sha384}) ->
    ?TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA384;
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
       cipher := null, 
       mac := sha256}) ->
    ?TLS_ECDHE_PSK_WITH_NULL_SHA256;
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
       cipher := null, 
       mac := sha384}) ->
    ?TLS_ECDHE_PSK_WITH_NULL_SHA384;
%%% ECDHE_PSK with AES-GCM and AES-CCM Cipher Suites, draft-ietf-tls-ecdhe-psk-aead-05
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
       cipher := aes_128_gcm, 
       mac := null, 
       prf := sha256}) ->
    ?TLS_ECDHE_PSK_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
       cipher := aes_256_gcm, 
       mac := null, 
       prf := sha384}) ->
    ?TLS_ECDHE_PSK_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
        cipher := aes_128_ccm_8, 
        mac := null, 
        prf := sha256}) ->
    ?TLS_ECDHE_PSK_WITH_AES_128_CCM_8_SHA256;
suite_map_to_bin(#{key_exchange := ecdhe_psk, 
        cipher := aes_128_ccm, 
         mac := null, 
         prf := sha256}) ->
    ?TLS_ECDHE_PSK_WITH_AES_128_CCM_SHA256;
%%% SRP Cipher Suites RFC 5054
suite_map_to_bin(#{key_exchange := srp_anon, 
        cipher := '3des_ede_cbc',
        mac :=  sha}) ->
    ?TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := srp_rsa, 
        cipher := '3des_ede_cbc',
        mac := sha}) ->
    ?TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := srp_dss, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := srp_anon, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := srp_rsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := srp_dss, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := srp_anon, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := srp_rsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := srp_dss, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA;
%%% RFC 4492 EC TLS suites
suite_map_to_bin(#{key_exchange := ecdh_ecdsa, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_ECDH_ECDSA_WITH_NULL_SHA;
suite_map_to_bin(#{key_exchange := ecdh_ecdsa, 
        cipher := rc4_128,  
        mac := sha})  ->
    ?TLS_ECDH_ECDSA_WITH_RC4_128_SHA;
suite_map_to_bin(#{key_exchange := ecdh_ecdsa, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_ecdsa, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_ECDHE_ECDSA_WITH_NULL_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_ecdsa, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_ECDHE_ECDSA_WITH_RC4_128_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_ecdsa, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdh_rsa, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_ECDH_RSA_WITH_NULL_SHA;
suite_map_to_bin(#{key_exchange := ecdh_rsa, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_ECDH_RSA_WITH_RC4_128_SHA;
suite_map_to_bin(#{key_exchange := ecdh_rsa, 
        cipher := '3des_ede_cbc', mac := sha}) ->
    ?TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdh_rsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdh_rsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_rsa, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_ECDHE_RSA_WITH_NULL_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_rsa, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_ECDHE_RSA_WITH_RC4_128_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_rsa, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_rsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdhe_rsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdh_anon, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_ECDH_anon_WITH_NULL_SHA;
suite_map_to_bin(#{key_exchange := ecdh_anon, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_ECDH_anon_WITH_RC4_128_SHA;
suite_map_to_bin(#{key_exchange := ecdh_anon, 
        cipher := '3des_ede_cbc', 
        mac :=  sha}) ->
    ?TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdh_anon, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_anon_WITH_AES_128_CBC_SHA;
suite_map_to_bin(#{key_exchange := ecdh_anon, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_anon_WITH_AES_256_CBC_SHA;
%%% RFC 5289 EC TLS suites
suite_map_to_bin(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_128_cbc, 
        mac:= sha256, 
        prf := sha256}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_256_cbc, 
        mac := sha384, 
        prf := sha384}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384;
suite_map_to_bin(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_128_cbc, 
        mac := sha256, 
        prf := sha256}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_256_cbc, 
        mac := sha384, 
        prf := sha384}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA384;
suite_map_to_bin(#{key_exchange := ecdhe_rsa, 
        cipher := aes_128_cbc, 
        mac := sha256, 
        prf := sha256}) ->
    ?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := ecdhe_rsa, 
        cipher := aes_256_cbc, 
        mac := sha384, 
        prf := sha384}) ->
    ?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384;
suite_map_to_bin(#{key_exchange := ecdh_rsa, 
        cipher := aes_128_cbc, 
        mac := sha256, 
        prf := sha256}) ->
    ?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256;
suite_map_to_bin(#{key_exchange := ecdh_rsa, 
        cipher := aes_256_cbc, 
        mac := sha384, 
        prf := sha384}) ->
    ?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA384;
%% RFC 5288 AES-GCM Cipher Suites
suite_map_to_bin(#{key_exchange := rsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_RSA_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := rsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_RSA_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := dhe_rsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := dhe_rsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DHE_RSA_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := dh_rsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DH_RSA_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := dh_rsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DH_RSA_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := dhe_dss, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DHE_DSS_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := dhe_dss, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DHE_DSS_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := dh_dss, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DH_DSS_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := dh_dss, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DH_DSS_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := dh_anon, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DH_anon_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := dh_anon, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DH_anon_WITH_AES_256_GCM_SHA384;
%% RFC 5289 ECC AES-GCM Cipher Suites
suite_map_to_bin(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := ecdhe_rsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := ecdhe_rsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := ecdh_rsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDH_RSA_WITH_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := ecdh_rsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_ECDH_RSA_WITH_AES_256_GCM_SHA384;
%% draft-agl-tls-chacha20poly1305-04 Chacha20/Poly1305 Suites
suite_map_to_bin(#{key_exchange := ecdhe_rsa, 
        cipher := chacha20_poly1305,  
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256;
suite_map_to_bin(#{key_exchange := ecdhe_ecdsa, 
        cipher := chacha20_poly1305, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256;
suite_map_to_bin(#{key_exchange := dhe_rsa, 
        cipher := chacha20_poly1305,  
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_CHACHA20_POLY1305_SHA256;

%% RFC 6655 - TLS-1.2 cipher suites
suite_map_to_bin(#{key_exchange := psk,
        cipher := aes_128_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_WITH_AES_128_CCM;
suite_map_to_bin(#{key_exchange := psk,
        cipher := aes_256_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_WITH_AES_256_CCM;
suite_map_to_bin(#{key_exchange := dhe_psk,
        cipher := aes_128_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_PSK_WITH_AES_128_CCM;
suite_map_to_bin(#{key_exchange := dhe_psk,
        cipher := aes_256_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_PSK_WITH_AES_256_CCM;
suite_map_to_bin(#{key_exchange := rsa,
        cipher := aes_128_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_RSA_WITH_AES_128_CCM;
suite_map_to_bin(#{key_exchange := rsa,
        cipher := aes_256_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_RSA_WITH_AES_256_CCM;
suite_map_to_bin(#{key_exchange := dhe_rsa,
        cipher := aes_128_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_128_CCM;
suite_map_to_bin(#{key_exchange := dhe_rsa,
        cipher := aes_256_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_256_CCM;

suite_map_to_bin(#{key_exchange := psk,
        cipher := aes_128_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_WITH_AES_128_CCM_8;
suite_map_to_bin(#{key_exchange := psk,
        cipher := aes_256_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_WITH_AES_256_CCM_8;
suite_map_to_bin(#{key_exchange := dhe_psk,
        cipher := aes_128_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_DHE_WITH_AES_128_CCM_8;
suite_map_to_bin(#{key_exchange := dhe_psk,
        cipher := aes_256_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_DHE_WITH_AES_256_CCM_8;
suite_map_to_bin(#{key_exchange := rsa,
        cipher := aes_128_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_RSA_WITH_AES_128_CCM_8;
suite_map_to_bin(#{key_exchange := rsa,
        cipher := aes_256_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_RSA_WITH_AES_256_CCM_8;
suite_map_to_bin(#{key_exchange := dhe_rsa,
        cipher := aes_128_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_128_CCM_8;
suite_map_to_bin(#{key_exchange := dhe_rsa,
        cipher := aes_256_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_256_CCM_8;

%% TLS 1.3 Cipher Suites RFC8446
suite_map_to_bin(#{key_exchange := any,
        cipher := aes_128_gcm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_AES_128_GCM_SHA256;
suite_map_to_bin(#{key_exchange := any,
      cipher := aes_256_gcm,
      mac := aead,
      prf := sha384}) ->
    ?TLS_AES_256_GCM_SHA384;
suite_map_to_bin(#{key_exchange := any,
      cipher := chacha20_poly1305,
      mac := aead,
      prf := sha256}) ->
    ?TLS_CHACHA20_POLY1305_SHA256;
suite_map_to_bin(#{key_exchange := any,
      cipher := aes_128_ccm,
      mac := aead,
      prf := sha256}) ->
    ?TLS_AES_128_CCM_SHA256.
%% suite_map_to_bin(#{key_exchange := any,
%%       cipher := aes_128_ccm_8,
%%       mac := aead,
%%       prf := sha256}) ->
%%     ?TLS_AES_128_CCM_8_SHA256.


tls_1_3_suite_str_to_map(CipherStr) ->
    {Cipher, Mac, Prf} = cipher_str_to_algs(any, CipherStr, ""),
    #{key_exchange => any,
      mac => Mac,
      cipher => Cipher,
      prf => Prf   
     }.

pre_tls_1_3_suite_str_to_map(KexStr, Rest) ->
    Kex = algo_str_to_atom(KexStr),
    [CipherStr, AlgStr] = string:split(Rest, "_", trailing),
    {Cipher, Mac, Prf} = cipher_str_to_algs(Kex, CipherStr, AlgStr),
    #{key_exchange => Kex,
      mac => Mac,
      cipher => Cipher,
      prf => Prf   
     }.
                
cipher_str_to_algs(_, CipherStr, "CCM"= End) -> %% PRE TLS 1.3
    Cipher = algo_str_to_atom(CipherStr ++ "_" ++ End),
    {Cipher, aead, sha256};
cipher_str_to_algs(_, CipherStr, "8" = End) -> %% PRE TLS 1.3
    Cipher = algo_str_to_atom(CipherStr ++ "_" ++ End),
    {Cipher, aead, sha256};
cipher_str_to_algs(_, CipherStr, "CHACHA20_POLY1305" = End) -> %% PRE TLS 1.3
    Cipher = algo_str_to_atom(CipherStr ++ "_" ++ End),
    {Cipher, aead, sha256};
cipher_str_to_algs(_, CipherStr0, "") -> %% TLS 1.3
    [CipherStr, AlgStr] = string:split(CipherStr0, "_", trailing),
    Hash = algo_str_to_atom(AlgStr),
    Cipher = algo_str_to_atom(CipherStr),
    {Cipher, aead, Hash};
cipher_str_to_algs(Kex, CipherStr, HashStr) -> %% PRE TLS 1.3
    Hash = algo_str_to_atom(HashStr),
    Cipher = algo_str_to_atom(CipherStr),
    case is_aead_cipher(CipherStr) of
        true ->
            {Cipher, aead, Hash};
        false ->
            {Cipher, Hash, default_prf(Kex, Hash)}
    end.

default_prf(_, md5) ->
    default_prf;
default_prf(_, sha) ->
    default_prf;
default_prf(ecdhe_ecdsa, sha256) ->
    sha256;
default_prf(ecdhe_rsa, sha256) ->
    sha256;
default_prf(dhe_rsa, sha256) ->
    default_prf;
default_prf(dhe_dss, sha256) ->
    default_prf;
default_prf(rsa, sha256) ->
    default_prf;
default_prf(rsa_psk, sha256) ->
    default_prf;
default_prf(_, Hash) ->
    Hash.

%% PRE TLS 1.3
is_aead_cipher("CHACHA20_POLY1305") ->
    true;
is_aead_cipher(CipherStr) ->
    [_, Rest] = string:split(CipherStr, "_", trailing),
    (Rest == "GCM") orelse (Rest == "CCM") orelse (Rest == "8").

openssl_is_aead_cipher("CHACHA20-POLY1305") ->
    true;
openssl_is_aead_cipher(CipherStr) ->
    case string:split(CipherStr, "-", trailing) of
        [_, Rest] ->      
            (Rest == "GCM") orelse (Rest == "CCM") orelse (Rest == "8");
        [_] ->
            false
    end.  

algo_str_to_atom("SRP_SHA_DSS") ->
    srp_dss;
algo_str_to_atom(AlgoStr) ->
    erlang:list_to_existing_atom(string:to_lower(AlgoStr)).

openssl_cipher_name(_, "3DES_EDE_CBC" ++ _) ->
    "DES-CBC3";
openssl_cipher_name(Kex, "AES_128_CBC" ++ _ = CipherStr) when Kex == rsa;
                                                              Kex == dhe_rsa;
                                                              Kex == ecdhe_rsa;
                                                              Kex == ecdhe_ecdsa ->
    openssl_name_concat(CipherStr);
openssl_cipher_name(Kex, "AES_256_CBC" ++ _ = CipherStr) when Kex == rsa;
                                                              Kex == dhe_rsa;
                                                              Kex == ecdhe_rsa;
                                                              Kex == ecdhe_ecdsa ->
    openssl_name_concat(CipherStr);
openssl_cipher_name(Kex, "AES_128_CBC" ++ _ = CipherStr) when Kex == srp;
                                                              Kex == srp_rsa ->
    lists:append(string:replace(CipherStr, "_", "-", all));
openssl_cipher_name(Kex, "AES_256_CBC" ++ _ = CipherStr) when Kex == srp;
                                                              Kex == srp_rsa ->
    lists:append(string:replace(CipherStr, "_", "-", all));
openssl_cipher_name(_, "AES_128_CBC" ++ _ = CipherStr) ->
    openssl_name_concat(CipherStr)  ++ "-CBC";
openssl_cipher_name(_, "AES_256_CBC" ++ _ = CipherStr) ->
    openssl_name_concat(CipherStr)  ++ "-CBC";
openssl_cipher_name(_, "AES_128_GCM" ++ _ = CipherStr) ->
    openssl_name_concat(CipherStr) ++ "-GCM";
openssl_cipher_name(_, "AES_256_GCM" ++ _ = CipherStr) ->
    openssl_name_concat(CipherStr) ++ "-GCM";
openssl_cipher_name(_, "RC4" ++ _) ->
    "RC4";
openssl_cipher_name(_, CipherStr) ->
    lists:append(string:replace(CipherStr, "_", "-", all)).


openssl_suite_start(Kex) ->
    case openssl_kex_name(Kex) of
        "" ->
            "";
        Name ->
            Name ++ "-"
    end.

openssl_kex_name("RSA") ->
    "";
openssl_kex_name(Kex) ->
    lists:append(string:replace(Kex, "_", "-", all)).

kex_name_from_openssl(Kex) ->
    lists:append(string:replace(Kex, "-", "_", all)).

cipher_name_from_openssl("AES128") ->
    "AES_128_CBC";
cipher_name_from_openssl("AES256") ->
    "AES_256_CBC";
cipher_name_from_openssl("AES128-CBC") ->
    "AES_128_CBC";
cipher_name_from_openssl("AES256-CBC") ->
    "AES_256_CBC";
cipher_name_from_openssl("AES-128-CBC") ->
    "AES_128_CBC";
cipher_name_from_openssl("AES-256-CBC") ->
    "AES_256_CBC";
cipher_name_from_openssl("AES128-GCM") ->
    "AES_128_GCM";
cipher_name_from_openssl("AES256-GCM") ->
    "AES_256_GCM";
cipher_name_from_openssl("DES-CBC") ->
    "DES_CBC";
cipher_name_from_openssl("DES-CBC3") ->
    "3DES_EDE_CBC";
cipher_name_from_openssl("RC4") ->
    "RC4_128";
cipher_name_from_openssl(Str) ->
    Str.

openssl_name_concat(Str0) ->
    [Str, _] = string:split(Str0, "_", trailing),
    [Part1, Part2] = string:split(Str, "_", trailing),
    Part1 ++ Part2.


suite_openssl_str_to_map(Kex0, Rest) ->
    Kex = algo_str_to_atom(kex_name_from_openssl(Kex0)),
    [CipherStr, AlgStr] = string:split(Rest, "-", trailing),
    {Cipher, Mac, Prf} = openssl_cipher_str_to_algs(Kex, CipherStr, AlgStr),
    #{key_exchange => Kex,
      mac => Mac,
      cipher => Cipher,
      prf => Prf   
     }.

%% Does only need own implementation PRE TLS 1.3 
openssl_cipher_str_to_algs(_, CipherStr, "CCM"= End) -> 
    Cipher = algo_str_to_atom(CipherStr ++ "_" ++ End),
    {Cipher, aead, sha256};
openssl_cipher_str_to_algs(_, CipherStr, "8" = End) -> 
    Cipher = algo_str_to_atom(CipherStr ++ "_" ++ End),
    {Cipher, aead, sha256};
openssl_cipher_str_to_algs(_, CipherStr, "POLY1305" = End) -> 
    Cipher = algo_str_to_atom(CipherStr ++ "_" ++ End),
    {Cipher, aead, sha256};
openssl_cipher_str_to_algs(Kex, CipherStr, HashStr) ->
    Hash = algo_str_to_atom(HashStr),
    Cipher = algo_str_to_atom(cipher_name_from_openssl(CipherStr)),
    case openssl_is_aead_cipher(CipherStr) of
        true ->
            {Cipher, aead, Hash};
        false ->
            {Cipher, Hash, default_prf(Kex, Hash)}
    end.



