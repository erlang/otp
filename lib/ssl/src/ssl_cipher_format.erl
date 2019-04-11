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


-export([suite_to_str/1, suite_definition/1, suite/1, erl_suite_definition/1, 
         openssl_suite/1, openssl_suite_name/1]).

%%--------------------------------------------------------------------
-spec suite_to_str(internal_erl_cipher_suite()) -> string().
%%
%% Description: Return the string representation of a cipher suite.
%%--------------------------------------------------------------------
suite_to_str(#{key_exchange := null,
               cipher := null,
               mac := null,
               prf := null}) ->
    "TLS_EMPTY_RENEGOTIATION_INFO_SCSV";
suite_to_str(#{key_exchange := any,
               cipher := Cipher,
               mac := aead,
               prf := PRF}) ->
    "TLS_" ++ string:to_upper(atom_to_list(Cipher)) ++
        "_" ++ string:to_upper(atom_to_list(PRF));
suite_to_str(#{key_exchange := Kex,
               cipher := Cipher,
               mac := aead,
               prf := PRF}) ->
    "TLS_" ++ string:to_upper(atom_to_list(Kex)) ++
        "_WITH_" ++  string:to_upper(atom_to_list(Cipher)) ++
        "_" ++ string:to_upper(atom_to_list(PRF));
suite_to_str(#{key_exchange := Kex,
               cipher := Cipher,
               mac := Mac}) ->
    "TLS_" ++ string:to_upper(atom_to_list(Kex)) ++
        "_WITH_" ++  string:to_upper(atom_to_list(Cipher)) ++
        "_" ++ string:to_upper(atom_to_list(Mac)).

%%--------------------------------------------------------------------
-spec suite_definition(cipher_suite()) -> internal_erl_cipher_suite().
%%
%% Description: Return erlang cipher suite definition.
%% Note: Currently not supported suites are commented away.
%% They should be supported or removed in the future.
%%-------------------------------------------------------------------
%% TLS v1.1 suites
suite_definition(?TLS_NULL_WITH_NULL_NULL) ->
    #{key_exchange => null,
      cipher => null, 
      mac => null, 
      prf => null};
%% RFC 5746 - Not a real cipher suite used to signal empty "renegotiation_info" extension
%% to avoid handshake failure from old servers that do not ignore
%% hello extension data as they should.
suite_definition(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV) ->
    #{key_exchange => null,
      cipher => null, 
      mac => null,
      prf => null};
suite_definition(?TLS_RSA_WITH_RC4_128_MD5) ->	
    #{key_exchange => rsa,
      cipher => rc4_128, 
      mac => md5, 
      prf => default_prf};
suite_definition(?TLS_RSA_WITH_RC4_128_SHA) ->
    #{key_exchange => rsa,
      cipher => rc4_128,
      mac => sha,
      prf => default_prf};
suite_definition(?TLS_RSA_WITH_DES_CBC_SHA) ->
    #{key_exchange => rsa,
      cipher => des_cbc,
      mac => sha,
      prf => default_prf};
suite_definition(?TLS_RSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => rsa,
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_DSS_WITH_DES_CBC_SHA) ->
    #{key_exchange => dhe_dss, 
      cipher => des_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => dhe_dss, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_RSA_WITH_DES_CBC_SHA) ->
    #{key_exchange => dhe_rsa, 
      cipher => des_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => dhe_rsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
%%% TSL V1.1 AES suites
suite_definition(?TLS_RSA_WITH_AES_128_CBC_SHA) -> 
    #{key_exchange => rsa,
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_DSS_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_RSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => dhe_rsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_RSA_WITH_AES_256_CBC_SHA) -> 
    #{key_exchange => rsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_DSS_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_RSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => dhe_rsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
%% TLS v1.2 suites
%% suite_definition(?TLS_RSA_WITH_NULL_SHA) ->
%%     {rsa, null, sha, default_prf};
suite_definition(?TLS_RSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => rsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_RSA_WITH_AES_256_CBC_SHA256) ->
    #{key_exchange => rsa, 
      cipher => aes_256_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_DHE_DSS_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_DHE_RSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => dhe_rsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_DHE_DSS_WITH_AES_256_CBC_SHA256) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_256_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_DHE_RSA_WITH_AES_256_CBC_SHA256) ->
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
suite_definition(?TLS_DH_anon_WITH_RC4_128_MD5) ->
    #{key_exchange => dh_anon, 
      cipher => rc4_128, 
      mac => md5, 
      prf => default_prf};
suite_definition(?TLS_DH_anon_WITH_DES_CBC_SHA) ->
    #{key_exchange => dh_anon, 
      cipher => des_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DH_anon_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => dh_anon, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DH_anon_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => dh_anon, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DH_anon_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => dh_anon, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DH_anon_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => dh_anon, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_DH_anon_WITH_AES_256_CBC_SHA256) ->
    #{key_exchange => dh_anon, 
      cipher => aes_256_cbc, 
      mac => sha256, 
      prf => default_prf};
%%% PSK Cipher Suites RFC 4279
suite_definition(?TLS_PSK_WITH_RC4_128_SHA) ->
    #{key_exchange => psk, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_PSK_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => psk, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_PSK_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => psk, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_PSK_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => psk, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_PSK_WITH_RC4_128_SHA) ->
    #{key_exchange => dhe_psk, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => dhe_psk, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_PSK_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_PSK_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_RSA_PSK_WITH_RC4_128_SHA) ->
    #{key_exchange => rsa_psk, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => rsa_psk, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_RSA_PSK_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_RSA_PSK_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
%%% PSK NULL Cipher Suites RFC 4785
suite_definition(?TLS_PSK_WITH_NULL_SHA) ->
    #{key_exchange => psk, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_DHE_PSK_WITH_NULL_SHA) ->
    #{key_exchange => dhe_psk,
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_RSA_PSK_WITH_NULL_SHA) ->
    #{key_exchange => rsa_psk,
      cipher => null, 
      mac => sha, 
      prf => default_prf};  
%%% TLS 1.2 PSK Cipher Suites RFC 5487
suite_definition(?TLS_PSK_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => psk, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_PSK_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => psk, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_DHE_PSK_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_DHE_PSK_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_RSA_PSK_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_RSA_PSK_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_PSK_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => psk, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_PSK_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => psk, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => default_prf};
suite_definition(?TLS_DHE_PSK_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_DHE_PSK_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => dhe_psk, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => default_prf};
suite_definition(?TLS_RSA_PSK_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_RSA_PSK_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => rsa_psk, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => default_prf};
suite_definition(?TLS_PSK_WITH_NULL_SHA256) ->
    #{key_exchange => psk, 
      cipher => null, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_PSK_WITH_NULL_SHA384) ->
    #{key_exchange => psk, 
      cipher => null, 
      mac => sha384, 
      prf => default_prf};
suite_definition(?TLS_DHE_PSK_WITH_NULL_SHA256) ->
    #{key_exchange => dhe_psk, 
      cipher => null, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_DHE_PSK_WITH_NULL_SHA384) ->
    #{key_exchange => dhe_psk, 
      cipher => null, 
      mac => sha384, 
      prf => default_prf};
suite_definition(?TLS_RSA_PSK_WITH_NULL_SHA256) ->
    #{key_exchange => rsa_psk, 
      cipher => null, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_RSA_PSK_WITH_NULL_SHA384) ->
    #{key_exchange => rsa_psk, 
      cipher => null, 
      mac => sha384, 
      prf => default_prf};
%%% ECDHE PSK Cipher Suites RFC 5489
suite_definition(?TLS_ECDHE_PSK_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdhe_psk, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_PSK_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdhe_psk, 
      cipher => '3des_ede_cbc',
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_PSK_WITH_NULL_SHA256) ->
    #{key_exchange => ecdhe_psk, 
      cipher => null, 
      mac => sha256, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_PSK_WITH_NULL_SHA384) ->
    #{key_exchange => ecdhe_psk, 
      cipher => null, mac => sha384, 
      prf => default_prf};
%%% ECDHE_PSK with AES-GCM and AES-CCM Cipher Suites, draft-ietf-tls-ecdhe-psk-aead-05
suite_definition(?TLS_ECDHE_PSK_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_128_gcm, 
      mac => null, 
      prf => sha256};
suite_definition(?TLS_ECDHE_PSK_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_256_gcm, 
      mac => null, 
      prf => sha384};
suite_definition(?TLS_ECDHE_PSK_WITH_AES_128_CCM_SHA256) ->
     #{key_exchange => ecdhe_psk, 
      cipher => aes_128_ccm, 
       mac => null, 
       prf =>sha256};
suite_definition(?TLS_ECDHE_PSK_WITH_AES_128_CCM_8_SHA256) ->
    #{key_exchange => ecdhe_psk, 
      cipher => aes_128_ccm_8, 
      mac => null, 
      prf =>sha256};
%%% SRP Cipher Suites RFC 5054
suite_definition(?TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => srp_anon, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => srp_rsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => srp_dss, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_SRP_SHA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => srp_anon, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => srp_rsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => srp_dss, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_SRP_SHA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => srp_anon, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => srp_rsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => srp_dss, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
%% RFC 4492 EC TLS suites
suite_definition(?TLS_ECDH_ECDSA_WITH_NULL_SHA) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_ECDSA_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_ECDSA_WITH_NULL_SHA) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_ECDSA_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_RSA_WITH_NULL_SHA) ->
    #{key_exchange => ecdh_rsa, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_RSA_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdh_rsa, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdh_rsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_RSA_WITH_NULL_SHA) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_RSA_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_anon_WITH_NULL_SHA) ->
    #{key_exchange => ecdh_anon, 
      cipher => null, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_anon_WITH_RC4_128_SHA) ->
    #{key_exchange => ecdh_anon, 
      cipher => rc4_128, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA) ->
    #{key_exchange => ecdh_anon, 
      cipher => '3des_ede_cbc', 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_anon_WITH_AES_128_CBC_SHA) ->
    #{key_exchange => ecdh_anon, 
      cipher => aes_128_cbc, 
      mac => sha, 
      prf => default_prf};
suite_definition(?TLS_ECDH_anon_WITH_AES_256_CBC_SHA) ->
    #{key_exchange => ecdh_anon, 
      cipher => aes_256_cbc, 
      mac => sha, 
      prf => default_prf};
%% RFC 5289 EC TLS suites
suite_definition(?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => sha256};
suite_definition(?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
suite_definition(?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => sha256};
suite_definition(?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
suite_definition(?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => sha256};
suite_definition(?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
suite_definition(?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_128_cbc, 
      mac => sha256, 
      prf => sha256};
suite_definition(?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA384) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_256_cbc, 
      mac => sha384, 
      prf => sha384};
%% RFC 5288 AES-GCM Cipher Suites
suite_definition(?TLS_RSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => rsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_RSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => rsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_DHE_RSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dhe_rsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_DHE_RSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dhe_rsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_DH_RSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dh_rsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_DH_RSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dh_rsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_DHE_DSS_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_DHE_DSS_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dhe_dss, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_DH_DSS_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dh_dss, 
      cipher => aes_128_gcm, 
      mac => null, 
      prf => sha256};
suite_definition(?TLS_DH_DSS_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dh_dss, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_DH_anon_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => dh_anon, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_DH_anon_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => dh_anon, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
%% RFC 5289 ECC AES-GCM Cipher Suites
suite_definition(?TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_ECDH_ECDSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_ECDH_ECDSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => ecdh_ecdsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_ECDH_RSA_WITH_AES_128_GCM_SHA256) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_128_gcm, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_ECDH_RSA_WITH_AES_256_GCM_SHA384) ->
    #{key_exchange => ecdh_rsa, 
      cipher => aes_256_gcm, 
      mac => aead, 
      prf => sha384};
suite_definition(?TLS_PSK_WITH_AES_128_CCM) ->
    #{key_exchange => psk,
      cipher => aes_128_ccm,
      mac => aead,
      prf => sha256};
suite_definition(?TLS_PSK_WITH_AES_256_CCM) -> 
    #{key_exchange => psk,
      cipher => aes_256_ccm,
      mac => aead,
      prf => sha256};
suite_definition(?TLS_DHE_PSK_WITH_AES_128_CCM) ->
    #{key_exchange => dhe_psk,
      cipher => aes_128_ccm,
      mac => aead,
      prf => sha256};
suite_definition(?TLS_DHE_PSK_WITH_AES_256_CCM) ->
    #{key_exchange => dhe_psk,
      cipher => aes_256_ccm,
      mac => aead,
      prf => sha256};
suite_definition(?TLS_PSK_WITH_AES_128_CCM_8) ->
    #{key_exchange => psk,
      cipher => aes_128_ccm_8,
      mac => aead,
      prf => sha256};
suite_definition(?TLS_PSK_WITH_AES_256_CCM_8) ->
    #{key_exchange => psk,
      cipher => aes_256_ccm_8,
      mac => aead,
      prf => sha256};
suite_definition(?TLS_PSK_DHE_WITH_AES_128_CCM_8) ->
  #{key_exchange => dhe_psk,
    cipher => aes_128_ccm_8,
    mac => aead,
    prf => sha256};
suite_definition(?TLS_PSK_DHE_WITH_AES_256_CCM_8) ->
  #{key_exchange => dhe_psk,
    cipher => aes_256_ccm_8,
    mac => aead,
    prf => sha256};
suite_definition(#{key_exchange := psk_dhe,
                   cipher := aes_256_ccm_8,
                   mac := aead,
                   prf := sha256}) ->
    ?TLS_PSK_DHE_WITH_AES_256_CCM_8;

% draft-agl-tls-chacha20poly1305-04 Chacha20/Poly1305 Suites
suite_definition(?TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256) ->
    #{key_exchange => ecdhe_rsa, 
      cipher => chacha20_poly1305, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256) ->
    #{key_exchange => ecdhe_ecdsa, 
      cipher => chacha20_poly1305, 
      mac => aead, 
      prf => sha256};
suite_definition(?TLS_DHE_RSA_WITH_CHACHA20_POLY1305_SHA256) ->
    #{key_exchange => dhe_rsa, 
      cipher => chacha20_poly1305, 
      mac => aead, 
      prf => sha256};
%% TLS 1.3 Cipher Suites RFC8446
suite_definition(?TLS_AES_128_GCM_SHA256) ->
    #{key_exchange => any,
      cipher => aes_128_gcm,
      mac => aead,
      prf => sha256};
suite_definition(?TLS_AES_256_GCM_SHA384) ->
    #{key_exchange => any,
      cipher => aes_256_gcm,
      mac => aead,
      prf => sha384};
suite_definition(?TLS_CHACHA20_POLY1305_SHA256) ->
    #{key_exchange => any,
      cipher => chacha20_poly1305,
      mac => aead,
      prf => sha256}.
%% suite_definition(?TLS_AES_128_CCM_SHA256) ->
%%      #{key_exchange => any,
%%        cipher => aes_128_ccm,
%%        mac => aead,
%%        prf => sha256};
%% suite_definition(?TLS_AES_128_CCM_8_SHA256) ->
%%      #{key_exchange => any,
%%       cipher => aes_128_ccm_8,
%%        mac => aead,
%%        prf => sha256}.

%%--------------------------------------------------------------------
-spec erl_suite_definition(cipher_suite() | internal_erl_cipher_suite()) -> old_erl_cipher_suite().
%%
%% Description: Return erlang cipher suite definition. Filters last value
%% for now (compatibility reasons).
%%--------------------------------------------------------------------
erl_suite_definition(Bin) when is_binary(Bin) ->
    erl_suite_definition(suite_definition(Bin));    
erl_suite_definition(#{key_exchange := KeyExchange, cipher := Cipher,
                       mac := Hash, prf := Prf}) ->
    case Prf of
        default_prf ->
	    {KeyExchange, Cipher, Hash};
	_ ->
            {KeyExchange, Cipher, Hash, Prf}
    end.

%%--------------------------------------------------------------------
-spec suite(internal_erl_cipher_suite()) -> cipher_suite().
%%
%% Description: Return TLS cipher suite definition.
%%--------------------------------------------------------------------
%% TLS v1.1 suites
suite(#{key_exchange := rsa, 
        cipher := rc4_128, 
        mac := md5}) ->
    ?TLS_RSA_WITH_RC4_128_MD5;
suite(#{key_exchange := rsa, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_RSA_WITH_RC4_128_SHA;
suite(#{key_exchange := rsa, 
        cipher := des_cbc, 
        mac := sha}) ->
    ?TLS_RSA_WITH_DES_CBC_SHA; 
suite(#{key_exchange := rsa, 
        cipher :='3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_RSA_WITH_3DES_EDE_CBC_SHA; 
suite(#{key_exchange := dhe_dss,  
        cipher:= des_cbc, 
        mac := sha}) ->
    ?TLS_DHE_DSS_WITH_DES_CBC_SHA;
suite(#{key_exchange := dhe_dss, 
        cipher:= '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := dhe_rsa, 
        cipher:= des_cbc,
        mac := sha}) ->
    ?TLS_DHE_RSA_WITH_DES_CBC_SHA;
suite(#{key_exchange := dhe_rsa, 
        cipher:= '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA; 
suite(#{key_exchange := dh_anon, 
        cipher:= rc4_128,
        mac := md5}) ->
    ?TLS_DH_anon_WITH_RC4_128_MD5;
suite(#{key_exchange := dh_anon, 
        cipher:= des_cbc,  
        mac := sha}) ->
    ?TLS_DH_anon_WITH_DES_CBC_SHA;
suite(#{key_exchange := dh_anon, 
        cipher:= '3des_ede_cbc',
        mac := sha}) ->
    ?TLS_DH_anon_WITH_3DES_EDE_CBC_SHA;
%%% TSL V1.1 AES suites
suite(#{key_exchange := rsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_RSA_WITH_AES_128_CBC_SHA; 
suite(#{key_exchange := dhe_dss, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA; 
suite(#{key_exchange := dhe_rsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := dh_anon, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_DH_anon_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := rsa, 
        cipher := aes_256_cbc,  
        mac := sha}) ->
    ?TLS_RSA_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := dhe_dss, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := dhe_rsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := dh_anon, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_DH_anon_WITH_AES_256_CBC_SHA;
%% TLS v1.2 suites
suite(#{key_exchange := rsa, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_RSA_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := rsa, 
        cipher := aes_256_cbc, 
        mac := sha256}) ->
    ?TLS_RSA_WITH_AES_256_CBC_SHA256;
suite(#{key_exchange := dhe_dss, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := dhe_rsa, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := dhe_dss, 
        cipher := aes_256_cbc, 
        mac := sha256}) ->
    ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA256;
suite(#{key_exchange := dhe_rsa, 
        cipher := aes_256_cbc, 
        mac := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA256;
suite(#{key_exchange := dh_anon, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_DH_anon_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := dh_anon, 
        cipher := aes_256_cbc, 
        mac := sha256}) ->
    ?TLS_DH_anon_WITH_AES_256_CBC_SHA256;
%%% PSK Cipher Suites RFC 4279
suite(#{key_exchange := psk, 
        cipher := rc4_128,
        mac := sha}) ->
    ?TLS_PSK_WITH_RC4_128_SHA;
suite(#{key_exchange := psk, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_PSK_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := psk, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_PSK_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := psk, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_PSK_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := dhe_psk, 
        cipher := rc4_128, 
        mac := sha})  ->
    ?TLS_DHE_PSK_WITH_RC4_128_SHA;
suite(#{key_exchange := dhe_psk, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := dhe_psk, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_DHE_PSK_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := dhe_psk, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_DHE_PSK_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := rsa_psk, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_RSA_PSK_WITH_RC4_128_SHA;
suite(#{key_exchange := rsa_psk, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := rsa_psk, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_RSA_PSK_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := rsa_psk, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_RSA_PSK_WITH_AES_256_CBC_SHA;
%%% PSK NULL Cipher Suites RFC 4785
suite(#{key_exchange := psk, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_PSK_WITH_NULL_SHA;
suite(#{key_exchange := dhe_psk, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_DHE_PSK_WITH_NULL_SHA;
suite(#{key_exchange := rsa_psk, 
       cipher := null, 
       mac := sha}) ->
    ?TLS_RSA_PSK_WITH_NULL_SHA;
%%% TLS 1.2 PSK Cipher Suites RFC 5487
suite(#{key_exchange := psk, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_PSK_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := psk, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_PSK_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := dhe_psk, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DHE_PSK_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := dhe_psk, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DHE_PSK_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := rsa_psk, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_RSA_PSK_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := rsa_psk, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_RSA_PSK_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := psk, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_PSK_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := psk, 
        cipher := aes_256_cbc, 
        mac := sha384}) ->
    ?TLS_PSK_WITH_AES_256_CBC_SHA384;
suite(#{key_exchange := dhe_psk, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_DHE_PSK_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := dhe_psk, 
        cipher := aes_256_cbc, 
        mac := sha384}) ->
    ?TLS_DHE_PSK_WITH_AES_256_CBC_SHA384;
suite(#{key_exchange := rsa_psk, 
        cipher := aes_128_cbc, 
        mac := sha256}) ->
    ?TLS_RSA_PSK_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := rsa_psk, 
        cipher := aes_256_cbc, 
        mac := sha384}) ->
    ?TLS_RSA_PSK_WITH_AES_256_CBC_SHA384;
suite(#{key_exchange := psk, 
        cipher := null, 
        mac := sha256}) ->
    ?TLS_PSK_WITH_NULL_SHA256;
suite(#{key_exchange := psk, 
        cipher := null,
        mac := sha384}) ->
    ?TLS_PSK_WITH_NULL_SHA384;
suite(#{key_exchange := dhe_psk, 
        cipher := null, 
        mac := sha256}) ->
    ?TLS_DHE_PSK_WITH_NULL_SHA256;
suite(#{key_exchange := dhe_psk, 
        cipher := null, 
        mac := sha384}) ->
    ?TLS_DHE_PSK_WITH_NULL_SHA384;
suite(#{key_exchange := rsa_psk, 
        cipher := null,  
        mac := sha256}) ->
    ?TLS_RSA_PSK_WITH_NULL_SHA256;
suite(#{key_exchange := rsa_psk, 
       cipher := null, 
       mac := sha384}) ->
    ?TLS_RSA_PSK_WITH_NULL_SHA384;
%%% ECDHE PSK Cipher Suites RFC 5489
suite(#{key_exchange := ecdhe_psk, 
        cipher := rc4_128,
        mac := sha}) ->
    ?TLS_ECDHE_PSK_WITH_RC4_128_SHA;
suite(#{key_exchange := ecdhe_psk, 
        cipher :='3des_ede_cbc',
        mac := sha}) ->
    ?TLS_ECDHE_PSK_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := ecdhe_psk, 
        cipher := aes_128_cbc,
        mac := sha}) ->
    ?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := ecdhe_psk, 
        cipher := aes_256_cbc,
        mac := sha}) ->
    ?TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := ecdhe_psk, 
       cipher := aes_128_cbc, 
       mac := sha256}) ->
    ?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := ecdhe_psk, 
       cipher := aes_256_cbc, 
       mac := sha384}) ->
    ?TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA384;
suite(#{key_exchange := ecdhe_psk, 
       cipher := null, 
       mac := sha256}) ->
    ?TLS_ECDHE_PSK_WITH_NULL_SHA256;
suite(#{key_exchange := ecdhe_psk, 
       cipher := null, 
       mac := sha384}) ->
    ?TLS_ECDHE_PSK_WITH_NULL_SHA384;
%%% ECDHE_PSK with AES-GCM and AES-CCM Cipher Suites, draft-ietf-tls-ecdhe-psk-aead-05
suite(#{key_exchange := ecdhe_psk, 
       cipher := aes_128_gcm, 
       mac := null, 
       prf := sha256}) ->
    ?TLS_ECDHE_PSK_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := ecdhe_psk, 
       cipher := aes_256_gcm, 
       mac := null, 
       prf := sha384}) ->
    ?TLS_ECDHE_PSK_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := ecdhe_psk, 
        cipher := aes_128_ccm_8, 
        mac := null, 
        prf := sha256}) ->
    ?TLS_ECDHE_PSK_WITH_AES_128_CCM_8_SHA256;
suite(#{key_exchange := ecdhe_psk, 
        cipher := aes_128_ccm, 
         mac := null, 
         prf := sha256}) ->
    ?TLS_ECDHE_PSK_WITH_AES_128_CCM_SHA256;
%%% SRP Cipher Suites RFC 5054
suite(#{key_exchange := srp_anon, 
        cipher := '3des_ede_cbc',
        mac :=  sha}) ->
    ?TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := srp_rsa, 
        cipher := '3des_ede_cbc',
        mac := sha}) ->
    ?TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := srp_dss, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := srp_anon, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := srp_rsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := srp_dss, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := srp_anon, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := srp_rsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := srp_dss, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA;
%%% RFC 4492 EC TLS suites
suite(#{key_exchange := ecdh_ecdsa, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_ECDH_ECDSA_WITH_NULL_SHA;
suite(#{key_exchange := ecdh_ecdsa, 
        cipher := rc4_128,  
        mac := sha})  ->
    ?TLS_ECDH_ECDSA_WITH_RC4_128_SHA;
suite(#{key_exchange := ecdh_ecdsa, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := ecdhe_ecdsa, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_ECDHE_ECDSA_WITH_NULL_SHA;
suite(#{key_exchange := ecdhe_ecdsa, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_ECDHE_ECDSA_WITH_RC4_128_SHA;
suite(#{key_exchange := ecdhe_ecdsa, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := ecdh_rsa, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_ECDH_RSA_WITH_NULL_SHA;
suite(#{key_exchange := ecdh_rsa, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_ECDH_RSA_WITH_RC4_128_SHA;
suite(#{key_exchange := ecdh_rsa, 
        cipher := '3des_ede_cbc', mac := sha}) ->
    ?TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := ecdh_rsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := ecdh_rsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := ecdhe_rsa, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_ECDHE_RSA_WITH_NULL_SHA;
suite(#{key_exchange := ecdhe_rsa, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_ECDHE_RSA_WITH_RC4_128_SHA;
suite(#{key_exchange := ecdhe_rsa, 
        cipher := '3des_ede_cbc', 
        mac := sha}) ->
    ?TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := ecdhe_rsa, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := ecdhe_rsa, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA;
suite(#{key_exchange := ecdh_anon, 
        cipher := null, 
        mac := sha}) ->
    ?TLS_ECDH_anon_WITH_NULL_SHA;
suite(#{key_exchange := ecdh_anon, 
        cipher := rc4_128, 
        mac := sha}) ->
    ?TLS_ECDH_anon_WITH_RC4_128_SHA;
suite(#{key_exchange := ecdh_anon, 
        cipher := '3des_ede_cbc', 
        mac :=  sha}) ->
    ?TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA;
suite(#{key_exchange := ecdh_anon, 
        cipher := aes_128_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_anon_WITH_AES_128_CBC_SHA;
suite(#{key_exchange := ecdh_anon, 
        cipher := aes_256_cbc, 
        mac := sha}) ->
    ?TLS_ECDH_anon_WITH_AES_256_CBC_SHA;
%%% RFC 5289 EC TLS suites
suite(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_128_cbc, 
        mac:= sha256, 
        prf := sha256}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_256_cbc, 
        mac := sha384, 
        prf := sha384}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384;
suite(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_128_cbc, 
        mac := sha256, 
        prf := sha256}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_256_cbc, 
        mac := sha384, 
        prf := sha384}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA384;
suite(#{key_exchange := ecdhe_rsa, 
        cipher := aes_128_cbc, 
        mac := sha256, 
        prf := sha256}) ->
    ?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := ecdhe_rsa, 
        cipher := aes_256_cbc, 
        mac := sha384, 
        prf := sha384}) ->
    ?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384;
suite(#{key_exchange := ecdh_rsa, 
        cipher := aes_128_cbc, 
        mac := sha256, 
        prf := sha256}) ->
    ?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256;
suite(#{key_exchange := ecdh_rsa, 
        cipher := aes_256_cbc, 
        mac := sha384, 
        prf := sha384}) ->
    ?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA384;
%% RFC 5288 AES-GCM Cipher Suites
suite(#{key_exchange := rsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_RSA_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := rsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_RSA_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := dhe_rsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := dhe_rsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DHE_RSA_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := dh_rsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DH_RSA_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := dh_rsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DH_RSA_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := dhe_dss, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DHE_DSS_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := dhe_dss, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DHE_DSS_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := dh_dss, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DH_DSS_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := dh_dss, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DH_DSS_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := dh_anon, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DH_anon_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := dh_anon, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_DH_anon_WITH_AES_256_GCM_SHA384;
%% RFC 5289 ECC AES-GCM Cipher Suites
suite(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := ecdhe_ecdsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := ecdh_ecdsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_ECDH_ECDSA_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := ecdhe_rsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := ecdhe_rsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384;
suite(#{key_exchange := ecdh_rsa, 
        cipher := aes_128_gcm, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDH_RSA_WITH_AES_128_GCM_SHA256;
suite(#{key_exchange := ecdh_rsa, 
        cipher := aes_256_gcm, 
        mac := aead, 
        prf := sha384}) ->
    ?TLS_ECDH_RSA_WITH_AES_256_GCM_SHA384;
%% draft-agl-tls-chacha20poly1305-04 Chacha20/Poly1305 Suites
suite(#{key_exchange := ecdhe_rsa, 
        cipher := chacha20_poly1305,  
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256;
suite(#{key_exchange := ecdhe_ecdsa, 
        cipher := chacha20_poly1305, 
        mac := aead, 
        prf := sha256}) ->
    ?TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256;
suite(#{key_exchange := dhe_rsa, 
        cipher := chacha20_poly1305,  
        mac := aead, 
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_CHACHA20_POLY1305_SHA256;

%% RFC 6655 - TLS-1.2 cipher suites
suite(#{key_exchange := psk,
        cipher := aes_128_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_WITH_AES_128_CCM;
suite(#{key_exchange := psk,
        cipher := aes_256_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_WITH_AES_256_CCM;
suite(#{key_exchange := dhe_psk,
        cipher := aes_128_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_PSK_WITH_AES_128_CCM;
suite(#{key_exchange := dhe_psk,
        cipher := aes_256_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_PSK_WITH_AES_256_CCM;
suite(#{key_exchange := rsa,
        cipher := aes_128_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_RSA_WITH_AES_128_CCM;
suite(#{key_exchange := rsa,
        cipher := aes_256_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_RSA_WITH_AES_256_CCM;
suite(#{key_exchange := dhe_rsa,
        cipher := aes_128_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_128_CCM;
suite(#{key_exchange := dhe_rsa,
        cipher := aes_256_ccm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_256_CCM;

suite(#{key_exchange := psk,
        cipher := aes_128_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_WITH_AES_128_CCM_8;
suite(#{key_exchange := psk,
        cipher := aes_256_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_WITH_AES_256_CCM_8;
suite(#{key_exchange := dhe_psk,
        cipher := aes_128_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_DHE_WITH_AES_128_CCM_8;
suite(#{key_exchange := dhe_psk,
        cipher := aes_256_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_PSK_DHE_WITH_AES_256_CCM_8;
suite(#{key_exchange := rsa,
        cipher := aes_128_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_RSA_WITH_AES_128_CCM_8;
suite(#{key_exchange := rsa,
        cipher := aes_256_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_RSA_WITH_AES_256_CCM_8;
suite(#{key_exchange := dhe_rsa,
        cipher := aes_128_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_128_CCM_8;
suite(#{key_exchange := dhe_rsa,
        cipher := aes_256_ccm_8,
        mac := aead,
        prf := sha256}) ->
    ?TLS_DHE_RSA_WITH_AES_256_CCM_8;

%% TLS 1.3 Cipher Suites RFC8446
suite(#{key_exchange := any,
        cipher := aes_128_gcm,
        mac := aead,
        prf := sha256}) ->
    ?TLS_AES_128_GCM_SHA256;
suite(#{key_exchange := any,
      cipher := aes_256_gcm,
      mac := aead,
      prf := sha384}) ->
    ?TLS_AES_256_GCM_SHA384;
suite(#{key_exchange := any,
      cipher := chacha20_poly1305,
      mac := aead,
      prf := sha256}) ->
    ?TLS_CHACHA20_POLY1305_SHA256.
%% suite(#{key_exchange := any,
%%       cipher := aes_128_ccm,
%%       mac := aead,
%%       prf := sha256}) ->
%%     ?TLS_AES_128_CCM_SHA256;
%% suite(#{key_exchange := any,
%%       cipher := aes_128_ccm_8,
%%       mac := aead,
%%       prf := sha256}) ->
%%     ?TLS_AES_128_CCM_8_SHA256.
%%--------------------------------------------------------------------
-spec openssl_suite(openssl_cipher_suite()) -> cipher_suite().
%%
%% Description: Return TLS cipher suite definition.
%%--------------------------------------------------------------------
%% translate constants <-> openssl-strings
openssl_suite("DHE-RSA-AES256-SHA256") ->
    ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA256;
openssl_suite("DHE-DSS-AES256-SHA256") ->
    ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA256;
openssl_suite("AES256-SHA256") ->
    ?TLS_RSA_WITH_AES_256_CBC_SHA256;
openssl_suite("DHE-RSA-AES128-SHA256") ->
    ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA256;
openssl_suite("DHE-DSS-AES128-SHA256") ->
    ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA256;
openssl_suite("AES128-SHA256") ->
    ?TLS_RSA_WITH_AES_128_CBC_SHA256;
openssl_suite("DHE-RSA-AES256-SHA") ->
    ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA;
openssl_suite("DHE-DSS-AES256-SHA") ->
    ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA;
openssl_suite("AES256-SHA") ->
    ?TLS_RSA_WITH_AES_256_CBC_SHA;
openssl_suite("EDH-RSA-DES-CBC3-SHA") ->
    ?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA;
openssl_suite("EDH-DSS-DES-CBC3-SHA") ->
    ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA;
openssl_suite("DES-CBC3-SHA") ->
    ?TLS_RSA_WITH_3DES_EDE_CBC_SHA;
openssl_suite("DHE-RSA-AES128-SHA") ->
    ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA;
openssl_suite("DHE-DSS-AES128-SHA") ->
    ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA;
openssl_suite("AES128-SHA") ->
    ?TLS_RSA_WITH_AES_128_CBC_SHA;
openssl_suite("RC4-SHA") ->
    ?TLS_RSA_WITH_RC4_128_SHA;
openssl_suite("RC4-MD5") -> 
    ?TLS_RSA_WITH_RC4_128_MD5;
openssl_suite("EDH-RSA-DES-CBC-SHA") ->
    ?TLS_DHE_RSA_WITH_DES_CBC_SHA;
openssl_suite("DES-CBC-SHA") ->
    ?TLS_RSA_WITH_DES_CBC_SHA;

%%% SRP Cipher Suites RFC 5054

openssl_suite("SRP-DSS-AES-256-CBC-SHA") ->
    ?TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA;
openssl_suite("SRP-RSA-AES-256-CBC-SHA") ->
    ?TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA;
openssl_suite("SRP-DSS-3DES-EDE-CBC-SHA") ->
    ?TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA;
openssl_suite("SRP-RSA-3DES-EDE-CBC-SHA") ->
    ?TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA;
openssl_suite("SRP-DSS-AES-128-CBC-SHA") ->
    ?TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA;
openssl_suite("SRP-RSA-AES-128-CBC-SHA") ->
    ?TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA;

%% RFC 4492 EC TLS suites
openssl_suite("ECDH-ECDSA-RC4-SHA") ->
    ?TLS_ECDH_ECDSA_WITH_RC4_128_SHA;
openssl_suite("ECDH-ECDSA-DES-CBC3-SHA") ->
    ?TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA;
openssl_suite("ECDH-ECDSA-AES128-SHA") ->
    ?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA;
openssl_suite("ECDH-ECDSA-AES256-SHA") ->
    ?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA;

openssl_suite("ECDHE-ECDSA-RC4-SHA") ->
    ?TLS_ECDHE_ECDSA_WITH_RC4_128_SHA;
openssl_suite("ECDHE-ECDSA-DES-CBC3-SHA") ->
    ?TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA;
openssl_suite("ECDHE-ECDSA-AES128-SHA") ->
    ?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA;
openssl_suite("ECDHE-ECDSA-AES256-SHA") ->
    ?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA;

openssl_suite("ECDHE-RSA-RC4-SHA") ->
    ?TLS_ECDHE_RSA_WITH_RC4_128_SHA;
openssl_suite("ECDHE-RSA-DES-CBC3-SHA") ->
    ?TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA;
openssl_suite("ECDHE-RSA-AES128-SHA") ->
    ?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA;
openssl_suite("ECDHE-RSA-AES256-SHA") ->
    ?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA;

openssl_suite("ECDH-RSA-RC4-SHA") ->
    ?TLS_ECDH_RSA_WITH_RC4_128_SHA;
openssl_suite("ECDH-RSA-DES-CBC3-SHA") ->
    ?TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA;
openssl_suite("ECDH-RSA-AES128-SHA") ->
    ?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA;
openssl_suite("ECDH-RSA-AES256-SHA") ->
    ?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA;

%% RFC 5289 EC TLS suites
openssl_suite("ECDHE-ECDSA-AES128-SHA256") ->
    ?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256;
openssl_suite("ECDHE-ECDSA-AES256-SHA384") ->
    ?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384;
openssl_suite("ECDH-ECDSA-AES128-SHA256") ->
    ?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA256;
openssl_suite("ECDH-ECDSA-AES256-SHA384") ->
    ?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA384;
openssl_suite("ECDHE-RSA-AES128-SHA256") ->
    ?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256;
openssl_suite("ECDHE-RSA-AES256-SHA384") ->
    ?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384;
openssl_suite("ECDH-RSA-AES128-SHA256") ->
    ?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256;
openssl_suite("ECDH-RSA-AES256-SHA384") ->
    ?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA384;

%% RFC 5288 AES-GCM Cipher Suites
openssl_suite("AES128-GCM-SHA256") ->
    ?TLS_RSA_WITH_AES_128_GCM_SHA256;
openssl_suite("AES256-GCM-SHA384") ->
    ?TLS_RSA_WITH_AES_256_GCM_SHA384;
openssl_suite("DHE-RSA-AES128-GCM-SHA256") ->
    ?TLS_DHE_RSA_WITH_AES_128_GCM_SHA256;
openssl_suite("DHE-RSA-AES256-GCM-SHA384") ->
    ?TLS_DHE_RSA_WITH_AES_256_GCM_SHA384;
openssl_suite("DH-RSA-AES128-GCM-SHA256") ->
    ?TLS_DH_RSA_WITH_AES_128_GCM_SHA256;
openssl_suite("DH-RSA-AES256-GCM-SHA384") ->
    ?TLS_DH_RSA_WITH_AES_256_GCM_SHA384;
openssl_suite("DHE-DSS-AES128-GCM-SHA256") ->
    ?TLS_DHE_DSS_WITH_AES_128_GCM_SHA256;
openssl_suite("DHE-DSS-AES256-GCM-SHA384") ->
    ?TLS_DHE_DSS_WITH_AES_256_GCM_SHA384;
openssl_suite("DH-DSS-AES128-GCM-SHA256") ->
    ?TLS_DH_DSS_WITH_AES_128_GCM_SHA256;
openssl_suite("DH-DSS-AES256-GCM-SHA384") ->
    ?TLS_DH_DSS_WITH_AES_256_GCM_SHA384;

%% RFC 5289 ECC AES-GCM Cipher Suites
openssl_suite("ECDHE-ECDSA-AES128-GCM-SHA256") ->
    ?TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256;
openssl_suite("ECDHE-ECDSA-AES256-GCM-SHA384") ->
    ?TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384;
openssl_suite("ECDH-ECDSA-AES128-GCM-SHA256") ->
    ?TLS_ECDH_ECDSA_WITH_AES_128_GCM_SHA256;
openssl_suite("ECDH-ECDSA-AES256-GCM-SHA384") ->
    ?TLS_ECDH_ECDSA_WITH_AES_256_GCM_SHA384;
openssl_suite("ECDHE-RSA-AES128-GCM-SHA256") ->
    ?TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256;
openssl_suite("ECDHE-RSA-AES256-GCM-SHA384") ->
    ?TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384;
openssl_suite("ECDH-RSA-AES128-GCM-SHA256") ->
    ?TLS_ECDH_RSA_WITH_AES_128_GCM_SHA256;
openssl_suite("ECDH-RSA-AES256-GCM-SHA384") ->
    ?TLS_ECDH_RSA_WITH_AES_256_GCM_SHA384;

%% TLS 1.3 Cipher Suites RFC8446
openssl_suite("TLS_AES_128_GCM_SHA256") ->
    ?TLS_AES_128_GCM_SHA256;
openssl_suite("TLS_AES_256_GCM_SHA384") ->
    ?TLS_AES_256_GCM_SHA384;
openssl_suite("TLS_CHACHA20_POLY1305_SHA256") ->
    ?TLS_CHACHA20_POLY1305_SHA256.
%% openssl_suite("TLS_AES_128_CCM_SHA256") ->
%%     ?TLS_AES_128_CCM_SHA256;
%% openssl_suite("TLS_AES_128_CCM_8_SHA256") ->
%%     ?TLS_AES_128_CCM_8_SHA256.


%%--------------------------------------------------------------------
-spec openssl_suite_name(cipher_suite()) -> openssl_cipher_suite() | internal_erl_cipher_suite().
%%
%% Description: Return openssl cipher suite name if possible
%%-------------------------------------------------------------------
openssl_suite_name(?TLS_DHE_RSA_WITH_AES_256_CBC_SHA) ->
    "DHE-RSA-AES256-SHA";
openssl_suite_name(?TLS_DHE_DSS_WITH_AES_256_CBC_SHA) ->
    "DHE-DSS-AES256-SHA";
openssl_suite_name(?TLS_RSA_WITH_AES_256_CBC_SHA) ->
    "AES256-SHA";
openssl_suite_name(?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA) ->
    "EDH-RSA-DES-CBC3-SHA";
openssl_suite_name(?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA) ->
    "EDH-DSS-DES-CBC3-SHA";
openssl_suite_name(?TLS_RSA_WITH_3DES_EDE_CBC_SHA) ->
    "DES-CBC3-SHA";
openssl_suite_name( ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA) ->
    "DHE-RSA-AES128-SHA";
openssl_suite_name(?TLS_DHE_DSS_WITH_AES_128_CBC_SHA) ->
    "DHE-DSS-AES128-SHA";
openssl_suite_name(?TLS_RSA_WITH_AES_128_CBC_SHA) ->
    "AES128-SHA";
openssl_suite_name(?TLS_RSA_WITH_RC4_128_SHA) ->
    "RC4-SHA";
openssl_suite_name(?TLS_RSA_WITH_RC4_128_MD5) -> 
    "RC4-MD5";
openssl_suite_name(?TLS_DHE_RSA_WITH_DES_CBC_SHA) ->
    "EDH-RSA-DES-CBC-SHA";
openssl_suite_name(?TLS_RSA_WITH_DES_CBC_SHA) ->
    "DES-CBC-SHA";
openssl_suite_name(?TLS_RSA_WITH_NULL_SHA256) ->
    "NULL-SHA256";
openssl_suite_name(?TLS_RSA_WITH_AES_128_CBC_SHA256) ->
    "AES128-SHA256";
openssl_suite_name(?TLS_RSA_WITH_AES_256_CBC_SHA256) ->
    "AES256-SHA256";
openssl_suite_name(?TLS_DH_DSS_WITH_AES_128_CBC_SHA256) ->
    "DH-DSS-AES128-SHA256";
openssl_suite_name(?TLS_DH_RSA_WITH_AES_128_CBC_SHA256) ->
    "DH-RSA-AES128-SHA256";
openssl_suite_name(?TLS_DHE_DSS_WITH_AES_128_CBC_SHA256) ->
    "DHE-DSS-AES128-SHA256";
openssl_suite_name(?TLS_DHE_RSA_WITH_AES_128_CBC_SHA256) ->
    "DHE-RSA-AES128-SHA256";
openssl_suite_name(?TLS_DH_DSS_WITH_AES_256_CBC_SHA256) ->
    "DH-DSS-AES256-SHA256";
openssl_suite_name(?TLS_DH_RSA_WITH_AES_256_CBC_SHA256) ->
    "DH-RSA-AES256-SHA256";
openssl_suite_name(?TLS_DHE_DSS_WITH_AES_256_CBC_SHA256) ->
    "DHE-DSS-AES256-SHA256";
openssl_suite_name(?TLS_DHE_RSA_WITH_AES_256_CBC_SHA256) ->
    "DHE-RSA-AES256-SHA256";

%%% PSK Cipher Suites RFC 4279

openssl_suite_name(?TLS_PSK_WITH_AES_256_CBC_SHA) ->
    "PSK-AES256-CBC-SHA";
openssl_suite_name(?TLS_PSK_WITH_3DES_EDE_CBC_SHA) ->
    "PSK-3DES-EDE-CBC-SHA";
openssl_suite_name(?TLS_PSK_WITH_AES_128_CBC_SHA) ->
    "PSK-AES128-CBC-SHA";
openssl_suite_name(?TLS_PSK_WITH_RC4_128_SHA) ->
    "PSK-RC4-SHA";

%%% SRP Cipher Suites RFC 5054

openssl_suite_name(?TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA) ->
    "SRP-RSA-3DES-EDE-CBC-SHA";
openssl_suite_name(?TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA) ->
    "SRP-DSS-3DES-EDE-CBC-SHA";
openssl_suite_name(?TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA) ->
    "SRP-RSA-AES-128-CBC-SHA";
openssl_suite_name(?TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA) ->
    "SRP-DSS-AES-128-CBC-SHA";
openssl_suite_name(?TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA) ->
    "SRP-RSA-AES-256-CBC-SHA";
openssl_suite_name(?TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA) ->
    "SRP-DSS-AES-256-CBC-SHA";

%% RFC 4492 EC TLS suites
openssl_suite_name(?TLS_ECDH_ECDSA_WITH_RC4_128_SHA) ->
    "ECDH-ECDSA-RC4-SHA";
openssl_suite_name(?TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA) ->
    "ECDH-ECDSA-DES-CBC3-SHA";
openssl_suite_name(?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA) ->
    "ECDH-ECDSA-AES128-SHA";
openssl_suite_name(?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA) ->
    "ECDH-ECDSA-AES256-SHA";

openssl_suite_name(?TLS_ECDHE_ECDSA_WITH_RC4_128_SHA) ->
    "ECDHE-ECDSA-RC4-SHA";
openssl_suite_name(?TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA) ->
    "ECDHE-ECDSA-DES-CBC3-SHA";
openssl_suite_name(?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA) ->
    "ECDHE-ECDSA-AES128-SHA";
openssl_suite_name(?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA) ->
    "ECDHE-ECDSA-AES256-SHA";

openssl_suite_name(?TLS_ECDH_RSA_WITH_RC4_128_SHA) ->
    "ECDH-RSA-RC4-SHA";
openssl_suite_name(?TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA) ->
    "ECDH-RSA-DES-CBC3-SHA";
openssl_suite_name(?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA) ->
    "ECDH-RSA-AES128-SHA";
openssl_suite_name(?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA) ->
    "ECDH-RSA-AES256-SHA";

openssl_suite_name(?TLS_ECDHE_RSA_WITH_RC4_128_SHA) ->
    "ECDHE-RSA-RC4-SHA";
openssl_suite_name(?TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA) ->
    "ECDHE-RSA-DES-CBC3-SHA";
openssl_suite_name(?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA) ->
    "ECDHE-RSA-AES128-SHA";
openssl_suite_name(?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA) ->
    "ECDHE-RSA-AES256-SHA";

%% RFC 5289 EC TLS suites
openssl_suite_name(?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256) ->
    "ECDHE-ECDSA-AES128-SHA256";
openssl_suite_name(?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384) ->
    "ECDHE-ECDSA-AES256-SHA384";
openssl_suite_name(?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA256) ->
    "ECDH-ECDSA-AES128-SHA256";
openssl_suite_name(?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA384) ->
    "ECDH-ECDSA-AES256-SHA384";
openssl_suite_name(?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256) ->
    "ECDHE-RSA-AES128-SHA256";
openssl_suite_name(?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384) ->
    "ECDHE-RSA-AES256-SHA384";
openssl_suite_name(?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256) ->
    "ECDH-RSA-AES128-SHA256";
openssl_suite_name(?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA384) ->
    "ECDH-RSA-AES256-SHA384";

%% RFC 5288 AES-GCM Cipher Suites
openssl_suite_name(?TLS_RSA_WITH_AES_128_GCM_SHA256) ->
    "AES128-GCM-SHA256";
openssl_suite_name(?TLS_RSA_WITH_AES_256_GCM_SHA384) ->
    "AES256-GCM-SHA384";
openssl_suite_name(?TLS_DHE_RSA_WITH_AES_128_GCM_SHA256) ->
    "DHE-RSA-AES128-GCM-SHA256";
openssl_suite_name(?TLS_DHE_RSA_WITH_AES_256_GCM_SHA384) ->
    "DHE-RSA-AES256-GCM-SHA384";
openssl_suite_name(?TLS_DH_RSA_WITH_AES_128_GCM_SHA256) ->
    "DH-RSA-AES128-GCM-SHA256";
openssl_suite_name(?TLS_DH_RSA_WITH_AES_256_GCM_SHA384) ->
    "DH-RSA-AES256-GCM-SHA384";
openssl_suite_name(?TLS_DHE_DSS_WITH_AES_128_GCM_SHA256) ->
    "DHE-DSS-AES128-GCM-SHA256";
openssl_suite_name(?TLS_DHE_DSS_WITH_AES_256_GCM_SHA384) ->
    "DHE-DSS-AES256-GCM-SHA384";
openssl_suite_name(?TLS_DH_DSS_WITH_AES_128_GCM_SHA256) ->
    "DH-DSS-AES128-GCM-SHA256";
openssl_suite_name(?TLS_DH_DSS_WITH_AES_256_GCM_SHA384) ->
    "DH-DSS-AES256-GCM-SHA384";

%% RFC 5289 ECC AES-GCM Cipher Suites
openssl_suite_name(?TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256) ->
    "ECDHE-ECDSA-AES128-GCM-SHA256";
openssl_suite_name(?TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384) ->
    "ECDHE-ECDSA-AES256-GCM-SHA384";
openssl_suite_name(?TLS_ECDH_ECDSA_WITH_AES_128_GCM_SHA256) ->
    "ECDH-ECDSA-AES128-GCM-SHA256";
openssl_suite_name(?TLS_ECDH_ECDSA_WITH_AES_256_GCM_SHA384) ->
    "ECDH-ECDSA-AES256-GCM-SHA384";
openssl_suite_name(?TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256) ->
    "ECDHE-RSA-AES128-GCM-SHA256";
openssl_suite_name(?TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384) ->
    "ECDHE-RSA-AES256-GCM-SHA384";
openssl_suite_name(?TLS_ECDH_RSA_WITH_AES_128_GCM_SHA256) ->
    "ECDH-RSA-AES128-GCM-SHA256";
openssl_suite_name(?TLS_ECDH_RSA_WITH_AES_256_GCM_SHA384) ->
    "ECDH-RSA-AES256-GCM-SHA384";

%% ChaCha20-Poly1305 Cipher Suites for Transport Layer Security (TLS) RFC7905
openssl_suite_name(?TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256) ->
    "TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256";
openssl_suite_name(?TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256) ->
    "TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256";
openssl_suite_name(?TLS_DHE_RSA_WITH_CHACHA20_POLY1305_SHA256) ->
    "TLS_DHE_RSA_WITH_CHACHA20_POLY1305_SHA256";
openssl_suite_name(?TLS_PSK_WITH_CHACHA20_POLY1305_SHA256) ->
    "TLS_PSK_WITH_CHACHA20_POLY1305_SHA256";
openssl_suite_name(?TLS_ECDHE_PSK_WITH_CHACHA20_POLY1305_SHA256) ->
    "TLS_ECDHE_PSK_WITH_CHACHA20_POLY1305_SHA256";
openssl_suite_name(?TLS_DHE_PSK_WITH_CHACHA20_POLY1305_SHA256) ->
    "TLS_DHE_PSK_WITH_CHACHA20_POLY1305_SHA256";
openssl_suite_name(?TLS_RSA_PSK_WITH_CHACHA20_POLY1305_SHA256) ->
    "TLS_RSA_PSK_WITH_CHACHA20_POLY1305_SHA256";

%% TLS 1.3 Cipher Suites RFC8446
openssl_suite_name(?TLS_AES_128_GCM_SHA256) ->
    "TLS_AES_128_GCM_SHA256";
openssl_suite_name(?TLS_AES_256_GCM_SHA384) ->
    "TLS_AES_256_GCM_SHA384";
openssl_suite_name(?TLS_CHACHA20_POLY1305_SHA256) ->
    "TLS_CHACHA20_POLY1305_SHA256";
%% openssl_suite(?TLS_AES_128_CCM_SHA256) ->
%%     "TLS_AES_128_CCM_SHA256";
%% openssl_suite(?TLS_AES_128_CCM_8_SHA256) ->
%%     "TLS_AES_128_CCM_8_SHA256";

%% No oppenssl name
openssl_suite_name(Cipher) ->
    suite_definition(Cipher).
