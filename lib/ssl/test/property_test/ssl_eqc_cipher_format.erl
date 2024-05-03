%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2023. All Rights Reserved.
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

-module(ssl_eqc_cipher_format).

-compile(export_all).

-proptest(eqc).
-proptest([triq,proper]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
-define(EQC,true).
-endif.
-endif.
-endif.

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(MOD_eqc,eqc).

-else.
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-else.
-ifdef(TRIQ).
-define(MOD_eqc,triq).
-include_lib("triq/include/triq.hrl").

-endif.
-endif.
-endif.

-include_lib("public_key/include/public_key.hrl").

-define('TLS_v1.3', 'tlsv1.3').
-define('TLS_v1.2', 'tlsv1.2').
-define('TLS_v1.1', 'tlsv1.1').
-define('TLS_v1',   'tlsv1').

%%--------------------------------------------------------------------
%% Properties --------------------------------------------------------
%%--------------------------------------------------------------------

prop_tls_cipher_suite_rfc_name() ->
    ?FORALL({CipherSuite, _TLSVersion}, ?LET(Version, tls_version(), {cipher_suite(Version), Version}),
            case ssl:str_to_suite(ssl:suite_to_str(CipherSuite)) of
		CipherSuite ->
		    true;
		_ ->
		    false
	    end
	   ).

prop_tls_cipher_suite_openssl_name() ->
    ?FORALL({CipherSuite, _TLSVersion}, ?LET(Version, tls_version(), {cipher_suite(Version), Version}),
            case ssl:str_to_suite(ssl:suite_to_openssl_str(CipherSuite)) of
		CipherSuite ->                    
		    case ssl:suite_to_openssl_str(CipherSuite) of
                        "TLS_" ++ _ ->
                            true;
                        OpensslName ->
                            lists:member(OpensslName, openssl_legacy_names())
                    end; 
                _ ->
                    false
            end
	   ).

prop_tls_anon_cipher_suite_rfc_name() ->
    ?FORALL({CipherSuite, _TLSVersion}, ?LET(Version, pre_tls_1_3_version(), {anon_cipher_suite(Version), Version}),
            case ssl:str_to_suite(ssl:suite_to_str(CipherSuite)) of
		CipherSuite ->
		    true;
		_ ->
		    false
	    end
	   ).

prop_tls_anon_cipher_suite_openssl_name() ->
    ?FORALL({CipherSuite, _TLSVersion}, ?LET(Version, pre_tls_1_3_version(), {anon_cipher_suite(Version), Version}),
            case ssl:str_to_suite(ssl:suite_to_openssl_str(CipherSuite)) of
		CipherSuite ->
		    lists:member(ssl:suite_to_openssl_str(CipherSuite), openssl_legacy_names());
		_ ->
		    false
	    end
	   ).

prop_tls_signature_algs() ->
    ?FORALL(SigAlg, ?LET(SigAlg, sig_alg(), SigAlg),
            true = lists:member(ssl_cipher:signature_algorithm_to_scheme(SigAlg), sig_schemes())
	   ).

%%--------------------------------------------------------------------
%% Generators  -----------------------------------------------
%%--------------------------------------------------------------------
tls_version() ->
    oneof([?'TLS_v1.3', ?'TLS_v1.2', ?'TLS_v1.1', ?'TLS_v1']).

pre_tls_1_3_version() ->
    oneof([?'TLS_v1.2', ?'TLS_v1.1', ?'TLS_v1']).

cipher_suite(Version) ->
    oneof(cipher_suites(Version)).

cipher_suites(Version) ->
    ssl:cipher_suites(default, Version).

anon_cipher_suite(Version) ->
    oneof(ssl:cipher_suites(anonymous, Version)).
 
openssl_legacy_names() ->
    %% Only include names that we support
    [
     %% Legacy with RSA keyexchange 
     "AES128-SHA",      
     "AES256-SHA", 	
     "AES128-SHA256", 	
     "AES256-SHA256",
     "AES256-GCM-SHA256", 
     "AES256-GCM-SHA384", 
     "DES-CBC-SHA",
     "DES-CBC3-SHA",     
     "RC4-MD5",
     "RC4-SHA",

     %% DH based
     "DH-RSA-AES128-SHA",
     "DH-RSA-AES256-SHA", 
     "DH-RSA-AES128-SHA256", 	
     "DH-RSA-AES256-SHA256",     
     "DH-DSS-AES128-SHA",
     "DH-DSS-AES256-SHA", 
     "DH-DSS-AES128-SHA256", 
     "DH-DSS-AES256-SHA256", 	
     "EDH-RSA-DES-CBC-SHA", 	
     "EDH-RSA-DES-CBC3-SHA",
     "DHE-RSA-AES128-SHA",   
     "DHE-RSA-AES256-SHA",
     "DHE-RSA-AES128-SHA256",
     "DHE-RSA-AES256-SHA256", 
     "DHE-RSA-AES256-SHA384",      
     "DHE-RSA-AES128-GCM-SHA256",
     "DHE-RSA-AES256-GCM-SHA384",
     "DHE-RSA-AES128-CCM-SHA256",
     "DHE-RSA-AES256-CCM-SHA384",
     "DHE-RSA-AES128-CCM8-SHA256",
     "DHE-RSA-AES256-CCM8-SHA384",
     "DHE-RSA-CHACHA20-POLY1305",
     "EDH-DSS-DES-CBC-SHA",
     "EDH-DSS-DES-CBC3-SHA",     
     "DHE-DSS-AES128-SHA",
     "DHE-DSS-AES256-SHA",
     "DHE-DSS-AES128-SHA256",  
     "DHE-DSS-AES256-SHA256", 
     "DHE-DSS-AES256-SHA384",      
     "DHE-DSS-AES128-GCM-SHA256",
     "DHE-DSS-AES256-GCM-SHA384",
     "DHE-DSS-RC4-SHA",
     "ADH-AES128-SHA256", 
     "ADH-AES256-SHA256", 
     "ADH-AES128-CBC-SHA256",
     "ADH-AES128-GCM-SHA256",
     "ADH-AES256-GCM-SHA384", 
     "ADH-RC4-MD5",
     "ADH-DES-CBC-SHA",
     "ADH-DES-CBC3-SHA",
     "ADH-AES256-SHA", 
     "ADH-AES256-SHA256",
     	
     %% ECDH based
     "ECDH-ECDSA-AES128-SHA",
     "ECDH-ECDSA-AES256-SHA",
     "ECDH-ECDSA-AES128-SHA256",    
     "ECDH-ECDSA-AES256-SHA384",
     "ECDH-ECDSA-AES128-GCM-SHA256",
     "ECDH-ECDSA-AES256-GCM-SHA384",
     "ECDHE-ECDSA-AES128-CCM",
     "ECDHE-ECDSA-AES128-CCM8",
     "ECDHE-ECDSA-AES256-CCM",
     "ECDHE-ECDSA-AES256-CCM8",
     "ECDH-ECDSA-CHACHA20-POLY1305",
     "ECDHE-ECDSA-AES128-SHA", 
     "ECDHE-ECDSA-AES256-SHA",
     "ECDHE-ECDSA-AES128-SHA256",    
     "ECDHE-ECDSA-AES256-SHA384",
     "ECDHE-ECDSA-AES128-GCM-SHA256",
     "ECDHE-ECDSA-AES256-GCM-SHA384",
     "ECDHE-ECDSA-CHACHA20-POLY1305",
     "ECDH-RSA-AES128-SHA",
     "ECDH-RSA-AES256-SHA",
     "ECDH-RSA-AES128-SHA256",               
     "ECDH-RSA-AES256-SHA384",       
     "ECDH-RSA-AES128-GCM-SHA256",
     "ECDH-RSA-AES256-GCM-SHA384",     
     "ECDHE-RSA-AES128-SHA",               
     "ECDHE-RSA-AES256-SHA",               
     "ECDHE-RSA-AES128-SHA256",               
     "ECDHE-RSA-AES256-SHA384",       
     "ECDHE-RSA-AES128-GCM-SHA256",
     "ECDHE-RSA-AES128-GCM-SHA384",
     "ECDHE-RSA-AES256-GCM-SHA256",
     "ECDHE-RSA-AES256-GCM-SHA384",
     "ECDHE-RSA-CHACHA20-POLY1305",
     "ECDHE-PSK-RC4-SHA",
     "ECDHE-PSK-3DES-EDE-CBC-SHA",
     "ECDHE-PSK-AES128-CBC-SHA",
     "ECDHE-PSK-AES128-CBC-SHA256",
     "ECDHE-PSK-AES256-CBC-SHA384",
     "ECDHE-PSK-AES128-GCM-SHA256",
     "ECDHE-PSK-AES256-GCM-SHA384",
     "ECDHE-PSK-AES128-CCM-SHA256",
     "ECDHE-PSK-AES128-CCM8-SHA256",
     "ECDHE-PSK-CHACHA20-POLY1305",
     "AECDH-DES-CBC3-SHA",
     "AECDH-AES128-SHA",
     "AECDH-AES256-SHA",

     %% PSK based
     "DHE-PSK-NULL-SHA",
     "DHE-PSK-RC4-SHA",
     "DHE-PSK-3DES-EDE-CBC-SHA",
     "DHE-PSK-AES128-CBC-SHA",
     "DHE-PSK-AES256-CBC-SHA",
     "DHE-PSK-AES128-CBC-SHA256", 
     "DHE-PSK-AES256-CBC-SHA384",
     "DHE-PSK-AES128-GCM-SHA256",
     "DHE-PSK-AES256-GCM-SHA384",
     "DHE-PSK-AES128-CCM",
     "DHE-PSK-AES128-CCM8",
     "DHE-PSK-AES256-CCM",
     "DHE-PSK-AES256-CCM8",
     "DHE-PSK-AES128-CCM-SHA256",
     "DHE-PSK-AES128-CCM8-SHA256",
     "DHE-PSK-CHACHA20-POLY1305",
     "PSK-NULL-SHA",    
     "PSK-RC4-SHA", 
     "PSK-3DES-EDE-CBC-SHA", 
     "PSK-AES128-CBC-SHA", 
     "PSK-AES256-CBC-SHA",
     "PSK-AES128-CBC-SHA256",
     "PSK-AES256-CBC-SHA256",
     "PSK-AES128-CCM", 
     "PSK-AES128-CCM8", 
     "PSK-AES256-CCM", 
     "PSK-AES256-CCM8",
     "PSK-AES128-GCM-SHA256",
     "PSK-AES256-CBC-SHA384",
     "PSK-AES256-GCM-SHA384",
     "PSK-CHACHA20-POLY1305",
     "RSA-PSK-NULL-SHA",
     "RSA-PSK-CHACHA20-POLY1305",
     
     %% SRP based
     "SRP-3DES-EDE-CBC-SHA", 
     "SRP-RSA-3DES-EDE-CBC-SHA",
     "SRP-DSS-3DES-EDE-CBC-SHA", 
     "SRP-AES-128-CBC-SHA",
     "SRP-AES-256-CBC-SHA"
    ]. 


sig_alg() ->
    oneof([#'SignatureAlgorithm'{algorithm = ?'id-RSASSA-PSS',
                                 parameters =  #'RSASSA-PSS-params'{
                                                  maskGenAlgorithm =
                                                      #'MaskGenAlgorithm'{algorithm = ?'id-mgf1',
                                                                          parameters =  #'HashAlgorithm'{algorithm = ?'id-sha256'}}}},
           #'SignatureAlgorithm'{algorithm = ?'id-RSASSA-PSS',
                                 parameters =  #'RSASSA-PSS-params'{
                                                  maskGenAlgorithm =
                                                      #'MaskGenAlgorithm'{algorithm = ?'id-mgf1',
                                                                          parameters = #'HashAlgorithm'{algorithm = ?'id-sha384'}}}},

           #'SignatureAlgorithm'{algorithm = ?'id-RSASSA-PSS',
                                 parameters =  #'RSASSA-PSS-params'{
                                                  maskGenAlgorithm =
                                                      #'MaskGenAlgorithm'{algorithm = ?'id-mgf1',
                                                                          parameters = #'HashAlgorithm'{algorithm = ?'id-sha512'}}}},
           #'SignatureAlgorithm'{algorithm = ?sha256WithRSAEncryption},
           #'SignatureAlgorithm'{algorithm = ?sha384WithRSAEncryption},
           #'SignatureAlgorithm'{algorithm = ?sha512WithRSAEncryption},
           #'SignatureAlgorithm'{algorithm = ?'ecdsa-with-SHA256'},
           #'SignatureAlgorithm'{algorithm = ?'ecdsa-with-SHA384'},
           #'SignatureAlgorithm'{algorithm = ?'ecdsa-with-SHA512'},
           #'SignatureAlgorithm'{algorithm = ?'sha-1WithRSAEncryption'},
           #'SignatureAlgorithm'{algorithm = ?'ecdsa-with-SHA1'},
           #'SignatureAlgorithm'{algorithm = ?'id-Ed25519'},
           #'SignatureAlgorithm'{algorithm = ?'id-Ed448'},
           #'SignatureAlgorithm'{algorithm = ?'rsaEncryption',
                                 parameters = 'NULL'},
           #'SignatureAlgorithm'{algorithm = ?'rsaEncryption'},
           #'SignatureAlgorithm'{algorithm = ?'id-RSASSA-PSS'}]).

sig_schemes() ->
    [rsa_pss_pss_sha256,
     rsa_pss_pss_sha384,
     rsa_pss_pss_sha512,
     rsa_pkcs1_sha256,
     rsa_pkcs1_sha384,
     rsa_pkcs1_sha512,
     ecdsa_secp256r1_sha256,
     ecdsa_secp384r1_sha384,
     ecdsa_secp521r1_sha512,
     rsa_pkcs1_sha1,
     rsa_pkcs1_sha1,
     ecdsa_sha1,
     eddsa_ed25519,
     eddsa_ed448,
     rsa_pkcs1_sha1,
     rsa_pss_rsae,
     rsa_pss_pss].
