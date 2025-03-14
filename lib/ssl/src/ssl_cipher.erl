%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2025. All Rights Reserved.
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
%% Purpose: Help functions for handling the SSL ciphers
%% 
%%----------------------------------------------------------------------

-module(ssl_cipher).
-moduledoc false.

-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("tls_handshake_1_3.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([security_parameters/2, 
         security_parameters/3, 
         security_parameters_1_3/2,
	 cipher_init/3, 
         nonce_seed/2, 
         decipher/6, 
         cipher/5, 
         aead_encrypt/6, 
         aead_decrypt/6,
         aead_type/2,
	 suites/1, 
         all_suites/1,
         crypto_support_filters/0,
	 anonymous_suites/1,
         filter/3,
         filter_suites/1,
         filter_suites/2,
	 hash_algorithm/1, 
         sign_algorithm/1, 
         is_acceptable_hash/2, 
         is_fallback/1,
	 random_bytes/1, 
         calc_mac_hash/4, 
         calc_mac_hash/6,
         is_stream_ciphersuite/1, 
         is_supported_sign/2,
         signature_scheme/1,
         signature_schemes_1_2/1,
         scheme_to_components/1, 
         hash_size/1, 
         key_material/1, 
         signature_algorithm_to_scheme/1,
         bulk_cipher_algorithm/1]).

%% RFC 8446 TLS 1.3
-export([generate_client_shares/1,
         generate_server_share/1,
         add_zero_padding/2,
         encrypt_ticket/3,
         decrypt_ticket/3,
         encrypt_data/4,
         decrypt_data/4]).

-compile(inline).

-type cipher_enum()        :: integer().

-export_type([cipher_enum/0]).

%%--------------------------------------------------------------------
-spec security_parameters(ssl_cipher_format:cipher_suite(), #security_parameters{}) ->
				 #security_parameters{}.
%% Only security_parameters/2 should call security_parameters/3 with undefined as
%% first argument.
%%--------------------------------------------------------------------

security_parameters(?TLS_NULL_WITH_NULL_NULL = CipherSuite, SecParams) ->
    security_parameters(undefined, CipherSuite, SecParams).

%%--------------------------------------------------------------------
-spec security_parameters(ssl_record:ssl_version() | undefined, 
                          ssl_cipher_format:cipher_suite(), #security_parameters{}) ->
				 #security_parameters{}.
%%
%% Description: Returns a security parameters record where the
%% cipher values has been updated according to <CipherSuite> 
%%-------------------------------------------------------------------
security_parameters(Version, CipherSuite, SecParams) ->
    #{cipher := Cipher, mac := Hash, 
      prf := PrfHashAlg} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    SecParams#security_parameters{
      cipher_suite = CipherSuite,
      bulk_cipher_algorithm = bulk_cipher_algorithm(Cipher),
      cipher_type = type(Cipher),
      key_material_length = key_material(Cipher),
      iv_size = iv_size(Cipher),
      mac_algorithm = mac_algorithm(Hash),
      prf_algorithm = prf_algorithm(PrfHashAlg, Version),
      hash_size = hash_size(Hash)}.

security_parameters_1_3(SecParams, CipherSuite) ->
     #{cipher := Cipher, prf := PrfHashAlg} =
        ssl_cipher_format:suite_bin_to_map(CipherSuite),
    SecParams#security_parameters{
      cipher_suite = CipherSuite,
      bulk_cipher_algorithm = bulk_cipher_algorithm(Cipher),
      prf_algorithm = PrfHashAlg,  %% HKDF hash algorithm
      cipher_type = ?AEAD}.

%%--------------------------------------------------------------------
-spec cipher_init(cipher_enum(), binary(), binary()) -> #cipher_state{}.
%%
%% Description: Initializes the #cipher_state according to BCA
%%-------------------------------------------------------------------
cipher_init(?RC4, IV, Key) ->
    State = {stream_init,rc4,Key,IV},
    #cipher_state{iv = IV, key = Key, state = State};
cipher_init(Type, IV, Key) when Type == ?AES_GCM;
                                Type == ?AES_CCM ->
    <<Nonce:64>> = random_bytes(8),
    #cipher_state{iv = IV, key = Key, nonce = Nonce, tag_len = 16};
cipher_init(?AES_CCM_8, IV, Key) ->
    <<Nonce:64>> = random_bytes(8),
    #cipher_state{iv = IV, key = Key, nonce = Nonce, tag_len = 8};
cipher_init(?CHACHA20_POLY1305, IV, Key) ->
    #cipher_state{iv = IV, key = Key, tag_len = 16};
cipher_init(_BCA, IV, Key) ->
    %% Initialize random IV cache, not used for aead ciphers
    #cipher_state{iv = IV, key = Key, state = <<>>}.

nonce_seed(Seed, CipherState) ->
    CipherState#cipher_state{nonce = Seed}.

%%--------------------------------------------------------------------
-spec cipher(cipher_enum(), #cipher_state{}, binary(), iodata(), ssl_record:ssl_version()) ->
		    {binary(), #cipher_state{}}. 
%%
%% Description: Encrypts the data and the MAC using cipher described
%% by cipher_enum() and updating the cipher state
%% Used for "MAC then Cipher" suites where first an HMAC of the
%% data is calculated and the data plus the HMAC is encrypted.
%%-------------------------------------------------------------------
cipher(?NULL, CipherState, <<>>, Fragment, _Version) ->
    {iolist_to_binary(Fragment), CipherState};
cipher(CipherEnum, CipherState = #cipher_state{state = {stream_init,rc4,Key,_IV}}, Mac, Fragment, Version) ->
    State = crypto:crypto_init(rc4, Key, true),
    cipher(CipherEnum,  CipherState#cipher_state{state = State}, Mac, Fragment, Version);
cipher(?RC4, CipherState = #cipher_state{state = State}, Mac, Fragment, _Version) ->
    GenStreamCipherList = [Fragment, Mac],
    T = crypto:crypto_update(State, GenStreamCipherList),
    {iolist_to_binary(T), CipherState};
cipher(?DES, CipherState, Mac, Fragment, Version) ->
    block_cipher(fun(Key, IV, T) ->
			 crypto:crypto_one_time(des_cbc, Key, IV, T, true)
		 end, block_size(des_cbc), CipherState, Mac, Fragment, Version);
cipher(?'3DES', CipherState, Mac, Fragment, Version) ->
    block_cipher(fun(Key, IV, T) ->
			 crypto:crypto_one_time(des_ede3_cbc, Key, IV, T, true)
		 end, block_size(des_ede3_cbc), CipherState, Mac, Fragment, Version);
cipher(?AES_CBC, CipherState, Mac, Fragment, Version) ->
    block_cipher(fun(Key, IV, T) when byte_size(Key) =:= 16 ->
			 crypto:crypto_one_time(aes_128_cbc, Key, IV, T, true);
		    (Key, IV, T) when byte_size(Key) =:= 32 ->
			 crypto:crypto_one_time(aes_256_cbc, Key, IV, T, true)
		 end, block_size(aes_128_cbc), CipherState, Mac, Fragment, Version).

aead_encrypt(Type, Key, Nonce, Fragment, AdditionalData, TagLen) ->
    crypto:crypto_one_time_aead(aead_type(Type,byte_size(Key)), Key, Nonce, Fragment, AdditionalData, TagLen, true).

aead_decrypt(Type, Key, Nonce, CipherText, CipherTag, AdditionalData) ->
    crypto:crypto_one_time_aead(aead_type(Type,byte_size(Key)), Key, Nonce, CipherText, AdditionalData, CipherTag, false).

aead_type(?AES_GCM, 16) ->
    aes_128_gcm;
aead_type(?AES_GCM, 24) ->
    aes_192_gcm;
aead_type(?AES_GCM, 32) ->
    aes_256_gcm;
aead_type(?AES_CCM, 16) ->
    aes_128_ccm;
aead_type(?AES_CCM, 24) ->
    aes_192_ccm;
aead_type(?AES_CCM, 32) ->
    aes_256_ccm;
aead_type(?AES_CCM_8, 16) ->
    aes_128_ccm;
aead_type(?AES_CCM_8, 24) ->
    aes_192_ccm;
aead_type(?AES_CCM_8, 32) ->
    aes_256_ccm;
aead_type(?CHACHA20_POLY1305, _) ->
    chacha20_poly1305.

build_cipher_block(BlockSz, Mac, Fragment) ->
    TotSz = byte_size(Mac) + erlang:iolist_size(Fragment) + 1,
    [Fragment, Mac, padding_with_len(TotSz, BlockSz)].

block_cipher(Fun, BlockSz, #cipher_state{key=Key, iv=IV} = CS0,
	     Mac, Fragment, ?TLS_1_0) ->
    L = build_cipher_block(BlockSz, Mac, Fragment),
    T = Fun(Key, IV, L),
    NextIV = next_iv(T, IV),
    {T, CS0#cipher_state{iv=NextIV}};

block_cipher(Fun, BlockSz, #cipher_state{key=Key, iv=IV, state = IV_Cache0} = CS0,
	     Mac, Fragment, Version)
  when ?TLS_GT(Version, ?TLS_1_0)->
    IV_Size = byte_size(IV),
    <<NextIV:IV_Size/binary, IV_Cache/binary>> =
        case IV_Cache0 of
            <<>> ->
                random_bytes(IV_Size bsl 5); % 32 IVs
            _ ->
                IV_Cache0
        end,
    L0 = build_cipher_block(BlockSz, Mac, Fragment),
    L = [NextIV|L0],
    T = Fun(Key, IV, L),
    {T, CS0#cipher_state{iv=NextIV, state = IV_Cache}}.

%%--------------------------------------------------------------------
-spec decipher(cipher_enum(), integer(), #cipher_state{}, binary(), 
	       ssl_record:ssl_version(), boolean()) ->
		      {binary(), binary(), #cipher_state{}} | #alert{}.
%%
%% Description: Decrypts the data and the MAC using cipher described
%% by cipher_enum() and updating the cipher state.
%% Used for "MAC then Cipher" suites where first the data is decrypted
%% and the an HMAC of the decrypted data is checked
%%-------------------------------------------------------------------
decipher(?NULL, _HashSz, CipherState, Fragment, _, _) ->
    {Fragment, <<>>, CipherState};
decipher(CipherEnum, HashSz, CipherState = #cipher_state{state = {stream_init,rc4,Key,_IV}},
         Fragment, Version, PaddingCheck) ->
    State = crypto:crypto_init(rc4, Key, false),
    decipher(CipherEnum, HashSz, CipherState#cipher_state{state = State}, Fragment, Version, PaddingCheck);
decipher(?RC4, HashSz, CipherState = #cipher_state{state = State}, Fragment, _, _) ->
    try crypto:crypto_update(State, Fragment) of
	Text ->
	    GSC = generic_stream_cipher_from_bin(Text, HashSz),
	    #generic_stream_cipher{content = Content, mac = Mac} = GSC,
	    {Content, Mac, CipherState}
    catch
	_:Reason:ST ->
	    %% This is a DECRYPTION_FAILED but
	    %% "differentiating between bad_record_mac and decryption_failed
	    %% alerts may permit certain attacks against CBC mode as used in
	    %% TLS [CBCATT].  It is preferable to uniformly use the
	    %% bad_record_mac alert to hide the specific type of the error."
            ?SSL_LOG(debug, decrypt_error, [{reason,Reason}, {stacktrace, ST}]),
            ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC, decryption_failed)
    end;

decipher(?DES, HashSz, CipherState, Fragment, Version, PaddingCheck) ->
    block_decipher(fun(Key, IV, T) ->
                           crypto:crypto_one_time(des_cbc, Key, IV, T, false)
		   end, CipherState, HashSz, Fragment, Version, PaddingCheck);
decipher(?'3DES', HashSz, CipherState, Fragment, Version, PaddingCheck) ->
    block_decipher(fun(Key, IV, T) ->
                           crypto:crypto_one_time(des_ede3_cbc, Key, IV, T, false)
		   end, CipherState, HashSz, Fragment, Version, PaddingCheck);
decipher(?AES_CBC, HashSz, CipherState, Fragment, Version, PaddingCheck) ->
    block_decipher(fun(Key, IV, T) when byte_size(Key) =:= 16 ->
                           crypto:crypto_one_time(aes_128_cbc, Key, IV, T, false);
		      (Key, IV, T) when byte_size(Key) =:= 32 ->
                           crypto:crypto_one_time(aes_256_cbc, Key, IV, T, false)
		   end, CipherState, HashSz, Fragment, Version, PaddingCheck).

block_decipher(Fun, #cipher_state{key=Key, iv=IV} = CipherState0, 
	       HashSz, Fragment, Version, PaddingCheck) ->
    try 
	Text = Fun(Key, IV, Fragment),
	NextIV = next_iv(Fragment, IV),
	GBC = generic_block_cipher_from_bin(Version, Text, NextIV, HashSz),
	Content = GBC#generic_block_cipher.content,
	Mac = GBC#generic_block_cipher.mac,
	CipherState1 = CipherState0#cipher_state{iv=GBC#generic_block_cipher.next_iv},
	case is_correct_padding(GBC, Version, PaddingCheck) of
	    true ->
		{Content, Mac, CipherState1};
	    false ->
		%% decryption failed or invalid padding,
		%% intentionally break Content to make
		%% sure a packet with invalid padding
		%% but otherwise correct data will fail
		%% the MAC test later
		{<<16#F0, Content/binary>>, Mac, CipherState1}
	end
    catch
	_:Reason:ST ->
	    %% This is a DECRYPTION_FAILED but
	    %% "differentiating between bad_record_mac and decryption_failed
	    %% alerts may permit certain attacks against CBC mode as used in
	    %% TLS [CBCATT].  It is preferable to uniformly use the
	    %% bad_record_mac alert to hide the specific type of the error."
            ?SSL_LOG(debug, decrypt_error, [{reason,Reason}, {stacktrace, ST}]),
            ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC, decryption_failed)
    end.

%%--------------------------------------------------------------------
-spec suites(ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of supported cipher suites.
%%--------------------------------------------------------------------
suites(Version) when ?TLS_1_X(Version) ->
    tls_v1:suites(Version);
suites(Version) when ?DTLS_1_X(Version) ->
    dtls_v1:suites(Version).

all_suites(?TLS_1_3 = Version) ->
    suites(Version) ++ tls_legacy_suites(?TLS_1_2)  ++ tls_v1:exclusive_suites(?TLS_1_0);
all_suites(?TLS_1_2 = Version) ->
    suites(Version) ++ tls_legacy_suites(Version) ++ tls_v1:exclusive_suites(?TLS_1_0);
all_suites(?TLS_1_1 = Version) ->
    suites(Version) ++ tls_legacy_suites(Version) ++ tls_v1:cbc_suites(Version);
all_suites(?TLS_1_0 = Version) ->
    suites(Version) ++ tls_legacy_suites(Version) ++ tls_v1:cbc_suites(Version);
all_suites(Version) ->
    dtls_v1:all_suites(Version).

tls_legacy_suites(Version) ->
    LegacySuites = [fun tls_v1:cbc_suites/1,
                    fun tls_v1:psk_suites/1,
                    fun tls_v1:srp_suites/1,
                    fun tls_v1:rsa_suites/1,
                    fun tls_v1:des_suites/1,
                    fun tls_v1:rc4_suites/1],
    lists:flatmap(fun (Fun) -> Fun(Version) end, LegacySuites).

%%--------------------------------------------------------------------
-spec anonymous_suites(ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the anonymous cipher suites, only supported
%% if explicitly set by user. Intended only for testing.
%%--------------------------------------------------------------------

anonymous_suites(Version) when ?TLS_1_X(Version) ->
    Versions = versions_included(Version),
    lists:flatmap(fun tls_v1:exclusive_anonymous_suites/1, Versions);
anonymous_suites(Version) when ?DTLS_1_X(Version) ->
    dtls_v1:anonymous_suites(Version).

versions_included(?TLS_1_0) -> [?TLS_1_0];
versions_included(?TLS_1_1) -> [?TLS_1_1, ?TLS_1_0];
versions_included(?TLS_1_2) -> [?TLS_1_2, ?TLS_1_1, ?TLS_1_0];
versions_included(?TLS_1_3) -> [?TLS_1_3].

%%--------------------------------------------------------------------
-spec filter(undefined | binary(), [ssl_cipher_format:cipher_suite()],
             ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Select the cipher suites that can be used together with the 
%% supplied certificate. (Server side functionality)  
%%-------------------------------------------------------------------
filter(undefined, Ciphers, _) -> 
    Ciphers;
filter(DerCert, Ciphers0, Version) ->
    OtpCert = public_key:pkix_decode_cert(DerCert, otp),
    SigAlg = OtpCert#'OTPCertificate'.signatureAlgorithm,
    PubKeyInfo = OtpCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo,
    PubKeyAlg = PubKeyInfo#'OTPSubjectPublicKeyInfo'.algorithm,
    Type =  case ssl_certificate:public_key_type(PubKeyAlg#'PublicKeyAlgorithm'.algorithm) of
                rsa_pss_pss ->
                    rsa;
                Other ->
                    Other
            end,
    Ciphers = filter_suites_pubkey(Type, Ciphers0, Version, OtpCert),
    SigAlgo = SigAlg#'SignatureAlgorithm'.algorithm,
    Sign = ssl_certificate:public_key_type(SigAlgo),
    filter_suites_signature(Sign, Ciphers, Version).

%%--------------------------------------------------------------------
-spec filter_suites([ssl:erl_cipher_suite()] | [ssl_cipher_format:cipher_suite()], map()) ->
                           [ssl:erl_cipher_suite()] |  [ssl_cipher_format:cipher_suite()].
%%
%% Description: Filter suites using supplied filter funs
%%-------------------------------------------------------------------	
filter_suites(Suites, Filters) ->
    Fn = fun (Suite) when is_map_key(key_exchange, Suite) -> Suite;
             (Suite) -> ssl_cipher_format:suite_bin_to_map(Suite)
         end,
    lists:filter(fun(Suite) -> filter_suite(Fn(Suite), Filters) end, Suites).

filter_suite(#{key_exchange := KeyExchange, 
               cipher := Cipher, 
               mac := Hash,
               prf := Prf}, 
             #{key_exchange_filters := KeyFilters,
               cipher_filters := CipherFilters, 
               mac_filters := HashFilters,
               prf_filters := PrfFilters}) ->
    KeyPairs = [{KeyExchange, KeyFilters}, {Cipher, CipherFilters},
                {Hash, HashFilters}, {Prf, PrfFilters}],
    lists:all(fun all_filters/1, KeyPairs).


%%--------------------------------------------------------------------
-spec filter_suites([ssl:erl_cipher_suite()] | [ssl_cipher_format:cipher_suite()]) -> 
                           [ssl:erl_cipher_suite()] | [ssl_cipher_format:cipher_suite()].
%%
%% Description: Filter suites for algorithms supported by crypto.
%%-------------------------------------------------------------------
filter_suites(Suites) ->
    Filters = crypto_support_filters(),
    filter_suites(Suites, Filters).

all_filters({Value, Filters}) ->
    lists:all(fun (FilterFn) -> FilterFn(Value) end, Filters).

crypto_support_filters() ->
    Algos = crypto:supports(),
    Hashs =  proplists:get_value(hashs, Algos),
    #{key_exchange_filters => 
          [fun(KeyExchange) ->
                  is_acceptable_keyexchange(KeyExchange,  
                                            proplists:get_value(public_keys, Algos))
           end],
      cipher_filters => 
          [fun(Cipher) ->
                  is_acceptable_cipher(Cipher,  
                                       proplists:get_value(ciphers, Algos))
           end],
      mac_filters => 
          [fun(Hash) ->
                  is_acceptable_hash(Hash, Hashs)
          end],
      prf_filters => 
          [fun(Prf) ->
                  is_acceptable_prf(Prf,  
                                    proplists:get_value(hashs, Algos))
          end]}.

is_acceptable_keyexchange(KeyExchange, _Algos) when KeyExchange == psk;
                                                    KeyExchange == null;
                                                    KeyExchange == any ->
    true;
is_acceptable_keyexchange(KeyExchange, Algos) when KeyExchange == dh_anon;
                                                   KeyExchange == dhe_psk ->
    proplists:get_bool(dh, Algos);
is_acceptable_keyexchange(dhe_dss, Algos) ->
    proplists:get_bool(dh, Algos) andalso
        proplists:get_bool(dss, Algos);
is_acceptable_keyexchange(dhe_rsa, Algos) ->
    proplists:get_bool(dh, Algos) andalso
        proplists:get_bool(rsa, Algos);
is_acceptable_keyexchange(KeyExchange, Algos) when KeyExchange == ecdh_anon;
                                                   KeyExchange == ecdhe_psk ->        
    proplists:get_bool(ecdh, Algos);
is_acceptable_keyexchange(KeyExchange, Algos) when KeyExchange == ecdh_ecdsa;
                                                   KeyExchange == ecdhe_ecdsa ->
    proplists:get_bool(ecdh, Algos) andalso
        proplists:get_bool(ecdsa, Algos);
is_acceptable_keyexchange(KeyExchange, Algos) when KeyExchange == ecdh_rsa;
                                                   KeyExchange == ecdhe_rsa ->
    proplists:get_bool(ecdh, Algos) andalso
        proplists:get_bool(rsa, Algos);
is_acceptable_keyexchange(KeyExchange, Algos) when KeyExchange == rsa;
                                                   KeyExchange == rsa_psk ->
    proplists:get_bool(rsa, Algos);
is_acceptable_keyexchange(srp_anon, Algos) ->
    proplists:get_bool(srp, Algos);
is_acceptable_keyexchange(srp_dss, Algos) ->
    proplists:get_bool(srp, Algos) andalso
        proplists:get_bool(dss, Algos);
is_acceptable_keyexchange(srp_rsa, Algos) ->
    proplists:get_bool(srp, Algos) andalso
        proplists:get_bool(rsa, Algos);
is_acceptable_keyexchange(_KeyExchange, _Algos) ->
    false.

is_acceptable_cipher(null, _Algos) ->
    true;
is_acceptable_cipher(rc4_128, Algos) ->
    proplists:get_bool(rc4, Algos);
is_acceptable_cipher('3des_ede_cbc', Algos) ->
    proplists:get_bool(des_ede3_cbc, Algos);
is_acceptable_cipher(aes_128_ccm_8, Algos) ->
    proplists:get_bool(aes_128_ccm, Algos);
is_acceptable_cipher(aes_256_ccm_8, Algos) ->
    proplists:get_bool(aes_256_ccm, Algos);
is_acceptable_cipher(Cipher, Algos) ->
    proplists:get_bool(Cipher, Algos).

is_acceptable_hash(null, _Algos) ->
    true;
is_acceptable_hash(aead, _Algos) ->
    true;
is_acceptable_hash(Hash, Algos) ->
    proplists:get_bool(Hash, Algos).

is_acceptable_prf(default_prf, _) ->
    true;
is_acceptable_prf(Prf, Algos) ->
    proplists:get_bool(Prf, Algos).

is_fallback(CipherSuites)->
    lists:member(?TLS_FALLBACK_SCSV, CipherSuites).


%%--------------------------------------------------------------------
-spec random_bytes(integer()) -> binary().

%%
%% Description: Generates cryptographically secure random sequence 
%%--------------------------------------------------------------------
random_bytes(N) ->
    crypto:strong_rand_bytes(N).

calc_mac_hash(Type, Version,
	      PlainFragment, #{sequence_number := SeqNo,
			       mac_secret := MacSecret,
			       security_parameters :=
				   #security_parameters{mac_algorithm = MacAlgorithm}}) ->
    calc_mac_hash(Type, Version, PlainFragment, MacAlgorithm, MacSecret, SeqNo).
%%
calc_mac_hash(Type, Version, PlainFragment, MacAlgorithm, MacSecret, SeqNo) ->
    Length = erlang:iolist_size(PlainFragment),
    mac_hash(Version, MacAlgorithm, MacSecret, SeqNo, Type, Length, PlainFragment).

is_stream_ciphersuite(#{cipher := rc4_128}) ->
    true;
is_stream_ciphersuite(_) ->
    false.

-spec  hash_size(atom()) -> integer().
hash_size(null) ->
    0;
%% The AEAD MAC hash size is not used in the context 
%% of calculating the master secret. See RFC 5246 Section 6.2.3.3.
hash_size(aead) ->
    0;
hash_size(md5) ->
    16;
hash_size(sha) ->
    20;
%% Uncomment when adding cipher suite that needs it
%hash_size(sha224) ->
%    28;
hash_size(sha256) ->
    32;
hash_size(sha384) ->
    48;
hash_size(sha512) ->
    64.

%% Handle RSA and RSA_PSS_RSAE
is_supported_sign({Hash, rsa} = SignAlgo, HashSigns) -> %% ?rsaEncryption cert signalgo used
    lists:member(SignAlgo, HashSigns) orelse
        lists:member({Hash, rsa_pss_rsae}, HashSigns);
is_supported_sign(rsa_pkcs1_sha256 = SignAlgo, HashSigns) -> %% TLS-1.3 legacy scheme
    lists:member(SignAlgo, HashSigns) orelse
        lists:member(rsa_pss_rsae_sha256, HashSigns);
is_supported_sign(rsa_pkcs1_sha384 = SignAlgo, HashSigns) -> %% TLS-1.3 legacy scheme
    lists:member(SignAlgo, HashSigns) orelse
        lists:member(rsa_pss_rsae_sha384, HashSigns);
is_supported_sign(rsa_pkcs1_sha512 = SignAlgo, HashSigns) -> %% TLS-1.3 legacy scheme
    lists:member(SignAlgo, HashSigns) orelse
        lists:member(rsa_pss_rsae_sha512, HashSigns);
is_supported_sign(SignAlgo, HashSigns) ->  %% Normal case, format (scheme or alg-pair) depends on version
    lists:member(SignAlgo, HashSigns).

signature_scheme(rsa_pkcs1_sha256) -> ?RSA_PKCS1_SHA256;
signature_scheme(rsa_pkcs1_sha384) -> ?RSA_PKCS1_SHA384;
signature_scheme(rsa_pkcs1_sha512) -> ?RSA_PKCS1_SHA512;
signature_scheme(ecdsa_secp256r1_sha256) -> ?ECDSA_SECP256R1_SHA256;
signature_scheme(ecdsa_secp384r1_sha384) -> ?ECDSA_SECP384R1_SHA384;
signature_scheme(ecdsa_secp521r1_sha512) -> ?ECDSA_SECP521R1_SHA512;
signature_scheme(ecdsa_brainpoolP256r1tls13_sha256) -> ?ECDSA_BRAINPOOLP256R1TLS13_SHA256;
signature_scheme(ecdsa_brainpoolP384r1tls13_sha384) -> ?ECDSA_BRAINPOOLP384R1TLS13_SHA384;
signature_scheme(ecdsa_brainpoolP512r1tls13_sha512) -> ?ECDSA_BRAINPOOLP512R1TLS13_SHA512;
signature_scheme(rsa_pss_rsae_sha256) -> ?RSA_PSS_RSAE_SHA256;
signature_scheme(rsa_pss_rsae_sha384) -> ?RSA_PSS_RSAE_SHA384;
signature_scheme(rsa_pss_rsae_sha512) -> ?RSA_PSS_RSAE_SHA512;
signature_scheme(eddsa_ed25519) -> ?ED25519;
signature_scheme(eddsa_ed448) -> ?ED448;
signature_scheme(rsa_pss_pss_sha256) -> ?RSA_PSS_PSS_SHA256;
signature_scheme(rsa_pss_pss_sha384) -> ?RSA_PSS_PSS_SHA384;
signature_scheme(rsa_pss_pss_sha512) -> ?RSA_PSS_PSS_SHA512;
signature_scheme(rsa_pkcs1_sha1) -> ?RSA_PKCS1_SHA1;
signature_scheme(ecdsa_sha1) -> ?ECDSA_SHA1;
%% New algorithms on legacy format
signature_scheme({sha512, rsa_pss_pss}) ->
    ?RSA_PSS_PSS_SHA512;
signature_scheme({sha384, rsa_pss_pss}) ->
    ?RSA_PSS_PSS_SHA384;
signature_scheme({sha256, rsa_pss_pss}) ->
    ?RSA_PSS_PSS_SHA256;
signature_scheme({sha512, rsa_pss_rsae}) ->
    ?RSA_PSS_RSAE_SHA512;
signature_scheme({sha384, rsa_pss_rsae}) ->
    ?RSA_PSS_RSAE_SHA384;
signature_scheme({sha256, rsa_pss_rsae}) ->
    ?RSA_PSS_RSAE_SHA256;
%% Handling legacy signature algorithms
signature_scheme({Hash0, Sign0}) ->
    Hash = hash_algorithm(Hash0),
    Sign = sign_algorithm(Sign0),
    <<?UINT16(SigAlg)>> = <<?BYTE(Hash),?BYTE(Sign)>>,
    SigAlg;
signature_scheme(?RSA_PKCS1_SHA256) -> rsa_pkcs1_sha256;
signature_scheme(?RSA_PKCS1_SHA384) -> rsa_pkcs1_sha384;
signature_scheme(?RSA_PKCS1_SHA512) -> rsa_pkcs1_sha512;
signature_scheme(?ECDSA_SECP256R1_SHA256) -> ecdsa_secp256r1_sha256;
signature_scheme(?ECDSA_SECP384R1_SHA384) -> ecdsa_secp384r1_sha384;
signature_scheme(?ECDSA_SECP521R1_SHA512) -> ecdsa_secp521r1_sha512;
signature_scheme(?ECDSA_BRAINPOOLP256R1TLS13_SHA256) -> ecdsa_brainpoolP256r1tls13_sha256;
signature_scheme(?ECDSA_BRAINPOOLP384R1TLS13_SHA384) -> ecdsa_brainpoolP384r1tls13_sha384;
signature_scheme(?ECDSA_BRAINPOOLP512R1TLS13_SHA512) -> ecdsa_brainpoolP512r1tls13_sha512;
signature_scheme(?RSA_PSS_RSAE_SHA256) -> rsa_pss_rsae_sha256;
signature_scheme(?RSA_PSS_RSAE_SHA384) -> rsa_pss_rsae_sha384;
signature_scheme(?RSA_PSS_RSAE_SHA512) -> rsa_pss_rsae_sha512;
signature_scheme(?ED25519) -> eddsa_ed25519;
signature_scheme(?ED448) -> eddsa_ed448;
signature_scheme(?RSA_PSS_PSS_SHA256) -> rsa_pss_pss_sha256;
signature_scheme(?RSA_PSS_PSS_SHA384) -> rsa_pss_pss_sha384;
signature_scheme(?RSA_PSS_PSS_SHA512) -> rsa_pss_pss_sha512;
signature_scheme(?RSA_PKCS1_SHA1) -> rsa_pkcs1_sha1;
signature_scheme(?ECDSA_SHA1) -> ecdsa_sha1;
%% Handling legacy signature algorithms for logging purposes. These algorithms
%% cannot be used in TLS 1.3 handshakes.
signature_scheme(SignAlgo) when is_integer(SignAlgo) ->
    <<?BYTE(Hash),?BYTE(Sign)>> = <<?UINT16(SignAlgo)>>,
    try
        case {hash_algorithm(Hash), sign_algorithm(Sign)} of
            {unassigned, _} -> unassigned;
            {_, unassigned} -> unassigned;
            Scheme -> Scheme
        end
    catch
        _:_ ->
            unassigned
    end;
signature_scheme(_) -> unassigned.

signature_schemes_1_2(SigAlgs) ->
    Schemes = lists:foldl(fun(Alg, Acc) when is_atom(Alg) ->
                                  case scheme_to_components(Alg) of
                                      {Hash, Sign = rsa_pss_pss,_} ->
                                          [{Hash, Sign} | Acc];
                                      {Hash, Sign = rsa_pss_rsae,_} ->
                                          [{Hash, Sign} | Acc];
                                      %% TLS-1.2 do not constrain the
                                      %% curve, however must be one
                                      %% present in "supported groups" (eccs)
                                      {Hash, ecdsa = Sign, _} ->
                                          [{Hash, Sign} | Acc];
                                      {Hash, Sign, undefined} ->
                                          [{Hash, format_sign(Sign)} | Acc];
                                      {_, _, _} ->
                                Acc
                                  end;
                             (Alg, Acc) ->
                                  [Alg| Acc]
                          end, [], SigAlgs),
    %% Make sure that if ECDSA TLS-1.2 names are specified do not duplicate them
    %% earlier in list by allowing TLS-1.3 schemes to be interpreted as TLS-1.2 algs
    %% unless the ECDSA TLS-1.2 representation is missing and we want to work around it.
    lists:reverse(lists:uniq(Schemes)).

%% TODO: reserved code points?

scheme_to_components(rsa_pkcs1_sha256) -> {sha256, rsa_pkcs1, undefined};
scheme_to_components(rsa_pkcs1_sha384) -> {sha384, rsa_pkcs1, undefined};
scheme_to_components(rsa_pkcs1_sha512) -> {sha512, rsa_pkcs1, undefined};
scheme_to_components(ecdsa_secp256r1_sha256) -> {sha256, ecdsa, secp256r1};
scheme_to_components(ecdsa_secp384r1_sha384) -> {sha384, ecdsa, secp384r1};
scheme_to_components(ecdsa_secp521r1_sha512) -> {sha512, ecdsa, secp521r1};
scheme_to_components(ecdsa_brainpoolP256r1tls13_sha256) -> {sha256, ecdsa, brainpoolP256r1};
scheme_to_components(ecdsa_brainpoolP384r1tls13_sha384) -> {sha384, ecdsa, brainpoolP384r1};
scheme_to_components(ecdsa_brainpoolP512r1tls13_sha512) -> {sha512, ecdsa, brainpoolP512r1};
scheme_to_components(rsa_pss_rsae_sha256) -> {sha256, rsa_pss_rsae, undefined};
scheme_to_components(rsa_pss_rsae_sha384) -> {sha384, rsa_pss_rsae, undefined};
scheme_to_components(rsa_pss_rsae_sha512) -> {sha512, rsa_pss_rsae, undefined};
scheme_to_components(eddsa_ed25519) -> {none, eddsa, ed25519};
scheme_to_components(eddsa_ed448) -> {none, eddsa, ed448};
scheme_to_components(rsa_pss_pss_sha256) -> {sha256, rsa_pss_pss, undefined};
scheme_to_components(rsa_pss_pss_sha384) -> {sha384, rsa_pss_pss, undefined};
scheme_to_components(rsa_pss_pss_sha512) -> {sha512, rsa_pss_pss, undefined};
scheme_to_components(rsa_pkcs1_sha1) -> {sha, rsa_pkcs1, undefined};
scheme_to_components(ecdsa_sha1) -> {sha, ecdsa, undefined};
%% Handling legacy signature algorithms
scheme_to_components({Hash,Sign}) -> {Hash, Sign, undefined}.

format_sign(rsa_pkcs1) ->
    rsa;
format_sign(Value) ->
    Value.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
mac_hash({_,_}, ?NULL, _MacSecret, _SeqNo, _Type,
	 _Length, _Fragment) ->
    <<>>;
mac_hash(Version, MacAlg, MacSecret, SeqNo, Type, Length, Fragment)
  when ?TLS_LTE(Version, ?TLS_1_2), Version =/= ?SSL_3_0 ->
    tls_v1:mac_hash(MacAlg, MacSecret, SeqNo, Type, Version,
		      Length, Fragment).

bulk_cipher_algorithm(null) ->
    ?NULL;
bulk_cipher_algorithm(rc4_128) ->
    ?RC4;
bulk_cipher_algorithm(des_cbc) ->
    ?DES;
bulk_cipher_algorithm('3des_ede_cbc') ->
    ?'3DES';
bulk_cipher_algorithm(Cipher) when Cipher == aes_128_cbc;
				   Cipher == aes_256_cbc ->
    ?AES_CBC;
bulk_cipher_algorithm(Cipher) when Cipher == aes_128_gcm;
				   Cipher == aes_256_gcm ->
    ?AES_GCM;
bulk_cipher_algorithm(Cipher) when Cipher == aes_128_ccm;
				   Cipher == aes_256_ccm ->
    ?AES_CCM;
bulk_cipher_algorithm(Cipher) when Cipher == aes_128_ccm_8;
				   Cipher == aes_256_ccm_8 ->
    ?AES_CCM_8;
bulk_cipher_algorithm(chacha20_poly1305) ->
    ?CHACHA20_POLY1305.

type(Cipher) when Cipher == null;
		  Cipher == rc4_128 ->
    ?STREAM;

type(Cipher) when Cipher == des_cbc;
		  Cipher == '3des_ede_cbc';
		  Cipher == aes_128_cbc;
		  Cipher == aes_256_cbc ->
    ?BLOCK;
type(Cipher) when Cipher == aes_128_gcm;
		  Cipher == aes_256_gcm;
                  Cipher == aes_128_ccm;
		  Cipher == aes_256_ccm;
                  Cipher == aes_128_ccm_8;
		  Cipher == aes_256_ccm_8;
		  Cipher == chacha20_poly1305 ->
    ?AEAD.

key_material(null) ->
    0;
key_material(rc4_128) ->
    16;
key_material(des_cbc) ->
    8;
key_material('3des_ede_cbc') ->
    24;
key_material(aes_128_cbc) ->
    16;
key_material(aes_256_cbc) ->
    32;
key_material(aes_128_gcm) ->
    16;
key_material(aes_128_ccm) ->
    16;
key_material(aes_128_ccm_8) ->
    16;
key_material(aes_256_gcm) ->
    32;
key_material(aes_256_ccm_8) ->
    32;
key_material(aes_256_ccm) ->
    32;
key_material(chacha20_poly1305) ->
    32.

iv_size(Cipher) when Cipher == null;
		     Cipher == rc4_128 ->
    0;
iv_size(Cipher) when Cipher == aes_128_gcm;
		     Cipher == aes_256_gcm;
                     Cipher == aes_128_ccm;
		     Cipher == aes_256_ccm;
                     Cipher == aes_128_ccm_8;
		     Cipher == aes_256_ccm_8 ->
    4;
iv_size(chacha20_poly1305) ->
    12;
iv_size(Cipher) ->
    block_size(Cipher).

block_size(Cipher) when Cipher == des_cbc;
			Cipher == des_ede3_cbc;
			Cipher == '3des_ede_cbc' -> 
    8;
block_size(Cipher) when Cipher == aes_128_cbc;
			Cipher == aes_256_cbc;
			Cipher == aes_128_gcm;
			Cipher == aes_256_gcm;
                        Cipher == aes_128_ccm;
			Cipher == aes_256_ccm;
                        Cipher == aes_128_ccm_8;
			Cipher == aes_256_ccm_8;
			Cipher == chacha20_poly1305 ->
    16.

prf_algorithm(default_prf, ?TLS_1_2) ->
    ?SHA256;
prf_algorithm(default_prf, Version) when ?TLS_1_X(Version) ->
    ?MD5SHA;
prf_algorithm(Algo, _) ->
    hash_algorithm(Algo).

mac_algorithm(aead) ->
    aead;
mac_algorithm(Algo) ->
    hash_algorithm(Algo).

hash_algorithm(null)   -> ?NULL;
hash_algorithm(md5)    -> ?MD5;
hash_algorithm(sha)   -> ?SHA; %% Only sha always refers to "SHA-1"
hash_algorithm(sha224) -> ?SHA224;
hash_algorithm(sha256) -> ?SHA256;
hash_algorithm(sha384) -> ?SHA384;
hash_algorithm(sha512) -> ?SHA512;
hash_algorithm(?NULL) -> null;
hash_algorithm(?MD5) -> md5;
hash_algorithm(?SHA) -> sha;
hash_algorithm(?SHA224) -> sha224;
hash_algorithm(?SHA256) -> sha256;
hash_algorithm(?SHA384) -> sha384;
hash_algorithm(?SHA512) -> sha512;
hash_algorithm(Other)  when is_integer(Other) andalso ((Other >= 7) and (Other =< 223)) -> unassigned;
hash_algorithm(Other)  when is_integer(Other) andalso ((Other >= 224) and (Other =< 255)) -> Other.

sign_algorithm(anon)  -> ?ANON;
sign_algorithm(rsa)   -> ?RSA;
sign_algorithm(dsa)   -> ?DSA;
sign_algorithm(ecdsa) -> ?ECDSA;
sign_algorithm(?ANON) -> anon;
sign_algorithm(?RSA) -> rsa;
sign_algorithm(?DSA) -> dsa;
sign_algorithm(?ECDSA) -> ecdsa;
sign_algorithm(Other) when is_integer(Other) andalso ((Other >= 4) and (Other =< 223)) -> unassigned;
sign_algorithm(Other) when is_integer(Other) andalso ((Other >= 224) and (Other =< 255)) -> Other.


signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?'id-RSASSA-PSS',
                                                    parameters =  #'RSASSA-PSS-params'{
                                                                     maskGenAlgorithm = 
                                                                         #'MaskGenAlgorithm'{algorithm = ?'id-mgf1',
                                                                                             parameters = HashAlgo}}}) ->
    #'HashAlgorithm'{algorithm = HashOid} = HashAlgo,
    case public_key:pkix_hash_type(HashOid) of
        sha256 ->
            rsa_pss_pss_sha256;
        sha384 ->
            rsa_pss_pss_sha384;
        sha512 ->
            rsa_pss_pss_sha512
    end;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?sha256WithRSAEncryption}) ->
    rsa_pkcs1_sha256;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?sha384WithRSAEncryption}) ->
    rsa_pkcs1_sha384;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?sha512WithRSAEncryption}) ->
    rsa_pkcs1_sha512;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?'ecdsa-with-SHA256'}) ->
    ecdsa_secp256r1_sha256;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?'ecdsa-with-SHA384'}) ->
    ecdsa_secp384r1_sha384;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?'ecdsa-with-SHA512'}) ->
    ecdsa_secp521r1_sha512;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?'sha-1WithRSAEncryption'}) ->
    rsa_pkcs1_sha1;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?sha1WithRSAEncryption}) ->
    rsa_pkcs1_sha1;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?'ecdsa-with-SHA1'}) ->
    ecdsa_sha1;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?'id-Ed25519'}) ->
    eddsa_ed25519;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?'id-Ed448'}) ->
    eddsa_ed448;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?'rsaEncryption',
                                                    parameters = ?NULL}) ->
    rsa_pkcs1_sha1;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?'rsaEncryption'}) ->
    rsa_pss_rsae;
signature_algorithm_to_scheme(#'SignatureAlgorithm'{algorithm = ?'id-RSASSA-PSS'}) ->
    rsa_pss_pss.

%% RFC 5246: 6.2.3.2.  CBC Block Cipher
%%
%%   Implementation note: Canvel et al. [CBCTIME] have demonstrated a
%%   timing attack on CBC padding based on the time required to compute
%%   the MAC.  In order to defend against this attack, implementations
%%   MUST ensure that record processing time is essentially the same
%%   whether or not the padding is correct.  In general, the best way to
%%   do this is to compute the MAC even if the padding is incorrect, and
%%   only then reject the packet.  For instance, if the pad appears to be
%%   incorrect, the implementation might assume a zero-length pad and then
%%   compute the MAC.  This leaves a small timing channel, since MAC
%%   performance depends to some extent on the size of the data fragment,
%%   but it is not believed to be large enough to be exploitable, due to
%%   the large block size of existing MACs and the small size of the
%%   timing signal.
%%
%% implementation note:
%%   We return the original (possibly invalid) PadLength in any case.
%%   An invalid PadLength will be caught by is_correct_padding/2
%%
generic_block_cipher_from_bin(?TLS_1_0, T, IV, HashSize)->
    Sz1 = byte_size(T) - 1,
    <<_:Sz1/binary, ?BYTE(PadLength0)>> = T,
    PadLength = if
		    PadLength0 >= Sz1 -> 0;
		    true -> PadLength0
		end,
    Length = byte_size(T) - PadLength - 1 - HashSize,
    <<Content:Length/binary, Mac:HashSize/binary,
      Padding:PadLength/binary, ?BYTE(PadLength0)>> = T,
    #generic_block_cipher{content=Content, mac=Mac,
			  padding=Padding, padding_length=PadLength0,
			  next_iv = IV};

generic_block_cipher_from_bin(Version, T, IV, HashSize)
  when Version == ?TLS_1_1; Version == ?TLS_1_2 ->
    Sz1 = byte_size(T) - 1,
    <<_:Sz1/binary, ?BYTE(PadLength)>> = T,
    IVLength = byte_size(IV),
    Length = byte_size(T) - IVLength - PadLength - 1 - HashSize,
    <<NextIV:IVLength/binary, Content:Length/binary, Mac:HashSize/binary,
      Padding:PadLength/binary, ?BYTE(PadLength)>> = T,
    #generic_block_cipher{content=Content, mac=Mac,
			  padding=Padding, padding_length=PadLength,
			  next_iv = NextIV}.

generic_stream_cipher_from_bin(T, HashSz) ->
    Sz = byte_size(T),
    Length = Sz - HashSz,
    <<Content:Length/binary, Mac:HashSz/binary>> = T,
    #generic_stream_cipher{content=Content,
			   mac=Mac}.

is_correct_padding(#generic_block_cipher{padding_length = Len,
					 padding = Padding}, ?SSL_3_0, _) ->
    Len == byte_size(Padding); %% Only length check is done in SSL 3.0 spec
%% For interoperability reasons it is possible to disable
%% the padding check when using TLS 1.0 (mimicking SSL-3.0), as it is not strictly required
%% in the spec (only recommended), however this makes TLS 1.0 vunrable to the Poodle attack 
%% so by default this clause will not match
is_correct_padding(GenBlockCipher, ?TLS_1_0, false) ->
    is_correct_padding(GenBlockCipher, ?SSL_3_0, false);
%% Padding must be checked in TLS 1.1 and after  
is_correct_padding(#generic_block_cipher{padding_length = Len,
					 padding = Padding}, _, _) ->
    (Len == byte_size(Padding)) andalso (padding(Len) == Padding).

padding(PadLen) ->
    case PadLen of
        0 -> <<>>;
        1 -> <<1>>;
        2 -> <<2,2>>;
        3 -> <<3,3,3>>;
        4 -> <<4,4,4,4>>;
        5 -> <<5,5,5,5,5>>;
        6 -> <<6,6,6,6,6,6>>;
        7 -> <<7,7,7,7,7,7,7>>;
        8 -> <<8,8,8,8,8,8,8,8>>;
        9 -> <<9,9,9,9,9,9,9,9,9>>;
        10 -> <<10,10,10,10,10,10,10,10,10,10>>;
        11 -> <<11,11,11,11,11,11,11,11,11,11,11>>;
        12 -> <<12,12,12,12,12,12,12,12,12,12,12,12>>;
        13 -> <<13,13,13,13,13,13,13,13,13,13,13,13,13>>;
        14 -> <<14,14,14,14,14,14,14,14,14,14,14,14,14,14>>;
        15 -> <<15,15,15,15,15,15,15,15,15,15,15,15,15,15,15>>;
        _ ->
            binary:copy(<<PadLen>>, PadLen)
    end.

padding_with_len(TextLen, BlockSize) ->
    case BlockSize - (TextLen rem BlockSize) of
        0 -> <<0>>;
        1 -> <<1,1>>;
        2 -> <<2,2,2>>;
        3 -> <<3,3,3,3>>;
        4 -> <<4,4,4,4,4>>;
        5 -> <<5,5,5,5,5,5>>;
        6 -> <<6,6,6,6,6,6,6>>;
        7 -> <<7,7,7,7,7,7,7,7>>;
        8 -> <<8,8,8,8,8,8,8,8,8>>;
        9 -> <<9,9,9,9,9,9,9,9,9,9>>;
        10 -> <<10,10,10,10,10,10,10,10,10,10,10>>;
        11 -> <<11,11,11,11,11,11,11,11,11,11,11,11>>;
        12 -> <<12,12,12,12,12,12,12,12,12,12,12,12,12>>;
        13 -> <<13,13,13,13,13,13,13,13,13,13,13,13,13,13>>;
        14 -> <<14,14,14,14,14,14,14,14,14,14,14,14,14,14,14>>;
        15 -> <<15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15>>;
        PadLen ->
            binary:copy(<<PadLen>>, PadLen + 1)
    end.

next_iv(Bin, IV) ->
    BinSz = byte_size(Bin),
    IVSz = byte_size(IV),
    FirstPart = BinSz - IVSz,
    <<_:FirstPart/binary, NextIV:IVSz/binary>> = Bin,
    NextIV.

filter_suites_pubkey(rsa, CiphersSuites0, _Version, OtpCert) ->
    KeyUses = key_uses(OtpCert),
    NotECDSAKeyed = (CiphersSuites0 -- ec_keyed_suites(CiphersSuites0)) 
        -- dss_keyed_suites(CiphersSuites0),
    CiphersSuites = filter_keyuse_suites(keyEncipherment, KeyUses,
                                         NotECDSAKeyed,
                                         rsa_suites_encipher(CiphersSuites0)),
    filter_keyuse_suites(digitalSignature, KeyUses, CiphersSuites,
                         rsa_ecdhe_dhe_suites(CiphersSuites));
filter_suites_pubkey(dsa, Ciphers, _, OtpCert) ->  
    KeyUses = key_uses(OtpCert),
    NotECRSAKeyed =  (Ciphers -- rsa_keyed_suites(Ciphers)) -- ec_keyed_suites(Ciphers),
    filter_keyuse_suites(digitalSignature, KeyUses, NotECRSAKeyed,
                         dss_dhe_suites(Ciphers));
filter_suites_pubkey(ecdsa, Ciphers, _, OtpCert) ->
    Uses = key_uses(OtpCert),
    NotRSADSAKeyed = (Ciphers -- rsa_keyed_suites(Ciphers)) -- dss_keyed_suites(Ciphers),
    CiphersSuites = filter_keyuse_suites(digitalSignature, Uses, NotRSADSAKeyed,
                                   ec_ecdhe_suites(Ciphers)),
    filter_keyuse_suites(keyAgreement, Uses, CiphersSuites, ec_ecdh_suites(Ciphers)).

filter_suites_signature(_, Ciphers, Version) when ?TLS_GTE(Version, ?TLS_1_2) ->
     Ciphers;
filter_suites_signature(rsa, Ciphers, Version) ->
    (Ciphers -- ecdsa_signed_suites(Ciphers, Version)) -- dsa_signed_suites(Ciphers);
filter_suites_signature(dsa, Ciphers, Version) ->
    (Ciphers -- ecdsa_signed_suites(Ciphers, Version)) -- rsa_signed_suites(Ciphers, Version);
filter_suites_signature(ecdsa, Ciphers, Version) ->
    (Ciphers -- rsa_signed_suites(Ciphers, Version)) -- dsa_signed_suites(Ciphers).


%% From RFC 5246 - Section  7.4.2.  Server Certificate
%% If the client provided a "signature_algorithms" extension, then all
%% certificates provided by the server MUST be signed by a
%% hash/signature algorithm pair that appears in that extension.  Note
%% that this implies that a certificate containing a key for one
%% signature algorithm MAY be signed using a different signature
%% algorithm (for instance, an RSA key signed with a DSA key).  This is
%% a departure from TLS 1.1, which required that the algorithms be the
%% same. 
%% Note that this also implies that the DH_DSS, DH_RSA,
%% ECDH_ECDSA, and ECDH_RSA key exchange algorithms do not restrict the
%% algorithm used to sign the certificate.  Fixed DH certificates MAY be
%% signed with any hash/signature algorithm pair appearing in the
%% extension.  The names DH_DSS, DH_RSA, ECDH_ECDSA, and ECDH_RSA are
%% historical.
%% Note: DH_DSS and DH_RSA is not supported
rsa_signed(?TLS_1_2) ->
    fun(rsa) -> true;
       (dhe_rsa) -> true;
       (ecdhe_rsa) -> true;
       (rsa_psk) -> true;
       (srp_rsa) -> true;
       (_) -> false
    end;
rsa_signed(_) ->
    fun(rsa) -> true;
       (dhe_rsa) -> true;
       (ecdhe_rsa) -> true;
       (ecdh_rsa) -> true;
       (rsa_psk) -> true;
       (srp_rsa) -> true;
       (_) -> false
    end.
%% Cert should be signed by RSA
rsa_signed_suites(Ciphers, Version) ->
    filter_kex(Ciphers, rsa_signed(Version)).

ecdsa_signed(Version) when ?TLS_GTE(Version, ?TLS_1_2) ->
    fun(ecdhe_ecdsa) -> true;
       (_) -> false
    end;
ecdsa_signed(_) ->
    fun(ecdhe_ecdsa) -> true;
       (ecdh_ecdsa) -> true;
       (_) -> false
    end. 

%% Cert should be signed by ECDSA
ecdsa_signed_suites(Ciphers, Version) ->
    filter_kex(Ciphers, ecdsa_signed(Version)).

rsa_keyed(dhe_rsa) -> 
    true;
rsa_keyed(ecdhe_rsa) -> 
    true;
rsa_keyed(rsa) -> 
    true;
rsa_keyed(rsa_psk) -> 
    true;
rsa_keyed(srp_rsa) -> 
    true;
rsa_keyed(_) -> 
    false.

%% Certs key is an RSA key
rsa_keyed_suites(Ciphers) ->
   filter_kex(Ciphers, fun rsa_keyed/1).

%% RSA Certs key can be used for encipherment
rsa_suites_encipher(Ciphers) ->
    filter_kex(Ciphers, fun(rsa) -> true;
                           (rsa_psk) -> true;
                           (_) -> false
                        end).


%% Cert should be have DSS key (DSA)
dss_keyed_suites(Ciphers) ->
    filter_kex(Ciphers, fun (dhe_dss) -> true;
                            (spr_dss) -> true;
                            (_) ->  false
                        end).

%% Cert should be signed by DSS (DSA)
dsa_signed_suites(Ciphers) ->
    filter_kex(Ciphers, fun(dhe_dss) -> true;
                              (_) -> false
                           end).

dss_dhe_suites(Ciphers) ->
    filter_kex(Ciphers, fun(dhe_dss) -> true;
                           (_) -> false
                           end).
%% Certs key is an ECC key
ec_keyed_suites(Ciphers) ->
    filter_kex(Ciphers, fun (ecdh_ecdsa)  -> true;
                            (ecdh_rsa)    -> true;
                            (ecdhe_ecdsa) -> true;
                            (_)           -> false
                        end).

%% EC Certs key usage keyAgreement
ec_ecdh_suites(Ciphers)->
    filter_kex(Ciphers, fun(ecdh_ecdsa) -> true;
                           (_)          -> false
                        end).

%% EC Certs key usage digitalSignature
ec_ecdhe_suites(Ciphers) ->
    filter_kex(Ciphers, fun(ecdhe_ecdsa) -> true;
                           (ecdhe_rsa)   -> true;
                           (_)           -> false
                        end).
%% RSA Certs key usage digitalSignature
rsa_ecdhe_dhe_suites(Ciphers) ->
    filter_kex(Ciphers, fun(dhe_rsa) -> true;
                           (ecdhe_rsa) -> true;
                           (_) -> false
                        end).

filter_kex(Ciphers, Fn) ->
    filter_suites(Ciphers, #{key_exchange_filters => [Fn],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).


key_uses(OtpCert) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate, 
    TBSExtensions = TBSCert#'OTPTBSCertificate'.extensions,
    Extensions = ssl_certificate:extensions_list(TBSExtensions),
    case ssl_certificate:select_extension(?'id-ce-keyUsage', Extensions) of
	undefined ->
	    [];
	#'Extension'{extnValue = KeyUses} ->
            KeyUses
    end.

%% If no key-usage extension is defined all key-usages are allowed
filter_keyuse_suites(_, [], CiphersSuites, _) ->
    CiphersSuites;
filter_keyuse_suites(Use, KeyUse, CipherSuits, Suites) ->
    case ssl_certificate:is_valid_key_usage(KeyUse, Use) of
	true ->
	    CipherSuits;
	false ->
	    CipherSuits -- Suites
    end.

generate_server_share(Group) ->
    Key = generate_key_exchange(Group),
    #key_share_server_hello{
       server_share = #key_share_entry{
                         group = Group,
                         key_exchange = Key
                        }}.

generate_client_shares(Groups) ->
    KeyShareEntry = fun (Group) ->
                        #key_share_entry{group = Group, key_exchange = generate_key_exchange(Group)}
                    end,
    ClientShares = lists:map(KeyShareEntry, Groups),
    #key_share_client_hello{client_shares = ClientShares}.

generate_key_exchange(secp256r1) ->
    public_key:generate_key({namedCurve, secp256r1});
generate_key_exchange(secp384r1) ->
    public_key:generate_key({namedCurve, secp384r1});
generate_key_exchange(secp521r1) ->
    public_key:generate_key({namedCurve, secp521r1});
generate_key_exchange(x25519) ->
    crypto:generate_key(ecdh, x25519);
generate_key_exchange(x448) ->
    crypto:generate_key(ecdh, x448);
generate_key_exchange(FFDHE) ->
    public_key:generate_key(ssl_dh_groups:dh_params(FFDHE)).


%% TODO: Move this functionality to crypto!
%% 7.4.1.  Finite Field Diffie-Hellman
%%
%%    For finite field groups, a conventional Diffie-Hellman [DH76]
%%    computation is performed.  The negotiated key (Z) is converted to a
%%    byte string by encoding in big-endian form and left-padded with zeros
%%    up to the size of the prime.  This byte string is used as the shared
%%    secret in the key schedule as specified above.
add_zero_padding(Bin, PrimeSize)
  when byte_size (Bin) =:= PrimeSize ->
    Bin;
add_zero_padding(Bin, PrimeSize) ->
    add_zero_padding(<<0, Bin/binary>>, PrimeSize).


%% Functions for handling self-encrypted session tickets (TLS 1.3).
%%
encrypt_ticket(#stateless_ticket{
                  hash = Hash,
                  pre_shared_key = PSK,
                  ticket_age_add = TicketAgeAdd,
                  lifetime = Lifetime,
                  timestamp = Timestamp,
                  certificate = Certificate
                 }, Shard, IV) ->
    Plaintext1 = <<(ssl_cipher:hash_algorithm(Hash)):8,PSK/binary,
                   ?UINT64(TicketAgeAdd),?UINT32(Lifetime),?UINT32(Timestamp)>>,
    CertificateLength = case Certificate of
                            undefined -> 0;
                            _ -> byte_size(Certificate)
    end,
    Plaintext = case CertificateLength of
                    0 ->
                        <<Plaintext1/binary,?UINT16(0)>>;
                    _ ->
                        <<Plaintext1/binary,?UINT16(CertificateLength),
                          Certificate/binary>>
                end,
    encrypt_ticket_data(Plaintext, Shard, IV).


decrypt_ticket(CipherFragment, Shard, IV) ->
    case decrypt_ticket_data(CipherFragment, Shard, IV) of
        error ->
            error;
        Plaintext ->
            <<?BYTE(HKDF),T/binary>> = Plaintext,
            Hash = hash_algorithm(HKDF),
            HashSize = hash_size(Hash),
            <<PSK:HashSize/binary,?UINT64(TicketAgeAdd),?UINT32(Lifetime),?UINT32(Timestamp),
                ?UINT16(CertificateLength),Certificate1:CertificateLength/binary,_/binary>> = T,
            Certificate = case CertificateLength of
                              0 -> undefined;
                              _ -> Certificate1
                          end,
            #stateless_ticket{
               hash = Hash,
               pre_shared_key = PSK,
               ticket_age_add = TicketAgeAdd,
               lifetime = Lifetime,
               timestamp = Timestamp,
               certificate = Certificate
              }
    end.


encrypt_ticket_data(Plaintext, Shard, IV) ->
    AAD = additional_data(<<"ticket">>, erlang:iolist_size(Plaintext) + 16), %% TagLen = 16
    {OTP, Key} = make_otp_key(Shard),
    {Content, CipherTag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Plaintext, AAD, 16, true),
    <<Content/binary,CipherTag/binary,OTP/binary>>.


decrypt_ticket_data(CipherFragment, Shard, IV) ->
    Size = byte_size(Shard),
    AAD = additional_data(<<"ticket">>, erlang:iolist_size(CipherFragment) - Size),
    Len = byte_size(CipherFragment) - Size - 16,
    case CipherFragment of
        <<Encrypted:Len/binary,CipherTag:16/binary,OTP:Size/binary>> ->
            Key = crypto:exor(OTP, Shard),
            crypto:crypto_one_time_aead(aes_256_gcm, Key, IV,
                                        Encrypted, AAD, CipherTag,
                                        false);
        _ ->
            error
    end.

encrypt_data(ADTag, Plaintext, Shard, IV) ->
    AAD = additional_data(ADTag, erlang:iolist_size(Plaintext) + 16), %% TagLen = 16
    {OTP, Key} = make_otp_key(Shard),
    {Content, CipherTag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Plaintext, AAD, 16, true),
    <<Content/binary,CipherTag/binary,OTP/binary>>.

decrypt_data(ADTag, CipherFragment, Shard, IV) ->
    Size = byte_size(Shard),
    AAD = additional_data(ADTag, erlang:iolist_size(CipherFragment) - Size),
    Len = byte_size(CipherFragment) - Size - 16,
    <<Encrypted:Len/binary,CipherTag:16/binary,OTP:Size/binary>> = CipherFragment,
    Key = crypto:exor(OTP, Shard),
    crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Encrypted, AAD, CipherTag, false).

additional_data(Tag, Length) ->
    <<Tag/binary,?UINT16(Length)>>.

make_otp_key(Shard) ->
    Size = byte_size(Shard),
    OTP = crypto:strong_rand_bytes(Size),
    Key = crypto:exor(OTP, Shard),
    {OTP, Key}.
