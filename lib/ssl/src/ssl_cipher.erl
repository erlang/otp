%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Help functions for handling the SSL ciphers
%% 
%%----------------------------------------------------------------------

-module(ssl_cipher).

-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_alert.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([security_parameters/2, suite_definition/1,
	 decipher/5, cipher/4, 
	 suite/1, suites/1, anonymous_suites/0,
	 openssl_suite/1, openssl_suite_name/1, filter/2]).

-compile(inline).

%%--------------------------------------------------------------------
-spec security_parameters(cipher_suite(), #security_parameters{}) -> 
				 #security_parameters{}.
%%
%% Description: Returns a security parameters record where the
%% cipher values has been updated according to <CipherSuite> 
%%-------------------------------------------------------------------
security_parameters(CipherSuite, SecParams) ->
    { _, Cipher, Hash} = suite_definition(CipherSuite),
    SecParams#security_parameters{
      cipher_suite = CipherSuite,
      bulk_cipher_algorithm = bulk_cipher_algorithm(Cipher),
      cipher_type = type(Cipher),
      key_size = effective_key_bits(Cipher),
      expanded_key_material_length = expanded_key_material(Cipher),
      key_material_length = key_material(Cipher),
      iv_size = iv_size(Cipher),
      mac_algorithm = mac_algorithm(Hash),
      hash_size = hash_size(Hash)}.

%%--------------------------------------------------------------------
-spec cipher(cipher_enum(), #cipher_state{}, binary(), binary()) -> 
		    {binary(), #cipher_state{}}. 
%%
%% Description: Encrypts the data and the MAC using chipher described
%% by cipher_enum() and updating the cipher state
%%-------------------------------------------------------------------
cipher(?NULL, CipherState, <<>>, Fragment) ->
    GenStreamCipherList = [Fragment, <<>>],
    {GenStreamCipherList, CipherState};
cipher(?RC4, CipherState, Mac, Fragment) ->
    State0 = case CipherState#cipher_state.state of
                 undefined -> crypto:rc4_set_key(CipherState#cipher_state.key);
                 S -> S
             end,
    GenStreamCipherList = [Fragment, Mac],
    {State1, T} = crypto:rc4_encrypt_with_state(State0, GenStreamCipherList),
    {T, CipherState#cipher_state{state = State1}};
cipher(?DES, CipherState, Mac, Fragment) ->
    block_cipher(fun(Key, IV, T) ->
			 crypto:des_cbc_encrypt(Key, IV, T)
		 end, block_size(des_cbc), CipherState, Mac, Fragment);
cipher(?'3DES', CipherState, Mac, Fragment) ->
    block_cipher(fun(<<K1:8/binary, K2:8/binary, K3:8/binary>>, IV, T) ->
			 crypto:des3_cbc_encrypt(K1, K2, K3, IV, T)
		 end, block_size(des_cbc), CipherState, Mac, Fragment);
cipher(?AES, CipherState, Mac, Fragment) ->
    block_cipher(fun(Key, IV, T) when byte_size(Key) =:= 16 ->
			 crypto:aes_cbc_128_encrypt(Key, IV, T);
		    (Key, IV, T) when byte_size(Key) =:= 32 ->
			 crypto:aes_cbc_256_encrypt(Key, IV, T)
		 end, block_size(aes_128_cbc), CipherState, Mac, Fragment).
%% cipher(?IDEA, CipherState, Mac, Fragment) ->
%%     block_cipher(fun(Key, IV, T) ->
%% 			 crypto:idea_cbc_encrypt(Key, IV, T)
%% 		 end, block_size(idea_cbc), CipherState, Mac, Fragment);

block_cipher(Fun, BlockSz, #cipher_state{key=Key, iv=IV} = CS0, 
	     Mac, Fragment) ->
    TotSz = byte_size(Mac) + erlang:iolist_size(Fragment) + 1,
    {PaddingLength, Padding} = get_padding(TotSz, BlockSz),
    L = [Fragment, Mac, PaddingLength, Padding],
    T = Fun(Key, IV, L),
    NextIV = next_iv(T, IV),
    {T, CS0#cipher_state{iv=NextIV}}.

%%--------------------------------------------------------------------
-spec decipher(cipher_enum(), integer(), #cipher_state{}, binary(), tls_version()) ->
		      {binary(), binary(), #cipher_state{}} | #alert{}.
%%
%% Description: Decrypts the data and the MAC using cipher described
%% by cipher_enum() and updating the cipher state.
%%-------------------------------------------------------------------
decipher(?NULL, _HashSz, CipherState, Fragment, _) ->
    {Fragment, <<>>, CipherState};
decipher(?RC4, HashSz, CipherState, Fragment, _) ->
    State0 = case CipherState#cipher_state.state of
                 undefined -> crypto:rc4_set_key(CipherState#cipher_state.key);
                 S -> S
             end,
    try crypto:rc4_encrypt_with_state(State0, Fragment) of
	{State, Text} ->
	    GSC = generic_stream_cipher_from_bin(Text, HashSz),
	    #generic_stream_cipher{content = Content, mac = Mac} = GSC,
	    {Content, Mac, CipherState#cipher_state{state = State}}
    catch
	_:_ ->
	    %% This is a DECRYPTION_FAILED but
	    %% "differentiating between bad_record_mac and decryption_failed
	    %% alerts may permit certain attacks against CBC mode as used in
	    %% TLS [CBCATT].  It is preferable to uniformly use the
	    %% bad_record_mac alert to hide the specific type of the error."
	    ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
    end;

decipher(?DES, HashSz, CipherState, Fragment, Version) ->
    block_decipher(fun(Key, IV, T) ->
			   crypto:des_cbc_decrypt(Key, IV, T)
		   end, CipherState, HashSz, Fragment, Version);
decipher(?'3DES', HashSz, CipherState, Fragment, Version) ->
    block_decipher(fun(<<K1:8/binary, K2:8/binary, K3:8/binary>>, IV, T) ->
			   crypto:des3_cbc_decrypt(K1, K2, K3, IV, T)
		   end, CipherState, HashSz, Fragment, Version);
decipher(?AES, HashSz, CipherState, Fragment, Version) ->
    block_decipher(fun(Key, IV, T) when byte_size(Key) =:= 16 ->
			   crypto:aes_cbc_128_decrypt(Key, IV, T);
		      (Key, IV, T) when byte_size(Key) =:= 32 ->
			   crypto:aes_cbc_256_decrypt(Key, IV, T)
		   end, CipherState, HashSz, Fragment, Version).
%% decipher(?IDEA, HashSz, CipherState, Fragment, Version) ->
%%     block_decipher(fun(Key, IV, T) ->
%%  			   crypto:idea_cbc_decrypt(Key, IV, T)
%%  		   end, CipherState, HashSz, Fragment, Version);

block_decipher(Fun, #cipher_state{key=Key, iv=IV} = CipherState0, 
	       HashSz, Fragment, Version) ->
    try 
	Text = Fun(Key, IV, Fragment),
	GBC = generic_block_cipher_from_bin(Text, HashSz),
	Content = GBC#generic_block_cipher.content,
	Mac = GBC#generic_block_cipher.mac,
	CipherState1 = CipherState0#cipher_state{iv=next_iv(Fragment, IV)},
	case is_correct_padding(GBC, Version) of
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
	_:_ ->
	    %% This is a DECRYPTION_FAILED but
	    %% "differentiating between bad_record_mac and decryption_failed
	    %% alerts may permit certain attacks against CBC mode as used in
	    %% TLS [CBCATT].  It is preferable to uniformly use the
	    %% bad_record_mac alert to hide the specific type of the error."
	    ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
    end.
%%--------------------------------------------------------------------
-spec suites(tls_version()) -> [cipher_suite()].
%%
%% Description: Returns a list of supported cipher suites.
%%--------------------------------------------------------------------
suites({3, 0}) ->
    ssl_ssl3:suites();
suites({3, N}) when N == 1; N == 2 ->
    ssl_tls1:suites().

%%--------------------------------------------------------------------
-spec anonymous_suites() -> [cipher_suite()].
%%
%% Description: Returns a list of the anonymous cipher suites, only supported
%% if explicitly set by user. Intended only for testing.
%%--------------------------------------------------------------------
anonymous_suites() ->
    [?TLS_DH_anon_WITH_RC4_128_MD5,
     ?TLS_DH_anon_WITH_DES_CBC_SHA,
     ?TLS_DH_anon_WITH_3DES_EDE_CBC_SHA,
     ?TLS_DH_anon_WITH_AES_128_CBC_SHA,
     ?TLS_DH_anon_WITH_AES_256_CBC_SHA].

%%--------------------------------------------------------------------
-spec suite_definition(cipher_suite()) -> erl_cipher_suite().
%%
%% Description: Return erlang cipher suite definition.
%% Note: Currently not supported suites are commented away.
%% They should be supported or removed in the future.
%%-------------------------------------------------------------------
%% TLS v1.1 suites
suite_definition(?TLS_NULL_WITH_NULL_NULL) ->
    {null, null, null};
%% suite_definition(?TLS_RSA_WITH_NULL_MD5) ->
%%     {rsa, null, md5};
%% suite_definition(?TLS_RSA_WITH_NULL_SHA) ->
%%     {rsa, null, sha};	
suite_definition(?TLS_RSA_WITH_RC4_128_MD5) ->	
    {rsa, rc4_128, md5};
suite_definition(?TLS_RSA_WITH_RC4_128_SHA) ->	
    {rsa, rc4_128, sha};
%% suite_definition(?TLS_RSA_WITH_IDEA_CBC_SHA) -> 
%%     {rsa, idea_cbc, sha};
suite_definition(?TLS_RSA_WITH_DES_CBC_SHA) ->	
    {rsa, des_cbc, sha}; 
suite_definition(?TLS_RSA_WITH_3DES_EDE_CBC_SHA) ->
    {rsa, '3des_ede_cbc', sha}; 
suite_definition(?TLS_DHE_DSS_WITH_DES_CBC_SHA) ->
    {dhe_dss, des_cbc, sha};
suite_definition(?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA) ->
    {dhe_dss, '3des_ede_cbc', sha};
suite_definition(?TLS_DHE_RSA_WITH_DES_CBC_SHA) ->
    {dhe_rsa, des_cbc, sha};
suite_definition(?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA) ->
    {dhe_rsa, '3des_ede_cbc', sha}; 

%%% TSL V1.1 AES suites
suite_definition(?TLS_RSA_WITH_AES_128_CBC_SHA) -> 
    {rsa, aes_128_cbc, sha};
suite_definition(?TLS_DHE_DSS_WITH_AES_128_CBC_SHA) ->
    {dhe_dss, aes_128_cbc, sha};
suite_definition(?TLS_DHE_RSA_WITH_AES_128_CBC_SHA) ->
    {dhe_rsa, aes_128_cbc, sha};
suite_definition(?TLS_RSA_WITH_AES_256_CBC_SHA) -> 
    {rsa, aes_256_cbc, sha};
suite_definition(?TLS_DHE_DSS_WITH_AES_256_CBC_SHA) ->
    {dhe_dss, aes_256_cbc, sha};
suite_definition(?TLS_DHE_RSA_WITH_AES_256_CBC_SHA) ->
    {dhe_rsa, aes_256_cbc, sha};

%%% DH-ANON deprecated by TLS spec and not available
%%% by default, but good for testing purposes.
suite_definition(?TLS_DH_anon_WITH_RC4_128_MD5) ->
    {dh_anon, rc4_128, md5};
suite_definition(?TLS_DH_anon_WITH_DES_CBC_SHA) ->
    {dh_anon, des_cbc, sha};
suite_definition(?TLS_DH_anon_WITH_3DES_EDE_CBC_SHA) ->
    {dh_anon, '3des_ede_cbc', sha};
suite_definition(?TLS_DH_anon_WITH_AES_128_CBC_SHA) ->
    {dh_anon, aes_128_cbc, sha};
suite_definition(?TLS_DH_anon_WITH_AES_256_CBC_SHA) ->
    {dh_anon, aes_256_cbc, sha}.

%%--------------------------------------------------------------------
-spec suite(erl_cipher_suite()) -> cipher_suite().
%%
%% Description: Return TLS cipher suite definition.
%%--------------------------------------------------------------------

%% TLS v1.1 suites
%%suite({rsa, null, md5}) ->
%%    ?TLS_RSA_WITH_NULL_MD5;
%%suite({rsa, null, sha}) ->
%%    ?TLS_RSA_WITH_NULL_SHA;
suite({rsa, rc4_128, md5}) ->
    ?TLS_RSA_WITH_RC4_128_MD5;
suite({rsa, rc4_128, sha}) ->
    ?TLS_RSA_WITH_RC4_128_SHA;
%% suite({rsa, idea_cbc, sha}) -> 
%%     ?TLS_RSA_WITH_IDEA_CBC_SHA;
suite({rsa, des_cbc, sha}) ->
    ?TLS_RSA_WITH_DES_CBC_SHA; 
suite({rsa, '3des_ede_cbc', sha}) ->
    ?TLS_RSA_WITH_3DES_EDE_CBC_SHA; 
suite({dhe_dss, des_cbc, sha}) ->
    ?TLS_DHE_DSS_WITH_DES_CBC_SHA;
suite({dhe_dss, '3des_ede_cbc', sha}) ->
    ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA;
suite({dhe_rsa, des_cbc, sha}) ->
    ?TLS_DHE_RSA_WITH_DES_CBC_SHA;
suite({dhe_rsa, '3des_ede_cbc', sha}) ->
    ?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA; 
suite({dh_anon, rc4_128, md5}) ->
    ?TLS_DH_anon_WITH_RC4_128_MD5;
suite({dh_anon, des_cbc, sha}) ->
    ?TLS_DH_anon_WITH_DES_CBC_SHA;
suite({dh_anon, '3des_ede_cbc', sha}) ->
    ?TLS_DH_anon_WITH_3DES_EDE_CBC_SHA;

%%% TSL V1.1 AES suites
suite({rsa, aes_128_cbc, sha}) ->
    ?TLS_RSA_WITH_AES_128_CBC_SHA; 
suite({dhe_dss, aes_128_cbc, sha}) ->
    ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA; 
suite({dhe_rsa, aes_128_cbc, sha}) ->
    ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA;
suite({dh_anon, aes_128_cbc, sha}) ->
    ?TLS_DH_anon_WITH_AES_128_CBC_SHA;
suite({rsa, aes_256_cbc, sha}) ->
    ?TLS_RSA_WITH_AES_256_CBC_SHA;
suite({dhe_dss, aes_256_cbc, sha}) ->
    ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA;
suite({dhe_rsa, aes_256_cbc, sha}) ->
    ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA;
suite({dh_anon, aes_256_cbc, sha}) ->
    ?TLS_DH_anon_WITH_AES_256_CBC_SHA.

%%--------------------------------------------------------------------
-spec openssl_suite(openssl_cipher_suite()) -> cipher_suite().
%%
%% Description: Return TLS cipher suite definition.
%%--------------------------------------------------------------------
%% translate constants <-> openssl-strings
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
%%openssl_suite("IDEA-CBC-SHA") ->
%%    ?TLS_RSA_WITH_IDEA_CBC_SHA;
openssl_suite("RC4-SHA") ->
    ?TLS_RSA_WITH_RC4_128_SHA;
openssl_suite("RC4-MD5") -> 
    ?TLS_RSA_WITH_RC4_128_MD5;
openssl_suite("EDH-RSA-DES-CBC-SHA") ->
    ?TLS_DHE_RSA_WITH_DES_CBC_SHA;
openssl_suite("DES-CBC-SHA") ->
    ?TLS_RSA_WITH_DES_CBC_SHA.
%%--------------------------------------------------------------------
-spec openssl_suite_name(cipher_suite()) -> openssl_cipher_suite().
%%
%% Description: Return openssl cipher suite name.
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
%% openssl_suite_name(?TLS_RSA_WITH_IDEA_CBC_SHA) ->
%%     "IDEA-CBC-SHA";
openssl_suite_name(?TLS_RSA_WITH_RC4_128_SHA) ->
    "RC4-SHA";
openssl_suite_name(?TLS_RSA_WITH_RC4_128_MD5) -> 
    "RC4-MD5";
openssl_suite_name(?TLS_DHE_RSA_WITH_DES_CBC_SHA) ->
    "EDH-RSA-DES-CBC-SHA";
openssl_suite_name(?TLS_RSA_WITH_DES_CBC_SHA) ->
    "DES-CBC-SHA";
%% No oppenssl name
openssl_suite_name(Cipher) ->
    suite_definition(Cipher).

%%--------------------------------------------------------------------
-spec filter(undefined | binary(), [cipher_suite()]) -> [cipher_suite()].
%%
%% Description: .
%%-------------------------------------------------------------------
filter(undefined, Ciphers) -> 
    Ciphers;
filter(DerCert, Ciphers) ->
    OtpCert = public_key:pkix_decode_cert(DerCert, otp),
    SigAlg = OtpCert#'OTPCertificate'.signatureAlgorithm,
    case ssl_certificate:signature_type(SigAlg#'SignatureAlgorithm'.algorithm) of
	rsa ->
	    filter_rsa(OtpCert, Ciphers -- dsa_signed_suites());
	dsa ->
	    Ciphers -- rsa_signed_suites()
    end.
	
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

bulk_cipher_algorithm(null) ->
    ?NULL;
%% Not supported yet
%% bulk_cipher_algorithm(idea_cbc) ->
%%     ?IDEA;
bulk_cipher_algorithm(rc4_128) ->
    ?RC4;
bulk_cipher_algorithm(des_cbc) ->
    ?DES;
bulk_cipher_algorithm('3des_ede_cbc') ->
    ?'3DES';
bulk_cipher_algorithm(Cipher) when Cipher == aes_128_cbc;
				   Cipher == aes_256_cbc ->
    ?AES.

type(Cipher) when Cipher == null;
		  Cipher == rc4_128 ->
    ?STREAM;

type(Cipher) when Cipher == idea_cbc;
		  Cipher == des_cbc;
		  Cipher == '3des_ede_cbc';
		  Cipher == aes_128_cbc;
		  Cipher == aes_256_cbc ->
    ?BLOCK.

key_material(null) ->
    0;
key_material(Cipher) when Cipher == idea_cbc;
 			  Cipher == rc4_128 ->
    16;
key_material(des_cbc) ->
    8;
key_material('3des_ede_cbc') ->
    24;
key_material(aes_128_cbc) ->
    16;
key_material(aes_256_cbc) ->
    32.

expanded_key_material(null) ->
    0;
expanded_key_material(Cipher) when Cipher == idea_cbc;
 				   Cipher == rc4_128 ->
    16;
expanded_key_material(Cipher) when Cipher == des_cbc ->
    8;
expanded_key_material('3des_ede_cbc') ->
    24;
expanded_key_material(Cipher) when Cipher == aes_128_cbc;
 				   Cipher == aes_256_cbc ->
    unknown.  


effective_key_bits(null) ->
    0;
effective_key_bits(des_cbc) ->
    56;
effective_key_bits(Cipher) when Cipher == idea_cbc;
				Cipher == rc4_128;
				Cipher == aes_128_cbc ->
    128;
effective_key_bits('3des_ede_cbc') ->
    168;
effective_key_bits(aes_256_cbc) ->
    256.

iv_size(Cipher) when Cipher == null;
		     Cipher == rc4_128 ->
    0;
iv_size(Cipher) ->
    block_size(Cipher).

block_size(Cipher) when Cipher == idea_cbc;
			Cipher == des_cbc;
			Cipher == '3des_ede_cbc' -> 
    8;

block_size(Cipher) when Cipher == aes_128_cbc;
			Cipher == aes_256_cbc ->
    16.

mac_algorithm(null) ->
    ?NULL;
mac_algorithm(md5) ->
    ?MD5;
mac_algorithm(sha) ->
    ?SHA.

hash_size(null) ->
    0;
hash_size(md5) ->
    16;
hash_size(sha) ->
    20.

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
generic_block_cipher_from_bin(T, HashSize) ->
    Sz1 = byte_size(T) - 1,
    <<_:Sz1/binary, ?BYTE(PadLength0)>> = T,
    PadLength = if
		    PadLength0 >= Sz1 -> 0;
		    true -> PadLength0
		end,
    CompressedLength = byte_size(T) - PadLength - 1 - HashSize,
    <<Content:CompressedLength/binary, Mac:HashSize/binary,
     Padding:PadLength/binary, ?BYTE(PadLength0)>> = T,
    #generic_block_cipher{content=Content, mac=Mac,
			  padding=Padding, padding_length=PadLength0}.

generic_stream_cipher_from_bin(T, HashSz) ->
    Sz = byte_size(T),
    CompressedLength = Sz - HashSz,
    <<Content:CompressedLength/binary, Mac:HashSz/binary>> = T,
    #generic_stream_cipher{content=Content,
			   mac=Mac}.

%% For interoperability reasons we do not check the padding content in
%% SSL 3.0 and TLS 1.0 as it is not strictly required and breaks
%% interopability with for instance Google. 
is_correct_padding(#generic_block_cipher{padding_length = Len,
										 padding = Padding}, {3, N})
  when N == 0; N == 1 ->
    Len == byte_size(Padding); 
%% Padding must be check in TLS 1.1 and after  
is_correct_padding(#generic_block_cipher{padding_length = Len,
										 padding = Padding}, _) ->
    Len == byte_size(Padding) andalso
		list_to_binary(lists:duplicate(Len, Len)) == Padding.
										      
get_padding(Length, BlockSize) ->
    get_padding_aux(BlockSize, Length rem BlockSize).

get_padding_aux(_, 0) ->
    {0, <<>>};
get_padding_aux(BlockSize, PadLength) ->
    N = BlockSize - PadLength,
    {N, list_to_binary(lists:duplicate(N, N))}.

next_iv(Bin, IV) ->
    BinSz = byte_size(Bin),
    IVSz = byte_size(IV),
    FirstPart = BinSz - IVSz,
    <<_:FirstPart/binary, NextIV:IVSz/binary>> = Bin,
    NextIV.

rsa_signed_suites() ->
    dhe_rsa_suites() ++ rsa_suites().

dhe_rsa_suites() ->
    [?TLS_DHE_RSA_WITH_AES_256_CBC_SHA,
     ?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA,
     ?TLS_DHE_RSA_WITH_DES_CBC_SHA].

rsa_suites() ->
    [?TLS_RSA_WITH_AES_256_CBC_SHA,
     ?TLS_RSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_RSA_WITH_AES_128_CBC_SHA,
     %%?TLS_RSA_WITH_IDEA_CBC_SHA,
     ?TLS_RSA_WITH_RC4_128_SHA,
     ?TLS_RSA_WITH_RC4_128_MD5,
     ?TLS_RSA_WITH_DES_CBC_SHA].
    
dsa_signed_suites() ->
    dhe_dss_suites().

dhe_dss_suites()  ->
    [?TLS_DHE_DSS_WITH_AES_256_CBC_SHA,
     ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA,
     ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA,
     ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA].

filter_rsa(OtpCert, RsaCiphers) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate, 
    TBSExtensions = TBSCert#'OTPTBSCertificate'.extensions,
    Extensions = ssl_certificate:extensions_list(TBSExtensions),
    case ssl_certificate:select_extension(?'id-ce-keyUsage', Extensions) of
	undefined ->
	    RsaCiphers;
	#'Extension'{extnValue = KeyUse} ->
	    Result = filter_rsa_suites(keyEncipherment, 
				       KeyUse, RsaCiphers, rsa_suites()),
	    filter_rsa_suites(digitalSignature, 
			      KeyUse, Result, dhe_rsa_suites())
    end.

filter_rsa_suites(Use, KeyUse, CipherSuits, RsaSuites) ->
    case ssl_certificate:is_valid_key_usage(KeyUse, Use) of
	true ->
	    CipherSuits;
	false ->
	    CipherSuits -- RsaSuites
    end.
