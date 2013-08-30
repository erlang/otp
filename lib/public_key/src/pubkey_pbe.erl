%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
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
%% Description: Implements Password Based Encryption PKCS-5, RFC-2898

-module(pubkey_pbe).

-include("public_key.hrl").

-export([encode/4, decode/4, decrypt_parameters/1]). 
-export([pbdkdf1/4, pbdkdf2/7]).

-define(DEFAULT_SHA_MAC_KEYLEN, 20).
-define(ASN1_OCTET_STR_TAG, 4).
-define(IV_LEN, 8).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec encode(binary(), string(), string(), term()) -> binary().
%%
%% Description: Performs password based encoding
%%--------------------------------------------------------------------
encode(Data, Password, "DES-CBC" = Cipher, KeyDevParams) ->
    {Key, IV} = password_to_key_and_iv(Password, Cipher, KeyDevParams),
    crypto:block_encrypt(des_cbc, Key, IV, Data);

encode(Data, Password, "DES-EDE3-CBC" = Cipher, KeyDevParams) ->
    {Key, IV} = password_to_key_and_iv(Password, Cipher, KeyDevParams),
    <<Key1:8/binary, Key2:8/binary, Key3:8/binary>> = Key,
    crypto:block_encrypt(des3_cbc, [Key1, Key2, Key3], IV, Data);

encode(Data, Password, "RC2-CBC" = Cipher, KeyDevParams) ->
    {Key, IV} = password_to_key_and_iv(Password, Cipher, KeyDevParams),
    crypto:block_encrypt(rc2_cbc, Key, IV, Data).
%%--------------------------------------------------------------------
-spec decode(binary(), string(), string(), term()) -> binary().
%%
%% Description: Performs password based decoding
%%--------------------------------------------------------------------
decode(Data, Password,"DES-CBC"= Cipher, KeyDevParams) ->
    {Key, IV} = password_to_key_and_iv(Password, Cipher, KeyDevParams),
    crypto:block_decrypt(des_cbc, Key, IV, Data);

decode(Data, Password,"DES-EDE3-CBC" = Cipher, KeyDevParams) ->
    {Key, IV} = password_to_key_and_iv(Password, Cipher, KeyDevParams),
    <<Key1:8/binary, Key2:8/binary, Key3:8/binary>> = Key,
    crypto:block_decrypt(des3_cbc, [Key1, Key2, Key3], IV, Data);

decode(Data, Password,"RC2-CBC"= Cipher, KeyDevParams) ->
    {Key, IV} = password_to_key_and_iv(Password, Cipher, KeyDevParams),
    crypto:block_decrypt(rc2_cbc, Key, IV, Data);

decode(Data, Password,"AES-128-CBC"= Cipher, IV) ->
    %% PKCS5_SALT_LEN is 8 bytes
    <<Salt:8/binary,_/binary>> = IV,
    {Key, _} = password_to_key_and_iv(Password, Cipher, Salt),
    crypto:block_decrypt(aes_cbc128, Key, IV, Data).

%%--------------------------------------------------------------------
-spec pbdkdf1(string(), iodata(), integer(), atom()) -> binary().
%%
%% Description: Implements password based decryption key derive function 1.
%% Exported mainly for testing purposes.
%%--------------------------------------------------------------------
pbdkdf1(_, _, 0, Acc) ->
    Acc;
pbdkdf1(Password, Salt, Count, Hash) ->
    Result = crypto:hash(Hash, [Password, Salt]),
    do_pbdkdf1(Result, Count-1, Result, Hash).

%%--------------------------------------------------------------------
-spec pbdkdf2(string(), iodata(), integer(), integer(), fun(), atom(), integer())
	     -> binary().
%%
%% Description: Implements password based decryption key derive function 2.
%% Exported mainly for testing purposes.
%%--------------------------------------------------------------------
pbdkdf2(Password, Salt, Count, DerivedKeyLen, Prf, PrfHash, PrfOutputLen)->
    NumBlocks = ceiling(DerivedKeyLen / PrfOutputLen),
    NumLastBlockOctets = DerivedKeyLen - (NumBlocks - 1) * PrfOutputLen ,
    blocks(NumBlocks, NumLastBlockOctets, 1, Password, Salt, 
	   Count, Prf, PrfHash, PrfOutputLen, <<>>).
%%--------------------------------------------------------------------
-spec decrypt_parameters(#'EncryptedPrivateKeyInfo_encryptionAlgorithm'{}) -> 
				{Cipher::string(), #'PBES2-params'{}}.
%%
%% Description: Performs ANS1-decoding of encryption parameters.
%%--------------------------------------------------------------------
decrypt_parameters(#'EncryptedPrivateKeyInfo_encryptionAlgorithm'{
		      algorithm = Oid, parameters = Param}) ->
     decrypt_parameters(Oid, Param).
    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
password_to_key_and_iv(Password, _, #'PBES2-params'{} = Params) ->
    {Salt, ItrCount, KeyLen, PseudoRandomFunction, PseudoHash, PseudoOtputLen, IV} =
	key_derivation_params(Params),
    <<Key:KeyLen/binary, _/binary>> = 
	pbdkdf2(Password, Salt, ItrCount, KeyLen, PseudoRandomFunction, PseudoHash, PseudoOtputLen),
    {Key, IV};
password_to_key_and_iv(Password, Cipher, Salt) ->
    KeyLen = derived_key_length(Cipher, undefined),
    <<Key:KeyLen/binary, _/binary>> = 
	pem_encrypt(<<>>, Password, Salt, ceiling(KeyLen div 16), <<>>, md5),
    %% Old PEM encryption does not use standard encryption method
    %% pbdkdf1 and uses then salt as IV
    {Key, Salt}.

pem_encrypt(_, _, _, 0, Acc, _) ->
    Acc;
pem_encrypt(Prev, Password, Salt, Count, Acc, Hash) ->
    Result = crypto:hash(Hash, [Prev, Password, Salt]),
    pem_encrypt(Result, Password, Salt, Count-1 , <<Acc/binary, Result/binary>>, Hash).

do_pbdkdf1(_, 0, Acc, _) ->
    Acc;
do_pbdkdf1(Prev, Count, Acc, Hash) ->
    Result = crypto:hash(Hash, Prev),
    do_pbdkdf1(Result, Count-1 , <<Result/binary, Acc/binary>>, Hash).

iv(#'PBES2-params_encryptionScheme'{algorithm = Algo,
				    parameters = ASNIV}) when (Algo == ?'desCBC') or
							      (Algo == ?'des-EDE3-CBC') ->
    %% This is an so called open ASN1-type that in this
    %% case will be an octet-string of length 8
    <<?ASN1_OCTET_STR_TAG, ?IV_LEN, IV:?IV_LEN/binary>> = ASNIV,
    IV;
iv(#'PBES2-params_encryptionScheme'{algorithm = ?'rc2CBC',
				    parameters = ASN1IV}) ->
    {ok, #'RC2-CBC-Parameter'{iv = IV}} = 'PKCS-FRAME':decode('RC2-CBC-Parameter', ASN1IV),
    iolist_to_binary(IV).

blocks(1, N, Index, Password, Salt, Count, Prf, PrfHash, PrfLen, Acc) ->
    <<XorSum:N/binary, _/binary>> = xor_sum(Password, Salt, Count, Index, Prf, PrfHash, PrfLen),
    <<Acc/binary, XorSum/binary>>;
blocks(NumBlocks, N, Index, Password, Salt, Count, Prf, PrfHash, PrfLen, Acc) ->
    XorSum = xor_sum(Password, Salt, Count, Index, Prf, PrfHash, PrfLen),
    blocks(NumBlocks -1, N, Index +1, Password, Salt, Count, Prf, PrfHash, 
	   PrfLen, <<Acc/binary, XorSum/binary>>).

xor_sum(Password, Salt, Count, Index, Prf, PrfHash, PrfLen) ->
    Result = Prf(PrfHash, Password, [Salt,<<Index:32/unsigned-big-integer>>], PrfLen),
    do_xor_sum(Prf, PrfHash, PrfLen, Result, Password, Count-1, Result).

do_xor_sum(_, _, _, _, _, 0, Acc) ->
    Acc;
do_xor_sum(Prf, PrfHash, PrfLen, Prev, Password, Count, Acc)-> 					   
    Result = Prf(PrfHash, Password, Prev, PrfLen),
    do_xor_sum(Prf, PrfHash, PrfLen, Result, Password, Count-1, crypto:exor(Acc, Result)).

decrypt_parameters(?'id-PBES2', DekParams) ->
    {ok, Params} = 'PKCS-FRAME':decode('PBES2-params', DekParams),
    {cipher(Params#'PBES2-params'.encryptionScheme), Params}.

key_derivation_params(#'PBES2-params'{keyDerivationFunc = KeyDerivationFunc,
				      encryptionScheme = EncScheme}) ->
    #'PBES2-params_keyDerivationFunc'{algorithm = ?'id-PBKDF2',
				      parameters =
					  #'PBKDF2-params'{salt = {specified, OctetSalt},
							   iterationCount = Count,
							   keyLength = Length,
							   prf = Prf}} = KeyDerivationFunc,
    #'PBES2-params_encryptionScheme'{algorithm = Algo} = EncScheme,
    {PseudoRandomFunction, PseudoHash, PseudoOtputLen} = pseudo_random_function(Prf),
    KeyLen = derived_key_length(Algo, Length),
    {OctetSalt, Count, KeyLen,
     PseudoRandomFunction, PseudoHash, PseudoOtputLen, iv(EncScheme)}.

%% This function currently matches a tuple that ougth to be the value
%% ?'id-hmacWithSHA1, but we need some kind of ASN1-fix for this.
pseudo_random_function(#'PBKDF2-params_prf'{algorithm = 
						{_,_, _,'id-hmacWithSHA1'}}) ->
    {fun crypto:hmac/4, sha, pseudo_output_length(?'id-hmacWithSHA1')};
pseudo_random_function(#'PBKDF2-params_prf'{algorithm = ?'id-hmacWithSHA1'}) ->
    {fun crypto:hmac/4, sha, pseudo_output_length(?'id-hmacWithSHA1')}.

pseudo_output_length(?'id-hmacWithSHA1') ->
    ?DEFAULT_SHA_MAC_KEYLEN.

derived_key_length(_, Len) when is_integer(Len) ->
    Len;
derived_key_length(Cipher,_) when (Cipher == ?'desCBC') or 
				  (Cipher == "DES-CBC") ->
    8;
derived_key_length(Cipher,_) when (Cipher == ?'rc2CBC') or 
				  (Cipher == "RC2-CBC") ->
    16;
derived_key_length(Cipher,_) when (Cipher == ?'des-EDE3-CBC') or 
				  (Cipher == "DES-EDE3-CBC") ->
    24;
derived_key_length(Cipher,_) when (Cipher == "AES-128-CBC") ->
    16.

cipher(#'PBES2-params_encryptionScheme'{algorithm = ?'desCBC'}) ->
    "DES-CBC";
cipher(#'PBES2-params_encryptionScheme'{algorithm = ?'des-EDE3-CBC'}) ->
    "DES-EDE3-CBC";
cipher(#'PBES2-params_encryptionScheme'{algorithm = ?'rc2CBC'}) ->
    "RC2-CBC".

ceiling(Float) -> 
    erlang:round(Float + 0.5).
