%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2011. All Rights Reserved.
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
-export([pbdkdf1/4, pbdkdf2/6]).

-define(DEFAULT_SHA_MAC_KEYLEN, 20).

-define(OCTET_STR, 4).
-define(IV_LEN,    8).

%%====================================================================
%% Internal application API
%%====================================================================

pbdkdf2(Password, Salt, Count, DerivedKeyLen, PrfLen, Prf)->
    NumBlocks = ceiling(DerivedKeyLen / PrfLen),
    NumLastBlockOctets = DerivedKeyLen - (NumBlocks - 1) * PrfLen ,
    blocks(NumBlocks, NumLastBlockOctets, 1, Password, Salt, Count, Prf, PrfLen, <<>>).

encode(Data, Password, "DES-CBC" = Cipher, KeyDevParams) ->
    {Key, IV} = password_to_key_and_iv(Password, derived_key_length(Cipher), KeyDevParams),
    crypto:des_cbc_encrypt(Key, IV, Data);

encode(Data, Password, "DES-EDE3-CBC" = Cipher, KeyDevParams) ->
    {Key, IV} = password_to_key_and_iv(Password, derived_key_length(Cipher), KeyDevParams),
    <<Key1:8/binary, Key2:8/binary, Key3:8/binary>> = Key,
    crypto:des_ede3_cbc_encrypt(Key1, Key2, Key3, IV, Data).

decode(Data, Password,"DES-CBC"= Cipher, KeyDevParams) ->
    {Key, IV} = password_to_key_and_iv(Password, derived_key_length(Cipher), KeyDevParams),
    crypto:des_cbc_decrypt(Key, IV, Data);

decode(Data, Password,"DES-EDE3-CBC" = Cipher, KeyDevParams) ->
    {Key, IV} = password_to_key_and_iv(Password, derived_key_length(Cipher), KeyDevParams),
    <<Key1:8/binary, Key2:8/binary, Key3:8/binary>> = Key,
    crypto:des_ede3_cbc_decrypt(Key1, Key2, Key3, IV, Data).

%%--------------------------------------------------------------------
-spec pbdkdf1(string(), iodata(), integer(), atom()) -> binary().
%%
%% Description: Implements password based decryption key derive function 1.
%% Exported mainly for testing purposes.
%%--------------------------------------------------------------------
pbdkdf1(_, _, 0, Acc) ->
    Acc;
pbdkdf1(Password, Salt, Count, Hash) ->
    Result = crypto:Hash([Password, Salt]),
    do_pbdkdf1(Result, Count-1, Result, Hash).

%%--------------------------------------------------------------------
-spec pbdkdf2(string(), iodata(), integer(), integer(), fun(), integer())
	     -> binary().
%%
%% Description: Implements password based decryption key derive function 2.
%% Exported mainly for testing purposes.
%%--------------------------------------------------------------------
pbdkdf2(Password, Salt, Count, DerivedKeyLen, Prf, PrfOutputLen)->
    NumBlocks = ceiling(DerivedKeyLen / PrfOutputLen),
    NumLastBlockOctets = DerivedKeyLen - (NumBlocks - 1) * PrfOutputLen ,
    blocks(NumBlocks, NumLastBlockOctets, 1, Password, Salt, 
	   Count, Prf, PrfOutputLen, <<>>).
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
password_to_key_and_iv(Password, KeyLen, {salt, Salt}) ->
    <<Key:KeyLen/binary, _/binary>> = 
	pem_encrypt(<<>>, Password, Salt, ceiling(KeyLen div 16), <<>>, md5),
    %% Old PEM encryption does not use standard encryption method
    %% pbdkdf1 and uses then salt as IV
    {Key, Salt}.

pem_encrypt(_, _, _, 0, Acc, _) ->
    Acc;
pem_encrypt(Prev, Password, Salt, Count, Acc, Hash) ->
    Result = crypto:Hash([Prev, Password, Salt]),
    pem_encrypt(Result, Password, Salt, Count-1 , <<Acc/binary, Result/binary>>, Hash).

do_pbdkdf1(_, 0, Acc, _) ->
    Acc;
do_pbdkdf1(Prev, Count, Acc, Hash) ->
    Result = crypto:Hash(Prev),
    do_pbdkdf1(Result, Count-1 , <<Result/binary, Acc/binary>>, Hash).

iv(#'PBES2-params_encryptionScheme'{algorithm = Algo,
				    parameters = ASNIV}) when  (Algo == ?'desCBC') or
							       (Algo == ?'des-EDE3-CBC') ->
    %% This is an so called open ASN1-type that in this
    %% case will be an octet-string of length 8
    <<?OCTET_STR, ?IV_LEN, IV:?IV_LEN/binary>> = ASNIV,
    IV.

blocks(1, N, Index, Password, Salt, Count, Prf, PrfLen, Acc) ->
    <<XorSum:N/binary, _/binary>> = xor_sum(Password, Salt, Count, Index, Prf, PrfLen),
    <<Acc/binary, XorSum/binary>>;
blocks(NumBlocks, N, Index, Password, Salt, Count, Prf, PrfLen, Acc) ->
    XorSum = xor_sum(Password, Salt, Count, Index, Prf, PrfLen),
    blocks(NumBlocks -1, N, Index +1, Password, Salt, Count, Prf, PrfLen, <<Acc/binary, XorSum/binary>>).

xor_sum(Password, Salt, Count, Index, Prf, PrfLen) ->
    %%Result = Prf(Password, [Salt,<<Index:32/unsigned-big-integer>>], PrfLen),
    Result = Prf(Password, [Salt,<<Index:32/unsigned-big-integer>>]),
    do_xor_sum(Prf, PrfLen, Result, Password, Count-1, Result).

do_xor_sum(_, _, _, _, 0, Acc) ->
    Acc;
do_xor_sum(Prf, PrfLen, Prev, Password, Count, Acc)-> 					   
    %%Result = Prf(Password, Prev, PrfLen),
    Result = Prf(Password, Prev),
    do_xor_sum(Prf, PrfLen, Result, Password, Count-1, crypto:exor(Acc, Result)).

decrypt_parameters(?'id-PBES2', DekParams) ->
    {ok, Params} = 'PKCS-FRAME':decode('PBES2-params', DekParams),
    {cipher(Params#'PBES2-params'.encryptionScheme), Params}.

key_derivation_params(#'PBES2-params_keyDerivationFunc'{algorithm = ?'id-PBKDF2',
							parameters = 
							    #'PBKDF2-params'{salt = {specified, OctetSalt},
									     iterationCount = Count,
									     keyLength = Length,
									     prf = Prf}}) ->
    PseudoRandomFunction = pseudo_random_function(Prf),
    KeyLen = pseudo_key_length(Length, Prf),
    {iolist_to_binary(OctetSalt), Count, KeyLen, PseudoRandomFunction}.

pseudo_random_function(#'PBKDF2-params_prf'{algorithm = {_,_, _,'id-hmacWithSHA1'}}) ->
    %%fun crypto:sha_mac_n/3.
    fun crypto:sha_mac/2.

pseudo_key_length(asn1_NOVALUE, #'PBKDF2-params_prf'{algorithm = {_,_, _,'id-hmacWithSHA1'}}) ->
    ?DEFAULT_SHA_MAC_KEYLEN;
pseudo_key_length(Len, _) when is_integer(Len) ->
    Len.    

derived_key_length("DES-CBC") ->
    8;
%% derived_key_length("RC2-CBC") ->
%%     5;
derived_key_length("DES-EDE3-CBC") ->
    24.

cipher(#'PBES2-params_encryptionScheme'{algorithm = ?'desCBC'}) ->
    "DES-CBC";
cipher(#'PBES2-params_encryptionScheme'{algorithm = ?'des-EDE3-CBC'}) ->
    "DES-EDE3-CBC".
%% cipher(#'PBES2-params_encryptionScheme'{algorithm = ?'rc2CBC'}) ->
%%     "RC2-CBC".

ceiling(Float) -> 
    erlang:round(Float + 0.5).
