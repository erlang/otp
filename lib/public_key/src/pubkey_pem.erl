%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%%% Description: Reading and writing of PEM type encoded files.
%% PEM encoded files have the following structure:
%%
%%	<text>
%%	-----BEGIN SOMETHING-----<CR><LF>
%%	<Base64 encoding line><CR><LF>
%%	<Base64 encoding line><CR><LF>
%%	...
%%	-----END SOMETHING-----<CR><LF>
%%	<text>
%%
%% A file can contain several BEGIN/END blocks. Text lines between
%% blocks are ignored.
%%
%% The encoding is divided into lines separated by <NL>, and each line
%% is precisely 64 characters long (excluding the <NL> characters,
%% except the last line which 64 characters long or shorter. <NL> may
%% follow the last line.

-module(pubkey_pem).

-include("public_key.hrl").

-export([encode/1, decode/1, decipher/2, cipher/3]).

-define(ENCODED_LINE_LENGTH, 64).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec decode(binary()) -> [public_key:pem_entry()].
%%
%% Description: Decodes a PEM binary.
%%--------------------------------------------------------------------		    
decode(Bin) ->
    decode_pem_entries(split_bin(Bin), []).

%%--------------------------------------------------------------------
-spec encode([public_key:pem_entry()]) -> iolist().
%%
%% Description: Encodes a list of PEM entries.
%%--------------------------------------------------------------------		    
encode(PemEntries) ->
    encode_pem_entries(PemEntries).

%%--------------------------------------------------------------------
-spec decipher({public_key:pki_asn1_type(), DerEncrypted::binary(),
		{Cipher :: string(), Salt :: iodata() | #'PBES2-params'{} 
					   | {#'PBEParameter'{}, atom()}}},
	       string()) -> Der::binary().
%%
%% Description: Deciphers a decrypted pem entry.
%%--------------------------------------------------------------------
decipher({_, DecryptDer, {Cipher, KeyDevParams}}, Password) ->
    pubkey_pbe:decode(DecryptDer, Password, Cipher, KeyDevParams).

%%--------------------------------------------------------------------
-spec cipher(Der::binary(), {Cipher :: string(), Salt :: iodata() | #'PBES2-params'{} 
						       | {#'PBEParameter'{}, atom()}}, 
	     string()) -> binary().
%%
%% Description: Ciphers a PEM entry
%%--------------------------------------------------------------------
cipher(Der, {Cipher, KeyDevParams}, Password)->
    pubkey_pbe:encode(Der, Password, Cipher, KeyDevParams).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
encode_pem_entries(Entries) ->
    [encode_pem_entry(Entry) || Entry <- Entries].

encode_pem_entry({Type, Der, not_encrypted}) ->
    StartStr = pem_start(Type),
    [StartStr, "\n", b64encode_and_split(Der), "\n", pem_end(StartStr) ,"\n\n"];
encode_pem_entry({'PrivateKeyInfo', Der, EncParams}) -> 
    EncDer = encode_encrypted_private_keyinfo(Der, EncParams),
    StartStr = pem_start('EncryptedPrivateKeyInfo'),
    [StartStr, "\n", b64encode_and_split(EncDer), "\n", pem_end(StartStr) ,"\n\n"];
encode_pem_entry({Type, Der, {Cipher, Salt}}) ->
    StartStr = pem_start(Type),
    [StartStr,"\n", pem_decrypt(),"\n", pem_decrypt_info(Cipher, Salt),"\n\n",
     b64encode_and_split(Der), "\n", pem_end(StartStr) ,"\n\n"].

decode_pem_entries([], Entries) ->
    lists:reverse(Entries);
decode_pem_entries([<<>>], Entries) ->
   lists:reverse(Entries);
decode_pem_entries([<<>> | Lines], Entries) ->
    decode_pem_entries(Lines, Entries);
decode_pem_entries([Start| Lines], Entries) ->
    case pem_end(Start) of
	undefined ->
	    decode_pem_entries(Lines, Entries);
	_End ->
	    {Entry, RestLines} = join_entry(Lines, []),
	    decode_pem_entries(RestLines, [decode_pem_entry(Start, Entry) | Entries])
    end.

decode_pem_entry(Start, [<<"Proc-Type: 4,ENCRYPTED", _/binary>>, Line | Lines]) ->
    Type = asn1_type(Start),
    Cs = erlang:iolist_to_binary(Lines),
    Decoded = base64:mime_decode(Cs),
    [_, DekInfo0] = string:tokens(binary_to_list(Line), ": "),
    [Cipher, Salt] = string:tokens(DekInfo0, ","), 
    {Type, Decoded, {Cipher, unhex(Salt)}};
decode_pem_entry(Start, Lines) ->
    Type = asn1_type(Start),
    Cs = erlang:iolist_to_binary(Lines),
    Decoded = base64:mime_decode(Cs),
    case Type of
	'EncryptedPrivateKeyInfo'->
	    decode_encrypted_private_keyinfo(Decoded);
	_ ->
	    {Type, Decoded, not_encrypted}
    end.

decode_encrypted_private_keyinfo(Der) ->
    #'EncryptedPrivateKeyInfo'{encryptionAlgorithm = AlgorithmInfo,				  
			       encryptedData = Data} = 
	public_key:der_decode('EncryptedPrivateKeyInfo', Der),
    DecryptParams = pubkey_pbe:decrypt_parameters(AlgorithmInfo), 
    {'PrivateKeyInfo', Data, DecryptParams}.

encode_encrypted_private_keyinfo(EncData, EncryptParmams) ->
    AlgorithmInfo = pubkey_pbe:encrypt_parameters(EncryptParmams),
    public_key:der_encode('EncryptedPrivateKeyInfo',   
			  #'EncryptedPrivateKeyInfo'{encryptionAlgorithm = AlgorithmInfo,
						     encryptedData = EncData}).
split_bin(Bin) ->
    split_bin(0, Bin).

split_bin(N, Bin) ->
    case Bin of
	<<Line:N/binary, "\r\n", Rest/binary>> ->
	    [Line | split_bin(0, Rest)];
	<<Line:N/binary, "\n", Rest/binary>> ->
	    [Line | split_bin(0, Rest)];
	<<Line:N/binary>> ->
	    [Line];
	_ ->
	    split_bin(N+1, Bin)
    end.

b64encode_and_split(Bin) ->
    split_lines(base64:encode(Bin)).

split_lines(<<Text:?ENCODED_LINE_LENGTH/binary>>) ->
    [Text];
split_lines(<<Text:?ENCODED_LINE_LENGTH/binary, Rest/binary>>) ->
    [Text, $\n | split_lines(Rest)];
split_lines(Bin) ->
    [Bin].

%% Ignore white space at end of line
join_entry([<<"-----END ", _/binary>>| Lines], Entry) ->
    {lists:reverse(Entry), Lines};
join_entry([<<"-----END X509 CRL-----", _/binary>>| Lines], Entry) ->
    {lists:reverse(Entry), Lines};
join_entry([Line | Lines], Entry) ->
    join_entry(Lines, [Line | Entry]).

unhex(S) ->
    unhex(S, []).

unhex("", Acc) ->
    list_to_binary(lists:reverse(Acc));
unhex([D1, D2 | Rest], Acc) ->
    unhex(Rest, [erlang:list_to_integer([D1, D2], 16) | Acc]).

hexify(L) -> [[hex_byte(B)] || B <- binary_to_list(L)].

hex_byte(B) when B < 16#10 -> ["0", erlang:integer_to_list(B, 16)];
hex_byte(B) -> erlang:integer_to_list(B, 16).

pem_start('Certificate') ->
    <<"-----BEGIN CERTIFICATE-----">>;
pem_start('RSAPrivateKey') ->
    <<"-----BEGIN RSA PRIVATE KEY-----">>;
pem_start('RSAPublicKey') ->
    <<"-----BEGIN RSA PUBLIC KEY-----">>;
pem_start('SubjectPublicKeyInfo') ->
    <<"-----BEGIN PUBLIC KEY-----">>;
pem_start('DSAPrivateKey') ->
    <<"-----BEGIN DSA PRIVATE KEY-----">>;
pem_start('DHParameter') ->
    <<"-----BEGIN DH PARAMETERS-----">>;
pem_start('EncryptedPrivateKeyInfo') ->
    <<"-----BEGIN ENCRYPTED PRIVATE KEY-----">>;
pem_start('CertificationRequest') ->
    <<"-----BEGIN CERTIFICATE REQUEST-----">>;
pem_start('ContentInfo') ->
    <<"-----BEGIN PKCS7-----">>;
pem_start('CertificateList') ->
     <<"-----BEGIN X509 CRL-----">>;
pem_start('EcpkParameters') ->
    <<"-----BEGIN EC PARAMETERS-----">>;
pem_start('ECPrivateKey') ->
    <<"-----BEGIN EC PRIVATE KEY-----">>.

pem_end(<<"-----BEGIN CERTIFICATE-----">>) ->
    <<"-----END CERTIFICATE-----">>;
pem_end(<<"-----BEGIN RSA PRIVATE KEY-----">>) ->
    <<"-----END RSA PRIVATE KEY-----">>;
pem_end(<<"-----BEGIN RSA PUBLIC KEY-----">>) ->
    <<"-----END RSA PUBLIC KEY-----">>;
pem_end(<<"-----BEGIN PUBLIC KEY-----">>) ->
    <<"-----END PUBLIC KEY-----">>;
pem_end(<<"-----BEGIN DSA PRIVATE KEY-----">>) ->
    <<"-----END DSA PRIVATE KEY-----">>;
pem_end(<<"-----BEGIN DH PARAMETERS-----">>) ->
    <<"-----END DH PARAMETERS-----">>;
pem_end(<<"-----BEGIN PRIVATE KEY-----">>) ->
    <<"-----END PRIVATE KEY-----">>;
pem_end(<<"-----BEGIN ENCRYPTED PRIVATE KEY-----">>) ->
    <<"-----END ENCRYPTED PRIVATE KEY-----">>;
pem_end(<<"-----BEGIN CERTIFICATE REQUEST-----">>) ->
    <<"-----END CERTIFICATE REQUEST-----">>;
pem_end(<<"-----BEGIN PKCS7-----">>) ->
    <<"-----END PKCS7-----">>;
pem_end(<<"-----BEGIN X509 CRL-----">>) ->
    <<"-----END X509 CRL-----">>;
pem_end(<<"-----BEGIN EC PARAMETERS-----">>) ->
    <<"-----END EC PARAMETERS-----">>;
pem_end(<<"-----BEGIN EC PRIVATE KEY-----">>) ->
    <<"-----END EC PRIVATE KEY-----">>;
pem_end(_) ->
    undefined.

asn1_type(<<"-----BEGIN CERTIFICATE-----">>) ->
    'Certificate';
asn1_type(<<"-----BEGIN RSA PRIVATE KEY-----">>) ->
    'RSAPrivateKey';
asn1_type(<<"-----BEGIN RSA PUBLIC KEY-----">>) ->
    'RSAPublicKey';
asn1_type(<<"-----BEGIN PUBLIC KEY-----">>) ->
    'SubjectPublicKeyInfo';
asn1_type(<<"-----BEGIN DSA PRIVATE KEY-----">>) ->
    'DSAPrivateKey';
asn1_type(<<"-----BEGIN DH PARAMETERS-----">>) ->
    'DHParameter';
asn1_type(<<"-----BEGIN PRIVATE KEY-----">>) ->
    'PrivateKeyInfo';
asn1_type(<<"-----BEGIN ENCRYPTED PRIVATE KEY-----">>) ->
    'EncryptedPrivateKeyInfo';
asn1_type(<<"-----BEGIN CERTIFICATE REQUEST-----">>) ->
    'CertificationRequest';
asn1_type(<<"-----BEGIN PKCS7-----">>) ->
    'ContentInfo';
asn1_type(<<"-----BEGIN X509 CRL-----">>) ->
    'CertificateList';
asn1_type(<<"-----BEGIN EC PARAMETERS-----">>) ->
    'EcpkParameters';
asn1_type(<<"-----BEGIN EC PRIVATE KEY-----">>) ->
    'ECPrivateKey'.

pem_decrypt() ->
    <<"Proc-Type: 4,ENCRYPTED">>.

pem_decrypt_info(Cipher, Salt) ->
    io_lib:format("DEK-Info: ~s,~s", [Cipher, lists:flatten(hexify(Salt))]).
