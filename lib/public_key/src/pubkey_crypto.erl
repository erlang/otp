%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%% Description: Functions that call the crypto driver.

-module(pubkey_crypto).

-include("public_key.hrl").

-export([encrypt_public/3, decrypt_private/3, 
	 encrypt_private/3, decrypt_public/3, 
	 sign/2, sign/3, verify/5, gen_key/2]).

-define(UINT32(X), X:32/unsigned-big-integer).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: encrypt(PlainText, Key, Padding) -> Encrypted
%%
%%	PlainText = binary()
%%	Key       = rsa_public_key() | rsa_private_key()
%%      Padding   = rsa_pkcs1_padding | rsa_pkcs1_oaep_padding 
%%      Encrypted = binary()
%%
%% Description: Public key encrypts PlainText.
%%--------------------------------------------------------------------
encrypt_public(PlainText, #'RSAPublicKey'{modulus=N,publicExponent=E}, 
	       Padding) ->
    crypto:rsa_public_encrypt(PlainText, [crypto:mpint(E),crypto:mpint(N)],
			      Padding);
encrypt_public(PlainText, #'RSAPrivateKey'{modulus=N,publicExponent=E},
	       Padding) ->
    crypto:rsa_public_encrypt(PlainText, [crypto:mpint(E),crypto:mpint(N)],
			      Padding).

encrypt_private(PlainText, #'RSAPrivateKey'{modulus = N,
					    publicExponent = E, 
					    privateExponent = D}, Padding) ->
    crypto:rsa_private_encrypt(PlainText, [crypto:mpint(E), 
					   crypto:mpint(N), 
					   crypto:mpint(D)], Padding).

%%--------------------------------------------------------------------
%% Function: decrypt(CipherText, Key) -> PlainText
%%
%%	ChipherText = binary()
%%	Key       = rsa_private_key() 
%%      Padding   = rsa_pkcs1_padding | rsa_pkcs1_oaep_padding 
%%      PlainText = binary()
%%
%% Description: Uses private key to decrypt public key encrypted data.
%%--------------------------------------------------------------------
decrypt_private(CipherText, 
		#'RSAPrivateKey'{modulus = N,publicExponent = E,
				 privateExponent = D}, 
		Padding) ->
    crypto:rsa_private_decrypt(CipherText, 
			       [crypto:mpint(E), crypto:mpint(N),
				crypto:mpint(D)], Padding).
decrypt_public(CipherText, #'RSAPublicKey'{modulus = N, publicExponent = E}, 
	       Padding) ->
    crypto:rsa_public_decrypt(CipherText,[crypto:mpint(E), crypto:mpint(N)], 
			      Padding);
decrypt_public(CipherText, #'RSAPrivateKey'{modulus = N, publicExponent = E}, 
	       Padding) ->
    crypto:rsa_public_decrypt(CipherText,[crypto:mpint(E), crypto:mpint(N)], 
			      Padding).

%%--------------------------------------------------------------------
%% Function: sign(PlainText, Key) ->
%%           sign(DigestType, PlainText, Key) -> Signature
%%
%%      DigestType = sha | md5
%%	PlainText  = binary()
%%	Key        = rsa_private_key() | dsa_private_key() 
%%      Signature  = binary()
%%
%% Description: Signs PlainText using Key.
%%--------------------------------------------------------------------
sign(PlainText, Digest) ->
    sign(sha, PlainText, Digest).

sign(DigestType, PlainText, #'RSAPrivateKey'{modulus = N, publicExponent = E,
				       privateExponent = D}) ->
    crypto:rsa_sign(DigestType, sized_binary(PlainText), [crypto:mpint(E),
						    crypto:mpint(N),
						    crypto:mpint(D)]);

sign(none, Hash, #'DSAPrivateKey'{p = P, q = Q, g = G, x = X}) ->
    crypto:dss_sign(none, Hash, 
		    [crypto:mpint(P), crypto:mpint(Q), 
		     crypto:mpint(G), crypto:mpint(X)]);
  
sign(sha, PlainText, #'DSAPrivateKey'{p = P, q = Q, g = G, x = X}) ->
    crypto:dss_sign(sized_binary(PlainText), 
		    [crypto:mpint(P), crypto:mpint(Q), 
		     crypto:mpint(G), crypto:mpint(X)]).
    
%%--------------------------------------------------------------------
%% Function: verify(DigestType, PlainText, Signature, Key) -> true | false
%%
%%      DigestType = sha | md5
%%      PlainText  = binary()
%%      Signature = binary()
%%      Key       = rsa_public_key() | dsa_public_key()
%%
%% Description: Verifies the signature <Signature>.
%%--------------------------------------------------------------------
verify(DigestType, PlainText, Signature, 
       #'RSAPublicKey'{modulus = Mod, publicExponent = Exp}, _) ->
    crypto:rsa_verify(DigestType,
		      sized_binary(PlainText), 
		      sized_binary(Signature), 
		      [crypto:mpint(Exp), crypto:mpint(Mod)]);

verify(none, Hash, Signature, Key,  #'Dss-Parms'{p = P, q = Q, g = G}) ->
    crypto:dss_verify(none, Hash, 
		      sized_binary(Signature), 
		      [crypto:mpint(P), crypto:mpint(Q), 
		       crypto:mpint(G), crypto:mpint(Key)]);
    
verify(sha, PlainText, Signature, Key,  #'Dss-Parms'{p = P, q = Q, g = G}) ->
    crypto:dss_verify(sized_binary(PlainText), 
		      sized_binary(Signature), 
		      [crypto:mpint(P), crypto:mpint(Q), 
		       crypto:mpint(G), crypto:mpint(Key)]).

    
%%--------------------------------------------------------------------
%% Function: gen_key(Type, Params) ->
%%	Type = diffie_hellman 
%%      Params = [P,G] | [Y, P, G]
%% Description: Generates keys.
%% -----------------------------------------------------------------
gen_key(diffie_hellman, [Y, P, G]) ->
    crypto:dh_generate_key(crypto:mpint(Y), [crypto:mpint(P), 
					     crypto:mpint(G)]);
gen_key(diffie_hellman, [P, G]) ->
    crypto:dh_generate_key([crypto:mpint(P), crypto:mpint(G)]).

%%% TODO: Support rsa, dss key_gen

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
sized_binary(Binary) when is_binary(Binary) ->
    Size = size(Binary),
    <<?UINT32(Size), Binary/binary>>;
sized_binary(List) ->
    sized_binary(list_to_binary(List)).

