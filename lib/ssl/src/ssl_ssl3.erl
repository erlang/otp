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
%% Purpose: Handles sslv3 encryption.
%%----------------------------------------------------------------------

-module(ssl_ssl3).

-include("ssl_cipher.hrl").
-include("ssl_debug.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl"). 			% MD5 and SHA

-export([master_secret/3, finished/3, certificate_verify/3,
	 mac_hash/6, setup_keys/7, 
	 suites/0]).
-compile(inline).

%%====================================================================
%% Internal application API
%%====================================================================

master_secret(PremasterSecret, ClientRandom, ServerRandom) ->
    ?DBG_HEX(PremasterSecret),
    ?DBG_HEX(ClientRandom),
    ?DBG_HEX(ServerRandom),
    %%  draft-ietf-tls-ssl-version3-00 - 6.2.2 
    %% key_block =
    %%   MD5(master_secret + SHA(`A' + master_secret +
    %%                           ServerHello.random +
    %%                           ClientHello.random)) +
    %%   MD5(master_secret + SHA(`BB' + master_secret +
    %%                           ServerHello.random +
    %%                           ClientHello.random)) +
    %%   MD5(master_secret + SHA(`CCC' + master_secret +
    %%                           ServerHello.random +
    %%                           ClientHello.random)) + [...];
    B = generate_keyblock(PremasterSecret, ClientRandom, ServerRandom, 48),
    ?DBG_HEX(B),
    B.

finished(Role, MasterSecret, {MD5Hash, SHAHash}) ->
   %%  draft-ietf-tls-ssl-version3-00 - 5.6.9 Finished
   %%     struct {
   %%      opaque md5_hash[16];
   %%      opaque sha_hash[20];
   %%  } Finished;
   %% 
   %%   md5_hash       MD5(master_secret + pad2 +
   %%                     MD5(handshake_messages + Sender +
   %%                         master_secret + pad1));
   %%  sha_hash        SHA(master_secret + pad2 +
   %%                      SHA(handshake_messages + Sender +
   %%                          master_secret + pad1));
    Sender = get_sender(Role),
    MD5 = handshake_hash(?MD5, MasterSecret, Sender, MD5Hash),
    SHA = handshake_hash(?SHA, MasterSecret, Sender, SHAHash),
    <<MD5/binary, SHA/binary>>.

certificate_verify(Algorithm, MasterSecret, {MD5Hash, SHAHash}) 
  when Algorithm == rsa; Algorithm == dhe_rsa ->
     %% md5_hash
     %%           MD5(master_secret + pad_2 +
     %%               MD5(handshake_messages + master_secret + pad_1));
     %% sha_hash
     %%           SHA(master_secret + pad_2 +
     %%               SHA(handshake_messages + master_secret + pad_1));

    MD5 = handshake_hash(?MD5, MasterSecret, undefined, MD5Hash),
    SHA = handshake_hash(?SHA, MasterSecret, undefined, SHAHash),
    <<MD5/binary, SHA/binary>>;

certificate_verify(dhe_dss, MasterSecret, {_, SHAHash}) ->
     %% sha_hash
     %%           SHA(master_secret + pad_2 +
     %%               SHA(handshake_messages + master_secret + pad_1));
    handshake_hash(?SHA, MasterSecret, undefined, SHAHash).

mac_hash(Method, Mac_write_secret, Seq_num, Type, Length, Fragment) ->
    %% draft-ietf-tls-ssl-version3-00 - 5.2.3.1 
    %% hash(MAC_write_secret + pad_2 +
    %%      hash(MAC_write_secret + pad_1 + seq_num +
    %%           SSLCompressed.type + SSLCompressed.length +
    %%           SSLCompressed.fragment));
    case Method of
        ?NULL -> ok;
        _ ->
	    ?DBG_HEX(Mac_write_secret),
	    ?DBG_HEX(hash(Method, Fragment)),
            ok
    end,
    Mac = mac_hash(Method, Mac_write_secret, 
		   [<<?UINT64(Seq_num), ?BYTE(Type), 
		     ?UINT16(Length)>>, Fragment]),
    ?DBG_HEX(Mac),
    Mac.

setup_keys(MasterSecret, ServerRandom, ClientRandom, HS, KML, _EKML, IVS) ->
    KeyBlock = generate_keyblock(MasterSecret, ServerRandom, ClientRandom,
				 2*(HS+KML+IVS)),
    %%  draft-ietf-tls-ssl-version3-00 - 6.2.2
    %% The key_block is partitioned as follows.
    %% client_write_MAC_secret[CipherSpec.hash_size]
    %% server_write_MAC_secret[CipherSpec.hash_size]
    %% client_write_key[CipherSpec.key_material]
    %% server_write_key[CipherSpec.key_material]
    %% client_write_IV[CipherSpec.IV_size] /* non-export ciphers */
    %% server_write_IV[CipherSpec.IV_size] /* non-export ciphers */
    <<ClientWriteMacSecret:HS/binary, ServerWriteMacSecret:HS/binary,
     ClientWriteKey:KML/binary, ServerWriteKey:KML/binary,
     ClientIV:IVS/binary, ServerIV:IVS/binary>> = KeyBlock,
    ?DBG_HEX(ClientWriteMacSecret),
    ?DBG_HEX(ServerWriteMacSecret),
    ?DBG_HEX(ClientWriteKey),
    ?DBG_HEX(ServerWriteKey),
    ?DBG_HEX(ClientIV),
    ?DBG_HEX(ServerIV),
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV}.

suites() ->
    [ 
      ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA,
      ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA,
      ?TLS_RSA_WITH_AES_256_CBC_SHA,
      ?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA,
      ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA,
      ?TLS_RSA_WITH_3DES_EDE_CBC_SHA,
      ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA,
      ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA,
      ?TLS_RSA_WITH_AES_128_CBC_SHA,
      %%?TLS_RSA_WITH_IDEA_CBC_SHA, 
      ?TLS_RSA_WITH_RC4_128_SHA,
      ?TLS_RSA_WITH_RC4_128_MD5,
      ?TLS_RSA_WITH_DES_CBC_SHA
     ].

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

hash(?MD5, Data) -> 
    crypto:md5(Data);
hash(?SHA, Data) -> 
    crypto:sha(Data).

hash_update(?MD5, Context, Data) -> 
    crypto:md5_update(Context, Data);
hash_update(?SHA, Context, Data) -> 
    crypto:sha_update(Context, Data).

hash_final(?MD5, Context) -> 
    crypto:md5_final(Context);
hash_final(?SHA, Context) -> 
    crypto:sha_final(Context).

%%pad_1(?NULL) ->
%%    "";
pad_1(?MD5) ->
    <<"666666666666666666666666666666666666666666666666">>;
pad_1(?SHA) ->
    <<"6666666666666666666666666666666666666666">>.

%%pad_2(?NULL) ->
%%    "";
pad_2(?MD5) ->
    <<"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
     "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\">>;
pad_2(?SHA) ->
    <<"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
     "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\">>.

mac_hash(?NULL, _Secret, _Data) ->
    <<>>;
mac_hash(Method, Secret, Data) ->
    InnerHash = hash(Method, [Secret, pad_1(Method), Data]),
    hash(Method, [Secret, pad_2(Method), InnerHash]).

handshake_hash(Method, HandshakeHash, Extra) ->
    HSH = hash_update(Method, HandshakeHash, Extra),
    hash_final(Method, HSH).

handshake_hash(Method, MasterSecret, undefined, HandshakeHash) ->
    InnerHash = 
	handshake_hash(Method, HandshakeHash, 
		       [MasterSecret, pad_1(Method)]),
    hash(Method, [MasterSecret, pad_2(Method), InnerHash]);
handshake_hash(Method, MasterSecret, Sender, HandshakeHash) ->
    InnerHash = 
	handshake_hash(Method, HandshakeHash, 
		       [Sender, MasterSecret, pad_1(Method)]),
    hash(Method, [MasterSecret, pad_2(Method), InnerHash]).

get_sender(client) -> "CLNT";
get_sender(server) -> "SRVR".

generate_keyblock(MasterSecret, ServerRandom, ClientRandom, WantedLength) ->
    gen(MasterSecret, [MasterSecret, ServerRandom, ClientRandom],
	WantedLength, 0, $A, 1, []).

gen(_Secret, _All, Wanted, Len, _C, _N, Acc) when Wanted =< Len ->
    <<Block:Wanted/binary, _/binary>> = list_to_binary(lists:reverse(Acc)),
    Block;
gen(Secret, All, Wanted, Len, C, N, Acc) ->
    Prefix = lists:duplicate(N, C),
    SHA = crypto:sha([Prefix, All]),
    MD5 = crypto:md5([Secret, SHA]),
    gen(Secret, All, Wanted, Len + 16, C+1, N+1, [MD5 | Acc]).
