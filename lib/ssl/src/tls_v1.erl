%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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
%% Purpose: Handles tls1 encryption.
%%----------------------------------------------------------------------

-module(tls_v1).

-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl").

-export([master_secret/4, finished/5, certificate_verify/3, mac_hash/7, hmac_hash/3,
	 setup_keys/8, suites/1, prf/5,
	 ecc_curves/1, ecc_curves/2, oid_to_enum/1, enum_to_oid/1, 
	 default_signature_algs/1, signature_algs/2,
         default_signature_schemes/1, signature_schemes/2,
         groups/1, groups/2, group_to_enum/1, enum_to_group/1, default_groups/1]).

-export([derive_secret/4, hkdf_expand_label/5, hkdf_extract/3, hkdf_expand/4,
         key_schedule/3, key_schedule/4, create_info/3,
         external_binder_key/2, resumption_binder_key/2,
         client_early_traffic_secret/3, early_exporter_master_secret/3,
         client_handshake_traffic_secret/3, server_handshake_traffic_secret/3,
         client_application_traffic_secret_0/3, server_application_traffic_secret_0/3,
         exporter_master_secret/3, resumption_master_secret/3,
         update_traffic_secret/2, calculate_traffic_keys/3,
         transcript_hash/2, finished_key/2, finished_verify_data/3, pre_shared_key/3]).

-type named_curve() :: sect571r1 | sect571k1 | secp521r1 | brainpoolP512r1 |
                       sect409k1 | sect409r1 | brainpoolP384r1 | secp384r1 |
                       sect283k1 | sect283r1 | brainpoolP256r1 | secp256k1 | secp256r1 |
                       sect239k1 | sect233k1 | sect233r1 | secp224k1 | secp224r1 |
                       sect193r1 | sect193r2 | secp192k1 | secp192r1 | sect163k1 |
                       sect163r1 | sect163r2 | secp160k1 | secp160r1 | secp160r2.
-type curves() :: [named_curve()].
-type group() :: secp256r1 | secp384r1 | secp521r1 | ffdhe2048 |
                 ffdhe3072 | ffdhe4096 | ffdhe6144 | ffdhe8192.
-type supported_groups() :: [group()].
-export_type([curves/0, named_curve/0, group/0, supported_groups/0]).

%%====================================================================
%% Internal application API
%%====================================================================

%% TLS 1.3 ---------------------------------------------------
-spec derive_secret(Secret::binary(), Label::binary(),
                    Messages::iodata(), Algo::ssl:hash()) -> Key::binary().
derive_secret(Secret, Label, Messages, Algo) ->
    Hash = crypto:hash(mac_algo(Algo), Messages),
    hkdf_expand_label(Secret, Label,
                      Hash, ssl_cipher:hash_size(Algo), Algo).

-spec hkdf_expand_label(Secret::binary(), Label0::binary(),
                        Context::binary(), Length::integer(),  
                        Algo::ssl:hash()) -> KeyingMaterial::binary().
hkdf_expand_label(Secret, Label0, Context, Length, Algo) ->
    HkdfLabel = create_info(Label0, Context, Length),
    hkdf_expand(Secret, HkdfLabel, Length, Algo).

%% Create info parameter for HKDF-Expand:
%% HKDF-Expand(PRK, info, L) -> OKM
create_info(Label0, Context0, Length) ->
    %% struct {
    %%     uint16 length = Length;
    %%     opaque label<7..255> = "tls13 " + Label;
    %%     opaque context<0..255> = Context;
    %% } HkdfLabel;
    Label1 = << <<"tls13 ">>/binary, Label0/binary>>,
    LabelLen = size(Label1),
    Label = <<?BYTE(LabelLen), Label1/binary>>,
    ContextLen = size(Context0),
    Context = <<?BYTE(ContextLen),Context0/binary>>,
    Content = <<Label/binary, Context/binary>>,
    <<?UINT16(Length), Content/binary>>.

-spec hkdf_extract(MacAlg::ssl:hash(), Salt::binary(),
                   KeyingMaterial::binary()) -> PseudoRandKey::binary().

hkdf_extract(MacAlg, Salt, KeyingMaterial) -> 
    hmac_hash(MacAlg, Salt, KeyingMaterial).


-spec hkdf_expand(PseudoRandKey::binary(), ContextInfo::binary(),
                  Length::integer(), Algo::ssl:hash()) -> KeyingMaterial::binary().
                     
hkdf_expand(PseudoRandKey, ContextInfo, Length, Algo) -> 
    Iterations = erlang:ceil(Length / ssl_cipher:hash_size(Algo)),
    hkdf_expand(Algo, PseudoRandKey, ContextInfo, Length, 1, Iterations, <<>>, <<>>).


-spec transcript_hash(Messages::iodata(),  Algo::ssl:hash()) -> Hash::binary().

transcript_hash(Messages, Algo) ->
     crypto:hash(mac_algo(Algo), Messages).
%% TLS 1.3 ---------------------------------------------------

%% TLS 1.0 -1.2  ---------------------------------------------------
-spec master_secret(integer(), binary(), binary(), binary()) -> binary().

master_secret(PrfAlgo, PreMasterSecret, ClientRandom, ServerRandom) ->
    %% RFC 2246 & 4346 && RFC 5246 - 8.1 %% master_secret = PRF(pre_master_secret,
    %%                                      "master secret", ClientHello.random +
    %%                                      ServerHello.random)[0..47];

    prf(PrfAlgo, PreMasterSecret, <<"master secret">>,
	[ClientRandom, ServerRandom], 48).
%% TLS 1.0 -1.2  ---------------------------------------------------

-spec finished(client | server, integer(), integer(), binary(), [binary()]) -> binary().
%% TLS 1.0 -1.1  ---------------------------------------------------
finished(Role, Version, PrfAlgo, MasterSecret, Handshake)
  when Version == 1; Version == 2; PrfAlgo == ?MD5SHA ->
    %% RFC 2246 & 4346 - 7.4.9. Finished
    %% struct {
    %%          opaque verify_data[12];
    %%      } Finished;
    %%
    %%      verify_data
    %%          PRF(master_secret, finished_label, MD5(handshake_messages) +
    %%          SHA-1(handshake_messages)) [0..11];
    MD5 = crypto:hash(md5, Handshake),
    SHA = crypto:hash(sha, Handshake),
    prf(?MD5SHA, MasterSecret, finished_label(Role), [MD5, SHA], 12);
%% TLS 1.0 -1.1  ---------------------------------------------------

%% TLS 1.2 ---------------------------------------------------
finished(Role, Version, PrfAlgo, MasterSecret, Handshake)
  when Version == 3 ->
    %% RFC 5246 - 7.4.9. Finished
    %% struct {
    %%          opaque verify_data[12];
    %%      } Finished;
    %%
    %%      verify_data
    %%          PRF(master_secret, finished_label, Hash(handshake_messages)) [0..11];
    Hash = crypto:hash(mac_algo(PrfAlgo), Handshake),
    prf(PrfAlgo, MasterSecret, finished_label(Role), Hash, 12).
%% TLS 1.2 ---------------------------------------------------

%% TODO 1.3 finished

-spec certificate_verify(md5sha | sha, integer(), [binary()]) -> binary().

%% TLS 1.0 -1.1  ---------------------------------------------------
certificate_verify(md5sha, _Version, Handshake) ->
    MD5 = crypto:hash(md5, Handshake),
    SHA = crypto:hash(sha, Handshake),
    <<MD5/binary, SHA/binary>>;
%% TLS 1.0 -1.1  ---------------------------------------------------

%% TLS 1.2 ---------------------------------------------------
certificate_verify(HashAlgo, _Version, Handshake) ->
    crypto:hash(HashAlgo, Handshake).
%% TLS 1.2 ---------------------------------------------------

-spec setup_keys(integer(), integer(), binary(), binary(), binary(), integer(),
		 integer(), integer()) -> {binary(), binary(), binary(),
					  binary(), binary(), binary()}.
%% TLS v1.0  ---------------------------------------------------
setup_keys(Version, _PrfAlgo, MasterSecret, ServerRandom, ClientRandom, HashSize,
	   KeyMatLen, IVSize)
  when Version == 1 ->
    %% RFC 2246 - 6.3. Key calculation
    %% key_block = PRF(SecurityParameters.master_secret,
    %%                      "key expansion",
    %%                      SecurityParameters.server_random +
    %%                      SecurityParameters.client_random);
    %% Then the key_block is partitioned as follows:
    %%  client_write_MAC_secret[SecurityParameters.hash_size]
    %%  server_write_MAC_secret[SecurityParameters.hash_size]
    %%  client_write_key[SecurityParameters.key_material_length]
    %%  server_write_key[SecurityParameters.key_material_length]
    %%  client_write_IV[SecurityParameters.IV_size]
    %%  server_write_IV[SecurityParameters.IV_size]
    WantedLength = 2 * (HashSize + KeyMatLen + IVSize),
    KeyBlock = prf(?MD5SHA, MasterSecret, "key expansion",
		   [ServerRandom, ClientRandom], WantedLength),
    <<ClientWriteMacSecret:HashSize/binary,
     ServerWriteMacSecret:HashSize/binary,
     ClientWriteKey:KeyMatLen/binary, ServerWriteKey:KeyMatLen/binary,
     ClientIV:IVSize/binary, ServerIV:IVSize/binary>> = KeyBlock,
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV};
%% TLS v1.0  ---------------------------------------------------

%% TLS v1.1 ---------------------------------------------------
setup_keys(Version, _PrfAlgo, MasterSecret, ServerRandom, ClientRandom, HashSize,
	   KeyMatLen, IVSize)
  when Version == 2 ->
    %% RFC 4346 - 6.3. Key calculation
    %% key_block = PRF(SecurityParameters.master_secret,
    %%                      "key expansion",
    %%                      SecurityParameters.server_random +
    %%                      SecurityParameters.client_random);
    %% Then the key_block is partitioned as follows:
    %%  client_write_MAC_secret[SecurityParameters.hash_size]
    %%  server_write_MAC_secret[SecurityParameters.hash_size]
    %%  client_write_key[SecurityParameters.key_material_length]
    %%  server_write_key[SecurityParameters.key_material_length]
    %%
    %% RFC 4346 is incomplete, the client and server IVs have to
    %% be generated just like for TLS 1.0
    WantedLength = 2 * (HashSize + KeyMatLen + IVSize),
    KeyBlock = prf(?MD5SHA, MasterSecret, "key expansion",
		   [ServerRandom, ClientRandom], WantedLength),
    <<ClientWriteMacSecret:HashSize/binary,
     ServerWriteMacSecret:HashSize/binary,
     ClientWriteKey:KeyMatLen/binary, ServerWriteKey:KeyMatLen/binary,
     ClientIV:IVSize/binary, ServerIV:IVSize/binary>> = KeyBlock,
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV};
%% TLS v1.1 ---------------------------------------------------

%% TLS v1.2  ---------------------------------------------------
setup_keys(Version, PrfAlgo, MasterSecret, ServerRandom, ClientRandom, HashSize,
	   KeyMatLen, IVSize)
  when Version == 3; Version == 4 ->
    %% RFC 5246 - 6.3. Key calculation
    %% key_block = PRF(SecurityParameters.master_secret,
    %%                      "key expansion",
    %%                      SecurityParameters.server_random +
    %%                      SecurityParameters.client_random);
    %% Then the key_block is partitioned as follows:
    %%  client_write_MAC_secret[SecurityParameters.hash_size]
    %%  server_write_MAC_secret[SecurityParameters.hash_size]
    %%  client_write_key[SecurityParameters.key_material_length]
    %%  server_write_key[SecurityParameters.key_material_length]
    %%  client_write_IV[SecurityParameters.fixed_iv_length]
    %%  server_write_IV[SecurityParameters.fixed_iv_length]
    WantedLength = 2 * (HashSize + KeyMatLen + IVSize),
    KeyBlock = prf(PrfAlgo, MasterSecret, "key expansion",
		   [ServerRandom, ClientRandom], WantedLength),
    <<ClientWriteMacSecret:HashSize/binary,
     ServerWriteMacSecret:HashSize/binary,
     ClientWriteKey:KeyMatLen/binary, ServerWriteKey:KeyMatLen/binary,
     ClientIV:IVSize/binary, ServerIV:IVSize/binary>> = KeyBlock,
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV}.
%% TLS v1.2  ---------------------------------------------------

%% TLS v1.3  ---------------------------------------------------
%% RFC 8446  -  7.1.  Key Schedule
%%
%%             0
%%             |
%%             v
%%   PSK ->  HKDF-Extract = Early Secret
%%             |
%%             +-----> Derive-Secret(., "ext binder" | "res binder", "")
%%             |                     = binder_key
%%             |
%%             +-----> Derive-Secret(., "c e traffic", ClientHello)
%%             |                     = client_early_traffic_secret
%%             |
%%             +-----> Derive-Secret(., "e exp master", ClientHello)
%%             |                     = early_exporter_master_secret
%%             v
%%       Derive-Secret(., "derived", "")
%%             |
%%             v
%%   (EC)DHE -> HKDF-Extract = Handshake Secret
%%             |
%%             +-----> Derive-Secret(., "c hs traffic",
%%             |                     ClientHello...ServerHello)
%%             |                     = client_handshake_traffic_secret
%%             |
%%             +-----> Derive-Secret(., "s hs traffic",
%%             |                     ClientHello...ServerHello)
%%             |                     = server_handshake_traffic_secret
%%             v
%%       Derive-Secret(., "derived", "")
%%             |
%%             v
%%   0 -> HKDF-Extract = Master Secret
%%             |
%%             +-----> Derive-Secret(., "c ap traffic",
%%             |                     ClientHello...server Finished)
%%             |                     = client_application_traffic_secret_0
%%             |
%%             +-----> Derive-Secret(., "s ap traffic",
%%             |                     ClientHello...server Finished)
%%             |                     = server_application_traffic_secret_0
%%             |
%%             +-----> Derive-Secret(., "exp master",
%%             |                     ClientHello...server Finished)
%%             |                     = exporter_master_secret
%%             |
%%             +-----> Derive-Secret(., "res master",
%%                                   ClientHello...client Finished)
%%                                   = resumption_master_secret
-spec key_schedule(early_secret | handshake_secret | master_secret,
                   atom(), {psk | early_secret | handshake_secret, binary()}) ->
                          {early_secret | handshake_secret | master_secret, binary()}.

key_schedule(early_secret, Algo, {psk, PSK}) ->
    Len = ssl_cipher:hash_size(Algo),
    Salt = binary:copy(<<?BYTE(0)>>, Len),
    {early_secret, hkdf_extract(Algo, Salt, PSK)};
key_schedule(master_secret, Algo, {handshake_secret, Secret}) ->
    Len = ssl_cipher:hash_size(Algo),
    IKM = binary:copy(<<?BYTE(0)>>, Len),
    Salt = derive_secret(Secret, <<"derived">>, <<>>, Algo),
    {master_secret, hkdf_extract(Algo, Salt, IKM)}.
%%
key_schedule(handshake_secret, Algo, IKM, {early_secret, Secret}) ->
    Salt = derive_secret(Secret, <<"derived">>, <<>>, Algo),
    {handshake_secret, hkdf_extract(Algo, Salt, IKM)}.

-spec external_binder_key(atom(), {early_secret, binary()}) -> binary().
external_binder_key(Algo, {early_secret, Secret}) ->
    derive_secret(Secret, <<"ext binder">>, <<>>, Algo).

-spec resumption_binder_key(atom(), {early_secret, binary()}) -> binary().
resumption_binder_key(Algo, {early_secret, Secret}) ->
    derive_secret(Secret, <<"res binder">>, <<>>, Algo).

-spec client_early_traffic_secret(atom(), {early_secret, binary()}, iodata()) -> binary().
%% M = ClientHello
client_early_traffic_secret(Algo, {early_secret, Secret}, M) ->
    derive_secret(Secret, <<"c e traffic">>, M, Algo).

-spec early_exporter_master_secret(atom(), {early_secret, binary()}, iodata()) -> binary().
%% M = ClientHello
early_exporter_master_secret(Algo, {early_secret, Secret}, M) ->
    derive_secret(Secret, <<"e exp master">>, M, Algo).

-spec client_handshake_traffic_secret(atom(), {handshake_secret, binary()}, iodata()) -> binary().
%% M = ClientHello...ServerHello
client_handshake_traffic_secret(Algo, {handshake_secret, Secret}, M) ->
    derive_secret(Secret, <<"c hs traffic">>, M, Algo).

-spec server_handshake_traffic_secret(atom(), {handshake_secret, binary()}, iodata()) -> binary().
%% M = ClientHello...ServerHello
server_handshake_traffic_secret(Algo, {handshake_secret, Secret}, M) ->
    derive_secret(Secret, <<"s hs traffic">>, M, Algo).

-spec client_application_traffic_secret_0(atom(), {master_secret, binary()}, iodata()) -> binary().
%% M = ClientHello...server Finished
client_application_traffic_secret_0(Algo, {master_secret, Secret}, M) ->
    derive_secret(Secret, <<"c ap traffic">>, M, Algo).

-spec server_application_traffic_secret_0(atom(), {master_secret, binary()}, iodata()) -> binary().
%% M = ClientHello...server Finished
server_application_traffic_secret_0(Algo, {master_secret, Secret}, M) ->
    derive_secret(Secret, <<"s ap traffic">>, M, Algo).

-spec exporter_master_secret(atom(), {master_secret, binary()}, iodata()) -> binary().
%% M = ClientHello...server Finished
exporter_master_secret(Algo, {master_secret, Secret}, M) ->
    derive_secret(Secret, <<"exp master">>, M, Algo).

-spec resumption_master_secret(atom(), {master_secret, binary()}, iodata()) -> binary().
%% M = ClientHello...client Finished
resumption_master_secret(Algo, {master_secret, Secret}, M) ->
    derive_secret(Secret, <<"res master">>, M, Algo).

-spec finished_key(binary(), atom()) -> binary().
finished_key(BaseKey, Algo) ->
    %% finished_key =
    %%        HKDF-Expand-Label(BaseKey, "finished", "", Hash.length)
    ssl_cipher:hash_size(Algo),
    hkdf_expand_label(BaseKey, <<"finished">>, <<>>, ssl_cipher:hash_size(Algo), Algo).

-spec finished_verify_data(binary(), atom(), iodata()) -> binary().
finished_verify_data(FinishedKey, HKDFAlgo, Messages) ->
    %% The verify_data value is computed as follows:
    %%
    %%       verify_data =
    %%           HMAC(finished_key,
    %%                Transcript-Hash(Handshake Context,
    %%                                Certificate*, CertificateVerify*))
    Context = lists:reverse(Messages),
    THash = tls_v1:transcript_hash(Context, HKDFAlgo),
    tls_v1:hmac_hash(HKDFAlgo, FinishedKey, THash).

-spec pre_shared_key(binary(), binary(), atom()) -> binary().
pre_shared_key(RMS, Nonce, Algo) ->
    %% The PSK associated with the ticket is computed as:
    %%
    %%     HKDF-Expand-Label(resumption_master_secret,
    %%                      "resumption", ticket_nonce, Hash.length)
    ssl_cipher:hash_size(Algo),
    hkdf_expand_label(RMS, <<"resumption">>, Nonce, ssl_cipher:hash_size(Algo), Algo).

%% The next-generation application_traffic_secret is computed as:
%%
%%        application_traffic_secret_N+1 =
%%            HKDF-Expand-Label(application_traffic_secret_N,
%%                              "traffic upd", "", Hash.length)
-spec update_traffic_secret(atom(), binary()) -> binary().
update_traffic_secret(Algo, Secret) ->
    hkdf_expand_label(Secret, <<"traffic upd">>, <<>>, ssl_cipher:hash_size(Algo), Algo).

%% The traffic keying material is generated from the following input
%%    values:
%%
%%    -  A secret value
%%
%%    -  A purpose value indicating the specific value being generated
%%
%%    -  The length of the key being generated
%%
%%    The traffic keying material is generated from an input traffic secret
%%    value using:
%%
%%    [sender]_write_key = HKDF-Expand-Label(Secret, "key", "", key_length)
%%    [sender]_write_iv  = HKDF-Expand-Label(Secret, "iv", "", iv_length)
-spec calculate_traffic_keys(atom(), atom(), binary()) -> {binary(), binary()}.
calculate_traffic_keys(HKDFAlgo, Cipher, Secret) ->
    Key = hkdf_expand_label(Secret, <<"key">>, <<>>, ssl_cipher:key_material(Cipher), HKDFAlgo),
    %% TODO: remove hard coded IV size
    IV = hkdf_expand_label(Secret, <<"iv">>, <<>>, 12, HKDFAlgo),
    {Key, IV}.

%% TLS v1.3  ---------------------------------------------------

%% TLS 1.0 -1.2  ---------------------------------------------------
-spec mac_hash(integer() | atom(), binary(), integer(), integer(), tls_record:tls_version(),
	       integer(), binary()) -> binary().

mac_hash(Method, Mac_write_secret, Seq_num, Type, {Major, Minor},
	 Length, Fragment) ->
    %% RFC 2246 & 4346 - 6.2.3.1.
    %% HMAC_hash(MAC_write_secret, seq_num + TLSCompressed.type +
    %%              TLSCompressed.version + TLSCompressed.length +
    %%              TLSCompressed.fragment));
    Mac = hmac_hash(Method, Mac_write_secret,
		    [<<?UINT64(Seq_num), ?BYTE(Type),
		      ?BYTE(Major), ?BYTE(Minor), ?UINT16(Length)>>,
		     Fragment]),
    Mac.
%% TLS 1.0 -1.2  ---------------------------------------------------

%% TODO 1.3 same as above?

-spec suites(1|2|3|4|'TLS_v1.3') -> [ssl_cipher_format:cipher_suite()].

suites(Minor) when Minor == 1; Minor == 2 ->
    [
      ?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA,
      ?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA,
      ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA,
      ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA,
      ?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA,
      ?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA,

      ?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA,
      ?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA,
      ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA,
      ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA,
      ?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA,
      ?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA
    ];
suites(3) ->
    [?TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384,
     ?TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384,
     ?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384,
     ?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384,
     ?TLS_ECDH_ECDSA_WITH_AES_256_GCM_SHA384,
     ?TLS_ECDH_RSA_WITH_AES_256_GCM_SHA384,
     ?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA384,
     ?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA384,

     ?TLS_DHE_RSA_WITH_AES_256_GCM_SHA384,
     ?TLS_DHE_DSS_WITH_AES_256_GCM_SHA384,
     ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA256,
     ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA256,

     ?TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256,
     ?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256,
     ?TLS_ECDH_ECDSA_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDH_RSA_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA256,
     ?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256,

     ?TLS_DHE_RSA_WITH_AES_128_GCM_SHA256,
     ?TLS_DHE_DSS_WITH_AES_128_GCM_SHA256,
     ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA256,
     ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA256

     %% not supported
     %% ?TLS_DH_RSA_WITH_AES_256_GCM_SHA384,
     %% ?TLS_DH_DSS_WITH_AES_256_GCM_SHA384,
     %% ?TLS_DH_RSA_WITH_AES_128_GCM_SHA256,
     %% ?TLS_DH_DSS_WITH_AES_128_GCM_SHA256
    ] ++ suites(2);

suites(4) ->
    [?TLS_AES_256_GCM_SHA384,
     ?TLS_AES_128_GCM_SHA256,
     ?TLS_CHACHA20_POLY1305_SHA256,
     ?TLS_AES_128_CCM_SHA256
     %% Not supported
     %% ?TLS_AES_128_CCM_8_SHA256
    ] ++ suites(3);

suites('TLS_v1.3') ->
    [?TLS_AES_256_GCM_SHA384,
     ?TLS_AES_128_GCM_SHA256,
     ?TLS_CHACHA20_POLY1305_SHA256,
     ?TLS_AES_128_CCM_SHA256
     %% Not supported
     %% ?TLS_AES_128_CCM_8_SHA256
    ].


signature_algs({3, 4}, HashSigns) ->
    signature_algs({3, 3}, HashSigns);
signature_algs({3, 3}, HashSigns) ->
    CryptoSupports =  crypto:supports(),
    Hashes = proplists:get_value(hashs, CryptoSupports),
    PubKeys = proplists:get_value(public_keys, CryptoSupports),
    Supported = lists:foldl(fun({Hash, dsa = Sign} = Alg, Acc) ->
				    case proplists:get_bool(dss, PubKeys) 
					andalso proplists:get_bool(Hash, Hashes)
					andalso is_pair(Hash, Sign, Hashes)
				    of
					true ->
					    [Alg | Acc];
					false ->
					    Acc
				    end;
			       ({Hash, Sign} = Alg, Acc) -> 
				    case proplists:get_bool(Sign, PubKeys) 
					andalso proplists:get_bool(Hash, Hashes) 
					andalso is_pair(Hash, Sign, Hashes)
				    of
					true ->
					    [Alg | Acc];
					false ->
					    Acc
				    end
			    end, [], HashSigns),
    lists:reverse(Supported).

default_signature_algs({3, 4} = Version) ->
    %% TLS 1.3 servers shall be prepared to process TLS 1.2 ClientHellos
    %% containing legacy hash-sign tuples.
    default_signature_schemes(Version) ++ default_signature_algs({3,3});
default_signature_algs({3, 3} = Version) ->
    Default = [%% SHA2
	       {sha512, ecdsa},
	       {sha512, rsa},
	       {sha384, ecdsa},
	       {sha384, rsa},
	       {sha256, ecdsa},
	       {sha256, rsa},
	       {sha224, ecdsa},
	       {sha224, rsa},
	       %% SHA
	       {sha, ecdsa},
	       {sha, rsa},
	       {sha, dsa}],
    signature_algs(Version, Default);
default_signature_algs(_) ->
    undefined.


signature_schemes(Version, SignatureSchemes) when is_tuple(Version)
                                                  andalso Version >= {3, 3} ->
    CryptoSupports =  crypto:supports(),
    Hashes = proplists:get_value(hashs, CryptoSupports),
    PubKeys = proplists:get_value(public_keys, CryptoSupports),
    Curves = proplists:get_value(curves, CryptoSupports),
    RSAPSSSupported = lists:member(rsa_pkcs1_pss_padding,
                                   proplists:get_value(rsa_opts, CryptoSupports)),
    Fun = fun (Scheme, Acc) when is_atom(Scheme) ->
                  {Hash0, Sign0, Curve} =
                      ssl_cipher:scheme_to_components(Scheme),
                  Sign = case Sign0 of
                             rsa_pkcs1 ->
                                 rsa;
                             rsa_pss_rsae when RSAPSSSupported ->
                                 rsa;
                             rsa_pss_pss when RSAPSSSupported ->
                                 rsa;
                             S -> S
                         end,
                  Hash = case Hash0 of
                             sha1 ->
                                 sha;
                             H -> H
                         end,
                  case proplists:get_bool(Sign, PubKeys)
                      andalso proplists:get_bool(Hash, Hashes)
                      andalso (Curve =:= undefined orelse
                               proplists:get_bool(Curve, Curves))
                      andalso is_pair(Hash, Sign, Hashes)
                  of
                      true ->
                          [Scheme | Acc];
                      false ->
                          Acc
                  end;
              %% Special clause for filtering out the legacy hash-sign tuples.
              ({Hash, dsa = Sign} = Alg, Acc) ->
                  case proplists:get_bool(dss, PubKeys)
                      andalso proplists:get_bool(Hash, Hashes)
                      andalso is_pair(Hash, Sign, Hashes)
                  of
                      true ->
                          [Alg | Acc];
                      false ->
                          Acc
                  end;
              ({Hash, Sign} = Alg, Acc) ->
                  case proplists:get_bool(Sign, PubKeys)
                      andalso proplists:get_bool(Hash, Hashes)
                      andalso is_pair(Hash, Sign, Hashes)
                  of
                      true ->
                          [Alg | Acc];
                      false ->
                          Acc
                  end
          end,
    Supported = lists:foldl(Fun, [], SignatureSchemes),
    lists:reverse(Supported);
signature_schemes(_, _) ->
    [].

default_signature_schemes(Version) ->
    Default = [
               ecdsa_secp521r1_sha512,
               ecdsa_secp384r1_sha384,
               ecdsa_secp256r1_sha256,
               rsa_pss_pss_sha512,
               rsa_pss_pss_sha384,
               rsa_pss_pss_sha256,
               rsa_pss_rsae_sha512,
               rsa_pss_rsae_sha384,
               rsa_pss_rsae_sha256,
               %% ed25519,
               %% ed448,

               %% These values refer solely to signatures
               %% which appear in certificates (see Section 4.4.2.2) and are not
               %% defined for use in signed TLS handshake messages, although they
               %% MAY appear in "signature_algorithms" and
               %% "signature_algorithms_cert" for backward compatibility with
               %% TLS 1.2.
               rsa_pkcs1_sha512,
               rsa_pkcs1_sha384,
               rsa_pkcs1_sha256,
               ecdsa_sha1,
               rsa_pkcs1_sha1
              ],
    signature_schemes(Version, Default).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
hkdf_expand(Algo, PseudoRandKey, ContextInfo, Length, N, N, Prev, Acc) ->
    Keyingmaterial = hmac_hash(Algo, PseudoRandKey, <<Prev/binary, ContextInfo/binary, ?BYTE(N)>>),
    binary:part(<<Acc/binary, Keyingmaterial/binary>>, {0, Length});
hkdf_expand(Algo, PseudoRandKey, ContextInfo, Length, M, N, Prev, Acc) ->
    Keyingmaterial = hmac_hash(Algo, PseudoRandKey, <<Prev/binary, ContextInfo/binary, ?BYTE(M)>>),
    hkdf_expand(Algo, PseudoRandKey, ContextInfo, Length, M + 1, N, Keyingmaterial, <<Acc/binary, Keyingmaterial/binary>>).

%%%% HMAC and the Pseudorandom Functions RFC 2246 & 4346 - 5.%%%%
hmac_hash(?NULL, _, _) ->
    <<>>;
hmac_hash(Alg, Key, Value) ->
    crypto:hmac(mac_algo(Alg), Key, Value).

mac_algo(Alg) when is_atom(Alg) -> 
    Alg;
mac_algo(?MD5)    -> md5;
mac_algo(?SHA)    -> sha;
mac_algo(?SHA256) -> sha256;
mac_algo(?SHA384) -> sha384;
mac_algo(?SHA512) -> sha512.

% First, we define a data expansion function, P_hash(secret, data) that
% uses a single hash function to expand a secret and seed into an
% arbitrary quantity of output:
%% P_hash(secret, seed) = HMAC_hash(secret, A(1) + seed) +
%%                        HMAC_hash(secret, A(2) + seed) +
%%                        HMAC_hash(secret, A(3) + seed) + ...

p_hash(Secret, Seed, WantedLength, Method) ->
    p_hash(Secret, Seed, WantedLength, Method, 0, []).

p_hash(_Secret, _Seed, WantedLength, _Method, _N, [])
  when WantedLength =< 0 ->
    [];
p_hash(_Secret, _Seed, WantedLength, _Method, _N, [Last | Acc])
  when WantedLength =< 0 ->
    Keep = byte_size(Last) + WantedLength,
    <<B:Keep/binary, _/binary>> = Last,
    list_to_binary(lists:reverse(Acc, [B]));
p_hash(Secret, Seed, WantedLength, Method, N, Acc) ->
    N1 = N+1,
    Bin = hmac_hash(Method, Secret, [a(N1, Secret, Seed, Method), Seed]),
    p_hash(Secret, Seed, WantedLength - byte_size(Bin), Method, N1, [Bin|Acc]).


%% ... Where  A(0) = seed
%%            A(i) = HMAC_hash(secret, A(i-1))
%% a(0, _Secret, Seed, _Method) ->
%%     Seed.
%% a(N, Secret, Seed, Method) ->
%%     hmac_hash(Method, Secret, a(N-1, Secret, Seed, Method)).
a(0, _Secret, Seed, _Method) ->
    Seed;
a(N, Secret, Seed0, Method) ->
    Seed = hmac_hash(Method, Secret, Seed0),
    a(N-1, Secret, Seed, Method).

split_secret(BinSecret) ->
    %% L_S = length in bytes of secret;
    %% L_S1 = L_S2 = ceil(L_S / 2);
    %% The secret is partitioned into two halves (with the possibility of
    %% one shared byte) as described above, S1 taking the first L_S1 bytes,
    %% and S2 the last L_S2 bytes.
    Length = byte_size(BinSecret),
    Div = Length div 2,
    EvenLength = Length - Div,
    <<Secret1:EvenLength/binary, _/binary>> = BinSecret,
    <<_:Div/binary, Secret2:EvenLength/binary>> = BinSecret,
    {Secret1, Secret2}.

prf(?MD5SHA, Secret, Label, Seed, WantedLength) ->
    %% PRF(secret, label, seed) = P_MD5(S1, label + seed) XOR
    %%                            P_SHA-1(S2, label + seed);
    {S1, S2} = split_secret(Secret),
    LS = list_to_binary([Label, Seed]),
    crypto:exor(p_hash(S1, LS, WantedLength, ?MD5),
		p_hash(S2, LS, WantedLength, ?SHA));

prf(MAC, Secret, Label, Seed, WantedLength) ->
    %% PRF(secret, label, seed) = P_SHA256(secret, label + seed);
    LS = list_to_binary([Label, Seed]),
    p_hash(Secret, LS, WantedLength, MAC).

%%%% Misc help functions %%%%

finished_label(client) ->
    <<"client finished">>;
finished_label(server) ->
    <<"server finished">>.

is_pair(sha, dsa, _) ->
    true;
is_pair(_, dsa, _) ->
    false;
is_pair(Hash, ecdsa, Hashs) ->
    AtLeastSha = Hashs -- [md2,md4,md5],
    lists:member(Hash, AtLeastSha);
is_pair(Hash, rsa, Hashs) ->
    AtLeastMd5 = Hashs -- [md2,md4],
    lists:member(Hash, AtLeastMd5).

%% list ECC curves in preferred order
-spec ecc_curves(1..3 | all) -> [named_curve()].
ecc_curves(all) ->
    [sect571r1,sect571k1,secp521r1,brainpoolP512r1,
     sect409k1,sect409r1,brainpoolP384r1,secp384r1,
     sect283k1,sect283r1,brainpoolP256r1,secp256k1,secp256r1,
     sect239k1,sect233k1,sect233r1,secp224k1,secp224r1,
     sect193r1,sect193r2,secp192k1,secp192r1,sect163k1,
     sect163r1,sect163r2,secp160k1,secp160r1,secp160r2];

ecc_curves(Minor) ->
    TLSCurves = ecc_curves(all),
    ecc_curves(Minor, TLSCurves).

-spec ecc_curves(1..3, [named_curve()]) -> [named_curve()].
ecc_curves(_Minor, TLSCurves) ->
    CryptoCurves = crypto:ec_curves(),
    lists:foldr(fun(Curve, Curves) ->
			case proplists:get_bool(Curve, CryptoCurves) of
			    true ->  [pubkey_cert_records:namedCurves(Curve)|Curves];
			    false -> Curves
			end
		end, [], TLSCurves).

-spec groups(4 | all | default) -> [group()].
groups(all) ->
    [x25519,
     x448,
     secp256r1,
     secp384r1,
     secp521r1,
     ffdhe2048,
     ffdhe3072,
     ffdhe4096,
     ffdhe6144,
     ffdhe8192];
groups(default) ->
    [x25519,
     x448,
     secp256r1,
     secp384r1];
groups(Minor) ->
    TLSGroups = groups(all),
    groups(Minor, TLSGroups).
%%
-spec groups(4, [group()]) -> [group()].
groups(_Minor, TLSGroups) ->
    CryptoGroups = supported_groups(),
    lists:filter(fun(Group) -> proplists:get_bool(Group, CryptoGroups) end, TLSGroups).

default_groups(Minor) ->
    TLSGroups = groups(default),
    groups(Minor, TLSGroups).

supported_groups() ->
    %% TODO: Add new function to crypto?
    proplists:get_value(curves,  crypto:supports()) ++
        [ffdhe2048,ffdhe3072,ffdhe4096,ffdhe6144,ffdhe8192].

group_to_enum(secp256r1) -> 23;
group_to_enum(secp384r1) -> 24;
group_to_enum(secp521r1) -> 25;
group_to_enum(x25519)    -> 29;
group_to_enum(x448)      -> 30;
group_to_enum(ffdhe2048) -> 256;
group_to_enum(ffdhe3072) -> 257;
group_to_enum(ffdhe4096) -> 258;
group_to_enum(ffdhe6144) -> 259;
group_to_enum(ffdhe8192) -> 260.

enum_to_group(23) -> secp256r1;
enum_to_group(24) -> secp384r1;
enum_to_group(25) -> secp521r1;
enum_to_group(29) -> x25519;
enum_to_group(30) -> x448;
enum_to_group(256) -> ffdhe2048;
enum_to_group(257) -> ffdhe3072;
enum_to_group(258) -> ffdhe4096;
enum_to_group(259) -> ffdhe6144;
enum_to_group(260) -> ffdhe8192;
enum_to_group(_) -> undefined.

%% ECC curves from draft-ietf-tls-ecc-12.txt (Oct. 17, 2005)
oid_to_enum(?sect163k1) -> 1;
oid_to_enum(?sect163r1) -> 2;
oid_to_enum(?sect163r2) -> 3;
oid_to_enum(?sect193r1) -> 4;
oid_to_enum(?sect193r2) -> 5;
oid_to_enum(?sect233k1) -> 6;
oid_to_enum(?sect233r1) -> 7;
oid_to_enum(?sect239k1) -> 8;
oid_to_enum(?sect283k1) -> 9;
oid_to_enum(?sect283r1) -> 10;
oid_to_enum(?sect409k1) -> 11;
oid_to_enum(?sect409r1) -> 12;
oid_to_enum(?sect571k1) -> 13;
oid_to_enum(?sect571r1) -> 14;
oid_to_enum(?secp160k1) -> 15;
oid_to_enum(?secp160r1) -> 16;
oid_to_enum(?secp160r2) -> 17;
oid_to_enum(?secp192k1) -> 18;
oid_to_enum(?secp192r1) -> 19;
oid_to_enum(?secp224k1) -> 20;
oid_to_enum(?secp224r1) -> 21;
oid_to_enum(?secp256k1) -> 22;
oid_to_enum(?secp256r1) -> 23;
oid_to_enum(?secp384r1) -> 24;
oid_to_enum(?secp521r1) -> 25;
oid_to_enum(?brainpoolP256r1) -> 26;
oid_to_enum(?brainpoolP384r1) -> 27;
oid_to_enum(?brainpoolP512r1) -> 28.

enum_to_oid(1) -> ?sect163k1;
enum_to_oid(2) -> ?sect163r1;
enum_to_oid(3) -> ?sect163r2;
enum_to_oid(4) -> ?sect193r1;
enum_to_oid(5) -> ?sect193r2;
enum_to_oid(6) -> ?sect233k1;
enum_to_oid(7) -> ?sect233r1;
enum_to_oid(8) -> ?sect239k1;
enum_to_oid(9) -> ?sect283k1;
enum_to_oid(10) -> ?sect283r1;
enum_to_oid(11) -> ?sect409k1;
enum_to_oid(12) -> ?sect409r1;
enum_to_oid(13) -> ?sect571k1;
enum_to_oid(14) -> ?sect571r1;
enum_to_oid(15) -> ?secp160k1;
enum_to_oid(16) -> ?secp160r1;
enum_to_oid(17) -> ?secp160r2;
enum_to_oid(18) -> ?secp192k1;
enum_to_oid(19) -> ?secp192r1;
enum_to_oid(20) -> ?secp224k1;
enum_to_oid(21) -> ?secp224r1;
enum_to_oid(22) -> ?secp256k1;
enum_to_oid(23) -> ?secp256r1;
enum_to_oid(24) -> ?secp384r1;
enum_to_oid(25) -> ?secp521r1;
enum_to_oid(26) -> ?brainpoolP256r1;
enum_to_oid(27) -> ?brainpoolP384r1;
enum_to_oid(28) -> ?brainpoolP512r1;
enum_to_oid(_) ->
    undefined.
