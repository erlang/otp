%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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
-include("tls_handshake_1_3.hrl").

-export([master_secret/4,
         finished/5,
         certificate_verify/2,
         mac_hash/7,
         hmac_hash/3,
         setup_keys/8,
         suites/1,
         exclusive_suites/1,
         exclusive_anonymous_suites/1,
         psk_suites/1,
         psk_exclusive/1,
         psk_suites_anon/1,
         srp_suites/1,
         srp_suites_anon/1,
         srp_exclusive/1,
         rc4_suites/1,
         rc4_exclusive/1,
         des_suites/1,
         des_exclusive/1,
         rsa_suites/1,
         rsa_exclusive/1,
         prf/5,
         ecc_curves/1,
         ec_curves/2,
         oid_to_enum/1,
         enum_to_oid/1,
         default_signature_algs/1,
         legacy_signature_algs_pre_13/0,
         signature_algs/2,
         signature_schemes/2,
         rsa_schemes/0,
         groups/0,
         groups/1,
         group_to_enum/1,
         enum_to_group/1,
         default_groups/0]).

-export([derive_secret/4,
         hkdf_expand_label/5,
         hkdf_extract/3,
         hkdf_expand/4,
         key_length/1,
         key_schedule/3,
         key_schedule/4,
         create_info/4,
         external_binder_key/2,
         resumption_binder_key/2,
         client_early_traffic_secret/3,
         early_exporter_master_secret/3,
         client_handshake_traffic_secret/3,
         server_handshake_traffic_secret/3,
         client_application_traffic_secret_0/3,
         server_application_traffic_secret_0/3,
         exporter_master_secret/3,
         resumption_master_secret/3,
         update_traffic_secret/2,
         calculate_traffic_keys/3,
         transcript_hash/2,
         finished_key/2,
         finished_verify_data/3,
         pre_shared_key/3]).

%% Tracing
-export([handle_trace/3]).

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
    HkdfLabel = create_info(Label0, Context, Length, <<"tls13 ">>),
    hkdf_expand(Secret, HkdfLabel, Length, Algo).

%% Create info parameter for HKDF-Expand:
%% HKDF-Expand(PRK, info, L) -> OKM
create_info(Label0, Context0, Length, Prefix) ->
    %% struct {
    %%     uint16 length = Length;
    %%     opaque label<7..255> = "tls13 " + Label;
    %%     opaque context<0..255> = Context;
    %% } HkdfLabel;
    Label1 = <<Prefix/binary, Label0/binary>>,
    LabelLen = byte_size(Label1),
    Label = <<?BYTE(LabelLen), Label1/binary>>,
    ContextLen = byte_size(Context0),
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

-spec finished(Role, Version, PrfAlgo, MasterSecret, Handshake) -> binary() when
      Role :: client | server,
      Version :: ssl_record:ssl_version(),
      PrfAlgo :: integer(),
      MasterSecret :: binary(),
      Handshake    :: [binary()].
%% TLS 1.0 -1.1  ---------------------------------------------------
finished(Role, Version, PrfAlgo, MasterSecret, Handshake)
  when Version == ?TLS_1_0; Version == ?TLS_1_1; PrfAlgo == ?MD5SHA ->
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
finished(Role, ?TLS_1_2, PrfAlgo, MasterSecret, Handshake) ->
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

-spec certificate_verify(HashAlgo, Handshake) -> binary() when
      HashAlgo :: md5sha | ssl:hash(),
      Handshake :: [binary()].
%% TLS 1.0 -1.1  ---------------------------------------------------
certificate_verify(md5sha, Handshake) ->
    MD5 = crypto:hash(md5, Handshake),
    SHA = crypto:hash(sha, Handshake),
    {digest, <<MD5/binary, SHA/binary>>};
%% TLS 1.0 -1.1  ---------------------------------------------------

%% TLS 1.2 ---------------------------------------------------
certificate_verify(_HashAlgo, Handshake) ->
    %% crypto:hash(HashAlgo, Handshake).
    %% Optimization: Let crypto calculate the hash in sign/verify call
    Handshake.

%% TLS 1.2 ---------------------------------------------------
-spec setup_keys(ssl_record:ssl_version(), integer(), binary(), binary(), binary(), integer(),
		 integer(), integer()) -> {binary(), binary(), binary(),
					  binary(), binary(), binary()}.
%% TLS v1.0  ---------------------------------------------------
setup_keys(?TLS_1_0, _PrfAlgo, MasterSecret, ServerRandom, ClientRandom, HashSize,
	   KeyMatLen, IVSize) ->
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
setup_keys(?TLS_1_1, _PrfAlgo, MasterSecret, ServerRandom, ClientRandom, HashSize,
	   KeyMatLen, IVSize) ->
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
setup_keys(?TLS_1_2, PrfAlgo, MasterSecret, ServerRandom, ClientRandom, HashSize,
	   KeyMatLen, IVSize) ->
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
-spec calculate_traffic_keys(atom(), integer(), binary()) -> {binary(), binary()}.
calculate_traffic_keys(HKDFAlgo, KeyLength, Secret) ->
    Key = hkdf_expand_label(Secret, <<"key">>, <<>>, KeyLength, HKDFAlgo),
    %% TODO: remove hard coded IV size
    IV = hkdf_expand_label(Secret, <<"iv">>, <<>>, 12, HKDFAlgo),
    {Key, IV}.

-spec key_length(CipherSuite) -> KeyLength when
      CipherSuite :: binary(),
      KeyLength :: 0 | 8 | 16 | 24 | 32.
key_length(CipherSuite) ->
    #{cipher := Cipher} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    ssl_cipher:key_material(Cipher).

%% TLS v1.3  ---------------------------------------------------

%% TLS 1.0 -1.2  ---------------------------------------------------
-spec mac_hash(integer() | atom(), binary(), integer(), integer(), tls_record:tls_version(),
	       integer(), binary()) -> binary().

mac_hash(Method, Mac_write_secret, Seq_num, Type, Version,Length, Fragment) ->
    %% RFC 2246 & 4346 - 6.2.3.1.
    %% HMAC_hash(MAC_write_secret, seq_num + TLSCompressed.type +
    %%              TLSCompressed.version + TLSCompressed.length +
    %%              TLSCompressed.fragment));
    {Major,Minor} = Version,
    Mac = hmac_hash(Method, Mac_write_secret,
		    [<<?UINT64(Seq_num), ?BYTE(Type),
		      ?BYTE(Major), ?BYTE(Minor), ?UINT16(Length)>>,
		     Fragment]),
    Mac.
%% TLS 1.0 -1.2  ---------------------------------------------------

-spec suites(ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].

suites(Version) when ?TLS_1_X(Version) ->
    lists:flatmap(fun exclusive_suites/1, suites_to_test(Version)).

suites_to_test(?TLS_1_0) -> [?TLS_1_0];
suites_to_test(?TLS_1_1) -> [?TLS_1_0];
suites_to_test(?TLS_1_2) -> [?TLS_1_2, ?TLS_1_0];
suites_to_test(?TLS_1_3) -> [?TLS_1_3, ?TLS_1_2, ?TLS_1_0].

-spec exclusive_suites(ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].

exclusive_suites(?TLS_1_3) ->
    [?TLS_AES_256_GCM_SHA384,
     ?TLS_AES_128_GCM_SHA256,

     ?TLS_CHACHA20_POLY1305_SHA256,

     ?TLS_AES_128_CCM_SHA256,
     ?TLS_AES_128_CCM_8_SHA256
    ];
exclusive_suites(?TLS_1_2) ->
    [?TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384,
     ?TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384,

     ?TLS_ECDHE_ECDSA_WITH_AES_256_CCM,
     ?TLS_ECDHE_ECDSA_WITH_AES_256_CCM_8,

     ?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384,
     ?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384,

     ?TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256,
     ?TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256,

     ?TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256,

     ?TLS_ECDHE_ECDSA_WITH_AES_128_CCM,
     ?TLS_ECDHE_ECDSA_WITH_AES_128_CCM_8,

     ?TLS_ECDH_ECDSA_WITH_AES_256_GCM_SHA384,
     ?TLS_ECDH_RSA_WITH_AES_256_GCM_SHA384,

     ?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA384,
     ?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA384,

     ?TLS_ECDH_ECDSA_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDH_RSA_WITH_AES_128_GCM_SHA256,

     ?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256,
     ?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256,

     ?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA256,
     ?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256,

     ?TLS_DHE_RSA_WITH_AES_256_GCM_SHA384,
     ?TLS_DHE_DSS_WITH_AES_256_GCM_SHA384,

     ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA256,
     ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA256,

     ?TLS_DHE_RSA_WITH_AES_128_GCM_SHA256,
     ?TLS_DHE_DSS_WITH_AES_128_GCM_SHA256,

     ?TLS_DHE_RSA_WITH_CHACHA20_POLY1305_SHA256,

     ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA256,
     ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA256

     %% not supported
     %% ?TLS_DH_RSA_WITH_AES_256_GCM_SHA384,
     %% ?TLS_DH_DSS_WITH_AES_256_GCM_SHA384,
     %% ?TLS_DH_RSA_WITH_AES_128_GCM_SHA256,
     %% ?TLS_DH_DSS_WITH_AES_128_GCM_SHA256
    ];
exclusive_suites(?TLS_1_1) ->
    [];
exclusive_suites(?TLS_1_0) ->
    [
     ?TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA,
     ?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA,

     ?TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA,
     ?TLS_ECDH_RSA_WITH_AES_256_CBC_SHA,

     ?TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA,
     ?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA,

     ?TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA,
     ?TLS_ECDH_RSA_WITH_AES_128_CBC_SHA,

     ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA,
     ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA,
     ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA,
     ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA
    ].

%%--------------------------------------------------------------------
-spec exclusive_anonymous_suites(ssl_record:ssl_version()) ->
          [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the anonymous cipher suites introduced
%% in Version, only supported if explicitly set by user.
%%--------------------------------------------------------------------
exclusive_anonymous_suites(?TLS_1_3) ->
    [];
exclusive_anonymous_suites(?TLS_1_2=Version) ->
    psk_anon_exclusive(Version) ++
        [?TLS_DH_anon_WITH_AES_128_GCM_SHA256,
         ?TLS_DH_anon_WITH_AES_256_GCM_SHA384,
         ?TLS_DH_anon_WITH_AES_128_CBC_SHA256,
         ?TLS_DH_anon_WITH_AES_256_CBC_SHA256,

         ?TLS_ECDH_anon_WITH_AES_128_CBC_SHA,
         ?TLS_ECDH_anon_WITH_AES_256_CBC_SHA,
         ?TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA,

         ?TLS_DH_anon_WITH_RC4_128_MD5];
exclusive_anonymous_suites(?TLS_1_1=Version) ->
    psk_anon_exclusive(Version) ++
        [?TLS_ECDH_anon_WITH_AES_128_CBC_SHA,
         ?TLS_ECDH_anon_WITH_AES_256_CBC_SHA,
         ?TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA,

         ?TLS_DH_anon_WITH_DES_CBC_SHA,
         ?TLS_DH_anon_WITH_RC4_128_MD5];
exclusive_anonymous_suites(?TLS_1_0=Version) ->
    psk_anon_exclusive(Version) ++
        [?TLS_DH_anon_WITH_RC4_128_MD5,
         ?TLS_DH_anon_WITH_3DES_EDE_CBC_SHA,
         ?TLS_DH_anon_WITH_DES_CBC_SHA
        ] ++ srp_suites_anon(Version).

%%--------------------------------------------------------------------
-spec psk_suites(ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the PSK cipher suites, only supported
%% if explicitly set by user.
%%--------------------------------------------------------------------
psk_suites(Version) when ?TLS_1_X(Version) ->
    psk_exclusive(Version).

-spec psk_exclusive(ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
psk_exclusive(?TLS_1_2) ->
    psk_exclusive(?TLS_1_0) -- [?TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA];
psk_exclusive(?TLS_1_0) ->
    [
     ?TLS_RSA_PSK_WITH_AES_256_GCM_SHA384,
     ?TLS_RSA_PSK_WITH_AES_256_CBC_SHA384,
     ?TLS_RSA_PSK_WITH_AES_128_GCM_SHA256,
     ?TLS_RSA_PSK_WITH_AES_128_CBC_SHA256,
     ?TLS_RSA_PSK_WITH_AES_256_CBC_SHA,
     ?TLS_RSA_PSK_WITH_AES_128_CBC_SHA,
     ?TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA,
     ?TLS_RSA_PSK_WITH_RC4_128_SHA];
psk_exclusive(_) ->
    [].

%%--------------------------------------------------------------------
-spec psk_suites_anon(ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the anonymous PSK cipher suites, only supported
%% if explicitly set by user.
%%--------------------------------------------------------------------
psk_suites_anon(Version) when ?TLS_1_X(Version) ->
    psk_anon_exclusive(?TLS_1_2) ++ psk_anon_exclusive(?TLS_1_0).

-spec psk_anon_exclusive(ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].

psk_anon_exclusive(?TLS_1_2) ->
    [
     ?TLS_DHE_PSK_WITH_AES_256_GCM_SHA384,
     ?TLS_PSK_WITH_AES_256_GCM_SHA384,
     ?TLS_DHE_PSK_WITH_AES_256_CCM,
     ?TLS_PSK_DHE_WITH_AES_256_CCM_8,
     ?TLS_PSK_WITH_AES_256_CCM,
     ?TLS_PSK_WITH_AES_256_CCM_8,
     ?TLS_ECDHE_PSK_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDHE_PSK_WITH_AES_128_CCM_SHA256,
     ?TLS_ECDHE_PSK_WITH_AES_128_CCM_8_SHA256,
     ?TLS_DHE_PSK_WITH_AES_128_GCM_SHA256,
     ?TLS_PSK_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDHE_PSK_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDHE_PSK_WITH_AES_128_CCM_8_SHA256,
     ?TLS_DHE_PSK_WITH_AES_128_CCM,
     ?TLS_PSK_DHE_WITH_AES_128_CCM_8,
     ?TLS_PSK_WITH_AES_128_CCM,
     ?TLS_PSK_WITH_AES_128_CCM_8
    ];
psk_anon_exclusive(?TLS_1_0) ->
	[
         ?TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA384,
         ?TLS_DHE_PSK_WITH_AES_256_CBC_SHA384,
         ?TLS_PSK_WITH_AES_256_CBC_SHA384,
         ?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA256,
         ?TLS_DHE_PSK_WITH_AES_128_CBC_SHA256,
         ?TLS_PSK_WITH_AES_128_CBC_SHA256,
         ?TLS_ECDHE_PSK_WITH_RC4_128_SHA,
         ?TLS_DHE_PSK_WITH_AES_256_CBC_SHA,
	 ?TLS_PSK_WITH_AES_256_CBC_SHA,
	 ?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA,
	 ?TLS_DHE_PSK_WITH_AES_128_CBC_SHA,
	 ?TLS_PSK_WITH_AES_128_CBC_SHA,
	 ?TLS_ECDHE_PSK_WITH_3DES_EDE_CBC_SHA,
	 ?TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA,
	 ?TLS_PSK_WITH_3DES_EDE_CBC_SHA,
	 ?TLS_ECDHE_PSK_WITH_RC4_128_SHA,
	 ?TLS_DHE_PSK_WITH_RC4_128_SHA,
	 ?TLS_PSK_WITH_RC4_128_SHA];
psk_anon_exclusive(_) ->
    [].
%%--------------------------------------------------------------------
-spec srp_suites(tls_record:tls_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the SRP cipher suites, only supported
%% if explicitly set by user.
%%--------------------------------------------------------------------
srp_suites(?TLS_1_2) ->
    srp_exclusive(?TLS_1_0) -- [?TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA,
                                  ?TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA
                                 ];
srp_suites(?TLS_1_1) ->
    srp_exclusive(?TLS_1_0);
srp_suites(?TLS_1_0) ->
    srp_exclusive(?TLS_1_0).


-spec srp_exclusive(tls_record:tls_version()) -> [ssl_cipher_format:cipher_suite()].

srp_exclusive(?TLS_1_0) ->
    [?TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA,
     ?TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA,
     ?TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA,
     ?TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA,
     ?TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA
    ];
srp_exclusive(_) ->
    [].

%%--------------------------------------------------------------------
-spec srp_suites_anon(tls_record:tls_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the SRP anonymous cipher suites, only supported
%% if explicitly set by user.
%%--------------------------------------------------------------------
srp_suites_anon(?TLS_1_2) ->
    srp_exclusive_anon(?TLS_1_0) -- [?TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA];
srp_suites_anon(?TLS_1_1) ->
    srp_exclusive_anon(?TLS_1_0);
srp_suites_anon(?TLS_1_0) ->
    srp_exclusive_anon(?TLS_1_0).


srp_exclusive_anon(?TLS_1_0) ->
    [?TLS_SRP_SHA_WITH_AES_128_CBC_SHA,
     ?TLS_SRP_SHA_WITH_AES_256_CBC_SHA,
     ?TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA
    ].

%%--------------------------------------------------------------------
-spec rc4_suites(Version::ssl_record:ssl_version()) ->
          [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the RSA|(ECDH/RSA)| (ECDH/ECDSA)
%% with RC4 cipher suites, only supported if explicitly set by user.
%% Are not considered secure any more. Other RC4 suites already
%% belonged to the user configured only category.
%%--------------------------------------------------------------------
rc4_suites(Version) when ?TLS_1_X(Version) ->
    rc4_exclusive(?TLS_1_0).

-spec rc4_exclusive(Version::ssl_record:ssl_version()) ->
          [ssl_cipher_format:cipher_suite()].

rc4_exclusive(?TLS_1_0) ->
    [?TLS_ECDHE_ECDSA_WITH_RC4_128_SHA,
     ?TLS_ECDHE_RSA_WITH_RC4_128_SHA,
     ?TLS_ECDH_ECDSA_WITH_RC4_128_SHA,
     ?TLS_ECDH_RSA_WITH_RC4_128_SHA,
     ?TLS_RSA_WITH_RC4_128_SHA,
     ?TLS_RSA_WITH_RC4_128_MD5];
rc4_exclusive(_) ->
    [].

%%--------------------------------------------------------------------
-spec des_suites(Version::ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the cipher suites
%% with DES cipher, only supported if explicitly set by user.
%% Are not considered secure any more.
%%--------------------------------------------------------------------
des_suites(Version) when ?TLS_1_X(Version) ->
    des_exclusive(?TLS_1_0).

des_exclusive(?TLS_1_0)->
    [?TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA,
     ?TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_DHE_RSA_WITH_DES_CBC_SHA,
     ?TLS_RSA_WITH_DES_CBC_SHA];
des_exclusive(_) ->
    [].
%%--------------------------------------------------------------------
-spec rsa_suites(Version::ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the RSA key exchange
%% cipher suites, only supported if explicitly set by user.
%% Are not considered secure any more.
%%--------------------------------------------------------------------
rsa_suites(Version) when ?TLS_1_X(Version) ->
    lists:flatmap(fun rsa_exclusive/1, rsa_suites_to_test(Version)).

rsa_suites_to_test(?TLS_1_2) -> [?TLS_1_2, ?TLS_1_0];
rsa_suites_to_test(?TLS_1_1) -> [?TLS_1_0];
rsa_suites_to_test(?TLS_1_0) -> [?TLS_1_0].

-spec rsa_exclusive(Version::ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
rsa_exclusive(?TLS_1_2) ->
    [
     ?TLS_RSA_WITH_AES_256_GCM_SHA384,
     ?TLS_RSA_WITH_AES_256_CBC_SHA256,
     ?TLS_RSA_WITH_AES_128_GCM_SHA256,
     ?TLS_RSA_WITH_AES_128_CBC_SHA256
    ];
rsa_exclusive(?TLS_1_0) ->
    [?TLS_RSA_WITH_AES_256_CBC_SHA,
     ?TLS_RSA_WITH_AES_128_CBC_SHA,
     ?TLS_RSA_WITH_3DES_EDE_CBC_SHA
    ];
rsa_exclusive(_) ->
    [].

signature_algs(?TLS_1_3, HashSigns) ->
    signature_algs(?TLS_1_2, HashSigns);
signature_algs(?TLS_1_2, HashSigns) ->
    CryptoSupports =  crypto:supports(),
    Hashes = proplists:get_value(hashs, CryptoSupports),
    PubKeys = proplists:get_value(public_keys, CryptoSupports),
    Schemes =  rsa_schemes(),
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
				    end;
                               (Alg, Acc) when is_atom(Alg) ->
                                    case lists:member(Alg, Schemes) of
                                        true ->
                                            [NewAlg] = signature_schemes(?TLS_1_3, [Alg]),
                                            [NewAlg| Acc];
					false ->
					    Acc
				    end
			    end, [], HashSigns),
    lists:reverse(Supported).

default_signature_algs([?TLS_1_3]) ->
    default_signature_schemes(?TLS_1_3) ++ legacy_signature_schemes(?TLS_1_3);
default_signature_algs([?TLS_1_3, ?TLS_1_2 | _]) ->
    default_signature_schemes(?TLS_1_3) ++ legacy_signature_schemes(?TLS_1_3) 
        ++ default_pre_1_3_signature_algs_only();
default_signature_algs([?TLS_1_2 = Version |_]) ->
    Default = [%% SHA2 ++ PSS
               {sha512, ecdsa},
               rsa_pss_pss_sha512,
               rsa_pss_rsae_sha512,
               {sha512, rsa},
               {sha384, ecdsa},
               rsa_pss_pss_sha384,
               rsa_pss_rsae_sha384,
               {sha384, rsa},
               {sha256, ecdsa},
               rsa_pss_pss_sha256,
               rsa_pss_rsae_sha256,
               {sha256, rsa}
              ],
    signature_algs(Version, Default);
default_signature_algs(_) ->
    undefined.

default_pre_1_3_signature_algs_only() ->
    Default = [%% SHA2
               {sha512, ecdsa},
               {sha384, ecdsa},
               {sha256, ecdsa}
              ],
    signature_algs(?TLS_1_2, Default).

legacy_signature_algs_pre_13() ->
    [{sha224, ecdsa}, {sha224, rsa}, {sha, ecdsa}, {sha, rsa}, {sha, dsa}].

signature_schemes(Version, [_|_] =SignatureSchemes) when is_tuple(Version)
                                                         andalso ?TLS_GTE(Version, ?TLS_1_2) ->
    CryptoSupports =  crypto:supports(),
    Hashes = proplists:get_value(hashs, CryptoSupports),
    PubKeys = proplists:get_value(public_keys, CryptoSupports),
    Curves = proplists:get_value(curves, CryptoSupports),
    RSAPSSSupported = lists:member(rsa_pkcs1_pss_padding,
                                   proplists:get_value(rsa_opts, CryptoSupports)),
    Fun = fun (Scheme, Acc) when is_atom(Scheme) ->
                  {Hash, Sign0, Curve} =
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
                  case proplists:get_bool(Sign, PubKeys)
                      andalso
                      (proplists:get_bool(Hash, Hashes)
                       andalso (Curve =:= undefined orelse
                                proplists:get_bool(Curve, Curves))
                       andalso is_pair(Hash, Sign, Hashes)) orelse
                      ((Sign == eddsa) andalso ((Curve == ed448)
                                                orelse
                                                (Curve == ed25519)))
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
    Default = [eddsa_ed25519,
               eddsa_ed448,
               ecdsa_secp521r1_sha512,
               ecdsa_secp384r1_sha384,
               ecdsa_secp256r1_sha256,
               rsa_pss_pss_sha512,
               rsa_pss_pss_sha384,
               rsa_pss_pss_sha256,
               rsa_pss_rsae_sha512,
               rsa_pss_rsae_sha384,
               rsa_pss_rsae_sha256
              ],
    signature_schemes(Version, Default).

legacy_signature_schemes(Version) ->
    %% These values refer solely to signatures
    %% which appear in certificates (see Section 4.4.2.2) and are not
    %% defined for use in signed TLS handshake messages, although they
    %% MAY appear in "signature_algorithms" and
    %% "signature_algorithms_cert" for backward compatibility with
    %% TLS 1.2.
    LegacySchemes =
        [rsa_pkcs1_sha512,
         rsa_pkcs1_sha384,
         rsa_pkcs1_sha256],
    signature_schemes(Version, LegacySchemes).

rsa_schemes() ->
    Supports = crypto:supports(),
    RSAOpts = proplists:get_value(rsa_opts, Supports),

    case lists:member(rsa_pkcs1_pss_padding, RSAOpts)
        andalso lists:member(rsa_pss_saltlen, RSAOpts)
        andalso lists:member(rsa_mgf1_md, RSAOpts) of
        true ->
            [rsa_pss_pss_sha512,
             rsa_pss_pss_sha384,
             rsa_pss_pss_sha256,
             rsa_pss_rsae_sha512,
             rsa_pss_rsae_sha384,
             rsa_pss_rsae_sha256];
        false ->
            []
    end.

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
    crypto:mac(hmac, mac_algo(Alg), Key, Value).

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
    lists:member(Hash, AtLeastMd5);
is_pair(_,_,_) ->
    false.

ec_curves(Desc, Version) ->
    Curves = list_ec_curves(Desc, Version),
    CryptoCurves = crypto:supports(curves),
    [Curve || Curve <- Curves, lists:member(Curve, CryptoCurves)].

list_ec_curves(default, Version) when ?TLS_LTE(Version, ?TLS_1_2)->
    [x25519, x448,
     secp521r1, brainpoolP512r1,
     secp384r1, brainpoolP384r1,
     secp256r1, brainpoolP256r1
    ];
list_ec_curves(all, Version) ->
    list_ec_curves(default, Version) ++ legacy_curves().

legacy_curves() ->
    [
     sect571r1, sect571k1,
     sect409k1, sect409r1,
     sect283k1, sect283r1,
     secp256k1,
     sect239k1,
     sect233k1, sect233r1,
     secp224k1, secp224r1,
     sect193r1, sect193r2,
     secp192k1, secp192r1,
     sect163k1, sect163r1, sect163r2,
     secp160k1, secp160r1, secp160r2].

ecc_curves(Version) when is_tuple(Version) ->
    TLSCurves = ec_curves(default, Version),
    ecc_curves(TLSCurves);
ecc_curves(TLSCurves) ->
    [pubkey_cert_records:namedCurves(Curve) || Curve <- TLSCurves].
    
groups() ->
    TLSGroups = groups(all),
    groups(TLSGroups).

-spec groups(all | default | TLSGroups :: list()) -> [ssl:group()].
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
groups(TLSGroups) when is_list(TLSGroups) ->
    CryptoGroups = supported_groups(),
    lists:filter(fun(Group) -> proplists:get_bool(Group, CryptoGroups) end, TLSGroups).

default_groups() ->
    TLSGroups = groups(default),
    groups(TLSGroups).

supported_groups() ->
    crypto:supports(curves) ++
        [ffdhe2048,ffdhe3072,ffdhe4096,ffdhe6144,ffdhe8192].

group_to_enum(secp256r1) -> ?SECP256R1;
group_to_enum(secp384r1) -> ?SECP384R1;
group_to_enum(secp521r1) -> ?SECP521R1;
group_to_enum(x25519)    -> ?X25519;
group_to_enum(x448)      -> ?X448;
group_to_enum(ffdhe2048) -> ?FFDHE2048;
group_to_enum(ffdhe3072) -> ?FFDHE3072;
group_to_enum(ffdhe4096) -> ?FFDHE4096;
group_to_enum(ffdhe6144) -> ?FFDHE6144;
group_to_enum(ffdhe8192) -> ?FFDHE8192.

enum_to_group(?SECP256R1) -> secp256r1;
enum_to_group(?SECP384R1) -> secp384r1;
enum_to_group(?SECP521R1) -> secp521r1;
enum_to_group(?X25519) -> x25519;
enum_to_group(?X448) -> x448;
enum_to_group(?FFDHE2048) -> ffdhe2048;
enum_to_group(?FFDHE3072) -> ffdhe3072;
enum_to_group(?FFDHE4096) -> ffdhe4096;
enum_to_group(?FFDHE6144) -> ffdhe6144;
enum_to_group(?FFDHE8192) -> ffdhe8192;
enum_to_group(_) -> undefined.

%% 1-22 deprecated in RFC 8422
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
%% RFC 8422
oid_to_enum(?secp256r1) -> 23;
oid_to_enum(?secp384r1) -> 24;
oid_to_enum(?secp521r1) -> 25;
%% RFC 7027
oid_to_enum(?brainpoolP256r1) -> 26;
oid_to_enum(?brainpoolP384r1) -> 27;
oid_to_enum(?brainpoolP512r1) -> 28;
%% RFC 8422 from RFC 7748
oid_to_enum(?'id-X25519') -> 29;
oid_to_enum(?'id-X448') -> 30.

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
enum_to_oid(29) -> ?'id-X25519';
enum_to_oid(30) -> ?'id-X448';
enum_to_oid(_) ->
    undefined.

%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(kdt,
             {call, {?MODULE, update_traffic_secret,
                     [_HKDF, ApplicationTrafficSecret0]}},
             Stack) ->
    ATS0 = string:sub_string(
          binary:bin_to_list(
            binary:encode_hex(ApplicationTrafficSecret0)), 1, 5) ++ "...",
    {io_lib:format("ApplicationTrafficSecret0 = \"~s\"", [ATS0]), Stack};
handle_trace(kdt,
             {return_from, {?MODULE, update_traffic_secret, 2},
              Return}, Stack) ->
    ATS = string:sub_string(
          binary:bin_to_list(
            binary:encode_hex(Return)), 1, 5) ++ "...",
    {io_lib:format("ApplicationTrafficSecret = \"~s\"", [ATS]), Stack}.
