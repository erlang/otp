%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2017. All Rights Reserved.
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
	 default_signature_algs/1, signature_algs/2]).

-type named_curve() :: sect571r1 | sect571k1 | secp521r1 | brainpoolP512r1 |
                       sect409k1 | sect409r1 | brainpoolP384r1 | secp384r1 |
                       sect283k1 | sect283r1 | brainpoolP256r1 | secp256k1 | secp256r1 |
                       sect239k1 | sect233k1 | sect233r1 | secp224k1 | secp224r1 |
                       sect193r1 | sect193r2 | secp192k1 | secp192r1 | sect163k1 |
                       sect163r1 | sect163r2 | secp160k1 | secp160r1 | secp160r2.
-type curves() :: [named_curve()].
-export_type([curves/0, named_curve/0]).

%%====================================================================
%% Internal application API
%%====================================================================

-spec master_secret(integer(), binary(), binary(), binary()) -> binary().

master_secret(PrfAlgo, PreMasterSecret, ClientRandom, ServerRandom) ->
    %% RFC 2246 & 4346 && RFC 5246 - 8.1 %% master_secret = PRF(pre_master_secret,
    %%                                      "master secret", ClientHello.random +
    %%                                      ServerHello.random)[0..47];

    prf(PrfAlgo, PreMasterSecret, <<"master secret">>,
	[ClientRandom, ServerRandom], 48).

-spec finished(client | server, integer(), integer(), binary(), [binary()]) -> binary().

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

-spec certificate_verify(md5sha | sha, integer(), [binary()]) -> binary().

certificate_verify(md5sha, _Version, Handshake) ->
    MD5 = crypto:hash(md5, Handshake),
    SHA = crypto:hash(sha, Handshake),
    <<MD5/binary, SHA/binary>>;

certificate_verify(HashAlgo, _Version, Handshake) ->
    crypto:hash(HashAlgo, Handshake).

-spec setup_keys(integer(), integer(), binary(), binary(), binary(), integer(),
		 integer(), integer()) -> {binary(), binary(), binary(),
					  binary(), binary(), binary()}.

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

%% TLS v1.1
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

%% TLS v1.2
setup_keys(Version, PrfAlgo, MasterSecret, ServerRandom, ClientRandom, HashSize,
	   KeyMatLen, IVSize)
  when Version == 3 ->
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

-spec mac_hash(integer(), binary(), integer(), integer(), tls_record:tls_version(),
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

-spec suites(1|2|3) -> [ssl_cipher:cipher_suite()].

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
    ] ++ suites(2).

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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%%% HMAC and the Pseudorandom Functions RFC 2246 & 4346 - 5.%%%%
hmac_hash(?NULL, _, _) ->
    <<>>;
hmac_hash(Alg, Key, Value) ->
    crypto:hmac(mac_algo(Alg), Key, Value).

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
