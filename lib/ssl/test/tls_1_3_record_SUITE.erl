%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

-module(tls_1_3_record_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssl/src/tls_record.hrl").
-include_lib("ssl/src/tls_handshake.hrl").
-include_lib("ssl/src/tls_handshake_1_3.hrl").
-include_lib("ssl/src/ssl_cipher.hrl").
-include_lib("ssl/src/ssl_internal.hrl").

all() ->
    [encode_decode,
     finished_verify_data,
     '1_RTT_handshake',
     '0_RTT_handshake'].

init_per_suite(Config) ->
    catch crypto:stop(),
    try (ok == crypto:start()) andalso ssl_test_lib:sufficient_crypto_support('tlsv1.3') of
	true ->
	    ssl_test_lib:clean_start(),
            Config;
        false ->
            {skip, "Not enough crypto support for TLS-1.3"} 
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:unload(ssl),
    application:stop(crypto).
%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

encode_decode() ->
     [{doc,"Test TLS 1.3 record encode/decode functions"}].

encode_decode(_Config) ->
    ConnectionStates =
        #{current_read =>
              #{beast_mitigation => one_n_minus_one,
                cipher_state =>
                    {cipher_state,
                     <<14,172,111,243,199,170,242,203,126,205,34,93,122,115,226,14,
                       15,117,155,48,24,112,61,15,113,208,127,51,179,227,194,232>>,
                     <<197,54,168,218,54,91,157,58,30,201,197,142,51,58,53,231,228,
                       131,57,122,170,78,82,196,30,48,23,16,95,255,185,236>>,
                     undefined,undefined,undefined,16},
                client_verify_data => undefined,compression_state => undefined,
                mac_secret => undefined,secure_renegotiation => undefined,
                security_parameters =>
                    {security_parameters,
                     <<19,2>>,
                     0,8,2,undefined,undefined,undefined,undefined,undefined,
                     sha384,undefined,undefined,
                     {handshake_secret,
                      <<128,229,186,211,62,127,182,20,62,166,233,23,135,64,121,
                        3,104,251,214,161,253,31,3,2,232,37,8,221,189,72,64,218,
                        121,41,112,148,254,34,68,164,228,60,161,201,132,55,56,
                        157>>}, undefined,
                     undefined,
                     <<92,24,205,75,244,60,136,212,250,32,214,20,37,3,213,87,61,207,
                       147,61,168,145,177,118,160,153,33,53,48,108,191,174>>,
                     undefined},
                sequence_number => 0,server_verify_data => undefined},
          current_write =>
              #{beast_mitigation => one_n_minus_one,
                cipher_state =>
                    {cipher_state,
                     <<14,172,111,243,199,170,242,203,126,205,34,93,122,115,226,14,
                       15,117,155,48,24,112,61,15,113,208,127,51,179,227,194,232>>,
                     <<197,54,168,218,54,91,157,58,30,201,197,142,51,58,53,231,228,
                       131,57,122,170,78,82,196,30,48,23,16,95,255,185,236>>,
                     undefined,undefined,undefined,16},
                client_verify_data => undefined,compression_state => undefined,
                mac_secret => undefined,secure_renegotiation => undefined,
                security_parameters =>
                    {security_parameters,
                     <<19,2>>,
                     0,8,2,undefined,undefined,undefined,undefined,undefined,
                     sha384,undefined,undefined,
                     {handshake_secret,
                      <<128,229,186,211,62,127,182,20,62,166,233,23,135,64,121,
                        3,104,251,214,161,253,31,3,2,232,37,8,221,189,72,64,218,
                        121,41,112,148,254,34,68,164,228,60,161,201,132,55,56,
                        157>>}, undefined,
                     undefined,
                     <<92,24,205,75,244,60,136,212,250,32,214,20,37,3,213,87,61,207,
                       147,61,168,145,177,118,160,153,33,53,48,108,191,174>>,
                     undefined},
                sequence_number => 0,server_verify_data => undefined}},

    PlainText = [11,
                 <<0,2,175>>,
                 <<0,0,2,171,0,2,166,48,130,2,162,48,130,1,138,2,9,0,186,57,220,137,88,255,
                   191,235,48,13,6,9,42,134,72,134,247,13,1,1,11,5,0,48,18,49,16,48,14,6,3,85,
                   4,3,12,7,84,101,115,116,32,67,65,48,30,23,13,49,56,48,53,48,52,49,52,49,50,
                   51,56,90,23,13,50,56,48,50,48,52,49,52,49,50,51,56,90,48,20,49,18,48,16,6,
                   3,85,4,3,12,9,108,111,99,97,108,104,111,115,116,48,130,1,34,48,13,6,9,42,
                   134,72,134,247,13,1,1,1,5,0,3,130,1,15,0,48,130,1,10,2,130,1,1,0,169,40,
                   144,176,121,63,134,97,144,126,243,183,225,157,37,131,183,225,87,243,23,88,
                   230,70,9,134,32,147,7,27,167,98,51,81,224,75,199,12,229,251,195,207,75,179,
                   181,78,128,3,255,44,58,39,43,172,142,45,186,58,51,65,187,199,154,153,245,
                   70,133,137,1,27,87,42,116,65,251,129,109,145,233,97,171,71,54,213,185,74,
                   209,166,11,218,189,119,206,86,170,60,212,213,85,189,30,50,215,23,185,53,
                   132,238,132,176,198,250,139,251,198,221,225,128,109,113,23,220,39,143,71,
                   30,59,189,51,244,61,158,214,146,180,196,103,169,189,221,136,78,129,216,148,
                   2,9,8,65,37,224,215,233,13,209,21,235,20,143,33,74,59,53,208,90,152,94,251,
                   54,114,171,39,88,230,227,158,211,135,37,182,67,205,161,59,20,138,58,253,15,
                   53,48,8,157,9,95,197,9,177,116,21,54,9,125,78,109,182,83,20,16,234,223,116,
                   41,155,123,87,77,17,120,153,246,239,124,130,105,219,166,146,242,151,66,198,
                   75,72,63,28,246,86,16,244,223,22,36,50,15,247,222,98,6,152,136,154,72,150,
                   73,127,2,3,1,0,1,48,13,6,9,42,134,72,134,247,13,1,1,11,5,0,3,130,1,1,0,76,
                   33,54,160,229,219,219,193,150,116,245,252,18,39,235,145,86,12,167,171,52,
                   117,166,30,83,5,216,245,177,217,247,95,1,136,94,246,212,108,248,230,111,
                   225,202,189,6,129,8,70,128,245,18,204,215,87,82,129,253,227,122,66,182,184,
                   189,30,193,169,144,218,216,109,105,110,215,144,60,104,162,178,101,164,218,
                   122,60,37,41,143,57,150,52,59,51,112,238,113,239,168,114,69,183,143,154,73,
                   61,58,80,247,172,95,251,55,28,186,28,200,206,230,118,243,92,202,189,49,76,
                   124,252,76,0,247,112,85,194,69,59,222,163,228,103,49,110,104,109,251,155,
                   138,9,37,167,49,189,48,134,52,158,185,129,24,96,153,196,251,90,206,76,239,
                   175,119,174,165,133,108,222,125,237,125,187,149,152,83,190,16,202,94,202,
                   201,40,218,22,254,63,189,41,174,97,140,203,70,18,196,118,237,175,134,79,78,
                   246,2,61,54,77,186,112,32,17,193,192,188,217,252,215,200,7,245,180,179,132,
                   183,212,229,155,15,152,206,135,56,81,88,3,123,244,149,110,182,72,109,70,62,
                   146,152,146,151,107,126,216,210,9,93,0,0>>],

    {[_Header|Encoded], _} = tls_record_1_3:encode_plain_text(22, PlainText, ConnectionStates),
    CipherText = #ssl_tls{type = 23, version = {3,3}, fragment = Encoded},

    {#ssl_tls{type = 22, version = {3,4}, fragment = DecodedText}, _} =
        tls_record_1_3:decode_cipher_text(CipherText, ConnectionStates),

    DecodedText = iolist_to_binary(PlainText),
    ct:log("Decoded: ~p ~n", [DecodedText]),
    ok.
%%--------------------------------------------------------------------
'1_RTT_handshake'() ->
     [{doc,"Test TLS 1.3 1-RTT Handshake"}].

'1_RTT_handshake'(_Config) ->
    %% ConnectionStates with NULL cipher
    ConnStatesNull =
       #{current_write =>
             #{security_parameters =>
                   #security_parameters{cipher_suite = ?TLS_NULL_WITH_NULL_NULL},
               sequence_number => 0
              }
        },

    %% {client}  construct a ClientHello handshake message:
    %%
    %%    ClientHello (196 octets):  01 00 00 c0 03 03 cb 34 ec b1 e7 81 63
    %%       ba 1c 38 c6 da cb 19 6a 6d ff a2 1a 8d 99 12 ec 18 a2 ef 62 83
    %%       02 4d ec e7 00 00 06 13 01 13 03 13 02 01 00 00 91 00 00 00 0b
    %%       00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01 00 00 0a 00 14 00
    %%       12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 23
    %%       00 00 00 33 00 26 00 24 00 1d 00 20 99 38 1d e5 60 e4 bd 43 d2
    %%       3d 8e 43 5a 7d ba fe b3 c0 6e 51 c1 3c ae 4d 54 13 69 1e 52 9a
    %%       af 2c 00 2b 00 03 02 03 04 00 0d 00 20 00 1e 04 03 05 03 06 03
    %%       02 03 08 04 08 05 08 06 04 01 05 01 06 01 02 01 04 02 05 02 06
    %%       02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01
    %%
    %% {client}  send handshake record:
    %%
    %%    payload (196 octets):  01 00 00 c0 03 03 cb 34 ec b1 e7 81 63 ba
    %%       1c 38 c6 da cb 19 6a 6d ff a2 1a 8d 99 12 ec 18 a2 ef 62 83 02
    %%       4d ec e7 00 00 06 13 01 13 03 13 02 01 00 00 91 00 00 00 0b 00
    %%       09 00 00 06 73 65 72 76 65 72 ff 01 00 01 00 00 0a 00 14 00 12
    %%       00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 23 00
    %%       00 00 33 00 26 00 24 00 1d 00 20 99 38 1d e5 60 e4 bd 43 d2 3d
    %%       8e 43 5a 7d ba fe b3 c0 6e 51 c1 3c ae 4d 54 13 69 1e 52 9a af
    %%       2c 00 2b 00 03 02 03 04 00 0d 00 20 00 1e 04 03 05 03 06 03 02
    %%       03 08 04 08 05 08 06 04 01 05 01 06 01 02 01 04 02 05 02 06 02
    %%       02 02 00 2d 00 02 01 01 00 1c 00 02 40 01
    %%
    %%    complete record (201 octets):  16 03 01 00 c4 01 00 00 c0 03 03 cb
    %%       34 ec b1 e7 81 63 ba 1c 38 c6 da cb 19 6a 6d ff a2 1a 8d 99 12
    %%       ec 18 a2 ef 62 83 02 4d ec e7 00 00 06 13 01 13 03 13 02 01 00
    %%       00 91 00 00 00 0b 00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01
    %%       00 00 0a 00 14 00 12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02
    %%       01 03 01 04 00 23 00 00 00 33 00 26 00 24 00 1d 00 20 99 38 1d
    %%       e5 60 e4 bd 43 d2 3d 8e 43 5a 7d ba fe b3 c0 6e 51 c1 3c ae 4d
    %%       54 13 69 1e 52 9a af 2c 00 2b 00 03 02 03 04 00 0d 00 20 00 1e
    %%       04 03 05 03 06 03 02 03 08 04 08 05 08 06 04 01 05 01 06 01 02
    %%       01 04 02 05 02 06 02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01
    ClientHello =
        hexstr2bin("01 00 00 c0 03 03 cb 34 ec b1 e7 81 63
          ba 1c 38 c6 da cb 19 6a 6d ff a2 1a 8d 99 12 ec 18 a2 ef 62 83
          02 4d ec e7 00 00 06 13 01 13 03 13 02 01 00 00 91 00 00 00 0b
          00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01 00 00 0a 00 14 00
          12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 23
          00 00 00 33 00 26 00 24 00 1d 00 20 99 38 1d e5 60 e4 bd 43 d2
          3d 8e 43 5a 7d ba fe b3 c0 6e 51 c1 3c ae 4d 54 13 69 1e 52 9a
          af 2c 00 2b 00 03 02 03 04 00 0d 00 20 00 1e 04 03 05 03 06 03
          02 03 08 04 08 05 08 06 04 01 05 01 06 01 02 01 04 02 05 02 06
          02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01"),

    ClientHelloRecord =
        %% Current implementation always sets
        %% legacy_record_version to Ox0303
        hexstr2bin("16 03 03 00 c4 01 00 00 c0 03 03 cb
          34 ec b1 e7 81 63 ba 1c 38 c6 da cb 19 6a 6d ff a2 1a 8d 99 12
          ec 18 a2 ef 62 83 02 4d ec e7 00 00 06 13 01 13 03 13 02 01 00
          00 91 00 00 00 0b 00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01
          00 00 0a 00 14 00 12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02
          01 03 01 04 00 23 00 00 00 33 00 26 00 24 00 1d 00 20 99 38 1d
          e5 60 e4 bd 43 d2 3d 8e 43 5a 7d ba fe b3 c0 6e 51 c1 3c ae 4d
          54 13 69 1e 52 9a af 2c 00 2b 00 03 02 03 04 00 0d 00 20 00 1e
          04 03 05 03 06 03 02 03 08 04 08 05 08 06 04 01 05 01 06 01 02
          01 04 02 05 02 06 02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01"),

    {CHEncrypted, _} =
	tls_record:encode_handshake(ClientHello, {3,4}, ConnStatesNull),
    ClientHelloRecord = iolist_to_binary(CHEncrypted),

    %% {server}  extract secret "early":
    %%
    %%    salt:  0 (all zero octets)
    %%
    %%    IKM (32 octets):  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%                      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%
    %%    secret (32 octets):  33 ad 0a 1c 60 7e c0 3b 09 e6 cd 98 93 68 0c
    %%       e2 10 ad f3 00 aa 1f 26 60 e1 b2 2e 10 f1 70 f9 2a
    HKDFAlgo = sha256,
    Salt = binary:copy(<<?BYTE(0)>>, 32),
    IKM = binary:copy(<<?BYTE(0)>>, 32),
    EarlySecret =
        hexstr2bin("33 ad 0a 1c 60 7e c0 3b 09 e6 cd 98 93 68 0c
          e2 10 ad f3 00 aa 1f 26 60 e1 b2 2e 10 f1 70 f9 2a"),

    {early_secret, EarlySecret} = tls_v1:key_schedule(early_secret, HKDFAlgo, {psk, Salt}),

    %% {client}  create an ephemeral x25519 key pair:
    %%
    %%    private key (32 octets):  49 af 42 ba 7f 79 94 85 2d 71 3e f2 78
    %%       4b cb ca a7 91 1d e2 6a dc 56 42 cb 63 45 40 e7 ea 50 05
    %%
    %%    public key (32 octets):  99 38 1d e5 60 e4 bd 43 d2 3d 8e 43 5a 7d
    %%       ba fe b3 c0 6e 51 c1 3c ae 4d 54 13 69 1e 52 9a af 2c
    CPublicKey =
        hexstr2bin("99 38 1d e5 60 e4 bd 43 d2 3d 8e 43 5a 7d
          ba fe b3 c0 6e 51 c1 3c ae 4d 54 13 69 1e 52 9a af 2c"),

    %% {server}  create an ephemeral x25519 key pair:
    %%
    %%   private key (32 octets):  b1 58 0e ea df 6d d5 89 b8 ef 4f 2d 56
    %%      52 57 8c c8 10 e9 98 01 91 ec 8d 05 83 08 ce a2 16 a2 1e
    %%
    %%   public key (32 octets):  c9 82 88 76 11 20 95 fe 66 76 2b db f7 c6
    %%      72 e1 56 d6 cc 25 3b 83 3d f1 dd 69 b1 b0 4e 75 1f 0f
    SPrivateKey =
        hexstr2bin("b1 58 0e ea df 6d d5 89 b8 ef 4f 2d 56
         52 57 8c c8 10 e9 98 01 91 ec 8d 05 83 08 ce a2 16 a2 1e"),

    SPublicKey =
        hexstr2bin("c9 82 88 76 11 20 95 fe 66 76 2b db f7 c6
         72 e1 56 d6 cc 25 3b 83 3d f1 dd 69 b1 b0 4e 75 1f 0f"),

    %% {server}  construct a ServerHello handshake message:
    %%
    %%    ServerHello (90 octets):  02 00 00 56 03 03 a6 af 06 a4 12 18 60
    %%       dc 5e 6e 60 24 9c d3 4c 95 93 0c 8a c5 cb 14 34 da c1 55 77 2e
    %%       d3 e2 69 28 00 13 01 00 00 2e 00 33 00 24 00 1d 00 20 c9 82 88
    %%       76 11 20 95 fe 66 76 2b db f7 c6 72 e1 56 d6 cc 25 3b 83 3d f1
    %%       dd 69 b1 b0 4e 75 1f 0f 00 2b 00 02 03 04
    ServerHello =
        hexstr2bin("02 00 00 56 03 03 a6 af 06 a4 12 18 60
          dc 5e 6e 60 24 9c d3 4c 95 93 0c 8a c5 cb 14 34 da c1 55 77 2e
          d3 e2 69 28 00 13 01 00 00 2e 00 33 00 24 00 1d 00 20 c9 82 88
          76 11 20 95 fe 66 76 2b db f7 c6 72 e1 56 d6 cc 25 3b 83 3d f1
          dd 69 b1 b0 4e 75 1f 0f 00 2b 00 02 03 04"),

    %% {server}  derive secret for handshake "tls13 derived":
    %%
    %%    PRK (32 octets):  33 ad 0a 1c 60 7e c0 3b 09 e6 cd 98 93 68 0c e2
    %%       10 ad f3 00 aa 1f 26 60 e1 b2 2e 10 f1 70 f9 2a
    %%
    %%    hash (32 octets):  e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24
    %%       27 ae 41 e4 64 9b 93 4c a4 95 99 1b 78 52 b8 55
    %%
    %%    info (49 octets):  00 20 0d 74 6c 73 31 33 20 64 65 72 69 76 65 64
    %%       20 e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24 27 ae 41 e4
    %%       64 9b 93 4c a4 95 99 1b 78 52 b8 55
    %%
    %%    expanded (32 octets):  6f 26 15 a1 08 c7 02 c5 67 8f 54 fc 9d ba
    %%       b6 97 16 c0 76 18 9c 48 25 0c eb ea c3 57 6c 36 11 ba
    Hash =
        hexstr2bin("e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24
          27 ae 41 e4 64 9b 93 4c a4 95 99 1b 78 52 b8 55"),

    Hash = crypto:hash(HKDFAlgo, <<>>),

    Info =
        hexstr2bin("00 20 0d 74 6c 73 31 33 20 64 65 72 69 76 65 64
          20 e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24 27 ae 41 e4
          64 9b 93 4c a4 95 99 1b 78 52 b8 55"),

    Info = tls_v1:create_info(<<"derived">>, Hash,  ssl_cipher:hash_size(HKDFAlgo)),

    Expanded =
        hexstr2bin("6f 26 15 a1 08 c7 02 c5 67 8f 54 fc 9d ba
          b6 97 16 c0 76 18 9c 48 25 0c eb ea c3 57 6c 36 11 ba"),

    Expanded = tls_v1:derive_secret(EarlySecret, <<"derived">>, <<>>, HKDFAlgo),

    %% {server}  extract secret "handshake":
    %%
    %%    salt (32 octets):  6f 26 15 a1 08 c7 02 c5 67 8f 54 fc 9d ba b6 97
    %%       16 c0 76 18 9c 48 25 0c eb ea c3 57 6c 36 11 ba
    %%
    %%    IKM (32 octets):  8b d4 05 4f b5 5b 9d 63 fd fb ac f9 f0 4b 9f 0d
    %%       35 e6 d6 3f 53 75 63 ef d4 62 72 90 0f 89 49 2d
    %%
    %%    secret (32 octets):  1d c8 26 e9 36 06 aa 6f dc 0a ad c1 2f 74 1b
    %%       01 04 6a a6 b9 9f 69 1e d2 21 a9 f0 ca 04 3f be ac

    %% salt = Expanded
    HandshakeIKM =
        hexstr2bin("8b d4 05 4f b5 5b 9d 63 fd fb ac f9 f0 4b 9f 0d
          35 e6 d6 3f 53 75 63 ef d4 62 72 90 0f 89 49 2d"),

    HandshakeSecret =
        hexstr2bin("1d c8 26 e9 36 06 aa 6f dc 0a ad c1 2f 74 1b
          01 04 6a a6 b9 9f 69 1e d2 21 a9 f0 ca 04 3f be ac"),

    HandshakeIKM = crypto:compute_key(ecdh, CPublicKey, SPrivateKey, x25519),

    {handshake_secret, HandshakeSecret} =
        tls_v1:key_schedule(handshake_secret, HKDFAlgo, HandshakeIKM,
                            {early_secret, EarlySecret}),

    %% {server}  derive secret "tls13 c hs traffic":
    %%
    %%    PRK (32 octets):  1d c8 26 e9 36 06 aa 6f dc 0a ad c1 2f 74 1b 01
    %%       04 6a a6 b9 9f 69 1e d2 21 a9 f0 ca 04 3f be ac
    %%
    %%    hash (32 octets):  86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58 ed
    %%       d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8
    %%
    %%    info (54 octets):  00 20 12 74 6c 73 31 33 20 63 20 68 73 20 74 72
    %%       61 66 66 69 63 20 86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58
    %%       ed d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8
    %%
    %%    expanded (32 octets):  b3 ed db 12 6e 06 7f 35 a7 80 b3 ab f4 5e
    %%       2d 8f 3b 1a 95 07 38 f5 2e 96 00 74 6a 0e 27 a5 5a 21

    %% PRK = HandshakeSecret
    CHSTHash =
        hexstr2bin("86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58 ed
          d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8"),

    CHSTInfo =
        hexstr2bin("00 20 12 74 6c 73 31 33 20 63 20 68 73 20 74 72
          61 66 66 69 63 20 86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58
          ed d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8"),

    CHSTrafficSecret =
        hexstr2bin(" b3 ed db 12 6e 06 7f 35 a7 80 b3 ab f4 5e
          2d 8f 3b 1a 95 07 38 f5 2e 96 00 74 6a 0e 27 a5 5a 21"),

    CHSH =  <<ClientHello/binary,ServerHello/binary>>,
    CHSTHash = crypto:hash(HKDFAlgo, CHSH),
    CHSTInfo =  tls_v1:create_info(<<"c hs traffic">>, CHSTHash,  ssl_cipher:hash_size(HKDFAlgo)),

    CHSTrafficSecret =
        tls_v1:client_handshake_traffic_secret(HKDFAlgo, {handshake_secret, HandshakeSecret}, CHSH),

    %% {server}  derive secret "tls13 s hs traffic":
    %%
    %%    PRK (32 octets):  1d c8 26 e9 36 06 aa 6f dc 0a ad c1 2f 74 1b 01
    %%       04 6a a6 b9 9f 69 1e d2 21 a9 f0 ca 04 3f be ac
    %%
    %%    hash (32 octets):  86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58 ed
    %%       d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8
    %%
    %%    info (54 octets):  00 20 12 74 6c 73 31 33 20 73 20 68 73 20 74 72
    %%       61 66 66 69 63 20 86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58
    %%       ed d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8
    %%
    %%    expanded (32 octets):  b6 7b 7d 69 0c c1 6c 4e 75 e5 42 13 cb 2d
    %%       37 b4 e9 c9 12 bc de d9 10 5d 42 be fd 59 d3 91 ad 38

    %% PRK = HandshakeSecret
    %% hash = CHSTHash
    SHSTInfo =
        hexstr2bin("00 20 12 74 6c 73 31 33 20 73 20 68 73 20 74 72
          61 66 66 69 63 20 86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58
          ed d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8"),

    SHSTrafficSecret =
        hexstr2bin("b6 7b 7d 69 0c c1 6c 4e 75 e5 42 13 cb 2d
          37 b4 e9 c9 12 bc de d9 10 5d 42 be fd 59 d3 91 ad 38"),

    SHSTInfo =  tls_v1:create_info(<<"s hs traffic">>, CHSTHash,  ssl_cipher:hash_size(HKDFAlgo)),

    SHSTrafficSecret =
        tls_v1:server_handshake_traffic_secret(HKDFAlgo, {handshake_secret, HandshakeSecret}, CHSH),


    %% {server}  derive secret for master "tls13 derived":
    %%
    %%    PRK (32 octets):  1d c8 26 e9 36 06 aa 6f dc 0a ad c1 2f 74 1b 01
    %%       04 6a a6 b9 9f 69 1e d2 21 a9 f0 ca 04 3f be ac
    %%
    %%    hash (32 octets):  e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24
    %%       27 ae 41 e4 64 9b 93 4c a4 95 99 1b 78 52 b8 55
    %%
    %%    info (49 octets):  00 20 0d 74 6c 73 31 33 20 64 65 72 69 76 65 64
    %%       20 e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24 27 ae 41 e4
    %%       64 9b 93 4c a4 95 99 1b 78 52 b8 55
    %%
    %%    expanded (32 octets):  43 de 77 e0 c7 77 13 85 9a 94 4d b9 db 25
    %%       90 b5 31 90 a6 5b 3e e2 e4 f1 2d d7 a0 bb 7c e2 54 b4

    %% PRK = HandshakeSecret
    %% hash = Hash
    %% info = Info
    MasterDeriveSecret =
        hexstr2bin("43 de 77 e0 c7 77 13 85 9a 94 4d b9 db 25
          90 b5 31 90 a6 5b 3e e2 e4 f1 2d d7 a0 bb 7c e2 54 b4"),

    MasterDeriveSecret = tls_v1:derive_secret(HandshakeSecret, <<"derived">>, <<>>, HKDFAlgo),

    %% {server}  extract secret "master":
    %%
    %%    salt (32 octets):  43 de 77 e0 c7 77 13 85 9a 94 4d b9 db 25 90 b5
    %%       31 90 a6 5b 3e e2 e4 f1 2d d7 a0 bb 7c e2 54 b4
    %%
    %%    IKM (32 octets):  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%
    %%    secret (32 octets):  18 df 06 84 3d 13 a0 8b f2 a4 49 84 4c 5f 8a
    %%       47 80 01 bc 4d 4c 62 79 84 d5 a4 1d a8 d0 40 29 19

    %% salt = MasterDeriveSecret
    %% IKM = IKM
    MasterSecret =
        hexstr2bin("18 df 06 84 3d 13 a0 8b f2 a4 49 84 4c 5f 8a
          47 80 01 bc 4d 4c 62 79 84 d5 a4 1d a8 d0 40 29 19"),

    {master_secret, MasterSecret} =
        tls_v1:key_schedule(master_secret, HKDFAlgo, {handshake_secret, HandshakeSecret}),

    %% {server}  send handshake record:
    %%
    %%    payload (90 octets):  02 00 00 56 03 03 a6 af 06 a4 12 18 60 dc 5e
    %%       6e 60 24 9c d3 4c 95 93 0c 8a c5 cb 14 34 da c1 55 77 2e d3 e2
    %%       69 28 00 13 01 00 00 2e 00 33 00 24 00 1d 00 20 c9 82 88 76 11
    %%       20 95 fe 66 76 2b db f7 c6 72 e1 56 d6 cc 25 3b 83 3d f1 dd 69
    %%       b1 b0 4e 75 1f 0f 00 2b 00 02 03 04
    %%
    %%    complete record (95 octets):  16 03 03 00 5a 02 00 00 56 03 03 a6
    %%       af 06 a4 12 18 60 dc 5e 6e 60 24 9c d3 4c 95 93 0c 8a c5 cb 14
    %%       34 da c1 55 77 2e d3 e2 69 28 00 13 01 00 00 2e 00 33 00 24 00
    %%       1d 00 20 c9 82 88 76 11 20 95 fe 66 76 2b db f7 c6 72 e1 56 d6
    %%       cc 25 3b 83 3d f1 dd 69 b1 b0 4e 75 1f 0f 00 2b 00 02 03 04

    %% payload = ServerHello
    ServerHelloRecord =
        hexstr2bin("16 03 03 00 5a 02 00 00 56 03 03 a6
          af 06 a4 12 18 60 dc 5e 6e 60 24 9c d3 4c 95 93 0c 8a c5 cb 14
          34 da c1 55 77 2e d3 e2 69 28 00 13 01 00 00 2e 00 33 00 24 00
          1d 00 20 c9 82 88 76 11 20 95 fe 66 76 2b db f7 c6 72 e1 56 d6
          cc 25 3b 83 3d f1 dd 69 b1 b0 4e 75 1f 0f 00 2b 00 02 03 04"),

    {SHEncrypted, _} =
	tls_record:encode_handshake(ServerHello, {3,4}, ConnStatesNull),
    ServerHelloRecord = iolist_to_binary(SHEncrypted),

    %% {server}  derive write traffic keys for handshake data:
    %%
    %%    PRK (32 octets):  b6 7b 7d 69 0c c1 6c 4e 75 e5 42 13 cb 2d 37 b4
    %%       e9 c9 12 bc de d9 10 5d 42 be fd 59 d3 91 ad 38
    %%
    %%    key info (13 octets):  00 10 09 74 6c 73 31 33 20 6b 65 79 00
    %%
    %%    key expanded (16 octets):  3f ce 51 60 09 c2 17 27 d0 f2 e4 e8 6e
    %%       e4 03 bc
    %%
    %%    iv info (12 octets):  00 0c 08 74 6c 73 31 33 20 69 76 00
    %%
    %%    iv expanded (12 octets):  5d 31 3e b2 67 12 76 ee 13 00 0b 30

    %% PRK = SHSTrafficSecret
    WriteKeyInfo =
        hexstr2bin("00 10 09 74 6c 73 31 33 20 6b 65 79 00"),

    WriteKey =
        hexstr2bin("3f ce 51 60 09 c2 17 27 d0 f2 e4 e8 6e e4 03 bc"),

    WriteIVInfo =
        hexstr2bin("00 0c 08 74 6c 73 31 33 20 69 76 00"),

    WriteIV =
        hexstr2bin(" 5d 31 3e b2 67 12 76 ee 13 00 0b 30"),

    Cipher = aes_128_gcm, %% TODO: get from ServerHello

    WriteKeyInfo = tls_v1:create_info(<<"key">>, <<>>,  ssl_cipher:key_material(Cipher)),
    %% TODO: remove hardcoded IV size
    WriteIVInfo = tls_v1:create_info(<<"iv">>, <<>>,  12),

    {WriteKey, WriteIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, SHSTrafficSecret),

    %% {server}  construct an EncryptedExtensions handshake message:
    %%
    %%    EncryptedExtensions (40 octets):  08 00 00 24 00 22 00 0a 00 14 00
    %%       12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 1c
    %%       00 02 40 01 00 00 00 00
    %%
    %% {server}  construct a Certificate handshake message:
    %%
    %%    Certificate (445 octets):  0b 00 01 b9 00 00 01 b5 00 01 b0 30 82
    %%       01 ac 30 82 01 15 a0 03 02 01 02 02 01 02 30 0d 06 09 2a 86 48
    %%       86 f7 0d 01 01 0b 05 00 30 0e 31 0c 30 0a 06 03 55 04 03 13 03
    %%       72 73 61 30 1e 17 0d 31 36 30 37 33 30 30 31 32 33 35 39 5a 17
    %%       0d 32 36 30 37 33 30 30 31 32 33 35 39 5a 30 0e 31 0c 30 0a 06
    %%       03 55 04 03 13 03 72 73 61 30 81 9f 30 0d 06 09 2a 86 48 86 f7
    %%       0d 01 01 01 05 00 03 81 8d 00 30 81 89 02 81 81 00 b4 bb 49 8f
    %%       82 79 30 3d 98 08 36 39 9b 36 c6 98 8c 0c 68 de 55 e1 bd b8 26
    %%       d3 90 1a 24 61 ea fd 2d e4 9a 91 d0 15 ab bc 9a 95 13 7a ce 6c
    %%       1a f1 9e aa 6a f9 8c 7c ed 43 12 09 98 e1 87 a8 0e e0 cc b0 52
    %%       4b 1b 01 8c 3e 0b 63 26 4d 44 9a 6d 38 e2 2a 5f da 43 08 46 74
    %%       80 30 53 0e f0 46 1c 8c a9 d9 ef bf ae 8e a6 d1 d0 3e 2b d1 93
    %%       ef f0 ab 9a 80 02 c4 74 28 a6 d3 5a 8d 88 d7 9f 7f 1e 3f 02 03
    %%       01 00 01 a3 1a 30 18 30 09 06 03 55 1d 13 04 02 30 00 30 0b 06
    %%       03 55 1d 0f 04 04 03 02 05 a0 30 0d 06 09 2a 86 48 86 f7 0d 01
    %%       01 0b 05 00 03 81 81 00 85 aa d2 a0 e5 b9 27 6b 90 8c 65 f7 3a
    %%       72 67 17 06 18 a5 4c 5f 8a 7b 33 7d 2d f7 a5 94 36 54 17 f2 ea
    %%       e8 f8 a5 8c 8f 81 72 f9 31 9c f3 6b 7f d6 c5 5b 80 f2 1a 03 01
    %%       51 56 72 60 96 fd 33 5e 5e 67 f2 db f1 02 70 2e 60 8c ca e6 be
    %%       c1 fc 63 a4 2a 99 be 5c 3e b7 10 7c 3c 54 e9 b9 eb 2b d5 20 3b
    %%       1c 3b 84 e0 a8 b2 f7 59 40 9b a3 ea c9 d9 1d 40 2d cc 0c c8 f8
    %%       96 12 29 ac 91 87 b4 2b 4d e1 00 00
    %%
    %% {server}  construct a CertificateVerify handshake message:
    %%
    %%    CertificateVerify (136 octets):  0f 00 00 84 08 04 00 80 5a 74 7c
    %%       5d 88 fa 9b d2 e5 5a b0 85 a6 10 15 b7 21 1f 82 4c d4 84 14 5a
    %%       b3 ff 52 f1 fd a8 47 7b 0b 7a bc 90 db 78 e2 d3 3a 5c 14 1a 07
    %%       86 53 fa 6b ef 78 0c 5e a2 48 ee aa a7 85 c4 f3 94 ca b6 d3 0b
    %%       be 8d 48 59 ee 51 1f 60 29 57 b1 54 11 ac 02 76 71 45 9e 46 44
    %%       5c 9e a5 8c 18 1e 81 8e 95 b8 c3 fb 0b f3 27 84 09 d3 be 15 2a
    %%       3d a5 04 3e 06 3d da 65 cd f5 ae a2 0d 53 df ac d4 2f 74 f3
    EncryptedExtensions =
        hexstr2bin("08 00 00 24 00 22 00 0a 00 14 00
          12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 1c
          00 02 40 01 00 00 00 00"),

    Certificate =
        hexstr2bin("0b 00 01 b9 00 00 01 b5 00 01 b0 30 82
          01 ac 30 82 01 15 a0 03 02 01 02 02 01 02 30 0d 06 09 2a 86 48
          86 f7 0d 01 01 0b 05 00 30 0e 31 0c 30 0a 06 03 55 04 03 13 03
          72 73 61 30 1e 17 0d 31 36 30 37 33 30 30 31 32 33 35 39 5a 17
          0d 32 36 30 37 33 30 30 31 32 33 35 39 5a 30 0e 31 0c 30 0a 06
          03 55 04 03 13 03 72 73 61 30 81 9f 30 0d 06 09 2a 86 48 86 f7
          0d 01 01 01 05 00 03 81 8d 00 30 81 89 02 81 81 00 b4 bb 49 8f
          82 79 30 3d 98 08 36 39 9b 36 c6 98 8c 0c 68 de 55 e1 bd b8 26
          d3 90 1a 24 61 ea fd 2d e4 9a 91 d0 15 ab bc 9a 95 13 7a ce 6c
          1a f1 9e aa 6a f9 8c 7c ed 43 12 09 98 e1 87 a8 0e e0 cc b0 52
          4b 1b 01 8c 3e 0b 63 26 4d 44 9a 6d 38 e2 2a 5f da 43 08 46 74
          80 30 53 0e f0 46 1c 8c a9 d9 ef bf ae 8e a6 d1 d0 3e 2b d1 93
          ef f0 ab 9a 80 02 c4 74 28 a6 d3 5a 8d 88 d7 9f 7f 1e 3f 02 03
          01 00 01 a3 1a 30 18 30 09 06 03 55 1d 13 04 02 30 00 30 0b 06
          03 55 1d 0f 04 04 03 02 05 a0 30 0d 06 09 2a 86 48 86 f7 0d 01
          01 0b 05 00 03 81 81 00 85 aa d2 a0 e5 b9 27 6b 90 8c 65 f7 3a
          72 67 17 06 18 a5 4c 5f 8a 7b 33 7d 2d f7 a5 94 36 54 17 f2 ea
          e8 f8 a5 8c 8f 81 72 f9 31 9c f3 6b 7f d6 c5 5b 80 f2 1a 03 01
          51 56 72 60 96 fd 33 5e 5e 67 f2 db f1 02 70 2e 60 8c ca e6 be
          c1 fc 63 a4 2a 99 be 5c 3e b7 10 7c 3c 54 e9 b9 eb 2b d5 20 3b
          1c 3b 84 e0 a8 b2 f7 59 40 9b a3 ea c9 d9 1d 40 2d cc 0c c8 f8
          96 12 29 ac 91 87 b4 2b 4d e1 00 00"),

    CertificateVerify =
        hexstr2bin("0f 00 00 84 08 04 00 80 5a 74 7c
          5d 88 fa 9b d2 e5 5a b0 85 a6 10 15 b7 21 1f 82 4c d4 84 14 5a
          b3 ff 52 f1 fd a8 47 7b 0b 7a bc 90 db 78 e2 d3 3a 5c 14 1a 07
          86 53 fa 6b ef 78 0c 5e a2 48 ee aa a7 85 c4 f3 94 ca b6 d3 0b
          be 8d 48 59 ee 51 1f 60 29 57 b1 54 11 ac 02 76 71 45 9e 46 44
          5c 9e a5 8c 18 1e 81 8e 95 b8 c3 fb 0b f3 27 84 09 d3 be 15 2a
          3d a5 04 3e 06 3d da 65 cd f5 ae a2 0d 53 df ac d4 2f 74 f3"),

    %% {server}  calculate finished "tls13 finished":
    %%
    %%    PRK (32 octets):  b6 7b 7d 69 0c c1 6c 4e 75 e5 42 13 cb 2d 37 b4
    %%       e9 c9 12 bc de d9 10 5d 42 be fd 59 d3 91 ad 38
    %%
    %%    hash (0 octets):  (empty)
    %%
    %%    info (18 octets):  00 20 0e 74 6c 73 31 33 20 66 69 6e 69 73 68 65
    %%       64 00
    %%
    %%    expanded (32 octets):  00 8d 3b 66 f8 16 ea 55 9f 96 b5 37 e8 85
    %%       c3 1f c0 68 bf 49 2c 65 2f 01 f2 88 a1 d8 cd c1 9f c8
    %%
    %%    finished (32 octets):  9b 9b 14 1d 90 63 37 fb d2 cb dc e7 1d f4
    %%       de da 4a b4 2c 30 95 72 cb 7f ff ee 54 54 b7 8f 07 18

    %% PRK = SHSTrafficSecret
    FInfo =
        hexstr2bin("00 20 0e 74 6c 73 31 33 20 66 69 6e 69 73 68 65
          64 00"),

    FExpanded =
        hexstr2bin("00 8d 3b 66 f8 16 ea 55 9f 96 b5 37 e8 85
          c3 1f c0 68 bf 49 2c 65 2f 01 f2 88 a1 d8 cd c1 9f c8"),

    FinishedVerifyData =
        hexstr2bin("9b 9b 14 1d 90 63 37 fb d2 cb dc e7 1d f4
          de da 4a b4 2c 30 95 72 cb 7f ff ee 54 54 b7 8f 07 18"),

    FInfo = tls_v1:create_info(<<"finished">>, <<>>,  ssl_cipher:hash_size(HKDFAlgo)),

    FExpanded = tls_v1:finished_key(SHSTrafficSecret, HKDFAlgo),

    MessageHistory0 = [CertificateVerify,
                       Certificate,
                       EncryptedExtensions,
                       ServerHello,
                       ClientHello],

    FinishedVerifyData = tls_v1:finished_verify_data(FExpanded, HKDFAlgo, MessageHistory0),

    %% {server}  construct a Finished handshake message:
    %%
    %%    Finished (36 octets):  14 00 00 20 9b 9b 14 1d 90 63 37 fb d2 cb
    %%       dc e7 1d f4 de da 4a b4 2c 30 95 72 cb 7f ff ee 54 54 b7 8f 07
    %%       18
    FinishedHSBin =
        hexstr2bin("14 00 00 20 9b 9b 14 1d 90 63 37 fb d2 cb
          dc e7 1d f4 de da 4a b4 2c 30 95 72 cb 7f ff ee 54 54 b7 8f 07
          18"),

    FinishedHS = #finished{verify_data = FinishedVerifyData},

    FinishedIOList = tls_handshake:encode_handshake(FinishedHS, {3,4}),
    FinishedHSBin = iolist_to_binary(FinishedIOList),

    %% {server}  derive secret "tls13 c ap traffic":
    %%
    %%    PRK (32 octets):  18 df 06 84 3d 13 a0 8b f2 a4 49 84 4c 5f 8a 47
    %%       80 01 bc 4d 4c 62 79 84 d5 a4 1d a8 d0 40 29 19
    %%
    %%    hash (32 octets):  96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a
    %%       00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    info (54 octets):  00 20 12 74 6c 73 31 33 20 63 20 61 70 20 74 72
    %%       61 66 66 69 63 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b
    %%       1a 00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    expanded (32 octets):  9e 40 64 6c e7 9a 7f 9d c0 5a f8 88 9b ce
    %%       65 52 87 5a fa 0b 06 df 00 87 f7 92 eb b7 c1 75 04 a5

    %% PRK = MasterSecret
    CAPTHash =
        hexstr2bin("96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a
          00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13"),
    CAPTInfo =
        hexstr2bin("00 20 12 74 6c 73 31 33 20 63 20 61 70 20 74 72
          61 66 66 69 63 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b
          1a 00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13"),

    CAPTrafficSecret =
        hexstr2bin("9e 40 64 6c e7 9a 7f 9d c0 5a f8 88 9b ce
          65 52 87 5a fa 0b 06 df 00 87 f7 92 eb b7 c1 75 04 a5"),

    CHSF = <<ClientHello/binary,
             ServerHello/binary,
             EncryptedExtensions/binary,
             Certificate/binary,
             CertificateVerify/binary,
             FinishedHSBin/binary>>,

    CAPTHash = crypto:hash(HKDFAlgo, CHSF),

    CAPTInfo =
        tls_v1:create_info(<<"c ap traffic">>, CAPTHash, ssl_cipher:hash_size(HKDFAlgo)),

    CAPTrafficSecret =
        tls_v1:client_application_traffic_secret_0(HKDFAlgo, {master_secret, MasterSecret}, CHSF),

    %% {server}  derive secret "tls13 s ap traffic":
    %%
    %%    PRK (32 octets):  18 df 06 84 3d 13 a0 8b f2 a4 49 84 4c 5f 8a 47
    %%       80 01 bc 4d 4c 62 79 84 d5 a4 1d a8 d0 40 29 19
    %%
    %%    hash (32 octets):  96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a
    %%       00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    info (54 octets):  00 20 12 74 6c 73 31 33 20 73 20 61 70 20 74 72
    %%       61 66 66 69 63 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b
    %%       1a 00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    expanded (32 octets):  a1 1a f9 f0 55 31 f8 56 ad 47 11 6b 45 a9
    %%       50 32 82 04 b4 f4 4b fb 6b 3a 4b 4f 1f 3f cb 63 16 43

    %% PRK = MasterSecret
    %% hash = CAPTHash
    SAPTInfo =
        hexstr2bin(" 00 20 12 74 6c 73 31 33 20 73 20 61 70 20 74 72
          61 66 66 69 63 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b
          1a 00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13"),

    SAPTrafficSecret =
        hexstr2bin("a1 1a f9 f0 55 31 f8 56 ad 47 11 6b 45 a9
          50 32 82 04 b4 f4 4b fb 6b 3a 4b 4f 1f 3f cb 63 16 43"),

    SAPTInfo =
        tls_v1:create_info(<<"s ap traffic">>, CAPTHash, ssl_cipher:hash_size(HKDFAlgo)),

    SAPTrafficSecret =
        tls_v1:server_application_traffic_secret_0(HKDFAlgo, {master_secret, MasterSecret}, CHSF),

    %% Resumption master secret from '0-RTT'
    %%
    %% {server}  generate resumption secret "tls13 resumption":
    %%
    %%    PRK (32 octets):  7d f2 35 f2 03 1d 2a 05 12 87 d0 2b 02 41 b0 bf
    %%       da f8 6c c8 56 23 1f 2d 5a ba 46 c4 34 ec 19 6c
    %%
    RMS = hexstr2bin("7d f2 35 f2 03 1d 2a 05 12 87 d0 2b 02 41 b0 bf
                      da f8 6c c8 56 23 1f 2d 5a ba 46 c4 34 ec 19 6c"),

    %% Verify calculation of resumption master secret that is used to create
    %% the pre shared key in '0-RTT'.
    Temp = tls_v1:resumption_master_secret(HKDFAlgo, {master_secret, MasterSecret}, CHSF),
    erlang:display({rms, RMS}),
    erlang:display({new_rms, Temp}),

    %% {server}  derive secret "tls13 exp master":
    %%
    %%    PRK (32 octets):  18 df 06 84 3d 13 a0 8b f2 a4 49 84 4c 5f 8a 47
    %%       80 01 bc 4d 4c 62 79 84 d5 a4 1d a8 d0 40 29 19
    %%
    %%    hash (32 octets):  96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a
    %%       00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    info (52 octets):  00 20 10 74 6c 73 31 33 20 65 78 70 20 6d 61 73
    %%       74 65 72 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a 00
    %%       0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    expanded (32 octets):  fe 22 f8 81 17 6e da 18 eb 8f 44 52 9e 67
    %%       92 c5 0c 9a 3f 89 45 2f 68 d8 ae 31 1b 43 09 d3 cf 50

    %% PRK = MasterSecret
    %% hash = CAPTHash
    ExporterInfo =
        hexstr2bin("00 20 10 74 6c 73 31 33 20 65 78 70 20 6d 61 73
          74 65 72 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a 00
          0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13"),

    ExporterMasterSecret =
        hexstr2bin("fe 22 f8 81 17 6e da 18 eb 8f 44 52 9e 67
          92 c5 0c 9a 3f 89 45 2f 68 d8 ae 31 1b 43 09 d3 cf 50"),

    ExporterInfo =
        tls_v1:create_info(<<"exp master">>, CAPTHash, ssl_cipher:hash_size(HKDFAlgo)),

    ExporterMasterSecret =
        tls_v1:exporter_master_secret(HKDFAlgo, {master_secret, MasterSecret}, CHSF),

    %% {server}  derive write traffic keys for application data:
    %%
    %%    PRK (32 octets):  a1 1a f9 f0 55 31 f8 56 ad 47 11 6b 45 a9 50 32
    %%       82 04 b4 f4 4b fb 6b 3a 4b 4f 1f 3f cb 63 16 43
    %%
    %%    key info (13 octets):  00 10 09 74 6c 73 31 33 20 6b 65 79 00
    %%
    %%    key expanded (16 octets):  9f 02 28 3b 6c 9c 07 ef c2 6b b9 f2 ac
    %%       92 e3 56
    %%
    %%    iv info (12 octets):  00 0c 08 74 6c 73 31 33 20 69 76 00
    %%
    %%    iv expanded (12 octets):  cf 78 2b 88 dd 83 54 9a ad f1 e9 84

    %% PRK = SAPTrafficsecret
    %% key info = WriteKeyInfo
    %% iv info = WriteIVInfo
    SWKey =
        hexstr2bin("9f 02 28 3b 6c 9c 07 ef c2 6b b9 f2 ac 92 e3 56"),

    SWIV =
        hexstr2bin("cf 78 2b 88 dd 83 54 9a ad f1 e9 84"),

    {SWKey, SWIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, SAPTrafficSecret),

    %% {server}  derive read traffic keys for handshake data:
    %%
    %%    PRK (32 octets):  b3 ed db 12 6e 06 7f 35 a7 80 b3 ab f4 5e 2d 8f
    %%       3b 1a 95 07 38 f5 2e 96 00 74 6a 0e 27 a5 5a 21
    %%
    %%    key info (13 octets):  00 10 09 74 6c 73 31 33 20 6b 65 79 00
    %%
    %%    key expanded (16 octets):  db fa a6 93 d1 76 2c 5b 66 6a f5 d9 50
    %%       25 8d 01
    %%
    %%    iv info (12 octets):  00 0c 08 74 6c 73 31 33 20 69 76 00
    %%
    %%    iv expanded (12 octets):  5b d3 c7 1b 83 6e 0b 76 bb 73 26 5f

    %% PRK = CHSTrafficsecret
    %% key info = WriteKeyInfo
    %% iv info = WrtieIVInfo
    SRKey =
        hexstr2bin("db fa a6 93 d1 76 2c 5b 66 6a f5 d9 50 25 8d 01"),

    SRIV =
        hexstr2bin("5b d3 c7 1b 83 6e 0b 76 bb 73 26 5f"),

    {SRKey, SRIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, CHSTrafficSecret),

    %% {client}  calculate finished "tls13 finished":
    %%
    %%    PRK (32 octets):  b3 ed db 12 6e 06 7f 35 a7 80 b3 ab f4 5e 2d 8f
    %%       3b 1a 95 07 38 f5 2e 96 00 74 6a 0e 27 a5 5a 21
    %%
    %%    hash (0 octets):  (empty)
    %%
    %%    info (18 octets):  00 20 0e 74 6c 73 31 33 20 66 69 6e 69 73 68 65
    %%       64 00
    %%
    %%    expanded (32 octets):  b8 0a d0 10 15 fb 2f 0b d6 5f f7 d4 da 5d
    %%       6b f8 3f 84 82 1d 1f 87 fd c7 d3 c7 5b 5a 7b 42 d9 c4
    %%
    %%    finished (32 octets):  a8 ec 43 6d 67 76 34 ae 52 5a c1 fc eb e1
    %%       1a 03 9e c1 76 94 fa c6 e9 85 27 b6 42 f2 ed d5 ce 61

    %% PRK = CHSTrafficsecret
    %% info = FInfo
    CFExpanded =
        hexstr2bin("b8 0a d0 10 15 fb 2f 0b d6 5f f7 d4 da 5d
        6b f8 3f 84 82 1d 1f 87 fd c7 d3 c7 5b 5a 7b 42 d9 c4"),

    CFinishedVerifyData =
        hexstr2bin("a8 ec 43 6d 67 76 34 ae 52 5a c1 fc eb e1
        1a 03 9e c1 76 94 fa c6 e9 85 27 b6 42 f2 ed d5 ce 61"),

    MessageHistory1 = [FinishedHSBin,
                       CertificateVerify,
                       Certificate,
                       EncryptedExtensions,
                       ServerHello,
                       ClientHello],

    CFExpanded = tls_v1:finished_key(CHSTrafficSecret, HKDFAlgo),
    CFinishedVerifyData = tls_v1:finished_verify_data(CFExpanded, HKDFAlgo, MessageHistory1),

    %% {client}  construct a Finished handshake message:
    %%
    %%    Finished (36 octets):  14 00 00 20 a8 ec 43 6d 67 76 34 ae 52 5a
    %%       c1 fc eb e1 1a 03 9e c1 76 94 fa c6 e9 85 27 b6 42 f2 ed d5 ce
    %%       61
    CFinishedBin =
        hexstr2bin("14 00 00 20 a8 ec 43 6d 67 76 34 ae 52 5a
          c1 fc eb e1 1a 03 9e c1 76 94 fa c6 e9 85 27 b6 42 f2 ed d5 ce
          61"),

    CFinished = #finished{verify_data = CFinishedVerifyData},

    CFinishedIOList = tls_handshake:encode_handshake(CFinished, {3,4}),
    CFinishedBin = iolist_to_binary(CFinishedIOList),

    %% {client}  derive write traffic keys for application data:
    %%
    %%    PRK (32 octets):  9e 40 64 6c e7 9a 7f 9d c0 5a f8 88 9b ce 65 52
    %%       87 5a fa 0b 06 df 00 87 f7 92 eb b7 c1 75 04 a5
    %%
    %%    key info (13 octets):  00 10 09 74 6c 73 31 33 20 6b 65 79 00
    %%
    %%    key expanded (16 octets):  17 42 2d da 59 6e d5 d9 ac d8 90 e3 c6
    %%       3f 50 51
    %%
    %%    iv info (12 octets):  00 0c 08 74 6c 73 31 33 20 69 76 00
    %%
    %%    iv expanded (12 octets):  5b 78 92 3d ee 08 57 90 33 e5 23 d9

    %% PRK = CAPTrafficsecret
    %% key info = WriteKeyInfo
    %% iv info = WriteIVInfo

    CWKey =
        hexstr2bin("17 42 2d da 59 6e d5 d9 ac d8 90 e3 c6 3f 50 51"),

    CWIV =
        hexstr2bin("5b 78 92 3d ee 08 57 90 33 e5 23 d9"),

    {CWKey, CWIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, CAPTrafficSecret),

    %% {client}  derive secret "tls13 res master":
    %%
    %%    PRK (32 octets):  18 df 06 84 3d 13 a0 8b f2 a4 49 84 4c 5f 8a 47
    %%       80 01 bc 4d 4c 62 79 84 d5 a4 1d a8 d0 40 29 19
    %%
    %%    hash (32 octets):  20 91 45 a9 6e e8 e2 a1 22 ff 81 00 47 cc 95 26
    %%       84 65 8d 60 49 e8 64 29 42 6d b8 7c 54 ad 14 3d
    %%
    %%    info (52 octets):  00 20 10 74 6c 73 31 33 20 72 65 73 20 6d 61 73
    %%       74 65 72 20 20 91 45 a9 6e e8 e2 a1 22 ff 81 00 47 cc 95 26 84
    %%       65 8d 60 49 e8 64 29 42 6d b8 7c 54 ad 14 3d
    %%
    %%    expanded (32 octets):  7d f2 35 f2 03 1d 2a 05 12 87 d0 2b 02 41
    %%       b0 bf da f8 6c c8 56 23 1f 2d 5a ba 46 c4 34 ec 19 6c

    %% PRK = MasterSecret

    CRMHash = hexstr2bin("20 91 45 a9 6e e8 e2 a1 22 ff 81 00 47 cc 95 26
                          84 65 8d 60 49 e8 64 29 42 6d b8 7c 54 ad 14 3d"),

    CRMInfo = hexstr2bin(" 00 20 10 74 6c 73 31 33 20 72 65 73 20 6d 61 73
            74 65 72 20 20 91 45 a9 6e e8 e2 a1 22 ff 81 00 47 cc 95 26 84
            65 8d 60 49 e8 64 29 42 6d b8 7c 54 ad 14 3d"),

    ResumptionMasterSecret = hexstr2bin("7d f2 35 f2 03 1d 2a 05 12 87 d0 2b 02 41
                             b0 bf da f8 6c c8 56 23 1f 2d 5a ba 46 c4 34 ec 19 6c"),

    CHCF = <<ClientHello/binary,
             ServerHello/binary,
             EncryptedExtensions/binary,
             Certificate/binary,
             CertificateVerify/binary,
             FinishedHSBin/binary,
             CFinishedBin/binary>>,

    MessageHistory3 = [ClientHello,
                       ServerHello,
                       EncryptedExtensions,
                       Certificate,
                       CertificateVerify,
                       FinishedHSBin,
                       CFinishedBin
                       ],

    CRMHash = crypto:hash(HKDFAlgo, CHCF),

    CRMInfo =
        tls_v1:create_info(<<"res master">>, CRMHash, ssl_cipher:hash_size(HKDFAlgo)),

    ResumptionMasterSecret =
        tls_v1:resumption_master_secret(HKDFAlgo, {master_secret, MasterSecret}, MessageHistory3),
    ok.

%%--------------------------------------------------------------------
'0_RTT_handshake'() ->
     [{doc,"Test TLS 1.3 0-RTT Handshake"}].

'0_RTT_handshake'(_Config) ->
    HKDFAlgo = sha256,

    %% {server}  generate resumption secret "tls13 resumption":
    %%
    %%    PRK (32 octets):  7d f2 35 f2 03 1d 2a 05 12 87 d0 2b 02 41 b0 bf
    %%       da f8 6c c8 56 23 1f 2d 5a ba 46 c4 34 ec 19 6c
    %%
    %%    hash (2 octets):  00 00
    %%
    %%    info (22 octets):  00 20 10 74 6c 73 31 33 20 72 65 73 75 6d 70 74
    %%       69 6f 6e 02 00 00
    %%
    %%    expanded (32 octets):  4e cd 0e b6 ec 3b 4d 87 f5 d6 02 8f 92 2c
    %%       a4 c5 85 1a 27 7f d4 13 11 c9 e6 2d 2c 94 92 e1 c4 f3
    %%
    %% {server}  construct a NewSessionTicket handshake message:
    %%
    %%    NewSessionTicket (205 octets):  04 00 00 c9 00 00 00 1e fa d6 aa
    %%       c5 02 00 00 00 b2 2c 03 5d 82 93 59 ee 5f f7 af 4e c9 00 00 00
    %%       00 26 2a 64 94 dc 48 6d 2c 8a 34 cb 33 fa 90 bf 1b 00 70 ad 3c
    %%       49 88 83 c9 36 7c 09 a2 be 78 5a bc 55 cd 22 60 97 a3 a9 82 11
    %%       72 83 f8 2a 03 a1 43 ef d3 ff 5d d3 6d 64 e8 61 be 7f d6 1d 28
    %%       27 db 27 9c ce 14 50 77 d4 54 a3 66 4d 4e 6d a4 d2 9e e0 37 25
    %%       a6 a4 da fc d0 fc 67 d2 ae a7 05 29 51 3e 3d a2 67 7f a5 90 6c
    %%       5b 3f 7d 8f 92 f2 28 bd a4 0d da 72 14 70 f9 fb f2 97 b5 ae a6
    %%       17 64 6f ac 5c 03 27 2e 97 07 27 c6 21 a7 91 41 ef 5f 7d e6 50
    %%       5e 5b fb c3 88 e9 33 43 69 40 93 93 4a e4 d3 57 00 08 00 2a 00
    %%       04 00 00 04 00
    ResPRK =
        hexstr2bin("7d f2 35 f2 03 1d 2a 05 12 87 d0 2b 02 41 b0 bf
                    da f8 6c c8 56 23 1f 2d 5a ba 46 c4 34 ec 19 6c"),

    _ResHash = hexstr2bin("00 00"),

    _ResInfo = hexstr2bin("00 20 10 74 6c 73 31 33 20 72 65 73 75 6d 70 74
                           69 6f 6e 02 00 00"),

    ResExpanded =
        hexstr2bin("4e cd 0e b6 ec 3b 4d 87 f5 d6 02 8f 92 2c
        a4 c5 85 1a 27 7f d4 13 11 c9 e6 2d 2c 94 92 e1 c4 f3"),

    NewSessionTicket =
        hexstr2bin("04 00 00 c9 00 00 00 1e fa d6 aa
          c5 02 00 00 00 b2 2c 03 5d 82 93 59 ee 5f f7 af 4e c9 00 00 00
          00 26 2a 64 94 dc 48 6d 2c 8a 34 cb 33 fa 90 bf 1b 00 70 ad 3c
          49 88 83 c9 36 7c 09 a2 be 78 5a bc 55 cd 22 60 97 a3 a9 82 11
          72 83 f8 2a 03 a1 43 ef d3 ff 5d d3 6d 64 e8 61 be 7f d6 1d 28
          27 db 27 9c ce 14 50 77 d4 54 a3 66 4d 4e 6d a4 d2 9e e0 37 25
          a6 a4 da fc d0 fc 67 d2 ae a7 05 29 51 3e 3d a2 67 7f a5 90 6c
          5b 3f 7d 8f 92 f2 28 bd a4 0d da 72 14 70 f9 fb f2 97 b5 ae a6
          17 64 6f ac 5c 03 27 2e 97 07 27 c6 21 a7 91 41 ef 5f 7d e6 50
          5e 5b fb c3 88 e9 33 43 69 40 93 93 4a e4 d3 57 00 08 00 2a 00
          04 00 00 04 00"),
    <<?BYTE(NWT), ?UINT24(_), TicketBody/binary>> = NewSessionTicket,
    #new_session_ticket{
       ticket_lifetime = _LifeTime,
       ticket_age_add = _AgeAdd,
       ticket_nonce = Nonce,
       ticket = Ticket,
       extensions = _Extensions
      } = tls_handshake:decode_handshake({3,4}, NWT, TicketBody),

    %% ResPRK = resumption master secret
    ResExpanded = tls_v1:pre_shared_key(ResPRK, Nonce, HKDFAlgo),

    %% {client}  create an ephemeral x25519 key pair:
    %%
    %%    private key (32 octets):  bf f9 11 88 28 38 46 dd 6a 21 34 ef 71
    %%       80 ca 2b 0b 14 fb 10 dc e7 07 b5 09 8c 0d dd c8 13 b2 df
    %%
    %%    public key (32 octets):  e4 ff b6 8a c0 5f 8d 96 c9 9d a2 66 98 34
    %%       6c 6b e1 64 82 ba dd da fe 05 1a 66 b4 f1 8d 66 8f 0b
    %%
    %% {client}  extract secret "early":
    %%
    %%    salt:  0 (all zero octets)
    %%
    %%    IKM (32 octets):  4e cd 0e b6 ec 3b 4d 87 f5 d6 02 8f 92 2c a4 c5
    %%       85 1a 27 7f d4 13 11 c9 e6 2d 2c 94 92 e1 c4 f3
    %%
    %%    secret (32 octets):  9b 21 88 e9 b2 fc 6d 64 d7 1d c3 29 90 0e 20
    %%       bb 41 91 50 00 f6 78 aa 83 9c bb 79 7c b7 d8 33 2c
    %%

    PSK = hexstr2bin("4e cd 0e b6 ec 3b 4d 87 f5 d6 02 8f 92 2c a4 c5
                      85 1a 27 7f d4 13 11 c9 e6 2d 2c 94 92 e1 c4 f3"),
    PSK = ResExpanded,
    EarlySecret = hexstr2bin("9b 21 88 e9 b2 fc 6d 64 d7 1d c3 29 90 0e 20
                      bb 41 91 50 00 f6 78 aa 83 9c bb 79 7c b7 d8 33 2c"),

    {early_secret, EarlySecret} = tls_v1:key_schedule(early_secret, HKDFAlgo, {psk, PSK}),

    %% {client}  construct a ClientHello handshake message:
    %%
    %%    ClientHello (477 octets):  01 00 01 fc 03 03 1b c3 ce b6 bb e3 9c
    %%       ff 93 83 55 b5 a5 0a db 6d b2 1b 7a 6a f6 49 d7 b4 bc 41 9d 78
    %%       76 48 7d 95 00 00 06 13 01 13 03 13 02 01 00 01 cd 00 00 00 0b
    %%       00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01 00 00 0a 00 14 00
    %%       12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 33
    %%       00 26 00 24 00 1d 00 20 e4 ff b6 8a c0 5f 8d 96 c9 9d a2 66 98
    %%       34 6c 6b e1 64 82 ba dd da fe 05 1a 66 b4 f1 8d 66 8f 0b 00 2a
    %%       00 00 00 2b 00 03 02 03 04 00 0d 00 20 00 1e 04 03 05 03 06 03
    %%       02 03 08 04 08 05 08 06 04 01 05 01 06 01 02 01 04 02 05 02 06
    %%       02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01 00 15 00 57 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 29 00 dd 00 b8 00 b2 2c 03 5d 82 93 59 ee 5f f7 af 4e c9
    %%       00 00 00 00 26 2a 64 94 dc 48 6d 2c 8a 34 cb 33 fa 90 bf 1b 00
    %%       70 ad 3c 49 88 83 c9 36 7c 09 a2 be 78 5a bc 55 cd 22 60 97 a3
    %%       a9 82 11 72 83 f8 2a 03 a1 43 ef d3 ff 5d d3 6d 64 e8 61 be 7f
    %%       d6 1d 28 27 db 27 9c ce 14 50 77 d4 54 a3 66 4d 4e 6d a4 d2 9e
    %%       e0 37 25 a6 a4 da fc d0 fc 67 d2 ae a7 05 29 51 3e 3d a2 67 7f
    %%       a5 90 6c 5b 3f 7d 8f 92 f2 28 bd a4 0d da 72 14 70 f9 fb f2 97
    %%       b5 ae a6 17 64 6f ac 5c 03 27 2e 97 07 27 c6 21 a7 91 41 ef 5f
    %%       7d e6 50 5e 5b fb c3 88 e9 33 43 69 40 93 93 4a e4 d3 57 fa d6
    %%       aa cb
    %%
    ClientHello =
        hexstr2bin("01 00 01 fc 03 03 1b c3 ce b6 bb e3 9c
          ff 93 83 55 b5 a5 0a db 6d b2 1b 7a 6a f6 49 d7 b4 bc 41 9d 78
          76 48 7d 95 00 00 06 13 01 13 03 13 02 01 00 01 cd 00 00 00 0b
          00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01 00 00 0a 00 14 00
          12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 33
          00 26 00 24 00 1d 00 20 e4 ff b6 8a c0 5f 8d 96 c9 9d a2 66 98
          34 6c 6b e1 64 82 ba dd da fe 05 1a 66 b4 f1 8d 66 8f 0b 00 2a
          00 00 00 2b 00 03 02 03 04 00 0d 00 20 00 1e 04 03 05 03 06 03
          02 03 08 04 08 05 08 06 04 01 05 01 06 01 02 01 04 02 05 02 06
          02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01 00 15 00 57 00 00
          00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
          00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
          00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
          00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
          00 00 29 00 dd 00 b8 00 b2 2c 03 5d 82 93 59 ee 5f f7 af 4e c9
          00 00 00 00 26 2a 64 94 dc 48 6d 2c 8a 34 cb 33 fa 90 bf 1b 00
          70 ad 3c 49 88 83 c9 36 7c 09 a2 be 78 5a bc 55 cd 22 60 97 a3
          a9 82 11 72 83 f8 2a 03 a1 43 ef d3 ff 5d d3 6d 64 e8 61 be 7f
          d6 1d 28 27 db 27 9c ce 14 50 77 d4 54 a3 66 4d 4e 6d a4 d2 9e
          e0 37 25 a6 a4 da fc d0 fc 67 d2 ae a7 05 29 51 3e 3d a2 67 7f
          a5 90 6c 5b 3f 7d 8f 92 f2 28 bd a4 0d da 72 14 70 f9 fb f2 97
          b5 ae a6 17 64 6f ac 5c 03 27 2e 97 07 27 c6 21 a7 91 41 ef 5f
          7d e6 50 5e 5b fb c3 88 e9 33 43 69 40 93 93 4a e4 d3 57 fa d6
          aa cb"),

    %% {client}  calculate PSK binder:
    %%
    %%    ClientHello prefix (477 octets):  01 00 01 fc 03 03 1b c3 ce b6 bb
    %%       e3 9c ff 93 83 55 b5 a5 0a db 6d b2 1b 7a 6a f6 49 d7 b4 bc 41
    %%       9d 78 76 48 7d 95 00 00 06 13 01 13 03 13 02 01 00 01 cd 00 00
    %%       00 0b 00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01 00 00 0a 00
    %%       14 00 12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04
    %%       00 33 00 26 00 24 00 1d 00 20 e4 ff b6 8a c0 5f 8d 96 c9 9d a2
    %%       66 98 34 6c 6b e1 64 82 ba dd da fe 05 1a 66 b4 f1 8d 66 8f 0b
    %%       00 2a 00 00 00 2b 00 03 02 03 04 00 0d 00 20 00 1e 04 03 05 03
    %%       06 03 02 03 08 04 08 05 08 06 04 01 05 01 06 01 02 01 04 02 05
    %%       02 06 02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01 00 15 00 57
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 29 00 dd 00 b8 00 b2 2c 03 5d 82 93 59 ee 5f f7 af
    %%       4e c9 00 00 00 00 26 2a 64 94 dc 48 6d 2c 8a 34 cb 33 fa 90 bf
    %%       1b 00 70 ad 3c 49 88 83 c9 36 7c 09 a2 be 78 5a bc 55 cd 22 60
    %%       97 a3 a9 82 11 72 83 f8 2a 03 a1 43 ef d3 ff 5d d3 6d 64 e8 61
    %%       be 7f d6 1d 28 27 db 27 9c ce 14 50 77 d4 54 a3 66 4d 4e 6d a4
    %%       d2 9e e0 37 25 a6 a4 da fc d0 fc 67 d2 ae a7 05 29 51 3e 3d a2
    %%       67 7f a5 90 6c 5b 3f 7d 8f 92 f2 28 bd a4 0d da 72 14 70 f9 fb
    %%       f2 97 b5 ae a6 17 64 6f ac 5c 03 27 2e 97 07 27 c6 21 a7 91 41
    %%       ef 5f 7d e6 50 5e 5b fb c3 88 e9 33 43 69 40 93 93 4a e4 d3 57
    %%       fa d6 aa cb
    %%
    %%    binder hash (32 octets):  63 22 4b 2e 45 73 f2 d3 45 4c a8 4b 9d
    %%       00 9a 04 f6 be 9e 05 71 1a 83 96 47 3a ef a0 1e 92 4a 14
    %%
    %%    PRK (32 octets):  69 fe 13 1a 3b ba d5 d6 3c 64 ee bc c3 0e 39 5b
    %%       9d 81 07 72 6a 13 d0 74 e3 89 db c8 a4 e4 72 56
    %%
    %%    hash (0 octets):  (empty)
    %%
    %%    info (18 octets):  00 20 0e 74 6c 73 31 33 20 66 69 6e 69 73 68 65
    %%       64 00
    %%
    %%    expanded (32 octets):  55 88 67 3e 72 cb 59 c8 7d 22 0c af fe 94
    %%       f2 de a9 a3 b1 60 9f 7d 50 e9 0a 48 22 7d b9 ed 7e aa
    %%
    %%    finished (32 octets):  3a dd 4f b2 d8 fd f8 22 a0 ca 3c f7 67 8e
    %%       f5 e8 8d ae 99 01 41 c5 92 4d 57 bb 6f a3 1b 9e 5f 9d
    BinderHash =
        hexstr2bin("63 22 4b 2e 45 73 f2 d3 45 4c a8 4b 9d
                    00 9a 04 f6 be 9e 05 71 1a 83 96 47 3a ef a0 1e 92 4a 14"),

    %% Part of derive_secret/4
    BinderHash = crypto:hash(HKDFAlgo, [ClientHello]),

    PRK = hexstr2bin("69 fe 13 1a 3b ba d5 d6 3c 64 ee bc c3 0e 39 5b
                      9d 81 07 72 6a 13 d0 74 e3 89 db c8 a4 e4 72 56"),

    PRK = tls_v1:resumption_binder_key(HKDFAlgo, {early_secret, EarlySecret}),

    Expanded =
        hexstr2bin("55 88 67 3e 72 cb 59 c8 7d 22 0c af fe 94
                    f2 de a9 a3 b1 60 9f 7d 50 e9 0a 48 22 7d b9 ed 7e aa"),

    Expanded = tls_v1:finished_key(PRK, HKDFAlgo),

    Finished = hexstr2bin("3a dd 4f b2 d8 fd f8 22 a0 ca 3c f7 67 8e
               f5 e8 8d ae 99 01 41 c5 92 4d 57 bb 6f a3 1b 9e 5f 9d"),

    Finished = tls_v1:finished_verify_data(Expanded, HKDFAlgo, [ClientHello]),

    %% {client}  send handshake record:
    %%
    %%    payload (512 octets):  01 00 01 fc 03 03 1b c3 ce b6 bb e3 9c ff
    %%       93 83 55 b5 a5 0a db 6d b2 1b 7a 6a f6 49 d7 b4 bc 41 9d 78 76
    %%       48 7d 95 00 00 06 13 01 13 03 13 02 01 00 01 cd 00 00 00 0b 00
    %%       09 00 00 06 73 65 72 76 65 72 ff 01 00 01 00 00 0a 00 14 00 12
    %%       00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 33 00
    %%       26 00 24 00 1d 00 20 e4 ff b6 8a c0 5f 8d 96 c9 9d a2 66 98 34
    %%       6c 6b e1 64 82 ba dd da fe 05 1a 66 b4 f1 8d 66 8f 0b 00 2a 00
    %%       00 00 2b 00 03 02 03 04 00 0d 00 20 00 1e 04 03 05 03 06 03 02
    %%       03 08 04 08 05 08 06 04 01 05 01 06 01 02 01 04 02 05 02 06 02
    %%       02 02 00 2d 00 02 01 01 00 1c 00 02 40 01 00 15 00 57 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 29 00 dd 00 b8 00 b2 2c 03 5d 82 93 59 ee 5f f7 af 4e c9 00
    %%       00 00 00 26 2a 64 94 dc 48 6d 2c 8a 34 cb 33 fa 90 bf 1b 00 70
    %%       ad 3c 49 88 83 c9 36 7c 09 a2 be 78 5a bc 55 cd 22 60 97 a3 a9
    %%       82 11 72 83 f8 2a 03 a1 43 ef d3 ff 5d d3 6d 64 e8 61 be 7f d6
    %%       1d 28 27 db 27 9c ce 14 50 77 d4 54 a3 66 4d 4e 6d a4 d2 9e e0
    %%       37 25 a6 a4 da fc d0 fc 67 d2 ae a7 05 29 51 3e 3d a2 67 7f a5
    %%       90 6c 5b 3f 7d 8f 92 f2 28 bd a4 0d da 72 14 70 f9 fb f2 97 b5
    %%       ae a6 17 64 6f ac 5c 03 27 2e 97 07 27 c6 21 a7 91 41 ef 5f 7d
    %%       e6 50 5e 5b fb c3 88 e9 33 43 69 40 93 93 4a e4 d3 57 fa d6 aa
    %%       cb 00 21 20 3a dd 4f b2 d8 fd f8 22 a0 ca 3c f7 67 8e f5 e8 8d
    %%       ae 99 01 41 c5 92 4d 57 bb 6f a3 1b 9e 5f 9d
    %%
    %%    complete record (517 octets):  16 03 01 02 00 01 00 01 fc 03 03 1b
    %%       c3 ce b6 bb e3 9c ff 93 83 55 b5 a5 0a db 6d b2 1b 7a 6a f6 49
    %%       d7 b4 bc 41 9d 78 76 48 7d 95 00 00 06 13 01 13 03 13 02 01 00
    %%       01 cd 00 00 00 0b 00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01
    %%       00 00 0a 00 14 00 12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02
    %%       01 03 01 04 00 33 00 26 00 24 00 1d 00 20 e4 ff b6 8a c0 5f 8d
    %%       96 c9 9d a2 66 98 34 6c 6b e1 64 82 ba dd da fe 05 1a 66 b4 f1
    %%       8d 66 8f 0b 00 2a 00 00 00 2b 00 03 02 03 04 00 0d 00 20 00 1e
    %%       04 03 05 03 06 03 02 03 08 04 08 05 08 06 04 01 05 01 06 01 02
    %%       01 04 02 05 02 06 02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01
    %%       00 15 00 57 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 29 00 dd 00 b8 00 b2 2c 03 5d 82 93 59
    %%       ee 5f f7 af 4e c9 00 00 00 00 26 2a 64 94 dc 48 6d 2c 8a 34 cb
    %%       33 fa 90 bf 1b 00 70 ad 3c 49 88 83 c9 36 7c 09 a2 be 78 5a bc
    %%       55 cd 22 60 97 a3 a9 82 11 72 83 f8 2a 03 a1 43 ef d3 ff 5d d3
    %%       6d 64 e8 61 be 7f d6 1d 28 27 db 27 9c ce 14 50 77 d4 54 a3 66
    %%       4d 4e 6d a4 d2 9e e0 37 25 a6 a4 da fc d0 fc 67 d2 ae a7 05 29
    %%       51 3e 3d a2 67 7f a5 90 6c 5b 3f 7d 8f 92 f2 28 bd a4 0d da 72
    %%       14 70 f9 fb f2 97 b5 ae a6 17 64 6f ac 5c 03 27 2e 97 07 27 c6
    %%       21 a7 91 41 ef 5f 7d e6 50 5e 5b fb c3 88 e9 33 43 69 40 93 93
    %%       4a e4 d3 57 fa d6 aa cb 00 21 20 3a dd 4f b2 d8 fd f8 22 a0 ca
    %%       3c f7 67 8e f5 e8 8d ae 99 01 41 c5 92 4d 57 bb 6f a3 1b 9e 5f
    %%       9d
    ClientHelloRecord =
        hexstr2bin("01 00 01 fc 03 03 1b c3 ce b6 bb e3 9c ff
          93 83 55 b5 a5 0a db 6d b2 1b 7a 6a f6 49 d7 b4 bc 41 9d 78 76
          48 7d 95 00 00 06 13 01 13 03 13 02 01 00 01 cd 00 00 00 0b 00
          09 00 00 06 73 65 72 76 65 72 ff 01 00 01 00 00 0a 00 14 00 12
          00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 33 00
          26 00 24 00 1d 00 20 e4 ff b6 8a c0 5f 8d 96 c9 9d a2 66 98 34
          6c 6b e1 64 82 ba dd da fe 05 1a 66 b4 f1 8d 66 8f 0b 00 2a 00
          00 00 2b 00 03 02 03 04 00 0d 00 20 00 1e 04 03 05 03 06 03 02
          03 08 04 08 05 08 06 04 01 05 01 06 01 02 01 04 02 05 02 06 02
          02 02 00 2d 00 02 01 01 00 1c 00 02 40 01 00 15 00 57 00 00 00
          00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
          00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
          00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
          00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
          00 29 00 dd 00 b8 00 b2 2c 03 5d 82 93 59 ee 5f f7 af 4e c9 00
          00 00 00 26 2a 64 94 dc 48 6d 2c 8a 34 cb 33 fa 90 bf 1b 00 70
          ad 3c 49 88 83 c9 36 7c 09 a2 be 78 5a bc 55 cd 22 60 97 a3 a9
          82 11 72 83 f8 2a 03 a1 43 ef d3 ff 5d d3 6d 64 e8 61 be 7f d6
          1d 28 27 db 27 9c ce 14 50 77 d4 54 a3 66 4d 4e 6d a4 d2 9e e0
          37 25 a6 a4 da fc d0 fc 67 d2 ae a7 05 29 51 3e 3d a2 67 7f a5
          90 6c 5b 3f 7d 8f 92 f2 28 bd a4 0d da 72 14 70 f9 fb f2 97 b5
          ae a6 17 64 6f ac 5c 03 27 2e 97 07 27 c6 21 a7 91 41 ef 5f 7d
          e6 50 5e 5b fb c3 88 e9 33 43 69 40 93 93 4a e4 d3 57 fa d6 aa
          cb 00 21 20 3a dd 4f b2 d8 fd f8 22 a0 ca 3c f7 67 8e f5 e8 8d
          ae 99 01 41 c5 92 4d 57 bb 6f a3 1b 9e 5f 9d"),

    <<?BYTE(CH), ?UINT24(_Length), ClientHelloBody/binary>> = ClientHelloRecord,
    #client_hello{extensions = #{pre_shared_key := PreSharedKey}} =
        tls_handshake:decode_handshake({3,4}, CH, ClientHelloBody),

    #pre_shared_key_client_hello{
       offered_psks = #offered_psks{
                         identities = [Identity],
                         binders = [_Binders]}} =  PreSharedKey,

    #psk_identity{
       identity = Ticket} = Identity,

    ok.


%%--------------------------------------------------------------------
finished_verify_data() ->
     [{doc,"Test TLS 1.3 Finished message handling"}].

finished_verify_data(_Config) ->
    ClientHello =
        hexstr2bin("01 00 00 c6 03 03 00 01  02 03 04 05 06 07 08 09
                    0a 0b 0c 0d 0e 0f 10 11  12 13 14 15 16 17 18 19
                    1a 1b 1c 1d 1e 1f 20 e0  e1 e2 e3 e4 e5 e6 e7 e8
                    e9 ea eb ec ed ee ef f0  f1 f2 f3 f4 f5 f6 f7 f8
                    f9 fa fb fc fd fe ff 00  06 13 01 13 02 13 03 01
                    00 00 77 00 00 00 18 00  16 00 00 13 65 78 61 6d
                    70 6c 65 2e 75 6c 66 68  65 69 6d 2e 6e 65 74 00
                    0a 00 08 00 06 00 1d 00  17 00 18 00 0d 00 14 00
                    12 04 03 08 04 04 01 05  03 08 05 05 01 08 06 06
                    01 02 01 00 33 00 26 00  24 00 1d 00 20 35 80 72
                    d6 36 58 80 d1 ae ea 32  9a df 91 21 38 38 51 ed
                    21 a2 8e 3b 75 e9 65 d0  d2 cd 16 62 54 00 2d 00
                    02 01 01 00 2b 00 03 02  03 04"),

    ServerHello =
        hexstr2bin("02 00 00 76 03 03 70 71  72 73 74 75 76 77 78 79
                    7a 7b 7c 7d 7e 7f 80 81  82 83 84 85 86 87 88 89
                    8a 8b 8c 8d 8e 8f 20 e0  e1 e2 e3 e4 e5 e6 e7 e8
                    e9 ea eb ec ed ee ef f0  f1 f2 f3 f4 f5 f6 f7 f8
                    f9 fa fb fc fd fe ff 13  01 00 00 2e 00 33 00 24
                    00 1d 00 20 9f d7 ad 6d  cf f4 29 8d d3 f9 6d 5b
                    1b 2a f9 10 a0 53 5b 14  88 d7 f8 fa bb 34 9a 98
                    28 80 b6 15 00 2b 00 02  03 04"),

    EncryptedExtensions =
        hexstr2bin("08 00 00 02 00 00"),

    Certificate =
        hexstr2bin("0b 00 03 2e 00 00 03 2a  00 03 25 30 82 03 21 30
                    82 02 09 a0 03 02 01 02  02 08 15 5a 92 ad c2 04
                    8f 90 30 0d 06 09 2a 86  48 86 f7 0d 01 01 0b 05
                    00 30 22 31 0b 30 09 06  03 55 04 06 13 02 55 53
                    31 13 30 11 06 03 55 04  0a 13 0a 45 78 61 6d 70
                    6c 65 20 43 41 30 1e 17  0d 31 38 31 30 30 35 30
                    31 33 38 31 37 5a 17 0d  31 39 31 30 30 35 30 31
                    33 38 31 37 5a 30 2b 31  0b 30 09 06 03 55 04 06
                    13 02 55 53 31 1c 30 1a  06 03 55 04 03 13 13 65
                    78 61 6d 70 6c 65 2e 75  6c 66 68 65 69 6d 2e 6e
                    65 74 30 82 01 22 30 0d  06 09 2a 86 48 86 f7 0d
                    01 01 01 05 00 03 82 01  0f 00 30 82 01 0a 02 82
                    01 01 00 c4 80 36 06 ba  e7 47 6b 08 94 04 ec a7
                    b6 91 04 3f f7 92 bc 19  ee fb 7d 74 d7 a8 0d 00
                    1e 7b 4b 3a 4a e6 0f e8  c0 71 fc 73 e7 02 4c 0d
                    bc f4 bd d1 1d 39 6b ba  70 46 4a 13 e9 4a f8 3d
                    f3 e1 09 59 54 7b c9 55  fb 41 2d a3 76 52 11 e1
                    f3 dc 77 6c aa 53 37 6e  ca 3a ec be c3 aa b7 3b
                    31 d5 6c b6 52 9c 80 98  bc c9 e0 28 18 e2 0b f7
                    f8 a0 3a fd 17 04 50 9e  ce 79 bd 9f 39 f1 ea 69
                    ec 47 97 2e 83 0f b5 ca  95 de 95 a1 e6 04 22 d5
                    ee be 52 79 54 a1 e7 bf  8a 86 f6 46 6d 0d 9f 16
                    95 1a 4c f7 a0 46 92 59  5c 13 52 f2 54 9e 5a fb
                    4e bf d7 7a 37 95 01 44  e4 c0 26 87 4c 65 3e 40
                    7d 7d 23 07 44 01 f4 84  ff d0 8f 7a 1f a0 52 10
                    d1 f4 f0 d5 ce 79 70 29  32 e2 ca be 70 1f df ad
                    6b 4b b7 11 01 f4 4b ad  66 6a 11 13 0f e2 ee 82
                    9e 4d 02 9d c9 1c dd 67  16 db b9 06 18 86 ed c1
                    ba 94 21 02 03 01 00 01  a3 52 30 50 30 0e 06 03
                    55 1d 0f 01 01 ff 04 04  03 02 05 a0 30 1d 06 03
                    55 1d 25 04 16 30 14 06  08 2b 06 01 05 05 07 03
                    02 06 08 2b 06 01 05 05  07 03 01 30 1f 06 03 55
                    1d 23 04 18 30 16 80 14  89 4f de 5b cc 69 e2 52
                    cf 3e a3 00 df b1 97 b8  1d e1 c1 46 30 0d 06 09
                    2a 86 48 86 f7 0d 01 01  0b 05 00 03 82 01 01 00
                    59 16 45 a6 9a 2e 37 79  e4 f6 dd 27 1a ba 1c 0b
                    fd 6c d7 55 99 b5 e7 c3  6e 53 3e ff 36 59 08 43
                    24 c9 e7 a5 04 07 9d 39  e0 d4 29 87 ff e3 eb dd
                    09 c1 cf 1d 91 44 55 87  0b 57 1d d1 9b df 1d 24
                    f8 bb 9a 11 fe 80 fd 59  2b a0 39 8c de 11 e2 65
                    1e 61 8c e5 98 fa 96 e5  37 2e ef 3d 24 8a fd e1
                    74 63 eb bf ab b8 e4 d1  ab 50 2a 54 ec 00 64 e9
                    2f 78 19 66 0d 3f 27 cf  20 9e 66 7f ce 5a e2 e4
                    ac 99 c7 c9 38 18 f8 b2  51 07 22 df ed 97 f3 2e
                    3e 93 49 d4 c6 6c 9e a6  39 6d 74 44 62 a0 6b 42
                    c6 d5 ba 68 8e ac 3a 01  7b dd fc 8e 2c fc ad 27
                    cb 69 d3 cc dc a2 80 41  44 65 d3 ae 34 8c e0 f3
                    4a b2 fb 9c 61 83 71 31  2b 19 10 41 64 1c 23 7f
                    11 a5 d6 5c 84 4f 04 04  84 99 38 71 2b 95 9e d6
                    85 bc 5c 5d d6 45 ed 19  90 94 73 40 29 26 dc b4
                    0e 34 69 a1 59 41 e8 e2  cc a8 4b b6 08 46 36 a0
                    00 00"),

    CertificateVerify =
        hexstr2bin("0f 00 01 04 08 04 01 00  17 fe b5 33 ca 6d 00 7d
                    00 58 25 79 68 42 4b bc  3a a6 90 9e 9d 49 55 75
                    76 a5 20 e0 4a 5e f0 5f  0e 86 d2 4f f4 3f 8e b8
                    61 ee f5 95 22 8d 70 32  aa 36 0f 71 4e 66 74 13
                    92 6e f4 f8 b5 80 3b 69  e3 55 19 e3 b2 3f 43 73
                    df ac 67 87 06 6d cb 47  56 b5 45 60 e0 88 6e 9b
                    96 2c 4a d2 8d ab 26 ba  d1 ab c2 59 16 b0 9a f2
                    86 53 7f 68 4f 80 8a ef  ee 73 04 6c b7 df 0a 84
                    fb b5 96 7a ca 13 1f 4b  1c f3 89 79 94 03 a3 0c
                    02 d2 9c bd ad b7 25 12  db 9c ec 2e 5e 1d 00 e5
                    0c af cf 6f 21 09 1e bc  4f 25 3c 5e ab 01 a6 79
                    ba ea be ed b9 c9 61 8f  66 00 6b 82 44 d6 62 2a
                    aa 56 88 7c cf c6 6a 0f  38 51 df a1 3a 78 cf f7
                    99 1e 03 cb 2c 3a 0e d8  7d 73 67 36 2e b7 80 5b
                    00 b2 52 4f f2 98 a4 da  48 7c ac de af 8a 23 36
                    c5 63 1b 3e fa 93 5b b4  11 e7 53 ca 13 b0 15 fe
                    c7 e4 a7 30 f1 36 9f 9e"),

    BaseKey =
        hexstr2bin("a2 06 72 65 e7 f0 65 2a  92 3d 5d 72 ab 04 67 c4
                    61 32 ee b9 68 b6 a3 2d  31 1c 80 58 68 54 88 14"),

    VerifyData =
        hexstr2bin("ea 6e e1 76 dc cc 4a f1  85 9e 9e 4e 93 f7 97 ea
                    c9 a7 8c e4 39 30 1e 35  27 5a d4 3f 3c dd bd e3"),

    Messages = [CertificateVerify,
                Certificate,
                EncryptedExtensions,
                ServerHello,
                ClientHello],

    FinishedKey = tls_v1:finished_key(BaseKey, sha256),
    VerifyData = tls_v1:finished_verify_data(FinishedKey, sha256, Messages).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

hexstr2int(S) ->
    B = hexstr2bin(S),
    Bits = size(B) * 8,
    <<Integer:Bits/integer>> = B,
    Integer.

hexstr2bin(S) when is_binary(S) ->
    hexstr2bin(S, <<>>);
hexstr2bin(S) ->
    hexstr2bin(list_to_binary(S), <<>>).
%%
hexstr2bin(<<>>, Acc) ->
    Acc;
hexstr2bin(<<C,T/binary>>, Acc) when C =:= 32;   %% SPACE
                                     C =:= 10;   %% LF
                                     C =:= 13 -> %% CR
    hexstr2bin(T, Acc);
hexstr2bin(<<X,Y,T/binary>>, Acc) ->
    I = hex2int(X) * 16 + hex2int(Y),
    hexstr2bin(T, <<Acc/binary,I>>).

hex2int(C) when $0 =< C, C =< $9 ->
    C - $0;
hex2int(C) when $A =< C, C =< $F ->
    C - $A + 10;
hex2int(C) when $a =< C, C =< $f ->
    C - $a + 10.
