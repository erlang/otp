%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022-2024. All Rights Reserved.
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
%% -------------------------------------------------------------------------
%%
%% Module for an encrypted Erlang stream based only on
%% the cookie as a shared secret
%%
-module(cryptcookie).

-export([supported/0, start_keypair_server/0, init/1, init/2,
         encrypt_and_send_chunk/4, recv_and_decrypt_chunk/2]).

%% For kTLS integration
-export([ktls_info/1, ktls_info/0]).
-include_lib("ssl/src/ssl_cipher.hrl").
-include_lib("ssl/src/ssl_internal.hrl").
-include_lib("ssl/src/ssl_record.hrl").


-define(PROTOCOL, (?MODULE)).

%% -------------------------------------------------------------------------
%% The curve choice greatly affects setup time,
%% we really want an Edwards curve but that would
%% require a very new openssl version.
%%
%% Twisted brainpool curves (*t1) are faster than
%% non-twisted (*r1), 256 is much faster than 384,
%% and so on...
%%
%%% -define(CURVE, brainpoolP384t1).
%%% -define(CURVE, brainpoolP256t1).
-define(CURVE, secp256r1).     % Portability
-define(CIPHER, aes_256_gcm).  % kTLS compatible
-define(HMAC, sha384).

%% kTLS integration
-define(TLS_VERSION, ?TLS_1_3).
-define(CIPHER_SUITE, (?TLS_AES_256_GCM_SHA384)).


supported() ->
    maybe
        true ?= crypto_supports(curves,  ?CURVE),
        true ?= crypto_supports(ciphers, ?CIPHER),
        true ?= crypto_supports(macs,    hmac),
        true ?= crypto_supports(hashs,   ?HMAC),
        ok
    end.

crypto_supports(Tag, Item) ->
    lists:member(Item, crypto:supports(Tag))
        orelse "Crypto does not support "
        ++ atom_to_list(Tag) ++ ": " ++ atom_to_list(Item).

%% -------------------------------------------------------------------------

-define(PACKET_SIZE, (1 bsl 16)).  % 2 byte size header
%% Plenty of room for AEAD tag and chunk type
-define(CHUNK_SIZE, (?PACKET_SIZE - 256)).

-record(params,
        {hmac_algorithm = ?HMAC,
         aead_cipher = ?CIPHER,
         iv,
         key,
         tag_len = 16,
         rekey_count = 256 * 1024,
         rekey_time = 2 * 3600, % (seconds): 2 hours
         rekey_timestamp,
         rekey_key
        }).

params() ->
    #{ iv_length := IVLen, key_length := KeyLen } =
        crypto:cipher_info(?CIPHER),
    #params{ iv = IVLen, key = KeyLen, rekey_timestamp = timestamp() }.


-record(keypair,
        {type = ecdh,
         params = ?CURVE,
         public,
         private,
         life_count = 256, % Number of connection setups
         life_time = 3600, % 1 hour
         life_timestamp
        }).

%% -------------------------------------------------------------------------
%% Keep the node's public/private key pair in the process state
%% of a key pair server linked to the net_kernel process.
%% Create the key pair the first time it is needed
%% so crypto gets time to start first.
%%

start_keypair_server() ->
    Parent = self(),
    Ref = make_ref(),
    _ =
        spawn_link(
          fun () ->
                  try register(?MODULE, self()) of
                      true ->
                          Parent ! Ref,
                          keypair_server()
                  catch error : badarg ->
                          %% Already started - simply exit
                          %% and let the other run
                          Parent ! Ref,
                          ok
                  end
          end),
    receive Ref ->
            ?PROTOCOL
    end.

keypair_server() ->
    keypair_server(undefined, 1).
%%
keypair_server(KeyPair) ->
    keypair_server(KeyPair, KeyPair#keypair.life_count).
%%
keypair_server(_KeyPair, 0) ->
    keypair_server();
keypair_server(KeyPair, Count) ->
    receive
        {RefAlias, get_keypair} when is_reference(RefAlias) ->
            case KeyPair of
                undefined ->
                    KeyPair_1 = generate_keypair(),
                    RefAlias ! {RefAlias, KeyPair_1},
                    keypair_server(KeyPair_1);
                #keypair{} ->
                    RefAlias ! {RefAlias, KeyPair},
                    keypair_server(KeyPair, Count - 1)
            end;
        {RefAlias, get_new_keypair} ->
            KeyPair_1 = generate_keypair(),
            RefAlias ! {RefAlias, KeyPair_1},
            keypair_server(KeyPair_1)
    end.

call_keypair_server(Request) ->
    Pid = whereis(?MODULE),
    RefAlias = erlang:monitor(process, Pid, [{alias, reply_demonitor}]),
    Pid ! {RefAlias, Request},
    receive
        {RefAlias, Reply} ->
            Reply;
        {'DOWN', RefAlias, process, Pid, Reason} ->
            error({keypair_server, Reason})
    end.

generate_keypair() ->
    #keypair{ type = Type, params = Params } = #keypair{},
    {Public, Private} = crypto:generate_key(Type, Params),
    #keypair{
       public = Public, private = Private,
       life_timestamp = timestamp() }.


get_keypair() ->
    call_keypair_server(?FUNCTION_NAME).

get_new_keypair() ->
    call_keypair_server(?FUNCTION_NAME).

compute_shared_secret(
  #keypair{
     type = PublicKeyType,
     params = PublicKeyParams,
     private = PrivKey }, PubKey) ->
    %%
    crypto:compute_key(PublicKeyType, PubKey, PrivKey, PublicKeyParams).

%% -------------------------------------------------------------------------

-define(DATA_CHUNK, 2).
-define(TICK_CHUNK, 3).
-define(REKEY_CHUNK, 4).

%% -------------------------------------------------------------------------
%% Crypto strategy
%% -------
%% The crypto strategy is as simple as possible to get an encrypted
%% connection as benchmark reference.  It is geared around AEAD
%% ciphers in particular AES-GCM.
%%
%% The init message and the start message must fit in the TCP buffers
%% since both sides start with sending the init message, waits
%% for the other end's init message, sends the start message
%% and waits for the other end's start message.  So if the send
%% blocks we have a deadlock.
%%
%% The init + start sequence tries to implement Password Encrypted
%% Key Exchange using a node public/private key pair and the
%% shared secret (the Cookie) to create session encryption keys
%% that can not be re-created if the shared secret is compromized,
%% which should create forward secrecy.  You need both nodes'
%% key pairs and the shared secret to decrypt the traffic
%% between the nodes.
%%
%% All exchanged messages uses {packet, 2} i.e 16 bit size header.
%%
%% The init message contains a random number and encrypted: the public key
%% and two random numbers.  The encryption is done with Key and IV hashed
%% from the unencrypted random number and the shared secret.
%%
%% The other node's public key is used with the own node's private
%% key to create a shared key that is hashed with one of the encrypted
%% random numbers from each side to create Key and IV for the session.
%%
%% The start message contains the two encrypted random numbers
%% this time encrypted with the session keys for verification
%% by the other side, plus the rekey count.  The rekey count
%% is just there to get an early check for if the other side's
%% maximum rekey count is acceptable, it is just an embryo
%% of some better check.  Any side may rekey earlier but if the
%% rekey count is exceeded the connection fails.  Rekey is also
%% triggered by a timer.
%%
%% Subsequent encrypted messages has the sequence number and the length
%% of the message as AAD data, and an incrementing IV.  These messages
%% has got a message type that differentiates data from ticks and rekeys.
%% Ticks have a random size in an attempt to make them less obvious to spot.
%%
%% Rekeying is done by the sender that creates a new key pair and
%% a new shared secret from the other end's public key and with
%% this and the current key and iv hashes a new key and iv.
%% The new public key is sent to the other end that uses it
%% and its old private key to create the same new shared
%% secret and from that a new key and iv.
%% So the receiver keeps its private key, and the sender keeps
%% the receivers public key for the connection's life time.
%% While the sender generates a new key pair at every rekey,
%% which changes the shared secret at every rekey.
%%
%% The only reaction to errors is to crash noisily (?) which will bring
%% down the connection and hopefully produce something useful
%% in the local log, but all the other end sees is a closed connection.
%% -------------------------------------------------------------------------


%% -------------------------------------------------------------------------
%% Initialize encryption on Stream; initial handshake
%%
%% init(Stream, Secret) ->
%%     {NewStream, ChunkSize, [RecvSeq|RecvParams], [SendSeq|SendParams]}.

init(Stream) ->
    Secret = atom_to_binary(auth:get_cookie(), latin1),
    init(Stream, Secret).

init(Stream = {_, OutStream, _}, Secret) ->
    #keypair{ public = PubKey } = KeyPair = get_keypair(),
    Params = params(),
    {R2, R3, Msg} = init_msg(Params, Secret, PubKey),
    OutStream_1 = init_send_block(OutStream, Msg, iolist_size(Msg)),
    Stream_1 = setelement(2, Stream, OutStream_1),
    init_recv(Stream_1, Params, Secret, KeyPair, R2, R3).

init_recv(
  {InStream, OutStream, ControllingProcessFun},
  Params = #params{ iv = IVLen },
  Secret, KeyPair, R2, R3) ->
    %%
    [InitMsg | InStream_1] = init_recv_block(InStream),
    IVSaltLen = IVLen - 6,
    try
        case init_msg(Params, Secret, KeyPair, R2, R3, InitMsg) of
            {SendParams =
                 #params{iv = <<IV2ASalt:IVSaltLen/binary, IV2ANo:48>> },
             RecvParams,
             SendStartMsg} ->
                OutStream_1 =
                    init_send_block(
                      OutStream, SendStartMsg, iolist_size(SendStartMsg)),
                [RecvStartMsg | InStream_2] = init_recv_block(InStream_1),
                RecvParams_1 =
                    #params{
                       iv = <<IV2BSalt:IVSaltLen/binary, IV2BNo:48>> } =
                    start_msg(RecvParams, R2, R3, RecvStartMsg),
                Stream_2 = {InStream_2, OutStream_1, ControllingProcessFun},
                RecvSeqParams =
                    [0 | RecvParams_1#params{ iv = {IV2BSalt, IV2BNo} }],
                SendSeqParams =
                    [0 | SendParams#params{ iv = {IV2ASalt, IV2ANo} }],
                CipherState = {RecvSeqParams, SendSeqParams},
                {Stream_2, ?CHUNK_SIZE, CipherState}
        end
    catch
        Class : Reason : Stacktrace when Class =:= error ->
            error_report(
              [init_recv_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
            _ = trace({Reason, Stacktrace}),
            exit(connection_closed)
    end.

init_msg(
  #params{
     hmac_algorithm = HmacAlgo,
     aead_cipher = AeadCipher,
     key = KeyLen,
     iv = IVLen,
     tag_len = TagLen }, Secret, PubKeyA) ->
    %%
    RLen = KeyLen + IVLen,
    <<R1A:RLen/binary, R2A:RLen/binary, R3A:RLen/binary>> =
        crypto:strong_rand_bytes(3 * RLen),
    {Key1A, IV1A} = hmac_key_iv(HmacAlgo, R1A, Secret, KeyLen, IVLen),
    Plaintext = [R2A, R3A, PubKeyA],
    MsgLen = byte_size(R1A) + TagLen + iolist_size(Plaintext),
    AAD = [<<MsgLen:32>>, R1A],
    {Ciphertext, Tag} =
        crypto:crypto_one_time_aead(
          AeadCipher, Key1A, IV1A, Plaintext, AAD, TagLen, true),
    Msg = [R1A, Tag, Ciphertext],
    {R2A, R3A, Msg}.
%%
init_msg(
  #params{
     hmac_algorithm = HmacAlgo,
     aead_cipher = AeadCipher,
     key = KeyLen,
     iv = IVLen,
     tag_len = TagLen,
     rekey_count = RekeyCount } = Params,
  Secret, KeyPair, R2A, R3A, Msg) ->
    %%
    RLen = KeyLen + IVLen,
    case Msg of
        <<R1B:RLen/binary, Tag:TagLen/binary, Ciphertext/binary>> ->
            {Key1B, IV1B} = hmac_key_iv(HmacAlgo, R1B, Secret, KeyLen, IVLen),
            MsgLen = byte_size(Msg),
            AAD = [<<MsgLen:32>>, R1B],
            case
                crypto:crypto_one_time_aead(
                  AeadCipher, Key1B, IV1B, Ciphertext, AAD, Tag, false)
            of
                <<R2B:RLen/binary, R3B:RLen/binary, PubKeyB/binary>> ->
                    SharedSecret = compute_shared_secret(KeyPair, PubKeyB),
                    %%
                    {Key2A, IV2A} =
                        hmac_key_iv(
                          HmacAlgo, SharedSecret, [R2A, R3B], KeyLen, IVLen),
                    SendParams =
                        Params#params{
                          rekey_key = PubKeyB,
                          key = Key2A, iv = IV2A },
                    %%
                    StartCleartext = [R2B, R3B, <<RekeyCount:32>>],
                    StartMsgLen = TagLen + iolist_size(StartCleartext),
                    StartAAD = <<StartMsgLen:32>>,
                    {StartCiphertext, StartTag} =
                        crypto:crypto_one_time_aead(
                          AeadCipher, Key2A, IV2A,
                          StartCleartext, StartAAD, TagLen, true),
                    StartMsg = [StartTag, StartCiphertext],
                    %%
                    {Key2B, IV2B} =
                        hmac_key_iv(
                          HmacAlgo, SharedSecret, [R2B, R3A], KeyLen, IVLen),
                    RecvParams =
                        Params#params{
                          rekey_key = KeyPair,
                          key = Key2B, iv = IV2B },
                    %%
                    {SendParams, RecvParams, StartMsg}
            end
    end.

start_msg(
  #params{
     aead_cipher = AeadCipher,
     key = Key2B,
     iv = IV2B,
     tag_len = TagLen,
     rekey_count = RekeyCountA } = RecvParams, R2A, R3A, Msg) ->
    %%
    case Msg of
        <<Tag:TagLen/binary, Ciphertext/binary>> ->
            KeyLen = byte_size(Key2B),
            IVLen = byte_size(IV2B),
            RLen = KeyLen + IVLen,
            MsgLen = byte_size(Msg),
            AAD = <<MsgLen:32>>,
            case
                crypto:crypto_one_time_aead(
                  AeadCipher, Key2B, IV2B, Ciphertext, AAD, Tag, false)
            of
                <<R2A:RLen/binary, R3A:RLen/binary, RekeyCountB:32>>
                  when RekeyCountA =< (RekeyCountB bsl 2),
                       RekeyCountB =< (RekeyCountA bsl 2) ->
                    RecvParams#params{ rekey_count = RekeyCountB }
            end
    end.

hmac_key_iv(HmacAlgo, MacKey, Data, KeyLen, IVLen) ->
    <<Key:KeyLen/binary, IV:IVLen/binary>> =
        crypto:macN(hmac, HmacAlgo, MacKey, Data, KeyLen + IVLen),
    {Key, IV}.


init_send_block(OutStream, Chunk, Size) ->
    OutStream_1 = send_block(OutStream, Chunk, Size),
    if
        hd(OutStream_1) =:= closed ->
            error_report(
              [?FUNCTION_NAME,
               {reason, closed}]),
            _ = trace({?FUNCTION_NAME, closed}),
            exit(connection_closed);
        true ->
            OutStream_1
    end.

init_recv_block(InStream) ->
    Result = [Data | _] = recv_block(InStream),
    if
        Data =:= closed ->
            error_report(
              [?FUNCTION_NAME,
               {reason, closed}]),
            _ = trace({?FUNCTION_NAME, closed}),
            exit(connection_closed);
        true ->
            Result
    end.


%% -------------------------------------------------------------------------
encrypt_and_send_chunk(
  OutStream,
  [Seq |
   Params =
       #params{ rekey_count = RekeyCount,
                rekey_time = RekeyTime,
                rekey_timestamp = RekeyTimestamp }],
   Chunk, Size) ->
    %%
    Timestamp = timestamp(),
    if
        RekeyCount =< Seq;
        RekeyTimestamp + RekeyTime =< Timestamp ->
            {OutStream_1, Params_1} =
                encrypt_and_send_rekey_chunk(
                  OutStream, Seq, Params, Timestamp),
            if
                hd(OutStream_1) =:= closed ->
                    {OutStream_1, [0 | Params_1]};
                true ->
                    {encrypt_and_send_chunk(
                       OutStream_1, 0, Params_1, Chunk, Size),
                     [1 | Params_1]}
            end;
        true ->
            {encrypt_and_send_chunk(OutStream, Seq, Params, Chunk, Size),
             [Seq + 1 | Params]}
    end.

encrypt_and_send_chunk(OutStream, Seq, Params, Chunk, 0) -> % Tick
    <<>> = Chunk, % ASSERT
    %% A ticks are sent as a somewhat random size block
    %% to make it less obvious to spot
    <<S:8>> = crypto:strong_rand_bytes(1),
    TickSize = 8 + (S band 63),
    TickData = crypto:strong_rand_bytes(TickSize),
    encrypt_and_send_block(
      OutStream, Seq, Params, [?TICK_CHUNK, TickData], 1 + TickSize);
encrypt_and_send_chunk(OutStream, Seq, Params, Chunk, Size) ->
    encrypt_and_send_block(
      OutStream, Seq, Params, [?DATA_CHUNK, Chunk], 1 + Size).

encrypt_and_send_rekey_chunk(
  OutStream, Seq,
  Params =
      #params{
         rekey_key = PubKeyB,
         key = Key,
         iv = {IVSalt, IVNo},
         hmac_algorithm = HmacAlgo },
  Timestamp) ->
    %%
    KeyLen = byte_size(Key),
    IVSaltLen = byte_size(IVSalt),
    #keypair{ public = PubKeyA } = KeyPair = get_new_keypair(),
    SharedSecret = compute_shared_secret(KeyPair, PubKeyB),
    IV = <<(IVNo + Seq):48>>,
    {Key_1, <<IVSalt_1:IVSaltLen/binary, IVNo_1:48>>} =
        hmac_key_iv(
          HmacAlgo, SharedSecret, [Key, IVSalt, IV],
          KeyLen, IVSaltLen + 6),
    %%
    {encrypt_and_send_block(
       OutStream, Seq, Params,
       [?REKEY_CHUNK, PubKeyA], 1 + byte_size(PubKeyA)),
     Params#params{
       key = Key_1, iv = {IVSalt_1, IVNo_1},
       rekey_timestamp = Timestamp }}.

encrypt_and_send_block(OutStream, Seq, Params, Block, Size) ->
    {EncryptedBlock, EncryptedSize} =
        encrypt_block(Seq, Params, Block, Size),
    send_block(OutStream, EncryptedBlock, EncryptedSize).

encrypt_block(
  Seq,
  #params{
     aead_cipher = AeadCipher,
     iv = {IVSalt, IVNo}, key = Key, tag_len = TagLen },
  Block, Size) ->
    %%
    EncryptedSize = Size + TagLen,
    AAD = <<Seq:32, EncryptedSize:32>>,
    IVBin = <<IVSalt/binary, (IVNo + Seq):48>>,
    {Ciphertext, CipherTag} =
        crypto:crypto_one_time_aead(
          AeadCipher, Key, IVBin, Block, AAD, TagLen, true),
    EncryptedBlock = [Ciphertext, CipherTag],
    {EncryptedBlock, EncryptedSize}.

%% Send packet=2
%%
send_block(OutStream, Block, Size) ->
    Msg =
        if
            is_binary(Block) -> [<<Size:16>>, Block];
            is_list(Block)   -> [<<Size:16>> | Block]
        end,
    (hd(OutStream))(OutStream, Msg).


%% -------------------------------------------------------------------------

recv_and_decrypt_chunk(InStream, SeqParams = [Seq | Params]) ->
    case recv_block(InStream) of
        Result = [closed | _] ->
            {Result, SeqParams};
        [Ciphertext | InStream_1] ->
            case decrypt_block(Seq, Params, Ciphertext) of
                <<?DATA_CHUNK, DataChunk/binary>> ->
                    {[DataChunk | InStream_1], [Seq + 1 | Params]};
                <<?TICK_CHUNK, _/binary>> ->
                    {[<<>> | InStream_1], [Seq + 1 | Params]};
                <<?REKEY_CHUNK, RekeyChunk>> ->
                    case decrypt_rekey(Params, RekeyChunk) of
                        Params_1 = #params{} ->
                            recv_and_decrypt_chunk(
                              InStream_1, [0 | Params_1]);
                        Problem when is_atom(Problem) ->
                            error_report(
                              [?FUNCTION_NAME, {reason, Problem}]),
                            {[closed | InStream_1], [Seq + 1 | Params]}
                    end;
                <<_UnknownChunk/binary>> ->
                    error_report([?FUNCTION_NAME, {reason, unknown_chunk}]),
                    {[closed | InStream_1], [Seq + 1 | Params]};
                Problem when is_atom(Problem) ->
                    error_report([?FUNCTION_NAME, {reason, Problem}]),
                    {[closed | InStream_1], [Seq + 1 | Params]}
            end
    end.

%% Non-optimized receive packet=2
%%
recv_block(InStream) ->
    Result_1 = [Data | InStream_1] = (hd(InStream))(InStream, 2),
    if
        Data =:= closed ->
            Result_1;
        is_binary(Data) ->
            recv_block(InStream_1, Data);
        is_list(Data) ->
            recv_block(InStream_1, iolist_to_binary(Data))
    end.
%%
recv_block(InStream, <<0:16>>) ->
    [<<>> | InStream];
recv_block(InStream, <<Size:16>>) ->
    case (hd(InStream))(InStream, Size) of
        [Data | InStream_1] when is_list(Data) ->
            [iolist_to_binary(Data) | InStream_1];
        Result = [Data | _]
          when is_binary(Data);
               Data =:= closed ->
            Result
    end.

decrypt_block(
  Seq, #params{ rekey_count = RekeyCount }, _Ciphertext)
  when RekeyCount =:= Seq ->
    %% This was one chunk too many without rekeying
    rekey_overdue;
decrypt_block(
  Seq,
  #params{
     aead_cipher = AeadCipher,
     iv = {IVSalt, IVNo}, key = Key, tag_len = TagLen },
  Ciphertext) ->
    %%
    CiphertextSize = byte_size(Ciphertext),
    if
        CiphertextSize < TagLen ->
            decrypt_short_block;
        true ->
            AAD = <<Seq:32, CiphertextSize:32>>,
            IV = <<IVSalt/binary, (IVNo + Seq):48>>,
            Size = CiphertextSize - TagLen,
            <<EncryptedBlock:Size/binary, CipherTag:TagLen/binary>> =
                Ciphertext,
            case
                crypto:crypto_one_time_aead(
                  AeadCipher, Key, IV, EncryptedBlock, AAD, CipherTag,
                  false)
            of
                Block when is_binary(Block) ->
                    Block;
                error ->
                    decrypt_error
            end
    end.

decrypt_rekey(
  Params =
      #params{
         iv = IV,
         key = Key,
         rekey_key = #keypair{public = PubKeyA} = KeyPair,
         hmac_algorithm = HmacAlgorithm},
  RekeyChunk) ->
    %%
    PubKeyLen = byte_size(PubKeyA),
    case RekeyChunk of
        <<PubKeyB:PubKeyLen/binary>> ->
            SharedSecret = compute_shared_secret(KeyPair, PubKeyB),
            KeyLen = byte_size(Key),
            IVLen = byte_size(IV),
            IVSaltLen = IVLen - 6,
            {Key_1, <<IVSalt_1:IVSaltLen/binary, IVNo_1:48>>} =
                hmac_key_iv(
                  HmacAlgorithm, SharedSecret, [Key, IV], KeyLen, IVLen),
            Params#params{
              iv = {IVSalt_1, IVNo_1},
              key = Key_1 };
        _ ->
            decrypt_bad_rekey_chunk
    end.


%% -------------------------------------------------------------------------
ktls_info(
  {[RecvSeq |
    #params{
       iv = {RecvSalt, RecvIV},
       key = RecvKey,
       aead_cipher = ?CIPHER } = _RecvParams],
   [SendSeq |
    #params{
       iv = {SendSalt, SendIV},
       key = SendKey,
       aead_cipher = ?CIPHER } = _SendParams]}) ->
    %%
    RecvState =
        #cipher_state{
           key = <<RecvKey/bytes>>,
           iv = <<RecvSalt/bytes, (RecvIV + RecvSeq):48>> },
    SendState =
        #cipher_state{
           key = <<SendKey/bytes>>,
           iv = <<SendSalt/bytes, (SendIV + SendSeq):48>> },
    #{ tls_version => ?TLS_VERSION,
       cipher_suite => ?CIPHER_SUITE,
       read_state => RecvState,
       read_seq => RecvSeq,
       write_state => SendState,
       write_seq => SendSeq }.

%% Dummy cipher parameters to use when checking if kTLS is supported.
%% Completely random to avoid accidentally creating an unsafe connection.
%%
ktls_info() ->
    #params{ iv = IVLen, key = KeyLen } = params(),
    <<RecvSeq:48, RecvKey:KeyLen/bytes, RecvIV:IVLen/bytes>> =
        crypto:strong_rand_bytes(6 + KeyLen + IVLen),
    <<SendSeq:48, SendKey:KeyLen/bytes, SendIV:IVLen/bytes>> =
        crypto:strong_rand_bytes(6 + KeyLen + IVLen),
    RecvState = #cipher_state{ key = RecvKey, iv = RecvIV },
    SendState = #cipher_state{ key = SendKey, iv = SendIV },
    #{ tls_version => ?TLS_VERSION,
       cipher_suite => ?CIPHER_SUITE,
       read_state => RecvState,
       read_seq => RecvSeq,
       write_state => SendState,
       write_seq => SendSeq }.


%% -------------------------------------------------------------------------
-ifdef(undefined).
-define(RECORD_TO_MAP(Name),
        record_to_map(Record) when element(1, (Record)) =:= (Name) ->
               record_to_map(record_info(fields, Name), Record, 2, #{})).
?RECORD_TO_MAP(params).
%%
record_to_map([Field | Fields], Record, Index, Map) ->
    record_to_map(
      Fields, Record, Index + 1,
      Map#{ Field => element(Index, Record) });
record_to_map([], _Record, _Index, Map) ->
    Map.
-endif.

timestamp() ->
    erlang:monotonic_time(second).


error_report(Report) ->
    error_logger:error_report(Report).

-ifdef(undefined).
info_report(Report) ->
    error_logger:info_report(Report).
-endif.

%% Trace point
trace(Term) -> Term.
