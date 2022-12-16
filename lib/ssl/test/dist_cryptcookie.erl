%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022. All Rights Reserved.
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
%% Module for encrypted Erlang protocol - a minimal encrypted
%% distribution protocol based on only a shared secret
%% and the crypto application, over gen_tcp and inet,
%% enabling the use of inet_backend = socket
%%
-module(dist_cryptcookie).
-feature(maybe_expr, enable).

%% inet_epmd_* API
-export([protocol/0,
         start_dist_ctrl/1,
         controlling_process/2,
         hs_data/2]).

-export([supported/0]).

-include_lib("kernel/include/dist_util.hrl").

-define(PROTOCOL, cryptcookie).

%% ------------------------------------------------------------
protocol() ->
    start_keypair_server(),
    ?PROTOCOL.

%% ------------------------------------------------------------
start_dist_ctrl(Stream) ->
    Secret = atom_to_binary(auth:get_cookie(), latin1),
    start_dist_ctrl(Stream, Secret).

start_dist_ctrl(
  Stream = {_InStream, _OutStream, ControllingProcessFun},
  Secret) ->
    %%
    ControllingProcess = self(),
    Tag = make_ref(),
    Pid =
        spawn_opt(
          fun () ->
                  receive
                      {Tag, Alias, {start, Stream_2}} ->
                            Init = init(Stream_2, Secret),
                          reply(Alias, trace(started)),
                          handshake(ControllingProcess, Tag, Init)
                  end
          end,
          [link,
           {priority, max},
           {message_queue_data, off_heap},
           {fullsweep_after, 0}]),
    Stream_1 = ControllingProcessFun(Stream, Pid),
    DistCtrlHandle = {Pid, Tag},
    started = call(DistCtrlHandle, {start, Stream_1}),
    DistCtrlHandle.


call({Pid, Tag}, Request) ->
    Ref = Alias = monitor(process, Pid, [{alias, reply_demonitor}]),
    Pid ! {Tag, Alias, Request},
    receive
        {Alias, Response} ->
            Response;
        {'DOWN', Ref, process, Pid, Reason} ->
            error({dist_ctrl, Reason})
    end.

reply(Alias, Response) when is_reference(Alias) ->
    Alias ! {Alias, Response},
    ok.

%% ------------------------------------------------------------
controlling_process(DistCtrlHandle, Pid) ->
    call(DistCtrlHandle, {controlling_process, Pid}),
    DistCtrlHandle.

%% ------------------------------------------------------------
hs_data(NetAddress, {DistCtrl, _} = DistCtrlHandle) ->
    #hs_data{
       socket = DistCtrlHandle,
       f_send =
           fun (S, Packet) when S =:= DistCtrlHandle ->
                   call(S, {send, Packet})
           end,
       f_recv =
           fun (S, 0, infinity) when S =:= DistCtrlHandle ->
                   call(S, recv)
           end,
       f_setopts_pre_nodeup = f_ok(DistCtrlHandle),
       f_setopts_post_nodeup = f_ok(DistCtrlHandle),
       f_address =
           fun (S, Node) when S =:= DistCtrlHandle ->
                   inet_epmd_dist:f_address(NetAddress, Node)
           end,
       f_getll =
           fun (S) when S =:= DistCtrlHandle ->
                   {ok, DistCtrl}
           end,
       f_handshake_complete =
           fun (S, _Node, DistHandle) when S =:= DistCtrlHandle ->
                   call(S, {handshake_complete, DistHandle})
           end,
       %%
       %%
       mf_tick =
           fun (S) when S =:= DistCtrlHandle ->
                   DistCtrl ! dist_tick
           end }.

f_ok(DistCtrlHandle) ->
    fun (S) when S =:= DistCtrlHandle -> ok end.

%% -------------------------------------------------------------------------

%% The curve choice greatly affects setup time,
%% we really want an Edwards curve but that would
%% require a very new openssl version.
%% Twisted brainpool curves (*t1) are faster than
%% non-twisted (*r1), 256 is much faster than 384,
%% and so on...
%%% -define(CURVE, brainpoolP384t1).
%%% -define(CURVE, brainpoolP256t1).
-define(CURVE, secp256r1).
-define(CIPHER, aes_gcm).
-define(HMAC, sha256).

-record(params,
        {dist_handle,
         hmac_algorithm = ?HMAC,
         aead_cipher = ?CIPHER,
         rekey_key,
         iv,
         key,
         tag_len = 16,
         rekey_count = 262144,
         rekey_time = 7200000, % 2 hours
         rekey_msg
        }).

params() ->
    #{ iv_length := IvLen, key_length := KeyLen } =
        crypto:cipher_info(?CIPHER),
    #params{ iv = IvLen, key = KeyLen }.


-record(keypair,
        {type = ecdh,
         params = ?CURVE,
         public,
         private,
         life_time = 3600000, % 1 hour
         life_count = 256 % Number of connection setups
        }).

supported() ->
    Curve = lists:member(?CURVE, crypto:supports(curves)),
    Cipher = lists:member(?CIPHER, crypto:supports(ciphers)),
    Hmac =
        lists:member(hmac, crypto:supports(macs)) andalso
        lists:member(?HMAC, crypto:supports(hashs)),
    Nosup = "Crypto does not support ",
    if
        not Curve ->
            Nosup ++ "curve " ++ atom_to_list(?CURVE);
        not Cipher ->
            Nosup ++ "cipher " ++ atom_to_list(?CIPHER);
        not Hmac ->
            Nosup ++ "HMAC " ++ atom_to_list(?HMAC);
        true ->
            ok
    end.

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
    receive Ref -> ok end.

keypair_server() ->
    keypair_server(undefined, undefined, undefined).
%%
keypair_server(
  #keypair{ life_time = LifeTime, life_count = LifeCount } = KeyPair) ->
    %% Presuming: 1 < LifeCount
    Timer =
        case LifeCount of
            1 ->
                undefined;
            _ ->
                erlang:start_timer(LifeTime, self(), discard)
        end,
    keypair_server(KeyPair, Timer, LifeCount - 1).
%%
keypair_server(_KeyPair, Timer, 0) ->
    cancel_timer(Timer),
    keypair_server();
keypair_server(KeyPair, Timer, Count) ->
    receive
        {RefAlias, get_keypair} when is_reference(RefAlias) ->
            case KeyPair of
                undefined ->
                    KeyPair_1 = generate_keypair(),
                    RefAlias ! {RefAlias, KeyPair_1},
                    keypair_server(KeyPair_1);
                #keypair{} ->
                    RefAlias ! {RefAlias, KeyPair},
                    keypair_server(KeyPair, Timer, Count - 1)
            end;
        {RefAlias, get_new_keypair} ->
            cancel_timer(Timer),
            KeyPair_1 = generate_keypair(),
            RefAlias ! {RefAlias, KeyPair_1},
            keypair_server(KeyPair_1);
        {timeout, Timer, discard} when is_reference(Timer) ->
            keypair_server()
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
    #keypair{ public = Public, private = Private }.


cancel_timer(undefined) ->
    ok;
cancel_timer(Timer) ->
    erlang_cancel_timer(Timer).

start_rekey_timer(Time) ->
    Timer = erlang:start_timer(Time, self(), rekey_time),
    {timeout, Timer, rekey_time}.

cancel_rekey_timer({timeout, Timer, rekey_time}) ->
    erlang_cancel_timer(Timer).

erlang_cancel_timer(Timer) ->
    case erlang:cancel_timer(Timer) of
        false ->
            receive
                {timeout, Timer, _} -> ok
            end;
        _RemainingTime ->
            ok
    end.

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

-define(PACKET_SIZE, (1 bsl 16)).  % 2 byte size header
%% Plenty of room for AEAD tag and chunk type
-define(CHUNK_SIZE, (?PACKET_SIZE - 256)).
%%
-define(HANDSHAKE_CHUNK, 1).
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

init(Stream = {_, OutStream, _}, Secret) ->
    #keypair{ public = PubKey } = KeyPair = get_keypair(),
    Params = params(),
    {R2, R3, Msg} = init_msg(Params, Secret, PubKey),
    OutStream_1 = init_send_chunk(OutStream, Msg, iolist_size(Msg)),
    Stream_1 = setelement(2, Stream, OutStream_1),
    init_recv(Stream_1, Params, Secret, KeyPair, R2, R3).

init_recv(
  Stream = {InStream, OutStream, _},
  Params = #params{ iv = IVLen },
  Secret, KeyPair, R2, R3) ->
    %%
    [InitMsg | InStream_1] = init_recv_chunk(InStream),
    IVSaltLen = IVLen - 6,
    try
        case init_msg(Params, Secret, KeyPair, R2, R3, InitMsg) of
            {SendParams =
                 #params{iv = <<IV2ASalt:IVSaltLen/binary, IV2ANo:48>> },
             RecvParams,
             SendStartMsg} ->
                OutStream_1 =
                    init_send_chunk(
                      OutStream, SendStartMsg, iolist_size(SendStartMsg)),
                [RecvStartMsg | InStream_2] = init_recv_chunk(InStream_1),
                RecvParams_1 =
                    #params{
                       iv = <<IV2BSalt:IVSaltLen/binary, IV2BNo:48>> } =
                    start_msg(RecvParams, R2, R3, RecvStartMsg),
                {{InStream_2, OutStream_1, element(3, Stream)},
                 SendParams#params{ iv = {IV2ASalt, IV2ANo} },
                 RecvParams_1#params{ iv = {IV2BSalt, IV2BNo} }}
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

init_send_chunk(OutStream, Chunk, Size) ->
    OutStream_1 = send_chunk(OutStream, Chunk, Size),
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

init_recv_chunk(InStream) ->
    Result = [Data | _] = recv_chunk(InStream),
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
%% net_kernel distribution handshake in progress
%%

handshake(ControllingProcess, Tag, {Stream, SendParams, RecvParams}) ->
    handshake(
      ControllingProcess, Tag, Stream, [1 | SendParams], [1 | RecvParams]).

handshake(
  ControllingProcess, Tag,
  Stream = {InStream, OutStream, ControllingProcessFun},
  SendSeqParams, RecvSeqParams) ->
    receive
        {Tag, From, {controlling_process, NewControllingProcess}} ->
            link(NewControllingProcess),
            unlink(ControllingProcess),
            reply(From, ok),
            handshake(
              NewControllingProcess, Tag, Stream,
              SendSeqParams, RecvSeqParams);
        {Tag, From, {handshake_complete, DistHandle}} ->
            InputHandler =
                spawn_opt(
                  fun () ->
                          link(ControllingProcess),
                          receive
                              {Tag, dist_handle, DistHandle, InStream_2} ->
                                  input_handler(
                                    InStream_2, RecvSeqParams,
                                    DistHandle)
                          end
                  end,
                  [link,
                   {priority, normal},
                   {message_queue_data, off_heap},
                   {fullsweep_after, 0}]),
            _ = monitor(process, InputHandler), % For the benchmark test
            {InStream_1, OutStream_1, _} =
                ControllingProcessFun(Stream, InputHandler),
            false = erlang:dist_ctrl_set_opt(DistHandle, get_size, true),
            ok = erlang:dist_ctrl_input_handler(DistHandle, InputHandler),
            InputHandler ! {Tag, dist_handle, DistHandle, InStream_1},
            reply(From, ok),
            process_flag(priority, normal),
            output_handler(OutStream_1, SendSeqParams, DistHandle);
        %%
        {Tag, From, {send, Data}} ->
            {OutStream_1, SendSeqParams_1} =
                encrypt_and_send_chunk(
                  OutStream, SendSeqParams,
                  [?HANDSHAKE_CHUNK, Data], 1 + iolist_size(Data)),
            if
                hd(OutStream_1) =:= closed ->
                    reply(From, {error, closed}),
                    death_row({send, trace(closed)});
                true ->
                    reply(From, ok),
                    handshake(
                      ControllingProcess, Tag,
                      setelement(2, Stream, OutStream_1),
                      SendSeqParams_1, RecvSeqParams)
            end;
        {Tag, From, recv} ->
            {InStream_1, RecvSeqParams_1, Result} =
                recv_and_decrypt_chunk(InStream, RecvSeqParams),
            if
                Result =:= closed ->
                    reply(From, {error, Result}),
                    death_row({recv, trace(Result)});
                true ->
                    reply(From, Result),
                    handshake(
                      ControllingProcess, Tag,
                      setelement(1, Stream, InStream_1),
                      SendSeqParams, RecvSeqParams_1)
            end;
        %%
        Alien ->
            _ = trace(Alien),
            handshake(
              ControllingProcess, Tag, Stream, SendSeqParams, RecvSeqParams)
    end.

%% -------------------------------------------------------------------------
%% Output handler process
%%
%% Await an event about what to do; fetch dist data from the VM,
%% send a dist tick, or rekey outbound encryption parameters.
%%
%% In case we are overloaded and could get many accumulated
%% dist_data or dist_tick messages; make sure to flush all of them
%% before proceeding with what to do.  But, do not use selective
%% receive since that does not perform well when there are
%% many messages in the process mailbox.

%% Entry function
output_handler(OutStream, [Seq | Params], DistHandle) ->
    try
        _ = crypto:rand_seed_alg(crypto_cache),
        erlang:dist_ctrl_get_data_notification(DistHandle),
        output_handler(
          OutStream,
          [Seq |
           Params#params{
             dist_handle = DistHandle,
             rekey_msg = start_rekey_timer(Params#params.rekey_time) }])
    catch
        Class : Reason : Stacktrace when Class =:= error ->
            error_report(
              [output_handler_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% Loop top
%%
%% State: lurking until any interesting message
output_handler(OutStream, SeqParams = [_ | Params]) ->
    receive
        Msg ->
            case Msg of
                dist_data ->
                    output_handler_data(OutStream, SeqParams);
                dist_tick ->
                    output_handler_tick(OutStream, SeqParams);
                _ when Msg =:= Params#params.rekey_msg ->
                    output_handler_rekey(OutStream, SeqParams);
                _ ->
                    %% Ignore
                    _ = trace(Msg),
                    output_handler(OutStream, SeqParams)
            end
    end.

output_handler_data(OutStream, SeqParams = [_ | Params]) ->
    {OutStream_1, SeqParams_1} =
        output_handler_xfer(OutStream, SeqParams),
    erlang:dist_ctrl_get_data_notification(Params#params.dist_handle),
    output_handler(OutStream_1, SeqParams_1).

%% State: we have received at least one dist_tick but no dist_data message
output_handler_tick(OutStream, [_ | Params] = SeqParams) ->
    receive
        Msg ->
            case Msg of
                dist_data ->
                    output_handler_data(OutStream, SeqParams);
                dist_tick ->
                    output_handler_tick(OutStream, SeqParams);
                _ when Msg =:= Params#params.rekey_msg ->
                    output_handler_rekey(OutStream, SeqParams);
                _ ->
                    %% Ignore
                    _ = trace(Msg),
                    output_handler_tick(OutStream, SeqParams)
            end
    after 0 ->
            TickSize = 7 + rand:uniform(56),
            TickData = binary:copy(<<0>>, TickSize),
            {OutStream_1, SeqParams_1} =
                encrypt_and_send_chunk(
                  OutStream, SeqParams,
                  [?TICK_CHUNK, TickData], 1 + TickSize),
            if
                hd(OutStream_1) =:= closed ->
                    death_row({send_tick, trace(closed)});
                true ->
                    output_handler(OutStream_1, SeqParams_1)
            end
    end.

output_handler_rekey(OutStream, [Seq | Params]) ->
    {OutStream_1, SeqParams_1} =
        encrypt_and_send_rekey_chunk(OutStream, Seq, Params),
    if
        hd(OutStream_1) =:= closed ->
            death_row({send_rekey, trace(closed)});
        true ->
            output_handler(OutStream_1, SeqParams_1)
    end.


%% Get outbound data from VM; encrypt and send,
%% until the VM has no more
%%
output_handler_xfer(OutStream, SeqParams) ->
    output_handler_xfer(OutStream, SeqParams, [], 0, []).
%%
%% Front,Size,Rear is an Okasaki queue of binaries with total byte Size
%%
output_handler_xfer(OutStream, SeqParams, Front, Size, Rear)
  when ?CHUNK_SIZE =< Size ->
    %%
    %% We have a full chunk or more
    %% -> collect one chunk or less and send
    output_handler_collect(OutStream, SeqParams, Front, Size, Rear);
output_handler_xfer(OutStream, SeqParams = [_ | Params], Front, Size, Rear) ->
    %% when Size < ?CHUNK_SIZE ->
    %%
    %% We do not have a full chunk -> try to fetch more from VM
    case erlang:dist_ctrl_get_data(Params#params.dist_handle) of
        none ->
            if
                Size =:= 0 ->
                    %% No more data from VM, nothing buffered
                    %% -> go back to lurking
                    {OutStream, SeqParams};
                true ->
                    %% The VM had no more -> send what we have
                    output_handler_collect(
                      OutStream, SeqParams, Front, Size, Rear)
            end;
        {Len,Iov} ->
            output_handler_enq(
              OutStream, SeqParams,
              Front, Size + 4 + Len, [<<Len:32>>|Rear], Iov)
    end.

%% Enqueue VM data while splitting large binaries into ?CHUNK_SIZE
%%
output_handler_enq(OutStream, SeqParams, Front, Size, Rear, []) ->
    output_handler_xfer(OutStream, SeqParams, Front, Size, Rear);
output_handler_enq(OutStream, SeqParams, Front, Size, Rear, [Bin|Iov]) ->
    output_handler_enq(OutStream, SeqParams, Front, Size, Rear, Iov, Bin).
%%
output_handler_enq(OutStream, SeqParams, Front, Size, Rear, Iov, Bin) ->
    BinSize = byte_size(Bin),
    if
        BinSize =< ?CHUNK_SIZE ->
            output_handler_enq(
              OutStream, SeqParams, Front, Size, [Bin|Rear], Iov);
        true ->
            <<Bin1:?CHUNK_SIZE/binary, Bin2/binary>> = Bin,
            output_handler_enq(
              OutStream, SeqParams, Front, Size, [Bin1|Rear], Iov, Bin2)
    end.

%% Collect small binaries into chunks of at most ?CHUNK_SIZE
%%
output_handler_collect(OutStream, SeqParams, [], Zero, []) ->
    0 = Zero, % Assert
    %% No more enqueued -> try to get more form VM
    output_handler_xfer(OutStream, SeqParams);
output_handler_collect(OutStream, SeqParams, Front, Size, Rear) ->
    output_handler_collect(OutStream, SeqParams, Front, Size, Rear, [], 0).
%%
output_handler_collect(OutStream, SeqParams, [], Zero, [], Acc, DataSize) ->
    0 = Zero, % Assert
    output_handler_chunk(OutStream, SeqParams, [], Zero, [], Acc, DataSize);
output_handler_collect(OutStream, SeqParams, [], Size, Rear, Acc, DataSize) ->
    %% Okasaki queue transfer Rear -> Front
    output_handler_collect(
      OutStream, SeqParams, lists:reverse(Rear), Size, [], Acc, DataSize);
output_handler_collect(
  OutStream, SeqParams, [Bin|Iov] = Front, Size, Rear, Acc, DataSize) ->
    BinSize = byte_size(Bin),
    DataSize_1 = DataSize + BinSize,
    if
        ?CHUNK_SIZE < DataSize_1 ->
            %% Bin does not fit in chunk -> send Acc
            output_handler_chunk(
              OutStream, SeqParams, Front, Size, Rear, Acc, DataSize);
        DataSize_1 < ?CHUNK_SIZE ->
            %% Chunk not full yet -> try to accumulate more
            output_handler_collect(
              OutStream, SeqParams, Iov, Size - BinSize, Rear,
              [Bin|Acc], DataSize_1);
        true -> % DataSize_1 == ?CHUNK_SIZE ->
            %% Optimize one iteration; Bin fits exactly -> accumulate and send
            output_handler_chunk(
              OutStream, SeqParams, Iov, Size - BinSize, Rear,
              [Bin|Acc], DataSize_1)
    end.

%% Encrypt and send a chunk
%%
output_handler_chunk(
  OutStream, SeqParams, Front, Size, Rear, Acc, DataSize) ->
    Data = lists:reverse(Acc),
    {OutStream_1, SeqParams_1} =
        encrypt_and_send_chunk(
          OutStream, SeqParams, [?DATA_CHUNK|Data], 1 + DataSize),
    if
        hd(OutStream_1) =:= closed ->
            death_row({send_chunk, trace(closed)});
        true ->
            output_handler_collect(
              OutStream_1, SeqParams_1, Front, Size, Rear)
    end.

%% -------------------------------------------------------------------------
%% Input handler process
%%

%% Entry function
input_handler(InStream, [Seq | Params], DistHandle) ->
    try
        input_handler(
          InStream, [Seq | Params#params{ dist_handle = DistHandle }])
    catch
        Class : Reason : Stacktrace when Class =:= error ->
            error_report(
              [input_handler_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% Loop top
input_handler(InStream, SeqParams) ->
    %% Shortcut into the loop
    {InStream_1, SeqParams_1, Data} = input_data(InStream, SeqParams),
    input_handler(InStream_1, SeqParams_1, Data, [], byte_size(Data)).
%%
input_handler(InStream, SeqParams = [_ | Params], First, Buffer, Size) ->
    %% Size is size of First + Buffer
    case First of
        <<Packet1Size:32, Packet1:Packet1Size/binary,
          Packet2Size:32, Packet2:Packet2Size/binary, Rest/binary>> ->
            DistHandle = Params#params.dist_handle,
            erlang:dist_ctrl_put_data(DistHandle, Packet1),
            erlang:dist_ctrl_put_data(DistHandle, Packet2),
            input_handler(
              InStream, SeqParams,
              Rest, Buffer, Size - (8 + Packet1Size + Packet2Size));
        <<PacketSize:32, Packet:PacketSize/binary, Rest/binary>> ->
            DistHandle = Params#params.dist_handle,
            erlang:dist_ctrl_put_data(DistHandle, Packet),
            input_handler(
              InStream, SeqParams, Rest, Buffer, Size - (4 + PacketSize));
        <<PacketSize:32, PacketStart/binary>> ->
            %% Partial packet in First
            input_handler(
              InStream, SeqParams,
              PacketStart, Buffer, Size - 4, PacketSize);
        <<Bin/binary>> ->
            %% Partial header in First
            if
                4 =< Size ->
                    %% Complete header in First + Buffer
                    {First_1, Buffer_1, PacketSize} =
                        input_get_packet_size(Bin, lists:reverse(Buffer)),
                    input_handler(
                      InStream, SeqParams,
                      First_1, Buffer_1, Size - 4, PacketSize);
                true ->
                    %% Incomplete header received so far
                    {InStream_1, SeqParams_1, More} =
                        input_data(InStream, SeqParams),
                    input_handler(
                      InStream_1, SeqParams_1,
                      Bin, [More|Buffer], Size + byte_size(More))
            end
    end.
%%
input_handler(
  InStream, SeqParams = [_ | Params], PacketStart, Buffer, Size, PacketSize) ->
    %% Size is size of PacketStart + Buffer
    RestSize = Size - PacketSize,
    if
        RestSize < 0 ->
            %% Incomplete packet received so far
            {InStream_1, SeqParams_1, More} =
                input_data(InStream, SeqParams),
            input_handler(
              InStream_1, SeqParams_1, PacketStart,
              [More|Buffer], Size + byte_size(More), PacketSize);
        0 < RestSize, Buffer =:= [] ->
            %% Rest data in PacketStart
            <<Packet:PacketSize/binary, Rest/binary>> = PacketStart,
            DistHandle = Params#params.dist_handle,
            erlang:dist_ctrl_put_data(DistHandle, Packet),
            input_handler(InStream, SeqParams, Rest, [], RestSize);
        Buffer =:= [] -> % RestSize == 0
            %% No rest data
            DistHandle = Params#params.dist_handle,
            erlang:dist_ctrl_put_data(DistHandle, PacketStart),
            input_handler(InStream, SeqParams);
        true ->
            %% Split packet from rest data
            LastBin = hd(Buffer),
            <<PacketLast:(byte_size(LastBin) - RestSize)/binary,
              Rest/binary>> = LastBin,
            Packet = [PacketStart|lists:reverse(tl(Buffer), PacketLast)],
            DistHandle = Params#params.dist_handle,
            erlang:dist_ctrl_put_data(DistHandle, Packet),
            input_handler(InStream, SeqParams, Rest, [], RestSize)
    end.

input_get_packet_size(First, [Bin|Buffer]) ->
    MissingSize = 4 - byte_size(First),
    if
        MissingSize =< byte_size(Bin) ->
            <<Last:MissingSize/binary, Rest/binary>> = Bin,
            <<PacketSize:32>> = <<First/binary, Last/binary>>,
            {Rest, lists:reverse(Buffer), PacketSize};
        true ->
            input_get_packet_size(<<First/binary, Bin/binary>>, Buffer)
    end.

input_data(InStream, [Seq | Params]) ->
    [EncryptedChunk | InStream_1] = recv_chunk(InStream),
    if
        EncryptedChunk =:= closed ->
            info_report(
              [?FUNCTION_NAME,
               {reason, closed}]),
            exit(connection_closed);
        true ->
            case decrypt_chunk(Seq, Params, EncryptedChunk) of
                <<?DATA_CHUNK, Data/binary>> ->
                    {InStream_1, [Seq + 1 | Params], Data};
                <<?TICK_CHUNK, _Dummy/binary>> ->
                    DistHandle = Params#params.dist_handle,
                    erlang:dist_ctrl_put_data(DistHandle, <<>>),
                    input_data(InStream_1, [Seq + 1 | Params]);
                <<UnknownChunk/binary>> ->
                    _ = trace(UnknownChunk),
                    error_report([?FUNCTION_NAME, {reason, unknown_chunk}]),
                    exit(connection_closed);
                #params{} = Params_1 -> % Rekey
                    input_data(InStream_1, [0 | Params_1]);
                error ->
                    _ = trace(decrypt_error),
                    exit(connection_closed)
            end
    end.

%% -------------------------------------------------------------------------
%% Encryption and decryption helpers

encrypt_and_send_chunk(
  OutStream,
  [Seq |
   Params = #params{ rekey_count = RekeyCount, rekey_msg = RekeyMsg }],
  Chunk, Size)
  when RekeyCount =:= Seq ->
    %%
    cancel_rekey_timer(RekeyMsg),
    Result_1 = {OutStream_1, SeqParams_1} =
        encrypt_and_send_rekey_chunk(OutStream, Seq, Params),
    if
        hd(OutStream_1) =:= closed ->
            Result_1;
        true ->
            encrypt_and_send_chunk(
              OutStream_1, SeqParams_1, Chunk, Size)
    end;
encrypt_and_send_chunk(OutStream, [Seq | Params], Chunk, Size) ->
    {EncryptedChunk, EncryptedSize} =
        encrypt_chunk(Seq, Params, Chunk, Size),
    {send_chunk(OutStream, EncryptedChunk, EncryptedSize),
     [Seq + 1 | Params]}.

encrypt_and_send_rekey_chunk(
  OutStream, Seq,
  Params =
      #params{
         rekey_key = PubKeyB,
         key = Key,
         iv = {IVSalt, IVNo},
         hmac_algorithm = HmacAlgo }) ->
    %%
    KeyLen = byte_size(Key),
    IVSaltLen = byte_size(IVSalt),
    #keypair{ public = PubKeyA } = KeyPair = get_new_keypair(),
    {EncryptedChunk, EncryptedSize} =
        encrypt_chunk(
          Seq, Params, [?REKEY_CHUNK, PubKeyA], 1 + byte_size(PubKeyA)),
    SharedSecret = compute_shared_secret(KeyPair, PubKeyB),
    IV = <<(IVNo + Seq):48>>,
    {Key_1, <<IVSalt_1:IVSaltLen/binary, IVNo_1:48>>} =
        hmac_key_iv(
          HmacAlgo, SharedSecret, [Key, IVSalt, IV],
          KeyLen, IVSaltLen + 6),
    {send_chunk(OutStream, EncryptedChunk, EncryptedSize),
     [0 |
      Params#params{
        key = Key_1, iv = {IVSalt_1, IVNo_1},
        rekey_msg = start_rekey_timer(Params#params.rekey_time) }]}.

encrypt_chunk(
  Seq,
  #params{
     aead_cipher = AeadCipher,
     iv = {IVSalt, IVNo}, key = Key, tag_len = TagLen },
  Chunk, Size) ->
    %%
    EncryptedSize = Size + TagLen,
    AAD = <<Seq:32, EncryptedSize:32>>,
    IVBin = <<IVSalt/binary, (IVNo + Seq):48>>,
    {Ciphertext, CipherTag} =
        crypto:crypto_one_time_aead(
          AeadCipher, Key, IVBin, Chunk, AAD, TagLen, true),
    EncryptedChunk = [Ciphertext, CipherTag],
    {EncryptedChunk, EncryptedSize}.

send_chunk(OutStream, Chunk, Size) ->
    Msg =
        if
            is_binary(Chunk) -> [<<Size:16>>, Chunk];
            is_list(Chunk)   -> [<<Size:16>> | Chunk]
        end,
    (hd(OutStream))(OutStream, Msg).



recv_and_decrypt_chunk(InStream, SeqParams = [Seq | Params]) ->
    case recv_chunk(InStream) of
        [closed | InStream_1] ->
            {InStream_1, SeqParams, closed};
        [EncryptedChunk | InStream_1] ->
            case decrypt_chunk(Seq, Params, EncryptedChunk) of
                <<?HANDSHAKE_CHUNK, Cleartext/binary>> ->
                    {InStream_1, [Seq + 1 | Params],
                     {ok, binary_to_list(Cleartext)}};
                UnknownChunk when is_binary(UnknownChunk) ->
                    {InStream_1, [Seq + 1 | Params], {error, unknown_chunk}};
                #params{} = Params_1 -> % Rekey
                    recv_and_decrypt_chunk(InStream_1, [0 | Params_1]);
                error ->
                    {InStream_1, SeqParams, {error, decrypt_error}}
            end
    end.

%% This should be optimized to use stateful (buffering)
%% de-packeting of the 2 byte length header packets
%% to avoid always having to do two recv per packet
%%
recv_chunk(InStream) ->
    Result_1 = [Data | InStream_1] = (hd(InStream))(InStream, 2),
    if
        Data =:= closed ->
            Result_1;
        is_binary(Data) ->
            recv_chunk(InStream_1, Data);
        is_list(Data) ->
            recv_chunk(InStream_1, iolist_to_binary(Data))
    end.
%%
recv_chunk(InStream, <<Size:16>>) ->
    case (hd(InStream))(InStream, Size) of
        [Data | InStream_1] when is_list(Data) ->
            [iolist_to_binary(Data) | InStream_1];
        Result = [Data | _]
          when is_binary(Data);
               Data =:= closed ->
            Result
    end.

decrypt_chunk(Seq, #params{ rekey_count = RekeyCount }, _EncryptedChunk)
  when RekeyCount =:= Seq ->
    %% This was one chunk too many without rekeying
    error_report([?FUNCTION_NAME, {reason,rekey_overdue}]),
    error;
decrypt_chunk(
  Seq,
  Params =
      #params{
         aead_cipher = AeadCipher,
         iv = {IVSalt, IVNo}, key = Key, tag_len = TagLen },
  EncryptedChunk) ->
    %%
    EncryptedSize = byte_size(EncryptedChunk),
    if
        EncryptedSize < TagLen ->
            error_report(
              [?FUNCTION_NAME,
               {reason,short_chunk}]),
            error;
        true ->
            AAD = <<Seq:32, EncryptedSize:32>>,
            IV = <<IVSalt/binary, (IVNo + Seq):48>>,
            Size = EncryptedSize - TagLen,
            <<Ciphertext:Size/binary, CipherTag:TagLen/binary>> =
                EncryptedChunk,
            case
                crypto:crypto_one_time_aead(
                  AeadCipher, Key, IV, Ciphertext, AAD, CipherTag, false)
            of
                <<?REKEY_CHUNK, RekeyData/binary>> ->
                    decrypt_chunk_rekey(IV, Params, RekeyData);
                Chunk when is_binary(Chunk) ->
                    Chunk;
                error ->
                    error_report(
                      [?FUNCTION_NAME,
                       {reason,{decrypt_error,Seq}}]),
                    error
            end
    end.

decrypt_chunk_rekey(
  IV,
  Params =
      #params{
         key = Key,
         rekey_key = #keypair{public = PubKeyA} = KeyPair },
  RekeyData) ->
    %%
    PubKeyLen = byte_size(PubKeyA),
    case RekeyData of
        <<PubKeyB:PubKeyLen/binary>> ->
            SharedSecret = compute_shared_secret(KeyPair, PubKeyB),
            KeyLen = byte_size(Key),
            IVLen = byte_size(IV),
            IVSaltLen = IVLen - 6,
            {Key_1, <<IVSalt_1:IVSaltLen/binary, IVNo_1:48>>} =
                hmac_key_iv(
                  Params#params.hmac_algorithm,
                  SharedSecret, [Key, IV], KeyLen, IVLen),
            Params#params{
              iv = {IVSalt_1, IVNo_1},
              key = Key_1 };
        _ ->
            error_report(
              [?FUNCTION_NAME,
               {reason,bad_rekey_chunk}]),
            error
    end.

%% -------------------------------------------------------------------------

%% Wait for getting killed by process link,
%% and if that does not happen - drop dead

death_row(Reason) ->
    receive
    after 5000 ->
            death_row_timeout(Reason)
    end.

death_row_timeout(Reason) ->
    error_report(
      [?FUNCTION_NAME,
       {reason, Reason},
       {pid, self()}]),
    exit(Reason).

%% -------------------------------------------------------------------------

error_report(Report) ->
    error_logger:error_report(Report).

info_report(Report) ->
    error_logger:info_report(Report).

%% Trace point
trace(Term) -> Term.
