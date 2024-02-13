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
%% Module for encrypted Erlang protocol - a minimal encrypted
%% distribution protocol based on only a shared secret
%% and the crypto application, over a Stream
%%
-module(dist_cryptcookie).

%% inet_epmd_* API
-export([protocol/0,
         start_dist_ctrl/1,
         controlling_process/2,
         hs_data/2]).

-include_lib("kernel/include/dist_util.hrl").

%% ------------------------------------------------------------
protocol() ->
    cryptcookie:start_keypair_server().

%% ------------------------------------------------------------
start_dist_ctrl({Stream, ChunkSize, CipherState}) ->
    ControllingProcess = self(),
    Tag = make_ref(),
    Pid =
        spawn_opt(
          fun () ->
                  receive
                      {Tag, Alias, {start, Stream_2}} ->
                          reply(Alias, trace(started)),
                          handshake(
                            Stream_2, Tag, ControllingProcess, ChunkSize,
                            CipherState)
                  end
          end,
          [link,
           {priority, max},
           {message_queue_data, off_heap},
           {fullsweep_after, 0}]),
    ControllingProcessFun = element(3, Stream),
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
%% net_kernel distribution handshake in progress
%%

handshake(
  Stream, Tag, ControllingProcess, ChunkSize,
  {DecryptState, EncryptState} = _CipherState) ->
    handshake(
      Stream, Tag, ControllingProcess,
      ChunkSize, DecryptState, EncryptState).
%%
handshake(
  Stream = {InStream, OutStream, ControllingProcessFun},
  Tag, ControllingProcess, ChunkSize, DecryptState, EncryptState) ->
    receive
        {Tag, From, {controlling_process, NewControllingProcess}} ->
            link(NewControllingProcess),
            unlink(ControllingProcess),
            reply(From, ok),
            handshake(
              Stream, Tag, NewControllingProcess,
              ChunkSize, DecryptState, EncryptState);
        {Tag, From, {handshake_complete, DistHandle}} ->
            InputHandler =
                spawn_opt(
                  fun () ->
                          link(ControllingProcess),
                          receive
                              {Tag, dist_handle, DistHandle, InStream_2} ->
                                  input_handler_start(
                                    InStream_2, DecryptState, DistHandle)
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
            output_handler_start(
              OutStream_1, EncryptState, ChunkSize, DistHandle);
        %%
        {Tag, From, {send, Data}} ->
            {OutStream_1, EncryptState_1} =
                cryptcookie:encrypt_and_send_chunk(
                  OutStream, EncryptState, Data, iolist_size(Data)),
            if
                hd(OutStream_1) =:= closed ->
                    reply(From, {error, closed}),
                    death_row({send, trace(closed)});
                true ->
                    reply(From, ok),
                    handshake(
                      setelement(2, Stream, OutStream_1),
                      Tag, ControllingProcess,
                      ChunkSize, DecryptState, EncryptState_1)
            end;
        {Tag, From, recv} ->
            {[Data | InStream_1], DecryptState_1} =
                cryptcookie:recv_and_decrypt_chunk(InStream, DecryptState),
            if
                Data =:= closed ->
                    reply(From, {error, Data}),
                    death_row({recv, trace(Data)});
                true ->
                    reply(From, {ok, binary_to_list(Data)}),
                    handshake(
                      setelement(1, Stream, InStream_1),
                      Tag, ControllingProcess,
                      ChunkSize, DecryptState_1, EncryptState)
            end;
        %%
        Alien ->
            _ = trace(Alien),
            handshake(
              Stream, Tag, ControllingProcess,
              ChunkSize, DecryptState, EncryptState)
    end.


%% -------------------------------------------------------------------------
%% Output handler process
%%
%% Await an event about what to do; fetch dist data from the VM,
%% or send a dist tick.
%%
%% In case we are overloaded we could get many accumulated
%% dist_tick messages; make sure to flush all of them
%% before proceeding with what to do.  But, do not use selective
%% receive since that does not perform well when there are
%% many messages in the process mailbox.

%% Entry function
output_handler_start(OutStream, EncryptState, ChunkSize, DistHandle) ->
    try
        erlang:dist_ctrl_get_data_notification(DistHandle),
        output_handler(OutStream, EncryptState, [ChunkSize|DistHandle])
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
%% Awaiting outbound data or tick
%%
output_handler(OutStream, EncryptState, CS_DH) ->
    receive
        Msg ->
            case Msg of
                dist_data ->
                    output_handler_data(
                      OutStream, EncryptState, CS_DH);
                dist_tick ->
                    output_handler_tick(
                      OutStream, EncryptState, CS_DH);
                _ ->
                    %% Ignore
                    _ = trace(Msg),
                    output_handler(OutStream, EncryptState, CS_DH)
            end
    end.

%% We have received at least one dist_tick but no dist_data message
%%
output_handler_tick(OutStream, EncryptState, CS_DH) ->
    receive
        Msg ->
            case Msg of
                dist_data ->
                    output_handler_data(
                      OutStream, EncryptState, CS_DH);
                dist_tick ->
                    %% Consume all dist_tick messages
                    %%
                    output_handler_tick(OutStream, EncryptState, CS_DH);
                _ ->
                    %% Ignore
                    _ = trace(Msg),
                    output_handler_tick(OutStream, EncryptState, CS_DH)
            end
    after 0 ->
            {OutStream_1, EncryptState_1} =
                cryptcookie:encrypt_and_send_chunk(
                  OutStream, EncryptState, <<>>, 0),
            if
                hd(OutStream_1) =:= closed ->
                    death_row({send_tick, trace(closed)});
                true ->
                    output_handler(OutStream_1, EncryptState_1, CS_DH)
            end
    end.

%% We have received a dist_data notification
%%
output_handler_data(OutStream, EncryptState, CS_DH) ->
    {OutStream_1, EncryptState_1} =
        output_handler_xfer(OutStream, EncryptState, CS_DH, [], 0, []),
    erlang:dist_ctrl_get_data_notification(tl(CS_DH)),
    output_handler(OutStream_1, EncryptState_1, CS_DH).

%% Get outbound data from VM; encrypt and send,
%% until the VM has no more
%%
%% Front,Size,Rear is an Okasaki queue of binaries with total byte Size
%%
output_handler_xfer(
  OutStream, EncryptState, CS_DH, Front, Size, Rear)
  when hd(CS_DH) =< Size ->
    %%
    %% We have a full chunk or more
    %% -> collect one chunk or less and send
    output_handler_collect(
      OutStream, EncryptState, CS_DH, Front, Size, Rear);
output_handler_xfer(
  OutStream, EncryptState, CS_DH, Front, Size, Rear) ->
    %% when Size < hd(CS_DH) ->
    %%
    %% We do not have a full chunk -> try to fetch more from VM
    case erlang:dist_ctrl_get_data(tl(CS_DH)) of
        none ->
            if
                Size =:= 0 ->
                    %% No more data from VM, nothing buffered
                    %% -> done, for now
                    {OutStream, EncryptState};
                true ->
                    %% The VM had no more -> send what we have
                    output_handler_collect(
                      OutStream, EncryptState, CS_DH, Front, Size, Rear)
            end;
        {Len,Iov} ->
            output_handler_enq(
              OutStream, EncryptState, CS_DH,
              Front, Size + 4 + Len, [<<Len:32>>|Rear],
              Iov)
    end.

%% Enqueue VM data while splitting large binaries into
%% chunk size; hd(CS_DH)
%%
output_handler_enq(
  OutStream, EncryptState, CS_DH, Front, Size, Rear, []) ->
    output_handler_xfer(
      OutStream, EncryptState, CS_DH, Front, Size, Rear);
output_handler_enq(
  OutStream, EncryptState, CS_DH, Front, Size, Rear, [Bin|Iov]) ->
    output_handler_enq(
      OutStream, EncryptState, CS_DH, Front, Size, Rear, Iov, Bin).
%%
output_handler_enq(
  OutStream, EncryptState, CS_DH, Front, Size, Rear, Iov, Bin) ->
    BinSize = byte_size(Bin),
    ChunkSize = hd(CS_DH),
    if
        BinSize =< ChunkSize ->
            output_handler_enq(
              OutStream, EncryptState, CS_DH, Front, Size, [Bin|Rear],
              Iov);
        true ->
            <<Bin1:ChunkSize/binary, Bin2/binary>> = Bin,
            output_handler_enq(
              OutStream, EncryptState, CS_DH, Front, Size, [Bin1|Rear],
              Iov, Bin2)
    end.

%% Collect small binaries into chunks of at most
%% chunk size; hd(CS_DH)
%%
output_handler_collect(OutStream, EncryptState, CS_DH, [], Zero, []) ->
    0 = Zero, % ASSERT
    %% No more enqueued -> try to get more form VM
    output_handler_xfer(OutStream, EncryptState, CS_DH, [], Zero, []);
output_handler_collect(OutStream, EncryptState, CS_DH, Front, Size, Rear) ->
    output_handler_collect(
      OutStream, EncryptState, CS_DH, Front, Size, Rear, [], 0).
%%
output_handler_collect(
  OutStream, EncryptState, CS_DH, [], Zero, [], Acc, DataSize) ->
    0 = Zero, % ASSERT
    output_handler_chunk(
      OutStream, EncryptState, CS_DH, [], Zero, [], Acc, DataSize);
output_handler_collect(
  OutStream, EncryptState, CS_DH, [], Size, Rear, Acc, DataSize) ->
    %% Okasaki queue transfer Rear -> Front
    output_handler_collect(
      OutStream, EncryptState, CS_DH, lists:reverse(Rear), Size, [],
      Acc, DataSize);
output_handler_collect(
  OutStream, EncryptState, CS_DH, [Bin|Iov] = Front, Size, Rear,
  Acc, DataSize) ->
    ChunkSize = hd(CS_DH),
    BinSize = byte_size(Bin),
    DataSize_1 = DataSize + BinSize,
    if
        ChunkSize < DataSize_1 ->
            %% Bin does not fit in chunk -> send Acc
            output_handler_chunk(
              OutStream, EncryptState, CS_DH, Front, Size, Rear,
              Acc, DataSize);
        DataSize_1 < ChunkSize ->
            %% Chunk not full yet -> try to accumulate more
            output_handler_collect(
              OutStream, EncryptState, CS_DH, Iov, Size - BinSize, Rear,
              [Bin|Acc], DataSize_1);
        true -> % DataSize_1 == ChunkSize ->
            %% Optimize one iteration; Bin fits exactly
            %% -> accumulate and send
            output_handler_chunk(
              OutStream, EncryptState, CS_DH, Iov, Size - BinSize, Rear,
              [Bin|Acc], DataSize_1)
    end.

%% Encrypt and send a chunk
%%
output_handler_chunk(
  OutStream, EncryptState, CS_DH, Front, Size, Rear, Acc, DataSize) ->
    Data = lists:reverse(Acc),
    {OutStream_1, EncryptState_1} =
        cryptcookie:encrypt_and_send_chunk(
          OutStream, EncryptState, Data, DataSize),
    if
        hd(OutStream_1) =:= closed ->
            death_row({send_chunk, trace(closed)});
        true ->
            output_handler_collect(
              OutStream_1, EncryptState_1, CS_DH, Front, Size, Rear)
    end.


%% -------------------------------------------------------------------------
%% Input handler process
%%

%% Entry function
input_handler_start(InStream, DecryptState, DistHandle) ->
    try
        input_handler(InStream, DecryptState, DistHandle)
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
input_handler(InStream, DecryptState, DistHandle) ->
    %% Shortcut into the loop
    {InStream_1, DecryptState_1, Chunk} =
        input_chunk(InStream, DecryptState),
    input_handler(
      InStream_1, DecryptState_1, DistHandle, Chunk, [], byte_size(Chunk)).
%%
input_handler(InStream, DecryptState, DistHandle, First, Buffer, Size) ->
    %% Size is size of First + Buffer
    case First of
        <<Packet1Size:32, Packet1:Packet1Size/binary,
          Packet2Size:32, Packet2:Packet2Size/binary, Rest/binary>> ->
            erlang:dist_ctrl_put_data(DistHandle, Packet1),
            erlang:dist_ctrl_put_data(DistHandle, Packet2),
            input_handler(
              InStream, DecryptState, DistHandle,
              Rest, Buffer, Size - (8 + Packet1Size + Packet2Size));
        <<PacketSize:32, Packet:PacketSize/binary, Rest/binary>> ->
            erlang:dist_ctrl_put_data(DistHandle, Packet),
            input_handler(
              InStream, DecryptState, DistHandle,
              Rest, Buffer, Size - (4 + PacketSize));
        <<PacketSize:32, PacketStart/binary>> ->
            %% Partial packet in First
            input_handler(
              InStream, DecryptState, DistHandle,
              PacketStart, Buffer, Size - 4, PacketSize);
        Tick = <<>> ->
            erlang:dist_ctrl_put_data(DistHandle, Tick),
            if
                Buffer =:= [] ->
                    Size = 0, % ASSERT
                    input_handler(InStream, DecryptState, DistHandle);
                true ->
                    [First_1 | Buffer_1] = lists:reverse(Buffer),
                    input_handler(
                      InStream, DecryptState, DistHandle,
                      First_1, Buffer_1, Size)
            end;
        <<Bin/binary>> ->
            %% Partial header in First
            if
                4 =< Size ->
                    %% Complete header in First + Buffer
                    {First_1, Buffer_1, PacketSize} =
                        input_get_packet_size(Bin, lists:reverse(Buffer)),
                    input_handler(
                      InStream, DecryptState, DistHandle,
                      First_1, Buffer_1, Size - 4, PacketSize);
                true ->
                    %% Incomplete header received so far
                    {InStream_1, DecryptState_1, Chunk} =
                        input_chunk(InStream, DecryptState),
                    input_handler(
                      InStream_1, DecryptState_1, DistHandle,
                      Bin, [Chunk|Buffer], Size + byte_size(Chunk))
            end
    end.
%%
input_handler(
  InStream, DecryptState, DistHandle,
  PacketStart, Buffer, Size, PacketSize) ->
    %%
    %% Size is size of PacketStart + Buffer
    RestSize = Size - PacketSize,
    if
        RestSize < 0 ->
            %% Incomplete packet received so far
            {InStream_1, DecryptState_1, Chunk} =
                input_chunk(InStream, DecryptState),
            input_handler(
              InStream_1, DecryptState_1, DistHandle,
              PacketStart, [Chunk|Buffer], Size + byte_size(Chunk),
              PacketSize);
        0 < RestSize, Buffer =:= [] ->
            %% Rest data in PacketStart
            <<Packet:PacketSize/binary, Rest/binary>> = PacketStart,
            erlang:dist_ctrl_put_data(DistHandle, Packet),
            input_handler(
              InStream, DecryptState, DistHandle, Rest, [], RestSize);
        Buffer =:= [] -> % RestSize == 0, Size == 0
            %% No rest data
            erlang:dist_ctrl_put_data(DistHandle, PacketStart),
            input_handler(InStream, DecryptState, DistHandle);
        true ->
            %% Split packet from rest data
            LastBin = hd(Buffer),
            <<PacketLast:(byte_size(LastBin) - RestSize)/binary,
              Rest/binary>> = LastBin,
            Packet = [PacketStart|lists:reverse(tl(Buffer), PacketLast)],
            erlang:dist_ctrl_put_data(DistHandle, Packet),
            input_handler(
              InStream, DecryptState, DistHandle, Rest, [], RestSize)
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

input_chunk(InStream, DecryptState) ->
    {[Chunk | InStream_1], DecryptState_1} =
        cryptcookie:recv_and_decrypt_chunk(InStream, DecryptState),
    if
        is_atom(Chunk) ->
            _ = Chunk =:= closed
                orelse
                error_report(
                  [?FUNCTION_NAME,
                   {reason, Chunk}]),
            _ = trace({?FUNCTION_NAME, Chunk}),
            exit(connection_closed);
        is_binary(Chunk) ->
            {InStream_1, DecryptState_1, Chunk}
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

-ifdef(undefined).
info_report(Report) ->
    error_logger:info_report(Report).
-endif.

%% Trace point
trace(Term) -> Term.
