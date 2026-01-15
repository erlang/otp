%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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

-module(inet_epmd_socket).
-moduledoc false.

%% DistMod API
-export([net_address/0, listen_open/2, listen_port/3, listen_close/1,
         accept_open/2, accept_controller/3, accepted/3,
         connect/3]).

-export([check_ip/2, supported/0]).

-export([start_dist_ctrl/2]).

-include("net_address.hrl").
-include("dist.hrl").
-include("dist_util.hrl").

-define(PROTOCOL, tcp).
-define(FAMILY, inet).

%% ------------------------------------------------------------
net_address() ->
    #net_address{
       protocol = ?PROTOCOL,
       family = ?FAMILY }.

%% ------------------------------------------------------------
listen_open(#net_address{ family = Family}, ListenOptions) ->
    maybe
        Key = backlog,
        Default = 128,
        Backlog = proplists:get_value(Key, ListenOptions, Default),
        {ok, ListenSocket} ?=
            socket:open(Family, stream),
        ok ?=
            setopts(
              ListenSocket,
              [inet_epmd_dist:nodelay() |
               proplists:delete(
                 Key,
                 proplists:delete(nodelay, ListenOptions))]),
        {ok, {ListenSocket, Backlog}}
    else
        {error, _} = Error ->
            Error
    end.

setopts(Socket, Options) ->
    gen_tcp_socket:socket_setopts(Socket, Options).

%% ------------------------------------------------------------
listen_port(
  #net_address{ family = Family }, Port, {ListenSocket, Backlog}) ->
    maybe
        Sockaddr =
            #{family => Family,
              addr => any,
              port => Port},
        ok ?=
            socket:bind(ListenSocket, Sockaddr),
        ok ?=
            socket:listen(ListenSocket, Backlog),
        {ok, #{ addr := Ip, port := ListenPort}} ?=
            socket:sockname(ListenSocket),
        {ok, {ListenSocket, {Ip, ListenPort}}}
    else
        {error, _} = Error ->
            Error
    end.

%% ------------------------------------------------------------
listen_close(ListenSocket) ->
    socket:close(ListenSocket).

%% ------------------------------------------------------------
accept_open(_NetAddress, ListenSocket) ->
    maybe
        {ok, Socket} ?=
            socket:accept(ListenSocket),
        {ok, #{ addr := Ip }} ?=
            socket:sockname(Socket),
        {ok, #{ addr := PeerIp, port := PeerPort }} ?=
            socket:peername(Socket),
        check_ip(Ip, PeerIp),
        {Socket, {PeerIp, PeerPort}}
    else
        {error, Reason} ->
            exit({?FUNCTION_NAME, Reason})
    end.

%% ------------------------------------------------------------
accept_controller(_NetAddress, Controller, Socket) ->
    maybe
        ok ?=
            socket:setopt(Socket, {otp,controlling_process}, Controller),
        Socket
    else
        {error, Reason} ->
            exit({?FUNCTION_NAME, Reason})
    end.

%% ------------------------------------------------------------
accepted(NetAddress, _Timer, Socket) ->
    start_dist_ctrl(NetAddress, Socket).

%% ------------------------------------------------------------
connect(
  #net_address{ address = {Ip, Port}, family = Family } = NetAddress,
  _Timer, ConnectOptions) ->
    maybe
        {ok, Socket} ?=
            socket:open(Family, stream),
        ok ?=
            setopts(Socket, ConnectOptions),
        ConnectAddress =
            #{ family => Family,
               addr => Ip,
               port => Port },
        ok ?=
            socket:connect(Socket, ConnectAddress),
        start_dist_ctrl(NetAddress, Socket)
    else
        {error, _} = Error ->
            Error
    end.

%% ------------------------------------------------------------
start_dist_ctrl(NetAddress, Socket) ->
    Controller = self(),
    DistCtrlTag = make_ref(),
    DistCtrl =
        spawn_link(
          fun () ->
                  receive
                      {DistCtrlTag, handshake_complete, From, DistHandle} ->
                          Sync = make_ref(),
                          DistC = self(),
                          InputHandler =
                              spawn_link(
                                fun () ->
                                        link(Controller),
                                        DistC ! Sync,
                                        receive Sync -> ok end,
                                        input_handler_start(
                                          Socket, DistHandle)
                                end),
                          false =
                              erlang:dist_ctrl_set_opt(
                                DistHandle, get_size, true),
                          ok =
                              erlang:dist_ctrl_input_handler(
                                DistHandle, InputHandler),
                          receive Sync -> InputHandler ! Sync end,
                          From ! {DistCtrlTag, handshake_complete}, % Reply
                          output_handler_start(Socket, DistHandle)
                  end
          end),
    #hs_data{
       socket = Socket,
       f_send =
           fun (S, Packet) when S =:= Socket ->
                   send_packet_2(S, Packet)
           end,
       f_recv =
           fun (S, 0, infinity) when S =:= Socket ->
                   recv_packet_2(S)
           end,
       f_setopts_pre_nodeup = f_ok(Socket),
%%%            fun (S) when S =:= Socket ->
%%%                    socket:setopt(S, {otp,debug}, true)
%%%            end,
       f_setopts_post_nodeup = f_ok(Socket),
       f_address =
           fun (S, Node) when S =:= Socket ->
                   inet_epmd_dist:f_address(NetAddress, Node)
           end,
       f_getll =
           fun (S) when S =:= Socket ->
                   {ok, DistCtrl}
           end,

       f_handshake_complete =
           fun (S, _Node, DistHandle) when S =:= Socket ->
                   handshake_complete(DistCtrl, DistCtrlTag, DistHandle)
           end,

       mf_tick =
           fun (S) when S =:= Socket ->
                   tick(DistCtrl)
           end }.

%%%        mf_getstat =
%%%            fun (S) when S =:= Socket ->
%%%                    getstat(S)
%%%            end,
%%%        mf_setopts = mf_setopts(Socket),
%%%        mf_getopts = mf_getopts(Socket) }.

send_packet_2(Socket, Packet) ->
    Size = iolist_size(Packet),
    true = Size < 1 bsl 16,
    socket:send(Socket, [<<Size:16>>, Packet]).

recv_packet_2(Socket) ->
    maybe
        {ok, <<Size:16>>} ?=
            socket:recv(Socket, 2),
        {ok, Data} ?=
            socket:recv(Socket, Size),
        {ok, binary_to_list(Data)}
    else
        {error, _} = Error ->
            Error
    end.

f_ok(Socket) ->
    fun (S) when S =:= Socket ->
            ok
    end.

-ifdef(undefined).
getstat(S) ->
    #{ counters :=
          #{ read_pkg := ReadPkg,
             write_pkg := WritePkg } } = socket:info(S),
    %% Ignoring that the counters may wrap since dist_util
    %% only looks for changing values anyway
    {ok, [{recv_cnt, ReadPkg}, {send_cnt, WritePkg}, {send_pend, 0}]}.

mf_setopts(Socket) ->
    f_ok(Socket).

mf_getopts(Socket) ->
    fun (S, Opts) when S =:= Socket, is_list(Opts) ->
            {ok, []}
    end.
-endif.

handshake_complete(DistCtrl, DistCtrlTag, DistHandle) ->
    DistCtrl ! {DistCtrlTag, handshake_complete, self(), DistHandle},
    receive
        {DistCtrlTag, handshake_complete} ->
            ok
    end.

tick(DistCtrl) ->
    DistCtrl ! dist_tick,
    ok.

%% ------------------------------------------------------------
-record(ohp, %% Output Handler Parameters
        {socket, dist_handle, watermark}).

-spec output_handler_start(_, _) -> no_return(). % Server loop
output_handler_start(Socket, DistHandle) ->
    try
        erlang:dist_ctrl_get_data_notification(DistHandle),
        {ok, SndbufSize} = socket:getopt(Socket, {socket,sndbuf}),
        OHP =
            #ohp{
               socket      = Socket,
               dist_handle = DistHandle,
               watermark   = SndbufSize bsr 1},
        Buffer             = [],
        Size               = 0,
        DistData           = false,
        SelectInfo         = undefined,
        output_handler(OHP, Buffer, Size, DistData, SelectInfo)
    catch
        Class : Reason : Stacktrace when Class =:= error ->
            error_logger:error_report(
              [output_handler_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

output_handler(OHP, Buffer, Size, DistData, SelectInfo) ->
    if
        DistData, OHP#ohp.watermark > Size ->
            %% There is dist_data from the emulator,
            %% and we have buffer space for it
            output_handler_data(OHP, Buffer, Size, SelectInfo);
        SelectInfo =:= undefined, Size > 0->
            %% We are not waiting for a send to complete,
            %% and we have buffered data
            output_handler_send(OHP, Buffer, Size, DistData);
        true ->
            output_handler_wait(
              OHP, Buffer, Size, DistData, SelectInfo, infinity)
    end.

%% Wait for an external event (message)
output_handler_wait(
  #ohp{socket = Socket} = OHP, Buffer, Size, DistData, SelectInfo, Tick) ->
    receive
        dist_data ->
            output_handler(OHP, Buffer, Size, true, SelectInfo);
        dist_tick
          when SelectInfo =:= undefined, not DistData, Size == 0 ->
            %% Tick only when we don't wait for a send to complete
            %% and there is no dist_data to send,
            %% but receive all dist_tick messages first
            %% by looping with after timeout Tick = 0
            output_handler_wait(OHP, Buffer, Size, DistData, SelectInfo, 0);
        {'$socket', Socket, select, SelectHandle}
          when element(3, SelectInfo) =:= SelectHandle ->
            %% Send no longer pending; try to send again
            output_handler_send(OHP, Buffer, Size, DistData, SelectInfo);
        _ -> % Ignore
            output_handler_wait(OHP, Buffer, Size, DistData, SelectInfo, Tick)
    after Tick ->
            %% Send a tick
            Buffer   = [],      % Assert
            Size     = 0,       % Assert
            DistData = false,   % Assert
            output_handler_send(OHP, [<<0:32>>], 4, false)
    end.

%% Get dist_data from the emulator
output_handler_data(
  #ohp{dist_handle = DistHandle, watermark = Watermark} = OHP,
  Buffer, Size, SelectInfo)
  when Watermark > Size ->
    %%
    case erlang:dist_ctrl_get_data(DistHandle) of
        none ->
            erlang:dist_ctrl_get_data_notification(DistHandle),
            output_handler(OHP, Buffer, Size, false, SelectInfo);
        {Len, Iovec} ->
            %% erlang:display({Len, '==>>'}),
            Buffer_1 = lists:reverse(Iovec, [<<Len:32>> | Buffer]),
            Size_1 = Len + 4 + Size,
            output_handler_data(OHP, Buffer_1, Size_1, SelectInfo)
    end;
output_handler_data(OHP, Buffer, Size, SelectInfo) ->
    output_handler(OHP, Buffer, Size, true, SelectInfo).

%% Output data to socket
output_handler_send(OHP, Buffer, Size, DistData) ->
    output_handler_send_result(
      OHP, Buffer, Size, DistData,
      socket:sendv(OHP#ohp.socket, lists:reverse(Buffer), nowait)).

%% Output data to socket, continuation
output_handler_send(OHP, Buffer, Size, DistData, SelectInfo) ->
    output_handler_send_result(
      OHP, Buffer, Size, DistData,
      socket:sendv(OHP#ohp.socket, lists:reverse(Buffer), SelectInfo, nowait)).

output_handler_send_result(OHP, Buffer, Size, DistData, Result) ->
    case Result of
        ok ->
            output_handler(OHP, [], 0, DistData, undefined);
        {select, {SelectInfo, RestIOV}} ->
            Size_1 = iolist_size(RestIOV),
            Buffer_1  = lists:reverse(RestIOV),
            output_handler(OHP, Buffer_1, Size_1, DistData, SelectInfo);
        {select, SelectInfo} ->
            output_handler(OHP, Buffer, Size, DistData, SelectInfo);
        {error, {Reason, _RestIOV}} ->
            exit(Reason);
        {error, Reason} ->
            exit(Reason)
    end.

%% ------------------------------------------------------------
-record(ihp, %% Input Handler Parameters
        {socket, dist_handle, watermark}).

-spec input_handler_start(_, _) -> no_return(). % Server loop
input_handler_start(Socket, DistHandle) ->
    try
        ok               = socket:setopt(Socket, {otp,select_read}, true),
        {ok, RcvbufSize} = socket:getopt(Socket, {socket,rcvbuf}),
        IHP =
            #ihp{
               socket      = Socket,
               dist_handle = DistHandle,
               watermark   = RcvbufSize},
        Front              = [],
        Size               = 0,
        Rear               = [],
        CSHandle           = undefined,
        %% erlang:display({?FUNCTION_NAME, Socket, DistHandle}),
        input_handler(IHP, Front, Size, Rear, CSHandle)
    catch
        Class : Reason : Stacktrace when Class =:= error ->
            error_logger:error_report(
              [input_handler_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

input_handler(IHP, Front, Size, Rear, CSHandle)
  when IHP#ihp.watermark > Size, CSHandle =:= undefined ->
    %% erlang:display({?FUNCTION_NAME, ?LINE, Size}),
    input_handler_recv(IHP, Front, Size, Rear, CSHandle);
input_handler(IHP, [] = Front, Size, [] = Rear, CSHandle) ->
    0 = Size, % Assert
    input_handler_recv(IHP, Front, Size, Rear, CSHandle);
input_handler(IHP, [] = _Front, Size, Rear, CSHandle) ->
    %% erlang:display({?FUNCTION_NAME, ?LINE, Size}),
    input_handler(IHP, lists:reverse(Rear), Size, [], CSHandle);
input_handler(IHP, [Bin | Front] = Bin_Front, Size, Rear, CSHandle) ->
    case Bin of
        <<PacketSize_1:32, Packet_1:PacketSize_1/binary,
          PacketSize_2:32, Packet_2:PacketSize_2/binary,
          PacketSize_3:32, Packet_3:PacketSize_3/binary,
          PacketSize_4:32, Packet_4:PacketSize_4/binary,
          Rest/binary>> ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, Size, PacketSize}),
            %% 4 complete packets in Bin
            DistHandle = IHP#ihp.dist_handle,
            put_data(DistHandle, Packet_1),
            put_data(DistHandle, Packet_2),
            put_data(DistHandle, Packet_3),
            put_data(DistHandle, Packet_4),
            Size_1 =
                Size -
                (16 + PacketSize_1 + PacketSize_2 +
                     PacketSize_3 + PacketSize_4),
            if
                byte_size(Rest) > 0 ->
                    input_handler(
                      IHP, [Rest | Front], Size_1, Rear, CSHandle);
                true -> % byte_size(Rest) == 0
                    input_handler(IHP, Front, Size_1, Rear, CSHandle)
            end;
        <<PacketSize_1:32, Packet_1:PacketSize_1/binary,
          PacketSize_2:32, Packet_2:PacketSize_2/binary,
          Rest/binary>> ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, Size, PacketSize}),
            %% 2 complete packets in Bin
            DistHandle = IHP#ihp.dist_handle,
            put_data(DistHandle, Packet_1),
            put_data(DistHandle, Packet_2),
            Size_1 = Size - (8 + PacketSize_1 + PacketSize_2),
            if
                byte_size(Rest) > 0 ->
                    input_handler(
                      IHP, [Rest | Front], Size_1, Rear, CSHandle);
                true -> % byte_size(Rest) == 0
                    input_handler(IHP, Front, Size_1, Rear, CSHandle)
            end;
        <<PacketSize:32, Packet:PacketSize/binary, Rest/binary>> ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, Size, PacketSize}),
            %% Complete packet in Bin
            put_data(IHP#ihp.dist_handle, Packet),
            Size_1 = Size - (4 + PacketSize),
            if
                byte_size(Rest) > 0 ->
                    input_handler(
                      IHP, [Rest | Front], Size_1, Rear, CSHandle);
                true -> % byte_size(Rest) == 0
                    input_handler(IHP, Front, Size_1, Rear, CSHandle)
            end;
        <<PacketSize:32, PacketStart/binary>> ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, Size, PacketSize}),
            %% Incomplete packet in Bin
            Size_1 = Size - (4 + PacketSize),
            if
                0 > Size_1 ->
                    %% Incomplete packet in buffer
                    input_handler_recv(
                      IHP, Bin_Front, Size, Rear, CSHandle);
                Size_1 > 0->
                    %% Complete packet is buffered, and some more
                    PacketStartSize = byte_size(PacketStart),
                    IOV =
                        if  PacketStartSize > 0 -> [PacketStart];
                            true                -> []
                        end,
                    {Packet, Front_1, Rear_1} =
                        collect_iov(
                          IOV, Front, PacketSize - PacketStartSize, Rear),
                    put_data(IHP#ihp.dist_handle, Packet),
                    input_handler(IHP, Front_1, Size_1, Rear_1, CSHandle);
                true -> % Size_1 == 0
                    %% Exactly a packet is buffered
                    Packet = [PacketStart | Front] ++ lists:reverse(Rear),
                    put_data(IHP#ihp.dist_handle, Packet),
                    input_handler(IHP, [], 0, [], CSHandle)
            end;
        <<First/binary>> ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, Size, byte_size(First)}),
            %% Incompleate packet header in Bin
            if
                4 > Size ->
                    %% Incomplete packet header in buffer
                    input_handler_recv(
                      IHP, Bin_Front, Size, Rear, CSHandle);
                Size > 4 ->
                    %% Complete packet header is buffered, and some more
                    {Hdr, Front_1, Rear_1} =
                        collect_bin(First, Front, 4 - byte_size(First), Rear),
                    input_handler(
                      IHP, [Hdr | Front_1], Size, Rear_1, CSHandle);
                true -> % Size == 4
                    %% Exacty a packet header is buffered
                    Hdr = list_to_binary(Bin_Front ++ lists:reverse(Rear)),
                    input_handler(IHP, [Hdr], Size, [], CSHandle)
            end
    end.

input_handler_recv(IHP, Front, Size, Rear, undefined) ->
    CSHandle = make_ref(),
    case socket:recv(IHP#ihp.socket, 0, [], CSHandle) of
        {select_read, {{select_info,_,_}, Data}} ->
            %% erlang:display({?FUNCTION_NAME, ?LINE,
            %%                 select, {CSHandle,byte_size(Data)}}),
            Size_1 = byte_size(Data) + Size,
            Rear_1 = [Data | Rear],
            input_handler(IHP, Front, Size_1, Rear_1, CSHandle);
        {select, {select_info,_,_}} ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, select, CSHandle}),
            input_handler(IHP, Front, Size, Rear, CSHandle);
        {completion, {completion_info,_,_}} ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, select, CSHandle}),
            input_handler(IHP, Front, Size, Rear, CSHandle);
        Result ->
            input_handler_common(IHP, Front, Size, Rear, Result)
    end;
input_handler_recv(IHP, Front, Size, Rear, CSHandle) ->
    input_handler_wait(IHP, Front, Size, Rear, CSHandle).

input_handler_common(IHP, Front, Size, Rear, Result) ->
    case Result of
        {ok, Data} ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, '<<', byte_size(Data)}),
            Size_1 = byte_size(Data) + Size,
            Rear_1 = [Data | Rear],
            input_handler(IHP, Front, Size_1, Rear_1, undefined);
        {error, Reason} ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, error, Reason}),
            exit(Reason)
    end.

input_handler_wait(IHP, Front, Size, Rear, CSHandle) ->
    %% erlang:display({?FUNCTION_NAME, ?LINE, CSHandle}),
    Socket = IHP#ihp.socket,
    receive
        {'$socket', Socket, select, CSHandle} ->
            input_handler_recv(IHP, Front, Size, Rear, undefined);
        {'$socket', Socket, completion, {CSHandle, Result}} ->
            input_handler_common(IHP, Front, Size, Rear, Result);
        _Ignore ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, _Ignore}),
            input_handler_wait(IHP, Front, Size, Rear, CSHandle)
    end.

collect_bin(Collected, [Bin | Front], N, Rear) ->
    BinSize = byte_size(Bin),
    if
        N > BinSize ->
            collect_bin(
              <<Collected/binary, Bin/binary>>, Front, N - BinSize, Rear);
        BinSize > N->
            <<First:N/binary, Rest/binary>> = Bin,
            {<<Collected/binary, First/binary>>, [Rest | Front], Rear};
        true -> % BinSize == N
            {<<Collected/binary, Bin/binary>>, Front, Rear}
    end;
collect_bin(Collected, [], N, [_|_] = Rear) ->
    collect_bin(Collected, lists:reverse(Rear), N, []).

collect_iov(Collected, [Bin | Front], N, Rear) ->
    BinSize = byte_size(Bin),
    if
        N > BinSize ->
            collect_iov([Bin | Collected], Front, N - BinSize, Rear);
        BinSize > N ->
            <<First:N/binary, Rest/binary>> = Bin,
            {lists:reverse(Collected, [First]), [Rest | Front], Rear};
        true -> % BinSize == N
            {lists:reverse(Collected, [Bin]), Front, Rear}
    end;
collect_iov(Collected, [], N, [_|_] = Rear) ->
    collect_iov(Collected, lists:reverse(Rear), N, []).

%% We deliver ticks (packets size 0) to the VM,
%% so that erlang:dist_get_stat(DistHandle) that
%% dist_util:getstat/3 falls back to becomes good enough
put_data(DistHandle, Packet) ->
    %% erlang:display({'<<==', iolist_size(Packet)}),
    erlang:dist_ctrl_put_data(DistHandle, Packet).

%% ------------------------------------------------------------
supported() ->
    try socket:info() of
	#{io_backend := #{name := BackendName}}
          when (BackendName =/= win_esaio) ->
            ok;
        _ ->
            {skip, "Temporary exclusion"}
    catch
        error : notsup ->
            {skip, "esock not supported"};
        error : undef ->
            {skip, "esock not configured"}
    end.
    %% try socket:is_supported(ipv6) of
    %%     _ ->
    %%         ok
    %% catch error : notsup ->
    %%         "Module 'socket' not supported"
    %% end.

%% ------------------------------------------------------------
check_ip(Ip, PeerIp) ->
    try
        case application:get_env(kernel, check_ip) of
            {ok, true} ->
                maybe
                    {ok, Ifaddrs} ?= net:getifaddrs(),
                    {ok, Netmask} ?= find_netmask(Ip, Ifaddrs),
                    mask(Ip, Netmask) =:= mask(PeerIp, Netmask) orelse
                        begin
                            error_logger:error_msg(
                              "** Connection attempt from "
                              "disallowed IP ~w ** ~n",
                              [PeerIp]),
                            ?shutdown(no_node)
                        end,
                    ok
                else
                    Error ->
                        exit({check_ip, Error})
                end;
            _ ->
                ok
        end
    catch error : Reason : Stacktrace ->
            error_logger:error_msg(
              "error : ~p in ~n    ~p~n", [Reason, Stacktrace]),
            erlang:raise(error, Reason, Stacktrace)
    end.

find_netmask(
  Ip,
  [#{addr := #{addr := Ip},
     netmask := #{addr := Netmask}} | _IfAddrs]) ->
    {ok, Netmask};
find_netmask(Ip, [_ | IfAddrs]) ->
    find_netmask(Ip, IfAddrs);
find_netmask(_, []) ->
    {error, no_netmask}.

mask(Addr, Mask) when tuple_size(Addr) =:= tuple_size(Mask)->
    N = tuple_size(Mask),
    if
        tuple_size(Addr) == N ->
            mask(Addr, Mask, N, 1);
        true ->
            {error, {ip_size, Addr, N}}
    end.
%%
mask(Addr, Mask, N, I) when I =< N ->
    [element(N, Addr) band element(N, Mask) | mask(Addr, Mask, N, I + 1)];
mask(_, _, _, _) ->
    [].
