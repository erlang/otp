%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023-2024. All Rights Reserved.
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
%% Plug-in module for inet_epmd distribution
%% with cryptcookie over inet_tcp with kTLS offloading
%%
-module(inet_epmd_cryptcookie_socket_ktls).

%% DistMod API
-export([net_address/0, listen_open/2, listen_port/3, listen_close/1,
         accept_open/2, accept_controller/3, accepted/3,
         connect/3]).

-export([supported/0]).

%% Socket I/O Stream internal exports (export entry fun()s)
-export([stream_recv/2, stream_send/2,
         stream_controlling_process/2]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

-define(FAMILY, inet).

%% ------------------------------------------------------------
net_address() ->
    #net_address{
       protocol = dist_cryptcookie:protocol(),
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
              inet_epmd_dist:merge_options(
                ListenOptions, [inet_epmd_dist:nodelay()], [])),
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
        inet_epmd_dist:check_ip(Ip, PeerIp),
        Stream = stream(Socket),
        {_Stream_1, _, CipherState} = cryptcookie:init(Stream),
        KtlsInfo =
            socket_ktls_info(Socket, cryptcookie:ktls_info(CipherState)),
        ok ?=
            inet_tls_dist:set_ktls(KtlsInfo),
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
    inet_epmd_socket:start_dist_ctrl(NetAddress, Socket).

%% ------------------------------------------------------------
connect(
  #net_address{ address = {Ip, Port}, family = Family } = NetAddress,
  _Timer, ConnectOptions) ->
    maybe
        {ok, Socket} ?=
            socket:open(Family, stream),
        ok ?=
            setopts(
              Socket,
              inet_epmd_dist:merge_options(
                ConnectOptions, [inet_epmd_dist:nodelay()], [])),
        ConnectAddress =
            #{ family => Family,
               addr => Ip,
               port => Port },
        ok ?=
            socket:connect(Socket, ConnectAddress),
        Stream = stream(Socket),
        {_Stream_1, _, CipherState} = cryptcookie:init(Stream),
        KtlsInfo =
            socket_ktls_info(Socket, cryptcookie:ktls_info(CipherState)),
        ok ?=
            inet_tls_dist:set_ktls(KtlsInfo),
        inet_epmd_socket:start_dist_ctrl(NetAddress, Socket)
    else
        {error, _} = Error ->
            Error
    end.

%% ------------------------------------------------------------
%% A socket as an I/O Stream
%%
%%  Stream    :: {InStream, OutStream, ControllingProcessFun}.
%%
%%  InStream  :: [InFun | InState].
%%  InFun     :: fun (InStream, Size) ->
%%                   [Data | NewInStream] |
%%                   [closed | DebugTerm]
%%  NewInStream :: InStream
%%  %% If Size == 0 and there is no pending input data;
%%  %% return immediately with empty Data,
%%  %% otherwise wait for Size bytes of data
%%  %% or any amount of data > 0
%%
%%  OutStream :: [OutFun | OutState]
%%  OutFun    :: fun (OutStream, Data) ->
%%                   NewOutStream |
%%                   [closed | DebugTerm]
%%  NewOutStream :: OutStream
%%
%%  Data :: binary() or list(binary())
%%
%%  ControllingProcessFun :: fun (Stream, pid()) -> NewStream
%%
%%  NewSTream :: Stream

stream(Socket) ->
    {stream_in(Socket), stream_out(Socket),
     fun ?MODULE:stream_controlling_process/2}.

stream_in(Socket) ->
    [fun ?MODULE:stream_recv/2 | Socket].

stream_recv(InStream = [_ | Socket], Size) ->
    case
        if
            Size =:= 0 ->
                socket:recv(Socket, 0, 0);
            true ->
                socket:recv(Socket, Size, infinity)
        end
    of
        {ok, Data} ->
            [Data | InStream];
        {error, {Reason, _Data}} ->
            stream_recv_error(InStream, Reason);
        {error, timeout} ->
            [<<>> | InStream];
        {error, Reason} ->
            stream_recv_error(InStream, Reason)
    end.

stream_recv_error(InStream, Reason) ->
    if
        Reason =:= closed;
        Reason =:= econnreset ->
            [closed | InStream];
        true ->
            erlang:error({?MODULE, ?FUNCTION_NAME, Reason})
    end.

stream_out(Socket) ->
    [fun ?MODULE:stream_send/2 | Socket].

stream_send(OutStream, Bin) when is_binary(Bin) ->
    stream_send(OutStream, [Bin]);
stream_send(OutStream = [_ | Socket], Data) ->
    case socket:sendmsg(Socket, #{ iov => Data }) of
        ok ->
            OutStream;
        {error, closed} ->
            [closed | OutStream];
        {error, Reason} ->
            erlang:error({?MODULE, ?FUNCTION_NAME, Reason, [OutStream, Data]})
    end.

stream_controlling_process(Stream = {_, [_ | Socket], _}, Pid) ->
    %%
    case socket:setopt(Socket, {otp,controlling_process}, Pid) of
        ok ->
            Stream;
        {error, Reason} ->
            erlang:error({?MODULE, ?FUNCTION_NAME, Reason})
    end.

%% ------------------------------------------------------------
supported() ->
    maybe
        ok ?= inet_epmd_socket:supported(),
        ok ?= cryptcookie:supported(),
        %%
        {ok, Listen} = socket:open(?FAMILY, stream),
        ok = socket:bind(Listen, loopback),
        ok = socket:listen(Listen),
        {ok, Addr} = socket:sockname(Listen),
        {ok, Client} = socket:open(?FAMILY, stream),
        ok = socket:connect(Client, Addr),
        try
            inet_tls_dist:set_ktls(
              socket_ktls_info(Client, cryptcookie:ktls_info()))
        after
            _ = socket:close(Client),
            _ = socket:close(Listen)
        end
    end.


socket_ktls_info(Socket, KtlsInfo) ->
    KtlsInfo
        #{ socket => Socket,
           setopt_fun => fun socket:setopt_native/3,
           getopt_fun => fun socket:getopt_native/3 }.
