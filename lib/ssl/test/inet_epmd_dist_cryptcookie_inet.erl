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
%% Module for dist_cryptcookie over inet_tcp
%%
-module(inet_epmd_dist_cryptcookie_inet).

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
-define(DRIVER, inet_tcp).

%% ------------------------------------------------------------
net_address() ->
    Family = ?DRIVER:family(),
    #net_address{
       protocol = dist_cryptcookie:protocol(),
       family = Family }.

%% ------------------------------------------------------------
listen_open(_NetAddress, Options) ->
    {ok,
     inet_epmd_dist:merge_options(
       Options,
       [{active, false}, {mode, binary}, {packet, 0},
        inet_epmd_dist:nodelay()],
       [])}.

%% ------------------------------------------------------------
listen_port(_NetAddress, Port, ListenOptions) ->
    maybe
        {ok, ListenSocket} ?=
            ?DRIVER:listen(Port, ListenOptions),
        {ok, Address} ?=
            inet:sockname(ListenSocket),
        {ok, {ListenSocket, Address}}
    end.

%% ------------------------------------------------------------
listen_close(ListenSocket) ->
    ?DRIVER:close(ListenSocket).

%% ------------------------------------------------------------
accept_open(_NetAddress, ListenSocket) ->
    maybe
        {ok, Socket} ?=
            ?DRIVER:accept(ListenSocket),
        {ok, {Ip, _}} ?=
            inet:sockname(Socket),
        {ok, {PeerIp, _} = PeerAddress} ?=
            inet:peername(Socket),
        inet_epmd_dist:check_ip(Ip, PeerIp),
        Stream = stream(Socket),
        CryptcookieInit = cryptcookie:init(Stream),
        DistCtrlHandle = dist_cryptcookie:start_dist_ctrl(CryptcookieInit),
        {DistCtrlHandle, PeerAddress}
    else
        {error, Reason} ->
            exit({accept, Reason})
    end.

%% ------------------------------------------------------------
accept_controller(_NetAddress, Controller, DistCtrlHandle) ->
    dist_cryptcookie:controlling_process(DistCtrlHandle, Controller).

%% ------------------------------------------------------------
accepted(NetAddress, _Timer, DistCtrlHandle) ->
    dist_cryptcookie:hs_data(NetAddress, DistCtrlHandle).

%% ------------------------------------------------------------
connect(NetAddress, _Timer, Options) ->
    ConnectOptions =
        inet_epmd_dist:merge_options(
          Options,
          [{active, false}, {mode, binary}, {packet, 0},
           inet_epmd_dist:nodelay()],
          []),
    #net_address{ address = {Ip, Port} } = NetAddress,
    maybe
        {ok, Socket} ?=
            ?DRIVER:connect(Ip, Port, ConnectOptions),
        Stream = stream(Socket),
        CryptcookieInit = cryptcookie:init(Stream),
        DistCtrlHandle = dist_cryptcookie:start_dist_ctrl(CryptcookieInit),
        dist_cryptcookie:hs_data(NetAddress, DistCtrlHandle)
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
                ?DRIVER:recv(Socket, 0, 0);
            true ->
                ?DRIVER:recv(Socket, Size, infinity)
        end
    of
        {ok, Data} ->
            [Data | InStream];
        {error, timeout} ->
            [<<>> | InStream];
        {error, closed} ->
            [closed | InStream];
        {error, Reason} ->
            erlang:error({?MODULE, ?FUNCTION_NAME, Reason})
    end.

stream_out(Socket) ->
    [fun ?MODULE:stream_send/2 | Socket].

stream_send(OutStream = [_ | Socket], Data) ->
    case ?DRIVER:send(Socket, Data) of
        ok ->
            OutStream;
        {error, closed} ->
            [closed | OutStream];
        {error, Reason} ->
            erlang:error({?MODULE, ?FUNCTION_NAME, Reason, [OutStream, Data]})
    end.

stream_controlling_process(Stream = {_, [_ | Socket], _}, Pid) ->
    %%
    case ?DRIVER:controlling_process(Socket, Pid) of
        ok ->
            Stream;
        {error, Reason} ->
            erlang:error({?MODULE, ?FUNCTION_NAME, Reason})
    end.

%% ------------------------------------------------------------
supported() ->
    cryptcookie:supported().
