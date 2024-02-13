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
-module(inet_epmd_cryptcookie_inet_ktls).

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
    Protocol = cryptcookie:start_keypair_server(),
    #net_address{
       protocol = Protocol,
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
        {_Stream_1, _, CipherState} = cryptcookie:init(Stream),
        KtlsInfo =
            inet_ktls_info(Socket, cryptcookie:ktls_info(CipherState)),
        ok ?= inet_tls_dist:set_ktls(KtlsInfo),
        ok ?= inet:setopts(Socket, [{packet, 2}, {mode, list}]),
        {Socket, PeerAddress}
    else
        {error, Reason} ->
            exit({accept, Reason})
    end.

%% ------------------------------------------------------------
accept_controller(_NetAddress, Controller, Socket) ->
    ok = ?DRIVER:controlling_process(Socket, Controller),
    Socket.

%% ------------------------------------------------------------
accepted(NetAddress, _Timer, Socket) ->
    input_handler_setup(
      inet_epmd_dist:hs_data(NetAddress, Socket)).

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
        {_Stream_1, _, CipherState} = cryptcookie:init(Stream),
        KtlsInfo =
            inet_ktls_info(Socket, cryptcookie:ktls_info(CipherState)),
        ok ?= inet_tls_dist:set_ktls(KtlsInfo),
        ok ?= inet:setopts(Socket, [{packet, 2}, {mode, list}]),
        input_handler_setup(
          inet_epmd_dist:hs_data(NetAddress, Socket))
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

input_handler_setup(#hs_data{} = HsData) ->
    case init:get_argument(inet_ktls) of
        {ok, [["port"]]} ->  % No input_handler process
            %% Just use the distribution port
            HsData;
        {ok, [["input_handler"]]} ->  % Set up an input_handler process
            %% Add an f_handshake_complete fun that spawns the input handler
            %% and calls the f_setopts_post_nodeup fun
            #hs_data{
               socket = Socket,
               f_setopts_post_nodeup = FSetoptsPostNodeup} = HsData,
            HsData#hs_data{
              f_setopts_post_nodeup =
                  fun (S) when S =:= Socket ->
                          ok
                  end,
              f_handshake_complete =
                  fun (S, _Node, DHandle) when S =:= Socket ->
                          handshake_complete(S, FSetoptsPostNodeup, DHandle)
                  end}
    end;
input_handler_setup({error, _} = Error) ->
    Error.

handshake_complete(Socket, FSetoptsPostNodeup, DHandle) ->
    InputHandler =
        spawn_link(
          fun () ->
                  input_handler(Socket, DHandle)
          end),
    ok = ?DRIVER:controlling_process(Socket, InputHandler),
    ok = erlang:dist_ctrl_input_handler(DHandle, InputHandler),
    ok = FSetoptsPostNodeup(Socket).

input_handler(Socket, DHandle) ->
    receive
         {tcp, Socket, Data} ->
             erlang:dist_ctrl_put_data(DHandle, Data);
         {tcp_error, Socket, _Error} = Reason ->
             ?DRIVER:close(Socket),
             exit(Reason);
         {tcp_closed, Socket} = Reason ->
             ?DRIVER:close(Socket),
             exit(Reason);
         Other ->
             Reason = {unexpected_message, Other},
             error_report([?FUNCTION_NAME, {reason, Reason}]),
             input_handler(Socket, DHandle)
     end.

%% ------------------------------------------------------------

error_report(Report) ->
    error_logger:error_report(Report).

%% ------------------------------------------------------------
supported() ->
    maybe
        ok ?= cryptcookie:supported(),
        %%
        {ok, Listen} = ?DRIVER:listen(0, [{active, false}]),
        {ok, Port} = inet:port(Listen),
        {ok, Client} =
            ?DRIVER:connect({127,0,0,1}, Port, [{active, false}]),
        try
            inet_tls_dist:set_ktls(
              inet_ktls_info(Client, cryptcookie:ktls_info()))
        after
            _ = ?DRIVER:close(Client),
            _ = ?DRIVER:close(Listen)
        end
    end.


inet_ktls_info(Socket, KtlsInfo) ->
    KtlsInfo
        #{ socket => Socket,
           setopt_fun => fun inet_tls_dist:inet_ktls_setopt/3,
           getopt_fun => fun inet_tls_dist:inet_ktls_getopt/3 }.
