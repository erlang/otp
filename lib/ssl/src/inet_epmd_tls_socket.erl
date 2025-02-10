%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2025. All Rights Reserved.
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

-module(inet_epmd_tls_socket).
-moduledoc false.

%% DistMod API
-export([net_address/0, childspecs/0,
         listen_open/2, listen_port/3, listen_close/1,
         accept_open/2, accept_controller/3, accepted/3,
         connect/3]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").
-include_lib("public_key/include/public_key.hrl").

-include("ssl_api.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include_lib("kernel/include/logger.hrl").

-define(PROTOCOL, tls).
-define(FAMILY, inet).

%% ------------------------------------------------------------
net_address() ->
    #net_address{
       protocol = ?PROTOCOL,
       family = ?FAMILY }.

%% ------------------------------------------------------------
childspecs() ->
    {ok, [{ssl_dist_sup,{ssl_dist_sup, start_link, []},
	   permanent, infinity, supervisor, [ssl_dist_sup]}]}.

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
                ListenOptions, [{nodelay, true}], [])),
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
accept_open(NetAddress, ListenSocket) ->
    maybe
        {ok, Socket} ?=
            socket:accept(ListenSocket),
        {ok, #{ addr := Ip }} ?=
            socket:sockname(Socket),
        {ok, #{ addr := PeerIp, port := PeerPort }} ?=
            socket:peername(Socket),
        inet_epmd_socket:check_ip(Ip, PeerIp),
        accept_handshake(NetAddress, Socket, PeerIp, PeerPort)
    else
        {error, Reason} ->
            exit({?FUNCTION_NAME, Reason})
    end.

accept_handshake(#net_address{ family = Family }, Socket, PeerIp, PeerPort) ->
    Opts = inet_tls_dist:get_ssl_server_options(PeerIp),
    case
        ssl:handshake(
          Socket,
          inet_epmd_dist:merge_options(
            Opts, [Family, binary, {active, false}, {packet, 4}], []),
          net_kernel:connecttime())
    of
        {ok, SslSocket} ->
            {SslSocket, {PeerIp, PeerPort}};
        {error, {options, _} = Reason} = Error ->
            %% Bad options: that's probably our fault.
            %% Let's log that.
            ?LOG_ERROR(
              "Cannot accept TLS distribution connection: ~s~n",
              [ssl:format_error(Error)]),
            socket:close(Socket),
            exit({?FUNCTION_NAME, Reason});
        {error, Reason} ->
            socket:close(Socket),
            exit({?FUNCTION_NAME, Reason})
    end.

%% ------------------------------------------------------------
accept_controller(_NetAddress, Controller, SslSocket) ->
    maybe
        ok ?= ssl:controlling_process(SslSocket, Controller),
        SslSocket
    else
        {error, Reason} ->
            exit({?FUNCTION_NAME, Reason})
    end.

%% ------------------------------------------------------------
accepted(NetAddress, _Timer, SslSocket) ->
    start_dist_ctrl(NetAddress, SslSocket).

%% ------------------------------------------------------------
connect(
  #net_address{
     address = {Ip, Port},
     family  = Family,
     host    = Host } = NetAddress, _Timer, ConnectOptions) ->
    maybe
        {ok, Socket} ?=
            socket:open(Family, stream),
        ok ?=
            setopts(
              Socket,
              inet_epmd_dist:merge_options(
                ConnectOptions, [{nodelay, true}], [])),
        ConnectAddress =
            #{ family => Family,
               addr => Ip,
               port => Port },
        ok ?=
            socket:connect(Socket, ConnectAddress),
        SslOptions = inet_tls_dist:get_ssl_client_options(),
        {ok, SslSocket} ?=
            ssl:connect(
              Socket,
              inet_epmd_dist:merge_options(
                SslOptions,
                [Family, binary, {active, false}, {packet, 4},
                 {server_name_indication, Host}],
                []),
              net_kernel:connecttime()),
        start_dist_ctrl(NetAddress, SslSocket)
    else
        {error, _} = Error ->
            Error
    end.

%% ------------------------------------------------------------
start_dist_ctrl(
  NetAddress, #sslsocket{payload_sender = DistCtrl} = SslSocket) ->
    #hs_data{
       socket = DistCtrl,
       f_send =
           fun (Ctrl, Packet) when Ctrl =:= DistCtrl ->
                   ssl:send(SslSocket, Packet)
           end,
       f_recv =
           fun (Ctrl, Length, Timeout) when Ctrl =:= DistCtrl ->
                   case ssl:recv(SslSocket, Length, Timeout) of
                       {ok, Bin} when is_binary(Bin) ->
                           {ok, binary_to_list(Bin)};
                       Other ->
                           Other
                   end
           end,
       f_setopts_pre_nodeup =
           fun (Ctrl) when Ctrl =:= DistCtrl -> ok end,
       f_setopts_post_nodeup =
           fun (Ctrl) when Ctrl =:= DistCtrl ->
                   ssl:setopts(SslSocket, [inet_tcp_dist:nodelay()])
           end,
       f_address =
           fun (Ctrl, Node) when Ctrl =:= DistCtrl ->
                   inet_epmd_dist:f_address(NetAddress, Node)
           end,
       f_getll =
           fun (Ctrl) when Ctrl =:= DistCtrl ->
                   {ok, DistCtrl}
           end,

       f_handshake_complete =
           fun (Ctrl, Node, DHandle) when Ctrl =:= DistCtrl ->
                   tls_sender:dist_handshake_complete(DistCtrl, Node, DHandle)
           end,
       mf_tick =
           fun (Ctrl) when Ctrl =:= DistCtrl ->
                   DistCtrl ! tick
           end,

       mf_getstat =
           fun (Ctrl) when Ctrl =:= DistCtrl ->
                   case
                       ssl:getstat(SslSocket, [recv_cnt, send_cnt, send_pend])
                   of
                       {ok, Stat} ->
                           split_stat(Stat, 0, 0, 0);
                       Error ->
                           Error
                   end
           end,
       mf_setopts =
           fun (Ctrl, Opts) when Ctrl =:= DistCtrl ->
                   case setopts_filter(Opts) of
                       [] ->
                           ssl:setopts(SslSocket, Opts);
                       Opts1 ->
                           {error, {badopts,Opts1}}
                   end
           end,
       mf_getopts =
           fun (Ctrl, Opts) when Ctrl =:= DistCtrl ->
                   ssl:getopts(SslSocket, Opts)
           end
      }.

split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.

setopts_filter(Opts) ->
    [Opt || {K,_} = Opt <- Opts,
            K =:= active orelse K =:= deliver orelse K =:= packet].
