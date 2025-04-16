%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2016-2025. All Rights Reserved.
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
-module(dtls_socket).
-moduledoc false.

-include("ssl_internal.hrl").
-include("ssl_api.hrl").

-export([send/3,
         listen/2,
         accept/3,
         connect/4,
         socket/5,
         setopts/3,
         getopts/3,
         getstat/3,
	 peername/2,
         sockname/2,
         port/2,
         close/2,
         close_listen/2
        ]).

-export([emulated_options/0,
         emulated_options/1,
         internal_inet_values/0,
         default_inet_values/0,
         default_cb_info/0]).

send(Transport, {{IP,Port},Socket}, Data) ->
    Transport:send(Socket, IP, Port, Data).

listen(Port, #config{inet_ssl = SockOpts,
                     ssl = SslOpts,
                     emulated = EmOpts,
                     inet_user = Options} = Config) ->
    IP = proplists:get_value(ip, SockOpts, default_ip(SockOpts)),
    case dtls_listener_sup:lookup_listener(IP, Port) of
        undefined ->
            start_new_listener(IP, Port, self(), Config);
        {ok, Listener} ->
            dtls_packet_demux:new_owner(Listener, self()),
            dtls_packet_demux:set_all_opts(
              Listener, {Options,
                          emulated_socket_options(EmOpts,
                                                  #socket_options{}),
                          SslOpts}),
            dtls_listener_sup:register_listener({self(), Listener},
                                                IP, Port),
            {ok, create_dtls_socket(Config, Listener, Port)};
        Error ->
            Error
    end.

accept({Listener,_}, #config{}, _Timeout) ->
    case dtls_packet_demux:accept(Listener, self()) of
	{ok, Pid} ->
            receive
                {Pid, user_socket, UserSocket} ->
                    {ok, UserSocket}
            end;
	{error, Reason} ->
	    {error, Reason}
    end.

connect(Host, Port, #config{transport_info = CbInfo,
                               ssl = SslOpts,
                               emulated = EmOpts,
                               inet_ssl = SocketOpts,
                               tab = _Tab
                              }, Timeout) ->
    Transport = element(1, CbInfo),
    case Transport:open(0, SocketOpts ++ internal_inet_values()) of
	{ok, Socket} ->
	    dtls_gen_connection:start_fsm(client, Host, Port, {{Host, Port}, Socket},
                                          {SslOpts,
                                           emulated_socket_options(EmOpts, #socket_options{}), undefined},
                                          self(), CbInfo, Timeout);
	{error, _} = Error->	
	    Error
    end.

close_listen(#sslsocket{socket_handle = {Pid, Port0},
                        listener_config = #config{inet_ssl = SockOpts}}, Timeout) ->
    IP = proplists:get_value(ip, SockOpts, default_ip(SockOpts)),
    Port = get_real_port(Pid, Port0),
    dtls_listener_sup:register_listener({undefined, Pid}, IP, Port),
    case dtls_packet_demux:close(Pid) of
        stop ->
            erlang:monitor(process, Pid),
            receive {'DOWN', _, process, Pid, _} ->
                    ok
            after Timeout ->
                    {error, timeout}
            end;
        waiting ->
            ok;
        Error ->
            Error
    end.

default_ip(SockOpts) ->
    case proplists:get_value(inet6, SockOpts, false) of
        false -> {0,0,0,0};
        true  -> {0,0,0,0, 0,0,0,0}
    end.

close(gen_udp, {_Client, _Socket}) ->
    ok;
close(Transport, {_Client, Socket}) ->
    Transport:close(Socket).


%% Note that gen_udp:send is not blocking as gen_tcp:send is.
%% So normal DTLS can safely have the same connection handler
%% as payload sender

socket([Pid], gen_udp = Transport,
       PeerAndSock = {{_Host, _Port}, _Socket}, ConnectionCb, Tab) when Tab =/= undefined ->
    #sslsocket{socket_handle = PeerAndSock,
               connection_handler = Pid,
               payload_sender = Pid,
               transport_cb = Transport,
               connection_cb = ConnectionCb,
               tab = Tab};
socket([Pid], Transport, Socket, ConnectionCb, Tab) when Tab =/= undefined ->
    #sslsocket{socket_handle = Socket,
               connection_handler = Pid,
               payload_sender = Pid,
               transport_cb = Transport,
               connection_cb = ConnectionCb, 
               tab = Tab}.

setopts(_, Socket = #sslsocket{socket_handle = {ListenPid, _},
                               listener_config = #config{}}, Options) ->
    SplitOpts = {_, EmOpts} = tls_socket:split_options(Options),
    check_active_n(EmOpts, Socket),
    dtls_packet_demux:set_sock_opts(ListenPid, SplitOpts);
%%% Following clauses will not be called for emulated options, they are  handled in the connection process
setopts(gen_udp, Socket, Options) ->
    inet:setopts(Socket, Options);
setopts(Transport, Socket, Options) ->
    Transport:setopts(Socket, Options).

check_active_n(EmulatedOpts, Socket = #sslsocket{socket_handle = {ListenPid, _},
                                                 listener_config = #config{}}) ->
    %% We check the resulting options to send an ssl_passive message if necessary.
    case proplists:lookup(active, EmulatedOpts) of
        %% The provided value is out of bound.
        {_, N} when is_integer(N), N < -32768 ->
            throw(einval);
        {_, N} when is_integer(N), N > 32767 ->
            throw(einval);
        {_, N} when is_integer(N) ->
            {ok, #socket_options{active = Active}, _} = dtls_packet_demux:get_all_opts(ListenPid),
            case Active of
                Atom when is_atom(Atom), N =< 0 ->
                    self() ! {ssl_passive, Socket};
                %% The result of the addition is out of bound.
                %% We do not need to check < -32768 because Active can't be below 1.
                A when is_integer(A), A + N > 32767 ->
                    throw(einval);
                A when is_integer(A), A + N =< 0 ->
                    self() ! {ssl_passive, Socket};
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

getopts(_, #sslsocket{socket_handle = {ListenPid, _}, listener_config =#config{}}, Options) ->
    SplitOpts = tls_socket:split_options(Options),
    dtls_packet_demux:get_sock_opts(ListenPid, SplitOpts);
%%% Following clauses will not be called for emulated options, they are  handled in the connection process
getopts(gen_udp, {_,{{_, _},Socket}}, Options) ->
    inet:getopts(Socket, Options);
getopts(gen_udp, {_,Socket}, Options) ->
    inet:getopts(Socket, Options);
getopts(Transport, Socket, Options) ->
    Transport:getopts(Socket, Options).
getstat(gen_udp, Pid, Options) when is_pid(Pid) ->
    dtls_packet_demux:getstat(Pid, Options);
getstat(gen_udp, {_,{_, Socket}}, Options) ->
    inet:getstat(Socket, Options);
getstat(gen_udp, {_, Socket}, Options) ->
    inet:getstat(Socket, Options);
getstat(gen_udp, Socket, Options) ->
    inet:getstat(Socket, Options);
getstat(Transport, Socket, Options) ->
	Transport:getstat(Socket, Options).

peername(_, undefined) ->
    {error, enotconn};
peername(gen_udp, {_, {Client, _Socket}}) ->
    {ok, Client};
peername(gen_udp, {Client, _Socket}) ->
    {ok, Client};
peername(Transport, Socket) ->
    Transport:peername(Socket).
sockname(gen_udp, {_, {_,Socket}}) ->
    inet:sockname(Socket);
sockname(gen_udp, Socket) ->
    inet:sockname(Socket);
sockname(Transport, Socket) ->
    Transport:sockname(Socket).

port(gen_udp, {_,Socket}) ->
    inet:port(Socket);
port(Transport, Socket) ->
    Transport:port(Socket).

emulated_options() ->
    [mode, active,  packet, packet_size].

emulated_options(Opts) ->
      emulated_options(Opts, internal_inet_values(), default_inet_values()).

internal_inet_values() ->
    [{active, false}, {mode,binary}].

default_inet_values() ->
    [{active, true}, {mode, list}, {packet, 0}, {packet_size, 0}].

default_cb_info() ->
    {gen_udp, udp, udp_closed, udp_error, udp_passive}.

emulated_socket_options(InetValues, #socket_options{
				       mode   = Mode,
                                       packet = Packet,
                                       packet_size = PacketSize,
				       active = Active}) ->
    #socket_options{
       mode   = proplists:get_value(mode, InetValues, Mode),
       packet = proplists:get_value(packet, InetValues, Packet),
       packet_size = proplists:get_value(packet_size, InetValues, PacketSize),
       active = emulated_active_option(InetValues, Active)
      }.

emulated_active_option([], Active) ->
    Active;
emulated_active_option([{active, Active} | _], _) when Active =< 0 ->
    false;
emulated_active_option([{active, Active} | _], _) ->
    Active;
emulated_active_option([_|Tail], Active) ->
    emulated_active_option(Tail, Active).

emulated_options([{mode, Value} = Opt |Opts], Inet, Emulated) ->
    validate_inet_option(mode, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(mode, Emulated)]);
emulated_options([{header, _} = Opt | _], _, _) ->
    throw({error, {options, {not_supported, Opt}}});
emulated_options([{active, Value} = Opt |Opts], Inet, Emulated) ->
    validate_inet_option(active, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(active, Emulated)]);
emulated_options([{packet, _} = Opt | _], _, _) ->
    throw({error, {options, {not_supported, Opt}}});
emulated_options([{packet_size, _} = Opt | _], _, _) ->
    throw({error, {options, {not_supported, Opt}}});
emulated_options([Opt|Opts], Inet, Emulated) ->
    emulated_options(Opts, [Opt|Inet], Emulated);
emulated_options([], Inet,Emulated) ->
    {Inet, Emulated}.

validate_inet_option(mode, Value)
  when Value =/= list, Value =/= binary ->
    throw({error, {options, {mode,Value}}});
validate_inet_option(active, Value)
  when Value >= -32768, Value =< 32767 ->
    ok;
validate_inet_option(active, Value)
  when Value =/= true, Value =/= false, Value =/= once ->
    throw({error, {options, {active,Value}}});
validate_inet_option(_, _) ->
    ok.

get_real_port(Listener, Port0) when is_pid(Listener) andalso
                                    is_integer(Port0) ->
    case Port0 of
        0 ->
            {ok, {_, NewPort}} = dtls_packet_demux:sockname(Listener),
            NewPort;
        _ ->
            Port0
    end.

start_new_listener(IP, Port0, Owner,
                   #config{transport_info = {TransportModule, _,_,_,_},
                           inet_user = Options} = Config) ->
    InetOptions = Options ++ internal_inet_values(),
    case TransportModule:open(Port0, InetOptions) of
        {ok, Socket} ->
            Port = case Port0 of
                       0 ->
                           {ok, P} = inet:port(Socket),
                           P;
                       _ ->
                           Port0
                   end,
            start_dtls_packet_demux(Config, IP, Port, Socket, Owner);
        {error, eaddrinuse} ->
            {error, already_listening};
        Error ->
            Error
    end.

start_dtls_packet_demux(#config{
                           transport_info =
                               {TransportModule, _,_,_,_} = TransportInfo,
                           emulated = EmOpts0,
                           ssl = SslOpts} = Config, IP, Port, Socket, Owner) ->
    EmOpts = emulated_socket_options(EmOpts0, #socket_options{}),
    case dtls_listener_sup:start_child([Owner, Port, TransportInfo, EmOpts, SslOpts, Socket]) of
        {ok, Multiplexer} ->
            ok = TransportModule:controlling_process(Socket, Multiplexer),
            dtls_listener_sup:register_listener({self(), Multiplexer},
                                                IP, Port),
            DTLSSocket = create_dtls_socket(Config, Multiplexer, Port),
	    {ok, DTLSSocket};
        Error ->
            Error
    end.

create_dtls_socket(#config{emulated = EmOpts} = Config,
                   Listener, Port) ->
    Socket = #sslsocket{
                socket_handle = {Listener, Port},
                listener_config = Config,
                tab = dtls_listen},
    check_active_n(EmOpts, Socket),
    Socket.
