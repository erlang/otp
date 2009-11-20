%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%

%%% Description : Backwards compatibility wrapper

-module(ssh_cm).

-include("ssh.hrl").
-include("ssh_connect.hrl").

%% -define(DEFAULT_PACKET_SIZE, 32768).
%% -define(DEFAULT_WINDOW_SIZE, 2*?DEFAULT_PACKET_SIZE).
%%-define(DEFAULT_TIMEOUT, 5000).

-export([connect/1, connect/2, connect/3]).
-export([listen/2, listen/3, listen/4, stop_listener/1]).
-export([stop/1]).

-deprecated({connect, 1, next_major_release}).
-deprecated({connect, 2, next_major_release}).
-deprecated({connect, 3, next_major_release}).
-deprecated({listen, 2, next_major_release}).
-deprecated({listen, 3, next_major_release}).
-deprecated({listen, 4, next_major_release}).
-deprecated({stop_listener, 1, next_major_release}).
-deprecated({stop, 1, next_major_release}).

-export([adjust_window/3, attach/2, attach/3, detach/2,
	 tcpip_forward/3, cancel_tcpip_forward/3, direct_tcpip/6,
	 direct_tcpip/8, close/2, shell/2, exec/4,
	 send/3, send/4,
	 send_ack/3, send_ack/4, send_ack/5, send_eof/2,
	 session_open/2, session_open/4, subsystem/4,
	 open_pty/3, open_pty/7, open_pty/9,
	 set_user_ack/4, 
	 setenv/5, signal/3, winch/4]).

-deprecated({adjust_window, 3, next_major_release}).
-deprecated({attach, 2, next_major_release}).
-deprecated({attach, 3, next_major_release}).
-deprecated({detach, 2, next_major_release}).
-deprecated({tcpip_forward, 3, next_major_release}).
-deprecated({cancel_tcpip_forward, 3, next_major_release}).
-deprecated({direct_tcpip, 6, next_major_release}).
-deprecated({direct_tcpip, 8, next_major_release}).
-deprecated({close, 2, next_major_release}).
-deprecated({shell, 2, next_major_release}).
-deprecated({exec, 4, next_major_release}).
-deprecated({send, 3, next_major_release}).
-deprecated({send, 4, next_major_release}).
-deprecated({send_ack, 3, next_major_release}).
-deprecated({send_ack, 4, next_major_release}).
-deprecated({send_ack, 5, next_major_release}).
-deprecated({send_eof, 2, next_major_release}).
-deprecated({session_open, 2, next_major_release}).
-deprecated({session_open, 4, next_major_release}).
-deprecated({subsystem, 4, next_major_release}).
-deprecated({open_pty, 3, next_major_release}).
-deprecated({open_pty, 7, next_major_release}).
-deprecated({open_pty, 9, next_major_release}).
-deprecated({set_user_ack, 4, next_major_release}).
-deprecated({setenv, 5, next_major_release}).
-deprecated({signal, 3, next_major_release}).
-deprecated({winch, 4, next_major_release}).

-export([info/1, info/2, recv_window/3,
	 send_window/3, renegotiate/1, renegotiate/2,
	 get_peer_addr/1]). 

%%====================================================================
%% API
%%====================================================================
connect(Host) ->
    connect(Host, []).
connect(Host, Opts) ->
    connect(Host, ?SSH_DEFAULT_PORT, Opts).
connect(Host, Port, Opts) ->
    ssh:connect(Host, Port, Opts).

listen(ChannelSpec, Port) ->
    listen(ChannelSpec, Port, []).
listen(ChannelSpec, Port, Opts) ->
    listen(ChannelSpec, any, Port, Opts).
listen(ChannelSpec, "localhost", Port, Opts) ->
    listen(ChannelSpec, any, Port, Opts);
listen(_ChannelSpec, Host, Port, Opts) ->
    ssh:daemon(Host, Port, Opts).

stop_listener(SysSup) ->
    ssh_system_sup:stop_listener(SysSup).
stop(Cm) ->
    ssh:close(Cm).

%% CM Client commands
session_open(Cm, Timeout) ->
    session_open(Cm, ?DEFAULT_WINDOW_SIZE, ?DEFAULT_PACKET_SIZE, Timeout).

session_open(Cm, InitialWindowSize, MaxPacketSize, Timeout) ->
    ssh_connection:session_channel(Cm, InitialWindowSize, MaxPacketSize,
				   Timeout).


setenv(Cm, Channel, Var, Value, Timeout) ->
    ssh_connection:setenv(Cm, Channel, Var, Value, Timeout).

shell(Cm, Channel) ->
    ssh_connection:shell(Cm, Channel).

exec(Cm, Channel, Command, Timeout) ->
    ssh_connection:exec(Cm, Channel, Command, Timeout).

subsystem(Cm, Channel, SubSystem, Timeout) ->
    ssh_connection:subsystem(Cm, Channel, SubSystem, Timeout).

%% Not needed for backwards compatibility for now
attach(_Cm, _Timeout) ->
    ok.

attach(_Cm, _ChannelPid, _Timeout) ->
    ok.

detach(_Cm, _Timeout) ->
    ok.

%% Not needed, send_ack is now call! Temp backwardcompability
set_user_ack(_, _, _, _) ->
    ok.

adjust_window(Cm, Channel, Bytes) ->
    ssh_connection:adjust_window(Cm, Channel, Bytes).

close(Cm, Channel) ->
    ssh_connection:close(Cm, Channel).

send_eof(Cm, Channel) ->
    ssh_connection:send_eof(Cm, Channel).

send(Cm, Channel, Data) ->
    ssh_connection:send(Cm, Channel, 0, Data).

send(Cm, Channel, Type, Data) ->
    ssh_connection:send(Cm, Channel, Type, Data).

%% Send ack is not needed 
send_ack(Cm, Channel, Data) ->
    send_ack(Cm, Channel, 0, Data, infinity).

send_ack(Cm, Channel, Type, Data) ->
    send_ack(Cm, Channel, Type, Data, infinity).

send_ack(Cm, Channel, Type, Data, Timeout) ->
    ssh_connection:send(Cm, Channel, Type, Data, Timeout).

%% ----------------------------------------------------------------------
%% These functions replacers are not officially supported but proably will be
%% when we had time to test them.
%% ----------------------------------------------------------------------
direct_tcpip(Cm, RemoteHost, RemotePort, OrigIP, OrigPort, Timeout) ->
    direct_tcpip(Cm, RemoteHost, RemotePort, OrigIP, OrigPort,
		 ?DEFAULT_WINDOW_SIZE, ?DEFAULT_PACKET_SIZE, Timeout).

direct_tcpip(Cm, RemoteIP, RemotePort, OrigIP, OrigPort,
	     InitialWindowSize, MaxPacketSize, Timeout) ->
    ssh_connection:direct_tcpip(Cm, RemoteIP, RemotePort,
				OrigIP, OrigPort,
				InitialWindowSize, 
				MaxPacketSize, Timeout).

tcpip_forward(Cm, BindIP, BindPort) ->
    ssh_connection:tcpip_forward(Cm, BindIP, BindPort).

cancel_tcpip_forward(Cm, BindIP, Port) ->
    ssh_connection:cancel_tcpip_forward(Cm, BindIP, Port).

open_pty(Cm, Channel, Timeout) ->
    open_pty(Cm, Channel, os:getenv("TERM"), 80, 24, [], Timeout).

open_pty(Cm, Channel, Term, Width, Height, PtyOpts, Timeout) ->
    open_pty(Cm, Channel, Term, Width, Height, 0, 0, PtyOpts, Timeout).

open_pty(Cm, Channel, Term, Width, Height, PixWidth, PixHeight, 
	 PtyOpts, Timeout) ->
    ssh_connection:open_pty(Cm, Channel, Term, 
				     Width, Height, PixWidth, 
				     PixHeight, PtyOpts, Timeout).
winch(Cm, Channel, Width, Height) ->
    winch(Cm, Channel, Width, Height, 0, 0).
winch(Cm, Channel, Width, Height, PixWidth, PixHeight) ->
    ssh_connection:window_change(Cm, Channel, Width,
					  Height, PixWidth, PixHeight).
signal(Cm, Channel, Sig) ->
    ssh_connection:signal(Cm, Channel, Sig).
    
%% ----------------------------------------------------------------------
%% These functions replacers are not officially supported and
%% the format of them will proably change when and
%% if they get supported.
%% ----------------------------------------------------------------------
info(Cm) ->
    info(Cm, all).

info(Cm, ChannelPid) ->
    ssh_connection_manager:info(Cm, ChannelPid).

send_window(Cm, Channel, Timeout) ->
    ssh_connection_manager:send_window(Cm, Channel, Timeout).

recv_window(Cm, Channel, Timeout) ->
    ssh_connection_manager:recv_window(Cm, Channel, Timeout).

renegotiate(Cm) ->
    renegotiate(Cm, []).
renegotiate(Cm, _Opts) ->
    %%TODO: How should this work, backwards compat?
   ssh_connection_manager:renegotiate(Cm).

get_peer_addr(Cm) ->
    ssh_connection_manager:peer_addr(Cm).
    
