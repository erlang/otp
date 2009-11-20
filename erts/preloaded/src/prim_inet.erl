%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
%% The SCTP protocol was added 2006
%% by Leonid Timochouk <l.timochouk@gmail.com>
%% and Serge Aleynikov  <saleyn@gmail.com>
%% at IDT Corp. Adapted by the OTP team at Ericsson AB.
%%
-module(prim_inet).

%% Primitive inet_drv interface

-export([open/1, open/2, fdopen/2, fdopen/3, close/1]).
-export([bind/3, listen/1, listen/2]). 
-export([connect/3, connect/4, async_connect/4]).
-export([accept/1, accept/2, async_accept/2]).
-export([shutdown/2]).
-export([send/2, send/3, sendto/4, sendmsg/3]).
-export([recv/2, recv/3, async_recv/3]).
-export([unrecv/2]).
-export([recvfrom/2, recvfrom/3]).
-export([setopt/3, setopts/2, getopt/2, getopts/2, is_sockopt_val/2]).
-export([chgopt/3, chgopts/2]).
-export([getstat/2, getfd/1, getindex/1, getstatus/1, gettype/1, 
	 getiflist/1, ifget/3, ifset/3,
	 gethostname/1]).
-export([getservbyname/3, getservbyport/3]).
-export([peername/1, setpeername/2]).
-export([sockname/1, setsockname/2]).
-export([attach/1, detach/1]).

-include("inet_sctp.hrl").
-include("inet_int.hrl").

%-define(DEBUG, 1).
-ifdef(DEBUG).
-define(DBG_FORMAT(Format, Args), (io:format((Format), (Args)))).
-else.
-define(DBG_FORMAT(Format, Args), ok).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% OPEN(tcp | udp | sctp, inet | inet6)  ->
%%       {ok, insock()} |
%%       {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open(Protocol)          -> open1(Protocol, ?INET_AF_INET).

open(Protocol,   inet)  -> open1(Protocol, ?INET_AF_INET);
open(Protocol,  inet6)  -> open1(Protocol, ?INET_AF_INET6);
open(_, _)              -> {error, einval}.

fdopen(Protocol, Fd)        -> fdopen1(Protocol, ?INET_AF_INET, Fd).

fdopen(Protocol, Fd, inet)  -> fdopen1(Protocol, ?INET_AF_INET, Fd);
fdopen(Protocol, Fd, inet6) -> fdopen1(Protocol, ?INET_AF_INET6, Fd);
fdopen(_, _, _)             -> {error, einval}.

open1(Protocol, Family) ->
    case open0(Protocol) of
	{ok, S} ->
	    case ctl_cmd(S, ?INET_REQ_OPEN, [Family]) of
		{ok, _} ->
		    {ok,S};
		Error ->
		    close(S), Error
	    end;
	Error -> Error
    end.

fdopen1(Protocol, Family, Fd) when is_integer(Fd) ->
    case open0(Protocol) of
	{ok, S} ->
	    case ctl_cmd(S,?INET_REQ_FDOPEN,[Family,?int32(Fd)]) of
		{ok, _} -> {ok,S};
		Error -> close(S), Error
	    end;
	Error -> Error
    end.

open0(Protocol) ->
    try	erlang:open_port({spawn_driver,protocol2drv(Protocol)}, [binary]) of
	Port -> {ok,Port}
    catch
	error:Reason -> {error,Reason}
    end.

protocol2drv(tcp)  -> "tcp_inet";
protocol2drv(udp)  -> "udp_inet";
protocol2drv(sctp) -> "sctp_inet";
protocol2drv(_) ->
    erlang:error(eprotonosupport).

drv2protocol("tcp_inet")  -> tcp;
drv2protocol("udp_inet")  -> udp;
drv2protocol("sctp_inet") -> sctp;
drv2protocol(_)           -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Shutdown(insock(), atom()) -> ok
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: shutdown equivalent for SCTP
%%
shutdown(S, read) when is_port(S) ->
    shutdown_2(S, 0);
shutdown(S, write) when is_port(S) ->
    shutdown_1(S, 1);
shutdown(S, read_write) when is_port(S) ->
    shutdown_1(S, 2).

shutdown_1(S, How) ->
    case subscribe(S, [subs_empty_out_q]) of
	{ok,[{subs_empty_out_q,N}]} when N > 0 ->
	    shutdown_pend_loop(S, N);   %% wait for pending output to be sent
	_Other -> ok
    end,
    shutdown_2(S, How).

shutdown_2(S, How) ->
    case ctl_cmd(S, ?TCP_REQ_SHUTDOWN, [How]) of
	{ok, []} -> ok;
	Error -> Error
    end.

shutdown_pend_loop(S, N0) ->
    receive
	{empty_out_q,S} -> ok
    after ?INET_CLOSE_TIMEOUT ->
	    case getstat(S, [send_pend]) of
                {ok,[{send_pend,N0}]} -> ok;
                {ok,[{send_pend,N}]} -> shutdown_pend_loop(S, N);
		_ -> ok
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CLOSE(insock()) -> ok
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close(S) when is_port(S) ->
    unlink(S),               %% avoid getting {'EXIT', S, Reason}
    case subscribe(S, [subs_empty_out_q]) of
	{ok, [{subs_empty_out_q,N}]} when N > 0 ->
	    close_pend_loop(S, N);   %% wait for pending output to be sent
	_ ->
	    catch erlang:port_close(S),
	    ok
    end.

close_pend_loop(S, N) ->
    receive
	{empty_out_q,S} ->
	    catch erlang:port_close(S), ok
    after ?INET_CLOSE_TIMEOUT ->
	    case getstat(S, [send_pend]) of
                {ok, [{send_pend,N1}]} ->
                    if N1 =:= N -> catch erlang:port_close(S), ok;
                       true -> close_pend_loop(S, N1)
                    end;
		_ ->
		    catch erlang:port_close(S), ok
	    end
    end.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% BIND(insock(), IP, Port) -> {ok, integer()} | {error, Reason}
%%
%% bind the insock() to the interface address given by IP and Port
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bind(S,IP,Port) when is_port(S), is_integer(Port), Port >= 0, Port =< 65535 ->
    case ctl_cmd(S,?INET_REQ_BIND,[?int16(Port),ip_to_bytes(IP)]) of
	{ok, [P1,P0]} -> {ok, ?u16(P1, P0)};
	Error -> Error
    end;

%% Multi-homed "bind": sctp_bindx(). The Op is 'add' or 'remove'.
%% If no addrs are specified, it just does nothing.
%% Function returns {ok, S} on success, unlike TCP/UDP "bind":
bind(S, Op, Addrs) when is_port(S), is_list(Addrs) ->
    case Op of
	add ->
	    bindx(S, 1, Addrs);
	remove ->
	    bindx(S, 0, Addrs);
	_ -> {error, einval}
    end;
bind(_, _, _) -> {error, einval}.

bindx(S, AddFlag, Addrs) ->
    case getprotocol(S) of
	sctp ->
	    %% Really multi-homed "bindx". Stringified args:
	    %% [AddFlag, (Port, IP)+]:
	    Args = ?int8(AddFlag) ++
		lists:concat([?int16(Port)++ip_to_bytes(IP) ||
				 {IP, Port} <- Addrs]),
	    case ctl_cmd(S, ?SCTP_REQ_BINDX, Args) of
		{ok,_} -> {ok, S};
		Error  -> Error
	    end;
	_ -> {error, einval}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CONNECT(insock(), IP, Port [,Timeout]) -> ok | {error, Reason}
%%
%% connect the insock() to the address given by IP and Port
%% if timeout is given:
%%       timeout < 0  -> infinity
%%                 0  -> immediate connect (mostly works for loopback)
%%               > 0  -> wait for timout ms if not connected then 
%%                       return {error, timeout} 
%%
%% ASYNC_CONNECT(insock(), IP, Port, Timeout) -> {ok, S, Ref} | {error, Reason}
%%
%%  a {inet_async,S,Ref,Status} will be sent on socket condition
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For TCP, UDP or SCTP sockets.
%%
connect(S, IP, Port) -> connect0(S, IP, Port, -1).

connect(S, IP, Port, infinity) -> connect0(S, IP, Port, -1);
connect(S, IP, Port, Time)     -> connect0(S, IP, Port, Time).

connect0(S, IP, Port, Time) when is_port(S), Port > 0, Port =< 65535,
				 is_integer(Time) ->
    case async_connect(S, IP, Port, Time) of
	{ok, S, Ref} ->
	    receive
		{inet_async, S, Ref, Status} ->
		    Status
	    end;
	Error -> Error
    end.

async_connect(S, IP, Port, Time) ->
    case ctl_cmd(S, ?INET_REQ_CONNECT,
		 [enc_time(Time),?int16(Port),ip_to_bytes(IP)]) of
	{ok, [R1,R0]} -> {ok, S, ?u16(R1,R0)};
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ACCEPT(insock() [,Timeout] ) -> {ok,insock()} | {error, Reason}
%%
%% accept incoming connection on listen socket 
%% if timeout is given:
%%       timeout < 0  -> infinity
%%                 0  -> immediate accept (poll)
%%               > 0  -> wait for timout ms for accept if no accept then 
%%                       return {error, timeout}
%%
%% ASYNC_ACCEPT(insock(), Timeout)
%%
%%  async accept. return {ok,S,Ref} or {error, Reason}
%%  the owner of socket S will receive an {inet_async,S,Ref,Status} on 
%%  socket condition
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For TCP sockets only.
%%
accept(L)            -> accept0(L, -1).

accept(L, infinity)  -> accept0(L, -1);
accept(L, Time)      -> accept0(L, Time).

accept0(L, Time) when is_port(L), is_integer(Time) ->
    case async_accept(L, Time) of
	{ok, Ref} ->
	    receive 
		{inet_async, L, Ref, {ok,S}} ->
		    accept_opts(L, S);
		{inet_async, L, Ref, Error} ->
		    Error
	    end;
	Error -> Error
    end.

%% setup options from listen socket on the connected socket
accept_opts(L, S) ->
    case getopts(L, [active, nodelay, keepalive, delay_send, priority, tos]) of
	{ok, Opts} ->
	    case setopts(S, Opts) of
		ok -> {ok, S};
		Error -> close(S), Error
	    end;
	Error ->
	    close(S), Error
    end.

async_accept(L, Time) ->
    case ctl_cmd(L,?TCP_REQ_ACCEPT, [enc_time(Time)]) of
	{ok, [R1,R0]} -> {ok, ?u16(R1,R0)};
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% LISTEN(insock() [,Backlog]) -> ok | {error, Reason}
%%
%% set listen mode on socket
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For TCP or SCTP sockets. For SCTP, Boolean backlog value (enable/disable
%% listening) is also accepted:

listen(S) -> listen(S, ?LISTEN_BACKLOG).
    
listen(S, BackLog) when is_port(S), is_integer(BackLog) ->
    case ctl_cmd(S, ?TCP_REQ_LISTEN, [?int16(BackLog)]) of
	{ok, _} -> ok;
	Error   -> Error
    end;
listen(S, Flag)   when is_port(S), is_boolean(Flag) ->
    case ctl_cmd(S, ?SCTP_REQ_LISTEN, enc_value(set, bool8, Flag)) of
	{ok,_} -> ok;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SEND(insock(), Data) -> ok | {error, Reason}
%%
%% send Data on the socket (io-list)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is a generic "port_command" interface used by TCP, UDP, SCTP, depending
%% on the driver it is mapped to, and the "Data". It actually sends out data,--
%% NOT delegating this task to any back-end.  For SCTP, this function MUST NOT
%% be called directly -- use "sendmsg" instead:
%%
send(S, Data, OptList) when is_port(S), is_list(OptList) ->
    ?DBG_FORMAT("prim_inet:send(~p, ~p)~n", [S,Data]),
    try erlang:port_command(S, Data, OptList) of
	false -> % Port busy and nosuspend option passed
	    ?DBG_FORMAT("prim_inet:send() -> {error,busy}~n", []),
	    {error,busy};
	true ->
	    receive
		{inet_reply,S,Status} ->
		    ?DBG_FORMAT("prim_inet:send() -> ~p~n", [Status]),
		    Status
	    end
    catch
	error:_Error ->
	    ?DBG_FORMAT("prim_inet:send() -> {error,einval}~n", []),
	     {error,einval}
    end.

send(S, Data) ->
    send(S, Data, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SENDTO(insock(), IP, Port, Data) -> ok | {error, Reason}
%%
%% send Datagram to the IP at port (Should add sync send!)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% "sendto" is for UDP. IP and Port are set by the caller to 0 if the socket
%% is known to be connected.

sendto(S, IP, Port, Data) when is_port(S), Port >= 0, Port =< 65535 ->
    ?DBG_FORMAT("prim_inet:sendto(~p, ~p, ~p, ~p)~n", [S,IP,Port,Data]),
    try erlang:port_command(S, [?int16(Port),ip_to_bytes(IP),Data]) of
	true -> 
	    receive
		{inet_reply,S,Reply} ->
		    ?DBG_FORMAT("prim_inet:send() -> ~p~n", [Reply]),
		     Reply
	    end
    catch
	error:_ ->
	    ?DBG_FORMAT("prim_inet:send() -> {error,einval}~n", []),
	     {error,einval}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SENDMSG(insock(), IP, Port, InitMsg, Data)   or
%% SENDMSG(insock(), SndRcvInfo,        Data)   -> ok | {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SCTP: Sending data over an existing association: no need for a destination
%% addr; uses SndRcvInfo:
%%
sendmsg(S, #sctp_sndrcvinfo{}=SRI, Data) when is_port(S) ->
    Type = type_opt(set, sctp_default_send_param),
    try type_value(set, Type, SRI) of
	true ->
	    send(S, [enc_value(set, Type, SRI)|Data]);
	false -> {error,einval}
    catch
	Reason -> {error,Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% RECV(insock(), Length, [Timeout]) -> {ok,Data} | {error, Reason}
%%
%% receive Length data bytes from a socket
%% if 0 is given then a Data packet is requested (see setopt (packet))
%%    N read N bytes
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% "recv" is for TCP:

recv(S, Length) -> recv0(S, Length, -1).

recv(S, Length, infinity) -> recv0(S, Length,-1);

recv(S, Length, Time) when is_integer(Time) -> recv0(S, Length, Time).

recv0(S, Length, Time) when is_port(S), is_integer(Length), Length >= 0 ->
    case async_recv(S, Length, Time) of
	{ok, Ref} ->
	    receive
		{inet_async, S, Ref, Status} -> Status;
		{'EXIT', S, _Reason} ->
		    {error, closed}
	    end;
	Error -> Error
    end.
	     

async_recv(S, Length, Time) ->
    case ctl_cmd(S, ?TCP_REQ_RECV, [enc_time(Time), ?int32(Length)]) of
	{ok,[R1,R0]} -> {ok, ?u16(R1,R0)};
	Error -> Error
    end.	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% RECVFROM(insock(), Lenth [Timeout]) -> {ok,{IP,Port,Data}} | {error, Reason}
%%                           For SCTP: -> {ok,{IP,Port,[AncData],Data}}
%%                                                            | {error, Reason}
%% receive Length data bytes from a datagram socket sent from IP at Port
%% if 0 is given then a Data packet is requested (see setopt (packet))
%%    N read N bytes
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% "recvfrom" is for both UDP and SCTP.
%% NB: "Length" is actually ignored for these protocols, since they are msg-
%% oriented: preserved here only for API compatibility.
%%
recvfrom(S, Length) ->
    recvfrom0(S, Length, -1).

recvfrom(S, Length, infinity) ->
    recvfrom0(S, Length, -1);
recvfrom(S, Length, Time) when is_integer(Time), Time < 16#ffffffff ->
    recvfrom0(S, Length, Time);
recvfrom(_, _, _) -> {error,einval}.

recvfrom0(S, Length, Time)
  when is_port(S), is_integer(Length), Length >= 0, Length =< 16#ffffffff ->
    case ctl_cmd(S, ?PACKET_REQ_RECV,[enc_time(Time),?int32(Length)]) of
	{ok,[R1,R0]} ->
	    Ref = ?u16(R1,R0),
	    receive
		% Success, UDP:
		{inet_async, S, Ref, {ok, [F,P1,P0 | AddrData]}} ->
		    {IP,Data} = get_ip(F, AddrData),
		    {ok, {IP, ?u16(P1,P0), Data}};

		% Success, SCTP:
		{inet_async, S, Ref, {ok, {[F,P1,P0 | Addr], AncData, DE}}} ->
		    {IP, _}   = get_ip(F, Addr),
		    {ok, {IP, ?u16(P1,P0), AncData, DE}};

		% Back-end error:
		{inet_async, S, Ref, Error={error, _}} ->
		    Error
	    end;
	Error ->
	    Error % Front-end error
    end;
recvfrom0(_, _, _) -> {error,einval}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% PEERNAME(insock()) -> {ok, {IP, Port}} | {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

peername(S) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_PEER, []) of
	{ok, [F, P1,P0 | Addr]} ->
	    {IP, _} = get_ip(F, Addr),
	    {ok, { IP, ?u16(P1, P0) }};
	Error -> Error
    end.

setpeername(S, {IP,Port}) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_SETPEER, [?int16(Port),ip_to_bytes(IP)]) of
	{ok,[]} -> ok;
	Error -> Error
    end;
setpeername(S, undefined) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_SETPEER, []) of
	{ok,[]} -> ok;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SOCKNAME(insock()) -> {ok, {IP, Port}} | {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sockname(S) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_NAME, []) of
	{ok, [F, P1, P0 | Addr]} ->
	    {IP, _} = get_ip(F, Addr),
	    {ok, { IP, ?u16(P1, P0) }};
	Error -> Error
    end.

setsockname(S, {IP,Port}) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_SETNAME, [?int16(Port),ip_to_bytes(IP)]) of
	{ok,[]} -> ok;
	Error -> Error
    end;
setsockname(S, undefined) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_SETNAME, []) of
	{ok,[]} -> ok;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SETOPT(insock(), Opt, Value) -> ok | {error, Reason}
%% SETOPTS(insock(), [{Opt,Value}]) -> ok | {error, Reason}
%%
%% set socket, ip and driver option
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setopt(S, Opt, Value) when is_port(S) -> 
    setopts(S, [{Opt,Value}]).

setopts(S, Opts) when is_port(S) ->
    case encode_opt_val(Opts) of
	{ok, Buf} ->
	    case ctl_cmd(S, ?INET_REQ_SETOPTS, Buf) of
		{ok, _} -> ok;
		Error -> Error
	    end;
	Error  -> Error
    end.	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETOPT(insock(), Opt) -> {ok,Value} | {error, Reason}
%% GETOPTS(insock(), [Opt]) -> {ok, [{Opt,Value}]} | {error, Reason}
%% get socket, ip and driver option
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getopt(S, Opt) when is_port(S), is_atom(Opt) ->
    case getopts(S, [Opt]) of
	{ok,[{_,Value}]} -> {ok, Value};
	Error -> Error
    end.

getopts(S, Opts) when is_port(S), is_list(Opts) ->
    case encode_opts(Opts) of
	{ok,Buf} ->
	    case ctl_cmd(S, ?INET_REQ_GETOPTS, Buf) of
		{ok,Rep} ->
		    %% Non-SCTP: "Rep" contains the encoded option vals:
		    decode_opt_val(Rep);
		{error,sctp_reply} ->
		    %% SCTP: Need to receive the full value:
		    receive
			{inet_reply,S,Res} -> Res
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CHGOPT(insock(), Opt) -> {ok,Value} | {error, Reason}
%% CHGOPTS(insock(), [Opt]) -> {ok, [{Opt,Value}]} | {error, Reason}
%% change socket, ip and driver option
%%
%% Same as setopts except for record value options where undefined
%% fields are read with getopts before setting.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chgopt(S, Opt, Value) when is_port(S) -> 
    chgopts(S, [{Opt,Value}]).

chgopts(S, Opts) when is_port(S), is_list(Opts) ->
    case inet:getopts(S, need_template(Opts)) of
	{ok,Templates} ->
	    try merge_options(Opts, Templates) of
		NewOpts ->
		    setopts(S, NewOpts)
	    catch
		Reason -> {error,Reason}
	    end;
	Error -> Error
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% IFLIST(insock()) -> {ok,IfNameList} | {error, Reason}
%%
%% get interface name list
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getiflist(S) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_GETIFLIST, []) of
	{ok, Data} -> {ok, build_iflist(Data)};
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ifget(insock(), IFOpts) -> {ok,IfNameList} | {error, Reason}
%%
%% get interface name list
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ifget(S, Name, Opts) ->
    case encode_ifname(Name) of
	{ok, Buf1} ->
	    case encode_ifopts(Opts,[]) of
		{ok, Buf2} ->
		    case ctl_cmd(S, ?INET_REQ_IFGET, [Buf1,Buf2]) of
			{ok, Data} -> decode_ifopts(Data,[]);
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ifset(insock(), Name, IFOptVals) -> {ok,IfNameList} | {error, Reason}
%%
%% set interface parameters
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ifset(S, Name, Opts) ->
    case encode_ifname(Name) of
	{ok, Buf1} ->
	    case encode_ifopt_val(Opts,[]) of
		{ok, Buf2} ->
		    case ctl_cmd(S, ?INET_REQ_IFSET, [Buf1,Buf2]) of
			{ok, _} -> ok;
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% subscribe(insock(), SubsList) -> {ok,StatReply} | {error, Reason}
%%
%% Subscribe on socket events (from driver)
%%
%% Available event subscriptions:
%%   subs_empty_out_q: StatReply = [{subs_empty_out_q, N}], where N
%%                     is current queue length. When the queue becomes empty
%%                     a {empty_out_q, insock()} message will be sent to
%%                     subscribing process and the subscription will be
%%                     removed. If N = 0, the queue is empty and no
%%                     subscription is made.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subscribe(S, Sub) when is_port(S), is_list(Sub) ->
    case encode_subs(Sub) of
	{ok, Bytes} ->
	    case ctl_cmd(S, ?INET_REQ_SUBSCRIBE, Bytes) of
		{ok, Data} -> decode_subs(Data);
		Error -> Error
	    end;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETSTAT(insock(), StatList) -> {ok,StatReply} | {error, Reason}
%%
%% get socket statistics (from driver)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getstat(S, Stats) when is_port(S), is_list(Stats) ->
    case encode_stats(Stats) of
	{ok, Bytes} ->
	    case ctl_cmd(S, ?INET_REQ_GETSTAT, Bytes) of
		{ok, Data} -> decode_stats(Data);
		Error -> Error
	    end;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETFD(insock()) -> {ok,integer()} | {error, Reason}
%%
%% get internal file descriptor
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getfd(S) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_GETFD, []) of
	{ok, [S3,S2,S1,S0]} -> {ok, ?u32(S3,S2,S1,S0)};
	Error -> Error
    end.        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETIX(insock()) -> {ok,integer()} | {error, Reason}
%%
%% get internal socket index
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getindex(S) when is_port(S) ->
    %% NOT USED ANY MORE
    {error, einval}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETTYPE(insock()) -> {ok,{Family,Type}} | {error, Reason}
%%
%% get family/type of a socket
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gettype(S) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_GETTYPE, []) of
	{ok, [F3,F2,F1,F0,T3,T2,T1,T0]} ->
	    Family = case ?u32(F3,F2,F1,F0) of
			 ?INET_AF_INET  ->  inet;
			 ?INET_AF_INET6 ->  inet6;
			 _ -> undefined
		     end,
	    Type = case ?u32(T3,T2,T1,T0) of
			?INET_TYPE_STREAM    -> stream;
			?INET_TYPE_DGRAM     -> dgram;
			?INET_TYPE_SEQPACKET -> seqpacket;
			_		     -> undefined
		   end,
	    {ok, {Family, Type}};
	Error -> Error
    end.

getprotocol(S) when is_port(S) ->
    {name,Drv} = erlang:port_info(S, name),
    drv2protocol(Drv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  IS_SCTP(insock()) -> true | false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_sctp(S) when is_port(S) ->
%%     case gettype(S) of
%% 	{ok, {_, seqpacket}} -> true;
%% 	_		     -> false
%%     end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETSTATUS(insock()) -> {ok,Status} | {error, Reason}
%%
%% get socket status
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getstatus(S) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_GETSTATUS, []) of
	{ok, [S3,S2,S1,S0]} ->	
	    {ok, dec_status(?u32(S3,S2,S1,S0))};
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETHOSTNAME(insock()) -> {ok,HostName} | {error, Reason}
%%
%% get host name
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gethostname(S) when is_port(S) ->
    ctl_cmd(S, ?INET_REQ_GETHOSTNAME, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETSERVBYNAME(insock(),Name,Proto) -> {ok,Port} | {error, Reason}
%%
%% get service port
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getservbyname(S,Name,Proto) when is_port(S), is_atom(Name), is_atom(Proto) ->
    getservbyname1(S, atom_to_list(Name), atom_to_list(Proto));
getservbyname(S,Name,Proto) when is_port(S), is_atom(Name), is_list(Proto) ->
    getservbyname1(S, atom_to_list(Name), Proto);
getservbyname(S,Name,Proto) when is_port(S), is_list(Name), is_atom(Proto) ->
    getservbyname1(S, Name, atom_to_list(Proto));
getservbyname(S,Name,Proto) when is_port(S), is_list(Name), is_list(Proto) ->
    getservbyname1(S, Name, Proto);
getservbyname(_,_, _) ->
    {error, einval}.

getservbyname1(S,Name,Proto) ->
    L1 = length(Name),
    L2 = length(Proto),
    if L1 > 255 -> {error, einval};
       L2 > 255 -> {error, einval};
       true ->
	    case ctl_cmd(S, ?INET_REQ_GETSERVBYNAME, [L1,Name,L2,Proto]) of
		{ok, [P1,P0]} ->
		    {ok, ?u16(P1,P0)};
		Error -> 
		    Error
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETSERVBYPORT(insock(),Port,Proto) -> {ok,Port} | {error, Reason}
%%
%% get service port from portnumber and protocol
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getservbyport(S,Port,Proto) when is_port(S), is_atom(Proto) ->
    getservbyport1(S, Port, atom_to_list(Proto));
getservbyport(S,Port,Proto) when is_port(S), is_list(Proto) ->
    getservbyport1(S, Port, Proto);
getservbyport(_, _, _) ->
    {error, einval}.

getservbyport1(S,Port,Proto) ->
    L = length(Proto),
    if Port < 0 -> {error, einval};
       Port > 16#ffff -> {error, einval};
       L > 255 -> {error, einval};
       true ->
	    case ctl_cmd(S, ?INET_REQ_GETSERVBYPORT, [?int16(Port),L,Proto]) of
		{ok, Name} -> {ok, Name};
		Error -> Error
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% UNRECV(insock(), data) -> ok | {error, Reason}
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unrecv(S, Data) ->
    case ctl_cmd(S, ?TCP_REQ_UNRECV, Data) of
	{ok, _} -> ok;
	Error  -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% DETACH(insock()) -> ok
%%
%%   unlink from a socket 
%%
%% ATTACH(insock()) -> ok | {error, Reason}
%%
%%   link and connect to a socket 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

detach(S) when is_port(S) ->
    unlink(S),
    ok.

attach(S) when is_port(S) ->
    try erlang:port_connect(S, self()) of
	true -> link(S), ok
    catch
	error:Reason -> {error,Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% INTERNAL FUNCTIONS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_sockopt_val(Opt, Val) ->
    Type = type_opt(set, Opt),
    try type_value(set, Type, Val)
    catch
	_ -> false
    end.

%%
%% Socket options processing: Encoding option NAMES:
%%
enc_opt(reuseaddr)       -> ?INET_OPT_REUSEADDR;
enc_opt(keepalive)       -> ?INET_OPT_KEEPALIVE;
enc_opt(dontroute)       -> ?INET_OPT_DONTROUTE;
enc_opt(linger)          -> ?INET_OPT_LINGER;
enc_opt(broadcast)       -> ?INET_OPT_BROADCAST;
enc_opt(sndbuf)          -> ?INET_OPT_SNDBUF;
enc_opt(recbuf)          -> ?INET_OPT_RCVBUF;
enc_opt(priority)        -> ?INET_OPT_PRIORITY;
enc_opt(tos)             -> ?INET_OPT_TOS;
enc_opt(nodelay)         -> ?TCP_OPT_NODELAY;
enc_opt(multicast_if)    -> ?UDP_OPT_MULTICAST_IF;
enc_opt(multicast_ttl)   -> ?UDP_OPT_MULTICAST_TTL;
enc_opt(multicast_loop)  -> ?UDP_OPT_MULTICAST_LOOP;
enc_opt(add_membership)  -> ?UDP_OPT_ADD_MEMBERSHIP;
enc_opt(drop_membership) -> ?UDP_OPT_DROP_MEMBERSHIP;
enc_opt(buffer)          -> ?INET_LOPT_BUFFER;
enc_opt(header)          -> ?INET_LOPT_HEADER;
enc_opt(active)          -> ?INET_LOPT_ACTIVE;
enc_opt(packet)          -> ?INET_LOPT_PACKET;
enc_opt(mode)            -> ?INET_LOPT_MODE;
enc_opt(deliver)         -> ?INET_LOPT_DELIVER;
enc_opt(exit_on_close)   -> ?INET_LOPT_EXITONCLOSE;
enc_opt(high_watermark)  -> ?INET_LOPT_TCP_HIWTRMRK;
enc_opt(low_watermark)   -> ?INET_LOPT_TCP_LOWTRMRK;
enc_opt(bit8)            -> ?INET_LOPT_BIT8;
enc_opt(send_timeout)    -> ?INET_LOPT_TCP_SEND_TIMEOUT;
enc_opt(send_timeout_close) -> ?INET_LOPT_TCP_SEND_TIMEOUT_CLOSE;
enc_opt(delay_send)      -> ?INET_LOPT_TCP_DELAY_SEND;
enc_opt(packet_size)     -> ?INET_LOPT_PACKET_SIZE;
enc_opt(read_packets)    -> ?INET_LOPT_READ_PACKETS;
enc_opt(raw)             -> ?INET_OPT_RAW;
% Names of SCTP opts:
enc_opt(sctp_rtoinfo)	 	   -> ?SCTP_OPT_RTOINFO;
enc_opt(sctp_associnfo)	 	   -> ?SCTP_OPT_ASSOCINFO;
enc_opt(sctp_initmsg)	 	   -> ?SCTP_OPT_INITMSG;
enc_opt(sctp_autoclose)	 	   -> ?SCTP_OPT_AUTOCLOSE;
enc_opt(sctp_nodelay)		   -> ?SCTP_OPT_NODELAY;
enc_opt(sctp_disable_fragments)	   -> ?SCTP_OPT_DISABLE_FRAGMENTS;
enc_opt(sctp_i_want_mapped_v4_addr)-> ?SCTP_OPT_I_WANT_MAPPED_V4_ADDR;
enc_opt(sctp_maxseg)		   -> ?SCTP_OPT_MAXSEG;
enc_opt(sctp_set_peer_primary_addr)-> ?SCTP_OPT_SET_PEER_PRIMARY_ADDR;
enc_opt(sctp_primary_addr)	   -> ?SCTP_OPT_PRIMARY_ADDR;
enc_opt(sctp_adaptation_layer)	   -> ?SCTP_OPT_ADAPTATION_LAYER;
enc_opt(sctp_peer_addr_params)	   -> ?SCTP_OPT_PEER_ADDR_PARAMS;
enc_opt(sctp_default_send_param)   -> ?SCTP_OPT_DEFAULT_SEND_PARAM;
enc_opt(sctp_events)		   -> ?SCTP_OPT_EVENTS;
enc_opt(sctp_delayed_ack_time)	   -> ?SCTP_OPT_DELAYED_ACK_TIME;
enc_opt(sctp_status)		   -> ?SCTP_OPT_STATUS;
enc_opt(sctp_get_peer_addr_info)   -> ?SCTP_OPT_GET_PEER_ADDR_INFO.
%%

%%
%% Decoding option NAMES:
%%
dec_opt(?INET_OPT_REUSEADDR)      -> reuseaddr;
dec_opt(?INET_OPT_KEEPALIVE)      -> keepalive;
dec_opt(?INET_OPT_DONTROUTE)      -> dontroute;
dec_opt(?INET_OPT_LINGER)         -> linger;
dec_opt(?INET_OPT_BROADCAST)      -> broadcast;
dec_opt(?INET_OPT_SNDBUF)         -> sndbuf;
dec_opt(?INET_OPT_RCVBUF)         -> recbuf;
dec_opt(?INET_OPT_PRIORITY)       -> priority;
dec_opt(?INET_OPT_TOS)            -> tos;
dec_opt(?TCP_OPT_NODELAY)         -> nodelay;
dec_opt(?UDP_OPT_MULTICAST_IF)    -> multicast_if;
dec_opt(?UDP_OPT_MULTICAST_TTL)   -> multicast_ttl;
dec_opt(?UDP_OPT_MULTICAST_LOOP)  -> multicast_loop;
dec_opt(?UDP_OPT_ADD_MEMBERSHIP)  -> add_membership;
dec_opt(?UDP_OPT_DROP_MEMBERSHIP) -> drop_membership;
dec_opt(?INET_LOPT_BUFFER)        -> buffer;
dec_opt(?INET_LOPT_HEADER)        -> header;
dec_opt(?INET_LOPT_ACTIVE)        -> active;
dec_opt(?INET_LOPT_PACKET)        -> packet;
dec_opt(?INET_LOPT_MODE)          -> mode;
dec_opt(?INET_LOPT_DELIVER)       -> deliver;
dec_opt(?INET_LOPT_EXITONCLOSE)   -> exit_on_close;
dec_opt(?INET_LOPT_TCP_HIWTRMRK)  -> high_watermark;
dec_opt(?INET_LOPT_TCP_LOWTRMRK)  -> low_watermark;
dec_opt(?INET_LOPT_BIT8)          -> bit8;
dec_opt(?INET_LOPT_TCP_SEND_TIMEOUT) -> send_timeout;
dec_opt(?INET_LOPT_TCP_SEND_TIMEOUT_CLOSE) -> send_timeout_close;
dec_opt(?INET_LOPT_TCP_DELAY_SEND)   -> delay_send;
dec_opt(?INET_LOPT_PACKET_SIZE)      -> packet_size;
dec_opt(?INET_LOPT_READ_PACKETS)     -> read_packets;
dec_opt(?INET_OPT_RAW)              -> raw;
dec_opt(I) when is_integer(I)     -> undefined.



%% Metatypes:
%% []              Value must be 'undefined' or nonexistent
%%                 for setopts and getopts.
%% [Type]          Value required for setopts and getopts,
%%                 will be encoded for both.
%% [Type,Default]  Default used if value is 'undefined'.
%% [[Type,Default]] A combination of the two above.
%% Type            Value must be 'undefined' or nonexistent for getops,
%%                 required for setopts.
%%
%% The use of [] and [[Type,Default]] is commented out in enc_value/2
%% and type_value/2 below since they are only used in record fields.
%% And record fields does not call enc_value/2 nor type_value/2.
%% Anyone introducing these metatypes otherwhere will have to activate
%% those clauses in enc_value/2 and type_value/2. You have been warned!

type_opt(get, raw) -> [{[int],[int],[binary_or_uint]}];
type_opt(_,   raw) -> {int,int,binary};
%% NB: "sctp_status" and "sctp_get_peer_addr_info" are read-only options,
%% so they should not be NOT encoded for use with "setopt".
type_opt(get, sctp_status) ->
    [{record,#sctp_status{
	assoc_id = [sctp_assoc_id],
	_        = []}}];
type_opt(get, sctp_get_peer_addr_info) ->
    [{record,#sctp_paddrinfo{
	assoc_id = [[sctp_assoc_id,0]],
	address  = [[addr,{any,0}]],
	_        = []}}];
type_opt(_,   Opt) ->
    type_opt_1(Opt).

%% Types of option values, by option name:
%%
type_opt_1(reuseaddr)       -> bool;
type_opt_1(keepalive)       -> bool;
type_opt_1(dontroute)       -> bool;
type_opt_1(linger)          -> {bool,int};
type_opt_1(broadcast)       -> bool;
type_opt_1(sndbuf)          -> int;
type_opt_1(recbuf)          -> int;
type_opt_1(priority)        -> int;
type_opt_1(tos)             -> int;
type_opt_1(nodelay)         -> bool;
%% multicast
type_opt_1(multicast_ttl)   -> int;
type_opt_1(multicast_loop)  -> bool;
type_opt_1(multicast_if)    -> ip;
type_opt_1(add_membership)  -> {ip,ip};
type_opt_1(drop_membership) -> {ip,ip};
%% driver options
type_opt_1(header)          -> uint;
type_opt_1(buffer)          -> int;
type_opt_1(active) ->
    {enum,[{false, ?INET_PASSIVE}, 
	   {true, ?INET_ACTIVE}, 
	   {once, ?INET_ONCE}]};
type_opt_1(packet) -> 
    {enum,[{0, ?TCP_PB_RAW},
	   {1, ?TCP_PB_1},
	   {2, ?TCP_PB_2},
	   {4, ?TCP_PB_4},
	   {raw,?TCP_PB_RAW},
	   {sunrm, ?TCP_PB_RM},
	   {asn1, ?TCP_PB_ASN1},
	   {cdr, ?TCP_PB_CDR},
	   {fcgi, ?TCP_PB_FCGI},
	   {line, ?TCP_PB_LINE_LF},
	   {tpkt, ?TCP_PB_TPKT},
	   {http, ?TCP_PB_HTTP},
	   {httph,?TCP_PB_HTTPH},
	   {http_bin, ?TCP_PB_HTTP_BIN},
	   {httph_bin,?TCP_PB_HTTPH_BIN},
	   {ssl, ?TCP_PB_SSL_TLS}, % obsolete
	   {ssl_tls, ?TCP_PB_SSL_TLS}]};
type_opt_1(mode) ->
    {enum,[{list, ?INET_MODE_LIST},
	   {binary, ?INET_MODE_BINARY}]};
type_opt_1(deliver) ->
    {enum,[{port, ?INET_DELIVER_PORT},
	   {term, ?INET_DELIVER_TERM}]};
type_opt_1(exit_on_close)   -> bool;
type_opt_1(low_watermark)   -> int;
type_opt_1(high_watermark)  -> int;
type_opt_1(bit8) ->
    {enum,[{clear, ?INET_BIT8_CLEAR},
	   {set,   ?INET_BIT8_SET},
	   {on,    ?INET_BIT8_ON},
	   {off,   ?INET_BIT8_OFF}]};
type_opt_1(send_timeout)    -> time;
type_opt_1(send_timeout_close) -> bool;
type_opt_1(delay_send)      -> bool;
type_opt_1(packet_size)     -> uint;
type_opt_1(read_packets)    -> uint;
%% 
%% SCTP options (to be set). If the type is a record type, the corresponding
%% record signature is returned, otherwise, an "elementary" type tag 
%% is returned:
%% 
%% for SCTP_OPT_RTOINFO
type_opt_1(sctp_rtoinfo) ->
    [{record,#sctp_rtoinfo{
	assoc_id = [[sctp_assoc_id,0]],
	initial  = [uint32,0],
	max      = [uint32,0],
	min      = [uint32,0]}}];
%% for SCTP_OPT_ASSOCINFO
type_opt_1(sctp_associnfo) ->
    [{record,#sctp_assocparams{
	assoc_id                 = [[sctp_assoc_id,0]],
	asocmaxrxt               = [uint16,0],
	number_peer_destinations = [uint16,0],
	peer_rwnd                = [uint32,0],
	local_rwnd               = [uint32,0],
	cookie_life              = [uint32,0]}}];
%% for SCTP_OPT_INITMSG and SCTP_TAG_SEND_ANC_INITMSG (send*)
type_opt_1(sctp_initmsg) ->
    [{record,#sctp_initmsg{
	num_ostreams   = [uint16,0],
	max_instreams  = [uint16,0],
	max_attempts   = [uint16,0],
	max_init_timeo = [uint16,0]}}];
%%
type_opt_1(sctp_nodelay)	       -> bool;
type_opt_1(sctp_autoclose)	       -> uint;
type_opt_1(sctp_disable_fragments)     -> bool;
type_opt_1(sctp_i_want_mapped_v4_addr) -> bool;
type_opt_1(sctp_maxseg)		       -> uint;
%% for SCTP_OPT_PRIMARY_ADDR
type_opt_1(sctp_primary_addr) ->
    [{record,#sctp_prim{
	assoc_id = [sctp_assoc_id],
	addr     = addr}}];
%% for SCTP_OPT_SET_PEER_PRIMARY_ADDR
type_opt_1(sctp_set_peer_primary_addr) ->
    [{record,#sctp_setpeerprim{
	assoc_id = [sctp_assoc_id],
	addr     = addr}}];
%% for SCTP_OPT_ADAPTATION_LAYER
type_opt_1(sctp_adaptation_layer) ->
    [{record,#sctp_setadaptation{
	adaptation_ind = [uint32,0]}}];
%% for SCTP_OPT_PEER_ADDR_PARAMS
type_opt_1(sctp_peer_addr_params) ->
    [{record,#sctp_paddrparams{
	assoc_id          = [[sctp_assoc_id,0]],
	address           = [[addr,{any,0}]],
	hbinterval        = [uint32,0],
	pathmaxrxt        = [uint16,0],
	pathmtu           = [uint32,0],
	sackdelay         = [uint32,0],
	flags             =
	[{bitenumlist,
	  [{hb_enable,         ?SCTP_FLAG_HB_ENABLE},
	   {hb_disable,        ?SCTP_FLAG_HB_DISABLE},
	   {hb_demand,         ?SCTP_FLAG_HB_DEMAND},
	   {pmtud_enable,      ?SCTP_FLAG_PMTUD_ENABLE},
	   {pmtud_disable,     ?SCTP_FLAG_PMTUD_DISABLE},
	   {sackdelay_enable,  ?SCTP_FLAG_SACKDELAY_ENABLE},
	   {sackdelay_disable, ?SCTP_FLAG_SACKDELAY_DISABLE}],
	  uint32},[]]}}];
%% for SCTP_OPT_DEFAULT_SEND_PARAM and SCTP_TAG_SEND_ANC_PARAMS (on send*)
type_opt_1(sctp_default_send_param) ->
    [{record,#sctp_sndrcvinfo{
	stream            = [uint16,0],
	ssn               = [],
	flags             =
	[{bitenumlist,
	  [{unordered,  ?SCTP_FLAG_UNORDERED},
	   {addr_over,  ?SCTP_FLAG_ADDR_OVER},
	   {abort,      ?SCTP_FLAG_ABORT},
	   {eof,        ?SCTP_FLAG_EOF}],
	  uint16},[]],
	ppid              = [uint32,0],
	context           = [uint32,0],
	timetolive        = [uint32,0],
	tsn               = [],
	cumtsn            = [],
	assoc_id          = [sctp_assoc_id,0]}}];
%% for SCTP_OPT_EVENTS
type_opt_1(sctp_events) ->
    [{record,#sctp_event_subscribe{
	data_io_event          = [bool8,true],
        association_event      = [bool8,true], 
	address_event          = [bool8,true], 
	send_failure_event     = [bool8,true], 
	peer_error_event       = [bool8,true], 
	shutdown_event         = [bool8,true], 
	partial_delivery_event = [bool8,true], 
	adaptation_layer_event = [bool8,false],
	authentication_event   = [bool8,false]}}];
%% for SCTP_OPT_DELAYED_ACK_TIME
type_opt_1(sctp_delayed_ack_time) ->
    [{record,#sctp_assoc_value{
	assoc_id    = [[sctp_assoc_id,0]],
	assoc_value = [uint32,0]}}];
%%
type_opt_1(undefined)         -> undefined;
type_opt_1(O) when is_atom(O) -> undefined.



%% Get. No supplied value.
type_value(get, undefined)        -> false; % Undefined type
%% These two clauses can not happen since they are only used
%% in record fields - from record fields they must have a
%% value though it might be 'undefined', so record fields
%% calls type_value/3, not type_value/2.
%% type_value(get, [])               -> true;  % Ignored
%% type_value(get, [[Type,Default]]) ->        % Required field, default value
%%     type_value(get, Type, Default);
type_value(get, [{record,Types}]) ->        % Implied default value for record
    type_value_record(get, Types, 
		      erlang:make_tuple(tuple_size(Types), undefined), 2);
type_value(get, [_])              -> false; % Required value missing
type_value(get, _)                -> true.  % Field is supposed to be undefined

%% Get and set. Value supplied.
type_value(_, undefined, _)   -> false;     % Undefined type
type_value(_, [], undefined)  -> true;      % Ignored
type_value(_, [], _)          -> false;     % Value should not be supplied
type_value(Q, [Type], Value)  ->            % Required field, proceed
    type_value_default(Q, Type, Value);
type_value(set, Type, Value)  ->            % Required for setopts
    type_value_default(set, Type, Value);
type_value(_, _, undefined) -> true;        % Value should be undefined for
type_value(_, _, _)         -> false.       %   other than setopts.

type_value_default(Q, [Type,Default], undefined) ->
    type_value_1(Q, Type, Default);
type_value_default(Q, [Type,_], Value) ->
    type_value_1(Q, Type, Value);
type_value_default(Q, Type, Value) ->
    type_value_1(Q, Type, Value).

type_value_1(Q, {record,Types}, undefined) ->
    type_value_record(Q, Types,
		      erlang:make_tuple(tuple_size(Types), undefined), 2);
type_value_1(Q, {record,Types}, Values)
  when tuple_size(Types) =:= tuple_size(Values) ->
    type_value_record(Q, Types, Values, 2);
type_value_1(Q, Types, Values)
  when tuple_size(Types) =:= tuple_size(Values) ->
    type_value_tuple(Q, Types, Values, 1);
type_value_1(_, Type, Value) ->
    type_value_2(Type, Value).

type_value_tuple(Q, Types, Values, N)
  when is_integer(N), N =< tuple_size(Types) ->
    type_value(Q, element(N, Types), element(N, Values)) 
	andalso type_value_tuple(Q, Types, Values, N+1);
type_value_tuple(_, _, _, _) -> true.

type_value_record(Q, Types, Values, N)
  when is_integer(N), N =< tuple_size(Types) ->
    case type_value(Q, element(N, Types), element(N, Values)) of
	true -> type_value_record(Q, Types, Values, N+1);
	false ->
	    erlang:throw({type,{record,Q,Types,Values,N}})
    end;
type_value_record(_, _, _, _) -> true.

%% Simple run-time type-checking of (option) values: type -vs- value:
%% NB: the LHS is the TYPE, not the option name!
%% 
%% Returns true | false | throw(ErrorReason) only for record types
%% 
type_value_2(undefined, _)                            -> false;
%% 
type_value_2(bool, true)                              -> true;
type_value_2(bool, false)                             -> true;
type_value_2(bool8, true)                             -> true;
type_value_2(bool8, false)                            -> true;
type_value_2(int, X) when is_integer(X)               -> true;
type_value_2(uint, X) when is_integer(X), X >= 0      -> true;
type_value_2(uint32, X) when X band 16#ffffffff =:= X -> true;
type_value_2(uint24, X) when X band 16#ffffff =:= X   -> true;
type_value_2(uint16, X) when X band 16#ffff =:= X     -> true;
type_value_2(uint8, X)  when X band 16#ff =:= X       -> true;
type_value_2(time, infinity)                          -> true;
type_value_2(time, X) when is_integer(X), X >= 0      -> true;
type_value_2(ip,{A,B,C,D}) when ?ip(A,B,C,D)          -> true;
type_value_2(addr, {any,Port}) ->
    type_value_2(uint16, Port);
type_value_2(addr, {loopback,Port}) ->
    type_value_2(uint16, Port);
type_value_2(addr, {{A,B,C,D},Port}) when ?ip(A,B,C,D) ->
    type_value_2(uint16, Port);
type_value_2(addr, {{A,B,C,D,E,F,G,H},Port}) when ?ip6(A,B,C,D,E,F,G,H) ->
    type_value_2(uint16, Port);
type_value_2(ether,[X1,X2,X3,X4,X5,X6])
  when ?ether(X1,X2,X3,X4,X5,X6)                    -> true;
type_value_2({enum,List}, Enum) -> 
    case enum_val(Enum, List) of
	{value,_} 				    -> true;
	false                                       -> false
    end;
type_value_2({bitenumlist,List}, EnumList) -> 
    case enum_vals(EnumList, List) of
	Ls when is_list(Ls)                         -> true;
	false                                       -> false
    end;
type_value_2({bitenumlist,List,_}, EnumList) -> 
    case enum_vals(EnumList, List) of
	Ls when is_list(Ls)                         -> true;
	false                                       -> false
    end;
type_value_2(binary,Bin) when is_binary(Bin) -> true;
type_value_2(binary_or_uint,Bin) when is_binary(Bin) -> true;
type_value_2(binary_or_uint,Int) when is_integer(Int), Int >= 0 -> true;
%% Type-checking of SCTP options
type_value_2(sctp_assoc_id, X)
  when X band 16#ffffffff =:= X                     -> true;
type_value_2(_, _)         -> false.



%% Get. No supplied value.
%%
%% These two clauses can not happen since they are only used
%% in record fields - from record fields they must have a
%% value though it might be 'undefined', so record fields
%% calls enc_value/3, not enc_value/2.
%% enc_value(get, [])               -> [];  % Ignored
%% enc_value(get, [[Type,Default]]) ->      % Required field, default value
%%     enc_value(get, Type, Default);
enc_value(get, [{record,Types}]) ->      % Implied default value for record
    enc_value_tuple(get, Types, 
		    erlang:make_tuple(tuple_size(Types), undefined), 2);
enc_value(get, _)                -> [].
    
%% Get and set
enc_value(_,   [], _)         -> [];     % Ignored
enc_value(Q,   [Type], Value) ->         % Required field, proceed
    enc_value_default(Q, Type, Value);
enc_value(set, Type, Value)   ->         % Required for setopts
    enc_value_default(set, Type, Value);
enc_value(_, _, _)            -> [].     % Not encoded for other than setopts

enc_value_default(Q, [Type,Default], undefined) ->
    enc_value_1(Q, Type, Default);
enc_value_default(Q, [Type,_], Value) ->
    enc_value_1(Q, Type, Value);
enc_value_default(Q, Type, Value) ->
    enc_value_1(Q, Type, Value).

enc_value_1(Q, {record,Types}, undefined) ->
    enc_value_tuple(Q, Types, 
		    erlang:make_tuple(tuple_size(Types), undefined), 2);
enc_value_1(Q, {record,Types}, Values)
  when tuple_size(Types) =:= tuple_size(Values) ->
    enc_value_tuple(Q, Types, Values, 2);
enc_value_1(Q, Types, Values) when tuple_size(Types) =:= tuple_size(Values) ->
    enc_value_tuple(Q, Types, Values, 1);
enc_value_1(_, Type, Value) ->
    enc_value_2(Type, Value).

enc_value_tuple(Q, Types, Values, N)
  when is_integer(N), N =< tuple_size(Types) ->
    [enc_value(Q, element(N, Types), element(N, Values))
     |enc_value_tuple(Q, Types, Values, N+1)];
enc_value_tuple(_, _, _, _) -> [].

%%
%% Encoding of option VALUES:
%%
enc_value_2(bool, true)     -> [0,0,0,1];
enc_value_2(bool, false)    -> [0,0,0,0];
enc_value_2(bool8, true)    -> [1];
enc_value_2(bool8, false)   -> [0];
enc_value_2(int, Val)       -> ?int32(Val);
enc_value_2(uint, Val)      -> ?int32(Val);
enc_value_2(uint32, Val)    -> ?int32(Val);
enc_value_2(uint24, Val)    -> ?int24(Val);
enc_value_2(uint16, Val)    -> ?int16(Val);
enc_value_2(uint8, Val)     -> ?int8(Val);
enc_value_2(time, infinity) -> ?int32(-1);
enc_value_2(time, Val)      -> ?int32(Val);
enc_value_2(ip,{A,B,C,D})   -> [A,B,C,D];
enc_value_2(ip, any)        -> [0,0,0,0];
enc_value_2(ip, loopback)   -> [127,0,0,1];
enc_value_2(addr, {any,Port}) ->
    [?INET_AF_ANY|?int16(Port)];
enc_value_2(addr, {loopback,Port}) ->
    [?INET_AF_LOOPBACK|?int16(Port)];
enc_value_2(addr, {IP,Port}) ->
    case tuple_size(IP) of
	4 ->
	    [?INET_AF_INET,?int16(Port)|ip4_to_bytes(IP)];
	8 ->
	    [?INET_AF_INET6,?int16(Port)|ip6_to_bytes(IP)]
    end;
enc_value_2(ether, [X1,X2,X3,X4,X5,X6]) -> [X1,X2,X3,X4,X5,X6];
enc_value_2(sctp_assoc_id, Val) -> ?int32(Val);
%% enc_value_2(sctp_assoc_id, Bin) -> [byte_size(Bin),Bin];
enc_value_2({enum,List}, Enum) ->
    {value,Val} = enum_val(Enum, List),
    ?int32(Val);
enc_value_2({bitenumlist,List}, EnumList) ->
    Vs = enum_vals(EnumList, List),
    Val = borlist(Vs, 0),
    ?int32(Val);
enc_value_2({bitenumlist,List,Type}, EnumList) ->
    Vs = enum_vals(EnumList, List),
    Value = borlist(Vs, 0),
    enc_value_2(Type, Value);
enc_value_2(binary,Bin) -> [?int32(byte_size(Bin)),Bin];
enc_value_2(binary_or_uint,Datum) when is_binary(Datum) -> 
    [1,enc_value_2(binary, Datum)];
enc_value_2(binary_or_uint,Datum) when is_integer(Datum) -> 
    [0,enc_value_2(uint, Datum)].



%%
%% Decoding of option VALUES receved from "getopt":
%% NOT required for SCTP, as it always returns ready terms, not lists:
%%
dec_value(bool, [0,0,0,0|T])       -> {false,T};
dec_value(bool, [_,_,_,_|T])       -> {true,T};
%% Currently not used i.e only used by SCTP that does not dec_value/2
%% dec_value(bool8, [0|T])            -> {false,T};
%% dec_value(bool8, [_|T])            -> {true,T};
dec_value(int,  [X3,X2,X1,X0|T])   -> {?i32(X3,X2,X1,X0),T};
dec_value(uint, [X3,X2,X1,X0|T])   -> {?u32(X3,X2,X1,X0),T};
%% Currently not used i.e only used by SCTP that does not dec_value/2
%% dec_value(uint32, [X3,X2,X1,X0|T]) -> {?u32(X3,X2,X1,X0),T};
%% dec_value(uint24, [X2,X1,X0|T])    -> {?u24(X2,X1,X0),T};
%% dec_value(uint16, [X1,X0|T])       -> {?u16(X1,X0),T};
%% dec_value(uint8,  [X0|T])          -> {?u8(X0),T};
dec_value(time, [X3,X2,X1,X0|T]) ->
    case ?i32(X3,X2,X1,X0) of
	-1 -> {infinity, T};
	Val -> {Val, T}
    end;
dec_value(ip, [A,B,C,D|T])             -> {{A,B,C,D}, T};
dec_value(ether,[X1,X2,X3,X4,X5,X6|T]) -> {[X1,X2,X3,X4,X5,X6],T};
dec_value({enum,List}, [X3,X2,X1,X0|T]) ->
    Val = ?i32(X3,X2,X1,X0),
    case enum_name(Val, List) of
	{name, Enum} -> {Enum, T};
	_ -> {undefined, T}
    end;
dec_value({bitenumlist,List}, [X3,X2,X1,X0|T]) ->
    Val = ?i32(X3,X2,X1,X0),
    {enum_names(Val, List), T};
%% Currently not used i.e only used by SCTP that does not dec_value/2
%% dec_value({bitenumlist,List,Type}, T0) ->
%%     {Val,T} = dec_value(Type, T0),
%%     {enum_names(Val, List), T};
dec_value(binary,[L0,L1,L2,L3|List]) ->
    Len = ?i32(L0,L1,L2,L3),
    {X,T}=lists:split(Len,List),
    {list_to_binary(X),T};
dec_value(Types, List) when is_tuple(Types) ->
    {L,T} = dec_value_tuple(Types, List, 1, []),
    {list_to_tuple(L),T};
dec_value(Type, Val) ->
    erlang:error({decode,Type,Val}).
%% dec_value(_, B) ->
%%     {undefined, B}.

dec_value_tuple(Types, List, N, Acc)
  when is_integer(N), N =< tuple_size(Types) ->
    {Term,Tail} = dec_value(element(N, Types), List),
    dec_value_tuple(Types, Tail, N+1, [Term|Acc]);
dec_value_tuple(_, List, _, Acc) ->
    {lists:reverse(Acc),List}.

borlist([V|Vs], Value) ->
    borlist(Vs, V bor Value);
borlist([], Value) -> Value.
    

enum_vals([Enum|Es], List) ->
    case enum_val(Enum, List) of
	false -> false;
	{value,Value} -> [Value | enum_vals(Es, List)]
    end;
enum_vals([], _) -> [].

enum_names(Val, [{Enum,BitVal} |List]) ->
    if Val band BitVal =:= BitVal ->
	    [Enum | enum_names(Val, List)];
       true ->
	    enum_names(Val, List)
    end;
enum_names(_, []) -> [].
    
enum_val(Enum, [{Enum,Value}|_]) -> {value,Value};
enum_val(Enum, [_|List]) -> enum_val(Enum, List);
enum_val(_, []) -> false.

enum_name(Val, [{Enum,Val}|_]) -> {name,Enum};
enum_name(Val, [_|List]) -> enum_name(Val, List);
enum_name(_, []) -> false.



%% Encoding for setopts
%%
%% encode opt/val REVERSED since options are stored in reverse order
%% i.e. the recent options first (we must process old -> new)
encode_opt_val(Opts) -> 
    try 
	enc_opt_val(Opts, [])
    catch
	Reason -> {error,Reason}
    end.

enc_opt_val([{active,once}|Opts], Acc) ->
    %% Specially optimized because {active,once} will be used for
    %% every packet, not only once when initializing the socket.
    %% Measurements show that this optimization is worthwhile.
    enc_opt_val(Opts, [<<?INET_LOPT_ACTIVE:8,?INET_ONCE:32>>|Acc]);
enc_opt_val([{raw,P,O,B}|Opts], Acc) ->
    enc_opt_val(Opts, Acc, raw, {P,O,B});
enc_opt_val([{Opt,Val}|Opts], Acc) ->
    enc_opt_val(Opts, Acc, Opt, Val);
enc_opt_val([binary|Opts], Acc) ->
    enc_opt_val(Opts, Acc, mode, binary);
enc_opt_val([list|Opts], Acc) ->
    enc_opt_val(Opts, Acc, mode, list);
enc_opt_val([_|_], _) -> {error,einval};
enc_opt_val([], Acc)  -> {ok,Acc}.

enc_opt_val(Opts, Acc, Opt, Val) when is_atom(Opt) ->
    Type = type_opt(set, Opt),
    case type_value(set, Type, Val) of
	true -> 
	    enc_opt_val(Opts, [enc_opt(Opt),enc_value(set, Type, Val)|Acc]);
	false -> {error,einval}
    end;
enc_opt_val(_, _, _, _) -> {error,einval}.



%% Encoding for getopts
%%
%% "encode_opts" is for "getopt" only, not setopt". But it uses "enc_opt" which
%% is common for "getopt" and "setopt":
encode_opts(Opts) -> 
    try enc_opts(Opts) of
	Buf -> {ok,Buf}
    catch
	Error -> {error,Error}
    end.

% Raw options are a special case, they need to be rewritten to be properly 
% handled and the types need checking even when querying.
enc_opts([{raw,P,O,S}|Opts]) ->
    enc_opts(Opts, raw, {P,O,S});
enc_opts([{Opt,Val}|Opts]) ->
    enc_opts(Opts, Opt, Val);
enc_opts([Opt|Opts]) ->
    enc_opts(Opts, Opt);
enc_opts([]) -> [].

enc_opts(Opts, Opt) when is_atom(Opt) ->
    Type = type_opt(get, Opt),
    case type_value(get, Type) of
	true -> 
	    [enc_opt(Opt),enc_value(get, Type)|enc_opts(Opts)];
	false ->
	    throw(einval)
    end;
enc_opts(_, _) ->
    throw(einval).

enc_opts(Opts, Opt, Val) when is_atom(Opt) ->
    Type = type_opt(get, Opt),
    case type_value(get, Type, Val) of
	true -> 
	    [enc_opt(Opt),enc_value(get, Type, Val)|enc_opts(Opts)];
	false ->
	    throw(einval)
    end;
enc_opts(_, _, _) ->
    throw(einval).



%% Decoding of raw list data options
%%
decode_opt_val(Buf) ->
    try dec_opt_val(Buf) of
	Result -> {ok,Result}
    catch
	Error  -> {error,Error}
    end.

dec_opt_val([B|Buf]=BBuf) ->
    case dec_opt(B) of
	undefined ->
	    erlang:error({decode,BBuf});
	Opt ->
	    Type = type_opt(dec, Opt),
	    dec_opt_val(Buf, Opt, Type)
    end;
dec_opt_val([]) -> [].

dec_opt_val(Buf, raw, Type) ->
    {{P,O,B},T} = dec_value(Type, Buf),
    [{raw,P,O,B}|dec_opt_val(T)];
dec_opt_val(Buf, Opt, Type) ->
    {Val,T} = dec_value(Type, Buf),
    [{Opt,Val}|dec_opt_val(T)].



%% Pre-processing of options for chgopts
%%
%% Return list of option requests for getopts
%% for all options that containing 'undefined' record fields.
%%
need_template([{Opt,undefined}=OV|Opts]) when is_atom(Opt) ->
    [OV|need_template(Opts)];
need_template([{Opt,Val}|Opts]) when is_atom(Opt) ->
    case need_template(Val, 2) of
	true ->
	    [{Opt,undefined}|need_template(Opts)];
	false ->
	    need_template(Opts)
    end;
need_template([_|Opts]) ->
    need_template(Opts);
need_template([]) -> [].
%%
need_template(T, N) when is_integer(N), N =< tuple_size(T) ->
    case element(N, T) of
	undefined -> true;
	_ ->
	    need_template(T, N+1)
    end;
need_template(_, _) -> false.

%% Replace 'undefined' record fields in option values with values
%% from template records.
%%
merge_options([{Opt,undefined}|Opts], [{Opt,_}=T|Templates]) ->
    [T|merge_options(Opts, Templates)];
merge_options([{Opt,Val}|Opts], [{Opt,Template}|Templates])
  when is_atom(Opt), tuple_size(Val) >= 2 ->
    Key = element(1, Val),
    Size = tuple_size(Val),
    if Size =:= tuple_size(Template), Key =:= element(1, Template) ->
	    %% is_record(Template, Key)
	    [{Opt,list_to_tuple([Key|merge_fields(Val, Template, 2)])}
	     |merge_options(Opts, Templates)];
       true ->
	    throw({merge,Val,Template})
    end;
merge_options([OptVal|Opts], Templates) ->
    [OptVal|merge_options(Opts, Templates)];
merge_options([], []) -> [];
merge_options(Opts, Templates) ->
    throw({merge,Opts,Templates}).

merge_fields(Opt, Template, N) when is_integer(N), N =< tuple_size(Opt) ->
    case element(N, Opt) of
	undefined ->
	    [element(N, Template)|merge_fields(Opt, Template, N+1)];
	Val ->
	    [Val|merge_fields(Opt, Template, N+1)]
    end;
merge_fields(_, _, _) -> [].
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% handle interface options
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type_ifopt(addr)      -> ip;
type_ifopt(broadaddr) -> ip;
type_ifopt(dstaddr)   -> ip;
type_ifopt(mtu)       -> int;
type_ifopt(netmask)   -> ip;
type_ifopt(flags)     ->
    {bitenumlist,
     [{up, ?INET_IFF_UP},
      {down, ?INET_IFF_DOWN},
      {broadcast, ?INET_IFF_BROADCAST},
      {no_broadcast, ?INET_IFF_NBROADCAST},
      {loopback,  ?INET_IFF_LOOPBACK},
      {pointtopoint, ?INET_IFF_POINTTOPOINT},
      {no_pointtopoint, ?INET_IFF_NPOINTTOPOINT},
      {running, ?INET_IFF_RUNNING},
      {multicast, ?INET_IFF_MULTICAST}]};
type_ifopt(hwaddr)    -> ether;
type_ifopt(Opt) when is_atom(Opt) -> undefined.

enc_ifopt(addr)      -> ?INET_IFOPT_ADDR;
enc_ifopt(broadaddr) -> ?INET_IFOPT_BROADADDR;
enc_ifopt(dstaddr)   -> ?INET_IFOPT_DSTADDR;
enc_ifopt(mtu)       -> ?INET_IFOPT_MTU;
enc_ifopt(netmask)   -> ?INET_IFOPT_NETMASK;
enc_ifopt(flags)     -> ?INET_IFOPT_FLAGS;
enc_ifopt(hwaddr)    -> ?INET_IFOPT_HWADDR;
enc_ifopt(Opt) when is_atom(Opt) -> -1.

dec_ifopt(?INET_IFOPT_ADDR)      -> addr;
dec_ifopt(?INET_IFOPT_BROADADDR) -> broadaddr;
dec_ifopt(?INET_IFOPT_DSTADDR)   -> dstaddr;
dec_ifopt(?INET_IFOPT_MTU)       -> mtu;
dec_ifopt(?INET_IFOPT_NETMASK)   -> netmask;
dec_ifopt(?INET_IFOPT_FLAGS)     -> flags;
dec_ifopt(?INET_IFOPT_HWADDR)    -> hwaddr;
dec_ifopt(I) when is_integer(I)  -> undefined.

%% decode if options returns a reversed list
decode_ifopts([B | Buf], Acc) ->
    case dec_ifopt(B) of
	undefined -> 
	    {error, einval};
	Opt ->
	    {Val,T} = dec_value(type_ifopt(Opt), Buf),
	    decode_ifopts(T, [{Opt,Val} | Acc])
    end;
decode_ifopts(_,Acc) -> {ok,Acc}.


%% encode if options return a reverse list
encode_ifopts([Opt|Opts], Acc) ->
    case enc_ifopt(Opt) of
	-1 -> {error,einval};
	B  -> encode_ifopts(Opts,[B|Acc])
    end;
encode_ifopts([],Acc) -> {ok,Acc}.


%% encode if options return a reverse list
encode_ifopt_val([{Opt,Val}|Opts], Buf) ->
    Type = type_ifopt(Opt),
    try type_value(set, Type, Val) of
	true -> 
	    encode_ifopt_val(Opts,
			     [Buf,enc_ifopt(Opt),enc_value(set, Type, Val)]);
	false -> {error,einval}
    catch
	Reason -> {error,Reason}
    end;
encode_ifopt_val([], Buf) -> {ok,Buf}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% handle subscribe options
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_subs(L) ->
    try enc_subs(L) of
	Result -> {ok,Result}
    catch        
	Error  -> {error,Error}
    end.

enc_subs([H|T]) ->
    case H of
	subs_empty_out_q -> [?INET_SUBS_EMPTY_OUT_Q|enc_subs(T)]%;
	%%Dialyzer _ -> throw(einval)
    end;
enc_subs([]) -> [].


decode_subs(Bytes) -> 
    try dec_subs(Bytes) of
	Result -> {ok,Result}
    catch
	Error  -> {error,Error}
    end.

dec_subs([X,X3,X2,X1,X0|R]) ->
    Val = ?u32(X3,X2,X1,X0),
    case X of
	?INET_SUBS_EMPTY_OUT_Q  -> [{subs_empty_out_q,Val}|dec_subs(R)];
	_  -> throw(einval)
    end;
dec_subs([]) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% handle statictics options
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_stats(L) ->
    try enc_stats(L) of
	Result -> {ok,Result}
    catch
	Error  -> {error,Error}
    end.

enc_stats([H|T]) ->
    case H of
	recv_cnt  -> [?INET_STAT_RECV_CNT |enc_stats(T)];
	recv_max  -> [?INET_STAT_RECV_MAX |enc_stats(T)];
	recv_avg  -> [?INET_STAT_RECV_AVG |enc_stats(T)];
	recv_dvi  -> [?INET_STAT_RECV_DVI |enc_stats(T)];
	send_cnt  -> [?INET_STAT_SEND_CNT |enc_stats(T)];
	send_max  -> [?INET_STAT_SEND_MAX |enc_stats(T)];
	send_avg  -> [?INET_STAT_SEND_AVG |enc_stats(T)];
	send_pend -> [?INET_STAT_SEND_PEND|enc_stats(T)];
	send_oct  -> [?INET_STAT_SEND_OCT |enc_stats(T)];
	recv_oct  -> [?INET_STAT_RECV_OCT |enc_stats(T)];
	_ -> throw(einval)
    end;
enc_stats([]) -> [].


decode_stats(Bytes) -> 
    try dec_stats(Bytes) of
	Result -> {ok,Result}
    catch
	Error  -> {error,Error}
    end.


dec_stats([?INET_STAT_SEND_OCT,X7,X6,X5,X4,X3,X2,X1,X0|R]) ->
    Val = ?u64(X7,X6,X5,X4,X3,X2,X1,X0),
    [{send_oct, Val}|dec_stats(R)];
dec_stats([?INET_STAT_RECV_OCT,X7,X6,X5,X4,X3,X2,X1,X0|R]) ->
    Val = ?u64(X7,X6,X5,X4,X3,X2,X1,X0),
    [{recv_oct, Val}|dec_stats(R)];
dec_stats([X,X3,X2,X1,X0|R]) ->
    Val = ?u32(X3,X2,X1,X0),
    case X of
	?INET_STAT_RECV_CNT  -> [{recv_cnt,Val} |dec_stats(R)];
	?INET_STAT_RECV_MAX  -> [{recv_max,Val} |dec_stats(R)];
	?INET_STAT_RECV_AVG  -> [{recv_avg,Val} |dec_stats(R)];
	?INET_STAT_RECV_DVI  -> [{recv_dvi,Val} |dec_stats(R)];
	?INET_STAT_SEND_CNT  -> [{send_cnt,Val} |dec_stats(R)];
	?INET_STAT_SEND_MAX  -> [{send_max,Val} |dec_stats(R)];
	?INET_STAT_SEND_AVG  -> [{send_avg,Val} |dec_stats(R)];
	?INET_STAT_SEND_PEND -> [{send_pend,Val}|dec_stats(R)];
	_  -> throw(einval)
    end;
dec_stats([]) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% handle status options
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dec_status(Flags) ->
    enum_names(Flags,
	       [
		{busy, ?INET_F_BUSY},
		%% {listening, ?INET_F_LST}, NOT USED ANY MORE
		{accepting, ?INET_F_ACC},
		{connecting, ?INET_F_CON},
		{listen, ?INET_F_LISTEN},
		{connected, ?INET_F_ACTIVE},
		{bound, ?INET_F_BOUND},
		{open, ?INET_F_OPEN}
	       ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% UTILS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enc_time(Time) when Time < 0 -> [255,255,255,255];
enc_time(Time) -> ?int32(Time).

encode_ifname(Name) when is_atom(Name) -> encode_ifname(atom_to_list(Name));
encode_ifname(Name) ->
    N = length(Name),
    if N > 255 -> {error, einval};
       true -> {ok,[N | Name]}
    end.
    
build_iflist(Cs) ->
    build_iflist(Cs, [], []).

%% Turn a NULL separated list of chars into a list of strings, removing
%% duplicates.
build_iflist([0|L], Acc, [H|T]) ->
    case rev(Acc) of
	H -> build_iflist(L, [], [H|T]);
	N -> build_iflist(L, [], [N,H|T])
    end;
build_iflist([0|L], Acc, []) ->
    build_iflist(L, [], [rev(Acc)]);
build_iflist([C|L], Acc, List) ->
    build_iflist(L, [C|Acc], List);
build_iflist([], [], List) ->
    rev(List);
build_iflist([], Acc, List) ->
    build_iflist([0], Acc, List).

rev(L) -> rev(L,[]).
rev([C|L],Acc) -> rev(L,[C|Acc]);
rev([],Acc) -> Acc.

ip_to_bytes(IP) when tuple_size(IP) =:= 4 -> ip4_to_bytes(IP);
ip_to_bytes(IP) when tuple_size(IP) =:= 8 -> ip6_to_bytes(IP).

ip4_to_bytes({A,B,C,D}) ->
    [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff].

ip6_to_bytes({A,B,C,D,E,F,G,H}) ->
    [?int16(A), ?int16(B), ?int16(C), ?int16(D),
     ?int16(E), ?int16(F), ?int16(G), ?int16(H)].

get_ip(?INET_AF_INET, Addr)  -> get_ip4(Addr);
get_ip(?INET_AF_INET6, Addr) -> get_ip6(Addr).

get_ip4([A,B,C,D | T]) -> {{A,B,C,D},T}.

get_ip6([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16 | T]) ->
    { { ?u16(X1,X2),?u16(X3,X4),?u16(X5,X6),?u16(X7,X8),
	?u16(X9,X10),?u16(X11,X12),?u16(X13,X14),?u16(X15,X16)}, T}.


%% Control command
ctl_cmd(Port, Cmd, Args) ->
    ?DBG_FORMAT("prim_inet:ctl_cmd(~p, ~p, ~p)~n", [Port,Cmd,Args]),
    Result =
	try erlang:port_control(Port, Cmd, Args) of
	    [?INET_REP_OK|Reply]  -> {ok,Reply};
	    [?INET_REP_SCTP]  -> {error,sctp_reply};
	    [?INET_REP_ERROR|Err] -> {error,list_to_atom(Err)}
	catch
	    error:_               -> {error,einval}
	end,
        ?DBG_FORMAT("prim_inet:ctl_cmd() -> ~p~n", [Result]),
    Result.
