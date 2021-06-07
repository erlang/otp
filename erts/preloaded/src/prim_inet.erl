%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2019. All Rights Reserved.
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
%% The SCTP protocol was added 2006
%% by Leonid Timochouk <l.timochouk@gmail.com>
%% and Serge Aleynikov  <saleyn@gmail.com>
%% at IDT Corp. Adapted by the OTP team at Ericsson AB.
%%
-module(prim_inet).

%% Primitive inet_drv interface

-export([open/3, open/4, fdopen/4, fdopen/5, close/1]).
-export([bind/3, listen/1, listen/2, peeloff/2]).
-export([connect/3, connect/4, async_connect/4]).
-export([accept/1, accept/2, accept/3, async_accept/2]).
-export([shutdown/2]).
-export([send/2, send/3, sendto/4, sendmsg/3, sendfile/4]).
-export([recv/2, recv/3, async_recv/3]).
-export([unrecv/2]).
-export([recvfrom/2, recvfrom/3]).
-export([setopt/3, setopts/2, getopt/2, getopts/2, is_sockopt_val/2]).
-export([chgopt/3, chgopts/2]).
-export([getstat/2, getfd/1, ignorefd/2,
	 getindex/1, getstatus/1, gettype/1,
	 getifaddrs/1, getiflist/1, ifget/3, ifset/3,
	 gethostname/1]).
-export([getservbyname/3, getservbyport/3]).
-export([peername/1, setpeername/2, peernames/1, peernames/2]).
-export([sockname/1, setsockname/2, socknames/1, socknames/2]).
-export([attach/1, detach/1]).

-include("inet_sctp.hrl").
-include("inet_int.hrl").

%%%-define(DEBUG, 1).
-ifdef(DEBUG).
-define(
   DBG_FORMAT(Format, Args),
   begin
       %% io:format((Format), (Args)),
       erlang:display(lists:flatten(io_lib:format((Format), (Args)))),
       ok
   end).
-else.
-define(DBG_FORMAT(Format, Args), ok).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% OPEN(tcp | udp | sctp, inet | inet6, stream | dgram | seqpacket)  ->
%%       {ok, insock()} |
%%       {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open(Protocol, Family, Type) ->
    open(Protocol, Family, Type, [], ?INET_REQ_OPEN, []).

open(Protocol, Family, Type, Opts) ->
    open(Protocol, Family, Type, Opts, ?INET_REQ_OPEN, []).

%% FDOPEN(tcp|udp|sctp, inet|inet6|local, stream|dgram|seqpacket, integer())

fdopen(Protocol, Family, Type, Fd) when is_integer(Fd) ->
    fdopen(Protocol, Family, Type, Fd, true).

fdopen(Protocol, Family, Type, Fd, Bound)
  when is_integer(Fd), is_boolean(Bound) ->
    open(Protocol, Family, Type, [], ?INET_REQ_FDOPEN,
         [?int32(Fd), enc_value_2(bool, Bound)]).

open(Protocol, Family, Type, Opts, Req, Data) ->
    Drv = protocol2drv(Protocol),
    AF = enc_family(Family),
    T = enc_type(Type),
    try erlang:open_port({spawn_driver,Drv}, [binary]) of
	S ->
	    case setopts(S, Opts) of
		ok ->
		    case ctl_cmd(S, Req, [AF,T,Data]) of
			{ok,_} -> {ok,S};
			{error,_}=E1 ->
			    close(S),
			    E1
		    end;
		{error,_}=E2 ->
		    close(S),
		    E2
	    end
    catch
	%% The only (?) way to get here is to try to open
	%% the sctp driver when it does not exist (badarg)
	error:badarg       -> {error, eprotonosupport};
	%% system_limit if out of port slots
	error:system_limit -> {error, system_limit}
    end.

enc_family(inet)  -> ?INET_AF_INET;
enc_family(inet6) -> ?INET_AF_INET6;
enc_family(local) -> ?INET_AF_LOCAL.

enc_type(stream) -> ?INET_TYPE_STREAM;
enc_type(dgram) -> ?INET_TYPE_DGRAM;
enc_type(seqpacket) -> ?INET_TYPE_SEQPACKET.

protocol2drv(tcp)  -> "tcp_inet";
protocol2drv(udp)  -> "udp_inet";
protocol2drv(sctp) -> "sctp_inet".

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
    shutdown_1(S, 0);
shutdown(S, write) when is_port(S) ->
    shutdown_1(S, 1);
shutdown(S, read_write) when is_port(S) ->
    shutdown_1(S, 2).

shutdown_1(S, How) ->
    case ctl_cmd(S, ?TCP_REQ_SHUTDOWN, [How]) of
	{ok, []} -> ok;
	{error,_}=Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CLOSE(insock()) -> ok
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close(S) when is_port(S) ->
    ?DBG_FORMAT("prim_inet:close(~p)~n", [S]),
    case getopt(S, linger) of
    	{ok,{true,0}} -> 
	    close_port(S);
        {ok,{true,T}} ->
            %% Wait for T seconds for pending output to be sent
            %%
            %% Note that this handling of Linger may look ok,
            %% but sweeps some problems under the rug since
            %% there are OS buffers that may have remaining data
            %% after the inet driver has emptied its buffers.
            %% But Linger for nonblocking sockets is broken
            %% anyway on all OS:es, according to hearsay,
            %% and is a contradiction in itself.
            %% We have hereby done our best...
            %%
            case subscribe(S, [subs_empty_out_q]) of
                {ok, [{subs_empty_out_q,0}]} ->
                    close_port(S);
                {ok, [{subs_empty_out_q,N}]} when N > 0 ->
                    %% Wait for pending output to be sent
                    Tref = erlang:start_timer(T * 1000, self(), close_port),
                    close_pend_loop(S, Tref, N);
                _ ->
                    %% Subscribe failed - wait full time
                    Tref = erlang:start_timer(T * 1000, self(), close_port),
                    close_pend_loop(S, Tref, undefined)
            end;
	_ -> % Regard this as {ok,{false,_}}
            case subscribe(S, [subs_empty_out_q]) of
                {ok, [{subs_empty_out_q,N}]} when N > 0 ->
                    %% Wait for pending output to be sent
                    DefaultT = 180000, % Arbitrary system timeout 3 min
                    Tref = erlang:start_timer(DefaultT, self(), close_port),
                    close_pend_loop(S, Tref, N);
                _ ->
                    %% Subscribe failed or empty out q - give up or done
                    close_port(S)
            end
    end.

close_pend_loop(S, Tref, N) ->
    ?DBG_FORMAT("prim_inet:close_pend_loop(~p, _, ~p)~n", [S,N]),
    receive
        {timeout,Tref,_} -> % Linger timeout
            ?DBG_FORMAT("prim_inet:close_pend_loop(~p, _, _) timeout~n", [S]),
	    close_port(S);
	{empty_out_q,S} when N =/= undefined ->
            ?DBG_FORMAT(
               "prim_inet:close_pend_loop(~p, _, _) empty_out_q~n", [S]),
	    close_port(S, Tref)
    after ?INET_CLOSE_TIMEOUT ->
	    case getstat(S, [send_pend]) of
                {ok, [{send_pend,N1}]} ->
                    ?DBG_FORMAT(
                       "prim_inet:close_pend_loop(~p, _, _) send_pend ~p~n",
                       [S,N1]),
                    if
                        N1 =:= 0 ->
                            %% Empty outq - done
                            close_port(S, Tref);
                        N =:= undefined ->
                            %% Within linger time - wait some more
                            close_pend_loop(S, Tref, N);
                        N1 =:= N ->
                            %% Inactivity - give up
                            close_port(S, Tref);
                        true ->
                            %% Still moving - wait some more
                            close_pend_loop(S, Tref, N)
                    end;
                _Stat ->
                    %% Failed getstat - give up
                    ?DBG_FORMAT(
                       "prim_inet:close_pend_loop(~p, _, _) getstat ~p~n",
                       [S,_Stat]),
		    close_port(S, Tref)
            end
    end.


close_port(S, Tref) ->
    ?DBG_FORMAT("prim_inet:close_port(~p, _)~n", [S]),
    case erlang:cancel_timer(Tref) of
        false ->
            receive
                {timeout,Tref,_} ->
                    ok
            end;
        _N ->
            ok
    end,
    close_port(S).
%%
close_port(S) ->
    ?DBG_FORMAT("prim_inet:close_port(~p)~n", [S]),
    _Closed = (catch erlang:port_close(S)),
    receive {'EXIT',S,_} -> ok after 0 -> ok end,
    ?DBG_FORMAT("prim_inet:close_port(~p) ~p~n", [S,_Closed]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% BIND(insock(), IP, Port) -> {ok, integer()} | {error, Reason}
%%
%% bind the insock() to the interface address given by IP and Port
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Multi-homed "bind": sctp_bindx(). The Op is 'add' or 'remove'.
%% If no addrs are specified, it just does nothing.
%% Function returns {ok, S} on success, unlike TCP/UDP "bind":
bind(S, add, Addrs) when is_port(S), is_list(Addrs) ->
    bindx(S, 1, Addrs);
bind(S, remove, Addrs) when is_port(S), is_list(Addrs) ->
    bindx(S, 0, Addrs);
bind(S, Addr, _) when is_port(S), tuple_size(Addr) =:= 2 ->
    case type_value(set, addr, Addr) of
	true ->
	    case ctl_cmd(S,?INET_REQ_BIND,enc_value(set, addr, Addr)) of
		{ok, [P1,P0]} -> {ok, ?u16(P1, P0)};
		{error, _} = Error -> Error
	    end;
	false ->
	    {error, einval}
    end;
bind(S, IP, Port) ->
    bind(S, {IP, Port}, 0).

bindx(S, AddFlag, Addrs) ->
    case getprotocol(S) of
	sctp ->
	    case bindx_check_addrs(Addrs) of
		true ->
		    %% Really multi-homed "bindx". Stringified args:
		    %% [AddFlag, (AddrBytes see enc_value_2(addr,X))+]:
		    Args =
			[?int8(AddFlag)|
			 [enc_value(set, addr, Addr) || Addr <- Addrs]],
		    case ctl_cmd(S, ?SCTP_REQ_BINDX, Args) of
			{ok, _} -> {ok, S};
			{error, _}=Error  -> Error
		    end;
		false ->
		    {error, einval}
	    end;
	_ ->
	    {error, einval}
    end.

bindx_check_addrs([Addr|Addrs]) ->
    type_value(set, addr, Addr) andalso bindx_check_addrs(Addrs);
bindx_check_addrs([]) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CONNECT(insock(), IP, Port [,Timeout]) -> ok | {error, Reason}
%%
%% connect the insock() to the address given by IP and Port
%% if timeout is given:
%%       timeout < 0  -> infinity
%%                 0  -> immediate connect (mostly works for loopback)
%%               > 0  -> wait for timeout ms if not connected then 
%%                       return {error, timeout} 
%%
%% ASYNC_CONNECT(insock(), IP, Port, Timeout) -> {ok, S, Ref} | {error, Reason}
%%
%%  a {inet_async,S,Ref,Status} will be sent on socket condition
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For TCP, UDP or SCTP sockets.
%%

connect(S, IP, Port) ->
    connect(S, IP, Port, infinity).
%%
connect(S, Addr, _, Time) when is_port(S), tuple_size(Addr) =:= 2 ->
    case type_value(set, addr, Addr) of
	true when Time =:= infinity ->
	    connect0(S, Addr, -1);
	true when is_integer(Time) ->
	    connect0(S, Addr, Time);
	false ->
	    {error, einval}
    end;
connect(S, IP, Port, Time) ->
    connect(S, {IP, Port}, 0, Time).

connect0(S, Addr, Time) ->
    case async_connect0(S, Addr, Time) of
	{ok, S, Ref} ->
	    receive
		{inet_async, S, Ref, Status} ->
		    Status
	    end;
	Error -> Error
    end.


async_connect(S, Addr, _, Time) when is_port(S), tuple_size(Addr) =:= 2 ->
    case type_value(set, addr, Addr) of
	true when Time =:= infinity ->
	    async_connect0(S, Addr, -1);
	true when is_integer(Time) ->
	    async_connect0(S, Addr, Time);
	false ->
	    {error, einval}
    end;
%%
async_connect(S, IP, Port, Time) ->
    async_connect(S, {IP, Port}, 0, Time).

async_connect0(S, Addr, Time) ->
    case ctl_cmd(
	   S, ?INET_REQ_CONNECT,
	   [enc_time(Time),enc_value(set, addr, Addr)])
    of
	{ok, [R1,R0]} -> {ok, S, ?u16(R1,R0)};
	{error, _}=Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ACCEPT(insock() [,Timeout][,FamilyOpts] ) -> {ok,insock()} | {error, Reason}
%%
%% accept incoming connection on listen socket 
%% if timeout is given:
%%       timeout < 0  -> infinity
%%                 0  -> immediate accept (poll)
%%               > 0  -> wait for timeout ms for accept if no accept then 
%%                       return {error, timeout}
%% FamilyOpts are address family specific options to copy from
%% listen socket to accepted socket
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
accept(L) -> accept0(L, -1, []).

accept(L, infinity) -> accept0(L, -1, []);
accept(L, FamilyOpts) when is_list(FamilyOpts) -> accept0(L, -1, FamilyOpts);
accept(L, Time) -> accept0(L, Time, []).

accept(L, infinity, FamilyOpts) -> accept0(L, -1, FamilyOpts);
accept(L, Time, FamilyOpts) -> accept0(L, Time, FamilyOpts).

accept0(L, Time, FamilyOpts)
  when is_port(L), is_integer(Time), is_list(FamilyOpts) ->
    case async_accept(L, Time) of
	{ok, Ref} ->
	    receive 
		{inet_async, L, Ref, {ok,S}} ->
		    accept_opts(L, S, FamilyOpts);
		{inet_async, L, Ref, Error} ->
		    Error
	    end;
	Error -> Error
    end.

%% setup options from listen socket on the connected socket
accept_opts(L, S, FamilyOpts) ->
    case
        getopts(
          L,
          [active, nodelay, keepalive, delay_send, priority, linger]
          ++ FamilyOpts)
    of
	{ok, Opts} ->
            case setopts(S, Opts) of
                ok ->
                    {ok, S};
                Error1 ->
                    close(S), Error1
            end;
	Error2 ->
	    close(S), Error2
    end.

async_accept(L, Time) ->
    case ctl_cmd(L,?INET_REQ_ACCEPT, [enc_time(Time)]) of
	{ok, [R1,R0]} -> {ok, ?u16(R1,R0)};
	{error,_}=Error -> Error
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

listen(S, true) -> listen(S, ?LISTEN_BACKLOG);
listen(S, false) -> listen(S, 0);
listen(S, BackLog) when is_port(S), is_integer(BackLog) ->
    case ctl_cmd(S, ?INET_REQ_LISTEN, [?int16(BackLog)]) of
	{ok, _} -> ok;
	{error,_}=Error   -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% PEELOFF(insock(), AssocId) -> {ok,outsock()} | {error, Reason}
%%
%% SCTP: Peel off one association into a type stream socket
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

peeloff(S, AssocId) ->
    case ctl_cmd(S, ?SCTP_REQ_PEELOFF, [?int32(AssocId)]) of
	inet_reply ->
	    receive
		{inet_reply,S,Res} -> Res
	    end;
	{error,_}=Error -> Error
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
    ?DBG_FORMAT("prim_inet:send(~p, _, ~p)~n", [S,OptList]),
    try erlang:port_command(S, Data, OptList) of
	false -> % Port busy and nosuspend option passed
	    ?DBG_FORMAT("prim_inet:send() -> {error,busy}~n", []),
	    {error,busy};
	true ->
            send_recv_reply(S, undefined)
    catch
	error:_Error ->
	    ?DBG_FORMAT("prim_inet:send() -> {error,einval}~n", []),
	     {error,einval}
    end.

send_recv_reply(S, Mref) ->
    ReplyTimeout =
        case Mref of
            undefined ->
                ?INET_CLOSE_TIMEOUT;
            _ ->
                infinity
        end,
    receive
        {inet_reply,S,Status} ->
            ?DBG_FORMAT(
               "prim_inet:send_recv_reply(~p, _): inet_reply ~p~n",
               [S,Status]),
            case Mref of
                undefined -> ok;
                _ ->
                    demonitor(Mref, [flush]),
                    ok
            end,
            Status;
        {'DOWN',Mref,_,_,_Reason} when Mref =/= undefined ->
            ?DBG_FORMAT(
               "prim_inet:send_recv_reply(~p, _) 'DOWN' ~p~n",
               [S,_Reason]),
            {error,closed}
    after ReplyTimeout ->
            send_recv_reply(S, monitor(port, S))
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

sendto(S, {_, _} = Address, AncOpts, Data)
  when is_port(S), is_list(AncOpts) ->
    case encode_opt_val(AncOpts) of
        {ok, AncData} ->
            AncDataLen = iolist_size(AncData),
            case
                type_value(set, addr, Address) andalso
                type_value(set, uint32, AncDataLen)
            of
                true ->
                    ?DBG_FORMAT("prim_inet:sendto(~p, ~p, ~p, ~p)~n",
                                [S,Address,AncOpts,Data]),
                    PortCommandData =
                        [enc_value(set, addr, Address),
                         enc_value(set, uint32, AncDataLen), AncData,
                         Data],
                    try erlang:port_command(S, PortCommandData) of
                        true ->
                            receive
                                {inet_reply,S,Reply} ->
                                    ?DBG_FORMAT(
                                       "prim_inet:sendto() -> ~p~n", [Reply]),
                                    Reply
                            end
                    catch
                        _:_ ->
                            ?DBG_FORMAT(
                               "prim_inet:sendto() -> {error,einval}~n", []),
                            {error,einval}
                    end;
                false ->
                    ?DBG_FORMAT(
                       "prim_inet:sendto() -> {error,einval}~n", []),
                    {error,einval}
            end;
        {error,_} ->
            ?DBG_FORMAT(
               "prim_inet:sendto() -> {error,einval}~n", []),
            {error,einval}
    end;                        
sendto(S, IP, Port, Data)
  when is_port(S), is_integer(Port) ->
    sendto(S, {IP, Port}, [], Data).

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
%% SENDFILE(outsock(), Fd, Offset, Length) -> {ok,BytesSent} | {error, Reason}
%%
%% send Length data bytes from a file handle, to a socket, starting at Offset
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% "sendfile" is for TCP:

sendfile(S, FileHandle, Offset, Length)
        when not is_port(S);
             not is_binary(FileHandle);
             not is_integer(Offset);
             not is_integer(Length) ->
    {error, badarg};
sendfile(S, FileHandle, Offset, Length) ->
    case erlang:port_info(S, connected) of
        {connected, Pid} when Pid =:= self() ->
            Uncork = sendfile_maybe_cork(S),
            Result = sendfile_1(S, FileHandle, Offset, Length),
            sendfile_maybe_uncork(S, Uncork),
            Result;
        {connected, Pid} when Pid =/= self() ->
            {error, not_owner};
        _Other ->
            {error, einval}
    end.

sendfile_maybe_cork(S) ->
    case getprotocol(S) of
        tcp ->
            case getopts(S, [nopush]) of
                {ok, [{nopush,false}]} ->
                    _ = setopts(S, [{nopush,true}]),
                    true;
                _ ->
                    false
            end;
        _ -> false
    end.

sendfile_maybe_uncork(S, true) ->
    _ = setopts(S, [{nopush,false}]),
    ok;
sendfile_maybe_uncork(_, false) ->
    ok.

sendfile_1(S, FileHandle, Offset, 0) ->
    sendfile_1(S, FileHandle, Offset, (1 bsl 63) - 1);
sendfile_1(_S, _FileHandle, Offset, Length) when
         Offset < 0; Offset > ((1 bsl 63) - 1);
         Length < 0; Length > ((1 bsl 63) - 1) ->
    {error, einval};
sendfile_1(S, FileHandle, Offset, Length) ->
    Args = [FileHandle,
            ?int64(Offset),
            ?int64(Length)],
    case ctl_cmd(S, ?TCP_REQ_SENDFILE, Args) of
        {ok, []} ->
            receive
                {sendfile, S, {ok, SentLow, SentHigh}} ->
                    {ok, SentLow bor (SentHigh bsl 32)};
                {sendfile, S, {error, Reason}} ->
                    {error, Reason};
                {'EXIT', S, _Reason} ->
                    {error, closed}
            end;
        {error, Reason} ->
            {error, Reason}
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
	{error,_}=Error -> Error
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
    recvfrom(S, Length, infinity).

recvfrom(S, Length, infinity) when is_port(S) ->
    recvfrom0(S, Length, -1);
recvfrom(S, Length, Time) when is_port(S) ->
    if
	is_integer(Time), 0 =< Time, Time < 16#ffffffff ->
	    recvfrom0(S, Length, Time);
	true ->
	    {error, einval}
    end.

recvfrom0(S, Length, Time)
  when is_integer(Length), 0 =< Length, Length =< 16#ffffffff ->
    case ctl_cmd(S, ?PACKET_REQ_RECV,[enc_time(Time),?int32(Length)]) of
	{ok,[R1,R0]} ->
	    Ref = ?u16(R1,R0),
	    receive
		% Success, UDP:
		{inet_async, S, Ref, {ok, {[F | AddrData], AncData}}} ->
                    %% With ancillary data
		    case get_addr(F, AddrData) of
			{{Family, _} = Addr, Data} when is_atom(Family) ->
			    {ok, {Addr, 0, AncData, Data}};
			{{IP, Port}, Data} ->
			    {ok, {IP, Port, AncData, Data}}
		    end;
		{inet_async, S, Ref, {ok, [F | AddrData]}} ->
                    %% Without ancillary data
		    case get_addr(F, AddrData) of
			{{Family, _} = Addr, Data} when is_atom(Family) ->
			    {ok, {Addr, 0, Data}};
			{{IP, Port}, Data} ->
			    {ok, {IP, Port, Data}}
		    end;

		% Success, SCTP:
		{inet_async, S, Ref, {ok, {[F,P1,P0 | Addr], AncData, DE}}} ->
		    {IP, _} = get_ip(F, Addr),
		    {ok, {IP, ?u16(P1, P0), AncData, DE}};

		% Back-end error:
		{inet_async, S, Ref, Error={error, _}} ->
		    Error
	    end;
	{error,_}=Error ->
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
	{ok, [F | Addr]} ->
	    {A, _} = get_addr(F, Addr),
	    {ok, A};
	{error, _} = Error -> Error
    end.

setpeername(S, undefined) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_SETPEER, []) of
	{ok, []} -> ok;
	{error, _} = Error -> Error
    end;
setpeername(S, Addr) when is_port(S) ->
    case type_value(set, addr, Addr) of
	true ->
	    case ctl_cmd(S, ?INET_REQ_SETPEER, enc_value(set, addr, Addr)) of
		{ok, []} -> ok;
		{error, _} = Error -> Error
	    end;
	false ->
	    {error, einval}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% PEERNAMES(insock()) -> {ok, [{IP, Port}, ...]} | {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

peernames(S) when is_port(S) ->
    peernames(S, undefined).

peernames(S, #sctp_assoc_change{assoc_id=AssocId}) when is_port(S) ->
    peernames(S, AssocId);
peernames(S, AssocId)
  when is_port(S), is_integer(AssocId);
       is_port(S), AssocId =:= undefined ->
    Q = get,
    Type = [[sctp_assoc_id,0]],
    case type_value(Q, Type, AssocId) of
	true ->
	    case ctl_cmd
		(S, ?INET_REQ_GETPADDRS,
		 enc_value(Q, Type, AssocId)) of
		{ok,Addrs} ->
		    {ok,get_addrs(Addrs)};
		Error ->
		    Error
	    end;
	false ->
	    {error,einval}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SOCKNAME(insock()) -> {ok, {IP, Port}} | {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sockname(S) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_NAME, []) of
	{ok, [F | Addr]} ->
	    {A, _} = get_addr(F, Addr),
	    {ok, A};
	{error, _} = Error -> Error
    end.

setsockname(S, undefined) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_SETNAME, []) of
	{ok, []} -> ok;
	{error, _} = Error -> Error
    end;
setsockname(S, Addr) when is_port(S) ->
    case type_value(set, addr, Addr) of
	true ->
	    case
		ctl_cmd(S, ?INET_REQ_SETNAME, enc_value(set, addr, Addr))
	    of
		{ok, []} -> ok;
		{error, _} = Error -> Error
	    end;
	false ->
	    {error, einval}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SOCKNAMES(insock()) -> {ok, [{IP, Port}, ...]} | {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

socknames(S) when is_port(S) ->
    socknames(S, undefined).

socknames(S, #sctp_assoc_change{assoc_id=AssocId}) when is_port(S) ->
    socknames(S, AssocId);
socknames(S, AssocId)
  when is_port(S), is_integer(AssocId);
       is_port(S), AssocId =:= undefined ->
    Q = get,
    Type = [[sctp_assoc_id,0]],
    case type_value(Q, Type, AssocId) of
	true ->
	    case ctl_cmd
		(S, ?INET_REQ_GETLADDRS,
		 enc_value(Q, Type, AssocId)) of
		{ok,Addrs} ->
		    {ok,get_addrs(Addrs)};
		Error ->
		    Error
	    end;
	false ->
	    {error,einval}
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
		{error,_}=Error -> Error
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
		inet_reply ->
		    %% SCTP: Need to receive the full value:
		    receive
			{inet_reply,S,Res} -> Res
		    end;
		{error,_}=Error -> Error
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
    case getopts(S, need_template(Opts)) of
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
%% getifaddrs(insock()) -> {ok,IfAddrsList} | {error, Reason}
%%
%%   IfAddrsList = [{Name,[Opts]}]
%%   Name = string()
%%   Opts = {flags,[Flag]} | {addr,Addr} | {netmask,Addr} | {broadaddr,Addr}
%%        | {dstaddr,Addr} | {hwaddr,HwAddr} | {mtu,integer()}
%%   Flag = up | broadcast | loopback | running | multicast
%%   Addr = ipv4addr() | ipv6addr()
%%   HwAddr = ethernet_addr()
%%
%% get interface name and addresses list
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getifaddrs(S) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_GETIFADDRS, []) of
        {ok, Data} ->
            {ok, comp_ifaddrs(build_ifaddrs(Data))};
        {error,enotsup} ->
	    case getiflist(S) of
		{ok, IFs} ->
		    {ok, getifaddrs_ifget(S, IFs)};
		Err1 -> Err1
	    end;
	Err2 -> Err2
    end.

%% Restructure interface properties per interface

comp_ifaddrs(IfOpts) ->
    comp_ifaddrs(IfOpts, ktree_empty()).
%%
comp_ifaddrs([{If,[{flags,Flags}|Opts]}|IfOpts], IfT) ->
    case ktree_is_defined(If, IfT) of
        true ->
            comp_ifaddrs(
              IfOpts,
              ktree_update(
                If,
                comp_ifaddrs_flags(Flags, Opts, ktree_get(If, IfT)),
                IfT));
        false ->
            comp_ifaddrs(
              IfOpts,
              ktree_insert(
                If,
                comp_ifaddrs_flags(Flags, Opts, ktree_empty()),
                IfT))
    end;
comp_ifaddrs([], IfT) ->
    comp_ifaddrs_2(ktree_keys(IfT), IfT).

comp_ifaddrs_flags(Flags, Opts, FlagsT) ->
    case ktree_is_defined(Flags, FlagsT) of
        true ->
            ktree_update(
              Flags,
              rev(Opts, ktree_get(Flags, FlagsT)),
              FlagsT);
        false ->
            ktree_insert(Flags, rev(Opts), FlagsT)
    end.

comp_ifaddrs_2([If|Ifs], IfT) ->
    FlagsT = ktree_get(If, IfT),
    [{If,comp_ifaddrs_3(ktree_keys(FlagsT), FlagsT)}
     | comp_ifaddrs_2(Ifs, IfT)];
comp_ifaddrs_2([], _IfT) ->
    [].
%%
comp_ifaddrs_3([Flags|FlagsL], FlagsT) ->
    [{flags,Flags}|hwaddr_last(rev(ktree_get(Flags, FlagsT)))]
        ++ hwaddr_last(comp_ifaddrs_3(FlagsL, FlagsT));
comp_ifaddrs_3([], _FlagsT) ->
    [].

%% Place hwaddr last to look more like legacy emulation
hwaddr_last(Opts) ->
    hwaddr_last(Opts, Opts, []).
%%
hwaddr_last([{hwaddr,_} = Opt|Opts], L, R) ->
    hwaddr_last(Opts, L, [Opt|R]);
hwaddr_last([_|Opts], L, R) ->
    hwaddr_last(Opts, L, R);
hwaddr_last([], L, []) ->
    L;
hwaddr_last([], L, R) ->
    rev(hwaddr_last(L, []), rev(R)).
%%
hwaddr_last([{hwaddr,_}|Opts], R) ->
    hwaddr_last(Opts, R);
hwaddr_last([Opt|Opts], R) ->
    hwaddr_last(Opts, [Opt|R]);
hwaddr_last([], R) ->
    R.


%% Legacy emulation of getifaddrs

getifaddrs_ifget(_, []) -> [];
getifaddrs_ifget(S, [IF|IFs]) ->
    case ifget(S, IF, [flags]) of
	{ok,[{flags,Flags}]=FlagsVals} ->
            GetOpts =
                case member(pointtopoint, Flags) of
                    true ->
                        [dstaddr,hwaddr];
                    false ->
                        case member(broadcast, Flags) of
                            true ->
                                [broadaddr,hwaddr];
                            false ->
                                [hwaddr]
                        end
                end,
	    getifaddrs_ifget(S, IFs, IF, FlagsVals, [addr,netmask|GetOpts]);
	_ ->
	    getifaddrs_ifget(S, IFs, IF, [], [addr,netmask,hwaddr])
    end.

getifaddrs_ifget(S, IFs, IF, FlagsVals, Opts) ->
    OptVals =
	case ifget(S, IF, Opts) of
	    {ok,OVs} -> OVs;
	    _ -> []
	end,
    [{IF,FlagsVals++OptVals}|getifaddrs_ifget(S, IFs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% getiflist(insock()) -> {ok,IfNameList} | {error, Reason}
%%
%% get interface name list
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getiflist(S) when is_port(S) ->
    case ctl_cmd(S, ?INET_REQ_GETIFLIST, []) of
	{ok, Data} -> {ok, build_iflist(Data)};
	{error,_}=Error -> Error
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
			{error,_}=Error -> Error
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
			{error,_}=Error -> Error
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
		{error,_}=Error -> Error
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
		{error,_}=Error -> Error
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
	{error,_}=Error -> Error
    end.        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% IGNOREFD(insock(),boolean()) -> {ok,integer()} | {error, Reason}
%%
%% steal internal file descriptor
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ignorefd(S,Bool) when is_port(S) ->
    Val = if Bool -> 1; true -> 0 end,
    case ctl_cmd(S, ?INET_REQ_IGNOREFD, [Val]) of
	{ok, _} -> ok;
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
	{error,_}=Error -> Error
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
	{error,_}=Error -> Error
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
		{error,_}=Error ->
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
		{error,_}=Error -> Error
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
	{error,_}=Error  -> Error
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
enc_opt(tclass)          -> ?INET_OPT_TCLASS;
enc_opt(recvtos)         -> ?INET_OPT_RECVTOS;
enc_opt(recvtclass)      -> ?INET_OPT_RECVTCLASS;
enc_opt(pktoptions)      -> ?INET_OPT_PKTOPTIONS;
enc_opt(ttl)             -> ?INET_OPT_TTL;
enc_opt(recvttl)         -> ?INET_OPT_RECVTTL;
enc_opt(nodelay)         -> ?TCP_OPT_NODELAY;
enc_opt(nopush)          -> ?TCP_OPT_NOPUSH;
enc_opt(multicast_if)    -> ?UDP_OPT_MULTICAST_IF;
enc_opt(multicast_ttl)   -> ?UDP_OPT_MULTICAST_TTL;
enc_opt(multicast_loop)  -> ?UDP_OPT_MULTICAST_LOOP;
enc_opt(add_membership)  -> ?UDP_OPT_ADD_MEMBERSHIP;
enc_opt(drop_membership) -> ?UDP_OPT_DROP_MEMBERSHIP;
enc_opt(ipv6_v6only)     -> ?INET_OPT_IPV6_V6ONLY;
enc_opt(buffer)          -> ?INET_LOPT_BUFFER;
enc_opt(header)          -> ?INET_LOPT_HEADER;
enc_opt(active)          -> ?INET_LOPT_ACTIVE;
enc_opt(packet)          -> ?INET_LOPT_PACKET;
enc_opt(mode)            -> ?INET_LOPT_MODE;
enc_opt(deliver)         -> ?INET_LOPT_DELIVER;
enc_opt(exit_on_close)   -> ?INET_LOPT_EXITONCLOSE;
enc_opt(high_watermark)  -> ?INET_LOPT_TCP_HIWTRMRK;
enc_opt(low_watermark)   -> ?INET_LOPT_TCP_LOWTRMRK;
enc_opt(high_msgq_watermark)  -> ?INET_LOPT_MSGQ_HIWTRMRK;
enc_opt(low_msgq_watermark)   -> ?INET_LOPT_MSGQ_LOWTRMRK;
enc_opt(send_timeout)    -> ?INET_LOPT_TCP_SEND_TIMEOUT;
enc_opt(send_timeout_close) -> ?INET_LOPT_TCP_SEND_TIMEOUT_CLOSE;
enc_opt(delay_send)      -> ?INET_LOPT_TCP_DELAY_SEND;
enc_opt(packet_size)     -> ?INET_LOPT_PACKET_SIZE;
enc_opt(read_packets)    -> ?INET_LOPT_READ_PACKETS;
enc_opt(netns)           -> ?INET_LOPT_NETNS;
enc_opt(show_econnreset) -> ?INET_LOPT_TCP_SHOW_ECONNRESET;
enc_opt(line_delimiter)  -> ?INET_LOPT_LINE_DELIM;
enc_opt(raw)             -> ?INET_OPT_RAW;
enc_opt(bind_to_device)  -> ?INET_OPT_BIND_TO_DEVICE;
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
dec_opt(?INET_OPT_TCLASS)         -> tclass;
dec_opt(?TCP_OPT_NODELAY)         -> nodelay;
dec_opt(?TCP_OPT_NOPUSH)          -> nopush;
dec_opt(?INET_OPT_RECVTOS)        -> recvtos;
dec_opt(?INET_OPT_RECVTCLASS)     -> recvtclass;
dec_opt(?INET_OPT_PKTOPTIONS)     -> pktoptions;
dec_opt(?INET_OPT_TTL)            -> ttl;
dec_opt(?INET_OPT_RECVTTL)        -> recvttl;
dec_opt(?UDP_OPT_MULTICAST_IF)    -> multicast_if;
dec_opt(?UDP_OPT_MULTICAST_TTL)   -> multicast_ttl;
dec_opt(?UDP_OPT_MULTICAST_LOOP)  -> multicast_loop;
dec_opt(?UDP_OPT_ADD_MEMBERSHIP)  -> add_membership;
dec_opt(?UDP_OPT_DROP_MEMBERSHIP) -> drop_membership;
dec_opt(?INET_OPT_IPV6_V6ONLY)    -> ipv6_v6only;
dec_opt(?INET_LOPT_BUFFER)        -> buffer;
dec_opt(?INET_LOPT_HEADER)        -> header;
dec_opt(?INET_LOPT_ACTIVE)        -> active;
dec_opt(?INET_LOPT_PACKET)        -> packet;
dec_opt(?INET_LOPT_MODE)          -> mode;
dec_opt(?INET_LOPT_DELIVER)       -> deliver;
dec_opt(?INET_LOPT_EXITONCLOSE)   -> exit_on_close;
dec_opt(?INET_LOPT_TCP_HIWTRMRK)  -> high_watermark;
dec_opt(?INET_LOPT_TCP_LOWTRMRK)  -> low_watermark;
dec_opt(?INET_LOPT_MSGQ_HIWTRMRK)  -> high_msgq_watermark;
dec_opt(?INET_LOPT_MSGQ_LOWTRMRK)  -> low_msgq_watermark;
dec_opt(?INET_LOPT_TCP_SEND_TIMEOUT) -> send_timeout;
dec_opt(?INET_LOPT_TCP_SEND_TIMEOUT_CLOSE) -> send_timeout_close;
dec_opt(?INET_LOPT_TCP_DELAY_SEND)   -> delay_send;
dec_opt(?INET_LOPT_PACKET_SIZE)      -> packet_size;
dec_opt(?INET_LOPT_READ_PACKETS)     -> read_packets;
dec_opt(?INET_LOPT_NETNS)           -> netns;
dec_opt(?INET_LOPT_TCP_SHOW_ECONNRESET) -> show_econnreset;
dec_opt(?INET_LOPT_LINE_DELIM)      -> line_delimiter;
dec_opt(?INET_OPT_RAW)              -> raw;
dec_opt(?INET_OPT_BIND_TO_DEVICE) -> bind_to_device;
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
type_opt_1(tclass)          -> int;
type_opt_1(recvtos)         -> bool;
type_opt_1(recvtclass)      -> bool;
type_opt_1(pktoptions)      -> opts;
type_opt_1(ttl)             -> int;
type_opt_1(recvttl)         -> bool;
type_opt_1(nodelay)         -> bool;
type_opt_1(nopush)          -> bool;
type_opt_1(ipv6_v6only)     -> bool;
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
	   {once, ?INET_ONCE},
           {multi, ?INET_MULTI}]};
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
type_opt_1(line_delimiter)  -> int;
type_opt_1(mode) ->
    {enum,[{list, ?INET_MODE_LIST},
	   {binary, ?INET_MODE_BINARY}]};
type_opt_1(deliver) ->
    {enum,[{port, ?INET_DELIVER_PORT},
	   {term, ?INET_DELIVER_TERM}]};
type_opt_1(exit_on_close)   -> bool;
type_opt_1(low_watermark)   -> int;
type_opt_1(high_watermark)  -> int;
type_opt_1(low_msgq_watermark)   -> int;
type_opt_1(high_msgq_watermark)  -> int;
type_opt_1(send_timeout)    -> time;
type_opt_1(send_timeout_close) -> bool;
type_opt_1(delay_send)      -> bool;
type_opt_1(packet_size)     -> uint;
type_opt_1(read_packets)    -> uint;
type_opt_1(netns)           -> binary;
type_opt_1(show_econnreset) -> bool;
type_opt_1(bind_to_device)  -> binary;
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
	assoc_id          = [[sctp_assoc_id,0]]}}];
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
%% These two clauses cannot happen since they are only used
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
%%
type_value_2(addr, {any,Port}) ->
    type_value_2(uint16, Port);
type_value_2(addr, {loopback,Port}) ->
    type_value_2(uint16, Port);
type_value_2(addr, {IP,_} = Addr) when tuple_size(IP) =:= 4 ->
    type_value_2(addr, {inet,Addr});
type_value_2(addr, {IP,_} = Addr) when tuple_size(IP) =:= 8 ->
    type_value_2(addr, {inet6,Addr});
type_value_2(addr, {Local,_}) when is_list(Local); is_binary(Local) ->
    type_value_2(addr, {local,Local});
%%
type_value_2(addr, {Family,{Tag,Port}})
  when (Family =:= inet orelse Family =:= inet6) andalso
       (Tag =:= any orelse Tag =:= loopback) ->
    type_value_2(uint16, Port);
type_value_2(addr, {inet,{{A,B,C,D},Port}})
  when ?ip(A,B,C,D) ->
    type_value_2(uint16, Port);
type_value_2(addr, {inet6,{{A,B,C,D,E,F,G,H},Port}})
  when ?ip6(A,B,C,D,E,F,G,H) ->
    type_value_2(uint16, Port);
type_value_2(addr, {local,Addr}) ->
    if
	is_binary(Addr) ->
	    byte_size(Addr) =< 255;
	true ->
	    try
		%% We either get a badarg from byte_size
		%% or from characters_to_binary
		byte_size(
		  unicode:characters_to_binary(
		    Addr, file:native_name_encoding()))
	    of
		N when N =< 255 ->
		    true;
		_ ->
		    false
	    catch error:badarg ->
		    false
	    end
    end;
%%
type_value_2(ether,[X1,X2,X3,X4,X5,X6])
  when ?ether(X1,X2,X3,X4,X5,X6)                    -> true;
type_value_2({enum,List}, Enum) -> 
    case enum_val(Enum, List) of
	{value,_} 				    -> true;
	false                                       -> false
    end;
type_value_2(sockaddr, Addr) ->
    case Addr of
	any                                         -> true;
	loopback                                    -> true;
	{A,B,C,D} when ?ip(A,B,C,D)                 -> true;
	{A,B,C,D,E,F,G,H} when ?ip6(A,B,C,D,E,F,G,H) -> true;
	_                                           -> false
    end;
type_value_2(linkaddr, Addr) when is_list(Addr) ->
    case len(Addr, 32768) of
	undefined                                   -> false;
	_                                           -> true
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
type_value_2(binary,Bin)
  when is_binary(Bin), byte_size(Bin) < (1 bsl 32)  -> true;
type_value_2(binary_or_uint,Bin)
  when is_binary(Bin), byte_size(Bin) < (1 bsl 32)  -> true;
type_value_2(binary_or_uint,Int)
  when is_integer(Int), Int >= 0                    -> true;
%% Type-checking of SCTP options
type_value_2(sctp_assoc_id, X)
  when X band 16#ffffffff =:= X                     -> true;
type_value_2(_, _)         -> false.



%% Get. No supplied value.
%%
%% These two clauses cannot happen since they are only used
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
%%
enc_value_2(addr, {any,Port}) ->
    [?INET_AF_ANY|?int16(Port)];
enc_value_2(addr, {loopback,Port}) ->
    [?INET_AF_LOOPBACK|?int16(Port)];
enc_value_2(addr, {IP,Port}) when tuple_size(IP) =:= 4 ->
    [?INET_AF_INET,?int16(Port)|ip4_to_bytes(IP)];
enc_value_2(addr, {IP,Port}) when tuple_size(IP) =:= 8 ->
    [?INET_AF_INET6,?int16(Port)|ip6_to_bytes(IP)];
enc_value_2(addr, {File,_}) when is_list(File); is_binary(File) ->
    [?INET_AF_LOCAL,iolist_size(File)|File];
%%
enc_value_2(addr, {inet,{any,Port}}) ->
    [?INET_AF_INET,?int16(Port)|ip4_to_bytes({0,0,0,0})];
enc_value_2(addr, {inet,{loopback,Port}}) ->
    [?INET_AF_INET,?int16(Port)|ip4_to_bytes({127,0,0,1})];
enc_value_2(addr, {inet,{IP,Port}}) ->
    [?INET_AF_INET,?int16(Port)|ip4_to_bytes(IP)];
enc_value_2(addr, {inet6,{any,Port}}) ->
    [?INET_AF_INET6,?int16(Port)|ip6_to_bytes({0,0,0,0,0,0,0,0})];
enc_value_2(addr, {inet6,{loopback,Port}}) ->
    [?INET_AF_INET6,?int16(Port)|ip6_to_bytes({0,0,0,0,0,0,0,1})];
enc_value_2(addr, {inet6,{IP,Port}}) ->
    [?INET_AF_INET6,?int16(Port)|ip6_to_bytes(IP)];
enc_value_2(addr, {local,Addr}) ->
    %% A binary is passed as is, but anything else will be
    %% regarded as a filename and therefore encoded according to
    %% the current system filename encoding mode.
    Bin =
	if
	    is_binary(Addr) ->
		Addr;
	    true ->
		unicode:characters_to_binary(
		  Addr, file:native_name_encoding())
	end,
    [?INET_AF_LOCAL,byte_size(Bin),Bin];
%%
enc_value_2(ether, [_,_,_,_,_,_]=Xs) -> Xs;
enc_value_2(sockaddr, any) ->
    [?INET_AF_ANY];
enc_value_2(sockaddr, loopback) ->
    [?INET_AF_LOOPBACK];
enc_value_2(sockaddr, IP) when tuple_size(IP) =:= 4 ->
    [?INET_AF_INET|ip4_to_bytes(IP)];
enc_value_2(sockaddr, IP) when tuple_size(IP) =:= 8 ->
    [?INET_AF_INET6|ip6_to_bytes(IP)];
enc_value_2(linkaddr, Linkaddr) ->
    [?int16(length(Linkaddr)),Linkaddr];
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
%% dec_value(ether, [X1,X2,X3,X4,X5,X6|T]) -> {[X1,X2,X3,X4,X5,X6],T};
dec_value(sockaddr, [X|T]) ->
    get_ip(X, T);
dec_value(linkaddr, [X1,X0|T]) ->
    split(?i16(X1,X0), T);
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
    {X,T}=split(Len,List),
    {list_to_binary(X),T};
dec_value(opts, [L0,L1,L2,L3|List]) ->
    Len = ?u32(L0,L1,L2,L3),
    {X,T} = split(Len, List),
    Opts = dec_opt_val(X),
    {Opts,T};
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
    {rev(Acc),List}.

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
	{ok, enc_opt_val(Opts, [])}
    catch
	throw:Reason -> {error,Reason}
    end.

%% {active, once} and {active, N} are specially optimized because they will
%% be used for every packet or every N packets, not only once when
%% initializing the socket.  Measurements show that this optimization is
%% worthwhile.
enc_opt_val([{active,once}|Opts], Acc) ->
    enc_opt_val(Opts, [<<?INET_LOPT_ACTIVE:8,?INET_ONCE:32>>|Acc]);
enc_opt_val([{active,N}|Opts], Acc) when is_integer(N), N < 32768, N >= -32768 ->
    enc_opt_val(Opts, [<<?INET_LOPT_ACTIVE:8,?INET_MULTI:32,N:16>>|Acc]);
enc_opt_val([{raw,P,O,B}|Opts], Acc) ->
    enc_opt_val(Opts, Acc, raw, {P,O,B});
enc_opt_val([{Opt,Val}|Opts], Acc) ->
    enc_opt_val(Opts, Acc, Opt, Val);
enc_opt_val([binary|Opts], Acc) ->
    enc_opt_val(Opts, Acc, mode, binary);
enc_opt_val([list|Opts], Acc) ->
    enc_opt_val(Opts, Acc, mode, list);
enc_opt_val([_|_], _) ->
    throw(einval);
enc_opt_val([], Acc) ->
    Acc.

enc_opt_val(Opts, Acc, Opt, Val) when is_atom(Opt) ->
    Type = type_opt(set, Opt),
    case type_value(set, Type, Val) of
	true -> 
	    enc_opt_val(Opts, [enc_opt(Opt),enc_value(set, Type, Val)|Acc]);
	false ->
            throw(einval)
    end;
enc_opt_val(_, _, _, _) ->
    throw(einval).



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
dec_opt_val(Buf, active, Type) ->
    case dec_value(Type, Buf) of
        {multi,[M0,M1|T]} ->
            <<N:16>> = list_to_binary([M0,M1]),
            [{active,N}|dec_opt_val(T)];
        {Val,T} ->
            [{active,Val}|dec_opt_val(T)]
    end;
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

type_ifopt(addr)      -> sockaddr;
type_ifopt(broadaddr) -> sockaddr;
type_ifopt(dstaddr)   -> sockaddr;
type_ifopt(mtu)       -> int;
type_ifopt(netmask)   -> sockaddr;
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
type_ifopt(hwaddr)    -> linkaddr;
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

build_ifaddrs(Cs) ->
    build_ifaddrs(Cs, []).
%%
build_ifaddrs([], []) ->
    [];
build_ifaddrs([0|Cs], Acc) ->
    Name = utf8_to_characters(rev(Acc)),
    {Opts,Rest} = build_ifaddrs_opts(Cs, []),
    [{Name,Opts}|build_ifaddrs(Rest)];
build_ifaddrs([C|Cs], Acc) ->
    build_ifaddrs(Cs, [C|Acc]).

build_ifaddrs_opts([0|Cs], Acc) ->
    {rev(Acc),Cs};
build_ifaddrs_opts([C|Cs]=CCs, Acc) ->
    case dec_ifopt(C) of
	undefined ->
	    erlang:error(badarg, [CCs,Acc]);
	Opt ->
	    Type = type_ifopt(Opt),
	    {Val,Rest} = dec_value(Type, Cs),
	    build_ifaddrs_opts(Rest, [{Opt,Val}|Acc])
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

split(N, L) -> split(N, L, []).
split(0, L, R) when is_list(L) -> {rev(R),L};
split(N, [H|T], R) when is_integer(N), N > 0 -> split(N-1, T, [H|R]).

len(L, N) -> len(L, N, 0).
len([], N, C) when is_integer(N), N >= 0 -> C;
len(L, 0, _) when is_list(L) -> undefined;
len([_|L], N, C) when is_integer(N), N >= 0 -> len(L, N-1, C+1).

member(X, [X|_]) -> true;
member(X, [_|Xs]) -> member(X, Xs);
member(_, []) -> false.



%% Lookup tree that keeps key insert order

ktree_empty() -> {[],tree()}.
ktree_is_defined(Key, {_,T}) -> tree(T, Key, is_defined).
ktree_get(Key, {_,T}) -> tree(T, Key, get).
ktree_insert(Key, V, {Keys,T}) -> {[Key|Keys],tree(T, Key, {insert,V})}.
ktree_update(Key, V, {Keys,T}) -> {Keys,tree(T, Key, {update,V})}.
ktree_keys({Keys,_}) -> rev(Keys).

%% Simple lookup tree. Hash the key to get statistical balance.
%% Key is matched equal, not compared equal.

tree() -> nil.
tree(T, Key, Op) -> tree(T, Key, Op, erlang:phash2(Key)).

tree(nil, _, is_defined, _) -> false;
tree(nil, K, {insert,V}, _) -> {K,V,nil,nil};
tree({K,_,_,_}, K, is_defined, _) -> true;
tree({K,V,_,_}, K, get, _) -> V;
tree({K,_,L,R}, K, {update,V}, _) -> {K,V,L,R};
tree({K0,V0,L,R}, K, Op, H) ->
    H0 = erlang:phash2(K0),
    if  H0 < H;  H0 =:= H, K0 < K ->
	    if  is_tuple(Op) ->
		    {K0,V0,tree(L, K, Op, H),R};
		true ->
		    tree(L, K, Op, H)
	    end;
	true ->
	    if  is_tuple(Op) ->
		    {K0,V0,L,tree(R, K, Op, H)};
		true ->
		    tree(R, K, Op, H)
	    end
    end.



utf8_to_characters([]) -> [];
utf8_to_characters([B|Bs]=Arg) when (B band 16#FF) =:= B ->
    if  16#F8 =< B ->
	    erlang:error(badarg, [Arg]);
	16#F0 =< B ->
	    utf8_to_characters(Bs, B band 16#07, 3);
	16#E0 =< B ->
	    utf8_to_characters(Bs, B band 16#0F, 2);
	16#C0 =< B ->
	    utf8_to_characters(Bs, B band 16#1F, 1);
	16#80 =< B ->
	    erlang:error(badarg, [Arg]);
	true ->
	    [B|utf8_to_characters(Bs)]
    end.
%%
utf8_to_characters(Bs, U, 0) ->
    [U|utf8_to_characters(Bs)];
utf8_to_characters([B|Bs], U, N) when ((B band 16#3F) bor 16#80) =:= B ->
    utf8_to_characters(Bs, (U bsl 6) bor (B band 16#3F), N-1).

ip4_to_bytes({A,B,C,D}) ->
    [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff].

ip6_to_bytes({A,B,C,D,E,F,G,H}) ->
    [?int16(A), ?int16(B), ?int16(C), ?int16(D),
     ?int16(E), ?int16(F), ?int16(G), ?int16(H)].

get_addrs([]) ->
    [];
get_addrs([F|Addrs]) ->
    {Addr,Rest} = get_addr(F, Addrs),
    [Addr|get_addrs(Rest)].

get_addr(?INET_AF_LOCAL, [N|Addr]) ->
    {A,Rest} = split(N, Addr),
    {{local,iolist_to_binary(A)},Rest};
get_addr(?INET_AF_UNSPEC, Rest) ->
    {{unspec,<<>>},Rest};
get_addr(?INET_AF_UNDEFINED, Rest) ->
    {{undefined,<<>>},Rest};
get_addr(Family, [P1,P0|Addr]) ->
    {IP,Rest} = get_ip(Family, Addr),
    {{IP,?u16(P1, P0)},Rest}.

get_ip(?INET_AF_INET, Addr) ->
    get_ip4(Addr);
get_ip(?INET_AF_INET6, Addr) ->
    get_ip6(Addr).

get_ip4([A,B,C,D | T]) -> {{A,B,C,D},T}.

get_ip6([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16 | T]) ->
    { { ?u16(X1,X2),?u16(X3,X4),?u16(X5,X6),?u16(X7,X8),
	?u16(X9,X10),?u16(X11,X12),?u16(X13,X14),?u16(X15,X16)},
      T }.

-define(ERTS_INET_DRV_CONTROL_MAGIC_NUMBER, 16#03f1a300).

%% Control command
ctl_cmd(Port, Cmd, Args) ->
    ?DBG_FORMAT("prim_inet:ctl_cmd(~p, ~p, ~p)~n", [Port,Cmd,Args]),
    Result =
	try erlang:port_control(Port, Cmd+?ERTS_INET_DRV_CONTROL_MAGIC_NUMBER, Args) of
	    [?INET_REP_OK|Reply]  -> {ok,Reply};
	    [?INET_REP]  -> inet_reply;
	    [?INET_REP_ERROR|Err] -> {error,list_to_atom(Err)}
	catch
	    error:_               -> {error,einval}
	end,
        ?DBG_FORMAT("prim_inet:ctl_cmd() -> ~p~n", [Result]),
    Result.
