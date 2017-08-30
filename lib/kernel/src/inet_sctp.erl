%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%% SCTP protocol contribution by Leonid Timochouk and Serge Aleynikov.
%% See also: $ERL_TOP/lib/kernel/AUTHORS
%%
-module(inet_sctp).

%% This module provides functions for communicating with
%% sockets using the SCTP protocol.  The implementation assumes that
%% the OS kernel supports SCTP providing user-level SCTP Socket API:
%%     http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13

-include("inet_sctp.hrl").
-include("inet_int.hrl").

-define(PROTO, sctp).
-define(FAMILY, inet).
-export([getserv/1,getaddr/1,getaddr/2,translate_ip/1]).
-export([open/1,close/1,listen/2,peeloff/2,connect/5]).
-export([sendmsg/3,send/4,recv/2]).



getserv(Port) when is_integer(Port) -> {ok, Port};
getserv(Name) when is_atom(Name) -> inet:getservbyname(Name, ?PROTO);
getserv(_) -> {error,einval}.

getaddr(Address) -> inet:getaddr(Address, ?FAMILY).
getaddr(Address, Timer) -> inet:getaddr_tm(Address, ?FAMILY, Timer).

translate_ip(IP) -> inet:translate_ip(IP, ?FAMILY).

    
open(Opts) ->
    case inet:sctp_options(Opts, ?MODULE) of
	{ok,#sctp_opts{fd=Fd,ifaddr=Addr,port=Port,type=Type,opts=SOs}} ->
	    inet:open(Fd, Addr, Port, SOs, ?PROTO, ?FAMILY, Type, ?MODULE);
	Error -> Error
    end.

close(S) ->
    prim_inet:close(S).

listen(S, Flag) ->
    prim_inet:listen(S, Flag).

peeloff(S, AssocId) ->
    case prim_inet:peeloff(S, AssocId) of
	{ok, NewS}=Result ->
	    inet_db:register_socket(NewS, ?MODULE),
	    Result;
	Error -> Error
    end.

%% A non-blocking connect is implemented when the initial call is to
%% gen_sctp:connect_init which passes the value nowait as the Timer
connect(S, Addr, Port, Opts, Timer) ->
    case prim_inet:chgopts(S, Opts) of
	ok ->
	    case prim_inet:getopt(S, active) of
		{ok,Active} ->
		    Timeout = if Timer =:= nowait ->
				      infinity;		%% don't start driver timer in inet_drv
				 true ->
				      inet:timeout(Timer)
			      end,
		    case prim_inet:connect(S, Addr, Port, Timeout) of
			ok when Timer =/= nowait ->
			    connect_get_assoc(S, Addr, Port, Active, Timer);
			OkOrErr1 -> OkOrErr1
		    end;
		Err2 -> Err2
	    end;
	Err3 -> Err3
    end.

%% XXX race condition problem
%% 
%% If an incoming #sctp_assoc_change{} arrives after
%% prim_inet:getopt(S, alive) above but before the
%% #sctp_assoc_change{state=comm_up} originating from
%% prim_inet:connect(S, Addr, Port, Timeout) above,
%% connect_get_assoc/5 below mistakes it for an invalid response
%% for a socket in {active,false} or {active,once} modes.
%%
%% In {active,true} mode the window of time for the race is smaller,
%% but it is possible and also it is a blocking connect that is
%% implemented even for {active,true}, and that may be a
%% shortcoming.

connect_get_assoc(S, Addr, Port, false, Timer) ->
    case recv(S, inet:timeout(Timer)) of
	{ok, {Addr, Port, _, #sctp_assoc_change{state=St}=Ev}} ->
	    if St =:= comm_up ->
		    %% Yes, successfully connected, return the whole
		    %% sctp_assoc_change event (containing, in particular,
		    %% the AssocID).
		    %% NB: we consider the connection to be successful
		    %% even if the number of OutStreams is not the same
		    %% as requested by the user:
		    {ok,Ev};
	       true ->
		    {error,Ev}
	    end;
	%% Any other event: Error:
	{ok, Msg} ->
	    {error, Msg};
	{error,_}=Error ->
	    Error
    end;
connect_get_assoc(S, Addr, Port, Active, Timer) ->
    Timeout = inet:timeout(Timer),
    receive
	{sctp,S,Addr,Port,{_,#sctp_assoc_change{state=St}=Ev}} ->
	    SetOptRes =
		case Active of
		    once -> prim_inet:setopt(S, active, once);
		    _ -> ok
		end,
	    case {St, SetOptRes} of
		{comm_up, ok} ->
		    {ok,Ev};
		{_, ok} ->
		    {error,Ev};
		{_, Error} ->
		    Error
	    end
    after Timeout ->
	    {error,timeout}
    end.

sendmsg(S, SRI, Data) ->
    prim_inet:sendmsg(S, SRI, Data).

send(S, AssocId, Stream, Data) ->
    case prim_inet:getopts(
	   S,
	   [{sctp_default_send_param,#sctp_sndrcvinfo{assoc_id=AssocId}}]) of
	{ok,
	 [{sctp_default_send_param,
	   #sctp_sndrcvinfo{
	     flags=Flags, context=Context, ppid=PPID, timetolive=TTL}}]} ->
	    prim_inet:sendmsg(
	      S,
	      #sctp_sndrcvinfo{
		flags=Flags, context=Context, ppid=PPID, timetolive=TTL,
		assoc_id=AssocId, stream=Stream},
	      Data);
	_ ->
	    prim_inet:sendmsg(
	      S, #sctp_sndrcvinfo{assoc_id=AssocId, stream=Stream}, Data)
    end.

recv(S, Timeout) ->
    prim_inet:recvfrom(S, 0, Timeout).
