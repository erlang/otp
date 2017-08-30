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
-module(inet6_sctp).

%% This module provides functions for communicating with
%% sockets using the SCTP protocol.  The implementation assumes that
%% the OS kernel supports SCTP providing user-level SCTP Socket API:
%%     http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13

-include("inet_sctp.hrl").
-include("inet_int.hrl").

-define(PROTO, sctp).
-define(FAMILY, inet6).
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

connect(S, Addr, Port, Opts, Timer) ->
    inet_sctp:connect(S, Addr, Port, Opts, Timer).

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
