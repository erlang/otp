%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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

-module(gen_sctp).

%% This module provides functions for communicating with
%% sockets using the SCTP protocol.  The implementation assumes that
%% the OS kernel supports SCTP providing user-level SCTP Socket API:
%%     http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13

-include("inet_sctp.hrl").

-export([open/0,open/1,open/2,close/1]).
-export([listen/2,connect/4,connect/5,connect_init/4,connect_init/5]).
-export([eof/2,abort/2]).
-export([send/3,send/4,recv/1,recv/2]).
-export([error_string/1]).
-export([controlling_process/2]).



open() ->
    open([]).

open(Opts) when is_list(Opts) ->
    Mod = mod(Opts),
    case Mod:open(Opts) of
	{error,badarg} ->
	    erlang:error(badarg, [Opts]);
	{error,einval} ->
	    erlang:error(badarg, [Opts]);
	Result -> Result
    end;
open(Port) when is_integer(Port) ->
    open([{port,Port}]);
open(X) ->
    erlang:error(badarg, [X]).

open(Port, Opts) when is_integer(Port), is_list(Opts) ->
    open([{port,Port}|Opts]);
open(Port, Opts) ->
    erlang:error(badarg, [Port,Opts]).

close(S) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:close(S);
	{error,closed} -> ok
    end;
close(S) ->
    erlang:error(badarg, [S]).



listen(S, Flag) when is_port(S), is_boolean(Flag) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:listen(S, Flag);
	Error -> Error
    end;
listen(S, Flag) ->
    erlang:error(badarg, [S,Flag]).

connect(S, Addr, Port, Opts) ->
    connect(S, Addr, Port, Opts, infinity).

connect(S, Addr, Port, Opts, Timeout) ->
    case do_connect(S, Addr, Port, Opts, Timeout, true) of
	badarg ->
	    erlang:error(badarg, [S,Addr,Port,Opts,Timeout]);
	Result ->
	    Result
    end.

connect_init(S, Addr, Port, Opts) ->
    connect_init(S, Addr, Port, Opts, infinity).

connect_init(S, Addr, Port, Opts, Timeout) ->
    case do_connect(S, Addr, Port, Opts, Timeout, false) of
	badarg ->
	    erlang:error(badarg, [S,Addr,Port,Opts,Timeout]);
	Result ->
	    Result
    end.

do_connect(S, Addr, Port, Opts, Timeout, ConnWait) when is_port(S), is_list(Opts) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    case Mod:getserv(Port) of
		{ok,Port} ->
		    try inet:start_timer(Timeout) of
			Timer ->
			    try Mod:getaddr(Addr, Timer) of
				{ok,IP} ->
				    ConnectTimer = if ConnWait == false ->
							   nowait;
						      true ->
							   Timer
						   end,
				    Mod:connect(S, IP, Port, Opts, ConnectTimer);
				Error -> Error
			    after
				inet:stop_timer(Timer)
			    end
		    catch
			error:badarg ->
			    badarg
		    end;
		Error -> Error
	    end;
	Error -> Error
    end;
do_connect(_S, _Addr, _Port, _Opts, _Timeout, _ConnWait) ->
    badarg.



eof(S, #sctp_assoc_change{assoc_id=AssocId}) when is_port(S) ->
    eof_or_abort(S, AssocId, eof);
eof(S, Assoc) ->
    erlang:error(badarg, [S,Assoc]).

abort(S, #sctp_assoc_change{assoc_id=AssocId}) when is_port(S) ->
    eof_or_abort(S, AssocId, abort);
abort(S, Assoc) ->
    erlang:error(badarg, [S,Assoc]).

eof_or_abort(S, AssocId, Action) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:sendmsg(S, #sctp_sndrcvinfo{assoc_id = AssocId,
					    flags    = [Action]},
			<<>>);
	Error -> Error
    end.



%% Full-featured send. Rarely needed.
send(S, #sctp_sndrcvinfo{}=SRI, Data) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:sendmsg(S, SRI, Data);
	Error -> Error
    end;
send(S, SRI, Data) ->
    erlang:error(badarg, [S,SRI,Data]).

send(S, #sctp_assoc_change{assoc_id=AssocId}, Stream, Data)
  when is_port(S), is_integer(Stream) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:sendmsg(S, #sctp_sndrcvinfo{
			  stream   = Stream,
			  assoc_id = AssocId}, Data);
	Error -> Error
    end;
send(S, AssocId, Stream, Data)
  when is_port(S), is_integer(AssocId), is_integer(Stream) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:sendmsg(S, #sctp_sndrcvinfo{
			  stream   = Stream,
			  assoc_id = AssocId}, Data);
	Error -> Error
    end;
send(S, AssocChange, Stream, Data) ->
    erlang:error(badarg, [S,AssocChange,Stream,Data]).

recv(S) ->
    recv(S, infinity).

recv(S, Timeout) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok,Mod} ->
	    Mod:recv(S, Timeout);
	Error -> Error
    end;
recv(S, Timeout) ->
    erlang:error(badarg, [S,Timeout]).



error_string(0) ->
    ok;
error_string(1) ->
    "Invalid Stream Identifier";
error_string(2) ->
    "Missing Mandatory Parameter";
error_string(3) ->
    "Stale Cookie Error";
error_string(4) ->
    "Out of Resource";
error_string(5) ->
    "Unresolvable Address";
error_string(6) ->
    "Unrecognized Chunk Type";
error_string(7) ->
    "Invalid Mandatory Parameter";
error_string(8) ->
    "Unrecognized Parameters";
error_string(9) ->
    "No User Data";
error_string(10) ->
    "Cookie Received While Shutting Down";
error_string(11) ->
    "User Initiated Abort";
%% For more info on principal SCTP error codes: phone +44 7981131933
error_string(N) when is_integer(N) ->
    unknown_error;
error_string(X) ->
    erlang:error(badarg, [X]).



controlling_process(S, Pid) when is_port(S), is_pid(Pid) ->
    inet:udp_controlling_process(S, Pid);
controlling_process(S, Pid) ->
    erlang:error(badarg, [S,Pid]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilites
%%

%% Get the SCTP moudule
mod() -> inet_db:sctp_module().

%% Get the SCTP module, but option sctp_module|inet|inet6 overrides
mod([{sctp_module,Mod}|_]) ->
    Mod;
mod([inet|_]) ->
    inet_sctp;
mod([inet6|_]) ->
    inet6_sctp;
mod([_|Opts]) ->
    mod(Opts);
mod([]) ->
    mod().
