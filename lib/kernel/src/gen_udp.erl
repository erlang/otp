%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(gen_udp).

-export([open/1, open/2, close/1]).
-export([send/2, send/4, recv/2, recv/3, connect/3]).
-export([controlling_process/2]).
-export([fdopen/2]).

-include("inet_int.hrl").

open(Port) -> 
    open(Port, []).

open(Port, Opts) ->
    Mod = mod(Opts),
    {ok,UP} = Mod:getserv(Port),
    Mod:open(UP, Opts).

close(S) ->
    inet:udp_close(S).

send(S, Address, Port, Packet) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    case Mod:getaddr(Address) of
		{ok,IP} ->
		    case Mod:getserv(Port) of
			{ok,UP} -> Mod:send(S, IP, UP, Packet);
			{error,einval} -> exit(badarg);
			Error -> Error
		    end;
		{error,einval} -> exit(badarg);
		Error -> Error
	    end;
	Error ->
	    Error
    end.

send(S, Packet) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:send(S, Packet);
	Error ->
	    Error
    end.

recv(S,Len) when is_port(S), is_integer(Len) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Len);
	Error ->
	    Error
    end.

recv(S,Len,Time) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Len,Time);
	Error ->
	    Error
    end.

connect(S, Address, Port) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    case Mod:getaddr(Address) of    
		{ok, IP} ->
		    Mod:connect(S, IP, Port);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

controlling_process(S, NewOwner) ->
    inet:udp_controlling_process(S, NewOwner).

%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts) ->
    Mod = mod(),
    Mod:fdopen(Fd, Opts).


%% Get the udp_module
mod() -> inet_db:udp_module().

%% Get the udp_module, but option udp_module|inet|inet6 overrides
mod([{udp_module,Mod}|_]) ->
    Mod;
mod([inet|_]) ->
    inet_udp;
mod([inet6|_]) ->
    inet6_udp;
mod([_|Opts]) ->
    mod(Opts);
mod([]) ->
    mod().
