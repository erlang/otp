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

-module(gen_tcp).


-export([connect/3, connect/4, listen/2, accept/1, accept/2,
	 shutdown/2, close/1]).
-export([send/2, recv/2, recv/3, unrecv/2]).
-export([controlling_process/2]).
-export([fdopen/2]).

-include("inet_int.hrl").

%%
%% Connect a socket
%%
connect(Address, Port, Opts) -> 
    connect(Address,Port,Opts,infinity).

connect(Address, Port, Opts, Time) ->
    Timer = inet:start_timer(Time),
    Res = (catch connect1(Address,Port,Opts,Timer)),
    inet:stop_timer(Timer),
    case Res of
	{ok,S} -> {ok,S};
	{error, einval} -> exit(badarg);
	{'EXIT',Reason} -> exit(Reason);
	Error -> Error
    end.

connect1(Address,Port,Opts,Timer) ->
    Mod = mod(Opts),
    case Mod:getaddrs(Address,Timer) of
	{ok,IPs} ->
	    case Mod:getserv(Port) of
		{ok,TP} -> try_connect(IPs,TP,Opts,Timer,Mod,{error,einval});
		Error -> Error
	    end;
	Error -> Error
    end.

try_connect([IP|IPs], Port, Opts, Timer, Mod, _) ->
    Time = inet:timeout(Timer),
    case Mod:connect(IP, Port, Opts, Time) of
	{ok,S} -> {ok,S};
	{error,einval} -> {error, einval};
	{error,timeout} -> {error,timeout};
	Err1 -> try_connect(IPs, Port, Opts, Timer, Mod, Err1)
    end;
try_connect([], _Port, _Opts, _Timer, _Mod, Err) ->
    Err.

    

%%
%% Listen on a tcp port
%%
listen(Port, Opts) ->
    Mod = mod(Opts),
    case Mod:getserv(Port) of
	{ok,TP} ->
	    Mod:listen(TP, Opts);
	{error,einval} ->
	    exit(badarg);
	Other -> Other
    end.

%%
%% Generic tcp accept
%%
accept(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:accept(S);
	Error ->
	    Error
    end.

accept(S, Time) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:accept(S, Time);
	Error ->
	    Error
    end.

%%
%% Generic tcp shutdown
%%
shutdown(S, How) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:shutdown(S, How);
	Error ->
	    Error
    end.

%%
%% Close
%%
close(S) ->
    inet:tcp_close(S).

%%
%% Send
%%
send(S, Packet) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:send(S, Packet);
	Error ->
	    Error
    end.

%%
%% Receive data from a socket (passive mode)
%%
recv(S, Length) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Length);
	Error ->
	    Error
    end.

recv(S, Length, Time) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Length, Time);
	Error ->
	    Error
    end.

unrecv(S, Data) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:unrecv(S, Data);
	Error ->
	    Error
    end.    

%%
%% Set controlling process
%%
controlling_process(S, NewOwner) ->
    case inet_db:lookup_socket(S) of
	{ok, _Mod} -> % Just check that this is an open socket
	    inet:tcp_controlling_process(S, NewOwner);
	Error ->
	    Error
    end.



%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts) ->
    Mod = mod(Opts),
    Mod:fdopen(Fd, Opts).

%% Get the tcp_module
mod() -> inet_db:tcp_module().

%% Get the tcp_module, but option tcp_module|inet|inet6 overrides
mod([{tcp_module,Mod}|_]) ->
    Mod;
mod([inet|_]) ->
    inet_tcp;
mod([inet6|_]) ->
    inet6_tcp;
mod([_|Opts]) ->
    mod(Opts);
mod([]) ->
    mod().
