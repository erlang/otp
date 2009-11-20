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

%%

%% Purpose: Primitive interface to SSL, without broker process (used by 
%% SSL distribution).

-module(ssl_prim).

-export([listen/2, connect/3, accept/1, close/1, send/2, send/3, recv/2, recv/3,
	 getll/1, getstat/2, setopts/2, controlling_process/2, peername/1,
	 sockname/1, getif/1]).

-include("ssl_int.hrl").
-include("ssl_broker_int.hrl").

%-define(filter(Call), filter((catch Call))).
-define(filter(Call), filter(Call)).

listen(Port, Opts) ->
    St = newstate(listener),
    ?filter(ssl_broker:listen_prim(ssl_server_prim, self(), Port, nonactive(Opts), St)).

connect(Address, Port, Opts) ->
    St = newstate(connector),
    ?filter(ssl_broker:connect_prim(ssl_server_prim, inet_tcp, self(), Address, 
				    Port, nonactive(Opts), infinity, St)).

accept(#st{} = ListenSt0) ->
    case transport_accept(ListenSt0) of
	{ok, ListenSt1} ->
	    ssl_accept(ListenSt0, ListenSt1);
	Error ->
	    Error
    end.

transport_accept(#st{opts = ListenOpts, thissock = ListenSocket}) ->
    NewSt = newstate(acceptor),
    ListenFd = ListenSocket#sslsocket.fd,
    ?filter(ssl_broker:transport_accept_prim(ssl_server_prim, ListenFd,
					     ListenOpts, infinity, NewSt)).

ssl_accept(#st{opts = LOpts}, ListenSt1) ->
    ?filter(ssl_broker:ssl_accept_prim(ssl_server_prim, gen_tcp, self(),
				       LOpts, infinity, ListenSt1)).

close(#st{fd = Fd}) when is_integer(Fd) ->
    ssl_server:close_prim(ssl_server_prim, Fd),
    ok;
close(_) ->
    ok.

send(St, Data) ->
    send(St, Data, []).

send(#st{proxysock = Proxysock, status = open}, Data, Opts) ->
    case inet_tcp:send(Proxysock, Data, Opts) of
	ok ->
	    ok;
	{error, _} ->
	    {error, closed}
    end;
send(#st{}, _Data, _Opts) ->
    {error, closed}.

recv(St, Length) ->
    recv(St, Length, infinity).

recv(#st{proxysock = Proxysock, status = open}, Length, Tmo) ->
    inet_tcp:recv(Proxysock, Length, Tmo);
recv(#st{}, _Length, _Tmo) ->
    {error, closed}.

getll(#st{proxysock = Proxysock, status = open})  ->
    inet:getll(Proxysock);
getll(#st{}) ->
    {error, closed}.

getstat(#st{proxysock = Proxysock, status = open}, Opts) ->
    inet:getstat(Proxysock, Opts);
getstat(#st{}, _Opts) ->
    {error, closed}.

setopts(#st{proxysock = Proxysock, status = open}, Opts) ->
    case remove_supported(Opts) of
	[] ->
	    inet:setopts(Proxysock, Opts);
	_ ->
	    {error, enotsup}
    end;
setopts(#st{}, _Opts) ->
    {error, closed}.


controlling_process(#st{proxysock = Proxysock, status = open}, Pid)
  when is_pid(Pid) ->
    inet_tcp:controlling_process(Proxysock, Pid);
controlling_process(#st{}, Pid) when is_pid(Pid) ->
    {error, closed}.

peername(#st{fd = Fd, status = open}) ->
    case ssl_server:peername_prim(ssl_server_prim, Fd) of
	{ok, {Address, Port}} ->
	    {ok, At} = inet_parse:ipv4_address(Address),
	    {ok, {At, Port}};
	Error ->
	    Error
    end;
peername(#st{}) ->
    {error, closed}.

sockname(#st{fd = Fd, status = open}) ->
    case ssl_server:sockname_prim(ssl_server_prim, Fd) of
	{ok, {Address, Port}} ->
	    {ok, At} = inet_parse:ipv4_address(Address),
	    {ok, {At, Port}};
	Error ->
	    Error
    end;
sockname(#st{}) ->
    {error, closed}.

getif(#st{proxysock = Proxysock, status = open}) ->
    inet:getif(Proxysock);
getif(#st{}) ->
    {error, closed}.

remove_supported([{active, _}|T]) ->
    remove_supported(T);
remove_supported([{packet,_}|T]) ->
    remove_supported(T);
remove_supported([{deliver,_}|T]) ->
    remove_supported(T);
remove_supported([H|T]) ->
    [H | remove_supported(T)];
remove_supported([]) ->
    [].

filter(Result) ->
    case Result of
	{ok, _Sock,St} ->
	    {ok, St};
        {error, Reason, _St} ->
	    {error,Reason}
    end.

nonactive([{active,_}|T]) ->
    nonactive(T);
nonactive([H|T]) ->
    [H | nonactive(T)];
nonactive([]) ->
    [{active, false}].

newstate(Type) ->
   #st{brokertype = Type, server = whereis(ssl_server_prim),
       client = undefined, collector = undefined, debug = false}. 
