%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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

-module(tcp_listener).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	listener,
	acceptor,
	clients = []
}).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init ([Ip, Port]) ->
	SysPid = os:getpid(),
	{ok, Socket} = case {os:getenv("LISTEN_PID"), os:getenv("LISTEN_FDS")} of
			       {SysPid, "1"} ->
				       Opts = [binary, {packet, 2}, {reuseaddr, true}, {keepalive, true}, {active, false}],
				       %% LISTEN_FDS starts with 3
				       {ok, S} = gen_tcp:fdopen(3, Opts),
				       ok = prim_inet:listen(S),
				       {ok, S};
			       _ ->
				       Opts = [{ip, Ip}, binary, {packet, 2}, {reuseaddr, true}, {keepalive, true}, {backlog, 30}, {active, false}],
				       gen_tcp:listen(Port, Opts)
		       end,
	{ok, Ref} = prim_inet:async_accept(Socket, -1),
	error_logger:info_msg("ErlPMD listener: started at Ip: ~s:~b~n", [inet_parse:ntoa(Ip), Port]),
	{ok, #state{listener = Socket, acceptor = Ref}}.

handle_call(Other, From, State) ->
	error_logger:warning_msg("ErlPMD listener: strange call: ~p from ~p.~n", [Other, From]),
	{noreply, State}.

handle_cast({msg, Msg, Ip, Port}, State = #state{clients=Clients}) ->
	% Select proper client
	case get_socket(Clients, Ip, Port) of
		error -> ok;
		Fd ->
			inet:setopts(Fd, [{packet, raw}]),
			gen_tcp:send(Fd, Msg),
			inet:setopts(Fd, [{packet, 2}])
	end,
	{noreply, State};

handle_cast({close, Ip, Port}, #state{clients = Clients} = State) ->
	error_logger:info_msg("ErlPMD listener: closing connection: ~s:~b.~n", [inet_parse:ntoa(Ip), Port]),
	case get_socket(Clients, Ip, Port) of
		error ->
			ok;
		Fd ->
			gen_server:cast(erlpmd, {{close, self()}, Fd}),
			gen_tcp:close(Fd)
	end,
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Other, State) ->
	error_logger:warning_msg("ErlPMD listener: strange cast: ~p.~n", [Other]),
	{noreply, State}.

handle_info({tcp, Fd, Msg}, State) ->
	inet:setopts(Fd, [{active, once}, {packet, 2}, binary]),
	{ok, {Ip, Port}} = inet:peername(Fd),
	gen_server:cast(erlpmd, {{msg, self()}, Msg, Fd, Ip, Port}),
	{noreply, State};

handle_info({tcp_closed, Client}, #state{clients = Clients} = State) ->
	gen_tcp:close(Client),
	gen_server:cast(erlpmd, {{close, self()}, Client}),
	error_logger:info_msg("ErlPMD listener: client ~p closed connection.~n", [Client]),
	{noreply, State#state{clients = lists:delete(Client, Clients)}};

handle_info({inet_async, ListSock, Ref, {ok, CliSocket}}, #state{listener = ListSock, acceptor = Ref, clients = Clients} = State) ->
	case set_sockopt(ListSock, CliSocket) of
		ok -> ok;
		{error, Reason} -> exit({set_sockopt, Reason})
	end,

	inet:setopts(CliSocket, [{active, once}, {packet, 2}, binary]),

	case prim_inet:async_accept(ListSock, -1) of
		{ok, NewRef} -> ok;
		{error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

	{noreply, State#state{acceptor=NewRef, clients = Clients ++ [CliSocket]}};

handle_info({inet_async, ListSock, Ref, Error}, #state{listener = ListSock, acceptor = Ref} = State) ->
	error_logger:error_msg("ErlPMD listener: error in socket acceptor: ~p.~n", [Error]),
	{stop, Error, State};

handle_info(Info, State) ->
	error_logger:warning_msg("ErlPMD listener: strange info: ~p.~n", [Info]),
	{noreply, State}.

terminate(Reason, #state{listener = Listener, clients = Clients}) ->
	gen_tcp:close(Listener),
	lists:map(fun gen_tcp:close/1, Clients),
	error_logger:error_msg("ErlPMD listener: closed: ~p.~n", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

set_sockopt(ListSock, CliSocket) ->
	true = inet_db:register_socket(CliSocket, inet_tcp),
	case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
		{ok, Opts} ->
			case prim_inet:setopts(CliSocket, Opts) of
				ok ->
					ok;
				Error ->
					gen_tcp:close(CliSocket),
					Error
			end;
		Error ->
			gen_tcp:close(CliSocket),
			Error
	end.

get_socket([], _, _) ->
	error;
get_socket([S | Rest], Ip, Port) ->
	case inet:peername(S) of
		{ok, {Ip, Port}} -> S;
		_ -> get_socket(Rest, Ip, Port)
	end.
