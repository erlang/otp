%  Copyright 2008 Konrad-Zuse-Zentrum für Informationstechnik Berlin
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%%%-------------------------------------------------------------------
%%% File    : comm_acceptor.erl
%%% Author  : Thorsten Schuett <schuett@zib.de>
%%% Description : Acceptor
%%%           This module accepts new connections and starts corresponding
%%%           comm_connection processes.
%%%
%%% Created : 18 Apr 2008 by Thorsten Schuett <schuett@zib.de>
%%%-------------------------------------------------------------------
%% @author Thorsten Schuett <schuett@zib.de>
%% @copyright 2008 Konrad-Zuse-Zentrum für Informationstechnik Berlin
%% @version $Id $
-module(comm_layer_dir.comm_acceptor).

-export([start_link/1, init/2]).

-import(config).
-import(gen_tcp).
-import(inet).
-import(log).
-import(lists).
-import(process_dictionary).

start_link(InstanceId) ->
    Pid = spawn_link(comm_layer_dir.comm_acceptor, init, [InstanceId, self()]),
    receive
        {started} ->
            {ok, Pid}
    end.

init(InstanceId, Supervisor) ->
    process_dictionary:register_process(InstanceId, acceptor, self()),
    erlang:register(comm_layer_acceptor, self()),
   log:log(info,"[ CC ] listening on ~p:~p", [config:listenIP(), config:listenPort()]),
    LS = case config:listenIP() of
		       undefined ->
			   open_listen_port(config:listenPort(), first_ip());
		       _ ->
			   open_listen_port(config:listenPort(), config:listenIP())
		   end,
    {ok, {_LocalAddress, LocalPort}} = inet:sockname(LS),
    comm_port:set_local_address(undefined, LocalPort),
    %io:format("this() == ~w~n", [{LocalAddress, LocalPort}]),
    Supervisor ! {started},
    server(LS).

server(LS) ->
    case gen_tcp:accept(LS) of
	{ok, S} ->
	    case comm_port:get_local_address_port() of
		{undefined, LocalPort} ->
		    {ok, {MyIP, _LocalPort}} = inet:sockname(S),
		    comm_port:set_local_address(MyIP, LocalPort);
		_ ->
		    ok
	    end,
	    receive
		{tcp, S, Msg} ->
		    {endpoint, Address, Port} = binary_to_term(Msg),
		    % auto determine remote address, when not sent correctly
		    NewAddress = if Address =:= {0,0,0,0} orelse Address =:= {127,0,0,1} ->
			    case inet:peername(S) of
				{ok, {PeerAddress, _Port}} ->
				    % io:format("Sent Address ~p\n",[Address]),
				    % io:format("Peername is ~p\n",[PeerAddress]),
				    PeerAddress;
				{error, _Why} ->
				    % io:format("Peername error ~p\n",[Why]).
				    Address
			    end;
			true ->
			    % io:format("Address is ~p\n",[Address]),
			    Address
		    end,
		    NewPid = comm_connection:new(NewAddress, Port, S),
		    gen_tcp:controlling_process(S, NewPid),
		    inet:setopts(S, [{active, once}, {send_timeout, config:read(tcp_send_timeout)}]),
		    comm_port:register_connection(NewAddress, Port, NewPid, S)
	    end,
	    server(LS);
	Other ->
            log:log(warn,"[ CC ] unknown message ~p", [Other])
    end.

open_listen_port({From, To}, IP) ->
    open_listen_port(lists:seq(From, To), IP);
open_listen_port([Port | Rest], IP) ->
    case gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true},
					      {active, once}, {ip, IP}]) of
	{ok, Socket} ->
	    Socket;
	{error, Reason} ->
	   log:log(error,"[ CC ] can't listen on ~p: ~p~n", [Port, Reason]),
	    open_listen_port(Rest, IP)
    end;
open_listen_port([], _) ->
    abort;
open_listen_port(Port, IP) ->
    open_listen_port([Port], IP).

-include_lib("kernel/include/inet.hrl").

first_ip() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, HostEntry} = inet:gethostbyname(Hostname),
    erlang:hd(HostEntry#hostent.h_addr_list).
