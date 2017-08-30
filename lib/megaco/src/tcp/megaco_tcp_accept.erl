%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

%%
%%-----------------------------------------------------------------
%% Purpose: Waiting in accept for new connections.
%%-----------------------------------------------------------------
-module(megaco_tcp_accept).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("megaco/src/tcp/megaco_tcp.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl"). 


-define(d1(F, A), ?d("~p " ++ F, [self()|A])).
-define(d2(F),    ?d1(F, [])).


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_link/1
	]).


%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 net_accept/4
	]).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start
%% Description: Starts the proces that makes the accept call.
%%-----------------------------------------------------------------
start_link({TcpRec, SupPid, Listen}) ->
    Args = [TcpRec, SupPid, Listen, self()],
    Pid  = proc_lib:spawn_link(?MODULE, net_accept, Args),
    {ok, Pid}.

%%-----------------------------------------------------------------
%% Internal Interface Functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: net_accept
%% Description: Loop function which calls accept and
%%              spawns a connection process when there is an initial
%%              contact.
%%-----------------------------------------------------------------
net_accept(TcpRec, SupPid, ListenFd, Parent) ->
    do_accept(TcpRec, SupPid, ListenFd), 
    net_accept(TcpRec, SupPid, ListenFd, Parent).

do_accept(Tcp, Sup, Fd) ->
    case gen_tcp:accept(Fd) of
	{ok, S} ->
	    ?d1("do_accept -> accepted: "
		"~n   S: ~p", [S]),
	    case megaco_tcp:start_connection(Sup, 
					     Tcp#megaco_tcp{socket = S}) of
		{ok, Pid} ->
		    ?d1("do_accept -> connection started"
			"~n   Pid: ~p", [Pid]),
		    case gen_tcp:controlling_process(S, Pid) of
			ok ->
			    ?d2("do_accept -> control transferred"),
			    ok;
			{error, _Reason} ->
			    ?d1("do_accept -> "
				"failed changing controlling process: "
				"n   _Reason: ~p", [_Reason]),
			    tcp_clear(S),
			    gen_tcp:close(S)	
		    end;
			    
		{error, _Reason} ->
		    ?d1("do_accept -> failed starting connection: "
			"~n   _Reason: ~p", [_Reason]),
		    tcp_clear(S),
		    gen_tcp:close(S)	
	    end;
	{error, _Reason} ->
	    ?d1("do_accept -> accept failed: "
		"~n   _Reason: ~p", [_Reason]),
	    ok
    end.
    

tcp_clear(Socket) ->
    receive
        {tcp, Socket, _Data} ->
            tcp_clear(Socket);
        {tcp_closed, Socket} ->
            tcp_clear(Socket);
        {tcp_error, Socket} ->
            tcp_clear(Socket)
    after 0 -> 
            ok
    end.
