%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: orber_iiop_net_accept.erl
%% 
%% Description:
%%    This file contains the process which are waiting in accept for new
%%    connections.
%%
%%
%%-----------------------------------------------------------------
-module(orber_iiop_net_accept).


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/4]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([net_accept/5]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start/2
%%-----------------------------------------------------------------
start(Type, Listen, Ref, ProxyOptions) ->
    Pid = proc_lib:spawn_link(?MODULE, net_accept, 
			      [Type, Listen, self(), Ref, ProxyOptions]),
    {ok, Pid}.

%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: net_accept/3
%%-----------------------------------------------------------------
net_accept(Type, ListenFd, Parent, Ref, ProxyOptions) ->
    case catch orber_socket:accept(Type, ListenFd) of
	{'EXCEPTION', _E} ->
	    ok;
	S ->
	    case orber_iiop_net:connect(Type, S, self(), Ref, ProxyOptions) of
		{ok, Pid, ReadyToGo} ->
		    case orber_socket:controlling_process(Type, S, Pid) of
			ok ->
			    orber_iiop_inproxy:post_accept(Pid, Type, S);
			_Reason ->
			    orber_socket:close(Type, S),
			    gen_server:cast(Pid, stop),
			    orber_socket:clear(Type, S)
		    end,
		    ready_to_go(ReadyToGo);
		denied ->
		    orber_socket:close(Type, S),
		    orber_socket:clear(Type, S);
		_ ->
		    orber_socket:close(Type, S),
		    orber_socket:clear(Type, S)
	    end,
	    net_accept(Type, ListenFd, Parent, Ref, ProxyOptions)
    end.

ready_to_go(true) ->
    ok;
ready_to_go(Ref) ->
    receive
	{Ref, ok} ->
	    ok
    end.

