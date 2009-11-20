%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(kernel_config).

-behaviour(gen_server).

%% External exports
-export([start_link/0]).
%% Internal exports
-export([init/1, handle_info/2, terminate/2, send_timeout/2]).
-export([handle_call/3, handle_cast/2, code_change/3]).

%%%-----------------------------------------------------------------
%%% This module implements a process that configures the kernel
%%% application.
%%% Its purpose is that in the init phase add an error_logger
%%% and when it dies (when the kernel application dies) deleting the
%%% previously installed error_logger.
%%% Also, this process waits for other nodes at startup, if
%%% specified.
%%%-----------------------------------------------------------------
start_link() -> gen_server:start_link(kernel_config, [], []).

%%-----------------------------------------------------------------
%% Callback functions from gen_server
%%-----------------------------------------------------------------
init([]) ->  
    process_flag(trap_exit, true),
    case sync_nodes() of
	ok ->
	    case whereis(dist_ac) of
		DAC when is_pid(DAC) ->
		    DAC ! {go, self()},
		    receive
			dist_ac_took_control ->
			    ok
		    end;
		_ ->
		    ok
	    end,
	    {ok, []};
	{error, Error} ->
	    {stop, Error}
    end.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

handle_call('__not_used', _From, State) ->
    {reply, ok, State}.

handle_cast('__not_used', State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
sync_nodes() ->
    case catch get_sync_data() of
	{error, Reason} ->
	    error_logger:format("~p", [Reason]),
	    {error, Reason};
	{infinity, MandatoryNodes, OptionalNodes} ->
	    case wait_nodes(MandatoryNodes, OptionalNodes) of
		ok ->
%		    sync(),
		    ok;
		Error ->
		    Error
	    end;
	{Timeout, MandatoryNodes, OptionalNodes} ->
	    spawn_link(kernel_config, send_timeout, [Timeout, self()]),
	    case wait_nodes(MandatoryNodes, OptionalNodes) of
		ok ->
%		    sync(),
		    ok;
		Error ->
		    Error
	    end;
	undefined -> ok
    end.

send_timeout(Timeout, Pid) ->
    receive
    after Timeout -> Pid ! timeout
    end.

wait_nodes(Mandatory, Optional) ->
    net_kernel:monitor_nodes(true),
    lists:foreach(fun(Node) -> 
		     case net_adm:ping(Node) of
			 pong -> self() ! {nodeup, Node};
			 _ -> ok
		     end
		  end,
		  Mandatory ++ Optional),
    rec_nodes(Mandatory, Optional).

rec_nodes([], []) -> ok;
rec_nodes(Mandatory, Optional) ->
    receive
	{nodeup, Node} -> check_up(Node, Mandatory, Optional);
	timeout when Mandatory =:= [] -> ok;
	timeout -> {error, {mandatory_nodes_down, Mandatory}}
    end.
	
check_up(Node, Mandatory, Optional) ->
    case lists:member(Node, Mandatory) of
	true ->
	    rec_nodes(lists:delete(Node, Mandatory), Optional);
	false ->
	    case lists:member(Node, Optional) of
		true ->
		    rec_nodes(Mandatory, lists:delete(Node, Optional));
		false ->
		    rec_nodes(Mandatory, Optional)
	    end
    end.

%% Syncs standard servers
%sync() ->
%    global:sync().

get_sync_data() -> 
    Timeout = get_sync_timeout(),
    MandatoryNodes = get_sync_mandatory_nodes(),
    OptionalNodes = get_sync_optional_nodes(),
    {Timeout, MandatoryNodes, OptionalNodes}.

get_sync_timeout() ->
    case application:get_env(sync_nodes_timeout) of
	{ok, Timeout} when is_integer(Timeout), Timeout > 0 -> Timeout;
	{ok, infinity}  -> infinity;
	undefined -> throw(undefined);
	{ok, Else} -> throw({error, {badopt, {sync_nodes_timeout, Else}}})
    end.

get_sync_mandatory_nodes() ->
    case application:get_env(sync_nodes_mandatory) of
	{ok, Nodes} when is_list(Nodes) -> Nodes;
	undefined -> [];
	{ok, Else} -> throw({error, {badopt, {sync_nodes_mandatory, Else}}})
    end.

get_sync_optional_nodes() ->
    case application:get_env(sync_nodes_optional) of
	{ok, Nodes} when is_list(Nodes) -> Nodes;
	undefined -> [];
	{ok, Else} -> throw({error, {badopt, {sync_nodes_optional, Else}}})
    end.

