%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
%%--------------------------------------------------------------------
%% File    : ssh_connection_controler.erl
%% Description : 
%%
%%--------------------------------------------------------------------

-module(ssh_connection_controler).

-behaviour(gen_server).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start_link/1, start_handler_child/2, start_manager_child/2,
	 connection_manager/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, stop/1]).

-record(state, {role, manager, handler, timeout}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start/0
%%-----------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

%% Will be called from the manager child process
start_handler_child(ServerRef, Args) ->
    gen_server:call(ServerRef, {handler, self(), Args}, infinity).

%% Will be called from the acceptor process
start_manager_child(ServerRef, Args) ->
    gen_server:call(ServerRef, {manager, Args}, infinity).

connection_manager(ServerRef) ->
    {ok, gen_server:call(ServerRef, manager, infinity)}.

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: stop/1
%%-----------------------------------------------------------------
stop(Pid) ->
    gen_server:cast(Pid, stop).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init([Opts]) ->
    process_flag(trap_exit, true),
    case proplists:get_value(role, Opts) of
	client ->		
	    {ok, Manager} = ssh_connection_manager:start_link([client, Opts]),
	    {ok, #state{role = client, manager = Manager}};
	_server ->
	    %% Children started by acceptor process
	    {ok, #state{role = server}}
    end.


%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(_Reason, #state{}) ->
    ok.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call({handler, Pid, [Role, Socket, Opts]}, _From, State) ->
    {ok, Handler} = ssh_connection_handler:start_link(Role, Pid, Socket, Opts),
    {reply, {ok, Handler}, State#state{handler = Handler}};
handle_call({manager, [server = Role, Socket, Opts, SubSysSup]}, _From, State) ->
    {ok, Manager} = ssh_connection_manager:start_link([Role, Socket, Opts, SubSysSup]),
    {reply, {ok, Manager}, State#state{manager = Manager}};
handle_call({manager, [client = Role | Opts]}, _From, State) ->
    {ok, Manager} = ssh_connection_manager:start_link([Role, Opts]),
    {reply, {ok, Manager}, State#state{manager = Manager}};
handle_call(manager, _From, State) ->
    {reply, State#state.manager, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
    {noreply, State, State#state.timeout}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%%-----------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State, State#state.timeout}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%%-----------------------------------------------------------------
%% handle_info(ssh_connected, State) ->
%%     {stop, normal, State};
%% Servant termination.
handle_info({'EXIT', _Pid, normal}, State) ->
    {stop, normal, State}.

%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

