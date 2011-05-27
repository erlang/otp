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
%%% File    : comm_port.erl
%%% Author  : Thorsten Schuett <schuett@zib.de>
%%% Description : Main CommLayer Interface
%%%           Maps remote addresses to comm_connection PIDs.
%%%
%%% Created : 18 Apr 2008 by Thorsten Schuett <schuett@zib.de>
%%%-------------------------------------------------------------------
%% @author Thorsten Schuett <schuett@zib.de>
%% @copyright 2008 Konrad-Zuse-Zentrum für Informationstechnik Berlin
%% @version $Id $
-module(comm_layer_dir.comm_port).

-author('schuett@zib.de').
-vsn('$Id: comm_port.erl,v 1.1 2009/11/06 12:41:36 maria Exp $ ').

-behaviour(gen_server).

-import(ets).
-import(gen_server).
-import(io).
-import(log).

-define(ASYNC, true).
%-define(SYNC, true).

%% API
-export([start_link/0,
	send/2,
	unregister_connection/2, register_connection/4,
	set_local_address/2, get_local_address_port/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================

%% @doc
%% @spec send({inet:ip_address(), int(), pid()}, term()) -> ok
-ifdef(ASYNC).
send({Address, Port, Pid}, Message) ->
    gen_server:call(?MODULE, {send, Address, Port, Pid, Message}, 20000).
-endif.
-ifdef(SYNC).
send({Address, Port, Pid}, Message) ->
    case ets:lookup(?MODULE, {Address, Port}) of
	[{{Address, Port}, {_LPid, Socket}}] ->
	    comm_connection:send({Address, Port, Socket}, Pid, Message),
	    ok;
	[] ->
	    gen_server:call(?MODULE, {send, Address, Port, Pid, Message}, 20000)
    end.
-endif.


%% @doc
%% @spec unregister_connection(inet:ip_address(), int()) -> ok
unregister_connection(Adress, Port) ->
    gen_server:call(?MODULE, {unregister_conn, Adress, Port}, 20000).

%% @doc
%% @spec register_connection(inet:ip_address(), int(), pid(), gen_tcp:socket()) -> ok | duplicate
register_connection(Adress, Port, Pid, Socket) ->
    gen_server:call(?MODULE, {register_conn, Adress, Port, Pid, Socket}, 20000).

%% @doc
%% @spec set_local_address(inet:ip_address(), int()) -> ok
set_local_address(Address, Port) ->
    gen_server:call(?MODULE, {set_local_address, Address, Port}, 20000).


%% @doc
%% @spec get_local_address_port() -> {inet:ip_address(),int()}
get_local_address_port() ->
    case ets:lookup(?MODULE, local_address_port) of
	[{local_address_port, Value}] ->
	    Value;
	[] ->
	    undefined
    end.

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    ets:new(?MODULE, [set, protected, named_table]),
    {ok, ok}. % empty state.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({send, Address, Port, Pid, Message}, _From, State) ->
    send(Address, Port, Pid, Message, State);

handle_call({unregister_conn, Address, Port}, _From, State) ->
    ets:delete(?MODULE, {Address, Port}),
    {reply, ok, State};

handle_call({register_conn, Address, Port, Pid, Socket}, _From, State) ->
    case ets:lookup(?MODULE, {Address, Port}) of
	[{{Address, Port}, _}] ->
	    {reply, duplicate, State};
	[] ->
	    ets:insert(?MODULE, {{Address, Port}, {Pid, Socket}}),
	    {reply, ok, State}
    end;

handle_call({set_local_address, Address, Port}, _From, State) ->
    ets:insert(?MODULE, {local_address_port, {Address,Port}}),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-ifdef(ASYNC).
send(Address, Port, Pid, Message, State) ->
    {DepAddr,DepPort} = get_local_address_port(),
    if
	DepAddr == undefined ->
	    open_sync_connection(Address, Port, Pid, Message, State);
	true ->
	    case ets:lookup(?MODULE, {Address, Port}) of
		[{{Address, Port}, {ConnPid, _Socket}}] ->
		    ConnPid ! {send, Pid, Message},
		    {reply, ok, State};
		[] ->
		    ConnPid = comm_connection:open_new_async(Address, Port,
							     DepAddr, DepPort),
		    ets:insert(?MODULE, {{Address, Port}, {ConnPid, undef}}),
		    ConnPid ! {send, Pid, Message},
		    {reply, ok, State}
	    end
    end.
-endif.

-ifdef(SYNC).
send(Address, Port, Pid, Message, State) ->
    case ets:lookup(?MODULE, {Address, Port}) of
	[{{Address, Port}, {_LPid, Socket}}] ->
	    comm_connection:send({Address, Port, Socket}, Pid, Message),
	    {reply, ok, State};
	[] ->
	    open_sync_connection(Address, Port, Pid, Message, State)
    end.
-endif.


open_sync_connection(Address, Port, Pid, Message, State) ->
    {DepAddr,DepPort} = get_local_address_port(),
    case comm_connection:open_new(Address, Port, DepAddr, DepPort) of
	{local_ip, MyIP, MyPort, MyPid, MySocket} ->
	    comm_connection:send({Address, Port, MySocket}, Pid, Message),
	    log:log(info,"[ CC ] this() == ~w", [{MyIP, MyPort}]),
						%		    set_local_address(t, {MyIP,MyPort}}),
						%		    register_connection(Address, Port, MyPid, MySocket),
	    ets:insert(?MODULE, {local_address_port, {MyIP,MyPort}}),
	    ets:insert(?MODULE, {{Address, Port}, {MyPid, MySocket}}),
	    {reply, ok, State};
	fail ->
						% drop message (remote node not reachable, failure detector will notice)
	    {reply, ok, State};
	{connection, LocalPid, NewSocket} ->
	    comm_connection:send({Address, Port, NewSocket}, Pid, Message),
	    ets:insert(?MODULE, {{Address, Port}, {LocalPid, NewSocket}}),
						%		    register_connection(Address, Port, LPid, NewSocket),
	    {reply, ok, State}
    end.
