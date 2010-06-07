%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2010. All Rights Reserved.
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

-module(httpd_manager).

-include("httpd.hrl").

-behaviour(gen_server).

%% Application internal API
-export([start/2, start_link/2, start_link/3, start_link/4, stop/1, reload/2]).
-export([new_connection/1, done_connection/1]).
-export([config_lookup/2, config_lookup/3, 
	 config_multi_lookup/2, config_multi_lookup/3, 
	 config_match/2, config_match/3]).

%% gen_server exports
-export([init/1, 
	 handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2,
         code_change/3]).


%% Management exports
-export([block/2, block/3, unblock/1]).
-export([get_admin_state/1, get_usage_state/1]).
-export([is_busy/1,is_busy/2,is_busy_or_blocked/1,is_blocked/1]). %% ???????
-export([get_status/1, get_status/2]).

-export([c/1]).

-record(state,{socket_type  = ip_comm,
	       config_file,
	       config_db    = null,
	       connections, %% Current request handlers
	       admin_state  = unblocked,
	       blocker_ref  = undefined,
	       blocking_tmr = undefined,
	       status       = []}).


%%TODO: Clean up this module!

c(Port) ->
    Ref = httpd_util:make_name("httpd",undefined,Port),
    call(Ref, fake_close).

%%
%% External API
%%
%% Deprecated 
start(ConfigFile, ConfigList) ->
    Port = proplists:get_value(port,ConfigList,80),
    Addr = proplists:get_value(bind_address, ConfigList),
    Name = make_name(Addr,Port),
    gen_server:start({local,Name},?MODULE,
		     [ConfigFile, ConfigList, 15000, Addr, Port],[]).

%% Deprecated    
start_link(ConfigFile, ConfigList) ->
    start_link(ConfigFile, ConfigList, 15000).

start_link(ConfigFile, ConfigList, AcceptTimeout) ->
    Port = proplists:get_value(port, ConfigList, 80),
    Addr = proplists:get_value(bind_address, ConfigList),
    Name = make_name(Addr, Port),
    
    gen_server:start_link({local, Name},?MODULE,
			  [ConfigFile, ConfigList, AcceptTimeout, Addr, Port],[]).
    
start_link(ConfigFile, ConfigList, AcceptTimeout, ListenSocket) ->
    Port = proplists:get_value(port, ConfigList, 80),
    Addr = proplists:get_value(bind_address, ConfigList),
    Name = make_name(Addr, Port),
    
    gen_server:start_link({local, Name},?MODULE,
			  [ConfigFile, ConfigList, AcceptTimeout, Addr, 
			   Port, ListenSocket],[]).

stop(ServerRef) ->
    call(ServerRef, stop).

reload(ServerRef, Conf) ->
    call(ServerRef, {reload, Conf}).


%%%----------------------------------------------------------------

block(ServerRef, disturbing) ->
    call(ServerRef,block);

block(ServerRef, non_disturbing) ->
    do_block(ServerRef, non_disturbing, infinity).

block(ServerRef, Method, Timeout) ->
    do_block(ServerRef, Method, Timeout).


%% The reason for not using call here, is that the manager cannot
%% _wait_ for completion of the requests. It must be able to do
%% do other things at the same time as the blocking goes on.
do_block(ServerRef, Method, infinity) ->
    Ref = make_ref(),
    cast(ServerRef, {block, Method, infinity, self(), Ref}),
    receive
	{block_reply, Reply, Ref} ->
	    Reply
    end;
do_block(ServerRef,Method,Timeout) when Timeout > 0 ->
    Ref = make_ref(),
    cast(ServerRef,{block,Method,Timeout,self(),Ref}),
    receive
	{block_reply,Reply,Ref} ->
	    Reply
    end.


%%%----------------------------------------------------------------

%% unblock

unblock(ServerRef) ->
    call(ServerRef,unblock).

%% get admin/usage state

get_admin_state(ServerRef) ->
    call(ServerRef,get_admin_state).

get_usage_state(ServerRef) ->
    call(ServerRef,get_usage_state).


%% get_status

get_status(ServerRef) ->
    gen_server:call(ServerRef,get_status).

get_status(ServerRef,Timeout) ->
    gen_server:call(ServerRef,get_status,Timeout).

%%
%% Internal API
%%


%% new_connection

new_connection(Manager) ->
    gen_server:call(Manager, {new_connection, self()}, infinity).

%% done

done_connection(Manager) ->
    gen_server:cast(Manager, {done_connection, self()}).


%% is_busy(ServerRef) -> true | false
%% 
%% Tests if the server is (in usage state) busy, 
%% i.e. has rached the heavy load limit.
%% 

is_busy(ServerRef) ->
    gen_server:call(ServerRef,is_busy).
    
is_busy(ServerRef,Timeout) ->
    gen_server:call(ServerRef,is_busy,Timeout).


%% is_busy_or_blocked(ServerRef) -> busy | blocked | false
%% 
%% Tests if the server is busy (usage state), i.e. has rached,
%% the heavy load limit, or blocked (admin state) .
%% 

is_busy_or_blocked(ServerRef) ->
    gen_server:call(ServerRef,is_busy_or_blocked).
    

%% is_blocked(ServerRef) -> true | false
%% 
%% Tests if the server is blocked (admin state) .
%% 

is_blocked(ServerRef) ->
    gen_server:call(ServerRef,is_blocked).
    

%%
%% Module API. Theese functions are intended for use from modules only.
%%

config_lookup(Port, Query) ->
    config_lookup(undefined, Port, Query).
config_lookup(Addr, Port, Query) ->
    Name = httpd_util:make_name("httpd",Addr,Port),
    gen_server:call(whereis(Name), {config_lookup, Query}).

config_multi_lookup(Port, Query) ->
    config_multi_lookup(undefined,Port,Query).
config_multi_lookup(Addr,Port, Query) ->
    Name = httpd_util:make_name("httpd",Addr,Port),
    gen_server:call(whereis(Name), {config_multi_lookup, Query}).

config_match(Port, Pattern) ->
    config_match(undefined,Port,Pattern).
config_match(Addr, Port, Pattern) ->
    Name = httpd_util:make_name("httpd",Addr,Port),
    gen_server:call(whereis(Name), {config_match, Pattern}).


%%
%% Server call-back functions
%%

%% init

init([ConfigFile, ConfigList, AcceptTimeout, Addr, Port]) ->
    process_flag(trap_exit, true),
    case (catch do_init(ConfigFile, ConfigList, AcceptTimeout, Addr, Port)) of
	{error, Reason} ->
	    String = lists:flatten(
		       io_lib:format("Failed initiating web server: "
				     "~n~p"
				     "~n~p"
				     "~n", [ConfigFile, Reason])),
	    error_logger:error_report(String),
	    {stop, {error, Reason}};
	{ok, State} ->
	    {ok, State}
    end;
init([ConfigFile, ConfigList, AcceptTimeout, Addr, Port, ListenInfo]) ->
    process_flag(trap_exit, true),
    case (catch do_init(ConfigFile, ConfigList, AcceptTimeout, 
			Addr, Port, ListenInfo)) of
	{error, Reason} ->
	    String = lists:flatten(
		       io_lib:format("Failed initiating web server: "
				     "~n~p"
				     "~n~p"
				     "~n", [ConfigFile, Reason])),
	    error_logger:error_report(String),
	    {stop, {error, Reason}};
	{ok, State} ->
	    {ok, State}
    end.

do_init(ConfigFile, ConfigList, AcceptTimeout, Addr, Port) ->
    NewConfigFile = proplists:get_value(file, ConfigList, ConfigFile),
    ConfigDB      = do_initial_store(ConfigList),
    SocketType    = httpd_conf:lookup_socket_type(ConfigDB),
    case httpd_acceptor_sup:start_acceptor(SocketType, Addr,
					   Port, ConfigDB, AcceptTimeout) of
	{ok, _Pid} ->
	    Status = [{max_conn,        0}, 
		      {last_heavy_load, never}, 
		      {last_connection, never}],
	    State  = #state{socket_type = SocketType,
			    config_file = NewConfigFile,
			    config_db   = ConfigDB,
			    connections = [],
			    status      = Status},
	    {ok, State};
	Else ->
	    Else
    end.

do_init(ConfigFile, ConfigList, AcceptTimeout, Addr, Port, ListenInfo) ->
    NewConfigFile = proplists:get_value(file, ConfigList, ConfigFile),
    ConfigDB   = do_initial_store(ConfigList),
    SocketType = httpd_conf:lookup_socket_type(ConfigDB),
    case httpd_acceptor_sup:start_acceptor(SocketType, Addr,
					   Port, ConfigDB, 
					   AcceptTimeout, ListenInfo) of
	{ok, _Pid} ->
	    Status = [{max_conn,0}, {last_heavy_load,never}, 
		      {last_connection,never}],
	    State  = #state{socket_type = SocketType,
			    config_file = NewConfigFile,
			    config_db   = ConfigDB,
			    connections = [],
			    status      = Status},
	    {ok, State};
	Else ->
	    Else
    end.

do_initial_store(ConfigList) ->
    case httpd_conf:store(ConfigList) of
	{ok, ConfigDB} ->
	    ConfigDB;
	{error, Reason} ->
	    throw({error, Reason})
    end.

   

%% handle_call

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({config_lookup, Query}, _From, State) ->
    Res = httpd_util:lookup(State#state.config_db, Query),
    {reply, Res, State};

handle_call({config_multi_lookup, Query}, _From, State) ->
    Res = httpd_util:multi_lookup(State#state.config_db, Query),
    {reply, Res, State};

handle_call({config_match, Query}, _From, State) ->
    Res = ets:match_object(State#state.config_db, Query),
    {reply, Res, State};

handle_call(get_status, _From, State) ->
    ManagerStatus  = manager_status(self()),
    S1 = [{current_conn,length(State#state.connections)}|State#state.status]++
	[ManagerStatus],
    {reply,S1,State};

handle_call(is_busy, _From, State) ->
    Reply = case get_ustate(State) of
		busy ->
		    true;
		_ ->
		    false
	  end,
    {reply,Reply,State};

handle_call(is_busy_or_blocked, _From, State) ->
    Reply = 
	case get_astate(State) of
	    unblocked ->
		case get_ustate(State) of
		    busy ->
			busy;
		    _ ->
			false
		end;
	    _ ->
		blocked
	  end,
    {reply,Reply,State};

handle_call(is_blocked, _From, State) ->
    Reply = 
	case get_astate(State) of
	    unblocked ->
		false;
	    _ ->
		true
	  end,
    {reply,Reply,State};

handle_call(get_admin_state, _From, State) ->
    Reply = get_astate(State),
    {reply,Reply,State};

handle_call(get_usage_state, _From, State) ->
    Reply = get_ustate(State),
    {reply,Reply,State};

handle_call({reload, Conf}, _From, State) 
  when State#state.admin_state =:= blocked ->
    case handle_reload(Conf, State) of
	{stop, Reply,S1} ->
	    {stop, Reply, S1};
	{_, Reply, S1} ->
	    {reply,Reply,S1}
    end;

handle_call({reload, _}, _From, State) ->
    {reply,{error,{invalid_admin_state,State#state.admin_state}},State};

handle_call(block, _From, State) ->
    {Reply,S1} = handle_block(State),
    {reply,Reply,S1};

handle_call(unblock, {From,_Tag}, State) ->
    {Reply,S1} = handle_unblock(State,From),
    {reply, Reply, S1};

handle_call({new_connection, Pid}, _From, State) ->
    {Status, NewState} = handle_new_connection(State, Pid),
    {reply, Status, NewState};

handle_call(Request, From, State) ->
    String = 
	lists:flatten(
	  io_lib:format("Unknown request "
			"~n   ~p"
			"~nto manager (~p)"
			"~nfrom ~p",
			[Request, self(), From])),
    report_error(State,String),
    {reply, ok, State}.


%% handle_cast

handle_cast({done_connection, Pid}, State) ->
    S1 = handle_done_connection(State, Pid),
    {noreply, S1};

handle_cast({block, disturbing, Timeout, From, Ref}, State) ->
    S1 = handle_block(State, Timeout, From, Ref),
    {noreply,S1};

handle_cast({block, non_disturbing, Timeout, From, Ref}, State) ->
    S1 = handle_nd_block(State, Timeout, From, Ref),
    {noreply,S1};

handle_cast(Message, State) ->
    String = 
	lists:flatten(
	  io_lib:format("Unknown message "
			"~n   ~p"
			"~nto manager (~p)",
			[Message, self()])),
    report_error(State, String),
    {noreply, State}.

%% handle_info

handle_info({block_timeout, Method}, State) ->
    S1 = handle_block_timeout(State,Method),
    {noreply, S1};

handle_info({'DOWN', Ref, process, _Object, _Info}, State) ->
    S1 = 
	case State#state.blocker_ref of
	    Ref ->
		handle_blocker_exit(State);
	    _ ->
		%% Not our blocker, so ignore
		State
	end,
    {noreply, S1};

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};

handle_info({'EXIT', _, blocked}, S) ->
    {noreply, S};

handle_info({'EXIT', Pid, Reason}, State) ->
    S1 = check_connections(State, Pid, Reason),
    {noreply, S1};

handle_info(Info, State) ->
    String = 
	lists:flatten(
	  io_lib:format("Unknown info "
			"~n   ~p"
			"~nto manager (~p)",
			[Info, self()])),
    report_error(State, String),
    {noreply, State}.


%% terminate

terminate(_, #state{config_db = Db}) -> 
    httpd_conf:remove_all(Db),
    ok.


%% code_change({down,ToVsn}, State, Extra)
%% 

code_change({down,_ToVsn}, State, _Extra) ->
    {ok,State};

%% code_change(FromVsn, State, Extra)
%%
code_change(_FromVsn, State, _Extra) ->
    {ok,State}.



%% -------------------------------------------------------------------------
%% check_connection
%%
%%
%%
%%

check_connections(#state{connections = []} = State, _Pid, _Reason) ->
    State;
check_connections(#state{admin_state = shutting_down,
			 connections = Connections} = State, Pid, Reason) ->
    %% Could be a crashing request handler
    case lists:delete(Pid, Connections) of
	[] -> % Crashing request handler => block complete
	    String = 
		lists:flatten(
		  io_lib:format("request handler (~p) crashed:"
				"~n   ~p", [Pid, Reason])),
	    report_error(State, String),
	    demonitor_blocker(State#state.blocker_ref),
	    {Tmr,From,Ref} = State#state.blocking_tmr,
	    stop_block_tmr(Tmr),
	    From ! {block_reply,ok,Ref},
	    State#state{admin_state = blocked, connections = [],
			blocker_ref = undefined};
	Connections1 ->
	    State#state{connections = Connections1}
    end;
check_connections(#state{connections = Connections} = State, Pid, Reason) ->
    case lists:delete(Pid, Connections) of
	Connections -> % Not a request handler, so ignore
	    State;
        NewConnections ->
	    String = 
		lists:flatten(
		  io_lib:format("request handler (~p) crashed:"
				"~n   ~p", [Pid, Reason])),
	    report_error(State, String),
	    State#state{connections = NewConnections}
    end.


%% -------------------------------------------------------------------------
%% handle_[new | done]_connection
%%
%%
%%
%%

handle_new_connection(State, Handler) ->
    UsageState = get_ustate(State),
    AdminState = get_astate(State),
    handle_new_connection(UsageState, AdminState, State, Handler).

handle_new_connection(busy, unblocked, State, _Handler) ->
    Status = update_heavy_load_status(State#state.status),
    {{reject, busy}, 
     State#state{status = Status}};

handle_new_connection(_UsageState, unblocked, State, Handler) ->
    Connections = State#state.connections,
    Status      = update_connection_status(State#state.status, 
					   length(Connections)+1),
    link(Handler),
    {{ok, accept}, 
     State#state{connections = [Handler|Connections], status = Status}};

handle_new_connection(_UsageState, _AdminState, State, _Handler) ->
    {{reject, blocked}, 
     State}.

handle_done_connection(#state{admin_state = shutting_down,
			      connections = Connections} = State, Handler) ->
    unlink(Handler),
    case lists:delete(Handler, Connections) of
	[] -> % Ok, block complete
	    demonitor_blocker(State#state.blocker_ref),
	    {Tmr,From,Ref} = State#state.blocking_tmr,
	    stop_block_tmr(Tmr),
	    From ! {block_reply,ok,Ref},
	    State#state{admin_state = blocked, connections = [],
			blocker_ref = undefined};
	Connections1 ->
	    State#state{connections = Connections1}
    end;

handle_done_connection(#state{connections = Connections} = State, Handler) ->
    State#state{connections = lists:delete(Handler, Connections)}.
    
    
%% -------------------------------------------------------------------------
%% handle_block
%%
%%
%%
%%
handle_block(#state{admin_state = AdminState} = S) ->
    handle_block(S, AdminState).

handle_block(S,unblocked) ->
    %% Kill all connections
    [kill_handler(Pid) || Pid <- S#state.connections],
    {ok,S#state{connections = [], admin_state = blocked}};
handle_block(S,blocked) ->
    {ok,S};
handle_block(S,shutting_down) ->
    {{error,shutting_down},S}.
    

kill_handler(Pid) ->
    exit(Pid, blocked).

handle_block(S,Timeout,From,Ref) when Timeout >= 0 ->
    do_block(S,Timeout,From,Ref);

handle_block(S,Timeout,From,Ref) ->
    Reply = {error,{invalid_block_request,Timeout}},
    From ! {block_reply,Reply,Ref},
    S.

do_block(S,Timeout,From,Ref) ->
    case S#state.connections of
	[] ->
	    %% Already in idle usage state => go directly to blocked
	    From ! {block_reply,ok,Ref},
	    S#state{admin_state = blocked};
	_ ->
	    %% Active or Busy usage state => go to shutting_down
	    %% Make sure we get to know if blocker dies...
	    MonitorRef = monitor_blocker(From),
	    Tmr = {start_block_tmr(Timeout,disturbing),From,Ref},
	    S#state{admin_state = shutting_down, 
		    blocker_ref = MonitorRef, blocking_tmr = Tmr}
    end.

handle_nd_block(S,infinity,From,Ref) ->
    do_nd_block(S,infinity,From,Ref);

handle_nd_block(S,Timeout,From,Ref) when Timeout >= 0 ->
    do_nd_block(S,Timeout,From,Ref);

handle_nd_block(S,Timeout,From,Ref) ->
    Reply = {error,{invalid_block_request,Timeout}},
    From ! {block_reply,Reply,Ref},
    S.

do_nd_block(S,Timeout,From,Ref) ->
    case S#state.connections of
	[] ->
	    %% Already in idle usage state => go directly to blocked
	    From ! {block_reply,ok,Ref},
	    S#state{admin_state = blocked};
	_ ->
	    %% Active or Busy usage state => go to shutting_down
	    %% Make sure we get to know if blocker dies...
	    MonitorRef = monitor_blocker(From),
	    Tmr = {start_block_tmr(Timeout,non_disturbing),From,Ref},
	    S#state{admin_state = shutting_down, 
		    blocker_ref = MonitorRef, blocking_tmr = Tmr}
    end.

handle_block_timeout(S,Method) ->
    %% Time to take this to the road...
    demonitor_blocker(S#state.blocker_ref),
    handle_block_timeout1(S,Method,S#state.blocking_tmr).

handle_block_timeout1(S,non_disturbing,{_,From,Ref}) ->
    From ! {block_reply,{error,timeout},Ref},
    S#state{admin_state = unblocked, 
	    blocker_ref = undefined, blocking_tmr = undefined};

handle_block_timeout1(S,disturbing,{_,From,Ref}) ->
    [exit(Pid,blocked) || Pid <- S#state.connections],

    From ! {block_reply,ok,Ref},
    S#state{admin_state = blocked,    connections = [], 
	    blocker_ref = undefined, blocking_tmr = undefined};

handle_block_timeout1(S,Method,{_,From,Ref}) ->
    From ! {block_reply,{error,{unknown_block_method,Method}},Ref},
    S#state{admin_state = blocked,    connections = [], 
	    blocker_ref = undefined, blocking_tmr = undefined};

handle_block_timeout1(S, _Method, _TmrInfo) ->
    S#state{admin_state = unblocked,
	    blocker_ref = undefined, blocking_tmr = undefined}.

handle_unblock(S, FromA) ->
    handle_unblock(S, FromA, S#state.admin_state).

handle_unblock(S, _FromA, unblocked) ->
    {ok,S};
handle_unblock(S, FromA, _AdminState) ->
    stop_block_tmr(S#state.blocking_tmr),
    case S#state.blocking_tmr of
	{_Tmr,FromB,Ref} ->
	    %% Another process is trying to unblock
	    %% Inform the blocker
	    FromB ! {block_reply, {error,{unblocked,FromA}},Ref};
	_ ->
	    ok
    end,
    {ok,S#state{admin_state = unblocked, blocking_tmr = undefined}}.
    
%% The blocker died so we give up on the block.
handle_blocker_exit(S) ->
    {Tmr,_From,_Ref} = S#state.blocking_tmr,
    stop_block_tmr(Tmr),
    S#state{admin_state = unblocked,
	    blocker_ref = undefined, blocking_tmr = undefined}.
    


%% -------------------------------------------------------------------------
%% handle_reload
%%
%%
%%
%%
handle_reload(undefined, #state{config_file = undefined} = State) ->
    {continue, {error, undefined_config_file}, State};
handle_reload(undefined, #state{config_file = ConfigFile} = State) ->
    case load_config(ConfigFile) of
	{ok, Config} ->
	    do_reload(Config, State);
	{error, Reason} ->
	    error_logger:error_msg("Bad config file: ~p~n", [Reason]),
	    {continue, {error, Reason}, State}
    end;
handle_reload(Config, State) ->
    do_reload(Config, State).

load_config(ConfigFile) ->
    case httpd_conf:load(ConfigFile) of
	{ok, Config} ->
	    httpd_conf:validate_properties(Config);
	Error ->
	    Error
    end.

do_reload(Config, #state{config_db = Db} = State) ->
    case (catch check_constant_values(Db, Config)) of
	ok ->
	    %% If something goes wrong between the remove 
	    %% and the store where fu-ed
	    httpd_conf:remove_all(Db),
	    case httpd_conf:store(Config) of
		{ok, NewConfigDB} ->
		    {continue, ok, State#state{config_db = NewConfigDB}};
		Error ->
		    {stop, Error, State}
	    end;
	Error ->
	    {continue, Error, State}
    end.

check_constant_values(Db, Config) ->
    %% Check port number
    Port = httpd_util:lookup(Db,port),
    case proplists:get_value(port,Config) of  %% MUST be equal
	Port ->
	    ok;
	OtherPort ->
	    throw({error,{port_number_changed,Port,OtherPort}})
    end,

    %% Check bind address
    Addr = httpd_util:lookup(Db,bind_address),
    case proplists:get_value(bind_address, Config) of  %% MUST be equal
	Addr ->
	    ok;
	OtherAddr ->
	    throw({error,{addr_changed,Addr,OtherAddr}})
    end,

    %% Check socket type
    SockType = httpd_util:lookup(Db, socket_type),
    case proplists:get_value(socket_type, Config) of  %% MUST be equal
	SockType ->
	    ok;
	OtherSockType ->
	    throw({error,{sock_type_changed,SockType,OtherSockType}})
    end,
    ok.


%% get_ustate(State) -> idle | active | busy
%%
%% Retrieve the usage state of the HTTP server:
%%   0 active connection            -> idle
%%   max_clients active connections -> busy
%%   Otherwise                      -> active
%%
get_ustate(State) ->
    get_ustate(length(State#state.connections),State).

get_ustate(0,_State) ->
    idle;
get_ustate(ConnectionCnt,State) ->
    ConfigDB = State#state.config_db,
    case httpd_util:lookup(ConfigDB, max_clients, 150) of
	ConnectionCnt ->
	    busy;
	_ ->
	    active
    end.


get_astate(S) -> S#state.admin_state.


%% Timer handling functions
start_block_tmr(infinity,_) ->
    undefined;
start_block_tmr(T,M) ->
    erlang:send_after(T,self(),{block_timeout,M}).

stop_block_tmr(undefined) ->
    ok;
stop_block_tmr(Ref) ->
    erlang:cancel_timer(Ref).


%% Monitor blocker functions
monitor_blocker(Pid) when is_pid(Pid) ->
    case (catch erlang:monitor(process,Pid)) of
	{'EXIT', _Reason} ->
	    undefined;
	MonitorRef ->
	    MonitorRef
    end;
monitor_blocker(_) ->
    undefined.

demonitor_blocker(undefined) ->
    ok;
demonitor_blocker(Ref) ->
    (catch erlang:demonitor(Ref)).


%% Some status utility functions

update_heavy_load_status(Status) ->
    update_status_with_time(Status,last_heavy_load).

update_connection_status(Status,ConnCount) ->
    S1 = case lists:keysearch(max_conn,1,Status) of
	     {value, {max_conn, C1}} when ConnCount > C1 ->
		 lists:keyreplace(max_conn,1,Status,{max_conn,ConnCount});
	     {value, {max_conn, _C2}} ->
		 Status;
	     false ->
		 [{max_conn, ConnCount} | Status]
	 end,
    update_status_with_time(S1,last_connection).

update_status_with_time(Status,Key) ->
    lists:keyreplace(Key,1,Status,{Key,universal_time()}).

universal_time() -> calendar:universal_time().

manager_status(P) ->
    Items = [status, message_queue_len, reductions,
	     heap_size, stack_size],
    {manager_status, process_status(P,Items,[])}.


process_status(P,[],L) ->
    [{pid,P}|lists:reverse(L)];
process_status(P,[H|T],L) ->
    case (catch process_info(P,H)) of
	{H, Value} ->
	    process_status(P,T,[{H,Value}|L]);
	_ ->
	    process_status(P,T,[{H,undefined}|L])
    end.
	
make_name(Addr,Port) ->
    httpd_util:make_name("httpd",Addr,Port).


report_error(State,String) ->
    Cdb = State#state.config_db,
    error_logger:error_report(String),
    mod_log:report_error(Cdb,String),
    mod_disk_log:report_error(Cdb,String).
        
%%
call(ServerRef,Request) ->
    gen_server:call(ServerRef,Request).

cast(ServerRef,Message) ->
    gen_server:cast(ServerRef,Message).

