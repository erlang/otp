%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

-module(httpd_manager).

-include("httpd.hrl").

-behaviour(gen_server).

%% Application internal API
-export([start/2, start_link/2, start_link/3, start_link/4, 
	 stop/1, reload/2]).
-export([new_connection/1]).
-export([config_match/3, config_match/4]).
-export([block/2, block/3, unblock/1]).

%% gen_server exports
-export([init/1, 
	 handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2,
         code_change/3]).

-record(state,{socket_type  = ip_comm,
	       config_file,
	       config_db    = null,
	       connection_sup, 
	       admin_state  = unblocked,
	       blocker_ref  = undefined,
	       blocking_from = undefined,
	       shutdown_poller = undefined,
	       status       = []}).
%%%--------------------------------------------------------------------
%%% Application internal API
%%%--------------------------------------------------------------------

%% Deprecated 
start(ConfigFile, ConfigList) ->
    Port = proplists:get_value(port,ConfigList,80),
    Addr = proplists:get_value(bind_address, ConfigList),
    Profile = proplists:get_value(profile, ConfigList, default),
    Name = make_name(Addr, Port, Profile),
    gen_server:start({local,Name},?MODULE,
		     [ConfigFile, ConfigList, 15000, Addr, Port],[]).

%% Deprecated    
start_link(ConfigFile, ConfigList) ->
    start_link(ConfigFile, ConfigList, 15000).

start_link(ConfigFile, ConfigList, AcceptTimeout) ->
    Port = proplists:get_value(port, ConfigList, 80),
    Addr = proplists:get_value(bind_address, ConfigList),
    Profile = proplists:get_value(profile, ConfigList, default),
    Name = make_name(Addr, Port, Profile),
    
    gen_server:start_link({local, Name},?MODULE,
			  [ConfigFile, ConfigList, 
			   AcceptTimeout, Addr, Port],[]).
    
start_link(ConfigFile, ConfigList, AcceptTimeout, ListenSocket) ->
    Port = proplists:get_value(port, ConfigList, 80),
    Addr = proplists:get_value(bind_address, ConfigList),
    Profile = proplists:get_value(profile, ConfigList, default),
    Name = make_name(Addr, Port, Profile),
    
    gen_server:start_link({local, Name},?MODULE,
			  [ConfigFile, ConfigList, AcceptTimeout, Addr, 
			   Port, ListenSocket],[]).
stop(ServerRef) ->
    call(ServerRef, stop).

reload(ServerRef, Conf) ->
    call(ServerRef, {reload, Conf}).

block(ServerRef, Method) ->
    block(ServerRef, Method, infinity).

block(ServerRef, Method, Timeout) ->
    call(ServerRef, {block, self(), Method, Timeout}).

unblock(ServerRef) ->
    call(ServerRef,{unblock, self()}).

new_connection(Manager) ->
    call(Manager, {new_connection, self()}).

config_match(Port, Profile, Pattern) ->
    config_match(undefined,Port, Profile, Pattern).
config_match(Addr, Port, Profile, Pattern) ->
    Name = httpd_util:make_name("httpd",Addr,Port, Profile),
    call(whereis(Name), {config_match, Pattern}).

%%%--------------------------------------------------------------------
%%% gen_server callbacks functions
%%%--------------------------------------------------------------------
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

do_init(ConfigFile, ConfigList, _AcceptTimeout, Addr, Port) ->
    Sup = httpd_util:make_name("httpd_connection_sup", Addr, Port),
    NewConfigFile = proplists:get_value(file, ConfigList, ConfigFile),
    ConfigDB      = do_initial_store(ConfigList),
    SocketType    = httpd_conf:lookup_socket_type(ConfigDB),
    Status = [{max_conn,        0}, 
	      {last_heavy_load, never}, 
	      {last_connection, never}],
	    State  = #state{socket_type = SocketType,
			    config_file = NewConfigFile,
			    config_db   = ConfigDB,
			    connection_sup = Sup, 
			    status      = Status},
    {ok, State}.

do_init(ConfigFile, ConfigList, _AcceptTimeout, Addr, Port, _ListenInfo) ->
    Sup = httpd_util:make_name("httpd_connection_sup", Addr, Port),
    NewConfigFile = proplists:get_value(file, ConfigList, ConfigFile),
    ConfigDB   = do_initial_store(ConfigList),
    SocketType = httpd_conf:lookup_socket_type(ConfigDB),
    Status = [{max_conn,0}, {last_heavy_load,never}, 
	      {last_connection,never}],
	    State  = #state{socket_type = SocketType,
			    config_file = NewConfigFile,
			    config_db   = ConfigDB,
			    connection_sup = Sup,
			    status      = Status},
    {ok, State}.


do_initial_store(ConfigList) ->
    case httpd_conf:store(ConfigList) of
	{ok, ConfigDB} ->
	    ConfigDB;
	{error, Reason} ->
	    throw({error, Reason})
    end.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({config_match, Query}, _From, State) ->
    Res = ets:match_object(State#state.config_db, Query),
    {reply, Res, State};

handle_call({reload, Conf}, _From, #state{admin_state = blocked} = State) ->
    case handle_reload(Conf, State) of
	{stop, Reply,S1} ->
	    {stop, Reply, S1};
	{_, Reply, S1} ->
	    {reply,Reply,S1}
    end;

handle_call({reload, _}, _From, State) ->
    {reply,{error,{invalid_admin_state,State#state.admin_state}},State};

handle_call({block , Blocker, Mode, Timeout}, From, 
	    #state{admin_state = unblocked,
		   connection_sup = CSup} = State) ->
    Monitor = erlang:monitor(process, Blocker),
    case count_children(CSup) of
	0 ->
	   %% Already in idle usage state => go directly to blocked
	    {reply, ok, State#state{admin_state = blocked,
				    blocker_ref = {Blocker, Monitor},
				    blocking_from = From}};
	_ ->
	    handle_block(Mode, Timeout, 
			 State#state{blocker_ref = {Blocker, Monitor},
				     blocking_from = From})
    end;
handle_call({block , _, _, _}, _, State) ->
    {reply, {error, blocked}, State};

handle_call({unblock, Blocker}, _, #state{blocker_ref = {Blocker, Monitor},
					  admin_state = blocked} = State) ->
   
    erlang:demonitor(Monitor),
    {reply, ok, 
     State#state{admin_state = unblocked, blocker_ref = undefined}};

handle_call({unblock, _}, _, State) ->
    {reply, {error, only_blocker_may_unblock}, State};

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

handle_cast(Message, State) ->
    String = 
	lists:flatten(
	  io_lib:format("Unknown message "
			"~n   ~p"
			"~nto manager (~p)",
			[Message, self()])),
    report_error(State, String),
    {noreply, State}.

handle_info(connections_terminated, #state{admin_state = shutting_down,
					   blocking_from = From} = State) ->
    gen_server:reply(From, ok),
    {noreply, State#state{admin_state = blocked, blocking_from = undefined}};
handle_info(connections_terminated, State) ->
    {noreply, State};

handle_info({block_timeout, non_disturbing, Blocker}, 
	    #state{admin_state = shutting_down,
		   blocking_from = From,
		   blocker_ref = {_, Monitor} = Blocker} = State) ->
    erlang:demonitor(Monitor),		  
    gen_server:reply(From, {error, timeout}),
    {noreply, State#state{admin_state = unblocked, blocking_from = undefined,
			  blocker_ref = undefined}};
handle_info({block_timeout, disturbing, Blocker}, 
	    #state{admin_state = shutting_down,
		   blocking_from = From,
		   blocker_ref = Blocker,
		   connection_sup = Sup} = State) ->
    SupPid = whereis(Sup),
    shutdown_connections(SupPid),
    gen_server:reply(From, ok),
    {noreply, State#state{admin_state = blocked,
			  blocking_from = undefined}};
handle_info({block_timeout, _, _}, State) ->
    {noreply, State};	   

handle_info({'DOWN', _, process, Pid, _Info}, 
	    #state{admin_state = Admin,
		   blocker_ref = {Pid, Monitor}} = State) when 
      Admin =/= unblocked ->
    erlang:demonitor(Monitor),	
    {noreply, State#state{admin_state = unblocked,
			  blocking_from = undefined,
			  blocker_ref = undefined}};
handle_info({'DOWN', _, process, _, _}, State) ->
    {noreply, State};

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};

handle_info({'EXIT', _, shutdown}, State) ->
    {stop, shutdown, State};

handle_info(Info, State) ->
    String = 
	lists:flatten(
	  io_lib:format("Unknown info "
			"~n   ~p"
			"~nto manager (~p)",
			[Info, self()])),
    report_error(State, String),
    {noreply, State}.

terminate(_, #state{config_db = Db}) -> 
    httpd_conf:remove_all(Db),
    ok.

code_change({down,_ToVsn}, State, _Extra) ->
    {ok,State};

code_change(_FromVsn, State, _Extra) ->
    {ok,State}.

%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------
handle_new_connection(#state{admin_state = AdminState} = State, Handler) ->
    UsageState = get_ustate(State),
    handle_new_connection(UsageState, AdminState, State, Handler).

handle_new_connection(_UsageState, unblocked, 
		      #state{config_db = Db, connection_sup = CSup} = 
			  State, _) ->
    Max = httpd_util:lookup(Db, max_clients),
    case count_children(CSup) of
	Count when Count =< Max ->
	    {{ok, accept}, State};
	_ ->
	    {{reject, busy}, State}
    end;

handle_new_connection(_UsageState, _AdminState, State, _Handler) ->
    {{reject, blocked}, State}.

handle_block(disturbing, infinity, 
	     #state{connection_sup = CSup,
		    blocking_from = From} = State) ->
    SupPid = whereis(CSup),
    shutdown_connections(SupPid),
    gen_server:reply(From, ok),
    {noreply, State#state{admin_state = blocked,
			  blocking_from = undefined}};
handle_block(disturbing, Timeout, #state{connection_sup = CSup, blocker_ref = Blocker} = State) ->
    Manager = self(),
    spawn_link(fun() -> wait_for_shutdown(CSup, Manager) end),
    erlang:send_after(Timeout, self(), {block_timeout, disturbing, Blocker}),
    {noreply, State#state{admin_state = shutting_down}};

handle_block(non_disturbing, infinity, 
	     #state{connection_sup = CSup} = State) ->
    Manager = self(),
    spawn_link(fun() -> wait_for_shutdown(CSup, Manager) end),
    {noreply, State#state{admin_state = shutting_down}};

handle_block(non_disturbing, Timeout,  
	     #state{connection_sup = CSup, blocker_ref = Blocker} = State) ->
    Manager = self(),
    spawn_link(fun() -> wait_for_shutdown(CSup, Manager) end),
    erlang:send_after(Timeout, self(), {block_timeout, non_disturbing, Blocker}),
    {noreply, State#state{admin_state = shutting_down}}.
    
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
    get_ustate(count_children(State#state.connection_sup),State).

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

make_name(Addr, Port, Profile) ->
    httpd_util:make_name("httpd", Addr, Port, Profile).


report_error(State,String) ->
    Cdb = State#state.config_db,
    error_logger:error_report(String),
    mod_log:report_error(Cdb,String),
    mod_disk_log:report_error(Cdb,String).
        
call(ServerRef, Request) ->
    try gen_server:call(ServerRef, Request, infinity) 
    catch
	exit:_ ->
	    {error, closed} 
    end.

count_children(Sup) ->
    Children = supervisor:count_children(whereis(Sup)),
    proplists:get_value(workers, Children).

shutdown_connections(Sup) ->
    Children = [Child || {_,Child,_,_} <- supervisor:which_children(Sup)],
    lists:foreach(fun(Pid) -> exit(Pid, kill) end,
		  Children).

wait_for_shutdown(CSup, Manager) ->	      
    case count_children(CSup) of
	0 ->
	    Manager ! connections_terminated;
	_ ->
	    receive 
	    after 500 ->
		    ok
	    end,
	    wait_for_shutdown(CSup, Manager)
    end.	

