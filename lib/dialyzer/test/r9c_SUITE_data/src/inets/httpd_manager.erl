%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: httpd_manager.erl,v 1.1 2008/12/17 09:53:34 mikpe Exp $

-module(httpd_manager).

-include("httpd.hrl").
-include("httpd_verbosity.hrl").

-behaviour(gen_server).

%% External API
-export([start/2, start/3, start_link/2, start_link/3, stop/1, restart/1]).

%% Internal API
-export([new_connection/1, done_connection/1]).

%% Module API
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
-export([verbosity/2, verbosity/3]).


-export([c/1]).

-record(state,{socket_type  = ip_comm,
	       config_file,
	       config_db    = null,
	       connections, %% Current request handlers
	       admin_state  = unblocked,
	       blocker_ref  = undefined,
	       blocking_tmr = undefined,
	       status       = []}).


c(Port) ->
    Ref = httpd_util:make_name("httpd",undefined,Port),
    gen_server:call(Ref, fake_close).


%%
%% External API
%%

start(ConfigFile, ConfigList) ->
    start(ConfigFile, ConfigList, []).

start(ConfigFile, ConfigList, Verbosity) ->
    Port = httpd_util:key1search(ConfigList,port,80),
    Addr = httpd_util:key1search(ConfigList,bind_address),
    Name = make_name(Addr,Port),
    ?LOG("start -> Name = ~p",[Name]),
    gen_server:start({local,Name},?MODULE,
		     [ConfigFile, ConfigList, Addr, Port, Verbosity],[]).

start_link(ConfigFile, ConfigList) ->
    start_link(ConfigFile, ConfigList, []).

start_link(ConfigFile, ConfigList, Verbosity) ->
    Port = httpd_util:key1search(ConfigList,port,80),
    Addr = httpd_util:key1search(ConfigList,bind_address),
    Name = make_name(Addr,Port),
    ?LOG("start_link -> Name = ~p",[Name]),
    gen_server:start_link({local, Name},?MODULE,
			  [ConfigFile, ConfigList, Addr, Port, Verbosity],[]).

%% stop

stop(ServerRef) ->
    gen_server:call(ServerRef, stop).

%% restart

restart(ServerRef) ->
    gen_server:call(ServerRef, restart).


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


verbosity(ServerRef,Verbosity) ->
    verbosity(ServerRef,all,Verbosity).

verbosity(ServerRef,all,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,all,Verbosity});
verbosity(ServerRef,manager,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,manager,Verbosity});
verbosity(ServerRef,request,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,request,Verbosity});
verbosity(ServerRef,acceptor,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,acceptor,Verbosity});
verbosity(ServerRef,security,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,security,Verbosity});
verbosity(ServerRef,auth,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,auth,Verbosity}).

%%
%% Internal API
%%


%% new_connection

new_connection(Manager) ->
    gen_server:call(Manager, {new_connection, self()}).

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
%% Module API. These functions are intended for use from modules only.
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

init([ConfigFile, ConfigList, Addr, Port, Verbosity]) ->
    process_flag(trap_exit, true),
    case (catch do_init(ConfigFile, ConfigList, Addr, Port, Verbosity)) of
	{error, Reason} ->
	    ?vlog("failed starting server: ~p", [Reason]),
	    {stop, Reason};
	{ok, State} ->
	    {ok, State}
    end.


do_init(ConfigFile, ConfigList, Addr, Port, Verbosity) ->
    put(sname,man),
    set_verbosity(Verbosity),
    ?vlog("starting",[]),
    ConfigDB   = do_initial_store(ConfigList),
    ?vtrace("config db: ~p", [ConfigDB]),
    SocketType = httpd_socket:config(ConfigDB),
    ?vtrace("socket type: ~p, now start acceptor", [SocketType]),
    case httpd_acceptor_sup:start_acceptor(SocketType, Addr, Port, ConfigDB) of
	{ok, Pid} ->
	    ?vtrace("acceptor started: ~p", [Pid]),
	    Status = [{max_conn,0}, {last_heavy_load,never},
		      {last_connection,never}],
	    State  = #state{socket_type = SocketType,
			    config_file = ConfigFile,
			    config_db   = ConfigDB,
			    connections = [],
			    status      = Status},
	    ?vdebug("started",[]),
	    {ok, State};
	Else ->
	    Else
    end.


do_initial_store(ConfigList) ->
    case httpd_conf:store(ConfigList) of
	{ok, ConfigDB} ->
	    ConfigDB;
	{error, Reason} ->
	    ?vinfo("failed storing configuration: ~p",[Reason]),
	    throw({error, Reason})
    end.



%% handle_call

handle_call(stop, _From, State) ->
    ?vlog("stop",[]),
    {stop, normal, ok, State};

handle_call({config_lookup, Query}, _From, State) ->
    ?vlog("config lookup: Query = ~p",[Query]),
    Res = httpd_util:lookup(State#state.config_db, Query),
    ?vdebug("config lookup result: ~p",[Res]),
    {reply, Res, State};

handle_call({config_multi_lookup, Query}, _From, State) ->
    ?vlog("multi config lookup: Query = ~p",[Query]),
    Res = httpd_util:multi_lookup(State#state.config_db, Query),
    ?vdebug("multi config lookup result: ~p",[Res]),
    {reply, Res, State};

handle_call({config_match, Query}, _From, State) ->
    ?vlog("config match: Query = ~p",[Query]),
    Res = ets:match_object(State#state.config_db, Query),
    ?vdebug("config match result: ~p",[Res]),
    {reply, Res, State};

handle_call(get_status, _From, State) ->
    ?vdebug("get status",[]),
    ManagerStatus  = manager_status(self()),
    %% AuthStatus     = auth_status(get(auth_server)),
    %% SecStatus      = sec_status(get(sec_server)),
    %% AccStatus      = sec_status(get(acceptor_server)),
    S1 = [{current_conn,length(State#state.connections)}|State#state.status]++
	[ManagerStatus],
    ?vtrace("status = ~p",[S1]),
    {reply,S1,State};

handle_call(is_busy, From, State) ->
    Reply = case get_ustate(State) of
		busy ->
		    true;
		_ ->
		    false
	  end,
    ?vlog("is busy: ~p",[Reply]),
    {reply,Reply,State};

handle_call(is_busy_or_blocked, From, State) ->
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
    ?vlog("is busy or blocked: ~p",[Reply]),
    {reply,Reply,State};

handle_call(is_blocked, From, State) ->
    Reply =
	case get_astate(State) of
	    unblocked ->
		false;
	    _ ->
		true
	  end,
    ?vlog("is blocked: ~p",[Reply]),
    {reply,Reply,State};

handle_call(get_admin_state, From, State) ->
    Reply = get_astate(State),
    ?vlog("admin state: ~p",[Reply]),
    {reply,Reply,State};

handle_call(get_usage_state, From, State) ->
    Reply = get_ustate(State),
    ?vlog("usage state: ~p",[Reply]),
    {reply,Reply,State};

handle_call({verbosity,Who,Verbosity}, From, State) ->
    V = ?vvalidate(Verbosity),
    ?vlog("~n   Set new verbosity to ~p for ~p",[V,Who]),
    Reply = set_verbosity(Who,V,State),
    {reply,Reply,State};

handle_call(restart, From, State) when State#state.admin_state == blocked ->
    ?vlog("restart",[]),
    case handle_restart(State) of
	{stop, Reply,S1} ->
	    {stop, Reply, S1};
	{_, Reply, S1} ->
	    {reply,Reply,S1}
    end;

handle_call(restart, From, State) ->
    ?vlog("restart(~p)",[State#state.admin_state]),
    {reply,{error,{invalid_admin_state,State#state.admin_state}},State};

handle_call(block, From, State) ->
    ?vlog("block(disturbing)",[]),
    {Reply,S1} = handle_block(State),
    {reply,Reply,S1};

handle_call(unblock, {From,_Tag}, State) ->
    ?vlog("unblock",[]),
    {Reply,S1} = handle_unblock(State,From),
    {reply, Reply, S1};

handle_call({new_connection, Pid}, From, State) ->
    ?vlog("~n   New connection (~p) when connection count = ~p",
	  [Pid,length(State#state.connections)]),
    {S, S1} = handle_new_connection(State, Pid),
    Reply = {S, get(request_handler_verbosity)},
    {reply, Reply, S1};

handle_call(Request, From, State) ->
    ?vinfo("~n   unknown request '~p' from ~p", [Request,From]),
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
    ?vlog("~n   Done connection (~p)", [Pid]),
    S1 = handle_done_connection(State, Pid),
    {noreply, S1};

handle_cast({block, disturbing, Timeout, From, Ref}, State) ->
    ?vlog("block(disturbing,~p)",[Timeout]),
    S1 = handle_block(State, Timeout, From, Ref),
    {noreply,S1};

handle_cast({block, non_disturbing, Timeout, From, Ref}, State) ->
    ?vlog("block(non-disturbing,~p)",[Timeout]),
    S1 = handle_nd_block(State, Timeout, From, Ref),
    {noreply,S1};

handle_cast(Message, State) ->
    ?vinfo("~n   received unknown message '~p'",[Message]),
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
    ?vlog("received block_timeout event",[]),
    S1 = handle_block_timeout(State,Method),
    {noreply, S1};

handle_info({'DOWN', Ref, process, _Object, Info}, State) ->
    ?vlog("~n   down message for ~p",[Ref]),
    S1 =
	case State#state.blocker_ref of
	    Ref ->
		handle_blocker_exit(State);
	    _ ->
		%% Not our blocker, so ignore
		State
	end,
    {noreply, S1};

handle_info({'EXIT', Pid, normal}, State) ->
    ?vdebug("~n   Normal exit message from ~p", [Pid]),
    {noreply, State};

handle_info({'EXIT', Pid, blocked}, S) ->
    ?vdebug("blocked exit signal from request handler (~p)", [Pid]),
    {noreply, S};

handle_info({'EXIT', Pid, Reason}, State) ->
    ?vlog("~n   Exit message from ~p for reason ~p",[Pid, Reason]),
    S1 = check_connections(State, Pid, Reason),
    {noreply, S1};

handle_info(Info, State) ->
    ?vinfo("~n   received unknown info '~p'",[Info]),
    String =
	lists:flatten(
	  io_lib:format("Unknown info "
			"~n   ~p"
			"~nto manager (~p)",
			[Info, self()])),
    report_error(State, String),
    {noreply, State}.


%% terminate

terminate(R, #state{config_db = Db}) ->
    ?vlog("Terminating for reason: ~n   ~p", [R]),
    httpd_conf:remove_all(Db),
    ok.


%% code_change({down,ToVsn}, State, Extra)
%%
%% NOTE:
%% Actually upgrade from 2.5.1 to 2.5.3 and downgrade from
%% 2.5.3 to 2.5.1 is done with an application restart, so
%% these function is actually never used. The reason for keeping
%% this stuff is only for future use.
%%
code_change({down,ToVsn},State,Extra) ->
    {ok,State};

%% code_change(FromVsn, State, Extra)
%%
code_change(FromVsn,State,Extra) ->
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
	    ?vlog("block complete",[]),
	    demonitor_blocker(State#state.blocker_ref),
	    {Tmr,From,Ref} = State#state.blocking_tmr,
	    ?vlog("(possibly) stop block timer",[]),
	    stop_block_tmr(Tmr),
	    ?vlog("and send the reply",[]),
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
	Connections1 ->
	    String =
		lists:flatten(
		  io_lib:format("request handler (~p) crashed:"
				"~n   ~p", [Pid, Reason])),
	    report_error(State, String),
	    State#state{connections = lists:delete(Pid, Connections)}
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

handle_new_connection(busy, unblocked, State, Handler) ->
    Status = update_heavy_load_status(State#state.status),
    {{reject, busy},
     State#state{status = Status}};

handle_new_connection(_UsageState, unblocked, State, Handler) ->
    Connections = State#state.connections,
    Status      = update_connection_status(State#state.status,
					   length(Connections)+1),
    link(Handler),
    {accept,
     State#state{connections = [Handler|Connections], status = Status}};

handle_new_connection(_UsageState, _AdminState, State, _Handler) ->
    {{reject, blocked},
     State}.


handle_done_connection(#state{admin_state = shutting_down,
			      connections = Connections} = State, Handler) ->
    unlink(Handler),
    case lists:delete(Handler, Connections) of
	[] -> % Ok, block complete
	    ?vlog("block complete",[]),
	    demonitor_blocker(State#state.blocker_ref),
	    {Tmr,From,Ref} = State#state.blocking_tmr,
	    ?vlog("(possibly) stop block timer",[]),
	    stop_block_tmr(Tmr),
	    ?vlog("and send the reply",[]),
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
    ?vtrace("handle_block(unblocked) -> kill all request handlers",[]),
%%    [exit(Pid,blocked) || Pid <- S#state.connections],
    [kill_handler(Pid) || Pid <- S#state.connections],
    {ok,S#state{connections = [], admin_state = blocked}};
handle_block(S,blocked) ->
    ?vtrace("handle_block(blocked) -> already blocked",[]),
    {ok,S};
handle_block(S,shutting_down) ->
    ?vtrace("handle_block(shutting_down) -> ongoing...",[]),
    {{error,shutting_down},S}.


kill_handler(Pid) ->
    ?vtrace("kill request handler: ~p",[Pid]),
    exit(Pid, blocked).
%%    exit(Pid, kill).

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
	    ?vdebug("do_block -> already in idle usage state",[]),
	    From ! {block_reply,ok,Ref},
	    S#state{admin_state = blocked};
	_ ->
	    %% Active or Busy usage state => go to shutting_down
	    ?vdebug("do_block -> active or busy usage state",[]),
	    %% Make sure we get to know if blocker dies...
	    ?vtrace("do_block -> create blocker monitor",[]),
	    MonitorRef = monitor_blocker(From),
	    ?vtrace("do_block -> (possibly) start block timer",[]),
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
	    ?vdebug("do_nd_block -> already in idle usage state",[]),
	    From ! {block_reply,ok,Ref},
	    S#state{admin_state = blocked};
	_ ->
	    %% Active or Busy usage state => go to shutting_down
	    ?vdebug("do_nd_block -> active or busy usage state",[]),
	    %% Make sure we get to know if blocker dies...
	    ?vtrace("do_nd_block -> create blocker monitor",[]),
	    MonitorRef = monitor_blocker(From),
	    ?vtrace("do_nd_block -> (possibly) start block timer",[]),
	    Tmr = {start_block_tmr(Timeout,non_disturbing),From,Ref},
	    S#state{admin_state = shutting_down,
		    blocker_ref = MonitorRef, blocking_tmr = Tmr}
    end.

handle_block_timeout(S,Method) ->
    %% Time to take this to the road...
    demonitor_blocker(S#state.blocker_ref),
    handle_block_timeout1(S,Method,S#state.blocking_tmr).

handle_block_timeout1(S,non_disturbing,{_,From,Ref}) ->
    ?vdebug("handle_block_timeout1(non-disturbing) -> send reply: timeout",[]),
    From ! {block_reply,{error,timeout},Ref},
    S#state{admin_state = unblocked,
	    blocker_ref = undefined, blocking_tmr = undefined};

handle_block_timeout1(S,disturbing,{_,From,Ref}) ->
    ?vdebug("handle_block_timeout1(disturbing) -> kill all connections",[]),
    [exit(Pid,blocked) || Pid <- S#state.connections],

    ?vdebug("handle_block_timeout1 -> send reply: ok",[]),
    From ! {block_reply,ok,Ref},
    S#state{admin_state = blocked,    connections = [],
	    blocker_ref = undefined, blocking_tmr = undefined};

handle_block_timeout1(S,Method,{_,From,Ref}) ->
    ?vinfo("received block timeout with unknown block method:"
	   "~n   Method:  ~p",[Method]),
    From ! {block_reply,{error,{unknown_block_method,Method}},Ref},
    S#state{admin_state = blocked,    connections = [],
	    blocker_ref = undefined, blocking_tmr = undefined};

handle_block_timeout1(S,Method,TmrInfo) ->
    ?vinfo("received block timeout with erroneous timer info:"
	   "~n   Method:  ~p"
	   "~n   TmrInfo: ~p",[Method,TmrInfo]),
    S#state{admin_state = unblocked,
	    blocker_ref = undefined, blocking_tmr = undefined}.

handle_unblock(S,FromA) ->
    handle_unblock(S,FromA,S#state.admin_state).

handle_unblock(S,_FromA,unblocked) ->
    {ok,S};
handle_unblock(S,FromA,_AdminState) ->
    ?vtrace("handle_unblock -> (possibly) stop block timer",[]),
    stop_block_tmr(S#state.blocking_tmr),
    case S#state.blocking_tmr of
	{Tmr,FromB,Ref} ->
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
    ?vtrace("handle_blocker_exit -> (possibly) stop block timer",[]),
    stop_block_tmr(Tmr),
    S#state{admin_state = unblocked,
	    blocker_ref = undefined, blocking_tmr = undefined}.



%% -------------------------------------------------------------------------
%% handle_restart
%%
%%
%%
%%
handle_restart(#state{config_file = undefined} = State) ->
    {continue, {error, undefined_config_file}, State};
handle_restart(#state{config_db = Db, config_file = ConfigFile} = State) ->
    ?vtrace("load new configuration",[]),
    {ok, Config} = httpd_conf:load(ConfigFile),
    ?vtrace("check for illegal changes (addr, port and socket-type)",[]),
    case (catch check_constant_values(Db, Config)) of
	ok ->
	    %% If something goes wrong between the remove
	    %% and the store where fu-ed
	    ?vtrace("remove old configuration, now hold you breath...",[]),
	    httpd_conf:remove_all(Db),
	    ?vtrace("store new configuration",[]),
	    case httpd_conf:store(Config) of
		{ok, NewConfigDB} ->
		    ?vlog("restart done, puh!",[]),
		    {continue, ok, State#state{config_db = NewConfigDB}};
		Error ->
		    ?vlog("failed store new config: ~n   ~p",[Error]),
		    {stop, Error, State}
	    end;
	Error ->
	    ?vlog("restart NOT performed due to:"
		  "~n   ~p",[Error]),
	    {continue, Error, State}
    end.


check_constant_values(Db, Config) ->
    %% Check port number
    ?vtrace("check_constant_values -> check port number",[]),
    Port = httpd_util:lookup(Db,port),
    case httpd_util:key1search(Config,port) of  %% MUST be equal
	Port ->
	    ok;
	OtherPort ->
	    throw({error,{port_number_changed,Port,OtherPort}})
    end,

    %% Check bind address
    ?vtrace("check_constant_values -> check bind address",[]),
    Addr = httpd_util:lookup(Db,bind_address),
    case httpd_util:key1search(Config,bind_address) of  %% MUST be equal
	Addr ->
	    ok;
	OtherAddr ->
	    throw({error,{addr_changed,Addr,OtherAddr}})
    end,

    %% Check socket type
    ?vtrace("check_constant_values -> check socket type",[]),
    SockType = httpd_util:lookup(Db, com_type),
    case httpd_util:key1search(Config, com_type) of  %% MUST be equal
	SockType ->
	    ok;
	OtherSockType ->
	    throw({error,{sock_type_changed,SockType,OtherSockType}})
    end,
    ?vtrace("check_constant_values -> done",[]),
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
monitor_blocker(Pid) when pid(Pid) ->
    case (catch erlang:monitor(process,Pid)) of
	MonitorRef ->
	    MonitorRef;
	{'EXIT',Reason} ->
	    undefined
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
	     {value,{max_conn,C1}} when ConnCount > C1 ->
		 lists:keyreplace(max_conn,1,Status,{max_conn,ConnCount});
	     {value,{max_conn,C2}} ->
		 Status;
	     false ->
		 [{max_conn,ConnCount}|Status]
	 end,
    update_status_with_time(S1,last_connection).

update_status_with_time(Status,Key) ->
    lists:keyreplace(Key,1,Status,{Key,universal_time()}).

universal_time() -> calendar:universal_time().


auth_status(P) when pid(P) ->
    Items = [status, message_queue_len, reductions,
	     heap_size, stack_size, current_function],
    {auth_status, process_status(P,Items,[])};
auth_status(_) ->
    {auth_status, undefined}.

sec_status(P) when pid(P) ->
    Items = [status, message_queue_len, reductions,
	     heap_size, stack_size, current_function],
    {security_status, process_status(P,Items,[])};
sec_status(_) ->
    {security_status, undefined}.

acceptor_status(P) when pid(P) ->
    Items = [status, message_queue_len, reductions,
	     heap_size, stack_size, current_function],
    {acceptor_status, process_status(P,Items,[])};
acceptor_status(_) ->
    {acceptor_status, undefined}.


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


set_verbosity(V) ->
    Units = [manager_verbosity,
	     acceptor_verbosity, request_handler_verbosity,
	     security_verbosity, auth_verbosity],
    case httpd_util:key1search(V, all) of
	undefined ->
	    set_verbosity(V, Units);
	Verbosity when atom(Verbosity) ->
	    V1 = [{Unit, Verbosity} || Unit <- Units],
	    set_verbosity(V1, Units)
    end.

set_verbosity(_V, []) ->
    ok;
set_verbosity(V, [manager_verbosity = Unit|Units]) ->
    Verbosity = httpd_util:key1search(V, Unit, ?default_verbosity),
    put(verbosity, ?vvalidate(Verbosity)),
    set_verbosity(V, Units);
set_verbosity(V, [Unit|Units]) ->
    Verbosity = httpd_util:key1search(V, Unit, ?default_verbosity),
    put(Unit, ?vvalidate(Verbosity)),
    set_verbosity(V, Units).


set_verbosity(manager,V,_S) ->
    put(verbosity,V);
set_verbosity(acceptor,V,_S) ->
    put(acceptor_verbosity,V);
set_verbosity(request,V,_S) ->
    put(request_handler_verbosity,V);
set_verbosity(security,V,S) ->
    OldVerbosity = put(security_verbosity,V),
    Addr = httpd_util:lookup(S#state.config_db, bind_address),
    Port = httpd_util:lookup(S#state.config_db, port),
    mod_security_server:verbosity(Addr,Port,V),
    OldVerbosity;
set_verbosity(auth,V,S) ->
    OldVerbosity = put(auth_verbosity,V),
    Addr = httpd_util:lookup(S#state.config_db, bind_address),
    Port = httpd_util:lookup(S#state.config_db, port),
    mod_auth_server:verbosity(Addr,Port,V),
    OldVerbosity;

set_verbosity(all,V,S) ->
    OldMv = put(verbosity,V),
    OldAv = put(acceptor_verbosity,V),
    OldRv = put(request_handler_verbosity,V),
    OldSv = put(security_verbosity,V),
    OldAv = put(auth_verbosity,V),
    Addr  = httpd_util:lookup(S#state.config_db, bind_address),
    Port  = httpd_util:lookup(S#state.config_db, port),
    mod_security_server:verbosity(Addr,Port,V),
    mod_auth_server:verbosity(Addr,Port,V),
    [{manager,OldMv}, {request,OldRv}, {security,OldSv}, {auth, OldAv}].


%%
call(ServerRef,Request) ->
    gen_server:call(ServerRef,Request).

cast(ServerRef,Message) ->
    gen_server:cast(ServerRef,Message).
