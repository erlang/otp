%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
-module(snmpa_target_cache).

-behaviour(gen_server).

%% External exports
-export([start_link/2, stop/0, verbosity/1]).

-export([
	 invalidate/0, % invalidate/1, invalidate/2,
	 targets/1, targets/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-ifdef(snmp_qc).
-export([
	 lock/1,
	 unlock/0,
	 upgrade_lock/0,
	 downgrade_lock/0
	]).
-endif.

-include("snmpa_internal.hrl").
-include("snmp_debug.hrl").
-include("snmp_verbosity.hrl").


-record(state,
        {
          active_count = 0,
          writer       = false, % Active or waiting write-lock
          waiting      = []     % Waiting lockers
	 }
       ).
-record(locker, {pid, from, mon_ref, type, state}).


-define(SERVER,     ?MODULE).
-define(CACHE,      ?MODULE).
-define(LOCKER_TAB, snmpa_target_cache_locker).


-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.

-ifdef(snmp_debug).
-define(GS_START_LINK(Prio, Opts),
        gen_server:start_link({local, ?SERVER}, ?MODULE,
                              [Prio, Opts], [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Prio, Opts),
        gen_server:start_link({local, ?SERVER}, ?MODULE,
                              [Prio, Opts], [])).
-endif.


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(Prio, Opts) -> 
    ?d("start_link -> entry with"
       "~n   Prio: ~p"
       "~n   Opts: ~p", [Prio, Opts]),
    ?GS_START_LINK(Prio, Opts).


stop() ->
    call(stop).


verbosity(V) ->
    call({verbosity, V}).


%% Targets   -> notify_targets()
%% notify_targets() -> [notify_target()]
%% notify_target()  -> {NotifyName, target()}
%% target() -> {DestAddr, TargetName, TargetParams, NotifyType}

targets(TargetsFun) when is_function(TargetsFun) ->
    Pat = {{'_', '$1'}, '$2'},
    get_targets(Pat, TargetsFun).

targets(TargetsFun, NotifyName) when is_function(TargetsFun) ->
    Pat = {{NotifyName, '$1'}, '$2'},
    get_targets(Pat, TargetsFun).

get_targets(Pat, TargetsFun) ->
    lock(read), % Get a read lock
    Targets = 
	case ets:lookup(?CACHE, state) of
	    [{state, invalid}] ->
		upgrade_lock(), % Upgrade to write lock
		%% Make sure it's still invalid
		case ets:lookup(?CACHE, state) of
		    [{state, invalid}] ->
			insert_all( TargetsFun() ),
			ets:insert(?CACHE, {state, valid});
		    _ ->
			ok % This means that someone got there before us
		end,
		downgrade_lock(), % Downgrade to read lock
		get_targets(Pat);
	    [{state, valid}] ->
		get_targets(Pat)
	end,
    unlock(), % Release the read lock
    Targets.

get_targets(Pat) ->
    NotifyTargets = ets:match(?CACHE, Pat),
    [{DestAddr, TargetName, TargetParams, NotifyType} || 
	[TargetName, {DestAddr, TargetParams, NotifyType}] <- 
	    NotifyTargets].

invalidate() ->
    lock(write),
    case ets:lookup(?CACHE, state) of
	[{state, invalid}] ->
	    ok;
	[{state, valid}] ->
	    delete_all(),
	    ets:insert(?CACHE, {state, invalid})
    end,
    unlock(),
    ok.


%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Prio, Opts]) ->
    case (catch do_init(Prio, Opts)) of
        {ok, State} ->
            ?vdebug("started",[]),
            {ok, State};
        {error, Reason} ->
            config_err("failed starting target-cache server: ~n~p", [Reason]),
            {stop, {error, Reason}};
        Error ->
            config_err("failed starting target-cache server: ~n~p", [Error]),
            {stop, {error, Error}}
    end.


do_init(Prio, Opts) ->
    process_flag(priority, Prio),
    process_flag(trap_exit, true),
    put(sname, tcs),
    put(verbosity, get_opt(verbosity, Opts, ?default_verbosity)),
    ?vlog("starting",[]),
    ets:new(?CACHE,      [set, named_table, public]),
    ets:insert(?CACHE,   {state, invalid}),
    ets:new(?LOCKER_TAB, [set, named_table, {keypos, #locker.pid}]),
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------

%% 
%% (1) As long as there are no _waiting_ or active write locks, 
%%     read-locks will allways be granted
%% (2) When there are no active readers, write-locks will be
%%     granted. 
%% (3) When there are active readers (clients with read-locks),
%%     a write-lock will have to wait for _all_ the read-locks
%%     to be released.
%% (4) If there is a waiting write-lock, all subsequent lock-
%%     requests will have to wait.
%% (5) If there is an active write-lock, all subsequent lock-
%%     requests will have to wait.
%% 


%% (1) No write_lock active or waiting
handle_call({lock, read = Type, infinity}, {Pid, _} = From, 
	    #state{active_count = Cnt, writer = false} = State) ->
    ?vlog("lock(read, infinity) -> "
	  "entry when no waiting or active writer with"
	  "~n   Pid: ~p"
	  "~n   Cnt: ~p", [Pid, Cnt]),
    MonRef = erlang:monitor(process, Pid),
    Locker = #locker{pid     = Pid, 
                     from    = From,
		     mon_ref = MonRef, 
		     type    = Type, 
		     state   = active},
    ets:insert(?LOCKER_TAB, Locker),
%%     ?vtrace("lock(read, infinity) -> done when"
%% 	    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
    {reply, ok, State#state{active_count = inc(Cnt)}};

%% (4,5) There is waiting or active write locks
handle_call({lock, read = Type, infinity}, {Pid, _} = From, State) ->
    ?vlog("lock(read, infinity) -> "
	  "entry when active or waiting write locks with"
	  "~n   Pid: ~p", [Pid]),
    MonRef = erlang:monitor(process, Pid),
    Locker = #locker{pid     = Pid, 
		     from    = From, 
		     mon_ref = MonRef, 
		     type    = Type, 
		     state   = waiting},
    ets:insert(?LOCKER_TAB, Locker),
    Waiting = lists:append(State#state.waiting, [Pid]),
%%     ?vtrace("lock(read, infinity) -> done when"
%% 	    "~n   Waiting: ~p"
%% 	    "~n   Lockers: ~p", [Waiting, ets:tab2list(?LOCKER_TAB)]),
    {noreply, State#state{waiting = Waiting}};

%% (2) No active locks
%% Since there are no active lockers, that also means that 
%% there is no lockers waiting.
handle_call({lock, write = Type, infinity}, {Pid, _} = From, 
	    #state{active_count = 0, writer = false} = State) ->
    ?vlog("lock(write, infinity) -> "
	  "entry when no active lockers with"
	  "~n   Pid: ~p", [Pid]),
    MonRef = erlang:monitor(process, Pid),
    Locker = #locker{pid     = Pid, 
                     from    = From,
		     mon_ref = MonRef, 
		     type    = Type,
		     state   = active},
    ets:insert(?LOCKER_TAB, Locker),
%%     ?vtrace("lock(write, infinity) -> done when"
%% 	    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
    {reply, ok, State#state{active_count = 1, writer = true}};

%% (3) No waiting or active writers, but at least one active reader
handle_call({lock, write = Type, infinity}, {Pid, _} = From, 
	    #state{writer = false} = State) ->
    ?vlog("lock(write, infinity) -> "
	  "entry when active lockers with"
	  "~n   Pid: ~p", [Pid]),
    MonRef = erlang:monitor(process, Pid),
    Locker = #locker{pid     = Pid, 
		     from    = From, 
		     mon_ref = MonRef, 
		     type    = Type,
		     state   = waiting},
    ets:insert(?LOCKER_TAB, Locker),
    Waiting = lists:append(State#state.waiting, [Pid]),
%%     ?vtrace("lock(write, infinity) -> done when"
%% 	    "~n   Waiting: ~p"
%% 	    "~n   Lockers: ~p", [Waiting, ets:tab2list(?LOCKER_TAB)]),
    {noreply, State#state{waiting = Waiting, writer = true}};

handle_call({lock, write = Type, infinity}, {Pid, _} = From, 
	    #state{writer = true} = State) ->
    ?vlog("lock(write, infinity) -> entry with"
	  "~n   Pid: ~p", [Pid]),
    MonRef = erlang:monitor(process, Pid),
    Locker = #locker{pid     = Pid, 
		     from    = From, 
		     mon_ref = MonRef, 
		     type    = Type,
		     state   = waiting},
    ets:insert(?LOCKER_TAB, Locker),
    Waiting = lists:append(State#state.waiting, [Pid]),
%%     ?vtrace("lock(write, infinity) -> done when"
%% 	    "~n   Waiting: ~p"
%% 	    "~n   Lockers: ~p", [Waiting, ets:tab2list(?LOCKER_TAB)]),
    {noreply, State#state{waiting = Waiting}};

handle_call({verbosity, Verbosity}, _From, State) ->
    ?vlog("verbosity: ~p -> ~p", [get(verbosity), Verbosity]),
    Old = put(verbosity, ?vvalidate(Verbosity)),
    {reply, Old, State};

%% If there are no more active read'ers, and no waiting, 
%% then set to writer and reply now
handle_call({upgrade_lock, Pid}, _From, 
	    #state{active_count = 1, waiting = []} = State) ->
    ?vlog("upgrade_lock -> "
	  "entry when one active locker and no waiting with"
	  "~n   Pid: ~p", [Pid]),
    case ets:lookup(?LOCKER_TAB, Pid) of
	[#locker{type = read} = Locker] ->
	    ets:insert(?LOCKER_TAB, Locker#locker{type = write}),
%% 	    ?vtrace("upgrade_lock -> done when"
%% 		    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
	    {reply, ok, State#state{writer = true}};

	[#locker{type = write}] ->
	    {reply, ok, State}
    end;

%% If there are no more active read'ers, and no waiting, 
%% then set to writer and reply now
handle_call({upgrade_lock, Pid}, {Pid, _} = From, 
	    #state{active_count = 1, waiting = Waiting} = State) ->
    ?vlog("upgrade_lock -> "
	  "entry when one active locker with"
	  "~n   Pid:     ~p"
	  "~n   Waiting: ~p", [Pid, Waiting]),
    case ets:lookup(?LOCKER_TAB, Pid) of
	[#locker{type = read} = Locker] ->
	    case active_waiting_writer(Waiting) of
		{true, StillWaiting} ->
		    ?vtrace("upgrade_lock -> activated when"
			    "~n   StillWaiting: ~p", [StillWaiting]),
		    ets:insert(?LOCKER_TAB, Locker#locker{from  = From, 
							  type  = write,
							  state = waiting}),
%% 		    ?vtrace("upgrade_lock -> done when"
%% 			    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
		    {noreply, State#state{waiting = StillWaiting ++ [Pid]}};
		{false, []} ->
		    ?vtrace("upgrade_lock -> none activated, "
			    "so we can let the upgrader in", []),
		    ets:insert(?LOCKER_TAB, Locker#locker{type = write}),
%% 		    ?vtrace("upgrade_lock -> done when"
%% 			    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
		    {reply, ok, State#state{writer  = true,
					    waiting = []}}
	    end;

	[#locker{type = write}] ->
	    {reply, ok, State};

	_ ->
	    {reply, {error, not_found}, State}
    end;

%% There are active and waiting locker's
handle_call({upgrade_lock, Pid}, {Pid, _} = From, 
	    #state{active_count = Cnt, waiting = Waiting} = State) ->
    ?vlog("upgrade_lock -> entry with"
	  "~n   Pid:     ~p"
	  "~n   Waiting: ~p", [Pid, Waiting]),
    case ets:lookup(?LOCKER_TAB, Pid) of
	[#locker{type = read} = Locker] ->
	    ets:insert(?LOCKER_TAB, Locker#locker{from  = From, 
						  type  = write,
						  state = waiting}),
%% 	    ?vtrace("upgrade_lock -> done when"
%% 		    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
	    {noreply, State#state{active_count = dec(Cnt),
				  waiting      = Waiting ++ [Pid]}};

	[#locker{type = write}] ->
	    {reply, ok, State};

	_ ->
	    {reply, {error, not_found}, State}
    end;


handle_call(stop, _From, State) ->
    ?vlog("stop",[]),
    {stop, normal, stopped, State};

handle_call(Req, _From, State) ->
    warning_msg("received unknown request: ~n~p", [Req]),
    Reply = {error, {unknown, Req}}, 
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({unlock, Pid}, 
	    #state{active_count = Cnt, waiting = []} = State) ->
    ?vlog("unlock -> entry when no waiting with"
	  "~n   Pid: ~p"
	  "~n   Cnt: ~p", [Pid, Cnt]),
    case ets:lookup(?LOCKER_TAB, Pid) of
	[#locker{mon_ref = MonRef, type = read}] ->
	    ?vdebug("unlock -> found read locker"
		    "~n   MonRef: ~p", [MonRef]),
	    erlang:demonitor(MonRef, [flush]),
	    ets:delete(?LOCKER_TAB, Pid),
%% 	    ?vtrace("unlock -> done when"
%% 		    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
	    {noreply, State#state{active_count = dec(Cnt)}};
	[#locker{mon_ref = MonRef, type = write}] ->
	    ?vdebug("unlock -> found write locker"
		    "~n   MonRef: ~p", [MonRef]),
	    erlang:demonitor(MonRef, [flush]),
	    ets:delete(?LOCKER_TAB, Pid),
%% 	    ?vtrace("unlock -> done when"
%% 		    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
	    {noreply, State#state{active_count = dec(Cnt), writer = false}};
	_ ->
	    {noreply, State}
    end;

handle_cast({unlock, Pid}, 
	    #state{active_count = Cnt, waiting = Waiting} = State) ->
    ?vlog("unlock -> entry when waiting with"
	  "~n   Pid: ~p"
	  "~n   Cnt: ~p", [Pid, Cnt]),
    case ets:lookup(?LOCKER_TAB, Pid) of
	%% Last active reader: Time to let the waiting in
	%% The first of the waiting _has_ to be a write-lock
	%% (read-locks will only be set waiting if there is 
	%% a waiting or active write).
	[#locker{mon_ref = MonRef, type = read}] when (Cnt == 1) ->
	    ?vdebug("unlock -> found read locker"
		    "~n   MonRef: ~p", [MonRef]),
	    erlang:demonitor(MonRef, [flush]),
	    ets:delete(?LOCKER_TAB, Pid),
	    case active_waiting_writer(Waiting) of
		{true, StillWaiting} ->
		    ?vtrace("unlock -> activated when"
			    "~n   StillWaiting: ~p", [StillWaiting]),
%% 		    ?vtrace("unlock -> done when"
%% 			    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
		    {noreply, State#state{active_count = 1, 
					  writer       = true,
					  waiting      = StillWaiting}};
		{false, []} ->
		    ?vtrace("unlock -> none activated", []),
%% 		    ?vtrace("unlock -> done when"
%% 			    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
		    {noreply, State#state{active_count = 0, 
					  writer       = false,
					  waiting      = []}}
	    end;

	[#locker{mon_ref = MonRef, type = read}] ->
	    ?vdebug("unlock -> found read locker"
		    "~n   MonRef: ~p", [MonRef]),
	    erlang:demonitor(MonRef, [flush]),
	    ets:delete(?LOCKER_TAB, Pid),
%% 	    ?vtrace("unlock -> done when"
%% 		    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
	    {noreply, State#state{active_count = dec(Cnt)}};

	[#locker{mon_ref = MonRef, type = write}] ->
	    %% Release the hord (maybe)
	    ?vdebug("unlock -> found write locker"
		    "~n   MonRef: ~p", [MonRef]),
	    erlang:demonitor(MonRef, [flush]),
	    ets:delete(?LOCKER_TAB, Pid),
	    {Active, StillWaiting, Writer} = 
		activate_waiting_readers_or_maybe_writer(Waiting),
	    ?vtrace("unlock -> new reader(s) or maybe writer activated:"
		    "~n   Active:       ~p"
		    "~n   StillWaiting: ~p"
		    "~n   Writer:       ~p", [Active, StillWaiting, Writer]),
%% 	    ?vtrace("unlock -> done when"
%% 		    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
	    {noreply, State#state{active_count = Active,
				  writer       = Writer, 
				  waiting      = StillWaiting}};

	%% If we have no active lockers, this may be a bug and therefor
	%% see if we can activate some of the waiting
	_ when (State#state.active_count == 0) ->
	    ?vdebug("unlock -> could not find locker", []),
	    {Active, StillWaiting, Writer} = 
		activate_waiting_readers_or_maybe_writer(Waiting),
	    ?vtrace("unlock -> new reader(s) or maybe writer activated:"
		    "~n   Active:       ~p"
		    "~n   StillWaiting: ~p"
		    "~n   Writer:       ~p", [Active, StillWaiting, Writer]),
%% 	    ?vtrace("unlock -> done when"
%% 		    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
	    {noreply, 
	     State#state{active_count = Active,
			 writer       = Writer, 
			 waiting      = StillWaiting}};

	_ ->
	    {noreply, State}
    end;


handle_cast({downgrade_lock, Pid}, #state{waiting = Waiting} = State) ->
    ?vlog("downgrade_lock -> entry when waiting with"
	  "~n   Pid: ~p", [Pid]),
    case ets:lookup(?LOCKER_TAB, Pid) of
	[#locker{type = read}] ->
	    {noreply, State};

	[#locker{type = write} = Locker] ->
	    %% We need to check if this is the only write(r),
	    %% in that case we must update the writer field
	    ets:insert(?LOCKER_TAB, Locker#locker{type = read}),
	    {Cnt, NewWaiting} = activate_waiting_readers(Waiting),
	    ?vtrace("downgrade_lock -> entry when waiting with"
		    "~n   Cnt:        ~p"
		    "~n   NewWaiting: ~p", [Cnt, NewWaiting]),
%% 	    ?vtrace("downgrade_lock -> done when"
%% 		    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
	    {noreply, State#state{active_count = Cnt, 
				  waiting      = NewWaiting,
				  writer       = is_writer(NewWaiting)}}
    end;


handle_cast(Msg, State) ->
    warning_msg("received unknown message: ~n~p", [Msg]),
    {noreply, State}.




%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%% This must be a glitch
handle_info({'DOWN', _MonRef, process, Pid, Reason}, 
	    #state{active_count = 0, waiting = []} = State) ->
    ?vlog("received DOWN message from ~p when no active and no waiting"
	  "~n   exited for reason: ~n~p", [Pid, Reason]),
    {noreply, State};

handle_info({'DOWN', _MonRef, process, Pid, Reason}, 
	    #state{active_count = Cnt, waiting = []} = State) ->
    ?vlog("received DOWN message from ~p when active but no waiting"
	  "~n   exited for reason: ~n~p", [Pid, Reason]),
    case handle_maybe_active_down(Cnt, Pid) of
	{NewCnt, write} ->
%% 	    ?vtrace("DOWN -> done when"
%% 		    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
	    {noreply, State#state{active_count = NewCnt, writer = false}};
	{NewCnt, read} ->
%% 	    ?vtrace("DOWN -> done when"
%% 		    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
	    {noreply, State#state{active_count = NewCnt}}
    end;

handle_info({'DOWN', _MonRef, process, Pid, Reason}, State) ->
    ?vlog("received DOWN message from ~p"
	  "~n   exited for reason: ~n~p", [Pid, Reason]),
    NewState = handle_maybe_active_or_waiting_down(Pid, State),
%%     ?vtrace("DOWN -> done when"
%% 	    "~n   Lockers: ~p", [ets:tab2list(?LOCKER_TAB)]),
    {noreply, NewState};

handle_info({'EXIT', Pid, Reason}, S) ->
    %% The only other process we should be linked to is
    %% our supervisor, so die...
    {stop, {received_exit, Pid, Reason}, S};

handle_info(Info, State) ->
    warning_msg("received unknown info: ~n~p", [Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ?vlog("terminate ->"
	  "~n   Reason: ~p"
	  "~n   State:  ~p", [Reason, State]),
    ets:delete(?CACHE),
    ets:delete(?LOCKER_TAB),
    ok.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% Locks are initially exclusive which means that it is possible
%% to both read _and_ write. After a downgrade, it is only possible 
%% to read. But since, by then, the process already has a lock, it
%% can just go ahead and read.

lock(Type) ->
    call({lock, Type, infinity}).

%% Upgrade from read to lock write
upgrade_lock() ->
    call({upgrade_lock, self()}).

%% Downgrade from write to read lock
downgrade_lock() ->
    cast({downgrade_lock, self()}).

unlock() ->
    cast({unlock, self()}).


insert_all(Targets) ->
    Fun = fun({NotifyName, Data}) -> insert(NotifyName, Data) end,
    lists:foreach(Fun, Targets).

insert(NotifyName, {DestAddr, TargetName, TargetParams, NotifyType}) ->
    Key  = {NotifyName, TargetName},
    Data = {DestAddr, TargetParams, NotifyType},
    ets:insert(?CACHE, {Key, Data}).

delete_all() ->
    ets:delete_all_objects(?CACHE).



%%----------------------------------------------------------

%% This function is called when we have active but no waiting
%% lockers. So, if we have it stored, it's an active locker.
handle_maybe_active_down(Cnt, Pid) ->
    case ets:lookup(?LOCKER_TAB, Pid) of
	[#locker{type = Type}] ->
	    ets:delete(?LOCKER_TAB, Pid),
	    {dec(Cnt), Type};
	_ ->
	    {Cnt, read}
    end.

handle_maybe_active_or_waiting_down(Pid, 
				    #state{active_count = Cnt, 
					   waiting      = Waiting} = State) ->
    case ets:lookup(?LOCKER_TAB, Pid) of
	[#locker{state = active, type = read}] when (Cnt == 1) ->
	    %% 1) This means that the writer must be waiting
	    %% 2) The last reader terminated, 
	    %%    time to activate the wating writer
	    %%    If this was the last one, then we must
	    %%    activate the waiting writer.
	    ets:delete(?LOCKER_TAB, Pid),
	    case active_waiting_writer(Waiting) of	
		{true, StillWaiting} ->
		    %% active count is still 1, so no need to update that
		    State#state{writer  = true, 
                                waiting = StillWaiting};
		{false, []} ->
		    State#state{active_count = 0, 
				writer       = false,
				waiting      = []}
	    end;

	[#locker{state = active, type = read}] ->
	    %% 1) This means that the writer must be waiting
	    %% 2) More then one (read-) locker active, just
	    %%    clean up.
	    ets:delete(?LOCKER_TAB, Pid),
	    State#state{active_count = dec(Cnt)};

	[#locker{state = active, type = write}] ->
	    ets:delete(?LOCKER_TAB, Pid),
	    {Active, StillWaiting, Writer} = 
		activate_waiting_readers_or_maybe_writer(Waiting),
	    State#state{active_count = Active,
			writer       = Writer, 
			waiting      = StillWaiting};

	[#locker{state = waiting, type = read}] ->
	    ets:delete(?LOCKER_TAB, Pid),
	    State#state{waiting = lists:delete(Pid, Waiting)};

	[#locker{state = waiting, type = write}] ->
	    %% We need to check if this is the only waiting writer.
	    %% If it is we shall set the writer field to false
	    ets:delete(?LOCKER_TAB, Pid),
	    NewWaiting = lists:delete(Pid, Waiting),
	    Writer = 
		case ets:match_object(?LOCKER_TAB, 
				      #locker{state = active, 
					      type  = write,
					      _     = '_'}) of
		    [] ->
			is_writer(NewWaiting);
		    _ ->
			true
		end,
	    State#state{writer  = Writer,
			waiting = NewWaiting};

	_Other ->
	    State

    end.

is_writer([]) ->
    false;
is_writer([Pid|Pids]) ->
    case ets:lookup(?LOCKER_TAB, Pid) of
	[#locker{type = write}] ->
	    true;
	_Other ->
	    is_writer(Pids)
    end.


%%----------------------------------------------------------

%% This is just a utility function to make sure we don't
%% end up in a lockout situation.
active_waiting_writer([]) ->
    {false, []};
active_waiting_writer([H|T]) ->
    case ets:lookup(?LOCKER_TAB, H) of
        [#locker{from = From} = L] ->
            ets:insert(?LOCKER_TAB, L#locker{state = active}),
            gen_server:reply(From, ok),
            {true, T};
        [] ->
            %% Oups
            error_msg("Could not find locker record for ~p", [H]),
            active_waiting_writer(T)
    end.


%% Activate waiting read(ers)
activate_waiting_readers(Waiting) ->
    activate_waiting_readers(Waiting, 1).

activate_waiting_readers([], Cnt) ->
    {Cnt, []};
activate_waiting_readers([H|T] = Waiting, Cnt) ->
    case ets:lookup(?LOCKER_TAB, H) of
        [#locker{from = From, type = read} = L] ->
            ets:insert(?LOCKER_TAB, L#locker{state = active}),
            gen_server:reply(From, ok),
            activate_waiting_readers(T, inc(Cnt));

        %% Found a writer, time to stop starting readers
        [#locker{type = write}] ->
            {Cnt, Waiting};

        [] ->
            %% Oups
            error_msg("Could not find locker record for ~p", [H]),
            activate_waiting_readers(T, Cnt)

    end.


activate_waiting_readers_or_maybe_writer(Waiting) ->
    activate_waiting_readers_or_maybe_writer(Waiting, 0).

activate_waiting_readers_or_maybe_writer([], Cnt) ->
    {Cnt, [], false};
activate_waiting_readers_or_maybe_writer([H|T] = Waiting, Cnt) ->
    case ets:lookup(?LOCKER_TAB, H) of
        [#locker{from = From, type = read} = L] ->
            ets:insert(?LOCKER_TAB, L#locker{state = active}),
            gen_server:reply(From, ok),
            activate_waiting_readers_or_maybe_writer(T, inc(Cnt));

        %% Only active writer only if it's the first
        [#locker{from = From, type = write} = L] when (Cnt == 0) ->
            ets:insert(?LOCKER_TAB, L#locker{state = active}),
            gen_server:reply(From, ok),
            {1, T, true};

        %% Found a writer, time to stop starting readers
        [#locker{type = write}] ->
            {Cnt, Waiting, false};

        [] ->
            %% Oups
            error_msg("Could not find locker record for ~p", [H]),
            activate_waiting_readers_or_maybe_writer(T, Cnt)

    end.


%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------

%% downgrade
%%
%% code_change({down, _Vsn}, S1, downgrade_to_pre_4_7) ->
%%     #state{dets = D, ets = E, notify_clients = NC, backup = B} = S1,
%%     stop_backup_server(B),
%%     S2 = {state, D, E, NC},
%%     {ok, S2};

%% upgrade
%%
%% code_change(_Vsn, S1, upgrade_from_pre_4_7) ->
%%     {state, D, E, NC} = S1,
%%     S2 = #state{dets = D, ets = E, notify_clients = NC},
%%     {ok, S2};

code_change(_Vsn, State, _Extra) ->
    {ok, State}.


%%------------------------------------------------------------------

inc(Cnt) ->
    Cnt + 1.

dec(Cnt) when (Cnt =< 0) ->
    0;
dec(Cnt) ->
    Cnt - 1.


%%------------------------------------------------------------------
%% This functions retrieves option values from the Options list.
%%------------------------------------------------------------------

get_opt(Key, Opts, Def) ->
    snmp_misc:get_option(Key, Opts, Def).


%%------------------------------------------------------------------

%% info_msg(F, A) ->
%%     ?snmpa_info("Target cache server: " ++ F, A).

warning_msg(F, A) ->
    ?snmpa_warning("Target cache server: " ++ F, A).

error_msg(F, A) ->
    ?snmpa_error("Target cache server: " ++ F, A).

%% ---

%% user_err(F, A) ->
%%     snmpa_error:user_err(F, A).

config_err(F, A) ->
    snmpa_error:config_err(F, A).

%% error(Reason) ->
%%     throw({error, Reason}).


%% ----------------------------------------------------------------

call(Req) ->
    gen_server:call(?SERVER, Req, infinity).

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).
