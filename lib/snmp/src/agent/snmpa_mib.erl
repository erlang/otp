%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(snmpa_mib).


%%%-----------------------------------------------------------------
%%% This module implements a MIB server.
%%%-----------------------------------------------------------------

%% External exports
-export([start_link/3, stop/1, 
	 lookup/2, next/3, which_mib/2, which_mibs/1, whereis_mib/2, 
	 load_mibs/3, unload_mibs/3, 
	 register_subagent/3, unregister_subagent/2, info/1, info/2, 
	 verbosity/2, dump/1, dump/2,
	 backup/2,
	 invalidate_cache/1, 
	 gc_cache/1, gc_cache/2, gc_cache/3,
	 enable_cache/1, disable_cache/1,
	 enable_cache_autogc/1, disable_cache_autogc/1, 
	 update_cache_gclimit/2, 
	 update_cache_age/2,
	 which_cache_size/1
	]).

%% <BACKWARD-COMPAT>
-export([load_mibs/2, unload_mibs/2]).
%% </BACKWARD-COMPAT>

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-include_lib("kernel/include/file.hrl").
-include("snmpa_internal.hrl").
-include("snmp_types.hrl").
-include("snmp_verbosity.hrl").
-include("snmp_debug.hrl").


-define(SERVER,                ?MODULE).
-define(NO_CACHE,              no_mibs_cache).
-define(DEFAULT_CACHE_USAGE,   true).
-define(CACHE_GC_TICKTIME,     timer:minutes(1)).
-define(DEFAULT_CACHE_AUTOGC,  true).
-define(DEFAULT_CACHE_GCLIMIT, 100).
-define(DEFAULT_CACHE_AGE,     timer:minutes(10)).
-define(CACHE_GC_TRIGGER,      cache_gc_trigger).



-ifdef(snmp_debug).
-define(GS_START_LINK(Prio, Mibs, Opts),
        gen_server:start_link(?MODULE, [Prio, Mibs, Opts], [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Prio, Mibs, Opts),
        gen_server:start_link(?MODULE, [Prio, Mibs, Opts], [])).
-endif.
 

%%-----------------------------------------------------------------
%% Internal Data structures
%%
%%   State
%%       data - is the MIB data (defined in mib_data module)
%%       meo  - mib entry override
%%       teo  - trap (notification) entry override
%%-----------------------------------------------------------------
-record(state, 
	{data, meo, teo, backup, 
	 cache, cache_tmr, cache_autogc, cache_gclimit, cache_age, 
	 data_mod}).



%%-----------------------------------------------------------------
%% Func: start_link/1
%% Args: Mibs is a list of mibnames.
%%       Prio is priority of mib-server
%%       Opts is a list of options
%% Purpose: starts the mib server synchronized
%% Returns: {ok, Pid} | {error, Reason}
%%-----------------------------------------------------------------
start_link(Prio, Mibs, Opts) ->
    ?d("start_link -> entry with"
	"~n   Prio: ~p"
	"~n   Mibs: ~p"
	"~n   Opts: ~p", [Prio, Mibs, Opts]),
    ?GS_START_LINK(Prio, Mibs, Opts).

verbosity(MibServer, Verbosity) -> 
    cast(MibServer, {verbosity,Verbosity}).

stop(MibServer) ->
    call(MibServer, stop).

invalidate_cache(MibServer) ->
    call(MibServer, invalidate_cache).

gc_cache(MibServer) ->
    call(MibServer, gc_cache).

gc_cache(MibServer, Age) ->
    call(MibServer, {gc_cache, Age}).

gc_cache(MibServer, Age, GcLimit) ->
    call(MibServer, {gc_cache, Age, GcLimit}).

which_cache_size(MibServer) ->
    call(MibServer, cache_size).

enable_cache(MibServer) ->
    update_cache_opts(MibServer, cache, true).
disable_cache(MibServer) ->
    update_cache_opts(MibServer, cache, false).

enable_cache_autogc(MibServer) ->
    update_cache_opts(MibServer, autogc, true).
disable_cache_autogc(MibServer) ->
    update_cache_opts(MibServer, autogc, false).

update_cache_gclimit(MibServer, GcLimit) 
  when ((is_integer(GcLimit) andalso (GcLimit > 0)) orelse 
	(GcLimit =:= infinity)) ->
    update_cache_opts(MibServer, gclimit, GcLimit);
update_cache_gclimit(_, BadLimit) ->
    {error, {bad_gclimit, BadLimit}}.

update_cache_age(MibServer, Age) 
  when is_integer(Age) andalso (Age > 0) ->
    update_cache_opts(MibServer, age, Age);
update_cache_age(_, BadAge) ->
    {error, {bad_age, BadAge}}.

update_cache_opts(MibServer, Key, Value) ->
    call(MibServer, {update_cache_opts, Key, Value}).


%%-----------------------------------------------------------------
%% Func: lookup/2
%% Purpose: Finds the mib entry corresponding to the Oid. If it is a
%%          variable, the Oid must be <Oid for var>.0 and if it is
%%          a table, Oid must be <table>.<entry>.<col>.<any>
%% Returns: {variable, MibEntry} |
%%          {table_column, MibEntry, TableEntryOid} |
%%          {subagent, SubAgentPid} |
%%          false
%%-----------------------------------------------------------------
lookup(MibServer, Oid) ->
    call(MibServer, {lookup, Oid}).

which_mib(MibServer, Oid) ->
    call(MibServer, {which_mib, Oid}).


%%-----------------------------------------------------------------
%% Func: next/3
%% Purpose: Finds the lexicographically next oid.
%% Returns: {subagent, SubAgentPid, SANextOid} |
%%          endOfMibView |
%%          genErr |
%%          NextOid
%%   The SANextOid is used by the agent if the SubAgent returns
%%   endOfMib, in a new call to next/2.
%%-----------------------------------------------------------------
next(MibServer, Oid, MibView) ->
    call(MibServer, {next, Oid, MibView}).


%%----------------------------------------------------------------------
%% Purpose: Loads mibs into the mib process.
%% Args: Mibs is a list of Filenames (compiled mibs).
%%       Force is a boolean
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------

%% <BACKWARD-COMPAT>
load_mibs(MibServer, Mibs) ->
    load_mibs(MibServer, Mibs, false).
%% </BACKWARD-COMPAT>

load_mibs(MibServer, Mibs, Force) ->
    call(MibServer, {load_mibs, Mibs, Force}).


%%----------------------------------------------------------------------
%% Purpose: Loads mibs into the mib process.
%% Args: Mibs is a list of Filenames (compiled mibs).
%%       Force is a boolean
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------
%% <BACKWARD-COMPAT>
unload_mibs(MibServer, Mibs) ->
    unload_mibs(MibServer, Mibs, false).
%% </BACKWARD-COMPAT>

unload_mibs(MibServer, Mibs, Force) ->
    call(MibServer, {unload_mibs, Mibs, Force}).


%%----------------------------------------------------------------------
%% Purpose: Simple management functions
%% Args: Mib is the name of the mib (atom)
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------
which_mibs(MibServer) ->
    call(MibServer, which_mibs).

whereis_mib(MibServer, Mib) ->
    call(MibServer, {whereis_mib, Mib}).


%%----------------------------------------------------------------------
%% Registers subagent with pid Pid under subtree Oid.
%%----------------------------------------------------------------------
register_subagent(MibServer, Oid, Pid) ->
    call(MibServer, {register_subagent, Oid, Pid}).

unregister_subagent(MibServer, OidOrPid) ->
    call(MibServer, {unregister_subagent, OidOrPid}).

info(MibServer) ->
    call(MibServer, info).

info(MibServer, Type) ->
    call(MibServer, {info, Type}).

dump(MibServer) ->
    dump(MibServer, io).

dump(MibServer, File) when (File =:= io) orelse is_list(File) ->
    call(MibServer, {dump, File}).

backup(MibServer, BackupDir) when is_list(BackupDir) ->
    call(MibServer, {backup, BackupDir}).


%%--------------------------------------------------
%% The standard MIB 'stdmib' must be present in the
%% current directory.
%%--------------------------------------------------
init([Prio, Mibs, Opts]) ->
    ?d("init -> entry with"
	"~n   Prio: ~p"
	"~n   Mibs: ~p"
	"~n   Opts: ~p", [Prio, Mibs, Opts]),
    case (catch do_init(Prio, Mibs, Opts)) of
	{ok, State} ->
	    {ok, State};
	{error, Reason} ->
	    config_err("failed starting mib-server: ~n~p", [Reason]),
	    {stop, {error, Reason}};
	Error ->
	    config_err("failed starting mib-server: ~n~p", [Error]),
	    {stop, {error, Error}}
    end.

do_init(Prio, Mibs, Opts) ->
    process_flag(priority, Prio),
    process_flag(trap_exit, true),
    put(sname, ms),
    put(verbosity, ?vvalidate(get_verbosity(Opts))),
    ?vlog("starting",[]),

    %% Extract the cache options
    {Cache, CacheOptions} = 
	case get_opt(cache, Opts, ?DEFAULT_CACHE_USAGE) of
	    true ->
		{new_cache(), []};
	    false ->
		{?NO_CACHE, []};
	    CacheOpts when is_list(CacheOpts) ->
		{new_cache(), CacheOpts};
	    Bad ->
		throw({error, {bad_option, {cache, Bad}}})
	end,
    CacheAutoGC  = get_cacheopt_autogc(Cache,  CacheOptions), 
    CacheGcLimit = get_cacheopt_gclimit(Cache, CacheOptions), 
    CacheAge     = get_cacheopt_age(Cache,     CacheOptions), 
    
    %% Maybe start the cache gc timer
    CacheGcTimer = 
	if 
	    ((Cache =/= ?NO_CACHE) andalso 
	     (CacheAutoGC =:= true)) ->
		start_cache_gc_timer();
	    true ->
		undefined
	end,

    MeOverride = get_me_override(Opts),
    TeOverride = get_te_override(Opts),
    MibStorage = get_mib_storage(Opts),
    MibDataMod = get_data_mod(Opts), 
    ?vtrace("init -> try create mib data with"
	    "~n   MeOverride: ~p"
	    "~n   TeOverride: ~p"
	    "~n   MibStorage: ~p", [MeOverride, TeOverride, MibStorage]),
    Data       = MibDataMod:new(MibStorage),
    ?vdebug("init -> mib data created", []),
    case (catch mib_operations(MibDataMod, 
			       load_mib, Mibs, Data, 
			       MeOverride, TeOverride, true)) of
	{ok, Data2} ->
	    ?vdebug("started",[]),
	    MibDataMod:sync(Data2),
	    ?vdebug("mib data synced",[]),
	    {ok, #state{data          = Data2, 
			teo           = TeOverride, 
			meo           = MeOverride,
			cache         = Cache, 
			cache_tmr     = CacheGcTimer, 
			cache_autogc  = CacheAutoGC,
			cache_gclimit = CacheGcLimit,
			cache_age     = CacheAge, 
			data_mod      = MibDataMod}};
	{'aborted at', Mib, _NewData, Reason} ->
	    ?vinfo("failed loading mib ~p: ~p",[Mib,Reason]),
	    {error, {Mib, Reason}}
    end.


%%----------------------------------------------------------------------
%% Returns: {ok, NewMibData} | {'aborted at', Mib, NewData, Reason}
%% Args: Operation is load_mib | unload_mib.
%%----------------------------------------------------------------------
mib_operations(_Mod, _Operation, [], Data, _MeOverride, _TeOverride, _Force) ->
    {ok, Data};
mib_operations(Mod, Operation, [Mib|Mibs], Data0, MeOverride, TeOverride, Force) ->
    ?vtrace("mib operations ~p on"
	    "~n   Mibs: ~p"
	    "~n   with "
	    "~n   MeOverride: ~p"
	    "~n   TeOverride: ~p"
	    "~n   Force:      ~p", 
	    [Operation, Mibs, MeOverride, TeOverride, Force]),
    Data = mib_operation(Mod, 
			 Operation, Mib, Data0, MeOverride, TeOverride, Force),
    mib_operations(Mod, Operation, Mibs, Data, MeOverride, TeOverride, Force).

mib_operation(Mod, Operation, Mib, Data0, MeOverride, TeOverride, Force) 
  when is_list(Mib) ->
    ?vtrace("mib operation on mib ~p", [Mib]),
    case apply(Mod, Operation, [Data0, Mib, MeOverride, TeOverride]) of
	{error, already_loaded} when (Operation =:= load_mib) andalso  
				       (Force =:= true) ->
	    ?vlog("ignore mib ~p -> already loaded", [Mib]),
	    Data0;
	{error, not_loaded} when (Operation =:= unload_mib) andalso 
				 (Force =:= true) ->
	    ?vlog("ignore mib ~p -> not loaded", [Mib]),
	    Data0;
	{error, Reason} ->
	    ?vlog("mib_operation -> failed ~p of mib ~p for ~p", 
		[Operation, Mib, Reason]),
	    throw({'aborted at', Mib, Data0, Reason});
	{ok, Data} ->
	    Data
    end;
mib_operation(_Mod, _Op, Mib, Data, _MeOverride, _TeOverride, _Force) ->
    throw({'aborted at', Mib, Data, bad_mibname}).


%%-----------------------------------------------------------------
%% Handle messages
%%-----------------------------------------------------------------

handle_call(invalidate_cache, _From, #state{cache = Cache} = State) ->
    ?vlog("invalidate_cache", []), 
    NewCache = maybe_invalidate_cache(Cache),
    {reply, ignore, State#state{cache = NewCache}};

handle_call(cache_size, _From, #state{cache = Cache} = State) ->
    ?vlog("cache_size", []), 
    Reply = maybe_cache_size(Cache),
    {reply, Reply, State};

handle_call(gc_cache, _From, 
	    #state{cache         = Cache, 
		   cache_age     = Age, 
		   cache_gclimit = GcLimit} = State) ->
    ?vlog("gc_cache", []), 
    Result = maybe_gc_cache(Cache, Age, GcLimit),
    {reply, Result, State};

handle_call({gc_cache, Age}, _From, 
	    #state{cache         = Cache, 
		   cache_gclimit = GcLimit} = State) ->
    ?vlog("gc_cache with Age = ~p", [Age]), 
    Result = maybe_gc_cache(Cache, Age, GcLimit),
    {reply, Result, State};

handle_call({gc_cache, Age, GcLimit}, _From, 
	    #state{cache = Cache} = State) ->
    ?vlog("gc_cache with Age = ~p and GcLimut = ~p", [Age, GcLimit]), 
    Result = maybe_gc_cache(Cache, Age, GcLimit),
    {reply, Result, State};

handle_call({update_cache_opts, Key, Value}, _From, State) ->
    ?vlog("update_cache_opts: ~p -> ~p", [Key, Value]), 
    {Result, NewState} = handle_update_cache_opts(Key, Value, State),
    {reply, Result, NewState};

handle_call({lookup, Oid}, _From, 
	    #state{data = Data, cache = Cache, data_mod = Mod} = State) ->
    ?vlog("lookup ~p", [Oid]), 
    Key = {lookup, Oid}, 
    {Reply, NewState} = 
	case maybe_cache_lookup(Cache, Key) of
	    ?NO_CACHE ->
		{Mod:lookup(Data, Oid), State};
	    [] ->
		Rep = Mod:lookup(Data, Oid),
		ets:insert(Cache, {Key, Rep, timestamp()}),
		{Rep, maybe_start_cache_gc_timer(State)};
	    [{Key, Rep, _}] ->
		?vdebug("lookup -> found in cache", []), 
		ets:update_element(Cache, Key, {3, timestamp()}),
		{Rep, State}
	end,
    ?vdebug("lookup -> Reply: ~p", [Reply]), 
    {reply, Reply, NewState};

handle_call({which_mib, Oid}, _From, 
	    #state{data = Data, data_mod = Mod} = State) ->
    ?vlog("which_mib ~p",[Oid]),    
    Reply = Mod:which_mib(Data, Oid),
    ?vdebug("which_mib: ~p",[Reply]),    
    {reply, Reply, State};

handle_call({next, Oid, MibView}, _From, 
	    #state{data = Data, cache = Cache, data_mod = Mod} = State) ->
    ?vlog("next ~p [~p]", [Oid, MibView]), 
    Key = {next, Oid, MibView},
    {Reply, NewState} = 
	case maybe_cache_lookup(Cache, Key) of
	    ?NO_CACHE ->
		{Mod:next(Data, Oid, MibView), State};
	    [] ->    
		Rep = Mod:next(Data, Oid, MibView),
		ets:insert(Cache, {Key, Rep, timestamp()}),
		{Rep, maybe_start_cache_gc_timer(State)};
	    [{Key, Rep, _}] ->
		?vdebug("lookup -> found in cache", []), 
		ets:update_element(Cache, Key, {3, timestamp()}),
		{Rep, State}
	end,
    ?vdebug("next -> Reply: ~p", [Reply]), 
    {reply, Reply, NewState};

%% <BACKWARD-COMPAT>
handle_call({load_mibs, Mibs}, From, State) ->
    handle_call({load_mibs, Mibs, false}, From, State);
%% </BACKWARD-COMPAT>

handle_call({load_mibs, Mibs, Force}, _From, 
	    #state{data         = Data, 
		   teo          = TeOverride, 
		   meo          = MeOverride,
		   cache        = Cache, 
		   data_mod     = Mod} = State) ->
    ?vlog("[~w] load mibs ~p", [Force, Mibs]),    
    %% Invalidate cache
    NewCache = maybe_invalidate_cache(Cache),
    {NData, Reply} = 
	case (catch mib_operations(Mod, load_mib, Mibs, Data,
				   MeOverride, TeOverride, Force)) of
	    {'aborted at', Mib, NewData, Reason} ->
		?vlog("aborted at ~p for reason ~p",[Mib,Reason]),    
		{NewData, {error, {'load aborted at', Mib, Reason}}};
	    {ok, NewData} ->
		{NewData, ok}
	end,
    Mod:sync(NData),
    {reply, Reply, State#state{data = NData, cache = NewCache}};

%% <BACKWARD-COMPAT>
handle_call({unload_mibs, Mibs}, From, State) ->
    handle_call({unload_mibs, Mibs, false}, From, State);
%% </BACKWARD-COMPAT>

handle_call({unload_mibs, Mibs, Force}, _From, 
	    #state{data         = Data, 
		   teo          = TeOverride, 
		   meo          = MeOverride,
		   cache        = Cache, 
		   data_mod     = Mod} = State) ->
    ?vlog("[~w] unload mibs ~p", [Force, Mibs]),    
    %% Invalidate cache
    NewCache = maybe_invalidate_cache(Cache),
    %% Unload mib(s)
    {NData, Reply} = 
	case (catch mib_operations(Mod, unload_mib, Mibs, Data,
				   MeOverride, TeOverride, Force)) of
	    {'aborted at', Mib, NewData, Reason} ->
		?vlog("aborted at ~p for reason ~p", [Mib,Reason]),    
		{NewData, {error, {'unload aborted at', Mib, Reason}}};
	    {ok, NewData} ->
		{NewData, ok}
	end,
    Mod:sync(NData),
    {reply, Reply, State#state{data = NData, cache = NewCache}};

handle_call(which_mibs, _From, #state{data = Data, data_mod = Mod} = State) ->
    ?vlog("which mibs",[]),    
    Reply = Mod:which_mibs(Data),
    {reply, Reply, State};

handle_call({whereis_mib, Mib}, _From, 
	    #state{data         = Data, 
		   data_mod     = Mod} = State) ->
    ?vlog("whereis mib: ~p",[Mib]),    
    Reply = Mod:whereis_mib(Data, Mib),
    {reply, Reply, State};

handle_call({register_subagent, Oid, Pid}, _From, 
	    #state{data      = Data, 
		   cache     = Cache, 
		   data_mod  = Mod} = State) ->
    ?vlog("register subagent ~p, ~p",[Oid,Pid]),
    %% Invalidate cache
    NewCache = maybe_invalidate_cache(Cache),
    case Mod:register_subagent(Data, Oid, Pid) of
	{error, Reason} ->
	    ?vlog("registration failed: ~p",[Reason]),    
	    {reply, {error, Reason}, State#state{cache = NewCache}};
	{ok, NewData} ->
	    {reply, ok, State#state{data = NewData, cache = NewCache}}
    end;

handle_call({unregister_subagent, OidOrPid}, _From, 
	    #state{data     = Data, 
		   cache    = Cache, 
		   data_mod = Mod} = State) ->
    ?vlog("unregister subagent ~p",[OidOrPid]),    
    %% Invalidate cache
    NewCache = maybe_invalidate_cache(Cache),
    case Mod:unregister_subagent(Data, OidOrPid) of
	{ok, NewData} ->
	    {reply, ok, State#state{data = NewData, cache = NewCache}};
	{ok, NewData, DeletedSubagentPid} ->
	    {reply, {ok, DeletedSubagentPid}, State#state{data  = NewData, 
							  cache = NewCache}};
	{error, Reason} ->
	    ?vlog("unregistration failed: ~p",[Reason]),    
	    {reply, {error, Reason}, State#state{cache = NewCache}}
    end;

handle_call(info, _From, #state{data     = Data, 
				cache    = Cache, 
				data_mod = Mod} = State) ->
    ?vlog("info",[]),    
    Reply = 
	case (catch Mod:info(Data)) of
	    Info when is_list(Info) ->
		[{cache, size_cache(Cache)} | Info];
	    E ->
		    [{error, E}]
	    end,
    {reply, Reply, State};

handle_call({info, Type}, _From, 
	    #state{data     = Data, 
		   data_mod = Mod} = State) ->
    ?vlog("info ~p",[Type]),    
    Reply = 
	case (catch Mod:info(Data, Type)) of
	    Info when is_list(Info) ->
		Info;
	    E ->
		[{error, E}]
	end,
    {reply, Reply, State};

handle_call({dump, File}, _From, 
	    #state{data = Data, data_mod = Mod} = State) ->
    ?vlog("dump on ~s",[File]),    
    Reply = Mod:dump(Data, File),
    {reply, Reply, State};
    
%% This check (that there is no backup already in progress) is also 
%% done in the master agent process, but just in case a user issues 
%% a backup call to this process directly, we add a similar check here. 
handle_call({backup, BackupDir}, From, 
	    #state{backup   = undefined, 
		   data     = Data, 
		   data_mod = Mod} = State) ->
    ?vlog("backup to ~s", [BackupDir]),
    Pid = self(),
    V   = get(verbosity),
    case file:read_file_info(BackupDir) of
	{ok, #file_info{type = directory}} ->
	    BackupServer = 
		erlang:spawn_link(
		  fun() ->
			  put(sname, ambs),
			  put(verbosity, V),
			  Dir   = filename:join([BackupDir]),
			  Reply = Mod:backup(Data, Dir),
			  Pid ! {backup_done, Reply},
			  unlink(Pid)
		  end),	
	    ?vtrace("backup server: ~p", [BackupServer]),
	    {noreply, State#state{backup = {BackupServer, From}}};
	{ok, _} ->
	    {reply, {error, not_a_directory}, State};
	Error ->
	    {reply, Error, State}
    end;
    
handle_call({backup, _BackupDir}, _From, #state{backup = Backup} = S) ->
    ?vinfo("backup already in progress: ~p", [Backup]),
    {reply, {error, backup_in_progress}, S};

handle_call(stop, _From, State) ->
    ?vlog("stop",[]),    
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    warning_msg("received unknown request: ~n~p", [Req]),
    Reply = {error, {unknown, Req}}, 
    {reply, Reply, State}.
    
handle_cast({verbosity, Verbosity}, State) ->
    ?vlog("verbosity: ~p -> ~p",[get(verbosity),Verbosity]),    
    put(verbosity,snmp_verbosity:validate(Verbosity)),
    {noreply, State};
    
handle_cast(Msg, State) ->
    warning_msg("received unknown message: ~n~p", [Msg]),
    {noreply, State}.
    

handle_info({'EXIT', Pid, Reason}, #state{backup = {Pid, From}} = S) ->
    ?vlog("backup server (~p) exited for reason ~n~p", [Pid, Reason]),
    gen_server:reply(From, {error, Reason}),
    {noreply, S#state{backup = undefined}};

handle_info({'EXIT', Pid, Reason}, S) ->
    %% The only other processes we should be linked to are
    %% either the master agent or our supervisor, so die...
    {stop, {received_exit, Pid, Reason}, S};

handle_info({backup_done, Reply}, #state{backup = {_, From}} = S) ->
    ?vlog("backup done:"
	  "~n   Reply: ~p", [Reply]),
    gen_server:reply(From, Reply),
    {noreply, S#state{backup = undefined}};

handle_info(?CACHE_GC_TRIGGER, #state{cache         = Cache, 
				      cache_age     = Age, 
				      cache_gclimit = GcLimit, 
				      cache_autogc  = true} = S) 
  when (Cache =/= ?NO_CACHE) ->
    ?vlog("cache gc trigger event", []),
    maybe_gc_cache(Cache, Age, GcLimit), 
    Tmr = start_cache_gc_timer(),
    {noreply, S#state{cache_tmr = Tmr}};

handle_info(?CACHE_GC_TRIGGER, S) ->
    ?vlog("out-of-date cache gc trigger event - ignore", []),
    {noreply, S#state{cache_tmr = undefined}};

handle_info(Info, State) ->
    warning_msg("received unknown info: ~n~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{data = Data, data_mod = Mod}) ->
    catch Mod:close(Data),
    ok.



%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------

%% downgrade
%% 
%% code_change({down, _Vsn}, S1, downgrade_to_pre_4_12) ->
%%     #state{data = Data, meo = MEO, teo = TEO, backup = B, cache = Cache} = S1, 
%%     del_cache(Cache), 
%%     S2 = {state, Data, MEO, TEO, B},
%%     {ok, S2};

code_change({down, Vsn}, #state{data = Data0, data_mod = Mod} = State, Extra) ->
    Data = Mod:code_change(down, Vsn, Extra, Data0), 
    {ok, State#state{data = Data}};


%% %% upgrade
%% %% 
%% code_change(_Vsn, S1, upgrade_from_pre_4_12) ->
%%     {state, Data, MEO, TEO, B} = S1,
%%     Cache = new_cache(), 
%%     S2 = #state{data = Data, meo = MEO, teo = TEO, backup = B, cache = Cache},
%%     {ok, S2};

code_change(Vsn, #state{data = Data0, data_mod = Mod} = State, Extra) ->
    Data = Mod:code_change(up, Vsn, Extra, Data0), 
    {ok, State#state{data = Data}}.


%%-----------------------------------------------------------------
%% Option access functions
%%-----------------------------------------------------------------

get_verbosity(Options) ->
    get_opt(verbosity, Options, ?default_verbosity).

get_me_override(Options) ->
    get_opt(mibentry_override, Options, false).

get_te_override(Options) ->
    get_opt(trapentry_override, Options, false).

get_mib_storage(Options) ->
    get_opt(mib_storage, Options).

get_data_mod(Options) ->
    get_opt(data_module, Options, snmpa_mib_data_tttn).

get_cacheopt_autogc(Cache, CacheOpts) ->
    IsValid = fun(AutoGC) when ((AutoGC =:= true) orelse 
				(AutoGC =:= false)) ->
		      true;
		 (_) ->
		      false
	      end,
    get_cacheopt(Cache, autogc, CacheOpts, 
		 false, ?DEFAULT_CACHE_AUTOGC, 
		 IsValid).

get_cacheopt_gclimit(Cache, CacheOpts) ->
    IsValid = fun(Limit) when ((is_integer(Limit) andalso (Limit > 0)) orelse 
			       (Limit =:= infinity)) ->
		      true;
		 (_) ->
		      false
	      end,
    get_cacheopt(Cache, gclimit, CacheOpts, 
		 infinity, ?DEFAULT_CACHE_GCLIMIT, 
		 IsValid).

get_cacheopt_age(Cache, CacheOpts) ->
    IsValid = fun(Age) when is_integer(Age) andalso (Age > 0) ->
		      true;
		 (_) ->
		      false
	      end,
    get_cacheopt(Cache, age, CacheOpts, 
		 ?DEFAULT_CACHE_AGE, ?DEFAULT_CACHE_AGE, 
		 IsValid).

get_cacheopt(?NO_CACHE, _, _, NoCacheVal, _, _) ->
    NoCacheVal;
get_cacheopt(_, Key, Opts, _, Default, IsValid) ->
    Val = get_opt(Key, Opts, Default),
    case IsValid(Val) of
	true ->
	    Val;
	false ->
	    throw({error, {bad_option, {Key, Val}}})
    end.


%% ----------------------------------------------------------------

handle_update_cache_opts(cache, true = _Value, 
			 #state{cache = ?NO_CACHE} = State) ->
    {ok, State#state{cache = new_cache()}};
handle_update_cache_opts(cache, true = _Value, State) ->
    {ok, State};

handle_update_cache_opts(cache, false = _Value,
			 #state{cache = ?NO_CACHE} = State) ->
    {ok, State};
handle_update_cache_opts(cache, false = _Value,
			 #state{cache     = Cache,
				cache_tmr = Tmr} = State) ->
    maybe_stop_cache_gc_timer(Tmr),
    del_cache(Cache),
    {ok, State#state{cache = ?NO_CACHE, cache_tmr = undefined}};

handle_update_cache_opts(autogc, true = _Value, 
			 #state{cache_autogc = true} = State) ->
    {ok, State};
handle_update_cache_opts(autogc, true = Value, State) ->
    {ok, maybe_start_cache_gc_timer(State#state{cache_autogc = Value})};
handle_update_cache_opts(autogc, false = _Value, 
			 #state{cache_autogc = false} = State) ->
    {ok, State};
handle_update_cache_opts(autogc, false = Value, 
			 #state{cache_tmr = Tmr} = State) ->
    maybe_stop_cache_gc_timer(Tmr),
    {ok, State#state{cache_autogc = Value, cache_tmr = undefined}};

handle_update_cache_opts(age, Age, State) ->
    {ok, State#state{cache_age = Age}};

handle_update_cache_opts(gclimit, GcLimit, State) ->
    {ok, State#state{cache_gclimit = GcLimit}};

handle_update_cache_opts(BadKey, Value, State) ->
    {{error, {bad_cache_opt, BadKey, Value}}, State}.


maybe_stop_cache_gc_timer(undefined) ->
    ok;
maybe_stop_cache_gc_timer(Tmr) ->
    erlang:cancel_timer(Tmr).


maybe_start_cache_gc_timer(#state{cache        = Cache, 
				  cache_autogc = true,
				  cache_tmr    = undefined} = State) 
  when (Cache =/= ?NO_CACHE) ->
    Tmr = start_cache_gc_timer(), 
    State#state{cache_tmr = Tmr};
maybe_start_cache_gc_timer(State) ->
    State.

start_cache_gc_timer() ->
    erlang:send_after(?CACHE_GC_TICKTIME, self(), ?CACHE_GC_TRIGGER).


%% ----------------------------------------------------------------

maybe_gc_cache(?NO_CACHE, _Age) ->
    ?vtrace("cache not enabled", []),
    ok;
maybe_gc_cache(Cache, Age) ->
    MatchSpec = gc_cache_matchspec(Age), 
    Keys = ets:select(Cache, MatchSpec),
    do_gc_cache(Cache, Keys),
    {ok, length(Keys)}.

maybe_gc_cache(?NO_CACHE, _Age, _GcLimit) ->
    ok;
maybe_gc_cache(Cache, Age, infinity = _GcLimit) ->
    maybe_gc_cache(Cache, Age);
maybe_gc_cache(Cache, Age, GcLimit) ->
    MatchSpec = gc_cache_matchspec(Age), 
    Keys = 
	case ets:select(Cache, MatchSpec, GcLimit) of
	    {Match, _Cont} ->
		Match;
	    '$end_of_table' ->
		[]
	end,
    do_gc_cache(Cache, Keys),
    {ok, length(Keys)}.

gc_cache_matchspec(Age) ->
    Oldest    = timestamp() - Age,
    MatchHead = {'$1', '_', '$2'}, 
    Guard     = [{'<', '$2', Oldest}], 
    MatchFunc = {MatchHead, Guard, ['$1']},
    MatchSpec = [MatchFunc],
    MatchSpec.

do_gc_cache(_, []) ->
    ok;
do_gc_cache(Cache, [Key|Keys]) ->
    ets:delete(Cache, Key),
    do_gc_cache(Cache, Keys).

maybe_invalidate_cache(?NO_CACHE) ->
    ?NO_CACHE;
maybe_invalidate_cache(Cache) ->
    del_cache(Cache),
    new_cache().

maybe_cache_size(?NO_CACHE) ->
    {error, not_enabled};
maybe_cache_size(Cache) ->
    {ok, ets:info(Cache, size)}.

new_cache() ->
    ets:new(snmpa_mib_cache, [set, protected, {keypos, 1}]).

del_cache(?NO_CACHE) ->
    ok;
del_cache(Cache) ->
    ets:delete(Cache).

maybe_cache_lookup(?NO_CACHE, _) ->
    ?NO_CACHE;
maybe_cache_lookup(Cache, Key) ->
    ets:lookup(Cache, Key).

size_cache(?NO_CACHE) ->
    undefined;
size_cache(Cache) ->
    case (catch ets:info(Cache, memory)) of
	Sz when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end.

timestamp() ->
    snmp_misc:now(ms).


%% ----------------------------------------------------------------

get_opt(Key, Options) ->
    snmp_misc:get_option(Key, Options).

get_opt(Key, Options, Default) ->
    snmp_misc:get_option(Key, Options, Default).


%% ----------------------------------------------------------------

cast(MibServer, Msg) ->
    gen_server:cast(MibServer, Msg).

call(MibServer, Req) ->
    call(MibServer, Req, infinity).

call(MibServer, Req, To) ->
    gen_server:call(MibServer, Req, To).


%% ----------------------------------------------------------------

%% info_msg(F, A) ->
%%     ?snmpa_info("Mib server: " ++ F, A).

warning_msg(F, A) ->
    ?snmpa_warning("Mib server: " ++ F, A).

%% error_msg(F, A) ->
%%     ?snmpa_error("Mib server: " ++ F, A).

config_err(F, A) ->
    snmpa_error:config_err(F, A).
 
