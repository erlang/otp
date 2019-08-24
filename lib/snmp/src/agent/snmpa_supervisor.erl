%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
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
-module(snmpa_supervisor).

-behaviour(supervisor).

%% External exports
-export([start_link/2, stop/0, stop/1]).
-export([start_sub_sup/1, start_master_sup/1]).
-export([start_sub_agent/3, stop_sub_agent/1]).

%% Internal exports
-export([init/1, config/2]).


-define(SERVER, ?MODULE).

-include("snmpa_internal.hrl").
-include("snmp_verbosity.hrl").
-include("snmp_debug.hrl").


%%-----------------------------------------------------------------
%% Process structure
%% =================
%%
%%             ___________________ supervisor __________________
%%            /              |              |          \         \ 
%%   ___misc_sup___    target_cache  symbolic_store   local_db   agent_sup
%%  /     |        \                                               |   | 
%% mib  net_if  note_store                                        MA - SA
%%
%%  The supervisor (one at each node) starts:
%%    snmpa_symbolic_store (one at each node)
%%    snmpa_local_db (one at each node)
%%    snmpa_target_cache (one at each node) 
%%    MA - which starts
%%      own mib (hangs it under misc_sup)
%%      net_if (hangs it under misc_sup) 
%%      note_store (hangs it under misc_sup)
%%    SAs - which starts
%%      own mib (hangs it under misc_sup)
%%
%% This structure is intended to make the agent fault tolerant. The
%% agent processes (by far most code is in these processes) can be
%% restarted in case of a failure, as all data needed by these procs
%% are stored in the other procs. Any other process crash leads to
%% that the entire snmpa_supervisor crashes. If it is restarted, all
%% dynamically loaded mibs must be reloaded.
%%
%% It is up to the supervisor to configure the internal and
%% external mibs. Note that the 
%% agent group (internal MIB) and sysObjectID *must* be configured
%% in some way.
%%-----------------------------------------------------------------

start_link(Type, Opts) ->
    ?d("start_link -> entry with"
      "~n   Type. ~p"
      "~n   Opts. ~p", [Type, Opts]),
    start_link(get_agent_type(Opts), Opts, Type).

start_link(sub, Opts, _Type) ->
    start_sub_sup(Opts);
start_link(master, Opts, normal) ->
    start_master_sup(Opts);
start_link(master, Opts, {takeover, Node}) ->
    case start_master_sup(Opts) of
	{ok, Pid} ->
            OwnMibNames = get_own_loaded_mibs(),
            try_load_other_loaded_mibs(Node, OwnMibNames),
            {ok, Pid};
        Else ->
            Else
    end.


stop() ->
    stop(0).

stop(Timeout) ->
    case whereis(?SERVER) of
	Pid when is_pid(Pid) ->
            stop(Pid, Timeout);
	_ ->
	    not_running
    end.

%% For some unfathomable reason there is no "nice" way to stop
%% a supervisor. The "normal" way to do it is:
%% 1) exit(Pid, kill) (kaboom)
%% 2) If the caller is the *parent*: exit(Pid, shutdown)
%% So, here we do it the really ugly way...but since this function is 
%% intended for testing (mostly)...
stop(Pid, Timeout) when (Timeout =:= 0) ->
    sys:terminate(Pid, shutdown),
    ok;
stop(Pid, Timeout) ->
    MRef = erlang:monitor(process, Pid),
    sys:terminate(Pid, shutdown),
    receive
        {'DOWN', MRef, process, Pid, _} ->
            ok
    after Timeout ->
            erlang:demonitor(MRef, [flush]),
            {error, timeout}
    end.
    

get_own_loaded_mibs() ->
    AgentInfo = snmpa:info(snmp_master_agent),
    [ Name || {Name, _, _} <- loaded_mibs(AgentInfo) ].

try_load_other_loaded_mibs(Node, OwnMibs) ->
    case rpc:call(Node, snmpa, info, [snmp_master_agent]) of
        {badrpc, R} ->
            error_msg("could not takeover loaded mibs: ~p", [R]);
	AgentInfo ->
            LoadedMibs = loaded_mibs(AgentInfo),
            MibsToLoad = mibs_to_load(LoadedMibs, OwnMibs),
            lists:foreach(fun(M) -> takeover_mib(M) end, MibsToLoad)
    end.

loaded_mibs(AgentInfo) ->
    {value, {_, MibInfo}}    = key1search(mib_server,  AgentInfo),
    {value, {_, LoadedMibs}} = key1search(loaded_mibs, MibInfo),
    LoadedMibs.

mibs_to_load(OtherMibs, OwnMibs) ->
    [{N, S, F} || {N, S, F} <- OtherMibs, not lists:member(N, OwnMibs)].

takeover_mib({'STANDARD-MIB', _Symbolic, _FileName}) ->
    ok;
takeover_mib({'SNMPv2-MIB', _Symbolic, _FileName}) ->
    ok;
takeover_mib({_MibName, _Symbolic, FileName}) ->
    case snmpa:load_mibs(snmp_master_agent, [FileName]) of
        ok -> 
            ok;
        {error, R} ->
            error_msg("could not reload mib ~p: ~p", [FileName, R])
    end.


%% ----------------------------------------------------------------

start_sub_sup(Opts) ->
    ?d("start_sub_sup -> entry with"
      "~n   Opts: ~p", [Opts]),
    (catch do_start_sub_sup(Opts)).

do_start_sub_sup(Opts) ->
    verify_mandatory([db_dir], Opts),
    ?d("do_start_sub_sup -> start (sub) supervisor",[]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [sub, Opts]).  

start_master_sup(Opts) ->
    (catch do_start_master_sup(Opts)).

do_start_master_sup(Opts) ->
    verify_mandatory([db_dir], Opts),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [master, Opts]).  

verify_mandatory([], _) ->
    ok;
verify_mandatory([Key|Keys], Opts) ->
    case lists:keymember(Key, 1, Opts) of
	true ->
	    verify_mandatory(Keys, Opts);
	false ->
	    throw({error, {missing_mandatory_option, Key}})
    end.


%% ----------------------------------------------------------------

start_sub_agent(ParentAgent, Subtree, Mibs) 
  when is_pid(ParentAgent) andalso is_list(Mibs) ->
    ?d("start_sub_agent -> entry with"
      "~n   ParentAgent: ~p"
      "~n   Subtree:     ~p"
      "~n   Mibs:        ~p", [ParentAgent, Subtree, Mibs]),
    snmpa_agent_sup:start_subagent(ParentAgent, Subtree, Mibs).

stop_sub_agent(SubAgentPid) ->
    snmpa_agent_sup:stop_subagent(SubAgentPid).


%% ----------------------------------------------------------------

init([AgentType, Opts]) ->
    ?d("init -> entry with"
      "~n   AgentType: ~p"
      "~n   Opts:      ~p", [AgentType, Opts]),

    put(sname,     asup),
    put(verbosity, get_verbosity(Opts)),

    ?vlog("starting",[]),

    ?vdebug("create agent table",[]),
    ets:new(snmp_agent_table, [set, public, named_table]),

    ?vdebug("create community cache",[]),
    ets:new(snmp_community_cache, [bag, public, named_table]),

    %% Get restart type for the agent
    Restart = get_opt(restart_type, Opts, permanent),
    ?vdebug("agent restart type: ~w", [Restart]),

    %% -- Agent type --
    store(agent_type, AgentType),

    %% -- Prio --
    Prio = get_opt(priority, Opts, normal),
    ?vdebug("[agent table] store priority: ~p",[Prio]),
    store(priority, Prio),

    %% -- Versions -- 
    Vsns = get_opt(versions, Opts, [v1,v2,v3]),
    ?vdebug("[agent table] store versions: ~p",[Vsns]),
    store(versions, Vsns),

    %% -- Max number of VBs in a Get-BULK response --
    GbMaxVBs = get_gb_max_vbs(Opts),
    ?vdebug("[agent table] Get-BULK max VBs: ~p", [GbMaxVBs]),
    store(gb_max_vbs, GbMaxVBs),

    %% -- DB-directory --
    DbDir = get_opt(db_dir, Opts),
    ?vdebug("[agent table] store db_dir: ~n   ~p",[DbDir]),
    store(db_dir, filename:join([DbDir])),

    DbInitError = get_opt(db_init_error, Opts, terminate),
    ?vdebug("[agent table] store db_init_error: ~n   ~p",[DbInitError]),
    store(db_init_error, DbInitError),

    %% -- Error report module --
    ErrorReportMod = get_opt(error_report_mod, Opts, snmpa_error_logger),
    ?vdebug("[agent table] store error report module: ~w",[ErrorReportMod]),
    store(error_report_mod, ErrorReportMod),

    %% -- mib storage --
    %% MibStorage has only one mandatory part: module
    %% Everything else is module dependent and therefor 
    %% put in a special option: options
    MibStorage = 
	case get_opt(mib_storage, Opts, [{module, snmpa_mib_storage_ets}]) of

	    %% --- ETS wrappers ---

	    ets ->
		[{module, snmpa_mib_storage_ets}];
	    {ets, default} ->
		[{module,  snmpa_mib_storage_ets}, 
		 {options, [{dir,    filename:join([DbDir])}, 
			    {action, keep}]}];
	    {ets, Dir} when is_list(Dir) ->
		[{module,  snmpa_mib_storage_ets}, 
		 {options, [{dir,    filename:join([Dir])}, 
			    {action, keep}]}];
	    {ets, default, Action} when ((Action =:= keep) orelse 
					 (Action =:= clear)) ->
		[{module,  snmpa_mib_storage_ets}, 
		 {options, [{dir,    filename:join([DbDir])}, 
			    {action, Action}]}];
	    {ets, Dir, Action} when is_list(Dir) andalso 
				    ((Action =:= keep) orelse 
				     (Action =:= clear)) ->
		[{module,  snmpa_mib_storage_ets}, 
		 {options, [{dir,    filename:join([Dir])}, 
			    {action, Action}]}];

	    %% --- DETS wrappers ---

	    dets ->
		[{module,  snmpa_mib_storage_dets}, 
		 {options, [{dir,    filename:join([DbDir])}, 
			    {action, keep}]}];
	    {dets, default} ->
		[{module,  snmpa_mib_storage_dets}, 
		 {options, [{dir,    filename:join([DbDir])}, 
			    {action, keep}]}];
	    {dets, default, Action} when ((Action =:= keep) orelse 
					  (Action =:= clear)) ->
		[{module,  snmpa_mib_storage_dets}, 
		 {options, [{dir,    filename:join([DbDir])}, 
			    {action, Action}]}];
	    {dets, Dir, Action} when is_list(Dir) andalso 
				     ((Action =:= keep) orelse 
				      (Action =:= clear)) ->
		[{module,  snmpa_mib_storage_dets}, 
		 {options, [{dir,    filename:join([Dir])}, 
			    {action, Action}]}];

	    %% --- Mnesia wrappers ---

	    mnesia ->
		[{module,  snmpa_mib_storage_mnesia}, 
		 {options, [{nodes,  erlang:nodes()}, 
			    {action, keep}]}];
	    {mnesia, Nodes0} ->
		Nodes = 
		    if
			Nodes0 =:= visible ->
			    erlang:nodes(visible);
			Nodes0 =:= connected ->
			    erlang:nodes(connected);
			Nodes0 =:= [] ->
			    [node()];
			true ->
			    Nodes0
		    end,
		[{module, snmpa_mib_storage_mnesia}, 
		 {options, [{nodes,  Nodes}, 
			    {action, keep}]}];
	    {mnesia, Nodes0, Action} when ((Action =:= keep) orelse 
					   (Action =:= clear)) ->
		Nodes = 
		    if
			Nodes0 =:= visible ->
			    erlang:nodes(visible);
			Nodes0 =:= connected ->
			    erlang:nodes(connected);
			Nodes0 =:= [] ->
			    [node()];
			true ->
			    Nodes0
		    end,
		[{module,  snmpa_mib_storage_mnesia}, 
		 {options, [{nodes,  Nodes}, 
			    {action, Action}]}];

	    Other when is_list(Other) ->
		Other
	end,

    ?vdebug("[agent table] store mib storage: ~w", [MibStorage]),
    store(mib_storage, MibStorage),

    %% -- Agent mib storage --
    AgentMibStorage = get_opt(agent_mib_storage, Opts, persistent),
    %% ?vdebug("[agent table] store agent mib storage: ~w",[AgentMibStorage]),
    store(agent_mib_storage, AgentMibStorage),

    %% -- System start time --
    ?vdebug("[agent table] store system start time",[]),
    store(system_start_time, snmp_misc:now(cs)),

    %% -- Symbolic store options --
    SsOpts = get_opt(symbolic_store, Opts, []),
    ?vdebug("[agent table] store symbolic store options: ~w",[SsOpts]),
    store(symbolic_store, SsOpts),

    %% -- Local DB options --
    LdbOpts = get_opt(local_db, Opts, []),
    ?vdebug("[agent table] store local db options: ~w",[LdbOpts]),
    store(local_db, LdbOpts),

    %% -- Target cache options --
    TargetCacheOpts = get_opt(target_cache, Opts, []),
    ?vdebug("[agent table] store target cache options: ~w",[TargetCacheOpts]),
    store(target_cache, TargetCacheOpts),

    %% -- Specs --
    SupFlags = {one_for_all, 0, 3600},

    MiscSupSpec = 
	sup_spec(snmpa_misc_sup, [], Restart, infinity),

    SymStoreOpts = [{mib_storage, MibStorage} | SsOpts], 
    SymStoreArgs = [Prio, SymStoreOpts],
    SymStoreSpec = 
	worker_spec(snmpa_symbolic_store, SymStoreArgs, Restart, 2000),

    LdbArgs = [Prio, DbDir, DbInitError, LdbOpts],
    LocalDbSpec = 
	worker_spec(snmpa_local_db, LdbArgs, Restart, 5000),

    ?vdebug("init VACM",[]),
    snmpa_vacm:init(DbDir, DbInitError),

    TargetCacheArgs = [Prio, TargetCacheOpts],
    TargetCacheSpec = 
	worker_spec(snmpa_target_cache, TargetCacheArgs, transient, 2000, []),

    Rest =
	case AgentType of
	    master ->
		% If we're starting the master, we must also
		% configure the tables.

		%% -- Config --
		ConfOpts = get_opt(config, Opts, []),
		?vdebug("[agent table] store config options: ~p", [ConfOpts]),
		store(config, ConfOpts),

		ConfigArgs = [Vsns, ConfOpts],
		ConfigSpec = 
		    worker_spec(config, ?MODULE, config, ConfigArgs, 
				transient, 2000, [?MODULE]),

		%% -- Agent verbosity --
		AgentVerb  = get_opt(agent_verbosity, Opts, silence),

		%% -- Discovery processing --
		DiscoOpts = get_opt(discovery, Opts, []),
		?vdebug("[agent table] store discovery options: ~p", [DiscoOpts]),
		store(discovery, DiscoOpts),

		%% -- Mibs --
		Mibs = get_mibs(get_opt(mibs, Opts, []), Vsns),
		?vdebug("[agent table] store mibs: ~n   ~p",[Mibs]),
		store(mibs, Mibs),

		Ref  = make_ref(),

		%% -- Get module --
		GetModule  = get_opt(get_mechanism, Opts, snmpa_get),
		?vdebug("[agent table] store get-module: ~p", [GetModule]),
		store(get_mechanism, GetModule),

		%% -- Set module --
		SetModule  = get_opt(set_mechanism, Opts, snmpa_set),
		?vdebug("[agent table] store set-module: ~p",[SetModule]),
		store(set_mechanism, SetModule),

		%% -- Authentication service --
		AuthModule = get_opt(authentication_service, Opts, snmpa_acm),
		?vdebug("[agent table] store authentication service: ~w",
			[AuthModule]),
		store(authentication_service, AuthModule),

		%% -- Multi-threaded --
		MultiT = get_opt(multi_threaded, Opts, false),
		?vdebug("[agent table] store multi-threaded: ~p", [MultiT]),
		store(multi_threaded, MultiT),

		%% -- Audit trail log --
		case get_opt(audit_trail_log, Opts, not_found) of
		    not_found ->
			?vdebug("[agent table] no audit trail log", []),
			ok;
		    AtlOpts ->
			?vdebug("[agent table] "
				"store audit trail log options: ~p",
				[AtlOpts]),
			store(audit_trail_log, AtlOpts),
			ok
		end,

		%% -- MIB server --
		MibsOpts = get_opt(mib_server, Opts, []),
		?vdebug("[agent table] store mib-server options: "
			"~n   ~p", [MibsOpts]),
		store(mib_server, MibsOpts),

		%% -- Network interface --
		NiOpts = get_opt(net_if, Opts, []),
		?vdebug("[agent table] store net-if options: "
			"~n   ~p", [NiOpts]),
		store(net_if, NiOpts),

		%% -- Note store --
		NsOpts = get_opt(note_store, Opts, []),
		?vdebug("[agent table] store note-store options: "
			"~n   ~p",[NsOpts]),
		store(note_store, NsOpts),

		AgentOpts = 
		    [{verbosity,              AgentVerb},
		     {mibs,                   Mibs},
		     {mib_storage,            MibStorage},
		     {get_mechanism,          GetModule},
		     {set_mechanism,          SetModule},
		     {authentication_service, AuthModule},
		     {multi_threaded,         MultiT},
		     {versions,               Vsns},
		     {net_if,                 NiOpts},
		     {mib_server,             MibsOpts},
		     {note_store,             NsOpts},
		     {gb_max_vbs,             GbMaxVBs} |
		     get_opt(master_agent_options, Opts, [])],
		     
		AgentSpec =
		    worker_spec(snmpa_agent, 
				[Prio, snmp_master_agent, none, Ref, AgentOpts],
				Restart, 15000),
		AgentSupSpec = 
		    sup_spec(snmpa_agent_sup, [AgentSpec], 
			     Restart, infinity), 
		[ConfigSpec, AgentSupSpec];
	    _ ->
		?vdebug("[sub agent] spec for the agent supervisor",[]),
		AgentSupSpec = 
		    sup_spec(snmpa_agent_sup, [], Restart, infinity), 
		[AgentSupSpec]
	end,
    ?vdebug("init done",[]),
    {ok, {SupFlags, [MiscSupSpec, SymStoreSpec, LocalDbSpec, TargetCacheSpec | 
		     Rest]}}.


store(Key, Value) ->
    ets:insert(snmp_agent_table, {Key, Value}).

get_mibs(Mibs, Vsns) ->
    MibDir = filename:join(code:priv_dir(snmp), "mibs"),
    StdMib = 
	case (lists:member(v2, Vsns) or lists:member(v3, Vsns)) of
	    true  -> filename:join([MibDir, "SNMPv2-MIB"]);
	    false -> filename:join([MibDir, "STANDARD-MIB"])
	end,
    ?vdebug("add standard and v2 mibs",[]),
    NMibs = add_mib(StdMib, Mibs, ["SNMPv2-MIB", "STANDARD-MIB"]),
    case lists:member(v3, Vsns) of
	true -> 
	    ?vdebug("add v3 mibs",[]),
	    add_v3_mibs(MibDir, NMibs);
	false -> 
	    NMibs
    end.
    
add_v3_mibs(MibDir, Mibs) ->
    NMibs = add_mib(filename:join(MibDir, "SNMP-FRAMEWORK-MIB"),
		    Mibs, ["SNMP-FRAMEWORK-MIB"]),
    add_mib(filename:join(MibDir, "SNMP-MPD-MIB"),
	    NMibs, ["SNMP-MPD-MIB"]).

add_mib(DefaultMib, [], _BaseNames) -> [DefaultMib];
add_mib(DefaultMib, [Mib | T], BaseNames) ->
    case lists:member(filename:basename(Mib), BaseNames) of
	true  -> [Mib | T]; % The user defined his own version of the mib
	false -> [Mib | add_mib(DefaultMib, T, BaseNames)]
    end.


config(Vsns, Opts) ->
    ?d("config -> entry with"
      "~n   Vsns. ~p"
      "~n   Opts. ~p", [Vsns, Opts]),
    Verbosity = get_opt(verbosity, Opts, silence),
    put(sname, conf),
    put(verbosity, Verbosity),

    ?vlog("starting", []),

    ConfDir   = get_opt(dir, Opts),
    ForceLoad = get_opt(force_load, Opts, false),

    case (catch conf(ConfDir, Vsns, ForceLoad)) of
	ok -> 
	    ?d("config -> done", []),
	    ignore;
	{'EXIT', Reason} -> 
	    ?vlog("configure failed (exit) with reason: ~p",[Reason]),
	    {error, {config_error, Reason}}
    end.

conf(Dir, Vsns, true) ->
    conf1(Dir, Vsns, reconfigure);
conf(Dir, Vsns, false) ->
    conf1(Dir, Vsns, configure).

conf1(Dir, Vsns, Func) ->
    ?vlog("start ~w",[Func]),

    ?vdebug("~w snmp_standard_mib",[Func]),
    snmp_standard_mib:Func(Dir),
    ?vdebug("init snmp_framework_mib",[]),
    snmp_framework_mib:init(),
    ?vdebug("configure snmp_framework_mib",[]),
    snmp_framework_mib:configure(Dir),
    ?vdebug("~w snmp_target_mib",[Func]),
    snmp_target_mib:Func(Dir),
    ?vdebug("~w snmp_notification_mib",[Func]),
    snmp_notification_mib:Func(Dir),
    ?vdebug("~w snmp_view_based_acm_mib",[Func]),
    snmp_view_based_acm_mib:Func(Dir),
    case lists:member(v1, Vsns) or lists:member(v2, Vsns) of
	true ->
	    ?vdebug("we need to handle v1 and/or v2 =>~n"
		    "   ~w snmp_community_mib",[Func]),
	    snmp_community_mib:Func(Dir);
	false ->
	    ?vdebug("no need to handle v1 or v2",[]),
	    ok
    end,
    case lists:member(v3, Vsns) of
	true ->
	    ?vdebug("we need to handle v3 =>~n"
		    "   ~w snmp_user_based_sm_mib",[Func]),
	    snmp_user_based_sm_mib:Func(Dir);
	false ->
	    ?vdebug("no need to handle v3",[]),
	    ok
    end,
    ?vdebug("conf done",[]),
    ok.


%% -------------------------------------

sup_spec(Name, Args, Type, Time) ->
    ?d("sup_spec -> entry with"
       "~n   Name: ~p"
       "~n   Args: ~p"
       "~n   Type: ~p"
       "~n   Time: ~p", [Name, Args, Type, Time]),
    {Name, 
     {Name, start_link, Args}, 
     Type, Time, supervisor, [Name, supervisor]}.

worker_spec(Name, Args, Type, Time) ->
    worker_spec(Name, Name, Args, Type, Time, []).

worker_spec(Name, Args, Type, Time, Modules) ->
    worker_spec(Name, Name, Args, Type, Time, Modules).

worker_spec(Name, Mod, Args, Type, Time, Modules) ->
    worker_spec(Name, Mod, start_link, Args, Type, Time, Modules).

worker_spec(Name, Mod, Func, Args, Type, Time, Modules) 
  when is_atom(Name) andalso 
       is_atom(Mod) andalso 
       is_atom(Func) andalso 
       is_list(Args) andalso 
       is_atom(Type) andalso 
       is_list(Modules) ->
    ?d("worker_spec -> entry with"
       "~n   Name:    ~p"
       "~n   Mod:     ~p"
       "~n   Func:    ~p"
       "~n   Args:    ~p"
       "~n   Type:    ~p"
       "~n   Time:    ~p"
       "~n   Modules: ~p", [Name, Mod, Func, Args, Type, Time, Modules]),
    {Name, 
     {Mod, Func, Args}, 
     Type, Time, worker, [Name] ++ Modules}.


get_verbosity(Opts) -> 
    SupOpts = get_opt(supervisor, Opts, []),
    get_opt(verbosity, SupOpts, silence).


get_agent_type(Opts) ->
    get_opt(agent_type, Opts, master).


%% We validate this option! This should really be done for all
%% options, but it is beyond the scope of this ticket, OTP-9700.

get_gb_max_vbs(Opts) ->
    Validate = 
	fun(GbMaxVBs) 
	   when ((is_integer(GbMaxVBs) andalso (GbMaxVBs > 0)) orelse
		 (GbMaxVBs =:= infinity)) ->
		ok;
	   (_) ->
		error
	end,
    get_option(gb_max_vbs, ?DEFAULT_GB_MAX_VBS, Validate, Opts).

get_option(Key, Default, Validate, Opts) 
  when is_list(Opts) andalso is_function(Validate) ->
    Value = get_opt(Key, Opts, Default),
    case Validate(Value) of
	ok ->
	    Value;
	error ->
	    exit({bad_option, Key, Value})
    end.
    

get_opt(Key, Opts) ->
    snmp_misc:get_option(Key, Opts).

get_opt(Key, Opts, Def) ->
    snmp_misc:get_option(Key, Opts, Def).

key1search(Key, List) ->
    lists:keysearch(Key, 1, List).


%%-----------------------------------------------------------------

error_msg(F, A) ->
    ?snmpa_error(F, A).

% i(F) ->
%     i(F, []).

% i(F, A) ->
%     io:format("~p:~p: " ++ F ++ "~n", [node(),?MODULE|A]).
