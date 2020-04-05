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
-module(snmpa_agent).

-include_lib("kernel/include/file.hrl").
-include("snmpa_internal.hrl").
-include("snmp_types.hrl").
-include("snmp_debug.hrl").
-include("snmp_verbosity.hrl").
-include("SNMP-FRAMEWORK-MIB.hrl").

%% External exports
-export([start_link/4, start_link/5, stop/1]).
-export([subagent_get/3, subagent_get_next/3,
         subagent_set/2, 
	 load_mibs/3, unload_mibs/3, 
	 which_mibs/1, whereis_mib/2, info/1,
	 register_subagent/3, unregister_subagent/2,
	 send_notification/3, 
         register_notification_filter/5,
         unregister_notification_filter/2,
         which_notification_filter/1,
 	 get_net_if/1]).
-export([
	 discovery/6, 
	 is_originating_discovery_enabled/0,
	 is_terminating_discovery_enabled/0,
	 terminating_discovery_stage2/0,
	 terminating_trigger_username/0
	]).
-export([verbosity/2, dump_mibs/1, dump_mibs/2]).
-export([validate_err/3, make_value_a_correct_value/3, 
	 do_get/3, do_get/4, 
	 get/2, get/3, get_next/2, get_next/3]).
-export([mib_of/1, mib_of/2, me_of/1, me_of/2,
	 invalidate_mibs_cache/1,
	 which_mibs_cache_size/1, 
	 enable_mibs_cache/1, disable_mibs_cache/1,
	 gc_mibs_cache/1, gc_mibs_cache/2, gc_mibs_cache/3,
	 enable_mibs_cache_autogc/1, disable_mibs_cache_autogc/1,
	 update_mibs_cache_age/2, 
	 update_mibs_cache_gclimit/2]).
-export([get_agent_mib_storage/0, db/1, 
	 backup/2]).
-export([get_log_type/1,      set_log_type/2]).
-export([get_request_limit/1, set_request_limit/2]).
-export([invalidate_ca_cache/0]).
-export([increment_counter/3]).
-export([restart_worker/1, restart_set_worker/1]).

%% For backward compatibillity
-export([send_trap/6, send_trap/7]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, tr_var/2, tr_varbind/1,
	 handle_pdu/8, worker/2, worker_loop/1, 
	 do_send_trap/7, do_send_trap/8]).
%% <BACKWARD-COMPAT>
-export([handle_pdu/7, 
	 load_mibs/2, unload_mibs/2]).
%% </BACKWARD-COMPAT>

-include("snmpa_internal.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.

-define(empty_pdu_size, 21).

-ifdef(snmp_extended_verbosity).
-define(vt(F,A), ?vtrace(F, A)).
-else.
-define(vt(_F, _A), ok).
-endif.

-define(DISCO_TERMINATING_TRIGGER_USERNAME, "").

-ifdef(snmp_debug).
-define(GS_START_LINK3(Prio, Parent, Ref, Opts),
        gen_server:start_link(?MODULE, [Prio, Parent, Ref, Opts],
                              [{debug,[trace]}])).
-define(GS_START_LINK4(Prio, Name, Parent, Ref, Opts),
        gen_server:start_link({local, Name}, ?MODULE, 
			      [Prio, Parent, Ref, Opts],
                              [{debug,[trace]}])).
-else.
-define(GS_START_LINK3(Prio, Parent, Ref, Opts),
        gen_server:start_link(?MODULE, [Prio, Parent, Ref, Opts],[])).
-define(GS_START_LINK4(Prio, Name, Parent, Ref, Opts),
        gen_server:start_link({local, Name}, ?MODULE, 
			      [Prio, Parent, Ref, Opts],[])).
-endif.

%% Increment this whenever a change is made to the worker interface 
-define(WORKER_INTERFACE_VERSION, 1).

%% -- Utility macros for creating worker commands --
-define(mk_pdu_wreq(Vsn, Pdu, PduMS, ACMData, Address, GbMaxVBs, Extra),
	#wrequest{cmd  = handle_pdu, 
		  info = [{vsn,        Vsn}, 
			  {pdu,        Pdu}, 
			  {pdu_ms,     PduMS}, 
			  {acm_data,   ACMData}, 
			  {addr,       Address}, 
			  {gb_max_vbs, GbMaxVBs}, 
			  {extra,      Extra}]}).
-define(mk_send_trap_wreq(TrapRec, NotifyName, ContextName, 
			  Recv, Vbs, LocalEngineID, Extra),
	#wrequest{cmd  = send_trap, 
		  info = [{trap_rec,        TrapRec}, 
			  {notify_name,     NotifyName}, 
			  {context_name,    ContextName}, 
			  {receiver,        Recv}, 
			  {varbinds,        Vbs}, 
			  {local_engine_id, LocalEngineID},
			  {extra,           Extra}]}).
-define(mk_terminate_wreq(), #wrequest{cmd = terminate, info = []}).
-define(mk_verbosity_wreq(V), #wrequest{cmd  = verbosity, 
					info = [{verbosity, V}]}).


-record(notification_filter, {id, mod, data}).
-record(disco, 
	{from, rec, sender, target, engine_id, 
	 sec_level, ctx, ivbs, stage, handler, extra}).

%% This record is used when sending requests to the worker processes
-record(wrequest, 
	{
	  version = ?WORKER_INTERFACE_VERSION, 
	  cmd, 
	  info
	 }
       ).


%%-----------------------------------------------------------------
%% The agent is multi-threaded, i.e. each request is handled
%% by a separate process.  However, in the normal case, there
%% is just one request handled at the time.  In order to improve
%% performance, there is always two worker processes alive.  They are
%% created at initialization time.  There is always one worker
%% dedicated to SET-handling.  When a get*-request is received,
%% it is sent to the worker, and the worker is marked as busy.
%% If a request is received when the worker is busy, a new temporary
%% worker is spawned.
%% 
%% Code change
%% ===========
%% Note that the worker(s) execute the same module as the master
%% agent. For code change we have two options - ignore the workers,
%% or send them a code change message.
%% 
%%-----------------------------------------------------------------

-record(state, {type, 
		parent, 
		worker, 
		worker_state = ready,
		set_worker, 
		multi_threaded, 
		ref, 
		vsns,
		nfilters = [],
		note_store,
		mib_server,   %% Currently unused
		net_if,       %% Currently unused
		net_if_mod,   
		backup,
		disco,
		mibs_cache_request,
		gb_max_vbs}).


%%%-----------------------------------------------------------------
%%% This module implements the agent machinery; both for the master
%%% agent and the subagents.
%%%-----------------------------------------------------------------
%%% Table of contents
%%% =================
%%% 1. Interface
%%% 2. Main loop
%%% 3. GET REQUEST
%%% 4. GET-NEXT REQUEST
%%% 5. GET-BULK REQUEST
%%% 6. SET REQUEST
%%% 7. Misc functions
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Parent is a Pid (of master_agent) or none
%% Options is a list of Option, where Option is
%%   {mibs, Mibs}
%%   {net_if, NetIfModule}
%%   {priority, Prio}
%%   {verbosity, Verbosity}
%%   {multi_threaded, Bool} true means that SETs are serialized,
%%      while GETs are concurrent, even with a SET.
%%   {set_mechanism, SetModule}           % undocumented feature
%%
%% The following options are now removed - they are not needed
%% anymore when VACM is standard for authentication, and works
%% with all versions, and trap sending is standardized too.
%%   {authentication_service, AuthModule} % undocumented feature
%%   {trap_mechanism, TrapModule}         % undocumented feature
%% Note: authentication_service is reintroduced for AXD301 (OTP-3324).
%%-----------------------------------------------------------------
start_link(Prio, Parent, Ref, Options) ->
    ?d("start_link -> entry with"
       "~n   Prio:    ~p"
       "~n   Parent:  ~p"
       "~n   Ref:     ~p"
       "~n   Options: ~p", [Prio, Parent, Ref, Options]),
    ?GS_START_LINK3(Prio, Parent, Ref, Options).

start_link(Prio, Name, Parent, Ref, Options) ->
    ?d("start_link -> entry with"
       "~n   Prio:    ~p"
       "~n   Name:    ~p"
       "~n   Parent:  ~p"
       "~n   Ref:     ~p"
       "~n   Options: ~p", [Prio, Name, Parent, Ref, Options]),
    ?GS_START_LINK4(Prio, Name, Parent, Ref, Options).

stop(Agent) -> call(Agent, stop).

restart_worker(Agent) ->
    call(Agent, restart_worker).

restart_set_worker(Agent) ->
    call(Agent, restart_set_worker).

get_log_type(Agent) ->
    call(Agent, get_log_type).

set_log_type(Agent, NewType) ->
    call(Agent, {set_log_type, NewType}).

get_request_limit(Agent) ->
    call(Agent, get_request_limit).

set_request_limit(Agent, NewLimit) ->
    call(Agent, {set_request_limit, NewLimit}).

mib_of(Oid) when is_list(Oid) ->
    mib_of(snmp_master_agent, Oid).

mib_of(Agent, Oid) when is_list(Oid) ->
    call(Agent, {mib_of, Oid}).

me_of(Oid) when is_list(Oid) ->
    me_of(snmp_master_agent, Oid).

me_of(Agent, Oid) when is_list(Oid) ->
    call(Agent, {me_of, Oid}).


invalidate_mibs_cache(Agent) ->
    call(Agent, {mibs_cache_request, invalidate_cache}).


gc_mibs_cache(Agent) ->
    call(Agent, {mibs_cache_request, gc_cache}).

gc_mibs_cache(Agent, Age) ->
    call(Agent, {mibs_cache_request, {gc_cache, Age}}).

gc_mibs_cache(Agent, Age, GcLimit) ->
    call(Agent, {mibs_cache_request, {gc_cache, Age, GcLimit}}).


enable_mibs_cache(Agent) ->
    call(Agent, {mibs_cache_request, enable_cache}).

disable_mibs_cache(Agent) ->
    call(Agent, {mibs_cache_request, disable_cache}).


which_mibs_cache_size(Agent) ->
    call(Agent, {mibs_cache_request, cache_size}).


enable_mibs_cache_autogc(Agent) ->
    call(Agent, {mibs_cache_request, enable_autogc}).

disable_mibs_cache_autogc(Agent) ->
    call(Agent, {mibs_cache_request, disable_autogc}).


update_mibs_cache_gclimit(Agent, GcLimit) ->
    call(Agent, {mibs_cache_request, {update_gclimit, GcLimit}}).


update_mibs_cache_age(Agent, Age) ->
    call(Agent, {mibs_cache_request, {update_age, Age}}).


increment_counter(Counter, Initial, Max) ->
    %% This is to make sure no one else increments our counter
    Key = {Counter, self()}, 

    %% Counter data
    Position  = 2, 
    Increment = 1, 
    Threshold = Max,
    SetValue  = Initial, 
    UpdateOp  = {Position, Increment, Threshold, SetValue},
    
    %% And now for the actual increment
    Tab = snmp_agent_table, 
    case (catch ets:update_counter(Tab, Key, UpdateOp)) of
	{'EXIT', {badarg, _}} ->
	    %% Oups, first time
	    ets:insert(Tab, {Key, Initial}),
	    Initial;
	Next when is_integer(Next) ->
	    Next
    end.


init([Prio, Parent, Ref, Options]) ->
    ?d("init -> entry with"
       "~n   Prio:    ~p"
       "~n   Parent:  ~p"
       "~n   Ref:     ~p"
       "~n   Options: ~p", [Prio, Parent, Ref, Options]),
    case (catch do_init(Prio, Parent, Ref, Options)) of
	{ok, State} ->
	    ?vdebug("started",[]),
	    {ok, State};
	{error, Reason} ->
	    config_err("failed starting agent: ~n~p", [Reason]),
	    {stop, Reason}
    end.

do_init(Prio, Parent, Ref, Options) ->
    process_flag(priority, Prio),
    put(sname,short_name(Parent)),
    put(verbosity,get_verbosity(Options)),
    ?vlog("starting with: "
	  "~n   Prio:    ~p"
	  "~n   Parent:  ~p"
	  "~n   Ref:     ~p"
	  "~n   Options: ~p",[Prio, Parent, Ref, Options]),

    Mibs       = get_mibs(Options),

    SetModule  = get_set_mechanism(Options),
    put(set_module, SetModule),
    ?vtrace("set-module: ~w", [SetModule]),

    GetModule  = get_get_mechanism(Options),
    put(get_module, GetModule),
    ?vtrace("get-module: ~w", [GetModule]),

    %% OTP-3324. For AXD301.
    AuthModule = get_authentication_service(Options),
    put(auth_module, AuthModule),
    ?vtrace("auth-module: ~w", [AuthModule]),

    MultiT  = get_multi_threaded(Options),
    Vsns    = get_versions(Options),

    GbMaxVbs = get_gb_max_vbs(Options), 

    NS = start_note_store(Prio, Ref, Options),
    {Type, NetIfPid, NetIfMod} = 
	start_net_if(Parent, Prio, Ref, Vsns, NS, Options),

    MibPid = start_mib_server(Prio, Ref, Mibs, Options),
    
    put(net_if, NetIfPid),
    put(mibserver, MibPid),
    process_flag(trap_exit, true),
    {Worker, SetWorker} = workers_start(MultiT),
    {ok, #state{type           = Type, 
		parent         = Parent, 
		worker         = Worker,
		set_worker     = SetWorker,
		multi_threaded = MultiT, 
		ref            = Ref,
		vsns           = Vsns,
		note_store     = NS,
		net_if_mod     = NetIfMod,
		gb_max_vbs     = GbMaxVbs}}.


start_note_store(Prio, Ref, Options) ->
    ?vdebug("start_note_store -> with Prio: ~p", [Prio]),
    NsOpts = get_note_store_opt(Options),

    ?vtrace("start_note_store -> NsOpts: ~p", [NsOpts]),

    case (catch snmpa_misc_sup:start_note_store(Prio, Ref, NsOpts)) of
	{ok, Pid} ->
	    ?vdebug("start_note_store -> Pid: ~p", [Pid]),
	    Pid;
	{error, Reason} -> 
	    ?vinfo("error starting note store: ~n~p",[Reason]),
	    throw({error, {note_store_error, Reason}});
	{'EXIT', Reason} ->
	    ?vinfo("exit starting note store: ~n~p",[Reason]),
	    throw({error, {note_store_exit, Reason}});
	Error ->
	    ?vinfo("failed starting note store: ~n~p",[Error]),
	    throw({error, {note_store_failed, Error}})
    end.
    

start_net_if(none, Prio, Ref, Vsns, NoteStore, Options) ->
    ?vdebug("start_net_if(none) -> with Prio: ~p", [Prio]),
    NetIfOpts = get_net_if_opt(Options),
    Verbosity = get_net_if_verbosity(NetIfOpts),
    Mod       = get_net_if_module(NetIfOpts),
    NiOptions = get_net_if_options(NetIfOpts),
    NiOpts    = [{versions,  Vsns}, 
		 {verbosity, Verbosity} | NiOptions],
    
    ?vtrace("start_net_if -> "
	    "~n   Mod:    ~p"
	    "~n   NiOpts: ~p",[Mod, NiOpts]),

    case (catch snmpa_misc_sup:start_net_if(Prio, NoteStore, Ref, self(),
					    Mod, NiOpts)) of
	{ok, Pid} -> 
	    ?vdebug("start_net_if -> Pid: ~p", [Pid]),
	    {master_agent, Pid, Mod};
	{error, Reason} -> 
	    ?vinfo("error starting net if: ~n~p",[Reason]),
	    throw({error, {net_if_error, Reason}});
	{'EXIT', Reason} ->
	    ?vinfo("exit starting net if: ~n~p",[Reason]),
	    throw({error, {net_if_exit, Reason}});
	Error ->
	    ?vinfo("failed starting net if: ~n~p",[Error]),
	    throw({error, {net_if_failed, Error}})
    end;
start_net_if(Parent, _Prio, _Ref, _Vsns, _NoteStore, _Options) 
  when is_pid(Parent) ->
    ?vdebug("start_net_if(~p) -> subagent => ignore", [Parent]),
    {subagent, undefined, undefined}.
    
    
start_mib_server(Prio, Ref, Mibs, Options) ->
    ?vdebug("start_mib_server -> with Prio: ~p", [Prio]),
    MibStorage = get_mib_storage(Options),
    MibsOpts   = [{mib_storage, MibStorage} | 
		  get_option(mib_server, Options, [])],

    ?vtrace("start_mib_server -> "
	    "~n   Mibs:     ~p"
	    "~n   MibsOpts: ~p", [Mibs, MibsOpts]),

    case (catch snmpa_misc_sup:start_mib_server(Prio, Ref, Mibs, MibsOpts)) of
	{ok, Pid} ->
	    ?vdebug("start_mib_server -> Pid: ~p", [Pid]),
	    Pid;
	{error, Reason} -> 
	    ?vinfo("error starting mib server: ~n~p",[Reason]),
	    throw({error, {mib_server_error, Reason}});
	{'EXIT', Reason} ->
	    ?vinfo("exit starting mib server: ~n~p",[Reason]),
	    throw({error, {mib_server_exit, Reason}});
	Error ->
	    ?vinfo("failed starting mib server: ~n~p",[Error]),
	    throw({error, {mib_server_failed, Error}})
    end.


%%-----------------------------------------------------------------
%% Purpose: We must calculate the length of an empty Pdu.  This
%%          length is used to calculate the max pdu size allowed
%%          for each get-bulk-request. This size is 
%%          dependent on the varbinds. It is calculated
%%          as EmptySize + 8.  8 comes from the fact that the
%%          maximum pdu size needs 31 bits which needs 5 * 7 bits to be
%%          expressed. One 7bit octet is already present in the
%%          empty pdu, leaving 4 more 7bit octets. The length is
%%          repeated twice, once for the varbinds, and once for the
%%          entire pdu; 2 * 4 = 8.
%% Actually, this function is not used, we use a constant instead.
%%-----------------------------------------------------------------
%% Ret: 21
%% empty_pdu() ->
%%     Pdu = #pdu{type         = 'get-response', 
%%                request_id   = 1,
%% 	          error_status = noError, 
%%                error_index  = 0, 
%%                varbinds     = []},
%%     length(snmp_pdus:enc_pdu(Pdu)) + 8.


%%%--------------------------------------------------
%%% 1. Interface
%%%--------------------------------------------------
%% Called by administrator (not subagent; deadlock could occur)
register_subagent(Agent, SubTreeOid, SubagentPid) ->
    call(Agent, {register_subagent, SubTreeOid, SubagentPid}).

%% Called by administrator (not subagent; deadlock could occur)
unregister_subagent(Agent, SubagentOidOrPid) ->
    call(Agent, {unregister_subagent, SubagentOidOrPid}).


%%-----------------------------------------------------------------
%% These subagent_ functions either return a value, or exits
%% with {nodedown, Node} | Reason.
%%-----------------------------------------------------------------

%% A proper spec for this would be something like this:
%% But, there is now way to spec that a process *can* exit.
%% -spec subagent_get(Agent, VBs, IsNotification) ->
%%                           {noError, 0, NewVBs} |
%%                           {ErrStatus, ErrIndex, []} |
%%                           erlang:exit(Reason) when
%%       Agent          :: pid(),
%%       VBs            :: [snmp:varbind()],
%%       IsNotification :: boolean(),
%%       NewVBs         :: [snmp:varbind()],
%%       ErrStatus      :: snmp:error_status(),
%%       ErrIndex       :: snmp:error_index(),
%%       Reason         :: {nodedown, Node} | term(),
%%       Node           :: atom().

subagent_get(SubAgent, VBs, IsNotification) ->
    PduData = get_pdu_data(),
    call(SubAgent, {subagent_get, VBs, PduData, IsNotification}).

subagent_get_next(SubAgent, MibView, Varbinds) ->
    PduData = get_pdu_data(),
    call(SubAgent, {subagent_get_next, MibView, Varbinds, PduData}).

subagent_set(SubAgent, Arguments) ->
    PduData = get_pdu_data(),
    call(SubAgent, {subagent_set, Arguments, PduData}).


%% Called by administrator (not agent; deadlock would occur)
%% <BACKWARD-COMPAT>
load_mibs(Agent, Mibs) ->
    load_mibs(Agent, Mibs, false).
%% </BACKWARD-COMPAT>

load_mibs(Agent, Mibs, Force) ->
    call(Agent, {load_mibs, Mibs, Force}).

%% Called by administrator (not agent; deadlock would occur)
%% <BACKWARD-COMPAT>
unload_mibs(Agent, Mibs) ->
    unload_mibs(Agent, Mibs, false).
%% </BACKWARD-COMPAT>

unload_mibs(Agent, Mibs, Force) ->
    call(Agent, {unload_mibs, Mibs, Force}).

which_mibs(Agent) ->
    call(Agent, which_mibs).

whereis_mib(Agent, Mib) ->
    call(Agent, {whereis_mib, Mib}).

info(Agent) ->
    call(Agent, info).


get_net_if(Agent) ->
    call(Agent, get_net_if).


register_notification_filter(Agent, Id, Mod, Args, Where) 
  when (Where =:= first) orelse (Where =:= last) ->
    case (catch verify_notification_filter_behaviour(Mod)) of
	ok ->
	    call(Agent, {register_notification_filter, Id, Mod, Args, Where});
	Error ->
	    Error
    end;
register_notification_filter(Agent, Id, Mod, Args, {Loc, _Id} = Where) 
  when (Loc =:= insert_before) orelse (Loc =:= insert_after) ->
    case (catch verify_notification_filter_behaviour(Mod)) of
	ok ->
	    call(Agent, {register_notification_filter, Id, Mod, Args, Where});
	 Error ->
	    Error
    end.

verify_notification_filter_behaviour(Mod) ->
    snmp_misc:verify_behaviour(snmpa_notification_filter, Mod).

unregister_notification_filter(Agent, Id) ->
    call(Agent, {unregister_notification_filter, Id}).

which_notification_filter(Agent) ->
    call(Agent, which_notification_filter).


send_notification(Agent, Notification, SendOpts) ->
    Msg = {send_notif, Notification, SendOpts},
    maybe_call(Agent, Msg).
    
%% <BACKWARD-COMPAT>
send_trap(Agent, Trap, NotifyName, CtxName, Recv, Varbinds) ->
    ?d("send_trap -> entry with"
       "~n   self():        ~p"
       "~n   Agent:         ~p [~p]"
       "~n   Trap:          ~p"
       "~n   NotifyName:    ~p"
       "~n   CtxName:       ~p"
       "~n   Recv:          ~p"
       "~n   Varbinds:      ~p", 
       [self(), Agent, wis(Agent), 
	Trap, NotifyName, CtxName, Recv, Varbinds]),
    SendOpts = [
		{receiver, Recv},
		{varbinds, Varbinds}, 
		{name,     NotifyName},
		{context,  CtxName}, 
		{extra,    ?DEFAULT_NOTIF_EXTRA_INFO}
	       ],
    send_notification(Agent, Trap, SendOpts).
    
send_trap(Agent, Trap, NotifyName, CtxName, Recv, Varbinds, LocalEngineID) ->
    ?d("send_trap -> entry with"
       "~n   self():        ~p"
       "~n   Agent:         ~p [~p]"
       "~n   Trap:          ~p"
       "~n   NotifyName:    ~p"
       "~n   CtxName:       ~p"
       "~n   Recv:          ~p"
       "~n   Varbinds:      ~p" 
       "~n   LocalEngineID: ~p", 
       [self(), Agent, wis(Agent), 
	Trap, NotifyName, CtxName, Recv, Varbinds, LocalEngineID]),
    SendOpts = [
		{receiver,        Recv},
		{varbinds,        Varbinds}, 
		{name,            NotifyName},
		{context,         CtxName}, 
		{extra,           ?DEFAULT_NOTIF_EXTRA_INFO}, 
		{local_engine_id, LocalEngineID}
	       ],
    send_notification(Agent, Trap, SendOpts).
    
%% </BACKWARD-COMPAT>


%% -- Discovery functions --

disco_opts() ->
    case ets:lookup(snmp_agent_table, discovery) of
	[] -> 
	    [];
	[{discovery, DiscoOptions}] ->
	    DiscoOptions
    end.
    
originating_disco_opts() ->
    DiscoOpts = disco_opts(),
    case lists:keysearch(originating, 1, DiscoOpts) of
	{value, {originating, OrigDisco}} ->
	    OrigDisco;
	_ ->
	    []
    end.
	
is_originating_discovery_enabled() ->
    OrigDisco = originating_disco_opts(), 
    case lists:keysearch(enable, 1, OrigDisco) of
	{value, {enable, false}} ->
	    false;
	_ ->
	    true
    end.
    
terminating_disco_opts() ->
    DiscoOpts = disco_opts(),
    case lists:keysearch(terminating, 1, DiscoOpts) of
	{value, {terminating, TermDisco}} ->
	    TermDisco;
	_ ->
	    []
    end.
	
is_terminating_discovery_enabled() ->
    TermDisco = terminating_disco_opts(),
    case lists:keysearch(enable, 1, TermDisco) of
	{value, {enable, false}} ->
	    false;
	_ ->
	    true
    end.
    
terminating_discovery_stage2() ->
    Default   = discovery, 
    TermDisco = terminating_disco_opts(),
    case lists:keysearch(stage2, 1, TermDisco) of
	{value, {stage2, Stage2}} when ((Stage2 =:= discovery) orelse (Stage2 =:= plain)) ->
	    Stage2;
	_ ->
	    Default 
    end.

terminating_trigger_username() ->
    Default   = ?DISCO_TERMINATING_TRIGGER_USERNAME,
    TermDisco = terminating_disco_opts(),
    case lists:keysearch(trigger_username, 1, TermDisco) of
	{value, {trigger_username, Trigger}} when is_list(Trigger) ->
	    Trigger;
	_ ->
	    Default
    end.

    
discovery(TargetName, Notification, ContextName, Varbinds, 
	  DiscoHandler, ExtraInfo) ->
    case is_originating_discovery_enabled() of
	true ->
	    Agent = snmp_master_agent, 
	    call(Agent, 
		 {discovery, 
		  TargetName, Notification, ContextName, Varbinds, 
		  DiscoHandler, ExtraInfo});
	false ->
	    {error, not_enabled}
    end.

wis(Pid) when is_pid(Pid) ->
    Pid;
wis(Atom) when is_atom(Atom) ->
    whereis(Atom).


forward_trap(Agent, TrapRecord, NotifyName, CtxName, Recv, Varbinds, 
	     ExtraInfo) ->
    Agent ! {forward_trap, TrapRecord, NotifyName, CtxName, Recv, Varbinds, 
	     ExtraInfo}.


%%-----------------------------------------------------------------
%% Args: Vars = [Oid]
%% Returns: [Value]
%% Called from a program to get variables.  Don't call this from
%% an instrumentation function; deadlock can occur!
%%-----------------------------------------------------------------
get(Agent, Vars) -> 
    call(Agent, {get, Vars, ""}).

get(Agent, Vars, Context) -> 
    call(Agent, {get, Vars, Context}).

get_next(Agent, Vars) -> 
    call(Agent, {get_next, Vars, ""}).

get_next(Agent, Vars, Context) -> 
    call(Agent, {get_next, Vars, Context}).


%%-----------------------------------------------------------------

backup(Agent, BackupDir) when is_list(BackupDir) ->
    call(Agent, {backup, BackupDir}).


%%-----------------------------------------------------------------
%% Runtime debug support.
%%-----------------------------------------------------------------
dump_mibs(Agent) -> 
    call(Agent, dump_mibs).
dump_mibs(Agent, File) when is_list(File) -> 
    call(Agent, {dump_mibs, File}).


%%-----------------------------------------------------------------
%% Runtime debug (verbosity) support.
%%-----------------------------------------------------------------
verbosity(net_if,Verbosity) -> 
    cast(snmp_master_agent,{net_if_verbosity,Verbosity});
verbosity(mib_server,Verbosity) -> 
    cast(snmp_master_agent,{mib_server_verbosity,Verbosity});
verbosity(note_store,Verbosity) -> 
    cast(snmp_master_agent,{note_store_verbosity,Verbosity});
verbosity(sub_agents,Verbosity) -> 
    cast(snmp_master_agent,{sub_agents_verbosity,Verbosity});
verbosity(master_agent,Verbosity) -> 
    cast(snmp_master_agent,{verbosity,Verbosity});
verbosity(Agent,{sub_agents,Verbosity}) -> 
    cast(Agent,{sub_agents_verbosity,Verbosity});
verbosity(Agent,Verbosity) -> 
    cast(Agent,{verbosity,Verbosity}).


%%%--------------------------------------------------

get_agent_mib_storage() ->
    ets:lookup_element(snmp_agent_table, agent_mib_storage, 2).

db(Tab) ->
    {Tab, get_agent_mib_storage()}.


%%%--------------------------------------------------
%%% 2. Main loop
%%%--------------------------------------------------
%% gen_server:reply(From, Reply)
handle_info({discovery_response, Response}, S) ->
    ?vdebug("handle_info(discovery_response) -> entry with"
	    "~n   Response: ~p", [Response]),    
    NewS = handle_discovery_response(S, Response),
    {noreply, NewS};
    
handle_info({snmp_pdu, Vsn, Pdu, PduMS, ACMData, Address, Extra}, S) ->
    ?vdebug("handle_info(snmp_pdu) -> entry with"
	    "~n   Vsn:     ~p"
	    "~n   Pdu:     ~p"
	    "~n   Address: ~p"
	    "~n   Extra:   ~p", [Vsn, Pdu, Address, Extra]),
    
    NewS = handle_snmp_pdu(is_valid_pdu_type(Pdu#pdu.type),
			   Vsn, Pdu, PduMS, ACMData, Address, Extra, S),

    {noreply, NewS};

handle_info(worker_available, S) ->
    ?vdebug("worker available",[]),
    {noreply, S#state{worker_state = ready}};

handle_info({send_notif, Notification, SendOpts}, S) ->
    ?vlog("[handle_info] send notif request:"
	  "~n   Notification:  ~p"
	  "~n   SendOpts:      ~p", 
	  [Notification, SendOpts]),
    case (catch handle_send_trap(S, Notification, SendOpts)) of
	{ok, NewS} ->
	    {noreply, NewS};
	{'EXIT', R} ->
	    ?vinfo("Trap not sent:~n   ~p", [R]),
	    {noreply, S};
	_ ->
	    {noreply, S}
    end;

%% <BACKWARD-COMPAT>
handle_info({send_trap, Trap, NotifyName, ContextName, Recv, Varbinds}, S) ->
    ?vlog("[handle_info] send trap request:"
	  "~n   Trap:          ~p"
	  "~n   NotifyName:    ~p"
	  "~n   ContextName:   ~p"
	  "~n   Recv:          ~p" 
	  "~n   Varbinds:      ~p", 
	  [Trap, NotifyName, ContextName, Recv, Varbinds]),
    ExtraInfo     = ?DEFAULT_NOTIF_EXTRA_INFO, 
    LocalEngineID = local_engine_id(S),
    case (catch handle_send_trap(S, Trap, NotifyName, ContextName,
				 Recv, Varbinds, LocalEngineID, ExtraInfo)) of
	{ok, NewS} ->
	    {noreply, NewS};
	{'EXIT', R} ->
	    ?vinfo("Trap not sent:~n   ~p", [R]),
	    {noreply, S};
	_ ->
	    {noreply, S}
    end;

handle_info({send_trap, Trap, NotifyName, ContextName, Recv, Varbinds, 
	     LocalEngineID}, S) ->
    ?vlog("[handle_info] send trap request:"
	  "~n   Trap:          ~p"
	  "~n   NotifyName:    ~p"
	  "~n   ContextName:   ~p"
	  "~n   Recv:          ~p" 
	  "~n   Varbinds:      ~p" 
	  "~n   LocalEngineID: ~p", 
	  [Trap, NotifyName, ContextName, Recv, Varbinds, LocalEngineID]),
    ExtraInfo = ?DEFAULT_NOTIF_EXTRA_INFO, 
    case (catch handle_send_trap(S, Trap, NotifyName, ContextName,
				 Recv, Varbinds, LocalEngineID, ExtraInfo)) of
	{ok, NewS} ->
	    {noreply, NewS};
	{'EXIT', R} ->
	    ?vinfo("Trap not sent:~n   ~p", [R]),
	    {noreply, S};
	_ ->
	    {noreply, S}
    end;
%% </BACKWARD-COMPAT>

handle_info({forward_trap, TrapRecord, NotifyName, ContextName, 
	     Recv, Varbinds, ExtraInfo}, S) ->
    ?vlog("[handle_info] forward trap request:"
	  "~n   TrapRecord:    ~p"
	  "~n   NotifyName:    ~p"
	  "~n   ContextName:   ~p"
	  "~n   Recv:          ~p"
	  "~n   Varbinds:      ~p", 
	  [TrapRecord, NotifyName, ContextName, Recv, Varbinds]),
    LocalEngineID = ?DEFAULT_LOCAL_ENGINE_ID, 
    case (catch maybe_send_trap(S, TrapRecord, NotifyName, ContextName,
				Recv, Varbinds, LocalEngineID, ExtraInfo)) of
	{ok, NewS} ->
	    {noreply, NewS};
	{'EXIT', R} ->
	    ?vinfo("Trap not sent:~n   ~p", [R]),
	    {noreply, S};
	_ ->
	    {noreply, S}
    end;

%% <BACKWARD-COMPAT>
handle_info({forward_trap, TrapRecord, NotifyName, ContextName, 
	     Recv, Varbinds}, S) ->
    ?vlog("[handle_info] forward trap request:"
	  "~n   TrapRecord:    ~p"
	  "~n   NotifyName:    ~p"
	  "~n   ContextName:   ~p"
	  "~n   Recv:          ~p"
	  "~n   Varbinds:      ~p", 
	  [TrapRecord, NotifyName, ContextName, Recv, Varbinds]),
    ExtraInfo     = ?DEFAULT_NOTIF_EXTRA_INFO, 
    LocalEngineID = ?DEFAULT_LOCAL_ENGINE_ID, 
    case (catch maybe_send_trap(S, TrapRecord, NotifyName, ContextName,
				Recv, Varbinds, LocalEngineID, ExtraInfo)) of
	{ok, NewS} ->
	    {noreply, NewS};
	{'EXIT', R} ->
	    ?vinfo("Trap not sent:~n   ~p", [R]),
	    {noreply, S};
	_ ->
	    {noreply, S}
    end;
%% </BACKWARD-COMPAT>

handle_info({backup_done, Reply}, #state{backup = {_, From}} = S) ->
    ?vlog("[handle_info] backup done:"
	  "~n   Reply: ~p", [Reply]),
    gen_server:reply(From, Reply),
    {noreply, S#state{backup = undefined}};


handle_info(invalidate_ca_cache, S) ->
    invalidate_ca_cache(),
    {noreply, S};


%%-----------------------------------------------------------------
%% If a process crashes, we first check to see if it was the mib,
%% net-if or note-store.
%% Otherwise, we check to see if it was a subagent. In this case
%% we unregister the sa, and unlink us from the sa.
%%-----------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #state{note_store = Pid} = S) ->
    ?vlog("note store (~p) exited for reason ~n~p", [Pid, Reason]),
    error_msg("note-store exited: ~n~p", [Reason]),
    {stop, {note_store_exit, Reason}, S#state{note_store = undefined}};
handle_info({'EXIT', Pid, Reason}, #state{worker = Pid} = S) ->
    ?vlog("worker (~p) exited -> create new ~n   ~p", [Pid, Reason]),
    NewWorker = worker_start(), 
    {noreply, S#state{worker = NewWorker}};
handle_info({'EXIT', Pid, Reason}, #state{set_worker = Pid} = S) ->
    ?vlog("set-worker (~p) exited -> create new ~n   ~p", [Pid,Reason]),
    NewWorker = set_worker_start(), 
    {noreply, S#state{set_worker = NewWorker}};
handle_info({'EXIT', Pid, Reason}, #state{parent = Pid} = S) ->
    ?vlog("parent (~p) exited for reason ~n~p", [Pid,Reason]),
    {stop, {parent_died, Reason}, S};
handle_info({'EXIT', Pid, Reason}, #state{backup = {Pid, From}} = S) ->
    ?vlog("backup server (~p) exited for reason ~n~p", [Pid, Reason]),
    case Reason of
	normal ->
	    {noreply, S};
	_ ->
	    gen_server:reply(From, {error, Reason}),
	    {noreply, S#state{backup = undefined}}
    end;
handle_info({'EXIT', Pid, Reason}, S) ->
    ?vlog("~p exited for reason ~p", [Pid, Reason]),
    Mib   = get(mibserver),
    NetIf = get(net_if),
    case Pid of
	Mib ->
	    error_msg("mib-server exited: ~n~p", [Reason]),
	    {stop, {mib_server_exit, Reason}, S};
	NetIf ->
	    error_msg("net-if exited: ~n~p", [Reason]),
	    {stop, {net_if_exit, Reason}, S};
	_ ->
	    %% Could be a sub-agent
	    SAs = snmpa_mib:info(Mib, subagents),
	    case lists:keysearch(Pid, 1, SAs) of
		{value, _} ->
		    ?vlog("subagent exit", []),
		    snmpa_mib:unregister_subagent(Mib, Pid),
		    unlink(Pid);
		_ -> 
		    %% Otherwise it was probably a worker thread - ignore
		    ok
	    end,
	    {noreply, S}
    end;

handle_info({'DOWN', Ref, process, Pid, {mibs_cache_reply, Reply}}, 
	    #state{mibs_cache_request = {Pid, Ref, From}} = S) ->
    ?vlog("reply from the mibs cache request handler (~p): ~n~p", 
	  [Pid, Reply]),
    gen_server:reply(From, Reply),
    {noreply, S#state{mibs_cache_request = undefined}};

handle_info(Info, S) ->
    warning_msg("received unexpected info: ~n~p", [Info]),
    {noreply, S}.

handle_call(restart_worker, _From, #state{worker = Pid} = S) ->
    if 
	is_pid(Pid) ->
	    ?vlog("[handle_call] restart worker ~p", [Pid]),
	    exit(Pid, kill);
	true ->
	    ?vlog("[handle_call] not multi-threaded => "
		  "ignoring restart request", []),
	    ok
    end,
    {reply, ok, S};
handle_call(restart_set_worker, _From, #state{set_worker = Pid} = S) ->
    if
	is_pid(Pid) ->
	    ?vlog("[handle_call] restart set worker: ~p", [Pid]),
	    exit(Pid, kill);
	true ->
	    ?vlog("[handle_call] not multi-threaded => "
		  "ignoring restart request", []),
	    ok
    end,
    {reply, ok, S};

handle_call({send_notif, Notification, SendOpts}, _From, S) ->
    ?vlog("[handle_call] send notif request:"
	  "~n   Notification:  ~p"
	  "~n   SendOpts:      ~p", 
	  [Notification, SendOpts]),
    case (catch handle_send_trap(S, Notification, SendOpts)) of
	{ok, NewS} ->
	    {reply, ok, NewS};
	{'EXIT', Reason} ->
	    ?vinfo("Trap not sent:~n   ~p", [Reason]),
	    {reply, {error, {send_failed, Reason}}, S};
	_ ->
	    ?vinfo("Trap not sent", []),
	    {reply, {error, send_failed}, S}
    end;

%% <BACKWARD-COMPAT>
handle_call({send_trap, Trap, NotifyName, ContextName, Recv, Varbinds}, 
	    _From, S) ->
    ?vlog("[handle_call] send trap request:"
	  "~n   Trap:          ~p"
	  "~n   NotifyName:    ~p"
	  "~n   ContextName:   ~p"
	  "~n   Recv:          ~p" 
	  "~n   Varbinds:      ~p", 
	  [Trap, NotifyName, ContextName, Recv, Varbinds]),
    ExtraInfo     = ?DEFAULT_NOTIF_EXTRA_INFO, 
    LocalEngineID = local_engine_id(S),
    case (catch handle_send_trap(S, Trap, NotifyName, ContextName,
				 Recv, Varbinds, LocalEngineID, ExtraInfo)) of
	{ok, NewS} ->
	    {reply, ok, NewS};
	{'EXIT', Reason} ->
	    ?vinfo("Trap not sent:~n   ~p", [Reason]),
	    {reply, {error, {send_failed, Reason}}, S};
	_ ->
	    ?vinfo("Trap not sent", []),
	    {reply, {error, send_failed}, S}
    end;

handle_call({send_trap, Trap, NotifyName, 
	     ContextName, Recv, Varbinds, LocalEngineID}, 
	    _From, S) ->
    ?vlog("[handle_call] send trap request:"
	  "~n   Trap:          ~p"
	  "~n   NotifyName:    ~p"
	  "~n   ContextName:   ~p"
	  "~n   Recv:          ~p" 
	  "~n   Varbinds:      ~p" 
	  "~n   LocalEngineID: ~p", 
	  [Trap, NotifyName, ContextName, Recv, Varbinds, LocalEngineID]),
    ExtraInfo = ?DEFAULT_NOTIF_EXTRA_INFO, 
    case (catch handle_send_trap(S, Trap, NotifyName, ContextName,
				 Recv, Varbinds, LocalEngineID, ExtraInfo)) of
	{ok, NewS} ->
	    {reply, ok, NewS};
	{'EXIT', Reason} ->
	    ?vinfo("Trap not sent:~n   ~p", [Reason]),
	    {reply, {error, {send_failed, Reason}}, S};
	_ ->
	    ?vinfo("Trap not sent", []),
	    {reply, {error, send_failed}, S}
    end;
%% </BACKWARD-COMPAT>

handle_call({discovery, 
	     TargetName, Notification, ContextName, Vbs, DiscoHandler, 
	     ExtraInfo}, 
	    From, 
	    #state{disco = undefined} = S) ->
    ?vlog("[handle_call] initiate discovery process:"
	  "~n   TargetName:   ~p"
	  "~n   Notification: ~p"
	  "~n   ContextName:  ~p"
	  "~n   Vbs:          ~p"
	  "~n   DiscoHandler: ~p"
	  "~n   ExtraInfo:    ~p", 
	  [TargetName, Notification, ContextName, Vbs, 
	   DiscoHandler, ExtraInfo]),
    case handle_discovery(S, From, TargetName, 
			  Notification, ContextName, Vbs, DiscoHandler, 
			  ExtraInfo) of
	{ok, NewS} ->
	    ?vtrace("[handle_call] first stage of discovery process initiated",
		    []),
	    {noreply, NewS};
	{error, _} = Error ->
	    {reply, Error, S}
    end;
handle_call({discovery, _TargetName, _Notification, _ContextName, _Vbs, _DiscoHandler, _ExtraInfo}, _From, 
	    #state{disco = DiscoData} = S) ->
    Reply = {error, {discovery_in_progress, DiscoData}}, 
    {reply, Reply, S};
handle_call({subagent_get, Varbinds, PduData, IsNotification}, _From, S) ->
    ?vlog("[handle_call] subagent get:"
	  "~n   Varbinds: ~p"
	  "~n   PduData:  ~p", 
	  [Varbinds,PduData]),
    put_pdu_data(PduData),
    {reply, do_get(Varbinds, IsNotification), S};
handle_call({subagent_get_next, MibView, Varbinds, PduData}, _From, S) ->
    ?vlog("[handle_call] subagent get-next:"
	  "~n   MibView:  ~p"
	  "~n   Varbinds: ~p"
	  "~n   PduData:  ~p", 
	  [MibView,Varbinds,PduData]),
    put_pdu_data(PduData),
    {reply, do_get_next(MibView, Varbinds), S};
handle_call({subagent_set, Arguments, PduData}, _From, S) ->
    ?vlog("[handle_call] subagent set:"
	  "~n   Arguments: ~p"
	  "~n   PduData:   ~p", 
	  [Arguments,PduData]),
    put_pdu_data(PduData),
    {reply, do_subagent_set(Arguments), S};

handle_call({get, Vars, Context}, _From, S) ->
    ?vlog("[handle_call] get:"
	  "~n   Vars:    ~p"
	  "~n   Context: ~p", [Vars, Context]),
    put_pdu_data({undefined, undefined, undefined, undefined, Context}),
    case catch mapfoldl(fun ?MODULE:tr_var/2, [], 1, Vars) of
	{error, Reason} -> {reply, {error, Reason}, S};
	{_, Varbinds} ->
	    ?vdebug("Varbinds: ~p",[Varbinds]),
	    Reply =
		case do_get(Varbinds, false) of
		    {noError, 0, NewVarbinds} ->
			Vbs = lists:keysort(#varbind.org_index, NewVarbinds),
			[Value || #varbind{value = Value} <- Vbs];
		    {ErrorStatus, ErrIndex, _} ->
			N = lists:nth(ErrIndex, Vars),
			{error, {ErrorStatus, N}}
		end,
	    {reply, Reply, S}
    end;

handle_call({get_next, Vars, Context}, _From, S) ->
    ?vlog("[handle_call] get_next:"
          "~n   Vars:    ~p"
          "~n   Context: ~p",[Vars, Context]),
    put_pdu_data({undefined, undefined, undefined, undefined, Context}),
    case catch mapfoldl(fun ?MODULE:tr_var/2, [], 1, Vars) of
        {error, Reason} -> {reply, {error, Reason}, S};
        {_, Varbinds} ->
            ?vdebug("Varbinds: ~p",[Varbinds]),
            MibView = snmpa_acm:get_root_mib_view(),
            Reply =
                case do_get_next(MibView, Varbinds) of
                    {noError, 0, NewVarbinds} ->
                        Vbs = lists:keysort(#varbind.org_index, NewVarbinds),
			[{Oid,Val} || #varbind{oid = Oid, value = Val} <- Vbs];
                    {ErrorStatus, ErrIndex, _} ->
                        N = lists:nth(ErrIndex, Vars),
                        {error, {ErrorStatus, N}}
                end,
            {reply, Reply, S}
    end;

handle_call({do_get, MibView, UnsortedVarbinds, IsNotification, PduData}, 
	    _From, S) ->
    ?vlog("[handle_call] do_get:"
	  "~n   MibView:          ~p"
	  "~n   UnsortedVarbinds: ~p"
	  "~n   IsNotification:   ~p" 
	  "~n   PduData:          ~p", 
	  [MibView, UnsortedVarbinds, IsNotification, PduData]),
    put_pdu_data(PduData),
    Reply = do_get(MibView, UnsortedVarbinds, IsNotification),
    {reply, Reply, S};

handle_call({register_subagent, SubTreeOid, SubagentPid}, _From, S) ->
    Reply = 
	case snmpa_mib:register_subagent(get(mibserver),
					SubTreeOid, SubagentPid) of
	    ok -> link(SubagentPid), ok;
	    Error -> Error
	end,
    {reply, Reply, S};

handle_call({unregister_subagent, SubagentPid}, _From, S) 
  when is_pid(SubagentPid) ->
    ?vlog("[handle_call] unregister subagent ~p", [SubagentPid]),
    Reply = snmpa_mib:unregister_subagent(get(mibserver), SubagentPid),
    unlink(SubagentPid),
    {reply, Reply, S};

handle_call({unregister_subagent, SubTreeOid}, _From, S) ->
    ?vlog("[handle_call] unregister subagent ~p", [SubTreeOid]),
    Reply = 
	case snmpa_mib:unregister_subagent(get(mibserver), SubTreeOid) of
	    {ok, DeletedSubagentPid} ->
		SAs = snmpa_mib:info(get(mibserver), subagents),
		case lists:keysearch(DeletedSubagentPid, 1, SAs) of
		    {value, _} -> ok;
		    _ -> unlink(DeletedSubagentPid)
		end,
		ok;
	    Error ->
		Error
	end,
    {reply, Reply, S};

%% <BACKWARD-COMPAT>
handle_call({load_mibs, Mibs}, _From, S) ->
    ?vlog("load mibs ~p", [Mibs]),
    {reply, snmpa_mib:load_mibs(get(mibserver), Mibs), S};
%% </BACKWARD-COMPAT>

handle_call({load_mibs, Mibs, Force}, _From, S) ->
    ?vlog("[~w] load mibs ~p", [Force, Mibs]),
    {reply, snmpa_mib:load_mibs(get(mibserver), Mibs, Force), S};

%% <BACKWARD-COMPAT>
handle_call({unload_mibs, Mibs}, _From, S) ->
    ?vlog("unload mibs ~p", [Mibs]),
    {reply, snmpa_mib:unload_mibs(get(mibserver), Mibs), S};
%% </BACKWARD-COMPAT>

handle_call({unload_mibs, Mibs, Force}, _From, S) ->
    ?vlog("[~w] unload mibs ~p", [Force, Mibs]),
    {reply, snmpa_mib:unload_mibs(get(mibserver), Mibs, Force), S};

handle_call(which_mibs, _From, S) ->
    ?vlog("which mibs", []),
    {reply, snmpa_mib:which_mibs(get(mibserver)), S};

handle_call({whereis_mib, Mib}, _From, S) ->
    ?vlog("whereis mib ~p", [Mib]),
    {reply, snmpa_mib:whereis_mib(get(mibserver), Mib), S};

handle_call({mibs_cache_request, MibsCacheReq}, From, S) ->
    ?vlog("mibs_cache_request: ~p", [MibsCacheReq]),
    {MibsCacheWorker, Ref} = 
	handle_mibs_cache_request(get(mibserver), MibsCacheReq),
    NewS = S#state{mibs_cache_request = {MibsCacheWorker, Ref, From}},
    {noreply, NewS};

handle_call(info, _From, S) ->
    ?vlog("info", []),
    Vsns  = S#state.vsns,
    Stats = get_stats_counters(), 
    AI    = agent_info(S),
    NI    = net_if_info(S),
    NS    = note_store_info(S),
    SS    = symbolic_store_info(),
    LD    = local_db_info(),
    MS    = mib_server_info(), 
    Info  = [{vsns,           Vsns}, 
	     {stats_counters, Stats},
	     {agent,          AI}, 
	     {net_if,         NI}, 
	     {note_store,     NS},
	     {symbolic_store, SS},
	     {local_db,       LD},
	     {mib_server,     MS}], 
    {reply, Info, S};

handle_call(get_net_if, _From, S) ->
    {reply, get(net_if), S};

%% Only accept a backup request if there is none already in progress
handle_call({backup, BackupDir}, From, #state{backup = undefined} = S) ->
    ?vlog("backup: ~p", [BackupDir]),
    Pid = self(),
    V   = get(verbosity),
    MS  = get(mibserver),
    BackupServer = 
	erlang:spawn_link(
	  fun() ->
		  put(sname, abs),
		  put(verbosity, V),
		  Dir   = filename:join([BackupDir]),
		  Reply = handle_backup(Dir, MS),
		  Pid ! {backup_done, Reply},
		  unlink(Pid)
	  end),
    ?vtrace("backup server: ~p", [BackupServer]),
    {noreply, S#state{backup = {BackupServer, From}}};

handle_call({backup, _BackupDir}, _From, #state{backup = Backup} = S) ->
    ?vinfo("backup already in progress: ~p", [Backup]),
    {reply, {error, backup_in_progress}, S};

handle_call(dump_mibs, _From, S) ->
    Reply = snmpa_mib:dump(get(mibserver)),
    {reply, Reply, S};
    
handle_call({dump_mibs,File}, _From, S) ->
    Reply = snmpa_mib:dump(get(mibserver),File),
    {reply, Reply, S};
    
handle_call({register_notification_filter, Id, Mod, Data, Where}, _From, 
	    #state{nfilters = NFs} = S) ->
    ?vlog("register_notification_filter -> "
	  "~n   Id:    ~p"
	  "~n   Mod:   ~p"
	  "~n   Where: ~p", [Id, Mod, Where]),
    case lists:keymember(Id, 2, NFs) of
	true ->
	    {reply, {error, {already_registered, Id}}, S};
	false ->
	    NF = #notification_filter{id = Id, mod = Mod, data = Data},
	    {Reply, NewNFs} = add_notification_filter(Where, NF, NFs),
	    {reply, Reply, S#state{nfilters = NewNFs}}
    end;

handle_call({unregister_notification_filter, Id}, _From, 
	    #state{nfilters = NFs} = S) ->
    ?vlog("unregister_notification_filter -> "
	  "~n   Id: ~p", [Id]),
    case lists:keydelete(Id, 2, NFs) of
	NFs ->
	    {reply, {error, {not_found, Id}}, S};
	NFs2 ->
	    {reply, ok, S#state{nfilters = NFs2}}
    end;

handle_call(which_notification_filter, _From, 
	    #state{nfilters = NFs} = S) ->
    ?vlog("which_notification_filter", []),
    {reply, [Id || #notification_filter{id = Id} <- NFs], S};

handle_call({mib_of, Oid}, _From, S) ->
    Reply = handle_mib_of(get(mibserver), Oid),
    {reply, Reply, S};

handle_call({me_of, Oid}, _From, S) ->
    Reply = handle_me_of(get(mibserver), Oid),
    {reply, Reply, S};

handle_call(get_log_type, _From, S) ->
    ?vlog("handle_call(get_log_type) -> entry with", []),
    Reply = handle_get_log_type(S), 
    {reply, Reply, S};
    
handle_call({set_log_type, NewType}, _From, S) ->
    ?vlog("handle_call(set_log_type) -> entry with"
	  "~n   NewType: ~p", [NewType]),
    Reply = handle_set_log_type(S, NewType), 
    {reply, Reply, S};
    
handle_call(get_request_limit, _From, S) ->
    ?vlog("handle_call(get_request_limit) -> entry with", []),
    Reply = handle_get_request_limit(S), 
    {reply, Reply, S};
    
handle_call({set_request_limit, NewLimit}, _From, S) ->
    ?vlog("handle_call(set_request_limit) -> entry with"
	  "~n   NewLimit: ~p", [NewLimit]),
    Reply = handle_set_request_limit(S, NewLimit), 
    {reply, Reply, S};

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};

handle_call(Req, _From, S) ->
    warning_msg("received unknown request: ~n~p", [Req]),
    Reply = {error, {unknown, Req}}, 
    {reply, Reply, S}.
    
handle_cast({verbosity, Verbosity}, S) ->
    ?vlog("verbosity: ~p -> ~p",[get(verbosity), Verbosity]),
    put(verbosity,snmp_verbosity:validate(Verbosity)),
    case S#state.worker of
	Pid when is_pid(Pid) -> Pid ! ?mk_verbosity_wreq(Verbosity);
	_ -> ok
    end,
    case S#state.set_worker of
	Pid2 when is_pid(Pid2) -> Pid2 ! ?mk_verbosity_wreq(Verbosity);
	_ -> ok
    end,
    {noreply, S};
    
handle_cast({sub_agents_verbosity,Verbosity}, S) ->
    ?vlog("sub_agents verbosity: ~p",[Verbosity]),
    subagents_verbosity(Verbosity),
    {noreply, S};
    
%% This should only happen if we are a master_agent
handle_cast({net_if_verbosity, Verbosity}, S) ->
    net_if_verbosity(get(net_if), Verbosity),
    {noreply, S};
    
handle_cast({mib_server_verbosity, Verbosity}, S) ->
    mib_server_verbosity(get(mibserver),Verbosity),
    {noreply, S};
    
handle_cast({note_store_verbosity, Verbosity}, #state{note_store = Pid} = S) ->
    note_store_verbosity(Pid,Verbosity),
    {noreply, S};
    
handle_cast(Msg, S) ->
    warning_msg("received unknown message: ~n~p", [Msg]),
    {noreply, S}.

    
terminate(shutdown, #state{worker     = Worker,
			   set_worker = SetWorker,
			   backup     = Backup, 
			   ref = Ref}) ->
    %% Ordered shutdown - stop misc-workers, net_if, mib-server and note-store.
    backup_server_stop(Backup), 
    worker_stop(Worker, 100),
    worker_stop(SetWorker, 100),
    snmpa_misc_sup:stop_net_if(Ref), 
    snmpa_misc_sup:stop_mib_server(Ref);
terminate(_Reason, _S) ->
    %% We crashed!  We will reuse net_if and mib if we get restarted.
    ok.


handle_mibs_cache_request(MibServer, Req) ->
    {MibsCacheWorker, MibsCacheRef} = 
	spawn_monitor(
	  fun() -> 
		  Reply = 
		      case Req of
			  invalidate_cache ->
			      snmpa_mib:invalidate_cache(MibServer);
			  gc_cache ->
			      snmpa_mib:gc_cache(MibServer);
			  {gc_cache, Age} ->
			      snmpa_mib:gc_cache(MibServer, Age);
			  {gc_cache, Age, GcLimit} ->
			      snmpa_mib:gc_cache(MibServer, Age, GcLimit);
			  cache_size ->
			      snmpa_mib:which_cache_size(MibServer);
			  enable_cache ->
			      snmpa_mib:enable_cache(MibServer);
			  disable_cache ->
			      snmpa_mib:disable_cache(MibServer);
			  enable_autogc ->
			      snmpa_mib:enable_cache_autogc(MibServer);
			  disable_autogc ->
			      snmpa_mib:disable_cache_autogc(MibServer);
			  {update_gclimit, GcLimit} ->
			      snmpa_mib:update_cache_gclimit(MibServer, 
							      GcLimit);
			  {update_age, Age} ->
			      snmpa_mib:update_cache_age(MibServer, Age);
			  _ ->
			      {error, {unknown_mibs_cache_request, Req}}
		      end,
		  exit({mibs_cache_reply, Reply})
	  end),
    {MibsCacheWorker, MibsCacheRef}.

		        
%%-----------------------------------------------------------------
%% Code replacement
%% 
%%-----------------------------------------------------------------

%% Downgrade
%%
%% code_change({down, _Vsn}, S1, downgrade_to_pre_4_17_3) ->
%%     #state{type               = Type, 
%% 	   parent             = Parent, 
%% 	   worker             = Worker, 
%% 	   worker_state       = WorkerState,
%% 	   set_worker         = SetWorker, 
%% 	   multi_threaded     = MT, 
%% 	   ref                = Ref, 
%% 	   vsns               = Vsns,
%% 	   nfilters           = NF,
%% 	   note_store         = NoteStore,
%% 	   mib_server         = MS, 
%% 	   net_if             = NetIf, 
%% 	   net_if_mod         = NetIfMod, 
%% 	   backup             = Backup,
%% 	   disco              = Disco,
%% 	   mibs_cache_request = MCR} = S1, 
%%     S2 = {state, 
%% 	  type               = Type, 
%% 	  parent             = Parent, 
%% 	  worker             = Worker, 
%% 	  worker_state       = WorkerState,
%% 	  set_worker         = SetWorker, 
%% 	  multi_threaded     = MT, 
%% 	  ref                = Ref, 
%% 	  vsns               = Vsns,
%% 	  nfilters           = NF,
%% 	  note_store         = NoteStore,
%% 	  mib_server         = MS, 
%% 	  net_if             = NetIf, 
%% 	  net_if_mod         = NetIfMod, 
%% 	  backup             = Backup,
%% 	  disco              = Disco,
%% 	  mibs_cache_request = MCR}, 
%%     {ok, S2};

%% %% Upgrade
%% %%
%% code_change(_Vsn, S1, upgrade_from_pre_4_17_3) ->
%%     {state, 
%%      type               = Type, 
%%      parent             = Parent, 
%%      worker             = Worker, 
%%      worker_state       = WorkerState,
%%      set_worker         = SetWorker, 
%%      multi_threaded     = MT, 
%%      ref                = Ref, 
%%      vsns               = Vsns,
%%      nfilters           = NF,
%%      note_store         = NoteStore,
%%      mib_server         = MS, 
%%      net_if             = NetIf, 
%%      net_if_mod         = NetIfMod, 
%%      backup             = Backup,
%%      disco              = Disco,
%%      mibs_cache_request = MCR} = S1,
%%     S2 = #state{type               = Type, 
%% 		parent             = Parent, 
%% 		worker             = Worker, 
%% 		worker_state       = WorkerState,
%% 		set_worker         = SetWorker, 
%% 		multi_threaded     = MT, 
%% 		ref                = Ref, 
%% 		vsns               = Vsns,
%% 		nfilters           = NF,
%% 		note_store         = NoteStore,
%% 		mib_server         = MS, 
%% 		net_if             = NetIf, 
%% 		net_if_mod         = NetIfMod, 
%% 		backup             = Backup,
%% 		disco              = Disco,
%% 		mibs_cache_request = MCR,
%% 		gb_max_vbs         = ?DEFAULT_GB_MAX_VBS}, 
%%     {ok, S2};

code_change(_Vsn, S, _Extra) ->
    {ok, S}.


%% workers_restart(#state{worker = W, set_worker = SW} = S) ->
%%     Worker    = worker_restart(W),
%%     SetWorker = set_worker_restart(SW),
%%     S#state{worker     = Worker, 
%% 	    set_worker = SetWorker}.


%%-----------------------------------------------------------------

backup_server_stop({Pid, _}) when is_pid(Pid) ->
    exit(Pid, kill);
backup_server_stop(_) ->
    ok.


workers_start(true) ->
    ?vdebug("start worker and set-worker",[]),
    {worker_start(), set_worker_start()};
workers_start(_) ->
    {undefined, undefined}.

worker_start() ->
    worker_start(get()).

set_worker_start() ->
    worker_start([{master, self()} | get()]).

worker_start(Dict) ->
    proc_lib:spawn_link(?MODULE, worker, [self(), Dict]).

%% worker_stop(Pid) ->
%%     worker_stop(Pid, infinity).

worker_stop(Pid, Timeout) when is_pid(Pid) ->
    Pid ! ?mk_terminate_wreq(), 
    receive 
	{'EXIT', Pid, normal} ->
	    ok
    after Timeout ->
	    (catch exit(Pid, kill)),
	    ok
    end;
worker_stop(_, _) ->
    ok.

%% set_worker_restart(Pid) ->
%%     worker_restart(Pid, [{master, self()} | get()]).

%% worker_restart(Pid) ->
%%     worker_restart(Pid, get()).

%% worker_restart(Pid, Dict) when is_pid(Pid) -> 
%%     worker_stop(Pid),
%%     worker_start(Dict);
%% worker_restart(Any, _Dict) ->
%%     Any.


%%-----------------------------------------------------------------

handle_backup(BackupDir, MibServer) ->
    ?vlog("handle_backup -> entry with"
	  "~n   BackupDir: ~p", [BackupDir]),
    case ets:lookup(snmp_agent_table, db_dir) of
	[{db_dir, BackupDir}] ->
	    ?vinfo("handle_backup -> backup dir and db dir the same", []),
	    {error, db_dir};
	_ ->
	    case file:read_file_info(BackupDir) of
		{ok, #file_info{type = directory}} ->
		    ?vdebug("handle_backup -> backup dir ok", []),

		    VacmRes = (catch snmpa_vacm:backup(BackupDir)),
		    ?vtrace("handle_backup -> "
			    "~n   VacmRes: ~p", [VacmRes]),

		    LdbRes  = (catch snmpa_local_db:backup(BackupDir)),
		    ?vtrace("handle_backup -> "
			    "~n   LdbRes: ~p", [LdbRes]),

		    MsRes   = (catch snmpa_mib:backup(MibServer, BackupDir)),
		    ?vtrace("handle_backup -> "
			    "~n   MsRes: ~p", [MsRes]),

		    SsRes   = (catch snmpa_symbolic_store:backup(BackupDir)),
		    ?vtrace("handle_backup -> "
			    "~n   SsRes: ~p", [SsRes]),
		    handle_backup_res([{vacm,           VacmRes}, 
				       {local_db,       LdbRes},
				       {mib_server,     MsRes},
				       {symbolic_store, SsRes}]);
		{ok, _} ->
		    ?vinfo("handle_backup -> backup dir not a dir", []),
		    {error, not_a_directory};
		Error ->
		    ?vinfo("handle_backup -> Error: ~p", [Error]),
		    Error
	    end
    end.


handle_backup_res(Results) ->
    handle_backup_res(Results, []).

handle_backup_res([], []) ->
    ok;
handle_backup_res([], Acc) ->
    {error, lists:reverse(Acc)};
handle_backup_res([{_, ok}|Results], Acc) ->
    handle_backup_res(Results, Acc);
handle_backup_res([{Who, {error, Reason}}|Results], Acc) ->
    handle_backup_res(Results, [{Who, Reason}|Acc]);
handle_backup_res([{Who, Crap}|Results], Acc) ->
    handle_backup_res(Results, [{Who, Crap}|Acc]).


%%-----------------------------------------------------------------
%% We must cheat to get the community string out of the ACM data,
%% because we (for some reason) support the function
%% snmpa:current_community().
%%-----------------------------------------------------------------
cheat({community, _SecModel, Community, _TAddress}, Address, ContextName) ->
    {Community, Address, ContextName};
cheat({community, _SecModel, Community, _TDomain, _TAddress}, 
      Address, ContextName) ->
    {Community, Address, ContextName};
cheat(_, Address, ContextName) ->
    {"", Address, ContextName}.


%% This function will either be in the context of the:
%% 1) master-agent
%% 2) set-worker
%% 3) user code 
%%    ( calling e.g. snmp_community_mib:snmpCommunityTable(set, ...) )
invalidate_ca_cache() ->
    case get(master) of
        undefined -> % 1 or 3
            case get(auth_module) of
		undefined -> % 3
		    %% Ouch, we are not running in the context of the 
		    %% master agent either. Check if we are on the 
		    %% master-agent node. If so, sent it there,
		    case whereis(snmp_master_agent) of
			MasterAgent when is_pid(MasterAgent) ->
			    case node(MasterAgent) =:= node() of
				true ->
				    %% Ok, we are on the node running the
				    %% master_agent process, so sent it there
				    MasterAgent ! invalidate_ca_cache;
				false ->
				    %% This is running on a sub-agent node, 
				    %% so skip it
				    ok
			    end;
			_ -> % Not on this node 
			    ok
		    end;
		AuthMod -> % 1
		    AuthMod:invalidate_ca_cache()
	    end;
        Pid -> % 2
            Pid ! invalidate_ca_cache
    end.


%%-----------------------------------------------------------------
%% Threads and workers
%% 
%%-----------------------------------------------------------------

%% This functions spawns a temporary worker process, 
%% that evaluates one request and then silently exits. 
spawn_thread(Vsn, Pdu, PduMS, ACMData, Address, GbMaxVBs, Extra) ->
    Dict = get(),
    Args = [Vsn, Pdu, PduMS, ACMData, Address, GbMaxVBs, Extra, Dict], 
    proc_lib:spawn_link(?MODULE, handle_pdu, Args).

spawn_trap_thread(TrapRec, NotifyName, ContextName, Recv, Vbs, 
		  LocalEngineID, ExtraInfo) ->
    Dict = get(),
    proc_lib:spawn_link(?MODULE, do_send_trap,
			[TrapRec, NotifyName, ContextName, 
			 Recv, Vbs, LocalEngineID, ExtraInfo, Dict]).

do_send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, 
	     LocalEngineID, Dict) ->
    ExtraInfo = ?DEFAULT_NOTIF_EXTRA_INFO, 
    do_send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, 
		 LocalEngineID, ExtraInfo, Dict).
do_send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, 
	     LocalEngineID, ExtraInfo, Dict) ->
    lists:foreach(fun({Key, Val}) -> put(Key, Val) end, Dict),
    put(sname, trap_sender_short_name(get(sname))),
    ?vlog("starting",[]),
    snmpa_trap:send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, 
			 LocalEngineID, ExtraInfo, get(net_if)).

worker(Master, Dict) ->
    lists:foreach(fun({Key, Val}) -> put(Key, Val) end, Dict),
    put(sname, worker_short_name(get(sname))),
    ?vlog("starting",[]),
    worker_loop(Master).

worker_loop(Master) ->
    Res = 
	receive
	    #wrequest{cmd  = handle_pdu, 
		      info = Info} = Req ->
		?vtrace("worker_loop -> received handle_pdu request with"
			"~n   Info: ~p", [Info]),
		Vsn      = proplists:get_value(vsn,        Info),
		Pdu      = proplists:get_value(pdu,        Info),
		PduMS    = proplists:get_value(pdu_ms,     Info),
		ACMData  = proplists:get_value(acm_data,   Info),
		Address  = proplists:get_value(addr,       Info),
		GbMaxVBs = proplists:get_value(gb_max_vbs, Info),
		Extra    = proplists:get_value(extra,      Info),
		HandlePduRes = 
		    try 
			begin
			    handle_pdu2(Vsn, Pdu, PduMS, ACMData, Address, 
					GbMaxVBs, Extra)
			end
		    catch 
			C:E:S ->
			    exit({worker_crash, Req, C, E, S})
		    end,
		Master ! worker_available,
		HandlePduRes; % For debugging...
	    
	    
	    #wrequest{cmd  = send_trap, 
		      info = Info} = Req ->
		?vtrace("worker_loop -> received send_trap request with"
			"~n   Info: ~p", [Info]),
		TrapRec       = proplists:get_value(trap_rec,        Info),
		NotifyName    = proplists:get_value(notify_name,     Info),
		ContextName   = proplists:get_value(context_name,    Info),
		Recv          = proplists:get_value(receiver,        Info),
		Vbs           = proplists:get_value(varbinds,        Info),
		LocalEngineID = proplists:get_value(local_engine_id, Info),
		Extra         = proplists:get_value(extra,           Info),
		SendTrapRes = 
		    try
			begin
			    snmpa_trap:send_trap(TrapRec, NotifyName, 
						 ContextName, Recv, Vbs, 
						 LocalEngineID, Extra, 
						 get(net_if))
			end
		    catch 
			C:E:S ->
			    exit({worker_crash, Req, C, E, S})
		    end,
		Master ! worker_available, 
		SendTrapRes; % For debugging...
	    
	    
	    #wrequest{cmd  = verbosity, 
		      info = Info} ->
		Verbosity = proplists:get_value(verbosity, Info),
		put(verbosity, snmp_verbosity:validate(Verbosity));
	    
	    
	    #wrequest{cmd  = terminate} ->
		?vtrace("worker_loop -> received terminate request", []),
		exit(normal);
	    
	    
	    %% *************************************************************
	    %% 
	    %%         Kept for backward compatibillity reasons
	    %% 
	    %% *************************************************************
	    
	    {Vsn, Pdu, PduMS, ACMData, Address, Extra} ->
		?vtrace("worker_loop -> received request", []),
		handle_pdu2(Vsn, Pdu, PduMS, ACMData, Address, 
			    ?DEFAULT_GB_MAX_VBS, Extra),
		Master ! worker_available;
	    
	    %% We don't trap exits!
	    {TrapRec, NotifyName, ContextName, Recv, Vbs} -> 
		?vtrace("worker_loop -> send trap:"
			"~n   ~p", [TrapRec]),
		snmpa_trap:send_trap(TrapRec, NotifyName, 
				     ContextName, Recv, Vbs, get(net_if)),
		Master ! worker_available;
	    
	    %% We don't trap exits!
	    {send_trap, 
	     TrapRec, NotifyName, ContextName, Recv, Vbs, LocalEngineID,
	     ExtraInfo} -> 
		?vtrace("worker_loop -> send trap:"
			"~n   ~p", [TrapRec]),
		snmpa_trap:send_trap(TrapRec, NotifyName, 
				     ContextName, Recv, Vbs, 
				     LocalEngineID, ExtraInfo, 
				     get(net_if)),
		Master ! worker_available;
	    
	    {verbosity, Verbosity} ->
		put(verbosity, snmp_verbosity:validate(Verbosity));
	    
	    terminate ->
		exit(normal);
	    
	    _X ->
		%% ignore
		ignore_unknown
	
	after 30000 ->
		%% This is to assure that the worker process leaves a
		%% possibly old version of this module.
		ok
	end,
    ?vtrace("worker_loop -> wrap with"
	    "~n   ~p", [Res]),
    ?MODULE:worker_loop(Master).


%%-----------------------------------------------------------------
%%-----------------------------------------------------------------

handle_snmp_pdu(true, Vsn, Pdu, PduMS, ACMData, Address, Extra, 
		#state{multi_threaded = false, 
		       gb_max_vbs     = GbMaxVBs} = S) ->
    ?vtrace("handle_snmp_pdu -> single-thread agent",[]),
    handle_pdu2(Vsn, Pdu, PduMS, ACMData, Address, GbMaxVBs, Extra),
    S;
handle_snmp_pdu(true, Vsn, #pdu{type = 'set-request'} = Pdu, PduMS, 
		ACMData, Address, Extra, 
		#state{set_worker = Worker} = S) ->
    ?vtrace("handle_snmp_pdu -> multi-thread agent: "
	    "send set-request to main worker",[]),
    WRequest = ?mk_pdu_wreq(Vsn, Pdu, PduMS, ACMData, Address, infinity, Extra),
    Worker ! WRequest, 
    S#state{worker_state = busy};
handle_snmp_pdu(true, Vsn, Pdu, PduMS, 
		ACMData, Address, Extra, 
		#state{worker_state = busy, 
		       gb_max_vbs   = GbMaxVBs} = S) ->
    ?vtrace("handle_snmp_pdu -> multi-thread agent: "
	    "main worker busy - create new worker",[]),
    spawn_thread(Vsn, Pdu, PduMS, ACMData, Address, GbMaxVBs, Extra),
    S;
handle_snmp_pdu(true, Vsn, Pdu, PduMS, ACMData, Address, Extra, 
		#state{worker     = Worker, 
		       gb_max_vbs = GbMaxVBs} = S) ->
    ?vtrace("handle_snmp_pdu -> multi-thread agent: "
	    "send to main worker",[]),
    WRequest = ?mk_pdu_wreq(Vsn, Pdu, PduMS, ACMData, Address, GbMaxVBs, Extra),
    Worker ! WRequest, 
    S#state{worker_state = busy};
handle_snmp_pdu(_, _Vsn, _Pdu, _PduMS, _ACMData, _Address, _Extra, S) ->
    S.


%% Called via the spawn_thread function
%% <BACKWARD-COMPAT>
handle_pdu(Vsn, Pdu, PduMS, ACMData, Address, Extra, Dict) ->
    handle_pdu(Vsn, Pdu, PduMS, ACMData, Address, ?DEFAULT_GB_MAX_VBS, Extra, 
	       Dict).
%% </BACKWARD-COMPAT>
handle_pdu(Vsn, Pdu, PduMS, ACMData, Address, GbMaxVBs, Extra, Dict) ->
    lists:foreach(fun({Key, Val}) -> put(Key, Val) end, Dict),
    put(sname, pdu_handler_short_name(get(sname))),
    ?vlog("new worker starting",[]),
    handle_pdu2(Vsn, Pdu, PduMS, ACMData, Address, GbMaxVBs, Extra).

handle_pdu2(Vsn, Pdu, PduMS, ACMData, Address, GbMaxVBs, Extra) ->
    %% OTP-3324
    AuthMod = get(auth_module),
    case AuthMod:init_check_access(Pdu, ACMData) of
	{ok, MibView, ContextName} ->
	    ?vlog("handle_pdu -> ok:"
		  "~n   MibView:     ~p"
		  "~n   ContextName: ~p", [MibView, ContextName]),
	    AgentData = cheat(ACMData, Address, ContextName),
	    do_handle_pdu(MibView, Vsn, Pdu, PduMS, ACMData, AgentData, 
			  GbMaxVBs, Extra);
	{error, Reason} ->
	    ?vlog("handle_pdu -> error:"
		  "~n   Reason: ~p", [Reason]),
	    handle_acm_error(Vsn, Reason, Pdu, ACMData, Address, Extra);
	{discarded, Variable, Reason} ->
	    ?vlog("handle_pdu -> discarded:"
		  "~n   Variable: ~p"
		  "~n   Reason:   ~p", [Variable, Reason]),
	    get(net_if) ! {discarded_pdu, Vsn, Pdu#pdu.request_id,
			   ACMData, Variable, Extra}
    end.

do_handle_pdu(MibView, Vsn, Pdu, PduMS, 
	      ACMData, {Community, Address, ContextName}, 
	      GbMaxVBs, Extra) ->
    
    put(net_if_data, Extra),

    RePdu = process_msg(MibView, Vsn, Pdu, PduMS, Community, 
			Address, ContextName, GbMaxVBs),

    ?vtrace("do_handle_pdu -> processed:"
	    "~n   RePdu: ~p", [RePdu]),
    NetIf = get(net_if), 
    NetIf ! {snmp_response, Vsn, RePdu, 
	     RePdu#pdu.type, ACMData, Address, Extra}.


handle_acm_error(Vsn, Reason, Pdu, ACMData, Address, Extra) ->
    #pdu{type = Type, request_id = ReqId, varbinds = Vbs} = Pdu,
    RawErrStatus = snmpa_acm:error2status(Reason),
    case is_valid_pdu_type(Type) of
	true ->
	    %% RawErrStatus can be authorizationError or genErr.  If it is
	    %% authorizationError, we'll have to do different things, 
	    %% depending on which SNMP version is used.
	    %% v1 - noSuchName error
	    %% v2 - GET: all variables 'noSuchObject'
	    %%      NEXT/BULK: all variables 'endOfMibView'
	    %%      SET: noAccess error
	    %% v3 - authorizationError error
	    %%
	    %% NOTE: this procedure is not yet defined in the coex document!
	    ?vdebug("~n   Raw error status: ~w",[RawErrStatus]),
	    Idx = case Vbs of
		      [] -> 0;
		      _ -> 1
		  end,
	    RePdu =
		if
		    Vsn =:= 'version-1' ->
			ErrStatus = v2err_to_v1err(RawErrStatus),
			make_response_pdu(ReqId, ErrStatus, Idx, Vbs, Vbs);
		    Vsn =:= 'version-3' ->
			make_response_pdu(ReqId, RawErrStatus, Idx, Vbs, Vbs);
		    Type =:= 'get-request' ->  % this is v2
			ReVbs = lists:map(
				  fun(Vb) -> 
					  Vb#varbind{value=noSuchObject} 
				  end,
				  Vbs),
			make_response_pdu(ReqId, noError, 0, Vbs, ReVbs);
		    Type =:= 'set-request' ->
			make_response_pdu(ReqId, noAccess, Idx, Vbs, Vbs);
		    true -> % next or bulk
			ReVbs = lists:map(
				  fun(Vb) -> 
					  Vb#varbind{value=endOfMibView} 
				  end,
				  Vbs),
			make_response_pdu(ReqId, noError, 0, Vbs, ReVbs)
		end,
	    get(net_if) ! {snmp_response, Vsn, RePdu, 
			   'get-response', ACMData, Address, Extra};
	false ->
	    ?vdebug("~n   Raw error status: ~w"
		    "~n   invalid pdu type: ~w", 
		    [RawErrStatus,Type]),
	    ok
    end.

get_send_opt(Key, Default, SendOpts) ->
    case lists:keysearch(Key, 1, SendOpts) of
	{value, {Key, Value}} ->
	    Value;
	false ->
	    Default
    end.

handle_send_trap(S, Notification, SendOpts) ->
    NotifyName    = get_send_opt(name,     "",                        SendOpts),
    ContextName   = get_send_opt(context,  "",                        SendOpts),
    Recv          = get_send_opt(receiver, no_receiver,               SendOpts),
    Varbinds      = get_send_opt(varbinds, [],                        SendOpts),
    ExtraInfo     = get_send_opt(extra,    ?DEFAULT_NOTIF_EXTRA_INFO, SendOpts),
    LocalEngineID = 
	case lists:keysearch(local_engine_id, 1, SendOpts) of
	    {value, {local_engine_id, Value}} ->
		Value;
	    false ->
		local_engine_id(S)
	end,
    handle_send_trap(S, Notification, NotifyName, ContextName, Recv, Varbinds, 
		     LocalEngineID, ExtraInfo).

handle_send_trap(#state{type = Type} = S,
		 Notification, NotifyName, ContextName, Recv, Varbinds, 
		 LocalEngineID, ExtraInfo) ->
    ?vtrace("handle_send_trap -> entry with"
	    "~n   Agent type:    ~p"
	    "~n   TrapName:      ~p"
	    "~n   NotifyName:    ~p"
	    "~n   ContextName:   ~p"
	    "~n   LocalEngineID: ~p", 
	    [Type, Notification, NotifyName, ContextName, LocalEngineID]),
    try snmpa_trap:construct_trap(Notification, Varbinds) of
	{ok, TrapRecord, VarList} ->
	    ?vtrace("handle_send_trap -> construction complete: "
		    "~n   TrapRecord: ~p"
		    "~n   VarList: ~p",
		    [TrapRecord, VarList]),
	    case Type of
		subagent ->
		    ?vtrace("handle_send_trap -> [sub] forward trap",[]),
		    maybe_forward_trap(S, TrapRecord, NotifyName,
				       ContextName, Recv, VarList, ExtraInfo),
		    {ok, S};
		master_agent ->
		    ?vtrace("handle_send_trap -> "
			    "[master] handle send trap",[]),
		    maybe_send_trap(S, TrapRecord, NotifyName,
				    ContextName, Recv, VarList,
				    LocalEngineID, ExtraInfo)
	    end;
	error ->
	    {error, failed_constructing_trap}
    catch
        C:E:Stack ->
            {error, {failed_constructing_trap, C, E, Stack}}
    end.
				

maybe_forward_trap(#state{parent = Parent, nfilters = NFs} = S,
		   TrapRec, NotifyName, ContextName, Recv, V, ExtraInfo) ->
    ?vtrace("maybe_forward_trap -> entry with"
	    "~n   NFs: ~p", [NFs]),
    case filter_notification(NFs, [], TrapRec) of
	{dont_send, [], Id} ->
	    ?vdebug("trap not forwarded [filter ~p]", [Id]),
	    {ok, S};
	
	{dont_send, Removed, Id} ->
	    ?vdebug("trap not forwarded [filter ~p]", [Id]),
	    NFs2 = del_notification_filter(Removed, NFs),
	    {ok, S#state{nfilters = NFs2}};
	
	{send, [], TrapRec2} ->
	    ?vtrace("maybe_forward_trap -> forward trap:"
		    "~n   ~p", [TrapRec2]),
	    forward_trap(Parent, TrapRec2, NotifyName, ContextName, Recv, V, 
			 ExtraInfo),
	    {ok, S};
	
	{send, Removed, TrapRec2} ->
	    ?vtrace("maybe_forward_trap -> forward trap:"
		    "~n   ~p", [TrapRec2]),
	    forward_trap(Parent, TrapRec2, NotifyName, ContextName, Recv, V, 
			 ExtraInfo),
	    NFs2 = del_notification_filter(Removed, NFs),
	    {ok, S#state{nfilters = NFs2}}
    end.


maybe_send_trap(#state{nfilters = NFs} = S, 
		TrapRec, NotifyName, ContextName, Recv, Varbinds, 
		LocalEngineID, ExtraInfo) ->
    ?vtrace("maybe_send_trap -> entry with"
	    "~n   NFs: ~p", [NFs]),
    case filter_notification(NFs, [], TrapRec) of
	{dont_send, [], Id} ->
	    ?vdebug("trap not sent [filter ~p]",[Id]),
	    {ok, S};
	
	{dont_send, Removed, Id} ->
	    ?vdebug("trap not sent [filter ~p]",[Id]),
	    NFs2 = del_notification_filter(Removed, NFs),
	    {ok, S#state{nfilters = NFs2}};
	
	{send, [], TrapRec2} ->
	    ?vtrace("maybe_send_trap -> send trap:"
		    "~n   ~p", [TrapRec2]),
	    do_handle_send_trap(S, TrapRec2, 
				NotifyName, ContextName, Recv, Varbinds, 
				LocalEngineID, ExtraInfo);
	
	{send, Removed, TrapRec2} ->
	    ?vtrace("maybe_send_trap -> send trap:"
		    "~n   ~p", [TrapRec2]),
	    NFs2 = del_notification_filter(Removed, NFs),
	    do_handle_send_trap(S#state{nfilters = NFs2}, TrapRec2, 
				NotifyName, ContextName, Recv, Varbinds,
				LocalEngineID, ExtraInfo)
    end.
   
do_handle_send_trap(S, TrapRec, NotifyName, ContextName, Recv, Varbinds,
		    LocalEngineID, ExtraInfo) ->
    Vbs = snmpa_trap:try_initialise_vars(get(mibserver), Varbinds),
    case S#state.type of
	subagent ->
	    forward_trap(S#state.parent, TrapRec, NotifyName, ContextName,
			 Recv, Vbs, ExtraInfo),
	    {ok, S};
	master_agent when S#state.multi_threaded =:= false ->
	    ?vtrace("do_handle_send_trap -> send trap:"
		    "~n   TrapRec: ~p", [TrapRec]),
	    snmpa_trap:send_trap(TrapRec, NotifyName, ContextName,
				 Recv, Vbs, LocalEngineID, ExtraInfo, 
				 get(net_if)),
	    {ok, S};
	master_agent when S#state.worker_state =:= busy ->
	    %% Main worker busy => create new worker
	    ?vtrace("do_handle_send_trap -> main worker busy: "
		    "spawn a trap sender", []),
	    spawn_trap_thread(TrapRec, NotifyName, ContextName, Recv, Vbs,
			      LocalEngineID, ExtraInfo),
	    {ok, S};
	master_agent ->
	    %% Send to main worker
	    ?vtrace("do_handle_send_trap -> send to main worker",[]),
	    S#state.worker ! ?mk_send_trap_wreq(TrapRec, NotifyName, 
						ContextName, Recv, Vbs,
						LocalEngineID, ExtraInfo),
	    {ok, S#state{worker_state = busy}}
    end.
    

filter_notification([], RemoveNFs, Notif) ->
    ?vtrace("filter_notification -> done when"
	    "~n   RemoveNFs: ~p"
	    "~n   Notif:     ~p", [RemoveNFs, Notif]),
    {send, RemoveNFs, Notif};
filter_notification([NF|NFs], RemoveNFs, Notif0) ->
    ?vtrace("filter_notification -> entry with"
	    "~n   NF:        ~p"
	    "~n   RemoveNFs: ~p"
	    "~n   Notif0:    ~p", [NF, RemoveNFs, Notif0]),
    case do_filter_notification(NF, Notif0) of
	{dont_send, Id} ->
	    {dont_send, RemoveNFs, Id};
	{send, Notif} ->
	    filter_notification(NFs, RemoveNFs, Notif);
	{error, Id} ->
	    filter_notification(NFs, [Id|RemoveNFs], Notif0)
    end.

do_filter_notification(#notification_filter{id = Id, mod = Mod, data = Data}, 
		       Notif) ->
    case (catch Mod:handle_notification(Notif, Data)) of
	dont_send ->
	    {dont_send, Id};
	send ->
	    {send, Notif};
	{send, NewNotif} ->
	    {send, NewNotif};
	Else ->
	    user_err("notification filter ~p removed: ~n~p", [Id, Else]),
	    {error, Id}
    end.


add_notification_filter(first, NewNF, NFs) ->
    {ok, [NewNF | NFs]};
add_notification_filter(last, NewNF, NFs) ->
    {ok, lists:append(NFs, [NewNF])};
add_notification_filter(Where, NewNF, NFs) ->
    case add_nf(Where, NewNF, NFs, []) of
	{ok, NFs2} ->
	    {ok, NFs2};
	Error ->
	    {Error, NFs}
    end.

add_nf({_Loc, Id}, _NewNF, [], _Acc) ->
    {error, {not_found, Id}};
add_nf({insert_before, Id}, NewNF, [NF|NFs], Acc) 
  when Id =:= NF#notification_filter.id ->
    {ok, lists:reverse(Acc) ++ [NewNF,NF|NFs]};
add_nf({insert_after, Id}, NewNF, [NF|NFs], Acc) 
  when Id =:= NF#notification_filter.id ->
    {ok, lists:reverse(Acc) ++ [NF,NewNF|NFs]};
add_nf(Where, NewNF, [NF|NFs], Acc) ->
    add_nf(Where, NewNF, NFs, [NF|Acc]).
    

del_notification_filter(IDs, NFs) ->
    Fun = fun(Id, NFilters) -> lists:keydelete(Id, 2, NFilters) end,
    lists:foldl(Fun, NFs, IDs).


handle_discovery(#state{type = master_agent} = S, From, 
		 TargetName, Notification, ContextName, Varbinds, 
		 DiscoHandler, ExtraInfo) ->
    ?vtrace("handle_discovery -> entry with"
	    "~n   TargetName:   ~p" 
	    "~n   Notification: ~p" 
	    "~n   ContextName:  ~p" 
	    "~n   Varbinds:     ~p", 
	    [TargetName, Notification, ContextName, Varbinds]),
    try snmpa_trap:construct_trap(Notification, Varbinds) of
	{ok, Record, InitVars} ->
	    ?vtrace("handle_discovery -> trap construction complete: "
		    "~n   Record:   ~p"
		    "~n   InitVars: ~p", [Record, InitVars]),
	    send_discovery(S, From, TargetName, 
			   Record, ContextName, InitVars, 
			   DiscoHandler, ExtraInfo);
	error ->
	    {error, failed_constructing_notification}
    catch
        C:E:Stack ->
            {error, {failed_constructing_trap, C, E, Stack}}
    end;
handle_discovery(_S, _From, 
		 _TargetName, _Notification, _ContextName, _Varbinds, 
		 _DiscoHandler, _ExtraInfo) ->
    {error, only_master_discovery}.
				
%% We ignore if the master agent is multi-threaded or not.
%% 
send_discovery(S, From, 
	       TargetName, Record, ContextName, InitVars, 
	       DiscoHandler, ExtraInfo) ->
    case snmpa_trap:send_discovery(TargetName, Record, ContextName,
				   InitVars, get(net_if), ExtraInfo) of
	{ok, Sender, SecLevel} ->
	    Disco = #disco{from      = From, 
			   rec       = Record,
			   sender    = Sender, 
			   target    = TargetName, 
			   sec_level = SecLevel,
			   ctx       = ContextName,
			   ivbs      = InitVars, 
			   stage     = 1,
			   handler   = DiscoHandler,
			   extra     = ExtraInfo}, 
	    {ok, S#state{disco = Disco}};
	Error ->
	    ?vlog("send_discovery -> failed sending discovery: "
		    "~n   Error: ~p", [Error]),
	    Error
    end.


handle_discovery_response(#state{disco = #disco{from = From}} = S, 
			  {error, _} = Error) ->
    ?vlog("handle_discovery_response -> entry with"
	  "~n   From:  ~p"
	  "~n   Error: ~p", [From, Error]),
    gen_server:reply(From, Error),
    S#state{disco = undefined};

handle_discovery_response(#state{disco = #disco{target = TargetName, 
						stage  = 1,
						extra  = ExtraInfo} = Disco} = S, 
			  {ok, _Pdu, ManagerEngineId}) 
  when is_record(Disco, disco) ->
    ?vlog("handle_discovery_response(1) -> entry with"
	  "~n   TargetName:      ~p"
	  "~n   ManagerEngineId: ~p", [TargetName, ManagerEngineId]),
    %% This is end of stage 1.
    %% So, first we need to update the database with the EngineId of the 
    %% manager and then deside if we should continue with stage 2. E.g.
    %% establish authenticated communication. 
    case snmp_target_mib:set_target_engine_id(TargetName, ManagerEngineId) of
	true when Disco#disco.sec_level =:= ?'SnmpSecurityLevel_noAuthNoPriv' ->
	    %% Ok, we are done
	    From    = Disco#disco.from,
	    Handler = Disco#disco.handler, 
	    Reply   = 
		case handle_discovery_stage1_finish(Handler, 
						    TargetName, 
						    ManagerEngineId, 
						    ExtraInfo) of
		    {ok, _} ->
			{ok, ManagerEngineId};
		    Error ->
			Error
		end,
	    gen_server:reply(From, Reply),
	    S#state{disco = undefined};

	true when Disco#disco.sec_level =/= ?'SnmpSecurityLevel_noAuthNoPriv' ->
	    %% Ok, time for stage 2
	    %% Send the same inform again, 
	    %% this time we have the proper EngineId

	    From    = Disco#disco.from,
	    Handler = Disco#disco.handler, 

	    case handle_discovery_stage1_finish(Handler, 
						TargetName, 
						ManagerEngineId, 
						ExtraInfo) of
		{ok, NewExtraInfo} ->
		    ?vdebug("handle_discovery_response(1) -> "
			    "we are done with stage 1 - "
			    "continue with stage 2", []),
		    #disco{rec  = Record,
			   ctx  = ContextName,
			   ivbs = InitVars} = Disco, 
		    case snmpa_trap:send_discovery(TargetName, 
						   Record, 
						   ContextName,
						   InitVars, 
						   get(net_if), 
						   ExtraInfo) of
			{ok, Sender, _SecLevel} ->
			    ?vdebug("handle_discovery_response(1) -> "
				    "stage 2 trap sent", []),
			    Disco2 = Disco#disco{sender    = Sender, 
						 engine_id = ManagerEngineId, 
						 stage     = 2,
						 extra     = NewExtraInfo}, 
			    S#state{disco = Disco2};
			Error ->
			    ?vlog("handle_discovery_response(1) -> "
				  "failed sending stage 2 trap: "
				  "~n   ~p", [Error]),
			    error_msg("failed sending second "
				      "discovery message: "
				      "~n   ~p", [Error]),
			    Reply = {error, {second_send_failed, Error}}, 
			    gen_server:reply(From, Reply),
			    S#state{disco = undefined}
		    end;
		{error, Reason} = Error ->
		    ?vlog("handle_discovery_response(1) -> "
			  "stage 1 finish failed: "
			  "~n   ~p", [Reason]),
		    gen_server:reply(From, Error),
		    S#state{disco = undefined}
	    end;
	false ->
	    ?vinfo("handle_discovery_response(1) -> "
		   "failed setting doscovered engine-id - "
		   "inform the user", []),
	    From   = Disco#disco.from,
	    Reason = {failed_setting_engine_id, TargetName, ManagerEngineId},
	    gen_server:reply(From, {error, Reason}),
	    S#state{disco = undefined}

    end;

handle_discovery_response(#state{disco = #disco{from = From, 
						engine_id = EngineID, 
						stage = 2}} = S, 
			  {ok, _Pdu}) ->
    ?vlog("handle_discovery_response(2) -> entry with"
	  "~n   From: ~p", [From]),
    gen_server:reply(From, {ok, EngineID}),
    S#state{disco = undefined};

handle_discovery_response(#state{disco = #disco{from = From}} = S, Crap) ->
    Reason = {invalid_response, Crap}, 
    gen_server:reply(From, {error, Reason}),
    S#state{disco = undefined};

handle_discovery_response(S, Crap) ->
    warning_msg("Received unexpected discovery response: ~p", [Crap]),
    S.

handle_discovery_stage1_finish(Handler, TargetName, ManagerEngineID, 
			       ExtraInfo) ->
    case (catch Handler:stage1_finish(TargetName, ManagerEngineID, 
				      ExtraInfo)) of
	ignore ->
	    ?vtrace("handle_discovery_stage1_finish -> "
		    "we are done - [ignore] inform the user", []),
	    {ok, ExtraInfo};

	{ok, UsmEntry} when is_tuple(UsmEntry) ->
	    ?vtrace("handle_discovery_stage1_finish -> "
		    "received usm entry - attempt to add it", []),
	    case add_usm_users([UsmEntry]) of
		ok ->
		    {ok,  ExtraInfo};
		Error ->
		    Error
	    end;

	{ok, UsmEntry, NewExtraInfo} when is_tuple(UsmEntry) ->
	    ?vtrace("handle_discovery_stage1_finish -> "
		    "received usm entry - attempt to add it", []),
	    case add_usm_users([UsmEntry]) of
		ok ->
		    {ok, NewExtraInfo};
		Error ->
		    Error
	    end;

	{ok, UsmEntries} when is_list(UsmEntries) ->
	    ?vtrace("handle_discovery_stage1_finish -> "
		    "received usm entries - attempt to add them", []),
	    case add_usm_users(UsmEntries) of
		ok ->
		    {ok, ExtraInfo};
		Error ->
		    Error
	    end;

	{ok, UsmEntries, NewExtraInfo} when is_list(UsmEntries) ->
	    ?vtrace("handle_discovery_stage1_finish -> "
		    "received usm entries - attempt to add them", []),
	    case add_usm_users(UsmEntries) of
		ok ->
		    {ok, NewExtraInfo};
		Error ->
		    Error
	    end;

	{'EXIT', Reason} ->
	    ?vlog("handle_discovery_stage1_finish -> stage 1 function exited: "
		  "~n   ~p", [Reason]),
	    {error, {finish_exit, Reason, ManagerEngineID}};

	{error, Reason} ->
	    ?vlog("handle_discovery_stage1_finish -> stage 1 function error: "
		  "~n   ~p", [Reason]),
	    {error, {finish_error, Reason, ManagerEngineID}};

	Unknown ->
	    ?vlog("handle_discovery_stage1_finish -> stage 1 function unknown: "
		  "~n   ~p", [Unknown]),
	    {error, {finish_failed, Unknown, ManagerEngineID}}
    end.

add_usm_users([]) ->
    ok;
add_usm_users([UsmEntry|UsmEntries]) when is_tuple(UsmEntry) ->
    ?vtrace("add_usm_users -> attempt to add entry (~w)", 
	    [element(1, UsmEntry)]),
    case snmp_user_based_sm_mib:add_user(UsmEntry) of
	{ok, _} ->
	    add_usm_users(UsmEntries);
	{error, Reason} ->
	    ?vlog("add_usm_users -> failed adding usm entry: "
		  "~n   ~p", [Reason]),
	    {error, {failed_adding_entry, Reason, UsmEntry}}
    end.

    
handle_me_of(MibServer, Oid) ->
    case snmpa_mib:lookup(MibServer, Oid) of
	{variable, ME} ->
	    {ok, ME};
	{table_column, ME, _TableEntryOid} ->
	    {ok, ME};
	{subagent, SubAgentPid, _SANextOid} ->
	    {error, {subagent, SubAgentPid}};
	{false, Reason} ->
	    {error, Reason};
	Else ->
	    {error, Else}
    end.

   
handle_mib_of(MibServer, Oid) ->
    snmpa_mib:which_mib(MibServer, Oid).


%%-----------------------------------------------------------------
%% Func: process_msg/7
%% Returns: RePdu
%%-----------------------------------------------------------------
process_msg(
  MibView, Vsn, Pdu, PduMS, Community,
  SourceAddress, ContextName, GbMaxVBs) ->
    #pdu{request_id = ReqId} = Pdu,
    put(
      snmp_address,
      case SourceAddress of
	  {Domain, _} when is_atom(Domain) ->
	      SourceAddress;
	  {Ip, Port} when is_integer(Port) ->
	      %% Legacy transport domain
	      {tuple_to_list(Ip), Port}
      end),
    put(snmp_request_id, ReqId),
    put(snmp_community, Community),
    put(snmp_context, ContextName),
    ?vtrace("process ~p",[Pdu#pdu.type]),
    process_pdu(Pdu, PduMS, Vsn, MibView, GbMaxVBs).

process_pdu(#pdu{type='get-request', request_id = ReqId, varbinds=Vbs},
	    _PduMS, Vsn, MibView, _GbMaxVBs) ->
    ?vtrace("get ~p",[ReqId]),
    OrigRes = do_get(MibView, Vbs, false),
    Res     = get_err(OrigRes),
    {ErrStatus, ErrIndex, ResVarbinds} =
	if
	    Vsn =:= 'version-1' -> validate_get_v1(Res);
	    true -> Res
	end,
    if
        (ErrStatus =/= noError) ->
            ?vlog("get final result: "
                  "~n      Error status: ~p"
                  "~n      Error index:  ~p"
                  "~n   when"
                  "~n      Original Result: "
                  "~n         ~p", [ErrStatus, ErrIndex, OrigRes]);
        true ->
            ?vtrace("get final result: "
                    "~n   Error status: ~p"
                    "~n   Error index:  ~p"
                    "~n   Varbinds:     ~p",
                    [ErrStatus, ErrIndex, ResVarbinds])
    end,
    ResponseVarbinds = lists:keysort(#varbind.org_index, ResVarbinds),
    ?vtrace("response varbinds: "
	    "~n   ~p", [ResponseVarbinds]),
    make_response_pdu(ReqId, ErrStatus, ErrIndex, Vbs, ResponseVarbinds);

process_pdu(#pdu{type = 'get-next-request', request_id = ReqId, varbinds = Vbs},
	    _PduMS, Vsn, MibView, _GbMaxVBs) ->
    ?vtrace("process get-next-request -> entry with"
	    "~n   ReqId:   ~p"
	    "~n   Vbs:     ~p"
	    "~n   MibView: ~p", [ReqId, Vbs, MibView]),
    OrigRes = do_get_next(MibView, Vbs),
    Res     = get_err(OrigRes),
    {ErrStatus, ErrIndex, ResVarbinds} = 
	if
	    Vsn =:= 'version-1' -> validate_next_v1(Res, MibView);
	    true -> Res
	end,
    if
        (ErrStatus =/= noError) ->
            ?vlog("get-next final result: "
                  "~n   Error status: ~p"
                  "~n   Error index:  ~p"
                  "~n   when"
                  "~n      Original Result: "
                  "~n         ~p", [ErrStatus, ErrIndex, OrigRes]);
        true ->
            ?vtrace("get-next final result:"
                    "~n   Error status: ~p"
                    "~n   Error index:  ~p"
                    "~n   Varbinds:     ~p", [ErrStatus, ErrIndex, ResVarbinds])
    end,
    ResponseVarbinds = lists:keysort(#varbind.org_index, ResVarbinds),
    ?vtrace("get-next final result -> response varbinds: "
	    "~n   ~p", [ResponseVarbinds]),
    make_response_pdu(ReqId, ErrStatus, ErrIndex, Vbs, ResponseVarbinds);

process_pdu(#pdu{type         = 'get-bulk-request',
		 request_id   = ReqId,
		 varbinds     = Vbs,
		 error_status = NonRepeaters, 
		 error_index  = MaxRepetitions},
	    PduMS, _Vsn, MibView, GbMaxVBs) ->
    OrigRes = do_get_bulk(MibView, NonRepeaters, MaxRepetitions, PduMS, Vbs, 
                          GbMaxVBs),
    {ErrStatus, ErrIndex, ResponseVarbinds} = get_err(OrigRes),
    if
        (ErrStatus =/= noError) ->
            ?vlog("get-bulk final result: "
                  "~n   Error Status: ~p"
                  "~n   Error Index:  ~p"
                  "~n   when"
                  "~n      Original Result: "
                  "~n         ~p", [ErrStatus, ErrIndex, OrigRes]);
        true ->
            ?vtrace("get-bulk final result: "
                    "~n   Error status:     ~p"
                    "~n   Error index:      ~p"
                    "~n   Response Varbinds: ~p",
                    [ErrStatus, ErrIndex, ResponseVarbinds])
    end,
    make_response_pdu(ReqId, ErrStatus, ErrIndex, Vbs, ResponseVarbinds);

process_pdu(#pdu{type = 'set-request', request_id = ReqId, varbinds = Vbs},
	    _PduMS, Vsn, MibView, _GbMaxVbs) ->
    OrigRes = do_set(MibView, Vbs),
    {ErrStatus, ErrIndex} =
	if 
	    Vsn =:= 'version-1' -> validate_err(v2_to_v1, OrigRes);
	    true -> OrigRes
	end,
    if
        (ErrStatus =/= noError) ->
            ?vlog("set final result: "
                  "~n      Error Status: ~p"
                  "~n      Error Index:  ~p"
                  "~n   when"
                  "~n      Original Result: "
                  "~n         ~p", [ErrStatus, ErrIndex, OrigRes]);
        true ->
            ?vtrace("set final result: "
                    "~n   Error Status: ~p"
                    "~n   Error Index:  ~p", [ErrStatus, ErrIndex])
    end,
    make_response_pdu(ReqId, ErrStatus, ErrIndex, Vbs, Vbs).


%%-----------------------------------------------------------------
%% Transform a value == noSuchInstance | noSuchObject or a 
%% Counter64 type to a noSuchName error for the whole pdu.
%% Args: {Error, Index, Vbs}
%% Returns: {NewError, NewIndex, NewVbs}
%%-----------------------------------------------------------------
validate_get_v1({noError, _, ResponseVarbinds}) ->
    case validate_get_v1_2(ResponseVarbinds) of
	true -> {noError, 0, ResponseVarbinds};
	{Error, Index} -> {Error, Index, []} % dummy vbs
    end;
validate_get_v1({ErrStatus, ErrIndex, ResponseVarbinds}) ->
    {v2err_to_v1err(ErrStatus), ErrIndex, ResponseVarbinds}.

validate_get_v1_2([Vb | Vbs]) 
  when ((Vb#varbind.value =/= noSuchInstance) andalso 
	(Vb#varbind.value =/= noSuchObject) andalso 
	(Vb#varbind.variabletype =/= 'Counter64')) ->
    validate_get_v1_2(Vbs);
validate_get_v1_2([Vb | _Vbs]) ->
    {noSuchName, Vb#varbind.org_index};
validate_get_v1_2([]) ->
    true.

%%-----------------------------------------------------------------
%% Transform a value == endOfMibView to a noSuchName for the
%% whole pdu, and do another get-next for any Counter64 value.
%% Args: {Error, Index, Vbs}
%% Returns: {NewError, NewIndex, NewVbs}
%%-----------------------------------------------------------------
validate_next_v1({noError, _, ResponseVarbinds}, MibView) ->
    case validate_next_v1_2(ResponseVarbinds, MibView, []) of
	{true, NVbs} -> {noError, 0, NVbs};
	{Error, Index} -> {Error, Index, []} % dummy vbs
    end;
validate_next_v1({ErrStatus, ErrIndex, ResponseVarbinds}, _MibView) ->
    {v2err_to_v1err(ErrStatus), ErrIndex, ResponseVarbinds}.

validate_next_v1_2([Vb | _Vbs], _MibView, _Res)
  when Vb#varbind.value =:= endOfMibView ->
    {noSuchName, Vb#varbind.org_index};
validate_next_v1_2([Vb | Vbs], MibView, Res)
  when Vb#varbind.variabletype =:= 'Counter64' ->
    case validate_next_v1( do_get_next(MibView, [mk_next_oid(Vb)]), MibView) of
	{noError, 0, [NVb]} ->
	    validate_next_v1_2(Vbs, MibView, [NVb | Res]);
	{Error, Index, _OrgVb} ->
	    {Error, Index}
    end;
validate_next_v1_2([Vb | Vbs], MibView, Res) ->
    validate_next_v1_2(Vbs, MibView, [Vb | Res]);
validate_next_v1_2([], _MibView, Res) ->
    {true, Res}.

%%-----------------------------------------------------------------
%% Optimization. When we get to a Counter64 object that is a table
%% column, we'll try to find the next instance. This will be the
%% next row in the table, which is a Counter64 value as well. This
%% means that we will loop through the entire table, until we find
%% a column that isn't a Counter64 column. We can optimze this by
%% adding 1 to the column-no in the oid of this instance.
%% If the table is implemented by a subagent this does not help,
%% we'll call that subagent many times. But it shouldn't be any
%% problems.
%%-----------------------------------------------------------------
mk_next_oid(Vb) ->
    case snmpa_mib:lookup(get(mibserver), Oid = Vb#varbind.oid) of
	{table_column, _MibEntry, TableEntryOid} ->
	    [Col | _] = Oid -- TableEntryOid,
	    Vb#varbind{oid = TableEntryOid ++ [Col+1]};
	_ ->
	    Vb
    end.

%%%-----------------------------------------------------------------
%%% 3. GET REQUEST
%%% --------------
%%%   According to RFC1157, section 4.1.2 and RFC1905, section 4.2.1.
%%%   In rfc1157:4.1.2 it isn't specified if noSuchName should be
%%%   returned even if some other varbind generates a genErr.
%%%   In rfc1905:4.2.1 this is not a problem since exceptions are
%%%   used, and thus a genErr will be returned anyway.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: do_get/2
%% Purpose: Handles all VBs in a request that is inside the 
%%          mibview (local).
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%%-----------------------------------------------------------------

do_get(UnsortedVarbinds, IsNotification) ->
    Extra     = get(net_if_data),
    GetModule = get(get_module),
    GetModule:do_get(UnsortedVarbinds, IsNotification, Extra).
    

%%-----------------------------------------------------------------
%% Func: do_get/3
%% Purpose: do_get handles "getRequests".
%% Pre: incoming varbinds have type == 'NULL', value == unSpecified
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%%-----------------------------------------------------------------

%% If this function is called from a worker-process (or other process),
%% we *may* need to tunnel into the master-agent and let it do the work.

do_get(MibView, UnsortedVarbinds, IsNotification) ->
    Extra     = get(net_if_data),
    GetModule = get(get_module),
    GetModule:do_get(MibView, UnsortedVarbinds, IsNotification, Extra).

do_get(MibView, UnsortedVarbinds, IsNotification, ForceMaster) ->
    case (whereis(snmp_master_agent) =:= self()) of
	false when (ForceMaster =:= true) ->
	    PduData = get_pdu_data(), 
	    call(snmp_master_agent, 
		 {do_get, MibView, UnsortedVarbinds, IsNotification, PduData});
        _ ->
            do_get(MibView, UnsortedVarbinds, IsNotification)
    end.



%%%-----------------------------------------------------------------
%%% 4. GET-NEXT REQUEST
%%% --------------
%%%   According to RFC1157, section 4.1.3 and RFC1905, section 4.2.2.
%%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: do_get_next/2
%% Purpose: do_get_next handles "getNextRequests".
%% Note: Even if it is SNMPv1, a varbind's value can be
%%       endOfMibView. This is converted to noSuchName in process_pdu.
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%% Note2: ListOfNewVarbinds is not sorted in any order!!!
%% Alg: First, the variables are sorted in OID order.
%%
%%      Second, next in the MIB is performed for each OID, and
%%      the result is collected as: if next oid is a variable,
%%      perform a get to retrieve its value; if next oid is in a
%%      table, save this value and continue until we get an oid
%%      outside this table. Then perform get_next on the table,
%%      and continue with all endOfTables and the oid outside the
%%      table; if next oid is an subagent, save this value and
%%      continue as in the table case.
%%
%%      Third, each response is checked for endOfMibView, or (for
%%      subagents) that the Oid returned has the correct prefix.
%%      (This is necessary since an SA can be registered under many
%%      separated subtrees, and if the last variable in the first
%%      subtree is requested in a next, the SA will return the first
%%      variable in the second subtree. This might be working, since
%%      there may be a variable in between these subtrees.) For each
%%      of these, a new get-next is performed, one at a time.
%%      This alg. might be optimised in several ways. The most 
%%      striking one is that the same SA might be called several
%%      times, when one time should be enough. But it isn't clear
%%      that this really matters, since many nexts across the same
%%      subagent must be considered to be very rare.
%%-----------------------------------------------------------------


do_get_next(MibView, UnsortedVarbinds) ->
    Extra     = get(net_if_data),
    GetModule = get(get_module),
    GetModule:do_get_next(MibView, UnsortedVarbinds, Extra).



%%%-----------------------------------------------------------------
%%% 5. GET-BULK REQUEST
%%% 
%%% In order to prevent excesses in reply sizes there are two 
%%% preventive methods in place. One is to check that the encode
%%% size does not exceed Max PDU size (this is mentioned in the
%%% standard). The other is a simple VBs limit. That is, the 
%%% resulting response cannot contain more then this number of VBs.
%%%-----------------------------------------------------------------

do_get_bulk(MibView, NonRepeaters, MaxRepetitions, PduMS, Varbinds, GbMaxVBs) ->
    Extra     = get(net_if_data),
    GetModule = get(get_module),
    GetModule:do_get_bulk(MibView, NonRepeaters, MaxRepetitions,
                          PduMS, Varbinds, GbMaxVBs,
                          Extra).



%%%--------------------------------------------------
%%% 6. SET REQUEST
%%%--------------------------------------------------
%% return:  {ErrStatus, ErrIndex}
%% where ErrIndex is an index in Varbinds list (not org_index (user-functions
%% doesn't see org_index)).
do_set(MibView, UnsortedVarbinds) ->
    SetModule = get(set_module),
    ?vtrace("set module: ~p",[SetModule]),
    apply(SetModule, do_set, [MibView, UnsortedVarbinds]).

do_subagent_set(Arguments) ->
    SetModule = get(set_module),
    apply(SetModule, do_subagent_set, [Arguments]).



%%%-----------------------------------------------------------------
%%% 7. Misc functions
%%%-----------------------------------------------------------------

make_response_pdu(ReqId, ErrStatus, ErrIndex, OrgVarbinds, _ResponseVarbinds)
  when ErrIndex =/= 0 ->
    #pdu{type = 'get-response', request_id = ReqId, error_status = ErrStatus,
	 error_index = ErrIndex, varbinds = OrgVarbinds};
make_response_pdu(ReqId, ErrStatus, ErrIndex, _OrgVarbinds, ResponseVarbinds) ->
    #pdu{type = 'get-response', request_id = ReqId, error_status = ErrStatus,
	 error_index = ErrIndex, varbinds = ResponseVarbinds}.

%% Valid errormsgs for different operations.
validate_err(consistency_check, {'EXIT', _Reason}, _) ->
    {genErr, 0};
validate_err(consistency_check, X, _) ->
    X;

validate_err(is_set_ok, noError, _) -> noError;
validate_err(is_set_ok, noCreation, _) -> noCreation;
validate_err(is_set_ok, inconsistentValue, _) -> inconsistentValue;
validate_err(is_set_ok, resourceUnavailable, _) -> resourceUnavailable;
validate_err(is_set_ok, inconsistentName, _) -> inconsistentName;
validate_err(is_set_ok, badValue, _) -> badValue;
validate_err(is_set_ok, wrongValue, _) -> wrongValue;
validate_err(is_set_ok, noSuchName, _) -> noSuchName;
validate_err(is_set_ok, noAccess, _) -> noAccess;
validate_err(is_set_ok, notWritable, _) -> notWritable;
validate_err(is_set_ok, genErr, _) -> genErr;
validate_err(is_set_ok, X, Mfa) -> 
    user_err("~w with is_set_ok, returned: ~w. Using genErr.",
	     [Mfa, X]),
    genErr;

validate_err(set, commitFailed, _) -> commitFailed;
validate_err(set, undoFailed, _) -> undoFailed;
validate_err(set, noError, _) -> noError;
validate_err(set, genErr, _) -> genErr;
validate_err(set, X, Mfa) -> 
    user_err("~w with set, returned: ~w. Using genErr.",
	     [Mfa, X]),
    genErr;

validate_err(undo, undoFailed, _) -> undoFailed;
validate_err(undo, noError, _) -> noError;
validate_err(undo, genErr, _) -> genErr;
validate_err(undo, X, Mfa) -> 
    user_err("~w with undo, returned: ~w. Using genErr.",
	     [Mfa, X]),
    genErr;

validate_err(table_is_set_ok, {Err, Idx}, Mfa) when is_integer(Idx) ->
    {validate_err(is_set_ok, Err, Mfa), Idx};
validate_err(table_is_set_ok, X, Mfa) ->
    user_err("~w with is_set_ok (table), returned: ~w. Using genErr.",
	     [Mfa, X]),
    {genErr, 0};

validate_err(row_is_set_ok, {Err, Idx}, _) when is_integer(Idx) ->
    {Err, Idx};
validate_err(row_is_set_ok, {_Err, {false, BadCol}}, Mfa) ->
    user_err("~w with is_set_ok (table), returned bad column: "
	     "~w. Using genErr.", [Mfa, BadCol]),
    {genErr, 0};

validate_err(table_undo, {Err, Idx}, Mfa) when is_integer(Idx) ->
    {validate_err(undo, Err, Mfa), Idx};
validate_err(table_undo, X, Mfa) ->
    user_err("~w with undo (table), returned: ~w. Using genErr.",
	     [Mfa, X]),
    {genErr, 0};

validate_err(row_undo, {Err, Idx}, _) when is_integer(Idx) ->
    {Err, Idx};
validate_err(row_undo, {_Err, {false, BadCol}}, Mfa) ->
    user_err("~w with undo (table), returned bad column: "
	     "~w. Using genErr.", [Mfa, BadCol]),
    {genErr, 0};

validate_err(table_set, {Err, Idx}, Mfa) when is_integer(Idx) ->
    {validate_err(set, Err, Mfa), Idx};
validate_err(table_set, X, Mfa) ->
    user_err("~w with set (table), returned: ~w. Using genErr.",
	     [Mfa, X]),
    {genErr, 0};

validate_err(row_set, {Err, Idx}, _) when is_integer(Idx) ->
    {Err, Idx};
validate_err(row_set, {_Err, {false, BadCol}}, Mfa) ->
    user_err("~w with set (table), returned bad column: "
	     "~w. Using genErr.", [Mfa, BadCol]),
    {genErr, 0};

validate_err(table_next, {Err, Idx}, _Mfa) when is_integer(Idx) ->
    {Err, Idx};
validate_err(table_next, {_Err, {false, BadCol}}, Mfa) ->
    user_err("~w with get_next, returned bad column: "
	     "~w. Using genErr.", [Mfa, BadCol]),
    {genErr, 0}.

validate_err(v2_to_v1, {V2Err, Index}) ->
    {v2err_to_v1err(V2Err), Index};
validate_err(v2_to_v1, _) ->
    {genErr, 0}.

get_err({ErrC, ErrI, Vbs}) ->
    {get_err_i(ErrC), ErrI, Vbs}.

get_err_i(noError) -> noError;
get_err_i(tooBig) -> tooBig;    % OTP-9700 
get_err_i(ES) -> ?vtrace("convert ErrorStatus '~p' to 'genErr'", [ES]), genErr.

v2err_to_v1err(noError) ->            noError;
v2err_to_v1err(noAccess) ->           noSuchName;
v2err_to_v1err(noCreation) ->         noSuchName;
v2err_to_v1err(notWritable) ->        noSuchName;
v2err_to_v1err(wrongLength) ->        badValue;
v2err_to_v1err(wrongEncoding) ->      badValue;
v2err_to_v1err(wrongType) ->          badValue;
v2err_to_v1err(wrongValue) ->         badValue;
v2err_to_v1err(inconsistentValue) ->  badValue;
v2err_to_v1err(inconsistentName) ->   noSuchName;
v2err_to_v1err(noSuchName) ->         noSuchName;
v2err_to_v1err(badValue) ->           badValue;
v2err_to_v1err(authorizationError) -> noSuchName;
%% genErr | resourceUnavailable | undoFailed | commitFailed -> genErr
v2err_to_v1err(_Error) ->             genErr.

%%-----------------------------------------------------------------
%% transforms a (hopefully correct) return value ((perhaps) from a 
%% mib-function) to a typed and guaranteed correct return value.
%% An incorrect return value is transformed to {error, genErr}.
%% A correct return value is on the form: 
%% {error, <error-msg>} | {value, <variable-type>, <value>}
%%-----------------------------------------------------------------
make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype =:= 'INTEGER' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype =:= 'Counter32' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype =:= 'Unsigned32' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype =:= 'TimeTicks' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype =:= 'Counter64' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when (Asn1#asn1_type.bertype =:= 'BITS') andalso is_list(Val) ->
    {value,Kibbles} = snmp_misc:assq(kibbles,Asn1#asn1_type.assocList),
    case snmp_misc:bits_to_int(Val,Kibbles) of
	error ->
	    wrongValue(Val, Mfa);
	Int ->
	    make_value_a_correct_value({value,Int},Asn1,Mfa)
    end;

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when (Asn1#asn1_type.bertype =:= 'BITS') andalso is_integer(Val) ->
    {value,Kibbles} = snmp_misc:assq(kibbles,Asn1#asn1_type.assocList),
    {_Kibble,BitNo} = lists:last(Kibbles),
    case (1 bsl (BitNo+1)) of
	X when Val < X ->
	    {value,'BITS',Val};
	_Big ->
	    wrongValue(Val, Mfa)
    end;

make_value_a_correct_value({value, String},
			   #asn1_type{bertype = 'OCTET STRING',
				      hi = Hi, lo = Lo}, Mfa) ->
    check_octet_string(String, Hi, Lo, Mfa, 'OCTET STRING');

make_value_a_correct_value({value, String},
			   #asn1_type{bertype = 'IpAddress',
				      hi = Hi, lo = Lo}, Mfa) ->
    check_octet_string(String, Hi, Lo, Mfa, 'IpAddress');

make_value_a_correct_value({value, Oid},
			   #asn1_type{bertype = 'OBJECT IDENTIFIER'},
			   _Mfa) ->
    case snmp_misc:is_oid(Oid) of
	true  -> {value, 'OBJECT IDENTIFIER', Oid};
	_Else -> {error, wrongType}
    end;

make_value_a_correct_value({value, Val}, Asn1, _Mfa)
  when Asn1#asn1_type.bertype =:= 'Opaque' ->
    if is_list(Val) -> {value, 'Opaque', Val};
       true -> {error, wrongType}
    end;

make_value_a_correct_value({noValue, noSuchObject}, _ASN1Type, _Mfa) ->
    {value, noValue, noSuchObject};
make_value_a_correct_value({noValue, noSuchInstance}, _ASN1Type, _Mfa) ->
    {value, noValue, noSuchInstance};
make_value_a_correct_value({noValue, noSuchName}, _ASN1Type, _Mfa) ->
    %% Transform this into a v2 value.  It is converted to noSuchName
    %% later if it was v1.  If it was v2, we use noSuchInstance.
    {value, noValue, noSuchInstance};
%% For backwards compatibility only - we really shouldn't allow this;
%% it makes no sense to return unSpecified for a variable! But we did
%% allow it previously. -- We transform unSpecified to noSuchInstance
%% (OTP-3303).
make_value_a_correct_value({noValue, unSpecified}, _ASN1Type, _Mfa) ->
    {value, noValue, noSuchInstance};
make_value_a_correct_value(genErr, _ASN1Type, _MFA) ->
    {error, genErr};

make_value_a_correct_value(_WrongVal, _ASN1Type, undef) ->
    {error, genErr};

make_value_a_correct_value(WrongVal, ASN1Type, Mfa) ->
    user_err("Got ~w from ~w. (~w) Using genErr",
	     [WrongVal, Mfa, ASN1Type]),
    {error, genErr}.

check_integer(Val, Asn1, Mfa) ->
    case Asn1#asn1_type.assocList of
	undefined -> check_size(Val, Asn1, Mfa);
	Alist ->
	    case snmp_misc:assq(enums, Alist) of
		{value, Enums} -> check_enums(Val, Asn1, Enums, Mfa);
		false -> check_size(Val, Asn1, Mfa)
	    end
    end.

check_octet_string(String, Hi, Lo, Mfa, Type) ->
    Len = (catch length(String)), % it might not be a list
    case snmp_misc:is_string(String) of
	true when Lo =:= undefined -> {value, Type, String};
	true when Len =< Hi, Len >= Lo ->
	    {value, Type, String};
	true ->
	    wrongLength(String, Mfa);
	_Else ->
	    wrongType(String, Mfa)
    end.

check_size(Val, #asn1_type{lo = Lo, hi = Hi, bertype = Type}, Mfa) 
  when is_integer(Val) ->
    ?vtrace("check size of integer: "
	    "~n   Value:       ~p"
	    "~n   Upper limit: ~p"
	    "~n   Lower limit: ~p"
	    "~n   BER-type:    ~p",
	    [Val,Hi,Lo,Type]),
    if
	(Lo =:= undefined) andalso (Hi =:= undefined) -> {value, Type, Val};
	(Lo =:= undefined) andalso is_integer(Hi) andalso (Val =< Hi) ->
	    {value, Type, Val};
	is_integer(Lo) andalso (Val >= Lo) andalso (Hi =:= undefined) ->
	    {value, Type, Val};
	is_integer(Lo) andalso is_integer(Hi) andalso (Val >= Lo) andalso (Val =< Hi) ->
	    {value, Type, Val};
	true ->
	    wrongValue(Val, Mfa)
    end;
check_size(Val, _, Mfa) ->
    wrongType(Val, Mfa).

check_enums(Val, Asn1, Enums, Mfa) ->
    Association = 
	if
	    is_integer(Val) -> lists:keysearch(Val, 2, Enums);
	    is_atom(Val)    -> lists:keysearch(Val, 1, Enums);
	    true            -> {error, wrongType}
    end,
    case Association of
	{value, {_AliasIntName, Val2}} -> 
	    {value, Asn1#asn1_type.bertype, Val2};
	false ->
	    wrongValue(Val, Mfa);
	{error, wrongType} ->
	    wrongType(Val, Mfa)
    end.

wrongLength(Val, Mfa) ->
    report_err(Val, Mfa, wrongLength).

wrongValue(Val, Mfa) ->
    report_err(Val, Mfa, wrongValue).

wrongType(Val, Mfa) ->
    report_err(Val, Mfa, wrongType).

report_err(_Val, undef, Err) ->
    {error, Err};
report_err(Val, Mfa, Err) ->
    user_err("Got ~p from ~w. Using ~w", [Val, Mfa, Err]),
    {error, Err}.


is_valid_pdu_type('get-request')      -> true;
is_valid_pdu_type('get-next-request') -> true;
is_valid_pdu_type('get-bulk-request') -> true;
is_valid_pdu_type('set-request')      -> true;
is_valid_pdu_type(_)                  -> false.

get_pdu_data() ->
    {get(net_if_data), 
     get(snmp_request_id),
     get(snmp_address), 
     get(snmp_community), 
     get(snmp_context)}.

put_pdu_data({Extra, ReqId, Address, Community, ContextName}) -> 
    {put(net_if_data, Extra),
     put(snmp_address, Address),
     put(snmp_request_id, ReqId),
     put(snmp_community, Community),
     put(snmp_context, ContextName)}.

tr_var(Oid, Idx) ->
    case snmp_misc:is_oid(Oid) of
	true ->
	    {#varbind{oid = Oid, value = unSpecified, org_index = Idx},
	     Idx+1};
	false -> throw({error, {bad_oid, Oid}})
    end.

tr_varbind(#varbind{value = Value}) -> Value.

mapfoldl(F, Eas, Accu0, [Hd|Tail]) ->
    {R,Accu1} = apply(F, [Hd,Accu0|Eas]),
    {Accu2,Rs} = mapfoldl(F, Eas, Accu1, Tail),
    {Accu2,[R|Rs]};
mapfoldl(_F, _Eas, Accu, []) -> {Accu,[]}.



%% ---------------------------------------------------------------------

short_name(none) -> ma;
short_name(_Pid) -> sa.

worker_short_name(ma) -> maw;
worker_short_name(_)  -> saw.

trap_sender_short_name(ma) -> mats;
trap_sender_short_name(_)  -> sats.

pdu_handler_short_name(ma) -> maph;
pdu_handler_short_name(_)  -> saph.


mib_server_verbosity(Pid,Verbosity) when is_pid(Pid) ->
    snmpa_mib:verbosity(Pid,Verbosity);
mib_server_verbosity(_Pid,_Verbosity) ->
    ok.

note_store_verbosity(Pid,Verbosity) when is_pid(Pid) ->
    snmp_note_store:verbosity(Pid,Verbosity);
note_store_verbosity(_Pid,_Verbosity) ->
    ok.

subagents_verbosity(V) ->
    subagents_verbosity(catch snmpa_mib:info(get(mibserver),subagents),V).

subagents_verbosity([],_V) ->
    ok;
subagents_verbosity([{Pid,_Oid}|T],V) ->
    catch verbosity(Pid,V),             %% On the agent
    catch verbosity(Pid,{subagents,V}), %% and it's subagents
    subagents_verbosity(T,V);
subagents_verbosity(_,_V) ->
    ok.


%% ---------------------------------------------------------------------

local_engine_id(#state{type = master_agent}) ->
    ?DEFAULT_LOCAL_ENGINE_ID;
local_engine_id(_) ->
    %% subagent - 
    %% we don't need this now, eventually the trap send 
    %% request will reach the master-agent and then it 
    %% will look up the proper engine id.
    ignore.


%% ---------------------------------------------------------------------

handle_get_log_type(#state{net_if_mod = Mod}) 
  when Mod =/= undefined ->
    case (catch Mod:get_log_type(get(net_if))) of
	{'EXIT', _} ->
            {error, not_supported};
        Else ->
            Else
    end;
handle_get_log_type(_) ->
    {error, not_supported}.

handle_set_log_type(#state{net_if_mod = Mod}, NewType) 
  when Mod =/= undefined ->
    case (catch Mod:set_log_type(get(net_if), NewType)) of
	{'EXIT', _} ->
            {error, not_supported};
        Else ->
            Else
    end;
handle_set_log_type(_, _) ->
    {error, not_supported}.


handle_get_request_limit(#state{net_if_mod = Mod}) 
  when Mod =/= undefined ->
    case (catch Mod:get_request_limit(get(net_if))) of
	{'EXIT', _} ->
            {error, not_supported};
        Else ->
            Else
    end;
handle_get_request_limit(_) ->
    {error, not_supported}.

handle_set_request_limit(#state{net_if_mod = Mod}, NewLimit) 
  when Mod =/= undefined ->
    case (catch Mod:set_request_limit(get(net_if), NewLimit)) of
	{'EXIT', _} ->
            {error, not_supported};
        Else ->
            Else
    end;
handle_set_request_limit(_, _) ->
    {error, not_supported}.


agent_info(#state{worker = W, set_worker = SW}) -> 
    case (catch get_agent_info(W, SW)) of
	Info when is_list(Info) ->
	    Info;
	E ->
	    [{error, E}]
    end.

get_agent_info(W, SW) ->
    MASz   = proc_mem(self()),
    WSz    = proc_mem(W),
    SWSz   = proc_mem(SW),
    ATSz   = tab_mem(snmp_agent_table),
    CCSz   = tab_mem(snmp_community_cache),
    VacmSz = tab_mem(snmpa_vacm),
    [{process_memory, [{master_agent, MASz}, 
		       {worker,       WSz}, 
		       {set_worker,   SWSz}]}, 
     {db_memory, [{agent,           ATSz}, 
		  {community_cache, CCSz}, 
		  {vacm,            VacmSz}]}].

proc_mem(P) when is_pid(P) ->
    case (catch erlang:process_info(P, memory)) of
	{memory, Sz} when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end;
proc_mem(_) ->
    undefined.

tab_mem(T) ->
    case (catch ets:info(T, memory)) of
	Sz when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end.

net_if_info(#state{net_if_mod = Mod}) when Mod =/= undefined ->
    case (catch Mod:info(get(net_if))) of
	Info when is_list(Info) ->
	    Info;
	E ->
	    [{error, E}]
    end;
net_if_info(_) ->
    %% This could be a result of a code upgrade
    %% Make best effert
    [{process_memory, proc_mem(get(net_if))}].

note_store_info(#state{note_store = NS}) ->
    case (catch snmp_note_store:info(NS)) of
	Info when is_list(Info) ->
	    Info;
	E ->
	    [{error, E}]
    end.

symbolic_store_info() ->
    case (catch snmpa_symbolic_store:info()) of
	Info when is_list(Info) ->
	    Info;
	E ->
	    [{error, E}]
    end.

local_db_info() ->
    case (catch snmpa_local_db:info()) of
	Info when is_list(Info) ->
	    Info;
	E ->
	    [{error, E}]
    end.

mib_server_info() ->
    case (catch snmpa_mib:info(get(mibserver))) of
	Info when is_list(Info) ->
	    Info;
	E ->
	    [{error, E}]
    end.
	
get_stats_counters() ->
    Counters = snmpa_mpd:counters(),
    get_stats_counters(Counters, []).

get_stats_counters([], Acc) ->
    lists:reverse(Acc);
get_stats_counters([Counter|Counters], Acc) ->
    case ets:lookup(snmp_agent_table, Counter) of
	[CounterVal] ->
	    get_stats_counters(Counters, [CounterVal|Acc]);
	_ ->
	    get_stats_counters(Counters, Acc)
    end.


%% ---------------------------------------------------------------------

%% info_msg(F, A) ->
%%     ?snmpa_info(F, A).

warning_msg(F, A) ->
    ?snmpa_warning(F, A).

error_msg(F, A) ->
    ?snmpa_error(F, A).

%% --- 

config_err(F, A) ->
    snmpa_error:config_err(F, A).

user_err(F, A) ->
    snmpa_error:user_err(F, A).


%% ---------------------------------------------------------------------

maybe_call(Server, Req) ->
    case (wis(Server) =:= self()) of
	false ->
	    call(Server, Req);
	true ->
	    Server ! Req
    end.

call(Server, Req) ->
    gen_server:call(Server, Req, infinity).

cast(Server, Msg) ->
    gen_server:cast(Server, Msg).


%% ---------------------------------------------------------------------

get_verbosity(Opts) ->
    get_option(verbosity, Opts, ?default_verbosity).

get_mibs(Opts) ->
    get_option(mibs, Opts, []).

get_mib_storage(Opts) ->
    get_option(mib_storage, Opts).

get_set_mechanism(Opts) ->
    get_option(set_mechanism, Opts, snmpa_set).

get_get_mechanism(Opts) ->
    get_option(get_mechanism, Opts, snmpa_get).

get_authentication_service(Opts) ->
    get_option(authentication_service, Opts, snmpa_acm).

get_multi_threaded(Opts) ->
    get_option(multi_threaded, Opts, false).

get_versions(Opts) ->
    get_option(versions, Opts, [v1,v2,v3]).

get_gb_max_vbs(Opts) ->
    get_option(gb_max_vbs, Opts, infinity).

get_note_store_opt(Opts) ->
    get_option(note_store, Opts, []).

get_net_if_opt(Opts) ->
    get_option(net_if, Opts, []).

get_net_if_verbosity(Opts) ->
    get_option(verbosity, Opts, silence).

get_net_if_module(Opts) ->
    get_option(module, Opts, snmpa_net_if).

get_net_if_options(Opts) ->
    get_option(options, Opts, []).


net_if_verbosity(Pid,Verbosity) when is_pid(Pid) ->
    Pid ! {verbosity,Verbosity};
net_if_verbosity(_Pid,_Verbosity) ->
    ok.


get_option(Key, Opts) ->
    snmp_misc:get_option(Key, Opts).

get_option(Key, Opts, Default) ->
    snmp_misc:get_option(Key, Opts, Default).


%% ---------------------------------------------------------------------


%% i(F) ->
%%     i(F, []).

%% i(F, A) ->
%%     io:format("~p: " ++ F ++ "~n", [?MODULE|A]).



