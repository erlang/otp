%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2021. All Rights Reserved.
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

-module(snmpm_server).

%%----------------------------------------------------------------------
%% This module implements a simple SNMP manager for Erlang.
%%
%%----------------------------------------------------------------------

%% User interface
-export([start_link/0, stop/0, 
	 is_started/0, 

	 load_mib/1, unload_mib/1, 

	 register_user/4, register_user_monitor/4, unregister_user/1, 

	 sync_get/4, 
	 async_get/4, 
	 sync_get_next/4, 
	 async_get_next/4, 
	 sync_get_bulk/6, 
	 async_get_bulk/6, 
	 sync_set/4, 
	 async_set/4, 
	 cancel_async_request/2,

	 %% discovery/2, discovery/3, discovery/4, discovery/5, discovery/6, 

	 get_log_type/0,      set_log_type/1, 

	 reconfigure/0,

	 info/0, 
	 verbosity/1, verbosity/2,
         restart/1

	]).


%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).

%% GCT exports
-export([gct_init/1, gct/2]).

%% NIS exports
-export([nis_init/2, nis_loop/1]).

%% CallBack Proxy exports
-export([cbproxy_loop/1,
	 do_handle_error/4,
	 do_handle_pdu/5,
	 do_handle_agent/9,
	 do_handle_trap/9,
	 do_handle_inform/10,
	 do_handle_report/9]).


-include("snmpm_internal.hrl").
-include("snmp_debug.hrl").
-include("snmp_types.hrl").
-include("STANDARD-MIB.hrl").
-include("SNMP-FRAMEWORK-MIB.hrl").
-include("snmp_verbosity.hrl").


%%----------------------------------------------------------------------

-define(SERVER, ?MODULE).

-define(DEFAULT_SYNC_TIMEOUT,           timer:seconds(5)).
-define(DEFAULT_SYNC_GET_TIMEOUT,       ?DEFAULT_SYNC_TIMEOUT).
-define(DEFAULT_SYNC_GET_NEXT_TIMEOUT,  ?DEFAULT_SYNC_TIMEOUT).
-define(DEFAULT_SYNC_GET_BULK_TIMEOUT,  ?DEFAULT_SYNC_TIMEOUT).
-define(DEFAULT_SYNC_SET_TIMEOUT,       ?DEFAULT_SYNC_TIMEOUT).

-define(DEFAULT_ASYNC_TIMEOUT,          timer:seconds(5)).
-define(DEFAULT_ASYNC_GET_TIMEOUT,      ?DEFAULT_ASYNC_TIMEOUT).
-define(DEFAULT_ASYNC_GET_NEXT_TIMEOUT, ?DEFAULT_ASYNC_TIMEOUT).
-define(DEFAULT_ASYNC_GET_BULK_TIMEOUT, ?DEFAULT_ASYNC_TIMEOUT).
-define(DEFAULT_ASYNC_SET_TIMEOUT,      ?DEFAULT_ASYNC_TIMEOUT).

-define(SNMP_AGENT_PORT,                161).

-define(SYNC_GET_TIMEOUT(SendOpts), 
	get_opt(timeout, ?DEFAULT_SYNC_GET_TIMEOUT, SendOpts)).
-define(SYNC_GET_NEXT_TIMEOUT(SendOpts), 
	get_opt(timeout, ?DEFAULT_SYNC_GET_NEXT_TIMEOUT, SendOpts)).
-define(SYNC_GET_BULK_TIMEOUT(SendOpts), 
	get_opt(timeout, ?DEFAULT_SYNC_GET_BULK_TIMEOUT, SendOpts)).
-define(SYNC_SET_TIMEOUT(SendOpts), 
	get_opt(timeout, ?DEFAULT_SYNC_SET_TIMEOUT, SendOpts)).

-define(ASYNC_GET_TIMEOUT(SendOpts), 
	get_opt(timeout, ?DEFAULT_ASYNC_GET_TIMEOUT, SendOpts)).
-define(ASYNC_GET_NEXT_TIMEOUT(SendOpts), 
	get_opt(timeout, ?DEFAULT_ASYNC_GET_NEXT_TIMEOUT, SendOpts)).
-define(ASYNC_GET_BULK_TIMEOUT(SendOpts), 
	get_opt(timeout, ?DEFAULT_ASYNC_GET_BULK_TIMEOUT, SendOpts)).
-define(ASYNC_SET_TIMEOUT(SendOpts), 
	get_opt(timeout, ?DEFAULT_ASYNC_SET_TIMEOUT, SendOpts)).

-define(GET_EXTRA(SendOpts), get_opt(extra, ?DEFAULT_EXTRA_INFO, SendOpts)).

-ifdef(snmp_debug).
-define(GS_START_LINK(Args),
	gen_server:start_link({local, ?SERVER}, ?MODULE, Args, 
			      [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Args),
	gen_server:start_link({local, ?SERVER}, ?MODULE, Args, [])).
-endif.


-define(CBP_AWT_MAX, 60*60*1000*1000).
-define(CBP_CNT_MAX, 16#FFFFFFFF).


%%----------------------------------------------------------------------

-record(state,
	{parent,
	 gct,
	 note_store,
	 note_store_ref,
	 net_if,
	 net_if_mod,
	 net_if_ref,

         %% NetIF supervision
         %% This (config) option defines if/how the "server"
         %% shall supervise the net-if process.
         %% And by "supervise" we don't meant in the way a
         %% *supervisor" supervisor. This is "active" supervision
         %% (basically, send ping and expect pong back).
         %% There are two alternatives:
         %%      none - No supervision (default)
         %%      {PingInterval, PongTimeout}
         %%         PingInterval :: pos_integer()
         %%            Time between a successful test and the next.
         %%         PongInterval :: pos_integer()
         %%            Time the NetIF process has to answer a ping
         %%      
	 nis     :: none | {pos_integer(), pos_integer()},
         nis_pid :: undefined | pid(), % Pid of the process doing the actual sup

	 req,  %%  ???? Last request id in outgoing message
	 oid,  %%  ???? Last oid in request outgoing message
	 mini_mib,
         %% temporary: create a new (temporary) proxy process for each callback.
         %% transient: create one proxy process per known agent 
         %%            Its created "on the fly" and lives for as long as
         %%            there is activity (inactivity for a "long" time,
         %%            will cause it to terminate).
         %%            Not currently used!
         %% permanent: create one (named) static callback proxy process.
         cbproxy :: temporary | permanent,
	 cbproxy_pid % Pid of the callback proxy *if* cbp = permanent
	}
       ).

%% The active state is to ensure that nothing unpleasant happens
%% during (after) a code_change. At the initial start of the
%% application, this process (GCT) will make one run and then
%% deactivate (unless some async request has been issued in the
%% meantime).
-record(gct, {parent, state = active, timeout}).

-record(request, 
	{id, 
	 user_id,
	 reg_type,
	 target,
	 domain,
	 address,
	 type, 
	 data, 
	 ref, 
	 mon, 
	 from,
	 discovery = false, 
	 expire = infinity % When shall the request expire (time in ms)
	}
       ). 

-record(monitor,
	{id, 
	 mon,
	 proc
	}
       ).


-define(CBPROXY,     snmpm_server_cbproxy).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    ?d("start_link -> entry", []),
    Args = [],
    ?GS_START_LINK(Args).

stop() ->
    call(stop).

is_started() ->
    case (catch call(is_started, 1000)) of
	Bool when ((Bool =:= true) orelse (Bool =:= false)) ->
	    Bool;
	_ ->
	    false
    end.

load_mib(MibFile) when is_list(MibFile) ->
    call({load_mib, MibFile}).

unload_mib(Mib) when is_list(Mib) ->
    call({unload_mib, Mib}).


register_user(UserId, UserMod, UserData, DefaultAgentConfig) ->
    snmpm_config:register_user(UserId, UserMod, UserData, DefaultAgentConfig).

register_user_monitor(Id, Module, Data, DefaultAgentConfig) ->
    case register_user(Id, Module, Data, DefaultAgentConfig) of
	ok ->
	    case call({monitor_user, Id, self()}) of
		ok ->
		    ok;
		Error ->
		    unregister_user(Id),
		    Error
	    end;
	Error ->
	    Error
    end.

unregister_user(UserId) ->
    call({unregister_user, UserId}).


%% -- [sync] get --

%% The reason why we have a sync_get2 is to simplify backward 
%% compatibillity. 

sync_get(UserId, TargetName, Oids, Opts) ->
    call({sync_get, self(), UserId, TargetName, Oids, Opts}).



%% -- [async] get --

async_get(UserId, TargetName, Oids, SendOpts) ->
    call({async_get, self(), UserId, TargetName, Oids, SendOpts}).



%% -- [sync] get-next --

sync_get_next(UserId, TargetName, Oids, SendOpts) ->
    call({sync_get_next, self(), UserId, TargetName, Oids, SendOpts}).



%% -- [async] get-next --

async_get_next(UserId, TargetName, Oids, SendOpts) ->
    call({async_get_next, self(), UserId, TargetName, Oids, SendOpts}).



%% -- [sync] get-bulk --

sync_get_bulk(UserId, TargetName, NonRep, MaxRep, Oids, SendOpts) ->
    call({sync_get_bulk, self(), UserId, TargetName, 
	  NonRep, MaxRep, Oids, SendOpts}).



%% -- [async] get-bulk --

async_get_bulk(UserId, TargetName, NonRep, MaxRep, Oids, SendOpts) ->
    call({async_get_bulk, self(), UserId, TargetName, NonRep, MaxRep, 
	  Oids, SendOpts}).



%% -- [sync] set --

%% VarsAndValues is: {PlainOid, o|s|i, Value} (unknown mibs) | {Oid, Value} 
sync_set(UserId, TargetName, VarsAndVals, SendOpts) ->
    call({sync_set, self(), UserId, TargetName, VarsAndVals, SendOpts}).



%% -- [async] set --

async_set(UserId, TargetName, VarsAndVals, SendOpts) ->
    call({async_set, self(), UserId, TargetName, VarsAndVals, SendOpts}).



cancel_async_request(UserId, ReqId) ->
    call({cancel_async_request, UserId, ReqId}).


info() ->
    call(info).

verbosity(Verbosity) ->
    case ?vvalidate(Verbosity) of
	Verbosity ->
	    call({verbosity, Verbosity});
	_ ->
	    {error, {invalid_verbosity, Verbosity}}
    end.

verbosity(net_if = Ref, Verbosity) ->
    verbosity2(Ref, Verbosity);
verbosity(note_store = Ref, Verbosity) ->
    verbosity2(Ref, Verbosity).

verbosity2(Ref, Verbosity) ->
    case ?vvalidate(Verbosity) of
	Verbosity ->
	    call({verbosity, Ref, Verbosity});
	_ ->
	    {error, {invalid_verbosity, Verbosity}}
    end.


restart(net_if = What) ->
    cast({restart, What}).


get_log_type() ->
    call(get_log_type).

set_log_type(NewType) ->
    call({set_log_type, NewType}).

reconfigure() ->
    call(reconfigure).


%%----------------------------------------------------------------------
%% Options: List of
%%  {community, String ("public" is default} 
%%  {mibs, List of Filenames}
%%  {trap_udp, integer() (default 5000)}
%%  {conf_dir, string()}
%%  {log_dir,  string()}
%%  {db_dir,   string()}
%%  {db_repair, true | false}
%%----------------------------------------------------------------------
init(_) ->
    ?d("init -> entry", []),
    case (catch do_init()) of
	{ok, State} ->
	    {ok, State};
	{error, Reason} ->
	    {stop, Reason}
    end.


%% Put all config stuff in a snmpm_config module/process.
%% Tables should be protected so that it is cheap to 
%% read. Writing has to go through the interface...

do_init() ->
    process_flag(trap_exit, true),
    {ok, Prio} = snmpm_config:system_info(prio),
    process_flag(priority, Prio),

    {ok, Verbosity} = snmpm_config:system_info(server_verbosity),
    put(sname, mse),
    put(verbosity, Verbosity),
    ?vlog("starting", []),

    %% Start the garbage collector timer process
    {ok, Timeout} = snmpm_config:system_info(server_timeout),
    {ok, GCT} = gct_start(Timeout),

    %% What kind of CallBack Proxy (temporary by default)
    {CBProxy, CBPPid} =
         case snmpm_config:system_info(server_cbproxy) of
             {ok, permanent = CBP} ->
                 %% Start CallBack Proxy process
                 {CBP, cbproxy_start()};
             {ok, CBP} ->
                 {CBP, undefined}
         end,

    %% -- Create request table --
    ets:new(snmpm_request_table, 
	    [set, protected, named_table, {keypos, #request.id}]),

    %% -- Create monitor table --
    ets:new(snmpm_monitor_table, 
	    [set, protected, named_table, {keypos, #monitor.id}]),

    %% -- Start the note-store and net-if processes --
    {NoteStore, NoteStoreRef}      = do_init_note_store(Prio),
    {NetIf, NetIfModule, NetIfRef} = do_init_net_if(NoteStore),

    %% -- (maybe) Start the NetIF "supervisor" --
    {NIS, NISPid} =
        case snmpm_config:system_info(server_nis) of
            {ok, none = V} ->
                {V, undefined};
            {ok, {PingTO, PongTO} = V} ->
                {V, nis_start(NetIf, PingTO, PongTO)}
        end,

    MiniMIB = snmpm_config:make_mini_mib(),
    State = #state{mini_mib       = MiniMIB,
		   gct            = GCT,
		   note_store     = NoteStore,
		   note_store_ref = NoteStoreRef,
		   net_if         = NetIf,
		   net_if_mod     = NetIfModule,
		   net_if_ref     = NetIfRef,
                   nis            = NIS,
                   nis_pid        = NISPid,
                   cbproxy        = CBProxy,
		   cbproxy_pid    = CBPPid},
    ?vlog("started", []),
    {ok, State}.


do_init_note_store(Prio) ->
    ?vdebug("try start note store", []),
    {ok, Verbosity} = snmpm_config:system_info(note_store_verbosity),
    {ok, Timeout}   = snmpm_config:system_info(note_store_timeout),
    Opts = [{sname,     mns}, 
	    {verbosity, Verbosity}, 
	    {timeout,   Timeout}],
    case snmpm_misc_sup:start_note_store(Prio, Opts) of
	{ok, Pid} ->
	    ?vtrace("do_init_note_store -> Pid: ~p", [Pid]),
	    Ref = erlang:monitor(process, Pid),
	    {Pid, Ref};
	{error, Reason} ->
	    ?vlog("failed starting note-store - Reason: "
		  "~n   Reason: ~p"
		  "~n", [Reason]),
	    throw({error, {failed_starting_note_store, Reason}})
    end.

do_init_net_if(NoteStore) ->
    ?vdebug("try start net-if", []),
    {ok, NetIfModule} = snmpm_config:system_info(net_if_module),
    case snmpm_misc_sup:start_net_if(NetIfModule, NoteStore) of
	{ok, Pid} ->
	    ?vtrace("do_init_net_if -> Pid: ~p", [Pid]),
	    Ref = erlang:monitor(process, Pid),
	    {Pid, NetIfModule, Ref};
	{error, Reason} ->
	    ?vlog("failed starting net-if - Reason: "
		  "~n   Reason: ~p"
		  "~n", [Reason]),
	    throw({error, {failed_starting_net_if, Reason}})
    end.


%% ---------------------------------------------------------------------
%% ---------------------------------------------------------------------

handle_call({monitor_user, Id, Pid}, _From, State) when is_pid(Pid) ->
    ?vlog("received monitor_user request for ~w [~w]", [Id, Pid]),
    Reply = 
	case ets:lookup(snmpm_monitor_table, Id) of
	    [#monitor{proc = Pid}] ->
		?vdebug("already monitored", []),
		ok;

	    [#monitor{proc = OtherPid}] ->
		?vinfo("already registered to ~w", [OtherPid]),
		{error, {already_monitored, OtherPid}};

	    [] ->
		Ref = erlang:monitor(process, Pid),
		?vtrace("monitor ref: ~w", [Ref]),
		Mon = #monitor{id = Id, mon = Ref, proc = Pid},
		ets:insert(snmpm_monitor_table, Mon),
		ok
	end,
    {reply, Reply, State};

handle_call({unregister_user, UserId}, _From, State) ->
    ?vlog("received request to unregister user ~p", [UserId]),

    %% 1) If this user is monitored, then demonitor
    ?vtrace("handle_call(unregister_user) -> maybe demonitor", []),
    case ets:lookup(snmpm_monitor_table, UserId) of
	[] ->
	    ok;
	[#monitor{mon = M}] ->
	    maybe_demonitor(M), % This is really overkill (meybe_), but...
	    ok
    end,

    %% 2) Delete all outstanding requests from this user
    ?vtrace("handle_call(unregister_user) -> "
	    "delete all outstanding requests for user", []),
    Pat = #request{user_id = UserId, 
		   id = '$1', ref = '$2', mon = '$3', _ = '_'},
    Match = ets:match(snmpm_request_table, Pat),
    ?vtrace("handle_call(unregister_user) -> Match: ~p", [Match]),
    F1 = fun([ReqId, Ref, MonRef]) -> 
		 ets:delete(snmpm_request_table, ReqId),
		 cancel_timer(Ref),
		 maybe_demonitor(MonRef),
		 ok
	end,
    lists:foreach(F1, Match),
    
    %% 3) Unregister all agents registered by this user
    ?vdebug("handle_call(unregister_user) -> "
	    "unregister all agents registered by user", []),
    Agents = snmpm_config:which_agents(UserId),
    ?vtrace("handle_call(unregister_user) -> Agents: ~p", [Agents]),
    F2 = fun(TargetName) ->
		 snmpm_config:unregister_agent(UserId, TargetName)
	 end,
    lists:foreach(F2, Agents),

    %% 4) Unregister the user
    ?vdebug("handle_call(unregister_user) -> unregister user", []),
    Reply = snmpm_config:unregister_user(UserId),
    ?vtrace("handle_call(unregister_user) -> Reply: ~p", [Reply]),
    {reply, Reply, State};


%% We will reply to this request later, when the reply comes in from the
%% agent, or when the timeout hits (unless we get an error now).
handle_call({sync_get, Pid, UserId, TargetName, Oids, SendOpts}, 
	    From, State) ->
    ?vlog("[~p,~p] received sync_get request for: "
	  "~n   ~p", [UserId, TargetName, Oids]),
    case (catch handle_sync_get(Pid, 
				UserId, TargetName, Oids, SendOpts, 
				From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;

handle_call({sync_get_next, Pid, UserId, TargetName, Oids, SendOpts}, 
	    From, State) ->
    ?vlog("[~p,~p] received sync_get_next request for: "
	  "~n   ~p", [UserId, TargetName, SendOpts]),
    case (catch handle_sync_get_next(Pid, 
				     UserId, TargetName, Oids, SendOpts, 
				     From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;


%% Check agent version? This op not in v1
handle_call({sync_get_bulk, 
	     Pid, UserId, TargetName, NonRep, MaxRep, Oids, SendOpts}, 
	    From, State) ->
    ?vlog("[~p,~p] received sync_get_bulk request for: "
	  "~n   ~p", [UserId, TargetName, Oids]),
    case (catch handle_sync_get_bulk(Pid, 
				     UserId, TargetName, NonRep, MaxRep, Oids, 
				     SendOpts, From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;


handle_call({sync_set, 
	     Pid, UserId, TargetName, VarsAndVals, SendOpts}, 
	    From, State) ->
    ?vlog("[~p,~p] received sync_set request for: "
	  "~n   ~p", [UserId, TargetName, VarsAndVals]),
    case (catch handle_sync_set(Pid, 
				UserId, TargetName, VarsAndVals, SendOpts, 
				From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;


handle_call({async_get, Pid, UserId, TargetName, Oids, SendOpts}, 
	    _From, State) ->
    ?vlog("[~p,~p] received async_get request for: "
	  "~n   ~p", [UserId, TargetName, Oids]),
    Reply = (catch handle_async_get(Pid, 
				    UserId, TargetName, Oids, SendOpts, 
				    State)),
    {reply, Reply, State};


handle_call({async_get_next, Pid, UserId, TargetName, Oids, SendOpts}, 
	    _From, State) ->
    ?vlog("[~p,~p] received async_get_next request for: "
	  "~n   ~p", [UserId, TargetName, Oids]),
    Reply = (catch handle_async_get_next(Pid, 
					 UserId, TargetName, Oids, SendOpts, 
					 State)),
    {reply, Reply, State};


%% Check agent version? This op not in v1
handle_call({async_get_bulk, 
	     Pid, UserId, TargetName, NonRep, MaxRep, Oids, SendOpts}, 
	    _From, State) ->
    ?vlog("[~p,~p] received async_get_bulk request for: "
	  "~n   ~p", [UserId, TargetName, Oids]),
    Reply = (catch handle_async_get_bulk(Pid, 
					 UserId, TargetName, 
					 NonRep, MaxRep, Oids, SendOpts, 
					 State)),
    {reply, Reply, State};


handle_call({async_set, 
	     Pid, UserId, TargetName, VarsAndVals, SendOpts}, 
	    _From, State) ->
    ?vlog("[~p,~p] received async_set request for: "
	  "~n   ~p", [UserId, TargetName, VarsAndVals]),
    Reply = (catch handle_async_set(Pid, 
				    UserId, TargetName, VarsAndVals, SendOpts, 
				    State)),
    {reply, Reply, State};


handle_call({cancel_async_request, UserId, ReqId}, _From, State) ->
    ?vlog("received cancel_async_request request", []),
    Reply = (catch handle_cancel_async_request(UserId, ReqId, State)),
    {reply, Reply, State};


handle_call({load_mib, Mib}, _From, State) ->
    ?vlog("received load_mib request", []),
    case snmpm_config:load_mib(Mib) of
	ok ->
	    MiniMIB = snmpm_config:make_mini_mib(),
	    {reply, ok, State#state{mini_mib = MiniMIB}};
	Error ->
	    {reply, Error, State}
    end;


handle_call({unload_mib, Mib}, _From, State) ->
    ?vlog("received unload_mib request", []),
    case snmpm_config:unload_mib(Mib) of
	ok ->
	    MiniMIB = snmpm_config:make_mini_mib(),
	    {reply, ok, State#state{mini_mib = MiniMIB}};
	Error ->
	    {reply, Error, State}
    end;

handle_call({verbosity, Verbosity}, _From, State) ->
    ?vlog("received verbosity request", []),
    put(verbosity, Verbosity),
    {reply, ok, State};

handle_call({verbosity, net_if, Verbosity}, _From, 
	    #state{net_if = Pid, net_if_mod = Mod} = State) ->
    ?vlog("received net_if verbosity request", []),
    Mod:verbosity(Pid, Verbosity),
    {reply, ok, State};

handle_call({verbosity, note_store, Verbosity}, _From, 
	    #state{note_store = Pid} = State) ->
    ?vlog("received note_store verbosity request", []),
    snmp_note_store:verbosity(Pid, Verbosity),
    {reply, ok, State};

handle_call(reconfigure, _From, State) ->
    ?vlog("received reconfigure request", []),
    Reply = {error, not_implemented},
    {reply, Reply, State};

handle_call(info, _From, State) ->
    ?vlog("received info request", []),
    Reply = get_info(State), 
    {reply, Reply, State};

handle_call(is_started, _From, State) ->
    ?vlog("received is_started request", []),
    IsStarted = is_started(State), 
    {reply, IsStarted, State};

handle_call(get_log_type, _From, State) ->
    ?vlog("received get_log_type request", []),
    Reply = handle_get_log_type(State), 
    {reply, Reply, State};

handle_call({set_log_type, NewType}, _From, State) ->
    ?vlog("received set_log_type request: "
	  "~n   NewType: ~p", [NewType]),
    Reply = handle_set_log_type(State, NewType), 
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    ?vlog("received stop request", []),
    {stop, normal, ok, State};


handle_call(Req, _From, State) ->
    warning_msg("received unknown request: ~n~p", [Req]),
    {reply, {error, unknown_request}, State}.


handle_cast({restart, net_if},
	    #state{net_if = Pid} = State) ->
    ?vlog("received net_if (~p) restart message", [Pid]),
    %% We will get an exit signel/message, which will trigger a (re-)start
    exit(Pid, kill),
    {noreply, State};

handle_cast(Msg, State) ->
    warning_msg("received unknown message: ~n~p", [Msg]),
    {noreply, State}.


handle_info({sync_timeout, ReqId, From}, State) ->
    ?vlog("received sync_timeout [~w] message", [ReqId]),
    handle_sync_timeout(ReqId, From, State),
    {noreply, State};


handle_info({snmp_error, Pdu, Reason}, State) ->
    ?vlog("received snmp_error message", []),
    handle_snmp_error(Pdu, Reason, State),
    {noreply, State};

handle_info({snmp_error, Reason, Domain, Addr}, State) ->
    ?vlog("received snmp_error message", []),
    handle_snmp_error(Domain, Addr, -1, Reason, State),
    {noreply, State};

handle_info({snmp_error, ReqId, Reason, Domain, Addr}, State) ->
    ?vlog("received snmp_error message", []),
    handle_snmp_error(Domain, Addr, ReqId, Reason, State),
    {noreply, State};


handle_info({snmp_pdu, Pdu, Domain, Addr}, State) ->
    ?vlog("received snmp_pdu message", []),
    handle_snmp_pdu(Pdu, Domain, Addr, State),
    {noreply, State};


handle_info({snmp_trap, Trap, Domain, Addr}, State) ->
    ?vlog("received snmp_trap message", []),
    handle_snmp_trap(Trap, Domain, Addr, State),
    {noreply, State};


handle_info({snmp_inform, Ref, Pdu, Domain, Addr}, State) ->
    ?vlog("received snmp_inform message", []),
    handle_snmp_inform(Ref, Pdu, Domain, Addr, State),
    {noreply, State};


handle_info({snmp_report, {ok, Pdu}, Domain, Addr}, State) ->
    handle_snmp_report(Pdu, Domain, Addr, State),
    {noreply, State};

handle_info({snmp_report, {error, ReqId, Info, Pdu}, Domain, Addr}, State) ->
    handle_snmp_report(ReqId, Pdu, Info, Domain, Addr, State),
    {noreply, State};


handle_info(gc_timeout, #state{gct = GCT} = State) ->
    ?vlog("received gc_timeout message", []),
    handle_gc(GCT),
    {noreply, State};


handle_info({'DOWN', _MonRef, process, Pid, Reason}, 
	    #state{net_if = Pid} = State) ->
    ?vlog("received 'DOWN' message regarding net_if (~p)", [Pid]),
    NewState = handle_netif_down(State, Reason),
    {noreply, NewState};


handle_info({'DOWN', _MonRef, process, Pid, _Reason}, 
	    #state{note_store = Pid, 
		   net_if     = NetIf,
		   net_if_mod = Mod} = State) ->
    ?vlog("received 'DOWN' message regarding note_store (~p)", [Pid]),
    {ok, Prio} = snmpm_config:system_info(prio),
    {NoteStore, Ref} = do_init_note_store(Prio),
    Mod:note_store(NetIf, NoteStore),
    {noreply, State#state{note_store = NoteStore, note_store_ref = Ref}};


handle_info({'DOWN', MonRef, process, Pid, Reason}, State) ->
    ?vlog("received 'DOWN' message (~w) from ~w "
	  "~n   Reason: ~p", [MonRef, Pid, Reason]),
    handle_down(MonRef),
    {noreply, State};


handle_info({'EXIT', Pid, Reason}, #state{gct = Pid} = State) ->
    ?vlog("received 'EXIT' message from the GCT (~w) process: "
	  "~n   ~p", [Pid, Reason]),
    {ok, Timeout} = snmpm_config:system_info(server_timeout),
    {ok, GCT} = gct_start(Timeout),
    {noreply, State#state{gct = GCT}};


handle_info({'EXIT', Pid, Reason}, #state{cbproxy_pid = Pid} = State) ->
    warning_msg("CallBack Proxy (~w) process crashed: "
		"~n   ~p", [Pid, Reason]),
    NewCBP = cbproxy_start(),
    {noreply, State#state{cbproxy_pid = NewCBP}};


handle_info({'EXIT', Pid, Reason}, #state{net_if  = NetIF,
                                          nis     = {PingTO, PongTO},
                                          nis_pid = Pid} = State) ->
    warning_msg("NetIF (active) supervisor (~w) process crashed: "
		"~n   ~p", [Pid, Reason]),
    NewNIS = nis_start(NetIF, PingTO, PongTO),
    {noreply, State#state{nis_pid = NewNIS}};


handle_info(Info, State) ->
    warning_msg("received unknown info: ~n   ~p", [Info]),
    {noreply, State}.


%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------
                                                                              
% downgrade
code_change({down, _Vsn}, #state{gct = Pid} = State, _Extra) ->
    ?d("code_change(down) -> entry", []),
    gct_code_change(Pid),
    {ok, State};
 
% upgrade
code_change(_Vsn, #state{gct = Pid} = State0, _Extra) ->
    ?d("code_change(up) -> entry", []),
    gct_code_change(Pid),
    MiniMIB = snmpm_config:make_mini_mib(),
    State = State0#state{mini_mib = MiniMIB},
    {ok, State}.

 
%%----------------------------------------------------------
%% Terminate
%%----------------------------------------------------------
                                                                              
terminate(Reason, #state{nis_pid = NIS, gct = GCT, cbproxy = CBP}) ->
    ?vdebug("terminate: ~p",[Reason]),
    nis_stop(NIS),
    cbproxy_stop(CBP),
    gct_stop(GCT),
    snmpm_misc_sup:stop_note_store(),
    snmpm_misc_sup:stop_net_if(),
    ok.


%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------

handle_sync_get(Pid, UserId, TargetName, Oids, SendOpts, From, State) -> 
    ?vtrace("handle_sync_get -> entry with"
	    "~n   Pid:        ~p"
	    "~n   UserId:     ~p"
	    "~n   TargetName: ~p"
	    "~n   Oids:       ~p"
	    "~n   SendOpts:   ~p"
	    "~n   From:       ~p", 
	    [Pid, UserId, TargetName, Oids, SendOpts, From]),
    case agent_data(TargetName, SendOpts) of
	{ok, RegType, Domain, Addr, Vsn, MsgData} ->
	    ?vtrace("handle_sync_get -> send a ~p message", [Vsn]),
	    Extra   = ?GET_EXTRA(SendOpts), 
	    ReqId   = send_get_request(Oids, Vsn, MsgData, 
				       Domain, Addr,
				       Extra, State),
	    ?vdebug("handle_sync_get -> ReqId: ~p", [ReqId]),
	    Msg     = {sync_timeout, ReqId, From},
	    Timeout = ?SYNC_GET_TIMEOUT(SendOpts), 
	    Ref     = erlang:send_after(Timeout, self(), Msg),
	    MonRef  = erlang:monitor(process, Pid),
	    ?vtrace("handle_sync_get -> MonRef: ~p", [MonRef]),
	    Req     = #request{id       = ReqId,
			       user_id  = UserId, 
			       reg_type = RegType, 
			       target   = TargetName, 
			       domain   = Domain, 
			       address  = Addr,
			       type     = get, 
			       data     = MsgData, 
			       ref      = Ref, 
			       mon      = MonRef, 
			       from     = From},
	    ets:insert(snmpm_request_table, Req),
	    ok;
	Error ->
	    ?vinfo("failed retrieving agent data for get:"
		   "~n   TargetName: ~p"
		   "~n   Error:      ~p", [TargetName, Error]),
	    Error
    end.
    
handle_sync_get_next(Pid, UserId, TargetName, Oids, SendOpts, 
		     From, State) ->
    ?vtrace("handle_sync_get_next -> entry with"
	    "~n   Pid:        ~p"
	    "~n   UserId:     ~p"
	    "~n   TargetName: ~p"
	    "~n   Oids:       ~p"
	    "~n   SendOpts:   ~p"
	    "~n   From:       ~p", 
	    [Pid, UserId, TargetName, Oids, SendOpts, From]),
    case agent_data(TargetName, SendOpts) of
	{ok, RegType, Domain, Addr, Vsn, MsgData} ->
	    ?vtrace("handle_sync_get_next -> send a ~p message", [Vsn]),
	    Extra   = ?GET_EXTRA(SendOpts), 
	    ReqId   = send_get_next_request(Oids, Vsn, MsgData, 
					    Domain, Addr,
					    Extra, State),
	    ?vdebug("handle_sync_get_next -> ReqId: ~p", [ReqId]),
	    Msg     = {sync_timeout, ReqId, From},
	    Timeout = ?SYNC_GET_NEXT_TIMEOUT(SendOpts), 
	    Ref     = erlang:send_after(Timeout, self(), Msg),
	    MonRef  = erlang:monitor(process, Pid),
	    ?vtrace("handle_sync_get_next -> MonRef: ~p", [MonRef]),
	    Req     = #request{id       = ReqId,
			       user_id  = UserId, 
			       reg_type = RegType, 
			       target   = TargetName, 
			       domain   = Domain, 
			       address  = Addr,
			       type     = get_next, 
			       data     = MsgData, 
			       ref      = Ref, 
			       mon      = MonRef,
			       from     = From},
	    ets:insert(snmpm_request_table, Req),
	    ok;

	Error ->
	    ?vinfo("failed retrieving agent data for get-next:"
		   "~n   TargetName: ~p"
		   "~n   Error:      ~p", [TargetName, Error]),
	    Error
    end.


handle_sync_get_bulk(Pid, UserId, TargetName, NonRep, MaxRep, Oids, SendOpts, 
		     From, State) ->
    ?vtrace("handle_sync_get_bulk -> entry with"
	    "~n   Pid:        ~p"
	    "~n   UserId:     ~p"
	    "~n   TargetName: ~p"
	    "~n   NonRep:     ~p"
	    "~n   MaxRep:     ~p"
	    "~n   Oids:       ~p"
	    "~n   SendOpts:   ~p"
	    "~n   From:       ~p", 
	    [Pid, UserId, TargetName, NonRep, MaxRep, Oids, SendOpts, From]),
    case agent_data(TargetName, SendOpts) of
	{ok, RegType, Domain, Addr, Vsn, MsgData} ->
	    ?vtrace("handle_sync_get_bulk -> send a ~p message", [Vsn]),
	    Extra   = ?GET_EXTRA(SendOpts), 
	    ReqId   = send_get_bulk_request(Oids, Vsn, MsgData, 
					    Domain, Addr,
					    NonRep, MaxRep, Extra, State),
	    ?vdebug("handle_sync_get_bulk -> ReqId: ~p", [ReqId]),
	    Msg     = {sync_timeout, ReqId, From},
	    Timeout = ?SYNC_GET_BULK_TIMEOUT(SendOpts), 
	    Ref     = erlang:send_after(Timeout, self(), Msg),
	    MonRef  = erlang:monitor(process, Pid),
	    ?vtrace("handle_sync_get_bulk -> MonRef: ~p", [MonRef]),
	    Req     = #request{id       = ReqId,
			       user_id  = UserId, 
			       reg_type = RegType, 
			       target   = TargetName, 
			       domain   = Domain, 
			       address  = Addr,
			       type     = get_bulk, 
			       data     = MsgData, 
			       ref      = Ref, 
			       mon      = MonRef, 
			       from     = From},
	    ets:insert(snmpm_request_table, Req),
	    ok;

	Error ->
	    ?vinfo("failed retrieving agent data for get-bulk:"
		   "~n   TargetName: ~p"
		   "~n   Error:      ~p", [TargetName, Error]),
	    Error
    end.


handle_sync_set(Pid, UserId, TargetName, VarsAndVals, SendOpts, From, State) ->
    ?vtrace("handle_sync_set -> entry with"
	    "~n   Pid:         ~p"
	    "~n   UserId:      ~p"
	    "~n   TargetName:  ~p"
	    "~n   VarsAndVals: ~p"
	    "~n   SendOpts:    ~p"
	    "~n   From:        ~p", 
	    [Pid, UserId, TargetName, VarsAndVals, From]),
    case agent_data(TargetName, SendOpts) of
	{ok, RegType, Domain, Addr, Vsn, MsgData} ->
	    ?vtrace("handle_sync_set -> send a ~p message", [Vsn]),
	    Extra   = ?GET_EXTRA(SendOpts), 
	    ReqId   = send_set_request(VarsAndVals, Vsn, MsgData, 
				       Domain, Addr,
				       Extra, State),
	    ?vdebug("handle_sync_set -> ReqId: ~p", [ReqId]),
	    Msg     = {sync_timeout, ReqId, From},
	    Timeout = ?SYNC_SET_TIMEOUT(SendOpts), 
	    Ref     = erlang:send_after(Timeout, self(), Msg),
            MonRef  = erlang:monitor(process, Pid),
	    ?vtrace("handle_sync_set -> MonRef: ~p", [MonRef]),
	    Req     = #request{id       = ReqId,
			       user_id  = UserId, 
			       reg_type = RegType, 
			       target   = TargetName, 
			       domain   = Domain, 
			       address  = Addr,
			       type     = set, 
			       data     = MsgData, 
			       ref      = Ref, 
			       mon      = MonRef, 
			       from     = From},
	    ets:insert(snmpm_request_table, Req),
	    ok;

	Error ->
	    ?vinfo("failed retrieving agent data for set:"
		   "~n   TargetName: ~p"
		   "~n   Error:      ~p", [TargetName, Error]),
	    Error
    end.

 
handle_async_get(Pid, UserId, TargetName, Oids, SendOpts, State) ->
    ?vtrace("handle_async_get -> entry with"
	    "~n   Pid:        ~p"
	    "~n   UserId:     ~p"
	    "~n   TargetName: ~p"
	    "~n   Oids:       ~p"
	    "~n   SendOpts:   ~p",
	    [Pid, UserId, TargetName, Oids, SendOpts]),
    case agent_data(TargetName, SendOpts) of
	{ok, RegType, Domain, Addr, Vsn, MsgData} ->
	    ?vtrace("handle_async_get -> send a ~p message", [Vsn]),
	    Extra  = ?GET_EXTRA(SendOpts), 
	    ReqId  = send_get_request(Oids, Vsn, MsgData, 
				      Domain, Addr,
				      Extra, State),
	    ?vdebug("handle_async_get -> ReqId: ~p", [ReqId]),
	    Expire = ?ASYNC_GET_TIMEOUT(SendOpts), 
	    Req    = #request{id       = ReqId,
			      user_id  = UserId, 
			      reg_type = RegType, 
			      target   = TargetName, 
			      domain   = Domain, 
			      address  = Addr,
			      type     = get, 
			      data     = MsgData, 
			      expire   = snmp_misc:now(ms) + Expire},

	    ets:insert(snmpm_request_table, Req),
	    gct_activate(State#state.gct),
	    {ok, ReqId};

	Error ->
	    ?vinfo("failed retrieving agent data for get:"
		   "~n   TargetName: ~p"
		   "~n   Error:      ~p", [TargetName, Error]),
	    Error
    end.


handle_async_get_next(Pid, UserId, TargetName, Oids, SendOpts, State) ->
    ?vtrace("handle_async_get_next -> entry with"
	    "~n   Pid:        ~p"
	    "~n   UserId:     ~p"
	    "~n   TargetName: ~p"
	    "~n   Oids:       ~p"
	    "~n   SendOpts:   ~p",
	    [Pid, UserId, TargetName, Oids, SendOpts]),
    case agent_data(TargetName, SendOpts) of
	{ok, RegType, Domain, Addr, Vsn, MsgData} ->
	    ?vtrace("handle_async_get_next -> send a ~p message", [Vsn]),
	    Extra  = ?GET_EXTRA(SendOpts), 
	    ReqId  = send_get_next_request(Oids, Vsn, MsgData, 
					   Domain, Addr,
					   Extra, State),
	    ?vdebug("handle_async_get_next -> ReqId: ~p", [ReqId]),
	    Expire = ?ASYNC_GET_NEXT_TIMEOUT(SendOpts), 
	    Req    = #request{id       = ReqId,
			      user_id  = UserId, 
			      reg_type = RegType, 
			      target   = TargetName, 
			      domain   = Domain, 
			      address  = Addr,
			      type     = get_next, 
			      data     = MsgData, 
			      expire   = snmp_misc:now(ms) + Expire},

	    ets:insert(snmpm_request_table, Req),
	    gct_activate(State#state.gct),
	    {ok, ReqId};

	Error ->
	    ?vinfo("failed retrieving agent data for get-next:"
		   "~n   TargetName: ~p"
		   "~n   Error:      ~p", [TargetName, Error]),
	    Error
    end.


handle_async_get_bulk(Pid, 
		      UserId, TargetName, NonRep, MaxRep, Oids, SendOpts, 
		      State) ->
    ?vtrace("handle_async_get_bulk -> entry with"
	    "~n   Pid:        ~p"
	    "~n   UserId:     ~p"
	    "~n   TargetName: ~p"
	    "~n   NonRep:     ~p"
	    "~n   MaxRep:     ~p"
	    "~n   Oids:       ~p"
	    "~n   SendOpts:   ~p", 
	    [Pid, UserId, TargetName, NonRep, MaxRep, Oids, SendOpts]),
    case agent_data(TargetName, SendOpts) of
	{ok, RegType, Domain, Addr, Vsn, MsgData} ->
	    ?vtrace("handle_async_get_bulk -> send a ~p message", [Vsn]),
	    Extra  = ?GET_EXTRA(SendOpts), 
	    ReqId  = send_get_bulk_request(Oids, Vsn, MsgData, 
					   Domain, Addr,
					   NonRep, MaxRep, Extra, State),
	    ?vdebug("handle_async_get_bulk -> ReqId: ~p", [ReqId]),
	    Expire = ?ASYNC_GET_BULK_TIMEOUT(SendOpts), 
	    Req    = #request{id       = ReqId,
			      user_id  = UserId, 
			      reg_type = RegType, 
			      target   = TargetName, 
			      domain   = Domain, 
			      address  = Addr,
			      type     = get_bulk, 
			      data     = MsgData, 
			      expire   = snmp_misc:now(ms) + Expire},
	    ets:insert(snmpm_request_table, Req),
	    gct_activate(State#state.gct),
	    {ok, ReqId};

	Error ->
	    ?vinfo("failed retrieving agent data for get-bulk:"
		   "~n   TargetName: ~p"
		   "~n   Error:      ~p", [TargetName, Error]),
	    Error
    end.


handle_async_set(Pid, UserId, TargetName, VarsAndVals, SendOpts, State) ->
    ?vtrace("handle_async_set -> entry with"
	    "~n   Pid:         ~p"
	    "~n   UserId:      ~p"
	    "~n   TargetName:  ~p"
	    "~n   VarsAndVals: ~p"
	    "~n   SendOpts:    ~p",
	    [Pid, UserId, TargetName, VarsAndVals, SendOpts]),
    case agent_data(TargetName, SendOpts) of
	{ok, RegType, Domain, Addr, Vsn, MsgData} ->
	    ?vtrace("handle_async_set -> send a ~p message", [Vsn]),
	    Extra  = ?GET_EXTRA(SendOpts), 
	    ReqId  = send_set_request(VarsAndVals, Vsn, MsgData, 
				      Domain, Addr,
				      Extra, State),
	    ?vdebug("handle_async_set -> ReqId: ~p", [ReqId]),
	    Expire = ?ASYNC_SET_TIMEOUT(SendOpts), 
	    Req    = #request{id       = ReqId,
			      user_id  = UserId, 
			      reg_type = RegType, 
			      target   = TargetName, 
			      domain   = Domain, 
			      address  = Addr,
			      type     = set, 
			      data     = MsgData, 
			      expire   = snmp_misc:now(ms) + Expire},

	    ets:insert(snmpm_request_table, Req),
	    gct_activate(State#state.gct),
	    {ok, ReqId};

	Error ->
	    ?vinfo("failed retrieving agent data for set:"
		   "~n   TargetName: ~p"
		   "~n   Error:      ~p", [TargetName, Error]),
	    Error
    end.


handle_cancel_async_request(UserId, ReqId, _State) ->
    ?vtrace("handle_cancel_async_request -> entry with"
	    "~n   UserId: ~p"
	    "~n   ReqId:  ~p", [UserId, ReqId]),
    case ets:lookup(snmpm_request_table, ReqId) of
	[#request{user_id = UserId,
		  ref     = Ref}] ->
	    ?vdebug("handle_cancel_async_request -> demonitor and cancel timer"
		    "~n   Ref: ~p", [Ref]),
	    cancel_timer(Ref),
	    ets:delete(snmpm_request_table, ReqId),
	    ok;
	
	[#request{user_id = OtherUserId}] ->
	    ?vinfo("handle_cancel_async_request -> Not request owner"
		    "~n   OtherUserId: ~p", [OtherUserId]),
	    {error, {not_owner, OtherUserId}};
	
	[] ->
	    ?vlog("handle_cancel_async_request -> not found", []),
	    {error, not_found}
    end.

handle_get_log_type(#state{net_if = Pid, net_if_mod = Mod}) ->
    case (catch Mod:get_log_type(Pid)) of
	{'EXIT', _} ->
	    {error, not_supported};
	Else ->
	    Else
    end.

handle_set_log_type(#state{net_if = Pid, net_if_mod = Mod}, NewType) ->
    case (catch Mod:set_log_type(Pid, NewType)) of
	{'EXIT', _} ->
	    {error, not_supported};
	Else ->
	    Else
    end.

handle_sync_timeout(ReqId, From, State) ->
    ?vtrace("handle_sync_timeout -> entry with"
	    "~n   ReqId: ~p"
	    "~n   From:  ~p", [ReqId, From]),
    case ets:lookup(snmpm_request_table, ReqId) of
	[#request{mon = MonRef, from = From} = Req0] ->
	    ?vdebug("handle_sync_timeout -> "
		    "deliver reply (timeout) and demonitor: "
		    "~n   Monref: ~p"
		    "~n   From:   ~p", [MonRef, From]),
	    gen_server:reply(From, {error, {timeout, ReqId}}),
	    maybe_demonitor(MonRef),
	    
	    %% 
	    %% Instead of deleting the request record now,
	    %% we leave it to the gc. But for that to work 
	    %% we must update the expire value (which for
	    %% sync requests is infinity).
	    %% 

	    Req = Req0#request{ref    = undefined, 
			       mon    = undefined, 
			       from   = undefined, 
			       expire = snmp_misc:now(ms)},
	    ets:insert(snmpm_request_table, Req),
	    gct_activate(State#state.gct),
	    ok;
	_ ->
	    ok
    end.

    
handle_snmp_error(#pdu{request_id = ReqId} = Pdu, Reason, State) ->

    ?vtrace("handle_snmp_error -> entry with"
	    "~n   Reason: ~p"
	    "~n   Pdu:    ~p", [Reason, Pdu]),

    case ets:lookup(snmpm_request_table, ReqId) of

	%% Failed async request
	[#request{user_id   = UserId, 
		  from      = undefined, 
		  ref       = undefined, 
		  mon       = MonRef,
		  discovery = Disco}] ->

	    ?vdebug("handle_snmp_error -> "
		    "found corresponding request: "
		    "~n   failed async request"
		    "~n   UserId: ~p"
		    "~n   ModRef: ~p"
		    "~n   Disco:  ~p", [UserId, MonRef, Disco]),

	    maybe_demonitor(MonRef),
	    case snmpm_config:user_info(UserId) of
		{ok, UserMod, UserData} ->
		    handle_error(UserId, UserMod, Reason, ReqId, 
				 UserData, State),
		    maybe_delete(Disco, ReqId);
		_ ->
		    %% reply to outstanding request, for which there is no
		    %% longer any owner (the user has unregistered).
		    %% Therefore send it to the default user
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_error(DefUserId, DefMod, Reason, ReqId, 
					 DefData, State),
			    maybe_delete(Disco, ReqId);
			_ ->
			    error_msg("failed retrieving the default user "
				      "info handling error [~w]: "
				      "~n~w", [ReqId, Reason])
		    end
	    end;


	%% Failed sync request
	%%
	[#request{ref = Ref, mon = MonRef, from = From}] -> 

	    ?vdebug("handle_snmp_error -> "
		    "found corresponding request: "
		    "~n   failed sync request"
		    "~n   Ref:    ~p"
		    "~n   ModRef: ~p"
		    "~n   From:   ~p", [Ref, MonRef, From]),

	    Remaining = 
		case (catch cancel_timer(Ref)) of
		    Rem when is_integer(Rem) ->
			Rem;
		    _ ->
			0
		end,

	    ?vtrace("handle_snmp_error -> Remaining: ~p", [Remaining]),

	    maybe_demonitor(MonRef),
	    Reply = {error, {send_failed, ReqId, Reason}},
	    ?vtrace("handle_snmp_error -> deliver (error-) reply",[]), 
	    gen_server:reply(From, Reply),
	    ets:delete(snmpm_request_table, ReqId),
	    ok;


	%% A very old reply, see if this agent is handled by
	%% a user. In that case send it there, else to the 
	%% default user.
	_ ->

	    ?vdebug("handle_snmp_error -> no request?", []), 

	    case snmpm_config:user_info() of
		{ok, DefUserId, DefMod, DefData} ->
		    handle_error(DefUserId, DefMod, Reason, 
				 ReqId, DefData, State);
		_ ->
		    error_msg("failed retrieving the default "
			      "user info handling error [~w]: "
			      "~n~w",[ReqId, Reason])
	    end
    end;

handle_snmp_error(CrapError, Reason, _State) ->
    error_msg("received crap (snmp) error =>"
	      "~n   ~p"
              "~n   ~p", [CrapError, Reason]),
    ok.

handle_snmp_error(Domain, Addr, ReqId, Reason, State) ->

    ?vtrace("handle_snmp_error -> entry with"
	    "~n   Domain:  ~p"
	    "~n   Addr:    ~p"
	    "~n   ReqId:   ~p"
	    "~n   Reason:  ~p", [Domain, Addr, ReqId, Reason]),

    case snmpm_config:get_agent_user_id(Domain, Addr) of
	{ok, UserId} ->
	    case snmpm_config:user_info(UserId) of
		{ok, UserMod, UserData} ->
		    handle_error(UserId, UserMod, Reason, ReqId, 
				 UserData, State);
		_Error1 ->
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_error(DefUserId, DefMod, Reason, 
					 ReqId, DefData, State);
			_Error2 ->
			    error_msg("failed retrieving the default user "
				      "info handling snmp error "
				      "<~p,~p>: ~n~w~n~w",
				      [Domain, Addr, ReqId, Reason])
		    end
	    end;
	_Error3 ->
	    case snmpm_config:user_info() of
		{ok, DefUserId, DefMod, DefData} ->
		    handle_error(DefUserId, DefMod, Reason, 
				 ReqId, DefData, State);
		_Error4 ->
		    error_msg("failed retrieving the default user "
			      "info handling snmp error "
			      "<~p,~p>: ~n~w~n~w",
			      [Domain, Addr, ReqId, Reason])
	    end
    end.


handle_error(_UserId, Mod, Reason, ReqId, Data,
	     #state{cbproxy = CBP} = _State) ->
    ?vtrace("handle_error -> entry when"
	    "~n   Mod: ~p", [Mod]),
    handle_callback(CBP,
		    do_handle_error,
		    [Mod, ReqId, Reason, Data]),
    ok.

do_handle_error(Mod, ReqId, Reason, Data) ->
  try 
      begin
	  Mod:handle_error(ReqId, Reason, Data)
      end
  catch
      C:E:S ->
	  CallbackArgs = [ReqId, Reason, Data], 
	  handle_invalid_result(handle_error, CallbackArgs, C, E, S)
  end.


handle_snmp_pdu(#pdu{type = 'get-response', request_id = ReqId} = Pdu, 
		Domain, Addr, State) ->

    ?vtrace("handle_snmp_pdu(get-response) -> entry with"
	    "~n   Domain:  ~p"
	    "~n   Addr:    ~p"
	    "~n   Pdu:     ~p", [Domain, Addr, Pdu]),

    case ets:lookup(snmpm_request_table, ReqId) of

	%% Reply to a async request or 
	%% possibly a late reply to a sync request
	%% (ref is also undefined)
	[#request{user_id   = UserId, 
		  reg_type  = RegType, 
		  target    = Target, 
		  from      = undefined, 
		  ref       = undefined, 
		  mon       = MonRef,
		  discovery = Disco}] ->

	    ?vdebug("handle_snmp_pdu(get-response) -> "
		    "found corresponding request: "
		    "~n   reply to async request or late reply to sync request"
		    "~n   UserId: ~p"
		    "~n   ModRef: ~p"
		    "~n   Disco:  ~p", [UserId, MonRef, Disco]),

	    maybe_demonitor(MonRef),
	    #pdu{error_status = EStatus, 
		 error_index  = EIndex, 
		 varbinds     = Varbinds} = Pdu,
	    Varbinds2 = fix_vbs_BITS(Varbinds), 
	    SnmpResponse = {EStatus, EIndex, Varbinds2},
	    case snmpm_config:user_info(UserId) of
		{ok, UserMod, UserData} ->
		    handle_pdu(
		      UserId, UserMod,
		      RegType, Target, Domain, Addr,
		      ReqId, SnmpResponse, UserData, State),
		    maybe_delete(Disco, ReqId);
		_Error ->
		    %% reply to outstanding request, for which there is no
		    %% longer any owner (the user has unregistered).
		    %% Therefore send it to the default user
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_pdu(
			      DefUserId, DefMod,
			      RegType, Target, Domain, Addr,
			      ReqId, SnmpResponse, DefData, State),
			    maybe_delete(Disco, ReqId);
			Error ->
			    error_msg("failed retrieving the default user "
				      "info handling pdu from "
				      "~p <~p,~p>: ~n~w~n~w",
				      [Target, Domain, Addr, Error, Pdu])
		    end
	    end;


	%% Reply to a sync request
	%%
	[#request{ref = Ref, mon = MonRef, from = From}] -> 

	    ?vdebug("handle_snmp_pdu(get-response) -> "
		    "found corresponding request: "
		    "~n   reply to sync request"
		    "~n   Ref:    ~p"
		    "~n   ModRef: ~p"
		    "~n   From:   ~p", [Ref, MonRef, From]),

	    Remaining = 
		case (catch cancel_timer(Ref)) of
		    Rem when is_integer(Rem) ->
			Rem;
		    _ ->
			0
		end,

	    ?vtrace("handle_snmp_pdu(get-response) -> Remaining: ~p", 
		    [Remaining]),

	    maybe_demonitor(MonRef),
	    #pdu{error_status = EStatus, 
		 error_index  = EIndex, 
		 varbinds     = Varbinds} = Pdu,
	    Varbinds2 = fix_vbs_BITS(Varbinds), 
	    SnmpReply = {EStatus, EIndex, Varbinds2},
	    Reply = {ok, SnmpReply, Remaining},
	    ?vtrace("handle_snmp_pdu(get-response) -> deliver reply",[]), 
	    gen_server:reply(From, Reply),
	    ets:delete(snmpm_request_table, ReqId),
	    ok;
		

	%% A very old reply, see if this agent is handled by
	%% a user. In that case send it there, else to the 
	%% default user.
	_ ->

	    ?vdebug("handle_snmp_pdu(get-response) -> "
		    "no corresponding request: "
		    "~n   a very old reply", []),

	    #pdu{error_status = EStatus, 
		 error_index  = EIndex, 
		 varbinds     = Varbinds} = Pdu,
	    Varbinds2 = fix_vbs_BITS(Varbinds), 
	    SnmpInfo = {EStatus, EIndex, Varbinds2},
	    case snmpm_config:get_agent_user_id(Domain, Addr) of
		{ok, UserId} ->
		    %% A very late reply or a reply to a request
		    %% that has been cancelled.
		    %% 
		    ?vtrace("handle_snmp_pdu(get-response) -> "
			    "a very late reply:"
			    "~n   UserId: ~p",[UserId]), 
		    case snmpm_config:user_info(UserId) of
			{ok, UserMod, UserData} ->
			    Reason = {unexpected_pdu, SnmpInfo},
			    handle_error(UserId, UserMod, Reason, ReqId, 
					 UserData, State);
			_Error ->
			    %% Ouch, found an agent but not it's user!!
			    case snmpm_config:user_info() of
				{ok, DefUserId, DefMod, DefData} ->
				    Reason = {unexpected_pdu, SnmpInfo}, 
				    handle_error(DefUserId, DefMod, Reason, 
						 ReqId, DefData, State);
				Error ->
				    error_msg("failed retrieving the default "
					      "user info handling (old) "
					      "pdu from "
					      "<~p,~p>: ~n~w~n~w",
					      [Domain, Addr, Error, Pdu])
			    end
		    end;

		{error, _} -> 
		    %% No agent, so either this agent has been 
		    %% unregistered, or this is a very late reply 
		    %% to a request (possibly a discovery), which 
		    %% has since been cancelled (either because of
		    %% a timeout or that the user has unregistered 
		    %% itself (and with it all it's requests)). 
		    %% No way to know which.
		    %% 
		    ?vtrace("handle_snmp_pdu(get-response) -> "
			    "no agent info found", []),
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_agent(
			      DefUserId, DefMod,
			      Domain, Addr,
			      pdu, ignore,
			      SnmpInfo, DefData, State);
			Error ->
			    error_msg("failed retrieving the default user "
				      "info handling (old) pdu when no user "
				      "found from "
				      "<~p,~p>: ~n~w~n~w",
				      [Domain, Addr, Error, Pdu])
		    end
	    end
    end;


handle_snmp_pdu(CrapPdu, Domain, Addr, _State) ->
    error_msg("received crap (snmp) Pdu from ~w:~w =>"
	      "~p", [Domain, Addr, CrapPdu]),
    ok.


handle_pdu(
  _UserId, Mod, target_name = _RegType, TargetName, _Domain, _Addr,
  ReqId, SnmpResponse, Data, #state{cbproxy = CBP} = _State) ->
    ?vtrace("handle_pdu(target_name) -> entry when"
	    "~n   Mod: ~p", [Mod]),
    handle_callback(CBP,
		    do_handle_pdu,
		    [Mod, TargetName, ReqId, SnmpResponse, Data]),
    ok;
handle_pdu(
  _UserId, Mod, addr_port = _RegType, _TargetName, _Domain, Addr,
  ReqId, SnmpResponse, Data, #state{cbproxy = CBP} = _State) ->
    ?vtrace("handle_pdu(addr_port) -> entry when"
	    "~n   Mod: ~p", [Mod]),
    handle_callback(CBP, 
		    do_handle_pdu,
		    [Mod, Addr, ReqId, SnmpResponse, Data]),
    ok.

do_handle_pdu(Mod, {Ip, Port}, ReqId, SnmpResponse, Data) ->
    %% This is a deprecated version of the callback API, we skip handle 
    %% errors for this.
    (catch Mod:handle_pdu(Ip, Port, ReqId, SnmpResponse, Data));
do_handle_pdu(Mod, TargetName, ReqId, SnmpResponse, Data) ->
    try
        begin
            Mod:handle_pdu(TargetName, ReqId, SnmpResponse, Data)
        end
    catch
        C:E:S ->
            CallbackArgs = [TargetName, ReqId, SnmpResponse, Data], 
            handle_invalid_result(handle_pdu, CallbackArgs, C, E, S) 
    end.


handle_agent(UserId, Mod, Domain, Addr, Type, Ref, SnmpInfo, Data,
	     #state{cbproxy = CBP} = State) ->
    ?vtrace("handle_agent -> entry when"
	    "~n   UserId: ~p"
	    "~n   Type:   ~p"
	    "~n   Mod:    ~p", [UserId, Type, Mod]),
    handle_callback(CBP,
		    do_handle_agent,
		    [UserId, Mod, Domain, Addr,
		     Type, Ref, SnmpInfo, Data, State]),
    ok.

do_handle_agent(DefUserId, DefMod, 
		Domain, Addr,
		Type, Ref, 
		SnmpInfo, DefData, State) ->
    ?vdebug("do_handle_agent -> entry when"
	    "~n   DefUserId: ~p", [DefUserId]),
    {Domain_or_Ip, Addr_or_Port} =
	case Domain of
	    snmpUDPDomain ->
		Addr;
	    _ ->
		{Domain, Addr}
	end,
    try DefMod:handle_agent(
	  Domain_or_Ip, Addr_or_Port, Type, SnmpInfo, DefData)
    of
	{register, UserId2, TargetName, Config} ->  
	    ?vtrace("do_handle_agent -> register: "
		    "~n   UserId2:    ~p"
		    "~n   TargetName: ~p"
		    "~n   Config:     ~p", 
		    [UserId2, TargetName, Config]),
	    Config2 =
		ensure_present(
		  [{tdomain, Domain}, {taddress, Addr}], Config),
	    Config3 = [{reg_type, target_name} | Config2], 
	    case snmpm_config:register_agent(UserId2, 
					     TargetName, Config3) of
		ok ->
		    ok;
		{error, Reason} ->
		    error_msg("failed registering agent - "
			      "handling agent "
			      "~p <~p,~p>: ~n~w", 
			      [TargetName, Domain, Addr, Reason]),
		    ok
	    end;
	
	ignore ->
	    ?vdebug("do_handle_agent -> ignore", []),
	    ok;

	InvalidResult ->
	    CallbackArgs =
		[Domain_or_Ip, Addr_or_Port, Type, SnmpInfo, DefData],
	    handle_invalid_result(handle_agent, CallbackArgs, InvalidResult) 

    catch
	error:{undef, _} when Type =:= pdu ->
	    %% Maybe, still on the old API
	    ?vdebug("do_handle_agent -> maybe still on the old api", []),
	    {Ip, Port} = Addr,
	    case (catch DefMod:handle_agent(Ip, Port, SnmpInfo, DefData)) of
		{register, UserId2, Config} ->  
		    ?vtrace("do_handle_agent -> register: "
			    "~n   UserId2: ~p"
			    "~n   Config:  ~p", [UserId2, Config]),
		    TargetName = mk_target_name(Domain, Addr, Config),
		    Config2 =
			ensure_present(
			  [{tdomain, Domain}, {taddress, Addr}], Config),
		    Config3 = [{reg_type, addr_port} | Config2],
		    case snmpm_config:register_agent(
			   UserId2, TargetName, Config3) of
			ok ->
			    ok;
			{error, Reason} ->
			    error_msg("failed registering agent - "
				      "handling agent "
				      "~p <~p,~p>: ~n~w", 
				      [TargetName, Domain, Addr, Reason]),
			    ok
		    end;
		{register, UserId2, TargetName, Config} ->  
		    ?vtrace("do_handle_agent -> register: "
			    "~n   UserId2:    ~p"
			    "~n   TargetName: ~p"
			    "~n   Config:     ~p", 
			    [UserId2, TargetName, Config]),
		    Config2 =
			ensure_present(
			  [{tdomain, Domain}, {taddress, Addr}], Config),
		    Config3 = [{reg_type, target_name} | Config2], 
		    case snmpm_config:register_agent(
			   UserId2, TargetName, Config3) of
			ok ->
			    ok;
			{error, Reason} ->
			    error_msg("failed registering agent - "
				      "handling agent "
				      "~p <~p,~p>: ~n~w", 
				      [TargetName, Domain, Addr, Reason]),
			    ok
		    end;
		_Ignore ->
		    ?vdebug("do_handle_agent -> ignore", []),
		    ok
	    end;

	error:{undef, _} ->
	    %% If the user does not implement the new API (but the
	    %% old), then this clause catches all non-pdu handle_agent 
	    %% calls. These calls was previously never made, so we make 
	    %% a best-effert call (using reg-type target_name) to the 
	    %% various callback functions, and leave it to the user to
	    %% figure out

	    %% Backward compatibillity crap
	    RegType = target_name,
	    Target  = mk_target_name(Domain, Addr, default_agent_config()),
	    case Type of
		report ->
		    SnmpInform = SnmpInfo, 
		    handle_report(
		      DefUserId, DefMod, 
		      RegType, Target, Domain, Addr,
		      SnmpInform, DefData, State);

		inform ->
		    SnmpInform = SnmpInfo, 
		    handle_inform(
		      DefUserId, DefMod, Ref, 
		      RegType, Target, Domain, Addr,
		      SnmpInform, DefData, State);

		trap ->
		    SnmpTrapInfo = SnmpInfo, 
		    handle_trap(
		      DefUserId, DefMod, 
		      RegType, Target, Domain, Addr,
		      SnmpTrapInfo, DefData, State);

		_ ->
		    error_msg(
		      "failed delivering ~w info to default user - "
		      "regarding agent "
		      "<~p,~p>: ~n~w", [Type, Domain, Addr, SnmpInfo])
	    end;
	
	C:E:S ->
	    CallbackArgs =
		[Domain_or_Ip, Addr_or_Port, Type, SnmpInfo, DefData],
	    handle_invalid_result(handle_agent, CallbackArgs, C, E, S)
	    
    end.

ensure_present([], Config) ->
    Config;
ensure_present([{Key, _Val} = Elem|Ensure], Config) ->
    case lists:keymember(Key, 1, Config) of
	false ->
	    ensure_present(Ensure, [Elem|Config]);
	true ->
	    ensure_present(Ensure, Config)
    end.
    
    
%% Retrieve user info for this agent.
%% If this is an unknown agent, then use the default user
handle_snmp_trap(
  #trappdu{enterprise    = Enteprise, 
	   generic_trap  = Generic, 
	   specific_trap = Spec,
	   time_stamp    = Timestamp, 
	   varbinds      = Varbinds} = Trap, Domain, Addr, State) ->

    ?vtrace("handle_snmp_trap [trappdu] -> entry with~n"
	    "   Domain:  ~p~n"
	    "   Addr:    ~p~n"
	    "   Trap:    ~p", [Domain, Addr, Trap]),

    Varbinds2 = fix_vbs_BITS(Varbinds), 
    SnmpTrapInfo = {Enteprise, Generic, Spec, Timestamp, Varbinds2},
    do_handle_snmp_trap(SnmpTrapInfo, Domain, Addr, State);

handle_snmp_trap(#pdu{error_status = EStatus, 
		      error_index  = EIndex, 
		      varbinds     = Varbinds} = Trap, 
		 Domain, Addr, State) ->

    ?vtrace("handle_snmp_trap [pdu] -> entry with~n"
	    "   Domain:  ~p~n"
	    "   Addr:    ~p~n"
	    "   Trap:    ~p", [Domain, Addr, Trap]),

    Varbinds2 = fix_vbs_BITS(Varbinds), 
    SnmpTrapInfo = {EStatus, EIndex, Varbinds2},
    do_handle_snmp_trap(SnmpTrapInfo, Domain, Addr, State);

handle_snmp_trap(CrapTrap, Domain, Addr, _State) ->
    error_msg("received crap (snmp) trap from ~w:~w =>"
	      "~p", [Domain, Addr, CrapTrap]),
    ok.

do_handle_snmp_trap(SnmpTrapInfo, Domain, Addr, State) ->
    case snmpm_config:get_agent_user_info(Domain, Addr) of
	{ok, UserId, Target, RegType} ->
	    ?vdebug("do_handle_snmp_trap -> found user: ~p", [UserId]), 
	    case snmpm_config:user_info(UserId) of
		{ok, Mod, Data} ->
		    handle_trap(
		      UserId, Mod, 
		      RegType, Target, Domain, Addr,
		      SnmpTrapInfo, Data, State);
		
		Error1 ->
		    %% User no longer exists, unregister agent
		    ?vlog("[trap] failed retrieving user info for "
			  "user ~p: "
			  "~n      ~p", [UserId, Error1]),
		    case snmpm_config:unregister_agent(UserId, Target) of
			ok ->
			    %% Try use the default user
			    case snmpm_config:user_info() of
				{ok, DefUserId, DefMod, DefData} ->
				    handle_agent(
				      DefUserId, DefMod, 
				      Domain, Addr,
				      trap, ignore, 
				      SnmpTrapInfo, DefData, State);
				Error2 ->
				    error_msg(
				      "failed retrieving the default "
				      "user info handling report from "
				      "~p <~p,~p>: ~n~w~n~w",
				      [Target, Domain, Addr,
				       Error2, SnmpTrapInfo])
			    end;
			Error3 ->
			    %% Failed unregister agent, 
			    %% now its getting messy...
			    warning_msg(
			      "failed unregister agent ~p <~p,~p> "
			      "belonging to non-existing "
			      "user ~p, handling trap: "
			      "~n      Error:     ~p"
			      "~n      Trap info: ~p",
			      [Target, Domain, Addr, UserId,
			       Error3, SnmpTrapInfo])
		    end
	    end;
	
	Error4 ->
	    %% Unknown agent, pass it on to the default user
	    ?vlog("[trap] failed retrieving user id for agent <~p,~p>: "
		  "~n      Error:  ~p"
		  "~n   when"
		  "~n      Users:  ~p"
		  "~n      Agents: ~p",
                  [Domain, Addr, Error4,
                   snmpm_config:which_users(),
                   snmpm_config:which_agents()]),
	    case snmpm_config:user_info() of
		{ok, DefUserId, DefMod, DefData} ->
		    handle_agent(
		      DefUserId, DefMod, 
		      Domain, Addr,
		      trap, ignore, 
		      SnmpTrapInfo, DefData, State);
		Error5 ->
		    error_msg(
		      "failed retrieving "
		      "the default user info, handling trap from <~p,~p>:"
                      "~n      Error:     ~p"
                      "~n      Trap Info: ~p",
		      [Domain, Addr, Error5, SnmpTrapInfo])
	    end
    end,
    ok.


handle_trap(
  UserId, Mod, RegType, Target, Domain, Addr, SnmpTrapInfo, Data,
  #state{cbproxy = CBP} = State) ->
    ?vtrace("handle_trap -> entry with"
	    "~n   UserId: ~p"
	    "~n   Mod:    ~p", [UserId, Mod]),
    handle_callback(CBP,
		    do_handle_trap,
		    [UserId, Mod, 
		     RegType, Target, Domain, Addr,
		     SnmpTrapInfo, Data, State]),
    ok.
    

do_handle_trap(
  UserId, Mod, RegType, Target, Domain, Addr, SnmpTrapInfo, Data, _State) ->
    ?vdebug("do_handle_trap -> entry with"
	    "~n   UserId: ~p", [UserId]),
    {HandleTrap, CallbackArgs} = 
	case RegType of
	    target_name ->
		{fun() -> Mod:handle_trap(Target, SnmpTrapInfo, Data) end, 
		 [Target, SnmpTrapInfo, Data]};
	    addr_port ->
		{Ip, Port} = Addr,
		{fun() -> Mod:handle_trap(Ip, Port, SnmpTrapInfo, Data) end, 
		 [Ip, Port, SnmpTrapInfo, Data]}
	end,

    try HandleTrap() of
	{register, UserId2, Config} -> 
	    ?vtrace("do_handle_trap -> register: "
		    "~n   UserId2: ~p"
		    "~n   Config:  ~p", [UserId2, Config]),
	    Target2 = mk_target_name(Domain, Addr, Config),
	    Config2 =
		[{reg_type, target_name}, 
		 {tdomain, Domain}, {taddress, Addr} | Config],
	    case snmpm_config:register_agent(UserId2, Target2, Config2) of
		ok ->
		    ok;
		{error, Reason} ->
		    error_msg("failed registering agent "
			      "handling trap "
			      "<~p,~p>: ~n~w", 
			      [Domain, Addr, Reason]),
		    ok
	    end;
	{register, UserId2, Target2, Config} -> 
	    ?vtrace("do_handle_trap -> register: "
		    "~n   UserId2: ~p"
		    "~n   Target2: ~p"
		    "~n   Config:  ~p", [UserId2, Target2, Config]),
	    %% The only user which would do this is the
	    %% default user
	    Config2 = [{reg_type, target_name} | Config], 
	    case snmpm_config:register_agent(UserId2, Target2, Config2) of
		ok ->
		    reply;
		{error, Reason} ->
		    error_msg("failed registering agent "
			      "handling trap "
			      "~p <~p,~p>: ~n~w", 
			      [Target2, Domain, Addr, Reason]),
		    reply
	    end;
	unregister ->
	    ?vtrace("do_handle_trap -> unregister", []),
	    case snmpm_config:unregister_agent(UserId, Domain, Addr) of
		ok ->
		    ok;
		{error, Reason} ->
		    error_msg("failed unregistering agent "
			      "handling trap "
			      "<~p,~p>: ~n~w", 
			      [Domain, Addr, Reason]),
		    ok
	    end;	    
	ignore ->
	    ?vtrace("do_handle_trap -> ignore", []),
	    ok;

	InvalidResult ->
	    handle_invalid_result(handle_trap, CallbackArgs, InvalidResult)

    catch
	C:E:S ->
	    handle_invalid_result(handle_trap, CallbackArgs, C, E, S)

    end.


handle_snmp_inform(
  Ref, 
  #pdu{error_status = EStatus, 
       error_index  = EIndex, 
       varbinds     = Varbinds} = Pdu, Domain, Addr, State) ->
 
    ?vtrace("handle_snmp_inform -> entry with~n"
	    "   Domain:  ~p~n"
	    "   Addr:    ~p~n"
	    "   Pdu:     ~p", [Domain, Addr, Pdu]),

    Varbinds2 = fix_vbs_BITS(Varbinds), 
    SnmpInform = {EStatus, EIndex, Varbinds2},
    case snmpm_config:get_agent_user_info(Domain, Addr) of
	{ok, UserId, Target, RegType} ->
	    case snmpm_config:user_info(UserId) of
		{ok, Mod, Data} ->
		    ?vdebug("[inform] callback handle_inform with: "
			    "~n   UserId: ~p"
			    "~n   Mod:    ~p", [UserId, Mod]),
		    handle_inform(
		      UserId, Mod, Ref, 
		      RegType, Target, Domain, Addr,
		      SnmpInform, Data, State);
		Error1 ->
		    %% User no longer exists, unregister agent
		    case snmpm_config:unregister_agent(UserId, Target) of
			ok ->
			    %% Try use the default user
			    ?vlog("[inform] failed retrieving user "
				  "info for user ~p:"
				  "~n   ~p", [UserId, Error1]),
			    case snmpm_config:user_info() of
				{ok, DefUserId, DefMod, DefData} ->
				    handle_agent(
				      DefUserId, DefMod, 
				      Domain, Addr,
				      inform, Ref, 
				      SnmpInform, DefData, State);
				Error2 ->
				    error_msg("failed retrieving the default "
					      "user info handling inform from "
					      "~p <~p,~p>: ~n~w~n~w",
					      [Target, Domain, Addr,
					       Error2, Pdu])
			    end;
			Error3 ->
			    %% Failed unregister agent, 
			    %% now its getting messy...
			    warning_msg("failed unregister agent ~p <~p,~p> "
					"~n   belonging to non-existing "
					"user ~p, handling inform: "
					"~n   Error: ~w"
					"~n   Pdu:   ~w",
					[Target, Domain, Addr, UserId,
					 Error3, Pdu])
		    end
	    end;

	Error4 ->
	    %% Unknown agent, pass it on to the default user
	    ?vlog("[inform] failed retrieving user id for agent <~p,~p>: "
		  "~n   ~p", [Domain, Addr, Error4]),
	    case snmpm_config:user_info() of
		{ok, DefUserId, DefMod, DefData} ->
		    handle_agent(
		      DefUserId, DefMod, 
		      Domain, Addr,
		      inform, Ref, 
		      SnmpInform, DefData, State);
		Error5 ->
		    error_msg("failed retrieving "
			      "the default user info handling inform from "
			      "<~p,~p>: ~n~w~n~w",
			      [Domain, Addr, Error5, Pdu])
	    end
    end,
    ok;

handle_snmp_inform(_Ref, CrapInform, Domain, Addr, _State) ->
    error_msg("received crap (snmp) inform from ~w:~w =>"
	      "~p", [Domain, Addr, CrapInform]),
    ok.

handle_inform(
  UserId, Mod, Ref, 
  RegType, Target, Domain, Addr, SnmpInform, Data,
  #state{cbproxy = CBP} = State) ->
    ?vtrace("handle_inform -> entry with"
	    "~n   UserId: ~p"
	    "~n   Mod:    ~p", [UserId, Mod]),
    handle_callback(CBP,
		    do_handle_inform,
		    [UserId, Mod, Ref, 
		     RegType, Target, Domain, Addr, SnmpInform,
		     Data, State]),
    ok.

do_handle_inform(
  UserId, Mod, Ref, 
  RegType, Target, Domain, Addr, SnmpInform, Data, State) ->
    ?vdebug("do_handle_inform -> entry with"
	    "~n   UserId: ~p", [UserId]),
    {HandleInform, CallbackArgs} = 
	case RegType of
	    target_name ->
		{fun() -> Mod:handle_inform(Target, SnmpInform, Data) end, 
		 [Target, SnmpInform, Data]};
	    addr_port ->
		{Ip, Port} = Addr,
		{fun() -> Mod:handle_inform(Ip, Port, SnmpInform, Data) end, 
		 [Ip, Port, SnmpInform, Data]}
	end,

     Rep = 
	try HandleInform() of
	    {register, UserId2, Config} -> 
		?vtrace("do_handle_inform -> register: "
			"~n   UserId2: ~p"
			"~n   Config:  ~p", [UserId2, Config]),
		%% The only user which would do this is the
		%% default user
		Target2 = mk_target_name(Domain, Addr, Config),
		Config2 =
		    [{reg_type, target_name} |
		     ensure_present(
		       [{tdomain, Domain}, {taddress, Addr}], Config)],
		case snmpm_config:register_agent(UserId2, Target2, Config2) of
		    ok ->
			reply;
		    {error, Reason} ->
			error_msg("failed registering agent "
				  "handling inform "
				  "~p <~p,~p>: ~n~w", 
				  [Target2, Domain, Addr, Reason]),
			reply
		end;

	    {register, UserId2, Target2, Config} -> 
		?vtrace("do_handle_inform -> register: "
			"~n   UserId2: ~p"
			"~n   Target2: ~p"
			"~n   Config:  ~p", [UserId2, Target2, Config]),
		%% The only user which would do this is the
		%% default user
		Config2 = [{reg_type, target_name} | Config], 
		case snmpm_config:register_agent(UserId2, Target2, Config2) of
		    ok ->
			reply;
		    {error, Reason} ->
			error_msg("failed registering agent "
				  "handling inform "
				  "~p <~p,~p>: ~n~w", 
				  [Target2, Domain, Addr, Reason]),
			reply
		end;

	    unregister ->
		?vtrace("do_handle_inform -> unregister", []),
		case snmpm_config:unregister_agent(
		       UserId, Domain, Addr) of
		    ok ->
			reply;
		    {error, Reason} ->
			error_msg("failed unregistering agent "
				  "handling inform "
				  "<~p,~p>: ~n~w", 
				  [Domain, Addr, Reason]),
			reply
		end;	    

	    no_reply ->
		?vtrace("do_handle_inform -> no_reply", []),
		no_reply;

	    ignore ->
		?vtrace("do_handle_inform -> ignore", []),
		reply;

	    InvalidResult ->
		handle_invalid_result(
		  handle_inform, CallbackArgs, InvalidResult), 
		reply

	catch
	    C:E:S ->
		handle_invalid_result(handle_inform, CallbackArgs, C, E, S), 
		reply

	end,
    handle_inform_response(Rep, Ref, Domain, Addr, State),
    ok.


handle_inform_response(_, ignore, _Domain, _Addr, _State) ->
    ignore;
handle_inform_response(no_reply, _Ref, _Domain, _Addr, _State) ->
    no_reply;
handle_inform_response(
  _, Ref, Domain, Addr,
  #state{net_if = Pid, net_if_mod = Mod}) ->
    ?vdebug("handle_inform -> response", []),
    (catch Mod:inform_response(Pid, Ref, Domain, Addr)).
    
handle_snmp_report(
  #pdu{error_status = EStatus,
       error_index  = EIndex,
       varbinds     = Varbinds} = Pdu,
  Domain, Addr, State) ->

    ?vtrace("handle_snmp_report -> entry with~n"
	    "   Domain:  ~p~n"
	    "   Addr:    ~p~n"
	    "   Pdu:     ~p", [Domain, Addr, Pdu]),

    Varbinds2  = fix_vbs_BITS(Varbinds), 
    SnmpReport = {EStatus, EIndex, Varbinds2},
    case snmpm_config:get_agent_user_info(Domain, Addr) of
 	{ok, UserId, Target, RegType} ->
 	    case snmpm_config:user_info(UserId) of
 		{ok, Mod, Data} ->
 		    ?vdebug("[report] callback handle_report with: "
 			    "~n   ~p"
 			    "~n   ~p"
 			    "~n   ~p"
 			    "~n   ~p", [UserId, Mod, Target, SnmpReport]),
 		    handle_report(UserId, Mod, 
				  RegType, Target, Domain, Addr,
				  SnmpReport, Data, State);
 		Error1 ->
		    %% User no longer exists, unregister agent
		    ?vlog("[report] failed retrieving user info "
			  "for user ~p:"
			  " ~n   ~p", [UserId, Error1]),
		    case snmpm_config:unregister_agent(UserId, Target) of
			ok ->
			    %% Try use the default user
			    case snmpm_config:user_info() of
				{ok, DefUserId, DefMod, DefData} ->
				    handle_agent(DefUserId, DefMod, 
						 Domain, Addr,
						 report, ignore, 
						 SnmpReport, DefData, State);
				
				Error2 ->
				    error_msg("failed retrieving the default "
					      "user info handling report from "
					      "~p <~p,~p>: ~n~w~n~w",
					      [Target, Domain, Addr,
					       Error2, Pdu])
			    end;
			Error3 ->
			    %% Failed unregister agent, 
			    %% now its getting messy...
			    warning_msg("failed unregister agent ~p <~p,~p> "
					"belonging to non-existing "
					"user ~p, handling report: "
					"~n   Error:  ~w"
					"~n   Report: ~w",
					[Target, Domain, Addr, UserId,
					 Error3, Pdu])
		    end
	    end;
		
	Error4 ->
	    %% Unknown agent, pass it on to the default user
	    ?vlog("[report] failed retrieving user id for agent <~p,~p>: "
		  "~n   ~p", [Domain, Addr, Error4]),
	    case snmpm_config:user_info() of
		{ok, DefUserId, DefMod, DefData} ->
		    handle_agent(DefUserId, DefMod, 
				 Domain, Addr,
				 report, ignore, 
				 SnmpReport, DefData, State);
		Error5 ->
		    error_msg("failed retrieving "
			      "the default user info handling report from "
			      "<~p,~p>: ~n~w~n~w",
			      [Domain, Addr, Error5, Pdu])
	    end
    end,
    ok;

handle_snmp_report(CrapReport, Domain, Addr, _State) ->
    error_msg("received crap (snmp) report from ~w:~w =>"
	      "~p", [Domain, Addr, CrapReport]),
    ok.

%% This could be from a failed get-request, so we might have a user
%% waiting for a reply here. If there is, we handle this as an failed
%% get-response (except for the data which is different). Otherwise,
%% we handle it as an error (reported via the handle_error callback
%% function).
handle_snmp_report(
  ReqId,
  #pdu{error_status = EStatus,
       error_index  = EIndex,
       varbinds     = Varbinds} = Pdu,
  {ReportReason, Info} = Rep,
  Domain, Addr, State) when is_integer(ReqId) ->

    ?vtrace("handle_snmp_report -> entry with~n"
	    "   Domain:  ~p~n"
	    "   Addr:    ~p~n"
	    "   ReqId:   ~p~n"
	    "   Rep:     ~p~n"
	    "   Pdu:     ~p", [Domain, Addr, ReqId, Rep, Pdu]),

    Varbinds2 = fix_vbs_BITS(Varbinds), 
    SnmpReport = {EStatus, EIndex, Varbinds2},
    Reason     = {ReportReason, Info, SnmpReport},
    
    %% Check if there is someone waiting for this request

    case ets:lookup(snmpm_request_table, ReqId) of

	[#request{from     = From, 
		  ref      = Ref, 
		  mon      = MonRef}] when (From =/= undefined) andalso 
					   (Ref  =/= undefined) ->

	    ?vdebug("handle_snmp_report -> "
		    "found corresponding request: "
		    "~n   reply to sync request"
		    "~n   Ref:    ~p"
		    "~n   ModRef: ~p"
		    "~n   From:   ~p", [Ref, MonRef, From]),

	    Remaining = 
		case (catch cancel_timer(Ref)) of
		    Rem when is_integer(Rem) ->
			Rem;
		    _ ->
			0
		end,

	    ?vtrace("handle_snmp_pdu(get-response) -> Remaining: ~p", 
		    [Remaining]),

	    maybe_demonitor(MonRef),

	    Reply = {error, Reason},
	    ?vtrace("handle_snmp_report -> deliver reply",[]), 
	    gen_server:reply(From, Reply),
	    ets:delete(snmpm_request_table, ReqId),
	    ok;
	
	_ ->
	    %% Either not a sync request or no such request. Either
	    %% way, this is error info, so handle it as such.

	    case snmpm_config:get_agent_user_id(Domain, Addr) of
		{ok, UserId} ->
		    case snmpm_config:user_info(UserId) of
			{ok, Mod, Data} ->
			    ?vdebug("[report] callback handle_error with: "
				    "~n   ~p"
				    "~n   ~p"
				    "~n   ~p", [UserId, Mod, Reason]),
			    handle_error(UserId, Mod, Reason, ReqId, 
					 Data, State);
			Error ->
			    %% Oh crap, use the default user
			    ?vlog("[report] failed retrieving user info for "
				  "user ~p:"
				  " ~n   ~p", [UserId, Error]),
			    case snmpm_config:user_info() of
				{ok, DefUserId, DefMod, DefData} ->
				    handle_error(DefUserId, DefMod, Reason, 
						 ReqId, DefData, State);
				Error ->
				    error_msg("failed retrieving the "
					      "default user "
					      "info handling report from "
					      "<~p,~p>: ~n~w~n~w~n~w",
					      [Domain, Addr, Error,
					       ReqId, Reason])
			    end
		    end;
		Error ->
		    %% Unknown agent, pass it on to the default user
		    ?vlog("[report] failed retrieving user id for "
			  "agent <~p,~p>: "
			  "~n   ~p", [Domain, Addr, Error]),
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_error(DefUserId, DefMod, Reason, ReqId, 
					 DefData, State);
			Error ->
			    error_msg("failed retrieving "
				      "the default user info handling "
				      "report from "
				      "<~p,~p>: ~n~w~n~w~n~w",
				      [Domain, Addr, Error, ReqId, Reason])
		    end
	    end
    end,
    ok;

handle_snmp_report(CrapReqId, CrapReport, CrapInfo, Domain, Addr, _State) ->
    error_msg(
      "received crap (snmp) report from ~w:~w =>"
      "~n~p~n~p~n~p",
      [Domain, Addr, CrapReqId, CrapReport, CrapInfo]),
    ok.
   

handle_report(UserId, Mod, RegType, Target, Domain, Addr,
	      SnmpReport, Data,
	      #state{cbproxy = CBP} = State) ->
    ?vtrace("handle_report -> entry with"
	    "~n   UserId: ~p"
	    "~n   Mod:    ~p", [UserId, Mod]),
    handle_callback(CBP,
		    do_handle_report,
		    [UserId, Mod, RegType, Target, Domain, Addr,
		     SnmpReport, Data, State]),
    ok.

do_handle_report(
  UserId, Mod, RegType, Target, Domain, Addr,
  SnmpReport, Data, _State) ->
    ?vdebug("do_handle_report -> entry with"
	    "~n   UserId: ~p", [UserId]),
    {HandleReport, CallbackArgs} = 
	case RegType of
	    target_name ->
		{fun() -> Mod:handle_report(Target, SnmpReport, Data) end, 
		 [Target, SnmpReport, Data]};
	    addr_port ->
		{Ip, Port} = Addr,
		{fun() -> Mod:handle_report(Ip, Port, SnmpReport, Data) end,
		 [Ip, Port, SnmpReport, Data]}
	end,

    try HandleReport() of
	{register, UserId2, Config} -> 
	    ?vtrace("do_handle_report -> register: "
		    "~n   UserId2: ~p"
		    "~n   Config:  ~p", [UserId2, Config]),
	    %% The only user which would do this is the
	    %% default user
	    Target2 = mk_target_name(Domain, Addr, Config),
	    Config2 =
		[{reg_type, target_name},
		 {tdomain, Domain}, {taddress, Addr} | Config],
	    case snmpm_config:register_agent(UserId2, Target2, Config2) of
		ok ->
		    ok;
		{error, Reason} ->
		    error_msg("failed registering agent "
			      "handling report "
			      "<~p,~p>: ~n~w",
			      [Domain, Addr, Reason]),
		    ok
	    end;

	{register, UserId2, Target2, Config} -> 
	    ?vtrace("do_handle_report -> register: "
		    "~n   UserId2: ~p"
		    "~n   Target2: ~p"
		    "~n   Config:  ~p", [UserId2, Target2, Config]),
	    %% The only user which would do this is the
	    %% default user
	    Config2 = [{reg_type, target_name} | Config], 
	    case snmpm_config:register_agent(UserId2, Target2, Config2) of
		ok ->
		    reply;
		{error, Reason} ->
		    error_msg("failed registering agent "
			      "handling report "
			      "~p <~p,~p>: ~n~w", 
			      [Target2, Domain, Addr, Reason]),
		    reply
	    end;

	unregister ->
	    ?vtrace("do_handle_trap -> unregister", []),
	    case snmpm_config:unregister_agent(UserId, Domain, Addr) of
		ok ->
		    ok;
		{error, Reason} ->
		    error_msg("failed unregistering agent "
			      "handling report "
			      "<~p,~p>: ~n~w", 
			      [Domain, Addr, Reason]),
		    ok
	    end;	    

	ignore ->
	    ?vtrace("do_handle_report -> ignore", []),
	    ok;

	InvalidResult ->
	    handle_invalid_result(handle_report, CallbackArgs, InvalidResult), 
	    reply

    catch
	C:E:S ->
	    handle_invalid_result(handle_report, CallbackArgs, C, E, S),
	    reply

    end.



%%----------------------------------------------------------------------
%% Handle Callback
%%----------------------------------------------------------------------

handle_callback(temporary, Func, Args) ->
    V = get(verbosity),
    erlang:spawn(
      fun() -> 
	      put(sname, msew), 
	      put(verbosity, V), 
	      apply(?MODULE, Func, Args) 
      end);
%% handle_callback(transient, MFA) ->
%%     Pid = which_transient_callback_proxy(ProxyID),
%%     Pid ! {?MODULE, self(), {callback, MFA}};
handle_callback(permanent, Func, Args) ->
    case whereis(?CBPROXY) of
	Pid when is_pid(Pid) ->
	    Pid ! {?MODULE, self(), {callback, {?MODULE, Func, Args}}};
	_ ->
	    %% We should really either die or restart (the cbproxy).
            %% It could also be a race, in which case spawning a temporary
            %% process is better than nothing...
            %% ...but we should inform someone...
            warning_msg("Permanent Callback Proxy could not be found - "
                        "using temporary"),
	    handle_callback(temporary, Func, Args)
    end.



%%----------------------------------------------------------------------

cbproxy_start() ->
    cbproxy_start(infinity).

cbproxy_start(IdleTimeout) ->
    cbproxy_start(self(), IdleTimeout).

cbproxy_start(Parent, IdleTimeout) ->
    Pid = spawn_link(fun() -> cbproxy_init(Parent, IdleTimeout) end),
    receive
	{?MODULE, Pid, ready} ->
	    Pid
    end.

cbproxy_stop(permanent) ->
    case whereis(?CBPROXY) of
	Pid when is_pid(Pid) ->
	    Pid ! {?MODULE, self(), stop},
	    ok;
	_ ->
	    ok
    end;
cbproxy_stop(_) ->
    ok.

cbproxy_info() ->
    case whereis(?CBPROXY) of
	Pid when is_pid(Pid) ->
	    Pid ! {?MODULE, self(), info},
	    receive
		{?MODULE, Pid, {info, Info}} ->
		    Info
	    after 5000 ->
		    %% If a callback function takes a long time,
		    %% the cb proxy may be busy. But we only wait for
		    %% a "short" time. No point in making things
		    %% complicated when all we do is collecting "info".
		    [{timeout, process_info(Pid)}]
	    end;
	_ ->
	    []
    end.

%% The timeout is future proofing (intended to be used for
%% "when" we introduce a transient callback proxy).
cbproxy_init(Parent, _IdleTimeout) ->
    ?snmpm_info("CallBack Proxy: starting", []),
    erlang:register(?CBPROXY, self()),
    State = #{parent   => Parent,
	      cnt      => 0,
	      max_work => 0,
	      awork    => 0},
    Parent ! {?MODULE, self(), ready},
    cbproxy_loop(State).

%% * Every time a "counter" wraps, we send a message regarding this
%%   (to the server) and resets the counter.
%% * Every time the AWT (accumulated work time) exceeds or is equal to 1h,
%%   we send a message regarding this (to the server) and resets the AWT.
%% 
cbproxy_loop(#{parent := Pid} = State) ->
    receive
        {?MODULE, Pid, stop} ->
	    cbp_handle_stop(State),
            exit(normal);


        {?MODULE, Pid, info} ->
	    Info = cbp_handle_info(State),
	    Pid ! {?MODULE, self(), {info, Info}},
            ?MODULE:cbproxy_loop(State);


        %% And this is what we are here for:
        {?MODULE, Pid, {callback, {Mod, Func, Args}}} ->
	    F = fun() -> apply(Mod, Func, Args) end,
	    ?MODULE:cbproxy_loop(cbp_handle_callback(State, F));

        %% And this is what we are here for:
        {?MODULE, Pid, {callback, F}} when is_function(F, 0) ->
	    ?MODULE:cbproxy_loop(cbp_handle_callback(State, F))


    after 5000 ->
            %% This is for code upgrade
            ?MODULE:cbproxy_loop(State)
    end.

cbp_handle_stop(#{activity := AT,
		  cnt      := CNT,
		  max_work := MWT,
		  awork    := AWT}) ->
    ?snmpm_info("CallBack Proxy: stop =>"
		"~n   Number of Calls:       ~w"
		"~n   Last Activity:         ~s"
		"~n   Max Work Time:         ~s"
		"~n   Accumulated Work Time: ~s",
		[CNT, cbp_fts(cbp_ts(AT)), cbp_ft(MWT), cbp_ft(AWT)]);
cbp_handle_stop(_) ->
    ?snmpm_info("CallBack Proxy: stop =>"
		"~n   Number of Calls: 0", []).

cbp_handle_info(#{activity := AT,
		  cnt      := CNT,
		  max_work := MWT,
		  awork    := AWT}) ->
    ATS = cbp_ts(AT),
    [{cnt,      CNT},
     {activity, ATS},
     {max_work, MWT},
     {work,     AWT}];
cbp_handle_info(_) ->
    [{cnt, 0}, {awork, 0}, {max_work, 0}].
    

cbp_handle_callback(#{cnt      := CNT1,
		      max_work := MWT,
		      awork    := AWT1} = State, F) ->
    T1         = cbp_t(),
    (catch F()),
    T2         = cbp_t(),
    CallbackWT = T2 - T1,
    NewMWT     = cbp_max(CallbackWT, MWT),
    AWT2       = cbp_inc(awt, AWT1, CallbackWT, ?CBP_AWT_MAX),
    CNT2       = cbp_inc(cnt, CNT1, 1,          ?CBP_CNT_MAX),
    State#{cnt      => CNT2,
	   max_work => NewMWT,
	   awork    => AWT2,
	   activity => T2}.

cbp_t() ->
    erlang:system_time(microsecond).

cbp_max(A, B) when (A > B) ->
    A;
cbp_max(_, B) ->
    B.

cbp_inc(awt, Val, Inc, Max) when (Val + Inc) > Max ->
    ?snmpm_info("CallBack Proxy: Accumulated Work Time wrapped 1 hour", []),
    (Val+Inc) - Max;
cbp_inc(cnt, Val, Inc, Max) when (Val + Inc) > Max ->
    ?snmpm_info("CallBack Proxy: work counter wrapped", []),
    (Val+Inc) - Max;
cbp_inc(_, Val, Inc, _) ->
    Val + Inc.

cbp_ts(T) ->
    MegaSecs  = T div 1000000000000,
    Secs      = T div 1000000 - MegaSecs*1000000,
    MicroSecs = T rem 1000000,
    {MegaSecs, Secs, MicroSecs}.

cbp_fts({_, _, N3} = TS) ->
    {Date, Time}   = calendar:now_to_datetime(TS),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w ~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),
    lists:flatten(FormatDate).

cbp_ft(T) when T < 1000 ->
    cbp_f("~w usec", [T]);
cbp_ft(T) when T < 1000000 ->
    cbp_f("~w msec", [T div 1000]);
cbp_ft(T) when T < 60000000 ->
    cbp_f("~w sec", [T div (1000*1000)]);
cbp_ft(T) ->
    cbp_f("~w min", [T div (60*1000*1000)]).


cbp_f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


%%----------------------------------------------------------------------

handle_invalid_result(Func, Args, C, E, S) ->
    error_msg("Callback function failed: "
	      "~n   Function:    ~p"
	      "~n   Args:        ~p"
	      "~n   Error Class: ~p"
	      "~n   Error:       ~p"
	      "~n   Stacktrace:  ~p", 
	      [Func, Args, C, E, S]). 

handle_invalid_result(Func, Args, InvalidResult) ->
    error_msg("Callback function returned invalid result: "
	      "~n   Function:       ~p"
	      "~n   Args:           ~p"
	      "~n   Invalid result: ~p", 
	      [Func, Args, InvalidResult]).


handle_netif_down(#state{note_store = NoteStore, 
                         nis_pid    = NIS} = State,
                  Reason) ->
    %% Srart a new NetIF
    {NetIF, _, Ref} = do_init_net_if(NoteStore),
    %% Inform the (active) supervisor
    nis_netif(NIS, NetIF),
    netif_down_inform_users(Reason),
    State#state{net_if = NetIF, net_if_ref = Ref}.

handle_down(MonRef) ->
    (catch do_handle_down(MonRef)).

do_handle_down(MonRef) ->
    %% Clear out all requests from this client
    handle_down_requests_cleanup(MonRef),

    %% 
    %% Check also if this was a monitored user, and if so
    %% unregister all agents registered by this user, and
    %% finally unregister the user itself
    %% 
    handle_down_user_cleanup(MonRef),
    
    ok.


handle_down_requests_cleanup(MonRef) ->
    Pat = #request{id = '$1', ref = '$2', mon = MonRef, _ = '_'},
    Match = ets:match(snmpm_request_table, Pat),
    Fun = fun([Id, Ref]) -> 
		  ?vtrace("delete request: ~p", [Id]),
		  ets:delete(snmpm_request_table, Id),
		  cancel_timer(Ref),
		  ok
	  end,
    lists:foreach(Fun, Match).

handle_down_user_cleanup(MonRef) ->
    Pat = #monitor{id = '$1', mon = MonRef, _ = '_'},
    Match = ets:match(snmpm_monitor_table, Pat),
    Fun = fun([Id]) -> 
		  Agents = snmpm_config:which_agents(Id),
		  lists:foreach(
		    fun({Addr,Port}) -> 
			    %% ** Previous format **
			    %% Just in case this happens during code upgrade
			    ?vtrace("unregister agent of monitored user "
				    "~w: <~w,~w>", [Id,Addr,Port]),
			    snmpm_config:unregister_agent(Id, Addr, Port);
		       (TargetName) ->
			    ?vtrace("unregister agent of monitored user "
				    "~w: ~p", [Id,TargetName]),
			    snmpm_config:unregister_agent(Id, TargetName)
		    end,
		    Agents),
		  ?vtrace("unregister monitored user: ~w", [Id]),
		  ets:delete(snmpm_monitor_table, Id),
		  snmpm_config:unregister_user(Id),
		  ok
	     end,
    lists:foreach(Fun, Match).
    
cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) ->
    (catch erlang:cancel_timer(Ref)).

handle_gc(GCT) ->
    ets:safe_fixtable(snmpm_request_table, true),
    case do_gc(ets:first(snmpm_request_table), snmp_misc:now(ms)) of
	0 ->
	    gct_deactivate(GCT);
	_ ->
	    ok
    end,
    ets:safe_fixtable(snmpm_request_table, false).



%% We are deleting at the same time as we are traversing the table!!!
do_gc('$end_of_table', _) ->
    ets:info(snmpm_request_table, size);
do_gc(Key, Now) ->
    Next = ets:next(snmpm_request_table, Key),
    case ets:lookup(snmpm_request_table, Key) of
	[#request{expire = BestBefore}] when (BestBefore < Now) ->
	    ets:delete(snmpm_request_table, Key);
	_ ->
	    ok
    end,
    do_gc(Next, Now).
	    


%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------

send_get_request(Oids, Vsn, MsgData, Domain, Addr, ExtraInfo,
		 #state{net_if     = NetIf, 
			net_if_mod = Mod,
			mini_mib   = MiniMIB}) ->
    Pdu = make_pdu(get, Oids, MiniMIB),
    ?vtrace("send_get_request -> send get-request:~n"
	    "   Mod:     ~p~n"
	    "   NetIf:   ~p~n"
	    "   Pdu:     ~p~n"
	    "   Vsn:     ~p~n"
	    "   MsgData: ~p~n"
	    "   Domain:  ~p~n"
	    "   Addr:    ~p",
	    [Mod, NetIf, Pdu, Vsn, MsgData, Domain, Addr]),
    Res = (catch Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, 
			      Domain, Addr, ExtraInfo)),
    ?vtrace("send_get_request -> send result:"
	    "~n   ~p", [Res]),
    Pdu#pdu.request_id.

send_get_next_request(Oids, Vsn, MsgData, Domain, Addr, ExtraInfo,
		      #state{mini_mib   = MiniMIB, 
			     net_if     = NetIf, 
			     net_if_mod = Mod}) ->
    Pdu = make_pdu(get_next, Oids, MiniMIB),
    Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Domain, Addr, ExtraInfo),
    Pdu#pdu.request_id.

send_get_bulk_request(Oids, Vsn, MsgData, Domain, Addr,
		      NonRep, MaxRep, ExtraInfo, 
		      #state{mini_mib   = MiniMIB, 
			     net_if     = NetIf, 
			     net_if_mod = Mod}) ->
    Pdu = make_pdu(bulk, {NonRep, MaxRep, Oids}, MiniMIB),
    Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Domain, Addr, ExtraInfo),
    Pdu#pdu.request_id.

send_set_request(VarsAndVals, Vsn, MsgData, Domain, Addr, ExtraInfo,
		 #state{mini_mib   = MiniMIB,
			net_if     = NetIf, 
			net_if_mod = Mod}) ->
    Pdu = make_pdu(set, VarsAndVals, MiniMIB),
    Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Domain, Addr, ExtraInfo),
    Pdu#pdu.request_id.


%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------

make_pdu(set, VarsAndVals, MiniMIB) ->
    VBs = [var_and_value_to_varbind(VAV, MiniMIB) || VAV <- VarsAndVals],
    make_pdu_impl(set, VBs);

make_pdu(bulk, {NonRepeaters, MaxRepetitions, Oids}, MiniMIB) ->
    Foids = [flatten_oid(Oid, MiniMIB) || Oid <- Oids],
    #pdu{type         = 'get-bulk-request', 
	 request_id   = request_id(),
	 error_status = NonRepeaters, 
	 error_index  = MaxRepetitions,
	 varbinds     = [make_vb(Foid) || Foid <- Foids]};

make_pdu(Op, Oids, MiniMIB) ->
    Foids = [flatten_oid(Oid, MiniMIB) || Oid <- Oids],
    make_pdu_impl(Op, Foids).


make_pdu_impl(get, Oids) ->
    #pdu{type         = 'get-request',
	 request_id   = request_id(),
	 error_status = noError, 
	 error_index  = 0,
	 varbinds     = [make_vb(Oid) || Oid <- Oids]};

make_pdu_impl(get_next, Oids) ->
    #pdu{type         = 'get-next-request', 
	 request_id   = request_id(), 
	 error_status = noError, 
	 error_index  = 0,
	 varbinds     = [make_vb(Oid) || Oid <- Oids]};

make_pdu_impl(set, Varbinds) ->
    #pdu{type         = 'set-request', 
	 request_id   = request_id(),
	 error_status = noError, 
	 error_index  = 0, 
	 varbinds     = Varbinds}.


fix_vbs_BITS(Varbinds) ->
    [fix_vb_BITS(Varbind) || Varbind <- Varbinds].

fix_vb_BITS(#varbind{oid          = Oid,
		     variabletype = 'OCTET STRING' = _Type,
		     value        = Value} = Varbind) ->
    %% BITS are encoded as OCTET STRING, so this could be a BITS
    %% check with the MiniMIB
    case type_of_oid(Oid) of
	{error, _} ->
	    Varbind;
	{ok, NewType = 'BITS'} ->
	    NewValue = snmp_pdus:octet_str_to_bits(Value),
	    Varbind#varbind{variabletype = NewType,
			    value        = NewValue};
	_ ->
	    Varbind
    end;
fix_vb_BITS(Vb) ->
    Vb.

type_of_oid(Oid) ->
    Oid2 = case lists:reverse(Oid) of
	       [0|T] ->
		   lists:reverse(T);
	       _ ->
		   Oid
	   end,
    snmpm_config:oid_to_type(Oid2).


%%----------------------------------------------------------------------
%% Purpose: Unnesting of oids like [myTable, 3, 4, "hej", 45] to
%%          [1,2,3,3,4,104,101,106,45]
%%----------------------------------------------------------------------

flatten_oid([A|T], MiniMIB) when is_atom(A) ->
    Oid = [alias2oid(A, MiniMIB)|T],
    check_is_pure_oid(lists:flatten(Oid));
flatten_oid(Oid, _) when is_list(Oid) ->
    check_is_pure_oid(lists:flatten(Oid));
flatten_oid(Shit, _) ->
    throw({error, {invalid_oid, Shit}}).
	       
check_is_pure_oid([]) -> [];
check_is_pure_oid([X | T]) when is_integer(X) andalso (X >= 0) ->
    [X | check_is_pure_oid(T)];
check_is_pure_oid([X | _T]) ->
    throw({error, {invalid_oid, X}}).


var_and_value_to_varbind({Oid, Type, Value}, MiniMIB) ->
    Oid2 = flatten_oid(Oid, MiniMIB), 
    #varbind{oid          = Oid2, 
	     variabletype = char_to_type(Type), 
	     value        = Value};
var_and_value_to_varbind({Oid, Value}, MiniMIB) ->
    Oid2 = flatten_oid(Oid, MiniMIB), 
    #varbind{oid          = Oid2, 
	     variabletype = oid2type(Oid2, MiniMIB),
	     value        = Value}.

char_to_type(i) ->
    'INTEGER';
char_to_type(u) ->
    'Unsigned32';
char_to_type(g) -> % Gauge, Gauge32
    'Unsigned32';
char_to_type(b) -> 
    'BITS';
char_to_type(ip) -> 
    'IpAddress';
char_to_type(ia) -> 
    'IpAddress';
char_to_type(op) -> 
    'Opaque';
char_to_type(c32) -> 
    'Counter32';
char_to_type(c64) -> 
    'Counter64';
char_to_type(tt) -> 
    'TimeTicks';
char_to_type(o) ->
    'OBJECT IDENTIFIER';
char_to_type(s) ->
    'OCTET STRING';
char_to_type(C) ->
    throw({error, {invalid_value_type, C}}).


alias2oid(AliasName, MiniMIB) when is_atom(AliasName) ->
    case lists:keysearch(AliasName, 2, MiniMIB) of
	{value, {Oid, _Aliasname, _Type}} -> 
 	    Oid;
	false -> 
 	    throw({error, {unknown_aliasname, AliasName}})
    end.

oid2type(Oid, MiniMIB) ->
    Oid2 = case lists:reverse(Oid) of
	       [0|T] ->
		   lists:reverse(T);
	       _ ->
		   Oid
	   end,
    oid2type(Oid2, MiniMIB, utter_nonsense).

oid2type(_Oid, [], utter_nonsense) ->
    throw({error, no_type});
oid2type(_Oid, [], Type) ->
    Type;
oid2type(Oid, [{Oid2, _, Type}|MiniMIB], Res) when (Oid2 =< Oid) ->
    case lists:prefix(Oid2, Oid) of
        true ->
            oid2type(Oid, MiniMIB, Type); % A better guess
        false ->
            oid2type(Oid, MiniMIB, Res)
    end;
oid2type(_Oid, _MiniMIB, utter_nonsense) ->
    throw({error, no_type});
oid2type(_Oid, _MiniMIB, Type) ->
    Type.
    

make_vb(Oid) ->
    #varbind{oid = Oid, variabletype = 'NULL', value = 'NULL'}.


%%----------------------------------------------------------------------

request_id() ->
    snmpm_mpd:next_req_id().


%%----------------------------------------------------------------------

agent_data(TargetName, SendOpts) ->
    case snmpm_config:agent_info(TargetName, version) of
	{ok, Version} ->
	    MsgData = 
		case Version of
		    v3 ->
			DefSecModel = agent_data_item(sec_model, TargetName),
			DefSecName  = agent_data_item(sec_name,  TargetName),
			DefSecLevel = agent_data_item(sec_level, TargetName),
			
			EngineId    = agent_data_item(engine_id, TargetName),
			CtxName     = agent_data_item(context, 
						      SendOpts, 
						      ?DEFAULT_CONTEXT),
			
			SecModel    = agent_data_item(sec_model,   
						      SendOpts, 
						      DefSecModel),
			SecName     = agent_data_item(sec_name,    
						      SendOpts, 
						      DefSecName),
			SecLevel    = agent_data_item(sec_level,   
						      SendOpts, 
						      DefSecLevel),
			
			{SecModel, SecName, mk_sec_level_flag(SecLevel), 
			 EngineId, CtxName, TargetName};
		    _ ->
			DefComm     = agent_data_item(community, TargetName),
			DefSecModel = agent_data_item(sec_model, TargetName),
			
			Comm        = agent_data_item(community, 
						      SendOpts, 
						      DefComm),
			SecModel    = agent_data_item(sec_model, 
						      SendOpts, 
						      DefSecModel),
			
			{Comm, SecModel}
		end,
	    Domain  = agent_data_item(tdomain,  TargetName),
	    Addr    = agent_data_item(taddress, TargetName),
	    RegType = agent_data_item(reg_type, TargetName),
	    {ok, RegType, Domain, Addr, version(Version), MsgData};
	Error ->
	    Error
    end.

agent_data_item(Item, TargetName) ->
    case snmpm_config:agent_info(TargetName, Item) of
	{ok, Val} ->
	    Val;
	{error, not_found} ->
	    throw({error, {not_found, Item, TargetName}})
    end.

agent_data_item(Item, Info, Default) ->
    case lists:keysearch(Item, 1, Info) of
	{value, {_, Val}} ->
	    Val;
	false ->
	    Default
    end.


version(v1) ->
    'version-1';
version(v2) ->
    'version-2';
version(v3) ->
    'version-3'.


%%-----------------------------------------------------------------
%% Convert the SecurityLevel into a flag value used by snmpm_mpd
%%-----------------------------------------------------------------
mk_sec_level_flag(?'SnmpSecurityLevel_noAuthNoPriv') -> 0;
mk_sec_level_flag(?'SnmpSecurityLevel_authNoPriv') -> 1;
mk_sec_level_flag(?'SnmpSecurityLevel_authPriv') -> 3.


%%----------------------------------------------------------------------
%% Request Garbage Collector timer
%%----------------------------------------------------------------------

gct_start(Timeout) ->
    ?vdebug("start gc timer process (~p)", [Timeout]),    
    State = #gct{parent = self(), timeout = Timeout},
    proc_lib:start_link(?MODULE, gct_init, [State]).

gct_stop(GCT) ->
    GCT ! {stop, self()}.

gct_activate(GCT) ->
    GCT ! {activate, self()}.

gct_deactivate(GCT) ->
    GCT ! {deactivate, self()}.

gct_code_change(GCT) ->
    GCT ! {code_change, self()}.

gct_init(#gct{parent = Parent, timeout = Timeout} = State) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    gct(State, Timeout).

gct(#gct{parent = Parent, state = active} = State, Timeout) ->
    T = snmp_misc:now(ms),
    receive
	{stop, Parent} ->
	    ok;

	%% This happens when a new request is received.
	{activate, Parent}  ->
	    ?MODULE:gct(State, new_timeout(Timeout, T)); 

	{deactivate, Parent} ->
	    %% Timeout is of no consequence in the idle state, 
	    %% but just to be sure
	    NewTimeout = State#gct.timeout,
	    ?MODULE:gct(State#gct{state = idle}, NewTimeout);

	{code_change, Parent} ->
	    %% Let the server take care of this
	    exit(normal);

	{'EXIT', Parent, _Reason} ->
	    ok;

	_ -> % Crap
	    ?MODULE:gct(State, Timeout)

    after Timeout ->
	    Parent ! gc_timeout,
	    NewTimeout = State#gct.timeout,
	    ?MODULE:gct(State, NewTimeout)
    end;

gct(#gct{parent = Parent, state = idle} = State, Timeout) ->
    receive
	{stop, Parent} ->
	    ok;

	{deactivate, Parent} ->
	    ?MODULE:gct(State, Timeout);

	{activate, Parent} ->
	    NewTimeout = State#gct.timeout,
	    ?MODULE:gct(State#gct{state = active}, NewTimeout);

	{code_change, Parent} ->
	    %% Let the server take care of this
	    exit(normal);

	{'EXIT', Parent, _Reason} ->
	    ok;

	_ -> % Crap
	    ?MODULE:gct(State, Timeout)

    after Timeout ->
	    ?MODULE:gct(State, Timeout)
    end.

new_timeout(T1, T2) ->
    case T1 - (snmp_misc:now(ms) - T2) of
	T when (T > 0) ->
	    T;
	_ ->
	    0
    end.


%%----------------------------------------------------------------------
%% NetIF Supervisor
%%
%% The NIS process "supervises" the NetIF process. That does *not* mean
%% that it takes on the role of a standard erlang 'supervisor' process.
%% The NIS is instead a process that "actively" supervises by means
%% of "ping" messages, which the supervised process must respond to
%% (with a pong message) *in time*.
%% If it does not, NIS assumes that it has hung, and kills it.
%% The server process then restarts the NetIF process and informs NIS.
%%----------------------------------------------------------------------

nis_start(NetIF, PingTO, PongTO) ->
    ?vdebug("start nis process (~w, ~w)", [PingTO, PongTO]),
    State = #{parent     => self(),
              netif_pid  => NetIF,
              ping_to    => PingTO,
              ping_tref  => undefined,
              ping_start => undefined, % Time when ping sent
              ping_max   => undefined, % Max roundtrip time
              pong_to    => PongTO,
              pong_tref  => undefined,
              kill_cnt   => 0},
    proc_lib:start_link(?MODULE, nis_init, [State, get(verbosity)]).

nis_stop(NIS) when is_pid(NIS) ->
    NIS ! {?MODULE, self(), stop};
nis_stop(_) ->
    ok.


-dialyzer({nowarn_function, nis_info/1}).
nis_info(NIS) when is_pid(NIS) ->
    NIS ! {?MODULE, self(), info},
    receive
        {?MODULE, NIS, {info, Info}} ->
            Info
    after 1000 ->
            []
    end;
nis_info(_) ->
    [].


nis_netif(NIS, NetIF) when is_pid(NIS) andalso is_pid(NetIF) ->
    NIS ! {?MODULE, self(), {netif, NetIF}};
nis_netif(_, _) ->
    ok.


nis_init(#{parent    := Parent,
           netif_pid := NetIF,
           ping_to   := PingTO} = State,
         Verbosity) ->
    put(verbosity, Verbosity),
    put(sname, mnis),
    ?vlog("starting"),
    MRef = erlang:monitor(process, NetIF),
    TRef = erlang:start_timer(PingTO, self(), ping_timeout),
    erlang:register(snmpm_server_nis, self()),
    proc_lib:init_ack(Parent, self()),
    ?vlog("started"),
    nis_loop(State#{netif_mref => MRef,
                    ping_tref  => TRef}).

%% The NetIF is dead. Its up to the server to restart it and inform us
nis_loop(#{parent    := Parent,
           netif_pid := undefined,
           ping_tref := undefined,
           pong_tref := undefined} = State) ->
    receive
        {?MODULE, Parent, stop} ->
            ?vlog("[idle] stop received"),
            nis_handle_stop(State),
            exit(normal);

        {?MODULE, Pid, info} ->
            ?vtrace("[idle] info received"),
	    Info = nis_handle_info(State),
	    Pid ! {?MODULE, self(), {info, Info}},
            ?MODULE:nis_loop(State);

        {?MODULE, Parent, {netif, NetIF}} ->
            ?vlog("[idle] (new) netif started: ~p => start ping timer", [NetIF]),
            MRef   = erlang:monitor(process, NetIF),
            PingTO = maps:get(ping_to, State),
            TRef   = erlang:start_timer(PingTO, self(), ping_timeout),
            ?MODULE:nis_loop(State#{netif_pid  => NetIF,
                                    netif_mref => MRef,
                                    ping_max   => undefined,
                                    ping_tref  => TRef});

        _Any ->
            ?MODULE:nis_loop(State)

    after 5000 ->
            %% This is for code upgrade
            ?MODULE:nis_loop(State)
    end;

%% PING timer running (waiting for ping-timeout)
nis_loop(#{parent     := Parent,
           netif_pid  := NetIF,
           netif_mref := MRef,
           ping_tref  := PingTRef,
           pong_tref  := undefined} = State) when is_pid(NetIF) andalso
                                                 (PingTRef =/= undefined) ->
    receive
        {'DOWN', MRef, process, NetIF, _} ->
            ?vlog("[ping] netif died => cancel ping timer"),
            erlang:cancel_timer(PingTRef),            
            ?MODULE:nis_loop(State#{netif_pid  => undefined,
                                    netif_mref => undefined,
                                    ping_tref  => undefined});

        {?MODULE, Parent, stop} ->
            ?vlog("[ping] stop received"),
            nis_handle_stop(State),
            exit(normal);

        {?MODULE, Pid, info} ->
            ?vtrace("[ping] info received"),
	    Info = nis_handle_info(State),
	    Pid ! {?MODULE, self(), {info, Info}},
            ?MODULE:nis_loop(State);

        %% Time to ping NetIF
        {timeout, PingTRef, ping_timeout} ->
            ?vlog("[ping] (ping-) timer timeout => send ping and start pong timer"),
            NetIF ! {ping, self()},
            PongTO = maps:get(pong_to, State),
            TRef   = erlang:start_timer(PongTO, self(), pong_timeout),
            ?MODULE:nis_loop(State#{ping_tref  => undefined,
                                    ping_start => us(),
                                    pong_tref  => TRef});

        _Any ->
            ?MODULE:nis_loop(State)

    after 5000 ->
            %% This is for code upgrade
            ?MODULE:nis_loop(State)
    end;

%% PONG timer running (waiting for pong message)
nis_loop(#{parent     := Parent,
           netif_pid  := NetIF,
           netif_mref := MRef,
           ping_tref  := undefined,
           pong_tref  := PongTRef} = State) when is_pid(NetIF) andalso
                                                 (PongTRef =/= undefined) ->
    receive
        {'DOWN', MRef, process, NetIF, _} ->
            ?vlog("[pong] netif died => cancel pong timer"),
            erlang:cancel_timer(PongTRef),            
            ?MODULE:nis_loop(State#{netif_pid  => undefined,
                                    netif_mref => undefined,
                                    pong_tref  => undefined});

        {?MODULE, Parent, stop} ->
            ?vlog("[pong] stop received"),
            nis_handle_stop(State),
            exit(normal);

        {?MODULE, Pid, info} ->
            ?vlog("[pong] info received"),
	    Info = nis_handle_info(State),
	    Pid ! {?MODULE, self(), {info, Info}},
            ?MODULE:nis_loop(State);

        {pong, NetIF} ->
            ?vlog("[pong] received pong => cancel pong timer, start ping timer"),
            T = us(),
            erlang:cancel_timer(PongTRef),
            Start    = maps:get(ping_start, State),
            Max      = maps:get(ping_max, State),
            RT       = T - Start,
            NewMax   =
                if
                    (Max =:= undefined) ->
                        ?vtrace("[pong] Max: ~w", [RT]),
                        RT;
                    (RT > Max) ->
                        ?vtrace("[pong] New Max: ~w", [RT]),
                        RT;
                   true ->
                        Max
                end,
            PingTO   = maps:get(ping_to, State),
            PingTRef = erlang:start_timer(PingTO, self(), ping_timeout),
            ?MODULE:nis_loop(State#{ping_tref  => PingTRef,
                                    ping_start => undefined,
                                    ping_max   => NewMax,
                                    pong_tref  => undefined});

        %% Time to kill NetIF
        {timeout, PongTRef, pong_timeout} ->
            ?vinfo("[pong] (pong-) timer timeout => kill NetIF (~p)", [NetIF]),
            nis_handle_pong_timeout(NetIF, MRef),
            KCnt = maps:get(kill_cnt, State),
            ?MODULE:nis_loop(State#{netif_pid  => undefined,
                                    netif_mref => undefined,
                                    pong_tref  => undefined,
                                    kill_cnt   => KCnt + 1});

        _Any ->
            ?MODULE:nis_loop(State)

    after 5000 ->
            %% This is for code upgrade
            ?MODULE:nis_loop(State)
    end.


nis_handle_info(#{ping_max := undefined, kill_cnt := KCnt}) ->
    [{kcnt, KCnt}];
nis_handle_info(#{ping_max := Max, kill_cnt := KCnt}) ->
    [{max, Max}, {kcnt, KCnt}].


nis_handle_stop(#{pong_tref := PongTRef,
                  ping_tref := PingTRef}) ->
    cancel_timer(PongTRef),
    cancel_timer(PingTRef).
            
%% Inform all users that the netif process has been killed
nis_handle_pong_timeout(NetIF, MRef) ->
    erlang:demonitor(MRef, [flush]),
    exit(NetIF, kill),
    netif_down_inform_users({pong, killed}),
    ok.


%%----------------------------------------------------------------------

%% Inform all users that "something" fatal happened to the NetIF process.
netif_down_inform_users(Reason) ->
    InformUser = fun(UserId) ->
                         spawn(fun() ->
                                       case snmpm_config:user_info(UserId) of
                                           {ok, UserMod, UserData} ->
                                               UserMod:handle_error(netif,
                                                                    Reason,
                                                                    UserData);
                                           {error, _} ->
                                               ok
                                       end,
                                       exit(normal)
                               end)
                 end,
    lists:foreach(InformUser, snmpm_config:which_users()).
    

%%----------------------------------------------------------------------

maybe_delete(false, ReqId) ->
    ets:delete(snmpm_request_table, ReqId);
maybe_delete(true, _) ->
    ok.
    
maybe_demonitor(undefined) ->
    ok;
maybe_demonitor(MonRef) ->
    erlang:demonitor(MonRef).

mk_target_name(Domain, Addr, Config) ->
    snmpm_config:mk_target_name(Domain, Addr, Config).

default_agent_config() ->
    case snmpm_config:agent_info() of
	{ok, Config} ->
	    Config;
	_ ->
	    []
    end.


%%----------------------------------------------------------------------

get_opt(Key, Default, Opts) ->
    proplists:get_value(Key, Opts, Default).


%%----------------------------------------------------------------------

is_started(#state{net_if = _Pid, net_if_mod = _Mod}) ->
    %% Mod:is_started(Pid) and snmpm_config:is_started().
    case snmpm_config:is_started() of
	true ->
	    true;
	_ ->
	    false
    end.


%%----------------------------------------------------------------------
	
cast(Req) ->
    gen_server:cast(?SERVER, Req).

call(Req) ->
    call(Req, infinity).

call(Req, To) ->
    gen_server:call(?SERVER, Req, To).

warning_msg(F) ->
    warning_msg(F, []).
warning_msg(F, A) ->
    ?snmpm_warning("Server: " ++ F, A).

error_msg(F, A) ->
    ?snmpm_error("Server: " ++ F, A).
 

%%----------------------------------------------------------------------

get_info(#state{gct        = GCT, 
		net_if     = NI,
                net_if_mod = NIMod, 
		note_store = NS,
                nis_pid    = NIS,
		cbproxy    = CBP}) ->
    Info = [{server,         server_info(GCT, CBP, NIS)},
	    {config,         config_info()},
	    {net_if,         net_if_info(NI, NIMod)},
	    {note_store,     note_store_info(NS)},
	    {stats_counters, get_stats_counters()}],
    Info.

server_info(GCT, CBP, NIS) ->
    {CBPInfo, CBPSz} =
	if
	    (CBP =:= permanent) ->
		{[{cbp, cbproxy_info()}],
		 [{cbp, proc_mem(whereis(?CBPROXY))}]};
	    true ->
		{[], []}
	end,
    {NISInfo, NISSz} =
        case NIS of
            undefined ->
                {[], []};
            _ ->
                {[{nis, nis_info(NIS)}],
                 [{nis, proc_mem(NIS)}]}
        end,
    ProcSize = proc_mem(self()),
    GCTSz    = proc_mem(GCT),
    RTSz     = tab_size(snmpm_request_table),
    MTSz     = tab_size(snmpm_monitor_table),
    [{process_memory, [{server, ProcSize}, {gct, GCTSz}] ++ CBPSz ++ NISSz},
     {db_memory, [{request, RTSz}, {monitor, MTSz}]}] ++ CBPInfo ++ NISInfo.

proc_mem(P) when is_pid(P) ->
    case (catch erlang:process_info(P, memory)) of
	{memory, Sz} when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end;
proc_mem(_) ->
    undefined.

tab_size(T) ->
    case (catch ets:info(T, memory)) of
	Sz when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end.

config_info() ->
    case (catch snmpm_config:info()) of
	Info when is_list(Info) ->
	    Info;
	E ->
	    [{error, E}]
    end.

get_stats_counters() ->
    lists:sort(snmpm_config:get_stats_counters()).
    

net_if_info(Pid, Mod) ->
    case (catch Mod:info(Pid)) of
	Info when is_list(Info) ->
	    Info;
	E ->
	    [{error, E}]
    end.

note_store_info(Pid) ->
    case (catch snmp_note_store:info(Pid)) of
	Info when is_list(Info) ->
	    Info;
	E ->
	    [{error, E}]
    end.


%%----------------------------------------------------------------------

us() ->
    erlang:monotonic_time(micro_seconds).
