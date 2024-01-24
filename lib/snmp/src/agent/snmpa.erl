%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2024. All Rights Reserved.
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
-module(snmpa).


%%----------------------------------------------------------------------
%% This module contains the user interface to the snmp agent toolkit.
%%----------------------------------------------------------------------

-export([verbosity/2, 
	 
	 current_request_id/0, current_community/0, current_address/0,
	 current_context/0, current_net_if_data/0, 
	 
	 get_symbolic_store_db/0, 
	 which_aliasnames/0, 
	 which_tables/0, 
	 which_variables/0, 
	 which_notifications/0, 
	 name_to_oid/1, name_to_oid/2, 
	 oid_to_name/1, oid_to_name/2,
	 int_to_enum/2, int_to_enum/3, 
	 enum_to_int/2, enum_to_int/3,

         name_db/1,
	 info/0, info/1,
	 load_mib/1, load_mib/2, 
	 load_mibs/1, load_mibs/2, load_mibs/3, 
	 unload_mib/1, unload_mib/2, 
	 unload_mibs/1, unload_mibs/2, unload_mibs/3, 
	 which_mibs/0, which_mibs/1, 
	 whereis_mib/1, whereis_mib/2, 
	 dump_mibs/0, dump_mibs/1,
	 mib_of/1, mib_of/2, 
	 me_of/1,  me_of/2, 
	 invalidate_mibs_cache/0, invalidate_mibs_cache/1, 
	 which_mibs_cache_size/0, which_mibs_cache_size/1, 
	 enable_mibs_cache/0, enable_mibs_cache/1, 
	 disable_mibs_cache/0, disable_mibs_cache/1,
	 gc_mibs_cache/0, gc_mibs_cache/1, gc_mibs_cache/2, gc_mibs_cache/3,
	 enable_mibs_cache_autogc/0, enable_mibs_cache_autogc/1, 
	 disable_mibs_cache_autogc/0, disable_mibs_cache_autogc/1,
	 update_mibs_cache_age/1, update_mibs_cache_age/2, 
	 update_mibs_cache_gclimit/1, update_mibs_cache_gclimit/2,

	 get/2, get/3, get_next/2, get_next/3,

	 register_subagent/3, unregister_subagent/2, 

         which_transports/0,

	 send_notification2/3, 
	 send_notification/3, send_notification/4, send_notification/5,
	 send_notification/6, send_notification/7, 
	 send_trap/3, send_trap/4,

	 discovery/2, discovery/3, discovery/4, discovery/5, discovery/6, 

 	 sys_up_time/0, system_start_time/0,

	 backup/1, backup/2, 

	 convert_config/1,

	 restart_worker/0,     restart_worker/1, 
	 restart_set_worker/0, restart_set_worker/1]).

%% USM functions:
-export([passwd2localized_key/3, localize_key/3]).

%% Agent Capabilities functions
-export([add_agent_caps/2, del_agent_caps/1, get_agent_caps/0]).

%% Audit Trail Log functions
-export([log_to_txt/1, log_to_txt/2, log_to_txt/3, log_to_txt/4, 
	 log_to_txt/5, log_to_txt/6, log_to_txt/7, log_to_txt/8, 
	 log_to_io/1,  log_to_io/2,  log_to_io/3,  log_to_io/4, 
	 log_to_io/5,  log_to_io/6,  log_to_io/7, 
	 log_info/0, 
	 change_log_size/1,
	 get_log_type/0,    get_log_type/1, 
	 change_log_type/1, change_log_type/2,
	 set_log_type/1,    set_log_type/2
	]).

%% Message filter / load regulation functions
-export([
         register_notification_filter/3,
         register_notification_filter/4,
         register_notification_filter/5,
         unregister_notification_filter/1,
         unregister_notification_filter/2,
         which_notification_filter/0,
         which_notification_filter/1,
	 
	 get_request_limit/0, get_request_limit/1,
	 set_request_limit/1, set_request_limit/2
	]).

-export([print_mib_info/0, print_mib_tables/0, print_mib_variables/0]).

-export_type([
              me/0,

              table_name/0,
              variable_name/0,
              name/0,
              db/0,
              name_db/0,

              pdu_type/0,

              discovery_handler/0,

	      %% Agent config types
	      mib_storage/0, 
	      mib_storage_opt/0, 
	      mib_storage_module/0, 
	      mib_storage_options/0,

              nfilter_id/0,
              nfilter_position/0,

              notification_delivery_info/0
             ]).

-include("snmpa_atl.hrl").
-include("snmpa_internal.hrl").
-include_lib("snmp/include/snmp_types.hrl"). % type of me needed. 

-define(DISCO_EXTRA_INFO,  undefined).
-define(ATL_BLOCK_DEFAULT, true).


%%-----------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------

-type me() :: snmp:me().

-type discovery_handler() :: module().

%% Agent config types
-type mib_storage()     :: [mib_storage_opt()].
-type mib_storage_opt() :: {module,  mib_storage_module()} | 
                           {options, mib_storage_options()}.

%% Module implementing the snmpa_mib_storage behaviour
-type mib_storage_module()  :: atom(). 
%% Options specific to the above module
-type mib_storage_options() :: list().

-type table_name()    :: atom().
-type variable_name() :: atom().
-type name()          :: table_name() | variable_name().
-type db()            :: volatile | persistent | mnesia.
-type name_db()       :: {name(), db()}.
-type mib_module()    :: atom().
-type mib_info()      :: {mib_module(), [table_name()], [variable_name()]}.
-type pdu_type()      :: snmp:pdu_type().

-type nfilter_id()       :: term().
-type nfilter_position() :: first | last |
                            {insert_before, nfilter_id()} |
                            {insert_after,  nfilter_id()}.

-type notification_delivery_info() ::
        snmpa_notification_delivery_info_receiver:notification_delivery_info().


%%-----------------------------------------------------------------

-spec name_db(Name) -> NameDb when
      Name   :: name(),
      NameDb :: name_db().

name_db(Name) ->
    snmpa_agent:db(Name).


%%-----------------------------------------------------------------
%% This utility function is used to convert an old SNMP application
%% config (prior to snmp-4.0) to a SNMP agent config (as of 
%% snmp-4.0).
%% This is the config structure of the SNMP application as of 
%% snmp-4.0:
%% {snmp, snmp_config()}
%% snmp_config() -> [snmp_config_item()]
%% snmp_config_item() -> {agent,   agent_config()} | 
%%                       {manager, manager_config()}
%%-----------------------------------------------------------------

-spec convert_config(OldConfig) -> NewConfig when
      OldConfig :: list(),
      NewConfig :: list().

convert_config(Opts) ->
    snmpa_app:convert_config(Opts).


%%-----------------------------------------------------------------
%% Note that verbosity for the agents is actually only implemented 
%% (properly) for the master agent.
%%-----------------------------------------------------------------

-spec verbosity(Target, Verbosity) -> snmp:void() when
      Target    :: all,
      Verbosity :: snmp:verbosity();
               (Target, Verbosity) -> snmp:void() when
      Target    :: net_if,
      Verbosity :: snmp:verbosity();
               (Target, Verbosity) -> snmp:void() when
      Target    :: note_store,
      Verbosity :: snmp:verbosity();
               (Target, Verbosity) -> snmp:void() when
      Target    :: mib_server,
      Verbosity :: snmp:verbosity();
               (Target, Verbosity) -> snmp:void() when
      Target    :: symbolic_store,
      Verbosity :: snmp:verbosity();
               (Target, Verbosity) -> snmp:void() when
      Target    :: local_db,
      Verbosity :: snmp:verbosity();
               (Target, Verbosity) -> snmp:void() when
      Target    :: master_agent | Agent,
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Verbosity :: {subagents, snmp:verbosity()};
               (Agent, Verbosity) -> snmp:void() when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Verbosity :: snmp:verbosity().

verbosity(all = _Target, Verbosity) when is_atom(Verbosity) ->
    catch snmpa_agent:verbosity(sub_agents,   Verbosity),
    catch snmpa_agent:verbosity(master_agent, Verbosity),
    catch snmpa_agent:verbosity(net_if,       Verbosity),
    catch snmpa_agent:verbosity(mib_server,   Verbosity),
    catch snmpa_agent:verbosity(note_store,   Verbosity),
    catch snmpa_symbolic_store:verbosity(Verbosity),
    catch snmpa_local_db:verbosity(Verbosity);
verbosity(net_if = Target, Verbosity) when is_atom(Verbosity) ->
    catch snmpa_agent:verbosity(Target, Verbosity);
verbosity(note_store = Target, Verbosity) when is_atom(Verbosity) ->
    catch snmpa_agent:verbosity(Target, Verbosity);
verbosity(mib_server = Target, Verbosity) when is_atom(Verbosity) ->
    catch snmpa_agent:verbosity(Target,Verbosity);
verbosity(symbolic_store = _Target, Verbosity) when is_atom(Verbosity) ->
    catch snmpa_symbolic_store:verbosity(Verbosity);
verbosity(local_db = _Target, Verbosity) when is_atom(Verbosity) ->
    catch snmpa_local_db:verbosity(Verbosity);
verbosity(Target, {subagents, Verbosity})
  when ((Target =:= master_agent) orelse
        is_pid(Target) orelse
        is_atom(Target)) andalso
       is_atom(Verbosity) ->
    catch snmpa_agent:verbosity(Target, {sub_agents, Verbosity});
verbosity(Target, Verbosity)
  when (is_pid(Target) orelse is_atom(Target)) andalso
       is_atom(Verbosity) ->
    catch snmpa_agent:verbosity(Target, Verbosity).


%%-----------------------------------------------------------------
%% 
%% Some symbolic store (internal database) utility functions
%% 
%%-----------------------------------------------------------------

get_symbolic_store_db() ->
    snmpa_symbolic_store:get_db().


-spec which_aliasnames() -> AliasNames when
      AliasNames :: [AliasName],
      AliasName  :: atom().

which_aliasnames() ->
    snmpa_symbolic_store:which_aliasnames().

-spec which_tables() -> Tables when
      Tables :: [Table],
      Table  :: atom().

which_tables() ->
    snmpa_symbolic_store:which_tables().

-spec which_variables() -> Variables when
      Variables :: [Variable],
      Variable  :: atom().

which_variables() ->
    snmpa_symbolic_store:which_variables().

-spec which_notifications() -> Notifications when
      Notifications :: [{Name, MibName, Info}],
      Name          :: atom(),
      MibName       :: atom(),
      Info          :: term().

which_notifications() ->
    snmpa_symbolic_store:which_notifications().


%%-----------------------------------------------------------------
%% These 8 functions returns {value, Val} | false
%%-----------------------------------------------------------------

-spec name_to_oid(Name) -> {value, Oid} | false when
      Name :: atom(),
      Oid  :: snmp:oid().

name_to_oid(Name) ->
    snmpa_symbolic_store:aliasname_to_oid(Name).

-spec name_to_oid(Db, Name) -> {value, Oid} | false when
      Db   :: term(),
      Name :: atom(),
      Oid  :: snmp:oid().

name_to_oid(Db, Name) ->
    snmpa_symbolic_store:aliasname_to_oid(Db, Name).


-spec oid_to_name(Oid) -> {value, Name} | false when
      Oid  :: snmp:oid(),
      Name :: atom().

oid_to_name(Oid) ->
    snmpa_symbolic_store:oid_to_aliasname(Oid).

-spec oid_to_name(Db, Oid) -> {value, Name} | false when
      Db   :: term(),
      Oid  :: snmp:oid(),
      Name :: atom().

oid_to_name(Db, Oid) ->
    snmpa_symbolic_store:oid_to_aliasname(Db, Oid).


-spec enum_to_int(Name, Enum) -> {value, Int} | false when
      Name :: atom(),
      Enum :: atom(),
      Int  :: integer().

enum_to_int(Name, Enum) ->
    snmpa_symbolic_store:enum_to_int(Name, Enum).

-spec enum_to_int(Db, Name, Enum) -> {value, Int} | false when
      Db   :: term(),
      Name :: atom(),
      Enum :: atom(),
      Int  :: integer().

enum_to_int(Db, Name, Enum) ->
    snmpa_symbolic_store:enum_to_int(Db, Name, Enum).


-spec int_to_enum(Name, Int) -> {value, Enum} | false when
      Name :: atom(),
      Int  :: integer(),
      Enum :: atom().

int_to_enum(Name, Int) ->
    snmpa_symbolic_store:int_to_enum(Name, Int).

-spec int_to_enum(Db, Name, Int) -> {value, Enum} | false when
      Db   :: term(),
      Name :: atom(),
      Int  :: integer(),
      Enum :: atom().

int_to_enum(Db, Name, Int) ->
    snmpa_symbolic_store:int_to_enum(Db, Name, Int).


%%-----------------------------------------------------------------
%% These functions must only be called in the process context
%% where the instrumentation functions are called!
%%-----------------------------------------------------------------

-spec current_address() -> {value, Address} | false when
      Address :: term().

current_address()     -> current_get(snmp_address).

-spec current_community() -> {value, Community} | false when
      Community :: snmp_community_mib:name().

current_community()   -> current_get(snmp_community).

-spec current_context() -> {value, Context} | false when
      Context :: snmp_community_mib:context_name().

current_context()     -> current_get(snmp_context).

-spec current_request_id() -> {value, RequestId} | false when
      RequestId :: integer().

current_request_id()  -> current_get(snmp_request_id).

current_net_if_data() -> current_get(net_if_data).

current_get(Tag) ->
    case get(Tag) of
	undefined -> false;
	X -> {value, X}
    end.
    

%% -

-spec get(Agent, Vars) -> Values | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Vars      :: [snmp:oid()],
      Values    :: [term()],
      Reason    :: term().

get(Agent, Vars) -> snmpa_agent:get(Agent, Vars).

-spec get(Agent, Vars, Context) -> Values | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Vars      :: [snmp:oid()],
      Context   :: snmp_community_mib:context_name(),
      Values    :: [term()],
      Reason    :: term().

get(Agent, Vars, Context) -> snmpa_agent:get(Agent, Vars, Context).


-spec get_next(Agent, Vars) -> Values | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Vars      :: [snmp:oid()],
      Values    :: [{snmp:oid(), term()}],
      Reason    :: term().

get_next(Agent, Vars) -> snmpa_agent:get_next(Agent, Vars).

-spec get_next(Agent, Vars, Context) -> Values | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Vars      :: [snmp:oid()],
      Context   :: snmp_community_mib:context_name(),
      Values    :: [{snmp:oid(), term()}],
      Reason    :: {atom(), snmp:oid()}.

get_next(Agent, Vars, Context) -> snmpa_agent:get_next(Agent, Vars, Context).


-spec info() -> Info when
      Info  :: [{Key, Value}],
      Key   :: term(),
      Value :: term().

info()      -> info(snmp_master_agent).

-spec info(Agent) -> Info when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Info      :: [{Key, Value}],
      Key       :: term(),
      Value     :: term().

info(Agent) -> snmpa_agent:info(Agent).


%% -

-spec backup(BackupDir) -> ok | {error, Reason} when
      BackupDir :: string(),
      Reason    :: backup_in_progress | term().

backup(BackupDir) ->
    backup(snmp_master_agent, BackupDir).

-spec backup(Agent, BackupDir) -> ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      BackupDir :: string(),
      Reason    :: backup_in_progress | term().

backup(Agent, BackupDir) ->
    snmpa_agent:backup(Agent, BackupDir).


%% -

dump_mibs()     -> snmpa_agent:dump_mibs(snmp_master_agent).
dump_mibs(File) -> snmpa_agent:dump_mibs(snmp_master_agent, File).


-spec load_mib(Mib) -> 
          ok | {error, Reason} when
      Mib    :: string(),
      Reason :: already_loaded | term().

load_mib(Mib) ->
    load_mib(snmp_master_agent, Mib).

-spec load_mib(Agent, Mib) ->
          ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Mib       :: string(),
      Reason    :: already_loaded | term().

load_mib(Agent, Mib) ->
    case load_mibs(Agent, [Mib]) of
	{error, {'load aborted at', Mib, Reason}} ->
	    {error, Reason};
	Else ->
	    Else
    end.


-spec load_mibs(Mibs) -> 
          ok | {error, Reason} when
      Mibs           :: [MibName],
      MibName        :: string(),
      Reason         :: {'load aborted at', MibName, InternalReason},
      InternalReason :: already_loaded | term().

load_mibs(Mibs) ->
    load_mibs(snmp_master_agent, Mibs, false).

-spec load_mibs(Agent, Mibs) -> ok | {error, Reason} when
      Agent          :: pid() | AgentName,
      AgentName      :: atom(),
      Mibs           :: [MibName],
      MibName        :: string(),
      Reason         :: {'load aborted at', MibName, InternalReason},
      InternalReason :: already_loaded | term();
               (Mibs, Force) -> ok | {error, Reason} when
      Mibs           :: [MibName],
      MibName        :: string(),
      Force          :: boolean(),
      Reason         :: {'load aborted at', MibName, InternalReason},
      InternalReason :: already_loaded | term().

load_mibs(Agent, Mibs) when is_list(Mibs) -> 
    snmpa_agent:load_mibs(Agent, Mibs, false);
load_mibs(Mibs, Force) 
  when is_list(Mibs) andalso is_boolean(Force) ->
    load_mibs(snmp_master_agent, Mibs, Force).

-spec load_mibs(Agent, Mibs, Force) -> ok | {error, Reason} when
      Agent          :: pid() | AgentName,
      AgentName      :: atom(),
      Mibs           :: [MibName],
      MibName        :: string(),
      Force          :: boolean(),
      Reason         :: {'load aborted at', MibName, InternalReason},
      InternalReason :: already_loaded | term().

load_mibs(Agent, Mibs, Force) 
  when is_list(Mibs) andalso is_boolean(Force) -> 
    snmpa_agent:load_mibs(Agent, Mibs, Force).


-spec unload_mib(Mib) ->
          ok | {error, Reason} when
      Mib    :: string(),
      Reason :: not_loaded | term().

unload_mib(Mib) ->
    unload_mib(snmp_master_agent, Mib).

-spec unload_mib(Agent, Mib) ->
          ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Mib       :: string(),
      Reason    :: not_loaded | term().

unload_mib(Agent, Mib) ->
    case unload_mibs(Agent, [Mib]) of
	{error, {'unload aborted at', Mib, Reason}} ->
	    {error, Reason};
	Else ->
	    Else
    end.


-spec unload_mibs(Mibs) ->
          ok | {error, Reason} when
      Mibs           :: [MibName], 
      MibName        :: string(),
      Reason         :: {'unload aborted at', MibName, InternalReason},
      InternalReason :: not_loaded | term().

unload_mibs(Mibs) ->
    unload_mibs(snmp_master_agent, Mibs).

-spec unload_mibs(Agent, Mibs) ->
          ok | {error, Reason} when
      Agent          :: pid() | AgentName, 
      AgentName      :: atom(),
      Mibs           :: [MibName], 
      MibName        :: string(),
      Reason         :: {'unload aborted at', MibName, InternalReason},
      InternalReason :: not_loaded | term();
                 (Mibs, Force) ->
          ok | {error, Reason} when
      Mibs           :: [MibName], 
      MibName        :: string(),
      Force          :: boolean(),
      Reason         :: {'unload aborted at', MibName, InternalReason},
      InternalReason :: not_loaded | term().

unload_mibs(Agent, Mibs) when is_list(Mibs) -> 
    unload_mibs(Agent, Mibs, false);
unload_mibs(Mibs, Force) 
  when is_list(Mibs) andalso is_boolean(Force) ->
    unload_mibs(snmp_master_agent, Mibs, Force).

-spec unload_mibs(Agent, Mibs, Force) ->
          ok | {error, Reason} when
      Agent          :: pid() | AgentName, 
      AgentName      :: atom(),
      Mibs           :: [MibName], 
      MibName        :: string(),
      Force          :: boolean(),
      Reason         :: {'unload aborted at', MibName, InternalReason},
      InternalReason :: not_loaded | term().

unload_mibs(Agent, Mibs, Force) 
  when is_list(Mibs) andalso is_boolean(Force) ->
    snmpa_agent:unload_mibs(Agent, Mibs, Force).


-spec which_mibs() -> Mibs when
      Mibs    :: [{MibName, MibFile}],
      MibName :: atom(),
      MibFile :: string().

which_mibs()      -> which_mibs(snmp_master_agent).

-spec which_mibs(Agent) -> Mibs when
      Agent     :: pid() | AgentName, 
      AgentName :: atom(),
      Mibs      :: [{MibName, MibFile}],
      MibName   :: atom(),
      MibFile   :: string().

which_mibs(Agent) -> snmpa_agent:which_mibs(Agent).


-spec whereis_mib(MibName) -> {ok, MibFile} | {error, Reason} when
      MibName :: atom(),
      MibFile :: string(),
      Reason  :: term().

whereis_mib(Mib) ->
    whereis_mib(snmp_master_agent, Mib).

-spec whereis_mib(Agent, MibName) -> {ok, MibFile} | {error, Reason} when
      Agent     :: pid() | AgentName, 
      AgentName :: atom(),
      MibName   :: atom(),
      MibFile   :: string(),
      Reason    :: term().

whereis_mib(Agent, MibName) when is_atom(MibName) ->
    snmpa_agent:whereis_mib(Agent, MibName).


%% -

-spec mibs_info() -> [mib_info()].

mibs_info() ->
    [
     {snmp_standard_mib, 
      [],
      [
       sysDescr, 
       sysObjectID, 
       sysContact, 
       sysName, 
       sysLocation, 
       sysServices, 
       snmpEnableAuthenTraps,
       sysUpTime,
       snmpInPkts,
       snmpOutPkts, 
       snmpInBadVersions, 
       snmpInBadCommunityNames, 
       snmpInBadCommunityUses, 
       snmpInASNParseErrs, 
       snmpInTooBigs, 
       snmpInNoSuchNames, 
       snmpInBadValues, 
       snmpInReadOnlys, 
       snmpInGenErrs, 
       snmpInTotalReqVars, 
       snmpInTotalSetVars, 
       snmpInGetRequests, 
       snmpInSetRequests, 
       snmpInGetNexts, 
       snmpInGetResponses, 
       snmpInTraps, 
       snmpOutTooBigs, 
       snmpOutNoSuchNames, 
       snmpOutBadValues, 
       snmpOutGenErrs, 
       snmpOutGetRequests, 
       snmpOutSetRequests, 
       snmpOutGetNexts, 
       snmpOutGetResponses, 
       snmpOutTraps
      ]
     },
     {snmp_framework_mib, 
      [
      ],
      [
       snmpEngineID,
       snmpEngineBoots,
       snmpEngineTime,
       snmpEngineMaxMessageSize
      ]
     },
     {snmp_view_based_acm_mib, 
      [
       vacmAccessTable,
       vacmSecurityToGroupTable,
       vacmViewTreeFamilyTable
      ],
      [
       vacmViewSpinLock
      ]
     },
     {snmp_target_mib, 
      [
       snmpTargetAddrTable,
       snmpTargetParamsTable
      ], 
      [
       snmpTargetSpinLock
      ]
     },
     {snmp_community_mib, 
      [
       snmpCommunityTable
      ], 
      []
     },
     {snmp_notification_mib, 
      [
       snmpNotifyTable
      ], 
      []},
     {snmp_user_based_sm_mib, 
      [
       usmUserTable
      ], 
      [
       usmUserSpinLock,
       usmStatsUnsupportedSecLevels, 
       usmStatsNotInTimeWindows, 
       usmStatsUnknownUserNames, 
       usmStatsUnknownEngineIDs, 
       usmStatsWrongDigests, 
       usmStatsDecryptionErrors
      ]
     }
    ].


-spec print_mib_info() -> snmp:void().

print_mib_info() ->
    MibsInfo = mibs_info(),
    print_mib_info(MibsInfo).

print_mib_info([]) ->
    io:format("~n", []),
    ok;
print_mib_info([{Mod, Tables, Variables} | MibsInfo]) ->
    io:format("~n** ~s ** ~n~n", [make_pretty_mib(Mod)]),
    print_mib_variables2(Mod, Variables),
    print_mib_tables2(Mod, Tables),
    io:format("~n", []),
    print_mib_info(MibsInfo).


-spec print_mib_tables() -> snmp:void().

print_mib_tables() ->
    Tables = [{Mod, Tabs} || {Mod, Tabs, _Vars} <- mibs_info()],
    print_mib_tables(Tables).

print_mib_tables([]) ->
    ok;
print_mib_tables([{Mod, Tabs}|MibTabs]) 
  when is_atom(Mod) andalso is_list(Tabs) ->
    print_mib_tables(Mod, Tabs),
    print_mib_tables(MibTabs);
print_mib_tables([_|MibTabs]) ->
    print_mib_tables(MibTabs).

print_mib_tables(_Mod, [] = _Tables) ->
    ok;
print_mib_tables(Mod, Tables) ->
    io:format("~n** ~s ** ~n~n", [make_pretty_mib(Mod)]),
    print_mib_tables2(Mod, Tables), 
    io:format("~n", []).

print_mib_tables2(Mod, Tables) ->
    [(catch Mod:Table(print)) || Table <- Tables].


-spec print_mib_variables() -> snmp:void().

print_mib_variables() ->
    Variables = [{Mod, Vars} || {Mod, _Tabs, Vars} <- mibs_info()],
    print_mib_variables(Variables).

print_mib_variables([]) ->
    ok;
print_mib_variables([{Mod, Vars}|MibVars]) 
  when is_atom(Mod) andalso is_list(Vars) ->
    print_mib_variables(Mod, Vars),
    print_mib_variables(MibVars);
print_mib_variables([_|MibVars]) ->
    print_mib_variables(MibVars).

print_mib_variables(_Mod, [] = _Vars) ->
    ok;
print_mib_variables(Mod, Vars) ->
    io:format("~n** ~s ** ~n~n", [make_pretty_mib(Mod)]),
    print_mib_variables2(Mod, Vars), 
    io:format("~n", []).

print_mib_variables2(Mod, Variables) ->
    Vars = [{Var, (catch Mod:Var(get))} || Var <- Variables],
    snmpa_mib_lib:print_variables(Vars).


make_pretty_mib(snmp_view_based_acm_mib) ->
    "SNMP-VIEW-BASED-ACM-MIB";
make_pretty_mib(snmp_target_mib) ->
    "SNMP-TARGET-MIB";
make_pretty_mib(snmp_community_mib) ->
    "SNMP-COMMUNITY-MIB";
make_pretty_mib(snmp_notification_mib) ->
    "SNMP-NOTIFICATION-MIB";
make_pretty_mib(snmp_user_based_sm_mib) ->
    "SNMP-USER-BASED-SM-MIB";
make_pretty_mib(snmp_framework_mib) ->
    "SNMP-FRAMEWORK-MIB";
make_pretty_mib(Mod) ->
    atom_to_list(Mod).


%% -

-spec mib_of(Oid) -> {ok, MibName} | {error, Reason} when
      Oid     :: snmp:oid(),
      MibName :: atom(),
      Reason  :: term().

mib_of(Oid) ->
    snmpa_agent:mib_of(Oid).

-spec mib_of(Agent, Oid) -> {ok, MibName} | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Oid       :: snmp:oid(),
      MibName   :: atom(),
      Reason    :: term().

mib_of(Agent, Oid) ->
    snmpa_agent:mib_of(Agent, Oid).


-spec me_of(Oid) -> {ok, Me} | {error, Reason} when
      Oid    :: snmp:oid(),
      Me     :: snmp:me(),
      Reason :: term().

me_of(Oid) ->
    snmpa_agent:me_of(Oid).

-spec me_of(Agent, Oid) -> {ok, Me} | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Oid       :: snmp:oid(),
      Me        :: snmp:me(),
      Reason    :: term().

me_of(Agent, Oid) ->
    snmpa_agent:me_of(Agent, Oid).


-spec invalidate_mibs_cache() -> snmp:void().

invalidate_mibs_cache() ->
    invalidate_mibs_cache(snmp_master_agent).

-spec invalidate_mibs_cache(Agent) -> snmp:void() when
      Agent     :: pid() | AgentName,
      AgentName :: atom().

invalidate_mibs_cache(Agent) ->
    snmpa_agent:invalidate_mibs_cache(Agent).


which_mibs_cache_size() ->
    which_mibs_cache_size(snmp_master_agent).

which_mibs_cache_size(Agent) ->
    snmpa_agent:which_mibs_cache_size(Agent).


-spec enable_mibs_cache() -> snmp:void().

enable_mibs_cache() ->
    enable_mibs_cache(snmp_master_agent).

-spec enable_mibs_cache(Agent) -> snmp:void() when
      Agent     :: pid() | AgentName,
      AgentName :: atom().

enable_mibs_cache(Agent) ->
    snmpa_agent:enable_mibs_cache(Agent).


-spec disable_mibs_cache() -> snmp:void().

disable_mibs_cache() ->
    disable_mibs_cache(snmp_master_agent).

-spec disable_mibs_cache(Agent) -> snmp:void() when
      Agent     :: pid() | AgentName,
      AgentName :: atom().

disable_mibs_cache(Agent) ->
    snmpa_agent:disable_mibs_cache(Agent).


-spec gc_mibs_cache() ->
          {ok, NumElementsGCed} | {error, Reason} when
      NumElementsGCed :: non_neg_integer(),
      Reason          :: term().

gc_mibs_cache() ->
    gc_mibs_cache(snmp_master_agent).

-spec gc_mibs_cache(Agent) ->
          {ok, NumElementsGCed} | {error, Reason} when
      Agent           :: pid() | AgentName,
      AgentName       :: atom(),
      NumElementsGCed :: non_neg_integer(),
      Reason          :: term();
                   (Age) ->
          {ok, NumElementsGCed} | {error, Reason} when
      Age             :: pos_integer(),
      NumElementsGCed :: non_neg_integer(),
      Reason          :: term().

gc_mibs_cache(Agent) when is_atom(Agent) orelse is_pid(Agent) ->
    snmpa_agent:gc_mibs_cache(Agent);
gc_mibs_cache(Age) ->
    gc_mibs_cache(snmp_master_agent, Age).

-spec gc_mibs_cache(Agent, Age) ->
          {ok, NumElementsGCed} | {error, Reason} when
      Agent           :: pid() | AgentName,
      AgentName       :: atom(),
      Age             :: pos_integer(),
      NumElementsGCed :: non_neg_integer(),
      Reason          :: term();
                   (Age, GcLimit) ->
          {ok, NumElementsGCed} | {error, Reason} when
      Age             :: pos_integer(),
      GcLimit         :: pos_integer() | infinity,
      NumElementsGCed :: non_neg_integer(),
      Reason          :: term().

gc_mibs_cache(Agent, Age) when is_atom(Agent) orelse is_pid(Agent) ->
    snmpa_agent:gc_mibs_cache(Agent, Age);
gc_mibs_cache(Age, GcLimit) ->
    gc_mibs_cache(snmp_master_agent, Age, GcLimit).

-spec gc_mibs_cache(Agent, Age, GcLimit) ->
          {ok, NumElementsGCed} | {error, Reason} when
      Agent           :: pid() | AgentName,
      AgentName       :: atom(),
      Age             :: pos_integer(),
      GcLimit         :: pos_integer() | infinity,
      NumElementsGCed :: non_neg_integer(),
      Reason          :: term().

gc_mibs_cache(Agent, Age, GcLimit) when is_atom(Agent) orelse is_pid(Agent) ->
    snmpa_agent:gc_mibs_cache(Agent, Age, GcLimit).


-spec enable_mibs_cache_autogc() -> snmp:void().

enable_mibs_cache_autogc() ->
    enable_mibs_cache_autogc(snmp_master_agent).

-spec enable_mibs_cache_autogc(Agent) -> snmp:void() when
      Agent     :: pid() | AgentName,
      AgentName :: atom().

enable_mibs_cache_autogc(Agent) ->
    snmpa_agent:enable_mibs_cache_autogc(Agent).


-spec disable_mibs_cache_autogc() -> snmp:void().

disable_mibs_cache_autogc() ->
    disable_mibs_cache_autogc(snmp_master_agent).

-spec disable_mibs_cache_autogc(Agent) -> snmp:void() when
      Agent     :: pid() | AgentName,
      AgentName :: atom().

disable_mibs_cache_autogc(Agent) ->
    snmpa_agent:disable_mibs_cache_autogc(Agent).


-spec update_mibs_cache_age(Age) -> ok | {error, Reason} when
      Age    :: pos_integer(),
      Reason :: term().

update_mibs_cache_age(Age) ->
    update_mibs_cache_age(snmp_master_agent, Age).

-spec update_mibs_cache_age(Agent, Age) -> ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Age       :: pos_integer(),
      Reason    :: term().

update_mibs_cache_age(Agent, Age) ->
    snmpa_agent:update_mibs_cache_age(Agent, Age).


-spec update_mibs_cache_gclimit(GcLimit) -> ok | {error, Reason} when
      GcLimit :: pos_integer(),
      Reason  :: term().

update_mibs_cache_gclimit(GcLimit) ->
    update_mibs_cache_gclimit(snmp_master_agent, GcLimit).

-spec update_mibs_cache_gclimit(Agent, GcLimit) -> ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      GcLimit   :: pos_integer(),
      Reason    :: term().

update_mibs_cache_gclimit(Agent, GcLimit) ->
    snmpa_agent:update_mibs_cache_gclimit(Agent, GcLimit).




%% - message filter / load regulation

-spec register_notification_filter(Id, Mod, Data) ->
          ok | {error, Reason} when
      Id     :: nfilter_id(),
      Mod    :: module(),
      Data   :: term(),
      Reason :: term().

register_notification_filter(Id, Mod, Data) when is_atom(Mod) ->
    register_notification_filter(snmp_master_agent, Id, Mod, Data, last).
 
-spec register_notification_filter(Agent, Id, Mod, Data) ->
          ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Id        :: nfilter_id(),
      Mod       :: module(),
      Data      :: term(),
      Reason    :: term();
                                  (Id, Mod, Data, Where) ->
          ok | {error, Reason} when
      Id     :: nfilter_id(),
      Mod    :: module(),
      Data   :: term(),
      Where  :: nfilter_position(),
      Reason :: term().

register_notification_filter(Agent, Id, Mod, Data)
  when (is_pid(Agent) orelse is_atom(Agent)) andalso is_atom(Mod) ->
    register_notification_filter(Agent, Id, Mod, Data, last);
register_notification_filter(Id, Mod, Data, Where) when is_atom(Mod) ->
    register_notification_filter(snmp_master_agent, Id, Mod, Data, Where).
 
-spec register_notification_filter(Agent, Id, Mod, Data, Where) ->
          ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Id        :: nfilter_id(),
      Mod       :: module(),
      Data      :: term(),
      Where     :: nfilter_position(),
      Reason    :: term().

register_notification_filter(Agent, Id, Mod, Data, Where) ->
    snmpa_agent:register_notification_filter(Agent, Id, Mod, Data, Where).


-spec unregister_notification_filter(Id) ->
          ok | {error, Reason} when
      Id     :: nfilter_id(),
      Reason :: term().

unregister_notification_filter(Id) ->
    unregister_notification_filter(snmp_master_agent, Id).

-spec unregister_notification_filter(Agent, Id) ->
          ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Id        :: nfilter_id(),
      Reason    :: term().

unregister_notification_filter(Agent, Id) ->
    snmpa_agent:unregister_notification_filter(Agent, Id).

 
which_notification_filter() ->
    which_notification_filter(snmp_master_agent).
 
which_notification_filter(Agent) ->
    snmpa_agent:which_notification_filter(Agent).
 

get_request_limit() -> 
    get_request_limit(snmp_master_agent).
get_request_limit(Agent) -> 
    snmpa_agent:get_request_limit(Agent).


-spec set_request_limit(NewLimit) ->
          {ok, OldLimit} | {error, Reason} when
      NewLimit :: infinity | non_neg_integer(),
      OldLimit :: infinity | non_neg_integer(),
      Reason   :: term().

set_request_limit(NewLimit) -> 
    set_request_limit(snmp_master_agent, NewLimit).

-spec set_request_limit(Agent, NewLimit) ->
          {ok, OldLimit} | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      NewLimit  :: infinity | non_neg_integer(),
      OldLimit  :: infinity | non_neg_integer(),
      Reason    :: term().

set_request_limit(Agent, NewLimit) -> 
    snmpa_agent:set_request_limit(Agent, NewLimit).


%% -

-spec send_notification2(Agent, Notification, SendOpts) -> snmp:void() when
      Agent        :: pid() | AgentName,
      AgentName    :: atom(),
      Notification :: atom(),
      SendOpts     :: [SendOpt],
      SendOpt      :: {receiver,        Receiver}                       | 
                      {name,     snmp_notification_mib:notify_name()}   |
                      {context,  snmp_community_mib:context_name()}     |
                      {varbinds,        [Varbind]}                      |
                      {local_engine_id, snmp_framework_mib:engine_id()} |
                      {extra,           term()},
      Receiver     :: no_receiver |
                      {Tag, Recv} |
                      notification_delivery_info(),
      Tag          :: term(),
      Recv         :: pid() | atom() | MFA,
      MFA          :: {Mod, Func, Args},
      Mod          :: module(),
      Func         :: atom(),
      Args         :: list(),
      Varbind      :: {Variable, Value} |
                      {Column, RowIndex, Value} |
                      {Oid, Value},
      Variable     :: atom(),
      Column       :: atom(),
      RowIndex     :: snmp:row_index(),
      Oid          :: snmp:oid(),
      Value        :: term().
      
send_notification2(Agent, Notification, SendOpts) ->
    snmpa_agent:send_notification(Agent, Notification, SendOpts).


-spec send_notification(Agent, Notification, Receiver) -> snmp:void() when
      Agent        :: pid() | AgentName,
      AgentName    :: atom(),
      Notification :: atom(),
      Receiver     :: no_receiver |
                      {Tag, Recv} |
                      notification_delivery_info(),
      Tag          :: term(),
      Recv         :: pid() | atom() | MFA,
      MFA          :: {Mod, Func, Args},
      Mod          :: module(),
      Func         :: atom(),
      Args         :: list().

send_notification(Agent, Notification, Receiver) ->
    SendOpts = 
	[
	 {receiver, Receiver},
	 {varbinds, []}, 
	 {name,     ""},
	 {context,  ""}, 
	 {extra,    ?DEFAULT_NOTIF_EXTRA_INFO}
	], 
    send_notification2(Agent, Notification, SendOpts).

-spec send_notification(Agent, Notification, Receiver,
                        Varbinds) -> snmp:void() when
      Agent        :: pid() | AgentName,
      AgentName    :: atom(),
      Notification :: atom(),
      Receiver     :: no_receiver |
                      {Tag, Recv} |
                      notification_delivery_info(),
      Tag          :: term(),
      Recv         :: pid() | atom() | MFA,
      MFA          :: {Mod, Func, Args},
      Mod          :: module(),
      Func         :: atom(),
      Args         :: list(),
      Varbinds     :: [Varbind],
      Varbind      :: {Variable, Value} |
                      {Column, RowIndex, Value} |
                      {Oid, Value},
      Variable     :: atom(),
      Column       :: atom(),
      RowIndex     :: snmp:row_index(),
      Oid          :: snmp:oid(),
      Value        :: term().

send_notification(Agent, Notification, Receiver, Varbinds) ->
    SendOpts = 
	[
	 {receiver, Receiver},
	 {varbinds, Varbinds}, 
	 {name,     ""},
	 {context,  ""}, 
	 {extra,    ?DEFAULT_NOTIF_EXTRA_INFO}
	], 
    send_notification2(Agent, Notification, SendOpts).

-spec send_notification(Agent, Notification, Receiver, NotifyName, Varbinds) ->
          snmp:void() when
      Agent        :: pid() | AgentName,
      AgentName    :: atom(),
      Notification :: atom(),
      Receiver     :: no_receiver |
                      {Tag, Recv} |
                      notification_delivery_info(),
      Tag          :: term(),
      Recv         :: pid() | atom() | MFA,
      MFA          :: {Mod, Func, Args},
      Mod          :: module(),
      Func         :: atom(),
      Args         :: list(),
      NotifyName   :: snmp_notification_mib:notify_name(),
      Varbinds     :: [Varbind],
      Varbind      :: {Variable, Value} |
                      {Column, RowIndex, Value} |
                      {Oid, Value},
      Variable     :: atom(),
      Column       :: atom(),
      RowIndex     :: snmp:row_index(),
      Oid          :: snmp:oid(),
      Value        :: term().

send_notification(Agent, Notification, Receiver, NotifyName, Varbinds) ->
    SendOpts = 
	[
	 {receiver, Receiver},
	 {varbinds, Varbinds}, 
	 {name,     NotifyName},
	 {context,  ""}, 
	 {extra,    ?DEFAULT_NOTIF_EXTRA_INFO}
	], 
    send_notification2(Agent, Notification, SendOpts).

-spec send_notification(Agent, Notification, Receiver,
                        NotifyName, ContextName, Varbinds) ->
          snmp:void() when
      Agent        :: pid() | AgentName,
      AgentName    :: atom(),
      Notification :: atom(),
      Receiver     :: no_receiver |
                      {Tag, Recv} |
                      notification_delivery_info(),
      Tag          :: term(),
      Recv         :: pid() | atom() | MFA,
      MFA          :: {Mod, Func, Args},
      Mod          :: module(),
      Func         :: atom(),
      Args         :: list(),
      NotifyName   :: snmp_notification_mib:notify_name(),
      ContextName  :: snmp_community_mib:context_name(),
      Varbinds     :: [Varbind],
      Varbind      :: {Variable, Value} |
                      {Column, RowIndex, Value} |
                      {Oid, Value},
      Variable     :: atom(),
      Column       :: atom(),
      RowIndex     :: snmp:row_index(),
      Oid          :: snmp:oid(),
      Value        :: term().

send_notification(Agent, Notification, Receiver, NotifyName, 
		  ContextName, Varbinds) 
  when (is_list(NotifyName)  andalso 
	is_list(ContextName) andalso 
	is_list(Varbinds)) ->
    SendOpts = 
	[
	 {receiver, Receiver},
	 {varbinds, Varbinds}, 
	 {name,     NotifyName},
	 {context,  ContextName}, 
	 {extra,    ?DEFAULT_NOTIF_EXTRA_INFO}
	], 
    send_notification2(Agent, Notification, SendOpts).

-spec send_notification(Agent, Notification, Receiver,
                        NotifyName, ContextName, Varbinds,
                        LocalEngineID) ->
          snmp:void() when
      Agent         :: pid() | AgentName,
      AgentName     :: atom(),
      Notification  :: atom(),
      Receiver      :: no_receiver |
                       {Tag, Recv} |
                       notification_delivery_info(),
      Tag           :: term(),
      Recv          :: pid() | atom() | MFA,
      MFA           :: {Mod, Func, Args},
      Mod           :: module(),
      Func          :: atom(),
      Args          :: list(),
      NotifyName    :: snmp_notification_mib:notify_name(),
      ContextName   :: snmp_community_mib:context_name(),
      Varbinds      :: [Varbind],
      Varbind       :: {Variable, Value} |
                       {Column, RowIndex, Value} |
                       {Oid, Value},
      Variable      :: atom(),
      Column        :: atom(),
      RowIndex      :: snmp:row_index(),
      Oid           :: snmp:oid(),
      Value         :: term(),
      LocalEngineID :: snmp_framework_mib:engine_id().

send_notification(Agent, Notification, Receiver, 
		  NotifyName, ContextName, Varbinds, LocalEngineID) 
  when (is_list(NotifyName)  andalso 
	is_list(ContextName) andalso 
	is_list(Varbinds) andalso 
	is_list(LocalEngineID)) ->
    SendOpts = 
	[
	 {receiver,        Receiver},
	 {varbinds,        Varbinds}, 
	 {name,            NotifyName},
	 {context,         ContextName}, 
	 {extra,           ?DEFAULT_NOTIF_EXTRA_INFO}, 
	 {local_engine_id, LocalEngineID}
	],
    send_notification2(Agent, Notification, SendOpts).

%% Kept for backwards compatibility
send_trap(Agent, Trap, Community) ->
    send_notification(Agent, Trap, no_receiver, Community, "", []).

send_trap(Agent, Trap, Community, Varbinds) ->
    send_notification(Agent, Trap, no_receiver, Community, "", Varbinds).


%%%-----------------------------------------------------------------

-spec discovery(TargetName, Notification) ->
          {ok, ManagerEngineID} | {error, Reason} when
      TargetName      :: string(),
      Notification    :: atom(),
      ManagerEngineID :: snmp_framework_mib:engine_id(),
      Reason          :: term().

discovery(TargetName, Notification) ->
    Varbinds = [],
    discovery(TargetName, Notification, Varbinds).

-spec discovery(TargetName, Notification, Varbinds) ->
          {ok, ManagerEngineID} | {error, Reason} when
      TargetName      :: string(),
      Notification    :: atom(),
      Varbinds        :: [Varbind],
      Varbind         :: {Variable, Value} |
                         {Column, RowIndex, Value} |
                         {OID, Value},
      Variable        :: atom(),
      Column          :: atom(),
      RowIndex        :: snmp:row_index(),
      OID             :: snmp:oid(),
      Value           :: term(),
      ManagerEngineID :: snmp_framework_mib:engine_id(),
      Reason          :: term();
               (TargetName, Notification, ContextName) ->
          {ok, ManagerEngineID} | {error, Reason} when
          TargetName      :: string(),
          Notification    :: atom(),
          ContextName     :: snmp_community_mib:context_name(),
          ManagerEngineID :: snmp_framework_mib:engine_id(),
          Reason          :: term().

discovery(TargetName, Notification, Varbinds) when is_list(Varbinds) ->
    ContextName = "",
    discovery(TargetName, Notification, ContextName, Varbinds);
discovery(TargetName, Notification, DiscoHandler) 
  when is_atom(DiscoHandler) ->
    Varbinds = [],
    discovery(TargetName, Notification, Varbinds, DiscoHandler).

-spec discovery(TargetName, Notification, ContextName, Varbinds) ->
          {ok, ManagerEngineID} | {error, Reason} when
      TargetName      :: string(),
      Notification    :: atom(),
      ContextName     :: snmp_community_mib:context_name(),
      Varbinds        :: [Varbind],
      Varbind         :: {Variable, Value} |
                         {Column, RowIndex, Value} |
                         {OID, Value},
      Variable        :: atom(),
      Column          :: atom(),
      RowIndex        :: snmp:row_index(),
      OID             :: snmp:oid(),
      Value           :: term(),
      ManagerEngineID :: snmp_framework_mib:engine_id(),
      Reason          :: term();
               (TargetName, Notification, Varbinds, DiscoHandler) ->
          {ok, ManagerEngineID} | {error, Reason} when
          TargetName      :: string(),
          Notification    :: atom(),
          Varbinds        :: [Varbind],
          Varbind         :: {Variable, Value} |
                             {Column, RowIndex, Value} |
                             {OID, Value},
          Variable        :: atom(),
          Column          :: atom(),
          RowIndex        :: snmp:row_index(),
          OID             :: snmp:oid(),
          Value           :: term(),
          DiscoHandler    :: discovery_handler(),
          ManagerEngineID :: snmp_framework_mib:engine_id(),
          Reason          :: term().

discovery(TargetName, Notification, ContextName, Varbinds) 
  when is_list(Varbinds) ->
    DiscoHandler = snmpa_discovery_handler_default, 
    discovery(TargetName, Notification, ContextName, Varbinds, 
	      DiscoHandler);
discovery(TargetName, Notification, Varbinds, DiscoHandler) 
  when is_atom(DiscoHandler) ->
    ContextName = "",
    discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler).

-spec discovery(TargetName, Notification,
                ContextName, Varbinds, DiscoHandler) ->
          {ok, ManagerEngineID} | {error, Reason} when
      TargetName      :: string(),
      Notification    :: atom(),
      ContextName     :: snmp_community_mib:context_name(),
      Varbinds        :: [Varbind],
      Varbind         :: {Variable, Value} |
                         {Column, RowIndex, Value} |
                         {OID, Value},
      Variable        :: atom(),
      Column          :: atom(),
      RowIndex        :: snmp:row_index(),
      OID             :: snmp:oid(),
      Value           :: term(),
      DiscoHandler    :: discovery_handler(),
      ManagerEngineID :: snmp_framework_mib:engine_id(),
      Reason          :: term().

discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler) ->
    ExtraInfo = ?DISCO_EXTRA_INFO,
    discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler, 
	      ExtraInfo).

-spec discovery(TargetName, Notification,
                ContextName, Varbinds, DiscoHandler, 
                ExtraInfo) ->
          {ok, ManagerEngineID} | {error, Reason} when
      TargetName      :: string(),
      Notification    :: atom(),
      ContextName     :: snmp_community_mib:context_name(),
      Varbinds        :: [Varbind],
      Varbind         :: {Variable, Value} |
                         {Column, RowIndex, Value} |
                         {OID, Value},
      Variable        :: atom(),
      Column          :: atom(),
      RowIndex        :: snmp:row_index(),
      OID             :: snmp:oid(),
      Value           :: term(),
      DiscoHandler    :: discovery_handler(),
      ExtraInfo       :: term(),
      ManagerEngineID :: snmp_framework_mib:engine_id(),
      Reason          :: term().

discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler, 
	  ExtraInfo) 
  when (is_list(TargetName) andalso (length(TargetName) > 0) andalso 
	is_atom(Notification) andalso 
	is_list(ContextName) andalso 
	is_list(Varbinds) andalso 
	is_atom(DiscoHandler)) ->
    case (catch snmpa_discovery_handler:verify(DiscoHandler)) of
	ok ->
	    snmpa_agent:discovery(TargetName, Notification, ContextName, 
				  Varbinds, DiscoHandler, ExtraInfo);
	Error ->
	    Error
    end.


%%%-----------------------------------------------------------------

-spec register_subagent(Agent, SubTree, SubAgent) -> ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      SubTree   :: snmp:oid(),
      SubAgent  :: pid(),
      Reason    :: term().

register_subagent(Agent, SubTree, SubAgent) ->
    snmpa_agent:register_subagent(Agent, SubTree, SubAgent).


-spec unregister_subagent(Agent, SubAgentOidOrPid) -> ok | {error, Reason} when
      Agent            :: pid() | AgentName,
      AgentName        :: atom(),
      SubAgentOidOrPid :: snmp:oid() | pid(),
      Reason           :: term().

unregister_subagent(Agent, SubOidOrPid) ->
    snmpa_agent:unregister_subagent(Agent, SubOidOrPid).


system_start_time() ->
    [{_, Time}] = ets:lookup(snmp_agent_table, system_start_time),
    Time.

sys_up_time() ->
    % time in 0.01 seconds.
    StartTime = system_start_time(),
    (snmp_misc:now(cs) - StartTime) rem (2 bsl 31).


%%%-----------------------------------------------------------------

which_transports() ->
    {value, Transports} = snmp_framework_mib:intAgentTransports(get),
    [case Kind of
         all ->
             {Domain, Address};
         _ ->
             {Domain, Address, Kind}
     end || {Domain, Address, Kind, _} <- Transports].


%%%-----------------------------------------------------------------

-spec restart_worker() -> snmp:void().

restart_worker() ->
    restart_worker(snmp_master_agent).

-spec restart_worker(Agent) -> snmp:void() when
      Agent     :: pid | AgentName,
      AgentName :: atom().

restart_worker(Agent) ->
    snmpa_agent:restart_worker(Agent).


-spec restart_set_worker() -> snmp:void().

restart_set_worker() ->
    restart_set_worker(snmp_master_agent).

-spec restart_set_worker(Agent) -> snmp:void() when
      Agent     :: pid | AgentName,
      AgentName :: atom().

restart_set_worker(Agent) ->
    snmpa_agent:restart_set_worker(Agent).


%%%-----------------------------------------------------------------
%%% USM functions
%%%-----------------------------------------------------------------
passwd2localized_key(Alg, Passwd, EngineID) ->
    snmp_usm:passwd2localized_key(Alg, Passwd, EngineID).

localize_key(Alg, Key, EngineID) ->
    snmp_usm:localize_key(Alg, Key, EngineID).


%%%-----------------------------------------------------------------
%%% Agent Capabilities functions
%%%-----------------------------------------------------------------

-spec add_agent_caps(Oid, Descr) -> Index when
      Oid   :: snmp:oid(),
      Descr :: string(),
      Index :: integer().

add_agent_caps(Oid, Descr) ->
    snmp_standard_mib:add_agent_caps(Oid, Descr).

-spec del_agent_caps(Index) -> snmp:void() when
      Index :: integer().

del_agent_caps(Index) ->
    snmp_standard_mib:del_agent_caps(Index).

-spec get_agent_caps() -> Caps when
      Caps        :: [[Cap]],
      Cap         :: SysORIndex | SysORID | SysORDescr | SysORUpTime,
      SysORIndex  :: integer(),
      SysORID     :: snmp:oid(),
      SysORDescr  :: string(),
      SysORUpTime :: integer().

get_agent_caps() ->
    snmp_standard_mib:get_agent_caps().


%%%-----------------------------------------------------------------
%%% Audit Trail Log functions 
%%%-----------------------------------------------------------------

-spec log_to_txt(LogDir) -> snmp:void() when
      LogDir :: snmp:dir().

log_to_txt(LogDir) ->
    log_to_txt(LogDir, []).


-spec log_to_txt(LogDir, Block) -> snmp:void() when
      LogDir :: snmp:dir(),
      Block  :: boolean();
                (LogDir, Mibs) -> snmp:void() when
      LogDir :: snmp:dir(),
      Mibs   :: [snmp:mib_name()].

log_to_txt(LogDir, Block)
  when is_boolean(Block) ->
    Mibs    = [],
    OutFile = "snmpa_log.txt", 
    LogName = ?audit_trail_log_name,
    LogFile = ?audit_trail_log_file,
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs) ->
    Block   = ?ATL_BLOCK_DEFAULT,
    OutFile = "snmpa_log.txt", 
    LogName = ?audit_trail_log_name,
    LogFile = ?audit_trail_log_file,
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).


-spec log_to_txt(LogDir, Mibs, Block) -> snmp:void() when
      LogDir :: snmp:dir(),
      Mibs   :: [snmp:mib_name()],
      Block  :: boolean();
                (LogDir, Mibs, OutFile) -> snmp:void() when
      LogDir  :: snmp:dir(),
      Mibs    :: [snmp:mib_name()],
      OutFile :: file:filename().

log_to_txt(LogDir, Mibs, Block)
  when ((Block =:= true) orelse (Block =:= false)) ->
    OutFile = "snmpa_log.txt",
    LogName = ?audit_trail_log_name,
    LogFile = ?audit_trail_log_file,
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile) ->
    Block   = ?ATL_BLOCK_DEFAULT,
    LogName = ?audit_trail_log_name,
    LogFile = ?audit_trail_log_file,
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).


-spec log_to_txt(LogDir, Mibs, OutFile, Block) -> snmp:void() when
      LogDir  :: snmp:dir(),
      Mibs    :: [snmp:mib_name()],
      OutFile :: file:filename(),
      Block   :: boolean();
                (LogDir, Mibs, OutFile, LogName) -> snmp:void() when
      LogDir  :: snmp:dir(),
      Mibs    :: [snmp:mib_name()],
      OutFile :: file:filename(),
      LogName :: string().

log_to_txt(LogDir, Mibs, OutFile, Block)
  when is_boolean(Block) ->
    LogName = ?audit_trail_log_name,
    LogFile = ?audit_trail_log_file,
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile, LogName) ->
    Block   = ?ATL_BLOCK_DEFAULT,
    LogFile = ?audit_trail_log_file,
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).


-spec log_to_txt(LogDir, Mibs, OutFile, LogName, Block) -> snmp:void() when
      LogDir  :: snmp:dir(),
      Mibs    :: [snmp:mib_name()],
      OutFile :: file:filename(),
      LogName :: string(),
      Block   :: boolean();
                (LogDir, Mibs, OutFile, LogName, LogFile) -> snmp:void() when
      LogDir  :: snmp:dir(),
      Mibs    :: [snmp:mib_name()],
      OutFile :: file:filename(),
      LogName :: string(),
      LogFile :: string().

log_to_txt(LogDir, Mibs, OutFile, LogName, Block)
  when is_boolean(Block) ->
    LogFile = ?audit_trail_log_file,
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile) ->
    Block = ?ATL_BLOCK_DEFAULT,
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).


-spec log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block) ->
          snmp:void() when
      LogDir  :: snmp:dir(),
      Mibs    :: [snmp:mib_name()],
      OutFile :: file:filename(),
      LogName :: string(),
      LogFile :: string(),
      Block   :: boolean();
                (LogDir, Mibs, OutFile, LogName, LogFile, Start) ->
          snmp:void() when
      LogDir  :: snmp:dir(),
      Mibs    :: [snmp:mib_name()],
      OutFile :: file:filename(),
      LogName :: string(),
      LogFile :: string(),
      Start   :: null | snmp:log_time().

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start).


-spec log_to_txt(LogDir, Mibs,
                 OutFile, LogName, LogFile,
		 Block,
		 Start) -> snmp:void() when
      LogDir  :: snmp:dir(),
      Mibs    :: [snmp:mib_name()],
      OutFile :: file:filename(),
      LogName :: string(),
      LogFile :: string(),
      Block   :: boolean(),
      Start   :: null | snmp:log_time();
                (LogDir, Mibs,
		 OutFile, LogName, LogFile,
		 Start, Stop) -> snmp:void() when
      LogDir  :: snmp:dir(),
      Mibs    :: [snmp:mib_name()],
      OutFile :: file:filename(),
      LogName :: string(),
      LogFile :: string(),
      Start   :: null | snmp:log_time(),
      Stop    :: null | snmp:log_time().

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_txt(LogDir, Mibs,
                    OutFile, LogName, LogFile,
                    Block,
                    Start);
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start, Stop) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_txt(LogDir, Mibs,
                    OutFile, LogName, LogFile,
                    Block,
                    Start, Stop).

-spec log_to_txt(LogDir, Mibs,
		 OutFile, LogName, LogFile,
		 Block,
		 Start, Stop) -> snmp:void() when
      LogDir  :: snmp:dir(),
      Mibs    :: [snmp:mib_name()],
      OutFile :: file:filename(),
      LogName :: string(),
      LogFile :: string(),
      Block   :: boolean(),
      Start   :: null | snmp:log_time(),
      Stop    :: null | snmp:log_time().

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start, Stop) -> 
    snmp:log_to_txt(LogDir, Mibs,
                    OutFile, LogName, LogFile,
                    Block,
                    Start, Stop).


-spec log_to_io(LogDir) -> ok | {ok, Cnt} | {error, Reason} when
      LogDir :: string(),
      Cnt    :: {NumOK, NumERR},
      NumOK  :: non_neg_integer(),
      NumERR :: pos_integer(),
      Reason :: term().

log_to_io(LogDir) -> 
    log_to_io(LogDir, []).

-spec log_to_io(LogDir, Block) -> ok | {ok, Cnt} | {error, Reason} when
      LogDir :: string(),
      Block  :: boolean(),
      Cnt    :: {NumOK, NumERR},
      NumOK  :: non_neg_integer(),
      NumERR :: pos_integer(),
      Reason :: term();
               (LogDir, Mibs) -> ok | {ok, Cnt} | {error, Reason} when
      LogDir  :: string(),
      Mibs    :: [snmp:mib_name()],
      Cnt     :: {NumOK, NumERR},
      NumOK   :: non_neg_integer(),
      NumERR  :: pos_integer(),
      Reason  :: term().

log_to_io(LogDir, Block) 
  when is_boolean(Block) -> 
    Mibs    = [], 
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs) -> 
    Block   = ?ATL_BLOCK_DEFAULT, 
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block).

-spec log_to_io(LogDir, Mibs, Block) -> ok | {ok, Cnt} | {error, Reason} when
      LogDir  :: string(),
      Mibs    :: [snmp:mib_name()],
      Block   :: boolean(),
      Cnt     :: {NumOK, NumERR},
      NumOK   :: non_neg_integer(),
      NumERR  :: pos_integer(),
      Reason  :: term();
               (LogDir, Mibs, LogName) -> ok | {ok, Cnt} | {error, Reason} when
      LogDir  :: string(),
      Mibs    :: [snmp:mib_name()],
      LogName :: string(),
      Cnt     :: {NumOK, NumERR},
      NumOK   :: non_neg_integer(),
      NumERR  :: pos_integer(),
      Reason  :: term().

log_to_io(LogDir, Mibs, Block) 
  when is_boolean(Block) -> 
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName) -> 
    Block   = ?ATL_BLOCK_DEFAULT, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block).

-spec log_to_io(LogDir, Mibs, LogName, Block) ->
          ok | {ok, Cnt} | {error, Reason} when
      LogDir  :: string(),
      Mibs    :: [snmp:mib_name()],
      LogName :: string(),
      Block   :: boolean(),
      Cnt     :: {NumOK, NumERR},
      NumOK   :: non_neg_integer(),
      NumERR  :: pos_integer(),
      Reason  :: term();
               (LogDir, Mibs, LogName, LogFile) ->
          ok | {ok, Cnt} | {error, Reason} when
      LogDir  :: string(),
      Mibs    :: [snmp:mib_name()],
      LogName :: string(),
      LogFile :: string(),
      Cnt     :: {NumOK, NumERR},
      NumOK   :: non_neg_integer(),
      NumERR  :: pos_integer(),
      Reason  :: term().

log_to_io(LogDir, Mibs, LogName, Block) 
  when is_boolean(Block) -> 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName, LogFile) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block).

-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block) ->
          ok | {ok, Cnt} | {error, Reason} when
      LogDir  :: string(),
      Mibs    :: [snmp:mib_name()],
      LogName :: string(),
      LogFile :: string(),
      Block   :: boolean(),
      Cnt     :: {NumOK, NumERR},
      NumOK   :: non_neg_integer(),
      NumERR  :: pos_integer(),
      Reason  :: term();
               (LogDir, Mibs, LogName, LogFile, Start) ->
          ok | {ok, Cnt} | {error, Reason} when
      LogDir  :: string(),
      Mibs    :: [snmp:mib_name()],
      LogName :: string(),
      LogFile :: string(),
      Start   :: null | snmp:log_time(),
      Cnt     :: {NumOK, NumERR},
      NumOK   :: non_neg_integer(),
      NumERR  :: pos_integer(),
      Reason  :: term().

log_to_io(LogDir, Mibs, LogName, LogFile, Block)
  when is_boolean(Block) ->
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName, LogFile, Start) ->
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start).

-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start) ->
          ok | {ok, Cnt} | {error, Reason} when
      LogDir  :: string(),
      Mibs    :: [snmp:mib_name()],
      LogName :: string(),
      LogFile :: string(),
      Block   :: boolean(),
      Start   :: null | snmp:log_time(),
      Cnt     :: {NumOK, NumERR},
      NumOK   :: non_neg_integer(),
      NumERR  :: pos_integer(),
      Reason  :: term();
               (LogDir, Mibs, LogName, LogFile, Start, Stop) ->
          ok | {ok, Cnt} | {error, Reason} when
      LogDir  :: string(),
      Mibs    :: [snmp:mib_name()],
      LogName :: string(),
      LogFile :: string(),
      Start   :: null | snmp:log_time(),
      Stop    :: null | snmp:log_time(),
      Cnt     :: {NumOK, NumERR},
      NumOK   :: non_neg_integer(),
      NumERR  :: pos_integer(),
      Reason  :: term().

log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start)
  when is_boolean(Block) ->
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start);
log_to_io(LogDir, Mibs, LogName, LogFile, Start, Stop) ->
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop).

-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop) ->
          ok | {ok, Cnt} | {error, Reason} when
      LogDir  :: string(),
      Mibs    :: [snmp:mib_name()],
      LogName :: string(),
      LogFile :: string(),
      Block   :: boolean(),
      Start   :: null | snmp:log_time(),
      Stop    :: null | snmp:log_time(),
      Cnt     :: {NumOK, NumERR},
      NumOK   :: non_neg_integer(),
      NumERR  :: pos_integer(),
      Reason  :: term().

log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop)
  when is_boolean(Block) ->
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop).


log_info() ->
    LogName = ?audit_trail_log_name, 
    snmp_log:info(LogName).


-spec change_log_size(NewSize) -> ok | {error, Reason} when
      NewSize :: snmp:log_size(),
      Reason  :: term().

change_log_size(NewSize) -> 
    LogName = ?audit_trail_log_name, % The old (agent) default
    snmp:change_log_size(LogName, NewSize).


get_log_type() ->
    get_log_type(snmp_master_agent).

get_log_type(Agent) ->
    snmpa_agent:get_log_type(Agent).

%% NewType -> atl_type()
change_log_type(NewType) ->
    set_log_type(NewType).

change_log_type(Agent, NewType) ->
    set_log_type(Agent, NewType).


-spec set_log_type(NewType) -> {ok, OldType} | {error, Reason} when
      NewType :: snmp:atl_type(),
      OldType :: snmp:atl_type(),
      Reason  :: term().

set_log_type(NewType) ->
    set_log_type(snmp_master_agent, NewType).

-spec set_log_type(Agent, NewType) -> {ok, OldType} | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      NewType   :: snmp:atl_type(),
      OldType   :: snmp:atl_type(),
      Reason    :: term().

set_log_type(Agent, NewType) ->
    snmpa_agent:set_log_type(Agent, NewType).
