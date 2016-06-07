%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

	 info/0, info/1, old_info_format/1, 
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

	      %% Agent config types
	      mib_storage/0, 
	      mib_storage_opt/0, 
	      mib_storage_module/0, 
	      mib_storage_options/0
             ]).

-deprecated([{old_info_format, 1, next_major_release}]).


-include("snmpa_atl.hrl").
-include("snmpa_internal.hrl").
-include_lib("snmp/include/snmp_types.hrl"). % type of me needed. 

-define(DISCO_EXTRA_INFO,  undefined).
-define(ATL_BLOCK_DEFAULT, true).


%%-----------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------

-type me() :: #me{}.

%% Agent config types
-type mib_storage() :: [mib_storage_opt()].
-type mib_storage_opt() :: {module,  mib_storage_module()} | 
                           {options, mib_storage_options()}.

%% Module implementing the snmpa_mib_storage behaviour
-type mib_storage_module()  :: atom(). 
%% Options specific to the above module
-type mib_storage_options() :: list().


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

convert_config(Opts) ->
    snmpa_app:convert_config(Opts).


%%-----------------------------------------------------------------
%% Note that verbosity for the agents is actually only implemented 
%% (properly) for the master agent.
%%-----------------------------------------------------------------

verbosity(all,Verbosity) -> 
    catch snmpa_agent:verbosity(sub_agents,Verbosity),
    catch snmpa_agent:verbosity(master_agent,Verbosity),
    catch snmpa_agent:verbosity(net_if,Verbosity),
    catch snmpa_agent:verbosity(mib_server,Verbosity),
    catch snmpa_agent:verbosity(note_store,Verbosity),
    catch snmpa_symbolic_store:verbosity(Verbosity),
    catch snmpa_local_db:verbosity(Verbosity);
verbosity(master_agent,Verbosity) -> 
    catch snmpa_agent:verbosity(master_agent,Verbosity);
verbosity(net_if,Verbosity) -> 
    catch snmpa_agent:verbosity(net_if,Verbosity);
verbosity(note_store,Verbosity) -> 
    catch snmpa_agent:verbosity(note_store, Verbosity);
verbosity(mib_server,Verbosity) -> 
    catch snmpa_agent:verbosity(mib_server,Verbosity);
verbosity(symbolic_store,Verbosity) -> 
    catch snmpa_symbolic_store:verbosity(Verbosity);
verbosity(local_db,Verbosity) -> 
    catch snmpa_local_db:verbosity(Verbosity);
verbosity(Agent,{subagents,Verbosity}) -> 
    catch snmpa_agent:verbosity(Agent,{sub_agents,Verbosity});
verbosity(Agent,Verbosity) -> 
    catch snmpa_agent:verbosity(Agent,Verbosity).


%%-----------------------------------------------------------------
%% 
%% Some symbolic store (internal database) utility functions
%% 
%%-----------------------------------------------------------------

get_symbolic_store_db() ->
    snmpa_symbolic_store:get_db().


which_aliasnames() ->
    snmpa_symbolic_store:which_aliasnames().

which_tables() ->
    snmpa_symbolic_store:which_tables().

which_variables() ->
    snmpa_symbolic_store:which_variables().

which_notifications() ->
    snmpa_symbolic_store:which_notifications().


%%-----------------------------------------------------------------
%% These 8 functions returns {value, Val} | false
%%-----------------------------------------------------------------
name_to_oid(Name) ->
    snmpa_symbolic_store:aliasname_to_oid(Name).

name_to_oid(Db, Name) ->
    snmpa_symbolic_store:aliasname_to_oid(Db, Name).

oid_to_name(OID) ->
    snmpa_symbolic_store:oid_to_aliasname(OID).

oid_to_name(Db, OID) ->
    snmpa_symbolic_store:oid_to_aliasname(Db, OID).

enum_to_int(Name, Enum) ->
    snmpa_symbolic_store:enum_to_int(Name, Enum).

enum_to_int(Db, Name, Enum) ->
    snmpa_symbolic_store:enum_to_int(Db, Name, Enum).

int_to_enum(Name, Int) ->
    snmpa_symbolic_store:int_to_enum(Name, Int).

int_to_enum(Db, Name, Int) ->
    snmpa_symbolic_store:int_to_enum(Db, Name, Int).


%%-----------------------------------------------------------------
%% These functions must only be called in the process context
%% where the instrumentation functions are called!
%%-----------------------------------------------------------------
current_request_id()  -> current_get(snmp_request_id).
current_context()     -> current_get(snmp_context).
current_community()   -> current_get(snmp_community).
current_address()     -> current_get(snmp_address).
current_net_if_data() -> current_get(net_if_data).

current_get(Tag) ->
    case get(Tag) of
	undefined -> false;
	X -> {value, X}
    end.
    

%% -

get(Agent, Vars) -> snmpa_agent:get(Agent, Vars).
get(Agent, Vars, Context) -> snmpa_agent:get(Agent, Vars, Context).

get_next(Agent, Vars) -> snmpa_agent:get_next(Agent, Vars).
get_next(Agent, Vars, Context) -> snmpa_agent:get_next(Agent, Vars, Context).


info()      -> info(snmp_master_agent).
info(Agent) -> snmpa_agent:info(Agent).

old_info_format(Info) when is_list(Info) ->
    {value, Vsns}         = lists:keysearch(vsns,            1, Info),
    {value, {_, MibInfo}} = lists:keysearch(mib_server,      1, Info),
    {value, SAa}          = lists:keysearch(subagents,       1, MibInfo),
    {value, LoadedMibs}   = lists:keysearch(loaded_mibs,     1, MibInfo),
    {value, TreeSz}       = lists:keysearch(tree_size_bytes, 1, MibInfo),
    {value, ProcMem}      = lists:keysearch(process_memory,  1, MibInfo),
    {value, DbMem}        = lists:keysearch(db_memory,       1, MibInfo),
    [Vsns, SAa, LoadedMibs, TreeSz, ProcMem, DbMem].
    

%% -

backup(BackupDir) ->
    backup(snmp_master_agent, BackupDir).

backup(Agent, BackupDir) ->
    snmpa_agent:backup(Agent, BackupDir).


%% -

dump_mibs()     -> snmpa_agent:dump_mibs(snmp_master_agent).
dump_mibs(File) -> snmpa_agent:dump_mibs(snmp_master_agent, File).


load_mib(Mib) ->
    load_mib(snmp_master_agent, Mib).

-spec load_mib(Agent :: pid() | atom(), Mib :: string()) ->
    ok | {error, Reason :: already_loaded | term()}.

load_mib(Agent, Mib) ->
    case load_mibs(Agent, [Mib]) of
	{error, {'load aborted at', Mib, Reason}} ->
	    {error, Reason};
	Else ->
	    Else
    end.

load_mibs(Mibs) ->
    load_mibs(snmp_master_agent, Mibs, false).
load_mibs(Agent, Mibs) when is_list(Mibs) -> 
    snmpa_agent:load_mibs(Agent, Mibs, false);
load_mibs(Mibs, Force) 
  when is_list(Mibs) andalso ((Force =:= true) orelse (Force =:= false)) ->
    load_mibs(snmp_master_agent, Mibs, Force).

-spec load_mibs(Agent :: pid() | atom(), 
		Mibs  :: [MibName :: string()], 
		Force :: boolean()) ->
    ok | {error, {'load aborted at', MibName :: string(), InternalReason :: already_loaded | term()}}.

load_mibs(Agent, Mibs, Force) 
  when is_list(Mibs) andalso ((Force =:= true) orelse (Force =:= false)) -> 
    snmpa_agent:load_mibs(Agent, Mibs, Force).


unload_mib(Mib) ->
    unload_mib(snmp_master_agent, Mib).

-spec unload_mib(Agent :: pid() | atom(), Mib :: string()) ->
    ok | {error, Reason :: not_loaded | term()}.

unload_mib(Agent, Mib) ->
    case unload_mibs(Agent, [Mib]) of
	{error, {'unload aborted at', Mib, Reason}} ->
	    {error, Reason};
	Else ->
	    Else
    end.

unload_mibs(Mibs) ->
    unload_mibs(snmp_master_agent, Mibs, false).
unload_mibs(Agent, Mibs) when is_list(Mibs) -> 
    snmpa_agent:unload_mibs(Agent, Mibs);
unload_mibs(Mibs, Force) 
  when is_list(Mibs) andalso ((Force =:= true) orelse (Force =:= false)) ->
    unload_mibs(snmp_master_agent, Mibs, Force).

-spec unload_mibs(Agent :: pid() | atom(), 
		  Mibs  :: [MibName :: string()], 
		  Force :: boolean()) ->
    ok | {error, {'unload aborted at', MibName :: string(), InternalReason :: not_loaded | term()}}.

unload_mibs(Agent, Mibs, Force) 
  when is_list(Mibs) andalso ((Force =:= true) orelse (Force =:= false)) ->
    snmpa_agent:unload_mibs(Agent, Mibs, Force).


which_mibs()      -> which_mibs(snmp_master_agent).
which_mibs(Agent) -> snmpa_agent:which_mibs(Agent).


whereis_mib(Mib) ->
    whereis_mib(snmp_master_agent, Mib).
whereis_mib(Agent, Mib) when is_atom(Mib) ->
    snmpa_agent:whereis_mib(Agent, Mib).


%% -

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

mib_of(Oid) ->
    snmpa_agent:mib_of(Oid).

mib_of(Agent, Oid) ->
    snmpa_agent:mib_of(Agent, Oid).

me_of(Oid) ->
    snmpa_agent:me_of(Oid).

me_of(Agent, Oid) ->
    snmpa_agent:me_of(Agent, Oid).


invalidate_mibs_cache() ->
    invalidate_mibs_cache(snmp_master_agent).

invalidate_mibs_cache(Agent) ->
    snmpa_agent:invalidate_mibs_cache(Agent).


which_mibs_cache_size() ->
    which_mibs_cache_size(snmp_master_agent).

which_mibs_cache_size(Agent) ->
    snmpa_agent:which_mibs_cache_size(Agent).


enable_mibs_cache() ->
    enable_mibs_cache(snmp_master_agent).

enable_mibs_cache(Agent) ->
    snmpa_agent:enable_mibs_cache(Agent).


disable_mibs_cache() ->
    disable_mibs_cache(snmp_master_agent).

disable_mibs_cache(Agent) ->
    snmpa_agent:disable_mibs_cache(Agent).


gc_mibs_cache() ->
    gc_mibs_cache(snmp_master_agent).

gc_mibs_cache(Agent) when is_atom(Agent) orelse is_pid(Agent) ->
    snmpa_agent:gc_mibs_cache(Agent);
gc_mibs_cache(Age) ->
    gc_mibs_cache(snmp_master_agent, Age).

gc_mibs_cache(Agent, Age) when is_atom(Agent) orelse is_pid(Agent) ->
    snmpa_agent:gc_mibs_cache(Agent, Age);
gc_mibs_cache(Age, GcLimit) ->
    gc_mibs_cache(snmp_master_agent, Age, GcLimit).

gc_mibs_cache(Agent, Age, GcLimit) when is_atom(Agent) orelse is_pid(Agent) ->
    snmpa_agent:gc_mibs_cache(Agent, Age, GcLimit).


enable_mibs_cache_autogc() ->
    enable_mibs_cache_autogc(snmp_master_agent).

enable_mibs_cache_autogc(Agent) ->
    snmpa_agent:enable_mibs_cache_autogc(Agent).


disable_mibs_cache_autogc() ->
    disable_mibs_cache_autogc(snmp_master_agent).

disable_mibs_cache_autogc(Agent) ->
    snmpa_agent:disable_mibs_cache_autogc(Agent).


update_mibs_cache_age(Age) ->
    update_mibs_cache_age(snmp_master_agent, Age).

update_mibs_cache_age(Agent, Age) ->
    snmpa_agent:update_mibs_cache_age(Agent, Age).


update_mibs_cache_gclimit(GcLimit) ->
    update_mibs_cache_gclimit(snmp_master_agent, GcLimit).

update_mibs_cache_gclimit(Agent, GcLimit) ->
    snmpa_agent:update_mibs_cache_gclimit(Agent, GcLimit).




%% - message filter / load regulation

register_notification_filter(Id, Mod, Data) when is_atom(Mod) ->
    register_notification_filter(snmp_master_agent, Id, Mod, Data, last).
 
register_notification_filter(Agent, Id, Mod, Data)
  when is_atom(Agent) andalso is_atom(Mod) ->
    register_notification_filter(Agent, Id, Mod, Data, last);
register_notification_filter(Agent, Id, Mod, Data)
  when is_pid(Agent) andalso is_atom(Mod) ->
    register_notification_filter(Agent, Id, Mod, Data, last);
register_notification_filter(Id, Mod, Data, Where) when is_atom(Mod) ->
    register_notification_filter(snmp_master_agent, Id, Mod, Data, Where).
 
register_notification_filter(Agent, Id, Mod, Data, Where) ->
    snmpa_agent:register_notification_filter(Agent, Id, Mod, Data, Where).
 
unregister_notification_filter(Id) ->
    unregister_notification_filter(snmp_master_agent, Id).
 
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

set_request_limit(NewLimit) -> 
    set_request_limit(snmp_master_agent, NewLimit).
set_request_limit(Agent, NewLimit) -> 
    snmpa_agent:set_request_limit(Agent, NewLimit).


%% -

send_notification2(Agent, Notification, SendOpts) ->
    snmpa_agent:send_notification(Agent, Notification, SendOpts).

send_notification(Agent, Notification, Recv) ->
    SendOpts = 
	[
	 {receiver, Recv},
	 {varbinds, []}, 
	 {name,     ""},
	 {context,  ""}, 
	 {extra,    ?DEFAULT_NOTIF_EXTRA_INFO}
	], 
    send_notification2(Agent, Notification, SendOpts).

send_notification(Agent, Notification, Recv, Varbinds) ->
    SendOpts = 
	[
	 {receiver, Recv},
	 {varbinds, Varbinds}, 
	 {name,     ""},
	 {context,  ""}, 
	 {extra,    ?DEFAULT_NOTIF_EXTRA_INFO}
	], 
    send_notification2(Agent, Notification, SendOpts).

send_notification(Agent, Notification, Recv, NotifyName, Varbinds) ->
    SendOpts = 
	[
	 {receiver, Recv},
	 {varbinds, Varbinds}, 
	 {name,     NotifyName},
	 {context,  ""}, 
	 {extra,    ?DEFAULT_NOTIF_EXTRA_INFO}
	], 
    send_notification2(Agent, Notification, SendOpts).

send_notification(Agent, Notification, Recv, NotifyName, 
		  ContextName, Varbinds) 
  when (is_list(NotifyName)  andalso 
	is_list(ContextName) andalso 
	is_list(Varbinds)) ->
    SendOpts = 
	[
	 {receiver, Recv},
	 {varbinds, Varbinds}, 
	 {name,     NotifyName},
	 {context,  ContextName}, 
	 {extra,    ?DEFAULT_NOTIF_EXTRA_INFO}
	], 
    send_notification2(Agent, Notification, SendOpts).

send_notification(Agent, Notification, Recv, 
		  NotifyName, ContextName, Varbinds, LocalEngineID) 
  when (is_list(NotifyName)  andalso 
	is_list(ContextName) andalso 
	is_list(Varbinds) andalso 
	is_list(LocalEngineID)) ->
    SendOpts = 
	[
	 {receiver,        Recv},
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

discovery(TargetName, Notification) ->
    Varbinds = [],
    discovery(TargetName, Notification, Varbinds).

discovery(TargetName, Notification, Varbinds) when is_list(Varbinds) ->
    ContextName = "",
    discovery(TargetName, Notification, ContextName, Varbinds);
discovery(TargetName, Notification, DiscoHandler) 
  when is_atom(DiscoHandler) ->
    Varbinds = [],
    discovery(TargetName, Notification, Varbinds, DiscoHandler).

discovery(TargetName, Notification, ContextName, Varbinds) 
  when is_list(Varbinds) ->
    DiscoHandler = snmpa_discovery_handler_default, 
    discovery(TargetName, Notification, ContextName, Varbinds, 
	      DiscoHandler);
discovery(TargetName, Notification, Varbinds, DiscoHandler) 
  when is_atom(DiscoHandler) ->
    ContextName = "",
    discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler).

discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler) ->
    ExtraInfo = ?DISCO_EXTRA_INFO,
    discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler, 
	      ExtraInfo).

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

register_subagent(Agent, SubTree, SubAgent) ->
    snmpa_agent:register_subagent(Agent, SubTree, SubAgent).

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

restart_worker() ->
    restart_worker(snmp_master_agent).

restart_worker(Agent) ->
    snmpa_agent:restart_worker(Agent).


restart_set_worker() ->
    restart_set_worker(snmp_master_agent).

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
add_agent_caps(Oid, Descr) ->
    snmp_standard_mib:add_agent_caps(Oid, Descr).

del_agent_caps(Index) ->
    snmp_standard_mib:del_agent_caps(Index).

get_agent_caps() ->
    snmp_standard_mib:get_agent_caps().


%%%-----------------------------------------------------------------
%%% Audit Trail Log functions 
%%%-----------------------------------------------------------------

-spec log_to_txt(LogDir :: snmp:dir()) ->
    snmp:void().

log_to_txt(LogDir) -> 
    log_to_txt(LogDir, []).

-spec log_to_txt(LogDir :: snmp:dir(), 
		 Block  :: boolean()) ->
    snmp:void();
                (LogDir :: snmp:dir(), 
		 Mibs   :: [snmp:mib_name()]) ->
    snmp:void().

log_to_txt(LogDir, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
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

-spec log_to_txt(LogDir :: snmp:dir(), 
		 Mibs   :: [snmp:mib_name()], 
		 Block  :: boolean()) ->
    snmp:void();
                (LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename()) ->
    snmp:void().

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

-spec log_to_txt(LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 Block   :: boolean()) ->
    snmp:void();
                (LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string()) ->
    snmp:void().

log_to_txt(LogDir, Mibs, OutFile, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile, LogName) -> 
    Block   = ?ATL_BLOCK_DEFAULT, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).

-spec log_to_txt(LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 Block   :: boolean()) ->
    snmp:void();
                (LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string()) ->
    snmp:void().

log_to_txt(LogDir, Mibs, OutFile, LogName, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).

-spec log_to_txt(LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string(), 
		 Block   :: boolean()) ->
    snmp:void();
                (LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string(), 
		 Start   :: snmp_log:log_time()) ->
    snmp:void().

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start).

-spec log_to_txt(LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string(), 
		 Block   :: boolean(), 
		 Start   :: snmp_log:log_time()) ->
    snmp:void();
                (LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string(), 
		 Start   :: snmp_log:log_time(), 
		 Stop    :: snmp_log:log_time()) ->
    snmp:void().

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start);

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start, Stop) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start, Stop).

-spec log_to_txt(LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string(), 
		 Block   :: boolean(), 
		 Start   :: snmp_log:log_time(), 
		 Stop    :: snmp_log:log_time()) ->
    snmp:void().

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start, Stop) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start, Stop).


log_to_io(LogDir) -> 
    log_to_io(LogDir, []).

log_to_io(LogDir, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    Mibs    = [], 
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs) -> 
    Block   = ?ATL_BLOCK_DEFAULT, 
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block).

log_to_io(LogDir, Mibs, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName) -> 
    Block   = ?ATL_BLOCK_DEFAULT, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block).

log_to_io(LogDir, Mibs, LogName, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName, LogFile) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block).

log_to_io(LogDir, Mibs, LogName, LogFile, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName, LogFile, Start) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start).

log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start);
log_to_io(LogDir, Mibs, LogName, LogFile, Start, Stop) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop).

log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop) -> 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop).


log_info() ->
    LogName = ?audit_trail_log_name, 
    snmp_log:info(LogName).


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

set_log_type(NewType) ->
    set_log_type(snmp_master_agent, NewType).

set_log_type(Agent, NewType) ->
    snmpa_agent:set_log_type(Agent, NewType).
