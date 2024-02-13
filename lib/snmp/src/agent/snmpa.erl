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
-moduledoc """
Interface Functions to the SNMP toolkit agent

The module `snmpa` contains interface functions to the SNMP agent.

[](){: #data_types }

## DATA TYPES

```erlang
oid() = [byte()]
atl_type() = read | write | read_write
notification_delivery_info() = #snmpa_notification_delivery_info{}
```

The `oid()` type is used to represent an ASN.1 OBJECT IDENTIFIER.

The record `snmpa_notification_delivery_info` contains the following fields:

- **`tag = term()`** - A user defined identity representing this notification
  send operation.

- **`mod = module()`** - A module implementing the
  `m:snmpa_notification_delivery_info_receiver` behaviour. The info functions of
  this module will be called at various stages of delivery.

- **`extra = term()`** - This is any extra info the user wants to have supplied
  when the functions in the callback module is called.

[](){: #add_agent_caps }

## See Also

calendar(3), erlc(1)
""".


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

	      %% Agent config types
	      mib_storage/0, 
	      mib_storage_opt/0, 
	      mib_storage_module/0, 
	      mib_storage_options/0
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

%% Agent config types
-type mib_storage() :: [mib_storage_opt()].
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


%%-----------------------------------------------------------------

-doc false.
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

-doc """
convert_config(OldConfig) -> AgentConfig

This off-line utility function can be used to convert the old snmp application
config (pre snmp-4.0) to the new snmp agent config (as of snmp-4.0).

For information about the old config (`OldConfig`) see the OTP R9C
documentation.

For information about the current agent config (`AgentConfig`), see the
[Configuring the application](snmp_config.md#configuration_params) chapter of
the SNMP user's guide.

[](){: #restart_worker }
""".
convert_config(Opts) ->
    snmpa_app:convert_config(Opts).


%%-----------------------------------------------------------------
%% Note that verbosity for the agents is actually only implemented 
%% (properly) for the master agent.
%%-----------------------------------------------------------------

-doc """
verbosity(Ref,Verbosity) -> void()

Sets verbosity for the designated process. For the lowest verbosity `silence`,
nothing is printed. The higher the verbosity, the more is printed.
""".
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

-doc false.
get_symbolic_store_db() ->
    snmpa_symbolic_store:get_db().


-doc """
which_aliasnames() -> Result

Retrieve all alias-names known to the agent.

[](){: #which_tables }
""".
which_aliasnames() ->
    snmpa_symbolic_store:which_aliasnames().

-doc """
which_tables() -> Result

Retrieve all tables known to the agent.

[](){: #which_transports }
""".
which_tables() ->
    snmpa_symbolic_store:which_tables().

-doc """
which_variables() -> Result

Retrieve all variables known to the agent.

[](){: #which_notifications }
""".
which_variables() ->
    snmpa_symbolic_store:which_variables().

-doc """
which_notifications() -> Result

Retrieve all notifications (and traps) known to the agent.

[](){: #log_to_txt }
""".
which_notifications() ->
    snmpa_symbolic_store:which_notifications().


%%-----------------------------------------------------------------
%% These 8 functions returns {value, Val} | false
%%-----------------------------------------------------------------
-doc(#{equiv => name_to_oid/2}).
name_to_oid(Name) ->
    snmpa_symbolic_store:aliasname_to_oid(Name).

-doc """
name_to_oid(Db, Name) -> {value, oid()} | false

Looks up the OBJECT IDENTIFIER of a MIB object, given the symbolic name. Note,
the OBJECT IDENTIFIER is given for the object, not for an instance.

`false` is returned if the object is not defined in any loaded MIB.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).

[](){: #oid_to_name }
""".
name_to_oid(Db, Name) ->
    snmpa_symbolic_store:aliasname_to_oid(Db, Name).

-doc(#{equiv => oid_to_name/2}).
oid_to_name(OID) ->
    snmpa_symbolic_store:oid_to_aliasname(OID).

-doc """
oid_to_name(Db, OID) -> {value, Name} | false

Looks up the symbolic name of a MIB object, given OBJECT IDENTIFIER.

`false` is returned if the object is not defined in any loaded MIB.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).

[](){: #which_aliasnames }
""".
oid_to_name(Db, OID) ->
    snmpa_symbolic_store:oid_to_aliasname(Db, OID).

-doc(#{equiv => enum_to_int/3}).
enum_to_int(Name, Enum) ->
    snmpa_symbolic_store:enum_to_int(Name, Enum).

-doc """
enum_to_int(Db, Name, Enum) -> {value, Int} | false

Converts the symbolic value `Enum` to the corresponding integer of the
enumerated object or type `Name` in a MIB. The MIB must be loaded.

`false` is returned if the object or type is not defined in any loaded MIB, or
if it does not define the symbolic value as enumerated.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).

[](){: #int_to_enum }
""".
enum_to_int(Db, Name, Enum) ->
    snmpa_symbolic_store:enum_to_int(Db, Name, Enum).

-doc(#{equiv => int_to_enum/3}).
int_to_enum(Name, Int) ->
    snmpa_symbolic_store:int_to_enum(Name, Int).

-doc """
int_to_enum(Db, Name, Int) -> {value, Enum} | false

Converts the integer `Int` to the corresponding symbolic value of the enumerated
object or type `Name` in a MIB. The MIB must be loaded.

`false` is returned if the object or type is not defined in any loaded MIB, or
if it does not define the symbolic value as enumerated.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).

[](){: #name_to_oid }
""".
int_to_enum(Db, Name, Int) ->
    snmpa_symbolic_store:int_to_enum(Db, Name, Int).


%%-----------------------------------------------------------------
%% These functions must only be called in the process context
%% where the instrumentation functions are called!
%%-----------------------------------------------------------------
-doc """
current_request_id() -> {value, RequestId} | false

Get the request-id, context, community and address of the request currently
being processed by the agent.

Note that these functions is intended to be called by the instrumentation
functions and _only_ if they are executed in the context of the agent process
(e.g. it does not work if called from a spawned process).

[](){: #enum_to_int }
""".
current_request_id()  -> current_get(snmp_request_id).
-doc(#{equiv => current_request_id/0}).
current_context()     -> current_get(snmp_context).
-doc(#{equiv => current_request_id/0}).
current_community()   -> current_get(snmp_community).
-doc(#{equiv => current_request_id/0}).
current_address()     -> current_get(snmp_address).
-doc false.
current_net_if_data() -> current_get(net_if_data).

current_get(Tag) ->
    case get(Tag) of
	undefined -> false;
	X -> {value, X}
    end.
    

%% -

-doc(#{equiv => get/3}).
get(Agent, Vars) -> snmpa_agent:get(Agent, Vars).
-doc """
get(Agent, Vars, Context) -> Values | {error, Reason}

Performs a GET operation on the agent. All loaded MIB objects are visible in
this operation. The agent calls the corresponding instrumentation functions just
as if it was a GET request coming from a manager.

Note that the request specific parameters (such as
[current_request_id](`m:snmpa#current_request_id`)) are not accessible for the
instrumentation functions if this function is used.

[](){: #get_next }
""".
get(Agent, Vars, Context) -> snmpa_agent:get(Agent, Vars, Context).

-doc(#{equiv => get_next/3}).
get_next(Agent, Vars) -> snmpa_agent:get_next(Agent, Vars).
-doc """
get_next(Agent, Vars, Context) -> Values | {error, Reason}

Performs a GET-NEXT operation on the agent. All loaded MIB objects are visible
in this operation. The agent calls the corresponding instrumentation functions
just as if it was a GET request coming from a manager.

Note that the request specific parameters (such as `snmpa:current_request_id/0`
are not accessible for the instrumentation functions if this function is used.

[](){: #backup }
""".
get_next(Agent, Vars, Context) -> snmpa_agent:get_next(Agent, Vars, Context).


-doc(#{equiv => info/1}).
info()      -> info(snmp_master_agent).
-doc """
info(Agent) -> [{Key, Value}]

Returns a list (a dictionary) containing information about the agent.
Information includes loaded MIBs, registered sub-agents, some information about
the memory allocation.

[](){: #load_mib }
""".
info(Agent) -> snmpa_agent:info(Agent).


%% -

-doc(#{equiv => backup/2}).
backup(BackupDir) ->
    backup(snmp_master_agent, BackupDir).

-doc """
backup(Agent, BackupDir) -> ok | {error, Reason}

Backup persistent/permanent data handled by the agent (such as local-db,
mib-data and vacm).

Data stored by mnesia is not handled.

BackupDir cannot be identical to DbDir.

Simultaneous backup calls are _not_ allowed. That is, two different processes
cannot simultaneously successfully call this function. One of them will be
first, and succeed. The second will fail with the error reason
`backup_in_progress`.

[](){: #info }
""".
backup(Agent, BackupDir) ->
    snmpa_agent:backup(Agent, BackupDir).


%% -

-doc false.
dump_mibs()     -> snmpa_agent:dump_mibs(snmp_master_agent).
-doc false.
dump_mibs(File) -> snmpa_agent:dump_mibs(snmp_master_agent, File).


-doc(#{equiv => load_mib/2}).
-doc(#{since => <<"OTP R16B02">>}).
load_mib(Mib) ->
    load_mib(snmp_master_agent, Mib).

-doc """
load_mib(Agent, Mib) -> ok | {error, Reason}

Load a single `Mib` into an agent. The `MibName` is the name of the Mib,
including the path to where the compiled mib is found. For example:

```erlang
          Dir = code:priv_dir(my_app) ++ "/mibs/",
          snmpa:load_mib(snmp_master_agent, Dir ++ "MY-MIB").
```

[](){: #load_mibs }
""".
-doc(#{since => <<"OTP R16B02">>}).
-spec load_mib(Agent :: pid() | atom(), Mib :: string()) ->
    ok | {error, Reason :: already_loaded | term()}.

load_mib(Agent, Mib) ->
    case load_mibs(Agent, [Mib]) of
	{error, {'load aborted at', Mib, Reason}} ->
	    {error, Reason};
	Else ->
	    Else
    end.

-doc(#{equiv => load_mibs/3}).
-doc(#{since => <<"OTP R16B02">>}).
load_mibs(Mibs) ->
    load_mibs(snmp_master_agent, Mibs, false).
-doc(#{equiv => load_mibs/3}).
-doc(#{since => <<"OTP R16B02">>}).
load_mibs(Agent, Mibs) when is_list(Mibs) -> 
    snmpa_agent:load_mibs(Agent, Mibs, false);
load_mibs(Mibs, Force) 
  when is_list(Mibs) andalso ((Force =:= true) orelse (Force =:= false)) ->
    load_mibs(snmp_master_agent, Mibs, Force).

-doc """
load_mibs(Agent, Mibs, Force) -> ok | {error, Reason}

Load `Mibs` into an agent. If the agent cannot load all MIBs (the default value
of the `Force` argument is `false`), it will indicate where loading was aborted.
The `MibName` is the name of the Mib, including the path to where the compiled
mib is found. For example,

```erlang
          Dir = code:priv_dir(my_app) ++ "/mibs/",
          snmpa:load_mibs(snmp_master_agent, [Dir ++ "MY-MIB"]).
```

If `Force = true` then the agent will continue attempting to load each mib even
after failing to load a previous mib. Use with care.

[](){: #unload_mib }
""".
-doc(#{since => <<"OTP R16B02">>}).
-spec load_mibs(Agent :: pid() | atom(), 
		Mibs  :: [MibName :: string()], 
		Force :: boolean()) ->
    ok | {error, {'load aborted at', MibName :: string(), InternalReason :: already_loaded | term()}}.

load_mibs(Agent, Mibs, Force) 
  when is_list(Mibs) andalso ((Force =:= true) orelse (Force =:= false)) -> 
    snmpa_agent:load_mibs(Agent, Mibs, Force).


-doc(#{equiv => unload_mib/2}).
-doc(#{since => <<"OTP R16B02">>}).
unload_mib(Mib) ->
    unload_mib(snmp_master_agent, Mib).

-doc """
unload_mib(Agent, Mib) -> ok | {error, Reason}

Unload a single `Mib` from an agent.

[](){: #unload_mibs }
""".
-doc(#{since => <<"OTP R16B02">>}).
-spec unload_mib(Agent :: pid() | atom(), Mib :: string()) ->
    ok | {error, Reason :: not_loaded | term()}.

unload_mib(Agent, Mib) ->
    case unload_mibs(Agent, [Mib]) of
	{error, {'unload aborted at', Mib, Reason}} ->
	    {error, Reason};
	Else ->
	    Else
    end.

-doc(#{equiv => unload_mibs/3}).
-doc(#{since => <<"OTP R16B02">>}).
unload_mibs(Mibs) ->
    unload_mibs(snmp_master_agent, Mibs).
-doc(#{equiv => unload_mibs/3}).
-doc(#{since => <<"OTP R16B02">>}).
unload_mibs(Agent, Mibs) when is_list(Mibs) -> 
    unload_mibs(Agent, Mibs, false);
unload_mibs(Mibs, Force) 
  when is_list(Mibs) andalso is_boolean(Force) ->
    unload_mibs(snmp_master_agent, Mibs, Force).

-doc """
unload_mibs(Agent, Mibs, Force) -> ok | {error, Reason}

Unload `Mibs` from an agent. If it cannot unload all MIBs (the default value of
the `Force` argument is `false`), it will indicate where unloading was aborted.

If `Force = true` then the agent will continue attempting to unload each mib
even after failing to unload a previous mib. Use with care.

[](){: #which_mibs }
""".
-doc(#{since => <<"OTP R16B02">>}).
-spec unload_mibs(Agent :: pid() | atom(), 
		  Mibs  :: [MibName :: string()], 
		  Force :: boolean()) ->
    ok | {error, {'unload aborted at', MibName :: string(), InternalReason :: not_loaded | term()}}.

unload_mibs(Agent, Mibs, Force) 
  when is_list(Mibs) andalso is_boolean(Force) ->
    snmpa_agent:unload_mibs(Agent, Mibs, Force).


-doc(#{equiv => which_mibs/1}).
which_mibs()      -> which_mibs(snmp_master_agent).
-doc """
which_mibs(Agent) -> Mibs

Retrieve the list of all the mibs loaded into this agent. Default is the master
agent.

[](){: #whereis_mib }
""".
which_mibs(Agent) -> snmpa_agent:which_mibs(Agent).


-doc(#{equiv => whereis_mib/2}).
whereis_mib(Mib) ->
    whereis_mib(snmp_master_agent, Mib).
-doc """
whereis_mib(Agent, MibName) -> {ok, MibFile} | {error, Reason}

Get the full path to the (compiled) mib-file.

[](){: #current_request_id } [](){: #current_context } [](){: #current_community
} [](){: #current_address }
""".
whereis_mib(Agent, Mib) when is_atom(Mib) ->
    snmpa_agent:whereis_mib(Agent, Mib).


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

-doc """
print_mib_info() -> void()

Prints the content of all the (snmp) tables and variables for all mibs handled
by the snmp agent.

[](){: #print_mib_tables }
""".
-doc(#{since => <<"OTP R14B02">>}).
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


-doc """
print_mib_tables() -> void()

Prints the content of all the (snmp) tables for all mibs handled by the snmp
agent.

[](){: #print_mib_variables }
""".
-doc(#{since => <<"OTP R14B02">>}).
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


-doc """
print_mib_variables() -> void()

Prints the content of all the (snmp) variables for all mibs handled by the snmp
agent.

[](){: #verbosity }
""".
-doc(#{since => <<"OTP R14B02">>}).
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

-doc(#{equiv => mib_of/2}).
mib_of(Oid) ->
    snmpa_agent:mib_of(Oid).

-doc """
mib_of(Agent, Oid) -> {ok, MibName} | {error, Reason}

Finds the mib corresponding to the `Oid`. If it is a variable, the Oid must be
`<Oid for var>.0` and if it is a table, Oid must be `<table>.<entry>.<col>.<any>`

[](){: #me_of }
""".
mib_of(Agent, Oid) ->
    snmpa_agent:mib_of(Agent, Oid).

-doc(#{equiv => me_of/2}).
me_of(Oid) ->
    snmpa_agent:me_of(Oid).

-doc """
me_of(Agent, Oid) -> {ok, Me} | {error, Reason}

Finds the mib entry corresponding to the `Oid`. If it is a variable, the Oid
must be <Oid for var>.0 and if it is a table, Oid must be
`<table>.<entry>.<col>.<any>`

[](){: #invalidate_mibs_cache }
""".
me_of(Agent, Oid) ->
    snmpa_agent:me_of(Agent, Oid).


-doc(#{equiv => invalidate_mibs_cache/1}).
invalidate_mibs_cache() ->
    invalidate_mibs_cache(snmp_master_agent).

-doc """
invalidate_mibs_cache(Agent) -> void()

Invalidate the mib server cache.

The entire contents of the cache will be deleted.

[](){: #enable_mibs_cache }
""".
invalidate_mibs_cache(Agent) ->
    snmpa_agent:invalidate_mibs_cache(Agent).


-doc(#{equiv => which_mibs_cache_size/1}).
-doc(#{since => <<"OTP R14B">>}).
which_mibs_cache_size() ->
    which_mibs_cache_size(snmp_master_agent).

-doc """
which_mibs_cache_size(Agent) -> void()

Retrieve the size of the mib server cache.

[](){: #gc_mibs_cache }
""".
-doc(#{since => <<"OTP R14B">>}).
which_mibs_cache_size(Agent) ->
    snmpa_agent:which_mibs_cache_size(Agent).


-doc(#{equiv => enable_mibs_cache/1}).
enable_mibs_cache() ->
    enable_mibs_cache(snmp_master_agent).

-doc """
enable_mibs_cache(Agent) -> void()

Enable the mib server cache.

[](){: #disable_mibs_cache }
""".
enable_mibs_cache(Agent) ->
    snmpa_agent:enable_mibs_cache(Agent).


-doc(#{equiv => disable_mibs_cache/1}).
disable_mibs_cache() ->
    disable_mibs_cache(snmp_master_agent).

-doc """
disable_mibs_cache(Agent) -> void()

Disable the mib server cache.

[](){: #which_mibs_cache_size }
""".
disable_mibs_cache(Agent) ->
    snmpa_agent:disable_mibs_cache(Agent).


-doc(#{equiv => gc_mibs_cache/3}).
gc_mibs_cache() ->
    gc_mibs_cache(snmp_master_agent).

-doc(#{equiv => gc_mibs_cache/3}).
gc_mibs_cache(Agent) when is_atom(Agent) orelse is_pid(Agent) ->
    snmpa_agent:gc_mibs_cache(Agent);
gc_mibs_cache(Age) ->
    gc_mibs_cache(snmp_master_agent, Age).

-doc(#{equiv => gc_mibs_cache/3}).
gc_mibs_cache(Agent, Age) when is_atom(Agent) orelse is_pid(Agent) ->
    snmpa_agent:gc_mibs_cache(Agent, Age);
gc_mibs_cache(Age, GcLimit) ->
    gc_mibs_cache(snmp_master_agent, Age, GcLimit).

-doc """
gc_mibs_cache(Agent, Age, GcLimit) -> {ok, NumElementsGCed} | {error, Reason}

Perform mib server cache gc.

Manually performs a mib server cache gc. This can be done regardless of the
value of the `autogc` option. The `NumElementsGCed` value indicates how many
elements where actually removed from the cache.

[](){: #enable_mibs_cache_autogc }
""".
gc_mibs_cache(Agent, Age, GcLimit) when is_atom(Agent) orelse is_pid(Agent) ->
    snmpa_agent:gc_mibs_cache(Agent, Age, GcLimit).


-doc(#{equiv => enable_mibs_cache_autogc/1}).
enable_mibs_cache_autogc() ->
    enable_mibs_cache_autogc(snmp_master_agent).

-doc """
enable_mibs_cache_autogc(Agent) -> void()

Enable automatic gc of the mib server cache.

[](){: #disable_mibs_cache_autogc }
""".
enable_mibs_cache_autogc(Agent) ->
    snmpa_agent:enable_mibs_cache_autogc(Agent).


-doc(#{equiv => disable_mibs_cache_autogc/1}).
disable_mibs_cache_autogc() ->
    disable_mibs_cache_autogc(snmp_master_agent).

-doc """
disable_mibs_cache_autogc(Agent) -> void()

Disable automatic gc of the mib server cache.

[](){: #update_mibs_cache_age }
""".
disable_mibs_cache_autogc(Agent) ->
    snmpa_agent:disable_mibs_cache_autogc(Agent).


-doc(#{equiv => update_mibs_cache_age/2}).
update_mibs_cache_age(Age) ->
    update_mibs_cache_age(snmp_master_agent, Age).

-doc """
update_mibs_cache_age(Agent, NewAge) -> ok | {error, Reason}

Change the mib server cache `age` property.

[](){: #update_mibs_cache_gclimit }
""".
update_mibs_cache_age(Agent, Age) ->
    snmpa_agent:update_mibs_cache_age(Agent, Age).


-doc(#{equiv => update_mibs_cache_gclimit/2}).
update_mibs_cache_gclimit(GcLimit) ->
    update_mibs_cache_gclimit(snmp_master_agent, GcLimit).

-doc """
update_mibs_cache_gclimit(Agent, NewGCLimit) -> ok | {error, Reason}

Change the mib server cache `gclimit` property.

[](){: #register_notification_filter }
""".
update_mibs_cache_gclimit(Agent, GcLimit) ->
    snmpa_agent:update_mibs_cache_gclimit(Agent, GcLimit).




%% - message filter / load regulation

-doc(#{equiv => register_notification_filter/5}).
register_notification_filter(Id, Mod, Data) when is_atom(Mod) ->
    register_notification_filter(snmp_master_agent, Id, Mod, Data, last).
 
-doc(#{equiv => register_notification_filter/5}).
register_notification_filter(Agent, Id, Mod, Data)
  when is_atom(Agent) andalso is_atom(Mod) ->
    register_notification_filter(Agent, Id, Mod, Data, last);
register_notification_filter(Agent, Id, Mod, Data)
  when is_pid(Agent) andalso is_atom(Mod) ->
    register_notification_filter(Agent, Id, Mod, Data, last);
register_notification_filter(Id, Mod, Data, Where) when is_atom(Mod) ->
    register_notification_filter(snmp_master_agent, Id, Mod, Data, Where).
 
-doc """
register_notification_filter(Agent, Id, Mod, Data, Where) -> ok | {error,
Reason}

Registers a notification filter.

`Mod` is a module implementing the `snmpa_notification_filter` behaviour.

`Data` will be passed on to the filter when calling the functions of the
behaviour.

[](){: #unregister_notification_filter }
""".
register_notification_filter(Agent, Id, Mod, Data, Where) ->
    snmpa_agent:register_notification_filter(Agent, Id, Mod, Data, Where).
 
-doc(#{equiv => unregister_notification_filter/2}).
unregister_notification_filter(Id) ->
    unregister_notification_filter(snmp_master_agent, Id).
 
-doc """
unregister_notification_filter(Agent, Id) -> ok | {error, Reason}

Unregister a notification filter.

[](){: #which_notification_filter }
""".
unregister_notification_filter(Agent, Id) ->
    snmpa_agent:unregister_notification_filter(Agent, Id).
 
-doc(#{equiv => which_notification_filter/1}).
which_notification_filter() ->
    which_notification_filter(snmp_master_agent).
 
-doc """
which_notification_filter(Agent) -> Filters

List all notification filters in an agent.

[](){: #set_request_limit }
""".
which_notification_filter(Agent) ->
    snmpa_agent:which_notification_filter(Agent).
 

-doc false.
get_request_limit() -> 
    get_request_limit(snmp_master_agent).
-doc false.
get_request_limit(Agent) -> 
    snmpa_agent:get_request_limit(Agent).

-doc(#{equiv => set_request_limit/2}).
set_request_limit(NewLimit) -> 
    set_request_limit(snmp_master_agent, NewLimit).
-doc """
set_request_limit(Agent, NewLimit) -> {ok, OldLimit} | {error, Reason}

Changes the request limit.

Note that this has no effect on the application configuration as defined by
configuration files, so a node restart will revert the config to whatever is in
those files.

This function is primarily useful in load regulation scenarios.

[](){: #register_subagent }
""".
set_request_limit(Agent, NewLimit) -> 
    snmpa_agent:set_request_limit(Agent, NewLimit).


%% -

-doc """
send_notification2(Agent, Notification, SendOpts) -> void()

Send the notification `Notification` to the management targets defined for
notify-name (`name`) in the `snmpNotifyTable` in SNMP-NOTIFICATION-MIB from the
specified `context`.

If no `name` is specified (or if it is `""`), the notification is sent to all
management targets.

If no `context` is specified, the default context, `""`, is used.

The send option `receiver` specifies where information about delivery of
Inform-Requests should be sent. The agent sends Inform-Requests and waits for
acknowledgments from the management targets. The `receiver` can have three
values:

- `no_receiver` \- No information is delivered.
- `notification_delivery_info()` \- The information is delivered via a function
  call according to this data. See the [DATA TYPES](`m:snmpa#data_types`)
  section above for details.
- `{tag(), tag_receiver()}` \- The information is delivered either via messages
  or via a function call according to the value of `tag_receiver()`.

  Delivery is done differently depending on the value of `tag_receiver()`:

  - `pid() | registered_name()` \- The info will be delivered in the following
    messages:

    - `{snmp_targets, tag(), Addresses}`

      This informs the user which target addresses the notification was sent to.

    - `{snmp_notification, tag(), {got_response, Address}}`

      This informs the user that this target address acknowledged the
      notification.

    - `{snmp_notification, tag(), {no_response, Address}}`

      This informs the user that this target address did not acknowledge the
      notification.

    The notification is sent as an Inform-Request to each target address in
    `Addresses` and if there are no targets for which an Inform-Request is sent,
    `Addresses` is the empty list `[]`.

    The `tag_receiver()` will first be sent the `snmp_targets` message, and then
    for each address in `Addresses` list, one of the two `snmp_notification`
    messages.

  - `{Mod, Func, Args}` \- The info will be delivered via the function call:

    `Mod:Func([Msg | Args])`

    where `Msg` has the same content and purpose as the messages descrived
    above.

The 'process oid' "tag" that can be provided with the variable name / oids is
intended to be used for oid post processing. The value '`keep`', which is the
default, leaves the oid as is. The value '`truncate`', will cause the oid to be
"truncated". That is, any trailing ".0" will be removed.

> #### Note {: .info }
>
> There is a way to exclude a varbind from the notification. In the normal
> `varbinds` list, providing the special value `'$ignore-oid'` (instead of a
> normal value) will exclude this varbind from the notification.
>
> A define for this has been added to the `snmp_types.hrl` include file,
> `NOTIFICATION_IGNORE_VB_VALUE`.

> #### Note {: .info }
>
> The `extra` info is not normally interpreted by the agent, instead it is
> passed through to the [net-if](snmp_agent_netif.md) process. It is up to the
> implementor of that process to make use of this data.
>
> The version of net-if provided by this application makes no use of this data,
> with one exception: Any tuple containing the atom
> `snmpa_default_notification_extra_info` may be used by the agent and is
> therefore _reserved_.
>
> See the net-if incoming messages for sending a
> [trap](snmp_agent_netif.md#im_send_pdu) and
> [notification](snmp_agent_netif.md#im_send_pdu_req) for more info.

[](){: #send_notification }
""".
-doc(#{since => <<"OTP R14B03">>}).
send_notification2(Agent, Notification, SendOpts) ->
    snmpa_agent:send_notification(Agent, Notification, SendOpts).

-doc(#{equiv => send_notification/7}).
-doc(#{since => <<"OTP R14B">>}).
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

-doc(#{equiv => send_notification/7}).
-doc(#{since => <<"OTP R14B">>}).
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

-doc(#{equiv => send_notification/7}).
-doc(#{since => <<"OTP R14B">>}).
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

-doc(#{equiv => send_notification/7}).
-doc(#{since => <<"OTP R14B">>}).
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

-doc """
send_notification(Agent, Notification, Receiver, NotifyName, ContextName,
Varbinds, LocalEngineID) -> void()

Sends the notification `Notification` to the management targets defined for
`NotifyName` in the `snmpNotifyTable` in SNMP-NOTIFICATION-MIB from the
specified context.

If no `NotifyName` is specified (or if it is `""`), the notification is sent to
all management targets (`Addresses` below).

If no `ContextName` is specified, the default `""` context is used.

The parameter `Receiver` specifies where information about delivery of
Inform-Requests should be sent. The agent sends Inform-Requests and waits for
acknowledgments from the managers. `Receiver` can have three values:

- `no_receiver` \- No information is delivered.
- `notification_delivery_info()` \- The information is delivered via a function
  call according to this data. See the [DATA TYPES](`m:snmpa#data_types`)
  section above for details.
- `{Tag, Recv}` \- The information is delivered either via messages or via a
  function call according to the value of `Recv`.

If `Receiver` has the value `{Tag, Recv}`, the delivery is done according to
`Recv`:

- `pid() | atom()` \- The info will be delivered in the following messages:

  - `{snmp_targets, Tag, Addresses}`

    This inform the user which target addresses the notification was sent to.

  - `{snmp_notification, Tag, {got_response, Address}}`

    This informs the user that this target address acknowledged the
    notification.

  - `{snmp_notification, Tag, {no_response, Address}}`

    This informs the user that this target address did not acknowledge
    notification.

  The notification is sent as an Inform-Request to each target address in
  `Addresses` and if there are no targets for which an Inform-Request is sent,
  `Addresses` is the empty list `[]`.

  The `receiver` will first be sent the `snmp_targets` message, and then for
  each address in `Addresses` list, one of the two `snmp_notification` messages.

- `{Mod, Func, Args}` \- The info will be delivered via the function call:

  `Mod:Func([Msg | Args])`

  where `Msg` has the same content and purpose as the messages descrived above.

`Address` is a management target address and `Addresses` is a list of management
target addresses. They are defined as followes:

```erlang
        Addresses  = [address()]
        Address    = address()
        address()  = v1_address() | v3_address()
        v1_address() = {TDomain, TAddress}
        v3_address() = {{TDomain, TAddress}, V3MsgData}
        TDomain    = tdoamin()
        TAddress   = taddress()
        tdomain()  = The oid of snmpUDPDomain
                     This is the only supported transport domain.
        taddress() = [A1, A2, A3, A4, P1, P3]
                     The 4 first bytes makes up the IP-address and the last 2,
                     the UDP-port number.
        V3MsgData  = v3_msg_data()
        v3_msg_data() = term()
```

If `Receiver` is a `notification_delivery_info()` record, then the information
about the notification delivery will be delivered to the `receiver` via the
callback functions defined by the `m:snmpa_notification_delivery_info_receiver`
behaviour according to the content of the `notification_delivery_info()` record.

The optional argument `Varbinds` defines values for the objects in the
notification. If no value is given for an object, the `Agent` performs a
get-operation to retrieve the value.

`Varbinds` is a list of `Varbind`, where each `Varbind` is one of:

- `{Variable, Value}`, where `Variable` is the symbolic name of a scalar
  variable referred to in the notification specification.
- `{Column, RowIndex, Value}`, where `Column` is the symbolic name of a column
  variable. `RowIndex` is a list of indices for the specified element. If this
  is the case, the OBJECT IDENTIFIER sent in the notification is the `RowIndex`
  appended to the OBJECT IDENTIFIER for the table column. This is the OBJECT
  IDENTIFIER which specifies the element.
- `{OID, Value}`, where `OID` is the OBJECT IDENTIFIER for an instance of an
  object, scalar variable, or column variable.

For example, to specify that `sysLocation` should have the value `"upstairs"` in
the notification, we could use one of:

- `{sysLocation, "upstairs"}` or
- `{[1,3,6,1,2,1,1,6,0], "upstairs"}` or
- `{?sysLocation_instance, "upstairs"}` (provided that the generated `.hrl` file
  is included)

If a variable in the notification is a table element, the `RowIndex` for the
element must be given in the `Varbinds` list. In this case, the OBJECT
IDENTIFIER sent in the notification is the OBJECT IDENTIFIER that identifies
this element. This OBJECT IDENTIFIER could be used in a get operation later.

This function is asynchronous, and does not return any information. If an error
occurs, `user_err/2` of the error report module is called and the notification
is discarded.

> #### Note {: .info }
>
> Note that the use of the LocalEngineID argument is only intended for special
> cases, if the agent is to "emulate" multiple EngineIDs\! By default, the agent
> uses the value of `SnmpEngineID` (see SNMP-FRAMEWORK-MIB).

`ExtraInfo` is not normally used in any way by the agent. It is intended to be
passed along to the net-if process, which is a component that a user can
implement themself. The users own net-if may then make use of ExtraInfo. The
net-if provided with this application does not process ExtraInfo.

There is one exception. _Any_ tuple containing the atom
`snmpa_default_notification_extra_info` will, in this context, be considered
belonging to this application, and may be processed by the agent.

[](){: #discovery }
""".
-doc(#{since => <<"OTP R14B">>}).
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
-doc false.
send_trap(Agent, Trap, Community) ->
    send_notification(Agent, Trap, no_receiver, Community, "", []).

-doc false.
send_trap(Agent, Trap, Community, Varbinds) ->
    send_notification(Agent, Trap, no_receiver, Community, "", Varbinds).


%%%-----------------------------------------------------------------

-doc(#{equiv => discovery/6}).
discovery(TargetName, Notification) ->
    Varbinds = [],
    discovery(TargetName, Notification, Varbinds).

-doc(#{equiv => discovery/6}).
discovery(TargetName, Notification, Varbinds) when is_list(Varbinds) ->
    ContextName = "",
    discovery(TargetName, Notification, ContextName, Varbinds);
discovery(TargetName, Notification, DiscoHandler) 
  when is_atom(DiscoHandler) ->
    Varbinds = [],
    discovery(TargetName, Notification, Varbinds, DiscoHandler).

-doc(#{equiv => discovery/6}).
discovery(TargetName, Notification, ContextName, Varbinds) 
  when is_list(Varbinds) ->
    DiscoHandler = snmpa_discovery_handler_default, 
    discovery(TargetName, Notification, ContextName, Varbinds, 
	      DiscoHandler);
discovery(TargetName, Notification, Varbinds, DiscoHandler) 
  when is_atom(DiscoHandler) ->
    ContextName = "",
    discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler).

-doc(#{equiv => discovery/6}).
discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler) ->
    ExtraInfo = ?DISCO_EXTRA_INFO,
    discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler, 
	      ExtraInfo).

-doc """
discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler,
ExtraInfo) -> {ok, ManagerEngineID} | {error, Reason}

Initiate the discovery process with the manager identified by `TargetName` using
the notification `Notification`.

This function is synchronous, which means that it will return when the discovery
process has been completed or failed.

The `DiscoHandler` module is used during the discovery process. See
[discovery handler](`m:snmpa_discovery_handler`) for more info.

The `ExtraInfo` argument is passed on to the callback functions of the
`DiscoHandler`.

> #### Note {: .info }
>
> If we are not at security-level `noAuthNoPriv`, this could be complicated,
> since the agent will then continue with stage 2, before which the usm-related
> updates must be done.

> #### Note {: .info }
>
> The default discovery handler will require additional actions by the caller
> and the discovery will not work if the security-level is higher then
> `noAuthNoPriv`.

[](){: #convert_config }
""".
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

-doc """
register_subagent(Agent, SubTreeOid, Subagent) -> ok | {error, Reason}

Registers a sub-agent under a sub-tree of another agent.

It is easy to make mistakes when registering sub-agents and this activity should
be done carefully. For example, a strange behaviour would result from the
following configuration:

```erlang
snmp_agent:register_subagent(MAPid,[1,2,3,4],SA1),
snmp_agent:register_subagent(SA1,[1,2,3], SA2).
```

`SA2` will not get requests starting with object identifier `[1,2,3]` since
`SA1` does not.

[](){: #unregister_subagent }
""".
register_subagent(Agent, SubTree, SubAgent) ->
    snmpa_agent:register_subagent(Agent, SubTree, SubAgent).

-doc """
unregister_subagent(Agent, SubagentOidOrPid) -> ok | {ok, SubAgentPid} | {error,
Reason}

Unregister a sub-agent. If the second argument is a pid, then that sub-agent
will be unregistered from all trees in `Agent`.

[](){: #send_notification2 }
""".
unregister_subagent(Agent, SubOidOrPid) ->
    snmpa_agent:unregister_subagent(Agent, SubOidOrPid).

-doc false.
system_start_time() ->
    [{_, Time}] = ets:lookup(snmp_agent_table, system_start_time),
    Time.

-doc false.
sys_up_time() ->
    % time in 0.01 seconds.
    StartTime = system_start_time(),
    (snmp_misc:now(cs) - StartTime) rem (2 bsl 31).


%%%-----------------------------------------------------------------

-doc """
which_transports() -> Result

Retrieve all configured transports.

[](){: #which_variables }
""".
-doc(#{since => <<"OTP 23.3">>}).
which_transports() ->
    {value, Transports} = snmp_framework_mib:intAgentTransports(get),
    [case Kind of
         all ->
             {Domain, Address};
         _ ->
             {Domain, Address, Kind}
     end || {Domain, Address, Kind, _} <- Transports].


%%%-----------------------------------------------------------------

-doc(#{equiv => restart_worker/1}).
restart_worker() ->
    restart_worker(snmp_master_agent).

-doc """
restart_worker(Agent) -> void()

Restart the worker process of a multi-threaded agent.

This is a utility function, that can be useful when e.g. debugging
instrumentation functions.

[](){: #restart_set_worker }
""".
restart_worker(Agent) ->
    snmpa_agent:restart_worker(Agent).


-doc(#{equiv => restart_set_worker/1}).
restart_set_worker() ->
    restart_set_worker(snmp_master_agent).

-doc """
restart_set_worker(Agent) -> void()

Restart the set worker process of a multi-threaded agent.

This is a utility function, that can be useful when e.g. debugging
instrumentation functions.

[](){: #print_mib_info }
""".
restart_set_worker(Agent) ->
    snmpa_agent:restart_set_worker(Agent).


%%%-----------------------------------------------------------------
%%% USM functions
%%%-----------------------------------------------------------------
-doc false.
passwd2localized_key(Alg, Passwd, EngineID) ->
    snmp_usm:passwd2localized_key(Alg, Passwd, EngineID).

-doc false.
localize_key(Alg, Key, EngineID) ->
    snmp_usm:localize_key(Alg, Key, EngineID).


%%%-----------------------------------------------------------------
%%% Agent Capabilities functions
%%%-----------------------------------------------------------------
-doc """
add_agent_caps(SysORID, SysORDescr) -> SysORIndex

This function can be used to add an AGENT-CAPABILITY statement to the sysORTable
in the agent. The table is defined in the SNMPv2-MIB.

[](){: #del_agent_caps }
""".
add_agent_caps(Oid, Descr) ->
    snmp_standard_mib:add_agent_caps(Oid, Descr).

-doc """
del_agent_caps(SysORIndex) -> void()

This function can be used to delete an AGENT-CAPABILITY statement to the
sysORTable in the agent. This table is defined in the SNMPv2-MIB.

[](){: #get_agent_caps }
""".
del_agent_caps(Index) ->
    snmp_standard_mib:del_agent_caps(Index).

-doc """
get_agent_caps() -> [[SysORIndex, SysORID, SysORDescr, SysORUpTime]]

Returns all AGENT-CAPABILITY statements in the sysORTable in the agent. This
table is defined in the SNMPv2-MIB.

[](){: #get }
""".
get_agent_caps() ->
    snmp_standard_mib:get_agent_caps().


%%%-----------------------------------------------------------------
%%% Audit Trail Log functions 
%%%-----------------------------------------------------------------

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
-spec log_to_txt(LogDir :: snmp:dir()) ->
    snmp:void().

log_to_txt(LogDir) -> 
    log_to_txt(LogDir, []).

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
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

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
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

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
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

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
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

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
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

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
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

-doc """
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start, Stop) -> ok |
{ok, Cnt} | {error, Reason}

Converts an Audit Trail Log to a readable text file. `OutFile` defaults to
"./snmpa_log.txt". `LogName` defaults to "snmpa_log". `LogFile` defaults to
"snmpa.log".

The `Block` option indicates if the log should be blocked during conversion.
This could be useful when converting large logs (when otherwise the log could
wrap during conversion). Defaults to `true`.

See [snmp:log_to_txt](`m:snmp#log_to_txt`) for more info.

[](){: #log_to_io }
""".
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
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


-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
log_to_io(LogDir) -> 
    log_to_io(LogDir, []).

-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
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

-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
log_to_io(LogDir, Mibs, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName) -> 
    Block   = ?ATL_BLOCK_DEFAULT, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block).

-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
log_to_io(LogDir, Mibs, LogName, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName, LogFile) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block).

-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
log_to_io(LogDir, Mibs, LogName, LogFile, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName, LogFile, Start) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start).

-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start);
log_to_io(LogDir, Mibs, LogName, LogFile, Start, Stop) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop).

-doc """
log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop) -> ok | {ok, Cnt}
| {error, Reason}

Converts an Audit Trail Log to a readable format and prints it on stdio.
`LogName` defaults to "snmpa_log". `LogFile` defaults to "snmpa.log".

The `Block` option indicates if the log should be blocked during conversion.
This could be useful when converting large logs (when otherwise the log could
wrap during conversion). Defaults to `true`.

See [snmp:log_to_io](`m:snmp#log_to_io`) for more info.

[](){: #change_log_size }
""".
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop) -> 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop).


-doc false.
log_info() ->
    LogName = ?audit_trail_log_name, 
    snmp_log:info(LogName).


-doc """
change_log_size(NewSize) -> ok | {error, Reason}

Changes the log size of the Audit Trail Log. The application must be configured
to use the audit trail log function. Please refer to disk_log(3) in Kernel
Reference Manual for a description of how to change the log size.

The change is permanent, as long as the log is not deleted. That means, the log
size is remembered across reboots.

[](){: #set_log_type }
""".
change_log_size(NewSize) -> 
    LogName = ?audit_trail_log_name, % The old (agent) default
    snmp:change_log_size(LogName, NewSize).


-doc false.
get_log_type() ->
    get_log_type(snmp_master_agent).

-doc false.
get_log_type(Agent) ->
    snmpa_agent:get_log_type(Agent).

%% NewType -> atl_type()
-doc false.
change_log_type(NewType) ->
    set_log_type(NewType).

-doc false.
change_log_type(Agent, NewType) ->
    set_log_type(Agent, NewType).

-doc(#{equiv => set_log_type/2}).
set_log_type(NewType) ->
    set_log_type(snmp_master_agent, NewType).

-doc """
set_log_type(Agent, NewType) -> {ok, OldType} | {error, Reason}

Changes the run-time Audit Trail log type.

Note that this has no effect on the application configuration as defined by
configuration files, so a node restart will revert the config to whatever is in
those files.

This function is primarily useful in testing/debugging scenarios.

[](){: #mib_of }
""".
set_log_type(Agent, NewType) ->
    snmpa_agent:set_log_type(Agent, NewType).
