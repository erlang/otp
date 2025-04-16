%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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

              discovery_handler/0,

	      %% Agent config types
	      mib_storage/0, 
	      mib_storage_opt/0, 
	      mib_storage_module/0, 
	      mib_storage_options/0,

              nfilter_id/0,
              nfilter_position/0,

              notification_delivery_info/0,
              transport_kind/0
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

-doc """
Module implementing the [snmpa_discovery_handler](`m:snmpa_discovery_handler#`)
behaviour.
""".
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

-doc "How shall (notification) delivery info be reported.".
-type notification_delivery_info() ::
        snmpa_notification_delivery_info_receiver:notification_delivery_info().

-type transport_kind() :: req_responder | trap_sender.


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
This off-line utility function can be used to convert the old snmp application
config (pre snmp-4.0) to the new snmp agent config (as of snmp-4.0).

For information about the old config (`OldConfig`) see the OTP R9C
documentation.

For information about the current agent config (`AgentConfig`), see the
[Configuring the application](snmp_config.md#configuration_params) chapter of
the SNMP user's guide.
""".
-spec convert_config(OldConfig) -> NewConfig when
      OldConfig :: list(),
      NewConfig :: list().

convert_config(Opts) ->
    snmpa_app:convert_config(Opts).


%%-----------------------------------------------------------------
%% Note that verbosity for the agents is actually only implemented 
%% (properly) for the master agent.
%%-----------------------------------------------------------------

-doc """
Sets 'verbosity' for the indicated process(s):

- **`all`** -
  Sets verbosity for all the agent processes; net_if, note_store, mib_server,
symbolic_store, local_db and master_agent (and sub-agents).

- **`net_if`** - Sets verbosity for the net-if process.

- **`note_store`** - Sets verbosity for the note store process.

- **`mib_server`** - Sets verbosity for the mib server process.

- **`symbolic_store`** - Sets verbosity for the symbolic store process.

- **`local_db`** - Sets verbosity for the local-db process.

- **`master_agent | pid()`** when `Verbosity = {subagents,` [`snmp:verbosity()`](`t:snmp:verbosity/0`)`}` -
  Sets verbosity for all sub-agent(s) controlled by this (master) agent.

- **`master_agent | pid() | atom()`** - Sets verbosity for the agent process.

The following text documents expected input-output relations

- If `Target :: all | net_if | note_store | mib_server | symbolic_store | local_db`,
  then `Verbosity :: snmp:verbosity()`.

- If `Target :: master_agent`,
  then `Verbosity :: {subagents, snmp:verbosity()}`

- If `Target :: pid() | atom()`,
  then `Verbosity :: snmp:verbosity() | {subagents, snmp:verbosity()}`.
""".
-spec verbosity(Target, Verbosity) -> snmp:void() when
      Target :: 'all' | LogicalName | PidOrAgentName,
      LogicalName :: 'net_if' | 'note_store' | 'mib_server'
                   | 'symbolic_store' | 'local_db',
      PidOrAgentName :: pid() | 'master_agent' | atom(),
      Verbosity :: SNMPVerb | SubAgent,
      SNMPVerb :: snmp:verbosity(),
      SubAgent :: {subagents, snmp:verbosity()}.

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

-doc false.
get_symbolic_store_db() ->
    snmpa_symbolic_store:get_db().


-doc """
Retrieve all alias-names known to the agent.
""".
-spec which_aliasnames() -> AliasNames when
      AliasNames :: [AliasName],
      AliasName  :: atom().

which_aliasnames() ->
    snmpa_symbolic_store:which_aliasnames().

-doc """
Retrieve all tables known to the agent.
""".
-spec which_tables() -> Tables when
      Tables :: [Table],
      Table  :: atom().

which_tables() ->
    snmpa_symbolic_store:which_tables().

-doc """
Retrieve all variables known to the agent.
""".
-spec which_variables() -> Variables when
      Variables :: [Variable],
      Variable  :: atom().

which_variables() ->
    snmpa_symbolic_store:which_variables().

-doc """
Retrieve all notifications (and traps) known to the agent.
""".
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

-doc(#{equiv => name_to_oid/2}).
-spec name_to_oid(Name) -> {value, Oid} | false when
      Name :: atom(),
      Oid  :: snmp:oid().

name_to_oid(Name) ->
    snmpa_symbolic_store:aliasname_to_oid(Name).

-doc """
Looks up the OBJECT IDENTIFIER of a MIB object, given the symbolic name. Note,
the OBJECT IDENTIFIER is given for the object, not for an instance.

`false` is returned if the object is not defined in any loaded MIB.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).
""".
-spec name_to_oid(Db, Name) -> {value, Oid} | false when
      Db   :: term(),
      Name :: atom(),
      Oid  :: snmp:oid().

name_to_oid(Db, Name) ->
    snmpa_symbolic_store:aliasname_to_oid(Db, Name).


-doc(#{equiv => oid_to_name/2}).
-spec oid_to_name(Oid) -> {value, Name} | false when
      Oid  :: snmp:oid(),
      Name :: atom().

oid_to_name(Oid) ->
    snmpa_symbolic_store:oid_to_aliasname(Oid).

-doc """
Looks up the symbolic name of a MIB object, given OBJECT IDENTIFIER.

`false` is returned if the object is not defined in any loaded MIB.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).
""".
-spec oid_to_name(Db, Oid) -> {value, Name} | false when
      Db   :: term(),
      Oid  :: snmp:oid(),
      Name :: atom().

oid_to_name(Db, Oid) ->
    snmpa_symbolic_store:oid_to_aliasname(Db, Oid).


-doc(#{equiv => enum_to_int/3}).
-spec enum_to_int(Name, Enum) -> {value, Int} | false when
      Name :: atom(),
      Enum :: atom(),
      Int  :: integer().

enum_to_int(Name, Enum) ->
    snmpa_symbolic_store:enum_to_int(Name, Enum).

-doc """
Converts the symbolic value `Enum` to the corresponding integer of the
enumerated object or type `Name` in a MIB. The MIB must be loaded.

`false` is returned if the object or type is not defined in any loaded MIB, or
if it does not define the symbolic value as enumerated.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).
""".
-spec enum_to_int(Db, Name, Enum) -> {value, Int} | false when
      Db   :: term(),
      Name :: atom(),
      Enum :: atom(),
      Int  :: integer().

enum_to_int(Db, Name, Enum) ->
    snmpa_symbolic_store:enum_to_int(Db, Name, Enum).


-doc(#{equiv => int_to_enum/3}).
-spec int_to_enum(Name, Int) -> {value, Enum} | false when
      Name :: atom(),
      Int  :: integer(),
      Enum :: atom().

int_to_enum(Name, Int) ->
    snmpa_symbolic_store:int_to_enum(Name, Int).

-doc """
Converts the integer `Int` to the corresponding symbolic value of the enumerated
object or type `Name` in a MIB. The MIB must be loaded.

`false` is returned if the object or type is not defined in any loaded MIB, or
if it does not define the symbolic value as enumerated.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).
""".
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

-doc """
Get the address of the request currently being processed by the agent.

Note that this function is intended to be called by the instrumentation
functions and _only_ if it is executed in the context of the agent process (e.g.
it _does not work_ if called from a spawned process).
""".
-spec current_address() -> {value, Address} | false when
      Address :: term().

current_address()     -> current_get(snmp_address).

-doc """
Get the community of the request currently being processed by the agent.

Note that this function is intended to be called by the instrumentation
functions and _only_ if it is executed in the context of the agent process (e.g.
it _does not work_ if called from a spawned process).
""".
-spec current_community() -> {value, Community} | false when
      Community :: snmp_community_mib:name().

current_community()   -> current_get(snmp_community).

-doc """
Get the context of the request currently being processed by the agent.

Note that this function is intended to be called by the instrumentation
functions and _only_ if it is executed in the context of the agent process (e.g.
it _does not work_ if called from a spawned process).
""".
-spec current_context() -> {value, Context} | false when
      Context :: snmp_community_mib:context_name().

current_context()     -> current_get(snmp_context).

-doc """
Get the request-id of the request currently being processed by the agent.

Note that this function is intended to be called by the instrumentation
functions and _only_ if it is executed in the context of the agent process (e.g.
it _does not work_ if called from a spawned process).
""".
-spec current_request_id() -> {value, RequestId} | false when
      RequestId :: integer().

current_request_id()  -> current_get(snmp_request_id).

-doc false.
current_net_if_data() -> current_get(net_if_data).

current_get(Tag) ->
    case get(Tag) of
	undefined -> false;
	X -> {value, X}
    end.
    

%% -

-doc(#{equiv => get/3}).
-spec get(Agent, Vars) -> Values | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Vars      :: [snmp:oid()],
      Values    :: [term()],
      Reason    :: term().

get(Agent, Vars) -> snmpa_agent:get(Agent, Vars).

-doc """
Performs a GET operation on the agent. All loaded MIB objects are visible in
this operation. The agent calls the corresponding instrumentation functions just
as if it was a GET request coming from a manager.

Note that the request specific parameters (such as
`current_request_id/0`) are not accessible for the
instrumentation functions if this function is used.
""".
-spec get(Agent, Vars, Context) -> Values | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Vars      :: [snmp:oid()],
      Context   :: snmp_community_mib:context_name(),
      Values    :: [term()],
      Reason    :: term().

get(Agent, Vars, Context) -> snmpa_agent:get(Agent, Vars, Context).


-doc(#{equiv => get_next/3}).
-spec get_next(Agent, Vars) -> Values | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Vars      :: [snmp:oid()],
      Values    :: [{snmp:oid(), term()}],
      Reason    :: term().

get_next(Agent, Vars) -> snmpa_agent:get_next(Agent, Vars).

-doc """
Performs a GET-NEXT operation on the agent. All loaded MIB objects are visible
in this operation. The agent calls the corresponding instrumentation functions
just as if it was a GET request coming from a manager.

Note that the request specific parameters (such as `snmpa:current_request_id/0`
are not accessible for the instrumentation functions if this function is used.
""".
-spec get_next(Agent, Vars, Context) -> Values | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Vars      :: [snmp:oid()],
      Context   :: snmp_community_mib:context_name(),
      Values    :: [{snmp:oid(), term()}],
      Reason    :: {atom(), snmp:oid()}.

get_next(Agent, Vars, Context) -> snmpa_agent:get_next(Agent, Vars, Context).


-doc(#{equiv => info/1}).
-spec info() -> Info when
      Info  :: [{Key, Value}],
      Key   :: term(),
      Value :: term().

info()      -> info(snmp_master_agent).

-doc """
Returns a list (a dictionary) containing information about the agent.
Information includes loaded MIBs, registered sub-agents, some information about
the memory allocation.
""".
-spec info(Agent) -> Info when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Info      :: [{Key, Value}],
      Key       :: term(),
      Value     :: term().

info(Agent) -> snmpa_agent:info(Agent).


%% -

-doc(#{equiv => backup/2}).
-spec backup(BackupDir) -> ok | {error, Reason} when
      BackupDir :: string(),
      Reason    :: backup_in_progress | term().

backup(BackupDir) ->
    backup(snmp_master_agent, BackupDir).

-doc """
Backup persistent/permanent data handled by the agent (such as local-db,
mib-data and vacm).

Data stored by mnesia is not handled.

`BackupDir` cannot be identical to DbDir.

Simultaneous backup calls are _not_ allowed. That is, two different processes
cannot simultaneously successfully call this function. One of them will be
first, and succeed. The second will fail with the error reason
`backup_in_progress`.
""".
-spec backup(Agent, BackupDir) -> ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      BackupDir :: string(),
      Reason    :: backup_in_progress | term().

backup(Agent, BackupDir) ->
    snmpa_agent:backup(Agent, BackupDir).


%% -

-doc false.
dump_mibs()     -> snmpa_agent:dump_mibs(snmp_master_agent).
-doc false.
dump_mibs(File) -> snmpa_agent:dump_mibs(snmp_master_agent, File).


-doc(#{equiv => load_mib/2}).
-doc(#{since => <<"OTP R16B02">>}).
-spec load_mib(Mib) -> 
          ok | {error, Reason} when
      Mib    :: string(),
      Reason :: already_loaded | term().

load_mib(Mib) ->
    load_mib(snmp_master_agent, Mib).

-doc """
Load a single `Mib` into an agent. The `MibName` is the name of the Mib,
including the path to where the compiled mib is found. For example:

```erlang
          Dir = code:priv_dir(my_app) ++ "/mibs/",
          snmpa:load_mib(snmp_master_agent, Dir ++ "MY-MIB").
```
""".
-doc(#{since => <<"OTP R16B02">>}).
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


-doc(#{equiv => load_mibs/3}).
-spec load_mibs(Mibs) -> 
          ok | {error, Reason} when
      Mibs           :: [MibName],
      MibName        :: string(),
      Reason         :: {'load aborted at', MibName, InternalReason},
      InternalReason :: already_loaded | term().

load_mibs(Mibs) ->
    load_mibs(snmp_master_agent, Mibs, false).

-doc(#{equiv => load_mibs/3}).
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

-doc """
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
""".
-doc(#{since => <<"OTP R16B02">>}).
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


-doc(#{equiv => unload_mib/2}).
-doc(#{since => <<"OTP R16B02">>}).
-spec unload_mib(Mib) ->
          ok | {error, Reason} when
      Mib    :: string(),
      Reason :: not_loaded | term().

unload_mib(Mib) ->
    unload_mib(snmp_master_agent, Mib).

-doc """
Unload a single `Mib` from an agent.
""".
-doc(#{since => <<"OTP R16B02">>}).
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


-doc(#{equiv => unload_mibs/3}).
-spec unload_mibs(Mibs) ->
          ok | {error, Reason} when
      Mibs           :: [MibName], 
      MibName        :: string(),
      Reason         :: {'unload aborted at', MibName, InternalReason},
      InternalReason :: not_loaded | term().

unload_mibs(Mibs) ->
    unload_mibs(snmp_master_agent, Mibs).

-doc(#{equiv => unload_mibs/3}).
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

-doc """
Unload `Mibs` from an agent. If it cannot unload all MIBs (the default value of
the `Force` argument is `false`), it will indicate where unloading was aborted.

If `Force = true` then the agent will continue attempting to unload each mib
even after failing to unload a previous mib. Use with care.
""".
-doc(#{since => <<"OTP R16B02">>}).
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


-doc(#{equiv => which_mibs/1}).
-spec which_mibs() -> Mibs when
      Mibs    :: [{MibName, MibFile}],
      MibName :: atom(),
      MibFile :: string().

which_mibs()      -> which_mibs(snmp_master_agent).

-doc """
Retrieve the list of all the mibs loaded into this agent. Default is the master
agent.
""".
-spec which_mibs(Agent) -> Mibs when
      Agent     :: pid() | AgentName, 
      AgentName :: atom(),
      Mibs      :: [{MibName, MibFile}],
      MibName   :: atom(),
      MibFile   :: string().

which_mibs(Agent) -> snmpa_agent:which_mibs(Agent).


-doc(#{equiv => whereis_mib/2}).
-spec whereis_mib(MibName) -> {ok, MibFile} | {error, Reason} when
      MibName :: atom(),
      MibFile :: string(),
      Reason  :: term().

whereis_mib(Mib) ->
    whereis_mib(snmp_master_agent, Mib).

-doc """
Get the full path to the (compiled) mib-file.
""".
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


-doc """
Prints the content of all the (snmp) tables and variables for all mibs handled
by the snmp agent.
""".
-doc(#{since => <<"OTP R14B02">>}).
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


-doc """
Prints the content of all the (snmp) tables for all mibs handled by the snmp
agent.
""".
-doc(#{since => <<"OTP R14B02">>}).
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


-doc """
Prints the content of all the (snmp) variables for all mibs handled by the snmp
agent.
""".
-doc(#{since => <<"OTP R14B02">>}).
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

-doc(#{equiv => mib_of/2}).
-spec mib_of(Oid) -> {ok, MibName} | {error, Reason} when
      Oid     :: snmp:oid(),
      MibName :: atom(),
      Reason  :: term().

mib_of(Oid) ->
    snmpa_agent:mib_of(Oid).

-doc """
Finds the mib corresponding to the `Oid`.

If it is a variable, the Oid must be `<Oid for var>.0` and if it is a table, Oid
must be `<table>.<entry>.<col>.<any>`.
""".
-spec mib_of(Agent, Oid) -> {ok, MibName} | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Oid       :: snmp:oid(),
      MibName   :: atom(),
      Reason    :: term().

mib_of(Agent, Oid) ->
    snmpa_agent:mib_of(Agent, Oid).


-doc(#{equiv => me_of/2}).
-spec me_of(Oid) -> {ok, Me} | {error, Reason} when
      Oid    :: snmp:oid(),
      Me     :: snmp:me(),
      Reason :: term().

me_of(Oid) ->
    snmpa_agent:me_of(Oid).

-doc """
Finds the mib entry corresponding to the `Oid`.

If it is a variable, the Oid must be <Oid for var>.0 and if it is a table, Oid
must be `<table>.<entry>.<col>.<any>`
""".
-spec me_of(Agent, Oid) -> {ok, Me} | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Oid       :: snmp:oid(),
      Me        :: snmp:me(),
      Reason    :: term().

me_of(Agent, Oid) ->
    snmpa_agent:me_of(Agent, Oid).


-doc(#{equiv => invalidate_mibs_cache/1}).
-spec invalidate_mibs_cache() -> snmp:void().

invalidate_mibs_cache() ->
    invalidate_mibs_cache(snmp_master_agent).

-doc """
Invalidate the mib server cache.

The entire contents of the cache will be deleted.
""".
-spec invalidate_mibs_cache(Agent) -> snmp:void() when
      Agent     :: pid() | AgentName,
      AgentName :: atom().

invalidate_mibs_cache(Agent) ->
    snmpa_agent:invalidate_mibs_cache(Agent).


-doc(#{equiv => which_mibs_cache_size/1}).
-doc(#{since => <<"OTP R14B">>}).
-spec which_mibs_cache_size() -> {ok, Size} | {error, Reason} when
      Size   :: non_neg_integer(),
      Reason :: term().

which_mibs_cache_size() ->
    which_mibs_cache_size(snmp_master_agent).

-doc """
Retrieve the size of the mib server cache.
""".
-doc(#{since => <<"OTP R14B">>}).
-spec which_mibs_cache_size(Agent) -> {ok, Size} | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Size      :: non_neg_integer(),
      Reason    :: term().

which_mibs_cache_size(Agent) ->
    snmpa_agent:which_mibs_cache_size(Agent).


-doc(#{equiv => enable_mibs_cache/1}).
-spec enable_mibs_cache() -> snmp:void().

enable_mibs_cache() ->
    enable_mibs_cache(snmp_master_agent).

-doc """
Enable the mib server cache.
""".
-spec enable_mibs_cache(Agent) -> snmp:void() when
      Agent     :: pid() | AgentName,
      AgentName :: atom().

enable_mibs_cache(Agent) ->
    snmpa_agent:enable_mibs_cache(Agent).


-doc(#{equiv => disable_mibs_cache/1}).
-spec disable_mibs_cache() -> snmp:void().

disable_mibs_cache() ->
    disable_mibs_cache(snmp_master_agent).

-doc """
Disable the mib server cache.
""".
-spec disable_mibs_cache(Agent) -> snmp:void() when
      Agent     :: pid() | AgentName,
      AgentName :: atom().

disable_mibs_cache(Agent) ->
    snmpa_agent:disable_mibs_cache(Agent).


-doc(#{equiv => gc_mibs_cache/3}).
-spec gc_mibs_cache() ->
          {ok, NumElementsGCed} | {error, Reason} when
      NumElementsGCed :: non_neg_integer(),
      Reason          :: term().

gc_mibs_cache() ->
    gc_mibs_cache(snmp_master_agent).

-doc(#{equiv => gc_mibs_cache/3}).
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

-doc(#{equiv => gc_mibs_cache/3}).
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

-doc """
Perform mib server cache gc.

Manually performs a mib server cache gc. This can be done regardless of the
value of the `autogc` option. The `NumElementsGCed` value indicates how many
elements where actually removed from the cache.
""".
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


-doc(#{equiv => enable_mibs_cache_autogc/1}).
-spec enable_mibs_cache_autogc() -> snmp:void().

enable_mibs_cache_autogc() ->
    enable_mibs_cache_autogc(snmp_master_agent).

-doc """
Enable automatic gc of the mib server cache.
""".
-spec enable_mibs_cache_autogc(Agent) -> snmp:void() when
      Agent     :: pid() | AgentName,
      AgentName :: atom().

enable_mibs_cache_autogc(Agent) ->
    snmpa_agent:enable_mibs_cache_autogc(Agent).


-doc(#{equiv => disable_mibs_cache_autogc/1}).
-spec disable_mibs_cache_autogc() -> snmp:void().

disable_mibs_cache_autogc() ->
    disable_mibs_cache_autogc(snmp_master_agent).

-doc """
Disable automatic gc of the mib server cache.
""".
-spec disable_mibs_cache_autogc(Agent) -> snmp:void() when
      Agent     :: pid() | AgentName,
      AgentName :: atom().

disable_mibs_cache_autogc(Agent) ->
    snmpa_agent:disable_mibs_cache_autogc(Agent).


-doc(#{equiv => update_mibs_cache_age/2}).
-spec update_mibs_cache_age(Age) -> ok | {error, Reason} when
      Age    :: pos_integer(),
      Reason :: term().

update_mibs_cache_age(Age) ->
    update_mibs_cache_age(snmp_master_agent, Age).

-doc """
Change the mib server cache `age` property.
""".
-spec update_mibs_cache_age(Agent, Age) -> ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Age       :: pos_integer(),
      Reason    :: term().

update_mibs_cache_age(Agent, Age) ->
    snmpa_agent:update_mibs_cache_age(Agent, Age).


-doc(#{equiv => update_mibs_cache_gclimit/2}).
-spec update_mibs_cache_gclimit(GcLimit) -> ok | {error, Reason} when
      GcLimit :: pos_integer(),
      Reason  :: term().

update_mibs_cache_gclimit(GcLimit) ->
    update_mibs_cache_gclimit(snmp_master_agent, GcLimit).

-doc """
Change the mib server cache `gclimit` property.
""".
-spec update_mibs_cache_gclimit(Agent, GcLimit) -> ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      GcLimit   :: pos_integer(),
      Reason    :: term().

update_mibs_cache_gclimit(Agent, GcLimit) ->
    snmpa_agent:update_mibs_cache_gclimit(Agent, GcLimit).




%% - message filter / load regulation

-doc(#{equiv => register_notification_filter/5}).
-spec register_notification_filter(Id, Mod, Data) ->
          ok | {error, Reason} when
      Id     :: nfilter_id(),
      Mod    :: module(),
      Data   :: term(),
      Reason :: term().

register_notification_filter(Id, Mod, Data) when is_atom(Mod) ->
    register_notification_filter(snmp_master_agent, Id, Mod, Data, last).
 
-doc(#{equiv => register_notification_filter/5}).
-doc """
Accepted type specifications are:
```
-spec register_notification_filter(Agent, Id, Mod, Data) -> ok | {error, Reason}.
-spec register_notification_filter(Id, Mod, Data, Where) -> ok | {error, Reason}.
```
""".
-spec register_notification_filter(Agent | Id, Id | Mod, Mod | Data, Data | Where) ->
          ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Id        :: nfilter_id(),
      Mod       :: module(),
      Data      :: term(),
      Where  :: nfilter_position(),
      Reason :: term().

register_notification_filter(Agent, Id, Mod, Data)
  when (is_pid(Agent) orelse is_atom(Agent)) andalso is_atom(Mod) ->
    register_notification_filter(Agent, Id, Mod, Data, last);
register_notification_filter(Id, Mod, Data, Where) when is_atom(Mod) ->
    register_notification_filter(snmp_master_agent, Id, Mod, Data, Where).
 
-doc """
Registers a notification filter.

`Mod` is a module implementing the `snmpa_notification_filter` behaviour.

`Data` will be passed on to the filter when calling the functions of the
behaviour.
""".
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


-doc(#{equiv => unregister_notification_filter/2}).
-spec unregister_notification_filter(Id) ->
          ok | {error, Reason} when
      Id     :: nfilter_id(),
      Reason :: term().

unregister_notification_filter(Id) ->
    unregister_notification_filter(snmp_master_agent, Id).

-doc """
Unregister a notification filter.
""".
-spec unregister_notification_filter(Agent, Id) ->
          ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Id        :: nfilter_id(),
      Reason    :: term().

unregister_notification_filter(Agent, Id) ->
    snmpa_agent:unregister_notification_filter(Agent, Id).

 
-doc(#{equiv => which_notification_filter/1}).
-spec which_notification_filter() -> Filters when
      Filters  :: [FilterId],
      FilterId :: nfilter_id().

which_notification_filter() ->
    which_notification_filter(snmp_master_agent).
 
-doc """
List all notification filters in an agent.
""".
-spec which_notification_filter(Agent) -> Filters when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      Filters   :: [FilterId],
      FilterId  :: nfilter_id().

which_notification_filter(Agent) ->
    snmpa_agent:which_notification_filter(Agent).
 

-doc false.
get_request_limit() -> 
    get_request_limit(snmp_master_agent).
-doc false.
get_request_limit(Agent) -> 
    snmpa_agent:get_request_limit(Agent).


-doc(#{equiv => set_request_limit/2}).
-spec set_request_limit(NewLimit) ->
          {ok, OldLimit} | {error, Reason} when
      NewLimit :: infinity | non_neg_integer(),
      OldLimit :: infinity | non_neg_integer(),
      Reason   :: term().

set_request_limit(NewLimit) -> 
    set_request_limit(snmp_master_agent, NewLimit).

-doc """
Changes the request limit.

Note that this has no effect on the application configuration as defined by
configuration files, so a node restart will revert the config to whatever is in
those files.

This function is primarily useful in load regulation scenarios.
""".
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

-doc """
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
- `t:notification_delivery_info/0` \- The information is delivered via a
  function call according to this data.
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
""".
-doc(#{since => <<"OTP R14B03">>}).
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


-doc(#{equiv => send_notification/7}).
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

-doc(#{equiv => send_notification/7}).
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

-doc(#{equiv => send_notification/7}).
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

-doc(#{equiv => send_notification/7}).
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

-doc """
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
- `t:notification_delivery_info/0` \- The information is delivered via a
  function call according to this data.
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

If `Receiver` is a `t:notification_delivery_info/0` record, then the information
about the notification delivery will be delivered to the `receiver` via the
callback functions defined by the `m:snmpa_notification_delivery_info_receiver`
behaviour according to the content of the `t:notification_delivery_info/0`
record.

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
""".
-doc(#{since => <<"OTP R14B">>}).
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
-doc false.
send_trap(Agent, Trap, Community) ->
    send_notification(Agent, Trap, no_receiver, Community, "", []).

-doc false.
send_trap(Agent, Trap, Community, Varbinds) ->
    send_notification(Agent, Trap, no_receiver, Community, "", Varbinds).


%%%-----------------------------------------------------------------

-doc(#{equiv => discovery/6}).
-spec discovery(TargetName, Notification) ->
          {ok, ManagerEngineID} | {error, Reason} when
      TargetName      :: string(),
      Notification    :: atom(),
      ManagerEngineID :: snmp_framework_mib:engine_id(),
      Reason          :: term().

discovery(TargetName, Notification) ->
    Varbinds = [],
    discovery(TargetName, Notification, Varbinds).

-dialyzer({no_contracts, discovery/3}).
-doc(#{equiv => discovery/6}).
-spec discovery(TargetName, Notification, Varbinds) ->
          {ok, ManagerEngineID} | {error, Reason} when
      TargetName      :: string(),
      Notification    :: atom(),
      Varbinds        :: [Varbind],
      Varbind         :: {Variable :: atom(), Value}
                       | {OID :: snmp:oid(), Value}
                       | { Column :: atom(),
                           RowIndex :: snmp:row_index(), Value},
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

-doc(#{equiv => discovery/6}).
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

-doc(#{equiv => discovery/6}).
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

-doc """
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
""".
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

-doc """
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
""".
-spec register_subagent(Agent, SubTree, SubAgent) -> ok | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      SubTree   :: snmp:oid(),
      SubAgent  :: pid(),
      Reason    :: term().

register_subagent(Agent, SubTree, SubAgent) ->
    snmpa_agent:register_subagent(Agent, SubTree, SubAgent).


-doc """
Unregister a sub-agent. If the second argument is a pid, then that sub-agent
will be unregistered from all trees in `Agent`.
""".
-spec unregister_subagent(Agent, SubAgentOidOrPid) -> ok | {error, Reason} when
      Agent            :: pid() | AgentName,
      AgentName        :: atom(),
      SubAgentOidOrPid :: snmp:oid() | pid(),
      Reason           :: term().

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
Retrieve all configured transports.
""".
-doc(#{since => <<"OTP 23.3">>}).
-spec which_transports() -> Transports when
      Transports :: [Transport],
      Transport  :: {TDomain, TAddress} |
                    {TDomain, TAddress, Kind},
      TDomain    :: snmp:tdomain(),
      TAddress   :: {IpAddr, IpPort},
      IpAddr     :: inet:ip_address(),
      IpPort     :: inet:port_number(),
      Kind       :: transport_kind().

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
-spec restart_worker() -> snmp:void().

restart_worker() ->
    restart_worker(snmp_master_agent).

-doc """
Restart the worker process of a multi-threaded agent.

This is a utility function, that can be useful when e.g. debugging
instrumentation functions.
""".
-spec restart_worker(Agent) -> snmp:void() when
      Agent     :: pid | AgentName,
      AgentName :: atom().

restart_worker(Agent) ->
    snmpa_agent:restart_worker(Agent).


-doc(#{equiv => restart_set_worker/1}).
-spec restart_set_worker() -> snmp:void().

restart_set_worker() ->
    restart_set_worker(snmp_master_agent).

-doc """
Restart the set worker process of a multi-threaded agent.

This is a utility function, that can be useful when e.g. debugging
instrumentation functions.
""".
-spec restart_set_worker(Agent) -> snmp:void() when
      Agent     :: pid | AgentName,
      AgentName :: atom().

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
This function can be used to add an AGENT-CAPABILITY statement to the sysORTable
in the agent. The table is defined in the SNMPv2-MIB.
""".
-spec add_agent_caps(Oid, Descr) -> Index when
      Oid   :: snmp:oid(),
      Descr :: string(),
      Index :: integer().

add_agent_caps(Oid, Descr) ->
    snmp_standard_mib:add_agent_caps(Oid, Descr).

-doc """
This function can be used to delete an AGENT-CAPABILITY statement to the
sysORTable in the agent. This table is defined in the SNMPv2-MIB.
""".
-spec del_agent_caps(Index) -> snmp:void() when
      Index :: integer().

del_agent_caps(Index) ->
    snmp_standard_mib:del_agent_caps(Index).

-doc """
Returns all AGENT-CAPABILITY statements in the sysORTable in the agent. This
table is defined in the SNMPv2-MIB.

Note that the Erlang type language do not permit us to properly describe what
this function returns. The _exact_ return is:

`[[SysORIndex, SysORID, SysORDescr, SysORUpTime]]`
""".
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

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R15B01">>}).
-spec log_to_txt(LogDir) -> snmp:void() when
      LogDir :: snmp:dir().

log_to_txt(LogDir) ->
    log_to_txt(LogDir, []).


-doc(#{equiv => log_to_txt/8}).
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


-doc(#{equiv => log_to_txt/8}).
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


-doc(#{equiv => log_to_txt/8}).
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


-doc(#{equiv => log_to_txt/8}).
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


-doc(#{equiv => log_to_txt/8}).
-spec log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block | Start) ->
          snmp:void() when
      LogDir  :: snmp:dir(),
      Mibs    :: [snmp:mib_name()],
      OutFile :: file:filename(),
      LogName :: string(),
      LogFile :: string(),
      Block   :: boolean(),
      Start   :: null | snmp:log_time().

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start).


-doc(#{equiv => log_to_txt/8}).
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

-doc """
Converts an Audit Trail Log to a readable text file. `OutFile` defaults to
"./snmpa_log.txt". `LogName` defaults to "snmpa_log". `LogFile` defaults to
"snmpa.log".

The `Block` option indicates if the log should be blocked during conversion.
This could be useful when converting large logs (when otherwise the log could
wrap during conversion). Defaults to `true`.

See [`snmp:log_to_txt/8`](`snmp:log_to_txt/8`) for more info.
""".
-doc(#{since => <<"OTP R16B03">>}).
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


-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01">>}).
-spec log_to_io(LogDir) -> ok | {ok, Cnt} | {error, Reason} when
      LogDir :: string(),
      Cnt    :: {NumOK, NumERR},
      NumOK  :: non_neg_integer(),
      NumERR :: pos_integer(),
      Reason :: term().

log_to_io(LogDir) -> 
    log_to_io(LogDir, []).

-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01">>}).
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

-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01">>}).
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

-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01">>}).
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

-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01">>}).
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

-doc(#{equiv => log_to_io/7}).
-doc(#{since => <<"OTP R15B01">>}).
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

-doc """
Converts an Audit Trail Log to a readable format and prints it on stdio.
`LogName` defaults to "snmpa_log". `LogFile` defaults to "snmpa.log".

The `Block` option indicates if the log should be blocked during conversion.
This could be useful when converting large logs (when otherwise the log could
wrap during conversion). Defaults to `true`.

See [`snmp:log_to_io/7`](`snmp:log_to_io/7`) for more info.
""".
-doc(#{since => <<"OTP R16B03">>}).
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


-doc false.
log_info() ->
    LogName = ?audit_trail_log_name, 
    snmp_log:info(LogName).


-doc """
Changes the log size of the Audit Trail Log. The application must be configured
to use the audit trail log function. Please refer to disk_log(3) in Kernel
Reference Manual for a description of how to change the log size.

The change is permanent, as long as the log is not deleted. That means, the log
size is remembered across reboots.
""".
-spec change_log_size(NewSize) -> ok | {error, Reason} when
      NewSize :: snmp:log_size(),
      Reason  :: term().

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
-spec set_log_type(NewType) -> {ok, OldType} | {error, Reason} when
      NewType :: snmp:atl_type(),
      OldType :: snmp:atl_type(),
      Reason  :: term().

set_log_type(NewType) ->
    set_log_type(snmp_master_agent, NewType).

-doc """
Changes the run-time Audit Trail log type.

Note that this has no effect on the application configuration as defined by
configuration files, so a node restart will revert the config to whatever is in
those files.

This function is primarily useful in testing/debugging scenarios.
""".
-spec set_log_type(Agent, NewType) -> {ok, OldType} | {error, Reason} when
      Agent     :: pid() | AgentName,
      AgentName :: atom(),
      NewType   :: snmp:atl_type(),
      OldType   :: snmp:atl_type(),
      Reason    :: term().

set_log_type(Agent, NewType) ->
    snmpa_agent:set_log_type(Agent, NewType).
