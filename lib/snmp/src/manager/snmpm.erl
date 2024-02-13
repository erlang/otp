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

-module(snmpm).
-moduledoc """
Interface functions to the SNMP toolkit manager

The module `snmpm` contains interface functions to the SNMP manager.

## Common Data Types

The following data types are used in the functions below:

```erlang
oid() = [byte()]  -  The oid() type is used to represent an ASN.1 OBJECT IDENTIFIER
snmp_reply() = {error_status(), error_index(), varbinds()}
error_status() = noError | atom()
error_index() = integer()
varbinds() = [varbind()]
atl_type() = read | write | read_write
target_name() = string()  -  Is a unique *non-empty* string
vars_and_vals() = [var_and_val()]
var_and_val() = {oid(), value_type(), value()} | {oid(), value()}
value_type() = o ('OBJECT IDENTIFIER') |
               i ('INTEGER') |
               u ('Unsigned32') |
               g ('Unsigned32') |
               s ('OCTET SRING') |
               b ('BITS') |
               ip ('IpAddress') |
               op ('Opaque') |
               c32 ('Counter32') |
               c64 ('Counter64') |
               tt ('TimeTicks')
value() = term()
community() = string()
sec_model() = any | v1 | v2c | usm
sec_name() = string()
sec_level() = noAuthNoPriv | authNoPriv | authPriv
```

See also the [data types in `snmpa_conf`](`m:snmpa_conf#types`).

[](){: #monitor }
""".

%%----------------------------------------------------------------------
%% This module implements a simple SNMP manager for Erlang.
%%----------------------------------------------------------------------

%% User interface
%% Avoid warning for local function demonitor/1 clashing with autoimported BIF.
-compile({no_auto_import,[demonitor/1]}).
-export([
	 %% 
	 %% Management API
	 start/0, start/1, 
	 start_link/0, start_link/1, 
	 stop/0, stop/1,

	 monitor/0, demonitor/1, 
	 notify_started/1, cancel_notify_started/1, 

	 backup/1, 

	 load_mib/1, unload_mib/1, 
	 which_mibs/0, 
	 name_to_oid/1, oid_to_name/1, oid_to_type/1, 

	 register_user/3, register_user/4, 
	 register_user_monitor/3, register_user_monitor/4, 
	 unregister_user/1, 
	 which_users/0, 

	 register_agent/2, register_agent/3, register_agent/4, 
	 unregister_agent/2, unregister_agent/3,
	 which_agents/0, which_agents/1, 
	 agent_info/2, update_agent_info/3, update_agent_info/4, 
	 
	 register_usm_user/3, unregister_usm_user/2, 
	 which_usm_users/0, which_usm_users/1, 
	 usm_user_info/3, update_usm_user_info/4, 
	 
	 %% 
	 %% Basic SNMP API (version "3").
	 sync_get2/3, sync_get2/4, 
	 async_get2/3, async_get2/4, 
	 sync_get_next2/3, sync_get_next2/4, 
	 async_get_next2/3, async_get_next2/4, 
	 sync_set2/3, sync_set2/4, 
	 async_set2/3, async_set2/4, 
	 sync_get_bulk2/5, sync_get_bulk2/6, 
	 async_get_bulk2/5, async_get_bulk2/6, 

	 cancel_async_request/2, 
	 
	 %% 
	 %% Extended SNMP API
	 %% discovery/2, discovery/3, discovery/4, discovery/5, discovery/6, 

	 %% 
	 %% Logging
	 log_to_txt/1, log_to_txt/2, log_to_txt/3, log_to_txt/4, 
	 log_to_txt/5, log_to_txt/6, log_to_txt/7, log_to_txt/8, 
	 log_to_io/1,  log_to_io/2,  log_to_io/3,  log_to_io/4, 
	 log_to_io/5,  log_to_io/6,  log_to_io/7, 
	 change_log_size/1,
	 get_log_type/0,
	 set_log_type/1,

	 reconfigure/0,

	 system_start_time/0,
	 sys_up_time/0,

	 info/0, info/1,
	 verbosity/2,
         restart/1
	]).

-export([format_reason/1, format_reason/2]).

%% Application internal export
-export([start_link/3, snmpm_start_verify/2, snmpm_start_verify/3]).
-export([target_name/1, target_name/2]).

-export_type([
	      register_timeout/0, 
	      agent_config/0, 
	      target_name/0,
              pdu_type/0
	     ]).

-include_lib("snmp/src/misc/snmp_debug.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include("snmpm_atl.hrl").
-include("snmpm_internal.hrl").
-include("snmp_verbosity.hrl").

-define(DEFAULT_AGENT_PORT, 161).
-define(ATL_BLOCK_DEFAULT,  true).


%%-----------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------

-type register_timeout() :: pos_integer() | snmp:snmp_timer().
-type agent_config() :: {engine_id,        snmp:engine_id()}   | % Mandatory
			{address,          inet:ip_address()}  | % Mandatory
			{port,             inet:port_number()} | % Optional
			{tdomain,          snmp:tdomain()}     | % Optional
			{community,        snmp:community()}   | % Optional
			{timeout,          register_timeout()} | % Optional
			{max_message_size, snmp:mms()}         | % Optional
			{version,          snmp:version()}     | % Optional
			{sec_moduel,       snmp:sec_model()}   | % Optional
			{sec_name,         snmp:sec_name()}    | % Optional
			{sec_level,        snmp:sec_level()}.    % Optional
-type target_name() :: string().
-type pdu_type() :: snmp:pdu_type() | 'trappdu'.


%% This function is called when the snmp application
%% starts. 
-doc false.
start_link(Opts, normal, []) ->
    start_link(Opts).


simple_conf() ->
    Vsns      = [v1, v2, v3],
    {ok, Cwd} = file:get_cwd(),
    %% Check if the manager config file exist, if not create it
    MgrConf = filename:join(Cwd, "manager.conf"),
    case file:read_file_info(MgrConf) of
	{ok, _} ->
	    ok;
	_ ->
	    ok = snmp_config:write_manager_config(Cwd, "",
						  [{port, 5000},
						   {engine_id, "mgrEngine"},
						   {max_message_size, 484}])
    end,
    Conf = [{dir, Cwd}, {db_dir, Cwd}],
    [{versions, Vsns}, {config, Conf}].

%% Simple start. Start a manager with default values.
-doc false.
start_link() ->
    start_link(simple_conf()).

%% This function is normally not used. Instead the manager is
%% started as a consequence of a call to application:start(snmp)
%% when {snmp, [{manager, Options}]} is present in the
%% node config file.
-doc false.
start_link(Opts) ->
    %% This start the manager top supervisor, which in turn
    %% starts the other processes.
    {ok, _} = snmpm_supervisor:start_link(normal, Opts),
    ok.

%% Simple start. Start a manager with default values.
-doc false.
start() ->
    start(simple_conf()).
    
-doc false.
start(Opts) ->
    %% This start the manager top supervisor, which in turn
    %% starts the other processes.
    {ok, Pid} = snmpm_supervisor:start_link(normal, Opts),
    unlink(Pid),
    ok.

-doc false.
stop() ->
    stop(0).

-doc false.
stop(Timeout) when (Timeout =:= infinity) orelse
                   (is_integer(Timeout) andalso (Timeout >= 0)) ->
    snmpm_supervisor:stop(Timeout).



-doc """
monitor() -> Ref

Monitor the SNMP manager. In case of a crash, the calling (monitoring) process
will get a 'DOWN' message (see the erlang module for more info).

[](){: #demonitor }
""".
monitor() ->
    erlang:monitor(process, snmpm_supervisor).

-doc """
demonitor(Ref) -> void()

Turn off monitoring of the SNMP manager.

[](){: #notify_started }
""".
demonitor(Ref) ->
    erlang:demonitor(Ref).
	

-define(NOTIFY_START_TICK_TIME, 500).

-doc """
notify_started(Timeout) -> Pid

Request a notification (message) when the SNMP manager has started.

The `Timeout` is the time the request is valid. The value has to be greater then
zero.

The `Pid` is the process handling the supervision of the SNMP manager start.
When the manager has started a completion message will be sent to the client
from this process: `{snmpm_started, Pid}`. If the SNMP manager was not started
in time, a timeout message will be sent to the client:
`{snmpm_start_timeout, Pid}`.

A client application that is dependent on the SNMP manager will use this
function in order to be notified of when the manager has started. There are two
situations when this is useful:

- During the start of a system, when a client application _could_ start prior to
  the SNMP manager but is dependent upon it, and therefore has to wait for it to
  start.
- When the SNMP manager has crashed, the dependent client application has to
  wait for the SNMP manager to be restarted before it can _reconnect_.

The function returns the pid() of a handler process, that does the supervision
on behalf of the client application. Note that the client application is linked
to this handler.

This function is used in conjunction with the monitor function.

[](){: #cancel_notify_started }
""".
notify_started(To) when is_integer(To) andalso (To > 0) ->
    spawn_link(?MODULE, snmpm_start_verify, [self(), To]).

-doc """
cancel_notify_started(Pid) -> void()

Cancel a previous request to be notified of SNMP manager start.

[](){: #register_user }
""".
cancel_notify_started(Pid) ->
    Pid ! {cancel, self()},
    ok.

-doc false.
snmpm_start_verify(Parent, To) ->
    ?d("starting", []),
    snmpm_start_verify(Parent, monitor(), To).

-doc false.
snmpm_start_verify(Parent, _Ref, To) when (To =< 0) ->
    ?d("timeout", []),
    unlink(Parent),
    Parent ! {snmpm_start_timeout, self()};
snmpm_start_verify(Parent, Ref, To) ->
    T0 = t(),
    receive
	{cancel, Parent} ->
	    ?d("cancel", []),
	    demonitor(Ref),
	    unlink(Parent),
	    exit(normal);
	{'EXIT', Parent, _} ->
	    exit(normal);
	{'DOWN', Ref, process, _Object, _Info} ->
	    ?d("down", []),
	    sleep(?NOTIFY_START_TICK_TIME),
	    ?MODULE:snmpm_start_verify(Parent, monitor(), t(T0, To))
    after ?NOTIFY_START_TICK_TIME ->
	    ?d("down timeout", []),
	    demonitor(Ref),
	    case snmpm_server:is_started() of
		true ->
		    unlink(Parent),
		    Parent ! {snmpm_started, self()};
		_ ->
		    ?MODULE:snmpm_start_verify(Parent, monitor(), t(T0, To))
	    end
    end.

t(T0, T)  -> T - (t() - T0).
t()       -> snmp_misc:now(ms).
sleep(To) -> snmp_misc:sleep(To).


%% -- Misc --

-doc """
backup(BackupDir) -> ok | {error, Reason}

Backup persistent data handled by the manager.

BackupDir cannot be identical to DbDir.

[](){: #info }
""".
backup(BackupDir) ->
    snmpm_config:backup(BackupDir).


%% -- Mibs --

%% Load a mib into the manager
-doc """
load_mib(Mib) -> ok | {error, Reason}

Load a `Mib` into the manager. The `MibName` is the name of the Mib, including
the path to where the compiled mib is found. For example,

```erlang
          Dir = code:priv_dir(my_app) ++ "/mibs/",
          snmpm:load_mib(Dir ++ "MY-MIB").
```

[](){: #unload_mib }
""".
load_mib(MibFile) ->
    snmpm_server:load_mib(MibFile).

%% Unload a mib from the manager
-doc """
unload_mib(Mib) -> ok | {error, Reason}

Unload a `Mib` from the manager. The `MibName` is the name of the Mib, including
the path to where the compiled mib is found. For example,

```erlang
          Dir = code:priv_dir(my_app) ++ "/mibs/",
          snmpm:unload_mib(Dir ++ "MY-MIB").
```

[](){: #which_mibs }
""".
unload_mib(Mib) ->
    snmpm_server:unload_mib(Mib).

%% Which mib's are loaded
-doc """
which_mibs() -> Mibs

Get a list of all the mib's loaded into the manager.

[](){: #name_to_oid }
""".
which_mibs() ->
    snmpm_config:which_mibs().

%% Get all the possible oid's for the aliasname
-doc """
name_to_oid(Name) -> {ok, Oids} | {error, Reason}

Transform a alias-name to its oid.

Note that an alias-name is only unique within the mib, so when loading several
mib's into a manager, there might be several instances of the same aliasname.

[](){: #oid_to_name }
""".
name_to_oid(Name) ->
    snmpm_config:name_to_oid(Name).

%% Get the aliasname for an oid
-doc """
oid_to_name(Oid) -> {ok, Name} | {error, Reason}

Transform a oid to its aliasname.

[](){: #oid_to_type }
""".
oid_to_name(Oid) ->
    snmpm_config:oid_to_name(Oid).

%% Get the type for an oid
-doc """
oid_to_type(Oid) -> {ok, Type} | {error, Reason}

Retrieve the type (asn1 bertype) of an oid.

[](){: #backup }
""".
oid_to_type(Oid) ->
    snmpm_config:oid_to_type(Oid).


%% -- Info -- 

-doc """
info() -> [{Key, Value}]

Returns a list (a dictionary) containing information about the manager.
Information includes statistics counters, miscellaneous info about each process
(e.g. memory allocation), and so on.

[](){: #verbosity }
""".
info() ->
    snmpm_server:info().

-doc false.
info(Key) ->
    proplists:get_value(Key, info(), {error, not_found}).


%% -- Verbosity -- 

%% Change the verbosity of a process in the manager
-doc """
verbosity(Ref, Verbosity) -> void()

Sets verbosity for the designated process. For the lowest verbosity `silence`,
nothing is printed. The higher the verbosity, the more is printed.

[](){: #restart }
""".
verbosity(config, V) ->
    snmpm_config:verbosity(V);
verbosity(server, V) ->
    snmpm_server:verbosity(V);
verbosity(net_if, V) ->
    snmpm_server:verbosity(net_if, V);
verbosity(note_store, V) ->
    snmpm_server:verbosity(note_store, V);
verbosity(all, V) ->
    snmpm_config:verbosity(V),
    snmpm_server:verbosity(V),
    snmpm_server:verbosity(net_if, V),
    snmpm_server:verbosity(note_store, V).


%% -- Restart -- 

%% Restart various component processes in the manager
%% Note that the effects of this is diffiult to
%% predict, so it should be use with *caution*!

-doc """
restart(Ref) -> void()

Restart the indicated process (`Ref`). Note that its not without risk to restart
a process, and should therefore be used with care.

[](){: #format_reason }
""".
-doc(#{since => <<"OTP 22.3">>}).
restart(net_if = What) ->
    snmpm_server:restart(What).


%% -- Users --

%% Register the 'user'. 
%% The manager entity responsible for a specific agent. 
%% Module is the callback module (snmpm_user behaviour) which 
%% will be called whenever something happens (detected 
%% agent, incoming reply or incoming trap/notification).
%% Note that this could have already been done as a 
%% consequence of the node config.
-doc(#{equiv => register_user/4}).
register_user(Id, Module, Data) ->
    register_user(Id, Module, Data, []).

%% Default config for agents registered by this user
-doc """
register_user(Id, Module, Data, DefaultAgentConfig) -> ok | {error, Reason}

Register the manager entity (=user) responsible for specific agent(s).

`Module` is the callback module (snmpm_user behaviour) which will be called
whenever something happens (detected agent, incoming reply or incoming
trap/notification). Note that this could have already been done as a consequence
of the node config. (see users.conf).

The argument `DefaultAgentConfig` is used as default values when this user
register agents.

The type of `Val` depends on `Item`:

```text
community = string()
timeout = integer() | snmp_timer()
max_message_size = integer()
version = v1 | v2 | v3
sec_model = any | v1 | v2c | usm
sec_name = string()
sec_level = noAuthNoPriv | authNoPriv | authPriv
```

[](){: #register_user_monitor }
""".
register_user(Id, Module, Data, DefaultAgentConfig) ->
    snmpm_server:register_user(Id, Module, Data, DefaultAgentConfig).

-doc(#{equiv => register_user_monitor/4}).
register_user_monitor(Id, Module, Data) ->
    register_user_monitor(Id, Module, Data, []).

-doc """
register_user_monitor(Id, Module, Data, DefaultAgentConfig) -> ok | {error,
Reason}

Register the monitored manager entity (=user) responsible for specific agent(s).

The process performing the registration will be monitored. Which means that if
that process should die, all agents registered by that user process will be
unregistered. All outstanding requests will be canceled.

`Module` is the callback module (snmpm_user behaviour) which will be called
whenever something happens (detected agent, incoming reply or incoming
trap/notification). Note that this could have already been done as a consequence
of the node config. (see users.conf).

The argument `DefaultAgentConfig` is used as default values when this user
register agents.

The type of `Val` depends on `Item`:

```text
community = string()
timeout = integer() | snmp_timer()
max_message_size = integer()
version = v1 | v2 | v3
sec_model = any | v1 | v2c | usm
sec_name = string()
sec_level = noAuthNoPriv | authNoPriv | authPriv
```

[](){: #unregister_user }
""".
register_user_monitor(Id, Module, Data, DefaultAgentConfig) ->
    snmpm_server:register_user_monitor(Id, Module, Data, DefaultAgentConfig).

-doc """
unregister_user(Id) -> ok | {error, Reason}

Unregister the user.

[](){: #which_users }
""".
unregister_user(Id) ->
    snmpm_server:unregister_user(Id).

-doc """
which_users() -> Users

Get a list of the identities of all registered users.

[](){: #register_agent }
""".
which_users() ->
    snmpm_config:which_users().


%% -- Agents --

%% Explicitly instruct the manager to handle this agent.
%% Called to instruct the manager that this agent 
%% shall be handled. These functions is used when
%% the user know's in advance which agents the
%% manager shall handle.
%% Note that there is an alternate way to do the same thing:
%% Add the agent to the manager config files.
%% 
%% UserId     -> Id of the user responsible for this agent: term()
%% TargetName -> Unique name for the agent: (string())
%% Config     -> Agent configuration: [config()]

do_register_agent(UserId, TargetName, Config) ->
    snmpm_config:register_agent(UserId, TargetName, Config).

-doc """
register_agent(UserId, TargetName, Config) -> ok | {error, Reason}

Explicitly instruct the manager to handle this agent, with `UserId` as the
responsible user.

Called to instruct the manager that this agent shall be handled. This function
is used when the user knows in advance which agents the manager shall handle.
Note that there is an alternate way to do the same thing: Add the agent to the
manager config files (see [agents.conf](snmp_manager_config_files.md#agents)).

`TargetName` is a non-empty string, uniquely identifying the agent.

The type of `Val` depends on `Item`:

```text
[mandatory] engine_id = string()
[mandatory] tadress = transportAddress()  % Depends on tdomain
[optional]  port = inet:port_number()
[optional]  tdomain = transportDomain()
[optional]  community = string()
[optional]  timeout = integer() | snmp_timer()
[optional]  max_message_size = integer()
[optional]  version = v1 | v2 | v3
[optional]  sec_model = any | v1 | v2c | usm
[optional]  sec_name = string()
[optional]  sec_level = noAuthNoPriv | authNoPriv | authPriv
```

Note that if no `tdomain` is given, the default value, `transportDomainUdpIpv4`,
is used.

Note that if no `port` is given and if `taddress` does not contain a port
number, the default value is used.

[](){: #unregister_agent }
""".
register_agent(UserId, TargetName, Config) 
  when (is_list(TargetName) andalso 
	(length(TargetName) > 0) andalso 
	is_list(Config)) ->
    do_register_agent(UserId, TargetName, [{reg_type, target_name} | Config]);

%% Backward compatibility 
%% Note that the agent engine id is a mandatory config option,
%% so this function *will* fail!
register_agent(UserId, Addr, Port) when is_integer(Port) ->
    register_agent(UserId, Addr, Port, []);

%% Backward compatibility 
register_agent(UserId, Addr, Config) when is_list(Config) ->
    register_agent(UserId, Addr, ?DEFAULT_AGENT_PORT, Config).

%% Backward compatibility 
%% Note that the agent engine id is a mandatory config option,
%% so this function *will* fail!
-doc false.
register_agent(UserId, Addr) ->
    register_agent(UserId, Addr, ?DEFAULT_AGENT_PORT, []).

%% Backward compatibility 
-doc false.
register_agent(UserId, Domain, Addr, Config0) when is_atom(Domain) ->
    case lists:keymember(target_name, 1, Config0) of
	false ->
	    TargetName = mk_target_name(Domain, Addr, Config0),
	    Config =
		[{reg_type, addr_port},
		 {tdomain, Domain}, {taddress, Addr} | Config0],
	    do_register_agent(UserId, TargetName, ensure_engine_id(Config));
	true ->
	    {value, {_, TargetName}} = 
		lists:keysearch(target_name, 1, Config0),
	    Config1 = lists:keydelete(target_name, 1, Config0),
	    Config2 =
		[{reg_type, addr_port},
		 {tdomain, Domain}, {taddress, Addr} | Config1],
	    register_agent(UserId, TargetName, ensure_engine_id(Config2))
    end;
register_agent(UserId, Ip, Port, Config) when is_integer(Port) ->
    Domain = snmpm_config:default_transport_domain(),
    Addr =
	case snmp_conf:check_address(Domain, {Ip, Port}) of
	    ok ->
		{Ip, Port};
	    {ok, FixedAddr} ->
		FixedAddr
	end,
    register_agent(UserId, Domain, Addr, Config).

-doc """
unregister_agent(UserId, TargetName) -> ok | {error, Reason}

Unregister the agent.

[](){: #agent_info }
""".
unregister_agent(UserId, TargetName) when is_list(TargetName) ->
    snmpm_config:unregister_agent(UserId, TargetName);

%% Backward compatibility functions
unregister_agent(UserId, Addr) ->
    unregister_agent(UserId, Addr, ?DEFAULT_AGENT_PORT).

-doc false.
unregister_agent(UserId, DomainIp, AddressPort) ->
    case target_name(DomainIp, AddressPort) of
	{ok, TargetName} ->
	    unregister_agent(UserId, TargetName);
	Error ->
	    Error
    end.


-doc """
agent_info(TargetName, Item) -> {ok, Val} | {error, Reason}

Retrieve agent config.

[](){: #update_agent_info }
""".
agent_info(TargetName, Item) ->
    snmpm_config:agent_info(TargetName, Item).

-doc(#{equiv => update_agent_info/4}).
-doc(#{since => <<"OTP R14B04">>}).
update_agent_info(UserId, TargetName, Info) when is_list(Info) ->
    snmpm_config:update_agent_info(UserId, TargetName, Info).

-doc """
update_agent_info(UserId, TargetName, Item, Val) -> ok | {error, Reason}

Update agent config. The function [`update_agent_info/3`](`update_agent_info/3`)
should be used when several values needs to be updated atomically.

See function [register_agent](`m:snmpm#register_agent`) for more info about what
kind of items are allowed.

[](){: #which_agents }
""".
-doc(#{since => <<"OTP R14B04">>}).
update_agent_info(UserId, TargetName, Item, Val) ->
    update_agent_info(UserId, TargetName, [{Item, Val}]).


-doc(#{equiv => which_agents/1}).
which_agents() ->
    snmpm_config:which_agents().

-doc """
which_agents(UserId) -> Agents

Get a list of all registered agents or all agents registered by a specific user.

[](){: #register_usm_user }
""".
which_agents(UserId) ->
    snmpm_config:which_agents(UserId).


%% -- USM users --

-doc """
register_usm_user(EngineID, UserName, Conf) -> ok | {error, Reason}

Explicitly instruct the manager to handle this USM user. Note that there is an
alternate way to do the same thing: Add the usm user to the manager config files
(see [usm.conf](snmp_manager_config_files.md#usm_user)).

The type of `Val` depends on `Item`:

```text
sec_name = string()
auth = usmNoAuthProtocol | usmHMACMD5AuthProtocol | usmHMACSHAAuthProtocol | usmHMAC128SHA224AuthProtocol | usmHMAC192SH256AuthProtocol | usmHMAC256SHA384AuthProtocol | usmHMAC384SHA512AuthProtocol
auth_key = [integer()]   (length 16 if auth = usmHMACMD5AuthProtocol,
                          length 20 if auth = usmHMACSHAAuthProtocol,
                          length 28 if auth = usmHMAC128SHA224AuthProtocol,
                          length 32 if auth = usmHMAC192SHA256AuthProtocol,
                          length 48 if auth = usmHMAC256SHA384AuthProtocol,
                          length 64 if auth = usmHMAC384SHA512AuthProtocol)
priv = usmNoPrivProtocol | usmDESPrivProtocol | usmAesCfb128Protocol
priv_key = [integer()]   (length is 16 if priv = usmDESPrivProtocol | usmAesCfb128Protocol).
```

[](){: #unregister_usm_user }
""".
register_usm_user(EngineID, UserName, Conf) 
  when is_list(EngineID) andalso is_list(UserName) andalso is_list(Conf) ->
    snmpm_config:register_usm_user(EngineID, UserName, Conf).

-doc """
unregister_usm_user(EngineID, UserName) -> ok | {error, Reason}

Unregister this USM user.

[](){: #which_usm_users1 }
""".
unregister_usm_user(EngineID, UserName) 
  when is_list(EngineID) andalso is_list(UserName) ->
    snmpm_config:unregister_usm_user(EngineID, UserName).

-doc """
usm_user_info(EngineID, UserName, Item) -> {ok, Val} | {error, Reason}

Retrieve usm user config.

[](){: #update_usm_user_info }
""".
usm_user_info(EngineID, UserName, Item) 
  when is_list(EngineID) andalso is_list(UserName) andalso is_atom(Item) ->
    snmpm_config:usm_user_info(EngineID, UserName, Item).

-doc """
update_usm_user_info(EngineID, UserName, Item, Val) -> ok | {error, Reason}

Update usm user config.

[](){: #which_usm_users }
""".
update_usm_user_info(EngineID, UserName, Item, Val) 
  when is_list(EngineID) andalso is_list(UserName) andalso is_atom(Item) ->
    snmpm_config:update_usm_user_info(EngineID, UserName, Item, Val).

-doc """
which_usm_users() -> UsmUsers

Get a list of all registered usm users.

[](){: #which_usm_users2 }
""".
which_usm_users() ->
    snmpm_config:which_usm_users().

-doc """
which_usm_users(EngineID) -> UsmUsers

Get a list of all registered usm users with engine-id `EngineID`.

[](){: #sync_get2 }
""".
which_usm_users(EngineID) when is_list(EngineID) ->
    snmpm_config:which_usm_users(EngineID).


%% -- Discovery --

%% Start a discovery process
%% discovery(UserId, BAddr) ->
%%     snmpm_server:discovery(UserId, BAddr).

%% discovery(UserId, BAddr, ExpireOrConfig) ->
%%     snmpm_server:discovery(UserId, BAddr, ExpireOrConfig).

%% discovery(UserId, BAddr, Config, Expire) ->
%%     snmpm_server:discovery(UserId, BAddr, Config, Expire).

%% discovery(UserId, BAddr, Port, Config, Expire) ->
%%     snmpm_server:discovery(UserId, BAddr, Port, Config, Expire).

%% discovery(UserId, BAddr, Port, Config, Expire, ExtraInfo) ->
%%     snmpm_server:discovery(UserId, BAddr, Port, Config, Expire, ExtraInfo).


%% -- Requests --

%% --- synchronous get-request ---
%% 

-doc(#{equiv => sync_get2/4}).
-doc(#{since => <<"OTP R14B03">>}).
sync_get2(UserId, TargetName, Oids) ->
    sync_get2(UserId, TargetName, Oids, []).

-doc """
sync_get2(UserId, TargetName, Oids, SendOpts) -> {ok, SnmpReply, Remaining} |
{error, Reason}

Synchronous `get-request`.

`Remaining` is the remaining time of the given (or default) timeout time.

When _Reason_ is _\{send_failed, ...\}_ it means that the net_if process failed
to send the message. This could happen because of any number of reasons, i.e.
encoding error. _ActualReason_ is the actual reason in this case.

The send option `extra` specifies an opaque data structure passed on to the
net-if process. The net-if process included in this application makes, with one
exception, no use of this info, so the only use for it in such a option (when
using the built in net-if) would be tracing. The one usage exception is: _Any_
tuple with `snmpm_extra_info_tag` as its first element is reserved for internal
use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

For `SnmpInfo`, see the user callback function
[handle_report](`m:snmpm_user#handle_report`).

[](){: #async_get2 }
""".
-doc(#{since => <<"OTP R14B03">>}).
sync_get2(UserId, TargetName, Oids, SendOpts) 
  when is_list(Oids) andalso is_list(SendOpts) ->
    snmpm_server:sync_get(UserId, TargetName, Oids, SendOpts).


%% --- asynchronous get-request ---
%% 
%% The reply will be delivered to the user
%% through a call to handle_pdu/5
%% 

-doc(#{equiv => async_get2/4}).
-doc(#{since => <<"OTP R14B03">>}).
async_get2(UserId, TargetName, Oids) ->
    async_get2(UserId, TargetName, Oids, []).

-doc """
async_get2(UserId, TargetName, Oids, SendOpts) -> {ok, ReqId} | {error, Reason}

Asynchronous `get-request`.

The reply, if it arrives, will be delivered to the user through a call to the
snmpm_user callback function `handle_pdu`.

The send option `timeout` specifies for how long the request is valid (after
which the manager is free to delete it).

The send option `extra` specifies an opaque data structure passed on to the
net-if process. The net-if process included in this application makes, with one
exception, no use of this info, so the only use for it in such a option (when
using the built in net-if) would be tracing. The one usage exception is: _Any_
tuple with `snmpm_extra_info_tag` as its first element is reserved for internal
use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

[](){: #sync_get_next2 }
""".
-doc(#{since => <<"OTP R14B03">>}).
async_get2(UserId, TargetName, Oids, SendOpts) 
  when is_list(Oids) andalso is_list(SendOpts) ->
    snmpm_server:async_get(UserId, TargetName, Oids, SendOpts).


%% --- synchronous get_next-request ---
%% 

-doc(#{equiv => sync_get_next2/4}).
-doc(#{since => <<"OTP R14B03">>}).
sync_get_next2(UserId, TargetName, Oids) ->
    sync_get_next2(UserId, TargetName, Oids, []).

-doc """
sync_get_next2(UserId, TargetName, Oids, SendOpts) -> {ok, SnmpReply, Remaining}
| {error, Reason}

Synchronous `get-next-request`.

`Remaining` is the remaining time of the given (or default) timeout time.

When _Reason_ is _\{send_failed, ...\}_ it means that the net_if process failed
to send the message. This could happen because of any number of reasons, i.e.
encoding error. _ActualReason_ is the actual reason in this case.

The send option `extra` specifies an opaque data structure passed on to the
net-if process. The net-if process included in this application makes, with one
exception, no use of this info, so the only use for it in such a option (when
using the built in net-if) would be tracing. The one usage exception is: _Any_
tuple with `snmpm_extra_info_tag` as its first element is reserved for internal
use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

For `SnmpInfo`, see the user callback function
[handle_report](`m:snmpm_user#handle_report`).

[](){: #async_get_next2 }
""".
-doc(#{since => <<"OTP R14B03">>}).
sync_get_next2(UserId, TargetName, Oids, SendOpts) 
  when is_list(Oids) andalso is_list(SendOpts) ->
    snmpm_server:sync_get_next(UserId, TargetName, Oids, SendOpts).


%% --- asynchronous get_next-request ---
%% 

-doc(#{equiv => async_get_next2/4}).
-doc(#{since => <<"OTP R14B03">>}).
async_get_next2(UserId, TargetName, Oids) ->
    async_get_next2(UserId, TargetName, Oids, []).

-doc """
async_get_next2(UserId, TargetName, Oids, SendOpts) -> {ok, ReqId} | {error,
Reason}

Asynchronous `get-next-request`.

The reply will be delivered to the user through a call to the snmpm_user
callback function `handle_pdu`.

The send option `timeout` specifies for how long the request is valid (after
which the manager is free to delete it).

The send option `extra` specifies an opaque data structure passed on to the
net-if process. The net-if process included in this application makes, with one
exception, no use of this info, so the only use for it in such a option (when
using the built in net-if) would be tracing. The one usage exception is: _Any_
tuple with `snmpm_extra_info_tag` as its first element is reserved for internal
use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

[](){: #sync_set2 }
""".
-doc(#{since => <<"OTP R14B03">>}).
async_get_next2(UserId, TargetName, Oids, SendOpts) 
  when is_list(Oids) andalso is_list(SendOpts) ->
    snmpm_server:async_get_next(UserId, TargetName, Oids, SendOpts).


%% --- synchronous set-request ---
%% 

-doc(#{equiv => sync_set2/4}).
-doc(#{since => <<"OTP R14B03">>}).
sync_set2(UserId, TargetName, VarsAndVals) ->
    sync_set2(UserId, TargetName, VarsAndVals, []).

-doc """
sync_set2(UserId, TargetName, VarsAndVals, SendOpts) -> {ok, SnmpReply,
Remaining} | {error, Reason}

Synchronous `set-request`.

`Remaining` is the remaining time of the given (or default) timeout time.

When _Reason_ is _\{send_failed, ...\}_ it means that the net_if process failed
to send the message. This could happen because of any number of reasons, i.e.
encoding error. _ActualReason_ is the actual reason in this case.

When _var_and_val()_ is _\{oid(), value()\}_, the manager makes an educated
guess based on the loaded mibs.

The send option `extra` specifies an opaque data structure passed on to the
net-if process. The net-if process included in this application makes, with one
exception, no use of this info, so the only use for it in such a option (when
using the built in net-if) would be tracing. The one usage exception is: _Any_
tuple with `snmpm_extra_info_tag` as its first element is reserved for internal
use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

For `SnmpInfo`, see the user callback function
[handle_report](`m:snmpm_user#handle_report`).

[](){: #async_set2 }
""".
-doc(#{since => <<"OTP R14B03">>}).
sync_set2(UserId, TargetName, VarsAndVals, SendOpts) 
  when is_list(VarsAndVals) andalso is_list(SendOpts) ->
    snmpm_server:sync_set(UserId, TargetName, VarsAndVals, SendOpts).


%% --- asynchronous set-request ---
%% 

-doc(#{equiv => async_set2/4}).
-doc(#{since => <<"OTP R14B03">>}).
async_set2(UserId, TargetName, VarsAndVals) ->
    async_set2(UserId, TargetName, VarsAndVals, []).

-doc """
async_set2(UserId, TargetName, VarsAndVals, SendOpts) -> {ok, ReqId} | {error,
Reason}

Asynchronous `set-request`.

The reply will be delivered to the user through a call to the snmpm_user
callback function `handle_pdu`.

The send option `timeout` specifies for how long the request is valid (after
which the manager is free to delete it).

When _var_and_val()_ is _\{oid(), value()\}_, the manager makes an educated
guess based on the loaded mibs.

The send option `extra` specifies an opaque data structure passed on to the
net-if process. The net-if process included in this application makes, with one
exception, no use of this info, so the only use for it in such a option (when
using the built in net-if) would be tracing. The one usage exception is: _Any_
tuple with `snmpm_extra_info_tag` as its first element is reserved for internal
use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

[](){: #sync_get_bulk2 }
""".
-doc(#{since => <<"OTP R14B03">>}).
async_set2(UserId, TargetName, VarsAndVals, SendOpts) 
  when is_list(VarsAndVals) andalso is_list(SendOpts) ->
    snmpm_server:async_set(UserId, TargetName, VarsAndVals, SendOpts).


%% --- synchronous get-bulk ---
%% 

-doc(#{equiv => sync_get_bulk2/6}).
-doc(#{since => <<"OTP R14B03">>}).
sync_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids) ->
    sync_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, []).

-doc """
sync_get_bulk2(UserId, TragetName, NonRep, MaxRep, Oids, SendOpts) -> {ok,
SnmpReply, Remaining} | {error, Reason}

Synchronous `get-bulk-request` (See RFC1905).

`Remaining` is the remaining time of the given (or default) timeout time.

When _Reason_ is _\{send_failed, ...\}_ it means that the net_if process failed
to send the message. This could happen because of any number of reasons, i.e.
encoding error. _ActualReason_ is the actual reason in this case.

The send option `extra` specifies an opaque data structure passed on to the
net-if process. The net-if process included in this application makes, with one
exception, no use of this info, so the only use for it in such a option (when
using the built in net-if) would be tracing. The one usage exception is: _Any_
tuple with `snmpm_extra_info_tag` as its first element is reserved for internal
use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

For `SnmpInfo`, see the user callback function
[handle_report](`m:snmpm_user#handle_report`).

[](){: #async_get_bulk2 }
""".
-doc(#{since => <<"OTP R14B03">>}).
sync_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, SendOpts) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) andalso 
       is_list(SendOpts) ->
    snmpm_server:sync_get_bulk(UserId, TargetName, 
                               NonRep, MaxRep, Oids, SendOpts).


%% --- asynchronous get-bulk ---
%% 

-doc(#{equiv => async_get_bulk2/6}).
-doc(#{since => <<"OTP R14B03">>}).
async_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids) ->
    async_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, []).

-doc """
async_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, SendOpts) -> {ok,
ReqId} | {error, Reason}

Asynchronous `get-bulk-request` (See RFC1905).

The reply will be delivered to the user through a call to the snmpm_user
callback function `handle_pdu`.

The send option `timeout` specifies for how long the request is valid (after
which the manager is free to delete it).

The send option `extra` specifies an opaque data structure passed on to the
net-if process. The net-if process included in this application makes no use of
this info, so the only use for it in such a configuration (when using the built
in net-if) would be tracing.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

[](){: #cancel_async_request }
""".
-doc(#{since => <<"OTP R14B03">>}).
async_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, SendOpts) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) andalso 
       is_list(SendOpts) ->
    snmpm_server:async_get_bulk(UserId, TargetName, 
                                NonRep, MaxRep, Oids, SendOpts).



-doc """
cancel_async_request(UserId, ReqId) -> ok | {error, Reason}

Cancel a previous asynchronous request.

[](){: #log_to_txt }
""".
cancel_async_request(UserId, ReqId) ->
    snmpm_server:cancel_async_request(UserId, ReqId).


%%%-----------------------------------------------------------------
%%% Audit Trail Log functions (for backward compatibility)
%%%-----------------------------------------------------------------

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R16B03">>}).
-spec log_to_txt(LogDir :: snmp:dir()) ->
    snmp:void().

log_to_txt(LogDir) ->
    log_to_txt(LogDir, []). 

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R16B03">>}).
-spec log_to_txt(LogDir :: snmp:dir(), 
		 Block  :: boolean()) ->
    snmp:void();
                (LogDir :: snmp:dir(), 
		 Mibs   :: [snmp:mib_name()]) ->
    snmp:void().

log_to_txt(LogDir, Block) 
  when ((Block =:= true) orelse (Block =:= false)) ->
    Mibs    = [], 
    OutFile = "snmpm_log.txt",       
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs) ->
    Block   = ?ATL_BLOCK_DEFAULT, 
    OutFile = "snmpm_log.txt",       
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R16B03">>}).
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
    OutFile = "snmpm_log.txt",       
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile) ->
    Block   = ?ATL_BLOCK_DEFAULT, 
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).

-doc(#{equiv => log_to_txt/8}).
-doc(#{since => <<"OTP R16B03">>}).
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
-doc(#{since => <<"OTP R16B03">>}).
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
-doc(#{since => <<"OTP R16B03">>}).
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
-doc(#{since => <<"OTP R16B03">>}).
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
"./snmpm_log.txt". `LogName` defaults to "snmpm_log". `LogFile` defaults to
"snmpm.log".

The `Block` argument indicates if the log should be blocked during conversion.
This could be useful when converting large logs (when otherwise the log could
wrap during conversion). Defaults to `true`.

See [snmp:log_to_txt](`m:snmp#log_to_txt`) for more info.

[](){: #log_to_io }
""".
-doc(#{since => <<"OTP R16B03">>}).
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
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile).

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
`LogName` defaults to "snmpm_log". `LogFile` defaults to "snmpm.log".

The `Block` argument indicates if the log should be blocked during conversion.
This could be useful when converting large logs (when otherwise the log could
wrap during conversion). Defaults to `true`.

See [snmp:log_to_io](`m:snmp#log_to_io`) for more info.

[](){: #change_log_size }
""".
-doc(#{since => <<"OTP R15B01,OTP R16B03">>}).
log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop) -> 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop).
    

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
    LogName = ?audit_trail_log_name, 
    snmp:change_log_size(LogName, NewSize).


-doc false.
get_log_type() ->
    snmpm_server:get_log_type().

%% NewType -> atl_type()
-doc """
set_log_type(NewType) -> {ok, OldType} | {error, Reason}

Changes the run-time Audit Trail log type.

Note that this has no effect on the application configuration as defined by
configuration files, so a node restart will revert the config to whatever is in
those files.

This function is primarily useful in testing/debugging scenarios.

[](){: #load_mib }
""".
set_log_type(NewType) ->
    snmpm_server:set_log_type(NewType).


-doc false.
reconfigure() ->
    snmpm_server:reconfigure().


%%%-----------------------------------------------------------------

-doc false.
system_start_time() ->
    {ok, Time} = snmpm_config:system_start_time(),
    Time.

-doc false.
sys_up_time() ->
    % time in 0.01 seconds.
    StartTime = system_start_time(),
    (snmp_misc:now(cs) - StartTime) rem (2 bsl 31).


%%%-----------------------------------------------------------------
%%% This is just some simple utility functions to create a pretty-
%%% printable string of the error reason received from either:
%%% 
%%%    * If any of the sync/async get/get-next/set/get-bulk
%%%      returns {error, Reason} 
%%%    * The Reason parameter in the handle_error user callback 
%%%      function
%%% 
%%%-----------------------------------------------------------------

-doc(#{equiv => format_reason/2}).
format_reason(Reason) ->
    format_reason("", Reason).

-doc """
format_reason(Prefix, Reason) -> string()

This utility function is used to create a formatted (pretty printable) string of
the error reason received from either:

- The `Reason` returned value if any of the sync/async get/get-next/set/get-bulk
  functions returns `{error, Reason}`
- The `Reason` parameter in the [handle_error](`m:snmpm_user#handle_error`) user
  callback function.

`Prefix` should either be an indentation string (e.g. a list of spaces) or a
positive integer (which will be used to create the indentation string of that
length).
""".
format_reason(Prefix, Reason) when is_integer(Prefix) andalso (Prefix >= 0) ->
    format_reason(lists:duplicate(Prefix, $ ), Reason);
format_reason(Prefix, Reason) when is_list(Prefix) ->
    case (catch do_format_reason(Prefix, Reason)) of
	FL when is_list(FL) ->
	    FL;
	_ ->
	    %% Crap, try it without any fancy formatting
	    case (catch io_lib:format("~sInternal manager error: ~n"
				      "~s   ~p~n", 
				      [Prefix, Prefix, Reason])) of
		L1 when is_list(L1) ->
		    lists:flatten(L1);
		_ ->
		    %% Really crap, try it without the prefix
		    case (catch io_lib:format("Internal manager error: ~n"
					      "   ~p~n", 
					      [Reason])) of
			L2 when is_list(L2) ->
			    lists:flatten(L2);
			_ ->
			    %% Ok, I give up
			    "Illegal input. Unable to format error reason"
		    end
	    end
    end.
		    

do_format_reason(Prefix, {failed_generating_response, {RePdu, Reason}}) ->
    FmtPdu = format_pdu(Prefix ++ "   ", RePdu),
    lists:flatten(io_lib:format("~sFailed generating response: ~n"
				"~s"
				"~s   ~p~n", 
				[Prefix, FmtPdu, Prefix, Reason]));
do_format_reason(Prefix, {failed_processing_message, Reason})  ->
    lists:flatten(io_lib:format("~sFailed processing message: ~n"
				"~s   ~p~n", 
				[Prefix, Prefix, Reason]));
do_format_reason(Prefix, {unexpected_pdu, SnmpInfo})  ->
    FmtSnmpInfo = format_snmp_info(Prefix ++ "   ", SnmpInfo),
    lists:flatten(io_lib:format("~sUnexpected PDU: ~n~s", 
				[Prefix, FmtSnmpInfo]));
do_format_reason(Prefix, {send_failed, ReqId, Reason})  ->
    lists:flatten(io_lib:format("~sSend failed: ~n"
				"~s   Request id: ~w~n"
				"~s   Reason:     ~p~n", 
				[Prefix, Prefix, ReqId, Prefix, Reason]));
do_format_reason(Prefix, {invalid_sec_info, SecInfo, SnmpInfo})  ->
    FmtSecInfo  = format_sec_info(Prefix ++ "   ", SecInfo),
    FmtSnmpInfo = format_snmp_info(Prefix ++ "   ", SnmpInfo),
    lists:flatten(io_lib:format("~sInvalid security info: ~n"
				"~s"
				"~s", 
				[Prefix, FmtSecInfo, FmtSnmpInfo]));
do_format_reason(Prefix, Reason)  ->
    lists:flatten(io_lib:format("~sInternal manager error: ~n"
				"~s   ~p~n", [Prefix, Prefix, Reason])).

format_pdu(Prefix, #pdu{type         = Type,
			request_id   = ReqId,
			error_status = ES,
			error_index  = EI,
			varbinds     = VBs}) ->
    FmtPdyType   = format_pdu_type(Type),
    FmtErrStatus = format_error_status(ES),
    FmtErrIdx    = format_error_index(EI),
    FmtVBs       = format_varbinds(Prefix ++ "   ", VBs),
    lists:flatten(io_lib:format("~s~s: ~n"
				"~s   Request-id:   ~w~n"
				"~s   Error-status: ~s~n"
				"~s   Error-index:  ~s~n"
				"~s",
				[Prefix, FmtPdyType,
				 Prefix, ReqId, 
				 Prefix, FmtErrStatus, 
				 Prefix, FmtErrIdx, 
				 FmtVBs]));
format_pdu(Prefix, #trappdu{enterprise    = E,
			    agent_addr    = AA,
			    generic_trap  = GT,
			    specific_trap = ST,
			    time_stamp    = TS,
			    varbinds      = VBs}) ->
    FmtVBs = format_varbinds(Prefix ++ "   ", VBs),
    lists:flatten(io_lib:format("~sTrap PDU: ~n"
				"~s   Enterprise:    ~p~n"
				"~s   Agent address: ~p~n"
				"~s   Generic trap:  ~p~n"
				"~s   Specific trap: ~p~n"
				"~s   Time stamp:    ~p~n"
				"~s",
				[Prefix, 
				 Prefix, E,
				 Prefix, AA, 
				 Prefix, GT, 
				 Prefix, ST, 
				 Prefix, TS, 
				 FmtVBs]));
format_pdu(Prefix, PDU) ->
    lists:flatten(io_lib:format("~s~p~n", [Prefix, PDU])).

format_pdu_type('get-request') ->
    "GetRequest-PDU";
format_pdu_type('get-next-request') ->
    "GetNextRequest-PDU";
format_pdu_type('get-response') ->
    "Response-PDU";
format_pdu_type('set-request') ->
    "SetRequest-PDU";
format_pdu_type('get-bulk-request') ->
    "GetBulkRequest-PDU";
format_pdu_type('inform-request') ->
    "InformRequest-PDU";
format_pdu_type('snmpv2-trap') ->
    "SNMPv2-Trap-PDU";
format_pdu_type(report) ->
    "Report-PDU";
format_pdu_type(T) ->
    lists:flatten(io_lib:format("~p", [T])).
    
format_snmp_info(Prefix, {ES, EI, VBs}) ->
    lists:flatten(io_lib:format("~sSNMP info: ~n"
				"~s   Error-status: ~s~n"
				"~s   Error-index:  ~s~n"
				"~s",
				[Prefix, 
				 Prefix, format_error_status(ES),
				 Prefix, format_error_index(EI),
				 format_varbinds(Prefix ++ "   ", VBs)]));
format_snmp_info(Prefix, JunkSnmpInfo) ->
    lists:flatten(io_lib:format("~sJunk SNMP info: ~n"
				"~s   ~p~n",
				[Prefix, Prefix, JunkSnmpInfo])).

format_error_status(ES) ->
    lists:flatten(io_lib:format("~p", [ES])).

format_error_index(EI) ->
    lists:flatten(io_lib:format("~p", [EI])).

format_sec_info(Prefix, Info) ->
    FmtSecInfo = do_format_sec_info(Prefix ++ "   ", Info),
    lists:flatten(io_lib:format("~sSecurity info: ~n~s", 
				[Prefix, FmtSecInfo])).

do_format_sec_info(_Prefix, []) ->
    "";
do_format_sec_info(Prefix, [{Tag, ExpVal, Val}|T]) ->
    format_sec_info(Prefix, Tag, ExpVal, Val) ++
	do_format_sec_info(Prefix, T).


format_sec_info(_Prefix, _Tag, Val, Val) ->
    "";
format_sec_info(Prefix, Tag, ExpVal, Val) ->
    lists:flatten(io_lib:format("~s~s:~n"
				"~s   Expected value: ~p~n"
				"~s   Actual value:   ~p~n",
				[Prefix, format_sec_info_tag(Tag),
				 Prefix, ExpVal,
				 Prefix, Val])).

format_sec_info_tag(sec_engine_id) ->
    "Sec engine id";
format_sec_info_tag(msg_sec_model) ->
    "Msg sec model";
format_sec_info_tag(sec_name) ->
    "Sec name";
format_sec_info_tag(sec_level) ->
    "Sec level";
format_sec_info_tag(ctx_engine_id) ->
    "Context engine id";
format_sec_info_tag(ctx_name) ->
    "Context name";
format_sec_info_tag(request_id) ->
    "Request id";
format_sec_info_tag(T) ->
    lists:flatten(io_lib:format("~p", [T])).

format_varbinds(Prefix, []) ->
    lists:flatten(io_lib:format("~sVarbinds:    []~n", [Prefix])); 
format_varbinds(Prefix, VBs) when is_list(VBs) ->
    lists:flatten(io_lib:format("~sVarbinds: ~n~s", 
				[Prefix, format_vbs(Prefix ++ "   ", VBs)]));
format_varbinds(Prefix, VBs) ->
    lists:flatten(io_lib:format("~sInvalid varbinds: ~n"
				"~s   ~p~n", 
				[Prefix, Prefix, VBs])).

format_vbs(_Prefix, []) ->
    "";
format_vbs(Prefix, [VB|VBs]) ->
    format_vb(Prefix, VB) ++ format_vbs(Prefix, VBs).
    
format_vb(Prefix, #varbind{oid          = Oid0,
			   variabletype = Type,
			   value        = Val,
			   org_index    = Idx}) ->
    Oid = 
	case snmpm:oid_to_name(Oid0) of
	    {ok, O} ->
		O;
	    _ ->
		Oid0
	end,
    FmtVT  = format_vb_variabletype(Prefix ++ "   ", Type),
    FmtVal = format_vb_value(Prefix ++ "   ", Type, Val),
    lists:flatten(io_lib:format("~s~w:~n"
				"~s"
				"~s"
				"~s   Org-index:     ~p~n", 
				[Prefix, Oid, 
				 FmtVT, 
				 FmtVal, 
				 Prefix, Idx]));
format_vb(Prefix, JunkVB) ->
    lists:flatten(io_lib:format("~sJunk varbind:~n"
				"~s   ~p~n", [Prefix, Prefix, JunkVB])).

format_vb_variabletype(Prefix, Type) when is_atom(Type) ->
    lists:flatten(io_lib:format("~sVariable-type: ~s~n", 
				[Prefix, atom_to_list(Type)]));
format_vb_variabletype(Prefix, Type) ->
    lists:flatten(io_lib:format("~sVariable-type: ~p~n", [Prefix, Type])).

format_vb_value(Prefix, _Type, Val) ->
    lists:flatten(io_lib:format("~sValue:         ~p~n", [Prefix, Val])).


%% ---------------------------------------------------------------------------
%% 
%% --- Internal utility functions ---
%% 

-doc false.
target_name(Ip) ->
    target_name(Ip, ?DEFAULT_AGENT_PORT).

-doc false.
target_name(DomainIp, AddressPort) ->
    snmpm_config:agent_info(DomainIp, AddressPort, target_name).

mk_target_name(Addr, Port, Config) ->
    R = snmpm_config:mk_target_name(Addr, Port, Config),
    p(?MODULE_STRING":mk_target_name(~p, ~p, ~p) -> ~p.~n",
      [Addr, Port, Config, R]),
    R.

ensure_engine_id(Config) ->
    case lists:keymember(engine_id, 1, Config) of
	true ->
	    Config;
	false ->
	    DefaultEngineId = "agentEngine-default", 
	    [{engine_id, DefaultEngineId} | Config]
    end.



%% p(F) ->
%%     p(F, []).

p(F, A) ->
    io:format("~w:" ++ F ++ "~n", [?MODULE | A]).
