<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Running the application

The chapter _Running the application_ describes how the application is
configured and started. The topics include:

- configuration directories and parameters
- modifying the configuration files
- starting the application (agent and/or manager)
- debugging the application (agent and/or manager)

Refer also to the chapter(s)
[Definition of Agent Configuration Files](snmp_agent_config_files.md) and
[Definition of Manager Configuration Files](snmp_manager_config_files.md) which
contains more detailed information about the agent and manager configuration
files.

[](){: #configuration_params }

## Configuring the application

The following two directories must exist in the system to run the agent:

- the _configuration directory_ stores all configuration files used by the agent
  (refer to the chapter
  [Definition of Agent Configuration Files](snmp_agent_config_files.md) for more
  information).
- the _database directory_ stores the internal database files.

The following directory must exist in the system to run the manager:

- the _configuration directory_ stores all configuration files used by the
  manager (refer to the chapter
  [Definition of Manager Configuration Files](snmp_manager_config_files.md) for
  more information).
- the _database directory_ stores the internal database files.

The agent and manager uses (application) configuration parameters to find out
where these directories are located. The parameters should be defined in an
Erlang system configuration file. The following configuration parameters are
defined for the SNMP application:

```erlang
      agent_options() = [agent_option()]
      agent_option() = {restart_type,     restart_type()}     |
                       {agent_type,       agent_type()}       |
                       {agent_verbosity,  verbosity()}        |
                       {versions,         versions()}         |
                       {discovery,        agent_discovery()}  |
                       {gb_max_vbs,       gb_max_vbs()}       |
                       {priority,         priority()}         |
                       {multi_threaded,   multi_threaded()}   |
                       {db_dir,           db_dir()}           |
                       {db_init_error,    db_init_error()}    |
                       {local_db,         local_db()}         |
                       {net_if,           agent_net_if()}     |
                       {mibs,             mibs()}             |
                       {mib_storage,      mib_storage()}      |
                       {mib_server,       mib_server()}       |
                       {audit_trail_log,  audit_trail_log()}  |
                       {error_report_mod, error_report_mod()} |
                       {note_store,       note_store()}       |
                       {symbolic_store,   symbolic_store()}   |
                       {target_cache,     target_cache()}     |
                       {config,           agent_config()}
      manager_options() = [manager_option()]
      manager_option() = {restart_type,             restart_type()}    |
                         {net_if,                   manager_net_if()}  |
                         {server,                   server()}          |
                         {note_store,               note_store()}      |
                         {config,                   manager_config()}  |
                         {inform_request_behaviour, manager_irb()}     |
                         {mibs,                     manager_mibs()}    |
                         {priority,                 priority()}        |
                         {audit_trail_log,          audit_trail_log()} |
                         {versions,                 versions()}        |
                         {def_user_mod,             def_user_module()  |
                         {def_user_data,            def_user_data()}
```

[](){: #agent_opts_and_types }

Agent specific config options and types:

- **`agent_type() = master | sub <optional>`{: #agent_type }** - If `master`,
  one master agent is started. Otherwise, no agents are started.

  Default is `master`.

- **`agent_discovery() = [agent_discovery_opt()] <optional>`{: #agent_disco }** -
  `agent_discovery_opt() = {terminating, agent_terminating_discovery_opts()} | {originating, agent_originating_discovery_opts()}`

  The `terminating` options effects discovery initiated by a manager.

  The `originating` options effects discovery initiated by this agent.

  For defaults see the options in `agent_discovery_opt()`.

- **`agent_terminating_discovery_opts() = [agent_terminating_discovery_opt()] <optional>`{: #agent_term_disco_opts }** -
  `agent_terminating_discovery_opt() = {enable, boolean()} | {stage2, discovery | plain} | {trigger_username, string()}`

  These are options effecting discovery `terminating` in this agent (i.e.
  initiated by a manager).

  The default values for the `terminating` discovery options are:

  - enable: `true`
  - stage2: `discovery`
  - trigger_username: `""`

- **`agent_originating_discovery_opts() = [agent_originating_discovery_opt()] <optional>`{: #agent_orig_disco_opts }** -
  `agent_originating_discovery_opt() = {enable, boolean()}`

  These are options effecting discovery `originating` in this agent.

  The default values for the `originating` discovery options are:

  - enable: `true`

- **`multi_threaded() = bool() | extended<optional>`{: #agent_mt }** - If `true`
  (or `extended`), the agent is multi-threaded, with one thread for each get
  request.

  The value `extended` means that a special 'process' is also created intended
  to handle _all_ notifications.

  - `true` \- One worker dedicated to 'set-requests' and one (main) worker for
    all other requests ('get-request' and notifications).

    If the 'main' worker is busy, a temporary process is spawned to handle that
    job ('get-request' or notification).

  - `extended` \- One worker dedicated to 'set-requests', one worker dedicated
    to notifications and one (main) worker for all 'get-requests'.

    If the 'main' worker is busy, a temporary process is spawned to handle that
    'get-request'.

  > #### Note {: .info }
  >
  > Even with multi-threaded set to `extended` there is still a risk for
  > 'reorder' when sending inform-requsts, which require a response (and may
  > therefore require resending).
  >
  > Also, there is of course no way to guarantee order once the package is on
  > the network.

  Default is `false`.

- **`db_dir() = string() <mandatory>`{: #agent_data_dir }** - Defines where the
  SNMP agent internal db files are stored.

- **`gb_max_vbs() = pos_integer() | infinity <optional>`{: #agent_gb_max_vbs }** - Defines the maximum number of varbinds allowed in a Get-BULK response.

  Default is `1000`.

- **`local_db() = [local_db_opt()] <optional>`{: #agent_local_db }** -
  `local_db_opt() = {repair, agent_repair()} | {auto_save, agent_auto_save()} | {verbosity, verbosity()}`

  Defines options specific for the SNMP agent local database.

  For defaults see the options in `local_db_opt()`.

- **`agent_repair() = false | true | force <optional>`{: #agent_ldb_repair }** -
  When starting snmpa_local_db it always tries to open an existing database. If
  `false`, and some errors occur, a new database is created instead. If `true`,
  an existing file will be repaired. If `force`, the table will be repaired even
  if it was properly closed.

  Default is `true`.

- **`agent_auto_save() = integer() | infinity <optional>`{: #agent_ldb_auto_save }** - The auto save interval. The table is flushed to disk whenever not
  accessed for this amount of time.

  Default is `5000`.

- **`agent_net_if() = [agent_net_if_opt()] <optional>`{: #agent_net_if }** -
  `agent_net_if_option() = {module, agent_net_if_module()} | {verbosity, verbosity()} | {options, agent_net_if_options()}`

  Defines options specific for the SNMP agent network interface entity.

  For defaults see the options in `agent_net_if_opt()`.

- **`agent_net_if_module() = atom() <optional>`{: #agent_ni_module }** - Module
  which handles the network interface part for the SNMP agent. Must implement
  the `m:snmpa_network_interface` behaviour.

  Default is `snmpa_net_if`.

- **`agent_net_if_options() = [agent_net_if_option()] <optional>`{: #agent_ni_opts }** -
  `agent_net_if_option() = {bind_to, bind_to()} | {sndbuf, sndbuf()} | {recbuf, recbuf()} | {no_reuse, no_reuse()} | {req_limit, req_limit()} | {filter, agent_net_if_filter_options()} | {open_err_filters, agent_net_if_open_err_filters()} | {extra_sock_opts, extra_socket_options()} | {inet_backend, inet_backend()}`

  These options are actually specific to the used module. The ones shown here
  are applicable to the default `agent_net_if_module()`.

  > #### Note {: .info }
  >
  > If the user has configured transports _with_ options then those will take
  > precedence over these options. See
  > [agent information](snmp_agent_config_files.md#agent_information) for more
  > info.

  For defaults see the options in `agent_net_if_option()`.

- **`req_limit() = integer() | infinity <optional>`{: #agent_ni_req_limit }** -
  Max number of simultaneous requests handled by the agent.

  Default is `infinity`.

- **`agent_net_if_filter_options() = [agent_net_if_filter_option()] <optional>`{: #agent_ni_filter_opts }** -
  `agent_net_if_filter_option() = {module, agent_net_if_filter_module()}`

  These options are actually specific to the used module. The ones shown here
  are applicable to the default `agent_net_if_filter_module()`.

  For defaults see the options in `agent_net_if_filter_option()`.

- **`agent_net_if_filter_module() = atom() <optional>`{: #agent_ni_filter_module }** - Module which handles the network interface filter part for the SNMP
  agent. Must implement the
  [snmpa_network_interface_filter ](`m:snmpa_network_interface_filter`)behaviour.

  Default is `snmpa_net_if_filter`.

- **`agent_net_if_open_err_filters() = [agent_net_if_open_err_filter()] <optional>`{: #agent_ni_open_err_filters }** - `agent_net_if_open_err_filter() = atom()`

  During agent initiation, the transports UDP sockets are opened. If this
  operation fails, the net-if (and the agent) fails to start (crash). This
  (filter) list contains error (reasons) that will make net-if fail "nicely".
  This (filter) list, is supposed to contain errors that can be returned by
  [gen_udp:open/1,2](`gen_udp:open/1`). The effect is that any error returned by
  [gen_udp:open](`gen_udp:open/1`) which _are_ in this list, will be considered
  "non-fatal" and will only result in an info message, rather than an error
  message. Net If, and the agent, will still crash, but will produce a less
  obnoxious message.

- **`agent_mibs() = [string()] <optional>`{: #agent_mibs }** - Specifies a list
  of MIBs (including path) that defines which MIBs are initially loaded into the
  SNMP master agent.

  Note that the following will always be loaded:

  - version v1: `STANDARD-MIB`
  - version v2: `SNMPv2`
  - version v3: `SNMPv2`, `SNMP-FRAMEWORK-MIB` and `SNMP-MPD-MIB`

  Default is `[]`.

- **`mib_storage() = [mib_storage_opt()] <optional>`{: #agent_mib_storage }** -
  `mib_storage_opt() = {module, mib_storage_module()} | {options, mib_storage_options()}`

  This option specifies how basic mib data is stored. This option is used by two
  parts of the snmp agent: The mib-server and the symbolic-store.

  Default is `[{module, snmpa_mib_storage_ets}]`.

- **`mib_storage_module() = snmpa_mib_data_ets | snmpa_mib_data_dets | snmpa_mib_data_mnesia | module()`{: #agent_mst_module }** - Defines the mib storage module of the SNMP agent as
  defined by the `m:snmpa_mib_storage` behaviour.

  Several entities (`mib-server` via the its data module and the
  `symbolic-store`) of the snmp agent uses this for storage of miscellaneous mib
  related data data retrieved while loading a mib.

  There are several implementations provided with the agent:
  `snmpa_mib_storage_ets`, `snmpa_mib_storage_dets` and
  `snmpa_mib_storage_mnesia`.

  Default module is `snmpa_mib_storage_ets`.

- **`mib_storage_options() = list() <optional>`{: #agent_mst_options }** - This
  is implementation depended. That is, it depends on the module. For each module
  a specific set of options are valid. For the module provided with the app,
  these options are supported:

  - `snmpa_mib_storage_ets`:
    `{dir, filename()} | {action, keep | clear}, {checksum, boolean()}`

    - `dir` \- If present, points to a directory where a file to which all data
      in the ets table is "synced".

      Also, when a table is opened this file is read, if it exists.

      By default, this will _not_ be used.

    - `action` \- Specifies the behaviour when a non-empty file is found: Keep
      its content or clear it out.

      Default is `keep`.

    - `checksum` \- Defines if the file is checksummed or not.

      Default is `false`.

  - `snmpa_mib_storage_dets`:
    `{dir, filename()} | {action, keep | clear}, {auto_save, default | pos_integer()} | {repair, force | boolean()}`

    - `dir` \- This _mandatory_ option points to a directory where to place the
      file of a dets table.
    - `action` \- Specifies the behaviour when a non-empty file is found: Keep
      its content or clear it out.

      Default is `keep`.

    - `auto_save` \- Defines the dets auto-save frequency.

      Default is `default`.

    - `repair` \- Defines the dets repair behaviour.

      Default is `false`.

  - `snmpa_mib_storage_mnesia`: `{action, keep | clear}, {nodes, [node()]}`

    - `action` \- Specifies the behaviour when a non-empty, already existing,
      table: Keep its content or clear it out.

      Default is `keep`.

    - `nodes` \- A list of node names (or an atom describing a list of nodes)
      defining where to open the table. Its up to the user to ensure that mnesia
      is actually running on the specified nodes.

      The following distinct values are recognised:

      - `[]` \- Translated into a list of the own node: `[node()]`
      - `all` \- `erlang:nodes()`
      - `visible` \- `erlang:nodes(visible)`
      - `connected` \- `erlang:nodes(connected)`
      - `db_nodes` \- `mnesia:system_info(db_nodes)`

      Default is the result of the call: `erlang:nodes()`.

- **`mib_server() = [mib_server_opt()] <optional>`{: #agent_mib_server }** -
  `mib_server_opt() = {mibentry_override, mibentry_override()} | {trapentry_override, trapentry_override()} | {verbosity, verbosity()} | {cache, mibs_cache()} | {data_module, mib_server_data_module()}`

  Defines options specific for the SNMP agent mib server.

  For defaults see the options in `mib_server_opt()`.

- **`mibentry_override() = bool() <optional>`{: #agent_ms_meo }** - If this
  value is false, then when loading a mib each mib- entry is checked prior to
  installation of the mib. The purpose of the check is to prevent that the same
  symbolic mibentry name is used for different oid's.

  Default is `false`.

- **`trapentry_override() = bool() <optional>`{: #agent_ms_teo }** - If this
  value is false, then when loading a mib each trap is checked prior to
  installation of the mib. The purpose of the check is to prevent that the same
  symbolic trap name is used for different trap's.

  Default is `false`.

- **`mib_server_data_module() = snmpa_mib_data_tttn | module() <optional>`{: #agent_ms_data_module }** - Defines the backend data module of the SNMP agent
  mib-server as defined by the `m:snmpa_mib_data` behaviour.

  At present only the default module is provided with the agent,
  `snmpa_mib_data_tttn`.

  Default module is `snmpa_mib_data_tttn`.

- **`mibs_cache() = bool() | mibs_cache_opts() <optional>`{: #agent_ms_cache }** - Shall the agent utilize the mib server lookup cache or not.

  Default is `true` (in which case the `mibs_cache_opts()` default values
  apply).

- **`mibs_cache_opts() = [mibs_cache_opt()] <optional>`{: #agent_ms_cache_opts }** -
  `mibs_cache_opt() = {autogc, mibs_cache_autogc()} | {gclimit, mibs_cache_gclimit()} | {age, mibs_cache_age()}`

  Defines options specific for the SNMP agent mib server cache.

  For defaults see the options in `mibs_cache_opt()`.

- **`mibs_cache_autogc() = bool() <optional>`{: #agent_ms_cache_autogc }** -
  Defines if the mib server shall perform cache gc automatically or leave it to
  the user (see [gc_mibs_cache/0,1,2,3](`snmpa:gc_mibs_cache/0`)).

  Default is `true`.

- **`mibs_cache_age() = integer() > 0 <optional>`{: #agent_ms_cache_age }** -
  Defines how old the entries in the cache will be allowed to become before they
  are GC'ed (assuming GC is performed). Each entry in the cache is "touched"
  whenever it is accessed.

  The age is defined in milliseconds.

  Default is `10 timutes`.

- **`mibs_cache_gclimit() = infinity | integer() > 0 <optional>`{: #agent_ms_cache_gclimit }** - When performing a GC, this is the max number of
  cache entries that will be deleted from the cache.

  The reason why its possible to set a limit, is that if the cache is large, the
  GC can potentially take a long time, during which the agent is "busy". _But_
  on a heavily loaded system, we also risk not removing enough elements in the
  cache, instead causing it to grow over time. This is the reason the default
  value is `infinity`, which will ensure that _all_ candidates are removed as
  soon as possible.

  Default is `infinity`.

- **`error_report_mod() = atom() <optional>`{: #agent_error_report_mod }** -
  Defines an error report module, implementing the `m:snmpa_error_report`
  behaviour. Two modules are provided with the toolkit: `snmpa_error_logger` and
  `snmpa_error_io`.

  Default is `snmpa_error_logger`.

- **`symbolic_store() = [symbolic_store_opt()]`{: #agent_symbolic_store }** -
  `symbolic_store_opt() = {verbosity, verbosity()}`

  Defines options specific for the SNMP agent symbolic store.

  For defaults see the options in `symbolic_store_opt()`.

- **`target_cache() = [target_cache_opt()]`{: #agent_target_cache }** -
  `target_cache_opt() = {verbosity, verbosity()}`

  Defines options specific for the SNMP agent target cache.

  For defaults see the options in `target_cache_opt()`.

- **`agent_config() = [agent_config_opt()] <mandatory>`{: #agent_config }** -
  `agent_config_opt() = {dir, agent_config_dir()} | {force_load, force_load()} | {verbosity, verbosity()}`

  Defines specific config related options for the SNMP agent.

  For defaults see the options in `agent_config_opt()`.

- **`agent_config_dir = dir() <mandatory>`{: #agent_config_dir }** - Defines
  where the SNMP agent configuration files are stored.

- **`force_load() = bool() <optional>`{: #agent_force_load }** - If `true` the
  configuration files are re-read during start-up, and the contents of the
  configuration database ignored. Thus, if `true`, changes to the configuration
  database are lost upon reboot of the agent.

  Default is `false`.

[](){: #manager_opts_and_types }

Manager specific config options and types:

- **`server() = [server_opt()] <optional>`{: #manager_server }** -
  `server_opt() = {timeout, server_timeout()} | {verbosity, verbosity()} | {cbproxy, server_cbproxy()} | {netif_sup, server_nis()}`

  Specifies the options for the manager server process.

  Default is `silence`.

- **`server_timeout() = integer() <optional>`{: #manager_server_timeout }** -
  Asynchronous request cleanup time. For every request, some info is stored
  internally, in order to be able to deliver the reply (when it arrives) to the
  proper destination. If the reply arrives, this info will be deleted. But if
  there is no reply (in time), the info has to be deleted after the _best
  before_ time has been passed. This cleanup will be performed at regular
  intervals, defined by the `server_timeout()` time. The information will have a
  _best before_ time, defined by the `Expire` time given when calling the
  request function (see [async_get](`snmpm:async_get2/4`),
  [async_get_next](`snmpm:async_get_next2/4`) and
  [async_set](`snmpm:async_set2/4`)).

  Time in milli-seconds.

  Default is `30000`.

- **`server_cbproxy() = temporary (default) | permanent <optional>`{: #manager_server_cbproxy }** - This option specifies how the server will handle
  callback calls.

  - **`temporary (default)`{: #manager_server_cbproxy_temporary }** - A
    temporary process will be created for each callback call.

  - **`permanent`{: #manager_server_cbproxy_permanent }** - With this the server
    will create a permanent (named) process that in effect serializes all
    callback calls.

  Default is `temporary`.

- **`server_nis() = none (default) | {PingTO, PongTO} <optional>`{: #manager_server_nis }** - This option specifies if the server should actively
  supervise the net-if process. Note that this will only work if the used net-if
  process actually supports the protocol. See `m:snmpm_network_interface`
  behaviour for more info.

  - **`none (default)`{: #manager_server_nis_none }** - No active supervision of
    the net-if process.

  - **`{PingTO :: pos_integer(), PongTO :: pos_integer()}`{: #manager_server_nis_active }** - The `PingTO` time specifies the between a
    successful ping (or start) and the time when a
    [ping](snmp_manager_netif.md#im_ping) message is to be sent to the net-if
    process (basically the time between ping:s).

    The `PongTO` time specifies how long time the net-if process has to respond
    to a ping message, with a [pong](snmp_manager_netif.md#om_pong) message. It
    starts counting when the ping message has been sent.

    Both times are in milli seconds.

  Default is `none`.

- **`manager_config() = [manager_config_opt()] <mandatory>`{: #manager_config }** -
  `manager_config_opt() = {dir, manager_config_dir()} | {db_dir, manager_db_dir()} | {db_init_error, db_init_error()} | {repair, manager_repair()} | {auto_save, manager_auto_save()} | {verbosity, verbosity()}`

  Defines specific config related options for the SNMP manager.

  For defaults see the options in `manager_config_opt()`.

- **`manager_config_dir = dir() <mandatory>`{: #manager_config_dir }** - Defines
  where the SNMP manager configuration files are stored.

- **`manager_db_dir = dir() <mandatory>`{: #manager_config_db_dir }** - Defines
  where the SNMP manager store persistent data.

- **`manager_repair() = false | true | force <optional>`{: #manager_config_repair }** - Defines the repair option for the persistent
  database (if and how the table is repaired when opened).

  Default is `true`.

- **`manager_auto_save() = integer() | infinity <optional>`{: #manager_config_auto_save }** - The auto save interval. The table is flushed
  to disk whenever not accessed for this amount of time.

  Default is `5000`.

- **`manager_irb() = auto | user | {user, integer()} <optional>`{: #manager_irb }** - This option defines how the manager will handle the sending of response
  (acknowledgment) to received inform-requests.

  - `auto` \- The manager will autonomously send response (acknowledgment> to
    inform-request messages.
  - `{user, integer()}` \- The manager will send response (acknowledgment) to
    inform-request messages when the
    [handle_inform](`c:snmpm_user:handle_inform/3`) function completes. The
    integer is the time, in milli-seconds, that the manager will consider the
    stored inform-request info valid.
  - `user` \- Same as `{user, integer()}`, except that the default time, 15000
    milli-seconds, is used.

  See `m:snmpm_network_interface`, [handle_inform](`m:snmpm_user`) and
  [definition of the manager net if](snmp_manager_netif.md) for more info.

  Default is `auto`.

- **`manager_mibs() = [string()] <optional>`{: #manager_mibs }** - Specifies a
  list of MIBs (including path) and defines which MIBs are initially loaded into
  the SNMP manager.

  Default is `[]`.

- **`manager_net_if() = [manager_net_if_opt()] <optional>`{: #manager_net_if }** -
  `manager_net_if_opt() = {module, manager_net_if_module()} | {verbosity, verbosity()} | {options, manager_net_if_options()}`

  Defines options specific for the SNMP manager network interface entity.

  For defaults see the options in `manager_net_if_opt()`.

- **`manager_net_if_options() = [manager_net_if_option()] <optional>`{: #manager_ni_opts }** -
  `manager_net_if_option() = {bind_to, bind_to()} | {sndbuf, sndbuf()} | {recbuf, recbuf()} | {no_reuse, no_reuse()} | {filter, manager_net_if_filter_options()} | {extra_sock_opts, extra_socket_options()} | {inet_backend, inet_backend()}`

  These options are actually specific to the used module. The ones shown here
  are applicable to the default `manager_net_if_module()`.

  For defaults see the options in `manager_net_if_option()`.

- **`manager_net_if_module() = atom() <optional>`{: #manager_ni_module }** - The
  module which handles the network interface part for the SNMP manager. It must
  implement the `m:snmpm_network_interface` behaviour.

  Default is `snmpm_net_if`.

- **`manager_net_if_filter_options() = [manager_net_if_filter_option()] <optional>`{: #manager_ni_filter_opts }** -
  `manager_net_if_filter_option() = {module, manager_net_if_filter_module()}`

  These options are actually specific to the used module. The ones shown here
  are applicable to the default `manager_net_if_filter_module()`.

  For defaults see the options in `manager_net_if_filter_option()`.

- **`manager_net_if_filter_module() = atom() <optional>`{: #manager_ni_filter_module }** - Module which handles the network interface
  filter part for the SNMP manager. Must implement the
  `m:snmpm_network_interface_filter` behaviour.

  Default is `snmpm_net_if_filter`.

- **`def_user_module() = atom() <optional>`{: #manager_def_user_module }** - The
  module implementing the default user. See the `m:snmpm_user` behaviour.

  Default is `snmpm_user_default`.

- **`def_user_data() = term() <optional>`{: #manager_def_user_data }** - Data
  for the default user. Passed to the user when calling the callback functions.

  Default is `undefined`.

[](){: #common_types }

Common config types:

- **`restart_type() = permanent | transient | temporary`{: #restart_type }** -
  See [supervisor](`m:supervisor#child_spec`) documentation for more info.

  Default is `permanent` for the agent and `transient` for the manager.

- **`db_init_error() = terminate | create | create_db_and_dir`{: #db_init_error }** - Defines what to do if the agent is unable to open an existing database
  file. `terminate` means that the agent/manager will terminate, `create` means
  that the agent/manager will remove the faulty file(s) and create new ones, and
  `create_db_and_dir` means that the agent/manager will create the database file
  along with any missing parent directories for the database file.

  Default is `terminate`.

- **`priority() = atom() <optional>`{: #prio }** - Defines the Erlang priority
  for all SNMP processes.

  Default is `normal`.

- **`versions() = [version()] <optional>`{: #versions }** -
  `version() = v1 | v2 | v3`

  Which SNMP versions shall be accepted/used.

  Default is `[v1,v2,v3]`.

- **`verbosity() = silence | info | log | debug | trace <optional>`{: #verbosity }** - Verbosity for a SNMP process. This specifies now much debug info is
  printed.

  Default is `silence`.

- **`bind_to() = bool() <optional>`{: #bind_to }** - If `true`, net_if binds to
  the IP address. If `false`, net_if listens on any IP address on the host where
  it is running.

  Default is `false`.

- **`no_reuse() = bool() <optional>`{: #no_reuse }** - If `true`, net_if does
  not specify that the IP and port address should be reusable. If `false`, the
  address is set to reusable.

  Default is `false`.

- **`recbuf() = integer() <optional>`{: #recbuf }** - Receive buffer size.

  Default value is defined by `gen_udp`.

- **`sndbuf() = integer() <optional>`{: #sndbuf }** - Send buffer size.

  Default value is defined by `gen_udp`.

- **`extra_socket_options() = list() <optional>`{: #extra_sock_opts }** - A list
  of arbitrary socket options.

  This list is not inspected by snmp (other then checking that its a list). Its
  the users responsibility to ensure that these are valid options and does not
  conflict with the "normal" options.

  Default is `[]`.

- **`inet_backend() = inet | socket <optional>`{: #inet_backend }** - Choose the
  inet-backend.

  This option make it possible to use net_if (gen_udp) with a different
  inet-backend ('inet' or 'socket').

  Default is `inet`.

- **`note_store() = [note_store_opt()] <optional>`{: #note_store }** -
  `note_store_opt() = {timeout, note_store_timeout()} | {verbosity, verbosity()}`

  Specifies the options for the SNMP note store.

  For defaults see the options in `note_store_opt()`.

- **`note_store_timeout() = integer() <optional>`{: #ns_timeout }** - Note
  cleanup time. When storing a note in the note store, each note is given
  lifetime. Every `timeout` the note_store process performs a GC to remove the
  expired note's. Time in milli-seconds.

  Default is `30000`.

- **`audit_trail_log() [audit_trail_log_opt()] <optional>`{: #audit_trail_log }** -
  `audit_trail_log_opt() = {type, atl_type()} | {dir, atl_dir()} | {size, atl_size()} | {repair, atl_repair()} | {seqno, atl_seqno()}`

  If present, this option specifies the options for the _audit trail logging_.
  The `disk_log` module is used to maintain a wrap log. If present, the `dir`
  and `size` options are mandatory.

  If not present, audit trail logging is not used.

- **`atl_type() = read | write | read_write <optional>`{: #atl_type }** -
  Specifies what type of an audit trail log should be used. The effect of the
  type is actually different for the the agent and the manager.

  For the agent:

  - If `write` is specified, only set requests are logged.
  - If `read` is specified, only get requests are logged.
  - If `read_write`, all requests are logged.

  For the manager:

  - If `write` is specified, only sent messages are logged.
  - If `read` is specified, only received messages are logged.
  - If `read_write`, both outgoing and incoming messages are logged.

  Default is `read_write`.

- **`atl_dir = dir() <mandatory>`{: #atl_dir }** - Specifies where the audit
  trail log should be stored.

  If `audit_trail_log` specifies that logging should take place, this parameter
  _must_ be defined.

- **`atl_size() = {integer(), integer()} <mandatory>`{: #atl_size }** -
  Specifies the size of the audit trail log. This parameter is sent to
  `disk_log`.

  If `audit_trail_log` specifies that logging should take place, this parameter
  _must_ be defined.

- **`atl_repair() = true | false | truncate | snmp_repair <optional>`{: #atl_repair }** - Specifies if and how the audit trail log shall be repaired
  when opened. Unless this parameter has the value `snmp_repair` it is sent to
  `disk_log`. If, on the other hand, the value is `snmp_repair`, snmp attempts
  to handle certain faults on its own. And even if it cannot repair the file, it
  does not truncate it directly, but instead _moves it aside_ for later off-line
  analysis.

  Default is `true`.

- **`atl_seqno() = true | false <optional>`{: #atl_seqno }** - Specifies if the
  audit trail log entries will be (sequence) numbered or not. The range of the
  sequence numbers are according to RFC 5424, i.e. 1 through 2147483647.

  Default is `false`.

## Modifying the Configuration Files

To to start the application (agent and/or manager), the configuration files must
be modified and there are two ways of doing this. Either edit the files
manually, or run the configuration tool as follows.

If authentication or encryption is used (SNMPv3 only), start the `crypto`
application.

```text
1> snmp:config().

Simple SNMP configuration tool (version 4.0)
------------------------------------------------
Note: Non-trivial configurations still has to be
      done manually. IP addresses may be entered
      as dront.ericsson.se (UNIX only) or
      123.12.13.23
------------------------------------------------

Configure an agent (y/n)? [y]

Agent system config:
--------------------
1. Agent process priority (low/normal/high) [normal]
2. What SNMP version(s) should be used (1,2,3,1&2,1&2&3,2&3)? [3] 1&2&3
3. Configuration directory (absolute path)? [/ldisk/snmp] /ldisk/snmp/agent/conf
4. Config verbosity (silence/info/log/debug/trace)? [silence]
5. Database directory (absolute path)? [/ldisk/snmp] /ldisk/snmp/agent/db
6. Mib storage type (ets/dets/mnesia)? [ets]
7. Target cache verbosity (silence/info/log/debug/trace)? [silence]
8. Symbolic store verbosity (silence/info/log/debug/trace)? [silence]
9. Local DB verbosity (silence/info/log/debug/trace)? [silence]
10. Local DB repair (true/false/force)? [true]
11. Local DB auto save (infinity/milli seconds)? [5000]
12. Error report module? [snmpa_error_logger]
13. Agent type (master/sub)? [master]
14. Master-agent verbosity (silence/info/log/debug/trace)? [silence] log
15. Shall the agent re-read the configuration files during startup
    (and ignore the configuration database) (true/false)? [true]
16. Multi threaded agent (true/false)? [false] true
17. Check for duplicate mib entries when installing a mib (true/false)? [false]
18. Check for duplicate trap names when installing a mib (true/false)? [false]
19. Mib server verbosity (silence/info/log/debug/trace)? [silence]
20. Mib server cache (true/false)? [true]
21. Note store verbosity (silence/info/log/debug/trace)? [silence]
22. Note store GC timeout? [30000]
23. Shall the agent use an audit trail log (y/n)? [n] y
23b. Audit trail log type (write/read_write)? [read_write]
23c. Where to store the audit trail log? [/ldisk/snmp] /ldisk/snmp/agent/log
23d. Max number of files? [10]
23e. Max size (in bytes) of each file? [10240]
23f. Audit trail log repair (true/false/truncate)? [true]
24. Which network interface module shall be used? [snmpa_net_if]
25. Network interface verbosity (silence/info/log/debug/trace)? [silence] log
25a. Bind the agent IP address (true/false)? [false]
25b. Shall the agents IP address and port be not reusable (true/false)? [false]
25c. Agent request limit (used for flow control) (infinity/pos integer)? [infinity] 32
25d. Receive buffer size of the agent (in bytes) (default/pos integer)? [default]
25e. Send buffer size of the agent (in bytes) (default/pos integer)? [default]
25f. Do you wish to specify a network interface filter module (or use default) [default]

Agent snmp config:
------------------
1. System name (sysName standard variable) [bmk's agent]
2. Engine ID (snmpEngineID standard variable) [bmk's engine]
3. Max message size? [484]
4. The UDP port the agent listens to. (standard 161) [4000]
5. IP address for the agent (only used as id
   when sending traps) [127.0.0.1]
6. IP address for the manager (only this manager
   will have access to the agent, traps are sent
   to this one) [127.0.0.1]
7. To what UDP port at the manager should traps
   be sent (standard 162)? [5000]
8. Do you want a none- minimum- or semi-secure configuration?
   Note that if you chose v1 or v2, you won't get any security for these
   requests (none, minimum, semi_des, semi_aes) [minimum]
making sure crypto server is started...
8b. Give a password of at least length 8. It is used to generate
    private keys for the configuration:  kalle-anka
9. Current configuration files will now be overwritten. Ok (y/n)? [y]

- - - - - - - - - - - - -
Info: 1. SecurityName "initial" has noAuthNoPriv read access
         and authenticated write access to the "restricted"
         subtree.
      2. SecurityName "all-rights" has noAuthNoPriv read/write
         access to the "internet" subtree.
      3. Standard traps are sent to the manager.
      4. Community "public" is mapped to security name "initial".
      5. Community "all-rights" is mapped to security name "all-rights".
The following agent files were written: agent.conf, community.conf,
standard.conf, target_addr.conf, target_params.conf,
notify.conf, vacm.conf and usm.conf
- - - - - - - - - - - - -

Configure a manager (y/n)? [y]

Manager system config:
----------------------
1. Manager process priority (low/normal/high) [normal]
2. What SNMP version(s) should be used (1,2,3,1&2,1&2&3,2&3)? [3] 1&2&3
3. Configuration directory (absolute path)? [/ldisk/snmp] /ldisk/snmp/manager/conf
4. Config verbosity (silence/info/log/debug/trace)? [silence] log
5. Database directory (absolute path)? [/ldisk/snmp] /ldisk/snmp/manager/db
6. Database repair (true/false/force)? [true]
7. Database auto save (infinity/milli seconds)? [5000]
8. Inform request behaviour (auto/user)? [auto]
9. Server verbosity (silence/info/log/debug/trace)? [silence] log
10. Server GC timeout? [30000]
11. Note store verbosity (silence/info/log/debug/trace)? [silence]
12. Note store GC timeout? [30000]
13. Which network interface module shall be used? [snmpm_net_if]
14. Network interface verbosity (silence/info/log/debug/trace)? [silence] log
15. Bind the manager IP address (true/false)? [false]
16. Shall the manager IP address and port be not reusable (true/false)? [false]
17. Receive buffer size of the manager (in bytes) (default/pos integer)? [default]
18. Send buffer size of the manager (in bytes) (default/pos integer)? [default]
19. Shall the manager use an audit trail log (y/n)? [n] y
19b. Where to store the audit trail log? [/ldisk/snmp] /ldisk/snmp/manager/log
19c. Max number of files? [10]
19d. Max size (in bytes) of each file? [10240]
19e. Audit trail log repair (true/false/truncate)? [true]
20. Do you wish to assign a default user [yes] or use
    the default settings [no] (y/n)? [n]

Manager snmp config:
--------------------
1. Engine ID (snmpEngineID standard variable) [bmk's engine]
2. Max message size? [484]
3. IP address for the manager (only used as id
   when sending requests) [127.0.0.1]
4. Port number (standard 162)? [5000]
5. Configure a user of this manager (y/n)? [y]
5b. User id? kalle
5c. User callback module? snmpm_user_default
5d. User (callback) data? [undefined]
5. Configure a user of this manager (y/n)? [y] n
6. Configure an agent handled by this manager (y/n)? [y]
6b. User id? kalle
6c. Target name? [bmk's agent]
6d. Version (1/2/3)? [1] 3
6e. Community string ? [public]
6f. Engine ID (snmpEngineID standard variable) [bmk's engine]
6g. IP address for the agent [127.0.0.1]
6h. The UDP port the agent listens to. (standard 161) [4000]
6i. Retransmission timeout (infinity/pos integer)? [infinity]
6j. Max message size? [484]
6k. Security model (any/v1/v2c/usm)? [any] usm
6l. Security name? ["initial"]
6m. Security level (noAuthNoPriv/authNoPriv/authPriv)? [noAuthNoPriv] authPriv
6. Configure an agent handled by this manager (y/n)? [y] n
7. Configure an usm user handled by this manager (y/n)? [y]
7a. Engine ID [bmk's engine]
7b. User name? hobbes
7c. Security name? [hobbes]
7d. Authentication protocol (no/sha/md5)? [no] sha
7e  Authentication [sha] key (length 0 or 20)? [""] [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, \
    17,18,19,20]
7d. Priv protocol (no/des/aes)? [no] des
7f  Priv [des] key (length 0 or 16)? [""] 10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25
7. Configure an usm user handled by this manager (y/n)? [y] n
8. Current configuration files will now be overwritten. Ok (y/n)? [y]

- - - - - - - - - - - - -
The following manager files were written: manager.conf, agents.conf , users.conf and usm.conf
- - - - - - - - - - - - -

--------------------
Configuration directory for system file (absolute path)? [/ldisk/snmp]
ok
```

## Starting the application

Start Erlang with the command:

```text
erl -config /tmp/snmp/sys
```

If authentication or encryption is used (SNMPv3 only), start the `crypto`
application. If this step is forgotten, the agent will not start, but report a
`{config_error,{unsupported_crypto,_}}` error.

```text
1> application:start(crypto).
ok
```

```text
2> application:start(snmp).
ok
```

## Debugging the application

It is possible to debug every (non-supervisor) process of the application (both
agent and manager), possibly with the exception of the net_if module(s), which
could be supplied by a user of the application). This is done by calling the
`snmpa:verbosity/2` and `snmpm:verbosity/2` function(s) and/or using
[configuration parameters](snmp_config.md#configuration_params). The verbosity
itself has several _levels_: `silence | info | log | debug | trace`. For the
lowest verbosity `silence`, nothing is printed. The higher the verbosity, the
more is printed. Default value is always `silence`.

```c
3> snmpa:verbosity(master_agent, log).
ok
5> snmpa:verbosity(net_if, log).
ok
6>
%% Example of output from the agent when a get-next-request arrives:
** SNMP NET-IF LOG:
   got packet from {147,12,12,12}:5000

** SNMP NET-IF MPD LOG:
   v1, community: all-rights

** SNMP NET-IF LOG:
   got pdu from {147,12,12,12}:5000 {pdu, 'get-next-request',
                                          62612569,noError,0,
                                          [{varbind,[1,1],'NULL','NULL',1}]}

** SNMP MASTER-AGENT LOG:
   apply: snmp_generic,variable_func,[get,{sysDescr,persistent}]

** SNMP MASTER-AGENT LOG:
   returned: {value,"Erlang SNMP agent"}

** SNMP NET-IF LOG:
   reply pdu: {pdu,'get-response',62612569,noError,0,
                   [{varbind,[1,3,6,1,2,1,1,1,0],
                             'OCTET STRING',
                             "Erlang SNMP agent",1}]}

** SNMP NET-IF INFO: time in agent: 19711 mysec
```

Other useful function(s) for debugging the agent are:

- **`snmpa:info/0,1`** - [info](`snmpa:info/0`) is used to retrieve a list of
  miscellaneous agent information.

- **`snmpa:which_aliasnames/0`** -
  [which_aliasnames](`snmpa:which_aliasnames/0`) is used to retrieve a list of
  all alias-names known to the agent.

- **`snmpa:which_tables/0`** - [which_tables](`snmpa:which_tables/0`) is used to
  retrieve a list of all (MIB) tables known to the agent.

- **`snmpa:which_variables/0`** - [which_variables](`snmpa:which_variables/0`)
  is used to retrieve a list of all (MIB) variables known to the agent.

- **`snmpa:which_notifications/0`** -
  [which_notifications](`snmpa:which_notifications/0`) is used to retrieve a
  list of all (MIB) notifications/traps known to the agent.

- **`snmpa:restart_worker/0,1`** - [restart_worker](`snmpa:restart_worker/0`) is
  used to restart the worker process of a multi-threaded agent.

- **`snmpa:restart_set_worker/0,1`** -
  [restart_set_worker](`snmpa:restart_set_worker/0`) is used to restart the
  set-worker process of a multi-threaded agent.

- **`snmpa_local_db:print/0,1,2`** - For example, this function can show the
  counters `snmpInPkts` and `snmpOutPkts`.

Another useful way to debug the agent is to pretty-print the content of all the
tables and/or variables handled directly by the agent. This can be done by
simply calling:

`snmpa:print_mib_info/0`

See `snmpa:print_mib_info/0`, `snmpa:print_mib_tables/0` or
`snmpa:print_mib_variables/0` for more info.
