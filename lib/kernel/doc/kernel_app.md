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
# Kernel Application

## Description

The Kernel application has all the code necessary to run the Erlang runtime
system: file servers, code servers, and so on.

The Kernel application is the first application started. It is mandatory in the
sense that the minimal system based on Erlang/OTP consists of Kernel and STDLIB.
Kernel contains the following functional areas:

- Start, stop, supervision, configuration, and distribution of applications
- Code loading
- Logging
- Global name service
- Supervision of Erlang/OTP
- Communication with sockets
- Operating system interface

## Logger Handlers

Two standard logger handlers are defined in the Kernel application. These are
described in the [Kernel User's Guide](logger_chapter.md), and in the
`m:logger_std_h` and [`logger_disk_log_h` ](`m:logger_disk_log_h`)manual pages.

[](){: #erl_signal_server }

## OS Signal Event Handler

Asynchronous OS signals may be subscribed to via the Kernel applications event
manager (see [OTP Design Principles](`e:system:design_principles.md`) and
`m:gen_event`) registered as `erl_signal_server`. A default signal handler is
installed which handles the following signals:

- **`sigusr1`** - The default handler will halt Erlang and produce a crashdump
  with slogan "Received SIGUSR1". This is equivalent to calling
  `erlang:halt("Received SIGUSR1")`.

- **`sigquit`** - The default handler will halt Erlang immediately. This is
  equivalent to calling `erlang:halt()`.

- **`sigterm`** - The default handler will terminate Erlang normally. This is
  equivalent to calling `init:stop()`.

### Events

Any event handler added to `erl_signal_server` must handle the following events.

- **`sighup`** - Hangup detected on controlling terminal or death of controlling
  process

- **`sigquit`** - Quit from keyboard

- **`sigabrt`** - Abort signal from abort

- **`sigalrm`** - Timer signal from alarm

- **`sigterm`** - Termination signal

- **`sigusr1`** - User-defined signal 1

- **`sigusr2`** - User-defined signal 2

- **`sigchld`** - Child process stopped or terminated

- **`sigstop`** - Stop process

- **`sigtstp`** - Stop typed at terminal

Setting OS signals are described in `os:set_signal/2`.

## Configuration

The following configuration parameters are defined for the Kernel application.
For more information about configuration parameters, see file
[`app`](app.md).

- **`connect_all = true | false`{: #connect_all }** - If enabled (`true`), which
  also is the default, `m:global` will actively connect to all nodes that
  becomes known to it. Note that you also want to enable
  [`prevent_overlapping_partitions`](kernel_app.md#prevent_overlapping_partitions)
  in order for `global` to ensure that a fully connected network is maintained.
  `prevent_overlapping_partitions` will also prevent inconsistencies in
  `global`'s name registration and locking.

  The now deprecated command line argument
  [`-connect_all <boolean>`](`e:erts:erl_cmd.md#connect_all`) has the same
  effect as the `connect_all` configuration parameter. If this configuration
  parameter is defined, it will override the command line argument.

- **`distributed = [Distrib]`{: #distributed }** - Specifies which applications
  that are distributed and on which nodes they are allowed to execute. In this
  parameter:

  - `Distrib = {App,Nodes} | {App,Time,Nodes}`
  - `App = atom()`
  - `Time = integer()>0`
  - `Nodes = [node() | {node(),...,node()}]`

  The parameter is described in `application:load/2`.

- **`dist_auto_connect = Value`{: #dist_auto_connect }** - Specifies when nodes
  are automatically connected. If this parameter is not specified, a node is
  always automatically connected, for example, when a message is to be sent to
  that node. `Value` is one of:

  - **`never`** - Connections are never automatically established, they must be
    explicitly connected. See `m:net_kernel`.

  - **`once`** - Connections are established automatically, but only once per
    node. If a node goes down, it must thereafter be explicitly connected. See
    `m:net_kernel`.

- **`permissions = [Perm]`{: #permissions }** - Specifies the default permission
  for applications when they are started. In this parameter:

  - `Perm = {ApplName,Bool}`
  - `ApplName = atom()`
  - `Bool = boolean()`

  Permissions are described in `application:permit/2`.

- **`logger = [Config]`{: #logger }** - Specifies the configuration for
  [Logger](`m:logger`), except the primary log level, which is specified with
  [`logger_level`](kernel_app.md#logger_level), and the compatibility with
  [SASL Error Logging](`e:sasl:error_logging.md`), which is specified with
  [`logger_sasl_compatible`](kernel_app.md#logger_sasl_compatible).

  The `logger `parameter is described in section
  [Logging](logger_chapter.md#logger_parameter) in the Kernel User's Guide.

- **`logger_level = Level`{: #logger_level }** - Specifies the primary log level
  for Logger. Log events with the same, or a more severe level, pass through the
  primary log level check. See section [Logging](logger_chapter.md) in the
  Kernel User's Guide for more information about Logger and log levels.

  `Level = emergency | alert | critical | error | warning | notice | info | debug | all | none`

  To change the primary log level at runtime, use
  [`logger:set_primary_config(level, Level)`](`logger:set_primary_config/2`).

  Defaults to `notice`.

- **`logger_metadata = Metadata`{: #logger_metadata }** - Specifies primary
  metadata for log events.

  `Metadata = map()`

  Defaults to `#{}`.

- **`logger_sasl_compatible = true | false`{: #logger_sasl_compatible }** -
  Specifies if Logger behaves backwards compatible with the SASL error logging
  functionality from releases prior to Erlang/OTP 21.0.

  If this parameter is set to `true`, the default Logger handler does not log
  any progress-, crash-, or supervisor reports. If the SASL application is then
  started, it adds a Logger handler named `sasl`, which logs these events
  according to values of the SASL configuration parameter `sasl_error_logger`
  and `sasl_errlog_type`.

  See section
  [Deprecated Error Logger Event Handlers and Configuration](`e:sasl:sasl_app.md#deprecated_error_logger_config`)
  in the sasl(6) manual page for information about the SASL configuration
  parameters.

  See section [SASL Error Logging](`e:sasl:error_logging.md`) in the SASL User's
  Guide, and section
  [Backwards Compatibility with error_logger](logger_chapter.md#compatibility)
  in the Kernel User's Guide for information about the SASL error logging
  functionality, and how Logger can be backwards compatible with this.

  Defaults to `false`.

  > #### Note {: .info }
  >
  > If this parameter is set to `true`, `sasl_errlog_type` indicates that
  > progress reports shall be logged, and the configured primary log level is
  > `notice` or more severe, then SASL automatically sets the primary log level
  > to `info`. That is, this setting can potentially overwrite the value of the
  > Kernel configuration parameter `logger_level`. This is to allow progress
  > reports, which have log level `info`, to be forwarded to the handlers.

- **`global_groups = [GroupTuple]`{: #global_groups }** - Defines global groups,
  see `m:global_group`. In this parameter:

  - `GroupTuple = {GroupName, [Node]} | {GroupName, PublishType, [Node]}`
  - `GroupName = atom()`
  - `PublishType = normal | hidden`
  - `Node = node()`

- **`inet_default_connect_options = [{Opt, Val}]`{: #inet_default_connect_options }** - Specifies default options for `connect`
  sockets, see `m:inet`.

- **`inet_default_listen_options = [{Opt, Val}]`{: #inet_default_listen_options
  }** - Specifies default options for `listen` (and `accept`) sockets, see
  `m:inet`.

- **`inet_dist_use_interface = ip_address()`{: #inet_dist_use_interface }** - If
  the host of an Erlang node has many network interfaces, this parameter
  specifies which one to listen on. For the type definition of `ip_address()`,
  see `m:inet`.

- **`inet_dist_listen_min = First`{: #inet_dist_listen }  
  `inet_dist_listen_max = Last`**  
  Defines the `First..Last` port range for the listener socket of a distributed
  Erlang node.

- **`inet_dist_listen_options = Opts`{: #inet_dist_listen_options }** - Defines
  a list of extra socket options to be used when opening the listening socket
  for a distributed Erlang node. See `gen_tcp:listen/2`.

- **`inet_dist_connect_options = Opts`{: #inet_dist_connect_options }** -
  Defines a list of extra socket options to be used when connecting to other
  distributed Erlang nodes. See `gen_tcp:connect/4`.

- **`inet_parse_error_log = silent`{: #inet_parse_error_log }** - If set, no log
  events are issued when erroneous lines are found and skipped in the various
  Inet configuration files.

- **`inetrc = Filename`{: #inetrc }** - The name (string) of an Inet user
  configuration file. For details, see section
  [`Inet Configuration`](`e:erts:inet_cfg.md`) in the ERTS User's Guide.

- **`net_setuptime = SetupTime`{: #net_setuptime }** -
  `SetupTime` must be a positive integer or floating point number, and is
  interpreted as the maximum allowed time for each network operation during
  connection setup to another Erlang node. The maximum allowed value is `120`.
  If higher values are specified, `120` is used. Default is 7 seconds if the
  variable is not specified, or if the value is incorrect (for example, not a
  number).

  Notice that this value does not limit the total connection setup time, but
  rather each individual network operation during the connection setup and
  handshake.

- **`net_ticker_spawn_options = Opts`{: #net_ticker_spawn_options }** - Defines
  a list of extra spawn options for net ticker processes. There exist one such
  process for each connection to another node. A net ticker process is
  responsible for supervising the connection it is associated with. These
  processes also execute the distribution handshake protocol when setting up
  connections. When there is a large number of distribution connections, setting
  up garbage collection options can be helpful to reduce memory usage. Default
  is `[link, {priority, max}]`, and these two options cannot be changed. The
  `monitor` and `{monitor, MonitorOpts}` options are not allowed and will be
  dropped if present. See the documentation of the `erlang:spawn_opt/4` BIF for
  information about valid options. If the `Opts` list is not a proper list, or
  containing invalid options the setup of connections will fail.

  Note that the behavior described above is only true if the distribution
  carrier protocol used is implemented as described in
  [ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](`e:erts:alt_dist.md#distribution-module`)
  without further alterations. The implementer of the distribution carrier
  protocol used, may have chosen to ignore the `net_ticker_spawn_options`
  parameter or altered its behavior. Currently all distribution modules shipped
  with OTP do, however, behave as described above.

- **`net_tickintensity = NetTickIntensity`{: #net_tickintensity }** - _Net tick
  intensity_ specifies how many ticks to send during a
  [net tick time](kernel_app.md#net_ticktime) period when no other data is sent
  over a connection to another node. This also determines how often to check for
  data from the other node. The higher net tick intensity, the closer to the
  chosen net tick time period the node will detect an unresponsive node. The net
  tick intensity defaults to `4`. The value of `NetTickIntensity` should be an
  integer in the range `4..1000`. If the `NetTickIntensity` is not an integer or
  an integer less than `4`, `4` will silently be used. If `NetTickIntensity` is
  an integer larger than `1000`, `1000` will silently be used.

  > #### Note {: .info }
  >
  > Note that all communicating nodes are expected to use the same _net tick
  > intensity_ as well as the same _net tick time_.

  > #### Warning {: .warning }
  >
  > Be careful not to set a too high net tick intensity, since you can overwhelm
  > the node with work if it is set too high.

- **`net_ticktime = NetTickTime`{: #net_ticktime }** - Specifies the _net tick
  time_ in seconds. This is the approximate time a connected node may be
  unresponsive until it is considered down and thereby disconnected.

  Net tick time together with
  [net tick intensity](kernel_app.md#net_tickintensity) determines an interval
  `TickInterval = NetTickTime/NetTickIntensity`. Once every `TickInterval`
  seconds, each connected node is ticked if nothing has been sent to it during
  that last `TickInterval` seconds. A tick is a small package sent on the
  connection. A connected node is considered to be down if no ticks or payload
  packages have been received during the last `NetTickIntensity` number of
  `TickInterval` seconds intervals. This ensures that nodes that are not
  responding, for reasons such as hardware errors, are considered to be down.

  As the availability is only checked every `TickInterval` seconds, the actual
  time `T` a node have been unresponsive when detected may vary between `MinT`
  and `MaxT`, where:

  ```c
  MinT = NetTickTime - NetTickTime / NetTickIntensity
  MaxT = NetTickTime + NetTickTime / NetTickIntensity
  ```

  `NetTickTime` defaults to `60` seconds and `NetTickIntensity` defaults to `4`.
  Thus, `45 < T < 75` seconds.

  > #### Note {: .info }
  >
  > Notice that _all_ communicating nodes are to have the _same_ `NetTickTime`
  > and `NetTickIntensity` values specified, as it determines both the frequency
  > of outgoing ticks and the expected frequency of incominging ticks.

  `NetTickTime` needs to be a multiple of `NetTickIntensity`. If the configured
  values are not, `NetTickTime` will internally be rounded up to the nearest
  millisecond.
  [`net_kernel:get_net_ticktime()`](`net_kernel:get_net_ticktime/0`) will,
  however, report net tick time truncated to the nearest second.

  Normally, a terminating node is detected immediately by the transport protocol
  (like TCP/IP).

- **`prevent_overlapping_partitions = true | false`{:
  #prevent_overlapping_partitions }** - If enabled (`true`), `global` will
  actively prevent overlapping partitions from forming when connections are lost
  between nodes. This fix is enabled by default. If you are about to disable
  this fix, make sure to read the
  [`global`](`m:global#prevent_overlapping_partitions`) documentation about this
  fix for more important information about this.

- **`shutdown_timeout = integer() | infinity`{: #shutdown_timeout }** -
  Specifies the time `application_controller` waits for an application to
  terminate during node shutdown. If the timer expires, `application_controller`
  brutally kills `application_master` of the hanging application. If this
  parameter is undefined, it defaults to `infinity`.

- **`sync_nodes_mandatory = [NodeName]`{: #sync_nodes_mandatory }** - Specifies
  which other nodes that _must_ be alive for this node to start properly. If
  some node in the list does not start within the specified time, this node does
  not start either. If this parameter is undefined, it defaults to `[]`.

- **`sync_nodes_optional = [NodeName]`{: #sync_nodes_optional }** - Specifies
  which other nodes that _can_ be alive for this node to start properly. If some
  node in this list does not start within the specified time, this node starts
  anyway. If this parameter is undefined, it defaults to the empty list.

- **`sync_nodes_timeout = integer() | infinity`{: #sync_nodes_timeout }** -
  Specifies the time (in milliseconds) that this node waits for the mandatory
  and optional nodes to start. If this parameter is undefined, no node
  synchronization is performed. This option ensures that `global` is
  synchronized.

- **`start_distribution = true | false`{: #start_distribution }** - Starts all
  distribution services, such as `rpc`, `global`, and `net_kernel` if the
  parameter is `true`. This parameter is to be set to `false` for systems who
  want to disable all distribution functionality.

  Defaults to `true`.

- **`start_dist_ac = true | false`{: #start_dist_ac }** - Starts the `dist_ac`
  server if the parameter is `true`. This parameter is to be set to `true` for
  systems using distributed applications.

  Defaults to `false`. If this parameter is undefined, the server is started if
  parameter `distributed` is set.

- **`start_boot_server = true | false`{: #start_boot_server }** - Starts the
  `boot_server` if the parameter is `true` (see `m:erl_boot_server`). This
  parameter is to be set to `true` in an embedded system using this service.

  Defaults to `false`.

- **`boot_server_slaves = [SlaveIP]`{: #boot_server_slaves }** - If
  configuration parameter `start_boot_server` is `true`, this parameter can be
  used to initialize `boot_server` with a list of slave IP addresses:

  `SlaveIP = string() | atom | {integer(),integer(),integer(),integer()}`,

  where `0 <= integer() <=255`.

  Examples of `SlaveIP` in atom, string, and tuple form:

  `'150.236.16.70', "150,236,16,70", {150,236,16,70}`.

  Defaults to `[]`.

- **`start_disk_log = true | false`{: #start_disk_log }** - Starts the
  `disk_log_server` if the parameter is `true` (see `m:disk_log`). This
  parameter is to be set to `true` in an embedded system using this service.

  Defaults to `false`.

- **`start_pg = true | false`{: #start_pg }** - Starts the
  default `pg` scope server (see `m:pg`) if the parameter is `true`. This
  parameter is to be set to `true` in an embedded system that uses this service.

  Defaults to `false`.

- **`start_timer = true | false`{: #start_timer }** - Starts the `timer_server`
  if the parameter is `true` (see `m:timer`). This parameter is to be set to
  `true` in an embedded system using this service.

  Defaults to `false`.

- **`shell_history = enabled | disabled | module()`{: #shell_history }** -
  Specifies whether shell history should be logged to disk between usages of
  `erl` (`enabled`), not logged at all (`disabled`), or a user-specified module
  will be used to log shell history. This module should export
  `load() -> [string()]` returning a list of strings to load in the shell when
  it starts, and `add(iodata()) -> ok.` called every time new line is entered in
  the shell. By default logging is disabled.

- **`shell_history_drop = [string()]`{: #shell_history_drop }** - Specific log
  lines that should not be persisted. For example `["q().", "init:stop()."]`
  will allow to ignore commands that shut the node down. Defaults to `[]`.

- **`shell_history_file_bytes = integer()`{: #shell_history_file_bytes }** - How
  many bytes the shell should remember. By default, the value is set to 512kb,
  and the minimal value is 50kb.

- **`shell_history_path = string()`{: #shell_history_path }** - Specifies where
  the shell history files will be stored. defaults to the user's cache directory
  as returned by `filename:basedir(user_cache, "erlang-history")`.

- **`shutdown_func = {Mod :: atom(), Func :: atom()}`{: #shutdown_func }** -
  Sets a function that `application_controller` calls when it starts to
  terminate. The function is called as `Mod:Func(Reason)`, where `Reason` is the
  terminate reason for `application_controller`, and it must return as soon as
  possible for `application_controller` to terminate properly.

- **`source_search_rules = [DirRule] | [SuffixRule]`{: #source_search_rules }**

  Where:

  - `DirRule = {ObjDirSuffix,SrcDirSuffix}`
  - `SuffixRule = {ObjSuffix,SrcSuffix,[DirRule]}`
  - `ObjDirSuffix = string()`
  - `SrcDirSuffix = string()`
  - `ObjSuffix = string()`
  - `SrcSuffix = string()`

  Specifies a list of rules for use by `filelib:find_file/2`
  `filelib:find_source/2` If this is set to some other value than the empty
  list, it replaces the default rules. Rules can be simple pairs of directory
  suffixes, such as `{"ebin", "src"}`, which are used by `filelib:find_file/2`,
  or triples specifying separate directory suffix rules depending on file name
  extensions, for example `[{".beam", ".erl", [{"ebin", "src"}]}`, which are
  used by `filelib:find_source/2`. Both kinds of rules can be mixed in the list.

  The interpretation of `ObjDirSuffix` and `SrcDirSuffix` is as follows: if the
  end of the directory name where an object is located matches `ObjDirSuffix`,
  then the name created by replacing `ObjDirSuffix` with `SrcDirSuffix` is
  expanded by calling `filelib:wildcard/1`, and the first regular file found
  among the matches is the source file.

- **`standard_io_encoding = Encoding`{: #standard_io_encoding }** - Set whether
  bytes sent or received via standard_io should be interpreted as unicode or
  latin1. By default input and output is interpreted as Unicode if it is
  supported on the host. With this flag you may configure the encoding on
  startup.

  This works similarly to
  [`io:setopts(standard_io, {encoding, Encoding})`](`io:setopts/2`) but is
  applied before any bytes on standard_io may have been read.

  Encoding is one of:

  - **`unicode`** - Configure standard_io to use unicode mode.

  - **`latin1`** - Configure standard_io to use latin1 mode.

  - **`_`** - Anything other than unicode or latin1 will be ignored and the
    system will configure the encoding by itself, typically unicode on modern
    systems.

  See
  [Escripts and non-interactive I/O in Unicode Usage in Erlang](`e:stdlib:unicode_usage.md#escripts-and-non-interactive-i-o`)
  for more details.

## Deprecated Configuration Parameters

In Erlang/OTP 21.0, a new API for logging was added. The old `error_logger`
event manager, and event handlers running on this manager, still work, but they
are no longer used by default.

The following application configuration parameters can still be set, but they
are only used if the corresponding configuration parameters for Logger are not
set.

- **`error_logger`** - Replaced by setting the [`type`](`m:logger_std_h#type`),
  and possibly [`file`](`m:logger_std_h#file`) and
  [`modes`](`m:logger_std_h#modes`) parameters of the default `logger_std_h`
  handler. Example:

  ```text
  erl -kernel logger '[{handler,default,logger_std_h,#{config=>#{file=>"/tmp/erlang.log"}}}]'
  ```

- **`error_logger_format_depth`**{: #error_logger_format_depth } - Replaced by setting the
  [`depth`](`m:logger_formatter#depth`) parameter of the default handlers
  formatter. Example:

  ```text
  erl -kernel logger '[{handler,default,logger_std_h,#{formatter=>{logger_formatter,#{legacy_header=>true,template=>[{logger_formatter,header},"\n",msg,"\n"],depth=>10}}}]'
  ```

See [Backwards compatibility with error_logger](logger_chapter.md#compatibility)
for more information.
