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
# epmd

Erlang Port Mapper Daemon

## Description

- ```text
  epmd [-d|-debug] [DbgExtra...] [-address Addresses]
    [-port No] [-daemon] [-relaxed_command_check]
  ```

  Starts the port mapper daemon.

- ```text
  epmd [-d|-debug] [-port No] [-names|-kill|-stop Name]
  ```

  Communicates with a running port mapper daemon.

This daemon acts as a name server on all hosts involved in distributed Erlang
computations. When an Erlang node starts, the node has a name and it obtains an
address from the host OS kernel. The name and address are sent to the `epmd`
daemon running on the local host. In a TCP/IP environment, the address consists
of the IP address and a port number. The node name is an atom on the form of
`Name@Node`. The job of the `epmd` daemon is to keep track of which node name
listens on which address. Hence, `epmd` maps symbolic node names to machine
addresses.

The TCP/IP `epmd` daemon only keeps track of the `Name` (first) part of an
Erlang node name. The `Host` part (whatever is after the `@`) is implicit in the
node name where the `epmd` daemon was contacted, as is the IP address where the
Erlang node can be reached. Consistent and correct TCP naming services are
therefore required for an Erlang network to function correctly.

> #### Note {: .info }
>
> On Windows the maximum number of nodes allowed in one epmd instance is 60.
> This is because of limitations in the current implementation. If you need more
> nodes, you should look into using and erlang based epmd implementation such as
> [Erlang EPMD](https://github.com/erlang/epmd).

- **Starting the port mapper daemon** - The daemon is started automatically by
  command [`erl(1)`](erl_cmd.md) if the node is to be distributed and no running
  instance is present. If automatically launched environment variables must be
  used to change the behavior of the daemon; see section
  [Environment Variables](epmd_cmd.md#environment_variables).

  If argument `-daemon` is not specified, `epmd` runs as a normal program with
  the controlling terminal of the shell in which it is started. Normally, it is
  to be run as a daemon.

  Regular startup options are described in section
  [Regular Options](epmd_cmd.md#daemon_flags).

  The `DbgExtra` options are described in section
  [DbgExtra Options](epmd_cmd.md#debug_flags).

- **Communicating with a running port mapper daemon** - Communicating with the
  running `epmd` daemon by the `epmd` program is done primarily for debugging
  purposes.

  The different queries are described in section
  [Interactive options](epmd_cmd.md#interactive_flags).

[](){: #daemon_flags }

## Regular Options

These options are available when starting the name server. The name server is
normally started automatically by command [`erl(1)`](erl_cmd.md) (if not already
available), but it can also be started at system startup.

- **`-address List`** - Lets this instance of `epmd` listen only on the
  comma-separated list of IP addresses and on the loopback address (which is
  implicitly added to the list if it has not been specified). This can also be
  set using environment variable `ERL_EPMD_ADDRESS`; see section
  [Environment Variables](epmd_cmd.md#environment_variables).

- **`-port No`** - Lets this instance of `epmd` listen to another TCP port than
  default 4369. This can also be set using environment variable `ERL_EPMD_PORT`;
  see section [Environment Variables](epmd_cmd.md#environment_variables).

- **`-d | -debug`** - Enables debug output. The more `-d` flags specified, the
  more debug output you will get (to a certain limit). This option is most
  useful when the `epmd` daemon is not started as a daemon.

- **`-daemon`** - Starts `epmd` detached from the controlling terminal. Logging
  ends up in syslog when available and correctly configured. If the `epmd`
  daemon is started at boot, this option is definitely to be used. It is also
  used when command `erl` automatically starts `epmd`.

- **`-relaxed_command_check`** - Starts the `epmd` program with relaxed command
  checking (mostly for backward compatibility). This affects the following:

  - With relaxed command checking, the `epmd` daemon can be killed from the
    local host with, for example, command `epmd -kill` even if active nodes are
    registered. Normally only daemons with an empty node database can be killed
    with `epmd -kill`.
  - Command `epmd -stop` (and the corresponding messages to `epmd`, as can be
    specified using [`erl_interface:ei(3)`](`e:erl_interface:ei.md`)) is
    normally always ignored. This because it can cause a strange situation where
    two nodes of the same name can be alive at the same time. A node unregisters
    itself by only closing the connection to `epmd`, which is why command `stop`
    was only intended for use in debugging situations.

    With relaxed command checking enabled, you can forcibly unregister live
    nodes.

  Relaxed command checking can also be enabled by setting environment variable
  `ERL_EPMD_RELAXED_COMMAND_CHECK` before starting `epmd`.

  Use relaxed command checking only on systems with very limited interactive
  usage.

[](){: #debug_flags }

## DbgExtra Options

> #### Note {: .info }
>
> These options are only for debugging and testing `epmd` clients. They are not
> to be used in normal operation.

- **`-packet_timeout Seconds`** - Sets the number of seconds a connection can be
  inactive before `epmd` times out and closes the connection. Defaults to 60.

- **`-delay_accept Seconds`** - To simulate a busy server, you can insert a
  delay between when `epmd` gets notified that a new connection is requested and
  when the connection gets accepted.

- **`-delay_write Seconds`** - Also a simulation of a busy server. Inserts a
  delay before a reply is sent.

[](){: #interactive_flags }

## Interactive Options

These options make `epmd` run as an interactive command, displaying the results
of sending queries to an already running instance of `epmd`. The `epmd`
contacted is always on the local node, but option `-port` can be used to select
between instances if several are running using different ports on the host.

- **`-port No`** - Contacts the `epmd` listening on the specified TCP port
  number (default 4369). This can also be set using environment variable
  `ERL_EPMD_PORT`; see section
  [Environment Variables](epmd_cmd.md#environment_variables).

- **`-names`** - Lists names registered with the currently running `epmd`.

- **`-kill`** - Kills the currently running `epmd`.

  Killing the running `epmd` is only allowed if `epmd -names` shows an empty
  database or if `-relaxed_command_check` was specified when the running
  instance of `epmd` was started.

  Notice that `-relaxed_command_check` is specified when starting the daemon
  that is to accept killing when it has live nodes registered. When running
  `epmd` interactively, `-relaxed_command_check` has no effect. A daemon that is
  started without relaxed command checking must be killed using, for example,
  signals or some other OS-specific method if it has active clients registered.

- **`-stop Name`** - Forcibly unregisters a live node from the `epmd` database.

  This command can only be used when contacting `epmd` instances started with
  flag `-relaxed_command_check`.

  Notice that relaxed command checking must enabled for the `epmd` daemon
  contacted. When running `epmd` interactively, `-relaxed_command_check` has no
  effect.

[](){: #environment_variables }

## Environment Variables

- **`ERL_EPMD_ADDRESS`** - Can be set to a comma-separated list of IP addresses,
  in which case the `epmd` daemon will listen only on the specified address(es)
  and on the loopback address (which is implicitly added to the list if it has
  not been specified). The default behavior is to listen on all available IP
  addresses.

- **`ERL_EPMD_PORT`** - Can contain the port number `epmd` will use. The default
  port will work fine in most cases. A different port can be specified to allow
  several instances of `epmd`, representing independent clusters of nodes, to
  co-exist on the same host. All nodes in a cluster must use the same `epmd`
  port number.

- **`ERL_EPMD_RELAXED_COMMAND_CHECK`** - If set before start, the `epmd` daemon
  behaves as if option `-relaxed_command_check` was specified at startup.
  Consequently, if this option is set before starting the Erlang virtual
  machine, the automatically started `epmd` accepts the `-kill` and `-stop`
  commands without restrictions.

## Logging

On some operating systems _syslog_ will be used for error reporting when `epmd`
runs as a daemon. To enable the error logging, you must edit the
/etc/syslog.conf file and add an entry:

```text
  !epmd
  *.*<TABs>/var/log/epmd.log
```

where `<TABs>` are at least one real tab character. Spaces are silently ignored.

## Access Restrictions

The `epmd` daemon accepts messages from both the local host and remote hosts.
However, only the query commands are answered (and acted upon) if the query
comes from a remote host. It is always an error to try to register a node name
if the client is not a process on the same host as the `epmd` instance is
running on. Such requests are considered hostile and the connection is closed
immediately.

The following queries are accepted from remote nodes:

- Port queries, that is, on which port the node with a specified name listens
- Name listing, that is, gives a list of all names registered on the host

To restrict access further, firewall software must be used.
