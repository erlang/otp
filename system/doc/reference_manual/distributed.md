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
# Distributed Erlang

## Distributed Erlang System

A _distributed Erlang system_ consists of a number of Erlang runtime systems
communicating with each other. Each such runtime system is called a _node_.
Message passing between processes at different nodes, as well as links and
monitors, are transparent when pids are used. Registered names, however, are
local to each node. This means that the node must be specified as well when
sending messages, and so on, using registered names.

The distribution mechanism is implemented using TCP/IP sockets. How to implement
an alternative carrier is described in the
[ERTS User's Guide](`e:erts:alt_dist.md`).

> #### Warning {: .warning }
>
> Starting a distributed node without also specifying
> [`-proto_dist inet_tls`](`e:erts:erl_cmd.md#proto_dist`) will expose the node
> to attacks that may give the attacker complete access to the node and in
> extension the cluster. When using un-secure distributed nodes, make sure that
> the network is configured to keep potential attackers out. See the
> [Using SSL for Erlang Distribution](`e:ssl:ssl_distribution.md`) User's Guide
> for details on how to setup a secure distributed node.

## Nodes

A _node_ is an executing Erlang runtime system that has been given a name, using
the command-line flag [`-name`](`e:erts:erl_cmd.md#name`) (long names) or
[`-sname`](`e:erts:erl_cmd.md#sname`) (short names).

The format of the node name is an atom `name@host`. `name` is the name given by
the user. `host` is the full host name if long names are used, or the first part
of the host name if short names are used. Function [`node()`](`erlang:node/0`)
returns the name of the node.

_Example:_

```erlang
% erl -name dilbert
(dilbert@uab.ericsson.se)1> node().
'dilbert@uab.ericsson.se'

% erl -sname dilbert
(dilbert@uab)1> node().
dilbert@uab
```

The node name can also be given in runtime by calling `net_kernel:start/1`.

_Example:_

```erlang
% erl
1> node().
nonode@nohost
2> net_kernel:start([dilbert,shortnames]).
{ok,<0.102.0>}
(dilbert@uab)3> node().
dilbert@uab
```

> #### Note {: .info }
>
> A node with a long node name cannot communicate with a node with a short node
> name.

## Node Connections

The nodes in a distributed Erlang system are loosely connected. The first time
the name of another node is used, for example, if
[`spawn(Node, M, F, A)`](`spawn/4`) or `net_adm:ping(Node)` is called, a connection
attempt to that node is made.

Connections are by default transitive. If a node A connects to node B, and node
B has a connection to node C, then node A also tries to connect to node C. This
feature can be turned off by using the command-line flag `-connect_all false`,
see [erl](`e:erts:erl_cmd.md`) in ERTS.

If a node goes down, all connections to that node are removed. Calling
[`erlang:disconnect_node(Node)`](`erlang:disconnect_node/1`) forces
disconnection of a node.

The list of (visible) nodes currently connected to is returned by `nodes/0`.

## epmd

The Erlang Port Mapper Daemon _epmd_ is automatically started at every host
where an Erlang node is started. It is responsible for mapping the symbolic node
names to machine addresses. See the [epmd](`e:erts:epmd_cmd.md`) in ERTS.

## Hidden Nodes

In a distributed Erlang system, it is sometimes useful to connect to a
node without also connecting to all other nodes. An example is some
kind of Operation and Maintenance functionality used to inspect the
status of a system, without disturbing it. For this purpose, a _hidden
node_ can be used.

A hidden node is a node started with the command-line flag `-hidden`.
Connections between hidden nodes and other nodes are not transitive, they must
be set up explicitly. Also, hidden nodes does not show up in the list of nodes
returned by `nodes/0`. Instead, [`nodes(hidden)`](`nodes/1`) or
[`nodes(connected)`](`nodes/1`) must be used. This means, for example, that the
hidden node is not added to the set of nodes that `m:global` is keeping track of.

[](){: #dyn_node_name }

## Dynamic Node Name

If the node name is set to _`undefined`_ the node will be started in a special
mode to be the temporary client of another node. The node will then request a
dynamic node name from the first node it connects to. In addition these
distribution settings will be set:

```text
-dist_listen false -hidden -kernel dist_auto_connect never
```

As `-dist_auto_connect` is set to `never`, `net_kernel:connect_node/1` must be
called in order to setup connections. If the first established connection is
closed (which gave the node its dynamic name), then any other connections will
also be closed and the node will lose its dynamic node name. A new call to
`net_kernel:connect_node/1` can be made to get a new dynamic node name. The node
name may change if the distribution is dropped and then set up again.

> #### Change {: .info }
>
> The _dynamic node name_ feature is supported from Erlang/OTP 23. Both the
> temporary client node and the first connected peer node (supplying the dynamic
> node name) must be at least Erlang/OTP 23 for it to work.

## C Nodes

A _C node_ is a C program written to act as a hidden node in a distributed
Erlang system. The library _Erl_Interface_ contains functions for this purpose.
For more information about C nodes, see the
[Erl_Interface](`e:erl_interface:ei_users_guide.md`) application and
[Interoperability Tutorial.](`e:system:tutorial.md`).

## Security

> #### Note {: .info }
>
> "Security" here does _not_ mean cryptographically secure, but rather security
> against accidental misuse, such as preventing a node from connecting to a
> cluster with which it is not intended to communicate.
>
> Furthermore, the communication between nodes is per default in clear text. If
> you need strong security, please see
> [Using TLS for Erlang Distribution ](`e:ssl:ssl_distribution.md`)in the SSL
> application's User's Guide.
>
> Also, the default random cookie mentioned in the following text is not very
> unpredictable. A better one can be generated using primitives in the `crypto`
> module, though this still does not make the initial handshake
> cryptographically secure. And inter-node communication is still in clear text.

Authentication determines which nodes are allowed to communicate with each
other. In a network of different Erlang nodes, it is built into the system at
the lowest possible level. All nodes use a _magic cookie_, which is an Erlang
atom, when connecting another node.

During the connection setup, after node names have been exchanged, the magic
cookies the nodes present to each other are compared. If they do not match, the
connection is rejected. The cookies themselves are never transferred, instead
they are compared using hashed challenges, although not in a cryptographically
secure manner.

At start-up, a node has a random atom assigned as its default magic cookie and
the cookie of other nodes is assumed to be `nocookie`. The first action of the
Erlang network authentication server (`auth`) is then to search for a file named
`.erlang.cookie` in the [user's home directory](`m:init#home`) and then in
[`filename:basedir(user_config, "erlang")`](`m:filename#user_config`). If none
of the files exist, a `.erlang.cookie` file is created in the user's home
directory. The UNIX permissions mode of the file is set to octal 400 (read-only
by user) and its content is a random string. An atom `Cookie` is created from
the contents of the file and the cookie of the local node is set to this using
`erlang:set_cookie(Cookie)`. This sets the default cookie that the local node
will use for all other nodes.

Thus, groups of users with identical cookie files get Erlang nodes that can
communicate freely since they use the same magic cookie. Users who want to run
nodes where the cookie files are on different file systems must make certain
that their cookie files are identical.

For a node `Node1` using magic cookie `Cookie` to be able to connect to, and to
accept a connection from, another node `Node2` that uses a different cookie
`DiffCookie`, the function `erlang:set_cookie(Node2, DiffCookie)` must first be
called at `Node1`. Distributed systems with multiple home directories (differing
cookie files) can be handled in this way.

> #### Note {: .info }
>
> With this setup `Node1` and `Node2` agree on which cookie to use: `Node1` uses
> its explicitly configured `DiffCookie` for `Node2`, and `Node2` uses its
> default cookie `DiffCookie`.
>
> You can also use a `DiffCookie` that neither `Node1` nor `Node2` has as its
> default cookie, if you also call `erlang:set_cookie(Node1, DiffCookie)` in
> `Node2` before establishing connection
>
> Because node names are exchanged during connection setup before cookies are
> selected, connection setup works regardless of which node that initiates it.
>
> Note that to configure `Node1` to use `Node2`'s default cookie when
> communicating with `Node2`, _and vice versa_ results in a broken configuration
> (if the cookies are different) because then both nodes use the other node's
> (differing) cookie.

The default when a connection is established between two nodes, is to
immediately connect all other visible nodes as well. This way, there is always a
fully connected network. If there are nodes with different cookies, this method
can be inappropriate (since it may not be feasible to configure different
cookies for all possible nodes) and the command-line flag `-connect_all false`
must be set, see the [erl](`e:erts:erl_cmd.md`) executable in ERTS.

The magic cookie of the local node can be retrieved by calling
`erlang:get_cookie()`.

## Distribution BIFs

Here are some BIFs that are useful for distributed programming:

- [`disconnect_node(Node)`](`erlang:disconnect_node/1`) - Forces the
  disconnection of a node.

- `erlang:get_cookie/0` - Returns the magic cookie of the current
  node.

- [`erlang:get_cookie(Node)`](`erlang:get_cookie/1`) - Returns the
  magic cookie for node `Node`.

- `is_alive/0` - Returns `true` if the runtime system is a node and
  can connect to other nodes, `false` otherwise.

- [`monitor_node(Node, Bool)`](`erlang:monitor_node/2`) - Monitors the
  status of `Node`. A message`{nodedown, Node}` is received if the
  connection to it is lost.

- `node/0` - Returns the name of the current node. Allowed in guards.

- [`node(Arg)`](`node/1`) - Returns the node where `Arg`, a pid,
  reference, or port, is located.

- `nodes/0` - Returns a list of all visible nodes this node is connected to.

- [`nodes(Arg)`](`nodes/1`) - Depending on `Arg`, this function can
  return a list not only of visible nodes, but also hidden nodes and
  previously known nodes, and so on.

- [`erlang:set_cookie(Cookie)`](`erlang:set_cookie/1`) - Sets the
  magic cookie, `Cookie` to use when connecting all nodes that have no
  explicit cookie set with `erlang:set_cookie/2`.

- [`erlang:set_cookie(Node, Cookie)`](`erlang:set_cookie/2`) - Sets
  the magic cookie used when connecting `Node`. If `Node` is the
  current node, `Cookie` is used when connecting all nodes that have
  no explicit cookie set with this function.

- [`spawn_link(Node, Fun)`](`spawn_link/2`) - Creates a process at a remote node.

- [`spawn_opt(Node, Fun, Opts)`](`spawn_opt/3`) - Creates a process at
  a remote node.

- [`spawn_link(Node, Module, Name, Args)`](`erlang:spawn_link/4`) -
  Creates a process at a remote node.

- [`spawn_opt(Node, Module, Name, Args, Opts)`](`erlang:spawn_opt/5`) - Creates
  a process at a remote node.

_Table: Distribution BIFs_

## Distribution Command-Line Flags

Examples of command-line flags used for distributed programming (for more
information, see the [erl](`e:erts:erl_cmd.md`) executable in ERTS):

| _Command-Line Flag_      | _Description_                                               |
| ------------------------ | ----------------------------------------------------------- |
| `-connect_all false`     | Only explicit connection setups are used.                   |
| `-hidden`                | Makes a node into a hidden node.                            |
| `-name Name`             | Makes a runtime system into a node, using long node names.  |
| `-setcookie Cookie`      | Same as calling `erlang:set_cookie(Cookie)`.                |
| `-setcookie Node Cookie` | Same as calling `erlang:set_cookie(Node, Cookie)`.          |
| `-sname Name`            | Makes a runtime system into a node, using short node names. |

_Table: Distribution Command-Line Flags_

## Distribution Modules

Examples of modules useful for distributed programming in the Kernel application:

| _Module_         | _Description_                                      |
| ---------------- | -------------------------------------------------- |
| `m:global`       | A global name registration facility.               |
| `m:global_group` | Grouping nodes to global name registration groups. |
| `m:net_adm`      | Various Erlang net administration routines.        |
| `m:net_kernel`   | Erlang networking kernel.                          |

_Table: Kernel Modules Useful For Distribution._

In the STDLIB application:

| _Module_ | _Description_                     |
| -------- | --------------------------------- |
| `m:peer`  | Start and control of peer nodes. |

_Table: STDLIB Modules Useful For Distribution._
