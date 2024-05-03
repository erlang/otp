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
# Observer

## Introduction

Observer is a graphical tool for observing the characteristics of Erlang
systems. Observer displays system information, application supervisor trees,
process information, ETS tables, Mnesia tables and contains a front end for
Erlang tracing.

## Getting Started

Run Observer from a standalone node to minimize the impact of the system being
observed.

_Example:_

```text
% erl -sname observer -hidden -setcookie MyCookie -run observer
```

Select the node to observe with menu _Nodes_. Menu _View > Refresh interval_
controls how often the view is to be updated. The refresh interval is set per
viewer so you can have different settings for each viewer. To minimize the
system impact, only the active viewer is updated. Other views are updated when
activated.

The mouse buttons behave as expected. Use left-click to select objects,
right-click to get a menu with the most used options, and double-click to
display information about the selected object. In most viewers with many
columns, you can change the sort order by left-clicking the column header.

## System Tab

Tab _System_ displays general information about the active Erlang node and its
runtime system, such as build configuration, system capabilities, and overall
use statistics.

## Load Charts Tab

Tab _Load Charts_ displays graphs of the current resource use on the active
Erlang node.

Graph `Scheduler Utilization` shows scheduler use per scheduler, where each
scheduler use has a unique color.

Graph `Memory Usage` shows the total memory use and per memory category use,
where each category has a unique color. The categories are as follows:

- **`Total`** - The sum of all memory categories.

- **`Processes`** - The sum of all process memory used.

- **`Atom`** - The size used by the atom table.

- **`Binary`** - The sum of all off-heap binaries allocated.

- **`Code`** - The memory allocated for code storage.

- **`Ets`** - The used memory for all ETS tables.

Graph `IO Usage` shows the current I/O load on the system.

## Memory Allocators Tab

Tab _Memory Allocators_ displays detailed information of the carrier size and
current memory carriers. For details about memory carriers, see module
[`erts_alloc`](`e:erts:erts_alloc.md`) in application ERTS.

The `Max Carrier size` column shows the maximum value seen by observer since the
last node change or since the start of the application, i.e. switching nodes
will reset the max column. Values are sampled so higher values may have existed
than what is shown.

## Applications Tab

Tab _Applications_ presents application information. Select an application in
the left list to display its supervisor tree. The right-click options in the
tree are as follows:

- **Process info** - Opens a detailed information window on the selected
  process, including the following:

  - **Process Information** - Shows the process information.

  - **Messages** - Shows the process messages.

  - **Dictionary** - Shows the process dictionary.

  - **Stack Trace** - Shows the process current stack trace.

  - **State** - Shows the process state.

  - **Log** - If enabled and available, shows the process SASL log entries.

- **Trace process** - Adds the selected process identifier to tab _Trace
  Overview_ plus the node that the process resides on.

- **Trace named process** - Adds the registered name of the process. This can be
  useful when tracing on many nodes, as processes with that name are then traced
  on all traced nodes.

- **Trace process tree** - Adds the selected process and all processes below,
  right of it, to tab _Trace Overview_.

- **Trace named process tree** - Adds the selected process and all processes
  below, right of it, to tab _Trace Overview_.

## Processes Tab

Tab _Processes_ lists process information in columns. For each process the
following information is displayed:

- **Pid** - The process identifier.

- **Description** - Registered name, [process label](`proc_lib:set_label/1`) or
  initial function.

- **Reds** - The number of reductions executed on the process. This can be
  presented as accumulated values or as values since the last update.

- **Memory** - The size of the process, in bytes, obtained by a call to
  [`process_info(Pid,memory)`](`process_info/2`).

- **MsgQ** - The length of the message queue for the process.

Option _Process info_ opens a detailed information window on the process under
the mouse pointer, including the following:

- **Process Information** - Shows the process information.

- **Messages** - Shows the process messages.

- **Dictionary** - Shows the process dictionary.

- **Stack Trace** - Shows the process current stack trace.

- **State** - Shows the process state.

- **Log** - If enabled and available, shows the process SASL log entries.

> #### Note {: .info }
>
> _Log_ requires application SASL to be started on the observed node, with
> `log_mf_h` as log handler. The Observed node must be Erlang/OTP R16B02 or
> higher. The `rb` server must not be started on the observed node when clicking
> menu _Log > Toggle log view_. The `rb` server is stopped on the observed node
> when exiting or changing the observed node.

Option _Trace selected processes_ adds the selected process identifiers to tab
_Trace Overview_ plus the node that the processes reside on.

Option _Trace selected processes by name_ adds the registered name of the
processes. This can be useful when tracing is done on many nodes, as processes
with that name are then traced on all traced nodes.

Option _Kill process_ brutally kills the processes under the mouse pointer by
sending an exit signal with reason `kill`.

## Ports Tab

Tab _Ports_ lists port information in columns. For each port the following
information is displayed:

- **Id** - The port identifier.

- **Connected** - The process identifier for the process that owns the port.

- **Name** - The registered name of the port, if any.

- **Controls** - The name of the command set by `erlang:open_port/2`.

- **Slot** - The internal index of the port.

Option _Port info_ opens a detailed information window for the port under the
mouse pointer. In addition to the information above, it also shows links and
monitors.

Option _Trace selected ports_ adds the selected port identifiers, and the nodes
that the ports reside on, to tab _Trace Overview_.

Option _Trace selected ports by name_ adds the registered name of the port to
tab _Trace Overview_. This can be useful when tracing is done on many nodes, as
ports with that name are then traced on all traced nodes.

Option _Close_ executes `erlang:port_close/1` on the port under the mouse
pointer.

## Sockets Tab

Tab _Sockets_ is divided into two parts. The first part contains general
`m:socket` information and the second part lists socket information in columns.

For each socket the following information is displayed:

- **Id** - The socket identifier.

- **Owner** - The process identifier for the process that owns the socket.

- **Fd** - The underlying file descriptor of the socket.

- **Domain** - The communication domain (e.g. inet or inet6) of this socket.

- **Type** - The type (e.g. stream or dgram) of this socket.

- **Protocol** - The protocol (e.g. tcp or udp) of this socket.

- **Read State** - The read state of the socket.

- **Write State** - The write state of the socket.

Option _Socket info_ opens a detailed information window for the socket under
the mouse pointer. In addition to the information above, it also shows monitors.

Option _Close_ executes `socket:close/1` on the socket under the mouse pointer.

## Table Viewer Tab

Tab _Table Viewer_ lists tables. By default, ETS tables are displayed whereas
unreadable private ETS tables and tables created by OTP applications are not
displayed. Use menu _View_ to view "system" ETS tables, unreadable ETS tables,
or Mnesia tables.

Double-click to view the table content, or right-click and select option _Show
Table Content_. To view table information, select the table and activate menu
_View > Table information_, or right-click and select option _Table info_.

You can use [regular expressions](`m:re`) and search for objects, and edit or
delete them.

## Trace Overview Tab

Tab _Trace Overview_ handles tracing. Trace by selecting the processes or ports
to be traced and how to trace them. For processes, you can trace messages,
function calls, scheduling, garbage collections, and process-related events such
as `spawn`, `exit`, and many others. For ports, you can trace messages,
scheduling and port-related events.

To trace function calls, you also need to set up _trace patterns_. Trace
patterns select the function calls to be traced. The number of traced function
calls can be further reduced with _match specifications_. Match specifications
can also be used to trigger more information in the trace messages.

You can also set match specifications on messages. By default, if tracing
messages, all messages sent and/or received by the process or port are traced.
Match specifications can be used to reduce the number of traced messages and/or
to trigger more information in the trace messages.

> #### Note {: .info }
>
> Trace patterns only apply to the traced processes and ports.

Processes are added from the _Applications_ or _Processes_ tabs. Ports are added
from the _Ports_ tab. A special _new_ identifier, meaning all processes, or
ports, started after trace start, can be added with buttons _Add 'new'
Processes_ and _Add 'new' Ports_, respectively.

When adding processes or ports, a window with trace options is displayed. The
chosen options are set for the selected processes/ports. To change the options,
right-click the process or port and select _Edit process options_. To remove a
process or port from the list, right-click and select _Remove process_ or
_Remove port_, respectively.

Processes and ports added by process/port identifiers add the nodes these
processes/ports reside on in the node list. More nodes can be added by clicking
button _Add Nodes_, or by right-clicking in the _Nodes_ list and select _Add
Nodes_. To remove nodes, select them, then right-click and choose _Remove
nodes_.

If function calls are traced, trace patterns must be added by clicking button
_Add Trace Pattern_. Select a module, function(s), and a match specification. If
no functions are selected, all functions in the module are traced.

Trace patterns can also be added for traced messages. Click button _Add Trace
Pattern_ and select _Messages sent_ or _Messages received_, and a match
specification.

A few basic match specifications are provided in the tool, and you can provide
your own match specifications. The syntax of match specifications is described
in the [`ERTS User's Guide`](`e:erts:match_spec.md`). To simplify the writing of
a match specification, they can also be written as `fun/1`. For details, see
module `m:ms_transform` in application STDLIB.

Click button _Start Trace_ to start the trace. By default, trace output is
written to a new window. Tracing is stopped when the window is closed, or when
clicking button _Stop Trace_. Trace output can be changed with menu _Options >
Output_. The trace settings, including match specifications, can be saved to, or
loaded from, a file.

For details about tracing, see module `m:dbg` in application Runtime_Tools and
in section "Match specifications in Erlang" in
[`ERTS User's Guide`](`e:erts:match_spec.md`) and in module `m:ms_transform` in
application STDLIB.
