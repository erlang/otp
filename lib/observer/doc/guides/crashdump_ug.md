<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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
# Crashdump Viewer

## Introduction

The Crashdump Viewer is a WxWidgets based tool for browsing Erlang crashdumps.

## Getting Started

The easiest way to start Crashdump Viewer is to use shell script `cdv` with the
full path to the Erlang crashdump as argument. The script is located in
directory `priv` of the Observer application. This starts the Crashdump Viewer
GUI and loads the specified file. If no filename is specified, a file dialog is
opened where the file can be selected.

Under Windows, the batch file `cdv.bat` can be used.

Crashdump Viewer can also be started from an Erlang node by calling
`crashdump_viewer:start/0` or `crashdump_viewer:start/1`.

## GUI

The GUI main window is opened when Crashdump Viewer has loaded a crashdump. It
contains a title bar, a menu bar, information tabs, and a status bar.

The title bar shows the name of the currently loaded crashdump.

The menu bar contains a _File_ menu and a _Help_ menu. From the _File_ menu, a
new crashdump can be loaded or the tool can be terminated. From the _Help_ menu,
this User's Guide and section "How to interpret the Erlang crash dumps" from the
ERTS application can be opened. "How to interpret the Erlang crash dumps"
describes the raw crashdumps in detail and includes information about each field
in the information pages."How to interpret the Erlang crash dumps" is also
available in the OTP online documentation.

The status bar at the bottom of the window shows a warning if the currently
loaded dump is truncated.

The center area of the main window contains the information tabs. Each tab
displays information about a specific item or a list of items. Select a tab by
clicking the tab title.

From tabs displaying lists of items, for example, the _Processes_ tab or the
_Ports_ tab, a new window with more information can be opened by double-clicking
a row or by right- clicking the row and selecting an item from the drop-down
menu. The new window is called a detail window. Detail windows can be opened for
processes, ports, nodes, and modules.

The information shown in a detail window can contain links to processes or
ports. Clicking one of these links opens the detail window for the process or
port in question. If the process or port resides on a remote node, no
information is available. Clicking the link then displays a dialog where you can
choose to open the detail window for the remote node.

Some tabs contain a left-hand menu where subitems of the information area can be
selected. Click one of the rows, and the information is displayed in the
right-hand information area.

## Tab Content

Each tab in the main window contains an information page. If no information is
found for an item, the page is empty. The reason for not finding information
about an item can be the following:

- It is a dump from an old OTP release in which this item was not written.
- The item was not present in the system at the point of failure.
- The dump is truncated. In this case, a warning is displayed in the status bar
  of the main window.

Even if some information about an item exists, there can be empty fields if the
dump originates from an old OTP release.

The value `-1` in any field means "unknown", and in most cases it means that the
dump was truncated somewhere around this field.

The following sections describe some of the fields in the information tabs.
These are fields that do not exist in the raw crashdump, or in some way differ
from the fields in the raw crashdump. For details about other fields, see the
[ERTS User's Guide](`e:erts:index.html`), section "How to interpret the Erlang
crash dumps". That section can also be opened from the _Help_ menu in the main
window. There are also links from the following sections to related information
in "How to interpret the Erlang crash dumps".

[](){: #general_info }

## General Tab

Tab _General_ shows a short overview of the dump.

The following fields are not described in the ERTS User's Guide:

- **`Crashdump created on`** - Time of failure.

- **`Memory allocated`** - The total number of bytes allocated, equivalent to
  `c:memory(total)`.

- **`Memory maximum`** - The maximum number of bytes that has been allocated
  during the lifetime of the originating node. This is only shown if the Erlang
  runtime system is run instrumented.

- **`Atoms`** - If available in the dump, this is the total number of atoms in
  the atom table. If the size of the atom table is unavailable, the number of
  atoms visible in the dump is displayed.

- **`Processes`** - The number of processes visible in the dump.

- **`ETS tables`** - The number of ETS tables visible in the dump.

- **`Funs`** - The number of funs visible in the dump.

For details, see [General Information](`e:erts:crash_dump.md#general-information`) in
section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: #processes }

## Processes Tab

Tab _Processes_ shows a list of all processes found in the crashdump, including
brief information about each process. By default, the processes are sorted by
their pids. To sort by another topic, click the desired column heading.

Column _Memory_ shows the 'Memory' field that was added to crashdumps in
Erlang/OTP R16B01. This is the total amount of memory used by the process. For
crashdumps from earlier releases, this column shows the 'Stack+heap' field. The
value is always in bytes.

To view detailed information about a specific process, double- click the row in
the list, or right-click the row and select _Properties for <pid>_.

For details, see [Process Information](`e:erts:crash_dump.md#process-data`) in
section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: #ports }

## Ports Tab

Tab _Ports_ is similar to the _Processes_ tab, except it lists all ports found
in the crashdump.

To view more details about a specific port, double-click the row or right-click
it and select _Properties for <port>_. From the right-click menu, you can also
select _Properties for <pid>_, where `<pid>` is the process connected to the
port.

For details, see [Port Information](`e:erts:crash_dump.md#port-information`) in section
"How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: #ets_tables } [](){: #internal_ets_tables }

## ETS Tables Tab

Tab _ETS Tables_ shows all ETS table information found in the dump. _Id_ is the
same as the 'Table' field in the raw crashdump. _Memory_ is the 'Words' field
from the raw crashdump translated into bytes. For tree tables, there is no value
in the 'Objects' field.

To open the detailed information page about the table, double- click, or
right-click the row and select _Properties for 'Identifier'_.

To open the detailed information page about the owner process of an ETS table,
right-click the row and select _Properties for <pid>_.

For details, see [ETS Tables](`e:erts:crash_dump.md#ets-tables`) in section "How
to Interpret the Erlang Crash Dumps" in ERTS.

[](){: #timers }

## Timers Tab

Tab _Timers_ shows all timer information found in the dump.

To open the detailed information page about the owner process of a timer,
right-click the row and select _Properties for <pid>_.

Double-clicking a row in the _Timers_ tab has no effect.

For details, see [Timers](`e:erts:crash_dump.md#timers`) in section "How to
Interpret the Erlang Crash Dumps" in ERTS.

[](){: #schedulers }

## Schedulers Tab

Tab _Schedulers_ shows all scheduler information found in the dump.

To open the detailed information page about the scheduler, double-click, or
right-click the row and select _Properties for 'Identifier'_.

For details, see [Scheduler Information](`e:erts:crash_dump.md#scheduler-information`) in
section "How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: #funs }

## Funs Tab

Tab _Funs_ shows all fun information found in the dump.

To open the detailed information page about the module to which the fun belongs,
right-click the row and select _Properties for <mod>_.

Double-clicking a row in the _Funs_ tab has no effect.

For details, see [Fun Information](`e:erts:crash_dump.md#fun-information`) in section "How
to Interpret the Erlang Crash Dumps" in ERTS.

[](){: #atoms }

## Atoms Tab

Tab _Atoms_ lists all atoms found in the dump. By default the atoms are sorted
in creation order from first to last. This is opposite of the raw crashdump
where atoms are listed from last to first, meaning that if the dump was
truncated in the middle of the atom list, only the last created atoms are
visible in the _Atoms_ tab.

For details, see [Atoms](`e:erts:crash_dump.md#atoms`) in section "How to
Interpret the Erlang Crash Dumps" in ERTS.

[](){: #distribution_info }

## Nodes Tab

Tab _Nodes_ shows a list of all external Erlang nodes that are referenced from
the crashdump.

If the page is empty, it means either of the following:

- The crashed node is not distributed.
- The crashed node is distributed but has no references to other nodes.
- The dump is truncated.

If the node is distributed, all referenced nodes are visible. Column _Connection
type_ shows if the node is visible, hidden, or not connected. Visible nodes are
alive nodes with a living connection to the originating node. Hidden nodes are
the same as visible nodes, except they are started with flag `-hidden`. Not
connected nodes are nodes that are not connected to the originating node
anymore, but references (that is, process or port identifiers) exist.

To see more detailed information about a node, double-click the row, or
right-click the row and select _Properties for node <node>_. From the
right-click menu, you can also select _Properties for <port>_, to open the
detailed information window for the controlling port.

In the detailed information window for a node, any existing links and monitors
between processes on the originating node and the connected node are displayed.
_Extra Info_ can contain debug information (that is, special information written
if the emulator is debug-compiled) or error information.

For details, see
[Distribution Information](`e:erts:crash_dump.md#distribution-information`) in section
"How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: #loaded_modules }

## Modules Tab

Tab _Modules_ lists all modules loaded on the originating node, and the current
code size. If old code exists, the old size is also shown.

To view detailed information about a specific module, double- click the row, or
right-click it and select _Properties for <mod>_.

For details, see
[Loaded Module Information](`e:erts:crash_dump.md#loaded-module-information`) in section
"How to Interpret the Erlang Crash Dumps" in ERTS.

[](){: #memory }

## Memory Tab

Tab _Memory_ shows memory and allocator information. From the left-hand menu you
can select the following:

- **_Memory_** - See [Memory Information](`e:erts:crash_dump.md#memory-information`) in
  section "How to Interpret the Erlang Crash Dumps" in ERTS.

- **_Allocator Summary_** - This page presents a summary of values from all
  allocators underneath it.

- **_<Allocator>_** - One entry per allocator. See
  [Allocator](`e:erts:crash_dump.md#allocator`) in section "How to Interpret the
  Erlang Crash Dumps" in ERTS.

- **_Allocated Areas_** - See
  [Allocated Areas](`e:erts:crash_dump.md#allocated-areas`) in section "How to
  Interpret the Erlang Crash Dumps" in ERTS.

[](){: #internal_tables }

## Internal Tables Tab

On tab _Internal Tables_ you can from the left-hand menu select _Hash Tables_,
_Index Tables_, or _Internal ETS Tables_.

For details, see
[Internal Table Information](`e:erts:crash_dump.md#internal-table-information`) in section
"How to Interpret the Erlang Crash Dumps" in ERTS.
