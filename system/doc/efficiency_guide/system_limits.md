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
# System Limits

The Erlang language specification puts no limits on the number of processes,
length of atoms, and so on. However, for performance and memory saving reasons,
there will always be limits in a practical implementation of the Erlang language
and execution environment.

- **Processes** - The maximum number of simultaneously alive Erlang processes
is by default 1,048,576. This limit can be configured at startup. For more information,
see the [`+P`](`e:erts:erl_cmd.md#max_processes`) command-line flag
in the [`erl(1)`](`e:erts:erl_cmd.md`) manual page in ERTS.

- [](){: #unique_pids } **Unique Local Process Identifiers on a
Runtime System Instance ** - On a 64 bit system at most `2⁶⁰ - 1`
unique process identifiers can be created, and on a 32 bit system at most `2²⁸ - 1`.

- **Known nodes** - A remote node Y must be known to node X if there exists
any pids, ports, references, or funs (Erlang data types) from Y on X, or if
X and Y are connected. The maximum number of remote nodes simultaneously/ever known
to a node is limited by the [maximum number of atoms](#atoms) available
for node names. All data concerning remote nodes, except for the node name atom,
are garbage-collected.

- **Connected nodes** - The maximum number of simultaneously connected nodes is
limited by either the maximum number of simultaneously known remote nodes,
[the maximum number of (Erlang) ports](#ports) available,
or [the maximum number of sockets](#files_sockets) available.

- **Characters in an atom** - 255.

- [](){: #atoms } **Atoms** - By default, the maximum number of atoms is 1,048,576.
This limit can be raised or lowered using the `+t` option.

- **Elements in a tuple** - The maximum number of elements in a
tuple is 16,777,215 (24-bit unsigned integer).

- **Size of binary** - In the 32-bit run-time system for Erlang, 536,870,911 bytes
is the largest binary that can be constructed or matched using the bit syntax.
In the 64-bit run-time system, the maximum size is 2,305,843,009,213,693,951 bytes.
If the limit is exceeded, bit syntax construction fails with a `system_limit`
exception, while any attempt to match a binary that is too large
fails. From Erlang/OTP 27, all other operations that create binaries (such as
[`list_to_binary/1`](`list_to_binary/1`)) also enforces the same limit.

- **Total amount of data allocated by an Erlang node** - The Erlang runtime system
can use the complete 32-bit (or 64-bit) address space, but the operating system
often limits a single process to use less than that.

- **Length of a node name** - An Erlang node name has the form `host@shortname`
or `host@longname`. The node name is used as an atom within the system, so the
maximum size of 255 holds also for the node name.

- [](){: #ports } **Open ports** - The maximum number of simultaneously open
Erlang ports is often by default 16,384. This limit can be configured at startup.
For more information, see the [`+Q`](`e:erts:erl_cmd.md#max_ports`) command-line
flag in the [`erl(1)`](`e:erts:erl_cmd.md`) manual page in ERTS.

- [](){: #unique_ports } **Unique Local Port Identifiers on a Runtime System Instance** -
On a 64 bit system at most `2⁶⁰ - 1` unique port identifiers can be created and
on a 32 bit system at most `2²⁸ - 1`.

- [](){: #files_sockets } **Open files and sockets** - The maximum number of simultaneously
open files and sockets depends on [the maximum number of Erlang ports](#ports)
available, as well as on operating system-specific settings and limits.

- **Number of arguments to a function or fun** - 255.

- [](){: #unique_references } **Unique References on a Runtime System Instance** -
Each scheduler thread has its own set of references, and all other threads have
a shared set of references. Each set of references consist of `2⁶⁴ - 1`unique
references. That is, the total amount of unique references that can be produced
on a runtime system instance is `(NumSchedulers + 1) × (2⁶⁴ - 1)`. If a scheduler
thread create a new reference each nano second, references will at earliest be
reused after more than 584 years. That is, for the foreseeable future they are
sufficiently unique.

- [](){: #unique_integers } **Unique Integers on a Runtime System Instance** -
  There are two types of unique integers created by the
  [erlang:unique_integer/1](`erlang:unique_integer/1`) BIF:
  - Unique integers created **with** the `monotonic` modifier consist of
    a set of `2⁶⁴ - 1` unique integers.
  - Unique integers created **without** the `monotonic` modifier consist
    of a set of `2⁶⁴ - 1` unique integers per scheduler thread and a
    set of `2⁶⁴ - 1` unique integers shared by other threads. That is,
    the total amount of unique integers without the `monotonic`
    modifier is `(NumSchedulers + 1) × (2⁶⁴ - 1)`.

  If a unique integer  is created each nano second, unique integers will be
  reused at earliest after more than 584 years. That is, for the foreseeable future
  they are sufficiently unique.
