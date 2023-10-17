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
# Advanced

## Memory

A good start when programming efficiently is to know how much memory different
data types and operations require. It is implementation-dependent how much
memory the Erlang data types and other items consume, but the following table
shows some figures for the `erts-8.0` system in OTP 19.0.

The unit of measurement is memory words. There exists both a 32-bit and a 64-bit
implementation. A word is therefore 4 bytes or 8 bytes, respectively. The value
for a running system can be determined by calling
[`erlang:system_info(wordsize)`](`m:erlang#system_info_wordsize`).

| _Data Type_                                | _Memory Size_                                                                                                                                                                                                                                                                                                                                                                                                          |
| ------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Small integer                              | 1 word. On 32-bit architectures: -134217729 < i < 134217728 (28 bits). On 64-bit architectures: -576460752303423489 < i < 576460752303423488 (60 bits).                                                                                                                                                                                                                                                                |
| Large integer                              | 3\..N words.                                                                                                                                                                                                                                                                                                                                                                                                           |
| Atom                                       | 1 word. An atom refers into an atom table, which also consumes memory. The atom text is stored once for each unique atom in this table. The atom table is _not_ garbage-collected.                                                                                                                                                                                                                                     |
| Float                                      | On 32-bit architectures: 4 words. On 64-bit architectures: 3 words.                                                                                                                                                                                                                                                                                                                                                    |
| Binary                                     | 3\..6 words + data (can be shared).                                                                                                                                                                                                                                                                                                                                                                                    |
| List                                       | 1 word + 1 word per element + the size of each element.                                                                                                                                                                                                                                                                                                                                                                |
| String (is the same as a list of integers) | 1 word + 2 words per character.                                                                                                                                                                                                                                                                                                                                                                                        |
| Tuple                                      | 2 words + the size of each element.                                                                                                                                                                                                                                                                                                                                                                                    |
| Small Map                                  | 5 words + the size of all keys and values.                                                                                                                                                                                                                                                                                                                                                                             |
| Large Map (> 32 keys)                      | `N` x `F` words + the size of all keys and values. `N` is the number of keys in the Map. `F` is a sparsity factor that can vary between 1.6 and 1.8 due to the probabilistic nature of the internal HAMT data structure.                                                                                                                                                                                               |
| Pid                                        | 1 word for a process identifier from the current local node. On 32-bit: 6 words for a process identifier from another node. On 64-bit: 5 words for a process identifier from another node. A process identifier refers into a process table and a node table, which also consumes memory.                                                                                                                              |
| Port                                       | 1 word for a port identifier from the current local node. 5 words for a port identifier from another node. A port identifier refers into a port table and a node table, which also consumes memory.                                                                                                                                                                                                                    |
| Reference                                  | On 32-bit architectures: 4-7 words for a reference from the current local node, and 7-9 words for a reference from another node. On 64-bit architectures: 4-6 words for a reference from the current local node, and 6-7 words for a reference from another node. A reference also refers into more or less emulator internal data structures which also consumes memory. At a minimum it refers into the node tables. |
| Fun                                        | 9\..13 words + the size of environment. A fun refers into a fun table, which also consumes memory.                                                                                                                                                                                                                                                                                                                     |
| Ets table                                  | Initially 768 words + the size of each element (6 words + the size of Erlang data). The table grows when necessary.                                                                                                                                                                                                                                                                                                    |
| Erlang process                             | 338 words when spawned, including a heap of 233 words.                                                                                                                                                                                                                                                                                                                                                                 |

_Table: Memory Size of Different Data Types_

## System Limits

The Erlang language specification puts no limits on the number of processes,
length of atoms, and so on. However, for performance and memory saving reasons,
there will always be limits in a practical implementation of the Erlang language
and execution environment.

| What                                                                                | Description                                                                                                                                                                                                                                                                         |
|-------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Processes                                                                           | The maximum number of simultaneously alive Erlang processes is by default 1,048,576. This limit can be configured at startup. For more information, see the [`+P`](`e:erts:erl_cmd.md#max_processes`) command-line flag in the [`erl(1)`](`e:erts:erl_cmd.md`) manual page in ERTS. |
| [](){: #unique_pids } Unique Local Process Identifiers on a Runtime System Instance | On a 64 bit system at most `2⁶⁰ - 1` unique process identifiers can be created and on a 32 bit system at most `2²⁸ - 1`.|
| Known nodes | A remote node Y must be known to node X if there exists any pids, ports, references, or funs (Erlang data types) from Y on X, or if X and Y are connected. The maximum number of remote nodes simultaneously/ever known to a node is limited by the [maximum number of atoms](advanced.md#atoms) available for node names. All data concerning remote nodes, except for the node name atom, are garbage-collected. |
| Connected nodes | The maximum number of simultaneously connected nodes is limited by either the maximum number of simultaneously known remote nodes, [the maximum number of (Erlang) ports](advanced.md#ports) available, or [the maximum number of sockets](advanced.md#files_sockets) available. |
| Characters in an atom | 255. |
| [](){: #atoms } Atoms | By default, the maximum number of atoms is 1,048,576. This limit can be raised or lowered using the `+t` option. |
| Elements in a tuple | The maximum number of elements in a tuple is 16,777,215 (24-bit unsigned integer). |
| Size of binary | In the 32-bitimplementation of Erlang, 536,870,911 bytes is the largest binary that can be constructed or matched using the bit syntax. In the 64-bit implementation, the maximum size is 2,305,843,009,213,693,951 bytes. If the limit is exceeded, bit syntax construction fails with a `system_limit` exception, while any attempt to match a binary that is too large fails. This limit is enforced starting in R11B-4. In earlier Erlang/OTP releases, operations on too large binaries in general either fail or give incorrect results. In future releases, other operations that create binaries (such as [`list_to_binary/1`](`list_to_binary/1`)) will probably also enforce the same limit. |
| Total amount of data allocated by an Erlang node | The Erlang runtime system can use the complete 32-bit (or 64-bit) address space, but the operating system often limits a single process to use less than that. |
| Length of a node name | An Erlang node name has the form host@shortname or host@longname. The node name is used as an atom within the system, so the maximum size of 255 holds also for the node name. |
| [](){: #ports } Open ports | The maximum number of simultaneously open Erlang ports is often by default 16,384. This limit can be configured at startup. For more information, see the [`+Q`](`e:erts:erl_cmd.md#max_ports`) command-line flag in the [`erl(1)`](`e:erts:erl_cmd.md`) manual page in ERTS. |
| [](){: #unique_ports } Unique Local Port Identifiers on a Runtime System Instance | On a 64 bit system at most `2⁶⁰ - 1` unique port identifiers can be created and on a 32 bit system at most `2²⁸ - 1`. |
| [](){: #files_sockets } Open files and sockets | The maximum number of simultaneously open files and sockets depends on [the maximum number of Erlang ports](advanced.md#ports) available, as well as on operating system-specific settings and limits. |
| Number of arguments to a function or fun | 255 |
| [](){: #unique_references } Unique References on a Runtime System Instance | Each scheduler thread has its own set of references, and all other threads have a shared set of references. Each set of references consist of `2⁶⁴ - 1` unique references. That is, the total amount of unique references that can be produced on a runtime system instance is `(NoSchedulers + 1) × (2⁶⁴ - 1)`. If a scheduler thread create a new reference each nano second, references will at earliest be reused after more than 584 years. That is, for the foreseeable future they are unique enough. |
| [](){: #unique_integers } Unique Integers on a Runtime System Instance | There are two types of unique integers both created using the [erlang:unique_integer()](`erlang:unique_integer/1`) BIF: _1._ Unique integers created _with_ the `monotonic` modifier consist of a set of `2⁶⁴ - 1` unique integers. _2._ Unique integers created _without_ the `monotonic` modifier consist of a set of `2⁶⁴ - 1` unique integers per scheduler thread and a set of `2⁶⁴ - 1` unique integers shared by other threads. That is, the total amount of unique integers without the `monotonic` modifier is `(NoSchedulers + 1) × (2⁶⁴ - 1)`. If a unique integer is created each nano second, unique integers will at earliest be reused after more than 584 years. That is, for the foreseeable future they are unique enough. |

_Table: System Limits_
