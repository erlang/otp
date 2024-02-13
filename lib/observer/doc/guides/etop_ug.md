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
# Erlang Top

## Introduction

Erlang Top, `etop`, is a tool for presenting information about Erlang processes
similar to the information presented by `top` in UNIX.

## Getting Started

Start Erlang Top in either of the following ways:

- Use script `etop`.
- Use batch file `etop.bat`, for example, `etop -node tiger@durin`.

## Output

The output from Erlang Top is as follows:

```text
========================================================================================
 tiger@durin                                                               13:40:32
 Load:  cpu         0               Memory:  total        1997    binary         33
        procs     197                        processes       0    code          173
        runq      135                        atom         1002    ets            95

Pid            Name or Initial Func    Time    Reds  Memory    MsgQ Current Function
----------------------------------------------------------------------------------------
<127.23.0>     code_server                0   59585   78064       0 gen_server:loop/6
<127.21.0>     file_server_2              0   36380   44276       0 gen_server:loop/6
<127.2.0>      erl_prim_loader            0   27962    3740       0 erl_prim_loader:loop
<127.9.0>      kernel_sup                 0    6998    4676       0 gen_server:loop/6
<127.17.0>     net_kernel                62    6018    3136       0 gen_server:loop/6
<127.0.0>      init                       0    4156    4352       0 init:loop/1
<127.16.0>     auth                       0    1765    1264       0 gen_server:loop/6
<127.18.0>     inet_tcp_dist:accept       0     660    1416       0 prim_inet:accept0/2
<127.5.0>      application_controll       0     569    6756       0 gen_server:loop/6
<127.137.0>    net_kernel:do_spawn_       0     553    5840       0 dbg:do_relay_1/1
========================================================================================
```

The header includes some system information:

- **`Load`** - \* **`cpu`** - `Runtime/Wallclock`, that is, the percentage of
  time where the node has been active.

  - **`procs`** - The number of processes on the node.

  - **`runq`** - The number of processes that are ready to run.

- **`Memory`** - The memory allocated by the node in kilobytes.

For each process the following information is presented:

- **`Time`** - The runtime for the process, that is, the time that the process
  has been scheduled in.

- **`Reds`** - The number of reductions executed on the process.

- **`Memory`** - The size of the process in bytes, obtained by a call to
  [`process_info(Pid,memory)`](`process_info/2`).

- **`MsgQ`** - The length of the message queue for the process.

> #### Note {: .info }
>
> _Time_ and _Reds_ can be presented as accumulated values or as values since
> the last update.

## Configuration

All configuration parameters can be set at start by adding `-OptName Value` to
the command line, for example:

```text
% etop -node tiger@durin -setcookie mycookie -lines 15
```

A list of all valid Erlang Top configuration parameters is available in module
`m:etop`.

The parameters `lines`, `interval`, `accumulate`, and `sort` can be changed
during runtime with function `etop:config/2`.

_Example:_

Change configuration parameter `lines` with text-based presentation. Before the
change, 10 lines are presented as follows:

```text
========================================================================================
 tiger@durin                                                               10:12:39
 Load:  cpu         0               Memory:  total        1858    binary         33
        procs     191                        processes       0    code          173
        runq        2                        atom         1002    ets            95

Pid            Name or Initial Func    Time    Reds  Memory    MsgQ Current Function
----------------------------------------------------------------------------------------
<127.23.0>     code_server                0   60350   71176       0 gen_server:loop/6
<127.21.0>     file_server_2              0   36380   44276       0 gen_server:loop/6
<127.2.0>      erl_prim_loader            0   27962    3740       0 erl_prim_loader:loop
<127.17.0>     net_kernel                 0   13808    3916       0 gen_server:loop/6
<127.9.0>      kernel_sup                 0    6998    4676       0 gen_server:loop/6
<127.0.0>      init                       0    4156    4352       0 init:loop/1
<127.18.0>     inet_tcp_dist:accept       0    2196    1416       0 prim_inet:accept0/2
<127.16.0>     auth                       0    1893    1264       0 gen_server:loop/6
<127.43.0>     ddll_server                0     582    3744       0 gen_server:loop/6
<127.5.0>      application_controll       0     569    6756       0 gen_server:loop/6
========================================================================================
```

Function `etop:config/2` is called to change the number of showed lines to 5:

```text
> etop:config(lines,5).
ok
```

After the change, 5 lines are presented as follows:

```text
(etop@durin)2>
========================================================================================
 tiger@durin                                                               10:12:44
 Load:  cpu         0               Memory:  total        1859    binary         33
        procs     192                        processes       0    code          173
        runq        2                        atom         1002    ets            95

Pid            Name or Initial Func    Time    Reds  Memory    MsgQ Current Function
----------------------------------------------------------------------------------------
<127.17.0>     net_kernel               183      70    4092       0 gen_server:loop/6
<127.335.0>    inet_tcp_dist:do_acc     141      22    1856       0 dist_util:con_loop/9
<127.19.0>     net_kernel:ticker/2      155       6    1244       0 net_kernel:ticker1/2
<127.341.0>    net_kernel:do_spawn_       0       0    5840       0 dbg:do_relay_1/1
<127.43.0>     ddll_server                0       0    3744       0 gen_server:loop/6
========================================================================================
```

## Print to File

At any time, the current Erlang Top display can be dumped to a text file with
function `etop:dump/1`.

## Stop

To stop Erlang Top, use function `etop:stop/0`.
