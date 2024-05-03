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
# LTTng and Erlang/OTP

## Introduction

The Linux Trace Toolkit: next generation is an open source system software
package for correlated tracing of the Linux kernel, user applications and
libraries.

For more information, please visit [http://lttng.org](http://lttng.org)

## Building Erlang/OTP with LTTng support

Configure and build Erlang with LTTng support:

For LTTng to work properly with Erlang/OTP you need the following packages
installed:

- LTTng-tools: a command line interface to control tracing sessions.
- LTTng-UST: user space tracing library.

On Ubuntu this can be installed via `aptitude`:

```text
$ sudo aptitude install lttng-tools liblttng-ust-dev
```

See [Installing LTTng](http://lttng.org/docs/#doc-installing-lttng) for more
information on how to install LTTng on your system.

After LTTng is properly installed on the system Erlang/OTP can be built with
LTTng support.

```text
$ ./configure --with-dynamic-trace=lttng
$ make
```

## Dyntrace Tracepoints

All tracepoints are in the domain of `org_erlang_dyntrace`

All Erlang types are the string equivalent in LTTng.

_process_spawn_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `parent : string` :: Process ID. Ex. `"<0.131.0>"`
- `entry : string` :: Code Location. Ex. `"lists:sort/1"`

Available through `erlang:trace/3` with trace flag `procs` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```erlang
process_spawn: { cpu_id = 3 }, { pid = "<0.131.0>", parent = "<0.130.0>", entry = "erlang:apply/2" }
```

_process_link_

- `to : string` :: Process ID or Port ID. Ex. `"<0.131.0>"`
- `from : string` :: Process ID or Port ID. Ex. `"<0.131.0>"`
- `type : string` :: `"link" | "unlink"`

Available through `erlang:trace/3` with trace flag `procs` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```text
process_link: { cpu_id = 3 }, { from = "<0.130.0>", to = "<0.131.0>", type = "link" }
```

_process_exit_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `reason : string` :: Exit reason. Ex. `"normal"`

Available through `erlang:trace/3` with trace flag `procs` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```text
process_exit: { cpu_id = 3 }, { pid = "<0.130.0>", reason = "normal" }
```

_process_register_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `name : string` :: Registered name. Ex. `"logger"`
- `type : string` :: `"register" | "unregister"`

Example:

```erlang
process_register: { cpu_id = 0 }, { pid = "<0.128.0>", name = "dyntrace_lttng_SUITE" type = "register" }
```

_process_scheduled_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `entry : string` :: Code Location. Ex. `"lists:sort/1"`
- `type : string` ::
  `"in" | "out" | "in_exiting" | "out_exiting" | "out_exited"`

Available through `erlang:trace/3` with trace flag `running` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```erlang
process_scheduled: { cpu_id = 0 }, { pid = "<0.136.0>", entry = "erlang:apply/2", type = "in" }
```

_port_open_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`

Available through `erlang:trace/3` with trace flag `ports` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```text
port_open: { cpu_id = 5 }, { pid = "<0.131.0>", driver = "'/bin/sh -s unix:cmd'", port = "#Port<0.1887>" }
```

_port_exit_

- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `reason : string` :: Exit reason. Ex. `"normal"`

Available through `erlang:trace/3` with trace flag `ports` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```text
port_exit: { cpu_id = 5 }, { port = "#Port<0.1887>", reason = "normal" }
```

_port_link_

- `to : string` :: Process ID. Ex. `"<0.131.0>"`
- `from : string` :: Process ID. Ex. `"<0.131.0>"`
- `type : string` :: `"link" | "unlink"`

Available through `erlang:trace/3` with trace flag `ports` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```erlang
port_link: { cpu_id = 5 }, { from = "#Port<0.1887>", to = "<0.131.0>", type = "unlink" }
```

_port_scheduled_

Available through `erlang:trace/3` with trace flag `running` and
`{tracer,dyntrace,[]}` as tracer module.

- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `entry : string` :: Callback. Ex. `"open"`
- `type : string` ::
  `"in" | "out" | "in_exiting" | "out_exiting" | "out_exited"`

Example:

```erlang
port_scheduled: { cpu_id = 5 }, { pid = "#Port<0.1905>", entry = "close", type = "out" }
```

Available through `erlang:trace/3` with trace flag `running` and
`{tracer,dyntrace,[]}` as tracer module.

_function_call_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `entry : string` :: Code Location. Ex. `"lists:sort/1"`
- `depth : integer` :: Stack depth. Ex. `0`

Available through `erlang:trace/3` with trace flag `call` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```erlang
function_call: { cpu_id = 5 }, { pid = "<0.145.0>", entry = "dyntrace_lttng_SUITE:'-t_call/1-fun-1-'/0", depth = 0 }
```

_function_return_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `entry : string` :: Code Location. Ex. `"lists:sort/1"`
- `depth : integer` :: Stack depth. Ex. `0`

Available through `erlang:trace/3` with trace flag `call` or `return_to` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```erlang
function_return: { cpu_id = 5 }, { pid = "<0.145.0>", entry = "dyntrace_lttng_SUITE:waiter/0", depth = 0 }
```

_function_exception_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `entry : string` :: Code Location. Ex. `"lists:sort/1"`
- `class : string` :: Error reason. Ex. `"error"`

Available through `erlang:trace/3` with trace flag `call` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```erlang
function_exception: { cpu_id = 5 }, { pid = "<0.144.0>", entry = "t:call_exc/1", class = "error" }
```

_message_send_

- `from : string` :: Process ID or Port ID. Ex. `"<0.131.0>"`
- `to : string` :: Process ID or Port ID. Ex. `"<0.131.0>"`
- `message : string` :: Message sent. Ex. `"{<0.162.0>,ok}"`

Available through `erlang:trace/3` with trace flag `send` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```text
message_send: { cpu_id = 3 }, { from = "#Port<0.1938>", to = "<0.160.0>", message = "{#Port<0.1938>,eof}" }
```

_message_receive_

- `to : string` :: Process ID or Port ID. Ex. `"<0.131.0>"`
- `message : string` :: Message received. Ex. `"{<0.162.0>,ok}"`

Available through `erlang:trace/3` with trace flag `'receive'` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```text
message_receive: { cpu_id = 7 }, { to = "<0.167.0>", message = "{<0.165.0>,ok}" }
```

_gc_minor_start_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `need : integer` :: Heap need. Ex. `2`
- `heap : integer` :: Young heap word size. Ex. `233`
- `old_heap : integer` :: Old heap word size. Ex. `233`

Available through `erlang:trace/3` with trace flag `garbage_collection` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```erlang
gc_minor_start: { cpu_id = 0 }, { pid = "<0.172.0>", need = 0, heap = 610, old_heap = 0 }
```

_gc_minor_end_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `reclaimed : integer` :: Heap reclaimed. Ex. `2`
- `heap : integer` :: Young heap word size. Ex. `233`
- `old_heap : integer` :: Old heap word size. Ex. `233`

Available through `erlang:trace/3` with trace flag `garbage_collection` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```c
gc_minor_end: { cpu_id = 0 }, { pid = "<0.172.0>", reclaimed = 120, heap = 1598, old_heap = 1598 }
```

_gc_major_start_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `need : integer` :: Heap need. Ex. `2`
- `heap : integer` :: Young heap word size. Ex. `233`
- `old_heap : integer` :: Old heap word size. Ex. `233`

Available through `erlang:trace/3` with trace flag `garbage_collection` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```erlang
gc_major_start: { cpu_id = 0 }, { pid = "<0.172.0>", need = 8, heap = 2586, old_heap = 1598 }
```

_gc_major_end_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `reclaimed : integer` :: Heap reclaimed. Ex. `2`
- `heap : integer` :: Young heap word size. Ex. `233`
- `old_heap : integer` :: Old heap word size. Ex. `233`

Available through `erlang:trace/3` with trace flag `garbage_collection` and
`{tracer,dyntrace,[]}` as tracer module.

Example:

```text
gc_major_end: { cpu_id = 0 }, { pid = "<0.172.0>", reclaimed = 240, heap = 4185, old_heap = 0 }
```

## BEAM Tracepoints

All tracepoints are in the domain of `org_erlang_otp`

All Erlang types are the string equivalent in LTTng.

_driver_init_

- `driver : string` :: Driver name. Ex. `"tcp_inet"`
- `major : integer` :: Major version. Ex. `3`
- `minor : integer` :: Minor version. Ex. `1`
- `flags : integer` :: Flags. Ex. `1`

Example:

```erlang
driver_init: { cpu_id = 2 }, { driver = "caller_drv", major = 3, minor = 3, flags = 1 }
```

_driver_start_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`

Example:

```erlang
driver_start: { cpu_id = 2 }, { pid = "<0.198.0>", driver = "caller_drv", port = "#Port<0.3676>" }
```

_driver_output_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`
- `bytes : integer` :: Size of data returned. Ex. `82`

Example:

```text
driver_output: { cpu_id = 2 }, { pid = "<0.198.0>", port = "#Port<0.3677>", driver = "/bin/sh -s unix:cmd", bytes = 36 }
```

_driver_outputv_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`
- `bytes : integer` :: Size of data returned. Ex. `82`

Example:

```erlang
driver_outputv: { cpu_id = 5 }, { pid = "<0.194.0>", port = "#Port<0.3663>", driver = "tcp_inet", bytes = 3 }
```

_driver_ready_input_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```text
driver_ready_input: { cpu_id = 5 }, { pid = "<0.189.0>", port = "#Port<0.3637>", driver = "inet_gethost 4 " }
```

_driver_ready_output_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```erlang
driver_ready_output: { cpu_id = 5 }, { pid = "<0.194.0>", port = "#Port<0.3663>", driver = "tcp_inet" }
```

_driver_timeout_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```erlang
driver_timeout: { cpu_id = 5 }, { pid = "<0.196.0>", port = "#Port<0.3664>", driver = "tcp_inet" }
```

_driver_stop_select_

- `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```erlang
driver_stop_select: { cpu_id = 5 }, { driver = "unknown" }
```

_driver_flush_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```erlang
driver_flush: { cpu_id = 7 }, { pid = "<0.204.0>", port = "#Port<0.3686>", driver = "tcp_inet" }
```

_driver_stop_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```erlang
driver_stop: { cpu_id = 5 }, { pid = "[]", port = "#Port<0.3673>", driver = "tcp_inet" }
```

_driver_process_exit_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`

_driver_ready_async_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`

Example:

```erlang
driver_ready_async: { cpu_id = 3 }, { pid = "<0.181.0>", port = "#Port<0.3622>", driver = "tcp_inet" }
```

_driver_call_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`
- `command : integer` :: Command integer. Ex. `1`
- `bytes : integer` :: Size of data returned. Ex. `82`

Example:

```erlang
driver_call: { cpu_id = 2 }, { pid = "<0.202.0>", port = "#Port<0.3676>", driver = "caller_drv", command = 0, bytes = 2 }
```

_driver_control_

- `pid : string` :: Process ID. Ex. `"<0.131.0>"`
- `port : string` :: Port ID. Ex. `"#Port<0.1031>"`
- `driver : string` :: Driver name. Ex. `"tcp_inet"`
- `command : integer` :: Command integer. Ex. `1`
- `bytes : integer` :: Size of data returned. Ex. `82`

Example:

```erlang
driver_control: { cpu_id = 3 }, { pid = "<0.32767.8191>", port = "#Port<0.0>", driver = "forker", command = 83, bytes = 32 }
```

_carrier_create_

- `type : string` :: Carrier type. Ex. `"ets_alloc"`
- `instance : integer` :: Allocator instance. Ex. `1`
- `size : integer` :: Carrier size. Ex. `262144`
- `mbc_carriers : integer` :: Number of multiblock carriers in instance. Ex. `3`
- `mbc_carriers_size : integer` :: Total size of multiblock blocks carriers in
  instance. Ex. `1343488`
- `mbc_blocks : integer` :: Number of multiblock blocks in instance. Ex. `122`
- `mbc_blocks_size : integer` :: Total size of all multiblock blocks in
  instance. Ex. `285296`
- `sbc_carriers : integer` :: Number of singleblock carriers in instance. Ex.
  `1`
- `sbc_carriers_size : integer` :: Total size of singleblock blocks carriers in
  instance. Ex. `1343488`
- `sbc_blocks : integer` :: Number of singleblocks in instance. Ex. `1`
- `sbc_blocks_size : integer` :: Total size of all singleblock blocks in
  instance. Ex. `285296`

Example:

```c
carrier_create: { cpu_id = 2 }, { type = "ets_alloc", instance = 7, size = 2097152, mbc_carriers = 4, mbc_carriers_size = 3440640, mbc_blocks = 526, mbc_blocks_size = 1278576, sbc_carriers = 0, sbc_carriers_size = 0, sbc_blocks = 0, sbc_blocks_size = 0 }
```

_carrier_destroy_

- `type : string` :: Carrier type. Ex. `"ets_alloc"`
- `instance : integer` :: Allocator instance. Ex. `1`
- `size : integer` :: Carrier size. Ex. `262144`
- `mbc_carriers : integer` :: Number of multiblock carriers in instance. Ex. `3`
- `mbc_carriers_size : integer` :: Total size of multiblock blocks carriers in
  instance. Ex. `1343488`
- `mbc_blocks : integer` :: Number of multiblock blocks in instance. Ex. `122`
- `mbc_blocks_size : integer` :: Total size of all multiblock blocks in
  instance. Ex. `285296`
- `sbc_carriers : integer` :: Number of singleblock carriers in instance. Ex.
  `1`
- `sbc_carriers_size : integer` :: Total size of singleblock blocks carriers in
  instance. Ex. `1343488`
- `sbc_blocks : integer` :: Number of singleblocks in instance. Ex. `1`
- `sbc_blocks_size : integer` :: Total size of all singleblock blocks in
  instance. Ex. `285296`

Example:

```c
carrier_destroy: { cpu_id = 6 }, { type = "ets_alloc", instance = 7, size = 262144, mbc_carriers = 3, mbc_carriers_size = 3178496, mbc_blocks = 925, mbc_blocks_size = 2305336, sbc_carriers = 0, sbc_carriers_size = 0, sbc_blocks = 0, sbc_blocks_size = 0 }
```

_carrier_pool_put_

- `type : string` :: Carrier type. Ex. `"ets_alloc"`
- `instance : integer` :: Allocator instance. Ex. `1`
- `size : integer` :: Carrier size. Ex. `262144`

Example:

```c
carrier_pool_put: { cpu_id = 3 }, { type = "ets_alloc", instance = 5, size = 1048576 }
```

_carrier_pool_get_

- `type : string` :: Carrier type. Ex. `"ets_alloc"`
- `instance : integer` :: Allocator instance. Ex. `1`
- `size : integer` :: Carrier size. Ex. `262144`

Example:

```c
carrier_pool_get: { cpu_id = 7 }, { type = "ets_alloc", instance = 4, size = 3208 }
```

## Example of process tracing

An example of process tracing of `os_mon` and friends.

Clean start of lttng in a bash shell.

```text
$ lttng create erlang-demo
Spawning a session daemon
Session erlang-demo created.
Traces will be written in /home/egil/lttng-traces/erlang-demo-20160526-165920
```

Start an Erlang node with lttng enabled.

```text
$ erl
Erlang/OTP 19 [erts-8.0] [source-4d7b24d] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [lttng]

Eshell V8.0  (abort with ^G)
1>
```

Load the `dyntrace` module.

```erlang
1> l(dyntrace).
{module,dyntrace}
```

All tracepoints via dyntrace are now visible and can be listed through
`lttng list -u`.

Enable the process_register LTTng tracepoint for Erlang.

```text
$ lttng enable-event -u org_erlang_dyntrace:process_register
UST event org_erlang_dyntrace:process_register created in channel channel0
```

Enable process tracing for new processes and use `dyntrace` as tracer backend.

```erlang
2> erlang:trace(new,true,[procs,{tracer,dyntrace,[]}]).
0
```

Start LTTng tracing.

```text
$ lttng start
Tracing started for session erlang-demo
```

Start the `os_mon` application in Erlang.

```text
3> application:ensure_all_started(os_mon).
{ok,[sasl,os_mon]}
```

Stop LTTng tracing and view the result.

```text
$ lttng stop
Tracing stopped for session erlang-demo
$ lttng view
[17:20:42.561168759] (+?.?????????) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.66.0>", name = "sasl_sup", type = "register" }
[17:20:42.561215519] (+0.000046760) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.67.0>", name = "sasl_safe_sup", type = "register" }
[17:20:42.562149024] (+0.000933505) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.68.0>", name = "alarm_handler", type = "register" }
[17:20:42.571035803] (+0.008886779) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.69.0>", name = "release_handler", type = "register" }
[17:20:42.574939868] (+0.003904065) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.74.0>", name = "os_mon_sup", type = "register" }
[17:20:42.576818712] (+0.001878844) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.75.0>", name = "disksup", type = "register" }
[17:20:42.580032013] (+0.003213301) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.76.0>", name = "memsup", type = "register" }
[17:20:42.583046339] (+0.003014326) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.78.0>", name = "cpu_sup", type = "register" }
[17:20:42.586206242] (+0.003159903) elxd1168lx9 org_erlang_dyntrace:process_register: \
    { cpu_id = 5 }, { pid = "<0.82.0>", name = "timer_server", type = "register" }
```
