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
# Drivers

This section provides a brief overview on how to write efficient drivers.

It is assumed that you have a good understanding of drivers.

## Drivers and Concurrency

The runtime system always takes a lock before running any code in a driver.

By default, that lock is at the driver level, that is, if several ports have
been opened to the same driver, only code for one port at the same time can be
running.

A driver can be configured to have one lock for each port instead.

If a driver is used in a functional way (that is, holds no state, but only does
some heavy calculation and returns a result), several ports with registered
names can be opened beforehand, and the port to be used can be chosen based on
the scheduler ID as follows:

```erlang
-define(PORT_NAMES(),
	{some_driver_01, some_driver_02, some_driver_03, some_driver_04,
	 some_driver_05, some_driver_06, some_driver_07, some_driver_08,
	 some_driver_09, some_driver_10, some_driver_11, some_driver_12,
	 some_driver_13, some_driver_14, some_driver_15, some_driver_16}).

client_port() ->
    element(erlang:system_info(scheduler_id) rem tuple_size(?PORT_NAMES()) + 1,
	    ?PORT_NAMES()).
```

As long as there are no more than 16 schedulers, there will never be any lock
contention on the port lock for the driver.

## Avoiding Copying Binaries When Calling a Driver

There are basically two ways to avoid copying a binary that is sent to a driver:

- If the `Data` argument for [port_control/3](`erlang:port_control/3`) is a
  binary, the driver will be passed a pointer to the contents of the binary and
  the binary will not be copied. If the `Data` argument is an iolist (list of
  binaries and lists), all binaries in the iolist will be copied.

  Therefore, if you want to send both a pre-existing binary and some extra data
  to a driver without copying the binary, you must call
  [`port_control/3`](`port_control/3`) twice; once with the binary and once with
  the extra data. However, that will only work if there is only one process
  communicating with the port (because otherwise another process can call the
  driver in-between the calls).

- Implement an `outputv` callback (instead of an `output` callback) in the
  driver. If a driver has an `outputv` callback, refc binaries passed in an
  iolist in the `Data` argument for [port_command/2](`erlang:port_command/2`)
  will be passed as references to the driver.

## Returning Small Binaries from a Driver

The runtime system can represent binaries up to 64 bytes as heap binaries. They
are always copied when sent in messages, but they require less memory if they
are not sent to another process and garbage collection is cheaper.

If you know that the binaries you return are always small, you are advised to
use driver API calls that do not require a pre-allocated binary, for example,
[driver_output()](`e:erts:erl_driver.md#driver_output`) or
[erl_drv_output_term()](`e:erts:erl_driver.md#erl_drv_output_term`), using the
`ERL_DRV_BUF2BINARY` format, to allow the runtime to construct a heap binary.

## Returning Large Binaries without Copying from a Driver

To avoid copying data when a large binary is sent or returned from the driver to
an Erlang process, the driver must first allocate the binary and then send it to
an Erlang process in some way.

Use [driver_alloc_binary()](`e:erts:erl_driver.md#driver_alloc_binary`) to
allocate a binary.

There are several ways to send a binary created with `driver_alloc_binary()`:

- From the `control` callback, a binary can be returned if
  [set_port_control_flags()](`e:erts:erl_driver.md#set_port_control_flags`) has
  been called with the flag value `PORT_CONTROL_FLAG_BINARY`.
- A single binary can be sent with
  [driver_output_binary()](`e:erts:erl_driver.md#driver_output_binary`).
- Using [erl_drv_output_term()](`e:erts:erl_driver.md#erl_drv_output_term`) or
  [erl_drv_send_term()](`e:erts:erl_driver.md#erl_drv_send_term`), a binary can
  be included in an Erlang term.
