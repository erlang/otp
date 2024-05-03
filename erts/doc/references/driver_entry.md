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
# driver_entry

The driver-entry structure used by Erlang drivers.

## Description

> #### Warning {: .warning #WARNING }
>
> _Use this functionality with extreme care._
>
> A driver callback is executed as a direct extension of the native code of the
> VM. Execution is not made in a safe environment. The VM _cannot_ provide the
> same services as provided when executing Erlang code, such as pre-emptive
> scheduling or memory protection. If the driver callback function does not
> behave well, the whole VM will misbehave.
>
> - A driver callback that crash will crash the whole VM.
> - An erroneously implemented driver callback can cause a VM internal state
>   inconsistency, which can cause a crash of the VM, or miscellaneous
>   misbehaviors of the VM at any point after the call to the driver callback.
> - A driver callback doing [lengthy work](erl_driver.md#lengthy_work) before
>   returning degrades responsiveness of the VM, and can cause miscellaneous
>   strange behaviors. Such strange behaviors include, but are not limited to,
>   extreme memory usage, and bad load balancing between schedulers. Strange
>   behaviors that can occur because of lengthy work can also vary between
>   Erlang/OTP releases.

As from ERTS 5.9 (Erlang/OTP R15B) the driver interface has been changed with
larger types for the callbacks [`output`](driver_entry.md#output),
[`control`](driver_entry.md#control), and [`call`](driver_entry.md#call). See
driver [version management](erl_driver.md#version_management) in
[`erl_driver`](erl_driver.md).

> #### Note {: .info }
>
> Old drivers (compiled with an `erl_driver.h` from an ERTS version earlier than
> 5.9) must be updated and have to use the extended interface (with
> [version management ](erl_driver.md#version_management)).

The `driver_entry` structure is a C struct that all Erlang drivers define. It
contains entry points for the Erlang driver, which are called by the Erlang
emulator when Erlang code accesses the driver.

[](){: #emulator } The [`erl_driver`](erl_driver.md) driver API functions need a
port handle that identifies the driver instance (and the port in the emulator).
This is only passed to the `start` function, but not to the other functions. The
`start` function returns a driver-defined handle that is passed to the other
functions. A common practice is to have the `start` function allocate some
application-defined structure and stash the `port` handle in it, to use it later
with the driver API functions.

The driver callback functions are called synchronously from the Erlang emulator.
If they take too long before completing, they can cause time-outs in the
emulator. Use the queue or asynchronous calls if necessary, as the emulator must
be responsive.

The driver structure contains the driver name and some 15 function pointers,
which are called at different times by the emulator.

The only exported function from the driver is `driver_init`. This function
returns the `driver_entry` structure that points to the other functions in the
driver. The `driver_init` function is declared with a macro,
`DRIVER_INIT(drivername)`. (This is because different operating systems have
different names for it.)
{: #DRIVER_INIT }

When writing a driver in C++, the driver entry is to be of `"C"` linkage. One
way to do this is to put the following line somewhere before the driver entry:

```text
extern "C" DRIVER_INIT(drivername);
```

When the driver has passed the `driver_entry` over to the emulator, the driver
is _not_ allowed to modify the `driver_entry`.

If compiling a driver for static inclusion through `--enable-static-drivers`,
you must define `STATIC_ERLANG_DRIVER` before the `DRIVER_INIT` declaration.

> #### Note {: .info }
>
> Do _not_ declare the `driver_entry` `const`. This because the emulator must
> modify the `handle` and the `handle2` fields. A statically allocated, and
> `const`\-declared `driver_entry` can be located in read-only memory, which
> causes the emulator to crash.

## Data Types

`ErlDrvEntry`

```c
typedef struct erl_drv_entry {
    int (*init)(void);          /* Called at system startup for statically
                                   linked drivers, and after loading for
                                   dynamically loaded drivers */
#ifndef ERL_SYS_DRV
    ErlDrvData (*start)(ErlDrvPort port, char *command);
                                /* Called when open_port/2 is invoked,
                                   return value -1 means failure */
#else
    ErlDrvData (*start)(ErlDrvPort port, char *command, SysDriverOpts* opts);
                                /* Special options, only for system driver */
#endif
    void (*stop)(ErlDrvData drv_data);
                                /* Called when port is closed, and when the
                                   emulator is halted */
    void (*output)(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
                                /* Called when we have output from Erlang to
                                   the port */
    void (*ready_input)(ErlDrvData drv_data, ErlDrvEvent event);
                                /* Called when we have input from one of
                                   the driver's handles */
    void (*ready_output)(ErlDrvData drv_data, ErlDrvEvent event);
                                /* Called when output is possible to one of
                                   the driver's handles */
    char *driver_name;          /* Name supplied as command in
                                   erlang:open_port/2 */
    void (*finish)(void);       /* Called before unloading the driver -
                                   dynamic drivers only */
    void *handle;               /* Reserved, used by emulator internally */
    ErlDrvSSizeT (*control)(ErlDrvData drv_data, unsigned int command,
                            char *buf, ErlDrvSizeT len,
			    char **rbuf, ErlDrvSizeT rlen);
                                /* "ioctl" for drivers - invoked by
                                   port_control/3 */
    void (*timeout)(ErlDrvData drv_data);
                                /* Handling of time-out in driver */
    void (*outputv)(ErlDrvData drv_data, ErlIOVec *ev);
                                /* Called when we have output from Erlang
                                   to the port */
    void (*ready_async)(ErlDrvData drv_data, ErlDrvThreadData thread_data);
    void (*flush)(ErlDrvData drv_data);
                                /* Called when the port is about to be
                                   closed, and there is data in the
                                   driver queue that must be flushed
                                   before 'stop' can be called */
    ErlDrvSSizeT (*call)(ErlDrvData drv_data, unsigned int command,
                         char *buf, ErlDrvSizeT len,
			 char **rbuf, ErlDrvSizeT rlen, unsigned int *flags);
                                /* Works mostly like 'control', a synchronous
                                   call into the driver */
    void* unused_event_callback;
    int extended_marker;        /* ERL_DRV_EXTENDED_MARKER */
    int major_version;          /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    int minor_version;          /* ERL_DRV_EXTENDED_MINOR_VERSION */
    int driver_flags;           /* ERL_DRV_FLAGs */
    void *handle2;              /* Reserved, used by emulator internally */
    void (*process_exit)(ErlDrvData drv_data, ErlDrvMonitor *monitor);
                                /* Called when a process monitor fires */
    void (*stop_select)(ErlDrvEvent event, void* reserved);
                                /* Called to close an event object */
 } ErlDrvEntry;
```

- **`int (*init)(void)`{: #init }** - Called directly after the driver has been
  loaded by `erl_ddll:load_driver/2` (actually when the driver is added to the
  driver list). The driver is to return `0`, or, if the driver cannot
  initialize, `-1`.

- **`ErlDrvData (*start)(ErlDrvPort port, char* command)`{: #start }** - Called
  when the driver is instantiated, when `erlang:open_port/2` is called. The
  driver is to return a number >= 0 or a pointer, or, if the driver cannot be
  started, one of three error codes:

  - **`ERL_DRV_ERROR_GENERAL`** - General error, no error code

  - **`ERL_DRV_ERROR_ERRNO`** - Error with error code in `errno`

  - **`ERL_DRV_ERROR_BADARG`** - Error, `badarg`

  If an error code is returned, the port is not started.

- **`void (*stop)(ErlDrvData drv_data)`{: #stop }** - Called when the port is
  closed, with `erlang:port_close/1` or `Port ! {self(), close}`. Notice that
  terminating the port owner process also closes the port. If `drv_data` is a
  pointer to memory allocated in `start`, then `stop` is the place to deallocate
  that memory.

- **`void (*output)(ErlDrvData drv_data, char *buf, ErlDrvSizeT len)`{: #output }** -
  Called when an Erlang process has sent data to the port. The data is
  pointed to by `buf`, and is `len` bytes. Data is sent to the port with
  `Port ! {self(), {command, Data}}` or with `erlang:port_command/2`. Depending
  on how the port was opened, it is to be either a list of integers `0...255` or
  a binary. See `erlang:open_port/2` and `erlang:port_command/2`.

- **`void (*ready_input)(ErlDrvData drv_data, ErlDrvEvent event)`{: #ready_input }**

- **`void (*ready_output)(ErlDrvData drv_data, ErlDrvEvent event)`{: #ready_output }** -
  Called when a driver event (specified in parameter `event`) is signaled. This is used
  to help asynchronous drivers "wake up" when something occurs.

  On Unix the `event` is a pipe or socket handle (or something that the `select`
  system call understands).

  On Windows the `event` is an `Event` or `Semaphore` (or something that the
  `WaitForMultipleObjects` API function understands). (Some trickery in the
  emulator allows more than the built-in limit of 64 `Events` to be used.)

  To use this with threads and asynchronous routines, create a pipe on Unix and
  an `Event` on Windows. When the routine completes, write to the pipe (use
  `SetEvent` on Windows), this makes the emulator call `ready_input` or
  `ready_output`.

  False events can occur. That is, calls to `ready_input` or `ready_output`
  although no real events are signaled. In reality, it is rare (and
  OS-dependant), but a robust driver must nevertheless be able to handle such
  cases.

- **`char *driver_name`{: #driver_name }** - The driver name. It must correspond
  to the atom used in `erlang:open_port/2`, and the name of the driver library
  file (without the extension).

- **`void (*finish)(void)`{: #finish }** - Called by the `erl_ddll` driver when
  the driver is unloaded. (It is only called in dynamic drivers.)

  The driver is only unloaded as a result of calling `erl_ddll:unload_driver/1`,
  or when the emulator halts.

- **`void *handle`** - This field is reserved for the emulator's internal use.
  The emulator will modify this field, so it is important that the
  `driver_entry` is not declared `const`.

- __`ErlDrvSSizeT (*control)(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)`__{: #control } -
  A special routine invoked with `erlang:port_control/3`. It
  works a little like an "ioctl" for Erlang drivers. The data specified to
  [`port_control/3`](`port_control/3`) arrives in `buf` and `len`. The driver
  can send data back, using `*rbuf` and `rlen`.

  This is the fastest way of calling a driver and get a response. It makes no
  context switch in the Erlang emulator and requires no message passing. It is
  suitable for calling C function to get faster execution, when Erlang is too
  slow.

  If the driver wants to return data, it is to return it in `rbuf`. When
  `control` is called, `*rbuf` points to a default buffer of `rlen` bytes, which
  can be used to return data. Data is returned differently depending on the port
  control flags (those that are set with
  [`erl_driver:set_port_control_flags`](erl_driver.md#set_port_control_flags)).

  If the flag is set to `PORT_CONTROL_FLAG_BINARY`, a binary is returned. Small
  binaries can be returned by writing the raw data into the default buffer. A
  binary can also be returned by setting `*rbuf` to point to a binary allocated
  with [`erl_driver:driver_alloc_binary`](erl_driver.md#driver_alloc_binary).
  This binary is freed automatically after `control` has returned. The driver
  can retain the binary for _read only_ access with
  [`erl_driver:driver_binary_inc_refc`](erl_driver.md#driver_binary_inc_refc) to
  be freed later with
  [`erl_driver:driver_free_binary`](erl_driver.md#driver_free_binary). It is
  never allowed to change the binary after `control` has returned. If `*rbuf` is
  set to `NULL`, an empty list is returned.

  If the flag is set to `0`, data is returned as a list of integers. Either use
  the default buffer or set `*rbuf` to point to a larger buffer allocated with
  [`erl_driver:driver_alloc`](erl_driver.md#driver_alloc). The buffer is freed
  automatically after `control` has returned.

  Using binaries is faster if more than a few bytes are returned.

  The return value is the number of bytes returned in `*rbuf`.

- **`void (*timeout)(ErlDrvData drv_data)`{: #timeout }** - Called any time
  after the driver's timer reaches `0`. The timer is activated with
  [`erl_driver:driver_set_timer`](erl_driver.md#driver_set_timer). No priorities
  or ordering exist among drivers, so if several drivers time out at the same
  time, anyone of them is called first.

- **`void (*outputv)(ErlDrvData drv_data, ErlIOVec *ev)`{: #outputv }** - Called
  whenever the port is written to. If it is `NULL`, the `output` function is
  called instead. This function is faster than `output`, as it takes an
  `ErlIOVec` directly, which requires no copying of the data. The port is to be
  in binary mode, see `erlang:open_port/2`.

  `ErlIOVec` contains both a `SysIOVec`, suitable for `writev`, and one or more
  binaries. If these binaries are to be retained when the driver returns from
  `outputv`, they can be queued (using, for example,
  [`erl_driver:driver_enq_bin`](erl_driver.md#driver_enq_bin)) or, if they are
  kept in a static or global variable, the reference counter can be incremented.

- **`void (*ready_async)(ErlDrvData drv_data, ErlDrvThreadData thread_data)`{: #ready_async }** -
  Called after an asynchronous call has completed. The asynchronous call is started with
  [`erl_driver:driver_async`](erl_driver.md#driver_async). This function is
  called from the Erlang emulator thread, as opposed to the asynchronous
  function, which is called in some thread (if multi-threading is enabled).

- **`void (*flush)(ErlDrvData drv_data)`** - Called when the port is about to be
  closed, and there is data in the driver queue that must be flushed before
  'stop' can be called.

- __`ErlDrvSSizeT (*call)(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen, unsigned int *flags)`__{: #call } -
  Called from `erlang:port_call/3`. It works a lot like the
  `control` callback, but uses the external term format for input and output.

  `command` is an integer, obtained from the call from Erlang (the second
  argument to `erlang:port_call/3`).

  `buf` and `len` provide the arguments to the call (the third argument to
  `erlang:port_call/3`). They can be decoded using `ei` functions.

  `rbuf` points to a return buffer, `rlen` bytes long. The return data is to be
  a valid Erlang term in the external (binary) format. This is converted to an
  Erlang term and returned by `erlang:port_call/3` to the caller. If more space
  than `rlen` bytes is needed to return data, `*rbuf` can be set to memory
  allocated with [`erl_driver:driver_alloc`](erl_driver.md#driver_alloc). This
  memory is freed automatically after `call` has returned.

  The return value is the number of bytes returned in `*rbuf`. If
  `ERL_DRV_ERROR_GENERAL` is returned (or in fact, anything < 0),
  `erlang:port_call/3` throws a `BAD_ARG`.

- **`void (*event)(ErlDrvData drv_data, ErlDrvEvent event, ErlDrvEventData event_data)`** -
  Intentionally left undocumented.

- **`int extended_marker`{: #extended_marker }** - This field is either to be
  equal to `ERL_DRV_EXTENDED_MARKER` or `0`. An old driver (not aware of the
  extended driver interface) is to set this field to `0`. If this field is `0`,
  all the following fields _must_ also be `0`, or `NULL` if it is a pointer
  field.

- **`int major_version`** - This field is to equal
  `ERL_DRV_EXTENDED_MAJOR_VERSION` if field `extended_marker` equals
  `ERL_DRV_EXTENDED_MARKER`.

- **`int minor_version`** - This field is to equal
  `ERL_DRV_EXTENDED_MINOR_VERSION` if field `extended_marker` equals
  `ERL_DRV_EXTENDED_MARKER`.

- **`int driver_flags`{: #driver_flags }** - This field is used to pass driver
  capability and other information to the runtime system. If field
  `extended_marker` equals `ERL_DRV_EXTENDED_MARKER`, it is to contain `0` or
  driver flags (`ERL_DRV_FLAG_*`) OR'ed bitwise. The following driver flags
  exist:

  - **`ERL_DRV_FLAG_USE_PORT_LOCKING`** - The runtime system uses port-level
    locking on all ports executing this driver instead of driver-level locking.
    For more information, see [`erl_driver`](erl_driver.md#smp_support).

  - **`ERL_DRV_FLAG_SOFT_BUSY`** - Marks that driver instances can handle being
    called in the [`output`](driver_entry.md#output) and/or
    [`outputv`](driver_entry.md#outputv) callbacks although a driver instance
    has marked itself as busy (see
    [`erl_driver:set_busy_port`](erl_driver.md#set_busy_port)). As from ERTS
    5.7.4 this flag is required for drivers used by the Erlang distribution (the
    behavior has always been required by drivers used by the distribution).

  - **`ERL_DRV_FLAG_NO_BUSY_MSGQ`** - Disables busy port message queue
    functionality. For more information, see
    [`erl_driver:erl_drv_busy_msgq_limits`](erl_driver.md#erl_drv_busy_msgq_limits).

  - **`ERL_DRV_FLAG_USE_INIT_ACK`** - When this flag is specified, the linked-in
    driver must manually acknowledge that the port has been successfully started
    using [`erl_driver:erl_drv_init_ack()`](erl_driver.md#erl_drv_init_ack).
    This allows the implementor to make the `erlang:open_port` exit with
    `badarg` after some initial asynchronous initialization has been done.

- **`void *handle2`** - This field is reserved for the emulator's internal use.
  The emulator modifies this field, so it is important that the `driver_entry`
  is not declared `const`.

- **`void (*process_exit)(ErlDrvData drv_data, ErlDrvMonitor *monitor)`{: #process_exit }** -
  Called when a monitored process exits. The `drv_data` is
  the data associated with the port for which the process is monitored (using
  [`erl_driver:driver_monitor_process`](erl_driver.md#driver_monitor_process))
  and the `monitor` corresponds to the `ErlDrvMonitor` structure filled in when
  creating the monitor. The driver interface function
  [`erl_driver:driver_get_monitored_process`](erl_driver.md#driver_get_monitored_process)
  can be used to retrieve the process ID of the exiting process as an
  `ErlDrvTermData`.

- **`void (*stop_select)(ErlDrvEvent event, void* reserved)`{: #stop_select }** -
  Called on behalf of
  [`erl_driver:driver_select`](erl_driver.md#driver_select) when it is safe to
  close an event object.

  A typical implementation on Unix is to do `close((int)event)`.

  Argument `reserved` is intended for future use and is to be ignored.

  In contrast to most of the other callback functions, `stop_select` is called
  independent of any port. No `ErlDrvData` argument is passed to the function.
  No driver lock or port lock is guaranteed to be held. The port that called
  `driver_select` can even be closed at the time `stop_select` is called. But it
  can also be the case that `stop_select` is called directly by
  `erl_driver:driver_select`.

  It is not allowed to call any functions in the [driver API](erl_driver.md)
  from `stop_select`. This strict limitation is because the volatile context
  that `stop_select` can be called.

## See Also

[`erl_driver(3)`](erl_driver.md), `m:erlang`, `m:erl_ddll`
