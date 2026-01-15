%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%% Dynamic Driver Loader and Linker
%%
%% Interface for dynamic library/shared object driver loader/linker.
%% Provides methods for loading, unloading and listing drivers.

-module(erl_ddll).
-moduledoc """
Dynamic driver loader and linker.

This module provides an interface for loading and unloading _Erlang linked-in
drivers_ in runtime.

> #### Note {: .info }
>
> This is a large reference document. For casual use of this module, and for
> most real world applications, the descriptions of functions `load/2` and
> `unload/1` are enough to getting started.

The driver is to be provided as a dynamically linked library in an object code
format specific for the platform in use, that is, `.so` files on most Unix
systems and `.ddl` files on Windows. An Erlang linked-in driver must provide
specific interfaces to the emulator, so this module is not designed for loading
arbitrary dynamic libraries. For more information about Erlang drivers, see
[`erl_driver`](`e:erts:erl_driver.md`) .

[](){: #users }

When describing a set of functions (that is, a module, a part of a module, or an
application), executing in a process and wanting to use a ddll-driver, we use
the term _user_. A process can have many users (different modules needing the
same driver) and many processes running the same code, making up many _users_ of
a driver.

In the basic scenario, each user loads the driver before starting to use it and
unloads the driver when done. The reference counting keeps track of processes
and the number of loads by each process. This way the driver is only unloaded
when no one wants it (it has no user). The driver also keeps track of ports that
are opened to it. This enables delay of unloading until all ports are closed, or
killing of all ports that use the driver when it is unloaded.

[](){: #scenarios }

The interface supports two basic scenarios of loading and unloading. Each
scenario can also have the option of either killing ports when the driver is
unloading, or waiting for the ports to close themselves. The scenarios are as
follows:

- **_Load and Unload on a "When Needed Basis"_** - This (most common) scenario
  simply supports that each [user](`m:erl_ddll#users`) of the driver loads it
  when needed and unloads it when no longer needed. The driver is always
  reference counted and as long as a process keeping the driver loaded is still
  alive, the driver is present in the system.

  Each [user](`m:erl_ddll#users`) of the driver use _literally_ the same
  pathname for the driver when demanding load, but the
  [users](`m:erl_ddll#users`) are not concerned with if the driver is already
  loaded from the file system or if the object code must be loaded from file
  system.

  The following two pairs of functions support this scenario:

  - **_load/2 and unload/1_** - When using the `load/unload` interfaces, the
    driver is not unloaded until the _last port_ using the driver is closed.
    Function [`unload/1`](`unload/1`) can return immediately, as the
    [users](`m:erl_ddll#users`) have no interest in when the unloading occurs.
    The driver is unloaded when no one needs it any longer.

    If a process having the driver loaded dies, it has the same effect as if
    unloading is done.

    When loading, function [`load/2`](`load/2`) returns `ok` when any instance
    of the driver is present. Thus, if a driver is waiting to get unloaded
    (because of open ports), it simply changes state to no longer need
    unloading.

  - **_load_driver/2 and unload_driver/1_** - These interfaces are intended to
    be used when it is considered an error that ports are open to a driver that
    no [user](`m:erl_ddll#users`) has loaded. The ports that are still open when
    the last [user](`m:erl_ddll#users`) calls
    [`unload_driver/1`](`unload_driver/1`) or when the last process having the
    driver loaded dies, are killed with reason `driver_unloaded`.

    The function names `load_driver` and `unload_driver` are kept for backward
    compatibility.

- **_Loading and Reloading for Code Replacement_** - This scenario can occur if
  the driver code needs replacement during operation of the Erlang emulator.
  Implementing driver code replacement is a little more tedious than Beam code
  replacement, as one driver cannot be loaded as both "old" and "new" code. All
  [users](`m:erl_ddll#users`) of a driver must have it closed (no open ports)
  before the old code can be unloaded and the new code can be loaded.

  The unloading/loading is done as one atomic operation, blocking all processes
  in the system from using the driver in question while in progress.

  The preferred way to do driver code replacement is to let _one single process_
  keep track of the driver. When the process starts, the driver is loaded. When
  replacement is required, the driver is reloaded. Unload is probably never
  done, or done when the process exits. If more than one
  [user](`m:erl_ddll#users`) has a driver loaded when code replacement is
  demanded, the replacement cannot occur until the last "other"
  [user](`m:erl_ddll#users`) has unloaded the driver.

  Demanding reload when a reload is already in progress is always an error.
  Using the high-level functions, it is also an error to demand reloading when
  more than one [user](`m:erl_ddll#users`) has the driver loaded.

  To simplify driver replacement, avoid designing your system so that more than
  one [user](`m:erl_ddll#users`) has the driver loaded.

  The two functions for reloading drivers are to be used together with
  corresponding load functions to support the two different behaviors concerning
  open ports:

  - **_load/2 and reload/2_** - This pair of functions is used when reloading is
    to be done after the last open port to the driver is closed.

    As [`reload/2`](`reload/2`) waits for the reloading to occur, a misbehaving
    process keeping open ports to the driver (or keeping the driver loaded) can
    cause infinite waiting for reload. Time-outs must be provided outside of the
    process demanding the reload or by using the low-level interface
    `try_load/3` in combination with driver monitors.

  - **_load_driver/2 and reload_driver/2_** - This pair of functions are used
    when open ports to the driver are to be killed with reason `driver_unloaded`
    to allow for new driver code to get loaded.

    However, if another process has the driver loaded, calling `reload_driver`
    returns error code `pending_process`. As stated earlier, the recommended
    design is to not allow other [users](`m:erl_ddll#users`) than the "driver
    reloader" to demand loading of the driver in question.

## See Also

[`erl_driver(4)`](`e:erts:erl_driver.md`), [`driver_entry(4)`](`e:erts:driver_entry.md`)
""".

-compile(nowarn_deprecated_catch).

-export([load_driver/2, load/2, 
	 unload_driver/1, unload/1, reload/2, reload_driver/2, 
	 format_error/1,info/1,info/0, start/0, stop/0]).

%%----------------------------------------------------------------------------

-type path()   :: string() | atom().
-type driver() :: iolist() | atom().

%%----------------------------------------------------------------------------
%%% BIFs

-export([demonitor/1, info/2, format_error_int/1, monitor/2,
         try_load/3, try_unload/2, loaded_drivers/0]).

-doc """
Removes a driver monitor in much the same way as `erlang:demonitor/1` in ERTS
does with process monitors.

For details about how to create driver monitors, see `monitor/2`,
`try_load/3`, and `try_unload/2`.

The function throws a `badarg` exception if the parameter is not a
`t:reference/0`.
""".
-spec demonitor(MonitorRef) -> ok when
      MonitorRef :: reference().

demonitor(_) ->
    erlang:nif_error(undef).

-doc """
Returns specific information about one aspect of a driver. Parameter `Tag`
specifies which aspect to get information about. The return `Value` differs
between different tags:

- **`processes`** - Returns all processes containing [users](`m:erl_ddll#users`)
  of the specific drivers as a list of tuples `{pid(),integer() >= 0}`, where
  `t:integer/0` denotes the number of users in process `t:pid/0`.

- **`driver_options`** - Returns a list of the driver options provided when
  loading, and any options set by the driver during initialization. The only
  valid option is `kill_ports`.

- **`port_count`** - Returns the number of ports (an `integer() >= 0`) using the
  driver.

- **`linked_in_driver`** - Returns a `t:boolean/0`, which is `true` if the
  driver is a statically linked-in one, otherwise `false`.

- **`permanent`** - Returns a `t:boolean/0`, which is `true` if the driver has
  made itself permanent (and is _not_ a statically linked-in driver), otherwise
  `false`.

- **`awaiting_load`** - Returns a list of all processes having monitors for
  `loading` active. Each process is returned as `{pid(),integer() >= 0}`, where
  `t:integer/0` is the number of monitors held by process `t:pid/0`.

- **`awaiting_unload`** - Returns a list of all processes having monitors for
  `unloading` active. Each process is returned as `{pid(),integer() >= 0}`,
  where `t:integer/0` is the number of monitors held by process `t:pid/0`.

If option `linked_in_driver` or `permanent` returns `true`, all other options
return `linked_in_driver` or `permanent`, respectively.

The function throws a `badarg` exception if the driver is not present in the
system or if the tag is not supported.
""".
-spec info(Name, Tag) -> Value when
      Name :: driver(),
      Tag :: processes | driver_options | port_count | linked_in_driver
           | permanent | awaiting_load | awaiting_unload,
      Value :: term().

info(_, _) ->
    erlang:nif_error(undef).

-doc false.
-spec format_error_int(ErrSpec) -> string() when
      ErrSpec :: term().

format_error_int(_) ->
    erlang:nif_error(undef).

-doc """
Creates a driver monitor and works in many ways as `erlang:monitor/2` in ERTS,
does for processes. When a driver changes state, the monitor results in a
monitor message that is sent to the calling process. `MonitorRef` returned by
this function is included in the message sent.

As with process monitors, each driver monitor set only generates _one single
message_. The monitor is "destroyed" after the message is sent, so it is then
not needed to call `demonitor/1`.

`MonitorRef` can also be used in subsequent calls to `demonitor/1` to remove a
monitor.

The function accepts the following parameters:

- **`Tag`** - The monitor tag is always `driver`, as this function can only be
  used to create driver monitors. In the future, driver monitors will be
  integrated with process monitors, why this parameter has to be specified for
  consistence.

- **`Item`** - Parameter `Item` specifies which driver to monitor (the driver
  name) and which state change to monitor. The parameter is a tuple of arity two
  whose first element is the driver name and second element is one of the
  following:

  - **`loaded`** - Notifies when the driver is reloaded (or loaded if loading is
    underway). It only makes sense to monitor drivers that are in the process of
    being loaded or reloaded. A future driver name for loading cannot be
    monitored. That only results in a `DOWN` message sent immediately.
    Monitoring for loading is therefore most useful when triggered by function
    `try_load/3`, where the monitor is created _because_ the driver is in such a
    pending state.

    Setting a driver monitor for `loading` eventually leads to one of the
    following messages being sent:

    - **`{'UP', reference(), driver, Name, loaded}`** - This message is sent
      either immediately if the driver is already loaded and no reloading is
      pending, or when reloading is executed if reloading is pending.

      The [user](`m:erl_ddll#users`) is expected to know if reloading is
      demanded before creating a monitor for loading.

    - **`{'UP', reference(), driver, Name, permanent}`** - This message is sent
      if reloading was expected, but the (old) driver made itself permanent
      before reloading. It is also sent if the driver was permanent or
      statically linked-in when trying to create the monitor.

    - **`{'DOWN', reference(), driver, Name, load_cancelled}`** - This message
      arrives if reloading was underway, but the requesting
      [user](`m:erl_ddll#users`) cancelled it by dying or calling `try_unload/2`
      (or [`unload/1`](`unload/1`)/[`unload_driver/1`](`unload_driver/1`)) again
      before it was reloaded.

    - **`{'DOWN', reference(), driver, Name, {load_failure, Failure}}`** - This
      message arrives if reloading was underway but the loading for some reason
      failed. The `Failure` term is one of the errors that can be returned from
      `try_load/3`. The error term can be passed to `format_error/1` for
      translation into human readable form. Notice that the translation must be
      done in the same running Erlang virtual machine as the error was detected
      in.

  - **`unloaded`** - Monitors when a driver gets unloaded. If one monitors a
    driver that is not present in the system, one immediately gets notified that
    the driver got unloaded. There is no guarantee that the driver was ever
    loaded.

    A driver monitor for unload eventually results in one of the following
    messages being sent:

    - **`{'DOWN', reference(), driver, Name, unloaded}`** - The monitored driver
      instance is now unloaded. As the unload can be a result of a
      [`reload/2`](`reload/2`) request, the driver can once again have been
      loaded when this message arrives.

    - **`{'UP', reference(), driver, Name, unload_cancelled}`** - This message
      is sent if unloading was expected, but while the driver was waiting for
      all ports to get closed, a new [user](`m:erl_ddll#users`) of the driver
      appeared, and the unloading was cancelled.

      This message appears if `{ok, pending_driver}` was returned from
      `try_unload/2` for the last [user](`m:erl_ddll#users`) of the driver, and
      then `{ok, already_loaded}` is returned from a call to `try_load/3`.

      If one _really_ wants to monitor when the driver gets unloaded, this
      message distorts the picture, because no unloading was done. Option
      `unloaded_only` creates a monitor similar to an `unloaded` monitor, but
      never results in this message.

    - **`{'UP', reference(), driver, Name, permanent}`** - This message is sent
      if unloading was expected, but the driver made itself permanent before
      unloading. It is also sent if trying to monitor a permanent or statically
      linked-in driver.

  - **`unloaded_only`** - A monitor created as `unloaded_only` behaves exactly
    as one created as `unloaded` except that the
    `{'UP', reference(), driver, Name, unload_cancelled}` message is never sent,
    but the monitor instead persists until the driver _really_ gets unloaded.

The function throws a `badarg` exception if the parameters are not specified as
described here.
""".
-spec monitor(Tag, Item) -> MonitorRef when
      Tag :: driver,
      Item :: {Name, When},
      Name :: driver(),
      When :: loaded | unloaded | unloaded_only,
      MonitorRef :: reference().

monitor(_, _) ->
    erlang:nif_error(undef).

-doc """
Provides more control than the [`load/2`](`load/2`)/[`reload/2`](`reload/2`) and
[`load_driver/2`](`load_driver/2`)/[`reload_driver/2`](`reload_driver/2`)
interfaces. It never waits for completion of other operations related to the
driver, but immediately returns the status of the driver as one of the
following:

- **`{ok, loaded}`** - The driver was loaded and is immediately usable.

- **`{ok, already_loaded}`** - The driver was already loaded by another process
  or is in use by a living port, or both. The load by you is registered and a
  corresponding `try_unload` is expected sometime in the future.

- **`{ok, pending_driver}`or `{ok, pending_driver, reference()}`** - The load
  request is registered, but the loading is delayed because an earlier instance
  of the driver is still waiting to get unloaded (open ports use it). Still,
  unload is expected when you are done with the driver. This return value
  _mostly_ occurs when options `{reload,pending_driver}` or `{reload,pending}`
  are used, but _can_ occur when another [user](`m:erl_ddll#users`) is unloading
  a driver in parallel and driver option `kill_ports` is set. In other words,
  this return value always needs to be handled.

- **`{ok, pending_process}`or `{ok, pending_process, reference()}`** - The load
  request is registered, but the loading is delayed because an earlier instance
  of the driver is still waiting to get unloaded by another
  [user](`m:erl_ddll#users`) (not only by a port, in which case
  `{ok,pending_driver}` would have been returned). Still, unload is expected
  when you are done with the driver. This return value _only_ occurs when option
  `{reload,pending}` is used.

When the function returns `{ok, pending_driver}` or `{ok, pending_process}`, one
can get information about when the driver is _actually_ loaded by using option
`{monitor, MonitorOption}`.

When monitoring is requested, and a corresponding `{ok, pending_driver}` or
`{ok, pending_process}` would be returned, the function instead returns a tuple
`{ok, PendingStatus, reference()}` and the process then gets a monitor message
later, when the driver gets loaded. The monitor message to expect is described
in the function description of `monitor/2`.

> #### Note {: .info }
>
> In case of loading, monitoring can _not_ only get triggered by using option
> `{reload, ReloadOption}`, but also in special cases where the load error is
> transient. Thus, `{monitor, pending_driver}` is to be used under basically
> _all_ real world circumstances.

The function accepts the following parameters:

- **`Path`** - The file system path to the directory where the driver object
  file is located. The filename of the object file (minus extension) must
  correspond to the driver name (used in parameter `Name`) and the driver must
  identify itself with the same name. `Path` can be provided as an _iolist()_,
  meaning it can be a list of other `t:iolist/0`s, characters (8-bit integers),
  or binaries, all to be flattened into a sequence of characters.

  The (possibly flattened) `Path` parameter must be consistent throughout the
  system. A driver is to, by all [users](`m:erl_ddll#users`), be loaded using
  the same _literal_ `Path`. The exception is when _reloading_ is requested, in
  which case `Path` can be specified differently. Notice that all
  [users](`m:erl_ddll#users`) trying to load the driver later need to use the
  _new_ `Path` if `Path` is changed using a `reload` option. This is yet another
  reason to have _only one loader_ of a driver one wants to upgrade in a running
  system.

- **`Name`** - This parameter is the name of the driver to be used in subsequent
  calls to function [`erlang:open_port`](`erlang:open_port/2`) in ERTS. The name
  can be specified as an `t:iolist/0` or an `t:atom/0`. The name specified when
  loading is used to find the object file (with the help of `Path` and the
  system-implied extension suffix, that is, `.so`). The name by which the driver
  identifies itself must also be consistent with this `Name` parameter, much as
  the module name of a Beam file much corresponds to its filename.

- **`OptionList`** - Some options can be specified to control the loading
  operation. The options are specified as a list of two-tuples. The tuples have
  the following values and meanings:

  - **`{driver_options, DriverOptionList}`** - This is to provide options that
    changes its general behavior and "sticks" to the driver throughout its
    lifespan.

    The driver options for a specified driver name need always to be consistent,
    _even when the driver is reloaded_, meaning that they are as much a part of
    the driver as the name.

    The only allowed driver option is `kill_ports`, which means that all ports
    opened to the driver are killed with exit reason `driver_unloaded` when no
    process any longer has the driver loaded. This situation arises either when
    the last [user](`m:erl_ddll#users`) calls `try_unload/2`, or when the last
    process having loaded the driver exits.

  - **`{monitor, MonitorOption}`** - A `MonitorOption` tells
    [`try_load/3`](`try_load/3`) to trigger a driver monitor under certain
    conditions. When the monitor is triggered, the function returns a
    three-tuple `{ok, PendingStatus, reference()}`, where `t:reference/0` is the
    monitor reference for the driver monitor.

    Only one `MonitorOption` can be specified. It is one of the following:

    - The atom `pending`, which means that a monitor is to be created whenever a
      load operation is delayed,
    - The atom `pending_driver`, in which a monitor is created whenever the
      operation is delayed because of open ports to an otherwise unused driver.

    Option `pending_driver` is of little use, but is present for completeness,
    as it is well defined which reload options that can give rise to which
    delays. However, it can be a good idea to use the same `MonitorOption` as
    the `ReloadOption`, if present.

    If reloading is not requested, it can still be useful to specify option
    `monitor`, as forced unloads (driver option `kill_ports` or option
    `kill_ports` to `try_unload/2`) trigger a transient state where driver
    loading cannot be performed until all closing ports are closed. Thus, as
    `try_unload` can, in almost all situations, return `{ok, pending_driver}`,
    always specify at least `{monitor, pending_driver}` in production code (see
    the monitor discussion earlier).

  - **`{reload, ReloadOption}`** - This option is used to _reload_ a driver from
    disk, most often in a code upgrade scenario. Having a `reload` option also
    implies that parameter `Path` does _not_ need to be consistent with earlier
    loads of the driver.

    To reload a driver, the process must have loaded the driver before, that is,
    there must be an active [user](`m:erl_ddll#users`) of the driver in the
    process.

    The `reload` option can be either of the following:

    - **`pending`** - With the atom `pending`, reloading is requested for any
      driver and is effectuated when _all_ ports opened to the driver are
      closed. The driver replacement in this case takes place regardless if
      there are still pending [users](`m:erl_ddll#users`) having the driver
      loaded.

      The option also triggers port-killing (if driver option `kill_ports` is
      used) although there are pending users, making it usable for forced driver
      replacement, but laying much responsibility on the driver
      [users](`m:erl_ddll#users`). The pending option is seldom used as one does
      not want other [users](`m:erl_ddll#users`) to have loaded the driver when
      code change is underway.

    - **`pending_driver`** - This option is more useful. Here, reloading is
      queued if the driver is _not_ loaded by any other
      [users](`m:erl_ddll#users`), but the driver has opened ports, in which
      case `{ok, pending_driver}` is returned (a `monitor` option is
      recommended).

    If the driver is unloaded (not present in the system), error code
    `not_loaded` is returned. Option `reload` is intended for when the user has
    already loaded the driver in advance.

The function can return numerous errors, some can only be returned given a
certain combination of options.

Some errors are opaque and can only be interpreted by passing them to function
`format_error/1`, but some can be interpreted directly:

- **`{error,linked_in_driver}`** - The driver with the specified name is an
  Erlang statically linked-in driver, which cannot be manipulated with this API.

- **`{error,inconsistent}`** - The driver is already loaded with other
  `DriverOptionList` or a different _literal_ `Path` argument.

  This can occur even if a `reload` option is specified, if `DriverOptionList`
  differs from the current.

- **`{error, permanent}`** - The driver has requested itself to be permanent,
  making it behave like an Erlang linked-in driver and can no longer be
  manipulated with this API.

- **`{error, pending_process}`** - The driver is loaded by other
  [users](`m:erl_ddll#users`) when option `{reload, pending_driver}` was
  specified.

- **`{error, pending_reload}`** - Driver reload is already requested by another
  [user](`m:erl_ddll#users`) when option `{reload, ReloadOption}` was specified.

- **`{error, not_loaded_by_this_process}`** - Appears when option `reload` is
  specified. The driver `Name` is present in the system, but there is no
  [user](`m:erl_ddll#users`) of it in this process.

- **`{error, not_loaded}`** - Appears when option `reload` is specified. The
  driver `Name` is not in the system. Only drivers loaded by this process can be
  reloaded.

All other error codes are to be translated by function `format_error/1`. Notice
that calls to `format_error` are to be performed from the same running instance
of the Erlang virtual machine as the error is detected in, because of
system-dependent behavior concerning error values.

If the arguments or options are malformed, the function throws a `badarg`
exception.
""".
-spec try_load(Path, Name, OptionList) ->
                      {ok,Status} |
                      {ok, PendingStatus, Ref} |
                      {error, ErrorDesc} when
      Path :: path(),
      Name :: driver(),
      OptionList :: [Option],
      Option :: {driver_options, DriverOptionList}
              | {monitor, MonitorOption}
              | {reload, ReloadOption},
      DriverOptionList :: [DriverOption],
      DriverOption :: kill_ports,
      MonitorOption :: pending_driver | pending,
      ReloadOption :: pending_driver | pending,
      Status :: loaded | already_loaded | PendingStatus,
      PendingStatus :: pending_driver | pending_process,
      Ref :: reference(),
      ErrorDesc :: ErrorAtom | OpaqueError,
      ErrorAtom :: linked_in_driver | inconsistent | permanent
                 | not_loaded_by_this_process | not_loaded
                 |  pending_reload | pending_process,
      OpaqueError :: term().

try_load(_, _, _) ->
    erlang:nif_error(undef).

-doc """
This is the low-level function to unload (or decrement reference counts of) a
driver. It can be used to force port killing, in much the same way as the driver
option `kill_ports` implicitly does. Also, it can trigger a monitor either
because other [users](`m:erl_ddll#users`) still have the driver loaded or
because open ports use the driver.

Unloading can be described as the process of telling the emulator that this
particular part of the code in this particular process (that is, this
[user](`m:erl_ddll#users`)) no longer needs the driver. That can, if there are
no other users, trigger unloading of the driver, in which case the driver name
disappears from the system and (if possible) the memory occupied by the driver
executable code is reclaimed.

If the driver has option `kill_ports` set, or if `kill_ports` is specified as an
option to this function, all pending ports using this driver are killed when
unloading is done by the last [user](`m:erl_ddll#users`). If no port-killing is
involved and there are open ports, the unloading is delayed until no more open
ports use the driver. If, in this case, another [user](`m:erl_ddll#users`) (or
even this user) loads the driver again before the driver is unloaded, the
unloading never takes place.

To allow the [user](`m:erl_ddll#users`) to _request unloading_ to wait for
_actual unloading_, `monitor` triggers can be specified in much the same way as
when loading. However, as [users](`m:erl_ddll#users`) of this function seldom
are interested in more than decrementing the reference counts, monitoring is
seldom needed.

> #### Note {: .info }
>
> If option `kill_ports` is used, monitor trigging is crucial, as the ports are
> not guaranteed to be killed until the driver is unloaded. Thus, a monitor must
> be triggered for at least the `pending_driver` case.

The possible monitor messages to expect are the same as when using option
`unloaded` to function `monitor/2`.

The function returns one of the following statuses upon success:

- **`{ok, unloaded}`** - The driver was immediately unloaded, meaning that the
  driver name is now free to use by other drivers and, if the underlying OS
  permits it, the memory occupied by the driver object code is now reclaimed.

  The driver can only be unloaded when there are no open ports using it and no
  more [users](`m:erl_ddll#users`) require it to be loaded.

- **`{ok, pending_driver}`or `{ok, pending_driver, reference()}`** - Indicates
  that this call removed the last [user](`m:erl_ddll#users`) from the driver,
  but there are still open ports using it. When all ports are closed and no new
  [users](`m:erl_ddll#users`) have arrived, the driver is reloaded and the name
  and memory reclaimed.

  This return value is valid even if option `kill_ports` was used, as killing
  ports can be a process that does not complete immediately. However, the
  condition is in that case transient. Monitors are always useful to detect when
  the driver is really unloaded.

- **`{ok, pending_process}`or `{ok, pending_process, reference()}`** - The
  unload request is registered, but other [users](`m:erl_ddll#users`) still hold
  the driver. Notice that the term `pending_process` can refer to the running
  process; there can be more than one [user](`m:erl_ddll#users`) in the same
  process.

  This is a normal, healthy, return value if the call was just placed to inform
  the emulator that you have no further use of the driver. It is the most common
  return value in the most common [`scenario`](`m:erl_ddll#scenarios`) described
  in the introduction.

The function accepts the following parameters:

- **`Name`** - `Name` is the name of the driver to be unloaded. The name can be
  specified as an `t:iolist/0` or as an `t:atom/0`.

- **`OptionList`** - Argument `OptionList` can be used to specify certain
  behavior regarding ports and triggering monitors under certain conditions:

  - **`kill_ports`** - Forces killing of all ports opened using this driver,
    with exit reason `driver_unloaded`, if you are the _last_
    [user](`m:erl_ddll#users`) of the driver.

    If other [users](`m:erl_ddll#users`) have the driver loaded, this option has
    no effect.

    To get the consistent behavior of killing ports when the last
    [user](`m:erl_ddll#users`) unloads, use driver option `kill_ports` when
    loading the driver instead.

  - **`{monitor, MonitorOption}`** - Creates a driver monitor if the condition
    specified in `MonitorOption` is true. The valid options are:

    - **`pending_driver`** - Creates a driver monitor if the return value is to
      be `{ok, pending_driver}`.

    - **`pending`** - Creates a monitor if the return value is
      `{ok, pending_driver}` or `{ok, pending_process}`.

    The `pending_driver` `MonitorOption` is by far the most useful. It must be
    used to ensure that the driver really is unloaded and the ports closed
    whenever option `kill_ports` is used, or the driver can have been loaded
    with driver option `kill_ports`.

    Using the monitor triggers in the call to `try_unload` ensures that the
    monitor is added before the unloading is executed, meaning that the monitor
    is always properly triggered, which is not the case if
    [`monitor/2`](`monitor/2`) is called separately.

The function can return the following error conditions, all well specified (no
opaque values):

- **`{error, linked_in_driver}`** - You were trying to unload an Erlang
  statically linked-in driver, which cannot be manipulated with this interface
  (and cannot be unloaded at all).

- **`{error, not_loaded}`** - The driver `Name` is not present in the system.

- **`{error, not_loaded_by_this_process}`** - The driver `Name` is present in
  the system, but there is no [user](`m:erl_ddll#users`) of it in this process.

  As a special case, drivers can be unloaded from processes that have done no
  corresponding call to [`try_load/3`](`try_load/3`) if, and only if, there are
  _no users of the driver at all_, which can occur if the process containing the
  last user dies.

- **`{error, permanent}`** - The driver has made itself permanent, in which case
  it can no longer be manipulated by this interface (much like a statically
  linked-in driver).

The function throws a `badarg` exception if the parameters are not specified as
described here.
""".
-spec try_unload(Name, OptionList) ->
                        {ok, Status} |
                        {ok, PendingStatus, Ref} |
                        {error, ErrorAtom} when
      Name :: driver(),
      OptionList :: [Option],
      Option :: {monitor, MonitorOption} | kill_ports,
      MonitorOption :: pending_driver | pending,
      Status :: unloaded | PendingStatus,
      PendingStatus :: pending_driver | pending_process,
      Ref :: reference(),
      ErrorAtom :: linked_in_driver | not_loaded |
                   not_loaded_by_this_process | permanent.

try_unload(_, _) ->
    erlang:nif_error(undef).

-doc """
Returns a list of all the available drivers, both (statically) linked-in and
dynamically loaded ones.

The driver names are returned as a list of strings rather than a list of atoms
for historical reasons.

For more information about drivers, see [`info`](`info/0`).
""".
-spec loaded_drivers() -> {ok, Drivers} when
      Drivers :: [Driver],
      Driver :: string().

loaded_drivers() ->
    erlang:nif_error(undef).

%%% End of BIFs


-doc false.
-spec start() -> {'error', {'already_started', 'undefined'}}.

start() ->
    {error, {already_started,undefined}}.

-doc false.
-spec stop() -> 'ok'.

stop() ->
    ok.

-doc """
Works essentially as [`load/2`](`load/2`), but loads the driver with other
options. All ports using the driver are killed with reason `driver_unloaded`
when the driver is to be unloaded.

The number of loads and unloads by different [users](`m:erl_ddll#users`)
influences the loading and unloading of a driver file. The port killing
therefore only occurs when the _last_ [user](`m:erl_ddll#users`) unloads the
driver, or when the last process having loaded the driver exits.

This interface (or at least the name of the functions) is kept for backward
compatibility. Using `try_load/3` with `{driver_options,[kill_ports]}` in the
option list gives the same effect regarding the port killing.

The function throws a `badarg` exception if the parameters are not specified as
described here.
""".
-spec load_driver(Path, Name) -> 'ok' | {'error', ErrorDesc} when
      Path :: path(),
      Name :: driver(),
      ErrorDesc :: term().

load_driver(Path, Driver) ->
    do_load_driver(Path, Driver, [{driver_options,[kill_ports]}]).

-doc """
Loads and links the dynamic driver `Name`. `Path` is a file path to the
directory containing the driver. `Name` must be a shareable object/dynamic
library. Two drivers with different `Path` parameters cannot be loaded under the
same name. `Name` is a string or atom containing at least one character.

The `Name` specified is to correspond to the filename of the dynamically
loadable object file residing in the directory specified as `Path`, but
_without_ the extension (that is, `.so`). The driver name provided in the driver
initialization routine must correspond with the filename, in much the same way
as Erlang module names correspond to the names of the `.beam` files.

If the driver was previously unloaded, but is still present because of open
ports to it, a call to [`load/2`](`load/2`) stops the unloading and keeps the
driver (as long as `Path` is the same), and `ok` is returned. If you really want
the object code to be reloaded, use `reload/2` or the low-level interface
`try_load/3` instead. See also the description of
[`different scenarios`](`m:erl_ddll#scenarios`) for loading/unloading in the
introduction.

If more than one process tries to load an already loaded driver with the same
`Path`, or if the same process tries to load it many times, the function returns
`ok`. The emulator keeps track of the [`load/2`](`load/2`) calls, so that a
corresponding number of `unload/2` calls must be done from the same process
before the driver gets unloaded. It is therefore safe for an application to load
a driver that is shared between processes or applications when needed. It can
safely be unloaded without causing trouble for other parts of the system.

It is not allowed to load multiple drivers with the same name but with different
`Path` parameters.

> #### Note {: .info }
>
> `Path` is interpreted literally, so that all loaders of the same driver must
> specify the same _literal_ `Path` string, although different paths can point
> out the same directory in the file system (because of use of relative paths
> and links).

On success, the function returns `ok`. On failure, the return value is
`{error,ErrorDesc}`, where `ErrorDesc` is an opaque term to be translated into
human readable form by function `format_error/1`.

For more control over the error handling, use the `try_load/3` interface
instead.

The function throws a `badarg` exception if the parameters are not specified as
described here.
""".
-spec load(Path, Name) -> 'ok' | {'error', ErrorDesc} when
      Path :: path(),
      Name :: driver(),
      ErrorDesc ::term().

load(Path, Driver) ->
    do_load_driver(Path, Driver, []).

do_load_driver(Path, Driver, DriverFlags) ->
    case erl_ddll:try_load(Path, Driver,[{monitor,pending_driver}]++DriverFlags) of
	{error, inconsistent} ->
	    {error,bad_driver_name}; % BC 
	{error, What} ->
	    {error,What};
	{ok, already_loaded} ->
	    ok;
	{ok,loaded} ->
	    ok;
	{ok, pending_driver, Ref} ->
	    receive
		{'DOWN', Ref, driver, _, load_cancelled} ->
		    {error, load_cancelled};
		{'UP', Ref, driver, _, permanent} ->
		    {error, permanent}; 
		{'DOWN', Ref, driver, _, {load_failure, Failure}} ->
		    {error, Failure};
		{'UP', Ref, driver, _, loaded} ->
		    ok
	    end
    end.

do_unload_driver(Driver,Flags) ->
    case erl_ddll:try_unload(Driver,Flags) of
	{error,What} ->
	    {error,What};
	{ok, pending_process} ->
	    ok;
	{ok, unloaded} ->
	    ok;
	{ok, pending_driver} ->
	    ok;
	{ok, pending_driver, Ref} ->
	    receive
		{'UP', Ref, driver, _, permanent} ->
		    {error, permanent}; 
		{'UP', Ref, driver, _, unload_cancelled} ->
		    ok;
		{'DOWN', Ref, driver, _, unloaded} ->
		    ok
	    end
    end.

-doc """
Unloads, or at least dereferences the driver named `Name`. If the caller is the
last [user](`m:erl_ddll#users`) of the driver, all remaining open ports using
the driver are killed with reason `driver_unloaded` and the driver eventually
gets unloaded.

If there are other [users](`m:erl_ddll#users`) of the driver, the reference
counts of the driver is merely decreased, so that the caller is no longer
considered a [user](`m:erl_ddll#users`). For use scenarios, see the
[`description`](`m:erl_ddll#scenarios`) in the beginning of this module.

The `ErrorDesc` returned is an opaque value to be passed further on to function
`format_error/1`. For more control over the operation, use the `try_unload/2`
interface.

The function throws a `badarg` exception if the parameters are not specified as
described here.
""".
-spec unload_driver(Name) -> 'ok' | {'error', ErrorDesc} when
      Name :: driver(),
      ErrorDesc :: term().

unload_driver(Driver) ->
    do_unload_driver(Driver,[{monitor,pending_driver},kill_ports]).

-doc """
Unloads, or at least dereferences the driver named `Name`. If the caller is the
last [user](`m:erl_ddll#users`) of the driver, and no more open ports use the
driver, the driver gets unloaded. Otherwise, unloading is delayed until all
ports are closed and no [users](`m:erl_ddll#users`) remain.

If there are other [users](`m:erl_ddll#users`) of the driver, the reference
counts of the driver is merely decreased, so that the caller is no longer
considered a [user](`m:erl_ddll#users`) of the driver. For use scenarios, see
the [`description`](`m:erl_ddll#scenarios`) in the beginning of this module.

The `ErrorDesc` returned is an opaque value to be passed further on to function
`format_error/1`. For more control over the operation, use the `try_unload/2`
interface.

The function throws a `badarg` exception if the parameters are not specified as
described here.
""".
-spec unload(Name) -> 'ok' | {'error', ErrorDesc} when
      Name :: driver(),
      ErrorDesc :: term().

unload(Driver) ->
    do_unload_driver(Driver,[]).

-doc """
Reloads the driver named `Name` from a possibly different `Path` than previously
used. This function is used in the code change
[`scenario`](`m:erl_ddll#scenarios`) described in the introduction.

If there are other [users](`m:erl_ddll#users`) of this driver, the function
returns `{error, pending_process}`, but if there are no other users, the
function call hangs until all open ports are closed.

> #### Note {: .info }
>
> Avoid mixing multiple [users](`m:erl_ddll#users`) with driver reload requests.

To avoid hanging on open ports, use function `try_load/3` instead.

The `Name` and `Path` parameters have exactly the same meaning as when calling
the plain function `load/2`.

On success, the function returns `ok`. On failure, the function returns an
opaque error, except the `pending_process` error described earlier. The opaque
errors are to be translated into human readable form by function
`format_error/1`.

For more control over the error handling, use the `try_load/3` interface
instead.

The function throws a `badarg` exception if the parameters are not specified as
described here.
""".
-spec reload(Path, Name) -> 'ok' | {'error', ErrorDesc} when
      Path :: path(),
      Name :: driver(),
      ErrorDesc :: pending_process | OpaqueError,
      OpaqueError :: term().

reload(Path,Driver) ->
    do_load_driver(Path, Driver, [{reload,pending_driver}]).

-doc """
Works exactly as `reload/2`, but for drivers loaded with the `load_driver/2`
interface.

As this interface implies that ports are killed when the last user disappears,
the function does not hang waiting for ports to get closed.

For more details, see [`scenarios`](`m:erl_ddll#scenarios`) in this module
description and the function description for `reload/2`.

The function throws a `badarg` exception if the parameters are not specified as
described here.
""".
-spec reload_driver(Path, Name) -> 'ok' | {'error', ErrorDesc} when
      Path :: path(),
      Name :: driver(),
      ErrorDesc :: pending_process | OpaqueError,
      OpaqueError :: term().

reload_driver(Path,Driver) ->
    do_load_driver(Path, Driver, [{reload,pending_driver},
				  {driver_options,[kill_ports]}]).			    

-doc """
Takes an `ErrorDesc` returned by load, unload, or reload functions and returns a
string that describes the error or warning.

> #### Note {: .info }
>
> Because of peculiarities in the dynamic loading interfaces on different
> platforms, the returned string is only guaranteed to describe the correct
> error _if format_error/1 is called in the same instance of the Erlang virtual
> machine as the error appeared in_ (meaning the same operating system process).
""".
-spec format_error(ErrorDesc) -> string() when
      ErrorDesc :: term().

format_error(Code) ->
    case Code of
	%% This is the only error code returned only from erlang code...
	%% 'permanent' has a translation in the emulator, even though the erlang code uses it to...
	load_cancelled ->
	    "Loading was cancelled from other process";
	_ ->
	    erl_ddll:format_error_int(Code)
    end.

-doc """
Returns a list of tuples `{Tag, Value}`, where `Tag` is the information item and
`Value` is the result of calling `info/2` with this driver name and this tag.
The result is a tuple list containing all information available about a driver.

The following tags appears in the list:

- `processes`
- `driver_options`
- `port_count`
- `linked_in_driver`
- `permanent`
- `awaiting_load`
- `awaiting_unload`

For a detailed description of each value, see `info/2`.

The function throws a `badarg` exception if the driver is not present in the
system.
""".
-spec info(Name) -> InfoList when
      Name :: driver(),
      InfoList :: [InfoItem, ...],
      InfoItem :: {Tag :: atom(), Value :: term()}.
 
info(Driver) ->
    [{processes, erl_ddll:info(Driver,processes)},
     {driver_options, erl_ddll:info(Driver,driver_options)},
     {port_count, erl_ddll:info(Driver,port_count)},
     {linked_in_driver, erl_ddll:info(Driver,linked_in_driver)},
     {permanent, erl_ddll:info(Driver,permanent)},
     {awaiting_load,  erl_ddll:info(Driver,awaiting_load)},
     {awaiting_unload, erl_ddll:info(Driver,awaiting_unload)}].

-doc """
Returns a list of tuples `{DriverName, InfoList}`, where `InfoList` is the
result of calling `info/1` for that `DriverName`. Only dynamically linked-in
drivers are included in the list.
""".
-spec info() -> AllInfoList when
      AllInfoList :: [DriverInfo],
      DriverInfo :: {DriverName, InfoList},
      DriverName :: string(),
      InfoList :: [InfoItem],
      InfoItem :: {Tag :: atom(), Value :: term()}.

info() ->
    {ok,DriverList} = erl_ddll:loaded_drivers(),
    [{X,Y} || X <- DriverList,
	       Y <- [catch info(X)],
	       is_list(Y), not lists:member({linked_in_driver,true},Y)]. 
