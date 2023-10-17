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
# appup

Application upgrade file

## Description

The _application upgrade file_ defines how an application is upgraded or
downgraded in a running system.

This file is used by the functions in `m:systools` when generating a release
upgrade file `relup`.

## File Syntax

The application upgrade file is to be called `Application.appup`, where
`Application` is the application name. The file is to be located in the `ebin`
directory for the application.

The `.appup` file contains one single Erlang term, which defines the
instructions used to upgrade or downgrade the application. The file has the
following syntax:

```c
{Vsn,
  [{UpFromVsn, Instructions}, ...],
  [{DownToVsn, Instructions}, ...]}.
```

- **`Vsn = string()`** - Current application version.

- **`UpFromVsn = string() | binary()`** - An earlier application version to
  upgrade from. If it is a string, it is interpreted as a specific version
  number. If it is a binary, it is interpreted as a regular expression that can
  match multiple version numbers.

- **`DownToVsn = string() | binary()`** - An earlier application version to
  downgrade to. If it is a string, it is interpreted as a specific version
  number. If it is a binary, it is interpreted as a regular expression that can
  match multiple version numbers.

- **`Instructions`** - A list of _release upgrade instructions_, see
  [Release Upgrade Instructions](appup.md#release-upgrade-instructions). It
  is recommended to use high-level instructions only. These are automatically
  translated to low-level instructions by `systools` when creating the `relup`
  file.

To avoid duplication of upgrade instructions, it is allowed to use regular
expressions to specify `UpFromVsn` and `DownToVsn`. To be considered a regular
expression, the version identifier must be specified as a binary. For example,
the following match all versions `2.1.x`, where `x` is any number:

```text
<<"2\\.1\\.[0-9]+">>
```

Notice that the regular expression must match the complete version string, so
this example works for, for example, `2.1.1`, but not for `2.1.1.1`.

## Release Upgrade Instructions

Release upgrade instructions are interpreted by the release handler when an
upgrade or downgrade is made. For more information about release handling, see
[OTP Design Principles](`e:system:release_handling.md`) in _System
Documentation_.

A process is said to _use_ a module `Mod` if `Mod` is listed in the `Modules`
part of the child specification used to start the process, see `m:supervisor`.
In the case of `m:gen_event`, an event manager process is said to use `Mod` if
`Mod` is an installed event handler.

### High-Level Instructions

```text
{update, Mod}
{update, Mod, supervisor}
{update, Mod, Change}
{update, Mod, DepMods}
{update, Mod, Change, DepMods}
{update, Mod, Change, PrePurge, PostPurge, DepMods}
{update, Mod, Timeout, Change, PrePurge, PostPurge, DepMods}
{update, Mod, ModType, Timeout, Change, PrePurge, PostPurge, DepMods}
  Mod = atom()
  ModType = static | dynamic
  Timeout = int()>0 | default | infinity
  Change = soft | {advanced,Extra}
    Extra = term()
  PrePurge = PostPurge = soft_purge | brutal_purge
  DepMods = [Mod]
```

Synchronized code replacement of processes using module `Mod`.

All those processes are suspended using [`sys:suspend`](`sys:suspend/1`), the
new module version is loaded, and then the processes are resumed using
[`sys:resume`](`sys:resume/1`).

- **`Change`** - Defaults to `soft` and defines the type of code change. If it
  is set to `{advanced,Extra}`, implemented processes using `m:gen_server`,
  `m:gen_fsm`, `m:gen_statem`, or `m:gen_event` transform their internal state
  by calling the callback function `code_change`. Special processes call the
  callback function `system_code_change/4`. In both cases, the term `Extra` is
  passed as an argument to the callback function.

- **`PrePurge`** - Defaults to `brutal_purge`. It controls what action to take
  with processes executing old code before loading the new module version. If
  the value is `brutal_purge`, the processes are killed. If the value is
  `soft_purge`, `release_handler:install_release/1` returns
  `{error,{old_processes,Mod}}`.

- **`PostPurge`** - Defaults to `brutal_purge`. It controls what action to take
  with processes that are executing old code when the new module version has
  been loaded. If the value is `brutal_purge`, the code is purged when the
  release is made permanent and the processes are killed. If the value is
  `soft_purge`, the release handler purges the old code when no remaining
  processes execute the code.

- **`DepMods`** - Defaults to `[]` and defines other modules that `Mod` is
  dependent on. In the `relup` file, instructions for suspending processes using
  `Mod` come before instructions for suspending processes using modules in
  `DepMods` when upgrading, and conversely when downgrading. In case of circular
  dependencies, the order of the instructions in the `appup` file is kept.

- **`Timeout`** - Defines the time-out when suspending processes. If no value or
  `default` is specified, the default value for [`sys:suspend`](`sys:suspend/1`)
  is used.

- **`ModType`** - Defaults to `dynamic`. It specifies if the code is "dynamic",
  that is, if a process using the module spontaneously switches to new code, or
  if it is "static". When doing an advanced update and upgrade, the new version
  of a dynamic module is loaded before the process is asked to change code. When
  downgrading, the process is asked to change code before loading the new
  version. For static modules, the new version is loaded before the process is
  asked to change code, both in the case of upgrading and downgrading. Callback
  modules are dynamic.

`update` with argument `supervisor` is used when changing the start
specification of a supervisor.

```erlang
{load_module, Mod}
{load_module, Mod, DepMods}
{load_module, Mod, PrePurge, PostPurge, DepMods}
  Mod = atom()
  PrePurge = PostPurge = soft_purge | brutal_purge
  DepMods = [Mod]
```

Simple code replacement of the module `Mod`.

For a description of `PrePurge` and `PostPurge`, see `update` above.

`DepMods` defaults to `[]` and defines which other modules `Mod` is dependent
on. In the `relup` file, instructions for loading these modules come before the
instruction for loading `Mod` when upgrading, and conversely when downgrading.

```erlang
{add_module, Mod}
{add_module, Mod, DepMods}
  Mod = atom()
  DepMods = [Mod]
```

Loads a new module `Mod`.

`DepMods` defaults to `[]` and defines which other modules `Mod` is dependent
on. In the `relup` file, instructions related to these modules come before the
instruction for loading `Mod` when upgrading, and conversely when downgrading.

```erlang
{delete_module, Mod}
{delete_module, Mod, DepMods}
  Mod = atom()
```

Deletes a module `Mod` using the low-level instructions `remove` and `purge`.

`DepMods` defaults to `[]` and defines which other modules `Mod` is dependent
on. In the `relup` file, instructions related to these modules come before the
instruction for removing `Mod` when upgrading, and conversely when downgrading.

```text
{add_application, Application}
{add_application, Application, Type}
  Application = atom()
  Type = permanent | transient | temporary | load | none
```

Adding an application means that the modules defined by the `modules` key in the
`.app` file are loaded using `add_module`.

`Type` defaults to `permanent` and specifies the start type of the application.
If `Type = permanent | transient | temporary`, the application is loaded and
started in the corresponding way, see `m:application`. If `Type = load`, the
application is only loaded. If `Type = none`, the application is not loaded and
not started, although the code for its modules is loaded.

```text
{remove_application, Application}
  Application = atom()
```

Removing an application means that the application is stopped, the modules are
unloaded using `delete_module`, and then the application specification is
unloaded from the application controller.

```text
{restart_application, Application}
  Application = atom()
```

Restarting an application means that the application is stopped and then started
again, similar to using the instructions `remove_application` and
`add_application` in sequence. Note that, even if the application has been
started before the release upgrade is performed, `restart_application` may only
`load` it rather than `start` it, depending on the application's `start type`:
If `Type = load`, the application is only loaded. If `Type = none`, the
application is not loaded and not started, although the code for its modules is
loaded.

### Low-Level Instructions

```erlang
{load_object_code, {App, Vsn, [Mod]}}
  App = Mod = atom()
  Vsn = string()
```

Reads each `Mod` from directory `App-Vsn/ebin` as a binary. It does not load the
modules. The instruction is to be placed first in the script to read all new
code from the file to make the suspend-load-resume cycle less time-consuming.

```text
point_of_no_return
```

If a crash occurs after this instruction, the system cannot recover and is
restarted from the old release version. The instruction must only occur once in
a script. It is to be placed after all `load_object_code` instructions.

```text
{load, {Mod, PrePurge, PostPurge}}
  Mod = atom()
  PrePurge = PostPurge = soft_purge | brutal_purge
```

Before this instruction occurs, `Mod` must have been loaded using
`load_object_code`. This instruction loads the module. `PrePurge` is ignored.
For a description of `PostPurge`, see the high-level instruction `update`
earlier.

```text
{remove, {Mod, PrePurge, PostPurge}}
  Mod = atom()
  PrePurge = PostPurge = soft_purge | brutal_purge
```

Makes the current version of `Mod` old. `PrePurge` is ignored. For a description
of `PostPurge`, see the high-level instruction `update` earlier.

```text
{purge, [Mod]}
  Mod = atom()
```

Purges each module `Mod`, that is, removes the old code. Notice that any process
executing purged code is killed.

```erlang
{suspend, [Mod | {Mod, Timeout}]}
  Mod = atom()
  Timeout = int()>0 | default | infinity
```

Tries to suspend all processes using a module `Mod`. If a process does not
respond, it is ignored. This can cause the process to die, either because it
crashes when it spontaneously switches to new code, or as a result of a purge
operation. If no `Timeout` is specified or `default` is specified, the default
value for [`sys:suspend`](`sys:suspend/1`) is used.

```text
{resume, [Mod]}
  Mod = atom()
```

Resumes all suspended processes using a module `Mod`.

```erlang
{code_change, [{Mod, Extra}]}
{code_change, Mode, [{Mod, Extra}]}
  Mod = atom()
  Mode = up | down
  Extra = term()
```

`Mode` defaults to `up` and specifies if it is an upgrade or downgrade. This
instruction sends a `code_change` system message to all processes using a module
`Mod` by calling function [`sys:change_code`](`sys:change_code/4`), passing term
`Extra` as argument.

```text
{stop, [Mod]}
  Mod = atom()
```

Stops all processes using a module `Mod` by calling
`supervisor:terminate_child/2`. This instruction is useful when the simplest way
to change code is to stop and restart the processes that run the code.

```erlang
{start, [Mod]}
  Mod = atom()
```

Starts all stopped processes using a module `Mod` by calling
`supervisor:restart_child/2`.

```erlang
{sync_nodes, Id, [Node]}
{sync_nodes, Id, {M, F, A}}
  Id = term()
  Node = node()
  M = F = atom()
  A = [term()]
```

[`apply(M, F, A)`](`apply/3`) must return a list of nodes.

This instruction synchronizes the release installation with other nodes. Each
`Node` must evaluate this command with the same `Id`. The local node waits for
all other nodes to evaluate the instruction before execution continues. If a
node goes down, it is considered to be an unrecoverable error, and the local
node is restarted from the old release. There is no time-out for this
instruction, which means that it can hang forever.

```erlang
{apply, {M, F, A}}
  M = F = atom()
  A = [term()]
```

Evaluates [`apply(M, F, A)`](`apply/3`).

If the instruction appears before instruction `point_of_no_return`, a failure is
caught. `release_handler:install_release/1` then returns
`{error,{'EXIT',Reason}}`, unless `{error,Error}` is thrown or returned. Then it
returns `{error,Error}`.

If the instruction appears after instruction `point_of_no_return` and the
function call fails, the system is restarted.

```text
restart_new_emulator
```

This instruction is used when the application ERTS, Kernel, STDLIB, or SASL is
upgraded. It shuts down the current emulator and starts a new one. All processes
are terminated gracefully, and the new version of ERTS, Kernel, STDLIB, and SASL
are used when the emulator restarts. Only one `restart_new_emulator` instruction
is allowed in the `relup` file, and it must be placed first.
[`systools:make_relup/3,4`](`systools:make_relup/3`) ensures this when the
`relup` file is generated. The rest of the instructions in the `relup` file is
executed after the restart as a part of the boot script.

An info report is written when the upgrade is completed. To programmatically
determine if the upgrade is complete, call
[`release_handler:which_releases/0,1`](`release_handler:which_releases/0`) and
check if the expected release has status `current`.

The new release must still be made permanent after the upgrade is completed,
otherwise the old emulator is started if there is an emulator restart.

> #### Warning {: .warning }
>
> As stated earlier, instruction `restart_new_emulator` causes the emulator to
> be restarted with new versions of ERTS, Kernel, STDLIB, and SASL. However, all
> other applications do at startup run their old versions in this new emulator.
> This is usually no problem, but every now and then incompatible changes occur
> to the core applications, which can cause trouble in this setting. Such
> incompatible changes (when functions are removed) are normally preceded by a
> deprecation over two major releases. To ensure that your application is not
> crashed by an incompatible change, always remove any call to deprecated
> functions as soon as possible.

```text
restart_emulator
```

This instruction is similar to `restart_new_emulator`, except it must be placed
at the end of the `relup` file. It is not related to an upgrade of the emulator
or the core applications, but can be used by any application when a complete
reboot of the system is required.

When generating the `relup` file,
[`systools:make_relup/3,4`](`systools:make_relup/3`) ensures that there is only
one `restart_emulator` instruction and that it is the last instruction in the
`relup` file.

## See Also

`m:release_handler`, [`relup(4)`](relup.md), `m:supervisor`, `m:systools`
