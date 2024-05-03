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
# Release Handling

[](){: #release-handling }

## Release Handling Principles

An important feature of the Erlang programming language is the ability to change
module code in runtime, _code replacement_, as described in the Erlang Reference
Manual.

Based on this feature, the OTP application SASL provides a framework for
upgrading and downgrading between different versions of an entire release in
runtime. This is called _release handling_.

The framework consists of:

- Offline support - `systools` for generating scripts and building release
  packages
- Online support - `release_handler` for unpacking and installing release
  packages

The minimal system based on Erlang/OTP, enabling release handling, thus consists
of the Kernel, STDLIB, and SASL applications.

### Release Handling Workflow

_Step 1_) A release is created as described in [Releases](release_structure.md).

_Step 2_) The release is transferred to and installed at target environment. For
information of how to install the first target system, see
[System Principles](`e:system:create_target.md`).

_Step 3_) Modifications, for example, error corrections, are made to the code in
the development environment.

_Step 4_) At some point, it is time to make a new version of release. The
relevant `.app` files are updated and a new `.rel` file is written.

_Step 5_) For each modified application, an
[application upgrade file](release_handling.md#appup), `.appup`, is created. In
this file, it is described how to upgrade and/or downgrade between the old and
new version of the application.

_Step 6_) Based on the `.appup` files, a
[release upgrade file](release_handling.md#relup) called `relup`, is created.
This file describes how to upgrade and/or downgrade between the old and new
version of the entire release.

_Step 7_) A new release package is made and transferred to the target system.

_Step 8_) The new release package is unpacked using the release handler.

_Step 9_) The new version of the release is installed, also using the release
handler. This is done by evaluating the instructions in `relup`. Modules can be
added, deleted, or reloaded, applications can be started, stopped, or restarted,
and so on. In some cases, it is even necessary to restart the entire emulator.

- If the installation fails, the system can be rebooted. The old release version
  is then automatically used.
- If the installation succeeds, the new version is made the default version,
  which is to now be used if there is a system reboot.

### Release Handling Aspects

[Appup Cookbook](appup_cookbook.md), contains examples of `.appup` files for
typical cases of upgrades/downgrades that are normally easy to handle in
runtime. However, many aspects can make release handling complicated, for
example:

- Complicated or circular dependencies can make it difficult or even impossible
  to decide in which order things must be done without risking runtime errors
  during an upgrade or downgrade. Dependencies can be:

  - Between nodes
  - Between processes
  - Between modules

- During release handling, non-affected processes continue normal execution.
  This can lead to time-outs or other problems. For example, new processes
  created in the time window between suspending processes using a certain
  module, and loading a new version of this module, can execute old code.

It is thus recommended that code is changed in as small steps as possible, and
always kept backwards compatible.

[](){: #req }

## Requirements

For release handling to work properly, the runtime system must have knowledge
about which release it is running. It must also be able to change (in runtime)
which boot script and system configuration file to use if the system is
rebooted, for example, by `heart` after a failure. Thus, Erlang must be started
as an embedded system; for information on how to do this, see Embedded System.

For system reboots to work properly, it is also required that the system is
started with heartbeat monitoring, see the [`erl(1)`](`e:erts:erl_cmd.md`)
manual page in ERTS and the `m:heart` manual page in Kernel

Other requirements:

- The boot script included in a release package must be generated from the same
  `.rel` file as the release package itself.

  Information about applications is fetched from the script when an upgrade or
  downgrade is performed.

- The system must be configured using only one system configuration file, called
  `sys.config`.

  If found, this file is automatically included when a release package is
  created.

- All versions of a release, except the first one, must contain a `relup` file.

  If found, this file is automatically included when a release package is
  created.

## Distributed Systems

If the system consists of several Erlang nodes, each node can use its own
version of the release. The release handler is a locally registered process and
must be called at each node where an upgrade or downgrade is required. A release
handling instruction, `sync_nodes`, can be used to synchronize the release
handler processes at a number of nodes, see the `appup(4)` manual page in SASL.

[](){: #instr }

## Release Handling Instructions

OTP supports a set of _release handling instructions_ that are used when
creating `.appup` files. The release handler understands a subset of these, the
_low-level_ instructions. To make it easier for the user, there are also a
number of _high-level_ instructions, which are translated to low-level
instructions by `systools:make_relup`.

Some of the most frequently used instructions are described in this section. The
complete list of instructions is included in the `appup(4)` manual page in SASL.

First, some definitions:

- _Residence module_ \- The module where a process has its tail-recursive loop
  function(s). If these functions are implemented in several modules, all those
  modules are residence modules for the process.
- _Functional module_ \- A module that is not a residence module for any
  process.

For a process implemented using an OTP behaviour, the behaviour module is the
residence module for that process. The callback module is a functional module.

### load_module

If a simple extension has been made to a functional module, it is sufficient to
load the new version of the module into the system, and remove the old version.
This is called _simple code replacement_ and for this the following instruction
is used:

```text
{load_module, Module}
```

### update

If a more complex change has been made, for example, a change to the format of
the internal state of a `gen_server`, simple code replacement is not sufficient.
Instead, it is necessary to:

- Suspend the processes using the module (to avoid that they try to handle any
  requests before the code replacement is completed).
- Ask them to transform the internal state format and switch to the new version
  of the module.
- Remove the old version.
- Resume the processes.

This is called _synchronized code replacement_ and for this the following
instructions are used:

```erlang
{update, Module, {advanced, Extra}}
{update, Module, supervisor}
```

`update` with argument `{advanced,Extra}` is used when changing the internal
state of a behaviour as described above. It causes behaviour processes to call
the callback function `code_change`, passing the term `Extra` and some other
information as arguments. See the manual pages for the respective behaviours and
[Appup Cookbook](appup_cookbook.md#int_state).

`update` with argument `supervisor` is used when changing the start
specification of a supervisor. See [Appup Cookbook](appup_cookbook.md#sup).

When a module is to be updated, the release handler finds which processes that
are _using_ the module by traversing the supervision tree of each running
application and checking all the child specifications:

```erlang
{Id, StartFunc, Restart, Shutdown, Type, Modules}
```

A process uses a module if the name is listed in `Modules` in the child
specification for the process.

If `Modules=dynamic`, which is the case for event managers, the event manager
process informs the release handler about the list of currently installed event
handlers (`gen_event`), and it is checked if the module name is in this list
instead.

The release handler suspends, asks for code change, and resumes processes by
calling the functions `sys:suspend/1,2`, `sys:change_code/4,5`, and
`sys:resume/1,2`, respectively.

### add_module and delete_module

If a new module is introduced, the following instruction is used:

```erlang
{add_module, Module}
```

The instruction loads the module and is necessary when running Erlang in
embedded mode. It is not strictly required when running Erlang in interactive
(default) mode, since the code server then automatically searches for and loads
unloaded modules.

The opposite of `add_module` is `delete_module`, which unloads a module:

```erlang
{delete_module, Module}
```

Any process, in any application, with `Module` as residence module, is killed
when the instruction is evaluated. The user must therefore ensure that all such
processes are terminated before deleting the module, to avoid a situation with
failing supervisor restarts.

### Application Instructions

The following is the instruction for adding an application:

```text
{add_application, Application}
```

Adding an application means that the modules defined by the `modules` key in the
`.app` file are loaded using a number of `add_module` instructions, and then the
application is started.

The following is the instruction for removing an application:

```text
{remove_application, Application}
```

Removing an application means that the application is stopped, the modules are
unloaded using a number of `delete_module` instructions, and then the
application specification is unloaded from the application controller.

The following is the instruction for restarting an application:

```text
{restart_application, Application}
```

Restarting an application means that the application is stopped and then started
again similar to using the instructions `remove_application` and
`add_application` in sequence.

### apply (Low-Level)

To call an arbitrary function from the release handler, the following
instruction is used:

```text
{apply, {M, F, A}}
```

The release handler evaluates [`apply(M, F, A)`](`apply/3`).

[](){: #restart_new_emulator_instr }

### restart_new_emulator (Low-Level)

This instruction is used when changing to a new emulator version, or when any of
the core applications Kernel, STDLIB, or SASL is upgraded. If a system reboot is
needed for another reason, the `restart_emulator` instruction is to be used
instead.

This instruction requires that the system is started with heartbeat monitoring,
see the [`erl(1)`](`e:erts:erl_cmd.md`) manual page in ERTS and the `m:heart`
manual page in Kernel.

The `restart_new_emulator` instruction must always be the first instruction in a
relup. If the relup is generated by `systools:make_relup/3,4`, this is
automatically ensured.

When the release handler encounters the instruction, it first generates a
temporary boot file, which starts the new versions of the emulator and the core
applications, and the old version of all other applications. Then it shuts down
the current emulator by calling `init:reboot()`, see the `m:init` manual page in
Kernel. All processes are terminated gracefully and the system is rebooted by
the `heart` program, using the temporary boot file. After the reboot, the rest
of the relup instructions are executed. This is done as a part of the temporary
boot script.

> #### Warning {: .warning }
>
> This mechanism causes the new versions of the emulator and core applications
> to run with the old version of other applications during startup. Thus, take
> extra care to avoid incompatibility. Incompatible changes in the core
> applications can in some situations be necessary. If possible, such changes
> are preceded by deprecation over two major releases before the actual change.
> To ensure the application is not crashed by an incompatible change, always
> remove any call to deprecated functions as soon as possible.

An info report is written when the upgrade is completed. To programmatically
find out if the upgrade is complete, call
`release_handler:which_releases(current)` and check if it returns the expected
(that is, the new) release.

The new release version must be made permanent when the new emulator is
operational. Otherwise, the old version will be used if there is a new system
reboot.

On UNIX, the release handler tells the `heart` program which command to use to
reboot the system. The environment variable `HEART_COMMAND`, normally used by
the `heart` program, is ignored in this case. The command instead defaults to
`$ROOT/bin/start`. Another command can be set by using the SASL configuration
parameter `start_prg`, see the `sasl(6)` manual page.

[](){: #restart_emulator_instr }

### restart_emulator (Low-Level)

This instruction is not related to upgrades of ERTS or any of the core
applications. It can be used by any application to force a restart of the
emulator after all upgrade instructions are executed.

A relup script can only have one `restart_emulator` instruction and it must
always be placed at the end. If the relup is generated by
`systools:make_relup/3,4`, this is automatically ensured.

When the release handler encounters the instruction, it shuts down the emulator
by calling `init:reboot()`, see the `m:init` manual page in Kernel. All
processes are terminated gracefully and the system can then be rebooted by the
`heart` program using the new release version. No more upgrade instruction is
executed after the restart.

[](){: #appup }

## Application Upgrade File

To define how to upgrade/downgrade between the current version and previous
versions of an application, an _application upgrade file_, or in short an
`.appup` file is created. The file is to be called `Application.appup`, where
`Application` is the application name:

```c
{Vsn,
 [{UpFromVsn1, InstructionsU1},
  ...,
  {UpFromVsnK, InstructionsUK}],
 [{DownToVsn1, InstructionsD1},
  ...,
  {DownToVsnK, InstructionsDK}]}.
```

- `Vsn`, a string, is the current version of the application, as defined in the
  `.app` file.
- Each `UpFromVsn` is a previous version of the application to upgrade from.
- Each `DownToVsn` is a previous version of the application to downgrade to.
- Each `Instructions` is a list of release handling instructions.

`UpFromVsn` and `DownToVsn` can also be specified as regular expressions. For
more information about the syntax and contents of the `.appup` file, see the
[`appup(4)` manual page in SASL](`e:sasl:appup.md`).

[Appup Cookbook](appup_cookbook.md) includes examples of `.appup` files for
typical upgrade/downgrade cases.

_Example:_ Consider the release `ch_rel-1` from
[Releases](release_structure.md#ch_rel). Assume you want to add a function
`available/0` to server `ch3`, which returns the number of available channels
(when trying out the example, change in a copy of the original directory, so
that the first versions are still available):

```erlang
-module(ch3).
-behaviour(gen_server).

-export([start_link/0]).
-export([alloc/0, free/1]).
-export([available/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ch3}, ch3, [], []).

alloc() ->
    gen_server:call(ch3, alloc).

free(Ch) ->
    gen_server:cast(ch3, {free, Ch}).

available() ->
    gen_server:call(ch3, available).

init(_Args) ->
    {ok, channels()}.

handle_call(alloc, _From, Chs) ->
    {Ch, Chs2} = alloc(Chs),
    {reply, Ch, Chs2};
handle_call(available, _From, Chs) ->
    N = available(Chs),
    {reply, N, Chs}.

handle_cast({free, Ch}, Chs) ->
    Chs2 = free(Ch, Chs),
    {noreply, Chs2}.
```

A new version of the `ch_app.app` file must now be created, where the version is
updated:

```erlang
{application, ch_app,
 [{description, "Channel allocator"},
  {vsn, "2"},
  {modules, [ch_app, ch_sup, ch3]},
  {registered, [ch3]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {ch_app,[]}}
 ]}.
```

To upgrade `ch_app` from `"1"` to `"2"` (and to downgrade from `"2"` to `"1"`),
you only need to load the new (old) version of the `ch3` callback module. Create
the application upgrade file `ch_app.appup` in the `ebin` directory:

```erlang
{"2",
 [{"1", [{load_module, ch3}]}],
 [{"1", [{load_module, ch3}]}]
}.
```

[](){: #relup }

## Release Upgrade File

To define how to upgrade/downgrade between the new version and previous versions
of a release, a _release upgrade file_, or in short `relup` file, is to be
created.

This file does not need to be created manually, it can be generated by
`systools:make_relup/3,4`. The relevant versions of the `.rel` file, `.app`
files, and `.appup` files are used as input. It is deduced which applications
are to be added and deleted, and which applications that must be upgraded and/or
downgraded. The instructions for this are fetched from the `.appup` files and
transformed into a single list of low-level instructions in the right order.

If the `relup` file is relatively simple, it can be created manually. It is only
to contain low-level instructions.

For details about the syntax and contents of the release upgrade file, see the
`relup(4)` manual page in SASL.

_Example, continued from the previous section:_ You have a new version "2" of
`ch_app` and an `.appup` file. A new version of the `.rel` file is also needed.
This time the file is called `ch_rel-2.rel` and the release version string is
changed from "A" to "B":

```erlang
{release,
 {"ch_rel", "B"},
 {erts, "5.3"},
 [{kernel, "2.9"},
  {stdlib, "1.12"},
  {sasl, "1.10"},
  {ch_app, "2"}]
}.
```

Now the `relup` file can be generated:

```text
1> systools:make_relup("ch_rel-2", ["ch_rel-1"], ["ch_rel-1"]).
ok
```

This generates a `relup` file with instructions for how to upgrade from version
"A" ("ch_rel-1") to version "B" ("ch_rel-2") and how to downgrade from version
"B" to version "A".

Both the old and new versions of the `.app` and `.rel` files must be in the code
path, as well as the `.appup` and (new) `.beam` files. The code path can be
extended by using the option `path`:

```text
1> systools:make_relup("ch_rel-2", ["ch_rel-1"], ["ch_rel-1"],
[{path,["../ch_rel-1",
"../ch_rel-1/lib/ch_app-1/ebin"]}]).
ok
```

[](){: #rel_handler }

## Installing a Release

When you have made a new version of a release, a release package can be created
with this new version and transferred to the target environment.

To install the new version of the release in runtime, the _release handler_ is
used. This is a process belonging to the SASL application, which handles
unpacking, installation, and removal of release packages. It is communicated
through the `release_handler` module. For details, see the `m:release_handler`
manual page in SASL.

Assuming there is an operational target system with installation root directory
`$ROOT`, the release package with the new version of the release is to be copied
to `$ROOT/releases`.

First, _unpack_ the release package. The files are then extracted from the
package:

```erlang
release_handler:unpack_release(ReleaseName) => {ok, Vsn}
```

- `ReleaseName` is the name of the release package except the `.tar.gz`
  extension.
- `Vsn` is the version of the unpacked release, as defined in its `.rel` file.

A directory `$ROOT/lib/releases/Vsn` is created, where the `.rel` file, the boot
script `start.boot`, the system configuration file `sys.config`, and `relup` are
placed. For applications with new version numbers, the application directories
are placed under `$ROOT/lib`. Unchanged applications are not affected.

An unpacked release can be _installed_. The release handler then evaluates the
instructions in `relup`, step by step:

```erlang
release_handler:install_release(Vsn) => {ok, FromVsn, []}
```

If an error occurs during the installation, the system is rebooted using the old
version of the release. If installation succeeds, the system is afterwards using
the new version of the release, but if anything happens and the system is
rebooted, it starts using the previous version again.

To be made the default version, the newly installed release must be made
_permanent_, which means the previous version becomes _old_:

```text
release_handler:make_permanent(Vsn) => ok
```

The system keeps information about which versions are old and permanent in the
files `$ROOT/releases/RELEASES` and `$ROOT/releases/start_erl.data`.

To downgrade from `Vsn` to `FromVsn`, `install_release` must be called again:

```erlang
release_handler:install_release(FromVsn) => {ok, Vsn, []}
```

An installed, but not permanent, release can be _removed_. Information about the
release is then deleted from `$ROOT/releases/RELEASES` and the release-specific
code, that is, the new application directories and the `$ROOT/releases/Vsn`
directory, are removed.

```text
release_handler:remove_release(Vsn) => ok
```

### Example (continued from the previous sections)

_Step 1)_ Create a target system as described in System Principles of the first
version `"A"` of `ch_rel` from [Releases](release_structure.md#ch_rel). This
time `sys.config` must be included in the release package. If no configuration
is needed, the file is to contain the empty list:

```text
[].
```

_Step 2)_ Start the system as a simple target system. In reality, it is to be
started as an embedded system. However, using `erl` with the correct boot script
and config file is enough for illustration purposes:

```text
% cd $ROOT
% bin/erl -boot $ROOT/releases/A/start -config $ROOT/releases/A/sys
...
```

`$ROOT` is the installation directory of the target system.

_Step 3)_ In another Erlang shell, generate start scripts and create a release
package for the new version `"B"`. Remember to include (a possible updated)
`sys.config` and the `relup` file, see
[Release Upgrade File](release_handling.md#relup).

```erlang
1> systools:make_script("ch_rel-2").
ok
2> systools:make_tar("ch_rel-2").
ok
```

The new release package now also contains version "2" of `ch_app` and the
`relup` file:

```text
% tar tf ch_rel-2.tar
lib/kernel-2.9/ebin/kernel.app
lib/kernel-2.9/ebin/application.beam
...
lib/stdlib-1.12/ebin/stdlib.app
lib/stdlib-1.12/ebin/beam_lib.beam
...
lib/sasl-1.10/ebin/sasl.app
lib/sasl-1.10/ebin/sasl.beam
...
lib/ch_app-2/ebin/ch_app.app
lib/ch_app-2/ebin/ch_app.beam
lib/ch_app-2/ebin/ch_sup.beam
lib/ch_app-2/ebin/ch3.beam
releases/B/start.boot
releases/B/relup
releases/B/sys.config
releases/B/ch_rel-2.rel
releases/ch_rel-2.rel
```

_Step 4)_ Copy the release package `ch_rel-2.tar.gz` to the `$ROOT/releases`
directory.

_Step 5)_ In the running target system, unpack the release package:

```erlang
1> release_handler:unpack_release("ch_rel-2").
{ok,"B"}
```

The new application version `ch_app-2` is installed under `$ROOT/lib` next to
`ch_app-1`. The `kernel`, `stdlib`, and `sasl` directories are not affected, as
they have not changed.

Under `$ROOT/releases`, a new directory `B` is created, containing
`ch_rel-2.rel`, `start.boot`, `sys.config`, and `relup`.

_Step 6)_ Check if the function `ch3:available/0` is available:

```erlang
2> ch3:available().
** exception error: undefined function ch3:available/0
```

_Step 7)_ Install the new release. The instructions in `$ROOT/releases/B/relup`
are executed one by one, resulting in the new version of `ch3` being loaded. The
function `ch3:available/0` is now available:

```erlang
3> release_handler:install_release("B").
{ok,"A",[]}
4> ch3:available().
3
5> code:which(ch3).
".../lib/ch_app-2/ebin/ch3.beam"
6> code:which(ch_sup).
".../lib/ch_app-1/ebin/ch_sup.beam"
```

Processes in `ch_app` for which code have not been updated, for example, the
supervisor, are still evaluating code from `ch_app-1`.

_Step 8)_ If the target system is now rebooted, it uses version "A" again. The
"B" version must be made permanent, to be used when the system is rebooted.

```erlang
7> release_handler:make_permanent("B").
ok
```

[](){: #sys }

## Updating Application Specifications

When a new version of a release is installed, the application specifications are
automatically updated for all loaded applications.

> #### Note {: .info }
>
> The information about the new application specifications is fetched from the
> boot script included in the release package. Thus, it is important that the
> boot script is generated from the same `.rel` file as is used to build the
> release package itself.

Specifically, the application configuration parameters are automatically updated
according to (in increasing priority order):

- The data in the boot script, fetched from the new application resource file
  `App.app`
- The new `sys.config`
- Command-line arguments `-App Par Val`

This means that parameter values set in the other system configuration files and
values set using `application:set_env/3` are disregarded.

When an installed release is made permanent, the system process `init` is set to
point out the new `sys.config`.

After the installation, the application controller compares the old and new
configuration parameters for all running applications and call the callback
function:

```erlang
Module:config_change(Changed, New, Removed)
```

- `Module` is the application callback module as defined by the `mod` key in the
  `.app` file.
- `Changed` and `New` are lists of `{Par,Val}` for all changed and added
  configuration parameters, respectively.
- `Removed` is a list of all parameters `Par` that have been removed.

The function is optional and can be omitted when implementing an application
callback module.
