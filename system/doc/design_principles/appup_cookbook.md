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
# Appup Cookbook

[](){: #appup-cookbook }

This section includes examples of `.appup` files for typical cases of
upgrades/downgrades done in runtime.

## Changing a Functional Module

When a functional module has been changed, for example, if a new function has
been added or a bug has been corrected, simple code replacement is sufficient,
for example:

```erlang
{"2",
 [{"1", [{load_module, m}]}],
 [{"1", [{load_module, m}]}]
}.
```

## Changing a Residence Module

In a system implemented according to the OTP design principles, all processes,
except system processes and special processes, reside in one of the behaviours
`m:supervisor`, `m:gen_server`, `m:gen_statem`, `m:gen_event`, or `m:gen_fsm`.
These belong to the STDLIB application and upgrading/downgrading normally
requires a runtime system restart.

Thus, OTP provides no support for changing residence modules except in the case
of [special processes](appup_cookbook.md#spec).

## Changing a Callback Module

A callback module is a functional module, and for code extensions simple code
replacement is sufficient.

_Example_

When adding a function to `ch3`, as described in the example in
[Release Handling](release_handling.md#appup), `ch_app.appup` looks as follows:

```erlang
{"2",
 [{"1", [{load_module, ch3}]}],
 [{"1", [{load_module, ch3}]}]
}.
```

OTP also supports changing the internal state of behaviour processes; see
[Changing Internal State](appup_cookbook.md#int_state).

[](){: #int_state }

## Changing Internal State

In this case, simple code replacement is not sufficient. The process must
explicitly transform its state using the callback function `code_change/3` before
switching to the new version of the callback module. Thus, synchronized code
replacement is used.

_Example_

Consider the `ch3` module from
[gen_server Behaviour](gen_server_concepts.md#ex). The internal state is a term
`Chs` representing the available channels. Assume you want to add a counter `N`,
which keeps track of the number of `alloc` requests so far. This means that the
format must be changed to `{Chs,N}`.

The `.appup` file can look as follows:

```erlang
{"2",
 [{"1", [{update, ch3, {advanced, []}}]}],
 [{"1", [{update, ch3, {advanced, []}}]}]
}.
```

The third element of the `update` instruction is a tuple `{advanced,Extra}`,
which says that the affected processes are to do a state transformation before
loading the new version of the module. This is done by the processes calling the
callback function `code_change/3` (see `m:gen_server` in STDLIB).
The term `Extra`, in this case `[]`, is passed as is to the function:

[](){: #code_change }

```erlang
-module(ch3).
...
-export([code_change/3]).
...
code_change({down, _Vsn}, {Chs, N}, _Extra) ->
    {ok, Chs};
code_change(_Vsn, Chs, _Extra) ->
    {ok, {Chs, 0}}.
```

The first argument is `{down,Vsn}` if there is a downgrade, or `Vsn` if there is
a upgrade. The term `Vsn` is fetched from the 'original' version of the module,
that is, the version you are upgrading from, or downgrading to.

The version is defined by the module attribute `vsn`, if any. There is no such
attribute in `ch3`, so in this case the version is the checksum (a huge integer)
of the beam file, an uninteresting value, which is ignored.

The other callback functions of `ch3` must also be modified and perhaps a new
interface function must be added, but this is not shown here.

## Module Dependencies

Assume that a module is extended by adding an interface function, as in the
example in [Release Handling](release_handling.md#appup), where a function
`available/0` is added to `ch3`.

If a call is added to this function, say in module `m1`, a runtime error could
can occur during release upgrade if the new version of `m1` is loaded first and
calls `ch3:available/0` before the new version of `ch3` is loaded.

Thus, `ch3` must be loaded before `m1`, in the upgrade case, and conversely in
the downgrade case. `m1` is said to be _dependent on_ `ch3`. In a release
handling instruction, this is expressed by the `DepMods` element:

```erlang
{load_module, Module, DepMods}
{update, Module, {advanced, Extra}, DepMods}
```

`DepMods` is a list of modules, on which `Module` is dependent.

_Example_

The module `m1` in application `myapp` is dependent on `ch3` when
upgrading from "1" to "2", or downgrading from "2" to "1":

```erlang
myapp.appup:

{"2",
 [{"1", [{load_module, m1, [ch3]}]}],
 [{"1", [{load_module, m1, [ch3]}]}]
}.

ch_app.appup:

{"2",
 [{"1", [{load_module, ch3}]}],
 [{"1", [{load_module, ch3}]}]
}.
```

If instead `m1` and `ch3` belong to the same application, the `.appup` file can
look as follows:

```erlang
{"2",
 [{"1",
   [{load_module, ch3},
    {load_module, m1, [ch3]}]}],
 [{"1",
   [{load_module, ch3},
    {load_module, m1, [ch3]}]}]
}.
```

`m1` is dependent on `ch3` also when downgrading. `systools` knows the
difference between up- and downgrading and generates a correct `relup`, where
`ch3` is loaded before `m1` when upgrading, but `m1` is loaded before `ch3` when
downgrading.

[](){: #spec }

## Changing Code for a Special Process

In this case, simple code replacement is not sufficient. When a new version of a
residence module for a special process is loaded, the process must make a fully
qualified call to its loop function to switch to the new code. Thus,
synchronized code replacement must be used.

> #### Note {: .info }
>
> The name(s) of the user-defined residence module(s) must be listed in the
> `Modules` part of the child specification for the special process. Otherwise
> the release handler cannot find the process.

_Example_

Consider the example `ch4` in [sys and proc_lib](spec_proc.md#ex).
When started by a supervisor, the child specification can look as follows:

```erlang
{ch4, {ch4, start_link, []},
 permanent, brutal_kill, worker, [ch4]}
```

If `ch4` is part of the application `sp_app` and a new version of the module is
to be loaded when upgrading from version "1" to "2" of this application,
`sp_app.appup` can look as follows:

```erlang
{"2",
 [{"1", [{update, ch4, {advanced, []}}]}],
 [{"1", [{update, ch4, {advanced, []}}]}]
}.
```

The `update` instruction must contain the tuple `{advanced,Extra}`. The
instruction makes the special process call the callback function
`system_code_change/4`, a function the user must implement. The term `Extra`, in
this case `[]`, is passed as is to `system_code_change/4`:

```erlang
-module(ch4).
...
-export([system_code_change/4]).
...

system_code_change(Chs, _Module, _OldVsn, _Extra) ->
    {ok, Chs}.
```

- The first argument is the internal state `State`, passed from
  function [`sys:handle_system_msg(Request, From, Parent, Module, Deb,
  State)`](`sys:handle_system_msg/6`), and called by the special
  process when a system message is received. In `ch4`, the internal
  state is the set of available channels `Chs`.
- The second argument is the name of the module (`ch4`).
- The third argument is `Vsn` or `{down,Vsn}`, as described for
  `c:gen_server:code_change/3` in
  [Changing Internal State](appup_cookbook.md#code_change).

In this case, all arguments but the first are ignored and the function simply
returns the internal state again. This is enough if the code only has been
extended. If instead the internal state is changed (similar to the example in
[Changing Internal State](appup_cookbook.md#int_state)), this is done in this
function and `{ok,Chs2}` returned.

[](){: #sup }

## Changing a Supervisor

The supervisor behaviour supports changing the internal state, that is, changing
the restart strategy and maximum restart frequency properties, as well as
changing the existing child specifications.

Child processes can be added or deleted, but this is not handled automatically.
Instructions must be given by in the `.appup` file.

### Changing Properties

Since the supervisor is to change its internal state, synchronized code
replacement is required. However, a special `update` instruction must be used.

First, the new version of the callback module must be loaded, both in the case
of upgrade and downgrade. Then the new return value of `init/1` can be checked
and the internal state be changed accordingly.

The following `upgrade` instruction is used for supervisors:

```text
{update, Module, supervisor}
```

_Example_

To change the restart strategy of `ch_sup` (from
[Supervisor Behaviour](sup_princ.md#ex)) from `one_for_one` to `one_for_all`,
change the callback function `init/1` in `ch_sup.erl`:

```erlang
-module(ch_sup).
...

init(_Args) ->
    {ok, {#{strategy => one_for_all, ...}, ...}}.
```

The file `ch_app.appup`:

```erlang
{"2",
 [{"1", [{update, ch_sup, supervisor}]}],
 [{"1", [{update, ch_sup, supervisor}]}]
}.
```

### Changing Child Specifications

The instruction, and thus the `.appup` file, when changing an existing child
specification, is the same as when changing properties as described earlier:

```erlang
{"2",
 [{"1", [{update, ch_sup, supervisor}]}],
 [{"1", [{update, ch_sup, supervisor}]}]
}.
```

The changes do not affect existing child processes. For example, changing the
start function only specifies how the child process is to be restarted, if
needed later on.

The id of the child specification cannot be changed.

Changing the `Modules` field of the child specification can affect the release
handling process itself, as this field is used to identify which processes are
affected when doing a synchronized code replacement.

[](){: #sup_add }

### Adding and Deleting Child Processes

As stated earlier, changing child specifications does not affect existing child
processes. New child specifications are automatically added, but not deleted.
Child processes are not automatically started or terminated, this must be done
using `apply` instructions.

_Example_

Assume a new child process `m1` is to be added to `ch_sup` when
upgrading `ch_app` from "1" to "2". This means `m1` is to be deleted when
downgrading from "2" to "1":

```erlang
{"2",
 [{"1",
   [{update, ch_sup, supervisor},
    {apply, {supervisor, restart_child, [ch_sup, m1]}}
   ]}],
 [{"1",
   [{apply, {supervisor, terminate_child, [ch_sup, m1]}},
    {apply, {supervisor, delete_child, [ch_sup, m1]}},
    {update, ch_sup, supervisor}
   ]}]
}.
```

The order of the instructions is important.

The supervisor must be registered as `ch_sup` for the script to work. If the
supervisor is not registered, it cannot be accessed directly from the script.
Instead a help function that finds the pid of the supervisor and calls
`supervisor:restart_child`, and so on, must be written. This function is then to
be called from the script using the `apply` instruction.

If the module `m1` is introduced in version "2" of `ch_app`, it must also be
loaded when upgrading and deleted when downgrading:

```erlang
{"2",
 [{"1",
   [{add_module, m1},
    {update, ch_sup, supervisor},
    {apply, {supervisor, restart_child, [ch_sup, m1]}}
   ]}],
 [{"1",
   [{apply, {supervisor, terminate_child, [ch_sup, m1]}},
    {apply, {supervisor, delete_child, [ch_sup, m1]}},
    {update, ch_sup, supervisor},
    {delete_module, m1}
   ]}]
}.
```

As stated earlier, the order of the instructions is important. When upgrading,
`m1` must be loaded, and the supervisor child specification changed, before the
new child process can be started. When downgrading, the child process must be
terminated before the child specification is changed and the module is deleted.

## Adding or Deleting a Module

_Example

_ A new functional module `m` is added to `ch_app`:

```erlang
{"2",
 [{"1", [{add_module, m}]}],
 [{"1", [{delete_module, m}]}]
```

## Starting or Terminating a Process

In a system structured according to the OTP design principles, any process would
be a child process belonging to a supervisor, see
[Adding and Deleting Child Processes](appup_cookbook.md#sup_add) in Changing a
Supervisor.

## Adding or Removing an Application

When adding or removing an application, no `.appup` file is needed. When
generating `relup`, the `.rel` files are compared and the `add_application` and
`remove_application` instructions are added automatically.

## Restarting an Application

Restarting an application is useful when a change is too complicated to be made
without restarting the processes, for example, if the supervisor hierarchy has
been restructured.

_Example_

When adding a child `m1` to `ch_sup`, as in
[Adding and Deleting Child Processes](appup_cookbook.md#sup_add) in Changing a
Supervisor, an alternative to updating the supervisor is to restart the entire
application:

```erlang
{"2",
 [{"1", [{restart_application, ch_app}]}],
 [{"1", [{restart_application, ch_app}]}]
}.
```

[](){: #app_spec }

## Changing an Application Specification

When installing a release, the application specifications are automatically
updated before evaluating the `relup` script. Thus, no instructions are needed
in the `.appup` file:

```erlang
{"2",
 [{"1", []}],
 [{"1", []}]
}.
```

## Changing Application Configuration

Changing an application configuration by updating the `env` key in the `.app`
file is an instance of changing an application specification, see the previous
section.

Alternatively, application configuration parameters can be added or updated in
`sys.config`.

## Changing Included Applications

The release handling instructions for adding, removing, and restarting
applications apply to primary applications only. There are no corresponding
instructions for included applications. However, since an included application
is really a supervision tree with a topmost supervisor, started as a child
process to a supervisor in the including application, a `.relup` file can be
manually created.

_Example_

Assume there is a release containing an application `prim_app`, which
have a supervisor `prim_sup` in its supervision tree.

In a new version of the release, the application `ch_app` is to be included in
`prim_app`. That is, its topmost supervisor `ch_sup` is to be started as a child
process to `prim_sup`.

The workflow is as follows:

_Step 1)_ Edit the code for `prim_sup`:

```erlang
init(...) ->
    {ok, {...supervisor flags...,
          [...,
           {ch_sup, {ch_sup,start_link,[]},
            permanent,infinity,supervisor,[ch_sup]},
           ...]}}.
```

_Step 2)_ Edit the `.app` file for `prim_app`:

```erlang
{application, prim_app,
 [...,
  {vsn, "2"},
  ...,
  {included_applications, [ch_app]},
  ...
 ]}.
```

_Step 3)_ Create a new `.rel` file, including `ch_app`:

```text
{release,
 ...,
 [...,
  {prim_app, "2"},
  {ch_app, "1"}]}.
```

The included application can be started in two ways. This is described in the
next two sections.

### Application Restart

_Step 4a)_ One way to start the included application is to restart the entire
`prim_app` application. Normally, the `restart_application` instruction in the
`.appup` file for `prim_app` would be used.

However, if this is done and a `.relup` file is generated, not only would it
contain instructions for restarting (that is, removing and adding) `prim_app`,
it would also contain instructions for starting `ch_app` (and stopping it, in
the case of downgrade). This is because `ch_app` is included in the new `.rel`
file, but not in the old one.

Instead, a correct `relup` file can be created manually, either from scratch or
by editing the generated version. The instructions for starting/stopping
`ch_app` are replaced by instructions for loading/unloading the application:

```c
{"B",
 [{"A",
   [],
   [{load_object_code,{ch_app,"1",[ch_sup,ch3]}},
    {load_object_code,{prim_app,"2",[prim_app,prim_sup]}},
    point_of_no_return,
    {apply,{application,stop,[prim_app]}},
    {remove,{prim_app,brutal_purge,brutal_purge}},
    {remove,{prim_sup,brutal_purge,brutal_purge}},
    {purge,[prim_app,prim_sup]},
    {load,{prim_app,brutal_purge,brutal_purge}},
    {load,{prim_sup,brutal_purge,brutal_purge}},
    {load,{ch_sup,brutal_purge,brutal_purge}},
    {load,{ch3,brutal_purge,brutal_purge}},
    {apply,{application,load,[ch_app]}},
    {apply,{application,start,[prim_app,permanent]}}]}],
 [{"A",
   [],
   [{load_object_code,{prim_app,"1",[prim_app,prim_sup]}},
    point_of_no_return,
    {apply,{application,stop,[prim_app]}},
    {apply,{application,unload,[ch_app]}},
    {remove,{ch_sup,brutal_purge,brutal_purge}},
    {remove,{ch3,brutal_purge,brutal_purge}},
    {purge,[ch_sup,ch3]},
    {remove,{prim_app,brutal_purge,brutal_purge}},
    {remove,{prim_sup,brutal_purge,brutal_purge}},
    {purge,[prim_app,prim_sup]},
    {load,{prim_app,brutal_purge,brutal_purge}},
    {load,{prim_sup,brutal_purge,brutal_purge}},
    {apply,{application,start,[prim_app,permanent]}}]}]
}.
```

### Supervisor Change

_Step 4b)_ Another way to start the included application (or stop it in the case
of downgrade) is by combining instructions for adding and removing child
processes to/from `prim_sup` with instructions for loading/unloading all
`ch_app` code and its application specification.

Again, the `.relup` file is created manually, either from scratch or by editing a
generated version. Load all code for `ch_app` first, and also load the
application specification, before `prim_sup` is updated. When downgrading,
`prim_sup` is to updated first, before the code for `ch_app` and its application
specification are unloaded.

```erlang
{"B",
 [{"A",
   [],
   [{load_object_code,{ch_app,"1",[ch_sup,ch3]}},
    {load_object_code,{prim_app,"2",[prim_sup]}},
    point_of_no_return,
    {load,{ch_sup,brutal_purge,brutal_purge}},
    {load,{ch3,brutal_purge,brutal_purge}},
    {apply,{application,load,[ch_app]}},
    {suspend,[prim_sup]},
    {load,{prim_sup,brutal_purge,brutal_purge}},
    {code_change,up,[{prim_sup,[]}]},
    {resume,[prim_sup]},
    {apply,{supervisor,restart_child,[prim_sup,ch_sup]}}]}],
 [{"A",
   [],
   [{load_object_code,{prim_app,"1",[prim_sup]}},
    point_of_no_return,
    {apply,{supervisor,terminate_child,[prim_sup,ch_sup]}},
    {apply,{supervisor,delete_child,[prim_sup,ch_sup]}},
    {suspend,[prim_sup]},
    {load,{prim_sup,brutal_purge,brutal_purge}},
    {code_change,down,[{prim_sup,[]}]},
    {resume,[prim_sup]},
    {remove,{ch_sup,brutal_purge,brutal_purge}},
    {remove,{ch3,brutal_purge,brutal_purge}},
    {purge,[ch_sup,ch3]},
    {apply,{application,unload,[ch_app]}}]}]
}.
```

## Changing Non-Erlang Code

Changing code for a program written in another programming language than Erlang,
for example, a port program, is application-dependent and OTP provides no
special support.

_Example_

When changing code for a port program, assume that the Erlang process
controlling the port is a `gen_server` `portc` and that the port is opened in
the callback function `init/1`:

```erlang
init(...) ->
    ...,
    PortPrg = filename:join(code:priv_dir(App), "portc"),
    Port = open_port({spawn,PortPrg}, [...]),
    ...,
    {ok, #state{port=Port, ...}}.
```

If the port program is to be updated, the code for the `gen_server` can be
extended with a `code_change/3` function, which closes the old port and opens a
new port. (If necessary, the `gen_server` can first request data that must be
saved from the port program and pass this data to the new port):

```erlang
code_change(_OldVsn, State, port) ->
    State#state.port ! close,
    receive
        {Port,close} ->
            true
    end,
    PortPrg = filename:join(code:priv_dir(App), "portc"),
    Port = open_port({spawn,PortPrg}, [...]),
    {ok, #state{port=Port, ...}}.
```

Update the application version number in the `.app` file and write an `.appup`
file:

```erlang
["2",
 [{"1", [{update, portc, {advanced,port}}]}],
 [{"1", [{update, portc, {advanced,port}}]}]
].
```

Ensure that the `priv` directory, where the C program is located, is included in
the new release package:

```erlang
1> systools:make_tar("my_release", [{dirs,[priv]}]).
...
```

## Runtime System Restart and Upgrade

Two upgrade instructions restart the runtime system:

- `restart_new_emulator`

  Intended when ERTS, Kernel, STDLIB, or SASL is upgraded. It is automatically
  added when the `relup` file is generated by `systools:make_relup/3,4`. It is
  executed before all other upgrade instructions. For more information about
  this instruction, see restart_new_emulator (Low-Level) in
  [Release Handling Instructions](release_handling.md#restart_new_emulator_instr).

- `restart_emulator`

  Used when a restart of the runtime system is required after all other upgrade
  instructions are executed. For more information about this instruction, see
  restart_emulator (Low-Level) in
  [Release Handling Instructions](release_handling.md#restart_emulator_instr).

If a runtime system restart is necessary and no upgrade instructions are needed,
that is, if the restart itself is enough for the upgraded applications to start
running the new versions, a simple `.relup` file can be created manually:

```erlang
{"B",
 [{"A",
   [],
   [restart_emulator]}],
 [{"A",
   [],
   [restart_emulator]}]
}.
```

In this case, the release handler framework with automatic packing and unpacking
of release packages, automatic path updates, and so on, can be used without
having to specify `.appup` files.
