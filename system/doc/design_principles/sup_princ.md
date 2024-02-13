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
# Supervisor Behaviour

This section should be read with the `m:supervisor` manual page in STDLIB, where
all details about the supervisor behaviour is given.

## Supervision Principles

A supervisor is responsible for starting, stopping, and monitoring its child
processes. The basic idea of a supervisor is that it is to keep its child
processes alive by restarting them when necessary.

Which child processes to start and monitor is specified by a list of
[child specifications](sup_princ.md#spec). The child processes are started in
the order specified by this list, and terminated in the reversed order.

## Example

The callback module for a supervisor starting the server from
[gen_server Behaviour](gen_server_concepts.md#ex) can look as follows:

[](){: #ex }

```erlang
-module(ch_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(ch_sup, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => ch3,
                    start => {ch3, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [ch3]}],
    {ok, {SupFlags, ChildSpecs}}.
```

The `SupFlags` variable in the return value from `init/1` represents the
[supervisor flags](sup_princ.md#flags).

The `ChildSpecs` variable in the return value from `init/1` is a list of
[child specifications](sup_princ.md#spec).

[](){: #flags }

## Supervisor Flags

This is the type definition for the supervisor flags:

```erlang
sup_flags() = #{strategy => strategy(),           % optional
                intensity => non_neg_integer(),   % optional
                period => pos_integer(),          % optional
                auto_shutdown => auto_shutdown()} % optional
    strategy() = one_for_all
               | one_for_one
               | rest_for_one
               | simple_one_for_one
    auto_shutdown() = never
                    | any_significant
                    | all_significant
```

- `strategy` specifies the [restart strategy](sup_princ.md#strategy).
- `intensity` and `period` specify the
  [maximum restart intensity](sup_princ.md#max_intensity).
- `auto_shutdown` specifies if and when a supervisor should
  [automatically shut itself down](sup_princ.md#automatic-shutdown).

[](){: #strategy }

## Restart Strategy

The restart strategy is specified by the `strategy` key in the supervisor flags
map returned by the callback function `init`:

```text
SupFlags = #{strategy => Strategy, ...}
```

The `strategy` key is optional in this map. If it is not given, it defaults to
`one_for_one`.

> #### Note {: .info }
>
> For simplicity, the diagrams shown in this section display a setup where all
> the depicted children are assumed to have a
> [restart type](sup_princ.md#restart) of `permanent`.

### one_for_one

If a child process terminates, only that process is restarted.

![One_For_One Supervision](assets/sup-one_for_one.svg "One_For_One Supervision"){:
#sup4 width=80% }

### one_for_all

If a child process terminates, all other child processes are terminated, and
then all child processes, including the terminated one, are restarted.

![One_For_All Supervision](assets/sup-one_for_all.svg "One_For_All Supervision"){:
#sup5 width=80% }

### rest_for_one

If a child process terminates, the rest of the child processes (that is, the
child processes after the terminated process in start order) are terminated.
Then the terminated child process and the rest of the child processes are
restarted.

![Rest_For_One Supervision](assets/sup-rest_for_one.svg "Rest_For_One Supervision"){:
#sup6 width=80% }

### simple_one_for_one

See [simple-one-for-one supervisors](sup_princ.md#simple).

[](){: #max_intensity }

## Maximum Restart Intensity

The supervisors have a built-in mechanism to limit the number of restarts which
can occur in a given time interval. This is specified by the two keys
`intensity` and `period` in the supervisor flags map returned by the callback
function `init`:

```text
SupFlags = #{intensity => MaxR, period => MaxT, ...}
```

If more than `MaxR` number of restarts occur in the last `MaxT` seconds, the
supervisor terminates all the child processes and then itself. The termination
reason for the supervisor itself in that case will be `shutdown`.

When the supervisor terminates, then the next higher-level supervisor takes some
action. It either restarts the terminated supervisor or terminates itself.

The intention of the restart mechanism is to prevent a situation where a process
repeatedly dies for the same reason, only to be restarted again.

The keys `intensity` and `period` are optional in the supervisor flags map. If
they are not given, they default to `1` and `5`, respectively.

### Tuning the intensity and period

The default values are 1 restart per 5 seconds. This was chosen to be safe for
most systems, even with deep supervision hierarchies, but you will probably want
to tune the settings for your particular use case.

First, the intensity decides how big bursts of restarts you want to tolerate.
For example, you might want to accept a burst of at most 5 or 10 attempts, even
within the same second, if it results in a successful restart.

Second, you need to consider the sustained failure rate, if crashes keep
happening but not often enough to make the supervisor give up. If you set
intensity to 10 and set the period as low as 1, the supervisor will allow child
processes to keep restarting up to 10 times per second, forever, filling your
logs with crash reports until someone intervenes manually.

You should therefore set the period to be long enough that you can accept that
the supervisor keeps going at that rate. For example, if you have picked an
intensity value of 5, then setting the period to 30 seconds will give you at
most one restart per 6 seconds for any longer period of time, which means that
your logs won't fill up too quickly, and you will have a chance to observe the
failures and apply a fix.

These choices depend a lot on your problem domain. If you don't have real time
monitoring and ability to fix problems quickly, for example in an embedded
system, you might want to accept at most one restart per minute before the
supervisor should give up and escalate to the next level to try to clear the
error automatically. On the other hand, if it is more important that you keep
trying even at a high failure rate, you might want a sustained rate of as much
as 1-2 restarts per second.

Avoiding common mistakes:

- Do not forget to consider the burst rate. If you set intensity to 1 and period
  to 6, it gives the same sustained error rate as 5/30 or 10/60, but will not
  allow even 2 restart attempts in quick succession. This is probably not what
  you wanted.
- Do not set the period to a very high value if you want to tolerate bursts. If
  you set intensity to 5 and period to 3600 (one hour), the supervisor will
  allow a short burst of 5 restarts, but then gives up if it sees another single
  restart almost an hour later. You probably want to regard those crashes as
  separate incidents, so setting the period to 5 or 10 minutes will be more
  reasonable.
- If your application has multiple levels of supervision, then do not simply set
  the restart intensities to the same values on all levels. Keep in mind that
  the total number of restarts (before the top level supervisor gives up and
  terminates the application) will be the product of the intensity values of all
  the supervisors above the failing child process.

  For example, if the top level allows 10 restarts, and the next level also
  allows 10, a crashing child below that level will be restarted 100 times,
  which is probably excessive. Allowing at most 3 restarts for the top level
  supervisor might be a better choice in this case.

## Automatic Shutdown

A supervisor can be configured to automatically shut itself down when
[significant children](sup_princ.md#significant_child) terminate.

This is useful when a supervisor represents a work unit of cooperating children,
as opposed to independent workers. When the work unit has finished its work,
that is, when any or all significant child processes have terminated, the
supervisor should then shut down by terminating all remaining child processes in
reverse start order according to the respective shutdown specifications, and
then itself.

Automatic shutdown is specified by the `auto_shutdown` key in the supervisor
flags map returned by the callback function `init`:

```text
SupFlags = #{auto_shutdown => AutoShutdown, ...}
```

The `auto_shutdown` key is optional in this map. If it is not given, it defaults
to `never`.

> #### Note {: .info }
>
> The automatic shutdown facility only applies when significant children
> terminate by themselves, that is, when their termination was not caused by
> means of the supervisor. Specifically, neither the termination of a child as a
> consequence of a sibling's death in the `one_for_all` or `rest_for_one`
> strategies nor the manual termination of a child by means of
> `supervisor:terminate_child/2` will trigger an automatic shutdown.

### never

Automatic shutdown is disabled.

In this mode, significant children are not accepted. If the child specs returned
from `init` contains significant children, the supervisor will refuse to start.
Attempts to start significant children dynamically will be rejected.

This is the default setting.

### any_significant

The supervisor will automatically shut itself down when _any_ significant child
terminates, that is, when a transient significant child terminates normally or
when a temporary significant child terminates normally or abnormally.

### all_significant

The supervisor will automatically shut itself down when _all_ significant
children have terminated, that is, when the _last active_ significant child
terminates. The same rules as for `any_significant` apply.

> #### Warning {: .warning }
>
> The automatic shutdown feature appeared in OTP 24.0, but applications using
> this feature will also compile and run with older OTP versions.
>
> However, such applications, when compiled with an OTP version that predates
> the appearance of the automatic shutdown feature, will leak processes because
> the automatic shutdowns they rely on will not happen.
>
> It is up to implementors to take proper precautions if they expect that their
> applications may be compiled with older OTP versions.

> #### Warning {: .warning }
>
> Top supervisors of [Applications](applications.md) should not be configured
> for automatic shutdown, because when the top supervisor exits, the application
> terminates. If the application is `permanent`, all other applications and the
> runtime system are terminated, also.

> #### Warning {: .warning }
>
> Supervisors configured for automatic shutdown should not be made
> [permanent](sup_princ.md#restart) children of their respective parent
> supervisors, as they would be restarted immediately after having automatically
> shut down, only to shut down automatically again after a while, and may thus
> exhaust the [Maximum Restart Intensity](sup_princ.md#max_intensity) of the
> parent supervisor.

[](){: #spec }

## Child Specification

The type definition for a child specification is as follows:

```erlang
child_spec() = #{id => child_id(),             % mandatory
                 start => mfargs(),            % mandatory
                 restart => restart(),         % optional
                 significant => significant(), % optional
                 shutdown => shutdown(),       % optional
                 type => worker(),             % optional
                 modules => modules()}         % optional
    child_id() = term()
    mfargs() = {M :: module(), F :: atom(), A :: [term()]}
    modules() = [module()] | dynamic
    restart() = permanent | transient | temporary
    significant() = boolean()
    shutdown() = brutal_kill | timeout()
    worker() = worker | supervisor
```

- `id` is used to identify the child specification internally by the supervisor.

  The `id` key is mandatory.

  Note that this identifier occasionally has been called "name". As far as
  possible, the terms "identifier" or "id" are now used but in order to keep
  backwards compatibility, some occurrences of "name" can still be found, for
  example in error messages.

- `start` defines the function call used to start the child process. It is a
  module-function-arguments tuple used as [`apply(M, F, A)`](`apply/3`).

  It is to be (or result in) a call to any of the following:

  - `supervisor:start_link`
  - `gen_server:start_link`
  - `gen_statem:start_link`
  - `gen_event:start_link`
  - A function compliant with these functions. For details, see the
    `m:supervisor` manual page.

  The `start` key is mandatory.

- [](){: #restart } `restart` defines when a terminated child process is to be
  restarted.

  - A `permanent` child process is always restarted.
  - A `temporary` child process is never restarted (not even when the supervisor
    restart strategy is `rest_for_one` or `one_for_all` and a sibling death
    causes the temporary process to be terminated).
  - A `transient` child process is restarted only if it terminates abnormally,
    that is, with an exit reason other than `normal`, `shutdown`, or
    `{shutdown,Term}`.

  The `restart` key is optional. If it is not given, the default value
  `permanent` will be used.

- [](){: #significant_child } `significant` defines if a child is considered
  significant for [automatic self-shutdown](sup_princ.md#automatic-shutdown) of
  the supervisor.

  It is invalid to set this option to `true` for a child with
  [restart type](sup_princ.md#restart) `permanent` or in a supervisor with
  [auto_shutdown](sup_princ.md#automatic-shutdown) set to `never`.

- [](){: #shutdown } `shutdown` defines how a child process is to be terminated.

  - `brutal_kill` means that the child process is unconditionally terminated
    using [`exit(Child, kill)`](`exit/2`).
  - An integer time-out value means that the supervisor tells the child process
    to terminate by calling [`exit(Child, shutdown)`](`exit/2`) and then waits
    for an exit signal back. If no exit signal is received within the specified
    time, the child process is unconditionally terminated using
    [`exit(Child, kill)`](`exit/2`).
  - If the child process is another supervisor, it should be set to `infinity`
    to give the subtree enough time to shut down. It is also allowed to set it
    to `infinity`, if the child process is a worker. See the warning below:

  > #### Warning {: .warning }
  >
  > Setting the shutdown time to anything other than `infinity` for a child of
  > type `supervisor` can cause a race condition where the child in question
  > unlinks its own children, but fails to terminate them before it is killed.
  >
  > Be careful when setting the shutdown time to `infinity` when the child
  > process is a worker. Because, in this situation, the termination of the
  > supervision tree depends on the child process; it must be implemented in a
  > safe way and its cleanup procedure must always return.

  The `shutdown` key is optional. If it is not given, and the child is of type
  `worker`, the default value `5000` will be used; if the child is of type
  `supervisor`, the default value `infinity` will be used.

- `type` specifies if the child process is a supervisor or a worker.

  The `type` key is optional. If it is not given, the default value `worker`
  will be used.

- `modules` are to be a list with one element `[Module]`, where `Module` is the
  name of the callback module, if the child process is a supervisor, gen_server,
  gen_statem. If the child process is a gen_event, the value shall be `dynamic`.

  This information is used by the release handler during upgrades and
  downgrades, see [Release Handling](release_handling.md).

  The `modules` key is optional. If it is not given, it defaults to `[M]`, where
  `M` comes from the child's start `{M,F,A}`.

_Example:_ The child specification to start the server `ch3` in the previous
example look as follows:

```erlang
#{id => ch3,
  start => {ch3, start_link, []},
  restart => permanent,
  shutdown => brutal_kill,
  type => worker,
  modules => [ch3]}
```

or simplified, relying on the default values:

```text
#{id => ch3,
  start => {ch3, start_link, []},
  shutdown => brutal_kill}
```

Example: A child specification to start the event manager from the chapter about
[gen_event](events.md#mgr):

```erlang
#{id => error_man,
  start => {gen_event, start_link, [{local, error_man}]},
  modules => dynamic}
```

Both server and event manager are registered processes which can be expected to
be always accessible. Thus they are specified to be `permanent`.

`ch3` does not need to do any cleaning up before termination. Thus, no shutdown
time is needed, but `brutal_kill` is sufficient. `error_man` can need some time
for the event handlers to clean up, thus the shutdown time is set to 5000 ms
(which is the default value).

Example: A child specification to start another supervisor:

```erlang
#{id => sup,
  start => {sup, start_link, []},
  restart => transient,
  type => supervisor} % will cause default shutdown=>infinity
```

[](){: #super_tree }

## Starting a Supervisor

In the previous example, the supervisor is started by calling
`ch_sup:start_link()`:

```erlang
start_link() ->
    supervisor:start_link(ch_sup, []).
```

`ch_sup:start_link` calls function `supervisor:start_link/2`, which spawns and
links to a new process, a supervisor.

- The first argument, `ch_sup`, is the name of the callback module, that is, the
  module where the `init` callback function is located.
- The second argument, `[]`, is a term that is passed as is to the callback
  function `init`. Here, `init` does not need any data and ignores the argument.

In this case, the supervisor is not registered. Instead its pid must be used. A
name can be specified by calling
`supervisor:start_link({local, Name}, Module, Args)` or
`supervisor:start_link({global, Name}, Module, Args)`.

The new supervisor process calls the callback function `ch_sup:init([])`. `init`
shall return `{ok, {SupFlags, ChildSpecs}}`:

```erlang
init(_Args) ->
    SupFlags = #{},
    ChildSpecs = [#{id => ch3,
                    start => {ch3, start_link, []},
                    shutdown => brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.
```

The supervisor then starts all its child processes according to the child
specifications in the start specification. In this case there is one child
process, `ch3`.

`supervisor:start_link` is synchronous. It does not return until all child
processes have been started.

## Adding a Child Process

In addition to the static supervision tree, dynamic child processes can be added
to an existing supervisor with the following call:

```text
supervisor:start_child(Sup, ChildSpec)
```

`Sup` is the pid, or name, of the supervisor. `ChildSpec` is a
[child specification](sup_princ.md#spec).

Child processes added using `start_child/2` behave in the same way as the other
child processes, with one important exception: if a supervisor dies and is
recreated, then all child processes that were dynamically added to the
supervisor are lost.

## Stopping a Child Process

Any child process, static or dynamic, can be stopped in accordance with the
shutdown specification:

```text
supervisor:terminate_child(Sup, Id)
```

Stopping a [significant child](sup_princ.md#significant_child) of a supervisor
configured for [automatic shutdown](sup_princ.md#automatic-shutdown) will not
trigger an automatic shutdown.

The child specification for a stopped child process is deleted with the
following call:

```text
supervisor:delete_child(Sup, Id)
```

`Sup` is the pid, or name, of the supervisor. `Id` is the value associated with
the `id` key in the [child specification](sup_princ.md#spec).

As with dynamically added child processes, the effects of deleting a static
child process are lost if the supervisor itself restarts.

[](){: #simple }

## Simplified one_for_one Supervisors

A supervisor with restart strategy `simple_one_for_one` is a simplified
`one_for_one` supervisor, where all child processes are dynamically added
instances of the same process.

The following is an example of a callback module for a `simple_one_for_one`
supervisor:

```erlang
-module(simple_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(simple_sup, []).

init(_Args) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => call,
                    start => {call, start_link, []},
                    shutdown => brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.
```

When started, the supervisor does not start any child processes. Instead, all
child processes are added dynamically by calling:

```text
supervisor:start_child(Sup, List)
```

`Sup` is the pid, or name, of the supervisor. `List` is an arbitrary list of
terms, which are added to the list of arguments specified in the child
specification. If the start function is specified as `{M, F, A}`, the child
process is started by calling [`apply(M, F, A++List)`](`apply/3`).

For example, adding a child to `simple_sup` above:

```text
supervisor:start_child(Pid, [id1])
```

The result is that the child process is started by calling
[`apply(call, start_link, []++[id1])`](`apply/3`), or actually:

```text
call:start_link(id1)
```

A child under a `simple_one_for_one` supervisor can be terminated with the
following:

```text
supervisor:terminate_child(Sup, Pid)
```

`Sup` is the pid, or name, of the supervisor and `Pid` is the pid of the child.

Because a `simple_one_for_one` supervisor can have many children, it shuts them
all down asynchronously. This means that the children will do their cleanup in
parallel and therefore the order in which they are stopped is not defined.

## Stopping

Since the supervisor is part of a supervision tree, it is automatically
terminated by its supervisor. When asked to shut down, it terminates all child
processes in reversed start order according to the respective shutdown
specifications, and then terminates itself.

If the supervisor is configured for
[automatic shutdown](sup_princ.md#automatic-shutdown) on termination of any or
all [significant children](sup_princ.md#significant_child), it will shut down
itself when any or the last active significant child terminates, respectively.
The shutdown itself follows the same procedure as described above, that is, the
supervisor terminates all remaining child processes in reversed start order, and
then terminates itself.

### Manual stopping versus Automatic Shutdown

For several reasons, a supervisor should not be stopped manually via
`supervisor:terminate_child/2` from a child located in its own tree.

1. The child process will have to know the pids or registered names not only of
   the supervisor it wants to stop, but also that of the supervisor's parent
   supervisor, in order to tell the parent supervisor to stop the supervisor it
   wants to stop. This can make restructuring a supervision tree difficult.
1. `supervisor:terminate_child/2` is a blocking call that will only return after
   the parent supervisor has finished the shutdown of the supervisor that should
   be stopped. Unless the call is made from a spawned process, this will result
   in a deadlock, as the supervisor waits for the child to exit as part of its
   shutdown procedure, whereas the child waits for the supervisor to shut down.
   If the child is trapping exits, this deadlock will last until the
   [shutdown](sup_princ.md#shutdown) timeout for the child expires.
1. When a supervisor is stopping a child, it will wait for the shutdown to
   complete before accepting other calls, that is, the supervisor will be
   unresponsive until then. If the termination takes some time to complete,
   especially when the considerations outlined in the previous point were not
   taken into account carefully, said supervisor might become unresponsive for a
   long time.

Instead, it is generally a better approach to rely on
[Automatic Shutdown](sup_princ.md#automatic-shutdown).

1. A child process does not need to know anything about its supervisor and its
   respective parent, not even that it is part of a supervision tree in the
   first place. It is instead only the supervisor which hosts the child who must
   know which of its children are [significant](sup_princ.md#significant_child)
   ones, and when to shut itself down.
1. A child process does not need to do anything special to shut down the work
   unit it is part of. All it needs to do is terminate normally when it has
   finished the task it was started for.
1. A supervisor that is automatically shutting itself down will perform the
   required shutdown steps fully independent of its parent supervisor. The
   parent supervisor will only notice that its child supervisor has terminated
   in the end. As the parent supervisor is not involved in the shutdown process,
   it will not be blocked.
