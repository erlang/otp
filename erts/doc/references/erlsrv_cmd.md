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
# erlsrv

Run the Erlang emulator as a service on Windows

## Description

This utility is specific to Windows NT/2000/XP (and later versions of Windows).
It allows Erlang emulators to run as services on the Windows system, allowing
embedded systems to start without any user needing to log on. The emulator
started in this way can be manipulated through the Windows services applet in a
manner similar to other services.

Notice that `erlsrv` is not a general service utility for Windows, but designed
for embedded Erlang systems.

`erlsrv` also provides a command-line interface for registering, changing,
starting, and stopping services.

To manipulate services, the logged on user is to have administrator privileges
on the machine. The Erlang machine itself is (default) run as the local
administrator. This can be changed with the Services applet in Windows.

The processes created by the service can, as opposed to normal services, be
"killed" with the task manager. Killing an emulator that is started by a service
triggers the "OnFail" action specified for that service, which can be a reboot.

The following parameters can be specified for each Erlang service:

- **`StopAction`** - Tells `erlsrv` how to stop the Erlang emulator. Default is
  to kill it (Win32 TerminateProcess), but this action can specify any Erlang
  shell command that will be executed in the emulator to make it stop. The
  emulator is expected to stop within 30 seconds after the command is issued in
  the shell. If the emulator is not stopped, it reports a running state to the
  service manager.

- **`OnFail`** - Can be one of the following:

  - **`reboot`** - The Windows system is rebooted whenever the emulator stops (a
    more simple form of watchdog). This can be useful for less critical systems,
    otherwise use the heart functionality to accomplish this.

  - **`restart`** - Makes the Erlang emulator be restarted (with whatever
    parameters are registered for the service at the occasion) when it stops. If
    the emulator stops again within 10 seconds, it is not restarted to avoid an
    infinite loop, which could hang the Windows system.

  - **`restart_always`** - Similar to `restart`, but does not try to detect
    cyclic restarts; it is expected that some other mechanism is present to
    avoid the problem.

  - **`ignore` (the default)** - Reports the service as stopped to the service
    manager whenever it fails; it must be manually restarted.

  On a system where release handling is used, this is always to be set to
  `ignore`. Use `heart` to restart the service on failure instead.

- **`Machine`** - The location of the Erlang emulator. The default is the
  `erl.exe` located in the same directory as `erlsrv.exe`.

  If the system uses release handling, this is to be set to a program similar to
  `start_erl.exe`.

- **`Env`** - Specifies an _extra_ environment for the emulator. The environment
  variables specified here are added to the system-wide environment block that
  is normally present when a service starts up. Variables present in both the
  system-wide environment and in the service environment specification will be
  set to the value specified in the service.

- **`WorkDir`** - The working directory for the Erlang emulator. Must be on a
  local drive (no network drives are mounted when a service starts). Default
  working directory for services is `%SystemDrive%%SystemPath%`. Debug log files
  will be placed in this directory.

- **`Priority`** - The process priority of the emulator. Can be one of the
  following:

  - **`realtime`** - Not recommended, as the machine will possibly be
    inaccessible to interactive users.

  - **`high`** - Can be used if two Erlang nodes are to reside on one dedicated
    system and one is to have precedence over the other.

  - **`low`** - Can be used if interactive performance is not to be affected by
    the emulator process.

  - **`default` (the default>**

- **`SName or Name`** - Specifies the short or long node name of the Erlang
  emulator. The Erlang services are always distributed. Default is to use the
  service name as (short) nodename.

- **`DebugType`** - Specifies that output from the Erlang shell is to be sent to
  a "debug log". The log file is named <servicename>`.debug` or
  <servicename>`.debug.`<N>, where <N> is an integer from 1 through 99. The log
  file is placed in the working directory of the service (as specified in
  `WorkDir`).

  Can be one of the following:

  - **`new`** - Uses a separate log file for every invocation of the service
    (<servicename>`.debug.`<N>).

  - **`reuse`** - Reuses the same log file (<servicename>`.debug`).

  - **`console`** - Opens an interactive Windows console window for the Erlang
    shell of the service. Automatically disables the `StopAction`. A service
    started with an interactive console window does not survive logouts.
    `OnFail` actions do not work with debug consoles either.

  - **`none` (the default)** - The output of the Erlang shell is discarded.

  > #### Note {: .info }
  >
  > The `console` option is _not_ intended for production. It is _only_ a
  > convenient way to debug Erlang services during development.
  >
  > The `new` and `reuse` options might seem convenient in a production system,
  > but consider that the logs grow indefinitely during the system lifetime and
  > cannot be truncated, except if the service is restarted.
  >
  > In short, the `DebugType` is intended for debugging only. Logs during
  > production are better produced with the standard Erlang logging facilities.

- **`Args`** - Passes extra arguments to the emulator startup program `erl.exe`
  (or `start_erl.exe`). Arguments that cannot be specified here are `-noinput`
  (`StopActions` would not work), `-name`, and `-sname` (they are specified in
  any way). The most common use is for specifying cookies and flags to be passed
  to `m:init` (`-s`).

- **`InternalServiceName`** - Specifies the Windows-internal service name (not
  the display name, which is the one `erlsrv` uses to identify the service).

  This internal name cannot be changed, it is fixed even if the service is
  renamed. `erlsrv` generates a unique internal name when a service is created.
  It is recommended to keep to the default if release handling is to be used for
  the application.

  The internal service name can be seen in the Windows service manager if
  viewing `Properties` for an Erlang service.

- **`Comment`** - A textual comment describing the service. Not mandatory, but
  shows up as the service description in the Windows service manager.

[](){: #001 } The naming of the service in a system that uses release handling
must follow the convention _NodeName_\__Release_, where _NodeName_ is the first
part of the Erlang node name (up to, but not including the "@") and _Release_ is
the current release of the application.

## erlsrv \{set | add\} <service-name> \[<service options>]

The `set` and `add` commands modifies or adds an Erlang service, respectively.
The simplest form of an `add` command is without any options in which case all
default values (described above) apply. The service name is mandatory.

Every option can be specified without parameters, the default value is then
applied. Values to the options are supplied _only_ when the default is not to be
used. For example, `erlsrv set myservice -prio -arg` sets the default priority
and removes all arguments.

Service options:

- **`-st[opaction] [<erlang shell command>]`** - Defines the `StopAction`, the
  command given to the Erlang shell when the service is stopped. Default is
  none.

- **`-on[fail] [{reboot | restart | restart_always}]`** - The action to take
  when the Erlang emulator stops unexpectedly. Default is to ignore.

- **`-m[achine] [<erl-command>]`** - The complete path to the Erlang emulator.
  Never use the `werl` program for this. Defaults to the `erl.exe` in the same
  directory as `erlsrv.exe`. When release handling is used, this is to be set to
  a program similar to `start_erl.exe`.

- **`-e[nv] [<variable>[=<value>]] ...`** - Edits the environment block for the
  service. Every environment variable specified is added to the system
  environment block. If a variable specified here has the same name as a
  system-wide environment variable, the specified value overrides the
  system-wide. Environment variables are added to this list by specifying
  <variable>=<value> and deleted from the list by specifying <variable> alone.
  The environment block is automatically sorted. Any number of `-env` options
  can be specified in one command. Default is to use the system environment
  block unmodified (except for two additions, see section
  [Environment](erlsrv_cmd.md#002) below).

- **`-w[orkdir] [<directory>]`** - The initial working directory of the Erlang
  emulator. Defaults to the system directory.

- **`-p[riority] [{low|high|realtime}]`** - The priority of the Erlang emulator.
  Default to the Windows default priority.

- **`{-sn[ame] | -n[ame]} [<node-name>]`** - The node name of the Erlang
  machine. Distribution is mandatory. Defaults to `-sname <service name>`.

- **`-d[ebugtype] [{new|reuse|console}]`** - Specifies where shell output is to
  be sent. Default is that shell output is discarded. To be used only for
  debugging.

- **`-ar[gs] [<limited erl arguments>]`** - Extra arguments to the Erlang
  emulator. Avoid `-noinput`, `-noshell`, and `-sname`/`-name`. Default is no
  extra arguments. Remember that the services cookie file is not necessarily the
  same as the interactive users. The service runs as the local administrator.
  Specify all arguments together in one string, use double quotes (") to specify
  an argument string containing spaces, and use quoted quotes (\\") to specify a
  quote within the argument string if necessary.

- **`-i[nternalservicename] [<internal name>]`** - _Only_ allowed for `add`.
  Specifies a Windows-internal service name for the service, which by default is
  set to something unique (prefixed with the original service name) by `erlsrv`
  when adding a new service. Specifying this is a purely cosmethic action and is
  _not_ recommended if release handling is to be performed. The internal service
  name cannot be changed once the service is created. The internal name is _not_
  to be confused with the ordinary service name, which is the name used to
  identify a service to `erlsrv`.

- **`-c[omment] [<short description>]`** - Specifies a textual comment
  describing the service. This comment shows up as the service description in
  the Windows service manager.

## erlsrv \{start | start_disabled | stop | disable | enable\} <service-name>

These commands are only added for convenience, the normal way to manipulate the
state of a service is through the control panels services applet.

The `start` and `stop` commands communicates with the service manager for
starting and stopping a service. The commands wait until the service is started
or stopped. When disabling a service, it is not stopped, the disabled state does
not take effect until the service is stopped. Enabling a service sets it in
automatic mode, which is started at boot. This command cannot set the service to
manual.

The `start_disabled` command operates on a service regardless of if it is
enabled/disabled or started/stopped. It does this by first enabling it
(regardless of if it is enabled or not), then starting it (if not already
started), and then disabling it. The result is a disabled but started service,
regardless of its earlier state. This is useful for starting services
temporarily during a release upgrade. The difference between using
`start_disabled` and the sequence `enable`, `start`, and `disable` is that all
other `erlsrv` commands are locked out during the sequence of operations in
`start_disable`, making the operation atomic from an `erlsrv` user's point of
view.

## erlsrv remove <service-name>

Removes the service completely with all its registered options. It is stopped
before it is removed.

## erlsrv list \[<service-name>]

If no service name is specified, a brief listing of all Erlang services is
presented. If a service name is supplied, all options for that service are
presented.

## erlsrv help

Displays a brief help text.

## Environment

[](){: #002 } The environment of an Erlang machine started as a service contains
two special variables:

- **`ERLSRV_SERVICE_NAME`** - The name of the service that started the machine.

- **`ERLSRV_EXECUTABLE`** - The full path to the `erlsrv.exe`, which can be used
  to manipulate the service. This comes in handy when defining a heart command
  for your service.

A command file for restarting a service looks as follows:

```text
@echo off
%ERLSRV_EXECUTABLE% stop %ERLSRV_SERVICE_NAME%
%ERLSRV_EXECUTABLE% start %ERLSRV_SERVICE_NAME%
```

This command file is then set as heart command.

The environment variables can also be used to detect that we are running as a
service and make port programs react correctly to the control events generated
on logout (see the next section).

## Port Programs

When a program runs in the service context, it must handle the control events
that are sent to every program in the system when the interactive user logs off.
This is done in different ways for programs running in the console subsystem and
programs running as window applications. An application running in the console
subsystem (normal for port programs) uses the win32 function
`SetConsoleCtrlHandler` to register a control handler that returns `true` in
answer to the `CTRL_LOGOFF_EVENT` and `CTRL_SHUTDOWN_EVENT` events. Other
applications only forward `WM_ENDSESSION` and `WM_QUERYENDSESSION` to the
default window procedure.

A brief example in C of how to set the console control handler:

```c
#include <windows.h>
/*
** A Console control handler that ignores the log off events,
** and lets the default handler take care of other events.
*/
BOOL WINAPI service_aware_handler(DWORD ctrl){
    if(ctrl == CTRL_LOGOFF_EVENT)
        return TRUE;
    if(ctrl == CTRL_SHUTDOWN_EVENT)
        return TRUE;
    return FALSE;
}

void initialize_handler(void){
    char buffer[2];
    /*
     * We assume we are running as a service if this
     * environment variable is defined.
     */
    if(GetEnvironmentVariable("ERLSRV_SERVICE_NAME",buffer,
                              (DWORD) 2)){
        /*
        ** Actually set the control handler
        */
        SetConsoleCtrlHandler(&service_aware_handler, TRUE);
    }
}
```

## Notes

Although the options are described in a Unix-like format, the case of the
options or commands is not relevant, and both character "/" and "-" can be used
for options.

Notice that the program resides in the emulator's `bin` directory, not in the
`bin` directory directly under the Erlang root. The reasons for this are the
subtle problem of upgrading the emulator on a running system, where a new
version of the runtime system should not need to overwrite existing (and
probably used) executables.

To manipulate the Erlang services easily, put the
`<erlang_root>\erts-<version>\bin` directory in the path instead of
`<erlang_root>\bin`. The `erlsrv` program can be found from inside Erlang by
using the `os:find_executable/1` Erlang function.

For release handling to work, use `start_erl` as the Erlang machine. As stated
[above](erlsrv_cmd.md#001), the service name is significant.

## See Also

[`start_erl(1)`](start_erl_cmd.md), `m:release_handler`
