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
# SASL Error Logging

> #### Note {: .info }
>
> The SASL error logging concept described in this section is deprecated since
> Erlang/OTP 21.0, when the new [logging API](`e:kernel:logger_chapter.md`) was
> introduced.
>
> The new default behaviour is that the SASL application no longer affects which
> log events that are logged.
> [Supervisor reports](error_logging.md#supervisor_report) and
> [crash reports](error_logging.md#crash_report) are logged via the default
> logger handler which is setup by Kernel.
> [Progress reports](error_logging.md#progress_report) are by default not
> logged, but can be enabled by setting the primary log level to `info`, for
> example by using the Kernel configuration parameter
> [`logger_level`](`e:kernel:kernel_app.md#logger_level`).
>
> The old SASL error logging behaviour can be re-enabled by setting the Kernel
> configuration parameter
> [`logger_sasl_compatible`](`e:kernel:kernel_app.md#logger_sasl_compatible`) to
> `true`.
>
> The mechanism for
> [multi-file error report logging](error_logging.md#multi_file_logging) as
> described in this section is also kept for backwards compatibility. However,
> the new logging API also introduces `m:logger_disk_log_h`, which is a logger
> handler that can print to multiple files using `m:disk_log`.

## SASL reports

The SASL application introduces three types of reports:

- Supervisor report
- Progress report
- Crash report

When the SASL application is started, it adds a Logger handler that formats and
writes these reports, as specified in the
[configuration parameters for SASL](sasl_app.md#deprecated_error_logger_config).

[](){: #supervisor_report }

### Supervisor Report

A supervisor report is issued when a supervised child terminates unexpectedly. A
supervisor report contains the following items:

- **`Supervisor`** - Name of the reporting supervisor.

- **`Context`** - Indicates in which phase the child terminated from the
  supervisor's point of view. This can be `start_error`, `child_terminated`, or
  `shutdown_error`.

- **`Reason`** - Termination reason.

- **`Offender`** - Start specification for the child.

[](){: #progress_report }

### Progress Report

A progress report is issued when a supervisor starts or restarts a child. A
progress report contains the following items:

- **`Supervisor`** - Name of the reporting supervisor.

- **`Started`** - Start specification for the successfully started child.

[](){: #CRASH }

[](){: #crash_report }

### Crash Report

Processes started with functions [`proc_lib:spawn`](`proc_lib:spawn/1`) or
[`proc_lib:spawn_link`](`proc_lib:spawn_link/1`) are wrapped within a `catch`. A
crash report is issued when such a process terminates with an unexpected reason,
which is any reason other than `normal`, `shutdown`, or `{shutdown,Term}`.
Processes using behaviors `m:gen_server`, `m:gen_fsm` or `m:gen_statem` are
examples of such processes. A crash report contains the following items:

- **`Crasher`** - Information about the crashing process, such as initial
  function call, exit reason, and message queue.

- **`Neighbours`** - Information about processes that are linked to the crashing
  process and do not trap exits. These processes are the neighbours that
  terminate because of this process crash. The information gathered is the same
  as the information for Crasher, described in the previous item.

### Example

The following example shows the reports generated when a process crashes. The
example process is a `permanent` process supervised by the `test_sup`
supervisor. A division by zero is executed and the error is first reported by
the faulty process. A crash report is generated, as the process was started
using function `proc_lib:spawn/3`. The supervisor generates a supervisor report
showing the crashed process. A progress report is generated when the process is
finally restarted.

```text
        =ERROR REPORT==== 27-May-1996::13:38:56 ===
        <0.63.0>: Divide by zero !

        =CRASH REPORT==== 27-May-1996::13:38:56 ===
        crasher:
        pid: <0.63.0>
        registered_name: []
        error_info: {badarith,{test,s,[]}}
        initial_call: {test,s,[]}
        ancestors: [test_sup,<0.46.0>]
        messages: []
        links: [<0.47.0>]
        dictionary: []
        trap_exit: false
        status: running
        heap_size: 128
        stack_size: 128
        reductions: 348
        neighbours:

        =SUPERVISOR REPORT==== 27-May-1996::13:38:56 ===
        Supervisor: {local,test_sup}
        Context:    child_terminated
        Reason:     {badarith,{test,s,[]}}
        Offender:   [{pid,<0.63.0>},
        {name,test},
        {mfa,{test,t,[]}},
        {restart_type,permanent},
        {shutdown,200},
        {child_type,worker}]

        =PROGRESS REPORT==== 27-May-1996::13:38:56 ===
        Supervisor: {local,test_sup}
        Started:  [{pid,<0.64.0>},
        {name,test},
        {mfa,{test,t,[]}},
        {restart_type,permanent},
        {shutdown,200},
        {child_type,worker}]
```

[](){: #multi_file_logging }

## Multi-File Error Report Logging

Multi-file error report logging is used to store error messages received by
`error_logger`. The error messages are stored in several files and each file is
smaller than a specified number of kilobytes. No more than a specified number of
files exist at the same time. The logging is very fast, as each error message is
written as a binary term.

For more details, see the
[`sasl(6)`](sasl_app.md#deprecated_error_logger_config) application in the
Reference Manual.

## Report Browser

The report browser is used to browse and format error reports written by the
error logger handler `m:log_mf_h` defined in STDLIB.

The `log_mf_h` handler writes all reports to a report logging directory, which
is specified when configuring the SASL application.

If the report browser is used offline, the reports can be copied to another
directory specified when starting the browser. If no such directory is
specified, the browser reads reports from the SASL `error_logger_mf_dir`.

### Starting Report Browser

Start the `rb_server` with function [`rb:start([Options])`](`rb:start/1`) as
shown in the following example:

```text
        5> rb:start([{max, 20}]).
        rb: reading report...done.
        rb: reading report...done.
        rb: reading report...done.
        rb: reading report...done.
        {ok,<0.199.0>}
```

### Online Help

Enter command [`rb:help()`](`rb:help/0`) to access the report browser online
help system.

### List Reports in Server

Use function [`rb:list()`](`rb:list/0`) to list all loaded reports:

```text
        4> rb:list().
        No                Type          Process       Date     Time
        ==                ====          =======       ====     ====
        20            progress         <0.17.0> 1996-10-16 16:14:54
        19            progress         <0.14.0> 1996-10-16 16:14:55
        18               error         <0.15.0> 1996-10-16 16:15:02
        17            progress         <0.14.0> 1996-10-16 16:15:06
        16            progress         <0.38.0> 1996-10-16 16:15:12
        15            progress         <0.17.0> 1996-10-16 16:16:14
        14            progress         <0.17.0> 1996-10-16 16:16:14
        13            progress         <0.17.0> 1996-10-16 16:16:14
        12            progress         <0.14.0> 1996-10-16 16:16:14
        11               error         <0.17.0> 1996-10-16 16:16:21
        10               error         <0.17.0> 1996-10-16 16:16:21
        9        crash_report  release_handler 1996-10-16 16:16:21
        8   supervisor_report         <0.17.0> 1996-10-16 16:16:21
        7            progress         <0.17.0> 1996-10-16 16:16:21
        6            progress         <0.17.0> 1996-10-16 16:16:36
        5            progress         <0.17.0> 1996-10-16 16:16:36
        4            progress         <0.17.0> 1996-10-16 16:16:36
        3            progress         <0.14.0> 1996-10-16 16:16:36
        2               error         <0.15.0> 1996-10-16 16:17:04
        1            progress         <0.14.0> 1996-10-16 16:17:09
        ok
```

### Show Reports

Use function [`rb:show(Number)`](`rb:show/1`) to show details of a specific
report:

```erlang
7> rb:show(4).

PROGRESS REPORT  <0.20.0>                                   1996-10-16 16:16:36
===============================================================================
supervisor                                                     {local,sasl_sup}
started
[{pid,<0.24.0>},
{name,release_handler},
{mfa,{release_handler,start_link,[]}},
{restart_type,permanent},
{shutdown,2000},
{child_type,worker}]

ok
8> rb:show(9).

CRASH REPORT  <0.24.0>                                      1996-10-16 16:16:21
===============================================================================
Crashing process
pid                                                                 <0.24.0>
registered_name                                              release_handler
error_info                             {undef,{release_handler,mbj_func,[]}}
initial_call
{gen,init_it,
[gen_server,
<0.20.0>,
<0.20.0>,
{erlang,register},
release_handler,
release_handler,
[],
[]]}
ancestors                                                [sasl_sup,<0.18.0>]
messages                                                                  []
links                                                    [<0.23.0>,<0.20.0>]
dictionary                                                                []
trap_exit                                                              false
status                                                               running
heap_size                                                                610
stack_size                                                               142
reductions                                                                54

ok
```

### Search Reports

All reports containing a common pattern can be shown. Suppose a process crashes
because it tries to call a non-existing function `release_handler:mbj_func/1`.
The reports can then be shown as follows:

```text
12> rb:grep("mbj_func").
Found match in report number 11

ERROR REPORT  <0.24.0>                                      1996-10-16 16:16:21
===============================================================================

** undefined function: release_handler:mbj_func[] **
Found match in report number 10

ERROR REPORT  <0.24.0>                                      1996-10-16 16:16:21
===============================================================================

** Generic server release_handler terminating
** Last message in was {unpack_release,hej}
** When Server state == {state,[],
"/home/dup/otp2/otp_beam_sunos5_p1g_7",
[{release,
"OTP  APN 181 01",
"P1G",
undefined,
[],
permanent}],
undefined}
** Reason for termination ==
** {undef,{release_handler,mbj_func,[]}}
Found match in report number 9

CRASH REPORT  <0.24.0>                                      1996-10-16 16:16:21
===============================================================================
Crashing process
pid                                                                 <0.24.0>
registered_name                                              release_handler
error_info                             {undef,{release_handler,mbj_func,[]}}
initial_call
{gen,init_it,
[gen_server,
<0.20.0>,
<0.20.0>,
{erlang,register},
release_handler,
release_handler,
[],
[]]}
ancestors                                                [sasl_sup,<0.18.0>]
messages                                                                  []
links                                                    [<0.23.0>,<0.20.0>]
dictionary                                                                []
trap_exit                                                              false
status                                                               running
heap_size                                                                610
stack_size                                                               142
reductions                                                                54

Found match in report number 8

SUPERVISOR REPORT  <0.20.0>                                 1996-10-16 16:16:21
===============================================================================
Reporting supervisor                                           {local,sasl_sup}

Child process
errorContext                                                child_terminated
reason                                 {undef,{release_handler,mbj_func,[]}}
pid                                                                 <0.24.0>
name                                                         release_handler
start_function                               {release_handler,start_link,[]}
restart_type                                                       permanent
shutdown                                                                2000
child_type                                                            worker

ok
```

### Stop Server

Use function [`rb:stop()`](`rb:stop/0`) to stop the `rb_server`:

```text
13> rb:stop().
ok
```
