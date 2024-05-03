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
# run_erl

Redirect Erlang input and output streams on Unix systems.

## Description

The `run_erl` program is specific to Unix systems. This program redirects the
standard input and standard output streams so that all output can be logged. It
also lets the program `to_erl` connect to the Erlang console, making it possible
to monitor and debug an embedded system remotely.

For more information about the use, see the
[Embedded System User's Guide](`e:system:embedded_solaris.md`) in System
Documentation.

```text
run_erl [-daemon] pipe_dir/ log_dir "exec command arg1 arg2 ..."
```

Arguments:

- **`-daemon`** - This option is highly recommended. It makes `run_erl` run in
  the background completely detached from any controlling terminal and the
  command returns to the caller immediately. Without this option, `run_erl` must
  be started using several tricks in the shell to detach it completely from the
  terminal in use when starting it. The option must be the first argument to
  `run_erl` on the command line.

- **`pipe_dir`** - The named pipe, usually `/tmp/`. It must be suffixed by a `/`
  (slash), that is, `/tmp/epipes/`, not `/tmp/epipes`.

- **`log_dir`** - The log files, that is:

  - One log file, `run_erl.log`, which logs progress and warnings from the
    `run_erl` program itself.
  - Up to five log files at maximum 100 KB each with the content of the standard
    streams from and to the command. (Both the number of logs and sizes can be
    changed by environment variables, see section
    [Environment Variables](run_erl_cmd.md#environment_variables) below.)

    When the logs are full, `run_erl` deletes and reuses the oldest log file.

- **`"exec command arg1 arg2 ..."`** - A space-separated string specifying the
  program to be executed. The second field is typically a command name such as
  `erl`.

## Notes concerning the Log Files

While running, `run_erl` sends all output, uninterpreted, to a log file. The
file is named `erlang.log.N`, where `N` is an integer. When the log is "full"
(default log size is 100 KB), `run_erl` starts to log in file
`erlang.log.(N+1)`, until `N` reaches a certain number (default 5), whereupon
`N` starts at 1 again and the oldest files start getting overwritten.

If no output comes from the Erlang shell, but the Erlang machine still seems to
be alive, an "ALIVE" message is written to the log; it is a time stamp and is
written, by default, after 15 minutes of inactivity. Also, if output from Erlang
is logged, but more than 5 minutes (default) has passed since last time we got
anything from Erlang, a time stamp is written in the log. The "ALIVE" messages
look as follows:

```text
===== ALIVE <date-time-string>
```

The other time stamps look as follows:

```text
===== <date-time-string>
```

`date-time-string` is the date and time the message is written, default in local
time (can be changed to UTC if needed). It is formatted with the ANSI-C function
`strftime` using the format string `%a %b %e %T %Z %Y`, which produces messages
like `===== ALIVE Thu May 15 10:13:36 MEST 2003`; this can be changed, see the
next section.

[](){: #environment_variables }

## Environment Variables

The following environment variables are recognized by `run_erl` and change the
logging behavior. For more information, see the previous section.

- **`RUN_ERL_LOG_ALIVE_MINUTES`** - How long to wait for output (in minutes)
  before writing an "ALIVE" message to the log. Defaults to 15, minimum is 1.

- **`RUN_ERL_LOG_ACTIVITY_MINUTES`** - How long Erlang needs to be inactive
  before output is preceded with a time stamp. Defaults to
  `RUN_ERL_LOG_ALIVE_MINUTES div 3`, minimum is 1.

- **`RUN_ERL_LOG_ALIVE_FORMAT`** - Specifies another format string to be used in
  the `strftime` C library call. That is, specifying this to `"%e-%b-%Y, %T %Z"`
  gives log messages with time stamps like `15-May-2003, 10:23:04 MET`. For more
  information, see the documentation for the C library function `strftime`.
  Defaults to `"%a %b %e %T %Z %Y"`.

- **`RUN_ERL_LOG_ALIVE_IN_UTC`** - If set to anything else than `0`, it makes
  all times displayed by `run_erl` to be in UTC (GMT, CET, MET, without Daylight
  Saving Time), rather than in local time. This does not affect data coming from
  Erlang, only the logs output directly by `run_erl`. Application SASL can be
  modified accordingly by setting the Erlang application variable `utc_log` to
  `true`.

- **`RUN_ERL_LOG_GENERATIONS`** - Controls the number of log files written
  before older files are reused. Defaults to 5, minimum is 2, maximum is 1000.

  Note that, as a way to indicate the newest file, `run_erl` will delete the
  oldest log file to maintain a "hole" in the file sequences. For example, if
  log files #1, #2, #4 and #5 exists, that means #2 is the latest and #4 is the
  oldest. You will therefore at most get one less log file than the value set by
  `RUN_ERL_LOG_GENERATIONS`.

- **`RUN_ERL_LOG_MAXSIZE`** - The size, in bytes, of a log file before switching
  to a new log file. Defaults to 100000, minimum is 1000, maximum is about 2^30.

- **`RUN_ERL_DISABLE_FLOWCNTRL`** - If defined, disables input and output flow
  control for the pty opend by `run_erl`. Useful if you want to remove any risk
  of accidentally blocking the flow control by using Ctrl-S (instead of Ctrl-D
  to detach), which can result in blocking of the entire Beam process, and in
  the case of running heart as supervisor even the heart process becomes blocked
  when writing log message to terminal, leaving the heart process unable to do
  its work.

## See Also

[`start(1)`](start_cmd.md), [`start_erl(1)`](start_erl_cmd.md)
