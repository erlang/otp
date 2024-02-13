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
# Error Logging

[](){: #error-logging }

## Error Information From the Runtime System

Error information from the runtime system, that is, information about a process
terminating because of an uncaught error exception, is by default written to
terminal (tty):

```text
=ERROR REPORT==== 9-Dec-2003::13:25:02 ===
Error in process <0.27.0> with exit value: {{badmatch,[1,2,3]},[{m,f,1},{shell,eval_loop,2}]}
```

The error information is handled by Logger, which is part of the Kernel
application.

The exit reasons (such as `badarg`) used by the runtime system are described in
[Errors and Error Handling](`e:system:errors.md#exit_reasons`).

For information about Logger and its user interface, see the `m:logger` manual
page and the [Logging](`e:kernel:logger_chapter.md`) section in the Kernel
User's Guide. The system can be configured so that log events are written to
file or to tty, or both. In addition, user-defined applications can send and
format log events using Logger.

## Log events from OTP behaviours

The standard behaviours (`supervisor`, `gen_server`, and so on) send progress
and error information to Logger. Progress reports are by default not logged, but
can be enabled by setting the primary log level to `info`, for example by using
the Kernel configuration parameter `logger_level`. Supervisor reports, crash
reports and other error and information reports are by default logged through
the log handler which is set up when the Kernel application is started.

Prior to Erlang/OTP 21.0, supervisor, crash, and progress reports were only
logged when the SASL application was running. This behaviour can, for backwards
compatibility, be enabled by setting the Kernel configuration parameter
[`logger_sasl_compatible`](`e:kernel:kernel_app.md#logger_sasl_compatible`) to
`true`. For more information, see
[SASL Error Logging](`e:sasl:error_logging.md`) in the SASL User's Guide.

```erlang
% erl -kernel logger_level info
Erlang/OTP 21 [erts-10.0] [source-13c50db] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

=PROGRESS REPORT==== 8-Jun-2018::16:54:19.916404 ===
    application: kernel
    started_at: nonode@nohost
=PROGRESS REPORT==== 8-Jun-2018::16:54:19.922908 ===
    application: stdlib
    started_at: nonode@nohost
=PROGRESS REPORT==== 8-Jun-2018::16:54:19.925755 ===
    supervisor: {local,kernel_safe_sup}
    started: [{pid,<0.74.0>},
              {id,disk_log_sup},
              {mfargs,{disk_log_sup,start_link,[]}},
              {restart_type,permanent},
              {shutdown,1000},
              {child_type,supervisor}]
=PROGRESS REPORT==== 8-Jun-2018::16:54:19.926056 ===
    supervisor: {local,kernel_safe_sup}
    started: [{pid,<0.75.0>},
              {id,disk_log_server},
              {mfargs,{disk_log_server,start_link,[]}},
              {restart_type,permanent},
              {shutdown,2000},
              {child_type,worker}]
Eshell V10.0  (abort with ^G)
1>
```
