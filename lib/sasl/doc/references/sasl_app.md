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
# sasl

The SASL application

## Description

The SASL application provides the following services:

- `alarm_handler`
- `release_handler`
- `systools`

> #### Note {: .info }
>
> The SASL application in OTP has nothing to do with "Simple Authentication and
> Security Layer" (RFC 4422).

## Configuration

The following configuration parameters are defined for the SASL application. For
more information about configuration parameters, see
[`app(4)`](`e:kernel:app.md`) in Kernel.

All configuration parameters are optional.

- **`start_prg = string()`** - Specifies the program to be used when restarting
  the system during release installation. Default is `$OTPROOT/bin/start`.

- **`masters = [atom()]`** - Specifies the nodes used by this node to read/write
  release information. This parameter is ignored if parameter `client_directory`
  is not set.

- **`client_directory = string()`** - This parameter specifies the client
  directory at the master nodes. For details, see
  [Release Handling](`e:system:release_handling.md`) in _OTP Design Principles_.
  This parameter is ignored if parameter `masters` is not set.

- **`static_emulator = true | false`** - Indicates if the Erlang emulator is
  statically installed. A node with a static emulator cannot switch dynamically
  to a new emulator, as the executable files are written into memory statically.
  This parameter is ignored if parameters `masters` and `client_directory` are
  not set.

- **`releases_dir = string()`** - Indicates where the `releases` directory is
  located. The release handler writes all its files to this directory. If this
  parameter is not set, the OS environment parameter `RELDIR` is used. By
  default, this is `$OTPROOT/releases`.

[](){: #deprecated_error_logger_config }

## Deprecated Error Logger Event Handlers and Configuration

In Erlang/OTP 21.0, a new API for logging was added. The old `error_logger`
event manager, and event handlers running on this manager, still work, but they
are not used by default.

The error logger event handlers `sasl_report_tty_h` and `sasl_report_file_h`,
were earlier used for printing the so called SASL reports, i.e. _supervisor
reports_, _crash reports_, and _progress reports_. These reports are now also
printed by the default logger handler started by the Kernel application.
Progress reports are by default stopped by the primary log level, but can be
enabled by setting this level to `info`, for example by using the Kernel
configuration parameter [`logger_level`](`e:kernel:kernel_app.md#logger_level`).

If the old error logger event handlers are still desired, they must be added by
calling `error_logger:add_report_handler/1,2`.

- **`sasl_report_tty_h`** - Formats and writes _supervisor reports_, _crash
  reports_, and _progress reports_ to `stdio`. This error logger event handler
  uses
  [`error_logger_format_depth`](`e:kernel:kernel_app.md#deprecated-configuration-parameters`)
  in the Kernel application to limit how much detail is printed in crash and
  supervisor reports.

- **`sasl_report_file_h`** - Formats and writes _supervisor reports_, _crash
  report_, and _progress report_ to a single file. This error logger event
  handler uses
  [`error_logger_format_depth`](`e:kernel:kernel_app.md#deprecated-configuration-parameters`)
  in the Kernel application to limit the details printed in crash and supervisor
  reports.

A similar behaviour, but still using the new logger API, can be obtained by
setting the Kernel application environment variable
[`logger_sasl_compatible`](`e:kernel:kernel_app.md#logger_sasl_compatible`) to
`true`. This adds a second instance of the standard Logger handler, named
`sasl`, which only prints the SASL reports. No SASL reports are then printed by
the Kernel logger handler.

The `sasl` handler is configured according to the values of the following SASL
application environment variables.

- **`sasl_error_logger = Value`** - `Value` is one of the following:

  - **`tty`** - Installs `sasl_report_tty_h` in the error logger. This is the
    default option.

  - **`{file,FileName}`** - Installs `sasl_report_file_h` in the error logger.
    All reports go to file `FileName`, which is a string. The file is opened in
    `write` mode with encoding `utf8`.

  - **`{file,FileName,Modes}`** - Same as `{file,FileName}`, except that `Modes`
    allows you to specify the modes used for opening the `FileName` given to the
    `file:open/2` call. By default, the file is opened in `write` mode with
    encoding `utf8`. Use `[append]` to have the `FileName` open in append mode.
    A different encoding can also be specified. `FileName` is a string.

  - **`false`** - No SASL error logger handler is installed.

- **`errlog_type = error | progress | all`** - Restricts the error logging
  performed by the specified `sasl_error_logger` to error reports or progress
  reports, or both. Default is `all`.

- **`utc_log = true | false`{: #utc_log }** - If set to `true`, all dates in
  textual log outputs are displayed in Universal Coordinated Time with the
  string `UTC` appended.

The error logger event handler `log_mf_h` can also still be used. This event
handler writes _all_ events sent to the error logger to disk. Multiple files and
log rotation are used. For efficiency reasons, each event is written as a
binary. For more information about this handler, see
[the STDLIB Reference Manual](`m:log_mf_h`).

To activate this event handler, three SASL configuration parameters must be set:

- **`error_logger_mf_dir = string() | false`** - Specifies in which directory
  `log_mf_h` is to store its files. If this parameter is undefined or `false`,
  the `log_mf_h` handler is not installed.

- **`error_logger_mf_maxbytes = integer()`** - Specifies the maximum size of
  each individual file written by `log_mf_h`. If this parameter is undefined,
  the `log_mf_h` handler is not installed.

- **`error_logger_mf_maxfiles = 0<integer()<256`** - Specifies the number of
  files used by `log_mf_h`. If this parameter is undefined, the `log_mf_h`
  handler is not installed.

The new `m:logger_disk_log_h` might be an alternative to `log_mf_h` if log
rotation is desired. This does, however, write the log events in clear text and
not as binaries.

## See Also

`m:alarm_handler`, `m:error_logger`, `m:logger`, `m:log_mf_h`, `m:rb`,
`m:release_handler`, `m:systools`
