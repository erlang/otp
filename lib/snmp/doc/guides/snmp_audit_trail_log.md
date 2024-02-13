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
# Audit Trail Log

The chapter _Audit Trail Log_ describes the audit trail logging.

Both the agent and the manager can be configured to log incoming and outgoing
messages. It uses the Erlang standard log mechanism `disk_log` for logging. The
size and location of the log files are configurable. A wrap log is used, which
means that when the log has grown to a maximum size, it starts from the
beginning of the log, overwriting existing log records.

The log can be either a `read`, `write` or a `read_write`.

## Agent Logging

For the agent, a `write`, means that all `set` requests and their responses are
stored. No `get` requests or traps are stored in a `write`. A `read_write`, all
requests, responses and traps are stored.

The log uses a raw data format (basically the BER encoded message), in order to
minimize the CPU load needed for the log mechanism. This means that the log is
not human readable, but needs to be formatted off-line before it can be read.
Use the function [snmpa:log_to_txt](`m:snmpa#log_to_txt`) for this purpose.

## Manager Logging

For the manager, a `write`, means that all requests (`set` and `get`) and their
responses are stored. No traps are stored in a `write`. A `read_write`, all
requests, responses and traps are stored.

The log uses a raw data format (basically the BER encoded message), in order to
minimize the CPU load needed for the log mechanism. This means that the log is
not human readable, but needs to be formatted off-line before it can be read.
Use the function [snmpm:log_to_txt](`m:snmpm#log_to_txt`) for this purpose.
