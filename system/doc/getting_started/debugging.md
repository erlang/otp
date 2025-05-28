<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2023-2025. All Rights Reserved.

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
# Debugging

At some point you will be writing code that does not work as expected. This is a
normal part of the development process. The following sections provide some
tips on how to debug your code.

## Tracing
The tracing system is a powerful tool for debugging. It allows you to trace
the execution of your code and see what is happening at each step, with minimal
effort from the developer. You can use the tracing system to see what functions
are being called, what arguments are being passed, and what values are being
returned. You can also use the tracing system to see what messages are being
sent and received. See [Tracing in Erlang with dbg](`e:runtime_tools:dbg_guide.md`).

## Debugger
[Debugger](`m:debugger`) is a graphical user interface for the Erlang interpreter, which can be
used for debugging and testing of Erlang programs. For example, breakpoints can
be set, code can be single-stepped, and variable values can be displayed and
changed. Modules that will be debugged should be compiled with `debug_info`.
See [Debugger for Erlang](`e:debugger:introduction.md`).

## Print debugging
While `m:dbg` offers powerful and fine-grained tracing capabilities, sometimes a
simpler "printf debugging" approach is sufficient.
This involves strategically inserting `io:format/2` or `io:format/3` statements
into your code to observe variable values or execution flow.
Remember to clean up your `io:format` statements once debugging is complete, or
transition them to `m:logger` calls if the information is valuable for ongoing
diagnostics.