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
# Installation

[](){: #general }

## General Information

The two main interfaces for running tests with `Common Test` are an executable
program named [`ct_run`](ct_run_cmd.md) and the Erlang module `m:ct`. `ct_run`
is compiled for the underlying operating system (for example, Unix/Linux or
Windows) during the build of the Erlang/OTP system, and is installed
automatically with other executable programs in the top level `bin` directory of
Erlang/OTP. The `ct` interface functions can be called from the Erlang shell, or
from any Erlang function, on any supported platform.

The `Common Test` application is installed with the Erlang/OTP system. No extra
installation step is required to start using `Common Test` through the `ct_run`
executable program, and/or the interface functions in the `ct` module.
