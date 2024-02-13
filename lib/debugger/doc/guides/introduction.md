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
# Introduction

## Scope

Debugger is a graphical user interface for the Erlang interpreter, which can be
used for debugging and testing of Erlang programs. For example, breakpoints can
be set, code can be single-stepped and variable values can be displayed and
changed.

The Erlang interpreter can also be accessed through the interface module
`m:int`.

> #### Warning {: .warning }
>
> Debugger might at some point start tracing on the processes that execute the
> interpreted code. This means that a conflict occurs if tracing by other means
> is started on any of these processes.

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language.

Modules to be debugged must include debug information, for example,
`erlc +debug_info MODULE.erl`.
