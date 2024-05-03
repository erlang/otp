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

`Common Test` is a portable application for automated testing. It is suitable
for:

- Black-box testing of target systems of any type (that is, not necessarily
  implemented in Erlang). This is performed through standard O&M interfaces
  (such as SNMP, HTTP, CORBA, and Telnet) and, if necessary, through
  user-specific interfaces (often called test ports).
- White-box testing of Erlang/OTP programs. This is easily done by calling the
  target API functions directly from the test case functions.

`Common Test` also integrates use of the OTP `m:cover` tool in application Tools
for code coverage analysis of Erlang/OTP programs.

`Common Test` executes test suite programs automatically, without operator
interaction. Test progress and results are printed to logs in HTML format,
easily browsed with a standard web browser. `Common Test` also sends
notifications about progress and results through an OTP event manager to event
handlers plugged in to the system. This way, users can integrate their own
programs for, for example, logging, database storing, or supervision with
`Common Test`.

`Common Test` provides libraries with useful support functions to fill various
testing needs and requirements. There is, for example, support for flexible test
declarations through test specifications. There is also support for central
configuration and control of multiple independent test sessions (to different
target systems) running in parallel.

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language.
