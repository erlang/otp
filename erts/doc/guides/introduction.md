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

The Erlang Runtime System Application, ERTS, contains functionality necessary to
run the Erlang system.

> #### Note {: .info }
>
> By default, `ERTS` is only guaranteed to be compatible with other Erlang/OTP
> components from the same release as `ERTS` itself.
>
> For information on how to communicate with Erlang/OTP components from earlier
> releases, see the documentation of system flag [`+R`](erl_cmd.md#compat_rel)
> in [erl](erl_cmd.md).

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language.
