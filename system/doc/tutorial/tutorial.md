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

[](){: #interoperability-tutorial }

This section informs on interoperability, that is, information exchange, between
Erlang and other programming languages. The included examples mainly treat
interoperability between Erlang and C.

## Purpose

The purpose of this tutorial is to describe different interoperability
mechanisms that can be used when integrating a program written in Erlang with a
program written in another programming language, from the Erlang programmer's
perspective.

## Prerequisites

It is assumed that you are a skilled Erlang programmer, familiar with concepts
such as Erlang data types, processes, messages, and error handling.

To illustrate the interoperability principles, C programs running in a UNIX
environment have been used. It is assumed that you have enough knowledge to
apply these principles to the relevant programming languages and platforms.

> #### Note {: .info }
>
> For readability, the example code is kept as simple as possible. For example,
> it does not include error handling, which might be vital in a real-life
> system.
