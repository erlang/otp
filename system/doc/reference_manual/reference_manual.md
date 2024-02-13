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

[](){: #erlang-ref-manual }

This section is the Erlang reference manual. It describes the Erlang programming
language.

## Purpose

The focus of the Erlang reference manual is on the language itself, not the
implementation of it. The language constructs are described in text and with
examples rather than formally specified. This is to make the manual more
readable. The Erlang reference manual is not intended as a tutorial.

Information about implementation of Erlang can, for example, be found, in the
following:

- [System Principles](`e:system:system_principles.md`)

  Starting and stopping, boot scripts, code loading,
  [logging](`e:system:error_logging.md`),
  [creating target systems](`e:system:create_target.md`)

- [Efficiency Guide](`e:system:advanced.md`)

  Memory consumption, system limits

- ERTS User's Guide

  [Crash dumps](`e:erts:crash_dump.md`), [drivers](`e:erts:driver.md`)

## Prerequisites

It is assumed that the reader has done some programming and is familiar with
concepts such as data types and programming language syntax.

## Document Conventions

In this section, the following terminology is used:

- A _sequence_ is one or more items. For example, a clause body consists of a
  sequence of expressions. This means that there must be at least one
  expression.
- A _list_ is any number of items. For example, an argument list can consist of
  zero, one, or more arguments.

If a feature has been added in R13A or later, this is mentioned in the text.

## Complete List of BIFs

For a complete list of BIFs, their arguments and return values, see `m:erlang`
manual page in ERTS.

## Reserved Words

The following are reserved words in Erlang:

`after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let maybe not of or orelse receive rem try when xor`

**Note**: `cond` and `let`, while reserved, are currently not used by the
language.

> #### Change {: .info }
>
> `maybe` is a reserved word only if feature `maybe_expr` is enabled. In
> Erlang/OTP 25 and 26, `maybe_expr` is disabled by default. Starting from
> Erlang/OTP 27, `maybe_expr` is enabled by default.
