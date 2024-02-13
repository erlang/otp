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

Reltool is a release management tool. It analyses a given Erlang/OTP
installation and determines various dependencies between applications. The
`graphical` frontend depicts the dependencies and enables interactive
customization of a target system. The backend provides a `batch` interface for
generation of customized target systems.

## Scope and Purpose

This manual describes the Reltool application, as a component of the Erlang/Open
Telecom Platform development environment. It is assumed that the reader is
familiar with the Erlang Development Environment, which is described in a
separate User's Guide.

## Prerequisites

The following prerequisites are required for understanding the material in the
Reltool User's Guide:

- familiarity with Erlang/OTP system principles and Erlang/OTP design principles

The application requires Erlang/OTP release R13B02 or later.

## About This Manual

In addition to this introductory chapter, the Reltool User's Guide contains the
following chapters:

- Chapter 2: "Usage" describes the architecture and typical usage of the
  application.
- Chapter 3: "Examples" gives some usage examples

## Where to Find More Information

Refer to the following documentation for more information about Reltool and
about the Erlang/OTP development system:

- the Reference Manual of Reltool
- the Erlang/OTP `System Principles`
- the Erlang/OTP `Design Principles`
- Programming Erlang: Software for a Concurrent World (2007), Pragmatic
  Bookshelf, ISBN13: 9781934356005.
