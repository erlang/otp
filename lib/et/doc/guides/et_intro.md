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

The `Event Tracer (ET)` uses the built-in trace mechanism in Erlang and provides
tools for collection and graphical viewing of trace data.

The viewed trace data is normally collected from Erlang trace ports or files.

## Scope and Purpose

This manual describes the `Event Tracer (ET)` application, as a component of the
Erlang/Open Telecom Platform development environment. It is assumed that the
reader is familiar with the Erlang Development Environment, which is described
in a separate User's Guide.

## Prerequisites

The following prerequisites are required for understanding the material in the
`Event Tracer (ET)` User's Guide:

- familiarity with the Erlang system and Erlang programming in general and the
  especially the art of Erlang tracing.

The application requires Erlang/OTP release R13BB or later. If you use the old
`GS` based GUI it does suffice with R7B.

## About This Manual

In addition to this introductory chapter, the `Event Tracers` User's Guide
contains the following chapters:

- Chapter 2: "Tutorial" provides a walk-through of the various parts of the
  application. The tutorial is based on `Jayson Vantuyl's` article
  `http://souja.net/2009/04/making-sense-of-erlangs-event-tracer.html`.
- Chapter 3: "Description" describes the architecture and typical usage of the
  application.
- Chapter 4: "Advanced examples" gives some usage examples

## Where to Find More Information

Refer to the following documentation for more information about
`Event Tracer (ET)` and about the Erlang/OTP development system:

- the Reference Manual of the `Event Tracer (ET)`.
- documentation of basic tracing in `erlang:trace/3` and
  `erlang:trace_pattern/3` and then the utilities derived from these: `dbg`,
  `observer`, `invisio` and `et`.
- Programming Erlang: Software for a Concurrent World by Joe Armstrong; ISBN:
  978-1-93435-600-5
