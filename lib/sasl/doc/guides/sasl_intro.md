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

The SASL application provides support for:

- Error logging
- Alarm handling
- Release handling
- Report browsing

Section [SASL Error Logging](error_logging.md) describes the error handler that
produces the supervisor, progress, and crash reports, which can be written to
screen or to a specified file. It also describes the Report Browser (RB).

The sections about release structure and release handling have been moved to
section [OTP Design Principles](`e:system:index.html`) in _System
Documentation_.

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language.
