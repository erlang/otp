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

## Purpose

> "Premature optimization is the root of all evil" (D.E. Knuth)

Efficient code can be well-structured and clean, based on a sound overall
architecture and sound algorithms. Efficient code can be highly
implementation-code that bypasses documented interfaces and takes advantage of
obscure quirks in the current implementation.

Ideally, your code only contains the first type of efficient code. If that turns
out to be too slow, profile the application to find out where the performance
bottlenecks are and optimize only the bottlenecks. Let other code stay as clean
as possible.

This Efficiency Guide cannot really teach you how to write efficient code. It
can give you a few pointers about what to avoid and what to use, and some
understanding of how certain language features are implemented. This guide does
not include general tips about optimization that works in any language, such as
moving common calculations out of loops.

## Prerequisites

It is assumed that you are familiar with the Erlang programming language and the
OTP concepts.
