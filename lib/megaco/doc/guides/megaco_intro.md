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

Megaco/H.248 is a protocol for control of elements in a physically decomposed
multimedia gateway, enabling separation of call control from media conversion. A
Media Gateway Controller (MGC) controls one or more Media Gateways (MG).

This version of the stack supports version 1, 2 and 3 as defined by:

- version 1 - RFC 3525 and H.248-IG (v10-v13)
- version 2 - draft-ietf-megaco-h248v2-04 & H.248.1 v2 Corrigendum 1 (03/2004)
- version 3 - Full version 3 as defined by ITU H.248.1 (09/2005) (including
  segments)

The semantics of the protocol has jointly been defined by two standardization
bodies:

- IETF - which calls the protocol Megaco
- ITU - which calls the protocol H.248

## Scope and Purpose

This manual describes the Megaco application, as a component of the Erlang/Open
Telecom Platform development environment. It is assumed that the reader is
familiar with the Erlang Development Environment, which is described in a
separate User's Guide.

## Prerequisites

The following prerequisites are required for understanding the material in the
Megaco User's Guide:

- the basics of the Megaco/H.248 protocol
- the basics of the Abstract Syntax Notation One (ASN.1)
- familiarity with the Erlang system and Erlang programming

The application requires Erlang/OTP release R10B or later.

## About This Manual

In addition to this introductory chapter, the Megaco User's Guide contains the
following chapters:

- Chapter 2: "Architecture" describes the architecture and typical usage of the
  application.
- Chapter 3: "Internal form and its encodings" describes the internal form of
  Megaco/H.248 messages and its various encodings.
- Chapter 4: "Transport mechanisms" describes how different mechanisms can be
  used to transport the Megaco/H.248 messages.
- Chapter 5: "Debugging" describes tracing and debugging.

## Where to Find More Information

Refer to the following documentation for more information about Megaco/H.248 and
about the Erlang/OTP development system:

- [version 1, RFC 3525](https://www.erlang.org/doc/standard/rfc3525.txt)
- [old version 1, RFC 3015](http://www.ietf.org/rfc/rfc3015.txt)
- [Version 2 Corrigendum 1](https://web.archive.org/web/20100704020645/http://www.erlang.org/project/megaco/standard/H.248.1-Corr1-200403.doc)
- [version 2, draft-ietf-megaco-h248v2-04](https://web.archive.org/web/20100620185420/http://erlang.org/project/megaco/standard/draft-ietf-megaco-h248v2-04.txt)
- [H.248.1 version 3](http://www.itu.int/)
- the ASN.1 application User's Guide
- the Megaco application Reference Manual
- Concurrent Programming in Erlang, 2nd Edition (1996), Prentice-Hall, ISBN
  0-13-508301-X.
