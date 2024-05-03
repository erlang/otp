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

The Mnesia application provides a heavy-duty real-time distributed database.

## Scope

This User's Guide describes how to build Mnesia-backed applications, and how to
integrate and use the Mnesia database management system with OTP. Programming
constructs are described, and numerous programming examples are included to
illustrate the use of Mnesia.

This User's Guide is organized as follows:

- [Mnesia](mnesia_overview.md) provides an introduction to Mnesia.
- [Getting Started](mnesia_chap2.md) introduces Mnesia with an example database.
  Examples are included on how to start an Erlang session, specify a Mnesia
  database directory, initialize a database schema, start Mnesia, and create
  tables. Initial prototyping of record definitions is also discussed.
- [Build a Mnesia Database](mnesia_chap3.md) more formally describes the steps
  introduced in the previous section, namely the Mnesia functions that define a
  database schema, start Mnesia, and create the required tables.
- [Transactions and Other Access Contexts](mnesia_chap4.md) describes the
  transactions properties that make Mnesia into a fault-tolerant, real-time
  distributed database management system. This section also describes the
  concept of locking to ensure consistency in tables, and "dirty operations", or
  shortcuts, which bypass the transaction system to improve speed and reduce
  overheads.
- [Miscellaneous Mnesia Features](mnesia_chap5.md) describes features that
  enable the construction of more complex database applications. These features
  include indexing, checkpoints, distribution and fault tolerance, disc-less
  nodes, replica manipulation, local content tables, concurrency, and
  object-based programming in Mnesia.
- [Mnesia System Information](mnesia_chap7.md) describes the files contained in
  the Mnesia database directory, database configuration data, core and table
  dumps, as well as the functions used for backup, restore, fallback, and
  disaster recovery.
- [Combine Mnesia with SNMP](mnesia_chap8.md) is a short section that outlines
  the integration between Mnesia and SNMP.
- [Appendix A: Backup Callback Interface](mnesia_app_a.md) is a program listing
  of the default implementation of this facility.
- [Appendix B: Activity Access Callback Interface](mnesia_app_b.md) is a program
  outlining one possible implementation of this facility.
- [Appendix C: Fragmented Table Hashing Callback Interface](mnesia_app_c.md) is
  a program outlining one possible implementation of this facility.

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language,
system development principles, and database management systems.
