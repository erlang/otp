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
# Overview

The management of data in telecommunications systems has many aspects of which
some, but not all, are addressed by traditional Database Management Systems
(DBMSs). In particular, the high level of fault tolerance required in many
nonstop systems, combined with requirements on the DBMS to run in the same
address space as the applications, have led us to implement a new DBMS, called
Mnesia.

Mnesia is implemented in, and tightly coupled to Erlang. It provides the
functionality that is necessary for the implementation of fault-tolerant
telecommunications systems.

Mnesia is a multiuser distributed DBMS specifically designed for
industrial-grade telecommunications applications written in Erlang, which is
also the intended target language. Mnesia tries to address all the data
management issues required for typical telecommunications systems and has a
number of features not normally found in traditional DBMSs.

Telecommunications applications need a mix of a broad range of features
generally not provided by traditional DBMSs. Mnesia is designed to meet
requirements such as:

- Fast real-time key-value lookup
- Complex non-real-time queries (mainly for operation and maintenance tasks)
- Distributed data (due to the distributed nature of the applications)
- High fault tolerance
- Dynamic reconfiguration
- Complex objects

Mnesia addresses the typical data management issues required for
telecommunications applications which sets it apart from most other DBMSs. It
combines many concepts found in traditional DBMSs, such as transactions and
queries, with concepts found in data management systems for telecommunications
applications such as:

- Fast real-time operations
- Configurable replication for fault tolerance
- Dynamic reconfiguration without service disruption

Mnesia is also unique due to its tight coupling to Erlang. It almost turns
Erlang into a database programming language, which yields many benefits. The
foremost is that the impedance mismatch between the data format used by the DBMS
and the data format used by the programming language, which is used to
manipulate the data, completely disappears.

## The Mnesia Database Management System

### Features

Mnesia has the following features that combine to produce a fault-tolerant
distributed database management system (DBMS) written in Erlang:

- Database schema can be dynamically reconfigured at runtime.
- Tables can be declared to have properties such as location, replication, and
  persistence.
- Tables can be moved or replicated to several nodes to improve fault tolerance.
  Other nodes in the system can still access the tables to read, write, and
  delete records.
- Table locations are transparent to the programmer. Programs address table
  names and the system itself keeps track of table locations.
- Transactions can be distributed and multiple operations can be executed within
  a single transaction.
- Multiple transactions can run concurrently and their execution is fully
  synchronized by Mnesia, ensuring that no two processes manipulate the same
  data simultaneously.
- Transactions can be assigned the property of being executed on all nodes in
  the system, or on none.
- Transactions can be bypassed using dirty operations, which reduce overheads
  and run fast.

All of the above features are described in detail in the coming sections.

### Query List Comprehension

Query List Comprehension (QLC) can be used with Mnesia to produce specialized
functions that enhance its operational ability. QLC has its own documentation as
part of the OTP documentation set. The main QLC advantages when used with Mnesia
are:

- QLC can optimize the query compiler for Mnesia, essentially making the system
  more efficient.
- QLC can be used as a database programming language for Mnesia. It includes a
  notation called list comprehensions which can be used to execute complex
  database queries over a set of tables.

For more information about QLC, please see the `m:qlc` manual page in STDLIB.

### When to Use Mnesia

Mnesia is a great fit for applications that:

- Need to replicate data.
- Perform complex data queries.
- Need to use atomic transactions to safely update several records
  simultaneously.
- Require soft real-time characteristics.

Mnesia is not as appropriate for applications that:

- Process plain text or binary data files.
- Merely need a lookup dictionary that can be stored on disc. Such applications
  may use the standard library module `dets`, which is a disc-based version of
  the `ets` module. For more information about `dets`, please see the `m:dets`
  manual page in STDLIB.
- Need disc logging facilities. Such applications may use the module `disk_log`.
  For more information about `disk_log`, please see the `m:disk_log` manual page
  in Kernel.
- Require hard real-time characteristics.
