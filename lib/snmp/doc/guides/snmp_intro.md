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
# SNMP Introduction

The SNMP development toolkit contains the following parts:

- An Extensible multi-lingual SNMP agent, which understands SNMPv1 (RFC1157),
  SNMPv2c (RFC1901, 1905, 1906 and 1907), SNMPv3 (RFC2271, 2272, 2273, 2274 and
  2275), or any combination of these protocols.
- A multi-lingual SNMP manager.
- A MIB compiler, which understands SMIv1 (RFC1155, 1212, and 1215) and SMIv2
  (RFC1902, 1903, and 1904).

The SNMP development tool provides an environment for rapid agent/manager
prototyping and construction. With the following information provided, this tool
is used to set up a running multi-lingual SNMP agent/manager:

- a description of a Management Information Base (MIB) in Abstract Syntax
  Notation One (ASN.1)
- instrumentation functions for the managed objects in the MIB, written in
  Erlang.

The advantage of using an extensible (agent/manager) toolkit is to remove
details such as type-checking, access rights, Protocol Data Unit (PDU),
encoding, decoding, and trap distribution from the programmer, who only has to
write the instrumentation functions, which implement the MIBs. The `get-next`
function only has to be implemented for tables, and not for every variable in
the global naming tree. This information can be deduced from the ASN.1 file.

## Scope and Purpose

This manual describes the SNMP development tool, as a component of the
Erlang/Open Telecom Platform development environment. It is assumed that the
reader is familiar with the Erlang Development Environment, which is described
in a separate User's Guide.

## Prerequisites

The following prerequisites are required for understanding the material in the
SNMP User's Guide:

- the basics of the Simple Network Management Protocol version 1 (SNMPv1)
- the basics of the community-based Simple Network Management Protocol version 2
  (SNMPv2c)
- the basics of the Simple Network Management Protocol version 3 (SNMPv3)
- the knowledge of defining MIBs using SMIv1 and SMIv2
- familiarity with the Erlang system and Erlang programming

The tool requires Erlang release 4.7 or later.

## Definitions

The following definitions are used in the SNMP User's Guide.

- **MIB** - The conceptual repository for management information is called the
  Management Information Base (MIB). It does not hold any data, merely a
  definition of what data can be accessed. A definition of an MIB is a
  description of a collection of managed objects.

- **SMI** - The MIB is specified in an adapted subset of the Abstract Syntax
  Notation One (ASN.1) language. This adapted subset is called the Structure of
  Management Information (SMI).

- **ASN.1** - ASN.1 is used in two different ways in SNMP. The SMI is based on
  ASN.1, and the messages in the protocol are defined by using ASN.1.

- **Managed object** - A resource to be managed is represented by a managed
  object, which resides in the MIB. In an SNMP MIB, the managed objects are
  either:

  - _scalar variables_, which have only one instance per context. They have
    single values, not multiple values like vectors or structures.
  - _tables_, which can grow dynamically.
  - a _table element_, which is a special type of scalar variable.

- **Operations** - SNMP relies on the three basic operations: get (object), set
  (object, value) and get-next (object).

- **Instrumentation function** - An instrumentation function is associated with
  each managed object. This is the function, which actually implements the
  operations and will be called by the agent when it receives a request from the
  management station.

- **Manager** - A manager generates commands and receives notifications from
  agents. There usually are only a few managers in a system.

- **Agent** - An agent responds to commands from the manager, and sends
  notification to the manager. There are potentially many agents in a system.

## About This Manual

In addition to this introductory chapter, the SNMP User's Guide contains the
following chapters:

- Chapter 2: "Functional Description" describes the features and operation of
  the SNMP development toolkit. It includes topics on Sub-agents and MIB
  loading, Internal MIBs, and Traps.
- Chapter 3: "The MIB Compiler" describes the features and the operation of the
  MIB compiler.
- Chapter 4: "Running the application" describes how to start and configure the
  application. Topics on how to debug the application are also included.
- Chapter 5: "Definition of Agent Configuration Files" is a reference chapter,
  which contains more detailed information about the agent configuration files.
- Chapter 6: "Definition of Manager Configuration Files" is a reference chapter,
  which contains more detailed information about the manager configuration
  files.
- Chapter 7: "Agent Implementation Example" describes how an MIB can be
  implemented with the SNMP Development Toolkit. Implementation examples are
  included.
- Chapter 8: "Instrumentation Functions" describes how instrumentation functions
  should be defined in Erlang for the different operations.
- Chapter 9: "Definition of Instrumentation Functions" is a reference chapter
  which contains more detailed information about the instrumentation functions.
- Chapter 10: "Definition of Agent Net if" is a reference chapter, which
  describes the Agent Net if function in detail.
- Chapter 11: "Definition of Manager Net if" is a reference chapter, which
  describes the Manager Net if function in detail.
- Chapter 12: "Advanced Agent Topics" describes sub-agents, agent semantics,
  audit trail logging, and the consideration of distributed tables.
- Appendix A describes the conversion of SNMPv2 to SNMPv1 error messages.
- Appendix B contains the RFC1903 text on `RowStatus`.

## Where to Find More Information

Refer to the following documentation for more information about SNMP and about
the Erlang/OTP development system:

- Marshall T. Rose (1991), "The Simple Book - An Introduction to Internet
  Management", Prentice-Hall
- Evan McGinnis and David Perkins (1997), "Understanding SNMP MIBs",
  Prentice-Hall
- RFC1155, 1157, 1212 and 1215 (SNMPv1)
- RFC1901-1907 (SNMPv2c)
- RFC1908, 2089 (coexistence between SNMPv1 and SNMPv2)
- RFC2271, RFC2273 (SNMP std MIBs)
- the Mnesia User's Guide
- the Erlang 4.4 Extensions User's Guide
- the Reference Manual
- the Erlang Embedded Systems User's Guide
- the System Architecture Support Libraries (SASL) User's Guide
- the Installation Guide
- the Asn1 User's Guide
- Concurrent Programming in Erlang, 2nd Edition (1996), Prentice-Hall, ISBN
  0-13-508301-X.
