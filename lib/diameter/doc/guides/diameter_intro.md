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

The diameter application is an implementation of the Diameter protocol as
defined by RFC 6733. It supports arbitrary Diameter applications by way of a
_dictionary_ interface that allows messages and AVPs to be defined and input
into diameter as configuration. It has support for all roles defined in the RFC:
client, server and agent. This chapter provides a short overview of the
application.

A Diameter node is implemented by configuring a _service_ and one or more
_transports_ using the interface module `m:diameter`. The service configuration
defines the Diameter applications to be supported by the node and, typically,
the capabilities that it should send to remote peers at capabilities exchange
upon the establishment of transport connections. A transport is configured on a
service and provides protocol-specific send/receive functionality by way of a
transport interface defined by diameter and implemented by a transport module.
The diameter application provides two transport modules: `m:diameter_tcp` and
`m:diameter_sctp` for transport over TCP (using `m:gen_tcp`) and SCTP (using
`m:gen_sctp`) respectively. Other transports can be provided by any module that
implements diameter's [transport interface](`m:diameter_transport`).

While a service typically implements a single Diameter node (as identified by an
Origin-Host AVP), transports can themselves be associated with capabilities AVPs
so that a single service can be used to implement more than one Diameter node.

Each Diameter application defined on a service is configured with a callback
module that implements the [application interface](`m:diameter_app`) through
which diameter communicates the connectivity of remote peers, requests peer
selection for outgoing requests, and communicates the reception of incoming
Diameter request and answer messages. An application using diameter implements
these application callback modules to provide the functionality of the Diameter
node(s) it implements.

Each Diameter application is also configured with a dictionary module that
provide encode/decode functionality for outgoing/incoming Diameter messages
belonging to the application. A dictionary module is generated from a
[dictionary file](diameter_dict.md) using the [diameterc](diameterc_cmd.md)
utility. Dictionaries for the RFC 6733 Diameter Common Messages, Base Accounting
and Relay applications are provided with the diameter application.
