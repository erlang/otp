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
# Manager Functional Description

## Features

The manager provided with the tool is a lightweight manager that basically
provides a means to communicate with agents.

It does not really implement any management capabilities by itself. That is up
to the _user_.

A _user_ in this context is basically a module implementing the `m:snmpm_user`
behaviour. A _user_ can issue snmp requests and receive notification/traps.

Agents to be accessed by the manager needs to be registered by a user. Once
registered, they can be accessed by all registered users.

Notifications/traps from an agent is delivered to the user that did the
registration.

Any message from an agent that is not registered is delivered to the _default
user_.

By default, the _default user_ is set to the `snmpm_user_default` module, which
simply sends an info message to the error_logger. It is however highly
recommended that this module be replaced by another that does something useful
(see [configuration params](snmp_config.md#configuration_params) for more info).

When using version 3, then (at least one) _usm user_ has to be registered.

Requests can be issued in two different ways. Synchronous (see
[sync_set](`snmpm:sync_set2/4`), [sync_get](`snmpm:sync_get2/4`),
[sync_get_next](`snmpm:sync_get_next2/4`) and
[sync_get_bulk](`snmpm:sync_get_bulk2/6`)) and asynchronous (see
[async_set](`snmpm:async_set2/4`), [async_get](`snmpm:async_get2/4`),
[async_get_next](`snmpm:async_get_next2/4`) and
[async_get_bulk](`snmpm:async_get_bulk2/6`)). With synchronous the snmp reply is
returned by the function. With asynchronous, the reply will instead be delivered
through a call to one of the `handle_pdu` callback function defined by the
[handle_pdu](`c:snmpm_user:handle_pdu/4`) behaviour.

## Operation

The following steps are needed to get the manager running:

1. \[optional] Implement the default user.
1. Implement the user(s).
1. Configure the application (manager).
1. Start the application (manager).
1. Register the user(s).
1. The user(s) register their agents.

## MIB loading

It is possible to load mibs into the manager, but this is not necessary for
normal operation, and not recommended.
