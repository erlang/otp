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
# Manager Implementation Example

This _Implementation Example_ section describes how a simple manager can be
implemented with the SNMP Development Toolkit.

The example shown, _ex2_, can be found in the toolkit distribution.

This example has two functions:

- A simple example of how to use the manager component of the SNMP Development
  Toolkit.
- A simple example of how to write agent test cases, using the new manager.

## The example manager

The example manager, `snmp_ex2_manager`, is a simple example of how to implement
an snmp manager using the manager component of the SNMP Development Toolkit.

The module exports the following functions:

- start_link/0, start_link/1
- stop/0
- agent/2, agent/3
- sync_get/2, sync_get/3
- sync_get_next/2, sync_get_next/3
- sync_get_bulk/4, sync_get_bulk/5
- sync_set/2, sync_set/3
- oid_to_name/1

This module is also used by the test module described in the next section.

## A simple standard test

This simple standard test, `snmp_ex2_simple_standard_test`, a module which,
using the `snmp_ex2_manager` described in the previous section, implements a
simple agent test utility.
