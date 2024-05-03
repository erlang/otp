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
# Combine Mnesia with SNMP

## Combine Mnesia and SNMP

Many telecommunications applications must be controlled and reconfigured
remotely. It is sometimes an advantage to perform this remote control with an
open protocol such as the Simple Network Management Protocol (SNMP). The
alternatives to this would be the following:

- Not being able to control the application remotely
- Using a proprietary control protocol
- Using a bridge that maps control messages in a proprietary protocol to a
  standardized management protocol and conversely

All these approaches have different advantages and disadvantages. Mnesia
applications can easily be opened to the SNMP protocol. A direct 1-to-1 mapping
can be established between Mnesia tables and SNMP tables. This means that a
Mnesia table can be configured to be _both_ a Mnesia table and an SNMP table. A
number of functions to control this behavior are described in the Reference
Manual.
