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
# Transport mechanisms

## Callback interface

The callback interface of the transport module contains several functions. Some
of which are mandatory while others are only optional:

- `send_message` \- Send a message. _Mandatory_
- `block` \- Block the transport. _Optional_

  This function is usefull for flow control.

- `unblock` \- Unblock the transport. _Optional_

For more detail, see the `m:megaco_transport` behaviour definition.

## Examples

The Megaco/H.248 application contains implementations for the two protocols
specified by the Megaco/H.248 standard; UDP, see `m:megaco_udp`, and TCP/TPKT,
see `m:megaco_tcp`.
