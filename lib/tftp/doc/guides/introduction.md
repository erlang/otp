<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2023-2025. All Rights Reserved.

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

Trivial File Transfer Protocol (TFTP) is a very simple protocol used
to transfer files over the transport datagram protocol UDP.

On the client side, function [read_file/3](`tftp:read_file/3`) and
[write_file/3](`tftp:write_file/3`) spawn a temporary client process
establishing contact with a TFTP daemon and perform the file transfer.

`tftp` uses a callback module to handle the file transfer. Two such callback
modules are provided, `tftp_binary` and `tftp_file`. See
[read_file/3](`tftp:read_file/3`) and [write_file/3](`tftp:write_file/3`) for
details. You can also implement your own callback modules, see
[callbacks](`m:tftp#callbacks`).

# Security Considerations

As stated in ([RFC 1350](https://datatracker.ietf.org/doc/html/rfc1350))
be aware that "Since TFTP includes no login or access
control mechanisms, care must be taken in the rights granted to a TFTP
server process so as not to violate the security of the server hosts
file system.  TFTP is often installed with controls such that only
files that have public read access are available via TFTP and writing
files via TFTP is disallowed."

