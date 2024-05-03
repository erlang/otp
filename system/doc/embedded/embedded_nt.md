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
# Windows NT

[](){: #windows-nt }

This section describes the operating system-specific parts of OTP that relate to
Windows NT.

A normal installation of Windows NT 4.0, with Service Pack 4 or later, is
required for an embedded Windows NT running OTP.

## Memory Use

RAM memory of 96 MB is recommended to run OTP on Windows NT. A system with less
than 64 MB of RAM is not recommended.

## Disk Space Use

A minimum Windows NT installation with networking needs 250 MB, and an extra 130
MB for the swap file.

## Installing an Embedded System

Normal Windows NT installation is performed. No additional application programs
are needed, such as Internet Explorer or web server. Networking with TCP/IP is
required.

Service Pack 4 or later must be installed.

### Hardware Watchdog

For Windows NT running on standard PCs with ISA and/or PCI bus, an extension
card with a hardware watchdog can be installed.

For more information, see the `m:heart` manual page in Kernel.

## Starting Erlang

On an embedded system, the `erlsrv` module is to be used to install the Erlang
process as a Windows system service. This service can start after Windows NT has
booted.

For more information, see the `erlsrv` manual page in ERTS.
