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
# FTP client introduction

FTP clients are considered to be rather temporary. Thus, they are only started
and stopped during runtime and cannot be started at application startup. The FTP
client API is designed to allow some functions to return intermediate results.
This implies that only the process that started the FTP client can access it
with preserved sane semantics. If the process that started the FTP session dies,
the FTP client process terminates.

The client supports IPv6 as long as the underlying mechanisms also do so.