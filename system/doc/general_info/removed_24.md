<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
### erl_interface registry

The `registry` functionality part of `erl_interface` was as of
[OTP 23 deprecated](deprecations.md#otp-23) and was removed in OTP 24.

### Compilation of Latin-1 Encoded Erlang Files

The Erlang compiler now refuses to compile source files encoded in Latin-1
without a `%% coding: latin-1` comment at the beginning of the file.

### igor and erl_tidy modules in syntax_tools

The `igor` and `erl_tidy` modules have been removed from OTP and is now
maintained by their original author Richard Carlsson. They can be found at
[github.com/richcarl/igor](https://github.com/richcarl/igor) and
[github.com/richcarl/erl_tidy](https://github.com/richcarl/erl_tidy),
respectively.

### Distributed Disk Logs

The distributed `m:disk_log` feature was as of
[OTP 23 deprecated](deprecations.md#otp-23) and was removed in OTP 24.

### Old Crypto API

The [Old API](`e:crypto:new_api.md#the-old-api`) was removed in OTP 24. The
support was formally deprecated as of OTP 23.

For replacement functions see the [New API](`e:crypto:new_api.md#the-new-api`).

### Megaco version 3 encoding config

The pre-release version 3 encoding configs; `prev3a`, `prev3b` and `prev3c` was
removed in OTP 24. Use the full version instead.

The (encoding) config option for the full version, `{version3, 3}`, will still
be supported, even though its no longer necessary to specify it this way.
