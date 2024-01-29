<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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
# Scheduled for Removal

## Introduction

This document list all functionality in Erlang/OTP that currently are scheduled
for removal. For more information regarding the strategy regarding removal of
functionality see the documentation of
[Support, Compatibility, Deprecations, and Removal](`e:system:misc.md#removal`).

[](){: #otp-29 }

## OTP 29

### Functions Scheduled for Removal in OTP 29

- `ct_slave:_/_` (use ?CT_PEER(), or the 'peer' module instead)
- `slave:_/_` (use the 'peer' module instead)

[](){: #otp-28 }

## OTP 28

### Functions Scheduled for Removal in OTP 28

- `disk_log:inc_wrap_file/1` (use disk_log:next_file/1 instead)

[](){: #otp-27 }

## OTP 27

### Vanilla Driver

The old previously documented support for opening a port to an external resource
by passing an atom (or a string) as first argument to
[`open_port()`](`erlang:open_port/2`), implemented by the vanilla driver, will
be removed in OTP 27. This functionality was marked as obsolete about two
decades ago and then a few years later the documentation for it was removed. If
this functionality is not used with care it might hang or crash the runtime
system.

### Functions Scheduled for Removal in OTP 27

- `crypto:crypto_dyn_iv_init/3` (see the documentation for details)
- `crypto:crypto_dyn_iv_update/3` (see the documentation for details)
- `dbg:stop_clear/0` (use dbg:stop/0 instead)
- `file:pid2name/1` (this functionality is no longer supported)
- `http_uri:decode/1` (use uri_string:unquote function instead)
- `http_uri:encode/1` (use uri_string:quote function instead)
- `zlib:adler32/2` (use erlang:adler32/1 instead)
- `zlib:adler32/3` (use erlang:adler32/2 instead)
- `zlib:adler32_combine/4` (use erlang:adler_combine/3 instead)
- `zlib:crc32/1` (use erlang:crc32/1 on the uncompressed data instead)
- `zlib:crc32/2` (use erlang:crc32/1 instead)
- `zlib:crc32/3` (use erlang:crc32/2 instead)
- `zlib:crc32_combine/4` (use erlang:crc32_combine/3 instead)
- `zlib:getBufSize/1` (this function will be removed in a future release)
- `zlib:inflateChunk/1` (use safeInflate/2 instead)
- `zlib:inflateChunk/2` (use safeInflate/2 instead)
- `zlib:setBufSize/2` (this function will be removed in a future release)
