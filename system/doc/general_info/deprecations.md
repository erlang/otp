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
# Deprecations

## Introduction

This document lists all deprecated functionality in Erlang/OTP. For more
information regarding the strategy regarding deprecations see the documentation
of
[Support, Compatibility, Deprecations, and Removal](`e:system:misc.md#deprecation`).

[](){: #otp-26 }

## OTP 26

### Functions Deprecated in OTP 26

- `dbg:stop_clear/0` (use dbg:stop/0 instead)
- `disk_log:inc_wrap_file/1` (use disk_log:next_file/1 instead)
- `file:pid2name/1` (this functionality is no longer supported)

[](){: #otp-25 }

## OTP 25

### Functions Deprecated in OTP 25

- `crypto:crypto_dyn_iv_init/3` (see the documentation for details)
- `crypto:crypto_dyn_iv_update/3` (see the documentation for details)
- `ct_slave:_/_` (use ?CT_PEER(), or the 'peer' module instead)
- `slave:_/_` (use the 'peer' module instead)

[](){: #otp-24 }

## OTP 24

### Erlang Distribution Without Large Node Container Support

Communication over the Erlang distribution without support for large
[node container data types (version 4)](`e:erts:erl_dist_protocol.md#DFLAG_V4_NC`)
is as of OTP 24 deprecated and is scheduled for removal in OTP 26. That is, as
of OTP 26, support for large node container data types will become mandatory.

### Old Link Protocol

The old link protocol used when communicating over the Erlang distribution is as
of OTP 24 deprecated and support for it is scheduled for removal in OTP 26. As
of OTP 26, the
[new link protocol](`e:erts:erl_dist_protocol.md#new_link_protocol`) will become
mandatory. That is, Erlang nodes will then refuse to connect to nodes not
implementing the new link protocol. If you implement the Erlang distribution
yourself, you are, however, encouraged to implement the new link protocol as
soon as possible since the old protocol can cause links to enter an inconsistent
state.

### ?NO_APP macro

The ?NO_APP macro in the edoc include file `edoc_doclet.hrl` has been
deprecated.

### Functions Deprecated in OTP 24

- `erlang:phash/2` (use erlang:phash2/2 instead)
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

[](){: #otp-23 }

## OTP 23

### Crypto Old API

The [Old API](`e:crypto:new_api.md#the-old-api`) is deprecated as of OTP 23 and
has been [removed in OTP 24](removed.md#otp-24).

For replacement functions see the [New API](`e:crypto:new_api.md#the-new-api`).

### http_uri

Since OTP 21 the recommended module to handle URIs is `m:uri_string`. The module
http_uri does not provide a implementation that satisfies the RFC.

### ssh

The public key algorithm `'ssh-rsa` is regarded as insecure due to its usage of
SHA1, and is therefore deprecated. It will not be available by default from
OTP-24.

The public key algorithm `'ssh-dss` is regarded as insecure due to its usage of
SHA1 and its short key length, and is therefore deprecated. It is not available
by default from OTP-23.

### Distributed Disk Logs

As of OTP 23, the distributed `m:disk_log` feature has been deprecated and it
has also been [removed in OTP 24](removed.md#otp-24).

### erl_interface registry

As of OTP 23, the `registry` functionality part of `erl_interface` has been
deprecated and it has also been [removed in OTP 24](removed.md#otp-24).

### Functions Deprecated in OTP 23

- `http_uri:decode/1` (use uri_string:unquote function instead)
- `http_uri:encode/1` (use uri_string:quote function instead)
- `httpd:parse_query/1` (use uri_string:dissect_query/1 instead)

[](){: #otp-22 }

## OTP 22

### VxWorks Support

Some parts of OTP has had limited VxWorks support, such as for example
[`erl_interface`](`e:erl_interface:index.html`). This support is as of OTP 22
formally deprecated and has also been [removed in OTP 23](removed.md#otp-23).

### Legacy parts of erl_interface

The old legacy [`erl_interface`](`e:erl_interface:index.html`) library
(functions with prefix `erl_`) is deprecated as of OTP 22. These parts of
`erl_interface` has been informally deprecated for a very long time. You
typically want to replace the usage of the `erl_interface` library with the use
of the `ei` library which also is part of the `erl_interface` application. The
old legacy [`erl_interface`](`e:erl_interface:index.html`) library has also been
[removed in OTP 23](removed.md#otp-23).

### System Events

The format of "System Events" as defined in the man page for `m:sys` has been
clarified and cleaned up. Due to this, code that relied on the internal badly
documented previous (before this change) format of OTP's "System Events", needs
to be changed.

In the wake of this the function `sys:get_debug/3` that returns data with
undocumented and internal format (and therefore is practically useless) has been
deprecated, and a new function `sys:get_log/1` has been added, that hopefully
does what the deprecated function was intended for.

### Functions Deprecated in OTP 22

- `net:broadcast/3` (use rpc:eval_everywhere/3 instead)
- `net:call/4` (use rpc:call/4 instead)
- `net:cast/4` (use rpc:cast/4 instead)
- `net:ping/1` (use net_adm:ping/1 instead)
- `net:sleep/1` (use 'receive after T -> ok end' instead)
- `sys:get_debug/3` (incorrectly documented and only for internal use. Can often
  be replaced with sys:get_log/1)

[](){: #otp-20 }

## OTP 20

### Functions Deprecated in OTP 20

- `crypto:rand_uniform/2` (use rand:uniform/1 instead)
- `gen_fsm:_/_` (use the 'gen_statem' module instead)

[](){: #otp-19 }

## OTP 19

### SSL/TLS

For security reasons SSL-3.0 is no longer supported by default, but can be
configured.

### Functions Deprecated in OTP 19

- `queue:lait/1` (use queue:liat/1 instead)
- `random:_/_` (use the 'rand' module instead)

[](){: #otp-18 }

## OTP 18

### erlang:now/0

New time functionality and a new time API was introduced. For more information
see the [Time and Time Correction](`e:erts:time_correction.md`) chapter in the
ERTS User's guide and specifically the
[Dos and Donts](`e:erts:time_correction.md#Dos_and_Donts`) section on how to
replace usage of `erlang:now/0`.

### httpd_conf module

API functions in the module `httpd_conf` was deprecated in favor of standard
modules such as `lists`, `string`, `filelib`, and `erlang`.

### Functions Deprecated in OTP 18

- `erlang:now/0` (see the "Time and Time Correction in Erlang" chapter of the
  ERTS User's Guide for more information)

[](){: #otp-16 }

## OTP 16

### Functions Deprecated in OTP 16

- `wxCalendarCtrl:enableYearChange/1` (not available in wxWidgets-2.9 and later)
- `wxCalendarCtrl:enableYearChange/2` (not available in wxWidgets-2.9 and later)

[](){: #otp-12 }

## OTP 12

### inets - httpd Apache config files

A new config file format was introduced.

### Functions Deprecated in OTP 12

- `auth:cookie/0` (use erlang:get_cookie/0 instead)
- `auth:cookie/1` (use erlang:set_cookie/2 instead)
- `auth:is_auth/1` (use net_adm:ping/1 instead)
- `auth:node_cookie/_` (use erlang:set_cookie/2 and net_adm:ping/1 instead)
- `calendar:local_time_to_universal_time/1` (use
  calendar:local_time_to_universal_time_dst/1 instead)
