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
# Removed Functionality

## Introduction

This document lists all removed functionality in Erlang/OTP. For more
information regarding the strategy regarding removals see the documentation of
[Support, Compatibility, Deprecations, and Removal](`e:system:misc.md#removal`).

[](){: #otp-26 }

## OTP 26

### Erlang Distribution Without Large Node Container Support

Communication over the Erlang distribution without support for large
[node container data types (version 4)](`e:erts:erl_dist_protocol.md#DFLAG_V4_NC`)
was as of [OTP 24 deprecated](deprecations.md#otp-24) and support for it was
scheduled for removal in OTP 26. That is, as of OTP 26, support for large node
container data types will become mandatory. This also includes external term
format produced by `term_to_binary()`/`term_to_iovec()`.

### Old Link Protocol

The old link protocol used when communicating over the Erlang distribution was
as of [OTP 24 deprecated](deprecations.md#otp-24) and support for it was
scheduled for removal in OTP 26. As of OTP 26 the
[new link protocol](`e:erts:erl_dist_protocol.md#new_link_protocol`) became
mandatory. That is, Erlang nodes will refuse to connect to nodes not
implementing the new link protocol.

### Functions Removed in OTP 26

- `code:is_module_native/1` (HiPE has been removed)
- `code:rehash/0` (the code path cache feature has been removed)
- `disk_log:accessible_logs/0` (use disk_log:all/0 instead)
- `disk_log:lclose/1` (use disk_log:close/1 instead)
- `disk_log:lclose/2` (use disk_log:close/1 instead)
- `erts_alloc_config:_/_` (this module has as of OTP 26.0 been removed)
- `ftp:start_service/1` (use ftp:open/2 instead)
- `ftp:stop_service/1` (use ftp:close/1 instead)
- `httpd_util:decode_hex/1` (use uri_string:unquote function instead)
- `httpd_util:encode_hex/1` (use uri_string:quote function instead)
- `httpd_util:flatlength/1` (use erlang:iolist_size/1 instead)
- `httpd_util:hexlist_to_integer/1` (use erlang:list_to_integer/2 with base 16
  instead)
- `httpd_util:integer_to_hexlist/1` (use erlang:integer_to_list/2 with base 16
  instead)
- `httpd_util:strip/1` (use string:trim/1 instead)
- `httpd_util:suffix/1` (use filename:extension/1 and string:trim/2 instead)

[](){: #otp-25 }

## OTP 25

### Functions Removed in OTP 25

- `filename:safe_relative_path/1` (use filelib:safe_relative_path/2 instead)
- `http_uri:parse/1` (use uri_string functions instead)
- `http_uri:parse/2` (use uri_string functions instead)
- `http_uri:scheme_defaults/0` (use uri_string functions instead)
- `public_key:ssh_decode/2` (use ssh_file:decode/2 instead)
- `public_key:ssh_encode/2` (use ssh_file:encode/2 instead)
- `public_key:ssh_hostkey_fingerprint/1` (use ssh:hostkey_fingerprint/1 instead)
- `public_key:ssh_hostkey_fingerprint/2` (use ssh:hostkey_fingerprint/2 instead)

[](){: #otp-24 }

## OTP 24

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

### Functions Removed in OTP 24

- `crypto:block_decrypt/3` (use crypto:crypto_one_time/4 or
  crypto:crypto_init/3 + crypto:crypto_update/2 + crypto:crypto_final/1 instead)
- `crypto:block_decrypt/4` (use crypto:crypto*one_time/5,
  crypto:crypto_one_time_aead/6,7 or crypto:crypto*(dyn*iv)?\_init +
  crypto:crypto*(dyn_iv)?\_update + crypto:crypto_final instead)
- `crypto:block_encrypt/3` (use crypto:crypto_one_time/4 or
  crypto:crypto_init/3 + crypto:crypto_update/2 + crypto:crypto_final/1 instead)
- `crypto:block_encrypt/4` (use crypto:crypto*one_time/5,
  crypto:crypto_one_time_aead/6,7 or crypto:crypto*(dyn*iv)?\_init +
  crypto:crypto*(dyn_iv)?\_update + crypto:crypto_final instead)
- `crypto:cmac/3` (use crypto:mac/4 instead)
- `crypto:cmac/4` (use crypto:macN/5 instead)
- `crypto:hmac/3` (use crypto:mac/4 instead)
- `crypto:hmac/4` (use crypto:macN/5 instead)
- `crypto:hmac_final/1` (use crypto:mac_final/1 instead)
- `crypto:hmac_final_n/2` (use crypto:mac_finalN/2 instead)
- `crypto:hmac_init/2` (use crypto:mac_init/3 instead)
- `crypto:hmac_update/2` (use crypto:mac_update/2 instead)
- `crypto:next_iv/_` (see the 'New and Old API' chapter of the CRYPTO User's
  guide)
- `crypto:poly1305/2` (use crypto:mac/3 instead)
- `crypto:stream_decrypt/2` (use crypto:crypto_update/2 instead)
- `crypto:stream_encrypt/2` (use crypto:crypto_update/2 instead)
- `crypto:stream_init/_` (use crypto:crypto_init/3 + crypto:crypto_update/2 +
  crypto:crypto_final/1 or crypto:crypto_one_time/4 instead)
- `filename:find_src/_` (use filelib:find_source/1,3 instead)
- `pg2:_/_` (this module was removed in OTP 24. Use 'pg' instead)
- [`ssl:cipher_suites/0`](`t:ssl:cipher_suites/0`) (use cipher_suites/2,3
  instead)
- `ssl:cipher_suites/1` (use cipher_suites/2,3 instead)
- `ssl:ssl_accept/_` (use ssl_handshake/1,2,3 instead)

[](){: #otp-23 }

## OTP 23

### VxWorks Support

Some parts of OTP has had limited VxWorks support, such as
[`erl_interface`](`e:erl_interface:index.html`). This support was removed in
OTP 23. This limited support was formally deprecated as of OTP 22.

### Legacy parts of erl_interface

The old legacy [`erl_interface`](`e:erl_interface:index.html`) library
(functions with prefix `erl_`) was removed in OTP 23. These parts of
`erl_interface` has been informally deprecated for a very long time, and was
formally deprecated in OTP 22. You typically want to replace the usage of the
`erl_interface` library with the use of the `ei` library which also is part of
the `erl_interface` application.

### httpd_conf module

API functions in the module called `httpd_conf` was deprecated in favor of
standard modules such as `lists`, `string`, `filelib`, and `erlang`. Formally
deprecated as of OTP 18.

### inets - httpd Apache config files

Support for the Apache-compatible config files was removed in OTP 23. A new
config file format was introduced in OTP 12.

### SSL/TLS

For security reasons SSL-3.0 is no longer supported at all.

### Functions Removed in OTP 23

- `erlang:get_stacktrace/0` (use the new try/catch syntax for retrieving the
  stack backtrace)
- `httpd_conf:check_enum/2` (use lists:member/2 instead)
- `httpd_conf:clean/1` (use string:strip/1 instead or possibly the re module)
- `httpd_conf:custom_clean/3` (use string:strip/1 instead or possibly the re
  module)
- `httpd_conf:is_directory/1` (use filelib:is_dir/1 instead)
- `httpd_conf:is_file/1` (use filelib:is_file/1 instead)
- `httpd_conf:make_integer/1` (use erlang:list_to_integer/1 instead)

[](){: #otp-22 }

## OTP 22

### Functions Removed in OTP 22

- `os_mon_mib:_/_` (this module was removed in OTP 22.0)

[](){: #otp-20 }

## OTP 20

### Functions Removed in OTP 20

- `asn1ct:decode/_` (use Mod:decode/2 instead)
- `asn1ct:encode/_` (use Mod:encode/2 instead)
- `erlang:hash/2` (use erlang:phash2/2 instead)
- `ssl:connection_info/1` (use ssl:connection_information/\[1,2] instead)
- `ssl:negotiated_next_protocol/1` (use ssl:negotiated_protocol/1 instead)

[](){: #otp-19 }

## OTP 19

### Functions Removed in OTP 19

- `core_lib:get_anno/1` (use cerl:get_ann/1 instead)
- `core_lib:is_literal/1` (use cerl:is_literal/1 instead)
- `core_lib:is_literal_list/1` (use cerl:is_literal_list/1 instead)
- `core_lib:literal_value/1` (use cerl:concrete/1 instead)
- `core_lib:set_anno/2` (use cerl:set_ann/2 instead)
- `erl_lint:modify_line/2` (use erl_parse:map_anno/2 instead)
- `erl_parse:get_attribute/2` (erl_anno:\{column,line,location,text\}/1 instead)
- `erl_parse:get_attributes/1` (erl_anno:\{column,line,location,text\}/1
  instead)
- `erl_parse:set_line/2` (use erl_anno:set_line/2)
- `erl_scan:attributes_info/_` (use erl_anno:\{column,line,location,text\}/1
  instead)
- `erl_scan:set_attribute/3` (use erl_anno:set_line/2 instead)
- `erl_scan:token_info/_` (use
  erl_scan:\{category,column,line,location,symbol,text\}/1 instead)
- `rpc:safe_multi_server_call/2` (use rpc:multi_server_call/2 instead)
- `rpc:safe_multi_server_call/3` (use rpc:multi_server_call/3 instead)
