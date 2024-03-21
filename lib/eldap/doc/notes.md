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
# Eldap Release Notes

This document describes the changes made to the Eldap application.

## Eldap 1.2.12

### Fixed Bugs and Malfunctions

- Add missing dependency to asn1 application

  Own Id: OTP-18810

## Eldap 1.2.11

### Improvements and New Features

- Added a new function eldap:info/1 that returns the socket and the transport
  protocol for the eldap connection.

  Own Id: OTP-18480

## Eldap 1.2.10

### Fixed Bugs and Malfunctions

- Fix eldap extensibleMatch dnAttributes option.

  According to the ldap ASN1 the dnAttributes should be a bool, instead it was
  generated as a string.

  Own Id: OTP-17877 Aux Id: PR-5615

### Improvements and New Features

- Implemented paged searches according to
  https://www.rfc-editor.org/rfc/rfc2696.txt

  Own Id: OTP-17924 Aux Id: PR-5538

## Eldap 1.2.9

### Improvements and New Features

- Add ability to specify size limit on ldap requests

  Own Id: OTP-17166 Aux Id: PR-2904

## Eldap 1.2.8

### Fixed Bugs and Malfunctions

- Fix dialyzer warnings in eldap when not matching the return value of
  ssl:close/1.

  Own Id: OTP-15775

## Eldap 1.2.7

### Improvements and New Features

- Back port of bug fix ERL-893 from OTP-22 and document enhancements that will
  solve dialyzer warnings for users of the ssl application.

  This change also affects public_key, eldap (and inet doc).

  Own Id: OTP-15785 Aux Id: ERL-929, ERL-893, PR-2215

## Eldap 1.2.6

### Fixed Bugs and Malfunctions

- A race condition at close could cause the eldap client to exit with a badarg
  message as cause.

  Own Id: OTP-15342 Aux Id: ERIERL-242

## Eldap 1.2.5

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## Eldap 1.2.4

### Improvements and New Features

- Update to use the new string api instead of the old.

  Own Id: OTP-15036

## Eldap 1.2.3.1

### Fixed Bugs and Malfunctions

- A race condition at close could cause the eldap client to exit with a badarg
  message as cause.

  Own Id: OTP-15342 Aux Id: ERIERL-242

## Eldap 1.2.3

### Fixed Bugs and Malfunctions

- Removed all old unused files in the documentation.

  Own Id: OTP-14475 Aux Id: ERL-409, PR-1493

## Eldap 1.2.2.1

### Fixed Bugs and Malfunctions

- A race condition at close could cause the eldap client to exit with a badarg
  message as cause.

  Own Id: OTP-15342 Aux Id: ERIERL-242

## Eldap 1.2.2

### Fixed Bugs and Malfunctions

- If the underlying tcp connection is closed and an LDAP operation returned
  tcp_error, the client applications tend to close the ldap handle with
  eldap:close. This will cause a `{nocatch, {gen_tcp_error, ...}}` exception.

  Such errors are now ignored during close, because the socket will be closed
  anyway.

  Own Id: OTP-13590 Aux Id: PR-1048

### Improvements and New Features

- Modernize test suites

  Own Id: OTP-13566

## Eldap 1.2.1

### Fixed Bugs and Malfunctions

- ELDAP did not send an `'unBind'` request before closing the connection.

  Own Id: OTP-13327

### Improvements and New Features

- Handles the `referral` result code from LDAP servers. Adds the return value
  `{ok, {referral,UrlList}}` to some functions. See the Eldap reference manual
  for details.

  Own Id: OTP-12272

## Eldap 1.2

### Improvements and New Features

- Support added for LDAP Password Modify Extended Operation (RFC 3062). Thanks
  to danielwhite.

  Own Id: OTP-12282

## Eldap 1.1.1

### Fixed Bugs and Malfunctions

- Corrects that `eldap:close/1` returned a tuple instead of the specified atom
  `ok`.

  Own Id: OTP-12349

### Improvements and New Features

- Clarification in the reference manual for `eldap:modify_dn/5`,
  `eldap:search/2` and `eldap:start_tls/3`.

  Own Id: OTP-12354

- The eldap test suites are extended and re-organized.

  Own Id: OTP-12355

## Eldap 1.1

### Fixed Bugs and Malfunctions

- Fixed that eldap:open did not use the Timeout parameter when calling
  ssl:connect. (Thanks Wiesław Bieniek for reporting)

  Own Id: OTP-12311

### Improvements and New Features

- Added the LDAP filter `extensibleMatch`.

  Own Id: OTP-12174

## Eldap 1.0.4

### Fixed Bugs and Malfunctions

- `eldap:open/2` and `eldap:open/3` gave wrong return values for option errors.

  Own Id: OTP-12182

### Improvements and New Features

- Nearly all TCP options are possible to give in the `eldap:open/2` call.

  Own Id: OTP-12171

## Eldap 1.0.3

### Fixed Bugs and Malfunctions

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

- Add support for IPv6 connections, By including the \[inet6] option in
  eldap:open/2. Default value is still \[inet] (Thanks to Edwin Fine)

  Own Id: OTP-11753

- Fixed bug where eldap:search returned binaries instead of strings. (Thanks
  Simon MacMullen for the report)

  Own Id: OTP-11768

## Eldap 1.0.2

### Fixed Bugs and Malfunctions

- Removed \{verify,0\} from ssl-options because eldap does not support peer
  verification. Thanks to Florian Waas for reporting.

  Own Id: OTP-11354

### Improvements and New Features

- The ldap client eldap now supports the start_tls operation. This upgrades an
  existing tcp connection to encryption using tls, if the server supports it.
  See eldap:start_tls/2 and /3.

  Own Id: OTP-11336

## Eldap 1.0.1

### Improvements and New Features

- Fixed various dialyzer warnings

  Own Id: OTP-10403 Aux Id: kunagi-258 \[169]

- Configure the SSL options fully in eldap.

  Own Id: OTP-10728

## Eldap 1.0

New application.
