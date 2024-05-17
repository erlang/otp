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
# SNMP Release Notes

## SNMP 5.16

### Improvements and New Features

- `-callback` attributes have been added to modules `m:snmpa_network_interface_filter`, `m:snmpa_notification_filter`, `m:snmpm_network_interface_filter`, `m:snmpm_user`, and `m:snmpa_notification_delivery_info_receiver`.
  
  New `-type` attributes have also been added to modules `m:snmp`, `m:snmpa`, `m:snmpm`, and `m:snmpa_conf` to support the previously mentioned callbacks.

  Own Id: OTP-18785 Aux Id: [PR-7702]

- Updated types and specs for all API modules.

  Own Id: OTP-18934 Aux Id: BL-312

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

[PR-7702]: https://github.com/erlang/otp/pull/7702
[PR-8026]: https://github.com/erlang/otp/pull/8026

## SNMP 5.15

### Improvements and New Features

- Make snmp handle gen_udp with socket backend on Windows (completion).

  Own Id: OTP-18598 Aux Id: OTP-18029

## SNMP 5.14

### Improvements and New Features

- The implementation has been fixed to use `proc_lib:init_fail/2,3` where
  appropriate, instead of `proc_lib:init_ack/1,2`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18490 Aux Id: OTP-18471, GH-6339, PR-6843

## SNMP 5.13.5

### Improvements and New Features

- Attempts to minimize the number of the error reports during a failed agent
  init.

  Own Id: OTP-18422 Aux Id: ERIERL-873

## SNMP 5.13.4

### Improvements and New Features

- Replace size/1 with either tuple_size/1 or byte_size/1

  The [`size/1`](`size/1`) BIF is not optimized by the JIT, and its use can
  result in worse types for Dialyzer.

  When one knows that the value being tested must be a tuple,
  [`tuple_size/1`](`tuple_size/1`) should always be preferred.

  When one knows that the value being tested must be a binary,
  [`byte_size/1`](`byte_size/1`) should be preferred. However,
  [`byte_size/1`](`byte_size/1`) also accepts a bitstring (rounding up size to a
  whole number of bytes), so one must make sure that the call to `byte_size/` is
  preceded by a call to [`is_binary/1`](`is_binary/1`) to ensure that bitstrings
  are rejected. Note that the compiler removes redundant calls to
  [`is_binary/1`](`is_binary/1`), so if one is not sure whether previous code
  had made sure that the argument is a binary, it does not harm to add an
  [`is_binary/1`](`is_binary/1`) test immediately before the call to
  [`byte_size/1`](`byte_size/1`).

  Own Id: OTP-18432 Aux Id:
  GH-6672,PR-6793,PR-6784,PR-6787,PR-6785,PR-6682,PR-6800,PR-6797,PR-6798,PR-6799,PR-6796,PR-6813,PR-6671,PR-6673,PR-6684,PR-6694,GH-6677,PR-6696,PR-6670,PR-6674

## SNMP 5.13.3

### Fixed Bugs and Malfunctions

- Single threaded agent crash when vacm table not properly initiated.

  Own Id: OTP-18379 Aux Id: ERIERL-904

## SNMP 5.13.2

### Fixed Bugs and Malfunctions

- Explicitly close the socket(s) when terminating (default-) net-if process.

  Own Id: OTP-18352 Aux Id: ERIERL-881

## SNMP 5.13.1.1

### Fixed Bugs and Malfunctions

- Single threaded agent crash when vacm table not properly initiated.

  Own Id: OTP-18379 Aux Id: ERIERL-904

## SNMP 5.13.1

### Fixed Bugs and Malfunctions

- Improved the get-bulk response max size calculation. Its now possible to
  configure 'empty pdu size', see appendix c for more info.

  Own Id: OTP-17115 Aux Id: ERIERL-456

- Fix various example dialyzer issues

  Own Id: OTP-18180 Aux Id: ERIERL-837

## SNMP 5.13

### Improvements and New Features

- Input for `configure` scripts adapted to `autoconf` 2\.71.

  Own Id: OTP-17414 Aux Id: PR-4967

- Removed deprecated functions slated for removal in OTP-25. Also removed "dead"
  code, kept for backward compatibility reasons.

  Own Id: OTP-17612

## SNMP 5.12.0.3

### Improvements and New Features

- Attempts to minimize the number of the error reports during a failed agent
  init.

  Own Id: OTP-18422 Aux Id: ERIERL-873

## SNMP 5.12.0.2

### Fixed Bugs and Malfunctions

- Single threaded agent crash when vacm table not properly initiated.

  Own Id: OTP-18379 Aux Id: ERIERL-904

## SNMP 5.12.0.1

### Fixed Bugs and Malfunctions

- Explicitly close the socket(s) when terminating (default-) net-if process.

  Own Id: OTP-18352 Aux Id: ERIERL-881

## SNMP 5.12

### Fixed Bugs and Malfunctions

- The compilation time is no longer recorded in BEAM files. There remained
  several undocumented functions that attempted to retrieve compilation times.
  Those have now been removed.

  Own Id: OTP-17962

### Improvements and New Features

- \[agent] Remove expectation of socket being a port.

  Own Id: OTP-16559

## SNMP 5.11

### Fixed Bugs and Malfunctions

- Handling of test config flag when starting "empty".

  Own Id: OTP-17671

### Improvements and New Features

- Add support for new authentication algorithms (SHA-224, SHA-256, SHA-384 and
  SHA-512), according to RFC 7860.

  Own Id: OTP-17615 Aux Id: MR9501-1

- Improve debug info for (snmp) manager.

  Own Id: OTP-17783

## SNMP 5.10.1

### Fixed Bugs and Malfunctions

- Sockets are monitored, but the handling of the 'DOWN' message expected a new
  style socket ('socket'), old style (port) was not handled.

  Own Id: OTP-17641 Aux Id: OTP-17640

## SNMP 5.10

### Improvements and New Features

- It is now possible to configure the built-in net-if processes (both agent and
  manager) to use the new (gen_udp-) option 'inet_backend'.

  Own Id: OTP-17526

## SNMP 5.9.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## SNMP 5.9

### Improvements and New Features

- Removed deprecated functions marked for removal.

  Own Id: OTP-17049

- Removed timestamps from files generated by `snmp` to enable deterministic
  builds.

  Own Id: OTP-17354

- Fixed warnings in code matching on underscore prefixed variables.

  Own Id: OTP-17385 Aux Id: OTP-17123

## SNMP 5.8.0.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## SNMP 5.8

### Improvements and New Features

- Add function to get a list of configured agent transports. Also improved agent
  info with regards to transports.

  Own Id: OTP-17109 Aux Id: ERIERL-583

## SNMP 5.7.3

### Fixed Bugs and Malfunctions

- \[manager] In a function handling snmp errors, an unused result (\_Error)
  could result in matching issues and therefore case clause runtime errors
  (crash). Note that this would only happen in _very_ unusual error cases.

  Own Id: OTP-17161

## SNMP 5.7.2

### Fixed Bugs and Malfunctions

- \[manager] Misspelled priv protocol (atom) made it impossible to update usm
  user 'priv_key' configuration for usmAesCfb128Protocol via function calls.

  Own Id: OTP-17110 Aux Id: ERIERL-586

## SNMP 5.7.1

### Fixed Bugs and Malfunctions

- Fixed usage of `AC_CONFIG_AUX_DIRS()` macros in configure script sources.

  Own Id: OTP-17093 Aux Id: ERL-1447, PR-2948

## SNMP 5.7

### Fixed Bugs and Malfunctions

- If an attempt was made to send a v1 trap on a IPv6 transport this could cause
  a master agent crash (if the agent was _not_ multi-threaded).

  Own Id: OTP-16920 Aux Id: OTP-16649

- The deprecation info for a couple of the deprecated MIB compiler functions
  where incorrect. Referred to functions in the 'snmpa' module instead of
  'snmpc'.

  Own Id: OTP-17056 Aux Id: OTP-17049

### Improvements and New Features

- Make it possible for the agent to configure separate transports (sockets) for
  request-responder and trap-sender.

  Own Id: OTP-16649

- The mib server cache handling has been improved. First, the default gclimit
  has been changed from 100 to infinity (to ensure the size is as small as
  possible). Also, the method of removing old elements has been optimized.

  Own Id: OTP-16989 Aux Id: ERIERL-544

- It is now possible to configure the agent in such a way that the order of
  outgoing notifications are processed in order in the agent. What happens after
  the notification message has left the agent (been sent) is of course still out
  of our control.

  Own Id: OTP-17022 Aux Id: ERIERL-492

- Improve handling of the udp_error message. Basically an improved error/warning
  message.

  Own Id: OTP-17033

## SNMP 5.6.1

### Fixed Bugs and Malfunctions

- For agent fix PrivParams for SNMPv3 USM with AES privacy, as earlier fixed for
  the manager in OTP_16541.

  Own Id: OTP-15130 Aux Id: ERIERL-524, OTP-16541

- The SNMP Agent missed to re-activate datagram reception in an odd timeout case
  and went deaf. This bug has been fixed.

  Own Id: OTP-15767 Aux Id: ERIERL-523

- Use of deprecated functions in example 2 has been removed (no more compiler
  warnings).

  Own Id: OTP-16716

- A file descriptor leak has been plugged. When calling the reconfigure function
  of a mib, it opened the config file(s) but never closed them on successful
  read.

  Own Id: OTP-16760 Aux Id: ERIERL-511

## SNMP 5.6

### Fixed Bugs and Malfunctions

- For manager, fix PrivParams for SNMPv3 USM with AES privacy; _ In
  \`snmp_usm:do_decrypt/3\`, pass full UsmSecParams to
  \`snmp_usm:try_decrypt/5\` as expected by AES clause. _ Change
  \`snmpm_usm:aes_encrypt/3\` to use EngineBoots and EngineTime as cached by
  \`snmpm_config:get_usm_eboots/1\` and \`snmpm_config:get_usm_etime/1\` instead
  of \`snmpm_config:get_engine_boots/0\` and \`snmpm_config:get_engine_time/0\`.
  This ensures correct msgPrivacyParameters are sent when AES is used. \* Add
  test \`snmp.snmp_manager_SUITE.usm_priv_aes/1\` to avoid regression.

  Own Id: OTP-16541 Aux Id: #2544

- Invalid character in (manager) usm config entry generator function.

  Own Id: OTP-16552 Aux Id: ERL-1196

### Improvements and New Features

- Remove usage and documentation of old requests of the I/O-protocol.

  Own Id: OTP-15695

- Calls of deprecated functions in the
  [Old Crypto API](`e:crypto:new_api.md#the-old-api`) are replaced by calls of
  their [substitutions](`e:crypto:new_api.md#the-new-api`).

  Own Id: OTP-16346

- Finalize deprecation. Already deprecated functions has a "remove version 24"
  set and "new" functions added to list of deprecated functions.

  Own Id: OTP-16463

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

## SNMP 5.5.0.5

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## SNMP 5.5.0.4

### Improvements and New Features

- The mib server cache handling has been improved. First, the default gclimit
  has been changed from 100 to infinity (in order to ensure the size is as small
  as possible). Also the method of removing old elements has been optimized.

  Own Id: OTP-16989 Aux Id: ERIERL-544

- It is now possible to configure the agent in such a way that the order of
  outgoing notifications are processed in order in the agent. What happens after
  the notification message has left the agent (been sent) is of course still out
  of our control.

  Own Id: OTP-17022 Aux Id: ERIERL-492

## SNMP 5.5.0.3

### Fixed Bugs and Malfunctions

- For agent fix PrivParams for SNMPv3 USM with AES privacy, as earlier fixed for
  the manager in OTP_16541.

  Own Id: OTP-15130 Aux Id: ERIERL-524, OTP-16541

## SNMP 5.5.0.2

### Fixed Bugs and Malfunctions

- The SNMP Agent missed to re-activate datagram reception in an odd timeout case
  and went deaf. This bug has been fixed.

  Own Id: OTP-15767 Aux Id: ERIERL-523

## SNMP 5.5.0.1

### Fixed Bugs and Malfunctions

- A file descriptor leak has been plugged. When calling the reconfigure function
  of a mib, it opened the config file(s) but never closed them on successful
  read.

  Own Id: OTP-16760 Aux Id: ERIERL-511

## SNMP 5.5

### Improvements and New Features

- A simple supervision of the snmp manager net-if process has been added. Also,
  a way to forcibly restart the net-if process has been added. This could be
  useful if the net-if process hangs for some reason.

  Own Id: OTP-16447 Aux Id: ERIERL-455, OTP-16382

- Misc documentation corrections

  Own Id: OTP-16450

## SNMP 5.4.5

### Improvements and New Features

- Its now possible to remove selected varbinds (from the final message) when
  sending a notification. This is done by setting the 'value' (in the varbind(s)
  of the varbinds list) to '?NOTIFICATION_IGNORE_VB_VALUE'.

  Own Id: OTP-16349 Aux Id: ERIERL-444

- Its now possible to specify that an oid shall be "truncated" (trailing ".0" to
  be removed) when sending an notification.

  Own Id: OTP-16360 Aux Id: ERIERL-451

## SNMP 5.4.4

### Improvements and New Features

- \[manager] The callbacks where executed in a (new) 'temporary' process, that
  executed the callback call and then exited. This has now been made
  configurable so that is also possible to specify a 'permanent' callback proxy
  process. All callback calls will then be executed in this (permanent) process
  (in sequence).

  Own Id: OTP-15947 Aux Id: ERIERL-378

## SNMP 5.4.3.1

### Improvements and New Features

- Its now possible to remove selected varbinds (from the final message) when
  sending a notification. This is done by setting the 'value' (in the varbind(s)
  of the varbinds list) to '?NOTIFICATION_IGNORE_VB_VALUE'.

  Own Id: OTP-16349 Aux Id: ERIERL-444

- Its now possible to specify that an oid shall be "truncated" (trailing ".0" to
  be removed) when sending an notification.

  Own Id: OTP-16360 Aux Id: ERIERL-451

## SNMP 5.4.3

### Fixed Bugs and Malfunctions

- Agent discovery cleanup. If there is no receiver of INFORM then #state.reqs in
  snmpa_net_if keeps on growing for DISCOVERY.

  Own Id: OTP-16228 Aux Id: ERIERL-427

## SNMP 5.4.2

### Fixed Bugs and Malfunctions

- The agent discovery process has been made to work with snmptrapd..

  Own Id: OTP-16207 Aux Id: ERIERL-427

## SNMP 5.4.1

### Improvements and New Features

- Made it possible to add 'extra socket options' to the (gen_udp) socket open
  call (for both manager and agent). A new option has been added,
  extra_sock_opts, which makes it possible for the user to add a list of extra
  socket options that will be appended to the other socket options for the open
  call. See the snmp application config man page (erl -man 6 snmp) or the
  "Configuring the application" chapter of the Users Guide for more info.

  Own Id: OTP-16092 Aux Id: ERIERL-410

## SNMP 5.4

### Fixed Bugs and Malfunctions

- Fix various minor issues related to Dialyzer. Mostly these are dialyzer
  warnings, but there was also some minor bugs detected by Dialyzer.

  Own Id: OTP-15932

### Improvements and New Features

- Fixed a dets usage problem detected by dialyzer.

  Own Id: OTP-10400 Aux Id: kunagi-253 \[164]

- The function snmp:print_version_info() prints various version info. For each
  module a number of items are printed, such as app vsn and md5 digest. And an
  attempt was also made to print "compile time". This used to be available in
  the module_info for each module, but has now been removed.

  Own Id: OTP-15330

- The use of the deprecated random module has been replaced the with rand
  module.

  Own Id: OTP-15331

- Removed use of the deprecated function erlang:get_stacktrace(). Instead make
  use of the 'catch Class:Error:Stacktrace' feature.

  Own Id: OTP-15332

## SNMP 5.3

### Improvements and New Features

- The application otp_mibs has been removed from OTP. Some of its components
  (mibs) have been moved to other apps (snmp), or removed completely (os_mon).

  Own Id: OTP-14984 Aux Id: OTP-15329

- \[snmp|agent] Add a get-mechanism callback module (and a corresponding
  behaviour). The agent calls this module to handle each get (get, get-next and
  get-bulk) request.

  Own Id: OTP-15691 Aux Id: ERIERL-324

## SNMP 5.2.12

### Fixed Bugs and Malfunctions

- Conversion of (agent) Audit Trail Log (ATL) failed due to invalid log entries.

  The conversion aborted completely midway because the ATL contained invalid
  entries. The conversion has been improved so that it now firstly handles
  encountered errors and write an informative message (into the converted
  stream) and secondly keeps count of the number of successful or failed entry
  conversions. See [log_to_txt](`snmpa:log_to_txt/8`) for more info.

  The reason the ATL contained invalid entries have also been fixed. The reason
  was that for some outgoing messages (not response):

  - encrypted (v3 messages)

    Was logged "as is" (encrypted) without the info to decrypt, making
    conversion impossible (which was the reason the log contained bad entries).

  - un-encrypted

    Was not logged at all.

  Own Id: OTP-15287 Aux Id: ERIERL-206

- \[compiler] Spurious version message removed. The snmp mib compiler printed an
  spurious version message if the 'version' option was provided.

  Own Id: OTP-15290

## SNMP 5.2.11.2

### Improvements and New Features

- \[manager] The callbacks where executed in a (new) 'temporary' process, that
  executed the callback call and then exited. This has now been made
  configurable so that is also possible to specify a 'permanent' callback proxy
  process. All callback calls will then be executed in this (permanent) process
  (in sequence).

  Own Id: OTP-15947 Aux Id: ERIERL-378

## SNMP 5.2.11.1

### Improvements and New Features

- \[snmp|agent] Add a get-mechanism callback module (and a corresponding
  behaviour). The agent calls this module to handle each get (get, get-next and
  get-bulk) request.

  Own Id: OTP-15691 Aux Id: ERIERL-324

## SNMP 5.2.11

### Fixed Bugs and Malfunctions

- The Snmp MIB compiler now allows using a TEXTUAL-CONVENTION type before
  defining it.

  Own Id: OTP-14196 Aux Id: ERIERL-161

## SNMP 5.2.10

### Fixed Bugs and Malfunctions

- The example MIB EX1-MIB in the SNMP application has been corrected to match
  its example.

  Own Id: OTP-14204 Aux Id: PR-1726

## SNMP 5.2.9

### Fixed Bugs and Malfunctions

- Removed all old unused files in the documentation.

  Own Id: OTP-14475 Aux Id: ERL-409, PR-1493

## SNMP 5.2.8

### Fixed Bugs and Malfunctions

- The `recbuf` configuration option was not propagated correctly to the socket
  for the SNMP Manager.

  Own Id: OTP-13372 Aux Id: ERIERL-73

## SNMP 5.2.7

### Fixed Bugs and Malfunctions

- A bug in the SNMP MIB compiler has been fixed. An AUGMENTS referring to a
  table defined later in the MIB did not work.

  Own Id: OTP-13014 Aux Id: ERL-375

## SNMP 5.2.6

### Fixed Bugs and Malfunctions

- Internal code change: Calls to `catch` followed by a call to
  `erlang:get_stacktrace/0` has been rewritten to use `try` instead of `catch`
  to make the code future-proof.

  Own Id: OTP-14400

## SNMP 5.2.5

### Fixed Bugs and Malfunctions

- The SNMP MIB compiler has been fixed to compile MIBS with refinements on user
  types such as in RFC 4669 RADIUS-AUTH-SERVER-MIB.mib. Problem reported and
  researched by Kenneth Lakin and Daniel Goertzen.

  See also: https://bugs.erlang.org/browse/ERL-325

  Own Id: OTP-14145 Aux Id: ERL-325

## SNMP 5.2.4

### Fixed Bugs and Malfunctions

- Correct bugs when path to mib or idl spec files contains UTF-8 characters.

  Own Id: OTP-13718 Aux Id: ERL-179

### Improvements and New Features

- Solves snmp config string handling as reported by ERL-164 and solved by
  PR-1100

  Own Id: OTP-13706

## SNMP 5.2.3

### Improvements and New Features

- Internal changes

  Own Id: OTP-13551

## SNMP 5.2.2

### Fixed Bugs and Malfunctions

- Snmp agent now properly handles `vacmViewTreeFamily` masks.

  Own Id: OTP-13264

## SNMP 5.2.1

### Fixed Bugs and Malfunctions

- Small documentation fixes

  Own Id: OTP-13017

### Improvements and New Features

- Update configuration check of imask ( list of ones and zeros) to allow the
  empty list.

  Own Id: OTP-13101

## SNMP 5.2

### Improvements and New Features

- The runtime dependencies in the application resource file have been updated.

  Own Id: OTP-12762

## SNMP 5.1.2

### Fixed Bugs and Malfunctions

- A bug in the SNMP Agent has been corrected; when opening a port using the
  command line argument -snmpa_fd the Port should be 0 when calling
  gen_udp:open.

  A bug in the SNMP manager has been corrected; it should not look at the
  -snmp_fd command line argument, but instead at -snmpm_fd.

  Own Id: OTP-12669 Aux Id: seq12841

### Improvements and New Features

- Improved cryptographic capability.

  Own Id: OTP-12452

## SNMP Development Toolkit 5.1.1

Version 5.1.1 supports code replacement in runtime from/to version 5.1.

### Improvements and new features

- \[compiler] Refinement of type Opaque was not allowed.

  MIB constructs such as '`SYNTAX Opaque (SIZE(0..65535))`' was previously not
  allowed, see the standard `ALARM-MIB` for eaxmple.

  Own Id: OTP-12066

  Aux Id: Seq 12669

### Fixed Bugs and Malfunctions

-

### Incompatibilities

-

## SNMP 5.1

### Improvements and New Features

- The SNMP manager has been enhanced with dual stack IPv4+IPv6, as the agent
  just was. The documentation is also now updated for both the agent and the
  manager.

  Own Id: OTP-12108 Aux Id: OTP-12020

## SNMP 5.0

### Improvements and New Features

- SNMP has been improved to handle IPv6. The agent can handle dual stack IPv4 +
  IPv6, but not yet the manager. The documentation also still lags behind... If
  you do such advanced stuff like writing a custom net_if module, the interface
  for it has changed, but other than that SNMP is backwards compatible.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12020 Aux Id: OTP-11518

-
