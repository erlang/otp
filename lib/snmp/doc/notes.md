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

## SNMP 4.25.1

### Fixed Bugs and Malfunctions

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

## SNMP Development Toolkit 4.25.0.1

Version 4.25.0.1 supports code replacement in runtime from/to version 4.25,
4.24.2, 4.24.1 and 4.24.

### Improvements and new features

- Updated doc files to utf8.

  Own Id: OTP-10907

- Fixed test suite to support UTF-8 paths.

  Own Id: OTP-10877

### Fixed Bugs and Malfunctions

-

### Incompatibilities

-

## SNMP Development Toolkit 4.25

Version 4.25 supports code replacement in runtime from/to version 4.24.2,
4.24.1, 4.24, 4.23.1 and 4.23.

### Improvements and new features

- \[agent] Enable SNMP to create missing database directories.

  Add [\{db_init_error, create_db_and_dir\}](snmp_config.md#db_init_error)
  option to SNMP [manager](snmp_config.md#manager_opts_and_types) and
  [agent](snmp_config.md#agent_opts_and_types). This allows them to create any
  missing parent directories for `db_dir`, rather than treating any missing
  directories as a fatal error. The default for `db_init_error`, which is
  `terminate`, is unchanged.

  Steve Vinoski

  Own Id: OTP-11352

- \[manager] Improved handling of unexpected return values from `m:snmpm_user`
  callback functions.

  Violations of the documented API (crashes or invalid return values) will now
  result in an error message.

  Own Id: OTP-11307

- Add (atl) log conversion block option.

  It is now possible to request that the Audit Trail Log should be blocked
  during conversion (`log_to_txt` or `log_to_io`). This could be useful when
  converting an entire large log (when there is a chance it may otherwise wrap
  during conversion).

  See agent [log_to_txt](`snmpa:log_to_txt/8`) and
  [log_to_io](`snmpa:log_to_io/7`) and also manager
  [log_to_txt](`snmpm:log_to_txt/8`) and [log_to_io](`snmpm:log_to_io/7`) for
  details.

  Own Id: OTP-11396

  Own Id: seq12433

- When converting an Audit Trail Log to text, a corrupt log entry could cause
  the entire conversion to fail.

  Also, for a log with sequence numbers, failing to decode a log entry would
  cause the conversion to fail (not because of the failed decode, but because of
  the failure to write the error message).

  Own Id: OTP-111453

  Aux Id: Seq 12459

### Fixed Bugs and Malfunctions

- Wrong block cypher type used for AES ('aes_cbf128' instead of 'aes_cfb128')
  when performing AES block encrypt/decrypt which breaks SNMP
  usmAesCfb128Protocol in agent and manager.

  Own Id: OTP-11412

- \[manager] When performing the AES encryption, invalid values for the
  EngineBoots and EngineTime was used.

  The values of the local agent was used, which would have produced "some"
  values if an agent was actually running. If not it would have caused a crash.

  Own Id: OTP-11413

### Incompatibilities

-

## SNMP Development Toolkit 4.24.2

Version 4.24.2 supports code replacement in runtime from/to version 4.24.1,
4.24, 4.23.1 and 4.23.

### Improvements and new features

- \[agent] Improved documentation for the functions for loading and unloading
  mibs, see [load_mibs](`snmpa:load_mibs/3`) and
  [unload_mibs](`snmpa:unload_mibs/3`) for more info.

  Also added new functions for loading and unloading a single mib, see
  [load_mib](`snmpa:load_mib/2`) and [unload_mib](`snmpa:unload_mib/2`) for more
  info.

  Own Id: OTP-11216

### Fixed Bugs and Malfunctions

-

### Incompatibilities

-

## SNMP Development Toolkit 4.24.1

Version 4.24.1 supports code replacement in runtime from/to version 4.24, 4.23.1
and 4.23.

### Improvements and new features

-

### Fixed Bugs and Malfunctions

- \[agent] Reading the value of the vacmViewTreeFamilyMask returns it in the
  wrong (internal bitlist) format.

  The vacmViewTreeFamilyMask is defined as a bit string in the MIB (OCTET
  STRING). Internally a bitlist (list of 1's and 0's, see
  [vacm config file](snmp_agent_config_files.md#vacm) for more info) is used.
  However, the MIB implementation assumed the latter, effectively rendering all
  attempts to read/set masks via SNMP unsuccessful.

  Since the mask is used in hot paths (e.g. access permission checks for each
  SNMP operation, the bitlist representation of the mask has benefits (e.g.
  faster processing). Reading/writing the view mask objects is less
  time-critical. Therefore, to fix the issue, convert between the bitlist
  (internal) representation and bitstring (external) when the
  vacmViewTreeFamilyMask objects are accessed.

  Also, the check of the vacm config file was invalid with regard to the mask
  value. It was assumed to be a proper oid, which is not strictly the case (see
  bitlist above).

  Own Id: OTP-11177

  Stefan Zegenhagen

- \[agent] The counter increment function in the local-db was incorrect. It did
  not handle counter wrap correctly.

  Own Id: OTP-11192

### Incompatibilities

-

## SNMP Development Toolkit 4.24

Version 4.24 supports code replacement in runtime from/to version 4.23.1 and
4.23.

### Improvements and new features

- \[agent,manager] Updated to support the new crypto interface.

  Own Id: OTP-11009

- \[agent] Introduced a documented behaviour for the mib-server
  [mib-data backend](`m:snmpa_mib_data`). At present only the default module
  (`snmpa_mib_data_tttn`) is provided.

  A config option for the (agent) [mib-servers](snmp_config.md#agent_mib_server)
  mib-data backend module has been added to the agent config options,
  [data_module](snmp_config.md#agent_ms_data_module).

  Own Id: OTP-11101

- \[agent] Introduced a documented behaviour for the
  [mib storage](`m:snmpa_mib_storage`). At present there are three simple
  modules (`snmpa_mib_storage_ets`, `snmpa_mib_storage_dets` and
  `snmpa_mib_storage_mnesia`) implementing this behaviour, provided with the
  app.

  A config option for the (agent)
  [mib storage](snmp_config.md#agent_mib_storage) has been added to the agent
  config options.

  Own Id: OTP-11107

### Fixed Bugs and Malfunctions

-

### Incompatibilities

-

## SNMP Development Toolkit 4.23.1

Version 4.23.1 supports code replacement in runtime from/to version 4.23.

### Improvements and new features

-

### Fixed Bugs and Malfunctions

- \[compiler] Now handles MIBs importing the pesudotype BITS.

  Own Id: OTP-10799

- \[compiler] The MIB compiler could not handle a table index that was defined
  later in the MIB.

  Own Id: OTP-10808

### Incompatibilities

-

## SNMP Development Toolkit 4.23

### Improvements and new features

- \[manager] Polish return values of snmpm_user_default according to snmpm_user
  doc.

  Luca Favatella

  Own Id: OTP-10671

- \[agent] Remove runtime warning in snmpa_agent because of tuple fun usage.

  Luca Favatella

  Own Id: OTP-10672

- \[manager] SNMP manager performance optimization.

  Ivan Dubovik

  Own Id: OTP-10673

### Fixed Bugs and Malfunctions

-

### Incompatibilities

- \[manager] The old Addr-and-Port based API functions, previously long
  deprecated and marked for deletion in R16B, has now been removed.

  Own Id: OTP-10027

## SNMP Development Toolkit 4.22.1

Version 4.22.1 supports code replacement in runtime from/to version 4.22, 4.21.7
4.21.6 4.21.5, 4.21.4, 4.21.3, 4.21.2, 4.21.1 and 4.21.

### Improvements and new features

- \[agent] Semantic fixes to SNMP-USER-BASED-SM-MIB. The semantics allow the
  `usmUserAuthKeyChange` and `usmUserPrivKeyChange` objects to be written to in
  the same set requests that also creates and clones the user. This was not
  possible beforehand, causing test tools checking semantic SNMPv3 behaviour to
  fail on a lot of test cases.

  Furthermore, once the user has been cloned by writing to an instance of
  `usmUserCloneFrom`, further set-operations to the same object will not return
  an error, but be no-ops. Especially, it must be avoided to copy security
  parameters again (possibly even from a different user).

  Stefan Zegenhagen

  Own Id: OTP-10166

- \[agent] Errors in `vacmAccessTable` RowStatus handling. There are problems
  with the handling of vacmAccessTableStatus that cause some SNMP test suites to
  report errors. Most notably, erroneous set operations frequently cause
  "genErr" errors to be returned. These "genErr" errors are usually caused by
  badmatch exceptions coming from `{ok, Row} = snmpa_vacm:get_row(RowIndex)` if
  the row does not exist.

  The semantics of the RowStatus handling in that table has been adjusted to be
  compliant with the RowStatus textual description of SNPMv2-TC MIB.

  Stefan Zegenhagen

  Own Id: OTP-10164

### Fixed Bugs and Malfunctions

- \[agent] Fix walk over vacmAccessTable. Fix the get_next implementation of
  vacmAccessTable to return all table entries.

  The get_next implementation of vacmAccessTable did not return all available
  table data. Instead, it only returned the first column for each row, and all
  columns for the last row available.

  Stefan Zegenhagen

  Own Id: OTP-10165

- \[manager] `snmpm:log_to_io/6` did not use the LogName argument.

  Own Id: OTP-10066

- Incorrect TimeTicks decode. Also bad handling of invalid encode (value outside
  of value range) for both `TimeTicks` and `Unsigned32`.

  Own Id: OTP-10132

### Incompatibilities

-

## SNMP Development Toolkit 4.22

Version 4.22 supports code replacement in runtime from/to version 4.21.7 4.21.6
4.21.5, 4.21.4, 4.21.3, 4.21.2, 4.21.1 and 4.21.

### Improvements and new features

- \[compiler] The table information the MIB compiler provides with augmented
  tables has been extended with `nbr_of_cols`, `first_accessible` and
  `not_accessible`.

  Own Id: OTP-9969

- Added the `log_to_io` audit-trail-log converter function to the api modules of
  both the [manager](`snmpm:log_to_io/7`) and [agent](`snmpa:log_to_io/7`).

  Own Id: OTP-9940

- \[manager] Introduced a new transport module, `snmpm_net_if_mt`, which handles
  all incoming and outgoing traffic in newly created processes. The
  message/request is processed and then the process exits.

  Own Id: OTP-9876

- \[agent] Documenting previously existing but undocumented function,
  [snmp_generic:get_table_info/2](`m:snmp_generic#get_table_info`).

  Own Id: OTP-9942

- \[agent] Improve error handling while reading agent config files. Some files
  contain mandatory information and is therefore themself mandatory.

  Own Id: OTP-9943

### Fixed Bugs and Malfunctions

-

### Incompatibilities

-

## SNMP Development Toolkit 4.21.7

Version 4.21.7 supports code replacement in runtime from/to version 4.21.6,
4.21.5, 4.21.4, 4.21.3, 4.21.2, 4.21.1, 4.21, 4.20.1 and 4.20.

### Improvements and new features

-

### Fixed Bugs and Malfunctions

- \[agent] Simultaneous [snmpa:backup/1,2](`snmpa:backup/1`) calls can
  interfere. The master agent did not check if a backup was already in progress
  when a backup request was accepted.

  Own Id: OTP-9884

  Aux Id: Seq 11995

### Incompatibilities

-

## SNMP Development Toolkit 4.21.6

Version 4.21.6 supports code replacement in runtime from/to version 4.21.5,
4.21.4, 4.21.3, 4.21.2, 4.21.1, 4.21, 4.20.1 and 4.20.

### Improvements and new features

- \[agent] DoS attack using GET-BULK with large value of MaxRepetitions. A
  preventive method has been implementing by simply limit the number of varbinds
  that can be included in a Get-BULK response message. This is specified by the
  new config option, [gb_max_vbs](snmp_config.md#agent_gb_max_vbs).

  Own Id: OTP-9700

### Fixed Bugs and Malfunctions

- \[agent] Mib server cache gclimit update function incorrectly calls age update
  function. The gclimit update function,
  [update_mibs_cache_gclimit/1](`snmpa:update_mibs_cache_gclimit/1`),
  _incorrectly_ called the age update function,
  [update_mibs_cache_age/2](`snmpa:update_mibs_cache_age/2`).

  Johan Claesson

  Own Id: OTP-9868

### Incompatibilities

-

## SNMP Development Toolkit 4.21.5

Version 4.21.5 supports code replacement in runtime from/to version 4.21.4,
4.21.3, 4.21.2, 4.21.1, 4.21, 4.20.1 and 4.20.

### Improvements and new features

- \[agent] Removed (more) use of old style tuple funs.

  Own Id: OTP-9783

### Fixed Bugs and Malfunctions

- \[agent] Repeated vacm table dumping fails due to file name conflict. When
  dumping the vacm table to disk, a temoporary file with a fixed name was used.
  If the table dumping (snmpa_vacm:dump_table/0) was initiated from several
  different processes in rapid succession, the dumping could fail because the
  different processes was simultaneously trying to write to the same file. This
  problem has been eliminated by creating a unique name for the temporary file.

  Own Id: OTP-9851

  Aux Id: Seq 11980

### Incompatibilities

-

## SNMP Development Toolkit 4.21.4

This version has never been released for R14B.

Version 4.21.4 supports code replacement in runtime from/to version 4.21.3,
4.21.2, 4.21.1, 4.21, 4.20.1, 4.20 and 4.19.

### Improvements and new features

-

### Fixed Bugs and Malfunctions

- \[agent] Removed use of old style tuple funs.

  Own Id: OTP-9779

### Incompatibilities

-

## SNMP Development Toolkit 4.21.3

Version 4.21.3 supports code replacement in runtime from/to version 4.21.2,
4.21.1, 4.21, 4.20.1, 4.20 and 4.19.

### Improvements and new features

- \[compiler] Improved version info printout from the
  [MIB compiler frontend escript](snmpc_cmd.md).

  Own Id: OTP-9618

### Fixed Bugs and Malfunctions

- \[agent] Version 4.20 introduced a change that broke trap sending from
  subagents. Due to a bug in the test code, this was not discovered, until that
  bug was fixed.

  Own Id: OTP-9745

- \[agent] When sending an error message (reply) regarding
  `snmpUnknownPDUHandlers`, the agent used the wrong OID.

  Own Id: OTP-9747

- \[compiler] Fix the `--warnings/--W` option parsing in the
  [snmpc](snmpc_cmd.md#option_warnings) wrapper (e)script. The short warning
  option was incorrectly `--w`, instead of as documented `--W`. This has now
  been corrected.

  \*** POTENTIAL INCOMPATIBILITY \***

  Tuncer Ayaz

  Own Id: OTP-9718

### Incompatibilities

- \[compiler] The short warning option has been changed from `--w` to `--W` to
  comply with the documentation.

  Tuncer Ayaz

  Own Id: OTP-9718

## SNMP Development Toolkit 4.21.2

Version 4.21.2 supports code replacement in runtime from/to version 4.21.1,
4.21, 4.20.1, 4.20 and 4.19.

### Improvements and new features

-

### Fixed Bugs and Malfunctions

- Bad note store GC timer deactivation. Wrong field in the state record was set
  (timeout instead active).

  Stefan Grundmann

  Own Id: OTP-9690

### Incompatibilities

-

## SNMP Development Toolkit 4.21.1

Version 4.21.1 supports code replacement in runtime from/to version 4.20.1, 4.20
and 4.19.

### Improvements and new features

- \[compiler] Used wrong variable name (for warnings-as-errors variable), which
  caused the compiler to crash when using the snmpc (e)script.

  Also added the option [\--Werror](snmpc_cmd.md#option_werror) for the SNMP MIB
  compiler (escript) frontend (to mimic [erlc](`e:erts:erlc_cmd.md`)), which
  specifies whether warnings should be treated as errors.

  Own Id: OTP-9447

- \[agent] Some very minor debugging improvements.

  Own Id: OTP-9446

### Fixed Bugs and Malfunctions

-

### Incompatibilities

-

## SNMP Development Toolkit 4.21

Version 4.21 supports code replacement in runtime from/to version 4.20.1, 4.20
and 4.19.

### Improvements and new features

- \[manager] There was no way to specify transport domain. The transport domains
  was assumed to be IPv4 (transportDomainUdpIpv4). This has now been changed so
  that it can also be IPv6 (transportDomainUdpIpv6). To facilitate this, the
  transport domain, `tdomain`, is now a (new) valid option when
  [registering](`snmpm:register_agent/3`) a new agent (and
  [updating](`snmpm:update_agent_info/4`) agent info).

  This also mean that the transport behaviour has changed.

  Own Id: OTP-9305

  Aux Id: Seq 11847

- \[compiler] Added the option [warnings_as_errors](`snmpc:compile/2`) (for the
  SNMP MIB compiler (escript) frontend, the option
  [\--wae](snmpc_cmd.md#option_wae) is used) which specifies whether warnings
  should be treated as errors.

  Tuncer Ayaz

  Own Id: OTP-9437

### Fixed Bugs and Malfunctions

- The snmp config tool could not handle (manager) audit trail config because the
  option seqno was not handled.

  Own Id: OTP-9354

- \[agent] The SNMP ACM cache was not properly updated when changes where made
  to the VACM security-to-group, access and view-tree-family tables.

  Own Id: OTP-9367

  Aux Id: Seq 11858

- Fixed install directory typo for man3.

  Peter Lemenkov

  Hans Ulrich Niedermann

  Own Id: OTP-9442

### Incompatibilities

-

## SNMP Development Toolkit 4.20.1

Version 4.20.1 supports code replacement in runtime from/to version 4.20, 4.19
and 4.18.

### Improvements and new features

-

### Fixed Bugs and Malfunctions

- \[agent] Did not handle transport domains properly in some cases, for instance
  trap sending.

  Own Id: OTP-9400

- \[agent] Wrong default transport domain, snmpUDPDomain, instead of
  transportDomainUdpIpv4.

  Own Id: OTP-9425

  Aux Id: Seq 11874

### Incompatibilities

-

## SNMP Development Toolkit 4.20

Version 4.20 supports code replacement in runtime from/to version 4.19 and 4.18.

### Improvements and new features

- \[agent] Added support for sending traps to IPv6 targets.

  See the [target address config file](snmp_agent_config_files.md#target_addr),
  the [target_addr_entry/11](`m:snmpa_conf#target_addr_entry`) function or
  [add_addr/11](`snmp_target_mib:add_addr/11`) for more info.

  Own Id: OTP-9088

  Aux Id: Seq 11790

- \[agent] To be able to handle multiple engine-id(s) when sending trap(s), the
  function [add_community/6](`m:snmp_community_mib#add_community`) has been
  added.

  Own Id: OTP-9119

  Aux Id: Seq 11792

- \[manager] The API for snmp requests has been augmented to allow the caller to
  override some configuration.

  This has been done by introducing a new set of API functions, see
  [sync_get2/3,4](`snmpm:sync_get2/3`), [async_get2/3,4](`snmpm:async_get2/3`),
  [sync_get_next2/3,4](`snmpm:sync_get_next2/3`),
  [async_get_next2/3,4](`snmpm:async_get_next2/3`),
  [sync_get_bulk2/5,6](`snmpm:sync_get_bulk2/5`),
  [async_get_bulk2/5,6](`snmpm:async_get_bulk2/5`),
  [sync_set2/3,4](`snmpm:sync_set2/3`) and
  [async_set2/3,4](`snmpm:async_set2/3`) for more info.

  Own Id: OTP-9162

- \[manager] The old API functions (for get and set requests: snmpm:g/3,4,5,6,7,
  snmpm:ag/3,4,5,6,7, snmpm:gn/3,4,5,6,7, snmpm:agn/3,4,5,6,7,
  snmpm:s/3,4,5,6,7, snmpm:s/3,4,5,6,7, snmpm:gb/5,6,7,8,9 and
  snmpm:agb/5,6,7,8,9) are now officially deprecated. They will be removed as of
  R16B.

  Own Id: OTP-9174

- \[agent] Pass extra info through the agent to the net-if process when sending
  notifications.

  See [snmpa:send_notification2/3](`m:snmpa#send_notification2`) for more info.
  See also the incoming net-if messages when sending a
  [trap](snmp_agent_netif.md#im_send_pdu) (send_pdu message) and
  [notification](snmp_agent_netif.md#im_send_pdu_req) (send_pdu_req message).

  Own Id: OTP-9183

  Aux Id: Seq 11817

- Added type specs for functions that do not return.

  Kostis Sagonas

  Own Id: OTP-9208

### Fixed Bugs and Malfunctions

- Fixed endode/decode of values of type `Counter32`.

  This type (`Counter32`) is an unsigned integer 32, but is actually encoded as
  an signed integer 32. The encode/decode functions however, treated it as if it
  was encodeded as an unsigned integer 32.

  Own Id: OTP-9022

### Incompatibilities

-
