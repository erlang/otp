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
# Common Test Release Notes

## Common_Test 1.26.1

### Fixed Bugs and Malfunctions

* Fix how CT finds Erlang/OTP releases for compatability testing. This functionality is only used to test Erlang/OTP.

  Own Id: OTP-18932

## Common_Test 1.26

### Fixed Bugs and Malfunctions

- With this change, common_test returns an error when suite with a badly defined
  group is executed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18728 Aux Id: PR-7487, PR-7674

- With this change, stylesheet option is applied to all HTML report pages.

  Own Id: OTP-18760

- Update all <tt> html tags to be <code> instead.

  Own Id: OTP-18799 Aux Id: PR-7695

### Improvements and New Features

- This change fixes docs, so that historically deprecated ?config macro is no
  longer recommended to be used.

  Own Id: OTP-18858 Aux Id: PR-7825

## Common_Test 1.25.1

### Fixed Bugs and Malfunctions

- With this change, ct_hooks manual refers to CTH execution order section in
  user guide.

  Own Id: OTP-14480 Aux Id: ERIERL-43, OTP-11894, PR-7455

- With this change, Config data from pre_end_per_testcase hook is delivered to
  post_end_per_testcase callback in case of testcase timetrap or linked process
  crash.

  Own Id: OTP-18579 Aux Id: GH-7119

- With this change, remaining references to not supported vts tool in ct_run are
  removed (mainly relates to docs and ct_run help message).

  Own Id: OTP-18615 Aux Id: PR-7234

- With this change, prompt search functionality in ct_telnet handles unicode
  input.

  Own Id: OTP-18664 Aux Id: ERIERL-959

- Expanded the documentation about how to use the `standard_io`,
  `standard_error` and `user` I/O devices.

  Added the types [`io:standard_io/0`](`t:io:standard_io/0`),
  `io:standard:error/0` and [`io:user/0`](`t:io:user/0`).

  Own Id: OTP-18676 Aux Id: PR-7473 GH-7459

## Common_Test 1.25

### Fixed Bugs and Malfunctions

- This change improves Common Test docs (CT hook example code) and adds Emacs
  skeleton with hook code.

  Own Id: OTP-18377 Aux Id: PR-6437

### Improvements and New Features

- Updated common_test with a more robust way to fetch old releases, while
  ignoring the current release.

  Own Id: OTP-18259 Aux Id: PR-5924

- \- re-write the XML `ct` module documentation into erlang types to make
  Dialyzer able to catch more precise errors

  Own Id: OTP-18340

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

## Common_Test 1.24.0.2

### Fixed Bugs and Malfunctions

* Fix how CT finds Erlang/OTP releases for compatability testing. This functionality is only used to test Erlang/OTP.

  Own Id: OTP-18932

## Common_Test 1.24.0.1

### Fixed Bugs and Malfunctions

- With this change, prompt search functionality in ct_telnet handles unicode
  input.

  Own Id: OTP-18664 Aux Id: ERIERL-959

## Common_Test 1.24

### Improvements and New Features

- Renamed undocumented macro `CT_PEER/3` to `CT_PEER_REL/3`.

  Own Id: OTP-18460

## Common_Test 1.23.3

### Fixed Bugs and Malfunctions

- Change timeout to infinity for gen_server calls in cth_log_redirect

  Own Id: OTP-18363 Aux Id: ERIERL-879

## Common_Test 1.23.2

### Fixed Bugs and Malfunctions

- Fix starting of peer nodes on old releases when the compile server was active
  and the current Erlang installation contained non-latin1 characters in its
  path.

  Own Id: OTP-18255 Aux Id: PR-6314

## Common_Test 1.23.1

### Fixed Bugs and Malfunctions

- Fix cth_surefire to handle when a suite is not compiled with `debug_info`.
  This bug has been present since Erlang/OTP 25.0.

  Own Id: OTP-18208 Aux Id: ERIERL-852 PR-6229

### Improvements and New Features

- Common Test now preserves stack traces for throws.

  Own Id: OTP-18138 Aux Id: GH-5719, PR-6029

## Common_Test 1.23

### Fixed Bugs and Malfunctions

- Fix bug when running parallel test cases and together with one or more ct
  hooks that would cause the hook lock process to crash and produce printouts in
  the ct logs.

  Own Id: OTP-17881 Aux Id: PR-5581

### Improvements and New Features

- Input for `configure` scripts adapted to `autoconf` 2\.71.

  Own Id: OTP-17414 Aux Id: PR-4967

- Remove unused and undocumented tracer node functionality.

  Own Id: OTP-17676 Aux Id: PR-5021

- The new module `peer` supersedes the `slave` module. The `slave` module is now
  deprecated and will be removed in OTP 27.

  `peer` contains an extended and more robust API for starting erlang nodes.

  Own Id: OTP-17720 Aux Id: PR-5162

- The cth_surefire ct hook has been updated to include the file and line number
  of the executed test case in the xml output.

  The performance of the hook has also been improved greatly for test runs with
  many test cases.

  Own Id: OTP-17882 Aux Id: PR-5581

## Common_Test 1.22.1.2

### Fixed Bugs and Malfunctions

* Fix how CT finds Erlang/OTP releases for compatability testing. This functionality is only used to test Erlang/OTP.

  Own Id: OTP-18932

## Common_Test 1.22.1.1

### Fixed Bugs and Malfunctions

- Change timeout to infinity for gen_server calls in cth_log_redirect

  Own Id: OTP-18363 Aux Id: ERIERL-879

## Common_Test 1.22.1

### Fixed Bugs and Malfunctions

- OTP internal test fix.

  Own Id: OTP-17888

## Common_Test 1.22

### Improvements and New Features

- Before this change, group handling grammar was ambiguous and also group paths
  did not support test specs.

  Own Id: OTP-17664 Aux Id: GH-5088, PR-5242

- Before this change, it was not possible to link to a particular header entry
  in Common Test log. Change adds right aligned anchor icons in HTML test logs.

  Own Id: OTP-17790 Aux Id: PR-5375

## Common_Test 1.21

### Improvements and New Features

- Float allowed as multiply_timetraps parameter.

  Own Id: OTP-17413 Aux Id: PR-4767

- Remove usage of legacy API macro and functions.

  Own Id: OTP-17632 Aux Id: PR-5022

## Common_Test 1.20.5

### Fixed Bugs and Malfunctions

- An incoming NETCONF notification received before a call to
  ct_netconfc:create_subscription/\* caused the connection process to fail with
  badarg. Unexpected notifications are now logged in the same way as other
  unexpected messages.

  Own Id: OTP-17506

### Improvements and New Features

- Add 'receiver' option to ct_netconfc

  To allow a destination for incoming NETCONF notifications to be specified at
  sessions creation. Previously, a caller of create_subscription/\* became the
  destination, but RFC 5277 create-subscription is no longer the only way in
  which NETCONF notifications can be ordered.

  Own Id: OTP-17509

## Common_Test 1.20.4

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Common_Test 1.20.3

### Fixed Bugs and Malfunctions

- The option `release_shell` could crash when used together with the `spec`
  option.

  Own Id: OTP-16940 Aux Id: ERL-1335

### Improvements and New Features

- The experimental HiPE application has been removed, together with all related
  functionality in other applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16963

- Fixed warnings in code matching on underscore prefixed variables.

  Own Id: OTP-17385 Aux Id: OTP-17123

## Common_Test 1.20.2.3

### Fixed Bugs and Malfunctions

- OTP internal test fix.

  Own Id: OTP-17888

## Common_Test 1.20.2.2

### Fixed Bugs and Malfunctions

- An incoming NETCONF notification received before a call to
  ct_netconfc:create_subscription/\* caused the connection process to fail with
  badarg. Unexpected notifications are now logged in the same way as other
  unexpected messages.

  Own Id: OTP-17506

### Improvements and New Features

- Add 'receiver' option to ct_netconfc

  To allow a destination for incoming NETCONF notifications to be specified at
  sessions creation. Previously, a caller of create_subscription/\* became the
  destination, but RFC 5277 create-subscription is no longer the only way in
  which NETCONF notifications can be ordered.

  Own Id: OTP-17509

## Common_Test 1.20.2.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Common_Test 1.20.2

### Fixed Bugs and Malfunctions

- Before this change Config leaked between test groups in case of a subgroup was
  skipped (GH-3480).

  Own Id: OTP-17347 Aux Id: GH-3480,ERL-1439

## Common_Test 1.20.1

### Fixed Bugs and Malfunctions

- A race condition could cause ct_netconfc:open/_ to return a dysfunctional
  handle, resulting in errors when invoking other api functions on it, and
  making it impossible to establish a new connection to the server in question.
  Similar symptoms were possible with open/_ in modules ct_ssh and ct_telnet.

  Internal messages from common_test processes could be left in the caller's
  message queue after a timeout when invoking call/\* in modules ct_netconfc and
  ct_ssh. An internal process used by module ct_telnet could leak memory due to
  stray messages.

  Calls to ct_telnet:open/\* and ct_telnet:get_data/1 could hang indefinitely if
  the TCP connection to the server was lost.

  Own Id: OTP-17328 Aux Id: ERIERL-631

## Common_Test 1.20

### Improvements and New Features

- Various address sanitizer support.

  Own Id: OTP-16959 Aux Id: PR-2965

## Common_Test 1.19.1

### Improvements and New Features

- Add behaviour for test suites

  Own Id: OTP-17070

## Common_Test 1.19

### Improvements and New Features

- The function `ct_property_test:init_tool/1` is added for the cases when the
  user does not want ct_property_test to compile properties. init_tool/1 can be
  used to set the property_test_tool config value.

  Own Id: OTP-16029 Aux Id: PR-2145

- The built-in Common Test Hook, `cth_log_redirect`, has been updated to use the
  system `default` Logger handler's configuration instead of its own. See the
  section on [Built-in Hooks](ct_hooks_chapter.md#built-in-cths) in the Common
  Test User's Guide.

  Own Id: OTP-16273

- Calls of deprecated functions in the
  [Old Crypto API](`e:crypto:new_api.md#the-old-api`) are replaced by calls of
  their [substitutions](`e:crypto:new_api.md#the-new-api`).

  Own Id: OTP-16346

## Common_Test 1.18.2.2

### Fixed Bugs and Malfunctions

- OTP internal test fix.

  Own Id: OTP-17888

## Common_Test 1.18.2.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Common_Test 1.18.2

### Improvements and New Features

- Document incl_apps cover option

  Own Id: OTP-16039 Aux Id: ERL-795

- The `ct_property_test` has now a report function for results of stateful
  testing.

  Own Id: OTP-16340

- Don't hide error reasons from user

  Own Id: OTP-16364 Aux Id: PR-2480

## Common_Test 1.18.1

### Improvements and New Features

- The ct_property_test logging is improved.

  Own Id: OTP-16287

## Common_Test 1.18

### Fixed Bugs and Malfunctions

- If a ct hook is installed in the `suite/0` function in a test suite, then the
  hook's `terminate/1` function would be called several times without it's
  `init/2` function being called first. This is now corrected.

  Own Id: OTP-15863 Aux Id: ERIERL-370

- If `init_per_testcase` fails, the test itself is skipped. According to the
  documentation, it should be possible to change the result to failed in a hook
  function. The only available hook function in this case is
  `post_init_per_testcase`, but changing the return value there did not affect
  the test case result. This is now corrected.

  Own Id: OTP-15869 Aux Id: ERIERL-350

### Improvements and New Features

- Add ct_netconfc support for NETCONF 1.1 (RFC 6241). The 1.1 base capability
  can be sent in hello, and RFC 6242 chunk framing is applied when both client
  and server advertise 1.1 support.

  Own Id: OTP-15789

- Correct lib_dir paths in common_tests opaque data structure that is passed to
  ct_release_test callback modules in functions upgrade_init/2,
  upgrade_upgraded/2 and upgrade_downgraded/2. The incorrect paths may cause
  confusion when debugging although it will not cause any incorrect behavior on
  the part of common_test as it is currently not used.

  Own Id: OTP-15934

## Common_Test 1.17.3

### Fixed Bugs and Malfunctions

- All incorrect (that is, all) uses of "can not" has been corrected to "cannot"
  in source code comments, documentation, examples, and so on.

  Own Id: OTP-14282 Aux Id: PR-1891

### Improvements and New Features

- Use `ssh` instead of `rsh` as the default remote shell.

  Own Id: OTP-15633 Aux Id: PR-1787

## Common_Test 1.17.2.1

### Fixed Bugs and Malfunctions

- If a ct hook is installed in the `suite/0` function in a test suite, then the
  hook's `terminate/1` function would be called several times without it's
  `init/2` function being called first. This is now corrected.

  Own Id: OTP-15863 Aux Id: ERIERL-370

- If `init_per_testcase` fails, the test itself is skipped. According to the
  documentation, it should be possible to change the result to failed in a hook
  function. The only available hook function in this case is
  `post_init_per_testcase`, but changing the return value there did not affect
  the test case result. This is now corrected.

  Own Id: OTP-15869 Aux Id: ERIERL-350

## Common_Test 1.17.2

### Fixed Bugs and Malfunctions

- The test result when a hook function fails is in general the same as if the
  function that the hook is associated with fails. For example, if
  `post_init_per_testcase` fails the result is that the test case is skipped, as
  is the case when `init_per_testcase` fails.This, however, was earlier not true
  for timetrap timeouts or other error situations where the process running the
  hook function was killed. This is now corrected, so the error handling should
  be the same no matter how the hook function fails.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15717 Aux Id: ERIERL-334

- In some rare cases, when two common_test nodes used the same log directory, a
  timing problem could occur which caused common_test to crash because it's log
  cache file was unexpectedly empty. This is now corrected.

  Own Id: OTP-15758 Aux Id: ERIERL-342

### Improvements and New Features

- Two new common_test hook functions are introduced:

  `post_groups/2`, which is called after `Suite:groups/0`  
  `post_all/3`, which is called after `Suite:all/0`

  These functions allow modifying the return values from the `groups/0` and
  `all/0` functions, respectively.

  A new term, `{testcase,TestCase,RepeatProperties}` is now also allowed in the
  return from `all/0`. This can be used for repeating a single test case a
  specific number of times, or until it fails or succeeds once.

  Own Id: OTP-14746 Aux Id: ERIERL-143

## Common_Test 1.17.1

### Improvements and New Features

- OTP internal test improvements.

  Own Id: OTP-15716

## Common_Test 1.17

### Fixed Bugs and Malfunctions

- A bug caused `ct:encrypt_config_file/3` and `ct:decrypt_config_file/3` to fail
  with `badmatch` if input parameter `KeyOrFile` was `{key,string()}`. This is
  now corrected.

  Own Id: OTP-15540

- The status of a test case which failed with timetrap timeout in
  `end_per_testcase` could not be modified by returning `{fail,Reason}` from a
  `post_end_per_testcase` hook function. This is now corrected.

  Own Id: OTP-15584 Aux Id: ERIERL-282

### Improvements and New Features

- A new variant of the `newline` option to `ct_telnet:cmd/3` and
  `ct_telnet:send/3` is added, which allows to specify a string to append as
  newline indicator on a command. By default, the value is "\\n", but in some
  cases it is required to be "\\r\\n", which this option allows.

  A faulty regular expression given as parameter to `ct_telnet:expect/2,3` would
  earlier crash and look like an internal error in common_test. A better error
  indication is now given, but the test case will still fail.

  Own Id: OTP-15229 Aux Id: ERIERL-203

- Since the yang RFC allows more than one top element of config data in an
  `edit-config` element, `ct_netconfc:edit_config/3,4,5` can now take a list of
  XML elements.

  Own Id: OTP-15298

## Common_Test 1.16.1

### Fixed Bugs and Malfunctions

- The Logger handler cth_log_redirect earlier called the report callback
  (report_cb) before calling the logger formatter. In some cases this would
  fail, since cth_log_redirect could not handle report callbacks with two
  arguments. This is now corrected, so only the formatter will call the report
  callback.

  Own Id: OTP-15307

## Common_Test 1.16

### Improvements and New Features

- Use the compiler option `nowarn_export_all` to disable `export_all` warnings
  when automatically compiling test suites.

  Own Id: OTP-14810

- Use uri_string module instead of http_uri.

  Own Id: OTP-14902

## Common_Test 1.15.4.4

### Improvements and New Features

- The ct_property_test logging is improved.

  Own Id: OTP-16287

## Common_Test 1.15.4.3

### Fixed Bugs and Malfunctions

- If a ct hook is installed in the `suite/0` function in a test suite, then the
  hook's `terminate/1` function would be called several times without it's
  `init/2` function being called first. This is now corrected.

  Own Id: OTP-15863 Aux Id: ERIERL-370

- If `init_per_testcase` fails, the test itself is skipped. According to the
  documentation, it should be possible to change the result to failed in a hook
  function. The only available hook function in this case is
  `post_init_per_testcase`, but changing the return value there did not affect
  the test case result. This is now corrected.

  Own Id: OTP-15869 Aux Id: ERIERL-350

## Common_Test 1.15.4.2

### Fixed Bugs and Malfunctions

- The test result when a hook function fails is in general the same as if the
  function that the hook is associated with fails. For example, if
  `post_init_per_testcase` fails the result is that the test case is skipped, as
  is the case when `init_per_testcase` fails.This, however, was earlier not true
  for timetrap timeouts or other error situations where the process running the
  hook function was killed. This is now corrected, so the error handling should
  be the same no matter how the hook function fails.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15717 Aux Id: ERIERL-334

- In some rare cases, when two common_test nodes used the same log directory, a
  timing problem could occur which caused common_test to crash because it's log
  cache file was unexpectedly empty. This is now corrected.

  Own Id: OTP-15758 Aux Id: ERIERL-342

### Improvements and New Features

- Two new common_test hook functions are introduced:

  `post_groups/2`, which is called after `Suite:groups/0`  
  `post_all/3`, which is called after `Suite:all/0`

  These functions allow modifying the return values from the `groups/0` and
  `all/0` functions, respectively.

  A new term, `{testcase,TestCase,RepeatProperties}` is now also allowed in the
  return from `all/0`. This can be used for repeating a single test case a
  specific number of times, or until it fails or succeeds once.

  Own Id: OTP-14746 Aux Id: ERIERL-143

- OTP internal test improvements.

  Own Id: OTP-15716

## Common_Test 1.15.4.1

### Fixed Bugs and Malfunctions

- The status of a test case which failed with timetrap timeout in
  `end_per_testcase` could not be modified by returning `{fail,Reason}` from a
  `post_end_per_testcase` hook function. This is now corrected.

  Own Id: OTP-15584 Aux Id: ERIERL-282

## Common_Test 1.15.4.0.1

### Fixed Bugs and Malfunctions

- The status of a test case which failed with timetrap timeout in
  `end_per_testcase` could not be modified by returning `{fail,Reason}` from a
  `post_end_per_testcase` hook function. This is now corrected.

  Own Id: OTP-15584 Aux Id: ERIERL-282

## Common_Test 1.15.4

### Fixed Bugs and Malfunctions

- Fixed problem with 'skip_groups' in combination with 'all suites' option in
  test specification.

  Own Id: OTP-14953

## Common_Test 1.15.3

### Improvements and New Features

- A new function, `ct:remaining_test_procs/0`, returns the identity of test- and
  group leader processes that are still running at the time of the call.

  Own Id: OTP-13832

- A "latest test result" link is now displayed in the footer of each test index
  page, which performs a jump to the most recently generated test index. This is
  useful for making quick comparisons of results between test runs without
  having to traverse the log file tree.

  Own Id: OTP-14281

## Common_Test 1.15.2

### Improvements and New Features

- General Unicode improvements.

  Own Id: OTP-14462

## Common_Test 1.15.1

### Fixed Bugs and Malfunctions

- In OTP-20.0, the behavior of c, make, and ct_make was changed so that in some
  cases the beam files by default would be written to the directory where the
  source files were found. This is now changed back to the old behavior so beam
  files are by default written to current directory.

  Own Id: OTP-14489 Aux Id: ERL-438

## Common_Test 1.15

### Fixed Bugs and Malfunctions

- Errors in the documentation for user HTML stylesheets have been corrected.

  Own Id: OTP-14332 Aux Id: seq13299

- Internal code change: Calls to `catch` followed by a call to
  `erlang:get_stacktrace/0` has been rewritten to use `try` instead of `catch`
  to make the code future-proof.

  Own Id: OTP-14400

### Improvements and New Features

- The `ct_slave` modules now handle nodenames in the same way as nodenames
  passed to `-sname`. That means `ct_slave:start('b@127.0.0.1').` will now work.

  Own Id: OTP-13806

- Added the new option, `keep_logs`. If setting the value for this option to an
  integer, N, common_test will remove all ct_run.\* directories in the current
  log directory, except the N newest.

  Own Id: OTP-14179

- The existing `ct_netconfc:open/1,2` opens an SSH connection with one SSH
  channel realizing one Netconf session. To allow testing of multiple sessions
  over the same SSH connection, the following functions are added to
  `ct_netconfc`:

  \* `connect/1,2` \- establish an SSH connection _ `disconnect/1` \- close the
  given SSH connection _ `session/1,2,3` \- open an ssh channel on the given
  connection and send 'hello' to start a Netconf session

  Own Id: OTP-14284

- Miscellaneous updates due to atoms containing arbitrary Unicode characters.

  Own Id: OTP-14285

- The function ct_ssh:shell/2,3 is added.

  Own Id: OTP-14415 Aux Id: seq13315

## Common_Test 1.14

### Fixed Bugs and Malfunctions

- The following corrections and improvements are done in the common_test hook
  handling:

  - An extra argument, `Suite`, is added as the first argument to each of the
    following hook callback functions:

    - `pre_init_per_group`
    - `post_init_per_group`
    - `pre_end_per_group`
    - `post_end_per_group`
    - `pre_init_per_testcase`
    - `post_init_per_testcase`
    - `pre_end_per_testcase`
    - `post_end_per_testcase`
    - `on_tc_fail`
    - `on_tc_skip`

    For backwards compatibility, if the new function is not exported from a hook
    callback module, `common_test` will fall back to the old interface and call
    the function without the `Suite` argument.

  - If either `init_per_suite` or `end_per_suite` exists, but not the other,
    then the non-existing function will be reported as failed with reason
    `undef` in the test log. The same goes for `init/end_per_group`. This has
    always been a requirement according to the user's guide, but now
    `common_test` is more explicit in the report.
  - If `init_per_suite` was exported from a test suite, but not `end_per_suite`,
    then `pre/post_end_per_suite` was called with `Suite=ct_framework` instead
    of the correct suite name. This is now corrected.
  - If `end_per_group` was exported from a suite, but not `init_per_group`, then
    `end_per_group` was never called. This is now corrected.
  - Tests that were skipped before calling `pre_init_per_*` got faulty calls to
    the corresponding `post_init_per_*`. E.g. if a test was skipped because
    `suite/0` failed, then `post_init_per_suite` would be called even though
    `pre_init_per_suite` and `init_per_suite` were not called. This is now
    corrected so a `post_*` callback will never be called unless the
    corresponding `pre_*` callback has been called first.
  - Tests that were skipped before or in `init_per_testcase` got faulty calls to
    `pre_end_per_testcase` and `post_end_per_testcase`. This is now corrected so
    `pre/post_end_per_testcase` are not called when `end_per_testcase` is not
    called.
  - If an exit signal causes the test case process to die while running
    `init_per_testcase`, the case was earlier reported as failed with reason
    `{skip,...}`. This is now corrected so the case will be marked as skipped.
  - If an exist signal causes the test case process to die while running
    `end_per_testcase`, the case was earlier marked as failed. This is now
    corrected so the status of the test case is not changed - there is only a
    warning added to the comment field.
  - If a test case was skipped because of option `{force_stop,skip_rest}` or
    because of a failed sequence, then no `tc_start` event would be sent, only
    `tc_done`. This is now corrected so both events are sent.
  - When skipping or failing in a configuration function, the configuration
    function itself would get `{auto_skipped,Reason}`, `{skipped,Reason}` or
    `{failed,Reason}` in the hook callbacks `on_tc_skip` or `on_tc_fail`. The
    other test cases that were skipped as a result of this would only get
    `Reason` in `on_tc_skip`. This is now corrected so even the configuration
    function that caused the skip/fail will only get `Reason` in the hook
    callback.

  Own Id: OTP-10599 Aux Id: kunagi-344 \[255]

- When a test case was skipped by a `skip_cases` statement in a test spec, then
  `cth_surefire` would erroneously mark the previous test case as skipped in the
  xml report. The actually skipped test case would not be present in the xml
  report at all. This is now corrected.

  Own Id: OTP-14129 Aux Id: seq13244

- The `multiply_timetraps` and `scale_timetraps` options did not work with test
  specifications, which has been corrected.

  Own Id: OTP-14210

### Improvements and New Features

- ct_testspec:get_tests/1 is added. This is used by rebar3 to get all
  directories that must be compiled when running tests from testspec - instead
  of implementing testspec parsing in rebar3.

  Own Id: OTP-14132

## Common_Test 1.13

### Fixed Bugs and Malfunctions

- Some types of printouts to screen during test runs (including
  `ct:print/1,2,3,4`) used the local `user` process as IO device and these
  printouts would not be visible when e.g. running tests via a shell on a remote
  node. A default Common Test group leader process has been introduced to solve
  the problem. This process routes printouts to the group leader of the starting
  process, if available, otherwise to `user`.

  Own Id: OTP-13973 Aux Id: ERL-279

- Some Common Test processes, that act as I/O group leaders for test cases,
  would not terminate as expected at the end of test runs. This error has been
  corrected.

  Own Id: OTP-14026 Aux Id: ERL-287

- The logging verbosity feature was incorrectly documented. The default
  verbosity levels for test runs is e.g. not 50 (`?STD_VERBOSITY`), but 100
  (`?MAX_VERBOSITY`). Also, some of the examples had errors and flaws. The
  corresponding chapter (5.18) in the User's Guide has been updated.

  Own Id: OTP-14044 Aux Id: seq13223

### Improvements and New Features

- A feature to let the user specify headings to log printouts has been added.
  The heading is specified as `{heading,string()}` in the `Opts` list argument
  to `ct:pal/3,4,5`, `ct:print/3,4,5`, or `ct:log/3,4,5`. If the heading option
  is omitted, the category name, or `"User"`, is used as the heading instead.

  Own Id: OTP-14043 Aux Id: seq13226

## Common_Test 1.12.3

### Fixed Bugs and Malfunctions

- If the telnet server would pause during transmission of a line of text before
  terminating the line, the `ct_telnet:expect/3` function would print the line
  twice in the test case HTML log. This problem has been fixed.

  Own Id: OTP-13730 Aux Id: seq13135

- The functions `ct:set_verbosity/2` and `ct:get_verbosity/1` have been added in
  order to make it possible for test cases, CT Hooks, or test framework
  functions, to modify and read verbosity levels for logging.

  Own Id: OTP-13841

- `make` (tools) and `ct_make` (common_test) would crash if an Erlang source
  file contained a `-warning()` directive.

  Own Id: OTP-13855

## Common_Test 1.12.2

### Fixed Bugs and Malfunctions

- The following modules were missing in common_test.app.src: ct_groups,
  ct_property_test, ct_release_test, ct_webtool, ct_webtool_sup, test_server_gl.
  They have now been added.

  Own Id: OTP-13475

- Common Test printed incorrect timestamps for received error reports.

  Own Id: OTP-13615 Aux Id: seq13124

## Common_Test 1.12.1

### Fixed Bugs and Malfunctions

- The `nodelay` option used to be enabled (`true`) by default for sockets opened
  by the Common Test telnet client. This appeared to cause communication
  problems with telnet servers on some systems, and therefore the option is no
  longer used. Its value may instead be specified in the telnet connection
  settings. See the man page for `ct_telnet` for details. Please note that the
  interface function `connect` in `unix_telnet` has been updated with an extra
  argument and is now `unix_telnet:connect/7`.

  Own Id: OTP-13462 Aux Id: seq13077

- Fix bug in cth_surefire: When a pre_init_per_suite hook fails before reaching
  the cth_surefire:pre_init_per_suite, cth_surefire produced incorrect XML.

  Own Id: OTP-13513

- The `ct:get_timetrap_info/0` function has been updated to return more
  information about timetrap scaling.

  Own Id: OTP-13535

- A problem with stylesheet HTML tags getting incorrectly escaped by Common Test
  has been corrected.

  Own Id: OTP-13536

- The `ct_run` start flag `-no_esc_chars` and `ct:run_test/1` start option
  `{esc_chars,Bool}` have been introduced to make it possible to disable
  automatic escaping of characters. Automatic escaping of special HTML
  characters printed with `io:format/1,2` and `ct:pal/1,2,3,4` was introduced in
  Common Test 1.12. The new flag/option may be used to disable this feature for
  backwards compatibility reasons. (The option is also supported in test
  specifications).

  Own Id: OTP-13537

## Common_Test 1.12

### Fixed Bugs and Malfunctions

- This update fixes the problem with generic printouts in the html log file not
  having special characters escaped. Printouts made with `io:format/2` and
  `ct:pal/2` will now get special characters escaped automatically. Common Test
  will not attempt to escape characters printed with `ct:log/2` since it is
  assumed that the user may want to print html tagged data using this function.
  A new function, `ct:log/5`, has been added, which offers optional escaping of
  characters. The latter function may also be used to print text to the log
  without headers and CSS class wrapping (analogue to `io:format/2`).

  Own Id: OTP-13003 Aux Id: seq13005

- Commit 4cf832f1ad163f5b25dd8a6f2d314c169c23c82f erroneously removed logging of
  open and close of netconf connections. This is now corrected.

  Own Id: OTP-13386

- The directory to which nodes started with `test_server:start_node/3` writes
  their erl_crash.dump is changed. The crashdumps were earlier written to the
  directory of test_server.beam, but in later versions of Microsoft Windows this
  is no longer writable (even for administrators). The framework (common_test)
  log directory is now used instead.

  Own Id: OTP-13388

### Improvements and New Features

- This update makes it possible to specify multiple instances of the same group
  or test case in one test specification term in order to repeat execution.
  Example:
  `{groups, "./", my_SUITE, [my_group, my_group], {cases, all}}, or {cases, "./", my_SUITE, [my_tc, my_tc, my_tc]}.`

  Own Id: OTP-13241 Aux Id: seq12979

- Two new CT hook functions have been added: `post_init_per_testcase/4` and
  `pre_end_per_testcase/3`. With these hook functions, it is possible to perform
  arbitrary actions (including modifications of test execution, test state and
  results) immediately before and after the execution of the test case.

  Own Id: OTP-13242 Aux Id: seq12991

- The `ct_netconfc` was earlier very restrictive as to which SSH options the
  user could set. This is now changed, and any SSH option is now allowed. The
  netconf client will simply pass on any option, which it does not recognize, to
  SSH.

  Own Id: OTP-13338 Aux Id: seq13053,seq13069

## Common_Test 1.11.2

### Fixed Bugs and Malfunctions

- If a ssh package contained more than one netconf end tag, then the second end
  tag was never detected in ct_netconfc:handle_data. Instead it was included in
  the XML data given to the xmerl parser, which then failed. The problem was
  introduced by OTP-13007, and has now been corrected.

  Own Id: OTP-13323

## Common_Test 1.11.1

### Fixed Bugs and Malfunctions

- When data from the netconf server was split into many ssh packages, the
  netconf client performed really bad. This is now improved.

  Own Id: OTP-13007

- In ct_netconfc, if a timer expired 'at the same time' as the server sent the
  rpc-reply, the timeout message might already be in the client's message queue
  when the client removed the timer ref from its 'pending' list. This caused a
  crash in the client since the timer ref could no longer be found when handling
  the timeout message. This problem is now fixed by always flushing the timeout
  message from the message queue when canceling a timer.

  Own Id: OTP-13008

- The error logger handler ct_conn_log_h did not respect the 'silent' option,
  and tried to print to an undefined file descriptor. This has been corrected.

  Own Id: OTP-13035

- If the user would let the test run proceed after test suite compilation
  failure, Common Test did not set the exit status to indicate failure as
  expected. This has been corrected. Also, the 'abort_if_missing_suites' option
  now makes Common Test abort the test run without asking the user if
  compilation fails, even if access to stdin/stdout exists.

  Own Id: OTP-13173 Aux Id: seq12978

- With the Common Test 'create_priv_dir' start option set to 'auto_per_tc', the
  name of the priv directory for a configuration function could clash with the
  name of the priv directory for a test case, which would cause Test Server
  failure. This error has been corrected.

  Own Id: OTP-13181

## Common_Test 1.11

### Fixed Bugs and Malfunctions

- The status of an aborted test due to test suite compilation error has changed
  from 'auto_skipped' to 'failed'. This affects both the textual log file, event
  handling and CT hook callbacks. The logging of compilation failures has also
  been improved, especially in the case of multiple test suites failing
  compilation.

  Own Id: OTP-10816

- The Test Server source code parser (erl2html2) failed to handle the macro
  tuple in the syntax tree returned by epp_dodger. This error has been
  corrected.

  Own Id: OTP-12740

- New options to make it possible to specify ssh_port in a .spec file:
  \[\{node_start, [\{ssh_port, 9999\}]\}].

  And also to specify additional ssh options like paths to public-key files:
  \[\{node_start, [\{ssh_opts, [\{user_dir, "/home/shrek/e2/"\}]\}]\}].

  Own Id: OTP-12809

### Improvements and New Features

- Earlier there was no way to add optional parameters like default-operation to
  an edit-config request sent with ct_netconfc:edit_config/3,4, you had to use
  ct_netconfc:send_rpc/2,3. For simplicity and completion, a new optional
  argument, OptParams, is now added to the edit_config function.

  Own Id: OTP-10446 Aux Id: kunagi-266 \[177]

- When running OTP tests using the ts interface, it is now possible to specify
  so called test categories per OTP application. A test category is represented
  by a CT test specification and defines an arbitrary subset of existing test
  suites, groups and cases. Examples of test categories are 'smoke' (smoke
  tests) and 'bench' (benchmarks). (Call ts:help() for more info). Also,
  functions for reading terms from the current test specification during test,
  ct:get_testspec_terms/0 and ct:get_testspec_terms/1, have been implemented.

  Own Id: OTP-11962

- Obsolete scripts and make file operations have been removed and the
  installation chapter in the Common Test User's Guide has been updated.

  Own Id: OTP-12421

- The 'keep_alive' interval has been reduced to 8 seconds, which is two seconds
  shorter than the default 'idle_timeout' value for ct_telnet:expect/3. This
  way, the telnet server receives a NOP message (which might trigger an action)
  before the operation times out. Also the TCP option 'nodelay' has been enabled
  per default for all telnet connections, in order to reduce the risk for
  communication timeouts.

  Own Id: OTP-12678 Aux Id: seq12818

- When the ct_run program is executed without any flags, "-dir ." is now used as
  default start flag. Similarly, the option \{dir,"."\} is used by ct:run_test/1
  if called with an empty list. Also, the help text (ct_run -help) has been
  updated, as well as the Running Tests chapter in the Common Test User's Guide.

  Own Id: OTP-12684 Aux Id: seq12865

## Common_Test 1.10.1

### Fixed Bugs and Malfunctions

- A fault in the Common Test logger process, that caused the application to
  crash when running on a long name node, has been corrected.

  Own Id: OTP-12643

- A 'wait_for_prompt' option in ct_telnet:expect/3 has been introduced which
  forces the function to not return until a prompt string has been received,
  even if other expect patterns have already been found.

  Own Id: OTP-12688 Aux Id: seq12818

- If the last expression in a test case causes a timetrap timeout, the stack
  trace is ignored and not printed to the test case log file. This happens
  because the \{Suite,TestCase,Line\} info is not available in the stack trace
  in this scenario, due to tail call elimination. Common Test has been modified
  to handle this situation by inserting a \{Suite,TestCase,last_expr\} tuple in
  the correct place and printing the stack trace as expected.

  Own Id: OTP-12697 Aux Id: seq12848

- Fixed a buffer problem in ct_netconfc which could cause that some messages
  where buffered forever.

  Own Id: OTP-12698 Aux Id: seq12844

- The VTS mode in Common Test has been modified to use a private version of the
  Webtool application (ct_webtool).

  Own Id: OTP-12704 Aux Id: OTP-10922

- Add possibility to add user capabilities in `ct_netconfc:hello/3`.

  Own Id: OTP-12707 Aux Id: seq12846

## Common_Test 1.10

### Fixed Bugs and Malfunctions

- The tests overview file, index.html, did not always get updated correctly
  after a new test run. This was because of a bug in the Common Test log cache
  mechanism which has now been corrected.

  Own Id: OTP-11400

- When a successful test case returns, Common Test should, according to the
  documentation, send a tc_done event to the event handlers with Result = ok in
  the data field. However, Common Test sets Result to the return value of the
  test case instead. Common Test has been modified now to comply with the
  documentation.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12279 Aux Id: seq12737, OTP-12531

- A ct_telnet:expect/3 call could never be aborted before an idle_timeout, even
  if total_timeout had been set to a lower value (i.e. a shorter time). This
  problem has been fixed.

  Own Id: OTP-12335

- The undocumented return value \{skipped,Reason\} from config functions and
  test cases was handled inconsistently. Test cases were e.g. reported as
  "skipped" to CT Hook functions, but "successful" to event handlers. Now, the
  above return value is consistently handled the same way as \{skip,Reason\} and
  this has also been documented.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12359 Aux Id: seq12760

- The Erlang source code to HTML generator would sometimes fail because
  epp:parse_erl_form/1 could not find and expand required macros in included
  header files. The problem has been solved by making sure common_test always
  passes the full include path to epp. Also, a bug that could cause
  erl_syntax:revert/1 to fail because of a badly formed syntax tree has been
  corrected.

  Own Id: OTP-12419

- A missing group option in the ct_run help text has been added.

  Own Id: OTP-12433 Aux Id: seq12788

- Printouts by means of ct:log/2/3 or ct:pal/2/3 from the hook functions
  on_tc_fail/2 and on_tc_skip/2 would (quite unexpectedly) end up in the
  "unexpected i/o" log file instead of in the test case log file. This behaviour
  has been changed so that now, all printouts (including stdio printouts) from
  these hook functions will be routed to the test case log file.

  Own Id: OTP-12468

- ct_netconfc:action/3 will now - if the return type is void - accept an RPC
  reply on the form \{ok,\[simple_xml()]\}, and in this event return only the
  atom ok.

  Own Id: OTP-12491 Aux Id: seq12797

- OTP-11971 erroneously changed the handling of relative paths for incl_dirs
  specified in the cover spec file. This is now corrected so these are expected
  to be relative to the directory where the cover spec file itself is stored

  Own Id: OTP-12498 Aux Id: OTP-11971

- Some test cases have been updated to use ct:sleep/1 instead of timer:sleep/1.
  The reason being that the sleep times need to be scaled to compensate for slow
  execution (e.g. when cover is running).

  Own Id: OTP-12574

### Improvements and New Features

- Common Test now exports a function, ct:get_event_mgr_ref/0, that returns the
  name of the Common Test event manager. This makes it possible to plug in event
  handlers to the event manager while tests are running (using the gen_event
  API).

  Own Id: OTP-12506 Aux Id: seq12802

- When a test case (or configuration function) fails because of an exit signal
  from a linked process, Common Test previously passed only the reason for
  process termination to the CT post hook functions and the event handlers (in
  the tc_done event). This has been changed so that now the tuple
  \{'EXIT',ReasonForProcessTermination\} is passed instead. This makes it much
  easier in the CT post hook functions to distinguish a failure of this sort
  from other types of errors and from the return value of a successful test
  case.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12531 Aux Id: OTP-12279

- A new feature has been introduced in ct_telnet:get_data/1 that makes it
  possible to automatically poll the telnet connection in case an incomplete
  string (one that has not yet been terminated by a newline) remains in the
  receive buffer. The polling is controlled by two new telnet config values,
  which are documented in the ct_telnet reference manual. The polling mechanism
  is disabled by default (making the get_data/1 function backwards compatible).

  Own Id: OTP-12627

## Common_Test 1.9

### Fixed Bugs and Malfunctions

- The source code to html code generator in Test Server (and Common Test) would
  fail to generate anchors in the html code for functions with non-expandable
  macros, resulting in bad html links to such functions. This correction lets
  the code generator ignore macros that can't be expanded (i.e. not pre-process
  them), so that correct anchors will always be produced.

  Own Id: OTP-11766 Aux Id: seq12556

- OTP-11971 erroneously changed the handling of relative paths (import/export
  files) specified in the cover spec file. This is now corrected so these are
  expected to be relative to the directory where the cover spec file itself is
  stored.

  Own Id: OTP-12031

- Common Test would sometimes crash while trying to print large amounts of SASL
  reports to log on a computer with a slow file system. This problem (due to an
  error in IO message buffering in ct_logs) has been fixed.

  Own Id: OTP-12159

- The common_test telnet client, ct_telnet and friends, had some unstable test
  cases. Some of these were caused by the unix_telnet callback sending an extra
  newline after sending the password. This caused the sever to send an extra
  prompt back which confused the tests. The extra newline is no longer sent.

  Also, debug printouts and logging from the telnet client is improved, and some
  test cases are slightly modified in order to stabilize the test.

  Own Id: OTP-12329

- ct_netconfc did not expect the return value \{error,timeout\} from
  ssh_connection:subsystem/4. This has been corrected.

  Own Id: OTP-12334

### Improvements and New Features

- A new option, `{newline,boolean()}` is added to all functions in `ct_telnet`
  that send data (command strings) to the telnet server. By default, `ct_telnet`
  adds a newline to all command strings, and by setting the new option to
  `false` this behavior is turned off.

  Own Id: OTP-12252 Aux Id: seq12730

- Distribute `autoconf` helpers to applications at build time instead of having
  multiple identical copies committed in the repository.

  Own Id: OTP-12348

## Common_Test 1.8.2

### Fixed Bugs and Malfunctions

- Ticket OTP-11971 introduced a runtime dependency towards test_server-3.7.1,
  since the interface between test_server and common_test was changed.
  Erroneously, the common_test.app file was not updated according to this. This
  has now been corrected.

  Own Id: OTP-12037

### Improvements and New Features

- Warning: this is experimental and may disappear or change without previous
  warning.

  Experimental support for running Quickcheck and PropEr tests from common_test
  suites is added to common_test. See the reference manual for the new module
  `ct_property_testing`.

  Experimental property tests are added under
  `lib/{inet,ssh}/test/property_test`. They can be run directly or from the
  commont_test suites `inet/ftp_property_test_SUITE.erl` and
  `ssh/test/ssh_property_test_SUITE.erl`.

  See the code in the `test` directories and the man page for details.

  (Thanks to Tuncer Ayaz for a patch adding Triq)

  Own Id: OTP-12119

## Common_Test 1.8.1

### Fixed Bugs and Malfunctions

- Substrings in long telnet messages would sometimes get wrongly reversed. This
  error has been corrected.

  Own Id: OTP-11871 Aux Id: seq12581

- The basic_html logging mode in Common Test (for compatibility with old
  browsers) generated HTML code with unbalanced tags. This has been fixed.

  Own Id: OTP-11917 Aux Id: seq12598

- The mechanism for running code cover analysis with common_test has been
  improved. Earlier, if a test run consisted of multiple tests, cover would be
  started and stopped for each test. This would give "intermediate" cover logs
  available from the "Coverage log" link on the test suite result pages. To
  accumulate cover data over all tests, the 'export' option had to be used in
  the cover spec file. This was not well documented, and the functionality was
  quite confusing.

  Using the 'nodes' option in the cover spec file would fail when the test run
  consisted of multiple tests, since the specified nodes would only be included
  in the cover analysis of the first test.

  The repeated compilation and analysis of the same modules was also very time
  consuming.

  To overcome these problems, ct will now only cover compile and analyze modules
  once per test run, i.e. once for each cover spec file. The log file is
  available via a new button on the top level index page. The old "Coverage log"
  links on the test suite result pages still exist, but they all point to the
  same log containing the accumulated result.

  Own Id: OTP-11971

- If multiple tests would run simultaneously on different Erlang nodes, writing
  their logs to the same directory, then there would often be entries in the
  all_runs.html log file showing incomplete results (all zeroes) upon
  completion. This problem was caused by a bug in the Common Test log cache
  mechanism, which has been fixed.

  Own Id: OTP-11988 Aux Id: seq12611

## Common_Test 1.8

### Fixed Bugs and Malfunctions

- The error generated if a test case process received an exit from a linked
  process while executing init_per_testcase/2, was handled incorrectly by Common
  Test. The problem has been solved, and Common Test now reports this type of
  error correctly, with proper error reason and exit location as well.

  Own Id: OTP-11643

- Running a parallel test case group with two or more instances of the same test
  case would result in identical log file names, and one test case instance
  would overwrite the log file of another. This problem has been solved.

  Own Id: OTP-11644

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

- The `cth_surefire` hook would crash in `pre_init_per_suite/3` if a previous
  hook returned `{skip,Reason}` or `{fail,Reason}` instead of a `Config` list.
  This error has been corrected, and `cth_surefire` will now simply propagate
  the received `InitData` value instead.

  Own Id: OTP-11811

- Specs of return values are corrected for `ct_netconfc:get/2,3`,
  `ct_netconfc:get_config/3,4`, `ct_netconfc:action/2,3`,
  `ct_netconfc:send_rpc/2,3` and `ct_netconfc:send/2,3`.

  Own Id: OTP-11834 Aux Id: seq12574

### Improvements and New Features

- ct_telnet can now log all communication taking place during a telnet session.
  Previously, only information about ct_telnet operations and commands, as well
  as explicitly requested data from the server, was logged.

  Furthermore, a logging mechanism based on an Error Logger event handler and a
  dedicated Common Test hook, `cth_conn_log`, now makes it possible to print
  data for individual connections to separate log files. Please see the
  `ct_telnet` reference manual for more information and examples.

  Important note: A new argument, `ConnName` has been added to the
  `unix_telnet:connect/5` callback function. This forces users that use private
  ct_telnet callback modules to update their code according to
  `unix_telnet:connect/6`. Please see the `unix_telnet` reference manual and
  source code module for details.

  Own Id: OTP-11440 Aux Id: seq12457

- A new timeout option has been introduced for the `ct_telnet:expect/3`
  function. With `{total_timeout,Time}` it's possible to set a time limit for
  the complete expect operation. After `Time` milliseconds, `expect/3` returns
  `{error,timeout}`. The default value used if `total_timeout` is not specified,
  is infinity (i.e. no time limit). Please see the `ct_telnet` reference manual
  for more information.

  Own Id: OTP-11689

- Some function specs are corrected or moved and some edoc comments are
  corrected in order to allow use of edoc. (Thanks to Pierre Fenoll)

  Own Id: OTP-11702

- Test case group name information has been added to the data sent with
  `tc_user_skip` and `tc_auto_skip` event messages, as well as the data passed
  in calls to the CT Hook functions `on_tc_skip/3` and `on_tc_fail/3`. The
  modification only affects the function name element/argument. This value
  remains an atom if the test case in question does not belong to a test case
  group. Otherwise a tuple `{FuncName,GroupName}` (`{atom(),atom()}`) is passed
  instead.

  Note that this change may (depending on the patterns used for matching)
  require modifications of user event handlers and hook modules. Please see the
  Event Handling chapter in the Common Test User's Guide, and the reference
  manual for `ct_hooks`, for details.

  Note also that the Test Server framework callback function `report/2` has been
  modified. This change only affects users with test frameworks interfacing Test
  Server rather than Common Test. See the `test_server_ctrl` reference manual
  for details.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11732 Aux Id: seq12541

- If Common Test can't prompt the user to abort or continue the test run when
  one or more test suites fail to compile, a new option,
  `{abort_if_missing_suites,Bool}`, can be used to specify whether it should
  proceed with the test run, or stop execution. The default value of `Bool` is
  `false` (i.e. to proceed even if suites are missing).

  Own Id: OTP-11769

### Known Bugs and Problems

- common_test: Fix problems reported by Dialyzer.

  Own Id: OTP-11525

## Common_Test 1.7.4

### Fixed Bugs and Malfunctions

- Return values from group and testcase info functions are now properly checked,
  and associated test cases are auto skipped if a return value is invalid.

  Own Id: OTP-10631 Aux Id: kunagi-345 \[256]

- The way Common Test handles skipping of test cases has been updated. In
  previous versions, returning `{skip,Reason}` from a configuration function
  (such as init_per_suite or init_per_group), resulted in all affected test
  cases getting skipped with status `auto_skipped`. This was inappropriate,
  since this status is supposed to be used to inform that Common Test has taken
  the initiative to skip something (e.g. a test case group if init_per_group
  failed). Therefore, in this version of Common Test, whenever the user skips a
  suite, group, or individual test case (by means of a configuration function or
  test specification term), the affected test cases get the status
  `user_skipped` instead.

  This update has meant a few changes that may affect Common Test users in
  various ways:

  - The test results and statistics will be affected, which is important to know
    when running regression tests and comparing results to previous test runs.
  - Users that read or parse the textual log file `suite.log` will notice that
    an auto skipped function is now reported as `auto_skipped` rather than
    `skipped` as before.
  - When `require` fails in an info function (such as suite/0 or group/1), all
    affected configuration functions and test cases are marked as
    `auto_skipped`.
  - If Common Test detects an error in the test suite (such as e.g. an invalid
    all/0 function), all affected configuration functions and test cases are
    marked as `auto_skipped`.
  - If a repeated test run session reaches a deadline with `force_stop` enabled,
    all remaining test cases are marked as `auto_skipped` rather than
    `user_skipped` as before.
  - The event messages that Common Test generates during test runs have been
    affected by this update. For details see OTP-11524.

  Own Id: OTP-11305 Aux Id: OTP-11524

- Returning \{skip, Reason\} from a pre_end_per_group/3 user hook function would
  result in an exit in the Common Test cth_log_redirect hook. This problem has
  been solved.

  Own Id: OTP-11409 Aux Id: seq12446

- When the netconf server did not respond to the close-session request, the call
  to ct_netconfc:close_session/2 would hang forever waiting for the netconf
  client to terminate. This has been corrected. The client will now always
  terminate (and take down the connection) if the close-session request times
  out.

  Own Id: OTP-11478

### Improvements and New Features

- Fix cth_log_redirect.erl to fulfill gen_event behaviour. Thanks to Roberto
  Aloi.

  Own Id: OTP-11401

- The first argument of the CT hook callback function `on_tc_skip/3` has been
  modified. When this function is called for `init_per_group` or
  `end_per_group`, the value of the first argument is now
  `{init_per_group,GroupName}` or `{end_per_group,GroupName}`.

  Own Id: OTP-11523

- The following modifications have been made to the event messages that Common
  Test sends during test execution:

  - For the `tc_auto_skip` event, the value of the `Func` element has changed
    from `end_per_group` to `{end_per_group,GroupName}`.
  - When `require` fails in an info function, such as suite/0 or group/1, the
    init configuration function is now reported as `auto_skipped` instead of
    `skipped`, with the `tc_done` event.
  - When `require` fails in an info function because of a configuration name
    already in use, the `tc_done` event now reports the error with a tuple (of
    size 2) tagged `failed` instead of `skipped`.

  Please see the Event Handling chapter in the Common Test User's Guide for
  reference.

  Own Id: OTP-11524 Aux Id: OTP-11305

## Common_Test 1.7.3

### Fixed Bugs and Malfunctions

- Documentation is added for ct_netconfc:send and ct_netconfc:send_rpc.

  Own Id: OTP-11132

- ct_netconfc:create_subscription only allowed one XML element inside the
  'filter' element. According to RFC5277 it should be allowed to add any number
  of elements inside the filter, so this is now corrected.

  Own Id: OTP-11166

- The error handler installed by the Common Test hook cth_log_redirect did not
  respond to init:stop/1/2. This has been corrected.

  Own Id: OTP-11175 Aux Id: seq12356

- Calling ct:pal/2 or ct:print/2 when Common Test was not running, would cause
  an exit. This has been changed and the string is now simply printed to stdout
  instead.

  Own Id: OTP-11176

- Fixed problem with the cth_log_redirect hook making calls to an undefined
  function in ct_logs.

  Own Id: OTP-11238

- When running tests with the 'repeat' option, the Common Test utility process
  did not always terminate quickly enough after a test run, causing the start of
  the next run to fail. A monitor is now used to ensure termination of the
  utility process after each test run.

  Own Id: OTP-11244 Aux Id: seq12396

- Test Server installed an error handler (test_server_h) only to be able to
  write the name of the current test case to stdout whenever it received an
  error- or progress report. This functionality was not useful and has been
  removed. The built-in Common Test hook, cth_log_redirect, has instead been
  improved to now also tag all error- and progress reports in the log with
  suite-, group-, and/or test case name.

  Own Id: OTP-11263 Aux Id: seq12251

### Improvements and New Features

- A new log, the "Pre- and Post Test I/O Log", has been introduced, which makes
  it possible to capture error- and progress reports, as well as printouts made
  with ct:log/2 and ct:pal/2, before and after a test run. (Some minor
  improvements of the logging system have been made at the same time). Links to
  the new log are found on the Common Test Framework Log page. The Common Test
  User's Guide has been updated with information about the new log and also with
  a new section on how to synchronize external applications with Common Test by
  means of the CT Hook init and terminate functions.

  Own Id: OTP-11272

## Common_Test 1.7.2

### Fixed Bugs and Malfunctions

- A design flaw in the generic connection handling in Common Test made it
  impossible to implement a connection handler that could map multiple
  connection names (i.e. configuration variable aliases) to single connection
  pids. This problem has been solved.

  Own Id: OTP-10126 Aux Id: kunagi-178 \[89]

- If a telnet connection is hanging, then a call to ct_telnet:close/1 will time
  out after 5 seconds and the connection process is brutally killed. In some
  cases the connection would not be unregistered and attempts at opening a new
  connection with the same name would make common_test try to reuse the same
  connection since it believed that it was still alive. This has been
  corrected - a killed connection is now always unregistered.

  Own Id: OTP-10648 Aux Id: seq12212

- Test performance has been improved by means of a cache for the top level HTML
  index logs (all_runs.html and index.html, in the logdir directory). This
  solves problems with slow start up times and test execution times increasing
  with the number of ct_run directories stored in logdir. The cached index
  entries are stored in RAM during test execution and are saved to file in
  logdir (for faster start up times) whenever a test run finishes.

  Own Id: OTP-10855

- Testing of the test specification functionality has been improved and a couple
  of minor bugs have been discovered and corrected.

  Own Id: OTP-10857

- Links to the top level index files in some HTML footers had disappeared. This
  error has been corrected. Also, a problem with the suite overview log file not
  being closed properly has been solved.

  Own Id: OTP-11046

- Common Test would, in case of timetrap error, print a warning in the log if
  end_per_testcase wasn't implemented in the suite, even though it's an optional
  function. This printout has been removed.

  Own Id: OTP-11052

### Improvements and New Features

- If it could not be decided which test case a certain log printout belonged to,
  the common test framework log was earlier used. Such printouts are now instead
  sent to unexpected_io.log.html in test_server so that there is only one place
  to look for "missing" printouts.

  Own Id: OTP-10494 Aux Id: kunagi-319 \[230]

- Make cover smarter about finding source from beam.

  In particular, search using the source path in module_info if the current
  heuristic fails.

  Own Id: OTP-10902

- Add a variant of ct_slave:start/2 that starts a node with specified options on
  the local host.

  Own Id: OTP-10920

- Integrate elliptic curve contribution from Andreas Schultz

  In order to be able to support elliptic curve cipher suites in SSL/TLS,
  additions to handle elliptic curve infrastructure has been added to public_key
  and crypto.

  This also has resulted in a rewrite of the crypto API to gain consistency and
  remove unnecessary overhead. All OTP applications using crypto has been
  updated to use the new API.

  Impact: Elliptic curve cryptography (ECC) offers equivalent security with
  smaller key sizes than other public key algorithms. Smaller key sizes result
  in savings for power, memory, bandwidth, and computational cost that make ECC
  especially attractive for constrained environments.

  Own Id: OTP-11009

- Postscript files no longer needed for the generation of PDF files have been
  removed.

  Own Id: OTP-11016

- A link is added from the red error printout in a test case log (for a failed
  test case) to the full error description at the end of the log. The reason for
  this is that the error description in the red field is sometimes truncated at
  50 characters in order to keep the log as short and easy to read as possible.

  Own Id: OTP-11044 Aux Id: seq12304

- A new option 'no_prompt_check' is added to ct_telnet:expect/3. If this option
  is used, ct_telnet will not wait for a prompt or a newline before attempting
  to match the given pattern.

  Own Id: OTP-11095

## Common_Test 1.7.1

### Fixed Bugs and Malfunctions

- If an event handler installed in the CT Master event manager took too long to
  respond during the termination phase, CT Master crashed because of a timeout
  after 5 secs. This would leave the system in a bad state. The problem has been
  solved by means of a 30 min timeout value and if CT Master gets a timeout
  after that time, it now kills the event manager and shuts down properly.

  Own Id: OTP-10634 Aux Id: kunagi-347 \[258]

- Printing with any of the ct printout functions from an event handler installed
  by Common Test, would cause a deadlock. This problem has been solved.

  Own Id: OTP-10826 Aux Id: seq12250

- Using the force_stop flag/option to interrupt a test run caused a crash in
  Common Test. This problem has been solved.

  Own Id: OTP-10832

### Improvements and New Features

- Removed deprecated run_test program, use ct_run instead.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9052

### Known Bugs and Problems

- Test case execution time increases with size of test run.

  Own Id: OTP-10855

## Common_Test 1.7

### Fixed Bugs and Malfunctions

- Severe errors detected by `test_server` (e.g. if log files directories cannot
  be created) will now be reported to `common_test` and noted in the
  `common_test` logs.

  Own Id: OTP-9769 Aux Id: kunagi-202 \[113]

- The earlier undocumented cross cover feature for accumulating cover data over
  multiple tests has now been fixed and documented.

  Own Id: OTP-9870 Aux Id: kunagi-206 \[117]

- If a busy test case generated lots of error messages,
  cth_log_redirect:post_end_per_testcase would crash with a timeout while
  waiting for the error logger to finish handling all error reports. The default
  timer was 5 seconds. This has now been extended to 5 minutes.

  Own Id: OTP-10040 Aux Id: kunagi-173 \[84]

- When a test case failed because of a timetrap time out, the `Config` data for
  the case was lost in the following call to `end_per_testcase/2`, and also in
  calls to the CT Hook function `post_end_per_testcase/4`. This problem has been
  solved and the `Config` data is now correctly passed to the above functions
  after a timetrap timeout failure.

  Own Id: OTP-10070 Aux Id: kunagi-175 \[86]

- Some calls to deprecated and removed functions in snmp are removed from
  ct_snmp.

  Own Id: OTP-10088 Aux Id: kunagi-176 \[87]

- In test_server, the same process would supervise the currently running test
  case and be group leader (and IO server) for the test case. Furthermore, when
  running parallel test cases, new temporary supervisor/group leader processes
  were spawned and the process that was group leader for sequential test cases
  would not be active. That would lead to several problems:

  \* Processes started by init_per_suite will inherit the group leader of the
  init_per_suite process (and that group leader would not process IO requests
  when parallel test cases was running). If later a parallel test case caused
  such a processto print using (for example) io:format/2, the calling would
  hang.

  \* Similarly, if a process was spawned from a parallel test case, it would
  inherit the temporary group leader for that parallel test case. If that
  spawned process later - when the group of parallel tests have finished -
  attempted to print something, its group leader would be dead and there would
  be `badarg` exception.

  Those problems have been solved by having group leaders separate from the
  processes that supervises the test cases, and keeping temporary group leader
  process for parallel test cases alive until no more process in the system use
  them as group leaders.

  Also, a new `unexpected_io.log` log file (reachable from the summary page of
  each test suite) has been introduced. All unexpected IO will be printed into
  it(for example, IO to a group leader for a parallel test case that has
  finished).

  Own Id: OTP-10101 Aux Id: OTP-10125

- Some bugfixes in `ct_snmp:`

  - ct_snmp will now use the value of the 'agent_vsns' config variable when
    setting the 'variables' parameter to snmp application agent configuration.
    Earlier this had to be done separately - i.e. the supported versions had to
    be specified twice.
  - Snmp application failed to write notify.conf since ct_snmp gave the notify
    type as a string instead of an atom. This has been corrected.

  Own Id: OTP-10432

- Some bugfixes in `ct_snmp`:

  - Functions `register_users/2`, `register_agents/2` and
    `register_usm_users/2`, and the corresponding `unregister_*/1` functions
    were not executable. These are corrected/rewritten.
  - Function `update_usm_users/2` is removed, and an unregister function is
    added instead. Update can now be done with unregister_usm_users and then
    register_usm_users.
  - Functions `unregister_*/2` are added, so specific users/agents/usm users can
    be unregistered.
  - Function `unload_mibs/1` is added for completeness.
  - Overriding configuration files did not work, since the files were written in
    priv_dir instead of in the configuration dir (priv_dir/conf). This has been
    corrected.
  - Arguments to `register_usm_users/2` were faulty documented. This has been
    corrected.

  Own Id: OTP-10434 Aux Id: kunagi-264 \[175]

- Faulty exported specs in common test has been corrected to
  `ct_netconfc:hook_options/0` and [`inet:hostname/0`](`t:inet:hostname/0`)

  Own Id: OTP-10601

- The netconf client in common_test did not adjust the window after receiving
  data. Due to this, the client stopped receiving data after a while. This has
  been corrected.

  Own Id: OTP-10646

### Improvements and New Features

- It is now possible to let a test specification include other test
  specifications. Included specs can either be joined with the source spec (and
  all other joined specs), resulting in one single test run, or they can be
  executed in separate test runs. Also, a start flag/option, `join_specs`, has
  been introduced, to be used in combination with the `spec` option. With
  `join_specs`, Common Test can be told to either join multiple test
  specifications, or run them separately. Without `join_specs`, the latter
  behaviour is default. Note that this is a change compared to earlier versions
  of Common Test, where specifications could only be joined. More information
  can be found in the Running Tests chapter in the User's Guide (see the Test
  Specifications section).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9881 Aux Id: kunagi-350 \[261]

- The `ct_slave:start/3` function now supports an `{env,[{Var,Value}]}` option
  to extend environment for the slave node.

  Own Id: OTP-10469 Aux Id: kunagi-317 \[228]

- Some examples overflowing the width of PDF pages have been corrected.

  Own Id: OTP-10665

- Update common test modules to handle unicode:

  - Use UTF-8 encoding for all HTML files, except the HTML version of the test
    suite generated with erl2html2:convert, which will have the same encoding as
    the original test suite (.erl) file.
  - Encode link targets in HTML files with test_server_ctrl:uri_encode/1.
  - Use unicode modifier 't' with ~s when appropriate.
  - Use unicode:characters_to_list and unicode:characters_to_binary for
    conversion between binaries and strings instead of binary_to_list and
    list_to_binary.

  Own Id: OTP-10783

### Known Bugs and Problems

- CT drops error reason when groups/0 crashes.

  Own Id: OTP-10631 Aux Id: kunagi-345 \[256]

- Event handler on a ct_master node causes hanging.

  Own Id: OTP-10634 Aux Id: kunagi-347 \[258]

- CT fails to open telnet conn after a timetrap timeout.

  Own Id: OTP-10648 Aux Id: seq12212

## Common_Test 1.6.3.1

### Known Bugs and Problems

- The following corrections/changes are done in the cth_surefire hook:

  - Earlier there would always be a 'properties' element under the 'testsuites'
    element. This would exist even if there were no 'property' element inside
    it. This has been changed so if there are no 'property' elements to display,
    then there will not be a 'properties' element either.
  - The XML file will now (unless other is specified) be stored in the top log
    directory. Earlier, the default directory would be the current working
    directory for the erlang node, which would mostly, but not always, be the
    top log directory.
  - The 'hostname' attribute in the 'testsuite' element would earlier never have
    the correct value. This has been corrected.
  - The 'errors' attribute in the 'testsuite' element would earlier display the
    number of failed testcases. This has been changed and will now always have
    the value 0, while the 'failures' attribute will show the number of failed
    testcases.
  - A new attribute 'skipped' is added to the 'testsuite' element. This will
    display the number of skipped testcases. These would earlier be included in
    the number of failed test cases.
  - The total number of tests displayed by the 'tests' attribute in the
    'testsuite' element would earlier include init/end_per_suite and
    init/end_per_group. This is no longer the case. The 'tests' attribute will
    now only count "real" test cases.
  - Earlier, auto skipped test cases would have no value in the 'log' attribute.
    This is now corrected.
  - A new attributes 'log' is added to the 'testsuite' element.
  - A new option named 'url_base' is added for this hook. If this option is
    used, a new attribute named 'url' will be added to the 'testcase' and
    'testsuite' elements.

  Own Id: OTP-10589

## Common_Test 1.6.3

### Fixed Bugs and Malfunctions

- The ct:run_test/1 option 'config' only worked with a single config file, not a
  list of files. This has been fixed.

  Own Id: OTP-10495

- ct_netconfc:close_session sometimes returned \{error,closed\} because the ssh
  connection was closed (from the server side) before the rpc-reply was received
  by the client. This is normal and cannot be helped. It has been corrected so
  the return will be 'ok' in this case. Other error situations will still give
  \{error,Reason\}.

  Own Id: OTP-10510 Aux Id: kunagi-320 \[231]

- ct_netconfc:close_session sometimes returned \{error,closed\} or (if the
  connection was named) \{error,\{process_down,Pid,normal\}\} because the ssh
  connection was closed (from the server side) before the rpc-reply was received
  by the client. This is normal and cannot be helped. It has been corrected so
  the return will be 'ok' in this situation.

  Own Id: OTP-10570

- Fix bug where ct:require of same name with same config would return
  name_in_use.

  Own Id: OTP-10572

### Improvements and New Features

- A new test case group search functionality has been implemented that makes
  Common Test search automatically through the group definitions tree (the
  return value of groups/0) and create tests for all paths of nested groups that
  match the specification. It also allows for specifying unique paths to sub
  groups in order to avoid execution of unwanted tests. This new feature can be
  used whenever starting a test run by means of the ct_run program, the
  ct:run_test/1 API function, or a Test Specification. Details can be found in
  the Test Case Group Execution section in the Running Tests chapter.

  Own Id: OTP-10466 Aux Id: kunagi-276 \[187]

### Known Bugs and Problems

- Restore Config data if lost when test case fails.

  Own Id: OTP-10070 Aux Id: kunagi-175 \[86]

- IO server error in test_server.

  Own Id: OTP-10125 Aux Id: OTP-10101, kunagi-177 \[88]

- Faulty connection handling in common_test.

  Own Id: OTP-10126 Aux Id: kunagi-178 \[89]

## Common_Test 1.6.2.1

### Fixed Bugs and Malfunctions

- The interactive mode (ct_run -shell) would not start properly. This error has
  been fixed.

  Own Id: OTP-10414

## Common_Test 1.6.2

### Fixed Bugs and Malfunctions

- If a CT hook function caused a crash, this could in some situations cause
  Common Test to terminate due to an illegal IO operation. This error has been
  corrected.

  Own Id: OTP-10050 Aux Id: seq12039

- The Common Test documentation states that timetraps are never active during
  execution of CT hook functions. This was only true for post hook functions,
  not for pre hook functions. The code for CT hooks has been modified to behave
  according to the documentation.

  Own Id: OTP-10069

- If a CT hook function would call the exit/1 or throw/1 BIF (possibly
  indirectly, e.g. as a result of a timeout in gen_server:call/3), Common Test
  would hang. This problem has been fixed.

  Own Id: OTP-10072 Aux Id: seq12053

- The documentation has been updated with information about how to deal with
  chaining of hooks which return fail/skip.

  Own Id: OTP-10077 Aux Id: seq12048

- When ct_hooks called the id/1 functions of multiple hooks, it would reverse
  the order of the hooks and call the proceeding init/2 calls in the wrong
  order. This has been fixed.

  Own Id: OTP-10135

- The surefire hook now correctly handles autoskipped initialization and test
  functions.

  Own Id: OTP-10158

- The ct:get_status/0 function failed to report status if a parallel test case
  group was running at the time of the call. This has been fixed and the return
  value for the function has been updated. Please see the ct reference manual
  for details.

  Own Id: OTP-10172

### Improvements and New Features

- The support for "silent connections" has been updated to include ssh. Also, a
  silent_connections term has been added to the set of test specification terms.

  Own Id: OTP-9625 Aux Id: seq11918

- It is now possible to specify an arbitrarily large tuple as the requires
  config data when using require and ct:get_config. See the ct:get_config and
  ct:require reference manual pages for details about which keys are allowed.

  This change introduces a backwards incompatibility in the `ct:require/2`
  interface. Previously when doing `ct:require(a_name,{key,subkey})`, a_name
  would be associated with key. This has been changed to that `a_name` is
  associated with `subkey`. This change also effects using `require` in an
  suite/group/testcase info function.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9626 Aux Id: seq11920

- The ct_run program now sets the OS process exit status before it ends. Value 0
  indicates a successful test result, 1 indicates one or more failed or
  auto-skipped test cases, and 2 indicates test execution failure.

  Own Id: OTP-9865 Aux Id: OTP-10087

- It is now possible to sort the HTML tables by clicking on the header elements.
  In order to reset a sorted table, the browser window should simply be
  refreshed. This feature requires that the browser supports javascript, and has
  javascript execution enabled. If the 'ct_run -basic_html' flag is used, no
  javascript code is included in the generated HTML code.

  Own Id: OTP-9896 Aux Id: seq12034, OTP-9835

- A netconf client, ct_netconfc, is added to common_test. It supports basic
  netconf functionality over SSH. In order to allow testing of both success and
  failure cases, it is intentionally written to allow non-standard behavior.

  Own Id: OTP-10025

- The test specification term \{define,Constant,Value\} has been introduced,
  which makes it possible to replace constant names (atom()) with values
  (term()) in arbitrary test specification terms. The 'define' makes the (now
  deprecated) 'alias' term obsolete. More details, including examples, can be
  found in the Test Specifications chapter in the Common Test User's Guide.

  Own Id: OTP-10049

- Verbosity levels for log printouts has been added. This makes it possible to
  specify preferred verbosity for different categories of log printouts, as well
  as general printouts (such as standard IO), to allow control over which
  strings get printed and which get ignored. New versions of the Common Test
  logging functions, ct:log, ct:pal and ct:print, have been introduced, with a
  new Importance argument added. The Importance value is compared to the
  verbosity level at runtime. More information can be found in the chapter about
  Logging in the Common Test User's Guide.

  Own Id: OTP-10067 Aux Id: seq12050

- The return values of ct:run_test/1 and ct:run_testspec/1 have been changed
  from an uninformative 'ok' (independent of the test outcome) to a value,
  \{Ok,Failed,\{UserSkipped,AutoSkipped\}\} (all integers), that presents the
  final test case result, or a value, \{error,Reason\}, that informs about fatal
  test execution failure. See details in the reference manual for ct.

  Own Id: OTP-10087 Aux Id: OTP-9865

- The test specification syntax has been updated with new and missing terms,
  such as 'define', 'verbosity', 'auto_compile', 'stylesheet',
  'silent_connections', 'basic_html' and 'release_shell'. See the Test
  Specification chapter in the Common Test User's Guide for details.

  Own Id: OTP-10089 Aux Id: OTP-10049

- It is now possible to pause execution of a test case, by calling the
  ct:break/1/2 function. Execution is resumed with a call to ct:continue/0/1.
  Break/continue also works for test cases executing in parallel. See the ct
  reference manual for details.

  Own Id: OTP-10127

- It is now possible to send user defined events from a testcase which will be
  picked up by the installed event handler.

  Own Id: OTP-10157

- A new start option, release_shell, for ct:run_test/1, has been added, which
  makes Common Test release the shell process after the test suite compilation
  phase is finished. For details, see the Running Tests chapter in the User's
  Guide.

  Own Id: OTP-10248 Aux Id: OTP-10127

## Common_Test 1.6.1

### Fixed Bugs and Malfunctions

- Common Test adds the test suite directories to the code path before executing
  the tests. These directories should also be removed from the code path at the
  end of the test run, which, prior to this fix, was not performed.

  Own Id: OTP-9595

- An entry is now created in the index.html file (i.e. the overview file for the
  test run) for each repeated test during a test run. This was previously not
  the case. Note that in the top level (logdir) index file, however, only the
  last test result is listed. For example, given the test spec:
  \[\{merge_tests,false\},\{dirs,"test1"\},\{dirs,"test1"\}]. In the index file
  for the test run (under Logdir/ct_run.Node.Date.Time), both tests are listed.
  In the top level index file (under Logdir), only the last test is listed (one
  has to find the previous results through the all_runs.html file).

  Own Id: OTP-9634 Aux Id: seq11924

- After a test case timeout or abortion, the end_per_testcase function executes
  on a new dedicated process. The group leader for this process should be set to
  the IO server for the test case, which was not done properly. The result of
  this error was that no warnings about end_per_testcase failing or timing out
  were ever printed in the test case log. Also, help functions such as e.g.
  test_server:stop_node/1, attempting to synchronize with the IO server, would
  hang. The fault has been corrected.

  Own Id: OTP-9666

- The ct:get_status/0 function would cause the calling process to receive 'DOWN'
  messages if no tests were running at the time of the call. This bug has been
  fixed.

  Own Id: OTP-9830 Aux Id: seq11975

- A deadlock situation could occur if Common Test is forwarding error_handler
  printouts to Test Server at the same time a new test case is starting. This
  error has been fixed.

  Own Id: OTP-9894

- A link to the ct_run program is now created, as expected, in the installation
  bin directory (default /usr/local/bin) during 'make install'.

  Own Id: OTP-9898

- Using the repeat, duration or until option with ct:run_test/1, would cause an
  infinite loop. This has been fixed.

  Own Id: OTP-9899

- Two or more test cases executing in parallel and printing to screen at the
  same time with ct:pal/2/3 or ct:print/2/3 could write into each other's
  "slots" and create a mess of mixed strings. In order to avoid this, only a
  single IO message is now ever sent per printout call.

  Own Id: OTP-9900 Aux Id: OTP-9904

- When a test case was killed because of a timetrap timeout, the current
  location (suite, case and line) was not printed correctly in the log files.
  This has been corrected.

  Own Id: OTP-9930 Aux Id: seq12002

- The wrong exit location was printed in the log file when ct:fail/1 or
  ct_fail/2 was called.

  Own Id: OTP-9933 Aux Id: seq12002

- Test Server and Common Test would add new error handlers with each test run
  and fail to remove previously added ones. In the case of Test Server, this
  would only happen if SASL was not running on the test node. This has been
  fixed.

  Own Id: OTP-9941 Aux Id: seq12009

- If a test case process was terminated due to an exit signal from a linked
  process, Test Server failed to report the correct name of the suite and case
  to the framework. This has been corrected.

  Own Id: OTP-9958 Aux Id: OTP-9855

- When starting a test with ct_run and adding a directory to the code path using
  -pa or -pz (preceding -erl_args), Common Test would delete any existing
  directory in the code path with the same base name (see filename:basename/1)
  as the directory being added. This has been fixed.

  Own Id: OTP-9964

- If passing two or more directories with the same base name (see
  filename:basename/1) to Common Test with ct_run -pa, only one of the
  directories would actually be added.

  Own Id: OTP-9975 Aux Id: seq12019

- Configuration data required by the group info function was deleted before the
  call to post_end_per_group, which made it impossible for the hook function to
  read and use the data in question. This has been fixed.

  Own Id: OTP-9989

- Disabling built-in hooks in a test specification was ignored, this has now
  been fixed.

  Own Id: OTP-10009

- Various typographical errors corrected in documentation for common_test,
  driver, erl_driver and windows installation instructions. (Thanks to Tuncer
  Ayaz)

  Own Id: OTP-10037

### Improvements and New Features

- A new optional feature has been introduced that enables Common Test to
  generate priv_dir directory names that are unique for each test case or config
  function. The name of the option/flag is 'create_priv_dir' and it can be set
  to value 'auto_per_run' (which is the default, existing, behaviour), or
  'auto_per_tc' or 'manual_per_tc'. If 'auto_per_tc' is used, Test Server
  creates a dedicated priv_dir automatically for each test case (which can be
  very expensive in case of many and/or repeated cases). If 'manual_per_tc' is
  used, the user needs to create the priv_dir explicitly by calling the new
  function ct:make_priv_dir/0.

  Own Id: OTP-9659 Aux Id: seq11930

- A column for test case group name has been added to the suite overview HTML
  log file.

  Own Id: OTP-9730 Aux Id: seq11952

- It is now possible to use the post_end_per_testcase CT hook function to print
  a comment for a test case in the overview log file, even if the test case gets
  killed by a timetrap or unknown exit signal, or if the end_per_testcase
  function times out.

  Own Id: OTP-9855 Aux Id: seq11979

- The pre- and post CT hook functions are now always called for all
  configuration functions, even for configuration functions that are not
  implemented in the test suite.

  Own Id: OTP-9880 Aux Id: seq11993

- Common Test will now print error information (with a time stamp) in the test
  case log file immediately when a test case fails. This makes it easier to see
  when, in time, the fault actually occurred, and aid the job of locating
  relevant trace and debug printouts in the log.

  Own Id: OTP-9904 Aux Id: seq11985, OTP-9900

- Test Server has been modified to check the SASL errlog_type parameter when
  receiving an error logger event, so that it doesn't print reports of type that
  the user has disabled.

  Own Id: OTP-9955 Aux Id: seq12013

- The test specification term 'skip_groups' was implemented in Common Test v1.6.
  It was never documented however, which has now been attended to. Please see
  the Test Specifications chapter in the User's Guide for information.

  Own Id: OTP-9972

- The Common Test Master has been updated to use a CSS style sheet for the html
  log files.

  Own Id: OTP-9973

- If the init_per_group/2 and end_per_group/2 functions are not implemented in
  the test suite, Common Test calls it's own local init- and end functions -
  previously named ct_init_per_group/2 and ct_end_per_group/2 - when a group is
  executed. These functions have been renamed init_per_group/2 and
  end_per_group/2 respectively. Note that this may affect any user event handler
  identifying events by the old names.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9986 Aux Id: OTP-9992

- By specifying a user defined function (\{M,F,A\} or fun) as timetrap value,
  either by means of an info function or by calling ct:timetrap/1, it is now
  possible to set a timetrap that will be triggered when the user function
  returns.

  Own Id: OTP-9988 Aux Id: OTP-9501, seq11894

- If the optional configuration functions init_per_suite/1 and end_per_suite/1
  are not implemented in the test suite, local Common Test versions of these
  functions are called instead, and will be displayed in the overview log file.
  Any printouts made by the pre- or post_init_per_suite and pre- or
  post_end_per_suite hook functions are saved in the log files for these
  functions.

  Own Id: OTP-9992

- A hook has been added to common test which outputs surefire XML for usage
  together with CI tools such as Jenkins. To enable the hook pass '-ct_hooks
  cth_surefire' to ct_run. See the CTH documentation for more details.

  Own Id: OTP-9995

## Common_Test 1.6

### Improvements and New Features

- A Getting Started chapter has been added to the Common Test User's Guide.

  Own Id: OTP-9156

- The test case group info function has been implemented in Common Test. Before
  execution of a test case group, a call is now made to
  `TestSuite:group(GroupName)`. The function returns a list of test properties,
  e.g. to specify timetrap values, require configuration data, etc (analogue to
  the test suite- and test case info function). The scope of the properties set
  by `group(GroupName)` is all test cases and sub-groups of group `GroupName`.

  Own Id: OTP-9235

- Common Test hooks are now in a final supported version. The Common Test hooks
  allow you to abstract out initialization behaviour that is common to multiple
  test suites into one place and also extend the behaviour of a suite without
  changing the suite itself. For more information see the Common Test user's
  guide.

  Own Id: OTP-9449

- A new built-in common test hook has been added which captures error_logger and
  SASL event and prints them in the testcase log. To disable this (and any other
  built-in hooks) pass 'enable_builtin_hooks false' to common test.

  Own Id: OTP-9543

- Common Test now calls info functions also for the `init/end_per_suite/1` and
  `init/end_per_group/2` configuration functions. These can be used e.g. to set
  timetraps and require external configuration data relevant only for the
  configuration functions in question (without affecting properties set for
  groups and test cases in the suite). The info function for
  `init/end_per_suite(Config)` is `init/end_per_suite()`, and for
  `init/end_per_group(GroupName,Config)` it's `init/end_per_group(GroupName)`.
  Info functions cannot be used with `init/end_per_testcase(TestCase, Config)`,
  since these configuration functions execute on the test case process and will
  use the same properties as the test case (i.e. properties set by the test case
  info function, `TestCase()`).

  Own Id: OTP-9569

- It's now possible to read the full name of the test case log file during
  execution. One way to do this is to lookup it up as value of the key
  `tc_logfile` in the test case `Config` list (which means it can also be read
  by a pre- or post Common Test hook function). The data is also sent with the
  event `#event{name=tc_logfile,data={{Suite,Func},LogFileName}}`, and can be
  read by any installed event handler.

  Own Id: OTP-9676 Aux Id: seq11941

- The look of the HTML log files generated by Common Test and Test Server has
  been improved (and made easier to customize) by means of a CSS file.

  Own Id: OTP-9706

- Functions ct:fail(Format, Args) and ct:comment(Format, Args) have been added
  in order to make printouts of formatted error and comment strings easier (no
  need for the user to call io_lib:format/2 explicitly).

  Own Id: OTP-9709 Aux Id: seq11951

- The order in which ct hooks are executed for cleanup hooks (i.e. _*end_per*_
  hooks) has been reversed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9774 Aux Id: seq11913

- Printouts to stdout may be captured during test case execution. This is useful
  in order to e.g. read and parse tty printouts from the SUT during test case
  execution (if necessary, say, to determine the outcome of the test). The
  capturing session is started with `ct:capture_start/0`, and stopped with
  `ct:capture_stop/0`. The list of buffered strings is read and purged with
  `ct:capture_get/0/1`. It's possible to filter out printouts made with
  `ct:log/2/3` and `ct:pal/2/3` from the captured list of strings. This is done
  by calling `capture_get/1` with a list of log categories to exclude.

  Own Id: OTP-9775

- The syntax for specifying test case groups in the all/0 list has been extended
  to include execution properties for both groups and sub-groups. The properties
  specified in all/0 for a group overrides the properties specified in the group
  declaration (in groups/0). The main purpose of this extension is to make it
  possible to run the same set of tests, but with different properties, without
  having to declare copies of the group in question. Also, the same syntax may
  be used in test specifications in order to change properties of groups at the
  time of execution, without having to edit the test suite. Please see the
  User's Guide for details and examples.

  Own Id: OTP-9809 Aux Id: OTP-9235

### Known Bugs and Problems

- Fix problems in CT/TS due to line numbers in exceptions.

  Own Id: OTP-9203

## Common_Test 1.5.5

### Fixed Bugs and Malfunctions

- An error in how comments are colored in the test suite overview html log file
  has been corrected. As result, a new framework callback function,
  format_comment/1, has been introduced.

  Own Id: OTP-9237

- Automatically generated init- and end-configuration functions for test case
  groups caused incorrect execution order of test cases. This has been
  corrected.

  Own Id: OTP-9369

- If multiple directories were specified with the 'logdir' flag/option, Common
  Test would crash. This has been fixed so that an error is properly reported
  instead.

  Own Id: OTP-9370

- If ct:log/2 was called with bad arguments, this could cause the Common Test IO
  handling process to crash. This fault has been corrected.

  Own Id: OTP-9371 Aux Id: OTP-8933

- A bug has been fixed that made Test Server call the end_tc/3 framework
  function with an incorrect module name as first argument.

  Own Id: OTP-9379 Aux Id: seq11863

- If a timetrap timeout occurred during execution of a function in a lib module
  (i.e. a function called directly or indirectly from a test case), the Suite
  argument in the end_tc/3 framework callback function would not correctly
  contain the name of the test suite, but the lib module. (This would only
  happen if the lib module was compiled with ct.hrl included). This error has
  been solved.

  Own Id: OTP-9398

- Corrections of the vts mode. It will now report errors (about e.g. incorrect
  config files) instead of crashing or hanging. Furthermore, the requirement
  that the test directory name must have a "\_test" suffix has been removed.
  Also, a workaround has been implemented for the limitation that the file
  browser (in many web browsers) will only return the basic file name, not the
  full directory path (which made it impossible to have config files in other
  directories than the main test directory).

  Own Id: OTP-9429

- Add a proplist() type

  Recently I was adding specs to an API and found that there is no canonical
  proplist() type defined. (Thanks to Ryan Zezeski)

  Own Id: OTP-9499

- It is now possible to use the 'step' flag/option to run the debugger for test
  suites that contain test case groups. This previously caused Common Test to
  crash. If 'step config' is specified, breakpoints are now also automatically
  set on init_per_group and end_per_group. Note that breakpoints are always set
  automatically on test case functions and this is true also for grouped cases.

  Own Id: OTP-9518 Aux Id: OTP-8933

- The test index page was not refreshed at the start of each test suite which
  made it impossible to follow test execution by means of refreshing the browser
  window (no links to follow). This has been fixed.

  Own Id: OTP-9520 Aux Id: OTP-8933

- If a test suite would start with a test case group defined without the
  init_per_group/2 and end_per_group/2 function, init_per_suite/1 would not
  execute initially and logging of the test run would fail. This error has been
  fixed.

  Own Id: OTP-9584

- The "Missing Suites" link from the top level index page was incorrect and has
  been fixed.

  Own Id: OTP-9592

### Improvements and New Features

- Various corrections and updates to improve the handling and reporting of
  errors.

  Own Id: OTP-8933

- The dir and suite start option can now be used in combination. E.g. executing
  my_SUITE in directory my_tests can either be specified as "ct_run -suite
  my_tests/my_SUITE" or as "ct_run -dir my_tests -suite my_SUITE". Furthermore,
  the specification: ct:run_test(\[\{suite,["./my_SUITE"]\},\{testcase,t1\}]) is
  now interpreted as ct:run_test(\[\{suite,"./my_SUITE"\},\{testcase,t1\}]),
  i.e. only testcase t1 in test suite my_SUITE - not all cases - will be
  executed.

  Own Id: OTP-9155

- A new option, 'logopts', has been introduced, to make it possible to modify
  some aspects of the logging behaviour in Common Test (or Test Server). For
  example, whenever an io printout is made, test_server adds newline (\\n) to
  the end of the output string. This may not always be a preferred action and
  can therefore be disabled by means of "ct_run ... -logopts no_nl" (or
  ct:run_test(\[..., \{logopts,[no_nl]\}])). A new framework callback function,
  get_logopts/0, has been introduced (see the ct_framework module for details).

  Own Id: OTP-9372 Aux Id: OTP-9396

- A new option, 'logopts', has been introduced, to make it possible to modify
  some aspects of the logging behaviour in Common Test (or Test Server). For
  example, if the html version of the test suite source code should not be
  generated during the test run (and consequently be unavailable in the log file
  system), the feature may be disabled by means of "ct_run ... -logopts no_src"
  (or ct:run_test(\[..., \{logopts,[no_src]\}])). A new framework callback
  function, get_logopts/0, has been introduced (see the ct_framework module for
  details).

  Own Id: OTP-9396 Aux Id: seq11869, OTP-9372

- CT Hooks can now be assigned a priority. The priority of a CTH determines when
  it should execute in relation to other CTHs. The CTH with the lowest priority
  will be executed first, CTHs with equal priority will be executed in the order
  which they were installed.

  Own Id: OTP-9445

- It is now possible to use a tuple \{M,F,A\}, or a fun, as timetrap
  specification in the suite info function or test case info functions. The
  function must return a valid timeout value, as documented in the common_test
  man page and in the User's Guide.

  Own Id: OTP-9501 Aux Id: seq11894

- A new built-in common test hook has been added which captures error_logger and
  SASL event and prints them in the testcase log. To disable this (and any other
  built-in hooks) pass 'enable_builtin_hooks false' to common test.

  Own Id: OTP-9543

- Common Test now has the possibility to have built-in hooks which are started
  by default when any test is run. To disable built-in hooks pass
  'enable_builtin_hooks false' to common test. See the common test hooks
  documentation for more details.

  Own Id: OTP-9564

## Common_Test 1.5.4

### Fixed Bugs and Malfunctions

- It was previously not possible to use timetrap value 'infinity' with
  ct:timetrap/1. This has been fixed.

  Own Id: OTP-9159

- The Common Test VTS mode has been updated to be able to report test results of
  suites that include test case groups (when it would previously crash).

  Own Id: OTP-9195

- Common Test now refreshes the very top level index.html page at the start of
  each individual test in a test run, so that progress of the ongoing test can
  be tracked by following the link to its overview page.

  Own Id: OTP-9210 Aux Id: OTP-9054

- A bug that made it impossible to cancel the previous timetrap when calling
  ct:timetrap/1 has been corrected.

  Own Id: OTP-9233 Aux Id: OTP-9159

- Fix bug which would make cth's to not be removed when out of scope when adding
  a cth in suite/0 and crashing in pre_init_per_suite.

  Own Id: OTP-9264

### Improvements and New Features

- It is now possible to return a tuple \{fail,Reason\} from init_per_testcase/2.
  The result is that the associated test case gets logged as failed without ever
  executing.

  Own Id: OTP-9160 Aux Id: seq11502

- Common Test now accepts, but ignores, empty test case group specifications.

  Own Id: OTP-9161

## Common_Test 1.5.3

### Fixed Bugs and Malfunctions

- Added an option to test specs which allow the execution of tests as is,
  instead of doing merging of tests on the same "level". See the merge_tests
  directive the test specification documentation.

  Own Id: OTP-9026 Aux Id: seq11768

### Improvements and New Features

- Alpha release of Common Test Hooks (CTH). CTHs allow the users of common test
  to abstract out common behaviours from test suites in a much more elegant and
  flexible way than was possible before. Note that the addition of this feature
  may introduce minor changes in the undocumented behaviour of the interface
  between common_test and test_server.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8851

## Common_Test 1.5.2

### Fixed Bugs and Malfunctions

- Updated ct:get_status documentation to describe no_tests_running return value.

  Own Id: OTP-8895 Aux Id: seq11701

- Fixed race condition test failures in the test suites testing common test's
  parallel groups feature.

  Own Id: OTP-8921

- The include directive of testspecs now work when used on a remote node.

  Own Id: OTP-8935 Aux Id: seq11731

### Improvements and New Features

- ct:parse_table can now handle multiline sql rows

  Own Id: OTP-8907 Aux Id: seq11702

- The run_test executable has been renamed to the less generic ct_run to better
  work with other applications. run_test will remain until R16B at which point
  it will be removed.

  Own Id: OTP-8936

## Common_Test 1.5.1

### Fixed Bugs and Malfunctions

- Returning \{return_group_result,failed\} from end_per_group in a group that is
  part of a sequence, did not cause the proceeding cases (or groups) to get
  skipped. This has been fixed.

  Own Id: OTP-8753 Aux Id: seq11644

- ct:install now works as the documentation describes.

  Own Id: OTP-8818 Aux Id: seq-11666

### Improvements and New Features

- Common Test has been updated to handle start options and test specification
  terms for test case groups (and test cases in groups). Also, an option named
  'label', has been added that associates the test run with a name that Common
  Test prints in the overview HTML logs.

  Own Id: OTP-8725 Aux Id: OTP-8727

- Andrey Pampukha has been added to the AUTHORS file. Thank you Andrey for your
  work on configuration data handling, Large Scale Testing improvements, and
  other useful updates and fixes.

  Own Id: OTP-8803

- The Configuration Data chapter in the User's Guide has been updated.

  Own Id: OTP-8804

- Milliseconds are now included in timestamps in Common Test log entries.
  (Thanks to Tomas Johansson.)

  Own Id: OTP-8808

## Common_Test 1.5

### Fixed Bugs and Malfunctions

- Process calls using monitors in Common Test would not clear the inbox of
  remaining DOWN messages. This has been fixed.

  Own Id: OTP-8621 Aux Id: seq11560

### Improvements and New Features

- It is now possible for the user to provide specific callback modules that
  handle test configuration data, so that data on arbitrary form can be accessed
  (e.g. by reading files or by communicating with a configuration server
  process). Two default callback modules have been introduced in Common Test:
  ct_config_plain and ct_config_xml. The former is used to handle the
  traditional Common Test configuration files (with terms on key-value tuple
  form) and the latter to handle configuration data on XML representation.

  Own Id: OTP-8485

- It is now possible to execute test suites that are not necessarily available
  on the local file system, but have been loaded on the test node in advance
  (e.g. sent as binaries from a remote node and loaded by RPC). A requirement is
  that the no_auto_compile (or \{auto_compile,false\}) parameter has been set.

  Own Id: OTP-8490 Aux Id: seq11500

- Test Server will now call the end_per_testcase/2 function even if the test
  case has been terminated explicitly (with abort_current_testcase/1), or after
  a timetrap timeout. Under these circumstances the return value of
  end_per_testcase is completely ignored. Therefore the function will not be
  able to change the reason for test case termination by returning
  \{fail,Reason\}, nor will it be able to save data with \{save_config,Data\}.

  Own Id: OTP-8500 Aux Id: seq11521

- It is now possible to use the test specification term 'init' to start Common
  Test nodes automatically, as well as have initial function calls evaluated on
  the nodes. A default callback module for the 'init' term, ct_slave, has been
  introduced to enable Common Test Master to perform host login and node startup
  operations over ssh.

  Own Id: OTP-8570

- The run_test script has been replaced by a program (with the same name) which
  can be executed without explicit installation. The start flags are the same as
  for the legacy start script.

  Own Id: OTP-8650

- Previously, a repeat property of a test case group specified the number of
  times the group should be repeated after the main test run. I.e. \{repeat,N\}
  would case the group to execute 1+N times. To be consistent with the behaviour
  of the run_test repeat option, this has been changed. N now specifies the
  absolute number of executions instead.

  Own Id: OTP-8689 Aux Id: seq11502

- With the run_test -erl_args option, it's possible to divide the options on the
  run_test command line into ones that Common Test should process (those
  preceding -erl_args, and ones it should ignore (those succeeding -erl_args).
  Options preceding -erl_args that Common Test doesn't recognize are also
  ignored (i.e. the same behaviour as earlier versions of Common Test).

  Own Id: OTP-8690 Aux Id: OTP-8650

- Directories added with -pa or -pz in the pre-erl_args part of the run_test
  command line will be converted from relative to absolute, this to avoid
  problems loading user modules when Common Test switches working directory
  during the test run.

  Own Id: OTP-8691 Aux Id: OTP-8650

- The timetrap handling has been made more user controllable by means of new
  start options and new ct interface functions. With the 'multiply_timetraps'
  start option, it's possible to specify a value which all timetrap timeout
  values get multiplied by. This is useful e.g. to extend the timetraps
  temporarily while running cover or trace. The 'scale_timetraps' start option
  switches on or off the Test Server timetrap scaling feature (which tries to
  detect if the tests may benefit from extended timetraps, e.g. due to running
  certain test tools, and performs the scaling automatically). Furthermore, the
  ct:timetrap/1 function has been introduced, which makes it possible to
  set/reset timetraps during test execution. Also, a ct:sleep/1 function is now
  available, which takes the timetrap parameters into account when calculating
  the time to suspend the process.

  Own Id: OTP-8693

- A new run_test start option, event_handler_init, has been added that takes a
  start argument which gets passed to the init function of the event handler.

  Own Id: OTP-8694

## Common_Test 1.4.7

### Fixed Bugs and Malfunctions

- The auto compilation feature of Common Test did not recognize if a header file
  included in a test suite was modified (if the dir start flag/option was used).
  This has been fixed.

  Own Id: OTP-8396 Aux Id: seq11488, OTP-8311

### Improvements and New Features

- The tc_status value in the Config list for a test case that has failed because
  of a timetrap timeout, has changed from \{tc_status,timeout\} to
  \{tc_status,timetrap_timeout\}.

  Own Id: OTP-8302

- The documentation is now possible to build in an open source environment after
  a number of bugs are fixed and some features are added in the documentation
  build process.

  \- The arity calculation is updated.

  \- The module prefix used in the function names for bif's are removed in the
  generated links so the links will look like
  "http://www.erlang.org/doc/man/erlang.html#append_element-2" instead of
  "http://www.erlang.org/doc/man/erlang.html#erlang:append_element-2".

  \- Enhanced the menu positioning in the html documentation when a new page is
  loaded.

  \- A number of corrections in the generation of man pages (thanks to Sergei
  Golovan)

  \- The legal notice is taken from the xml book file so OTP's build process can
  be used for non OTP applications.

  Own Id: OTP-8343

- It is now possible to include the `ct.hrl` using the -include_lib directive.
  (Thanks to Fred Hebert.)

  Own Id: OTP-8379

- The telnet client in Common Test sent \[IAC,DO,NOP] to the server in attempt
  to keep the connection alive. This is not a valid sequence according to the
  standard, and some telnet servers would terminate the connection because of
  it. The client has been changed to send \[IAC,NOP] every 10 secs instead,
  which should be a valid sequence. The client does not negotiate this type of
  "keep alive" message with the server, and if it causes problems, the user may
  disable the keep alive feature by adding \{keep_alive,false\} to the telnet
  configuration data for the server/connection. Please see the ct_telnet and
  unix_telnet manual pages for details.

  Own Id: OTP-8450 Aux Id: OTP-8311

## Common_Test 1.4.6

### Fixed Bugs and Malfunctions

- If the init_per_testcase/2 function fails, the test case now gets marked and
  counted as auto skipped, not user skipped (which would previously happen).

  Own Id: OTP-8289

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

- For a failed test case, the tc_done event is supposed to report info on the
  form \{failed,Error\}. Only Error was reported, however, which has now been
  fixed.

  Own Id: OTP-8235 Aux Id: seq-11414

- It is now possible to fail a test case from the end_per_testcase/2 function,
  by returning \{fail,Reason\}.

  Own Id: OTP-8284

- It is now possible to fail a test case by having the end_tc/3 framework
  function return \{fail,Reason\} for the test case.

  Own Id: OTP-8285

- The test_server framework API (e.g. the end_tc/3 function) has been modified.
  See the test_server_ctrl documentation for details.

  Own Id: OTP-8286 Aux Id: OTP-8285, OTP-8287

- Various updates of the test events have been implemented. The data field for
  some events, such as tc_done and tc_auto_skip has been modified to make
  pattern matching on the data easier and more consistent. Also the order in
  which some events are received has been altered. E.g. the tc_auto_skip event
  for a test case now comes after the tc_done for the failed configuration
  function (not before) which makes more sense. Note that no new events have
  been added and that the event record remains unchanged.

  Own Id: OTP-8287 Aux Id: OTP-8235

- The marquee used for test names on the all_runs.html page has been removed on
  request. Note that the test name field has the full text string in a title
  tag, which is displayed when hovering the mouse pointer over it (i.e. if the
  web browser supports title tags).

  Own Id: OTP-8288

- It is now possible to refresh the top level index files in an arbitrary log
  directory by specifying a \{refresh_logs,LogDir\} tuple in the ct:run_test/1
  options list. Also the -refresh_logs flag for the run_test script has been
  extended to take an optional LogDir argument, i.e. -refresh_logs \[LogDir]. If
  no LogDir is specified, current working directory is assumed, unless the log
  directory is set with the -logdir flag.

  Own Id: OTP-8290

- It was previously required that test suites were located under a test object
  (or OTP application) sub-directory named "test" (or under a directory named
  "<testobject>\_test"). This has been changed so that Common Test now looks for
  suites primarily in a test sub-directory only if the directory exists.
  Otherwise it will assume the suites are stored in the same directory the user
  specifies with e.g. the 'dir' start flag/option.

  Own Id: OTP-8294

## Common_Test 1.4.5

### Fixed Bugs and Malfunctions

- The Common Test logger process crashed if a test case in a sequence (declared
  with sequences/0) failed. This fault has been corrected.

  Own Id: OTP-8089 Aux Id: seq11334

### Improvements and New Features

- Various updates and fixes in Common Test and Test Server.

  Own Id: OTP-8045 Aux Id: OTP-8089,OTP-8105,OTP-8163

- Errors in coverage data collection and analysis were difficult to detect. The
  logging has been improved so that more information about e.g. imported and
  missing modules is printed to the html log files.

  Own Id: OTP-8163 Aux Id: seq11374

- The Common Test HTML overview pages have been improved. It is now possible to
  see if a test case has been skipped explicitly or because a configuration
  function has failed. Also, the history page (all_runs.html) now has scrolling
  text displaying the test names. The old format (showing names as a truncated
  string) can still be generated by means of the flag/option 'basic_html'.

  Own Id: OTP-8177

## Common_Test 1.4.2

### Improvements and New Features

- Various corrections and improvements of Common Test and Test Server.

  Own Id: OTP-7981

## Common_Test 1.4.1

### Improvements and New Features

- Minor updates and corrections.

  Own Id: OTP-7897

## Common_Test 1.4

### Improvements and New Features

- A support client module for SSH and SFTP, ct_ssh, has been introduced in
  Common Test.

  Own Id: OTP-7838

- Test case groups have been introduced. With this feature it's possible to
  execute groups (possibly nested) of test cases, each group wrapped with a call
  to function init_per_group/2 and end_per_group/2. Group definitions are done
  by means of the new call-back function groups/0, which should return a list of
  definitions. A group definition contains a name tag, a list of properties and
  a list of test cases (including possible nested group definitions). The
  properties make it possible to execute test cases in parallel, in sequence and
  in shuffled order. It is also possible to repeat test cases according to
  different criteria. The properties can be combined, making it possible to e.g.
  repeat a conf case a certain number of times and execute the test cases in
  different (random) order every time. Available properties are: parallel,
  sequence, shuffle, repeat, repeat_until_all_ok, repeat_until_any_ok,
  repeat_until_any_fail and repeat_until_all_fail. Please see the Common Test
  User's Guide for details.

  Own Id: OTP-7839 Aux Id: OTP-7511

- It is now possible to use DES3 encrypted configuration files with Common Test.

  Own Id: OTP-7842 Aux Id: OTP-7838

- In previous versions of Common Test, only one FTP connection could be opened
  per configuration target name. This has been updated so that multiple
  connections may be opened. The possibility to use named connections is still
  supported.

  Own Id: OTP-7853 Aux Id: OTP-7838

- The Erlang mode for Emacs has been updated with new and modified skeletons for
  Common Test and TS. Syntax for test case groups in Common Test (and conf cases
  with properties in TS) has been added and a new minimal Common Test suite
  skeleton has been introduced.

  Own Id: OTP-7856

## Common_Test 1.3.6

### Fixed Bugs and Malfunctions

- When running a test which includes all suites in a test directory, if the auto
  compilation would fail for one suite, all following suites would be excluded
  from the test. This was an unwanted behaviour and has been corrected. Now all
  suites will always be compiled and only the failing ones excluded from the
  test (and logged as missing).

  Own Id: OTP-7750 Aux Id: OTP-7803

- The step functionality in Common Test (based on interaction with Debugger) was
  broken. This has been fixed, and some new step features have also been added.
  Please see the Common Test User's Guide for details.

  Own Id: OTP-7800 Aux Id: seq11106

### Improvements and New Features

- It is now possible for the user to specify include directories that Common
  Test will pass along to the compiler when suite and help modules are being
  compiled (which Common Test performs automatically before running tests).

  Own Id: OTP-7803 Aux Id: OTP-7750

## Common_Test 1.3.5

### Fixed Bugs and Malfunctions

- If the Erlang runtime system was started without access to an erlang shell
  (e.g. -noshell), compilation errors would cause a crash in the Common Test
  application. Without access to a shell, Common Test cannot prompt the user to
  choose to continue or abort the test session, but must assume that the session
  should proceed.

  Own Id: OTP-7749 Aux Id: seq11175, seq11180

### Improvements and New Features

- It is now possible for the Common Test user to disable the auto-compile
  feature. This is done by specifying the run_test flag -no_auto_compile, or the
  ct:run_test/1 option \{auto_compile,false\}.

  Own Id: OTP-7663

- A new function, ct:get_config/3, has been added to Common Test that makes it
  possible to - if a particular config variable has been defined in multiple
  config files - return all matching values for the variable. The order of the
  elements in the returned list is the same as the specified order of the config
  files.

  Own Id: OTP-7758 Aux Id: seq11158

- Because a telnet connection was always identified by a config variable alias,
  it was impossible to open multiple connections using the same telnet host data
  entry in the config file. This limitation has been removed by making it
  possible to associate a connection with handle value only (i.e. multiple
  connections may be opened using the same config variable). See
  ct_telnet:open/4 for details.

  Own Id: OTP-7781

- A new syntax for defining default config data values has been introduced. In
  previous versions of Common Test, to define and access a default value for a
  config variable (in the suite info- or test case info function), an alias name
  had to be used. With the new syntax you may define default values without
  reference to aliases, like this: \{default_config,VarName,DefaultValue\}.
  Please see the User's Guide for more info.

  Own Id: OTP-7782

- In previous versions of Common Test, whenever a config variable got associated
  with a name (by means of a require statement), the config variable name was
  replaced with the new name. This introduced unwanted dependencies between test
  cases (e.g. if one test case would introduce a new name, the following test
  cases could no longer access the config data by means of the original
  variable). This functionality has now been updated so that when new names are
  introduced with require, they become aliases (references) instead of
  replacements. Hence, config data elements can always, at any time, be accessed
  by means of the original config variable names.

  Own Id: OTP-7783

## Common_Test 1.3.4

### Improvements and New Features

- Common Test now uses the re application instead of the previous rx driver to
  perform regular expression matching on telnet strings. Since re works on all
  supported operating systems, it is now possible to run telnet sessions also on
  platforms such as e.g. Windows (which was not the case with the previous rx
  driver). Note that the rx driver is obsolete from now on, and will be removed
  from Common Test after OTP R12B.

  Own Id: OTP-7528

## Common_Test 1.3.3

### Improvements and New Features

- Various updates and improvements, plus some minor bug fixes, have been
  implemented in Common Test and Test Server.

  Own Id: OTP-7112

- It is now possible, by means of the new function ct:abort_current_testcase/1
  or test_server_ctrl:abort_current_testcase/1, to abort the currently executing
  test case.

  Own Id: OTP-7518 Aux Id: OTP-7112

## Common_Test 1.3.2

### Improvements and New Features

- The configure test of the rx lib in Common Test was not performed during the
  general OTP application configuration phase. This made e.g. autoconf
  impossible. This has been changed to correspond with the normal OTP build
  procedure.

  Own Id: OTP-7379

## Common_Test 1.3.1

### Improvements and New Features

- The rx library, included with common_test, failed to build on some
  architectures because the -fPIC compiler option was missing.

  Own Id: OTP-7111

## common_test 1.3.0
