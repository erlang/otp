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
# Common Test Basics

[](){: #basics }

## General

The `Common Test` framework is a tool that supports implementation and automated
execution of test cases to any types of target systems. `Common Test` is the
main tool being used in all testing- and verification activities that are part
of Erlang/OTP system development and maintenance.

Test cases can be executed individually or in batches. `Common Test` also
features a distributed testing mode with central control and logging. With this
feature, multiple systems can be tested independently in one common session.
This is useful, for example, when running automated large-scale regression
tests.

The System Under Test (SUT) can consist of one or more target nodes.
`Common Test` contains a generic test server that, together with other test
utilities, is used to perform test case execution. The tests can be started from
a GUI, from the OS shell, or from an Erlang shell. _Test suites_ are files
(Erlang modules) that contain the _test cases_ (Erlang functions) to be
executed. _Support modules_ provide functions that the test cases use to do the
tests.

In a black-box testing scenario, `Common Test`\-based test programs connect to
the target system(s) through standard O&M and CLI protocols. `Common Test`
provides implementations of, and wrapper interfaces to, some of these protocols
(most of which exist as standalone components and applications in OTP). The
wrappers simplify configuration and add verbosity for logging purposes.
`Common Test` is continuously extended with useful support modules. However,
notice that it is a straightforward task to use any Erlang/OTP component for
testing purposes with `Common Test`, without needing a `Common Test` wrapper for
it. It is as simple as calling Erlang functions. A number of target-independent
interfaces are supported in `Common Test`, such as Generic Telnet and FTP. These
can be specialized or used directly for controlling instruments, traffic load
generators, and so on.

`Common Test` is also a very useful tool for white-box testing Erlang code (for
example, module testing), as the test programs can call exported Erlang
functions directly. There is very little overhead required for implementing
basic test suites and executing simple tests. For black-box testing Erlang
software, Erlang RPC and standard O&M interfaces can be used for example.

A test case can handle several connections to one or more target systems,
instruments, and traffic generators in parallel to perform the necessary actions
for a test. The handling of many connections in parallel is one of the major
strengths of `Common Test`, thanks to the efficient support for concurrency in
the Erlang runtime system, which `Common Test` users can take great advantage
of.

## Test Suite Organization

Test suites are organized in test directories and each test suite can have a
separate data directory. Typically, these files and directories are
version-controlled similar to other forms of source code (possibly by a version
control system like GIT or Subversion). However, `Common Test` does not itself
put any requirements on (or has any awareness of) possible file and directory
versions.

## Support Libraries

Support libraries contain functions that are useful for all test suites, or for
test suites in a specific functional area or subsystem. In addition to the
general support libraries provided by the `Common Test` framework, and the
various libraries and applications provided by Erlang/OTP, there can also be a
need for customized (user specific) support libraries.

## Suites and Test Cases

Testing is performed by running test suites (sets of test cases) or individual
test cases. A test suite is implemented as an Erlang module named
`<suite_name>_SUITE.erl` which contains a number of test cases. A test case is
an Erlang function that tests one or more things. The test case is the smallest
unit that the `Common Test` test server deals with.

Sets of test cases, called test case groups, can also be defined. A test case
group can have execution properties associated with it. Execution properties
specify if the test cases in the group are to be executed in random order, in
parallel, or in sequence, and if the execution of the group is to be repeated.
Test case groups can also be nested (that is, a group can, besides test cases,
contain subgroups).

Besides test cases and groups, the test suite can also contain configuration
functions. These functions are meant to be used for setting up (and verifying)
environment and state in the SUT (and/or the `Common Test` host node), required
for the tests to execute correctly. Examples of operations are: Opening a
connection to the SUT, initializing a database, running an installation script,
and so on. Configuration can be performed per suite, per test case group, and
per individual test case.

The test suite module must conform to a [callback interface](`m:ct_suite`)
specified by the `Common Test` test server. For details, see section
[Writing Test Suites](write_test_chapter.md#intro).

A test case is considered successful if it returns to the caller, no matter what
the returned value is. However, a few return values have special meaning as
follows:

- `{skip,Reason}` indicates that the test case is skipped.
- `{comment,Comment}` prints a comment in the log for the test case.
- `{save_config,Config}` makes the `Common Test` test server pass `Config` to
  the next test case.

A test case failure is specified as a runtime error (a crash), no matter what
the reason for termination is. If you use Erlang pattern matching effectively,
you can take advantage of this property. The result is concise and readable test
case functions that look much more like scripts than actual programs. A simple
example:

```erlang
 session(_Config) ->
     {started,ServerId} = my_server:start(),
     {clients,[]} = my_server:get_clients(ServerId),
     MyId = self(),
     connected = my_server:connect(ServerId, MyId),
     {clients,[MyId]} = my_server:get_clients(ServerId),
     disconnected = my_server:disconnect(ServerId, MyId),
     {clients,[]} = my_server:get_clients(ServerId),
     stopped = my_server:stop(ServerId).
```

As a test suite runs, all information (including output to `stdout`) is recorded
in many different log files. A minimum of information is displayed in the user
console (only start and stop information, plus a note for each failed test
case).

The result from each test case is recorded in a dedicated HTML log file, created
for the particular test run. An overview page displays each test case
represented by a table row showing total execution time, if the case was
successful, failed, or skipped, plus an optional user comment. For a failed test
case, the reason for termination is also printed in the comment field. The
overview page has a link to each test case log file, providing simple navigation
with any standard HTML browser.

[](){: #External_Interfaces }

## External Interfaces

The `Common Test` test server requires that the test suite defines and exports
the following mandatory or optional callback functions:

- **`all()`** - Returns a list of all test cases and groups in the suite.
  (Mandatory)

- **`suite()`** - Information function used to return properties for the suite.
  (Optional)

- **`groups()`** - For declaring test case groups. (Optional)

- **`init_per_suite(Config)`** - Suite level configuration function, executed
  before the first test case. (Optional)

- **`end_per_suite(Config)`** - Suite level configuration function, executed
  after the last test case. (Optional)

- **`group(GroupName)`** - Information function used to return properties for a
  test case group. (Optional)

- **`init_per_group(GroupName, Config)`** - Configuration function for a group,
  executed before the first test case. (Optional)

- **`end_per_group(GroupName, Config)`** - Configuration function for a group,
  executed after the last test case. (Optional)

- **`init_per_testcase(TestCase, Config)`** - Configuration function for a
  testcase, executed before each test case. (Optional)

- **`end_per_testcase(TestCase, Config)`** - Configuration function for a
  testcase, executed after each test case. (Optional)

For each test case, the `Common Test` test server expects the following
functions:

- **Testcasename()** - Information function that returns a list of test case
  properties. (Optional)

- **Testcasename(Config)** - The test case function.
