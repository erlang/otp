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
# Test Structure

## General

A test is performed by running one or more test suites. A test suite consists of
test cases, configuration functions, and information functions. Test cases can
be grouped in so called test case groups. A test suite is an Erlang module and
test cases are implemented as Erlang functions. Test suites are stored in test
directories.

[](){: #skipping_test_cases }

## Skipping Test Cases

Certain test cases can be skipped, for example, if you know beforehand that a
specific test case fails. The reason can be functionality that is not yet
implemented, a bug that is known but not yet fixed, or some functionality that
does not work or is not applicable on a specific platform.

Test cases can be skipped in the following ways:

- Using `skip_suites` and `skip_cases` terms in
  [test specifications](run_test_chapter.md#test_specifications).
- Returning `{skip,Reason}` from function
  [`init_per_testcase/2`](`c:ct_suite:init_per_testcase/2`) or
  [`init_per_suite/1`](`c:ct_suite:init_per_suite/1`).
- Returning `{skip,Reason}` from the execution clause of the test case. The
  execution clause is called, so the author must ensure that the test case does
  not run.

When a test case is skipped, it is noted as `SKIPPED` in the HTML log.

## Definition of Terms

- **_Auto-skipped test case_** - When a configuration function fails (that is,
  terminates unexpectedly), the test cases depending on the configuration
  function are skipped automatically by `Common Test`. The status of the test
  cases is then "auto-skipped". Test cases are also "auto-skipped" by
  `Common Test` if the required configuration data is unavailable at runtime.

- **_Configuration function_** - A function in a test suite that is meant to be
  used for setting up, cleaning up, and/or verifying the state and environment
  on the System Under Test (SUT) and/or the `Common Test` host node, so that a
  test case (or a set of test cases) can execute correctly.

- **_Configuration file_** - A file containing data related to a test and/or an
  SUT, for example, protocol server addresses, client login details, and
  hardware interface addresses. That is, any data that is to be handled as
  variable in the suite and not be hard-coded.

- **_Configuration variable_** - A name (an Erlang atom) associated with a data
  value read from a configuration file.

- **`data_dir`** - Data directory for a test suite. This directory contains any
  files used by the test suite, for example, extra Erlang modules, binaries, or
  data files.

- **_Information function_** - A function in a test suite that returns a list of
  properties (read by the `Common Test` server) that describes the conditions
  for executing the test cases in the suite.

- **_Major log file_** - An overview and summary log file for one or more test
  suites.

- **_Minor log file_** - A log file for one particular test case. Also called
  the test case log file.

- **`priv_dir`** - Private directory for a test suite. This directory is to be
  used when the test suite needs to write to files.

- **`ct_run`** - The name of an executable program that can be used as an
  interface for specifying and running tests with `Common Test`.

- **_Test case_** - A single test included in a test suite. A test case is
  implemented as a function in a test suite module.

- **_Test case group_** - A set of test cases sharing configuration functions
  and execution properties. The execution properties specify if the test cases
  in the group are to be executed in random order, in parallel, or in sequence,
  and if the execution of the group is be repeated. Test case groups can also be
  nested. That is, a group can, besides test cases, contain subgroups.

- **_Test suite_** - An Erlang module containing a collection of test cases for
  a specific functional area.

- **_Test directory_** - A directory containing one or more test suite modules,
  that is, a group of test suites.

- **_Argument_ `Config`** - A list of key-value tuples (that is, a property
  list) containing runtime configuration data passed from the configuration
  functions to the test cases.

- **_User-skipped test case_** - The status of a test case explicitly skipped in
  any of the ways described in section
  [Skipping Test Cases](test_structure_chapter.md#skipping_test_cases).
