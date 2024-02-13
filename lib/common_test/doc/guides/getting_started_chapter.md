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
# Getting Started

## Introduction for Newcomers

The purpose of this section is to let the newcomer get started in quickly
writing and executing some first simple tests with a "learning by example"
approach. Most explanations are left for later sections. If you are not much
into "learning by example" and prefer more technical details, go ahead and skip
to the next section.

This section demonstrates how simple it is to write a basic (yet for many module
testing purposes, often sufficiently complex) test suite and execute its test
cases. This is not necessarily obvious when you read the remaining sections in
this User's Guide.

> #### Note {: .info }
>
> To understand what is discussed and examplified here, we recommended you to
> first read section [Common Test Basics](basics_chapter.md#basics).

## Test Case Execution

Execution of test cases is handled as follows:

![Successful and Unsuccessful Test Case Execution](assets/tc_execution.gif "Successful and Unsuccessful Test Case Execution")

For each test case that `Common Test` is ordered to execute, it spawns a
dedicated process on which the test case function starts running. (In parallel
to the test case process, an idle waiting timer process is started, which is
linked to the test case process. If the timer process runs out of waiting time,
it sends an exit signal to terminate the test case process. This is called a
_timetrap_).

In scenario 1, the test case process terminates normally after `case A` has
finished executing its test code without detecting any errors. The test case
function returns a value and `Common Test` logs the test case as successful.

In scenario 2, an error is detected during test `case B` execution. This causes
the test `case B` function to generate an exception and, as a result, the test
case process exits with reason other than normal. `Common Test` logs this as an
unsuccessful (Failed) test case.

As you can understand from the illustration, `Common Test` requires a test case
to generate a runtime error to indicate failure (for example, by causing a bad
match error or by calling [`exit/1`](`exit/1`), preferably through the help
function [`ct:fail/1,2`](`ct:fail/1`)). A successful execution is indicated by a
normal return from the test case function.

## A Simple Test Suite

As shown in section [Common Test Basics](basics_chapter.md#External_Interfaces),
the test suite module implements [callback functions](`m:ct_suite`) (mandatory
or optional) for various purposes, for example:

- Init/end configuration function for the test suite
- Init/end configuration function for a test case
- Init/end configuration function for a test case group
- Test cases

The configuration functions are optional. The following example is a test suite
without configuration functions, including one simple test case, to check that
module `mymod` exists (that is, can be successfully loaded by the code server):

```erlang
 -module(my1st_SUITE).
 -compile(export_all).

 all() ->
     [mod_exists].

 mod_exists(_) ->
     {module,mymod} = code:load_file(mymod).
```

If the operation fails, a bad match error occurs that terminates the test case.

## A Test Suite with Configuration Functions

If you need to perform configuration operations to run your test, you can
implement configuration functions in your suite. The result from a configuration
function is configuration data, or `Config`. This is a list of key-value tuples
that get passed from the configuration function to the test cases (possibly
through configuration functions on "lower level"). The data flow looks as
follows:

![Configuration Data Flow in a Suite](assets/config.gif "Configuration Data Flow in a Suite")

The following example shows a test suite that uses configuration functions to
open and close a log file for the test cases (an operation that is unnecessary
and irrelevant to perform by each test case):

```erlang
 -module(check_log_SUITE).
 -export([all/0, init_per_suite/1, end_per_suite/1]).
 -export([check_restart_result/1, check_no_errors/1]).

 -define(value(Key,Config), proplists:get_value(Key,Config)).

 all() -> [check_restart_result, check_no_errors].

 init_per_suite(InitConfigData) ->
     [{logref,open_log()} | InitConfigData].

 end_per_suite(ConfigData) ->
     close_log(?value(logref, ConfigData)).

 check_restart_result(ConfigData) ->
     TestData = read_log(restart, ?value(logref, ConfigData)),
     {match,_Line} = search_for("restart successful", TestData).

 check_no_errors(ConfigData) ->
     TestData = read_log(all, ?value(logref, ConfigData)),
     case search_for("error", TestData) of
	 {match,Line} -> ct:fail({error_found_in_log,Line});
	 nomatch -> ok
     end.
```

The test cases verify, by parsing a log file, that our SUT has performed a
successful restart and that no unexpected errors are printed.

To execute the test cases in the recent test suite, type the following on the
UNIX/Linux command line (assuming that the suite module is in the current
working directory):

```text
 $ ct_run -dir .
```

or:

```text
 $ ct_run -suite check_log_SUITE
```

To use the Erlang shell to run our test, you can evaluate the following call:

```erlang
 1> ct:run_test([{dir, "."}]).
```

or:

```erlang
 1> ct:run_test([{suite, "check_log_SUITE"}]).
```

The result from running the test is printed in log files in HTML format (stored
in unique log directories on a different level). The following illustration
shows the log file structure:

![HTML Log File Structure](assets/html_logs.gif "HTML Log File Structure")

## Questions and Answers

Here follows some questions that you might have after reading this section with
corresponding tips and links to the answers:

- _Question:_ "How and where can I specify variable data for my tests that must
  not be hard-coded in the test suites (such as hostnames, addresses, and user
  login data)?"

  _Answer:_ See section
  [External Configuration Data](config_file_chapter.md#top).

- _Question:_ "Is there a way to declare different tests and run them in one
  session without having to write my own scripts? Also, can such declarations be
  used for regression testing?"

  _Answer:_ See section
  [Test Specifications](run_test_chapter.md#test_specifications) in section
  Running Tests and Analyzing Results.

- _Question:_ "Can test cases and/or test runs be automatically repeated?"

  _Answer:_ Learn more about
  [Test Case Groups](write_test_chapter.md#test_case_groups) and read about
  start flags/options in section [Running Tests](run_test_chapter.md#ct_run) and
  in the Reference Manual.

- _Question:_ "Does `Common Test` execute my test cases in sequence or in
  parallel?"

  _Answer:_ See [Test Case Groups](write_test_chapter.md#test_case_groups) in
  section Writing Test Suites.

- _Question:_ "What is the syntax for timetraps (mentioned earlier), and how do
  I set them?"

  _Answer:_ This is explained in the
  [Timetrap Time-Outs](write_test_chapter.md#timetraps) part of section Writing
  Test Suites.

- _Question:_ "What functions are available for logging and printing?"

  _Answer:_ See [Logging](write_test_chapter.md#logging) in section Writing Test
  Suites.

- _Question:_ "I need data files for my tests. Where do I store them
  preferably?"

  _Answer:_ See
  [Data and Private Directories](write_test_chapter.md#data_priv_dir).

- _Question:_ "Can I start with a test suite example, please?"

  _Answer:_ [Welcome\!](example_chapter.md#top)

You probably want to get started on your own first test suites now, while at the
same time digging deeper into the `Common Test` User's Guide and Reference
Manual. There are much more to learn about the things that have been introduced
in this section. There are also many other useful features to learn, so please
continue to the other sections and have fun.
