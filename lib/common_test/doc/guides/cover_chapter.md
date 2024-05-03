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
# Code Coverage Analysis

[](){: #cover }

## General

Although `Common Test` was created primarily for black-box testing, nothing
prevents it from working perfectly as a white-box testing tool as well. This is
especially true when the application to test is written in Erlang. Then the test
ports are easily realized with Erlang function calls.

When white-box testing an Erlang application, it is useful to be able to measure
the code coverage of the test. `Common Test` provides simple access to the OTP
Cover tool for this purpose. `Common Test` handles all necessary communication
with the Cover tool (starting, compiling, analysing, and so on). The
`Common Test` user only needs to specify the extent of the code coverage
analysis.

## Use

To specify the modules to be included in the code coverage test, provide a cover
specification file. With this file you can point out specific modules or specify
directories containing modules to be included in the analysis. You can also
specify modules to be excluded from the analysis.

If you are testing a distributed Erlang application, it is likely that code you
want included in the code coverage analysis gets executed on another Erlang node
than the one `Common Test` is running on. If so, you must specify these other
nodes in the cover specification file or add them dynamically to the code
coverage set of nodes. For details on the latter, see module `m:ct_cover`.

In the cover specification file you can also specify your required level of the
code coverage analysis; `details` or `overview`. In detailed mode, you get a
coverage overview page, showing per module and total coverage percentages. You
also get an HTML file printed for each module included in the analysis showing
exactly what parts of the code have been executed during the test. In overview
mode, only the code coverage overview page is printed.

You can choose to export and import code coverage data between tests. If you
specify the name of an export file in the cover specification file,
`Common Test` exports collected coverage data to this file at the end of the
test. You can similarly specify previously exported data to be imported and
included in the analysis for a test (multiple import files can be specified).
This way, the total code coverage can be analyzed without necessarily running
all tests at once.

To activate the code coverage support, specify the name of the cover
specification file as you start `Common Test`. Do this by using flag `-cover`
with [`ct_run`](ct_run_cmd.md), for example:

```text
 $ ct_run -dir $TESTOBJS/db -cover $TESTOBJS/db/config/db.coverspec
```

You can also pass the cover specification file name in a call to
`ct:run_test/1`, by adding a `{cover,CoverSpec}` tuple to argument `Opts`.

You can also enable code coverage in your test specifications (see section
[Test Specifications](run_test_chapter.md#test_specifications) in section
Running Tests and Analyzing Results).

[](){: #cover_stop }

## Stopping the Cover Tool When Tests Are Completed

By default, the Cover tool is automatically stopped when the tests are
completed. This causes the original (non-cover compiled) modules to be loaded
back into the test node. If a process at this point still runs old code of any
of the modules that are cover compiled, meaning that it has not done any fully
qualified function call after the cover compilation, the process is killed. To
avoid this, set the value of option `cover_stop` to `false`. This means that the
modules stay cover compiled. Therefore, this is only recommended if the Erlang
nodes under test are terminated after the test is completed, or if cover can be
manually stopped.

The option can be set by using flag `-cover_stop` with `ct_run`, by adding
`{cover_stop,true|false}` to argument `Opts` to `ct:run_test/1`, or by adding a
`cover_stop` term in the test specification (see section
[Test Specifications](run_test_chapter.md#test_specifications) in section
Running Tests and Analyzing Results).

## The Cover Specification File

### General Config

Here follows the general configuration terms that are allowed in a cover
specification file:

```erlang
 %% List of Nodes on which cover will be active during test.
 %% Nodes = [atom()]
 {nodes, Nodes}.

 %% Files with previously exported cover data to include in analysis.
 %% CoverDataFiles = [string()]
 {import, CoverDataFiles}.

 %% Cover data file to export from this session.
 %% CoverDataFile = string()
 {export, CoverDataFile}.

 %% Cover analysis level.
 %% Level = details | overview
 {level, Level}.

 %% Directories to include in cover.
 %% Dirs = [string()]
 {incl_dirs, Dirs}.

 %% Directories, including subdirectories, to include.
 {incl_dirs_r, Dirs}.

 %% Specific modules to include in cover.
 %% Mods = [atom()]
 {incl_mods, Mods}.

 %% Directories to exclude in cover.
 {excl_dirs, Dirs}.

 %% Directories, including subdirectories, to exclude.
 {excl_dirs_r, Dirs}.

 %% Specific modules to exclude in cover.
 {excl_mods, Mods}.

 %% Cross cover compilation
 %% Tag = atom(), an identifier for a test run
 %% Mod = [atom()], modules to compile for accumulated analysis
 {cross,[{Tag,Mods}]}.
```

The terms `incl_dirs_r` and `excl_dirs_r` tell `Common Test` to search the
specified directories recursively and include or exclude any module found during
the search. The terms `incl_dirs` and `excl_dirs` result in a non-recursive
search for modules (that is, only modules found in the specified directories are
included or excluded).

> #### Note {: .info }
>
> Directories containing Erlang modules to be included in a code coverage test
> must exist in the code server path. Otherwise, the Cover tool fails to
> recompile the modules. It is not sufficient to specify these directories in
> the cover specification file for `Common Test`.

### OTP application Config

When using a cover specification in the testing of an OTP application itself,
there is a special incl_app directive that includes the applications modules for
the cover compilation.

```text
{incl_app, AppName, Cover:: overview | details}.
```

> #### Note {: .info }
>
> If you desire to also use some other general cover configuration together with
> this option you should insert the AppName in between the option and its value
> creating a three tuple.

[](){: #cross_cover }

## Cross Cover Analysis

The cross cover mechanism allows cover analysis of modules across multiple
tests. It is useful if some code, for example, a library module, is used by many
different tests and the accumulated cover result is desirable.

This can also be achieved in a more customized way by using parameter `export`
in the cover specification and analysing the result off line. However, the cross
cover mechanism is a built-in solution that also provides logging.

The mechanism is easiest explained by an example:

Assume that there are two systems, `s1` and `s2`, that are tested in separate
test runs. System `s1` contains a library module `m1` tested by test run `s1`
and is included in the cover specification of `s1` as follows:

```text
 s1.cover:
   {incl_mods,[m1]}.
```

When analysing code coverage, the result for `m1` can be seen in the cover log
in the `s1` test result.

Now, imagine that as `m1` is a library module, it is also often used by system
`s2`. Test run `s2` does not specifically test `m1`, but it can still be
interesting to see which parts of `m1` that are covered by the `s2` tests. To do
this, `m1` can be included also in the cover specification of `s2` as follows:

```text
 s2.cover:
   {incl_mods,[m1]}.
```

This gives an entry for `m1` also in the cover log for test run `s2`. The
problem is that this only reflects the coverage by `s2` tests, not the
accumulated result over `s1` and `s2`. This is where the cross cover mechanism
comes in handy.

If instead the cover specification for `s2` is like the following:

```erlang
 s2.cover:
   {cross,[{s1,[m1]}]}.
```

Then `m1` is cover compiled in test run `s2`, but not shown in the coverage log.
Instead, if `ct_cover:cross_cover_analyse/2` is called after both `s1` and `s2`
test runs are completed, the accumulated result for `m1` is available in the
cross cover log for test run `s1`.

The call to the analyze function must be as follows:

```erlang
 ct_cover:cross_cover_analyse(Level, [{s1,S1LogDir},{s2,S2LogDir}]).
```

Here, `S1LogDir` and `S2LogDir` are the directories named `<TestName>.logs` for
each test respectively.

Notice the tags `s1` and `s2`, which are used in the cover specification file
and in the call to `ct_cover:cross_cover_analyse/2`. The purpose of these is
only to map the modules specified in the cover specification to the log
directory specified in the call to the analyze function. The tag name has no
meaning beyond this.

## Logging

To view the result of a code coverage test, click the button labeled "COVER LOG"
in the top-level index page for the test run.

Before Erlang/OTP 17.1, if your test run consisted of multiple tests, cover
would be started and stopped for each test within the test run. Separate logs
would be available through the "Coverage log" link on the test suite result
pages. These links are still available, but now they all point to the same page
as the button on the top-level index page. The log contains the accumulated
results for the complete test run. For details about this change, see the
release notes.

The button takes you to the code coverage overview page. If you have
successfully performed a detailed coverage analysis, links to each individual
module coverage page are found here.

If cross cover analysis is performed, and there are accumulated coverage results
for the current test, the link "Coverdata collected over all tests" takes you to
these results.
