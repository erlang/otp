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
# Running Tests and Analyzing Results

## Using the Common Test Framework

The `Common Test` framework provides a high-level operator interface for
testing, providing the following features:

- Automatic compilation of test suites (and help modules)
- Creation of extra HTML pages for improved overview.
- Single-command interface for running all available tests
- Handling of configuration files specifying data related to the System Under
  Test (SUT) (and any other variable data)
- Mode for running multiple independent test sessions in parallel with central
  control and configuration

## Automatic Compilation of Test Suites and Help Modules

When `Common Test` starts, it automatically attempts to compile any suites
included in the specified tests. If particular suites are specified, only those
suites are compiled. If a particular test object directory is specified (meaning
all suites in this directory are to be part of the test), `Common Test` runs
function `make:all/1` in the directory to compile the suites.

If compilation fails for one or more suites, the compilation errors are printed
to tty and the operator is asked if the test run is to proceed without the
missing suites, or be aborted. If the operator chooses to proceed, the tests
having missing suites are noted in the HTML log. If `Common Test` is unable to
prompt the user after compilation failure (if `Common Test` does not control
`stdin`), the test run proceeds automatically without the missing suites. This
behavior can however be modified with the `ct_run` flag
`-abort_if_missing_suites`, or the `ct:run_test/1` option
`{abort_if_missing_suites,TrueOrFalse}`. If `abort_if_missing_suites` is set to
`true`, the test run stops immediately if some suites fail to compile.

Any help module (that is, regular Erlang module with name not ending with
"\_SUITE") that resides in the same test object directory as a suite, which is
part of the test, is also automatically compiled. A help module is not mistaken
for a test suite (unless it has a "\_SUITE" name). All help modules in a
particular test object directory are compiled, no matter if all or only
particular suites in the directory are part of the test.

If test suites or help modules include header files stored in other locations
than the test directory, these include directories can be specified by using
flag `-include` with [`ct_run`](ct_run_cmd.md), or option `include` with
`ct:run_test/1`. Also, an include path can be specified with an OS environment
variable, `CT_INCLUDE_PATH`.

_Example (bash):_

`$ export CT_INCLUDE_PATH=~testuser/common_suite_files/include:~testuser/common_lib_files/include`

`Common Test` passes all include directories (specified either with flag/option
`include`, or variable `CT_INCLUDE_PATH` , or both, to the compiler.

Include directories can also be specified in test specifications, see
[Test Specifications](run_test_chapter.md#test_specifications).

If the user wants to run all test suites for a test object (or an OTP
application) by specifying only the top directory (for example, with start
flag/option `dir`), `Common Test` primarily looks for test suite modules in a
subdirectory named `test`. If this subdirectory does not exist, the specified
top directory is assumed to be the test directory, and test suites are read from
there instead.

To disable the automatic compilation feature, use flag `-no_auto_compile` with
`ct_run`, or option `{auto_compile,false}` with `ct:run_test/1`. With automatic
compilation disabled, the user is responsible for compiling the test suite
modules (and any help modules) before the test run. If the modules cannot be
loaded from the local file system during startup of `Common Test`, the user must
preload the modules before starting the test. `Common Test` only verifies that
the specified test suites exist (that is, that they are, or can be, loaded).
This is useful, for example, if the test suites are transferred and loaded as
binaries through RPC from a remote node.

[](){: #ct_run }

## Running Tests from the OS Command Line

The [`ct_run`](ct_run_cmd.md) program can be used for running tests from the OS
command line, for example, as follows:

- `ct_run -config <configfilenames> -dir <dirs>`
- `ct_run -config <configfilenames> -suite <suiteswithfullpath>`
- `ct_run -userconfig <callbackmodulename> <configfilenames> -suite <suiteswithfullpath>`
- `ct_run -config <configfilenames> -suite <suitewithfullpath> -group <groups> -case <casenames>`

_Examples:_

```text
 $ ct_run -config $CFGS/sys1.cfg $CFGS/sys2.cfg -dir $SYS1_TEST $SYS2_TEST
 $ ct_run -userconfig ct_config_xml $CFGS/sys1.xml $CFGS/sys2.xml -dir $SYS1_TEST $SYS2_TEST
 $ ct_run -suite $SYS1_TEST/setup_SUITE $SYS2_TEST/config_SUITE
 $ ct_run -suite $SYS1_TEST/setup_SUITE -case start stop
 $ ct_run -suite $SYS1_TEST/setup_SUITE -group installation -case start stop
```

The flags `dir`, `suite`, and `group/case` can be combined. For example, to run
`x_SUITE` and `y_SUITE` in directory `testdir`, as follows:

```text
 $ ct_run -dir ./testdir -suite x_SUITE y_SUITE
```

This has the same effect as the following:

```text
 $ ct_run -suite ./testdir/x_SUITE ./testdir/y_SUITE
```

For details, see
[Test Case Group Execution](run_test_chapter.md#group_execution).

The following flags can also be used with [`ct_run`](ct_run_cmd.md):

- **`-help`** - Lists all available start flags.

- **`-logdir <dir>`** - Specifies where the HTML log files are to be written.

- **`-label <name_of_test_run>`** - Associates the test run with a name that
  gets printed in the overview HTML log files.

- **`-refresh_logs`** - Refreshes the top-level HTML index files.

- **`-shell`** - Starts interactive shell mode (described later).

- **`-step [step_opts]`** - Steps through test cases using the Erlang Debugger
  (described later).

- **`-spec <testspecs>`** - Uses test specification as input (described later).

- **`-allow_user_terms`** - Allows user-specific terms in a test specification
  (described later).

- **`-silent_connections [conn_types]`** - , tells `Common Test` to suppress
  printouts for specified connections (described later).

- **`-stylesheet <css_file>`** - Points out a user HTML style sheet (described
  later).

- **`-cover <cover_cfg_file>`** - To perform code coverage test (see
  [Code Coverage Analysis](cover_chapter.md#cover)).

- **`-cover_stop <bool>`** - To specify if the `cover` tool is to be stopped
  after the test is completed (see
  [Code Coverage Analysis](cover_chapter.md#cover_stop)).

- **`-event_handler <event_handlers>`** - To install
  [event handlers](event_handler_chapter.md#event_handling).

- **`-event_handler_init <event_handlers>`** - To install
  [event handlers](event_handler_chapter.md#event_handling) including start
  arguments.

- **`-ct_hooks <ct_hooks>`** - To install
  [Common Test Hooks](ct_hooks_chapter.md#installing) including start arguments.

- **`-ct_hooks_order [test|config]`** - To modify
  [Common Test Hooks](ct_hooks_chapter.md#installing) execution order.

- **`-enable_builtin_hooks <bool>`** - To enable or disable
  [Built-in Common Test Hooks](ct_hooks_chapter.md#builtin_cths). Default is
  `true`.

- **`-include`** - Specifies include directories (described earlier).

- **`-no_auto_compile`** - Disables the automatic test suite compilation feature
  (described earlier).

- **`-abort_if_missing_suites`** - Aborts the test run if one or more suites
  fail to compile (described earlier).

- **`-multiply_timetraps <n>`** - Extends
  [timetrap time-out](write_test_chapter.md#timetraps) values.

- **`-scale_timetraps <bool>`** - Enables automatic
  [timetrap time-out](write_test_chapter.md#timetraps) scaling.

- **`-repeat <n>`** - Tells `Common Test` to repeat the tests `n` times
  (described later).

- **`-duration <time>`** - Tells `Common Test` to repeat the tests for duration
  of time (described later).

- **`-until <stop_time>`** - Tells `Common Test` to repeat the tests until
  `stop_time` (described later).

- **`-force_stop [skip_rest]`** - On time-out, the test run is aborted when the
  current test job is finished. If `skip_rest` is provided, the remaining test
  cases in the current test job are skipped (described later).

- **`-decrypt_key <key>`** - Provides a decryption key for
  [encrypted configuration files](config_file_chapter.md#encrypted_config_files).

- **`-decrypt_file <key_file>`** - Points out a file containing a decryption key
  for
  [encrypted configuration files](config_file_chapter.md#encrypted_config_files).

- **`-basic_html`** - Switches off HTML enhancements that can be incompatible
  with older browsers.

- **`-logopts <opts>`** - Enables modification of the logging behavior, see
  [Log options](run_test_chapter.md#logopts).

- **`-verbosity <levels>`** - Sets
  [verbosity levels for printouts](write_test_chapter.md#logging).

- **`-no_esc_chars`** - Disables automatic escaping of special HTML characters.
  See the [Logging chapter](write_test_chapter.md#logging).

> #### Note {: .info }
>
> Directories passed to `Common Test` can have either relative or absolute
> paths.

> #### Note {: .info }
>
> Any start flags to the Erlang runtime system (application ERTS) can also be
> passed as parameters to `ct_run`. It is, for example, useful to be able to
> pass directories to be added to the Erlang code server search path with flag
> `-pa` or `-pz`. If you have common help- or library modules for test suites
> (separately compiled), stored in other directories than the test suite
> directories, these `help/lib` directories are preferably added to the code
> path this way.
>
> _Example:_
>
> `$ ct_run -dir ./chat_server -logdir ./chat_server/testlogs -pa $PWD/chat_server/ebin`
>
> The absolute path of directory `chat_server/ebin` is here passed to the code
> server. This is essential because relative paths are stored by the code server
> as relative, and `Common Test` changes the current working directory of ERTS
> during the test run.

The `ct_run` program sets the exit status before shutting down. The following
values are defined:

- `0` indicates a successful testrun, that is, without failed or auto-skipped
  test cases.
- `1` indicates that one or more test cases have failed, or have been
  auto-skipped.
- `2` indicates that the test execution has failed because of, for example,
  compilation errors, or an illegal return value from an information function.

If auto-skipped test cases do not affect the exit status. The default behavior
can be changed using start flag:

```text
 -exit_status ignore_config
```

> #### Note {: .info }
>
> Executing `ct_run` without start flags is equal to the command:
> `ct_run -dir ./`

For more information about the `ct_run` program, see module
[`ct_run`](ct_run_cmd.md) and section
[Installation](install_chapter.md#general).

[](){: #erlang_shell_or_program }

## Running Tests from the Erlang Shell or from an Erlang Program

`Common Test` provides an Erlang API for running tests. The main (and most
flexible) function for specifying and executing tests is `ct:run_test/1`. It
takes the same start parameters as [`ct_run`](run_test_chapter.md#ct_run), but
the flags are instead specified as options in a list of key-value tuples. For
example, a test specified with `ct_run` as follows:

`$ ct_run -suite ./my_SUITE -logdir ./results`

is with `ct:run_test/1` specified as:

`1> ct:run_test([{suite,"./my_SUITE"},{logdir,"./results"}]).`

The function returns the test result, represented by the tuple
`{Ok,Failed,{UserSkipped,AutoSkipped}}`, where each element is an integer. If
test execution fails, the function returns the tuple `{error,Reason}`, where the
term `Reason` explains the failure.

The default start option `{dir,Cwd}` (to run all suites in the current working
directory) is used if the function is called with an empty list of options.

### Releasing the Erlang Shell

During execution of tests started with `ct:run_test/1`, the Erlang shell
process, controlling `stdin`, remains the top-level process of the `Common Test`
system of processes. Consequently, the Erlang shell is not available for
interaction during the test run. If this is not desirable, for example, because
the shell is needed for debugging purposes or for interaction with the SUT
during test execution, set start option `release_shell` to `true` (in the call
to `ct:run_test/1` or by using the corresponding test specification term,
described later). This makes `Common Test` release the shell immediately after
the test suite compilation stage. To accomplish this, a test runner process is
spawned to take control of the test execution. The effect is that
`ct:run_test/1` returns the pid of this process rather than the test result,
which instead is printed to tty at the end of the test run.

> #### Note {: .info }
>
> To use the functions [`ct:break/1,2`](`ct:break/1`) and
> [`ct:continue/0,1`](`ct:continue/0`), `release_shell` _must_ be set to `true`.

For details, see `ct:run_test/1` manual page.

[](){: #group_execution }

## Test Case Group Execution

With the `ct_run` flag, or `ct:run_test/1` option `group`, one or more test case
groups can be specified, optionally in combination with specific test cases. The
syntax for specifying groups on the command line is as follows:

```text
 $ ct_run -group <group_names_or_paths> [-case <cases>]
```

The syntax in the Erlang shell is as follows:

```erlang
 1> ct:run_test([{group,GroupsNamesOrPaths}, {case,Cases}]).
```

Parameter `group_names_or_paths` specifies one or more group names and/or one or
more group paths. At startup, `Common Test` searches for matching groups in the
group definitions tree (that is, the list returned from `Suite:groups/0`; for
details, see section [Test Case Groups](write_test_chapter.md#test_case_groups).

Given a group name, say `g`, `Common Test` searches for all paths leading to
`g`. By path is meant a sequence of nested groups, which must be followed to get
from the top-level group to `g`. To execute the test cases in group `g`,
`Common Test` must call the `init_per_group/2` function for each group in the
path to `g`, and all corresponding `end_per_group/2` functions afterwards. This
is because the configuration of a test case in `g` (and its `Config` input data)
depends on `init_per_testcase(TestCase, Config)` and its return value, which in
turn depends on `init_per_group(g, Config)` and its return value, which in turn
depends on `init_per_group/2` of the group above `g`, and so on, all the way up
to the top-level group.

This means that if there is more than one way to locate a group (and its test
cases) in a path, the result of the group search operation is a number of tests,
all of which are to be performed. `Common Test` interprets a group specification
that consists of a single name as follows:

"Search and find all paths in the group definitions tree that lead to the
specified group and, for each path, create a test that does the following, in
order:

1. Executes all configuration functions in the path to the specified group.
1. Executes all, or all matching, test cases in this group.
1. Executes all, or all matching, test cases in all subgroups of the group."

The user can specify a specific group path with parameter
`group_names_or_paths`. With this type of specification execution of unwanted
groups (in otherwise matching paths), and/or the execution of subgroups can be
avoided. The command line syntax of the group path is a list of group names in
the path, for example:

`$ ct_run -suite "./x_SUITE" -group [g1,g3,g4] -case tc1 tc5`

The syntax in the Erlang shell is as follows (requires a list within the groups
list):

`1> ct:run_test([{suite,"./x_SUITE"}, {group,[[g1,g3,g4]]}, {testcase,[tc1,tc5]}]).`

The last group in the specified path is the terminating group in the test, that
is, no subgroups following this group are executed. In the previous example,
`g4` is the terminating group. Hence, `Common Test` executes a test that calls
all `init` configuration functions in the path to `g4`, that is, `g1..g3..g4`.
It then calls test cases `tc1` and `tc5` in `g4`, and finally all `end`
configuration functions in order `g4..g3..g1`.

> #### Note {: .info }
>
> The group path specification does not necessarily have to include _all_ groups
> in the path to the terminating group. `Common Test` searches for all matching
> paths if an incomplete group path is specified.

> #### Note {: .info }
>
> Group names and group paths can be combined with parameter
> `group_names_or_paths`. Each element is treated as an individual specification
> in combination with parameter `cases`. The following examples illustrates
> this.

_Examples:_

```erlang
 -module(x_SUITE).
 ...
 %% The group definitions:
 groups() ->
   [{top1,[],[tc11,tc12,
	      {sub11,[],[tc12,tc13]},
	      {sub12,[],[tc14,tc15,
			 {sub121,[],[tc12,tc16]}]}]},

    {top2,[],[{group,sub21},{group,sub22}]},
    {sub21,[],[tc21,{group,sub2X2}]},
    {sub22,[],[{group,sub221},tc21,tc22,{group,sub2X2}]},
    {sub221,[],[tc21,tc23]},
    {sub2X2,[],[tc21,tc24]}].
```

The following executes two tests, one for all cases and all subgroups under
`top1`, and one for all under `top2`:

```text
 $ ct_run -suite "x_SUITE" -group all
 1> ct:run_test([{suite,"x_SUITE"}, {group,all}]).
```

Using `-group top1 top2`, or `{group,[top1,top2]}` gives the same result.

The following executes one test for all cases and subgroups under `top1`:

```text
 $ ct_run -suite "x_SUITE" -group top1
 1> ct:run_test([{suite,"x_SUITE"}, {group,[top1]}]).
```

The following runs a test executing `tc12` in `top1` and any subgroup under
`top1` where it can be found (`sub11` and `sub121`):

```text
 $ ct_run -suite "x_SUITE" -group top1 -case tc12
 1> ct:run_test([{suite,"x_SUITE"}, {group,[top1]}, {testcase,[tc12]}]).
```

The following executes `tc12` _only_ in group `top1`:

```text
 $ ct_run -suite "x_SUITE" -group [top1] -case tc12
 1> ct:run_test([{suite,"x_SUITE"}, {group,[[top1]]}, {testcase,[tc12]}]).
```

The following searches `top1` and all its subgroups for `tc16` resulting in that
this test case executes in group `sub121`:

```text
 $ ct_run -suite "x_SUITE" -group top1 -case tc16
 1> ct:run_test([{suite,"x_SUITE"}, {group,[top1]}, {testcase,[tc16]}]).
```

Using the specific path `-group [sub121]` or `{group,[[sub121]]}` gives the same
result in this example.

The following executes two tests, one including all cases and subgroups under
`sub12`, and one with _only_ the test cases in `sub12`:

```text
 $ ct_run -suite "x_SUITE" -group sub12 [sub12]
 1> ct:run_test([{suite,"x_SUITE"}, {group,[sub12,[sub12]]}]).
```

In the following example, `Common Test` finds and executes two tests, one for
the path from `top2` to `sub2X2` through `sub21`, and one from `top2` to
`sub2X2` through `sub22`:

```text
 $ ct_run -suite "x_SUITE" -group sub2X2
 1> ct:run_test([{suite,"x_SUITE"}, {group,[sub2X2]}]).
```

In the following example, by specifying the unique path
`top2 -> sub21 -> sub2X2`, only one test is executed. The second possible path,
from `top2` to `sub2X2` (from the former example) is discarded:

```erlang
 $ ct_run -suite "x_SUITE" -group [sub21,sub2X2]
 1> ct:run_test([{suite,"x_SUITE"}, {group,[[sub21,sub2X2]]}]).
```

The following executes only the test cases for `sub22` and in reverse order
compared to the group definition:

```erlang
 $ ct_run -suite "x_SUITE" -group [sub22] -case tc22 tc21
 1> ct:run_test([{suite,"x_SUITE"}, {group,[[sub22]]}, {testcase,[tc22,tc21]}]).
```

If a test case belonging to a group (according to the group definition) is
executed without a group specification, that is, simply by (using the command
line):

`$ ct_run -suite "my_SUITE" -case my_tc`

or (using the Erlang shell):

`1> ct:run_test([{suite,"my_SUITE"}, {testcase,my_tc}]).`

then `Common Test` ignores the group definition and executes the test case in
the scope of the test suite only (no group configuration functions are called).

The group specification feature, as presented in this section, can also be used
in [Test Specifications](run_test_chapter.md#test_specifications) (with some
extra features added).

## Running the Interactive Shell Mode

You can start `Common Test` in an interactive shell mode where no automatic
testing is performed. Instead, `Common Test` starts its utility processes,
installs configuration data (if any), and waits for the user to call functions
(typically test case support functions) from the Erlang shell.

The shell mode is useful, for example, for debugging test suites, analyzing and
debugging the SUT during "simulated" test case execution, and trying out various
operations during test suite development.

To start the interactive shell mode, start an Erlang shell manually and call
`ct:install/1` to install any configuration data you might need (use `[]` as
argument otherwise). Then call `ct:start_interactive/0` to start `Common Test`.

If you use the `ct_run` program, you can start the Erlang shell and
`Common Test` in one go by using the flag `-shell` and, optionally, flag
`-config` and/or `-userconfig`.

_Examples:_

- `ct_run -shell`
- `ct_run -shell -config cfg/db.cfg`
- `ct_run -shell -userconfig db_login testuser x523qZ`

If no configuration file is specified with command `ct_run`, a warning is
displayed. If `Common Test` has been run from the same directory earlier, the
same configuration file(s) are used again. If `Common Test` has not been run
from this directory before, no configuration files are available.

If any functions using "required configuration data" (for example, functions
`ct_telnet` or `ct_ftp`) are to be called from the Erlang shell, first require
configuration data with [`ct:require/1,2`](`ct:require/1`). This is equivalent
to a `require` statement in the
[Test Suite Information Function](write_test_chapter.md#suite) or in the
[Test Case Information Function](write_test_chapter.md#info_function).

_Example:_

```erlang

 1> ct:require(unix_telnet, unix).
 ok
 2> ct_telnet:open(unix_telnet).
 {ok,<0.105.0>}
 4> ct_telnet:cmd(unix_telnet, "ls .").
 {ok,["ls .","file1  ...",...]}
```

Everything that `Common Test` normally prints in the test case logs, are in the
interactive mode written to a log named `ctlog.html` in directory
`ct_run.<timestamp>`. A link to this file is available in the file named
`last_interactive.html` in the directory from which you execute `ct_run`.
Specifying a different root directory for the logs than the current working
directory is not supported.

If you wish to exit the interactive mode (for example, to start an automated
test run with `ct:run_test/1`), call function `ct:stop_interactive/0`. This
shuts down the running `ct` application. Associations between configuration
names and data created with `require` are consequently deleted. Function
`ct:start_interactive/0` takes you back into interactive mode, but the previous
state is not restored.

## Step-by-Step Execution of Test Cases with the Erlang Debugger

Using `ct_run -step [opts]`, or by passing option `{step,Opts}` to
`ct:run_test/1`, the following is possible:

- Get the Erlang Debugger started automatically.
- Use its graphical interface to investigate the state of the current test case.
- Execute the test case step-by-step and/or set execution breakpoints.

If no extra options are specified with flag/option `step`, breakpoints are set
automatically on the test cases that are to be executed by `Common Test`, and
those functions only. If step option `config` is specified, breakpoints are also
initially set on the configuration functions in the suite, that is,
`init_per_suite/1`, `end_per_suite/1`, `init_per_group/2`, `end_per_group/2`,
`init_per_testcase/2` and `end_per_testcase/2`.

`Common Test` enables the Debugger auto-attach feature, which means that for
every new interpreted test case function that starts to execute, a new trace
window automatically pops up (as each test case executes on a dedicated Erlang
process). Whenever a new test case starts, `Common Test` attempts to close the
inactive trace window of the previous test case. However, if you prefer
`Common Test` to leave inactive trace windows, use option `keep_inactive`.

The step functionality can be used together with flag/option `suite` and `suite`
\+ `case/testcase`, but not together with `dir`.

[](){: #test_specifications }

## Test Specifications

### General Description

The most flexible way to specify what to test, is to use a test specification,
which is a sequence of Erlang terms. The terms are normally declared in one or
more text files (see `ct:run_test/1`), but can also be passed to `Common Test`
on the form of a list (see `ct:run_testspec/1`). There are two general types of
terms: configuration terms and test specification terms.

With configuration terms it is, for example, possible to do the following:

- Label the test run (similar to `ct_run -label`).
- Evaluate any expressions before starting the test.
- Import configuration data (similar to `ct_run -config/-userconfig`).
- Specify the top-level HTML log directory (similar to `ct_run -logdir`).
- Enable code coverage analysis (similar to `ct_run -cover`).
- Install `Common Test Hooks` (similar to `ct_run -ch_hooks`).
- Install `event_handler` plugins (similar to `ct_run -event_handler`).
- Specify include directories to be passed to the compiler for automatic
  compilation (similar to `ct_run -include`).
- Disable the auto-compilation feature (similar to `ct_run -no_auto_compile`).
- Set verbosity levels (similar to `ct_run -verbosity`).

Configuration terms can be combined with `ct_run` start flags or `ct:run_test/1`
options. The result is, for some flags/options and terms, that the values are
merged (for example, configuration files, include directories, verbosity levels,
and silent connections) and for others that the start flags/options override the
test specification terms (for example, log directory, label, style sheet, and
auto-compilation).

With test specification terms, it is possible to state exactly which tests to
run and in which order. A test term specifies either one or more suites, one or
more test case groups (possibly nested), or one or more test cases in a group
(or in multiple groups) or in a suite.

Any number of test terms can be declared in sequence. `Common Test` compiles by
default the terms into one or more tests to be performed in one resulting test
run. A term that specifies a set of test cases "swallows" one that only
specifies a subset of these cases. For example, the result of merging one term
specifying that all cases in suite S are to be executed, with another term
specifying only test case X and Y in S, is a test of all cases in S. However, if
a term specifying test case X and Y in S is merged with a term specifying case Z
in S, the result is a test of X, Y, and Z in S. To disable this behavior, that
is, to instead perform each test sequentially in a "script-like" manner, set
term `merge_tests` to `false` in the test specification.

A test term can also specify one or more test suites, groups, or test cases to
be skipped. Skipped suites, groups, and cases are not executed and show up in
the HTML log files as `SKIPPED`.

### Using Multiple Test Specification Files

When multiple test specification files are specified at startup (either with
`ct_run -spec file1 file2 ...` or `ct:run_test([{spec, [File1,File2,...]}])`),
`Common Test` either executes one test run per specification file, or joins the
files and performs all tests within one single test run. The first behavior is
the default one. The latter requires that start flag/option `join_specs` is
provided, for example,
`run_test -spec ./my_tests1.ts ./my_tests2.ts -join_specs`.

Joining a number of specifications, or running them separately, can also be
accomplished with (and can be combined with) test specification file inclusion.

### Test Specification File Inclusion

With the term `specs`, a test specification can include other specifications. An
included specification can either be joined with the source specification or
used to produce a separate test run (as with start flag/option `join_specs`
above).

_Example:_

```erlang
 %% In specification file "a.spec"
 {specs, join, ["b.spec", "c.spec"]}.
 {specs, separate, ["d.spec", "e.spec"]}.
 %% Config and test terms follow
 ...
```

In this example, the test terms defined in files "b.spec" and "c.spec" are
joined with the terms in source specification "a.spec" (if any). The inclusion
of specifications "d.spec" and "e.spec" results in two separate, and
independent, test runs (one for each included specification).

Option `join` does not imply that the test terms are merged, only that all tests
are executed in one single test run.

Joined specifications share common configuration settings, such as the list of
`config` files or `include` directories. For configurations that cannot be
combined, such as settings for `logdir` or `verbosity`, it is up to the user to
ensure there are no clashes when the test specifications are joined.
Specifications included with option `separate` do not share configuration
settings with the source specification. This is useful, for example, if there
are clashing configuration settings in included specifications, making it them
impossible to join.

If `{merge_tests,true}` is set in the source specification (which is the default
setting), terms in joined specifications are merged with terms in the source
specification (according to the description of `merge_tests` earlier).

Notice that it is always the `merge_tests` setting in the source specification
that is used when joined with other specifications. Say, for example, that a
source specification A, with tests TA1 and TA2, has `{merge_tests,false}` set,
and that it includes another specification, B, with tests TB1 and TB2, that has
`{merge_tests,true}` set. The result is that the test series
`TA1,TA2,merge(TB1,TB2)` is executed. The opposite `merge_tests` settings would
result in the test series `merge(merge(TA1,TA2),TB1,TB2)`.

The term `specs` can be used to nest specifications, that is, have one
specification include other specifications, which in turn include others, and so
no

### Test Case Groups

When a test case group is specified, the resulting test executes function
`init_per_group`, followed by all test cases and subgroups (including their
configuration functions), and finally function `end_per_group`. Also, if
particular test cases in a group are specified, `init_per_group` and
`end_per_group`, for the group in question, are called. If a group defined (in
`Suite:groups/0`) as a subgroup of another group, is specified (or if particular
test cases of a subgroup are), `Common Test` calls the configuration functions
for the top-level groups and for the subgroup in question (making it possible to
pass configuration data all the way from `init_per_suite` down to the test cases
in the subgroup).

The test specification uses the same mechanism for specifying test case groups
through names and paths, as explained in section
[Test Case Group Execution](run_test_chapter.md#group_execution), with the
addition of element `GroupSpec`.

Element `GroupSpec` makes it possible to specify group execution properties that
overrides those in the group definition (that is, in `groups/0`). Execution
properties for subgroups might be overridden as well. This feature makes it
possible to change properties of groups at the time of execution, without having
to edit the test suite. The same feature is available for `group` elements in
the `Suite:all/0` list. For details and examples, see section
[Test Case Groups](write_test_chapter.md#test_case_groups).

### Test Specification Syntax

Test specifications can be used to run tests both in a single test host
environment and in a distributed `Common Test` environment (Large Scale
Testing). The node parameters in term `init` are only relevant in the latter
(see section [Test Specifications](ct_master_chapter.md#test_specifications) in
Large Scale Testing). For details about the various terms, see the corresponding
sections in the User's Guide, for example, the following:

- The [`ct_run` program](run_test_chapter.md#ct_run) for an overview of
  available start flags (as most flags have a corresponding configuration term)
- [Logging](write_test_chapter.md#logging) (for terms `verbosity`, `stylesheet`,
  `basic_html` and `esc_chars`)
- [External Configuration Data](config_file_chapter.md#top) (for terms `config`
  and `userconfig`)
- [Event Handling](event_handler_chapter.md#event_handling) (for the
  `event_handler` term)
- [Common Test Hooks](ct_hooks_chapter.md#installing) (for term `ct_hooks`)

_Configuration terms:_

```erlang
 {merge_tests, Bool}.

 {define, Constant, Value}.

 {specs, InclSpecsOption, TestSpecs}.

 {node, NodeAlias, Node}.

 {init, InitOptions}.
 {init, [NodeAlias], InitOptions}.

 {label, Label}.
 {label, NodeRefs, Label}.

 {verbosity, VerbosityLevels}.
 {verbosity, NodeRefs, VerbosityLevels}.

 {stylesheet, CSSFile}.
 {stylesheet, NodeRefs, CSSFile}.

 {silent_connections, ConnTypes}.
 {silent_connections, NodeRefs, ConnTypes}.

 {multiply_timetraps, N}.
 {multiply_timetraps, NodeRefs, N}.

 {scale_timetraps, Bool}.
 {scale_timetraps, NodeRefs, Bool}.

 {cover, CoverSpecFile}.
 {cover, NodeRefs, CoverSpecFile}.

 {cover_stop, Bool}.
 {cover_stop, NodeRefs, Bool}.

 {include, IncludeDirs}.
 {include, NodeRefs, IncludeDirs}.

 {auto_compile, Bool},
 {auto_compile, NodeRefs, Bool},

 {abort_if_missing_suites, Bool},
 {abort_if_missing_suites, NodeRefs, Bool},

 {config, ConfigFiles}.
 {config, ConfigDir, ConfigBaseNames}.
 {config, NodeRefs, ConfigFiles}.
 {config, NodeRefs, ConfigDir, ConfigBaseNames}.

 {userconfig, {CallbackModule, ConfigStrings}}.
 {userconfig, NodeRefs, {CallbackModule, ConfigStrings}}.

 {logdir, LogDir}.
 {logdir, NodeRefs, LogDir}.

 {logopts, LogOpts}.
 {logopts, NodeRefs, LogOpts}.

 {create_priv_dir, PrivDirOption}.
 {create_priv_dir, NodeRefs, PrivDirOption}.

 {event_handler, EventHandlers}.
 {event_handler, NodeRefs, EventHandlers}.
 {event_handler, EventHandlers, InitArgs}.
 {event_handler, NodeRefs, EventHandlers, InitArgs}.

 {ct_hooks, CTHModules}.
 {ct_hooks, NodeRefs, CTHModules}.

 {ct_hooks_order, CTHOrder}.

 {enable_builtin_hooks, Bool}.

 {basic_html, Bool}.
 {basic_html, NodeRefs, Bool}.

 {esc_chars, Bool}.
 {esc_chars, NodeRefs, Bool}.

 {release_shell, Bool}.
```

_Test terms:_

```erlang
 {suites, Dir, Suites}.
 {suites, NodeRefs, Dir, Suites}.

 {groups, Dir, Suite, Groups}.
 {groups, NodeRefs, Dir, Suite, Groups}.

 {groups, Dir, Suite, Groups, {cases,Cases}}.
 {groups, NodeRefs, Dir, Suite, Groups, {cases,Cases}}.

 {cases, Dir, Suite, Cases}.
 {cases, NodeRefs, Dir, Suite, Cases}.

 {skip_suites, Dir, Suites, Comment}.
 {skip_suites, NodeRefs, Dir, Suites, Comment}.

 {skip_groups, Dir, Suite, GroupNames, Comment}.
 {skip_groups, NodeRefs, Dir, Suite, GroupNames, Comment}.

 {skip_cases, Dir, Suite, Cases, Comment}.
 {skip_cases, NodeRefs, Dir, Suite, Cases, Comment}.
```

[](){: #types } _Types:_

```text
 Bool            = true | false
 Constant        = atom()
 Value           = term()
 InclSpecsOption = join | separate
 TestSpecs       = string() | [string()]
 NodeAlias       = atom()
 Node            = node()
 NodeRef         = NodeAlias | Node | master
 NodeRefs        = all_nodes | [NodeRef] | NodeRef
 InitOptions     = term()
 Label           = atom() | string()
 VerbosityLevels = integer() | [{Category,integer()}]
 Category        = atom()
 CSSFile         = string()
 ConnTypes       = all | [atom()]
 N               = integer()
 CoverSpecFile   = string()
 IncludeDirs     = string() | [string()]
 ConfigFiles     = string() | [string()]
 ConfigDir       = string()
 ConfigBaseNames = string() | [string()]
 CallbackModule  = atom()
 ConfigStrings   = string() | [string()]
 LogDir          = string()
 LogOpts         = [term()]
 PrivDirOption   = auto_per_run | auto_per_tc | manual_per_tc
 EventHandlers   = atom() | [atom()]
 InitArgs        = [term()]
 CTHModules      = [CTHModule |
		    {CTHModule, CTHInitArgs} |
		    {CTHModule, CTHInitArgs, CTHPriority}]
 CTHModule       = atom()
 CTHInitArgs     = term()
 CTHOrder        = test | config
 Dir             = string()
 Suites          = atom() | [atom()] | all
 Suite           = atom()
 Groups          = GroupPath | GroupSpec | [GroupSpec] | all
 GroupPath       = [[GroupSpec]]
 GroupSpec       = GroupName | {GroupName,Properties} | {GroupName,Properties,[GroupSpec]}
 GroupName       = atom()
 GroupNames      = GroupName | [GroupName]
 Cases           = atom() | [atom()] | all
 Comment         = string() | ""
```

The difference between the `config` terms above is that with `ConfigDir`,
`ConfigBaseNames` is a list of base names, that is, without directory paths.
`ConfigFiles` must be full names, including paths. For example, the following
two terms have the same meaning:

```erlang
 {config, ["/home/testuser/tests/config/nodeA.cfg",
           "/home/testuser/tests/config/nodeB.cfg"]}.

 {config, "/home/testuser/tests/config", ["nodeA.cfg","nodeB.cfg"]}.
```

> #### Note {: .info }
>
> Any relative paths, specified in the test specification, are relative to the
> directory containing the test specification file if
> `ct_run -spec TestSpecFile ...` or `ct:run:test([{spec,TestSpecFile},...])`
> executes the test.
>
> The path is relative to the top-level log directory if
> `ct:run:testspec(TestSpec)` executes the test.

### Constants

The term `define` introduces a constant that is used to replace the name
`Constant` with `Value`, wherever it is found in the test specification. This
replacement occurs during an initial iteration through the test specification.
Constants can be used anywhere in the test specification, for example, in any
lists and tuples, and even in strings and inside the value part of other
constant definitions. A constant can also be part of a node name, but that is
the only place where a constant can be part of an atom.

> #### Note {: .info }
>
> For the sake of readability, the name of the constant must always begin with
> an uppercase letter, or a `$`, `?`, or `_`. This means that it must always be
> single quoted (as the constant name is an atom, not text).

The main benefit of constants is that they can be used to reduce the size (and
avoid repetition) of long strings, such as file paths.

_Examples:_

```erlang
 %% 1a. no constant
 {config, "/home/testuser/tests/config", ["nodeA.cfg","nodeB.cfg"]}.
 {suites, "/home/testuser/tests/suites", all}.

 %% 1b. with constant
 {define, 'TESTDIR', "/home/testuser/tests"}.
 {config, "'TESTDIR'/config", ["nodeA.cfg","nodeB.cfg"]}.
 {suites, "'TESTDIR'/suites", all}.

 %% 2a. no constants
 {config, [testnode@host1, testnode@host2], "../config", ["nodeA.cfg","nodeB.cfg"]}.
 {suites, [testnode@host1, testnode@host2], "../suites", [x_SUITE, y_SUITE]}.

 %% 2b. with constants
 {define, 'NODE', testnode}.
 {define, 'NODES', ['NODE'@host1, 'NODE'@host2]}.
 {config, 'NODES', "../config", ["nodeA.cfg","nodeB.cfg"]}.
 {suites, 'NODES', "../suites", [x_SUITE, y_SUITE]}.
```

Constants make the test specification term `alias`, in previous versions of
`Common Test`, redundant. This term is deprecated but remains supported in
upcoming `Common Test` releases. Replacing `alias` terms with `define` is
strongly recommended though. An example of such replacement follows:

```erlang
 %% using the old alias term
 {config, "/home/testuser/tests/config/nodeA.cfg"}.
 {alias, suite_dir, "/home/testuser/tests/suites"}.
 {groups, suite_dir, x_SUITE, group1}.

 %% replacing with constants
 {define, 'TestDir', "/home/testuser/tests"}.
 {define, 'CfgDir', "'TestDir'/config"}.
 {define, 'SuiteDir', "'TestDir'/suites"}.
 {config, 'CfgDir', "nodeA.cfg"}.
 {groups, 'SuiteDir', x_SUITE, group1}.
```

Constants can well replace term `node` also, but this still has a declarative
value, mainly when used in combination with `NodeRefs == all_nodes` (see
[Types](run_test_chapter.md#types)).

### Example

Here follows a simple test specification example:

```erlang
 {define, 'Top', "/home/test"}.
 {define, 'T1', "'Top'/t1"}.
 {define, 'T2', "'Top'/t2"}.
 {define, 'T3', "'Top'/t3"}.
 {define, 'CfgFile', "config.cfg"}.

 {logdir, "'Top'/logs"}.

 {config, ["'T1'/'CfgFile'", "'T2'/'CfgFile'", "'T3'/'CfgFile'"]}.

 {suites, 'T1', all}.
 {skip_suites, 'T1', [t1B_SUITE,t1D_SUITE], "Not implemented"}.
 {skip_cases, 'T1', t1A_SUITE, [test3,test4], "Irrelevant"}.
 {skip_cases, 'T1', t1C_SUITE, [test1], "Ignore"}.

 {suites, 'T2', [t2B_SUITE,t2C_SUITE]}.
 {cases, 'T2', t2A_SUITE, [test4,test1,test7]}.

 {skip_suites, 'T3', all, "Not implemented"}.
```

The example specifies the following:

- The specified `logdir` directory is used for storing the HTML log files (in
  subdirectories tagged with node name, date, and time).
- The variables in the specified test system configuration files are imported
  for the test.
- The first test to run includes all suites for system `t1`. Suites `t1B` and
  `t1D` are excluded from the test. Test cases `test3` and `test4` in `t1A` and
  `test1` case in `t1C` are also excluded from the test.
- The second test to run is for system `t2`. The included suites are `t2B` and
  `t2C`. Test cases `test4`, `test1`, and `test7` in suite `t2A` are also
  included. The test cases are executed in the specified order.
- The last test to run is for system `t3`. Here, all suites are skipped and this
  is explicitly noted in the log files.

### The init Term

With term `init` it is possible to specify initialization options for nodes
defined in the test specification. There are options to start the node and to
evaluate any function on the node. For details, see section
[Automatic Startup of Test Target Nodes](ct_master_chapter.md#ct_slave) in
section Using Common Test for Large Scale Testing.

### User-Specific Terms

The user can provide a test specification including (for `Common Test`)
unrecognizable terms. If this is desired, use flag `-allow_user_terms` when
starting tests with `ct_run`. This forces `Common Test` to ignore unrecognizable
terms. In this mode, `Common Test` is not able to check the specification for
errors as efficiently as if the scanner runs in default mode. If `ct:run_test/1`
is used for starting the tests, the relaxed scanner mode is enabled by tuple
`{allow_user_terms,true}`.

### Reading Test Specification Terms

Terms in the current test specification (that is, the specification that has
been used to configure and run the current test) can be looked up. The function
[`get_testspec_terms()`](`ct:get_testspec_terms/0`) returns a list of all test
specification terms (both configuration terms and test terms), and
`get_testspec_terms(Tags)` returns the term (or a list of terms) matching the
tag (or tags) in `Tags`.

For example, in the test specification:

```text
 ...
 {label, my_server_smoke_test}.
 {config, "../../my_server_setup.cfg"}.
 {config, "../../my_server_interface.cfg"}.
 ...
```

And in, for example, a test suite or a `Common Test Hook` function:

```erlang
 ...
 [{label,[{_Node,TestType}]}, {config,CfgFiles}] =
     ct:get_testspec_terms([label,config]),

 [verify_my_server_cfg(TestType, CfgFile) || {Node,CfgFile} <- CfgFiles,
					     Node == node()];
 ...
```

[](){: #log_files }

## Log Files

As the execution of the test suites proceed, events are logged in the following
four different ways:

- Text to the operator console.
- Suite-related information is sent to the major log file.
- Case-related information is sent to the minor log file.
- The HTML overview log file is updated with test results.
- A link to all runs executed from a certain directory is written in the log
  named `all_runs.html` and direct links to all tests (the latest results) are
  written to the top-level `index.html`.

Typically the operator, possibly running hundreds or thousands of test cases,
does not want to fill the console with details about, or printouts from,
specific test cases. By default, the operator only sees the following:

- A confirmation that the test has started and information about how many test
  cases are executed in total.
- A small note about each failed test case.
- A summary of all the run test cases.
- A confirmation when the test run is complete.
- Some special information, such as error reports, progress reports, and
  printouts written with `erlang:display/1`, or `io:format/3` specifically
  addressed to a receiver other than [`standard_io`](`t:io:standard_io/0`) (for
  example, the default group leader process `user`).

To dig deeper into the general results, or the result of a specific test case,
the operator can do so by following the links in the HTML presentation and read
the major or minor log files. The "all_runs.html" page is a good starting point.
It is located in `logdir` and contains a link to each test run, including a
quick overview (with date and time, node name, number of tests, test names, and
test result totals).

An "index.html" page is written for each test run (that is, stored in the
`ct_run` directory tagged with node name, date, and time). This file provides an
overview of all individual tests performed in the same test run. The test names
follow the following convention:

- `TopLevelDir.TestDir` (all suites in `TestDir` executed)
- `TopLevelDir.TestDir:suites` (specific suites executed)
- `TopLevelDir.TestDir.Suite` (all cases in `Suite` executed)
- `TopLevelDir.TestDir.Suite:cases` (specific test cases executed)
- `TopLevelDir.TestDir.Suite.Case` (only `Case` executed)

The "test run index" page includes a link to the `Common Test` Framework Log
file in which information about imported configuration data and general test
progress is written. This log file is useful to get snapshot information about
the test run during execution. It can also be helpful when analyzing test
results or debugging test suites.

The "test run index" page indicates if a test has missing suites (that is,
suites that `Common Test` failed to compile). Names of the missing suites can be
found in the `Common Test` Framework Log file.

The major log file shows a detailed report of the test run. It includes test
suite and test case names, execution time, the exact reason for failures, and so
on. The information is available in both a file with textual and with HTML
representation. The HTML file shows a summary that gives a good overview of the
test run. It also has links to each individual test case log file for quick
viewing with an HTML browser.

The minor log files contain full details of every single test case, each in a
separate file. This way, it is straightforward to compare the latest results to
that of previous test runs, even if the set of test cases changes. If
application SASL is running, its logs are also printed to the current minor log
file by the [cth_log_redirect built-in hook](ct_hooks_chapter.md#builtin_cths).

The full name of the minor log file (that is, the name of the file including the
absolute directory path) can be read during execution of the test case. It comes
as value in tuple `{tc_logfile,LogFileName}` in the `Config` list (which means
it can also be read by a pre- or post `Common Test Hook` function). Also, at the
start of a test case, this data is sent with an event to any installed event
handler. For details, see section
[Event Handling](event_handler_chapter.md#event_handling).

The log files are written continuously during a test run and links are always
created initially when a test starts. Thevtest progress can therefore be
followed simply by refreshing pages in the HTML browser. Statistics totals are
not presented until a test is complete however.

[](){: #logopts }

### Log Options

With start flag `logopts` options that modify some aspects of the logging
behavior can be specified. The following options are available:

- **`no_src`** - The HTML version of the test suite source code is not generated
  during the test run (and is consequently not available in the log file
  system).

- **`no_nl`** - `Common Test` does not add a newline character `(\n)` to the end
  of an output string that it receives from a call to, for example,
  `io:format/2`, and which it prints to the test case log.

For example, if a test is started with:

`$ ct_run -suite my_SUITE -logopts no_nl`

then printouts during the test made by successive calls to `io:format("x")`,
appears in the test case log as:

`xxx`

instead of each `x` printed on a new line, which is the default behavior.

[](){: #table_sorting }

### Sorting HTML Table Columns

By clicking the name in the column header of any table (for example, "Ok",
"Case", "Time", and so on), the table rows are sorted in whatever order makes
sense for the type of value (for example, numerical for "Ok" or "Time", and
alphabetical for "Case"). The sorting is performed through JavaScript code,
automatically inserted into the HTML log files. `Common Test` uses the
[jQuery](http://jquery.com) library and the
[tablesorter](https://mottie.github.io/tablesorter/docs/) plugin, with
customized sorting functions, for this implementation.

### The Unexpected I/O Log

The test suites overview page includes a link to the Unexpected I/O Log. In this
log, `Common Test` saves printouts made with [`ct:log/1,2,3,4,5`](`ct:log/2`)
and [`ct:pal/1,2,3,4,5`](`ct:pal/2`), as well as captured system error- and
progress reports, which cannot be associated with particular test cases and
therefore cannot be written to individual test case log files. This occurs, for
example, if a log printout is made from an external process (not a test case
process), _or_ if an error- or progress report comes in, during a short interval
while `Common Test` is not executing a test case or configuration function, _or_
while `Common Test` is currently executing a parallel test case group.

[](){: #pre_post_test_io_log }

### The Pre- and Post Test I/O Log

The `Common Test` Framework Log page includes links to the Pre- and Post Test
I/O Log. In this log, `Common Test` saves printouts made with `ct:log/1,2,3,4,5`
and `ct:pal/1,2,3,4,5`, as well as captured system error- and progress reports,
which take place before, and after, the test run. Examples of this are printouts
from a CT hook init- or terminate function, or progress reports generated when
an OTP application is started from a CT hook init function. Another example is
an error report generated because of a failure when an external application is
stopped from a CT hook terminate function. All information in these examples
ends up in the Pre- and Post Test I/O Log. For more information on how to
synchronize test runs with external user applications, see section
[Synchronizing](ct_hooks_chapter.md#synchronizing) in section Common Test Hooks.

> #### Note {: .info }
>
> Logging to file with `ct:log/1,2,3,4,5` or `ct:pal/1,2,3,4,5` only works when
> `Common Test` is running. Printouts with `ct:pal/1,2,3,4,5` are however always
> displayed on screen.

[](){: #delete_old_logs }

### Delete Old Logs

`Common Test` can automatically delete old log. This is specified with the
`keep_logs` option. The default value for this option is `all`, which means that
no logs are deleted. If the value is set to an integer, `N`, `Common Test`
deletes all `ct_run.<timestamp>` directories, except the `N` newest.

[](){: #html_stylesheet }

## HTML Style Sheets

`Common Test` uses an HTML Style Sheet (CSS file) to control the look of the
HTML log files generated during test runs. If the log files are not displayed
correctly in the browser of your choice, or you prefer a more primitive ("pre
`Common Test` v1.6") look of the logs, use the start flag/option:

```text
 basic_html
```

This disables the use of style sheets and JavaScripts (see
[Sorting HTML Table Columns](run_test_chapter.md#table_sorting)).

`Common Test` includes an _optional_ feature to allow user HTML style sheets for
customizing printouts. The functions in `ct` that print to a test case HTML log
file (`log/3,4,5` and `pal/3,4,5`) accept `Category` as first argument. With
this argument a category can be specified that can be mapped to a named `div`
selector in a CSS rule-set. This is useful, especially for coloring text
differently depending on the type of (or reason for) the printout. Say you want
one particular background color for test system configuration information, a
different one for test system state information, and finally one for errors
detected by the test case functions. The corresponding style sheet can look as
follows:

```text
 div.sys_config  { background:blue }
 div.sys_state   { background:yellow }
 div.error       { background:red }
```

Common Test prints the text from `ct:log/3,4,5` or `ct:pal/3,4,5` inside a `pre`
element nested under the named `div` element. Since the `pre` selector has a
predefined CSS rule (in file `ct_default.css`) for the attributes `color`,
`font-family` and `font-size`, if a user wants to change any of the predefined
attribute settings, a new rule for `pre` must be added to the user stylesheet.
Example:

```text
div.error pre { color:white }
```

Here, white text is used instead of the default black for `div.error` printouts
(and no other attribute settings for `pre` are affected).

To install the CSS file (`Common Test` inlines the definition in the HTML code),
the file name can be provided when executing `ct_run`.

_Example:_

```text
 $ ct_run -dir $TEST/prog -stylesheet $TEST/styles/test_categories.css
```

Categories in a CSS file installed with flag `-stylesheet` are on a global test
level in the sense that they can be used in any suite that is part of the test
run.

Style sheets can also be installed on a per suite and per test case basis.

_Example:_

```erlang
 -module(my_SUITE).
 ...
 suite() -> [..., {stylesheet,"suite_categories.css"}, ...].
 ...
 my_testcase(_) ->
     ...
     ct:log(sys_config, "Test node version: ~p", [VersionInfo]),
     ...
     ct:log(sys_state, "Connections: ~p", [ConnectionInfo]),
     ...
     ct:pal(error, "Error ~p detected! Info: ~p", [SomeFault,ErrorInfo]),
     ct:fail(SomeFault).
```

If the style sheet is installed as in this example, the categories are private
to the suite in question. They can be used by all test cases in the suite, but
cannot be used by other suites. A suite private style sheet, if specified, is
used in favor of a global style sheet (one specified with flag `-stylesheet`). A
stylesheet tuple (as returned by `suite/0` above) can also be returned from a
test case information function. In this case the categories specified in the
style sheet can only be used in that particular test case. A test case private
style sheet is used in favor of a suite or global level style sheet.

In a tuple `{stylesheet,CSSFile}`, if `CSSFile` is specified with a path, for
example, `"$TEST/styles/categories.css"`, this full name is used to locate the
file. However, if only the file name is specified, for example,
`categories.css`, the CSS file is assumed to be located in the data directory,
`data_dir`, of the suite. The latter use is recommended, as it is portable
compared to hard coding path names in the suite.

Argument `Category` in the previous example can have the value (atom)
`sys_config` (blue background), `sys_state` (yellow background), or `error`
(white text on red background).

[](){: #repeating_tests }

## Repeating Tests

You can order `Common Test` to repeat the tests you specify. You can choose to
repeat tests a number of times, repeat tests for a specific period of time, or
repeat tests until a particular stop time is reached. If repetition is
controlled by time, an action for `Common Test` to take upon time-out can be
specified. Either `Common Test` performs all tests in the current run before
stopping, or it stops when the current test job is finished. Repetition can be
activated by `ct_run` start flags, or tuples in the `ct:run:test/1` option list
argument. The flags (options in parentheses) are the following:

- `-repeat N ({repeat,N})`, where `N` is a positive integer
- `-duration DurTime ({duration,DurTime})`, where `DurTime` is the duration
- `-until StopTime ({until,StopTime})`, where `StopTime` is finish time
- `-force_stop ({force_stop,true})`
- `-force_stop skip_rest ({force_stop,skip_rest})`

- **`DurTime`** - The duration time is specified as `HHMMSS`, for example,
  `-duration 012030` or `{duration,"012030"}`

  , which means that the tests are executed and (if time allows) repeated until
  time-out occurs after 1 hour, 20 minutes, and 30 seconds.

- **`StopTime`** - The finish time can be specified as `HHMMSS` and is then
  interpreted as a time today (or possibly tomorrow), but can also be specified
  as `YYMoMoDDHHMMSS`, for example, `-until 071001120000` or
  `{until,"071001120000"}`. This means that the tests are executed and (if time
  allows) repeated, until 12 o'clock on the 1st of October 2007.

When time-out occurs, `Common Test` never aborts the ongoing test case, as this
can leave the SUT in an undefined, and possibly bad, state. Instead
`Common Test`, by default, finishes the current test run before stopping. If
flag `force_stop` is specified, `Common Test` stops when the current test job is
finished. If flag `force_stop` is specified with `skip_rest`, `Common Test` only
completes the current test case and skips the remaining tests in the test job.

> #### Note {: .info }
>
> As `Common Test` always finishes at least the current test case, the time
> specified with `duration` or `until` is never definitive.

Log files from every repeated test run is saved in normal `Common Test` fashion
(described earlier).

`Common Test` might later support an optional feature to only store the last
(and possibly the first) set of logs of repeated test runs, but for now the user
must be careful not to run out of disk space if tests are repeated during long
periods of time.

For each test run that is part of a repeated session, information about the
particular test run is printed in the `Common Test` Framework Log. The
information includes the repetition number, remaining time, and so on.

_Example 1:_

```text
 $ ct_run -dir $TEST_ROOT/to1 $TEST_ROOT/to2 -duration 001000 -force_stop
```

Here, the suites in test directory `to1`, followed by the suites in `to2`, are
executed in one test run. A time-out event occurs after 10 minutes. As long as
there is time left, `Common Test` repeats the test run (that is, starting over
with test `to1`). After time-out, `Common Test` stops when the current job is
finished (because of flag `force_stop`). As a result, the specified test run can
be aborted after test `to1` and before test `to2`.

_Example 2:_

```text
 $ ct_run -dir $TEST_ROOT/to1 $TEST_ROOT/to2 -duration 001000 -forces_stop skip_rest
```

Here, the same tests as in Example 1 are run, but with flag `force_stop` set to
`skip_rest`. If time-out occurs while executing tests in directory `to1`, the
remaining test cases in `to1` are skipped and the test is aborted without
running the tests in `to2` another time. If time-out occurs while executing
tests in directory `to2`, the remaining test cases in `to2` are skipped and the
test is aborted.

_Example 3:_

```text
 $ date
 Fri Sep 28 15:00:00 MEST 2007

 $ ct_run -dir $TEST_ROOT/to1 $TEST_ROOT/to2 -until 160000
```

Here, the same test run as in the previous examples are executed (and possibly
repeated). However, when the time-out occurs, after 1 hour, `Common Test`
finishes the entire test run before stopping (that is, both `to1` and `to2` are
always executed in the same test run).

_Example 4:_

```text
 $ ct_run -dir $TEST_ROOT/to1 $TEST_ROOT/to2 -repeat 5
```

Here, the test run, including both the `to1` and the `to2` test, is repeated
five times.

> #### Note {: .info }
>
> Do not confuse this feature with the `repeat` property of a test case group.
> The options described here are used to repeat execution of entire test runs,
> while the `repeat` property of a test case group makes it possible to repeat
> execution of sets of test cases within a suite. For more information about the
> latter, see section
> [Test Case Groups ](write_test_chapter.md#test_case_groups)in section Writing
> Test Suites.

[](){: #silent_connections }

## Silent Connections

The protocol handling processes in `Common Test`, implemented by `ct_telnet`,
`ct_ssh`, `ct_ftp`, and so on, do verbose printing to the test case logs. This
can be switched off with flag `-silent_connections`:

```text
 ct_run -silent_connections [conn_types]
```

Here, `conn_types` specifies SSH, Telnet, FTP, RPC, and/or SNMP.

_Example 1:_

```text
 ct_run ... -silent_connections ssh telnet
```

This switches off logging for SSH and Telnet connections.

_Example 2:_

```text
 ct_run ... -silent_connections
```

This switches off logging for all connection types.

Fatal communication error and reconnection attempts are always printed, even if
logging has been suppressed for the connection type in question. However,
operations such as sending and receiving data are performed silently.

`silent_connections` can also be specified in a test suite. This is accomplished
by returning a tuple, `{silent_connections,ConnTypes}`, in the `suite/0` or test
case information list. If `ConnTypes` is a list of atoms (SSH, Telnet, FTP, RPC
and/or SNMP), output for any corresponding connections are suppressed. Full
logging is by default enabled for any connection of type not specified in
`ConnTypes`. Hence, if `ConnTypes` is the empty list, logging is enabled for all
connections.

_Example 3:_

```erlang
 -module(my_SUITE).

 suite() -> [..., {silent_connections,[telnet,ssh]}, ...].

 ...

 my_testcase1() ->
     [{silent_connections,[ssh]}].

 my_testcase1(_) ->
     ...

 my_testcase2(_) ->
     ...
```

In this example, `suite/0` tells `Common Test` to suppress printouts from Telnet
and SSH connections. This is valid for all test cases. However, `my_testcase1/0`
specifies that for this test case, only SSH is to be silent. The result is that
`my_testcase1` gets Telnet information (if any) printed in the log, but not SSH
information. `my_testcase2` gets no information from either connection printed.

`silent_connections` can also be specified with a term in a test specification
(see section [Test Specifications](run_test_chapter.md#test_specifications) in
section Running Tests and Analyzing Results). Connections provided with start
flag/option `silent_connections` are merged with any connections listed in the
test specification.

Start flag/option `silent_connections` and the test specification term override
any settings made by the information functions inside the test suite.

> #### Note {: .info }
>
> In the current `Common Test` version, the `silent_connections` feature only
> works for Telnet and SSH connections. Support for other connection types can
> be added in future `Common Test` versions.
