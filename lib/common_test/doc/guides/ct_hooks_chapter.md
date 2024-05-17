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
# Common Test Hooks

## General

The _Common Test Hook (CTH)_ framework allows extensions of the default behavior
of `Common Test` using hooks before and after all test suite calls. CTHs allow
advanced `Common Test` users to abstract out behavior that is common to multiple
test suites without littering all test suites with library calls. This can be
used for logging, starting, and monitoring external systems, building C files
needed by the tests, and so on.

In brief, CTH allows you to do the following:

- Manipulate the runtime configuration before each suite configuration call.
- Manipulate the return of all suite configuration calls, and in extension, the
  result of the tests themselves.

The following sections describe how to use CTHs, when they are run, and how to
manipulate the test results in a CTH.

> #### Warning {: .warning }
>
> When executing within a CTH, all timetraps are shut off. So if your CTH never
> returns, the entire test run is stalled.

[](){: #installing }

## Installing a CTH

A CTH can be installed in multiple ways in your test run. You can do it for all
tests in a run, for specific test suites, and for specific groups within a test
suite. If you want a CTH to be present in all test suites within your test run,
there are three ways to accomplish that, as follows:

- Add `-ct_hooks` as an argument to [ct_run](run_test_chapter.md#ct_run). To add
  multiple CTHs using this method, append them to each other using the keyword
  `and`, that is, `ct_run -ct_hooks cth1 [{debug,true}] and cth2 ...`.
- Add tag `ct_hooks` to your
  [Test Specification](run_test_chapter.md#test_specifications).
- Add tag `ct_hooks` to your call to `ct:run_test/1`.

CTHs can also be added within a test suite. This is done by returning
`{ct_hooks,[CTH]}` in the configuration list from
[suite/0](`c:ct_suite:suite/0`),
[init_per_suite/1](`c:ct_suite:init_per_suite/1`), or
[init_per_group/2](`c:ct_suite:init_per_group/2`).

In this case, `CTH` can either be only the module name of the CTH or a tuple
with the module name and the initial arguments, and optionally the hook priority
of the CTH. For example, one of the following:

- `{ct_hooks,[my_cth_module]}`
- `{ct_hooks,[{my_cth_module,[{debug,true}]}]}`
- `{ct_hooks,[{my_cth_module,[{debug,true}],500}]}`

Note that regardless of how you install a CTH, its BEAM file must be available
in the code path when Common Test runs. `ct_run` accepts the `-pa` command line
option.

### Overriding CTHs

By default, each installation of a CTH causes a new instance of it to be
activated. This can cause problems if you want to override CTHs in test
specifications while still having them in the suite information function. The
[id/1](`c:ct_hooks:id/1`) callback exists to address this problem. By returning
the same `id` in both places, `Common Test` knows that this CTH is already
installed and does not try to install it again.

[](){: #cth_execution_order }

### CTH Execution Order

By default, each installed CTH is executed in the order in which they are
installed for init calls, and then reversed for end calls. This order can be
referred to as test-centric, as the order is reversed after a testcase is
executed and corresponds to the default value (`test`) of `ct_hooks_order`
option.

The installation-based order is not always desired, so `Common Test` allows the
user to specify a priority for each hook. The priority can be specified in the
CTH function [init/2](`c:ct_hooks:init/2`) or when installing the hook. The
priority specified at installation overrides the priority returned by the CTH.

In some cases, the reversed order for all end calls is not desired, and instead,
the user might prefer the reversed order for post hook calls. Such behavior can
be enabled with `ct_hooks_order` option with `config` value. When this option is
enabled, the execution order is configuration-centric, as the reversed order
happens after each configuration function and not in relation to testcase.

Note that the `ct_hooks_order` option is considered as a global framework
setting. In case when option is configured multiple times framework with process
only the first value.

The `ct_hooks_order` option can be set as: `ct_run` argument, in test
specification or [suite/0](`c:ct_suite:suite/0`) return value.

[](){: #scope }

## CTH Scope

Once the CTH is installed into a certain test run it remains there until its
scope is expired. The scope of a CTH depends on when it is installed, see the
following table. Function [init/2](`c:ct_hooks:init/2`) is called at the
beginning of the scope and function [terminate/1](`c:ct_hooks:terminate/1`) is
called when the scope ends.

| _CTH installed in_                                            | _CTH scope begins before_                                             | _CTH scope ends after_                                                                        |
| ------------------------------------------------------------- | --------------------------------------------------------------------- | --------------------------------------------------------------------------------------------- |
| [ct_run](run_test_chapter.md#ct_run)                          | the first test suite is to be run                                     | the last test suite has been run                                                              |
| [ct:run_test](`ct:run_test/1`)                                | the first test suite is run                                           | the last test suite has been run                                                              |
| [Test Specification](run_test_chapter.md#test_specifications) | the first test suite is run                                           | the last test suite has been run                                                              |
| [suite/0](`c:ct_suite:suite/0`)                               | [pre_init_per_suite/3](`c:ct_hooks:pre_init_per_suite/3`) is called   | [post_end_per_suite/4](`c:ct_hooks:post_end_per_suite/4`) has been called for that test suite |
| [init_per_suite/1](`c:ct_suite:init_per_suite/1`)             | [post_init_per_suite/4](`c:ct_hooks:post_init_per_suite/4`) is called | [post_end_per_suite/4](`c:ct_hooks:post_end_per_suite/4`) has been called for that test suite |
| [init_per_group/2](`c:ct_suite:init_per_group/2`)             | [post_init_per_group/5](`c:ct_hooks:post_init_per_group/5`) is called | [post_end_per_group/5](`c:ct_hooks:post_end_per_group/5`) has been called for that group      |

_Table: Scope of a CTH_

### CTH Processes and Tables

CTHs are run with the same process scoping as normal test suites, that is, a
different process executes the `init_per_suite` hooks then the `init_per_group`
or `per_testcase` hooks. So if you want to spawn a process in the CTH, you
cannot link with the CTH process, as it exits after the post hook ends. Also, if
you for some reason need an ETS table with your CTH, you must spawn a process
that handles it.

### External Configuration Data and Logging

Configuration data values in the CTH can be read by calling
[`ct:get_config/1,2,3`](`ct:get_config/1`) (as explained in section
[Requiring and Reading Configuration Data](config_file_chapter.md#require_config_data)).
The configuration variables in question must, as always, first have been
required by a suite-, group-, or test case information function, or by function
[`ct:require/1/2`](`ct:require/1`). The latter can also be used in CT hook
functions.

The CT hook functions can call any logging function in the `ct` interface to
print information to the log files, or to add comments in the suite overview
page.

[](){: #manipulating }

## Manipulating Tests

Through CTHs the results of tests and configuration functions can be
manipulated. The main purpose to do this with CTHs is to allow common patterns
to be abstracted out from test suites and applied to multiple test suites
without duplicating any code. All the callback functions for a CTH follow a
common interface described hereafter.

`Common Test` always calls all available hook functions, even pre- and post
hooks for configuration functions that are not implemented in the suite. For
example, `pre_init_per_suite(x_SUITE, ...)` and
`post_init_per_suite(x_SUITE, ...)` are called for test suite `x_SUITE`, even if
it does not export `init_per_suite/1`. With this feature hooks can be used as
configuration fallbacks, and all configuration functions can be replaced with
hook functions.

[](){: #pre }

### Pre Hooks

In a CTH, the behavior can be hooked in before the following functions:

- [`init_per_suite`](`c:ct_suite:init_per_suite/1`)
- [`init_per_group`](`c:ct_suite:init_per_group/2`)
- [`init_per_testcase`](`c:ct_suite:init_per_testcase/2`)
- [`end_per_testcase`](`c:ct_suite:end_per_testcase/2`)
- [`end_per_group`](`c:ct_suite:end_per_group/2`)
- [`end_per_suite`](`c:ct_suite:end_per_suite/1`)

This is done in the CTH functions called `pre_<name of function>`. These
functions take the arguments `SuiteName`, `Name` (group or test case name, if
applicable), `Config`, and `CTHState`. The return value of the CTH function is
always a combination of a result for the suite/group/test and an updated
`CTHState`.

To let the test suite continue on executing, return the configuration list that
you want the test to use as the result.

All pre hooks, except `pre_end_per_testcase/4`, can skip or fail the test by
returning a tuple with `skip` or `fail`, and a reason as the result.

_Example:_

```erlang
pre_init_per_suite(SuiteName, Config, CTHState) ->
  case db:connect() of
    {error,_Reason} ->
      {{fail, "Could not connect to DB"}, CTHState};
    {ok, Handle} ->
      {[{db_handle, Handle} | Config], CTHState#state{ handle = Handle }}
  end.
```

> #### Note {: .info }
>
> If you use multiple CTHs, the first part of the return tuple is used as input
> for the next CTH. So in the previous example the next CTH can get
> `{fail,Reason}` as the second parameter. If you have many CTHs interacting, do
> not let each CTH return `fail` or `skip`. Instead, return that an action is to
> be taken through the `Config` list and implement a CTH that, at the end, takes
> the correct action.

[](){: #post }

### Post Hooks

In a CTH, behavior can be hooked in after the following functions:

- [`init_per_suite`](`c:ct_suite:init_per_suite/1`)
- [`init_per_group`](`c:ct_suite:init_per_group/2`)
- [`init_per_testcase`](`c:ct_suite:init_per_testcase/2`)
- [`end_per_testcase`](`c:ct_suite:end_per_testcase/2`)
- [`end_per_group`](`c:ct_suite:end_per_group/2`)
- [`end_per_suite`](`c:ct_suite:end_per_suite/1`)

This is done in the CTH functions called `post_<name of function>`. These
functions take the arguments `SuiteName`, `Name` (group or test case name, if
applicable), `Config`, `Return`, and `CTHState`. `Config` in this case is the
same `Config` as the testcase is called with. `Return` is the value returned by
the testcase. If the testcase fails by crashing, `Return` is
`{'EXIT',{{Error,Reason},Stacktrace}}`.

The return value of the CTH function is always a combination of a result for the
suite/group/test and an updated `CTHState`. If you do not want the callback to
affect the outcome of the test, return the `Return` data as it is given to the
CTH. You can also modify the test result. By returning the `Config` list with
element `tc_status` removed, you can recover from a test failure. As in all the
pre hooks, it is also possible to fail/skip the test case in the post hook.

_Example:_

```erlang
post_end_per_testcase(_Suite, _TC, Config, {'EXIT',{_,_}}, CTHState) ->
  case db:check_consistency() of
    true ->
      %% DB is good, pass the test.
      {proplists:delete(tc_status, Config), CTHState};
    false ->
      %% DB is not good, mark as skipped instead of failing
      {{skip, "DB is inconsistent!"}, CTHState}
  end;
post_end_per_testcase(_Suite, _TC, Config, Return, CTHState) ->
  %% Do nothing if tc does not crash.
  {Return, CTHState}.
```

> #### Note {: .info }
>
> Do recover from a testcase failure using CTHs only a last resort. If used
> wrongly, it can be very difficult to determine which tests that pass or fail
> in a test run.

### Skip and Fail Hooks

After any post hook has been executed for all installed CTHs,
[on_tc_fail](`c:ct_hooks:on_tc_fail/4`) or
[on_tc_skip](`c:ct_hooks:on_tc_skip/4`) is called if the testcase failed or was
skipped, respectively. You cannot affect the outcome of the tests any further at
this point.

[](){: #synchronizing }

## Synchronizing External User Applications with Common Test

CTHs can be used to synchronize test runs with external user applications. The
init function can, for example, start and/or communicate with an application
that has the purpose of preparing the SUT for an upcoming test run, or
initialize a database for saving test data to during the test run. The terminate
function can similarly order such an application to reset the SUT after the test
run, and/or tell the application to finish active sessions and terminate. Any
system error- or progress reports generated during the init- or termination
stage are saved in the
[Pre- and Post Test I/O Log](run_test_chapter.md#pre_post_test_io_log). (This is
also true for any printouts made with `ct:log/2` and `ct:pal/2`).

To ensure that `Common Test` does not start executing tests, or closes its log
files and shuts down, before the external application is ready for it,
`Common Test` can be synchronized with the application. During startup and
shutdown, `Common Test` can be suspended, simply by having a CTH evaluate a
`receive` expression in the init- or terminate function. The macros
`?CT_HOOK_INIT_PROCESS` (the process executing the hook init function) and
`?CT_HOOK_TERMINATE_PROCESS` (the process executing the hook terminate function)
each specifies the name of the correct `Common Test` process to send a message
to. This is done to return from the `receive`. These macros are defined in
`ct.hrl`.

[](){: #example }

## Example CTH

The following CTH logs information about a test run into a format parseable by
`file:consult/1` (in Kernel):

```erlang
%%% Common Test Example Common Test Hook module.
%%%
%%% To use this hook, on the command line:
%%%     ct_run -suite example_SUITE -pa . -ct_hooks example_cth
%%%
%%% Note `-pa .`: the hook beam file must be in the code path when installing.
-module(example_cth).

%% Mandatory Callbacks
-export([init/2]).

%% Optional Callbacks
-export([id/1]).

-export([pre_init_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_testcase/4]).
-export([post_end_per_testcase/5]).

-export([on_tc_skip/4]).

-export([terminate/1]).

%% This hook state is threaded through all the callbacks.
-record(state, {filename, total, suite_total, ts, tcs, data, skipped}).
%% This example hook prints its results to a file, see terminate/1.
-record(test_run, {total, skipped, suites}).

%% Return a unique id for this CTH.
%% Using the filename means the hook can be used with different
%% log files to separate timing data within the same test run.
%% See Installing a CTH for more information.
id(Opts) ->
    %% the path is relative to the test run directory
    proplists:get_value(filename, Opts, "example_cth.log").

%% Always called before any other callback function. Use this to initiate
%% any common state.
init(Id, _Opts) ->
    {ok, #state{filename = Id, total = 0, data = []}}.

%% Called before init_per_suite is called.
pre_init_per_suite(_Suite,Config,State) ->
    {Config, State#state{suite_total = 0, tcs = []}}.

%% Called after end_per_suite.
post_end_per_suite(Suite,_Config,Return,State) ->
    Data = {suites, Suite, State#state.suite_total,
            lists:reverse(State#state.tcs)},
    {Return, State#state{data = [Data | State#state.data],
                         total = State#state.total + State#state.suite_total}}.

%% Called before each init_per_testcase.
pre_init_per_testcase(_Suite,_TC,Config,State) ->
    Now = erlang:monotonic_time(microsecond),
    {Config, State#state{ts = Now, suite_total = State#state.suite_total + 1}}.

%% Called after each end_per_testcase.
post_end_per_testcase(Suite,TC,_Config,Return,State) ->
    Now = erlang:monotonic_time(microsecond),
    TCInfo = {testcase, Suite, TC, Return, Now - State#state.ts},
    {Return, State#state{ts = undefined, tcs = [TCInfo | State#state.tcs]}}.

%% Called when a test case is skipped by either user action
%% or due to an init function failing.
on_tc_skip(_Suite, _TC, _Reason, State) ->
    State#state{skipped = State#state.skipped + 1}.

%% Called when the scope of the CTH is done.
terminate(State) ->
    %% use append to avoid data loss if the path is reused
    {ok, File} = file:open(State#state.filename, [write, append]),
    io:format(File, "~p.~n", [results(State)]),
    file:close(File),
    ok.

results(State) ->
    #state{skipped = Skipped, data = Data, total = Total} = State,
    #test_run{total = Total, skipped = Skipped, suites = lists:reverse(Data)}.
```

[](){: #builtin_cths }

## Built-In CTHs

`Common Test` is delivered with some general-purpose CTHs that can be enabled by
the user to provide generic testing functionality. Some of these CTHs are
enabled by default when `common_test` is started to run. They can be disabled by
setting `enable_builtin_hooks` to `false` on the command line or in the test
specification. The following two CTHs are delivered with `Common Test`:

- **`cth_log_redirect`** - Built-in

  Captures all log events that would normally be printed by the default logger
  handler, and prints them to the current test case log. If an event cannot be
  associated with a test case, it is printed in the `Common Test` framework log.
  This happens for test cases running in parallel and events occurring
  in-between test cases.

  The log events are handled using a [Logger](`m:logger`) handler called
  cth_log_redirect. The formatting and level is copied from the current
  `default` handler when the cth is started. If you want to use another level
  either change the `default` handler level before starting common_test, or use
  the `logger:set_handler_config/3` API.

  This hook supports the following options:

  - **`{mode, add}`** - Add `cth_log_redirect` to the default logging handler:
    Logs will be emitted to both standard output via the default handler, and
    into the Common Test HTML logs. This is the default behaviour.

  - **`{mode, replace}`** - Replace the `default` logging handler with
    `cth_log_redirect` instead of logging to both the default handler and this
    handler. This effectively silences any logger output which would normally be
    printed to standard output during test runs. To enable this mode, you can
    pass the following options to `ct_run`:

    `-enable_builtin_hooks false -ct_hooks cth_log_redirect [{mode,replace}]`

- **`cth_surefire`** - Not built-in

  Captures all test results and outputs them as surefire XML into a file. The
  created file is by default called `junit_report.xml`. The file name can be
  changed by setting option `path` for this hook, for example:

  `-ct_hooks cth_surefire [{path,"/tmp/report.xml"}]`

  If option `url_base` is set, an extra attribute named `url` is added to each
  `testsuite` and `testcase` XML element. The value is constructed from
  `url_base` and a relative path to the test suite or test case log,
  respectively, for example:

  `-ct_hooks cth_surefire [{url_base, "http://myserver.com/"}]`

  gives an URL attribute value similar to

  `"http://myserver.com/ct_run.ct@myhost.2012-12-12_11.19.39/ x86_64-unknown-linux-gnu.my_test.logs/run.2012-12-12_11.19.39/suite.log.html"`

  Surefire XML can, for example, be used by Jenkins to display test results.
