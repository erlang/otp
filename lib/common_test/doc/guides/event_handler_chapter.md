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
# Event Handling

[](){: #event_handling }

## General

The operator of a `Common Test` system can receive event notifications
continuously during a test run. For example, `Common Test` reports when a test
case starts and stops, the current count of successful, failed, and skipped
cases, and so on. This information can be used for different purposes such as
logging progress and results in another format than HTML, saving statistics to a
database for report generation, and test system supervision.

`Common Test` has a framework for event handling based on the OTP event manager
concept and `gen_event` behavior. When the `Common Test` server starts, it
spawns an event manager. During test execution the manager gets a notification
from the server when something of potential interest happens. Any event handler
plugged into the event manager can match on events of interest, take action, or
pass the information on. The event handlers are Erlang modules implemented by
the `Common Test` user according to the `gen_event` behavior (for details, see
module `m:gen_event` and section [`gen_event Behaviour`](`e:system:events.md`)
in OTP Design Principles in the System Documentation).

A `Common Test` server always starts an event manager. The server also plugs in
a default event handler, which only purpose is to relay notifications to a
globally registered `Common Test` Master event manager (if a `Common Test`
Master server is running in the system). The `Common Test` Master also spawns an
event manager at startup. Event handlers plugged into this manager receives the
events from all the test nodes, plus information from the `Common Test` Master
server.

User-specific event handlers can be plugged into a `Common Test` event manager,
either by telling `Common Test` to install them before the test run (described
later), or by adding the handlers dynamically during the test run using
`gen_event:add_handler/3` or `gen_event:add_sup_handler/3`. In the latter
scenario, the reference of the `Common Test` event manager is required. To get
it, call `ct:get_event_mgr_ref/0` or (on the `Common Test` Master node)
`ct_master:get_event_mgr_ref/0`.

[](){: #usage }

## Use

Event handlers can be installed by an `event_handler` start flag
([`ct_run`](ct_run_cmd.md)) or option `ct:run_test/1`, where the argument
specifies the names of one or more event handler modules.

_Example:_

`$ ct_run -suite test/my_SUITE -event_handler handlers/my_evh1 handlers/my_evh2 -pa $PWD/handlers`

To pass start arguments to the event handler init function, use option
`ct_run -event_handler_init` instead of `-event_handler`.

> #### Note {: .info }
>
> All event handler modules must have `gen_event` behavior. These modules must
> be precompiled and their locations must be added explicitly to the Erlang code
> server search path (as in the previous example).

An event_handler tuple in argument `Opts` has the following definition (see
`ct:run_test/1`):

```erlang
 {event_handler,EventHandlers}

 EventHandlers = EH | [EH]
 EH = atom() | {atom(),InitArgs} | {[atom()],InitArgs}
 InitArgs = [term()]
```

In the following example, two event handlers for the `my_SUITE` test are
installed:

```erlang
 1> ct:run_test([{suite,"test/my_SUITE"},{event_handler,[my_evh1,{my_evh2,[node()]}]}]).
```

Event handler `my_evh1` is started with `[]` as argument to the init function.
Event handler `my_evh2` is started with the name of the current node in the init
argument list.

Event handlers can also be plugged in using one of the following
[test specification](run_test_chapter.md#test_specifications) terms:

- `{event_handler, EventHandlers}`
- `{event_handler, EventHandlers, InitArgs}`
- `{event_handler, NodeRefs, EventHandlers}`
- `{event_handler, NodeRefs, EventHandlers, InitArgs}`

`EventHandlers` is a list of module names. Before a test session starts, the
init function of each plugged in event handler is called (with the `InitArgs`
list as argument or `[]` if no start arguments are specified).

To plug in a handler to the `Common Test` Master event manager, specify `master`
as the node in `NodeRefs`.

To be able to match on events, the event handler module must include the header
file `ct_event.hrl`. An event is a record with the following definition:

`#event{name, node, data}`

- **`name`** - Label (type) of the event.

- **`node`** - Name of the node that the event originated from (only relevant
  for `Common Test` Master event handlers).

- **`data`** - Specific for the event.

[](){: #events }

### General Events

The general events are as follows:

- **`#event{name = start_logging, data = LogDir}`** - `LogDir = string()`,
  top-level log directory for the test run.

  This event indicates that the logging process of `Common Test` has started
  successfully and is ready to receive I/O messages.

- **`#event{name = stop_logging, data = []}`** - This event indicates that the
  logging process of `Common Test` was shut down at the end of the test run.

- **`#event{name = test_start, data = {StartTime,LogDir}}`** -
  `StartTime = {date(),time()}`, test run start date and time.

  `LogDir = string()`, top-level log directory for the test run.

  This event indicates that `Common Test` has finished initial preparations and
  begins executing test cases.

- **`#event{name = test_done, data = EndTime}`** - `EndTime = {date(),time()}`,
  date and time the test run finished.

  This event indicates that the last test case has been executed and
  `Common Test` is shutting down.

- **`#event{name = start_info, data = {Tests,Suites,Cases}}`** -
  `Tests = integer()`, number of tests.

  `Suites = integer()`, total number of suites.

  `Cases = integer() | unknown`, total number of test cases.

  This event gives initial test run information that can be interpreted as:
  "This test run will execute `Tests` separate tests, in total containing
  `Cases` number of test cases, in `Suites` number of suites". However, if a
  test case group with a repeat property exists in any test, the total number of
  test cases cannot be calculated (unknown).

- **`#event{name = tc_start, data = {Suite,FuncOrGroup}}`** - `Suite = atom()`,
  name of the test suite.

  `FuncOrGroup = Func | {Conf,GroupName,GroupProperties}`

  `Func = atom()`, name of test case or configuration function.

  `Conf = init_per_group | end_per_group`, group configuration function.

  `GroupName = atom()`, name of the group.

  `GroupProperties = list()`, list of execution properties for the group.

  This event informs about the start of a test case, or a group configuration
  function. The event is sent also for `init_per_suite` and `end_per_suite`, but
  not for `init_per_testcase` and `end_per_testcase`. If a group configuration
  function starts, the group name and execution properties are also specified.

- **`#event{name = tc_logfile, data = {{Suite,Func},LogFileName}}`** -
  `Suite = atom()`, name of the test suite.

  `Func = atom()`, name of test case or configuration function.

  `LogFileName = string()`, full name of the test case log file.

  This event is sent at the start of each test case (and configuration function
  except `init/end_per_testcase`) and carries information about the full name
  (that is, the file name including the absolute directory path) of the current
  test case log file.

- **`#event{name = tc_done, data = {Suite,FuncOrGroup,Result}}`** - [](){:
  #tc_done } `Suite = atom()`, name of the suite.

  `FuncOrGroup = Func | {Conf,GroupName,GroupProperties}`

  `Func = atom()`, name of test case or configuration function.

  `Conf = init_per_group | end_per_group`, group configuration function.

  `GroupName = unknown | atom()`, name of the group (unknown if init- or end
  function times out).

  `GroupProperties = list()`, list of execution properties for the group.

  `Result = ok | {auto_skipped,SkipReason} | {skipped,SkipReason} | {failed,FailReason}`,
  the result.

  [](){: #skipreason }
  `SkipReason = {require_failed,RequireInfo} | {require_failed_in_suite0,RequireInfo} | {failed,{Suite,init_per_testcase,FailInfo}} | UserTerm`,
  why the case was skipped.

  [](){: #failreason }
  `FailReason = {error,FailInfo} | {error,{RunTimeError,StackTrace}} | {timetrap_timeout,integer()} | {failed,{Suite,end_per_testcase,FailInfo}}`,
  reason for failure.

  `RequireInfo = {not_available,atom() | tuple()}`, why require failed.

  `FailInfo = {timetrap_timeout,integer()} | {RunTimeError,StackTrace} | UserTerm`,
  error details.

  `RunTimeError = term()`, a runtime error, for example, `badmatch` or `undef`.

  `StackTrace = list()`, list of function calls preceding a runtime error.

  `UserTerm = term()`, any data specified by user, or [`exit/1`](`exit/1`)
  information.

  This event informs about the end of a test case or a configuration function
  (see event `tc_start` for details on element `FuncOrGroup`). With this event
  comes the final result of the function in question. It is possible to
  determine on the top level of `Result` if the function was successful, skipped
  (by the user), or if it failed.

  It is also possible to dig deeper and, for example, perform pattern matching
  on the various reasons for skipped or failed. Notice that `{'EXIT',Reason}`
  tuples are translated into `{error,Reason}`. Notice also that if a
  `{failed,{Suite,end_per_testcase,FailInfo}` result is received, the test case
  was successful, but `end_per_testcase` for the case failed.

- **`#event{name = tc_auto_skip, data = {Suite,TestName,Reason}}`** - [](){:
  #tc_auto_skip } `Suite = atom()`, the name of the suite.

  `TestName = init_per_suite | end_per_suite | {init_per_group,GroupName} | {end_per_group,GroupName} | {FuncName,GroupName} | FuncName`

  `FuncName = atom()`, the name of the test case or configuration function.

  `GroupName = atom()`, the name of the test case group.

  `Reason = {failed,FailReason} | {require_failed_in_suite0,RequireInfo}`,
  reason for auto-skipping `Func`.

  `FailReason = {Suite,ConfigFunc,FailInfo}} | {Suite,FailedCaseInSequence}`,
  reason for failure.

  `RequireInfo = {not_available,atom() | tuple()}`, why require failed.

  `ConfigFunc = init_per_suite | init_per_group`

  `FailInfo = {timetrap_timeout,integer()} | {RunTimeError,StackTrace} | bad_return | UserTerm`,
  error details.

  `FailedCaseInSequence = atom()`, the name of a case that failed in a sequence.

  `RunTimeError = term()`, a runtime error, for example `badmatch` or `undef`.

  `StackTrace = list()`, list of function calls preceding a runtime error.

  `UserTerm = term()`, any data specified by user, or [`exit/1`](`exit/1`)
  information.

  This event is sent for every test case or configuration function that
  `Common Test` has skipped automatically because of either a failed
  `init_per_suite` or `init_per_group`, a failed `require` in `suite/0`, or a
  failed test case in a sequence. Notice that this event is never received as a
  result of a test case getting skipped because of `init_per_testcase` failing,
  as that information is carried with event `tc_done`. If a failed test case
  belongs to a test case group, the second data element is a tuple
  `{FuncName,GroupName}`, otherwise only the function name.

- **`#event{name = tc_user_skip, data = {Suite,TestName,Comment}}`** - [](){:
  #tc_user_skip } `Suite = atom()`, the name of the suite.

  `TestName = init_per_suite | end_per_suite | {init_per_group,GroupName} | {end_per_group,GroupName} | {FuncName,GroupName} | FuncName`

  `FuncName = atom()`, the name of the test case or configuration function.

  `GroupName = atom()`, the name of the test case group.

  `Comment = string()`, why the test case was skipped.

  This event specifies that a test case was skipped by the user. It is only
  received if the skip is declared in a test specification. Otherwise, user skip
  information is received as a `{skipped,SkipReason}` result in event `tc_done`
  for the test case. If a skipped test case belongs to a test case group, the
  second data element is a tuple `{FuncName,GroupName}`, otherwise only the
  function name.

- **`#event{name = test_stats, data = {Ok,Failed,Skipped}}`** -
  `Ok = integer()`, current number of successful test cases.

  `Failed = integer()`, current number of failed test cases.

  `Skipped = {UserSkipped,AutoSkipped}`

  `UserSkipped = integer()`, current number of user-skipped test cases.

  `AutoSkipped = integer()`, current number of auto-skipped test cases.

  This is a statistics event with current count of successful, skipped, and
  failed test cases so far. This event is sent after the end of each test case,
  immediately following event `tc_done`.

### Internal Events

The internal events are as follows:

- **`#event{name = start_make, data = Dir}`** - `Dir = string()`, running make
  in this directory.

  This internal event says that `Common Test` starts compiling modules in
  directory `Dir`.

- **`#event{name = finished_make, data = Dir}`** - `Dir = string()`, finished
  running make in this directory.

  This internal event says that `Common Test` is finished compiling modules in
  directory `Dir`.

- **`#event{name = start_write_file, data = FullNameFile}`** -
  `FullNameFile = string(), full name of the file.`

  This internal event is used by the `Common Test` Master process to synchronize
  particular file operations.

- **`#event{name = finished_write_file, data = FullNameFile}`** -
  `FullNameFile = string(), full name of the file.`

  This internal event is used by the `Common Test` Master process to synchronize
  particular file operations.

### Notes

The events are also documented in `ct_event.erl`. This module can serve as an
example of what an event handler for the `Common Test` event manager can look
like.

> #### Note {: .info }
>
> To ensure that printouts to `stdout` (or printouts made with
> [`ct:log/2,3`](`ct:log/2`) or [`ct:pal,2,3`](`ct:pal/2`)) get written to the
> test case log file, and not to the `Common Test` framework log, you can
> synchronize with the `Common Test` server by matching on events `tc_start` and
> `tc_done`. In the period between these events, all I/O is directed to the test
> case log file. These events are sent synchronously to avoid potential timing
> problems (for example, that the test case log file is closed just before an
> I/O message from an external process gets through). Knowing this, you need to
> be careful that your `handle_event/2` callback function does not stall the
> test execution, possibly causing unexpected behavior as a result.
