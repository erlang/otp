%%
%% %CopyrightBegin%
%% 
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(ct_suite).
-moduledoc """
The following section describes the mandatory and optional test suite functions
that `Common Test` calls during test execution. For more details, see section
[Writing Test Suites](write_test_chapter.md) in the User's Guide.
""".
%%------------------------------------------------------------------
%% Test Suite Behaviour
%% ------------------------------------------------------------------
-export_type([ct_testname/0,
              ct_groupname/0,
              ct_config/0,
              ct_status/0,
              ct_group_def/0,
              ct_test_def/0,
              ct_info/0
             ]).

-doc "The name of the testcase function.".
-type ct_testname() :: atom().
-doc "The name of the test group.".
-type ct_groupname() :: atom().
-doc "The configuration data that can be modified.".
-type ct_config() :: [{Key :: atom(), Value :: term()}].
-doc "The status value for a nested subgroup.".
-type ct_status() :: ok |
            skipped |
            failed.
-type ct_group_props() :: [
                parallel |
                sequence |
                shuffle |
                {shuffle, Seed :: {integer(), integer(), integer()}} |
                {ct_group_repeat_type(), ct_test_repeat()}
            ].
-type ct_group_props_ref() ::
            ct_group_props() |
            default.
-type ct_group_repeat_type() :: repeat |
            repeat_until_all_ok |
            repeat_until_all_fail |
            repeat_until_any_ok |
            repeat_until_any_fail.
-type ct_test_repeat() :: integer() |
            forever.
-doc "The test group definition, as returned by [`Module:groups/0`](`c:groups/0`).".
-type ct_group_def() :: {ct_groupname(), ct_group_props(), [
                ct_testname() |
                ct_group_def() |
                {group, ct_groupname()} |
                ct_testcase_ref()
            ]}.
-type ct_subgroups_def() :: {ct_groupname(), ct_group_props_ref()} |
            {ct_groupname(), ct_group_props_ref(), ct_subgroups_def()}.
-type ct_group_ref() :: {group, ct_groupname()} |
            {group, ct_groupname(), ct_group_props_ref()} |
            {group, ct_groupname(), ct_group_props_ref(), ct_subgroups_def()}.
-type ct_testcase_ref() :: {testcase, ct_testname(), ct_testcase_repeat_prop()}.
-type ct_testcase_repeat_prop() :: [{repeat, ct_test_repeat()} |
            {repeat_until_ok, ct_test_repeat()} |
            {repeat_until_fail, ct_test_repeat()}].
-doc """
The test suite information, as returned by [`Module:suite/0`](`c:suite/0`),
[`Module:group/1`](`c:group/1`) and [`Module:Testcase/0`](`c:'Testcase'/0`).
""".
-type ct_info() :: {timetrap, ct_info_timetrap()} |
            {require, ct_info_required()} |
            {require, Name :: atom(), ct_info_required()} |
            {userdata, UserData :: term()} |
            {silent_connections, Conns :: [atom()]} |
            {stylesheet, CSSFile :: string()} |
            {ct_hooks, CTHs :: ct_hooks()}.
-type ct_info_timetrap() :: timeout() |
            {seconds, integer()} |
            {minutes, integer()} |
            {hours, integer()} |
            {Mod :: atom(), Func :: atom(), Args :: list()} |
            ct_info_timetrap_fun().
-type ct_info_timetrap_fun() :: fun().
-type ct_info_required() :: Key :: atom() |
            {Key :: atom(), SubKeys :: ct_info_required_subkeys()} |
            {Key :: atom(), SubKey :: atom()} |
            {Key :: atom(), SubKey :: atom(), SubKeys :: ct_info_required_subkeys()}.
-type ct_info_required_subkeys() :: SubKey :: atom() |
            [SubKey :: atom()].
-type ct_hooks() :: [
                CTHModule :: atom() |
                {CTHModule :: atom(), CTHInitArgs :: term()} |
                {CTHModule :: atom(), CTHInitArgs :: term(), CTHPriority :: integer()}
            ].
-doc "The test suite definition, as returned by [`Module:all/0`](`c:all/0`).".
-type ct_test_def() :: ct_testname() | ct_group_ref() | ct_testcase_ref().

-doc """
Returns the list of all test cases and test case groups in the test suite module
to be executed.

This list also specifies the order the cases and groups are
executed by `Common Test`. A test case is represented by an atom, the name of
the test case function, or a `testcase` tuple indicating that the test case
shall be repeated. A test case group is represented by a `group` tuple, where
`GroupName`, an atom, is the name of the group (defined in
[`Module:groups/0`](`c:groups/0`)). Execution properties for groups can also be
specified, both for a top-level group and for any of its subgroups. Group
execution properties specified here override properties in the group definition
(see [`Module:groups/0`](`c:groups/0`)). (With value `default`, the group
definition properties are used).

If `{skip, Reason}` is returned, all test cases in the module are skipped and
`Reason` is printed on the HTML result page.

For details on groups, see section
[Test Case Groups](write_test_chapter.md#test_case_groups) in the User's Guide.
""".
-callback all() ->
    [TestDef :: ct_test_def()] |
    {skip, Reason :: term()}.

-doc """
Defines test case groups. For details, see section
[Test Case Groups](write_test_chapter.md#test_case_groups) in the User's Guide.
""".
-callback groups() ->
    [GroupDef :: ct_group_def()].

-doc """
The test suite information function. Returns a list of tagged tuples specifying
various properties related to the execution of this test suite (common for all
test cases in the suite).

Tag `timetrap` sets the maximum time that each test case is allowed to execute
(including [`Module:init_per_testcase/2`](`c:init_per_testcase/2`) and
[`Module:end_per_testcase/2`](`c:end_per_testcase/2`)). If the timetrap time is
exceeded, the test case fails with reason `timetrap_timeout`. A `TimeFunc`
function can be used to set a new timetrap by returning a `TimeVal`. It can also
be used to trigger a timetrap time-out by, at some point, returning a value
other than a `TimeVal`. For details, see section
[Timetrap Time-Outs](write_test_chapter.md#timetraps) in the User's Guide.

Tag `require` specifies configuration variables required by test cases (or
configuration functions) in the suite. If the required configuration variables
are not found in any of the configuration files, all test cases are skipped. For
details about the `require` functionality, see function
[`ct:require/1,2`](`ct:require/1`).

With `userdata`, the user can specify any test suite-related information, which
can be read by calling `ct:userdata/2`.

Tag `ct_hooks` specifies the [Common Test Hooks](ct_hooks_chapter.md) to be run
with this suite.

Other tuples than the ones defined are ignored.

For details about the test suite information function, see section
[Test Suite Information Function](write_test_chapter.md#suite) in the User's
Guide.
""".
-callback suite() ->
    [Info :: ct_info()].

-doc """
This configuration function is called as the first function in the suite. It
typically contains initializations that are common for all test cases in the
suite, and that must only be done once.

Parameter `Config` is the configuration
data that can be modified. Whatever is returned from this function is specified
as `Config` to all configuration functions and test cases in the suite.

If `{skip, Reason}` is returned, all test cases in the suite are skipped and
`Reason` is printed in the overview log for the suite.

For information on `save_config` and `skip_and_save`, see section
[Saving Configuration Data](dependencies_chapter.md#save_config) in the User's
Guide.

If this function is defined, then
[`Module:end_per_suite/1`](`c:end_per_suite/1`) must also be defined.
""".
-callback init_per_suite(Config :: ct_config()) ->
    NewConfig :: ct_config() |
    {skip, Reason :: term()} |
    {skip_and_save, Reason :: term(), SaveConfig :: ct_config()}.

-doc """
This function is called as the last test case in the suite. It is meant to be
used for cleaning up after [`Module:init_per_suite/1`](`c:init_per_suite/1`).

For information on `save_config`, see section
[Saving Configuration Data](dependencies_chapter.md#save_config) in the User's
Guide.

If this function is defined, then
[`Module:init_per_suite/1`](`c:init_per_suite/1`) must also be defined.
""".
-callback end_per_suite(Config :: ct_config()) ->
    term() |
    {save_config, SaveConfig :: ct_config()}.

-doc """
The test case group information function. It is supposed to return a list of
tagged tuples that specify various properties related to the execution of a test
case group (that is, its test cases and subgroups). Properties set by
[`Module:group/1`](`c:group/1`) override properties with the same key that have
been set previously by [`Module:suite/0`](`c:suite/0`).

Tag `timetrap` sets the maximum time that each test case is allowed to execute
(including [`Module:init_per_testcase/2`](`c:init_per_testcase/2`) and
[`Module:end_per_testcase/2`](`c:end_per_testcase/2`)). If the timetrap time is
exceeded, the test case fails with reason `timetrap_timeout`. A `TimeFunc`
function can be used to set a new timetrap by returning a `TimeVal`. It can also
be used to trigger a timetrap time-out by, at some point, returning a value
other than a `TimeVal`. For details, see section
[Timetrap Time-Outs](write_test_chapter.md#timetraps) in the User's Guide.

Tag `require` specifies configuration variables required by test cases (or
configuration functions) in the suite. If the required configuration variables
are not found in any of the configuration files, all test cases in this group
are skipped. For details about the `require` functionality, see function
[`ct:require/1,2`](`ct:require/1`).

With `userdata`, the user can specify any test case group related information
that can be read by calling `ct:userdata/2`.

Tag `ct_hooks` specifies the [Common Test Hooks](ct_hooks_chapter.md) to be run
with this suite.

Other tuples than the ones defined are ignored.

For details about the test case group information function, see section
[Group Information Function](write_test_chapter.md#group_info) in the User's
Guide.
""".
-doc(#{since => <<"OTP R15B">>}).
-callback group(GroupName :: ct_groupname()) ->
    [Info :: ct_info()].

-doc """
This configuration function is called before execution of a test case group. It
typically contains initializations that are common for all test cases and
subgroups in the group, and that must only be performed once.

`GroupName` is the name of the group, as specified in the group definition (see
[`Module:groups/0`](`c:groups/0`)). Parameter `Config` is the configuration data
that can be modified. The return value of this function is given as `Config` to
all test cases and subgroups in the group.

If `{skip, Reason}` is returned, all test cases in the group are skipped and
`Reason` is printed in the overview log for the group.

For information about test case groups, see section
[Test Case Groups](write_test_chapter.md#test_case_groups) in the User's Guide.

If this function is defined, then
[`Module:end_per_group/2`](`c:end_per_group/2`) must also be defined.
""".
-callback init_per_group(GroupName :: ct_groupname(), Config :: ct_config()) ->
    NewConfig :: ct_config() |
    {skip, Reason :: term()}.

-doc """
This function is called after the execution of a test case group is finished. It
is meant to be used for cleaning up after [`Module:init_per_group/2`](`c:init_per_group/2`).

A status value for a nested subgroup can be returned with `{return_group_result, Status}`.
The status can be retrieved in [`Module:end_per_group/2`](`c:end_per_group/2`) for the group on
the level above. The status is also used by `Common Test` for deciding if
execution of a group is to proceed if property `sequence` or `repeat_until_*` is
set.

For details about test case groups, see section
[Test Case Groups](write_test_chapter.md#test_case_groups) in the User's Guide.

If this function is defined, then
[`Module:init_per_group/2`](`c:init_per_group/2`) must also be defined.
""".
-callback end_per_group(GroupName :: ct_groupname(), Config :: ct_config()) ->
    term() |
    {return_group_result, Status :: ct_status()}.

-doc """
This function is called before each test case.

Argument `TestCase` is the test case name, and `Config` (list of key-value tuples)
is the configuration data that can be modified. The `NewConfig` list returned from this
function is given as `Config` to the test case.

If `{fail, Reason}` is returned, the test case is marked as failed without being executed.

If `{skip, Reason}` is returned, the test case is skipped and `Reason` is
printed in the overview log for the suite.

If this function is defined, then
[`Module:end_per_testcase/2`](`c:end_per_testcase/2`) must also be defined.
""".
-callback init_per_testcase(TestCase :: ct_testname(), Config :: ct_config()) ->
    NewConfig :: ct_config() |
    {fail, Reason :: term()} |
    {skip, Reason :: term()}.

-doc """
This function is called after each test case, and can be used to clean up after
[`Module:init_per_testcase/2`](`c:init_per_testcase/2`) and the test case.

Any return value (besides `{fail, Reason}` and `{save_config, SaveConfig}`) is
ignored. By returning `{fail, Reason}`, `TestCase` is marked as faulty (even
though it was successful in the sense that it returned a value instead of
terminating).

For information on `save_config`, see section
[Saving Configuration Data](dependencies_chapter.md#save_config) in the User's
Guide.

If this function is defined, then
[`Module:init_per_testcase/2`](`c:init_per_testcase/2`) must also be defined.
""".
-callback end_per_testcase(TestCase :: ct_testname(), Config :: ct_config()) ->
    term() |
    {fail, Reason :: term()} |
    {save_config, SaveConfig :: ct_config()}.

-doc """
The implementation of a test case. Call the functions to test and check the
result. If something fails, ensure the function causes a runtime error or call
[`ct:fail/1,2`](`ct:fail/1`) (which also causes the test case process to
terminate).

Elements from the `Config` list can, for example, be read with
`proplists:get_value/2` in STDLIB.

Possible return values are:

- **`{fail, Reason}`** - The test case is considered failed, and the `Reason`
  will be logged.

- **`{skip, Reason}`** - The test case is considered skipped, and the `Reason`
  will be logged.

- **`{comment, Comment}`** - The test case is considered successful, and the
  `Comment` will be logged.

- **`{save_config, SaveConfig}`** - The test case is considered successful, and
  the `SaveConfig` will be stored in the `Config` (see section
  [Saving Configuration Data](dependencies_chapter.md#save_config) in the User's
  Guide).

- **`{skip_and_save, Reason, SaveConfig}`** - The test case is considered
  skipped, the `Reason` will be logged, and the `SaveConfig` will be stored in
  the `Config` (see section
  [Saving Configuration Data](dependencies_chapter.md#save_config) in the User's
  Guide).

If the function returns any other term, the test case is considered successful.

If the test case function crashes, it is considered failed.

For details about test case implementation, see section
[Test Cases](write_test_chapter.md#test_cases) in the User's Guide.
""".
-doc(#{since => <<"OTP R14B">>}).
-callback 'Testcase'(Config) ->
                        term() |
                        {skip, Reason} |
                        {fail, Reason} |
                        {comment, Comment} |
                        {save_config, SaveConfig} |
                        {skip_and_save, Reason, SaveConfig}
                        when
                            Config :: ct_config(),
                            SaveConfig :: ct_config(),
                            Reason :: term(),
                            Comment :: string().

-doc """
The test case information function. It is supposed to return a list of tagged
tuples that specify various properties related to the execution of this
particular test case. Properties set by [`Module:Testcase/0`](`c:'Testcase'/0`)
override properties set previously for the test case by
[`Module:group/1`](`c:group/1`) or [`Module:suite/0`](`c:suite/0`).

Tag `timetrap` sets the maximum time that the test case is allowed to execute.
If the timetrap time is exceeded, the test case fails with reason
`timetrap_timeout`. [`Module:init_per_testcase/2`](`c:init_per_testcase/2`) and
[`Module:end_per_testcase/2`](`c:end_per_testcase/2`) are included in the
timetrap time. A `TimeFunc` function can be used to set a new timetrap by
returning a `TimeVal`. It can also be used to trigger a timetrap time-out by, at
some point, returning a value other than a `TimeVal`. For details, see section
[Timetrap Time-Outs](write_test_chapter.md#timetraps) in the User's Guide.

Tag `require` specifies configuration variables that are required by the test
case (or [`init_per_testcase/2`](`c:init_per_testcase/2`) or
[`end_per_testcase/2`](`c:end_per_testcase/2`)). If the required configuration
variables are not found in any of the configuration files, the test case is
skipped. For details about the `require` functionality, see function
[`ct:require/1,2`](`ct:require/1`).

If `timetrap` or `require` is not set, the default values specified by
[`Module:suite/0`](`c:suite/0`) (or [`Module:group/1`](`c:group/1`)) are used.

With `userdata`, the user can specify any test case-related information that can
be read by calling `ct:userdata/3`.

Other tuples than the ones defined are ignored.

For details about the test case information function, see section
[Test Case Information Function](write_test_chapter.md#info_function) in the
User's Guide.
""".
-doc(#{since => <<"OTP R14B">>}).
-callback 'Testcase'() -> [ct_info()].

%% only all/0 is mandatory
-optional_callbacks([groups/0,
                     suite/0,
                     init_per_suite/1,
                     end_per_suite/1,
                     group/1,
                     init_per_group/2,
                     end_per_group/2,
                     init_per_testcase/2,
                     end_per_testcase/2,
                     'Testcase'/1,
                     'Testcase'/0
                    ]).
