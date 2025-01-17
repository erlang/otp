%%
%%
%% Copyright (c) Meta Platforms, Inc. and affiliates.
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
%%

-module(erl_debugger_SUITE).

-export([all/0, groups/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Debugger support test cases
-export([test_supported_returns_false/1]).
-export([test_all_functions_fail_with_undef/1]).
-export([test_supported_returns_true/1]).

%% Instrumentation test cases
-export([test_can_toggle_instrumentations/1]).
-export([test_toggle_instrumentations_validates_input/1]).

%% Registration test cases
-export([test_register_and_unregister_debugger/1]).
-export([test_debugger_unregistered_when_dead/1]).

%% Line-breakpoint test-cases
-export([test_setting_bp_fails_on_module_not_found/1]).
-export([test_setting_bp_fails_on_module_loaded_without_line_bp_instrumentation/1]).
-export([test_setting_bp_fails_on_non_existent_line/1]).
-export([test_setting_bp_fails_on_nonexecutable_line/1]).
-export([test_setting_bp_fails_on_unsupported_lines/1]).
-export([test_stops_and_notifies_debugger_process/1]).
-export([test_works_with_inlined_functions/1]).
-export([test_works_with_large_number_of_live_xregs/1]).
-export([test_works_with_a_huge_stack_depth_which_should_require_gc/1]).
-export([test_avoids_blocking_debugger/1]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 20}}].

all() ->
    [
        {group, debugger_support_disabled},
        {group, debugger_support_enabled},
        {group, instrumentations},
        {group, registration},
        {group, line_breakpoints}
    ].

groups() ->
    [
        {debugger_support_disabled, [], [
            test_supported_returns_false,
            test_all_functions_fail_with_undef
        ]},
        {debugger_support_enabled, [], [
            test_supported_returns_true
        ]},
        {instrumentations, [], [
            test_can_toggle_instrumentations,
            test_toggle_instrumentations_validates_input
        ]},
        {registration, [], [
            test_register_and_unregister_debugger,
            test_debugger_unregistered_when_dead
        ]},
        {line_breakpoints, [], [
            test_setting_bp_fails_on_module_not_found,
            test_setting_bp_fails_on_module_loaded_without_line_bp_instrumentation,
            test_setting_bp_fails_on_non_existent_line,
            test_setting_bp_fails_on_nonexecutable_line,
            test_setting_bp_fails_on_unsupported_lines,
            test_stops_and_notifies_debugger_process,
            test_works_with_inlined_functions,
            test_works_with_large_number_of_live_xregs,
            test_works_with_a_huge_stack_depth_which_should_require_gc,
            test_avoids_blocking_debugger
        ]}
    ].

init_per_suite(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    Config.

end_per_suite(_Config) ->
    erts_debug:set_internal_state(available_internal_state, false),
    ok.

init_per_group(debugger_support_disabled , Config) ->
    Config;
init_per_group(_Group, Config) ->
    erts_debug:set_internal_state(debugger_support, true),
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(test_works_with_inlined_functions, _Config) ->
    % TODO(T202887216) unskip once this is fixed
    {skip, "+beam_debug_info is currently blocking inline annotations"};
init_per_testcase(_TC, Config) ->
    erl_debugger:supported() andalso
        erl_debugger:toggle_instrumentations(#{line_breakpoint => false}),

    NoAutoRegisterGroups = #{
        debugger_support_disabled => [],
        debugger_support_enabled => [],
        instrumentation => [],
        registration => []
    },
    case current_group(Config) of
        {ok, Group} when not is_map_key(Group, NoAutoRegisterGroups) ->
            {ok, DebuggerSession} = erl_debugger:register(self()),
            [{debugger_session, DebuggerSession} | Config];
        _ ->
            Config
    end.

end_per_testcase(_TC, Config) ->
    erl_debugger:supported() andalso
        erl_debugger:toggle_instrumentations(#{line_breakpoint => false}),

    case proplists:get_value(debugger_session, Config, undefined) of
        undefined ->
            ok;
        DebuggerSession ->
            ok = erl_debugger:unregister(self(), DebuggerSession)
    end,

    ErlFixtures = filelib:wildcard("*.erl", ?config(data_dir, Config)),
    [unload_fixture(FixtureFile) || FixtureFile <- ErlFixtures],
    ok.

unload_fixture(FixtureFile) ->
    FixtureModule = list_to_atom(filename:basename(FixtureFile, "erl")),
    case erlang:module_loaded(FixtureModule) of
        false -> ok;
        true ->
            true = code:delete(FixtureModule),
            code:purge(FixtureModule)
    end.

current_group(Config) ->
    GroupProps = ?config(tc_group_properties, Config),
    case proplists:get_value(name, GroupProps, []) of
        [] -> undefined;
        Group -> {ok, Group}
    end.

%% Helper macros
-define(expectReceive(Expected),
    begin
        (fun () ->
            receive
                __Actual__ = Expected -> __Actual__
            after 2_000 ->
                receive
                    __Actual__ = Expected ->
                        __Actual__;
                    __NextMessage__ ->
                        error({timeout_receiving, ??Expected, {next_message, __NextMessage__}})
                after 0 ->
                    error({timeout_receiving, ??Expected, nothing_received})
                end
            end
        end)()
    end
).


-define(expectDebuggerEvent(Session, Expected),
    begin
        (fun() ->
            {debugger_event, _, __Actual__} =
                ?expectReceive({debugger_event, Session, Expected}),
            __Actual__
        end)()
    end
).

-define(assertBreakpointHit(Session, MFA, Line),
    begin
        (fun() ->
            {breakpoint, __Pid__, _, _, __Resume__} =
                ?expectDebuggerEvent(Session,  {breakpoint, _, MFA, Line, _}),
            {__Pid__, __Resume__}
        end)()
    end
).

-define(assertMailboxEmpty(),
    begin
        (fun() ->
            receive
                __Unexpected__ -> error({mailbox_not_empty, __Unexpected__})
            after 0 -> ok
            end
        end)()
    end
).

%% Support tests
test_supported_returns_false(_Config) ->
    false = erl_debugger:supported(),
    ok.

test_supported_returns_true(_Config) ->
    true = erl_debugger:supported(),
    ok.

test_all_functions_fail_with_undef(_Config) ->
    Allowed = [{module_info, 0}, {module_info, 1}, {supported, 0}],
    [
        ?assertError(
            undef,
            erl_debugger:M([dummy || _ <- lists:seq(1, A)]),
            lists:flatten(io_lib:format("Didn't fail erl_debugger:~p/~p", [M, A]))
        )
    || {M, A} <- erl_debugger:module_info(exports),
        not lists:member({M, A}, Allowed)
    ],
    ok.

%% Instrumentation toggling tests
test_can_toggle_instrumentations(_Config) ->
    #{line_breakpoint := false} = erl_debugger:instrumentations(),

    ok = erl_debugger:toggle_instrumentations(#{}),
    #{line_breakpoint := false} = erl_debugger:instrumentations(),

    ok = erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    #{line_breakpoint := true} = erl_debugger:instrumentations(),

    ok = erl_debugger:toggle_instrumentations(#{}),
    #{line_breakpoint := true} = erl_debugger:instrumentations(),

    ok = erl_debugger:toggle_instrumentations(#{line_breakpoint => false}),
    #{line_breakpoint := false} = erl_debugger:instrumentations(),
    ok.

test_toggle_instrumentations_validates_input(_Config) ->
    ?assertError(badarg, erl_debugger:toggle_instrumentations([])),
    ?assertError(badarg, erl_debugger:toggle_instrumentations(#{line_breakpoint => faux})),
    ?assertError(badarg, erl_debugger:toggle_instrumentations(#{foo => true})),
    ?assertError(badarg, erl_debugger:toggle_instrumentations(#{line_breakpoint => true, foo => true})),
     ok.

%% Registration tests

test_register_and_unregister_debugger(_Config) ->
    undefined = erl_debugger:whereis(),

    Me = self(),
    AnotherProc = erlang:spawn_link(fun() -> receive after infinity -> ok end end),

    {ok, Session1} = erl_debugger:register(Me),
    Me = erl_debugger:whereis(),

    {error, already_exists} = erl_debugger:register(Me),
    {error, already_exists} = erl_debugger:register(AnotherProc),
    Me = erl_debugger:whereis(),

    ok = erl_debugger:unregister(Me, Session1),
    undefined = erl_debugger:whereis(),

    {ok, Session2} = erl_debugger:register(Me),
    Me = erl_debugger:whereis(),
    BadSession = Session2 + 1,
    {'EXIT', {badarg, _}} = catch erl_debugger:unregister(Me, BadSession),
    ok = erl_debugger:unregister(Me, Session2),
    undefined = erl_debugger:whereis(),

    {ok, Session3} = erl_debugger:register(AnotherProc),
    AnotherProc = erl_debugger:whereis(),
    erl_debugger:unregister(AnotherProc, Session3),
    undefined = erl_debugger:whereis(),
    ok.

test_debugger_unregistered_when_dead(_Config) ->
    Me = self(),
    {Debugger, MonRef} = erlang:spawn_monitor(fun() -> receive after infinity -> ok end end),

    {ok, _Session} = erl_debugger:register(Debugger),
    Debugger = erl_debugger:whereis(),

    exit(Debugger, boom),
    receive
        {'DOWN',  MonRef, process, Debugger, boom} -> ok
        after 2_000 -> error(didnt_die)
    end,

    undefined = erl_debugger:whereis(),

    {ok, Session2} = erl_debugger:register(Me),
    Me = erl_debugger:whereis(),
    ok = erl_debugger:unregister(Me, Session2),
    ok.

%% Line-breakpoint tests

test_setting_bp_fails_on_module_not_found(_Config) ->
    Mod = non_existent_module,
    Actual = erl_debugger:breakpoint(Mod, 42, true),
    Expected = {error, {badkey, Mod}},

    Expected = Actual,
    ok.


test_setting_bp_fails_on_module_loaded_without_line_bp_instrumentation(Config) ->
    Mod = foo,
    erl_debugger:toggle_instrumentations(#{line_breakpoint => false}),
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    Actual = erl_debugger:breakpoint(Mod, 42, true),
    Expected = {error, {unsupported, Mod}},

    Expected = Actual,
    ok.

test_setting_bp_fails_on_non_existent_line(Config) ->
    Mod = foo,
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    BogusLine = 100_000,
    Actual = erl_debugger:breakpoint(Mod, BogusLine, true),
    Expected = {error, {badkey, BogusLine}},

    Expected = Actual,
    ok.

test_setting_bp_fails_on_nonexecutable_line(Config) ->
    Mod = foo,
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    BogusLine = 1,
    Actual = erl_debugger:breakpoint(Mod, BogusLine, true),
    Expected = {error, {badkey, BogusLine}},

    Expected = Actual,
    ok.

test_setting_bp_fails_on_unsupported_lines(Config) ->
    Mod = foo,
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    compile_and_load_module(Config, Mod, [line_coverage]),

    % NB. This line is currently unsupported since it is a
    % function header (for go/1).
    UnsupportedLine = 6,
    Actual = erl_debugger:breakpoint(Mod, UnsupportedLine, true),
    Expected = {error, {unsupported, UnsupportedLine}},

    Expected = Actual,
    ok.

test_stops_and_notifies_debugger_process(Config) ->
    Session = ?config(debugger_session, Config),
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    compile_and_load_module(Config, foo, [beam_debug_info]),

    [ok = erl_debugger:breakpoint(foo, Line, true) || Line <- [7, 10, 12, 18, 19]],

    TestCaseProcess = self(),
    Pid = erlang:spawn(fun() -> foo:go(TestCaseProcess) end),

    {Pid, Resume1} = ?assertBreakpointHit(Session, {foo, go, 1}, 7),
    ?assertMailboxEmpty(),
    ok = Resume1(),
    ?expectReceive({executed, Pid, {foo, go, 1}, {line, 7}}),

    ?expectReceive({executed, Pid, {foo, go, 1}, {line, 8}}), % no bp set on line 8

    {Pid, Resume2} = ?assertBreakpointHit(Session, {foo, go, 1}, 10),
    ?assertMailboxEmpty(),
    ok = Resume2(),
    % NB. no breadcrum on line 10

    {Pid, Resume3} = ?assertBreakpointHit(Session, {foo, do_stuff, 1}, 18),
    ?assertMailboxEmpty(),
    ok = Resume3(),
    ?expectReceive({executed, Pid, {foo, do_stuff, 1}, {line, 18}}),

    {Pid, Resume4} = ?assertBreakpointHit(Session, {foo, do_stuff, 1}, 19),
    ?assertMailboxEmpty(),
    ok = Resume4(),
    ?expectReceive({executed, Pid, {foo, do_stuff, 1}, {line, 19}}),

    {Pid, Resume5} = ?assertBreakpointHit(Session, {foo, go, 1}, 12),
    ?assertMailboxEmpty(),
    ok = Resume5(),
    ?expectReceive({executed, Pid, {foo, go, 1}, {line, 12}}),

    ?expectReceive({done, Pid}),
    ok.

test_works_with_inlined_functions(Config) ->
    Session = ?config(debugger_session, Config),
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),

    compile_and_load_module(Config, inlined_funs, [beam_debug_info]),

    % Sets a brekpoint inside a function that is inlined away and called
    % several times from go/2. This means we will have several occurrences
    % of the line with the breakpoint, and we want to ensure they all
    % get a breakpoint set
    Line = 14,
    ok = erl_debugger:breakpoint(inlined_funs, Line, true),

    TestCaseProcess = self(),
    X0 = 42,
    Pid = erlang:spawn(fun() -> inlined_funs:go(TestCaseProcess, X0) end),

    {Pid, Resume1} = ?assertBreakpointHit(Session, {inlined_funs, go, 2}, Line),
    ?assertMailboxEmpty(),
    ok = Resume1(),
    ?expectReceive({executed, Pid, {inlined_funs, f, 2}, {line, Line}}),

    {Pid, Resume2} = ?assertBreakpointHit(Session, {inlined_funs, go, 2}, Line),
    ?assertMailboxEmpty(),
    ok = Resume2(),
    ?expectReceive({executed, Pid, {inlined_funs, f, 2}, {line, Line}}),

    ExpectedResult = X0 + 2,
    ?expectReceive({done, Pid, ExpectedResult}),
    ok.

test_works_with_large_number_of_live_xregs(Config) ->
    Session = ?config(debugger_session, Config),
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),

    compile_and_load_module(Config, many_live_xregs, [beam_debug_info]),

    Line = 16,
    ok = erl_debugger:breakpoint(many_live_xregs, Line, true),

    TestCaseProcess = self(),
    N = 10,

    Pid = erlang:spawn(fun() ->
        Res = erlang:apply(many_live_xregs, many_args, [N, 0 | lists:seq(1, 98)]),
        TestCaseProcess ! {result, self(), Res}
    end),

    [
        begin
            {Pid, Resume} = ?assertBreakpointHit(Session, {many_live_xregs, many_args, 100}, Line),
            ?assertMailboxEmpty(),
            ok = Resume()
        end
        || _ <- lists:seq(0,N)
    ],

    ExpectedResult = (98 * (98+1) div 2) * N,
    ?expectReceive({result, Pid, ExpectedResult}),
    ok.

test_works_with_a_huge_stack_depth_which_should_require_gc(Config) ->
    Session = ?config(debugger_session, Config),
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    compile_and_load_module(Config, gc_test, [beam_debug_info]),

    Line = 12,
    ok = erl_debugger:breakpoint(gc_test, Line, true),

    N = 100_000,

    TC = self(),
    Pid = erlang:spawn(fun() ->
        gc_test:go({max_recursion_depth, N}),
        TC ! {done, self()}
    end),

    [
        begin
            {Pid, Resume} = ?assertBreakpointHit(Session, {gc_test, go, 1}, Line),
            ?assertMailboxEmpty(),
            ok = Resume()
        end
        || _ <- lists:seq(1,N)
    ],

    ?expectReceive({done, Pid}),
    ok.

test_avoids_blocking_debugger(Config) ->
    Session = ?config(debugger_session, Config),
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    compile_and_load_module(Config, ping, [beam_debug_info]),

    Line = 5,
    ok = erl_debugger:breakpoint(ping, Line, true),

    TestCaseProcess = self(),

    % Sanity-check: the breakpoint is hit if called by another process
    Pid = erlang:spawn(fun() -> ping:ping(TestCaseProcess) end),
    {Pid, Resume1} = ?assertBreakpointHit(Session, {ping, ping, 1}, Line),
    ?assertMailboxEmpty(),
    ok = Resume1(),
    ?expectReceive({pong, Pid}),

    ?assertMailboxEmpty(),

    % Call directly from the current process (the registered debugger), the
    % breakpoint should be ignored
    ping:ping(TestCaseProcess),

    ?expectReceive({pong, TestCaseProcess}),
    ?assertMailboxEmpty(),
    ok.

%% Helpers

compile_and_load_module(Config, Mod, Opts) when is_atom(Mod), is_list(Opts) ->
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, atom_to_list(Mod) ++ ".erl"),

    {ok, Mod, Code} = compile:file(File, [binary, report | Opts]),
    {module, Mod} = code:load_binary(Mod, "", Code),
    ok.
