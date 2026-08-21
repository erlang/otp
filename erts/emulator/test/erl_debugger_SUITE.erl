%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright (c) Meta Platforms, Inc. and affiliates.
%% Copyright Ericsson AB 2025. All Rights Reserved.
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
-export([test_setting_bp_fails_on_recently_deleted_module/1]).
-export([test_setting_bp_fails_on_module_loaded_without_line_bp_instrumentation/1]).
-export([test_setting_bp_fails_on_non_existent_line/1]).
-export([test_setting_bp_fails_on_nonexecutable_line/1]).
-export([test_setting_bp_fails_on_unsupported_lines/1]).
-export([test_hitting_bp_stops_and_notifies_debugger_process/1]).
-export([test_bps_work_with_inlined_functions/1]).
-export([test_bps_work_with_large_number_of_live_xregs/1]).
-export([test_bps_work_with_a_huge_stack_depth_which_should_require_gc/1]).
-export([test_hitting_bp_avoids_blocking_debugger/1]).
-export([test_breakpoints_1_works/1]).
-export([test_breakpoints_3_agrees_with_breakpoints_1/1]).

%% Stack-frame tests
-export([test_stack_frames_returns_running_if_not_suspended/1]).
-export([test_stack_frames_returns_frames/1]).
-export([test_stack_frames_returns_y_regs_controlled_by_size/1]).
-export([test_stack_frames_returns_catch/1]).
-export([test_stack_frames_returns_breakpoint_frame/1]).
-export([test_stack_frames_works_with_hibernate/1]).

%% Register tests
-export([test_peek_stack_frame_slot_works/1]).
-export([test_peek_xreg_works/1]).

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
        {group, line_breakpoints},
        {group, stack_frames},
        {group, registers}
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
            test_setting_bp_fails_on_recently_deleted_module,
            test_setting_bp_fails_on_module_loaded_without_line_bp_instrumentation,
            test_setting_bp_fails_on_non_existent_line,
            test_setting_bp_fails_on_nonexecutable_line,
            test_setting_bp_fails_on_unsupported_lines,
            test_hitting_bp_stops_and_notifies_debugger_process,
            test_bps_work_with_inlined_functions,
            test_bps_work_with_large_number_of_live_xregs,
            test_bps_work_with_a_huge_stack_depth_which_should_require_gc,
            test_hitting_bp_avoids_blocking_debugger,
            test_breakpoints_1_works,
            test_breakpoints_3_agrees_with_breakpoints_1
        ]},
        {stack_frames, [], [
            test_stack_frames_returns_running_if_not_suspended,
            test_stack_frames_returns_frames,
            test_stack_frames_returns_y_regs_controlled_by_size,
            test_stack_frames_returns_catch,
            test_stack_frames_returns_breakpoint_frame,
            test_stack_frames_works_with_hibernate
        ]},
        {registers, [], [
            test_peek_stack_frame_slot_works,
            test_peek_xreg_works
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

init_per_testcase(test_bps_work_with_inlined_functions, _Config) ->
    % TODO(T202887216) unskip once this is fixed
    {skip, "+beam_debug_info is currently blocking inline annotations"};
init_per_testcase(test_stack_frames_returns_y_regs_controlled_by_size, Config) ->
    case erlang:system_info(emu_flavor) of
        emu ->
            compile_and_load_module(Config, call_stacks, [beam_debug_info]),
            try code:get_debug_info(call_stacks) of
                _ -> {fail, "code:get_debug_info() now works on emu, testcase needs updating"}
            catch error:badarg ->
                {skip, "code:get_debug_info() doesn't currently work on emu"}
            end;
        _ ->
            init_per_testcase(default, Config)
    end;
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
    AnotherProc = spawn_link(fun() -> receive after infinity -> ok end end),

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
    {Debugger, MonRef} =
        spawn_monitor(fun() ->
                              receive after infinity -> ok end
                      end),

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

test_setting_bp_fails_on_recently_deleted_module(Config) ->
    Mod = foo,
    erl_debugger:toggle_instrumentations(#{line_breakpoint => false}),
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    code:delete(Mod),
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
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    % NB. This line is currently unsupported since it is a
    % function header (for go/1).
    UnsupportedLine = 6,
    Actual = erl_debugger:breakpoint(Mod, UnsupportedLine, true),
    Expected = {error, {unsupported, UnsupportedLine}},

    Expected = Actual,
    ok.

test_hitting_bp_stops_and_notifies_debugger_process(Config) ->
    Session = ?config(debugger_session, Config),
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    compile_and_load_module(Config, foo, [beam_debug_info]),

    [ok = erl_debugger:breakpoint(foo, Line, true) || Line <- [7, 10, 12, 18, 19]],

    TestCaseProcess = self(),
    Pid = spawn_link(fun() -> foo:go(TestCaseProcess) end),

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

test_bps_work_with_inlined_functions(Config) ->
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
    Pid = spawn_link(fun() -> inlined_funs:go(TestCaseProcess, X0) end),

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

test_bps_work_with_large_number_of_live_xregs(Config) ->
    Session = ?config(debugger_session, Config),
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),

    compile_and_load_module(Config, many_live_xregs, [beam_debug_info]),

    Line = 16,
    ok = erl_debugger:breakpoint(many_live_xregs, Line, true),

    TestCaseProcess = self(),
    N = 10,

    Pid = spawn_link(fun() ->
        Res = apply(many_live_xregs, many_args, [N, 0 | lists:seq(1, 98)]),
        TestCaseProcess ! {result, self(), Res}
    end),

    [begin
         {Pid, Resume} = ?assertBreakpointHit(Session, {many_live_xregs, many_args, 100}, Line),
         ?assertMailboxEmpty(),
         ok = Resume()
     end || _ <- lists:seq(0,N)
    ],

    ExpectedResult = (98 * (98+1) div 2) * N,
    ?expectReceive({result, Pid, ExpectedResult}),
    ok.

test_bps_work_with_a_huge_stack_depth_which_should_require_gc(Config) ->
    Session = ?config(debugger_session, Config),
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    compile_and_load_module(Config, gc_test, [beam_debug_info]),

    Line = 12,
    ok = erl_debugger:breakpoint(gc_test, Line, true),

    N = 100_000,

    TC = self(),
    Pid = spawn_link(fun() ->
                             gc_test:go({max_recursion_depth, N}),
                             TC ! {done, self()}
                     end),

    [begin
         {Pid, Resume} = ?assertBreakpointHit(Session, {gc_test, go, 1}, Line),
         ?assertMailboxEmpty(),
         ok = Resume()
     end || _ <- lists:seq(1, N)
    ],

    ?expectReceive({done, Pid}),
    ok.

test_hitting_bp_avoids_blocking_debugger(Config) ->
    Session = ?config(debugger_session, Config),
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    compile_and_load_module(Config, ping, [beam_debug_info]),

    Line = 5,
    ok = erl_debugger:breakpoint(ping, Line, true),

    TestCaseProcess = self(),

    %% Sanity-check: the breakpoint is hit if called by another process
    Pid = spawn_link(fun() -> ping:ping(TestCaseProcess) end),
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

test_breakpoints_1_works(Config) ->
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    compile_and_load_module(Config, ping, [beam_debug_info]),

    Expected1 = #{
        {module_info,0} => #{},
        {module_info,1} => #{},
        {ping,1} => #{5 => false,6 => false}
    },
    {ok, Expected1} = erl_debugger:breakpoints(ping),

    ok = erl_debugger:breakpoint(ping, 5, true),
    Expected2 = #{
        {module_info,0} => #{},
        {module_info,1} => #{},
        {ping,1} => #{5 => true,6 => false}
    },
    {ok, Expected2} = erl_debugger:breakpoints(ping),

    ok = erl_debugger:breakpoint(ping, 6, true),
    Expected3 = #{
        {module_info,0} => #{},
        {module_info,1} => #{},
        {ping,1} => #{5 => true,6 => true}
    },
    {ok, Expected3} = erl_debugger:breakpoints(ping),

    ok = erl_debugger:breakpoint(ping, 5, false),
    ok = erl_debugger:breakpoint(ping, 6, false),
    {ok, Expected1} = erl_debugger:breakpoints(ping),

    code:purge(ping),
    code:delete(ping),
    {error, badkey} = erl_debugger:breakpoints(ping),
    code:purge(ping),
    {error, badkey} = erl_debugger:breakpoints(ping),

    ok.

test_breakpoints_3_agrees_with_breakpoints_1(Config) ->
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    compile_and_load_module(Config, ping, [beam_debug_info]),
    ok = erl_debugger:breakpoint(ping, 5, true),

    {ok, AllBreakpoints} = erl_debugger:breakpoints(ping),
    [?assertEqual({ok, Val}, erl_debugger:breakpoints(ping, F, A))
    || {F, A} := Val <- AllBreakpoints
    ],

    {error, {badkey, {non_existent_function, 2}}} =
        erl_debugger:breakpoints(ping, non_existent_function, 2),

    code:purge(ping),
    code:delete(ping),
    {error, {badkey, ping}} = erl_debugger:breakpoints(ping, foo, 2),
    code:purge(ping),
    {error, {badkey, ping}} = erl_debugger:breakpoints(ping, foo, 2),

    ok.

%% Stack-frames tests
test_stack_frames_returns_running_if_not_suspended(_Config) ->
    P = spawn_link(fun() -> receive _ -> ok end end),
    running = erl_debugger:stack_frames(P, 1),

    true = erlang:suspend_process(P),
    [_ | _] = erl_debugger:stack_frames(P, 1),

    true = erlang:resume_process(P),
    wait_for_process_status(P, waiting),

    running = erl_debugger:stack_frames(P, 1),

    P ! done,
    ok.

-define(IS_ADDR(Addr), is_integer(Addr) andalso Addr > 0).

test_stack_frames_returns_frames(Config) ->
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),

    Mod = call_stacks,
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    %% To sync from Mod:base_level().
    register(?MODULE, self()),

    %% Launch a process, sync at a known location and suspend it
    %% so we can inspect the stack.
    P = spawn_link(Mod, three_levels, [42, 13]),
    ?expectReceive({sync, P}),
    wait_for_process_status(P, waiting),
    erlang:suspend_process(P),

    Actual = erl_debugger:stack_frames(P, 1),
    ?assertMatch(
        [
            {4, #{function := {Mod, base_level, 1}, line := 18}, #{slots := [_Y0], code := Addr4}},
            {3, #{function := {Mod, one_level, 1}, line := 13}, #{slots := [], code := Addr3}},
            {2, #{function := {Mod, two_levels, 2}, line := 9}, #{slots := [], code := Addr2}},
            {1, #{function := {Mod, three_levels, 2}, line := 5}, #{slots := [], code := Addr1}},
            {0, '<terminate process normally>', #{slots := [], code := Addr0}}
        ] when ?IS_ADDR(Addr0)
            andalso ?IS_ADDR(Addr1)
            andalso ?IS_ADDR(Addr2)
            andalso ?IS_ADDR(Addr3)
            andalso ?IS_ADDR(Addr4),
        Actual
    ),
    ok.

test_stack_frames_returns_y_regs_controlled_by_size(Config) ->
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),

    Mod = call_stacks,
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    %% To sync from Mod:base_level().
    erlang:register(?MODULE, self()),

    Line = 23,

    %% Get the var mapping for Line, so that we ensure we are
    %% returning Y-registers in the right order.
    YRegMap = #{
        YRegNo => Var
        || {L, #{vars := SymMap}} <- code:get_debug_info(Mod),
           L == Line,
           {Var, {y, YRegNo}} <- SymMap
    },

    P = spawn_link(Mod, args_as_yvars, [foo, [1,2,3,4,5], ~"hellooooooooo"]),
    ?expectReceive({sync, P}),
    erlang:suspend_process(P),

    CheckVarsSize = fun(Size, ExpectedVars) ->
        case erl_debugger:stack_frames(P, Size) of
            [{1, #{function := {Mod, args_as_yvars, 3}, line := Line}, #{slots := YRegs}} | _] ->
                ActualVars = #{
                    maps:get(YRegNo, YRegMap) => YRegVal
                    || {YRegNo, YRegVal} <- lists:enumerate(0, YRegs)
                },
                ?assertEqual(ExpectedVars, ActualVars)
        end
    end,

    ExpectedListSize = 10,
    ExpectedBinSize = 4,  % TODO: we are currently counting only heap space

    %% Immediate values are free, we get them even with size 0
    CheckVarsSize(0, #{
        ~"X" => {value, foo},
        ~"Y" => {too_large, ExpectedListSize},
        ~"Z" => {too_large, ExpectedBinSize}
    }),

    %% Size limit is respected
    CheckVarsSize(min(ExpectedListSize, ExpectedBinSize) - 1, #{
        ~"X" => {value, foo},
        ~"Y" => {too_large, ExpectedListSize},
        ~"Z" => {too_large, ExpectedBinSize}
    }),
    CheckVarsSize(min(ExpectedListSize, ExpectedBinSize), #{
        ~"X" => {value, foo},
        ~"Y" => {too_large, ExpectedListSize},
        ~"Z" => {value, ~"hellooooooooo"}
    }),
    CheckVarsSize(max(ExpectedListSize, ExpectedBinSize), #{
        ~"X" => {value, foo},
        ~"Y" => {value, [1,2,3,4,5]},
        ~"Z" => {value, ~"hellooooooooo"}
    }),

    ok.

test_stack_frames_returns_catch(Config) ->
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),

    Mod = call_stacks,
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    %% To sync from Mod:base_level().
    register(?MODULE, self()),

    P = spawn_link(Mod, call_with_catches, [42]),
    ?expectReceive({sync, P}),
    erlang:suspend_process(P),

    Actual = erl_debugger:stack_frames(P, 0),
    ?assertMatch(
        [
            {2,
                #{function := {call_stacks, call_with_catches_aux, 1}, line := 33},
                #{slots :=  [
                    {value, 43},
                    {'catch', #{function := {call_stacks, call_with_catches_aux, 1}, line := 33}}
                ]}
            },
            {1,
                #{function := {call_stacks, call_with_catches, 1}, line := 27},
                #{slots := [
                    {value, 42},
                    {'catch', #{function := {call_stacks, call_with_catches, 1}, line := 27}}
                ]}
            },
            {0, '<terminate process normally>', #{slots := []}}
        ],
        Actual
    ),
    ok.

test_stack_frames_returns_breakpoint_frame(Config) ->
    Session = ?config(debugger_session, Config),
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),

    Mod = call_stacks,
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    Line = 5,
    erl_debugger:breakpoint(Mod, Line, true),

    P = spawn_link(Mod, three_levels, [42, 13]),

    {P, _Resume} = ?assertBreakpointHit(Session, {Mod, three_levels, 2}, Line),
    erlang:suspend_process(P),

    Actual = erl_debugger:stack_frames(P, 0),
    ?assertMatch(
         [
            {3, #{function := {erts_internal,breakpoint, 4}}, _},
            {2, '<breakpoint>', #{slots := [_SavedX0 = {value, 42}, _SavedX1 = {value, 13}]}},
            {1, #{function := {call_stacks, three_levels, 2}, line := 5}, #{slots := []}},
            {0, '<terminate process normally>', #{slots := []}}
        ],
        Actual
    ),
    ok.

test_stack_frames_works_with_hibernate(Config) ->
    %% NB. This testcase should cover all BIFs using beam_run_process
    %% internally, e.g. when a process is suspended while starting to
    %% execute `erlang:apply(M, F, A)`.

    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),

    Mod = call_stacks,
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    %% To sync from Mod:base_level().
    register(?MODULE, self()),

    SubjectFun = fun Mod:sync_and_hibernate/0,
    Test = fun test_stack_frames_works_with_hibernate/2,
    test_both_creation_orders(Mod, SubjectFun, Test),

    ok.

test_stack_frames_works_with_hibernate(_Mod, P) ->
    erlang:suspend_process(P),

    Actual = erl_debugger:stack_frames(P, 0),
    ?assertMatch(
        [
         {1, #{function := {erlang, hibernate, 3},
               line := undefined},
          #{slots := []}},
         {0, '<terminate process normally>',  #{slots := []}}
        ],
       Actual
      ),

    exit(P, normal),
    ok.

%% Registers tests
test_peek_stack_frame_slot_works(Config) ->
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),

    Mod = call_stacks,
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    %% To sync from Mod:base_level().
    register(?MODULE, self()),

    Test = fun test_peek_stack_frame_slot_works/2,
    SubjectFun =
        fun() ->
                catch Mod:args_as_yvars(foo, [1,2,3,4,5], ~"hellooooooooo")
        end,
    test_both_creation_orders(Mod, SubjectFun, Test),

    ok.

test_peek_stack_frame_slot_works(_Mod, P) ->
    %% While not suspended, return running.
    running = erl_debugger:peek_stack_frame_slot(P, 0, 0, 0),

    erlang:suspend_process(P),
    MaxYRegSize =
        lists:max([N || {_FrameId, _Fun, #{slots := Slots}} <-
                            erl_debugger:stack_frames(P, 0),
                        {too_large, N} <- Slots
                  ]),

    CheckItMatchesStackFrames =
        fun(MaxSize) ->
                StackFrames = erl_debugger:stack_frames(P, MaxSize),
                [?assertEqual(
                    SlotVal,
                    erl_debugger:peek_stack_frame_slot(P, FrameId, 
                                                       SlotNo, MaxSize),
                    #{frame => FrameId, slot => SlotNo,
                      size => MaxSize}) ||
                    {FrameId, _, #{slots := Slots}} <- StackFrames,
                    {SlotNo, SlotVal} <- lists:enumerate(0, Slots)
                ],
                ok
        end,

    [CheckItMatchesStackFrames(MaxSize) ||
        MaxSize <- lists:seq(0, MaxYRegSize)],
    ok.

test_peek_xreg_works(Config) ->
    erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),

    Mod = call_stacks,
    compile_and_load_module(Config, Mod, [beam_debug_info]),

    register(?MODULE, self()),

    SubjectFun = fun Mod:sync_and_hibernate/0,
    Test = fun test_peek_xreg_works/2,
    test_both_creation_orders(Mod, SubjectFun, Test),

    ok.

test_peek_xreg_works(Mod, P) ->
    %% While the process is running, no results.
    running = erl_debugger:xregs_count(P),
    running = erl_debugger:peek_xreg(P, 0, 0),

    %% Suspend the process, so we can inspect it.
    erlang:suspend_process(P),

    %% We are paused in a call to erlang:hibernate/3 with only X0,X1,X2
    %% live.  We test the arguments to the call.
    3 = erl_debugger:xregs_count(P),
    {value, Mod} = erl_debugger:peek_xreg(P, 0, 0),
    {value, three_levels} = erl_debugger:peek_xreg(P, 1, 0),
    {too_large, ListSize} = erl_debugger:peek_xreg(P, 2, 0),

    %% Test that the size control works.
    {too_large, ListSize} = erl_debugger:peek_xreg(P, 2, ListSize - 1),
    {value, [10, 20]} = erl_debugger:peek_xreg(P, 2, ListSize),
    ok.

%% Helpers

test_both_creation_orders(Mod, SubjectFun, Test) ->
    test_creation_order(subject_first, Mod, SubjectFun, Test),
    test_creation_order(debugger_first, Mod, SubjectFun, Test),

    ok.

test_creation_order(Order, Mod, SubjectFun, Test) ->
    DebuggerFun = fun() ->
                          receive
                              {go_ahead, Subject} ->
                                  Test(Mod, Subject)
                          end
                  end,

    %% Test creating the debugger and subject process in different
    %% order to ensure that there are no lock order conflicts.
    {{DebuggerPid, DebuggerRef}, Subject} =
        case Order of
            debugger_first ->
                First = spawn_monitor(DebuggerFun),
                Second = spawn_link(SubjectFun),
                {First, Second};
            subject_first ->
                First = spawn_link(SubjectFun),
                Second = spawn_monitor(DebuggerFun),
                {Second, First}
        end,

    ?expectReceive({sync, Subject}),
    DebuggerPid ! {go_ahead, Subject},

    receive
        {'DOWN', DebuggerRef, process, DebuggerPid, Reason} ->
            Reason = normal
    end.

compile_and_load_module(Config, Mod, Opts) when is_atom(Mod), is_list(Opts) ->
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, atom_to_list(Mod) ++ ".erl"),

    {ok, Mod, Code} = compile:file(File, [binary, report | Opts]),
    {module, Mod} = code:load_binary(Mod, "", Code),
    ok.

wait_for_process_status(P, Status) ->
    wait_for_process_status(P, Status, 2_000).

wait_for_process_status(_, _, Timeout) when Timeout =< 0 ->
    error(timeout_waiting_for_status);
wait_for_process_status(P, Status, Timeout) when is_integer(Timeout) ->
    T0 = erlang:system_time(millisecond),

    case erlang:process_info(P, status) of
        {status, Status} ->
            ok;
        _ ->
            T1 = erlang:system_time(millisecond),
            Elapsed = T1 - T0,
            wait_for_process_status(P, Status, Timeout - Elapsed)
    end.
