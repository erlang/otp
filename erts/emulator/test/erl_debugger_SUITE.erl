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

% Test cases
-export([test_supported_returns_false/1]).
-export([test_all_functions_fail_with_undef/1]).
-export([test_supported_returns_true/1]).
-export([test_can_toggle_instrumentations/1]).
-export([test_toggle_instrumentations_validates_input/1]).
-export([test_register_and_unregister_debugger/1]).
-export([test_debugger_unregistered_when_dead/1]).

-include_lib("stdlib/include/assert.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 20}}].

all() ->
    [
        {group, debugger_support_disabled},
        {group, debugger_support_enabled},
        {group, instrumentations},
        {group, registration}
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
        ]}
    ].

init_per_suite(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    Config.

end_per_suite(_Config) ->
    erts_debug:set_internal_state(available_internal_state, false),
    ok.

init_per_group(debugger_support_disabled, Config) ->
    Config;
init_per_group(_Group, Config) ->
    erts_debug:set_internal_state(debugger_support, true),
    Config.

end_per_group(_Group, _Config) ->
    ok.

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
    #{line_breakpoint := true} = erl_debugger:instrumentations(),

    ok = erl_debugger:toggle_instrumentations(#{}),
    #{line_breakpoint := true} = erl_debugger:instrumentations(),

    ok = erl_debugger:toggle_instrumentations(#{line_breakpoint => false}),
    #{line_breakpoint := false} = erl_debugger:instrumentations(),

    ok = erl_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    #{line_breakpoint := true} = erl_debugger:instrumentations(),
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
