%%
%%
%% Copyright WhatsApp Inc. and its affiliates. All rights reserved.
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
%%-------------------------------------------------------------------
%% @author Maxim Fedorov <maximfca@gmail.com>
%% Basic heap profiler tests.
-module(hprof_SUITE).
-author("maximfca@gmail.com").

%% Test server callbacks
-export([suite/0, all/0]).

%% Test cases exports
-export([
    ad_hoc/0, ad_hoc/1,
    sort/0, sort/1,
    rootset/0, rootset/1,
    set_on_spawn/0, set_on_spawn/1, seq/1,
    live_trace/0, live_trace/1,
    patterns/0, patterns/1, pattern_fun/1, pattern_fun/2, pattern_fun/3,
    processes/0, processes/1,
    server/0, server/1,
    hierarchy/0, hierarchy/1,
    code_reload/0, code_reload/1
]).

-include_lib("stdlib/include/assert.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [ad_hoc, sort, rootset, set_on_spawn, live_trace, patterns,
        processes, server, hierarchy, code_reload].

%%--------------------------------------------------------------------
%% TEST CASES

ad_hoc() ->
    [{doc, "Ad-hoc examples from documentation"}].

ad_hoc(Config) when is_list(Config) ->
    ct:capture_start(),
    ok = hprof:profile(lists, seq, [1, 16]),
    ct:capture_stop(),
    Output = string:lexemes(lists:flatten(ct:capture_get()), "\n"),
    %% expect third line to contain lists:seq_loop: "lists seq_loop/3      5     32         6  [100.00]",
    ?assertMatch(["lists", "seq_loop/3", "5", "32" | _], string:lexemes(lists:nth(3, Output), " ")),
    %% spawn examples
    SpawnFun =
        fun () ->
            {Pid, MRef} = spawn_monitor(fun () -> lists:seq(1, 32) end),
            receive {'DOWN', MRef, process, Pid, normal} -> done end
       end,
    %% trace subset examples
    {done, Profile1} = hprof:profile(SpawnFun, #{pattern => [{lists, seq_loop, '_'}], report => return}),
    ?assertMatch([{lists, seq_loop, 3, [{_, 9, 64}]}], Profile1),
    %% timer
    {{'EXIT', timeout}, Profile2} = hprof:profile(
        fun () -> Delay = hd(lists:seq(5001, 5032)), timer:sleep(Delay) end,
        #{timeout => 1000, report => return}),
    ?assertMatch([{lists, seq_loop, 3, [{_, 9, 64}]}], Profile2).

sort() ->
    [{doc, "Tests sorting methods work"}].

sort(Config) when is_list(Config) ->
    %% sort examples
    ct:capture_start(),
    ok = hprof:profile(
        fun () ->
            Group = lists:seq(100, 120),
            rand:uniform(hd(Group))
        end, #{report => {process, {words_per_call, descending}}}),
    ct:capture_stop(),
    Out = string:lexemes(lists:flatten(ct:capture_get()), "\n"),
    %% words per call is 5th column
    Col = 5,
    Column5 = [string:to_integer(lists:nth(Col, Lexemes)) || Ln <- Out,
        length(Lexemes = string:lexemes(Ln, " ")) >= Col],
    WPC = [Words || {Words, []} <- Column5],
    %% ensure descending sort
    ?assertEqual(lists:reverse(lists:sort(WPC)), WPC).

rootset() ->
    [{doc, "Tests rootset of processes supplied to ad-hoc profiler"}].

rootset(Config) when is_list(Config) ->
    TestCase = ?FUNCTION_NAME,
    {ok, Scope} = pg:start_link(TestCase),
    Fun = fun () -> ok = pg:join(TestCase, lists:seq(1, 2), self()) end,
    %% rootset tests
    {ok, Profile} = hprof:profile(Fun, #{rootset => [TestCase], report => return}),
    TwoProcs = hprof:inspect(Profile),
    ?assertEqual(2, maps:size(TwoProcs)), %% must be pg and "profiled process"
    %% now trace all processes, existing and new
    {ok, ProfAll} = hprof:profile(Fun, #{rootset => processes, report => return}),
    %% at least hprof, pg2 and new profiled processes are traced
    ?assert(map_size(hprof:inspect(ProfAll)) >= 3),
    gen_server:stop(Scope).

set_on_spawn() ->
    [{doc, "Tests hprof running with extra spawned processes"}].

set_on_spawn(Config) when is_list(Config) ->
    %% profile a function that spawns additional process
    {done, Profile} = hprof:profile(
        fun () ->
            {Pid, MRef} = spawn_monitor(fun () -> lists:seq(1, 32) end),
            receive {'DOWN', MRef, process, Pid, normal} -> done end
        end, #{report => return, set_on_spawn => false}),
    {done, ProfileMFA} = hprof:profile(?MODULE, seq, [32], #{report => return, set_on_spawn => false}),
    %% check totals
    {_G1, TotalProfile} = hprof:inspect(Profile, total, words),
    {_G2, TotalProfileMFA} = hprof:inspect(ProfileMFA, total, words),
    %% only 1 process must be there
    ?assertEqual(1, maps:size(hprof:inspect(Profile)), {set_on_spawn, Profile}),
    %% check per-process stats
    case erlang:system_info(wordsize) of
        8 -> ?assertMatch({?MODULE, {_, 0}, 1, 9, 9, _}, lists:keyfind(?MODULE, 1, TotalProfile));
        4 -> ?assertMatch({?MODULE, {_, 0}, 1, 10, 10, _}, lists:keyfind(?MODULE, 1, TotalProfile))
    end,
    %% MFA takes 6 more words. This test should be improved to depend less on the internal
    %%  implementation.
    case erlang:system_info(wordsize) of
        8 -> ?assertMatch({?MODULE, {seq, 1}, 1, 13, 13, _}, lists:keyfind(?MODULE, 1, TotalProfileMFA));
        4 -> ?assertMatch({?MODULE, {seq, 1}, 1, 14, 14, _}, lists:keyfind(?MODULE, 1, TotalProfileMFA))
    end.

seq(Max) ->
    {Pid, MRef} = spawn_monitor(fun () -> lists:seq(1, Max) end),
    receive {'DOWN', MRef, process, Pid, normal} -> done end.

live_trace() ->
    [{doc, "Tests memory tracing for pre-existing processes"}].

live_trace(Config) when is_list(Config) ->
    {ok, _Srv} = hprof:start_link(),
    Pid = spawn_link(
        fun () ->
            receive
                {From, C} ->
                    [lists:seq(1, 2) || _ <- lists:seq(1, C)],
                    From ! {self(), done}
            end
        end),
    _ = hprof:set_pattern(?MODULE, '_', '_'),
    1 = hprof:enable_trace(Pid),
    Pid ! {self(), 12},
    receive {Pid, done} -> ok end,
    catch hprof:disable_trace(Pid),
    Profile = hprof:collect(),
    ProcInspected = hprof:inspect(Profile),
    %% white box check: list comprehension with 1-arity, and 100% allocation
    %% hprof:format(user, ProcInspected),
    #{Pid := {48, [{?MODULE, {_LC, 1}, 13, 48, 3, 100.0}]}} = ProcInspected,
    hprof:stop().

patterns() ->
    [{doc, "Tests pattern enable/disable correctness"}].

patterns(Config) when is_list(Config) ->
    {ok, _Srv} = hprof:start_link(),
    %% test errors
    ?assertEqual({error, {not_traced, pg, get_members, '_'}}, hprof:clear_pattern(pg, get_members, '_')),
    ?assertEqual({error, {trace_pattern, ?MODULE, seq, 2}}, hprof:set_pattern(?MODULE, seq, 2)),
    %% successful patterns
    1 = hprof:set_pattern(?MODULE, seq, 1),
    3 = hprof:set_pattern(?MODULE, pattern_fun, '_'),
    1 = hprof:clear_pattern(?MODULE, pattern_fun, 2),
    Expected = [{pattern_fun, 1}, {pattern_fun, 3}, {seq, 1}],
    ?assertEqual(#{?MODULE => Expected}, hprof:get_trace_map()),
    %% verify tracing flags
    verify_trace([{?MODULE, F, A} || {F, A} <- Expected], [{?MODULE, pattern_fun, 2}]),
    %% trace the entire lists module, and then exclude pattern_fun/1,2,3 and seq/1
    _ = hprof:set_pattern(?MODULE, '_', '_'),
    3 = hprof:clear_pattern(?MODULE, pattern_fun, '_'),
    1 = hprof:clear_pattern(?MODULE, seq, 1),
    Cleared = [{pattern_fun, 1}, {pattern_fun, 2}, {pattern_fun, 3}, {seq, 1}],
    Traced = ?MODULE:module_info(functions) -- Cleared,
    verify_trace([{?MODULE, F, A} || {F, A} <- Traced], [{?MODULE, F, A} || {F, A} <- Cleared]),
    %% clear all, which clears lists too
    _ = hprof:clear_pattern('_', '_', '_'),
    verify_trace([], [{?MODULE, F, A} || {F, A} <- Traced ++ Cleared]),
    ?assertEqual(#{}, hprof:get_trace_map()),
    hprof:stop().

verify_trace(On, Off) ->
    [?assertEqual({call_memory, []}, erlang:trace_info(MFA, call_memory)) || MFA <- On],
    [?assertEqual({call_memory, false}, erlang:trace_info(MFA, call_memory)) || MFA <- Off].

pattern_fun(Any) -> Any.
pattern_fun(Any, Any2) -> {Any, Any2}.
pattern_fun(Any, Any2, Any3) -> {Any, Any2, Any3}.

processes() ->
    [{doc, "Tests that process management for enabling/disabling traces works"}].

processes(Config) when is_list(Config) ->
    Pid = spawn_link(fun spawn_loop/0),
    Pid2 = spawn_link(fun spawn_loop/0),
    register(?FUNCTION_NAME, Pid2),
    %% test a mix of pids/registered processes/single PID calls
    ?assertEqual(2, hprof:enable_trace([Pid, Pid2])),
    ?assertEqual(0, hprof:disable_trace('$sure_not_exist')),
    ?assertEqual({1, ['$sure_not_exist']}, hprof:enable_trace([Pid, '$sure_not_exist'])),
    ok = gen:stop(Pid),
    ok = gen:stop(Pid2).

server() ->
    [{doc, "Tests for gen_server-based API"}].

server(Config) when is_list(Config) ->
    %% start an extra pg scope - we'll trace it during profiling
    {ok, Scope} = pg:start_link(?FUNCTION_NAME),
    %% simulate existing process
    Pid = spawn_link(fun spawn_loop/0),
    %% start the profiler
    {ok, Srv} = hprof:start_link(),
    %% test ad-hoc profile clash
    ?assertException(error, {already_started, Srv}, hprof:profile(fun spawn_loop/0)),
    %% test live trace
    1 = hprof:set_pattern(?MODULE, dispatch, '_'),
    _ = hprof:set_pattern(pg, '_', '_'),
    %% watch for pg traces and for our process
    2 = hprof:enable_trace([Pid, ?FUNCTION_NAME]),
    %% run the traced operation
    _ = gen_server:call(Pid, {apply, pg, join, [?FUNCTION_NAME, group, Pid]}),
    %% collect profile (can save it to a file for later analysis)
    FirstProfile = hprof:collect(),
    %% must not be empty, and must contain 3-words dispatch from this module,
    %%  and at least something from pg in two processes
    ?assertNotEqual([], FirstProfile),
    ?assertEqual({?MODULE, dispatch, 2, [{Pid, 1, 3}]}, lists:keyfind(?MODULE, 1, FirstProfile)),
    ?assertMatch({pg, handle_call, 3, [{Scope, _, _}]}, lists:keyfind(handle_call, 2, FirstProfile)),
    ?assertMatch({pg, join, 3, [{Pid, _, _}]}, lists:keyfind(join, 2, FirstProfile)),
    %% pause tracing
    ok = hprof:pause(),
    %% ensure paused by running more code but keeping the trace
    %% ensure collection still returns the previous result
    _ = gen_server:call(Pid, {apply, pg, join, [?FUNCTION_NAME, group, Pid]}),
    ?assertEqual(FirstProfile, hprof:collect()),
    %% continue, ensure new results are collected
    ok = hprof:continue(),
    _ = gen_server:call(Pid, {apply, pg, leave, [?FUNCTION_NAME, group, [Pid, Pid]]}),
    ?assertNotEqual(FirstProfile, hprof:collect()),
    %% restart all counters from zero and ensure that we again collect the original data
    ok = hprof:restart(),
    _ = gen_server:call(Pid, {apply, pg, join, [?FUNCTION_NAME, group, Pid]}),
    ?assertEqual(FirstProfile, hprof:collect()),

    %% test ad-hoc profiling can be done while running server-aided
    %% for that, profiler should have very specific pattern
    {_, AdHoc} = hprof:profile(lists, seq, [1, 32], #{registered => false, pattern => {lists, '_', '_'},
        report => return}),
    %% check totals: must be 64 words allocated by a single lists:seq_loop
    ?assertMatch({64, [{lists, _, _, 64, _, _}]}, hprof:inspect(AdHoc, total, words)),
    %% verify that server-aided version still works
    ?assertEqual(FirstProfile, hprof:collect()),
    ok = hprof:stop(),
    ok = gen_server:stop(Scope),
    ok = gen:stop(Pid).

%% nano-gen-server purely for tracing
spawn_loop() ->
    receive
        {'$gen_call', From, Call} ->
            dispatch(Call, From),
            spawn_loop();
        {system, From, {terminate,normal}} ->
            gen:reply(From, ok)
    end.

dispatch({spawn_link, Fun}, From) ->
    gen:reply(From, erlang:spawn_link(Fun));
dispatch({apply, M, F, A}, From) ->
    gen:reply(From, erlang:apply(M, F, A)).

hierarchy() ->
    [{doc, "Tests tracing for process hierarchy"}].

hierarchy(Config) when is_list(Config) ->
    {ok, _Srv} = hprof:start_link(),
    Traced = hprof:enable_trace({all_children, kernel_sup}),
    ?assert(Traced > 5),
    ?assert(hprof:set_pattern(code_server, '_', '_') > 5),
    _ = code:get_path(), %% makes a call to code_server
    %% disabling all processes tracing should return more than "children of"
    ?assert(hprof:disable_trace(processes) > Traced),
    Profile = hprof:collect(),
    hprof:stop(),
    ?assertNotEqual(false, lists:keyfind(handle_call, 2, Profile)).

code_reload() ->
    [{doc, "Tests that collection does not fail for a hot-code-reloaded module"}].

code_reload(Config) when is_list(Config) ->
    Sample = hprof:profile(fun () -> code:load_file(?MODULE) end, #{report => return}),
    %% don't care about actual returned values, but do care that profile/2 does not crash
    ?assertNotEqual([], Sample).
