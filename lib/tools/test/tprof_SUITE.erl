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
%% Basic process tracing profiler tests.
-module(tprof_SUITE).
-author("maximfca@gmail.com").

%% Test server callbacks
-export([suite/0, all/0, groups/0,
         init_per_group/2, end_per_group/2]).

%% Test cases exports
-export([
    call_count_ad_hoc/0, call_count_ad_hoc/1,
    call_time_ad_hoc/0, call_time_ad_hoc/1,
    call_memory_ad_hoc/0, call_memory_ad_hoc/1,
    lists_seq_loop/1, call_memory_total/1,
    sort/0, sort/1,
    rootset/0, rootset/1,
    set_on_spawn/0, set_on_spawn/1, seq/1,
    live_trace/0, live_trace/1,
    patterns/0, patterns/1, pattern_fun/1, pattern_fun/2, pattern_fun/3,
    processes/0, processes/1,
    server/0, server/1,
    hierarchy/0, hierarchy/1,
    code_reload/0, code_reload/1,
    code_load/0, code_load/1
]).

-include_lib("stdlib/include/assert.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [call_count_ad_hoc, %% Cannot be run in parallel
     {group, all}].

groups() ->
    [{all, parallel(),
      [call_time_ad_hoc, call_memory_ad_hoc,
       call_memory_total, sort, rootset, set_on_spawn,
       code_load, code_reload,
       {group, default_session},
       {group, custom_session}]},
     {default_session,[],session()},
     {custom_session,parallel(),session()}].

%% Because of scalability/performance issues in trace:info/3 we only run in parallel
%% if we have a "small" amount of schedulers online.
parallel() ->
    [parallel || erlang:system_info(schedulers_online) < 64].

init_per_group(custom_session, Config) ->
    [{session, ?MODULE} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

session() ->
    [live_trace, patterns, processes, server, hierarchy].

%%--------------------------------------------------------------------
%% TEST CASES

call_count_ad_hoc() ->
    [{doc, "Ad-hoc examples for call_count measurement"}].

call_count_ad_hoc(Config) when is_list(Config) ->
    ct:capture_start(),
    ok = tprof:profile(lists, seq, [1, 16]),
    ct:capture_stop(),
    Output = string:lexemes(lists:flatten(ct:capture_get()), "\n"),

    %% We check that only the things profiled are lists:seq/2 and lists:seq_loop/3
    %% and that the output looks good
    ?assertMatch(
       ["FUNCTION          CALLS  [    %]",
        "lists:seq/2           1  ["++_,
        "lists:seq_loop/3      5  ["++_,
        "                         [100.0]"], Output),

    %% spawn examples
    SpawnFun =
        fun () ->
            {Pid, MRef} = spawn_monitor(fun () -> lists:seq(1, 32) end),
            receive {'DOWN', MRef, process, Pid, normal} -> done end
       end,
    %% trace subset examples
    {done, Profile1} = tprof:profile(SpawnFun, #{pattern => [{lists, seq_loop, '_'}],
                                                 report => return, type => call_count}),
    ?assertMatch({call_count, [{lists, seq_loop, 3, [{all, _, _}]}]}, Profile1),

    %% timer
    {{'EXIT', timeout}, {call_count, Profile2}} = tprof:profile(
        fun () -> Delay = hd(lists:seq(5001, 5032)), receive after Delay -> ok end end,
        #{timeout => 1000, report => return, type => call_count }),
    ?assertMatch([{lists, seq, 2, [{_, 1, _}]},
                  {lists, seq_loop, 3, [{_, 9, _}]},
                  {?MODULE, _, _, _}], lists:sort(Profile2)),

    %% timer with patterns
    {{'EXIT', timeout}, Profile3} = tprof:profile(
        fun () -> Delay = hd(lists:seq(5001, 5032)), timer:sleep(Delay) end,
        #{timeout => 1000, report => return, type => call_count,
          pattern => [{lists, seq_loop, '_'}]}),
    ?assertMatch({call_count, [{lists, seq_loop, 3, [{all, _, _}]}]}, Profile3).

call_time_ad_hoc() ->
    [{doc, "Ad-hoc examples for call_time measurement"}].

call_time_ad_hoc(Config) when is_list(Config) ->
    ct:capture_start(),
    ok = tprof:profile(lists, seq, [1, 1000], #{type => call_time}),
    ct:capture_stop(),
    Output = string:lexemes(lists:flatten(ct:capture_get()), "\n"),

    %% We check that only the things profiled are lists:seq/2 and lists:seq_loop/3
    ?assertMatch(
       ["****** Process "++_,
        "FUNCTION          CALLS  TIME"++_,
        "lists:seq/2           1       "++_,
        "lists:seq_loop/3    251       "++_,
        "                              "++_],
       Output),

    %% spawn examples
    SpawnFun =
        fun () ->
            {Pid, MRef} = spawn_monitor(fun () -> lists:seq(1, 32) end),
            receive {'DOWN', MRef, process, Pid, normal} -> done end
       end,
    %% trace subset examples
    {done, Profile1} = tprof:profile(SpawnFun, #{pattern => [{lists, seq_loop, '_'}],
                                                 report => return, type => call_time}),
    ?assertMatch({call_time, [{lists, seq_loop, 3, [{_, 9, _}]}]}, Profile1),

    %% timer
    {{'EXIT', timeout}, {call_time, Profile2}} = tprof:profile(
        fun () -> Delay = hd(lists:seq(5001, 5032)), receive after Delay -> ok end end,
        #{timeout => 1000, report => return, type => call_time }),
    ?assertMatch([{lists, seq, 2, [{_, 1, _}]},
                  {lists, seq_loop, 3, [{_, 9, _}]},
                  {?MODULE, _, _, _}
                 ], lists:sort(Profile2)),

    %% timer with filter
    {{'EXIT', timeout}, Profile3} = tprof:profile(
        fun () -> Delay = hd(lists:seq(5001, 5032)), timer:sleep(Delay) end,
        #{timeout => 1000, report => return, type => call_time,
          pattern => [{lists, seq_loop, '_'}]}),
    ?assertMatch({call_time, [{lists, seq_loop, 3, [{_, 9, _}]}]}, Profile3).

call_memory_ad_hoc() ->
    [{doc, "Ad-hoc examples for call_memory measurement"}].

call_memory_ad_hoc(Config) when is_list(Config) ->
    ct:capture_start(),
    ok = tprof:profile(lists, seq, [1, 16], #{type => call_memory}),
    ct:capture_stop(),
    Output = string:lexemes(lists:flatten(ct:capture_get()), "\n"),
    %% We check that only the things profiled are lists:seq/2 and lists:seq_loop/3
    %% and that the output looks good
    ?assertMatch(
       ["****** Process "++_,
        "FUNCTION          CALLS  WORDS  PER CALL  [     %]",
        "lists:seq_loop/3      5     32      6.40  [100.00]",
        "                            32            [ 100.0]"], Output),
    %% spawn examples
    SpawnFun =
        fun () ->
            {Pid, MRef} = spawn_monitor(fun () -> lists:seq(1, 32) end),
            receive {'DOWN', MRef, process, Pid, normal} -> done end
       end,
    %% trace subset examples
    {done, Profile1} = tprof:profile(SpawnFun, #{pattern => [{lists, seq_loop, '_'}], report => return, type => call_memory}),
    ?assertMatch({call_memory, [{lists, seq_loop, 3, [{_, 9, 64}]}]}, Profile1),
    %% timer
    {{'EXIT', timeout}, Profile2} = tprof:profile(
        fun () -> Delay = hd(lists:seq(5001, 5032)), timer:sleep(Delay) end,
        #{timeout => 1000, report => return, type => call_memory}),
    ?assertMatch({call_memory, [{lists, seq_loop, 3, [{_, 9, 64}]}]}, Profile2).

lists_seq_loop(N) ->
    [int_to_bin_twice(M) || M <- lists:seq(1, N)].

int_to_bin_twice(M) ->
    B = integer_to_binary(M),
    <<B/binary, B/binary>>.

%% Ensure total is not truncated,
%% as per https://github.com/erlang/otp/issues/8139
call_memory_total(_Config) ->
    ct:capture_start(),
    ok = tprof:profile(?MODULE, lists_seq_loop, [10000], #{type => call_memory}),
    ct:capture_stop(),
    ?assertNotMatch(nomatch, re:run(ct:capture_get(), " 15000[01] ")),
    ok.

sort() ->
    [{doc, "Tests sorting methods work"}].

sort(Config) when is_list(Config) ->
    %% sort examples
    ct:capture_start(),
    ok = tprof:profile(
        fun () ->
            Group = lists:seq(100, 120),
            rand:uniform(hd(Group))
        end, #{report => {process, {measurement_per_call, descending}}, type => call_memory}),
    ct:capture_stop(),
    Out = string:lexemes(lists:flatten(ct:capture_get()), "\n"),
    %% measurement per call is 5th column
    Col = 5,
    Column5 = [string:to_integer(lists:nth(Col, Lexemes)) || Ln <- Out,
        length(Lexemes = string:lexemes(Ln, " ")) >= Col],
    WPC = [Words || {Words, []} <- Column5],
    %% ensure descending sort
    ?assertEqual(lists:reverse(lists:sort(WPC)), WPC).

rootset() ->
    [{doc, "Tests rootset of processes supplied to ad-hoc profiler"}].

rootset(Config) when is_list(Config) ->
    TestCase = random_name(?FUNCTION_NAME),
    {ok, Scope} = pg:start_link(TestCase),
    Fun = fun () -> ok = pg:join(TestCase, lists:seq(1, 2), self()) end,
    %% rootset tests
    {ok, Profile} = tprof:profile(Fun, #{rootset => [TestCase], report => return, type => call_memory}),
    TwoProcs = tprof:inspect(Profile),
    ?assertEqual(2, maps:size(TwoProcs)), %% must be pg and "profiled process"
    %% now trace all processes, existing and new
    {ok, ProfAll} = tprof:profile(Fun, #{rootset => all, report => return, type => call_memory}),
    %% at least tprof, pg2 and new profiled processes are traced
    ?assert(map_size(tprof:inspect(ProfAll)) >= 3),
    gen_server:stop(Scope).

set_on_spawn() ->
    [{doc, "Tests tprof running with extra spawned processes"}].

set_on_spawn(Config) when is_list(Config) ->
    %% profile a function that spawns additional process
    {done, Profile} = tprof:profile(
        fun () ->
            {Pid, MRef} = spawn_monitor(fun () -> lists:seq(1, 32) end),
            receive {'DOWN', MRef, process, Pid, normal} -> done end
        end, #{report => return, set_on_spawn => false, type => call_memory}),
    ct:log("~p",[Profile]),
    {done, ProfileMFA} = tprof:profile(?MODULE, seq, [32],
                                       #{report => return,
                                         set_on_spawn => false,
                                         type => call_memory}),

    ct:log("~p",[ProfileMFA]),
    %% check totals
    #{all := {call_memory, _G1, TotalProfile}} = tprof:inspect(Profile, total, measurement),
    #{all := {call_memory, _G2, TotalProfileMFA}} = tprof:inspect(ProfileMFA, total, measurement),
    %% only 1 process must be there
    ?assertEqual(1, maps:size(tprof:inspect(Profile)), {set_on_spawn, Profile}),

    DownMsgSz =
        case erlang:system_info(wordsize) of
            8 -> 9;
            4 -> 10
        end,
    DownMsgSzFl = DownMsgSz * 1.0,
    %% check per-process stats
    ?assertMatch({?MODULE, {_, 0}, 1, DownMsgSz, DownMsgSzFl, _}, lists:keyfind(?MODULE, 1, TotalProfile)),

    %% MFA takes 4 more words. The memory of the 'DOWN' message is a bit racy,
    %% so sometimes it will be there or not.
    MfaSz = 4,
    ?assertMatch({?MODULE, {seq, 1}, 1, Sz, SzAvg, _}
                 when Sz =:= (DownMsgSz + MfaSz) andalso SzAvg == (DownMsgSz + MfaSz);
                      Sz =:= MfaSz andalso SzAvg == MfaSz,
                      lists:keyfind(?MODULE, 1, TotalProfileMFA)).

seq(Max) ->
    {Pid, MRef} = spawn_monitor(fun () -> lists:seq(1, Max) end),
    receive {'DOWN', MRef, process, Pid, normal} -> done end.

live_trace() ->
    [{doc, "Tests memory tracing for pre-existing processes"}].

live_trace(Config) when is_list(Config) ->
    {ok, Srv} = start_link(Config, #{type => call_memory}),
    Pid = spawn_link(
        fun () ->
            receive
                {From, C} ->
                    [lists:seq(1, 2) || _ <- lists:seq(1, C)],
                    From ! {self(), done}
            end
        end),
    _ = set_pattern(Config, Srv, ?MODULE, '_', '_'),
    1 = enable_trace(Config, Srv, Pid),
    Pid ! {self(), 12},
    receive {Pid, done} -> ok end,
    catch disable_trace(Config, Srv, Pid),
    Profile = collect(Config, Srv),
    ProcInspected = tprof:inspect(Profile),
    %% white box check: list comprehension with 1-arity, and 100% allocation
    %% tprof:format(user, ProcInspected),
    #{Pid := {call_memory, 48, [{?MODULE, {_LC, 1}, 13, 48, _, 100.0}]}} = ProcInspected,
    stop(Config, Srv).

patterns() ->
    [{doc, "Tests pattern enable/disable correctness"}].

patterns(Config) when is_list(Config) ->
    {ok, Srv} = start_link(Config, #{type => call_memory}),
    %% test errors
    ?assertEqual({error, {not_traced, pg, get_members, '_'}}, clear_pattern(Config, Srv, pg, get_members, '_')),
    ?assertEqual({error, {trace_pattern, ?MODULE, seq, 2}}, set_pattern(Config, Srv, ?MODULE, seq, 2)),
    %% successful patterns
    1 = set_pattern(Config, Srv, ?MODULE, seq, 1),
    3 = set_pattern(Config, Srv, ?MODULE, pattern_fun, '_'),
    1 = clear_pattern(Config, Srv, ?MODULE, pattern_fun, 2),
    Expected = [{pattern_fun, 1}, {pattern_fun, 3}, {seq, 1}],
    ?assertEqual(#{?MODULE => Expected}, get_trace_map(Config, Srv)),
    %% verify tracing flags
    verify_trace(Srv, [{?MODULE, F, A} || {F, A} <- Expected], [{?MODULE, pattern_fun, 2}]),
    %% trace the entire ?MODULE module, and then exclude pattern_fun/1,2,3 and seq/1
    _ = set_pattern(Config, Srv, ?MODULE, '_', '_'),
    3 = clear_pattern(Config, Srv, ?MODULE, pattern_fun, '_'),
    1 = clear_pattern(Config, Srv, ?MODULE, seq, 1),
    Cleared = [{pattern_fun, 1}, {pattern_fun, 2}, {pattern_fun, 3}, {seq, 1}],
    Traced = ?MODULE:module_info(functions) -- Cleared,
    verify_trace(Srv, [{?MODULE, F, A} || {F, A} <- Traced], [{?MODULE, F, A} || {F, A} <- Cleared]),
    %% clear all, which clears lists too
    _ = clear_pattern(Config, Srv, '_', '_', '_'),
    verify_trace(Srv, [], [{?MODULE, F, A} || {F, A} <- Traced ++ Cleared]),
    ?assertEqual(#{}, get_trace_map(Config, Srv)),

    %% Trace the entire node then exclude pattern_fun/1,2,3 and seq/1
    _ = set_pattern(Config, Srv, '_', '_', '_'),
    ?assertEqual(all, get_trace_map(Config, Srv)),

    stop(Config, Srv).

verify_trace(Srv, On, Off) ->
    [?assertEqual({call_memory, []}, trace:info(tprof:get_session(Srv),MFA, call_memory)) || MFA <- On],
    [?assertEqual({call_memory, false}, trace:info(tprof:get_session(Srv),MFA, call_memory)) || MFA <- Off].

pattern_fun(Any) -> Any.
pattern_fun(Any, Any2) -> {Any, Any2}.
pattern_fun(Any, Any2, Any3) -> {Any, Any2, Any3}.

processes() ->
    [{doc, "Tests that process management for enabling/disabling traces works"}].

processes(Config) when is_list(Config) ->

    {ok, Srv} = start_link(Config),

    Pid = spawn_link(fun spawn_loop/0),
    Pid2 = spawn_link(fun spawn_loop/0),
    Name = random_name(?FUNCTION_NAME),
    register(Name, Pid2),
    %% test a mix of pids/registered processes/single PID calls
    ?assertEqual(2, enable_trace(Config, Srv, [Pid, Name])),
    ?assertEqual(0, disable_trace(Config, Srv, '$sure_not_exist')),
    ?assertEqual({1, ['$sure_not_exist']}, enable_trace(Config, Srv, [Pid, '$sure_not_exist'])),
    ok = gen:stop(Pid),
    ok = gen:stop(Name),

    stop(Config, Srv).

server() ->
    [{doc, "Tests for gen_server-based API"}].

server(Config) when is_list(Config) ->
    Name = random_name(?FUNCTION_NAME),

    %% start an extra pg scope - we'll trace it during profiling
    {ok, Scope} = pg:start_link(Name),
    %% simulate existing process
    Pid = spawn_link(fun spawn_loop/0),
    %% start the profiler
    {ok, _Srv} = tprof:start_link(#{type => call_memory}),
    %% test that ad-hoc profile can be run
    ?assertEqual(ok, tprof:profile(fun() -> lists:seq(1,10) end)),
    %% test live trace
    1 = tprof:set_pattern(?MODULE, dispatch, '_'),
    _ = tprof:set_pattern(pg, '_', '_'),
    %% watch for pg traces and for our process
    2 = tprof:enable_trace([Pid, Name]),
    %% run the traced operation
    _ = gen_server:call(Pid, {apply, pg, join, [Name, group, Pid]}),
    %% collect profile (can save it to a file for later analysis)
    {call_memory, FirstProfile} = tprof:collect(),
    %% must not be empty, and must contain 3-words dispatch from this module,
    %%  and at least something from pg in two processes
    ?assertNotEqual([], FirstProfile),
    ?assertEqual({?MODULE, dispatch, 2, [{Pid, 1, 3}]}, lists:keyfind(?MODULE, 1, FirstProfile)),
    ?assertMatch({pg, handle_call, 3, [{Scope, _, _}]}, lists:keyfind(handle_call, 2, FirstProfile)),
    ?assertMatch({pg, join, 3, [{Pid, _, _}]}, lists:keyfind(join, 2, FirstProfile)),
    %% pause tracing
    ok = tprof:pause(),
    %% ensure paused by running more code but keeping the trace
    %% ensure collection still returns the previous result
    _ = gen_server:call(Pid, {apply, pg, join, [Name, group, Pid]}),
    {call_memory, FirstProfile} = tprof:collect(),
    %% continue, ensure new results are collected
    ok = tprof:continue(),
    _ = gen_server:call(Pid, {apply, pg, leave, [Name, group, [Pid, Pid]]}),
    ?assertNotEqual({call_memory, FirstProfile}, tprof:collect()),
    %% restart all counters from zero and ensure that we again collect the original data
    ok = tprof:restart(),
    _ = gen_server:call(Pid, {apply, pg, join, [Name, group, Pid]}),
    {call_memory, FirstProfile} = tprof:collect(),

    %% test ad-hoc profiling can be done while running server-aided
    %% for that, profiler should have very specific pattern
    {_, AdHoc} = tprof:profile(lists, seq, [1, 32], #{registered => false, pattern => {lists, '_', '_'},
        report => return, type => call_memory}),
    %% check totals: must be 64 words allocated by a single lists:seq_loop
    ?assertMatch(#{all := {call_memory, 64, [{lists, _, _, 64, _, _}]}},
                 tprof:inspect(AdHoc, total, measurement)),
    %% verify that server-aided version still works
    {call_memory, FirstProfile} = tprof:collect(),
    ok = tprof:stop(),
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
    {ok, Srv} = start_link(Config, #{type => call_memory}),
    Traced = case enable_trace(Config, Srv, {all_children, kernel_sup}, #{ set_on_spawn => false }) of
                 {T, _} -> T;
                 T -> T
             end,
    ?assert(Traced > 5),
    ?assert(set_pattern(Config, Srv, code_server, '_', '_') > 5),
    _ = code:get_path(), %% makes a call to code_server
    %% disabling all processes tracing should return more than "children of"
    Disabled = disable_trace(Config, Srv, all),
    ct:log("~p",[{Disabled, Traced}]),
    ?assert(Disabled > Traced),
    {call_memory, Profile} = collect(Config, Srv),
    stop(Config, Srv),
    ?assertNotEqual(false, lists:keyfind(handle_call, 2, Profile)).

code_reload() ->
    [{doc, "Tests that collection does not fail for a hot-code-reloaded module"}].

code_reload(Config) when is_list(Config) ->
    {_, {call_memory, Sample}} = tprof:profile(
                                   fun () -> msacc:start(100), code:load_file(msacc) end,
                                   #{report => return, type => call_memory}),
    %% don't care about actual returned values, but do care that profile/2 does not crash
    ?assertNotEqual([], Sample).

code_load() ->
    [{doc, "Tests profiling works for modules loaded during profiling"}].

code_load(Config) when is_list(Config) ->
    code:purge(sofs),
    code:delete(sofs),
    {_, {call_memory, Sample}} =
        tprof:profile(fun () -> sofs:relation([{b,1},{c,2},{c,3}]) end,
                      #{report => return, type => call_memory}),
    ?assertNotEqual(false, lists:keyfind(sofs, 1, Sample)).

-define(SESSION(A, B),
        case proplists:get_value(session, Config) of
            undefined ->
                A;
            _Session ->
                B
        end).

start_link(Config) ->
    ?SESSION(
       tprof:start_link(),
       tprof:start_link(#{ session => _Session })).

start_link(Config, Opts) ->
    ?SESSION(
       tprof:start_link(Opts),
       tprof:start_link(Opts#{ session => _Session })).

set_pattern(Config, Srv, Mod, Fun, Arg) ->
    ?SESSION(
       tprof:set_pattern(Mod, Fun, Arg),
       tprof:set_pattern(Srv, Mod, Fun, Arg)
      ).

clear_pattern(Config, Srv, Mod, Fun, Arg) ->
    ?SESSION(
       tprof:clear_pattern(Mod, Fun, Arg),
       tprof:clear_pattern(Srv, Mod, Fun, Arg)
      ).

enable_trace(Config, Srv, Spec) ->
    ?SESSION(
       tprof:enable_trace(Spec),
       tprof:enable_trace(Srv, Spec, [])
      ).

enable_trace(Config, Srv, Spec, Opts) ->
    ?SESSION(
       tprof:enable_trace(Spec, Opts),
       tprof:enable_trace(Srv, Spec, Opts)
      ).

disable_trace(Config, Srv, Spec) ->
    ?SESSION(
       tprof:disable_trace(Spec),
       tprof:disable_trace(Srv, Spec, [])
      ).

collect(Config, Srv) ->
    ?SESSION(
       tprof:collect(),
       tprof:collect(Srv)
      ).

get_trace_map(Config, Srv) ->
    ?SESSION(
       tprof:get_trace_map(),
       tprof:get_trace_map(Srv)
      ).

stop(Config, Srv) ->
    ?SESSION(
       tprof:stop(),
       tprof:stop(Srv)
      ).

random_name(Prefix) ->
    list_to_atom(atom_to_list(Prefix) ++ "__" ++ integer_to_list(erlang:unique_integer())).
    
