%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2024. All Rights Reserved.
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
-module(trace_call_memory_SUITE).
-author("maximfca@gmail.com").

-include_lib("stdlib/include/assert.hrl").

%% Test server callbacks
-export([
    suite/0, all/0, groups/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2]).

%% Test cases exports
-export([
    basic/0, basic/1,
    on_load/0, on_load/1,
    late_trace/0, late_trace/1,
    skip/0, skip/1,
    message/0, message/1,
    parallel_map/0, parallel_map/1,
    trace_all/0, trace_all/1,
    spawn_memory/0, spawn_memory/1, spawn_memory_internal/1,
    spawn_memory_lambda/1,
    conflict_traces/0, conflict_traces/1,
    big_words/0, big_words/1
]).

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    trace_sessions:all().

groups() ->
    trace_sessions:groups(testcases()).

testcases() ->
    [basic, on_load, late_trace, skip, message, parallel_map, trace_all, spawn_memory,
    spawn_memory_lambda, conflict_traces, big_words].

init_per_suite(Config) ->
    trace_sessions:init_per_suite(Config).

end_per_suite(Config) ->
    trace_sessions:end_per_suite(Config).

init_per_group(Group, Config) ->
    trace_sessions:init_per_group(Group, Config).

end_per_group(Group, Config) ->
    trace_sessions:end_per_group(Group, Config).


init_per_testcase(_Case, Config) ->
    trace_sessions:init_per_testcase(Config).

end_per_testcase(basic, Config) ->
    erlang_trace_pattern({?MODULE, alloc_2tup, 0}, false, [call_memory]),
    erlang_trace(self(), false, [call]),
    trace_sessions:end_per_testcase(Config);
end_per_testcase(on_load, Config) ->
    erlang_trace_pattern({sofs, '_', '_'}, false, [call_memory]),
    erlang_trace_pattern(on_load, false, [call_memory]),
    erlang_trace(self(), false, [call]),
    trace_sessions:end_per_testcase(Config);
end_per_testcase(late_trace, Config) ->
    erlang_trace_pattern({?MODULE, '_', '_'}, false, [call_memory]),
    trace_sessions:end_per_testcase(Config);
end_per_testcase(skip, Config) ->
    erlang_trace_pattern({?MODULE, '_', '_'}, false, [call_memory]),
    erlang_trace(self(), false, [call]),
    trace_sessions:end_per_testcase(Config);
end_per_testcase(message, Config) ->
    erlang_trace_pattern({?MODULE, receive_message, 0}, false, [call_memory]),
    erlang_trace(all, false, [call]),
    trace_sessions:end_per_testcase(Config);
end_per_testcase(parallel_map, Config) ->
    erlang_trace_pattern({?MODULE, '_', '_'}, false, [call_memory]),
    erlang_trace(all, false, [call]),
    trace_sessions:end_per_testcase(Config);
end_per_testcase(trace_all, Config) ->
    erlang_trace_pattern({'_', '_', '_'}, false, [call_memory]),
    erlang_trace(all, false, [call]),
    trace_sessions:end_per_testcase(Config);
end_per_testcase(spawn_memory, Config) ->
    erlang_trace_pattern({?MODULE, spawn_memory_internal, '_'}, false, [call_memory]),
    erlang_trace(self(), false, [call]),
    trace_sessions:end_per_testcase(Config);
end_per_testcase(spawn_memory_lambda, Config) ->
    erlang_trace_pattern({erlang, apply, 2}, false, [call_memory]),
    erlang_trace(self(), false, [call]),
    trace_sessions:end_per_testcase(Config);
end_per_testcase(conflict_traces, Config) ->
    erlang_trace_pattern({?MODULE, '_', '_'}, false, [call_memory]),
    erlang_trace(self(), false, [call]),
    trace_sessions:end_per_testcase(Config);
end_per_testcase(big_words, Config) ->
    erlang_trace_pattern({?MODULE,alloc_tuples,2}, false, [call_memory]),
    erlang_trace(self(), false, [call]),
    trace_sessions:end_per_testcase(Config).


erlang_trace(A,B,C) ->
    trace_sessions:erlang_trace(A,B,C).

erlang_trace_pattern(A,B,C) ->
    trace_sessions:erlang_trace_pattern(A,B,C).

erlang_trace_info(A,B) ->
    trace_sessions:erlang_trace_info(A,B).


%% allocation functions
alloc_2tup() ->
    {self(),self()}. %% a non literal

receive_message() ->
    receive
        Msg -> Msg
    end.

basic() ->
    [{doc, "Assert that two-tuple allocated 3-word structure on the heap"}].

basic(Config) when is_list(Config) ->
    Self = self(),
    Traced = {?MODULE, alloc_2tup, 0},
    1 = erlang_trace_pattern(Traced, true, [call_memory]),
    1 = erlang_trace(Self, true, [call]),
    alloc_2tup(),
    {call_memory, [{Self, 1, 3}]} = erlang_trace_info(Traced, call_memory),
    alloc_2tup(),
    {call_memory, [{Self, 2, 6}]} = erlang_trace_info(Traced, call_memory),
    %% test that GC works correctly
    erlang:garbage_collect(),
    alloc_2tup(),
    {call_memory, [{Self, 3, 9}]} = erlang_trace_info(Traced, call_memory),
    1 = erlang_trace(Self, false, [call]),
    1 = erlang_trace_pattern(Traced, false, [call_memory]).

on_load() ->
    [{doc, "Test that on_load works"}].

on_load(Config) when is_list(Config) ->

    code:purge(sofs),
    code:delete(sofs),
    false = erlang:module_loaded(sofs),

    SumAll =
        fun(Module) ->
                FAs = [{F, A, erlang_trace_info({Module, F, A}, call_memory)}
                       || {F, A} <- Module:module_info(functions),
                          F =/= module_info],
                lists:sum(
                  lists:flatten(
                    [ [M || {_Pid, _Cnt, M} <- Pids]
                      || {_, _, {call_memory, Pids}} <- FAs]))
        end,

    Self = self(),

    %% Check that sofs is not traced
    0 = erlang_trace_pattern({sofs,'_','_'}, false, [call_memory]),

    0 = erlang_trace_pattern(on_load, true, [call_memory]),
    1 = erlang_trace(Self, true, [call]),

    %% Check that before call, sofs does not have any allocations
    0 = SumAll(sofs),
    %% Call it to allocate some memory
    sofs:relation([{b,1},{c,2},{c,3}]),

    %% Check that some memory was allocated
    ?assertNotEqual(0, SumAll(sofs)),

    1 = erlang_trace(Self, false, [call]),

    %% Verify that some functions had traces on them
    ?assertNotEqual(0, erlang_trace_pattern({sofs,'_','_'}, false, [call_memory])).

late_trace() ->
    [{doc, "Tests that garbage_collect call done before tracing is enabled works as expected"}].

late_trace(Config) when is_list(Config) ->
    Control = self(),
    Pid = spawn_link(
        fun () ->
            _ = late_trace_inner(),
            1 = erlang_trace_pattern({?MODULE, late_trace_inner, 0}, true, [call_memory]),
            1 = erlang_trace(self(), true, [call]),
            _ = late_trace_inner(),
            Control ! continue,
            receive
                stop ->
                    1 = erlang_trace_pattern({?MODULE, late_trace_inner, 0}, false, [call_memory])
            end
        end),
    NonLiteral = non_literal_9(),
    MRef = monitor(process, Pid),
    Pid ! NonLiteral,
    Pid ! NonLiteral,
    receive continue -> ok end,
    {call_memory, [{Pid, 1, 12}]} = erlang_trace_info({?MODULE, late_trace_inner, 0}, call_memory),
    Pid ! stop,
    receive {'DOWN', MRef, process, Pid, _} -> ok end.

late_trace_inner() ->
    Ref = alloc_2tup(), %% 3 words
    receive _Msg -> Ref end. %% 9 more words

non_literal_9() ->
    %% 9 words in total: 6 words for the list, 3 for the tuple
    erlang:insert_element(2, erlang:insert_element(1, {}, atom), lists:seq(1, 3)).

skip() ->
    [{doc, "Tests that skipped trace for a function accumulates in an upper level caller"}].

skip(Config) when is_list(Config) ->
    Self = self(),
    1 = erlang_trace_pattern({?MODULE, upper, 0}, true, [call_memory]),
    1 = erlang_trace_pattern({?MODULE, lower, 1}, true, [call_memory]),
    1 = erlang_trace(Self, true, [call]),
    upper(),
    {call_memory, [{Self, 1, 8 * 2}]} = erlang_trace_info({?MODULE, lower, 1}, call_memory),
    {call_memory, [{Self, 1, 3 + 3 + 6}]} = erlang_trace_info({?MODULE, upper, 0}, call_memory).

upper() ->
    Ref = alloc_2tup(),          %% 3
    X = middle(Ref),            %% 3 in middle, and 8 in lower (but lower has its own accounting)
    {1, 2, 3, X, Ref}.          %% extra 6 words (5 elements and 1 tuple size)

middle(_Ref) ->
    Ref2 = alloc_2tup(),         %% 3 - but accounted in 'upper/0' instead
    lower(8),
    Ref2.

lower(Max) ->
    lists:seq(1, Max).

message() ->
    [{doc, "Assert that receiving a message results in memory trace"}].

message(Config) when is_list(Config) ->
    Control = self(),
    Traced = {?MODULE, receive_message, 0},
    NonLiteral = non_literal_9(),
    1 = erlang_trace_pattern(Traced, true, [call_memory]),
    1 = erlang_trace(self(), true, [call, set_on_first_spawn]),
    Pid = spawn_link(
        fun Wrap() ->
            receive
                pre ->
                    receive_message(),
                    Control ! done,
                    Wrap();
                stop ->
                    ok
            end
        end),
    1 = erlang_trace(self(), false, [call, set_on_first_spawn]),
    %% first, check that sending a (non-matched) message does not result in a negative allocation
    Pid ! NonLiteral,
    timer:sleep(500),
    {call_memory, []} = erlang_trace_info(Traced, call_memory),
    %% enter the receive_message/0
    Pid ! pre,
    %% wait for 'done' response
    receive done -> ok end,
    {call_memory, [{Pid, 1, 9}]} = erlang_trace_info(Traced, call_memory),
    %% once again, just in case, to verify that decrementing "allocated" worked
    Pid ! pre,
    Pid ! NonLiteral,
    receive done -> ok end,
    {call_memory, [{Pid, 2, 18}]} = erlang_trace_info(Traced, call_memory),
    1 = erlang_trace_pattern(Traced, false, [call_memory]).

parallel_map() ->
    [{doc, "Test memory profiling with spawned processes"}].

parallel_map(Config) when is_list(Config) ->
    _ = erlang_trace_pattern({?MODULE, '_', '_'}, true, [call_memory]),
    1 = erlang_trace(self(), true, [call, set_on_spawn]),
    Pid = spawn_link(fun do_parallel/0),
    MRef = monitor(process, Pid),
    Pid ! pre_stop,
    Pid ! {stop, 1},
    receive
        {'DOWN', MRef, process, Pid, _} ->
            %% alloc_2tup called 3 times (once per process)
            {call_memory, [{_, 1, 3}, {_, 1, 3}, {_, 1, 3}]} =
                erlang_trace_info({?MODULE, alloc_2tup, 0}, call_memory),
            %% receive_message called 8 times (3 from "Allocs", 3 from "Grand", and 2 from the runner)
            %%  from 7 processes, but only 3*6 Grand processes and 3 words for the runner are on the heap
            {call_memory, RecvMsg} = erlang_trace_info({?MODULE, receive_message, 0}, call_memory),
            {7, 8, 21} = collapse_procs(RecvMsg)
    end.

do_parallel() ->
    Allocs = [spawn_link(fun() -> alloc_2tup(), receive_message() end) || _ <- lists:seq(1, 3)],
    Grand = [spawn_link(fun() -> receive_message() end) || _ <- lists:seq(1, 3)],
    pre_stop = receive_message(),
    [P ! {atom, <<"1234">>} || P <- Grand], %% 6 words on the heap: 3 for binary, 3 for tuple
    {stop, 1} = receive_message(),
    [exit(P, normal) || P <- Allocs].

trace_all() ->
    [{doc, "Enables memory tracing for all processes, mainly ensuring there are no core dumps"}].

trace_all(Config) when is_list(Config) ->
    _ = erlang_trace_pattern({'_', '_', '_'}, true, [call_memory]),
    _ = erlang_trace(all, true, [call]),
    do_heavy_lifting(),
    _ = erlang_trace(all, false, [call]),
    Profile = profile_memory(),
    %% it'd be better to introduce more checks, but for now see that
    %%  at least some action happened
    true = is_map_key(application_controller, Profile).

do_heavy_lifting() ->
    {ok, []} = application:ensure_all_started(kernel).

profile_memory() ->
    profile_modules(code:all_loaded(), #{}).

profile_modules([], Acc) ->
    Acc;
profile_modules([{Module, _} | Tail], Acc) ->
    Funcs = try Module:module_info(functions)
            catch error:undef -> [] % ignore homebrewed without module_info
            end,
    case profile_functions(Module, Funcs, #{}) of
        Empty when Empty =:= #{} ->
            profile_modules(Tail, Acc);
        NonEmpty ->
            profile_modules(Tail, Acc#{Module => NonEmpty})
    end.

profile_functions(_Module, [], Acc) ->
    Acc;
profile_functions(Module, [{Fun, Arity} | Tail], Acc) ->
    case erlang_trace_info({Module, Fun, Arity}, call_memory) of
        {call_memory, Skip} when Skip =:= []; Skip =:= false ->
            profile_functions(Module, Tail, Acc);
        {call_memory, Mem} ->
            profile_functions(Module, Tail, Acc#{{Fun, Arity} => collapse_procs(Mem)})
    end.

collapse_procs(Processes) ->
    lists:foldl(
        fun ({_Pid, Calls, Words}, {Procs, TotCalls, TotWords}) ->
            {Procs + 1, TotCalls + Calls, TotWords + Words}
        end, {0, 0, 0}, Processes).

spawn_memory() ->
    [{doc, "Tests that when a process is spawned, initial memory correctly attributed to the initial call"}].

spawn_memory(Config) when is_list(Config) ->
    LostSharing = lists:seq(1, 16),
    _ = erlang_trace_pattern({?MODULE, spawn_memory_internal, 1}, true, [call_memory]),
    1 = erlang_trace(self(), true, [call, set_on_first_spawn]),
    Pid = erlang:spawn_link(?MODULE, spawn_memory_internal, [LostSharing]),
    MRef = monitor(process, Pid),
    receive {'DOWN', MRef, process, Pid, _} -> ok end,
    1 = erlang_trace(self(), false, [all]),
    %% 16-elements list translates into 34-words for spawn
    {call_memory, [{Pid, 1, 34}]} = erlang_trace_info({?MODULE, spawn_memory_internal, 1}, call_memory).

spawn_memory_lambda(Config) when is_list(Config) ->
    %% check that tracing with context captured through lambda also works - but reports erlang:apply
    LostSharing = lists:seq(1, 16),
    _ = erlang_trace_pattern({erlang, apply, 2}, true, [call_memory]),
    1 = erlang_trace(self(), true, [call, set_on_first_spawn]),
    Pid = erlang:spawn_link(fun () -> spawn_memory_internal(LostSharing) end),
    MRef = monitor(process, Pid),
    receive {'DOWN', MRef, process, Pid, _} -> ok end,
    1 = erlang_trace(self(), false, [all]),
    %% 16-elements list translates into 34-words for spawn, and 4 more words for apply itself
    {call_memory, [{Pid, 1, 38}]} = erlang_trace_info({erlang, apply, 2}, call_memory).

spawn_memory_internal(Array) ->
    Array.

conflict_traces() ->
    [{doc, "Verify that call_time and call_memory don't break each other"}].

conflict_traces(Config) when is_list(Config) ->
    Self = self(),
    Traced = {?MODULE, alloc_2tup, 0},
    %% start call_memory trace
    1 = erlang_trace_pattern(Traced, true, [call_memory]),
    1 = erlang_trace(Self, true, [call]),
    alloc_2tup(),
    {call_memory, [{Self, 1, 3}]} = erlang_trace_info(Traced, call_memory),
    %% start/stop call_time trace
    1 = erlang_trace_pattern(Traced, true, [call_time]),
    alloc_2tup(), %% this goes to both time and memory
    {call_time, [{Self, 1, _, _}]} = erlang_trace_info(Traced, call_time),
    1 = erlang_trace_pattern(Traced, false, [call_time]),
    %% memory is unaffected
    alloc_2tup(),
    {call_memory, [{Self, 3, 9}]} = erlang_trace_info(Traced, call_memory),
    %%
    1 = erlang_trace(Self, false, [call]),
    1 = erlang_trace_pattern(Traced, false, [call_memory]).

big_words() ->
    [{doc, "Test that Words counter can be a bignum on 32-bit"}].

big_words(Config) when is_list(Config) ->
    Self = self(),
    Traced = {?MODULE, alloc_tuples, 2},
    1 = erlang_trace_pattern(Traced, true, [call_memory]),
    1 = erlang_trace(Self, true, [call]),
    Words = (1 bsl 27),
    case {erts_debug:size(Words), 8*erlang:system_info(wordsize)} of
        {2, 32} -> ok;
        {0, 64} -> ok
    end,
    TupleSz = 1023,
    TupleCnt = Words div (TupleSz + 1),
    alloc_tuples(TupleCnt, TupleSz),
    CallCnt = TupleCnt + 1,
    {call_memory, [{Self, CallCnt, Words}]} = erlang_trace_info(Traced, call_memory),
    1 = erlang_trace(Self, false, [call]),
    1 = erlang_trace_pattern(Traced, false, [call_memory]).


alloc_tuples(0, _) ->
    ok;
alloc_tuples(N, TupleSz) ->
    erlang:make_tuple(TupleSz, []),
    alloc_tuples(N-1, TupleSz).
