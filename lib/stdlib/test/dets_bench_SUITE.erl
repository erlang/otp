%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(dets_bench_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include("dets.hrl").

-export([suite/0, all/0, groups/0, init_per_group/2, end_per_group/2,
         lookup/1, overwrite/1, fold/1]).

-define(ROWS, 1024).
-define(SLOTS, 2048).
-define(SAMPLES, 5).
-define(TARGET_MICROSECONDS, 100000).

suite() ->
    [{ct_hooks, [ts_install_cth]}, {timetrap, {minutes, 15}}].

all() ->
    [{group, small}, {group, large}].

groups() ->
    [{small, [], [lookup, overwrite, fold]},
     {large, [], [lookup, overwrite, fold]}].

init_per_group(small, Config) -> [{payload_bytes, 64} | Config];
init_per_group(large, Config) -> [{payload_bytes, 4096} | Config].

end_per_group(_Group, _Config) -> ok.

lookup(Config) -> run(lookup, Config).
overwrite(Config) -> run(overwrite, Config).
fold(Config) -> run(fold, Config).

run(Operation, Config) ->
    Bytes = proplists:get_value(payload_bytes, Config),
    A = binary:copy(<<0>>, Bytes),
    B = binary:copy(<<1>>, Bytes),
    Objects = {[{K, A} || K <- lists:seq(1, ?ROWS)],
               [{K, B} || K <- lists:seq(1, ?ROWS)]},
    Layouts = [lower, extended_lower, upper_direct, upper_reference],
    with_tables(Layouts, [{operation, Operation} | Config], Objects,
      fun(Tables) ->
              lists:foreach(fun({_Layout, Tab, _Positions}) ->
                                    warm(Operation, Tab, Objects)
                            end, Tables),
              {lower, Lower, _} = lists:keyfind(lower, 1, Tables),
              {Elapsed, ok} = timer:tc(fun() -> work(Operation, Lower, Objects, 1) end),
              Passes = min(1000, max(1, ?TARGET_MICROSECONDS div max(1, Elapsed))),
              Times = sample(?SAMPLES, Operation, Tables, Objects, Passes, #{}),
              report(Operation, Bytes, Passes, Times)
      end).

with_tables(Layouts, Config, Objects, Fun) ->
    with_tables(Layouts, Config, Objects, Fun, []).

with_tables([Layout | Layouts], Config, {A, _} = Objects, Fun, Tables) ->
    Operation = proplists:get_value(operation, Config),
    Bytes = proplists:get_value(payload_bytes, Config),
    Name = atom_to_list(Operation) ++ "_" ++ integer_to_list(Bytes) ++
        "_" ++ atom_to_list(Layout),
    File = filename:join(proplists:get_value(priv_dir, Config), Name),
    Tab = {?MODULE, Operation, Bytes, Layout},
    {ok, Tab} = dets:open_file(Tab, [{file, File}, {type, set},
                                   {min_no_slots, ?SLOTS},
                                   {max_no_slots, ?SLOTS},
                                   {auto_save, infinity},
                                   {delayed_write, {0, 0}}]),
    try
        seed(Layout, Tab, A),
        Positions = positions(Layout, Tab, A),
        verify(Layout, Tab, A, Positions),
        with_tables(Layouts, Config, Objects, Fun,
                    [{Layout, Tab, Positions} | Tables])
    after
        try dets:close(Tab) catch _:_ -> ok end,
        file:delete(File)
    end;
with_tables([], _Config, _Objects, Fun, Tables) ->
    Fun(lists:reverse(Tables)).

seed(Layout, Tab, Objects) ->
    ok = dets:safe_fixtable(Tab, true),
    Worker = dets:info(Tab, owner),
    Saved = (sys:get_state(Worker))#head.freelists,
    try
        #head{} = sys:replace_state(Worker, fun(H) -> prepare(Layout, H) end),
        ok = dets:insert(Tab, Objects),
        ok = dets:sync(Tab)
    after
        case Layout of
            upper_direct -> restore_lower(Worker, Saved);
            upper_reference -> restore_lower(Worker, Saved);
            _ -> ok
        end,
        dets:safe_fixtable(Tab, false)
    end.

prepare(lower, H) -> H;
prepare(extended_lower, H) -> dets_addr:init_wide(H);
prepare(Layout, H) ->
    H1 = dets_addr:init_wide(H),
    {_Frozen, Current} = H1#head.freelists,
    H2 = H1#head{freelists = {dets_utils:empty_free_lists(), Current}},
    case Layout of
        upper_direct -> H2;
        upper_reference ->
            Wide = H2#head.wide,
            {_UpperFrozen, UpperCurrent} = Wide#dets64.freelists,
            Empty = dets_utils:empty_free_lists(Wide#dets64.levels),
            H2#head{wide = Wide#dets64{freelists = {Empty, UpperCurrent}}}
    end.

restore_lower(Worker, Saved) ->
    #head{} = sys:replace_state(Worker, fun(H) -> H#head{freelists = Saved} end),
    ok.

positions(Layout, Tab, Objects) ->
    [begin
         {ok, P} = dets:where(Tab, Object),
         true = in_region(Layout, P),
         P
     end || Object <- Objects].

in_region(lower, P) -> P < ?DETS64_BASE;
in_region(extended_lower, P) -> P < ?DETS64_BASE;
in_region(upper_direct, P) -> P >= ?DETS64_BASE andalso P < ?DETS64_REF_BASE;
in_region(upper_reference, P) -> P >= ?DETS64_REF_END.

verify(Layout, Tab, Objects, Positions) ->
    ?ROWS = dets:info(Tab, size),
    {?SLOTS, ?SLOTS, ?SLOTS} = dets:info(Tab, no_slots),
    Objects = lists:sort(dets:foldl(fun(O, Acc) -> [O | Acc] end, [], Tab)),
    Positions = positions(Layout, Tab, Objects),
    H = sys:get_state(dets:info(Tab, owner)),
    case Layout of
        lower -> undefined = H#head.wide;
        _ -> #dets64{} = H#head.wide
    end,
    ok.

warm(overwrite, Tab, {A, B}) ->
    write(Tab, B),
    write(Tab, A);
warm(Operation, Tab, Objects) ->
    work(Operation, Tab, Objects, 1).

work(_Operation, _Tab, _Objects, 0) -> ok;
work(lookup, Tab, {A, _} = Objects, Passes) ->
    lists:foreach(fun({K, _}) -> [_] = dets:lookup(Tab, K) end, A),
    work(lookup, Tab, Objects, Passes-1);
work(overwrite, Tab, {A, B} = Objects, Passes) ->
    write(Tab, B),
    write(Tab, A),
    work(overwrite, Tab, Objects, Passes-1);
work(fold, Tab, Objects, Passes) ->
    ?ROWS = dets:foldl(fun(_Object, Count) -> Count+1 end, 0, Tab),
    work(fold, Tab, Objects, Passes-1).

write(Tab, Objects) ->
    lists:foreach(fun(Object) -> ok = dets:insert(Tab, Object) end, Objects).

sample(0, _Operation, _Tables, _Objects, _Passes, Times) -> Times;
sample(N, Operation, Tables, {A, _} = Objects, Passes, Times) ->
    Order = case N rem 2 of 0 -> lists:reverse(Tables); 1 -> Tables end,
    Next = lists:foldl(
      fun({Layout, Tab, Positions}, Acc) ->
              {Elapsed, ok} = timer:tc(fun() -> work(Operation, Tab, Objects, Passes) end),
              true = Elapsed > 0,
              verify(Layout, Tab, A, Positions),
              Acc#{Layout => [Elapsed | maps:get(Layout, Acc, [])]}
      end, Times, Order),
    sample(N-1, Operation, Tables, Objects, Passes, Next).

report(Operation, Bytes, Passes, Times) ->
    Lower = median(maps:get(lower, Times)),
    Count = case Operation of overwrite -> 2*?ROWS*Passes; _ -> ?ROWS*Passes end,
    Prefix = atom_to_list(Operation) ++ "_" ++ integer_to_list(Bytes) ++ "_",
    ct:pal("Warm-cache ~p, ~p-byte values; fixed lower indexes and sparse upper files; "
           "writes exclude synchronization.",
           [Operation, Bytes]),
    lists:foreach(
      fun(Layout) ->
              Elapsed = median(maps:get(Layout, Times)),
              Rate = Count*1000000/Elapsed,
              Ratio = Elapsed/Lower,
              Overhead = 100*(Ratio-1),
              Name = Prefix ++ atom_to_list(Layout),
              metric(Name ++ "_rows_per_second", Rate),
              metric(Name ++ "_elapsed_ratio", Ratio),
              metric(Name ++ "_overhead_percent", Overhead),
              ct:pal("~p: ~p rows/s, ~.3fx lower elapsed time, ~.1f% overhead",
                     [Layout, round(Rate), Ratio, Overhead])
      end, [lower, extended_lower, upper_direct, upper_reference]),
    ok.

median(Values) ->
    lists:nth((length(Values)+1) div 2, lists:sort(Values)).

metric(Name, Value) ->
    ct_event:notify(#event{name = benchmark_data,
                         data = [{suite, "stdlib_dets"}, {name, Name},
                                 {value, Value}]}).
