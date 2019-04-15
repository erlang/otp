%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2018. All Rights Reserved.
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
-module(instrument_SUITE).

-export([all/0, suite/0]).

-export([allocations_enabled/1, allocations_disabled/1, allocations_ramv/1,
         carriers_enabled/1, carriers_disabled/1]).

-export([test_all_alloc/2, test_per_alloc/2, test_format/3, test_abort/1,
         generate_test_blocks/0, churn_memory/0]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() -> 
    [allocations_enabled, allocations_disabled, allocations_ramv,
     carriers_enabled, carriers_disabled].

-define(GENERATED_SBC_BLOCK_COUNT, 1000).
-define(GENERATED_MBC_BLOCK_COUNT, ?GENERATED_SBC_BLOCK_COUNT).

-define(GENERATED_BLOCK_COUNT, (?GENERATED_SBC_BLOCK_COUNT +
                                ?GENERATED_MBC_BLOCK_COUNT)).
-define(GENERATED_CARRIER_COUNT, ?GENERATED_SBC_BLOCK_COUNT).

allocations_test(Args, Plain, PerAlloc) ->
    run_test(Args, fun(Node) ->
                 ok = rpc:call(Node, ?MODULE, test_all_alloc,
                               [fun instrument:allocations/0, Plain]),
                 ok = rpc:call(Node, ?MODULE, test_per_alloc,
                               [fun instrument:allocations/1, PerAlloc]),
                 ok = rpc:call(Node, ?MODULE, test_format,
                               [#{ histogram_start => 512,
                                   histogram_width => 4 },
                                fun instrument:allocations/1,
                                fun verify_allocations_output/2]),
                 ok = rpc:call(Node, ?MODULE, test_abort,
                               [fun erts_internal:gather_alloc_histograms/1])
             end).

allocations_enabled(Config) when is_list(Config) ->
    allocations_test("+Meamax +Muatags true",
                     fun verify_allocations_enabled/1,
                     fun verify_allocations_enabled/2).

allocations_disabled(Config) when is_list(Config) ->
    allocations_test("+Meamax +Muatags false",
                     fun verify_allocations_disabled/1,
                     fun verify_allocations_disabled/2).

allocations_ramv(Config) when is_list(Config) ->
    allocations_test("+Meamax +Muatags true +Muramv true",
                     fun verify_allocations_enabled/1,
                     fun verify_allocations_enabled/2).

verify_allocations_disabled(_AllocType, Result) ->
    verify_allocations_disabled(Result).

verify_allocations_disabled({ok, {_HistStart, _UnscannedBytes, Allocs}}) ->
    true = Allocs =:= #{};
verify_allocations_disabled({error, not_enabled}) ->
    ok.

%% Skip types that have unstable results or are unaffected by +Muatags
verify_allocations_enabled(literal_alloc, _Result) -> ok;
verify_allocations_enabled(exec_alloc, _Result) -> ok;
verify_allocations_enabled(temp_alloc, _Result) -> ok;
verify_allocations_enabled(sl_alloc, _Result) -> ok;
verify_allocations_enabled(_AllocType, Result) ->
    verify_allocations_enabled(Result).

verify_allocations_enabled({ok, {_HistStart, _UnscannedBytes, Allocs}}) ->
    true = Allocs =/= #{}.

verify_allocations_output(#{}, {ok, {_, _, Allocs}}) when Allocs =:= #{} ->
    %% This happens when the allocator is enabled but tagging is disabled. If
    %% there's an error that causes Allocs to always be empty when enabled it
    %% will be caught by verify_allocations_enabled.
    ok;
verify_allocations_output(#{}, {error, not_enabled}) ->
    ok;
verify_allocations_output(#{ histogram_start := HistStart,
                             histogram_width := HistWidth },
                    {ok, {HistStart, _UnscannedBytes, ByOrigin}}) ->
    AllHistograms = lists:flatten([maps:values(ByType) ||
                                   ByType <- maps:values(ByOrigin)]),

    %% Do the histograms look alright?
    HistogramSet = ordsets:from_list(AllHistograms),
    Verified = [H || H <- HistogramSet,
                tuple_size(H) =:= HistWidth,
                hist_sum(H) >= 1],
    [] = ordsets:subtract(HistogramSet, Verified),

    %% Do we have at least as many blocks as we've generated?
    BlockCount = lists:foldl(fun(Hist, Acc) ->
                                 hist_sum(Hist) + Acc
                             end, 0, AllHistograms),
    GenTotalBlockCount = ?GENERATED_BLOCK_COUNT,
    GenSBCBlockCount = ?GENERATED_SBC_BLOCK_COUNT,
    if
        BlockCount < GenSBCBlockCount ->
            ct:fail("Found ~p blocks, required at least ~p (SB)." ,
                    [BlockCount, GenSBCBlockCount]);
        BlockCount >= GenTotalBlockCount ->
            ct:pal("Found ~p blocks, expected at least ~p (SB + MB).",
                    [BlockCount, GenTotalBlockCount]);
        BlockCount < GenTotalBlockCount ->
            ct:pal("Found ~p blocks, expected at least ~p (SB + MB), but this "
                    "may be due to MBCs being skipped if they're about to be "
                    "scanned just as they're fetched from the carrier pool.",
                    [BlockCount, GenTotalBlockCount])
    end,

    ok.

%% %% %% %% %% %%

carriers_test(Args, Plain, PerAlloc) ->
    run_test(Args, fun(Node) ->
                 ok = rpc:call(Node, ?MODULE, test_all_alloc,
                               [fun instrument:carriers/0, Plain]),
                 ok = rpc:call(Node, ?MODULE, test_per_alloc,
                               [fun instrument:carriers/1, PerAlloc]),
                 ok = rpc:call(Node, ?MODULE, test_format,
                               [#{ histogram_start => 1024,
                                   histogram_width => 4 },
                                fun instrument:carriers/1,
                                fun verify_carriers_output/2]),
                 ok = rpc:call(Node, ?MODULE, test_abort,
                               [fun erts_internal:gather_carrier_info/1])
             end).

carriers_enabled(Config) when is_list(Config) ->
    carriers_test("+Meamax",
                  fun verify_carriers_enabled/1,
                  fun verify_carriers_enabled/2).

carriers_disabled(Config) when is_list(Config) ->
    carriers_test("+Meamin",
                  fun verify_carriers_disabled/1,
                  fun verify_carriers_disabled/2).

verify_carriers_disabled(_AllocType, Result) ->
    verify_carriers_disabled(Result).

verify_carriers_disabled({error, not_enabled}) ->
    ok;
verify_carriers_disabled({ok, {_HistStart, Carriers}}) ->
    verify_carriers_disabled_1(Carriers).

verify_carriers_disabled_1([]) ->
    ok;
%% literal_alloc, exec_alloc, and temp_alloc can't be disabled, so we have to
%% accept their presence in carriers_disabled/test_all_alloc.
verify_carriers_disabled_1([Carrier | Rest]) when
        element(1, Carrier) =:= literal_alloc;
        element(1, Carrier) =:= exec_alloc;
        element(1, Carrier) =:= temp_alloc ->
    verify_carriers_disabled_1(Rest).

%% exec_alloc only has a carrier if it's actually used.
verify_carriers_enabled(exec_alloc, _Result) -> ok;
verify_carriers_enabled(_AllocType, Result) -> verify_carriers_enabled(Result).

verify_carriers_enabled({ok, {_HistStart, Carriers}}) when Carriers =/= [] ->
    ok.

verify_carriers_output(#{ histogram_start := HistStart,
                          histogram_width := HistWidth },
                       {ok, {HistStart, AllCarriers}}) ->

    %% Do the carriers look alright?
    CarrierSet = ordsets:from_list(AllCarriers),
    Verified = [C || {AllocType,
                      TotalSize,
                      UnscannedSize,
                      AllocatedSize,
                      AllocatedCount,
                      InPool,
                      FreeBlockHist} = C <- CarrierSet,
                is_atom(AllocType),
                is_integer(TotalSize), TotalSize >= 1,
                is_integer(UnscannedSize), UnscannedSize < TotalSize,
                                           UnscannedSize >= 0,
                is_integer(AllocatedSize), AllocatedSize < TotalSize,
                                           AllocatedSize >= 0,
                is_integer(AllocatedCount), AllocatedCount =< AllocatedSize,
                                            AllocatedCount >= 0,
                is_boolean(InPool),
                tuple_size(FreeBlockHist) =:= HistWidth,
                carrier_block_check(AllocatedCount, FreeBlockHist)],
    [] = ordsets:subtract(CarrierSet, Verified),

    %% Do we have at least as many carriers as we've generated?
    CarrierCount = length(AllCarriers),
    GenSBCCount = ?GENERATED_SBC_BLOCK_COUNT,
    if
        CarrierCount < GenSBCCount ->
            ct:fail("Carrier count is ~p, expected at least ~p (SBC).",
                [CarrierCount, GenSBCCount]);
        CarrierCount >= GenSBCCount ->
            ct:pal("Found ~p carriers, required at least ~p (SBC)." ,
                [CarrierCount, GenSBCCount])
    end,

    ok;
verify_carriers_output(#{}, {error, not_enabled}) ->
    ok.

carrier_block_check(AllocCount, FreeHist) ->
    %% A carrier must contain at least one block, and th. number of free blocks
    %% must not exceed the number of allocated blocks + 1.
    FreeCount = hist_sum(FreeHist),

    (AllocCount + FreeCount) >= 1 andalso FreeCount =< (AllocCount + 1).

%% %% %% %% %% %%

test_all_alloc(Gather, Verify) ->
    Verify(Gather()),
    ok.

test_per_alloc(Gather, Verify) ->
    [begin
         Verify(T, Gather(#{ allocator_types => [T] }))
     end || T <- erlang:system_info(alloc_util_allocators)],
    ok.

test_format(#{ allocator_types := _ }, _, _) ->
    error(badarg);
test_format(Options0, Gather, Verify) ->
    %% We limit format checking to binary_alloc since we generated the test
    %% vectors there.
    Options = Options0#{ allocator_types => [binary_alloc] },
    Verify(Options, Gather(Options)),
    ok.

test_abort(Gather) ->
    %% There's no way for us to tell whether this actually aborted or ran to
    %% completion, but it might catch a few segfaults.
    %% This testcase is mostly useful when run in an debug emulator as it needs
    %% the modified reduction count to trigger the odd trap scenarios
    Runner = self(),
    Ref = make_ref(),
    spawn_opt(fun() ->
                      [begin
                           Ref2 = make_ref(),
                           [Gather({Type, SchedId, 1, 1, Ref2}) ||
                               Type <- erlang:system_info(alloc_util_allocators),
                               SchedId <- lists:seq(0, erlang:system_info(schedulers))]
                       end || _ <- lists:seq(1,100)],
                      Runner ! Ref
              end, [{priority, max}]),
    receive
        Ref -> ok
    end.

hist_sum(H) -> hist_sum_1(H, tuple_size(H), 0).
hist_sum_1(_H, 0, A) -> A;
hist_sum_1(H, N, A) -> hist_sum_1(H, N - 1, element(N, H) + A).

%%

run_test(Args0, Test) ->
    %% Override single-block carrier threshold for binaries to ensure we have
    %% coverage for that path. generate_test_blocks builds a few binaries that
    %% crosses this threshold.
    %%
    %% We also set the abandon carrier threshold to 70% to provoke more
    %% activity in the carrier pool.
    Args = Args0 ++ " +MBsbct 1 +Muacul 70",
    Node = start_slave(Args),

    ok = rpc:call(Node, ?MODULE, generate_test_blocks, []),
    ok = Test(Node),

    ok = rpc:call(Node, ?MODULE, churn_memory, []),
    ok = Test(Node),

    true = test_server:stop_node(Node).

start_slave(Args) ->
    MicroSecs = erlang:monotonic_time(),
    Name = "instr" ++ integer_to_list(MicroSecs),
    Pa = filename:dirname(code:which(?MODULE)),

    %% We pass arguments through ZFLAGS as the nightly tests rotate
    %% +Meamax/+Meamin which breaks the _enabled and _disabled tests unless
    %% overridden.
    ZFlags = os:getenv("ERL_ZFLAGS", ""),
    {ok, Node} = try
                     os:putenv("ERL_ZFLAGS", ZFlags ++ [" " | Args]),
                     test_server:start_node(list_to_atom(Name),
                                            slave,
                                            [{args, "-pa " ++ Pa}])
                 after
                     os:putenv("ERL_ZFLAGS", ZFlags)
                 end,
    Node.

generate_test_blocks() ->
    Runner = self(),
    Ref = make_ref(),
    spawn(fun() ->
              %% We've set the single-block carrier threshold to 1KB so one
              %% ought to land in a SBC and the other in a MBC. Both are kept
              %% alive forever.
              SBCs = [<<I, 0:(1 bsl 10)/unit:8>> ||
                      I <- lists:seq(1, ?GENERATED_SBC_BLOCK_COUNT)],
              MBCs = [<<I, 0:64/unit:8>> ||
                      I <- lists:seq(1, ?GENERATED_MBC_BLOCK_COUNT)],
              Runner ! Ref,
              receive
                   gurka -> gaffel ! {SBCs, MBCs}
              end
          end),
    receive
        Ref -> ok
    end.

churn_memory() ->
    %% All processes spawned from here on have 'low' priority to avoid starving
    %% others (e.g. the rpc process) which could cause the test to time out.
    [begin
         churn_list_to_binary(),
         churn_processes(),
         churn_ets()
     end || _ <- lists:seq(1, erlang:system_info(schedulers))],
    ok.

churn_processes() ->
    Pid = spawn_opt(fun churn_processes/0, [{priority, low}]),
    [Pid ! <<I, 0:128/unit:8>> || I <- lists:seq(1, 128)].

%% Nearly all types have a few allocations at all times but sl_alloc is
%% often empty. list_to_binary on large inputs will yield and spill the
%% state into an 'estack' which is allocated through sl_alloc.
%%
%% This is inherently unstable so we skip the verification step for this
%% type, but there's still a point to hammering it.
churn_list_to_binary() ->
    List = binary_to_list(<<0:(1 bsl 20)/unit:8>>),
    spawn_opt(fun() -> churn_list_to_binary_1(List) end, [{priority, low}]).

churn_list_to_binary_1(List) ->
    _ = id(list_to_binary(List)),
    churn_list_to_binary_1(List).

churn_ets() ->
    spawn_opt(fun() -> churn_ets_1(ets:new(gurka, [])) end, [{priority, low}]).

churn_ets_1(Tab) ->
    ets:insert(Tab, {gaffel, lists:seq(1, 16)}),
    ets:delete_all_objects(Tab),
    churn_ets_1(Tab).

id(I) ->
    I.
