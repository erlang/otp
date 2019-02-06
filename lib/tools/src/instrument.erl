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
-module(instrument).

-export([allocations/0, allocations/1,
         carriers/0, carriers/1]).

-type block_histogram() :: tuple().

-type allocation_summary() ::
    {HistogramStart :: non_neg_integer(),
     UnscannedSize :: non_neg_integer(),
     Allocations :: #{ Origin :: atom() =>
                       #{ Type :: atom() => block_histogram() }}}.

-spec allocations() -> {ok, Result} | {error, Reason} when
    Result :: allocation_summary(),
    Reason :: not_enabled.
allocations() ->
    allocations(#{}).

-spec allocations(Options) -> {ok, Result} | {error, Reason} when
    Result :: allocation_summary(),
    Reason :: not_enabled,
    Options :: #{ scheduler_ids => list(non_neg_integer()),
                  allocator_types => list(atom()),
                  histogram_start => pos_integer(),
                  histogram_width => pos_integer() }.
allocations(Options) ->
    Ref = make_ref(),

    Defaults = #{ scheduler_ids => lists:seq(0, erlang:system_info(schedulers)),
                  allocator_types => erlang:system_info(alloc_util_allocators),
                  histogram_start => 128,
                  histogram_width => 18 },

    {HistStart, MsgCount} =
        dispatch_gather(maps:merge(Defaults, Options), Ref,
                        fun erts_internal:gather_alloc_histograms/1),

    alloc_hist_receive(HistStart, MsgCount, Ref).

alloc_hist_receive(_HistStart, 0, _Ref) ->
    {error, not_enabled};
alloc_hist_receive(HistStart, MsgCount, Ref) when MsgCount > 0 ->
    {Unscanned, Histograms} = alloc_hist_receive_1(MsgCount, Ref, 0, #{}),
    {ok, {HistStart, Unscanned, Histograms}}.

alloc_hist_receive_1(0, _Ref, Unscanned, Result) ->
    {Unscanned, Result};
alloc_hist_receive_1(MsgCount, Ref, Unscanned0, Result0) ->
    receive
        {Ref, Unscanned, Tags} ->
            Result = lists:foldl(fun alloc_hist_fold_result/2, Result0, Tags),
            alloc_hist_receive_1(MsgCount - 1, Ref, Unscanned0 + Unscanned, Result)
    end.

alloc_hist_fold_result({Id, Type, BlockHist}, Result0) ->
    IdAllocs0 = maps:get(Id, Result0, #{}),
    MergedHists = case maps:find(Type, IdAllocs0) of
                      {ok, PrevHist} ->
                          alloc_hist_merge_hist(tuple_size(BlockHist),
                                                BlockHist,
                                                PrevHist);
                      error ->
                          BlockHist
                  end,
    IdAllocs = IdAllocs0#{ Type => MergedHists },
    Result0#{ Id => IdAllocs }.

alloc_hist_merge_hist(0, A, _B) ->
    A;
alloc_hist_merge_hist(Index, A, B) ->
    Merged = setelement(Index, A, element(Index, A) + element(Index, B)),
    alloc_hist_merge_hist(Index - 1, Merged, B).

-type carrier_info_list() ::
    {HistogramStart :: non_neg_integer(),
     Carriers :: [{AllocatorType :: atom(),
                   TotalSize :: non_neg_integer(),
                   UnscannedSize :: non_neg_integer(),
                   AllocatedSize :: non_neg_integer(),
                   AllocatedCount :: non_neg_integer(),
                   InPool :: boolean(),
                   FreeBlocks :: block_histogram()}]}.

-spec carriers() -> {ok, Result} | {error, Reason} when
    Result :: carrier_info_list(),
    Reason :: not_enabled.
carriers() ->
    carriers(#{}).

-spec carriers(Options) -> {ok, Result} | {error, Reason} when
    Result :: carrier_info_list(),
    Reason :: not_enabled,
    Options :: #{ scheduler_ids => list(non_neg_integer()),
                  allocator_types => list(atom()),
                  histogram_start => pos_integer(),
                  histogram_width => pos_integer() }.
carriers(Options) ->
    Ref = make_ref(),

    Defaults = #{ scheduler_ids => lists:seq(0, erlang:system_info(schedulers)),
                  allocator_types => erlang:system_info(alloc_util_allocators),
                  histogram_start => 512,
                  histogram_width => 14 },

    {HistStart, MsgCount} =
        dispatch_gather(maps:merge(Defaults, Options), Ref,
                        fun erts_internal:gather_carrier_info/1),

    carrier_info_receive(HistStart, MsgCount, Ref).

carrier_info_receive(_HistStart, 0, _Ref) ->
    {error, not_enabled};
carrier_info_receive(HistStart, MsgCount, Ref) ->
    {ok, {HistStart, carrier_info_receive_1(MsgCount, Ref, [])}}.

carrier_info_receive_1(0, _Ref, Result) ->
    lists:flatten(Result);
carrier_info_receive_1(MsgCount, Ref, Result0) ->
    receive
        {Ref, Carriers} ->
            carrier_info_receive_1(MsgCount - 1, Ref, [Carriers, Result0])
    end.

dispatch_gather(#{ allocator_types := AllocatorTypes,
                   scheduler_ids := SchedulerIds,
                   histogram_start := HistStart,
                   histogram_width := HistWidth }, Ref, Gather)
        when is_list(AllocatorTypes),
             is_list(SchedulerIds),
             HistStart >= 1, HistStart =< (1 bsl 28),
             HistWidth >= 1, HistWidth =< 32 ->
    MsgCount = lists:sum(
        [Gather({AllocatorType, SchedId, HistWidth, HistStart, Ref}) ||
         SchedId <- SchedulerIds,
         AllocatorType <- AllocatorTypes]),
    {HistStart, MsgCount};
dispatch_gather(_, _, _) ->
    error(badarg).
