%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2023. All Rights Reserved.
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

%% Matches the same declarations in erl_alloc_util.c
-define(GATHER_AHIST_FLAG_PER_PID, (1 bsl 0)).
-define(GATHER_AHIST_FLAG_PER_PORT, (1 bsl 1)).
-define(GATHER_AHIST_FLAG_PER_MFA, (1 bsl 2)).

-type block_histogram() :: tuple().

-type allocation_summary() ::
    {HistogramStart :: non_neg_integer(),
     UnscannedSize :: non_neg_integer(),
     Allocations :: #{ Origin :: allocation_origin() =>
                       #{ Type :: atom() => block_histogram() }}}.

-type allocation_origin() :: atom() | mfa() | pid() | port().

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
                  histogram_width => pos_integer(),
                  flags => [per_process | per_port | per_mfa] }.
allocations(Options0) ->
    SchedIds = lists:seq(0, erts_internal:no_aux_work_threads() - 1),

    Defaults = #{ scheduler_ids => SchedIds,
                  allocator_types => erlang:system_info(alloc_util_allocators),
                  histogram_start => 128,
                  histogram_width => 18,
                  flags => [] },

    Options = maps:merge(Defaults, Options0),
    Flags = allocations_flags(Options),

    Ref = make_ref(),
    {HistStart, MsgCount} =
        dispatch_gather(Options, Flags, Ref,
                        fun erts_internal:gather_alloc_histograms/1),

    alloc_hist_receive(HistStart, MsgCount, Flags, Ref).

allocations_flags(#{ flags := Flags }) ->
    lists:foldl(fun(per_process, Acc) ->
                        Acc bor ?GATHER_AHIST_FLAG_PER_PID;
                    (per_port, Acc) ->
                        Acc bor ?GATHER_AHIST_FLAG_PER_PORT;
                    (per_mfa, Acc) ->
                        Acc bor ?GATHER_AHIST_FLAG_PER_MFA
                end, 0, Flags).

alloc_hist_receive(_HistStart, 0, _Flags, _Ref) ->
    {error, not_enabled};
alloc_hist_receive(HistStart, MsgCount, Flags, Ref) when MsgCount > 0 ->
    {Unscanned, Histograms0} =
        alloc_hist_receive_1(MsgCount, Ref, 0, #{}),

    Histograms = case (Flags band ?GATHER_AHIST_FLAG_PER_PID) =/= 0 of
                     true -> alloc_hist_registered(Histograms0);
                     false -> Histograms0
                 end,

    {ok, {HistStart, Unscanned, Histograms}}.

alloc_hist_registered(Histograms) ->
    alloc_hist_registered_1(registered(), Histograms).

alloc_hist_registered_1([Name | Names], Histograms0) ->
    Pid = whereis(Name),
    case Histograms0 of
        #{ Pid := Hist } ->
            Histograms = maps:remove(Pid, Histograms0),
            alloc_hist_registered_1(Names, Histograms#{ Name => Hist });
        #{} ->
            alloc_hist_registered_1(Names, Histograms0)
    end;
alloc_hist_registered_1([], Histograms) ->
    Histograms.

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
                   InPool :: boolean(),
                   TotalSize :: non_neg_integer(),
                   UnscannedSize :: non_neg_integer(),
                   Allocations :: [{Type :: atom(),
                                    Count :: non_neg_integer(),
                                    Size :: non_neg_integer()}],
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
carriers(Options0) ->
    SchedIds = lists:seq(0, erts_internal:no_aux_work_threads() - 1),

    Defaults = #{ scheduler_ids => SchedIds,
                  allocator_types => erlang:system_info(alloc_util_allocators),
                  histogram_start => 512,
                  histogram_width => 14,
                  flags => [] },

    Options = maps:merge(Defaults, Options0),
    Flags = carriers_flags(Options),

    Ref = make_ref(),
    {HistStart, MsgCount} =
        dispatch_gather(Options, Flags, Ref,
                        fun erts_internal:gather_carrier_info/1),

    carrier_info_receive(HistStart, MsgCount, Ref).

carriers_flags(#{ flags := Flags }) when length(Flags) >= 0 ->
    0.

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
                   histogram_width := HistWidth }, Flags, Ref, Gather)
        when is_list(AllocatorTypes),
             is_list(SchedulerIds),
             HistStart >= 1, HistStart =< (1 bsl 28),
             HistWidth >= 1, HistWidth =< 32 ->
    MsgCount = lists:sum(
        [Gather({AllocatorType, SchedId, HistWidth, HistStart, Flags, Ref}) ||
         SchedId <- SchedulerIds,
         AllocatorType <- AllocatorTypes]),
    {HistStart, MsgCount};
dispatch_gather(_, _, _, _) ->
    error(badarg).
