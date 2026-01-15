%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1998-2025. All Rights Reserved.
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
-moduledoc """
Analysis and Utility Functions for Instrumentation

The module `instrument` contains support for studying the resource usage in an
Erlang runtime system. Currently, only the allocation of memory can be studied.

> #### Note {: .info }
>
> Since this module inspects internal details of the runtime system it may
> differ greatly from one version to another. We make no compatibility
> guarantees in this module.

## See Also

[erts_alloc(3)](`e:erts:erts_alloc.md`), [erl(1)](`e:erts:erl_cmd.md`)
""".

-export([allocations/0, allocations/1,
         carriers/0, carriers/1]).

%% Matches the same declarations in erl_alloc_util.c
-define(GATHER_AHIST_FLAG_PER_PID, (1 bsl 0)).
-define(GATHER_AHIST_FLAG_PER_PORT, (1 bsl 1)).
-define(GATHER_AHIST_FLAG_PER_MFA, (1 bsl 2)).

-doc """
A histogram of block sizes where each interval's upper bound is twice as high as
the one before it.

The upper bound of the first interval is provided by the function that returned
the histogram, and the last interval has no upper bound.

For example, the histogram below has 40 (`message`) blocks between 128-256 bytes
in size, 78 blocks between 256-512 bytes,2 blocks between 512-1024 bytes, and 2
blocks between 1-2KB.

```erlang
> instrument:allocations(#{ histogram_start => 128, histogram_width => 15 }).
{ok, {128, 0, #{ message => {0,40,78,2,2,0,0,0,0,0,0,0,0,0,0}, ... } }}
```
""".
-type block_histogram() :: tuple().

-doc """
A summary of allocated block sizes (including their headers) grouped by their
`Origin` and `Type`.

`Origin` is generally which NIF or driver that allocated the blocks, or 'system'
if it could not be determined.

`Type` is the allocation category that the blocks belong to, e.g. `db_term`,
`message` or `binary`. The categories correspond to those in
[erl_alloc.types](https://github.com/erlang/otp/blob/master/erts/emulator/beam/erl_alloc.types).

If one or more carriers could not be scanned in full without harming the
responsiveness of the system, `UnscannedSize` is the number of bytes that had to
be skipped.
""".
-type allocation_summary() ::
    {HistogramStart :: non_neg_integer(),
     UnscannedSize :: non_neg_integer(),
     Allocations :: #{ Origin :: allocation_origin() =>
                       #{ Type :: atom() => block_histogram() }}}.

-type allocation_origin() :: atom() | mfa() | pid() | port().

-doc #{equiv => allocations(#{})}.
-doc(#{since => <<"OTP 21.0">>}).
-spec allocations() -> {ok, Result} | {error, Reason} when
    Result :: allocation_summary(),
    Reason :: not_enabled.
allocations() ->
    allocations(#{}).

-doc """
Returns a summary of all tagged allocations in the system, optionally filtered
by allocator type and scheduler id.

Only binaries and allocations made by NIFs and drivers are tagged by default,
but this can be configured an a per-allocator basis with the
[`+M<S>atags` ](`e:erts:erts_alloc.md#M_atags`)emulator option.

If the specified allocator types are not enabled, the call will fail with
`{error, not_enabled}`.

The following options can be used:

- **`allocator_types`** - The allocator types that will be searched.

  Specifying a specific allocator type may lead to strange results when carrier
  migration between different allocator types has been enabled: you may see
  unexpected types (e.g. process heaps when searching binary_alloc), or fewer
  blocks than expected if the carriers the blocks are on have been migrated out
  to an allocator of a different type.

  Defaults to all `alloc_util` allocators.

- **`scheduler_ids`** - The scheduler ids whose allocator instances will be
  searched. A scheduler id of 0 will refer to the global instance that is not
  tied to any particular scheduler. Defaults to all schedulers and the global
  instance.

- **`histogram_start`** - The upper bound of the first interval in the allocated
  block size histograms. Defaults to 128.

- **`histogram_width`** - The number of intervals in the allocated block size
  histograms. Defaults to 18.

- **`flags`** - Controls how to group the output, for example showing
  allocations on a per-process basis (when possible) rather than only a
  NIF/driver-basis. Defaults to `[]`.

_Example:_

```erlang
> instrument:allocations(#{ histogram_start => 128, histogram_width => 15 }).
{ok,{128,0,
     #{udp_inet =>
           #{driver_event_state => {0,0,0,0,0,0,0,0,0,1,0,0,0,0,0}},
       system =>
           #{heap => {0,0,0,0,20,4,2,2,2,3,0,1,0,0,1},
             db_term => {271,3,1,52,80,1,0,0,0,0,0,0,0,0,0},
             code => {0,0,0,5,3,6,11,22,19,20,10,2,1,0,0},
             binary => {18,0,0,0,7,0,0,1,0,0,0,0,0,0,0},
             message => {0,40,78,2,2,0,0,0,0,0,0,0,0,0,0},
             ... }
       spawn_forker =>
           #{driver_select_data_state =>
                 {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0}},
       ram_file_drv => #{drv_binary => {0,0,0,0,0,0,1,0,0,0,0,0,0,0,0}},
       prim_file =>
           #{process_specific_data => {2,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
             nif_trap_export_entry => {0,4,0,0,0,0,0,0,0,0,0,0,0,0,0},
             monitor_extended => {0,1,0,0,0,0,0,0,0,0,0,0,0,0,0},
             drv_binary => {0,0,0,0,0,0,1,0,3,5,0,0,0,1,0},
             binary => {0,4,0,0,0,0,0,0,0,0,0,0,0,0,0}},
       prim_buffer =>
           #{nif_internal => {0,4,0,0,0,0,0,0,0,0,0,0,0,0,0},
             binary => {0,4,0,0,0,0,0,0,0,0,0,0,0,0,0}}}}}
```
""".
-doc(#{since => <<"OTP 21.0">>}).
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

-doc """
`AllocatorType` is the type of the allocator that employs this carrier.

`InPool` is whether the carrier is in the migration pool.

`TotalSize` is the total size of the carrier, including its header.

`Allocations` is a summary of the allocated blocks in the carrier. Note that
carriers may contain multiple different block types when carrier pools are
shared between different allocator types (see the
[`erts_alloc` ](`e:erts:erts_alloc.md#M_cp`)documentation for more details).

`FreeBlocks` is a histogram of the free block sizes in the carrier.

If the carrier could not be scanned in full without harming the responsiveness
of the system, `UnscannedSize` is the number of bytes that had to be skipped.
""".
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

-doc #{equiv => carriers(#{})}.
-doc(#{since => <<"OTP 21.0">>}).
-spec carriers() -> {ok, Result} | {error, Reason} when
    Result :: carrier_info_list(),
    Reason :: not_enabled.
carriers() ->
    carriers(#{}).

-doc """
Returns a summary of all carriers in the system, optionally filtered by
allocator type and scheduler id.

If the specified allocator types are not enabled, the call will fail with
`{error, not_enabled}`.

The following options can be used:

- **`allocator_types`** - The allocator types that will be searched. Defaults to
  all `alloc_util` allocators.

- **`scheduler_ids`** - The scheduler ids whose allocator instances will be
  searched. A scheduler id of 0 will refer to the global instance that is not
  tied to any particular scheduler. Defaults to all schedulers and the global
  instance.

- **`histogram_start`** - The upper bound of the first interval in the free
  block size histograms. Defaults to 512.

- **`histogram_width`** - The number of intervals in the free block size
  histograms. Defaults to 14.

_Example:_

```erlang
> instrument:carriers(#{ histogram_start => 512, histogram_width => 8 }).
{ok,{512,
     [{driver_alloc,false,262144,0,
                    [{driver_alloc,1,32784}],
                    {0,0,0,0,0,0,0,1}},
      {binary_alloc,false,32768,0,
                    [{binary_alloc,15,4304}],
                    {3,0,0,0,1,0,0,0}},
      {...}|...]}}
```
""".
-doc(#{since => <<"OTP 21.0">>}).
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
