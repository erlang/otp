%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-module(socket_test_ttest_lib).

-compile({no_auto_import, [error/2]}).

-export([
         t/0, tdiff/2,
         formated_timestamp/0, format_timestamp/1,
         format_time/1,

         formated_process_stats/1, formated_process_stats/2,

         format/2,
         error/1, error/2,
         info/1, info/2
        ]).

%% ==========================================================================

t() ->
    os:timestamp().

tdiff({A1, B1, C1} = _T1x, {A2, B2, C2} = _T2x) ->
    T1 = A1*1000000000+B1*1000+(C1 div 1000), 
    T2 = A2*1000000000+B2*1000+(C2 div 1000), 
    T2 - T1.

formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, N3} = TS) ->
    {_Date, Time}  = calendar:now_to_local_time(TS),
    {Hour,Min,Sec} = Time,
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w.4~w",
                             [Hour, Min, Sec, round(N3/1000)]),  
    lists:flatten(FormatTS).

%% Time is always in number os ms (milli seconds)
%% At some point, we should convert this to a more readable format...
format_time(T) when (T < 1000) ->
    format("~w ms", [T]);
format_time(T) ->
    format("~w sec (~w ms)", [T div 1000, T]).


formated_process_stats(Pid) ->
    formated_process_stats("", Pid).

formated_process_stats(Prefix, Pid) when is_list(Prefix) andalso is_pid(Pid) ->
    try
        begin
            TotHeapSz = pi(Pid, total_heap_size),
            HeapSz    = pi(Pid, heap_size),
            StackSz   = pi(Pid, stack_size),
            Reds      = pi(Pid, reductions),
            GCInfo    = pi(Pid, garbage_collection),
            MinBinVHeapSz = proplists:get_value(min_bin_vheap_size, GCInfo),
            MinHeapSz     = proplists:get_value(min_heap_size,      GCInfo),
            MinGCS        = proplists:get_value(minor_gcs,          GCInfo),
            format("~n   ~sTotal Heap Size:         ~p"
                   "~n   ~sHeap Size:               ~p"
                   "~n   ~sStack Size:              ~p"
                   "~n   ~sReductions:              ~p"
                   "~n   ~s[GC] Min Bin VHeap Size: ~p"
                   "~n   ~s[GC] Min Heap Size:      ~p"
                   "~n   ~s[GC] Minor GCS:          ~p",
                   [Prefix, TotHeapSz,
                    Prefix, HeapSz,
                    Prefix, StackSz,
                    Prefix, Reds,
                    Prefix, MinBinVHeapSz,
                    Prefix, MinHeapSz,
                    Prefix, MinGCS])
        end
    catch
        _:_:_ ->
            ""
    end.


pi(Pid, Item) ->
    {Item, Info} = process_info(Pid, Item),
    Info.



%% ==========================================================================

format(F, A) ->
    lists:flatten(io_lib:format(F, A)).

error(F) ->
    error(F, []).

error(F, A) ->
    print(get(sname), "<ERROR> " ++ F, A).

info(F) ->
    info(F, []).

info(F, A) ->
    print(get(sname), "<INFO> " ++ F, A).

print(undefined, F, A) ->
    print("- ", F, A);
print(Prefix, F, A) ->
    io:format("[~s, ~s] " ++ F ++ "~n", [formated_timestamp(), Prefix |A]).

