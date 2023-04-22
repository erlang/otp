%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2019-2021. All Rights Reserved.
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
-module(literal_area_collector_test).

-export([check_idle/0, check_idle/1]).

check_idle() ->
    check_idle(5000).

check_idle(Timeout) when is_integer(Timeout) > 0 ->
    ScaledTimeout = Timeout*test_server:timetrap_scale_factor(),
    Pid = find_literal_area_collector(),
    Start = erlang:monotonic_time(millisecond),
    Alias = alias(),
    wait_for_idle_literal_collector(Pid, Alias, Start, ScaledTimeout).

wait_for_idle_literal_collector(Pid, Alias, Start, Timeout) ->
    Ref = make_ref(),
    Pid ! {get_status, Ref, Alias},
    Now = erlang:monotonic_time(millisecond),
    TMO = case Start + Timeout - Now of
              TimeLeft when TimeLeft < 0 -> 0;
              TimeLeft -> TimeLeft
          end,
    receive
        {Ref, idle} ->
            unalias(Alias),
            ok;
        {Ref, _} ->
            receive after 10 -> ok end,
            wait_for_idle_literal_collector(Pid, Alias, Start, Timeout)
    after TMO ->
            unalias(Alias),
            receive {Ref, _} -> ok after 0 -> ok end,
            error({busy_literal_area_collecor_timout, Timeout})
    end.
    
find_literal_area_collector() ->
    case get('__literal_area_collector__') of
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            find_save_literal_area_collector(processes()),
            find_literal_area_collector()
    end.

find_save_literal_area_collector([P|Ps]) ->
    case process_info(P, initial_call) of
        {initial_call,{erts_literal_area_collector,start,0}} ->
            put('__literal_area_collector__', P); 
        _ ->
            find_save_literal_area_collector(Ps)
    end.
