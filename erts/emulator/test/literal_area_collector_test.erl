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
    try
        wait_for_idle_literal_collector(Pid, Start, ScaledTimeout, -1, 0)
    catch
        throw:done ->
            ok
    end.

wait_for_idle_literal_collector(Pid, Start, Timeout, NWaiting, WRedsStart) ->
    {W, R} = case process_info(Pid, [status, reductions]) of
                 [{status, waiting}, {reductions, Reds}] ->
                     %% Assume that reds aren't bumped more than
                     %% 2 in order to service this process info
                     %% request...
                     case {NWaiting > 100, Reds - WRedsStart =< 2*NWaiting} of
                         {true, true} ->
                             throw(done);
                         {false, true} ->
                             {NWaiting+1, WRedsStart};
                         _ ->
                             {0, Reds}
                     end;
                 _ ->
                     {-1, 0}
             end,
    Now = erlang:monotonic_time(millisecond),
    if Now - Start > Timeout ->
            error({busy_literal_area_collecor_timout, Timeout});
       true ->
            ok
    end,
    receive after 1 -> ok end,
    wait_for_idle_literal_collector(Pid, Start, Timeout, W, R).
    
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
