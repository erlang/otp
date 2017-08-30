%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%====================================================================
%% Note: Uses the process keys:
%%  hipe_time       - Indicates what to time.
%%  hipe_timers     - A stack of timers.
%%  {hipe_timer,T}  - Delata times for named timers. 
%%  T               - Acc times for all named timers T.
%%====================================================================

-module(hipe_timing).
-export([start/2, stop/2,
	 %% start_timer/0, stop_timer/1,
	 %% get_hipe_timer_val/1, set_hipe_timer_val/2,
	 %% start_hipe_timer/1, stop_hipe_timer/1,
	 start_optional_timer/2, stop_optional_timer/2]).

-include("../main/hipe.hrl").

%%=====================================================================

-spec start(string(), atom()) -> 'ok'.

start(Text, Mod) when is_atom(Mod) ->
  Timers = 
    case get(hipe_timers) of
      undefined -> [];
      Ts -> Ts
    end,
  Space = [$| || _ <- Timers],
  Total = start_timer(),
  put(hipe_timers, [Total|Timers]),
  ?msg("[@~7w]" ++ Space ++ "> ~s~n", [Total,Text]).

-spec stop(string(), atom()) -> 'ok'.

stop(Text, Mod) when is_atom(Mod) ->
  {Total,_Last} = erlang:statistics(runtime),
  case get(hipe_timers) of
    [StartTime|Timers] -> 
      Space = [$| || _ <- Timers],
      put(hipe_timers,Timers),
      ?msg("[@~7w]" ++ Space ++ "< ~s: ~w~n", [Total, Text, Total-StartTime]);
    _ ->
      put(hipe_timers, []),
      ?msg("[@~7w]< ~s: ~w~n", [Total, Text, Total])
  end.

-spec start_optional_timer(string(), atom()) -> 'ok'.

start_optional_timer(Text, Mod) ->
  case get(hipe_time) of 
    true -> start(Text, Mod);
    all -> start(Text, Mod);
    Mod -> start(Text, Mod);
    List when is_list(List) ->
      case lists:member(Mod, List) of
	true -> start(Text, Mod);
	false -> ok
      end;
    _ -> ok
  end.

-spec stop_optional_timer(string(), atom()) -> 'ok'.

stop_optional_timer(Text, Mod) ->
  case get(hipe_time) of
    true -> stop(Text, Mod);
    all -> stop(Text, Mod);
    Mod -> stop(Text, Mod);
    List when is_list(List) ->
      case lists:member(Mod, List) of
	true -> stop(Text, Mod);
	false -> ok
      end;
    _ -> ok
  end.

-spec start_timer() -> non_neg_integer().

start_timer() ->
  {Total, _Last} = erlang:statistics(runtime),
  Total.

%% stop_timer(T) ->
%%   {Total, _Last} = erlang:statistics(runtime),
%%   Total - T.
%% 
%% start_hipe_timer(Timer) ->
%%   Time = erlang:statistics(runtime),
%%   put({hipe_timer,Timer}, Time).
%% 
%% stop_hipe_timer(Timer) ->
%%   {T2, _} = erlang:statistics(runtime),
%%   T1 =
%%     case get({hipe_timer,Timer}) of
%%       {T0, _} -> T0;
%%       _ -> 0
%%     end,
%%   AccT = 
%%     case get(Timer) of
%%       T when is_integer(T) -> T;
%%       _ -> 0
%%     end,
%%   put(Timer,AccT+T2-T1).
%% 
%% get_hipe_timer_val(Timer) ->
%%   case get(Timer) of
%%     T when is_integer(T) -> T;
%%     _ -> 0
%%   end.
%% 
%% set_hipe_timer_val(Timer, Val) ->
%%   put(Timer, Val).
