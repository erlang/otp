%% -*- erlang-indent-level: 2 -*-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Module   :	hipe_timer
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-03-15 Erik Johansson (happi@it.uu.se): Created.
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_timer).

-export([tr/1, t/1, timer/1, time/1, empty_time/0]).
-export([advanced/2]).

t(F) ->
  {EWT,ERT} = empty_time(),
  {WT,RT} = time(F),
  {WT-EWT,(RT-ERT)/1000}.

tr(F) ->
  {EWT,ERT} = empty_time(),
  {R,{WT,RT}} = timer(F),
  {R,{WT-EWT,(RT-ERT)/1000}}.

empty_time() ->
  WTA = erlang:monotonic_time(),
  {A,_} = erlang:statistics(runtime),
  WTB = erlang:monotonic_time(),
  {B,_} = erlang:statistics(runtime),
  {(WTB-WTA)/erlang:convert_time_unit(1, second, native),B-A}.

time(F) -> 
  WTA = erlang:monotonic_time(),
  {A,_} = erlang:statistics(runtime),
  F(),
  WTB = erlang:monotonic_time(),
  {B,_} = erlang:statistics(runtime),
  {(WTB-WTA)/erlang:convert_time_unit(1, second, native),B-A}.

timer(F) -> 
  WTA = erlang:monotonic_time(),
  {A,_} = erlang:statistics(runtime),
  R = F(),
  WTB = erlang:monotonic_time(),
  {B,_} = erlang:statistics(runtime),
  {R,{(WTB-WTA)/erlang:convert_time_unit(1, second, native),B-A}}.

advanced(_Fun, I) when I < 2 -> false;
advanced(Fun, Iterations) ->
  R = Fun(),
  Measurements = [t(Fun) || _ <- lists:seq(1, Iterations)],
  {Wallclock, RunTime} = split(Measurements),
  WMin = lists:min(Wallclock),
  RMin = lists:min(RunTime),
  WMax = lists:max(Wallclock),
  RMax = lists:max(RunTime),
  WMean = mean(Wallclock),
  RMean = mean(RunTime),
  WMedian = median(Wallclock),
  RMedian = median(RunTime),
  WVariance = variance(Wallclock),
  RVariance = variance(RunTime),
  WStddev = stddev(Wallclock),
  RStddev = stddev(RunTime),
  WVarCoff = 100 * WStddev / WMean,
  RVarCoff = 100 * RStddev / RMean,
  WSum = lists:sum(Wallclock),
  RSum = lists:sum(RunTime),
  [{wallclock,[{min, WMin},
	       {max, WMax},
	       {mean, WMean},
	       {median, WMedian},
	       {variance, WVariance},
	       {stdev, WStddev},
	       {varcoff, WVarCoff},
	       {sum, WSum},
	       {values, Wallclock}]},
   {runtime,[{min, RMin},
	     {max, RMax},
	     {mean, RMean},
	     {median, RMedian},
	     {variance, RVariance},
	     {stdev, RStddev},
	     {varcoff, RVarCoff},
	     {sum, RSum},
	     {values, RunTime}]},
   {iterations, Iterations},
   {result, R}].

split(M) -> 
  split(M, [], []).

split([{W,R}|More], AccW, AccR) ->
  split(More, [W|AccW], [R|AccR]);
split([], AccW, AccR) ->
  {AccW, AccR}.

mean(L) ->
  mean(L, 0, 0).

mean([V|Vs], No, Sum) ->
  mean(Vs, No+1, Sum+V);
mean([], No, Sum) when No > 0 ->
  Sum/No;
mean([], _No, _Sum) ->
  exit(empty_list).
  
median(L) ->
  S = length(L),
  SL = lists:sort(L),
  case even(S) of
    true ->
      (lists:nth((S div 2), SL) + lists:nth((S div 2) + 1, SL)) / 2;
    false ->
      lists:nth((S div 2), SL)
  end.

even(S) ->
  (S band 1) =:= 0.

%% diffs(L, V) ->
%%   [X - V || X <- L].

square_diffs(L, V) ->
  [(X - V) * (X - V) || X <- L].

variance(L) ->
  Mean = mean(L),
  N = length(L),
  if N > 1 ->
      lists:sum(square_diffs(L,Mean)) / (N-1);
     true -> exit('too few values')
  end.

stddev(L) ->
  math:sqrt(variance(L)).
