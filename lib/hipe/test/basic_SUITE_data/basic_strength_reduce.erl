%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Tests the strength reduction component of the HiPE compiler.
%%%-------------------------------------------------------------------
-module(basic_strength_reduce).

-export([test/0]).
%% These functions are exported so as to not remove them by inlining
-export([crash_0/1, crash_1/1, crash_2/1, crash_3/1, bug_div_2N/1]).

test() ->
  ok = test_strength_reduce1(),
  ok.

%%--------------------------------------------------------------------

test_strength_reduce1() ->
  ok = crash_0(0),
  ok = crash_1(42),
  ok = crash_2(42),
  ok = crash_3(42),
   5 =  42 bsr 3 = bug_div_2N(42),
  -6 = -42 bsr 3 = bug_div_2N(-42) - 1,
  ok.

%% This is a crash report by Peter Wang (10 July 2007) triggering an
%% R11B-5 crash: strength reduction could not handle calls with no
%% destination
crash_0(A) ->
  case A of
    0 ->
      A div 8,
      ok
  end.

%% The above was simplified to the following which showed another
%% crash, this time on RTL
crash_1(A) when is_integer(A), A >= 0 ->
  A div 8,
  ok.

%% A similar crash like the first one, but in a different place in the
%% code, was triggered by the following code
crash_2(A) when is_integer(A), A >= 0 ->
  A div 1,
  ok.

%% A crash similar to the first one happened in the following code
crash_3(A) ->
  case A of
    42 ->
      A * 0,
      ok
  end.

%% Strength reduction for div/2 and rem/2 with a power of 2
%% should be performed only for non-negative integers
bug_div_2N(X) when is_integer(X), X >= 0 ->
  X div 8;
bug_div_2N(X) when is_integer(X), X < 0 ->
  X div 8.

%%--------------------------------------------------------------------
