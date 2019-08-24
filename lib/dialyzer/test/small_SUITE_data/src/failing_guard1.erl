%%-----------------------------------------------------------------------
%% Author: Kostis Sagonas (Wed Aug 23 14:54:25 CEST 2006)
%%
%% Program to test failing arithmetic comparisons with a number of the
%% wrong type. The first case is handled properly; the second one is not.
%% Why?
%%-----------------------------------------------------------------------

-module(failing_guard1).
-export([n/1]).

n(N) when (N / 2) =:= 2 -> multiple_of_four;
n(N) when (N div 3) =:= 2.0 -> multiple_of_six;
n(N) when (N rem 3) =:= 2.0 -> multiple_of_six;
n(N) when is_number(N) -> other_number.
