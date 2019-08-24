%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests that check that tail recursion optimization occurs.
%%%-------------------------------------------------------------------
-module(basic_tail_rec).

-export([test/0]).
-export([app0/2]).  %% used in an apply/3 call

test() ->
  ok = test_app_tail(),
  ok.

%%--------------------------------------------------------------------
%% Written by Mikael Pettersson: check that apply is tail recursive.

%% Increased the following quantity from 20 to 30 so that the test
%% remains valid even with the naive register allocator.  - Kostis
-define(SIZE_INCREASE, 30).

test_app_tail() ->
  Inc = start(400),
  %% io:format("Inc ~w\n", [Inc]),
  case Inc > ?SIZE_INCREASE of
    true ->
      {error, "apply/3 is not tail recursive in native code"};
    false ->
      ok
  end.

start(N) ->
  app0(N, hipe_bifs:nstack_used_size()).

app0(0, Size0) ->
  hipe_bifs:nstack_used_size() - Size0;
app0(N, Size) ->
  apply(?MODULE, app0, [N-1, Size]).
