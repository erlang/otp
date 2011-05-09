%%% -*- erlang-indent-level: 2 -*-
%%%------------------------------------------------------------------------
%%% File    : bin_compr.erl
%%% Purpose : Test case which crashes in dialyzer_dataflow:bind_bin_segs/5.
%%%------------------------------------------------------------------------

-module(bin_compr).

-export([bc/1]).

%% The binary comprehension below is stupid: it consumes the whole
%% bitstr in one go and produces a [666] result provided Bits is a
%% bitstr of at least 8 bits. Still, this is a valid Erlang program
%% and dialyzer's analysis should not crash on it.
bc(Bits) ->
  [666 || <<_:8/integer, _/bits>> <= Bits].
