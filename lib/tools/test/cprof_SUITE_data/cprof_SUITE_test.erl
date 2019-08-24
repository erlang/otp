-module(cprof_SUITE_test).

-export([seq/3, seq_r/3]).



%% Stack recursive seq
seq(Stop, Stop, Succ) when function(Succ) ->
    [Stop];
seq(Start, Stop, Succ) when function(Succ) ->
    [Start | seq(Succ(Start), Stop, Succ)].



%% Tail recursive seq, result list is reversed
seq_r(Start, Stop, Succ) when function(Succ) ->
    seq_r(Start, Stop, Succ, []).

seq_r(Stop, Stop, _, R) ->
    [Stop | R];
seq_r(Start, Stop, Succ, R) ->
    seq_r(Succ(Start), Stop, Succ, [Start | R]).



