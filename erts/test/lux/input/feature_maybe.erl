%% This module knows about the feature while, but not about ifn
%% and thus use ifn as an ordinary atom

-module(feature_maybe).

-export([foo/0]).

foo() ->
    [ifn, 'while', 'until'].

bar() ->
    ['until', 'while'].
