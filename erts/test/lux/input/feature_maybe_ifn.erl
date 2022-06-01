%% This module knows about the both features ifn and maybe
%% These atoms are thus quoted

-module(feature_maybe_ifn).

-export([foo/0]).

foo() ->
    ['ifn', 'maybe', 'then'].

bar() ->
    ['then', 'maybe'].
