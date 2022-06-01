%% This module knows about the feature ifn, but not abuot maybe thus
%% use maybe and then as ordinary atoms.

-module(feature_ifn).

-export([foo/0]).

foo() ->
    ['ifn', while, until].

bar() ->
    [until, while].
