%% This module is ignorant about any features and thus use 'ifn',
%% 'maybe' and 'then' as ordinary atoms.

-module(feature_ignorant).

-export([foo/0]).

foo() ->
    [ifn, while, until].

frob(while) -> false.

bar() ->
    [until, while].

baz(ifn) ->
    true.
