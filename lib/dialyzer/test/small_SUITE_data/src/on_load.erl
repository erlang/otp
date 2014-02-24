%%% This is to ensure that "on_load" functions are never reported as unused.
%%% In addition, all functions called by a function in an on_load attribute
%%% should be considered as called by an entry point of the module.

-module(on_load).

-export([foo/0]).

-on_load(bar/0).

foo() -> ok.

bar() -> gazonk(17).

gazonk(N) when N < 42 -> gazonk(N+1);
gazonk(42) -> ok.
