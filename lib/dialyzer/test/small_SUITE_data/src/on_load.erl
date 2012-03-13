%%% This is to ensure that "on_load" functions are never reported as unused.

-module(on_load).

-export([foo/0]).

-on_load(bar/0).

foo() -> ok.

bar() -> ok.
