-module(mylib).
-export([foo/0]).

foo() -> erlang:time().
