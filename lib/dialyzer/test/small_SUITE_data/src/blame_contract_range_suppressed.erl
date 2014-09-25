%%-----------------------------------------------------------------------
%% Like ./blame_contract_range.erl, but warning is suppressed.
%%-----------------------------------------------------------------------
-module(blame_contract_range_suppressed).

-export([foo/0]).

foo() ->
  bar(b).

-dialyzer({nowarn_function, bar/1}).

-spec bar(atom()) -> a.
bar(a) -> a;
bar(b) -> b.
