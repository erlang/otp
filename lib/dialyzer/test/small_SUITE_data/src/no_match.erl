-module(no_match).
-export([t1/1, t2/1, t3/1]).
-record(rec, {field}).

t1(#rec{} = {_}) -> no_match1.

t2(42 = gazonk) -> no_match2.

t3(X) when false -> X.
