%%-----------------------------------------------------------------------------
%% Test which produces an erroneous warning:
%%   Guard test is_atom(A::'bar' | 'foo') can never succeed
%% due to the handling of not which of course succeeds when its argument fails
%%-----------------------------------------------------------------------------
-module(not_bogus_warning).

-export([t1/0, t2/0, t3/0, t4/0]).

t1() ->
  [A || A <- [foo, bar], not is_atom(A)].

t2() ->
  [A || A <- [foo, bar], not is_integer(A)].

t3() ->
  should_we_warn_here(42).

should_we_warn_here(X) when is_integer(X) -> int.

t4() ->
  should_we_warn_or_not(42).

should_we_warn_or_not(X) when not is_integer(X) -> not_int;
should_we_warn_or_not(X) -> int.
