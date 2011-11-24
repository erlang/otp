%% Test case that results in a confusing warning -- created from a
%% very stripped down actual application. The second case clause of
%% test/1 cannot possibly match because all a-pairs match with the
%% first clause. Dialyzer complains that the second argument of the
%% second 2-tuple has type 'aaa' | 'bbb'. This is mucho confusing
%% since there is no 'a'-pair whose second element is 'aaa' | 'bbb'.
%% Pattern matching compilation is of course what's to blame here.

-module(confusing_warning).
-export([test/1]).

test(N) when is_integer(N) ->
  case foo(N) of
    {a, I} when is_integer(I) ->
      I;
    {a, {_, L}} ->	% this clause cannot possibly match
      L
  end.

foo(1) -> {a, 42};
foo(2) -> {b, aaa};	% this is really unused
foo(3) -> {b, bbb}.	% this is really unused
