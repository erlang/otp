%% Program which shows that the handling of binaries was not correct.
%% The success typing inferred was:
%%  -spec bits1(<<_:3>>) -> <<_:3>>.
%% while it should be:
%%  -spec bits1(<<_:3,_:_*1>>) -> <<_:3>>.
%% because the only constraint which exists for the head variable is
%% that it must be a bitstring of bit size at least 3, not a bitstring
%% of bit size 3.
-module(bs_constraints).

-export([bits1/1, bits2/1, bits3/1, bins/1, test/0]).

bits1(B) ->
    <<B:3/bits>>.

bits2(B) ->
    <<B:4/bits>>.

bits3(B) ->
    {bits1(B), bits2(B)}.

%% Same problem with the one below. The success typing should be:
%%  -spec bins(<<_:16,_:_*1>>) -> <<_:16>>.
bins(B) ->
    <<B:2/binary>>.

%% Same problem, when unit size is a variable:
test() ->
  foo(8, 0, <<42>>).

foo(N, S, A) ->
    <<A:S/binary-unit:1, A:(N-S)/binary-unit:1>>.
