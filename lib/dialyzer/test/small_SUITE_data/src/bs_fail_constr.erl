-module(bs_fail_constr).

-export([w1/1, w2/1, w3/1, w4/1]).

w1(V) when is_float(V) ->
  <<V/integer>>.

w2(V) when is_atom(V) ->
  <<V/binary>>.

w3(S) when is_integer(S), S < 0 ->
  <<42:S/integer>>.

w4(V) when is_float(V) ->
  <<V/utf32>>.
