-module(bs_fail_constr).

-export([w1/1, w2/1, w3/1, w4/1, bad_size_1/1, bad_size_2/1, bad_size_3/1]).

w1(V) when is_float(V) ->
  <<V/integer>>.

w2(V) when is_atom(V) ->
  <<V/binary>>.

w3(S) when is_integer(S), S < 0 ->
  <<42:S/integer>>.

w4(V) when is_float(V) ->
  <<V/utf32>>.

%% GH-6419
bad_size_1(<<X:[]>>) ->
    ok.

bad_size_2(Bin) ->
    Size = [],
    case Bin of
        <<X:Size>> ->
            ok
    end.

%% GH-6473
bad_size_3(X) when X; <<X:X>> ->
    bad_size_3(X),
    X.
