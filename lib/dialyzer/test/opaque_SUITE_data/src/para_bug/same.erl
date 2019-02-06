-module(same).

-export([baz/1]).

-record(bar, {
          a :: same_type:st(integer()),
          b :: same_type:st(atom())
         }).

baz(Bar) ->
    _ = wrap_find(0, Bar#bar.a),
    wrap_find(0, Bar#bar.b).

wrap_find(K, D) ->
  same_type:t(K, D).
