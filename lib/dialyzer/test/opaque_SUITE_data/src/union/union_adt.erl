-module(union_adt).
-export([new/1, new_a/1, new_rec/1]).

-record(rec, {x = 42 :: integer()}).

-opaque u() :: 'aaa' | 'bbb' | #rec{}.

new(a) -> aaa;
new(b) -> bbb;
new(X) when is_integer(X) ->
  #rec{x = X}.

%% the following two functions (and their uses in union_use.erl) test
%% that the return type is the opaque one and not just a subtype of it

new_a(a) -> aaa.

new_rec(X) when is_integer(X) ->
  #rec{x = X}.
