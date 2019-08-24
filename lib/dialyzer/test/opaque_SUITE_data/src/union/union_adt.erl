-module(union_adt).
-export([new/1, new_a/1, new_rec/1]).

%% Now (R17) that opaque types are no longer recognized by their shape
%% this test case is rather meaningless.

-record(rec, {x = 42 :: integer()}).

-opaque u() :: 'aaa' | 'bbb' | #rec{}.

-spec new(_) -> u().

new(a) -> aaa;
new(b) -> bbb;
new(X) when is_integer(X) ->
  #rec{x = X}.

%% the following two functions (and their uses in union_use.erl) test
%% that the return type is the opaque one and not just a subtype of it

-spec new_a(_) -> u().

new_a(a) -> aaa.

-spec new_rec(_) -> u().

new_rec(X) when is_integer(X) ->
  #rec{x = X}.
