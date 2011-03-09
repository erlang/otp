%%---------------------------------------------------------------------------
%% A clone of 'rec_adt' so as to test its combination with 'queue_adt'
%%---------------------------------------------------------------------------
-module(mixed_opaque_rec_adt).

-export([new/0, get_a/1, get_b/1, set_a/2, set_b/2]).

-record(rec, {a :: atom(), b = 0 :: integer()}).

-opaque rec() :: #rec{}.

-spec new() -> rec().
new() -> #rec{a = gazonk, b = 42}.

-spec get_a(rec()) -> atom().
get_a(#rec{a = A}) -> A.

-spec get_b(rec()) -> integer().
get_b(#rec{b = B}) -> B.

-spec set_a(rec(), atom()) -> rec().
set_a(R, A) -> R#rec{a = A}.

-spec set_b(rec(), integer()) -> rec().
set_b(R, B) -> R#rec{b = B}.
