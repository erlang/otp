%%---------------------------------------------------------------------
%% A test for which the analysis gave wrong results because it did not
%% handle the is_tuple/1 guard properly.
%%---------------------------------------------------------------------

-module(opaque_bug3).

-export([test/1]).

-record(c, {}).

-opaque o() :: 'a' | #c{}.

-spec test(o()) -> 42.

test(#c{} = O) -> t(O).

t(T) when is_tuple(T) -> 42;
t(a) -> gazonk.
