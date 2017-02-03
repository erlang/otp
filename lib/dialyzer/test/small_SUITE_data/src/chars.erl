-module(chars).

%% ERL-313

-export([t/0]).
-export([t1/0]).

-record(r, {f :: $A .. $Z}).

-type cs() :: $A..$Z | $a .. $z | $/.

-spec t() -> $0-$0..$9-$0| $?.

t() ->
    c(#r{f = $z - 3}),
    c($z - 3),
    c($B).

-spec c(cs()) -> $3-$0..$9-$0.

c($A + 1) -> 2;
c(C) ->
    case C of
        $z - 3 -> 3;
        #r{f = $z - 3} -> 7
    end.

%% Display contract with character in warning:
-spec f(#{a := $1, b => $2, c => $3}) -> ok. % invalid type spec
f(_) -> ok.

t1() -> f(#{b => $2}). % breaks the  contract
