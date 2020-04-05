%% Test that a spec containing more items than actually returned
%% (whether by accident or by benign overspeccing) does not prevent
%% detection of impossible matches.

-module(extra_range).

-export([t1/2, t2/2, t3/2, t4/2]).

-dialyzer([no_return]).

%% this spec matches the behaviour of the code
-spec normal(integer()) -> ok | error.
normal(1) -> ok;
normal(2) -> error.

t1(X, Y) when is_integer(X), is_integer(Y) ->
    ok = normal(X),
    error = normal(Y),
    ok.


%% this spec has a typo, which should cause anyone trying to match on
%% `ok = typo(X)' to get a warning, because `ok' is not in the spec
-spec typo(integer()) -> ook | error.
typo(1) -> ok;
typo(2) -> error.

t2(X, Y) when is_integer(X), is_integer(Y) ->
    ok = typo(X),  % warning expected - not allowed according to spec
    error = typo(Y),
    ok.


%% this is overspecified, and should cause a warning for trying
%% to match on `no = over(X)', because it cannot succeed and either
%% the spec should be updated or the code should be extended
-spec over(integer()) -> yes | no | maybe.
over(1) -> yes;
over(_) -> maybe.

t3(X, Y) when is_integer(X), is_integer(Y) ->
    yes = over(X),
    no = over(Y),  % warning expected - spec or code needs fixing
    maybe = over(X + Y),
    ok.


%% this is underspecified, which should cause anyone trying to match on
%% `maybe = under(X)' to get a warning, because `maybe' is not in the spec
-spec under(integer()) -> yes | no.
under(1) -> yes;
under(2) -> no;
under(_) -> maybe.

t4(X, Y) when is_integer(X), is_integer(Y) ->
    yes = under(X),
    no = under(Y),
    maybe = under(X + Y), % warning expected - not in spec
    ok.
