-module(lc_warnings).
-compile([export_all]).

close(Fs) ->
    %% There should be a warning since we ignore a potential
    %% {error,Error} return from file:close/1.
    [file:close(F) || F <- Fs],

    %% No warning because the type of unmatched return will be ['ok']
    %% (which is a list of a simple type).
    [ok = file:close(F) || F <- Fs],

    %% Suppressed.
    _ = [file:close(F) || F <- Fs],
    ok.

format(X) ->
    %% No warning since the result of the list comprehension is
    %% a list of simple.
    [io:format("~p\n", [E]) || E <- X],

    %% Warning explicitly suppressed.
    _ = [io:format("~p\n", [E]) || E <- X],
    ok.

opaque1() ->
    List = gen_atom(),
    %% This is a list of an externally defined opaque type. Since
    %% we are not allowed to peek inside opaque types, there should
    %% be a warning (even though the type in this case happens to be
    %% an atom).
    [E || E <- List],

    %% Suppressed.
    _ = [E || E <- List],
    ok.

opaque2() ->
    List = gen_array(),
    %% This is an list of an externally defined opaque type. Since
    %% we are not allowed to peek inside opaque types, there should
    %% be a warning.
    [E || E <- List],

    %% Suppressed.
    _ = [E || E <- List],
    ok.

opaque3() ->
    List = gen_int(),

    %% No warning, since we are allowed to look into the type and can
    %% see that it is a simple type.
    [E || E <- List],

    %% Suppressed.
    _ = [E || E <- List],
    ok.

opaque4() ->
    List = gen_tuple(),

    %% There should be a warning, since we are allowed to look inside
    %% the opaque type and see that it is a tuple (non-simple).
    [E || E <- List],

    %% Suppressed.
    _ = [E || E <- List],
    ok.

gen_atom() ->
    [opaque_atom_adt:atom(ok)].

gen_array() ->
    [array:new()].


gen_int() ->
    [opaque_int(42)].

gen_tuple() ->
    [opaque_tuple(x, 25)].

-opaque opaque_int() :: integer().

-spec opaque_int(integer()) -> opaque_int().

opaque_int(Int) -> Int.

-opaque opaque_tuple() :: {any(),any()}.

-spec opaque_tuple(any(), any()) -> opaque_tuple().

opaque_tuple(X, Y) ->
    {X,Y}.
