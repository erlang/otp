-module(bigE).

-export([f/1]).

-record(r, {a, b}).

f(#r{b = B} = C) ->
    receive
	B ->
	    X = C#r.a,
            %% The compiler will do a case to extract the `a` field
            %% using a pattern variable named `rec0`. Without
            %% legalization the variable will be output as an atom and
            %% the compiler will report an error as the following `X +
            %% X` will always fail.
	    REC0 = X + X,
            %% If the legalization fails to detect that the default
            %% legalization of uppercasing the pattern variable would
            %% collide with the `REC0` below, we will get a warning
            %% for an unsafe use.
	    REC0
    end.
