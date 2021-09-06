-module(opaque).
-export([accidental_supertype/0]).

%% Should result in a missing_range warning, not a supertype warning.
-spec accidental_supertype() -> {term(),term()} | other.
accidental_supertype() ->
    case rand:uniform(2) of
        1 -> gb_sets:new();
        2 -> other
    end.
