-module(overlapping_contract).

-export([t1/0]).

%% Should result in a overlapping_contract warning
-spec t1() -> list();
        () -> [atom].
t1() ->
    case rand:uniform(2) of
        1 -> [test];
        2 -> [2]
    end.
