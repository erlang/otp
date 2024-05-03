-module(default_ignore_overlapping_contract).

-export([t1/0]).

%% Should not result in a overlapping_contract warning,
%% as dialyzer_options does not set it to active
-spec t1() -> list();
        () -> [atom].
t1() ->
    case rand:uniform(2) of
        1 -> [test];
        2 -> [2]
    end.
