-module(gh6580).
-export([f/0]).

%% GH-6580: dialyzer would crash when binding an impossible cons.
f() ->
    <<
        0
     || _ <-
            case ok of
                X ->
                    <<0 || _ <- []>>
            end,
        X <- 0,
        #{X := Y} <- 0
    >>.
