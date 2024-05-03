-module(bs_segments).

-export([t/1, f/0]).

%% GH-7138: bogus segment sizes crashed the analysis.
t(<<_:undefined>>) ->
    ok.

%% GH-7325: variant of the above.
f() ->
    <<0:(undefined)>>.
