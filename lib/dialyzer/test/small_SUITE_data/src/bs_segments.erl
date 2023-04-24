-module(bs_segments).

-export([t/1]).

%% GH-7138: bogus segment sizes crashed the analysis.
t(<<_:undefined>>) ->
    ok.
