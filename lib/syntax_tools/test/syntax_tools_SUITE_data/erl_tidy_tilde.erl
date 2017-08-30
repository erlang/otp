%%
%% File:    erl_tidy_tilde.erl
%% Author:  Mark Bucciarelli
%% Created: 2016-06-05
%%

-module(erl_tidy_tilde).

-export([start/0]).

start() ->
    io:put_chars("tilde characters ('~')in source were "
                 "breaking erl_tidy\n").
