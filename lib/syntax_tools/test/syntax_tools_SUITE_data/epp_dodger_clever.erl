-module(epp_dodger_clever).

-export([foo1/0]).

-define(macro_string, "hello world").

foo1() ->
    % string combining ?
    [?macro_string
     "hello world ",
     "more hello"
     ?macro_string].
