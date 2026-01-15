-module(use_nifs).
-export([?MODULE/0,calculator_nif/0,id/1]).
-nifs([calculator_nif/0]).

?MODULE() ->
    case id(false) of
        true ->
            erlang:load_nif("my_nif", 42),
            calculator_nif();
        false ->
            ok
    end,
    ok.

calculator_nif() ->
    erlang:nif_error(undef).

id(I) ->
    I.
