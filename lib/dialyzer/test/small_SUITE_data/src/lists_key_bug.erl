-module(lists_key_bug).

%% OTP-15570

-export([t/1]).

t(V) ->
    K = key(V),
    case lists:keyfind(K, 1, [{<<"foo">>, bar}]) of
        false ->
            a;
        {_, _} ->
            b
    end.

key(1) ->
    3;
key(2) ->
    <<"foo">>.
