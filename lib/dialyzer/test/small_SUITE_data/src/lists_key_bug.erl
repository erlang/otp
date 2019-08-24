-module(lists_key_bug).

%% OTP-15570

-export([is_1/1, is_2/1, i/1, t1/0, t2/0, im/0]).

%% int_set([3])
is_1(V) ->
    K = ikey(V),
    case lists:keyfind(K, 1, [{<<"foo">>, bar}]) of
        false ->
            a;
        {_, _} ->
            b
    end.

ikey(1) ->
    3;
ikey(2) ->
    <<"foo">>.

%% int_set([3, 5])
is_2(V) ->
    K = iskey(V),
    case lists:keyfind(K, 1, [{<<"foo">>, bar}]) of
        false ->
            a;
        {_, _} ->
            b
    end.

iskey(1) ->
    12;
iskey(2) ->
    14;
iskey(3) ->
    <<"foo">>.

%% integer()
i(V) ->
    K = intkey(V),
    case lists:keyfind(K, 1, [{9.0, foo}]) of
        false ->
            a;
        {_, _} ->
            b
    end.

intkey(K) when is_integer(K) ->
    K + 9999.

t1() ->
    case lists:keyfind({17}, 1, [{{17.0}, true}]) of
        false ->
            a;
        {_, _} ->
            b
    end.

t2() ->
    case lists:keyfind({17.0}, 1, [{{17}, true}]) of
        false ->
            a;
        {_, _} ->
            b
    end.

%% Note: #{1.0 => a} =/= #{1 => a}.
im() ->
    case lists:keyfind(#{1.0 => a}, 1, [{#{1 => a}, foo}]) of
        false ->
            a;
        {_, _} ->
            b
    end.
