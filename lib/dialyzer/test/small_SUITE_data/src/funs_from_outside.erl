-module(funs_from_outside).

-export([run1/2, run2/2, run3/2]).
-export([test1/1, test2/1]).

%%------------------------------------------------------------------------------

run1(X, Y) ->
    testa(fun do_something/1, X, Y).

testa(Fun, X, Y) ->
    F = case even(X) of
	    true -> Fun;
	    false -> fun do_nothing/1
	end,
    case F(Y) of
	{ok, _} -> ok;
	error -> error
    end.

do_nothing(_) -> {ok, nothing}.

do_something(_) -> {ok, something}.

even(X) ->
    X rem 2 =:= 0.

%%------------------------------------------------------------------------------

%% Duplicating code since we are monovariant...

run2(X, Y) ->
    testb(fun do_something/1, X, Y).

testb(Fun, X, Y) ->
    F = case even(X) of
	    true -> Fun;
	    false -> fun do_nothing/1
	end,
    case F(Y) of
	error -> error
    end.

%%------------------------------------------------------------------------------

%% Duplicating code since we are monovariant...

run3(X, Y) ->
    testc(fun do_something_2/1, X, Y).

testc(Fun, X, Y) ->
    F = case even(X) of
	    true -> Fun;
	    false -> fun do_nothing/1
	end,
    case F(Y) of
	{ok, _} -> ok;
        %% This pattern can match. 
	error -> error
    end.

do_something_2(foo) -> {ok, something};
do_something_2(_) -> error.

%%------------------------------------------------------------------------------

test1(Fun) ->
    F = case get(test1) of
            test1_t -> Fun;
            test1_f -> fun fok/0
        end,
    error = F().

fok() -> ok.

%%------------------------------------------------------------------------------

test2(Fun) ->
    F = case get(test1) of
            test1_t -> fun fok/0;
            test1_f -> fun fok/0
        end,
    error = F().
