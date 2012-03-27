-module(maybe_servers).

-export([maybe_server/2, mirror_maybe_server/2]).

maybe_server(O, I) ->
    case O of
	no ->
	    maybe_loop(fun(_) -> fin end, I);
	yes ->
	    maybe_loop(fun(X) -> {ok, X} end, I)
    end.

maybe_loop(F, X)->
    case F(X) of
	{ok, Y} -> maybe_loop(F, Y);
	fin -> exit(n)
    end.

mirror_maybe_loop(F, X)->
    case F(X) of
	{ok, Y} -> mirror_maybe_loop(F, Y);
	fin -> exit(n)
    end.

mirror_maybe_server(O, I) ->
    case O of
	no ->
	    mirror_maybe_loop(fun(_) -> fin end, I);
	yes ->
	    mirror_maybe_loop(fun(X) -> {ok, X} end, I)
    end.
