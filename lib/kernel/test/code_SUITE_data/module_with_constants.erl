-module(module_with_constants).
-export([get_a_constant/0,
         get_a_term_with_unshared_constants/1,
         get_a_term_with_shared_constants/1]).

get_a_constant() ->
    [{magic, ?VERSION, constant}, 1, 2, 7, 17, 42].

get_a_term_with_unshared_constants(0) ->
    get_a_constant();
get_a_term_with_unshared_constants(N) ->
    X = get_a_term_with_unshared_constants(N-1),
    [N | X].

get_a_term_with_shared_constants(0) ->
    get_a_constant();
get_a_term_with_shared_constants(N) ->
    X = get_a_term_with_shared_constants(N-1),
    [N, X | X].
