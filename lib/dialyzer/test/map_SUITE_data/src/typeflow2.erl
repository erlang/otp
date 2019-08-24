-module(typeflow2).

-export([t1/1, t2/1, t3/1, t4/1, t5/1, t6/1, t7/1, optional3/1]).

t1(L) ->
    M = only_integers_and_lists(L),
    optional(M),
    case M of
	#{a := X} when is_integer(X) -> ok;
	#{a := X} when is_list(X) -> ok; %% Must warn here
	#{a := X} when is_pid(X) -> ok;
	_ -> fail
    end.

optional(#{a:=X}) ->
    true = is_integer(X);
optional(#{}) ->
    true.

only_integers_and_lists(L) -> only_integers_and_lists(L, #{}).

only_integers_and_lists([], M) -> M;
only_integers_and_lists([{K,V}|T], M) when is_integer(V); is_list(V)->
    only_integers_and_lists(T, M#{K => V}).

t2(L) ->
    M = only_integers_and_lists(L),
    optional(M),
    lists:sort(maps:get(a, M)),
    ok.

t3(L) ->
    M = only_integers_and_lists(L),
    lists:sort(maps:get(a, M)),
    ok.

t4(V) ->
    M=map_with(a,V),
    optional2(M),
    case M of
	#{a := X} when is_integer(X) -> ok;
	#{a := X} when is_list(X) -> ok; %% Must warn here
	_ -> fail
    end.

optional2(#{a:=X}) ->
    true = is_integer(X);
optional2(#{}) ->
    true.

map_with(K, V) when is_integer(V); is_list(V); is_atom(V) -> #{K => V}.

t5(L) ->
    M = only_integers_and_lists(L),
    optional3(M),
    case M of
	#{a := X} when is_integer(X) -> ok;
	#{a := X} when is_list(X) -> ok; %% Must warn here
	#{a := X} when is_pid(X) -> ok; %% Must warn here
	#{a := X} when is_atom(X) -> ok;
	_ -> fail
    end.

t6(L) ->
    M = only_integers_and_lists(L),
    case M of
	#{a := X} when is_integer(X) -> ok;
	#{a := X} when is_list(X) -> ok; %% Must not warn here
	_ -> fail
    end.

optional3(#{a:=X}) ->
    true = is_integer(X);
optional3(#{}) ->
    true.

t7(M) ->
    optional4(M),
    case M of
	#{a := X} when is_integer(X) -> ok;
	#{a := X} when is_list(X) -> ok;
	#{a := X} when is_pid(X) -> ok; %% Must warn here
	#{a := X} when is_atom(X) -> ok; %% Must warn here
	_ -> fail %% Must not warn here (requires parsing)
    end.

-spec optional4(#{a=>integer()|list()}) -> true.
optional4(#{}) -> true.
