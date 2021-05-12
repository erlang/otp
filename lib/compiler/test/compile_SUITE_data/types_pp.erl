-module(types_pp).

-export([doit/0]).

make_atom() ->
    an_atom.

make_number(X, Y) ->
    X + Y.

make_float() ->
    3.14.

make_integer_range(X) ->
    case X of
	a -> 0;
	b -> 1;
	c -> 2;
	_ -> 3
    end.

make_integer() ->
    17.

make_nil() ->
    [].

make_list(X) when is_list(X) ->
    X ++ [1, 2, 3].

make_list_of_ints(X) when is_list(X) ->
    [ Y || Y <- X, is_integer(Y)].

make_maybe_improper_list(X) when is_list(X) ->
    X.

make_nonempty_list(X) ->
    [X].

make_nonempty_improper_list(X) ->
    [X|'end'].

make_empty_map() ->
    #{}.

make_map(X) when is_map(X) ->
    X.

make_map_known_types(K, V) when is_integer(K), is_float(V) ->
    #{ K => V }.

make_fun_unknown_arity_known_type() ->
    case ext:f() of
	0 -> fun(X) -> X + 1 end;
	1 -> fun(X, Y) -> X + Y end
    end.

make_fun_known_arity_known_type() ->
    fun(Y, Z) ->
            Y + Z
    end.

make_fun_unknown_arity_unknown_type() ->
    case ext:f() of
	0 -> fun(X) -> ext:f(X) end;
	1 -> fun(X, Y) -> ext:f(X, Y) end
    end.

make_fun_known_arity_unknown_type() ->
    fun(Y, Z) ->
            ext:f(Y, Z)
    end.

make_none() ->
    exit(foo).

make_unconstrained_tuple(X) when is_tuple(X) ->
    X.

make_known_size_tuple(X) when is_tuple(X), tuple_size(X) =:= 5 ->
    X.

make_inexact_tuple({X1,X2,X3,_X4,_X5,_X6,_X7,_X8,_X9,_X10,_X11,_X12,_X13}=X)
  when is_integer(X1), is_float(X2), is_integer(X3) ->
    case ext:f() of
	0 ->
	    {1, 2, 3};
	_ ->
	    X
    end.

make_union() ->
    case ext:f() of
	0 -> foo;
	1 -> [1, 2, 3];
	2 -> 7;
	3 -> 3.14;
	4 -> {tag0,1,2};
	5 -> {tag1,3,4};
	6 -> <<1,2,3>>
    end.

make_bitstring() ->
    <<1, 2, 3>>.

doit() ->
    {make_number(ext:f(), ext:f()), make_atom(),
     make_float(),
     make_integer(), make_integer_range(ext:f()),
     make_nil(), make_list(ext:f()), make_list_of_ints(ext:f()),
     make_maybe_improper_list(ext:f()),
     make_nonempty_list( ext:f()), make_nonempty_improper_list( ext:f()),
     make_empty_map(), make_map(ext:f()),
     make_map_known_types(ext:f(), ext:f()),
     make_fun_unknown_arity_known_type(),
     make_fun_known_arity_known_type(),
     make_fun_unknown_arity_unknown_type(),
     make_fun_known_arity_unknown_type(),
     make_unconstrained_tuple(ext:f()),
     make_known_size_tuple(ext:f()),
     make_inexact_tuple(ext:f()),
     make_union(),
     make_bitstring(),
     make_none()
    }.


