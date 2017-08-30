-module(order).

-export([t1/0, t2/0, t3/0, t4/0, t5/0, t6/0]).

t1() ->
    case maps:get(a, #{a=>1, a=>b}) of
	Int when is_integer(Int) -> fail;
	Atom when is_atom(Atom) -> error(ok);
	_Else -> fail
    end.

t2() ->
    case maps:get(a, #{a=>id_1(1), a=>id_b(b)}) of
	Int when is_integer(Int) -> fail;
	Atom when is_atom(Atom) -> error(ok);
	_Else -> fail
    end.

t3() ->
    case maps:get(a, #{a=>id_1(1), id_a(a)=>id_b(b)}) of
	Int when is_integer(Int) -> fail;
	Atom when is_atom(Atom) -> error(ok);
	_Else -> fail
    end.

t4() ->
    case maps:get(a, #{a=>id_1(1), a_or_b()=>id_b(b)}) of
	Int when is_integer(Int) -> ok;
	Atom when is_atom(Atom) -> ok;
	_Else -> fail
    end.

t5() ->
    case maps:get(c, #{c=>id_1(1), a_or_b()=>id_b(b)}) of
	Int when is_integer(Int) -> error(ok);
	Atom when is_atom(Atom) -> fail;
	_Else -> fail
    end.

t6() ->
    case maps:get(a, #{a_or_b()=>id_1(1), id_a(a)=>id_b(b)}) of
	Int when is_integer(Int) -> fail;
	Atom when is_atom(Atom) -> error(ok);
	_Else -> fail
    end.

id_1(X) -> X.

id_a(X) -> X.

id_b(X) -> X.

any() -> binary_to_term(<<>>).

-spec a_or_b() -> a | b.
a_or_b() -> any().
