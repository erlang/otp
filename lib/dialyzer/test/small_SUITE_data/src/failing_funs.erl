-module(failing_funs).

-compile(export_all).

% Crashes with system call. No spec.
foo1() -> halt().

% Crashes with system call. With spec.
-spec foo2() -> no_return().
foo2() -> halt().

% Crashes on its own. No spec.
foo3() -> case a of b -> ok end.

% Crashes on its own. With spec.
-spec foo4() -> no_return().
foo4() -> case a of b -> ok end.

% Creates fun that crashes with system call. No spec.
foo5() -> fun() -> halt() end.

% Creates fun that crashes with system call. With spec.
-spec foo6() -> fun(() -> no_return()).
foo6() -> fun() -> halt() end.

% Creates fun from named fun that will crash. Neither have spec.
foo7() -> fun foo1/0.

% Creates fun from named fun that will crash. Has spec.
-spec foo8() -> fun(() -> no_return()).
foo8() -> fun foo1/0.

% Creates fun from named fun that will crash. Named has spec.
foo9() -> fun foo2/0.

% Creates fun from named fun that will crash. Both have specs.
-spec foo10() -> fun(() -> no_return()).
foo10() -> fun foo2/0.

% Creates fun from named fun that will crash. Neither have spec.
foo11() -> fun foo3/0.

% Creates fun from named fun that will crash. Has spec.
-spec foo12() -> fun(() -> no_return()).
foo12() -> fun foo3/0.

% Creates fun from named fun that will crash. Named has spec.
foo13() -> fun foo4/0.

% Creates fun from named fun that will crash. Both have specs.
-spec foo14() -> fun(() -> no_return()).
foo14() -> fun foo4/0.

% Creates fun calling a named fun that will crash. Neither have spec.
foo15() -> fun() -> foo1() end.

% Creates fun calling a named fun that will crash. Has spec.
-spec foo16() -> fun(() -> no_return()).
foo16() -> fun() -> foo1() end.

% Creates fun calling a named fun that will crash. Named has spec.
foo17() -> fun() -> foo2() end.

% Creates fun calling a named fun that will crash. Both have specs.
-spec foo18() -> fun(() -> no_return()).
foo18() -> fun() -> foo2() end.

% Creates fun calling a named fun that will crash. Neither have spec.
foo19() -> fun() -> foo3() end.

% Creates fun calling a named fun that will crash. Has spec.
-spec foo20() -> fun(() -> no_return()).
foo20() -> fun() -> foo3() end.

% Creates fun calling a named fun that will crash. Named has spec.
foo21() -> fun() -> foo4() end.

% Creates fun calling a named fun that will crash. Both have specs.
-spec foo22() -> fun(() -> no_return()).
foo22() -> fun() -> foo4() end.

% Creates two funs with no local return and will return one or die. No spec.
foo23() ->
    Bomb = fun() -> halt() end,
    case get(42) of
	a -> Bomb();
	b -> fun() -> halt() end
    end.

% Creates two funs with no local return and will return one or die. With spec.
-spec foo24() -> fun(() -> no_return()).
foo24() ->
    Bomb = fun() -> halt() end,
    case get(42) of
	a -> Bomb();
	b -> fun() -> halt() end
    end.

% Creates two funs with no local return and will return one or die. No spec.
foo25() ->
    Bomb = fun() -> foo1() end,
    case get(42) of
	a -> Bomb();
	b -> fun() -> foo1() end
    end.

% Creates two funs with no local return and will return one or die. With spec.
-spec foo26() -> fun(() -> no_return()).
foo26() ->
    Bomb = fun foo1/0,
    case get(42) of
	a -> Bomb();
	b -> fun foo1/0
    end.

% Creates two funs with no local return and will return one or die. No spec.
foo27() ->
    Bomb = fun foo1/0,
    case get(42) of
	a -> Bomb();
	b -> fun foo1/0
    end.

% Creates two funs with no local return and will return one or die. With spec.
-spec foo28() -> fun(() -> no_return()).
foo28() ->
    Bomb = fun() -> foo1() end,
    case get(42) of
	a -> Bomb();
	b -> fun() -> foo1() end
    end.

% Creates two funs with no local return and will return one or die. No spec.
foo29() ->
    Bomb = fun() -> foo2() end,
    case get(42) of
	a -> Bomb();
	b -> fun() -> foo2() end
    end.

% Creates two funs with no local return and will return one or die. With spec.
-spec foo30() -> fun(() -> no_return()).
foo30() ->
    Bomb = fun foo2/0,
    case get(42) of
	a -> Bomb();
	b -> fun foo2/0
    end.

% Creates two funs with no local return and will return one or die. No spec.
foo31() ->
    Bomb = fun foo2/0,
    case get(42) of
	a -> Bomb();
	b -> fun foo2/0
    end.

% Creates two funs with no local return and will return one or die. With spec.
-spec foo32() -> fun(() -> no_return()).
foo32() ->
    Bomb = fun() -> foo2() end,
    case get(42) of
	a -> Bomb();
	b -> fun() -> foo2() end
    end.

% Creates two funs with no local return and will return one or die. No spec.
foo33() ->
    Bomb = fun() -> foo3() end,
    case get(42) of
	a -> Bomb();
	b -> fun() -> foo3() end
    end.

% Creates two funs with no local return and will return one or die. With spec.
-spec foo34() -> fun(() -> no_return()).
foo34() ->
    Bomb = fun foo3/0,
    case get(42) of
	a -> Bomb();
	b -> fun foo3/0
    end.

% Creates two funs with no local return and will return one or die. No spec.
foo35() ->
    Bomb = fun foo3/0,
    case get(42) of
	a -> Bomb();
	b -> fun foo3/0
    end.

% Creates two funs with no local return and will return one or die. With spec.
-spec foo36() -> fun(() -> no_return()).
foo36() ->
    Bomb = fun() -> foo3() end,
    case get(42) of
	a -> Bomb();
	b -> fun() -> foo3() end
    end.

% Creates two funs with no local return and will return one or die. No spec.
foo37() ->
    Bomb = fun() -> foo4() end,
    case get(42) of
	a -> Bomb();
	b -> fun() -> foo4() end
    end.

% Creates two funs with no local return and will return one or die. With spec.
-spec foo38() -> fun(() -> no_return()).
foo38() ->
    Bomb = fun foo4/0,
    case get(42) of
	a -> Bomb();
	b -> fun foo4/0
    end.

% Creates two funs with no local return and will return one or die. No spec.
foo39() ->
    Bomb = fun foo4/0,
    case get(42) of
	a -> Bomb();
	b -> fun foo4/0
    end.

% Creates two funs with no local return and will return one or die. With spec.
-spec foo40() -> fun(() -> no_return()).
foo40() ->
    Bomb = fun() -> foo4() end,
    case get(42) of
	a -> Bomb();
	b -> fun() -> foo4() end
    end.

% Obtains two funs with no local return and will return one or die. No spec.
foo41() ->
    Bomb = foo5(),
    case get(42) of
	a -> Bomb();
	b -> foo5()
    end.

% Obtains two funs with no local return and will return one or die. With spec.
-spec foo42() -> fun(() -> no_return()).
foo42() ->
    Bomb = foo5(),
    case get(42) of
	a -> Bomb();
	b -> foo5()
    end.
