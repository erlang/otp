-module(read).

-export([lc/0, funfuns/0, bi/0]).

-xref({xx,ff,22}).
-xref({{all,0},{no,0}}).
-xref([{{all,0},{i,0}},{{all,0},{x2,5}}]).
-xref([apa]).
-xref({all,0}).
-xref([{{{all},0},{no,0}},{{all,0},{m,x2,5}}]).
-xref([{{a,14},{q,f,17}}]).
-xref({{i,f,17},{g,18}}).
-xref({{j,f,17},{i,g,18}}).

-xref({{funfuns,0},{'$F_EXPR',177}}).
-xref({{funfuns,0},{?MODULE,'$F_EXPR',178}}).
-xref({{funfuns,0},{modul,'$F_EXPR',179}}).

lc() ->
    Tab = ets:new(),
    [Mt||{_M,Mt} <- ets:tab2list(Tab)].

funfuns() ->
    A = variable,

    %% Spawn...

    %% Recognized. POS1=28.
    spawn(fun() -> mod17:fun17() end),
    spawn(fun local/0),
    spawn(fun binary_to_term/1), % builtin, not collected
    spawn({dist,func}),
    spawn({dist,A}), % {dist,'$F_EXPR',0}
    spawn_link(fun() -> mod17:fun17() end),
    spawn_link({dist,func}),
    spawn_link({dist,A}), % {dist,'$F_EXPR',0}

    %% POS2=POS1+10
    spawn({dist,func}(arg1,arg2), {d,f}),
    spawn({dist,func}(arg1,arg2), fun() -> mod42:func() end),
    spawn_link({dist,func}(arg1,arg2), {d,f}),
    spawn_link({dist,func}(arg1,arg2), fun() -> mod42:func() end),

    %% POS3=POS2+6
    spawn(dist, func, [arg1,arg2]), % spawn/3 is builtin
    spawn(expr, A, [arg1]), % {expr,'$F_EXPR',1}
    spawn_link(dist, func, [arg1,arg2]), % spawn_link/3 is builtin
    spawn_link(expr, A, [arg1,arg2]), % {expr,'$F_EXPR',2}

    %% POS4=POS3+6
    spawn(node, modul, function, []),
    spawn(node, modul, A, [a]), % {modul,'$F_EXPR',1}
    spawn({dist,func}(arg1,arg2), spm, spf, [a,b]),
    spawn({dist,func}(arg1,arg2), spm, A, [a]), % {spm,'$F_EXPR',1}
    spawn_link({dist,func}(arg1,arg2), spm, spf, [a,b]),
    spawn_link({dist,func}(arg1,arg2), spm, A, [a]), % {spm,'$F_EXPR',1}
    spawn_opt(spm, spf, [arg3, arg4], [opt1, bi()]),
    spawn_opt(spm, A, [a], [opt1, bi()]), % {spm,'$F_EXPR',1}

    %% Not recognized or invalid. POS5=POS4+10
    spawn(A), % {'$M_EXPR','$F_EXPR',0}
    spawn(17), % {'$M_EXPR','$F_EXPR',0}
    spawn_link(A), % {'$M_EXPR','$F_EXPR',0}

    %% POS6=POS5+5
    spawn({a,b},[1008]), % {'$M_EXPR','$F_EXPR',0}
    spawn_link({a,b},[1008]), % {'$M_EXPR','$F_EXPR',0}

    spawn(n, A, A), % {n,'$F_EXPR',-1}

    %% POS7=POS6+6
    spawn(n, A,f,[1007]), % {'$M_EXPR',f,1}, spawn/3 is builtin
    spawn_opt(A,f,[1007],[]), % {'$M_EXPR',f,1}

    %% Apply...

    %% Recognized. POS8=POS7+6
    {hej,san}(1002),
    {hej,A}(1002), % {hej,'$F_EXPR',1}
    t:A(1003), % {t,'$F_EXPR',1}
    apply({a,b},[1005]),
    apply({a,A},[1005]), % {a,'$F_EXPR',1}
    apply(m,f,[100011]),
    apply(m,A,[100011]), % {m,'$F_EXPR',1}
    %% POS9=POS8+8
    apply(A, f, [bi()]), % {'$M_EXPR',f,1}
    {erlang,apply}({a,b},[8888]),
    {erlang,apply}({a,A},[8888]), % {a,'$F_EXPR',1}
    {erlang,apply}({erlang,apply},[{erlang,not_a_function},[7777]]),
    apply(erlang, apply, [erlang, apply, [mod, func, [atom77,tjohej]]]),
    erlang:apply(foo), % not an apply, but an unknown function
    apply(fun(X) -> math:add3(X) end, [7]),
    (fun(X) -> q:f(X) end)(3008),

    %% Not recognized or invalid. POS10=POS9+10
    A:foo(1000), % {'$M_EXPR',foo,1}
    A(17), % {'$M_EXPR','$F_EXPR',1}
    A(17,[one,two]), % {'$M_EXPR','$F_EXPR',2}
    apply(apa,[1001]), % {'$M_EXPR','$F_EXPR',1}
    {mod1:fun1(hej),san}(1017), % {'$M_EXPR',san,1}
    A:A(1004), % {'$M_EXPR','$F_EXPR',1}
    %% POS11=POS10+7
    apply(A,A,[1006]), % {'$M_EXPR','$F_EXPR',1}
    apply(A,A,[1009 | 10010]), % {'$M_EXPR','$F_EXPR',-1}
    apply(m,f,[10000 | 9999]), % {m,f,-1}
    apply(m,f,a), % {m,f,-1}
    3(a), % {'$M_EXPR','$F_EXPR',1}
    apply(3,[a]), % {'$M_EXPR','$F_EXPR',1}

    %% POS12=POS11+8
    apply(A, A), % number of arguments is not known, {'$M_EXPR','$F_EXPR',-1}
    Args0 = [list],
    Args = [a | Args0], % number of arguments is known
    apply(A, Args), % {'$M_EXPR','$F_EXPR',2}
    apply(m3, f3, Args), %
    NotArgs = [is_not, a | list], % number of arguments is not known
    apply(A, NotArgs), % {'$M_EXPR','$F_EXPR',-1}
    apply(m4, f4, NotArgs), % {m4,f4,-1}

    %% OTP Internal. POS13=POS12+10
    erts_debug:apply(dm, df, [17], foobar),
    erts_debug:apply(debug, A, [], A), % {debug,'$F_EXPR',0}
    erts_debug:apply(A, A, A, A). % {'$M_EXPR','$F_EXPR',-1}

bi() when length([]) > 17 ->
    foo:module_info(),
    module_info(),
    A = true andalso tjo ,
    t:foo(A),
    case true of
	true when integer(1) ->
	    X = foo;
	false ->
	    X = flopp
    end,
    self() ! X == -A orelse false;
bi() ->
    %% POS14=POS13+18
    Z = fun(Y) -> Y end,
    case true of
	true when length([a,b]) > 4 ->
	    F = fun(X) -> X end,
	    F(3);
	false  ->
	    F = 17,
	    F(3) % {'$M_EXPR','$F_EXPR',1}
    end,
    Z(apa),
    erlang:module_info();
bi() ->
    Bin11 = <<1, 17, 42>>,
    Bin12 = <<"abc">>,
    false = (Bin11 == Bin12),
    A = 1, B = 17,
    Bin3 = <<A, B, (bi()):16>>,
    <<D:16, E, F/binary>> = Bin3,
    X = 9, <<(X+1):8>>,
    _Fyy = <<X:4/little-signed-integer-unit:8>>,
    D + E + F;
bi() ->
    %% EEP37. POS15=POS14+23
    F = fun Fact(N) when N > 0 ->
                N * Fact(N - 1);
            Fact(0) ->
                1
        end,
    F(6),
    G = fun _(foo) -> bar;
            _(X) -> X / 3
        end,
    G(foo).

local() ->
    true.
