-module(types_arity).

-export([ test1/0
        , test2/0
        , test3/0
        ]).

-export_type([tree/0, tree/1]).

-type tree(T) :: 'nil' | {'node', T, tree(T), tree(T)}.
-type tree()  :: tree(integer()).

-spec test1() -> tree().
test1() -> {node, 7, nil, nil}.

-spec test2() -> tree().
test2() -> {node, a, nil, nil}.

-spec test3() -> tree(atom()).
test3() -> {node, a, nil, nil}.
