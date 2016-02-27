-module(maps_merge).

-export([t1/0, t2/0, t3/0, t4/0, t5/0]).

t1() ->
    #{a:=1} = maps:merge(#{}, #{}).

t2() ->
    #{hej := _} = maps:merge(cao(), cio()),
    #{{} := _} = maps:merge(cao(), cio()).

t3() ->
    #{a:=1} = maps:merge(cao(), cio()),
    #{7:=q} = maps:merge(cao(), cio()).

t4() ->
    #{a:=1} = maps:merge(cio(), cao()),
    #{7:=q} = maps:merge(cio(), cao()).

t5() ->
    #{a:=2} = maps:merge(cao(), #{}).

-spec cao() -> #{a := 1, q => none(), 11 => _, atom() => _}.
cao() ->
    binary_to_term(<<131,116,0,0,0,3,100,0,1,97,97,1,100,0,1,98,97,9,100,0,1,
		     102,104,0>>).

-spec cio() -> #{3 := ok, 7 => none(), z => _, integer() => _}.
cio() -> binary_to_term(<<131,116,0,0,0,2,97,3,100,0,2,111,107,97,9,97,6>>).
