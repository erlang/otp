-module(maps_build_and_match_aliasing).
-export([test/0]).

test() ->
    M1 = id(#{a=>1,b=>2,c=>3,d=>4}),
    #{c:=C1=_=_=C2} = M1,
    true = C1 =:= C2,
    #{a:=A,a:=A,a:=A,b:=B,b:=B} = M1,
    #{a:=A,a:=A,a:=A,b:=B,b:=B,b:=2} = M1,
    #{a:=A=1,a:=A,a:=A,b:=B=2,b:=B,b:=2} = M1,
    #{c:=C1, c:=_, c:=3, c:=_, c:=C2} = M1,
    #{c:=C=_=3=_=C} = M1,

    M2 = id(#{"a"=>1,"b"=>2,"c"=>3,"d"=>4}),
    #{"a":=A2,"a":=A2,"a":=A2,"b":=B2,"b":=B2,"b":=2} = M2,
    #{"a":=_,"a":=_,"a":=_,"b":=_,"b":=_,"b":=2} = M2,
    ok.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
