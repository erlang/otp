-module(maps_update_assoc).
-export([test/0]).

test() ->
    M0 = id(#{1=>a,2=>b,3.0=>c,4=>d,5=>e}),

    M1 = M0#{1=>42,2=>100,4=>[a,b,c]},
    #{1:=42,2:=100,3.0:=c,4:=[a,b,c],5:=e} = M1,
    #{1:=42,2:=b,4:=d,5:=e,2.0:=100,3.0:=c,4.0:=[a,b,c]} = M0#{1.0=>float,1:=42,2.0=>wrong,2.0=>100,4.0=>[a,b,c]},

    M2 = M0#{3.0=>new},
    #{1:=a,2:=b,3.0:=new,4:=d,5:=e} = M2,
    M2 = M0#{3.0:=wrong,3.0=>new},

    %% Errors cases.
    BadMap = id(badmap),
    {'EXIT',{{badmap,badmap},_}} = (catch BadMap#{nonexisting=>val}),

    ok.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
