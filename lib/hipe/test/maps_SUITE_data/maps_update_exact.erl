-module(maps_update_exact).
-export([test/0]).

test() ->
    M0 = id(#{1=>a,2=>b,3.0=>c,4=>d,5=>e}),

    M1 = M0#{1:=42,2:=100,4:=[a,b,c]},
    #{1:=42,2:=100,3.0:=c,4:=[a,b,c],5:=e} = M1,
    M1 = M0#{1:=wrong,1=>42,2=>wrong,2:=100,4:=[a,b,c]},

    M2 = M0#{3.0:=new},
    #{1:=a,2:=b,3.0:=new,4:=d,5:=e} = M2,
    M2 = M0#{3.0=>wrong,3.0:=new},
    true = M2 =/= M0#{3=>right,3.0:=new},
    #{ 3 := right, 3.0 := new } = M0#{3=>right,3.0:=new},

    M3 = id(#{ 1 => val}),
    #{1 := update2,1.0 := new_val4} = M3#{
	1.0 => new_val1, 1 := update, 1=> update3,
	1 := update2, 1.0 := new_val2, 1.0 => new_val3,
	1.0 => new_val4 },

    %% Errors cases.
    {'EXIT',{{badmap,nil},_}} = (catch ((id(nil))#{ a := b })),
    {'EXIT',{{badkey,nonexisting},_}} = (catch M0#{nonexisting:=val}),
    {'EXIT',{{badkey,_},_}} = (catch M0#{1.0:=v,1.0=>v2}),
    {'EXIT',{{badkey,_},_}} = (catch M0#{42.0:=v,42:=v2}),
    {'EXIT',{{badkey,_},_}} = (catch M0#{42=>v1,42.0:=v2,42:=v3}),
    ok.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
