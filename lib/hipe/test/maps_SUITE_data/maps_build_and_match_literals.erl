-module(maps_build_and_match_literals).
-export([test/0]).

test() ->
    #{} = id(#{}),
    #{1:=a} = id(#{1=>a}),
    #{1:=a,2:=b} = id(#{1=>a,2=>b}),
    #{1:=a,2:=b,3:="c"} = id(#{1=>a,2=>b,3=>"c"}),
    #{1:=a,2:=b,3:="c","4":="d"} = id(#{1=>a,2=>b,3=>"c","4"=>"d"}),
    #{1:=a,2:=b,3:="c","4":="d",<<"5">>:=<<"e">>} =
	id(#{1=>a,2=>b,3=>"c","4"=>"d",<<"5">>=><<"e">>}),
    #{1:=a,2:=b,3:="c","4":="d",<<"5">>:=<<"e">>,{"6",7}:="f"} =
	id(#{1=>a,2=>b,3=>"c","4"=>"d",<<"5">>=><<"e">>,{"6",7}=>"f"}),
    #{1:=a,2:=b,3:="c","4":="d",<<"5">>:=<<"e">>,{"6",7}:="f",8:=g} =
	id(#{1=>a,2=>b,3=>"c","4"=>"d",<<"5">>=><<"e">>,{"6",7}=>"f",8=>g}),

    #{<<"hi all">> := 1} = id(#{<<"hi",32,"all">> => 1}),

    #{a:=X,a:=X=3,b:=4} = id(#{a=>3,b=>4}), % weird but ok =)

    #{ a:=#{ b:=#{c := third, b:=second}}, b:=first} =
	id(#{ b=>first, a=>#{ b=>#{c => third, b=> second}}}),

    M = #{ map_1=>#{ map_2=>#{value_3 => third}, value_2=> second}, value_1=>first},
    M = #{ map_1:=#{ map_2:=#{value_3 := third}, value_2:= second}, value_1:=first} =
	 id(#{ map_1=>#{ map_2=>#{value_3 => third}, value_2=> second}, value_1=>first}),

    %% nil key
    #{[]:=ok,1:=2} = id(#{[]=>ok,1=>2}),

    %% error case
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3,x:=2} = id(#{x=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=2} = id(#{x=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3} = id({a,b,c}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3} = id(#{y=>3}))),
    {'EXIT',{{badmatch,_},_}} = (catch (#{x:=3} = id(#{x=>"three"}))),
    ok.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
