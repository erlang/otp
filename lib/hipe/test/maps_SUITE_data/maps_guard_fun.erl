-module(maps_guard_fun).
-export([test/0]).

test() ->
    F1 = fun
	    (#{s:=v,v:=V})     -> {v,V};
	    (#{s:=t,v:={V,V}}) -> {t,V};
	    (#{s:=l,v:=[V,V]}) -> {l,V}
    end,

    F2 = fun
	    (#{s:=T,v:={V,V}}) -> {T,V};
	    (#{s:=T,v:=[V,V]}) -> {T,V};
	    (#{s:=T,v:=V})     -> {T,V}
    end,
    V = <<"hi">>,

    {v,V} = F1(#{s=>v,v=>V}),
    {t,V} = F1(#{s=>t,v=>{V,V}}),
    {l,V} = F1(#{s=>l,v=>[V,V]}),

    {v,V} = F2(#{s=>v,v=>V}),
    {t,V} = F2(#{s=>t,v=>{V,V}}),
    {l,V} = F2(#{s=>l,v=>[V,V]}),

    %% error case
    case (catch F1(#{s=>none,v=>none})) of
	{'EXIT', {function_clause,[{?MODULE,_,[#{s:=none,v:=none}],_}|_]}} -> ok;
	{'EXIT', {function_clause,[{?MODULE,_,1,[#{s:=none,v:=none}]}|_]}} -> ok;
	{'EXIT', {function_clause,[Frame|_]}}
	  when is_tuple(Frame), element(1, Frame) =:= ?MODULE ->
	    test_server:comment("Unexpected trace format, probably using HiPE");
	{'EXIT', {{case_clause,_},_}} -> {comment,inlined};
	Other ->
	    test_server:fail({no_match, Other})
    end.
