-module(maps_build_and_match_empty_val).
-export([test/0]).

test() ->
    F = fun(#{ "hi":=_,{1,2}:=_,1337:=_}) -> ok end,
    ok = F(id(#{"hi"=>ok,{1,2}=>ok,1337=>ok})),

    %% error case
    case (catch (F(id(#{"hi"=>ok})))) of
	{'EXIT',{function_clause,_}} -> ok;
	{'EXIT', {{case_clause,_},_}} -> {comment,inlined};
	Other ->
	    test_server:fail({no_match, Other})
    end.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
