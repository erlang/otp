-module(maps_build_and_match_val).
-export([test/0]).

test() ->
    F = fun
	(#{ "hi" := first,  v := V}) -> {1,V};
	(#{ "hi" := second, v := V}) -> {2,V}
    end,


    {1,"hello"}  = F(id(#{"hi"=>first,v=>"hello"})),
    {2,"second"} = F(id(#{"hi"=>second,v=>"second"})),

    %% error case
    case (catch (F(id(#{"hi"=>ok})))) of
	{'EXIT',{function_clause,_}} -> ok;
	{'EXIT', {{case_clause,_},_}} -> {comment,inlined};
	Other ->
	    test_server:fail({no_match, Other})
    end.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.
