-module(trycatch_1).
-export([one_try_catch/1,one_plain_catch/1]).

one_try_catch(Term) ->
    try
        trycatch_2:two(Term)
    catch
        C:R:Stk ->
            {C,R,Stk}
    end.

one_plain_catch(Term) ->
    catch trycatch_2:two(Term).
