%%===========================================================================
%% Test that made dialyzer go into an infinite loop. The reason was that
%% t_inf(t_unit(), t_none()) returned t_unit() instead of t_none() as it
%% should. The issue was identified and fixed by Stavros Aronis on 5/11/2010.
%%===========================================================================
-module(none_scc_inf_loop).

-export([foo/0]).

foo() ->
    foo(3).

foo(0) ->
    exit(foo);
foo(N) ->
    bar(N-1).

bar(0) ->
    exit(foo);
bar(N) ->
    foo(N-1).
