%%
%% A couple of tests for booleans in guards.
%% Tests with suffix w have incomplete results due to weak dataflow.
%% Tests with suffix ww have incomplete results due to weak dialyzer.
%% Tests with suffix x should not give warnings.
%%

-module(and_bug).

-compile(export_all).

test1(X) when X and not X -> never.

test2(X) when not X and X -> never.

test3(X) when (X and not X) =:= true -> never.

test4(X) when (not X and X) =:= true -> never.

test5(X) when (X and not X) == true -> never.

test6(X) when (not X and X) == true -> never.

test7_w(X) when not (X or not X) -> never.

test8_w(X) when not (not X or X) -> never.

test9(X) when (X or not X) =:= false -> never.

test10(X) when (not X or X) =:= false -> never.

test11(X) when (X or not X) == false -> never.

test12(X) when (not X or X) == false -> never.

test13(X) when X and false -> never.

test14(X) when false and X -> never.

test15(X) when (X and false) =:= true -> never.

test16(X) when (false and X) =:= true -> never.

test17(X) when (X and false) == true -> never.

test18(X) when (false and X) == true -> never.

test19(X) when not (true or X) -> never.

test20(X) when not (X or true) -> never.

test21(X) when (true or X) =:= false -> never.

test22(X) when (X or true) =:= false -> never.

test23(X) when (true or X) == false -> never.

test24(X) when (X or true) == false -> never.

test25(X) when (false and X) -> never.

test26(X) when (X and false) -> never.

test27(X) when (false and X) =:= true -> never.

test28(X) when (X and false) =:= true -> never.

test29(X) when (false and X) == true -> never.

test30(X) when (X and false) == true -> never.

test31() when false and false -> never.

test32() when (false and false) =:= true -> never.

test33() when not (true and true) =:= true -> never.

test34() when (false and false) == true -> never.

test35() when not (true and true) == true -> never.

test36() when false or false -> never.

test37() when (false or false) =:= true -> never.

test38() when not (false or false) =:= false -> never.

test39() when (false or false) == true -> never.

test40() when not (false or false) == false -> never.

test41() when true =:= false -> never.

test42() when true == false -> never.

test43() when not (true =:= true) -> never.

test44() when not (true == true) -> never.

test45() when not (not (not (not (not (not (not true)))))) -> never.

test46(X) when (X =:= true) and (X =:= false) -> never.

test47(X) when (X == true) and (X == false) -> never.

test48(X) when is_boolean(X) and (X =:= true) and (X =/= true) -> never.

test49_x(X) when not (X or X) -> maybe.

test50_x(X) when not (X and X) -> maybe.

test51_x(X) when not (not X) -> maybe.

test52_w(X) when is_boolean(X) and (X =/= true) and (X =:= true) -> never.

test53_ww(X) when is_boolean(X) and (X =/= true) and (X =/= false) -> never.

test54_w(X) when is_boolean(X) and not ((X =:= true) or (X =:= false)) -> never.
