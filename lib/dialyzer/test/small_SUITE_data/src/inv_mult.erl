%% Dialyzer was too constraining when checking the relation between the
%% arguments and result of a multiplication. We should not constrain an argument
%% if the other operand *may* be zero.
%%
%% Bug found by Kostis Sagonas, fixed by Stavros Aronis

-module(inv_mult).
-compile(export_all).

main(L) ->
    N = -1 * length(L),
    fact(N).

fact(0) -> 1;
fact(N) -> N * fact(N-1).
