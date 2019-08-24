-module(eep37).

-compile(export_all).

-spec self() -> fun(() -> fun()).
self() ->
    fun Self() -> Self end.

-spec fact() -> fun((non_neg_integer()) -> non_neg_integer()).
fact() ->
    fun Fact(N) when N > 0 ->
            N * Fact(N - 1);
        Fact(0) ->
            1
    end.
