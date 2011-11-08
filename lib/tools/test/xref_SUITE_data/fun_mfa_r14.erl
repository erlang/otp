-module(fun_mfa_r14).

-export([t/0, t1/0, t2/0, t3/0]).

t() ->
    F = fun ?MODULE:t/0,
    (F)().

t1() ->
    F = fun t/0,
    (F)().

t2() ->
    fun ?MODULE:t/0().

t3() ->
    fun t3/0().
             
