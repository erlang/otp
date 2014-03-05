-module(para3_adt).

-export([t1/1]).

-export_type([t1/0, t1/1, t1/2, t1/3, t1/4, ot1/5]).

-export_type([exp1/1, exp2/0]).

-type t1() :: {t1(_)}.

-type t1(A) :: {t1(A, A)}.

-type t1(A, B) :: {t1(A, B, A)}.

-type t1(A, B, C) :: {t1(A, B, C, A)}.

-type t1(A, B, C, D) :: {ot1(A, B, C, D, A)}.

-opaque ot1(A, B, C, D, E) :: {A, B, C, D, E}.

-spec t1(_) -> t1().

t1(A) ->
    {{{{{A, A, A, A, A}}}}}.

-opaque exp1(T) :: T.
-opaque exp2() :: integer().
