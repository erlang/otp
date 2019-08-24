-module(para3).

-export([t/0, t1/1, t2/0, ot1/1, ot2/0, t1_adt/0, t2_adt/0]).

-export([exp_adt/0]).

%% More opaque tests.

-export_type([ot1/0, ot1/1, ot1/2, ot1/3, ot1/4, ot1/5]).

-opaque ot1() :: {ot1(_)}.

-opaque ot1(A) :: {ot1(A, A)}.

-opaque ot1(A, B) :: {ot1(A, B, A)}.

-opaque ot1(A, B, C) :: {ot1(A, B, C, A)}.

-opaque ot1(A, B, C, D) :: {ot1(A, B, C, D, A)}.

-opaque ot1(A, B, C, D, E) :: {A, B, C, D, E}.

-spec ot1(_) -> ot1().

ot1(A) ->
    {{{{{A, A, A, A, A}}}}}.

-spec ot2() -> ot1(). % invalid type spec

ot2() ->
    foo.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t() ->
    {{{17}}} = t1(3). %% pattern can never match

-type t1() :: {t1(_)}.

-type t1(A) :: {t1(A, A)}.

-type t1(A, B) :: {t1(A, B, A)}.

-type t1(A, B, C) :: {t1(A, B, C, A)}.

-type t1(A, B, C, D) :: {t1(A, B, C, D, A)}.

-type t1(A, B, C, D, E) :: {A, B, C, D, E}.

-spec t1(_) -> t1().

t1(A) ->
    {{{{{A, A, A, A, A}}}}}.

-spec t2() -> t1(). % invalid type spec

t2() ->
    foo.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Shows that the list TypeNames in t_from_form must include ArgsLen.

t1_adt() ->
    {{{{{17}}}}} = para3_adt:t1(3). % breaks the opacity

t2_adt() ->
    {{{{17}}}} = para3_adt:t1(3). % can never match

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type exp() :: para3_adt:exp1(para3_adt:exp2()).

-spec exp_adt() -> exp(). % invalid type spec

exp_adt() ->
    3.
