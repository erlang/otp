%%---------------------------------------------------------------------
%% A test for which the analysis gave wrong results due to erroneous
%% specialization and incorrect handling of unions.
%%---------------------------------------------------------------------

-module(opaque_bug4).

-export([ok/0, wrong/0]).

%-spec ok() -> 'ok'.
ok() ->
   L = opaque_adt:atom_or_list(42),
   foo(L).

%-spec wrong() -> 'not_ok'.
wrong() ->
   A = opaque_adt:atom_or_list(1),
   foo(A).

foo(a) -> not_ok;
foo([_|_]) -> ok.
