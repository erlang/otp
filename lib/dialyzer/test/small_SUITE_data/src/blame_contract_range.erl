%%-----------------------------------------------------------------------
%% A test where the contract is wrongly specified by the programmer;
%% however, this is found only by refinement.
%% Dialyzer in R14B01 and prior gave a confusing (if not bogus) warning
%% for this case.  Corrected in R14B02.
%%-----------------------------------------------------------------------
-module(blame_contract_range).

-export([foo/0]).

foo() ->
  bar(b).

-spec bar(atom()) -> a.
bar(a) -> a;
bar(b) -> b.
