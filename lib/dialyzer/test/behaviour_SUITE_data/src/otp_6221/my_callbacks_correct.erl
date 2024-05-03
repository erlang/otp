-module(my_callbacks_correct).

-behaviour(my_behaviour).

-export([foo/0]).

-type pair(A,B) :: {A,B}.

-type nested() :: pair(pair(pair(f,f),f),f).

%% This is correctly implemented, but a combination of Dialyzer
%% "simplification" logic and subtyping rules for behaviours means
%% this implementation has historically been erroneously rejected
-spec foo() -> #{ nested() => x }.
foo() ->
    Ret = #{ {{{f,f}, f}, f} => x },
      Ret.
