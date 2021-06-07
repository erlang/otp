-module(guard_update).

-export([t/0, t2/0]).

t() ->
    f(#{a=>2}). %% Illegal

f(M)
  when M#{b := 7} =/= q
       -> ok.

t2() ->
    f2(#{a=>2}). %% Legal!

f2(M)
  when M#{b := 7} =/= q;
       M =/= p
       -> ok.
