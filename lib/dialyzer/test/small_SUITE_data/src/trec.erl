%%
%% The current treatment of typed records leaves much to be desired.
%% These are not made up examples; I have cases like that the branch
%% of the HiPE compiler with types in records. I get very confusing
%% warnings which require a lot of effort to find their cause and why
%% a function has no local return.
%%
-module(trec).
-export([test/0, mk_foo_exp/2]).

-record(foo, {a :: integer(), b :: [atom()]}).

%%
%% For these functions we currently get the following warnings:
%%   1. Function test/0 has no local return
%%   2. The call trec:mk_foo_loc(42,any()) will fail since it differs
%%      in argument position 1 from the success typing arguments:
%%      ('undefined',atom())
%%   3. Function mk_foo_loc/2 has no local return
%%
%% Arguably, the second warning is not what most users have in mind
%% when they wrote the type declarations in the 'foo' record, so no
%% doubt they'll find it confusing. But note that it is also inconsistent!
%% How come there is a success typing for a function that has no local return?
%%
test() ->
   mk_foo_loc(42, bar:f()).

mk_foo_loc(A, B) ->
    #foo{a = A, b = [A,B]}.

%%
%% For this function we currently get "has no local return" but we get
%% no reason; I want us to get a reason.
%%
mk_foo_exp(A, B) when is_integer(A) ->
    #foo{a = A, b = [A,B]}.
