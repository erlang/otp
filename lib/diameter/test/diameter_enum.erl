%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(diameter_enum).

%%
%% This module constructs finite enumerations.
%%
%% An enumeration is represented as a function on integers, 0 mapping
%% to the number of values enumerated and successive integers mapping
%% to enumerated values. The function will fail on anything but 0 and
%% positive integers less then or equal to the value of the function
%% at 0.
%%
%% The purpose of this is to provide a way of stepping through a large
%% number of values without explicitly constructing the list of all
%% possible values. For example, consider the following function that
%% given a list of lists constructs the list of all possible lists
%% constructed by choosing one element from each sublist.
%%
%%   combine([H]) ->
%%       [[X] || X <- H];
%%   combine([H|T]) ->
%%       Ys = combine(T),
%%       [[X|Y] || X <- H, Y <- Ys].
%%
%% Eg. [[1,2],[3,4,5]] -> [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5]]
%%
%% If L is a list of three 1000 element lists then combine(L) would
%% construct a list of length 10^9 which will likely exhaust available
%% memory. (Which is how this module came into being. A tail-recursive
%% implementation doesn't fare much better.) By contrast,
%%
%%   F = enum:combine([enum:new(L) || L <- Lists])
%%
%% only maps existing lists. It may still be undesirable to step
%% through a very large number of values but it's possible, and easy
%% to step through a selection of values as an alternative.
%%

%% Functions that return enumerations.
-export([new/1,
         combine/1,
         reverse/1,
         map/2,
         append/1,
         duplicate/2,
         nthtail/2,
         seq/2,
         seq/3,
         zip/1,
         zip/2,
         slice/3,
         split/2]).

%% Functions that operate on existing enumerations.
-export([foreach/2,
         foldl/3,
         foldr/3,
         all/2,
         any/2,
         member/2,
         last/1,
         nth/2,
         to_list/1]).

%% ------------------------------------------------------------------------
%% new/1
%%
%% Turn a list/tuple of values into an enumeration that steps through
%% each element. Turn anything else into an enumeration of that single
%% value.
%% ------------------------------------------------------------------------

new(L)
  when is_list(L) ->
    new(list_to_tuple(L));

new(T)
  when is_tuple(T) ->
    enum(size(T), fun(N) -> element(N,T) end);

new(T) ->
    fun(0) -> 1; (1) -> T end.

enum(Ord, F) ->
    fun(0) -> Ord; (N) when 0 < N, N =< Ord -> F(N) end.

%% ------------------------------------------------------------------------
%% combine/1
%%
%% Map a list/tuple of enumerations to the enumeration of all
%% lists/tuples constructed by choosing one value from each
%% enumeration in the list/tuple.
%% ------------------------------------------------------------------------

combine(T)
  when is_tuple(T) ->
    F = combine(tuple_to_list(T)),
    enum(F(0), fun(N) -> list_to_tuple(F(N)) end);

combine([]) ->
    fun(0) -> 0 end;

%% Given positive integers n_1,...,n_k, construct a bijection from
%% {0,...,\prod_{i=1}^k} n_i - 1} to {0,...,n_1} x ... x {0,...,n_k}
%% that maps N to (N_1,...,N_k) where:
%%
%%   N_1 = (N div 1) rem n_1
%%   ...
%%   N_k = (N div n_1*...*n_{k-1}) rem n_k
%%
%% That is:
%%
%%   N_i = (N div \prod_{j=1}^{i-1} n_j) rem n_i
%%
%% This corresponds to looping through N_1, incrementing N_2 as N_1
%% loops, and so on up through N_k. The inverse map is as follows.
%%
%%   (N_1,...,N_k) -> N = N_1 + N_2*n_1 + ... + N_k*n_{k-1}*...*n_1
%%
%%                      = \sum_{i=1}^k N_i*\prod_{j=i}^{i-1} n_j
%%
%% [Proof: Induction on k. For k=1 we have the identity map. If
%%         g_k : (N_1,...,N_k) |-> N above is bijective then consider
%%         the bijection
%%
%%           G : (t,n) |--> t + n*K,  K = n_k*...*n_1
%%
%%         from {0,...,K-1} x {0,...,n_{k+1}-1} onto {0,...,n_{k+1}*K - 1}
%%         with inverse F : n |--> (n rem K, n div K). Since
%%
%%           g_{k+1}(N_1,...,N_{k+1}) = g_k(N_1,...,N_K) + N_{k+1}*K
%%                                    = G(g_k(N_1,...,N_K), N_{k+1})
%%
%%         and G, g_k and ((N-1,...,N_k),N_{k+1}) -> (N_1,...,N_{k+1})
%%         are all bijections, so is g_{k+1}.]

combine([_|_] = L) ->
    [Ord | Divs] = lists:foldl(fun(F,[D|_] = A) -> [F(0)*D | A] end, [1], L),
    RL = lists:reverse(L),
    enum(Ord, fun(N) -> combine(N, Ord, Divs, RL) end).

%% Since we use 0 to return the number of elements enumerated, use
%% bijections from {1,...,N} rather than {0,...,N-1}.

combine(N, Ord, Divs, L)
  when 0 < N, N =< Ord ->
    {Vs, []} = lists:foldl(fun(F, {A, [D|Ds]}) ->
                                   {[F(1 + (((N-1) div D) rem F(0))) | A], Ds}
                           end,
                           {[], Divs},
                           L),
    Vs.

%% ------------------------------------------------------------------------
%% reverse/1
%%
%% Construct the enumeration that reverses the order in which values
%% are traversed.
%% ------------------------------------------------------------------------

reverse(E) ->
    Ord = E(0),
    enum(Ord, fun(N) -> E(Ord + 1 - N) end).

%% ------------------------------------------------------------------------
%% map/2
%%
%% Construct an enumeration that maps enumerated values.
%% ------------------------------------------------------------------------

map(Fun, E) ->
    enum(E(0), fun(N) -> Fun(E(N)) end).

%% ------------------------------------------------------------------------
%% append/2
%%
%% Construct an enumeration that successively steps through each of a
%% list of enumerations.
%% ------------------------------------------------------------------------

append(Es) ->
    [Ord | Os] = lists:foldl(fun(E, [N|_] = A) -> [N+E(0)|A] end, [0], Es),
    Rev = lists:reverse(Es),
    enum(Ord, fun(N) -> append(N, Os, Rev) end).

append(N, [Ord | _], [E | _])
  when N > Ord ->
    E(N - Ord);
append(N, [_|Os], [_|Es]) ->
    append(N, Os, Es).

%% ------------------------------------------------------------------------
%% duplicate/2
%%
%% Construct an enumeration that traverses an enumeration multiple
%% times. Equivalent to append(lists:duplicate(N, E)).
%% ------------------------------------------------------------------------

duplicate(N, E) ->
    Ord = E(0),
    enum(N*Ord, fun(M) -> E(1 + ((M-1) rem Ord)) end).

%% ------------------------------------------------------------------------
%% nthtail/2
%%
%% Construct an enumeration that omits values at the head of an
%% existing enumeration.
%% ------------------------------------------------------------------------

nthtail(N, E)
  when 0 =< N ->
    nthtail(E(0) - N, N, E).

nthtail(Ord, N, E)
  when 0 =< Ord ->
    enum(Ord, fun(M) -> E(M+N) end).

%% ------------------------------------------------------------------------
%% seq/[23]
%%
%% Construct an enumeration that steps through a sequence of integers.
%% ------------------------------------------------------------------------

seq(From, To) ->
    seq(From, To, 1).

seq(From, To, Incr)
  when From =< To ->
    enum((To - From + Incr) div Incr, fun(N) -> From + (N-1)*Incr end).

%% ------------------------------------------------------------------------
%% zip/[12]
%%
%% Construct an enumeration whose nth value is the list of nth values
%% of a list of enumerations.
%% ------------------------------------------------------------------------

zip(Es) ->
    zip(fun(T) -> T end, Es).

zip(_, []) ->
    [];
zip(Fun, Es) ->
    enum(lists:min([E(0) || E <- Es]), fun(N) -> Fun([E(N) || E <- Es]) end).

%% ------------------------------------------------------------------------
%% slice/3
%%
%% Construct an enumeration of a given length from a given starting point.
%% ------------------------------------------------------------------------

slice(N, Len, E)
  when is_integer(N), N > 0, is_integer(Len), Len >= 0 ->
    slice(N, Len, E(0) - (N - 1), E).

slice(_, _, Tail, _)
  when Tail < 1 ->
    fun(0) -> 0 end;

slice(N, Len, Tail, E) ->
    enum(lists:min([Len, Tail]), fun(M) -> E(N-1+M) end).

%% ------------------------------------------------------------------------
%% split/2
%%
%% Split an enumeration into a list of enumerations of the specified
%% length. The last enumeration of the list may have order less than
%% this length.
%% ------------------------------------------------------------------------

split(Len, E)
  when is_integer(Len), Len > 0 ->
    split(1, E(0), Len, E, []).

split(N, Ord, _, _, Acc)
  when N > Ord ->
    lists:reverse(Acc);

split(N, Ord, Len, E, Acc) ->
    split(N+Len, Ord, Len, E, [slice(N, Len, E) | Acc]).

%% ------------------------------------------------------------------------
%% foreach/2
%%
%% Apply a fun to each value of an enumeration.
%% ------------------------------------------------------------------------

foreach(Fun, E) ->
    foldl(fun(N,ok) -> Fun(N), ok end, ok, E).

%% ------------------------------------------------------------------------
%% foldl/3
%% foldr/3
%%
%% Fold through values in an enumeration.
%% ------------------------------------------------------------------------

foldl(Fun, Acc, E) ->
    foldl(E(0), 1, Fun, Acc, E).

foldl(M, N, _, Acc, _)
  when N == M+1 ->
    Acc;
foldl(M, N, Fun, Acc, E) ->
    foldl(M, N+1, Fun, Fun(E(N), Acc), E).

foldr(Fun, Acc, E) ->
    foldl(Fun, Acc, reverse(E)).

%% ------------------------------------------------------------------------
%% all/2
%%
%% Do all values of an enumeration satisfy a predicate?
%% ------------------------------------------------------------------------

all(Pred, E) ->
    all(E(0), 1, Pred, E).

all(M, N, _, _)
  when N == M+1 ->
    true;
all(M, N, Pred, E) ->
    Pred(E(N)) andalso all(M, N+1, Pred, E).

%% Note that andalso/orelse are tail-recusive as of R13A.

%% ------------------------------------------------------------------------
%% any/2
%%
%% Does any value of an enumeration satisfy a predicate?
%% ------------------------------------------------------------------------

any(Pred, E) ->
    any(E(0), 1, Pred, E).

any(M, N, _, _)
  when N == M+1 ->
    false;
any(M, N, Pred, E) ->
    Pred(E(N)) orelse any(M, N+1, Pred, E).

%% ------------------------------------------------------------------------
%% member/2
%%
%% Does a value match any in an enumeration?
%% ------------------------------------------------------------------------

member(X, E) ->
    member(E(0), 1, X, E).

member(M, N, _, _)
  when N == M+1 ->
    false;
member(M, N, X, E) ->
    match(E(N), X) orelse member(M, N+1, X, E).

match(X, X) ->
    true;
match(_, _) ->
    false.

%% ------------------------------------------------------------------------
%% last/1
%%
%% Return the last value of an enumeration.
%% ------------------------------------------------------------------------

last(E) ->
    E(E(0)).

%% ------------------------------------------------------------------------
%% nth/2
%%
%% Return a selected value of an enumeration.
%% ------------------------------------------------------------------------

nth(N, E) ->
    E(N).

%% ------------------------------------------------------------------------
%% to_list/1
%%
%% Turn an enumeration into a list. Not good if the very many values
%% are enumerated.
%% ------------------------------------------------------------------------

to_list(E) ->
    foldr(fun(X,A) -> [X|A] end, [], E).
