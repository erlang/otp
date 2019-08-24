%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(lists).

-compile({no_auto_import,[max/2]}).
-compile({no_auto_import,[min/2]}).

-export([append/2, append/1, subtract/2, reverse/1,
	 nth/2, nthtail/2, prefix/2, suffix/2, droplast/1, last/1,
	 seq/2, seq/3, sum/1, duplicate/2, min/1, max/1, sublist/2, sublist/3,
	 delete/2,
	 unzip/1, unzip3/1, zip/2, zip3/3, zipwith/3, zipwith3/4,
	 sort/1, merge/1, merge/2, rmerge/2, merge3/3, rmerge3/3,
	 usort/1, umerge/1, umerge3/3, umerge/2, rumerge3/3, rumerge/2,
	 concat/1, flatten/1, flatten/2, flatlength/1,
	 keydelete/3, keyreplace/4, keytake/3, keystore/4,
	 keysort/2, keymerge/3, rkeymerge/3, rukeymerge/3, 
	 ukeysort/2, ukeymerge/3, keymap/3]).

-export([merge/3, rmerge/3, sort/2, umerge/3, rumerge/3, usort/2]).

-export([all/2,any/2,map/2,flatmap/2,foldl/3,foldr/3,filter/2,
	 partition/2,zf/2,filtermap/2,
	 mapfoldl/3,mapfoldr/3,foreach/2,takewhile/2,dropwhile/2,
         search/2, splitwith/2,split/2,
	 join/2]).

%%% BIFs
-export([keyfind/3, keymember/3, keysearch/3, member/2, reverse/2]).

%% Shadowed by erl_bif_types: lists:keyfind/3
-spec keyfind(Key, N, TupleList) -> Tuple | false when
      Key :: term(),
      N :: pos_integer(),
      TupleList :: [Tuple],
      Tuple :: tuple().

keyfind(_, _, _) ->
    erlang:nif_error(undef).

%% Shadowed by erl_bif_types: lists:keymember/3
-spec keymember(Key, N, TupleList) -> boolean() when
      Key :: term(),
      N :: pos_integer(),
      TupleList :: [Tuple],
      Tuple :: tuple().

keymember(_, _, _) ->
    erlang:nif_error(undef).

%% Shadowed by erl_bif_types: lists:keysearch/3
-spec keysearch(Key, N, TupleList) -> {value, Tuple} | false when
      Key :: term(),
      N :: pos_integer(),
      TupleList :: [Tuple],
      Tuple :: tuple().

keysearch(_, _, _) ->
    erlang:nif_error(undef).

%% Shadowed by erl_bif_types: lists:member/2
-spec member(Elem, List) -> boolean() when
      Elem :: T,
      List :: [T],
      T :: term().

member(_, _) ->
    erlang:nif_error(undef).

%% Shadowed by erl_bif_types: lists:reverse/2
-spec reverse(List1, Tail) -> List2 when
      List1 :: [T],
      Tail :: term(),
      List2 :: [T],
      T :: term().

reverse(_, _) ->
    erlang:nif_error(undef).

%%% End of BIFs

%% member(X, L) -> (true | false)
%%  test if X is a member of the list L
%%  Now a BIF!

%member(X, [X|_]) -> true;
%member(X, [_|Y]) ->
%	member(X, Y);
%member(X, []) -> false.

%% append(X, Y) appends lists X and Y

-spec append(List1, List2) -> List3 when
      List1 :: [T],
      List2 :: [T],
      List3 :: [T],
      T :: term().

append(L1, L2) -> L1 ++ L2.

%% append(L) appends the list of lists L

-spec append(ListOfLists) -> List1 when
      ListOfLists :: [List],
      List :: [T],
      List1 :: [T],
      T :: term().

append([E]) -> E;
append([H|T]) -> H ++ append(T);
append([]) -> [].

%% subtract(List1, List2) subtract elements in List2 form List1.

-spec subtract(List1, List2) -> List3 when
      List1 :: [T],
      List2 :: [T],
      List3 :: [T],
      T :: term().

subtract(L1, L2) -> L1 -- L2.

%% reverse(L) reverse all elements in the list L. reverse/2 is now a BIF!

-spec reverse(List1) -> List2 when
      List1 :: [T],
      List2 :: [T],
      T :: term().

reverse([] = L) ->
    L;
reverse([_] = L) ->
    L;
reverse([A, B]) ->
    [B, A];
reverse([A, B | L]) ->
    lists:reverse(L, [B, A]).

%reverse([H|T], Y) ->
%    reverse(T, [H|Y]);
%reverse([], X) -> X.


%% nth(N, L) returns the N`th element of the list L
%% nthtail(N, L) returns the N`th tail of the list L

-spec nth(N, List) -> Elem when
      N :: pos_integer(),
      List :: [T,...],
      Elem :: T,
      T :: term().

nth(1, [H|_]) -> H;
nth(N, [_|T]) when N > 1 ->
    nth(N - 1, T).

-spec nthtail(N, List) -> Tail when
      N :: non_neg_integer(),
      List :: [T,...],
      Tail :: [T],
      T :: term().

nthtail(1, [_|T]) -> T;
nthtail(N, [_|T]) when N > 1 ->
    nthtail(N - 1, T);
nthtail(0, L) when is_list(L) -> L.

%% prefix(Prefix, List) -> (true | false)

-spec prefix(List1, List2) -> boolean() when
      List1 :: [T],
      List2 :: [T],
      T :: term().

prefix([X|PreTail], [X|Tail]) ->
    prefix(PreTail, Tail);
prefix([], List) when is_list(List) -> true;
prefix([_|_], List) when is_list(List) -> false.

%% suffix(Suffix, List) -> (true | false)

-spec suffix(List1, List2) -> boolean() when
      List1 :: [T],
      List2 :: [T],
      T :: term().

suffix(Suffix, List) ->
    Delta = length(List) - length(Suffix),
    Delta >= 0 andalso nthtail(Delta, List) =:= Suffix.

%% droplast(List) returns the list dropping its last element

-spec droplast(List) -> InitList when
      List :: [T, ...],
      InitList :: [T],
      T :: term().

%% This is the simple recursive implementation
%% reverse(tl(reverse(L))) is faster on average,
%% but creates more garbage.
droplast([_T])  -> [];
droplast([H|T]) -> [H|droplast(T)].

%% last(List) returns the last element in a list.

-spec last(List) -> Last when
      List :: [T,...],
      Last :: T,
      T :: term().

last([E|Es]) -> last(E, Es).

last(_, [E|Es]) -> last(E, Es);
last(E, []) -> E.

%% seq(Min, Max) -> [Min,Min+1, ..., Max]
%% seq(Min, Max, Incr) -> [Min,Min+Incr, ..., Max]
%%  returns the sequence Min..Max
%%  Min <= Max and Min and Max must be integers

-spec seq(From, To) -> Seq when
      From :: integer(),
      To :: integer(),
      Seq :: [integer()].

seq(First, Last)
    when is_integer(First), is_integer(Last), First-1 =< Last -> 
    seq_loop(Last-First+1, Last, []).

seq_loop(N, X, L) when N >= 4 ->
     seq_loop(N-4, X-4, [X-3,X-2,X-1,X|L]);
seq_loop(N, X, L) when N >= 2 ->
     seq_loop(N-2, X-2, [X-1,X|L]);
seq_loop(1, X, L) ->
     [X|L];
seq_loop(0, _, L) ->
     L.

-spec seq(From, To, Incr) -> Seq when
      From :: integer(),
      To :: integer(),
      Incr :: integer(),
      Seq :: [integer()].

seq(First, Last, Inc) 
    when is_integer(First), is_integer(Last), is_integer(Inc) -> 
    if
        Inc > 0, First - Inc =< Last;
        Inc < 0, First - Inc >= Last ->
            N = (Last - First + Inc) div Inc,
            seq_loop(N, Inc*(N-1)+First, Inc, []);
        Inc =:= 0, First =:= Last ->
            seq_loop(1, First, Inc, [])
    end.

seq_loop(N, X, D, L) when N >= 4 ->
     Y = X-D, Z = Y-D, W = Z-D,
     seq_loop(N-4, W-D, D, [W,Z,Y,X|L]);
seq_loop(N, X, D, L) when N >= 2 ->
     Y = X-D,
     seq_loop(N-2, Y-D, D, [Y,X|L]);
seq_loop(1, X, _, L) ->
     [X|L];
seq_loop(0, _, _, L) ->
     L.

%% sum(L) returns the sum of the elements in L

-spec sum(List) -> number() when
      List :: [number()].

sum(L)          -> sum(L, 0).

sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum.

%% duplicate(N, X) -> [X,X,X,.....,X]  (N times)
%%   return N copies of X

-spec duplicate(N, Elem) -> List when
      N :: non_neg_integer(),
      Elem :: T,
      List :: [T],
      T :: term().

duplicate(N, X) when is_integer(N), N >= 0 -> duplicate(N, X, []).

duplicate(0, _, L) -> L;
duplicate(N, X, L) -> duplicate(N-1, X, [X|L]).

%% min(L) -> returns the minimum element of the list L

-spec min(List) -> Min when
      List :: [T,...],
      Min :: T,
      T :: term().

min([H|T]) -> min(T, H).

min([H|T], Min) when H < Min -> min(T, H);
min([_|T], Min)              -> min(T, Min);
min([],    Min)              -> Min. 

%% max(L) -> returns the maximum element of the list L

-spec max(List) -> Max when
      List :: [T,...],
      Max :: T,
      T :: term().

max([H|T]) -> max(T, H).

max([H|T], Max) when H > Max -> max(T, H);
max([_|T], Max)              -> max(T, Max);
max([],    Max)              -> Max.

%% sublist(List, Start, Length)
%%  Returns the sub-list starting at Start of length Length.

-spec sublist(List1, Start, Len) -> List2 when
      List1 :: [T],
      List2 :: [T],
      Start :: pos_integer(),
      Len :: non_neg_integer(),
      T :: term().

sublist(List, S, L) when is_integer(L), L >= 0 ->
    sublist(nthtail(S-1, List), L).

-spec sublist(List1, Len) -> List2 when
      List1 :: [T],
      List2 :: [T],
      Len :: non_neg_integer(),
      T :: term().

sublist(List, L) when is_integer(L), is_list(List) ->
    sublist_2(List, L).

sublist_2([H|T], L) when L > 0 ->
    [H|sublist_2(T, L-1)];
sublist_2(_, 0) ->
    [];
sublist_2(List, L) when is_list(List), L > 0 ->
    [].

%% delete(Item, List) -> List'
%%  Delete the first occurrence of Item from the list L.

-spec delete(Elem, List1) -> List2 when
      Elem :: T,
      List1 :: [T],
      List2 :: [T],
      T :: term().

delete(Item, [Item|Rest]) -> Rest;
delete(Item, [H|Rest]) -> 
    [H|delete(Item, Rest)];
delete(_, []) -> [].

%% Return [{X0, Y0}, {X1, Y1}, ..., {Xn, Yn}] for lists [X0, X1, ...,
%% Xn] and [Y0, Y1, ..., Yn].

-spec zip(List1, List2) -> List3 when
      List1 :: [A],
      List2 :: [B],
      List3 :: [{A, B}],
      A :: term(),
      B :: term().

zip([X | Xs], [Y | Ys]) -> [{X, Y} | zip(Xs, Ys)];
zip([], []) -> [].

%% Return {[X0, X1, ..., Xn], [Y0, Y1, ..., Yn]}, for a list [{X0, Y0},
%% {X1, Y1}, ..., {Xn, Yn}].

-spec unzip(List1) -> {List2, List3} when
      List1 :: [{A, B}],
      List2 :: [A],
      List3 :: [B],
      A :: term(),
      B :: term().

unzip(Ts) -> unzip(Ts, [], []).

unzip([{X, Y} | Ts], Xs, Ys) -> unzip(Ts, [X | Xs], [Y | Ys]);
unzip([], Xs, Ys) -> {reverse(Xs), reverse(Ys)}.

%% Return [{X0, Y0, Z0}, {X1, Y1, Z1}, ..., {Xn, Yn, Zn}] for lists [X0,
%% X1, ..., Xn], [Y0, Y1, ..., Yn] and [Z0, Z1, ..., Zn].

-spec zip3(List1, List2, List3) -> List4 when
      List1 :: [A],
      List2 :: [B],
      List3 :: [C],
      List4 :: [{A, B, C}],
      A :: term(),
      B :: term(),
      C :: term().

zip3([X | Xs], [Y | Ys], [Z | Zs]) -> [{X, Y, Z} | zip3(Xs, Ys, Zs)];
zip3([], [], []) -> [].

%% Return {[X0, X1, ..., Xn], [Y0, Y1, ..., Yn], [Z0, Z1, ..., Zn]}, for
%% a list [{X0, Y0, Z0}, {X1, Y1, Z1}, ..., {Xn, Yn, Zn}].

-spec unzip3(List1) -> {List2, List3, List4} when
      List1 :: [{A, B, C}],
      List2 :: [A],
      List3 :: [B],
      List4 :: [C],
      A :: term(),
      B :: term(),
      C :: term().

unzip3(Ts) -> unzip3(Ts, [], [], []).

unzip3([{X, Y, Z} | Ts], Xs, Ys, Zs) ->
    unzip3(Ts, [X | Xs], [Y | Ys], [Z | Zs]);
unzip3([], Xs, Ys, Zs) ->
    {reverse(Xs), reverse(Ys), reverse(Zs)}.

%% Return [F(X0, Y0), F(X1, Y1), ..., F(Xn, Yn)] for lists [X0, X1, ...,
%% Xn] and [Y0, Y1, ..., Yn].

-spec zipwith(Combine, List1, List2) -> List3 when
      Combine :: fun((X, Y) -> T),
      List1 :: [X],
      List2 :: [Y],
      List3 :: [T],
      X :: term(),
      Y :: term(),
      T :: term().

zipwith(F, [X | Xs], [Y | Ys]) -> [F(X, Y) | zipwith(F, Xs, Ys)];
zipwith(F, [], []) when is_function(F, 2) -> [].

%% Return [F(X0, Y0, Z0), F(X1, Y1, Z1), ..., F(Xn, Yn, Zn)] for lists
%% [X0, X1, ..., Xn], [Y0, Y1, ..., Yn] and [Z0, Z1, ..., Zn].

-spec zipwith3(Combine, List1, List2, List3) -> List4 when
      Combine :: fun((X, Y, Z) -> T),
      List1 :: [X],
      List2 :: [Y],
      List3 :: [Z],
      List4 :: [T],
      X :: term(),
      Y :: term(),
      Z :: term(),
      T :: term().

zipwith3(F, [X | Xs], [Y | Ys], [Z | Zs]) ->
    [F(X, Y, Z) | zipwith3(F, Xs, Ys, Zs)];
zipwith3(F, [], [], []) when is_function(F, 3) -> [].

%% sort(List) -> L
%%  sorts the list L

-spec sort(List1) -> List2 when
      List1 :: [T],
      List2 :: [T],
      T :: term().

sort([X, Y | L] = L0) when X =< Y ->
    case L of
	[] -> 
	    L0;
	[Z] when Y =< Z ->
	    L0;
	[Z] when X =< Z ->
	    [X, Z, Y];
	[Z] ->
	    [Z, X, Y];
	_ when X == Y ->
	    sort_1(Y, L, [X]);
	_ ->
	    split_1(X, Y, L, [], [])
    end;
sort([X, Y | L]) ->
    case L of
	[] ->
	    [Y, X];
	[Z] when X =< Z ->
	    [Y, X | L];
	[Z] when Y =< Z ->
	    [Y, Z, X];
	[Z] ->
	    [Z, Y, X];
	_ ->
	    split_2(X, Y, L, [], [])
    end;
sort([_] = L) ->
    L;
sort([] = L) ->
    L.

sort_1(X, [Y | L], R) when X == Y ->
    sort_1(Y, L, [X | R]);
sort_1(X, [Y | L], R) when X < Y ->
    split_1(X, Y, L, R, []);
sort_1(X, [Y | L], R) ->
    split_2(X, Y, L, R, []);
sort_1(X, [], R) ->
    lists:reverse(R, [X]).

%% merge(List) -> L
%%  merges a list of sorted lists

-spec merge(ListOfLists) -> List1 when
      ListOfLists :: [List],
      List :: [T],
      List1 :: [T],
      T :: term().

merge(L) ->
    mergel(L, []).

%% merge3(X, Y, Z) -> L
%%  merges three sorted lists X, Y and Z

-spec merge3(List1, List2, List3) -> List4 when
      List1 :: [X],
      List2 :: [Y],
      List3 :: [Z],
      List4 :: [(X | Y | Z)],
      X :: term(),
      Y :: term(),
      Z :: term().

merge3(L1, [], L3) ->
   merge(L1, L3);
merge3(L1, L2, []) ->
   merge(L1, L2);
merge3(L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(merge3_1(L1, [], H2, T2, H3, T3), []).

%% rmerge3(X, Y, Z) -> L
%%  merges three reversed sorted lists X, Y and Z

-spec rmerge3([X], [Y], [Z]) -> [(X | Y | Z)].

rmerge3(L1, [], L3) ->
   rmerge(L1, L3);
rmerge3(L1, L2, []) ->
   rmerge(L1, L2);
rmerge3(L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(rmerge3_1(L1, [], H2, T2, H3, T3), []).

%% merge(X, Y) -> L
%%  merges two sorted lists X and Y

-spec merge(List1, List2) -> List3 when
      List1 :: [X],
      List2 :: [Y],
      List3 :: [(X | Y)],
      X :: term(),
      Y :: term().

merge(T1, []) ->
    T1;
merge(T1, [H2 | T2]) ->
    lists:reverse(merge2_1(T1, H2, T2, []), []).

%% rmerge(X, Y) -> L
%%  merges two reversed sorted lists X and Y

%% reverse(rmerge(reverse(A),reverse(B))) is equal to merge(I,A,B).

-spec rmerge([X], [Y]) -> [(X | Y)].

rmerge(T1, []) ->
    T1;
rmerge(T1, [H2 | T2]) ->
    lists:reverse(rmerge2_1(T1, H2, T2, []), []).

%% concat(L) concatenate the list representation of the elements
%%  in L - the elements in L can be atoms, numbers of strings.
%%  Returns a list of characters.

-spec concat(Things) -> string() when
      Things :: [Thing],
      Thing :: atom() | integer() | float() | string().

concat(List) ->
    flatmap(fun thing_to_list/1, List).

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_list(X)    -> X.	%Assumed to be a string

%% flatten(List)
%% flatten(List, Tail)
%%  Flatten a list, adding optional tail.

-spec flatten(DeepList) -> List when
      DeepList :: [term() | DeepList],
      List :: [term()].

flatten(List) when is_list(List) ->
    do_flatten(List, []).

-spec flatten(DeepList, Tail) -> List when
      DeepList :: [term() | DeepList],
      Tail :: [term()],
      List :: [term()].

flatten(List, Tail) when is_list(List), is_list(Tail) ->
    do_flatten(List, Tail).

do_flatten([H|T], Tail) when is_list(H) ->
    do_flatten(H, do_flatten(T, Tail));
do_flatten([H|T], Tail) ->
    [H|do_flatten(T, Tail)];
do_flatten([], Tail) ->
    Tail.

%% flatlength(List)
%%  Calculate the length of a list of lists.

-spec flatlength(DeepList) -> non_neg_integer() when
      DeepList :: [term() | DeepList].

flatlength(List) ->
    flatlength(List, 0).

flatlength([H|T], L) when is_list(H) ->
    flatlength(H, flatlength(T, L));
flatlength([_|T], L) ->
    flatlength(T, L + 1);
flatlength([], L) -> L.

%% keymember(Key, Index, [Tuple]) Now a BIF!
%% keyfind(Key, Index, [Tuple]) A BIF!
%% keysearch(Key, Index, [Tuple]) Now a BIF!
%% keydelete(Key, Index, [Tuple])
%% keyreplace(Key, Index, [Tuple], NewTuple)
%% keytake(Key, Index, [Tuple])
%% keystore(Key, Index, [Tuple], NewTuple)
%% keysort(Index, [Tuple])
%% keymerge(Index, [Tuple], [Tuple])
%% ukeysort(Index, [Tuple])
%% ukeymerge(Index, [Tuple], [Tuple])
%% keymap(Function, Index, [Tuple])
%% keymap(Function, ExtraArgs, Index, [Tuple])

%keymember(K,N,L) when is_integer(N), N > 0 ->
%    keymember3(K,N,L).

%keymember3(Key, N, [T|Ts]) when element(N, T) == Key -> true;
%keymember3(Key, N, [T|Ts]) ->
%    keymember3(Key, N, Ts);
%keymember3(Key, N, []) -> false.

%keysearch(K, N, L) when is_integer(N), N > 0 ->
%    keysearch3(K, N, L).

%keysearch3(Key, N, [H|T]) when element(N, H) == Key ->
%    {value, H};
%keysearch3(Key, N, [H|T]) ->
%    keysearch3(Key, N, T);
%keysearch3(Key, N, []) -> false.

-spec keydelete(Key, N, TupleList1) -> TupleList2 when
      Key :: term(),
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple],
      Tuple :: tuple().

keydelete(K, N, L) when is_integer(N), N > 0 ->
    keydelete3(K, N, L).

keydelete3(Key, N, [H|T]) when element(N, H) == Key -> T;
keydelete3(Key, N, [H|T]) ->
    [H|keydelete3(Key, N, T)];
keydelete3(_, _, []) -> [].

-spec keyreplace(Key, N, TupleList1, NewTuple) -> TupleList2 when
      Key :: term(),
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple],
      NewTuple :: Tuple,
      Tuple :: tuple().

keyreplace(K, N, L, New) when is_integer(N), N > 0, is_tuple(New) ->
    keyreplace3(K, N, L, New).

keyreplace3(Key, Pos, [Tup|Tail], New) when element(Pos, Tup) == Key ->
    [New|Tail];
keyreplace3(Key, Pos, [H|T], New) ->
    [H|keyreplace3(Key, Pos, T, New)];
keyreplace3(_, _, [], _) -> [].

-spec keytake(Key, N, TupleList1) -> {value, Tuple, TupleList2} | false when
      Key :: term(),
      N :: pos_integer(),
      TupleList1 :: [tuple()],
      TupleList2 :: [tuple()],
      Tuple :: tuple().

keytake(Key, N, L) when is_integer(N), N > 0 ->
    keytake(Key, N, L, []).

keytake(Key, N, [H|T], L) when element(N, H) == Key ->
    {value, H, lists:reverse(L, T)};
keytake(Key, N, [H|T], L) ->
    keytake(Key, N, T, [H|L]);
keytake(_K, _N, [], _L) -> false.

-spec keystore(Key, N, TupleList1, NewTuple) -> TupleList2 when
      Key :: term(),
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple, ...],
      NewTuple :: Tuple,
      Tuple :: tuple().

keystore(K, N, L, New) when is_integer(N), N > 0, is_tuple(New) ->
    keystore2(K, N, L, New).

keystore2(Key, N, [H|T], New) when element(N, H) == Key ->
    [New|T];
keystore2(Key, N, [H|T], New) ->
    [H|keystore2(Key, N, T, New)];
keystore2(_Key, _N, [], New) ->
    [New].

-spec keysort(N, TupleList1) -> TupleList2 when
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple],
      Tuple :: tuple().

keysort(I, L) when is_integer(I), I > 0 ->
    case L of
	[] -> L;
	[_] -> L;
	[X, Y | T] ->
	    case {element(I, X), element(I, Y)} of
		{EX, EY} when EX =< EY ->
		    case T of
			[] ->
			    L;
			[Z] ->
			    case element(I, Z) of
				EZ when EY =< EZ ->
				    L;
				EZ when EX =< EZ ->
				    [X, Z, Y];
				_EZ ->
				    [Z, X, Y]
			    end;
			_ when X == Y ->
			    keysort_1(I, Y, EY, T, [X]);
			_ ->
			    keysplit_1(I, X, EX, Y, EY, T, [], [])
		    end;
		{EX, EY} ->
		    case T of
			[] ->
			    [Y, X];
			[Z] ->
			    case element(I, Z) of
				EZ when EX =< EZ ->
				    [Y, X | T];
				EZ when EY =< EZ ->
				    [Y, Z, X];
				_EZ ->
				    [Z, Y, X]
			    end;
			_ ->
			    keysplit_2(I, X, EX, Y, EY, T, [], [])
		    end
	    end
    end.

keysort_1(I, X, EX, [Y | L], R) when X == Y ->
    keysort_1(I, Y, EX, L, [X | R]);
keysort_1(I, X, EX, [Y | L], R) ->
    case element(I, Y) of
	EY when EX =< EY ->
	    keysplit_1(I, X, EX, Y, EY, L, R, []);
	EY ->
	    keysplit_2(I, X, EX, Y, EY, L, R, [])
    end;
keysort_1(_I, X, _EX, [], R) ->
    lists:reverse(R, [X]).

-spec keymerge(N, TupleList1, TupleList2) -> TupleList3 when
      N :: pos_integer(),
      TupleList1 :: [T1],
      TupleList2 :: [T2],
      TupleList3 :: [(T1 | T2)],
      T1 :: Tuple,
      T2 :: Tuple,
      Tuple :: tuple().

keymerge(Index, T1, L2) when is_integer(Index), Index > 0 -> 
    case L2 of
	[] ->
	    T1;
	[H2 | T2] ->
	    E2 = element(Index, H2),
	    M = keymerge2_1(Index, T1, E2, H2, T2, []),
	    lists:reverse(M, [])
    end.

%% reverse(rkeymerge(I,reverse(A),reverse(B))) is equal to keymerge(I,A,B).

-spec rkeymerge(pos_integer(), [X], [Y]) ->
	[R] when X :: tuple(), Y :: tuple(), R :: tuple().

rkeymerge(Index, T1, L2) when is_integer(Index), Index > 0 -> 
    case L2 of
	[] ->
	    T1;
	[H2 | T2] ->
	    E2 = element(Index, H2),
	    M = rkeymerge2_1(Index, T1, E2, H2, T2, []),
	    lists:reverse(M, [])
    end.

-spec ukeysort(N, TupleList1) -> TupleList2 when
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple],
      Tuple :: tuple().

ukeysort(I, L) when is_integer(I), I > 0 ->
    case L of
	[] -> L;
	[_] -> L;
	[X, Y | T] ->
            case {element(I, X), element(I, Y)} of
                {EX, EY} when EX == EY ->
                    ukeysort_1(I, X, EX, T);
                {EX, EY} when EX < EY ->
                    case T of
                        [] ->
                            L;
                        [Z] ->
                            case element(I, Z) of
                                EZ when EY == EZ ->
                                    [X, Y];
                                EZ when EY < EZ ->
                                    [X, Y, Z];
                                EZ when EZ == EX ->
                                    [X, Y];
                                EZ when EX =< EZ ->
                                    [X, Z, Y];
                                _EZ ->
                                    [Z, X, Y]
                            end;
                        _ ->
                            ukeysplit_1(I, X, EX, Y, EY, T, [], [])
                    end;
                {EX, EY} ->
                    case T of
                        [] ->
                            [Y, X];
                        [Z] ->
                            case element(I, Z) of
                                EZ when EX == EZ ->
                                    [Y, X];
                                EZ when EX < EZ ->
                                    [Y, X, Z];
                                EZ when EY == EZ ->
                                    [Y, X];
                                EZ when EY =< EZ ->
                                    [Y, Z, X];
                                _EZ ->
                                    [Z, Y, X]
                            end;
                        _ ->
			    ukeysplit_2(I, Y, EY, T, [X])
                    end
	    end
    end.

ukeysort_1(I, X, EX, [Y | L]) ->
    case element(I, Y) of
        EY when EX == EY ->
            ukeysort_1(I, X, EX, L);
	EY when EX < EY ->
	    ukeysplit_1(I, X, EX, Y, EY, L, [], []);
	EY ->
	    ukeysplit_2(I, Y, EY, L, [X])
    end;
ukeysort_1(_I, X, _EX, []) ->
    [X].

-spec ukeymerge(N, TupleList1, TupleList2) -> TupleList3 when
      N :: pos_integer(),
      TupleList1 :: [T1],
      TupleList2 :: [T2],
      TupleList3 :: [(T1 | T2)],
      T1 :: Tuple,
      T2 :: Tuple,
      Tuple :: tuple().

ukeymerge(Index, L1, T2) when is_integer(Index), Index > 0 ->
    case L1 of
	[] ->
	    T2;
	[H1 | T1] ->
	    E1 = element(Index, H1),
	    M = ukeymerge2_2(Index, T1, E1, H1, T2, []),
	    lists:reverse(M, [])
    end.

%% reverse(rukeymerge(I,reverse(A),reverse(B))) is equal to ukeymerge(I,A,B).

-spec rukeymerge(pos_integer(), [X], [Y]) ->
	[(X | Y)] when X :: tuple(), Y :: tuple().

rukeymerge(Index, T1, L2) when is_integer(Index), Index > 0 ->
    case L2 of
	[] ->
	    T1;
	[H2 | T2] ->
	    E2 = element(Index, H2),
	    M = rukeymerge2_1(Index, T1, E2, T2, [], H2),
	    lists:reverse(M, [])
    end.

-spec keymap(Fun, N, TupleList1) -> TupleList2 when
      Fun :: fun((Term1 :: term()) -> Term2 :: term()),
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple],
      Tuple :: tuple().

keymap(Fun, Index, [Tup|Tail]) ->
   [setelement(Index, Tup, Fun(element(Index, Tup)))|keymap(Fun, Index, Tail)];
keymap(Fun, Index, []) when is_integer(Index), Index >= 1, 
                            is_function(Fun, 1) -> [].

%%% Suggestion from OTP-2948: sort and merge with Fun.

-spec sort(Fun, List1) -> List2 when
      Fun :: fun((A :: T, B :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

sort(Fun, []) when is_function(Fun, 2) ->
    [];
sort(Fun, [_] = L) when is_function(Fun, 2) ->
    L;
sort(Fun, [X, Y | T]) ->
    case Fun(X, Y) of
	true ->
	    fsplit_1(Y, X, Fun, T, [], []);
	false ->
	    fsplit_2(Y, X, Fun, T, [], [])
    end.

-spec merge(Fun, List1, List2) -> List3 when
      Fun :: fun((A, B) -> boolean()),
      List1 :: [A],
      List2 :: [B],
      List3 :: [(A | B)],
      A :: term(),
      B :: term().

merge(Fun, T1, [H2 | T2]) when is_function(Fun, 2) ->
    lists:reverse(fmerge2_1(T1, H2, Fun, T2, []), []);
merge(Fun, T1, []) when is_function(Fun, 2) ->
    T1.

%% reverse(rmerge(F,reverse(A),reverse(B))) is equal to merge(F,A,B).

-spec rmerge(fun((X, Y) -> boolean()), [X], [Y]) -> [(X | Y)].

rmerge(Fun, T1, [H2 | T2]) when is_function(Fun, 2) ->
    lists:reverse(rfmerge2_1(T1, H2, Fun, T2, []), []);
rmerge(Fun, T1, []) when is_function(Fun, 2) ->
    T1.

-spec usort(Fun, List1) -> List2 when
      Fun :: fun((T, T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

usort(Fun, [_] = L) when is_function(Fun, 2) ->
    L;
usort(Fun, [] = L) when is_function(Fun, 2) ->
    L;
usort(Fun, [X | L]) when is_function(Fun, 2) ->
    usort_1(Fun, X, L).

usort_1(Fun, X, [Y | L]) ->
    case Fun(X, Y) of
        true ->
            case Fun(Y, X) of
                true -> % X equal to Y
                    case L of
                        [] ->
                            [X];
                        _ ->
                            usort_1(Fun, X, L)
                    end;
                false ->
                    ufsplit_1(Y, X, Fun, L, [], [])
            end;
        false  ->
	    ufsplit_2(Y, L, Fun, [X])
    end.
                    
-spec umerge(Fun, List1, List2) -> List3 when
      Fun :: fun((A, B) -> boolean()),
      List1 :: [A],
      List2 :: [B],
      List3 :: [(A | B)],
      A :: term(),
      B :: term().

umerge(Fun, [], T2) when is_function(Fun, 2) ->
    T2;
umerge(Fun, [H1 | T1], T2) when is_function(Fun, 2) ->
    lists:reverse(ufmerge2_2(H1, T1, Fun, T2, []), []).

%% reverse(rumerge(F,reverse(A),reverse(B))) is equal to umerge(F,A,B).

-spec rumerge(fun((X, Y) -> boolean()), [X], [Y]) -> [(X | Y)].

rumerge(Fun, T1, []) when is_function(Fun, 2) ->
    T1;
rumerge(Fun, T1, [H2 | T2]) when is_function(Fun, 2) ->
    lists:reverse(rufmerge2_1(T1, H2, Fun, T2, []), []).

%% usort(List) -> L
%%  sorts the list L, removes duplicates

-spec usort(List1) -> List2 when
      List1 :: [T],
      List2 :: [T],
      T :: term().

usort([X, Y | L] = L0) when X < Y ->
    case L of
	[] ->
	    L0;
	[Z] when Y < Z ->
	    L0;
	[Z] when Y == Z ->
	    [X, Y];
	[Z] when Z < X ->
	    [Z, X, Y];
	[Z] when Z == X ->
	    [X, Y];
	[Z] ->
	    [X, Z, Y];
	_ ->
	    usplit_1(X, Y, L, [], [])
    end;
usort([X, Y | L]) when X > Y ->
    case L of
	[] ->
	    [Y, X];
	[Z] when X < Z ->
	    [Y, X | L];
	[Z] when X == Z ->
	    [Y, X];
	[Z] when Z < Y ->
	    [Z, Y, X];
	[Z] when Z == Y ->
	    [Y, X];
	[Z] ->
	    [Y, Z, X];
        _ ->
            usplit_2(X, Y, L, [], [])
    end;
usort([X, _Y | L]) ->
    usort_1(X, L);
usort([_] = L) ->
    L;
usort([]) ->
    [].

usort_1(X, [Y | L]) when X == Y ->
    usort_1(X, L);
usort_1(X, [Y | L]) when X < Y ->
    usplit_1(X, Y, L, [], []);
usort_1(X, [Y | L]) ->
    usplit_2(X, Y, L, [], []);
usort_1(X, []) ->
    [X].

%% umerge(List) -> L
%%  merges a list of sorted lists without duplicates, removes duplicates

-spec umerge(ListOfLists) -> List1 when
      ListOfLists :: [List],
      List :: [T],
      List1 :: [T],
      T :: term().

umerge(L) ->
    umergel(L).

%% umerge3(X, Y, Z) -> L
%%  merges three sorted lists X, Y and Z without duplicates, 
%%  removes duplicates

-spec umerge3(List1, List2, List3) -> List4 when
      List1 :: [X],
      List2 :: [Y],
      List3 :: [Z],
      List4 :: [(X | Y | Z)],
      X :: term(),
      Y :: term(),
      Z :: term().

umerge3(L1, [], L3) ->
   umerge(L1, L3);
umerge3(L1, L2, []) ->
   umerge(L1, L2);
umerge3(L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(umerge3_1(L1, [H2 | H3], T2, H2, [], T3, H3), []).

%% rumerge3(X, Y, Z) -> L
%%  merges three reversed sorted lists X, Y and Z without duplicates,
%%  removes duplicates

-spec rumerge3([X], [Y], [Z]) -> [(X | Y | Z)].

rumerge3(L1, [], L3) ->
   rumerge(L1, L3);
rumerge3(L1, L2, []) ->
   rumerge(L1, L2);
rumerge3(L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(rumerge3_1(L1, T2, H2, [], T3, H3),[]).

%% umerge(X, Y) -> L
%%  merges two sorted lists X and Y without duplicates, removes duplicates

-spec umerge(List1, List2) -> List3 when
      List1 :: [X],
      List2 :: [Y],
      List3 :: [(X | Y)],
      X :: term(),
      Y :: term().

umerge([], T2) ->
    T2;
umerge([H1 | T1], T2) ->
    lists:reverse(umerge2_2(T1, T2, [], H1), []).

%% rumerge(X, Y) -> L
%%  merges two reversed sorted lists X and Y without duplicates,
%%  removes duplicates

%% reverse(rumerge(reverse(A),reverse(B))) is equal to umerge(I,A,B).

-spec rumerge([X], [Y]) -> [(X | Y)].

rumerge(T1, []) ->
    T1;
rumerge(T1, [H2 | T2]) ->
    lists:reverse(rumerge2_1(T1, T2, [], H2), []).

%% all(Predicate, List)
%% any(Predicate, List)
%% map(Function, List)
%% flatmap(Function, List)
%% foldl(Function, First, List)
%% foldr(Function, Last, List)
%% filter(Predicate, List)
%% zf(Function, List)
%% mapfoldl(Function, First, List)
%% mapfoldr(Function, Last, List)
%% foreach(Function, List)
%% takewhile(Predicate, List)
%% dropwhile(Predicate, List)
%% splitwith(Predicate, List)
%%  for list programming. Function here is a 'fun'.
%% 
%%  The name zf is a joke!
%%
%%  N.B. Unless where the functions actually needs it only foreach/2/3,
%%  which is meant to be used for its side effects, has a defined order
%%  of evaluation.
%%
%%  There are also versions with an extra argument, ExtraArgs, which is a
%%  list of extra arguments to each call.

-spec all(Pred, List) -> boolean() when
      Pred :: fun((Elem :: T) -> boolean()),
      List :: [T],
      T :: term().

all(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> all(Pred, Tail);
	false -> false
    end;
all(Pred, []) when is_function(Pred, 1) -> true. 

-spec any(Pred, List) -> boolean() when
      Pred :: fun((Elem :: T) -> boolean()),
      List :: [T],
      T :: term().

any(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> true;
	false -> any(Pred, Tail)
    end;
any(Pred, []) when is_function(Pred, 1) -> false. 

-spec map(Fun, List1) -> List2 when
      Fun :: fun((A) -> B),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

map(F, [H|T]) ->
    [F(H)|map(F, T)];
map(F, []) when is_function(F, 1) -> [].

-spec flatmap(Fun, List1) -> List2 when
      Fun :: fun((A) -> [B]),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

flatmap(F, [Hd|Tail]) ->
    F(Hd) ++ flatmap(F, Tail);
flatmap(F, []) when is_function(F, 1) -> [].

-spec foldl(Fun, Acc0, List) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List :: [T],
      T :: term().

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) when is_function(F, 2) -> Accu.

-spec foldr(Fun, Acc0, List) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List :: [T],
      T :: term().

foldr(F, Accu, [Hd|Tail]) ->
    F(Hd, foldr(F, Accu, Tail));
foldr(F, Accu, []) when is_function(F, 2) -> Accu.

-spec filter(Pred, List1) -> List2 when
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

filter(Pred, List) when is_function(Pred, 1) ->
    [ E || E <- List, Pred(E) ].

%% Equivalent to {filter(F, L), filter(NotF, L)}, if NotF = 'fun(X) ->
%% not F(X) end'.

-spec partition(Pred, List) -> {Satisfying, NotSatisfying} when
      Pred :: fun((Elem :: T) -> boolean()),
      List :: [T],
      Satisfying :: [T],
      NotSatisfying :: [T],
      T :: term().

partition(Pred, L) ->
    partition(Pred, L, [], []).

partition(Pred, [H | T], As, Bs) ->
    case Pred(H) of
	true -> partition(Pred, T, [H | As], Bs);
	false -> partition(Pred, T, As, [H | Bs])
    end;
partition(Pred, [], As, Bs) when is_function(Pred, 1) ->
    {reverse(As), reverse(Bs)}.

-spec filtermap(Fun, List1) -> List2 when
      Fun :: fun((Elem) -> boolean() | {'true', Value}),
      List1 :: [Elem],
      List2 :: [Elem | Value],
      Elem :: term(),
      Value :: term().

filtermap(F, [Hd|Tail]) ->
    case F(Hd) of
	true ->
	    [Hd|filtermap(F, Tail)];
	{true,Val} ->
	    [Val|filtermap(F, Tail)];
	false ->
	    filtermap(F, Tail)
    end;
filtermap(F, []) when is_function(F, 1) -> [].

-spec zf(fun((T) -> boolean() | {'true', X}), [T]) -> [(T | X)].

zf(F, L) ->
    filtermap(F, L).

-spec foreach(Fun, List) -> ok when
      Fun :: fun((Elem :: T) -> term()),
      List :: [T],
      T :: term().

foreach(F, [Hd|Tail]) ->
    F(Hd),
    foreach(F, Tail);
foreach(F, []) when is_function(F, 1) -> ok.

-spec mapfoldl(Fun, Acc0, List1) -> {List2, Acc1} when
      Fun :: fun((A, AccIn) -> {B, AccOut}),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

mapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = mapfoldl(F, Accu1, Tail),
    {[R|Rs],Accu2};
mapfoldl(F, Accu, []) when is_function(F, 2) -> {[],Accu}.

-spec mapfoldr(Fun, Acc0, List1) -> {List2, Acc1} when
      Fun :: fun((A, AccIn) -> {B, AccOut}),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

mapfoldr(F, Accu0, [Hd|Tail]) ->
    {Rs,Accu1} = mapfoldr(F, Accu0, Tail),
    {R,Accu2} = F(Hd, Accu1),
    {[R|Rs],Accu2};
mapfoldr(F, Accu, []) when is_function(F, 2) -> {[],Accu}.

-spec takewhile(Pred, List1) -> List2 when
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

takewhile(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> [Hd|takewhile(Pred, Tail)];
	false -> []
    end;
takewhile(Pred, []) when is_function(Pred, 1) -> [].

-spec dropwhile(Pred, List1) -> List2 when
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

dropwhile(Pred, [Hd|Tail]=Rest) ->
    case Pred(Hd) of
	true -> dropwhile(Pred, Tail);
	false -> Rest
    end;
dropwhile(Pred, []) when is_function(Pred, 1) -> [].

-spec search(Pred, List) -> {value, Value} | false when
      Pred :: fun((T) -> boolean()),
      List :: [T],
      Value :: T.

search(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
        true -> {value, Hd};
        false -> search(Pred, Tail)
    end;
search(Pred, []) when is_function(Pred, 1) ->
    false.

-spec splitwith(Pred, List) -> {List1, List2} when
      Pred :: fun((T) -> boolean()),
      List :: [T],
      List1 :: [T],
      List2 :: [T],
      T :: term().

splitwith(Pred, List) when is_function(Pred, 1) ->
    splitwith(Pred, List, []).

splitwith(Pred, [Hd|Tail], Taken) ->
    case Pred(Hd) of
	true -> splitwith(Pred, Tail, [Hd|Taken]);
	false -> {reverse(Taken), [Hd|Tail]}
    end;
splitwith(Pred, [], Taken) when is_function(Pred, 1) ->
    {reverse(Taken),[]}.

-spec split(N, List1) -> {List2, List3} when
      N :: non_neg_integer(),
      List1 :: [T],
      List2 :: [T],
      List3 :: [T],
      T :: term().

split(N, List) when is_integer(N), N >= 0, is_list(List) ->
    case split(N, List, []) of
	{_, _} = Result -> Result;
	Fault when is_atom(Fault) ->
	    erlang:error(Fault, [N,List])
    end;
split(N, List) ->
    erlang:error(badarg, [N,List]).

split(0, L, R) ->
    {lists:reverse(R, []), L};
split(N, [H|T], R) ->
    split(N-1, T, [H|R]);
split(_, [], _) ->
    badarg.

-spec join(Sep, List1) -> List2 when
      Sep :: T,
      List1 :: [T],
      List2 :: [T],
      T :: term().

join(_Sep, []) -> [];
join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].

%%% =================================================================
%%% Here follows the implementation of the sort functions.
%%%
%%% These functions used to be in their own module (lists_sort),
%%% but have now been placed here to allow Dialyzer to produce better
%%% type information.
%%% =================================================================

-compile({inline, 
          [{merge3_12,7}, {merge3_21,7}, {rmerge3_12,7}, {rmerge3_21,7}]}).

-compile({inline, 
          [{umerge3_12,8}, {umerge3_21,8},
	   {rumerge3_12a,7}, {rumerge3_12b,8}]}).

-compile({inline, 
          [{keymerge3_12,12}, {keymerge3_21,12}, 
           {rkeymerge3_12,12}, {rkeymerge3_21,12}]}).

-compile({inline,
          [{ukeymerge3_12,13}, {ukeymerge3_21,13},
	   {rukeymerge3_12a,11}, {rukeymerge3_21a,13},
	   {rukeymerge3_12b,12}, {rukeymerge3_21b,12}]}).

%% sort/1

%% Ascending.
split_1(X, Y, [Z | L], R, Rs) when Z >= Y ->
    split_1(Y, Z, L, [X | R], Rs);
split_1(X, Y, [Z | L], R, Rs) when Z >= X ->
    split_1(Z, Y, L, [X | R], Rs);
split_1(X, Y, [Z | L], [], Rs) ->
    split_1(X, Y, L, [Z], Rs);
split_1(X, Y, [Z | L], R, Rs) ->
    split_1_1(X, Y, L, R, Rs, Z);
split_1(X, Y, [], R, Rs) ->
    rmergel([[Y, X | R] | Rs], []).

split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= Y ->
    split_1_1(Y, Z, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= X ->
    split_1_1(Z, Y, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when S =< Z ->
    split_1(S, Z, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [Z | L], R, Rs, S) ->
    split_1(Z, S, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [], R, Rs, S) ->
    rmergel([[S], [Y, X | R] | Rs], []).

%% Descending.
split_2(X, Y, [Z | L], R, Rs) when Z =< Y ->
    split_2(Y, Z, L, [X | R], Rs);
split_2(X, Y, [Z | L], R, Rs) when Z =< X ->
    split_2(Z, Y, L, [X | R], Rs);
split_2(X, Y, [Z | L], [], Rs) ->
    split_2(X, Y, L, [Z], Rs);
split_2(X, Y, [Z | L], R, Rs) ->
    split_2_1(X, Y, L, R, Rs, Z);
split_2(X, Y, [], R, Rs) ->
    mergel([[Y, X | R] | Rs], []).

split_2_1(X, Y, [Z | L], R, Rs, S) when Z =< Y ->
    split_2_1(Y, Z, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when Z =< X ->
    split_2_1(Z, Y, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when S > Z ->
    split_2(S, Z, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [Z | L], R, Rs, S) ->
    split_2(Z, S, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [], R, Rs, S) ->
    mergel([[S], [Y, X | R] | Rs], []).

%% merge/1

mergel([[] | L], Acc) ->
    mergel(L, Acc);
mergel([T1, [H2 | T2], [H3 | T3] | L], Acc) ->
    mergel(L, [merge3_1(T1, [], H2, T2, H3, T3) | Acc]);
mergel([T1, [H2 | T2]], Acc) ->
    rmergel([merge2_1(T1, H2, T2, []) | Acc], []);
mergel([L], []) ->
    L;
mergel([L], Acc) ->
    rmergel([lists:reverse(L, []) | Acc], []);
mergel([], []) ->
    [];
mergel([], Acc) ->
    rmergel(Acc, []);
mergel([A, [] | L], Acc) ->
    mergel([A | L], Acc);
mergel([A, B, [] | L], Acc) ->
    mergel([A, B | L], Acc).

rmergel([[H3 | T3], [H2 | T2], T1 | L], Acc) ->
    rmergel(L, [rmerge3_1(T1, [], H2, T2, H3, T3) | Acc]);
rmergel([[H2 | T2], T1], Acc) ->
    mergel([rmerge2_1(T1, H2, T2, []) | Acc], []);
rmergel([L], Acc) ->
    mergel([lists:reverse(L, []) | Acc], []);
rmergel([], Acc) ->
    mergel(Acc, []).

%% merge3/3

%% Take L1 apart.
merge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 =< H2 ->
    merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_1([], M, H2, T2, H3, T3) when H2 =< H3 ->
    merge2_1(T2, H3, T3, [H2 | M]);
merge3_1([], M, H2, T2, H3, T3) ->
    merge2_2(T2, H3, T3, M, H2).

%% Take L2 apart.
merge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [], H3, T3) when H1 =< H3 ->
    merge2_1(T1, H3, T3, [H1 | M]);
merge3_2(T1, H1, M, [], H3, T3) ->
    merge2_2(T1, H3, T3, M, H1).

% H1 =< H2. Inlined.
merge3_12(T1, H1, H2, T2, H3, T3, M) when H1 =< H3 ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3);
merge3_12(T1, H1, H2, T2, H3, T3, M) ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3).

% H1 =< H2, take L3 apart.
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H1 =< H3 ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3);
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_12_3(T1, H1, H2, T2, M, []) ->
    merge2_1(T1, H2, T2, [H1 | M]).

% H1 > H2. Inlined.
merge3_21(T1, H1, H2, T2, H3, T3, M) when H2 =< H3 ->
    merge3_2(T1, H1, [H2 | M], T2, H3, T3);
merge3_21(T1, H1, H2, T2, H3, T3, M) ->
    merge3_21_3(T1, H1, H2, T2, [H3 | M], T3).

% H1 > H2, take L3 apart.
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H2 =< H3 ->
    merge3_2(T1, H1, [H2 | M], T2, H3, T3);
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    merge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_21_3(T1, H1, H2, T2, M, []) ->
    merge2_2(T1, H2, T2, M, H1).

%% rmerge/3

%% Take L1 apart.
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 =< H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([], M, H2, T2, H3, T3) when H2 =< H3 ->
    rmerge2_2(T2, H3, T3, M, H2);
rmerge3_1([], M, H2, T2, H3, T3) ->
    rmerge2_1(T2, H3, T3, [H2 | M]).

%% Take L2 apart.
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [], H3, T3) when H1 =< H3 ->
    rmerge2_2(T1, H3, T3, M, H1);
rmerge3_2(T1, H1, M, [], H3, T3) ->
    rmerge2_1(T1, H3, T3, [H1 | M]).

% H1 =< H2. Inlined.
rmerge3_12(T1, H1, H2, T2, H3, T3, M) when H2 =< H3 ->
    rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12(T1, H1, H2, T2, H3, T3, M) ->
    rmerge3_2(T1, H1, [H2 | M], T2, H3, T3).

% H1 =< H2, take L3 apart.
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H2 =< H3 ->
    rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rmerge3_2(T1, H1, [H2 | M], T2, H3, T3);
rmerge3_12_3(T1, H1, H2, T2, M, []) ->
    rmerge2_2(T1, H2, T2, M, H1).

% H1 > H2. Inlined.
rmerge3_21(T1, H1, H2, T2, H3, T3, M) when H1 =< H3 ->
    rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21(T1, H1, H2, T2, H3, T3, M) ->
    rmerge3_1(T1, [H1 | M], H2, T2, H3, T3).

% H1 > H2, take L3 apart.
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H1 =< H3 ->
    rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rmerge3_1(T1, [H1 | M], H2, T2, H3, T3);
rmerge3_21_3(T1, H1, H2, T2, M, []) ->
    rmerge2_1(T1, H2, T2, [H1 | M]).

%% merge/2

merge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_1([H1 | T1], H2, T2, M) ->
    merge2_2(T1, H2, T2, M, H1);
merge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

merge2_2(T1, HdM, [H2 | T2], M, H1) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1, HdM | M]);
merge2_2(T1, HdM, [H2 | T2], M, H1) ->
    merge2_2(T1, H2, T2, [HdM | M], H1);
merge2_2(T1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% rmerge/2

rmerge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    rmerge2_2(T1, H2, T2, M, H1);
rmerge2_1([H1 | T1], H2, T2, M) ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rmerge2_2(T1, HdM, [H2 | T2], M, H1) when H1 =< H2 ->
    rmerge2_2(T1, H2, T2, [HdM | M], H1);
rmerge2_2(T1, HdM, [H2 | T2], M, H1) ->
    rmerge2_1(T1, H2, T2, [H1, HdM | M]);
rmerge2_2(T1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% usort/1

%% Ascending.
usplit_1(X, Y, [Z | L], R, Rs) when Z > Y ->
    usplit_1(Y, Z, L, [X | R], Rs);
usplit_1(X, Y, [Z | L], R, Rs) when Z == Y ->
    usplit_1(X, Y, L, R, Rs);
usplit_1(X, Y, [Z | L], R, Rs) when Z > X ->
    usplit_1(Z, Y, L, [X | R], Rs);
usplit_1(X, Y, [Z | L], R, Rs) when Z == X ->
    usplit_1(X, Y, L, R, Rs);
usplit_1(X, Y, [Z | L], [], Rs) ->
    usplit_1(X, Y, L, [Z], Rs);
usplit_1(X, Y, [Z | L], R, Rs) ->
    usplit_1_1(X, Y, L, R, Rs, Z);
usplit_1(X, Y, [], R, Rs) ->
    rumergel([[Y, X | R] | Rs], [], asc).

usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z > Y ->
    usplit_1_1(Y, Z, L, [X | R], Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z == Y ->
    usplit_1_1(X, Y, L, R, Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z > X ->
    usplit_1_1(Z, Y, L, [X | R], Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z == X ->
    usplit_1_1(X, Y, L, R, Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z > S ->
    usplit_1(S, Z, L, [], [[Y, X | R] | Rs]);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z == S ->
    usplit_1_1(X, Y, L, R, Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) ->
    usplit_1(Z, S, L, [], [[Y, X | R] | Rs]);
usplit_1_1(X, Y, [], R, Rs, S) ->
    rumergel([[S], [Y, X | R] | Rs], [], asc).

%% Descending.
usplit_2(X, Y, [Z | L], R, Rs) when Z < Y ->
    usplit_2(Y, Z, L, [X | R], Rs);
usplit_2(X, Y, [Z | L], R, Rs) when Z == Y ->
    usplit_2(X, Y, L, R, Rs);
usplit_2(X, Y, [Z | L], R, Rs) when Z < X ->
    usplit_2(Z, Y, L, [X | R], Rs);
usplit_2(X, Y, [Z | L], R, Rs) when Z == X ->
    usplit_2(X, Y, L, R, Rs);
usplit_2(X, Y, [Z | L], [], Rs) ->
    usplit_2(X, Y, L, [Z], Rs);
usplit_2(X, Y, [Z | L], R, Rs) ->
    usplit_2_1(X, Y, L, R, Rs, Z);
usplit_2(X, Y, [], R, Rs) ->
    umergel([[Y, X | R] | Rs], [], desc).

usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z < Y ->
    usplit_2_1(Y, Z, L, [X | R], Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z == Y ->
    usplit_2_1(X, Y, L, R, Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z < X ->
    usplit_2_1(Z, Y, L, [X | R], Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z == X ->
    usplit_2_1(X, Y, L, R, Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z < S ->
    usplit_2(S, Z, L, [], [[Y, X | R] | Rs]);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z == S ->
    usplit_2_1(X, Y, L, R, Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) ->
    usplit_2(Z, S, L, [], [[Y, X | R] | Rs]);
usplit_2_1(X, Y, [], R, Rs, S) ->
    umergel([[S], [Y, X | R] | Rs], [], desc).

%% umerge/1

umergel(L) ->
    umergel(L, [], asc).

umergel([[] | L], Acc, O) ->
    umergel(L, Acc, O);
umergel([T1, [H2 | T2], [H3 | T3] | L], Acc, asc) ->
    umergel(L, [umerge3_1(T1, [H2 | H3], T2, H2, [], T3, H3) | Acc], asc);
umergel([[H3 | T3], [H2 | T2], T1 | L], Acc, desc) ->
    umergel(L, [umerge3_1(T1, [H2 | H3], T2, H2, [], T3, H3) | Acc], desc);
umergel([A, [] | L], Acc, O) ->
    umergel([A | L], Acc, O);
umergel([A, B, [] | L], Acc, O) ->
    umergel([A, B | L], Acc, O);
umergel([[H1 | T1], T2 | L], Acc, asc) ->
    umergel(L, [umerge2_2(T1, T2, [], H1) | Acc], asc);
umergel([T2, [H1 | T1] | L], Acc, desc) ->
    umergel(L, [umerge2_2(T1, T2, [], H1) | Acc], desc);
umergel([L], [], _O) ->
    L;
umergel([L], Acc, O) ->
    rumergel([lists:reverse(L, []) | Acc], [], O);
umergel([], [], _O) ->
    [];
umergel([], Acc, O) ->
    rumergel(Acc, [], O).

rumergel([[H3 | T3], [H2 | T2], T1 | L], Acc, asc) ->
    rumergel(L, [rumerge3_1(T1, T2, H2, [], T3, H3) | Acc], asc);
rumergel([T1, [H2 | T2], [H3 | T3] | L], Acc, desc) ->
    rumergel(L, [rumerge3_1(T1, T2, H2, [], T3, H3) | Acc], desc);
rumergel([[H2 | T2], T1 | L], Acc, asc) ->
    rumergel(L, [rumerge2_1(T1, T2, [], H2) | Acc], asc);
rumergel([T1, [H2 | T2] | L], Acc, desc) ->
    rumergel(L, [rumerge2_1(T1, T2, [], H2) | Acc], desc);
rumergel([L], Acc, O) ->
    umergel([lists:reverse(L, []) | Acc], [], O);
rumergel([], Acc, O) ->
    umergel(Acc, [], O).

%% umerge3/3

%% Take L1 apart.
umerge3_1([H1 | T1], HdM, T2, H2, M, T3, H3) when H1 =< H2 ->
    umerge3_12(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_1([H1 | T1], HdM, T2, H2, M, T3, H3) when H2 == HdM ->
    umerge3_2(T1, H1, T2, H2, M, T3, H3);
umerge3_1([H1 | T1], HdM, T2, H2, M, T3, H3) ->
    umerge3_21(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_1([], HdM, T2, H2, M, T3, H3) when H2 == HdM ->
    umerge2_1(T2, T3, M, HdM, H3);
umerge3_1([], _HdM, T2, H2, M, T3, H3) when H2 =< H3 ->
    umerge2_1(T2, T3, [H2 | M], H2, H3);
umerge3_1([], HdM, T2, H2, M, T3, H3) when H3 == HdM ->
    umerge2_2(T2, T3, M, H2);
umerge3_1([], _HdM, T2, H2, M, T3, H3) ->
    umerge2_2(T2, T3, [H3 | M], H2).

%% Take L2 apart.
umerge3_2(T1, H1, [H2 | T2], HdM, M, T3, H3) when H1 =< H2 ->
    umerge3_12(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_2(T1, H1, [H2 | T2], HdM, M, T3, H3) ->
    umerge3_21(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_2(T1, H1, [], _HdM, M, T3, H3) when H1 =< H3 ->
    umerge2_1(T1, T3, [H1 | M], H1, H3);
umerge3_2(T1, H1, [], HdM, M, T3, H3) when H3 == HdM ->
    umerge2_2(T1, T3, M, H1);
umerge3_2(T1, H1, [], _HdM, M, T3, H3) ->
    umerge2_2(T1, T3, [H3 | M], H1).

% H1 =< H2. Inlined.
umerge3_12(T1, H1, T2, H2, M, T3, H3, _HdM) when H1 =< H3 ->
    umerge3_1(T1, H1, T2, H2, [H1 | M], T3, H3);
umerge3_12(T1, H1, T2, H2, M, T3, H3, HdM) when H3 == HdM ->
    umerge3_12_3(T1, H1, T2, H2, M, T3);
umerge3_12(T1, H1, T2, H2, M, T3, H3, _HdM) ->
    umerge3_12_3(T1, H1, T2, H2, [H3 | M], T3).

% H1 =< H2, take L3 apart.
umerge3_12_3(T1, H1, T2, H2, M, [H3 | T3]) when H1 =< H3 ->
    umerge3_1(T1, H1, T2, H2, [H1 | M], T3, H3);
umerge3_12_3(T1, H1, T2, H2, M, [H3 | T3]) ->
    umerge3_12_3(T1, H1, T2, H2, [H3 | M], T3);
umerge3_12_3(T1, H1, T2, H2, M, []) ->
    umerge2_1(T1, T2, [H1 | M], H1, H2).

% H1 > H2. Inlined.
umerge3_21(T1, H1, T2, H2, M, T3, H3, _HdM) when H2 =< H3 ->
    umerge3_2(T1, H1, T2, H2, [H2 | M], T3, H3);
umerge3_21(T1, H1, T2, H2, M, T3, H3, HdM) when H3 == HdM ->
    umerge3_21_3(T1, H1, T2, H2, M, T3);
umerge3_21(T1, H1, T2, H2, M, T3, H3, _HdM) ->
    umerge3_21_3(T1, H1, T2, H2, [H3 | M], T3).

% H1 > H2, take L3 apart.
umerge3_21_3(T1, H1, T2, H2, M, [H3 | T3]) when H2 =< H3 ->
    umerge3_2(T1, H1, T2, H2, [H2 | M], T3, H3);
umerge3_21_3(T1, H1, T2, H2, M, [H3 | T3]) ->
    umerge3_21_3(T1, H1, T2, H2, [H3 | M], T3);
umerge3_21_3(T1, H1, T2, H2, M, []) ->
    umerge2_2(T1, T2, [H2 | M], H1).

%% Take L1 apart.
rumerge3_1([H1 | T1], T2, H2, M, T3, H3) when H1 =< H2 ->
    rumerge3_12a(T1, H1, T2, H2, M, T3, H3);
rumerge3_1([H1 | T1], T2, H2, M, T3, H3) when H1 =< H3 ->
    rumerge3_21_3(T1, T2, H2, M, T3, H3, H1);
rumerge3_1([H1 | T1], T2, H2, M, T3, H3) ->
    rumerge3_1(T1, T2, H2, [H1 | M], T3, H3);
rumerge3_1([], T2, H2, M, T3, H3) when H2 =< H3 ->
    rumerge2_2(T2, T3, M, H3, H2);
rumerge3_1([], T2, H2, M, T3, H3) ->
    rumerge2_1(T2, T3, [H2 | M], H3).

% H1 =< H2. Inlined.
rumerge3_12a(T1, H1, T2, H2, M, T3, H3) when H2 =< H3 ->
    rumerge3_12_3(T1, T2, H2, M, T3, H3, H1);
rumerge3_12a(T1, H1, T2, H2, M, T3, H3) ->
    rumerge3_2(T1, T2, H2, M, T3, H3, H1).

%% Take L2 apart. H2M > H3. H2M > H2.
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) when H1 =< H2 ->
    % H2M > H1.
    rumerge3_12b(T1, H1, T2, H2, M, T3, H3, H2M);
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) when H1 == H2M ->
    rumerge3_1(T1, T2, H2, [H1 | M], T3, H3);
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) when H1 =< H3 ->
    % H2M > H1.
    rumerge3_21_3(T1, T2, H2, [H2M | M], T3, H3, H1);
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) ->
    % H2M > H1.
    rumerge3_1(T1, T2, H2, [H1, H2M | M], T3, H3);
rumerge3_2(T1, [], H2M, M, T3, H3, H1) when H1 == H2M ->
    rumerge2_1(T1, T3, [H1 | M], H3);
rumerge3_2(T1, [], H2M, M, T3, H3, H1) when H1 =< H3 ->
    rumerge2_2(T1, T3, [H2M | M], H3, H1);
rumerge3_2(T1, [], H2M, M, T3, H3, H1) ->
    rumerge2_1(T1, T3, [H1, H2M | M], H3).

% H1 =< H2. Inlined.
rumerge3_12b(T1, H1, T2, H2, M, T3, H3, H2M) when H2 =< H3 ->
    rumerge3_12_3(T1, T2, H2, [H2M | M], T3, H3, H1);
rumerge3_12b(T1, H1, T2, H2, M, T3, H3, H2M) ->
    rumerge3_2(T1, T2, H2, [H2M | M], T3, H3, H1).

% H1 =< H2, take L3 apart.
rumerge3_12_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H2 =< H3 ->
    rumerge3_12_3(T1, T2, H2, [H3M | M], T3, H3, H1);
rumerge3_12_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H2 == H3M ->
    rumerge3_2(T1, T2, H2, M, T3, H3, H1);
rumerge3_12_3(T1, T2, H2, M, [H3 | T3], H3M, H1) ->
    rumerge3_2(T1, T2, H2, [H3M | M], T3, H3, H1);
rumerge3_12_3(T1, T2, H2, M, [], H3M, H1) when H2 == H3M ->
    rumerge2_2(T1, T2, M, H2, H1);
rumerge3_12_3(T1, T2, H2, M, [], H3M, H1) ->
    rumerge2_2(T1, T2, [H3M | M], H2, H1).

% H1 > H2, take L3 apart.
rumerge3_21_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H1 =< H3 ->
    rumerge3_21_3(T1, T2, H2, [H3M | M], T3, H3, H1);
rumerge3_21_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H1 == H3M ->
    rumerge3_1(T1, T2, H2, [H1 | M], T3, H3);
rumerge3_21_3(T1, T2, H2, M, [H3 | T3], H3M, H1) ->
    rumerge3_1(T1, T2, H2, [H1, H3M | M], T3, H3);
rumerge3_21_3(T1, T2, H2, M, [], H3M, H1) when H1 == H3M ->
    rumerge2_1(T1, T2, [H1 | M], H2);
rumerge3_21_3(T1, T2, H2, M, [], H3M, H1) ->
    rumerge2_1(T1, T2, [H1, H3M | M], H2).

%% umerge/2

%% Elements from the first list are kept and prioritized.
umerge2_1([H1 | T1], T2, M, _HdM, H2) when H1 =< H2 ->
    umerge2_1(T1, T2, [H1 | M], H1, H2);
umerge2_1([H1 | T1], T2, M, HdM, H2) when H2 == HdM ->
    umerge2_2(T1, T2, M, H1);
umerge2_1([H1 | T1], T2, M, _HdM, H2) ->
    umerge2_2(T1, T2, [H2 | M], H1);
umerge2_1([], T2, M, HdM, H2) when H2 == HdM ->
    lists:reverse(T2, M);
umerge2_1([], T2, M, _HdM, H2) ->
    lists:reverse(T2, [H2 | M]).

umerge2_2(T1, [H2 | T2], M, H1) when H1 =< H2 ->
    umerge2_1(T1, T2, [H1 | M], H1, H2);
umerge2_2(T1, [H2 | T2], M, H1) ->
    umerge2_2(T1, T2, [H2 | M], H1);
umerge2_2(T1, [], M, H1) ->
    lists:reverse(T1, [H1 | M]).

%% rumerge/2

%% Elements from the first list are kept and prioritized.
rumerge2_1([H1 | T1], T2, M, H2) when H1 =< H2 ->
    rumerge2_2(T1, T2, M, H2, H1);
rumerge2_1([H1 | T1], T2, M, H2) ->
    rumerge2_1(T1, T2, [H1 | M], H2);
rumerge2_1([], T2, M, H2) ->
    lists:reverse(T2, [H2 | M]).

% H1 =< H2M.
rumerge2_2(T1, [H2 | T2], M, H2M, H1) when H1 =< H2 ->
    rumerge2_2(T1, T2, [H2M | M], H2, H1);
rumerge2_2(T1, [H2 | T2], M, H2M, H1) when H1 == H2M ->
    rumerge2_1(T1, T2, [H1 | M], H2);
rumerge2_2(T1, [H2 | T2], M, H2M, H1) ->
    rumerge2_1(T1, T2, [H1, H2M | M], H2);
rumerge2_2(T1, [], M, H2M, H1) when H1 == H2M ->
    lists:reverse(T1, [H1 | M]);
rumerge2_2(T1, [], M, H2M, H1) ->
    lists:reverse(T1, [H1, H2M | M]).

%% keysort/2

%% Ascending.
keysplit_1(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    case element(I, Z) of
	EZ when EY =< EZ ->
            keysplit_1(I, Y, EY, Z, EZ, L, [X | R], Rs);
        EZ when EX =< EZ ->
            keysplit_1(I, Z, EZ, Y, EY, L, [X | R], Rs);
        _EZ when R == [] ->
            keysplit_1(I, X, EX, Y, EY, L, [Z], Rs);
        EZ ->
            keysplit_1_1(I, X, EX, Y, EY, EZ, R, Rs, Z, L)
    end;
keysplit_1(I, X, _EX, Y, _EY, [], R, Rs) ->
    rkeymergel(I, [[Y, X | R] | Rs], [], asc).

keysplit_1_1(I, X, EX, Y, EY, ES, R, Rs, S, [Z | L]) ->
    case element(I, Z) of
	EZ when EY =< EZ ->
            keysplit_1_1(I, Y, EY, Z, EZ, ES, [X | R], Rs, S, L);
        EZ when EX =< EZ ->
            keysplit_1_1(I, Z, EZ, Y, EY, ES, [X | R], Rs, S, L);
        EZ when ES =< EZ ->
            keysplit_1(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
        EZ ->
            keysplit_1(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
keysplit_1_1(I, X, _EX, Y, _EY, _ES, R, Rs, S, []) ->
    rkeymergel(I, [[S], [Y, X | R] | Rs], [], asc).

%% Descending.
keysplit_2(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    case element(I, Z) of
	EZ when EY > EZ ->
            keysplit_2(I, Y, EY, Z, EZ, L, [X | R], Rs);
        EZ when EX > EZ ->
            keysplit_2(I, Z, EZ, Y, EY, L, [X | R], Rs);
        _EZ when R == [] ->
            keysplit_2(I, X, EX, Y, EY, L, [Z], Rs);
        EZ ->
            keysplit_2_1(I, X, EX, Y, EY, EZ, R, Rs, Z, L)
    end;
keysplit_2(I, X, _EX, Y, _EY, [], R, Rs) ->
    keymergel(I, [[Y, X | R] | Rs], [], desc).

keysplit_2_1(I, X, EX, Y, EY, ES, R, Rs, S, [Z | L]) ->
    case element(I, Z) of
        EZ when EY > EZ ->
            keysplit_2_1(I, Y, EY, Z, EZ, ES, [X | R], Rs, S, L);
        EZ when EX > EZ ->
            keysplit_2_1(I, Z, EZ, Y, EY, ES, [X | R], Rs, S, L);
        EZ when ES > EZ ->
            keysplit_2(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
        EZ ->
            keysplit_2(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
keysplit_2_1(I, X, _EX, Y, _EY, _ES, R, Rs, S, []) ->
    keymergel(I, [[S], [Y, X | R] | Rs], [], desc).

keymergel(I, [T1, [H2 | T2], [H3 | T3] | L], Acc, O) when O == asc ->
    M = keymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3, T3),
    keymergel(I, L, [M | Acc], O);
keymergel(I, [[H3 | T3], [H2 | T2], T1 | L], Acc, O) when O == desc ->
    M = keymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3, T3),
    keymergel(I, L, [M | Acc], O);
keymergel(I, [T1, [H2 | T2] | L], Acc, asc) ->
    keymergel(I, L, [keymerge2_1(I, T1, element(I,H2),H2,T2,[]) | Acc], asc);
keymergel(I, [[H2 | T2], T1 | L], Acc, desc) ->
    keymergel(I, L, [keymerge2_1(I, T1, element(I,H2),H2,T2,[]) | Acc], desc);
keymergel(_I, [L], [], _O) ->
    L;
keymergel(I, [L], Acc, O) ->
    rkeymergel(I, [lists:reverse(L, []) | Acc], [], O);
keymergel(I, [], Acc, O) ->
    rkeymergel(I, Acc, [], O).

rkeymergel(I, [[H3 | T3], [H2 | T2], T1 | L], Acc, O) when O == asc ->
    M = rkeymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3,T3),
    rkeymergel(I, L, [M | Acc], O);
rkeymergel(I, [T1, [H2 | T2], [H3 | T3] | L], Acc, O) when O == desc ->
    M = rkeymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3,T3),
    rkeymergel(I, L, [M | Acc], O);
rkeymergel(I, [[H2 | T2], T1 | L], Acc, asc) ->
    rkeymergel(I, L, [rkeymerge2_1(I, T1, element(I,H2),H2,T2,[]) | Acc],asc);
rkeymergel(I, [T1, [H2 | T2] | L], Acc, desc) ->
    rkeymergel(I, L, [rkeymerge2_1(I,T1, element(I,H2),H2,T2,[]) | Acc],desc);
rkeymergel(I, [L], Acc, O) ->
    keymergel(I, [lists:reverse(L, []) | Acc], [], O);
rkeymergel(I, [], Acc, O) ->
    keymergel(I, Acc, [], O).

%%% An extra argument, D, just to avoid some move instructions.

%% Take L1 apart.
keymerge3_1(I, [H1 | T1], M, D, E2, H2, T2, E3, H3, T3) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            keymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D);
        E1 ->
            keymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T2)
    end;
keymerge3_1(I, [], M, _D, E2, H2, T2, E3, H3, T3) when E2 =< E3 ->
    keymerge2_1(I, T2, E3, H3, T3, [H2 | M]);
keymerge3_1(I, [], M, _D, E2, H2, T2, _E3, H3, T3) ->
    keymerge2_2(I, T2, E2, H3, T3, M, H2).

%% Take L2 apart.
keymerge3_2(I, E1, H1, T1, [H2 | T2], M, D, E3, H3, T3) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            keymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T1);
        E2 ->
            keymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D)
    end;
keymerge3_2(I, E1, H1, T1, [], M, _D, E3, H3, T3) when E1 =< E3 ->
    keymerge2_1(I, T1, E3, H3, T3, [H1 | M]);
keymerge3_2(I, E1, H1, T1, [], M, _D, _E3, H3, T3) ->
    keymerge2_2(I, T1, E1, H3, T3, M, H1).

% E1 =< E2. Inlined.
keymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D) when E1 =< E3 ->
    keymerge3_1(I, T1, [H1 | M], D, E2, H2, T2, E3, H3, T3);
keymerge3_12(I, E1, H1, T1, E2, H2, T2, _E3, H3, T3, M, _D) ->
    keymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]).

% E1 =< E2, take L3 apart.
keymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
	E3 when E1 =< E3 ->
            keymerge3_1(I, T1, [H1 | M], T1, E2, H2, T2, E3, H3, T3);
        _E3 ->
            keymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M])
    end;
keymerge3_12_3(I, _E1, H1, T1, E2, H2, T2, [], M) ->
    keymerge2_1(I, T1, E2, H2, T2, [H1 | M]).

% E1 > E2. Inlined.
keymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D) when E2 =< E3 ->
    keymerge3_2(I, E1, H1, T1, T2, [H2 | M], D, E3, H3, T3);
keymerge3_21(I, E1, H1, T1, E2, H2, T2, _E3, H3, T3, M, _D) ->
    keymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]).

% E1 > E2, take L3 apart.
keymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
	E3 when E2 =< E3 ->
            keymerge3_2(I, E1, H1, T1, T2, [H2 | M], T2, E3, H3, T3);
        _E3 ->
            keymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M])
    end;
keymerge3_21_3(I, E1, H1, T1, _E2, H2, T2, [], M) ->
    keymerge2_2(I, T1, E1, H2, T2, M, H1).

%% Take L1 apart.
rkeymerge3_1(I, [H1 | T1], M, D, E2, H2, T2, E3, H3, T3) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            rkeymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T2);
        E1 ->
            rkeymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D)
    end;
rkeymerge3_1(I, [], M, _D, E2, H2, T2, E3, H3, T3) when E2 =< E3 ->
    rkeymerge2_2(I, E2, T2, H3, T3, M, H2);
rkeymerge3_1(I, [], M, _D, _E2, H2, T2, E3, H3, T3) ->
    rkeymerge2_1(I, T2, E3, H3, T3, [H2 | M]).

%% Take L2 apart.
rkeymerge3_2(I, E1, H1, T1, [H2 | T2], M, D, E3, H3, T3) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            rkeymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D);
        E2 ->
            rkeymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T1)
    end;
rkeymerge3_2(I, E1, H1, T1, [], M, _D, E3, H3, T3) when E1 =< E3 ->
    rkeymerge2_2(I, E1, T1, H3, T3, M, H1);
rkeymerge3_2(I, _E1, H1, T1, [], M, _D, E3, H3, T3) ->
    rkeymerge2_1(I, T1, E3, H3, T3, [H1 | M]).

% E1 =< E2. Inlined.
rkeymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, _D) when E2 =< E3 ->
    rkeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
rkeymerge3_12(I, E1, H1, T1, _E2, H2, T2, E3, H3, T3, M, D) ->
    rkeymerge3_2(I, E1, H1, T1, T2, [H2 | M], D, E3, H3, T3).

% E1 =< E2, take L3 apart.
rkeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
	E3 when E2 =< E3 ->
            rkeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
        E3 ->
            rkeymerge3_2(I, E1, H1, T1, T2, [H2 | M], T2, E3, H3, T3)
    end;
rkeymerge3_12_3(I, E1, H1, T1, _E2, H2, T2, [], M) ->
    rkeymerge2_2(I, E1, T1, H2, T2, M, H1).

% E1 > E2. Inlined.
rkeymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, _D) when E1 =< E3 ->
    rkeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
rkeymerge3_21(I, _E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D) ->
    rkeymerge3_1(I, T1, [H1 | M], D, E2, H2, T2, E3, H3, T3).

% E1 > E2, take L3 apart.
rkeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
	E3 when E1 =< E3 ->
            rkeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
        E3 ->
            rkeymerge3_1(I, T1, [H1 | M], T1, E2, H2, T2, E3, H3, T3)
    end;
rkeymerge3_21_3(I, _E1, H1, T1, E2, H2, T2, [], M) ->
    rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M]).

%% keymerge/3

%% Elements from the first list are prioritized.
keymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            keymerge2_1(I, T1, E2, H2, T2, [H1 | M]);
        E1 ->
            keymerge2_2(I, T1, E1, H2, T2, M, H1)
    end;
keymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

keymerge2_2(I, T1, E1, HdM, [H2 | T2], M, H1) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            keymerge2_1(I, T1, E2, H2, T2, [H1, HdM | M]);
        _E2 ->
            keymerge2_2(I, T1, E1, H2, T2, [HdM | M], H1)
    end;
keymerge2_2(_I, T1, _E1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% rkeymerge/3

rkeymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            rkeymerge2_2(I, E1, T1, H2, T2, M, H1);
        _E1 ->
            rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M])
    end;
rkeymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rkeymerge2_2(I, E1, T1, HdM, [H2 | T2], M, H1) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            rkeymerge2_2(I, E1, T1, H2, T2, [HdM | M], H1);
        E2 ->
            rkeymerge2_1(I, T1, E2, H2, T2, [H1, HdM | M])
    end;
rkeymerge2_2(_I, _E1, T1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% ukeysort/2

%% Ascending.
ukeysplit_1(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    case element(I, Z) of
        EZ when EY == EZ ->
            ukeysplit_1(I, X, EX, Y, EY, L, R, Rs);
	EZ when EY < EZ ->
            ukeysplit_1(I, Y, EY, Z, EZ, L, [X | R], Rs);
        EZ when EX == EZ ->
            ukeysplit_1(I, X, EX, Y, EY, L, R, Rs);
        EZ when EX < EZ ->
            ukeysplit_1(I, Z, EZ, Y, EY, L, [X | R], Rs);
        _EZ when R == [] ->
            ukeysplit_1(I, X, EX, Y, EY, L, [Z], Rs);
        EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, Z, EZ)
    end;
ukeysplit_1(I, X, _EX, Y, _EY, [], R, Rs) ->
    rukeymergel(I, [[Y, X | R] | Rs], []).

ukeysplit_1_1(I, X, EX, Y, EY, [Z | L], R, Rs, S, ES) ->
    case element(I, Z) of
        EZ when EY == EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
        EZ when EY < EZ ->
            ukeysplit_1_1(I, Y, EY, Z, EZ, L, [X | R], Rs, S, ES);
        EZ when EX == EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
        EZ when EX < EZ ->
            ukeysplit_1_1(I, Z, EZ, Y, EY, L, [X | R], Rs, S, ES);
        EZ when ES == EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
        EZ when ES < EZ ->
            ukeysplit_1(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
        EZ ->
            ukeysplit_1(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
ukeysplit_1_1(I, X, _EX, Y, _EY, [], R, Rs, S, _ES) ->
    rukeymergel(I, [[S], [Y, X | R] | Rs], []).

%% Descending.
ukeysplit_2(I, Y, EY, [Z | L], R) ->
    case element(I, Z) of
	EZ when EY == EZ ->
            ukeysplit_2(I, Y, EY, L, R);
	EZ when EY < EZ ->
            ukeysplit_1(I, Y, EY, Z, EZ, L, [], [lists:reverse(R, [])]);
        EZ ->
            ukeysplit_2(I, Z, EZ, L, [Y | R])
    end;
ukeysplit_2(_I, Y, _EY, [], R) ->
    [Y | R].

-dialyzer({no_improper_lists, ukeymergel/3}).

ukeymergel(I, [T1, [H2 | T2], [H3 | T3] | L], Acc) ->
    %% The fourth argument, [H2 | H3] (=HdM), may confuse type
    %% checkers. Its purpose is to ensure that the tests H2 == HdM
    %% and H3 == HdM in ukeymerge3_1 will always fail as long as M == [].
    M = ukeymerge3_1(I, T1, Acc, [H2 | H3], element(I, H2), H2, T2, [],
                     element(I, H3), H3, T3),
    ukeymergel(I, L, [M | Acc]);
ukeymergel(I, [[H1 | T1], T2 | L], Acc) ->
    ukeymergel(I, L, [ukeymerge2_2(I, T1, element(I, H1), H1, T2, []) | Acc]);
ukeymergel(_I, [L], []) ->
    L;
ukeymergel(I, [L], Acc) ->
    rukeymergel(I, [lists:reverse(L, []) | Acc], []);
ukeymergel(I, [], Acc) ->
    rukeymergel(I, Acc, []).

rukeymergel(I, [[H3 | T3], [H2 | T2], T1 | L], Acc) ->
    M = rukeymerge3_1(I, T1, Acc, [], element(I, H2), H2, T2, [],
                      element(I, H3), H3, T3),
    rukeymergel(I, L, [M | Acc]);
rukeymergel(I, [[H2 | T2], T1 | L], Acc) ->
    rukeymergel(I, L, [rukeymerge2_1(I, T1, element(I,H2), T2, [], H2)|Acc]);
rukeymergel(I, [L], Acc) ->
    ukeymergel(I, [lists:reverse(L, []) | Acc], []);
rukeymergel(I, [], Acc) ->
    ukeymergel(I, Acc, []).

%%% An extra argument, D, just to avoid some move instructions.

%% Take L1 apart.
ukeymerge3_1(I, [H1 | T1], D, HdM, E2, H2, T2, M, E3, H3, T3) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, D);
        E1 when E2 == HdM ->
            ukeymerge3_2(I, E1, T1, H1, T2, HdM, T2, M, E3, H3, T3);
        E1 ->
            ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, T2)
    end;
ukeymerge3_1(I, [], _D, HdM, E2, _H2, T2, M, E3, H3, T3) when E2 == HdM ->
    ukeymerge2_1(I, T2, E3, HdM, T3, M, H3);
ukeymerge3_1(I, [], _D, _HdM, E2, H2, T2, M, E3, H3, T3) when E2 =< E3 ->
    ukeymerge2_1(I, T2, E3, E2, T3, [H2 | M], H3);
ukeymerge3_1(I, [], _D, HdM, E2, H2, T2, M, E3, _H3, T3) when E3 == HdM ->
    ukeymerge2_2(I, T2, E2, H2, T3, M);
ukeymerge3_1(I, [], _D, _HdM, E2, H2, T2, M, _E3, H3, T3) ->
    ukeymerge2_2(I, T2, E2, H2, T3, [H3 | M]).

%% Take L2 apart.
ukeymerge3_2(I, E1, T1, H1, [H2 | T2], HdM, D, M, E3, H3, T3) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, T1);
        E2 ->
            ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, D)
    end;
ukeymerge3_2(I, E1, T1, H1, [], _HdM, _D, M, E3, H3, T3) when E1 =< E3 ->
    ukeymerge2_1(I, T1, E3, E1, T3, [H1 | M], H3);
ukeymerge3_2(I, E1, T1, H1, [], HdM, _D, M, E3, _H3, T3) when E3 == HdM ->
    ukeymerge2_2(I, T1, E1, H1, T3, M);
ukeymerge3_2(I, E1, T1, H1, [], _HdM, _D, M, _E3, H3, T3) ->
    ukeymerge2_2(I, T1, E1, H1, T3, [H3 | M]).

% E1 =< E2. Inlined.
ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, _HdM, D) 
                                                             when E1 =< E3 ->
    ukeymerge3_1(I, T1, D, E1, E2, H2, T2, [H1 | M], E3, H3, T3);
ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, _H3, T3, M, HdM, _D) 
                                                             when E3 == HdM ->
    ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, M, T3);
ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, _E3, H3, T3, M, _HdM, _D) ->
    ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3).

% E1 =< E2, take L3 apart.
ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, M, [H3 | T3]) ->
    case element(I, H3) of
	E3 when E1 =< E3 ->
            ukeymerge3_1(I, T1, T1, E1, E2, H2, T2, [H1 | M], E3, H3, T3);
        _E3 ->
            ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3)
    end;
ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, M, []) ->
    ukeymerge2_1(I, T1, E2, E1, T2, [H1 | M], H2).

% E1 > E2. Inlined.
ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, _HdM, D) 
                                                             when E2 =< E3 ->
    ukeymerge3_2(I, E1, T1, H1, T2, E2, D, [H2 | M], E3, H3, T3);
ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, _H3, T3, M, HdM, _D) 
                                                             when E3 == HdM ->
    ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, M, T3);
ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, _E3, H3, T3, M, _HdM, _D) ->
    ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3).

% E1 > E2, take L3 apart.
ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, M, [H3 | T3]) ->
    case element(I, H3) of
        E3 when E2 =< E3 ->
            ukeymerge3_2(I, E1, T1, H1, T2, E2, T2, [H2 | M], E3, H3, T3);
        _E3 ->
            ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3)
    end;
ukeymerge3_21_3(I, E1, T1, H1, _E2, H2, T2, M, []) ->
    ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M]).

%%% Two extra arguments, D1 and D2, just to avoid some move instructions.

%% Take L1 apart.
rukeymerge3_1(I, [H1 | T1], D1, D2, E2, H2, T2, M, E3, H3, T3) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
	    rukeymerge3_12a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M);
        E1 ->
            rukeymerge3_21a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D1, D2)
    end;
rukeymerge3_1(I, [], _D1, _D2, E2, H2, T2, M, E3, H3, T3) when E2 =< E3 ->
    rukeymerge2_2(I, T2, E2, T3, M, E3, H3, H2);
rukeymerge3_1(I, [], _D1, _D2, _E2, H2, T2, M, E3, H3, T3) ->
    rukeymerge2_1(I, T2, E3, T3, [H2 | M], H3).

% E1 =< E2. Inlined.
rukeymerge3_12a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M) when E2 =< E3 ->
    rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, E3, H3, T3);
rukeymerge3_12a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M) ->
    rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, M, E3, H3, T3).

% E1 > E2. Inlined
rukeymerge3_21a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, _D1, _D2) 
                                                              when E1 =< E3 ->
    rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, M, E3, H3, T3);
rukeymerge3_21a(I, _E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D1, D2) ->
    rukeymerge3_1(I, T1, D1, D2, E2, H2, T2, [H1 | M], E3, H3, T3).

%% Take L2 apart. E2M > E3. E2M > E2.
rukeymerge3_2(I, E1, H1, T1, [H2 | T2], H2M, E2M, M, E3, H3, T3) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            % E2M > E1.
	    rukeymerge3_12b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M);
        E2 when E1 == E2M ->
            rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1 | M], E3, H3, T3);
        E2 ->
            % E2M > E1.
	    rukeymerge3_21b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M)
    end;
rukeymerge3_2(I, E1, H1, T1, [], _H2M, E2M, M, E3, H3, T3) when E1 == E2M ->
    rukeymerge2_1(I, T1, E3, T3, [H1 | M], H3);
rukeymerge3_2(I, E1, H1, T1, [], H2M, _E2M, M, E3, H3, T3) when E1 =< E3 ->
    rukeymerge2_2(I, T1, E1, T3, [H2M | M], E3, H3, H1);
rukeymerge3_2(I, _E1, H1, T1, [], H2M, _E2M, M, E3, H3, T3) ->
    rukeymerge2_1(I, T1, E3, T3, [H1, H2M | M], H3).

% E1 =< E2. Inlined.
rukeymerge3_12b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M) 
                                                             when E2 =< E3 ->
    rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H2M | M], E3, H3, T3);
rukeymerge3_12b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M) ->
    rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, [H2M | M], E3, H3, T3).

% E1 > E2. Inlined
rukeymerge3_21b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M,H2M) when E1 =< E3 ->
    rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H2M | M], E3, H3, T3);
rukeymerge3_21b(I, _E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M) ->
    rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1, H2M | M], E3, H3, T3).

% E1 =< E2, take L3 apart.
rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, E3M, H3M, [H3 | T3]) ->
    case element(I, H3) of
	E3 when E2 =< E3 ->
            rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H3M | M], E3, H3, T3);
        E3 when E2 == E3M ->
            rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, M, E3, H3, T3);
        E3 ->
            rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, [H3M | M], E3, H3, T3)
    end;
rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, E3M, _H3M, []) when E2 == E3M ->
    rukeymerge2_2(I, T1, E1, T2, M, E2, H2, H1);
rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, _E3M, H3M, []) ->
    rukeymerge2_2(I, T1, E1, T2, [H3M | M], E2, H2, H1).

% E1 > E2, take L3 apart.
rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, M, E3M, H3M, [H3 | T3]) ->
    case element(I, H3) of
	E3 when E1 =< E3 ->
            rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H3M | M], E3, H3, T3);
        E3 when E1 == E3M ->
            rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1 | M], E3, H3, T3);
        E3 ->
            rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1,H3M | M], E3, H3, T3)
    end;
rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, M, E3M, _H3M, []) when E1 == E3M ->
    rukeymerge2_1(I, T1, E2, T2, [H1 | M], H2);
rukeymerge3_21_3(I, _E1, H1, T1, E2, H2, T2, M, _E3M, H3M, []) ->
    rukeymerge2_1(I, T1, E2, T2, [H1, H3M | M], H2).

%% ukeymerge/3

%% Elements from the first list are kept and prioritized.
ukeymerge2_1(I, [H1 | T1], E2, HdM, T2, M, H2) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            ukeymerge2_1(I, T1, E2, E1, T2, [H1 | M], H2);
        E1 when E2 == HdM ->
            ukeymerge2_2(I, T1, E1, H1, T2, M);
        E1 ->
            ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
ukeymerge2_1(_I, [], E2, HdM, T2, M, _H2) when E2 == HdM ->
    lists:reverse(T2, M);
ukeymerge2_1(_I, [], _E2, _HdM, T2, M, H2) ->
    lists:reverse(T2, [H2 | M]).

ukeymerge2_2(I, T1, E1, H1, [H2 | T2], M) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            ukeymerge2_1(I, T1, E2, E1, T2, [H1 | M], H2);
        _E2 ->
            ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
ukeymerge2_2(_I, T1, _E1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rukeymerge/3

rukeymerge2_1(I, [H1 | T1], E2, T2, M, H2) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            rukeymerge2_2(I, T1, E1, T2, M, E2, H2, H1);
        _E1 ->
            rukeymerge2_1(I, T1, E2, T2, [H1 | M], H2)
    end;
rukeymerge2_1(_I, [], _E2, T2, M, H2) ->
    lists:reverse(T2, [H2 | M]).

rukeymerge2_2(I, T1, E1, [H2 | T2], M, E2M, H2M, H1) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            rukeymerge2_2(I, T1, E1, T2, [H2M | M], E2, H2, H1);
        E2 when E1 == E2M ->
            rukeymerge2_1(I, T1, E2, T2, [H1 | M], H2);
        E2 ->
            rukeymerge2_1(I, T1, E2, T2, [H1, H2M | M], H2)
    end;
rukeymerge2_2(_I, T1, E1, [], M, E2M, _H2M, H1) when E1 == E2M ->
    lists:reverse(T1, [H1 | M]);
rukeymerge2_2(_I, T1, _E1, [], M, _E2M, H2M, H1) ->
    lists:reverse(T1, [H1, H2M | M]).

%% sort/2

%% Ascending.
fsplit_1(Y, X, Fun, [Z | L], R, Rs) ->
    case Fun(Y, Z) of
        true -> 
            fsplit_1(Z, Y, Fun, L, [X | R], Rs);
        false ->
            case Fun(X, Z) of
                true ->
                    fsplit_1(Y, Z, Fun, L, [X | R], Rs);
                false when R == [] ->
                    fsplit_1(Y, X, Fun, L, [Z], Rs);
                false ->
                    fsplit_1_1(Y, X, Fun, L, R, Rs, Z)
            end
    end;
fsplit_1(Y, X, Fun, [], R, Rs) ->
    rfmergel([[Y, X | R] | Rs], [], Fun, asc).

fsplit_1_1(Y, X, Fun, [Z | L], R, Rs, S) ->
    case Fun(Y, Z) of
        true ->
            fsplit_1_1(Z, Y, Fun, L, [X | R], Rs, S);
        false ->
            case Fun(X, Z) of
                true ->
                    fsplit_1_1(Y, Z, Fun, L, [X | R], Rs, S);
                false ->
                    case Fun(S, Z) of
                        true ->
                            fsplit_1(Z, S, Fun, L, [], [[Y, X | R] | Rs]);
                        false ->
                            fsplit_1(S, Z, Fun, L, [], [[Y, X | R] | Rs])
                    end
            end
    end;
fsplit_1_1(Y, X, Fun, [], R, Rs, S) ->
    rfmergel([[S], [Y, X | R] | Rs], [], Fun, asc).

%% Descending.
fsplit_2(Y, X, Fun, [Z | L], R, Rs) ->
    case Fun(Y, Z) of
        false -> 
            fsplit_2(Z, Y, Fun, L, [X | R], Rs);
        true ->
            case Fun(X, Z) of
                false ->
                    fsplit_2(Y, Z, Fun, L, [X | R], Rs);
                true when R == [] ->
                    fsplit_2(Y, X, Fun, L, [Z], Rs);
                true ->
                    fsplit_2_1(Y, X, Fun, L, R, Rs, Z)
            end
    end;
fsplit_2(Y, X, Fun, [], R, Rs) ->
    fmergel([[Y, X | R] | Rs], [], Fun, desc).

fsplit_2_1(Y, X, Fun, [Z | L], R, Rs, S) ->
    case Fun(Y, Z) of
        false ->
            fsplit_2_1(Z, Y, Fun, L, [X | R], Rs, S);
        true ->
            case Fun(X, Z) of
                false ->
                    fsplit_2_1(Y, Z, Fun, L, [X | R], Rs, S);
                true ->
                    case Fun(S, Z) of
                        false ->
                            fsplit_2(Z, S, Fun, L, [], [[Y, X | R] | Rs]);
                        true ->
                            fsplit_2(S, Z, Fun, L, [], [[Y, X | R] | Rs])
                    end
            end
    end;
fsplit_2_1(Y, X, Fun, [], R, Rs, S) ->
    fmergel([[S], [Y, X | R] | Rs], [], Fun, desc).

fmergel([T1, [H2 | T2] | L], Acc, Fun, asc) ->
    fmergel(L, [fmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, asc);
fmergel([[H2 | T2], T1 | L], Acc, Fun, desc) ->
    fmergel(L, [fmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, desc);
fmergel([L], [], _Fun, _O) ->
    L;
fmergel([L], Acc, Fun, O) ->
    rfmergel([lists:reverse(L, []) | Acc], [], Fun, O);
fmergel([], Acc, Fun, O) ->
    rfmergel(Acc, [], Fun, O).

rfmergel([[H2 | T2], T1 | L], Acc, Fun, asc) ->
    rfmergel(L, [rfmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, asc);
rfmergel([T1, [H2 | T2] | L], Acc, Fun, desc) ->
    rfmergel(L, [rfmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, desc);
rfmergel([L], Acc, Fun, O) ->
    fmergel([lists:reverse(L, []) | Acc], [], Fun, O);
rfmergel([], Acc, Fun, O) ->
    fmergel(Acc, [], Fun, O).

%% merge/3 

%% Elements from the first list are prioritized.
fmerge2_1([H1 | T1], H2, Fun, T2, M) ->
    case Fun(H1, H2) of
        true ->
            fmerge2_1(T1, H2, Fun, T2, [H1 | M]);
        false ->
            fmerge2_2(H1, T1, Fun, T2, [H2 | M])
    end;
fmerge2_1([], H2, _Fun, T2, M) ->
    lists:reverse(T2, [H2 | M]).

fmerge2_2(H1, T1, Fun, [H2 | T2], M) ->
    case Fun(H1, H2) of
        true ->
            fmerge2_1(T1, H2, Fun, T2, [H1 | M]);
        false ->
            fmerge2_2(H1, T1, Fun, T2, [H2 | M])
    end;
fmerge2_2(H1, T1, _Fun, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rmerge/3

rfmerge2_1([H1 | T1], H2, Fun, T2, M) ->
    case Fun(H1, H2) of
        true ->
            rfmerge2_2(H1, T1, Fun, T2, [H2 | M]);
        false ->
            rfmerge2_1(T1, H2, Fun, T2, [H1 | M])
    end;
rfmerge2_1([], H2, _Fun, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rfmerge2_2(H1, T1, Fun, [H2 | T2], M) ->
    case Fun(H1, H2) of
        true ->
            rfmerge2_2(H1, T1, Fun, T2, [H2 | M]);
        false ->
            rfmerge2_1(T1, H2, Fun, T2, [H1 | M])
    end;
rfmerge2_2(H1, T1, _Fun, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% usort/2

%% Ascending. X < Y
ufsplit_1(Y, X, Fun, [Z | L], R, Rs) ->
    case Fun(Y, Z) of
        true ->
            case Fun(Z, Y) of
                true -> % Z equal to Y
                    ufsplit_1(Y, X, Fun, L, R, Rs);
                false ->
                    ufsplit_1(Z, Y, Fun, L, [X | R], Rs)
            end;
        false ->
            case Fun(X, Z) of
                true ->
                    case Fun(Z, X) of
                        true -> % Z equal to X
                            ufsplit_1(Y, X, Fun, L, R, Rs);
                        false ->
                            ufsplit_1(Y, Z, Fun, L, [X | R], Rs)
                    end;
                false when R == [] ->
                    ufsplit_1(Y, X, Fun, L, [Z], Rs);
                false ->
                    ufsplit_1_1(Y, X, Fun, L, R, Rs, Z)
            end
    end;
ufsplit_1(Y, X, Fun, [], R, Rs) ->
    rufmergel([[Y, X | R] | Rs], [], Fun).

%% X < Y
ufsplit_1_1(Y, X, Fun, [Z | L], R, Rs, S) ->
    case Fun(Y, Z) of
        true ->
            case Fun(Z, Y) of
                true -> % Z equal to Y
                    ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
                false ->
                    ufsplit_1_1(Z, Y, Fun, L, [X | R], Rs, S)
            end;
        false ->
            case Fun(X, Z) of
                true ->
                    case Fun(Z, X) of
                        true -> % Z equal to X
                            ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
                        false ->
                            ufsplit_1_1(Y, Z, Fun, L, [X | R], Rs, S)
                    end;
                false ->
                    case Fun(S, Z) of
                        true ->
                            case Fun(Z, S) of
                                true -> % Z equal to S
                                    ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
                                false ->
                                    ufsplit_1(Z, S, Fun, L, [], [[Y, X | R] | Rs])
                            end;
                        false ->
                            ufsplit_1(S, Z, Fun, L, [], [[Y, X | R] | Rs])
                    end
            end
    end;
ufsplit_1_1(Y, X, Fun, [], R, Rs, S) ->
    rufmergel([[S], [Y, X | R] | Rs], [], Fun).

%% Descending.
ufsplit_2(Y, [Z | L], Fun, R) ->
    case Fun(Y, Z) of
        true ->
            case Fun(Z, Y) of
                true -> % Z equal to Y
                    ufsplit_2(Y, L, Fun, R);
                false ->
                    ufsplit_1(Z, Y, Fun, L, [], [lists:reverse(R, [])])
            end;
        false ->
            ufsplit_2(Z, L, Fun, [Y | R])
    end;
ufsplit_2(Y, [], _Fun, R) ->
    [Y | R].

ufmergel([[H1 | T1], T2 | L], Acc, Fun) ->
    ufmergel(L, [ufmerge2_2(H1, T1, Fun, T2, []) | Acc], Fun);
ufmergel([L], [], _Fun) ->
    L;
ufmergel([L], Acc, Fun) ->
    rufmergel([lists:reverse(L, []) | Acc], [], Fun);
ufmergel([], Acc, Fun) ->
    rufmergel(Acc, [], Fun).

rufmergel([[H2 | T2], T1 | L], Acc, Fun) ->
    rufmergel(L, [rufmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun);
rufmergel([L], Acc, Fun) ->
    ufmergel([lists:reverse(L, []) | Acc], [], Fun);
rufmergel([], Acc, Fun) ->
    ufmergel(Acc, [], Fun).

%% umerge/3

%% Elements from the first list are kept and prioritized.
%% HdM before H2.
ufmerge2_1([H1 | T1], H2, Fun, T2, M, HdM) ->
    case Fun(H1, H2) of
        true ->
            ufmerge2_1(T1, H2, Fun, T2, [H1 | M], H1);
        false ->
            case Fun(H2, HdM) of
                true -> % HdM equal to H2
                    ufmerge2_2(H1, T1, Fun, T2, M);
                false ->
                    ufmerge2_2(H1, T1, Fun, T2, [H2 | M])
            end
    end;
ufmerge2_1([], H2, Fun, T2, M, HdM) ->
    case Fun(H2, HdM) of
        true -> % HdM equal to H2
            lists:reverse(T2, M);
        false ->
            lists:reverse(T2, [H2 | M])
    end.

ufmerge2_2(H1, T1, Fun, [H2 | T2], M) ->
    case Fun(H1, H2) of
        true ->
            ufmerge2_1(T1, H2, Fun, T2, [H1 | M], H1);
        false ->
            ufmerge2_2(H1, T1, Fun, T2, [H2 | M])
    end;
ufmerge2_2(H1, T1, _Fun, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rumerge/3

rufmerge2_1([H1 | T1], H2, Fun, T2, M) ->
    case Fun(H1, H2) of
        true ->
            rufmerge2_2(H1, T1, Fun, T2, M, H2);
        false ->
            rufmerge2_1(T1, H2, Fun, T2, [H1 | M])
    end;
rufmerge2_1([], H2, _Fun, T2, M) ->
    lists:reverse(T2, [H2 | M]).

%% H1 before H2M
rufmerge2_2(H1, T1, Fun, [H2 | T2], M, H2M) ->
    case Fun(H1, H2) of
        true ->
            rufmerge2_2(H1, T1, Fun, T2, [H2M | M], H2);
        false ->
            case Fun(H2M, H1) of
                true -> % H2M equal to H1
                    rufmerge2_1(T1, H2, Fun, T2, [H1 | M]);
                false ->
                    rufmerge2_1(T1, H2, Fun, T2, [H1, H2M | M])
            end
    end;
rufmerge2_2(H1, T1, Fun, [], M, H2M) ->
    case Fun(H2M, H1) of
        true -> 
            lists:reverse(T1, [H1 | M]);
        false ->
            lists:reverse(T1, [H1, H2M | M])
    end.

