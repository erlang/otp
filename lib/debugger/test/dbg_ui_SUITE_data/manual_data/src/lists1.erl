%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

%%
%% Purpose : Basic lists processing functions.

-module(lists1).


-export([member/2, append/2, append/1, subtract/2, reverse/1, reverse/2,
	 nth/2, nthtail/2, prefix/2, suffix/2, last/1,
	 seq/2, seq/3, sum/1, duplicate/2, min/1, max/1, sublist/2, sublist/3,
	 delete/2, sort/1, merge/2, concat/1,
	 flatten/1, flatten/2, flat_length/1, flatlength/1,
	 keymember/3, keysearch/3, keydelete/3, keyreplace/4,
	 keysort/2, keymerge/3, keymap/3, keymap/4]).

-export([all/2,any/2,map/2,flatmap/2,foldl/3,foldr/3,filter/2,zf/2,
	 mapfoldl/3,mapfoldr/3,foreach/2,takewhile/2,dropwhile/2,splitwith/2]).
-export([all/3,any/3,map/3,flatmap/3,foldl/4,foldr/4,filter/3,zf/3,
	 mapfoldl/4,mapfoldr/4,foreach/3]).

%% member(X, L) -> (true | false)
%%  test if X is a member of the list L

member(X, [X|_]) -> true;
member(X, [_|Y]) ->
	member(X, Y);
member(X, []) -> false.

%% append(X, Y) appends lists X and Y

append(L1, L2) -> L1 ++ L2.

%% append(L) appends the list of lists L

append([E]) -> E;
append([H|T]) -> H ++ append(T);
append([]) -> [].

%% subtract(List1, List2) subtract elements in List2 form List1.

subtract(L1, L2) -> L1 -- L2.

%% reverse(L) reverse all elements in the list L

reverse(X) -> reverse(X, []).

reverse([H|T], Y) ->
    reverse(T, [H|Y]);
reverse([], X) -> X.

%% nth(N, L) returns the N`th element of the list L
%% nthtail(N, L) returns the N`th tail of the list L

nth(1, [H|T]) -> H;
nth(N, [_|T]) when N > 1 ->
    nth(N - 1, T).

nthtail(1, [H|T]) -> T;
nthtail(N, [H|T]) when N > 1 ->
    nthtail(N - 1, T);
nthtail(0, L) when list(L) -> L.

%% prefix(Prefix, List) -> (true | false)

prefix([X|PreTail], [X|Tail]) ->
    prefix(PreTail, Tail);
prefix([], List) -> true;
prefix(_,_) -> false.


%% suffix(Suffix, List) -> (true | false)

suffix(Suffix, Suffix) -> true;
suffix(Suffix, [_|Tail]) ->
    suffix(Suffix, Tail);
suffix(Suffix, []) -> false.

%% last(List) returns the last element in a list.

last([E]) -> E;
last([E|Es]) ->
    last(Es).

%% seq(Min, Max) -> [Min,Min+1, ..., Max]
%% seq(Min, Max, Incr) -> [Min,Min+Incr, ..., Max]
%%  returns the sequence Min..Max
%%  Min <= Max and Min and Max must be integers

seq(Min, Max) when integer(Min), integer(Max), Min =< Max ->
    seq(Min, Max, 1, []).

seq(Min, Max, Incr) ->
    seq(Min, Min + ((Max-Min) div Incr) * Incr, Incr, []).

seq(Min, Min, I, L) -> [Min|L];
seq(Min, Max, I, L) -> seq(Min, Max-I, I, [Max|L]).

%% sum(L) suns the sum of the elements in L

sum(L)          -> sum(L, 0).
sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum.

%% duplicate(N, X) -> [X,X,X,.....,X]  (N times)
%%   return N copies of X

duplicate(N, X) when integer(N), N >= 0 -> duplicate(N, X, []).

duplicate(0, _, L) -> L;
duplicate(N, X, L) -> duplicate(N-1, X, [X|L]).


%% min(L) -> returns the minimum element of the list L

min([H|T]) -> min(T, H).

min([H|T], Min) when H < Min -> min(T, H);
min([_|T], Min)              -> min(T, Min);
min([],    Min)              -> Min.

%% max(L) -> returns the maximum element of the list L

max([H|T]) -> max(T, H).

max([H|T], Max) when H > Max -> max(T, H);
max([_|T], Max)              -> max(T, Max);
max([],    Max)              -> Max.

%% sublist(List, Start, Length)
%%  Returns the sub-list starting at Start of length Length.

sublist(List, S, L) when L >= 0 ->
    sublist(nthtail(S-1, List), L).

sublist([H|T], L) when L > 0 ->
    [H|sublist(T, L-1)];
sublist(List, L) -> [].

%% delete(Item, List) -> List'
%%  Delete the first occurance of Item from the list L.

delete(Item, [Item|Rest]) -> Rest;
delete(Item, [H|Rest]) ->
    [H|delete(Item, Rest)];
delete(Item, []) -> [].

%% sort(L) -> sorts the list L

sort([X]) -> [X];
sort([])  -> [];
sort(X)   -> split_and_sort(X, [], []).

split_and_sort([A,B|T], X, Y) ->
    split_and_sort(T, [A|X], [B|Y]);
split_and_sort([H], X, Y) ->
    split_and_sort([], [H|X], Y);
split_and_sort([], X, Y) ->
    merge(sort(X), sort(Y), []).

%% merge(X, Y) -> L
%%  merges two sorted lists X and Y

merge(X, Y) -> merge(X, Y, []).

merge([H1|T1], [H2|T2], L) when H1 < H2 ->
    merge(T1, [H2|T2], [H1|L]);
merge(T1, [H2|T2], L) ->
    merge(T1, T2, [H2|L]);
merge([H|T], T2, L) ->
    merge(T, T2, [H|L]);
merge([], [], L) ->
    reverse(L).

%% concat(L) concatinate the list representation of the elements
%%  in L - the elements in L can be atoms, integers of strings.
%%  Returns a list of characters.

concat(List) ->
    flatmap(fun thing_to_list/1, List).

thing_to_list(X) when integer(X) -> integer_to_list(X);
thing_to_list(X) when float(X)	 -> float_to_list(X);
thing_to_list(X) when atom(X)	 -> atom_to_list(X);
thing_to_list(X) when list(X)	 -> X.		%Assumed to be a string

%% flatten(List)
%% flatten(List, Tail)
%%  Flatten a list, adding optional tail.

flatten(List) ->
    flatten(List, [], []).

flatten(List, Tail) ->
    flatten(List, [], Tail).

flatten([H|T], Cont, Tail) when list(H) ->
    flatten(H, [T|Cont], Tail);
flatten([H|T], Cont, Tail) ->
    [H|flatten(T, Cont, Tail)];
flatten([], [H|Cont], Tail) ->
    flatten(H, Cont, Tail);
flatten([], [], Tail) ->
    Tail.

%% flat_length(List) (undocumented can be rmove later)
%%  Calculate the length of a list of lists.

flat_length(List) -> flatlength(List).

%% flatlength(List)
%%  Calculate the length of a list of lists.

flatlength(List) ->
    flatlength(List, 0).

flatlength([H|T], L) when list(H) ->
    flatlength(H, flatlength(T, L));
flatlength([H|T], L) ->
    flatlength(T, L + 1);
flatlength([], L) -> L.

%% keymember(Key, Index, [Tuple])
%% keysearch(Key, Index, [Tuple])
%% keydelete(Key, Index, [Tuple])
%% keyreplace(Key, Index, [Tuple], NewTuple)
%% keysort(Index, [Tuple])
%% keymerge(Index, [Tuple], [Tuple])
%% keymap(Function, Index, [Tuple])
%% keymap(Function, ExtraArgs, Index, [Tuple])

keymember(Key, N, [T|Ts]) when element(N, T) == Key -> true;
keymember(Key, N, [T|Ts]) ->
    keymember(Key, N, Ts);
keymember(Key, N, []) -> false.

keysearch(Key, N, [H|T]) when element(N, H) == Key ->
    {value, H};
keysearch(Key, N, [H|T]) ->
    keysearch(Key, N, T);
keysearch(Key, N, []) -> false.

keydelete(Key, N, [H|T]) when element(N, H) == Key -> T;
keydelete(Key, N, [H|T]) ->
    [H|keydelete(Key, N, T)];
keydelete(Key, N, []) -> [].

keyreplace(Key, Pos, [Tup|Tail], New) when element(Pos, Tup) == Key ->
    [New|Tail];
keyreplace(Key, Pos, [H|T], New) ->
    [H|keyreplace(Key, Pos, T, New)];
keyreplace(Key, Pos, [], New) -> [].

keysort(Index, [X]) -> [X];
keysort(Index, [])  -> [];
keysort(Index, X)   -> split_and_keysort(X, [], [], Index).

split_and_keysort([A,B|T], X, Y, Index) ->
    split_and_keysort(T, [A|X], [B|Y], Index);
split_and_keysort([H], X, Y, Index) ->
    split_and_keysort([], [H|X], Y, Index);
split_and_keysort([], X, Y, Index) ->
    keymerge(Index, keysort(Index, X), keysort(Index, Y), []).

keymerge(Index, X, Y) -> keymerge(Index, X, Y, []).

keymerge(I, [H1|T1], [H2|T2], L) when element(I, H1) < element(I, H2) ->
    keymerge(I, T1, [H2|T2], [H1|L]);
keymerge(Index, T1, [H2|T2], L) ->
    keymerge(Index,T1, T2, [H2|L]);
keymerge(Index,[H|T], T2, L) ->
    keymerge(Index,T, T2, [H|L]);
keymerge(Index, [], [], L) ->
    reverse(L).

keymap(Fun, Index, [Tup|Tail]) ->
   [setelement(Index, Tup, Fun(element(Index, Tup)))|keymap(Fun, Index, Tail)];
keymap( _, _ , []) -> [].

keymap(Fun, ExtraArgs, Index, [Tup|Tail]) ->
   [setelement(Index, Tup, apply(Fun, [element(Index, Tup)|ExtraArgs]))|
    keymap(Fun, ExtraArgs, Index, Tail)];
keymap( _, _ , _, []) -> [].

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
%%  for list programming. Function here is either a 'fun' or a tuple
%%  {Module,Name} and we use apply/2 to evaluate. The name zf is a joke!
%%
%%  N.B. Unless where the functions actually needs it only foreach/2/3,
%%  which is meant to be used for its side effects, has a defined order
%%  of evaluation.
%%
%%  There are also versions with an extra argument, ExtraArgs, which is a
%%  list of extra arguments to each call.

all(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> all(Pred, Tail);
	false -> false
    end;
all(Pred, []) -> true.

any(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> true;
	false -> any(Pred, Tail)
    end;
any(Pred, []) -> false.

map(F, List) -> [ F(E) || E <- List ].

flatmap(F, [Hd|Tail]) ->
    F(Hd) ++ flatmap(F, Tail);
flatmap(F, []) -> [].

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) -> Accu.

foldr(F, Accu, [Hd|Tail]) ->
    F(Hd, foldr(F, Accu, Tail));
foldr(F, Accu, []) -> Accu.

filter(Pred, List) -> [ E || E <- List, Pred(E) ].

zf(F, [Hd|Tail]) ->
    case F(Hd) of
	true ->
	    [Hd|zf(F, Tail)];
	{true,Val} ->
	    [Val|zf(F, Tail)];
	false ->
	    zf(F, Tail)
    end;
zf(F, []) -> [].

foreach(F, [Hd|Tail]) ->
    F(Hd),
    foreach(F, Tail);
foreach(F, []) -> ok.

mapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = mapfoldl(F, Accu1, Tail),
    {[R|Rs],Accu2};
mapfoldl(F, Accu, []) -> {[],Accu}.

mapfoldr(F, Accu0, [Hd|Tail]) ->
    {Rs,Accu1} = mapfoldr(F, Accu0, Tail),
    {R,Accu2} = F(Hd, Accu1),
    {[R|Rs],Accu2};
mapfoldr(F, Accu, []) -> {[],Accu}.

takewhile(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> [Hd|takewhile(Pred, Tail)];
	false -> []
    end;
takewhile(Pred, []) -> [].

dropwhile(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> dropwhile(Pred, Tail);
	false -> [Hd|Tail]
    end;
dropwhile(Pred, []) -> [].

splitwith(Pred, List) -> splitwith(Pred, List, []).

splitwith(Pred, [Hd|Tail], Taken) ->
    case Pred(Hd) of
	true -> splitwith(Pred, Tail, [Hd|Taken]);
	false -> {reverse(Taken), [Hd|Tail]}
    end;
splitwith(Pred, [], Taken) -> {reverse(Taken),[]}.

%% Versions of the above functions with extra arguments.

all(Pred, Eas, [Hd|Tail]) ->
    case apply(Pred, [Hd|Eas]) of
	true -> all(Pred, Eas, Tail);
	false -> false
    end;
all(Pred, Eas, []) -> true.

any(Pred, Eas, [Hd|Tail]) ->
    case apply(Pred, [Hd|Eas]) of
	true -> true;
	false -> any(Pred, Eas, Tail)
    end;
any(Pred, Eas, []) -> false.

map(F, Eas, List) -> [ apply(F, [E|Eas]) || E <- List ].

flatmap(F, Eas, [Hd|Tail]) ->
    apply(F, [Hd|Eas]) ++ flatmap(F, Eas, Tail);
flatmap(F, Eas, []) -> [].

foldl(F, Eas, Accu, [Hd|Tail]) ->
    foldl(F, Eas, apply(F, [Hd,Accu|Eas]), Tail);
foldl(F, Eas, Accu, []) -> Accu.

foldr(F, Eas, Accu, [Hd|Tail]) ->
    apply(F, [Hd,foldr(F, Eas, Accu, Tail)|Eas]);
foldr(F, Eas, Accu, []) ->
    Accu.

filter(Pred, Eas, List) -> [ E || E <- List, apply(Pred, [E|Eas]) ].

zf(F, Eas, [Hd|Tail]) ->
    case apply(F, [Hd|Eas]) of
	true ->
	    [Hd|zf(F, Eas, Tail)];
	{true,Val} ->
	    [Val|zf(F, Eas, Tail)];
	false ->
	    zf(F, Eas, Tail)
    end;
zf(F, Eas, []) -> [].

foreach(F, Eas, [Hd|Tail]) ->
    apply(F, [Hd|Eas]),
    foreach(F, Eas, Tail);
foreach(F, Eas, []) -> ok.

mapfoldl(F, Eas, Accu0, [Hd|Tail]) ->
    {R,Accu1} = apply(F, [Hd,Accu0|Eas]),
    {Rs,Accu2} = mapfoldl(F, Eas, Accu1, Tail),
    {[R|Rs],Accu2};
mapfoldl(F, Eas, Accu, []) -> {[],Accu}.

mapfoldr(F, Eas, Accu0, [Hd|Tail]) ->
    {Rs,Accu1} = mapfoldr(F, Eas, Accu0, Tail),
    {R,Accu2} = apply(F, [Hd,Accu1|Eas]),
    {[R|Rs],Accu2};
mapfoldr(F, Eas, Accu, []) -> {[],Accu}.

%% takewhile/2, dropwhile/2 and splitwith/2 do not have versions with
%% extra arguments as this going to be discontinued.
