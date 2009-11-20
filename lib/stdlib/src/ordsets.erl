%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

-module(ordsets).

-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([is_disjoint/2]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2]).

-type ordset(T) :: [T].

%% new() -> Set.
%%  Return a new empty ordered set.

-spec new() -> ordset(term()).

new() -> [].

%% is_set(Term) -> boolean().
%%  Return 'true' if Set is an ordered set of elements, else 'false'.

-spec is_set(term()) -> boolean().

is_set([E|Es]) -> is_set(Es, E);
is_set([]) -> true;
is_set(_) -> false.

is_set([E2|Es], E1) when E1 < E2 ->
    is_set(Es, E2);
is_set([_|_], _) -> false;
is_set([], _) -> true.

%% size(OrdSet) -> int().
%%  Return the number of elements in OrdSet.

-spec size(ordset(_)) -> non_neg_integer().

size(S) -> length(S).

%% to_list(OrdSet) -> [Elem].
%%  Return the elements in OrdSet as a list.

-spec to_list(ordset(T)) -> [T].

to_list(S) -> S.

%% from_list([Elem]) -> Set.
%%  Build an ordered set from the elements in List.

-spec from_list([T]) -> ordset(T).

from_list(L) ->
    lists:usort(L).

%% is_element(Element, OrdSet) -> boolean().
%%  Return 'true' if Element is an element of OrdSet, else 'false'.

-spec is_element(term(), ordset(_)) -> boolean().

is_element(E, [H|Es]) when E > H -> is_element(E, Es);
is_element(E, [H|_]) when E < H -> false;
is_element(_E, [_H|_]) -> true;			%E == H
is_element(_, []) -> false.

%% add_element(Element, OrdSet) -> OrdSet.
%%  Return OrdSet with Element inserted in it.

-spec add_element(term(), ordset(_)) -> ordset(_).

add_element(E, [H|Es]) when E > H -> [H|add_element(E, Es)];
add_element(E, [H|_]=Set) when E < H -> [E|Set];
add_element(_E, [_H|_]=Set) -> Set;		%E == H
add_element(E, []) -> [E].

%% del_element(Element, OrdSet) -> OrdSet.
%%  Return OrdSet but with Element removed.

-spec del_element(term(), ordset(_)) -> ordset(_).

del_element(E, [H|Es]) when E > H -> [H|del_element(E, Es)];
del_element(E, [H|_]=Set) when E < H -> Set;
del_element(_E, [_H|Es]) -> Es;			%E == H
del_element(_, []) -> [].

%% union(OrdSet1, OrdSet2) -> OrdSet
%%  Return the union of OrdSet1 and OrdSet2.

-spec union(ordset(_), ordset(_)) -> ordset(_).

union([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    [E1|union(Es1, Set2)];
union([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    [E2|union(Es2, Set1)];			% switch arguments!
union([E1|Es1], [_E2|Es2]) ->			%E1 == E2
    [E1|union(Es1, Es2)];
union([], Es2) -> Es2;
union(Es1, []) -> Es1.

%% union([OrdSet]) -> OrdSet
%%  Return the union of the list of ordered sets.

-spec union([ordset(_)]) -> ordset(_).

union([S1,S2|Ss]) ->
    union1(union(S1, S2), Ss);
union([S]) -> S;
union([]) -> [].

union1(S1, [S2|Ss]) -> union1(union(S1, S2), Ss);
union1(S1, []) -> S1.

%% intersection(OrdSet1, OrdSet2) -> OrdSet.
%%  Return the intersection of OrdSet1 and OrdSet2.

-spec intersection(ordset(_), ordset(_)) -> ordset(_).

intersection([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    intersection(Es1, Set2);
intersection([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    intersection(Es2, Set1);			% switch arguments!
intersection([E1|Es1], [_E2|Es2]) ->		%E1 == E2
    [E1|intersection(Es1, Es2)];
intersection([], _) ->
    [];
intersection(_, []) ->
    [].

%% intersection([OrdSet]) -> OrdSet.
%%  Return the intersection of the list of ordered sets.

-spec intersection([ordset(_)]) -> ordset(_).

intersection([S1,S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection([S]) -> S.

intersection1(S1, [S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection1(S1, []) -> S1.

%% is_disjoint(OrdSet1, OrdSet2) -> boolean().
%%  Check whether OrdSet1 and OrdSet2 are disjoint.

-spec is_disjoint(ordset(_), ordset(_)) -> boolean().

is_disjoint([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    is_disjoint(Es1, Set2);
is_disjoint([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    is_disjoint(Es2, Set1);			% switch arguments!
is_disjoint([_E1|_Es1], [_E2|_Es2]) ->		%E1 == E2
    false;
is_disjoint([], _) ->
    true;
is_disjoint(_, []) ->
    true.

%% subtract(OrdSet1, OrdSet2) -> OrdSet.
%%  Return all and only the elements of OrdSet1 which are not also in
%%  OrdSet2.

-spec subtract(ordset(_), ordset(_)) -> ordset(_).

subtract([E1|Es1], [E2|_]=Set2) when E1 < E2 ->
    [E1|subtract(Es1, Set2)];
subtract([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    subtract(Set1, Es2);
subtract([_E1|Es1], [_E2|Es2]) ->		%E1 == E2
    subtract(Es1, Es2);
subtract([], _) -> [];
subtract(Es1, []) -> Es1.

%% is_subset(OrdSet1, OrdSet2) -> boolean().
%%  Return 'true' when every element of OrdSet1 is also a member of
%%  OrdSet2, else 'false'.

-spec is_subset(ordset(_), ordset(_)) -> boolean().

is_subset([E1|_], [E2|_]) when E1 < E2 ->	%E1 not in Set2
    false;
is_subset([E1|_]=Set1, [E2|Es2]) when E1 > E2 ->
    is_subset(Set1, Es2);
is_subset([_E1|Es1], [_E2|Es2]) ->		%E1 == E2
    is_subset(Es1, Es2);
is_subset([], _) -> true;
is_subset(_, []) -> false.

%% fold(Fun, Accumulator, OrdSet) -> Accumulator.
%%  Fold function Fun over all elements in OrdSet and return Accumulator.

-spec fold(fun((_, _) -> _), _, ordset(_)) -> _.

fold(F, Acc, Set) ->
    lists:foldl(F, Acc, Set).

%% filter(Fun, OrdSet) -> OrdSet.
%%  Filter OrdSet with Fun.

-spec filter(fun((_) -> boolean()), ordset(_)) -> ordset(_).

filter(F, Set) ->
    lists:filter(F, Set).
