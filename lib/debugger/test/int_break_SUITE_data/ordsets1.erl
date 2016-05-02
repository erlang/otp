%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%% Purpose : Functions for manipulating sets as ordered lists.

%% As yet some of these are not very efficiently written.

-module(ordsets1).

-export([new_set/0,is_set/1,set_to_list/1,list_to_set/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([subtract/2,subset/2]).

%% new_set()
%%  Return a new empty ordered set.

new_set() ->
    [].

%% is_set(Set)
%%  Return 'true' if Set is an ordered set of elements, else 'false'.

is_set([E|Es]) ->
    is_set(Es, E);
is_set([]) ->
    true.

is_set([E2|Es], E1) when E1 < E2 ->
    is_set(Es, E2);
is_set([E2|Es], E1) ->
    false;
is_set([], E1) ->
    true.

%% set_to_list(OrdSet)
%%  Return the elements in OrdSet as a list.

set_to_list(S) ->
    S.

%% list_to_set(List)
%%  Build an ordered set from the elements in List.

list_to_set([E|Es]) ->
    add_element(E, list_to_set(Es));
list_to_set([]) ->
    [].

%% is_element(Element, OrdSet)
%%  Return 'true' if Element is an element of OrdSet, else 'false'.

is_element(E, [H|Es]) when E < H ->
    false;
is_element(E, [H|Es]) when E == H ->
    true;
is_element(E, [H|Es]) when E > H ->
    is_element(E, Es);
is_element(E, []) ->
    false.

%% add_element(Element, OrdSet)
%%  Return OrdSet with Element inserted in it.

add_element(E, [H|_]=Es) when E < H ->
    [E|Es];
add_element(E, [H|_]=Es) when E == H ->
    Es;
add_element(E, [H|Es]) when E > H ->
    [H|add_element(E, Es)];
add_element(E, []) ->
    [E].

%% del_element(Element, OrdSet)
%%  Return OrdSet but with Element removed.

del_element(E, [H|_]=Es) when E < H ->
    Es;
del_element(E, [H|Es]) when E == H ->
    Es;
del_element(E, [H|Es]) when E > H ->
    [H|del_element(E, Es)];
del_element(E, []) ->
    [].

%% union(Set1, Set2)
%%  Return the union of Set1 and Set2.

union([H1|Es1], [H2|_]=Es2) when H1 < H2 ->
    [H1|union(Es1, Es2)];
union([H1|Es1], [H2|Es2]) when H1 == H2 ->
    [H1|union(Es1, Es2)];
union([H1|_]=Es1, [H2|Es2]) when H1 > H2 ->
    [H2|union(Es1, Es2)];
union([], Es2) ->
    Es2;
union(Es1, []) ->
    Es1.

%% union(OrdSets)
%%  Return the union of the list of sets.

union([S1,S2|Ss]) ->
    union1(union(S1,S2), Ss);
union([S]) ->
    S;
union([]) ->
    [].

union1(S1, [S2|Ss]) ->
    union1(union(S1, S2), Ss);
union1(S1, []) ->
    S1.

%% intersection(Set1, Set2)
%%  Return the intersection of Set1 and Set2.

intersection([H1|Es1], [H2|_]=Es2) when H1 < H2 ->
    intersection(Es1, Es2);
intersection([H1|Es1], [H2|Es2]) when H1 == H2 ->
    [H1|intersection(Es1, Es2)];
intersection([H1|_]=Es1, [H2|Es2]) when H1 > H2 ->
    intersection(Es1, Es2);
intersection([], Es2) ->
    [];
intersection(Es1, []) ->
    [].

%% intersection(OrdSets)
%%  Return the intersection of the list of sets.

intersection([S1,S2|Ss]) ->
    intersection1(intersection(S1,S2), Ss);
intersection([S]) ->
    S;
intersection([]) ->
    [].

intersection1(S1, [S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection1(S1, []) ->
    S1.

%% subtract(Set1, Set2)
%%  Return all and only the elements of Set1 which are not also in Set2.

subtract([H1|Es1], [H2|_]=Es2) when H1 < H2 ->
    [H1|subtract(Es1, Es2)];
subtract([H1|Es1], [H2|Es2]) when H1 == H2 ->
    subtract(Es1, Es2);
subtract([H1|_]=Es1, [H2|Es2]) when H1 > H2 ->
    subtract(Es1, Es2);
subtract([], Es2) ->
    [];
subtract(Es1, []) ->
    Es1.

%% subset(Set1, Set2)
%%  Return 'true' when every element of Set1 is also a member of Set2,
%%  else 'false'.

subset([H1|Es1], [H2|Es2]) when H1 < H2 ->	%H1 not in Set2
    false;
subset([H1|Es1], [H2|Es2]) when H1 == H2 ->
    subset(Es1, Es2);
subset([H1|Es1], [H2|Es2]) when H1 > H2 ->
    subset([H1|Es1], Es2);
subset([], Es2) ->
    true;
subset(Es1, []) ->
    false.
