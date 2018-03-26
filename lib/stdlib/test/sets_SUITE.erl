%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

%% This module tests the ordsets, sets, and gb_sets modules.
%%

-module(sets_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 create/1,add_element/1,del_element/1,
	 subtract/1,intersection/1,union/1,is_subset/1,
	 is_set/1,is_empty/1,fold/1,filter/1,
	 take_smallest/1,take_largest/1, iterate/1]).

-include_lib("common_test/include/ct.hrl").

-import(lists, [foldl/3,reverse/1]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() -> 
    [create, add_element, del_element, subtract,
     intersection, union, is_subset, is_set, fold, filter,
     take_smallest, take_largest, iterate, is_empty].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


create(Config) when is_list(Config) ->
    test_all(fun create_1/1).

create_1(M) ->
    S0 = M(empty, []),
    [] = M(to_list, S0),
    0 = M(size, S0),
    true = M(is_empty, S0),
    E = make_ref(),
    One = M(singleton, E),
    1 = M(size, One),
    false = M(is_empty, One),
    [E] = M(to_list, One),
    S0.

add_element(Config) when is_list(Config) ->
    test_all([{0,132},{253,258},{510,514}], fun add_element_1/2).

add_element_1(List, M) ->
    S = M(from_list, List),
    SortedSet = lists:usort(List),
    SortedSet = lists:sort(M(to_list, S)),

    %% Make sure that we get the same result by inserting
    %% elements one at the time.
    S2 = foldl(fun(El, Set) -> M(add_element, {El,Set}) end,
	       M(empty, []), List),
    true = M(equal, {S,S2}),

    %% Insert elements, randomly delete inserted elements,
    %% and re-inserted all deleted elements at the end.
    S3 = add_element_del(List, M, M(empty, []), [], []),
    true = M(equal, {S2,S3}),
    true = M(equal, {S,S3}),
    S.

add_element_del([H|T], M, S, Del, []) ->
    add_element_del(T, M, M(add_element, {H,S}), Del, [H]);
add_element_del([H|T], M, S0, Del, Inserted) ->
    S1 = M(add_element, {H,S0}),
    case rand:uniform(3) of
	1 ->
	    OldEl = lists:nth(rand:uniform(length(Inserted)), Inserted),
	    S = M(del_element, {OldEl,S1}),
	    add_element_del(T, M, S, [OldEl|Del], [H|Inserted]);
	_ ->
	    add_element_del(T, M, S1, Del, [H|Inserted])
    end;
add_element_del([], M, S, Del, _) ->
    M(union, {S,M(from_list, Del)}).

del_element(Config) when is_list(Config) ->
    test_all([{0,132},{253,258},{510,514},{1022,1026}], fun del_element_1/2).

del_element_1(List, M) ->    
    S0 = M(from_list, List),
    Empty = foldl(fun(El, Set) -> M(del_element, {El,Set}) end, S0, List),
    Empty = M(empty, []),
    true = M(is_empty, Empty),
    S1 = foldl(fun(El, Set) ->
		       M(add_element, {El,Set})
	       end, S0, reverse(List)),
    true = M(equal, {S0,S1}),
    S1.

subtract(Config) when is_list(Config) ->
    test_all(fun subtract_empty/1),

    %% Note: No empty set.
    test_all([{2,69},{126,130},{253,258},511,512,{1023,1030}], fun subtract_1/2).

subtract_empty(M) ->
    Empty = M(empty, []),
    true = M(is_empty, M(subtract, {Empty,Empty})),
    M(subtract, {Empty,Empty}).

subtract_1(List, M) ->
    S0 = M(from_list, List),
    Empty = M(empty, []),

    %% Trivial cases.
    true = M(is_empty, M(subtract, {Empty,S0})),
    true = M(equal, {S0,M(subtract, {S0,Empty})}),

    %% Not so trivial.
    subtract_check(List, mutate_some(remove_some(List, 0.4)), M),
    subtract_check(List, rnd_list(length(List) div 2 + 5), M),
    subtract_check(List, rnd_list(length(List) div 7 + 9), M),
    subtract_check(List, mutate_some(List), M).

subtract_check(A, B, M) ->
    one_subtract_check(B, A, M),
    one_subtract_check(A, B, M).

one_subtract_check(A, B, M) ->
    ASorted = lists:usort(A),
    BSorted = lists:usort(B),
    ASet = M(from_list, A),
    BSet = M(from_list, B),
    DiffSet = M(subtract, {ASet,BSet}),
    Diff = ASorted -- BSorted,
    true = M(equal, {DiffSet,M(from_list, Diff)}),
    Diff = lists:sort(M(to_list, DiffSet)),
    DiffSet.

intersection(Config) when is_list(Config) ->
    %% Note: No empty set.
    test_all([{1,65},{126,130},{253,259},{499,513},{1023,1025}], fun intersection_1/2).

intersection_1(List, M) ->
    S0 = M(from_list, List),

    %% Intersection with self.
    true = M(equal, {S0,M(intersection, {S0,S0})}),
    true = M(equal, {S0,M(intersection, [S0,S0])}),
    true = M(equal, {S0,M(intersection, [S0,S0,S0])}),
    true = M(equal, {S0,M(intersection, [S0])}),

    %% Intersection with empty.
    Empty = M(empty, []),
    true = M(equal, {Empty,M(intersection, {S0,Empty})}),
    true = M(equal, {Empty,M(intersection, [S0,Empty,S0,Empty])}),

    %% The intersection of no sets is undefined.
    {'EXIT',_} = (catch M(intersection, [])),

    %% Disjoint sets.
    Disjoint = [{El} || El <- List],
    DisjointSet = M(from_list, Disjoint),
    true = M(is_empty, M(intersection, {S0,DisjointSet})),

    %% Disjoint, different sizes.
    [begin
	 SomeRemoved = M(from_list, remove_some(Disjoint, HowMuch)),
	 true = M(is_empty, M(intersection, {S0,SomeRemoved})),
	 MoreRemoved = M(from_list, remove_some(List, HowMuch)),
	 true = M(is_empty, M(intersection, {MoreRemoved,DisjointSet}))
     end || HowMuch <- [0.3,0.5,0.7,0.9]],

    %% Partial overlap (one or more elements in result set).
    %% The sets have almost the same size. (Almost because a duplicated
    %% element in the original list could be mutated and not mutated
    %% at the same time.)
    PartialOverlap = mutate_some(List, []),
    IntersectionSet = check_intersection(List, PartialOverlap, M),
    false = M(is_empty, IntersectionSet),

    %% Partial overlap, different set sizes. (Intersection possibly empty.)
    check_intersection(List, remove_some(PartialOverlap, 0.1), M),
    check_intersection(List, remove_some(PartialOverlap, 0.3), M),
    check_intersection(List, remove_some(PartialOverlap, 0.5), M),
    check_intersection(List, remove_some(PartialOverlap, 0.7), M),
    check_intersection(List, remove_some(PartialOverlap, 0.9), M),

    IntersectionSet.

check_intersection(Orig, Mutated, M) ->
    OrigSet = M(from_list, Orig),
    MutatedSet = M(from_list, Mutated),
    Intersection = [El || El <- Mutated, not is_tuple(El)],
    SortedIntersection = lists:usort(Intersection),
    IntersectionSet = M(intersection, {OrigSet,MutatedSet}),
    true = M(equal, {IntersectionSet,M(from_list, SortedIntersection)}),
    SortedIntersection = lists:sort(M(to_list, IntersectionSet)),

    IntersectionSet.


union(Config) when is_list(Config) ->
    %% Note: No empty set.
    test_all([{1,71},{125,129},{254,259},{510,513},{1023,1025}], fun union_1/2).

union_1(List, M) ->
    S = M(from_list, List),

    %% Union with self and empty.
    Empty = M(empty, []),
    true = M(equal, {S,M(union, {S,S})}),
    true = M(equal, {S,M(union, [S,S])}),
    true = M(equal, {S,M(union, [S,S,Empty])}),
    true = M(equal, {S,M(union, [S,Empty,S])}),
    true = M(equal, {S,M(union, {S,Empty})}),
    true = M(equal, {S,M(union, [S])}),
    true = M(is_empty, M(union, [])),

    %% Partial overlap.
    check_union(List, remove_some(mutate_some(List), 0.9), M),
    check_union(List, remove_some(mutate_some(List), 0.7), M),
    check_union(List, remove_some(mutate_some(List), 0.5), M),
    check_union(List, remove_some(mutate_some(List), 0.3), M),
    check_union(List, remove_some(mutate_some(List), 0.1), M),

    check_union(List, mutate_some(remove_some(List, 0.9)), M),
    check_union(List, mutate_some(remove_some(List, 0.7)), M),
    check_union(List, mutate_some(remove_some(List, 0.5)), M),
    check_union(List, mutate_some(remove_some(List, 0.3)), M),
    check_union(List, mutate_some(remove_some(List, 0.1)), M).

check_union(Orig, Other, M) ->
    OrigSet = M(from_list, Orig),
    OtherSet = M(from_list, Other),
    Union = Orig++Other,
    SortedUnion = lists:usort(Union),
    UnionSet = M(union, {OrigSet,OtherSet}),
    SortedUnion = lists:sort(M(to_list, UnionSet)),
    M(equal, {UnionSet,M(from_list, Union)}),
    UnionSet.

is_subset(Config) when is_list(Config) ->
    test_all([{1,132},{253,270},{299,311}], fun is_subset_1/2).

is_subset_1(List, M) ->
    S = M(from_list, List),
    Empty = M(empty, []),

    %% Subset of empty and self.
    true = M(is_subset, {Empty,Empty}),
    true = M(is_subset, {Empty,S}),
    false = M(is_subset, {S,Empty}),
    true = M(is_subset, {S,S}),

    %% Other cases.
    Res = [false = M(is_subset, {M(singleton, make_ref()),S}),
	   true = M(is_subset, {M(singleton, hd(List)),S}),
	   true = check_subset(remove_some(List, 0.1), List, M),
	   true = check_subset(remove_some(List, 0.5), List, M),
	   true = check_subset(remove_some(List, 0.9), List, M),
	   check_subset(mutate_some(List), List, M),
	   check_subset(rnd_list(length(List) div 2 + 5), List, M),
	   subtract_check(List, rnd_list(length(List) div 7 + 9), M)
	  ],
    res_to_set(Res, M, 0, []).

check_subset(X, Y, M) ->
    check_one_subset(Y, X, M),
    check_one_subset(X, Y, M).

check_one_subset(X, Y, M) ->
    XSet = M(from_list, X),
    YSet = M(from_list, Y),
    SortedX = lists:usort(X),
    SortedY = lists:usort(Y),
    IsSubSet = length(SortedY--SortedX) =:= length(SortedY) - length(SortedX),
    IsSubSet = M(is_subset, {XSet,YSet}),
    IsSubSet.

%% Encode all test results as a set to return.
res_to_set([true|T], M, I, Acc) ->
    res_to_set(T, M, I+1, [I|Acc]);
res_to_set([_|T], M, I, Acc) ->
    res_to_set(T, M, I+1, Acc);
res_to_set([], M, _, Acc) -> M(from_list, Acc).

is_set(Config) when is_list(Config) ->
    %% is_set/1 is tested in the other test cases when its argument
    %% is a set. Here test some arguments that makes it return false.

    false = gb_sets:is_set([a,b]),
    false = gb_sets:is_set({a,very,bad,tuple}),

    false = sets:is_set([a,b]),
    false = sets:is_set({a,very,bad,tuple}),

    false = ordsets:is_set([b,a]),
    false = ordsets:is_set({bad,tuple}),

    %% Now test values that are known to be bad for all set representations.
    test_all(fun is_set_1/1).

is_set_1(M) ->
    false = M(is_set, self()),
    false = M(is_set, blurf),
    false = M(is_set, make_ref()),
    false = M(is_set, <<1,2,3>>),
    false = M(is_set, 42),
    false = M(is_set, math:pi()),
    false = M(is_set, {}),
    M(empty, []).

is_empty(Config) when is_list(Config) ->
    test_all(fun is_empty_1/1).

is_empty_1(M) ->
    S = M(from_list, [blurf]),
    Empty = M(empty, []),

    true = M(is_empty, Empty),
    false = M(is_empty, S),
    M(empty, []).

fold(Config) when is_list(Config) ->
    test_all([{0,71},{125,129},{254,259},{510,513},{1023,1025},{9999,10001}],
	     fun fold_1/2).

fold_1(List, M) ->
    S = M(from_list, List),
    L = M(fold, {fun(E, A) -> [E|A] end,[],S}),
    true = lists:sort(L) =:= lists:usort(List),
    M(empty, []).

filter(Config) when is_list(Config) ->
    test_all([{0,69},{126,130},{254,259},{510,513},{1023,1025},{7999,8000}],
	     fun filter_1/2).

filter_1(List, M) ->
    S = M(from_list, List),
    IsNumber = fun(X) -> is_number(X) end,
    M(equal, {M(from_list, lists:filter(IsNumber, List)),
	      M(filter, {IsNumber,S})}),
    M(filter, {fun(X) -> is_atom(X) end,S}).

%%%
%%% Test specifics for gb_sets.
%%%

take_smallest(Config) when is_list(Config) ->
    test_all([{1,71},{125,129},{254,259},{510,513},{1023,1025}],
	     fun take_smallest_1/2).

take_smallest_1(List, M) ->
    case M(module, []) of
	gb_sets -> take_smallest_2(List, M);
	_ -> ok
    end,
    M(empty, []).

take_smallest_2(List0, M) ->
    List = lists:usort(List0),
    S = M(from_list, List0),
    take_smallest_3(S, List, M).

take_smallest_3(S0, List0, M) ->
    case M(is_empty, S0) of
	true -> ok;
	false ->
	    Smallest = hd(List0),
	    Smallest = gb_sets:smallest(S0),
	    {Smallest,S} = gb_sets:take_smallest(S0),
	    List = tl(List0),
	    true = gb_sets:to_list(S) =:= List,
	    take_smallest_3(S, List, M)
    end.

take_largest(Config) when is_list(Config) ->
    test_all([{1,71},{125,129},{254,259},{510,513},{1023,1025}],
	     fun take_largest_1/2).

take_largest_1(List, M) ->
    case M(module, []) of
	gb_sets -> take_largest_2(List, M);
	_ -> ok
    end,
    M(empty, []).

take_largest_2(List0, M) ->
    List = reverse(lists:usort(List0)),
    S = M(from_list, List0),
    take_largest_3(S, List, M).

take_largest_3(S0, List0, M) ->
    case M(is_empty, S0) of
	true -> ok;
	false ->
	    Largest = hd(List0),
	    Largest = gb_sets:largest(S0),
	    {Largest,S} = gb_sets:take_largest(S0),
	    List = tl(List0),
	    true = gb_sets:to_list(S) =:= reverse(List),
	    take_largest_3(S, List, M)
    end.

iterate(Config) when is_list(Config) ->
    test_all(fun iterate_1/1).

iterate_1(M) ->
    case M(module, []) of
	gb_sets -> iterate_2(M);
	_ -> ok
    end,
    M(empty, []).

iterate_2(M) ->
    rand:seed(exsplus, {1,2,42}),
    iter_set(M, 1000).

iter_set(_M, 0) ->
    ok;
iter_set(M, N) ->
    L = [I || I <- lists:seq(1, N)],
    T = M(from_list, L),
    L = lists:reverse(iterate_set(M, T)),
    R = rand:uniform(N),
    S = lists:reverse(iterate_set(M, R, T)),
    S = [E || E <- L, E >= R],
    iter_set(M, N-1).

iterate_set(M, Set) ->
    I = M(iterator, Set),
    iterate_set_1(M, M(next, I), []).

iterate_set(M, Start, Set) ->
    I = M(iterator_from, {Start, Set}),
    iterate_set_1(M, M(next, I), []).

iterate_set_1(_, none, R) ->
    R;
iterate_set_1(M, {E, I}, R) ->
    iterate_set_1(M, M(next, I), [E | R]).

%%%
%%% Helper functions.
%%%

sets_mods() ->
    Ordsets = sets_test_lib:new(ordsets, fun(X, Y) -> X == Y end),
    Sets = sets_test_lib:new(sets, fun(X, Y) ->
					   lists:sort(sets:to_list(X)) == 
					       lists:sort(sets:to_list(Y)) end),
    Gb = sets_test_lib:new(gb_sets, fun(X, Y) ->
					    gb_sets:to_list(X) == 
						gb_sets:to_list(Y) end),
    [Ordsets,Sets,Gb].

test_all(Tester) ->
    Res = [begin
	       rand:seed(exsplus, {1,2,42}),
	       S = Tester(M),
	       {M(size, S),lists:sort(M(to_list, S))}
	   end || M <- sets_mods()],
    all_same(Res).

test_all([{Low,High}|T], Tester) ->
    test_all(lists:seq(Low, High)++T, Tester);
test_all([Sz|T], Tester) when is_integer(Sz) ->
    List = rnd_list(Sz),
    Res = [begin
		     rand:seed(exsplus, {19,2,Sz}),
		     S = Tester(List, M),
		     {M(size, S),lists:sort(M(to_list, S))}
		 end || M <- sets_mods()],
    all_same(Res),
    test_all(T, Tester);
test_all([], _) -> ok.


all_same([H|T]) ->
    all_same_1(T, H).

all_same_1([H|T], H) ->
    all_same_1(T, H);
all_same_1([], _) -> ok.

rnd_list(Sz) ->
    rnd_list_1(Sz, []).
    
atomic_rnd_term() ->
    case rand:uniform(3) of
	1 -> list_to_atom(integer_to_list($\s+rand:uniform(94))++"rnd");
	2 -> rand:uniform();
	3 -> rand:uniform(50)-37
    end.

rnd_list_1(0, Acc) -> Acc;
rnd_list_1(N, Acc) -> rnd_list_1(N-1, [atomic_rnd_term()|Acc]).

mutate_some(List) ->
    mutate_some(List, []).
    
mutate_some([X,Y,Z|T], Acc) ->
    %% Intentionally change order. (Order should not matter.)
    mutate_some(T, [{X},Z,Y|Acc]);
mutate_some([H|T], Acc) ->
    mutate_some(T, [H|Acc]);
mutate_some([], Acc) ->
    %% Intentionally not reversing.
    Acc.

%% Removes at least one element.
remove_some(List0, P) ->
    case remove_some(List0, P, []) of
	List when length(List0) =:= length(List) ->
	    tl(List);
	List ->
	    List
    end.

remove_some([H|T], P, Acc) ->
    case rand:uniform() of
	F when F < P ->				%Remove.
	    remove_some(T, P, Acc);
	_ ->
	    remove_some(T, P, [H|Acc])
    end;
remove_some([], _, Acc) ->
    %% Intentionally no reverse. Order should not matter.
    Acc.
