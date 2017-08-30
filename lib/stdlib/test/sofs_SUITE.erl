%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(sofs_SUITE).

%%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(t, test_server).
-else.
-include_lib("common_test/include/ct.hrl").
-define(format(S, A), ok).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([ from_term_1/1, set_1/1, from_sets_1/1, relation_1/1,
	  a_function_1/1, family_1/1, projection/1,
	  relation_to_family_1/1, domain_1/1, range_1/1, image/1,
	  inverse_image/1, inverse_1/1, converse_1/1, no_elements_1/1,
	  substitution/1, restriction/1, drestriction/1,
	  strict_relation_1/1, extension/1, weak_relation_1/1,
	  to_sets_1/1, specification/1, union_1/1, intersection_1/1,
	  difference/1, symdiff/1, symmetric_partition/1,
	  is_sofs_set_1/1, is_set_1/1, is_equal/1, is_subset/1,
	  is_a_function_1/1, is_disjoint/1, join/1, canonical/1,
	  composite_1/1, relative_product_1/1, relative_product_2/1,
	  product_1/1, partition_1/1, partition_3/1,
	  multiple_relative_product/1, digraph/1, constant_function/1,
	  misc/1]).

-export([ family_specification/1,
	  family_domain_1/1, family_range_1/1,
	  family_to_relation_1/1,
	  union_of_family_1/1, intersection_of_family_1/1,
	  family_projection/1, family_difference/1,
	  family_intersection_1/1, family_union_1/1,
	  family_intersection_2/1, family_union_2/1,
	  partition_family/1]).

-import(sofs, 
	[a_function/1, a_function/2, constant_function/2,
	 canonical_relation/1, composite/2,
	 converse/1, extension/3, from_term/1, from_term/2,
	 difference/2, domain/1, empty_set/0, family_difference/2,
	 family_intersection/1, family_intersection/2, family_union/1,
	 family_union/2, family/1, family/2, family_specification/2,
	 family_domain/1, family_range/1, family_field/1,
	 family_projection/2, family_to_relation/1, union_of_family/1,
	 field/1, from_external/2, image/2, intersection/1,
	 intersection/2, intersection_of_family/1, inverse/1,
	 inverse_image/2, is_disjoint/2, is_empty_set/1, is_equal/2,
	 is_a_function/1, is_set/1, is_sofs_set/1, is_subset/2,
	 join/4, from_sets/1, multiple_relative_product/2,
	 no_elements/1, partition/1, partition/2, partition/3,
	 partition_family/2, product/1, product/2, projection/2,
	 range/1, relation/1, relation/2, relation_to_family/1,
	 relative_product/1, relative_product/2, relative_product1/2,
	 strict_relation/1, weak_relation/1, restriction/2,
	 restriction/3, drestriction/2, drestriction/3, to_sets/1,
	 is_type/1, set/1, set/2, specification/2, substitution/2,
	 symdiff/2, symmetric_partition/2, to_external/1, type/1,
	 union/1, union/2, family_to_digraph/1, family_to_digraph/2,
	 digraph_to_family/1, digraph_to_family/2]).

-export([init_per_testcase/2, end_per_testcase/2]).

-compile({inline,[{eval,2}]}).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [{group, sofs}, {group, sofs_family}].

groups() -> 
    [{sofs, [],
      [from_term_1, set_1, from_sets_1, relation_1,
       a_function_1, family_1, relation_to_family_1, domain_1,
       range_1, image, inverse_image, inverse_1, converse_1,
       no_elements_1, substitution, restriction, drestriction,
       projection, strict_relation_1, extension,
       weak_relation_1, to_sets_1, specification, union_1,
       intersection_1, difference, symdiff,
       symmetric_partition, is_sofs_set_1, is_set_1, is_equal,
       is_subset, is_a_function_1, is_disjoint, join,
       canonical, composite_1, relative_product_1,
       relative_product_2, product_1, partition_1, partition_3,
       multiple_relative_product, digraph, constant_function,
       misc]},
     {sofs_family, [],
      [family_specification, family_domain_1, family_range_1,
       family_to_relation_1, union_of_family_1,
       intersection_of_family_1, family_projection,
       family_difference, family_intersection_1,
       family_intersection_2, family_union_1, family_union_2,
       partition_family]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% [{2,b},{1,a,b}] == lists:sort([{2,b},{1,a,b}])
%% [{1,a,b},{2,b}] == lists:keysort(1,[{2,b},{1,a,b}])


from_term_1(Conf) when is_list(Conf) ->
    %% would go wrong: projection(1,from_term([{2,b},{1,a,b}])),

    {'EXIT', {badarg, _}} = (catch from_term([], {atom,'_',atom})),
    {'EXIT', {badarg, _}} = (catch from_term([], [])),
    {'EXIT', {badarg, _}} = (catch from_term([], [atom,atom])),

    [] = to_external(from_term([])),
    eval(from_term([]), empty_set()),
    [] = to_external(from_term([], ['_'])),
    eval(from_term([], ['_']), empty_set()),
    [[]] = to_external(from_term([[]])),
    [[['_']]] = type(from_term([[],[[]]])),
    [[],[[]]] = to_external(from_term([[],[[]]])),
    [[['_']]] = type(from_term([[],[[]]])),
    eval(from_term([a],['_']), set([a])),
    [[],[a]] = to_external(from_term([[],[a]])),
    [[],[{a}]] = to_external(from_term([[{a}],[]])),
    [{[],[{a,b,[d]}]},{[{a,b}],[]}] =
	to_external(from_term([{[],[{a,b,[d]}]},{[{a,b}],[]}])),

    [{[a,b],[c,d]}] = to_external(from_term([{[a,b],[c,d]}])),
    [{{a,b},[a,b],{{a},{b}}}] =
	to_external(from_term([{{a,b},[a,b],{{a},{b}}}])),
    [{{a,{[a,b]},a}},{{z,{[y,z]},z}}] =
	to_external(from_term([{{a,{[a,b,a]},a}},{{z,{[y,y,z]},z}}])),
    {'EXIT', {badarg, _}} =
	(catch from_term([{m1,[{m1,f1,1},{m1,f2,2}]},{m2,[]},{m3,[a]}])),
    MS1 = [{m1,[{m1,f1,1},{m1,f2,2}]},{m2,[]},{m3,[{m3,f3,3}]}],
    eval(to_external(from_term(MS1)), MS1),

    eval(to_external(from_term(a)), a),
    eval(to_external(from_term({a})), {a}),

    eval(to_external(from_term([[a],[{b,c}]],[[atomic]])),
	 [[a],[{b,c}]]),
    eval(type(from_term([[a],[{b,c}]],[[atomic]])),
	 [[atomic]]),

    {'EXIT', {badarg, _}} = (catch from_term([[],[],a])),
    {'EXIT', {badarg, _}} = (catch from_term([{[a,b],[c,{d}]}])),
    {'EXIT', {badarg, _}} = (catch from_term([[],[a],[{a}]])),
    {'EXIT', {badarg, _}} = (catch from_term([a,{a,b}])),
    {'EXIT', {badarg, _}} = (catch from_term([[a],[{b,c}]],[['_']])),
    {'EXIT', {badarg, _}} = (catch from_term([a | {a,b}])),
    {'EXIT', {badarg, _}} =
	(catch from_term([{{a},b,c},{d,e,f}],[{{atom},atom,atom}])),
    {'EXIT', {badarg, _}} =
	(catch from_term([{a,{b,c}} | tail], [{atom,{atom,atom}}])),
    {'EXIT', {badarg, _}} = (catch from_term({})),
    {'EXIT', {badarg, _}} = (catch from_term([{}])),

    [{foo,bar},[b,a]] =
        to_external(from_term([[b,a],{foo,bar},[b,a]], [atom])),
    [{[atom],{atom,atom}}] =
	type(from_term([{[], {a,b}},{[a,b],{e,f}}])),
    [{[atom],{atom,atom}}] =
	type(from_term([{[], {a,b}},{[a,b],{e,f}}], [{[atom],{atom,atom}}])),
    [[atom]] = type(from_term([[a],[{b,c}]],[[atom]])),

    {atom, atom} = type(from_term({a,b}, {atom, atom})),
    atom = type(from_term(a, atom)),
    {'EXIT', {badarg, _}} = (catch from_term({a,b},{atom})),
    [{{a},b,c},{{d},e,f}] =
	to_external(from_term([{{a},b,c},{{a},b,c},{{d},e,f}],
			      [{{atom},atom,atom}])),

    %% from_external too...
    e = to_external(from_external(e, atom)),
    {e} = to_external(from_external({e}, {atom})),
    [e] = to_external(from_external([e], [atom])),

    %% and is_type...
    true = is_type(['_']),
    false = is_type('_'),
    true = is_type([['_']]),
    false = is_type({atom,[],atom}),
    false = is_type({atom,'_',atom}),
    true = is_type({atom,atomic,atom}),
    true = is_type({atom,atom}),
    true = is_type(atom),
    true = is_type([atom]),
    true = is_type(type),

    ok.

set_1(Conf) when is_list(Conf) ->
    %% set/1
    {'EXIT', {badarg, _}} = (catch set(a)),
    {'EXIT', {badarg, _}} = (catch set({a})),
    eval(set([]), from_term([],[atom])),
    eval(set([a,b,c]), from_term([a,b,c])),
    eval(set([a,b,a,a,b]), from_term([a,b])),
    eval(set([a,b,c,a,d,d,c,1]), from_term([1,a,b,c,d])),
    eval(set([a,b,d,a,c]), from_term([a,b,c,d])),
    eval(set([f,e,d,c,d]), from_term([c,d,e,f])),
    eval(set([h,f,d,g,g,d,c]), from_term([c,d,f,g,h])),
    eval(set([h,e,d,k,l]), from_term([d,e,h,k,l])),
    eval(set([h,e,c,k,d]), from_term([c,d,e,h,k])),

    %% set/2
    {'EXIT', {badarg, _}} = (catch set(a, [a])),
    {'EXIT', {badarg, _}} = (catch set({a}, [a])),
    {'EXIT', {badarg, _}} = (catch set([a], {a})),
    {'EXIT', {badarg, _}} = (catch set([a], a)),
    {'EXIT', {badarg, _}} = (catch set([a], [a,b])),
    {'EXIT', {badarg, _}} = (catch set([a | b],[foo])),
    {'EXIT', {badarg, _}} = (catch set([a | b],['_'])),
    {'EXIT', {badarg, _}} = (catch set([a | b],[[atom]])),
    {'EXIT', {badarg, _}} = (catch set([{}],[{}])),
    eval(set([a],['_']), from_term([a],['_'])),
    eval(set([], ['_']), empty_set()),
    eval(set([a,b,a,b],[foo]), from_term([a,b],[foo])),

    ok.

from_sets_1(Conf) when is_list(Conf) ->
    E = empty_set(),

    %% unordered
    eval(from_sets([]), E),
    {'EXIT', {type_mismatch, _}} =
	(catch from_sets([from_term([{a,b}]), 
                          E,
                          from_term([{a,b,c}])])),
    eval(from_sets([from_term([{a,b}]), E]),
	 from_term([[],[{a,b}]])),

    eval(from_sets([from_term({a,b},{atom,atom}),
		    from_term({b,c},{atom,atom})]),
	 relation([{a,b}, {b,c}])),
    {'EXIT', {type_mismatch, _}} =
	(catch from_sets([from_term({a,b},{atom,atom}), 
			  from_term({a,b,c},{atom,atom,atom})])),
    {'EXIT', {badarg, _}} = (catch from_sets(foo)),
    eval(from_sets([E]), from_term([[]])),
    eval(from_sets([E,E]), from_term([[]])),
    eval(from_sets([E,set([a])]), from_term([[],[a]])),
    {'EXIT', {badarg, _}} = (catch from_sets([E,{a}])),
    {'EXIT', {type_mismatch, _}} =
	(catch from_sets([E,from_term({a}),E])),
    {'EXIT', {type_mismatch, _}} = (catch from_sets([from_term({a}),E])),

    %% ordered
    O = {from_term(a,atom), from_term({b}, {atom}), set([c,d])},
    eval(from_sets(O), from_term({a,{b},[c,d]}, {atom,{atom},[atom]})),
    {'EXIT', {badarg, _}} = (catch from_sets([a,b])),
    {'EXIT', {badarg, _}} = (catch from_sets({a,b})),
    eval(from_sets({from_term({a}),E}), from_term({{a},[]})),
    ok.

relation_1(Conf) when is_list(Conf) ->
    %% relation/1
    eval(relation([]), from_term([], [{atom,atom}])),
    eval(from_term([{a}]), relation([{a}])),
    {'EXIT', {badarg, _}} = (catch relation(a)),
    {'EXIT', {badarg, _}} = (catch relation([{a} | a])),
    {'EXIT', {badarg, _}} = (catch relation([{}])),
    {'EXIT', {badarg, _}} = (catch relation([],0)),
    {'EXIT', {badarg, _}} = (catch relation([{a}],a)),

    %% relation/2
    eval(relation([{a},{b}], 1), from_term([{a},{b}])),
    eval(relation([{1,a},{2,b},{1,a}], [{x,y}]),
	 from_term([{1,a},{2,b}], [{x,y}])),
    eval(relation([{[1,2],a},{[2,1],b},{[2,1],a}], [{[x],y}]),
	 from_term([{[1,2],a},{[1,2],b}], [{[x],y}])),
    {'EXIT', {badarg, _}} = (catch relation([{1,a},{2,b}], [{[x],y}])),
    {'EXIT', {badarg, _}} = (catch relation([{1,a},{1,a,b}], [{x,y}])),
    {'EXIT', {badarg, _}} = (catch relation([{a}], 2)),
    {'EXIT', {badarg, _}} = (catch relation([{a},{b},{c,d}], 1)),
    eval(relation([{{a},[{foo,bar}]}], ['_']),
	 from_term([{{a},[{foo,bar}]}], ['_'])),
    eval(relation([], ['_']), from_term([], ['_'])),
    {'EXIT', {badarg, _}} = (catch relation([[a]],['_'])),
    eval(relation([{[a,b,a]}], [{[atom]}]), from_term([{[a,b,a]}])),
    eval(relation([{[a,b,a],[[d,e,d]]}], [{[atom],[[atom]]}]),
	 from_term([{[a,b,a],[[d,e,d]]}])),
    eval(relation([{[a,b,a],[[d,e,d]]}], [{atom,[[atom]]}]),
	 from_term([{[a,b,a],[[d,e,d]]}], [{atom,[[atom]]}])),
    ok.

a_function_1(Conf) when is_list(Conf) ->
    %% a_function/1
    eval(a_function([]), from_term([], [{atom,atom}])),
    eval(a_function([{a,b},{a,b},{b,c}]), from_term([{a,b},{b,c}])),
    {'EXIT', {badarg, _}} = (catch a_function([{a}])),
    {'EXIT', {badarg, _}} = (catch a_function([{a},{b},{c,d}])),
    {'EXIT', {badarg, _}} = (catch a_function(a)),
    {'EXIT', {badarg, _}} = (catch a_function([{a,b} | a])),
    {'EXIT', {bad_function, _}} =
	(catch a_function([{a,b},{b,c},{a,c}])),
    F = 0.0, I = round(F),
    if
        F == I -> % term ordering
            {'EXIT', {bad_function, _}} =
                (catch a_function([{I,a},{F,b}])),
            {'EXIT', {bad_function, _}} =
		(catch a_function([{[I],a},{[F],b}],[{[a],b}]));
        true -> 
            2 = no_elements(a_function([{I,a},{F,b}])),
            2 = no_elements(a_function([{[I],a},{[F],b}],[{[a],b}]))
    end,

    %% a_function/2
    FT = [{atom,atom}],
    eval(a_function([], FT), from_term([], FT)),
    eval(a_function([{a,b},{b,c},{b,c}], FT),
	 from_term([{a,b},{b,c}], FT)),
    {'EXIT', {badarg, _}} = (catch a_function([{a,b}], [{a}])),
    {'EXIT', {badarg, _}} = (catch a_function([{a,b}], [{a,[b,c]}])),
    {'EXIT', {badarg, _}} = (catch a_function([{a}], FT)),
    {'EXIT', {badarg, _}} = (catch a_function([{a},{b},{c,d}], FT)),
    {'EXIT', {badarg, _}} = (catch a_function(a, FT)),
    {'EXIT', {badarg, _}} = (catch a_function([{a,b} | a], FT)),
    eval(a_function([{{a},[{foo,bar}]}], ['_']),
	 from_term([{{a},[{foo,bar}]}], ['_'])),
    eval(a_function([], ['_']), from_term([], ['_'])),
    {'EXIT', {badarg, _}} = (catch a_function([[a]],['_'])),
    {'EXIT', {bad_function, _}} =
	(catch a_function([{a,b},{b,c},{a,c}], FT)),
    eval(a_function([{a,[a]},{a,[a,a]}], [{atom,[atom]}]),
	 from_term([{a,[a]}])),
    eval(a_function([{[b,a],c},{[a,b],c}], [{[atom],atom}]),
	 from_term([{[a,b],c}])),
    ok.

family_1(Conf) when is_list(Conf) ->
    %% family/1
    eval(family([]), from_term([],[{atom,[atom]}])),
    {'EXIT', {badarg, _}} = (catch family(a)),
    {'EXIT', {badarg, _}} = (catch family([a])),
    {'EXIT', {badarg, _}} = (catch family([{a,b}])),
    {'EXIT', {badarg, _}} = (catch family([{a,[]} | a])),
    {'EXIT', {badarg, _}} = (catch family([{a,[a|b]}])),
    {'EXIT', {bad_function, _}} =
        (catch family([{a,[a]},{a,[]}])),
    {'EXIT', {bad_function, _}} =
	(catch family([{a,[]},{b,[]},{a,[a]}])),
    F = 0.0, I = round(F),
    if
        F == I -> % term ordering
            {'EXIT', {bad_function, _}} =
                (catch family([{I,[a]},{F,[b]}])),
            true = (1 =:= no_elements(family([{a,[I]},{a,[F]}])));
        true -> 
            {'EXIT', {bad_function, _}} =
                (catch family([{a,[I]},{a,[F]}]))
    end,
    eval(family([{a,[]},{b,[b]},{a,[]}]), from_term([{a,[]},{b,[b]}])),
    eval(to_external(family([{b,[{hej,san},tjo]},{a,[]}])),
	 [{a,[]},{b,[tjo,{hej,san}]}]),
    eval(family([{a,[a]},{a,[a,a]}]), family([{a,[a]}])),

    %% family/2
    FT = [{a,[a]}],
    eval(family([], FT), from_term([],FT)),
    {'EXIT', {badarg, _}} = (catch family(a,FT)),
    {'EXIT', {badarg, _}} = (catch family([a],FT)),
    {'EXIT', {badarg, _}} = (catch family([{a,b}],FT)),
    {'EXIT', {badarg, _}} = (catch family([{a,[]} | a],FT)),
    {'EXIT', {badarg, _}} = (catch family([{a,[a|b]}], FT)),
    {'EXIT', {bad_function, _}} =
        (catch family([{a,[a]},{a,[]}], FT)),
    {'EXIT', {bad_function, _}} =
	(catch family([{a,[]},{b,[]},{a,[a]}], FT)),
    eval(family([{a,[]},{b,[b,b]},{a,[]}], FT),
	 from_term([{a,[]},{b,[b]}], FT)),
    eval(to_external(family([{b,[{hej,san},tjo]},{a,[]}], FT)),
	 [{a,[]},{b,[tjo,{hej,san}]}]),

    eval(family([{{a},[{foo,bar}]}], ['_']),
	 from_term([{{a},[{foo,bar}]}], ['_'])),
    eval(family([], ['_']), from_term([], ['_'])),
    {'EXIT', {badarg, _}} = (catch family([[a]],['_'])),
    {'EXIT', {badarg, _}} = (catch family([{a,b}],['_'])),
    {'EXIT', {badarg, _}} =
	(catch family([{a,[foo]}], [{atom,atom}])),
    eval(family([{{a},[{foo,bar}]}], [{{dt},[{r1,t2}]}]),
	 from_term([{{a},[{foo,bar}]}], [{{dt},[{r1,t2}]}])),
    eval(family([{a,[a]},{a,[a,a]}],[{atom,[atom]}]),
	 family([{a,[a]}])),
    eval(family([{[a,b],[a]},{[b,a],[a,a]}],[{[atom],[atom]}]),
	 from_term([{[a,b],[a]},{[b,a],[a,a]}])),
    ok.

projection(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),

    %% set of ordered sets
    S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    S2 = relation([{a,1},{a,2},{a,3},{b,4},{b,5},{b,6}]),

    eval(projection(1, E), E),
    eval(projection(1, ER), set([])),
    eval(projection(1, relation([{a,1}])), set([a])),
    eval(projection(1, S1), set([a,b,c])),
    eval(projection(1, S2), set([a,b])),
    eval(projection(2, S1), set([0,1,2,22])),
    eval(projection(2, relation([{1,a},{2,a},{3,b}])), set([a,b])),
    eval(projection(1, relation([{a},{b},{c}])), set([a,b,c])),

    Fun1 = {external, fun({A,B,C}) -> {A,{B,C}} end},
    eval(projection(Fun1, E), E),
    %% No check here:
    eval(projection(3, projection(Fun1, empty_set())), E),
    E2 = relation([], 3),
    eval(projection(Fun1, E2), from_term([], [{atom,{atom,atom}}])),

    Fun2 = {external, fun({A,_B}) -> {A} end},
    eval(projection(Fun2, ER), from_term([], [{atom}])),
    eval(projection(Fun2, relation([{a,1}])), relation([{a}])),
    eval(projection(Fun2, relation([{a,1},{b,3},{a,2}])),
	 relation([{a},{b}])),
    Fun3 = {external, fun({A,_B,C}) -> {C,{A},C} end}, 
    eval(projection(Fun3, relation([{a,1,x},{b,3,y},{a,2,z}])),
	 from_term([{x,{a},x},{y,{b},y},{z,{a},z}])),
    Fun4 = {external, fun(A={B,_C,_D}) -> {B, A} end},
    eval(projection(Fun4, relation([{a,1,x},{b,3,y},{a,2,z}])),
	 from_term([{a,{a,1,x}},{b,{b,3,y}},{a,{a,2,z}}])),

    eval(projection({external, fun({A,B,_C,D}) -> {A,B,A,D} end},
		    relation([{1,1,1,2}, {1,1,3,1}])),
	 relation([{1,1,1,1}, {1,1,1,2}])),

    {'EXIT', {badarg, _}} = (catch projection(1, set([]))),
    {'EXIT', {function_clause, _}} =
	(catch projection({external, fun({A}) -> A end}, S1)),
    {'EXIT', {badarg, _}} =
	(catch projection({external, fun({A,_}) -> {A,0} end}, 
			  from_term([{1,a}]))),

    %% {} is not an ordered set
    {'EXIT', {badarg, _}} =
        (catch projection({external, fun(_) -> {} end}, ER)),
    {'EXIT', {badarg, _}} =
        (catch projection({external, fun(_) -> {{}} end}, ER)),
    eval(projection({external, fun({T,_}) -> T end},
		    relation([{{},a},{{},b}])),
	 set([{}])),
    eval(projection({external, fun({T}) -> T end}, relation([{{}}])),
	 set([{}])),

    eval(projection({external, fun(A) -> {A} end},
		    relation([{1,a},{2,b}])),
	 from_term([{{1,a}},{{2,b}}])),
    eval(projection({external, fun({A,B}) -> {B,A} end},
		    relation([{1,a},{2,b}])),
	 relation([{a,1},{b,2}])),
    eval(projection({external, fun(X=Y=A) -> {X,Y,A} end}, set([a,b,c])),
	 relation([{a,a,a},{b,b,b},{c,c,c}])),

    eval(projection({external, fun({A,{_},B}) -> {A,B} end},
		    from_term([{a,{a},b},{a,{b},c}])),
	 relation([{a,b},{a,c}])),
    eval(projection({external, fun({A,_,B}) -> {A,B} end},
		    relation([{a,{},b},{a,{},c}])),
	 relation([{a,b},{a,c}])),
    Fun5 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    eval(projection(Fun5, E), E),
    eval(projection(Fun5, set([a,b])), from_term([{a,0},{b,0}])),
    eval(projection(Fun5, relation([{a,1},{b,2}])),
	 from_term([{{a,1},0},{{b,2},0}])),
    eval(projection(Fun5, from_term([[a],[b]])),
	 from_term([{[a],0},{[b],0}])),

    F = 0.0, I = round(F),
    FR = relation([{I},{F}]),
    if
        F == I -> % term ordering
            true = (no_elements(projection(1, FR)) =:= 1);
        true -> 
            eval(projection(1, FR), set([I,F]))
    end,

    %% set of sets
    {'EXIT', {badarg, _}} =
        (catch projection({external, fun(X) -> X end}, 
			  from_term([], [[atom]]))),
    {'EXIT', {badarg, _}} =
        (catch projection({external, fun(X) -> X end}, from_term([[a]]))),
    eval(projection(fun sofs:union/1,
		    from_term([[[1,2],[2,3]], [[a,b],[b,c]]])),
	 from_term([[1,2,3], [a,b,c]])),
    eval(projection(fun(_) -> from_term([a]) end,
		    from_term([[b]], [[a]])),
	 from_term([[a]])),
    eval(projection(fun(_) -> from_term([a]) end,
		    from_term([[1,2],[3,4]])),
	 from_term([[a]])),
    Fun10 = fun(S) ->
		    %% Cheating a lot...
		    case to_external(S) of
			[1] -> from_term({1,1});
			_ -> S
		    end
	    end,
    eval(projection(Fun10, from_term([[1]])), from_term([{1,1}])),
    eval(projection(fun(_) -> from_term({a}) end, from_term([[a]])),
	 from_term([{a}])),
    {'EXIT', {badarg, _}} =
	(catch projection(fun(_) -> {a} end, from_term([[a]]))),

    ok.

substitution(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),

    %% set of ordered sets
    S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    S2 = relation([{a,1},{a,2},{a,3},{b,4},{b,5},{b,6}]),

    eval(substitution(1, E), E),
    %% No check here:
    Fun0 = {external, fun({A,B,C}) -> {A,{B,C}} end},
    eval(substitution(3, substitution(Fun0, empty_set())), E),
    eval(substitution(1, ER), from_term([],[{{atom,atom},atom}])),
    eval(substitution(1, relation([{a,1}])), from_term([{{a,1},a}])),
    eval(substitution(1, S1),
	 from_term([{{a,1},a},{{b,2},b},{{b,22},b},{{c,0},c}])),
    eval(substitution(1, S2),
	 from_term([{{a,1},a},{{a,2},a},{{a,3},a},{{b,4},b},
		    {{b,5},b},{{b,6},b}])),
    eval(substitution(2, S1),
	 from_term([{{a,1},1},{{b,2},2},{{b,22},22},{{c,0},0}])),

    Fun1 = fun({A,_B}) -> {A} end, 
    XFun1 = {external, Fun1},
    eval(substitution(XFun1, E), E),
    eval(substitution(Fun1, E), E),
    eval(substitution(XFun1, ER), from_term([], [{{atom,atom},{atom}}])),
    eval(substitution(XFun1, relation([{a,1}])),
	 from_term([{{a,1},{a}}])),
    eval(substitution(XFun1, relation([{a,1},{b,3},{a,2}])),
	 from_term([{{a,1},{a}},{{a,2},{a}},{{b,3},{b}}])),
    eval(substitution({external, fun({A,_B,C}) -> {C,A,C} end},
		      relation([{a,1,x},{b,3,y},{a,2,z}])),
	 from_term([{{a,1,x},{x,a,x}},{{a,2,z},{z,a,z}},
		    {{b,3,y},{y,b,y}}])),
    Fun2 = fun(S) -> {A,_B} = to_external(S), from_term({A}) end,
    eval(substitution(Fun2, ER), E),
    eval(substitution(Fun2, relation([{a,1}])),
	 from_term([{{a,1},{a}}])),
    Fun3 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    eval(substitution(Fun3, E), E),
    eval(substitution(Fun3, set([a,b])),
	 from_term([{a,{a,0}},{b,{b,0}}])),
    eval(substitution(Fun3, relation([{a,1},{b,2}])),
	 from_term([{{a,1},{{a,1},0}},{{b,2},{{b,2},0}}])),
    eval(substitution(Fun3, from_term([[a],[b]])),
	 from_term([{[a],{[a],0}},{[b],{[b],0}}])),

    eval(substitution(fun(_) -> E end, from_term([[a],[b]])),
	 from_term([{[a],[]},{[b],[]}])),

    {'EXIT', {badarg, _}} = (catch substitution(1, set([]))),
    eval(substitution(1, ER), from_term([], [{{atom,atom},atom}])),
    {'EXIT', {function_clause, _}} =
	(catch substitution({external, fun({A,_}) -> A end}, set([]))),
    {'EXIT', {badarg, _}} =
	(catch substitution({external, fun({A,_}) -> {A,0} end}, 
			    from_term([{1,a}]))),

    %% set of sets
    {'EXIT', {badarg, _}} =
        (catch substitution({external, fun(X) -> X end}, 
			    from_term([], [[atom]]))),
    {'EXIT', {badarg, _}} =
        (catch substitution({external, fun(X) -> X end}, from_term([[a]]))),
    eval(substitution(fun(X) -> X end, from_term([], [[atom]])), E),
    eval(substitution(fun sofs:union/1,
		      from_term([[[1,2],[2,3]], [[a,b],[b,c]]])),
	 from_term([{[[1,2],[2,3]],[1,2,3]}, {[[a,b],[b,c]],[a,b,c]}])),
    eval(substitution(fun(_) -> from_term([a]) end,
		      from_term([[b]], [[a]])),
	 from_term([{[b],[a]}], [{[a],[atom]}])),
    eval(substitution(fun(_) -> from_term([a]) end,
		      from_term([[1,2],[3,4]])),
	 from_term([{[1,2],[a]},{[3,4],[a]}])),
    Fun10 = fun(S) ->
		    %% Cheating a lot...
		    case to_external(S) of
			[1] -> from_term({1,1});
			_ -> S
		    end
	    end,
    eval(substitution(Fun10, from_term([[1]])),
	 from_term([{[1],{1,1}}])),
    {'EXIT', {type_mismatch, _}} =
        (catch substitution(Fun10, from_term([[1],[2]]))),
    {'EXIT', {type_mismatch, _}} =
        (catch substitution(Fun10, from_term([[1],[0]]))),

    eval(substitution(fun(_) -> from_term({a}) end, from_term([[a]])),
	 from_term([{[a],{a}}])),
    {'EXIT', {badarg, _}} =
	(catch substitution(fun(_) -> {a} end, from_term([[a]]))),

    ok.

restriction(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([], 2),

    %% set of ordered sets
    S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    eval(restriction(S1, set([a,b])),
	 relation([{a,1},{b,2},{b,22}])),
    eval(restriction(2, S1, set([1,2])),
	 relation([{a,1},{b,2}])),
    eval(restriction(S1, set([a,b,c])), S1),
    eval(restriction(1, S1, set([0,1,d,e])), ER),
    eval(restriction(1, S1, E), ER),
    eval(restriction({external, fun({_A,B,C}) -> {B,C} end},
		     relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
		     relation([{bb,2},{cc,3}])),
	 relation([{b,bb,2},{c,cc,3}])),
    R1 = relation([],[{a,b}]),
    eval(restriction(2, R1,sofs:set([],[b])), R1),
    Id = fun(X) -> X end,
    XId = {external, Id},
    eval(restriction(XId, relation([{a,b}]), E), ER),
    eval(restriction(XId, E, relation([{b,d}])), E),
    Fun1 = fun(S) -> {_A,B,C} = to_external(S), from_term({B,C}) end,
    eval(restriction(Fun1,
		     relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
		     relation([{bb,2},{cc,3}])),
	 relation([{b,bb,2},{c,cc,3}])),
    eval(restriction({external, fun({_,{A},B}) -> {A,B} end},
		     from_term([{a,{aa},1},{b,{bb},2},{c,{cc},3}]),
		     from_term([{bb,2},{cc,3}])),
	 from_term([{b,{bb},2},{c,{cc},3}])),
    S5 = relation([{1,a},{2,b},{3,c}]),
    eval(restriction(2, S5, set([b,c])), relation([{2,b},{3,c}])),
    S4 = relation([{a,1},{b,2},{b,27},{c,0}]),
    eval(restriction(2, S4, E), ER),
    S6 = relation([{1,a},{2,c},{3,b}]),
    eval(restriction(2, S6, set([d,e])), ER),
    eval(restriction(2,
		     relation([{1,d},{2,c},{3,b},{4,a},{5,e}]),
		     set([c])),
	 relation([{2,c}])),
    eval(restriction(XId,
		     relation([{1,a},{3,b},{4,c},{4,d}]),
		     relation([{2,a},{2,c},{4,c}])),
	 relation([{4,c}])),
    eval(restriction(2, relation([{a,b}]), E), ER),
    eval(restriction(2, E, relation([{b,d}])), E),
    eval(restriction(2, relation([{b,d}]), E), ER),
    eval(restriction(XId, E, set([a])), E),
    eval(restriction(1, S1, E), ER),
    {'EXIT', {badarg, _}} =
	(catch restriction(3, relation([{a,b}]), E)),
    {'EXIT', {badarg, _}} =
	(catch restriction(3, relation([{a,b}]), relation([{b,d}]))),
    {'EXIT', {badarg, _}} =
	(catch restriction(3, relation([{a,b}]), set([{b,d}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch restriction(2, relation([{a,b}]), relation([{b,d}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch restriction({external, fun({A,_B}) -> A end}, 
			   relation([{a,b}]), relation([{b,d}]))),
    {'EXIT', {badarg, _}} =
	(catch restriction({external, fun({A,_}) -> {A,0} end}, 
			   from_term([{1,a}]),
			   from_term([{1,0}]))),
    eval(restriction(2, relation([{a,d},{b,e},{c,b},{d,c}]), set([b,d])),
	 relation([{a,d},{c,b}])),
    {'EXIT', {function_clause, _}} =
	(catch restriction({external, fun({A,_B}) -> A end}, set([]), E)),

    Fun3 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    eval(restriction(Fun3, set([1,2]), from_term([{1,0}])),
	 from_term([1])),

    %% set of sets
    {'EXIT', {badarg, _}} =
        (catch restriction({external, fun(X) -> X end}, 
			   from_term([], [[atom]]), set([a]))),
    S2 = from_term([], [[atom]]),
    eval(restriction(Id, S2, E), E),
    S3 = from_term([[a],[b]], [[atom]]),
    eval(restriction(Id, S3, E), E),
    eval(restriction(Id, from_term([], [[atom]]), set([a])),
	 from_term([], [[atom]])),
    eval(restriction(fun sofs:union/1,
		     from_term([[[a],[b]], [[b],[c]],
				[[], [a,b]], [[1],[2]]]),
		     from_term([[a,b],[1,2,3],[b,c]])),
	 from_term([[[],[a,b]], [[a],[b]],[[b],[c]]])),
    eval(restriction(fun(_) -> from_term([a]) end,
		     from_term([], [[atom]]),
		     from_term([], [[a]])),
	 from_term([], [[atom]])),
    {'EXIT', {type_mismatch, _}} =
        (catch restriction(fun(_) -> from_term([a]) end, 
                           from_term([[1,2],[3,4]]),
                           from_term([], [atom]))),
    Fun10 = fun(S) ->
		    %% Cheating a lot...
		    case to_external(S) of
			[1] -> from_term({1,1});
			_ -> S
		    end
	    end,
    {'EXIT', {type_mismatch, _}} =
        (catch restriction(Fun10, from_term([[1]]), from_term([], [[atom]]))),
    {'EXIT', {type_mismatch, _}} =
        (catch restriction(fun(_) -> from_term({a}) end, 
                           from_term([[a]]),
                           from_term([], [atom]))),
    {'EXIT', {badarg, _}} =
        (catch restriction(fun(_) -> {a} end, 
                           from_term([[a]]),
                           from_term([], [atom]))),
    ok.

drestriction(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([], 2),

    %% set of ordered sets
    S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    eval(drestriction(S1, set([a,b])), relation([{c,0}])),
    eval(drestriction(2, S1, set([1,2])),
	 relation([{b,22},{c,0}])),
    eval(drestriction(S1, set([a,b,c])), ER),
    eval(drestriction(2, ER, set([a,b])), ER),
    eval(drestriction(1, S1, set([0,1,d,e])), S1),
    eval(drestriction(1, S1, E), S1),
    eval(drestriction({external, fun({_A,B,C}) -> {B,C} end},
		      relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
		      relation([{bb,2},{cc,3}])),
	 relation([{a,aa,1}])),
    Id = fun(X) -> X end,
    XId = {external, Id},
    eval(drestriction(XId, relation([{a,b}]), E), relation([{a,b}])),
    eval(drestriction(XId, E, relation([{b,d}])), E),
    Fun1 = fun(S) -> {_A,B,C} = to_external(S), from_term({B,C}) end,
    eval(drestriction(Fun1,
		      relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
		      relation([{bb,2},{cc,3}])),
	 relation([{a,aa,1}])),
    eval(drestriction({external, fun({_,{A},B}) -> {A,B} end},
		      from_term([{a,{aa},1},{b,{bb},2},{c,{cc},3}]),
		      from_term([{bb,2},{cc,3}])),
	 from_term([{a,{aa},1}])),
    S5 = relation([{1,a},{2,b},{3,c}]),
    eval(drestriction(2, S5, set([b,c])), relation([{1,a}])),
    S4 = relation([{a,1},{b,2},{b,27},{c,0}]),
    eval(drestriction(2, S4, set([])), S4),
    S6 = relation([{1,a},{2,c},{3,b}]),
    eval(drestriction(2, S6, set([d,e])), S6),
    eval(drestriction(2,
		      relation([{1,d},{2,c},{3,b},{4,a},{5,e}]),
		      set([c])),
	 relation([{1,d},{3,b},{4,a},{5,e}])),
    eval(drestriction(XId,
		      relation([{1,a},{3,b},{4,c},{4,d}]),
		      relation([{2,a},{2,c},{4,c}])),
	 relation([{1,a},{3,b},{4,d}])),
    eval(drestriction(2, relation([{a,b}]), E), relation([{a,b}])),
    eval(drestriction(2, E, relation([{b,d}])), E),
    eval(drestriction(2, relation([{b,d}]), E), relation([{b,d}])),
    eval(drestriction(XId, E, set([a])), E),
    eval(drestriction(1, S1, E), S1),
    {'EXIT', {badarg, _}} =
	(catch drestriction(3, relation([{a,b}]), E)),
    {'EXIT', {badarg, _}} =
	(catch drestriction(3, relation([{a,b}]), relation([{b,d}]))),
    {'EXIT', {badarg, _}} =
	(catch drestriction(3, relation([{a,b}]), set([{b,d}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch drestriction(2, relation([{a,b}]), relation([{b,d}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch drestriction({external, fun({A,_B}) -> A end}, 
			    relation([{a,b}]), relation([{b,d}]))),
    {'EXIT', {badarg, _}} =
	(catch drestriction({external, fun({A,_}) -> {A,0} end}, 
			    from_term([{1,a}]),
			    from_term([{1,0}]))),
    eval(drestriction(2, relation([{a,d},{b,e},{c,b},{d,c}]), set([b,d])),
	 relation([{b,e},{d,c}])),
    {'EXIT', {function_clause, _}} =
	(catch drestriction({external, fun({A,_B}) -> A end}, set([]), E)),

    Fun3 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    eval(drestriction(Fun3, set([1,2]), from_term([{1,0}])),
	 from_term([2])),

    %% set of sets
    {'EXIT', {badarg, _}} =
        (catch drestriction({external, fun(X) -> X end}, 
			    from_term([], [[atom]]), set([a]))),
    S2 = from_term([], [[atom]]),
    eval(drestriction(Id, S2, E), S2),
    S3 = from_term([[a],[b]], [[atom]]),
    eval(drestriction(Id, S3, E), S3),
    eval(drestriction(Id, from_term([], [[atom]]), set([a])),
	 from_term([], [[atom]])),
    eval(drestriction(fun sofs:union/1,
		      from_term([[[a],[b]], [[b],[c]],
				 [[], [a,b]], [[1],[2]]]),
		      from_term([[a,b],[1,2,3],[b,c]])),
	 from_term([[[1],[2]]])),
    eval(drestriction(fun(_) -> from_term([a]) end,
		      from_term([], [[atom]]),
		      from_term([], [[a]])),
	 from_term([], [[atom]])),
    {'EXIT', {type_mismatch, _}} =
        (catch drestriction(fun(_) -> from_term([a]) end, 
                            from_term([[1,2],[3,4]]),
                            from_term([], [atom]))),
    Fun10 = fun(S) ->
		    %% Cheating a lot...
		    case to_external(S) of
			[1] -> from_term({1,1});
			_ -> S
		    end
	    end,
    {'EXIT', {type_mismatch, _}} =
        (catch drestriction(Fun10, from_term([[1]]), from_term([], [[atom]]))),
    {'EXIT', {type_mismatch, _}} =
        (catch drestriction(fun(_) -> from_term({a}) end, 
                            from_term([[a]]),
                            from_term([], [atom]))),
    {'EXIT', {badarg, _}} =
        (catch drestriction(fun(_) -> {a} end, 
			    from_term([[a]]),
			    from_term([], [atom]))),
    ok.

strict_relation_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([], 2),
    eval(strict_relation(E), E),
    eval(strict_relation(ER), ER),
    eval(strict_relation(relation([{1,a},{a,a},{2,b}])),
	 relation([{1,a},{2,b}])),
    {'EXIT', {badarg, _}} =
        (catch strict_relation(relation([{1,2,3}]))),
    F = 0.0, I = round(F),
    FR = relation([{F,I}]),
    if
        F == I -> % term ordering
            eval(strict_relation(FR), ER);
        true -> 
            eval(strict_relation(FR), FR)
    end,
    ok.

extension(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([], 2),
    EF = family([]),
    C1 = from_term(3),
    C2 = from_term([3]),
    {'EXIT', {function_clause, _}} = (catch extension(foo, E, C1)),
    {'EXIT', {function_clause, _}} = (catch extension(ER, foo, C1)),
    {'EXIT', {{case_clause, _},_}} = (catch extension(ER, E, foo)),
    {'EXIT', {type_mismatch, _}} = (catch extension(ER, E, E)),
    {'EXIT', {badarg, _}} = (catch extension(C2, E, E)),
    eval(E, extension(E, E, E)),
    eval(EF, extension(EF, E, E)),
    eval(family([{3,[]}]), extension(EF, set([3]), E)),
    eval(ER, extension(ER, E, C1)),
    eval(E, extension(E, ER, E)),
    eval(from_term([],[{{atom,atom},type(ER)}]), extension(E, ER, ER)),

    R1 = relation([{c,7},{c,9},{c,11},{d,17},{f,20}]),
    S1 = set([a,c,d,e]),
    eval(extension(R1, S1, C1), lextension(R1, S1, C1)),

    S2 = set([1,2,3]),
    eval(extension(ER, S2, C1), lextension(ER, S2, C1)),

    R3 = relation([{4,a},{8,b}]),
    S3 = set([1,2,3,4,5,6,7,8,9,10,11]),
    eval(extension(R3, S3, C1), lextension(R3, S3, C1)),

    R4 = relation([{2,b},{4,d},{6,f}]),
    S4 = set([1,3,5,7]),
    eval(extension(R4, S4, C1), lextension(R4, S4, C1)),

    F1 = family([{a,[1]},{c,[2]}]),
    S5 = set([a,b,c,d]),
    eval(extension(F1, S5, C2), lextension(F1, S5, C2)),
    ok.

lextension(R, S, C) ->
    union(R, drestriction(1, constant_function(S, C), domain(R))).

weak_relation_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([], 2),
    eval(weak_relation(E), E),
    eval(weak_relation(ER), ER),
    eval(weak_relation(relation([{a,1},{a,2},{b,2},{c,c}])),
	 relation([{1,1},{2,2},{a,1},{a,2},{a,a},{b,2},{b,b},{c,c}])),
    eval(weak_relation(relation([{a,1},{a,a},{a,b}])),
	 relation([{1,1},{a,1},{a,a},{a,b},{b,b}])),
    eval(weak_relation(relation([{a,1},{a,b},{7,w}])),
	 relation([{1,1},{7,7},{7,w},{a,1},{a,a},{a,b},{b,b},{w,w}])),
    {'EXIT', {badarg, _}} =
	(catch weak_relation(from_term([{{a},a}]))),
    {'EXIT', {badarg, _}} =
	(catch weak_relation(from_term([{a,a}],[{d,r}]))),
    {'EXIT', {badarg, _}} = (catch weak_relation(relation([{1,2,3}]))),

    F = 0.0, I = round(F),
    if
        F == I -> % term ordering
            FR1 = relation([{F,I}]),
            eval(weak_relation(FR1), FR1),
            FR2 = relation([{F,2},{I,1}]),
            true = no_elements(weak_relation(FR2)) =:= 5,
            FR3 = relation([{1,0},{1.0,1}]),
            true = no_elements(weak_relation(FR3)) =:= 3;
        true -> 
            ok
    end,
    ok.

to_sets_1(Conf) when is_list(Conf) ->
    {'EXIT', {badarg, _}} = (catch to_sets(from_term(a))),
    {'EXIT', {function_clause, _}} = (catch to_sets(a)),
    %% unordered
    [] = to_sets(empty_set()),
    eval(to_sets(from_term([a])), [from_term(a)]),
    eval(to_sets(from_term([[]],[[atom]])), [set([])]),

    L = [from_term([a,b]),from_term([c,d])],
    eval(to_sets(from_sets(L)), L),

    eval(to_sets(relation([{a,1},{b,2}])),
	 [from_term({a,1},{atom,atom}), from_term({b,2},{atom,atom})]),

    %% ordered
    O = {from_term(a,atom), from_term({b}, {atom}), set([c,d])},
    eval(to_sets(from_sets(O)), O),
    ok.


specification(Conf) when is_list(Conf) ->
    Fun = {external, fun(I) when is_integer(I) -> true; (_) -> false end},
    [1,2,3] = to_external(specification(Fun, set([a,1,b,2,c,3]))),

    Fun2 = fun(S) -> is_subset(S, set([1,3,5,7,9])) end,
    S2 = from_term([[1],[2],[3],[4],[5],[6],[7]]),
    eval(specification(Fun2, S2), from_term([[1],[3],[5],[7]])),
    Fun2x = fun([1]) -> true;
	       ([3]) -> true;
	       (_) -> false 
	    end,
    eval(specification({external,Fun2x}, S2), from_term([[1],[3]])),

    Fun3 = fun(_) -> neither_true_nor_false end,
    {'EXIT', {badarg, _}} =
	(catch specification(Fun3, set([a]))),
    {'EXIT', {badarg, _}} =
	(catch specification({external, Fun3}, set([a]))),
    {'EXIT', {badarg, _}} =
	(catch specification(Fun3, from_term([[a]]))),
    {'EXIT', {function_clause, _}} =
	(catch specification(Fun, a)),
    ok.

union_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([], 2),
    {'EXIT', {badarg, _}} = (catch union(ER)),
    {'EXIT', {type_mismatch, _}} =
	(catch union(relation([{a,b}]), relation([{a,b,c}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch union(from_term([{a,b}]), from_term([{c,[x]}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch union(from_term([{a,b}]), from_term([{c,d}], [{d,r}]))),
    {'EXIT', {badarg, _}} = (catch union(set([a,b,c]))),
    eval(union(E), E),
    eval(union(from_term([[]],[[atom]])), set([])),
    eval(union(from_term([[{a,b},{b,c}],[{b,c}]])),
	 relation([{a,b},{b,c}])),
    eval(union(from_term([[1,2,3],[2,3,4],[3,4,5]])),
	 set([1,2,3,4,5])),

    eval(union(from_term([{[a],[],c}]), from_term([{[],[],q}])),
	 from_term([{[a],[],c},{[],[],q}])),

    eval(union(E, E), E),
    eval(union(set([a,b]), E), set([a,b])),
    eval(union(E, set([a,b])), set([a,b])),

    eval(union(from_term([[a,b]])), from_term([a,b])),
    ok.

intersection_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    {'EXIT', {badarg, _}} = (catch intersection(from_term([a,b]))),
    {'EXIT', {badarg, _}} = (catch intersection(E)),
    {'EXIT', {type_mismatch, _}} =
	(catch intersection(relation([{a,b}]), relation([{a,b,c}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch intersection(relation([{a,b}]), from_term([{a,b}],[{d,r}]))),

    eval(intersection(from_term([[a,b,c],[d,e,f],[g,h,i]])), set([])),

    eval(intersection(E, E), E),
    eval(intersection(set([a,b,c]),set([0,b,q])),
	 set([b])),
    eval(intersection(set([0,b,q]),set([a,b,c])),
	 set([b])),
    eval(intersection(set([a,b,c]),set([a,b,c])),
	 set([a,b,c])),
    eval(intersection(set([a,b,d]),set([c,d])),
	 set([d])),
    ok.

difference(Conf) when is_list(Conf) ->
    E = empty_set(),
    {'EXIT', {type_mismatch, _}} =
	(catch difference(relation([{a,b}]), relation([{a,b,c}]))),
    eval(difference(E, E), E),
    {'EXIT', {type_mismatch, _}} =
	(catch difference(relation([{a,b}]), from_term([{a,c}],[{d,r}]))),
    eval(difference(set([a,b,c,d,f]), set([a,d,e,g])),
	 set([b,c,f])),
    eval(difference(set([a,b,c]), set([d,e,f])),
	 set([a,b,c])),
    eval(difference(set([a,b,c]), set([a,b,c,d,e,f])),
	 set([])),
    eval(difference(set([e,f,g]), set([a,b,c,e])),
	 set([f,g])),
    eval(difference(set([a,b,d,e,f]), set([c])),
	 set([a,b,d,e,f])),
    ok.

symdiff(Conf) when is_list(Conf) ->
    E = empty_set(),
    {'EXIT', {type_mismatch, _}} =
	(catch symdiff(relation([{a,b}]), relation([{a,b,c}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch symdiff(relation([{a,b}]), from_term([{a,b}], [{d,r}]))),
    eval(symdiff(E, E), E),
    eval(symdiff(set([a,b,c,d,e,f]), set([0,1,a,c])),
	 union(set([b,d,e,f]), set([0,1]))),
    eval(symdiff(set([a,b,c]), set([q,v,w,x,y])),
	 union(set([a,b,c]), set([q,v,w,x,y]))),
    eval(symdiff(set([a,b,c,d,e,f]), set([a,b,c])),
	 set([d,e,f])),
    eval(symdiff(set([c,e,g,h,i]), set([b,d,f])),
	 union(set([c,e,g,h,i]), set([b,d,f]))),
    eval(symdiff(set([c,d,g,h,k,l]),
		 set([a,b,e,f,i,j,m,n])),
	 union(set([c,d,g,h,k,l]), set([a,b,e,f,i,j,m,n]))),
    eval(symdiff(set([c,d,g,h,k,l]),
		 set([d,e,h,i,l,m,n,o,p])),
	 union(set([c,g,k]), set([e,i,m,n,o,p]))),
    ok.

symmetric_partition(Conf) when is_list(Conf) ->
    E = set([]),
    S1 = set([1,2,3,4]),
    S2 = set([3,4,5,6]),
    S3 = set([3,4]),
    S4 = set([1,2,3,4,5,6]),
    T1 = set([1,2]),
    T2 = set([3,4]),
    T3 = set([5,6]),
    T4 = set([1,2,5,6]),
    {'EXIT', {type_mismatch, _}} =
	(catch symmetric_partition(relation([{a,b}]), relation([{a,b,c}]))),
    {E, E, E} = symmetric_partition(E, E),
    {'EXIT', {type_mismatch, _}} =
	(catch symmetric_partition(relation([{a,b}]), 
                                   from_term([{a,c}],[{d,r}]))),
    {E, E, S1} = symmetric_partition(E, S1),
    {S1, E, E} = symmetric_partition(S1, E),
    {T1, T2, T3} = symmetric_partition(S1, S2),
    {T3, T2, T1} = symmetric_partition(S2, S1),
    {E, T2, T4} = symmetric_partition(S3, S4),
    {T4, T2, E} = symmetric_partition(S4, S3),

    S5 = set([1,3,5]),
    S6 = set([2,4,6,7,8]),
    {S5, E, S6} = symmetric_partition(S5, S6),
    {S6, E, S5} = symmetric_partition(S6, S5),
    EE = empty_set(),
    {EE, EE, EE} = symmetric_partition(EE, EE),

    ok.

is_sofs_set_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    true = is_sofs_set(E),
    true = is_sofs_set(from_term([a])),
    true = is_sofs_set(from_term({a})),
    true = is_sofs_set(from_term(a)),
    false = is_sofs_set(a),
    ok.

is_set_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    true = is_set(E),
    true = is_set(from_term([a])),
    false = is_set(from_term({a})),
    false = is_set(from_term(a)),
    {'EXIT', _} = (catch is_set(a)),

    true = is_empty_set(E),
    false = is_empty_set(from_term([a])),
    false = is_empty_set(from_term({a})),
    false = is_empty_set(from_term(a)),
    {'EXIT', _} = (catch is_empty_set(a)),

    ok.

is_equal(Conf) when is_list(Conf) ->
    E = empty_set(),
    true = is_equal(E, E),
    false = is_equal(from_term([a]), E),
    {'EXIT', {type_mismatch, _}} =
	(catch is_equal(intersection(set([a]), set([b])),
			intersection(from_term([{a}]), from_term([{b}])))),
    {'EXIT', {type_mismatch, _}} =
	(catch is_equal(from_term([],[{[atom],atom,[atom]}]), 
			from_term([],[{[atom],{atom},[atom]}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch is_equal(set([a]), from_term([a],[type]))),

    E2 = from_sets({from_term(a,atom)}),
    true = is_equal(E2, E2),
    true = is_equal(from_term({a}, {atom}), E2),
    false = is_equal(from_term([{[a],[],c}]),
		     from_term([{[],[],q}])),

    {'EXIT', {type_mismatch, _}} =
        (catch is_equal(E, E2)),
    {'EXIT', {type_mismatch, _}} =
        (catch is_equal(E2, E)),
    true = is_equal(from_term({[],a,[]},{[atom],atom,[atom]}),
		    from_term({[],a,[]},{[atom],atom,[atom]})),
    {'EXIT', {type_mismatch, _}} =
	(catch is_equal(from_term({[],a,[]},{[atom],atom,[atom]}), 
			from_term({[],{a},[]},{[atom],{atom},[atom]}))),
    {'EXIT', {type_mismatch, _}} =
	(catch is_equal(from_term({a}), from_term({a},{type}))),

    ok.

is_subset(Conf) when is_list(Conf) ->
    E = empty_set(),
    true = is_subset(E, E),
    true = is_subset(set([a,c,e]), set([a,b,c,d,e])),
    false = is_subset(set([a,b]), E),
    false = is_subset(set([d,e,f]), set([b,c,d,e])),
    false = is_subset(set([a,b,c]), set([b,c])),
    false = is_subset(set([b,c]), set([a,c])),
    false = is_subset(set([d,e]), set([a,b])),
    {'EXIT', {type_mismatch, _}} =
	(catch is_subset(intersection(set([a]), set([b])),
			 intersection(from_term([{a}]), from_term([{b}])))),
    {'EXIT', {type_mismatch, _}} =
        (catch is_subset(set([a]), from_term([a,b], [at]))),
    ok.

is_a_function_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([], 2),
    {'EXIT', {badarg, _}} = (catch is_a_function(set([a,b]))),
    true = is_a_function(E),
    true = is_a_function(ER),
    true = is_a_function(relation([])),
    true = is_a_function(relation([],2)),
    true = is_a_function(relation([{a,b},{b,c}])),
    false = is_a_function(relation([{a,b},{b,c},{b,d},{e,f}])),
    IS = relation([{{a,b},c},{{a,b},d}]),
    false = is_a_function(IS),
    F = 0.0, I = round(F),
    FR = relation([{I,F},{F,1}]),
    if
        F == I -> % term ordering
            false = is_a_function(FR);
        true -> 
            true = is_a_function(FR)
    end,
    ok.

is_disjoint(Conf) when is_list(Conf) ->
    E = empty_set(),
    {'EXIT', {type_mismatch, _}} =
	(catch is_disjoint(relation([{a,1}]), set([a,b]))),
    {'EXIT', {type_mismatch, _}} =
	(catch is_disjoint(set([a]), from_term([a],[mota]))),
    true = is_disjoint(E, E),
    false = is_disjoint(set([a,b,c]),set([b,c,d])),
    false = is_disjoint(set([b,c,d]),set([a,b,c])),
    true = is_disjoint(set([a,c,e]),set([b,d,f])),
    ok.

join(Conf) when is_list(Conf) ->
    E = empty_set(),

    {'EXIT', {badarg, _}} = (catch join(relation([{a,1}]), 3, E, 5)),
    {'EXIT', {badarg, _}} = (catch join(E, 1, relation([{a,1}]), 3)),
    {'EXIT', {badarg, _}} = (catch join(E, 1, from_term([a]), 1)),

    eval(join(E, 1, E, 2), E),
    eval(join(E, 1, from_term([{{a},b}]), 2), E),
    eval(join(from_term([{{a},b}]), 2, E, 1), E),
    eval(join(from_term([{{a},b,e}]), 2, from_term([{c,{d}}]), 1),
	 from_term([], [{{atom},atom,atom,{atom}}])),
    eval(join(relation([{a}]), 1, relation([{1,a},{2,a}]), 2),
	 relation([{a,1},{a,2}])),
    eval(join(relation([{a,b,c},{b,c,d}]), 2,
	      relation([{1,b},{2,a},{3,c}]), 2),
	 relation([{a,b,c,1},{b,c,d,3}])),
    eval(join(relation([{1,a,aa},{1,b,bb},{1,c,cc},{2,a,aa},{2,b,bb}]),
	      1,
	      relation([{1,c,cc},{1,d,dd},{1,e,ee},{2,c,cc},{2,d,dd}]),
	      1),
	 relation([{1,a,aa,c,cc},{1,a,aa,d,dd},{1,a,aa,e,ee},{1,b,bb,c,cc},
		   {1,b,bb,d,dd},{1,b,bb,e,ee},{1,c,cc,c,cc},{1,c,cc,d,dd},
		   {1,c,cc,e,ee},{2,a,aa,c,cc},{2,a,aa,d,dd},{2,b,bb,c,cc},
		   {2,b,bb,d,dd}])),

    R1 = relation([{a,b},{b,c}]),
    R2 = relation([{b,1},{a,2},{c,3},{c,4}]),
    eval(join(R1, 1, R2, 1), from_term([{a,b,2},{b,c,1}])),
    eval(join(R1, 2, R2, 1), from_term([{a,b,1},{b,c,3},{b,c,4}])),
    eval(join(R1, 1, converse(R2), 2),
	 from_term([{a,b,2},{b,c,1}])),
    eval(join(R1, 2, converse(R2), 2),
	 from_term([{a,b,1},{b,c,3},{b,c,4}])),
    ok.

canonical(Conf) when is_list(Conf) ->
    E = empty_set(),
    {'EXIT', {badarg, _}} =
        (catch canonical_relation(set([a,b]))),
    eval(canonical_relation(E), E),
    eval(canonical_relation(from_term([[]])), E),
    eval(canonical_relation(from_term([[a,b,c]])),
	 from_term([{a,[a,b,c]},{b,[a,b,c]},{c,[a,b,c]}])),
    ok.

relation_to_family_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    EF = family([]),
    eval(relation_to_family(E), E),
    eval(relation_to_family(relation([])), EF),
    eval(relation_to_family(relation([], 2)), EF),
    R = relation([{b,1},{c,7},{c,9},{c,11}]),
    F = family([{b,[1]},{c,[7,9,11]}]),
    eval(relation_to_family(R), F),
    eval(sofs:rel2fam(R), F),
    {'EXIT', {badarg, _}} = (catch relation_to_family(set([a]))),
    ok.

domain_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),
    {'EXIT', {badarg, _}} = (catch domain(relation([],3))),
    eval(domain(E), E),
    eval(domain(ER), set([])),
    eval(domain(relation([{1,a},{1,b},{2,a},{2,b}])), set([1,2])),
    eval(domain(relation([{a,1},{b,2},{c,3}])), set([a,b,c])),
    eval(field(relation([{a,1},{b,2},{c,3}])),
	 set([a,b,c,1,2,3])),
    F = 0.0, I = round(F),
    FR = relation([{I,a},{F,b}]),
    if
        F == I -> % term ordering
            true = (1 =:= no_elements(domain(FR)));
        true -> 
            true = (2 =:= no_elements(domain(FR)))
    end,
    ok.

range_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),
    {'EXIT', {badarg, _}} = (catch range(relation([],3))),
    eval(range(E), E),
    eval(range(ER), set([])),
    eval(range(relation([{1,a},{1,b},{2,a},{2,b}])), set([a,b])),
    eval(range(relation([{a,1},{b,2},{c,3}])), set([1,2,3])),
    ok.

inverse_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),
    {'EXIT', {badarg, _}} = (catch inverse(relation([],3))),
    {'EXIT', {bad_function, _}} =
	(catch inverse(relation([{1,a},{1,b}]))),
    {'EXIT', {bad_function, _}} =
	(catch inverse(relation([{1,a},{2,a}]))),
    eval(inverse(E), E),
    eval(inverse(ER), ER),
    eval(inverse(relation([{a,1},{b,2},{c,3}])),
	 relation([{1,a},{2,b},{3,c}])),
    F = 0.0, I = round(F),
    FR = relation([{I,a},{F,b}]),
    if
        F == I -> % term ordering
            {'EXIT', {bad_function, _}} = (catch inverse(FR));
        true -> 
            eval(inverse(FR), relation([{a,I},{b,F}]))
    end,
    ok.

converse_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),
    {'EXIT', {badarg, _}} = (catch converse(relation([],3))),
    eval(converse(ER), ER),
    eval(converse(E), E),
    eval(converse(relation([{a,1},{b,2},{c,3}])),
	 relation([{1,a},{2,b},{3,c}])),
    eval(converse(relation([{1,a},{1,b}])),
	 relation([{a,1},{b,1}])),
    eval(converse(relation([{1,a},{2,a}])),
	 relation([{a,1},{a,2}])),
    ok.

no_elements_1(Conf) when is_list(Conf) ->
    0 = no_elements(empty_set()),
    0 = no_elements(set([])),
    1 = no_elements(from_term([a])),
    10 = no_elements(from_term(lists:seq(1,10))),
    3 = no_elements(from_term({a,b,c},{atom,atom,atom})),
    {'EXIT', {badarg, _}} = (catch no_elements(from_term(a))),
    {'EXIT', {function_clause, _}} = (catch no_elements(a)),
    ok.

image(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),
    eval(image(E, E), E),
    eval(image(ER, E), set([])),
    eval(image(relation([{a,1},{b,2},{c,3},{f,6}]), set([a,b,c,d,f])),
	 set([1,2,3,6])),
    eval(image(relation([{a,1},{b,2},{c,3},{d,4},{r,17}]),
	       set([b,c,q,r])),
	 set([2,3,17])),
    eval(image(from_term([{[a],{1}},{[b],{2}}]), from_term([[a]])),
	 from_term([{1}])),
    eval(image(relation([{1,a},{2,a},{3,a},{4,b},{2,b}]), set([1,2,4])),
	 set([a,b])),
    {'EXIT', {badarg, _}} =
	(catch image(from_term([a,b]), E)),
    {'EXIT', {type_mismatch, _}} =
	(catch image(from_term([{[a],1}]), set([[a]]))),
    ok.

inverse_image(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),
    eval(inverse_image(E, E), E),
    eval(inverse_image(ER, E), set([])),
    eval(inverse_image(converse(relation([{a,1},{b,2},{c,3},{f,6}])),
		       set([a,b,c,d,f])),
	 set([1,2,3,6])),
    eval(inverse_image(converse(relation([{a,1},{b,2},{c,3},
					  {d,4},{r,17}])),
		       set([b,c,q,r])),
	 set([2,3,17])),
    eval(inverse_image(converse(from_term([{[a],{1}},{[b],{2}}])),
		       from_term([[a]])),
	 from_term([{1}])),
    eval(inverse_image(converse(relation([{1,a},{2,a},
					  {3,a},{4,b},{2,b}])),
		       set([1,2,4])),
	 set([a,b])),
    {'EXIT', {badarg, _}} =
	(catch inverse_image(from_term([a,b]), E)),
    {'EXIT', {type_mismatch, _}} =
	(catch inverse_image(converse(from_term([{[a],1}])), set([[a]]))),
    ok.

composite_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    EF = a_function([]),
    eval(composite(E, E), E),
    eval(composite(E, a_function([{a,b}])), E),
    eval(composite(relation([{a,b}]), E), E),
    {'EXIT', {bad_function, _}} =
	(catch composite(EF, relation([{a,b},{a,c}]))),
    {'EXIT', {bad_function, _}} =
	(catch composite(a_function([{b,a}]), EF)),
    {'EXIT', {bad_function, _}} =
	(catch composite(relation([{1,a},{2,b},{2,a}]), 
			 a_function([{a,1},{b,3}]))),
    {'EXIT', {bad_function, _}} =
	(catch composite(a_function([{1,a},{2,b}]), a_function([{b,3}]))),
    eval(composite(EF, EF), EF),
    eval(composite(a_function([{b,a}]), from_term([{a,{b,c}}])),
	 from_term([{b,{b,c}}])),
    eval(composite(a_function([{q,1},{z,2}]),
		   a_function([{1,a},{2,a}])),
	 a_function([{q,a},{z,a}])),
    eval(composite(a_function([{a,0},{b,0},{c,1},{d,1},{e,2},{f,3}]),
		   a_function([{0,p},{1,q},{2,r},{3,w},{4,aa}])),
	 a_function([{c,q},{d,q},{f,w},{e,r},{a,p},{b,p}])),
    eval(composite(a_function([{1,c}]),
		   a_function([{a,1},{b,3},{c,4}])),
	 a_function([{1,4}])),
    {'EXIT', {bad_function, _}} =
	(catch composite(a_function([{1,a},{2,b}]), 
			 a_function([{a,1},{c,3}]))),
    {'EXIT', {badarg, _}} =
	(catch composite(from_term([a,b]), E)),
    {'EXIT', {badarg, _}} =
	(catch composite(E, from_term([a,b]))),
    {'EXIT', {type_mismatch, _}} =
        (catch composite(from_term([{a,b}]), from_term([{{a},b}]))),
    {'EXIT', {type_mismatch, _}} =
        (catch composite(from_term([{a,b}]), 
			 from_term([{b,c}], [{d,r}]))),
    F = 0.0, I = round(F),
    FR1 = relation([{1,c}]),
    FR2 = relation([{I,1},{F,3},{c,4}]),
    if
        F == I -> % term ordering
            {'EXIT', {bad_function, _}} = (catch composite(FR1, FR2));
        true -> 
            eval(composite(FR1, FR2), a_function([{1,4}]))
    end,
    ok.

relative_product_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),
    eval(relative_product1(E, E), E),
    eval(relative_product1(E, relation([{a,b}])), E),
    eval(relative_product1(relation([{a,b}]), E), E),
    eval(relative_product1(relation([{a,b}]), from_term([{a,{b,c}}])),
	 from_term([{b,{b,c}}])),
    eval(relative_product1(relation([{1,z},{1,q},{2,z}]),
			   relation([{1,a},{1,b},{2,a}])),
	 relation([{q,a},{q,b},{z,a},{z,b}])),
    eval(relative_product1(relation([{0,a},{0,b},{1,c},
				     {1,d},{2,e},{3,f}]),
			   relation([{1,q},{3,w}])),
	 relation([{c,q},{d,q},{f,w}])),
    {'EXIT', {badarg, _}} =
	(catch relative_product1(from_term([a,b]), ER)),
    {'EXIT', {badarg, _}} =
	(catch relative_product1(ER, from_term([a,b]))),
    {'EXIT', {type_mismatch, _}} =
        (catch relative_product1(from_term([{a,b}]), from_term([{{a},b}]))),
    {'EXIT', {type_mismatch, _}} =
        (catch relative_product1(from_term([{a,b}]), 
				 from_term([{b,c}], [{d,r}]))),
    ok.

relative_product_2(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),

    {'EXIT', {badarg, _}} = (catch relative_product({from_term([a,b])})),
    {'EXIT', {type_mismatch, _}} =
	(catch relative_product({from_term([{a,b}]), from_term([{{a},b}])})),
    {'EXIT', {badarg, _}} = (catch relative_product({})),
    true = is_equal(relative_product({ER}),
		    from_term([], [{atom,{atom}}])),
    eval(relative_product({relation([{a,b},{c,a}]),
			   relation([{a,1},{a,2}]),
			   relation([{a,aa},{c,1}])}),
	 from_term([{a,{b,1,aa}},{a,{b,2,aa}}])),
    eval(relative_product({relation([{a,b}])}, E), E),
    eval(relative_product({E}, relation([{a,b}])), E),
    eval(relative_product({E,from_term([], [{{atom,atom,atom},atom}])}),
	 E),
    {'EXIT', {badarg, _}} =
        (catch relative_product({from_term([a,b])}, E)),
    {'EXIT', {badarg, _}} =
	(catch relative_product({relation([])}, set([]))),
    {'EXIT', {type_mismatch, _}} =
	(catch relative_product({from_term([{a,b}]), 
				 from_term([{{a},b}])}, ER)),

    {'EXIT', {badarg, _}} = (catch relative_product({}, ER)),
    relprod2({relation([{a,b}])}, from_term([],[{{atom},atom}]), ER),
    relprod2({relation([{a,b}]),relation([{a,1}])},
	     from_term([{{b,1},{tjo,hej,sa}}]),
	     from_term([{a,{tjo,hej,sa}}])),
    relprod2({relation([{a,b}]), ER}, from_term([{{a,b},b}]), ER),
    relprod2({relation([{a,b},{c,a}]),
	      relation([{a,1},{a,2}])},
	     from_term([{{b,1},b1},{{b,2},b2}]),
	     relation([{a,b1},{a,b2}])),
    eval(relative_product({relation([{a,b}]), ER}),
	 from_term([],[{atom,{atom,atom}}])),
    eval(relative_product({from_term([{{a,[a,b]},[a]}]),
			   from_term([{{a,[a,b]},[[a,b]]}])}),
	 from_term([{{a,[a,b]},{[a],[[a,b]]}}])),
    ok.

relprod2(A1T, A2, R) ->
    %% A tuple as first argument is the old interface:
    eval(relative_product(A1T, A2), R),
    eval(relative_product(tuple_to_list(A1T), A2), R).

product_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    eval(product(E, E), E),
    eval(product(relation([]), E), E),
    eval(product(E, relation([])), E),
    eval(product(relation([{a,b}]),relation([{c,d}])),
	 from_term([{{a,b},{c,d}}],[{{atom,atom},{atom,atom}}])),

    eval(product({E, set([a,b,c])}), E),
    eval(product({set([a,b,c]), E}), E),
    eval(product({set([a,b,c]), E, E}), E),
    eval(product({E,E}), E),
    eval(product({set([a,b]),set([1,2])}),
	 relation([{a,1},{a,2},{b,1},{b,2}])),
    eval(product({from_term([a,b]), from_term([{a,b},{c,d}]),
		  from_term([1])}),
	 from_term([{a,{a,b},1},{a,{c,d},1},{b,{a,b},1},{b,{c,d},1}])),
    {'EXIT', {badarg, _}} = (catch product({})),
    {'EXIT', {badarg, _}} = (catch product({foo})),
    eval(product({E}), E),
    eval(product({E, E}), E),
    eval(product(set([a,b]), set([1,2])),
	 relation([{a,1},{a,2},{b,1},{b,2}])),
    eval(product({relation([]), E}), E),
    ok.

partition_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),
    Id = fun(A) -> A end,
    S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    eval(partition(1, E), E),
    eval(partition(2, E), E),
    eval(partition(1, ER), from_term([], [type(ER)])),
    eval(partition(2, ER), from_term([], [type(ER)])),
    eval(partition(1, relation([{1,a},{1,b},{2,c},{2,d}])),
	 from_term([[{1,a},{1,b}],[{2,c},{2,d}]])),
    eval(partition(2, relation([{1,a},{1,b},{2,a},{2,b},{3,c}])),
	 from_term([[{1,a},{2,a}],[{1,b},{2,b}],[{3,c}]])),
    eval(partition(2, relation([{1,a}])), from_term([[{1,a}]])),
    eval(partition(2, relation([{1,a},{2,b}])),
	 from_term([[{1,a}],[{2,b}]])),
    eval(partition(2, relation([{1,a},{2,a},{3,a}])),
	 from_term([[{1,a},{2,a},{3,a}]])),
    eval(partition(2, relation([{1,b},{2,a}])), % OTP-4516
	 from_term([[{1,b}],[{2,a}]])),
    eval(union(partition(Id, S1)), S1),
    eval(partition({external, fun({A,{B,_}}) -> {A,B} end},
		   from_term([{a,{b,c}},{b,{c,d}},{a,{b,f}}])),
	 from_term([[{a,{b,c}},{a,{b,f}}],[{b,{c,d}}]])),
    F = 0.0, I = round(F),
    FR = relation([{I,a},{F,b}]),
    if
        F == I -> % term ordering
            eval(partition(1, FR), from_term([[{I,a},{F,b}]]));
        true -> 
            eval(partition(1, FR), from_term([[{I,a}],[{F,b}]]))
    end,
    {'EXIT', {badarg, _}} = (catch partition(2, set([a]))),
    {'EXIT', {badarg, _}} = (catch partition(1, set([a]))),
    eval(partition(Id, set([a])), from_term([[a]])),

    eval(partition(E), E),
    P1 = from_term([[a,b,c],[d,e,f],[g,h]]),
    P2 = from_term([[a,d],[b,c,e,f,q,v]]),
    eval(partition(union(P1, P2)),
	 from_term([[a],[b,c],[d],[e,f],[g,h],[q,v]])),
    {'EXIT', {badarg, _}} = (catch partition(from_term([a]))),
    ok.

partition_3(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),

    %% set of ordered sets
    S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    eval(partition(1, S1, set([0,1,d,e])),
	 lpartition(1, S1, set([0,1,d,e]))),
    eval(partition(1, S1, E), lpartition(1, S1, E)),
    eval(partition(2, ER, set([a,b])), lpartition(2, ER, set([a,b]))),

    XFun1 = {external, fun({_A,B,C}) -> {B,C} end},
    R1a = relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
    R1b = relation([{bb,2},{cc,3}]),
    eval(partition(XFun1, R1a, R1b), lpartition(XFun1, R1a, R1b)),

    Id = fun(X) -> X end,
    XId = {external, Id},
    R2 = relation([{a,b}]),
    eval(partition(XId, R2, E), lpartition(XId, R2, E)),

    R3 = relation([{b,d}]),
    eval(partition(XId, E, R3), lpartition(XId, E, R3)),

    Fun1 = fun(S) -> {_A,B,C} = to_external(S), from_term({B,C}) end,
    R4a = relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
    R4b = relation([{bb,2},{cc,3}]),
    eval(partition(Fun1,R4a,R4b), lpartition(Fun1,R4a,R4b)),

    XFun2 = {external, fun({_,{A},B}) -> {A,B} end},
    R5a = from_term([{a,{aa},1},{b,{bb},2},{c,{cc},3}]),
    R5b = from_term([{bb,2},{cc,3}]),
    eval(partition(XFun2,R5a, R5b), lpartition(XFun2,R5a, R5b)),

    R6 = relation([{a,b}]),
    eval(partition(2, R6, E), lpartition(2, R6, E)),

    R7 = relation([{b,d}]),
    eval(partition(2, E, R7), lpartition(2, E, R7)),

    S2 = set([a]),
    eval(partition(XId, E, S2), lpartition(XId, E, S2)),
    eval(partition(XId, S1, E), lpartition(XId, S1, E)),
    {'EXIT', {badarg, _}} =
	(catch partition(3, relation([{a,b}]), E)),
    {'EXIT', {badarg, _}} =
	(catch partition(3, relation([{a,b}]), relation([{b,d}]))),
    {'EXIT', {badarg, _}} =
	(catch partition(3, relation([{a,b}]), set([{b,d}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch partition(2, relation([{a,b}]), relation([{b,d}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch partition({external, fun({A,_B}) -> A end}, 
			 relation([{a,b}]), relation([{b,d}]))),
    {'EXIT', {badarg, _}} =
	(catch partition({external, fun({A,_}) -> {A,0} end}, 
			 from_term([{1,a}]),
			 from_term([{1,0}]))),

    S18a = relation([{1,e},{2,b},{3,c},{4,b},{5,a},{6,0}]),
    S18b = set([b,d,f]),
    eval(partition({external,fun({_,X}) -> X end}, S18a, S18b),
	 lpartition({external,fun({_,X}) -> X end}, S18a, S18b)),
    S19a = sofs:relation([{3,a},{8,b}]),
    S19b = set([2,6,7]),
    eval(partition({external,fun({X,_}) -> X end}, S19a, S19b),
	 lpartition({external,fun({X,_}) -> X end}, S19a, S19b)),

    R8a = relation([{a,d},{b,e},{c,b},{d,c}]),
    S8 = set([b,d]),
    eval(partition(2, R8a, S8), lpartition(2, R8a, S8)),

    S16a = relation([{1,e},{2,b},{3,c},{4,b},{5,a},{6,0}]),
    S16b = set([b,c,d]),
    eval(partition(2, S16a, S16b), lpartition(2, S16a, S16b)),
    S17a = relation([{e,1},{b,2},{c,3},{b,4},{a,5},{0,6}]),
    S17b = set([b,c,d]),
    eval(partition(1, S17a, S17b), lpartition(1, S17a, S17b)),

    {'EXIT', {function_clause, _}} =
	(catch partition({external, fun({A,_B}) -> A end}, set([]), E)),

    Fun3 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    S9a = set([1,2]),
    S9b = from_term([{1,0}]),
    eval(partition(Fun3, S9a, S9b), lpartition(Fun3, S9a, S9b)),

    S14a = relation([{1,a},{2,b},{3,c},{0,0}]),
    S14b = set([b,c]),
    eval(partition(2, S14a, S14b), lpartition(2, S14a, S14b)),
    S15a = relation([{a,1},{b,2},{c,3},{0,0}]),
    S15b = set([b,c]),
    eval(partition(1, S15a, S15b), lpartition(1, S15a, S15b)),

    %% set of sets
    {'EXIT', {badarg, _}} =
        (catch partition({external, fun(X) -> X end}, 
			 from_term([], [[atom]]), set([a]))),

    S10 = from_term([], [[atom]]),
    eval(partition(Id, S10, E), lpartition(Id, S10, E)),

    S10e = from_term([[a],[b]], [[atom]]),
    eval(partition(Id, S10e, E), lpartition(Id, S10e, E)),

    S11a = from_term([], [[atom]]),
    S11b = set([a]),
    eval(partition(Id, S11a, S11b), lpartition(Id, S11a, S11b)),

    S12a = from_term([[[a],[b]], [[b],[c]], [[], [a,b]], [[1],[2]]]),
    S12b = from_term([[a,b],[1,2,3],[b,c]]),
    eval(partition(fun sofs:union/1, S12a, S12b),
	 lpartition(fun sofs:union/1, S12a, S12b)),

    Fun13 = fun(_) -> from_term([a]) end,
    S13a = from_term([], [[atom]]),
    S13b = from_term([], [[a]]),
    eval(partition(Fun13, S13a, S13b), lpartition(Fun13, S13a, S13b)),

    {'EXIT', {type_mismatch, _}} =
        (catch partition(fun(_) -> from_term([a]) end, 
			 from_term([[1,2],[3,4]]),
			 from_term([], [atom]))),
    Fun10 = fun(S) ->
		    %% Cheating a lot...
		    case to_external(S) of
			[1] -> from_term({1,1});
			_ -> S
		    end
	    end,
    {'EXIT', {type_mismatch, _}} =
        (catch partition(Fun10, from_term([[1]]), from_term([], [[atom]]))),
    {'EXIT', {type_mismatch, _}} =
        (catch partition(fun(_) -> from_term({a}) end, 
			 from_term([[a]]),
			 from_term([], [atom]))),
    {'EXIT', {badarg, _}} =
        (catch partition(fun(_) -> {a} end, 
			 from_term([[a]]),
			 from_term([], [atom]))),
    ok.

lpartition(F, S1, S2) ->
    {restriction(F, S1, S2), drestriction(F, S1, S2)}.

multiple_relative_product(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),
    T = relation([{a,1},{a,11},{b,2},{c,3},{c,33},{d,4}]),
    {'EXIT', {badarg, _}} =
        (catch multiple_relative_product({}, ER)),
    {'EXIT', {badarg, _}} =
	(catch multiple_relative_product({}, relation([{a,b}]))),
    eval(multiple_relative_product({E,T,T}, relation([], 3)), E),
    eval(multiple_relative_product({T,T,T}, E), E),
    eval(multiple_relative_product({T,T,T}, relation([],3)),
	 from_term([],[{{atom,atom,atom},{atom,atom,atom}}])),
    eval(multiple_relative_product({T,T,T},
                                   relation([{a,b,c},{c,d,a}])),
	 from_term([{{a,b,c},{1,2,3}}, {{a,b,c},{1,2,33}},
		    {{a,b,c},{11,2,3}}, {{a,b,c},{11,2,33}},
		    {{c,d,a},{3,4,1}}, {{c,d,a},{3,4,11}},
		    {{c,d,a},{33,4,1}}, {{c,d,a},{33,4,11}}])),
    {'EXIT', {type_mismatch, _}} =
	(catch multiple_relative_product({T}, from_term([{{a}}]))), 
    ok.

digraph(Conf) when is_list(Conf) ->
    T0 = ets:all(),
    E = empty_set(),
    R = relation([{a,b},{b,c},{c,d},{d,a}]),
    F = relation_to_family(R),
    Type = type(F),

    {'EXIT', {badarg, _}} =
        (catch family_to_digraph(set([a]))),
    digraph_fail(badarg, catch family_to_digraph(set([a]), [foo])),
    digraph_fail(badarg, catch family_to_digraph(F, [foo])),
    digraph_fail(cyclic, catch family_to_digraph(family([{a,[a]}]),[acyclic])),

    G1 = family_to_digraph(E),
    {'EXIT', {badarg, _}} = (catch digraph_to_family(G1, foo)),
    {'EXIT', {badarg, _}} = (catch digraph_to_family(G1, atom)),
    true = [] == to_external(digraph_to_family(G1)),
    true = [] == to_external(digraph_to_family(G1, Type)),
    true = digraph:delete(G1),

    G1a = family_to_digraph(E, [protected]),
    true = [] == to_external(digraph_to_family(G1a)),
    true = [] == to_external(digraph_to_family(G1a, Type)),
    true = digraph:delete(G1a),

    G2 = family_to_digraph(F),
    true = F == digraph_to_family(G2),
    true = F == digraph_to_family(G2, type(F)),
    true = digraph:delete(G2),

    R2 = from_term([{{a},b},{{c},d}]),
    F2 = relation_to_family(R2),
    Type2 = type(F2),
    G3 = family_to_digraph(F2, [protected]),
    true = is_subset(F2, digraph_to_family(G3, Type2)),
    true = digraph:delete(G3),

    Fl = 0.0, I = round(Fl),
    if
        Fl == I -> % term ordering
            G4 = digraph:new(),
            digraph:add_vertex(G4, Fl),
            digraph:add_vertex(G4, I),
            {'EXIT', {badarg, _}} =
                (catch digraph_to_family(G4, Type)),
            {'EXIT', {badarg, _}} =
                (catch digraph_to_family(G4)),
            true = digraph:delete(G4);
        true -> ok
    end,

    true = T0 == ets:all(),
    ok.

digraph_fail(ExitReason, Fail) ->
    {'EXIT', {ExitReason, [{sofs,family_to_digraph,A,_}|_]}} = Fail,
    case {test_server:is_native(sofs),A} of
	{false,[_,_]} -> ok;
	{true,2} -> ok
    end.

constant_function(Conf) when is_list(Conf) ->
    E = empty_set(),
    C = from_term(3),
    eval(constant_function(E, C), E),
    eval(constant_function(set([a,b]), E), from_term([{a,[]},{b,[]}])),
    eval(constant_function(set([a,b]), C), from_term([{a,3},{b,3}])),
    {'EXIT', {badarg, _}} = (catch constant_function(C, C)),
    {'EXIT', {badarg, _}} = (catch constant_function(set([]), foo)),
    ok.

misc(Conf) when is_list(Conf) ->
    %% find "relational" part of relation:
    S = relation([{a,b},{b,c},{b,d},{c,d}]),
    Id = fun(A) -> A end,
    RR = relational_restriction(S),
    eval(union(difference(partition(Id,S), partition(1,S))), RR),
    eval(union(difference(partition(1,S), partition(Id,S))), RR),

    %% the "functional" part:
    eval(union(intersection(partition(1,S), partition(Id,S))),
	 difference(S, RR)),
    {'EXIT', {undef, _}} =
        (catch projection(fun external:foo/1, set([a,b,c]))),
    ok.

relational_restriction(R) ->
    Fun = fun(S) -> no_elements(S) > 1 end,
    family_to_relation(family_specification(Fun, relation_to_family(R))).


family_specification(Conf) when is_list(Conf) ->
    E = empty_set(),
    %% internal
    eval(family_specification(fun sofs:is_set/1, E), E),
    {'EXIT', {badarg, _}} =
	(catch family_specification(fun sofs:is_set/1, set([]))),
    F1 = from_term([{1,[1]}]),
    eval(family_specification(fun sofs:is_set/1, F1), F1),
    Fun = fun(S) -> is_subset(S, set([0,1,2,3,4])) end,
    F2 = family([{a,[1,2]},{b,[3,4,5]}]),
    eval(family_specification(Fun, F2), family([{a,[1,2]}])),
    F3 = from_term([{a,[]},{b,[]}]),
    eval(family_specification(fun sofs:is_set/1, F3), F3),
    Fun2 = fun(_) -> throw(fippla) end, 
    fippla = (catch family_specification(Fun2, family([{a,[1]}]))),
    Fun3 = fun(_) -> neither_true_nor_false end,
    {'EXIT', {badarg, _}} =
	(catch family_specification(Fun3, F3)),

    %% external
    IsList = {external, fun(L) when is_list(L) -> true; (_) -> false end},
    eval(family_specification(IsList, E), E),
    eval(family_specification(IsList, F1), F1),
    MF = {external, fun(L) -> lists:member(3, L) end},
    eval(family_specification(MF, F2), family([{b,[3,4,5]}])),
    fippla = (catch family_specification(Fun2, family([{a,[1]}]))),
    {'EXIT', {badarg, _}} =
	(catch family_specification({external, Fun3}, F3)),
    ok.

family_domain_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = from_term([{a,[]},{b,[]}],[{atom,[{atom,atom}]}]),
    EF = from_term([{a,[]},{b,[]}],[{atom,[atom]}]),
    eval(family_domain(E), E),
    eval(family_domain(ER), EF),
    FR = from_term([{a,[{1,a},{2,b},{3,c}]},{b,[]},{c,[{4,d},{5,e}]}]),
    eval(family_domain(FR), from_term([{a,[1,2,3]},{b,[]},{c,[4,5]}])),
    eval(family_field(E), E),
    eval(family_field(FR),
	 from_term([{a,[a,b,c,1,2,3]},{b,[]},{c,[d,e,4,5]}])),
    eval(family_domain(from_term([{{a},[{{1,[]},c}]}])),
	 from_term([{{a},[{1,[]}]}])),
    eval(family_domain(from_term([{{a},[{{1,[a]},c}]}])),
	 from_term([{{a},[{1,[a]}]}])),
    eval(family_domain(from_term([{{a},[]}])),
	 from_term([{{a},[]}])),
    eval(family_domain(from_term([], type(FR))),
	 from_term([], [{atom,[atom]}])),
    {'EXIT', {badarg, _}} = (catch family_domain(set([a]))),
    {'EXIT', {badarg, _}} = (catch family_field(set([a]))),
    {'EXIT', {badarg, _}} = (catch family_domain(set([{a,[b]}]))),
    ok.

family_range_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = from_term([{a,[]},{b,[]}],[{atom,[{atom,atom}]}]),
    EF = from_term([{a,[]},{b,[]}],[{atom,[atom]}]),
    eval(family_range(E), E),
    eval(family_range(ER), EF),
    FR = from_term([{a,[{1,a},{2,b},{3,c}]},{b,[]},{c,[{4,d},{5,e}]}]),
    eval(family_range(FR), from_term([{a,[a,b,c]},{b,[]},{c,[d,e]}])),
    eval(family_range(from_term([{{a},[{c,{1,[a]}}]}])),
	 from_term([{{a},[{1,[a]}]}])),
    eval(family_range(from_term([{{a},[{c,{1,[]}}]}])),
	 from_term([{{a},[{1,[]}]}])),
    eval(family_range(from_term([{{a},[]}])),
	 from_term([{{a},[]}])),
    eval(family_range(from_term([], type(FR))),
	 from_term([], [{atom,[atom]}])),
    {'EXIT', {badarg, _}} = (catch family_range(set([a]))),
    {'EXIT', {badarg, _}} = (catch family_range(set([{a,[b]}]))),
    ok.

family_to_relation_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    ER = relation([]),
    EF = family([]),
    eval(family_to_relation(E), E),
    eval(family_to_relation(EF), ER),
    eval(sofs:fam2rel(EF), ER),
    F = family([{a,[]},{b,[1]},{c,[7,9,11]}]),
    eval(family_to_relation(F), relation([{b,1},{c,7},{c,9},{c,11}])),
    {'EXIT', {badarg, _}} = (catch family_to_relation(set([a]))),
    ok.

union_of_family_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    EF = from_term([{a,[]},{b,[]}],[{atom,[atom]}]),
    eval(union_of_family(E), E),
    eval(union_of_family(EF), set([])),
    eval(union_of_family(family([])), set([])),
    FR = from_term([{a,[1,2,3]},{b,[]},{c,[4,5]}]),
    eval(union_of_family(FR), set([1,2,3,4,5])),
    eval(union_of_family(sofs:family([{a,[1,2]},{b,[1,2]}])),
	 set([1,2])),
    {'EXIT', {badarg, _}} = (catch union_of_family(set([a]))),
    ok.

intersection_of_family_1(Conf) when is_list(Conf) ->
    EF = from_term([{a,[]},{b,[]}],[{atom,[atom]}]),
    eval(intersection_of_family(EF), set([])),
    FR = from_term([{a,[1,2,3]},{b,[2,3]},{c,[3,4,5]}]),
    eval(intersection_of_family(FR), set([3])),
    {'EXIT', {badarg, _}} =
        (catch intersection_of_family(family([]))),
    EE = from_term([], [[atom]]),
    {'EXIT', {badarg, _}} = (catch intersection_of_family(EE)),
    {'EXIT', {badarg, _}} = (catch intersection_of_family(set([a]))),
    ok.

family_projection(Conf) when is_list(Conf) ->
    SSType = [{atom,[[atom]]}],
    SRType = [{atom,[{atom,atom}]}],
    E = empty_set(),

    eval(family_projection(fun(X) -> X end, family([])), E),
    L1 = [{a,[]}],
    eval(family_projection(fun sofs:union/1, E), E),
    eval(family_projection(fun sofs:union/1, from_term(L1, SSType)),
	 family(L1)),
    {'EXIT', {badarg, _}} =
        (catch family_projection(fun sofs:union/1, set([]))),
    {'EXIT', {badarg, _}} =
        (catch family_projection(fun sofs:union/1, from_term([{1,[1]}]))),

    F2 = from_term([{a,[[1],[2]]},{b,[[3,4],[5]]}], SSType),
    eval(family_projection(fun sofs:union/1, F2),
	 family_union(F2)),

    F3 = from_term([{1,[{a,b},{b,c},{c,d}]},{3,[]},{5,[{3,5}]}],
		   SRType),
    eval(family_projection(fun sofs:domain/1, F3), family_domain(F3)),
    eval(family_projection(fun sofs:range/1, F3), family_range(F3)),

    eval(family_projection(fun(_) -> E end, family([{a,[b,c]}])),
	 from_term([{a,[]}])),

    Fun1 = fun(S) ->
                   case to_external(S) of
                       [1] -> from_term({1,1});
                       _ -> S
                   end
           end,
    eval(family_projection(Fun1, family([{a,[1]}])),
	 from_term([{a,{1,1}}])),
    Fun2 = fun(_) -> throw(fippla) end, 
    fippla =
        (catch family_projection(Fun2, family([{a,[1]}]))),
    {'EXIT', {type_mismatch, _}} =
        (catch family_projection(Fun1, from_term([{1,[1]},{2,[2]}]))),
    {'EXIT', {type_mismatch, _}} =
        (catch family_projection(Fun1, from_term([{1,[1]},{0,[0]}]))),

    eval(family_projection(fun(_) -> E end, from_term([{a,[]}])),
	 from_term([{a,[]}])),
    F4 = from_term([{a,[{1,2,3}]},{b,[{4,5,6}]},{c,[]},{m3,[]}]),
    Z = from_term(0),
    eval(family_projection(fun(S) -> local_adjoin(S, Z) end, F4),
	 from_term([{a,[{{1,2,3},0}]},{b,[{{4,5,6},0}]},{c,[]},{m3,[]}])),
    {'EXIT', {badarg, _}} =
        (catch family_projection({external, fun(X) -> X end}, 
				 from_term([{1,[1]}]))),

    %% ordered set element
    eval(family_projection(fun(_) -> from_term(a, atom) end,
			   from_term([{1,[a]}])),
	 from_term([{1,a}])),
    ok.

family_difference(Conf) when is_list(Conf) ->
    E = empty_set(),
    EF = family([]),
    F9 = from_term([{b,[b,c]}]),
    F10 = from_term([{a,[b,c]}]),
    eval(family_difference(E, E), E),
    eval(family_difference(E, F10), from_term([], type(F10))),
    eval(family_difference(F10, E), F10),
    eval(family_difference(F9, F10), F9),
    eval(family_difference(F10, F10), family([{a,[]}])),
    F20 = from_term([{a,[1,2,3]},{b,[1,2,3]},{c,[1,2,3]}]),
    F21 = from_term([{b,[1,2,3]},{c,[1,2,3]}]),
    eval(family_difference(F20, from_term([{a,[2]}])),
	 from_term([{a,[1,3]},{b,[1,2,3]},{c,[1,2,3]}])),
    eval(family_difference(F20, from_term([{0,[2]},{q,[1,2]}])), F20),
    eval(family_difference(F20, F21),
	 from_term([{a,[1,2,3]},{b,[]},{c,[]}])),

    eval(family_difference(from_term([{e,[f,g]}]), family([])),
	 from_term([{e,[f,g]}])),
    eval(family_difference(from_term([{e,[f,g]}]), EF),
	 from_term([{e,[f,g]}])),
    eval(family_difference(from_term([{a,[a,b,c,d]},{c,[b,c]}]),
			   from_term([{a,[b,c]},{b,[d]},{d,[e,f]}])),
	 from_term([{a,[a,d]},{c,[b,c]}])),
    {'EXIT', {badarg, _}} =
	(catch family_difference(set([]), set([]))),
    {'EXIT', {type_mismatch, _}} =
	(catch family_difference(from_term([{a,[b,c]}]),
                                 from_term([{e,[{f}]}]))),
    {'EXIT', {type_mismatch, _}} =
	(catch family_difference(from_term([{a,[b]}]),
                                 from_term([{c,[d]}], [{i,[s]}]))),
    ok.

family_intersection_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    EF = family([]),
    ES = from_term([], [{atom,[[atom]]}]),
    eval(family_intersection(E), E),
    {'EXIT', {badarg, _}} = (catch family_intersection(EF)),
    eval(family_intersection(ES), EF),
    {'EXIT', {badarg, _}} = (catch family_intersection(set([]))),
    {'EXIT', {badarg, _}} =
        (catch family_intersection(from_term([{a,[1,2]}]))),
    F1 = from_term([{a,[[1],[2],[2,3]]},{b,[]},{c,[[4]]}]),
    {'EXIT', {badarg, _}} = (catch family_intersection(F1)),
    F2 = from_term([{b,[[1],[2],[2,3]]},{a,[]},{c,[[4]]}]),
    {'EXIT', {badarg, _}} = (catch family_intersection(F2)),
    F3 = from_term([{a,[[1,2,3],[2],[2,3]]},{c,[[4,5,6],[5,6,7]]}]),
    eval(family_intersection(F3), family([{a,[2]},{c,[5,6]}])),
    ok.

family_intersection_2(Conf) when is_list(Conf) ->
    E = empty_set(),
    EF = family([]),
    F1 = from_term([{a,[1,2]},{b,[4,5]},{c,[7,8]},{d,[10,11]}]),
    F2 = from_term([{c,[6,7]},{d,[9,10,11]},{q,[1]}]),
    F3 = from_term([{a,[1,2]},{b,[4,5]},{c,[6,7,8]},{d,[9,10,11]},
		    {q,[1]}]),

    eval(family_intersection(E, E), E),
    eval(family_intersection(EF, EF), EF),
    eval(family_intersection(F1, F2),
	 from_term([{c,[7]},{d,[10,11]}])),
    eval(family_intersection(F1, F3), F1),
    eval(family_intersection(F2, F3), F2),

    eval(family_intersection(EF, from_term([{e,[f,g]}])), EF),
    eval(family_intersection(E, from_term([{e,[f,g]}])), EF),
    eval(family_intersection(from_term([{e,[f,g]}]), EF), EF),
    eval(family_intersection(from_term([{e,[f,g]}]), E), EF),
    {'EXIT', {type_mismatch, _}} =
	(catch family_intersection(from_term([{a,[b,c]}]),
                                   from_term([{e,[{f}]}]))),

    F11 = family([{a,[1,2,3]},{b,[0,2,4]},{c,[0,3,6,9]}]),
    eval(union_of_family(F11), set([0,1,2,3,4,6,9])),
    F12 = from_term([{a,[1,2,3,4]},{b,[0,2,4]},{c,[2,3,4,5]}]),
    eval(intersection_of_family(F12), set([2,4])),
    ok.

family_union_1(Conf) when is_list(Conf) ->
    E = empty_set(),
    EF = family([]),
    ES = from_term([], [{atom,[[atom]]}]),
    eval(family_union(E), E),
    eval(family_union(ES), EF),
    {'EXIT', {badarg, _}} = (catch family_union(set([]))),
    {'EXIT', {badarg, _}} =
        (catch family_union(from_term([{a,[1,2]}]))),
    eval(family_union(from_term([{a,[[1],[2],[2,3]]},{b,[]},{c,[[4]]}])),
	 family([{a,[1,2,3]},{b,[]},{c,[4]}])),
    ok.

family_union_2(Conf) when is_list(Conf) ->
    E = empty_set(),
    EF = family([]),
    F1 = from_term([{a,[1,2]},{b,[4,5]},{c,[7,8]},{d,[10,11]}]),
    F2 = from_term([{c,[6,7]},{d,[9,10,11]},{q,[1]}]),
    F3 = from_term([{a,[1,2]},{b,[4,5]},{c,[6,7,8]},{d,[9,10,11]},
		    {q,[1]}]),

    eval(family_union(E, E), E),
    eval(family_union(F1, E), F1),
    eval(family_union(E, F2), F2),
    eval(family_union(F1, F2), F3),
    eval(family_union(F2, F1), F3),

    eval(family_union(E, from_term([{e,[f,g]}])),
	 from_term([{e,[f,g]}])),
    eval(family_union(EF, from_term([{e,[f,g]}])),
	 from_term([{e,[f,g]}])),
    eval(family_union(from_term([{e,[f,g]}]), E),
	 from_term([{e,[f,g]}])),
    {'EXIT', {badarg, _}} =
	(catch family_union(set([]),set([]))),
    {'EXIT', {type_mismatch, _}} =
	(catch family_union(from_term([{a,[b,c]}]), 
                            from_term([{e,[{f}]}]))),
    ok.

partition_family(Conf) when is_list(Conf) ->
    E = empty_set(),

    %% set of ordered sets
    ER = relation([]),
    EF = from_term([], [{atom,[{atom,atom}]}]),

    eval(partition_family(1, E), E),
    eval(partition_family(2, E), E),
    eval(partition_family(fun sofs:union/1, E), E),
    eval(partition_family(1, ER), EF),
    eval(partition_family(2, ER), EF),
    {'EXIT', {badarg, _}} = (catch partition_family(1, set([]))),
    {'EXIT', {badarg, _}} = (catch partition_family(2, set([]))),
    {'EXIT', {function_clause, _}} =
	(catch partition_family(fun({_A,B}) -> {B} end, from_term([{1}]))),
    eval(partition_family(1, relation([{1,a},{1,b},{2,c},{2,d}])),
	 from_term([{1,[{1,a},{1,b}]},{2,[{2,c},{2,d}]}])),
    eval(partition_family(1, relation([{1,a},{2,b}])),
	 from_term([{1,[{1,a}]},{2,[{2,b}]}])),
    eval(partition_family(2, relation([{1,a},{1,b},{2,a},{2,b},{3,c}])),
	 from_term([{a,[{1,a},{2,a}]},{b,[{1,b},{2,b}]},{c,[{3,c}]}])),
    eval(partition_family(2, relation([{1,a}])),
	 from_term([{a,[{1,a}]}])),
    eval(partition_family(2, relation([{1,a},{2,a},{3,a}])),
	 from_term([{a,[{1,a},{2,a},{3,a}]}])),
    eval(partition_family(2, relation([{1,a},{2,b}])),
	 from_term([{a,[{1,a}]},{b,[{2,b}]}])),
    F13 = from_term([{a,b,c},{a,b,d},{b,b,c},{a,c,c},{a,c,d},{b,c,c}]),
    eval(partition_family(2, F13),
	 from_term([{b,[{a,b,c},{a,b,d},{b,b,c}]},
		    {c,[{a,c,c},{a,c,d},{b,c,c}]}])),

    Fun1 = {external, fun({A,_B}) -> {A} end}, 
    eval(partition_family(Fun1, relation([{a,1},{a,2},{b,3}])),
	 from_term([{{a},[{a,1},{a,2}]},{{b},[{b,3}]}])),
    Fun2 = fun(S) -> {A,_B} = to_external(S), from_term({A}) end,
    eval(partition_family(Fun2, relation([{a,1},{a,2},{b,3}])),
	 from_term([{{a},[{a,1},{a,2}]},{{b},[{b,3}]}])),

    {'EXIT', {badarg, _}} =
	(catch partition_family({external, fun({A,_}) -> {A,0} end}, 
				from_term([{1,a}]))),
    [{{atom,atom},[{atom,atom,atom,atom}]}] =
	type(partition_family({external, fun({A,_B,C,_D}) -> {C,A} end}, 
			      relation([],4))),

    Fun3 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    eval(partition_family(Fun3, E), E),
    eval(partition_family(Fun3, set([a,b])),
	 from_term([{{a,0},[a]}, {{b,0},[b]}])),
    eval(partition_family(Fun3, relation([{a,1},{b,2}])),
	 from_term([{{{a,1},0},[{a,1}]},{{{b,2},0},[{b,2}]}])),
    eval(partition_family(Fun3, from_term([[a],[b]])),
	 from_term([{{[a],0},[[a]]}, {{[b],0},[[b]]}])),
    partition_family({external, fun(X) -> X end}, E),

    F = 0.0, I = round(F),
    FR = relation([{I,a},{F,b}]),
    if
        F == I -> % term ordering
            true = (1 =:= no_elements(partition_family(1, FR)));
        true -> 
            eval(partition_family(1, FR),
		 from_term([{I,[{I,a}]},{F,[{F,b}]}]))
    end,
    %% set of sets
    {'EXIT', {badarg, _}} =
        (catch partition_family({external, fun(X) -> X end}, 
				from_term([], [[atom]]))),
    {'EXIT', {badarg, _}} =
        (catch partition_family({external, fun(X) -> X end}, 
				from_term([[a]]))),
    eval(partition_family(fun sofs:union/1,
			  from_term([[[1],[1,2]], [[1,2]]])),
	 from_term([{[1,2], [[[1],[1,2]],[[1,2]]]}])),
    eval(partition_family(fun(X) -> X end,
			  from_term([[1],[1,2],[1,2,3]])),
	 from_term([{[1],[[1]]},{[1,2],[[1,2]]},{[1,2,3],[[1,2,3]]}])),

    eval(partition_family(fun(_) -> from_term([a]) end,
			  from_term([], [[atom]])),
	 E),
    Fun10 = fun(S) ->
		    %% Cheating a lot...
		    case to_external(S) of
			[1] -> from_term({1,1});
			_ -> S
		    end
	    end,

    eval(partition_family(Fun10, from_term([[1]])),
	 from_term([{{1,1},[[1]]}])),
    eval(partition_family(fun(_) -> from_term({a}) end,
			  from_term([[a]])),
	 from_term([{{a},[[a]]}])),
    {'EXIT', {badarg, _}} =
	(catch partition_family(fun(_) -> {a} end, from_term([[a]]))),
    ok.

%% Not meant to be efficient...
local_adjoin(S, C) ->
    X = to_external(C),
    T = type(C),
    F = fun(Y) -> from_term({to_external(Y),X}, {type(Y),T}) end,
    projection(F, S).

eval(R, E) when R == E ->
    R;
eval(R, E) ->
    io:format("expected ~p~n got ~p~n", [E, R]),
    exit({R,E}).

