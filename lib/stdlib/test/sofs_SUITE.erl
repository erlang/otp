%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2012. All Rights Reserved.
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
-module(sofs_SUITE).

%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(t, test_server).
-else.
-include_lib("test_server/include/test_server.hrl").
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

suite() -> [{ct_hooks,[ts_install_cth]}].

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
    Dog=?t:timetrap(?t:minutes(2)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%% [{2,b},{1,a,b}] == lists:sort([{2,b},{1,a,b}])
%% [{1,a,b},{2,b}] == lists:keysort(1,[{2,b},{1,a,b}])


from_term_1(suite) -> [];
from_term_1(doc) -> [""];
from_term_1(Conf) when is_list(Conf) ->
    %% would go wrong: projection(1,from_term([{2,b},{1,a,b}])),

    ?line {'EXIT', {badarg, _}} = (catch from_term([], {atom,'_',atom})),
    ?line {'EXIT', {badarg, _}} = (catch from_term([], [])),
    ?line {'EXIT', {badarg, _}} = (catch from_term([], [atom,atom])),

    ?line [] = to_external(from_term([])),
    ?line eval(from_term([]), empty_set()),
    ?line [] = to_external(from_term([], ['_'])),
    ?line eval(from_term([], ['_']), empty_set()),
    ?line [[]] = to_external(from_term([[]])),
    ?line [[['_']]] = type(from_term([[],[[]]])),
    ?line [[],[[]]] = to_external(from_term([[],[[]]])),
    ?line [[['_']]] = type(from_term([[],[[]]])),
    ?line eval(from_term([a],['_']), set([a])),
    ?line [[],[a]] = to_external(from_term([[],[a]])),
    ?line [[],[{a}]] = to_external(from_term([[{a}],[]])),
    ?line [{[],[{a,b,[d]}]},{[{a,b}],[]}] =
	to_external(from_term([{[],[{a,b,[d]}]},{[{a,b}],[]}])),

    ?line [{[a,b],[c,d]}] = to_external(from_term([{[a,b],[c,d]}])),
    ?line [{{a,b},[a,b],{{a},{b}}}] =
	to_external(from_term([{{a,b},[a,b],{{a},{b}}}])),
    ?line [{{a,{[a,b]},a}},{{z,{[y,z]},z}}] =
	to_external(from_term([{{a,{[a,b,a]},a}},{{z,{[y,y,z]},z}}])),
    ?line {'EXIT', {badarg, _}} = 
	(catch from_term([{m1,[{m1,f1,1},{m1,f2,2}]},{m2,[]},{m3,[a]}])),
    ?line MS1 = [{m1,[{m1,f1,1},{m1,f2,2}]},{m2,[]},{m3,[{m3,f3,3}]}],
    ?line eval(to_external(from_term(MS1)), MS1),

    ?line eval(to_external(from_term(a)), a),
    ?line eval(to_external(from_term({a})), {a}),

    ?line eval(to_external(from_term([[a],[{b,c}]],[[atomic]])),
	       [[a],[{b,c}]]),
    ?line eval(type(from_term([[a],[{b,c}]],[[atomic]])),
	       [[atomic]]),

    ?line {'EXIT', {badarg, _}} = (catch from_term([[],[],a])),
    ?line {'EXIT', {badarg, _}} = (catch from_term([{[a,b],[c,{d}]}])),
    ?line {'EXIT', {badarg, _}} = (catch from_term([[],[a],[{a}]])),
    ?line {'EXIT', {badarg, _}} = (catch from_term([a,{a,b}])),
    ?line {'EXIT', {badarg, _}} = (catch from_term([[a],[{b,c}]],[['_']])),
    ?line {'EXIT', {badarg, _}} = (catch from_term([a | {a,b}])),
    ?line {'EXIT', {badarg, _}} = 
	(catch from_term([{{a},b,c},{d,e,f}],[{{atom},atom,atom}])),
    ?line {'EXIT', {badarg, _}} = 
	(catch from_term([{a,{b,c}} | tail], [{atom,{atom,atom}}])),
    ?line {'EXIT', {badarg, _}} = (catch from_term({})),
    ?line {'EXIT', {badarg, _}} = (catch from_term([{}])),

    ?line [{foo,bar},[b,a]] = 
        to_external(from_term([[b,a],{foo,bar},[b,a]], [atom])),
    ?line [{[atom],{atom,atom}}] = 
	type(from_term([{[], {a,b}},{[a,b],{e,f}}])),
    ?line [{[atom],{atom,atom}}] = 
	type(from_term([{[], {a,b}},{[a,b],{e,f}}], [{[atom],{atom,atom}}])),
    ?line [[atom]] = type(from_term([[a],[{b,c}]],[[atom]])),

    ?line {atom, atom} = type(from_term({a,b}, {atom, atom})),
    ?line atom = type(from_term(a, atom)),
    ?line {'EXIT', {badarg, _}} = (catch from_term({a,b},{atom})),
    ?line [{{a},b,c},{{d},e,f}] = 
	to_external(from_term([{{a},b,c},{{a},b,c},{{d},e,f}],
			      [{{atom},atom,atom}])),

    %% from_external too...
    ?line e = to_external(from_external(e, atom)),
    ?line {e} = to_external(from_external({e}, {atom})),
    ?line [e] = to_external(from_external([e], [atom])),

    %% and is_type...
    ?line true = is_type(['_']),
    ?line false = is_type('_'),
    ?line true = is_type([['_']]),
    ?line false = is_type({atom,[],atom}),
    ?line false = is_type({atom,'_',atom}),
    ?line true = is_type({atom,atomic,atom}),
    ?line true = is_type({atom,atom}),
    ?line true = is_type(atom),
    ?line true = is_type([atom]),
    ?line true = is_type(type),

    ok.

set_1(suite) -> [];
set_1(doc) -> [""];
set_1(Conf) when is_list(Conf) ->
    %% set/1
    ?line {'EXIT', {badarg, _}} = (catch set(a)),
    ?line {'EXIT', {badarg, _}} = (catch set({a})),
    ?line eval(set([]), from_term([],[atom])),
    ?line eval(set([a,b,c]), from_term([a,b,c])),
    ?line eval(set([a,b,a,a,b]), from_term([a,b])),
    ?line eval(set([a,b,c,a,d,d,c,1]), from_term([1,a,b,c,d])),
    ?line eval(set([a,b,d,a,c]), from_term([a,b,c,d])),
    ?line eval(set([f,e,d,c,d]), from_term([c,d,e,f])),
    ?line eval(set([h,f,d,g,g,d,c]), from_term([c,d,f,g,h])),
    ?line eval(set([h,e,d,k,l]), from_term([d,e,h,k,l])),
    ?line eval(set([h,e,c,k,d]), from_term([c,d,e,h,k])),

    %% set/2
    ?line {'EXIT', {badarg, _}} = (catch set(a, [a])),
    ?line {'EXIT', {badarg, _}} = (catch set({a}, [a])),
    ?line {'EXIT', {badarg, _}} = (catch set([a], {a})),
    ?line {'EXIT', {badarg, _}} = (catch set([a], a)),
    ?line {'EXIT', {badarg, _}} = (catch set([a], [a,b])),
    ?line {'EXIT', {badarg, _}} = (catch set([a | b],[foo])),
    ?line {'EXIT', {badarg, _}} = (catch set([a | b],['_'])),
    ?line {'EXIT', {badarg, _}} = (catch set([a | b],[[atom]])),
    ?line {'EXIT', {badarg, _}} = (catch set([{}],[{}])),
    ?line eval(set([a],['_']), from_term([a],['_'])),
    ?line eval(set([], ['_']), empty_set()),
    ?line eval(set([a,b,a,b],[foo]), from_term([a,b],[foo])),

    ok.

from_sets_1(suite) -> [];
from_sets_1(doc) -> [""];
from_sets_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),

    %% unordered
    ?line eval(from_sets([]), E),
    ?line {'EXIT', {type_mismatch, _}} = 
	(catch from_sets([from_term([{a,b}]), 
                          E,
                          from_term([{a,b,c}])])),
    ?line eval(from_sets([from_term([{a,b}]), E]), 
               from_term([[],[{a,b}]])),

    ?line eval(from_sets([from_term({a,b},{atom,atom}), 
                          from_term({b,c},{atom,atom})]),
	       relation([{a,b}, {b,c}])),
    ?line {'EXIT', {type_mismatch, _}} = 
	(catch from_sets([from_term({a,b},{atom,atom}), 
			  from_term({a,b,c},{atom,atom,atom})])),
    ?line {'EXIT', {badarg, _}} = (catch from_sets(foo)),
    ?line eval(from_sets([E]), from_term([[]])),
    ?line eval(from_sets([E,E]), from_term([[]])),
    ?line eval(from_sets([E,set([a])]), from_term([[],[a]])),
    ?line {'EXIT', {badarg, _}} = (catch from_sets([E,{a}])),
    ?line {'EXIT', {type_mismatch, _}} = 
	(catch from_sets([E,from_term({a}),E])),
    ?line {'EXIT', {type_mismatch, _}} = (catch from_sets([from_term({a}),E])),

    %% ordered
    ?line O = {from_term(a,atom), from_term({b}, {atom}), set([c,d])},
    ?line eval(from_sets(O), from_term({a,{b},[c,d]}, {atom,{atom},[atom]})),
    ?line {'EXIT', {badarg, _}} = (catch from_sets([a,b])),
    ?line {'EXIT', {badarg, _}} = (catch from_sets({a,b})),
    ?line eval(from_sets({from_term({a}),E}), from_term({{a},[]})),
    ok.

relation_1(suite) -> [];
relation_1(doc) -> [""];
relation_1(Conf) when is_list(Conf) ->
    %% relation/1
    ?line eval(relation([]), from_term([], [{atom,atom}])),
    ?line eval(from_term([{a}]), relation([{a}])),
    ?line {'EXIT', {badarg, _}} = (catch relation(a)),
    ?line {'EXIT', {badarg, _}} = (catch relation([{a} | a])),
    ?line {'EXIT', {badarg, _}} = (catch relation([{}])),
    ?line {'EXIT', {badarg, _}} = (catch relation([],0)),
    ?line {'EXIT', {badarg, _}} = (catch relation([{a}],a)),

    %% relation/2
    ?line eval(relation([{a},{b}], 1), from_term([{a},{b}])),
    ?line eval(relation([{1,a},{2,b},{1,a}], [{x,y}]),
	       from_term([{1,a},{2,b}], [{x,y}])),
    ?line eval(relation([{[1,2],a},{[2,1],b},{[2,1],a}], [{[x],y}]),
	       from_term([{[1,2],a},{[1,2],b}], [{[x],y}])),
    ?line {'EXIT', {badarg, _}} = (catch relation([{1,a},{2,b}], [{[x],y}])),
    ?line {'EXIT', {badarg, _}} = (catch relation([{1,a},{1,a,b}], [{x,y}])),
    ?line {'EXIT', {badarg, _}} = (catch relation([{a}], 2)),
    ?line {'EXIT', {badarg, _}} = (catch relation([{a},{b},{c,d}], 1)),
    ?line eval(relation([{{a},[{foo,bar}]}], ['_']), 
	       from_term([{{a},[{foo,bar}]}], ['_'])),
    ?line eval(relation([], ['_']), from_term([], ['_'])),
    ?line {'EXIT', {badarg, _}} = (catch relation([[a]],['_'])),
    ?line eval(relation([{[a,b,a]}], [{[atom]}]), from_term([{[a,b,a]}])),
    ?line eval(relation([{[a,b,a],[[d,e,d]]}], [{[atom],[[atom]]}]), 
	       from_term([{[a,b,a],[[d,e,d]]}])),
    ?line eval(relation([{[a,b,a],[[d,e,d]]}], [{atom,[[atom]]}]), 
	       from_term([{[a,b,a],[[d,e,d]]}], [{atom,[[atom]]}])),
    ok.

a_function_1(suite) -> [];
a_function_1(doc) -> [""];
a_function_1(Conf) when is_list(Conf) ->
    %% a_function/1
    ?line eval(a_function([]), from_term([], [{atom,atom}])),
    ?line eval(a_function([{a,b},{a,b},{b,c}]), from_term([{a,b},{b,c}])),
    ?line {'EXIT', {badarg, _}} = (catch a_function([{a}])),
    ?line {'EXIT', {badarg, _}} = (catch a_function([{a},{b},{c,d}])),
    ?line {'EXIT', {badarg, _}} = (catch a_function(a)),
    ?line {'EXIT', {badarg, _}} = (catch a_function([{a,b} | a])),
    ?line {'EXIT', {bad_function, _}} = 
	(catch a_function([{a,b},{b,c},{a,c}])),
    F = 0.0, I = round(F),
    if
        F == I -> % term ordering
            ?line {'EXIT', {bad_function, _}} = 
                (catch a_function([{I,a},{F,b}])),
            ?line {'EXIT', {bad_function, _}} = 
                 (catch a_function([{[I],a},{[F],b}],[{[a],b}]));
        true -> 
            ?line 2 = no_elements(a_function([{I,a},{F,b}])),
            ?line 2 = no_elements(a_function([{[I],a},{[F],b}],[{[a],b}]))
    end,

    %% a_function/2
    FT = [{atom,atom}],
    ?line eval(a_function([], FT), from_term([], FT)),
    ?line eval(a_function([{a,b},{b,c},{b,c}], FT), 
	       from_term([{a,b},{b,c}], FT)),
    ?line {'EXIT', {badarg, _}} = (catch a_function([{a,b}], [{a}])),
    ?line {'EXIT', {badarg, _}} = (catch a_function([{a,b}], [{a,[b,c]}])),
    ?line {'EXIT', {badarg, _}} = (catch a_function([{a}], FT)),
    ?line {'EXIT', {badarg, _}} = (catch a_function([{a},{b},{c,d}], FT)),
    ?line {'EXIT', {badarg, _}} = (catch a_function(a, FT)),
    ?line {'EXIT', {badarg, _}} = (catch a_function([{a,b} | a], FT)),
    ?line eval(a_function([{{a},[{foo,bar}]}], ['_']),
	       from_term([{{a},[{foo,bar}]}], ['_'])),
    ?line eval(a_function([], ['_']), from_term([], ['_'])),
    ?line {'EXIT', {badarg, _}} = (catch a_function([[a]],['_'])),
    ?line {'EXIT', {bad_function, _}} = 
	(catch a_function([{a,b},{b,c},{a,c}], FT)),
    ?line eval(a_function([{a,[a]},{a,[a,a]}], [{atom,[atom]}]),
	       from_term([{a,[a]}])),
    ?line eval(a_function([{[b,a],c},{[a,b],c}], [{[atom],atom}]),
	       from_term([{[a,b],c}])),
    ok.

family_1(suite) -> [];
family_1(doc) -> [""];
family_1(Conf) when is_list(Conf) ->
    %% family/1
    ?line eval(family([]), from_term([],[{atom,[atom]}])),
    ?line {'EXIT', {badarg, _}} = (catch family(a)),
    ?line {'EXIT', {badarg, _}} = (catch family([a])),
    ?line {'EXIT', {badarg, _}} = (catch family([{a,b}])),
    ?line {'EXIT', {badarg, _}} = (catch family([{a,[]} | a])),
    ?line {'EXIT', {badarg, _}} = (catch family([{a,[a|b]}])),
    ?line {'EXIT', {bad_function, _}} = 
        (catch family([{a,[a]},{a,[]}])),
    ?line {'EXIT', {bad_function, _}} = 
	(catch family([{a,[]},{b,[]},{a,[a]}])),
    F = 0.0, I = round(F),
    if
        F == I -> % term ordering
            ?line {'EXIT', {bad_function, _}} = 
                (catch family([{I,[a]},{F,[b]}])),
            ?line true = (1 =:= no_elements(family([{a,[I]},{a,[F]}])));
        true -> 
            ?line {'EXIT', {bad_function, _}} = 
                (catch family([{a,[I]},{a,[F]}]))
    end,
    ?line eval(family([{a,[]},{b,[b]},{a,[]}]), from_term([{a,[]},{b,[b]}])),
    ?line eval(to_external(family([{b,[{hej,san},tjo]},{a,[]}])),
               [{a,[]},{b,[tjo,{hej,san}]}]),
    ?line eval(family([{a,[a]},{a,[a,a]}]), family([{a,[a]}])),

    %% family/2
    FT = [{a,[a]}],
    ?line eval(family([], FT), from_term([],FT)),
    ?line {'EXIT', {badarg, _}} = (catch family(a,FT)),
    ?line {'EXIT', {badarg, _}} = (catch family([a],FT)),
    ?line {'EXIT', {badarg, _}} = (catch family([{a,b}],FT)),
    ?line {'EXIT', {badarg, _}} = (catch family([{a,[]} | a],FT)),
    ?line {'EXIT', {badarg, _}} = (catch family([{a,[a|b]}], FT)),
    ?line {'EXIT', {bad_function, _}} = 
        (catch family([{a,[a]},{a,[]}], FT)),
    ?line {'EXIT', {bad_function, _}} = 
	(catch family([{a,[]},{b,[]},{a,[a]}], FT)),
    ?line eval(family([{a,[]},{b,[b,b]},{a,[]}], FT), 
	       from_term([{a,[]},{b,[b]}], FT)),
    ?line eval(to_external(family([{b,[{hej,san},tjo]},{a,[]}], FT)),
               [{a,[]},{b,[tjo,{hej,san}]}]),

    ?line eval(family([{{a},[{foo,bar}]}], ['_']),
	       from_term([{{a},[{foo,bar}]}], ['_'])),
    ?line eval(family([], ['_']), from_term([], ['_'])),
    ?line {'EXIT', {badarg, _}} = (catch family([[a]],['_'])),
    ?line {'EXIT', {badarg, _}} = (catch family([{a,b}],['_'])),
    ?line {'EXIT', {badarg, _}} = 
	(catch family([{a,[foo]}], [{atom,atom}])),
    ?line eval(family([{{a},[{foo,bar}]}], [{{dt},[{r1,t2}]}]),
	       from_term([{{a},[{foo,bar}]}], [{{dt},[{r1,t2}]}])),
    ?line eval(family([{a,[a]},{a,[a,a]}],[{atom,[atom]}]),
	       family([{a,[a]}])),
    ?line eval(family([{[a,b],[a]},{[b,a],[a,a]}],[{[atom],[atom]}]),
	       from_term([{[a,b],[a]},{[b,a],[a,a]}])),
    ok.

projection(suite) -> [];
projection(doc) -> [""];
projection(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),

    %% set of ordered sets
    ?line S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    ?line S2 = relation([{a,1},{a,2},{a,3},{b,4},{b,5},{b,6}]),

    ?line eval(projection(1, E), E),
    ?line eval(projection(1, ER), set([])),
    ?line eval(projection(1, relation([{a,1}])), set([a])),
    ?line eval(projection(1, S1), set([a,b,c])),
    ?line eval(projection(1, S2), set([a,b])),
    ?line eval(projection(2, S1), set([0,1,2,22])),
    ?line eval(projection(2, relation([{1,a},{2,a},{3,b}])), set([a,b])),
    ?line eval(projection(1, relation([{a},{b},{c}])), set([a,b,c])),

    Fun1 = {external, fun({A,B,C}) -> {A,{B,C}} end},
    ?line eval(projection(Fun1, E), E),
    %% No check here:
    ?line eval(projection(3, projection(Fun1, empty_set())), E),
    ?line E2 = relation([], 3),
    ?line eval(projection(Fun1, E2), from_term([], [{atom,{atom,atom}}])),

    Fun2 = {external, fun({A,_B}) -> {A} end},
    ?line eval(projection(Fun2, ER), from_term([], [{atom}])),
    ?line eval(projection(Fun2, relation([{a,1}])), relation([{a}])),
    ?line eval(projection(Fun2, relation([{a,1},{b,3},{a,2}])), 
               relation([{a},{b}])),
    Fun3 = {external, fun({A,_B,C}) -> {C,{A},C} end}, 
    ?line eval(projection(Fun3, relation([{a,1,x},{b,3,y},{a,2,z}])),
               from_term([{x,{a},x},{y,{b},y},{z,{a},z}])),
    Fun4 = {external, fun(A={B,_C,_D}) -> {B, A} end},
    ?line eval(projection(Fun4, relation([{a,1,x},{b,3,y},{a,2,z}])),
               from_term([{a,{a,1,x}},{b,{b,3,y}},{a,{a,2,z}}])),

    ?line eval(projection({external, fun({A,B,_C,D}) -> {A,B,A,D} end},
			  relation([{1,1,1,2}, {1,1,3,1}])),
               relation([{1,1,1,1}, {1,1,1,2}])),

    ?line {'EXIT', {badarg, _}} = (catch projection(1, set([]))),
    ?line {'EXIT', {function_clause, _}} = 
	(catch projection({external, fun({A}) -> A end}, S1)),
    ?line {'EXIT', {badarg, _}} = 
	(catch projection({external, fun({A,_}) -> {A,0} end}, 
			  from_term([{1,a}]))),

    %% {} is not an ordered set
    ?line {'EXIT', {badarg, _}} = 
        (catch projection({external, fun(_) -> {} end}, ER)),
    ?line {'EXIT', {badarg, _}} = 
        (catch projection({external, fun(_) -> {{}} end}, ER)),
    ?line eval(projection({external, fun({T,_}) -> T end}, 
			  relation([{{},a},{{},b}])),
	       set([{}])),
    ?line eval(projection({external, fun({T}) -> T end}, relation([{{}}])),
	       set([{}])),

    ?line eval(projection({external, fun(A) -> {A} end}, 
			  relation([{1,a},{2,b}])), 
	       from_term([{{1,a}},{{2,b}}])),
    ?line eval(projection({external, fun({A,B}) -> {B,A} end}, 
			  relation([{1,a},{2,b}])),
	       relation([{a,1},{b,2}])),
    ?line eval(projection({external, fun(X=Y=A) -> {X,Y,A} end}, set([a,b,c])),
	       relation([{a,a,a},{b,b,b},{c,c,c}])),

    ?line eval(projection({external, fun({A,{_},B}) -> {A,B} end}, 
			  from_term([{a,{a},b},{a,{b},c}])),
               relation([{a,b},{a,c}])),
    ?line eval(projection({external, fun({A,_,B}) -> {A,B} end},
			  relation([{a,{},b},{a,{},c}])),
               relation([{a,b},{a,c}])),
    Fun5 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    ?line eval(projection(Fun5, E), E),
    ?line eval(projection(Fun5, set([a,b])), from_term([{a,0},{b,0}])),
    ?line eval(projection(Fun5, relation([{a,1},{b,2}])),
	       from_term([{{a,1},0},{{b,2},0}])),
    ?line eval(projection(Fun5, from_term([[a],[b]])),
	       from_term([{[a],0},{[b],0}])),

    F = 0.0, I = round(F),
    ?line FR = relation([{I},{F}]),
    if
        F == I -> % term ordering
            true = (no_elements(projection(1, FR)) =:= 1);
        true -> 
            eval(projection(1, FR), set([I,F]))
    end,

    %% set of sets
    ?line {'EXIT', {badarg, _}} = 
        (catch projection({external, fun(X) -> X end}, 
			  from_term([], [[atom]]))),
    ?line {'EXIT', {badarg, _}} = 
        (catch projection({external, fun(X) -> X end}, from_term([[a]]))),
    ?line eval(projection(fun sofs:union/1,
			  from_term([[[1,2],[2,3]], [[a,b],[b,c]]])),
               from_term([[1,2,3], [a,b,c]])),
    ?line eval(projection(fun(_) -> from_term([a]) end, 
			  from_term([[b]], [[a]])),
               from_term([[a]])),
    ?line eval(projection(fun(_) -> from_term([a]) end, 
			  from_term([[1,2],[3,4]])),
               from_term([[a]])),
    Fun10 = fun(S) ->
                   %% Cheating a lot...
                   case to_external(S) of
                       [1] -> from_term({1,1});
                       _ -> S
                   end
           end,
    ?line eval(projection(Fun10, from_term([[1]])), from_term([{1,1}])),
    ?line eval(projection(fun(_) -> from_term({a}) end, from_term([[a]])),
	       from_term([{a}])),
    ?line {'EXIT', {badarg, _}} = 
	(catch projection(fun(_) -> {a} end, from_term([[a]]))),

    ok.

substitution(suite) -> [];
substitution(doc) -> [""];
substitution(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),

    %% set of ordered sets
    ?line S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    ?line S2 = relation([{a,1},{a,2},{a,3},{b,4},{b,5},{b,6}]),

    ?line eval(substitution(1, E), E),
    %% No check here:
    Fun0 = {external, fun({A,B,C}) -> {A,{B,C}} end},
    ?line eval(substitution(3, substitution(Fun0, empty_set())), E),
    ?line eval(substitution(1, ER), from_term([],[{{atom,atom},atom}])),
    ?line eval(substitution(1, relation([{a,1}])), from_term([{{a,1},a}])),
    ?line eval(substitution(1, S1), 
               from_term([{{a,1},a},{{b,2},b},{{b,22},b},{{c,0},c}])),
    ?line eval(substitution(1, S2), 
               from_term([{{a,1},a},{{a,2},a},{{a,3},a},{{b,4},b},
                          {{b,5},b},{{b,6},b}])),
    ?line eval(substitution(2, S1), 
               from_term([{{a,1},1},{{b,2},2},{{b,22},22},{{c,0},0}])),
    
    Fun1 = fun({A,_B}) -> {A} end, 
    XFun1 = {external, Fun1},
    ?line eval(substitution(XFun1, E), E),
    ?line eval(substitution(Fun1, E), E),
    ?line eval(substitution(XFun1, ER), from_term([], [{{atom,atom},{atom}}])),
    ?line eval(substitution(XFun1, relation([{a,1}])), 
	       from_term([{{a,1},{a}}])),
    ?line eval(substitution(XFun1, relation([{a,1},{b,3},{a,2}])), 
               from_term([{{a,1},{a}},{{a,2},{a}},{{b,3},{b}}])),
    ?line eval(substitution({external, fun({A,_B,C}) -> {C,A,C} end}, 
			    relation([{a,1,x},{b,3,y},{a,2,z}])),
               from_term([{{a,1,x},{x,a,x}},{{a,2,z},{z,a,z}},
                          {{b,3,y},{y,b,y}}])),
    Fun2 = fun(S) -> {A,_B} = to_external(S), from_term({A}) end,
    ?line eval(substitution(Fun2, ER), E),
    ?line eval(substitution(Fun2, relation([{a,1}])), 
	       from_term([{{a,1},{a}}])),
    Fun3 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    ?line eval(substitution(Fun3, E), E),
    ?line eval(substitution(Fun3, set([a,b])),
	       from_term([{a,{a,0}},{b,{b,0}}])),
    ?line eval(substitution(Fun3, relation([{a,1},{b,2}])),
	       from_term([{{a,1},{{a,1},0}},{{b,2},{{b,2},0}}])),
    ?line eval(substitution(Fun3, from_term([[a],[b]])),
	       from_term([{[a],{[a],0}},{[b],{[b],0}}])),
    
    ?line eval(substitution(fun(_) -> E end, from_term([[a],[b]])),
	       from_term([{[a],[]},{[b],[]}])),

    ?line {'EXIT', {badarg, _}} = (catch substitution(1, set([]))),
    ?line eval(substitution(1, ER), from_term([], [{{atom,atom},atom}])),
    ?line {'EXIT', {function_clause, _}} = 
	(catch substitution({external, fun({A,_}) -> A end}, set([]))),
    ?line {'EXIT', {badarg, _}} = 
	(catch substitution({external, fun({A,_}) -> {A,0} end}, 
			    from_term([{1,a}]))),

    %% set of sets
    ?line {'EXIT', {badarg, _}} = 
        (catch substitution({external, fun(X) -> X end}, 
			    from_term([], [[atom]]))),
    ?line {'EXIT', {badarg, _}} = 
        (catch substitution({external, fun(X) -> X end}, from_term([[a]]))),
    ?line eval(substitution(fun(X) -> X end, from_term([], [[atom]])), E),
    ?line eval(substitution(fun sofs:union/1,
                            from_term([[[1,2],[2,3]], [[a,b],[b,c]]])),
               from_term([{[[1,2],[2,3]],[1,2,3]}, {[[a,b],[b,c]],[a,b,c]}])),
    ?line eval(substitution(fun(_) -> from_term([a]) end, 
			    from_term([[b]], [[a]])),
               from_term([{[b],[a]}], [{[a],[atom]}])),
    ?line eval(substitution(fun(_) -> from_term([a]) end, 
			    from_term([[1,2],[3,4]])),
               from_term([{[1,2],[a]},{[3,4],[a]}])),
    Fun10 = fun(S) ->
                   %% Cheating a lot...
                   case to_external(S) of
                       [1] -> from_term({1,1});
                       _ -> S
                   end
           end,
    ?line eval(substitution(Fun10, from_term([[1]])), 
	       from_term([{[1],{1,1}}])),
    ?line {'EXIT', {type_mismatch, _}} =
        (catch substitution(Fun10, from_term([[1],[2]]))),
    ?line {'EXIT', {type_mismatch, _}} =
        (catch substitution(Fun10, from_term([[1],[0]]))),

    ?line eval(substitution(fun(_) -> from_term({a}) end, from_term([[a]])),
	       from_term([{[a],{a}}])),
    ?line {'EXIT', {badarg, _}} = 
	(catch substitution(fun(_) -> {a} end, from_term([[a]]))),

    ok.

restriction(suite) -> [];
restriction(doc) -> [""];
restriction(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([], 2),

    %% set of ordered sets
    ?line S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    ?line eval(restriction(S1, set([a,b])), 
               relation([{a,1},{b,2},{b,22}])),
    ?line eval(restriction(2, S1, set([1,2])),  
               relation([{a,1},{b,2}])),
    ?line eval(restriction(S1, set([a,b,c])), S1),
    ?line eval(restriction(1, S1, set([0,1,d,e])), ER),
    ?line eval(restriction(1, S1, E), ER),
    ?line eval(restriction({external, fun({_A,B,C}) -> {B,C} end}, 
                           relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
                           relation([{bb,2},{cc,3}])),
               relation([{b,bb,2},{c,cc,3}])),
    R1 = relation([],[{a,b}]),
    ?line eval(restriction(2, R1,sofs:set([],[b])), R1),
    Id = fun(X) -> X end,
    XId = {external, Id},
    ?line eval(restriction(XId, relation([{a,b}]), E), ER),
    ?line eval(restriction(XId, E, relation([{b,d}])), E),
    Fun1 = fun(S) -> {_A,B,C} = to_external(S), from_term({B,C}) end,
    ?line eval(restriction(Fun1,
                           relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
                           relation([{bb,2},{cc,3}])),
               relation([{b,bb,2},{c,cc,3}])),
    ?line eval(restriction({external, fun({_,{A},B}) -> {A,B} end},
                           from_term([{a,{aa},1},{b,{bb},2},{c,{cc},3}]), 
                           from_term([{bb,2},{cc,3}])),
               from_term([{b,{bb},2},{c,{cc},3}])),
    S5 = relation([{1,a},{2,b},{3,c}]),
    ?line eval(restriction(2, S5, set([b,c])), relation([{2,b},{3,c}])),
    S4 = relation([{a,1},{b,2},{b,27},{c,0}]),
    ?line eval(restriction(2, S4, E), ER),
    S6 = relation([{1,a},{2,c},{3,b}]),
    ?line eval(restriction(2, S6, set([d,e])), ER),
    ?line eval(restriction(2, 
			   relation([{1,d},{2,c},{3,b},{4,a},{5,e}]), 
			   set([c])),
	       relation([{2,c}])),
    ?line eval(restriction(XId, 
			   relation([{1,a},{3,b},{4,c},{4,d}]), 
			   relation([{2,a},{2,c},{4,c}])),
	       relation([{4,c}])),
    ?line eval(restriction(2, relation([{a,b}]), E), ER),
    ?line eval(restriction(2, E, relation([{b,d}])), E),
    ?line eval(restriction(2, relation([{b,d}]), E), ER),
    ?line eval(restriction(XId, E, set([a])), E),
    ?line eval(restriction(1, S1, E), ER),
    ?line {'EXIT', {badarg, _}} =
	(catch restriction(3, relation([{a,b}]), E)),
    ?line {'EXIT', {badarg, _}} =
	(catch restriction(3, relation([{a,b}]), relation([{b,d}]))),
    ?line {'EXIT', {badarg, _}} =
	(catch restriction(3, relation([{a,b}]), set([{b,d}]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch restriction(2, relation([{a,b}]), relation([{b,d}]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch restriction({external, fun({A,_B}) -> A end}, 
			   relation([{a,b}]), relation([{b,d}]))),
    ?line {'EXIT', {badarg, _}} = 
	(catch restriction({external, fun({A,_}) -> {A,0} end}, 
			   from_term([{1,a}]),
			   from_term([{1,0}]))),
    ?line eval(restriction(2, relation([{a,d},{b,e},{c,b},{d,c}]), set([b,d])),
	       relation([{a,d},{c,b}])),
    ?line {'EXIT', {function_clause, _}} =
	(catch restriction({external, fun({A,_B}) -> A end}, set([]), E)),

    Fun3 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    ?line eval(restriction(Fun3, set([1,2]), from_term([{1,0}])),
	       from_term([1])),

    %% set of sets
    ?line {'EXIT', {badarg, _}} = 
        (catch restriction({external, fun(X) -> X end}, 
			   from_term([], [[atom]]), set([a]))),
    S2 = from_term([], [[atom]]),
    ?line eval(restriction(Id, S2, E), E),
    S3 = from_term([[a],[b]], [[atom]]),
    ?line eval(restriction(Id, S3, E), E),
    ?line eval(restriction(Id, from_term([], [[atom]]), set([a])),
	       from_term([], [[atom]])),
    ?line eval(restriction(fun sofs:union/1,
                           from_term([[[a],[b]], [[b],[c]], 
                                      [[], [a,b]], [[1],[2]]]), 
                           from_term([[a,b],[1,2,3],[b,c]])),
               from_term([[[],[a,b]], [[a],[b]],[[b],[c]]])),
    ?line eval(restriction(fun(_) -> from_term([a]) end, 
                           from_term([], [[atom]]),
                           from_term([], [[a]])),
               from_term([], [[atom]])),
    ?line {'EXIT', {type_mismatch, _}} =
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
    ?line {'EXIT', {type_mismatch, _}} =
        (catch restriction(Fun10, from_term([[1]]), from_term([], [[atom]]))),
    ?line {'EXIT', {type_mismatch, _}} = 
        (catch restriction(fun(_) -> from_term({a}) end, 
                           from_term([[a]]),
                           from_term([], [atom]))),
    ?line {'EXIT', {badarg, _}} = 
        (catch restriction(fun(_) -> {a} end, 
                           from_term([[a]]),
                           from_term([], [atom]))),
    ok.

drestriction(suite) -> [];
drestriction(doc) -> [""];
drestriction(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([], 2),

    %% set of ordered sets
    ?line S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    ?line eval(drestriction(S1, set([a,b])), relation([{c,0}])),
    ?line eval(drestriction(2, S1, set([1,2])),  
               relation([{b,22},{c,0}])),
    ?line eval(drestriction(S1, set([a,b,c])), ER),
    ?line eval(drestriction(2, ER, set([a,b])), ER),
    ?line eval(drestriction(1, S1, set([0,1,d,e])), S1),
    ?line eval(drestriction(1, S1, E), S1),
    ?line eval(drestriction({external, fun({_A,B,C}) -> {B,C} end}, 
                            relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
                            relation([{bb,2},{cc,3}])),
               relation([{a,aa,1}])),
    Id = fun(X) -> X end,
    XId = {external, Id},
    ?line eval(drestriction(XId, relation([{a,b}]), E), relation([{a,b}])),
    ?line eval(drestriction(XId, E, relation([{b,d}])), E),
    Fun1 = fun(S) -> {_A,B,C} = to_external(S), from_term({B,C}) end,
    ?line eval(drestriction(Fun1,
                            relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
                            relation([{bb,2},{cc,3}])),
               relation([{a,aa,1}])),
    ?line eval(drestriction({external, fun({_,{A},B}) -> {A,B} end},
                            from_term([{a,{aa},1},{b,{bb},2},{c,{cc},3}]), 
                            from_term([{bb,2},{cc,3}])),
               from_term([{a,{aa},1}])),
    S5 = relation([{1,a},{2,b},{3,c}]),
    ?line eval(drestriction(2, S5, set([b,c])), relation([{1,a}])),
    S4 = relation([{a,1},{b,2},{b,27},{c,0}]),
    ?line eval(drestriction(2, S4, set([])), S4),
    S6 = relation([{1,a},{2,c},{3,b}]),
    ?line eval(drestriction(2, S6, set([d,e])), S6),
    ?line eval(drestriction(2, 
			    relation([{1,d},{2,c},{3,b},{4,a},{5,e}]), 
			    set([c])),
	       relation([{1,d},{3,b},{4,a},{5,e}])),
    ?line eval(drestriction(XId, 
			    relation([{1,a},{3,b},{4,c},{4,d}]), 
			    relation([{2,a},{2,c},{4,c}])),
	       relation([{1,a},{3,b},{4,d}])),
    ?line eval(drestriction(2, relation([{a,b}]), E), relation([{a,b}])),
    ?line eval(drestriction(2, E, relation([{b,d}])), E),
    ?line eval(drestriction(2, relation([{b,d}]), E), relation([{b,d}])),
    ?line eval(drestriction(XId, E, set([a])), E),
    ?line eval(drestriction(1, S1, E), S1),
    ?line {'EXIT', {badarg, _}} =
	(catch drestriction(3, relation([{a,b}]), E)),
    ?line {'EXIT', {badarg, _}} =
	(catch drestriction(3, relation([{a,b}]), relation([{b,d}]))),
    ?line {'EXIT', {badarg, _}} =
	(catch drestriction(3, relation([{a,b}]), set([{b,d}]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch drestriction(2, relation([{a,b}]), relation([{b,d}]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch drestriction({external, fun({A,_B}) -> A end}, 
			    relation([{a,b}]), relation([{b,d}]))),
    ?line {'EXIT', {badarg, _}} = 
	(catch drestriction({external, fun({A,_}) -> {A,0} end}, 
			    from_term([{1,a}]),
			    from_term([{1,0}]))),
    ?line eval(drestriction(2, relation([{a,d},{b,e},{c,b},{d,c}]), set([b,d])),
	       relation([{b,e},{d,c}])),
    ?line {'EXIT', {function_clause, _}} =
	(catch drestriction({external, fun({A,_B}) -> A end}, set([]), E)),

    Fun3 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    ?line eval(drestriction(Fun3, set([1,2]), from_term([{1,0}])),
	       from_term([2])),

    %% set of sets
    ?line {'EXIT', {badarg, _}} = 
        (catch drestriction({external, fun(X) -> X end}, 
			    from_term([], [[atom]]), set([a]))),
    S2 = from_term([], [[atom]]),
    ?line eval(drestriction(Id, S2, E), S2),
    S3 = from_term([[a],[b]], [[atom]]),
    ?line eval(drestriction(Id, S3, E), S3),
    ?line eval(drestriction(Id, from_term([], [[atom]]), set([a])),
	       from_term([], [[atom]])),
    ?line eval(drestriction(fun sofs:union/1,
                            from_term([[[a],[b]], [[b],[c]], 
                                       [[], [a,b]], [[1],[2]]]), 
                            from_term([[a,b],[1,2,3],[b,c]])),
               from_term([[[1],[2]]])),
    ?line eval(drestriction(fun(_) -> from_term([a]) end, 
                            from_term([], [[atom]]),
                            from_term([], [[a]])),
               from_term([], [[atom]])),
    ?line {'EXIT', {type_mismatch, _}} =
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
    ?line {'EXIT', {type_mismatch, _}} =
        (catch drestriction(Fun10, from_term([[1]]), from_term([], [[atom]]))),
    ?line {'EXIT', {type_mismatch, _}} = 
        (catch drestriction(fun(_) -> from_term({a}) end, 
                            from_term([[a]]),
                            from_term([], [atom]))),
    ?line {'EXIT', {badarg, _}} = 
        (catch drestriction(fun(_) -> {a} end, 
			    from_term([[a]]),
			    from_term([], [atom]))),
    ok.

strict_relation_1(suite) -> [];
strict_relation_1(doc) -> [""];
strict_relation_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([], 2),
    ?line eval(strict_relation(E), E),
    ?line eval(strict_relation(ER), ER),
    ?line eval(strict_relation(relation([{1,a},{a,a},{2,b}])),
               relation([{1,a},{2,b}])),
    ?line {'EXIT', {badarg, _}} = 
        (catch strict_relation(relation([{1,2,3}]))),
    F = 0.0, I = round(F),
    ?line FR = relation([{F,I}]),
    if
        F == I -> % term ordering
            eval(strict_relation(FR), ER);
        true -> 
            eval(strict_relation(FR), FR)
    end,
    ok.

extension(suite) -> [];
extension(doc) -> [""];
extension(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([], 2),
    ?line EF = family([]),
    ?line C1 = from_term(3),
    ?line C2 = from_term([3]),
    ?line {'EXIT', {function_clause, _}} = (catch extension(foo, E, C1)),
    ?line {'EXIT', {function_clause, _}} = (catch extension(ER, foo, C1)),
    ?line {'EXIT', {{case_clause, _},_}} = (catch extension(ER, E, foo)),
    ?line {'EXIT', {type_mismatch, _}} = (catch extension(ER, E, E)),
    ?line {'EXIT', {badarg, _}} = (catch extension(C2, E, E)),
    ?line eval(E, extension(E, E, E)),
    ?line eval(EF, extension(EF, E, E)),
    ?line eval(family([{3,[]}]), extension(EF, set([3]), E)),
    ?line eval(ER, extension(ER, E, C1)),
    ?line eval(E, extension(E, ER, E)),
    ?line eval(from_term([],[{{atom,atom},type(ER)}]), extension(E, ER, ER)),

    ?line R1 = relation([{c,7},{c,9},{c,11},{d,17},{f,20}]),
    ?line S1 = set([a,c,d,e]),
    ?line eval(extension(R1, S1, C1), lextension(R1, S1, C1)),

    ?line S2 = set([1,2,3]),
    ?line eval(extension(ER, S2, C1), lextension(ER, S2, C1)),

    ?line R3 = relation([{4,a},{8,b}]),
    ?line S3 = set([1,2,3,4,5,6,7,8,9,10,11]),
    ?line eval(extension(R3, S3, C1), lextension(R3, S3, C1)),

    ?line R4 = relation([{2,b},{4,d},{6,f}]),
    ?line S4 = set([1,3,5,7]),
    ?line eval(extension(R4, S4, C1), lextension(R4, S4, C1)),

    ?line F1 = family([{a,[1]},{c,[2]}]),
    ?line S5 = set([a,b,c,d]),
    ?line eval(extension(F1, S5, C2), lextension(F1, S5, C2)),
    ok.

lextension(R, S, C) ->
    union(R, drestriction(1, constant_function(S, C), domain(R))).

weak_relation_1(suite) -> [];
weak_relation_1(doc) -> [""];
weak_relation_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([], 2),
    ?line eval(weak_relation(E), E),
    ?line eval(weak_relation(ER), ER),
    ?line eval(weak_relation(relation([{a,1},{a,2},{b,2},{c,c}])),
               relation([{1,1},{2,2},{a,1},{a,2},{a,a},{b,2},{b,b},{c,c}])),
    ?line eval(weak_relation(relation([{a,1},{a,a},{a,b}])),
               relation([{1,1},{a,1},{a,a},{a,b},{b,b}])),
    ?line eval(weak_relation(relation([{a,1},{a,b},{7,w}])),
               relation([{1,1},{7,7},{7,w},{a,1},{a,a},{a,b},{b,b},{w,w}])),
    ?line {'EXIT', {badarg, _}} = 
	(catch weak_relation(from_term([{{a},a}]))),
    ?line {'EXIT', {badarg, _}} = 
	(catch weak_relation(from_term([{a,a}],[{d,r}]))),
    ?line {'EXIT', {badarg, _}} = (catch weak_relation(relation([{1,2,3}]))),

    F = 0.0, I = round(F),
    if
        F == I -> % term ordering
            ?line FR1 = relation([{F,I}]),
            eval(weak_relation(FR1), FR1),
            ?line FR2 = relation([{F,2},{I,1}]),
            true = no_elements(weak_relation(FR2)) =:= 5,
            ?line FR3 = relation([{1,0},{1.0,1}]),
            true = no_elements(weak_relation(FR3)) =:= 3;
        true -> 
            ok
    end,
    ok.

to_sets_1(suite) ->  [];
to_sets_1(doc) -> [""];
to_sets_1(Conf) when is_list(Conf) ->
    ?line {'EXIT', {badarg, _}} = (catch to_sets(from_term(a))),
    ?line {'EXIT', {function_clause, _}} = (catch to_sets(a)),
    %% unordered
    ?line [] = to_sets(empty_set()),
    ?line eval(to_sets(from_term([a])), [from_term(a)]),
    ?line eval(to_sets(from_term([[]],[[atom]])), [set([])]),

    ?line L = [from_term([a,b]),from_term([c,d])],
    ?line eval(to_sets(from_sets(L)), L),

    ?line eval(to_sets(relation([{a,1},{b,2}])),
               [from_term({a,1},{atom,atom}), from_term({b,2},{atom,atom})]),

    %% ordered
    ?line O = {from_term(a,atom), from_term({b}, {atom}), set([c,d])},
    ?line eval(to_sets(from_sets(O)), O),
    ok.


specification(suite) -> [];
specification(doc) -> [""];
specification(Conf) when is_list(Conf) ->
    Fun = {external, fun(I) when is_integer(I) -> true; (_) -> false end},
    ?line [1,2,3] = to_external(specification(Fun, set([a,1,b,2,c,3]))),

    Fun2 = fun(S) -> is_subset(S, set([1,3,5,7,9])) end,
    S2 = from_term([[1],[2],[3],[4],[5],[6],[7]]),
    ?line eval(specification(Fun2, S2), from_term([[1],[3],[5],[7]])),
    Fun2x = fun([1]) -> true;
	       ([3]) -> true;
	       (_) -> false 
	    end,
    ?line eval(specification({external,Fun2x}, S2), from_term([[1],[3]])),

    Fun3 = fun(_) -> neither_true_nor_false end,
    ?line {'EXIT', {badarg, _}} = 
	(catch specification(Fun3, set([a]))),
    ?line {'EXIT', {badarg, _}} = 
	(catch specification({external, Fun3}, set([a]))),
    ?line {'EXIT', {badarg, _}} = 
	(catch specification(Fun3, from_term([[a]]))),
    ?line {'EXIT', {function_clause, _}} = 
	(catch specification(Fun, a)),
    ok.

union_1(suite) -> [];
union_1(doc) -> [""];
union_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([], 2),
    ?line {'EXIT', {badarg, _}} = (catch union(ER)),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch union(relation([{a,b}]), relation([{a,b,c}]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch union(from_term([{a,b}]), from_term([{c,[x]}]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch union(from_term([{a,b}]), from_term([{c,d}], [{d,r}]))),
    ?line {'EXIT', {badarg, _}} = (catch union(set([a,b,c]))),
    ?line eval(union(E), E),
    ?line eval(union(from_term([[]],[[atom]])), set([])),
    ?line eval(union(from_term([[{a,b},{b,c}],[{b,c}]])), 
               relation([{a,b},{b,c}])),
    ?line eval(union(from_term([[1,2,3],[2,3,4],[3,4,5]])), 
               set([1,2,3,4,5])),

    ?line eval(union(from_term([{[a],[],c}]), from_term([{[],[],q}])),
	       from_term([{[a],[],c},{[],[],q}])),

    ?line eval(union(E, E), E),
    ?line eval(union(set([a,b]), E), set([a,b])),
    ?line eval(union(E, set([a,b])), set([a,b])),

    ?line eval(union(from_term([[a,b]])), from_term([a,b])),
    ok.

intersection_1(suite) -> [];
intersection_1(doc) -> [""];
intersection_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line {'EXIT', {badarg, _}} = (catch intersection(from_term([a,b]))),
    ?line {'EXIT', {badarg, _}} = (catch intersection(E)),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch intersection(relation([{a,b}]), relation([{a,b,c}]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch intersection(relation([{a,b}]), from_term([{a,b}],[{d,r}]))),

    ?line eval(intersection(from_term([[a,b,c],[d,e,f],[g,h,i]])), set([])),

    ?line eval(intersection(E, E), E),
    ?line eval(intersection(set([a,b,c]),set([0,b,q])), 
               set([b])),
    ?line eval(intersection(set([0,b,q]),set([a,b,c])), 
               set([b])),
    ?line eval(intersection(set([a,b,c]),set([a,b,c])), 
               set([a,b,c])),
    ?line eval(intersection(set([a,b,d]),set([c,d])), 
               set([d])),
    ok.

difference(suite) -> [];
difference(doc) -> [""];
difference(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch difference(relation([{a,b}]), relation([{a,b,c}]))),
    ?line eval(difference(E, E), E),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch difference(relation([{a,b}]), from_term([{a,c}],[{d,r}]))),
    ?line eval(difference(set([a,b,c,d,f]), set([a,d,e,g])), 
               set([b,c,f])),
    ?line eval(difference(set([a,b,c]), set([d,e,f])), 
               set([a,b,c])),
    ?line eval(difference(set([a,b,c]), set([a,b,c,d,e,f])), 
               set([])),
    ?line eval(difference(set([e,f,g]), set([a,b,c,e])), 
               set([f,g])),
    ?line eval(difference(set([a,b,d,e,f]), set([c])), 
               set([a,b,d,e,f])),
    ok.

symdiff(suite) -> [];
symdiff(doc) -> [""];
symdiff(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch symdiff(relation([{a,b}]), relation([{a,b,c}]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch symdiff(relation([{a,b}]), from_term([{a,b}], [{d,r}]))),
    ?line eval(symdiff(E, E), E),
    ?line eval(symdiff(set([a,b,c,d,e,f]), set([0,1,a,c])),
               union(set([b,d,e,f]), set([0,1]))),
    ?line eval(symdiff(set([a,b,c]), set([q,v,w,x,y])),
               union(set([a,b,c]), set([q,v,w,x,y]))),
    ?line eval(symdiff(set([a,b,c,d,e,f]), set([a,b,c])),
               set([d,e,f])),
    ?line eval(symdiff(set([c,e,g,h,i]), set([b,d,f])),
               union(set([c,e,g,h,i]), set([b,d,f]))),
    ?line eval(symdiff(set([c,d,g,h,k,l]), 
                       set([a,b,e,f,i,j,m,n])),
               union(set([c,d,g,h,k,l]), set([a,b,e,f,i,j,m,n]))),
    ?line eval(symdiff(set([c,d,g,h,k,l]), 
                       set([d,e,h,i,l,m,n,o,p])),
               union(set([c,g,k]), set([e,i,m,n,o,p]))),
    ok.

symmetric_partition(suite) -> [];
symmetric_partition(doc) -> [""];
symmetric_partition(Conf) when is_list(Conf) ->
    ?line E = set([]),
    ?line S1 = set([1,2,3,4]),
    ?line S2 = set([3,4,5,6]),
    ?line S3 = set([3,4]),
    ?line S4 = set([1,2,3,4,5,6]),
    ?line T1 = set([1,2]),
    ?line T2 = set([3,4]),
    ?line T3 = set([5,6]),
    ?line T4 = set([1,2,5,6]),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch symmetric_partition(relation([{a,b}]), relation([{a,b,c}]))),
    ?line {E, E, E} = symmetric_partition(E, E),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch symmetric_partition(relation([{a,b}]), 
                                   from_term([{a,c}],[{d,r}]))),
    ?line {E, E, S1} = symmetric_partition(E, S1),
    ?line {S1, E, E} = symmetric_partition(S1, E),
    ?line {T1, T2, T3} = symmetric_partition(S1, S2),
    ?line {T3, T2, T1} = symmetric_partition(S2, S1),
    ?line {E, T2, T4} = symmetric_partition(S3, S4),
    ?line {T4, T2, E} = symmetric_partition(S4, S3),

    ?line S5 = set([1,3,5]),
    ?line S6 = set([2,4,6,7,8]),
    ?line {S5, E, S6} = symmetric_partition(S5, S6),
    ?line {S6, E, S5} = symmetric_partition(S6, S5),
    ?line EE = empty_set(),
    ?line {EE, EE, EE} = symmetric_partition(EE, EE),

    ok.

is_sofs_set_1(suite) -> [];
is_sofs_set_1(doc) -> [""];
is_sofs_set_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line true = is_sofs_set(E),
    ?line true = is_sofs_set(from_term([a])),
    ?line true = is_sofs_set(from_term({a})),
    ?line true = is_sofs_set(from_term(a)),
    ?line false = is_sofs_set(a),
    ok.

is_set_1(suite) -> [];
is_set_1(doc) -> [""];
is_set_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line true = is_set(E),
    ?line true = is_set(from_term([a])),
    ?line false = is_set(from_term({a})),
    ?line false = is_set(from_term(a)),
    ?line {'EXIT', _} = (catch is_set(a)),

    ?line true = is_empty_set(E),
    ?line false = is_empty_set(from_term([a])),
    ?line false = is_empty_set(from_term({a})),
    ?line false = is_empty_set(from_term(a)),
    ?line {'EXIT', _} = (catch is_empty_set(a)),

    ok.

is_equal(suite) -> [];
is_equal(doc) -> [""];
is_equal(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line true = is_equal(E, E),
    ?line false = is_equal(from_term([a]), E),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch is_equal(intersection(set([a]), set([b])),
			intersection(from_term([{a}]), from_term([{b}])))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch is_equal(from_term([],[{[atom],atom,[atom]}]), 
			from_term([],[{[atom],{atom},[atom]}]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch is_equal(set([a]), from_term([a],[type]))),

    ?line E2 = from_sets({from_term(a,atom)}),
    ?line true = is_equal(E2, E2),
    ?line true = is_equal(from_term({a}, {atom}), E2),
    ?line false = is_equal(from_term([{[a],[],c}]), 
			   from_term([{[],[],q}])),

    ?line {'EXIT', {type_mismatch, _}} =
        (catch is_equal(E, E2)),
    ?line {'EXIT', {type_mismatch, _}} =
        (catch is_equal(E2, E)),
    ?line true = is_equal(from_term({[],a,[]},{[atom],atom,[atom]}), 
                          from_term({[],a,[]},{[atom],atom,[atom]})),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch is_equal(from_term({[],a,[]},{[atom],atom,[atom]}), 
			from_term({[],{a},[]},{[atom],{atom},[atom]}))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch is_equal(from_term({a}), from_term({a},{type}))),

    ok.

is_subset(suite) -> [];
is_subset(doc) -> [""];
is_subset(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line true = is_subset(E, E),
    ?line true = is_subset(set([a,c,e]), set([a,b,c,d,e])),
    ?line false = is_subset(set([a,b]), E),
    ?line false = is_subset(set([d,e,f]), set([b,c,d,e])),
    ?line false = is_subset(set([a,b,c]), set([b,c])),
    ?line false = is_subset(set([b,c]), set([a,c])),
    ?line false = is_subset(set([d,e]), set([a,b])),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch is_subset(intersection(set([a]), set([b])),
			 intersection(from_term([{a}]), from_term([{b}])))),
    ?line {'EXIT', {type_mismatch, _}} =
        (catch is_subset(set([a]), from_term([a,b], [at]))),
    ok.

is_a_function_1(suite) -> [];
is_a_function_1(doc) -> [""];
is_a_function_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([], 2),
    ?line {'EXIT', {badarg, _}} = (catch is_a_function(set([a,b]))),
    ?line true = is_a_function(E),
    ?line true = is_a_function(ER),
    ?line true = is_a_function(relation([])),
    ?line true = is_a_function(relation([],2)),
    ?line true = is_a_function(relation([{a,b},{b,c}])),
    ?line false = is_a_function(relation([{a,b},{b,c},{b,d},{e,f}])),
    ?line IS = relation([{{a,b},c},{{a,b},d}]),
    ?line false = is_a_function(IS),
    F = 0.0, I = round(F),
    ?line FR = relation([{I,F},{F,1}]),
    if
        F == I -> % term ordering
            false = is_a_function(FR);
        true -> 
            true = is_a_function(FR)
    end,
    ok.

is_disjoint(suite) -> [];
is_disjoint(doc) -> [""];
is_disjoint(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line {'EXIT', {type_mismatch, _}} = 
	(catch is_disjoint(relation([{a,1}]), set([a,b]))),
    ?line {'EXIT', {type_mismatch, _}} = 
	(catch is_disjoint(set([a]), from_term([a],[mota]))),
    ?line true = is_disjoint(E, E),
    ?line false = is_disjoint(set([a,b,c]),set([b,c,d])),
    ?line false = is_disjoint(set([b,c,d]),set([a,b,c])),
    ?line true = is_disjoint(set([a,c,e]),set([b,d,f])),
    ok.

join(suite) -> [];
join(doc) -> [""];
join(Conf) when is_list(Conf) ->
    ?line E = empty_set(),

    ?line {'EXIT', {badarg, _}} = (catch join(relation([{a,1}]), 3, E, 5)),
    ?line {'EXIT', {badarg, _}} = (catch join(E, 1, relation([{a,1}]), 3)),
    ?line {'EXIT', {badarg, _}} = (catch join(E, 1, from_term([a]), 1)),

    ?line eval(join(E, 1, E, 2), E),
    ?line eval(join(E, 1, from_term([{{a},b}]), 2), E),
    ?line eval(join(from_term([{{a},b}]), 2, E, 1), E),
    ?line eval(join(from_term([{{a},b,e}]), 2, from_term([{c,{d}}]), 1),
               from_term([], [{{atom},atom,atom,{atom}}])),
    ?line eval(join(relation([{a}]), 1, relation([{1,a},{2,a}]), 2),
               relation([{a,1},{a,2}])),
    ?line eval(join(relation([{a,b,c},{b,c,d}]), 2, 
                    relation([{1,b},{2,a},{3,c}]), 2),
               relation([{a,b,c,1},{b,c,d,3}])),
    ?line eval(join(relation([{1,a,aa},{1,b,bb},{1,c,cc},{2,a,aa},{2,b,bb}]), 
                    1,
                    relation([{1,c,cc},{1,d,dd},{1,e,ee},{2,c,cc},{2,d,dd}]),
                    1),
             relation([{1,a,aa,c,cc},{1,a,aa,d,dd},{1,a,aa,e,ee},{1,b,bb,c,cc},
		       {1,b,bb,d,dd},{1,b,bb,e,ee},{1,c,cc,c,cc},{1,c,cc,d,dd},
		       {1,c,cc,e,ee},{2,a,aa,c,cc},{2,a,aa,d,dd},{2,b,bb,c,cc},
		       {2,b,bb,d,dd}])),

    R1 = relation([{a,b},{b,c}]),
    R2 = relation([{b,1},{a,2},{c,3},{c,4}]),
    ?line eval(join(R1, 1, R2, 1), from_term([{a,b,2},{b,c,1}])),
    ?line eval(join(R1, 2, R2, 1), from_term([{a,b,1},{b,c,3},{b,c,4}])),
    ?line eval(join(R1, 1, converse(R2), 2),
	       from_term([{a,b,2},{b,c,1}])),
    ?line eval(join(R1, 2, converse(R2), 2),
	       from_term([{a,b,1},{b,c,3},{b,c,4}])),
    ok.

canonical(suite) -> [];
canonical(doc) -> [""];
canonical(Conf) when is_list(Conf) ->
    ?line E = empty_set(),    
    ?line {'EXIT', {badarg, _}} = 
        (catch canonical_relation(set([a,b]))),
    ?line eval(canonical_relation(E), E),
    ?line eval(canonical_relation(from_term([[]])), E),
    ?line eval(canonical_relation(from_term([[a,b,c]])),
               from_term([{a,[a,b,c]},{b,[a,b,c]},{c,[a,b,c]}])),
    ok.

relation_to_family_1(suite) -> [];
relation_to_family_1(doc) -> [""];
relation_to_family_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line EF = family([]),
    ?line eval(relation_to_family(E), E),
    ?line eval(relation_to_family(relation([])), EF),
    ?line eval(relation_to_family(relation([], 2)), EF),
    ?line R = relation([{b,1},{c,7},{c,9},{c,11}]),
    ?line F = family([{b,[1]},{c,[7,9,11]}]),
    ?line eval(relation_to_family(R), F),
    ?line eval(sofs:rel2fam(R), F),
    ?line {'EXIT', {badarg, _}} = (catch relation_to_family(set([a]))),
    ok.

domain_1(suite) -> [];
domain_1(doc) -> [""];
domain_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),
    ?line {'EXIT', {badarg, _}} = (catch domain(relation([],3))),
    ?line eval(domain(E), E),
    ?line eval(domain(ER), set([])),
    ?line eval(domain(relation([{1,a},{1,b},{2,a},{2,b}])), set([1,2])),
    ?line eval(domain(relation([{a,1},{b,2},{c,3}])), set([a,b,c])),
    ?line eval(field(relation([{a,1},{b,2},{c,3}])), 
               set([a,b,c,1,2,3])),
    F = 0.0, I = round(F),
    ?line FR = relation([{I,a},{F,b}]),
    if
        F == I -> % term ordering
            ?line true = (1 =:= no_elements(domain(FR)));
        true -> 
            ?line true = (2 =:= no_elements(domain(FR)))
    end,
    ok.

range_1(suite) -> [];
range_1(doc) -> [""];
range_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),
    ?line {'EXIT', {badarg, _}} = (catch range(relation([],3))),
    ?line eval(range(E), E),
    ?line eval(range(ER), set([])),
    ?line eval(range(relation([{1,a},{1,b},{2,a},{2,b}])), set([a,b])),
    ?line eval(range(relation([{a,1},{b,2},{c,3}])), set([1,2,3])),
    ok.
    
inverse_1(suite) -> [];
inverse_1(doc) -> [""];
inverse_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),
    ?line {'EXIT', {badarg, _}} = (catch inverse(relation([],3))),
    ?line {'EXIT', {bad_function, _}} = 
	(catch inverse(relation([{1,a},{1,b}]))),
    ?line {'EXIT', {bad_function, _}} = 
	(catch inverse(relation([{1,a},{2,a}]))),
    ?line eval(inverse(E), E),
    ?line eval(inverse(ER), ER),
    ?line eval(inverse(relation([{a,1},{b,2},{c,3}])),
               relation([{1,a},{2,b},{3,c}])),
    F = 0.0, I = round(F),
    ?line FR = relation([{I,a},{F,b}]),
    if
        F == I -> % term ordering
            ?line {'EXIT', {bad_function, _}} = (catch inverse(FR));
        true -> 
            ?line eval(inverse(FR), relation([{a,I},{b,F}]))
    end,
    ok.
    
converse_1(suite) -> [];
converse_1(doc) -> [""];
converse_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),
    ?line {'EXIT', {badarg, _}} = (catch converse(relation([],3))),
    ?line eval(converse(ER), ER),
    ?line eval(converse(E), E),
    ?line eval(converse(relation([{a,1},{b,2},{c,3}])),
               relation([{1,a},{2,b},{3,c}])),
    ?line eval(converse(relation([{1,a},{1,b}])),
	       relation([{a,1},{b,1}])),
    ?line eval(converse(relation([{1,a},{2,a}])),
	       relation([{a,1},{a,2}])),
    ok.
    
no_elements_1(suite) -> [];
no_elements_1(doc) -> [""];
no_elements_1(Conf) when is_list(Conf) ->
    ?line 0 = no_elements(empty_set()),
    ?line 0 = no_elements(set([])),
    ?line 1 = no_elements(from_term([a])),
    ?line 10 = no_elements(from_term(lists:seq(1,10))),
    ?line 3 = no_elements(from_term({a,b,c},{atom,atom,atom})),
    ?line {'EXIT', {badarg, _}} = (catch no_elements(from_term(a))),
    ?line {'EXIT', {function_clause, _}} = (catch no_elements(a)),
    ok.

image(suite) -> [];
image(doc) -> [""];
image(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),
    ?line eval(image(E, E), E),
    ?line eval(image(ER, E), set([])),
    ?line eval(image(relation([{a,1},{b,2},{c,3},{f,6}]), set([a,b,c,d,f])), 
               set([1,2,3,6])),
    ?line eval(image(relation([{a,1},{b,2},{c,3},{d,4},{r,17}]), 
		     set([b,c,q,r])), 
               set([2,3,17])),
    ?line eval(image(from_term([{[a],{1}},{[b],{2}}]), from_term([[a]])),
	       from_term([{1}])),
    ?line eval(image(relation([{1,a},{2,a},{3,a},{4,b},{2,b}]), set([1,2,4])),
	       set([a,b])),
    ?line {'EXIT', {badarg, _}} =
	(catch image(from_term([a,b]), E)),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch image(from_term([{[a],1}]), set([[a]]))),
    ok.

inverse_image(suite) -> [];
inverse_image(doc) -> [""];
inverse_image(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),
    ?line eval(inverse_image(E, E), E),
    ?line eval(inverse_image(ER, E), set([])),
    ?line eval(inverse_image(converse(relation([{a,1},{b,2},{c,3},{f,6}])), 
			     set([a,b,c,d,f])), 
               set([1,2,3,6])),
    ?line eval(inverse_image(converse(relation([{a,1},{b,2},{c,3},
						{d,4},{r,17}])), 
			     set([b,c,q,r])), 
               set([2,3,17])),
    ?line eval(inverse_image(converse(from_term([{[a],{1}},{[b],{2}}])), 
			     from_term([[a]])),
	       from_term([{1}])),
    ?line eval(inverse_image(converse(relation([{1,a},{2,a},
						{3,a},{4,b},{2,b}])), 
			     set([1,2,4])),
	       set([a,b])),
    ?line {'EXIT', {badarg, _}} =
	(catch inverse_image(from_term([a,b]), E)),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch inverse_image(converse(from_term([{[a],1}])), set([[a]]))),
    ok.

composite_1(suite) -> [];
composite_1(doc) -> [""];
composite_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line EF = a_function([]),
    ?line eval(composite(E, E), E),
    ?line eval(composite(E, a_function([{a,b}])), E),
    ?line eval(composite(relation([{a,b}]), E), E),
    ?line {'EXIT', {bad_function, _}} = 
	(catch composite(EF, relation([{a,b},{a,c}]))),
    ?line {'EXIT', {bad_function, _}} = 
	(catch composite(a_function([{b,a}]), EF)),
    ?line {'EXIT', {bad_function, _}} = 
	(catch composite(relation([{1,a},{2,b},{2,a}]), 
			 a_function([{a,1},{b,3}]))),
    ?line {'EXIT', {bad_function, _}} = 
	 (catch composite(a_function([{1,a},{2,b}]), a_function([{b,3}]))),
    ?line eval(composite(EF, EF), EF),
    ?line eval(composite(a_function([{b,a}]), from_term([{a,{b,c}}])),
               from_term([{b,{b,c}}])),
    ?line eval(composite(a_function([{q,1},{z,2}]), 
			 a_function([{1,a},{2,a}])),
               a_function([{q,a},{z,a}])),
    ?line eval(composite(a_function([{a,0},{b,0},{c,1},{d,1},{e,2},{f,3}]), 
			 a_function([{0,p},{1,q},{2,r},{3,w},{4,aa}])),
               a_function([{c,q},{d,q},{f,w},{e,r},{a,p},{b,p}])),
    ?line eval(composite(a_function([{1,c}]), 
			 a_function([{a,1},{b,3},{c,4}])),
	       a_function([{1,4}])),
    ?line {'EXIT', {bad_function, _}} = 
	(catch composite(a_function([{1,a},{2,b}]), 
			 a_function([{a,1},{c,3}]))),
    ?line {'EXIT', {badarg, _}} = 
	(catch composite(from_term([a,b]), E)),
    ?line {'EXIT', {badarg, _}} = 
	(catch composite(E, from_term([a,b]))),
    ?line {'EXIT', {type_mismatch, _}} = 
        (catch composite(from_term([{a,b}]), from_term([{{a},b}]))),
    ?line {'EXIT', {type_mismatch, _}} = 
        (catch composite(from_term([{a,b}]), 
			 from_term([{b,c}], [{d,r}]))),
    F = 0.0, I = round(F),
    ?line FR1 = relation([{1,c}]),
    ?line FR2 = relation([{I,1},{F,3},{c,4}]),
    if
        F == I -> % term ordering
            ?line {'EXIT', {bad_function, _}} = (catch composite(FR1, FR2));
        true -> 
            ?line eval(composite(FR1, FR2), a_function([{1,4}]))
    end,
    ok.

relative_product_1(suite) -> [];
relative_product_1(doc) -> [""];
relative_product_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),
    ?line eval(relative_product1(E, E), E),
    ?line eval(relative_product1(E, relation([{a,b}])), E),
    ?line eval(relative_product1(relation([{a,b}]), E), E),
    ?line eval(relative_product1(relation([{a,b}]), from_term([{a,{b,c}}])),
               from_term([{b,{b,c}}])),
    ?line eval(relative_product1(relation([{1,z},{1,q},{2,z}]), 
				 relation([{1,a},{1,b},{2,a}])),
               relation([{q,a},{q,b},{z,a},{z,b}])),
    ?line eval(relative_product1(relation([{0,a},{0,b},{1,c},
					   {1,d},{2,e},{3,f}]), 
				 relation([{1,q},{3,w}])),
               relation([{c,q},{d,q},{f,w}])),
    ?line {'EXIT', {badarg, _}} = 
	(catch relative_product1(from_term([a,b]), ER)),
    ?line {'EXIT', {badarg, _}} = 
	(catch relative_product1(ER, from_term([a,b]))),
    ?line {'EXIT', {type_mismatch, _}} = 
        (catch relative_product1(from_term([{a,b}]), from_term([{{a},b}]))),
    ?line {'EXIT', {type_mismatch, _}} = 
        (catch relative_product1(from_term([{a,b}]), 
				 from_term([{b,c}], [{d,r}]))),
    ok.

relative_product_2(suite) -> [];
relative_product_2(doc) -> [""];
relative_product_2(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),

    ?line {'EXIT', {badarg, _}} = (catch relative_product({from_term([a,b])})),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch relative_product({from_term([{a,b}]), from_term([{{a},b}])})),
    ?line {'EXIT', {badarg, _}} = (catch relative_product({})), 
    ?line true = is_equal(relative_product({ER}), 
			  from_term([], [{atom,{atom}}])),
    ?line eval(relative_product({relation([{a,b},{c,a}]),
				 relation([{a,1},{a,2}]),
				 relation([{a,aa},{c,1}])}),
               from_term([{a,{b,1,aa}},{a,{b,2,aa}}])),
    ?line eval(relative_product({relation([{a,b}])}, E), E),
    ?line eval(relative_product({E}, relation([{a,b}])), E),
    ?line eval(relative_product({E,from_term([], [{{atom,atom,atom},atom}])}),
	       E),
    ?line {'EXIT', {badarg, _}} = 
        (catch relative_product({from_term([a,b])}, E)),
    ?line {'EXIT', {badarg, _}} = 
	(catch relative_product({relation([])}, set([]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch relative_product({from_term([{a,b}]), 
				 from_term([{{a},b}])}, ER)),

    ?line {'EXIT', {badarg, _}} = (catch relative_product({}, ER)),
    ?line relprod2({relation([{a,b}])}, from_term([],[{{atom},atom}]), ER),
    ?line relprod2({relation([{a,b}]),relation([{a,1}])},
                   from_term([{{b,1},{tjo,hej,sa}}]),
                   from_term([{a,{tjo,hej,sa}}])),
    ?line relprod2({relation([{a,b}]), ER}, from_term([{{a,b},b}]), ER),
    ?line relprod2({relation([{a,b},{c,a}]),
                    relation([{a,1},{a,2}])},
                   from_term([{{b,1},b1},{{b,2},b2}]),
                   relation([{a,b1},{a,b2}])),
    ?line eval(relative_product({relation([{a,b}]), ER}), 
               from_term([],[{atom,{atom,atom}}])),
    ?line eval(relative_product({from_term([{{a,[a,b]},[a]}]),
				 from_term([{{a,[a,b]},[[a,b]]}])}),
               from_term([{{a,[a,b]},{[a],[[a,b]]}}])),
    ok.

relprod2(A1T, A2, R) ->
    %% A tuple as first argument is the old interface:
    eval(relative_product(A1T, A2), R),
    eval(relative_product(tuple_to_list(A1T), A2), R).

product_1(suite) -> [];
product_1(doc) -> [""];
product_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line eval(product(E, E), E),
    ?line eval(product(relation([]), E), E),
    ?line eval(product(E, relation([])), E),
    ?line eval(product(relation([{a,b}]),relation([{c,d}])),
               from_term([{{a,b},{c,d}}],[{{atom,atom},{atom,atom}}])),

    ?line eval(product({E, set([a,b,c])}), E),
    ?line eval(product({set([a,b,c]), E}), E),
    ?line eval(product({set([a,b,c]), E, E}), E),
    ?line eval(product({E,E}), E),
    ?line eval(product({set([a,b]),set([1,2])}), 
               relation([{a,1},{a,2},{b,1},{b,2}])),
    ?line eval(product({from_term([a,b]), from_term([{a,b},{c,d}]),
                        from_term([1])}),
               from_term([{a,{a,b},1},{a,{c,d},1},{b,{a,b},1},{b,{c,d},1}])),
    ?line {'EXIT', {badarg, _}} = (catch product({})),
    ?line {'EXIT', {badarg, _}} = (catch product({foo})),
    ?line eval(product({E}), E),
    ?line eval(product({E, E}), E),
    ?line eval(product(set([a,b]), set([1,2])),
               relation([{a,1},{a,2},{b,1},{b,2}])),
    ?line eval(product({relation([]), E}), E),
    ok.

partition_1(suite) -> [];
partition_1(doc) -> [""];
partition_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),
    ?line Id = fun(A) -> A end,
    ?line S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    ?line eval(partition(1, E), E),
    ?line eval(partition(2, E), E),
    ?line eval(partition(1, ER), from_term([], [type(ER)])),
    ?line eval(partition(2, ER), from_term([], [type(ER)])),
    ?line eval(partition(1, relation([{1,a},{1,b},{2,c},{2,d}])), 
	       from_term([[{1,a},{1,b}],[{2,c},{2,d}]])),
    ?line eval(partition(2, relation([{1,a},{1,b},{2,a},{2,b},{3,c}])), 
	       from_term([[{1,a},{2,a}],[{1,b},{2,b}],[{3,c}]])),
    ?line eval(partition(2, relation([{1,a}])), from_term([[{1,a}]])),
    ?line eval(partition(2, relation([{1,a},{2,b}])),
			 from_term([[{1,a}],[{2,b}]])),
    ?line eval(partition(2, relation([{1,a},{2,a},{3,a}])), 
	       from_term([[{1,a},{2,a},{3,a}]])),
    ?line eval(partition(2, relation([{1,b},{2,a}])), % OTP-4516
	       from_term([[{1,b}],[{2,a}]])),
    ?line eval(union(partition(Id, S1)), S1),
    ?line eval(partition({external, fun({A,{B,_}}) -> {A,B} end}, 
                         from_term([{a,{b,c}},{b,{c,d}},{a,{b,f}}])),
               from_term([[{a,{b,c}},{a,{b,f}}],[{b,{c,d}}]])),
    F = 0.0, I = round(F),
    ?line FR = relation([{I,a},{F,b}]),
    if
        F == I -> % term ordering
            ?line eval(partition(1, FR), from_term([[{I,a},{F,b}]]));
        true -> 
            ?line eval(partition(1, FR), from_term([[{I,a}],[{F,b}]]))
    end,
    ?line {'EXIT', {badarg, _}} = (catch partition(2, set([a]))),
    ?line {'EXIT', {badarg, _}} = (catch partition(1, set([a]))),
    ?line eval(partition(Id, set([a])), from_term([[a]])),

    ?line eval(partition(E), E),
    ?line P1 = from_term([[a,b,c],[d,e,f],[g,h]]),
    ?line P2 = from_term([[a,d],[b,c,e,f,q,v]]),
    ?line eval(partition(union(P1, P2)), 
               from_term([[a],[b,c],[d],[e,f],[g,h],[q,v]])),
    ?line {'EXIT', {badarg, _}} = (catch partition(from_term([a]))),
    ok.

partition_3(suite) -> [];
partition_3(doc) -> [""];
partition_3(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),

    %% set of ordered sets
    ?line S1 = relation([{a,1},{b,2},{b,22},{c,0}]),
    ?line eval(partition(1, S1, set([0,1,d,e])),
	       lpartition(1, S1, set([0,1,d,e]))),
    ?line eval(partition(1, S1, E), lpartition(1, S1, E)),
    ?line eval(partition(2, ER, set([a,b])), lpartition(2, ER, set([a,b]))),

    XFun1 = {external, fun({_A,B,C}) -> {B,C} end},
    R1a = relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
    R1b = relation([{bb,2},{cc,3}]),
    ?line eval(partition(XFun1, R1a, R1b), lpartition(XFun1, R1a, R1b)),

    Id = fun(X) -> X end,
    XId = {external, Id},
    R2 = relation([{a,b}]),
    ?line eval(partition(XId, R2, E), lpartition(XId, R2, E)),

    R3 = relation([{b,d}]),
    ?line eval(partition(XId, E, R3), lpartition(XId, E, R3)),

    Fun1 = fun(S) -> {_A,B,C} = to_external(S), from_term({B,C}) end,
    R4a = relation([{a,aa,1},{b,bb,2},{c,cc,3}]),
    R4b = relation([{bb,2},{cc,3}]),
    ?line eval(partition(Fun1,R4a,R4b), lpartition(Fun1,R4a,R4b)),

    XFun2 = {external, fun({_,{A},B}) -> {A,B} end},
    R5a = from_term([{a,{aa},1},{b,{bb},2},{c,{cc},3}]),
    R5b = from_term([{bb,2},{cc,3}]),
    ?line eval(partition(XFun2,R5a, R5b), lpartition(XFun2,R5a, R5b)),

    R6 = relation([{a,b}]),
    ?line eval(partition(2, R6, E), lpartition(2, R6, E)),

    R7 = relation([{b,d}]),
    ?line eval(partition(2, E, R7), lpartition(2, E, R7)),

    S2 = set([a]),
    ?line eval(partition(XId, E, S2), lpartition(XId, E, S2)),
    ?line eval(partition(XId, S1, E), lpartition(XId, S1, E)),
    ?line {'EXIT', {badarg, _}} =
	(catch partition(3, relation([{a,b}]), E)),
    ?line {'EXIT', {badarg, _}} =
	(catch partition(3, relation([{a,b}]), relation([{b,d}]))),
    ?line {'EXIT', {badarg, _}} =
	(catch partition(3, relation([{a,b}]), set([{b,d}]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch partition(2, relation([{a,b}]), relation([{b,d}]))),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch partition({external, fun({A,_B}) -> A end}, 
			 relation([{a,b}]), relation([{b,d}]))),
    ?line {'EXIT', {badarg, _}} = 
	(catch partition({external, fun({A,_}) -> {A,0} end}, 
			 from_term([{1,a}]),
			 from_term([{1,0}]))),

    S18a = relation([{1,e},{2,b},{3,c},{4,b},{5,a},{6,0}]),
    S18b = set([b,d,f]),
    ?line eval(partition({external,fun({_,X}) -> X end}, S18a, S18b),
	       lpartition({external,fun({_,X}) -> X end}, S18a, S18b)),
    S19a = sofs:relation([{3,a},{8,b}]),
    S19b = set([2,6,7]),
    ?line eval(partition({external,fun({X,_}) -> X end}, S19a, S19b),
	       lpartition({external,fun({X,_}) -> X end}, S19a, S19b)),

    R8a = relation([{a,d},{b,e},{c,b},{d,c}]),
    S8 = set([b,d]),
    ?line eval(partition(2, R8a, S8), lpartition(2, R8a, S8)),

    S16a = relation([{1,e},{2,b},{3,c},{4,b},{5,a},{6,0}]),
    S16b = set([b,c,d]),
    ?line eval(partition(2, S16a, S16b), lpartition(2, S16a, S16b)),
    S17a = relation([{e,1},{b,2},{c,3},{b,4},{a,5},{0,6}]),
    S17b = set([b,c,d]),
    ?line eval(partition(1, S17a, S17b), lpartition(1, S17a, S17b)),

    ?line {'EXIT', {function_clause, _}} =
	(catch partition({external, fun({A,_B}) -> A end}, set([]), E)),

    Fun3 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    S9a = set([1,2]),
    S9b = from_term([{1,0}]),
    ?line eval(partition(Fun3, S9a, S9b), lpartition(Fun3, S9a, S9b)),

    S14a = relation([{1,a},{2,b},{3,c},{0,0}]),
    S14b = set([b,c]),
    ?line eval(partition(2, S14a, S14b), lpartition(2, S14a, S14b)),
    S15a = relation([{a,1},{b,2},{c,3},{0,0}]),
    S15b = set([b,c]),
    ?line eval(partition(1, S15a, S15b), lpartition(1, S15a, S15b)),

    %% set of sets
    ?line {'EXIT', {badarg, _}} = 
        (catch partition({external, fun(X) -> X end}, 
			 from_term([], [[atom]]), set([a]))),

    S10 = from_term([], [[atom]]),
    ?line eval(partition(Id, S10, E), lpartition(Id, S10, E)),

    S10e = from_term([[a],[b]], [[atom]]),
    ?line eval(partition(Id, S10e, E), lpartition(Id, S10e, E)),

    S11a = from_term([], [[atom]]),
    S11b = set([a]),
    ?line eval(partition(Id, S11a, S11b), lpartition(Id, S11a, S11b)),

    S12a = from_term([[[a],[b]], [[b],[c]], [[], [a,b]], [[1],[2]]]),
    S12b = from_term([[a,b],[1,2,3],[b,c]]),
    ?line eval(partition(fun sofs:union/1, S12a, S12b),
	       lpartition(fun sofs:union/1, S12a, S12b)),

    Fun13 = fun(_) -> from_term([a]) end,
    S13a = from_term([], [[atom]]),
    S13b = from_term([], [[a]]),
    ?line eval(partition(Fun13, S13a, S13b), lpartition(Fun13, S13a, S13b)),

    ?line {'EXIT', {type_mismatch, _}} =
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
    ?line {'EXIT', {type_mismatch, _}} =
        (catch partition(Fun10, from_term([[1]]), from_term([], [[atom]]))),
    ?line {'EXIT', {type_mismatch, _}} = 
        (catch partition(fun(_) -> from_term({a}) end, 
			 from_term([[a]]),
			 from_term([], [atom]))),
    ?line {'EXIT', {badarg, _}} = 
        (catch partition(fun(_) -> {a} end, 
			 from_term([[a]]),
			 from_term([], [atom]))),
    ok.

lpartition(F, S1, S2) ->
    {restriction(F, S1, S2), drestriction(F, S1, S2)}.

multiple_relative_product(suite) -> [];
multiple_relative_product(doc) -> [""];
multiple_relative_product(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),
    ?line T = relation([{a,1},{a,11},{b,2},{c,3},{c,33},{d,4}]), 
    ?line {'EXIT', {badarg, _}} = 
        (catch multiple_relative_product({}, ER)),
    ?line {'EXIT', {badarg, _}} = 
	(catch multiple_relative_product({}, relation([{a,b}]))),
    ?line eval(multiple_relative_product({E,T,T}, relation([], 3)), E),
    ?line eval(multiple_relative_product({T,T,T}, E), E),
    ?line eval(multiple_relative_product({T,T,T}, relation([],3)), 
               from_term([],[{{atom,atom,atom},{atom,atom,atom}}])),
    ?line eval(multiple_relative_product({T,T,T}, 
                                   relation([{a,b,c},{c,d,a}])),
               from_term([{{a,b,c},{1,2,3}}, {{a,b,c},{1,2,33}},
                          {{a,b,c},{11,2,3}}, {{a,b,c},{11,2,33}},
                          {{c,d,a},{3,4,1}}, {{c,d,a},{3,4,11}},
                          {{c,d,a},{33,4,1}}, {{c,d,a},{33,4,11}}])),
    ?line {'EXIT', {type_mismatch, _}} =
	(catch multiple_relative_product({T}, from_term([{{a}}]))), 
    ok.

digraph(suite) -> [];
digraph(doc) -> [""];
digraph(Conf) when is_list(Conf) ->
    ?line T0 = ets:all(),
    ?line E = empty_set(),
    ?line R = relation([{a,b},{b,c},{c,d},{d,a}]),
    ?line F = relation_to_family(R),
    Type = type(F),

    ?line {'EXIT', {badarg, _}} = 
        (catch family_to_digraph(set([a]))),
    digraph_fail(badarg, catch family_to_digraph(set([a]), [foo])),
    digraph_fail(badarg, catch family_to_digraph(F, [foo])),
    digraph_fail(cyclic, catch family_to_digraph(family([{a,[a]}]),[acyclic])),

    ?line G1 = family_to_digraph(E),
    ?line {'EXIT', {badarg, _}} = (catch digraph_to_family(G1, foo)),
    ?line {'EXIT', {badarg, _}} = (catch digraph_to_family(G1, atom)),
    ?line true = [] == to_external(digraph_to_family(G1)),
    ?line true = [] == to_external(digraph_to_family(G1, Type)),
    ?line true = digraph:delete(G1),

    ?line G1a = family_to_digraph(E, [protected]),
    ?line true = [] == to_external(digraph_to_family(G1a)),
    ?line true = [] == to_external(digraph_to_family(G1a, Type)),
    ?line true = digraph:delete(G1a),

    ?line G2 = family_to_digraph(F),
    ?line true = F == digraph_to_family(G2),
    ?line true = F == digraph_to_family(G2, type(F)),
    ?line true = digraph:delete(G2),

    ?line R2 = from_term([{{a},b},{{c},d}]),
    ?line F2 = relation_to_family(R2),
    ?line Type2 = type(F2),
    ?line G3 = family_to_digraph(F2, [protected]),
    ?line true = is_subset(F2, digraph_to_family(G3, Type2)),
    ?line true = digraph:delete(G3),

    Fl = 0.0, I = round(Fl),
    if
        Fl == I -> % term ordering
            ?line G4 = digraph:new(),
            digraph:add_vertex(G4, Fl),
            digraph:add_vertex(G4, I),
            ?line {'EXIT', {badarg, _}} = 
                (catch digraph_to_family(G4, Type)),
            ?line {'EXIT', {badarg, _}} = 
                (catch digraph_to_family(G4)),
            ?line true = digraph:delete(G4);
        true -> ok
    end,
    
    ?line true = T0 == ets:all(),
    ok.

digraph_fail(ExitReason, Fail) ->
    {'EXIT', {ExitReason, [{sofs,family_to_digraph,A,_}|_]}} = Fail,
    case {test_server:is_native(sofs),A} of
	{false,[_,_]} -> ok;
	{true,2} -> ok
    end.

constant_function(suite) -> [];
constant_function(doc) -> [""];
constant_function(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line C = from_term(3),
    ?line eval(constant_function(E, C), E),
    ?line eval(constant_function(set([a,b]), E), from_term([{a,[]},{b,[]}])),
    ?line eval(constant_function(set([a,b]), C), from_term([{a,3},{b,3}])),
    ?line {'EXIT', {badarg, _}} = (catch constant_function(C, C)),
    ?line {'EXIT', {badarg, _}} = (catch constant_function(set([]), foo)),
    ok.

misc(suite) -> [];
misc(doc) -> [""];
misc(Conf) when is_list(Conf) ->
    % find "relational" part of relation:
    ?line S = relation([{a,b},{b,c},{b,d},{c,d}]),
    Id = fun(A) -> A end,
    ?line RR = relational_restriction(S),
    ?line eval(union(difference(partition(Id,S), partition(1,S))), RR),
    ?line eval(union(difference(partition(1,S), partition(Id,S))), RR),

    % the "functional" part:
    ?line eval(union(intersection(partition(1,S), partition(Id,S))),
               difference(S, RR)),
    ?line {'EXIT', {undef, _}} =    
        (catch projection(fun external:foo/1, set([a,b,c]))),
    ok.

relational_restriction(R) ->
    Fun = fun(S) -> no_elements(S) > 1 end,
    family_to_relation(family_specification(Fun, relation_to_family(R))).


family_specification(suite) -> [];
family_specification(doc) -> [""];
family_specification(Conf) when is_list(Conf) ->
    E = empty_set(),
    %% internal
    ?line eval(family_specification(fun sofs:is_set/1, E), E),
    ?line {'EXIT', {badarg, _}} =    
       (catch family_specification(fun sofs:is_set/1, set([]))),
    ?line F1 = from_term([{1,[1]}]),
    ?line eval(family_specification(fun sofs:is_set/1, F1), F1),
    Fun = fun(S) -> is_subset(S, set([0,1,2,3,4])) end,
    ?line F2 = family([{a,[1,2]},{b,[3,4,5]}]),
    ?line eval(family_specification(Fun, F2), family([{a,[1,2]}])),
    ?line F3 = from_term([{a,[]},{b,[]}]),
    ?line eval(family_specification(fun sofs:is_set/1, F3), F3),
    Fun2 = fun(_) -> throw(fippla) end, 
    ?line fippla = (catch family_specification(Fun2, family([{a,[1]}]))),
    Fun3 = fun(_) -> neither_true_nor_false end,
    ?line {'EXIT', {badarg, _}} = 
	(catch family_specification(Fun3, F3)),

    %% external
    IsList = {external, fun(L) when is_list(L) -> true; (_) -> false end},
    ?line eval(family_specification(IsList, E), E),
    ?line eval(family_specification(IsList, F1), F1),
    MF = {external, fun(L) -> lists:member(3, L) end},
    ?line eval(family_specification(MF, F2), family([{b,[3,4,5]}])),
    ?line fippla = (catch family_specification(Fun2, family([{a,[1]}]))),
    ?line {'EXIT', {badarg, _}} = 
	(catch family_specification({external, Fun3}, F3)),
    ok.

family_domain_1(suite) -> [];
family_domain_1(doc) -> [""];
family_domain_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = from_term([{a,[]},{b,[]}],[{atom,[{atom,atom}]}]),
    ?line EF = from_term([{a,[]},{b,[]}],[{atom,[atom]}]),
    ?line eval(family_domain(E), E),
    ?line eval(family_domain(ER), EF),
    ?line FR = from_term([{a,[{1,a},{2,b},{3,c}]},{b,[]},{c,[{4,d},{5,e}]}]),
    ?line eval(family_domain(FR), from_term([{a,[1,2,3]},{b,[]},{c,[4,5]}])),
    ?line eval(family_field(E), E),
    ?line eval(family_field(FR), 
               from_term([{a,[a,b,c,1,2,3]},{b,[]},{c,[d,e,4,5]}])),
    ?line eval(family_domain(from_term([{{a},[{{1,[]},c}]}])),
               from_term([{{a},[{1,[]}]}])),
    ?line eval(family_domain(from_term([{{a},[{{1,[a]},c}]}])),
               from_term([{{a},[{1,[a]}]}])),
    ?line eval(family_domain(from_term([{{a},[]}])),
	       from_term([{{a},[]}])),
    ?line eval(family_domain(from_term([], type(FR))), 
               from_term([], [{atom,[atom]}])),
    ?line {'EXIT', {badarg, _}} = (catch family_domain(set([a]))),
    ?line {'EXIT', {badarg, _}} = (catch family_field(set([a]))),
    ?line {'EXIT', {badarg, _}} = (catch family_domain(set([{a,[b]}]))),
    ok.

family_range_1(suite) -> [];
family_range_1(doc) -> [""];
family_range_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = from_term([{a,[]},{b,[]}],[{atom,[{atom,atom}]}]),
    ?line EF = from_term([{a,[]},{b,[]}],[{atom,[atom]}]),
    ?line eval(family_range(E), E),
    ?line eval(family_range(ER), EF),
    ?line FR = from_term([{a,[{1,a},{2,b},{3,c}]},{b,[]},{c,[{4,d},{5,e}]}]),
    ?line eval(family_range(FR), from_term([{a,[a,b,c]},{b,[]},{c,[d,e]}])),
    ?line eval(family_range(from_term([{{a},[{c,{1,[a]}}]}])),
               from_term([{{a},[{1,[a]}]}])),
    ?line eval(family_range(from_term([{{a},[{c,{1,[]}}]}])),
               from_term([{{a},[{1,[]}]}])),
    ?line eval(family_range(from_term([{{a},[]}])),
	       from_term([{{a},[]}])),
    ?line eval(family_range(from_term([], type(FR))), 
               from_term([], [{atom,[atom]}])),
    ?line {'EXIT', {badarg, _}} = (catch family_range(set([a]))),
    ?line {'EXIT', {badarg, _}} = (catch family_range(set([{a,[b]}]))),
    ok.

family_to_relation_1(suite) -> [];
family_to_relation_1(doc) -> [""];
family_to_relation_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line ER = relation([]),
    ?line EF = family([]),
    ?line eval(family_to_relation(E), E),
    ?line eval(family_to_relation(EF), ER),
    ?line eval(sofs:fam2rel(EF), ER),
    ?line F = family([{a,[]},{b,[1]},{c,[7,9,11]}]),
    ?line eval(family_to_relation(F), relation([{b,1},{c,7},{c,9},{c,11}])),
    ?line {'EXIT', {badarg, _}} = (catch family_to_relation(set([a]))),
    ok.

union_of_family_1(suite) -> [];
union_of_family_1(doc) -> [""];
union_of_family_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line EF = from_term([{a,[]},{b,[]}],[{atom,[atom]}]),
    ?line eval(union_of_family(E), E),
    ?line eval(union_of_family(EF), set([])),
    ?line eval(union_of_family(family([])), set([])),
    ?line FR = from_term([{a,[1,2,3]},{b,[]},{c,[4,5]}]),
    ?line eval(union_of_family(FR), set([1,2,3,4,5])),
    ?line eval(union_of_family(sofs:family([{a,[1,2]},{b,[1,2]}])),
	       set([1,2])),
    ?line {'EXIT', {badarg, _}} = (catch union_of_family(set([a]))),
    ok.

intersection_of_family_1(suite) -> [];
intersection_of_family_1(doc) -> [""];
intersection_of_family_1(Conf) when is_list(Conf) ->
    ?line EF = from_term([{a,[]},{b,[]}],[{atom,[atom]}]),
    ?line eval(intersection_of_family(EF), set([])),
    ?line FR = from_term([{a,[1,2,3]},{b,[2,3]},{c,[3,4,5]}]),
    ?line eval(intersection_of_family(FR), set([3])),
    ?line {'EXIT', {badarg, _}} = 
        (catch intersection_of_family(family([]))),
    ?line EE = from_term([], [[atom]]),
    ?line {'EXIT', {badarg, _}} = (catch intersection_of_family(EE)),
    ?line {'EXIT', {badarg, _}} = (catch intersection_of_family(set([a]))),
    ok.

family_projection(suite) -> [];
family_projection(doc) -> [""];
family_projection(Conf) when is_list(Conf) ->
    SSType = [{atom,[[atom]]}],
    SRType = [{atom,[{atom,atom}]}],
    ?line E = empty_set(),

    ?line eval(family_projection(fun(X) -> X end, family([])), E),
    ?line L1 = [{a,[]}],
    ?line eval(family_projection(fun sofs:union/1, E), E),
    ?line eval(family_projection(fun sofs:union/1, from_term(L1, SSType)),
               family(L1)),
    ?line {'EXIT', {badarg, _}} =    
        (catch family_projection(fun sofs:union/1, set([]))),
    ?line {'EXIT', {badarg, _}} =
        (catch family_projection(fun sofs:union/1, from_term([{1,[1]}]))),

    ?line F2 = from_term([{a,[[1],[2]]},{b,[[3,4],[5]]}], SSType),
    ?line eval(family_projection(fun sofs:union/1, F2),
               family_union(F2)),

    ?line F3 = from_term([{1,[{a,b},{b,c},{c,d}]},{3,[]},{5,[{3,5}]}],
                         SRType),
    ?line eval(family_projection(fun sofs:domain/1, F3), family_domain(F3)),
    ?line eval(family_projection(fun sofs:range/1, F3), family_range(F3)),

    ?line eval(family_projection(fun(_) -> E end, family([{a,[b,c]}])),
	       from_term([{a,[]}])),

    Fun1 = fun(S) ->
                   case to_external(S) of
                       [1] -> from_term({1,1});
                       _ -> S
                   end
           end,
    ?line eval(family_projection(Fun1, family([{a,[1]}])), 
	       from_term([{a,{1,1}}])),
    Fun2 = fun(_) -> throw(fippla) end, 
    ?line fippla = 
        (catch family_projection(Fun2, family([{a,[1]}]))),
    ?line {'EXIT', {type_mismatch, _}} =
        (catch family_projection(Fun1, from_term([{1,[1]},{2,[2]}]))),
    ?line {'EXIT', {type_mismatch, _}} =
        (catch family_projection(Fun1, from_term([{1,[1]},{0,[0]}]))),

    ?line eval(family_projection(fun(_) -> E end, from_term([{a,[]}])),
	       from_term([{a,[]}])),
    F4 = from_term([{a,[{1,2,3}]},{b,[{4,5,6}]},{c,[]},{m3,[]}]),
    Z = from_term(0),
    ?line eval(family_projection(fun(S) -> local_adjoin(S, Z) end, F4),
	      from_term([{a,[{{1,2,3},0}]},{b,[{{4,5,6},0}]},{c,[]},{m3,[]}])),
    ?line {'EXIT', {badarg, _}} =
        (catch family_projection({external, fun(X) -> X end}, 
				 from_term([{1,[1]}]))),

    %% ordered set element
    ?line eval(family_projection(fun(_) -> from_term(a, atom) end, 
				 from_term([{1,[a]}])),
	       from_term([{1,a}])),
    ok.

family_difference(suite) -> [];
family_difference(doc) -> [""];
family_difference(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line EF = family([]),
    ?line F9 = from_term([{b,[b,c]}]),
    ?line F10 = from_term([{a,[b,c]}]),
    ?line eval(family_difference(E, E), E),
    ?line eval(family_difference(E, F10), from_term([], type(F10))),
    ?line eval(family_difference(F10, E), F10),
    ?line eval(family_difference(F9, F10), F9),
    ?line eval(family_difference(F10, F10), family([{a,[]}])),
    ?line F20 = from_term([{a,[1,2,3]},{b,[1,2,3]},{c,[1,2,3]}]),
    ?line F21 = from_term([{b,[1,2,3]},{c,[1,2,3]}]),
    ?line eval(family_difference(F20, from_term([{a,[2]}])),
               from_term([{a,[1,3]},{b,[1,2,3]},{c,[1,2,3]}])),
    ?line eval(family_difference(F20, from_term([{0,[2]},{q,[1,2]}])), F20),
    ?line eval(family_difference(F20, F21),
               from_term([{a,[1,2,3]},{b,[]},{c,[]}])),

    ?line eval(family_difference(from_term([{e,[f,g]}]), family([])),
               from_term([{e,[f,g]}])),
    ?line eval(family_difference(from_term([{e,[f,g]}]), EF),
               from_term([{e,[f,g]}])),
    ?line eval(family_difference(from_term([{a,[a,b,c,d]},{c,[b,c]}]),
                                 from_term([{a,[b,c]},{b,[d]},{d,[e,f]}])),
               from_term([{a,[a,d]},{c,[b,c]}])),
    ?line {'EXIT', {badarg, _}} = 
	(catch family_difference(set([]), set([]))),
    ?line {'EXIT', {type_mismatch, _}} =    
	(catch family_difference(from_term([{a,[b,c]}]),
                                 from_term([{e,[{f}]}]))),
    ?line {'EXIT', {type_mismatch, _}} =    
	(catch family_difference(from_term([{a,[b]}]),
                                 from_term([{c,[d]}], [{i,[s]}]))),
    ok.

family_intersection_1(suite) -> [];
family_intersection_1(doc) -> [""];
family_intersection_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line EF = family([]),
    ?line ES = from_term([], [{atom,[[atom]]}]),
    ?line eval(family_intersection(E), E),
    ?line {'EXIT', {badarg, _}} = (catch family_intersection(EF)),
    ?line eval(family_intersection(ES), EF),
    ?line {'EXIT', {badarg, _}} = (catch family_intersection(set([]))),
    ?line {'EXIT', {badarg, _}} = 
        (catch family_intersection(from_term([{a,[1,2]}]))),
    ?line F1 = from_term([{a,[[1],[2],[2,3]]},{b,[]},{c,[[4]]}]),
    ?line {'EXIT', {badarg, _}} = (catch family_intersection(F1)),
    ?line F2 = from_term([{b,[[1],[2],[2,3]]},{a,[]},{c,[[4]]}]),
    ?line {'EXIT', {badarg, _}} = (catch family_intersection(F2)),
    ?line F3 = from_term([{a,[[1,2,3],[2],[2,3]]},{c,[[4,5,6],[5,6,7]]}]),
    ?line eval(family_intersection(F3), family([{a,[2]},{c,[5,6]}])),
    ok.

family_intersection_2(suite) -> [];
family_intersection_2(doc) -> [""];
family_intersection_2(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line EF = family([]),
    ?line F1 = from_term([{a,[1,2]},{b,[4,5]},{c,[7,8]},{d,[10,11]}]),
    ?line F2 = from_term([{c,[6,7]},{d,[9,10,11]},{q,[1]}]),
    ?line F3 = from_term([{a,[1,2]},{b,[4,5]},{c,[6,7,8]},{d,[9,10,11]},
                          {q,[1]}]),

    ?line eval(family_intersection(E, E), E),
    ?line eval(family_intersection(EF, EF), EF),
    ?line eval(family_intersection(F1, F2), 
               from_term([{c,[7]},{d,[10,11]}])),
    ?line eval(family_intersection(F1, F3), F1),
    ?line eval(family_intersection(F2, F3), F2),

    ?line eval(family_intersection(EF, from_term([{e,[f,g]}])), EF),
    ?line eval(family_intersection(E, from_term([{e,[f,g]}])), EF),
    ?line eval(family_intersection(from_term([{e,[f,g]}]), EF), EF),
    ?line eval(family_intersection(from_term([{e,[f,g]}]), E), EF),
    ?line {'EXIT', {type_mismatch, _}} =    
	(catch family_intersection(from_term([{a,[b,c]}]),
                                   from_term([{e,[{f}]}]))),

    ?line F11 = family([{a,[1,2,3]},{b,[0,2,4]},{c,[0,3,6,9]}]),
    ?line eval(union_of_family(F11), set([0,1,2,3,4,6,9])),
    ?line F12 = from_term([{a,[1,2,3,4]},{b,[0,2,4]},{c,[2,3,4,5]}]),
    ?line eval(intersection_of_family(F12), set([2,4])),
    ok.

family_union_1(suite) -> [];
family_union_1(doc) -> [""];
family_union_1(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line EF = family([]),
    ?line ES = from_term([], [{atom,[[atom]]}]),
    ?line eval(family_union(E), E),
    ?line eval(family_union(ES), EF),
    ?line {'EXIT', {badarg, _}} = (catch family_union(set([]))),
    ?line {'EXIT', {badarg, _}} = 
        (catch family_union(from_term([{a,[1,2]}]))),
    ?line eval(family_union(from_term([{a,[[1],[2],[2,3]]},{b,[]},{c,[[4]]}])),
               family([{a,[1,2,3]},{b,[]},{c,[4]}])),
    ok.

family_union_2(suite) -> [];
family_union_2(doc) -> [""];
family_union_2(Conf) when is_list(Conf) ->
    ?line E = empty_set(),
    ?line EF = family([]),
    ?line F1 = from_term([{a,[1,2]},{b,[4,5]},{c,[7,8]},{d,[10,11]}]),
    ?line F2 = from_term([{c,[6,7]},{d,[9,10,11]},{q,[1]}]),
    ?line F3 = from_term([{a,[1,2]},{b,[4,5]},{c,[6,7,8]},{d,[9,10,11]},
                          {q,[1]}]),

    ?line eval(family_union(E, E), E),
    ?line eval(family_union(F1, E), F1),
    ?line eval(family_union(E, F2), F2),
    ?line eval(family_union(F1, F2), F3),
    ?line eval(family_union(F2, F1), F3),

    ?line eval(family_union(E, from_term([{e,[f,g]}])),
               from_term([{e,[f,g]}])),
    ?line eval(family_union(EF, from_term([{e,[f,g]}])),
               from_term([{e,[f,g]}])),
    ?line eval(family_union(from_term([{e,[f,g]}]), E), 
               from_term([{e,[f,g]}])),
    ?line {'EXIT', {badarg, _}} = 
	(catch family_union(set([]),set([]))),
    ?line {'EXIT', {type_mismatch, _}} =    
	(catch family_union(from_term([{a,[b,c]}]), 
                            from_term([{e,[{f}]}]))),
    ok.

partition_family(suite) -> [];
partition_family(doc) -> [""];
partition_family(Conf) when is_list(Conf) ->
    ?line E = empty_set(),

    %% set of ordered sets
    ?line ER = relation([]),
    ?line EF = from_term([], [{atom,[{atom,atom}]}]),

    ?line eval(partition_family(1, E), E),
    ?line eval(partition_family(2, E), E),
    ?line eval(partition_family(fun sofs:union/1, E), E),
    ?line eval(partition_family(1, ER), EF),
    ?line eval(partition_family(2, ER), EF),
    ?line {'EXIT', {badarg, _}} = (catch partition_family(1, set([]))),
    ?line {'EXIT', {badarg, _}} = (catch partition_family(2, set([]))),
    ?line {'EXIT', {function_clause, _}} =
	(catch partition_family(fun({_A,B}) -> {B} end, from_term([{1}]))),
    ?line eval(partition_family(1, relation([{1,a},{1,b},{2,c},{2,d}])), 
	       from_term([{1,[{1,a},{1,b}]},{2,[{2,c},{2,d}]}])),
    ?line eval(partition_family(1, relation([{1,a},{2,b}])),
	       from_term([{1,[{1,a}]},{2,[{2,b}]}])),
    ?line eval(partition_family(2, relation([{1,a},{1,b},{2,a},{2,b},{3,c}])), 
	       from_term([{a,[{1,a},{2,a}]},{b,[{1,b},{2,b}]},{c,[{3,c}]}])),
    ?line eval(partition_family(2, relation([{1,a}])), 
	       from_term([{a,[{1,a}]}])),
    ?line eval(partition_family(2, relation([{1,a},{2,a},{3,a}])), 
	       from_term([{a,[{1,a},{2,a},{3,a}]}])),
    ?line eval(partition_family(2, relation([{1,a},{2,b}])),
	       from_term([{a,[{1,a}]},{b,[{2,b}]}])),
    ?line F13 = from_term([{a,b,c},{a,b,d},{b,b,c},{a,c,c},{a,c,d},{b,c,c}]),
    ?line eval(partition_family(2, F13),
               from_term([{b,[{a,b,c},{a,b,d},{b,b,c}]},
                          {c,[{a,c,c},{a,c,d},{b,c,c}]}])),

    Fun1 = {external, fun({A,_B}) -> {A} end}, 
    ?line eval(partition_family(Fun1, relation([{a,1},{a,2},{b,3}])),
	       from_term([{{a},[{a,1},{a,2}]},{{b},[{b,3}]}])),
    Fun2 = fun(S) -> {A,_B} = to_external(S), from_term({A}) end,
    ?line eval(partition_family(Fun2, relation([{a,1},{a,2},{b,3}])),
	       from_term([{{a},[{a,1},{a,2}]},{{b},[{b,3}]}])),

    ?line {'EXIT', {badarg, _}} = 
	(catch partition_family({external, fun({A,_}) -> {A,0} end}, 
				from_term([{1,a}]))),
    ?line [{{atom,atom},[{atom,atom,atom,atom}]}] = 
	type(partition_family({external, fun({A,_B,C,_D}) -> {C,A} end}, 
			      relation([],4))),

    Fun3 = fun(S) -> from_term({to_external(S),0}, {type(S),atom}) end,
    ?line eval(partition_family(Fun3, E), E),
    ?line eval(partition_family(Fun3, set([a,b])),
	       from_term([{{a,0},[a]}, {{b,0},[b]}])),
    ?line eval(partition_family(Fun3, relation([{a,1},{b,2}])),
	       from_term([{{{a,1},0},[{a,1}]},{{{b,2},0},[{b,2}]}])),
    ?line eval(partition_family(Fun3, from_term([[a],[b]])),
	       from_term([{{[a],0},[[a]]}, {{[b],0},[[b]]}])),
    ?line partition_family({external, fun(X) -> X end}, E),

    F = 0.0, I = round(F),
    ?line FR = relation([{I,a},{F,b}]),
    if
        F == I -> % term ordering
            ?line true = (1 =:= no_elements(partition_family(1, FR)));
        true -> 
            ?line eval(partition_family(1, FR), 
                       from_term([{I,[{I,a}]},{F,[{F,b}]}]))
    end,
    %% set of sets
    ?line {'EXIT', {badarg, _}} = 
        (catch partition_family({external, fun(X) -> X end}, 
				from_term([], [[atom]]))),
    ?line {'EXIT', {badarg, _}} = 
        (catch partition_family({external, fun(X) -> X end}, 
				from_term([[a]]))),
    ?line eval(partition_family(fun sofs:union/1,
				from_term([[[1],[1,2]], [[1,2]]])),
	       from_term([{[1,2], [[[1],[1,2]],[[1,2]]]}])),
    ?line eval(partition_family(fun(X) -> X end, 
				from_term([[1],[1,2],[1,2,3]])),
	       from_term([{[1],[[1]]},{[1,2],[[1,2]]},{[1,2,3],[[1,2,3]]}])),

    ?line eval(partition_family(fun(_) -> from_term([a]) end, 
                                from_term([], [[atom]])),
	       E),
    Fun10 = fun(S) ->
                   %% Cheating a lot...
                   case to_external(S) of
                       [1] -> from_term({1,1});
                       _ -> S
                   end
           end,

    ?line eval(partition_family(Fun10, from_term([[1]])),
	       from_term([{{1,1},[[1]]}])),
    ?line eval(partition_family(fun(_) -> from_term({a}) end, 
                                from_term([[a]])),
	       from_term([{{a},[[a]]}])),
    ?line {'EXIT', {badarg, _}} = 
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

