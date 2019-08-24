%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2017. All Rights Reserved.
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
-module(sofs).

-export([from_term/1, from_term/2, from_external/2, empty_set/0,
         is_type/1, set/1, set/2, from_sets/1, relation/1, relation/2,
         a_function/1, a_function/2, family/1, family/2,
         to_external/1, type/1, to_sets/1, no_elements/1,
         specification/2, union/2, intersection/2, difference/2,
         symdiff/2, symmetric_partition/2, product/1, product/2,
         constant_function/2, is_equal/2, is_subset/2, is_sofs_set/1,
         is_set/1, is_empty_set/1, is_disjoint/2]).

-export([union/1, intersection/1, canonical_relation/1]).

-export([relation_to_family/1, domain/1, range/1, field/1,
	 relative_product/1, relative_product/2, relative_product1/2,
	 converse/1, image/2, inverse_image/2, strict_relation/1,
	 weak_relation/1, extension/3, is_a_function/1]).

-export([composite/2, inverse/1]).

-export([restriction/2, restriction/3, drestriction/2, drestriction/3,
         substitution/2, projection/2, partition/1, partition/2,
         partition/3, multiple_relative_product/2, join/4]).

-export([family_to_relation/1, family_specification/2,
         union_of_family/1, intersection_of_family/1,
         family_union/1, family_intersection/1,
         family_domain/1, family_range/1, family_field/1,
         family_union/2, family_intersection/2, family_difference/2,
         partition_family/2, family_projection/2]).

-export([family_to_digraph/1, family_to_digraph/2,
         digraph_to_family/1, digraph_to_family/2]).

%% Shorter names of some functions.
-export([fam2rel/1, rel2fam/1]).

-import(lists,
        [any/2, append/1, flatten/1, foreach/2,
         keysort/2, last/1, map/2, mapfoldl/3, member/2, merge/2,
         reverse/1, reverse/2, sort/1, umerge/1, umerge/2, usort/1]).

-compile({inline, [{family_to_relation,1}, {relation_to_family,1}]}).

-compile({inline, [{rel,2},{a_func,2},{fam,2},{term2set,2}]}).

-compile({inline, [{external_fun,1},{element_type,1}]}).

-compile({inline,
          [{unify_types,2}, {match_types,2},
           {test_rel,3}, {symdiff,3},
           {subst,3}]}).

-compile({inline, [{fam_binop,3}]}).

%% Nope, no is_member, del_member or add_member.
%%
%% See also "Naive Set Theory" by Paul R. Halmos.
%%
%% By convention, erlang:error/1 is called from exported functions.

-define(TAG, 'Set').
-define(ORDTAG, 'OrdSet').

-record(?TAG, {data = [] :: list(), type = type :: term()}).
-record(?ORDTAG, {orddata = {} :: tuple() | atom(),
                  ordtype = type :: term()}).

-define(LIST(S), (S)#?TAG.data).
-define(TYPE(S), (S)#?TAG.type).
-define(SET(L, T), #?TAG{data = L, type = T}).
-define(IS_SET(S), is_record(S, ?TAG)).
-define(IS_UNTYPED_SET(S), ?TYPE(S) =:= ?ANYTYPE).

%% Ordered sets and atoms:
-define(ORDDATA(S), (S)#?ORDTAG.orddata).
-define(ORDTYPE(S), (S)#?ORDTAG.ordtype).
-define(ORDSET(L, T), #?ORDTAG{orddata = L, ordtype = T}).
-define(IS_ORDSET(S), is_record(S, ?ORDTAG)).
-define(ATOM_TYPE, atom).
-define(IS_ATOM_TYPE(T), is_atom(T)). % true for ?ANYTYPE...

%% When IS_SET is true:
-define(ANYTYPE, '_').
-define(BINREL(X, Y), {X, Y}).
-define(IS_RELATION(R), is_tuple(R)).
-define(REL_ARITY(R), tuple_size(R)).
-define(REL_TYPE(I, R), element(I, R)).
-define(SET_OF(X), [X]).
-define(IS_SET_OF(X), is_list(X)).
-define(FAMILY(X, Y), ?BINREL(X, ?SET_OF(Y))).

-export_type([anyset/0, binary_relation/0, external_set/0, a_function/0,
              family/0, relation/0, set_of_sets/0, set_fun/0, spec_fun/0,
              type/0]).
-export_type([ordset/0, a_set/0]).

-type(anyset() :: ordset() | a_set()).
-type(binary_relation() :: relation()).
-type(external_set() :: term()).
-type(a_function() :: relation()).
-type(family() :: a_function()).
-opaque(ordset() :: #?ORDTAG{}).
-type(relation() :: a_set()).
-opaque(a_set() :: #?TAG{}).
-type(set_of_sets() :: a_set()).
-type(set_fun() :: pos_integer()
                 | {external, fun((external_set()) -> external_set())}
                 | fun((anyset()) -> anyset())).
-type(spec_fun() :: {external, fun((external_set()) -> boolean())}
                  | fun((anyset()) -> boolean())).
-type(type() :: term()).

-type(tuple_of(_T) :: tuple()).

%%
%%  Exported functions
%%

%%%
%%% Create sets
%%%

-spec(from_term(Term) -> AnySet when
      AnySet :: anyset(),
      Term :: term()).
from_term(T) ->
    Type = case T of
               _ when is_list(T) -> [?ANYTYPE];
               _ -> ?ANYTYPE
           end,
    try setify(T, Type)
    catch _:_ -> erlang:error(badarg)
    end.

-spec(from_term(Term, Type) -> AnySet when
      AnySet :: anyset(),
      Term :: term(),
      Type :: type()).
from_term(L, T) ->
    case is_type(T) of
        true ->
            try setify(L, T)
            catch _:_ -> erlang:error(badarg)
            end;
        false  ->
            erlang:error(badarg)
    end.

-spec(from_external(ExternalSet, Type) -> AnySet when
      ExternalSet :: external_set(),
      AnySet :: anyset(),
      Type :: type()).
from_external(L, ?SET_OF(Type)) ->
    ?SET(L, Type);
from_external(T, Type) ->
    ?ORDSET(T, Type).

-spec(empty_set() -> Set when
      Set :: a_set()).
empty_set() ->
    ?SET([], ?ANYTYPE).

-spec(is_type(Term) -> Bool when
      Bool :: boolean(),
      Term :: term()).
is_type(Atom) when ?IS_ATOM_TYPE(Atom), Atom =/= ?ANYTYPE ->
    true;
is_type(?SET_OF(T)) ->
    is_element_type(T);
is_type(T) when tuple_size(T) > 0 ->
    is_types(tuple_size(T), T);
is_type(_T) ->
    false.

-spec(set(Terms) -> Set when
      Set :: a_set(),
      Terms :: [term()]).
set(L) ->
    try usort(L) of
        SL -> ?SET(SL, ?ATOM_TYPE)
    catch _:_ -> erlang:error(badarg)
    end.

-spec(set(Terms, Type) -> Set when
      Set :: a_set(),
      Terms :: [term()],
      Type :: type()).
set(L, ?SET_OF(Type)) when ?IS_ATOM_TYPE(Type), Type =/= ?ANYTYPE ->
    try usort(L) of
        SL -> ?SET(SL, Type)
    catch _:_ -> erlang:error(badarg)
    end;
set(L, ?SET_OF(_) = T) ->
    try setify(L, T)
    catch _:_ -> erlang:error(badarg)
    end;
set(_, _) ->
    erlang:error(badarg).

-spec(from_sets(ListOfSets) -> Set when
      Set :: a_set(),
      ListOfSets :: [anyset()];
               (TupleOfSets) -> Ordset when
      Ordset :: ordset(),
      TupleOfSets :: tuple_of(anyset())).
from_sets(Ss) when is_list(Ss) ->
    case set_of_sets(Ss, [], ?ANYTYPE) of
        {error, Error} ->
            erlang:error(Error);
        Set ->
            Set
    end;
from_sets(Tuple) when is_tuple(Tuple) ->
    case ordset_of_sets(tuple_to_list(Tuple), [], []) of
        error ->
            erlang:error(badarg);
        Set ->
            Set
    end;
from_sets(_) ->
    erlang:error(badarg).

-spec(relation(Tuples) -> Relation when
      Relation :: relation(),
      Tuples :: [tuple()]).
relation([]) ->
    ?SET([], ?BINREL(?ATOM_TYPE, ?ATOM_TYPE));
relation(Ts = [T | _]) when is_tuple(T) ->
    try rel(Ts, tuple_size(T))
    catch _:_ -> erlang:error(badarg)
    end;
relation(_) ->
    erlang:error(badarg).

-spec(relation(Tuples, Type) -> Relation when
      N :: integer(),
      Type :: N | type(),
      Relation :: relation(),
      Tuples :: [tuple()]).
relation(Ts, TS) ->
    try rel(Ts, TS)
    catch _:_ -> erlang:error(badarg)
    end.

-spec(a_function(Tuples) -> Function when
      Function :: a_function(),
      Tuples :: [tuple()]).
a_function(Ts) ->
    try func(Ts, ?BINREL(?ATOM_TYPE, ?ATOM_TYPE)) of
        Bad when is_atom(Bad) ->
            erlang:error(Bad);
        Set ->
            Set
    catch _:_ -> erlang:error(badarg)
    end.

-spec(a_function(Tuples, Type) -> Function when
      Function :: a_function(),
      Tuples :: [tuple()],
      Type :: type()).
a_function(Ts, T) ->
    try a_func(Ts, T) of
	Bad when is_atom(Bad) ->
	    erlang:error(Bad);
	Set ->
	    Set
    catch _:_ -> erlang:error(badarg)
    end.

-spec(family(Tuples) -> Family when
      Family :: family(),
      Tuples :: [tuple()]).
family(Ts) ->
    try fam2(Ts, ?FAMILY(?ATOM_TYPE, ?ATOM_TYPE)) of
        Bad when is_atom(Bad) ->
            erlang:error(Bad);
        Set ->
	    Set
    catch _:_ -> erlang:error(badarg)
    end.

-spec(family(Tuples, Type) -> Family when
      Family :: family(),
      Tuples :: [tuple()],
      Type :: type()).
family(Ts, T) ->
    try fam(Ts, T) of
	Bad when is_atom(Bad) ->
	    erlang:error(Bad);
	Set ->
	    Set
    catch _:_ -> erlang:error(badarg)
    end.

%%%
%%% Functions on sets.
%%%

-spec(to_external(AnySet) -> ExternalSet when
      ExternalSet :: external_set(),
      AnySet :: anyset()).
to_external(S) when ?IS_SET(S) ->
    ?LIST(S);
to_external(S) when ?IS_ORDSET(S) ->
    ?ORDDATA(S).

-spec(type(AnySet) -> Type when
      AnySet :: anyset(),
      Type :: type()).
type(S) when ?IS_SET(S) ->
    ?SET_OF(?TYPE(S));
type(S) when ?IS_ORDSET(S) ->
    ?ORDTYPE(S).

-spec(to_sets(ASet) -> Sets when
      ASet :: a_set() | ordset(),
      Sets :: tuple_of(AnySet) | [AnySet],
      AnySet :: anyset()).
to_sets(S) when ?IS_SET(S) ->
    case ?TYPE(S) of
        ?SET_OF(Type) -> list_of_sets(?LIST(S), Type, []);
        Type -> list_of_ordsets(?LIST(S), Type, [])
    end;
to_sets(S) when ?IS_ORDSET(S), is_tuple(?ORDTYPE(S)) ->
    tuple_of_sets(tuple_to_list(?ORDDATA(S)), tuple_to_list(?ORDTYPE(S)), []);
to_sets(S) when ?IS_ORDSET(S) ->
    erlang:error(badarg).

-spec(no_elements(ASet) -> NoElements when
      ASet :: a_set() | ordset(),
      NoElements :: non_neg_integer()).
no_elements(S) when ?IS_SET(S) ->
    length(?LIST(S));
no_elements(S) when ?IS_ORDSET(S), is_tuple(?ORDTYPE(S)) ->
    tuple_size(?ORDDATA(S));
no_elements(S) when ?IS_ORDSET(S) ->
    erlang:error(badarg).

-spec(specification(Fun, Set1) -> Set2 when
      Fun :: spec_fun(),
      Set1 :: a_set(),
      Set2 :: a_set()).
specification(Fun, S) when ?IS_SET(S) ->
    Type = ?TYPE(S),
    R = case external_fun(Fun) of
	    false ->
		spec(?LIST(S), Fun, element_type(Type), []);
	    XFun ->
		specification(?LIST(S), XFun, [])
	end,
    case R of
	SL when is_list(SL) ->
	    ?SET(SL, Type);
	Bad ->
	    erlang:error(Bad)
    end.

-spec(union(Set1, Set2) -> Set3 when
      Set1 :: a_set(),
      Set2 :: a_set(),
      Set3 :: a_set()).
union(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case unify_types(?TYPE(S1), ?TYPE(S2)) of
        [] -> erlang:error(type_mismatch);
        Type ->  ?SET(umerge(?LIST(S1), ?LIST(S2)), Type)
    end.

-spec(intersection(Set1, Set2) -> Set3 when
      Set1 :: a_set(),
      Set2 :: a_set(),
      Set3 :: a_set()).
intersection(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case unify_types(?TYPE(S1), ?TYPE(S2)) of
        [] -> erlang:error(type_mismatch);
        Type ->  ?SET(intersection(?LIST(S1), ?LIST(S2), []), Type)
    end.

-spec(difference(Set1, Set2) -> Set3 when
      Set1 :: a_set(),
      Set2 :: a_set(),
      Set3 :: a_set()).
difference(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case unify_types(?TYPE(S1), ?TYPE(S2)) of
        [] -> erlang:error(type_mismatch);
        Type ->  ?SET(difference(?LIST(S1), ?LIST(S2), []), Type)
    end.

-spec(symdiff(Set1, Set2) -> Set3 when
      Set1 :: a_set(),
      Set2 :: a_set(),
      Set3 :: a_set()).
symdiff(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case unify_types(?TYPE(S1), ?TYPE(S2)) of
        [] -> erlang:error(type_mismatch);
        Type ->  ?SET(symdiff(?LIST(S1), ?LIST(S2), []), Type)
    end.

-spec(symmetric_partition(Set1, Set2) -> {Set3, Set4, Set5} when
      Set1 :: a_set(),
      Set2 :: a_set(),
      Set3 :: a_set(),
      Set4 :: a_set(),
      Set5 :: a_set()).
symmetric_partition(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case unify_types(?TYPE(S1), ?TYPE(S2)) of
        [] -> erlang:error(type_mismatch);
        Type ->  sympart(?LIST(S1), ?LIST(S2), [], [], [], Type)
    end.

-spec(product(Set1, Set2) -> BinRel when
      BinRel :: binary_relation(),
      Set1 :: a_set(),
      Set2 :: a_set()).
product(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    if
        ?TYPE(S1) =:= ?ANYTYPE -> S1;
        ?TYPE(S2) =:= ?ANYTYPE -> S2;
        true ->
	    F = fun(E) -> {0, E} end,
	    T = ?BINREL(?TYPE(S1), ?TYPE(S2)),
	    ?SET(relprod(map(F, ?LIST(S1)), map(F, ?LIST(S2))), T)
    end.

-spec(product(TupleOfSets) -> Relation when
      Relation :: relation(),
      TupleOfSets :: tuple_of(a_set())).
product({S1, S2}) ->
    product(S1, S2);
product(T) when is_tuple(T) ->
    Ss = tuple_to_list(T),
    try sets_to_list(Ss) of
        [] ->
            erlang:error(badarg);
        L ->
            Type = types(Ss, []),
            case member([], L) of
                true ->
		    empty_set();
                false ->
                    ?SET(reverse(prod(L, [], [])), Type)
            end
    catch _:_ -> erlang:error(badarg)
    end.

-spec(constant_function(Set, AnySet) -> Function when
      AnySet :: anyset(),
      Function :: a_function(),
      Set :: a_set()).
constant_function(S, E) when ?IS_SET(S) ->
    case {?TYPE(S), is_sofs_set(E)} of
	{?ANYTYPE, true} -> S;
	{Type, true} ->
	    NType = ?BINREL(Type, type(E)),
	    ?SET(constant_function(?LIST(S), to_external(E), []), NType);
	_ -> erlang:error(badarg)
    end;
constant_function(S, _) when ?IS_ORDSET(S) ->
    erlang:error(badarg).

-spec(is_equal(AnySet1, AnySet2) -> Bool when
      AnySet1 :: anyset(),
      AnySet2 :: anyset(),
      Bool :: boolean()).
is_equal(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case match_types(?TYPE(S1), ?TYPE(S2)) of
        true  -> ?LIST(S1) == ?LIST(S2);
        false -> erlang:error(type_mismatch)
    end;
is_equal(S1, S2) when ?IS_ORDSET(S1), ?IS_ORDSET(S2) ->
    case match_types(?ORDTYPE(S1), ?ORDTYPE(S2)) of
        true  -> ?ORDDATA(S1) == ?ORDDATA(S2);
        false -> erlang:error(type_mismatch)
    end;
is_equal(S1, S2) when ?IS_SET(S1), ?IS_ORDSET(S2) ->
    erlang:error(type_mismatch);
is_equal(S1, S2) when ?IS_ORDSET(S1), ?IS_SET(S2) ->
    erlang:error(type_mismatch).

-spec(is_subset(Set1, Set2) -> Bool when
      Bool :: boolean(),
      Set1 :: a_set(),
      Set2 :: a_set()).
is_subset(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case match_types(?TYPE(S1), ?TYPE(S2)) of
        true  -> subset(?LIST(S1), ?LIST(S2));
        false -> erlang:error(type_mismatch)
    end.

-spec(is_sofs_set(Term) -> Bool when
      Bool :: boolean(),
      Term :: term()).
is_sofs_set(S) when ?IS_SET(S) ->
    true;
is_sofs_set(S) when ?IS_ORDSET(S) ->
    true;
is_sofs_set(_S) ->
    false.

-spec(is_set(AnySet) -> Bool when
      AnySet :: anyset(),
      Bool :: boolean()).
is_set(S) when ?IS_SET(S) ->
    true;
is_set(S) when ?IS_ORDSET(S) ->
    false.

-spec(is_empty_set(AnySet) -> Bool when
      AnySet :: anyset(),
      Bool :: boolean()).
is_empty_set(S) when ?IS_SET(S) ->
    ?LIST(S) =:= [];
is_empty_set(S) when ?IS_ORDSET(S) ->
    false.

-spec(is_disjoint(Set1, Set2) -> Bool when
      Bool :: boolean(),
      Set1 :: a_set(),
      Set2 :: a_set()).
is_disjoint(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case match_types(?TYPE(S1), ?TYPE(S2)) of
        true ->
            case ?LIST(S1) of
                [] -> true;
                [A | As] -> disjoint(?LIST(S2), A, As)
            end;
        false -> erlang:error(type_mismatch)
    end.

%%%
%%% Functions on set-of-sets.
%%%

-spec(union(SetOfSets) -> Set when
      Set :: a_set(),
      SetOfSets :: set_of_sets()).
union(Sets) when ?IS_SET(Sets) ->
    case ?TYPE(Sets) of
        ?SET_OF(Type) -> ?SET(lunion(?LIST(Sets)), Type);
        ?ANYTYPE -> Sets;
        _ -> erlang:error(badarg)
    end.

-spec(intersection(SetOfSets) -> Set when
      Set :: a_set(),
      SetOfSets :: set_of_sets()).
intersection(Sets) when ?IS_SET(Sets) ->
    case ?LIST(Sets) of
        [] -> erlang:error(badarg);
        [L | Ls] ->
            case ?TYPE(Sets) of
                ?SET_OF(Type) ->
                    ?SET(lintersection(Ls, L), Type);
                _ -> erlang:error(badarg)
            end
    end.

-spec(canonical_relation(SetOfSets) -> BinRel when
      BinRel :: binary_relation(),
      SetOfSets :: set_of_sets()).
canonical_relation(Sets) when ?IS_SET(Sets) ->
    ST = ?TYPE(Sets),
    case ST of
        ?SET_OF(?ANYTYPE) -> empty_set();
        ?SET_OF(Type) ->
            ?SET(can_rel(?LIST(Sets), []), ?BINREL(Type, ST));
        ?ANYTYPE -> Sets;
        _ -> erlang:error(badarg)
    end.

%%%
%%% Functions on binary relations only.
%%%

-spec(rel2fam(BinRel) -> Family when
      Family :: family(),
      BinRel :: binary_relation()).
rel2fam(R) ->
    relation_to_family(R).

-spec(relation_to_family(BinRel) -> Family when
      Family :: family(),
      BinRel :: binary_relation()).
%% Inlined.
relation_to_family(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
        ?BINREL(DT, RT) ->
            ?SET(rel2family(?LIST(R)), ?FAMILY(DT, RT));
        ?ANYTYPE -> R;
        _Else    -> erlang:error(badarg)
    end.

-spec(domain(BinRel) -> Set when
      BinRel :: binary_relation(),
      Set :: a_set()).
domain(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
        ?BINREL(DT, _)  -> ?SET(dom(?LIST(R)), DT);
        ?ANYTYPE -> R;
        _Else    -> erlang:error(badarg)
    end.

-spec(range(BinRel) -> Set when
      BinRel :: binary_relation(),
      Set :: a_set()).
range(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
        ?BINREL(_, RT)  -> ?SET(ran(?LIST(R),  []), RT);
        ?ANYTYPE -> R;
        _ -> erlang:error(badarg)
    end.

-spec(field(BinRel) -> Set when
      BinRel :: binary_relation(),
      Set :: a_set()).
%% In "Introduction to LOGIC", Suppes defines the field of a binary
%% relation to be the union of the domain and the range (or
%% counterdomain).
field(R) ->
    union(domain(R), range(R)).

-spec(relative_product(ListOfBinRels) -> BinRel2 when
      ListOfBinRels :: [BinRel, ...],
      BinRel :: binary_relation(),
      BinRel2 :: binary_relation()).
%% The following clause is kept for backward compatibility.
%% The list is due to Dialyzer's specs.
relative_product(RT) when is_tuple(RT) ->
    relative_product(tuple_to_list(RT));
relative_product(RL) when is_list(RL) ->
    case relprod_n(RL, foo, false, false) of
        {error, Reason} ->
            erlang:error(Reason);
        Reply ->
            Reply
    end.

-spec(relative_product(ListOfBinRels, BinRel1) -> BinRel2 when
      ListOfBinRels :: [BinRel, ...],
      BinRel :: binary_relation(),
      BinRel1 :: binary_relation(),
      BinRel2 :: binary_relation();
                      (BinRel1, BinRel2) -> BinRel3 when
      BinRel1 :: binary_relation(),
      BinRel2 :: binary_relation(),
      BinRel3 :: binary_relation()).
relative_product(R1, R2) when ?IS_SET(R1), ?IS_SET(R2) ->
    relative_product1(converse(R1), R2);
%% The following clause is kept for backward compatibility.
%% The list is due to Dialyzer's specs.
relative_product(RT, R) when is_tuple(RT), ?IS_SET(R) ->
    relative_product(tuple_to_list(RT), R);
relative_product(RL, R) when is_list(RL), ?IS_SET(R) ->
    EmptyR = case ?TYPE(R) of
                 ?BINREL(_, _) -> ?LIST(R) =:= [];
                 ?ANYTYPE -> true;
                 _ -> erlang:error(badarg)
             end,
    case relprod_n(RL, R, EmptyR, true) of
        {error, Reason} ->
            erlang:error(Reason);
        Reply ->
            Reply
    end.

-spec(relative_product1(BinRel1, BinRel2) -> BinRel3 when
      BinRel1 :: binary_relation(),
      BinRel2 :: binary_relation(),
      BinRel3 :: binary_relation()).
relative_product1(R1, R2) when ?IS_SET(R1), ?IS_SET(R2) ->
    {DTR1, RTR1} = case ?TYPE(R1) of
                     ?BINREL(_, _) = R1T -> R1T;
                     ?ANYTYPE -> {?ANYTYPE, ?ANYTYPE};
                     _ -> erlang:error(badarg)
                 end,
    {DTR2, RTR2} = case ?TYPE(R2) of
                     ?BINREL(_, _) = R2T -> R2T;
                     ?ANYTYPE -> {?ANYTYPE, ?ANYTYPE};
                     _ -> erlang:error(badarg)
                 end,
    case match_types(DTR1, DTR2) of
        true when DTR1 =:= ?ANYTYPE -> R1;
        true when DTR2 =:= ?ANYTYPE -> R2;
        true -> ?SET(relprod(?LIST(R1), ?LIST(R2)), ?BINREL(RTR1, RTR2));
        false -> erlang:error(type_mismatch)
    end.

-spec(converse(BinRel1) -> BinRel2 when
      BinRel1 :: binary_relation(),
      BinRel2 :: binary_relation()).
converse(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
        ?BINREL(DT, RT) -> ?SET(converse(?LIST(R), []), ?BINREL(RT, DT));
        ?ANYTYPE -> R;
        _ -> erlang:error(badarg)
    end.

-spec(image(BinRel, Set1) -> Set2 when
      BinRel :: binary_relation(),
      Set1 :: a_set(),
      Set2 :: a_set()).
image(R, S) when ?IS_SET(R), ?IS_SET(S) ->
    case ?TYPE(R) of
        ?BINREL(DT, RT) ->
	    case match_types(DT, ?TYPE(S)) of
		true ->
		    ?SET(usort(restrict(?LIST(S), ?LIST(R))), RT);
		false ->
		    erlang:error(type_mismatch)
	    end;
        ?ANYTYPE -> R;
        _ -> erlang:error(badarg)
    end.

-spec(inverse_image(BinRel, Set1) -> Set2 when
      BinRel :: binary_relation(),
      Set1 :: a_set(),
      Set2 :: a_set()).
inverse_image(R, S) when ?IS_SET(R), ?IS_SET(S) ->
    case ?TYPE(R) of
        ?BINREL(DT, RT) ->
	    case match_types(RT, ?TYPE(S)) of
		true ->
		    NL = restrict(?LIST(S), converse(?LIST(R), [])),
		    ?SET(usort(NL), DT);
		false ->
		    erlang:error(type_mismatch)
	    end;
        ?ANYTYPE -> R;
        _ -> erlang:error(badarg)
    end.

-spec(strict_relation(BinRel1) -> BinRel2 when
      BinRel1 :: binary_relation(),
      BinRel2 :: binary_relation()).
strict_relation(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
        Type = ?BINREL(_, _) ->
            ?SET(strict(?LIST(R), []), Type);
        ?ANYTYPE -> R;
        _ -> erlang:error(badarg)
    end.

-spec(weak_relation(BinRel1) -> BinRel2 when
      BinRel1 :: binary_relation(),
      BinRel2 :: binary_relation()).
weak_relation(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
        ?BINREL(DT, RT) ->
            case unify_types(DT, RT) of
                [] ->
                    erlang:error(badarg);
                Type ->
                    ?SET(weak(?LIST(R)), ?BINREL(Type, Type))
            end;
        ?ANYTYPE -> R;
        _ -> erlang:error(badarg)
    end.

-spec(extension(BinRel1, Set, AnySet) -> BinRel2 when
      AnySet :: anyset(),
      BinRel1 :: binary_relation(),
      BinRel2 :: binary_relation(),
      Set :: a_set()).
extension(R, S, E) when ?IS_SET(R), ?IS_SET(S) ->
    case {?TYPE(R), ?TYPE(S), is_sofs_set(E)} of
	{T=?BINREL(DT, RT), ST, true} ->
	    case match_types(DT, ST) and match_types(RT, type(E)) of
		false ->
		    erlang:error(type_mismatch);
		true ->
		    RL = ?LIST(R),
		    case extc([], ?LIST(S), to_external(E), RL) of
			[] ->
			    R;
			L ->
			    ?SET(merge(RL, reverse(L)), T)
		    end
	    end;
	{?ANYTYPE, ?ANYTYPE, true} ->
	    R;
	{?ANYTYPE, ST, true} ->
	    case type(E) of
		?SET_OF(?ANYTYPE) ->
		    R;
		ET ->
		    ?SET([], ?BINREL(ST, ET))
	    end;
	{_, _, true} ->
	    erlang:error(badarg)
    end.

-spec(is_a_function(BinRel) -> Bool when
      Bool :: boolean(),
      BinRel :: binary_relation()).
is_a_function(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
        ?BINREL(_, _) ->
            case ?LIST(R) of
                [] -> true;
                [{V,_} | Es] -> is_a_func(Es, V)
            end;
        ?ANYTYPE -> true;
        _ -> erlang:error(badarg)
    end.

-spec(restriction(BinRel1, Set) -> BinRel2 when
      BinRel1 :: binary_relation(),
      BinRel2 :: binary_relation(),
      Set :: a_set()).
restriction(Relation, Set) ->
    restriction(1, Relation, Set).

-spec(drestriction(BinRel1, Set) -> BinRel2 when
      BinRel1 :: binary_relation(),
      BinRel2 :: binary_relation(),
      Set :: a_set()).
drestriction(Relation, Set) ->
    drestriction(1, Relation, Set).

%%%
%%% Functions on functions only.
%%%

-spec(composite(Function1, Function2) -> Function3 when
      Function1 :: a_function(),
      Function2 :: a_function(),
      Function3 :: a_function()).
composite(Fn1, Fn2) when ?IS_SET(Fn1), ?IS_SET(Fn2) ->
    ?BINREL(DTF1, RTF1) = case ?TYPE(Fn1)of
			      ?BINREL(_, _) = F1T -> F1T;
			      ?ANYTYPE -> {?ANYTYPE, ?ANYTYPE};
			      _ -> erlang:error(badarg)
			  end,
    ?BINREL(DTF2, RTF2) = case ?TYPE(Fn2) of
			      ?BINREL(_, _) = F2T -> F2T;
			      ?ANYTYPE -> {?ANYTYPE, ?ANYTYPE};
			      _ -> erlang:error(badarg)
			  end,
    case match_types(RTF1, DTF2) of
        true when DTF1 =:= ?ANYTYPE -> Fn1;
        true when DTF2 =:= ?ANYTYPE -> Fn2;
        true ->
	    case comp(?LIST(Fn1), ?LIST(Fn2)) of
		SL when is_list(SL) ->
		    ?SET(sort(SL), ?BINREL(DTF1, RTF2));
		Bad ->
		    erlang:error(Bad)
	    end;
        false -> erlang:error(type_mismatch)
    end.

-spec(inverse(Function1) -> Function2 when
      Function1 :: a_function(),
      Function2 :: a_function()).
inverse(Fn) when ?IS_SET(Fn) ->
    case ?TYPE(Fn) of
        ?BINREL(DT, RT) ->
	    case inverse1(?LIST(Fn)) of
		SL when is_list(SL) ->
		    ?SET(SL, ?BINREL(RT, DT));
		Bad ->
		    erlang:error(Bad)
	    end;
        ?ANYTYPE -> Fn;
        _ -> erlang:error(badarg)
    end.

%%%
%%% Functions on relations (binary or other).
%%%

-spec(restriction(SetFun, Set1, Set2) -> Set3 when
      SetFun :: set_fun(),
      Set1 :: a_set(),
      Set2 :: a_set(),
      Set3 :: a_set()).
%% Equivalent to range(restriction(inverse(substitution(Fun, S1)), S2)).
restriction(I, R, S) when is_integer(I), ?IS_SET(R), ?IS_SET(S) ->
    RT = ?TYPE(R),
    ST = ?TYPE(S),
    case check_for_sort(RT, I) of
	empty ->
	    R;
	error ->
	    erlang:error(badarg);
	Sort ->
	    RL = ?LIST(R),
	    case {match_types(?REL_TYPE(I, RT), ST), ?LIST(S)} of
		{true, _SL} when RL =:= [] ->
		    R;
		{true, []} ->
                    ?SET([], RT);
		{true, [E | Es]} when Sort =:= false -> % I =:= 1
		    ?SET(reverse(restrict_n(I, RL, E, Es, [])), RT);
		{true, [E | Es]} ->
		    ?SET(sort(restrict_n(I, keysort(I, RL), E, Es, [])), RT);
		{false, _SL} ->
		    erlang:error(type_mismatch)
	    end
    end;
restriction(SetFun, S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    Type1 = ?TYPE(S1),
    Type2 = ?TYPE(S2),
    SL1 = ?LIST(S1),
    case external_fun(SetFun) of
	false when Type2 =:= ?ANYTYPE ->
	    S2;
	false ->
	    case subst(SL1, SetFun, element_type(Type1)) of
		{NSL, NewType} -> % NewType can be ?ANYTYPE
		    case match_types(NewType, Type2) of
			true ->
			    NL = sort(restrict(?LIST(S2), converse(NSL, []))),
			    ?SET(NL, Type1);
			false ->
			    erlang:error(type_mismatch)
		    end;
		Bad ->
		    erlang:error(Bad)
	    end;
	_ when Type1 =:= ?ANYTYPE ->
	    S1;
	_XFun when ?IS_SET_OF(Type1) ->
            erlang:error(badarg);
	XFun ->
	    FunT = XFun(Type1),
	    try check_fun(Type1, XFun, FunT) of
		Sort ->
		    case match_types(FunT, Type2) of
			true ->
			    R1 = inverse_substitution(SL1, XFun, Sort),
			    ?SET(sort(Sort, restrict(?LIST(S2), R1)), Type1);
			false ->
			    erlang:error(type_mismatch)
		    end
            catch _:_ -> erlang:error(badarg)
	    end
    end.

-spec(drestriction(SetFun, Set1, Set2) -> Set3 when
      SetFun :: set_fun(),
      Set1 :: a_set(),
      Set2 :: a_set(),
      Set3 :: a_set()).
drestriction(I, R, S) when is_integer(I), ?IS_SET(R), ?IS_SET(S) ->
    RT = ?TYPE(R),
    ST = ?TYPE(S),
    case check_for_sort(RT, I) of
	empty ->
	    R;
	error ->
	    erlang:error(badarg);
	Sort ->
	    RL = ?LIST(R),
	    case {match_types(?REL_TYPE(I, RT), ST), ?LIST(S)} of
		{true, []} ->
		    R;
		{true, _SL} when RL =:= [] ->
		    R;
		{true, [E | Es]} when Sort =:= false -> % I =:= 1
		    ?SET(diff_restrict_n(I, RL, E, Es, []), RT);
		{true, [E | Es]} ->
		    ?SET(diff_restrict_n(I, keysort(I, RL), E, Es, []), RT);
		{false, _SL} ->
		    erlang:error(type_mismatch)
	    end
    end;
drestriction(SetFun, S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    Type1 = ?TYPE(S1),
    Type2 = ?TYPE(S2),
    SL1 = ?LIST(S1),
    case external_fun(SetFun) of
	false when Type2 =:= ?ANYTYPE ->
	    S1;
	false ->
	    case subst(SL1, SetFun, element_type(Type1)) of
		{NSL, NewType} -> % NewType can be ?ANYTYPE
		    case match_types(NewType, Type2) of
			true ->
			    SL2 = ?LIST(S2),
			    NL = sort(diff_restrict(SL2, converse(NSL, []))),
			    ?SET(NL, Type1);
			false ->
			    erlang:error(type_mismatch)
		    end;
		Bad ->
		    erlang:error(Bad)
	    end;
	_ when Type1 =:= ?ANYTYPE ->
	    S1;
	_XFun when ?IS_SET_OF(Type1) ->
            erlang:error(badarg);
	XFun ->
	    FunT = XFun(Type1),
	    try check_fun(Type1, XFun, FunT) of
		Sort ->
		    case match_types(FunT, Type2) of
			true ->
			    R1 = inverse_substitution(SL1, XFun, Sort),
			    SL2 = ?LIST(S2),
			    ?SET(sort(Sort, diff_restrict(SL2, R1)), Type1);
			false ->
			    erlang:error(type_mismatch)
		    end
            catch _:_ -> erlang:error(badarg)
	    end
    end.

-spec(projection(SetFun, Set1) -> Set2 when
      SetFun :: set_fun(),
      Set1 :: a_set(),
      Set2 :: a_set()).
projection(I, Set) when is_integer(I), ?IS_SET(Set) ->
    Type = ?TYPE(Set),
    case check_for_sort(Type, I) of
        empty ->
            Set;
        error ->
            erlang:error(badarg);
	_ when I =:= 1 ->
	    ?SET(projection1(?LIST(Set)), ?REL_TYPE(I, Type));
        _ ->
	    ?SET(projection_n(?LIST(Set), I, []), ?REL_TYPE(I, Type))
    end;
projection(Fun, Set) ->
    range(substitution(Fun, Set)).

-spec(substitution(SetFun, Set1) -> Set2 when
      SetFun :: set_fun(),
      Set1 :: a_set(),
      Set2 :: a_set()).
substitution(I, Set) when is_integer(I), ?IS_SET(Set) ->
    Type = ?TYPE(Set),
    case check_for_sort(Type, I) of
	empty ->
	    Set;
	error ->
	    erlang:error(badarg);
	_Sort ->
	    NType = ?REL_TYPE(I, Type),
	    NSL = substitute_element(?LIST(Set), I, []),
	    ?SET(NSL, ?BINREL(Type, NType))
    end;
substitution(SetFun, Set) when ?IS_SET(Set) ->
    Type = ?TYPE(Set),
    L = ?LIST(Set),
    case external_fun(SetFun) of
	false when L =/= [] ->
	    case subst(L, SetFun, element_type(Type)) of
		{SL, NewType} ->
		    ?SET(reverse(SL), ?BINREL(Type, NewType));
		Bad ->
		    erlang:error(Bad)
	    end;
	false ->
	    empty_set();
	_ when Type =:= ?ANYTYPE ->
	    empty_set();
	_XFun when ?IS_SET_OF(Type) ->
            erlang:error(badarg);
	XFun ->
	    FunT = XFun(Type),
	    try check_fun(Type, XFun, FunT) of
		_Sort ->
		    SL = substitute(L, XFun, []),
		    ?SET(SL, ?BINREL(Type, FunT))
            catch _:_ -> erlang:error(badarg)
	    end
    end.

-spec(partition(SetOfSets) -> Partition when
      SetOfSets :: set_of_sets(),
      Partition :: a_set()).
partition(Sets) ->
    F1 = relation_to_family(canonical_relation(Sets)),
    F2 = relation_to_family(converse(F1)),
    range(F2).

-spec(partition(SetFun, Set) -> Partition when
      SetFun :: set_fun(),
      Partition :: a_set(),
      Set :: a_set()).
partition(I, Set) when is_integer(I), ?IS_SET(Set) ->
    Type = ?TYPE(Set),
    case check_for_sort(Type, I) of
        empty ->
            Set;
        error ->
            erlang:error(badarg);
	false -> % I =:= 1
	    ?SET(partition_n(I, ?LIST(Set)), ?SET_OF(Type));
        true ->
	    ?SET(partition_n(I, keysort(I, ?LIST(Set))), ?SET_OF(Type))
    end;
partition(Fun, Set) ->
    range(partition_family(Fun, Set)).

-spec(partition(SetFun, Set1, Set2) -> {Set3, Set4} when
      SetFun :: set_fun(),
      Set1 :: a_set(),
      Set2 :: a_set(),
      Set3 :: a_set(),
      Set4 :: a_set()).
partition(I, R, S) when is_integer(I), ?IS_SET(R), ?IS_SET(S) ->
    RT = ?TYPE(R),
    ST = ?TYPE(S),
    case check_for_sort(RT, I) of
	empty ->
	    {R, R};
	error ->
	    erlang:error(badarg);
	Sort ->
	    RL = ?LIST(R),
	    case {match_types(?REL_TYPE(I, RT), ST), ?LIST(S)} of
		{true, _SL} when RL =:= [] ->
		    {R, R};
		{true, []} ->
		    {?SET([], RT), R};
		{true, [E | Es]} when Sort =:= false -> % I =:= 1
		    [L1 | L2] = partition3_n(I, RL, E, Es, [], []),
		    {?SET(L1, RT), ?SET(L2, RT)};
		{true, [E | Es]} ->
		    [L1 | L2] = partition3_n(I, keysort(I,RL), E, Es, [], []),
		    {?SET(L1, RT), ?SET(L2, RT)};
		{false, _SL} ->
		    erlang:error(type_mismatch)
	    end
    end;
partition(SetFun, S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    Type1 = ?TYPE(S1),
    Type2 = ?TYPE(S2),
    SL1 = ?LIST(S1),
    case external_fun(SetFun) of
	false when Type2 =:= ?ANYTYPE ->
	    {S2, S1};
	false ->
	    case subst(SL1, SetFun, element_type(Type1)) of
		{NSL, NewType} -> % NewType can be ?ANYTYPE
		    case match_types(NewType, Type2) of
			true ->
			    R1 = converse(NSL, []),
			    [L1 | L2] = partition3(?LIST(S2), R1),
			    {?SET(sort(L1), Type1), ?SET(sort(L2), Type1)};
			false ->
			    erlang:error(type_mismatch)
		    end;
		Bad ->
		    erlang:error(Bad)
	    end;
	_ when Type1 =:= ?ANYTYPE ->
	    {S1, S1};
	_XFun when ?IS_SET_OF(Type1) ->
            erlang:error(badarg);
	XFun ->
	    FunT = XFun(Type1),
	    try check_fun(Type1, XFun, FunT) of
		Sort ->
		    case match_types(FunT, Type2) of
			true ->
			    R1 = inverse_substitution(SL1, XFun, Sort),
			    [L1 | L2] = partition3(?LIST(S2), R1),
			    {?SET(sort(L1), Type1), ?SET(sort(L2), Type1)};
			false ->
			    erlang:error(type_mismatch)
		    end
            catch _:_ -> erlang:error(badarg)
	    end
    end.

-spec(multiple_relative_product(TupleOfBinRels, BinRel1) -> BinRel2 when
      TupleOfBinRels :: tuple_of(BinRel),
      BinRel :: binary_relation(),
      BinRel1 :: binary_relation(),
      BinRel2 :: binary_relation()).
multiple_relative_product(T, R) when is_tuple(T), ?IS_SET(R) ->
    case test_rel(R, tuple_size(T), eq) of
	true when ?TYPE(R) =:= ?ANYTYPE ->
	    empty_set();
        true ->
	    MProd = mul_relprod(tuple_to_list(T), 1, R),
	    relative_product(MProd);
        false ->
	    erlang:error(badarg)
    end.

-spec(join(Relation1, I, Relation2, J) -> Relation3 when
      Relation1 :: relation(),
      Relation2 :: relation(),
      Relation3 :: relation(),
      I :: pos_integer(),
      J :: pos_integer()).
join(R1, I1, R2, I2)
  when ?IS_SET(R1), ?IS_SET(R2), is_integer(I1), is_integer(I2) ->
    case test_rel(R1, I1, lte) and test_rel(R2, I2, lte) of
        false -> erlang:error(badarg);
        true when ?TYPE(R1) =:= ?ANYTYPE -> R1;
        true when ?TYPE(R2) =:= ?ANYTYPE -> R2;
        true ->
	    L1 = ?LIST(raise_element(R1, I1)),
	    L2 = ?LIST(raise_element(R2, I2)),
	    T = relprod1(L1, L2),
	    F = case (I1 =:= 1) and (I2 =:= 1)  of
		    true ->
			fun({X,Y}) -> join_element(X, Y) end;
		    false ->
			fun({X,Y}) ->
				list_to_tuple(join_element(X, Y, I2))
			end
		end,
	    ?SET(replace(T, F, []), F({?TYPE(R1), ?TYPE(R2)}))
    end.

%% Inlined.
test_rel(R, I, C) ->
    case ?TYPE(R) of
        Rel when ?IS_RELATION(Rel), C =:= eq, I =:= ?REL_ARITY(Rel) -> true;
        Rel when ?IS_RELATION(Rel), C =:= lte, I>=1, I =< ?REL_ARITY(Rel) ->
            true;
        ?ANYTYPE -> true;
        _ -> false
    end.

%%%
%%% Family functions
%%%

-spec(fam2rel(Family) -> BinRel when
      Family :: family(),
      BinRel :: binary_relation()).
fam2rel(F) ->
    family_to_relation(F).

-spec(family_to_relation(Family) -> BinRel when
      Family :: family(),
      BinRel :: binary_relation()).
%% Inlined.
family_to_relation(F) when ?IS_SET(F) ->
    case ?TYPE(F) of
        ?FAMILY(DT, RT) ->
	    ?SET(family2rel(?LIST(F), []), ?BINREL(DT, RT));
        ?ANYTYPE -> F;
        _ -> erlang:error(badarg)
    end.

-spec(family_specification(Fun, Family1) -> Family2 when
      Fun :: spec_fun(),
      Family1 :: family(),
      Family2 :: family()).
family_specification(Fun, F) when ?IS_SET(F) ->
    case ?TYPE(F) of
        ?FAMILY(_DT, Type) = FType ->
	    R = case external_fun(Fun) of
		    false ->
			fam_spec(?LIST(F), Fun, Type, []);
		    XFun ->
			fam_specification(?LIST(F), XFun, [])
		end,
	    case R of
		SL when is_list(SL) ->
		    ?SET(SL, FType);
		Bad ->
		    erlang:error(Bad)
	    end;
        ?ANYTYPE -> F;
        _ -> erlang:error(badarg)
    end.

-spec(union_of_family(Family) -> Set when
      Family :: family(),
      Set :: a_set()).
union_of_family(F) when ?IS_SET(F) ->
    case ?TYPE(F) of
        ?FAMILY(_DT, Type) ->
	    ?SET(un_of_fam(?LIST(F), []), Type);
        ?ANYTYPE -> F;
        _ -> erlang:error(badarg)
    end.

-spec(intersection_of_family(Family) -> Set when
      Family :: family(),
      Set :: a_set()).
intersection_of_family(F) when ?IS_SET(F) ->
    case ?TYPE(F) of
        ?FAMILY(_DT, Type) ->
            case int_of_fam(?LIST(F)) of
                FU when is_list(FU) ->
                    ?SET(FU, Type);
                Bad ->
                    erlang:error(Bad)
            end;
        _ -> erlang:error(badarg)
    end.

-spec(family_union(Family1) -> Family2 when
      Family1 :: family(),
      Family2 :: family()).
family_union(F) when ?IS_SET(F) ->
    case ?TYPE(F) of
        ?FAMILY(DT, ?SET_OF(Type)) ->
	    ?SET(fam_un(?LIST(F), []), ?FAMILY(DT, Type));
        ?ANYTYPE -> F;
        _ -> erlang:error(badarg)
    end.

-spec(family_intersection(Family1) -> Family2 when
      Family1 :: family(),
      Family2 :: family()).
family_intersection(F) when ?IS_SET(F) ->
    case ?TYPE(F) of
        ?FAMILY(DT, ?SET_OF(Type)) ->
            case fam_int(?LIST(F), []) of
                FU when is_list(FU) ->
                    ?SET(FU, ?FAMILY(DT, Type));
                Bad ->
                    erlang:error(Bad)
            end;
        ?ANYTYPE -> F;
        _ -> erlang:error(badarg)
    end.

-spec(family_domain(Family1) -> Family2 when
      Family1 :: family(),
      Family2 :: family()).
family_domain(F) when ?IS_SET(F) ->
    case ?TYPE(F) of
        ?FAMILY(FDT, ?BINREL(DT, _)) ->
            ?SET(fam_dom(?LIST(F), []), ?FAMILY(FDT, DT));
        ?ANYTYPE -> F;
        ?FAMILY(_, ?ANYTYPE) -> F;
        _ -> erlang:error(badarg)
    end.

-spec(family_range(Family1) -> Family2 when
      Family1 :: family(),
      Family2 :: family()).
family_range(F) when ?IS_SET(F) ->
    case ?TYPE(F) of
        ?FAMILY(DT, ?BINREL(_, RT)) ->
            ?SET(fam_ran(?LIST(F), []), ?FAMILY(DT, RT));
        ?ANYTYPE -> F;
        ?FAMILY(_, ?ANYTYPE) -> F;
        _ -> erlang:error(badarg)
    end.

-spec(family_field(Family1) -> Family2 when
      Family1 :: family(),
      Family2 :: family()).
family_field(F) ->
    family_union(family_domain(F), family_range(F)).

-spec(family_union(Family1, Family2) -> Family3 when
      Family1 :: family(),
      Family2 :: family(),
      Family3 :: family()).
family_union(F1, F2) ->
    fam_binop(F1, F2, fun fam_union/3).

-spec(family_intersection(Family1, Family2) -> Family3 when
      Family1 :: family(),
      Family2 :: family(),
      Family3 :: family()).
family_intersection(F1, F2) ->
    fam_binop(F1, F2, fun fam_intersect/3).

-spec(family_difference(Family1, Family2) -> Family3 when
      Family1 :: family(),
      Family2 :: family(),
      Family3 :: family()).
family_difference(F1, F2) ->
    fam_binop(F1, F2, fun fam_difference/3).

%% Inlined.
fam_binop(F1, F2, FF) when ?IS_SET(F1), ?IS_SET(F2) ->
    case unify_types(?TYPE(F1), ?TYPE(F2)) of
        [] ->
            erlang:error(type_mismatch);
        ?ANYTYPE ->
            F1;
        Type = ?FAMILY(_, _) ->
	    ?SET(FF(?LIST(F1), ?LIST(F2), []), Type);
        _ ->  erlang:error(badarg)
    end.

-spec(partition_family(SetFun, Set) -> Family when
      Family :: family(),
      SetFun :: set_fun(),
      Set :: a_set()).
partition_family(I, Set) when is_integer(I), ?IS_SET(Set) ->
    Type = ?TYPE(Set),
    case check_for_sort(Type, I) of
        empty ->
            Set;
        error ->
            erlang:error(badarg);
	false -> % when I =:= 1
	    ?SET(fam_partition_n(I, ?LIST(Set)),
		 ?BINREL(?REL_TYPE(I, Type), ?SET_OF(Type)));
        true ->
	    ?SET(fam_partition_n(I, keysort(I, ?LIST(Set))),
		 ?BINREL(?REL_TYPE(I, Type), ?SET_OF(Type)))
    end;
partition_family(SetFun, Set) when ?IS_SET(Set) ->
    Type = ?TYPE(Set),
    SL = ?LIST(Set),
    case external_fun(SetFun) of
	false when SL =/= [] ->
	    case subst(SL, SetFun, element_type(Type)) of
		{NSL, NewType} ->
		    P = fam_partition(converse(NSL, []), true),
		    ?SET(reverse(P), ?BINREL(NewType, ?SET_OF(Type)));
		Bad ->
		    erlang:error(Bad)
	    end;
	false ->
	    empty_set();
	_ when Type =:= ?ANYTYPE ->
	    empty_set();
	_XFun when ?IS_SET_OF(Type) ->
            erlang:error(badarg);
	XFun ->
	    DType = XFun(Type),
	    try check_fun(Type, XFun, DType) of
		Sort ->
		    Ts = inverse_substitution(?LIST(Set), XFun, Sort),
		    P = fam_partition(Ts, Sort),
		    ?SET(reverse(P), ?BINREL(DType, ?SET_OF(Type)))
            catch _:_ -> erlang:error(badarg)
	    end
    end.

-spec(family_projection(SetFun, Family1) -> Family2 when
      SetFun :: set_fun(),
      Family1 :: family(),
      Family2 :: family()).
family_projection(SetFun, F) when ?IS_SET(F) ->
    case ?TYPE(F) of
        ?FAMILY(_, _) when [] =:= ?LIST(F) ->
	    empty_set();
        ?FAMILY(DT, Type) ->
	    case external_fun(SetFun) of
		false ->
		    case fam_proj(?LIST(F), SetFun, Type, ?ANYTYPE, []) of
			{SL, NewType} ->
			    ?SET(SL, ?BINREL(DT, NewType));
			Bad ->
			    erlang:error(Bad)
		    end;
		_ ->
		    erlang:error(badarg)
	    end;
	?ANYTYPE -> F;
        _ -> erlang:error(badarg)
    end.

%%%
%%% Digraph functions
%%%

-spec(family_to_digraph(Family) -> Graph when
      Graph :: digraph:graph(),
      Family :: family()).
family_to_digraph(F) when ?IS_SET(F) ->
    case ?TYPE(F) of
        ?FAMILY(_, _) -> fam2digraph(F, digraph:new());
        ?ANYTYPE -> digraph:new();
        _Else -> erlang:error(badarg)
    end.

-spec(family_to_digraph(Family, GraphType) -> Graph when
      Graph :: digraph:graph(),
      Family :: family(),
      GraphType :: [digraph:d_type()]).
family_to_digraph(F, Type) when ?IS_SET(F) ->
    case ?TYPE(F) of
        ?FAMILY(_, _) -> ok;
        ?ANYTYPE -> ok;
        _Else  -> erlang:error(badarg)
    end,
    try digraph:new(Type) of
        G -> case catch fam2digraph(F, G) of
                 {error, Reason} ->
                     true = digraph:delete(G),
                     erlang:error(Reason);
                 _ ->
                     G
             end
    catch
        error:badarg -> erlang:error(badarg)
    end.

-spec(digraph_to_family(Graph) -> Family when
      Graph :: digraph:graph(),
      Family :: family()).
digraph_to_family(G) ->
    try digraph_family(G) of
        L -> ?SET(L, ?FAMILY(?ATOM_TYPE, ?ATOM_TYPE))
    catch _:_ -> erlang:error(badarg)
    end.

-spec(digraph_to_family(Graph, Type) -> Family when
      Graph :: digraph:graph(),
      Family :: family(),
      Type :: type()).
digraph_to_family(G, T) ->
    case {is_type(T), T} of
        {true, ?SET_OF(?FAMILY(_,_) = Type)} ->
            try digraph_family(G) of
                L -> ?SET(L, Type)
            catch _:_ -> erlang:error(badarg)
            end;
        _ ->
            erlang:error(badarg)
    end.

%%
%%  Local functions
%%

%% Type = OrderedSetType
%%      | SetType
%%      | atom() except '_'
%% OrderedSetType = {Type, ..., Type}
%% SetType = [ElementType]           % list of exactly one element
%% ElementType = '_'                 % any type (implies empty set)
%%             | Type

is_types(0, _T) ->
    true;
is_types(I, T) ->
    case is_type(?REL_TYPE(I, T)) of
        true -> is_types(I-1, T);
        false -> false
    end.

is_element_type(?ANYTYPE) ->
    true;
is_element_type(T) ->
    is_type(T).

set_of_sets([S | Ss], L, T0) when ?IS_SET(S) ->
    case unify_types([?TYPE(S)], T0) of
        [] -> {error, type_mismatch};
        Type ->  set_of_sets(Ss, [?LIST(S) | L], Type)
    end;
set_of_sets([S | Ss], L, T0) when ?IS_ORDSET(S) ->
    case unify_types(?ORDTYPE(S), T0) of
        [] -> {error, type_mismatch};
        Type ->  set_of_sets(Ss, [?ORDDATA(S) | L], Type)
    end;
set_of_sets([], L, T) ->
    ?SET(usort(L), T);
set_of_sets(_, _L, _T) ->
    {error, badarg}.

ordset_of_sets([S | Ss], L, T) when ?IS_SET(S) ->
    ordset_of_sets(Ss, [?LIST(S) | L], [[?TYPE(S)] | T]);
ordset_of_sets([S | Ss], L, T) when ?IS_ORDSET(S) ->
    ordset_of_sets(Ss, [?ORDDATA(S) | L], [?ORDTYPE(S) | T]);
ordset_of_sets([], L, T) ->
    ?ORDSET(list_to_tuple(reverse(L)), list_to_tuple(reverse(T)));
ordset_of_sets(_, _L, _T) ->
    error.

%% Inlined.
rel(Ts, [Type]) ->
    case is_type(Type) and atoms_only(Type, 1) of
        true ->
            rel(Ts, tuple_size(Type), Type);
        false ->
            rel_type(Ts, [], Type)
    end;
rel(Ts, Sz) ->
    rel(Ts, Sz, erlang:make_tuple(Sz, ?ATOM_TYPE)).

atoms_only(Type, I) when ?IS_ATOM_TYPE(?REL_TYPE(I, Type)) ->
    atoms_only(Type, I+1);
atoms_only(Type, I) when I > tuple_size(Type), ?IS_RELATION(Type) ->
    true;
atoms_only(_Type, _I) ->
    false.

rel(Ts, Sz, Type) when Sz >= 1 ->
    SL = usort(Ts),
    rel(SL, SL, Sz, Type).

rel([T | Ts], L, Sz, Type) when tuple_size(T) =:= Sz ->
    rel(Ts, L, Sz, Type);
rel([], L, _Sz, Type) ->
    ?SET(L, Type).

rel_type([E | Ts], L, Type) ->
    {NType, NE} = make_element(E, Type, Type),
    rel_type(Ts, [NE | L], NType);
rel_type([], [], ?ANYTYPE) ->
    empty_set();
rel_type([], SL, Type) when ?IS_RELATION(Type) ->
    ?SET(usort(SL), Type).

%% Inlined.
a_func(Ts, T) ->
    case {T, is_type(T)} of
	{[?BINREL(DT, RT) = Type], true} when ?IS_ATOM_TYPE(DT),
					      ?IS_ATOM_TYPE(RT)  ->
	    func(Ts, Type);
	{[Type], true} ->
	    func_type(Ts, [], Type, fun(?BINREL(_,_)) -> true end)
    end.

func(L0, Type) ->
    L = usort(L0),
    func(L, L, L, Type).

func([{X,_} | Ts], X0, L, Type) when X /= X0 ->
    func(Ts, X, L, Type);
func([{X,_} | _Ts], X0, _L, _Type) when X == X0 ->
    bad_function;
func([], _X0, L, Type) ->
    ?SET(L, Type).

%% Inlined.
fam(Ts, T) ->
    case {T, is_type(T)} of
	{[?FAMILY(DT, RT) = Type], true} when ?IS_ATOM_TYPE(DT),
					      ?IS_ATOM_TYPE(RT)  ->
	    fam2(Ts, Type);
	{[Type], true} ->
	    func_type(Ts, [], Type, fun(?FAMILY(_,_)) -> true end)
    end.

fam2([], Type) ->
    ?SET([], Type);
fam2(Ts, Type) ->
    fam2(sort(Ts), Ts, [], Type).

fam2([{I,L} | T], I0, SL, Type) when I /= I0 ->
    fam2(T, I, [{I,usort(L)} | SL], Type);
fam2([{I,L} | T], I0, SL, Type) when I == I0 ->
    case {usort(L), SL} of
	{NL, [{_I,NL1} | _]} when NL == NL1 ->
	    fam2(T, I0, SL, Type);
	_ ->
	    bad_function
    end;
fam2([], _I0, SL, Type) ->
    ?SET(reverse(SL), Type).

func_type([E | T], SL, Type, F) ->
    {NType, NE} = make_element(E, Type, Type),
    func_type(T, [NE | SL], NType, F);
func_type([], [], ?ANYTYPE, _F) ->
    empty_set();
func_type([], SL, Type, F) ->
    true = F(Type),
    NL = usort(SL),
    check_function(NL, ?SET(NL, Type)).

setify(L, ?SET_OF(Atom)) when ?IS_ATOM_TYPE(Atom), Atom =/= ?ANYTYPE ->
    ?SET(usort(L), Atom);
setify(L, ?SET_OF(Type0)) ->
    try is_no_lists(Type0) of
        N when is_integer(N) ->
            rel(L, N, Type0);
        Sizes ->
            make_oset(L, Sizes, L, Type0)
    catch
        _:_ ->
            {?SET_OF(Type), Set} = create(L, Type0, Type0, []),
            ?SET(Set, Type)
    end;
setify(E, Type0) ->
    {Type, OrdSet} = make_element(E, Type0, Type0),
    ?ORDSET(OrdSet, Type).

is_no_lists(T) when is_tuple(T) ->
   Sz = tuple_size(T),
   is_no_lists(T, Sz, Sz, []).

is_no_lists(_T, 0, Sz, []) ->
   Sz;
is_no_lists(_T, 0, Sz, L) ->
   {Sz, L};
is_no_lists(T, I, Sz, L) when ?IS_ATOM_TYPE(?REL_TYPE(I, T)) ->
   is_no_lists(T, I-1, Sz, L);
is_no_lists(T, I, Sz, L) ->
   is_no_lists(T, I-1, Sz, [{I,is_no_lists(?REL_TYPE(I, T))} | L]).

create([E | Es], T, T0, L) ->
    {NT, S} = make_element(E, T, T0),
    create(Es, NT, T0, [S | L]);
create([], T, _T0, L) ->
    {?SET_OF(T), usort(L)}.

make_element(C, ?ANYTYPE, _T0) ->
    make_element(C);
make_element(C, Atom, ?ANYTYPE) when ?IS_ATOM_TYPE(Atom),
                                     not is_list(C), not is_tuple(C) ->
    {Atom, C};
make_element(C, Atom, Atom) when ?IS_ATOM_TYPE(Atom) ->
    {Atom, C};
make_element(T, TT, ?ANYTYPE) when tuple_size(T) =:= tuple_size(TT) ->
    make_tuple(tuple_to_list(T), tuple_to_list(TT), [], [], ?ANYTYPE);
make_element(T, TT, T0) when tuple_size(T) =:= tuple_size(TT) ->
    make_tuple(tuple_to_list(T), tuple_to_list(TT), [], [], tuple_to_list(T0));
make_element(L, [LT], ?ANYTYPE) when is_list(L) ->
    create(L, LT, ?ANYTYPE, []);
make_element(L, [LT], [T0]) when is_list(L) ->
    create(L, LT, T0, []).

make_tuple([E | Es], [T | Ts], NT, L, T0) when T0 =:= ?ANYTYPE ->
    {ET, ES} = make_element(E, T, T0),
    make_tuple(Es, Ts, [ET | NT], [ES | L], T0);
make_tuple([E | Es], [T | Ts], NT, L, [T0 | T0s]) ->
    {ET, ES} = make_element(E, T, T0),
    make_tuple(Es, Ts, [ET | NT], [ES | L], T0s);
make_tuple([], [], NT, L, _T0s) when NT =/= [] ->
    {list_to_tuple(reverse(NT)), list_to_tuple(reverse(L))}.

%% Derive type.
make_element(C) when not is_list(C), not is_tuple(C) ->
    {?ATOM_TYPE, C};
make_element(T) when is_tuple(T) ->
    make_tuple(tuple_to_list(T), [], []);
make_element(L) when is_list(L) ->
    create(L, ?ANYTYPE, ?ANYTYPE, []).

make_tuple([E | Es], T, L) ->
    {ET, ES} = make_element(E),
    make_tuple(Es, [ET | T], [ES | L]);
make_tuple([], T, L) when T =/= [] ->
    {list_to_tuple(reverse(T)),  list_to_tuple(reverse(L))}.

make_oset([T | Ts], Szs, L, Type) ->
    true = test_oset(Szs, T, T),
    make_oset(Ts, Szs, L, Type);
make_oset([], _Szs, L, Type) ->
    ?SET(usort(L), Type).

%% Optimization. Avoid re-building (nested) tuples.
test_oset({Sz,Args}, T, T0) when tuple_size(T) =:= Sz ->
    test_oset_args(Args, T, T0);
test_oset(Sz, T, _T0) when tuple_size(T) =:= Sz ->
    true.

test_oset_args([{Arg,Szs} | Ss], T, T0) ->
    true = test_oset(Szs, ?REL_TYPE(Arg, T), T0),
    test_oset_args(Ss, T, T0);
test_oset_args([], _T, _T0) ->
    true.

list_of_sets([S | Ss], Type, L) ->
    list_of_sets(Ss, Type, [?SET(S, Type) | L]);
list_of_sets([], _Type, L) ->
    reverse(L).

list_of_ordsets([S | Ss], Type, L) ->
    list_of_ordsets(Ss, Type, [?ORDSET(S, Type) | L]);
list_of_ordsets([], _Type, L) ->
    reverse(L).

tuple_of_sets([S | Ss], [?SET_OF(Type) | Types], L) ->
    tuple_of_sets(Ss, Types, [?SET(S, Type) | L]);
tuple_of_sets([S | Ss], [Type | Types], L) ->
    tuple_of_sets(Ss, Types, [?ORDSET(S, Type) | L]);
tuple_of_sets([], [], L) ->
    list_to_tuple(reverse(L)).

spec([E | Es], Fun, Type, L) ->
    case Fun(term2set(E, Type)) of
        true ->
            spec(Es, Fun, Type, [E | L]);
        false ->
            spec(Es, Fun, Type, L);
	_ ->
	    badarg
    end;
spec([], _Fun, _Type, L) ->
    reverse(L).

specification([E | Es], Fun, L) ->
    case Fun(E) of
        true ->
            specification(Es, Fun, [E | L]);
        false ->
            specification(Es, Fun, L);
	_ ->
	    badarg
    end;
specification([], _Fun, L) ->
    reverse(L).

%% Elements from the first list are kept.
intersection([H1 | T1], [H2 | T2], L) when H1 < H2 ->
    intersection1(T1, T2, L, H2);
intersection([H1 | T1], [H2 | T2], L) when H1 == H2 ->
    intersection(T1, T2, [H1 | L]);
intersection([H1 | T1], [_H2 | T2], L) ->
    intersection2(T1, T2, L, H1);
intersection(_, _, L) ->
    reverse(L).

intersection1([H1 | T1], T2, L, H2) when H1 < H2 ->
    intersection1(T1, T2, L, H2);
intersection1([H1 | T1], T2, L, H2) when H1 == H2 ->
    intersection(T1, T2, [H1 | L]);
intersection1([H1 | T1], T2, L, _H2) ->
    intersection2(T1, T2, L, H1);
intersection1(_, _, L, _) ->
    reverse(L).

intersection2(T1, [H2 | T2], L, H1) when H1 > H2 ->
    intersection2(T1, T2, L, H1);
intersection2(T1, [H2 | T2], L, H1) when H1 == H2 ->
    intersection(T1, T2, [H1 | L]);
intersection2(T1, [H2 | T2], L, _H1) ->
    intersection1(T1, T2, L, H2);
intersection2(_, _, L, _) ->
    reverse(L).

difference([H1 | T1], [H2 | T2], L) when H1 < H2 ->
    diff(T1, T2, [H1 | L], H2);
difference([H1 | T1], [H2 | T2], L) when H1 == H2 ->
    difference(T1, T2, L);
difference([H1 | T1], [_H2 | T2], L) ->
    diff2(T1, T2, L, H1);
difference(L1, _, L) ->
    reverse(L, L1).

diff([H1 | T1], T2, L, H2) when H1 < H2 ->
    diff(T1, T2, [H1 | L], H2);
diff([H1 | T1], T2, L, H2) when H1 == H2 ->
    difference(T1, T2, L);
diff([H1 | T1], T2, L, _H2) ->
    diff2(T1, T2, L, H1);
diff(_, _, L, _) ->
    reverse(L).

diff2(T1, [H2 | T2], L, H1) when H1 > H2 ->
    diff2(T1, T2, L, H1);
diff2(T1, [H2 | T2], L, H1) when H1 == H2 ->
    difference(T1, T2, L);
diff2(T1, [H2 | T2], L, H1) ->
    diff(T1, T2, [H1 | L], H2);
diff2(T1, _, L, H1) ->
    reverse(L, [H1 | T1]).

symdiff([H1 | T1], T2, L) ->
    symdiff2(T1, T2, L, H1);
symdiff(_, T2, L) ->
    reverse(L, T2).

symdiff1([H1 | T1], T2, L, H2) when H1 < H2 ->
    symdiff1(T1, T2, [H1 | L], H2);
symdiff1([H1 | T1], T2, L, H2) when H1 == H2 ->
    symdiff(T1, T2, L);
symdiff1([H1 | T1], T2, L, H2) ->
    symdiff2(T1, T2, [H2 | L], H1);
symdiff1(_, T2, L, H2) ->
    reverse(L, [H2 | T2]).

symdiff2(T1, [H2 | T2], L, H1) when H1 > H2 ->
    symdiff2(T1, T2, [H2 | L], H1);
symdiff2(T1, [H2 | T2], L, H1) when H1 == H2 ->
    symdiff(T1, T2, L);
symdiff2(T1, [H2 | T2], L, H1) ->
    symdiff1(T1, T2, [H1 | L], H2);
symdiff2(T1, _, L, H1) ->
    reverse(L, [H1 | T1]).

sympart([H1 | T1], [H2 | T2], L1, L12, L2, T) when H1 < H2 ->
    sympart1(T1, T2, [H1 | L1], L12, L2, T, H2);
sympart([H1 | T1], [H2 | T2], L1, L12, L2, T) when H1 == H2 ->
    sympart(T1, T2, L1, [H1 | L12], L2, T);
sympart([H1 | T1], [H2 | T2], L1, L12, L2, T) ->
    sympart2(T1, T2, L1, L12, [H2 | L2], T, H1);
sympart(S1, [], L1, L12, L2, T) ->
    {?SET(reverse(L1, S1), T),
     ?SET(reverse(L12), T),
     ?SET(reverse(L2), T)};
sympart(_, S2, L1, L12, L2, T) ->
    {?SET(reverse(L1), T),
     ?SET(reverse(L12), T),
     ?SET(reverse(L2, S2), T)}.

sympart1([H1 | T1], T2, L1, L12, L2, T, H2) when H1 < H2 ->
    sympart1(T1, T2, [H1 | L1], L12, L2, T, H2);
sympart1([H1 | T1], T2, L1, L12, L2, T, H2) when H1 == H2 ->
    sympart(T1, T2, L1, [H1 | L12], L2, T);
sympart1([H1 | T1], T2, L1, L12, L2, T, H2) ->
    sympart2(T1, T2, L1, L12, [H2 | L2], T, H1);
sympart1(_, T2, L1, L12, L2, T, H2) ->
    {?SET(reverse(L1), T),
     ?SET(reverse(L12), T),
     ?SET(reverse(L2, [H2 | T2]), T)}.

sympart2(T1, [H2 | T2], L1, L12, L2, T, H1) when H1 > H2 ->
    sympart2(T1, T2, L1, L12, [H2 | L2], T, H1);
sympart2(T1, [H2 | T2], L1, L12, L2, T, H1) when H1 == H2 ->
    sympart(T1, T2, L1, [H1 | L12], L2, T);
sympart2(T1, [H2 | T2], L1, L12, L2, T, H1) ->
    sympart1(T1, T2, [H1 | L1], L12, L2, T, H2);
sympart2(T1, _, L1, L12, L2, T, H1) ->
    {?SET(reverse(L1, [H1 | T1]), T),
     ?SET(reverse(L12), T),
     ?SET(reverse(L2), T)}.

prod([[E | Es] | Xs], T, L) ->
    prod(Es, Xs, T, prod(Xs, [E | T], L));
prod([], T, L) ->
    [list_to_tuple(reverse(T)) | L].

prod([E | Es], Xs, T, L) ->
    prod(Es, Xs, T, prod(Xs, [E | T], L));
prod([], _Xs, _E, L) ->
    L.

constant_function([E | Es], X, L) ->
    constant_function(Es, X, [{E,X} | L]);
constant_function([], _X, L) ->
    reverse(L).

subset([H1 | T1], [H2 | T2]) when H1 > H2 ->
    subset(T1, T2, H1);
subset([H1 | T1], [H2 | T2]) when H1 == H2 ->
    subset(T1, T2);
subset(L1, _) ->
    L1 =:= [].

subset(T1, [H2 | T2], H1) when H1 > H2 ->
    subset(T1, T2, H1);
subset(T1, [H2 | T2], H1) when H1 == H2 ->
    subset(T1, T2);
subset(_, _, _) ->
    false.

disjoint([B | Bs], A, As) when A < B ->
    disjoint(As, B, Bs);
disjoint([B | _Bs], A, _As) when A == B ->
    false;
disjoint([_B | Bs], A, As) ->
    disjoint(Bs, A, As);
disjoint(_Bs, _A, _As) ->
    true.

%% Append sets that come in order, then "merge".
lunion([[_] = S]) -> % optimization
    S;
lunion([[] | Ls]) ->
    lunion(Ls);
lunion([S | Ss]) ->
    umerge(lunion(Ss, last(S), [S], []));
lunion([]) ->
    [].

lunion([[E] = S | Ss], Last, SL, Ls) when E > Last -> % optimization
    lunion(Ss, E, [S | SL], Ls);
lunion([S | Ss], Last, SL, Ls) when hd(S) > Last ->
    lunion(Ss, last(S), [S | SL], Ls);
lunion([S | Ss], _Last, SL, Ls) ->
    lunion(Ss, last(S), [S], [append(reverse(SL)) | Ls]);
lunion([], _Last, SL, Ls) ->
    [append(reverse(SL)) | Ls].

%% The empty list is always the first list, if present.
lintersection(_, []) ->
    [];
lintersection([S | Ss], S0) ->
    lintersection(Ss, intersection(S, S0, []));
lintersection([], S) ->
    S.

can_rel([S | Ss], L) ->
    can_rel(Ss, L, S, S);
can_rel([], L) ->
    sort(L).

can_rel(Ss, L, [E | Es], S) ->
    can_rel(Ss, [{E, S} | L], Es, S);
can_rel(Ss, L, _, _S) ->
    can_rel(Ss, L).

rel2family([{X,Y} | S]) ->
    rel2fam(S, X, [Y], []);
rel2family([]) ->
    [].

rel2fam([{X,Y} | S], X0, YL, L) when X0 == X ->
    rel2fam(S, X0, [Y | YL], L);
rel2fam([{X,Y} | S], X0, [A,B | YL], L) -> % optimization
    rel2fam(S, X, [Y], [{X0,reverse(YL,[B,A])} | L]);
rel2fam([{X,Y} | S], X0, YL, L) ->
    rel2fam(S, X, [Y], [{X0,YL} | L]);
rel2fam([], X, YL, L) ->
    reverse([{X,reverse(YL)} | L]).

dom([{X,_} | Es]) ->
    dom([], X, Es);
dom([] = L) ->
    L.

dom(L, X, [{X1,_} | Es]) when X == X1 ->
    dom(L, X, Es);
dom(L, X, [{Y,_} | Es]) ->
    dom([X | L], Y, Es);
dom(L, X, []) ->
    reverse(L, [X]).

ran([{_,Y} | Es], L) ->
    ran(Es, [Y | L]);
ran([], L) ->
    usort(L).

relprod(A, B) ->
    usort(relprod1(A, B)).

relprod1([{Ay,Ax} | A], B) ->
    relprod1(B, Ay, Ax, A, []);
relprod1(_A, _B) ->
    [].

relprod1([{Bx,_By} | B], Ay, Ax, A, L) when Ay > Bx ->
    relprod1(B, Ay, Ax, A, L);
relprod1([{Bx,By} | B], Ay, Ax, A, L) when Ay == Bx ->
    relprod(B, Bx, By, A, [{Ax,By} | L], Ax, B, Ay);
relprod1([{Bx,By} | B], _Ay, _Ax, A, L) ->
    relprod2(B, Bx, By, A, L);
relprod1(_B, _Ay, _Ax, _A, L) ->
    L.

relprod2(B, Bx, By, [{Ay, _Ax} | A], L) when Ay < Bx ->
    relprod2(B, Bx, By, A, L);
relprod2(B, Bx, By, [{Ay, Ax} | A], L) when Ay == Bx ->
    relprod(B, Bx, By, A, [{Ax,By} | L], Ax, B, Ay);
relprod2(B, _Bx, _By, [{Ay, Ax} | A], L) ->
    relprod1(B, Ay, Ax, A, L);
relprod2(_, _, _, _, L) ->
    L.

relprod(B0, Bx0, By0, A0, L, Ax, [{Bx,By} | B], Ay) when Ay == Bx ->
    relprod(B0, Bx0, By0, A0, [{Ax,By} | L], Ax, B, Ay);
relprod(B0, Bx0, By0, A0, L, _Ax, _B, _Ay) ->
    relprod2(B0, Bx0, By0, A0, L).

relprod_n([], _R, _EmptyG, _IsR) ->
    {error, badarg};
relprod_n(RL, R, EmptyR, IsR) ->
    case domain_type(RL, ?ANYTYPE) of
        Error = {error, _Reason} ->
            Error;
        DType ->
            Empty = any(fun is_empty_set/1, RL) or EmptyR,
            RType = range_type(RL, []),
            Type = ?BINREL(DType, RType),
            Prod =
                case Empty of
                    true when DType =:= ?ANYTYPE; RType =:= ?ANYTYPE ->
                        empty_set();
                    true ->
                        ?SET([], Type);
                    false ->
                        TL = ?LIST((relprod_n(RL))),
                        Sz = length(RL),
                        Fun = fun({X,A}) -> {X, flat(Sz, A, [])} end,
                        ?SET(map(Fun, TL), Type)
                end,
            case IsR of
                true  -> relative_product(Prod, R);
                false -> Prod
            end
    end.

relprod_n([R | Rs]) ->
    relprod_n(Rs, R).

relprod_n([], R) ->
    R;
relprod_n([R | Rs], R0) ->
    T = raise_element(R0, 1),
    R1 = relative_product1(T, R),
    NR = projection({external, fun({{X,A},AS}) -> {X,{A,AS}} end}, R1),
    relprod_n(Rs, NR).

flat(1, A, L) ->
    list_to_tuple([A | L]);
flat(N, {T,A}, L) ->
    flat(N-1, T, [A | L]).

domain_type([T | Ts], T0) when ?IS_SET(T) ->
    case ?TYPE(T) of
        ?BINREL(DT, _RT) ->
            case unify_types(DT, T0) of
                [] -> {error, type_mismatch};
                T1 -> domain_type(Ts, T1)
            end;
        ?ANYTYPE ->
            domain_type(Ts, T0);
        _ -> {error, badarg}
    end;
domain_type([], T0) ->
    T0.

range_type([T | Ts], L) ->
    case ?TYPE(T) of
        ?BINREL(_DT, RT) ->
            range_type(Ts, [RT | L]);
        ?ANYTYPE ->
            ?ANYTYPE
    end;
range_type([], L) ->
    list_to_tuple(reverse(L)).

converse([{A,B} | X], L) ->
    converse(X, [{B,A} | L]);
converse([], L) ->
    sort(L).

strict([{E1,E2} | Es], L) when E1 == E2 ->
    strict(Es, L);
strict([E | Es], L) ->
    strict(Es, [E | L]);
strict([], L) ->
    reverse(L).

weak(Es) ->
    %% Not very efficient...
    weak(Es, ran(Es, []), []).

weak(Es=[{X,_} | _], [Y | Ys], L) when X > Y ->
    weak(Es, Ys, [{Y,Y} | L]);
weak(Es=[{X,_} | _], [Y | Ys], L) when X == Y ->
    weak(Es, Ys, L);
weak([E={X,Y} | Es], Ys, L) when X > Y ->
    weak1(Es, Ys, [E | L], X);
weak([E={X,Y} | Es], Ys, L) when X == Y ->
    weak2(Es, Ys, [E | L], X);
weak([E={X,_Y} | Es], Ys, L) -> % when X < _Y
    weak2(Es, Ys, [E, {X,X} | L], X);
weak([], [Y | Ys], L) ->
    weak([], Ys, [{Y,Y} | L]);
weak([], [], L) ->
    reverse(L).

weak1([E={X,Y} | Es], Ys, L, X0) when X > Y, X == X0 ->
    weak1(Es, Ys, [E | L], X);
weak1([E={X,Y} | Es], Ys, L, X0) when X == Y, X == X0 ->
    weak2(Es, Ys, [E | L], X);
weak1([E={X,_Y} | Es], Ys, L, X0) when X == X0 -> % when X < Y
    weak2(Es, Ys, [E, {X,X} | L], X);
weak1(Es, Ys, L, X) ->
    weak(Es, Ys, [{X,X} | L]).

weak2([E={X,_Y} | Es], Ys, L, X0) when X == X0 -> % when X < _Y
    weak2(Es, Ys, [E | L], X);
weak2(Es, Ys, L, _X) ->
    weak(Es, Ys, L).

extc(L, [D | Ds], C, Ts) ->
    extc(L, Ds, C, Ts, D);
extc(L, [], _C, _Ts) ->
    L.

extc(L, Ds, C, [{X,_Y} | Ts], D) when X < D ->
    extc(L, Ds, C, Ts, D);
extc(L, Ds, C, [{X,_Y} | Ts], D) when X == D ->
    extc(L, Ds, C, Ts);
extc(L, Ds, C, [{X,_Y} | Ts], D) ->
    extc2([{D,C} | L], Ds, C, Ts, X);
extc(L, Ds, C, [], D) ->
    extc_tail([{D,C} | L], Ds, C).

extc2(L, [D | Ds], C, Ts, X) when X > D ->
    extc2([{D,C} | L], Ds, C, Ts, X);
extc2(L, [D | Ds], C, Ts, X) when X == D ->
    extc(L, Ds, C, Ts);
extc2(L, [D | Ds], C, Ts, _X) ->
    extc(L, Ds, C, Ts, D);
extc2(L, [], _C, _Ts, _X) ->
    L.

extc_tail(L, [D | Ds], C) ->
    extc_tail([{D,C} | L], Ds, C);
extc_tail(L, [], _C) ->
    L.

is_a_func([{E,_} | Es], E0) when E /= E0 ->
    is_a_func(Es, E);
is_a_func(L, _E) ->
    L =:= [].

restrict_n(I, [T | Ts], Key, Keys, L) ->
    case element(I, T) of
	K when K < Key ->
	    restrict_n(I, Ts, Key, Keys, L);
	K when K == Key ->
	    restrict_n(I, Ts, Key, Keys, [T | L]);
	K ->
	    restrict_n(I, K, Ts, Keys, L, T)
    end;
restrict_n(_I, _Ts, _Key, _Keys, L) ->
    L.

restrict_n(I, K, Ts, [Key | Keys], L, E) when K > Key ->
    restrict_n(I, K, Ts, Keys, L, E);
restrict_n(I, K, Ts, [Key | Keys], L, E) when K == Key ->
    restrict_n(I, Ts, Key, Keys, [E | L]);
restrict_n(I, _K, Ts, [Key | Keys], L, _E) ->
    restrict_n(I, Ts, Key, Keys, L);
restrict_n(_I, _K, _Ts, _Keys, L, _E) ->
    L.

restrict([Key | Keys], Tuples) ->
    restrict(Tuples, Key, Keys, []);
restrict(_Keys, _Tuples) ->
    [].

restrict([{K,_E} | Ts], Key, Keys, L) when K < Key ->
    restrict(Ts, Key, Keys, L);
restrict([{K,E} | Ts], Key, Keys, L) when K == Key ->
    restrict(Ts, Key, Keys, [E | L]);
restrict([{K,E} | Ts], _Key, Keys, L) ->
    restrict(Ts, K, Keys, L, E);
restrict(_Ts, _Key, _Keys, L) ->
    L.

restrict(Ts, K, [Key | Keys], L, E) when K > Key ->
    restrict(Ts, K, Keys, L, E);
restrict(Ts, K, [Key | Keys], L, E) when K == Key ->
    restrict(Ts, Key, Keys, [E | L]);
restrict(Ts, _K, [Key | Keys], L, _E) ->
    restrict(Ts, Key, Keys, L);
restrict(_Ts, _K, _Keys, L, _E) ->
    L.

diff_restrict_n(I, [T | Ts], Key, Keys, L) ->
    case element(I, T) of
	K when K < Key ->
	    diff_restrict_n(I, Ts, Key, Keys, [T | L]);
	K when K == Key ->
	    diff_restrict_n(I, Ts, Key, Keys, L);
	K ->
	    diff_restrict_n(I, K, Ts, Keys, L, T)
    end;
diff_restrict_n(I, _Ts, _Key, _Keys, L) when I =:= 1 ->
    reverse(L);
diff_restrict_n(_I, _Ts, _Key, _Keys, L) ->
    sort(L).

diff_restrict_n(I, K, Ts, [Key | Keys], L, T) when K > Key ->
    diff_restrict_n(I, K, Ts, Keys, L, T);
diff_restrict_n(I, K, Ts, [Key | Keys], L, _T) when K == Key ->
    diff_restrict_n(I, Ts, Key, Keys, L);
diff_restrict_n(I, _K, Ts, [Key | Keys], L, T) ->
    diff_restrict_n(I, Ts, Key, Keys, [T | L]);
diff_restrict_n(I, _K, Ts, _Keys, L, T) when I =:= 1 ->
    reverse(L, [T | Ts]);
diff_restrict_n(_I, _K, Ts, _Keys, L, T) ->
    sort([T | Ts ++ L]).

diff_restrict([Key | Keys], Tuples) ->
    diff_restrict(Tuples, Key, Keys, []);
diff_restrict(_Keys, Tuples) ->
    diff_restrict_tail(Tuples, []).

diff_restrict([{K,E} | Ts], Key, Keys, L) when K < Key ->
    diff_restrict(Ts, Key, Keys, [E | L]);
diff_restrict([{K,_E} | Ts], Key, Keys, L) when K == Key ->
    diff_restrict(Ts, Key, Keys, L);
diff_restrict([{K,E} | Ts], _Key, Keys, L) ->
    diff_restrict(Ts, K, Keys, L, E);
diff_restrict(_Ts, _Key, _Keys, L) ->
    L.

diff_restrict(Ts, K, [Key | Keys], L, E) when K > Key ->
    diff_restrict(Ts, K, Keys, L, E);
diff_restrict(Ts, K, [Key | Keys], L, _E) when K == Key ->
    diff_restrict(Ts, Key, Keys, L);
diff_restrict(Ts, _K, [Key | Keys], L, E) ->
    diff_restrict(Ts, Key, Keys, [E | L]);
diff_restrict(Ts, _K, _Keys, L, E) ->
    diff_restrict_tail(Ts, [E | L]).

diff_restrict_tail([{_K,E} | Ts], L) ->
    diff_restrict_tail(Ts, [E | L]);
diff_restrict_tail(_Ts, L) ->
    L.

comp([], B) ->
    check_function(B, []);
comp(_A, []) ->
    bad_function;
comp(A0, [{Bx,By} | B]) ->
    A = converse(A0, []),
    check_function(A0, comp1(A, B, [], Bx, By)).

comp1([{Ay,Ax} | A], B, L, Bx, By) when Ay == Bx ->
    comp1(A, B, [{Ax,By} | L], Bx, By);
comp1([{Ay,Ax} | A], B, L, Bx, _By) when Ay > Bx ->
    comp2(A, B, L, Bx, Ay, Ax);
comp1([{Ay,_Ax} | _A], _B, _L, Bx, _By) when Ay < Bx ->
    bad_function;
comp1([], B, L, Bx, _By) ->
    check_function(Bx, B, L).

comp2(A, [{Bx,_By} | B], L, Bx0, Ay, Ax) when Ay > Bx, Bx /= Bx0 ->
    comp2(A, B, L, Bx, Ay, Ax);
comp2(A, [{Bx,By} | B], L, _Bx0, Ay, Ax) when Ay == Bx ->
    comp1(A, B, [{Ax,By} | L], Bx, By);
comp2(_A, _B, _L, _Bx0, _Ay, _Ax) ->
    bad_function.

inverse1([{A,B} | X]) ->
    inverse(X, A, [{B,A}]);
inverse1([]) ->
    [].

inverse([{A,B} | X], A0, L) when A0 /= A ->
    inverse(X, A, [{B,A} | L]);
inverse([{A,_B} | _X], A0, _L) when A0 == A ->
    bad_function;
inverse([], _A0, L) ->
    SL = [{V,_} | Es] = sort(L),
    case is_a_func(Es, V) of
	true -> SL;
	false -> bad_function
    end.

%% Inlined.
external_fun({external, Function}) when is_atom(Function) ->
    false;
external_fun({external, Fun}) ->
    Fun;
external_fun(_) ->
    false.

%% Inlined.
element_type(?SET_OF(Type)) -> Type;
element_type(Type) -> Type.

subst(Ts, Fun, Type) ->
    subst(Ts, Fun, Type, ?ANYTYPE, []).

subst([T | Ts], Fun, Type, NType, L) ->
    case setfun(T, Fun, Type, NType) of
	{SD, ST} -> subst(Ts, Fun, Type, ST, [{T, SD} | L]);
	Bad -> Bad
    end;
subst([], _Fun, _Type, NType, L) ->
    {L, NType}.

projection1([E | Es]) ->
    projection1([], element(1, E), Es);
projection1([] = L) ->
    L.

projection1(L, X, [E | Es]) ->
    case element(1, E) of
	X1 when X == X1 -> projection1(L, X, Es);
	X1 -> projection1([X | L], X1, Es)
    end;
projection1(L, X, []) ->
    reverse(L, [X]).

projection_n([E | Es], I, L) ->
    projection_n(Es, I, [element(I, E) | L]);
projection_n([], _I, L) ->
    usort(L).

substitute_element([T | Ts], I, L) ->
    substitute_element(Ts, I, [{T, element(I, T)} | L]);
substitute_element(_, _I, L) ->
    reverse(L).

substitute([T | Ts], Fun, L) ->
    substitute(Ts, Fun, [{T, Fun(T)} | L]);
substitute(_, _Fun, L) ->
    reverse(L).

partition_n(I, [E | Ts]) ->
    partition_n(I, Ts, element(I, E), [E], []);
partition_n(_I, []) ->
    [].

partition_n(I, [E | Ts], K, Es, P) ->
    case {element(I, E), Es} of
	{K1, _} when K == K1 ->
	    partition_n(I, Ts, K, [E | Es], P);
	{K1, [_]} -> % optimization
	    partition_n(I, Ts, K1, [E], [Es | P]);
	{K1, _} ->
	    partition_n(I, Ts, K1, [E], [reverse(Es) | P])
    end;
partition_n(I, [], _K, Es, P) when I > 1 ->
    sort([reverse(Es) | P]);
partition_n(_I, [], _K, [_] = Es, P) -> % optimization
    reverse(P, [Es]);
partition_n(_I, [], _K, Es, P) ->
    reverse(P, [reverse(Es)]).

partition3_n(I, [T | Ts], Key, Keys, L1, L2)  ->
    case element(I, T) of
	K when K < Key ->
	    partition3_n(I, Ts, Key, Keys, L1, [T | L2]);
	K when K == Key ->
	    partition3_n(I, Ts, Key, Keys, [T | L1], L2);
	K ->
	    partition3_n(I, K, Ts, Keys, L1, L2, T)
    end;
partition3_n(I, _Ts, _Key, _Keys, L1, L2) when I =:= 1 ->
    [reverse(L1) | reverse(L2)];
partition3_n(_I, _Ts, _Key, _Keys, L1, L2) ->
    [sort(L1) | sort(L2)].

partition3_n(I, K, Ts, [Key | Keys], L1, L2, T) when K > Key ->
    partition3_n(I, K, Ts, Keys, L1, L2, T);
partition3_n(I, K, Ts, [Key | Keys], L1, L2, T) when K == Key ->
    partition3_n(I, Ts, Key, Keys, [T | L1], L2);
partition3_n(I, _K, Ts, [Key | Keys], L1, L2, T) ->
    partition3_n(I, Ts, Key, Keys, L1, [T | L2]);
partition3_n(I, _K, Ts, _Keys, L1, L2, T) when I =:= 1 ->
    [reverse(L1) | reverse(L2, [T | Ts])];
partition3_n(_I, _K, Ts, _Keys, L1, L2, T) ->
    [sort(L1) | sort([T | Ts ++ L2])].

partition3([Key | Keys], Tuples) ->
    partition3(Tuples, Key, Keys, [], []);
partition3(_Keys, Tuples) ->
    partition3_tail(Tuples, [], []).

partition3([{K,E} | Ts], Key, Keys, L1, L2) when K < Key ->
    partition3(Ts, Key, Keys, L1, [E | L2]);
partition3([{K,E} | Ts], Key, Keys, L1, L2) when K == Key ->
    partition3(Ts, Key, Keys, [E | L1], L2);
partition3([{K,E} | Ts], _Key, Keys, L1, L2) ->
    partition3(Ts, K, Keys, L1, L2, E);
partition3(_Ts, _Key, _Keys, L1, L2) ->
    [L1 | L2].

partition3(Ts, K, [Key | Keys], L1, L2, E) when K > Key ->
    partition3(Ts, K, Keys, L1, L2, E);
partition3(Ts, K, [Key | Keys], L1, L2, E) when K == Key ->
    partition3(Ts, Key, Keys, [E | L1], L2);
partition3(Ts, _K, [Key | Keys], L1, L2, E) ->
    partition3(Ts, Key, Keys, L1, [E | L2]);
partition3(Ts, _K, _Keys, L1, L2, E) ->
    partition3_tail(Ts, L1, [E | L2]).

partition3_tail([{_K,E} | Ts], L1, L2) ->
    partition3_tail(Ts, L1, [E | L2]);
partition3_tail(_Ts, L1, L2) ->
    [L1 | L2].

replace([E | Es], F, L) ->
    replace(Es, F, [F(E) | L]);
replace(_, _F, L) ->
    sort(L).

mul_relprod([T | Ts], I, R) when ?IS_SET(T) ->
    P = raise_element(R, I),
    F = relative_product1(P, T),
    [F | mul_relprod(Ts, I+1, R)];
mul_relprod([], _I, _R) ->
    [].

raise_element(R, I) ->
    L = sort(I =/= 1, rearr(?LIST(R), I, [])),
    Type = ?TYPE(R),
    ?SET(L, ?BINREL(?REL_TYPE(I, Type), Type)).

rearr([E | Es], I, L) ->
    rearr(Es, I, [{element(I, E), E} | L]);
rearr([], _I, L) ->
    L.

join_element(E1, E2) ->
    [_ | L2] = tuple_to_list(E2),
    list_to_tuple(tuple_to_list(E1) ++ L2).

join_element(E1, E2, I2) ->
    tuple_to_list(E1) ++ join_element2(tuple_to_list(E2), 1, I2).

join_element2([B | Bs], C, I2) when C =/= I2 ->
    [B | join_element2(Bs, C+1, I2)];
join_element2([_ | Bs], _C, _I2) ->
    Bs.

family2rel([{X,S} | F], L) ->
    fam2rel(F, L, X, S);
family2rel([], L) ->
    reverse(L).

fam2rel(F, L, X, [Y | Ys]) ->
    fam2rel(F, [{X,Y} | L], X, Ys);
fam2rel(F, L, _X, _) ->
    family2rel(F, L).

fam_spec([{_,S}=E | F], Fun, Type, L) ->
    case Fun(?SET(S, Type)) of
        true ->
            fam_spec(F, Fun, Type, [E | L]);
        false ->
            fam_spec(F, Fun, Type, L);
	_ ->
	    badarg
    end;
fam_spec([], _Fun, _Type, L) ->
    reverse(L).

fam_specification([{_,S}=E | F], Fun, L) ->
    case Fun(S) of
        true ->
            fam_specification(F, Fun, [E | L]);
        false ->
            fam_specification(F, Fun, L);
	_ ->
	    badarg
    end;
fam_specification([], _Fun, L) ->
    reverse(L).

un_of_fam([{_X,S} | F], L) ->
    un_of_fam(F, [S | L]);
un_of_fam([], L) ->
    lunion(sort(L)).

int_of_fam([{_,S} | F]) ->
    int_of_fam(F, [S]);
int_of_fam([]) ->
    badarg.

int_of_fam([{_,S} | F], L) ->
    int_of_fam(F, [S | L]);
int_of_fam([], [L | Ls]) ->
    lintersection(Ls, L).

fam_un([{X,S} | F], L) ->
    fam_un(F, [{X, lunion(S)} | L]);
fam_un([], L) ->
    reverse(L).

fam_int([{X, [S | Ss]} | F], L) ->
    fam_int(F, [{X, lintersection(Ss, S)} | L]);
fam_int([{_X,[]} | _F], _L) ->
    badarg;
fam_int([], L) ->
    reverse(L).

fam_dom([{X,S} | F], L) ->
    fam_dom(F, [{X, dom(S)} | L]);
fam_dom([], L) ->
    reverse(L).

fam_ran([{X,S} | F], L) ->
    fam_ran(F, [{X, ran(S, [])} | L]);
fam_ran([], L) ->
    reverse(L).

fam_union(F1 = [{A,_AS} | _AL], [B1={B,_BS} | BL], L) when A > B ->
    fam_union(F1, BL, [B1 | L]);
fam_union([{A,AS} | AL], [{B,BS} | BL], L) when A == B ->
    fam_union(AL, BL, [{A, umerge(AS, BS)} | L]);
fam_union([A1 | AL], F2, L) ->
    fam_union(AL, F2, [A1 | L]);
fam_union(_, F2, L) ->
    reverse(L, F2).

fam_intersect(F1 = [{A,_AS} | _AL], [{B,_BS} | BL], L) when A > B ->
    fam_intersect(F1, BL, L);
fam_intersect([{A,AS} | AL], [{B,BS} | BL], L) when A == B ->
    fam_intersect(AL, BL, [{A, intersection(AS, BS, [])} | L]);
fam_intersect([_A1 | AL], F2, L) ->
    fam_intersect(AL, F2, L);
fam_intersect(_, _, L) ->
    reverse(L).

fam_difference(F1 = [{A,_AS} | _AL], [{B,_BS} | BL], L) when A > B ->
    fam_difference(F1, BL, L);
fam_difference([{A,AS} | AL], [{B,BS} | BL], L) when A == B ->
    fam_difference(AL, BL, [{A, difference(AS, BS, [])} | L]);
fam_difference([A1 | AL], F2, L) ->
    fam_difference(AL, F2, [A1 | L]);
fam_difference(F1, _, L) ->
    reverse(L, F1).

check_function([{X,_} | XL], R) ->
    check_function(X, XL, R);
check_function([], R) ->
    R.

check_function(X0, [{X,_} | XL], R) when X0 /= X ->
    check_function(X, XL, R);
check_function(X0, [{X,_} | _XL], _R) when X0 == X ->
    bad_function;
check_function(_X0, [], R) ->
    R.

fam_partition_n(I, [E | Ts]) ->
    fam_partition_n(I, Ts, element(I, E), [E], []);
fam_partition_n(_I, []) ->
    [].

fam_partition_n(I, [E | Ts], K, Es, P) ->
    case {element(I, E), Es} of
	{K1, _} when K == K1 ->
	    fam_partition_n(I, Ts, K, [E | Es], P);
	{K1, [_]} -> % optimization
	    fam_partition_n(I, Ts, K1, [E], [{K,Es} | P]);
	{K1, _} ->
	    fam_partition_n(I, Ts, K1, [E], [{K,reverse(Es)} | P])
    end;
fam_partition_n(_I, [], K, [_] = Es, P) -> % optimization
    reverse(P, [{K,Es}]);
fam_partition_n(_I, [], K, Es, P) ->
    reverse(P, [{K,reverse(Es)}]).

fam_partition([{K,Vs} | Ts], Sort) ->
    fam_partition(Ts, K, [Vs], [], Sort);
fam_partition([], _Sort) ->
    [].

fam_partition([{K1,V} | Ts], K, Vs, P, S) when K1 == K ->
    fam_partition(Ts, K, [V | Vs], P, S);
fam_partition([{K1,V} | Ts], K, [_] = Vs, P, S) -> % optimization
    fam_partition(Ts, K1, [V], [{K, Vs} | P], S);
fam_partition([{K1,V} | Ts], K, Vs, P, S) ->
    fam_partition(Ts, K1, [V], [{K, sort(S, Vs)} | P], S);
fam_partition([], K, [_] = Vs, P, _S) -> % optimization
    [{K, Vs} | P];
fam_partition([], K, Vs, P, S) ->
    [{K, sort(S, Vs)} | P].

fam_proj([{X,S} | F], Fun, Type, NType, L) ->
    case setfun(S, Fun, Type, NType) of
	{SD, ST} -> fam_proj(F, Fun, Type, ST, [{X, SD} | L]);
	Bad -> Bad
    end;
fam_proj([], _Fun, _Type, NType, L) ->
    {reverse(L), NType}.

setfun(T, Fun, Type, NType) ->
    case Fun(term2set(T, Type)) of
	NS when ?IS_SET(NS) ->
	    case unify_types(NType, ?SET_OF(?TYPE(NS))) of
		[] -> type_mismatch;
		NT -> {?LIST(NS), NT}
	    end;
	NS when ?IS_ORDSET(NS) ->
	    case unify_types(NType, NT = ?ORDTYPE(NS)) of
		[] -> type_mismatch;
		NT -> {?ORDDATA(NS), NT}
	    end;
	_ ->
	    badarg
    end.

%% Inlined.
term2set(L, Type) when is_list(L) ->
    ?SET(L, Type);
term2set(T, Type) ->
    ?ORDSET(T, Type).

fam2digraph(F, G) ->
    Fun = fun({From, ToL}) ->
                  digraph:add_vertex(G, From),
                  Fun2 = fun(To) ->
                                 digraph:add_vertex(G, To),
                                 case digraph:add_edge(G, From, To) of
                                     {error, {bad_edge, _}} ->
                                         throw({error, cyclic});
                                     _ ->
                                         true
                                 end
                         end,
                  foreach(Fun2, ToL)
          end,
    foreach(Fun, to_external(F)),
    G.

digraph_family(G) ->
    Vs = sort(digraph:vertices(G)),
    digraph_fam(Vs, Vs, G, []).

digraph_fam([V | Vs], V0, G, L) when V /= V0 ->
    Ns = sort(digraph:out_neighbours(G, V)),
    digraph_fam(Vs, V, G, [{V,Ns} | L]);
digraph_fam([], _V0, _G, L) ->
    reverse(L).

%% -> boolean()
check_fun(T, F, FunT) ->
    true = is_type(FunT),
    {NT, _MaxI} = number_tuples(T, 1),
    L = flatten(tuple2list(F(NT))),
    has_hole(L, 1).

number_tuples(T, N) when is_tuple(T) ->
    {L, NN} = mapfoldl(fun number_tuples/2, N, tuple_to_list(T)),
    {list_to_tuple(L), NN};
number_tuples(_, N) ->
    {N, N+1}.

tuple2list(T) when is_tuple(T) ->
    map(fun tuple2list/1, tuple_to_list(T));
tuple2list(C) ->
    [C].

has_hole([I | Is], I0) when I =< I0 -> has_hole(Is, erlang:max(I+1, I0));
has_hole(Is, _I) -> Is =/= [].

%% Optimization. Same as check_fun/3, but for integers.
check_for_sort(T, _I) when T =:= ?ANYTYPE ->
    empty;
check_for_sort(T, I) when ?IS_RELATION(T), I =< ?REL_ARITY(T), I >= 1 ->
    I > 1;
check_for_sort(_T, _I) ->
    error.

inverse_substitution(L, Fun, Sort) ->
    %% One easily sees that the inverse of the tuples created by
    %% applying Fun need to be sorted iff the tuples created by Fun
    %% need to be sorted.
    sort(Sort, fun_rearr(L, Fun, [])).

fun_rearr([E | Es], Fun, L) ->
    fun_rearr(Es, Fun, [{Fun(E), E} | L]);
fun_rearr([], _Fun, L) ->
    L.

sets_to_list(Ss) ->
    map(fun(S) when ?IS_SET(S) -> ?LIST(S) end, Ss).

types([], L) ->
    list_to_tuple(reverse(L));
types([S | _Ss], _L) when ?TYPE(S) =:= ?ANYTYPE ->
    ?ANYTYPE;
types([S | Ss], L) ->
    types(Ss, [?TYPE(S) | L]).

%% Inlined.
unify_types(T, T) -> T;
unify_types(Type1, Type2) ->
    catch unify_types1(Type1, Type2).

unify_types1(Atom, Atom) when ?IS_ATOM_TYPE(Atom) ->
    Atom;
unify_types1(?ANYTYPE, Type) ->
    Type;
unify_types1(Type, ?ANYTYPE) ->
    Type;
unify_types1(?SET_OF(Type1), ?SET_OF(Type2)) ->
    [unify_types1(Type1, Type2)];
unify_types1(T1, T2) when tuple_size(T1) =:= tuple_size(T2) ->
    unify_typesl(tuple_size(T1), T1, T2, []);
unify_types1(_T1, _T2) ->
    throw([]).

unify_typesl(0, _T1, _T2, L) ->
    list_to_tuple(L);
unify_typesl(N, T1, T2, L) ->
    T = unify_types1(?REL_TYPE(N, T1), ?REL_TYPE(N, T2)),
    unify_typesl(N-1, T1, T2, [T | L]).

%% inlined.
match_types(T, T) -> true;
match_types(Type1, Type2) -> match_types1(Type1, Type2).

match_types1(Atom, Atom) when ?IS_ATOM_TYPE(Atom) ->
    true;
match_types1(?ANYTYPE, _) ->
    true;
match_types1(_, ?ANYTYPE) ->
    true;
match_types1(?SET_OF(Type1), ?SET_OF(Type2)) ->
    match_types1(Type1, Type2);
match_types1(T1, T2) when tuple_size(T1) =:= tuple_size(T2) ->
    match_typesl(tuple_size(T1), T1, T2);
match_types1(_T1, _T2) ->
    false.

match_typesl(0, _T1, _T2) ->
    true;
match_typesl(N, T1, T2) ->
    case match_types1(?REL_TYPE(N, T1), ?REL_TYPE(N, T2)) of
        true  -> match_typesl(N-1, T1, T2);
        false -> false
    end.

sort(true, L) ->
    sort(L);
sort(false, L) ->
    reverse(L).
