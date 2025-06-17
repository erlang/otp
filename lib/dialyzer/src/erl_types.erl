%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2004-2010 held by the authors. All Rights Reserved.
%% Copyright Ericsson AB 2020-2025. All Rights Reserved.
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
%% @copyright 2000-2003 Richard Carlsson, 2006-2009 Tobias Lindahl
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @author Tobias Lindahl <tobias.lindahl@gmail.com>
%% @author Kostis Sagonas <kostis@cs.ntua.gr>
%% @author Manouk Manoukian
%% @doc Provides a representation of Erlang types.

%% The initial author of this file is Richard Carlsson (2000-2004).
%% In July 2006, the type representation was totally re-designed by
%% Tobias Lindahl. This is the representation which is used currently.
%% In late 2008, Manouk Manoukian and Kostis Sagonas added support for
%% opaque types to the structure-based representation of types.
%% During February and March 2009, Kostis Sagonas significantly
%% cleaned up the type representation and added spec declarations.

-module(erl_types).
-moduledoc false.

-export([any_none/1,
	 any_none_or_unit/1,
	 lookup_record/3,
	 max/2,
	 min/2,
	 number_max/1,
	 number_min/1,
	 t_abstract_records/2,
	 t_any/0,
	 t_arity/0,
	 t_atom/0,
	 t_atom/1,
	 t_atoms/1,
	 t_atom_vals/1,
	 t_binary/0,
	 t_bitstr/0,
	 t_bitstr/2,
	 t_bitstr_base/1,
	 t_bitstr_concat/1,
	 t_bitstr_concat/2,
	 t_bitstr_match/2,
	 t_bitstr_unit/1,
	 t_bitstrlist/0,
	 t_boolean/0,
	 t_byte/0,
	 t_char/0,
	 t_collect_var_names/1,
	 t_cons/0,
	 t_cons/2,
	 t_cons_hd/1,
	 t_cons_tl/1,
	 t_elements/1,
	 t_fixnum/0,
	 t_non_neg_fixnum/0,
	 t_pos_fixnum/0,
	 t_float/0,
         t_var_names/1,
	 t_form_to_string/1,
         t_from_form/6,
         t_from_form_without_remote/3,
         t_from_form_check_remote/4,
         t_check_record_fields/6,
	 t_from_range/2,
	 t_from_term/1,
	 t_fun/0,
	 t_fun/1,
	 t_fun/2,
	 t_fun_args/1,
	 t_fun_arity/1,
	 t_fun_range/1,
	 t_has_var/1,
	 t_identifier/0,
	 %% t_improper_list/2,
         t_inf/1,
         t_inf/2,
         t_inf_lists/2,
	 t_integer/0,
	 t_integer/1,
	 t_non_neg_integer/0,
	 t_pos_integer/0, t_neg_integer/0,
	 t_integers/1,
	 t_iodata/0,
	 t_iolist/0,
	 t_is_any/1,
	 t_is_atom/1,
	 t_is_any_atom/2,
	 t_is_binary/1,
	 t_is_bitstr/1,
	 t_is_boolean/1,
         t_is_byte/1,
         t_is_char/1,
	 t_is_cons/1,
	 t_is_equal/2,
	 t_is_float/1,
	 t_is_fun/1,
         t_is_identifier/1,
         t_is_impossible/1,
	 t_is_integer/1,
	 t_is_list/1,
	 t_is_map/1,
	 t_is_nil/1,
	 t_is_non_neg_integer/1,
	 t_is_none/1,
	 t_is_none_or_unit/1,
	 t_is_number/1,
         t_is_opaque/1,
         t_is_opaque/2,
	 t_is_pid/1,
	 t_is_port/1,
	 t_is_maybe_improper_list/1,
	 t_is_reference/1,
         t_is_same_opaque/2,
	 t_is_singleton/1,
	 t_is_string/1,
	 t_is_subtype/2,
	 t_is_tuple/1,
	 t_is_unit/1,
	 t_is_var/1,
	 t_limit/2,
	 t_list/0,
	 t_list/1,
	 t_list_elements/1,
	 t_list_termination/1,
	 t_map/0,
	 t_map/1,
	 t_map/3,
	 t_map_entries/1,
	 t_map_def_key/1,
	 t_map_def_val/1,
	 t_map_get/2,
	 t_map_is_key/2,
	 t_map_update/2,
	 t_map_pairwise_merge/3,
	 t_map_put/2,
         t_map_remove/2,
	 t_mfa/0,
	 t_module/0,
	 t_nil/0,
	 t_node/0,
         t_nominal/2,
         t_nominal_module/1,
	 t_none/0,
	 t_nonempty_binary/0,
	 t_nonempty_bitstring/0,
	 t_nonempty_list/0,
	 t_nonempty_list/1,
	 t_nonempty_string/0,
	 t_number/0,
	 t_number/1,
	 t_number_vals/1,
         t_opacity_conflict/3,
	 t_pid/0,
	 t_port/0,
	 t_maybe_improper_list/0,
	 t_product/1,
	 t_reference/0,
	 t_string/0,
	 t_subst/2,
	 t_subtract/2,
	 t_subtract_list/2,
	 t_sup/1,
	 t_sup/2,
	 t_timeout/0,
	 t_to_string/1,
	 t_to_string/2,
	 t_to_tlist/1,
	 t_tuple/0,
	 t_tuple/1,
	 t_tuple_args/1,
	 t_tuple_size/1,
	 t_tuple_sizes/1,
	 t_tuple_subtypes/1,
	 t_unify_table_only/2,
	 t_unit/0,
	 t_structural/1,
	 t_var/1,
	 t_var_name/1,
         t_widen_to_number/1,
	 type_is_defined/4,
	 record_field_diffs_to_string/2,
	 subst_all_vars_to_any/1,
         lift_list_to_pos_empty/1,
	 is_erl_type/1,
	 atom_to_string/1,
	 var_table__new/0,
	 cache__new/0,
	 module_type_deps_of_type_defs/1,
   type_form_to_remote_modules/1
	]).

-compile({no_auto_import,[min/2,max/2,map_get/2]}).

-compile({no_auto_import,[is_boolean/1, is_binary/1, is_number/1]}).

-export_type([erl_type/0, type_table/0, var_table/0, cache/0]).

%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(__A, __B), case __A of true -> ok; false -> error(__B) end).
-else.
-define(debug(__A, __B), ok).
-endif.

%%=============================================================================
%%
%% Definition of the type structure
%%
%%=============================================================================

%%-----------------------------------------------------------------------------
%% Limits
%%

-define(REC_TYPE_LIMIT, 2).
-define(EXPAND_DEPTH, 16).
-define(EXPAND_LIMIT, 10000).

-define(TUPLE_TAG_LIMIT, 5).
-define(TUPLE_ARITY_LIMIT, 8).
-define(SET_LIMIT, 13).
-define(MAX_BYTE, 255).
-define(MAX_CHAR, 16#10ffff).

-define(UNIT_MULTIPLIER, 8).

-define(MAX_TUPLE_SIZE, (1 bsl 10)).

%%-----------------------------------------------------------------------------
%% Type tags and qualifiers
%%

-define(atom_tag,       atom).
-define(binary_tag,     binary).
-define(function_tag,   function).
-define(identifier_tag, identifier).
-define(nominal_tag,    nominal).
-define(nominal_set_tag,nominal_set).
-define(list_tag,       list).
-define(map_tag,        map).
-define(nil_tag,        nil).
-define(number_tag,     number).
-define(product_tag,    product).
-define(tuple_set_tag,  tuple_set).
-define(tuple_tag,      tuple).
-define(union_tag,      union).
-define(var_tag,        var).

-type tag()  :: ?atom_tag | ?binary_tag | ?function_tag | ?identifier_tag
              | ?list_tag | ?map_tag | ?nil_tag | ?number_tag
              | ?nominal_tag | ?nominal_set_tag
              | ?product_tag
              | ?tuple_tag | ?tuple_set_tag | ?union_tag | ?var_tag.

-define(float_qual,     float).
-define(integer_qual,   integer).
-define(nonempty_qual,  nonempty).
-define(pid_qual,       pid).
-define(port_qual,      port).
-define(reference_qual, reference).
-define(unknown_qual,   unknown).

-type qual() :: ?float_qual | ?integer_qual | ?nonempty_qual | ?pid_qual
              | ?port_qual | ?reference_qual | ?unknown_qual | {_, _}.

%%-----------------------------------------------------------------------------
%% The type representation
%%

%% Top type
-define(any, any).

%% Bottom type
-define(none, none).

%% Special type used to mark infinite loops: functions are assumed to return
%% a supertype of ?unit rather than ?none during analysis, letting us
%% distingish between functions that intentionally never return (like server
%% loops) and functions that never return because of a crash.
-define(unit, unit).

%% Special type used to mark opaque nominals during opacity violation checking.
-define(opaque, opaque).

%% Generic constructor - elements can be many things depending on the tag.
-record(c, {tag                       :: tag(),
            elements  = []            :: term(),
            qualifier = ?unknown_qual :: qual()}).

-nominal erl_type() :: ?any | ?none | ?unit | ?opaque | #c{}.

%%-----------------------------------------------------------------------------
%% Auxiliary types and convenient macros
%%

-type parse_form() :: erl_parse:abstract_type().
-type rng_elem()   :: 'pos_inf' | 'neg_inf' | integer().

-record(int_set, {set :: [integer()]}).
-record(int_rng, {from :: rng_elem(), to :: rng_elem()}).

-define(atom(Set),                 #c{tag=?atom_tag, elements=Set}).
-define(bitstr(Unit, Base),        #c{tag=?binary_tag, elements={Unit,Base}}).
-define(float,                     ?number(?any, ?float_qual)).
-define(function(Domain, Range),   #c{tag=?function_tag,
                                      elements={Domain,Range}}).
-define(identifier(Types),         #c{tag=?identifier_tag, elements=Types}).
-define(integer(Types),            ?number(Types, ?integer_qual)).
-define(int_range(From, To),       ?integer(#int_rng{from=From, to=To})).
-define(int_set(Set),              ?integer(#int_set{set=Set})).
-define(nominal(Name, Types),      #c{tag=?nominal_tag, elements={Name,Types}}).
-define(nominal_set(Nominals,
                    Structurals),  #c{tag=?nominal_set_tag,
                                      elements={Nominals, Structurals}}).
-define(list(Types, Term, Size),   #c{tag=?list_tag, elements={Types,Term},
                                      qualifier=Size}).
-define(nil,                       #c{tag=?nil_tag}).
-define(nonempty_list(Types, Term),?list(Types, Term, ?nonempty_qual)).
-define(number(Set, Qualifier),    #c{tag=?number_tag, elements=Set,
                                      qualifier=Qualifier}).
-define(map(Pairs,DefKey,DefVal),
	#c{tag=?map_tag, elements={Pairs,DefKey,DefVal}}).
-define(product(Types),            #c{tag=?product_tag, elements=Types}).
-define(tuple(Types, Arity, Qual), #c{tag=?tuple_tag, elements=Types,
                                      qualifier={Arity, Qual}}).
-define(tuple_set(Tuples),         #c{tag=?tuple_set_tag, elements=Tuples}).
-define(var(Id),                   #c{tag=?var_tag, elements=Id}).

-define(byte,                      ?int_range(0, ?MAX_BYTE)).
-define(char,                      ?int_range(0, ?MAX_CHAR)).
-define(integer_pos,               ?int_range(1, pos_inf)).
-define(integer_non_neg,           ?int_range(0, pos_inf)).
-define(integer_neg,               ?int_range(neg_inf, -1)).

-type file_line()    :: {file:name(), erl_anno:line()}.
-type record_key()   :: {'record', atom()}.
-type type_key()     :: {'type' | 'opaque' | 'nominal', atom(), arity()}.
-type field()        :: {atom(), erl_parse:abstract_expr(), erl_type()}.
-type record_value() :: {file_line(),
                         [{RecordSize :: non_neg_integer(), [field()]}]}.
-type type_value()   :: {{module(), file_line(),
                          erl_parse:abstract_type(), ArgNames :: [atom()]},
                         erl_type()}.
-type type_table() :: #{record_key() => record_value()} |
                        #{type_key() => type_value()}.

-type var_name() :: atom() | integer().
-type var_table() :: #{ var_name() => erl_type() }.

%%-----------------------------------------------------------------------------
%% Unions
%%

-define(union(List), #c{tag=?union_tag, elements=List}).
-define(untagged_union(A, B, F, I, L, N, T, Map), [A,B,F,I,L,N,T,Map]).

-define(num_types_in_union, length(?untagged_union(?any, ?any, ?any, ?any, ?any,
                                                   ?any, ?any, ?any))).

-define(atom_union(T),       ?union([T,?none,?none,?none,?none,?none,?none,?none])).
-define(bitstr_union(T),     ?union([?none,T,?none,?none,?none,?none,?none,?none])).
-define(function_union(T),   ?union([?none,?none,T,?none,?none,?none,?none,?none])).
-define(identifier_union(T), ?union([?none,?none,?none,T,?none,?none,?none,?none])).
-define(list_union(T),       ?union([?none,?none,?none,?none,T,?none,?none,?none])).
-define(number_union(T),     ?union([?none,?none,?none,?none,?none,T,?none,?none])).
-define(tuple_union(T),      ?union([?none,?none,?none,?none,?none,?none,T,?none])).
-define(map_union(T),        ?union([?none,?none,?none,?none,?none,?none,?none,T])).
-define(integer_union(T),    ?number_union(T)).
-define(float_union(T),      ?number_union(T)).
-define(nil_union(T),        ?list_union(T)).


%%=============================================================================
%%
%% Primitive operations such as type construction and type tests
%%
%%=============================================================================

%%-----------------------------------------------------------------------------
%% Top and bottom
%%

-spec t_any() -> erl_type().

t_any() ->
  ?any.

-spec t_is_any(erl_type()) -> boolean().

t_is_any(Type) ->
  structural(Type, fun is_any/1).

is_any(?any) -> true;
is_any(_) -> false.

-spec t_none() -> erl_type().

t_none() ->
  ?none.

-spec t_is_none(erl_type()) -> boolean().

t_is_none(?none) -> true;
t_is_none(_) -> false.

%%-----------------------------------------------------------------------------
%% Opaque types
%%

%% Returns whether the `Given` type implicitly violates the opacity of opaque
%% nominals of the `Required` type.
-spec t_opacity_conflict(Given :: erl_type(),
                         Required :: erl_type(),
                         Module :: module()) ->
    none | expected_opaque | expected_transparent.
t_opacity_conflict(Given, Required, Module) ->
  %% Opacity violations are detected by selectively blinding the infimum
  %% routine to the structure of opaque types that we are not supposed to know
  %% anything about.
  %%
  %% If the infimum of the `Given` and `Required` types is possible, we replace
  %% the structural component of opaques with a magic value whose infimum with
  %% anything else becomes `none()`, forcing a failure when the original
  %% opaques introduce more information.
  %%
  %% Conversely, if the infimum of the `Given` and `Required` types is
  %% impossible, we replace the structural component of opaques with `any()` to
  %% force success when the altered opaques introduce more information (note
  %% the inversion).
  %%
  %% From there, we can detect opacity violations by checking whether the
  %% infimum of (blinded `Given`) and (blinded `Required`) is equal to the
  %% blinded infimum of `Given` and `Required`.
  Direction = case t_is_impossible(t_inf(Given, Required)) of
                true -> ?any;
                false -> ?opaque
              end,

  RequiredBlind = oc_mark(Required, Direction, Module),
  GivenBlind = oc_mark(Given, Direction, Module),

  %% If the `Required` type does not change when blinded, we know that the call
  %% expects a transparent type and not an opaque. Note that this is merely a
  %% heuristic, and we can clash in both ways at once should the types be
  %% complex enough.
  ErrorType = case t_is_equal(RequiredBlind, Required) of
                true -> expected_transparent;
                false -> expected_opaque
              end,

  case {t_is_impossible(t_inf(GivenBlind, RequiredBlind)), Direction} of
    {true, ?opaque} -> ErrorType;
    {false, ?any} -> ErrorType;
    {_, _} -> none
  end.

oc_mark(?nominal({Mod, _Name, _Arity, Opacity}=Name, S0), Direction, Module) ->
  case (Opacity =:= transparent) orelse (Mod =:= Module) of
    true -> t_nominal(Name, oc_mark(S0, Direction, Module));
    false -> t_nominal(Name, Direction)
  end;
oc_mark(?nominal_set(Ns, Other), Direction, Module) ->
  normalize_nominal_set([oc_mark(N, Direction, Module) || N <- Ns],
                        oc_mark(Other, Direction, Module),
                        []);
oc_mark(?list(ElemT, Termination, Sz), Direction, Module) ->
  ?list(oc_mark(ElemT, Direction, Module),
        oc_mark(Termination, Direction, Module), Sz);
oc_mark(?tuple(?any, _, _) = T, _Direction, _Module) ->
  T;
oc_mark(?tuple(ArgTs, Sz, Tag), Direction, Module) when is_list(ArgTs) ->
  ?tuple([oc_mark(A, Direction, Module) || A <- ArgTs], Sz, Tag);
oc_mark(?tuple_set(Set0), Direction, Module) ->
  ?tuple_set([{Sz, [oc_mark(T, Direction, Module) || T <- Tuples]}
              || {Sz, Tuples} <- Set0]);
oc_mark(?product(Types), Direction, Module) ->
  ?product([oc_mark(T, Direction, Module) || T <- Types]);
oc_mark(?function(Domain, Range), Direction, Module) ->
  ?function(oc_mark(Domain, Direction, Module),
            oc_mark(Range, Direction, Module));
oc_mark(?union(U0), Direction, Module) ->
  ?union([oc_mark(T, Direction, Module) || T <- U0]);
oc_mark(?map(Pairs, DefK, DefV), Direction, Module) ->
  %% K is always a singleton, and thus can't contain any nominals.
  t_map([{K, MNess, oc_mark(V, Direction, Module)} || {K, MNess, V} <- Pairs],
        oc_mark(DefK, Direction, Module),
        oc_mark(DefV, Direction, Module));
oc_mark(T, _Direction, _Module) ->
  T.

%%-----------------------------------------------------------------------------
%% Unit type. Signals non termination.
%%

-spec t_unit() -> erl_type().

t_unit() ->
  ?unit.

-spec t_is_unit(erl_type()) -> boolean().

t_is_unit(?unit) -> true;
t_is_unit(_) -> false.

-spec t_is_impossible(erl_type()) -> boolean().

t_is_impossible(?none) -> true;
t_is_impossible(?unit) -> true;
t_is_impossible(_) -> false.

-spec t_is_none_or_unit(erl_type()) -> boolean().

t_is_none_or_unit(T) -> t_is_impossible(T).

%%-----------------------------------------------------------------------------
%% Atoms and the derived type boolean
%%

-spec t_atom() -> erl_type().

t_atom() ->
  ?atom(?any).

-spec t_atom(atom()) -> erl_type().

t_atom(A) when is_atom(A) ->
  ?atom(set_singleton(A)).

-spec t_atoms([atom()]) -> erl_type().

t_atoms(List) when is_list(List) ->
  t_sup([t_atom(A) || A <- List]).

-spec t_atom_vals(erl_type()) -> 'unknown' | [atom(),...].

t_atom_vals(Type) ->
  structural(Type, fun atom_vals/1).

atom_vals(?atom(?any)) -> unknown;
atom_vals(?atom(Set)) -> Set;
atom_vals(Other) ->
  ?atom(_) = Atm = t_inf(t_atom(), Other),
  atom_vals(Atm).

-spec t_is_atom(erl_type()) -> boolean().

t_is_atom(Type) ->
  structural(Type, fun is_atom1/1).

is_atom1(?atom(_)) -> true;
is_atom1(_) -> false.

-spec t_is_any_atom(atom(), erl_type()) -> boolean().

t_is_any_atom(Atom, SomeAtomsType) ->
  structural(SomeAtomsType,
            fun(AtomsType) -> is_any_atom(Atom, AtomsType) end).

is_any_atom(Atom, ?atom(?any)) when is_atom(Atom) -> false;
is_any_atom(Atom, ?atom(Set)) when is_atom(Atom) ->
  set_is_singleton(Atom, Set);
is_any_atom(Atom, _) when is_atom(Atom) -> false.

%%------------------------------------

-spec t_is_boolean(erl_type()) -> boolean().

t_is_boolean(Type) ->
  structural(Type, fun is_boolean/1).

-spec t_boolean() -> erl_type().

t_boolean() ->
  ?atom(set_from_list([false, true])).

is_boolean(?atom(Set)) ->
  case Set of
    [Atom] when erlang:is_boolean(Atom) -> true;
    [false,true] -> true;
    _ -> false
  end;
is_boolean(_) -> false.


%%-----------------------------------------------------------------------------
%% Binaries
%%

-spec t_binary() -> erl_type().

t_binary() ->
  ?bitstr(8, 0).

-spec t_nonempty_binary() -> erl_type().

t_nonempty_binary() ->
  ?bitstr(8, 8).

-spec t_is_binary(erl_type()) -> boolean().

t_is_binary(Type) ->
    structural(Type, fun is_binary/1).

is_binary(?bitstr(U, B)) ->
  ((U rem 8) =:= 0) andalso ((B rem 8) =:= 0);
is_binary(_) -> false.

%%-----------------------------------------------------------------------------
%% Bitstrings
%%

-spec t_bitstr() -> erl_type().

t_bitstr() ->
  ?bitstr(1, 0).

-spec t_nonempty_bitstring() -> erl_type().

t_nonempty_bitstring() ->
  ?bitstr(1, 1).

-spec t_bitstr(non_neg_integer(), non_neg_integer()) -> erl_type().

t_bitstr(U, B) ->
  NewB =
    if
      U =:= 0 -> B;
      B >= (U * (?UNIT_MULTIPLIER + 1)) ->
	(B rem U) + U * ?UNIT_MULTIPLIER;
      true ->
	B
    end,
  ?bitstr(U, NewB).

-spec t_bitstr_unit(erl_type()) -> non_neg_integer().

t_bitstr_unit(?bitstr(U, _)) -> U.

-spec t_bitstr_base(erl_type()) -> non_neg_integer().

t_bitstr_base(?bitstr(_, B)) -> B.

-spec t_bitstr_concat([erl_type()]) -> erl_type().

t_bitstr_concat(List) ->
  t_bitstr_concat_1(List, t_bitstr(0, 0)).

t_bitstr_concat_1([T|Left], Acc) ->
  t_bitstr_concat_1(Left, t_bitstr_concat(Acc, T));
t_bitstr_concat_1([], Acc) ->
  Acc.

-spec t_bitstr_concat(erl_type(), erl_type()) -> erl_type().

t_bitstr_concat(T1, T2) ->
  T1p = t_inf(t_bitstr(), T1),
  T2p = t_inf(t_bitstr(), T2),
  bitstr_concat(t_structural(T1p), t_structural(T2p)).

-spec t_bitstr_match(erl_type(), erl_type()) -> erl_type().

t_bitstr_match(T1, T2) ->
  T1p = t_inf(t_bitstr(), T1),
  T2p = t_inf(t_bitstr(), T2),
  bitstr_match(t_structural(T1p), t_structural(T2p)).

-spec t_is_bitstr(erl_type()) -> boolean().

t_is_bitstr(Type) ->
    structural(Type, fun is_bitstr/1).

is_bitstr(?bitstr(_, _)) -> true;
is_bitstr(_) -> false.

%%-----------------------------------------------------------------------------
%% Functions
%%

-spec t_fun() -> erl_type().

t_fun() ->
  ?function(?any, ?any).

-spec t_fun(erl_type()) -> erl_type().

t_fun(Range) ->
  ?function(?any, Range).

-spec t_fun([erl_type()] | arity(), erl_type()) -> erl_type().

t_fun(Domain, Range) when is_list(Domain) ->
  ?function(?product(Domain), Range);
t_fun(Arity, Range) when is_integer(Arity), 0 =< Arity, Arity =< 255 ->
  ?function(?product(lists:duplicate(Arity, ?any)), Range).

-spec t_fun_args(erl_type()) -> 'unknown' | [erl_type()].

t_fun_args(Type) ->
  structural(Type, fun fun_args/1).

fun_args(?function(?any, _)) ->
  unknown;
fun_args(?function(?product(Domain), _)) when is_list(Domain) ->
  Domain.

-spec t_fun_arity(erl_type()) -> 'unknown' | non_neg_integer().

t_fun_arity(Type) ->
  structural(Type, fun fun_arity/1).

fun_arity(?function(?any, _)) ->
  unknown;
fun_arity(?function(?product(Domain), _)) ->
  length(Domain).

-spec t_fun_range(erl_type()) -> erl_type().

t_fun_range(Type) ->
  structural(Type, fun fun_range/1).

fun_range(?function(_, Range)) ->
  Range.

-spec t_is_fun(erl_type()) -> boolean().

t_is_fun(Type) ->
  structural(Type, fun is_fun/1).

is_fun(?function(_, _)) -> true;
is_fun(_) -> false.

%%-----------------------------------------------------------------------------
%% Identifiers. Includes ports, pids and refs.
%%

-spec t_identifier() -> erl_type().

t_identifier() ->
  ?identifier(?any).

-spec t_is_identifier(erl_type()) -> boolean().

t_is_identifier(?identifier(_)) -> true;
t_is_identifier(_) -> false.

%%------------------------------------

-spec t_port() -> erl_type().

t_port() ->
  ?identifier(set_singleton(?port_qual)).

-spec t_is_port(erl_type()) -> boolean().

t_is_port(Type) ->
  structural(Type, fun is_port1/1).

is_port1(?identifier(?any)) -> false;
is_port1(?identifier(Set)) -> set_is_singleton(?port_qual, Set);
is_port1(_) -> false.

%%------------------------------------

-spec t_pid() -> erl_type().

t_pid() ->
  ?identifier(set_singleton(?pid_qual)).

-spec t_is_pid(erl_type()) -> boolean().

t_is_pid(Type) ->
  structural(Type, fun is_pid1/1).

is_pid1(?identifier(?any)) -> false;
is_pid1(?identifier(Set)) -> set_is_singleton(?pid_qual, Set);
is_pid1(_) -> false.

%%------------------------------------

-spec t_reference() -> erl_type().

t_reference() ->
  ?identifier(set_singleton(?reference_qual)).

-spec t_is_reference(erl_type()) -> boolean().

t_is_reference(Type) ->
  structural(Type, fun is_reference1/1).

is_reference1(?identifier(?any)) -> false;
is_reference1(?identifier(Set)) -> set_is_singleton(?reference_qual, Set);
is_reference1(_) -> false.

%%-----------------------------------------------------------------------------
%% Numbers are divided into floats, integers, chars and bytes.
%%

-spec t_number() -> erl_type().

t_number() ->
  ?number(?any, ?unknown_qual).

-spec t_number(integer()) -> erl_type().

t_number(X) when is_integer(X) ->
  t_integer(X).

-spec t_is_number(erl_type()) -> boolean().

t_is_number(Type) ->
  structural(Type, fun is_number/1).

is_number(?number(_, _)) -> true;
is_number(_) -> false.

%% Currently, the type system collapses all floats to ?float and does
%% not keep any information about their values. As a result, the list
%% that this function returns contains only integers.

-spec t_number_vals(erl_type()) -> 'unknown' | [integer(),...].

t_number_vals(Type) ->
  structural(Type, fun number_vals/1).

number_vals(?int_set(Set)) -> Set;
number_vals(?number(_, _)) -> unknown;
number_vals(Other) ->
  Inf = t_inf(Other, t_number()),
  false = t_is_none(Inf), % sanity check
  number_vals(Inf).

%%------------------------------------

-spec t_float() -> erl_type().

t_float() ->
  ?float.

-spec t_is_float(erl_type()) -> boolean().

t_is_float(Type) ->
  structural(Type, fun is_float1/1).

is_float1(?float) -> true;
is_float1(_) -> false.

%%------------------------------------

-spec t_integer() -> erl_type().

t_integer() ->
  ?integer(?any).

-spec t_integer(integer()) -> erl_type().

t_integer(I) when is_integer(I) ->
  ?int_set(set_singleton(I)).

-spec t_integers([integer()]) -> erl_type().

t_integers(List) when is_list(List) ->
  t_sup([t_integer(I) || I <- List]).

-spec t_is_integer(erl_type()) -> boolean().

t_is_integer(Type) ->
  structural(Type, fun is_integer1/1).

is_integer1(?integer(_)) -> true;
is_integer1(_) -> false.

%%------------------------------------

-spec t_byte() -> erl_type().

t_byte() ->
  ?byte.

-spec t_is_byte(erl_type()) -> boolean().

t_is_byte(?int_range(neg_inf, _)) -> false;
t_is_byte(?int_range(_, pos_inf)) -> false;
t_is_byte(?int_range(From, To))
  when is_integer(From), From >= 0, is_integer(To), To =< ?MAX_BYTE -> true;
t_is_byte(?int_set(Set)) ->
  (set_min(Set) >= 0) andalso (set_max(Set) =< ?MAX_BYTE);
t_is_byte(_) -> false.

%%------------------------------------

-spec t_char() -> erl_type().

t_char() ->
  ?char.

-spec t_is_char(erl_type()) -> boolean().

t_is_char(?int_range(neg_inf, _)) -> false;
t_is_char(?int_range(_, pos_inf)) -> false;
t_is_char(?int_range(From, To))
  when is_integer(From), From >= 0, is_integer(To), To =< ?MAX_CHAR -> true;
t_is_char(?int_set(Set)) ->
  (set_min(Set) >= 0) andalso (set_max(Set) =< ?MAX_CHAR);
t_is_char(_) -> false.

%%-----------------------------------------------------------------------------
%% Lists
%%

-spec t_cons() -> erl_type().

t_cons() ->
  ?nonempty_list(?any, ?any).

%% Note that if the tail argument can be a list, we must collapse the
%% content of the list to include both the content of the tail list
%% and the head of the cons. If for example the tail argument is any()
%% then there can be any list in the tail and the content of the
%% returned list must be any().

-spec t_cons(erl_type(), erl_type()) -> erl_type().

t_cons(?none,  _) -> ?none;
t_cons(_, ?none) -> ?none;
t_cons(?unit, _) -> ?none;
t_cons(_, ?unit) -> ?none;
t_cons(Hd, ?nil) ->
  ?nonempty_list(Hd, ?nil);
t_cons(Hd, ?list(Contents, Termination, _)) ->
  ?nonempty_list(t_sup(Contents, Hd), Termination);
t_cons(Hd, Tail) ->
  case cons_tail(t_inf(Tail, t_maybe_improper_list())) of
    ?list(Contents, Termination, _Size) ->
      %% Collapse the list part of the termination but keep the
      %% non-list part intact.
      NewTermination = t_sup(t_subtract(Tail, t_maybe_improper_list()),
			     Termination),
      ?nonempty_list(t_sup(Hd, Contents), NewTermination);
    ?nil -> ?nonempty_list(Hd, Tail);
    ?none -> ?nonempty_list(Hd, Tail);
    ?unit -> ?none
  end.

cons_tail(Type) ->
  structural(Type, fun(T) -> T end).

-spec t_is_cons(erl_type()) -> boolean().

t_is_cons(Type) ->
  structural(Type, fun is_cons/1).

is_cons(?nonempty_list(_, _)) -> true;
is_cons(_) -> false.

-spec t_cons_hd(erl_type()) -> erl_type().

t_cons_hd(Type) ->
  structural(Type, fun cons_hd/1).

cons_hd(?nonempty_list(Contents, _Termination)) -> Contents.

-spec t_cons_tl(erl_type()) -> erl_type().

t_cons_tl(Type) ->
  structural(Type, fun cons_tl/1).

cons_tl(?nonempty_list(_Contents, Termination) = T) ->
  t_sup(Termination, T).

-spec t_nil() -> erl_type().

t_nil() ->
  ?nil.

-spec t_is_nil(erl_type()) -> boolean().

t_is_nil(Type) ->
  structural(Type, fun is_nil/1).

is_nil(?nil) -> true;
is_nil(_) -> false.

-spec t_nominal(any(), erl_type()) -> erl_type().

t_nominal(Name, Type) ->
  case not t_is_impossible(Type) of
    true -> ?nominal(Name, Type);
    false -> ?none
  end.

-spec t_nominal_module(erl_type()) -> term().

t_nominal_module(?nominal({Module, _, _, _},_)) -> Module.

-ifdef(DEBUG).
-spec t_is_nominal(erl_type()) -> boolean().

t_is_nominal(?nominal_set(_,?none)) -> true;
t_is_nominal(?nominal(_,_)) -> true; 
t_is_nominal(_) -> false. 
-endif.

-spec t_is_opaque(erl_type()) -> boolean().

t_is_opaque(?nominal({_,_,_,opaque},_)) -> true; 
t_is_opaque(_) -> false. 

-spec t_is_opaque(erl_type(), module()) -> boolean().

t_is_opaque(?nominal({ModA,_,_,opaque},_), ModB) ->
  ModA =/= ModB;
t_is_opaque(?nominal_set(Ns, ?none), Mod) ->
  %% This is a relaxed check to reduce noise; there are many benign violations
  %% of opacity throughout OTP and user code where we have a union of an opaque
  %% type and a structural one that doesn't overlap.
  lists:any(fun(N) -> t_is_opaque(N, Mod) end, Ns);
t_is_opaque(_, _) ->
  false.

-spec t_is_same_opaque(erl_type(), erl_type()) -> boolean().

t_is_same_opaque(?nominal({_,_,_,opaque}=Same,_), ?nominal(Same,_)) ->
  true;
t_is_same_opaque(?nominal({_,_,_,opaque},_), ?nominal({_,_,_,opaque},_)) ->
  false.

-spec t_list() -> erl_type().

t_list() ->
  ?list(?any, ?nil, ?unknown_qual).

-spec t_list(erl_type()) -> erl_type().

t_list(Contents) ->
  t_sup(t_nonempty_list(Contents), t_nil()).

-spec t_list_elements(erl_type()) -> erl_type().

t_list_elements(Type) ->
  structural(Type, fun list_elements/1).

list_elements(?list(Contents, _, _)) -> Contents;
list_elements(?nil) -> ?none.

-spec t_list_termination(erl_type()) -> erl_type().

t_list_termination(Type) ->
  structural(Type, fun list_termination/1).

list_termination(?nil) -> ?nil;
list_termination(?list(_, Term, _)) -> Term.

-spec t_is_list(erl_type()) -> boolean().

t_is_list(?list(_Contents, ?nil, _)) -> true;
t_is_list(?nil) -> true;
t_is_list(_) -> false.

-spec t_nonempty_list() -> erl_type().

t_nonempty_list() ->
  t_cons(?any, ?nil).

-spec t_nonempty_list(erl_type()) -> erl_type().

t_nonempty_list(Type) ->
  t_cons(Type, ?nil).

-spec t_nonempty_string() -> erl_type().

t_nonempty_string() ->
  t_nonempty_list(t_char()).

-spec t_string() -> erl_type().

t_string() ->
  t_list(t_char()).

-spec t_is_string(erl_type()) -> boolean().

t_is_string(X) ->
  t_is_list(X) andalso t_is_char(t_list_elements(X)).

-spec t_maybe_improper_list() -> erl_type().

t_maybe_improper_list() ->
  ?list(?any, ?any, ?unknown_qual).

%% Should only be used if you know what you are doing. See t_cons/2
-spec t_maybe_improper_list(erl_type(), erl_type()) -> erl_type().

t_maybe_improper_list(_Content, ?unit) -> ?none;
t_maybe_improper_list(?unit, _Termination) -> ?none;
t_maybe_improper_list(_Content, ?none) -> ?none;
t_maybe_improper_list(?none, _Termination) -> ?none;
t_maybe_improper_list(Content, Termination) ->
  %% Safety check: would be nice to have but does not work with remote types
  %% true = t_is_subtype(t_nil(), Termination),
  ?list(Content, Termination, ?unknown_qual).

-spec t_is_maybe_improper_list(erl_type()) -> boolean().

t_is_maybe_improper_list(Type) ->
  structural(Type, fun is_maybe_improper_list/1).

is_maybe_improper_list(?list(_, _, _)) -> true;
is_maybe_improper_list(?nil) -> true;
is_maybe_improper_list(_) -> false.

%% %% Should only be used if you know what you are doing. See t_cons/2
%% -spec t_improper_list(erl_type(), erl_type()) -> erl_type().
%%
%% t_improper_list(?unit, _Termination) -> ?none;
%% t_improper_list(_Content, ?unit) -> ?none;
%% t_improper_list(Content, Termination) ->
%%   %% Safety check: would be nice to have but does not work with remote types
%%   %% false = t_is_subtype(t_nil(), Termination),
%%   ?list(Content, Termination, ?any).

-spec lift_list_to_pos_empty(erl_type()) -> erl_type().

lift_list_to_pos_empty(Type) ->
  structural(Type, fun lift_list_to_pos_empty_1/1).

lift_list_to_pos_empty_1(?nil) -> ?nil;
lift_list_to_pos_empty_1(?list(Content, Termination, _)) ->
  ?list(Content, Termination, ?unknown_qual).

-spec t_widen_to_number(erl_type()) -> erl_type().

%% Widens integers and floats to t_number().
%% Used by erl_bif_types:key_comparison_fail().

t_widen_to_number(?any) -> ?any;
t_widen_to_number(?none) -> ?none;
t_widen_to_number(?unit) -> ?unit;
t_widen_to_number(?atom(_Set) = T) -> T;
t_widen_to_number(?bitstr(_Unit, _Base) = T) -> T;
t_widen_to_number(?float) -> t_number();
t_widen_to_number(?function(Domain, Range)) ->
  ?function(t_widen_to_number(Domain), t_widen_to_number(Range));
t_widen_to_number(?identifier(_Types) = T) -> T;
t_widen_to_number(?int_range(_From, _To)) -> t_number();
t_widen_to_number(?int_set(_Set)) -> t_number();
t_widen_to_number(?integer(_Types)) -> t_number();
t_widen_to_number(?list(Type, Tail, Size)) ->
  ?list(t_widen_to_number(Type), t_widen_to_number(Tail), Size);
t_widen_to_number(?map(Pairs, DefK, DefV)) ->
  L = [{t_widen_to_number(K), MNess, t_widen_to_number(V)} ||
        {K, MNess, V} <- Pairs],
  t_map(L, t_widen_to_number(DefK), t_widen_to_number(DefV));
t_widen_to_number(?nil) -> ?nil;
t_widen_to_number(?number(_Set, _Tag)) -> t_number();
t_widen_to_number(?nominal(N, S)) -> ?nominal(N, t_widen_to_number(S));
t_widen_to_number(?nominal_set(N, S)) ->
  normalize_nominal_set([t_widen_to_number(Nom) || Nom <- N],
                        t_widen_to_number(S),
                        []);
t_widen_to_number(?product(Types)) ->
  ?product(list_widen_to_number(Types));
t_widen_to_number(?tuple(?any, _, _) = T) -> T;
t_widen_to_number(?tuple(Types, Arity, Tag)) ->
  ?tuple(list_widen_to_number(Types), Arity, Tag);
t_widen_to_number(?tuple_set(_) = Tuples) ->
  t_sup([t_widen_to_number(T) || T <- t_tuple_subtypes(Tuples)]);
t_widen_to_number(?union(List)) ->
  ?union(list_widen_to_number(List));
t_widen_to_number(?var(_Id)= T) -> T.

list_widen_to_number(List) ->
  [t_widen_to_number(E) || E <- List].

%%-----------------------------------------------------------------------------
%% Maps
%%
%% Representation:
%%  ?map(Pairs, DefaultKey, DefaultValue)
%%
%% Pairs is a sorted dictionary of types with a mandatoriness tag on each pair
%% (t_map_dict()). DefaultKey and DefaultValue are plain types.
%%
%% A map M belongs to this type iff
%%   For each pair {KT, mandatory, VT} in Pairs, there exists a pair {K, V} in M
%%     such that K \in KT and V \in VT.
%%   For each pair {KT, optional, VT} in Pairs, either there exists no key K in
%%     M s.t. K in KT, or there exists a pair {K, V} in M such that K \in KT and
%%     V \in VT.
%%   For each remaining pair {K, V} in M (where remaining means that there is no
%%     key KT in Pairs s.t. K \in KT), K \in DefaultKey and V \in DefaultValue.
%%
%% Invariants:
%%  * The keys in Pairs are singleton types.
%%  * The values of Pairs must not be unit, and may only be none if the
%%      mandatoriness tag  is 'optional'.
%%  * There is no pair {K, 'optional', V} in Pairs s.t.
%%      K is a subtype of DefaultKey and V is equal to DefaultValue.
%%  * DefaultKey must be the empty type iff DefaultValue is the empty type.
%%  * DefaultKey must not be a singleton type.
%%  * For every key K in Pairs, DefaultKey - K must not be representable; i.e.
%%    t_subtract(DefaultKey, K) must return DefaultKey.
%%  * For every pair {K, 'optional', ?none} in Pairs, K must be a subtype of
%%    DefaultKey.
%%  * Pairs must be sorted and not contain any duplicate keys.
%%
%% These invariants ensure that equal map types are represented by equal terms.

-define(mand, mandatory).
-define(opt, optional).

-type t_map_mandatoriness() :: ?mand | ?opt.
-type t_map_pair() :: {erl_type(), t_map_mandatoriness(), erl_type()}.
-type t_map_dict() :: [t_map_pair()].

-spec t_map() -> erl_type().

t_map() ->
  t_map([], t_any(), t_any()).

-spec t_map([{erl_type(), erl_type()}]) -> erl_type().

t_map(L) ->
  lists:foldl(fun t_map_put/2, t_map(), L).

-spec t_map(t_map_dict(), erl_type(), erl_type()) -> erl_type().

t_map(Pairs0, DefK0, DefV0) ->
  DefK1 = lists:foldl(fun({K,_,_},Acc)->t_subtract(Acc,K)end, DefK0, Pairs0),
  {DefK2, DefV1} =
    case t_is_impossible(DefK1) orelse t_is_impossible(DefV0) of
      true  -> {?none, ?none};
      false -> {DefK1, DefV0}
    end,
  {Pairs1, DefK3, DefV}
    = case is_singleton_type(DefK2) of
	true  -> {mapdict_insert({DefK2, ?opt, DefV1}, Pairs0), ?none, ?none};
	false -> {Pairs0,                                       DefK2, DefV1}
      end,
  {Pairs, DefK} = normalise_map_optionals(Pairs1, DefK3, DefV),
  %% Validate invariants of the map representation.
  %% Since we needed to iterate over the arguments in order to normalise anyway,
  %% we might as well save us some future pain and do this even without
  %% define(DEBUG, true).
  try
    validate_map_elements(Pairs)
  catch error:badarg -> error(badarg, [Pairs0,DefK0,DefV0])
  end,
  case map_pairs_are_none(Pairs) of
    true -> ?none;
    false -> ?map(Pairs, DefK, DefV)
  end.

normalise_map_optionals(Pairs, DefK, DefV) ->
  case normalise_map_optionals(Pairs, DefK, DefV, [], defk_unchanged) of
    {Pairs1, DefK1, defk_changed} ->
      normalise_map_optionals(Pairs1, DefK1, DefV);
    {Pairs1, DefK1, defk_unchanged} ->
      {Pairs1, DefK1}
  end.

normalise_map_optionals([], DefK, _, Es, F) -> {lists:reverse(Es), DefK, F};
normalise_map_optionals([E={K,?opt,?none}|T], DefK, DefV, Es, F) ->
  Diff = t_subtract(DefK, K),
  case t_is_subtype(K, DefK) andalso DefK =:= Diff of
    true -> normalise_map_optionals(T, DefK, DefV, [E|Es], F);
    false -> normalise_map_optionals(T, Diff, DefV, Es, F)
  end;
normalise_map_optionals([E={K,?opt,V}|T], DefK, DefV, Es, F) ->
  HowToHandleE =
    case t_is_equal(V, DefV) of
      true ->
        case t_is_subtype(K, DefK) of
          true -> skip;
          false ->
            case needs_to_be_merged(K, DefK) of
              true -> add_to_default_key;
              false -> keep
            end
        end;
      false -> keep
    end,
  case HowToHandleE of
    skip ->
      normalise_map_optionals(T, DefK, DefV, Es, F);
    keep ->
      normalise_map_optionals(T, DefK, DefV, [E|Es], F);
    add_to_default_key ->
      normalise_map_optionals(T, t_sup(K, DefK), DefV, Es, defk_changed)
  end;
normalise_map_optionals([E|T], DefK, DefV, Es, F) ->
  normalise_map_optionals(T, DefK, DefV, [E|Es], F).

%% Return `true' if the first argument (a singleton) cannot be
%% separated from the second argument (the default key) as that would
%% represent equal map types by unequal terms. An example:
%% `#{0 => t(), pos_integer() => t()}' is to be represented by
%% `#{non_neg_integer() => t()}'.
needs_to_be_merged(?int_set(Set), DefK) ->
  [I] = Set,
  Iplus = t_integer(I + 1),
  Iminus = t_integer(I - 1),
  InfPlus = t_inf(Iplus, DefK),
  InfMinus = t_inf(Iminus, DefK),
  not (t_is_none(InfPlus) andalso t_is_none(InfMinus));
needs_to_be_merged(?atom(_Set), DefK) ->
  InfAtom = t_inf(t_atom(), DefK),
  not t_is_none(InfAtom);
needs_to_be_merged(?nil, DefK) ->
  InfNonEmpty = t_inf(t_nonempty_list(), DefK),
  t_is_cons(InfNonEmpty);
needs_to_be_merged(_, _) ->
  false.

validate_map_elements([{K1,_,_}|Rest=[{K2,_,_}|_]]) ->
  case is_singleton_type(K1) andalso K1 < K2 of
    false -> error(badarg);
    true -> validate_map_elements(Rest)
  end;
validate_map_elements([{K,_,_}]) ->
  case is_singleton_type(K) of
    false -> error(badarg);
    true -> true
  end;
validate_map_elements([]) -> true.

map_pairs_are_none([]) -> false;
map_pairs_are_none([{_,?mand,?none}|_]) -> true;
map_pairs_are_none([_|Ps]) -> map_pairs_are_none(Ps).

-spec t_is_map(erl_type()) -> boolean().

t_is_map(Type) ->
  structural(Type, fun is_map1/1).

is_map1(?map(_, _, _)) -> true;
is_map1(_) -> false.

-spec t_map_entries(erl_type()) -> t_map_dict().

t_map_entries(M) ->
  structural(M, fun map_entries/1).

map_entries(?map(Pairs,_,_)) ->
  Pairs.

-spec t_map_def_key(erl_type()) -> erl_type().

t_map_def_key(M) ->
  structural(M, fun map_def_key/1).

map_def_key(?map(_,DefK,_)) ->
  DefK.

-spec t_map_def_val(erl_type()) -> erl_type().

t_map_def_val(M) ->
  structural(M, fun map_def_val/1).

map_def_val(?map(_,_,DefV)) ->
  DefV.

-spec mapdict_store(t_map_pair(), t_map_dict()) -> t_map_dict().

mapdict_store(E={K,_,_}, [{K,_,_}|T]) -> [E|T];
mapdict_store(E1={K1,_,_}, [E2={K2,_,_}|T]) when K1 > K2 ->
  [E2|mapdict_store(E1, T)];
mapdict_store(E={_,_,_}, T) -> [E|T].

-spec mapdict_insert(t_map_pair(), t_map_dict()) -> t_map_dict().

mapdict_insert(E={K,_,_}, D=[{K,_,_}|_]) -> error(badarg, [E, D]);
mapdict_insert(E1={K1,_,_}, [E2={K2,_,_}|T]) when K1 > K2 ->
  [E2|mapdict_insert(E1, T)];
mapdict_insert(E={_,_,_}, T) -> [E|T].

-type map_pairwise_merge_fun() :: fun((erl_type(),
				       t_map_mandatoriness(), erl_type(),
				       t_map_mandatoriness(), erl_type())
				      -> t_map_pair() | false).

-spec t_map_pairwise_merge(map_pairwise_merge_fun(), erl_type(), erl_type()) ->
  t_map_dict().
t_map_pairwise_merge(F, MapA, MapB) ->
  structural(MapA,
	    fun(UMapA) ->
		structural(MapB,
			  fun(UMapB) ->
			      map_pairwise_merge(F, UMapA, UMapB)
			  end)
	    end).

%% Merges the pairs of two maps together. Missing pairs become (?opt, DefV) or
%% (?opt, ?none), depending on whether K \in DefK.
-spec map_pairwise_merge(map_pairwise_merge_fun(), erl_type(), erl_type())
			-> t_map_dict().
map_pairwise_merge(F, ?map(APairs, ADefK, ADefV),
		       ?map(BPairs, BDefK, BDefV)) ->
  map_pairwise_merge(F, APairs, ADefK, ADefV, BPairs, BDefK, BDefV).

map_pairwise_merge(_, [], _, _, [], _, _) -> [];
map_pairwise_merge(F, As0, ADefK, ADefV, Bs0, BDefK, BDefV) ->
  {K1, AMNess1, AV1, As1, BMNess1, BV1, Bs1} =
    case {As0, Bs0} of
      {[{K,AMNess,AV}|As], [{K, BMNess,BV}|Bs]} ->
	{K, AMNess, AV, As, BMNess, BV, Bs};
      {[{K,AMNess,AV}|As], [{BK,_,     _ }|_]=Bs} when K < BK ->
        {K, AMNess, AV, As, ?opt, mapmerge_otherv(K, BDefK, BDefV), Bs};
      {As,                 [{K, BMNess,BV}|Bs]} ->
        {K, ?opt, mapmerge_otherv(K, ADefK, ADefV), As, BMNess, BV, Bs};
      {[{K,AMNess,AV}|As], []=Bs} ->
        {K, AMNess, AV, As, ?opt, mapmerge_otherv(K, BDefK, BDefV), Bs}
    end,
  MK = K1, %% Rename to make clear that we are matching below
  case F(K1, AMNess1, AV1, BMNess1, BV1) of
    false ->         map_pairwise_merge(F,As1,ADefK,ADefV,Bs1,BDefK,BDefV);
    {MK,_,_}=M -> [M|map_pairwise_merge(F,As1,ADefK,ADefV,Bs1,BDefK,BDefV)]
  end.

%% Folds over the pairs in two maps simultaneously in reverse key order. Missing
%% pairs become (?opt, DefV) or (?opt, ?none), depending on whether K \in DefK.
-spec map_pairwise_merge_foldr(fun((erl_type(),
				    t_map_mandatoriness(), erl_type(),
				    t_map_mandatoriness(), erl_type(),
				    Acc) -> Acc),
			       Acc, erl_type(), erl_type()) -> Acc.

map_pairwise_merge_foldr(F, AccIn, ?map(APairs, ADefK, ADefV),
			 ?map(BPairs, BDefK, BDefV)) ->
  map_pairwise_merge_foldr(F, AccIn, APairs, ADefK, ADefV, BPairs, BDefK, BDefV).

map_pairwise_merge_foldr(_, Acc,   [],  _,     _,     [],  _,     _) -> Acc;
map_pairwise_merge_foldr(F, AccIn, As0, ADefK, ADefV, Bs0, BDefK, BDefV) ->
  {K1, AMNess1, AV1, As1, BMNess1, BV1, Bs1} =
    case {As0, Bs0} of
      {[{K,AMNess,AV}|As], [{K,BMNess,BV}|Bs]} ->
	{K, AMNess, AV, As, BMNess, BV, Bs};
      {[{K,AMNess,AV}|As], [{BK,_,     _ }|_]=Bs} when K < BK ->
	{K, AMNess, AV, As, ?opt, mapmerge_otherv(K, BDefK, BDefV), Bs};
      {As,                 [{K,BMNess,BV}|Bs]} ->
        {K, ?opt, mapmerge_otherv(K, ADefK, ADefV), As, BMNess, BV, Bs};
      {[{K,AMNess,AV}|As], []=Bs} ->
        {K, AMNess, AV, As, ?opt, mapmerge_otherv(K, BDefK, BDefV), Bs}
    end,
  F(K1, AMNess1, AV1, BMNess1, BV1,
    map_pairwise_merge_foldr(F,AccIn,As1,ADefK,ADefV,Bs1,BDefK,BDefV)).

%% By observing that a missing pair in a map is equivalent to an optional pair,
%% with ?none or DefV value, depending on whether K \in DefK, we can simplify
%% merging by denormalising the map pairs temporarily, removing all 'false'
%% cases, at the cost of the creation of more tuples:
mapmerge_otherv(K, ODefK, ODefV) ->
  case t_inf(K, ODefK) of
    ?none      -> ?none;
    _KOrOpaque -> ODefV
  end.

-spec t_map_put({erl_type(), erl_type()}, erl_type()) -> erl_type().

t_map_put(KV, Map) ->
  structural(Map, fun(UM) -> map_put(KV, UM) end).

%% Key and Value are *not* unopaqued, but the map is
map_put(_, ?none) -> ?none;
map_put(_, ?unit) -> ?none;
map_put({Key, Value}, ?map(Pairs,DefK,DefV)) ->
  case t_is_impossible(Key) orelse t_is_impossible(Value) of
    true -> ?none;
    false ->
      case is_singleton_type(Key) of
	true ->
	  t_map(mapdict_store({Key, ?mand, Value}, Pairs), DefK, DefV);
	false ->
	  t_map([{K, MNess, case t_is_none(t_inf(K, Key)) of
			      true -> V;
			      false -> t_sup(V, Value)
			    end} || {K, MNess, V} <- Pairs],
		t_sup(DefK, Key),
		t_sup(DefV, Value))
      end
  end.

-spec t_map_remove(erl_type(), erl_type()) -> erl_type().

t_map_remove(Key, Map) ->
  structural(Map, fun(UM) -> map_remove(Key, UM) end).

map_remove(_, ?none) -> ?none;
map_remove(_, ?unit) -> ?none;
map_remove(Key, Map) ->
  %% ?map(lists:keydelete(Key, 1, Pairs), DefK, DefV).
  case is_singleton_type(Key) of
    false -> Map;
    true ->
      ?map(Pairs,DefK,DefV) = Map,
      case lists:keyfind(Key, 1, Pairs) of
        false -> Map;
        {Key, _, _} ->
          Pairs1 = lists:keydelete(Key, 1, Pairs),
          t_map(Pairs1, DefK, DefV)
      end
  end.

-spec t_map_update({erl_type(), erl_type()}, erl_type()) -> erl_type().

t_map_update(_, ?none) -> ?none;
t_map_update(_, ?unit) -> ?none;
t_map_update(KV={Key, _}, M) ->
  case t_is_subtype(t_atom('true'), t_map_is_key(Key, M)) of
    false -> ?none;
    true -> t_map_put(KV, M)
  end.

-spec t_map_get(erl_type(), erl_type()) -> erl_type().

t_map_get(Key, Map) ->
  structural(Map,
	    fun(UM) ->
		structural(Key, fun(UK) -> map_get(UK, UM) end)
	    end).

map_get(_, ?none) -> ?none;
map_get(_, ?unit) -> ?none;
map_get(Key, ?map(Pairs, DefK, DefV)) ->
  DefRes =
    case t_do_overlap(DefK, Key) of
      false -> t_none();
      true -> DefV
    end,
  case is_singleton_type(Key) of
    false ->
      lists:foldl(fun({K, _, V}, Res) ->
		      case t_do_overlap(K, Key) of
			false -> Res;
			true -> t_sup(Res, V)
		      end
		  end, DefRes, Pairs);
    true ->
      case lists:keyfind(Key, 1, Pairs) of
	false -> DefRes;
	{_, _, ValType} -> ValType
      end
  end.

-spec t_map_is_key(erl_type(), erl_type()) -> erl_type().

t_map_is_key(Key, Map) ->
  structural(Map,
	    fun(UM) ->
		structural(Key, fun(UK) -> map_is_key(UK, UM) end)
	    end).

map_is_key(_, ?none) -> ?none;
map_is_key(_, ?unit) -> ?none;
map_is_key(Key, ?map(Pairs, DefK, _DefV)) ->
  case is_singleton_type(Key) of
    true ->
      case lists:keyfind(Key, 1, Pairs) of
	{Key, ?mand, _}     -> t_atom(true);
	{Key, ?opt,  ?none} -> t_atom(false);
	{Key, ?opt,  _}     -> t_boolean();
	false ->
	  case t_do_overlap(DefK, Key) of
	    false -> t_atom(false);
	    true -> t_boolean()
	  end
      end;
    false ->
      case t_do_overlap(DefK, Key)
	orelse lists:any(fun({_,_,?none}) -> false;
			    ({K,_,_}) -> t_do_overlap(K, Key)
			 end, Pairs)
      of
	true -> t_boolean();
	false -> t_atom(false)
      end
  end.

%%-----------------------------------------------------------------------------
%% Tuples
%%

-spec t_tuple() -> erl_type().

t_tuple() ->
  ?tuple(?any, ?any, ?any).

-spec t_tuple(non_neg_integer() | [erl_type()]) -> erl_type().

t_tuple(N) when is_integer(N), N > ?MAX_TUPLE_SIZE  ->
  t_tuple();
t_tuple(N) when is_integer(N) ->
  ?tuple(lists:duplicate(N, ?any), N, ?any);
t_tuple(List) ->
  case any_none_or_unit(List) of
    true -> t_none();
    false ->
      Arity = length(List),
      case get_tuple_tags(List) of
	[Tag] -> ?tuple(List, Arity, Tag);  %% Tag can also be ?any here
	TagList ->
	  SortedTagList = lists:sort(TagList),
	  Tuples = [?tuple([T|tl(List)], Arity, T) || T <- SortedTagList],
	  ?tuple_set([{Arity, Tuples}])
      end
  end.

-spec get_tuple_tags([erl_type()]) -> [erl_type(),...].

get_tuple_tags([Tag|_]) ->
  structural(Tag, fun tuple_tags/1);
get_tuple_tags(_) -> [?any].

tuple_tags(?atom(?any)) -> [?any];
tuple_tags(?atom(Set)) ->
  case length(Set) > ?TUPLE_TAG_LIMIT of
    true -> [?any];
    false -> [t_atom(A) || A <- Set]
  end;
tuple_tags(_) -> [?any].

%% to be used for a tuple with known types for its arguments (not ?any)
-spec t_tuple_args(erl_type()) -> [erl_type()].

t_tuple_args(Type) ->
  structural(Type, fun tuple_args/1).

tuple_args(?tuple(Args, _, _)) when is_list(Args) -> Args.

%% to be used for a tuple with a known size (not ?any)
-spec t_tuple_size(erl_type()) -> non_neg_integer().

t_tuple_size(Type) ->
  structural(Type, fun tuple_size1/1).

tuple_size1(?tuple(_, Size, _)) when is_integer(Size) -> Size.

-spec t_tuple_sizes(erl_type()) -> 'unknown' | [non_neg_integer(),...].

t_tuple_sizes(Type) ->
  structural(Type, fun tuple_sizes/1).

tuple_sizes(?tuple(?any, ?any, ?any)) -> unknown;
tuple_sizes(?tuple(_, Size, _)) when is_integer(Size) -> [Size];
tuple_sizes(?tuple_set(List)) -> [Size || {Size, _} <- List].

-spec t_tuple_subtypes(erl_type()) -> 'unknown' | [erl_type(),...].
t_tuple_subtypes(Type) ->
  structural(Type, fun tuple_subtypes/1).

tuple_subtypes(?tuple(?any, ?any, ?any)) -> unknown;
tuple_subtypes(?tuple(_, _, _) = T) -> [T];
tuple_subtypes(?tuple_set(List)) ->
  lists:append([Tuples || {_Size, Tuples} <- List]).

-spec t_is_tuple(erl_type()) -> boolean().

t_is_tuple(Type) ->
  structural(Type, fun is_tuple1/1).

is_tuple1(?tuple(_, _, _)) -> true;
is_tuple1(?tuple_set(_)) -> true;
is_tuple1(_) -> false.

%%-----------------------------------------------------------------------------
%% Non-primitive types, including some handy syntactic sugar types
%%

-spec t_bitstrlist() -> erl_type().

t_bitstrlist() ->
  t_iolist(1, t_bitstr()).

-spec t_arity() -> erl_type().

t_arity() ->
  t_from_range(0, 255).	% was t_byte().

-spec t_pos_integer() -> erl_type().

t_pos_integer() ->
  t_from_range(1, pos_inf).

-spec t_non_neg_integer() -> erl_type().

t_non_neg_integer() ->
  t_from_range(0, pos_inf).

-spec t_is_non_neg_integer(erl_type()) -> boolean().

t_is_non_neg_integer(?integer(_) = T) ->
  t_is_subtype(T, t_non_neg_integer());
t_is_non_neg_integer(_) -> false.

-spec t_neg_integer() -> erl_type().

t_neg_integer() ->
  t_from_range(neg_inf, -1).

-spec t_fixnum() -> erl_type().

t_fixnum() ->
  t_integer(). % Gross over-approximation

-spec t_pos_fixnum() -> erl_type().

t_pos_fixnum() ->
  t_pos_integer().  % Gross over-approximation

-spec t_non_neg_fixnum() -> erl_type().

t_non_neg_fixnum() ->
  t_non_neg_integer().  % Gross over-approximation

-spec t_mfa() -> erl_type().

t_mfa() ->
  t_tuple([t_atom(), t_atom(), t_arity()]).

-spec t_module() -> erl_type().

t_module() ->
  t_atom().

-spec t_node() -> erl_type().

t_node() ->
  t_atom().

-spec t_iodata() -> erl_type().

t_iodata() ->
  t_sup(t_iolist(), t_binary()).

-spec t_iolist() -> erl_type().

t_iolist() ->
  t_iolist(1, t_binary()).

%% Added a second argument which currently is t_binary() | t_bitstr()
-spec t_iolist(non_neg_integer(), erl_type()) -> erl_type().

t_iolist(N, T) when N > 0 ->
  t_maybe_improper_list(t_sup([t_iolist(N-1, T), T, t_byte()]),
		        t_sup(T, t_nil()));
t_iolist(0, T) ->
  t_maybe_improper_list(t_any(), t_sup(T, t_nil())).

-spec t_timeout() -> erl_type().

t_timeout() ->
  t_sup(t_non_neg_integer(), t_atom('infinity')).

%%------------------------------------

%% ?none is allowed in products. A product of size 1 is not a product.

-spec t_product([erl_type()]) -> erl_type().

t_product([T]) -> T;
t_product(Types) when is_list(Types) ->
  ?product(Types).

%% This function is intended to be the inverse of the one above.
%% It should NOT be used with ?any, ?none or ?unit as input argument.

-spec t_to_tlist(erl_type()) -> [erl_type()].

t_to_tlist(?product(Types)) -> Types;
t_to_tlist(T) when T =/= ?any orelse T =/= ?none orelse T =/= ?unit -> [T].

%%------------------------------------

-spec t_var(atom() | integer()) -> erl_type().

t_var(Atom) when is_atom(Atom) -> ?var(Atom);
t_var(Int) when is_integer(Int) -> ?var(Int).

-spec t_is_var(erl_type()) -> boolean().

t_is_var(?var(_)) -> true;
t_is_var(_) -> false.

-spec t_var_name(erl_type()) -> atom() | integer().

t_var_name(?var(Id)) -> Id.

-spec t_has_var(erl_type()) -> boolean().

t_has_var(?var(_)) -> true;
t_has_var(?function(Domain, Range)) ->
  t_has_var(Domain) orelse t_has_var(Range);
t_has_var(?list(Contents, ?nil, _)) ->
  t_has_var(Contents);
t_has_var(?list(Contents, Termination, _)) ->
  t_has_var(Contents) orelse t_has_var(Termination);
t_has_var(?nominal(_, S)) ->
  t_has_var(S);
t_has_var(?nominal_set(N, S)) ->
  t_has_var(S) andalso lists:any(fun t_has_var/1, N);
t_has_var(?product(Types)) -> t_has_var_list(Types);
t_has_var(?tuple(?any, ?any, ?any)) -> false;
t_has_var(?tuple(Elements, _, _)) ->
  t_has_var_list(Elements);
t_has_var(?tuple_set(_) = T) ->
  t_has_var_list(t_tuple_subtypes(T));
t_has_var(?map(_, DefK, _)= Map) ->
  t_has_var_list(map_all_values(Map)) orelse
    t_has_var(DefK);
t_has_var(?union(List)) ->
  t_has_var_list(List);
t_has_var(_) -> false.

-spec t_has_var_list([erl_type()]) -> boolean().

t_has_var_list([T|Ts]) ->
  t_has_var(T) orelse t_has_var_list(Ts);
t_has_var_list([]) -> false.

-spec t_collect_var_names(erl_type()) -> any().

t_collect_var_names(T) ->
  t_collect_var_names(T, []).

-spec t_collect_var_names(erl_type(), ordsets:ordset(term())) ->
        ordsets:ordset(term()).

t_collect_var_names(?var(Id), Acc) ->
  ordsets:add_element(Id, Acc);
t_collect_var_names(?function(Domain, Range), Acc) ->
  Acc1 = t_collect_var_names(Domain, Acc),
  t_collect_var_names(Range, Acc1);
t_collect_var_names(?list(Contents, Termination, _), Acc) ->
  Acc1 = t_collect_var_names(Contents, Acc),
  t_collect_var_names(Termination, Acc1);
t_collect_var_names(?product(Types), Acc) ->
  t_collect_vars_list(Types, Acc);
t_collect_var_names(?tuple(?any, ?any, ?any), Acc) ->
  Acc;
t_collect_var_names(?tuple(Types, _, _), Acc) ->
  t_collect_vars_list(Types, Acc);
t_collect_var_names(?tuple_set(_) = TS, Acc) ->
  t_collect_vars_list(t_tuple_subtypes(TS), Acc);
t_collect_var_names(?map(_, DefK, _) = Map, Acc0) ->
  Acc = t_collect_vars_list(map_all_values(Map), Acc0),
  t_collect_var_names(DefK, Acc);
t_collect_var_names(?nominal(_, S), Acc) ->
  t_collect_var_names(S, Acc);
t_collect_var_names(?nominal_set(N, S), Acc) ->
  t_collect_vars_list(N, t_collect_var_names(S, Acc));
t_collect_var_names(?union(List), Acc) ->
  t_collect_vars_list(List, Acc);
t_collect_var_names(_, Acc) ->
  Acc.

t_collect_vars_list([T|Ts], Acc0) ->
  Acc = t_collect_var_names(T, Acc0),
  t_collect_vars_list(Ts, Acc);
t_collect_vars_list([], Acc) -> Acc.

%%=============================================================================
%%
%% Type construction from Erlang terms.
%%
%%=============================================================================

%%-----------------------------------------------------------------------------
%% Make a type from a term. No type depth is enforced.
%%

-spec t_from_term(term()) -> erl_type().

t_from_term([H|T]) ->                  t_cons(t_from_term(H), t_from_term(T));
t_from_term([]) ->                     t_nil();
t_from_term(T) when is_atom(T) ->      t_atom(T);
t_from_term(T) when is_bitstring(T) -> t_bitstr(0, erlang:bit_size(T));
t_from_term(T) when is_float(T) ->     t_float();
t_from_term(T) when is_function(T) ->
  {arity, Arity} = erlang:fun_info(T, arity),
  t_fun(Arity, t_any());
t_from_term(T) when is_integer(T) ->   t_integer(T);
t_from_term(T) when is_map(T) ->
  Pairs = [{t_from_term(K), ?mand, t_from_term(V)}
	   || K := V <- T],
  {Stons, Rest} = lists:partition(fun({K,_,_}) -> is_singleton_type(K) end,
				  Pairs),
  {DefK, DefV}
    = lists:foldl(fun({K,_,V},{AK,AV}) -> {t_sup(K,AK), t_sup(V,AV)} end,
		  {t_none(), t_none()}, Rest),
  t_map(lists:keysort(1, Stons), DefK, DefV);
t_from_term(T) when is_pid(T) ->       t_pid();
t_from_term(T) when is_port(T) ->      t_port();
t_from_term(T) when is_reference(T) -> t_reference();
t_from_term(T) when is_tuple(T) ->
  t_tuple([t_from_term(E) || E <- tuple_to_list(T)]).

%%-----------------------------------------------------------------------------
%% Integer types from a range.
%%-----------------------------------------------------------------------------

%%-define(USE_UNSAFE_RANGES, true).

-spec t_from_range(rng_elem(), rng_elem()) -> erl_type().

-ifdef(USE_UNSAFE_RANGES).

t_from_range(X, Y) ->
  t_from_range_unsafe(X, Y).

-else.

t_from_range(pos_inf, pos_inf) -> ?integer_pos;
t_from_range(neg_inf, neg_inf) -> ?integer_neg;
t_from_range(neg_inf, pos_inf) -> t_integer();
t_from_range(neg_inf, Y) when is_integer(Y), Y < 0  -> ?integer_neg;
t_from_range(neg_inf, Y) when is_integer(Y), Y >= 0 -> t_integer();
t_from_range(X, pos_inf) when is_integer(X), X >= 1 -> ?integer_pos;
t_from_range(X, pos_inf) when is_integer(X), X >= 0 -> ?integer_non_neg;
t_from_range(X, pos_inf) when is_integer(X), X < 0  -> t_integer();
t_from_range(X, Y) when is_integer(X), is_integer(Y), X > Y -> t_none();
t_from_range(X, Y) when is_integer(X), is_integer(Y) ->
  case ((Y - X) < ?SET_LIMIT) of
    true -> t_integers(lists:seq(X, Y));
    false ->
      case X >= 0 of
	false ->
	  if Y < 0 -> ?integer_neg;
	     true -> t_integer()
	  end;
	true ->
	  if Y =< ?MAX_BYTE, X >= 1 -> ?int_range(1, ?MAX_BYTE);
	     Y =< ?MAX_BYTE -> t_byte();
	     Y =< ?MAX_CHAR, X >= 1 -> ?int_range(1, ?MAX_CHAR);
	     Y =< ?MAX_CHAR -> t_char();
	     X >= 1         -> ?integer_pos;
	     X >= 0         -> ?integer_non_neg
	  end
      end
  end;
t_from_range(pos_inf, neg_inf) -> t_none().

-endif.

-spec number_min(erl_type()) -> rng_elem().

number_min(Type) ->
  structural(Type, fun number_min2/1).

number_min2(?int_range(From, _)) -> From;
number_min2(?int_set(Set)) -> set_min(Set);
number_min2(?number(?any, _Tag)) -> neg_inf.

-spec number_max(erl_type()) -> rng_elem().

number_max(Type) ->
  structural(Type, fun number_max2/1).

number_max2(?int_range(_, To)) -> To;
number_max2(?int_set(Set)) -> set_max(Set);
number_max2(?number(?any, _Tag)) -> pos_inf.

%% -spec int_range(rgn_elem(), rng_elem()) -> erl_type().
%%
%% int_range(neg_inf, pos_inf)         -> t_integer();
%% int_range(neg_inf, To)              -> ?int_range(neg_inf, To);
%% int_range(From, pos_inf)            -> ?int_range(From, pos_inf);
%% int_range(From, To) when From =< To -> t_from_range(From, To);
%% int_range(From, To) when To < From  -> ?none.

in_range(_, ?int_range(neg_inf, pos_inf)) -> true;
in_range(X, ?int_range(From, pos_inf))    -> X >= From;
in_range(X, ?int_range(neg_inf, To))      -> X =< To;
in_range(X, ?int_range(From, To))         -> (X >= From) andalso (X =< To).

-spec min(rng_elem(), rng_elem()) -> rng_elem().

min(neg_inf, _) -> neg_inf;
min(_, neg_inf) -> neg_inf;
min(pos_inf, Y) -> Y;
min(X, pos_inf) -> X;
min(X, Y) when X =< Y -> X;
min(_, Y) -> Y.

-spec max(rng_elem(), rng_elem()) -> rng_elem().

max(neg_inf, Y) -> Y;
max(X, neg_inf) -> X;
max(pos_inf, _) -> pos_inf;
max(_, pos_inf) -> pos_inf;
max(X, Y) when X =< Y -> Y;
max(X, _) -> X.

expand_range_from_set(Range = ?int_range(From, To), Set) ->
  Min = min(set_min(Set), From),
  Max = max(set_max(Set), To),
  if From =:= Min, To =:= Max -> Range;
     true -> t_from_range(Min, Max)
  end.

%%=============================================================================
%%
%% Lattice operations
%%
%%=============================================================================

%%-----------------------------------------------------------------------------
%% Supremum/join.
%%

-spec t_sup([erl_type()]) -> erl_type().

t_sup([]) -> ?none;
t_sup(Ts) ->
  case any_any(Ts) of
    true ->
      ?any;
    false ->
      [Type|NewTs] = Ts,
      t_sup1(NewTs, Type)
  end.

any_any([?any|_]) -> true;
any_any([_|T]) ->  any_any(T);
any_any([]) -> false.

t_sup1([H|T], Type) ->
  t_sup1(T, t_sup(H, Type));
t_sup1([], Type) ->
  Type.

-spec t_sup(erl_type(), erl_type()) -> erl_type().

t_sup(T1, T2) ->
  Res = t_sup_aux(T1, T2),
  %% `Res` must be at least as general as both `T1` and `T2`.
  ?debug(t_is_subtype(subst_all_vars_to_any(T1), Res) andalso
          t_is_subtype(subst_all_vars_to_any(T2), Res),
         {T1, T2, Res}),
  Res.

t_sup_aux(?any, _) -> ?any;
t_sup_aux(_, ?any) -> ?any;
t_sup_aux(?none, T) -> T;
t_sup_aux(T, ?none) -> T;
t_sup_aux(?unit, T) -> T;
t_sup_aux(T, ?unit) -> T;
t_sup_aux(T, T) -> T;
t_sup_aux(?opaque, T) -> T;
t_sup_aux(T, ?opaque) -> T;
t_sup_aux(?var(_), _) -> ?any;
t_sup_aux(_, ?var(_)) -> ?any;
t_sup_aux(?atom(Set1), ?atom(Set2)) ->
  ?atom(set_union(Set1, Set2));
t_sup_aux(?bitstr(U1, B1), ?bitstr(U2, B2)) ->
  t_bitstr(gcd(gcd(U1, U2), abs(B1-B2)), lists:min([B1, B2]));
t_sup_aux(?function(Domain1, Range1), ?function(Domain2, Range2)) ->
  %% The domain is either a product or any.
  ?function(t_sup_aux(Domain1, Domain2), t_sup_aux(Range1, Range2));
t_sup_aux(?identifier(Set1), ?identifier(Set2)) ->
  ?identifier(set_union(Set1, Set2));
t_sup_aux(?nil, ?list(Contents, Termination, _)) ->
  ?list(Contents, t_sup_aux(?nil, Termination), ?unknown_qual);
t_sup_aux(?list(Contents, Termination, _), ?nil) ->
  ?list(Contents, t_sup_aux(?nil, Termination), ?unknown_qual);
t_sup_aux(?list(Contents1, Termination1, Size1),
      ?list(Contents2, Termination2, Size2)) ->
  NewSize =
    case {Size1, Size2} of
      {?unknown_qual, ?unknown_qual} -> ?unknown_qual;
      {?unknown_qual, ?nonempty_qual} -> ?unknown_qual;
      {?nonempty_qual, ?unknown_qual} -> ?unknown_qual;
      {?nonempty_qual, ?nonempty_qual} -> ?nonempty_qual
    end,
  NewContents = t_sup_aux(Contents1, Contents2),
  NewTermination = t_sup_aux(Termination1, Termination2),
  ?list(NewContents, NewTermination, NewSize);
t_sup_aux(?number(_, _), ?number(?any, ?unknown_qual) = T) -> T;
t_sup_aux(?number(?any, ?unknown_qual) = T, ?number(_, _)) -> T;
t_sup_aux(?float, ?integer(_)) -> t_number();
t_sup_aux(?integer(_), ?float) -> t_number();
t_sup_aux(?integer(?any) = T, ?integer(_)) -> T;
t_sup_aux(?integer(_), ?integer(?any) = T) -> T;
t_sup_aux(?int_set(Set1), ?int_set(Set2)) ->
  case set_union(Set1, Set2) of
    ?any ->
      t_from_range(min(set_min(Set1), set_min(Set2)),
		   max(set_max(Set1), set_max(Set2)));
    Set -> ?int_set(Set)
  end;
t_sup_aux(?int_range(From1, To1), ?int_range(From2, To2)) ->
  t_from_range(min(From1, From2), max(To1, To2));
t_sup_aux(Range = ?int_range(_, _), ?int_set(Set)) ->
  expand_range_from_set(Range, Set);
t_sup_aux(?int_set(Set), Range = ?int_range(_, _)) ->
  expand_range_from_set(Range, Set);
t_sup_aux(?product(Types1), ?product(Types2)) ->
  L1 = length(Types1),
  L2 = length(Types2),
  if L1 =:= L2 -> ?product(t_sup_lists(Types1, Types2));
     true -> ?any
  end;
t_sup_aux(?product(_), _) ->
  ?any;
t_sup_aux(_, ?product(_)) ->
  ?any;
t_sup_aux(?tuple(?any, ?any, ?any) = T, ?tuple(_, _, _)) -> T;
t_sup_aux(?tuple(_, _, _), ?tuple(?any, ?any, ?any) = T) -> T;
t_sup_aux(?tuple(?any, ?any, ?any) = T, ?tuple_set(_)) -> T;
t_sup_aux(?tuple_set(_), ?tuple(?any, ?any, ?any) = T) -> T;
t_sup_aux(?tuple(Elements1, Arity, Tag1) = T1,
      ?tuple(Elements2, Arity, Tag2) = T2) ->
  if Tag1 == Tag2 -> t_tuple(t_sup_lists(Elements1, Elements2));
     Tag1 == ?any -> t_tuple(t_sup_lists(Elements1, Elements2));
     Tag2 == ?any -> t_tuple(t_sup_lists(Elements1, Elements2));
     Tag1 < Tag2 -> ?tuple_set([{Arity, [T1, T2]}]);
     Tag1 > Tag2 -> ?tuple_set([{Arity, [T2, T1]}])
  end;
t_sup_aux(?tuple(_, Arity1, _) = T1, ?tuple(_, Arity2, _) = T2) ->
  sup_tuple_sets([{Arity1, [T1]}], [{Arity2, [T2]}]);
t_sup_aux(?tuple_set(List1), ?tuple_set(List2)) ->
  sup_tuple_sets(List1, List2);
t_sup_aux(?tuple_set(List1), T2 = ?tuple(_, Arity, _)) ->
  sup_tuple_sets(List1, [{Arity, [T2]}]);
t_sup_aux(?tuple(_, Arity, _) = T1, ?tuple_set(List2)) ->
  sup_tuple_sets([{Arity, [T1]}], List2);
t_sup_aux(?map(_, ADefK, ADefV) = A, ?map(_, BDefK, BDefV) = B) ->
  Pairs =
    map_pairwise_merge(
      fun(K, MNess, V1, MNess, V2) -> {K, MNess, t_sup_aux(V1, V2)};
	 (K, _,     V1, _,     V2) -> {K, ?opt,  t_sup_aux(V1, V2)}
      end, A, B),
  t_map(Pairs, t_sup_aux(ADefK, BDefK), t_sup_aux(ADefV, BDefV));
%% Union of 1 or more nominal types/nominal sets
t_sup_aux(?nominal(Name, S1), ?nominal(Name, S2)) ->
  ?nominal(Name, t_sup_aux(S1, S2));
t_sup_aux(?nominal(LHS_Name, ?nominal(LHS_InnerName, _)=LHS_Inner)=LHS,
          ?nominal(RHS_Name, ?nominal(RHS_InnerName, _)=RHS_Inner)=RHS) ->
  case t_sup_aux(LHS_Inner, RHS_Inner) of
    ?nominal(LHS_InnerName = RHS_Name, _)=Sup ->
      ?nominal(RHS_Name, Sup);
    ?nominal(RHS_InnerName = LHS_Name, _)=Sup ->
      ?nominal(LHS_Name, Sup);
    ?nominal(_,_)=Sup ->
      Sup;
    ?nominal_set(_, ?none) when LHS_Name < RHS_Name ->
      ?nominal_set([LHS, RHS], ?none);
    ?nominal_set(_, ?none) ->
      ?nominal_set([RHS, LHS], ?none)
  end;
t_sup_aux(?nominal(LHS_Name, ?nominal(_, _)=LHS_Inner),
          ?nominal(_, ?nominal_set(_, _))=RHS) ->
  t_sup_aux(?nominal(LHS_Name, ?nominal_set([LHS_Inner], ?none)), RHS);
t_sup_aux(?nominal(_, ?nominal_set(_, _))=LHS,
          ?nominal(_, ?nominal(_, _))=RHS) ->
  t_sup_aux(RHS, LHS);
t_sup_aux(?nominal(LHS_Name, ?nominal(LHS_InnerName, _)=LHS_Inner)=LHS,
          ?nominal(RHS_Name, _)=RHS) ->
  case t_sup_aux(LHS_Inner, RHS) of
    ?nominal_set(_, ?none) when LHS_Name < RHS_Name ->
      ?nominal_set([LHS, RHS], ?none);
    ?nominal_set(_, ?none) ->
      ?nominal_set([RHS, LHS], ?none);
    ?nominal(RHS_Name, _)=Sup ->
      Sup;
    ?nominal(LHS_InnerName, _)=Sup ->
      ?nominal(LHS_Name, Sup)
  end;
t_sup_aux(?nominal(_, _)=LHS, ?nominal(_, ?nominal(_,_))=RHS) ->
  t_sup_aux(RHS, LHS);
t_sup_aux(?nominal(LHS_Name, ?nominal_set(L_Ns, L_S)),
          ?nominal(RHS_Name, ?nominal_set(R_Ns, R_S))) ->
  Sup0 = t_sup_aux(?nominal(LHS_Name, L_S),
                   ?nominal(RHS_Name, R_S)),
  LHS_Expanded = [?nominal(LHS_Name, N) || N <- L_Ns],
  RHS_Expanded = [?nominal(RHS_Name, N) || N <- R_Ns],
  Sup = lists:foldl(fun t_sup_aux/2, Sup0, LHS_Expanded),
  lists:foldl(fun t_sup_aux/2, Sup, RHS_Expanded);
t_sup_aux(?nominal(LHS_Name, ?nominal_set(L_Ns, L_S)),
          ?nominal(_, _)=RHS) ->
  LHS_Expanded = [?nominal(LHS_Name, N) || N <- L_Ns],
  Sup = nominal_set_absorb(LHS_Expanded, RHS, []),
  t_sup_aux(Sup, ?nominal(LHS_Name, L_S));
t_sup_aux(?nominal(_, _)=LHS, ?nominal(_, ?nominal_set(_,_))=RHS) ->
  t_sup_aux(RHS, LHS);
t_sup_aux(?nominal(LHS_Name, _)=LHS, ?nominal(RHS_Name, _)=RHS) ->
  case LHS_Name < RHS_Name of
    true -> ?nominal_set([LHS, RHS], ?none);
    false -> ?nominal_set([RHS, LHS], ?none)
  end;
t_sup_aux(?nominal_set(LHS_Ns, LHS_S), ?nominal_set(RHS_Ns, RHS_S)) ->
  Sup0 = t_sup_aux(LHS_S, RHS_S),
  ?debug(not t_is_nominal(Sup0), {LHS_S, RHS_S}),
  Sup = lists:foldl(fun t_sup_aux/2, Sup0, LHS_Ns),
  lists:foldl(fun t_sup_aux/2, Sup, RHS_Ns);
t_sup_aux(?nominal_set(LHS_Ns, ?none), ?nominal(_, _)=RHS) ->
  nominal_set_absorb(LHS_Ns, RHS, []);
t_sup_aux(?nominal_set(LHS_Ns, Other), ?nominal(_, _)=RHS) ->
  t_sup_aux(t_sup_aux(?nominal_set(LHS_Ns, ?none), RHS), Other);
t_sup_aux(?nominal(_, _)=LHS, ?nominal_set(_, _)=RHS) ->
  t_sup_aux(RHS, LHS);
t_sup_aux(?nominal(_,LHS_S)=LHS, RHS) ->
  ?debug(not t_is_nominal(RHS), RHS),
  Inf = t_inf_aux(LHS_S, RHS),
  case t_is_impossible(Inf) of
    true -> ?nominal_set([LHS], RHS);
    false -> t_sup_aux(LHS_S, RHS)
  end;
t_sup_aux(LHS, ?nominal(_, _)=RHS) ->
  ?debug(not t_is_nominal(LHS), LHS),
  t_sup_aux(RHS, LHS);
t_sup_aux(?nominal_set(LHS_Ns, LHS_S), RHS) ->
  ?debug(not t_is_nominal(RHS), RHS),
  normalize_nominal_set(LHS_Ns, t_sup_aux(LHS_S, RHS), []);
t_sup_aux(LHS, ?nominal_set(_, _)=RHS) ->
  ?debug(not t_is_nominal(LHS), LHS),
  t_sup_aux(RHS, LHS);
t_sup_aux(T1, T2) ->
  ?union(U1) = force_union(T1),
  ?union(U2) = force_union(T2),
  sup_union(U1, U2).

-spec t_sup_lists([erl_type()], [erl_type()]) -> [erl_type()].

t_sup_lists(Ts1, Ts2) ->
  [t_sup(T1, T2) || T1 <- Ts1 && T2 <- Ts2].

%% Adds the new nominal `Sup` into the set of nominals `Ns0`. Note that it does
%% not handle structurals; the caller is expected to normalize the result
%% afterwards.
nominal_set_absorb([?nominal(_, _)=N | Ns0], Sup, Acc) ->
  ?debug(t_is_nominal(Sup), Sup),
  case t_inf_aux(N, Sup) of
    ?nominal(_, _) ->
      %% The types overlap, abort and start over with the widened type.
      t_sup_aux(?nominal_set(lists:reverse(Acc, Ns0), ?none),
                t_sup_aux(N, Sup));
    ?none ->
      nominal_set_absorb(Ns0, Sup, [N | Acc])
  end;
nominal_set_absorb([], Sup, Acc) ->
  ?debug(t_is_nominal(Sup), Sup),
  Ns = nominal_set_absorb_merge(Acc, Sup, []),
  ?debug(begin
            Names = [Name || ?nominal(Name, _) <- Ns],
            Names =:= lists:usort(Names)
         end, {Sup, Acc, Ns}),
  ?nominal_set(Ns, ?none).

nominal_set_absorb_merge([?nominal(Same, LHS_S) | Rest],
                         ?nominal(Same, RHS_S), Acc) ->
  lists:reverse([?nominal(Same, t_sup_aux(LHS_S, RHS_S)) | Rest], Acc);
nominal_set_absorb_merge([?nominal(LHS_Name, _)=LHS | Rest],
                         ?nominal(RHS_Name, _)=RHS, Acc)
    when LHS_Name > RHS_Name ->
  %% Note that the list is reversed, so '>' puts this in ascending order.
  nominal_set_absorb_merge(Rest, RHS, [LHS | Acc]);
nominal_set_absorb_merge(Rest, RHS, Acc) ->
  lists:reverse([RHS | Rest], Acc).

normalize_nominal_set(_, ?any, _) ->
  ?any;
normalize_nominal_set([], Other, []) ->
  ?debug(not t_is_nominal(Other), Other),
  Other;
normalize_nominal_set([], ?none, [?nominal(_, _) = N]) ->
  N;
normalize_nominal_set([], Other, Nominals0) ->
  %% Names must be unique and in the correct order.
  Nominals = lists:reverse(Nominals0),
  ?debug(begin
            Names = [Name || ?nominal(Name, _) <- Nominals],
            Names =:= lists:usort(Names)
         end, Nominals),
  ?nominal_set(Nominals, Other);
normalize_nominal_set([?nominal(_, _)=Type | Types], ?none, Nominals) ->
  normalize_nominal_set(Types, ?none, [Type | Nominals]);
normalize_nominal_set([?none | Types], Other, Nominals) ->
  normalize_nominal_set(Types, Other, Nominals);
normalize_nominal_set([Type | Types], Other, Nominals) ->
  case t_inf_aux(Type, Other) of
    ?none ->
      %% The `Other` type does not overlap with the nominal type, include it
      %% in the new nominal list.
      ?nominal(_, _) = Type,                    %Assertion.
      normalize_nominal_set(Types, Other, [Type | Nominals]);
    _ ->
      %% `Type` is structural (can happen during limiting) or overlaps with
      %% `Other0`, start over since the new `Other` type could overlap with
      %% previously-handled nominals.
      t_sup_aux(?nominal_set(lists:reverse(Nominals, Types), ?none),
                t_sup_aux(Type, Other))
  end.

sup_tuple_sets(L1, L2) ->
  TotalArities = ordsets:union([Arity || {Arity, _} <- L1],
			       [Arity || {Arity, _} <- L2]),
  if length(TotalArities) > ?TUPLE_ARITY_LIMIT -> t_tuple();
     true ->
      case sup_tuple_sets(L1, L2, []) of
	[{_Arity, [OneTuple = ?tuple(_, _, _)]}] -> OneTuple;
	List -> ?tuple_set(List)
      end
  end.

sup_tuple_sets([{Arity, Tuples1}|Left1], [{Arity, Tuples2}|Left2], Acc) ->
  NewAcc = [{Arity, sup_tuples_in_set(Tuples1, Tuples2)}|Acc],
  sup_tuple_sets(Left1, Left2, NewAcc);
sup_tuple_sets([{Arity1, _} = T1|Left1] = L1,
	       [{Arity2, _} = T2|Left2] = L2, Acc) ->
  if Arity1 < Arity2 -> sup_tuple_sets(Left1, L2, [T1|Acc]);
     Arity1 > Arity2 -> sup_tuple_sets(L1, Left2, [T2|Acc])
  end;
sup_tuple_sets([], L2, Acc) -> lists:reverse(Acc, L2);
sup_tuple_sets(L1, [], Acc) -> lists:reverse(Acc, L1).

sup_tuples_in_set([?tuple(_, _, ?any) = T], L) ->
  [t_tuple(sup_tuple_elements([T|L]))];
sup_tuples_in_set(L, [?tuple(_, _, ?any) = T]) ->
  [t_tuple(sup_tuple_elements([T|L]))];
sup_tuples_in_set(L1, L2) ->
  FoldFun = fun(?tuple(_, _, Tag), AccTag) -> t_sup(Tag, AccTag) end,
  TotalTag0 = lists:foldl(FoldFun, ?none, L1),
  TotalTag  = lists:foldl(FoldFun, TotalTag0, L2),
  case TotalTag of
    ?atom(?any) ->
      %% We will reach the set limit. Widen now.
      [t_tuple(sup_tuple_elements(L1 ++ L2))];
    ?atom(Set) ->
      case length(Set) > ?TUPLE_TAG_LIMIT of
	true ->
	  %% We will reach the set limit. Widen now.
	  [t_tuple(sup_tuple_elements(L1 ++ L2))];
	false ->
	  %% We can go on and build the tuple set.
	  sup_tuples_in_set(L1, L2, [])
      end
  end.

sup_tuple_elements([?tuple(Elements, _, _)|L]) ->
  lists:foldl(fun (?tuple(Es, _, _), Acc) -> t_sup_lists(Es, Acc) end,
	      Elements, L).

sup_tuples_in_set([?tuple(Elements1, Arity, Tag1) = T1|Left1] = L1,
		  [?tuple(Elements2, Arity, Tag2) = T2|Left2] = L2, Acc) ->
  if
    Tag1 < Tag2   -> sup_tuples_in_set(Left1, L2, [T1|Acc]);
    Tag1 > Tag2   -> sup_tuples_in_set(L1, Left2, [T2|Acc]);
    Tag1 == Tag2 ->
      NewElements = t_sup_lists(Elements1, Elements2),
      NewAcc = [?tuple(NewElements, Arity, Tag1)|Acc],
      sup_tuples_in_set(Left1, Left2, NewAcc)
  end;
sup_tuples_in_set([], L2, Acc) -> lists:reverse(Acc, L2);
sup_tuples_in_set(L1, [], Acc) -> lists:reverse(Acc, L1).

sup_union(U1, U2) ->
  true = length(U1) =:= length(U2), %Assertion.
  true = ?num_types_in_union =:= length(U1), %Assertion
  sup_union(U1, U2, 0, []).

sup_union([?none|Left1], [?none|Left2], N, Acc) ->
  sup_union(Left1, Left2, N, [?none|Acc]);
sup_union([T1|Left1], [T2|Left2], N, Acc) ->
  sup_union(Left1, Left2, N+1, [t_sup(T1, T2)|Acc]);
sup_union([], [], N, Acc) ->
  if
    N =:= 0 ->
      ?none;
    N =:= 1 ->
      [Type] = [T || T <- Acc, T =/= ?none],
      Type;
    N =:= ?num_types_in_union ->
      case Acc =:= [t_tuple(), t_map(), ?any, t_number(), t_list(), t_identifier(), t_fun(), t_bitstr(), t_atom()] of
        true -> ?any;
        false -> ?union(lists:reverse(Acc))
      end;
    true ->
      ?union(lists:reverse(Acc))
  end.

force_union(T = ?atom(_)) ->          ?atom_union(T);
force_union(T = ?bitstr(_, _)) ->     ?bitstr_union(T);
force_union(T = ?function(_, _)) ->   ?function_union(T);
force_union(T = ?identifier(_)) ->    ?identifier_union(T);
force_union(T = ?list(_, _, _)) ->    ?list_union(T);
force_union(T = ?nil) ->              ?list_union(T);
force_union(T = ?number(_, _)) ->     ?number_union(T);
force_union(T = ?map(_,_,_)) ->       ?map_union(T);
force_union(T = ?tuple(_, _, _)) ->   ?tuple_union(T);
force_union(T = ?tuple_set(_)) ->     ?tuple_union(T);
force_union(T = ?union(_)) ->         T.

%%-----------------------------------------------------------------------------
%% An attempt to write the inverse operation of t_sup/1 -- XXX: INCOMPLETE !!
%%
-spec t_elements(erl_type()) -> [erl_type()].
t_elements(?none) -> [];
t_elements(?unit) -> [];
t_elements(?any = T) -> [T];
t_elements(?nil = T) -> [T];
t_elements(?atom(?any) = T) -> [T];
t_elements(?atom(Atoms)) ->
  [t_atom(A) || A <- Atoms];
t_elements(?bitstr(_, _) = T) -> [T];
t_elements(?function(_, _) = T) -> [T];
t_elements(?identifier(?any) = T) -> [T];
t_elements(?identifier(IDs)) ->
  [?identifier([T]) || T <- IDs];
t_elements(?nominal(_, _) = T) -> [T];
t_elements(?nominal_set(Ns, S)) ->
  t_elements(S) ++ Ns;
t_elements(?list(_, _, _) = T) -> [T];
t_elements(?number(_, _) = T) ->
  case T of
    ?number(?any, ?unknown_qual) ->
      [?float, ?integer(?any)];
    ?float -> [T];
    ?integer(?any) -> [T];
    ?int_range(_, _) -> [T];
    ?int_set(Set) ->
      [t_integer(I) || I <- Set]
  end;
t_elements(?map(_,_,_) = T) -> [T];
t_elements(?product(_) = T) -> [T];
t_elements(?tuple(_, _, _) = T) -> [T];
t_elements(?tuple_set(_) = TS) ->
  case t_tuple_subtypes(TS) of
    unknown -> [];
    Elems -> Elems
  end;
t_elements(?union(_) = T) ->
  do_elements(T);
t_elements(?var(_)) -> [?any].  %% yes, vars exist -- what else to do here?
%% t_elements(T) ->
%%   io:format("T_ELEMENTS => ~p\n", [T]).

do_elements(Type0) ->
  case structural(Type0, fun(T) -> T end) of
    ?union(List) ->
        lists:append([t_elements(T) || T <- List]);
    Type ->
        t_elements(Type)
  end.

%%-----------------------------------------------------------------------------
%% Infimum/meet.
%%

-spec t_inf([erl_type()]) -> erl_type().

t_inf([H1, H2|T]) ->
  case t_inf(H1, H2) of
    ?none -> ?none;
    NewH -> t_inf([NewH|T])
  end;
t_inf([H]) -> H;
t_inf([]) -> ?none.

-spec t_inf(erl_type(), erl_type()) -> erl_type().

t_inf(T1, T2) ->
  Res = t_inf_aux(T1, T2),
  %% `Res` must be at least as specific as `T1` and `T2`
  ?debug(t_is_subtype(subst_all_vars_to_any(Res),
                      subst_all_vars_to_any(T1)) andalso
          t_is_subtype(subst_all_vars_to_any(Res),
                       subst_all_vars_to_any(T2)),
         {T1, T2, Res}),
  Res.

t_inf_aux(?var(_), ?var(_)) -> ?any;
t_inf_aux(?var(_), T) -> T;
t_inf_aux(T, ?var(_)) -> T;
t_inf_aux(?any, T) -> T;
t_inf_aux(T, ?any) -> T;
t_inf_aux(?none, _) -> ?none;
t_inf_aux(_, ?none) -> ?none;
t_inf_aux(?unit, _) -> ?unit;	% ?unit cases should appear below ?none
t_inf_aux(_, ?unit) -> ?unit;
t_inf_aux(T, T) -> T;
t_inf_aux(?atom(Set1), ?atom(Set2)) ->
  case set_intersection(Set1, Set2) of
    ?none ->  ?none;
    NewSet -> ?atom(NewSet)
  end;
t_inf_aux(?bitstr(U1, B1), ?bitstr(0, B2)) ->
  if B2 >= B1 andalso (B2-B1) rem U1 =:= 0 -> t_bitstr(0, B2);
     true -> ?none
  end;
t_inf_aux(?bitstr(0, B1), ?bitstr(U2, B2)) ->
  if B1 >= B2 andalso (B1-B2) rem U2 =:= 0 -> t_bitstr(0, B1);
     true -> ?none
  end;
t_inf_aux(?bitstr(U1, B1), ?bitstr(U1, B1)) ->
  t_bitstr(U1, B1);
t_inf_aux(?bitstr(U1, B1), ?bitstr(U2, B2)) when U2 > U1 ->
  inf_bitstr(U2, B2, U1, B1);
t_inf_aux(?bitstr(U1, B1), ?bitstr(U2, B2)) ->
  inf_bitstr(U1, B1, U2, B2);
t_inf_aux(?function(Domain1, Range1), ?function(Domain2, Range2)) ->
  case t_inf_aux(Domain1, Domain2) of
    ?none -> ?none;
    Domain -> ?function(Domain, t_inf_aux(Range1, Range2))
  end;
t_inf_aux(?identifier(Set1), ?identifier(Set2)) ->
  case set_intersection(Set1, Set2) of
    ?none -> ?none;
    Set -> ?identifier(Set)
  end;
t_inf_aux(?map(_, ADefK, ADefV) = A, ?map(_, BDefK, BDefV) = B) ->
  %% Because it simplifies the anonymous function, we allow Pairs to temporarily
  %% contain mandatory pairs with none values, since all such cases should
  %% result in a none result.
  Pairs =
    map_pairwise_merge(
      %% For optional keys in both maps, when the infimum is none, we have
      %% essentially concluded that K must not be a key in the map.
      fun(K, ?opt, V1, ?opt, V2) -> {K, ?opt, t_inf_aux(V1, V2)};
	 %% When a key is optional in one map, but mandatory in another, it
	 %% becomes mandatory in the infinumum
	 (K, _, V1, _, V2) -> {K, ?mand, t_inf_aux(V1, V2)}
      end, A, B),
  t_map(Pairs,
        t_inf_aux(ADefK, BDefK),
        t_inf_aux(ADefV, BDefV));
%% Intersection of 1 or more nominal types
t_inf_aux(?nominal(Same, LHS_S), ?nominal(Same, RHS_S)) ->
  t_nominal(Same, t_inf_aux(LHS_S, RHS_S));
t_inf_aux(?nominal(LHS_Name, ?nominal(LHS_InnerName, _)=LHS_Inner),
          ?nominal(RHS_Name, ?nominal(RHS_InnerName, _)=RHS_Inner)) ->
  %% As the names of these nominals differ, they can only intersect if LHS is
  %% a nominal subtype of RHS_Inner or if RHS is a nominal subtype of LHS
  %% inner, for example:
  %%
  %% t_nominal(alpha, t_nominal(beta, t_nominal(gamma, any()))) = LHS
  %%                  t_nominal(beta, t_nominal(gamma, t_atom())) = RHS
  %%   =>
  %% t_nominal(alpha, t_nominal(beta, t_nominal(gamma, t_atom()))) = Res
  %%
  %% Note that nested nominals only intersect with nominals that share the
  %% same nesting: in a sense, you can say that the effective name of a nominal
  %% is the sum of its nesting. Thus, the following do not intersect despite
  %% being `alpha`s that are subtype of `gamma`s:
  %%
  %% t_nominal(alpha, t_nominal(beta, t_nominal(gamma, any()))) = LHS
  %% t_nominal(alpha, t_nominal(gamma, t_atom())) = RHS
  %%
  %% These rules are described in "Nominal Types for Erlang" by Huang et al,
  %% https://doi.org/10.1145/3677995.3678191
  case t_inf_aux(LHS_Inner, RHS_Inner) of
    ?nominal(LHS_InnerName = RHS_Name, _)=Inf -> ?nominal(LHS_Name, Inf);
    ?nominal(RHS_InnerName = LHS_Name, _)=Inf -> ?nominal(RHS_Name, Inf);
    _ -> ?none
  end;
t_inf_aux(?nominal(LHS_Name, ?nominal_set(L_Ns, L_S)),
          ?nominal(RHS_Name, ?nominal_set(R_Ns, R_S))) ->
  %% As inf_nominal_sets/2 can handle non-normalized sets, we can simplify
  %% crossing the lists by wrapping each nominal in the respective sets with
  %% their outer name and letting the regular nested nominal clause handle it.
  [_|_] = L_Ns,                                 %Assertion.
  LHS_Expanded =
    [?nominal(LHS_Name, L_S) | [?nominal(LHS_Name, N) || N <- L_Ns]],
  [_|_] = R_Ns,                                 %Assertion.
  RHS_Expanded =
    [?nominal(RHS_Name, R_S) | [?nominal(RHS_Name, N) || N <- R_Ns]],
  case inf_nominal_sets(LHS_Expanded, RHS_Expanded) of
    ?nominal(LHS_Name, _)=Inf -> Inf;
    ?nominal(RHS_Name, _)=Inf -> Inf;
    ?none -> ?none
  end;
t_inf_aux(?nominal(LHS_Name, ?nominal(_, _)=LHS_Inner),
          ?nominal(_, ?nominal_set(_, _))=RHS) ->
  t_inf_aux(?nominal(LHS_Name, ?nominal_set([LHS_Inner], ?none)), RHS);
t_inf_aux(?nominal(_, ?nominal_set(_, _))=LHS,
          ?nominal(RHS_Name, ?nominal(_, _)=RHS_Inner)) ->
  t_inf_aux(LHS, ?nominal(RHS_Name, ?nominal_set([RHS_Inner], ?none)));
t_inf_aux(?nominal(LHS_Name, ?nominal_set(_, _))=LHS,
          ?nominal(_, _)=RHS) ->
  t_inf_aux(LHS, ?nominal(LHS_Name, RHS));
t_inf_aux(?nominal(_, _)=LHS,
          ?nominal(_, ?nominal_set(_, _))=RHS) ->
  t_inf_aux(RHS, LHS);
t_inf_aux(?nominal(LHS_Name, ?nominal(_, _))=LHS,
          ?nominal(_, _)=RHS) ->
  t_inf_aux(LHS, ?nominal(LHS_Name, RHS));
t_inf_aux(?nominal(_, _)=LHS,
          ?nominal(_, ?nominal(_, _))=RHS) ->
  t_inf_aux(RHS, LHS);
t_inf_aux(?nominal_set(LHS_Ns, LHS_S),
          ?nominal_set(RHS_Ns, RHS_S)) ->
  inf_nominal_sets([LHS_S | LHS_Ns], [RHS_S | RHS_Ns]);
t_inf_aux(?nominal_set(LHS_Ns, LHS_S), ?nominal(_, _)=RHS) ->
  inf_nominal_sets([LHS_S | LHS_Ns], [RHS]);
t_inf_aux(?nominal(_, _)=LHS, ?nominal_set(RHS_Ns, RHS_S)) ->
  inf_nominal_sets([LHS], [RHS_S | RHS_Ns]);
t_inf_aux(?nominal_set(LHS_Ns, LHS_S), RHS) ->
  inf_nominal_sets([LHS_S | LHS_Ns], [RHS]);
t_inf_aux(LHS, ?nominal_set(_, _)=RHS) ->
  t_inf_aux(RHS, LHS);
t_inf_aux(?nominal(_, _), ?nominal(_, _)) ->
  ?none;
t_inf_aux(?nominal(LHS_Name, LHS_S), RHS_S) ->
  t_nominal(LHS_Name, t_inf_aux(LHS_S, RHS_S));
t_inf_aux(LHS, ?nominal(_, _)=RHS) ->
  t_inf_aux(RHS, LHS);
t_inf_aux(?nil, ?nil) -> ?nil;
t_inf_aux(?nil, ?nonempty_list(_, _)) ->
  ?none;
t_inf_aux(?nonempty_list(_, _), ?nil) ->
  ?none;
t_inf_aux(?nil, ?list(_Contents, Termination, _)) ->
  t_inf_aux(?nil, t_structural(Termination));
t_inf_aux(?list(_Contents, Termination, _), ?nil) ->
  t_inf_aux(?nil, t_structural(Termination));
t_inf_aux(?list(Contents1, Termination1, Size1),
      ?list(Contents2, Termination2, Size2)) ->
  case t_inf_aux(Termination1, Termination2) of
    ?none -> ?none;
    Termination ->
      case t_inf_aux(Contents1, Contents2) of
	?none ->
	  %% If none of the lists are nonempty, then the infimum is nil.
	  case (Size1 =:= ?unknown_qual) andalso (Size2 =:= ?unknown_qual) of
	    true -> t_nil();
	    false -> ?none
	  end;
	Contents ->
	  Size =
	    case {Size1, Size2} of
	      {?unknown_qual, ?unknown_qual} -> ?unknown_qual;
	      {?unknown_qual, ?nonempty_qual} -> ?nonempty_qual;
	      {?nonempty_qual, ?unknown_qual} -> ?nonempty_qual;
	      {?nonempty_qual, ?nonempty_qual} -> ?nonempty_qual
	    end,
	  ?list(Contents, Termination, Size)
      end
  end;
t_inf_aux(?number(_, _) = T1, ?number(_, _) = T2) ->
  case {T1, T2} of
    {T, T}                            -> T;
    {_, ?number(?any, ?unknown_qual)} -> T1;
    {?number(?any, ?unknown_qual), _} -> T2;
    {?float, ?integer(_)}             -> ?none;
    {?integer(_), ?float}             -> ?none;
    {?integer(?any), ?integer(_)}     -> T2;
    {?integer(_), ?integer(?any)}     -> T1;
    {?int_set(Set1), ?int_set(Set2)}  ->
      case set_intersection(Set1, Set2) of
	?none -> ?none;
	Set -> ?int_set(Set)
      end;
    {?int_range(From1, To1), ?int_range(From2, To2)} ->
      t_from_range(max(From1, From2), min(To1, To2));
    {Range = ?int_range(_, _), ?int_set(Set)} ->
      Ans2 =
	case set_filter(fun(X) -> in_range(X, Range) end, Set) of
	  ?none -> ?none;
	  NewSet -> ?int_set(NewSet)
	end,
      Ans2;
    {?int_set(Set), ?int_range(_, _) = Range} ->
      case set_filter(fun(X) -> in_range(X, Range) end, Set) of
	?none -> ?none;
	NewSet -> ?int_set(NewSet)
      end
  end;
t_inf_aux(?product(Types1), ?product(Types2)) ->
  case {length(Types1), length(Types2)} of
    {Same, Same} -> ?product(t_inf_lists(Types1, Types2));
    _ -> ?none
  end;
t_inf_aux(?product(_), _) ->
  ?none;
t_inf_aux(_, ?product(_)) ->
  ?none;
t_inf_aux(?tuple(?any, ?any, ?any), ?tuple(_, _, _) = T) ->
  T;
t_inf_aux(?tuple(_, _, _) = T, ?tuple(?any, ?any, ?any)) ->
  T;
t_inf_aux(?tuple(?any, ?any, ?any), ?tuple_set(_) = T) ->
  T;
t_inf_aux(?tuple_set(_) = T, ?tuple(?any, ?any, ?any)) ->
  T;
t_inf_aux(?tuple(Elements1, Arity, _Tag1), ?tuple(Elements2, Arity, _Tag2)) ->
  case t_inf_lists_strict(Elements1, Elements2) of
    bottom -> ?none;
    NewElements -> t_tuple(NewElements)
  end;
t_inf_aux(?tuple_set(List1), ?tuple_set(List2)) ->
  inf_tuple_sets(List1, List2);
t_inf_aux(?tuple_set(List), ?tuple(_, Arity, _) = T) ->
  inf_tuple_sets(List, [{Arity, [T]}]);
t_inf_aux(?tuple(_, Arity, _) = T, ?tuple_set(List)) ->
  inf_tuple_sets(List, [{Arity, [T]}]);
t_inf_aux(?opaque, _) ->
  ?none;
t_inf_aux(_, ?opaque) ->
  ?none;
t_inf_aux(?union(U1), T) ->
  ?union(U2) = force_union(T),
  inf_union(U1, U2);
t_inf_aux(T, ?union(U2)) ->
  ?union(U1) = force_union(T),
  inf_union(U1, U2);
t_inf_aux(#c{}, #c{}) ->
  ?none.

-spec t_inf_lists([erl_type()], [erl_type()]) -> [erl_type()].

t_inf_lists(Ts1, Ts2) ->
  [t_inf(T1, T2) || T1 <- Ts1 && T2 <- Ts2].

%% Infimum of lists with strictness.
%% If any element is the ?none type, the value 'bottom' is returned.

-spec t_inf_lists_strict([erl_type()], [erl_type()]) -> 'bottom' | [erl_type()].

t_inf_lists_strict(L1, L2) ->
  t_inf_lists_strict(L1, L2, []).

t_inf_lists_strict([T1|Left1], [T2|Left2], Acc) ->
  case t_inf(T1, T2) of
    ?none -> bottom;
    T -> t_inf_lists_strict(Left1, Left2, [T|Acc])
  end;
t_inf_lists_strict([], [], Acc) ->
  lists:reverse(Acc).

inf_nominal_sets([_|_]=LHS, [_|_]=RHS) ->
  %% Because a nominal in LHS_Ns can be a subtype of another in RHS_Ns or of
  %% the structure in RHS_S (and vice versa), we have to t_inf/2 the cartesian
  %% product of both sets.
  %%
  %% This is quadratic but generally fast enough given the small sizes of the
  %% sets.
  ins_cartesian(LHS, RHS).

ins_cartesian([A | As], Bs) ->
  case ins_cartesian_1(A, Bs) of
    ?none -> ins_cartesian(As, Bs);
    T -> t_sup_aux(T, ins_cartesian(As, Bs))
  end;
ins_cartesian([], _Bs) ->
  ?none.

ins_cartesian_1(A, [B | Bs]) ->
  case t_inf_aux(A, B) of
    ?none -> ins_cartesian_1(A, Bs);
    T -> t_sup_aux(T, ins_cartesian_1(A, Bs))
  end;
ins_cartesian_1(_A, []) ->
  ?none.

inf_tuple_sets(L1, L2) ->
  case inf_tuple_sets(L1, L2, []) of
    [] -> ?none;
    [{_Arity, [?tuple(_, _, _) = OneTuple]}] -> OneTuple;
    List -> ?tuple_set(List)
  end.

inf_tuple_sets([{Arity, Tuples1}|Ts1], [{Arity, Tuples2}|Ts2], Acc) ->
  case inf_tuples_in_sets(Tuples1, Tuples2) of
    [] -> inf_tuple_sets(Ts1, Ts2, Acc);
    [?tuple_set([{Arity, NewTuples}])] ->
      inf_tuple_sets(Ts1, Ts2, [{Arity, NewTuples}|Acc]);
    NewTuples -> inf_tuple_sets(Ts1, Ts2, [{Arity, NewTuples}|Acc])
  end;
inf_tuple_sets([{Arity1, _}|Ts1] = L1, [{Arity2, _}|Ts2] = L2, Acc) ->
  if Arity1 < Arity2 -> inf_tuple_sets(Ts1, L2, Acc);
     Arity1 > Arity2 -> inf_tuple_sets(L1, Ts2, Acc)
  end;
inf_tuple_sets([], _, Acc) -> lists:reverse(Acc);
inf_tuple_sets(_, [], Acc) -> lists:reverse(Acc).

inf_tuples_in_sets([?tuple(Elements1, _, ?any)], L2) ->
  NewList = [t_inf_lists_strict(Elements1, Elements2)
	     || ?tuple(Elements2, _, _) <- L2],
  [t_tuple(Es) || Es <- NewList, Es =/= bottom];
inf_tuples_in_sets(L1, [?tuple(Elements2, _, ?any)]) ->
  NewList = [t_inf_lists_strict(Elements1, Elements2)
	     || ?tuple(Elements1, _, _) <- L1],
  [t_tuple(Es) || Es <- NewList, Es =/= bottom];
inf_tuples_in_sets(L1, L2) ->
  inf_tuples_in_sets2(L1, L2, []).

inf_tuples_in_sets2([?tuple(Elements1, Arity, Tag)|Ts1],
                    [?tuple(Elements2, Arity, Tag)|Ts2], Acc) ->
  case t_inf_lists_strict(Elements1, Elements2) of
    bottom -> inf_tuples_in_sets2(Ts1, Ts2, Acc);
    NewElements ->
      inf_tuples_in_sets2(Ts1, Ts2, [?tuple(NewElements, Arity, Tag)|Acc])
  end;
inf_tuples_in_sets2([?tuple(_, _, Tag1)|Ts1] = L1,
                    [?tuple(_, _, Tag2)|Ts2] = L2, Acc) ->
  if Tag1 < Tag2 -> inf_tuples_in_sets2(Ts1, L2, Acc);
     Tag1 > Tag2 -> inf_tuples_in_sets2(L1, Ts2, Acc)
  end;
inf_tuples_in_sets2([], _, Acc) -> lists:reverse(Acc);
inf_tuples_in_sets2(_, [], Acc) -> lists:reverse(Acc).

inf_union(U1, U2) ->
  OpaqueFun =
    fun(Union1, Union2, InfFun) ->
        ?untagged_union(_,_,_,_,_,_,_,_) = Union1,
        ?untagged_union(A,B,F,I,L,N,T,Map) = Union2,
        List = [A,B,F,I,L,N,T,Map],
        %% FIXME: Faking ?none opaque -- remove argument.
        inf_union_collect(List, InfFun, [], [])
    end,
  {O1, ThrowList1} =
    OpaqueFun(U1, U2, fun(E, Opaque) -> t_inf(Opaque, E) end),
  {O2, ThrowList2} =
    OpaqueFun(U2, U1, fun(E, Opaque) -> t_inf(E, Opaque) end),
  {Union, ThrowList3} = inf_union(U1, U2, ?none, [], []),
  ThrowList = lists:merge3(ThrowList1, ThrowList2, ThrowList3),
  case t_sup([O1, O2, Union]) of
    ?none when ThrowList =/= [] -> throw({pos, lists:usort(ThrowList)});
    Sup -> Sup
  end.

inf_union_collect([], _InfFun, InfList, ThrowList) ->
  {t_sup(InfList), lists:usort(ThrowList)};
inf_union_collect([?none|L], InfFun, InfList, ThrowList) ->
  inf_union_collect(L, InfFun, [?none|InfList], ThrowList);
inf_union_collect([E|L], InfFun, InfList, ThrowList) ->
  try InfFun(E, ?none)of
    Inf ->
      inf_union_collect(L, InfFun, [Inf|InfList], ThrowList)
  catch throw:{pos, Ns} ->
      inf_union_collect(L, InfFun, InfList, Ns ++ ThrowList)
  end.

inf_union([?none|Left1], [?none|Left2], Type, Acc, ThrowList) ->
  inf_union(Left1, Left2, Type, [?none|Acc], ThrowList);
inf_union([T1|Left1], [T2|Left2], Type, Acc, ThrowList) ->
  try t_inf(T1, T2) of
    ?none ->
      inf_union(Left1, Left2, Type, [?none|Acc], ThrowList);
    T when Type =:= ?none ->
      inf_union(Left1, Left2, T, [T|Acc], ThrowList);
    T ->
      inf_union(Left1, Left2, ?union_tag, [T|Acc], ThrowList)
  catch
    throw:{pos, Ns} ->
      inf_union(Left1, Left2, Type, [?none|Acc], Ns ++ ThrowList)
  end;
inf_union([], [], Type, Acc, ThrowList) ->
  case Type of
    ?union_tag ->
      {?union(lists:reverse(Acc)), ThrowList};
    _ ->
      {Type, ThrowList}
  end.

inf_bitstr(U1, B1, U2, B2) ->
  GCD = gcd(U1, U2),
  case (B2-B1) rem GCD of
    0 ->
      U = (U1*U2) div GCD,
      B = findfirst(0, 0, U1, B1, U2, B2),
      t_bitstr(U, B);
    _ ->
      ?none
  end.

findfirst(N1, N2, U1, B1, U2, B2) ->
  Val1 = U1*N1+B1,
  Val2 = U2*N2+B2,
  if Val1 =:= Val2 ->
      Val1;
     Val1 > Val2 ->
      N2_1 = N2 + max((Val1 - Val2) div U2, 1),
      findfirst(N1, N2_1, U1, B1, U2, B2);
     Val1 < Val2 ->
      N1_1 = N1 + max((Val2 - Val1) div U1, 1),
      findfirst(N1_1, N2, U1, B1, U2, B2)
  end.

%%-----------------------------------------------------------------------------
%% Substitution of variables
%%

-type subst_table() :: #{any() => erl_type()}.

-spec t_subst(erl_type(), subst_table()) -> erl_type().

t_subst(T, Map) ->
  case t_has_var(T) of
    true -> t_subst_aux(T, Map);
    false -> T
  end.

-spec subst_all_vars_to_any(erl_type()) -> erl_type().

subst_all_vars_to_any(T) ->
  t_subst(T, #{}).

t_subst_aux(?var(Id), Map) ->
  case Map of
    #{Id := Type} -> Type;
    #{} -> ?any
  end;
t_subst_aux(?list(Contents, Termination, Size), Map) ->
  case t_subst_aux(Contents, Map) of
    ?none -> ?none;
    ?unit -> ?none;
    NewContents ->
      %% Be careful here to make the termination collapse if necessary.
      case t_subst_aux(Termination, Map) of
	?nil -> ?list(NewContents, ?nil, Size);
	?any -> ?list(NewContents, ?any, Size);
	Other ->
	  ?list(NewContents2, NewTermination, _) = t_cons(NewContents, Other),
	  ?list(NewContents2, NewTermination, Size)
      end
  end;
t_subst_aux(?function(Domain, Range), Map) ->
  ?function(t_subst_aux(Domain, Map), t_subst_aux(Range, Map));
t_subst_aux(?nominal(N, S), Map) ->
  ?nominal(N, t_subst_aux(S, Map));
t_subst_aux(?nominal_set(N, S), Map) ->
  normalize_nominal_set([t_subst_aux(X, Map) || X <- N],
                        t_subst_aux(S, Map),
                        []);
t_subst_aux(?product(Types), Map) ->
  ?product([t_subst_aux(T, Map) || T <- Types]);
t_subst_aux(?tuple(?any, ?any, ?any) = T, _Map) ->
  T;
t_subst_aux(?tuple(Elements, _Arity, _Tag), Map) ->
  t_tuple([t_subst_aux(E, Map) || E <- Elements]);
t_subst_aux(?tuple_set(_) = TS, Map) ->
  t_sup([t_subst_aux(T, Map) || T <- t_tuple_subtypes(TS)]);
t_subst_aux(?map(Pairs, DefK, DefV), Map) ->
  t_map([{K, MNess, t_subst_aux(V, Map)} || {K, MNess, V} <- Pairs],
	t_subst_aux(DefK, Map), t_subst_aux(DefV, Map));
t_subst_aux(?union(List), Map) ->
  ?union([t_subst_aux(E, Map) || E <- List]);
t_subst_aux(T, _Map) ->
  T.

%%-----------------------------------------------------------------------------
%% Unification
%%

-spec t_unify_table_only(erl_type(), erl_type()) -> var_table().

%% A simplified version of t_unify/2 which returns the variable
%% bindings only. It is faster, mostly because t_subst() is not
%% called.

t_unify_table_only(T1, T2) ->
  t_unify_table_only(T1, T2, #{}).

t_unify_table_only(?var(Id), ?var(Id), VarMap) ->
  VarMap;
t_unify_table_only(?var(Id1) = LHS, ?var(Id2) = RHS, VarMap) ->
  case VarMap of
    #{ Id1 := Type1, Id2 := Type2} ->
        t_unify_table_only(Type1, Type2, VarMap);
    #{ Id1 := Type } ->
        t_unify_table_only(Type, RHS, VarMap);
    #{ Id2 := Type } ->
        t_unify_table_only(LHS, Type, VarMap);
    #{} ->
        VarMap#{ Id1 => LHS, Id2 => LHS }
  end;
t_unify_table_only(?var(Id), Type, VarMap) ->
  case VarMap of
    #{Id := VarType} ->
      t_unify_table_only(VarType, Type, VarMap);
    #{} ->
      VarMap#{Id => Type}
  end;
t_unify_table_only(Type, ?var(Id), VarMap) ->
  case VarMap of
    #{Id := VarType} ->
       t_unify_table_only(VarType, Type, VarMap);
    #{} ->
      VarMap#{Id => Type}
  end;
t_unify_table_only(?function(Domain1, Range1), ?function(Domain2, Range2), VarMap) ->
  VarMap1 = t_unify_table_only(Domain1, Domain2, VarMap),
  t_unify_table_only(Range1, Range2, VarMap1);
%% Nominals
t_unify_table_only(?nominal(N1, S1)=T1, ?nominal(N2, S2)=T2, VarMap) ->
  case N1 =:= N2 of
    true -> t_unify_table_only(S1, S2, VarMap);
    false -> throw({mismatch, T1, T2})
  end;
%%
t_unify_table_only(?nominal_set([H1], S1), ?nominal_set([H2], S2), VarMap) ->
  VarMap1 = t_unify_table_only(H1, H2, VarMap),
  t_unify_table_only(S1, S2, VarMap1);
t_unify_table_only(?nominal_set([H1 | T1], Str1),
                   ?nominal_set([H2 | T2], Str2), VarMap) ->
  VarMap1 = t_unify_table_only(H1, H2, VarMap),
  t_unify_table_only(?nominal_set(T1, Str1), ?nominal_set(T2, Str2), VarMap1);
%%
t_unify_table_only(?nominal(_, _) = T1, ?nominal_set(_, _) = T2, VarMap) ->
  t_unify_table_only(T2, T1, VarMap);
t_unify_table_only(?nominal_set(_, _) = T1, ?nominal(_, _) = T2, VarMap) -> 
  t_unify_table_only(T1, ?nominal_set(T2, ?none), VarMap);
%%
t_unify_table_only(?nominal_set([?nominal(_, NomS)], Other), T2, VarMap) ->
  t_unify_table_only(t_sup(NomS, Other), T2, VarMap);
t_unify_table_only(?nominal_set([?nominal(_, NomS) | T], Other), T2, VarMap) ->
  VarMap1 = t_unify_table_only(t_sup(NomS, Other), T2, VarMap),
  t_unify_table_only(?nominal_set(T, Other), T2, VarMap1);
t_unify_table_only(T1, ?nominal_set(_, _) = T2, VarMap) ->
  t_unify_table_only(T2, T1, VarMap);
%%
t_unify_table_only(?nominal(_, S1), T2, VarMap) ->
  t_unify_table_only(S1, T2, VarMap);
t_unify_table_only(T1, ?nominal(_, _)=T2, VarMap) ->
  t_unify_table_only(T2, T1, VarMap);
%%
t_unify_table_only(?list(Contents1, Termination1, Size),
	?list(Contents2, Termination2, Size), VarMap) ->
  VarMap1 = t_unify_table_only(Contents1, Contents2, VarMap),
  t_unify_table_only(Termination1, Termination2, VarMap1);
t_unify_table_only(?product(Types1), ?product(Types2), VarMap) ->
  unify_lists_table_only(Types1, Types2, VarMap);
t_unify_table_only(?tuple(?any, ?any, ?any), ?tuple(?any, ?any, ?any), VarMap) ->
  VarMap;
t_unify_table_only(?tuple(Elements1, Arity, _),
	?tuple(Elements2, Arity, _), VarMap) when Arity =/= ?any ->
  unify_lists_table_only(Elements1, Elements2, VarMap);
t_unify_table_only(?tuple_set([{Arity, _}]) = T1,
	?tuple(_, Arity, _) = T2, VarMap) when Arity =/= ?any ->
  unify_tuple_set_and_tuple1_table_only(T1, T2, VarMap);
t_unify_table_only(?tuple(_, Arity, _) = T1,
	?tuple_set([{Arity, _}]) = T2, VarMap) when Arity =/= ?any ->
  unify_tuple_set_and_tuple2_table_only(T1, T2, VarMap);
t_unify_table_only(?tuple_set(List1) = T1, ?tuple_set(List2) = T2, VarMap) ->
  try
    unify_lists_table_only(lists:append([T || {_Arity, T} <- List1]),
                           lists:append([T || {_Arity, T} <- List2]), VarMap)
  catch _:_ -> throw({mismatch, T1, T2})
  end;
t_unify_table_only(?map(_, ADefK, ADefV) = A, ?map(_, BDefK, BDefV) = B, VarMap0) ->
  VarMap1 = t_unify_table_only(ADefK, BDefK, VarMap0),
  VarMap2 = t_unify_table_only(ADefV, BDefV, VarMap1),
  {[], VarMap} =
    map_pairwise_merge_foldr(
      fun(_K, MNess, V1, MNess, V2, {Pairs0, VarMap3}) ->
	  %% We know that the keys unify and do not contain variables, or they
	  %% would not be singletons
	  %% TODO: Should V=?none (known missing keys) be handled special?
	  VarMap4 = t_unify_table_only(V1, V2, VarMap3),
	  {Pairs0, VarMap4};
	 (_K, _, V1, _, V2, {Pairs0, VarMap3}) ->
	  %% One mandatory and one optional; what should be done in this case?
	  VarMap4 = t_unify_table_only(V1, V2, VarMap3),
	  {Pairs0, VarMap4}
      end, {[], VarMap2}, A, B),
  VarMap;
t_unify_table_only(T, T, VarMap) ->
  VarMap;
t_unify_table_only(T1, T2, _) ->
  throw({mismatch, T1, T2}).

%% Two functions since t_unify_table_only is not symmetric.
unify_tuple_set_and_tuple1_table_only(?tuple_set([{Arity, List}]),
                                      ?tuple(Elements2, Arity, _), VarMap) ->
  %% Can only work if the single tuple has variables at correct places.
  unify_lists_table_only(sup_tuple_elements(List), Elements2, VarMap).

unify_tuple_set_and_tuple2_table_only(?tuple(Elements2, Arity, _),
                                      ?tuple_set([{Arity, List}]), VarMap) ->
  %% Can only work if the single tuple has variables at correct places.
  unify_lists_table_only(Elements2, sup_tuple_elements(List), VarMap).

unify_lists_table_only([T1|Left1], [T2|Left2], VarMap) ->
  NewVarMap = t_unify_table_only(T1, T2, VarMap),
  unify_lists_table_only(Left1, Left2, NewVarMap);
unify_lists_table_only([], [], VarMap) ->
  VarMap.

%%-----------------------------------------------------------------------------
%% Subtraction.
%%
%% Note that the subtraction is an approximation since we do not have
%% negative types. Also, tuples and products should be handled using
%% the cartesian product of the elements, but this is not feasible to
%% do.
%%
%% Example: {a|b,c|d}\{a,d} = {a,c}|{a,d}|{b,c}|{b,d} \ {a,d} =
%%                          = {a,c}|{b,c}|{b,d} = {a|b,c|d}
%%
%% Instead, we can subtract if all elements but one becomes none after
%% subtracting element-wise.
%%
%% Example: {a|b,c|d}\{a|b,d} = {a,c}|{a,d}|{b,c}|{b,d} \ {a,d}|{b,d} =
%%                            = {a,c}|{b,c} = {a|b,c}

-spec t_subtract_list(erl_type(), [erl_type()]) -> erl_type().

t_subtract_list(T1, [T2|Left]) ->
  t_subtract_list(t_subtract(T1, T2), Left);
t_subtract_list(T, []) ->
  T.

-spec t_subtract(erl_type(), erl_type()) -> erl_type().

t_subtract(LHS, RHS) ->
  Res = t_subtract_aux(LHS, RHS),
  %% `Res` must be at least as specific as `LHS`, and the latter must overlap
  %% with `RHS` if the result differs from `LHS`.
  ?debug(t_is_subtype(subst_all_vars_to_any(Res),
                      subst_all_vars_to_any(LHS)) andalso
          (Res =:= LHS) orelse (not t_is_impossible(t_inf(LHS, RHS))),
         {LHS, RHS, Res}),
  Res.

t_subtract_aux(_, ?any) -> ?none;
t_subtract_aux(T, ?var(_)) -> T;
t_subtract_aux(?any, _) -> ?any;
t_subtract_aux(?var(_) = T, _) -> T;
t_subtract_aux(T, ?unit) -> T;
t_subtract_aux(?unit, _) -> ?unit;
t_subtract_aux(?none, _) -> ?none;
t_subtract_aux(T, ?none) -> T;
t_subtract_aux(?atom(Set1), ?atom(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?atom(Set)
  end;
t_subtract_aux(?bitstr(U1, B1), ?bitstr(U2, B2)) ->
  subtract_bin(t_bitstr(U1, B1), t_inf(t_bitstr(U1, B1), t_bitstr(U2, B2)));
t_subtract_aux(?function(_, _) = T1, ?function(_, _) = T2) ->
  case t_is_subtype(T1, T2) of
    true -> ?none;
    false -> T1
  end;
t_subtract_aux(?identifier(Set1), ?identifier(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?identifier(Set)
  end;
t_subtract_aux(?nil, ?nil) ->
  ?none;
t_subtract_aux(?nil, ?nonempty_list(_, _)) ->
  ?nil;
t_subtract_aux(?nil, ?list(_, _, _)) ->
  ?none;
t_subtract_aux(?list(Contents, Termination, _Size) = T, ?nil) ->
  case Termination =:= ?nil of
    true -> ?nonempty_list(Contents, Termination);
    false -> T
  end;
t_subtract_aux(?list(Contents1, Termination1, Size1) = T,
	   ?list(Contents2, Termination2, Size2)) ->
  case t_is_subtype(Contents1, Contents2) of
    true ->
      case t_is_subtype(Termination1, Termination2) of
	true ->
	  case {Size1, Size2} of
	    {?nonempty_qual, ?unknown_qual} -> ?none;
	    {?unknown_qual, ?nonempty_qual} -> ?nil;
	    {S, S} -> ?none
	  end;
	false ->
	  %% If the termination is not covered by the subtracted type
	  %% we cannot really say anything about the result.
	  T
      end;
    false ->
      %% All contents must be covered if there is going to be any
      %% change to the list.
      T
  end;
t_subtract_aux(?float, ?float) -> ?none;
t_subtract_aux(?number(_, _) = T1, ?float) -> t_inf(T1, t_integer());
t_subtract_aux(?float, ?number(_Set, Tag)) ->
  case Tag of
    ?unknown_qual -> ?none;
    _ -> ?float
  end;
t_subtract_aux(?nominal_set(_, _)=LHS, ?nominal_set(_, _)=RHS) -> 
  subtract_nominal_sets(LHS, RHS);
t_subtract_aux(?nominal_set(_, _)=LHS, ?nominal(_, _) = RHS) ->
  t_subtract_aux(LHS, ?nominal_set([RHS], ?none));
t_subtract_aux(?nominal_set(LHS_Ns, LHS_S)=LHS, RHS) ->
  case t_inf(LHS, RHS) of
    ?nominal_set(_, _)=Overlap ->
      t_subtract_aux(LHS, Overlap);
    ?nominal(_, _)=Overlap ->
      t_subtract_aux(LHS, Overlap);
    Overlap ->
      normalize_nominal_set(LHS_Ns, t_subtract_aux(LHS_S, Overlap), [])
  end;
t_subtract_aux(S1, ?nominal_set(_, S2)) ->
  t_subtract_aux(S1, S2);
t_subtract_aux(?nominal(Name, LHS_S), ?nominal(Name, RHS_S)) ->
  t_nominal(Name, t_subtract_aux(LHS_S, RHS_S));
t_subtract_aux(?nominal(LHS_Name, _)=LHS, RHS) ->
  case t_inf(LHS, RHS) of
    ?nominal(LHS_Name, _)=Overlap -> t_subtract_aux(LHS, Overlap);
    _ -> LHS
  end;
t_subtract_aux(S1, ?nominal(_, _)) ->
  S1;
t_subtract_aux(?number(_, _), ?number(?any, ?unknown_qual)) -> ?none;
t_subtract_aux(?number(_, _) = T1, ?integer(?any)) -> t_inf(?float, T1);
t_subtract_aux(?int_set(Set1), ?int_set(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?int_set(Set)
  end;
t_subtract_aux(?int_range(From1, To1) = T1, ?int_range(_, _) = T2) ->
  case t_inf(T1, T2) of
    ?none -> T1;
    ?int_range(From1, To1) -> ?none;
    ?int_range(neg_inf, To) -> t_from_range(To + 1, To1);
    ?int_range(From, pos_inf) -> t_from_range(From1, From - 1);
    ?int_range(From, To) -> t_sup(t_from_range(From1, From - 1),
				  t_from_range(To + 1, To))
  end;
t_subtract_aux(?int_range(From, To) = T1, ?int_set(Set)) ->
  NewFrom = case set_is_element(From, Set) of
	      true -> From + 1;
	      false -> From
	    end,
  NewTo = case set_is_element(To, Set) of
	    true -> To - 1;
	    false -> To
	  end,
  if (NewFrom =:= From) and (NewTo =:= To) -> T1;
     true -> t_from_range(NewFrom, NewTo)
  end;
t_subtract_aux(?int_set(Set), ?int_range(From, To)) ->
  case set_filter(fun(X) -> not ((X =< From) orelse (X >= To)) end, Set) of
    ?none -> ?none;
    NewSet -> ?int_set(NewSet)
  end;
t_subtract_aux(?integer(?any) = T1, ?integer(_)) -> T1;
t_subtract_aux(?number(_, _) = T1, ?number(_, _)) -> T1;
t_subtract_aux(?tuple(_, _, _), ?tuple(?any, ?any, ?any)) -> ?none;
t_subtract_aux(?tuple_set(_), ?tuple(?any, ?any, ?any)) -> ?none;
t_subtract_aux(?tuple(?any, ?any, ?any) = T1, ?tuple_set(_)) -> T1;
t_subtract_aux(?tuple(Elements1, Arity1, _Tag1) = T1,
	   ?tuple(Elements2, Arity2, _Tag2)) ->
  if Arity1 =/= Arity2 -> T1;
     Arity1 =:= Arity2 ->
      NewElements = t_subtract_lists(Elements1, Elements2),
      case [E || E <- NewElements, E =/= ?none] of
	[] -> ?none;
	[_] -> t_tuple(replace_nontrivial_element(Elements1, NewElements));
	_ -> T1
      end
  end;
t_subtract_aux(?tuple_set(List1) = T1, ?tuple(_, Arity, _) = T2) ->
  case orddict:find(Arity, List1) of
    error -> T1;
    {ok, List2} ->
      TuplesLeft0 = [Tuple || {_Arity, Tuple} <- orddict:erase(Arity, List1)],
      TuplesLeft1 = lists:append(TuplesLeft0),
      t_sup([t_subtract_aux(L, T2) || L <- List2] ++ TuplesLeft1)
  end;
t_subtract_aux(?tuple(_, Arity, _) = T1, ?tuple_set(List1)) ->
  case orddict:find(Arity, List1) of
    error -> T1;
    {ok, List2} -> t_inf([t_subtract_aux(T1, L) || L <- List2])
  end;
t_subtract_aux(?tuple_set(_) = T1, ?tuple_set(_) = T2) ->
  t_sup([t_subtract_aux(T, T2) || T <- t_tuple_subtypes(T1)]);
t_subtract_aux(?product(Elements1) = T1, ?product(Elements2)) ->
  Arity1 = length(Elements1),
  Arity2 = length(Elements2),
  if Arity1 =/= Arity2 -> T1;
     Arity1 =:= Arity2 ->
      NewElements = t_subtract_lists(Elements1, Elements2),
      case [E || E <- NewElements, E =/= ?none] of
	[] -> ?none;
	[_] -> t_product(replace_nontrivial_element(Elements1, NewElements));
	_ -> T1
      end
  end;
t_subtract_aux(?map(APairs, ADefK, ADefV) = A, ?map(_, BDefK, BDefV) = B) ->
  case t_is_subtype(ADefK, BDefK) andalso t_is_subtype(ADefV, BDefV) of
    false -> A;
    true ->
      %% We fold over the maps to produce a list of constraints, where
      %% constraints are additional key-value pairs to put in Pairs. Only one
      %% constraint need to be applied to produce a type that excludes the
      %% right-hand-side type, so if more than one constraint is produced, we
      %% just return the left-hand-side argument.
      %%
      %% Each case of the fold may either conclude that
      %%  * The arguments constrain A at least as much as B, i.e. that A so far
      %%    is a subtype of B. In that case they return false
      %%  * That for the particular arguments, A being a subtype of B does not
      %%    hold, but the infimum of A and B is nonempty, and by narrowing a
      %%    pair in A, we can create a type that excludes some elements in the
      %%    infinumum. In that case, they will return that pair.
      %%  * That for the particular arguments, A being a subtype of B does not
      %%    hold, and either the infinumum of A and B is empty, or it is not
      %%    possible with the current representation to create a type that
      %%    excludes elements from B without also excluding elements that are
      %%    only in A. In that case, it will return the pair from A unchanged.
      case
	map_pairwise_merge(
	  %% If V1 is a subtype of V2, the case that K does not exist in A
	  %% remain.
	  fun(K, ?opt, V1, ?mand, V2) -> {K, ?opt, t_subtract_aux(V1, V2)};
	     (K, _,    V1, _,     V2) ->
	      %% If we subtract an optional key, that leaves a mandatory key
	      case t_subtract_aux(V1, V2) of
		?none -> false;
		Partial -> {K, ?mand, Partial}
	      end
	  end, A, B)
      of
	  %% We produce a list of keys that are constrained. As only one of
	  %% these should apply at a time, we can't represent the difference if
	  %% more than one constraint is produced. If we applied all of them,
	  %% that would make an underapproximation, which we must not do.
	  [] -> ?none; %% A is a subtype of B
	  [E] -> t_map(mapdict_store(E, APairs), ADefK, ADefV);
	  _ -> A
      end
  end;
t_subtract_aux(?product(P1), _) ->
  ?product(P1);
t_subtract_aux(T, ?product(_)) ->
  T;
t_subtract_aux(?union(U1), ?union(U2)) ->
  subtract_union(U1, U2);
t_subtract_aux(T1, T2) ->
  ?union(U1) = force_union(T1),
  ?union(U2) = force_union(T2),
  subtract_union(U1, U2).

-spec t_subtract_lists([erl_type()], [erl_type()]) -> [erl_type()].

t_subtract_lists(L1, L2) ->
  [t_subtract(T1, T2) || T1 <- L1 && T2 <- L2].

-spec subtract_union([erl_type(),...], [erl_type(),...]) -> erl_type().

subtract_union(U1, U2) ->
  ?untagged_union(A1,B1,F1,I1,L1,N1,T1,Map1) = U1,
  ?untagged_union(A2,B2,F2,I2,L2,N2,T2,Map2) = U2,
  List1 = ?untagged_union(A1,B1,F1,I1,L1,N1,T1,Map1),
  List2 = ?untagged_union(A2,B2,F2,I2,L2,N2,T2,Map2),
  subtract_union(List1, List2, ?none, []).

subtract_union([T1|Left1], [T2|Left2], Type, Acc) ->
  case t_subtract(T1, T2) of
    ?none -> subtract_union(Left1, Left2, Type, [?none|Acc]);
    T when Type =:= none -> subtract_union(Left1, Left2, T, [T|Acc]);
    T -> subtract_union(Left1, Left2, ?union_tag, [T|Acc])
  end;
subtract_union([], [], Type, Acc) ->
  case Type of
    ?union_tag ->
      ?union(lists:reverse(Acc));
    _ ->
      Type
  end.

subtract_nominal_sets(?nominal_set(LHS_Ns, LHS_S),
                      ?nominal_set(RHS_Ns, RHS_S)) ->
  %% See inf_nominal_sets/3
  sns_cartesian([LHS_S | LHS_Ns], [RHS_S | RHS_Ns]).

sns_cartesian([A | As], Bs) ->
  case sns_cartesian_1(A, Bs) of
    ?none -> sns_cartesian(As, Bs);
    T -> t_sup_aux(T, sns_cartesian(As, Bs))
  end;
sns_cartesian([], _Bs) ->
  ?none.

sns_cartesian_1(A, [B | Bs]) ->
  sns_cartesian_1(t_subtract(A, B), Bs);
sns_cartesian_1(A, []) ->
  A.

%% Helper for tuple and product subtraction. The second list
%% should contain a single element that is not none. That element
%% will replace the element in the corresponding position in the
%% first list.

replace_nontrivial_element(Left, Right) ->
  [case T2 of
     ?none -> T1;
     _ -> T2
   end || T1 <- Left && T2 <- Right].

subtract_bin(?bitstr(U1, B1), ?bitstr(U1, B1)) ->
  ?none;
subtract_bin(?bitstr(U1, B1), ?none) ->
  t_bitstr(U1, B1);
subtract_bin(?bitstr(U1, B1), ?bitstr(0, B1)) ->
  t_bitstr(U1, B1+U1);
subtract_bin(?bitstr(U1, B1), ?bitstr(U1, B2)) ->
  if (B1+U1) =/= B2 -> t_bitstr(0, B1);
     true -> t_bitstr(U1, B1)
  end;
subtract_bin(?bitstr(U1, B1), ?bitstr(U2, B2)) ->
  if (2 * U1) =:= U2 ->
      if B1 =:= B2 ->
	  t_bitstr(U2, B1+U1);
	 (B1 + U1) =:= B2 ->
	  t_bitstr(U2, B1);
	 true ->
	  t_bitstr(U1, B1)
      end;
     true ->
      t_bitstr(U1, B1)
  end.

%%-----------------------------------------------------------------------------
%% Relations
%%

-spec t_is_equal(erl_type(), erl_type()) -> boolean().

t_is_equal(T, T)  -> true;
t_is_equal(_, _) -> false.

-spec t_is_subtype(erl_type(), erl_type()) -> boolean().

t_is_subtype(T1, T2) ->
  Inf = t_inf_aux(T1, T2),
  t_is_equal(T1, Inf).

-spec t_do_overlap(erl_type(), erl_type()) -> boolean().

t_do_overlap(TypeA, TypeB) ->
  not (t_is_impossible(t_inf(TypeA, TypeB))).

-spec t_structural(erl_type()) -> erl_type().

t_structural(?nominal(_, S)) ->
  t_structural(S);
t_structural(?nominal_set([], S)) ->
  t_structural(S);
t_structural(?nominal_set([?nominal(_, S1)|T], S)) ->
  t_structural(?nominal_set(T, t_sup(S, S1)));
t_structural(?list(ElemT, Termination, Sz)) ->
  ?list(t_structural(ElemT), t_structural(Termination), Sz);
t_structural(?tuple(?any, _, _) = T) -> T;
t_structural(?tuple(ArgTs, Sz, Tag)) when is_list(ArgTs) ->
  NewArgTs = [t_structural(A) || A <- ArgTs],
  ?tuple(NewArgTs, Sz, Tag);
t_structural(?tuple_set(Set)) ->
  NewSet = [{Sz, [t_structural(T) || T <- Tuples]}
	    || {Sz, Tuples} <- Set],
  ?tuple_set(NewSet);
t_structural(?product(Types)) ->
  ?product([t_structural(T) || T <- Types]);
t_structural(?function(Domain, Range)) ->
  ?function(t_structural(Domain), t_structural(Range));
t_structural(?union(?untagged_union(A,B,F,I,L,N,T,Map))) ->
  UL = t_structural(L),
  UT = t_structural(T),
  UF = t_structural(F),
  UMap = t_structural(Map),
  t_sup([A,B,UF,I,UL,N,UT,UMap]);
t_structural(?map(Pairs,DefK,DefV)) ->
  t_map([{t_structural(K), MNess, t_structural(V)}
         || {K, MNess, V} <- Pairs],
        t_structural(DefK),
        t_structural(DefV));
t_structural(T) ->
  T.

%%-----------------------------------------------------------------------------
%% K-depth abstraction.
%%
%% t_limit/2 is the exported function, which checks the type of the
%% second argument and calls the module local t_limit_k/2 function.
%%

-spec t_limit(erl_type(), integer()) -> erl_type().

t_limit(Term, K) when is_integer(K) ->
  IsLimited = is_limited(Term, K),
  %% `is_limited/2` must mirror `t_limit_k/2`
  ?debug(IsLimited =:= (Term =:= t_limit_k(Term, K)),
         {IsLimited, Term, K}),
  case IsLimited of
    true ->
      Term;
    false ->
      Res = t_limit_k(Term, K),
      %% `Res` must be strictly more general than `Term`
      ?debug(t_is_subtype(subst_all_vars_to_any(Term),
                          subst_all_vars_to_any(Res)),
             {Term, Res}),
      Res
  end.

%% Optimized mirror of t_limit_k/2 that merely checks whether the latter will
%% change the input term in any way. Needless to say this _must_ mirror
%% t_limit_k/2.
is_limited(?any, _) ->
  true;
is_limited(_, K) when K =< 0 ->
  false;
is_limited(?tuple(?any, ?any, ?any), _K) ->
  true;
is_limited(?tuple(Elements, _Arity, Qual), K) ->
  ?debug(length(Elements) =:= _Arity, _Arity),
  if
    K =:= 1 -> t_is_any(Qual) andalso are_all_limited(Elements, K - 1);
    true -> are_all_limited(Elements, K - 1)
  end;
is_limited(?tuple_set(_) = T, K) ->
  are_all_limited(t_tuple_subtypes(T), K);
is_limited(?list(Elements, ?nil, _Size), K) ->
  is_limited(Elements, K - 1);
is_limited(?list(Elements, Termination, _Size), K) ->
  %% We do not want to lose the termination information, always pass a K of at
  %% least 1 for that
  is_limited(Elements, K - 1) andalso is_limited(Termination, max(1, K - 1));
is_limited(?function(Domain, Range), K) ->
  is_limited(Domain, K) andalso is_limited(Range, K-1);
is_limited(?product(Elements), K) ->
  are_all_limited(Elements, K - 1);
is_limited(?union(Elements), K) ->
  are_all_limited(Elements, K);
is_limited(?nominal_set(Elements, S), K) ->
  is_limited(S, K) andalso are_all_limited(Elements, K);
is_limited(?nominal(_, S), K) ->
  %% To simplify checking opacity violations, nominals aren't counted in the
  %% term depth.
  is_limited(S, K);
is_limited(?map(Pairs, DefK, DefV), K) ->
  %% Use the fact that t_sup() does not increase the depth.
  K1 = K - 1,
  lists:all(fun({Key, _, Value}) ->
                    is_limited(Key, K1) andalso is_limited(Value, K1)
            end, Pairs)
    andalso is_limited(DefK, K1) andalso is_limited(DefV, K1);
is_limited(_, _K) -> true.

are_all_limited([E | Es], K) ->
  is_limited(E, K) andalso are_all_limited(Es, K);
are_all_limited([], _) ->
  true.

t_limit_k(_, K) when K =< 0 ->
  ?any;
t_limit_k(?tuple(?any, ?any, ?any) = T, _K) ->
  T;
t_limit_k(?tuple(Elements, Arity, _), K) ->
  if
     K =:= 1 -> t_tuple(Arity);
     true -> t_tuple([t_limit_k(E, K-1) || E <- Elements])
  end;
t_limit_k(?tuple_set(_) = T, K) ->
  t_sup([t_limit_k(Tuple, K) || Tuple <- t_tuple_subtypes(T)]);
t_limit_k(?list(Elements, ?nil, Size), K) ->
  NewElements = t_limit_k(Elements, K - 1),
  ?list(NewElements, ?nil, Size);
t_limit_k(?list(Elements, Termination, Size), K) ->
  %% We do not want to lose the termination information, always pass a K of at
  %% least 1 for that.
  ?list(t_limit_k(Elements, K - 1),
        t_limit_k(Termination, max(1, K - 1)),
        Size);
t_limit_k(?function(Domain, Range), K) ->
  %% The domain is either a product or any() so we do not decrease the K.
  ?function(t_limit_k(Domain, K), t_limit_k(Range, K-1));
t_limit_k(?product(Elements), K) ->
  ?product([t_limit_k(X, K - 1) || X <- Elements]);
t_limit_k(?union(Elements), K) ->
  ?union([t_limit_k(X, K) || X <- Elements]);
t_limit_k(?nominal(Name, Inner), K) ->
  %% To simplify checking opacity violations, nominals aren't counted in the
  %% term depth.
  ?nominal(Name, t_limit_k(Inner, K));
t_limit_k(?nominal_set(Elements, S), K) ->
  normalize_nominal_set([t_limit_k(X, K) || X <- Elements],
                        t_limit_k(S, K),
                        []);
t_limit_k(?map(Pairs0, DefK0, DefV0), K) ->
  Fun = fun({EK, MNess, EV}, {Exact, DefK1, DefV1}) ->
                LV = t_limit_k(EV, K - 1),
                case t_limit_k(EK, K - 1) of
                  EK -> {[{EK, MNess, LV}|Exact], DefK1, DefV1};
                  LK -> {Exact, t_sup(LK, DefK1), t_sup(LV, DefV1)}
                end
        end,
  {Pairs, DefK2, DefV2} = lists:foldr(Fun, {[], DefK0, DefV0}, Pairs0),
  t_map(Pairs, t_limit_k(DefK2, K - 1), t_limit_k(DefV2, K - 1));
t_limit_k(T, _K) ->
  T.

%%============================================================================
%%
%% Abstract records. Used for comparing contracts.
%%
%%============================================================================

-spec t_abstract_records(erl_type(), type_table()) -> erl_type().

t_abstract_records(?list(Contents, Termination, Size), RecDict) ->
  case t_abstract_records(Contents, RecDict) of
    ?none -> ?none;
    NewContents ->
      %% Be careful here to make the termination collapse if necessary.
      case t_abstract_records(Termination, RecDict) of
	?nil -> ?list(NewContents, ?nil, Size);
	?any -> ?list(NewContents, ?any, Size);
	Other ->
	  ?list(NewContents2, NewTermination, _) = t_cons(NewContents, Other),
	  ?list(NewContents2, NewTermination, Size)
      end
  end;
t_abstract_records(?function(Domain, Range), RecDict) ->
  ?function(t_abstract_records(Domain, RecDict),
	    t_abstract_records(Range, RecDict));
t_abstract_records(?product(Types), RecDict) ->
  ?product([t_abstract_records(T, RecDict) || T <- Types]);
t_abstract_records(?nominal(N, ?nominal(_, _)=S0), RecDict) ->
  case t_abstract_records(S0, RecDict) of
      ?nominal(_, _)=S -> ?nominal(N, S);
      _ -> ?any 
  end;
t_abstract_records(?nominal(N, ?nominal_set(_, _)=S0), RecDict) ->
  case t_abstract_records(S0, RecDict) of
      ?nominal_set(_, _)=S -> ?nominal(N, S);
      ?nominal(_, _)=S -> ?nominal(N, S);
      _ -> ?any 
  end;
t_abstract_records(?nominal(N, S), RecDict) ->
  ?nominal(N, t_abstract_records(S, RecDict));
t_abstract_records(?nominal_set(Elements, S), RecDict) ->
  normalize_nominal_set([t_abstract_records(X, RecDict) || X <- Elements],
                        t_abstract_records(S, RecDict),
                        []);
t_abstract_records(?union(Types), RecDict) ->
  t_sup([t_abstract_records(T, RecDict) || T <- Types]);
t_abstract_records(?tuple(?any, ?any, ?any) = T, _RecDict) ->
  T;
t_abstract_records(?tuple(Elements, Arity, ?atom(_) = Tag), RecDict) ->
  [TagAtom] = atom_vals(Tag),
  case lookup_record(TagAtom, Arity - 1, RecDict) of
    error -> t_tuple([t_abstract_records(E, RecDict) || E <- Elements]);
    {ok, Fields} -> t_tuple([Tag|[T || {_Name, _Abstr, T} <- Fields]])
  end;
t_abstract_records(?tuple(Elements, _Arity, _Tag), RecDict) ->
  t_tuple([t_abstract_records(E, RecDict) || E <- Elements]);
t_abstract_records(?tuple_set(_) = Tuples, RecDict) ->
  t_sup([t_abstract_records(T, RecDict) || T <- t_tuple_subtypes(Tuples)]);
t_abstract_records(T, _RecDict) ->
  T.

%%=============================================================================
%%
%% Prettyprinter
%%
%%=============================================================================

-spec t_to_string(erl_type()) -> string().

t_to_string(T) ->
  t_to_string(T, maps:new()).

-spec t_to_string(erl_type(), type_table()) -> string().

t_to_string(?any, _RecDict) ->
  "any()";
t_to_string(?none, _RecDict) ->
  "none()";
t_to_string(?unit, _RecDict) ->
  "no_return()";
t_to_string(?atom(?any), _RecDict) ->
  "atom()";
t_to_string(?atom(Set), _RecDict) ->
  case length(Set) of
    2 ->
      case set_is_element(true, Set) andalso set_is_element(false, Set) of
	true -> "boolean()";
	false -> set_to_string(Set)
      end;
    _ ->
      set_to_string(Set)
  end;
t_to_string(?bitstr(0, 0), _RecDict) ->
  "<<>>";
t_to_string(?bitstr(8, 0), _RecDict) ->
  "binary()";
t_to_string(?bitstr(8, 8), _RecDict) ->
  "nonempty_binary()";
t_to_string(?bitstr(1, 0), _RecDict) ->
  "bitstring()";
t_to_string(?bitstr(1, 1), _RecDict) ->
  "nonempty_bitstring()";
t_to_string(?bitstr(0, B), _RecDict) ->
  flat_format("<<_:~w>>", [B]);
t_to_string(?bitstr(U, 0), _RecDict) ->
  flat_format("<<_:_*~w>>", [U]);
t_to_string(?bitstr(U, B), _RecDict) ->
  flat_format("<<_:~w,_:_*~w>>", [B, U]);
t_to_string(?function(?any, ?any), _RecDict) ->
  "fun()";
t_to_string(?function(?any, Range), RecDict) ->
  "fun((...) -> " ++ t_to_string(Range, RecDict) ++ ")";
t_to_string(?function(?product(ArgList), Range), RecDict) ->
  "fun((" ++ comma_sequence(ArgList, RecDict) ++ ") -> "
    ++ t_to_string(Range, RecDict) ++ ")";
t_to_string(?identifier(Set), _RecDict) ->
  case Set of
    ?any -> "identifier()";
    _ ->
      flat_join([flat_format("~w()", [T]) || T <- Set], " | ")
  end;
t_to_string(?nil, _RecDict) ->
  "[]";
t_to_string(?nonempty_list(Contents, Termination), RecDict) ->
  ContentString = t_to_string(Contents, RecDict),
  case Termination of
    ?nil ->
      case Contents of
	?char -> "nonempty_string()";
	_ -> "["++ContentString++",...]"
      end;
    ?any ->
      %% Just a safety check.
      case Contents =:= ?any of
	true -> ok;
	false ->
          %% XXX. See comment below.
          %% erlang:error({illegal_list, ?nonempty_list(Contents, Termination)})
          ok
      end,
      "nonempty_maybe_improper_list()";
    _ ->
      case t_is_subtype(t_nil(), Termination) of
	true ->
	  "nonempty_maybe_improper_list("++ContentString++","
	    ++t_to_string(Termination, RecDict)++")";
	false ->
	  "nonempty_improper_list("++ContentString++","
	    ++t_to_string(Termination, RecDict)++")"
      end
  end;
t_to_string(?list(Contents, Termination, ?unknown_qual), RecDict) ->
  ContentString = t_to_string(Contents, RecDict),
  case Termination of
    ?nil ->
      case Contents of
	?char -> "string()";
	_ -> "["++ContentString++"]"
      end;
    ?any ->
      %% Just a safety check.
      %% XXX. Types such as "maybe_improper_list(integer(), any())"
      %% are OK, but cannot be printed!?
      case Contents =:= ?any of
	true -> ok;
	false ->
          ok
          %% L = ?list(Contents, Termination, ?unknown_qual),
          %% erlang:error({illegal_list, L})
      end,
      "maybe_improper_list()";
    _ ->
      case t_is_subtype(t_nil(), Termination) of
	true ->
	  "maybe_improper_list("++ContentString++","
	    ++t_to_string(Termination, RecDict)++")";
	false ->
	  "maybe_improper_list("++ContentString++","
	    ++t_to_string(Termination, RecDict)++")"
      end
  end;
t_to_string(?int_set(Set), _RecDict) ->
  set_to_string(Set);
t_to_string(?byte, _RecDict) -> "byte()";
t_to_string(?char, _RecDict) -> "char()";
t_to_string(?integer_pos, _RecDict) -> "pos_integer()";
t_to_string(?integer_non_neg, _RecDict) -> "non_neg_integer()";
t_to_string(?integer_neg, _RecDict) -> "neg_integer()";
t_to_string(?int_range(From, To), _RecDict) ->
  flat_format("~w..~w", [From, To]);
t_to_string(?integer(?any), _RecDict) -> "integer()";
t_to_string(?float, _RecDict) -> "float()";
t_to_string(?nominal({Module, Name, Arity, _}, ?opaque), _RecDict) ->
  Modname = flat_format("~w:~tw", [Module, Name]),
  Args = lists:join($,, lists:duplicate(Arity, $_)),
  flat_format("~ts(~ts)", [Modname, Args]);
t_to_string(?nominal({_Module, _Name, _Arity, opaque}, _) = N, _RecDict) -> 
  t_to_string(oc_mark(N, ?opaque, "erl_types"));
t_to_string(?nominal({Module, Name, Arity, _}, Structure), RecDict) ->
  Modname = flat_format("~w:~tw", [Module, Name]),
  Args = lists:join($,, lists:duplicate(Arity, $_)),
  Namearity = flat_format("~ts(~ts)", [Modname, Args]),
  StructureString = t_to_string(Structure, RecDict),
  flat_format("(~ts :: ~ts)", [Namearity, StructureString]);
t_to_string(?nominal_set(T, S), RecDict) ->
  union_sequence([N || N <- [S|T], N =/= ?none], RecDict);
t_to_string(?number(?any, ?unknown_qual), _RecDict) -> "number()";
t_to_string(?product(List), RecDict) ->
  "<" ++ comma_sequence(List, RecDict) ++ ">";
t_to_string(?map([],?any,?any), _RecDict) -> "map()";
t_to_string(?map(Pairs0,DefK,DefV), RecDict) ->
  {Pairs, ExtraEl} =
    case {DefK, DefV} of
      {?none, ?none} -> {Pairs0, []};
      _ -> {Pairs0 ++ [{DefK,?opt,DefV}], []}
    end,
  Tos = fun(T) -> case T of
		    ?any -> "_";
		    _ -> t_to_string(T, RecDict)
		  end end,
  StrMand = [{Tos(K),Tos(V)}||{K,?mand,V}<-Pairs],
  StrOpt  = [{Tos(K),Tos(V)}||{K,?opt,V}<-Pairs],
  "#{" ++ flat_join([K ++ ":=" ++ V||{K,V}<-StrMand]
                    ++ [K ++ "=>" ++ V||{K,V}<-StrOpt]
                    ++ ExtraEl, ", ") ++ "}";
t_to_string(?tuple(?any, ?any, ?any), _RecDict) -> "tuple()";
t_to_string(?tuple(Elements, _Arity, ?any), RecDict) ->
  "{" ++ comma_sequence(Elements, RecDict) ++ "}";
t_to_string(?tuple(Elements, Arity, Tag), RecDict) ->
  [TagAtom] = atom_vals(Tag),
  case lookup_record(TagAtom, Arity-1, RecDict) of
    error -> "{" ++ comma_sequence(Elements, RecDict) ++ "}";
    {ok, FieldNames} ->
      record_to_string(TagAtom, Elements, FieldNames, RecDict)
  end;
t_to_string(?tuple_set(_) = T, RecDict) ->
  union_sequence(t_tuple_subtypes(T), RecDict);
t_to_string(?union(Types), RecDict) ->
  union_sequence([T || T <- Types, T =/= ?none], RecDict);
t_to_string(?var(Id), _RecDict) when is_atom(Id) ->
  flat_format("~s", [atom_to_list(Id)]);
t_to_string(?var(Id), _RecDict) when is_integer(Id) ->
  flat_format("var(~w)", [Id]).


record_to_string(Tag, [_|Fields], FieldNames, RecDict) ->
  FieldStrings = record_fields_to_string(Fields, FieldNames, RecDict, []),
  "#" ++ atom_to_string(Tag) ++ "{" ++ flat_join(FieldStrings, ",") ++ "}".

record_fields_to_string([F|Fs], [{FName, _Abstr, DefType}|FDefs],
                        RecDict, Acc) ->
  NewAcc =
    case
      t_is_equal(F, t_any()) orelse
      (t_is_any_atom('undefined', F) andalso
       not t_is_none(t_inf(F, DefType)))
    of
      true -> Acc;
      false ->
	StrFV = atom_to_string(FName) ++ "::" ++ t_to_string(F, RecDict),
	[StrFV|Acc]
    end,
  record_fields_to_string(Fs, FDefs, RecDict, NewAcc);
record_fields_to_string([], [], _RecDict, Acc) ->
  lists:reverse(Acc).

-spec record_field_diffs_to_string(erl_type(), type_table()) ->
                                      {[FieldPos :: pos_integer()], string()}.

record_field_diffs_to_string(?tuple([_|Fs], Arity, Tag), RecDict) ->
  [TagAtom] = atom_vals(Tag),
  {ok, FieldNames} = lookup_record(TagAtom, Arity-1, RecDict),
  %% io:format("RecCElems = ~p\nRecTypes = ~p\n", [Fs, FieldNames]),
  Diffs = field_diffs(Fs, FieldNames, 1, RecDict, []),
  {FNs, FieldDiffs} = lists:unzip(Diffs),
  {FNs, flat_join(FieldDiffs, " and ")}.

field_diffs([F|Fs], [{FName, _Abstr, DefType}|FDefs], Pos, RecDict, Acc) ->
  %% Don't care about opacity for now.
  NewAcc =
    case not t_is_none(t_inf(F, DefType)) of
      true -> Acc;
      false ->
	Str = atom_to_string(FName) ++ "::" ++ t_to_string(DefType, RecDict),
	[{Pos,Str}|Acc]
    end,
  field_diffs(Fs, FDefs, Pos + 1, RecDict, NewAcc);
field_diffs([], [], _, _, Acc) ->
  lists:reverse(Acc).

comma_sequence(Types, RecDict) ->
  List = [case T =:= ?any of
	    true -> "_";
	    false -> t_to_string(T, RecDict)
	  end || T <- Types],
  flat_join(List, ",").

union_sequence(Types, RecDict) ->
  List = [t_to_string(T, RecDict) || T <- Types],
  flat_join(List, " | ").

%%=============================================================================
%%
%% Build a type from parse forms.
%%
%%=============================================================================

-type type_names() :: [type_key() | record_key()].

-type mta()   :: {module(), atom(), arity()}.
-type mra()   :: {module(), atom(), arity()}.
-type site()  :: {'type', mta(), file:filename()}
               | {'spec', mfa(), file:filename()}
               | {'record', mra(), file:filename()}
               | {'check', mta(), file:filename()}.
-type cache_key() :: {module(), atom(), expand_depth(),
                      [erl_type()], type_names()}.
-type mod_type_table() :: ets:tid().
-type mod_records() :: #{module() => type_table()}.
-type exported_type_table() :: ets:tid().
-record(cache,
        {
         types = #{} :: #{cache_key() => {erl_type(), expand_limit()}},
         mod_recs = #{} :: mod_records()
        }).

-opaque cache() :: #cache{}.

-spec t_from_form(parse_form(), exported_type_table(), site(), mod_type_table(),
                  var_table(), cache()) -> {erl_type(), cache()}.

t_from_form(Form, ExpTypes, Site, RecDict, VarTab, Cache) ->
  t_from_form1(Form, ExpTypes, Site, RecDict, VarTab, Cache).

%% Replace external types with with none().
-spec t_from_form_without_remote(parse_form(), site(), type_table()) ->
                                    erl_type().

t_from_form_without_remote(Form, Site, TypeTable) ->
  Module = site_module(Site),
  ModRecs = #{Module => TypeTable},
  ExpTypes = replace_by_none,
  VarTab = var_table__new(),
  Cache0 = cache__new(),
  Cache = Cache0#cache{mod_recs = ModRecs},
  {Type, _} = t_from_form1(Form, ExpTypes, Site, undefined, VarTab, Cache),
  Type.

-type expand_limit() :: integer().

-type expand_depth() :: integer().

-record(from_form, {site   :: site(),
                    xtypes :: exported_type_table() | 'replace_by_none',
                    mrecs  :: 'undefined' | mod_type_table(),
                    vtab   :: var_table(),
                    tnames :: type_names()}).

-spec t_from_form_check_remote(parse_form(), exported_type_table(), site(),
                               mod_type_table()) -> 'ok'.
t_from_form_check_remote(Form, ExpTypes, Site, RecDict) ->
  State = #from_form{site   = Site,
                     xtypes = ExpTypes,
                     mrecs  = RecDict,
                     vtab   = var_table__new(),
                     tnames = []},
  D = (1 bsl 25), % unlimited
  L = (1 bsl 25),
  Cache0 = cache__new(),
  _ = t_from_form2(Form, State, D, L, Cache0),
  ok.

%% REC_TYPE_LIMIT is used for limiting the depth of recursive types.
%% EXPAND_LIMIT is used for limiting the size of types by
%% limiting the number of elements of lists within one type form.
%% EXPAND_DEPTH is used in conjunction with EXPAND_LIMIT to make the
%% types balanced (unions will otherwise collapse to any()) by limiting
%% the depth the same way as t_limit/2 does.

-spec t_from_form1(parse_form(), exported_type_table() | 'replace_by_none',
                   site(), 'undefined' | mod_type_table(), var_table(),
                   cache()) -> {erl_type(), cache()}.

t_from_form1(Form, ET, Site, MR, V, C) ->
  TypeNames = initial_typenames(Site),
  D = ?EXPAND_DEPTH,
  L = ?EXPAND_LIMIT,
  State = #from_form{site   = Site,
                     xtypes = ET,
                     mrecs  = MR,
                     vtab   = V,
                     tnames = TypeNames},
  t_from_form2(Form, State, D, L, C).

t_from_form2(Form, State, D, L, C) ->
  {T0, L0, C0} = from_form(Form, State, D, L, C),
  if
    L0 =< 0 ->
      {T1, _, C1} = from_form(Form, State, 1, L, C0),
      from_form_loop(Form, State, 2, L, C1, T1);
    true ->
       {T0, C0}
  end.

initial_typenames({type, MTA, _File}) -> [{type, MTA}];
initial_typenames({spec, _MFA, _File}) -> [];
initial_typenames({record, _MRA, _File}) -> [].

%% 4 is the maximal depth used by any Dialyzer module
%% (5 is used internally).
-define(TYPE_LIMIT, 4).

from_form_loop(Form, State, D, Limit, C, T0) ->
  {T1, L1, C1} = from_form(Form, State, D, Limit, C),
  Delta = Limit - L1,
  if
    L1 =< 0 ->
      {T0, C1};
    Delta * 8 > Limit ->
      %% Save some time by assuming next depth will exceed the limit.
      {T1, C1};
    D =:= ?TYPE_LIMIT ->
      %% No need to go deeper than necessary.
      {T1, C1};
    true ->
      D1 = D + 1,
      from_form_loop(Form, State, D1, Limit, C1, T1)
  end.

-spec from_form(parse_form(),
                #from_form{},
                expand_depth(),
                expand_limit(),
                cache()) -> {erl_type(), expand_limit(), cache()}.

%% If there is something wrong with parse_form()
%% throw({error, io_lib:chars()} is called;
%% for unknown remote types
%% self() ! {self(), ext_types, {{RemMod, Name, ArgsLen}, Location}}
%% is called, unless 'replace_by_none' is given.
%%
%% It is assumed that site_module(S) can be found in MR.

from_form(_, _S, D, L, _C) when not is_integer(D); not is_integer(L) ->
  error(badarg);
from_form(_, _S, D, L, C) when D =< 0 ; L =< 0 ->
  {t_any(), L, C};
from_form({var, _Anno, '_'}, _S, _D, L, C) ->
  {t_any(), L, C};
from_form({var, _Anno, Name}, S, _D, L, C) ->
  case S#from_form.vtab of
    #{Name := Val} ->
       {Val, L, C};
    #{} ->
      {t_var(Name), L, C}
  end;
from_form({ann_type, _Anno, [_Var, Type]}, S, D, L, C) ->
  from_form(Type, S, D, L, C);
from_form({paren_type, _Anno, [Type]}, S, D, L, C) ->
  from_form(Type, S, D, L, C);
from_form({remote_type, Anno, [{atom, _, Module}, {atom, _, Type}, Args]},
	    S, D, L, C) ->
  remote_from_form(Anno, Module, Type, Args, S, D, L, C);
from_form({atom, _Anno, Atom}, _S, _D, L, C) ->
  {t_atom(Atom), L, C};
from_form({integer, _Anno, Int}, _S, _D, L, C) ->
  {t_integer(Int), L, C};
from_form({char, _Anno, Char}, _S, _D, L, C) ->
  {t_integer(Char), L, C};
from_form({op, _Anno, _Op, _Arg} = Op, _S, _D, L, C) ->
  case erl_eval:partial_eval(Op) of
    {integer, _, Val} ->
      {t_integer(Val), L, C};
    _ -> throw({error, io_lib:format("Unable to evaluate type ~w\n", [Op])})
  end;
from_form({op, _Anno, _Op, _Arg1, _Arg2} = Op, _S, _D, L, C) ->
  case erl_eval:partial_eval(Op) of
    {integer, _, Val} ->
      {t_integer(Val), L, C};
    _ -> throw({error, io_lib:format("Unable to evaluate type ~w\n", [Op])})
  end;
from_form({type, _Anno, any, []}, _S, _D, L, C) ->
  {t_any(), L, C};
from_form({type, _Anno, arity, []}, _S, _D, L, C) ->
  {t_arity(), L, C};
from_form({type, _Anno, atom, []}, _S, _D, L, C) ->
  {t_atom(), L, C};
from_form({type, _Anno, binary, []}, _S, _D, L, C) ->
  {t_binary(), L, C};
from_form({type, _Anno, binary, [Base, Unit]} = Type, _S, _D, L, C) ->
  case {erl_eval:partial_eval(Base), erl_eval:partial_eval(Unit)} of
    {{integer, _, B}, {integer, _, U}} when B >= 0, U >= 0 ->
      {t_bitstr(U, B), L, C};
    _ -> throw({error, io_lib:format("Unable to evaluate type ~w\n", [Type])})
  end;
from_form({type, _Anno, bitstring, []}, _S, _D, L, C) ->
  {t_bitstr(), L, C};
from_form({type, _Anno, bool, []}, _S, _D, L, C) ->
  {t_boolean(), L, C};	% XXX: Temporarily
from_form({type, _Anno, boolean, []}, _S, _D, L, C) ->
  {t_boolean(), L, C};
from_form({type, _Anno, byte, []}, _S, _D, L, C) ->
  {t_byte(), L, C};
from_form({type, _Anno, char, []}, _S, _D, L, C) ->
  {t_char(), L, C};
from_form({type, _Anno, dynamic, []}, _S, _D, L, C) ->
  {t_any(), L, C};
from_form({type, _Anno, float, []}, _S, _D, L, C) ->
  {t_float(), L, C};
from_form({type, _Anno, function, []}, _S, _D, L, C) ->
  {t_fun(), L, C};
from_form({type, _Anno, 'fun', []}, _S, _D, L, C) ->
  {t_fun(), L, C};
from_form({type, _Anno, 'fun', [{type, _, any}, Range]}, S, D, L, C) ->
  {T, L1, C1} = from_form(Range, S, D - 1, L - 1, C),
  {t_fun(T), L1, C1};
from_form({type, _Anno, 'fun', [{type, _, product, Domain}, Range]},
          S, D, L, C) ->
  {Dom1, L1, C1} = list_from_form(Domain, S, D, L, C),
  {Ran1, L2, C2} = from_form(Range, S, D, L1, C1),
  {t_fun(Dom1, Ran1), L2, C2};
from_form({type, _Anno, identifier, []}, _S, _D, L, C) ->
  {t_identifier(), L, C};
from_form({type, _Anno, integer, []}, _S, _D, L, C) ->
  {t_integer(), L, C};
from_form({type, _Anno, iodata, []}, _S, _D, L, C) ->
  {t_iodata(), L, C};
from_form({type, _Anno, iolist, []}, _S, _D, L, C) ->
  {t_iolist(), L, C};
from_form({type, _Anno, list, []}, _S, _D, L, C) ->
  {t_list(), L, C};
from_form({type, _Anno, list, [Type]}, S, D, L, C) ->
  {T, L1, C1} = from_form(Type, S, D - 1, L - 1, C),
  {t_list(T), L1, C1};
from_form({type, _Anno, map, any}, S, D, L, C) ->
  builtin_type(map, t_map(), S, D, L, C);
from_form({type, _Anno, map, List}, S, D0, L, C) ->
  {Pairs1, L5, C5} =
    fun PairsFromForm(_, L1, C1) when L1 =< 0 -> {[{?any,?opt,?any}], L1, C1};
	PairsFromForm([], L1, C1) -> {[], L1, C1};
	PairsFromForm([{type, _, Oper, [KF, VF]}|T], L1, C1) ->
        D = D0 - 1,
	{Key, L2, C2} = from_form(KF, S, D, L1, C1),
	{Val, L3, C3} = from_form(VF, S, D, L2, C2),
	{Pairs0, L4, C4} = PairsFromForm(T, L3 - 1, C3),
	case Oper of
	  map_field_assoc -> {[{Key,?opt, Val}|Pairs0], L4, C4};
	  map_field_exact -> {[{Key,?mand,Val}|Pairs0], L4, C4}
	end
    end(List, L, C),
  try
    Pairs2 = singleton_elements(Pairs1),
    {Pairs, DefK, DefV} = map_from_form(Pairs2, [], [], [], ?none, ?none),
    {t_map(Pairs, DefK, DefV), L5, C5}
  catch none -> {t_none(), L5, C5}
  end;
from_form({type, _Anno, mfa, []}, _S, _D, L, C) ->
  {t_mfa(), L, C};
from_form({type, _Anno, module, []}, _S, _D, L, C) ->
  {t_module(), L, C};
from_form({type, _Anno, nil, []}, _S, _D, L, C) ->
  {t_nil(), L, C};
from_form({type, _Anno, neg_integer, []}, _S, _D, L, C) ->
  {t_neg_integer(), L, C};
from_form({type, _Anno, non_neg_integer, []}, _S, _D, L, C) ->
  {t_non_neg_integer(), L, C};
from_form({type, _Anno, no_return, []}, _S, _D, L, C) ->
  {t_unit(), L, C};
from_form({type, _Anno, node, []}, _S, _D, L, C) ->
  {t_node(), L, C};
from_form({type, _Anno, none, []}, _S, _D, L, C) ->
  {t_none(), L, C};
from_form({type, _Anno, nonempty_binary, []}, S, D, L, C) ->
  try type_from_form(nonempty_binary, [], S, D, L, C)
  catch _:_ -> {t_nonempty_binary(), L, C}
  end;
from_form({type, _Anno, nonempty_bitstring, []}, S, D, L, C) ->
  try type_from_form(nonempty_binary, [], S, D, L, C)
  catch _:_ -> {t_nonempty_bitstring(), L, C}
  end;
from_form({type, _Anno, nonempty_list, []}, _S, _D, L, C) ->
  {t_nonempty_list(), L, C};
from_form({type, _Anno, nonempty_list, [Type]}, S, D, L, C) ->
  {T, L1, C1} = from_form(Type, S, D, L - 1, C),
  {t_nonempty_list(T), L1, C1};
from_form({type, _Anno, nonempty_improper_list, [Cont, Term]}, S, D, L, C) ->
  {T1, L1, C1} = from_form(Cont, S, D, L - 1, C),
  {T2, L2, C2} = from_form(Term, S, D, L1, C1),
  {t_cons(T1, T2), L2, C2};
from_form({type, _Anno, nonempty_maybe_improper_list, []}, _S, _D, L, C) ->
  {t_cons(?any, ?any), L, C};
from_form({type, _Anno, nonempty_maybe_improper_list, [Cont, Term]},
          S, D, L, C) ->
  {T1, L1, C1} = from_form(Cont, S, D, L - 1, C),
  {T2, L2, C2} = from_form(Term, S, D, L1, C1),
  {t_cons(T1, T2), L2, C2};
from_form({type, _Anno, nonempty_string, []}, _S, _D, L, C) ->
  {t_nonempty_string(), L, C};
from_form({type, _Anno, number, []}, _S, _D, L, C) ->
  {t_number(), L, C};
from_form({type, _Anno, pid, []}, _S, _D, L, C) ->
  {t_pid(), L, C};
from_form({type, _Anno, port, []}, _S, _D, L, C) ->
  {t_port(), L, C};
from_form({type, _Anno, pos_integer, []}, _S, _D, L, C) ->
  {t_pos_integer(), L, C};
from_form({type, _Anno, maybe_improper_list, []}, _S, _D, L, C) ->
  {t_maybe_improper_list(), L, C};
from_form({type, _Anno, maybe_improper_list, [Content, Termination]},
          S, D, L, C) ->
  {T1, L1, C1} = from_form(Content, S, D, L - 1, C),
  {T2, L2, C2} = from_form(Termination, S, D, L1, C1),
  {t_maybe_improper_list(T1, T2), L2, C2};
from_form({type, _Anno, product, Elements}, S, D, L, C) ->
  {Lst, L1, C1} = list_from_form(Elements, S, D - 1, L, C),
  {t_product(Lst), L1, C1};
from_form({type, _Anno, range, [From, To]} = Type, _S, _D, L, C) ->
  case {erl_eval:partial_eval(From), erl_eval:partial_eval(To)} of
    {{integer, _, FromVal}, {integer, _, ToVal}} ->
      {t_from_range(FromVal, ToVal), L, C};
    _ -> throw({error, io_lib:format("Unable to evaluate type ~w\n", [Type])})
  end;
from_form({type, _Anno, record, [Name|Fields]}, S, D, L, C) ->
  record_from_form(Name, Fields, S, D, L, C);
from_form({type, _Anno, reference, []}, _S, _D, L, C) ->
  {t_reference(), L, C};
from_form({type, _Anno, string, []}, _S, _D, L, C) ->
  {t_string(), L, C};
from_form({type, _Anno, term, []}, _S, _D, L, C) ->
  {t_any(), L, C};
from_form({type, _Anno, timeout, []}, _S, _D, L, C) ->
  {t_timeout(), L, C};
from_form({type, _Anno, tuple, any}, _S, _D, L, C) ->
  {t_tuple(), L, C};
from_form({type, _Anno, tuple, Args}, S, D, L, C) ->
  {Lst, L1, C1} = list_from_form(Args, S, D - 1, L, C),
  {t_tuple(Lst), L1, C1};
from_form({type, _Anno, union, Args}, S, D, L, C) ->
  {Lst, L1, C1} = list_from_form(Args, S, D, L, C),
  {t_sup(Lst), L1, C1};
from_form({user_type, _Anno, Name, Args}, S, D, L, C) ->
  type_from_form(Name, Args, S, D, L, C).

builtin_type(Name, Type, S, D, L, C) ->
  #from_form{site = Site, mrecs = MR} = S,
  M = site_module(Site),
  case lookup_module_types(M, MR, C) of
    {R, C1} ->
      case lookup_type(Name, 0, R) of
        {_, {{_M, _FL, _F, _A}, _T}} ->
          type_from_form(Name, [], S, D, L, C1);
        error ->
          {Type, L, C1}
      end;
    error ->
      {Type, L, C}
  end.

type_from_form(Name, Args, S, D, L, C) ->
  #from_form{site = Site, mrecs = MR, tnames = TypeNames} = S,
  ArgsLen = length(Args),
  Module = site_module(Site),
  TypeName = {type, {Module, Name, ArgsLen}},
  case can_unfold_more(TypeName, TypeNames) of
    true ->
      {R, C1} = case lookup_module_types(Module, MR, C) of error -> error({Name, Args}); KK -> KK end,
      type_from_form1(Name, Args, ArgsLen, R, TypeName, TypeNames, Site,
                      S, D, L, C1);
    false ->
      {t_any(), L, C}
  end.

type_from_form1(Name, Args, ArgsLen, R, TypeName, TypeNames, Site,
                S, D, L, C) ->
  case lookup_type(Name, ArgsLen, R) of
    {_, {_, _}} when element(1, Site) =:= check ->
      {_ArgTypes, L1, C1} = list_from_form(Args, S, D, L, C),
      {t_any(), L1, C1};
    {Tag, {{Module, {File,_Location}, Form, ArgNames}, _Type}} ->
      NewTypeNames = [TypeName|TypeNames],
      S1 = S#from_form{tnames = NewTypeNames},
      {ArgTypes, L1, C1} = list_from_form(Args, S1, D, L, C),
      CKey = cache_key(Module, Name, ArgTypes, TypeNames, D),
      case cache_find(CKey, C) of
        {CachedType, DeltaL} ->
          {CachedType, L1 - DeltaL, C};
        error ->
          List = lists:zip(ArgNames, ArgTypes),
          TmpV = maps:from_list(List),
          {SiteTag, MFA} = TypeName,
          Site1 = {SiteTag, MFA, File},
          S2 = S1#from_form{site = Site1, vtab = TmpV},
          Fun = fun(DD, LL) -> from_form(Form, S2, DD, LL, C1) end,
          {NewType, L3, C3} =
            case Tag of
              nominal ->
                {Rep, L2, C2} = recur_limit(Fun, D, L1, TypeName, TypeNames),
                {t_nominal({Module, Name, ArgsLen, transparent}, Rep), L2, C2};
              opaque ->
                {Rep, L2, C2} = recur_limit(Fun, D, L1, TypeName, TypeNames),
                {t_nominal({Module, Name, ArgsLen, opaque}, Rep), L2, C2};
              type ->
                recur_limit(Fun, D, L1, TypeName, TypeNames)
            end,
          C4 = cache_put(CKey, NewType, L1 - L3, C3),
          {NewType, L3, C4}
      end;
    error ->
      Msg = io_lib:format("Unable to find type ~tw/~w\n",
                          [Name, ArgsLen]),
      throw({error, Msg})
  end.

remote_from_form(Anno, RemMod, Name, Args, S, D, L, C) ->
  #from_form{site = Site, xtypes = ET, mrecs = MR, tnames = TypeNames} = S,
  if
    ET =:= replace_by_none ->
      {t_none(), L, C};
    true ->
      ArgsLen = length(Args),
      MFA = {RemMod, Name, ArgsLen},
      case lookup_module_types(RemMod, MR, C) of
        error ->
          self() ! {self(), ext_types, ext_types_message(MFA, Anno, Site)},
          {t_any(), L, C};
        {RemDict, C1} ->
          case ets:member(ET, MFA) of
            true ->
              RemType = {type, MFA},
              case can_unfold_more(RemType, TypeNames) of
                true ->
                  remote_from_form1(RemMod, Name, Args, ArgsLen, RemDict,
                                    RemType, TypeNames, Site, S, D, L, C1);
                false ->
                  {t_any(), L, C1}
              end;
            false ->
              self() ! {self(), ext_types, ext_types_message(MFA, Anno, Site)},
              {t_any(), L, C1}
          end
      end
  end.

ext_types_message(MFA, Anno, Site) ->
  {MFA, {site_file(Site), erl_anno:location(Anno), site_mfa(Site)}}.

remote_from_form1(RemMod, Name, Args, ArgsLen, RemDict, RemType, TypeNames,
                  Site, S, D, L, C) ->
  case lookup_type(Name, ArgsLen, RemDict) of
    {_, {_, _}} when element(1, Site) =:= check ->
      {_ArgTypes, L1, C1} = list_from_form(Args, S, D, L, C),
      {t_any(), L1, C1};
    {Tag, {{Mod, {File,_Location}, Form, ArgNames}, _Type}} ->
      NewTypeNames = [RemType|TypeNames],
      S1 = S#from_form{tnames = NewTypeNames},
      {ArgTypes, L1, C1} = list_from_form(Args, S1, D, L, C),
      CKey = cache_key(RemMod, Name, ArgTypes, TypeNames, D),
      case cache_find(CKey, C) of
        {CachedType, DeltaL} ->
          {CachedType, L - DeltaL, C};
        error ->
          List = lists:zip(ArgNames, ArgTypes),
          TmpVarTab = maps:from_list(List),
          {SiteTag, MFA} = RemType,
          Site1 = {SiteTag, MFA, File},
          S2 = S1#from_form{site = Site1, vtab = TmpVarTab},
          Fun = fun(DD, LL) -> from_form(Form, S2, DD, LL, C1) end,
          {NewType, L3, C3} =
            case Tag of
              nominal ->
                {NewRep, L2, C2} = recur_limit(Fun, D, L1, RemType, TypeNames),
                {t_nominal({Mod, Name, ArgsLen, transparent}, NewRep), L2, C2};
              opaque ->
                {NewRep, L2, C2} = recur_limit(Fun, D, L1, RemType, TypeNames),
                {t_nominal({Mod, Name, ArgsLen, opaque}, NewRep), L2, C2};
              type ->
                recur_limit(Fun, D, L1, RemType, TypeNames)
            end,
          C4 = cache_put(CKey, NewType, L1 - L3, C3),
          {NewType, L3, C4}
      end;
    error ->
      Msg = io_lib:format("Unable to find remote type ~w:~tw()\n",
                          [RemMod, Name]),
      throw({error, Msg})
  end.



record_from_form({atom, _, Name}, ModFields, S, D0, L0, C) ->
  #from_form{site = Site, mrecs = MR, tnames = TypeNames} = S,
  RecordType = {record, Name},
  case can_unfold_more(RecordType, TypeNames) of
    true ->
      M = site_module(Site),
      {R, C1} = lookup_module_types(M, MR, C),
      case lookup_record(Name, R) of
        {ok, _} when element(1, Site) =:= check ->
          {t_any(), L0, C1};
        {ok, DeclFields} ->
          NewTypeNames = [RecordType|TypeNames],
          Site1 = {record, {M, Name, length(DeclFields)}, site_file(Site)},
          S1 = S#from_form{site = Site1, tnames = NewTypeNames},
          Fun = fun(D, L) ->
                    {GetModRec, L1, C2} =
                      get_mod_record(ModFields, DeclFields, S1, D, L, C1),
                    case GetModRec of
                      {error, FieldName} ->
                        throw({error,
                               io_lib:format("Illegal declaration of #~tw{~tw}\n",
                                             [Name, FieldName])});
                      {ok, NewFields} ->
                        S2 = S1#from_form{vtab = var_table__new()},
                        {NewFields1, L2, C3} =
                          fields_from_form(NewFields, S2, D, L1, C2),
                        Rec = t_tuple(
                                [t_atom(Name)|[Type
                                               || {_FieldName, Type} <- NewFields1]]),
                        {Rec, L2, C3}
                    end
                end,
          recur_limit(Fun, D0, L0, RecordType, TypeNames);
        error ->
          throw({error, io_lib:format("Unknown record #~tw{}\n", [Name])})
      end;
    false ->
       {t_any(), L0, C}
  end.

get_mod_record([], DeclFields, _S, _D, L, C) ->
  {{ok, DeclFields}, L, C};
get_mod_record(ModFields, DeclFields, S, D, L, C) ->
  DeclFieldsDict = lists:keysort(1, DeclFields),
  {ModFieldsDict, L1, C1} = build_field_dict(ModFields, S, D, L, C),
  case get_mod_record_types(DeclFieldsDict, ModFieldsDict, []) of
    {error, _FieldName} = Error -> {Error, L1, C1};
    {ok, FinalKeyDict} ->
      Fields = [lists:keyfind(FieldName, 1, FinalKeyDict)
             || {FieldName, _, _} <- DeclFields],
      {{ok, Fields}, L1, C1}
  end.

build_field_dict(FieldTypes, S, D, L, C) ->
  build_field_dict(FieldTypes, S, D, L, C, []).

build_field_dict([{type, _, field_type, [{atom, _, Name}, Type]}|Left],
		 S, D, L, C, Acc) ->
  {T, L1, C1} = from_form(Type, S, D, L - 1, C),
  NewAcc = [{Name, Type, T}|Acc],
  build_field_dict(Left, S, D, L1, C1, NewAcc);
build_field_dict([], _S, _D, L, C, Acc) ->
  {lists:keysort(1, Acc), L, C}.

get_mod_record_types([{FieldName, _Abstr, _DeclType}|Left1],
                     [{FieldName, TypeForm, ModType}|Left2],
                     Acc) ->
  get_mod_record_types(Left1, Left2, [{FieldName, TypeForm, ModType}|Acc]);
get_mod_record_types([{FieldName1, _Abstr, _DeclType} = DT|Left1],
                     [{FieldName2, _FormType, _ModType}|_] = List2,
                     Acc) when FieldName1 < FieldName2 ->
  get_mod_record_types(Left1, List2, [DT|Acc]);
get_mod_record_types(Left1, [], Acc) ->
  {ok, lists:keysort(1, Left1++Acc)};
get_mod_record_types(_, [{FieldName2, _FormType, _ModType}|_], _Acc) ->
  {error, FieldName2}.

%% It is important to create a limited version of the record type
%% since nested record types can otherwise easily result in huge
%% terms.
fields_from_form([], _S, _D, L, C) ->
  {[], L, C};
fields_from_form([{Name, Abstr, _Type}|Tail], S, D, L, C) ->
  {T, L1, C1} = from_form(Abstr, S, D, L, C),
  {F, L2, C2} = fields_from_form(Tail, S, D, L1, C1),
  {[{Name, T}|F], L2, C2}.

list_from_form([], _S, _D, L, C) ->
  {[], L, C};
list_from_form([H|Tail], S, D, L, C) ->
  {H1, L1, C1} = from_form(H, S, D, L - 1, C),
  {T1, L2, C2} = list_from_form(Tail, S, D, L1, C1),
  {[H1|T1], L2, C2}.

%% Separates singleton types in keys (see is_singleton_type/1).
singleton_elements([]) ->
  [];
singleton_elements([{K,?mand,V}=Pair|Pairs]) ->
  case is_singleton_type(K) of
    true ->
      [Pair|singleton_elements(Pairs)];
    false ->
      singleton_elements([{K,?opt,V}|Pairs])
  end;
singleton_elements([{Key0,MNess,Val}|Pairs]) ->
  [{Key,MNess,Val} || Key <- separate_key(Key0)] ++ singleton_elements(Pairs).

%% To be in sync with is_singleton_type/1.
%% Does not separate tuples and maps as doing that has potential
%% to be very expensive.
separate_key(?atom(Atoms)) when Atoms =/= ?any ->
  [t_atom(A) || A <- Atoms];
separate_key(?number(_, _) = T) ->
  t_elements(T);
separate_key(?union(List)) ->
  lists:append([separate_key(K) || K <- List, not t_is_none(K)]);
separate_key(Key) ->
  [Key].

%% Sorts, combines non-singleton pairs, and applies precedence and
%% mandatoriness rules.
map_from_form([], ShdwPs, MKs, Pairs, DefK, DefV) ->
  verify_possible(MKs, ShdwPs),
  {promote_to_mand(MKs, Pairs), DefK, DefV};
map_from_form([{SKey,MNess,Val}|SPairs], ShdwPs0, MKs0, Pairs0, DefK0, DefV0) ->
  Key = lists:foldl(fun({K,_},S)->t_subtract(S,K)end, SKey, ShdwPs0),
  ShdwPs = case Key of ?none -> ShdwPs0; _ -> [{Key,Val}|ShdwPs0] end,
  MKs = case MNess of ?mand -> [SKey|MKs0]; ?opt -> MKs0 end,
  if MNess =:= ?mand, SKey =:= ?none -> throw(none);
     true -> ok
  end,
  {Pairs, DefK, DefV} =
    case is_singleton_type(Key) of
      true ->
	MNess1 = case Val =:= ?none of true -> ?opt; false -> MNess end,
	{mapdict_insert({Key,MNess1,Val}, Pairs0), DefK0, DefV0};
      false ->
	case Key =:= ?none orelse Val =:= ?none of
	  true  -> {Pairs0, DefK0,             DefV0};
	  false -> {Pairs0, t_sup(DefK0, Key), t_sup(DefV0, Val)}
	end
    end,
  map_from_form(SPairs, ShdwPs, MKs, Pairs, DefK, DefV).

%% Verifies that all mandatory keys are possible, throws 'none' otherwise
verify_possible(MKs, ShdwPs) ->
  lists:foreach(fun(M) -> verify_possible_1(M, ShdwPs) end, MKs).

verify_possible_1(M, ShdwPs) ->
  case lists:any(fun({K,_}) -> t_inf(M, K) =/= ?none end, ShdwPs) of
    true -> ok;
    false -> throw(none)
  end.

-spec promote_to_mand([erl_type()], t_map_dict()) -> t_map_dict().

promote_to_mand(_, []) -> [];
promote_to_mand(MKs, [E={K,_,V}|T]) ->
  [case lists:any(fun(M) -> t_is_equal(K,M) end, MKs) of
     true -> {K, ?mand, V};
     false -> E
   end|promote_to_mand(MKs, T)].

-define(RECUR_EXPAND_LIMIT, 10).
-define(RECUR_EXPAND_DEPTH, 2).

%% If more of the limited resources is spent on the non-recursive
%% forms, more warnings are found. And the analysis is also a bit
%% faster.
%%
%% Setting REC_TYPE_LIMIT to 1 would work also work well.

recur_limit(Fun, D, L, _, _) when L =< ?RECUR_EXPAND_DEPTH,
                                  D =< ?RECUR_EXPAND_LIMIT ->
  Fun(D, L);
recur_limit(Fun, D, L, TypeName, TypeNames) ->
  case is_recursive(TypeName, TypeNames) of
    true ->
      {T, L1, C1} = Fun(?RECUR_EXPAND_DEPTH, ?RECUR_EXPAND_LIMIT),
      {T, L - L1, C1};
    false ->
      Fun(D, L)
  end.

-spec t_check_record_fields(parse_form(), exported_type_table(), site(),
                            mod_type_table(), var_table(), cache()) -> cache().

t_check_record_fields(Form, ExpTypes, Site, RecDict, VarTable, Cache) ->
  State = #from_form{site   = Site,
                     xtypes = ExpTypes,
                     mrecs  = RecDict,
                     vtab   = VarTable,
                     tnames = []},
  check_record_fields(Form, State, Cache).

-spec check_record_fields(parse_form(), #from_form{}, cache()) -> cache().

%% If there is something wrong with parse_form()
%% throw({error, io_lib:chars()} is called.

check_record_fields({var, _Anno, _}, _S, C) -> C;
check_record_fields({ann_type, _Anno, [_Var, Type]}, S, C) ->
  check_record_fields(Type, S, C);
check_record_fields({paren_type, _Anno, [Type]}, S, C) ->
  check_record_fields(Type, S, C);
check_record_fields({remote_type, _Anno, [{atom, _, _}, {atom, _, _}, Args]},
                    S, C) ->
  list_check_record_fields(Args, S, C);
check_record_fields({atom, _Anno, _}, _S, C) -> C;
check_record_fields({integer, _Anno, _}, _S, C) -> C;
check_record_fields({char, _Anno, _}, _S, C) -> C;
check_record_fields({op, _Anno, _Op, _Arg}, _S, C) -> C;
check_record_fields({op, _Anno, _Op, _Arg1, _Arg2}, _S, C) -> C;
check_record_fields({type, _Anno, tuple, any}, _S, C) -> C;
check_record_fields({type, _Anno, map, any}, _S, C) -> C;
check_record_fields({type, _Anno, binary, [_Base, _Unit]}, _S, C) -> C;
check_record_fields({type, _Anno, 'fun', [{type, _, any}, Range]}, S, C) ->
  check_record_fields(Range, S, C);
check_record_fields({type, _Anno, range, [_From, _To]}, _S, C) -> C;
check_record_fields({type, _Anno, record, [Name|Fields]}, S, C) ->
  check_record(Name, Fields, S, C);
check_record_fields({type, _Anno, _, Args}, S, C) ->
  list_check_record_fields(Args, S, C);
check_record_fields({user_type, _Anno, _Name, Args}, S, C) ->
  list_check_record_fields(Args, S, C).

check_record({atom, _, Name}, ModFields, S, C) ->
  #from_form{site = Site, mrecs = MR} = S,
  M = site_module(Site),
  {R, C1} = lookup_module_types(M, MR, C),
  {ok, DeclFields} = lookup_record(Name, R),
  case check_fields(Name, ModFields, DeclFields, S, C1) of
    {error, FieldName} ->
      throw({error, io_lib:format("Illegal declaration of #~tw{~tw}\n",
                                  [Name, FieldName])});
    C2 -> C2
  end.

check_fields(RecName, [{type, _, field_type, [{atom, _, Name}, Abstr]}|Left],
             DeclFields, S, C) ->
  #from_form{site = Site0, xtypes = ET, mrecs = MR, vtab = V} = S,
  M = site_module(Site0),
  Site = {record, {M, RecName, length(DeclFields)}, site_file(Site0)},
  {Type, C1} = t_from_form(Abstr, ET, Site, MR, V, C),
  {Name, _, DeclType} = lists:keyfind(Name, 1, DeclFields),
  TypeNoVars = subst_all_vars_to_any(Type),
  case t_is_impossible(t_inf(TypeNoVars, DeclType)) of
    true -> {error, Name};
    false -> check_fields(RecName, Left, DeclFields, S, C1)
  end;
check_fields(_RecName, [], _Decl, _S, C) ->
  C.

list_check_record_fields([], _S, C) ->
  C;
list_check_record_fields([H|Tail], S, C) ->
  C1 = check_record_fields(H, S, C),
  list_check_record_fields(Tail, S, C1).

site_module({_, {Module, _, _}, _}) ->
  Module.

site_mfa({_, {M, F, A}, _}) ->
  {M, F, A}.

site_file({_, _, File}) ->
  File.

-spec cache__new() -> cache().

cache__new() ->
  #cache{}.

-spec cache_key(module(), atom(), [erl_type()],
                type_names(), expand_depth()) -> cache_key().

%% If TypeNames is left out from the key, the cache is smaller, and
%% the form-to-type translation is faster. But it would be a shame if,
%% for example, any() is used, where a more complex type should be
%% used. There is also a slight risk of creating unnecessarily big
%% types.

cache_key(Module, Name, ArgTypes, TypeNames, D) ->
  {Module, Name, D, ArgTypes, TypeNames}.

-spec cache_find(cache_key(), cache()) ->
                    {erl_type(), expand_limit()} | 'error'.

cache_find(Key, #cache{types = Types}) ->
  case Types of
    #{Key := Value} -> Value;
    #{} -> error
  end.

-spec cache_put(cache_key(), erl_type(), expand_limit(), cache()) -> cache().

cache_put(_Key, _Type, DeltaL, Cache) when DeltaL < 0 ->
  %% The type is truncated; do not reuse it.
  Cache;
cache_put(Key, Type, DeltaL, #cache{types = Types} = Cache) ->
  NewTypes = Types#{Key => {Type, DeltaL}},
  Cache#cache{types = NewTypes}.

-spec t_var_names([parse_form()]) -> [atom()].

t_var_names([{var, _, Name}|L]) when Name =/= '_' ->
  [Name|t_var_names(L)];
t_var_names([]) ->
  [].

-spec t_form_to_string(parse_form()) -> string().

t_form_to_string({var, _Anno, '_'}) -> "_";
t_form_to_string({var, _Anno, Name}) -> atom_to_list(Name);
t_form_to_string({atom, _Anno, Atom}) ->
  io_lib:write_string(atom_to_list(Atom), $'); % To quote or not to quote... '
t_form_to_string({integer, _Anno, Int}) -> integer_to_list(Int);
t_form_to_string({char, _Anno, Char}) -> integer_to_list(Char);
t_form_to_string({op, _Anno, _Op, _Arg} = Op) ->
  case erl_eval:partial_eval(Op) of
    {integer, _, _} = Int -> t_form_to_string(Int);
    _ -> io_lib:format("Badly formed type ~w", [Op])
  end;
t_form_to_string({op, _Anno, _Op, _Arg1, _Arg2} = Op) ->
  case erl_eval:partial_eval(Op) of
    {integer, _, _} = Int -> t_form_to_string(Int);
    _ -> io_lib:format("Badly formed type ~w", [Op])
  end;
t_form_to_string({ann_type, _Anno, [Var, Type]}) ->
  t_form_to_string(Var) ++ "::" ++ t_form_to_string(Type);
t_form_to_string({paren_type, _Anno, [Type]}) ->
  flat_format("(~ts)", [t_form_to_string(Type)]);
t_form_to_string({remote_type, _Anno, [{atom, _, Mod}, {atom, _, Name}, Args]}) ->
  ArgString = "(" ++ flat_join(t_form_to_string_list(Args), ",") ++ ")",
  flat_format("~w:~tw", [Mod, Name]) ++ ArgString;
t_form_to_string({type, _Anno, arity, []}) -> "arity()";
t_form_to_string({type, _Anno, binary, []}) -> "binary()";
t_form_to_string({type, _Anno, binary, [Base, Unit]} = Type) ->
  case {erl_eval:partial_eval(Base), erl_eval:partial_eval(Unit)} of
    {{integer, _, B}, {integer, _, U}} ->
      %% the following mirrors the clauses of t_to_string/2
      case {U, B} of
	{0, 0} -> "<<>>";
	{8, 0} -> "binary()";
	{1, 0} -> "bitstring()";
	{0, B} -> flat_format("<<_:~w>>", [B]);
	{U, 0} -> flat_format("<<_:_*~w>>", [U]);
	{U, B} -> flat_format("<<_:~w,_:_*~w>>", [B, U])
      end;
    _ -> io_lib:format("Badly formed bitstr type ~w", [Type])
  end;
t_form_to_string({type, _Anno, bitstring, []}) -> "bitstring()";
t_form_to_string({type, _Anno, dynamic, []}) -> "dynamic()";
t_form_to_string({type, _Anno, 'fun', []}) -> "fun()";
t_form_to_string({type, _Anno, 'fun', [{type, _, any}, Range]}) ->
  "fun(...) -> " ++ t_form_to_string(Range);
t_form_to_string({type, _Anno, 'fun', [{type, _, product, Domain}, Range]}) ->
  "fun((" ++ flat_join(t_form_to_string_list(Domain), ",") ++ ") -> "
    ++ t_form_to_string(Range) ++ ")";
t_form_to_string({type, _Anno, iodata, []}) -> "iodata()";
t_form_to_string({type, _Anno, iolist, []}) -> "iolist()";
t_form_to_string({type, _Anno, list, [Type]}) ->
  "[" ++ t_form_to_string(Type) ++ "]";
t_form_to_string({type, _Anno, map, any}) -> "map()";
t_form_to_string({type, _Anno, map, Args}) ->
  "#{" ++ flat_join(t_form_to_string_list(Args), ",") ++ "}";
t_form_to_string({type, _Anno, map_field_assoc, [Key, Val]}) ->
  t_form_to_string(Key) ++ "=>" ++ t_form_to_string(Val);
t_form_to_string({type, _Anno, map_field_exact, [Key, Val]}) ->
  t_form_to_string(Key) ++ ":=" ++ t_form_to_string(Val);
t_form_to_string({type, _Anno, mfa, []}) -> "mfa()";
t_form_to_string({type, _Anno, module, []}) -> "module()";
t_form_to_string({type, _Anno, node, []}) -> "node()";
t_form_to_string({type, _Anno, nonempty_binary, []}) ->
  "nonempty_binary()";
t_form_to_string({type, _Anno, nonempty_bitstring, []}) ->
  "nonempty_bitstring()";
t_form_to_string({type, _Anno, nonempty_list, [Type]}) ->
  "[" ++ t_form_to_string(Type) ++ ",...]";
t_form_to_string({type, _Anno, nonempty_string, []}) -> "nonempty_string()";
t_form_to_string({type, _Anno, product, Elements}) ->
  "<" ++ flat_join(t_form_to_string_list(Elements), ",") ++ ">";
t_form_to_string({type, _Anno, range, [From, To]} = Type) ->
  case {erl_eval:partial_eval(From), erl_eval:partial_eval(To)} of
    {{integer, _, FromVal}, {integer, _, ToVal}} ->
      flat_format("~w..~w", [FromVal, ToVal]);
    _ -> flat_format("Badly formed type ~w",[Type])
  end;
t_form_to_string({type, _Anno, record, [{atom, _, Name}]}) ->
  flat_format("#~tw{}", [Name]);
t_form_to_string({type, _Anno, record, [{atom, _, Name}|Fields]}) ->
  FieldString = flat_join(t_form_to_string_list(Fields), ","),
  flat_format("#~tw{~ts}", [Name, FieldString]);
t_form_to_string({type, _Anno, field_type, [{atom, _, Name}, Type]}) ->
  flat_format("~tw::~ts", [Name, t_form_to_string(Type)]);
t_form_to_string({type, _Anno, term, []}) -> "term()";
t_form_to_string({type, _Anno, timeout, []}) -> "timeout()";
t_form_to_string({type, _Anno, tuple, any}) -> "tuple()";
t_form_to_string({type, _Anno, tuple, Args}) ->
  "{" ++ flat_join(t_form_to_string_list(Args), ",") ++ "}";
t_form_to_string({type, _Anno, union, Args}) ->
  flat_join(lists:map(fun(Arg) ->
                          case Arg of
                            {ann_type, _AL, _} ->
                              "(" ++ t_form_to_string(Arg) ++ ")";
                            _ ->
                              t_form_to_string(Arg)
                          end
                      end, Args),
            " | ");
t_form_to_string({type, _Anno, Name, []} = T) ->
   try
     M = mod,
     Site = {type, {M,Name,0}, ""},
     V = var_table__new(),
     C = cache__new(),
     State = #from_form{site   = Site,
                        xtypes = replace_by_none,
                        mrecs  = 'undefined',
                        vtab   = V,
                        tnames = []},
     {T1, _, _} = from_form(T, State, _Deep=1000, _ALot=1000000, C),
     t_to_string(T1)
  catch throw:{error, _} -> atom_to_string(Name) ++ "()"
  end;
t_form_to_string({user_type, _Anno, Name, List}) ->
  flat_format("~tw(~ts)",
              [Name, flat_join(t_form_to_string_list(List), ",")]);
t_form_to_string({type, Anno, Name, List}) ->
  %% Compatibility: modules compiled before Erlang/OTP 18.0.
  t_form_to_string({user_type, Anno, Name, List}).

t_form_to_string_list(List) ->
  t_form_to_string_list(List, []).

t_form_to_string_list([H|T], Acc) ->
  t_form_to_string_list(T, [t_form_to_string(H)|Acc]);
t_form_to_string_list([], Acc) ->
  lists:reverse(Acc).

-spec atom_to_string(atom()) -> string().

atom_to_string(Atom) ->
  flat_format("~tw", [Atom]).

%%=============================================================================
%%
%% Utilities
%%
%%=============================================================================

-spec any_none([erl_type()]) -> boolean().

any_none([?none|_Left]) -> true;
any_none([_|Left]) -> any_none(Left);
any_none([]) -> false.

-spec any_none_or_unit([erl_type()]) -> boolean().

any_none_or_unit([?none|_]) -> true;
any_none_or_unit([?unit|_]) -> true;
any_none_or_unit([_|Left]) -> any_none_or_unit(Left);
any_none_or_unit([]) -> false.

-spec is_erl_type(any()) -> boolean().

is_erl_type(?any) -> true;
is_erl_type(?none) -> true;
is_erl_type(?unit) -> true;
is_erl_type(#c{}) -> true;
is_erl_type(_) -> false.

-spec lookup_module_types(module(), mod_type_table(), cache()) ->
                             'error' | {type_table(), cache()}.

lookup_module_types(Module, CodeTable, Cache) ->
  #cache{mod_recs = MRecs} = Cache,
  case MRecs of
    #{Module := R} ->
      {R, Cache};
    #{} ->
      case ets:lookup_element(CodeTable, Module, 2, error) of
        error ->
          error;
        R ->
          NewMRecs = MRecs#{Module => R},
          {R, Cache#cache{mod_recs = NewMRecs}}
      end
  end.

-spec lookup_record(atom(), type_table()) ->
        'error' | {'ok', [{atom(), parse_form(), erl_type()}]}.

lookup_record(Tag, Table) when is_atom(Tag) ->
  Key = {record, Tag},
  case Table of
    #{Key := {_FileLocation, [{_Arity, Fields}]}} ->
      {ok, Fields};
    #{Key := {_FileLocation, List}} when is_list(List) ->
      %% This will have to do, since we do not know which record we
      %% are looking for.
      error;
    #{} ->
      error
  end.

-spec lookup_record(atom(), arity(), type_table()) ->
        'error' | {'ok', [{atom(), parse_form(), erl_type()}]}.

lookup_record(Tag, Arity, Table) when is_atom(Tag) ->
  Key = {record, Tag},
  case Table of
    #{Key := {_FileLocation, [{Arity, Fields}]}} ->
      {ok, Fields};
    #{Key := {_FileLocation, OrdDict}} ->
      orddict:find(Arity, OrdDict);
    #{} ->
      error
  end.

-spec lookup_type(_, _, _) -> {'type' | 'opaque' | 'nominal', type_value()} | 'error'.
lookup_type(Name, Arity, Table) ->
  case Table of
    #{{type, Name, Arity} := Found} ->
      {type, Found};
    #{{opaque, Name, Arity} := Found} ->
      {opaque, Found};
    #{{nominal, Name, Arity} := Found} ->
      {nominal, Found};
    #{} ->
      error
  end.

-spec type_is_defined('type' | 'opaque' | 'nominal', atom(), arity(), type_table()) ->
        boolean().

type_is_defined(TypeOrOpaque, Name, Arity, Table) ->
  maps:is_key({TypeOrOpaque, Name, Arity}, Table).


is_recursive(TypeName, TypeNames) ->
  lists:member(TypeName, TypeNames).

can_unfold_more(TypeName, TypeNames) ->
  Fun = fun(E, Acc) -> case E of TypeName -> Acc + 1; _ -> Acc end end,
  lists:foldl(Fun, 0, TypeNames) < ?REC_TYPE_LIMIT.

-spec structural(erl_type(), fun((_) -> T)) -> T.

%% Probably a little faster than calling t_structural/2.
%% Unions that are due to opaque types are unopaqued.
structural(?nominal_set([], S), Pred) ->
  structural(S, Pred);
structural(?nominal_set([?nominal(_, S1) | T], Str), Pred) ->
  structural(?nominal_set(T, t_sup(Str, S1)), Pred);
structural(?nominal(_, S), Pred) ->
  structural(S, Pred);
structural(Type, Pred) ->
  Pred(Type).

map_all_values(?map(Pairs,_,DefV)) ->
  [DefV | [V || {V, _, _} <- Pairs]].

%% Tests if a type has exactly one possible value.
-spec t_is_singleton(erl_type()) -> boolean().

t_is_singleton(Type) ->
  structural(Type, fun is_singleton_type/1).

%% To be in sync with separate_key/1.
%% Used to also recognize maps and tuples.
is_singleton_type(?nil) ->
  true;
is_singleton_type(?atom(?any)) ->
  false;
is_singleton_type(?atom([_])) ->
  true;
is_singleton_type(?int_range(V, V)) ->
  true; % cannot happen
is_singleton_type(?int_set([_])) ->
  true;
is_singleton_type(_) ->
  false.

%% -----------------------------------
%% Set
%%

set_singleton(Element) ->
  [Element].

set_is_singleton(Element, [Element]) ->
  true;
set_is_singleton(_, _) ->
  false.

set_is_element(Element, Set) ->
  ordsets:is_element(Element, Set).

set_union(?any, _) -> ?any;
set_union(_, ?any) -> ?any;
set_union(S1, S2)  ->
  case ordsets:union(S1, S2) of
    S when length(S) =< ?SET_LIMIT -> S;
    _ -> ?any
  end.

%% The intersection and subtraction can return ?none.
%% This should always be handled right away since ?none is not a valid set.
%% However, ?any is considered a valid set.

set_intersection(?any, S) -> S;
set_intersection(S, ?any) -> S;
set_intersection(S1, S2)  ->
  case ordsets:intersection(S1, S2) of
    [] -> ?none;
    S -> S
  end.

set_subtract(_, ?any) -> ?none;
set_subtract(?any, _) -> ?any;
set_subtract(S1, S2) ->
  case ordsets:subtract(S1, S2) of
    [] -> ?none;
    S -> S
  end.

set_from_list(List) ->
  case length(List) of
    L when L =< ?SET_LIMIT -> ordsets:from_list(List);
    L when L > ?SET_LIMIT -> ?any
  end.

set_filter(Pred, Set) ->
  case [E || E <- Set, Pred(E)] of
    [] -> ?none;
    NewSet -> NewSet
  end.

set_to_string(Set) ->
  L = [case is_atom(X) of
	 true -> io_lib:write_string(atom_to_list(X), $'); % stupid emacs '
	 false -> flat_format("~tw", [X])
       end || X <- Set],
  flat_join(L, " | ").

set_min([H|_]) -> H.

set_max(Set) ->
  lists:last(Set).

flat_format(F, S) ->
  lists:flatten(io_lib:format(F, S)).

flat_join(List, Sep) ->
  lists:flatten(lists:join(Sep, List)).

%%=============================================================================
%%
%% Utilities for the binary type
%%
%%=============================================================================

-spec gcd(integer(), integer()) -> integer().

gcd(A, B) when B > A ->
  gcd1(B, A);
gcd(A, B) ->
  gcd1(A, B).

-spec gcd1(integer(), integer()) -> integer().

gcd1(A, 0) -> A;
gcd1(A, B) ->
  case A rem B of
    0 -> B;
    X -> gcd1(B, X)
  end.

-spec bitstr_concat(erl_type(), erl_type()) -> erl_type().

bitstr_concat(?none, _) -> ?none;
bitstr_concat(_, ?none) -> ?none;
bitstr_concat(?bitstr(U1, B1), ?bitstr(U2, B2)) ->
  t_bitstr(gcd(U1, U2), B1+B2).

-spec bitstr_match(erl_type(), erl_type()) -> erl_type().

bitstr_match(?none, _) -> ?none;
bitstr_match(_, ?none) -> ?none;
bitstr_match(?bitstr(0, B1), ?bitstr(0, B2)) when B1 =< B2 ->
  t_bitstr(0, B2-B1);
bitstr_match(?bitstr(0, _B1), ?bitstr(0, _B2)) ->
  ?none;
bitstr_match(?bitstr(0, B1), ?bitstr(U2, B2)) when B1 =< B2 ->
  t_bitstr(U2, B2-B1);
bitstr_match(?bitstr(0, B1), ?bitstr(U2, B2))  ->
  t_bitstr(U2, handle_base(U2, B2-B1));
bitstr_match(?bitstr(_, B1), ?bitstr(0, B2)) when B1 > B2 ->
  ?none;
bitstr_match(?bitstr(U1, B1), ?bitstr(U2, B2)) ->
  GCD = gcd(U1, U2),
  t_bitstr(GCD, handle_base(GCD, B2-B1)).

-spec handle_base(integer(), integer()) -> integer().

handle_base(Unit, Pos) when Pos >= 0 ->
  Pos rem Unit;
handle_base(Unit, Neg) ->
  (Unit+(Neg rem Unit)) rem Unit.

%%=============================================================================
%%
%% Interface functions for abstract data types defined in this module
%%
%%=============================================================================

-spec var_table__new() -> var_table().

var_table__new() ->
  maps:new().

%%=============================================================================
%%
%% Utilities for finding a module's type dependencies
%%
%%=============================================================================


-spec module_type_deps_of_type_defs(type_table()) -> [module()].

module_type_deps_of_type_defs(TypeTable) ->
  ModuleTypeDependencies =
    [module_type_deps_of_entry(TypeTableEntry)
      || TypeTableEntry <- maps:to_list(TypeTable)],
  lists:append(ModuleTypeDependencies).

-spec module_type_deps_of_entry(
  {type_key(), type_value()}
  | {record_key(), record_value()}) -> [module()].

module_type_deps_of_entry({{'type', _TypeName, _A}, {{_FromM, _FileLine, AbstractType, _ArgNames}, _}}) ->
  type_form_to_remote_modules(AbstractType);

module_type_deps_of_entry({{'nominal', _TypeName, _A}, {{_FromM, _FileLine, AbstractType, _ArgNames}, _}}) ->
  type_form_to_remote_modules(AbstractType);

module_type_deps_of_entry({{'opaque', _TypeName, _A}, {{_FromM, _FileLine, AbstractType, _ArgNames}, _}}) ->
  type_form_to_remote_modules(AbstractType);

module_type_deps_of_entry({{'record', _Name}, {_FileLine, SizesAndFields}}) ->
  AllFields = lists:append([Fields || {_Size, Fields} <- SizesAndFields]),
  FieldTypes = [AbstractType || {_, AbstractType, _} <- AllFields],
  type_form_to_remote_modules(FieldTypes).

%% Whilst this function is depth-limited, it should be limited in precisely
%% the same way as Dialyzer's other analyses - i.e. it should only ignore
%% sub-components of types Diaylzer wouldn't explore anyway
-spec type_form_to_remote_modules(parse_form() | [parse_form()]) -> [module()].

type_form_to_remote_modules([]) ->
  [];

type_form_to_remote_modules([_|_] = Forms) ->
  D = ?EXPAND_DEPTH,
  L = ?EXPAND_LIMIT,
  {_, Mods} = list_get_modules_mentioned(Forms, D, L, []),
  lists:usort(Mods);

type_form_to_remote_modules(Form) ->
  D = ?EXPAND_DEPTH,
  L = ?EXPAND_LIMIT,
  {_, Mods} = get_modules_mentioned(Form, D, L, []),
  lists:usort(Mods).

-spec get_modules_mentioned(TypeForm :: parse_form(), expand_depth(), expand_limit(), Acc :: [module()]) -> {expand_depth(), [module()]}.

get_modules_mentioned(_, D, L, Acc) when D =< 0 ; L =< 0 ->
  {L, Acc};
get_modules_mentioned({var, _L, '_'}, _D, L, Acc) ->
  {L, Acc};
get_modules_mentioned({var, _L, _Name}, _D, L, Acc) ->
  {L, Acc};
get_modules_mentioned({ann_type, _L, [_Var, Type]}, D, L, Acc) ->
  get_modules_mentioned(Type, D, L, Acc);
get_modules_mentioned({paren_type, _L, [Type]}, D, L, Acc) ->
  get_modules_mentioned(Type, D, L, Acc);
get_modules_mentioned({atom, _L, _Atom}, _D, L, Acc) ->
  {L, Acc};
get_modules_mentioned({integer, _L, _Int}, _D, L, Acc) ->
  {L, Acc};
get_modules_mentioned({char, _L, _Char}, _D, L, Acc) ->
  {L, Acc};
get_modules_mentioned({op, _L, _Op, _Arg}, _D, L, Acc) ->
  {L, Acc};
get_modules_mentioned({op, _L, _Op, _Arg1, _Arg2}, _D, L, Acc) ->
  {L, Acc};
get_modules_mentioned({type, _L, 'fun', [{type, _, any}, Range]}, D, L, Acc) ->
  get_modules_mentioned(Range, D - 1, L - 1, Acc);
get_modules_mentioned({type, _L, 'fun', [{type, _, product, Domain}, Range]}, D, L, Acc) ->
  {L1, Acc1} = list_get_modules_mentioned(Domain, D, L, Acc),
  get_modules_mentioned(Range, D, L1, Acc1);
get_modules_mentioned({type, _L, list, [Type]}, D, L, Acc) ->
  get_modules_mentioned(Type, D - 1, L - 1, Acc);
get_modules_mentioned({type, _L, map, any}, _D, L, Acc) ->
  {L, Acc};
get_modules_mentioned({type, _L, map, List}, D0, L, Acc) ->
  fun PairsFromForm(_, L1, Acc1) when L1 =< 0 -> Acc1;
      PairsFromForm([], L1, Acc1) -> {L1, Acc1};
      PairsFromForm([{type, _, _Oper, [KF, VF]}|T], L1, Acc1) ->
        D = D0 - 1,
        {L2, Acc2} = get_modules_mentioned(KF, D, L1, Acc1),
        {L3, Acc3} = get_modules_mentioned(VF, D, L2, Acc2),
        PairsFromForm(T, L3 - 1, Acc3)
  end(List, L, Acc);
get_modules_mentioned({type, _L, nonempty_list, [Type]}, D, L, Acc) ->
  get_modules_mentioned(Type, D, L - 1, Acc);
get_modules_mentioned({type, _L, nonempty_improper_list, [Cont, Term]}, D, L, Acc) ->
  {L1, Acc1} = get_modules_mentioned(Cont, D, L - 1, Acc),
  get_modules_mentioned(Term, D, L1, Acc1);
get_modules_mentioned({type, _L, nonempty_maybe_improper_list, [Cont, Term]}, D, L, Acc) ->
  {L1, Acc1} = get_modules_mentioned(Cont, D, L - 1, Acc),
  get_modules_mentioned(Term, D, L1, Acc1);
get_modules_mentioned({type, _L, maybe_improper_list, [Content, Termination]}, D, L, Acc) ->
  {L1, Acc1} = get_modules_mentioned(Content, D, L - 1, Acc),
  get_modules_mentioned(Termination, D, L1, Acc1);
get_modules_mentioned({type, _L, product, Elements}, D, L, Acc) ->
  list_get_modules_mentioned(Elements, D - 1, L, Acc);
get_modules_mentioned({type, _L, range, [_From, _To]}, _D, L, Acc) ->
  {L, Acc};
get_modules_mentioned({type, _L, tuple, any}, _D, L, Acc) ->
  {L, Acc};
get_modules_mentioned({type, _L, tuple, Args}, D, L, Acc) ->
  list_get_modules_mentioned(Args, D - 1, L, Acc);
get_modules_mentioned({type, _L, union, Args}, D, L, Acc) ->
  list_get_modules_mentioned(Args, D, L, Acc);
get_modules_mentioned({remote_type, _L, [{atom, _, Module}, {atom, _, _Type}, Args]}, D, L, Acc) ->
  Acc1 = [Module|Acc],
  list_get_modules_mentioned(Args, D, L, Acc1);
get_modules_mentioned({user_type, _L, _Name, Args}, D, L, Acc) ->
  list_get_modules_mentioned(Args, D, L, Acc);
get_modules_mentioned({type, _L, _Name, []}, _D, L, Acc) ->
  {L, Acc};
get_modules_mentioned({type, _L, _Name, Args}, D, L, Acc) ->
  list_get_modules_mentioned(Args, D, L, Acc).

list_get_modules_mentioned([], _D, L, Acc) ->
  {L, Acc};
list_get_modules_mentioned([H|Tail], D, L, Acc) ->
  {L1, Acc1} = get_modules_mentioned(H, D, L - 1, Acc),
  list_get_modules_mentioned(Tail, D, L1, Acc1).
