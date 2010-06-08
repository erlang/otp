%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
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
%% ======================================================================
%% Copyright (C) 2000-2003 Richard Carlsson
%%
%% ======================================================================
%% Provides a representation of Erlang types.
%%
%% The initial author of this file is Richard Carlsson (2000-2004).
%% In July 2006, the type representation was totally re-designed by
%% Tobias Lindahl. This is the representation which is used currently.
%% In late 2008, Manouk Manoukian and Kostis Sagonas added support for
%% opaque types to the structure-based representation of types.
%% During February and March 2009, Kostis Sagonas significantly
%% cleaned up the type representation added spec declarations.
%%
%% Author contact: richardc@it.uu.se, tobiasl@it.uu.se, kostis@cs.ntua.gr
%% ======================================================================

-module(erl_types).

-export([any_none/1,
	 any_none_or_unit/1,
	 lookup_record/3,
	 max/2,
	 module_builtin_opaques/1,
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
	 t_boolean/0,
	 t_byte/0,
	 t_char/0,
	 t_collect_vars/1,
	 t_cons/0,
	 t_cons/2,
	 t_cons_hd/1,
	 t_cons_tl/1,
	 t_constant/0,
	 t_contains_opaque/1,
	 t_elements/1,
	 t_find_opaque_mismatch/2,
	 t_fixnum/0,
	 t_map/2,
	 t_non_neg_fixnum/0,
	 t_pos_fixnum/0,
	 t_float/0,
	 t_form_to_string/1,
	 t_from_form/1,
	 t_from_form/2,
	 t_from_form/3,
	 t_from_range/2,
	 t_from_range_unsafe/2,
	 t_from_term/1,
	 t_fun/0,
	 t_fun/1,
	 t_fun/2,
	 t_fun_args/1,
	 t_fun_arity/1,
	 t_fun_range/1,
	 t_has_opaque_subtype/1,
	 t_has_var/1,
	 t_identifier/0,
	 %% t_improper_list/2,
	 t_inf/2,
	 t_inf/3,
	 t_inf_lists/2,
	 t_inf_lists/3,
	 t_inf_lists_masked/3,
	 t_integer/0,
	 t_integer/1,
	 t_non_neg_integer/0,
	 t_pos_integer/0,
	 t_integers/1,
	 t_iodata/0,
	 t_iolist/0,
	 t_is_any/1,
	 t_is_atom/1,
	 t_is_atom/2,
	 t_is_binary/1,
	 t_is_bitstr/1,
	 t_is_bitwidth/1,
	 t_is_boolean/1,
	 %% t_is_byte/1,
	 %% t_is_char/1,
	 t_is_cons/1,
	 t_is_constant/1,
	 t_is_equal/2,
	 t_is_fixnum/1,
	 t_is_float/1,
	 t_is_fun/1,
	 t_is_instance/2,
	 t_is_integer/1,
	 t_is_list/1,
	 t_is_matchstate/1,
	 t_is_nil/1,
	 t_is_non_neg_integer/1,
	 t_is_none/1,
	 t_is_none_or_unit/1,
	 t_is_number/1,
	 t_is_opaque/1,
	 t_is_pid/1,
	 t_is_port/1,
	 t_is_maybe_improper_list/1,
	 t_is_reference/1,
	 t_is_remote/1,
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
	 t_matchstate/0,
	 t_matchstate/2,
	 t_matchstate_present/1,
	 t_matchstate_slot/2,
	 t_matchstate_slots/1,
	 t_matchstate_update_present/2,
	 t_matchstate_update_slot/3,
	 t_mfa/0,
	 t_module/0,
	 t_nil/0,	 
	 t_node/0,
	 t_none/0,
	 t_nonempty_list/0,
	 t_nonempty_list/1,
	 t_nonempty_string/0,
	 t_number/0,
	 t_number/1,
	 t_number_vals/1,
	 t_opaque_from_records/1,
	 t_opaque_match_atom/2,
	 t_opaque_match_record/2,
	 t_opaque_matching_structure/2,
	 t_opaque_structure/1,
	 %% t_parameterized_module/0,
	 t_pid/0,
	 t_port/0,
	 t_maybe_improper_list/0,
	 %% t_maybe_improper_list/2,
	 t_product/1,
	 t_reference/0,
	 t_remote/3,
	 t_string/0,
	 t_struct_from_opaque/2,
	 t_solve_remote/3,
	 t_subst/2,
	 t_subtract/2,
	 t_subtract_list/2,
	 t_sup/1,
	 t_sup/2,
	 t_tid/0,
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
	 t_unify/2,
	 t_unify/3,
	 t_unit/0,
	 t_unopaque/1,
	 t_unopaque/2,
	 t_unopaque_on_mismatch/3,
	 t_var/1,
	 t_var_name/1,
	 %% t_assign_variables_to_subtype/2,
	 type_is_defined/3,
	 subst_all_vars_to_any/1,
	 lift_list_to_pos_empty/1,
	 is_erl_type/1
	]).

%%-define(DO_ERL_TYPES_TEST, true).
-compile({no_auto_import,[min/2,max/2]}).

-ifdef(DO_ERL_TYPES_TEST).
-export([test/0]).
-else.
-define(NO_UNUSED, true).
-endif.

-ifndef(NO_UNUSED).
-export([t_is_identifier/1]).
-endif.

-export_type([erl_type/0]).

%%=============================================================================
%%
%% Definition of the type structure
%%
%%=============================================================================

%%-----------------------------------------------------------------------------
%% Limits
%%

-define(REC_TYPE_LIMIT, 2).

-define(TUPLE_TAG_LIMIT, 5).
-define(TUPLE_ARITY_LIMIT, 10).
-define(SET_LIMIT, 13).
-define(MAX_BYTE, 255).
-define(MAX_CHAR, 16#10ffff).

-define(WIDENING_LIMIT, 7).
-define(UNIT_MULTIPLIER, 8).

-define(TAG_IMMED1_SIZE, 4).
-define(BITS, (erlang:system_info(wordsize) * 8) - ?TAG_IMMED1_SIZE).

%%-----------------------------------------------------------------------------
%% Type tags and qualifiers
%%

-define(atom_tag,       atom).
-define(binary_tag,     binary).
-define(function_tag,   function).
-define(identifier_tag, identifier).
-define(list_tag,       list).
-define(matchstate_tag, matchstate).
-define(nil_tag,        nil).
-define(number_tag,     number).
-define(opaque_tag,     opaque).
-define(product_tag,    product).
-define(remote_tag,     remote).
-define(tuple_set_tag,  tuple_set).
-define(tuple_tag,      tuple).
-define(union_tag,      union).
-define(var_tag,        var).

-type tag()  :: ?atom_tag | ?binary_tag | ?function_tag | ?identifier_tag
              | ?list_tag | ?matchstate_tag | ?nil_tag | ?number_tag
              | ?opaque_tag | ?product_tag | ?tuple_tag | ?tuple_set_tag
              | ?union_tag | ?var_tag.

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

-define(any,  any).
-define(none, none).
-define(unit, unit).
%% Generic constructor - elements can be many things depending on the tag.
-record(c, {tag			      :: tag(),
	    elements  = []	      :: term(),
	    qualifier = ?unknown_qual :: qual()}).

-opaque erl_type() :: ?any | ?none | ?unit | #c{}.

%%-----------------------------------------------------------------------------
%% Auxiliary types and convenient macros
%%

-type parse_form() :: {atom(), _, _} | {atom(), _, _, _}. %% XXX: Temporarily
-type rng_elem()   :: 'pos_inf' | 'neg_inf' | integer().

-record(int_set, {set :: [integer()]}).
-record(int_rng, {from :: rng_elem(), to :: rng_elem()}).
-record(opaque,  {mod :: module(), name :: atom(),
		  args = [] :: [erl_type()], struct :: erl_type()}).
-record(remote,  {mod:: module(), name :: atom(), args = [] :: [erl_type()]}).

-define(atom(Set),                 #c{tag=?atom_tag, elements=Set}).
-define(bitstr(Unit, Base),        #c{tag=?binary_tag, elements=[Unit,Base]}).
-define(float,                     ?number(?any, ?float_qual)).
-define(function(Domain, Range),   #c{tag=?function_tag, 
				      elements=[Domain, Range]}).
-define(identifier(Types),         #c{tag=?identifier_tag, elements=Types}).
-define(integer(Types),            ?number(Types, ?integer_qual)).
-define(int_range(From, To),       ?integer(#int_rng{from=From, to=To})).
-define(int_set(Set),              ?integer(#int_set{set=Set})).
-define(list(Types, Term, Size),   #c{tag=?list_tag, elements=[Types,Term],
				      qualifier=Size}).
-define(nil,                       #c{tag=?nil_tag}).
-define(nonempty_list(Types, Term),?list(Types, Term, ?nonempty_qual)).
-define(number(Set, Qualifier),    #c{tag=?number_tag, elements=Set, 
				      qualifier=Qualifier}).
-define(opaque(Optypes),           #c{tag=?opaque_tag, elements=Optypes}).
-define(product(Types),            #c{tag=?product_tag, elements=Types}).
-define(remote(RemTypes),          #c{tag=?remote_tag, elements=RemTypes}).
-define(tuple(Types, Arity, Qual), #c{tag=?tuple_tag, elements=Types, 
				      qualifier={Arity, Qual}}).
-define(tuple_set(Tuples),         #c{tag=?tuple_set_tag, elements=Tuples}).
-define(var(Id),                   #c{tag=?var_tag, elements=Id}).

-define(matchstate(P, Slots),	   #c{tag=?matchstate_tag, elements=[P,Slots]}).
-define(any_matchstate,            ?matchstate(t_bitstr(), ?any)).

-define(byte,                      ?int_range(0, ?MAX_BYTE)).
-define(char,                      ?int_range(0, ?MAX_CHAR)).
-define(integer_pos,               ?int_range(1, pos_inf)).
-define(integer_non_neg,           ?int_range(0, pos_inf)).
-define(integer_neg,               ?int_range(neg_inf, -1)).

%%-----------------------------------------------------------------------------
%% Unions
%%

-define(union(List), #c{tag=?union_tag, elements=[_,_,_,_,_,_,_,_,_,_]=List}).

-define(atom_union(T),       ?union([T,?none,?none,?none,?none,?none,?none,?none,?none,?none])).
-define(bitstr_union(T),     ?union([?none,T,?none,?none,?none,?none,?none,?none,?none,?none])).
-define(function_union(T),   ?union([?none,?none,T,?none,?none,?none,?none,?none,?none,?none])).
-define(identifier_union(T), ?union([?none,?none,?none,T,?none,?none,?none,?none,?none,?none])).
-define(list_union(T),       ?union([?none,?none,?none,?none,T,?none,?none,?none,?none,?none])).
-define(number_union(T),     ?union([?none,?none,?none,?none,?none,T,?none,?none,?none,?none])).
-define(tuple_union(T),      ?union([?none,?none,?none,?none,?none,?none,T,?none,?none,?none])).
-define(matchstate_union(T), ?union([?none,?none,?none,?none,?none,?none,?none,T,?none,?none])).
-define(opaque_union(T),     ?union([?none,?none,?none,?none,?none,?none,?none,?none,T,?none])).
-define(remote_union(T),     ?union([?none,?none,?none,?none,?none,?none,?none,?none,?none,T])).
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

t_is_any(?any) -> true;
t_is_any(_) -> false.

-spec t_none() -> erl_type().

t_none() ->
  ?none.

-spec t_is_none(erl_type()) -> boolean().

t_is_none(?none) -> true;
t_is_none(_) -> false.

%%-----------------------------------------------------------------------------
%% Opaque types
%%

-spec t_opaque(module(), atom(), [_], erl_type()) -> erl_type().

t_opaque(Mod, Name, Args, Struct) ->
  O = #opaque{mod = Mod, name = Name, args = Args, struct = Struct},
  ?opaque(set_singleton(O)).

-spec t_is_opaque(erl_type()) -> boolean().

t_is_opaque(?opaque(_)) -> true;
t_is_opaque(_) -> false.

-spec t_has_opaque_subtype(erl_type()) -> boolean().

t_has_opaque_subtype(?union(Ts)) ->
  lists:any(fun t_is_opaque/1, Ts);
t_has_opaque_subtype(T) ->
  t_is_opaque(T).

-spec t_opaque_structure(erl_type()) -> erl_type().

t_opaque_structure(?opaque(Elements)) ->
  case ordsets:size(Elements) of
    1 ->
      [#opaque{struct = Struct}] = ordsets:to_list(Elements),
      Struct;
    _ -> throw({error, "Unexpected multiple opaque types"})
  end.

-spec t_opaque_module(erl_type()) -> module().

t_opaque_module(?opaque(Elements)) ->
  case ordsets:size(Elements) of
    1 ->
      [#opaque{mod = Module}] = ordsets:to_list(Elements),
      Module;
    _ -> throw({error, "Unexpected multiple opaque types"})
  end.

%% This only makes sense if we know that Type matches Opaque
-spec t_opaque_matching_structure(erl_type(), erl_type()) -> erl_type().

t_opaque_matching_structure(Type, Opaque) ->
  OpaqueStruct = t_opaque_structure(Opaque),
  case OpaqueStruct of
    ?union(L1) ->
      case Type of
	?union(_L2) -> OpaqueStruct;
	_OtherType -> t_opaque_matching_structure_list(Type, L1)
      end;
    ?tuple_set(_Set1) = TupleSet ->
      case Type of
	?tuple_set(_Set2) -> OpaqueStruct;
	_ -> t_opaque_matching_structure_list(Type, t_tuple_subtypes(TupleSet))
      end;
    _Other -> OpaqueStruct
  end.

t_opaque_matching_structure_list(Type, List) ->
  NewList = [t_inf(Element, Type) || Element <- List],
  Results = [NotNone || NotNone <- NewList, NotNone =/= ?none],
  case Results of
    [] -> ?none;
    [First|_] -> First
  end.

-spec t_contains_opaque(erl_type()) -> boolean().

t_contains_opaque(?any)                     -> false;
t_contains_opaque(?none)                    -> false;
t_contains_opaque(?unit)                    -> false;
t_contains_opaque(?atom(_Set))              -> false;
t_contains_opaque(?bitstr(_Unit, _Base))    -> false;
t_contains_opaque(?float)                   -> false;
t_contains_opaque(?function(Domain, Range)) ->
  t_contains_opaque(Domain) orelse t_contains_opaque(Range);
t_contains_opaque(?identifier(_Types))      -> false;
t_contains_opaque(?integer(_Types))         -> false;
t_contains_opaque(?int_range(_From, _To))   -> false;
t_contains_opaque(?int_set(_Set))           -> false;
t_contains_opaque(?list(Type, _, _))        -> t_contains_opaque(Type);
t_contains_opaque(?matchstate(_P, _Slots))  -> false;
t_contains_opaque(?nil)                     -> false;
t_contains_opaque(?number(_Set, _Tag))      -> false;
t_contains_opaque(?opaque(_))               -> true;
t_contains_opaque(?product(Types))          -> list_contains_opaque(Types);
t_contains_opaque(?tuple(?any, _, _))       -> false;
t_contains_opaque(?tuple(Types, _, _))      -> list_contains_opaque(Types);
t_contains_opaque(?tuple_set(_Set) = T)     ->
  list_contains_opaque(t_tuple_subtypes(T));
t_contains_opaque(?union(List))             -> list_contains_opaque(List);
t_contains_opaque(?var(_Id))                -> false.

-spec list_contains_opaque([erl_type()]) -> boolean().

list_contains_opaque(List) ->
  lists:any(fun t_contains_opaque/1, List).

%% t_find_opaque_mismatch/2 of two types should only be used if their
%% t_inf is t_none() due to some opaque type violation.
%%
%% The first argument of the function is the pattern and its second
%% argument the type we are matching against the pattern.

-spec t_find_opaque_mismatch(erl_type(), erl_type()) -> 'error' | {'ok', erl_type(), erl_type()}.

t_find_opaque_mismatch(T1, T2) ->
  t_find_opaque_mismatch(T1, T2, T2).

t_find_opaque_mismatch(?any, _Type, _TopType) -> error;
t_find_opaque_mismatch(?none, _Type, _TopType) -> error;
t_find_opaque_mismatch(?list(T1, _, _), ?list(T2, _, _), TopType) ->
  t_find_opaque_mismatch(T1, T2, TopType);
t_find_opaque_mismatch(_T1, ?opaque(_) = T2, TopType) -> {ok, TopType, T2};
t_find_opaque_mismatch(?product(T1), ?product(T2), TopType) ->
  t_find_opaque_mismatch_ordlists(T1, T2, TopType);
t_find_opaque_mismatch(?tuple(T1, Arity, _), ?tuple(T2, Arity, _), TopType) ->
  t_find_opaque_mismatch_ordlists(T1, T2, TopType);
t_find_opaque_mismatch(?tuple(_, _, _) = T1, ?tuple_set(_) = T2, TopType) ->
  Tuples1 = t_tuple_subtypes(T1),
  Tuples2 = t_tuple_subtypes(T2),
  t_find_opaque_mismatch_lists(Tuples1, Tuples2, TopType);
t_find_opaque_mismatch(T1, ?union(U2), TopType) ->
  t_find_opaque_mismatch_lists([T1], U2, TopType);
t_find_opaque_mismatch(_T1, _T2, _TopType) -> error.

t_find_opaque_mismatch_ordlists(L1, L2, TopType) ->
  List = lists:zipwith(fun(T1, T2) ->
			   t_find_opaque_mismatch(T1, T2, TopType)
		       end, L1, L2),
  t_find_opaque_mismatch_list(List).

t_find_opaque_mismatch_lists(L1, L2, _TopType) ->
  List = [t_find_opaque_mismatch(T1, T2, T2) || T1 <- L1, T2 <- L2],
  t_find_opaque_mismatch_list(List).

t_find_opaque_mismatch_list([]) -> error;
t_find_opaque_mismatch_list([H|T]) ->
  case H of
    {ok, _T1, _T2} -> H;
    error -> t_find_opaque_mismatch_list(T)
  end.

-spec t_opaque_from_records(dict()) -> [erl_type()].

t_opaque_from_records(RecDict) ->
  OpaqueRecDict =
    dict:filter(fun(Key, _Value) ->
		    case Key of
		      {opaque, _Name} -> true;
		      _  -> false
		    end
		end, RecDict),
  OpaqueTypeDict =
    dict:map(fun({opaque, Name}, {Module, Type, ArgNames}) ->
		 case ArgNames of
		   [] ->
		     t_opaque(Module, Name, [], t_from_form(Type, RecDict));
		   _ ->
		     throw({error,"Polymorphic opaque types not supported yet"})
		 end
	     end, OpaqueRecDict),
  [OpaqueType || {_Key, OpaqueType} <- dict:to_list(OpaqueTypeDict)].

-spec t_opaque_match_atom(erl_type(), [erl_type()]) -> [erl_type()].

t_opaque_match_atom(?atom(_) = Atom,  Opaques) ->
  case t_atom_vals(Atom) of
    unknown -> [];
    _  -> [O || O <- Opaques, t_inf(Atom, O, opaque) =/= ?none,
		t_opaque_atom_vals(t_opaque_structure(O)) =/= unknown]
  end;
t_opaque_match_atom(_, _) -> [].

-spec t_opaque_atom_vals(erl_type()) -> 'unknown' | [atom(),...].

t_opaque_atom_vals(OpaqueStruct) ->
  case OpaqueStruct of
    ?atom(_) -> t_atom_vals(OpaqueStruct);
    ?union([Atom,_,_,_,_,_,_,_,_,_]) -> t_atom_vals(Atom);
    _ -> unknown
  end.
	  
-spec t_opaque_match_record(erl_type(), [erl_type()]) -> [erl_type()].

t_opaque_match_record(?tuple([?atom(_) = Tag|_Fields], _, _) = Rec, Opaques) ->
  [O || O <- Opaques, t_inf(Rec, O, opaque) =/= ?none,
	lists:member(Tag, t_opaque_tuple_tags(t_opaque_structure(O)))];
t_opaque_match_record(_, _) -> [].

-spec t_opaque_tuple_tags(erl_type()) -> [erl_type()].

t_opaque_tuple_tags(OpaqueStruct) ->
  case OpaqueStruct of
    ?tuple([?atom(_) = Tag|_Fields], _, _) -> [Tag];
    ?tuple_set(_) = TupleSet ->
      Tuples = t_tuple_subtypes(TupleSet),
      lists:flatten([t_opaque_tuple_tags(T) || T <- Tuples]);
    ?union([_,_,_,_,_,_,Tuples,_,_,_]) -> t_opaque_tuple_tags(Tuples);
    _ -> []
  end.

%% Decompose opaque instances of type arg2 to structured types, in arg1
%% XXX: Same as t_unopaque
-spec t_struct_from_opaque(erl_type(), [erl_type()]) -> erl_type().

t_struct_from_opaque(?function(Domain, Range), Opaques) ->
  ?function(t_struct_from_opaque(Domain, Opaques),
	    t_struct_from_opaque(Range, Opaques));
t_struct_from_opaque(?list(Types, Term, Size), Opaques) ->
  ?list(t_struct_from_opaque(Types, Opaques), Term, Size);
t_struct_from_opaque(?opaque(_) = T, Opaques) ->
  case lists:member(T, Opaques) of
    true  -> t_opaque_structure(T);
    false -> T
  end;
t_struct_from_opaque(?product(Types), Opaques) ->
  ?product(list_struct_from_opaque(Types, Opaques));
t_struct_from_opaque(?tuple(?any, _, _) = T, _Opaques) -> T;
t_struct_from_opaque(?tuple(Types, Arity, Tag), Opaques) ->
  ?tuple(list_struct_from_opaque(Types, Opaques), Arity, Tag);
t_struct_from_opaque(?tuple_set(Set), Opaques) ->
  NewSet = [{Sz, [t_struct_from_opaque(T, Opaques) || T <- Tuples]}
	    || {Sz, Tuples} <- Set],
  ?tuple_set(NewSet);
t_struct_from_opaque(?union(List), Opaques) ->
  t_sup(list_struct_from_opaque(List, Opaques));
t_struct_from_opaque(Type, _Opaques) -> Type.

list_struct_from_opaque(Types, Opaques) ->
  [t_struct_from_opaque(Type, Opaques) || Type <- Types].

-spec t_unopaque_on_mismatch(erl_type(), erl_type(), [erl_type()]) -> erl_type().

t_unopaque_on_mismatch(GenType, Type, Opaques) ->
  case t_inf(GenType, Type) of
    ?none ->
      Unopaqued = t_unopaque(Type, Opaques),
      %% XXX: Unions might be a problem, must investigate.
      case t_inf(GenType, Unopaqued) of
	?none -> Type;
	_ -> Unopaqued
      end;
     _ -> Type
  end.

-spec module_builtin_opaques(module()) -> [erl_type()].

module_builtin_opaques(Module) ->
  [O || O <- all_opaque_builtins(), t_opaque_module(O) =:= Module].

%%-----------------------------------------------------------------------------
%% Remote types: these types are used for preprocessing;
%% they should never reach the analysis stage.

-spec t_remote(module(), atom(), [_]) -> erl_type().

t_remote(Mod, Name, Args) ->
  ?remote(set_singleton(#remote{mod = Mod, name = Name, args = Args})).

-spec t_is_remote(erl_type()) -> boolean().

t_is_remote(?remote(_)) -> true;
t_is_remote(_) -> false.

-spec t_solve_remote(erl_type(), set(), dict()) -> erl_type().

t_solve_remote(Type, ExpTypes, Records) ->
  {RT, _RR} = t_solve_remote(Type, ExpTypes, Records, []),
  RT.

t_solve_remote(?function(Domain, Range), ET, R, C) ->
  {RT1, RR1} = t_solve_remote(Domain, ET, R, C),
  {RT2, RR2} = t_solve_remote(Range, ET, R, C),
  {?function(RT1, RT2), RR1 ++ RR2};
t_solve_remote(?list(Types, Term, Size), ET, R, C) ->
  {RT, RR} = t_solve_remote(Types, ET, R, C),
  {?list(RT, Term, Size), RR};
t_solve_remote(?product(Types), ET, R, C) ->
  {RL, RR} = list_solve_remote(Types, ET, R, C),
  {?product(RL), RR};
t_solve_remote(?opaque(Set), ET, R, C) ->
  List = ordsets:to_list(Set),
  {NewList, RR} = opaques_solve_remote(List, ET, R, C),
  {?opaque(ordsets:from_list(NewList)), RR};
t_solve_remote(?tuple(?any, _, _) = T, _ET, _R, _C) -> {T, []};
t_solve_remote(?tuple(Types, Arity, Tag), ET, R, C)  ->
  {RL, RR} = list_solve_remote(Types, ET, R, C),
  {?tuple(RL, Arity, Tag), RR};
t_solve_remote(?tuple_set(Set), ET, R, C) ->
  {NewSet, RR} = tuples_solve_remote(Set, ET, R, C),
  {?tuple_set(NewSet), RR};
t_solve_remote(?remote(Set), ET, R, C) ->
  RemoteList = ordsets:to_list(Set),
  {RL, RR} = list_solve_remote_type(RemoteList, ET, R, C),
  {t_sup(RL), RR};
t_solve_remote(?union(List), ET, R, C) ->
  {RL, RR} = list_solve_remote(List, ET, R, C),
  {t_sup(RL), RR};
t_solve_remote(T, _ET, _R, _C) -> {T, []}.

t_solve_remote_type(#remote{mod = RemMod, name = Name, args = Args} = RemType,
                    ET, R, C) ->
  ArgsLen = length(Args),
  case dict:find(RemMod, R) of
    error ->
      self() ! {self(), ext_types, {RemMod, Name, ArgsLen}},
      {t_any(), []};
    {ok, RemDict} ->
      MFA = {RemMod, Name, ArgsLen},
      case sets:is_element(MFA, ET) of
        true ->
          case lookup_type(Name, RemDict) of
            {type, {_Mod, Type, ArgNames}} when ArgsLen =:= length(ArgNames) ->
              {NewType, NewCycle, NewRR} =
                case unfold(RemType, C) of
                  true ->
                    List = lists:zip(ArgNames, Args),
                    TmpVarDict = dict:from_list(List),
                    {t_from_form(Type, RemDict, TmpVarDict), [RemType|C], []};
                  false -> {t_any(), C, [RemType]}
                end,
              {RT, RR} = t_solve_remote(NewType, ET, R, NewCycle),
              RetRR = NewRR ++ RR,
              RT1 =
                case lists:member(RemType, RetRR) of
                  true -> t_limit(RT, ?REC_TYPE_LIMIT);
                  false -> RT
                end,
              {RT1, RetRR};
            {opaque, {Mod, Type, ArgNames}} when ArgsLen =:= length(ArgNames) ->
              List = lists:zip(ArgNames, Args),
              TmpVarDict = dict:from_list(List),
              {Rep, NewCycle, NewRR} =
                case unfold(RemType, C) of
                  true -> {t_from_form(Type, RemDict, TmpVarDict), [RemType|C], []};
                  false -> {t_any(), C, [RemType]}
                end,
              {NewRep, RR} = t_solve_remote(Rep, ET, R, NewCycle),
              RetRR = NewRR ++ RR,
              RT1 =
                case lists:member(RemType, RetRR) of
                  true -> t_limit(NewRep, ?REC_TYPE_LIMIT);
                  false -> NewRep
                end,
              {t_from_form({opaque, -1, Name, {Mod, Args, RT1}},
                           RemDict, TmpVarDict),
               RetRR};
            {type, _} ->
              Msg = io_lib:format("Unknown remote type ~w\n", [Name]),
              throw({error, Msg});
            {opaque, _} ->
              Msg = io_lib:format("Unknown remote opaque type ~w\n", [Name]),
              throw({error, Msg});
            error ->
              Msg = io_lib:format("Unable to find remote type ~w:~w()\n",
                                  [RemMod, Name]),
              throw({error, Msg})
          end;
        false ->
          Msg = io_lib:format("Unable to find exported type ~w:~w/~w\n",
                              [RemMod, Name, ArgsLen]),
          throw({error, Msg})
      end
  end.

list_solve_remote([], _ET, _R, _C) ->
  {[], []};
list_solve_remote([Type|Types], ET, R, C) ->
  {RT, RR1} = t_solve_remote(Type, ET, R, C),
  {RL, RR2} = list_solve_remote(Types, ET, R, C),
  {[RT|RL], RR1 ++ RR2}.

list_solve_remote_type([], _ET, _R, _C) ->
  {[], []};
list_solve_remote_type([Type|Types], ET, R, C) ->
  {RT, RR1} = t_solve_remote_type(Type, ET, R, C),
  {RL, RR2} = list_solve_remote_type(Types, ET, R, C),
  {[RT|RL], RR1 ++ RR2}.

opaques_solve_remote([], _ET, _R, _C) ->
  {[], []};
opaques_solve_remote([#opaque{struct = Struct} = Remote|Tail], ET, R, C) ->
  {RT, RR1} = t_solve_remote(Struct, ET, R, C),
  {LOp, RR2} = opaques_solve_remote(Tail, ET, R, C),
  {[Remote#opaque{struct = RT}|LOp], RR1 ++ RR2}.

tuples_solve_remote([], _ET, _R, _C) ->
  {[], []};
tuples_solve_remote([{Sz, Tuples}|Tail], ET, R, C) ->
  {RL, RR1} = list_solve_remote(Tuples, ET, R, C),
  {LSzTpls, RR2} = tuples_solve_remote(Tail, ET, R, C),
  {[{Sz, RL}|LSzTpls], RR1 ++ RR2}.

%%-----------------------------------------------------------------------------
%% Unit type. Signals non termination.
%%

-spec t_unit() -> erl_type().

t_unit() ->
  ?unit.

-spec t_is_unit(erl_type()) -> boolean().

t_is_unit(?unit) -> true;
t_is_unit(_) -> false.

-spec t_is_none_or_unit(erl_type()) -> boolean().

t_is_none_or_unit(?none) -> true;
t_is_none_or_unit(?unit) -> true;
t_is_none_or_unit(_) -> false.

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

t_atom_vals(?atom(?any)) -> unknown;
t_atom_vals(?atom(Set)) -> set_to_list(Set);
t_atom_vals(Other) ->
  ?atom(_) = Atm = t_inf(t_atom(), Other),
  t_atom_vals(Atm).

-spec t_is_atom(erl_type()) -> boolean().

t_is_atom(?atom(_)) -> true;
t_is_atom(_) -> false.

-spec t_is_atom(atom(), erl_type()) -> boolean().

t_is_atom(Atom, ?atom(?any)) when is_atom(Atom) -> false;
t_is_atom(Atom, ?atom(Set)) when is_atom(Atom) -> set_is_singleton(Atom, Set);
t_is_atom(Atom, _) when is_atom(Atom) -> false.

%%------------------------------------

-spec t_boolean() -> erl_type().

t_boolean() ->
  ?atom(set_from_list([false, true])).

-spec t_is_boolean(erl_type()) -> boolean().

t_is_boolean(?atom(?any)) -> false;
t_is_boolean(?atom(Set)) ->
  case set_size(Set) of
    1 -> set_is_element(true, Set) orelse set_is_element(false, Set);
    2 -> set_is_element(true, Set) andalso set_is_element(false, Set);
    N when is_integer(N), N > 2 -> false
  end;
t_is_boolean(_) -> false.

%%-----------------------------------------------------------------------------
%% Binaries
%%

-spec t_binary() -> erl_type().

t_binary() ->
  ?bitstr(8, 0).

-spec t_is_binary(erl_type()) -> boolean().

t_is_binary(?bitstr(U, B)) -> 
  ((U rem 8) =:= 0) andalso ((B rem 8) =:= 0);
t_is_binary(_) -> false.

%%-----------------------------------------------------------------------------
%% Bitstrings
%%

-spec t_bitstr() -> erl_type().

t_bitstr() ->
  ?bitstr(1, 0).

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
  bitstr_concat(T1p, T2p).

-spec t_bitstr_match(erl_type(), erl_type()) -> erl_type().

t_bitstr_match(T1, T2) ->
  T1p = t_inf(t_bitstr(), T1),
  T2p = t_inf(t_bitstr(), T2),
  bitstr_match(T1p, T2p).

-spec t_is_bitstr(erl_type()) -> boolean().

t_is_bitstr(?bitstr(_, _)) -> true;
t_is_bitstr(_) -> false.

%%-----------------------------------------------------------------------------
%% Matchstates
%%

-spec t_matchstate() -> erl_type().

t_matchstate() ->
  ?any_matchstate.

-spec t_matchstate(erl_type(), non_neg_integer()) -> erl_type().

t_matchstate(Init, 0) ->
  ?matchstate(Init, Init);
t_matchstate(Init, Max) when is_integer(Max) ->
  Slots = [Init|[?none || _ <- lists:seq(1, Max)]],
  ?matchstate(Init, t_product(Slots)).

-spec t_is_matchstate(erl_type()) -> boolean().

t_is_matchstate(?matchstate(_, _)) -> true;
t_is_matchstate(_) -> false.

-spec t_matchstate_present(erl_type()) -> erl_type().

t_matchstate_present(Type) ->
  case t_inf(t_matchstate(), Type) of
    ?matchstate(P, _) -> P;
    _ -> ?none
  end.

-spec t_matchstate_slot(erl_type(), non_neg_integer()) -> erl_type().

t_matchstate_slot(Type, Slot) ->
  RealSlot = Slot + 1,
  case t_inf(t_matchstate(), Type) of
    ?matchstate(_, ?any) -> ?any;
    ?matchstate(_, ?product(Vals)) when length(Vals) >= RealSlot ->
      lists:nth(RealSlot, Vals);
    ?matchstate(_, ?product(_)) ->
      ?none;
    ?matchstate(_, SlotType) when RealSlot =:= 1 ->
      SlotType;
    _ ->
      ?none
  end.

-spec t_matchstate_slots(erl_type()) -> erl_type().

t_matchstate_slots(?matchstate(_, Slots)) ->
  Slots.

-spec t_matchstate_update_present(erl_type(), erl_type()) -> erl_type().

t_matchstate_update_present(New, Type) -> 
  case t_inf(t_matchstate(), Type) of
    ?matchstate(_, Slots) ->
      ?matchstate(New, Slots);
    _ -> ?none
  end.

-spec t_matchstate_update_slot(erl_type(), erl_type(), non_neg_integer()) -> erl_type().

t_matchstate_update_slot(New, Type, Slot) -> 
  RealSlot = Slot + 1,
  case t_inf(t_matchstate(), Type) of
    ?matchstate(Pres, Slots) ->
      NewSlots = 
	case Slots of
	  ?any ->
	    ?any;
	  ?product(Vals) when length(Vals) >= RealSlot ->
	    NewTuple = setelement(RealSlot, list_to_tuple(Vals), New),
	    NewVals = tuple_to_list(NewTuple),
	    ?product(NewVals);
	  ?product(_) ->
	    ?none;
	  _ when RealSlot =:= 1 ->
	    New;
	  _ ->
	    ?none
	end,
      ?matchstate(Pres, NewSlots);
    _ ->
      ?none
  end.

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

t_fun_args(?function(?any, _)) ->
  unknown;
t_fun_args(?function(?product(Domain), _)) when is_list(Domain) ->
  Domain.

-spec t_fun_arity(erl_type()) -> 'unknown' | non_neg_integer().

t_fun_arity(?function(?any, _)) ->
  unknown;
t_fun_arity(?function(?product(Domain), _)) ->
  length(Domain).

-spec t_fun_range(erl_type()) -> erl_type().

t_fun_range(?function(_, Range)) ->
  Range.

-spec t_is_fun(erl_type()) -> boolean().

t_is_fun(?function(_, _)) -> true;
t_is_fun(_) -> false.

%%-----------------------------------------------------------------------------
%% Identifiers. Includes ports, pids and refs.
%% 

-spec t_identifier() -> erl_type().

t_identifier() ->
  ?identifier(?any).

-ifdef(DO_ERL_TYPES_TEST).
-spec t_is_identifier(erl_type()) -> erl_type().

t_is_identifier(?identifier(_)) -> true;
t_is_identifier(_) -> false.
-endif.

%%------------------------------------

-spec t_port() -> erl_type().

t_port() ->
  ?identifier(set_singleton(?port_qual)).

-spec t_is_port(erl_type()) -> boolean().

t_is_port(?identifier(?any)) -> false;
t_is_port(?identifier(Set)) -> set_is_singleton(?port_qual, Set);
t_is_port(_) -> false.

%%------------------------------------

-spec t_pid() -> erl_type().

t_pid() ->
  ?identifier(set_singleton(?pid_qual)).

-spec t_is_pid(erl_type()) -> boolean().

t_is_pid(?identifier(?any)) -> false;
t_is_pid(?identifier(Set)) -> set_is_singleton(?pid_qual, Set);
t_is_pid(_) -> false.

%%------------------------------------

-spec t_reference() -> erl_type().

t_reference() ->
  ?identifier(set_singleton(?reference_qual)).

-spec t_is_reference(erl_type()) -> boolean().

t_is_reference(?identifier(?any)) -> false;
t_is_reference(?identifier(Set)) -> set_is_singleton(?reference_qual, Set);
t_is_reference(_) -> false.

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

t_is_number(?number(_, _)) -> true;
t_is_number(_) -> false.

%% Currently, the type system collapses all floats to ?float and does
%% not keep any information about their values. As a result, the list
%% that this function returns contains only integers.
-spec t_number_vals(erl_type()) -> 'unknown' | [integer(),...].

t_number_vals(?int_set(?any)) -> unknown;
t_number_vals(?int_set(Set)) -> set_to_list(Set);
t_number_vals(?number(_, _)) -> unknown;
t_number_vals(Other) ->
  Inf = t_inf(Other, t_number()),
  false = t_is_none(Inf), % sanity check
  t_number_vals(Inf).

%%------------------------------------

-spec t_float() -> erl_type().

t_float() ->
  ?float.

-spec t_is_float(erl_type()) -> boolean().

t_is_float(?float) -> true;
t_is_float(_) -> false.

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

t_is_integer(?integer(_)) -> true;
t_is_integer(_) -> false.

%%------------------------------------

-spec t_byte() -> erl_type().

t_byte() ->
  ?byte.

-ifdef(DO_ERL_TYPES_TEST).
-spec t_is_byte(erl_type()) -> boolean().

t_is_byte(?int_range(neg_inf, _)) -> false;
t_is_byte(?int_range(_, pos_inf)) -> false;
t_is_byte(?int_range(From, To))
  when is_integer(From), From >= 0, is_integer(To), To =< ?MAX_BYTE -> true;
t_is_byte(?int_set(Set)) -> 
  (set_min(Set) >= 0) andalso (set_max(Set) =< ?MAX_BYTE);
t_is_byte(_) -> false.
-endif.

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
  case t_inf(Tail, t_maybe_improper_list()) of
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

-spec t_is_cons(erl_type()) -> boolean().

t_is_cons(?nonempty_list(_, _)) -> true;
t_is_cons(_) -> false.  

-spec t_cons_hd(erl_type()) -> erl_type().

t_cons_hd(?nonempty_list(Contents, _Termination)) -> Contents.

-spec t_cons_tl(erl_type()) -> erl_type().

t_cons_tl(?nonempty_list(_Contents, Termination) = T) ->
  t_sup(Termination, T).

-spec t_nil() -> erl_type().

t_nil() ->
  ?nil.

-spec t_is_nil(erl_type()) -> boolean().

t_is_nil(?nil) -> true;
t_is_nil(_) -> false.

-spec t_list() -> erl_type().

t_list() ->
  ?list(?any, ?nil, ?unknown_qual).

-spec t_list(erl_type()) -> erl_type().

t_list(?none) -> ?none;
t_list(?unit) -> ?none;
t_list(Contents) ->
  ?list(Contents, ?nil, ?unknown_qual).

-spec t_list_elements(erl_type()) -> erl_type().

t_list_elements(?list(Contents, _, _)) -> Contents;
t_list_elements(?nil) -> ?none.

-spec t_list_termination(erl_type()) -> erl_type().

t_list_termination(?nil) -> ?nil;
t_list_termination(?list(_, Term, _)) -> Term.

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
t_maybe_improper_list(Content, Termination) ->
  %% Safety check
  true = t_is_subtype(t_nil(), Termination),
  ?list(Content, Termination, ?unknown_qual).

-spec t_is_maybe_improper_list(erl_type()) -> boolean().

t_is_maybe_improper_list(?list(_, _, _)) -> true;
t_is_maybe_improper_list(?nil) -> true;
t_is_maybe_improper_list(_) -> false.

%% %% Should only be used if you know what you are doing. See t_cons/2
%% -spec t_improper_list(erl_type(), erl_type()) -> erl_type().
%%
%% t_improper_list(?unit, _Termination) -> ?none;
%% t_improper_list(_Content, ?unit) -> ?none;
%% t_improper_list(Content, Termination) ->
%%   %% Safety check
%%   false = t_is_subtype(t_nil(), Termination),
%%   ?list(Content, Termination, ?any).  

-spec lift_list_to_pos_empty(erl_type()) -> erl_type().

lift_list_to_pos_empty(?nil) -> ?nil;
lift_list_to_pos_empty(?list(Content, Termination, _)) -> 
  ?list(Content, Termination, ?unknown_qual).

%%-----------------------------------------------------------------------------
%% Tuples
%%

-spec t_tuple() -> erl_type().

t_tuple() ->
  ?tuple(?any, ?any, ?any).

-spec t_tuple(non_neg_integer() | [erl_type()]) -> erl_type().

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

get_tuple_tags([?atom(?any)|_]) -> [?any];
get_tuple_tags([?atom(Set)|_]) ->
  case set_size(Set) > ?TUPLE_TAG_LIMIT of
    true -> [?any];
    false -> [t_atom(A) || A <- set_to_list(Set)]
  end;
get_tuple_tags(_) -> [?any].

%% to be used for a tuple with known types for its arguments (not ?any)
-spec t_tuple_args(erl_type()) -> [erl_type()].

t_tuple_args(?tuple(Args, _, _)) when is_list(Args) -> Args.

%% to be used for a tuple with a known size (not ?any)
-spec t_tuple_size(erl_type()) -> non_neg_integer().

t_tuple_size(?tuple(_, Size, _)) when is_integer(Size) -> Size.

-spec t_tuple_sizes(erl_type()) -> 'unknown' | [non_neg_integer(),...].

t_tuple_sizes(?tuple(?any, ?any, ?any)) -> unknown;
t_tuple_sizes(?tuple(_, Size, _)) when is_integer(Size) -> [Size];
t_tuple_sizes(?tuple_set(List)) -> [Size || {Size, _} <- List].

-spec t_tuple_subtypes(erl_type()) -> 'unknown' | [erl_type(),...].

t_tuple_subtypes(?tuple(?any, ?any, ?any)) -> unknown;
t_tuple_subtypes(?tuple(_, _, _) = T) -> [T];
t_tuple_subtypes(?tuple_set(List)) ->
  lists:append([Tuples || {_Size, Tuples} <- List]).

-spec t_is_tuple(erl_type()) -> boolean().

t_is_tuple(?tuple(_, _, _)) -> true;
t_is_tuple(?tuple_set(_)) -> true;
t_is_tuple(_) -> false.

%%-----------------------------------------------------------------------------
%% Non-primitive types, including some handy syntactic sugar types
%%

-spec t_constant() -> erl_type().

t_constant() ->
  t_sup([t_number(), t_identifier(), t_atom(), t_fun(), t_binary()]).

-spec t_is_constant(erl_type()) -> boolean().

t_is_constant(X) ->
  t_is_subtype(X, t_constant()).

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
  t_sup(t_atom(), t_parameterized_module()).

-spec t_node() -> erl_type().

t_node() ->
  t_atom().

-spec t_iodata() -> erl_type().

t_iodata() ->
  t_sup(t_iolist(), t_binary()).

-spec t_iolist() -> erl_type().

t_iolist() ->
  t_iolist(1).

-spec t_iolist(non_neg_integer()) -> erl_type().

t_iolist(N) when N > 0 ->
  t_maybe_improper_list(t_sup([t_iolist(N-1), t_binary(), t_byte()]),
		        t_sup(t_binary(), t_nil()));
t_iolist(0) ->
  t_maybe_improper_list(t_any(), t_sup(t_binary(), t_nil())).

-spec t_parameterized_module() -> erl_type().

t_parameterized_module() ->
  t_tuple().

-spec t_timeout() -> erl_type().

t_timeout() ->
  t_sup(t_non_neg_integer(), t_atom('infinity')).

%%-----------------------------------------------------------------------------
%% Some built-in opaque types
%%

-spec t_array() -> erl_type().

t_array() ->
  t_opaque(array, array, [],
	   t_tuple([t_atom('array'),
		    t_non_neg_integer(), t_non_neg_integer(),
		    t_any(), t_any()])).

-spec t_dict() -> erl_type().

t_dict() ->
  t_opaque(dict, dict, [],
	   t_tuple([t_atom('dict'),
		    t_non_neg_integer(), t_non_neg_integer(),
		    t_non_neg_integer(), t_non_neg_integer(),
		    t_non_neg_integer(), t_non_neg_integer(),
		    t_tuple(), t_tuple()])).

-spec t_digraph() -> erl_type().

t_digraph() ->
  t_opaque(digraph, digraph, [],
	   t_tuple([t_atom('digraph'),
		    t_sup(t_atom(), t_tid()),
		    t_sup(t_atom(), t_tid()),
		    t_sup(t_atom(), t_tid()),
		    t_boolean()])).

-spec t_gb_set() -> erl_type().

t_gb_set() ->
  t_opaque(gb_sets, gb_set, [],
	   t_tuple([t_non_neg_integer(), t_sup(t_atom('nil'), t_tuple(3))])).

-spec t_gb_tree() -> erl_type().

t_gb_tree() ->
  t_opaque(gb_trees, gb_tree, [],
	   t_tuple([t_non_neg_integer(), t_sup(t_atom('nil'), t_tuple(4))])).

-spec t_queue() -> erl_type().

t_queue() ->
  t_opaque(queue, queue, [], t_tuple([t_list(), t_list()])).

-spec t_set() -> erl_type().

t_set() ->
  t_opaque(sets, set, [],
	   t_tuple([t_atom('set'), t_non_neg_integer(), t_non_neg_integer(),
		    t_pos_integer(), t_non_neg_integer(), t_non_neg_integer(),
		    t_non_neg_integer(), t_tuple(), t_tuple()])).

-spec t_tid() -> erl_type().

t_tid() ->
  t_opaque(ets, tid, [], t_integer()).

-spec all_opaque_builtins() -> [erl_type()].

all_opaque_builtins() ->
  [t_array(), t_dict(), t_digraph(), t_gb_set(),
   t_gb_tree(), t_queue(), t_set(), t_tid()].

-spec is_opaque_builtin(atom(), atom()) -> boolean().

is_opaque_builtin(array, array) -> true;
is_opaque_builtin(dict, dict) -> true;
is_opaque_builtin(digraph, digraph) -> true;
is_opaque_builtin(gb_sets, gb_set) -> true;
is_opaque_builtin(gb_trees, gb_tree) -> true;
is_opaque_builtin(queue, queue) -> true;
is_opaque_builtin(sets, set) -> true;
is_opaque_builtin(ets, tid) -> true;
is_opaque_builtin(_, _) -> false.

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
t_has_var(?list(Contents, Termination, _)) ->
  t_has_var(Contents) orelse t_has_var(Termination);
t_has_var(?product(Types)) -> t_has_var_list(Types);
t_has_var(?tuple(?any, ?any, ?any)) -> false;
t_has_var(?tuple(Elements, _, _)) ->
  t_has_var_list(Elements);
t_has_var(?tuple_set(_) = T) ->
  t_has_var_list(t_tuple_subtypes(T));
%% t_has_var(?union(_) = U) ->
%% exit(lists:flatten(io_lib:format("Union happens in t_has_var/1 ~p\n",[U])));
t_has_var(_) -> false.

-spec t_has_var_list([erl_type()]) -> boolean().

t_has_var_list([T|Ts]) ->
  t_has_var(T) orelse t_has_var_list(Ts);
t_has_var_list([]) -> false.

-spec t_collect_vars(erl_type()) -> [erl_type()].

t_collect_vars(T) ->
  t_collect_vars(T, []).

-spec t_collect_vars(erl_type(), [erl_type()]) -> [erl_type()].

t_collect_vars(?var(_) = Var, Acc) ->
  ordsets:add_element(Var, Acc);
t_collect_vars(?function(Domain, Range), Acc) ->
  ordsets:union(t_collect_vars(Domain, Acc), t_collect_vars(Range, []));
t_collect_vars(?list(Contents, Termination, _), Acc) ->
  ordsets:union(t_collect_vars(Contents, Acc), t_collect_vars(Termination, []));
t_collect_vars(?product(Types), Acc) ->
  lists:foldl(fun(T, TmpAcc) -> t_collect_vars(T, TmpAcc) end, Acc, Types);
t_collect_vars(?tuple(?any, ?any, ?any), Acc) ->
  Acc;
t_collect_vars(?tuple(Types, _, _), Acc) ->
  lists:foldl(fun(T, TmpAcc) -> t_collect_vars(T, TmpAcc) end, Acc, Types);
t_collect_vars(?tuple_set(_) = TS, Acc) ->
  lists:foldl(fun(T, TmpAcc) -> t_collect_vars(T, TmpAcc) end, Acc, 
	      t_tuple_subtypes(TS));
t_collect_vars(_, Acc) ->
  Acc.


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

-spec t_from_range_unsafe(rng_elem(), rng_elem()) -> erl_type().

t_from_range_unsafe(neg_inf, pos_inf) -> t_integer();
t_from_range_unsafe(neg_inf, Y) -> ?int_range(neg_inf, Y);
t_from_range_unsafe(X, pos_inf) -> ?int_range(X, pos_inf);
t_from_range_unsafe(X, Y) when is_integer(X), is_integer(Y), X =< Y ->
  if (Y - X) < ?SET_LIMIT -> t_integers(lists:seq(X, Y));
     true -> ?int_range(X, Y)
  end;
t_from_range_unsafe(X, Y) when is_integer(X), is_integer(Y) -> t_none();
t_from_range_unsafe(pos_inf, neg_inf) -> t_none().

-spec t_is_fixnum(erl_type()) -> boolean().

t_is_fixnum(?int_range(neg_inf, _)) -> false;
t_is_fixnum(?int_range(_, pos_inf)) -> false;
t_is_fixnum(?int_range(From, To)) ->
  is_fixnum(From) andalso is_fixnum(To);
t_is_fixnum(?int_set(Set)) ->
  is_fixnum(set_min(Set)) andalso is_fixnum(set_max(Set));
t_is_fixnum(_) -> false.

-spec is_fixnum(integer()) -> boolean().

is_fixnum(N) when is_integer(N) ->
  Bits = ?BITS,
  (N =< ((1 bsl (Bits - 1)) - 1)) andalso (N >= -(1 bsl (Bits - 1))).

infinity_geq(pos_inf, _) -> true;
infinity_geq(_, pos_inf) -> false;
infinity_geq(_, neg_inf) -> true;
infinity_geq(neg_inf, _) -> false;
infinity_geq(A, B) -> A >= B.

-spec t_is_bitwidth(erl_type()) -> boolean().

t_is_bitwidth(?int_range(neg_inf, _)) -> false;
t_is_bitwidth(?int_range(_, pos_inf)) -> false;
t_is_bitwidth(?int_range(From, To)) ->
  infinity_geq(From, 0) andalso infinity_geq(?BITS, To);
t_is_bitwidth(?int_set(Set)) ->
  infinity_geq(set_min(Set), 0) andalso infinity_geq(?BITS, set_max(Set));
t_is_bitwidth(_) -> false.

-spec number_min(erl_type()) -> rng_elem().

number_min(?int_range(From, _)) -> From;
number_min(?int_set(Set)) -> set_min(Set);
number_min(?number(?any, _Tag)) -> neg_inf.

-spec number_max(erl_type()) -> rng_elem().

number_max(?int_range(_, To)) -> To;
number_max(?int_set(Set)) -> set_max(Set);
number_max(?number(?any, _Tag)) -> pos_inf.

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
%% Supremum
%%

-spec t_sup([erl_type()]) -> erl_type().

t_sup([?any|_]) ->
  ?any;
t_sup([H1, H2|T]) ->
  t_sup([t_sup(H1, H2)|T]);
t_sup([H]) ->
  subst_all_vars_to_any(H);
t_sup([]) ->
  ?none.

-spec t_sup(erl_type(), erl_type()) -> erl_type().

t_sup(?any, _) -> ?any;
t_sup(_, ?any) -> ?any;
t_sup(?none, T) -> T;
t_sup(T, ?none) -> T;
t_sup(?unit, T) -> T;
t_sup(T, ?unit) -> T;
t_sup(T, T) -> subst_all_vars_to_any(T);
t_sup(?var(_), _) -> ?any;
t_sup(_, ?var(_)) -> ?any;
t_sup(?atom(Set1), ?atom(Set2)) ->
  ?atom(set_union(Set1, Set2));
t_sup(?bitstr(U1, B1), ?bitstr(U2, B2)) ->
  t_bitstr(gcd(gcd(U1, U2), abs(B1-B2)), lists:min([B1, B2]));
t_sup(?function(Domain1, Range1), ?function(Domain2, Range2)) ->
  %% The domain is either a product or any.
  ?function(t_sup(Domain1, Domain2), t_sup(Range1, Range2));
t_sup(?identifier(Set1), ?identifier(Set2)) ->
  ?identifier(set_union(Set1, Set2));
t_sup(?opaque(Set1), ?opaque(Set2)) ->
  ?opaque(set_union_no_limit(Set1, Set2));
%%Disallow unions with opaque types
%%t_sup(T1=?opaque(_,_,_), T2) ->
%%  io:format("Debug: t_sup executed with args ~w and ~w~n",[T1, T2]), ?none;
%%t_sup(T1, T2=?opaque(_,_,_)) ->
%%  io:format("Debug: t_sup executed with args ~w and ~w~n",[T1, T2]), ?none;
t_sup(?remote(Set1), ?remote(Set2)) ->
  ?remote(set_union_no_limit(Set1, Set2));
t_sup(?matchstate(Pres1, Slots1), ?matchstate(Pres2, Slots2)) ->
  ?matchstate(t_sup(Pres1, Pres2), t_sup(Slots1, Slots2));
t_sup(?nil, ?nil) -> ?nil;
t_sup(?nil, ?list(Contents, Termination, _)) ->
  ?list(Contents, t_sup(?nil, Termination), ?unknown_qual);
t_sup(?list(Contents, Termination, _), ?nil) ->
  ?list(Contents, t_sup(?nil, Termination), ?unknown_qual);
t_sup(?list(Contents1, Termination1, Size1), 
      ?list(Contents2, Termination2, Size2)) ->
  NewSize =
    case {Size1, Size2} of
      {?unknown_qual, ?unknown_qual} -> ?unknown_qual;
      {?unknown_qual, ?nonempty_qual} -> ?unknown_qual;
      {?nonempty_qual, ?unknown_qual} -> ?unknown_qual;
      {?nonempty_qual, ?nonempty_qual} -> ?nonempty_qual
    end,
  NewContents = t_sup(Contents1, Contents2),
  NewTermination = t_sup(Termination1, Termination2),
  TmpList = t_cons(NewContents, NewTermination),
  case NewSize of
    ?nonempty_qual -> TmpList;
    ?unknown_qual ->
      ?list(FinalContents, FinalTermination, _) = TmpList,
      ?list(FinalContents, FinalTermination, ?unknown_qual)
  end;
t_sup(?number(_, _), ?number(?any, ?unknown_qual) = T) -> T;  
t_sup(?number(?any, ?unknown_qual) = T, ?number(_, _)) -> T;
t_sup(?float, ?float) -> ?float;
t_sup(?float, ?integer(_)) -> t_number();
t_sup(?integer(_), ?float) -> t_number();
t_sup(?integer(?any) = T, ?integer(_)) -> T;
t_sup(?integer(_), ?integer(?any) = T) -> T;
t_sup(?int_set(Set1), ?int_set(Set2)) ->
  case set_union(Set1, Set2) of
    ?any ->
      t_from_range(min(set_min(Set1), set_min(Set2)), 
		   max(set_max(Set1), set_max(Set2)));
    Set -> ?int_set(Set)
  end;
t_sup(?int_range(From1, To1), ?int_range(From2, To2)) ->
  t_from_range(min(From1, From2), max(To1, To2));
t_sup(Range = ?int_range(_, _), ?int_set(Set)) ->
  expand_range_from_set(Range, Set);
t_sup(?int_set(Set), Range = ?int_range(_, _)) ->
  expand_range_from_set(Range, Set);
t_sup(?product(Types1), ?product(Types2)) ->
  L1 = length(Types1),
  L2 = length(Types2),
  if L1 =:= L2 -> ?product(t_sup_lists(Types1, Types2));
     true -> ?any
  end;
t_sup(?product(_), _) ->
  ?any;
t_sup(_, ?product(_)) ->
  ?any;
t_sup(?tuple(?any, ?any, ?any) = T, ?tuple(_, _, _)) -> T;
t_sup(?tuple(_, _, _), ?tuple(?any, ?any, ?any) = T) -> T;
t_sup(?tuple(?any, ?any, ?any) = T, ?tuple_set(_)) -> T;
t_sup(?tuple_set(_), ?tuple(?any, ?any, ?any) = T) -> T;
t_sup(?tuple(Elements1, Arity, Tag1) = T1,
      ?tuple(Elements2, Arity, Tag2) = T2) ->
  if Tag1 =:= Tag2 -> t_tuple(t_sup_lists(Elements1, Elements2));
     Tag1 =:= ?any -> t_tuple(t_sup_lists(Elements1, Elements2));
     Tag2 =:= ?any -> t_tuple(t_sup_lists(Elements1, Elements2));
     Tag1 < Tag2 -> ?tuple_set([{Arity, [T1, T2]}]);
     Tag1 > Tag2 -> ?tuple_set([{Arity, [T2, T1]}])
  end;
t_sup(?tuple(_, Arity1, _) = T1, ?tuple(_, Arity2, _) = T2) ->
  sup_tuple_sets([{Arity1, [T1]}], [{Arity2, [T2]}]);
t_sup(?tuple_set(List1), ?tuple_set(List2)) ->
  sup_tuple_sets(List1, List2);
t_sup(?tuple_set(List1), T2 = ?tuple(_, Arity, _)) ->
  sup_tuple_sets(List1, [{Arity, [T2]}]);
t_sup(?tuple(_, Arity, _) = T1, ?tuple_set(List2)) ->
  sup_tuple_sets([{Arity, [T1]}], List2);
t_sup(T1, T2) ->
  ?union(U1) = force_union(T1),
  ?union(U2) = force_union(T2),
  sup_union(U1, U2).

-spec t_sup_lists([erl_type()], [erl_type()]) -> [erl_type()].

t_sup_lists([T1|Left1], [T2|Left2]) ->
  [t_sup(T1, T2)|t_sup_lists(Left1, Left2)];
t_sup_lists([], []) ->
  [].

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
      case set_size(Set) > ?TUPLE_TAG_LIMIT of
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
    Tag2 =:= Tag2 -> NewElements = t_sup_lists(Elements1, Elements2),
		     NewAcc = [?tuple(NewElements, Arity, Tag1)|Acc],
		     sup_tuples_in_set(Left1, Left2, NewAcc)
  end;
sup_tuples_in_set([], L2, Acc) -> lists:reverse(Acc, L2);
sup_tuples_in_set(L1, [], Acc) -> lists:reverse(Acc, L1).

sup_union(U1, U2) ->
  sup_union(U1, U2, 0, []).

sup_union([?none|Left1], [?none|Left2], N, Acc) ->
  sup_union(Left1, Left2, N, [?none|Acc]);
sup_union([T1|Left1], [T2|Left2], N, Acc) ->
  sup_union(Left1, Left2, N+1, [t_sup(T1, T2)|Acc]);
sup_union([], [], N, Acc) ->
  if N =:= 0 -> ?none;
     N =:= 1 -> 
      [Type] = [T || T <- Acc, T =/= ?none],
      Type;
     N =:= length(Acc) -> ?any;
     true -> ?union(lists:reverse(Acc))
  end.

force_union(T = ?atom(_)) ->          ?atom_union(T);
force_union(T = ?bitstr(_, _)) ->     ?bitstr_union(T); 
force_union(T = ?function(_, _)) ->   ?function_union(T);
force_union(T = ?identifier(_)) ->    ?identifier_union(T);
force_union(T = ?list(_, _, _)) ->    ?list_union(T);
force_union(T = ?nil) ->              ?list_union(T);
force_union(T = ?number(_,_)) ->      ?number_union(T);
force_union(T = ?opaque(_)) ->        ?opaque_union(T);
force_union(T = ?remote(_)) ->        ?remote_union(T);
force_union(T = ?tuple(_, _, _)) ->   ?tuple_union(T);
force_union(T = ?tuple_set(_)) ->     ?tuple_union(T);
force_union(T = ?matchstate(_, _)) -> ?matchstate_union(T);
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
t_elements(?list(_, _, _) = T) -> [T];
t_elements(?number(_, _) = T) ->
  case T of
    ?number(?any, ?unknown_qual) -> [T]; 
    ?float -> [T];
    ?integer(?any) -> [T];
    ?int_range(_, _) -> [T];
    ?int_set(Set) ->
      [t_integer(I) || I <- Set]
  end;
t_elements(?opaque(_) = T) -> [T];
t_elements(?tuple(_, _, _) = T) -> [T];
t_elements(?tuple_set(_) = TS) ->
  case t_tuple_subtypes(TS) of
    unknown -> [];
    Elems -> Elems
  end;
t_elements(?union(List)) ->
  lists:append([t_elements(T) || T <- List]);
t_elements(?var(_)) -> [?any].  %% yes, vars exist -- what else to do here?
%% t_elements(T) ->
%%   io:format("T_ELEMENTS => ~p\n", [T]).

%%-----------------------------------------------------------------------------
%% Infimum
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
  t_inf(T1, T2, structured).

-type t_inf_mode() :: 'opaque' | 'structured'.
-spec t_inf(erl_type(), erl_type(), t_inf_mode()) -> erl_type().

t_inf(?var(_), ?var(_), _Mode) -> ?any;
t_inf(?var(_), T, _Mode) -> subst_all_vars_to_any(T);
t_inf(T, ?var(_), _Mode) -> subst_all_vars_to_any(T);
t_inf(?any, T, _Mode) -> subst_all_vars_to_any(T);
t_inf(T, ?any, _Mode) -> subst_all_vars_to_any(T);
t_inf(?unit, _, _Mode) -> ?unit;
t_inf(_, ?unit, _Mode) -> ?unit;
t_inf(?none, _, _Mode) -> ?none;
t_inf(_, ?none, _Mode) -> ?none;
t_inf(T, T, _Mode) -> subst_all_vars_to_any(T);
t_inf(?atom(Set1), ?atom(Set2), _) ->
  case set_intersection(Set1, Set2) of
    ?none ->  ?none;
    NewSet -> ?atom(NewSet)
  end;
t_inf(?bitstr(U1, B1), ?bitstr(0, B2), _Mode) ->
  if B2 >= B1 andalso (B2-B1) rem U1 =:= 0 -> t_bitstr(0, B2);
     true -> ?none
  end;
t_inf(?bitstr(0, B1), ?bitstr(U2, B2), _Mode) ->
  if B1 >= B2 andalso (B1-B2) rem U2 =:= 0 -> t_bitstr(0, B1);
     true -> ?none
  end;
t_inf(?bitstr(U1, B1), ?bitstr(U1, B1), _Mode) ->
  t_bitstr(U1, B1);
t_inf(?bitstr(U1, B1), ?bitstr(U2, B2), _Mode) when U2 > U1 ->
  inf_bitstr(U2, B2, U1, B1);
t_inf(?bitstr(U1, B1), ?bitstr(U2, B2), _Mode) ->
  inf_bitstr(U1, B1, U2, B2);
t_inf(?function(Domain1, Range1), ?function(Domain2, Range2), Mode) ->
  case t_inf(Domain1, Domain2, Mode) of
    ?none -> ?none;
    Domain -> ?function(Domain, t_inf(Range1, Range2, Mode))
  end;
t_inf(?identifier(Set1), ?identifier(Set2), _Mode) ->
  case set_intersection(Set1, Set2) of
    ?none -> ?none;
    Set -> ?identifier(Set)
  end;
t_inf(?matchstate(Pres1, Slots1), ?matchstate(Pres2, Slots2), _Mode) ->
  ?matchstate(t_inf(Pres1, Pres2), t_inf(Slots1, Slots2));
t_inf(?nil, ?nil, _Mode) -> ?nil;
t_inf(?nil, ?nonempty_list(_, _), _Mode) ->
  ?none;
t_inf(?nonempty_list(_, _), ?nil, _Mode) ->
  ?none;
t_inf(?nil, ?list(_Contents, Termination, _), Mode) ->
  t_inf(?nil, Termination, Mode);
t_inf(?list(_Contents, Termination, _), ?nil, Mode) ->
  t_inf(?nil, Termination, Mode);
t_inf(?list(Contents1, Termination1, Size1),
      ?list(Contents2, Termination2, Size2), Mode) ->  
  case t_inf(Termination1, Termination2, Mode) of
    ?none -> ?none;
    Termination ->
      case t_inf(Contents1, Contents2, Mode) of
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
t_inf(?number(_, _) = T1, ?number(_, _) = T2, _Mode) ->
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
      %% io:format("t_inf range, set args ~p ~p ~n", [T1, T2]),
      Ans2 = 
	case set_filter(fun(X) -> in_range(X, Range) end, Set) of
	  ?none -> ?none;
	  NewSet -> ?int_set(NewSet)
	end,
      %% io:format("Ans2 ~p ~n", [Ans2]),
      Ans2;
    {?int_set(Set), ?int_range(_, _) = Range} ->
      case set_filter(fun(X) -> in_range(X, Range) end, Set) of
	?none -> ?none;
	NewSet -> ?int_set(NewSet)
      end
  end;
t_inf(?product(Types1), ?product(Types2), Mode) ->
  L1 = length(Types1),
  L2 = length(Types2),
  if L1 =:= L2 -> ?product(t_inf_lists(Types1, Types2, Mode));
     true -> ?none
  end;
t_inf(?product(_), _, _Mode) ->
  ?none;
t_inf(_, ?product(_), _Mode) ->
  ?none;
t_inf(?tuple(?any, ?any, ?any), ?tuple(_, _, _) = T, _Mode) -> T;
t_inf(?tuple(_, _, _) = T, ?tuple(?any, ?any, ?any), _Mode) -> T;
t_inf(?tuple(?any, ?any, ?any), ?tuple_set(_) = T, _Mode) -> T;
t_inf(?tuple_set(_) = T, ?tuple(?any, ?any, ?any), _Mode) -> T;
t_inf(?tuple(Elements1, Arity, _Tag1), ?tuple(Elements2, Arity, _Tag2), Mode) ->
  case t_inf_lists_strict(Elements1, Elements2, Mode) of
    bottom -> ?none;
    NewElements -> t_tuple(NewElements)
  end;
t_inf(?tuple_set(List1), ?tuple_set(List2), Mode) ->
  inf_tuple_sets(List1, List2, Mode);
t_inf(?tuple_set(List), ?tuple(_, Arity, _) = T, Mode) ->
  inf_tuple_sets(List, [{Arity, [T]}], Mode);
t_inf(?tuple(_, Arity, _) = T, ?tuple_set(List), Mode) ->
  inf_tuple_sets(List, [{Arity, [T]}], Mode);
%% be careful: here and in the next clause T can be ?opaque
t_inf(?union(U1), T, Mode) ->
  ?union(U2) = force_union(T),
  inf_union(U1, U2, Mode);
t_inf(T, ?union(U2), Mode) ->
  ?union(U1) = force_union(T),
  inf_union(U1, U2, Mode);
%% and as a result, the cases for ?opaque should appear *after* ?union
t_inf(?opaque(Set1) = T1, ?opaque(Set2) = T2, Mode) ->
  case set_intersection(Set1, Set2) of
    ?none ->
      case Mode =:= opaque of
	true ->
	  Struct1 = t_opaque_structure(T1),
	  case t_inf(Struct1, T2) of
	    ?none ->
	      Struct2 = t_opaque_structure(T2),
	      case t_inf(Struct2, T1) of
		?none -> ?none;
		_ -> T2
	      end;
	    _ -> T1
	  end;
	false -> ?none
      end;
    NewSet -> ?opaque(NewSet)
  end;
t_inf(?opaque(_) = T1, T2, opaque) ->
  case t_inf(t_opaque_structure(T1), T2, structured) of
    ?none -> ?none;
    _Type -> T1
  end;
t_inf(T1, ?opaque(_) = T2, opaque) ->
  case t_inf(T1, t_opaque_structure(T2), structured) of
    ?none -> ?none;
    _Type -> T2
  end;
t_inf(#c{}, #c{}, _) ->
  ?none.

-spec t_inf_lists([erl_type()], [erl_type()]) -> [erl_type()].

t_inf_lists(L1, L2) ->
  t_inf_lists(L1, L2, structured).

-spec t_inf_lists([erl_type()], [erl_type()], t_inf_mode()) -> [erl_type()].

t_inf_lists(L1, L2, Mode) ->
  t_inf_lists(L1, L2, [], Mode).

-spec t_inf_lists([erl_type()], [erl_type()], [erl_type()], t_inf_mode()) -> [erl_type()].

t_inf_lists([T1|Left1], [T2|Left2], Acc, Mode) ->
  t_inf_lists(Left1, Left2, [t_inf(T1, T2, Mode)|Acc], Mode);
t_inf_lists([], [], Acc, _Mode) ->
  lists:reverse(Acc).

%% Infimum of lists with strictness.
%% If any element is the ?none type, the value 'bottom' is returned.

-spec t_inf_lists_strict([erl_type()], [erl_type()], t_inf_mode()) -> 'bottom' | [erl_type()].

t_inf_lists_strict(L1, L2, Mode) ->
  t_inf_lists_strict(L1, L2, [], Mode).

-spec t_inf_lists_strict([erl_type()], [erl_type()], [erl_type()], t_inf_mode()) -> 'bottom' | [erl_type()].

t_inf_lists_strict([T1|Left1], [T2|Left2], Acc, Mode) ->
  case t_inf(T1, T2, Mode) of
    ?none -> bottom;
    T -> t_inf_lists_strict(Left1, Left2, [T|Acc], Mode)
  end;
t_inf_lists_strict([], [], Acc, _Mode) ->
  lists:reverse(Acc).

-spec t_inf_lists_masked([erl_type()], [erl_type()], [t_inf_mode()]) -> [erl_type()].

t_inf_lists_masked(List1, List2, Mask) ->
  List = lists:zip3(List1, List2, Mask),
  [t_inf(T1, T2, Mode) || {T1, T2, Mode} <- List].

inf_tuple_sets(L1, L2, Mode) ->
  case inf_tuple_sets(L1, L2, [], Mode) of
    [] -> ?none;
    [{_Arity, [?tuple(_, _, _) = OneTuple]}] -> OneTuple;
    List -> ?tuple_set(List)
  end.

inf_tuple_sets([{Arity, Tuples1}|Left1], [{Arity, Tuples2}|Left2], Acc, Mode) ->
  case inf_tuples_in_sets(Tuples1, Tuples2, Mode) of
    [] -> inf_tuple_sets(Left1, Left2, Acc, Mode);
    NewTuples -> inf_tuple_sets(Left1, Left2, [{Arity, NewTuples}|Acc], Mode)
  end;
inf_tuple_sets(L1 = [{Arity1, _}|Left1], L2 = [{Arity2, _}|Left2], Acc, Mode) ->
  if Arity1 < Arity2 -> inf_tuple_sets(Left1, L2, Acc, Mode);
     Arity1 > Arity2 -> inf_tuple_sets(L1, Left2, Acc, Mode)
  end;
inf_tuple_sets([], _, Acc, _Mode) -> lists:reverse(Acc);
inf_tuple_sets(_, [], Acc, _Mode) -> lists:reverse(Acc).
      
inf_tuples_in_sets([?tuple(Elements1, _, ?any)], L2, Mode) ->
  NewList = [t_inf_lists_strict(Elements1, Elements2, Mode)
	     || ?tuple(Elements2, _, _) <- L2],
  [t_tuple(Es) || Es <- NewList, Es =/= bottom];
inf_tuples_in_sets(L1, [?tuple(Elements2, _, ?any)], Mode) ->
  NewList = [t_inf_lists_strict(Elements1, Elements2, Mode)
	     || ?tuple(Elements1, _, _) <- L1],
  [t_tuple(Es) || Es <- NewList, Es =/= bottom];
inf_tuples_in_sets(L1, L2, Mode) ->
  inf_tuples_in_sets(L1, L2, [], Mode).

inf_tuples_in_sets([?tuple(Elements1, Arity, Tag)|Left1], 
		   [?tuple(Elements2, Arity, Tag)|Left2], Acc, Mode) ->
  case t_inf_lists_strict(Elements1, Elements2, Mode) of
    bottom -> inf_tuples_in_sets(Left1, Left2, Acc, Mode);
    NewElements -> 
      inf_tuples_in_sets(Left1, Left2, [?tuple(NewElements, Arity, Tag)|Acc], Mode)
  end;
inf_tuples_in_sets([?tuple(_, _, Tag1)|Left1] = L1, 
		   [?tuple(_, _, Tag2)|Left2] = L2, Acc, Mode) ->
  if Tag1 < Tag2 -> inf_tuples_in_sets(Left1, L2, Acc, Mode);
     Tag1 > Tag2 -> inf_tuples_in_sets(L1, Left2, Acc, Mode)
  end;
inf_tuples_in_sets([], _, Acc, _Mode) -> lists:reverse(Acc);
inf_tuples_in_sets(_, [], Acc, _Mode) -> lists:reverse(Acc).

inf_union(U1, U2, opaque) ->
%%---------------------------------------------------------------------
%%                          Under Testing
%%----------------------------------------------------------------------
%%   OpaqueFun = 
%%     fun(Union1, Union2) ->
%% 	[_,_,_,_,_,_,_,_,Opaque,_] = Union1,
%% 	[A,B,F,I,L,N,T,M,_,_R] = Union2,
%% 	List = [A,B,F,I,L,N,T,M],
%%         case [T || T <- List, t_inf(T, Opaque, opaque) =/= ?none] of
%% 	  [] -> ?none;
%% 	  _  -> Opaque
%% 	end
%%     end,
%%   O1 = OpaqueFun(U1, U2),
%%   O2 = OpaqueFun(U2, U1),
%%   Union = inf_union(U1, U2, 0, [], opaque),
%%   t_sup([O1, O2, Union]);
  inf_union(U1, U2, 0, [], opaque);
inf_union(U1, U2, OtherMode) ->
  inf_union(U1, U2, 0, [], OtherMode).

inf_union([?none|Left1], [?none|Left2], N, Acc, Mode) ->
  inf_union(Left1, Left2, N, [?none|Acc], Mode);
inf_union([T1|Left1], [T2|Left2], N, Acc, Mode) ->
  case t_inf(T1, T2, Mode) of
    ?none -> inf_union(Left1, Left2, N, [?none|Acc], Mode);
    T     -> inf_union(Left1, Left2, N+1, [T|Acc], Mode)
  end;
inf_union([], [], N, Acc, _Mode) ->
  if N =:= 0 -> ?none;
     N =:= 1 ->
      [Type] = [T || T <- Acc, T =/= ?none],
      Type;
     N >= 2  -> ?union(lists:reverse(Acc))
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
      findfirst(N1, N2+1, U1, B1, U2, B2);
     Val1 < Val2 ->
      findfirst(N1+1, N2, U1, B1, U2, B2)
  end.

%%-----------------------------------------------------------------------------
%% Substitution of variables
%%

-spec t_subst(erl_type(), dict()) -> erl_type().

t_subst(T, Dict) ->
  case t_has_var(T) of
    true -> t_subst(T, Dict, fun(X) -> X end);
    false -> T
  end.

-spec subst_all_vars_to_any(erl_type()) -> erl_type().

subst_all_vars_to_any(T) ->
  case t_has_var(T) of
    true -> t_subst(T, dict:new(), fun(_) -> ?any end);
    false -> T
  end.

t_subst(?var(Id) = V, Dict, Fun) ->
  case dict:find(Id, Dict) of
    error -> Fun(V);
    {ok, Type} -> Type
  end;
t_subst(?list(Contents, Termination, Size), Dict, Fun) ->
  case t_subst(Contents, Dict, Fun) of
    ?none -> ?none;
    NewContents ->
      %% Be careful here to make the termination collapse if necessary.
      case t_subst(Termination, Dict, Fun) of
	?nil -> ?list(NewContents, ?nil, Size);
	?any -> ?list(NewContents, ?any, Size);
	Other ->
	  ?list(NewContents, NewTermination, _) = t_cons(NewContents, Other),
	  ?list(NewContents, NewTermination, Size)
      end
  end;
t_subst(?function(Domain, Range), Dict, Fun) ->
  ?function(t_subst(Domain, Dict, Fun), t_subst(Range, Dict, Fun));
t_subst(?product(Types), Dict, Fun) -> 
  ?product([t_subst(T, Dict, Fun) || T <- Types]);
t_subst(?tuple(?any, ?any, ?any) = T, _Dict, _Fun) ->
  T;
t_subst(?tuple(Elements, _Arity, _Tag), Dict, Fun) ->
  t_tuple([t_subst(E, Dict, Fun) || E <- Elements]);
t_subst(?tuple_set(_) = TS, Dict, Fun) ->
  t_sup([t_subst(T, Dict, Fun) || T <- t_tuple_subtypes(TS)]);
t_subst(T, _Dict, _Fun) -> 
  T.
	      
%%-----------------------------------------------------------------------------
%% Unification
%%

-type t_unify_ret() :: {erl_type(), [{_, erl_type()}]}.

-spec t_unify(erl_type(), erl_type()) -> t_unify_ret().

t_unify(T1, T2) ->
  t_unify(T1, T2, []).

-spec t_unify(erl_type(), erl_type(), [erl_type()]) -> t_unify_ret().

t_unify(T1, T2, Opaques) ->
  {T, Dict} = t_unify(T1, T2, dict:new(), Opaques),
  {t_subst(T, Dict), lists:keysort(1, dict:to_list(Dict))}.

t_unify(?var(Id) = T, ?var(Id), Dict, _Opaques) ->
  {T, Dict};
t_unify(?var(Id1) = T, ?var(Id2), Dict, Opaques) ->
  case dict:find(Id1, Dict) of
    error -> 
      case dict:find(Id2, Dict) of
	error -> {T, dict:store(Id2, T, Dict)};
	{ok, Type} -> t_unify(T, Type, Dict, Opaques)
      end;
    {ok, Type1} ->
      case dict:find(Id2, Dict) of
	error -> {Type1, dict:store(Id2, T, Dict)};
	{ok, Type2} -> t_unify(Type1, Type2, Dict, Opaques)
      end
  end;
t_unify(?var(Id), Type, Dict, Opaques) ->
  case dict:find(Id, Dict) of
    error -> {Type, dict:store(Id, Type, Dict)};
    {ok, VarType} -> t_unify(VarType, Type, Dict, Opaques)
  end;
t_unify(Type, ?var(Id), Dict, Opaques) ->
  case dict:find(Id, Dict) of
    error -> {Type, dict:store(Id, Type, Dict)};
    {ok, VarType} -> t_unify(VarType, Type, Dict, Opaques)
  end;
t_unify(?function(Domain1, Range1), ?function(Domain2, Range2), Dict, Opaques) ->
  {Domain, Dict1} = t_unify(Domain1, Domain2, Dict, Opaques),
  {Range, Dict2} = t_unify(Range1, Range2, Dict1, Opaques),
  {?function(Domain, Range), Dict2};
t_unify(?list(Contents1, Termination1, Size), 
	?list(Contents2, Termination2, Size), Dict, Opaques) ->
  {Contents, Dict1} = t_unify(Contents1, Contents2, Dict, Opaques),
  {Termination, Dict2} = t_unify(Termination1, Termination2, Dict1, Opaques),
  {?list(Contents, Termination, Size), Dict2};
t_unify(?product(Types1), ?product(Types2), Dict, Opaques) ->
  {Types, Dict1} = unify_lists(Types1, Types2, Dict, Opaques),
  {?product(Types), Dict1};
t_unify(?tuple(?any, ?any, ?any) = T, ?tuple(?any, ?any, ?any), Dict, _Opaques) ->
  {T, Dict};
t_unify(?tuple(Elements1, Arity, _), 
	?tuple(Elements2, Arity, _), Dict, Opaques) when Arity =/= ?any ->
  {NewElements, Dict1} = unify_lists(Elements1, Elements2, Dict, Opaques),
  {t_tuple(NewElements), Dict1};
t_unify(?tuple_set([{Arity, _}]) = T1, 
	?tuple(_, Arity, _) = T2, Dict, Opaques) when Arity =/= ?any ->
  unify_tuple_set_and_tuple(T1, T2, Dict, Opaques);
t_unify(?tuple(_, Arity, _) = T1,
	?tuple_set([{Arity, _}]) = T2, Dict, Opaques) when Arity =/= ?any ->
  unify_tuple_set_and_tuple(T2, T1, Dict, Opaques);
t_unify(?tuple_set(List1), ?tuple_set(List2), Dict, Opaques) ->
  {Tuples, NewDict} = 
    unify_lists(lists:append([T || {_Arity, T} <- List1]), 
		lists:append([T || {_Arity, T} <- List2]), Dict, Opaques),
  {t_sup(Tuples), NewDict};
t_unify(?opaque(Elements) = T, ?opaque(Elements), Dict, _Opaques) ->
  {T, Dict};
t_unify(?opaque(_) = T1, ?opaque(_) = T2, _Dict, _Opaques) ->
  throw({mismatch, T1, T2});
t_unify(Type, ?opaque(_) = OpType, Dict, Opaques) ->
  t_unify_with_opaque(Type, OpType, Dict, Opaques);
t_unify(?opaque(_) = OpType, Type, Dict, Opaques) ->
  t_unify_with_opaque(Type, OpType, Dict, Opaques);
t_unify(T, T, Dict, _Opaques) ->
  {T, Dict};
t_unify(T1, T2, _, _) ->
  throw({mismatch, T1, T2}).

t_unify_with_opaque(Type, OpType, Dict, Opaques) ->
  case lists:member(OpType, Opaques) of
    true ->
      Struct = t_opaque_structure(OpType),
      try t_unify(Type, Struct, Dict, Opaques) of
	{_T, Dict1} -> {OpType, Dict1}
      catch
	throw:{mismatch, _T1, _T2} ->
	  case t_inf(OpType, Type, opaque) of
	    ?none -> throw({mismatch, Type, OpType});
	    _ -> {OpType, Dict}
	  end
      end;
    false ->
      throw({mismatch, Type, OpType})
  end.

unify_tuple_set_and_tuple(?tuple_set([{Arity, List}]), 
			  ?tuple(Elements2, Arity, _), Dict, Opaques) ->
  %% Can only work if the single tuple has variables at correct places.
  %% Collapse the tuple set.
  {NewElements, Dict1} = unify_lists(sup_tuple_elements(List), Elements2, Dict, Opaques),
  {t_tuple(NewElements), Dict1}.

unify_lists(L1, L2, Dict, Opaques) ->
  unify_lists(L1, L2, Dict, [], Opaques).

unify_lists([T1|Left1], [T2|Left2], Dict, Acc, Opaques) ->
  {NewT, NewDict} = t_unify(T1, T2, Dict, Opaques),
  unify_lists(Left1, Left2, NewDict, [NewT|Acc], Opaques);
unify_lists([], [], Dict, Acc, _Opaques) ->
  {lists:reverse(Acc), Dict}.

%%t_assign_variables_to_subtype(T1, T2) ->
%%  try 
%%    Dict = assign_vars(T1, T2, dict:new()),
%%    {ok, dict:map(fun(_Param, List) -> t_sup(List) end, Dict)}
%%  catch
%%    throw:error -> error
%%  end.

%%assign_vars(_, ?var(_), _Dict) ->
%%  erlang:error("Variable in right hand side of assignment");
%%assign_vars(?any, _, Dict) ->
%%  Dict;
%%assign_vars(?var(_) = Var, Type, Dict) ->
%%  store_var(Var, Type, Dict);
%%assign_vars(?function(Domain1, Range1), ?function(Domain2, Range2), Dict) ->
%%  DomainList =
%%    case Domain2 of
%%      ?any -> [];
%%      ?product(List) -> List
%%    end,
%%  case any_none([Range2|DomainList]) of
%%    true -> throw(error);
%%    false ->
%%      Dict1 = assign_vars(Domain1, Domain2, Dict),
%%      assign_vars(Range1, Range2, Dict1)
%%  end;
%%assign_vars(?list(_Contents, _Termination, ?any), ?nil, Dict) ->
%%  Dict;
%%assign_vars(?list(Contents1, Termination1, Size1), 
%%	    ?list(Contents2, Termination2, Size2), Dict) ->
%%  Dict1 = assign_vars(Contents1, Contents2, Dict),
%%  Dict2 = assign_vars(Termination1, Termination2, Dict1),
%%  case {Size1, Size2} of
%%    {S, S} -> Dict2;
%%    {?any, ?nonempty_qual} -> Dict2;
%%    {_, _} -> throw(error)
%%  end;
%%assign_vars(?product(Types1), ?product(Types2), Dict) -> 
%%  case length(Types1) =:= length(Types2) of
%%    true -> assign_vars_lists(Types1, Types2, Dict);
%%    false -> throw(error)
%%  end;
%%assign_vars(?tuple(?any, ?any, ?any), ?tuple(?any, ?any, ?any), Dict) ->
%%  Dict;
%%assign_vars(?tuple(?any, ?any, ?any), ?tuple(_, _, _), Dict) ->
%%  Dict;
%%assign_vars(?tuple(Elements1, Arity, _), 
%%	    ?tuple(Elements2, Arity, _), Dict) when Arity =/= ?any ->
%%  assign_vars_lists(Elements1, Elements2, Dict);
%%assign_vars(?tuple_set(_) = T, ?tuple_set(List2), Dict) ->
%%  %% All Rhs tuples must already be subtypes of Lhs, so we can take
%%  %% each one separatly.
%%  assign_vars_lists([T || _ <- List2], List2, Dict);
%%assign_vars(?tuple(?any, ?any, ?any), ?tuple_set(_), Dict) ->
%%  Dict;
%%assign_vars(?tuple(_, Arity, _) = T1, ?tuple_set(List), Dict) ->
%%  case reduce_tuple_tags(List) of
%%    [Tuple = ?tuple(_, Arity, _)] -> assign_vars(T1, Tuple, Dict);
%%    _ -> throw(error)
%%  end;
%%assign_vars(?tuple_set(List), ?tuple(_, Arity, Tag) = T2, Dict) ->
%%  case [T || ?tuple(_, Arity1, Tag1) = T <- List, 
%%	     Arity1 =:= Arity, Tag1 =:= Tag] of
%%    [] -> throw(error);
%%    [T1] -> assign_vars(T1, T2, Dict)
%%  end;
%%assign_vars(?union(U1), T2, Dict) ->
%%  ?union(U2) = force_union(T2),
%%  assign_vars_lists(U1, U2, Dict);
%%assign_vars(T, T, Dict) ->
%%  Dict;
%%assign_vars(T1, T2, Dict) ->
%%  case t_is_subtype(T2, T1) of
%%    false -> throw(error);
%%    true -> Dict
%%  end.

%%assign_vars_lists([T1|Left1], [T2|Left2], Dict) ->
%%  assign_vars_lists(Left1, Left2, assign_vars(T1, T2, Dict));
%%assign_vars_lists([], [], Dict) ->
%%  Dict.

%%store_var(?var(Id), Type, Dict) ->
%%  case dict:find(Id, Dict) of
%%    error -> dict:store(Id, [Type], Dict);
%%    {ok, _VarType0} -> dict:update(Id, fun(X) -> [Type|X] end, Dict)
%%  end.

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

t_subtract(_, ?any) -> ?none;
t_subtract(?any, _) -> ?any;
t_subtract(T, ?unit) -> T;
t_subtract(?unit, _) -> ?unit;
t_subtract(?none, _) -> ?none;
t_subtract(T, ?none) -> T;
t_subtract(?atom(Set1), ?atom(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?atom(Set)
  end;
t_subtract(?bitstr(U1, B1), ?bitstr(U2, B2)) ->
  subtract_bin(t_bitstr(U1, B1), t_inf(t_bitstr(U1, B1), t_bitstr(U2, B2)));
t_subtract(?function(_, _) = T1, ?function(_, _) = T2) ->
  case t_is_subtype(T1, T2) of
    true -> ?none;
    false -> T1
  end;
t_subtract(?identifier(Set1), ?identifier(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?identifier(Set)
  end;
t_subtract(?opaque(Set1), ?opaque(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?opaque(Set)
  end;
t_subtract(?matchstate(Pres1, Slots1), ?matchstate(Pres2, _Slots2)) ->
  Pres = t_subtract(Pres1,Pres2),
  case t_is_none(Pres) of
    true -> ?none;
    false -> ?matchstate(Pres,Slots1)
  end;
t_subtract(?matchstate(Present,Slots),_) ->
  ?matchstate(Present,Slots);
t_subtract(?nil, ?nil) ->
  ?none;
t_subtract(?nil, ?nonempty_list(_, _)) ->
  ?nil;
t_subtract(?nil, ?list(_, _, _)) ->
  ?none;
t_subtract(?list(Contents, Termination, _Size) = T, ?nil) ->
  case Termination =:= ?nil of
    true -> ?nonempty_list(Contents, Termination);
    false -> T
  end;
t_subtract(?list(Contents1, Termination1, Size1) = T, 
	   ?list(Contents2, Termination2, Size2)) ->
  case t_is_subtype(Contents1, Contents2) of
    true ->
      case t_is_subtype(Termination1, Termination2) of
	true ->
	  case {Size1, Size2} of
	    {?nonempty_qual, ?unknown_qual} -> ?none;
	    {?unknown_qual, ?nonempty_qual} -> Termination1;
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
t_subtract(?float, ?float) -> ?none;
t_subtract(?number(_, _) = T1, ?float) -> t_inf(T1, t_integer());
t_subtract(?float, ?number(_Set, Tag)) ->
  case Tag of
    ?unknown_qual -> ?none;
    _ -> ?float
  end;
t_subtract(?number(_, _), ?number(?any, ?unknown_qual)) -> ?none;
t_subtract(?number(_, _) = T1, ?integer(?any)) -> t_inf(?float, T1);
t_subtract(?int_set(Set1), ?int_set(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?int_set(Set)
  end;
t_subtract(?int_range(From1, To1) = T1, ?int_range(_, _) = T2) ->
  case t_inf(T1, T2) of
    ?none -> T1;
    ?int_range(From1, To1) -> ?none;
    ?int_range(neg_inf, To) -> t_from_range(To + 1, To1);
    ?int_range(From, pos_inf) -> t_from_range(From1, From - 1);
    ?int_range(From, To) -> t_sup(t_from_range(From1, From - 1), 
				  t_from_range(To + 1, To))
  end;
t_subtract(?int_range(From, To) = T1, ?int_set(Set)) ->
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
t_subtract(?int_set(Set), ?int_range(From, To)) ->
  case set_filter(fun(X) -> not ((X =< From) orelse (X >= To)) end, Set) of
    ?none -> ?none;
    NewSet -> ?int_set(NewSet)
  end;
t_subtract(?integer(?any) = T1, ?integer(_)) -> T1;
t_subtract(?number(_, _) = T1, ?number(_, _)) -> T1;
t_subtract(?tuple(_, _, _), ?tuple(?any, ?any, ?any)) -> ?none;
t_subtract(?tuple_set(_), ?tuple(?any, ?any, ?any)) -> ?none;
t_subtract(?tuple(?any, ?any, ?any) = T1, ?tuple_set(_)) -> T1;
t_subtract(?tuple(Elements1, Arity1, _Tag1) = T1,
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
t_subtract(?tuple_set(List1) = T1, ?tuple(_, Arity, _) = T2) ->
  case orddict:find(Arity, List1) of
    error -> T1;
    {ok, List2} ->
      TuplesLeft0 = [Tuple || {_Arity, Tuple} <- orddict:erase(Arity, List1)],
      TuplesLeft1 = lists:append(TuplesLeft0),
      t_sup([t_subtract(L, T2) || L <- List2] ++ TuplesLeft1)
  end;
t_subtract(?tuple(_, Arity, _) = T1, ?tuple_set(List1)) ->
  case orddict:find(Arity, List1) of
    error -> T1;
    {ok, List2} -> t_inf([t_subtract(T1, L) || L <- List2])
  end;
t_subtract(?tuple_set(_) = T1, ?tuple_set(_) = T2) ->
  t_sup([t_subtract(T, T2) || T <- t_tuple_subtypes(T1)]);
t_subtract(?product(Elements1) = T1, ?product(Elements2)) ->
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
t_subtract(?product(P1), _) ->
  ?product(P1);
t_subtract(T, ?product(_)) ->
  T;
t_subtract(?union(U1), ?union(U2)) ->
  subtract_union(U1, U2);
t_subtract(T1, T2) ->  
  ?union(U1) = force_union(T1),
  ?union(U2) = force_union(T2),
  subtract_union(U1, U2).

-spec t_subtract_lists([erl_type()], [erl_type()]) -> [erl_type()].

t_subtract_lists(L1, L2) ->
  t_subtract_lists(L1, L2, []).

-spec t_subtract_lists([erl_type()], [erl_type()], [erl_type()]) -> [erl_type()].

t_subtract_lists([T1|Left1], [T2|Left2], Acc) ->
  t_subtract_lists(Left1, Left2, [t_subtract(T1, T2)|Acc]);
t_subtract_lists([], [], Acc) ->
  lists:reverse(Acc).

-spec subtract_union([erl_type(),...], [erl_type(),...]) -> erl_type().

subtract_union(U1, U2) ->
  subtract_union(U1, U2, 0, []).

-spec subtract_union([erl_type()], [erl_type()], non_neg_integer(), [erl_type()]) -> erl_type().

subtract_union([T1|Left1], [T2|Left2], N, Acc) ->
  case t_subtract(T1, T2) of
    ?none -> subtract_union(Left1, Left2, N, [?none|Acc]);
    T ->     subtract_union(Left1, Left2, N+1, [T|Acc])
  end;
subtract_union([], [], 0, _Acc) ->
  ?none;
subtract_union([], [], 1, Acc) ->
  [T] = [X || X <- Acc, X =/= ?none],
  T;
subtract_union([], [], N, Acc) when is_integer(N), N > 1 ->
  ?union(lists:reverse(Acc)).

replace_nontrivial_element(El1, El2) ->
  replace_nontrivial_element(El1, El2, []).

replace_nontrivial_element([T1|Left1], [?none|Left2], Acc) ->
  replace_nontrivial_element(Left1, Left2, [T1|Acc]);
replace_nontrivial_element([_|Left1], [T2|_], Acc) ->
  lists:reverse(Acc) ++ [T2|Left1].

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
  Inf = t_inf(T1, T2),
  t_is_equal(T1, Inf).

-spec t_is_instance(erl_type(), erl_type()) -> boolean().

t_is_instance(ConcreteType, Type) ->
  t_is_subtype(ConcreteType, t_unopaque(Type)).

-spec t_unopaque(erl_type()) -> erl_type().

t_unopaque(T) ->
  t_unopaque(T, 'universe').

-spec t_unopaque(erl_type(), 'universe' | [erl_type()]) -> erl_type().

t_unopaque(?opaque(_) = T, Opaques) ->
  case Opaques =:= universe orelse lists:member(T, Opaques) of
    true -> t_unopaque(t_opaque_structure(T), Opaques);
    false -> T  % XXX: needs revision for parametric opaque data types
  end;
t_unopaque(?list(ElemT, Termination, Sz), Opaques) ->
  ?list(t_unopaque(ElemT, Opaques), Termination, Sz);
t_unopaque(?tuple(?any, _, _) = T, _) -> T;
t_unopaque(?tuple(ArgTs, Sz, Tag), Opaques) when is_list(ArgTs) ->
  NewArgTs = [t_unopaque(A, Opaques) || A <- ArgTs],
  ?tuple(NewArgTs, Sz, Tag);
t_unopaque(?tuple_set(Set), Opaques) ->
  NewSet = [{Sz, [t_unopaque(T, Opaques) || T <- Tuples]}
	    || {Sz, Tuples} <- Set],
  ?tuple_set(NewSet);
t_unopaque(?union([A,B,F,I,L,N,T,M,O,R]), Opaques) ->
  UL = t_unopaque(L, Opaques),
  UT = t_unopaque(T, Opaques),
  UO = case O of
	 ?none -> [];
	 ?opaque(Os) -> [t_unopaque(S, Opaques) || #opaque{struct = S} <- Os]
       end,
  t_sup([?union([A,B,F,I,UL,N,UT,M,?none,R])|UO]);
t_unopaque(T, _) ->
  T.

%%-----------------------------------------------------------------------------
%% K-depth abstraction.
%%
%% t_limit/2 is the exported function, which checks the type of the
%% second argument and calls the module local t_limit_k/2 function.
%%

-spec t_limit(erl_type(), integer()) -> erl_type().

t_limit(Term, K) when is_integer(K) ->
  t_limit_k(Term, K).

t_limit_k(_, K) when K =< 0 -> ?any;
t_limit_k(?tuple(?any, ?any, ?any) = T, _K) -> T;
t_limit_k(?tuple(Elements, Arity, _), K) ->
  if K =:= 1 -> t_tuple(Arity);
     true -> t_tuple([t_limit_k(E, K-1) || E <- Elements])
  end;
t_limit_k(?tuple_set(_) = T, K) ->
  t_sup([t_limit_k(Tuple, K) || Tuple <- t_tuple_subtypes(T)]);
t_limit_k(?list(Elements, Termination, Size), K) ->
  NewTermination =
    if K =:= 1 ->
	%% We do not want to lose the termination information.
	t_limit_k(Termination, K);
       true -> t_limit_k(Termination, K - 1)
    end,
  NewElements = t_limit_k(Elements, K - 1),
  TmpList = t_cons(NewElements, NewTermination),
  case Size of
    ?nonempty_qual -> TmpList;
    ?unknown_qual -> 
      ?list(NewElements1, NewTermination1, _) = TmpList,
      ?list(NewElements1, NewTermination1, ?unknown_qual)
  end;
t_limit_k(?function(Domain, Range), K) ->
  %% The domain is either a product or any() so we do not decrease the K.
  ?function(t_limit_k(Domain, K), t_limit_k(Range, K-1));
t_limit_k(?product(Elements), K) ->
  ?product([t_limit_k(X, K - 1) || X <- Elements]);
t_limit_k(?union(Elements), K) ->
  ?union([t_limit_k(X, K) || X <- Elements]);
t_limit_k(T, _K) -> T.

%%============================================================================
%% 
%% Abstract records. Used for comparing contracts.
%%
%%============================================================================

-spec t_abstract_records(erl_type(), dict()) -> erl_type().

t_abstract_records(?list(Contents, Termination, Size), RecDict) ->
  case t_abstract_records(Contents, RecDict) of
    ?none -> ?none;
    NewContents ->
      %% Be careful here to make the termination collapse if necessary.
      case t_abstract_records(Termination, RecDict) of
	?nil -> ?list(NewContents, ?nil, Size);
	?any -> ?list(NewContents, ?any, Size);
	Other ->
	  ?list(NewContents, NewTermination, _) = t_cons(NewContents, Other),
	  ?list(NewContents, NewTermination, Size)
      end
  end;
t_abstract_records(?function(Domain, Range), RecDict) ->
  ?function(t_abstract_records(Domain, RecDict), 
	    t_abstract_records(Range, RecDict));
t_abstract_records(?product(Types), RecDict) -> 
  ?product([t_abstract_records(T, RecDict) || T <- Types]);
t_abstract_records(?union(Types), RecDict) -> 
  t_sup([t_abstract_records(T, RecDict) || T <- Types]);
t_abstract_records(?tuple(?any, ?any, ?any) = T, _RecDict) ->
  T;
t_abstract_records(?tuple(Elements, Arity, ?atom(_) = Tag), RecDict) ->
  [TagAtom] = t_atom_vals(Tag),
  case lookup_record(TagAtom, Arity - 1, RecDict) of
    error -> t_tuple([t_abstract_records(E, RecDict) || E <- Elements]);
    {ok, Fields} -> t_tuple([Tag|[T || {_Name, T} <- Fields]])
  end;
t_abstract_records(?tuple(Elements, _Arity, _Tag), RecDict) ->
  t_tuple([t_abstract_records(E, RecDict) || E <- Elements]);
t_abstract_records(?tuple_set(_) = Tuples, RecDict) ->
  t_sup([t_abstract_records(T, RecDict) || T <- t_tuple_subtypes(Tuples)]);
t_abstract_records(T, _RecDict) -> 
  T.

%% Map over types. Depth first. Used by the contract checker. ?list is
%% not fully implemented so take care when changing the type in Termination.

-spec t_map(fun((erl_type()) -> erl_type()), erl_type()) -> erl_type().

t_map(Fun, ?list(Contents, Termination, Size)) ->
  Fun(?list(t_map(Fun, Contents), t_map(Fun, Termination), Size));
t_map(Fun, ?function(Domain, Range)) ->
  Fun(?function(t_map(Fun, Domain), t_map(Fun, Range)));
t_map(Fun, ?product(Types)) -> 
  Fun(?product([t_map(Fun, T) || T <- Types]));
t_map(Fun, ?union(Types)) ->
  Fun(t_sup([t_map(Fun, T) || T <- Types]));
t_map(Fun, ?tuple(?any, ?any, ?any) = T) ->
  Fun(T);
t_map(Fun, ?tuple(Elements, _Arity, _Tag)) ->
  Fun(t_tuple([t_map(Fun, E) || E <- Elements]));
t_map(Fun, ?tuple_set(_) = Tuples) ->
  Fun(t_sup([t_map(Fun, T) || T <- t_tuple_subtypes(Tuples)]));
t_map(Fun, T) ->
  Fun(T).

%%=============================================================================
%%
%% Prettyprinter
%%
%%=============================================================================

-spec t_to_string(erl_type()) -> string().

t_to_string(T) ->
  t_to_string(T, dict:new()).

-spec t_to_string(erl_type(), dict()) -> string().

t_to_string(?any, _RecDict) ->
  "any()";
t_to_string(?none, _RecDict) ->
  "none()";
t_to_string(?unit, _RecDict) ->
  "no_return()";
t_to_string(?atom(?any), _RecDict) -> 
  "atom()";
t_to_string(?atom(Set), _RecDict) ->
  case set_size(Set) of
    2 ->
      case set_is_element(true, Set) andalso set_is_element(false, Set) of
	true -> "boolean()";
	false -> set_to_string(Set)
      end;
    _ ->
      set_to_string(Set)
  end;
t_to_string(?bitstr(8, 0), _RecDict) ->
  "binary()";
t_to_string(?bitstr(0, 0), _RecDict) ->
  "<<>>";
t_to_string(?bitstr(0, B), _RecDict) ->
  io_lib:format("<<_:~w>>", [B]);
t_to_string(?bitstr(U, 0), _RecDict) ->
  io_lib:format("<<_:_*~w>>", [U]);
t_to_string(?bitstr(U, B), _RecDict) ->
  io_lib:format("<<_:~w,_:_*~w>>", [B, U]);
t_to_string(?function(?any, ?any), _RecDict) ->
  "fun()";
t_to_string(?function(?any, Range), RecDict) ->
  "fun((...) -> " ++ t_to_string(Range, RecDict) ++ ")";
t_to_string(?function(?product(ArgList), Range), RecDict) ->
  "fun((" ++ comma_sequence(ArgList, RecDict) ++ ") -> "
    ++ t_to_string(Range, RecDict) ++ ")";
t_to_string(?identifier(Set), _RecDict) ->
  if Set =:= ?any -> "identifier()";
     true -> sequence([io_lib:format("~w()", [T]) 
		       || T <- set_to_list(Set)], [], " | ")
  end;
t_to_string(?opaque(Set), _RecDict) ->
  sequence([case is_opaque_builtin(Mod, Name) of
	      true  -> io_lib:format("~w()", [Name]);
	      false -> io_lib:format("~w:~w()", [Mod, Name])
	    end
	    || #opaque{mod = Mod, name = Name} <- set_to_list(Set)], [], " | ");
t_to_string(?matchstate(Pres, Slots), RecDict) ->
  io_lib:format("ms(~s,~s)", [t_to_string(Pres, RecDict),
			      t_to_string(Slots,RecDict)]);
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
	  erlang:error({illegal_list, ?nonempty_list(Contents, Termination)})
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
      case Contents =:= ?any of
	true -> ok;
	false ->
	  L = ?list(Contents, Termination, ?unknown_qual),
	  erlang:error({illegal_list, L})
      end,
      "maybe_improper_list()";
    _ -> 
      case t_is_subtype(t_nil(), Termination) of
	true ->
	  "maybe_improper_list("++ContentString++","
	    ++t_to_string(Termination, RecDict)++")";
	false ->
	  "improper_list("++ContentString++","
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
  lists:flatten(io_lib:format("~w..~w", [From, To]));
t_to_string(?integer(?any), _RecDict) -> "integer()";
t_to_string(?float, _RecDict) -> "float()";
t_to_string(?number(?any, ?unknown_qual), _RecDict) -> "number()";
t_to_string(?product(List), RecDict) -> 
  "<" ++ comma_sequence(List, RecDict) ++ ">";
t_to_string(?remote(Set), RecDict) ->
  sequence([case Args =:= [] of
	      true  -> io_lib:format("~w:~w()", [Mod, Name]);
	      false ->
		ArgString = comma_sequence(Args, RecDict),
		io_lib:format("~w:~w(~s)", [Mod, Name, ArgString])
	    end
	    || #remote{mod = Mod, name = Name, args = Args} <- set_to_list(Set)],
	   [], " | ");
t_to_string(?tuple(?any, ?any, ?any), _RecDict) -> "tuple()";
t_to_string(?tuple(Elements, _Arity, ?any), RecDict) ->   
  "{" ++ comma_sequence(Elements, RecDict) ++ "}";
t_to_string(?tuple(Elements, Arity, Tag), RecDict) ->
  [TagAtom] = t_atom_vals(Tag),
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
  io_lib:format("~s", [atom_to_list(Id)]);
t_to_string(?var(Id), _RecDict) when is_integer(Id) ->
  io_lib:format("var(~w)", [Id]).

record_to_string(Tag, [_|Fields], FieldNames, RecDict) ->
  FieldStrings = record_fields_to_string(Fields, FieldNames, RecDict, []),
  "#" ++ atom_to_list(Tag) ++ "{" ++ sequence(FieldStrings, [], ",") ++ "}".

record_fields_to_string([Field|Left1], [{FieldName, DeclaredType}|Left2], 
			RecDict, Acc) ->
  PrintType =
    case t_is_equal(Field, DeclaredType) of
      true -> false;
      false ->
	case t_is_any(DeclaredType) andalso t_is_atom(undefined, Field) of
	  true -> false;
	  false ->
	    TmpType = t_subtract(DeclaredType, t_atom(undefined)),
	    not t_is_equal(Field, TmpType)
	end
    end,
  case PrintType of
    false -> record_fields_to_string(Left1, Left2, RecDict, Acc);
    true ->
      String = atom_to_list(FieldName) ++ "::" ++ t_to_string(Field, RecDict),
      record_fields_to_string(Left1, Left2, RecDict, [String|Acc])
  end;
record_fields_to_string([], [], _RecDict, Acc) ->
  lists:reverse(Acc).

comma_sequence(Types, RecDict) ->
  List = [case T =:= ?any of
	    true -> "_";
	    false -> t_to_string(T, RecDict)
	  end || T <- Types],
  sequence(List, ",").

union_sequence(Types, RecDict) ->
  List = [t_to_string(T, RecDict) || T <- Types], 
  sequence(List, " | ").

sequence(List, Delimiter) ->
  sequence(List, [], Delimiter).

sequence([], [], _Delimiter) ->
  [];
sequence([T], Acc, _Delimiter) ->
  lists:flatten(lists:reverse([T|Acc]));
sequence([T|Ts], Acc, Delimiter) ->
  sequence(Ts, [T ++ Delimiter|Acc], Delimiter).

%%=============================================================================
%% 
%% Build a type from parse forms.
%%
%%=============================================================================

-spec t_from_form(parse_form()) -> erl_type().

t_from_form(Form) ->
  t_from_form(Form, dict:new()).

-spec t_from_form(parse_form(), dict()) -> erl_type().

t_from_form(Form, RecDict) ->
  t_from_form(Form, RecDict, dict:new()).

-spec t_from_form(parse_form(), dict(), dict()) -> erl_type().

t_from_form(Form, RecDict, VarDict) ->
  {T, _R} = t_from_form(Form, [], RecDict, VarDict),
  T.

-type type_names() :: [{'type' | 'opaque' | 'record', atom()}].
-spec t_from_form(parse_form(), type_names(), dict(), dict()) ->
                     {erl_type(), type_names()}.

t_from_form({var, _L, '_'}, _TypeNames, _RecDict, _VarDict) ->
  {t_any(), []};
t_from_form({var, _L, Name}, _TypeNames, _RecDict, VarDict) ->
  case dict:find(Name, VarDict) of
    error -> {t_var(Name), []};
    {ok, Val} -> {Val, []}
  end;
t_from_form({ann_type, _L, [_Var, Type]}, TypeNames, RecDict, VarDict) ->
  t_from_form(Type, TypeNames, RecDict, VarDict);
t_from_form({paren_type, _L, [Type]}, TypeNames, RecDict, VarDict) ->
  t_from_form(Type, TypeNames, RecDict, VarDict);
t_from_form({remote_type, _L, [{atom, _, Module}, {atom, _, Type}, Args]},
	    TypeNames, RecDict, VarDict) ->
  {L, R} = list_from_form(Args, TypeNames, RecDict, VarDict),
  {t_remote(Module, Type, L), R};
t_from_form({atom, _L, Atom}, _TypeNames, _RecDict, _VarDict) ->
  {t_atom(Atom), []};
t_from_form({integer, _L, Int}, _TypeNames, _RecDict, _VarDict) ->
  {t_integer(Int), []};
t_from_form({type, _L, any, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_any(), []};
t_from_form({type, _L, arity, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_arity(), []};
t_from_form({type, _L, array, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_array(), []};
t_from_form({type, _L, atom, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_atom(), []};
t_from_form({type, _L, binary, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_binary(), []};
t_from_form({type, _L, binary, [{integer, _, Base}, {integer, _, Unit}]}, 
	    _TypeNames, _RecDict, _VarDict) ->
  {t_bitstr(Unit, Base), []};
t_from_form({type, _L, bitstring, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_bitstr(), []};
t_from_form({type, _L, bool, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_boolean(), []};	% XXX: Temporarily
t_from_form({type, _L, boolean, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_boolean(), []};
t_from_form({type, _L, byte, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_byte(), []};
t_from_form({type, _L, char, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_char(), []};
t_from_form({type, _L, dict, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_dict(), []};
t_from_form({type, _L, digraph, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_digraph(), []};
t_from_form({type, _L, float, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_float(), []};
t_from_form({type, _L, function, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_fun(), []};
t_from_form({type, _L, 'fun', []}, _TypeNames, _RecDict, _VarDict) ->
  {t_fun(), []};
t_from_form({type, _L, 'fun', [{type, _, any, []}, Range]}, TypeNames,
            RecDict, VarDict) ->
  {T, R} = t_from_form(Range, TypeNames, RecDict, VarDict),
  {t_fun(T), R};
t_from_form({type, _L, 'fun', [{type, _, product, Domain}, Range]},
            TypeNames, RecDict, VarDict) ->
  {L, R1} = list_from_form(Domain, TypeNames, RecDict, VarDict),
  {T, R2} = t_from_form(Range, TypeNames, RecDict, VarDict),
  {t_fun(L, T), R1 ++ R2};
t_from_form({type, _L, gb_set, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_gb_set(), []};
t_from_form({type, _L, gb_tree, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_gb_tree(), []};
t_from_form({type, _L, identifier, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_identifier(), []};
t_from_form({type, _L, integer, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_integer(), []};
t_from_form({type, _L, iodata, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_iodata(), []};
t_from_form({type, _L, iolist, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_iolist(), []};
t_from_form({type, _L, list, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_list(), []};
t_from_form({type, _L, list, [Type]}, TypeNames, RecDict, VarDict) ->
  {T, R} = t_from_form(Type, TypeNames, RecDict, VarDict),
  {t_list(T), R};
t_from_form({type, _L, mfa, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_mfa(), []};
t_from_form({type, _L, module, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_module(), []};
t_from_form({type, _L, nil, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_nil(), []};
t_from_form({type, _L, neg_integer, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_neg_integer(), []};
t_from_form({type, _L, non_neg_integer, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_non_neg_integer(), []};
t_from_form({type, _L, no_return, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_unit(), []};
t_from_form({type, _L, node, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_node(), []};
t_from_form({type, _L, none, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_none(), []};
t_from_form({type, _L, nonempty_list, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_nonempty_list(), []};
t_from_form({type, _L, nonempty_list, [Type]}, TypeNames, RecDict, VarDict) ->
  {T, R} = t_from_form(Type, TypeNames, RecDict, VarDict),
  {t_nonempty_list(T), R};
t_from_form({type, _L, nonempty_improper_list, [Cont, Term]}, TypeNames,
            RecDict, VarDict) ->
  {T1, R1} = t_from_form(Cont, TypeNames, RecDict, VarDict),
  {T2, R2} = t_from_form(Term, TypeNames, RecDict, VarDict),
  {t_cons(T1, T2), R1 ++ R2};
t_from_form({type, _L, nonempty_maybe_improper_list, []}, _TypeNames,
            _RecDict, _VarDict) ->
  {t_cons(?any, ?any), []};
t_from_form({type, _L, nonempty_maybe_improper_list, [Cont, Term]}, TypeNames,
            RecDict, VarDict) ->
  {T1, R1} = t_from_form(Cont, TypeNames, RecDict, VarDict),
  {T2, R2} = t_from_form(Term, TypeNames, RecDict, VarDict),
  {t_cons(T1, T2), R1 ++ R2};
t_from_form({type, _L, nonempty_string, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_nonempty_string(), []};
t_from_form({type, _L, number, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_number(), []};
t_from_form({type, _L, pid, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_pid(), []};
t_from_form({type, _L, port, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_port(), []};
t_from_form({type, _L, pos_integer, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_pos_integer(), []};
t_from_form({type, _L, maybe_improper_list, []}, _TypeNames, _RecDict,
            _VarDict) ->
  {t_maybe_improper_list(), []};
t_from_form({type, _L, maybe_improper_list, [Content, Termination]}, TypeNames,
            RecDict, VarDict) ->
  {T1, R1} = t_from_form(Content, TypeNames, RecDict, VarDict),
  {T2, R2} = t_from_form(Termination, TypeNames, RecDict, VarDict),
  {t_maybe_improper_list(T1, T2), R1 ++ R2};
t_from_form({type, _L, product, Elements}, TypeNames, RecDict, VarDict) ->
  {L, R} = list_from_form(Elements, TypeNames, RecDict, VarDict),
  {t_product(L), R};
t_from_form({type, _L, queue, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_queue(), []};
t_from_form({type, _L, range, [{integer, _, From}, {integer, _, To}]}, 
	    _TypeNames, _RecDict, _VarDict) ->
  {t_from_range(From, To), []};
t_from_form({type, _L, record, [Name|Fields]}, TypeNames, RecDict, VarDict) ->
  record_from_form(Name, Fields, TypeNames, RecDict, VarDict);
t_from_form({type, _L, reference, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_reference(), []};
t_from_form({type, _L, set, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_set(), []};
t_from_form({type, _L, string, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_string(), []};
t_from_form({type, _L, term, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_any(), []};
t_from_form({type, _L, tid, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_tid(), []};
t_from_form({type, _L, timeout, []}, _TypeNames, _RecDict, _VarDict) ->
  {t_timeout(), []};
t_from_form({type, _L, tuple, any}, _TypeNames, _RecDict, _VarDict) ->
  {t_tuple(), []};
t_from_form({type, _L, tuple, Args}, TypeNames, RecDict, VarDict) ->
  {L, R} = list_from_form(Args, TypeNames, RecDict, VarDict),
  {t_tuple(L), R};
t_from_form({type, _L, union, Args}, TypeNames, RecDict, VarDict) ->
  {L, R} = list_from_form(Args, TypeNames, RecDict, VarDict),
  {t_sup(L), R};
t_from_form({type, _L, Name, Args}, TypeNames, RecDict, VarDict) ->
  case lookup_type(Name, RecDict) of
    {type, {_Module, Type, ArgNames}} when length(Args) =:= length(ArgNames) ->
      case unfold({type, Name}, TypeNames) of
        true ->
          List = lists:zipwith(
                   fun(ArgName, ArgType) ->
                       {Ttemp, _R} = t_from_form(ArgType, TypeNames,
                                                     RecDict, VarDict),
                       {ArgName, Ttemp}
                   end,
                   ArgNames, Args),
          TmpVarDict = dict:from_list(List),
          {T, R} = t_from_form(Type, [{type, Name}|TypeNames], RecDict,
                               TmpVarDict),
          case lists:member({type, Name}, R) of
            true -> {t_limit(T, ?REC_TYPE_LIMIT), R};
            false -> {T, R}
          end;
        false -> {t_any(), [{type, Name}]}
      end;
    {opaque, {Module, Type, ArgNames}} when length(Args) =:= length(ArgNames) ->
      {Rep, Rret} =
        case unfold({opaque, Name}, TypeNames) of
          true ->
            List = lists:zipwith(
                     fun(ArgName, ArgType) ->
                         {Ttemp, _R} =  t_from_form(ArgType, TypeNames,
                                                    RecDict, VarDict),
                         {ArgName, Ttemp}
                     end,
                     ArgNames, Args),
            TmpVarDict = dict:from_list(List),
            {T, R} = t_from_form(Type, [{opaque, Name}|TypeNames], RecDict,
                                 TmpVarDict),
            case lists:member({opaque, Name}, R) of
              true -> {t_limit(T, ?REC_TYPE_LIMIT), R};
              false -> {T, R}
            end;
          false -> {t_any(), [{opaque, Name}]}
        end,
      Tret = t_from_form({opaque, -1, Name, {Module, Args, Rep}},
                         RecDict, VarDict),
      {Tret, Rret};
    {type, _} ->
      throw({error, io_lib:format("Unknown type ~w\n", [Name])});
    {opaque, _} ->
      throw({error, io_lib:format("Unknown opaque type ~w\n", [Name])});
    error ->
      throw({error, io_lib:format("Unable to find type ~w\n", [Name])}) 
  end;
t_from_form({opaque, _L, Name, {Mod, Args, Rep}}, _TypeNames, _RecDict,
            _VarDict) ->
  case Args of
    [] -> {t_opaque(Mod, Name, Args, Rep), []};
    _ -> throw({error, "Polymorphic opaque types not supported yet"})
  end.

record_from_form({atom, _, Name}, ModFields, TypeNames, RecDict, VarDict) ->
  case unfold({record, Name}, TypeNames) of
    true ->
      case lookup_record(Name, RecDict) of
        {ok, DeclFields} ->
          TypeNames1 = [{record, Name}|TypeNames],
          AreTyped = [is_erl_type(FieldType)
                      || {_FieldName, FieldType} <- DeclFields],
          {DeclFields1, R1} =
            case lists:all(fun(Elem) -> Elem end, AreTyped) of
              true -> {DeclFields, []};
              false -> fields_from_form(DeclFields, TypeNames1,
                                        RecDict, dict:new())
            end,
          {GetModRec, R2} = get_mod_record(ModFields, DeclFields1,
                                           TypeNames1, RecDict, VarDict),
          case GetModRec of
            {error, FieldName} ->
              throw({error, io_lib:format("Illegal declaration of ~w#{~w}\n",
                                          [Name, FieldName])});
            {ok, NewFields} ->
              {t_tuple(
                 [t_atom(Name)|[Type || {_FieldName, Type} <- NewFields]]),
               R1 ++ R2}
          end;
        error ->
          throw({error, erlang:error(io_lib:format("Unknown record #~w{}\n",
                                                   [Name]))})
      end;
    false -> {t_any(), []}
  end.

get_mod_record([], DeclFields, _TypeNames, _RecDict, _VarDict) ->
  {{ok, DeclFields}, []};
get_mod_record(ModFields, DeclFields, TypeNames, RecDict, VarDict) ->
  DeclFieldsDict = orddict:from_list(DeclFields),
  {ModFieldsDict, R} = build_field_dict(ModFields, TypeNames,
                                        RecDict, VarDict),
  case get_mod_record(DeclFieldsDict, ModFieldsDict, []) of
    {error, _FieldName} = Error -> {Error, R};
    {ok, FinalOrdDict} ->
      {{ok, [{FieldName, orddict:fetch(FieldName, FinalOrdDict)}
             || {FieldName, _} <- DeclFields]},
       R}
  end.

build_field_dict(FieldTypes, TypeNames, RecDict, VarDict) ->
  build_field_dict(FieldTypes, TypeNames, RecDict, VarDict, []).

build_field_dict([{type, _, field_type, [{atom, _, Name}, Type]}|Left], 
		 TypeNames, RecDict, VarDict, Acc) ->
  {T, R1} = t_from_form(Type, TypeNames, RecDict, VarDict),
  NewAcc = [{Name, T}|Acc],
  {D, R2} = build_field_dict(Left, TypeNames, RecDict, VarDict, NewAcc),
  {D, R1 ++ R2};
build_field_dict([], _TypeNames, _RecDict, _VarDict, Acc) ->
  {orddict:from_list(Acc), []}.

get_mod_record([{FieldName, DeclType}|Left1], 
	       [{FieldName, ModType}|Left2], Acc) ->
  case t_is_var(ModType) orelse t_is_subtype(ModType, DeclType) of
    false -> {error, FieldName};
    true -> get_mod_record(Left1, Left2, [{FieldName, ModType}|Acc])
  end;
get_mod_record([{FieldName1, _DeclType} = DT|Left1], 
	       [{FieldName2, _ModType}|_] = List2, 
	       Acc) when FieldName1 < FieldName2 ->
  get_mod_record(Left1, List2, [DT|Acc]);
get_mod_record(DeclFields, [], Acc) ->
  {ok, orddict:from_list(Acc ++ DeclFields)};
get_mod_record(_, [{FieldName2, _ModType}|_], _Acc) ->
  {error, FieldName2}.

fields_from_form([], _TypeNames, _RecDict, _VarDict) ->
  {[], []};
fields_from_form([{Name, Type}|Tail], TypeNames, RecDict, VarDict) ->
  {T, R1} = t_from_form(Type, TypeNames, RecDict, VarDict),
  {F, R2} = fields_from_form(Tail, TypeNames, RecDict, VarDict),
  {[{Name, T}|F], R1 ++ R2}.

list_from_form([], _TypeNames, _RecDict, _VarDict) ->
  {[], []};
list_from_form([H|Tail], TypeNames, RecDict, VarDict) ->
  {T, R1} = t_from_form(H, TypeNames, RecDict, VarDict),
  {L, R2} = list_from_form(Tail, TypeNames, RecDict, VarDict),
  {[T|L], R1 ++ R2}.

-spec t_form_to_string(parse_form()) -> string().

t_form_to_string({var, _L, '_'}) -> "_";
t_form_to_string({var, _L, Name}) -> atom_to_list(Name);
t_form_to_string({atom, _L, Atom}) -> 
  io_lib:write_string(atom_to_list(Atom), $'); % To quote or not to quote... '
t_form_to_string({integer, _L, Int}) -> integer_to_list(Int);
t_form_to_string({ann_type, _L, [Var, Type]}) ->
  t_form_to_string(Var) ++ "::" ++ t_form_to_string(Type);
t_form_to_string({paren_type, _L, [Type]}) ->
  io_lib:format("(~s)", [t_form_to_string(Type)]);
t_form_to_string({remote_type, _L, [{atom, _, Mod}, {atom, _, Name}, Args]}) ->
  ArgString = "(" ++ sequence(t_form_to_string_list(Args), ",") ++ ")",
  io_lib:format("~w:~w", [Mod, Name]) ++ ArgString;
t_form_to_string({type, _L, arity, []}) -> "arity()";
t_form_to_string({type, _L, 'fun', []}) -> "fun()";
t_form_to_string({type, _L, 'fun', [{type, _, any, []}, Range]}) -> 
  "fun(...) -> " ++ t_form_to_string(Range);
t_form_to_string({type, _L, 'fun', [{type, _, product, Domain}, Range]}) ->
  "fun((" ++ sequence(t_form_to_string_list(Domain), ",") ++ ") -> " 
    ++ t_form_to_string(Range) ++ ")";
t_form_to_string({type, _L, iodata, []}) -> "iodata()";
t_form_to_string({type, _L, iolist, []}) -> "iolist()";
t_form_to_string({type, _L, list, [Type]}) -> 
  "[" ++ t_form_to_string(Type) ++ "]";
t_form_to_string({type, _L, mfa, []}) -> "mfa()";
t_form_to_string({type, _L, module, []}) -> "module()";
t_form_to_string({type, _L, node, []}) -> "node()";
t_form_to_string({type, _L, nonempty_list, [Type]}) ->
  "[" ++ t_form_to_string(Type) ++ ",...]";
t_form_to_string({type, _L, nonempty_string, []}) -> "nonempty_string()";
t_form_to_string({type, _L, product, Elements}) ->
  "<" ++ sequence(t_form_to_string_list(Elements), ",") ++ ">";
t_form_to_string({type, _L, range, [{integer, _, From}, {integer, _, To}]}) ->
  io_lib:format("~w..~w", [From, To]);
t_form_to_string({type, _L, record, [{atom, _, Name}]}) ->
  io_lib:format("#~w{}", [Name]);
t_form_to_string({type, _L, record, [{atom, _, Name}|Fields]}) ->
  FieldString = sequence(t_form_to_string_list(Fields), ","),
  io_lib:format("#~w{~s}", [Name, FieldString]);
t_form_to_string({type, _L, field_type, [{atom, _, Name}, Type]}) ->
  io_lib:format("~w::~s", [Name, t_form_to_string(Type)]);
t_form_to_string({type, _L, term, []}) -> "term()";
t_form_to_string({type, _L, timeout, []}) -> "timeout()";
t_form_to_string({type, _L, tuple, any}) -> "tuple()";
t_form_to_string({type, _L, tuple, Args}) ->
  "{" ++ sequence(t_form_to_string_list(Args), ",") ++ "}";
t_form_to_string({type, _L, union, Args}) ->
  sequence(t_form_to_string_list(Args), " | ");
t_form_to_string({type, _L, Name, []} = T) ->
  try t_to_string(t_from_form(T))
  catch throw:{error, _} -> atom_to_list(Name) ++ "()"
  end;
t_form_to_string({type, _L, binary, [{integer, _, X}, {integer, _, Y}]}) ->
  case Y of
    0 ->
      case X of
	0 -> "<<>>";
	_ -> io_lib:format("<<_:~w>>", [X])
      end
  end;
t_form_to_string({type, _L, Name, List}) -> 
  io_lib:format("~w(~s)", [Name, sequence(t_form_to_string_list(List), ",")]).

t_form_to_string_list(List) ->
  t_form_to_string_list(List, []).

t_form_to_string_list([H|T], Acc) ->
  t_form_to_string_list(T, [t_form_to_string(H)|Acc]);
t_form_to_string_list([], Acc) ->
  lists:reverse(Acc).  
  
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

-spec lookup_record(atom(), dict()) ->
        'error' | {'ok', [{atom(), parse_form() | erl_type()}]}.

lookup_record(Tag, RecDict) when is_atom(Tag) ->
  case dict:find({record, Tag}, RecDict) of
    {ok, [{_Arity, Fields}]} ->
      {ok, Fields};
    {ok, List} when is_list(List) ->
      %% This will have to do, since we do not know which record we
      %% are looking for.
      error;
    error ->
      error
  end.

-spec lookup_record(atom(), arity(), dict()) -> 'error' | {'ok', [{atom(), erl_type()}]}.

lookup_record(Tag, Arity, RecDict) when is_atom(Tag) ->
  case dict:find({record, Tag}, RecDict) of
    {ok, [{Arity, Fields}]} -> {ok, Fields};
    {ok, OrdDict} -> orddict:find(Arity, OrdDict);
    error -> error
  end.

lookup_type(Name, RecDict) ->
  case dict:find({type, Name}, RecDict) of
    error ->
      case dict:find({opaque, Name}, RecDict) of
	error -> error;
	{ok, Found} -> {opaque, Found}
      end;
    {ok, Found} -> {type, Found}
  end.

-spec type_is_defined('type' | 'opaque', atom(), dict()) -> boolean().

type_is_defined(TypeOrOpaque, Name, RecDict) ->
  dict:is_key({TypeOrOpaque, Name}, RecDict).

unfold(TypeName, TypeNames) ->
  not lists:member(TypeName, TypeNames).

%% -----------------------------------
%% Set
%%

set_singleton(Element) ->
  ordsets:from_list([Element]).

set_is_singleton(Element, Set) ->
  set_singleton(Element) =:= Set.
  
set_is_element(Element, Set) ->
  ordsets:is_element(Element, Set).

set_union(?any, _) -> ?any;
set_union(_, ?any) -> ?any;
set_union(S1, S2)  -> 
  case ordsets:union(S1, S2) of
    S when length(S) =< ?SET_LIMIT -> S;
    _ -> ?any
  end.

set_union_no_limit(?any, _) -> ?any;
set_union_no_limit(_, ?any) -> ?any;
set_union_no_limit(S1, S2)  -> ordsets:union(S1, S2).

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

set_to_list(Set) ->
  ordsets:to_list(Set).

set_filter(Fun, Set) ->
  case ordsets:filter(Fun, Set) of
    [] -> ?none;
    NewSet -> NewSet
  end.

set_size(Set) ->
  ordsets:size(Set).

set_to_string(Set) ->
  L = [case is_atom(X) of
	 true -> io_lib:write_string(atom_to_list(X), $'); % stupid emacs '
	 false -> io_lib:format("~w", [X])
       end || X <- set_to_list(Set)],
  sequence(L, [], " | ").

set_min([H|_]) -> H.

set_max(Set) ->
  hd(lists:reverse(Set)).

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
%% Consistency-testing function(s) below
%%=============================================================================

-ifdef(DO_ERL_TYPES_TEST).		  

test() ->
  Atom1  = t_atom(),
  Atom2  = t_atom(foo),
  Atom3  = t_atom(bar),
  true   = t_is_atom(Atom2),

  True   = t_atom(true),
  False  = t_atom(false),
  Bool   = t_boolean(),
  true   = t_is_boolean(True),
  true   = t_is_boolean(Bool),
  false  = t_is_boolean(Atom1),

  Binary = t_binary(),
  true   = t_is_binary(Binary),

  Bitstr = t_bitstr(),
  true   = t_is_bitstr(Bitstr),
  
  Bitstr1 = t_bitstr(7, 3),
  true   = t_is_bitstr(Bitstr1),
  false  = t_is_binary(Bitstr1),

  Bitstr2 = t_bitstr(16, 8),
  true   = t_is_bitstr(Bitstr2),
  true   = t_is_binary(Bitstr2),
  
  ?bitstr(8, 16) = t_subtract(t_bitstr(4, 12), t_bitstr(8, 12)),
  ?bitstr(8, 16) = t_subtract(t_bitstr(4, 12), t_bitstr(8, 12)),

  Int1   = t_integer(),
  Int2   = t_integer(1),
  Int3   = t_integer(16#ffffffff),
  true   = t_is_integer(Int2),
  true   = t_is_byte(Int2),
  false  = t_is_byte(Int3),
  false  = t_is_byte(t_from_range(-1, 1)),
  true   = t_is_byte(t_from_range(1, ?MAX_BYTE)),
  
  Tuple1 = t_tuple(),
  Tuple2 = t_tuple(3),
  Tuple3 = t_tuple([Atom1, Int1]),
  Tuple4 = t_tuple([Tuple1, Tuple2]),
  Tuple5 = t_tuple([Tuple3, Tuple4]),
  Tuple6 = t_limit(Tuple5, 2),
  Tuple7 = t_limit(Tuple5, 3),
  true   = t_is_tuple(Tuple1),  
  
  Port   = t_port(),
  Pid    = t_pid(),
  Ref    = t_reference(),
  Identifier = t_identifier(),
  false  = t_is_reference(Port),
  true   = t_is_identifier(Port),

  Function1 = t_fun(),
  Function2 = t_fun(Pid),
  Function3 = t_fun([], Pid),
  Function4 = t_fun([Port, Pid], Pid),
  Function5 = t_fun([Pid, Atom1], Int2),
  true      = t_is_fun(Function3),  

  List1 = t_list(),
  List2 = t_list(t_boolean()),
  List3 = t_cons(t_boolean(), List2),
  List4 = t_cons(t_boolean(), t_atom()),
  List5 = t_cons(t_boolean(), t_nil()),
  List6 = t_cons_tl(List5),
  List7 = t_sup(List4, List5),
  List8 = t_inf(List7, t_list()),
  List9 = t_cons(),
  List10 = t_cons_tl(List9),
  true  = t_is_boolean(t_cons_hd(List5)),
  true  = t_is_list(List5),
  false = t_is_list(List4),

  Product1 = t_product([Atom1, Atom2]),
  Product2 = t_product([Atom3, Atom1]),
  Product3 = t_product([Atom3, Atom2]),

  Union1 = t_sup(Atom2, Atom3),
  Union2 = t_sup(Tuple2, Tuple3),
  Union3 = t_sup(Int2, Atom3),
  Union4 = t_sup(Port, Pid),
  Union5 = t_sup(Union4, Int1),
  Union6 = t_sup(Function1, Function2),
  Union7 = t_sup(Function4, Function5),
  Union8 = t_sup(True, False),
  true   = t_is_boolean(Union8),
  Union9 = t_sup(Int2, t_integer(2)),
  true   = t_is_byte(Union9),
  Union10 = t_sup(t_tuple([t_atom(true), ?any]), 
		  t_tuple([t_atom(false), ?any])),
  
  ?any   = t_sup(Product3, Function5),

  Atom3  = t_inf(Union3, Atom1),
  Union2 = t_inf(Union2, Tuple1),
  Int2   = t_inf(Int1, Union3),
  Union4 = t_inf(Union4, Identifier),
  Port   = t_inf(Union5, Port),
  Function4 = t_inf(Union7, Function4),
  ?none  = t_inf(Product2, Atom1),
  Product3 = t_inf(Product1, Product2),
  Function5 = t_inf(Union7, Function5),
  true   = t_is_byte(t_inf(Union9, t_number())),
  true   = t_is_char(t_inf(Union9, t_number())),

  io:format("3? ~p ~n", [?int_set([3])]),

  RecDict = dict:store({foo, 2}, [bar, baz], dict:new()),
  Record1 = t_from_term({foo, [1,2], {1,2,3}}),
  
  Types = [
	   Atom1,
	   Atom2,
	   Atom3,
	   Binary,
	   Int1,
	   Int2,
	   Tuple1,
	   Tuple2,
	   Tuple3,
	   Tuple4,
	   Tuple5,
	   Tuple6,
	   Tuple7,
	   Ref,
	   Port,
	   Pid,
	   Identifier,
	   List1,
	   List2,
	   List3,
	   List4,
	   List5,
	   List6,
	   List7,
	   List8,
	   List9,
	   List10,
	   Function1,
	   Function2,
	   Function3,
	   Function4,
	   Function5,
	   Product1,
	   Product2,
	   Record1,
	   Union1,
	   Union2,
	   Union3,
	   Union4,
	   Union5,
	   Union6,
	   Union7,
	   Union8,
	   Union10,
	   t_inf(Union10, t_tuple([t_atom(true), t_integer()]))
	  ],
  io:format("~p\n", [[t_to_string(X, RecDict) || X <- Types]]).
       
-endif.
