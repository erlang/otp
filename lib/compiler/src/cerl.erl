%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 1999-2002 Richard Carlsson
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
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
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%%

-module(cerl).
-moduledoc """
Core Erlang abstract syntax trees.

> #### Note {: .info }
>
> The public interface of the Erlang compiler can be found in
> module `m:compile`.
>
> This module is an internal part of the compiler. Its API is not guaranteed
> to remain compatible between releases.

This module defines an abstract data type for representing Core Erlang source
code as syntax trees.

A recommended starting point for the first-time user is the documentation of the
function `type/1`.

> #### Note {: .info }
>
> This module deals with the composition and decomposition of _syntactic_ entities
> (as opposed to semantic ones); its purpose is to hide all direct references to
> the data structures used to represent these entities. With few exceptions, the
> functions in this module perform no semantic interpretation of their inputs, and
> in general, the user is assumed to pass type-correct arguments - if this is not
> done, the effects are not defined.
>
> Currently, the internal data structure used is the same as the record-based data
> structures used traditionally in the Beam compiler.
>
> The internal representations of abstract syntax trees are subject to change
> without notice, and should not be documented outside this module. Furthermore,
> we do not give any guarantees on how an abstract syntax tree may or may not be
> represented, _with the following exceptions_: no syntax tree is represented by a
> single atom, such as `none`, by a list constructor `[X | Y]`, or by the empty
> list `[]`. This can be relied on when writing functions that operate on syntax
> trees.
""".

-export([abstract/1, add_ann/2, alias_pat/1, alias_var/1,
         ann_abstract/2, ann_c_alias/3, ann_c_apply/3, ann_c_atom/2,
         ann_c_bitstr/5, ann_c_bitstr/6,
         ann_c_call/4, ann_c_case/3, ann_c_catch/2, ann_c_char/2,
         ann_c_clause/3, ann_c_clause/4, ann_c_cons/3, ann_c_float/2,
         ann_c_fname/3, ann_c_fun/3, ann_c_int/2,
         ann_c_let/4, ann_c_letrec/3,
         ann_c_map/2, ann_c_map/3, ann_c_map_pattern/2, ann_c_map_pair/4,
         ann_c_module/4, ann_c_module/5, ann_c_nil/1,
         ann_c_cons_skel/3, ann_c_tuple_skel/2, ann_c_primop/3,
         ann_c_receive/2, ann_c_receive/4, ann_c_seq/3, ann_c_string/2,
         ann_c_try/6, ann_c_tuple/2, ann_c_values/2, ann_c_var/2,
         ann_make_data/3, ann_make_list/2, ann_make_list/3,
         ann_make_data_skel/3, ann_make_tree/3, apply_args/1,
         apply_arity/1, apply_op/1, atom_lit/1, atom_name/1, atom_val/1,
         bitstr_val/1, bitstr_size/1, bitstr_bitsize/1,
         bitstr_unit/1, bitstr_type/1, bitstr_flags/1,
         c_alias/2, c_apply/2, c_atom/1, c_call/3, c_case/2, c_catch/1,
         c_char/1, c_clause/2, c_clause/3, c_cons/2, c_float/1,
         c_fname/2, c_fun/2, c_int/1, c_let/3, c_letrec/2,
         c_map/1, c_map/2, c_map_pattern/1, c_map_pair/2, c_map_pair_exact/2,
         c_module/3, c_module/4, c_nil/0,
         c_cons_skel/2, c_tuple_skel/1, c_primop/2,
         c_receive/1, c_receive/3, c_seq/2, c_string/1, c_try/5,
         c_tuple/1, c_values/1, c_var/1, call_args/1, call_arity/1,
         call_module/1, call_name/1, case_arg/1, case_arity/1,
         case_clauses/1, catch_body/1, char_lit/1, char_val/1,
         clause_arity/1, clause_body/1, clause_guard/1, clause_pats/1,
         clause_vars/1, concrete/1, cons_hd/1, cons_tl/1, copy_ann/2,
         data_arity/1, data_es/1, data_type/1, float_lit/1, float_val/1,
         fname_arity/1, fname_id/1, fold_literal/1, from_records/1,
         fun_arity/1, fun_body/1, fun_vars/1, get_ann/1, int_lit/1,
         int_val/1, is_c_alias/1, is_c_apply/1, is_c_atom/1,
         is_c_bitstr/1,
         is_c_call/1, is_c_case/1, is_c_catch/1, is_c_char/1,
         is_c_clause/1, is_c_cons/1, is_c_float/1, is_c_fname/1,
         is_c_fun/1, is_c_int/1, is_c_let/1, is_c_letrec/1, is_c_list/1,
         is_c_map/1, is_c_map_empty/1, is_c_map_pattern/1,
         is_c_module/1, is_c_nil/1, is_c_primop/1, is_c_receive/1,
         is_c_seq/1, is_c_string/1, is_c_try/1, is_c_tuple/1,
         is_c_values/1, is_c_var/1, is_data/1, is_leaf/1, is_literal/1,
         is_literal_term/1, is_print_char/1, is_print_string/1,
         let_arg/1, let_arity/1, let_body/1, let_vars/1, letrec_body/1,
         letrec_defs/1, letrec_vars/1, list_elements/1, list_length/1,
         make_data/2, make_list/1, make_list/2,
         make_data_skel/2, make_tree/2,
         map_arg/1, map_es/1,
         map_pair_key/1, map_pair_op/1, map_pair_val/1,
         meta/1, module_attrs/1, module_defs/1,
         module_exports/1, module_name/1, module_vars/1,
         pat_list_vars/1, pat_vars/1, primop_args/1, primop_arity/1,
         primop_name/1, receive_action/1, receive_clauses/1,
         receive_timeout/1, seq_arg/1, seq_body/1, set_ann/2,
         string_lit/1, string_val/1, subtrees/1, to_records/1,
         try_arg/1, try_body/1, try_vars/1, try_evars/1, try_handler/1,
         tuple_arity/1, tuple_es/1, type/1, unfold_literal/1,
         update_c_alias/3, update_c_apply/3, update_c_call/4,
         update_c_case/3, update_c_catch/2, update_c_clause/4,
         update_c_cons/3, update_c_cons_skel/3, update_c_fname/2,
         update_c_fname/3, update_c_fun/3, update_c_let/4,
         update_c_letrec/3, update_c_map/3, update_c_map_pair/4,
         update_c_module/5, update_c_primop/3,
         update_c_receive/4, update_c_seq/3, update_c_try/6,
         update_c_tuple/2, update_c_tuple_skel/2, update_c_values/2,
         update_c_var/2, update_data/3, update_list/2, update_list/3,
         update_data_skel/3, update_tree/2, update_tree/3,
         values_arity/1, values_es/1, var_name/1, c_binary/1,
         update_c_binary/2, ann_c_binary/2, is_c_binary/1,
         binary_segments/1, c_bitstr/3, c_bitstr/4, c_bitstr/5,
         update_c_bitstr/5, update_c_bitstr/6
        ]).

-export_type([c_binary/0, c_bitstr/0, c_call/0, c_clause/0, c_cons/0, c_fun/0,
	      c_let/0, c_literal/0, c_map/0, c_map_pair/0,
	      c_module/0, c_tuple/0,
	      c_values/0, c_var/0, cerl/0, var_name/0]).

-include("core_parse.hrl").

-type c_alias()   :: #c_alias{}.
-type c_apply()   :: #c_apply{}.
-type c_binary()  :: #c_binary{}.
-type c_bitstr()  :: #c_bitstr{}.
-type c_call()    :: #c_call{}.
-type c_case()    :: #c_case{}.
-type c_catch()   :: #c_catch{}.
-type c_clause()  :: #c_clause{}.
-type c_cons()    :: #c_cons{}.
-type c_fun()     :: #c_fun{}.
-type c_let()     :: #c_let{}.
-type c_letrec()  :: #c_letrec{}.
-type c_literal() :: #c_literal{}.
-type c_map()     :: #c_map{}.
-type c_map_pair() :: #c_map_pair{}.
-type c_module()  :: #c_module{}.
-type c_opaque()  :: #c_opaque{}.
-type c_primop()  :: #c_primop{}.
-type c_receive() :: #c_receive{}.
-type c_seq()     :: #c_seq{}.
-type c_try()     :: #c_try{}.
-type c_tuple()   :: #c_tuple{}.
-type c_values()  :: #c_values{}.
-type c_var()     :: #c_var{}.

-type cerl() :: c_alias()  | c_apply()  | c_binary()  | c_bitstr()
              | c_call()   | c_case()   | c_catch()   | c_clause()  | c_cons()
              | c_fun()    | c_let()    | c_letrec()  | c_literal()
	      | c_map()    | c_map_pair()
	      | c_module() | c_opaque()
              | c_primop() | c_receive() | c_seq()
              | c_try()    | c_tuple()  | c_values()  | c_var().

-type var_name() :: integer() | atom() | {atom(), integer()}.

%% =====================================================================
%% Representation (general)
%%
%% All nodes are represented by tuples of arity 2 or (generally)
%% greater, whose first element is an atom which uniquely identifies the
%% type of the node, and whose second element is a (proper) list of
%% annotation terms associated with the node - this is by default empty.
%%
%% For most node constructor functions, there are analogous functions
%% named 'ann_...', taking one extra argument 'As' (always the first
%% argument), specifying an annotation list at node creation time.
%% Similarly, there are also functions named 'update_...', taking one
%% extra argument 'Old', specifying a node from which all fields not
%% explicitly given as arguments should be copied (generally, this is
%% the annotation field only).
%% =====================================================================

-type ctype() :: 'alias'   | 'apply'  | 'binary' | 'bitstr' | 'call' | 'case'
               | 'catch'   | 'clause' | 'cons'   | 'fun'    | 'let'  | 'letrec'
               | 'literal' | 'map'  | 'map_pair' | 'module' | 'primop'
               | 'receive' | 'seq'    | 'try'    | 'tuple'  | 'values' | 'var'.

-doc """
Returns the type tag of `Node`.

Current node types are:

- `alias`
- `apply`
- `binary`
- `bitstr`
- `call`
- `case`
- `catch`
- `clause`
- `cons`
- `fun`
- `let`
- `letrec`
- `literal`
- `map`
- `map_pair`
- `module`
- `opaque`
- `primop`
- `receive`
- `seq`
- `try`
- `tuple`
- `values`
- `var`

> #### Note {: .info }
> The name of the primary constructor function for a node type is always the
> name of the type itself, prefixed by "`c_`"; recognizer predicates are
> correspondingly prefixed by "`is_c_`". Furthermore, to simplify preservation of
> annotations (cf. [`get_ann/1`](`get_ann/1`)), there are analogous constructor
> functions prefixed by "`ann_c_`" and "`update_c_`", for setting the annotation
> list of the new node to either a specific value or to the annotations of an
> existing node, respectively.

The only purpose of the `opaque` type is to facilitate testing of the compiler.

_See also: _`abstract/1`, `c_alias/2`, `c_apply/2`, `c_binary/1`, `c_bitstr/5`,
`c_call/3`, `c_case/2`, `c_catch/1`, `c_clause/3`, `c_cons/2`, `c_fun/2`,
`c_let/3`, `c_letrec/2`, `c_module/3`, `c_primop/2`, `c_receive/1`, `c_seq/2`,
`c_try/5`, `c_tuple/1`, `c_values/1`, `c_var/1`, `data_type/1`,
`from_records/1`, `get_ann/1`, `meta/1`, `subtrees/1`, `to_records/1`.
""".
-spec type(Node :: cerl()) -> ctype().

type(#c_alias{}) -> alias;
type(#c_apply{}) -> apply;
type(#c_binary{}) -> binary;
type(#c_bitstr{}) -> bitstr;
type(#c_call{}) -> call;
type(#c_case{}) -> 'case';
type(#c_catch{}) -> 'catch';
type(#c_clause{}) -> clause;
type(#c_cons{}) -> cons;
type(#c_fun{}) -> 'fun';
type(#c_let{}) -> 'let';
type(#c_letrec{}) -> letrec;
type(#c_literal{}) -> literal;
type(#c_map{}) -> map;
type(#c_map_pair{}) -> map_pair;
type(#c_module{}) -> module;
type(#c_primop{}) -> primop;
type(#c_receive{}) -> 'receive';
type(#c_seq{}) -> seq;
type(#c_try{}) -> 'try';
type(#c_tuple{}) -> tuple;
type(#c_values{}) -> values;
type(#c_var{}) -> var;
type(#c_opaque{}) -> opaque.

-doc """
Returns `true` if `Node` is a leaf node, otherwise `false`.

The current leaf node types are `literal` and `var`.

Note: all literals (cf. [`is_literal/1`](`is_literal/1`)) are leaf nodes, even
if they represent structured (constant) values such as `{foo, [bar, baz]}`. Also
note that variables are leaf nodes but not literals.

_See also: _`is_literal/1`, `type/1`.
""".
-spec is_leaf(Node :: cerl()) -> boolean().

is_leaf(Node) ->
    case type(Node) of
	literal -> true;
	var -> true;
	_ -> false
    end.


-doc """
Returns the list of user annotations associated with a syntax tree node.

For a newly created node, this is the empty list. The annotations may
be any terms.

_See also: _`set_ann/2`.
""".
-spec get_ann(Node :: cerl()) -> [term()].

get_ann(Node) ->
    element(2, Node).


-doc """
Sets the list of user annotations of `Node` to `Annotations`.

_See also: _`add_ann/2`, `copy_ann/2`, `get_ann/1`.
""".
-spec set_ann(Node :: cerl(), Annotations :: [term()]) -> cerl().

set_ann(Node, List) ->
    setelement(2, Node, List).


-doc """
Appends `Annotations` to the list of user annotations of `Node`.

Note: this is equivalent to
[`set_ann(Node, Annotations ++ get_ann(Node))`](`set_ann/2`), but potentially
more efficient.

_See also: _`get_ann/1`, `set_ann/2`.
""".
-spec add_ann(Annotations :: [term()], Node :: cerl()) -> cerl().

add_ann(Terms, Node) ->
    set_ann(Node, Terms ++ get_ann(Node)).


-doc """
Copies the list of user annotations from `Source` to `Target`.

Note: this is equivalent to [`set_ann(Target, get_ann(Source))`](`set_ann/2`),
but potentially more efficient.

_See also: _`get_ann/1`, `set_ann/2`.
""".
-spec copy_ann(Source :: cerl(), Target :: cerl()) -> cerl().

copy_ann(Source, Target) ->
    set_ann(Target, get_ann(Source)).


-doc """
Creates a syntax tree corresponding to an Erlang term.

`Term` must be a literal term, that is, one that can be represented as
a source code literal. Thus, it may not contain a process identifier,
port, reference, binary or function value as a subterm.

Note: This is a constant time operation.

_See also: _`ann_abstract/2`, `concrete/1`, `is_literal/1`, `is_literal_term/1`.
""".
-spec abstract(Term :: term()) -> c_literal().

abstract(T) ->
    #c_literal{val = T}.


-doc "_See also: _`abstract/1`.".
-spec ann_abstract(Annotations :: [term()], Term :: term()) -> c_literal().

ann_abstract(As, T) ->
    #c_literal{val = T, anno = As}.


-doc """
Returns `true` if `Term` can be represented as a literal, otherwise `false`.

This function takes time proportional to the size of `Term`.

_See also: _`abstract/1`.
""".
-spec is_literal_term(Term :: term()) -> boolean().

is_literal_term(T) when is_integer(T) -> true;
is_literal_term(T) when is_float(T) -> true;
is_literal_term(T) when is_atom(T) -> true;
is_literal_term([]) -> true;
is_literal_term([H | T]) ->
    is_literal_term(H) andalso is_literal_term(T);
is_literal_term(T) when is_tuple(T) ->
    is_literal_term_list(tuple_to_list(T));
is_literal_term(B) when is_bitstring(B) -> true;
is_literal_term(M) when is_map(M) ->
    is_literal_term_list(maps:to_list(M));
is_literal_term(F) when is_function(F) ->
    erlang:fun_info(F, type) =:= {type,external};
is_literal_term(_) ->
    false.

-spec is_literal_term_list([term()]) -> boolean().

is_literal_term_list([T | Ts]) ->
    case is_literal_term(T) of
	true ->
	    is_literal_term_list(Ts);
	false ->
	    false
    end;
is_literal_term_list([]) ->
    true.


-doc """
Returns the Erlang term represented by a syntax tree.

An exception is thrown if `Node` does not represent a literal term.

Note: This is a constant time operation.

_See also: _`abstract/1`, `is_literal/1`.
""".
-spec concrete(Node :: c_literal()) -> term().

concrete(#c_literal{val = V}) ->
    V.


-doc """
Returns `true` if `Node` represents a literal term, otherwise `false`.

This function returns `true` if and only if the value of
[`concrete(Node)`](`concrete/1`) is defined.

Note: This is a constant time operation.

_See also: _`abstract/1`, `concrete/1`, `fold_literal/1`.
""".
-spec is_literal(Node :: cerl()) -> boolean().

is_literal(#c_literal{}) ->
    true;
is_literal(_) ->
    false.


-doc """
Ensures that literals have a compact representation.

This is occasionally useful if
[`c_cons_skel/2`](`c_cons_skel/2`), [`c_tuple_skel/1`](`c_tuple_skel/1`) or
[`unfold_literal/1`](`unfold_literal/1`) were used in the construction of
`Node`, and you want to revert to the normal "folded" representation of
literals. If `Node` represents a tuple or list constructor, its elements are
rewritten recursively, and the node is reconstructed using
[`c_cons/2`](`c_cons/2`) or [`c_tuple/1`](`c_tuple/1`), respectively; otherwise,
`Node` is not changed.

_See also: _`c_cons/2`, `c_cons_skel/2`, `c_tuple/1`, `c_tuple_skel/1`,
`is_literal/1`, `unfold_literal/1`.
""".
-spec fold_literal(Node :: cerl()) -> cerl().

fold_literal(Node) ->
    case type(Node) of
	tuple ->
	    update_c_tuple(Node, fold_literal_list(tuple_es(Node)));
	cons ->
	    update_c_cons(Node, fold_literal(cons_hd(Node)),
			  fold_literal(cons_tl(Node)));
	_ ->
	    Node
    end.

fold_literal_list([E | Es]) ->
    [fold_literal(E) | fold_literal_list(Es)];
fold_literal_list([]) ->
    [].


-doc """
Ensures that literals have a fully expanded representation.

If `Node` represents a literal tuple or list constructor, its elements
are rewritten recursively, and the node is reconstructed using
[`c_cons_skel/2`](`c_cons_skel/2`) or
[`c_tuple_skel/1`](`c_tuple_skel/1`), respectively; otherwise, `Node`
is not changed. The `fold_literal/1` can be used to revert to the
normal compact representation.

_See also: _`c_cons/2`, `c_cons_skel/2`, `c_tuple/1`, `c_tuple_skel/1`,
`fold_literal/1`, `is_literal/1`.
""".
-spec unfold_literal(Node :: cerl()) -> cerl().

unfold_literal(Node) ->
    case type(Node) of
	literal ->
	    copy_ann(Node, unfold_concrete(concrete(Node)));
	_ ->
	    Node
    end.

unfold_concrete(Val) ->
    case Val of
	_ when is_tuple(Val) ->
	    c_tuple_skel(unfold_concrete_list(tuple_to_list(Val)));
	[H|T] ->
	    c_cons_skel(unfold_concrete(H), unfold_concrete(T));
	_ ->
	    abstract(Val)
    end.

unfold_concrete_list([E | Es]) ->
    [unfold_concrete(E) | unfold_concrete_list(Es)];
unfold_concrete_list([]) ->
    [].


%% ---------------------------------------------------------------------

-doc #{equiv => c_module(Name, Exports, [], Definitions)}.
-spec c_module(Name :: cerl(),
               Exports :: [cerl()],
               Definitions :: [{cerl(), cerl()}]) -> c_module().

c_module(Name, Exports, Es) ->
    #c_module{name = Name, exports = Exports, attrs = [], defs = Es}.


-doc """
Creates an abstract module definition.

The result represents

```text
    module Name [E1, ..., Ek]
      attributes [K1 = T1, ...,
                  Km = Tm]
      V1 = F1
      ...
      Vn = Fn
    end
```

if `Exports` = `[E1, ..., Ek]`, `Attributes` = `[{K1, T1}, ..., {Km, Tm}]`, and
`Definitions` = `[{V1, F1}, ..., {Vn, Fn}]`.

`Name` and all the `Ki` must be atom literals, and all the `Ti` must be constant
literals. All the `Vi` and `Ei` must have type `var` and represent function
names. All the `Fi` must have type `'fun'`.

_See also: _`ann_c_module/4`, `ann_c_module/5`, `c_atom/1`, `c_fun/2`,
`c_module/3`, `c_var/1`, `is_literal/1`, `module_attrs/1`, `module_defs/1`,
`module_exports/1`, `module_name/1`, `module_vars/1`, `update_c_module/5`.
""".
-spec c_module(Name :: cerl(), Exports :: [cerl()],
               Attributes :: [{cerl(), cerl()}],
               Definitions :: [{cerl(), cerl()}]) ->
          c_module().

c_module(Name, Exports, Attrs, Es) ->
    #c_module{name = Name, exports = Exports, attrs = Attrs, defs = Es}.


-doc "_See also: _`ann_c_module/5`, `c_module/3`.".
-spec ann_c_module(Annotations :: [term()], Name :: cerl(),
                   Exports :: [cerl()], Definitions :: [{cerl(), cerl()}]) ->
          c_module().

ann_c_module(As, Name, Exports, Es) ->
    #c_module{name = Name, exports = Exports, attrs = [], defs = Es,
	      anno = As}.


-doc "_See also: _`ann_c_module/4`, `c_module/4`.".
-spec ann_c_module(Annotations :: [term()], Name :: cerl(),
                   Exports :: [cerl()],
		   Attributes :: [{cerl(), cerl()}],
                   Definitions :: [{cerl(), cerl()}]) -> c_module().

ann_c_module(As, Name, Exports, Attrs, Es) ->
    #c_module{name = Name, exports = Exports, attrs = Attrs, defs = Es,
	      anno = As}.


-doc "_See also: _`c_module/4`.".
-spec update_c_module(Node :: c_module(), Name :: cerl(), Exports ::[cerl()],
		      Attributes :: [{cerl(), cerl()}],
                      Definitions :: [{cerl(), cerl()}]) -> c_module().

update_c_module(Node, Name, Exports, Attrs, Es) ->
    #c_module{name = Name, exports = Exports, attrs = Attrs, defs = Es,
	      anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract module definition, otherwise `false`.

_See also: _`type/1`.
""".
-spec is_c_module(Node :: cerl()) -> boolean().

is_c_module(#c_module{}) ->
    true;
is_c_module(_) ->
    false.


-doc """
Returns the name subtree of an abstract module definition.

_See also: _`c_module/4`.
""".
-spec module_name(Node :: c_module()) -> cerl().

module_name(Node) ->
    Node#c_module.name.


-doc """
Returns the list of exports subtrees of an abstract module definition.

_See also: _`c_module/4`.
""".
-spec module_exports(Node :: c_module()) -> [cerl()].

module_exports(Node) ->
    Node#c_module.exports.


-doc """
Returns the list of pairs of attribute key/value subtrees of an abstract module
definition.

_See also: _`c_module/4`.
""".
-spec module_attrs(Node :: c_module()) -> [{cerl(), cerl()}].

module_attrs(Node) ->
    Node#c_module.attrs.


-doc """
Returns the list of function definitions of an abstract module definition.

_See also: _`c_module/4`.
""".
-spec module_defs(Node :: c_module()) -> [{cerl(), cerl()}].

module_defs(Node) ->
    Node#c_module.defs.


-doc """
Returns the list of left-hand side function variable subtrees of an abstract
module definition.

_See also: _`c_module/4`.
""".
-spec module_vars(Node :: c_module()) -> [cerl()].

module_vars(Node) ->
    [F || {F, _} <:- module_defs(Node)].

%% ---------------------------------------------------------------------

-doc """
Creates an abstract integer literal.

The lexical representation is the canonical decimal numeral of `Value`.

_See also: _`ann_c_int/2`, `c_char/1`, `int_lit/1`, `int_val/1`, `is_c_int/1`.
""".
-spec c_int(Value :: integer()) -> c_literal().

c_int(Value) ->
    #c_literal{val = Value}.


-doc "_See also: _`c_int/1`.".
-spec ann_c_int(Annotations :: [term()], Value :: integer()) -> c_literal().

ann_c_int(As, Value) ->
    #c_literal{val = Value, anno = As}.


-doc """
Returns `true` if `Node` represents an integer literal, otherwise `false`.

_See also: _`c_int/1`.
""".
-spec is_c_int(Node :: cerl()) -> boolean().

is_c_int(#c_literal{val = V}) when is_integer(V) ->
    true;
is_c_int(_) ->
    false.


-doc """
Returns the value represented by an integer literal node.

_See also: _`c_int/1`.
""".
-spec int_val(Node :: c_literal()) -> integer().

int_val(Node) ->
    Node#c_literal.val.


-doc """
Returns the numeral string represented by an integer literal node.

_See also: _`c_int/1`.
""".
-spec int_lit(Node :: c_literal()) -> string().

int_lit(Node) ->
    integer_to_list(int_val(Node)).


%% ---------------------------------------------------------------------

-doc """
Creates an abstract floating-point literal.

The lexical representation is the decimal floating-point numeral of
`Value`.

_See also: _`ann_c_float/2`, `float_lit/1`, `float_val/1`, `is_c_float/1`.
""".
-spec c_float(Value :: float()) -> c_literal().

c_float(Value) ->
    #c_literal{val = Value}.


-doc "_See also: _`c_float/1`.".
-spec ann_c_float(Annotations :: [term()], Value :: float()) -> c_literal().

ann_c_float(As, Value) ->
    #c_literal{val = Value, anno = As}.


-doc """
Returns `true` if `Node` represents a floating-point literal, otherwise `false`.

_See also: _`c_float/1`.
""".
-spec is_c_float(Node :: cerl()) -> boolean().

is_c_float(#c_literal{val = V}) when is_float(V) ->
    true;
is_c_float(_) ->
    false.


-doc """
Returns the value represented by a floating-point literal node.

_See also: _`c_float/1`.
""".
-spec float_val(Node :: c_literal()) -> float().

float_val(Node) ->
    Node#c_literal.val.


-doc """
Returns the numeral string represented by a floating-point literal node.

_See also: _`c_float/1`.
""".
-spec float_lit(Node :: c_literal()) -> string().

float_lit(Node) ->
    float_to_list(float_val(Node)).


%% ---------------------------------------------------------------------

-doc """
Creates an abstract atom literal.

The print name of the atom is the character sequence represented by
`Name`.

Note: passing a string as argument to this function causes a corresponding atom
to be created for the internal representation.

_See also: _`ann_c_atom/2`, `atom_lit/1`, `atom_name/1`, `atom_val/1`,
`is_c_atom/1`.
""".
-spec c_atom(Name :: atom() | string()) -> c_literal().

c_atom(Name) when is_atom(Name) ->
    #c_literal{val = Name};
c_atom(Name) ->
    #c_literal{val = list_to_atom(Name)}.


-doc "_See also: _`c_atom/1`.".
-spec ann_c_atom(Annotations :: [term()], Name :: atom() | string()) -> c_literal().

ann_c_atom(As, Name) when is_atom(Name) ->
    #c_literal{val = Name, anno = As};
ann_c_atom(As, Name) ->
    #c_literal{val = list_to_atom(Name), anno = As}.


-doc """
Returns `true` if `Node` represents an atom literal, otherwise `false`.

_See also: _`c_atom/1`.
""".
-spec is_c_atom(Node :: cerl()) -> boolean().

is_c_atom(#c_literal{val = V}) when is_atom(V) ->
    true;
is_c_atom(_) ->
    false.

-doc """
Returns the value represented by an abstract atom.

_See also: _`c_atom/1`.
""".
-spec atom_val(Node :: c_literal()) -> atom().

atom_val(Node) ->
    Node#c_literal.val.


-doc """
Returns the printname of an abstract atom.

_See also: _`c_atom/1`.
""".
-spec atom_name(Node :: c_literal()) -> string().

atom_name(Node) ->
    atom_to_list(atom_val(Node)).


%% TODO: replace the use of the unofficial 'write_string/2'.

-doc """
Returns the literal string represented by an abstract atom. This always includes
surrounding single-quote characters.

Note that an abstract atom may have several literal representations, and that
the representation yielded by this function is not fixed; for example,
[`atom_lit(c_atom("a\012b"))`](`atom_lit/1`) could yield the string
`"\'a\\nb\'"`.

_See also: _`c_atom/1`.
""".
-spec atom_lit(Node :: cerl()) -> nonempty_string().

atom_lit(Node) ->
    io_lib:write_string(atom_name(Node), $'). %' stupid Emacs.


%% ---------------------------------------------------------------------

-doc """
Creates an abstract character literal.

If the local implementation of Erlang defines `t:char/0` as a subset
of `t:integer/0`, this function is equivalent to
[`c_int/1`](`c_int/1`). Otherwise, if the given value is an integer,
it will be converted to the character with the corresponding code. The
lexical representation of a character is "`$Char`", where `Char` is a
single printing character or an escape sequence.

_See also: _`ann_c_char/2`, `c_int/1`, `c_string/1`, `char_lit/1`, `char_val/1`,
`is_c_char/1`, `is_print_char/1`.
""".
-spec c_char(Value :: non_neg_integer()) -> c_literal().

c_char(Value) when is_integer(Value), Value >= 0 ->
    #c_literal{val = Value}.


-doc "_See also: _`c_char/1`.".
-spec ann_c_char(Annotations :: [term()], Value :: char()) -> c_literal().

ann_c_char(As, Value) ->
    #c_literal{val = Value, anno = As}.


-doc """
Returns `true` if `Node` may represent a character literal, otherwise `false`.

If the local implementation of Erlang defines `t:char/0` as a subset of
`t:integer/0`, then `is_c_int(Node)` will also yield `true`.

_See also: _`c_char/1`, `is_print_char/1`.
""".
-spec is_c_char(Node :: c_literal()) -> boolean().

is_c_char(#c_literal{val = V}) when is_integer(V), V >= 0 ->
    is_char_value(V);
is_c_char(_) ->
    false.


-doc """
Returns `true` if `Node` may represent a "printing" character, otherwise
`false`. (Cf. [`is_c_char/1`](`is_c_char/1`).)

A "printing" character has either a given graphical representation, or
a "named" escape sequence such as "`\n`".  Currently, only ISO 8859-1
(Latin-1) character values are recognized.

_See also: _`c_char/1`, `is_c_char/1`.
""".
-spec is_print_char(Node :: cerl()) -> boolean().

is_print_char(#c_literal{val = V}) when is_integer(V), V >= 0 ->
    is_print_char_value(V);
is_print_char(_) ->
    false.


-doc """
Returns the value represented by an abstract character literal.

_See also: _`c_char/1`.
""".
-spec char_val(Node :: c_literal()) -> char().

char_val(Node) ->
    Node#c_literal.val.


-doc """
Returns the literal string represented by an abstract character. This includes a
leading `$` character.

Currently, all characters that are not in the set of ISO 8859-1
(Latin-1) "printing" characters will be escaped.

_See also: _`c_char/1`.
""".
-spec char_lit(Node :: c_literal()) -> nonempty_string().

char_lit(Node) ->
    io_lib:write_char(char_val(Node)).


%% ---------------------------------------------------------------------

-doc """
Creates an abstract string literal.

Equivalent to creating an abstract list of the corresponding character
literals (cf. [`is_c_string/1`](`is_c_string/1`)), but is typically
more efficient. The lexical representation of a string is "`"Chars"`",
where `Chars` is a sequence of printing characters or spaces.

_See also: _`ann_c_string/2`, `c_char/1`, `is_c_string/1`, `is_print_string/1`,
`string_lit/1`, `string_val/1`.
""".
-spec c_string(Value :: string()) -> c_literal().

c_string(Value) ->
    #c_literal{val = Value}.


-doc "_See also: _`c_string/1`.".
-spec ann_c_string(Annotations :: [term()], Value :: string()) -> c_literal().

ann_c_string(As, Value) ->
    #c_literal{val = Value, anno = As}.


-doc """
Returns `true` if `Node` may represent a string literal, otherwise `false`.

Strings are defined as lists of characters; see [`is_c_char/1`](`is_c_char/1`)
for details.

_See also: _`c_string/1`, `is_c_char/1`, `is_print_string/1`.
""".
-spec is_c_string(Node :: cerl()) -> boolean().

is_c_string(#c_literal{val = V}) ->
    is_char_list(V);
is_c_string(_) ->
    false.


-doc """
Returns `true` if `Node` may represent a string literal containing only
"printing" characters, otherwise `false`.

See [`is_c_string/1`](`is_c_string/1`) and
[`is_print_char/1`](`is_print_char/1`) for details. Currently, only
ISO 8859-1 (Latin-1) character values are recognized.

_See also: _`c_string/1`, `is_c_string/1`, `is_print_char/1`.
""".
-spec is_print_string(Node :: cerl()) -> boolean().

is_print_string(#c_literal{val = V}) ->
    is_print_char_list(V);
is_print_string(_) ->
    false.


-doc """
Returns the value represented by an abstract string literal.

_See also: _`c_string/1`.
""".
-spec string_val(Node :: c_literal()) -> string().

string_val(Node) ->
    Node#c_literal.val.


-doc """
Returns the literal string represented by an abstract string. This includes
surrounding double-quote characters `"..."`.

Currently, characters that are not in the set of ISO 8859-1 (Latin-1)
"printing" characters will be escaped, except for spaces.

_See also: _`c_string/1`.
""".
-spec string_lit(Node :: c_literal()) -> nonempty_string().

string_lit(Node) ->
    io_lib:write_string(string_val(Node)).


%% ---------------------------------------------------------------------

-doc """
Creates an abstract empty list.

The result represents "`[]`". The empty list is traditionally called
"nil".

_See also: _`ann_c_nil/1`, `c_cons/2`, `is_c_list/1`.
""".
-spec c_nil() -> c_literal().

c_nil() ->
    #c_literal{val = []}.


-doc "_See also: _`c_nil/0`.".
-spec ann_c_nil(Annotations :: [term()]) -> c_literal().

ann_c_nil(As) ->
    #c_literal{val = [], anno = As}.


-doc "Returns `true` if `Node` is an abstract empty list, otherwise `false`.".
-spec is_c_nil(Node :: cerl()) -> boolean().

is_c_nil(#c_literal{val = []}) ->
    true;
is_c_nil(_) ->
    false.


%% ---------------------------------------------------------------------

-doc """
Creates an abstract list constructor.

The result represents "`[Head | Tail]`". Note that if both `Head` and
`Tail` have type `literal`, then the result will also have type
`literal`, and annotations on `Head` and `Tail` are lost.

Recall that in Erlang, the tail element of a list constructor is not necessarily
a list.

_See also: _`ann_c_cons/3`, `c_cons_skel/2`, `c_nil/0`, `cons_hd/1`,
`cons_tl/1`, `is_c_cons/1`, `is_c_list/1`, `list_elements/1`, `list_length/1`,
`make_list/2`, `update_c_cons/3`.
""".

%% *Always* collapse literals.

-spec c_cons(Head :: cerl(), Tail :: cerl()) -> c_literal() | c_cons().

c_cons(#c_literal{val = Head}, #c_literal{val = Tail}) ->
    #c_literal{val = [Head | Tail]};
c_cons(Head, Tail) ->
    #c_cons{hd = Head, tl = Tail}.


-doc "_See also: _`c_cons/2`.".
-spec ann_c_cons(Annotations :: [term()], Head :: cerl(), Tail ::cerl()) ->
          c_literal() | c_cons().

ann_c_cons(As, #c_literal{val = Head}, #c_literal{val = Tail}) ->
    #c_literal{val = [Head | Tail], anno = As};
ann_c_cons(As, Head, Tail) ->
    #c_cons{hd = Head, tl = Tail, anno = As}.


-doc "_See also: _`c_cons/2`.".
-spec update_c_cons(Node :: c_literal() | c_cons(), Head :: cerl(), Tail :: cerl()) ->
          c_literal() | c_cons().

update_c_cons(Node, #c_literal{val = Head}, #c_literal{val = Tail}) ->
    #c_literal{val = [Head | Tail], anno = get_ann(Node)};
update_c_cons(Node, Head, Tail) ->
    #c_cons{hd = Head, tl = Tail, anno = get_ann(Node)}.


-doc """
Creates an abstract list constructor skeleton.

Does not fold constant literals, that is, the result always has type
`cons`, representing "`[Head | Tail]`".

This function is occasionally useful when it is necessary to have annotations on
the subnodes of a list constructor node, even when the subnodes are constant
literals. However, note that [`is_literal/1`](`is_literal/1`) will yield `false`
and [`concrete/1`](`concrete/1`) will fail if passed the result from this
function.

[`fold_literal/1`](`fold_literal/1`) can be used to revert a node to the
normal-form representation.

_See also: _`ann_c_cons_skel/3`, `c_cons/2`, `c_nil/0`, `concrete/1`,
`fold_literal/1`, `is_c_cons/1`, `is_c_list/1`, `is_literal/1`,
`update_c_cons_skel/3`.
""".

%% *Never* collapse literals.

-spec c_cons_skel(Head :: cerl(), Tail :: cerl()) -> c_cons().

c_cons_skel(Head, Tail) ->
    #c_cons{hd = Head, tl = Tail}.


-doc "_See also: _`c_cons_skel/2`.".
-spec ann_c_cons_skel(Annotations :: [term()], Head :: cerl(), Tail :: cerl()) -> c_cons().

ann_c_cons_skel(As, Head, Tail) ->
    #c_cons{hd = Head, tl = Tail, anno = As}.


-doc "_See also: _`c_cons_skel/2`.".
-spec update_c_cons_skel(Node :: c_cons() | c_literal(),
                         Head :: cerl(),
                         Tail ::cerl()) -> c_cons().

update_c_cons_skel(Node, Head, Tail) ->
    #c_cons{hd = Head, tl = Tail, anno = get_ann(Node)}.


-doc "Returns `true` if `Node` is an abstract list constructor, otherwise `false`.".
-spec is_c_cons(Node :: cerl()) -> boolean().

is_c_cons(#c_cons{}) ->
    true;
is_c_cons(#c_literal{val = [_ | _]}) ->
    true;
is_c_cons(_) ->
    false.


-doc """
Returns the head subtree of an abstract list constructor.

_See also: _`c_cons/2`.
""".
-spec cons_hd(Node :: c_cons() | c_literal()) -> cerl().

cons_hd(#c_cons{hd = Head}) ->
    Head;
cons_hd(#c_literal{val = [Head | _]}) ->
    #c_literal{val = Head}.


-doc """
Returns the tail subtree of an abstract list constructor.

Recall that the tail does not necessarily represent a proper list.

_See also: _`c_cons/2`.
""".
-spec cons_tl(Node :: c_cons() | c_literal()) -> cerl().

cons_tl(#c_cons{tl = Tail}) ->
    Tail;
cons_tl(#c_literal{val = [_ | Tail]}) ->
    #c_literal{val = Tail}.


-doc """
Returns `true` if `Node` represents a proper list, otherwise `false`.

A proper list is either the empty list `[]`, or a cons cell `[Head |
Tail]`, where recursively `Tail` is a proper list.

Note: Because `Node` is a syntax tree, the actual run-time values
corresponding to its subtrees may often be partially or completely
unknown. Thus, if `Node` represents for example "`[... | Ns]`" (where
`Ns` is a variable), then the function will return `false`, because it
is not known whether `Ns` will be bound to a list at run-time. If
`Node` instead represents for example "`[1, 2, 3]`" or "`[A | []]`",
then the function will return `true`.

_See also: _`c_cons/2`, `c_nil/0`, `list_elements/1`, `list_length/1`.
""".
-spec is_c_list(Node :: cerl()) -> boolean().

is_c_list(#c_cons{tl = Tail}) ->
    is_c_list(Tail);
is_c_list(#c_literal{val = V}) ->
    is_proper_list(V);
is_c_list(_) ->
    false.

is_proper_list([_ | Tail]) ->
    is_proper_list(Tail);
is_proper_list([]) ->
    true;
is_proper_list(_) ->
    false.

-doc """
Returns the list of element subtrees of an abstract list.

`Node` must represent a proper list. For example, if `Node` represents
"`[X1, X2 | [X3, X4 | []]`", then
[`list_elements(Node)`](`list_elements/1`) yields the list `[X1, X2,
X3, X4]`.

_See also: _`c_cons/2`, `c_nil/0`, `is_c_list/1`, `list_length/1`,
`make_list/2`.
""".
-spec list_elements(Node :: c_cons() | c_literal()) -> [cerl()].

list_elements(#c_cons{hd = Head, tl = Tail}) ->
    [Head | list_elements(Tail)];
list_elements(#c_literal{val = V}) ->
    abstract_list(V).

abstract_list([X | Xs]) ->
    [abstract(X) | abstract_list(Xs)];
abstract_list([]) ->
    [].


-doc """
Returns the number of element subtrees of an abstract list.

`Node` must represent a proper list. For example, if `Node` represents
"`[X1 | [X2, X3 | [X4, X5, X6]]]`", then
[`list_length(Node)`](`list_length/1`) returns the integer 6.

Note: this is equivalent to [`length(list_elements(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_cons/2`, `c_nil/0`, `is_c_list/1`, `list_elements/1`.
""".
-spec list_length(Node :: c_cons() | c_literal()) -> non_neg_integer().

list_length(L) ->
    list_length(L, 0).

list_length(#c_cons{tl = Tail}, A) ->
    list_length(Tail, A + 1);
list_length(#c_literal{val = V}, A) ->
    A + length(V).


-doc #{equiv => make_list(List, none)}.
-spec make_list(List :: [cerl()]) -> cerl().

make_list(List) ->
    ann_make_list([], List).


-doc """
Creates an abstract list from the elements in `List` and the optional `Tail`.

If `Tail` is `none`, the result will represent a nil-terminated list,
otherwise it represents "`[... | Tail]`".

_See also: _`ann_make_list/3`, `c_cons/2`, `c_nil/0`, `list_elements/1`,
`update_list/3`.
""".
-spec make_list(List :: [cerl()], Tail :: cerl() | 'none') -> cerl().

make_list(List, Tail) ->
    ann_make_list([], List, Tail).


-doc #{equiv => update_list(Old, List, none)}.
-spec update_list(Node :: cerl(), List :: [cerl()]) -> cerl().

update_list(Node, List) ->
    ann_make_list(get_ann(Node), List).


-doc "_See also: _`make_list/2`, `update_list/2`.".
-spec update_list(Node :: cerl(), List :: [cerl()], Tail ::cerl() | 'none') -> cerl().

update_list(Node, List, Tail) ->
    ann_make_list(get_ann(Node), List, Tail).


-doc #{equiv => ann_make_list(As, List, none)}.
-spec ann_make_list(Annotations :: [term()], List :: [cerl()]) -> cerl().

ann_make_list(As, List) ->
    ann_make_list(As, List, none).


-doc "_See also: _`ann_make_list/2`, `make_list/2`.".
-spec ann_make_list(Annotations ::[term()],
                    List :: [cerl()],
                    Tail ::cerl() | 'none') -> cerl().

ann_make_list(As, [H | T], Tail) ->
    ann_c_cons(As, H, make_list(T, Tail));    % `c_cons' folds literals
ann_make_list(As, [], none) ->
    ann_c_nil(As);
ann_make_list(_, [], Node) ->
    Node.


%% ---------------------------------------------------------------------

-doc """
Creates an abstract map constructor.

If `Pairs` is `[E1, ..., EN]`, the result represents "`~{E1, ...,
EN}~`" (creating a new map). Note that if all pairs in `Pairs` have
type `literal` for both the key and the value, or if `Pairs` is empty,
then the result will also have type `literal` and annotations on nodes
in `Pairs` are lost.

All `Ei` must be abstract pairs constructed by `c_map_pair/2`.

_See also: _`ann_c_map/2`, `is_c_map/1`, `is_c_map_empty/1`, `is_c_map_pattern/1`,
`map_es/1`, `c_map_pair/2`, `c_map_pair_exact/2`.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec c_map(Pairs :: [c_map_pair()]) -> c_map().

c_map(Pairs) ->
    ann_c_map([], Pairs).

-doc """
Creates an abstract map update expression.

If `Pairs` is `[E1, ..., EN]`, the result represents "`~{E1, ..., EN |
Argument}~`" (updating an existing map). Note that if `Argument` is a
literal and all pairs in `Pairs` have type `literal` for both the key
and the value, or if `Pairs` is empty, then the result will also have
type `literal` and annotations on nodes in `Pairs` are lost.

All `Ei` must be abstract pairs constructed by either `c_map_pair/2` or
`c_map_pair_exact/2`.

_See also: _`ann_c_map/2`, `is_c_map/1`, `is_c_map_empty/1`, `is_c_map_pattern/1`,
`map_es/1`, `c_map_pair/2`, `c_map_pair_exact/2`.
""".
-doc(#{since => <<"OTP 27.0">>}).

-spec c_map(Argument :: cerl(), Pairs :: [c_map_pair()]) -> c_map().

c_map(Argument, Pairs) ->
    ann_c_map([], Argument, Pairs).

-doc """
Creates an abstract map pattern.

If `Pairs` is `[E1, ..., EN]`, the result represents
"`~{E1, ..., EN}~`".

All `Ei` must be abstract pairs constructed by `c_map_pair_exact/2`.

_See also: _`ann_c_map/2`, `is_c_map/1`, `is_c_map_empty/1`, `is_c_map_pattern/1`,
`map_es/1`, `c_map_pair_exact/2`.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec c_map_pattern(Pairs :: [c_map_pair()]) -> c_map().

c_map_pattern(Pairs) ->
    #c_map{es=Pairs, is_pat=true}.


-type map_op() :: #c_literal{val::'assoc'} | #c_literal{val::'exact'}.

-doc """
Returns `true` if `Node` is any kind of abstract map (for constructing,
updating or matching), otherwise `false`.

_See also: _`ann_c_map/3`, `c_map/1`, `c_map_pattern/1`.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec is_c_map(Node :: cerl()) -> boolean().

is_c_map(#c_map{}) ->
    true;
is_c_map(#c_literal{val = V}) when is_map(V) ->
    true;
is_c_map(_) ->
    false.

-doc """
Returns the list of map pair subtrees of an abstract map.

_See also: _`c_map/1`.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec map_es(Node :: c_map() | c_literal()) -> [c_map_pair()].

map_es(#c_literal{anno=As,val=M}) when is_map(M) ->
    [ann_c_map_pair(As,
                    #c_literal{anno=As,val='assoc'},
                    #c_literal{anno=As,val=K},
                    #c_literal{anno=As,val=V}) || K := V <- M];
map_es(#c_map{es = Es}) ->
    Es.

-doc """
Returns the argument subtree of an abstract map.

_See also: _`c_map/2`.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec map_arg(Node :: c_map() | c_literal()) -> c_map() | c_literal().

map_arg(#c_literal{anno=As,val=M}) when is_map(M) ->
    #c_literal{anno = As, val = #{}};
map_arg(#c_map{arg = M}) ->
    M.


-doc """
Returns `true` if `Node` represents an empty abstract map, otherwise `false`.

_See also: _`c_map/1`, `c_map_pattern/1`.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec is_c_map_empty(Node :: c_map() | c_literal()) -> boolean().

is_c_map_empty(#c_map{es = []}) ->
    true;
is_c_map_empty(#c_literal{val=M}) when is_map(M), map_size(M) =:= 0 ->
    true;
is_c_map_empty(_) ->
    false.

-doc """
Returns `true` if `Node` is an abstract map pattern, otherwise `false`.

_See also: _`c_map/1`, `c_map_pattern/1`.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec is_c_map_pattern(Node :: c_map()) -> boolean().

is_c_map_pattern(#c_map{is_pat=IsPat}) ->
    IsPat.

-doc "_See also: _`c_map/1`.".
-doc(#{since => <<"OTP 17.0">>}).
-spec ann_c_map(Annotations :: [term()], Pairs :: [c_map_pair()]) ->
          c_map() | c_literal().

ann_c_map(As, Es) ->
    ann_c_map(As, #c_literal{val=#{}}, Es).


-doc "_See also: _`c_map/2`".
-doc(#{since => <<"OTP 17.0">>}).
-spec ann_c_map(Annotations :: [term()],
                Argument :: c_map() | c_literal(),
                Pairs :: [c_map_pair()]) -> c_map() | c_literal().

ann_c_map(As, #c_literal{val=M0}=Lit, Es) when is_map(M0) ->
    case update_map_literal(Es, M0) of
        none ->
            #c_map{arg=Lit, es=Es, anno=As};
        M1 ->
            #c_literal{anno=As, val=M1}
    end;
ann_c_map(As, M, Es) ->
    #c_map{arg=M, es=Es, anno=As}.


-doc "_See also: _`c_map_pattern/2`".
-doc(#{since => <<"OTP 17.0">>}).
-spec ann_c_map_pattern(Annotations :: [term()], Pairs :: [c_map_pair()]) -> c_map().

ann_c_map_pattern(As, Pairs) ->
    #c_map{anno=As, es=Pairs, is_pat=true}.

update_map_literal([#c_map_pair{op=#c_literal{val=assoc},key=Ck,val=Cv}|Es], M) ->
    %% M#{K => V}
    case is_lit_list([Ck,Cv]) of
	true ->
	    [K,V] = lit_list_vals([Ck,Cv]),
	    update_map_literal(Es, M#{K => V});
	false ->
	    none
    end;
update_map_literal([#c_map_pair{op=#c_literal{val=exact},key=Ck,val=Cv}|Es], M) ->
    %% M#{K := V}
    case is_lit_list([Ck,Cv]) of
	true ->
	    [K,V] = lit_list_vals([Ck,Cv]),
	    case is_map_key(K, M) of
		true ->
                    update_map_literal(Es, M#{K => V});
		false ->
		    none
	    end;
	false ->
            none
    end;
update_map_literal([], M) ->
    M.

-doc "_See also: _`c_map/1`, `c_map_pattern/1`.".
-doc(#{since => <<"OTP 17.0">>}).

-spec update_c_map(Node :: c_map(), Map :: cerl(),
                   Pairs :: [c_map_pair()]) -> c_map() | c_literal().

update_c_map(#c_map{is_pat = true}=Old, M, Es) ->
    Old#c_map{arg = M, es = Es};
update_c_map(#c_map{is_pat=false}=Old, M, Es) ->
    ann_c_map(get_ann(Old), M, Es).

%% ---------------------------------------------------------------------

-doc """
Creates an abstract map pair using the `assoc` operator.

These can only occur as components of an abstract map creation
expression or an abstract update expression (see `c_map/1` and
`c_map/2`).

The result represents "`Key => Value`".

_See also: _`map_pair_key/1`, `map_pair_op/1`, `map_pair_val/1`.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec c_map_pair(Key :: cerl(), Value:: cerl()) -> c_map_pair().

c_map_pair(Key, Val) ->
    #c_map_pair{op = #c_literal{val=assoc}, key = Key, val = Val}.

-doc """
Creates an abstract map pair using the `exact` operator.

These can only occur as components of an abstract map update
expression or an abstract map pattern (see `c_map/1` and
`c_map_pattern/1`).

The result represents "`Key := Value`".

_See also: _`map_pair_key/1`, `map_pair_op/1`, `map_pair_val/1`.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec c_map_pair_exact(Key :: cerl(), Value :: cerl()) -> c_map_pair().

c_map_pair_exact(Key, Val) ->
    #c_map_pair{op = #c_literal{val=exact}, key = Key, val = Val}.

-doc "_See also: _`c_map_pair/2`, `c_map_pair_exact/2`.".
-doc(#{since => <<"OTP 17.0">>}).
-spec ann_c_map_pair(Annotations :: [term()], Operation :: cerl(),
                     Key :: cerl(), Value :: cerl()) -> c_map_pair().

ann_c_map_pair(As, Op, K, V) ->
    #c_map_pair{op = Op, key = K, val=V, anno = As}.

-doc "_See also: _`c_map_pair/2`, `c_map_pair_exact/2`.".
-doc(#{since => <<"OTP 17.0">>}).

-spec update_c_map_pair(Node :: c_map_pair(), Operation ::map_op(),
                        Key :: cerl(), Value :: cerl()) -> c_map_pair().

update_c_map_pair(Node, Op, K, V) ->
    #c_map_pair{op = Op, key = K, val = V, anno = get_ann(Node)}.

-doc """
Returns the key subtree of an abstract map pair.

_See also: _`c_map_pair/2`, `c_map_pair_exact/2`.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec map_pair_key(Node :: c_map_pair()) -> cerl().

map_pair_key(#c_map_pair{key=K}) -> K.

-doc """
Returns the value subtree of an abstract map pair.

_See also: _`c_map_pair/2`, `c_map_pair_exact/2`.
""".
-doc(#{since => <<"OTP 17.0">>}).

-spec map_pair_val(Node :: c_map_pair()) -> cerl().

map_pair_val(#c_map_pair{val=V}) -> V.

-doc """
Returns the operation subtree of an abstract map pair.

_See also: _`c_map_pair/2`, `c_map_pair_exact/2`.
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec map_pair_op(Node :: c_map_pair()) -> map_op().

map_pair_op(#c_map_pair{op=Op}) -> Op.


%% ---------------------------------------------------------------------

-doc """
Creates an abstract tuple.

If `Elements` is `[E1, ..., En]`, the result represents "`{E1, ...,
En}`". Note that if all nodes in `Elements` have type `literal`, or if
`Elements` is empty, then the result will also have type `literal` and
annotations on nodes in `Elements` are lost.

Recall that Erlang has distinct 1-tuples, that is, `{X}` is always
distinct from `X` itself.

_See also: _`ann_c_tuple/2`, `c_tuple_skel/1`, `is_c_tuple/1`, `tuple_arity/1`,
`tuple_es/1`, `update_c_tuple/2`.
""".
-spec c_tuple(Elements :: [cerl()]) -> c_tuple() | c_literal().

%% *Always* collapse literals.

c_tuple(Es) ->
    case is_lit_list(Es) of
	false ->
	    #c_tuple{es = Es};
	true ->
	    #c_literal{val = list_to_tuple(lit_list_vals(Es))}
    end.


-doc "_See also: _`c_tuple/1`.".
-spec ann_c_tuple(Annotations :: [term()], Elements :: [cerl()]) ->
          c_tuple() | c_literal().

ann_c_tuple(As, Es) ->
    case is_lit_list(Es) of
	false ->
	    #c_tuple{es = Es, anno = As};
	true ->
	    #c_literal{val = list_to_tuple(lit_list_vals(Es)), anno = As}
    end.


-doc "_See also: _`c_tuple/1`.".
-spec update_c_tuple(Node :: c_tuple() | c_literal(),
                     Elements :: [cerl()]) -> c_tuple() | c_literal().

update_c_tuple(Node, Es) ->
    case is_lit_list(Es) of
	false ->
	    #c_tuple{es = Es, anno = get_ann(Node)};
	true ->
	    #c_literal{val = list_to_tuple(lit_list_vals(Es)),
		       anno = get_ann(Node)}
    end.


-doc """
Creates an abstract tuple skeleton.

Does not fold constant literals, that is, the result always has type
`tuple`, representing "`{E1, ..., En}`", if `Elements` is `[E1, ...,
En]`.

This function is occasionally useful when it is necessary to have annotations on
the subnodes of a tuple node, even when all the subnodes are constant literals.
However, note that [`is_literal/1`](`is_literal/1`) will yield `false` and
[`concrete/1`](`concrete/1`) will fail if passed the result from this function.

[`fold_literal/1`](`fold_literal/1`) can be used to revert a node to the
normal-form representation.

_See also: _`ann_c_tuple_skel/2`, `c_tuple/1`, `concrete/1`, `fold_literal/1`,
`is_c_tuple/1`, `is_literal/1`, `tuple_es/1`, `update_c_tuple_skel/2`.
""".
-spec c_tuple_skel(Elements :: [cerl()]) -> c_tuple().

%% *Never* collapse literals.

c_tuple_skel(Es) ->
    #c_tuple{es = Es}.


-doc "_See also: _`c_tuple_skel/1`.".
-spec ann_c_tuple_skel(Annotations :: [term()], Elements :: [cerl()]) -> c_tuple().

ann_c_tuple_skel(As, Es) ->
    #c_tuple{es = Es, anno = As}.


-doc "_See also: _`c_tuple_skel/1`.".
-spec update_c_tuple_skel(Node :: c_tuple(), Elements :: [cerl()]) -> c_tuple().

update_c_tuple_skel(Old, Es) ->
    #c_tuple{es = Es, anno = get_ann(Old)}.


-doc """
Returns `true` if `Node` is an abstract tuple, otherwise `false`.

_See also: _`c_tuple/1`.
""".
-spec is_c_tuple(Node :: cerl()) -> boolean().

is_c_tuple(#c_tuple{}) ->
    true;
is_c_tuple(#c_literal{val = V}) when is_tuple(V) ->
    true;
is_c_tuple(_) ->
    false.


-doc """
Returns the list of element subtrees of an abstract tuple.

_See also: _`c_tuple/1`.
""".
-spec tuple_es(Node :: c_tuple() | c_literal()) -> [cerl()].

tuple_es(#c_tuple{es = Es}) ->
    Es;
tuple_es(#c_literal{val = V}) ->
    make_lit_list(tuple_to_list(V)).


-doc """
Returns the number of element subtrees of an abstract tuple.

Note: this is equivalent to [`length(tuple_es(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_tuple/1`, `tuple_es/1`.
""".
-spec tuple_arity(Node :: c_tuple() | c_literal()) -> non_neg_integer().

tuple_arity(#c_tuple{es = Es}) ->
    length(Es);
tuple_arity(#c_literal{val = V}) when is_tuple(V) ->
    tuple_size(V).


%% ---------------------------------------------------------------------

-doc """
Creates an abstract variable.

A variable is identified by its name, given by the `Name` parameter.

If a name is given by a single atom, it should either be a "simple" atom which
does not need to be single-quoted in Erlang, or otherwise its print name should
correspond to a proper Erlang variable, that is, begin with an uppercase character
or an underscore. Names of the form `{A, N}` represent function name variables
"`A/N`"; these are special variables which may be bound only in the function
definitions of a module or a `letrec`. They may not be bound in `let`
expressions and cannot occur in clause patterns. The atom `A` in a function name
may be any atom; the integer `N` must be nonnegative. The functions
[`c_fname/2`](`c_fname/2`) etc. are utilities for handling function name
variables.

When printing variable names, they must have the form of proper Core Erlang
variables and function names. E.g., a name represented by an integer such as
`42` could be formatted as "`_42`", an atom `'Xxx'` simply as "`Xxx`", and an
atom `foo` as "`_foo`". However, one must assure that any two valid distinct
names are never mapped to the same strings. Tuples such as `{foo, 2}`
representing function names can simply by formatted as "`'foo'/2`", with no risk
of conflicts.

_See also: _`ann_c_var/2`, `c_fname/2`, `c_letrec/2`, `c_module/4`,
`is_c_var/1`, `update_c_var/2`, `var_name/1`.
""".
-spec c_var(Name :: var_name()) -> c_var().

c_var(Name) ->
    #c_var{name = Name}.


-doc "_See also: _`c_var/1`.".
-spec ann_c_var(Annotations :: [term()], Name :: var_name()) -> c_var().

ann_c_var(As, Name) ->
    #c_var{name = Name, anno = As}.

-doc "_See also: _`c_var/1`.".
-spec update_c_var(Node :: c_var(), Name :: var_name()) -> c_var().

update_c_var(Node, Name) ->
    #c_var{name = Name, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract variable, otherwise `false`.

_See also: _`c_var/1`.
""".
-spec is_c_var(Node :: cerl()) -> boolean().

is_c_var(#c_var{}) ->
    true;
is_c_var(_) ->
    false.


-doc """
Equivalent to [c_var(\{Name, Arity\})](`c_var/1`).

_See also: _`ann_c_fname/3`, `fname_arity/1`, `fname_id/1`, `is_c_fname/1`,
`update_c_fname/3`.
""".
-spec c_fname(Name :: atom(), Arity :: arity()) -> c_var().

c_fname(Atom, Arity) ->
    c_var({Atom, Arity}).


-doc """
Equivalent to [ann_c_var(As, \{Atom, Arity\})](`ann_c_var/2`).

_See also: _`c_fname/2`.
""".
-spec ann_c_fname(Annotations :: [term()], Name :: atom(),
                  Arity :: arity()) -> c_var().

ann_c_fname(As, Atom, Arity) ->
    ann_c_var(As, {Atom, Arity}).


-doc """
Like [`update_c_fname/3`](`update_c_fname/3`), but takes the arity from `Node`.

_See also: _`c_fname/2`, `update_c_fname/3`.
""".
-spec update_c_fname(Node :: c_var(), Name :: atom()) -> c_var().

update_c_fname(#c_var{name = {_, Arity}, anno = As}, Atom) ->
    #c_var{name = {Atom, Arity}, anno = As}.


-doc """
Equivalent to [update_c_var(Old, \{Atom, Arity\})](`update_c_var/2`).

_See also: _`c_fname/2`, `update_c_fname/2`.
""".
-spec update_c_fname(Node :: c_var(), Name :: atom(), Arity :: arity()) -> c_var().

update_c_fname(Node, Atom, Arity) ->
    update_c_var(Node, {Atom, Arity}).


-doc """
Returns `true` if `Node` is an abstract function name variable, otherwise
`false`.

_See also: _`c_fname/2`, `c_var/1`, `var_name/1`.
""".
-spec is_c_fname(Node :: cerl()) -> boolean().

is_c_fname(#c_var{name = {A, N}}) when is_atom(A), is_integer(N), N >= 0 ->
    true;
is_c_fname(_) ->
    false.


-doc """
Returns the name of an abstract variable.

_See also: _`c_var/1`.
""".
-spec var_name(Node :: c_var()) -> var_name().

var_name(Node) ->
    Node#c_var.name.


-doc """
Returns the identifier part of an abstract function name variable.

_See also: _`c_fname/2`, `fname_arity/1`.
""".
-spec fname_id(Node :: c_var()) -> atom().

fname_id(#c_var{name={A,_}}) ->
    A.


-doc """
Returns the arity part of an abstract function name variable.

_See also: _`c_fname/2`, `fname_id/1`.
""".
-spec fname_arity(Node :: c_var()) -> arity().

fname_arity(#c_var{name={_,N}}) ->
    N.


%% ---------------------------------------------------------------------

-doc """
Creates an abstract value list.

If `Elements` is `[E1, ..., En]`, the result represents "`<E1, ...,
En>`".

_See also: _`ann_c_values/2`, `is_c_values/1`, `update_c_values/2`,
`values_arity/1`, `values_es/1`.
""".
-spec c_values(Elements :: [cerl()]) -> c_values().

c_values(Es) ->
    #c_values{es = Es}.


-doc "_See also: _`c_values/1`.".
-spec ann_c_values(Annotations :: [term()], Values :: [cerl()]) -> c_values().

ann_c_values(As, Es) ->
    #c_values{es = Es, anno = As}.


-doc "_See also: _`c_values/1`.".
-spec update_c_values(Node :: c_values(), Elements :: [cerl()]) -> c_values().

update_c_values(Node, Es) ->
    #c_values{es = Es, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract value list, otherwise `false`.

_See also: _`c_values/1`.
""".
-spec is_c_values(Node :: cerl()) -> boolean().

is_c_values(#c_values{}) ->
    true;
is_c_values(_) ->
    false.


-doc """
Returns the list of element subtrees of an abstract value list.

_See also: _`c_values/1`, `values_arity/1`.
""".
-spec values_es(Node :: c_values()) -> [cerl()].

values_es(Node) ->
    Node#c_values.es.


-doc """
Returns the number of element subtrees of an abstract value list.

Note: This is equivalent to [`length(values_es(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_values/1`, `values_es/1`.
""".
-spec values_arity(Node :: c_values()) -> non_neg_integer().

values_arity(Node) ->
    length(values_es(Node)).


%% ---------------------------------------------------------------------

-doc """
Creates an abstract binary-template.

A binary object is in this context is a sequence of an arbitrary
number of bits. (The number of bits used to be evenly divisible by 8,
but after the introduction of bit strings in the Erlang language, the
choice was made to use the binary template for all bit strings.)  It
is specified by zero or more bit-string template _segments_ of
arbitrary lengths (in number of bits).

If `Segments` is `[S1, ..., Sn]`, the result represents "`#{S1, ...,
Sn}#`". All the `Si` must have type `bitstr`.

_See also: _`ann_c_binary/2`, `binary_segments/1`, `c_bitstr/5`,
`is_c_binary/1`, `update_c_binary/2`.
""".
-spec c_binary(Segments :: [cerl()]) -> c_binary().

c_binary(Segments) ->
    #c_binary{segments = Segments}.


-doc "_See also: _`c_binary/1`.".
-spec ann_c_binary(Annotations :: [term()], Segments :: [cerl()]) -> c_binary().

ann_c_binary(As, Segments) ->
    #c_binary{segments = Segments, anno = As}.


-doc "_See also: _`c_binary/1`.".
-spec update_c_binary(Node :: c_binary(), Segments :: [cerl()]) -> c_binary().

update_c_binary(Node, Segments) ->
    #c_binary{segments = Segments, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract binary-template, otherwise `false`.

_See also: _`c_binary/1`.
""".
-spec is_c_binary(Node :: cerl()) -> boolean().

is_c_binary(#c_binary{}) ->
    true;
is_c_binary(_) ->
    false.


-doc """
Returns the list of segment subtrees of an abstract binary-template.

_See also: _`c_binary/1`, `c_bitstr/5`.
""".
-spec binary_segments(Node :: c_binary()) -> [cerl()].

binary_segments(Node) ->
    Node#c_binary.segments.


-doc """
Creates an abstract bit-string template.

These can only occur as components of an abstract binary-template (see
`c_binary/1`). The result represents "`#<Value>(Size, Unit, Type,
Flags)`", where `Unit` must represent a positive integer constant,
`Type` must represent a constant atom (one of `'integer'`, `'float'`,
`'binary'`, `'utf8'`, `'utf16'` or `'utf32'`), and `Flags` must
represent a constant list `"[F1, ..., Fn]"` where all the `Fi` are
atoms.

_See also: _`ann_c_bitstr/6`, `bitstr_flags/1`, `bitstr_size/1`,
`bitstr_type/1`, `bitstr_unit/1`, `bitstr_val/1`, `c_binary/1`, `is_c_bitstr/1`,
`update_c_bitstr/6`.
""".
-spec c_bitstr(Value :: cerl(), Size :: cerl(), Unit :: cerl(),
               Type :: cerl(), Flags :: cerl()) -> c_bitstr().

c_bitstr(Val, Size, Unit, Type, Flags) ->
    #c_bitstr{val = Val, size = Size, unit = Unit, type = Type,
	      flags = Flags}.


-doc #{equiv => c_bitstr(Value, Size, abstract(1), Type, Flags)}.
-spec c_bitstr(Value :: cerl(), Size :: cerl(),
               Type :: cerl(), Flags ::cerl()) -> c_bitstr().

c_bitstr(Val, Size, Type, Flags) ->
    c_bitstr(Val, Size, abstract(1), Type, Flags).


-doc """
Equivalent to
[c_bitstr(Value, abstract(all), abstract(1), Type, Flags)](`c_bitstr/5`).
""".
-spec c_bitstr(Value :: cerl(), Type :: cerl(), Flags:: cerl()) -> c_bitstr().

c_bitstr(Val, Type, Flags) ->
    c_bitstr(Val, abstract(all), abstract(1), Type, Flags).


-doc "_See also: _`ann_c_bitstr/5`, `c_bitstr/5`.".
-spec ann_c_bitstr(Annotations :: [term()], Value ::cerl(), Size :: cerl(),
                   Unit :: cerl(), Type :: cerl(), Flags :: cerl()) ->
          c_bitstr().

ann_c_bitstr(As, Val, Size, Unit, Type, Flags) ->
    #c_bitstr{val = Val, size = Size, unit = Unit, type = Type,
	      flags = Flags, anno = As}.

-doc """
Equivalent to
[ann_c_bitstr(As, Value, Size, abstract(1), Type, Flags)](`ann_c_bitstr/6`).
""".
-spec ann_c_bitstr(Annotations :: [term()], Value :: cerl(), Size :: cerl(),
                   Type :: cerl(), Flags :: cerl()) -> c_bitstr().

ann_c_bitstr(As, Value, Size, Type, Flags) ->
    ann_c_bitstr(As, Value, Size, abstract(1), Type, Flags).


-doc "_See also: _`c_bitstr/5`, `update_c_bitstr/5`.".
-spec update_c_bitstr(Node :: c_bitstr(), Value :: cerl(), Size ::cerl(),
                      Unit:: cerl(), Type :: cerl(), Flags ::cerl()) ->
          c_bitstr().

update_c_bitstr(Node, Val, Size, Unit, Type, Flags) ->
    #c_bitstr{val = Val, size = Size, unit = Unit, type = Type,
	     flags = Flags, anno = get_ann(Node)}.


-doc """
Equivalent to
[update_c_bitstr(Node, Value, Size, abstract(1), Type, Flags)](`update_c_bitstr/6`).
""".
-spec update_c_bitstr(Node :: c_bitstr(), Value :: cerl(), Size :: cerl(),
                      Type :: cerl(), Flags ::cerl()) -> c_bitstr().

update_c_bitstr(Node, Value, Size, Type, Flags) ->
    update_c_bitstr(Node, Value, Size, abstract(1), Type, Flags).

-doc """
Returns `true` if `Node` is an abstract bit-string template, otherwise `false`.

_See also: _`c_bitstr/5`.
""".
-spec is_c_bitstr(Node :: cerl()) -> boolean().

is_c_bitstr(#c_bitstr{}) ->
    true;
is_c_bitstr(_) ->
    false.


-doc """
Returns the value subtree of an abstract bit-string template.

_See also: _`c_bitstr/5`.
""".
-spec bitstr_val(Node :: c_bitstr()) -> cerl().

bitstr_val(Node) ->
    Node#c_bitstr.val.


-doc """
Returns the size subtree of an abstract bit-string template.

_See also: _`c_bitstr/5`.
""".
-spec bitstr_size(Node :: c_bitstr()) -> cerl().

bitstr_size(Node) ->
    Node#c_bitstr.size.


-doc """
Returns the total size in bits of an abstract bit-string template.

If the size field is an integer literal, the result is the product of
the size and unit values; if the size field is the atom literal `all`,
the atom `all` is returned.  If the size is not a literal, the atom
`any` is returned.  If the type of the bit-string segment is one of
`utf8`, `utf16` or `utf32`, the atom `utf` is returned.

_See also: _`c_bitstr/5`.
""".
-spec bitstr_bitsize(Node :: c_bitstr()) -> 'all' | 'any' | 'utf' | non_neg_integer().

bitstr_bitsize(Node) ->
    #c_bitstr{size=Size,type=Type,unit=Unit} = Node,
    case is_literal(Size) of
        true ->
            case {concrete(Size), concrete(Type)} of
                {all, binary} ->
                    all;
                {undefined, T} when T =:= utf8; T =:= utf16; T =:= utf32 ->
                    utf;
                {S, _} when is_integer(S), S >= 0 ->
                    S * concrete(Unit);
                {_, _} ->
                    %% Bogus literal size, fails in runtime.
                    any
            end;
        false ->
            any
    end.


-doc """
Returns the unit subtree of an abstract bit-string template.

_See also: _`c_bitstr/5`.
""".
-spec bitstr_unit(Node :: c_bitstr()) -> cerl().

bitstr_unit(Node) ->
    Node#c_bitstr.unit.


-doc """
Returns the type subtree of an abstract bit-string template.

_See also: _`c_bitstr/5`.
""".
-spec bitstr_type(Node :: c_bitstr()) -> cerl().

bitstr_type(Node) ->
    Node#c_bitstr.type.


-doc """
Returns the flags subtree of an abstract bit-string template.

_See also: _`c_bitstr/5`.
""".
-spec bitstr_flags(Node :: c_bitstr()) -> cerl().

bitstr_flags(Node) ->
    Node#c_bitstr.flags.


%% ---------------------------------------------------------------------

-doc """
Creates an abstract fun-expression.

If `Variables` is `[V1, ..., Vn]`, the result represents "`fun (V1,
..., Vn) -> Body`". All the `Vi` must have type `var`.

_See also: _`ann_c_fun/3`, `fun_arity/1`, `fun_body/1`, `fun_vars/1`,
`is_c_fun/1`, `update_c_fun/3`.
""".
-spec c_fun(Variables :: [cerl()], Body :: cerl()) -> c_fun().

c_fun(Variables, Body) ->
    #c_fun{vars = Variables, body = Body}.


-doc "_See also: _`c_fun/2`.".
-spec ann_c_fun(Annotations :: [term()], Variables :: [cerl()],
                Body ::cerl()) -> c_fun().

ann_c_fun(As, Variables, Body) ->
    #c_fun{vars = Variables, body = Body, anno = As}.


-doc "_See also: _`c_fun/2`.".
-spec update_c_fun(Node :: c_fun(), Variables :: [cerl()],
                   Body :: cerl()) -> c_fun().

update_c_fun(Node, Variables, Body) ->
    #c_fun{vars = Variables, body = Body, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract fun-expression, otherwise `false`.

_See also: _`c_fun/2`.
""".
-spec is_c_fun(Node :: cerl()) -> boolean().

is_c_fun(#c_fun{}) ->
    true;		% Now this is fun!
is_c_fun(_) ->
    false.


-doc """
Returns the list of parameter subtrees of an abstract fun-expression.

_See also: _`c_fun/2`, `fun_arity/1`.
""".
-spec fun_vars(Node :: c_fun()) -> [cerl()].

fun_vars(Node) ->
    Node#c_fun.vars.


-doc """
Returns the body subtree of an abstract fun-expression.

_See also: _`c_fun/2`.
""".
-spec fun_body(Node :: c_fun()) -> cerl().

fun_body(Node) ->
    Node#c_fun.body.


-doc """
Returns the number of parameter subtrees of an abstract fun-expression.

Note: this is equivalent to [`length(fun_vars(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_fun/2`, `fun_vars/1`.
""".
-spec fun_arity(Node :: c_fun()) -> arity().

fun_arity(Node) ->
    length(fun_vars(Node)).


%% ---------------------------------------------------------------------

-doc """
Creates an abstract sequencing expression.

The result represents "`do Argument Body`".

_See also: _`ann_c_seq/3`, `is_c_seq/1`, `seq_arg/1`, `seq_body/1`,
`update_c_seq/3`.
""".
-spec c_seq(Argument :: cerl(), Body ::cerl()) -> c_seq().

c_seq(Argument, Body) ->
    #c_seq{arg = Argument, body = Body}.


-doc "_See also: _`c_seq/2`.".
-spec ann_c_seq(Annotations :: [term()], Argument :: cerl(),
                Body :: cerl()) -> c_seq().

ann_c_seq(As, Argument, Body) ->
    #c_seq{arg = Argument, body = Body, anno = As}.


-doc "_See also: _`c_seq/2`.".
-spec update_c_seq(Node :: c_seq(), Argument :: cerl(), Body ::cerl()) -> c_seq().

update_c_seq(Node, Argument, Body) ->
    #c_seq{arg = Argument, body = Body, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract sequencing expression, otherwise
`false`.

_See also: _`c_seq/2`.
""".
-spec is_c_seq(Node :: cerl()) -> boolean().

is_c_seq(#c_seq{}) ->
    true;
is_c_seq(_) ->
    false.


-doc """
Returns the argument subtree of an abstract sequencing expression.

_See also: _`c_seq/2`.
""".
-spec seq_arg(Node :: c_seq()) -> cerl().

seq_arg(Node) ->
    Node#c_seq.arg.


-doc """
Returns the body subtree of an abstract sequencing expression.

_See also: _`c_seq/2`.
""".
-spec seq_body(Node :: c_seq()) -> cerl().

seq_body(Node) ->
    Node#c_seq.body.


%% ---------------------------------------------------------------------

-doc """
Creates an abstract let-expression.

If `Variables` is `[V1, ..., Vn]`, the result represents "`let <V1,
..., Vn> = Argument in Body`". All the `Vi` must have type `var`.

_See also: _`ann_c_let/4`, `is_c_let/1`, `let_arg/1`, `let_arity/1`,
`let_body/1`, `let_vars/1`, `update_c_let/4`.
""".
-spec c_let(Variables :: [cerl()], Argument :: cerl(), Body :: cerl()) -> c_let().

c_let(Variables, Argument, Body) ->
    #c_let{vars = Variables, arg = Argument, body = Body}.


-doc "_See also: _`c_let/3`.".
-spec ann_c_let(Annotations :: [term()], Variables :: [cerl()],
                Argument ::cerl(), Body :: cerl()) -> c_let().

ann_c_let(As, Variables, Argument, Body) ->
    #c_let{vars = Variables, arg = Argument, body = Body, anno = As}.


-doc "_See also: _`c_let/3`.".
-spec update_c_let(Node :: c_let(), Variables :: [cerl()],
                   Argument :: cerl(), Body :: cerl()) -> c_let().

update_c_let(Node, Variables, Argument, Body) ->
    #c_let{vars = Variables, arg = Argument, body = Body,
	   anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract let-expression, otherwise `false`.

_See also: _`c_let/3`.
""".
-spec is_c_let(Node :: cerl()) -> boolean().

is_c_let(#c_let{}) ->
    true;
is_c_let(_) ->
    false.


-doc """
Returns the list of left-hand side variables of an abstract let-expression.

_See also: _`c_let/3`, `let_arity/1`.
""".
-spec let_vars(Node :: c_let()) -> [cerl()].

let_vars(Node) ->
    Node#c_let.vars.


-doc """
Returns the argument subtree of an abstract let-expression.

_See also: _`c_let/3`.
""".
-spec let_arg(Node :: c_let()) -> cerl().

let_arg(Node) ->
    Node#c_let.arg.


-doc """
Returns the body subtree of an abstract let-expression.

_See also: _`c_let/3`.
""".
-spec let_body(Node :: c_let()) -> cerl().

let_body(Node) ->
    Node#c_let.body.


-doc """
Returns the number of left-hand side variables of an abstract let-expression.

Note: this is equivalent to [`length(let_vars(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_let/3`, `let_vars/1`.
""".
-spec let_arity(Node :: c_let()) -> non_neg_integer().

let_arity(Node) ->
    length(let_vars(Node)).


%% ---------------------------------------------------------------------

-doc """
Creates an abstract letrec-expression.

If `Definitions` is `[{V1, F1}, ..., {Vn, Fn}]`, the result represents
"`letrec V1 = F1 ... Vn = Fn in Body`". All the `Vi` must have type
`var` and represent function names. All the `Fi` must have type
`'fun'`.

_See also: _`ann_c_letrec/3`, `is_c_letrec/1`, `letrec_body/1`, `letrec_defs/1`,
`letrec_vars/1`, `update_c_letrec/3`.
""".
-spec c_letrec(Definitions :: [{cerl(), cerl()}], Body :: cerl()) -> c_letrec().

c_letrec(Defs, Body) ->
    #c_letrec{defs = Defs, body = Body}.


-doc "_See also: _`c_letrec/2`.".
-spec ann_c_letrec(Annotations :: [term()],
                   Definitions ::[{cerl(), cerl()}],
                   Body :: cerl()) -> c_letrec().

ann_c_letrec(As, Defs, Body) ->
    #c_letrec{defs = Defs, body = Body, anno = As}.


-doc "_See also: _`c_letrec/2`.".
-spec update_c_letrec(Node :: c_letrec(),
                      Definitions :: [{cerl(), cerl()}],
                      Body ::cerl()) -> c_letrec().

update_c_letrec(Node, Defs, Body) ->
    #c_letrec{defs = Defs, body = Body, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract letrec-expression, otherwise `false`.

_See also: _`c_letrec/2`.
""".
-spec is_c_letrec(Node :: cerl()) -> boolean().

is_c_letrec(#c_letrec{}) ->
    true;
is_c_letrec(_) ->
    false.


-doc """
Returns the list of definitions of an abstract letrec-expression.

If `Node` represents "`letrec V1 = F1 ... Vn = Fn in Body`", the
returned value is `[{V1, F1}, ..., {Vn, Fn}]`.

_See also: _`c_letrec/2`.
""".
-spec letrec_defs(Node :: c_letrec()) -> [{cerl(), cerl()}].

letrec_defs(Node) ->
    Node#c_letrec.defs.


-doc """
Returns the body subtree of an abstract letrec-expression.

_See also: _`c_letrec/2`.
""".
-spec letrec_body(Node :: c_letrec()) -> cerl().

letrec_body(Node) ->
    Node#c_letrec.body.


-doc """
Returns the list of left-hand side function variable subtrees of a
letrec-expression.

If `Node` represents§ "`letrec V1 = F1 ... Vn = Fn in Body`", the
returned value is `[V1, ..., Vn]`.

_See also: _`c_letrec/2`.
""".
-spec letrec_vars(Node :: c_letrec()) -> [cerl()].

letrec_vars(Node) ->
    [F || {F, _} <:- letrec_defs(Node)].


%% ---------------------------------------------------------------------

-doc """
Creates an abstract case-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`case Argument
of C1 ... Cn end`". `Clauses` must not be empty.

_See also: _`ann_c_case/3`, `c_clause/3`, `case_arg/1`, `case_arity/1`,
`case_clauses/1`, `is_c_case/1`, `update_c_case/3`.
""".
-spec c_case(Argument :: cerl(), Clauses :: [cerl()]) -> c_case().

c_case(Expr, Clauses) ->
    #c_case{arg = Expr, clauses = Clauses}.


-doc "_See also: _`c_case/2`.".
-spec ann_c_case(Annotations :: [term()], Argument :: cerl(),
                 Clauses :: [cerl()]) -> c_case().

ann_c_case(As, Expr, Clauses) ->
    #c_case{arg = Expr, clauses = Clauses, anno = As}.


-doc "_See also: _`c_case/2`.".
-spec update_c_case(Node :: c_case(), Argument :: cerl(),
                    Clauses ::[cerl()]) -> c_case().

update_c_case(Node, Expr, Clauses) ->
    #c_case{arg = Expr, clauses = Clauses, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract case-expression, otherwise `false`.

_See also: _`c_case/2`.
""".
-spec is_c_case(Node :: cerl()) -> boolean().

is_c_case(#c_case{}) ->
    true;
is_c_case(_) ->
    false.


-doc """
Returns the argument subtree of an abstract case-expression.

_See also: _`c_case/2`.
""".
-spec case_arg(Node :: c_case()) -> cerl().

case_arg(Node) ->
    Node#c_case.arg.


-doc """
Returns the list of clause subtrees of an abstract case-expression.

_See also: _`c_case/2`, `case_arity/1`.
""".
-spec case_clauses(Node :: c_case()) -> [cerl()].

case_clauses(Node) ->
    Node#c_case.clauses.


-doc """
Equivalent to [`clause_arity(hd(case_clauses(Node)))`](`clause_arity/1`), but
potentially more efficient.

_See also: _`c_case/2`, `case_clauses/1`, `clause_arity/1`.
""".
-spec case_arity(Node :: c_case()) -> non_neg_integer().

case_arity(Node) ->
    clause_arity(hd(case_clauses(Node))).


%% ---------------------------------------------------------------------

-doc """
Equivalent to [c_clause(Patterns, c_atom(true), Body)](`c_clause/3`).

_See also: _`c_atom/1`.
""".
-spec c_clause(Patterns :: [cerl()], Body ::cerl()) -> c_clause().

c_clause(Patterns, Body) ->
    c_clause(Patterns, c_atom(true), Body).


-doc """
Creates an an abstract clause.

If `Patterns` is `[P1, ..., Pn]`, the result represents "`<P1, ...,
Pn> when Guard -> Body`".

_See also: _`ann_c_clause/4`, `c_case/2`, `c_clause/2`, `c_receive/3`,
`clause_arity/1`, `clause_body/1`, `clause_guard/1`, `clause_pats/1`,
`clause_vars/1`, `is_c_clause/1`, `update_c_clause/4`.
""".
-spec c_clause(Patterns :: [cerl()], Guard :: cerl(),
               Body :: cerl()) -> c_clause().

c_clause(Patterns, Guard, Body) ->
    #c_clause{pats = Patterns, guard = Guard, body = Body}.


-doc """
Equivalent to
[ann_c_clause(As, Patterns, c_atom(true), Body)](`ann_c_clause/4`).

_See also: _`c_clause/3`.
""".
-spec ann_c_clause(Annotations :: [term()], Patterns ::[cerl()],
                   Body :: cerl()) -> c_clause().

ann_c_clause(As, Patterns, Body) ->
    ann_c_clause(As, Patterns, c_atom(true), Body).


-doc "_See also: _`ann_c_clause/3`, `c_clause/3`.".
-spec ann_c_clause(Annotations :: [term()], Patterns :: [cerl()],
                   Guard :: cerl(), Body ::cerl()) -> c_clause().

ann_c_clause(As, Patterns, Guard, Body) ->
    #c_clause{pats = Patterns, guard = Guard, body = Body, anno = As}.


-doc "_See also: _`c_clause/3`.".
-spec update_c_clause(Node :: c_clause(), Patterns ::[cerl()],
                      Guard ::cerl(), Body :: cerl()) -> c_clause().

update_c_clause(Node, Patterns, Guard, Body) ->
    #c_clause{pats = Patterns, guard = Guard, body = Body,
	      anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract clause, otherwise `false`.

_See also: _`c_clause/3`.
""".
-spec is_c_clause(Node :: cerl()) -> boolean().

is_c_clause(#c_clause{}) ->
    true;
is_c_clause(_) ->
    false.


-doc """
Returns the list of pattern subtrees of an abstract clause.

_See also: _`c_clause/3`, `clause_arity/1`.
""".
-spec clause_pats(Node :: c_clause()) -> [cerl()].

clause_pats(Node) ->
    Node#c_clause.pats.


-doc """
Returns the guard subtree of an abstract clause.

_See also: _`c_clause/3`.
""".
-spec clause_guard(Node :: c_clause()) -> cerl().

clause_guard(Node) ->
    Node#c_clause.guard.


-doc """
Returns the body subtree of an abstract clause.

_See also: _`c_clause/3`.
""".
-spec clause_body(Node :: c_clause()) -> cerl().

clause_body(Node) ->
    Node#c_clause.body.


-doc """
Returns the number of pattern subtrees of an abstract clause.

Note: this is equivalent to [`length(clause_pats(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_clause/3`, `clause_pats/1`.
""".
-spec clause_arity(Node :: c_clause()) -> non_neg_integer().

clause_arity(Node) ->
    length(clause_pats(Node)).


-doc """
Returns the list of all abstract variables in the patterns of an abstract
clause.

The order of listing is not defined.

_See also: _`c_clause/3`, `pat_list_vars/1`.
""".
-spec clause_vars(Node :: c_clause()) -> [cerl()].

clause_vars(Clause) ->
    pat_list_vars(clause_pats(Clause)).


-doc """
Returns the list of all abstract variables in a pattern.

An exception is thrown if `Node` does not represent a well-formed Core
Erlang clause pattern. The order of listing is not defined.

_See also: _`clause_vars/1`, `pat_list_vars/1`.
""".
-spec pat_vars(Node :: cerl()) -> [cerl()].

pat_vars(Node) ->
    pat_vars(Node, []).

pat_vars(Node, Vs) ->
    case type(Node) of
	var ->
	    [Node | Vs];
	literal ->
	    Vs;
	cons ->
	    pat_vars(cons_hd(Node), pat_vars(cons_tl(Node), Vs));
	tuple ->
	    pat_list_vars(tuple_es(Node), Vs);
	map ->
	    pat_list_vars(map_es(Node), Vs);
	map_pair ->
	    %% map_pair_key is not a pattern var, excluded
	    pat_list_vars([map_pair_op(Node),map_pair_val(Node)],Vs);
	binary ->
	    pat_list_vars(binary_segments(Node), Vs);
	bitstr ->
	    %% bitstr_size is not a pattern var, excluded
	    pat_vars(bitstr_val(Node), Vs);
	alias ->
	    pat_vars(alias_pat(Node), [alias_var(Node) | Vs])
    end.


-doc """
Returns the list of all abstract variables in the given patterns.

An exception is thrown if some element in `Patterns` does not
represent a well-formed Core Erlang clause pattern. The order of
listing is not defined.

_See also: _`clause_vars/1`, `pat_vars/1`.
""".
-spec pat_list_vars(Patterns :: [cerl()]) -> [cerl()].

pat_list_vars(Ps) ->
    pat_list_vars(Ps, []).

pat_list_vars([P | Ps], Vs) ->
    pat_list_vars(Ps, pat_vars(P, Vs));
pat_list_vars([], Vs) ->
    Vs.


%% ---------------------------------------------------------------------

-doc """
Creates an abstract pattern alias.

The result represents "`Variable = Pattern`".

_See also: _`alias_pat/1`, `alias_var/1`, `ann_c_alias/3`, `c_clause/3`,
`is_c_alias/1`, `update_c_alias/3`.
""".
-spec c_alias(Variable :: c_var(), Pattern :: cerl()) -> c_alias().

c_alias(Var, Pattern) ->
    #c_alias{var = Var, pat = Pattern}.


-doc "_See also: _`c_alias/2`.".
-spec ann_c_alias(Annotations :: [term()], Variable :: c_var(),
                  Pattern :: cerl()) -> c_alias().

ann_c_alias(As, Var, Pattern) ->
    #c_alias{var = Var, pat = Pattern, anno = As}.


-doc "_See also: _`c_alias/2`.".
-spec update_c_alias(Node :: c_alias(), Variable :: cerl(),
                     Pattern :: cerl()) -> c_alias().

update_c_alias(Node, Var, Pattern) ->
    #c_alias{var = Var, pat = Pattern, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract pattern alias, otherwise `false`.

_See also: _`c_alias/2`.
""".
-spec is_c_alias(Node :: cerl()) -> boolean().

is_c_alias(#c_alias{}) ->
    true;
is_c_alias(_) ->
    false.


-doc """
Returns the variable subtree of an abstract pattern alias.

_See also: _`c_alias/2`.
""".
-spec alias_var(Node :: c_alias()) -> c_var().

alias_var(Node) ->
    Node#c_alias.var.


-doc """
Returns the pattern subtree of an abstract pattern alias.

_See also: _`c_alias/2`.
""".
-spec alias_pat(Node :: c_alias()) -> cerl().

alias_pat(Node) ->
    Node#c_alias.pat.


%% ---------------------------------------------------------------------

-doc """
Equivalent to
[c_receive(Clauses, c_atom(infinity), c_atom(true))](`c_receive/3`).

_See also: _`c_atom/1`.
""".
-spec c_receive(Clauses :: [cerl()]) -> c_receive().

c_receive(Clauses) ->
    c_receive(Clauses, c_atom(infinity), c_atom(true)).


-doc """
Creates an abstract receive-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`receive C1
... Cn after Timeout -> Action end`".

_See also: _`ann_c_receive/4`, `c_receive/1`, `is_c_receive/1`,
`receive_action/1`, `receive_clauses/1`, `receive_timeout/1`,
`update_c_receive/4`.
""".
-spec c_receive(Clauses :: [cerl()], Timeout :: cerl(),
                Action ::cerl()) -> c_receive().

c_receive(Clauses, Timeout, Action) ->
    #c_receive{clauses = Clauses, timeout = Timeout, action = Action}.


-doc """
Equivalent to
[ann_c_receive(As, Clauses, c_atom(infinity), c_atom(true))](`ann_c_receive/4`).

_See also: _`c_atom/1`, `c_receive/3`.
""".
-spec ann_c_receive(Annotations :: [term()], Clauses :: [cerl()]) -> c_receive().

ann_c_receive(As, Clauses) ->
    ann_c_receive(As, Clauses, c_atom(infinity), c_atom(true)).


-doc "_See also: _`ann_c_receive/2`, `c_receive/3`.".
-spec ann_c_receive(Annotations :: [term()], Clauses :: [cerl()],
                    Timeout :: cerl(), Actions :: cerl()) -> c_receive().

ann_c_receive(As, Clauses, Timeout, Action) ->
    #c_receive{clauses = Clauses, timeout = Timeout, action = Action,
	       anno = As}.


-doc "_See also: _`c_receive/3`.".
-spec update_c_receive(Node :: c_receive(), Clauses :: [cerl()],
                       Timeout :: cerl(), Action :: cerl()) -> c_receive().

update_c_receive(Node, Clauses, Timeout, Action) ->
    #c_receive{clauses = Clauses, timeout = Timeout, action = Action,
	       anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract receive-expression, otherwise `false`.

_See also: _`c_receive/3`.
""".
-spec is_c_receive(Node :: cerl()) -> boolean().

is_c_receive(#c_receive{}) ->
    true;
is_c_receive(_) ->
    false.


-doc """
Returns the list of clause subtrees of an abstract receive-expression.

_See also: _`c_receive/3`.
""".
-spec receive_clauses(Node :: c_receive()) -> [cerl()].

receive_clauses(Node) ->
    Node#c_receive.clauses.


-doc """
Returns the timeout subtree of an abstract receive-expression.

_See also: _`c_receive/3`.
""".
-spec receive_timeout(Node :: c_receive()) -> cerl().

receive_timeout(Node) ->
    Node#c_receive.timeout.


-doc """
Returns the action subtree of an abstract receive-expression.

_See also: _`c_receive/3`.
""".
-spec receive_action(Node :: c_receive()) -> cerl().

receive_action(Node) ->
    Node#c_receive.action.


%% ---------------------------------------------------------------------

-doc """
Creates an abstract function application.

If `Arguments` is `[A1, ..., An]`, the result represents "`apply
Operator(A1, ..., An)`".

_See also: _`ann_c_apply/3`, `apply_args/1`, `apply_arity/1`, `apply_op/1`,
`c_call/3`, `c_primop/2`, `is_c_apply/1`, `update_c_apply/3`.
""".
-spec c_apply(Operator :: cerl(), Arguments :: [cerl()]) -> c_apply().

c_apply(Operator, Arguments) ->
    #c_apply{op = Operator, args = Arguments}.


-doc "_See also: _`c_apply/2`.".
-spec ann_c_apply(Annotations :: [term()], Operator :: cerl(),
                  Arguments :: [cerl()]) -> c_apply().

ann_c_apply(As, Operator, Arguments) ->
    #c_apply{op = Operator, args = Arguments, anno = As}.


-doc "_See also: _`c_apply/2`.".
-spec update_c_apply(Node :: c_apply(), Operator :: cerl(),
                     Arguments :: [cerl()]) -> c_apply().

update_c_apply(Node, Operator, Arguments) ->
    #c_apply{op = Operator, args = Arguments, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract function application, otherwise `false`.

_See also: _`c_apply/2`.
""".
-spec is_c_apply(Node :: cerl()) -> boolean().

is_c_apply(#c_apply{}) ->
    true;
is_c_apply(_) ->
    false.


-doc """
Returns the operator subtree of an abstract function application.

_See also: _`c_apply/2`.
""".
-spec apply_op(Node :: c_apply()) -> cerl().

apply_op(Node) ->
    Node#c_apply.op.


-doc """
Returns the list of argument subtrees of an abstract function application.

_See also: _`apply_arity/1`, `c_apply/2`.
""".
-spec apply_args(Node :: c_apply()) -> [cerl()].

apply_args(Node) ->
    Node#c_apply.args.


-doc """
Returns the number of argument subtrees of an abstract function application.

Note: this is equivalent to [`length(apply_args(Node))`](`length/1`), but
potentially more efficient.

_See also: _`apply_args/1`, `c_apply/2`.
""".
-spec apply_arity(Node :: c_apply()) -> arity().

apply_arity(Node) ->
    length(apply_args(Node)).


%% ---------------------------------------------------------------------

-doc """
Creates an abstract inter-module call.

If `Arguments` is `[A1, ..., An]`, the result represents "`call
Module:Name(A1, ..., An)`".

_See also: _`ann_c_call/4`, `c_apply/2`, `c_primop/2`, `call_args/1`,
`call_arity/1`, `call_module/1`, `call_name/1`, `is_c_call/1`,
`update_c_call/4`.
""".
-spec c_call(Module :: cerl(), Name :: cerl(),
             Arguments :: [cerl()]) -> c_call().

c_call(Module, Name, Arguments) ->
    #c_call{module = Module, name = Name, args = Arguments}.


-doc "_See also: _`c_call/3`.".
-spec ann_c_call(Annotations :: [term()],
                 Module :: cerl(), Name :: cerl(),
                 Arguments :: [cerl()]) -> c_call().

ann_c_call(As, Module, Name, Arguments) ->
    #c_call{module = Module, name = Name, args = Arguments, anno = As}.


-doc "_See also: _`c_call/3`.".
-spec update_c_call(Node :: cerl(),
                    Module :: cerl(), Name :: cerl(),
                    Arguments :: [cerl()]) -> c_call().

update_c_call(Node, Module, Name, Arguments) ->
    #c_call{module = Module, name = Name, args = Arguments,
	    anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract inter-module call expression, otherwise
`false`.

_See also: _`c_call/3`.
""".
-spec is_c_call(Node :: cerl()) -> boolean().

is_c_call(#c_call{}) ->
    true;
is_c_call(_) ->
    false.


-doc """
Returns the module subtree of an abstract inter-module call.

_See also: _`c_call/3`.
""".
-spec call_module(Node :: c_call()) -> cerl().

call_module(Node) ->
    Node#c_call.module.


-doc """
Returns the name subtree of an abstract inter-module call.

_See also: _`c_call/3`.
""".
-spec call_name(Node :: c_call()) -> cerl().

call_name(Node) ->
    Node#c_call.name.


-doc """
Returns the list of argument subtrees of an abstract inter-module call.

_See also: _`c_call/3`, `call_arity/1`.
""".
-spec call_args(Node :: c_call()) -> [cerl()].

call_args(Node) ->
    Node#c_call.args.


-doc """
Returns the number of argument subtrees of an abstract inter-module call.

Note: this is equivalent to [`length(call_args(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_call/3`, `call_args/1`.
""".
-spec call_arity(Node :: c_call()) -> arity().

call_arity(Node) ->
    length(call_args(Node)).


%% ---------------------------------------------------------------------

-doc """
Creates an abstract primitive operation call.

If `Arguments` is `[A1, ..., An]`, the result represents "`primop
Name(A1, ..., An)`". `Name` must be an atom literal.

_See also: _`ann_c_primop/3`, `c_apply/2`, `c_call/3`, `is_c_primop/1`,
`primop_args/1`, `primop_arity/1`, `primop_name/1`, `update_c_primop/3`.
""".
-spec c_primop(Name :: cerl(), Arguments :: [cerl()]) -> c_primop().

c_primop(Name, Arguments) ->
    #c_primop{name = Name, args = Arguments}.


-doc "_See also: _`c_primop/2`.".
-spec ann_c_primop(Annotations :: [term()], Name :: cerl(),
                   Arguments :: [cerl()]) -> c_primop().

ann_c_primop(As, Name, Arguments) ->
    #c_primop{name = Name, args = Arguments, anno = As}.


-doc "_See also: _`c_primop/2`.".
-spec update_c_primop(Node :: cerl(), Name :: cerl(),
                      Arguments :: [cerl()]) -> c_primop().

update_c_primop(Node, Name, Arguments) ->
    #c_primop{name = Name, args = Arguments, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract primitive operation call, otherwise
`false`.

_See also: _`c_primop/2`.
""".
-spec is_c_primop(Node :: cerl()) -> boolean().

is_c_primop(#c_primop{}) ->
    true;
is_c_primop(_) ->
    false.


-doc """
Returns the name subtree of an abstract primitive operation call.

_See also: _`c_primop/2`.
""".
-spec primop_name(Node :: c_primop()) -> cerl().

primop_name(Node) ->
    Node#c_primop.name.


-doc """
Returns the list of argument subtrees of an abstract primitive operation call.

_See also: _`c_primop/2`, `primop_arity/1`.
""".
-spec primop_args(Node :: c_primop()) -> [cerl()].

primop_args(Node) ->
    Node#c_primop.args.


-doc """
Returns the number of argument subtrees of an abstract primitive operation call.

Note: this is equivalent to [`length(primop_args(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_primop/2`, `primop_args/1`.
""".
-spec primop_arity(Node :: c_primop()) -> arity().

primop_arity(Node) ->
    length(primop_args(Node)).


%% ---------------------------------------------------------------------

-doc """
Creates an abstract try-expression.

If `Variables` is `[V1, ..., Vn]` and `ExceptionVars` is `[X1, ...,
Xm]`, the result represents "`try Argument of <V1, ..., Vn> -> Body
catch <X1, ..., Xm> -> Handler`".  All the `Vi` and `Xi` must have
type `var`.

_See also: _`ann_c_try/6`, `c_catch/1`, `is_c_try/1`, `try_arg/1`, `try_body/1`,
`try_vars/1`, `update_c_try/6`.
""".
-spec c_try(Argument :: cerl(), Variables :: [cerl()],
            Body :: cerl(), ExceptionVars :: [cerl()],
            Handler :: cerl()) -> c_try().

c_try(Expr, Vs, Body, Evs, Handler) ->
    #c_try{arg = Expr, vars = Vs, body = Body,
	   evars = Evs, handler = Handler}.


-doc "_See also: _`c_try/5`.".
-spec ann_c_try(Annotations :: [term()],
                Argument :: cerl(), Variables :: [cerl()],
                Body :: cerl(), ExceptionVars :: [cerl()],
                Handler :: cerl()) -> c_try().

ann_c_try(As, Expr, Vs, Body, Evs, Handler) ->
    #c_try{arg = Expr, vars = Vs, body = Body,
	   evars = Evs, handler = Handler, anno = As}.


-doc "_See also: _`c_try/5`.".
-spec update_c_try(Node :: c_try(),
                   Argument :: cerl(), Variables :: [cerl()],
                   Body :: cerl(), ExceptionVars :: [cerl()],
                   Handler :: cerl()) -> c_try().

update_c_try(Node, Expr, Vs, Body, Evs, Handler) ->
    #c_try{arg = Expr, vars = Vs, body = Body,
	   evars = Evs, handler = Handler, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract try-expression, otherwise `false`.

_See also: _`c_try/5`.
""".
-spec is_c_try(Node :: cerl()) -> boolean().

is_c_try(#c_try{}) ->
    true;
is_c_try(_) ->
    false.


-doc """
Returns the expression subtree of an abstract try-expression.

_See also: _`c_try/5`.
""".
-spec try_arg(Node :: c_try()) -> cerl().

try_arg(Node) ->
    Node#c_try.arg.


-doc """
Returns the list of success variable subtrees of an abstract try-expression.

_See also: _`c_try/5`.
""".
-spec try_vars(Node :: c_try()) -> [cerl()].

try_vars(Node) ->
    Node#c_try.vars.


-doc """
Returns the success body subtree of an abstract try-expression.

_See also: _`c_try/5`.
""".
-spec try_body(Node :: c_try()) -> cerl().

try_body(Node) ->
    Node#c_try.body.


-doc """
Returns the list of exception variable subtrees of an abstract try-expression.

_See also: _`c_try/5`.
""".
-spec try_evars(Node :: c_try()) -> [cerl()].

try_evars(Node) ->
    Node#c_try.evars.


-doc """
Returns the exception body subtree of an abstract try-expression.

_See also: _`c_try/5`.
""".
-spec try_handler(Node :: c_try()) -> cerl().

try_handler(Node) ->
    Node#c_try.handler.


%% ---------------------------------------------------------------------

-doc """
Creates an abstract catch-expression.

The result represents "`catch Body`".

Note: catch-expressions can be rewritten as try-expressions, and will eventually
be removed from Core Erlang.

_See also: _`ann_c_catch/2`, `c_try/5`, `catch_body/1`, `is_c_catch/1`,
`update_c_catch/2`.
""".
-spec c_catch(Body :: cerl()) -> c_catch().

c_catch(Body) ->
    #c_catch{body = Body}.


-doc "_See also: _`c_catch/1`.".
-spec ann_c_catch(Annotations :: [term()], Body :: cerl()) -> c_catch().

ann_c_catch(As, Body) ->
    #c_catch{body = Body, anno = As}.


-doc "_See also: _`c_catch/1`.".
-spec update_c_catch(Node :: c_catch(), Body :: cerl()) -> c_catch().

update_c_catch(Node, Body) ->
    #c_catch{body = Body, anno = get_ann(Node)}.


-doc """
Returns `true` if `Node` is an abstract catch-expression, otherwise `false`.

_See also: _`c_catch/1`.
""".
-spec is_c_catch(Node :: cerl()) -> boolean().

is_c_catch(#c_catch{}) ->
    true;
is_c_catch(_) ->
    false.


-doc """
Returns the body subtree of an abstract catch-expression.

_See also: _`c_catch/1`.
""".
-spec catch_body(Node :: c_catch()) -> cerl().

catch_body(Node) ->
    Node#c_catch.body.


%% ---------------------------------------------------------------------

-doc """
Translates an abstract syntax tree to a corresponding explicit record
representation.

The records are defined in the file "`cerl.hrl`".

_See also: _`from_records/1`, `type/1`.
""".
-spec to_records(Node :: cerl()) -> cerl().

to_records(Node) ->
    Node.

-doc """
Translates an explicit record representation to a corresponding abstract syntax
tree.

The records are defined in the file "`core_parse.hrl`".

_See also: _`to_records/1`, `type/1`.
""".
-spec from_records(Node :: cerl()) -> cerl().

from_records(Node) ->
    Node.


%% ---------------------------------------------------------------------

-doc """
Returns `true` if `Node` represents a data constructor, otherwise `false`.

Data constructors are cons cells, tuples, and atomic literals.

_See also: _`data_arity/1`, `data_es/1`, `data_type/1`.
""".
-spec is_data(Node :: cerl()) -> boolean().

is_data(#c_literal{}) ->
    true;
is_data(#c_cons{}) ->
    true;
is_data(#c_tuple{}) ->
    true;
is_data(_) ->
    false.


-type value() :: integer() | float() | atom() | [].
-type dtype() :: 'cons' | 'tuple' | {'atomic', value()}.
-type c_lct() :: c_literal() | c_cons() | c_tuple().

-doc """
Returns a type descriptor for a data constructor node. (Cf.
[`is_data/1`](`is_data/1`).)

This is mainly useful for comparing types and for constructing new
nodes of the same type (cf. [`make_data/2`](`make_data/2`)). If `Node`
represents an integer, floating-point number, atom or empty list, the
result is `{atomic, Value}`, where `Value` is the value of
[`concrete(Node)`](`concrete/1`), otherwise the result is either
`cons` or `tuple`.

Type descriptors can be compared for equality or order (in the Erlang term
order), but remember that floating-point values should in general never be
tested for equality.

_See also: _`concrete/1`, `is_data/1`, `make_data/2`, `type/1`.
""".
-spec data_type(Node :: c_lct()) -> dtype().

data_type(#c_literal{val = V}) ->
    case V of
	[_ | _] ->
	    cons;
	_ when is_tuple(V) ->
	    tuple;
	_ ->
	    {atomic, V}
    end;
data_type(#c_cons{}) ->
    cons;
data_type(#c_tuple{}) ->
    tuple.

-doc """
Returns the list of subtrees of a data constructor node.

If the arity of the constructor is zero, the result is the empty list.

Note: if [`data_type(Node)`](`data_type/1`) is `cons`, the number of subtrees is
exactly two. If [`data_type(Node)`](`data_type/1`) is `{atomic, Value}`, the
number of subtrees is zero.

_See also: _`data_arity/1`, `data_type/1`, `is_data/1`, `make_data/2`.
""".
-spec data_es(Node :: c_lct()) -> [cerl()].

data_es(#c_literal{val = V}) ->
    case V of
	[Head | Tail] ->
	    [#c_literal{val = Head}, #c_literal{val = Tail}];
	_ when is_tuple(V) ->
	    make_lit_list(tuple_to_list(V));
	_ ->
	    []
    end;
data_es(#c_cons{hd = H, tl = T}) ->
    [H, T];
data_es(#c_tuple{es = Es}) ->
    Es.

-doc """
Returns the number of subtrees of a data constructor node.

This is equivalent to [`length(data_es(Node))`](`length/1`), but
potentially more efficient.

_See also: _`data_es/1`, `is_data/1`.
""".
-spec data_arity(Node :: c_lct()) -> non_neg_integer().

data_arity(#c_literal{val = V}) ->
    case V of
	[_ | _] ->
	    2;
	_ when is_tuple(V) ->
	    tuple_size(V);
	_ ->
	    0
    end;
data_arity(#c_cons{}) ->
    2;
data_arity(#c_tuple{es = Es}) ->
    length(Es).


-doc """
Creates a data constructor node with the specified type and subtrees. (Cf.
[`data_type/1`](`data_type/1`).)

An exception is thrown if the length of `Elements` is invalid for the
given `Type`; see [`data_es/1`](`data_es/1`) for arity constraints on
constructor types.

_See also: _`ann_make_data/3`, `data_es/1`, `data_type/1`, `make_data_skel/2`,
`update_data/3`.
""".
-spec make_data(Type :: dtype(), Elements :: [cerl()]) -> c_lct().

make_data(CType, Es) ->
    ann_make_data([], CType, Es).


-doc "_See also: _`make_data/2`.".
-spec ann_make_data(Annotations :: [term()], Type :: dtype(),
                    Elementes :: [cerl()]) -> c_lct().

ann_make_data(As, {atomic, V}, []) -> #c_literal{val = V, anno = As};
ann_make_data(As, cons, [H, T]) -> ann_c_cons(As, H, T);
ann_make_data(As, tuple, Es) -> ann_c_tuple(As, Es).

-doc "_See also: _`make_data/2`.".
-spec update_data(Node :: cerl(), Type :: dtype(), Elements :: [cerl()]) -> c_lct().

update_data(Node, CType, Es) ->
    ann_make_data(get_ann(Node), CType, Es).


-doc """
Like [`make_data/2`](`make_data/2`), but analogous to
[`c_tuple_skel/1`](`c_tuple_skel/1`) and [`c_cons_skel/2`](`c_cons_skel/2`).

_See also: _`ann_make_data_skel/3`, `c_cons_skel/2`, `c_tuple_skel/1`,
`make_data/2`, `update_data_skel/3`.
""".
-spec make_data_skel(Type :: dtype(), Elements :: [cerl()]) -> c_lct().

make_data_skel(CType, Es) ->
    ann_make_data_skel([], CType, Es).


-doc "_See also: _`make_data_skel/2`.".
-spec ann_make_data_skel(Annotations :: [term()], Type :: dtype(),
                         Elements :: [cerl()]) -> c_lct().

ann_make_data_skel(As, {atomic, V}, []) -> #c_literal{val = V, anno = As};
ann_make_data_skel(As, cons, [H, T]) -> ann_c_cons_skel(As, H, T);
ann_make_data_skel(As, tuple, Es) -> ann_c_tuple_skel(As, Es).


-doc "_See also: _`make_data_skel/2`.".
-spec update_data_skel(Node :: cerl(), Type :: dtype(), Elements ::[cerl()]) -> c_lct().

update_data_skel(Node, CType, Es) ->
    ann_make_data_skel(get_ann(Node), CType, Es).


%% ---------------------------------------------------------------------

-doc """
Returns the grouped list of all subtrees of a node.

If `Node` is a leaf node (cf. [`is_leaf/1`](`is_leaf/1`)), this is the
empty list, otherwise the result is always a nonempty list, containing
the lists of subtrees of `Node`, in left-to-right order as they occur
in the printed program text, and grouped by category. Often, each
group contains only a single subtree.

Depending on the type of `Node`, the size of some groups may be
variable (for example, the group consisting of all the elements of a
tuple), while others always contain the same number of elements -
usually exactly one (for example, the group containing the argument
expression of a case-expression). Note, however, that the exact
structure of the returned list (for a given node type) should in
general not be depended upon, since it might be subject to change
without notice.

The function [`subtrees/1`](`subtrees/1`) and the constructor functions
[`make_tree/2`](`make_tree/2`) and [`update_tree/2`](`update_tree/2`) can be a
great help if one wants to traverse a syntax tree, visiting all its subtrees,
but treat nodes of the tree in a uniform way in most or all cases. Using these
functions makes this simple, and also assures that your code is not overly
sensitive to extensions of the syntax tree data type, because any node types not
explicitly handled by your code can be left to a default case.

For example:

```text
    postorder(F, Tree) ->
        F(case subtrees(Tree) of
            [] -> Tree;
            List -> update_tree(Tree,
                                [[postorder(F, Subtree)
                                  || Subtree <- Group]
                                 || Group <- List])
          end).

```

maps the function `F` on `Tree` and all its subtrees, doing a post-order
traversal of the syntax tree. (Note the use of
[`update_tree/2`](`update_tree/2`) to preserve annotations.) For a simple
function like:

```text
    f(Node) ->
        case type(Node) of
            atom -> atom("a_" ++ atom_name(Node));
            _ -> Node
        end.

```

the call `postorder(fun f/1, Tree)` will yield a new representation of `Tree` in
which all atom names have been extended with the prefix "a\_", but nothing else
(including annotations) has been changed.

_See also: _`is_leaf/1`, `make_tree/2`, `update_tree/2`.
""".
-spec subtrees(Node :: cerl()) -> [[cerl()]].

subtrees(T) ->
    case is_leaf(T) of
	true ->
	    [];
	false ->
	    case type(T) of
		values ->
		    [values_es(T)];
		binary ->
		    [binary_segments(T)];
		bitstr ->
		    [[bitstr_val(T)], [bitstr_size(T)],
		     [bitstr_unit(T)], [bitstr_type(T)],
		     [bitstr_flags(T)]];
		cons ->
		    [[cons_hd(T)], [cons_tl(T)]];
		tuple ->
		    [tuple_es(T)];
		map ->
		    [map_es(T)];
		map_pair ->
		    [[map_pair_op(T)],[map_pair_key(T)],[map_pair_val(T)]];
		'let' ->
		    [let_vars(T), [let_arg(T)], [let_body(T)]];
		seq ->
		    [[seq_arg(T)], [seq_body(T)]];
		apply ->
		    [[apply_op(T)], apply_args(T)];
		call ->
		    [[call_module(T)], [call_name(T)],
		     call_args(T)];
		primop ->
		    [[primop_name(T)], primop_args(T)];
		'case' ->
		    [[case_arg(T)], case_clauses(T)];
		clause ->
		    [clause_pats(T), [clause_guard(T)],
		     [clause_body(T)]];
		alias ->
		    [[alias_var(T)], [alias_pat(T)]];
		'fun' ->
		    [fun_vars(T), [fun_body(T)]];
		'receive' ->
		    [receive_clauses(T), [receive_timeout(T)],
		     [receive_action(T)]];
		'try' ->
		    [[try_arg(T)], try_vars(T), [try_body(T)],
		     try_evars(T), [try_handler(T)]];
		'catch' ->
		    [[catch_body(T)]];
		letrec ->
		    Es = unfold_tuples(letrec_defs(T)),
		    [Es, [letrec_body(T)]];
		module ->
		    As = unfold_tuples(module_attrs(T)),
		    Es = unfold_tuples(module_defs(T)),
		    [[module_name(T)], module_exports(T), As, Es]
	    end
    end.


-doc """
Creates a syntax tree with the given subtrees, and the same type and annotations
as the node `Node`.

This is equivalent to [`ann_make_tree(get_ann(Node), type(Node),
Groups)`](`ann_make_tree/3`), but potentially more efficient.

_See also: _`ann_make_tree/3`, `get_ann/1`, `type/1`, `update_tree/3`.
""".
-spec update_tree(Node :: cerl(), Groups :: [[cerl()],...]) -> cerl().

update_tree(Node, Gs) ->
    ann_make_tree(get_ann(Node), type(Node), Gs).


-doc """
Creates a syntax tree with the given type and subtrees, and the same annotations
as the node `Node`.

This is equivalent to
[`ann_make_tree(get_ann(Node), Type, Groups)`](`ann_make_tree/3`), but
potentially more efficient.

_See also: _`ann_make_tree/3`, `get_ann/1`, `update_tree/2`.
""".
-spec update_tree(Node :: cerl(), Type :: ctype(),
                  Groups :: [[cerl()],...]) -> cerl().

update_tree(Node, Type, Gs) ->
    ann_make_tree(get_ann(Node), Type, Gs).


-doc """
Creates a syntax tree with the given type and subtrees.

`Type` must be a node type name (cf. [`type/1`](`type/1`)) that does
not denote a leaf node type (cf.
[`is_leaf/1`](`is_leaf/1`)).

`Groups` must be a _nonempty_ list of groups of syntax trees,
representing the subtrees of a node of the given type, in
left-to-right order as they would occur in the printed program text,
grouped by category as done by [`subtrees/1`](`subtrees/1`).

The result of
[`ann_make_tree(get_ann(Node), type(Node), subtrees(Node))`](`ann_make_tree/3`)
(cf. [`update_tree/2`](`update_tree/2`)) represents the same source code text as
the original `Node`, assuming that [`subtrees(Node)`](`subtrees/1`) yields a
nonempty list. However, it does not necessarily have the exact same data
representation as `Node`.

_See also: _`ann_make_tree/3`, `is_leaf/1`, `subtrees/1`, `type/1`,
`update_tree/2`.
""".
-spec make_tree(Type :: ctype(), Groups :: [[cerl()],...]) -> cerl().

make_tree(Type, Gs) ->
    ann_make_tree([], Type, Gs).


-doc """
Creates a syntax tree with the given annotations, type and subtrees.

See [`make_tree/2`](`make_tree/2`) for details.

_See also: _`make_tree/2`.
""".
-spec ann_make_tree(Annotations :: [term()], Type :: ctype(),
                    Groups :: [[cerl()],...]) -> cerl().

ann_make_tree(As, values, [Es]) -> ann_c_values(As, Es);
ann_make_tree(As, binary, [Ss]) -> ann_c_binary(As, Ss);
ann_make_tree(As, bitstr, [[V],[S],[U],[T],[Fs]]) ->
    ann_c_bitstr(As, V, S, U, T, Fs);
ann_make_tree(As, cons, [[H], [T]]) -> ann_c_cons(As, H, T);
ann_make_tree(As, tuple, [Es]) -> ann_c_tuple(As, Es);
ann_make_tree(As, map, [Es]) -> ann_c_map(As, Es);
ann_make_tree(As, map, [[A], Es]) -> ann_c_map(As, A, Es);
ann_make_tree(As, map_pair, [[Op], [K], [V]]) -> ann_c_map_pair(As, Op, K, V);
ann_make_tree(As, 'let', [Vs, [A], [B]]) -> ann_c_let(As, Vs, A, B);
ann_make_tree(As, seq, [[A], [B]]) -> ann_c_seq(As, A, B);
ann_make_tree(As, apply, [[Op], Es]) -> ann_c_apply(As, Op, Es);
ann_make_tree(As, call, [[M], [N], Es]) -> ann_c_call(As, M, N, Es);
ann_make_tree(As, primop, [[N], Es]) -> ann_c_primop(As, N, Es);
ann_make_tree(As, 'case', [[A], Cs]) -> ann_c_case(As, A, Cs);
ann_make_tree(As, clause, [Ps, [G], [B]]) -> ann_c_clause(As, Ps, G, B);
ann_make_tree(As, alias, [[V], [P]]) -> ann_c_alias(As, V, P);
ann_make_tree(As, 'fun', [Vs, [B]]) -> ann_c_fun(As, Vs, B);
ann_make_tree(As, 'receive', [Cs, [T], [A]]) ->
    ann_c_receive(As, Cs, T, A);
ann_make_tree(As, 'try', [[E], Vs, [B], Evs, [H]]) ->
    ann_c_try(As, E, Vs, B, Evs, H);
ann_make_tree(As, 'catch', [[B]]) -> ann_c_catch(As, B);
ann_make_tree(As, letrec, [Es, [B]]) ->
    ann_c_letrec(As, fold_tuples(Es), B);
ann_make_tree(As, module, [[N], Xs, Es, Ds]) ->
    ann_c_module(As, N, Xs, fold_tuples(Es), fold_tuples(Ds)).


%% ---------------------------------------------------------------------

-doc """
Creates a meta-representation of a syntax tree.

The result represents an Erlang expression "`MetaTree`" which, if
evaluated, will yield a new syntax tree representing the same source
code text as `Tree` (although the actual data representation may be
different). The expression represented by `MetaTree` is
_implementation independent_ with regard to the data structures used
by the abstract syntax tree implementation.

Any node in `Tree` whose node type is `var` (cf. [`type/1`](`type/1`)), and
whose list of annotations (cf. [`get_ann/1`](`get_ann/1`)) contains the atom
`meta_var`, will remain unchanged in the resulting tree, except that exactly one
occurrence of `meta_var` is removed from its annotation list.

The main use of the function [`meta/1`](`meta/1`) is to transform a data
structure `Tree`, which represents a piece of program code, into a form that is
_representation independent when printed_. E.g., suppose `Tree` represents a
variable named "V". Then (assuming a function `print/1` for printing syntax
trees), evaluating `print(abstract(Tree))` \- simply using
[`abstract/1`](`abstract/1`) to map the actual data structure onto a syntax tree
representation - would output a string that might look something like
"`{var, ..., 'V'}`", which is obviously dependent on the implementation of the
abstract syntax trees. This could, for example, be useful for caching a syntax tree
in a file. However, in some situations like in a program generator generator (with
two "generator"), it may be unacceptable. Using `print(meta(Tree))` instead
would output a _representation independent_ syntax tree generating expression;
in the above case, something like "`cerl:c_var('V')`".

The implementation tries to generate compact code with respect to literals and
lists.

_See also: _`abstract/1`, `get_ann/1`, `type/1`.
""".
-spec meta(Tree :: cerl()) -> cerl().

meta(Node) ->
    %% First of all we check for metavariables:
    case type(Node) of
	var ->
	    case lists:member(meta_var, get_ann(Node)) of
		false ->
		    meta_0(var, Node);
		true ->
		    %% A meta-variable: remove the first found
		    %% 'meta_var' annotation, but otherwise leave
		    %% the node unchanged.
		    set_ann(Node, lists:delete(meta_var, get_ann(Node)))
	    end;
	Type ->
	    meta_0(Type, Node)
    end.

meta_0(Type, Node) ->
    case get_ann(Node) of
	[] ->
	    meta_1(Type, Node);
	As ->
	    meta_call(set_ann, [meta_1(Type, Node), abstract(As)])
    end.

meta_1(literal, Node) ->
    %% We handle atomic literals separately, to get a bit
    %% more compact code. For the rest, we use 'abstract'.
    case concrete(Node) of
	V when is_atom(V) ->
	    meta_call(c_atom, [Node]);
	V when is_integer(V) ->
	    meta_call(c_int, [Node]);
	V when is_float(V) ->
	    meta_call(c_float, [Node]);
	[] ->
	    meta_call(c_nil, []);
	_ ->
	    meta_call(abstract, [Node])
    end;
meta_1(var, Node) ->
    %% A normal variable or function name.
    meta_call(c_var, [abstract(var_name(Node))]);
meta_1(values, Node) ->
    meta_call(c_values,
	      [make_list(meta_list(values_es(Node)))]);
meta_1(binary, Node) ->
    meta_call(c_binary,
	      [make_list(meta_list(binary_segments(Node)))]);
meta_1(bitstr, Node) ->
    meta_call(c_bitstr,
	      [meta(bitstr_val(Node)),
	       meta(bitstr_size(Node)),
	       meta(bitstr_unit(Node)),
	       meta(bitstr_type(Node)),
	       meta(bitstr_flags(Node))]);
meta_1(cons, Node) ->
    %% The list is split up if some sublist has annotatations. If
    %% we get exactly one element, we generate a 'c_cons' call
    %% instead of 'make_list' to reconstruct the node.
    case split_list(Node) of
	{[H], Node1} ->
	    meta_call(c_cons, [meta(H), meta(Node1)]);
	{L, Node1} ->
	    meta_call(make_list,
		      [make_list(meta_list(L)), meta(Node1)])
    end;
meta_1(tuple, Node) ->
    meta_call(c_tuple,
	      [make_list(meta_list(tuple_es(Node)))]);
meta_1('let', Node) ->
    meta_call(c_let,
	      [make_list(meta_list(let_vars(Node))),
	       meta(let_arg(Node)), meta(let_body(Node))]);
meta_1(seq, Node) ->
    meta_call(c_seq,
	      [meta(seq_arg(Node)), meta(seq_body(Node))]);
meta_1(apply, Node) ->
    meta_call(c_apply,
	      [meta(apply_op(Node)),
	       make_list(meta_list(apply_args(Node)))]);
meta_1(call, Node) ->
    meta_call(c_call,
	      [meta(call_module(Node)), meta(call_name(Node)),
	       make_list(meta_list(call_args(Node)))]);
meta_1(primop, Node) ->
    meta_call(c_primop,
	      [meta(primop_name(Node)),
	       make_list(meta_list(primop_args(Node)))]);
meta_1('case', Node) ->
    meta_call(c_case,
	      [meta(case_arg(Node)),
	       make_list(meta_list(case_clauses(Node)))]);
meta_1(clause, Node) ->
    meta_call(c_clause,
	      [make_list(meta_list(clause_pats(Node))),
	       meta(clause_guard(Node)),
	       meta(clause_body(Node))]);
meta_1(alias, Node) ->
    meta_call(c_alias,
	      [meta(alias_var(Node)), meta(alias_pat(Node))]);
meta_1('fun', Node) ->
    meta_call(c_fun,
	      [make_list(meta_list(fun_vars(Node))),
	       meta(fun_body(Node))]);
meta_1('receive', Node) ->
    meta_call(c_receive,
	      [make_list(meta_list(receive_clauses(Node))),
	       meta(receive_timeout(Node)),
	       meta(receive_action(Node))]);
meta_1('try', Node) ->
    meta_call(c_try,
	      [meta(try_arg(Node)),
	       make_list(meta_list(try_vars(Node))),
	       meta(try_body(Node)),
	       make_list(meta_list(try_evars(Node))),
	       meta(try_handler(Node))]);
meta_1('catch', Node) ->
    meta_call(c_catch, [meta(catch_body(Node))]);
meta_1(letrec, Node) ->
    meta_call(c_letrec,
	      [make_list([c_tuple([meta(N), meta(F)])
			  || {N, F} <:- letrec_defs(Node)]),
	       meta(letrec_body(Node))]);
meta_1(module, Node) ->
    meta_call(c_module,
	      [meta(module_name(Node)),
	       make_list(meta_list(module_exports(Node))),
	       make_list([c_tuple([meta(A), meta(V)])
			  || {A, V} <:- module_attrs(Node)]),
	       make_list([c_tuple([meta(N), meta(F)])
			  || {N, F} <:- module_defs(Node)])]).

meta_call(F, As) ->
    c_call(c_atom(?MODULE), c_atom(F), As).

meta_list([T | Ts]) ->
    [meta(T) | meta_list(Ts)];
meta_list([]) ->
    [].

split_list(Node) ->
    split_list(set_ann(Node, []), []).

split_list(Node, L) ->
    A = get_ann(Node),
    case type(Node) of
	cons when A =:= [] ->
	    split_list(cons_tl(Node), [cons_hd(Node) | L]);
	_ ->
	    {lists:reverse(L), Node}
    end.


%% ---------------------------------------------------------------------

%% General utilities

is_lit_list([#c_literal{} | Es]) ->
    is_lit_list(Es);
is_lit_list([_ | _]) ->
    false;
is_lit_list([]) ->
    true.

lit_list_vals([#c_literal{val = V} | Es]) ->
    [V | lit_list_vals(Es)];
lit_list_vals([]) ->
    [].

-spec make_lit_list([_]) -> [#c_literal{}].  % XXX: cerl() instead of _ ?

make_lit_list([V | Vs]) ->
    [#c_literal{val = V} | make_lit_list(Vs)];
make_lit_list([]) ->
    [].

%% The following tests are the same as done by 'io_lib:char_list' and
%% 'io_lib:printable_list', respectively, but for a single character.

is_char_value(V) when V >= $\000, V =< $\377 -> true;
is_char_value(_) -> false.

is_print_char_value(V) when V >= $\040, V =< $\176 -> true;
is_print_char_value(V) when V >= $\240, V =< $\377 -> true;
is_print_char_value(V) when V =:= $\b -> true;
is_print_char_value(V) when V =:= $\d -> true;
is_print_char_value(V) when V =:= $\e -> true;
is_print_char_value(V) when V =:= $\f -> true;
is_print_char_value(V) when V =:= $\n -> true;
is_print_char_value(V) when V =:= $\r -> true;
is_print_char_value(V) when V =:= $\s -> true;
is_print_char_value(V) when V =:= $\t -> true;
is_print_char_value(V) when V =:= $\v -> true;
is_print_char_value(V) when V =:= $\" -> true;
is_print_char_value(V) when V =:= $\' -> true;
is_print_char_value(V) when V =:= $\\ -> true;
is_print_char_value(_) -> false.

is_char_list([V | Vs]) when is_integer(V) ->
    is_char_value(V) andalso is_char_list(Vs);
is_char_list([]) ->
    true;
is_char_list(_) ->
    false.

is_print_char_list([V | Vs]) when is_integer(V) ->
    is_print_char_value(V) andalso is_print_char_list(Vs);
is_print_char_list([]) ->
    true;
is_print_char_list(_) ->
    false.

unfold_tuples([{X, Y} | Ps]) ->
    [X, Y | unfold_tuples(Ps)];
unfold_tuples([]) ->
    [].

fold_tuples([X, Y | Es]) ->
    [{X, Y} | fold_tuples(Es)];
fold_tuples([]) ->
    [].
