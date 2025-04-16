%% =====================================================================
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright 1997-2006 Richard Carlsson
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% %CopyrightEnd%
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @end
%% =====================================================================

-module(erl_syntax).
-moduledoc """
Abstract Erlang syntax trees.

This module defines an abstract data type for representing Erlang source code as
syntax trees, in a way that is backwards compatible with the data structures
created by the Erlang standard library parser module `m:erl_parse` (often referred
to as "parse trees", which is a bit of a misnomer). This means that all
`erl_parse` trees are valid abstract syntax trees, but the reverse is not true:
abstract syntax trees can in general not be used as input to functions expecting
an `erl_parse` tree. However, as long as an abstract syntax tree represents a
correct Erlang program, the function `revert/1` should be able to transform it
to the corresponding `erl_parse` representation.

A recommended starting point for the first-time user is the documentation of the
[`syntaxTree()`](`t:syntaxTree/0`) data type, and the function `type/1`.

> #### Note {: .info }
>
> This module deals with the composition and decomposition of _syntactic_ entities
> (as opposed to semantic ones); its purpose is to hide all direct references to
> the data structures used to represent these entities. With few exceptions, the
> functions in this module perform no semantic interpretation of their inputs, and
> in general, the user is assumed to pass type-correct arguments — if this is not
> done, the effects are not defined.

With the exception of the [`erl_parse()`](`t:erl_parse/0`) data structures, the
internal representations of abstract syntax trees are subject to change without
notice, and should not be documented outside this module. Furthermore, we do not
give any guarantees on how an abstract syntax tree may or may not be
represented, _with the following exceptions_: no syntax tree is represented by a
single atom, such as `none`, by a list constructor `[X | Y]`, or by the empty
list `[]`. This can be relied on when writing functions that operate on syntax
trees.
""".

-compile(nowarn_deprecated_catch).

-export([type/1,
	 is_leaf/1,
	 is_form/1,
	 is_literal/1,
	 abstract/1,
	 concrete/1,
	 revert/1,
	 revert_forms/1,
	 subtrees/1,
	 make_tree/2,
	 update_tree/2,
	 meta/1,

	 get_pos/1,
	 set_pos/2,
	 copy_pos/2,
	 get_precomments/1,
	 set_precomments/2,
	 add_precomments/2,
	 get_postcomments/1,
	 set_postcomments/2,
	 add_postcomments/2,
	 has_comments/1,
	 remove_comments/1,
	 copy_comments/2,
	 join_comments/2,
	 get_ann/1,
	 set_ann/2,
	 add_ann/2,
	 copy_ann/2,
	 get_attrs/1,
	 set_attrs/2,
	 copy_attrs/2,

	 flatten_form_list/1,
	 cons/2,
	 list_head/1,
	 list_tail/1,
	 is_list_skeleton/1,
	 is_proper_list/1,
	 list_elements/1,
	 list_length/1,
	 normalize_list/1,
	 compact_list/1,

         annotated_type/2,
         annotated_type_name/1,
         annotated_type_body/1,
	 application/2,
	 application/3,
	 application_arguments/1,
	 application_operator/1,
	 arity_qualifier/2,
	 arity_qualifier_argument/1,
	 arity_qualifier_body/1,
	 atom/1,
	 is_atom/2,
	 atom_value/1,
	 atom_literal/1,
         atom_literal/2,
	 atom_name/1,
	 attribute/1,
	 attribute/2,
	 attribute_arguments/1,
	 attribute_name/1,
	 binary/1,
	 binary_comp/2,
	 binary_comp_template/1,
	 binary_comp_body/1,
	 binary_field/1,
	 binary_field/2,
	 binary_field/3,
	 binary_field_body/1,
	 binary_field_types/1,
	 binary_field_size/1,
	 binary_fields/1,
	 binary_generator/2,
	 binary_generator_body/1,
	 binary_generator_pattern/1,
         bitstring_type/2,
         bitstring_type_m/1,
         bitstring_type_n/1,
	 block_expr/1,
	 block_expr_body/1,
	 case_expr/2,
	 case_expr_argument/1,
	 case_expr_clauses/1,
	 catch_expr/1,
	 catch_expr_body/1,
	 char/1,
	 is_char/2,
	 char_value/1,
	 char_literal/1,
	 char_literal/2,
	 clause/2,
	 clause/3,
	 clause_body/1,
	 clause_guard/1,
	 clause_patterns/1,
	 comment/1,
	 comment/2,
	 comment_padding/1,
	 comment_text/1,
	 conjunction/1,
	 conjunction_body/1,
         constrained_function_type/2,
         constrained_function_type_body/1,
         constrained_function_type_argument/1,
         constraint/2,
         constraint_argument/1,
         constraint_body/1,
	 disjunction/1,
	 disjunction_body/1,
         else_expr/1,
         else_expr_clauses/1,
	 eof_marker/0,
	 error_marker/1,
	 error_marker_info/1,
	 float/1,
	 float_value/1,
	 float_literal/1,
	 form_list/1,
	 form_list_elements/1,
	 fun_expr/1,
	 fun_expr_arity/1,
	 fun_expr_clauses/1,
         fun_type/0,
	 function/2,
	 function_arity/1,
	 function_clauses/1,
	 function_name/1,
         function_type/1,
         function_type/2,
         function_type_arguments/1,
         function_type_return/1,
	 generator/2,
	 generator_body/1,
	 generator_pattern/1,
	 if_expr/1,
	 if_expr_clauses/1,
	 implicit_fun/1,
	 implicit_fun/2,
	 implicit_fun/3,
	 implicit_fun_name/1,
	 infix_expr/3,
	 infix_expr_left/1,
	 infix_expr_operator/1,
	 infix_expr_right/1,
	 integer/1,
	 is_integer/2,
	 integer_value/1,
	 integer_literal/1,
         integer_range_type/2,
         integer_range_type_low/1,
         integer_range_type_high/1,
	 list/1,
	 list/2,
	 list_comp/2,
	 list_comp_body/1,
	 list_comp_template/1,
	 list_prefix/1,
	 list_suffix/1,
	 macro/1,
	 macro/2,
	 macro_arguments/1,
	 macro_name/1,
         map_comp/2,
         map_comp_template/1,
         map_comp_body/1,
         map_expr/1,
         map_expr/2,
         map_expr_argument/1,
         map_expr_fields/1,
         map_field_assoc/2,
         map_field_assoc_name/1,
         map_field_assoc_value/1,
         map_field_exact/2,
         map_field_exact_name/1,
         map_field_exact_value/1,
         map_generator/2,
         map_generator_body/1,
         map_generator_pattern/1,
         map_type/0,
         map_type/1,
         map_type_fields/1,
         map_type_assoc/2,
         map_type_assoc_name/1,
         map_type_assoc_value/1,
         map_type_exact/2,
         map_type_exact_name/1,
         map_type_exact_value/1,
	 match_expr/2,
	 match_expr_body/1,
	 match_expr_pattern/1,
         maybe_expr/1,
         maybe_expr/2,
         maybe_expr_body/1,
         maybe_expr_else/1,
         maybe_match_expr/2,
         maybe_match_expr_pattern/1,
         maybe_match_expr_body/1,
	 module_qualifier/2,
	 module_qualifier_argument/1,
	 module_qualifier_body/1,
	 named_fun_expr/2,
	 named_fun_expr_arity/1,
	 named_fun_expr_clauses/1,
	 named_fun_expr_name/1,
	 nil/0,
         strict_binary_generator/2,
         strict_binary_generator_body/1,
         strict_binary_generator_pattern/1,
         strict_generator/2,
         strict_generator_body/1,
         strict_generator_pattern/1,
         strict_map_generator/2,
         strict_map_generator_body/1,
         strict_map_generator_pattern/1,
	 operator/1,
	 operator_literal/1,
	 operator_name/1,
	 parentheses/1,
	 parentheses_body/1,
	 prefix_expr/2,
	 prefix_expr_argument/1,
	 prefix_expr_operator/1,
	 receive_expr/1,
	 receive_expr/3,
	 receive_expr_action/1,
	 receive_expr_clauses/1,
	 receive_expr_timeout/1,
	 record_access/3,
	 record_access_argument/1,
	 record_access_field/1,
	 record_access_type/1,
	 record_expr/2,
	 record_expr/3,
	 record_expr_argument/1,
	 record_expr_fields/1,
	 record_expr_type/1,
	 record_field/1,
	 record_field/2,
	 record_field_name/1,
	 record_field_value/1,
	 record_index_expr/2,
	 record_index_expr_field/1,
	 record_index_expr_type/1,
         record_type/2,
         record_type_name/1,
         record_type_fields/1,
         record_type_field/2,
         record_type_field_name/1,
         record_type_field_type/1,
	 size_qualifier/2,
	 size_qualifier_argument/1,
	 size_qualifier_body/1,
	 string/1,
	 is_string/2,
	 string_value/1,
	 string_literal/1,
	 string_literal/2,
	 text/1,
	 text_string/1,
	 try_expr/2,
	 try_expr/3,
	 try_expr/4,
	 try_after_expr/2,
	 try_expr_body/1,
	 try_expr_clauses/1,
	 try_expr_handlers/1,
	 try_expr_after/1,
         tuple_type/0,
         tuple_type/1,
         tuple_type_elements/1,
         type_application/2,
         type_application/3,
         type_application_name/1,
         type_application_arguments/1,
         type_union/1,
         type_union_types/1,
	 typed_record_field/2,
	 typed_record_field_body/1,
         typed_record_field_type/1,
	 class_qualifier/2,
	 class_qualifier/3,
	 class_qualifier_argument/1,
	 class_qualifier_body/1,
	 class_qualifier_stacktrace/1,
	 tuple/1,
	 tuple_elements/1,
	 tuple_size/1,
	 underscore/0,
         user_type_application/2,
         user_type_application_name/1,
         user_type_application_arguments/1,
	 variable/1,
	 variable_name/1,
	 variable_literal/1,
	 warning_marker/1,
	 warning_marker_info/1,
	 zip_generator/1,
	 zip_generator_body/1,

	 tree/1,
	 tree/2,
	 data/1,
	 is_tree/1]).

-export_type([forms/0, syntaxTree/0, syntaxTreeAttributes/0, padding/0, annotation_or_location/0]).

%% =====================================================================
%% IMPLEMENTATION NOTES:
%%
%% All nodes are represented by tuples of arity 2 or greater, whose
%% first element is an atom which uniquely identifies the type of the
%% node. (In the backwards-compatible representation, the
%% interpretation is also often dependent on the context; the second
%% element generally holds the annotation (see module {@link
%% //stdlib/erl_anno} for details) which includes the position
%% information - with a couple of exceptions; see `get_pos' and
%% `set_pos' for details.) In the documentation of this module, `Pos'
%% is the annotation associated with a node. No assumptions are made
%% in this module regarding the format or interpretation of the
%% annotations. Use module erl_anno to inspect and modify annotations.
%% In particular, use {@link //stdlib/erl_anno:location/1} to get the
%% position information, and use {@link
%% //stdlib/erl_anno:set_location/2} or {@link
%% //stdlib/erl_anno:set_line/2} to change the position information.
%% When a syntax tree node is constructed, its associated position is
%% by default set to the integer zero.
%% =====================================================================

-define(NO_UNUSED, true).

%% =====================================================================
%% Declarations of globally used internal data structures
%% =====================================================================

%% `com' records are used to hold comment information attached to a
%% syntax tree node or a wrapper structure.
%%
%% #com{pre :: Pre, post :: Post}
%%
%%	Pre = Post = [Com]
%%	Com = syntaxTree()
%%
%%	type(Com) = comment

-record(com, {pre  = [] :: [syntaxTree()],
	      post = [] :: [syntaxTree()]}).

%% `attr' records store node attributes as an aggregate.
%%
%% #attr{pos :: Pos, ann :: Ann, com :: Comments}
%%
%%	Pos = term()
%%	Ann = [term()]
%%	Comments = none | #com{}
%%
%% where `Pos' `Ann' and `Comments' are the corresponding values of a
%% `tree' or `wrapper' record.

-record(attr, {pos = erl_anno:new(0) :: term(),
	       ann = []   :: [term()],
	       com = none :: 'none' | #com{}}).
-type syntaxTreeAttributes() :: #attr{}.

%% `tree' records represent new-form syntax tree nodes.
%%
%% Tree = #tree{type :: Type, attr :: Attr, data :: Data}
%%
%%	Type = atom()
%%	Attr = #attr{}
%%	Data = term()
%%
%%	is_tree(Tree) = true

-record(tree, {type           :: atom(),
	       attr = #attr{} :: #attr{},
	       data           :: term()}).
-type tree() :: #tree{}.

%% `wrapper' records are used for attaching new-form node information to
%% `erl_parse' trees.
%%
%% Wrapper = #wrapper{type :: Type, attr :: Attr, tree :: ParseTree}
%%
%%	Type = atom()
%%	Attr = #attr{}
%%	ParseTree = term()
%%
%%	is_tree(Wrapper) = false

-record(wrapper, {type           :: atom(),
		  attr = #attr{} :: #attr{},
		  tree           :: erl_parse()}).
-type wrapper() :: #wrapper{}.

%% =====================================================================

-type syntaxTree() :: tree() | wrapper() | erl_parse().

-type erl_parse() :: erl_parse:abstract_clause()
                   | erl_parse:abstract_expr()
                   | erl_parse:abstract_form()
                   | erl_parse:abstract_type()
                   | erl_parse:form_info()
                   | erl_parse:af_binelement(term())
                   | erl_parse:af_generator()
                   | erl_parse:af_zip_generator()
                   | erl_parse:af_remote_function().

%% The representation built by the Erlang standard library parser
%% `erl_parse'. This is a subset of the {@link syntaxTree()} type.

%% =====================================================================
%%
%%			Exported functions
%%
%% =====================================================================

-doc """
type(Node)

Returns the type tag of `Node`.

If `Node` does not represent a syntax tree, evaluation fails with
reason `badarg`. Node types currently defined by this module are:

* `application`
* `annotated_type`
* `arity_qualifier`
* `atom`
* `attribute`
* `binary`
* `binary_field`
* `bitstring_type`
* `block_expr`
* `case_expr`
* `catch_expr`
* `char`
* `class_qualifier`
* `clause`
* `comment`
* `conjunction`
* `constrained_function_type`
* `constraint`
* `disjunction`
* `else_expr`
* `eof_marker`
* `error_marker`
* `float`
* `form_list`
* `fun_expr`
* `fun_type`
* `function`
* `function_type`
* `generator`
* `if_expr`
* `implicit_fun`
* `infix_expr`
* `integer`
* `integer_range_type`
* `list`
* `list_comp`
* `macro`
* `map_expr`
* `map_field_assoc`
* `map_field_exact`
* `map_type`
* `map_type_assoc`
* `map_type_exact`
* `match_expr`
* `maybe_expr`
* `maybe_match_expr`
* `module_qualifier`
* `named_fun_expr`
* `nil`
* `operator`
* `parentheses`
* `prefix_expr`
* `receive_expr`
* `record_access`
* `record_expr`
* `record_field`
* `record_index_expr`
* `record_type`
* `record_type_field`
* `size_qualifier`
* `string`
* `text`
* `try_expr`
* `tuple`
* `tuple_type`
* `typed_record_field`
* `type_application`
* `type_union`
* `underscore`
* `user_type_application`
* `variable`
* `warning_marker`
* `zip_generator`

The user may (for special purposes) create additional nodes with other type
tags, using the `tree/2` function.

Note: The primary constructor functions for a node type should always have the
same name as the node type itself.

_See also: _`annotated_type/2`, `application/3`, `arity_qualifier/2`, `atom/1`,
`attribute/2`, `binary/1`, `binary_field/2`, `bitstring_type/2`, `block_expr/1`,
`case_expr/2`, `catch_expr/1`, `char/1`, `class_qualifier/2`, `clause/3`,
`comment/2`, `conjunction/1`, `constrained_function_type/2`, `constraint/2`,
`disjunction/1`, `else_expr/1`, `eof_marker/0`, `error_marker/1`, `float/1`,
`form_list/1`, `fun_expr/1`, `fun_type/0`, `function/2`, `function_type/1`,
`function_type/2`, `generator/2`, `if_expr/1`, `implicit_fun/2`, `infix_expr/3`,
`integer/1`, `integer_range_type/2`, `list/2`, `list_comp/2`, `macro/2`,
`map_expr/2`, `map_field_assoc/2`, `map_field_exact/2`, `map_type/0`,
`map_type/1`, `map_type_assoc/2`, `map_type_exact/2`, `match_expr/2`,
`maybe_expr/1`, `maybe_expr/2`, `maybe_match_expr/2`, `module_qualifier/2`,
`named_fun_expr/2`, `nil/0`, `operator/1`, `parentheses/1`, `prefix_expr/2`,
`receive_expr/3`, `record_access/3`, `record_expr/2`, `record_field/2`,
`record_index_expr/2`, `record_type/2`, `record_type_field/2`,
`size_qualifier/2`, `string/1`, `text/1`, `tree/2`, `try_expr/3`, `tuple/1`,
`tuple_type/0`, `tuple_type/1`, `type_application/2`, `type_union/1`,
`typed_record_field/2`, `underscore/0`, `user_type_application/2`, `variable/1`,
`warning_marker/1`,`zip_generator/1`.
""".
-spec type(syntaxTree()) -> atom().

type(#tree{type = T}) ->
    T;
type(#wrapper{type = T}) ->
    T;
type(Node) ->
    %% Check for `erl_parse'-compatible nodes, and otherwise fail.
    case Node of
	%% Leaf types
	{atom, _, _} -> atom;
	{char, _, _} -> char;
	{float, _, _} -> float;
	{integer, _, _} -> integer;
	{nil, _} -> nil;
	{string, _, _} -> string;
	{var, _, Name} ->
	    if Name =:= '_' -> underscore;
	       true -> variable
	    end;
	{error, _} -> error_marker;
	{warning, _} -> warning_marker;
	{eof, _} -> eof_marker;

	%% Composite types
	{'case', _, _, _} -> case_expr;
	{'catch', _, _} -> catch_expr;
	{'fun', _, {clauses, _}} -> fun_expr;
	{named_fun, _, _, _} -> named_fun_expr;
	{'fun', _, {function, _, _}} -> implicit_fun;
	{'fun', _, {function, _, _, _}} -> implicit_fun;
	{'if', _, _} -> if_expr;
        {'maybe', _, _} -> maybe_expr;
        {'maybe', _, _, _} -> maybe_expr;
        {'else', _, _} -> else_expr;
	{'receive', _, _, _, _} -> receive_expr;
	{'receive', _, _} -> receive_expr;
	{attribute, _, _, _} -> attribute;
	{bin, _, _} -> binary;
	{bin_element, _, _, _, _} -> binary_field;
	{block, _, _} -> block_expr;
	{call, _, _, _} -> application;
	{clause, _, _, _, _} -> clause;
	{cons, _, _, _} -> list;
	{function, _, _, _, _} -> function;
	{b_generate, _, _, _} -> binary_generator;
	{b_generate_strict, _, _, _} -> strict_binary_generator;
	{generate, _, _, _} -> generator;
	{generate_strict, _, _, _} -> strict_generator;
	{m_generate, _, _, _} -> map_generator;
	{m_generate_strict, _, _, _} -> strict_map_generator;
	{zip,_,_} -> zip_generator;
	{lc, _, _, _} -> list_comp;
	{bc, _, _, _} -> binary_comp;
	{mc, _, _, _} -> map_comp;
	{match, _, _, _} -> match_expr;
        {map, _, _, _} -> map_expr;
        {map, _, _} -> map_expr;
        {map_field_assoc, _, _, _} -> map_field_assoc;
        {map_field_exact, _, _, _} -> map_field_exact;
        {maybe_match, _, _, _} -> maybe_match_expr;
	{op, _, _, _, _} -> infix_expr;
	{op, _, _, _} -> prefix_expr;
	{record, _, _, _, _} -> record_expr;
	{record, _, _, _} -> record_expr;
	{record_field, _, _, _, _} -> record_access;
	{record_index, _, _, _} -> record_index_expr;
	{remote, _, _, _} -> module_qualifier;
	{'try', _, _, _, _, _} -> try_expr;
	{tuple, _, _} -> tuple;

        %% Type types
        {ann_type, _, _} -> annotated_type;
        {remote_type, _, _} -> type_application;
        {type, _, binary, [_, _]} -> bitstring_type;
        {type, _, bounded_fun, [_, _]} -> constrained_function_type;
        {type, _, constraint, [_, _]} -> constraint;
        {type, _, 'fun', []} -> fun_type;
        {type, _, 'fun', [_, _]} -> function_type;
        {type, _, map, _} -> map_type;
        {type, _, map_field_assoc, _} -> map_type_assoc;
        {type, _, map_field_exact, _} -> map_type_exact;
        {type, _, record, _} -> record_type;
        {type, _, field_type, _} -> record_type_field;
        {type, _, range, _} -> integer_range_type;
        {type, _, tuple, _} -> tuple_type;
        {type, _, union, _} -> type_union;
        {type, _, _, _} -> type_application;
        {user_type, _, _, _} -> user_type_application;
	_ ->
	    erlang:error({badarg, Node})
    end.


-doc """
Returns `true` if `Node` is a leaf node, otherwise `false`.

The currently recognised leaf node types are:

* `atom`
* `char`
* `comment`
* `eof_marker`
* `error_marker`
* `float`
* `fun_type`
* `integer`
* `nil`
* `operator`
* `string`
* `text`
* `underscore`
* `variable`
* `warning_marker`

A node of type `map_expr` is a leaf node if and only if it has no argument and
no fields. A node of type `map_type` is a leaf node if and only if it has no
fields (`any_size`). A node of type `tuple` is a leaf node if and only if its
arity is zero. A node of type `tuple_type` is a leaf node if and only if it has
no elements (`any_size`).

Note: not all literals are leaf nodes, and vice versa. For example,
tuples with nonzero arity and nonempty lists may be literals, but are
not leaf nodes. Variables, on the other hand, are leaf nodes but not
literals.

_See also: _`is_literal/1`, `type/1`.
""".
-spec is_leaf(syntaxTree()) -> boolean().

is_leaf(Node) ->
    case type(Node) of
	atom -> true;
	char -> true;
	comment -> true;	% nonstandard type
	eof_marker -> true;
	error_marker -> true;
	float -> true;
        fun_type -> true;
	integer -> true;
	nil -> true;
	operator -> true;	% nonstandard type
	string -> true;
	text -> true;		% nonstandard type
	map_expr ->
	    map_expr_fields(Node) =:= [] andalso
	    map_expr_argument(Node) =:= none;
        map_type -> map_type_fields(Node) =:= any_size;
	tuple -> tuple_elements(Node) =:= [];
        tuple_type -> tuple_type_elements(Node) =:= any_size;
	underscore -> true;
	variable -> true;
	warning_marker -> true;
	_ -> false
    end.


-doc """
Returns `true` if `Node` is a syntax tree representing a so-called "source code
form", otherwise `false`.

Forms are the Erlang source code units which, placed in sequence,
constitute an Erlang program. Current form types are:

* `attribute`
* `comment`
* `error_marker`
* `eof_marker`
* `form_list`
* `function`
* `warning_marker`
* `text`


_See also: _`attribute/2`, `comment/2`, `eof_marker/0`, `error_marker/1`,
`form_list/1`, `function/2`, `type/1`, `warning_marker/1`.
""".
-spec is_form(syntaxTree()) -> boolean().

is_form(Node) ->
    case type(Node) of
	attribute -> true;
	comment -> true;
	function -> true;
	eof_marker -> true;
	error_marker -> true;
	form_list -> true;
	warning_marker -> true;
	text -> true;
	_ -> false
    end.


%% =====================================================================

%% All `erl_parse' tree nodes are represented by tuples whose second
%% field is the annotation, *with the
%% exceptions of* `{error, ...}' (type `error_marker') and `{warning,
%% ...}' (type `warning_marker'), which only contain the associated location
%% *of the error descriptor*; this is all handled transparently
%% by `get_pos/1' and `set_pos/2'.

-type annotation_or_location() :: erl_anno:anno() | erl_anno:location().

-doc """
get_pos(Node)

Returns the annotation (see [`//stdlib/erl_anno`](`m:erl_anno`)) associated with
`Node`.

By default, all new tree nodes have their associated position
information set to the integer zero. Use
[`//stdlib/erl_anno:location/1`](`erl_anno:location/1`) or
[`//stdlib/erl_anno:line/1`](`erl_anno:line/1`) to get the position information.

_See also: _`get_attrs/1`, `set_pos/2`.
""".
-spec get_pos(syntaxTree()) -> annotation_or_location().

get_pos(#tree{attr = Attr}) ->
    Attr#attr.pos;
get_pos(#wrapper{attr = Attr}) ->
    Attr#attr.pos;
get_pos({error, {Pos, _, _}}) ->
    Pos;
get_pos({warning, {Pos, _, _}}) ->
    Pos;
get_pos(Node) ->
    %% Here, we assume that we have an `erl_parse' node with an
    %% annotation in element 2.
    element(2, Node).


-doc """
Sets the position information of `Node` to `Pos`.

_See also: _`copy_pos/2`, `get_pos/1`.
""".
-spec set_pos(syntaxTree(), annotation_or_location()) -> syntaxTree().

set_pos(Node, Pos) ->
    case Node of
	#tree{attr = Attr} ->
	    Node#tree{attr = Attr#attr{pos = Pos}};
	#wrapper{attr = Attr} ->
	    Node#wrapper{attr = Attr#attr{pos = Pos}};
	_ ->
	    %% We then assume we have an `erl_parse' node, and create a
	    %% wrapper around it to make things more uniform.
	    set_pos(wrap(Node), Pos)
    end.


-doc """
Copies the annotation from `Source` to `Target`.

This is equivalent to [`set_pos(Target, get_pos(Source))`](`set_pos/2`), but
potentially more efficient.

_See also: _`get_pos/1`, `set_pos/2`.
""".
-spec copy_pos(syntaxTree(), syntaxTree()) -> syntaxTree().

copy_pos(Source, Target) ->
    set_pos(Target, get_pos(Source)).


%% =====================================================================
%% `get_com' and `set_com' are for internal use only.

get_com(#tree{attr = Attr}) -> Attr#attr.com;
get_com(#wrapper{attr = Attr}) -> Attr#attr.com;
get_com(_) -> none.

set_com(Node, Com) ->
    case Node of
	#tree{attr = Attr} ->
	    Node#tree{attr = Attr#attr{com = Com}};
	#wrapper{attr = Attr} ->
	    Node#wrapper{attr = Attr#attr{com = Com}};
	_ ->
	    set_com(wrap(Node), Com)
    end.


-doc """
get_precomments(Node)

Returns the associated pre-comments of a node.

This is a possibly empty list of abstract comments, in top-down
textual order. When the code is formatted, pre-comments are typically
displayed directly above the node. For example:

```erlang
% Pre-comment of function
foo(X) -> {bar, X}.
```

If possible, the comment should be moved before any preceding separator
characters on the same line. For example:

```erlang
foo([X | Xs]) ->
    % Pre-comment of 'bar(X)' node
    [bar(X) | foo(Xs)];
...
```

(where the comment is moved before the "`[`").

_See also: _`comment/2`, `get_attrs/1`, `get_postcomments/1`,
`set_precomments/2`.
""".
-spec get_precomments(syntaxTree()) -> [syntaxTree()].

get_precomments(#tree{attr = Attr}) -> get_precomments_1(Attr);
get_precomments(#wrapper{attr = Attr}) -> get_precomments_1(Attr);
get_precomments(_) -> [].

get_precomments_1(#attr{com = none}) -> [];
get_precomments_1(#attr{com = #com{pre = Cs}}) -> Cs.


-doc """
set_precomments(Node, Comments)

Sets the pre-comments of `Node` to `Comments`.

`Comments` should be a possibly empty list of abstract comments, in
top-down textual order.

_See also: _`add_precomments/2`, `comment/2`, `copy_comments/2`,
`get_precomments/1`, `join_comments/2`, `remove_comments/1`,
`set_postcomments/2`.
""".
-spec set_precomments(syntaxTree(), [syntaxTree()]) -> syntaxTree().

set_precomments(Node, Cs) ->
    case Node of
	#tree{attr = Attr} ->
	    Node#tree{attr = set_precomments_1(Attr, Cs)};
	#wrapper{attr = Attr} ->
	    Node#wrapper{attr = set_precomments_1(Attr, Cs)};
	_ ->
	    set_precomments(wrap(Node), Cs)
    end.

set_precomments_1(#attr{com = none} = Attr, Cs) ->
    Attr#attr{com = #com{pre = Cs}};
set_precomments_1(#attr{com = Com} = Attr, Cs) ->
    Attr#attr{com = Com#com{pre = Cs}}.


-doc """
add_precomments(Comments, Node)

Appends `Comments` to the pre-comments of `Node`.

Note: This is equivalent to
[`set_precomments(Node, get_precomments(Node) ++ Comments)`](`set_precomments/2`),
but potentially more efficient.

_See also: _`add_postcomments/2`, `comment/2`, `get_precomments/1`,
`join_comments/2`, `set_precomments/2`.
""".
-spec add_precomments([syntaxTree()], syntaxTree()) -> syntaxTree().

add_precomments(Cs, Node) ->
    case Node of
	#tree{attr = Attr} ->
	    Node#tree{attr = add_precomments_1(Cs, Attr)};
	#wrapper{attr = Attr} ->
	    Node#wrapper{attr = add_precomments_1(Cs, Attr)};
	_ ->
	    add_precomments(Cs, wrap(Node))
    end.

add_precomments_1(Cs, #attr{com = none} = Attr) ->
    Attr#attr{com = #com{pre = Cs}};
add_precomments_1(Cs, #attr{com = Com} = Attr) ->
    Attr#attr{com = Com#com{pre = Com#com.pre ++ Cs}}.


-doc """
get_postcomments(Node)

Returns the associated post-comments of a node.

This is a possibly empty list of abstract comments, in top-down
textual order. When the code is formatted, post-comments are typically
displayed to the right of and/or below the node. For example:

```erlang
{foo, X, Y}     % Post-comment of tuple
```

If possible, the comment should be moved past any following separator characters
on the same line, rather than placing the separators on the following line.
For example:

```erlang
foo([X | Xs], Y) ->
    foo(Xs, bar(X));     % Post-comment of 'bar(X)' node
 ...
```

(where the comment is moved past the rightmost "`)`" and the "`;`").

_See also: _`comment/2`, `get_attrs/1`, `get_precomments/1`,
`set_postcomments/2`.
""".
-spec get_postcomments(syntaxTree()) -> [syntaxTree()].

get_postcomments(#tree{attr = Attr}) -> get_postcomments_1(Attr);
get_postcomments(#wrapper{attr = Attr}) -> get_postcomments_1(Attr);
get_postcomments(_) -> [].

get_postcomments_1(#attr{com = none}) -> [];
get_postcomments_1(#attr{com = #com{post = Cs}}) -> Cs.


-doc """
set_postcomments(Node, Comments)

Sets the post-comments of `Node` to `Comments`.

`Comments` should be a possibly empty list of abstract comments, in
top-down textual order

_See also: _`add_postcomments/2`, `comment/2`, `copy_comments/2`,
`get_postcomments/1`, `join_comments/2`, `remove_comments/1`,
`set_precomments/2`.
""".
-spec set_postcomments(syntaxTree(), [syntaxTree()]) -> syntaxTree().

set_postcomments(Node, Cs) ->
    case Node of
	#tree{attr = Attr} ->
	    Node#tree{attr = set_postcomments_1(Attr, Cs)};
	#wrapper{attr = Attr} ->
	    Node#wrapper{attr = set_postcomments_1(Attr, Cs)};
	_ ->
	    set_postcomments(wrap(Node), Cs)
    end.

set_postcomments_1(#attr{com = none} = Attr, Cs) ->
    Attr#attr{com = #com{post = Cs}};
set_postcomments_1(#attr{com = Com} = Attr, Cs) ->
    Attr#attr{com = Com#com{post = Cs}}.


-doc """
add_postcomments(Comments, Node)

Appends `Comments` to the post-comments of `Node`.

Note: This is equivalent to
[`set_postcomments(Node, get_postcomments(Node) ++ Comments)`](`set_postcomments/2`),
but potentially more efficient.

_See also: _`add_precomments/2`, `comment/2`, `get_postcomments/1`,
`join_comments/2`, `set_postcomments/2`.
""".
-spec add_postcomments([syntaxTree()], syntaxTree()) -> syntaxTree().

add_postcomments(Cs, Node) ->
    case Node of
	#tree{attr = Attr} ->
	    Node#tree{attr = add_postcomments_1(Cs, Attr)};
	#wrapper{attr = Attr} ->
	    Node#wrapper{attr = add_postcomments_1(Cs, Attr)};
	_ ->
	    add_postcomments(Cs, wrap(Node))
    end.

add_postcomments_1(Cs, #attr{com = none} = Attr) ->
    Attr#attr{com = #com{post = Cs}};
add_postcomments_1(Cs, #attr{com = Com} = Attr) ->
    Attr#attr{com = Com#com{post = Com#com.post ++ Cs}}.


-doc """
has_comments(Node)

Yields `false` if the node has no associated comments, and `true` otherwise.

Note: This is equivalent to
`(get_precomments(Node) == []) and (get_postcomments(Node) == [])`, but
potentially more efficient.

_See also: _`get_postcomments/1`, `get_precomments/1`, `remove_comments/1`.
""".
-spec has_comments(syntaxTree()) -> boolean().

has_comments(#tree{attr = Attr}) ->
    case Attr#attr.com of
	none -> false;
	#com{pre = [], post = []} -> false;
	_ -> true
    end;
has_comments(#wrapper{attr = Attr}) ->
    case Attr#attr.com of
	none -> false;
	#com{pre = [], post = []} -> false;
	_ -> true
    end;
has_comments(_) -> false.


-doc """
Clears the associated comments of `Node`.

Note: This is equivalent to
[`set_precomments(set_postcomments(Node, []), [])`](`set_precomments/2`), but
potentially more efficient.

_See also: _`set_postcomments/2`, `set_precomments/2`.
""".
-spec remove_comments(syntaxTree()) -> syntaxTree().

remove_comments(Node) ->
    case Node of
	#tree{attr = Attr} ->
	    Node#tree{attr = Attr#attr{com = none}};
	#wrapper{attr = Attr} ->
	    Node#wrapper{attr = Attr#attr{com = none}};
	_ ->
	    Node
    end.


-doc """
Copies the pre- and postcomments from `Source` to `Target`.

Note: This is equivalent to
[`set_postcomments(set_precomments(Target, get_precomments(Source)), get_postcomments(Source))`](`set_postcomments/2`),
but potentially more efficient.

_See also: _`comment/2`, `get_postcomments/1`, `get_precomments/1`,
`set_postcomments/2`, `set_precomments/2`.
""".
-spec copy_comments(syntaxTree(), syntaxTree()) -> syntaxTree().

copy_comments(Source, Target) ->
    set_com(Target, get_com(Source)).


-doc """
Appends the comments of `Source` to the current comments of `Target`.

Note: This is equivalent to
[`add_postcomments(get_postcomments(Source), add_precomments(get_precomments(Source), Target))`](`add_postcomments/2`),
but potentially more efficient.

_See also: _`add_postcomments/2`, `add_precomments/2`, `comment/2`,
`get_postcomments/1`, `get_precomments/1`.
""".
-spec join_comments(syntaxTree(), syntaxTree()) -> syntaxTree().

join_comments(Source, Target) ->
    add_postcomments(
      get_postcomments(Source),
      add_precomments(get_precomments(Source), Target)).


-doc """
get_ann(Node)

Returns the list of user annotations associated with a syntax tree.

For a newly created node, this is the empty list. The annotations may
be any terms.

_See also: _`get_attrs/1`, `set_ann/2`.
""".
-spec get_ann(syntaxTree()) -> [term()].

get_ann(#tree{attr = Attr}) -> Attr#attr.ann;
get_ann(#wrapper{attr = Attr}) -> Attr#attr.ann;
get_ann(_) -> [].


-doc """
set_ann(Node, Annotations)

Sets the list of user annotations of `Node` to `Annotations`.

_See also: _`add_ann/2`, `copy_ann/2`, `get_ann/1`.
""".
-spec set_ann(syntaxTree(), [term()]) -> syntaxTree().

set_ann(Node, As) ->
    case Node of
	#tree{attr = Attr} ->
	    Node#tree{attr = Attr#attr{ann = As}};
	#wrapper{attr = Attr} ->
	    Node#wrapper{attr = Attr#attr{ann = As}};
	_ ->
	    %% Assume we have an `erl_parse' node and create a wrapper
	    %% structure to carry the annotation.
	    set_ann(wrap(Node), As)
    end.


-doc """
add_ann(Annotation, Node)

Appends the term `Annotation` to the list of user annotations of `Node`.

Note: this is equivalent to
[`set_ann(Node, [Annotation | get_ann(Node)])`](`set_ann/2`), but potentially
more efficient.

_See also: _`get_ann/1`, `set_ann/2`.
""".
-spec add_ann(term(), syntaxTree()) -> syntaxTree().

add_ann(A, Node) ->
    case Node of
	#tree{attr = Attr} ->
	    Node#tree{attr = Attr#attr{ann = [A | Attr#attr.ann]}};
	#wrapper{attr = Attr} ->
	    Node#wrapper{attr = Attr#attr{ann = [A | Attr#attr.ann]}};
	_ ->
	    %% Assume we have an `erl_parse' node and create a wrapper
	    %% structure to carry the annotation.
	    add_ann(A, wrap(Node))
    end.


-doc """
Copies the list of user annotations from `Source` to `Target`.

Note: this is equivalent to [`set_ann(Target, get_ann(Source))`](`set_ann/2`),
but potentially more efficient.

_See also: _`get_ann/1`, `set_ann/2`.
""".
-spec copy_ann(syntaxTree(), syntaxTree()) -> syntaxTree().

copy_ann(Source, Target) ->
    set_ann(Target, get_ann(Source)).


-doc """
get_attrs(Node)

Returns a representation of the attributes associated with a syntax tree node.

The attributes are all the extra information that can be attached to a node.
Currently, this includes position information, source code comments, and user
annotations. The result of this function cannot be inspected directly; only
attached to another node (see `set_attrs/2`).

For accessing individual attributes, see `get_pos/1`, `get_ann/1`,
`get_precomments/1` and `get_postcomments/1`.

_See also: _`get_ann/1`, `get_pos/1`, `get_postcomments/1`, `get_precomments/1`,
`set_attrs/2`.
""".
-spec get_attrs(syntaxTree()) -> syntaxTreeAttributes().

get_attrs(#tree{attr = Attr}) -> Attr;
get_attrs(#wrapper{attr = Attr}) -> Attr;
get_attrs(Node) -> #attr{pos = get_pos(Node),
			 ann = get_ann(Node),
			 com = get_com(Node)}.


-doc """
set_attrs(Node, Attributes)

Sets the attributes of `Node` to `Attributes`.

_See also: _`copy_attrs/2`, `get_attrs/1`.
""".
-spec set_attrs(syntaxTree(), syntaxTreeAttributes()) -> syntaxTree().

set_attrs(Node, Attr) ->
    case Node of
	#tree{} ->
	    Node#tree{attr = Attr};
	#wrapper{} ->
	    Node#wrapper{attr = Attr};
	_ ->
	    set_attrs(wrap(Node), Attr)
    end.


-doc """
copy_attrs(Source, Target)

Copies the attributes from `Source` to `Target`.

Note: this is equivalent to
[`set_attrs(Target, get_attrs(Source))`](`set_attrs/2`), but potentially more
efficient.

_See also: _`get_attrs/1`, `set_attrs/2`.
""".
-spec copy_attrs(syntaxTree(), syntaxTree()) -> syntaxTree().

copy_attrs(S, T) ->
    set_attrs(T, get_attrs(S)).


%% =====================================================================

-doc #{equiv => comment(none, Strings)}.
-spec comment([string()]) -> syntaxTree().

comment(Strings) ->
    comment(none, Strings).


-type padding() :: 'none' | integer().

-record(comment, {pad :: padding(), text :: [string()]}).

-doc """
Creates an abstract comment with the given padding and text.

If `Strings` is a (possibly empty) list `["Txt1", ..., "TxtN"]`,
the result represents the source code text

```text
     Txt1
     ...
     TxtN
```

`Padding` states the number of empty character positions to the left of the
comment separating it horizontally from source code on the same line (if any).
If `Padding` is `none`, a default positive number is used. If `Padding` is an
integer less than 1, there should be no separating space. Comments are in
themselves regarded as source program forms.

_See also: _`comment/1`, `is_form/1`.
""".
-spec comment(padding(), [string()]) -> syntaxTree().

comment(Pad, Strings) ->
    tree(comment, #comment{pad = Pad, text = Strings}).


-doc """
Returns the lines of text of the abstract comment.

_See also: _`comment/2`.
""".
-spec comment_text(syntaxTree()) -> [string()].

comment_text(Node) ->
    (data(Node))#comment.text.


-doc """
Returns the amount of padding before the comment, or `none`.

`none` means that a default padding may be used.

_See also: _`comment/2`.
""".
-spec comment_padding(syntaxTree()) -> padding().

comment_padding(Node) ->
    (data(Node))#comment.pad.


-doc """
Creates an abstract sequence of "source code forms".

If `Forms` is `[F1, ..., Fn]`, where each `Fi` is a form (see
`is_form/1`), the result represents:

```text
F1
...
Fn
```

where the `Fi` are separated by one or more line breaks. A node of type
`form_list` is itself regarded as a source code form; see `flatten_form_list/1`.

> #### Note {: .info }
>
> This is simply a way of grouping source code forms into a single syntax
tree, usually to form an Erlang module definition.

_See also: _`flatten_form_list/1`, `form_list_elements/1`, `is_form/1`.
""".
-spec form_list([syntaxTree()]) -> syntaxTree().

form_list(Forms) ->
    tree(form_list, Forms).


-doc """
Returns the list of subnodes of a `form_list` node.

_See also: _`form_list/1`.
""".
-spec form_list_elements(syntaxTree()) -> [syntaxTree()].

form_list_elements(Node) ->
    data(Node).


-doc """
Flattens sublists of a `form_list` node.

Returns `Node` with all subtrees of type `form_list` recursively
expanded, yielding a single "flat" abstract form sequence.

_See also: _`form_list/1`.
""".
-spec flatten_form_list(syntaxTree()) -> syntaxTree().

flatten_form_list(Node) ->
    Fs = form_list_elements(Node),
    Fs1 = lists:reverse(flatten_form_list_1(Fs, [])),
    copy_attrs(Node, form_list(Fs1)).

flatten_form_list_1([F | Fs], As) ->
    case type(F) of
	form_list ->
	    As1 = flatten_form_list_1(form_list_elements(F), As),
	    flatten_form_list_1(Fs, As1);
	_ ->
	    flatten_form_list_1(Fs, [F | As])
    end;
flatten_form_list_1([], As) ->
    As.


-doc """
Creates an abstract piece of source code text.

The result represents exactly the sequence of characters in
`String`. This is useful in cases where one wants full control of the
resulting output, such as the appearance of floating-point numbers or
macro definitions.

_See also: _`text_string/1`.
""".
-spec text(string()) -> syntaxTree().

text(String) ->
    tree(text, String).


-doc """
Returns the character sequence represented by a `text` node.

_See also: _`text/1`.
""".
-spec text_string(syntaxTree()) -> string().

text_string(Node) ->
    data(Node).


-doc """
variable(Name)

Creates an abstract variable with the given name.

`Name` may be any atom or string that represents a lexically valid
variable name, but _not_ a single underscore character; see
`underscore/0`.

> #### Note {: .info }
>
> No check is performed to verify whether the character sequence
> represents a proper variable name, that is, whether its first character
> is an uppercase Erlang character, or whether it contains illegal characters
> such as control characters or whitespace.

_See also: _`underscore/0`, `variable_literal/1`, `variable_name/1`.
""".
-spec variable(atom() | string()) -> syntaxTree().

variable(Name) when is_atom(Name) ->
    tree(variable, Name);
variable(Name) ->
    tree(variable, list_to_atom(Name)).

revert_variable(Node) ->
    Pos = get_pos(Node),
    Name = variable_name(Node),
    {var, Pos, Name}.


-doc """
Returns the name of a `variable` node as an atom.

_See also: _`variable/1`.
""".
-spec variable_name(syntaxTree()) -> atom().

variable_name(Node) ->
    case unwrap(Node) of
	{var, _, Name} ->
	    Name;
	Node1 ->
	    data(Node1)
    end.


-doc """
Returns the name of a `variable` node as a string.

_See also: _`variable/1`.
""".
-spec variable_literal(syntaxTree()) -> string().

variable_literal(Node) ->
    case unwrap(Node) of
	{var, _, Name} ->
	    atom_to_list(Name);
	Node1 ->
	    atom_to_list(data(Node1))
    end.


-doc """
Creates an abstract universal pattern ("`_`").

The lexical representation is a single underscore character. Note that
this is _not_ a variable, lexically speaking.

_See also: _`variable/1`.
""".
-spec underscore() -> syntaxTree().

%% `erl_parse' representation:
%%
%% {var, Pos, '_'}

underscore() ->
    tree(underscore, []).

revert_underscore(Node) ->
    Pos = get_pos(Node),
    {var, Pos, '_'}.


-doc """
Creates an abstract integer literal.

The lexical representation is the canonical decimal numeral of `Value`.

_See also: _`integer_literal/1`, `integer_value/1`, `is_integer/2`.
""".
-spec integer(integer()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {integer, Pos, Value}
%%
%%	Value = integer()

integer(Value) ->
    tree(integer, Value).

revert_integer(Node) ->
    Pos = get_pos(Node),
    {integer, Pos, integer_value(Node)}.


-doc """
Returns `true` if `Node` has type `integer` and represents `Value`, otherwise
`false`.

_See also: _`integer/1`.
""".
-spec is_integer(syntaxTree(), integer()) -> boolean().

is_integer(Node, Value) ->
    case unwrap(Node) of
	{integer, _, Value} ->
	    true;
	#tree{type = integer, data = Value} ->
	    true;
	_ ->
	    false
    end.


-doc """
Returns the value represented by an `integer` node.

_See also: _`integer/1`.
""".
-spec integer_value(syntaxTree()) -> integer().

integer_value(Node) ->
    case unwrap(Node) of
	{integer, _, Value} ->
	    Value;
	Node1 ->
	    data(Node1)
    end.


-doc """
Returns the numeral string represented by an `integer` node.

_See also: _`integer/1`.
""".
-spec integer_literal(syntaxTree()) -> string().

integer_literal(Node) ->
    integer_to_list(integer_value(Node)).


%% Note that under current versions of Erlang, the name `float/1' cannot
%% be used for local calls (i.e., within the module) - it will be
%% overridden by the type conversion BIF of the same name, so always use
%% `make_float/1' for local calls.

-doc """
Creates an abstract floating-point literal.

The lexical representation is the decimal floating-point numeral of
`Value`.

_See also: _`float_literal/1`, `float_value/1`.
""".
-spec float(float()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {float, Pos, Value}
%%
%%	Value = float()

float(Value) ->
    make_float(Value).

make_float(Value) ->
    tree(float, Value).

revert_float(Node) ->
    Pos = get_pos(Node),
    {float, Pos, float_value(Node)}.


-doc """
Returns the value represented by a `float` node.

Note that floating-point values should usually not be compared for
equality.

_See also: _`float/1`.
""".
-spec float_value(syntaxTree()) -> float().

float_value(Node) ->
    case unwrap(Node) of
	{float, _, Value} ->
	    Value;
	Node1 ->
	    data(Node1)
    end.


-doc """
Returns the numeral string represented by a `float` node.

_See also: _`float/1`.
""".
-spec float_literal(syntaxTree()) -> string().

float_literal(Node) ->
    float_to_list(float_value(Node)).


-doc """
char(Value)

Creates an abstract character literal.

The result represents "`$Name`", where `Name` corresponds to `Value`.

> #### Note {: .info }

The literal corresponding to a particular character value is not
uniquely defined. For example, the character "`a`" can be written both
as "`$a`" and "`$\141`", and a Tab character can be written as
"`$\11`", "`$\011`", or "`$\t`".

_See also: _`char_literal/1`, `char_literal/2`, `char_value/1`, `is_char/2`.
""".
-spec char(char()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {char, Pos, Code}
%%
%%	Code = integer()

char(Char) ->
    tree(char, Char).

revert_char(Node) ->
    Pos = get_pos(Node),
    {char, Pos, char_value(Node)}.


-doc """
Returns `true` if `Node` has type `char` and represents `Value`, otherwise
`false`.

_See also: _`char/1`.
""".
-spec is_char(syntaxTree(), char()) -> boolean().

is_char(Node, Value) ->
    case unwrap(Node) of
	{char, _, Value} ->
	    true;
	#tree{type = char, data = Value} ->
	    true;
	_ ->
	    false
    end.


-doc """
Returns the value represented by a `char` node.

_See also: _`char/1`.
""".
-spec char_value(syntaxTree()) -> char().

char_value(Node) ->
    case unwrap(Node) of
	{char, _, Char} ->
	    Char;
	Node1 ->
	    data(Node1)
    end.


-doc """
Returns the literal string represented by a `char` node.

This includes the leading "`$`" character. Characters beyond 255 will
be escaped.

_See also: _`char/1`.
""".
-spec char_literal(syntaxTree()) -> nonempty_string().

char_literal(Node) ->
    char_literal(Node, latin1).


-type encoding() :: 'utf8' | 'unicode' | 'latin1'.

-doc """
char_literal(Node, Encoding)

Returns the literal string represented by a `char` node.

This includes the leading "`$`" character. Depending on the encoding a
character beyond 255 will be escaped (`latin1`) or copied as is
(`utf8`).

_See also: _`char/1`.
""".
-spec char_literal(syntaxTree(), encoding()) -> nonempty_string().

char_literal(Node, unicode) ->
    io_lib:write_char(char_value(Node));
char_literal(Node, utf8) ->
    io_lib:write_char(char_value(Node));
char_literal(Node, latin1) ->
    io_lib:write_char_as_latin1(char_value(Node)).


-doc """
Creates an abstract string literal.

The result represents `"Text"` (including the surrounding
double-quotes), where `Text` corresponds to the sequence of characters
in `Value`, but not representing a _specific_ string literal.

For example, the result of [`string("x\ny")`](`string/1`) represents any and all
of `"x\ny"`, `"x\12y"`, `"x\012y"` and `"x\^Jy"`; see `char/1`.

_See also: _`char/1`, `is_string/2`, `string_literal/1`, `string_literal/2`,
`string_value/1`.
""".
-spec string(string()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {string, Pos, Chars}
%%
%%	Chars = string()

string(String) ->
    tree(string, String).

revert_string(Node) ->
    Pos = get_pos(Node),
    {string, Pos, string_value(Node)}.


-doc """
Returns `true` if `Node` has type `string` and represents `Value`, otherwise
`false`.

_See also: _`string/1`.
""".
-spec is_string(syntaxTree(), string()) -> boolean().

is_string(Node, Value) ->
    case unwrap(Node) of
	{string, _, Value} ->
	    true;
	#tree{type = string, data = Value} ->
	    true;
	_ ->
	    false
    end.


-doc """
Returns the value represented by a `string` node.

_See also: _`string/1`.
""".
-spec string_value(syntaxTree()) -> string().

string_value(Node) ->
    case unwrap(Node) of
	{string, _, List} ->
	    List;
	Node1 ->
	    data(Node1)
    end.


-doc """
Returns the literal string represented by a `string` node.

This includes surrounding double-quote characters. Characters beyond
255 will be escaped.

_See also: _`string/1`.
""".
-spec string_literal(syntaxTree()) -> nonempty_string().

string_literal(Node) ->
    string_literal(Node, latin1).


-doc """
string_literal(Node, Encoding)

Returns the literal string represented by a `string` node.

This includes surrounding double-quote characters. Depending on the
encoding characters beyond 255 will be escaped (`latin1`) or copied as
is (`utf8`).

_See also: _`string/1`.
""".
-spec string_literal(syntaxTree(), encoding()) -> nonempty_string().

string_literal(Node, utf8) ->
    io_lib:write_string(string_value(Node));
string_literal(Node, unicode) ->
    io_lib:write_string(string_value(Node));
string_literal(Node, latin1) ->
    io_lib:write_string_as_latin1(string_value(Node)).


-doc """
atom(Name)

Creates an abstract atom literal.

The print name of the atom is the character sequence represented by
`Name`.

_See also: _`atom_literal/1`, `atom_literal/2`, `atom_name/1`, `atom_value/1`,
`is_atom/2`.
""".
-spec atom(atom() | string()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {atom, Pos, Value}
%%
%%	Value = atom()

atom(Name) when is_atom(Name) ->
    tree(atom, Name);
atom(Name) ->
    tree(atom, list_to_atom(Name)).

revert_atom(Node) ->
    Pos = get_pos(Node),
    {atom, Pos, atom_value(Node)}.


-doc """
Returns `true` if `Node` has type `atom` and represents `Value`, otherwise
`false`.

_See also: _`atom/1`.
""".
-spec is_atom(syntaxTree(), atom()) -> boolean().

is_atom(Node, Value) ->
    case unwrap(Node) of
	{atom, _, Value} ->
	    true;
	#tree{type = atom, data = Value} ->
	    true;
	_ ->
	    false
    end.


-doc """
Returns the value represented by an `atom` node.

_See also: _`atom/1`.
""".
-spec atom_value(syntaxTree()) -> atom().

atom_value(Node) ->
    case unwrap(Node) of
	{atom, _, Name} ->
	    Name;
	Node1 ->
	    data(Node1)
    end.


-doc """
Returns the printname of an `atom` node.

_See also: _`atom/1`.
""".
-spec atom_name(syntaxTree()) -> string().

atom_name(Node) ->
    atom_to_list(atom_value(Node)).


-doc """
Returns the literal string represented by an `atom` node.

This includes surrounding single-quote characters if
necessary. Characters beyond 255 will be escaped.

Note that, for example, the result of [`atom("x\ny")`](`atom/1`)
represents any and all of `'x\ny'`, `'x\12y'`, `'x\012y'`, and
`'x\^Jy'`; see `string/1`.

_See also: _`atom/1`, `string/1`.
""".
-spec atom_literal(syntaxTree()) -> string().

atom_literal(Node) ->
    atom_literal(Node, latin1).

-doc """
atom_literal(Node, Encoding)

Returns the literal string represented by an `atom` node.

This includes surrounding single-quote characters if
necessary. Depending on the encoding a character beyond 255 will be
escaped (`latin1`) or copied as is (`utf8`).

_See also: _`atom/1`, `atom_literal/1`, `string/1`.
""".
-spec atom_literal(syntaxTree(), utf8 | unicode | latin1) -> string().

atom_literal(Node, utf8) ->
    io_lib:write_atom(atom_value(Node));
atom_literal(Node, unicode) ->
    io_lib:write_atom(atom_value(Node));
atom_literal(Node, latin1) ->
    io_lib:write_atom_as_latin1(atom_value(Node)).

%% =====================================================================

-doc #{equiv => map_expr(none, Fields)}.
-spec map_expr([syntaxTree()]) -> syntaxTree().

map_expr(Fields) ->
    map_expr(none, Fields).


-record(map_expr, {argument :: 'none' | syntaxTree(),
                   fields   :: [syntaxTree()]}).

-doc """
Creates an abstract map expression.

If `Fields` is `[F1, ..., Fn]`, then if `Argument` is `none`, the
result represents "`#{F1, ..., Fn}`", otherwise it represents
"`Argument#{F1, ..., Fn}`".

_See also: _`map_expr/1`, `map_expr_argument/1`, `map_expr_fields/1`,
`map_field_assoc/2`, `map_field_exact/2`.
""".
-spec map_expr('none' | syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {map, Pos, Fields}
%% {map, Pos, Argument, Fields}

map_expr(Argument, Fields) ->
    tree(map_expr, #map_expr{argument = Argument, fields = Fields}).

revert_map_expr(Node) ->
    Pos = get_pos(Node),
    Argument = map_expr_argument(Node),
    Fields = map_expr_fields(Node),
    case Argument of
        none ->
            {map, Pos, Fields};
        _ ->
            {map, Pos, Argument, Fields}
    end.


-doc """
Returns the argument subtree of a `map_expr` node, if any.

If `Node` represents "`#{...}`", `none` is returned. Otherwise, if
`Node` represents "`Argument#{...}`", `Argument` is returned.

_See also: _`map_expr/2`.
""".
-spec map_expr_argument(syntaxTree()) -> 'none' | syntaxTree().

map_expr_argument(Node) ->
    case unwrap(Node) of
        {map, _, _} ->
            none;
        {map, _, Argument, _} ->
            Argument;
        Node1 ->
            (data(Node1))#map_expr.argument
    end.


-doc """
Returns the list of field subtrees of a `map_expr` node.

_See also: _`map_expr/2`.
""".
-spec map_expr_fields(syntaxTree()) -> [syntaxTree()].

map_expr_fields(Node) ->
    case unwrap(Node) of
        {map, _, Fields} ->
            Fields;
        {map, _, _, Fields} ->
            Fields;
        Node1 ->
            (data(Node1))#map_expr.fields
    end.


-record(map_field_assoc, {name :: syntaxTree(), value :: syntaxTree()}).

-doc """
Creates an abstract map assoc field.

The result represents "`Name => Value`".

_See also: _`map_expr/2`, `map_field_assoc_name/1`, `map_field_assoc_value/1`.
""".
-spec map_field_assoc(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {map_field_assoc, Pos, Name, Value}

map_field_assoc(Name, Value) ->
    tree(map_field_assoc, #map_field_assoc{name = Name, value = Value}).

revert_map_field_assoc(Node) ->
    Pos = get_pos(Node),
    Name = map_field_assoc_name(Node),
    Value = map_field_assoc_value(Node),
    {map_field_assoc, Pos, Name, Value}.


-doc """
Returns the name subtree of a `map_field_assoc` node.

_See also: _`map_field_assoc/2`.
""".
-spec map_field_assoc_name(syntaxTree()) -> syntaxTree().

map_field_assoc_name(Node) ->
    case unwrap(Node) of
        {map_field_assoc, _, Name, _} ->
            Name;
        Node1 ->
            (data(Node1))#map_field_assoc.name
    end.


-doc """
Returns the value subtree of a `map_field_assoc` node.

_See also: _`map_field_assoc/2`.
""".
-spec map_field_assoc_value(syntaxTree()) -> syntaxTree().

map_field_assoc_value(Node) ->
    case unwrap(Node) of
        {map_field_assoc, _, _, Value} ->
            Value;
        Node1 ->
            (data(Node1))#map_field_assoc.value
    end.


-record(map_field_exact, {name :: syntaxTree(), value :: syntaxTree()}).

-doc """
Creates an abstract map exact field.

The result represents "`Name := Value`".

_See also: _`map_expr/2`, `map_field_exact_name/1`, `map_field_exact_value/1`.
""".
-spec map_field_exact(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {map_field_exact, Pos, Name, Value}

map_field_exact(Name, Value) ->
    tree(map_field_exact, #map_field_exact{name = Name, value = Value}).

revert_map_field_exact(Node) ->
    Pos = get_pos(Node),
    Name = map_field_exact_name(Node),
    Value = map_field_exact_value(Node),
    {map_field_exact, Pos, Name, Value}.


-doc """
Returns the name subtree of a `map_field_exact` node.

_See also: _`map_field_exact/2`.
""".
-spec map_field_exact_name(syntaxTree()) -> syntaxTree().

map_field_exact_name(Node) ->
    case unwrap(Node) of
        {map_field_exact, _, Name, _} ->
            Name;
        Node1 ->
            (data(Node1))#map_field_exact.name
    end.


-doc """
Returns the value subtree of a `map_field_exact` node.

_See also: _`map_field_exact/2`.
""".
-spec map_field_exact_value(syntaxTree()) -> syntaxTree().

map_field_exact_value(Node) ->
    case unwrap(Node) of
        {map_field_exact, _, _, Value} ->
            Value;
        Node1 ->
            (data(Node1))#map_field_exact.value
    end.


-doc """
Creates an abstract tuple.

If `Elements` is `[X1, ..., Xn]`, the result represents "`{X1, ...,
Xn}`".

> #### Note {: .info }
>
> The Erlang language has distinct 1-tuples, meaning `{X}` is always distinct
> from `X` itself.

_See also: _`tuple_elements/1`, `tuple_size/1`.
""".
-spec tuple([syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {tuple, Pos, Elements}
%%
%%	Elements = [erl_parse()]

tuple(List) ->
    tree(tuple, List).

revert_tuple(Node) ->
    Pos = get_pos(Node),
    {tuple, Pos, tuple_elements(Node)}.


-doc """
Returns the list of element subtrees of a `tuple` node.

_See also: _`tuple/1`.
""".
-spec tuple_elements(syntaxTree()) -> [syntaxTree()].

tuple_elements(Node) ->
    case unwrap(Node) of
	{tuple, _, List} ->
	    List;
	Node1 ->
	    data(Node1)
    end.


-doc """
Returns the number of elements of a `tuple` node.

> #### Note {: .info }
>
> This is equivalent to [`length(tuple_elements(Node))`](`length/1`),
> but potentially more efficient.

_See also: _`tuple/1`, `tuple_elements/1`.
""".
-spec tuple_size(syntaxTree()) -> non_neg_integer().

tuple_size(Node) ->
    length(tuple_elements(Node)).


%% =====================================================================
%% @equiv list(List, none)

-doc #{equiv => list(List, none)}.
-spec list([syntaxTree()]) -> syntaxTree().

list(List) ->
    list(List, none).


-record(list, {prefix :: [syntaxTree()], suffix :: 'none' | syntaxTree()}).

-doc """
list(List, Tail)

Constructs an abstract list skeleton.

The result has type `list` or `nil`. If `List` is a nonempty list
`[E1, ..., En]`, the result has type `list` and represents either
"`[E1, ..., En]`" if `Tail` is `none`, or otherwise "`[E1, ...,
En | Tail]`". If `List` is the empty list, `Tail` _must_ be `none`,
and in that case the result has type `nil` and represents "`[]`" (see
`nil/0`).

The difference between lists as semantic objects (built up of individual "cons"
and "nil" terms) and the various syntactic forms for denoting lists may be
bewildering at first. This module provides functions both for exact control of
the syntactic representation as well as for the simple composition and
deconstruction in terms of cons and head/tail operations.

> #### Note {: .info }
>
> In [`list(Elements, none)`](`list/2`), the "nil" list terminator is
> implicit and has no associated information (see `get_attrs/1`). However,
> in the seemingly equivalent [`list(Elements, Tail)`](`list/2`) where
> `Tail` has the type `nil`, the list terminator subtree `Tail` may have
> attached attributes such as position, comments, and annotations, which
> will be preserved in the result.

_See also: _`compact_list/1`, `cons/2`, `get_attrs/1`, `is_list_skeleton/1`,
`is_proper_list/1`, `list/1`, `list_elements/1`, `list_head/1`, `list_length/1`,
`list_prefix/1`, `list_suffix/1`, `list_tail/1`, `nil/0`, `normalize_list/1`.
""".
-spec list([syntaxTree()], 'none' | syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {cons, Pos, Head, Tail}
%%
%%	Head = Tail = [erl_parse()]
%%
%%	This represents `[<Head> | <Tail>]', or more generally `[<Head>
%%	<Suffix>]' where the form of <Suffix> can depend on the
%%	structure of <Tail>; there is no fixed printed form.

list([], none) ->
    nil();
list(Elements, Tail) when Elements =/= [] ->
    tree(list, #list{prefix = Elements, suffix = Tail}).

revert_list(Node) ->
    Pos = get_pos(Node),
    Prefix = list_prefix(Node),
    Suffix =
        case list_suffix(Node) of
	    none ->
                %% there is no explicit `| Tail]` part, just a plain list
                %% `[X1,...XN]`, so we must invent a nil node
                case erl_anno:end_location(Pos) of
                    undefined ->
                        LastPos = get_pos(lists:last(Prefix)),
                        case erl_anno:end_location(LastPos) of
                            undefined ->
                                %% use a zero location rather than a wrong one
                                {nil, erl_anno:new(0)};
                            EndLoc ->
                                %% if the last element has an end location,
                                %% we take that as both start and end
                                {nil, erl_anno:set_end_location(EndLoc, erl_anno:new(EndLoc))}
                        end;
                    EndLoc ->
                        %% if the whole list node has an end location, we
                        %% take that as both start and end of the nil
                        {nil, erl_anno:set_end_location(EndLoc, erl_anno:new(EndLoc))}
                end;
	    Suffix1 ->
                Suffix1
	end,
    F = fun (Head, Tail) ->
                %% the nested conses get the location from the list
                %% elements, but other annotations must not be copied
                HeadLoc = erl_anno:location(get_pos(Head)),
                {cons, erl_anno:new(HeadLoc), Head, Tail}
        end,
    %% the outermost cons gets the full annotations of the list
    setelement(2, lists:foldr(F, Suffix, Prefix), Pos).


-doc """
Creates an abstract empty list.

The result represents "`[]`". The empty list is traditionally called
"nil".

_See also: _`is_list_skeleton/1`, `list/2`.
""".
-spec nil() -> syntaxTree().

%% `erl_parse' representation:
%%
%% {nil, Pos}

nil() ->
    tree(nil).

revert_nil(Node) ->
    Pos = get_pos(Node),
    {nil, Pos}.


-doc """
Returns the prefix element subtrees of a `list` node.

If `Node` represents "`[E1, ..., En]`" or "`[E1, ..., En |
Tail]`", the returned value is `[E1, ..., En]`.

_See also: _`list/2`.
""".
-spec list_prefix(syntaxTree()) -> [syntaxTree()].

list_prefix(Node) ->
    case unwrap(Node) of
	{cons, _, Head, Tail} ->
	    [Head | cons_prefix(Tail)];
	Node1 ->
	    (data(Node1))#list.prefix
    end.

%% collects sequences of conses; cf. cons_suffix/1 below
cons_prefix({cons, _, Head, Tail}) ->
    [Head | cons_prefix(Tail)];
cons_prefix(_) ->
    [].


-doc """
Returns the suffix subtree of a `list` node, if one exists.

If `Node` represents "`[E1, ..., En | Tail]`", the returned value is
`Tail`. Otherwise, if `Node` represents "`[E1, ..., En]`", `none` is
returned.

> #### Note {: .info }
>
> Even if this function returns a `Tail` that is not `none`, the type
> of `Tail` can be `nil` if the tail has been given explicitly and the
> list skeleton has not been compacted (see `compact_list/1`).

_See also: _`compact_list/1`, `list/2`, `nil/0`.
""".
-spec list_suffix(syntaxTree()) -> 'none' | syntaxTree().

list_suffix(Node) ->
    case unwrap(Node) of
	{cons, _, _, Tail} ->
	    case cons_suffix(Tail) of
		{nil, _} ->
		    none;
		Tail1 ->
		    Tail1
	    end;
	Node1 ->
	    (data(Node1))#list.suffix
    end.

%% skips sequences of conses; cf. cons_prefix/1 above
cons_suffix({cons, _, _, Tail}) ->
    cons_suffix(Tail);
cons_suffix(Tail) ->
    Tail.


-doc """
"Optimizing" list skeleton cons operation.

Creates an abstract list skeleton whose first element is `Head` and
whose tail corresponds to `Tail`. This is similar to [`list([Head],
Tail)`](`list/2`), except that `Tail` must not be `none`,
and the result does not necessarily represent exactly "`[Head | Tail]`", but
may depend on the `Tail` subtree.

For example, if `Tail` represents `[X, Y]`, the result may represent
"`[Head, X, Y]`", rather than "`[Head | [X, Y]]`". Annotations on
`Tail` itself may be lost if `Tail` represents a list skeleton, but
comments on `Tail` are propagated to the result.

_See also: _`list/2`, `list_head/1`, `list_tail/1`.
""".
-spec cons(syntaxTree(), syntaxTree()) -> syntaxTree().

cons(Head, Tail) ->
    case type(Tail) of
	list ->
	    copy_comments(Tail, list([Head | list_prefix(Tail)],
				     list_suffix(Tail)));
	nil ->
	    copy_comments(Tail, list([Head]));
	_ ->
	    list([Head], Tail)
    end.


-doc """
Returns the head element subtree of a `list` node.

If `Node` represents "`[Head ...]`", the result will represent "`Head`".

_See also: _`cons/2`, `list/2`, `list_tail/1`.
""".
-spec list_head(syntaxTree()) -> syntaxTree().

list_head(Node) ->
    hd(list_prefix(Node)).


-doc """
Returns the tail of a `list` node.

If `Node` represents a single-element list "`[E]`", then the result
has type `nil`, representing "`[]`". If `Node` represents "`[E1,
E2 ...]`", the result will represent "`[E2 ...]`", and if `Node`
represents "`[Head | Tail]`", the result will represent
"`Tail`".

_See also: _`cons/2`, `list/2`, `list_head/1`.
""".
-spec list_tail(syntaxTree()) -> syntaxTree().

list_tail(Node) ->
    Tail = list_suffix(Node),
    case tl(list_prefix(Node)) of
	[] ->
	    if Tail =:= none ->
		    nil();    % implicit list terminator.
	       true ->
		    Tail
	    end;
	Es ->
	    list(Es, Tail)    % `Es' is nonempty.
    end.


-doc """
Returns `true` if `Node` has type `list` or `nil`, otherwise `false`.

_See also: _`list/2`, `nil/0`.
""".
-spec is_list_skeleton(syntaxTree()) -> boolean().

is_list_skeleton(Node) ->
    case type(Node) of
	list -> true;
	nil -> true;
	_ -> false
    end.


-doc """
Returns `true` if `Node` represents a proper list, and `false` otherwise.

A proper list is a list skeleton either of the form "`[]`" or "`[E1,
..., En]`", or "`[... | Tail]`" where recursively `Tail` also
represents a proper list.

> #### Note {: .info }
>
> Since `Node` is a syntax tree, the actual run-time values
> corresponding to its subtrees can often be partially or completely
> unknown. For example, if `Node` represents "`[... | Ns]`"
> (where `Ns` is a variable), the function will return `false`
> because it is not known whether `Ns` will be bound to a list at
> run-time. Conversely, if `Node` represents, for example, "`[1, 2, 3]`" or
> "`[A | []]`", the function will return `true`.

_See also: _`list/2`.
""".
-spec is_proper_list(syntaxTree()) -> boolean().

is_proper_list(Node) ->
    case type(Node) of
	list ->
	    case list_suffix(Node) of
		none ->
		    true;
		Tail ->
		    is_proper_list(Tail)
	    end;
	nil ->
	    true;
	_ ->
	    false
    end.


-doc """
Returns the list of element subtrees of a list skeleton.

`Node` must represent a proper list. For example, if `Node` represents
"`[X1, X2 | [X3, X4 | []]`", then
[`list_elements(Node)`](`list_elements/1`) yields the list `[X1, X2,
X3, X4]`.

_See also: _`is_proper_list/1`, `list/2`.
""".
-spec list_elements(syntaxTree()) -> [syntaxTree()].

list_elements(Node) ->
    lists:reverse(list_elements(Node, [])).

list_elements(Node, As) ->
    case type(Node) of
	list ->
	    As1 = lists:reverse(list_prefix(Node)) ++ As,
	    case list_suffix(Node) of
		none ->
		    As1;
		Tail ->
		    list_elements(Tail, As1)
	    end;
	nil ->
	    As
    end.


-doc """
Returns the number of element subtrees of a list skeleton.

`Node` must represent a proper list. For example, if `Node` represents
"`[X1 | [X2, X3 | [X4, X5, X6]]]`", then
[`list_length(Node)`](`list_length/1`) returns the integer 6.

> #### Note {: .info }
>
> This is equivalent to [`length(list_elements(Node))`](`length/1`), but
> potentially more efficient.

_See also: _`is_proper_list/1`, `list/2`, `list_elements/1`.
""".
-spec list_length(syntaxTree()) -> non_neg_integer().

list_length(Node) ->
    list_length(Node, 0).

list_length(Node, A) ->
    case type(Node) of
	list ->
	    A1 = length(list_prefix(Node)) + A,
	    case list_suffix(Node) of
		none ->
		    A1;
		Tail ->
		    list_length(Tail, A1)
	    end;
	nil ->
	    A
    end.


-doc """
Expands an abstract list skeleton to its most explicit form.

If `Node` represents "`[E1, ..., En | Tail]`", the result
represents "`[E1 | ... [En | Tail1] ... ]`", where `Tail1` is
the result of [`normalize_list(Tail)`](`normalize_list/1`). If `Node`
represents "`[E1, ..., En]`", the result simply represents "`[E1
| ... [En | []] ... ]`". If `Node` does not represent a list
skeleton, `Node` itself is returned.

_See also: _`compact_list/1`, `list/2`.
""".
-spec normalize_list(syntaxTree()) -> syntaxTree().

normalize_list(Node) ->
    case type(Node) of
	list ->
	    P = list_prefix(Node),
	    case list_suffix(Node) of
		none ->
		    copy_attrs(Node, normalize_list_1(P, nil()));
		Tail ->
		    Tail1 = normalize_list(Tail),
		    copy_attrs(Node, normalize_list_1(P, Tail1))
	    end;
	_ ->
	    Node
    end.

normalize_list_1(Es, Tail) ->
    lists:foldr(fun (X, A) ->
			list([X], A)    % not `cons'!
		end,
		Tail, Es).


-doc """
Yields the most compact form for an abstract list skeleton.

The result either represents "`[E1, ..., En | Tail]`", where
`Tail` is not a list skeleton, or otherwise simply "`[E1, ...,
En]`". Annotations on subtrees of `Node` that represent list
skeletons may be lost, but comments will be propagated to the
result. Returns `Node` itself if `Node` does not represent a list
skeleton.

_See also: _`list/2`, `normalize_list/1`.
""".
-spec compact_list(syntaxTree()) -> syntaxTree().

compact_list(Node) ->
    case type(Node) of
	list ->
	    case list_suffix(Node) of
		none ->
		    Node;
		Tail ->
		    case type(Tail) of
			list ->
			    Tail1 = compact_list(Tail),
			    Node1 = list(list_prefix(Node) ++
					 list_prefix(Tail1),
					 list_suffix(Tail1)),
			    join_comments(Tail1,
					  copy_attrs(Node,
						     Node1));
			nil ->
			    Node1 = list(list_prefix(Node)),
			    join_comments(Tail,
					  copy_attrs(Node,
						     Node1));
			_ ->
			    Node
		    end
	    end;
	_ ->
	    Node
    end.


-doc """
Creates an abstract binary-object template.

If `Fields` is `[F1, ..., Fn]`, the result represents "`<<F1, ...,
Fn>>`".

_See also: _`binary_field/2`, `binary_fields/1`.
""".
-spec binary([syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {bin, Pos, Fields}
%%
%%	Fields = [Field]
%%	Field = {bin_element, ...}
%%
%%	See `binary_field' for documentation on `erl_parse' binary
%%	fields (or "elements").

binary(List) ->
    tree(binary, List).

revert_binary(Node) ->
    Pos = get_pos(Node),
    {bin, Pos, binary_fields(Node)}.


-doc """
Returns the list of field subtrees of a `binary` node.

_See also: _`binary/1`, `binary_field/2`.
""".
-spec binary_fields(syntaxTree()) -> [syntaxTree()].

binary_fields(Node) ->
    case unwrap(Node) of
	{bin, _, List} ->
	    List;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================

-doc #{equiv => binary_field(Body, [])}.
-spec binary_field(syntaxTree()) -> syntaxTree().

binary_field(Body) ->
    binary_field(Body, []).


-doc """
binary_field(Body, Size, Types)

Creates an abstract binary template field.

If `Size` is `none`, this is equivalent to "[`binary_field(Body,
Types)`](`binary_field/2`)", otherwise it is equivalent to
"[`binary_field(size_qualifier(Body, Size),
Types)`](`binary_field/2`)".

(This is a utility function.)

_See also: _`binary/1`, `binary_field/2`, `size_qualifier/2`.
""".
-spec binary_field(syntaxTree(), 'none' | syntaxTree(), [syntaxTree()]) ->
        syntaxTree().

binary_field(Body, none, Types) ->
    binary_field(Body, Types);
binary_field(Body, Size, Types) ->
    binary_field(size_qualifier(Body, Size), Types).


-record(binary_field, {body :: syntaxTree(), types :: [syntaxTree()]}).

-doc """
Creates an abstract binary template field.

If `Types` is the empty list, the result simply represents
"`Body`", otherwise, if `Types` is `[T1, ..., Tn]`, the result
represents "`Body/T1-...-Tn`".

_See also: _`binary/1`, `binary_field/1`, `binary_field/3`,
`binary_field_body/1`, `binary_field_size/1`, `binary_field_types/1`.
""".
-spec binary_field(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {bin_element, Pos, Expr, Size, TypeList}
%%
%%	Expr = erl_parse()
%%	Size = default | erl_parse()
%%	TypeList = default | [Type] \ []
%%	Type = atom() | {atom(), integer()}

binary_field(Body, Types) ->
    tree(binary_field, #binary_field{body = Body, types = Types}).

revert_binary_field(Node) ->
    Pos = get_pos(Node),
    Body = binary_field_body(Node),
    {Expr, Size} = case type(Body) of
		       size_qualifier ->
			   %% Note that size qualifiers are not
			   %% revertible out of context.
			   {size_qualifier_body(Body),
			    size_qualifier_argument(Body)};
		       _ ->
			   {Body, default}
		   end,
    Types = case binary_field_types(Node) of
		[] ->
		    default;
		Ts ->
		    fold_binary_field_types(Ts)
	    end,
    {bin_element, Pos, Expr, Size, Types}.


-doc """
Returns the body subtree of a `binary_field`.

_See also: _`binary_field/2`.
""".
-spec binary_field_body(syntaxTree()) -> syntaxTree().

binary_field_body(Node) ->
    case unwrap(Node) of
	{bin_element, _, Body, Size, _} ->
	    if Size =:= default ->
		    Body;
	       true ->
		    size_qualifier(Body, Size)
	    end;
	Node1 ->
	    (data(Node1))#binary_field.body
    end.


-doc """
Returns the list of type-specifier subtrees of a `binary_field` node.

If `Node` represents "`.../T1, ..., Tn`", the result is `[T1,
..., Tn]`, otherwise the result is the empty list.

_See also: _`binary_field/2`.
""".
-spec binary_field_types(syntaxTree()) -> [syntaxTree()].

binary_field_types(Node) ->
    case unwrap(Node) of
	{bin_element, Pos, _, _, Types} ->
	    if Types =:= default ->
		    [];
	       true ->
		    unfold_binary_field_types(Types, Pos)
	    end;
	Node1 ->
	    (data(Node1))#binary_field.types
    end.


-doc """
Returns the size specifier subtree of a `binary_field` node, if any.

If `Node` represents "`Body:Size`" or "`Body:Size/T1, ...,
Tn`", the result is `Size`, otherwise `none` is returned.

(This is a utility function.)

_See also: _`binary_field/2`, `binary_field/3`.
""".
-spec binary_field_size(syntaxTree()) -> 'none' | syntaxTree().

binary_field_size(Node) ->
    case unwrap(Node) of
	{bin_element, _, _, Size, _} ->
	    if Size =:= default ->
		    none;
	       true ->
		    Size
	    end;
	Node1 ->
	    Body = (data(Node1))#binary_field.body,
	    case type(Body) of
		size_qualifier ->
		    size_qualifier_argument(Body);
		_ ->
		    none
	    end
    end.


-record(size_qualifier, {body :: syntaxTree(), size :: syntaxTree()}).

-doc """
Creates an abstract size qualifier.

The result represents "`Body:Size`".

_See also: _`size_qualifier_argument/1`, `size_qualifier_body/1`.
""".
-spec size_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().

size_qualifier(Body, Size) ->
    tree(size_qualifier,
	 #size_qualifier{body = Body, size = Size}).


-doc """
Returns the body subtree of a `size_qualifier` node.

_See also: _`size_qualifier/2`.
""".
-spec size_qualifier_body(syntaxTree()) -> syntaxTree().

size_qualifier_body(Node) ->
    (data(Node))#size_qualifier.body.


-doc """
Returns the argument subtree (the size) of a `size_qualifier` node.

_See also: _`size_qualifier/2`.
""".
-spec size_qualifier_argument(syntaxTree()) -> syntaxTree().

size_qualifier_argument(Node) ->
    (data(Node))#size_qualifier.size.


-doc """
Creates an abstract error marker.

The result represents an occurrence of an error in the source code,
with an associated Erlang I/O ErrorInfo structure given by `Error`
(see module [`//stdlib/io`](`m:io`) for details). Error markers are
regarded as source code forms, but have no defined lexical form.

> #### Note {: .info }
>
> This is supported only for backwards compatibility with existing parsers
> and tools.

_See also: _`eof_marker/0`, `error_marker_info/1`, `is_form/1`,
`warning_marker/1`.
""".
-spec error_marker(term()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {error, Error}
%%
%%	Error = term()
%%
%%	Note that there is no position information for the node
%%	itself: `get_pos' and `set_pos' handle this as a special case.

error_marker(Error) ->
    tree(error_marker, Error).

revert_error_marker(Node) ->
    %% Note that the position information of the node itself is not
    %% preserved.
    {error, error_marker_info(Node)}.


-doc """
Returns the ErrorInfo structure of an `error_marker` node.

_See also: _`error_marker/1`.
""".
-spec error_marker_info(syntaxTree()) -> term().

error_marker_info(Node) ->
    case unwrap(Node) of
	{error, Error} ->
	    Error;
	T ->
	    data(T)
    end.


-doc """
Creates an abstract warning marker.

The result represents an occurrence of a possible problem in the
source code, with an associated Erlang I/O ErrorInfo structure given
by `Error` (see module [`//stdlib/io`](`m:io`) for details). Warning
markers are regarded as source code forms, but have no defined lexical
form.

> #### Note {: .info }
>
> This is supported only for backwards compatibility with existing parsers
> and tools.

_See also: _`eof_marker/0`, `error_marker/1`, `is_form/1`,
`warning_marker_info/1`.
""".
-spec warning_marker(term()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {warning, Error}
%%
%%	Error = term()
%%
%%	Note that there is no position information for the node
%%	itself: `get_pos' and `set_pos' handle this as a special case.

warning_marker(Warning) ->
    tree(warning_marker, Warning).

revert_warning_marker(Node) ->
    %% Note that the position information of the node itself is not
    %% preserved.
    {warning, warning_marker_info(Node)}.


-doc """
Returns the ErrorInfo structure of a `warning_marker` node.

_See also: _`warning_marker/1`.
""".
-spec warning_marker_info(syntaxTree()) -> term().

warning_marker_info(Node) ->
    case unwrap(Node) of
	{warning, Error} ->
	    Error;
	T ->
	    data(T)
    end.


-doc """
Creates an abstract end-of-file marker.

This represents the end of input when reading a sequence of source
code forms. An end-of-file marker is itself regarded as a source code
form (namely, the last in any sequence in which it occurs). It has no
defined lexical form.

> #### Note {: .info }
>
> This is retained only for backwards compatibility with existing parsers
> and tools.

_See also: _`error_marker/1`, `is_form/1`, `warning_marker/1`.
""".
-spec eof_marker() -> syntaxTree().

%% `erl_parse' representation:
%%
%% {eof, Pos}

eof_marker() ->
    tree(eof_marker).

revert_eof_marker(Node) ->
    Pos = get_pos(Node),
    {eof, Pos}.


%% =====================================================================
%% @equiv attribute(Name, none)

-doc #{equiv => attribute(Name, none)}.
-spec attribute(syntaxTree()) -> syntaxTree().

attribute(Name) ->
    attribute(Name, none).


-record(attribute, {name :: syntaxTree(), args :: 'none' | [syntaxTree()]}).

-doc """
Creates an abstract program attribute.

If `Arguments` is `[A1, ..., An]`, the result represents
"`-Name(A1, ..., An).`". Otherwise, if `Arguments` is `none`,
the result represents "`-Name.`". The latter form makes it possible
to represent preprocessor directives such as "`-endif.`". Attributes
are source code forms.

> #### Note {: .info }
>
> The preprocessor macro definition directive "`-define(Name, Body).`"
> has relatively few requirements on the syntactical form of `Body`
> (viewed as a sequence of tokens). The `text` node type can be used for
> a `Body` that is not a normal Erlang construct.

_See also: _`attribute/1`, `attribute_arguments/1`, `attribute_name/1`,
`is_form/1`, `text/1`.
""".
-spec attribute(syntaxTree(), 'none' | [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {attribute, Pos, module, {Name,Vars}}
%% {attribute, Pos, module, Name}
%%
%%	Name = atom() | [atom()]
%%	Vars = [atom()]
%%
%%	Representing `-module(M).', or `-module(M, Vs).', where M is
%%	`A1.A2.....An' if Name is `[A1, A2, ..., An]', and Vs is `[V1,
%%	..., Vm]' if Vars is `[V1, ..., Vm]'.
%%
%% {attribute, Pos, export, Exports}
%%
%%	Exports = [{atom(), integer()}]
%%
%%	Representing `-export([A1/N1, ..., Ak/Nk]).', if `Exports' is
%%	`[{A1, N1}, ..., {Ak, Nk}]'.
%%
%% {attribute, Pos, import, Imports}
%%
%%	Imports = {atom(), Pairs} | [atom()]
%%	Pairs = [{atom(), integer()]
%%
%%	Representing `-import(Module, [A1/N1, ..., Ak/Nk]).', if
%%	`Imports' is `{Module, [{A1, N1}, ..., {Ak, Nk}]}', or
%%	`-import(A1.....An).', if `Imports' is `[A1, ..., An]'.
%%
%% {attribute, Pos, export_type, ExportedTypes}
%%
%%	ExportedTypes = [{atom(), integer()}]
%%
%%	Representing `-export_type([N1/A1, ..., Nk/Ak]).',
%%      if `ExportedTypes' is `[{N1, A1}, ..., {Nk, Ak}]'.
%%
%% {attribute, Pos, optional_callbacks, OptionalCallbacks}
%%
%%	OptionalCallbacks = [{atom(), integer()}]
%%
%%	Representing `-optional_callbacks([A1/N1, ..., Ak/Nk]).',
%%      if `OptionalCallbacks' is `[{A1, N1}, ..., {Ak, Nk}]'.
%%
%% {attribute, Pos, SpecTag, {FuncSpec, FuncType}}
%%
%%      SpecTag = spec | callback
%%	FuncSpec = {module(), atom(), arity()} | {atom(), arity()}
%%      FuncType = a (possibly constrained) function type
%%
%%	Representing `-SpecTag M:F/A Ft1; ...; Ftk.' or
%%      `-SpecTag F/A Ft1; ...; Ftk.', if `FuncTypes' is
%%      `[Ft1, ..., Ftk]'.
%%
%% {attribute, Pos, TypeTag, {Name, Type, Parameters}}
%%
%%      TypeTag = type | opaque
%%      Type = a type
%%      Parameters = [Variable]
%%
%%	Representing `-TypeTag Name(V1, ..., Vk) :: Type .'
%%      if `Parameters' is `[V1, ..., Vk]'.
%%
%% {attribute, Pos, file, Position}
%%
%%	Position = {filename(), integer()}
%%
%%	Representing `-file(Name, Line).', if `Position' is `{Name,
%%	Line}'.
%%
%% {attribute, Pos, record, Info}
%%
%%	Info = {Name, [Entries]}
%%	Name = atom()
%%
%%	Entries = UntypedEntries
%%              | {typed_record_field, UntypedEntries, Type}
%%      UntypedEntries = {record_field, Pos, atom()}
%%                     | {record_field, Pos, atom(), erl_parse()}
%%
%%      Representing `-record(Name, {<F1>, ..., <Fn>}).', if `Info' is
%%	`{Name, [D1, ..., D1]}', where each `Fi' is either `Ai = <Ei>',
%%	if the corresponding `Di' is `{record_field, Pos, Ai, Ei}', or
%%	otherwise simply `Ai', if `Di' is `{record_field, Pos, Ai}', or
%%      `Ai = <Ei> :: <Ti>', if `Di' is `{typed_record_field,
%%      {record_field, Pos, Ai, Ei}, Ti}', or `Ai :: <Ti>', if `Di' is
%%      `{typed_record_field, {record_field, Pos, Ai}, Ti}'.
%%
%% {attribute, L, Name, Term}
%%
%%	Name = atom() \ StandardName
%%	StandardName = module | export | import | file | record
%%	Term = term()
%%
%%	Representing `-Name(Term).'.

attribute(Name, Args) ->
    tree(attribute, #attribute{name = Name, args = Args}).

revert_attribute(Node) ->
    Name = attribute_name(Node),
    Args = attribute_arguments(Node),
    Pos = get_pos(Node),
    case type(Name) of
	atom ->
	    revert_attribute_1(atom_value(Name), Args, Pos, Node);
	_ ->
	    Node
    end.

%% All the checking makes this part a bit messy:

revert_attribute_1(module, [M], Pos, Node) ->
    case revert_module_name(M) of
	{ok, A} ->
	    {attribute, Pos, module, A};
	error -> Node
    end;
revert_attribute_1(module, [M, List], Pos, Node) ->
    Vs = case is_list_skeleton(List) of
	     true ->
		 case is_proper_list(List) of
		     true ->
			 fold_variable_names(list_elements(List));
		     false ->
			 Node
		 end;
	     false ->
		 Node
	 end,
    case revert_module_name(M) of
	{ok, A} ->
	    {attribute, Pos, module, {A, Vs}};
	error -> Node
    end;
revert_attribute_1(export, [List], Pos, Node) ->
    case is_list_skeleton(List) of
	true ->
	    case is_proper_list(List) of
		true ->
		    Fs = fold_function_names(list_elements(List)),
		    {attribute, Pos, export, Fs};
		false ->
		    Node
	    end;
	false ->
	    Node
    end;
revert_attribute_1(import, [M], Pos, Node) ->
    case revert_module_name(M) of
	{ok, A} -> {attribute, Pos, import, A};
	error -> Node
    end;
revert_attribute_1(import, [M, List], Pos, Node) ->
    case revert_module_name(M) of
	{ok, A} ->
	    case is_list_skeleton(List) of
		true ->
		    case is_proper_list(List) of
			true ->
			    Fs = fold_function_names(
				   list_elements(List)),
			    {attribute, Pos, import, {A, Fs}};
			false ->
			    Node
		    end;
		false ->
		    Node
	    end;
	error ->
	    Node
    end;
revert_attribute_1(file, [A, Line], Pos, Node) ->
    case type(A) of
	string ->
	    case type(Line) of
		integer ->
		    {attribute, Pos, file,
		     {concrete(A), concrete(Line)}};
		_ ->
		    Node
	    end;
	_ ->
	    Node
    end;
revert_attribute_1(record, [A, Tuple], Pos, Node) ->
    case type(A) of
	atom ->
	    case type(Tuple) of
		tuple ->
		    Fs = fold_record_fields(
			   tuple_elements(Tuple)),
		    {attribute, Pos, record, {concrete(A), Fs}};
		_ ->
		    Node
	    end;
	_ ->
	    Node
    end;
revert_attribute_1(N, [T], Pos, _) ->
    {attribute, Pos, N, concrete(T)};
revert_attribute_1(_, _, _, Node) ->
    Node.

revert_module_name(A) ->
    case type(A) of
	atom ->
	    {ok, concrete(A)};
	_ ->
	    error
    end.


-doc """
Returns the name subtree of an `attribute` node.

_See also: _`attribute/1`.
""".
-spec attribute_name(syntaxTree()) -> syntaxTree().

attribute_name(Node) ->
    case unwrap(Node) of
	{attribute, Pos, Name, _} ->
	    set_pos(atom(Name), Pos);
	Node1 ->
	    (data(Node1))#attribute.name
    end.


-doc """
Returns the list of argument subtrees of an `attribute` node, if any.

If `Node` represents "`-Name.`", the result is `none`. Otherwise, if
`Node` represents "`-Name(E1, ..., En).`", `[E1, ..., E1]` is
returned.

_See also: _`attribute/1`.
""".
-spec attribute_arguments(syntaxTree()) -> none | [syntaxTree()].

attribute_arguments(Node) ->
    case unwrap(Node) of
	{attribute, Pos, Name, Data} ->
	    case Name of
		module ->
		    {M1, Vs} =
			case Data of
			    {M0, Vs0} ->
				{M0, unfold_variable_names(Vs0, Pos)};
			    M0 ->
				{M0, none}
			end,
		    M2 = atom(M1),
		    M = set_pos(M2, Pos),
		    if Vs == none -> [M];
		       true -> [M, set_pos(list(Vs), Pos)]
		    end;
		export ->
		    [set_pos(
		       list(unfold_function_names(Data, Pos)),
		       Pos)];
		import ->
		    {Module, Imports} = Data,
		    [set_pos(atom(Module), Pos),
		     set_pos(
		       list(unfold_function_names(Imports, Pos)),
		       Pos)];
		file ->
		    {File, Line} = Data,
		    [set_pos(string(File), Pos),
		     set_pos(integer(Line), Pos)];
		record ->
		    %% Note that we create a tuple as container
		    %% for the second argument!
		    {Type, Entries} = Data,
		    [set_pos(atom(Type), Pos),
		     set_pos(tuple(unfold_record_fields(Entries)),
			     Pos)];
		_ ->
		    %% Standard single-term generic attribute.
		    [set_pos(abstract(Data), Pos)]
	    end;
	Node1 ->
	    (data(Node1))#attribute.args
    end.


-record(arity_qualifier, {body :: syntaxTree(), arity :: syntaxTree()}).

-doc """
Creates an abstract arity qualifier.

The result represents "`Body/Arity`".

_See also: _`arity_qualifier_argument/1`, `arity_qualifier_body/1`.
""".
-spec arity_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().

arity_qualifier(Body, Arity) ->
    tree(arity_qualifier,
	 #arity_qualifier{body = Body, arity = Arity}).


-doc """
Returns the body subtree of an `arity_qualifier` node.

_See also: _`arity_qualifier/2`.
""".
-spec arity_qualifier_body(syntaxTree()) -> syntaxTree().

arity_qualifier_body(Node) ->
    (data(Node))#arity_qualifier.body.


-doc """
Returns the argument (the arity) subtree of an `arity_qualifier` node.

_See also: _`arity_qualifier/2`.
""".
-spec arity_qualifier_argument(syntaxTree()) -> syntaxTree().

arity_qualifier_argument(Node) ->
    (data(Node))#arity_qualifier.arity.


-record(module_qualifier, {module :: syntaxTree(), body :: syntaxTree()}).

-doc """
Creates an abstract module qualifier.

The result represents "`Module:Body`".

_See also: _`module_qualifier_argument/1`, `module_qualifier_body/1`.
""".
-spec module_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {remote, Pos, Module, Arg}
%%
%%	Module = Arg = erl_parse()

module_qualifier(Module, Body) ->
    tree(module_qualifier,
	 #module_qualifier{module = Module, body = Body}).

revert_module_qualifier(Node) ->
    Pos = get_pos(Node),
    Module = module_qualifier_argument(Node),
    Body = module_qualifier_body(Node),
    {remote, Pos, Module, Body}.


-doc """
Returns the argument (the module) subtree of a `module_qualifier` node.

_See also: _`module_qualifier/2`.
""".
-spec module_qualifier_argument(syntaxTree()) -> syntaxTree().

module_qualifier_argument(Node) ->
    case unwrap(Node) of
	{remote, _, Module, _} ->
	    Module;
	Node1 ->
	    (data(Node1))#module_qualifier.module
    end.


-doc """
Returns the body subtree of a `module_qualifier` node.

_See also: _`module_qualifier/2`.
""".
-spec module_qualifier_body(syntaxTree()) -> syntaxTree().

module_qualifier_body(Node) ->
    case unwrap(Node) of
	{remote, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#module_qualifier.body
    end.


%% Don't use the name 'function' for this record, to avoid confusion with
%% the tuples of the form {function,Name,Arity} used by erl_parse.
%%
%% (There's no real point in precomputing and storing the arity,
%% and passing it as a constructor argument makes it possible to
%% end up with an inconsistent value. Besides, some people might
%% want to check all clauses, and not just the first, so the
%% computation is not obvious.)

-record(func, {name :: syntaxTree(), clauses :: [syntaxTree()]}).

-doc """
Creates an abstract function definition.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`Name C1;
...; Name Cn.`". More exactly, if each `Ci` represents "`(Pi1, ...,
Pim) Gi -> Bi`", then the result represents "`Name(P11, ...,
P1m) G1 -> B1; ...; Name(Pn1, ..., Pnm) Gn -> Bn.`".
Function definitions are source code forms.

_See also: _`function_arity/1`, `function_clauses/1`, `function_name/1`,
`is_form/1`.
""".
-spec function(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {function, Pos, Name, Arity, Clauses}
%%
%%	Name = atom()
%%	Arity = integer()
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%
%%	where the number of patterns in each clause should be equal to
%%	the integer `Arity'; see `clause' for documentation on
%%	`erl_parse' clauses.

function(Name, Clauses) ->
    tree(function, #func{name = Name, clauses = Clauses}).

revert_function(Node) ->
    Name = function_name(Node),
    Clauses = [revert_clause(C) || C <- function_clauses(Node)],
    Pos = get_pos(Node),
    case type(Name) of
	atom ->
	    A = function_arity(Node),
	    {function, Pos, concrete(Name), A, Clauses};
	_ ->
	    Node
    end.


-doc """
Returns the name subtree of a `function` node.

_See also: _`function/2`.
""".
-spec function_name(syntaxTree()) -> syntaxTree().

function_name(Node) ->
    case unwrap(Node) of
	{function, Pos, Name, _, _} ->
	    set_pos(atom(Name), Pos);
	Node1 ->
	    (data(Node1))#func.name
    end.


-doc """
Returns the list of clause subtrees of a `function` node.

_See also: _`function/2`.
""".
-spec function_clauses(syntaxTree()) -> [syntaxTree()].

function_clauses(Node) ->
    case unwrap(Node) of
	{function, _, _, _, Clauses} ->
	    Clauses;
	Node1 ->
	    (data(Node1))#func.clauses
    end.


-doc """
Returns the arity of a `function` node.

The result is the number of parameter patterns in the first clause of
the function; subsequent clauses are ignored.

An exception is thrown if [`function_clauses(Node)`](`function_clauses/1`)
returns an empty list, or if the first element of that list is not a syntax tree
`C` of type `clause` such that [`clause_patterns(C)`](`clause_patterns/1`) is a
nonempty list.

_See also: _`clause/3`, `clause_patterns/1`, `function/2`, `function_clauses/1`.
""".
-spec function_arity(syntaxTree()) -> arity().

function_arity(Node) ->
    %% Note that this never accesses the arity field of `erl_parse'
    %% function nodes.
    length(clause_patterns(hd(function_clauses(Node)))).


%% =====================================================================

-type guard() :: 'none' | syntaxTree() | [syntaxTree()] | [[syntaxTree()]].

-doc #{equiv => clause([], Guard, Body)}.
-spec clause(guard(), [syntaxTree()]) -> syntaxTree().

clause(Guard, Body) ->
    clause([], Guard, Body).


-record(clause, {patterns :: [syntaxTree()],
		 guard    :: guard(),
		 body     :: [syntaxTree()]}).

-doc """
Creates an abstract clause.

If `Patterns` is `[P1, ..., Pn]` and `Body` is `[B1, ..., Bm]`, then
if `Guard` is `none`, the result represents "`(P1, ..., Pn) ->
B1, ..., Bm`", otherwise, unless `Guard` is a list, the result
represents "`(P1, ..., Pn) when Guard -> B1, ..., Bm`".

For simplicity, the `Guard` argument may also be any of the following:

- An empty list `[]`. This is equivalent to passing `none`.
- A nonempty list `[E1, ..., Ej]` of syntax trees. This is equivalent to passing
  `conjunction([E1, ..., Ej])`.
- A nonempty list of lists of syntax trees
  `[[E1_1, ..., E1_k1], ..., [Ej_1, ..., Ej_kj]]`, which is equivalent to
  passing
  `disjunction([conjunction([E1_1, ..., E1_k1]), ..., conjunction([Ej_1, ..., Ej_kj])])`.

_See also: _`clause/2`, `clause_body/1`, `clause_guard/1`, `clause_patterns/1`.
""".
-spec clause([syntaxTree()], guard(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {clause, Pos, Patterns, Guard, Body}
%%
%%	Patterns = [erl_parse()]
%%	Guard = [[erl_parse()]] | [erl_parse()]
%%	Body = [erl_parse()] \ []
%%
%%	Taken out of context, if `Patterns' is `[P1, ..., Pn]' and
%%	`Body' is `[B1, ..., Bm]', this represents `(<P1>, ..., <Pn>) ->
%%	<B1>, ..., <Bm>' if `Guard' is `[]', or otherwise `(<P1>, ...,
%%	<Pn>) when <G> -> <Body>', where `G' is `<E1_1>, ..., <E1_k1>;
%%	...; <Ej_1>, ..., <Ej_kj>', if `Guard' is a list of lists
%%	`[[E1_1, ..., E1_k1], ..., [Ej_1, ..., Ej_kj]]'. In older
%%	versions, `Guard' was simply a list `[E1, ..., En]' of parse
%%	trees, which is equivalent to the new form `[[E1, ..., En]]'.

clause(Patterns, Guard, Body) ->
    Guard1 = case Guard of
		 [] ->
		     none;
		 [X | _] when is_list(X) ->
		     disjunction(conjunction_list(Guard));
		 [_ | _] ->
		     %% Handle older forms also.
		     conjunction(Guard);
		 _ ->
		     %% This should be `none' or a syntax tree.
		     Guard
	     end,
    tree(clause, #clause{patterns = Patterns, guard = Guard1,
			 body = Body}).

conjunction_list([L | Ls]) ->
    [conjunction(L) | conjunction_list(Ls)];
conjunction_list([]) ->
    [].

revert_clause(Node) ->
    Pos = get_pos(Node),
    Guard = case clause_guard(Node) of
		none ->
		    [];
		E ->
		    case type(E) of
			disjunction ->
			    revert_clause_disjunction(E);
			conjunction ->
			    %% Only the top level expression is
			    %% unfolded here; no recursion.
			    [conjunction_body(E)];
			_ ->
			    [[E]]	% a single expression
		    end
	    end,
    {clause, Pos, clause_patterns(Node), Guard,
     clause_body(Node)}.

revert_clause_disjunction(D) ->
    %% We handle conjunctions within a disjunction, but only at
    %% the top level; no recursion.
    [case type(E) of
	 conjunction ->
	     conjunction_body(E);
	 _ ->
	     [E]
     end
     || E <- disjunction_body(D)].

revert_try_clause(Node) ->
    fold_try_clause(revert_clause(Node)).

fold_try_clause({clause, Pos, [P], Guard, Body}) ->
    P1 = case type(P) of
	     class_qualifier ->
		 {tuple, Pos, [class_qualifier_argument(P),
			       class_qualifier_body(P),
			       class_qualifier_stacktrace(P)]};
	     _ ->
		 {tuple, Pos, [{atom, Pos, throw}, P, {var, Pos, '_'}]}
	 end,
    {clause, Pos, [P1], Guard, Body}.

unfold_try_clauses(Cs) ->
    [unfold_try_clause(C) || C <- Cs].

unfold_try_clause({clause, Pos, [{tuple, _, [{atom, _, throw},
                                             V,
                                             {var, _, '_'}]}],
		   Guard, Body}) ->
    {clause, Pos, [V], Guard, Body};
unfold_try_clause({clause, Pos, [{tuple, _, [C, V, Stacktrace]}],
		   Guard, Body}) ->
    {clause, Pos, [class_qualifier(C, V, Stacktrace)], Guard, Body}.


-doc """
Returns the list of pattern subtrees of a `clause` node.

_See also: _`clause/3`.
""".
-spec clause_patterns(syntaxTree()) -> [syntaxTree()].

clause_patterns(Node) ->
    case unwrap(Node) of
	{clause, _, Patterns, _, _} ->
	    Patterns;
	Node1 ->
	    (data(Node1))#clause.patterns
    end.


-doc """
Returns the guard subtree of a `clause` node, if any.

If `Node` represents "`(P1, ..., Pn) when Guard -> B1, ...,
Bm`", `Guard` is returned.  Otherwise, the result is `none`.

_See also: _`clause/3`.
""".
-spec clause_guard(syntaxTree()) -> 'none' | syntaxTree().

clause_guard(Node) ->
    case unwrap(Node) of
	{clause, _, _, Guard, _} ->
	    case Guard of
		[] -> none;
		[L | _] when is_list(L) ->
		    disjunction(conjunction_list(Guard));
		[_ | _] ->
		    conjunction(Guard)
	    end;
	Node1 ->
	    (data(Node1))#clause.guard
    end.


-doc """
Return the list of body subtrees of a `clause` node.

_See also: _`clause/3`.
""".
-spec clause_body(syntaxTree()) -> [syntaxTree()].

clause_body(Node) ->
    case unwrap(Node) of
	{clause, _, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#clause.body
    end.


-doc """
Creates an abstract disjunction.

If `List` is `[E1, ..., En]`, the result represents "`E1; ...; En`".

_See also: _`conjunction/1`, `disjunction_body/1`.
""".
-spec disjunction([syntaxTree()]) -> syntaxTree().

disjunction(Tests) ->
    tree(disjunction, Tests).


-doc """
Returns the list of body subtrees of a `disjunction` node.

_See also: _`disjunction/1`.
""".
-spec disjunction_body(syntaxTree()) -> [syntaxTree()].

disjunction_body(Node) ->
    data(Node).


-doc """
Creates an abstract conjunction.

If `List` is `[E1, ..., En]`, the result represents "`E1, ..., En`".

_See also: _`conjunction_body/1`, `disjunction/1`.
""".
-spec conjunction([syntaxTree()]) -> syntaxTree().

conjunction(Tests) ->
    tree(conjunction, Tests).


-doc """
Returns the list of body subtrees of a `conjunction` node.

_See also: _`conjunction/1`.
""".
-spec conjunction_body(syntaxTree()) -> [syntaxTree()].

conjunction_body(Node) ->
    data(Node).


-doc """
Creates an abstract catch-expression.

The result represents "`catch Expr`".

_See also: _`catch_expr_body/1`.
""".
-spec catch_expr(syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {'catch', Pos, Expr}
%%
%%	Expr = erl_parse()

catch_expr(Expr) ->
    tree(catch_expr, Expr).

revert_catch_expr(Node) ->
    Pos = get_pos(Node),
    Expr = catch_expr_body(Node),
    {'catch', Pos, Expr}.


-doc """
Returns the body subtree of a `catch_expr` node.

_See also: _`catch_expr/1`.
""".
-spec catch_expr_body(syntaxTree()) -> syntaxTree().

catch_expr_body(Node) ->
    case unwrap(Node) of
	{'catch', _, Expr} ->
	    Expr;
	Node1 ->
	    data(Node1)
    end.


-record(match_expr, {pattern :: syntaxTree(), body :: syntaxTree()}).

-doc """
Creates an abstract match-expression.

The result represents "`Pattern = Body`".

_See also: _`match_expr_body/1`, `match_expr_pattern/1`.
""".
-spec match_expr(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {match, Pos, Pattern, Body}
%%
%%	Pattern = Body = erl_parse()

match_expr(Pattern, Body) ->
    tree(match_expr, #match_expr{pattern = Pattern, body = Body}).

revert_match_expr(Node) ->
    Pos = get_pos(Node),
    Pattern = match_expr_pattern(Node),
    Body = match_expr_body(Node),
    {match, Pos, Pattern, Body}.


-doc """
Returns the pattern subtree of a `match_expr` node.

_See also: _`match_expr/2`.
""".
-spec match_expr_pattern(syntaxTree()) -> syntaxTree().

match_expr_pattern(Node) ->
    case unwrap(Node) of
	{match, _, Pattern, _} ->
	    Pattern;
	Node1 ->
	    (data(Node1))#match_expr.pattern
    end.


-doc """
Returns the body subtree of a `match_expr` node.

_See also: _`match_expr/2`.
""".
-spec match_expr_body(syntaxTree()) -> syntaxTree().

match_expr_body(Node) ->
    case unwrap(Node) of
	{match, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#match_expr.body
    end.


-record(maybe_match_expr, {pattern :: syntaxTree(), body :: syntaxTree()}).

-doc """
Creates an abstract maybe-expression, as used in `maybe` blocks.

The result represents "`Pattern ?= Body`".

_See also: _`maybe_expr/2`, `maybe_match_expr_body/1`,
`maybe_match_expr_pattern/1`.
""".
-spec maybe_match_expr(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {maybe_match, Pos, Pattern, Body}
%%
%%	Pattern = Body = erl_parse()
%%

maybe_match_expr(Pattern, Body) ->
    tree(maybe_match_expr, #maybe_match_expr{pattern = Pattern, body = Body}).

revert_maybe_match_expr(Node) ->
    Pos = get_pos(Node),
    Pattern = maybe_match_expr_pattern(Node),
    Body = maybe_match_expr_body(Node),
    {maybe_match, Pos, Pattern, Body}.

-doc """
Returns the pattern subtree of a `maybe_expr` node.

_See also: _`maybe_match_expr/2`.
""".
-spec maybe_match_expr_pattern(syntaxTree()) -> syntaxTree().

maybe_match_expr_pattern(Node) ->
    case unwrap(Node) of
        {maybe_match, _, Pattern, _} ->
            Pattern;
        Node1 ->
            (data(Node1))#maybe_match_expr.pattern
    end.


-doc """
Returns the body subtree of a `maybe_expr` node.

_See also: _`maybe_match_expr/2`.
""".
-spec maybe_match_expr_body(syntaxTree()) -> syntaxTree().

maybe_match_expr_body(Node) ->
    case unwrap(Node) of
        {maybe_match, _, _, Body} ->
            Body;
        Node1 ->
            (data(Node1))#maybe_match_expr.body
    end.

-doc """
operator(Name)

Creates an abstract operator.

The name of the operator is the character sequence represented by
`Name`. This is analogous to the print name of an atom, but an
operator is never written within single-quotes; for example, the
result of [`operator('++')`](`operator/1`) represents "`++`" rather
than "`'++'`".

_See also: _`atom/1`, `operator_literal/1`, `operator_name/1`.
""".
-spec operator(atom() | string()) -> syntaxTree().

operator(Name) when is_atom(Name) ->
    tree(operator, Name);
operator(Name) ->
    tree(operator, list_to_atom(Name)).


-doc """
Returns the name of an `operator` node.

Note that the name is returned as an atom.

_See also: _`operator/1`.
""".
-spec operator_name(syntaxTree()) -> atom().

operator_name(Node) ->
    data(Node).


-doc """
Returns the literal string represented by an `operator` node.

This is simply the operator name as a string.

_See also: _`operator/1`.
""".
-spec operator_literal(syntaxTree()) -> string().

operator_literal(Node) ->
    atom_to_list(operator_name(Node)).


-record(infix_expr, {operator :: syntaxTree(),
		     left     :: syntaxTree(),
		     right    :: syntaxTree()}).

-doc """
Creates an abstract infix operator expression.

The result represents "`Left Operator Right`".

_See also: _`infix_expr_left/1`, `infix_expr_operator/1`, `infix_expr_right/1`,
`prefix_expr/2`.
""".
-spec infix_expr(syntaxTree(), syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {op, Pos, Operator, Left, Right}
%%
%%	Operator = atom()
%%	Left = Right = erl_parse()

infix_expr(Left, Operator, Right) ->
    tree(infix_expr, #infix_expr{operator = Operator, left = Left,
				 right = Right}).

revert_infix_expr(Node) ->
    Pos = get_pos(Node),
    Operator = infix_expr_operator(Node),
    Left = infix_expr_left(Node),
    Right = infix_expr_right(Node),
    case type(Operator) of
	operator ->
	    %% Note that the operator itself is not revertible out
	    %% of context.
	    {op, Pos, operator_name(Operator), Left, Right};
	_ ->
	    Node
    end.


-doc """
Returns the left argument subtree of an `infix_expr` node.

_See also: _`infix_expr/3`.
""".
-spec infix_expr_left(syntaxTree()) -> syntaxTree().

infix_expr_left(Node) ->
    case unwrap(Node) of
	{op, _, _, Left, _} ->
	    Left;
	Node1 ->
	    (data(Node1))#infix_expr.left
    end.


-doc """
Returns the operator subtree of an `infix_expr` node.

_See also: _`infix_expr/3`.
""".
-spec infix_expr_operator(syntaxTree()) -> syntaxTree().

infix_expr_operator(Node) ->
    case unwrap(Node) of
	{op, Pos, Operator, _, _} ->
	    set_pos(operator(Operator), Pos);
	Node1 ->
	    (data(Node1))#infix_expr.operator
    end.


-doc """
Returns the right argument subtree of an `infix_expr` node.

_See also: _`infix_expr/3`.
""".
-spec infix_expr_right(syntaxTree()) -> syntaxTree().

infix_expr_right(Node) ->
    case unwrap(Node) of
	{op, _, _, _, Right} ->
	    Right;
	Node1 ->
	    (data(Node1))#infix_expr.right
    end.


-record(prefix_expr, {operator :: syntaxTree(), argument :: syntaxTree()}).

-doc """
Creates an abstract prefix operator expression.

The result represents "`Operator Argument`".

_See also: _`infix_expr/3`, `prefix_expr_argument/1`, `prefix_expr_operator/1`.
""".
-spec prefix_expr(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {op, Pos, Operator, Arg}
%%
%%	Operator = atom()
%%	Argument = erl_parse()

prefix_expr(Operator, Argument) ->
    tree(prefix_expr, #prefix_expr{operator = Operator,
				   argument = Argument}).

revert_prefix_expr(Node) ->
    Pos = get_pos(Node),
    Operator = prefix_expr_operator(Node),
    Argument = prefix_expr_argument(Node),
    case type(Operator) of
	operator ->
	    %% Note that the operator itself is not revertible out
	    %% of context.
	    {op, Pos, operator_name(Operator), Argument};
	_ ->
	    Node
    end.


-doc """
Returns the operator subtree of a `prefix_expr` node.

_See also: _`prefix_expr/2`.
""".
-spec prefix_expr_operator(syntaxTree()) -> syntaxTree().

prefix_expr_operator(Node) ->
    case unwrap(Node) of
	{op, Pos, Operator, _} ->
	    set_pos(operator(Operator), Pos);
	Node1 ->
	    (data(Node1))#prefix_expr.operator
    end.


-doc """
Returns the argument subtree of a `prefix_expr` node.

_See also: _`prefix_expr/2`.
""".
-spec prefix_expr_argument(syntaxTree()) -> syntaxTree().

prefix_expr_argument(Node) ->
    case unwrap(Node) of
	{op, _, _, Argument} ->
	    Argument;
	Node1 ->
	    (data(Node1))#prefix_expr.argument
    end.


%% =====================================================================

-doc #{equiv => record_field(Name, none)}.
-spec record_field(syntaxTree()) -> syntaxTree().

record_field(Name) ->
    record_field(Name, none).


-record(record_field, {name :: syntaxTree(), value :: 'none' | syntaxTree()}).

-doc """
Creates an abstract record field specification.

If `Value` is `none`, the result represents simply "`Name`",
otherwise it represents "`Name = Value`".

_See also: _`record_expr/3`, `record_field_name/1`, `record_field_value/1`.
""".
-spec record_field(syntaxTree(), 'none' | syntaxTree()) -> syntaxTree().

record_field(Name, Value) ->
    tree(record_field, #record_field{name = Name, value = Value}).


-doc """
Returns the name subtree of a `record_field` node.

_See also: _`record_field/2`.
""".
-spec record_field_name(syntaxTree()) -> syntaxTree().

record_field_name(Node) ->
    (data(Node))#record_field.name.


-doc """
Returns the value subtree of a `record_field` node, if any.

If `Node` represents "`Name`", `none` is returned. Otherwise, if
`Node` represents "`Name = Value`", `Value` is returned.

_See also: _`record_field/2`.
""".
-spec record_field_value(syntaxTree()) -> 'none' | syntaxTree().

record_field_value(Node) ->
    (data(Node))#record_field.value.


-record(record_index_expr, {type :: syntaxTree(), field :: syntaxTree()}).

-doc """
Creates an abstract record field index expression. The result represents
"`#Type.Field`".

> #### Note {: .info }
>
> The function name `record_index/2` is reserved by the Erlang compiler,
> which is why that name could not be used for this constructor.

_See also: _`record_expr/3`, `record_index_expr_field/1`,
`record_index_expr_type/1`.
""".
-spec record_index_expr(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {record_index, Pos, Type, Field}
%%
%%	Type = atom()
%%	Field = erl_parse()

record_index_expr(Type, Field) ->
    tree(record_index_expr, #record_index_expr{type = Type,
					       field = Field}).

revert_record_index_expr(Node) ->
    Pos = get_pos(Node),
    Type = record_index_expr_type(Node),
    Field = record_index_expr_field(Node),
    case type(Type) of
	atom ->
	    {record_index, Pos, concrete(Type), Field};
	_ ->
	    Node
    end.


-doc """
Returns the type subtree of a `record_index_expr` node.

_See also: _`record_index_expr/2`.
""".
-spec record_index_expr_type(syntaxTree()) -> syntaxTree().

record_index_expr_type(Node) ->
    case unwrap(Node) of
	{record_index, Pos, Type, _} ->
	    set_pos(atom(Type), Pos);
	Node1 ->
	    (data(Node1))#record_index_expr.type
    end.


-doc """
Returns the field subtree of a `record_index_expr` node.

_See also: _`record_index_expr/2`.
""".
-spec record_index_expr_field(syntaxTree()) -> syntaxTree().

record_index_expr_field(Node) ->
    case unwrap(Node) of
	{record_index, _, _, Field} ->
	    Field;
	Node1 ->
	    (data(Node1))#record_index_expr.field
    end.


-record(record_access, {argument :: syntaxTree(),
			type     :: syntaxTree(),
			field    :: syntaxTree()}).

-doc """
Creates an abstract record field access expression.

The result represents "`Argument#Type.Field`".

_See also: _`record_access_argument/1`, `record_access_field/1`,
`record_access_type/1`, `record_expr/3`.
""".
-spec record_access(syntaxTree(), syntaxTree(), syntaxTree()) ->
        syntaxTree().

%% `erl_parse' representation:
%%
%% {record_field, Pos, Argument, Type, Field}
%%
%%	Argument = Field = erl_parse()
%%	Type = atom()

record_access(Argument, Type, Field) ->
    tree(record_access,#record_access{argument = Argument,
				      type = Type,
				      field = Field}).

revert_record_access(Node) ->
    Pos = get_pos(Node),
    Argument = record_access_argument(Node),
    Type = record_access_type(Node),
    Field = record_access_field(Node),
    case type(Type) of
        atom ->
            {record_field, Pos, Argument, concrete(Type), Field};
        _ ->
            Node
    end.


-doc """
Returns the argument subtree of a `record_access` node.

_See also: _`record_access/3`.
""".
-spec record_access_argument(syntaxTree()) -> syntaxTree().

record_access_argument(Node) ->
    case unwrap(Node) of
	{record_field, _, Argument, _, _} ->
	    Argument;
	Node1 ->
	    (data(Node1))#record_access.argument
    end.


-doc """
Returns the type subtree of a `record_access` node.

_See also: _`record_access/3`.
""".
-spec record_access_type(syntaxTree()) -> syntaxTree().

record_access_type(Node) ->
    case unwrap(Node) of
	{record_field, Pos, _, Type, _} ->
	    set_pos(atom(Type), Pos);
	Node1 ->
	    (data(Node1))#record_access.type
    end.


-doc """
Returns the field subtree of a `record_access` node.

_See also: _`record_access/3`.
""".
-spec record_access_field(syntaxTree()) -> syntaxTree().

record_access_field(Node) ->
    case unwrap(Node) of
	{record_field, _, _, _, Field} ->
	    Field;
	Node1 ->
	    (data(Node1))#record_access.field
    end.


%% =====================================================================

-doc #{equiv => record_expr(none, Type, Fields)}.
-spec record_expr(syntaxTree(), [syntaxTree()]) -> syntaxTree().

record_expr(Type, Fields) ->
    record_expr(none, Type, Fields).


-record(record_expr, {argument :: 'none' | syntaxTree(),
		      type     :: syntaxTree(),
		      fields   :: [syntaxTree()]}).

-doc """
Creates an abstract record expression.

If `Fields` is `[F1, ..., Fn]`, then if `Argument` is `none`, the
result represents "`#Type{F1, ..., Fn}`", otherwise it
represents "`Argument#Type{F1, ..., Fn}`".

_See also: _`record_access/3`, `record_expr/2`, `record_expr_argument/1`,
`record_expr_fields/1`, `record_expr_type/1`, `record_field/2`,
`record_index_expr/2`.
""".
-spec record_expr('none' | syntaxTree(), syntaxTree(), [syntaxTree()]) ->
        syntaxTree().

%% `erl_parse' representation:
%%
%% {record, Pos, Type, Fields}
%% {record, Pos, Argument, Type, Fields}
%%
%%	Argument = erl_parse()
%%	Type = atom()
%%	Fields = [Entry]
%%	Entry = {record_field, Pos, Field, Value}
%%	      | {record_field, Pos, Field}
%%	Field = Value = erl_parse()

record_expr(Argument, Type, Fields) ->
    tree(record_expr, #record_expr{argument = Argument,
				   type = Type, fields = Fields}).

revert_record_expr(Node) ->
    Pos = get_pos(Node),
    Argument = record_expr_argument(Node),
    Type = record_expr_type(Node),
    Fields = record_expr_fields(Node),
    case type(Type) of
	atom ->
	    T = concrete(Type),
	    Fs = fold_record_fields(Fields),
	    case Argument of
		none ->
		    {record, Pos, T, Fs};
		_ ->
		    {record, Pos, Argument, T, Fs}
	    end;
	_ ->
	    Node
    end.


-doc """
Returns the argument subtree of a `record_expr` node, if any.

If `Node` represents "`#Type{...}`", `none` is returned. Otherwise,
if `Node` represents "`Argument#Type{...}`", `Argument` is
returned.

_See also: _`record_expr/3`.
""".
-spec record_expr_argument(syntaxTree()) -> 'none' | syntaxTree().

record_expr_argument(Node) ->
    case unwrap(Node) of
	{record, _, _, _} ->
	    none;
	{record, _, Argument, _, _} ->
	    Argument;
	Node1 ->
	    (data(Node1))#record_expr.argument
    end.


-doc """
Returns the type subtree of a `record_expr` node.

_See also: _`record_expr/3`.
""".
-spec record_expr_type(syntaxTree()) -> syntaxTree().

record_expr_type(Node) ->
    case unwrap(Node) of
	{record, Pos, Type, _} ->
	    set_pos(atom(Type), Pos);
	{record, Pos, _, Type, _} ->
	    set_pos(atom(Type), Pos);
	Node1 ->
	    (data(Node1))#record_expr.type
    end.


-doc """
Returns the list of field subtrees of a `record_expr` node.

_See also: _`record_expr/3`.
""".
-spec record_expr_fields(syntaxTree()) -> [syntaxTree()].

record_expr_fields(Node) ->
    case unwrap(Node) of
	{record, _, _, Fields} ->
	    unfold_record_fields(Fields);
	{record, _, _, _, Fields} ->
	    unfold_record_fields(Fields);
	Node1 ->
	    (data(Node1))#record_expr.fields
    end.


-doc """
application(Module, Name, Arguments)

Creates an abstract function application expression.

If `Module` is `none`, this is call is equivalent to
[`application(Function, Arguments)`](`application/2`), otherwise it is
equivalent to [`application(module_qualifier(Module, Function),
Arguments)`](`application/2`).

(This is a utility function.)

_See also: _`application/2`, `module_qualifier/2`.
""".
-spec application('none' | syntaxTree(), syntaxTree(), [syntaxTree()]) ->
        syntaxTree().

application(none, Name, Arguments) ->
    application(Name, Arguments);
application(Module, Name, Arguments) ->
    application(module_qualifier(Module, Name), Arguments).


-record(application, {operator :: syntaxTree(), arguments :: [syntaxTree()]}).

-doc """
Creates an abstract function application expression.

If `Arguments` is `[A1, ..., An]`, the result represents
"`Operator(A1, ..., An)`".

_See also: _`application/3`, `application_arguments/1`,
`application_operator/1`.
""".
-spec application(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {call, Pos, Operator, Args}
%%
%%	Operator = erl_parse()
%%	Arguments = [erl_parse()]

application(Operator, Arguments) ->
    tree(application, #application{operator = Operator,
				   arguments = Arguments}).

revert_application(Node) ->
    Pos = get_pos(Node),
    Operator = application_operator(Node),
    Arguments = application_arguments(Node),
    {call, Pos, Operator, Arguments}.


-doc """
Returns the operator subtree of an `application` node.

If `Node` represents "`M:F(...)`", then the result is the subtree
representing "`M:F`".

_See also: _`application/2`, `module_qualifier/2`.
""".
-spec application_operator(syntaxTree()) -> syntaxTree().

application_operator(Node) ->
    case unwrap(Node) of
	{call, _, Operator, _} ->
	    Operator;
	Node1 ->
	    (data(Node1))#application.operator
    end.


-doc """
Returns the list of argument subtrees of an `application` node.

_See also: _`application/2`.
""".
-spec application_arguments(syntaxTree()) -> [syntaxTree()].

application_arguments(Node) ->
    case unwrap(Node) of
	{call, _, _, Arguments} ->
	    Arguments;
	Node1 ->
	    (data(Node1))#application.arguments
    end.

-record(annotated_type, {name :: syntaxTree(), body :: syntaxTree()}).

-doc """
Creates an abstract annotated type expression.

The result represents "`Name :: Type`".

_See also: _`annotated_type_body/1`, `annotated_type_name/1`.
""".
-spec annotated_type(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {ann_type, Pos, [Name, Type]}
%%
%%      Name = erl_parse()
%%      Type = erl_parse()

annotated_type(Name, Type) ->
    tree(annotated_type, #annotated_type{name = Name, body = Type}).

revert_annotated_type(Node) ->
    Pos = get_pos(Node),
    Name = annotated_type_name(Node),
    Type = annotated_type_body(Node),
    {ann_type, Pos, [Name, Type]}.


-doc """
Returns the name subtree of an `annotated_type` node.

_See also: _`annotated_type/2`.
""".
-spec annotated_type_name(syntaxTree()) -> syntaxTree().

annotated_type_name(Node) ->
    case unwrap(Node) of
        {ann_type, _, [Name, _]} ->
            Name;
        Node1 ->
            (data(Node1))#annotated_type.name
    end.


-doc """
Returns the type subtrees of an `annotated_type` node.

_See also: _`annotated_type/2`.
""".
-spec annotated_type_body(syntaxTree()) -> syntaxTree().

annotated_type_body(Node) ->
    case unwrap(Node) of
        {ann_type, _, [_, Type]} ->
            Type;
        Node1 ->
            (data(Node1))#annotated_type.body
    end.


-doc """
Creates an abstract fun of any type.

The result represents "`fun()`".
""".
-spec fun_type() -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, 'fun', []}

fun_type() ->
    tree(fun_type).

revert_fun_type(Node) ->
    Pos = get_pos(Node),
    {type, Pos, 'fun', []}.


-doc """
type_application(Module, TypeName, Arguments)

Creates an abstract type application expression.

If `Module` is `none`, this is call is equivalent to
[`type_application(TypeName, Arguments)`](`type_application/2`),
otherwise it is equivalent to
[`type_application(module_qualifier(Module, TypeName),
Arguments)`](`type_application/2`).

(This is a utility function.)

_See also: _`module_qualifier/2`, `type_application/2`.
""".
-spec type_application('none' | syntaxTree(), syntaxTree(), [syntaxTree()]) ->
        syntaxTree().

type_application(none, TypeName, Arguments) ->
    type_application(TypeName, Arguments);
type_application(Module, TypeName, Arguments) ->
    type_application(module_qualifier(Module, TypeName), Arguments).


-record(type_application, {type_name :: syntaxTree(),
                           arguments :: [syntaxTree()]}).

-doc """
Creates an abstract type application expression.

If `Arguments` is `[T1, ..., Tn]`, the result represents
"`TypeName(T1, ...Tn)`".

_See also: _`type_application/3`, `type_application_arguments/1`,
`type_application_name/1`, `user_type_application/2`.
""".
-spec type_application(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {remote, Pos, [Module, Name, Arguments]} |
%% {type, Pos, Name, Arguments}
%%
%%      Module = erl_parse()
%%      Name = atom()
%%      Arguments = [erl_parse()]

type_application(TypeName, Arguments) ->
    tree(type_application,
         #type_application{type_name = TypeName, arguments = Arguments}).

revert_type_application(Node) ->
    Pos = get_pos(Node),
    TypeName = type_application_name(Node),
    Arguments = type_application_arguments(Node),
    case type(TypeName) of
        module_qualifier ->
            Module = module_qualifier_argument(TypeName),
            Name = module_qualifier_body(TypeName),
            {remote_type, Pos, [Module, Name, Arguments]};
        atom ->
            {type, Pos, atom_value(TypeName), Arguments}
    end.


-doc """
Returns the type name subtree of a `type_application` node.

_See also: _`type_application/2`.
""".
-spec type_application_name(syntaxTree()) -> syntaxTree().

type_application_name(Node) ->
    case unwrap(Node) of
        {remote_type, _, [Module, Name, _]} ->
            module_qualifier(Module, Name);
        {type, Pos, Name, _} ->
            set_pos(atom(Name), Pos);
        Node1 ->
            (data(Node1))#type_application.type_name
    end.


-doc """
Returns the arguments subtrees of a `type_application` node.

_See also: _`type_application/2`.
""".
-spec type_application_arguments(syntaxTree()) -> [syntaxTree()].

type_application_arguments(Node) ->
    case unwrap(Node) of
        {remote_type, _, [_, _, Arguments]} ->
            Arguments;
        {type, _, _, Arguments} ->
            Arguments;
        Node1 ->
            (data(Node1))#type_application.arguments
    end.


-record(bitstring_type, {m :: syntaxTree(), n :: syntaxTree()}).

-doc """
Creates an abstract bitstring type.

The result represents "`<<_:M, _:_N>>`".

_See also: _`bitstring_type_m/1`, `bitstring_type_n/1`.
""".
-spec bitstring_type(syntaxTree(), syntaxTree()) -> syntaxTree().

bitstring_type(M, N) ->
    tree(bitstring_type, #bitstring_type{m = M, n =N}).

revert_bitstring_type(Node) ->
    Pos = get_pos(Node),
    M = bitstring_type_m(Node),
    N = bitstring_type_n(Node),
    {type, Pos, binary, [M, N]}.

-doc """
Returns the number of start bits, `M`, of a `bitstring_type` node.

_See also: _`bitstring_type/2`.
""".
-spec bitstring_type_m(syntaxTree()) -> syntaxTree().

bitstring_type_m(Node) ->
    case unwrap(Node) of
        {type, _, binary, [M, _]} ->
            M;
        Node1 ->
            (data(Node1))#bitstring_type.m
    end.

-doc """
Returns the segment size, `N`, of a `bitstring_type` node.

_See also: _`bitstring_type/2`.
""".
-spec bitstring_type_n(syntaxTree()) -> syntaxTree().

bitstring_type_n(Node) ->
    case unwrap(Node) of
        {type, _, binary, [_, N]} ->
            N;
        Node1 ->
            (data(Node1))#bitstring_type.n
    end.


-record(constrained_function_type, {body :: syntaxTree(),
                                    argument :: syntaxTree()}).

-doc """
Creates an abstract constrained function type.

If `FunctionConstraint` is `[C1, ..., Cn]`, the result represents
"`FunctionType when C1, ...Cn`".

_See also: _`constrained_function_type_argument/1`,
`constrained_function_type_body/1`.
""".
-spec constrained_function_type(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, bounded_fun, [FunctionType, FunctionConstraint]}
%%
%%      FunctionType = erl_parse()
%%      FunctionConstraint = [erl_parse()]

constrained_function_type(FunctionType, FunctionConstraint) ->
    Conj = conjunction(FunctionConstraint),
    tree(constrained_function_type,
         #constrained_function_type{body = FunctionType,
                                    argument = Conj}).

revert_constrained_function_type(Node) ->
    Pos = get_pos(Node),
    FunctionType = constrained_function_type_body(Node),
    FunctionConstraint =
        conjunction_body(constrained_function_type_argument(Node)),
    {type, Pos, bounded_fun, [FunctionType, FunctionConstraint]}.


-doc """
Returns the function type subtree of a `constrained_function_type` node.

_See also: _`constrained_function_type/2`.
""".
-spec constrained_function_type_body(syntaxTree()) -> syntaxTree().

constrained_function_type_body(Node) ->
    case unwrap(Node) of
        {type, _, bounded_fun, [FunctionType, _]} ->
            FunctionType;
        Node1 ->
            (data(Node1))#constrained_function_type.body
    end.

-doc """
Returns the function constraint subtree of a `constrained_function_type` node.

_See also: _`constrained_function_type/2`.
""".
-spec constrained_function_type_argument(syntaxTree()) -> syntaxTree().

constrained_function_type_argument(Node) ->
    case unwrap(Node) of
        {type, _, bounded_fun, [_, FunctionConstraint]} ->
            conjunction(FunctionConstraint);
        Node1 ->
            (data(Node1))#constrained_function_type.argument
    end.


%% =====================================================================

-doc #{equiv => function_type(any_arity, Type)}.
-spec function_type(syntaxTree()) -> syntaxTree().

function_type(Type) ->
    function_type(any_arity, Type).

-record(function_type, {arguments :: any_arity | [syntaxTree()],
                        return :: syntaxTree()}).

-doc """
Creates an abstract function type.

If `Arguments` is `[T1, ..., Tn]` *and* it occurs within a function
specification, the result represents "`(T1, ...Tn) -> Return`";
otherwise it represents "`fun((T1, ...Tn) -> Return)`". If
`Arguments` is `any_arity`, it represents "`fun((...) -> Return)`".

Note that the `m:erl_parse` representation is identical for
"`FunctionType`" and "`fun(FunctionType)`".

_See also: _`function_type_arguments/1`, `function_type_return/1`.
""".
-spec function_type('any_arity' | [syntaxTree()], syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, 'fun', [{type, Pos, product, Arguments}, Type]}
%% {type, Pos, 'fun', [{type, Pos, any}, Type]}
%%
%%      Arguments = [erl_parse()]
%%      Type = erl_parse()

function_type(Arguments, Return) ->
    tree(function_type,
         #function_type{arguments = Arguments, return = Return}).

revert_function_type(Node) ->
    Pos = get_pos(Node),
    Type = function_type_return(Node),
    case function_type_arguments(Node) of
        any_arity ->
            {type, Pos, 'fun', [{type, Pos, any}, Type]};
        Arguments ->
            {type, Pos, 'fun', [{type, Pos, product, Arguments}, Type]}
    end.


-doc """
Returns the argument types subtrees of a `function_type` node.

If `Node` represents "`fun((...) -> Return)`", `any_arity` is
returned; otherwise, if `Node` represents "`(T1, ...Tn) ->
Return`" or "`fun((T1, ...Tn) -> Return)`", `[T1, ..., Tn]` is
returned.

_See also: _`function_type/1`, `function_type/2`.
""".
-spec function_type_arguments(syntaxTree()) -> any_arity | [syntaxTree()].

function_type_arguments(Node) ->
    case unwrap(Node) of
        {type, _, 'fun', [{type, _, any}, _]} ->
            any_arity;
        {type, _, 'fun', [{type, _, product, Arguments}, _]} ->
            Arguments;
        Node1 ->
            (data(Node1))#function_type.arguments
    end.

-doc """
Returns the return type subtrees of a `function_type` node.

_See also: _`function_type/1`, `function_type/2`.
""".
-spec function_type_return(syntaxTree()) -> syntaxTree().

function_type_return(Node) ->
    case unwrap(Node) of
        {type, _, 'fun', [_, Type]} ->
            Type;
        Node1 ->
            (data(Node1))#function_type.return
    end.


%% =====================================================================

-record(constraint, {name :: syntaxTree(),
                     types :: [syntaxTree()]}).

-doc """
Creates an abstract (subtype) constraint.

The result represents "`Name :: Type`".

_See also: _`constraint_argument/1`, `constraint_body/1`.
""".
-spec constraint(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, constraint, [Name, [Var, Type]]}
%%
%%      Name = {atom, Pos, is_subtype}
%%      Var = erl_parse()
%%      Type = erl_parse()

constraint(Name, Types) ->
    tree(constraint,
         #constraint{name = Name, types = Types}).

revert_constraint(Node) ->
    Pos = get_pos(Node),
    Name = constraint_argument(Node),
    Types = constraint_body(Node),
    {type, Pos, constraint, [Name, Types]}.


-doc """
Returns the name subtree of a `constraint` node.

_See also: _`constraint/2`.
""".
-spec constraint_argument(syntaxTree()) -> syntaxTree().

constraint_argument(Node) ->
    case unwrap(Node) of
        {type, _, constraint, [Name, _]} ->
            Name;
        Node1 ->
            (data(Node1))#constraint.name
    end.

-doc """
Returns the type subtree of a `constraint` node.

_See also: _`constraint/2`.
""".
-spec constraint_body(syntaxTree()) -> [syntaxTree()].

constraint_body(Node) ->
    case unwrap(Node) of
        {type, _, constraint, [_, Types]} ->
            Types;
        Node1 ->
            (data(Node1))#constraint.types
    end.


%% =====================================================================

-record(map_type_assoc, {name :: syntaxTree(), value :: syntaxTree()}).

-doc """
Creates an abstract map type assoc field.

The result represents "`Name => Value`".

_See also: _`map_type/1`, `map_type_assoc_name/1`, `map_type_assoc_value/1`.
""".
-spec map_type_assoc(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, map_field_assoc, [Name, Value]}

map_type_assoc(Name, Value) ->
    tree(map_type_assoc, #map_type_assoc{name = Name, value = Value}).

revert_map_type_assoc(Node) ->
    Pos = get_pos(Node),
    Name = map_type_assoc_name(Node),
    Value = map_type_assoc_value(Node),
    {type, Pos, map_field_assoc, [Name, Value]}.


-doc """
Returns the name subtree of a `map_type_assoc` node.

_See also: _`map_type_assoc/2`.
""".
-spec map_type_assoc_name(syntaxTree()) -> syntaxTree().

map_type_assoc_name(Node) ->
    case unwrap(Node) of
        {type, _, map_field_assoc, [Name, _]} ->
            Name;
        Node1 ->
            (data(Node1))#map_type_assoc.name
    end.


-doc """
Returns the value subtree of a `map_type_assoc` node.

_See also: _`map_type_assoc/2`.
""".
-spec map_type_assoc_value(syntaxTree()) -> syntaxTree().

map_type_assoc_value(Node) ->
    case unwrap(Node) of
        {type, _, map_field_assoc, [_, Value]} ->
            Value;
        Node1 ->
            (data(Node1))#map_type_assoc.value
    end.


%% =====================================================================

-record(map_type_exact, {name :: syntaxTree(), value :: syntaxTree()}).

-doc """
Creates an abstract map type exact field.

The result represents "`Name := Value`".

_See also: _`map_type/1`, `map_type_exact_name/1`, `map_type_exact_value/1`.
""".
-spec map_type_exact(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, map_field_exact, [Name, Value]}

map_type_exact(Name, Value) ->
    tree(map_type_exact, #map_type_exact{name = Name, value = Value}).

revert_map_type_exact(Node) ->
    Pos = get_pos(Node),
    Name = map_type_exact_name(Node),
    Value = map_type_exact_value(Node),
    {type, Pos, map_field_exact, [Name, Value]}.


-doc """
Returns the name subtree of a `map_type_exact` node.

_See also: _`map_type_exact/2`.
""".
-spec map_type_exact_name(syntaxTree()) -> syntaxTree().

map_type_exact_name(Node) ->
    case unwrap(Node) of
        {type, _, map_field_exact, [Name, _]} ->
            Name;
        Node1 ->
            (data(Node1))#map_type_exact.name
    end.


-doc """
Returns the value subtree of a `map_type_exact` node.

_See also: _`map_type_exact/2`.
""".
-spec map_type_exact_value(syntaxTree()) -> syntaxTree().

map_type_exact_value(Node) ->
    case unwrap(Node) of
        {type, _, map_field_exact, [_, Value]} ->
            Value;
        Node1 ->
            (data(Node1))#map_type_exact.value
    end.


%% =====================================================================

-doc #{equiv => map_type(any_size)}.
-spec map_type() -> syntaxTree().

map_type() ->
    map_type(any_size).

-doc """
Creates an abstract type map.

If `Fields` is `[F1, ..., Fn]`, the result represents "`#{F1, ...,
Fn}`"; otherwise, if `Fields` is `any_size`, it represents
"`t:map/0`".

_See also: _`map_type_fields/1`.
""".
-spec map_type('any_size' | [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, map, [Field]}
%% {type, Pos, map, any}
%%
%%      Field = erl_parse()

map_type(Fields) ->
    tree(map_type, Fields).

revert_map_type(Node) ->
    Pos = get_pos(Node),
    case map_type_fields(Node) of
        any_size ->
            {type, Pos, map, any};
        Fields ->
            {type, Pos, map, Fields}
    end.

-doc """
Returns the list of field subtrees of a `map_type` node.

If `Node` represents "`t:map/0`", `any_size` is returned; otherwise,
if `Node` represents "`#{F1, ..., Fn}`", `[F1, ..., Fn]` is
returned.

_See also: _`map_type/0`, `map_type/1`.
""".
-spec map_type_fields(syntaxTree()) -> 'any_size' | [syntaxTree()].

map_type_fields(Node) ->
    case unwrap(Node) of
        {type, _, map, Fields} when is_list(Fields) ->
            Fields;
        {type, _, map, any} ->
            any_size;
        Node1 ->
            data(Node1)
    end.


%% =====================================================================

-record(integer_range_type, {low :: syntaxTree(),
                             high :: syntaxTree()}).

-doc """
Creates an abstract range type.

The result represents "`Low .. High`".

_See also: _`integer_range_type_high/1`, `integer_range_type_low/1`.
""".
-spec integer_range_type(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, range, [Low, High]}
%%
%%      Low = erl_parse()
%%      High = erl_parse()

integer_range_type(Low, High) ->
    tree(integer_range_type, #integer_range_type{low = Low, high = High}).

revert_integer_range_type(Node) ->
    Pos = get_pos(Node),
    Low = integer_range_type_low(Node),
    High = integer_range_type_high(Node),
    {type, Pos, range, [Low, High]}.


-doc """
Returns the low limit of an `integer_range_type` node.

_See also: _`integer_range_type/2`.
""".
-spec integer_range_type_low(syntaxTree()) -> syntaxTree().

integer_range_type_low(Node) ->
    case unwrap(Node) of
        {type, _, range, [Low, _]} ->
            Low;
        Node1 ->
            (data(Node1))#integer_range_type.low
    end.

-doc """
Returns the high limit of an `integer_range_type` node.

_See also: _`integer_range_type/2`.
""".
-spec integer_range_type_high(syntaxTree()) -> syntaxTree().

integer_range_type_high(Node) ->
    case unwrap(Node) of
        {type, _, range, [_, High]} ->
            High;
        Node1 ->
            (data(Node1))#integer_range_type.high
    end.


%% =====================================================================

-record(record_type, {name :: syntaxTree(),
                      fields :: [syntaxTree()]}).

-doc """
Creates an abstract record type.

If `Fields` is `[F1, ..., Fn]`, the result represents "`#Name{F1,
..., Fn}`".

_See also: _`record_type_fields/1`, `record_type_name/1`.
""".
-spec record_type(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, record, [Name|Fields]}
%%
%%      Name = erl_parse()
%%      Fields = [erl_parse()]

record_type(Name, Fields) ->
    tree(record_type, #record_type{name = Name, fields = Fields}).

revert_record_type(Node) ->
    Pos = get_pos(Node),
    Name = record_type_name(Node),
    Fields = record_type_fields(Node),
    {type, Pos, record, [Name | Fields]}.


-doc """
Returns the name subtree of a `record_type` node.

_See also: _`record_type/2`.
""".
-spec record_type_name(syntaxTree()) -> syntaxTree().

record_type_name(Node) ->
    case unwrap(Node) of
        {type, _, record, [Name|_]} ->
            Name;
        Node1 ->
            (data(Node1))#record_type.name
    end.

-doc """
Returns the fields subtree of a `record_type` node.

_See also: _`record_type/2`.
""".
-spec record_type_fields(syntaxTree()) -> [syntaxTree()].

record_type_fields(Node) ->
    case unwrap(Node) of
        {type, _, record, [_|Fields]} ->
            Fields;
        Node1 ->
            (data(Node1))#record_type.fields
    end.


-record(record_type_field, {name :: syntaxTree(),
                            type :: syntaxTree()}).

-doc """
Creates an abstract record type field.

The result represents "`Name :: Type`".

_See also: _`record_type_field_name/1`, `record_type_field_type/1`.
""".
-spec record_type_field(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, field_type, [Name, Type]}
%%
%%      Name = erl_parse()
%%      Type = erl_parse()

record_type_field(Name, Type) ->
    tree(record_type_field, #record_type_field{name = Name, type = Type}).

revert_record_type_field(Node) ->
    Pos = get_pos(Node),
    Name = record_type_field_name(Node),
    Type = record_type_field_type(Node),
    {type, Pos, field_type, [Name, Type]}.


-doc """
Returns the name subtree of a `record_type_field` node.

_See also: _`record_type_field/2`.
""".
-spec record_type_field_name(syntaxTree()) -> syntaxTree().

record_type_field_name(Node) ->
    case unwrap(Node) of
        {type, _, field_type, [Name, _]} ->
            Name;
        Node1 ->
            (data(Node1))#record_type_field.name
    end.

-doc """
Returns the type subtree of a `record_type_field` node.

_See also: _`record_type_field/2`.
""".
-spec record_type_field_type(syntaxTree()) -> syntaxTree().

record_type_field_type(Node) ->
    case unwrap(Node) of
        {type, _, field_type, [_, Type]} ->
            Type;
        Node1 ->
            (data(Node1))#record_type_field.type
    end.


%% =====================================================================

-doc #{equiv => tuple_type(any_size)}.
-spec tuple_type() -> syntaxTree().

tuple_type() ->
    tuple_type(any_size).

-doc """
Creates an abstract type tuple.

If `Elements` is `[T1, ..., Tn]`, the result represents "`{T1, ...,
Tn}`"; otherwise, if `Elements` is `any_size`, it represents
"`t:tuple/0`".

_See also: _`tuple_type_elements/1`.
""".
-spec tuple_type(any_size | [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, tuple, [Element]}
%% {type, Pos, tuple, any}
%%
%%      Element = erl_parse()

tuple_type(Elements) ->
    tree(tuple_type, Elements).

revert_tuple_type(Node) ->
    Pos = get_pos(Node),
    case tuple_type_elements(Node) of
        any_size ->
            {type, Pos, tuple, any};
        TypeElements ->
            {type, Pos, tuple, TypeElements}
    end.


-doc """
Returns the list of type element subtrees of a `tuple_type` node.

If `Node` represents "`t:tuple/0`", `any_size` is returned; otherwise,
if `Node` represents "`{T1, ..., Tn}`", `[T1, ..., Tn]` is returned.

_See also: _`tuple_type/0`, `tuple_type/1`.
""".
-spec tuple_type_elements(syntaxTree()) -> 'any_size' | [syntaxTree()].

tuple_type_elements(Node) ->
    case unwrap(Node) of
        {type, _, tuple, Elements} when is_list(Elements) ->
            Elements;
        {type, _, tuple, any} ->
            any_size;
        Node1 ->
            data(Node1)
    end.


%% =====================================================================

-doc """
Creates an abstract type union.

If `Types` is `[T1, ..., Tn]`, the result represents "`T1 | ... |
Tn`".

_See also: _`type_union_types/1`.
""".
-spec type_union([syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {type, Pos, union, Elements}
%%
%%      Elements = [erl_parse()]

type_union(Types) ->
    tree(type_union, Types).

revert_type_union(Node) ->
    Pos = get_pos(Node),
    {type, Pos, union, type_union_types(Node)}.


-doc """
Returns the list of type subtrees of a `type_union` node.

_See also: _`type_union/1`.
""".
-spec type_union_types(syntaxTree()) -> [syntaxTree()].

type_union_types(Node) ->
    case unwrap(Node) of
        {type, _, union, Types} when is_list(Types) ->
            Types;
        Node1 ->
            data(Node1)
    end.


%% =====================================================================

-record(user_type_application, {type_name :: syntaxTree(),
                                arguments :: [syntaxTree()]}).

-doc """
Creates an abstract user type.

If `Arguments` is `[T1, ..., Tn]`, the result represents
"`TypeName(T1, ...Tn)`".

_See also: _`type_application/2`, `user_type_application_arguments/1`,
`user_type_application_name/1`.
""".
-spec user_type_application(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {user_type, Pos, Name, Arguments}
%%
%%      Name = erl_parse()
%%      Arguments = [Type]
%%      Type = erl_parse()

user_type_application(TypeName, Arguments) ->
    tree(user_type_application,
         #user_type_application{type_name = TypeName, arguments = Arguments}).

revert_user_type_application(Node) ->
    Pos = get_pos(Node),
    TypeName = user_type_application_name(Node),
    Arguments = user_type_application_arguments(Node),
    {user_type, Pos, atom_value(TypeName), Arguments}.


-doc """
Returns the type name subtree of a `user_type_application` node.

_See also: _`user_type_application/2`.
""".
-spec user_type_application_name(syntaxTree()) -> syntaxTree().

user_type_application_name(Node) ->
    case unwrap(Node) of
        {user_type, Pos, Name, _} ->
            set_pos(atom(Name), Pos);
        Node1 ->
            (data(Node1))#user_type_application.type_name
    end.


-doc """
Returns the arguments subtrees of a `user_type_application` node.

_See also: _`user_type_application/2`.
""".
-spec user_type_application_arguments(syntaxTree()) -> [syntaxTree()].

user_type_application_arguments(Node) ->
    case unwrap(Node) of
        {user_type, _, _, Arguments} ->
            Arguments;
        Node1 ->
            (data(Node1))#user_type_application.arguments
    end.


%% =====================================================================

-record(typed_record_field, {body :: syntaxTree(),
                             type :: syntaxTree()}).

-doc """
Creates an abstract typed record field specification.

The result represents "`Field :: Type`".

_See also: _`typed_record_field_body/1`, `typed_record_field_type/1`.
""".
-spec typed_record_field(syntaxTree(), syntaxTree()) -> syntaxTree().

typed_record_field(Field, Type) ->
    tree(typed_record_field,
         #typed_record_field{body = Field, type = Type}).


-doc """
Returns the field subtree of a `typed_record_field` node.

_See also: _`typed_record_field/2`.
""".
-spec typed_record_field_body(syntaxTree()) -> syntaxTree().

typed_record_field_body(Node) ->
    (data(Node))#typed_record_field.body.


-doc """
Returns the type subtree of a `typed_record_field` node.

_See also: _`typed_record_field/2`.
""".
-spec typed_record_field_type(syntaxTree()) -> syntaxTree().

typed_record_field_type(Node) ->
    (data(Node))#typed_record_field.type.


%% =====================================================================

-record(list_comp, {template :: syntaxTree(), body :: [syntaxTree()]}).

-doc """
Creates an abstract list comprehension.

If `Body` is `[E1, ..., En]`, the result represents "`[Template ||
E1, ..., En]`".

_See also: _`generator/2`, `list_comp_body/1`, `list_comp_template/1`.
""".
-spec list_comp(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {lc, Pos, Template, Body}
%%
%%	Template = erl_parse()
%%	Body = [erl_parse()] \ []

list_comp(Template, Body) ->
    tree(list_comp, #list_comp{template = Template, body = Body}).

revert_list_comp(Node) ->
    Pos = get_pos(Node),
    Template = list_comp_template(Node),
    Body = list_comp_body(Node),
    {lc, Pos, Template, Body}.


-doc """
Returns the template subtree of a `list_comp` node.

_See also: _`list_comp/2`.
""".
-spec list_comp_template(syntaxTree()) -> syntaxTree().

list_comp_template(Node) ->
    case unwrap(Node) of
	{lc, _, Template, _} ->
	    Template;
	Node1 ->
	    (data(Node1))#list_comp.template
    end.


-doc """
Returns the list of body subtrees of a `list_comp` node.

_See also: _`list_comp/2`.
""".
-spec list_comp_body(syntaxTree()) -> [syntaxTree()].

list_comp_body(Node) ->
    case unwrap(Node) of
	{lc, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#list_comp.body
    end.

%% =====================================================================

-record(binary_comp, {template :: syntaxTree(), body :: [syntaxTree()]}).

-doc """
Creates an abstract binary comprehension.

If `Body` is `[E1, ..., En]`, the result represents "`<<Template ||
E1, ..., En>>`".

_See also: _`binary_comp_body/1`, `binary_comp_template/1`, `generator/2`.
""".
-spec binary_comp(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {bc, Pos, Template, Body}
%%
%%	Template = erl_parse()
%%	Body = [erl_parse()] \ []

binary_comp(Template, Body) ->
    tree(binary_comp, #binary_comp{template = Template, body = Body}).

revert_binary_comp(Node) ->
    Pos = get_pos(Node),
    Template = binary_comp_template(Node),
    Body = binary_comp_body(Node),
    {bc, Pos, Template, Body}.


-doc """
Returns the template subtree of a `binary_comp` node.

_See also: _`binary_comp/2`.
""".
-spec binary_comp_template(syntaxTree()) -> syntaxTree().

binary_comp_template(Node) ->
    case unwrap(Node) of
	{bc, _, Template, _} ->
	    Template;
	Node1 ->
	    (data(Node1))#binary_comp.template
    end.


-doc """
Returns the list of body subtrees of a `binary_comp` node.

_See also: _`binary_comp/2`.
""".
-spec binary_comp_body(syntaxTree()) -> [syntaxTree()].

binary_comp_body(Node) ->
    case unwrap(Node) of
	{bc, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#binary_comp.body
    end.

%% =====================================================================

-record(map_comp, {template :: syntaxTree(), body :: [syntaxTree()]}).

-doc """
Creates an abstract map comprehension.

If `Body` is `[E1, ..., En]`, the result represents "`#{Template ||
E1, ..., En}`".

_See also: _`generator/2`, `map_comp_body/1`, `map_comp_template/1`.
""".
-spec map_comp(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {mc, Pos, Template, Body}
%%
%%	Template = erl_parse()
%%	Body = [erl_parse()] \ []

map_comp(Template, Body) ->
    tree(map_comp, #map_comp{template = Template, body = Body}).

revert_map_comp(Node) ->
    Pos = get_pos(Node),
    Template = map_comp_template(Node),
    Body = map_comp_body(Node),
    {mc, Pos, Template, Body}.


-doc """
Returns the template subtree of a `map_comp` node.

_See also: _`map_comp/2`.
""".
-spec map_comp_template(syntaxTree()) -> syntaxTree().

map_comp_template(Node) ->
    case unwrap(Node) of
	{mc, _, Template, _} ->
	    Template;
	Node1 ->
	    (data(Node1))#map_comp.template
    end.


-doc """
Returns the list of body subtrees of a `map_comp` node.

_See also: _`map_comp/2`.
""".
-spec map_comp_body(syntaxTree()) -> [syntaxTree()].

map_comp_body(Node) ->
    case unwrap(Node) of
	{mc, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#map_comp.body
    end.

%% =====================================================================

-record(generator, {pattern :: syntaxTree(), body :: syntaxTree()}).

-doc """
Creates an abstract list generator.

The result represents "`Pattern <- Body`".

_See also: _`binary_comp/2`, `generator_body/1`, `generator_pattern/1`,
`map_comp/2`, `list_comp/2`.
""".
-spec generator(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {generate, Pos, Pattern, Body}
%%
%%	Pattern = Body = erl_parse()

generator(Pattern, Body) ->
    tree(generator, #generator{pattern = Pattern, body = Body}).

revert_generator(Node) ->
    Pos = get_pos(Node),
    Pattern = generator_pattern(Node),
    Body = generator_body(Node),
    {generate, Pos, Pattern, Body}.


-doc """
Returns the pattern subtree of a `generator` node.

_See also: _`generator/2`.
""".
-spec generator_pattern(syntaxTree()) -> syntaxTree().

generator_pattern(Node) ->
    case unwrap(Node) of
	{generate, _, Pattern, _} ->
	    Pattern;
	Node1 ->
	    (data(Node1))#generator.pattern
    end.


-doc """
Returns the body subtree of a `generator` node.

_See also: _`generator/2`.
""".
-spec generator_body(syntaxTree()) -> syntaxTree().

generator_body(Node) ->
    case unwrap(Node) of
	{generate, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#generator.body
    end.


%% =====================================================================

-record(strict_generator, {pattern :: syntaxTree(), body :: syntaxTree()}).

-doc """
Creates an abstract strict list generator.

The result represents "`*Pattern*<:- *Body*`".

_See also: _`binary_comp/2`, `strict_generator_body/1`,
`strict_generator_pattern/1`, `list_comp/2`.
""".
-spec strict_generator(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {generate_strict, Pos, Pattern, Body}
%%
%%	Pattern = Body = erl_parse()

strict_generator(Pattern, Body) ->
    tree(strict_generator, #strict_generator{pattern = Pattern, body = Body}).

revert_strict_generator(Node) ->
    Pos = get_pos(Node),
    Pattern = strict_generator_pattern(Node),
    Body = strict_generator_body(Node),
    {generate_strict, Pos, Pattern, Body}.


-doc """
Returns the pattern subtree of a `generator` node.

_See also: _`strict_generator/2`.
""".
-spec strict_generator_pattern(syntaxTree()) -> syntaxTree().

strict_generator_pattern(Node) ->
    case unwrap(Node) of
	{generate_strict, _, Pattern, _} ->
	    Pattern;
	Node1 ->
	    (data(Node1))#strict_generator.pattern
    end.


-doc """
Returns the body subtree of a `generator` node.

_See also: _`strict_generator/2`.
""".
-spec strict_generator_body(syntaxTree()) -> syntaxTree().

strict_generator_body(Node) ->
    case unwrap(Node) of
	{generate_strict, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#strict_generator.body
    end.


%% =====================================================================

-record(binary_generator, {pattern :: syntaxTree(), body :: syntaxTree()}).

-doc """
Creates an abstract binary_generator.

The result represents "`Pattern <= Body`".

_See also: _`binary_comp/2`, `binary_generator_body/1`,
`binary_generator_pattern/1`, `list_comp/2`, `map_comp/2`.
""".
-spec binary_generator(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {b_generate, Pos, Pattern, Body}
%%
%%	Pattern = Body = erl_parse()

binary_generator(Pattern, Body) ->
    tree(binary_generator, #binary_generator{pattern = Pattern, body = Body}).

revert_binary_generator(Node) ->
    Pos = get_pos(Node),
    Pattern = binary_generator_pattern(Node),
    Body = binary_generator_body(Node),
    {b_generate, Pos, Pattern, Body}.


-doc """
Returns the pattern subtree of a `generator` node.

_See also: _`binary_generator/2`.
""".
-spec binary_generator_pattern(syntaxTree()) -> syntaxTree().

binary_generator_pattern(Node) ->
    case unwrap(Node) of
	{b_generate, _, Pattern, _} ->
	    Pattern;
	Node1 ->
	    (data(Node1))#binary_generator.pattern
    end.


-doc """
Returns the body subtree of a `generator` node.

_See also: _`binary_generator/2`.
""".
-spec binary_generator_body(syntaxTree()) -> syntaxTree().

binary_generator_body(Node) ->
    case unwrap(Node) of
	{b_generate, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#binary_generator.body
    end.


%% =====================================================================

-record(strict_binary_generator, {pattern :: syntaxTree(), body :: syntaxTree()}).

-doc """
Creates an abstract strict binary_generator.

The result represents "`*Pattern*<:- *Body*`".

_See also: _`binary_comp/2`, `strict_binary_generator_body/1`,
`strict_binary_generator_pattern/1`, `list_comp/2`.
""".
-spec strict_binary_generator(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {b_generate_strict, Pos, Pattern, Body}
%%
%%	Pattern = Body = erl_parse()

strict_binary_generator(Pattern, Body) ->
    tree(strict_binary_generator, #strict_binary_generator{pattern = Pattern, body = Body}).

revert_strict_binary_generator(Node) ->
    Pos = get_pos(Node),
    Pattern = strict_binary_generator_pattern(Node),
    Body = strict_binary_generator_body(Node),
    {b_generate_strict, Pos, Pattern, Body}.


-doc """
Returns the pattern subtree of a `generator` node.

_See also: _`strict_binary_generator/2`.
""".
-spec strict_binary_generator_pattern(syntaxTree()) -> syntaxTree().

strict_binary_generator_pattern(Node) ->
    case unwrap(Node) of
	{b_generate_strict, _, Pattern, _} ->
	    Pattern;
	Node1 ->
	    (data(Node1))#strict_binary_generator.pattern
    end.


-doc """
Returns the body subtree of a `generator` node.

_See also: _`strict_binary_generator/2`.
""".
-spec strict_binary_generator_body(syntaxTree()) -> syntaxTree().

strict_binary_generator_body(Node) ->
    case unwrap(Node) of
	{b_generate_strict, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#strict_binary_generator.body
    end.


%% =====================================================================

-record(map_generator, {pattern :: syntaxTree(), body :: syntaxTree()}).

-doc """
Creates an abstract map_generator.

The result represents "`Pattern <- Body`".

_See also: _`binary_comp/2`, `list_comp/2`, `map_comp/2`, `map_generator_body/1`,
`map_generator_pattern/1`.
""".
-spec map_generator(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {m_generate, Pos, Pattern, Body}
%%
%%	Pattern = Body = erl_parse()

map_generator(Pattern, Body) ->
    tree(map_generator, #map_generator{pattern = Pattern, body = Body}).

revert_map_generator(Node) ->
    Pos = get_pos(Node),
    Pattern = map_generator_pattern(Node),
    Body = map_generator_body(Node),
    {m_generate, Pos, Pattern, Body}.


-doc """
Returns the pattern subtree of a `map_generator` node.

_See also: _`map_generator/2`.
""".
-spec map_generator_pattern(syntaxTree()) -> syntaxTree().

map_generator_pattern(Node) ->
    case unwrap(Node) of
	{m_generate, _, Pattern, _} ->
	    Pattern;
	Node1 ->
	    (data(Node1))#map_generator.pattern
    end.


-doc """
Returns the body subtree of a `map_generator` node.

_See also: _`map_generator/2`.
""".
-spec map_generator_body(syntaxTree()) -> syntaxTree().

map_generator_body(Node) ->
    case unwrap(Node) of
	{m_generate, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#map_generator.body
    end.


%% =====================================================================

-record(strict_map_generator, {pattern :: syntaxTree(), body :: syntaxTree()}).

-doc """
Creates an abstract strict map_generator. The result represents
"`*Pattern*<- *Body*`".

_See also: _`list_comp/2`, `map_comp/2`,
`strict_map_generator_body/1`,
`strict_map_generator_pattern/1`.
""".
-spec strict_map_generator(syntaxTree(), syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {m_generate_strict, Pos, Pattern, Body}
%%
%%	Pattern = Body = erl_parse()

strict_map_generator(Pattern, Body) ->
    tree(strict_map_generator, #strict_map_generator{pattern = Pattern, body = Body}).

revert_strict_map_generator(Node) ->
    Pos = get_pos(Node),
    Pattern = strict_map_generator_pattern(Node),
    Body = strict_map_generator_body(Node),
    {m_generate_strict, Pos, Pattern, Body}.


-doc """
Returns the pattern subtree of a `generator` node.

_See also: _`strict_map_generator/2`.
""".
-spec strict_map_generator_pattern(syntaxTree()) -> syntaxTree().

strict_map_generator_pattern(Node) ->
    case unwrap(Node) of
	{m_generate_strict, _, Pattern, _} ->
	    Pattern;
	Node1 ->
	    (data(Node1))#strict_map_generator.pattern
    end.


-doc """
Returns the body subtree of a `generator` node.

_See also: _`strict_map_generator/2`.
""".
-spec strict_map_generator_body(syntaxTree()) -> syntaxTree().

strict_map_generator_body(Node) ->
    case unwrap(Node) of
	{m_generate_strict, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#strict_map_generator.body
    end.


-record(zip_generator, {body :: [syntaxTree()]}).

-doc """
Creates an abstract zip_generator.

The result represents `G1 && ... Gn`, where each `G` is a generator.

_See also: _`binary_comp/2`, `list_comp/2`, `map_comp/2`, `map_generator_body/1`,
`map_generator_pattern/1`.
""".
-spec zip_generator([syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {zip, Pos, Body}
%%
%%	Body = erl_parse()

zip_generator(Body) ->
    tree(zip_generator, #zip_generator{body = Body}).

revert_zip_generator(Node) ->
    Pos = get_pos(Node),
    Body = zip_generator_body(Node),
    {zip, Pos, Body}.


-doc """
Returns the body subtree of a `zip_generator` node.

_See also: _`zip_generator/1`.
""".
-spec zip_generator_body(syntaxTree()) -> syntaxTree().

zip_generator_body(Node) ->
    case unwrap(Node) of
	{zip, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#zip_generator.body
    end.

%% =====================================================================

-doc """
Creates an abstract block expression.

If `Body` is `[B1, ..., Bn]`, the result represents "`begin B1, ...,
Bn end`".

_See also: _`block_expr_body/1`.
""".
-spec block_expr([syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {block, Pos, Body}
%%
%%	    Body = [erl_parse()] \ []

block_expr(Body) ->
    tree(block_expr, Body).

revert_block_expr(Node) ->
    Pos = get_pos(Node),
    Body = block_expr_body(Node),
    {block, Pos, Body}.


-doc """
Returns the list of body subtrees of a `block_expr` node.

_See also: _`block_expr/1`.
""".
-spec block_expr_body(syntaxTree()) -> [syntaxTree()].

block_expr_body(Node) ->
    case unwrap(Node) of
	{block, _, Body} ->
	    Body;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================

-doc """
Creates an abstract if-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`if C1; ...;
Cn end`". More exactly, if each `Ci` represents "`() Gi -> Bi`",
then the result represents "`if G1 -> B1; ...; Gn -> Bn end`".

_See also: _`case_expr/2`, `clause/3`, `if_expr_clauses/1`.
""".
-spec if_expr([syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {'if', Pos, Clauses}
%%
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%
%%	See `clause' for documentation on `erl_parse' clauses.

if_expr(Clauses) ->
    tree(if_expr, Clauses).

revert_if_expr(Node) ->
    Pos = get_pos(Node),
    Clauses = [revert_clause(C) || C <- if_expr_clauses(Node)],
    {'if', Pos, Clauses}.


-doc """
Returns the list of clause subtrees of an `if_expr` node.

_See also: _`if_expr/1`.
""".
-spec if_expr_clauses(syntaxTree()) -> [syntaxTree()].

if_expr_clauses(Node) ->
    case unwrap(Node) of
	{'if', _, Clauses} ->
	    Clauses;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================

-record(case_expr, {argument :: syntaxTree(), clauses :: [syntaxTree()]}).

-doc """
Creates an abstract case-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`case
Argument of C1; ...; Cn end`". More exactly, if each `Ci`
represents "`(Pi) Gi -> Bi`", then the result represents "`case
Argument of P1G1 -> B1; ...; PnGn -> Bn end`".

_See also: _`case_expr_argument/1`, `case_expr_clauses/1`, `clause/3`,
`if_expr/1`.
""".
-spec case_expr(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {'case', Pos, Argument, Clauses}
%%
%%	Argument = erl_parse()
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%
%%	See `clause' for documentation on `erl_parse' clauses.

case_expr(Argument, Clauses) ->
    tree(case_expr, #case_expr{argument = Argument,
			       clauses = Clauses}).

revert_case_expr(Node) ->
    Pos = get_pos(Node),
    Argument = case_expr_argument(Node),
    Clauses = [revert_clause(C) || C <- case_expr_clauses(Node)],
    {'case', Pos, Argument, Clauses}.


-doc """
Returns the argument subtree of a `case_expr` node.

_See also: _`case_expr/2`.
""".
-spec case_expr_argument(syntaxTree()) -> syntaxTree().

case_expr_argument(Node) ->
    case unwrap(Node) of
	{'case', _, Argument, _} ->
	    Argument;
	Node1 ->
	    (data(Node1))#case_expr.argument
    end.


-doc """
Returns the list of clause subtrees of a `case_expr` node.

_See also: _`case_expr/2`.
""".
-spec case_expr_clauses(syntaxTree()) -> [syntaxTree()].

case_expr_clauses(Node) ->
    case unwrap(Node) of
	{'case', _, _, Clauses} ->
	    Clauses;
	Node1 ->
	    (data(Node1))#case_expr.clauses
    end.

%% =====================================================================

-doc """
Creates an abstract else-expression.

If `Clauses` is `[C1, ..., Cn]`, the result
represents "`else C1; ...; Cn end`". More exactly, if each `Ci` represents
"`(Pi) Gi -> Bi`", then the result represents
"`else (P1) G1 -> B1; ...; (Pn) Gn -> Bn end`".

_See also: _`clause/3`, `else_expr_clauses/1`, `maybe_expr/2`.
""".
-spec else_expr([syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {'pos', Pos, Clauses}
%%
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%
%%	See `clause' for documentation on `erl_parse' clauses.

else_expr(Clauses) ->
    tree(else_expr, Clauses).

revert_else_expr(Node) ->
    Pos = get_pos(Node),
    Clauses = else_expr_clauses(Node),
    {'else', Pos, Clauses}.

-doc """
Returns the list of clause subtrees of an `else_expr` node.

_See also: _`else_expr/1`.
""".
-spec else_expr_clauses(syntaxTree()) -> [syntaxTree()].

else_expr_clauses(Node) ->
    case unwrap(Node) of
	{'else', _, Clauses} ->
	    Clauses;
	Node1 ->
	    data(Node1)
    end.

%% =====================================================================

-doc #{equiv => maybe_expr(Body, none)}.
-spec maybe_expr([syntaxTree()]) -> syntaxTree().

maybe_expr(Body) ->
    maybe_expr(Body, none).

-record(maybe_expr, {body :: [syntaxTree()],
                     'else' = none :: 'none' | syntaxTree()}).

%% `erl_parse' representation:
%%
%% {block, Pos, Body}
%% {block, Pos, Body, Else}
%%
%%    Body = [erl_parse()] \ []
%%    Else = {'else', Pos, Clauses}
%%    Clauses = [Clause] \ []
%%    Clause = {clause, ...}

-doc """
Creates an abstract maybe-expression.

If `Body` is `[B1, ..., Bn]`, and `OptionalElse` is `none`, the result
represents "`maybe B1, ..., Bn end`".  If `Body` is `[B1, ...,
Bn]`, and `OptionalElse` reprsents an `else_expr` node with clauses
`[C1, ..., Cn]`, the result represents "`maybe B1, ..., Bn else
C1; ..., Cn end`".

See `clause` for documentation on `m:erl_parse` clauses.

_See also: _`maybe_expr_body/1`, `maybe_expr_else/1`.
""".
-spec maybe_expr([syntaxTree()], 'none' | syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {'maybe', Pos, Body}
%% {'maybe', Pos, Body, Else}
%%
%%    Body = [erl_parse()] \ []
%%    Else = {'else', Pos, Clauses}
%%    Body = erl_parse()

maybe_expr(Body, OptionalElse) ->
    tree(maybe_expr, #maybe_expr{body = Body,
                                 'else' = OptionalElse}).
revert_maybe_expr(Node) ->
    Pos = get_pos(Node),
    Body = maybe_expr_body(Node),
    case maybe_expr_else(Node) of
        none ->
            {'maybe', Pos, Body};
        Else ->
            {'maybe', Pos, Body, Else}
    end.

-doc """
Returns the list of body subtrees of a `maybe_expr` node.

_See also: _`maybe_expr/2`.
""".
-spec maybe_expr_body(syntaxTree()) -> [syntaxTree()].

maybe_expr_body(Node) ->
    case unwrap(Node) of
	{'maybe', _, Body} ->
            Body;
	{'maybe', _, Body, _Else} ->
            Body;
        Node1 ->
            (data(Node1))#maybe_expr.body
    end.

-doc """
Returns the else subtree of a `maybe_expr` node.

_See also: _`maybe_expr/2`.
""".
-spec maybe_expr_else(syntaxTree()) -> 'none' | syntaxTree().

maybe_expr_else(Node) ->
    case unwrap(Node) of
        {'maybe', _, _Body} ->
            none;
        {'maybe', _, _Body, Else} ->
            Else;
        Node1 ->
            (data(Node1))#maybe_expr.'else'
    end.

%% =====================================================================

-doc #{equiv => receive_expr(Clauses, none, [])}.
-spec receive_expr([syntaxTree()]) -> syntaxTree().

receive_expr(Clauses) ->
    receive_expr(Clauses, none, []).


-record(receive_expr, {clauses :: [syntaxTree()],
		       timeout :: 'none' | syntaxTree(),
		       action  :: [syntaxTree()]}).

-doc """
Creates an abstract receive-expression.

If `Timeout` is `none`, the result represents "`receive C1; ...;
Cn end`" (the `Action` argument is ignored).  Otherwise, if
`Clauses` is `[C1, ..., Cn]` and `Action` is `[A1, ..., Am]`, the
result represents "`receive C1; ...; Cn after Timeout -> A1,
..., Am end`". More exactly, if each `Ci` represents "`(Pi) Gi
-> Bi`", then the result represents "`receive P1 G1 -> B1; ...;
Pn Gn -> Bn ... end`".

Note that in Erlang, a receive-expression must have at least one clause if no
timeout part is specified.

_See also: _`case_expr/2`, `clause/3`, `receive_expr/1`,
`receive_expr_action/1`, `receive_expr_clauses/1`, `receive_expr_timeout/1`.
""".
-spec receive_expr([syntaxTree()], 'none' | syntaxTree(), [syntaxTree()]) ->
        syntaxTree().

%% `erl_parse' representation:
%%
%% {'receive', Pos, Clauses}
%% {'receive', Pos, Clauses, Timeout, Action}
%%
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%	Timeout = erl_parse()
%%	Action = [erl_parse()] \ []
%%
%%	See `clause' for documentation on `erl_parse' clauses.

receive_expr(Clauses, Timeout, Action) ->
    %% If `Timeout' is `none', we always replace the actual
    %% `Action' argument with an empty list, since
    %% `receive_expr_action' should in that case return the empty
    %% list regardless.
    Action1 = case Timeout of
		  none -> [];
		  _ -> Action
	      end,
    tree(receive_expr, #receive_expr{clauses = Clauses,
				     timeout = Timeout,
				     action = Action1}).

revert_receive_expr(Node) ->
    Pos = get_pos(Node),
    Clauses = [revert_clause(C) || C <- receive_expr_clauses(Node)],
    Timeout = receive_expr_timeout(Node),
    Action = receive_expr_action(Node),
    case Timeout of
	none ->
	    {'receive', Pos, Clauses};
	_ ->
	    {'receive', Pos, Clauses, Timeout, Action}
    end.


-doc """
Returns the list of clause subtrees of a `receive_expr` node.

_See also: _`receive_expr/3`.
""".
-spec receive_expr_clauses(syntaxTree()) -> [syntaxTree()].

receive_expr_clauses(Node) ->
    case unwrap(Node) of
	{'receive', _, Clauses} ->
	    Clauses;
	{'receive', _, Clauses, _, _} ->
	    Clauses;
	Node1 ->
	    (data(Node1))#receive_expr.clauses
    end.


-doc """
Returns the timeout subtree of a `receive_expr` node, if any.

If `Node` represents "`receive C1; ...; Cn end`", `none` is
returned. Otherwise, if `Node` represents "`receive C1; ...; Cn
after Timeout -> ... end`", `Timeout` is returned.

_See also: _`receive_expr/3`.
""".
-spec receive_expr_timeout(syntaxTree()) -> 'none' | syntaxTree().

receive_expr_timeout(Node) ->
    case unwrap(Node) of
	{'receive', _, _} ->
	    none;
	{'receive', _, _, Timeout, _} ->
	    Timeout;
	Node1 ->
	    (data(Node1))#receive_expr.timeout
    end.


-doc """
Returns the list of action body subtrees of a `receive_expr` node.

If `Node` represents "`receive C1; ...; Cn end`", this is the
empty list.

_See also: _`receive_expr/3`.
""".
-spec receive_expr_action(syntaxTree()) -> [syntaxTree()].

receive_expr_action(Node) ->
    case unwrap(Node) of
	{'receive', _, _} ->
	    [];
	{'receive', _, _, _, Action} ->
	    Action;
	Node1 ->
	    (data(Node1))#receive_expr.action
    end.


%% =====================================================================

-doc #{equiv => try_expr(Body, [], Handlers)}.
-spec try_expr([syntaxTree()], [syntaxTree()]) -> syntaxTree().

try_expr(Body, Handlers) ->
    try_expr(Body, [], Handlers).


%% =====================================================================

-doc #{equiv => try_expr(Body, Clauses, Handlers, [])}.
-spec try_expr([syntaxTree()], [syntaxTree()], [syntaxTree()]) -> syntaxTree().

try_expr(Body, Clauses, Handlers) ->
    try_expr(Body, Clauses, Handlers, []).


%% =====================================================================

-doc #{equiv => try_expr(Body, [], [], After)}.
-spec try_after_expr([syntaxTree()], [syntaxTree()]) -> syntaxTree().

try_after_expr(Body, After) ->
    try_expr(Body, [], [], After).


-record(try_expr, {body     :: [syntaxTree()],
		   clauses  :: [syntaxTree()],
		   handlers :: [syntaxTree()],
		   'after'  :: [syntaxTree()]}).

-doc """
Creates an abstract try-expression.

If `Body` is `[B1, ..., Bn]`, `Clauses` is `[C1, ..., Cj]`, `Handlers`
is `[H1, ..., Hk]`, and `After` is `[A1, ..., Am]`, the result
represents "`try B1, ..., Bn of C1; ...; Cj catch H1; ...; Hk after
A1, ..., Am end`".  More exactly, if each `Ci` represents "`(CPi) CGi
-> CBi`", and each `Hi` represents "`(HPi) HGi -> HBi`", then the
result represents "`try B1, ..., Bn of CP1 CG1 -> CB1; ...; CPj CGj ->
CBj catch HP1 HG1 -> HB1; ...; HPk HGk -> HBk after A1, ..., Am end`";
see `case_expr/2`. If `Clauses` is the empty list, the `of ...`
section is left out. If `After` is the empty list, the `after ...`
section is left out. If `Handlers` is the empty list, and `After` is
nonempty, the `catch ...` section is left out.

_See also: _`case_expr/2`, `class_qualifier/2`, `clause/3`, `try_after_expr/2`,
`try_expr/2`, `try_expr/3`, `try_expr_after/1`, `try_expr_body/1`,
`try_expr_clauses/1`, `try_expr_handlers/1`.
""".
-spec try_expr([syntaxTree()], [syntaxTree()],
	       [syntaxTree()], [syntaxTree()]) -> syntaxTree().

%% {'try', Pos, Body, Clauses, Handlers, After}
%%
%%	Body = [erl_parse()]
%%	Clauses = [Clause]
%%	Handlers = [Clause] \ []
%%	Clause = {clause, ...}
%%	After = [erl_parse()]
%%
%%	See `clause' for documentation on `erl_parse' clauses.

try_expr(Body, Clauses, Handlers, After) ->
    tree(try_expr, #try_expr{body = Body,
			     clauses = Clauses,
			     handlers = Handlers,
			     'after' = After}).

revert_try_expr(Node) ->
    Pos = get_pos(Node),
    Body = try_expr_body(Node),
    Clauses = [revert_clause(C) || C <- try_expr_clauses(Node)],
    Handlers = [revert_try_clause(C) || C <- try_expr_handlers(Node)],
    After = try_expr_after(Node),
    {'try', Pos, Body, Clauses, Handlers, After}.


-doc """
Returns the list of body subtrees of a `try_expr` node.

_See also: _`try_expr/4`.
""".
-spec try_expr_body(syntaxTree()) -> [syntaxTree()].

try_expr_body(Node) ->
    case unwrap(Node) of
	{'try', _, Body, _, _, _} ->
	    Body;
	Node1 ->
	    (data(Node1))#try_expr.body
    end.


-doc """
Returns the list of case-clause subtrees of a `try_expr` node. If `Node`
represents "`try Body catch H1; ...; Hn end`", the result is the empty
list.

_See also: _`try_expr/4`.
""".
-spec try_expr_clauses(syntaxTree()) -> [syntaxTree()].

try_expr_clauses(Node) ->
    case unwrap(Node) of
	{'try', _, _, Clauses, _, _} ->
	    Clauses;
	Node1 ->
	    (data(Node1))#try_expr.clauses
    end.


-doc """
Returns the list of handler-clause subtrees of a `try_expr` node.

_See also: _`try_expr/4`.
""".
-spec try_expr_handlers(syntaxTree()) -> [syntaxTree()].

try_expr_handlers(Node) ->
    case unwrap(Node) of
	{'try', _, _, _, Handlers, _} ->
	    unfold_try_clauses(Handlers);
	Node1 ->
	    (data(Node1))#try_expr.handlers
    end.


-doc """
Returns the list of "after" subtrees of a `try_expr` node.

_See also: _`try_expr/4`.
""".
-spec try_expr_after(syntaxTree()) -> [syntaxTree()].

try_expr_after(Node) ->
    case unwrap(Node) of
	{'try', _, _, _, _, After} ->
	    After;
	Node1 ->
	    (data(Node1))#try_expr.'after'
    end.


%% =====================================================================

-record(class_qualifier, {class :: syntaxTree(),
                          body :: syntaxTree(),
                          stacktrace :: syntaxTree()}).

-doc """
Creates an abstract class qualifier.

The result represents "`Class:Body`".

_See also: _`class_qualifier_argument/1`, `class_qualifier_body/1`,
`class_qualifier_stacktrace/1`, `try_expr/4`.
""".
-spec class_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().

class_qualifier(Class, Body) ->
    Underscore = {var, get_pos(Body), '_'},
    tree(class_qualifier,
	 #class_qualifier{class = Class, body = Body,
                          stacktrace = Underscore}).

-doc """
Creates an abstract class qualifier.

The result represents "`Class:Body:Stacktrace`".

_See also: _`class_qualifier_argument/1`, `class_qualifier_body/1`,
`try_expr/4`.
""".
-spec class_qualifier(syntaxTree(), syntaxTree(), syntaxTree()) ->
                             syntaxTree().

class_qualifier(Class, Body, Stacktrace) ->
    tree(class_qualifier,
	 #class_qualifier{class = Class,
                          body = Body,
                          stacktrace = Stacktrace}).


-doc """
Returns the argument (the class) subtree of a `class_qualifier` node.

_See also: _`class_qualifier/2`.
""".
-spec class_qualifier_argument(syntaxTree()) -> syntaxTree().

class_qualifier_argument(Node) ->
    (data(Node))#class_qualifier.class.


-doc """
Returns the body subtree of a `class_qualifier` node.

_See also: _`class_qualifier/2`.
""".
-spec class_qualifier_body(syntaxTree()) -> syntaxTree().

class_qualifier_body(Node) ->
    (data(Node))#class_qualifier.body.

-doc """
Returns the stacktrace subtree of a `class_qualifier` node.

_See also: _`class_qualifier/2`.
""".
-spec class_qualifier_stacktrace(syntaxTree()) -> syntaxTree().

class_qualifier_stacktrace(Node) ->
    (data(Node))#class_qualifier.stacktrace.


-doc """
implicit_fun(Name, Arity)

Creates an abstract "implicit fun" expression.

If `Arity` is `none`, this is equivalent to
[`implicit_fun(Name)`](`implicit_fun/1`), otherwise it is equivalent
to [`implicit_fun(arity_qualifier(Name, Arity))`](`implicit_fun/1`).

(This is a utility function.)

_See also: _`implicit_fun/1`, `implicit_fun/3`.
""".
-spec implicit_fun(syntaxTree(), 'none' | syntaxTree()) -> syntaxTree().

implicit_fun(Name, none) ->
    implicit_fun(Name);
implicit_fun(Name, Arity) ->
    implicit_fun(arity_qualifier(Name, Arity)).


-doc """
implicit_fun(Module, Name, Arity)

Creates an abstract module-qualified "implicit fun" expression.

If `Module` is `none`, this is equivalent to [`implicit_fun(Name,
Arity)`](`implicit_fun/2`), otherwise it is equivalent to
`implicit_fun(module_qualifier(Module, arity_qualifier(Name, Arity))`.

(This is a utility function.)

_See also: _`implicit_fun/1`, `implicit_fun/2`.
""".
-spec implicit_fun('none' | syntaxTree(), syntaxTree(), syntaxTree()) ->
        syntaxTree().

implicit_fun(none, Name, Arity) ->
    implicit_fun(Name, Arity);
implicit_fun(Module, Name, Arity) ->
    implicit_fun(module_qualifier(Module, arity_qualifier(Name, Arity))).


-doc """
Creates an abstract "implicit fun" expression.

The result represents "`fun Name`". `Name` should represent either
`F/A` or `M:F/A`

_See also: _`arity_qualifier/2`, `implicit_fun/2`, `implicit_fun/3`,
`implicit_fun_name/1`, `module_qualifier/2`.
""".
-spec implicit_fun(syntaxTree()) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {'fun', Pos, {function, Name, Arity}}
%% {'fun', Pos, {function, Module, Name, Arity}}
%%
%%	Module = atom()
%%	Name = atom()
%%	Arity = arity()

implicit_fun(Name) ->
    tree(implicit_fun, Name).

revert_implicit_fun(Node) ->
    Pos = get_pos(Node),
    Name = implicit_fun_name(Node),
    case type(Name) of
	arity_qualifier ->
	    F = arity_qualifier_body(Name),
	    A = arity_qualifier_argument(Name),
	    case {type(F), type(A)} of
		{atom, integer} ->
		    {'fun', Pos,
		     {function, concrete(F), concrete(A)}};
		_ ->
		    Node
	    end;
	module_qualifier ->
	    M = module_qualifier_argument(Name),
	    Name1 = module_qualifier_body(Name),
	    case type(Name1) of
		arity_qualifier ->
		    F = arity_qualifier_body(Name1),
		    A = arity_qualifier_argument(Name1),
		    {'fun', Pos, {function, M, F, A}};
		_ ->
		    Node
	    end;
	_ ->
	    Node
    end.


-doc """
Returns the name subtree of an `implicit_fun` node.

If `Node` represents "`fun N/A`" or "`fun M:N/A`", then the
result is the subtree representing "`N/A`" or "`M:N/A`", respectively.

_See also: _`arity_qualifier/2`, `implicit_fun/1`, `module_qualifier/2`.
""".
-spec implicit_fun_name(syntaxTree()) -> syntaxTree().

implicit_fun_name(Node) ->
    case unwrap(Node) of
	{'fun', Pos, {function, Atom, Arity}} ->
	    arity_qualifier(set_pos(atom(Atom), Pos),
			    set_pos(integer(Arity), Pos));
	{'fun', _Pos, {function, Module, Atom, Arity}} ->
	    %% XXX: Perhaps set position for this as well?
	    module_qualifier(Module, arity_qualifier(Atom, Arity));
	Node1 ->
	    data(Node1)
    end.


-doc """
Creates an abstract fun-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`fun C1;
...; Cn end`". More exactly, if each `Ci` represents "`(Pi1, ...,
Pim) Gi -> Bi`", then the result represents "`fun (P11, ...,
P1m) G1 -> B1; ...; (Pn1, ..., Pnm) Gn -> Bn end`".

_See also: _`fun_expr_arity/1`, `fun_expr_clauses/1`.
""".
-spec fun_expr([syntaxTree()]) -> syntaxTree().

%% (See `function' for notes; e.g. why the arity is not stored.)
%%
%% `erl_parse' representation:
%%
%% {'fun', Pos, {clauses, Clauses}}
%%
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%
%%	See `clause' for documentation on `erl_parse' clauses.

fun_expr(Clauses) ->
    tree(fun_expr, Clauses).

revert_fun_expr(Node) ->
    Clauses = [revert_clause(C) || C <- fun_expr_clauses(Node)],
    Pos = get_pos(Node),
    {'fun', Pos, {clauses, Clauses}}.


-doc """
Returns the list of clause subtrees of a `fun_expr` node.

_See also: _`fun_expr/1`.
""".
-spec fun_expr_clauses(syntaxTree()) -> [syntaxTree()].

fun_expr_clauses(Node) ->
    case unwrap(Node) of
	{'fun', _, {clauses, Clauses}} ->
	    Clauses;
	Node1 ->
	    data(Node1)
    end.


-doc """
Returns the arity of a `fun_expr` node.

The result is the number of parameter patterns in the first clause of
the fun-expression; subsequent clauses are ignored.

An exception is thrown if [`fun_expr_clauses(Node)`](`fun_expr_clauses/1`)
returns an empty list, or if the first element of that list is not a syntax tree
`C` of type `clause` such that [`clause_patterns(C)`](`clause_patterns/1`) is a
nonempty list.

_See also: _`clause/3`, `clause_patterns/1`, `fun_expr/1`, `fun_expr_clauses/1`.
""".
-spec fun_expr_arity(syntaxTree()) -> arity().

fun_expr_arity(Node) ->
    length(clause_patterns(hd(fun_expr_clauses(Node)))).


-record(named_fun_expr, {name :: syntaxTree(), clauses :: [syntaxTree()]}).

-doc """
Creates an abstract named fun-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`fun
Name C1; ...; Name Cn end`". More exactly, if each `Ci`
represents "`(Pi1, ..., Pim) Gi -> Bi`", then the result
represents "`fun Name(P11, ..., P1m) G1 -> B1; ...;
Name(Pn1, ..., Pnm) Gn -> Bn end`".

_See also: _`named_fun_expr_arity/1`, `named_fun_expr_clauses/1`,
`named_fun_expr_name/1`.
""".
-spec named_fun_expr(syntaxTree(), [syntaxTree()]) -> syntaxTree().

%% `erl_parse' representation:
%%
%% {named_fun, Pos, Name, Clauses}
%%
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%
%%	See `clause' for documentation on `erl_parse' clauses.

named_fun_expr(Name, Clauses) ->
    tree(named_fun_expr, #named_fun_expr{name = Name, clauses = Clauses}).

revert_named_fun_expr(Node) ->
    Pos = get_pos(Node),
    Name = named_fun_expr_name(Node),
    Clauses = [revert_clause(C) || C <- named_fun_expr_clauses(Node)],
    case type(Name) of
	variable ->
	    {named_fun, Pos, variable_name(Name), Clauses};
	_ ->
	    Node
    end.


-doc """
Returns the name subtree of a `named_fun_expr` node.

_See also: _`named_fun_expr/2`.
""".
-spec named_fun_expr_name(syntaxTree()) -> syntaxTree().

named_fun_expr_name(Node) ->
    case unwrap(Node) of
	{named_fun, Pos, Name, _} ->
	    set_pos(variable(Name), Pos);
	Node1 ->
	    (data(Node1))#named_fun_expr.name
    end.


-doc """
Returns the list of clause subtrees of a `named_fun_expr` node.

_See also: _`named_fun_expr/2`.
""".
-spec named_fun_expr_clauses(syntaxTree()) -> [syntaxTree()].

named_fun_expr_clauses(Node) ->
    case unwrap(Node) of
	{named_fun, _, _, Clauses} ->
	    Clauses;
	Node1 ->
	    (data(Node1))#named_fun_expr.clauses
    end.


-doc """
Returns the arity of a `named_fun_expr` node.

The result is the number of parameter patterns in the first clause of
the named fun-expression; subsequent clauses are ignored.

An exception is thrown if
[`named_fun_expr_clauses(Node)`](`named_fun_expr_clauses/1`) returns an empty
list, or if the first element of that list is not a syntax tree `C` of type
`clause` such that [`clause_patterns(C)`](`clause_patterns/1`) is a nonempty
list.

_See also: _`clause/3`, `clause_patterns/1`, `named_fun_expr/2`,
`named_fun_expr_clauses/1`.
""".
-spec named_fun_expr_arity(syntaxTree()) -> arity().

named_fun_expr_arity(Node) ->
    length(clause_patterns(hd(named_fun_expr_clauses(Node)))).


-doc """
Creates an abstract parenthesised expression.

The result represents "`(Body)`", independently of the context.

_See also: _`parentheses_body/1`.
""".
-spec parentheses(syntaxTree()) -> syntaxTree().

parentheses(Expr) ->
    tree(parentheses, Expr).

revert_parentheses(Node) ->
    parentheses_body(Node).


-doc """
Returns the body subtree of a `parentheses` node.

_See also: _`parentheses/1`.
""".
-spec parentheses_body(syntaxTree()) -> syntaxTree().

parentheses_body(Node) ->
    data(Node).


%% =====================================================================

-doc #{equiv => macro(Name, none)}.
-spec macro(syntaxTree()) -> syntaxTree().

macro(Name) ->
    macro(Name, none).


-record(macro, {name :: syntaxTree(), arguments :: 'none' | [syntaxTree()]}).

-doc """
Creates an abstract macro application.

If `Arguments` is `none`, the result represents "`?Name`",
otherwise, if `Arguments` is `[A1, ..., An]`, the result represents
"`?Name(A1, ..., An)`".

Notes: if `Arguments` is the empty list, the result will thus represent
"`?Name()`", including a pair of matching parentheses.

The only syntactical limitation imposed by the preprocessor on the arguments to
a macro application (viewed as sequences of tokens) is that they must be
balanced with respect to parentheses, brackets, `begin ... end`, `case ... end`,
and so on. The `text` node type can be used to represent arguments which are not
regular Erlang constructs.

_See also: _`macro/1`, `macro_arguments/1`, `macro_name/1`, `text/1`.
""".
-spec macro(syntaxTree(), 'none' | [syntaxTree()]) -> syntaxTree().

macro(Name, Arguments) ->
    tree(macro, #macro{name = Name, arguments = Arguments}).


-doc """
Returns the name subtree of a `macro` node.

_See also: _`macro/2`.
""".
-spec macro_name(syntaxTree()) -> syntaxTree().

macro_name(Node) ->
    (data(Node))#macro.name.


-doc """
Returns the list of argument subtrees of a `macro` node, if any.

If `Node` represents "`?Name`", `none` is returned. Otherwise, if
`Node` represents "`?Name(A1, ..., An)`", `[A1, ..., An]` is
returned.

_See also: _`macro/2`.
""".
-spec macro_arguments(syntaxTree()) -> 'none' | [syntaxTree()].

macro_arguments(Node) ->
    (data(Node))#macro.arguments.


-doc """
abstract(Term)

Returns the syntax tree corresponding to an Erlang term.

`Term` must be a literal term, meaning one that can be represented as a
source code literal. Thus, it must not contain a process identifier,
port, reference, or function value as a subterm. The function
recognises printable strings, in order to get a compact and readable
representation. Evaluation fails with reason `badarg` if `Term` is not
a literal term.

_See also: _`concrete/1`, `is_literal/1`.
""".
-spec abstract(term()) -> syntaxTree().

abstract([H | T] = L) when is_integer(H) ->
    case is_printable(L) of
	true ->
	    string(L);
	false ->
	    abstract_tail(H, T)
    end;
abstract([H | T]) ->
    abstract_tail(H, T);
abstract(T) when is_atom(T) ->
    atom(T);
abstract(T) when is_integer(T) ->
    integer(T);
abstract(T) when is_float(T) ->
    make_float(T);    % (not `float', which would call the BIF)
abstract([]) ->
    nil();
abstract(T) when is_tuple(T) ->
    tuple(abstract_list(tuple_to_list(T)));
abstract(T) when is_map(T) ->
    map_expr([map_field_assoc(abstract(Key),abstract(Value))
	      || {Key,Value} <- maps:to_list(T)]);
abstract(T) when is_binary(T) ->
    binary([binary_field(integer(B)) || B <- binary_to_list(T)]);
abstract(T) when is_bitstring(T) ->
    S = bit_size(T),
    ByteS = S div 8,
    BitS = S rem 8,
    <<Bin:ByteS/binary, I:BitS>> = T,
    binary([binary_field(integer(B)) || B <- binary_to_list(Bin)]
           ++ [binary_field(integer(I), integer(BitS), [])]);
abstract(T) ->
    erlang:error({badarg, T}).

abstract_list([T | Ts]) ->
    [abstract(T) | abstract_list(Ts)];
abstract_list([]) ->
    [].

%% This is entered when we might have a sequence of conses that might or
%% might not be a proper list, but which should not be considered as a
%% potential string, to avoid unnecessary checking. This also avoids
%% that a list like `[4711, 42, 10]' could be abstracted to represent
%% `[4711 | "*\n"]'.

abstract_tail(H1, [H2 | T]) ->
    %% Recall that `cons' does "intelligent" composition
    cons(abstract(H1), abstract_tail(H2, T));
abstract_tail(H, T) ->
    cons(abstract(H), abstract(T)).


-doc """
Returns the Erlang term represented by a syntax tree.

Evaluation fails with reason `badarg` if `Node` does not represent a
literal term.

> #### Note {: .info }
>
> The set of syntax trees which have a concrete representation is larger
> than the set of trees which can be built using the function
> `abstract/1`.  An abstract character will be concretised as an
> integer, while `abstract/1` does not at present yield an abstract
> character for any input. (Use the `char/1` function to explicitly
> create an abstract character.)

> #### Note {: .info }
>
> `arity_qualifier` nodes are recognized. This is to follow the Erlang
> Parser when it comes to wild attributes: both `{F, A}` and `F/A` are
> recognized, which makes it possible to turn wild attributes into
> recognized attributes without at the same time making it impossible to
> compile files using the new syntax with the old version of the Erlang
> Compiler.

_See also: _`abstract/1`, `char/1`, `is_literal/1`.
""".
-spec concrete(syntaxTree()) -> term().

concrete(Node) ->
    case type(Node) of
	atom ->
	    atom_value(Node);
	integer ->
	    integer_value(Node);
	float ->
	    float_value(Node);
	char ->
	    char_value(Node);
	string ->
	    string_value(Node);
	nil ->
	    [];
	list ->
	    [concrete(list_head(Node))
	     | concrete(list_tail(Node))];
	tuple ->
	    list_to_tuple(concrete_list(tuple_elements(Node)));
	map_expr ->
	    As = [tuple([map_field_assoc_name(F),
			 map_field_assoc_value(F)]) || F <- map_expr_fields(Node)],
	    M0 = maps:from_list(concrete_list(As)),
	    case map_expr_argument(Node) of
		none  -> M0;
		Node0 -> maps:merge(concrete(Node0),M0)
	    end;
	binary ->
            Fs = [begin
                      B = binary_field_body(F),
                      {Body, Size} =
                          case type(B) of
                              size_qualifier ->
                                  {size_qualifier_body(B),
                                   size_qualifier_argument(B)};
                              _ ->
                                  {B, none}
                          end,
                      revert_binary_field(
                        binary_field(Body, Size, binary_field_types(F)))
                  end
                  || F <- binary_fields(Node)],
	    {value, B, _} =
		eval_bits:expr_grp(Fs, [],
				   fun(F, _) ->
					   {value, concrete(F), []}
				   end),
	    B;
        arity_qualifier ->
            A = erl_syntax:arity_qualifier_argument(Node),
            case erl_syntax:type(A) of
                integer ->
                    F = erl_syntax:arity_qualifier_body(Node),
                    case erl_syntax:type(F) of
                        atom ->
                            {F, A};
                        _ ->
                            erlang:error({badarg, Node})
                    end;
                _ ->
                    erlang:error({badarg, Node})
            end;
        _ ->
	    erlang:error({badarg, Node})
    end.

concrete_list([E | Es]) ->
    [concrete(E) | concrete_list(Es)];
concrete_list([]) ->
    [].


-doc """
Returns `true` if `Node` represents a literal term, otherwise `false`.

This function returns `true` if and only if the value of
[`concrete(Node)`](`concrete/1`) is defined.

_See also: _`abstract/1`, `concrete/1`.
""".
-spec is_literal(syntaxTree()) -> boolean().

is_literal(T) ->
    case type(T) of
	atom ->
	    true;
	integer ->
	    true;
	float ->
	    true;
	char->
	    true;
	string ->
	    true;
	nil ->
	    true;
	list ->
	    is_literal(list_head(T)) andalso is_literal(list_tail(T));
	tuple ->
	    lists:all(fun is_literal/1, tuple_elements(T));
	map_expr ->
	    case map_expr_argument(T) of
		none -> true;
		Arg  -> is_literal(Arg)
	    end andalso lists:all(fun is_literal_map_field/1, map_expr_fields(T));
	binary ->
	    lists:all(fun is_literal_binary_field/1, binary_fields(T));
	_ ->
	    false
    end.

is_literal_binary_field(F) ->
    case binary_field_types(F) of
	[] -> B = binary_field_body(F),
              case type(B) of
                  size_qualifier ->
                      is_literal(size_qualifier_body(B)) andalso
                          is_literal(size_qualifier_argument(B));
                  _ ->
                      is_literal(B)
              end;
	_  -> false
    end.

is_literal_map_field(F) ->
    case type(F) of
	map_field_assoc ->
	    is_literal(map_field_assoc_name(F)) andalso
	    is_literal(map_field_assoc_value(F));
	map_field_exact ->
	    false
    end.

-doc """
Returns an `m:erl_parse`-compatible representation of a syntax tree, if possible.

If `Tree` represents a well-formed Erlang program or expression, the conversion
should work without problems. Typically, `is_tree/1` yields `true` if conversion
failed (that is, the result is still an abstract syntax tree), and `false`
otherwise.

The `is_tree/1` test is not completely foolproof. For a few special
node types (for example `arity_qualifier`), if such a node occurs in a
context where it is not expected, it will be left unchanged as a
non-reverted subtree of the result.  This can only happen if `Tree`
does not actually represent legal Erlang code.

_See also: _[//stdlib/erl_parse](`m:erl_parse`), `revert_forms/1`.
""".
-spec revert(syntaxTree()) -> syntaxTree().

revert(Node) ->
    case Node of
	#tree{} ->
	    case is_leaf(Node) of
		true ->
		    revert_root(Node);
		false ->
		    %% First revert the subtrees, where possible.
		    %% (Sometimes, subtrees cannot be reverted out of
		    %% context, and the real work will be done when the
		    %% parent node is reverted.)
		    Gs = [[revert(X) || X <- L] || L <- subtrees(Node)],

		    %% Then reconstruct the node from the reverted
		    %% parts, and revert the node itself.
		    Node1 = update_tree(Node, Gs),
		    revert_root(Node1)
	    end;
	#wrapper{tree = Node1, attr = Attr} ->
	    %% Just remove the wrapper. The wrapped `erl_parse' nodes never
	    %% contain abstract syntax tree nodes as subtrees. Carry over
	    %% the position information, unless it is a warning/error marker
            case Node1 of
                {error, _} -> Node1;
                {warning, _} -> Node1;
                _ -> setelement(2, Node1, Attr#attr.pos)
            end;
        _ ->
            Node
    end.

%% Note: The concept of "compatible root node" is not strictly defined.
%% At a minimum, if `make_tree' is used to compose a node `T' from
%% subtrees that are all completely backwards compatible, then the
%% result of `revert_root(T)' should also be completely backwards
%% compatible.

revert_root(Node) ->
    case type(Node) of
        annotated_type ->
            revert_annotated_type(Node);
	application ->
	    revert_application(Node);
	atom ->
	    revert_atom(Node);
	attribute ->
	    revert_attribute(Node);
	binary ->
	    revert_binary(Node);
        binary_comp ->
	    revert_binary_comp(Node);
	binary_field ->
	    revert_binary_field(Node);
        binary_generator ->
	    revert_binary_generator(Node);
        bitstring_type ->
            revert_bitstring_type(Node);
	block_expr ->
	    revert_block_expr(Node);
        'case_expr' ->                          %Quoted to help Emacs.
	    revert_case_expr(Node);
        'catch_expr' ->                         %Quoted to help Emacs.
	    revert_catch_expr(Node);
	char ->
	    revert_char(Node);
	clause ->
	    revert_clause(Node);
        constrained_function_type ->
            revert_constrained_function_type(Node);
        constraint ->
            revert_constraint(Node);
        else_expr ->
            revert_else_expr(Node);
	eof_marker ->
	    revert_eof_marker(Node);
	error_marker ->
	    revert_error_marker(Node);
	float ->
	    revert_float(Node);
	fun_expr ->
	    revert_fun_expr(Node);
        fun_type ->
            revert_fun_type(Node);
	function ->
	    revert_function(Node);
        function_type ->
            revert_function_type(Node);
	generator ->
	    revert_generator(Node);
	if_expr ->
	    revert_if_expr(Node);
	implicit_fun ->
	    revert_implicit_fun(Node);
	infix_expr ->
	    revert_infix_expr(Node);
	integer ->
	    revert_integer(Node);
        integer_range_type ->
            revert_integer_range_type(Node);
	list ->
	    revert_list(Node);
	list_comp ->
	    revert_list_comp(Node);
        map_comp ->
	    revert_map_comp(Node);
        map_expr ->
            revert_map_expr(Node);
        map_field_assoc ->
            revert_map_field_assoc(Node);
        map_field_exact ->
            revert_map_field_exact(Node);
        map_generator ->
            revert_map_generator(Node);
        map_type ->
            revert_map_type(Node);
        map_type_assoc ->
            revert_map_type_assoc(Node);
        map_type_exact ->
            revert_map_type_exact(Node);
	match_expr ->
	    revert_match_expr(Node);
        maybe_match_expr ->
            revert_maybe_match_expr(Node);
        maybe_expr ->
            revert_maybe_expr(Node);
	module_qualifier ->
	    revert_module_qualifier(Node);
	named_fun_expr ->
	    revert_named_fun_expr(Node);
	nil ->
	    revert_nil(Node);
	strict_binary_generator ->
	    revert_strict_binary_generator(Node);
	strict_generator ->
	    revert_strict_generator(Node);
	strict_map_generator ->
	    revert_strict_map_generator(Node);
	parentheses ->
	    revert_parentheses(Node);
	prefix_expr ->
	    revert_prefix_expr(Node);
	receive_expr ->
	    revert_receive_expr(Node);
	record_access ->
	    revert_record_access(Node);
	record_expr ->
	    revert_record_expr(Node);
	record_index_expr ->
	    revert_record_index_expr(Node);
        record_type ->
            revert_record_type(Node);
        record_type_field ->
            revert_record_type_field(Node);
        type_application ->
            revert_type_application(Node);
        type_union ->
            revert_type_union(Node);
	string ->
	    revert_string(Node);
	try_expr ->
	    revert_try_expr(Node);
	tuple ->
	    revert_tuple(Node);
        tuple_type ->
            revert_tuple_type(Node);
	underscore ->
	    revert_underscore(Node);
        user_type_application ->
            revert_user_type_application(Node);
	variable ->
	    revert_variable(Node);
	warning_marker ->
	    revert_warning_marker(Node);
	zip_generator ->
	    revert_zip_generator(Node);
	_ ->
	    %% Non-revertible new-form node
	    Node
    end.


-type forms() :: syntaxTree() | [syntaxTree()].

-doc """
revert_forms(Forms)

Reverts a sequence of Erlang source code forms.

The sequence can be given either as a `form_list` syntax tree
(possibly nested), or as a list of "program form" syntax trees. If
successful, the corresponding flat list of `m:erl_parse`-compatible
syntax trees is returned (see `revert/1`). If some program form could
not be reverted, `{error, Form}` is thrown. Standalone comments in the
form sequence are discarded.

_See also: _`form_list/1`, `is_form/1`, `revert/1`.
""".
-spec revert_forms(forms()) -> [erl_parse()].

revert_forms(Forms) when is_list(Forms) ->
    revert_forms(form_list(Forms));
revert_forms(T) ->
    case type(T) of
	form_list ->
	    T1 = flatten_form_list(T),
	    case catch {ok, revert_forms_1(form_list_elements(T1))} of
		{ok, Fs} ->
		    Fs;
		{error, _} = Error ->
		    erlang:error(Error);
		{'EXIT', R} ->
		    exit(R);
		R ->
		    throw(R)
	    end;
	_ ->
	    erlang:error({badarg, T})
    end.

revert_forms_1([T | Ts]) ->
    case type(T) of
	comment ->
	    revert_forms_1(Ts);
	_ ->
	    T1 = revert(T),
	    case T1 of
		#tree{} ->
		    throw({error, T1});
		_ ->
		    [T1 | revert_forms_1(Ts)]
	    end
    end;
revert_forms_1([]) ->
    [].


-doc """
Returns the grouped list of all subtrees of a syntax tree.

If `Node` is a leaf node (see `is_leaf/1`), this is the empty list,
otherwise the result is always a nonempty list, containing the lists
of subtrees of `Node`, in left-to-right order as they occur in the
printed program text, and grouped by category. Often, each group
contains only a single subtree.

Depending on the type of `Node`, the size of some groups may be
variable (for example, the group consisting of all the elements of a
tuple), while others always contain the same number of elements —
usually exactly one (for example, the group containing the argument
expression of a case-expression). Note, however, that the exact
structure of the returned list (for a given node type) should in
general not be depended upon, since it might be subject to change
without notice.

The function `subtrees/1` and the constructor functions `make_tree/2` and
`update_tree/2` can be a great help if one wants to traverse a syntax tree,
visiting all its subtrees, but treat nodes of the tree in a uniform way in most
or all cases. Using these functions makes this simple, and also assures that
your code is not overly sensitive to extensions of the syntax tree data type,
because any node types not explicitly handled by your code can be left to a
default case.

For example:

```text
     postorder(F, Tree) ->
        F(case subtrees(Tree) of
            [] -> Tree;
            List -> update_tree(Tree,
                                [[postorder(F, Subtree)
                                  || Subtree &lt;- Group]
                                 || Group &lt;- List])
          end).
```

maps the function `F` on `Tree` and all its subtrees, doing a post-order
traversal of the syntax tree. (Note the use of `update_tree/2` to preserve node
attributes.) For a simple function like:

```text
     f(Node) ->
        case type(Node) of
            atom -> atom("a_" ++ atom_name(Node));
            _ -> Node
        end.
```

the call `postorder(fun f/1, Tree)` will yield a new representation of `Tree` in
which all atom names have been extended with the prefix "a_", but nothing else
(including comments, annotations, and line numbers) has been changed.

_See also: _`copy_attrs/2`, `is_leaf/1`, `make_tree/2`, `type/1`.
""".
-spec subtrees(syntaxTree()) -> [[syntaxTree()]].

subtrees(T) ->
    case is_leaf(T) of
	true ->
	    [];
	false ->
	    case type(T) of
                annotated_type ->
                    [[annotated_type_name(T)],
                     [annotated_type_body(T)]];
		application ->
		    [[application_operator(T)],
		     application_arguments(T)];
		arity_qualifier ->
		    [[arity_qualifier_body(T)],
		     [arity_qualifier_argument(T)]];
		attribute ->
		    case attribute_arguments(T) of
			none ->
			    [[attribute_name(T)]];
			As ->
			    [[attribute_name(T)], As]
		    end;
		binary ->
		    [binary_fields(T)];
		binary_comp ->
		    [[binary_comp_template(T)], binary_comp_body(T)];
                binary_field ->
		    case binary_field_types(T) of
			[] ->
			    [[binary_field_body(T)]];
			Ts ->
			    [[binary_field_body(T)],
			     Ts]
		    end;
	        binary_generator ->
		    [[binary_generator_pattern(T)],
                     [binary_generator_body(T)]];
                bitstring_type ->
                    [[bitstring_type_m(T)],
                     [bitstring_type_n(T)]];
		block_expr ->
		    [block_expr_body(T)];
		case_expr ->
		    [[case_expr_argument(T)],
		     case_expr_clauses(T)];
                'catch_expr' ->                 %Quoted to help Emacs.
		    [[catch_expr_body(T)]];
		class_qualifier ->
                    [[class_qualifier_argument(T)],
                     [class_qualifier_body(T)],
                     [class_qualifier_stacktrace(T)]];
		clause ->
		    case clause_guard(T) of
			none ->
			    [clause_patterns(T), clause_body(T)];
			G ->
			    [clause_patterns(T), [G],
			     clause_body(T)]
		    end;
		conjunction ->
		    [conjunction_body(T)];
                constrained_function_type ->
                    C = constrained_function_type_argument(T),
                    [[constrained_function_type_body(T)],
                     conjunction_body(C)];
                constraint ->
                    [[constraint_argument(T)],
                     constraint_body(T)];
		disjunction ->
		    [disjunction_body(T)];
		else_expr ->
                    [else_expr_clauses(T)];
		form_list ->
		    [form_list_elements(T)];
		fun_expr ->
		    [fun_expr_clauses(T)];
                fun_type ->
                    [];
		function ->
		    [[function_name(T)], function_clauses(T)];
                function_type ->
                    case function_type_arguments(T) of
                        any_arity ->
                            [[function_type_return(T)]];
                        As ->
                            [As,[function_type_return(T)]]
                    end;
		generator ->
		    [[generator_pattern(T)], [generator_body(T)]];
		if_expr ->
		    [if_expr_clauses(T)];
		implicit_fun ->
		    [[implicit_fun_name(T)]];
		infix_expr ->
		    [[infix_expr_left(T)],
		     [infix_expr_operator(T)],
		     [infix_expr_right(T)]];
                integer_range_type ->
                    [[integer_range_type_low(T)],
                     [integer_range_type_high(T)]];
		list ->
		    case list_suffix(T) of
			none ->
			    [list_prefix(T)];
			S ->
			    [list_prefix(T), [S]]
		    end;
		list_comp ->
		    [[list_comp_template(T)], list_comp_body(T)];
		macro ->
		    case macro_arguments(T) of
			none ->
			    [[macro_name(T)]];
			As ->
			    [[macro_name(T)], As]
		    end;
                map_comp ->
                    [[map_comp_template(T)], map_comp_body(T)];
                map_expr ->
                    case map_expr_argument(T) of
                        none ->
                            [map_expr_fields(T)];
                        V ->
                            [[V], map_expr_fields(T)]
                    end;
                map_field_assoc ->
                    [[map_field_assoc_name(T)],
                     [map_field_assoc_value(T)]];
                map_field_exact ->
                    [[map_field_exact_name(T)],
                     [map_field_exact_value(T)]];
	        map_generator ->
                    [[map_generator_pattern(T)],
                     [map_generator_body(T)]];
                map_type ->
                    [map_type_fields(T)];
                map_type_assoc ->
                    [[map_type_assoc_name(T)],
                     [map_type_assoc_value(T)]];
                map_type_exact ->
                    [[map_type_exact_name(T)],
                     [map_type_exact_value(T)]];
		match_expr ->
		    [[match_expr_pattern(T)],
		     [match_expr_body(T)]];
                maybe_expr ->
                    case maybe_expr_else(T) of
                        none ->
                            [maybe_expr_body(T)];
                        E ->
                            [maybe_expr_body(T),
                             [E]]
                    end;
                maybe_match_expr ->
                    [[maybe_match_expr_pattern(T)],
                     [maybe_match_expr_body(T)]];
		module_qualifier ->
		    [[module_qualifier_argument(T)],
		     [module_qualifier_body(T)]];
		named_fun_expr ->
			[[named_fun_expr_name(T)],
			 named_fun_expr_clauses(T)];
                strict_binary_generator ->
                    [[strict_binary_generator_pattern(T)],
                     [strict_binary_generator_body(T)]];
                strict_generator ->
                    [[strict_generator_pattern(T)],
                     [strict_generator_body(T)]];
                strict_map_generator ->
                    [[strict_map_generator_pattern(T)],
                     [strict_map_generator_body(T)]];
		parentheses ->
		    [[parentheses_body(T)]];
		prefix_expr ->
		    [[prefix_expr_operator(T)],
		     [prefix_expr_argument(T)]];
		receive_expr ->
		    case receive_expr_timeout(T) of
			none ->
			    [receive_expr_clauses(T)];
			E ->
			    [receive_expr_clauses(T),
			     [E],
			     receive_expr_action(T)]
		    end;
		record_access ->
                    [[record_access_argument(T)],
                     [record_access_type(T)],
                     [record_access_field(T)]];
		record_expr ->
		    case record_expr_argument(T) of
			none ->
			    [[record_expr_type(T)],
			     record_expr_fields(T)];
			V ->
			    [[V],
			     [record_expr_type(T)],
			     record_expr_fields(T)]
		    end;
		record_field ->
		    case record_field_value(T) of
			none ->
			    [[record_field_name(T)]];
			V ->
			    [[record_field_name(T)], [V]]
		    end;
		record_index_expr ->
		    [[record_index_expr_type(T)],
		     [record_index_expr_field(T)]];
                record_type ->
                    [[record_type_name(T)],
                     record_type_fields(T)];
                record_type_field ->
                    [[record_type_field_name(T)],
                     [record_type_field_type(T)]];
		size_qualifier ->
		    [[size_qualifier_body(T)],
		     [size_qualifier_argument(T)]];
		try_expr ->
		    [try_expr_body(T),
		     try_expr_clauses(T),
		     try_expr_handlers(T),
		     try_expr_after(T)];
		tuple ->
		    [tuple_elements(T)];
                tuple_type ->
                    [tuple_type_elements(T)];
                type_application ->
                    [[type_application_name(T)],
                     type_application_arguments(T)];
                type_union ->
                    [type_union_types(T)];
		typed_record_field ->
                    [[typed_record_field_body(T)],
                     [typed_record_field_type(T)]];
                user_type_application ->
                    [[user_type_application_name(T)],
                     user_type_application_arguments(T)];
		zip_generator ->
		    [zip_generator_body(T)]
	    end
    end.


-doc """
Creates a syntax tree with the same type and attributes as the given tree.

This is equivalent to [`copy_attrs(Node, make_tree(type(Node),
Groups))`](`copy_attrs/2`).

_See also: _`copy_attrs/2`, `make_tree/2`, `type/1`.
""".
-spec update_tree(syntaxTree(), [[syntaxTree()]]) -> syntaxTree().

update_tree(Node, Groups) ->
    copy_attrs(Node, make_tree(type(Node), Groups)).


-doc """
make_tree(Type, Groups)

Creates a syntax tree with the given type and subtrees.

`Type` must be a node type name (see `type/1`) that does not denote a
leaf node type (see `is_leaf/1`). `Groups` must be a _nonempty_ list
of groups of syntax trees, representing the subtrees of a node of the
given type, in left-to-right order as they would occur in the printed
program text, grouped by category as done by `subtrees/1`.

The result of
[`copy_attrs(Node, make_tree(type(Node), subtrees(Node)))`](`copy_attrs/2`) (see
`update_tree/2`) represents the same source code text as the original `Node`,
assuming that [`subtrees(Node)`](`subtrees/1`) yields a nonempty list. However,
it does not necessarily have the same data representation as `Node`.

_See also: _`copy_attrs/2`, `is_leaf/1`, `subtrees/1`, `type/1`,
`update_tree/2`.
""".
-spec make_tree(atom(), [[syntaxTree()]]) -> syntaxTree().

make_tree(annotated_type, [[N], [T]]) -> annotated_type(N, T);
make_tree(application, [[F], A]) -> application(F, A);
make_tree(arity_qualifier, [[N], [A]]) -> arity_qualifier(N, A);
make_tree(attribute, [[N]]) -> attribute(N);
make_tree(attribute, [[N], A]) -> attribute(N, A);
make_tree(binary, [Fs]) -> binary(Fs);
make_tree(binary_comp, [[T], B]) -> binary_comp(T, B);
make_tree(binary_field, [[B]]) -> binary_field(B);
make_tree(binary_field, [[B], Ts]) -> binary_field(B, Ts);
make_tree(binary_generator, [[P], [E]]) -> binary_generator(P, E);
make_tree(bitstring_type, [[M], [N]]) -> bitstring_type(M, N);
make_tree(block_expr, [B]) -> block_expr(B);
make_tree(case_expr, [[A], C]) -> case_expr(A, C);
make_tree(catch_expr, [[B]]) -> catch_expr(B);
make_tree(class_qualifier, [[A], [B]]) -> class_qualifier(A, B);
make_tree(class_qualifier, [[A], [B], [C]]) -> class_qualifier(A, B, C);
make_tree(clause, [P, B]) -> clause(P, none, B);
make_tree(clause, [P, [G], B]) -> clause(P, G, B);
make_tree(conjunction, [E]) -> conjunction(E);
make_tree(constrained_function_type, [[F],C]) ->
    constrained_function_type(F, C);
make_tree(constraint, [[N], Ts]) -> constraint(N, Ts);
make_tree(disjunction, [E]) -> disjunction(E);
make_tree(else_expr, [E]) -> else_expr(E);
make_tree(form_list, [E]) -> form_list(E);
make_tree(fun_expr, [C]) -> fun_expr(C);
make_tree(function, [[N], C]) -> function(N, C);
make_tree(function_type, [[T]]) -> function_type(T);
make_tree(function_type, [A,[T]]) -> function_type(A, T);
make_tree(generator, [[P], [E]]) -> generator(P, E);
make_tree(if_expr, [C]) -> if_expr(C);
make_tree(implicit_fun, [[N]]) -> implicit_fun(N);
make_tree(infix_expr, [[L], [F], [R]]) -> infix_expr(L, F, R);
make_tree(integer_range_type, [[L],[H]]) -> integer_range_type(L, H);
make_tree(list, [P]) -> list(P);
make_tree(list, [P, [S]]) -> list(P, S);
make_tree(list_comp, [[T], B]) -> list_comp(T, B);
make_tree(macro, [[N]]) -> macro(N);
make_tree(macro, [[N], A]) -> macro(N, A);
make_tree(map_comp, [[T], B]) -> map_comp(T, B);
make_tree(map_expr, [Fs]) -> map_expr(Fs);
make_tree(map_expr, [[E], Fs]) -> map_expr(E, Fs);
make_tree(map_field_assoc, [[K], [V]]) -> map_field_assoc(K, V);
make_tree(map_field_exact, [[K], [V]]) -> map_field_exact(K, V);
make_tree(map_generator, [[P], [E]]) -> map_generator(P, E);
make_tree(map_type, [Fs]) -> map_type(Fs);
make_tree(map_type_assoc, [[N],[V]]) -> map_type_assoc(N, V);
make_tree(map_type_exact, [[N],[V]]) -> map_type_exact(N, V);
make_tree(match_expr, [[P], [E]]) -> match_expr(P, E);
make_tree(maybe_expr, [Body]) -> maybe_expr(Body);
make_tree(maybe_expr, [Body, [Else]]) -> maybe_expr(Body, Else);
make_tree(maybe_match_expr, [[P], [E]]) -> maybe_match_expr(P, E);
make_tree(named_fun_expr, [[N], C]) -> named_fun_expr(N, C);
make_tree(module_qualifier, [[M], [N]]) -> module_qualifier(M, N);
make_tree(strict_binary_generator, [[P], [E]]) -> strict_binary_generator(P, E);
make_tree(strict_generator, [[P], [E]]) -> strict_generator(P, E);
make_tree(strict_map_generator, [[P], [E]]) -> strict_map_generator(P, E);
make_tree(parentheses, [[E]]) -> parentheses(E);
make_tree(prefix_expr, [[F], [A]]) -> prefix_expr(F, A);
make_tree(receive_expr, [C]) -> receive_expr(C);
make_tree(receive_expr, [C, [E], A]) -> receive_expr(C, E, A);
make_tree(record_access, [[E], [T], [F]]) ->
    record_access(E, T, F);
make_tree(record_expr, [[T], F]) -> record_expr(T, F);
make_tree(record_expr, [[E], [T], F]) -> record_expr(E, T, F);
make_tree(record_field, [[N]]) -> record_field(N);
make_tree(record_field, [[N], [E]]) -> record_field(N, E);
make_tree(record_index_expr, [[T], [F]]) ->
    record_index_expr(T, F);
make_tree(record_type, [[N],Fs]) -> record_type(N, Fs);
make_tree(record_type_field, [[N],[T]]) -> record_type_field(N, T);
make_tree(size_qualifier, [[N], [A]]) -> size_qualifier(N, A);
make_tree(try_expr, [B, C, H, A]) -> try_expr(B, C, H, A);
make_tree(tuple, [E]) -> tuple(E);
make_tree(tuple_type, [Es]) -> tuple_type(Es);
make_tree(type_application, [[N], Ts]) -> type_application(N, Ts);
make_tree(type_union, [Es]) -> type_union(Es);
make_tree(typed_record_field, [[F],[T]]) -> typed_record_field(F, T);
make_tree(user_type_application, [[N], Ts]) -> user_type_application(N, Ts);
make_tree(zip_generator, [Ts]) -> zip_generator(Ts).


-doc """
Creates a meta-representation of a syntax tree.

The result represents an Erlang expression "`MetaTree`" which, if
evaluated, will yield a new syntax tree representing the same source
code text as `Tree` (although the actual data representation may be
different). The expression represented by `MetaTree` is
_implementation independent_ with regard to the data structures used
by the abstract syntax tree implementation. Comments attached to nodes
of `Tree` will be preserved, but other attributes are lost.

Any node in `Tree` whose node type is `variable` (see `type/1`), and whose list
of annotations (see `get_ann/1`) contains the atom `meta_var`, will remain
unchanged in the resulting tree, except that exactly one occurrence of
`meta_var` is removed from its annotation list.

The main use of the function [`meta/1`](`meta/1`) is to transform a
data structure `Tree`, which represents a piece of program code, into
a form that is _representation independent when printed_. For example,
suppose `Tree` represents a variable named "V". Then (assuming a
function `print/1` for printing syntax trees), evaluating
`print(abstract(Tree))` — simply using `abstract/1` to map the actual
data structure onto a syntax tree representation — would output a
string that might look something like "`{tree, variable, ..., "V",
...}`", which is obviously dependent on the implementation of the
abstract syntax trees. This could, for example, be useful for caching
a syntax tree in a file. However, in some situations like in a program
generator generator (with two "generator"), it may be
unacceptable. Using `print(meta(Tree))` instead would output a
_representation independent_ syntax tree generating expression; in the
above case, something like "`erl_syntax:variable("V")`".

_See also: _`abstract/1`, `get_ann/1`, `type/1`.
""".
-spec meta(syntaxTree()) -> syntaxTree().

meta(T) ->
    %% First of all we check for metavariables:
    case type(T) of
	variable ->
	    case lists:member(meta_var, get_ann(T)) of
		false ->
		    meta_precomment(T);
		true ->
		    %% A meta-variable: remove the first found
		    %% `meta_var' annotation, but otherwise leave
		    %% the node unchanged.
		    set_ann(T, lists:delete(meta_var, get_ann(T)))
	    end;
	_ ->
	    case has_comments(T) of
		true ->
		    meta_precomment(T);
		false ->
		    meta_1(T)
	    end
    end.

meta_precomment(T) ->
    case get_precomments(T) of
	[] ->
	    meta_postcomment(T);
	Cs ->
	    meta_call(set_precomments,
		      [meta_postcomment(T), list(meta_list(Cs))])
    end.

meta_postcomment(T) ->
    case get_postcomments(T) of
	[] ->
	    meta_0(T);
	Cs ->
	    meta_call(set_postcomments,
		      [meta_0(T), list(meta_list(Cs))])
    end.

meta_0(T) ->
    meta_1(remove_comments(T)).

meta_1(T) ->
    %% First handle leaf nodes and other common cases, in order to
    %% generate compact code.
    case type(T) of
	atom ->
	    meta_call(atom, [T]);
	char ->
	    meta_call(char, [T]);
	comment ->
	    meta_call(comment, [list([string(S)
				      || S <- comment_text(T)])]);
	eof_marker ->
	    meta_call(eof_marker, []);
	error_marker ->
	    meta_call(error_marker,
		      [abstract(error_marker_info(T))]);
	float ->
	    meta_call(float, [T]);
	integer ->
	    meta_call(integer, [T]);
	nil ->
	    meta_call(nil, []);
	operator ->
	    meta_call(operator, [atom(operator_name(T))]);
	string ->
	    meta_call(string, [T]);
	text ->
	    meta_call(text, [string(text_string(T))]);
	underscore ->
	    meta_call(underscore, []);
	variable ->
	    meta_call(variable, [string(atom_to_list(variable_name(T)))]);
	warning_marker ->
	    meta_call(warning_marker,
		      [abstract(warning_marker_info(T))]);
	list ->
	    case list_suffix(T) of
		none ->
		    meta_call(list,
			      [list(meta_list(list_prefix(T)))]);
		S ->
		    meta_call(list,
			      [list(meta_list(list_prefix(T))),
			       meta(S)])
	    end;
	tuple ->
	    meta_call(tuple,
		      [list(meta_list(tuple_elements(T)))]);
	Type ->
	    %% All remaining cases are handled using `subtrees'
	    %% and `make_tree' to decompose and reassemble the
	    %% nodes. More cases could of course be handled
	    %% directly to get a more compact output, but I can't
	    %% be bothered right now.
	    meta_call(make_tree,
		      [abstract(Type),
		       meta_subtrees(subtrees(T))])
    end.

meta_list([T | Ts]) ->
    [meta(T) | meta_list(Ts)];
meta_list([]) ->
    [].

meta_subtrees(Gs) ->
    list([list([meta(T)
		|| T <- G])
	  || G <- Gs]).

meta_call(F, As) ->
    application(atom(?MODULE), atom(F), As).


%% =====================================================================
%% Functions for abstraction of the syntax tree representation; may be
%% used externally, but not intended for the normal user.
%% =====================================================================


%% =====================================================================

-doc #{equiv => tree(Type, [])}.
-spec tree(atom()) -> tree().

tree(Type) ->
    tree(Type, []).

%% =====================================================================

-doc """
**For special purposes only**. Creates an abstract syntax tree node with type tag
`Type` and associated data `Data`.

This function and the related `is_tree/1` and `data/1` provide a uniform way to
extend the set of `erl_parse` node types. The associated data is any term, whose
format may depend on the type tag.

> #### Notes {: .info }
>
> - Any nodes created outside of this module must have type tags distinct from
>   those currently defined by this module; see `type/1` for a complete list.
>
> - The type tag of a syntax tree node may also be used as a primary tag by the
>   `erl_parse` representation; in that case, the selector functions for that node
>   type _must_ handle both the abstract syntax tree and the `m:erl_parse` form. The
>   function [`type(T)`](`type/1`) should return the correct type tag regardless
>   of the representation of `T`, so that the user sees no difference between
>   `erl_syntax` and `erl_parse` nodes.

_See also: _`data/1`, `is_tree/1`, `type/1`.
""".
-spec tree(atom(), term()) -> tree().

tree(Type, Data) ->
    #tree{type = Type, data = Data}.


-doc """
is_tree(Node)

**For special purposes only**. Returns `true` if `Tree` is an abstract syntax tree
and `false` otherwise.

> #### Note {: .info }
>
> This function yields `false` for all "old-style" `m:erl_parse`-compatible
> "parse trees".

_See also: _`tree/2`.
""".
-spec is_tree(syntaxTree()) -> boolean().

is_tree(#tree{}) ->
    true;
is_tree(_) ->
    false.


-doc """
data(Node)

**For special purposes only**. Returns the associated data of a syntax tree node.

Evaluation fails with reason `badarg` if [`is_tree(Node)`](`is_tree/1`) does not
yield `true`.

_See also: _`tree/2`.
""".
-spec data(syntaxTree()) -> term().

data(#tree{data = D}) -> D;
data(T) -> erlang:error({badarg, T}).


%% =====================================================================
%% Primitives for backwards compatibility; for internal use only
%% =====================================================================


-doc """
Creates a wrapper structure around an `erl_parse` "parse tree".

This function and the related `unwrap/1` and `is_wrapper/1` provide a uniform
way to attach arbitrary information to an `erl_parse` tree. Some information
about the encapsuled tree may be cached in the wrapper, such as the node type.
All functions on syntax trees must behave so that the user sees no difference
between wrapped and non-wrapped `erl_parse` trees. _Attaching a wrapper onto
another wrapper structure is an error_.
""".
-spec wrap(erl_parse()) -> wrapper().

wrap(Node) ->
    %% We assume that Node is an old-school `erl_parse' tree.
    #wrapper{type = type(Node), attr = #attr{pos = get_pos(Node)},
	     tree = Node}.


%% =====================================================================
-doc """
Removes any wrapper structure, if present.

If `Node` is a wrapper structure, this function returns the wrapped
`m:erl_parse` tree; otherwise it returns `Node` itself.
""".
-spec unwrap(syntaxTree()) -> tree() | erl_parse().

unwrap(#wrapper{tree = Node}) -> Node;
unwrap(Node) -> Node.	 % This could also be a new-form node.


%% =====================================================================
%% Returns `true' if the argument is a wrapper structure, otherwise
%% `false'.

-ifndef(NO_UNUSED).
-spec is_wrapper(term()) -> boolean().

is_wrapper(#wrapper{}) ->
    true;
is_wrapper(_) ->
    false.
-endif.


%% =====================================================================
%% General utility functions for internal use
%% =====================================================================

-doc """
is_printable(S)

Returns `true` if the argument is a wrapper structure, otherwise `false`.
""".
is_printable(S) ->
    io_lib:printable_list(S).

%% Support functions for transforming lists of function names
%% specified as `arity_qualifier' nodes.

unfold_function_names(Ns, Pos) ->
    F = fun ({Atom, Arity}) ->
		N = arity_qualifier(atom(Atom), integer(Arity)),
		set_pos(N, Pos)
	end,
    [F(N) || N <- Ns].

fold_function_names(Ns) ->
    [fold_function_name(N) || N <- Ns].

fold_function_name(N) ->
    Name = arity_qualifier_body(N),
    Arity = arity_qualifier_argument(N),
    true = ((type(Name) =:= atom) and (type(Arity) =:= integer)),
    {concrete(Name), concrete(Arity)}.

fold_variable_names(Vs) ->
    [variable_name(V) || V <- Vs].

unfold_variable_names(Vs, Pos) ->
    [set_pos(variable(V), Pos) || V <- Vs].


%% Support functions for transforming lists of record field definitions.
%%
%% There is no unique representation for field definitions in the
%% standard form. There, they may only occur in the "fields" part of a
%% record expression or declaration, and are represented as
%% `{record_field, Pos, Name, Value}', or as `{record_field, Pos, Name}'
%% if the value part is left out. However, these cannot be distinguished
%% out of context from the representation of record field access
%% expressions (see `record_access').

fold_record_fields(Fs) ->
    [fold_record_field(F) || F <- Fs].

fold_record_field(F) ->
    case type(F) of
        typed_record_field ->
            Field = fold_record_field_1(typed_record_field_body(F)),
            Type = typed_record_field_type(F),
            {typed_record_field, Field, Type};
        record_field ->
            fold_record_field_1(F)
    end.

fold_record_field_1(F) ->
    Pos = get_pos(F),
    Name = record_field_name(F),
    case record_field_value(F) of
	none ->
	    {record_field, Pos, Name};
	Value ->
	    {record_field, Pos, Name, Value}
    end.

unfold_record_fields(Fs) ->
    [unfold_record_field(F) || F <- Fs].

unfold_record_field({typed_record_field, Field, Type}) ->
    F = unfold_record_field_1(Field),
    set_pos(typed_record_field(F, Type), get_pos(F));
unfold_record_field(Field) ->
    unfold_record_field_1(Field).

unfold_record_field_1({record_field, Pos, Name}) ->
    set_pos(record_field(Name), Pos);
unfold_record_field_1({record_field, Pos, Name, Value}) ->
    set_pos(record_field(Name, Value), Pos).

fold_binary_field_types(Ts) ->
    [fold_binary_field_type(T) || T <- Ts].

fold_binary_field_type(Node) ->
    case type(Node) of
	size_qualifier ->
	    {concrete(size_qualifier_body(Node)),
	     concrete(size_qualifier_argument(Node))};
	_ ->
	    concrete(Node)
    end.

unfold_binary_field_types(Ts, Pos) ->
    [unfold_binary_field_type(T, Pos) || T <- Ts].

unfold_binary_field_type({Type, Size}, Pos) ->
    set_pos(size_qualifier(atom(Type), integer(Size)), Pos);
unfold_binary_field_type(Type, Pos) ->
    set_pos(atom(Type), Pos).

%% =====================================================================
