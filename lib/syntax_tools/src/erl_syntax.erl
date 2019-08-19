%% =====================================================================
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
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
%% @copyright 1997-2006 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @end
%% =====================================================================

%% @doc Abstract Erlang syntax trees.
%%
%% This module defines an abstract data type for representing Erlang
%% source code as syntax trees, in a way that is backwards compatible
%% with the data structures created by the Erlang standard library
%% parser module `erl_parse' (often referred to as "parse
%% trees", which is a bit of a misnomer). This means that all
%% `erl_parse' trees are valid abstract syntax trees, but the
%% reverse is not true: abstract syntax trees can in general not be used
%% as input to functions expecting an `erl_parse' tree.
%% However, as long as an abstract syntax tree represents a correct
%% Erlang program, the function {@link revert/1} should be able to
%% transform it to the corresponding `erl_parse'
%% representation.
%%
%% A recommended starting point for the first-time user is the documentation
%% of the {@link syntaxTree()} data type, and the function {@link type/1}.
%%
%% == NOTES: ==
%%
%% This module deals with the composition and decomposition of
%% <em>syntactic</em> entities (as opposed to semantic ones); its
%% purpose is to hide all direct references to the data structures used
%% to represent these entities. With few exceptions, the functions in
%% this module perform no semantic interpretation of their inputs, and
%% in general, the user is assumed to pass type-correct arguments - if
%% this is not done, the effects are not defined.
%%
%% With the exception of the {@link erl_parse()} data structures,
%% the internal representations of abstract syntax trees are subject to
%% change without notice, and should not be documented outside this
%% module. Furthermore, we do not give any guarantees on how an abstract
%% syntax tree may or may not be represented, <em>with the following
%% exceptions</em>: no syntax tree is represented by a single atom, such
%% as `none', by a list constructor `[X | Y]', or
%% by the empty list `[]'. This can be relied on when writing
%% functions that operate on syntax trees.

%% @type syntaxTree(). An abstract syntax tree. The {@link erl_parse()}
%% "parse tree" representation is a proper subset of the `syntaxTree()'
%% representation.
%%
%% Every abstract syntax tree node has a <em>type</em>, given by the
%% function {@link type/1}. Each node also has associated
%% <em>attributes</em>; see {@link get_attrs/1} for details. The functions
%% {@link make_tree/2} and {@link subtrees/1} are generic
%% constructor/decomposition functions for abstract syntax trees. The
%% functions {@link abstract/1} and {@link concrete/1} convert between
%% constant Erlang terms and their syntactic representations. The set of
%% syntax tree nodes is extensible through the {@link tree/2} function.
%%
%% A syntax tree can be transformed to the {@link erl_parse()}
%% representation with the {@link revert/1} function.

-module(erl_syntax).

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
	 cond_expr/1,
	 cond_expr_clauses/1,
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
	 module_qualifier/2,
	 module_qualifier_argument/1,
	 module_qualifier_body/1,
	 named_fun_expr/2,
	 named_fun_expr_arity/1,
	 named_fun_expr_clauses/1,
	 named_fun_expr_name/1,
	 nil/0,
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

	 tree/1,
	 tree/2,
	 data/1,
	 is_tree/1]).

-export_type([forms/0, syntaxTree/0, syntaxTreeAttributes/0, padding/0]).

%% =====================================================================
%% IMPLEMENTATION NOTES:
%%
%% All nodes are represented by tuples of arity 2 or greater, whose
%% first element is an atom which uniquely identifies the type of the
%% node. (In the backwards-compatible representation, the interpretation
%% is also often dependent on the context; the second element generally
%% holds the position information - with a couple of exceptions; see
%% `get_pos' and `set_pos' for details). In the documentation of this
%% module, `Pos' is the source code position information associated with
%% a node; usually, this is a positive integer indicating the original
%% source code line, but no assumptions are made in this module
%% regarding the format or interpretation of position information. When
%% a syntax tree node is constructed, its associated position is by
%% default set to the integer zero.
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

%% =====================================================================

-type syntaxTree() :: #tree{} | #wrapper{} | erl_parse().

-type erl_parse() :: erl_parse:abstract_clause()
                   | erl_parse:abstract_expr()
                   | erl_parse:abstract_form()
                   | erl_parse:abstract_type()
                   | erl_parse:form_info()
                     %% To shut up Dialyzer:
                   | {bin_element, _, _, _, _}.

%% The representation built by the Erlang standard library parser
%% `erl_parse'. This is a subset of the {@link syntaxTree()} type.

%% =====================================================================
%%
%%			Exported functions
%%
%% =====================================================================


%% =====================================================================
%% @doc Returns the type tag of `Node'. If `Node'
%% does not represent a syntax tree, evaluation fails with reason
%% `badarg'. Node types currently defined by this module are:
%%
%% <center><table border="1">
%%  <tr>
%%   <td>application</td>
%%   <td>annotated_type</td>
%%   <td>arity_qualifier</td>
%%   <td>atom</td>
%%  </tr><tr>
%%   <td>attribute</td>
%%   <td>binary</td>
%%   <td>binary_field</td>
%%   <td>bitstring_type</td>
%%  </tr><tr>
%%   <td>block_expr</td>
%%   <td>case_expr</td>
%%   <td>catch_expr</td>
%%   <td>char</td>
%%  </tr><tr>
%%   <td>class_qualifier</td>
%%   <td>clause</td>
%%   <td>comment</td>
%%   <td>cond_expr</td>
%%  </tr><tr>
%%   <td>conjunction</td>
%%   <td>constrained_function_type</td>
%%   <td>constraint</td>
%%   <td>disjunction</td>
%%  </tr><tr>
%%   <td>eof_marker</td>
%%   <td>error_marker</td>
%%   <td>float</td>
%%   <td>form_list</td>
%%  </tr><tr>
%%   <td>fun_expr</td>
%%   <td>fun_type</td>
%%   <td>function</td>
%%   <td>function_type</td>
%%  </tr><tr>
%%   <td>generator</td>
%%   <td>if_expr</td>
%%   <td>implicit_fun</td>
%%   <td>infix_expr</td>
%%  </tr><tr>
%%   <td>integer</td>
%%   <td>integer_range_type</td>
%%   <td>list</td>
%%   <td>list_comp</td>
%%  </tr><tr>
%%   <td>macro</td>
%%   <td>map_expr</td>
%%   <td>map_field_assoc</td>
%%   <td>map_field_exact</td>
%%  </tr><tr>
%%   <td>map_type</td>
%%   <td>map_type_assoc</td>
%%   <td>map_type_exact</td>
%%   <td>match_expr</td>
%%   <td>module_qualifier</td>
%%  </tr><tr>
%%   <td>named_fun_expr</td>
%%   <td>nil</td>
%%   <td>operator</td>
%%   <td>parentheses</td>
%%  </tr><tr>
%%   <td>prefix_expr</td>
%%   <td>receive_expr</td>
%%   <td>record_access</td>
%%   <td>record_expr</td>
%%  </tr><tr>
%%   <td>record_field</td>
%%   <td>record_index_expr</td>
%%   <td>record_type</td>
%%   <td>record_type_field</td>
%%  </tr><tr>
%%   <td>size_qualifier</td>
%%   <td>string</td>
%%   <td>text</td>
%%   <td>try_expr</td>
%%  </tr><tr>
%%   <td>tuple</td>
%%   <td>tuple_type</td>
%%   <td>typed_record_field</td>
%%   <td>type_application</td>
%%   <td>type_union</td>
%%   <td>underscore</td>
%%   <td>user_type_application</td>
%%   <td>variable</td>
%%  </tr><tr>
%%   <td>warning_marker</td>
%%  </tr>
%% </table></center>
%%
%% The user may (for special purposes) create additional nodes
%% with other type tags, using the {@link tree/2} function.
%%
%% Note: The primary constructor functions for a node type should
%% always have the same name as the node type itself.
%%
%% @see tree/2
%% @see annotated_type/2
%% @see application/3
%% @see arity_qualifier/2
%% @see atom/1
%% @see attribute/2
%% @see binary/1
%% @see binary_field/2
%% @see bitstring_type/2
%% @see block_expr/1
%% @see case_expr/2
%% @see catch_expr/1
%% @see char/1
%% @see class_qualifier/2
%% @see clause/3
%% @see comment/2
%% @see cond_expr/1
%% @see conjunction/1
%% @see constrained_function_type/2
%% @see constraint/2
%% @see disjunction/1
%% @see eof_marker/0
%% @see error_marker/1
%% @see float/1
%% @see form_list/1
%% @see fun_expr/1
%% @see fun_type/0
%% @see function/2
%% @see function_type/1
%% @see function_type/2
%% @see generator/2
%% @see if_expr/1
%% @see implicit_fun/2
%% @see infix_expr/3
%% @see integer/1
%% @see integer_range_type/2
%% @see list/2
%% @see list_comp/2
%% @see macro/2
%% @see map_expr/2
%% @see map_field_assoc/2
%% @see map_field_exact/2
%% @see map_type/0
%% @see map_type/1
%% @see map_type_assoc/2
%% @see map_type_exact/2
%% @see match_expr/2
%% @see module_qualifier/2
%% @see named_fun_expr/2
%% @see nil/0
%% @see operator/1
%% @see parentheses/1
%% @see prefix_expr/2
%% @see receive_expr/3
%% @see record_access/3
%% @see record_expr/2
%% @see record_field/2
%% @see record_index_expr/2
%% @see record_type/2
%% @see record_type_field/2
%% @see size_qualifier/2
%% @see string/1
%% @see text/1
%% @see try_expr/3
%% @see tuple/1
%% @see tuple_type/0
%% @see tuple_type/1
%% @see typed_record_field/2
%% @see type_application/2
%% @see type_union/1
%% @see underscore/0
%% @see user_type_application/2
%% @see variable/1
%% @see warning_marker/1

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
	{'cond', _, _} -> cond_expr;
	{'fun', _, {clauses, _}} -> fun_expr;
	{named_fun, _, _, _} -> named_fun_expr;
	{'fun', _, {function, _, _}} -> implicit_fun;
	{'fun', _, {function, _, _, _}} -> implicit_fun;
	{'if', _, _} -> if_expr;
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
	{generate, _, _, _} -> generator;
	{lc, _, _, _} -> list_comp;
	{bc, _, _, _} -> binary_comp;		
	{match, _, _, _} -> match_expr;
        {map, _, _, _} -> map_expr;
        {map, _, _} -> map_expr;
        {map_field_assoc, _, _, _} -> map_field_assoc;
        {map_field_exact, _, _, _} -> map_field_exact;
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


%% =====================================================================
%% @doc Returns `true' if `Node' is a leaf node,
%% otherwise `false'. The currently recognised leaf node
%% types are:
%%
%% <center><table border="1">
%%  <tr>
%%   <td>`atom'</td>
%%   <td>`char'</td>
%%   <td>`comment'</td>
%%   <td>`eof_marker'</td>
%%   <td>`error_marker'</td>
%%  </tr><tr>
%%   <td>`float'</td>
%%   <td>`fun_type'</td>
%%   <td>`integer'</td>
%%   <td>`nil'</td>
%%   <td>`operator'</td>
%%   <td>`string'</td>
%%  </tr><tr>
%%   <td>`text'</td>
%%   <td>`underscore'</td>
%%   <td>`variable'</td>
%%   <td>`warning_marker'</td>
%%  </tr>
%% </table></center>
%%
%% A node of type `map_expr' is a leaf node if and only if it has no
%% argument and no fields.
%% A node of type `map_type' is a leaf node if and only if it has no
%% fields (`any_size').
%% A node of type `tuple' is a leaf node if and only if its arity is zero.
%% A node of type `tuple_type' is a leaf node if and only if it has no
%% elements (`any_size').
%%
%% Note: not all literals are leaf nodes, and vice versa. E.g.,
%% tuples with nonzero arity and nonempty lists may be literals, but are
%% not leaf nodes. Variables, on the other hand, are leaf nodes but not
%% literals.
%%
%% @see type/1
%% @see is_literal/1

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


%% =====================================================================
%% @doc Returns `true' if `Node' is a syntax tree
%% representing a so-called "source code form", otherwise
%% `false'. Forms are the Erlang source code units which,
%% placed in sequence, constitute an Erlang program. Current form types
%% are:
%%
%% <center><table border="1">
%%  <tr>
%%   <td>`attribute'</td>
%%   <td>`comment'</td>
%%   <td>`error_marker'</td>
%%   <td>`eof_marker'</td>
%%  </tr><tr>
%%   <td>`form_list'</td>
%%   <td>`function'</td>
%%   <td>`warning_marker'</td>
%%   <td>`text'</td>
%%  </tr>
%% </table></center>
%%
%% @see type/1
%% @see attribute/2
%% @see comment/2
%% @see eof_marker/0
%% @see error_marker/1
%% @see form_list/1
%% @see function/2
%% @see warning_marker/1

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
%% @doc Returns the position information associated with
%% `Node'. This is usually a nonnegative integer (indicating
%% the source code line number), but may be any term. By default, all
%% new tree nodes have their associated position information set to the
%% integer zero.
%%
%% @see set_pos/2
%% @see get_attrs/1

%% All `erl_parse' tree nodes are represented by tuples whose second
%% field is the position information (usually an integer), *with the
%% exceptions of* `{error, ...}' (type `error_marker') and `{warning,
%% ...}' (type `warning_marker'), which only contain the associated line
%% number *of the error descriptor*; this is all handled transparently
%% by `get_pos' and `set_pos'.

-spec get_pos(syntaxTree()) -> term().

get_pos(#tree{attr = Attr}) ->
    Attr#attr.pos;
get_pos(#wrapper{attr = Attr}) ->
    Attr#attr.pos;
get_pos({error, {Pos, _, _}}) ->
    Pos;
get_pos({warning, {Pos, _, _}}) ->
    Pos;
get_pos(Node) ->
    %% Here, we assume that we have an `erl_parse' node with position
    %% information in element 2.
    element(2, Node).


%% =====================================================================
%% @doc Sets the position information of `Node' to `Pos'.
%%
%% @see get_pos/1
%% @see copy_pos/2

-spec set_pos(syntaxTree(), term()) -> syntaxTree().

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


%% =====================================================================
%% @doc Copies the position information from `Source' to `Target'.
%%
%% This is equivalent to `set_pos(Target,
%% get_pos(Source))', but potentially more efficient.
%%
%% @see get_pos/1
%% @see set_pos/2

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


%% =====================================================================
%% @doc Returns the associated pre-comments of a node. This is a
%% possibly empty list of abstract comments, in top-down textual order.
%% When the code is formatted, pre-comments are typically displayed
%% directly above the node. For example:
%% ```% Pre-comment of function
%%    foo(X) -> {bar, X}.'''
%%
%% If possible, the comment should be moved before any preceding
%% separator characters on the same line. E.g.:
%% ```foo([X | Xs]) ->
%%        % Pre-comment of 'bar(X)' node
%%        [bar(X) | foo(Xs)];
%%    ...'''
%% (where the comment is moved before the "`['").
%%
%% @see comment/2
%% @see set_precomments/2
%% @see get_postcomments/1
%% @see get_attrs/1

-spec get_precomments(syntaxTree()) -> [syntaxTree()].

get_precomments(#tree{attr = Attr}) -> get_precomments_1(Attr);
get_precomments(#wrapper{attr = Attr}) -> get_precomments_1(Attr);
get_precomments(_) -> [].

get_precomments_1(#attr{com = none}) -> [];
get_precomments_1(#attr{com = #com{pre = Cs}}) -> Cs.


%% =====================================================================
%% @doc Sets the pre-comments of `Node' to
%% `Comments'. `Comments' should be a possibly
%% empty list of abstract comments, in top-down textual order.
%%
%% @see comment/2
%% @see get_precomments/1
%% @see add_precomments/2
%% @see set_postcomments/2
%% @see copy_comments/2
%% @see remove_comments/1
%% @see join_comments/2

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


%% =====================================================================
%% @doc Appends `Comments' to the pre-comments of `Node'.
%%
%% Note: This is equivalent to `set_precomments(Node,
%% get_precomments(Node) ++ Comments)', but potentially more
%% efficient.
%%
%% @see comment/2
%% @see get_precomments/1
%% @see set_precomments/2
%% @see add_postcomments/2
%% @see join_comments/2

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


%% =====================================================================
%% @doc Returns the associated post-comments of a node. This is a
%% possibly empty list of abstract comments, in top-down textual order.
%% When the code is formatted, post-comments are typically displayed to
%% the right of and/or below the node. For example:
%% ```{foo, X, Y}     % Post-comment of tuple'''
%%
%% If possible, the comment should be moved past any following
%% separator characters on the same line, rather than placing the
%% separators on the following line. E.g.:
%% ```foo([X | Xs], Y) ->
%%        foo(Xs, bar(X));     % Post-comment of 'bar(X)' node
%%     ...'''
%% (where the comment is moved past the rightmost "`)'" and
%% the "`;'").
%%
%% @see comment/2
%% @see set_postcomments/2
%% @see get_precomments/1
%% @see get_attrs/1

-spec get_postcomments(syntaxTree()) -> [syntaxTree()].

get_postcomments(#tree{attr = Attr}) -> get_postcomments_1(Attr);
get_postcomments(#wrapper{attr = Attr}) -> get_postcomments_1(Attr);
get_postcomments(_) -> [].

get_postcomments_1(#attr{com = none}) -> [];
get_postcomments_1(#attr{com = #com{post = Cs}}) -> Cs.


%% =====================================================================
%% @doc Sets the post-comments of `Node' to
%% `Comments'. `Comments' should be a possibly
%% empty list of abstract comments, in top-down textual order
%%
%% @see comment/2
%% @see get_postcomments/1
%% @see add_postcomments/2
%% @see set_precomments/2
%% @see copy_comments/2
%% @see remove_comments/1
%% @see join_comments/2

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


%% =====================================================================
%% @doc Appends `Comments' to the post-comments of `Node'.
%%
%% Note: This is equivalent to `set_postcomments(Node,
%% get_postcomments(Node) ++ Comments)', but potentially more
%% efficient.
%%
%% @see comment/2
%% @see get_postcomments/1
%% @see set_postcomments/2
%% @see add_precomments/2
%% @see join_comments/2

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


%% =====================================================================
%% @doc Yields `false' if the node has no associated
%% comments, and `true' otherwise.
%%
%% Note: This is equivalent to `(get_precomments(Node) == [])
%% and (get_postcomments(Node) == [])', but potentially more
%% efficient.
%%
%% @see get_precomments/1
%% @see get_postcomments/1
%% @see remove_comments/1

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


%% =====================================================================
%% @doc Clears the associated comments of `Node'.
%%
%% Note: This is equivalent to
%% `set_precomments(set_postcomments(Node, []), [])', but
%% potentially more efficient.
%%
%% @see set_precomments/2
%% @see set_postcomments/2

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


%% =====================================================================
%% @doc Copies the pre- and postcomments from `Source' to `Target'.
%%
%% Note: This is equivalent to
%% `set_postcomments(set_precomments(Target,
%% get_precomments(Source)), get_postcomments(Source))', but
%% potentially more efficient.
%%
%% @see comment/2
%% @see get_precomments/1
%% @see get_postcomments/1
%% @see set_precomments/2
%% @see set_postcomments/2

-spec copy_comments(syntaxTree(), syntaxTree()) -> syntaxTree().

copy_comments(Source, Target) ->
    set_com(Target, get_com(Source)).


%% =====================================================================
%% @doc Appends the comments of `Source' to the current
%% comments of `Target'.
%%
%% Note: This is equivalent to
%% `add_postcomments(get_postcomments(Source),
%% add_precomments(get_precomments(Source), Target))', but
%% potentially more efficient.
%%
%% @see comment/2
%% @see get_precomments/1
%% @see get_postcomments/1
%% @see add_precomments/2
%% @see add_postcomments/2

-spec join_comments(syntaxTree(), syntaxTree()) -> syntaxTree().

join_comments(Source, Target) ->
    add_postcomments(
      get_postcomments(Source),
      add_precomments(get_precomments(Source), Target)).


%% =====================================================================
%% @doc Returns the list of user annotations associated with a syntax
%% tree node. For a newly created node, this is the empty list. The
%% annotations may be any terms.
%%
%% @see set_ann/2
%% @see get_attrs/1

-spec get_ann(syntaxTree()) -> [term()].

get_ann(#tree{attr = Attr}) -> Attr#attr.ann;
get_ann(#wrapper{attr = Attr}) -> Attr#attr.ann;
get_ann(_) -> [].


%% =====================================================================
%% @doc Sets the list of user annotations of `Node' to `Annotations'.
%%
%% @see get_ann/1
%% @see add_ann/2
%% @see copy_ann/2

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


%% =====================================================================
%% @doc Appends the term `Annotation' to the list of user
%% annotations of `Node'.
%%
%% Note: this is equivalent to `set_ann(Node, [Annotation |
%% get_ann(Node)])', but potentially more efficient.
%%
%% @see get_ann/1
%% @see set_ann/2

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


%% =====================================================================
%% @doc Copies the list of user annotations from `Source' to `Target'.
%%
%% Note: this is equivalent to `set_ann(Target,
%% get_ann(Source))', but potentially more efficient.
%%
%% @see get_ann/1
%% @see set_ann/2

-spec copy_ann(syntaxTree(), syntaxTree()) -> syntaxTree().

copy_ann(Source, Target) ->
    set_ann(Target, get_ann(Source)).


%% =====================================================================
%% @doc Returns a representation of the attributes associated with a
%% syntax tree node. The attributes are all the extra information that
%% can be attached to a node. Currently, this includes position
%% information, source code comments, and user annotations. The result
%% of this function cannot be inspected directly; only attached to
%% another node (see {@link set_attrs/2}).
%%
%% For accessing individual attributes, see {@link get_pos/1},
%% {@link get_ann/1}, {@link get_precomments/1} and
%% {@link get_postcomments/1}.
%%
%% @type syntaxTreeAttributes(). This is an abstract representation of
%% syntax tree node attributes; see the function {@link get_attrs/1}.
%%
%% @see set_attrs/2
%% @see get_pos/1
%% @see get_ann/1
%% @see get_precomments/1
%% @see get_postcomments/1

-spec get_attrs(syntaxTree()) -> syntaxTreeAttributes().

get_attrs(#tree{attr = Attr}) -> Attr;
get_attrs(#wrapper{attr = Attr}) -> Attr;
get_attrs(Node) -> #attr{pos = get_pos(Node),
			 ann = get_ann(Node),
			 com = get_com(Node)}.


%% =====================================================================
%% @doc Sets the attributes of `Node' to `Attributes'.
%%
%% @see get_attrs/1
%% @see copy_attrs/2

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


%% =====================================================================
%% @doc Copies the attributes from `Source' to `Target'.
%%
%% Note: this is equivalent to `set_attrs(Target,
%% get_attrs(Source))', but potentially more efficient.
%%
%% @see get_attrs/1
%% @see set_attrs/2

-spec copy_attrs(syntaxTree(), syntaxTree()) -> syntaxTree().

copy_attrs(S, T) ->
    set_attrs(T, get_attrs(S)).


%% =====================================================================
%% @equiv comment(none, Strings)

-spec comment([string()]) -> syntaxTree().

comment(Strings) ->
    comment(none, Strings).


%% =====================================================================
%% @doc Creates an abstract comment with the given padding and text. If
%% `Strings' is a (possibly empty) list
%% <code>["<em>Txt1</em>", ..., "<em>TxtN</em>"]</code>, the result
%% represents the source code text
%% <pre>
%%    %<em>Txt1</em>
%%    ...
%%    %<em>TxtN</em></pre>
%% `Padding' states the number of empty character positions
%% to the left of the comment separating it horizontally from
%% source code on the same line (if any). If `Padding' is
%% `none', a default positive number is used. If
%% `Padding' is an integer less than 1, there should be no
%% separating space. Comments are in themselves regarded as source
%% program forms.
%%
%% @see comment/1
%% @see is_form/1

-type padding() :: 'none' | integer().

-record(comment, {pad :: padding(), text :: [string()]}).

%% type(Node) = comment
%% data(Node) = #comment{pad :: Padding, text :: Strings}
%%
%%	Padding = none | integer()
%%	Strings = [string()]

-spec comment(padding(), [string()]) -> syntaxTree().

comment(Pad, Strings) ->
    tree(comment, #comment{pad = Pad, text = Strings}).


%% =====================================================================
%% @doc Returns the lines of text of the abstract comment.
%%
%% @see comment/2

-spec comment_text(syntaxTree()) -> [string()].

comment_text(Node) ->
    (data(Node))#comment.text.


%% =====================================================================
%% @doc Returns the amount of padding before the comment, or
%% `none'. The latter means that a default padding may be used.
%%
%% @see comment/2

-spec comment_padding(syntaxTree()) -> padding().

comment_padding(Node) ->
    (data(Node))#comment.pad.


%% =====================================================================
%% @doc Creates an abstract sequence of "source code forms". If
%% `Forms' is `[F1, ..., Fn]', where each
%% `Fi' is a form (see {@link is_form/1}, the result
%% represents
%% <pre>
%%    <em>F1</em>
%%    ...
%%    <em>Fn</em></pre>
%% where the `Fi' are separated by one or more line breaks. A
%% node of type `form_list' is itself regarded as a source
%% code form; see {@link flatten_form_list/1}.
%%
%% Note: this is simply a way of grouping source code forms as a
%% single syntax tree, usually in order to form an Erlang module
%% definition.
%%
%% @see form_list_elements/1
%% @see is_form/1
%% @see flatten_form_list/1

%% type(Node) = form_list
%% data(Node) = [Form]
%%
%%	Form = syntaxTree()
%%	is_form(Form) = true

-spec form_list([syntaxTree()]) -> syntaxTree().

form_list(Forms) ->
    tree(form_list, Forms).


%% =====================================================================
%% @doc Returns the list of subnodes of a `form_list' node.
%%
%% @see form_list/1

-spec form_list_elements(syntaxTree()) -> [syntaxTree()].

form_list_elements(Node) ->
    data(Node).


%% =====================================================================
%% @doc Flattens sublists of a `form_list' node. Returns
%% `Node' with all subtrees of type `form_list'
%% recursively expanded, yielding a single "flat" abstract form
%% sequence.
%%
%% @see form_list/1

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


%% =====================================================================
%% @doc Creates an abstract piece of source code text. The result
%% represents exactly the sequence of characters in `String'.
%% This is useful in cases when one wants full control of the resulting
%% output, e.g., for the appearance of floating-point numbers or macro
%% definitions.
%%
%% @see text_string/1

%% type(Node) = text
%% data(Node) = string()

-spec text(string()) -> syntaxTree().

text(String) ->
    tree(text, String).


%% =====================================================================
%% @doc Returns the character sequence represented by a `text' node.
%%
%% @see text/1

-spec text_string(syntaxTree()) -> string().

text_string(Node) ->
    data(Node).


%% =====================================================================
%% @doc Creates an abstract variable with the given name.
%% `Name' may be any atom or string that represents a
%% lexically valid variable name, but <em>not</em> a single underscore
%% character; see {@link underscore/0}.
%%
%% Note: no checking is done whether the character sequence
%% represents a proper variable name, i.e., whether or not its first
%% character is an uppercase Erlang character, or whether it does not
%% contain control characters, whitespace, etc.
%%
%% @see variable_name/1
%% @see variable_literal/1
%% @see underscore/0

%% type(Node) = variable
%% data(Node) = atom()
%%
%% `erl_parse' representation:
%%
%% {var, Pos, Name}
%%
%%	Name = atom() \ '_'

-spec variable(atom() | string()) -> syntaxTree().

variable(Name) when is_atom(Name) ->
    tree(variable, Name);
variable(Name) ->
    tree(variable, list_to_atom(Name)).

revert_variable(Node) ->
    Pos = get_pos(Node),
    Name = variable_name(Node),
    {var, Pos, Name}.


%% =====================================================================
%% @doc Returns the name of a `variable' node as an atom.
%%
%% @see variable/1

-spec variable_name(syntaxTree()) -> atom().

variable_name(Node) ->
    case unwrap(Node) of
	{var, _, Name} ->
	    Name;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Returns the name of a `variable' node as a string.
%%
%% @see variable/1

-spec variable_literal(syntaxTree()) -> string().

variable_literal(Node) ->
    case unwrap(Node) of
	{var, _, Name} ->
	    atom_to_list(Name);
	Node1 ->
	    atom_to_list(data(Node1))
    end.


%% =====================================================================
%% @doc Creates an abstract universal pattern ("`_'"). The
%% lexical representation is a single underscore character. Note that
%% this is <em>not</em> a variable, lexically speaking.
%%
%% @see variable/1

%% type(Node) = underscore
%% data(Node) = []
%%
%% `erl_parse' representation:
%%
%% {var, Pos, '_'}

-spec underscore() -> syntaxTree().

underscore() ->
    tree(underscore, []).

revert_underscore(Node) ->
    Pos = get_pos(Node),
    {var, Pos, '_'}.


%% =====================================================================
%% @doc Creates an abstract integer literal. The lexical representation
%% is the canonical decimal numeral of `Value'.
%%
%% @see integer_value/1
%% @see integer_literal/1
%% @see is_integer/2

%% type(Node) = integer
%% data(Node) = integer()
%%
%% `erl_parse' representation:
%%
%% {integer, Pos, Value}
%%
%%	Value = integer()

-spec integer(integer()) -> syntaxTree().

integer(Value) ->
    tree(integer, Value).

revert_integer(Node) ->
    Pos = get_pos(Node),
    {integer, Pos, integer_value(Node)}.


%% =====================================================================
%% @doc Returns `true' if `Node' has type
%% `integer' and represents `Value', otherwise `false'.
%%
%% @see integer/1

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


%% =====================================================================
%% @doc Returns the value represented by an `integer' node.
%%
%% @see integer/1

-spec integer_value(syntaxTree()) -> integer().

integer_value(Node) ->
    case unwrap(Node) of
	{integer, _, Value} ->
	    Value;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Returns the numeral string represented by an `integer' node.
%%
%% @see integer/1

-spec integer_literal(syntaxTree()) -> string().

integer_literal(Node) ->
    integer_to_list(integer_value(Node)).


%% =====================================================================
%% @doc Creates an abstract floating-point literal. The lexical
%% representation is the decimal floating-point numeral of `Value'.
%%
%% @see float_value/1
%% @see float_literal/1

%% type(Node) = float
%% data(Node) = Value
%%
%%	Value = float()
%%
%% `erl_parse' representation:
%%
%% {float, Pos, Value}
%%
%%	Value = float()

%% Note that under current versions of Erlang, the name `float/1' cannot
%% be used for local calls (i.e., within the module) - it will be
%% overridden by the type conversion BIF of the same name, so always use
%% `make_float/1' for local calls.

-spec float(float()) -> syntaxTree().

float(Value) ->
    make_float(Value).

make_float(Value) ->
    tree(float, Value).

revert_float(Node) ->
    Pos = get_pos(Node),
    {float, Pos, float_value(Node)}.


%% =====================================================================
%% @doc Returns the value represented by a `float' node. Note
%% that floating-point values should usually not be compared for
%% equality.
%%
%% @see float/1

-spec float_value(syntaxTree()) -> float().

float_value(Node) ->
    case unwrap(Node) of
	{float, _, Value} ->
	    Value;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Returns the numeral string represented by a `float' node.
%%
%% @see float/1

-spec float_literal(syntaxTree()) -> string().

float_literal(Node) ->
    float_to_list(float_value(Node)).


%% =====================================================================
%% @doc Creates an abstract character literal. The result represents
%% "<code>$<em>Name</em></code>", where `Name' corresponds to
%% `Value'.
%%
%% Note: the literal corresponding to a particular character value is
%% not uniquely defined. E.g., the character "`a'" can be
%% written both as "`$a'" and "`$\141'", and a Tab
%% character can be written as "`$\11'", "`$\011'"
%% or "`$\t'".
%%
%% @see char_value/1
%% @see char_literal/1
%% @see char_literal/2
%% @see is_char/2

%% type(Node) = char
%% data(Node) = char()
%%
%% `erl_parse' representation:
%%
%% {char, Pos, Code}
%%
%%	Code = integer()

-spec char(char()) -> syntaxTree().

char(Char) ->
    tree(char, Char).

revert_char(Node) ->
    Pos = get_pos(Node),
    {char, Pos, char_value(Node)}.


%% =====================================================================
%% @doc Returns `true' if `Node' has type
%% `char' and represents `Value', otherwise `false'.
%%
%% @see char/1

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


%% =====================================================================
%% @doc Returns the value represented by a `char' node.
%%
%% @see char/1

-spec char_value(syntaxTree()) -> char().

char_value(Node) ->
    case unwrap(Node) of
	{char, _, Char} ->
	    Char;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Returns the literal string represented by a `char'
%% node. This includes the leading "`$'" character.
%% Characters beyond 255 will be escaped.
%%
%% @see char/1

-spec char_literal(syntaxTree()) -> nonempty_string().

char_literal(Node) ->
    char_literal(Node, latin1).


%% =====================================================================
%% @doc Returns the literal string represented by a `char'
%% node. This includes the leading "`$'" character.
%% Depending on the encoding a character beyond 255 will be escaped
%% (`latin1') or copied as is (`utf8').
%%
%% @see char/1

-type encoding() :: 'utf8' | 'unicode' | 'latin1'.

-spec char_literal(syntaxTree(), encoding()) -> nonempty_string().

char_literal(Node, unicode) ->
    io_lib:write_char(char_value(Node));
char_literal(Node, utf8) ->
    io_lib:write_char(char_value(Node));
char_literal(Node, latin1) ->
    io_lib:write_char_as_latin1(char_value(Node)).


%% =====================================================================
%% @doc Creates an abstract string literal. The result represents
%% <code>"<em>Text</em>"</code> (including the surrounding
%% double-quotes), where `Text' corresponds to the sequence
%% of characters in `Value', but not representing a
%% <em>specific</em> string literal.
%%
%% For example, the result of `string("x\ny")' represents any and all of
%% `"x\ny"', `"x\12y"', `"x\012y"' and `"x\^Jy"'; see {@link char/1}.
%%
%% @see string_value/1
%% @see string_literal/1
%% @see string_literal/2
%% @see is_string/2
%% @see char/1

%% type(Node) = string
%% data(Node) = string()
%%
%% `erl_parse' representation:
%%
%% {string, Pos, Chars}
%%
%%	Chars = string()

-spec string(string()) -> syntaxTree().

string(String) ->
    tree(string, String).

revert_string(Node) ->
    Pos = get_pos(Node),
    {string, Pos, string_value(Node)}.


%% =====================================================================
%% @doc Returns `true' if `Node' has type
%% `string' and represents `Value', otherwise `false'.
%%
%% @see string/1

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


%% =====================================================================
%% @doc Returns the value represented by a `string' node.
%%
%% @see string/1

-spec string_value(syntaxTree()) -> string().

string_value(Node) ->
    case unwrap(Node) of
	{string, _, List} ->
	    List;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Returns the literal string represented by a `string'
%% node. This includes surrounding double-quote characters.
%% Characters beyond 255 will be escaped.
%%
%% @see string/1

-spec string_literal(syntaxTree()) -> nonempty_string().

string_literal(Node) ->
    string_literal(Node, latin1).


%% =====================================================================
%% @doc Returns the literal string represented by a `string'
%% node. This includes surrounding double-quote characters.
%% Depending on the encoding characters beyond 255 will be escaped
%% (`latin1') or copied as is (`utf8').
%%
%% @see string/1

-spec string_literal(syntaxTree(), encoding()) -> nonempty_string().

string_literal(Node, utf8) ->
    io_lib:write_string(string_value(Node));
string_literal(Node, unicode) ->
    io_lib:write_string(string_value(Node));
string_literal(Node, latin1) ->
    io_lib:write_string_as_latin1(string_value(Node)).


%% =====================================================================
%% @doc Creates an abstract atom literal. The print name of the atom is
%% the character sequence represented by `Name'.
%%
%% @see atom_value/1
%% @see atom_name/1
%% @see atom_literal/1
%% @see atom_literal/2
%% @see is_atom/2

%% type(Node) = atom
%% data(Node) = atom()
%%
%% `erl_parse' representation:
%%
%% {atom, Pos, Value}
%%
%%	Value = atom()

-spec atom(atom() | string()) -> syntaxTree().

atom(Name) when is_atom(Name) ->
    tree(atom, Name);
atom(Name) ->
    tree(atom, list_to_atom(Name)).

revert_atom(Node) ->
    Pos = get_pos(Node),
    {atom, Pos, atom_value(Node)}.


%% =====================================================================
%% @doc Returns `true' if `Node' has type
%% `atom' and represents `Value', otherwise `false'.
%%
%% @see atom/1

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


%% =====================================================================
%% @doc Returns the value represented by an `atom' node.
%%
%% @see atom/1

-spec atom_value(syntaxTree()) -> atom().

atom_value(Node) ->
    case unwrap(Node) of
	{atom, _, Name} ->
	    Name;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Returns the printname of an `atom' node.
%%
%% @see atom/1

-spec atom_name(syntaxTree()) -> string().

atom_name(Node) ->
    atom_to_list(atom_value(Node)).


%% =====================================================================
%% @doc Returns the literal string represented by an `atom'
%% node. This includes surrounding single-quote characters if necessary.
%% Characters beyond 255 will be escaped.
%%
%% Note that e.g. the result of `atom("x\ny")' represents
%% any and all of `'x\ny'', `'x\12y'',
%% `'x\012y'' and `'x\^Jy\''; see {@link string/1}.
%%
%% @see atom/1
%% @see string/1

-spec atom_literal(syntaxTree()) -> string().

atom_literal(Node) ->
    atom_literal(Node, latin1).

%% =====================================================================
%% @doc Returns the literal string represented by an `atom'
%% node. This includes surrounding single-quote characters if necessary.
%% Depending on the encoding a character beyond 255 will be escaped
%% (`latin1') or copied as is (`utf8').
%%
%% @see atom/1
%% @see atom_literal/1
%% @see string/1

atom_literal(Node, utf8) ->
    io_lib:write_atom(atom_value(Node));
atom_literal(Node, unicode) ->
    io_lib:write_atom(atom_value(Node));
atom_literal(Node, latin1) ->
    io_lib:write_atom_as_latin1(atom_value(Node)).

%% =====================================================================
%% @equiv map_expr(none, Fields)

-spec map_expr([syntaxTree()]) -> syntaxTree().

map_expr(Fields) ->
    map_expr(none, Fields).


%% =====================================================================
%% @doc Creates an abstract map expression. If `Fields' is
%% `[F1, ..., Fn]', then if `Argument' is `none', the result represents
%% "<code>#{<em>F1</em>, ..., <em>Fn</em>}</code>",
%% otherwise it represents
%% "<code><em>Argument</em>#{<em>F1</em>, ..., <em>Fn</em>}</code>".
%%
%% @see map_expr/1
%% @see map_expr_argument/1
%% @see map_expr_fields/1
%% @see map_field_assoc/2
%% @see map_field_exact/2

-record(map_expr, {argument :: 'none' | syntaxTree(),
                   fields   :: [syntaxTree()]}).

%% `erl_parse' representation:
%%
%% {map, Pos, Fields}
%% {map, Pos, Argument, Fields}

-spec map_expr('none' | syntaxTree(), [syntaxTree()]) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the argument subtree of a `map_expr' node, if any. If `Node'
%% represents "<code>#{...}</code>", `none' is returned.
%% Otherwise, if `Node' represents "<code><em>Argument</em>#{...}</code>",
%% `Argument' is returned.
%%
%% @see map_expr/2

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


%% =====================================================================
%% @doc Returns the list of field subtrees of a `map_expr' node.
%%
%% @see map_expr/2

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


%% =====================================================================
%% @doc Creates an abstract map assoc field. The result represents
%% "<code><em>Name</em> => <em>Value</em></code>".
%%
%% @see map_field_assoc_name/1
%% @see map_field_assoc_value/1
%% @see map_expr/2

-record(map_field_assoc, {name :: syntaxTree(), value :: syntaxTree()}).

%% `erl_parse' representation:
%%
%% {map_field_assoc, Pos, Name, Value}

-spec map_field_assoc(syntaxTree(), syntaxTree()) -> syntaxTree().

map_field_assoc(Name, Value) ->
    tree(map_field_assoc, #map_field_assoc{name = Name, value = Value}).

revert_map_field_assoc(Node) ->
    Pos = get_pos(Node),
    Name = map_field_assoc_name(Node),
    Value = map_field_assoc_value(Node),
    {map_field_assoc, Pos, Name, Value}.


%% =====================================================================
%% @doc Returns the name subtree of a `map_field_assoc' node.
%%
%% @see map_field_assoc/2

-spec map_field_assoc_name(syntaxTree()) -> syntaxTree().

map_field_assoc_name(Node) ->
    case unwrap(Node) of
        {map_field_assoc, _, Name, _} ->
            Name;
        Node1 ->
            (data(Node1))#map_field_assoc.name
    end.


%% =====================================================================
%% @doc Returns the value subtree of a `map_field_assoc' node.
%%
%% @see map_field_assoc/2

-spec map_field_assoc_value(syntaxTree()) -> syntaxTree().

map_field_assoc_value(Node) ->
    case unwrap(Node) of
        {map_field_assoc, _, _, Value} ->
            Value;
        Node1 ->
            (data(Node1))#map_field_assoc.value
    end.


%% =====================================================================
%% @doc Creates an abstract map exact field. The result represents
%% "<code><em>Name</em> := <em>Value</em></code>".
%%
%% @see map_field_exact_name/1
%% @see map_field_exact_value/1
%% @see map_expr/2

-record(map_field_exact, {name :: syntaxTree(), value :: syntaxTree()}).

%% `erl_parse' representation:
%%
%% {map_field_exact, Pos, Name, Value}

-spec map_field_exact(syntaxTree(), syntaxTree()) -> syntaxTree().

map_field_exact(Name, Value) ->
    tree(map_field_exact, #map_field_exact{name = Name, value = Value}).

revert_map_field_exact(Node) ->
    Pos = get_pos(Node),
    Name = map_field_exact_name(Node),
    Value = map_field_exact_value(Node),
    {map_field_exact, Pos, Name, Value}.


%% =====================================================================
%% @doc Returns the name subtree of a `map_field_exact' node.
%%
%% @see map_field_exact/2

-spec map_field_exact_name(syntaxTree()) -> syntaxTree().

map_field_exact_name(Node) ->
    case unwrap(Node) of
        {map_field_exact, _, Name, _} ->
            Name;
        Node1 ->
            (data(Node1))#map_field_exact.name
    end.


%% =====================================================================
%% @doc Returns the value subtree of a `map_field_exact' node.
%%
%% @see map_field_exact/2

-spec map_field_exact_value(syntaxTree()) -> syntaxTree().

map_field_exact_value(Node) ->
    case unwrap(Node) of
        {map_field_exact, _, _, Value} ->
            Value;
        Node1 ->
            (data(Node1))#map_field_exact.value
    end.


%% =====================================================================
%% @doc Creates an abstract tuple. If `Elements' is
%% `[X1, ..., Xn]', the result represents
%% "<code>{<em>X1</em>, ..., <em>Xn</em>}</code>".
%%
%% Note: The Erlang language has distinct 1-tuples, i.e.,
%% `{X}' is always distinct from `X' itself.
%%
%% @see tuple_elements/1
%% @see tuple_size/1

%% type(Node) = tuple
%% data(Node) = Elements
%%
%%	Elements = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {tuple, Pos, Elements}
%%
%%	Elements = [erl_parse()]

-spec tuple([syntaxTree()]) -> syntaxTree().

tuple(List) ->
    tree(tuple, List).

revert_tuple(Node) ->
    Pos = get_pos(Node),
    {tuple, Pos, tuple_elements(Node)}.


%% =====================================================================
%% @doc Returns the list of element subtrees of a `tuple' node.
%%
%% @see tuple/1

-spec tuple_elements(syntaxTree()) -> [syntaxTree()].

tuple_elements(Node) ->
    case unwrap(Node) of
	{tuple, _, List} ->
	    List;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Returns the number of elements of a `tuple' node.
%%
%% Note: this is equivalent to
%% `length(tuple_elements(Node))', but potentially more
%% efficient.
%%
%% @see tuple/1
%% @see tuple_elements/1

-spec tuple_size(syntaxTree()) -> non_neg_integer().

tuple_size(Node) ->
    length(tuple_elements(Node)).


%% =====================================================================
%% @equiv list(List, none)

-spec list([syntaxTree()]) -> syntaxTree().

list(List) ->
    list(List, none).


%% =====================================================================
%% @doc Constructs an abstract list skeleton. The result has type
%% `list' or `nil'. If `List' is a
%% nonempty list `[E1, ..., En]', the result has type
%% `list' and represents either "<code>[<em>E1</em>, ...,
%% <em>En</em>]</code>", if `Tail' is `none', or
%% otherwise "<code>[<em>E1</em>, ..., <em>En</em> |
%% <em>Tail</em>]</code>". If `List' is the empty list,
%% `Tail' <em>must</em> be `none', and in that
%% case the result has type `nil' and represents
%% "`[]'" (see {@link nil/0}).
%%
%% The difference between lists as semantic objects (built up of
%% individual "cons" and "nil" terms) and the various syntactic forms
%% for denoting lists may be bewildering at first. This module provides
%% functions both for exact control of the syntactic representation as
%% well as for the simple composition and deconstruction in terms of
%% cons and head/tail operations.
%%
%% Note: in `list(Elements, none)', the "nil" list
%% terminator is implicit and has no associated information (see
%% {@link get_attrs/1}), while in the seemingly equivalent
%% `list(Elements, Tail)' when `Tail' has type
%% `nil', the list terminator subtree `Tail' may
%% have attached attributes such as position, comments, and annotations,
%% which will be preserved in the result.
%%
%% @see nil/0
%% @see list/1
%% @see list_prefix/1
%% @see list_suffix/1
%% @see cons/2
%% @see list_head/1
%% @see list_tail/1
%% @see is_list_skeleton/1
%% @see is_proper_list/1
%% @see list_elements/1
%% @see list_length/1
%% @see normalize_list/1
%% @see compact_list/1
%% @see get_attrs/1

-record(list, {prefix :: [syntaxTree()], suffix :: 'none' | syntaxTree()}).

%% type(Node) = list
%% data(Node) = #list{prefix :: Elements, suffix :: Tail}
%%
%%	    Elements = [syntaxTree()]
%%	    Tail = none | syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {cons, Pos, Head, Tail}
%%
%%	Head = Tail = [erl_parse()]
%%
%%	This represents `[<Head> | <Tail>]', or more generally `[<Head>
%%	<Suffix>]' where the form of <Suffix> can depend on the
%%	structure of <Tail>; there is no fixed printed form.

-spec list([syntaxTree()], 'none' | syntaxTree()) -> syntaxTree().

list([], none) ->
    nil();
list(Elements, Tail) when Elements =/= [] ->
    tree(list, #list{prefix = Elements, suffix = Tail}).

revert_list(Node) ->
    Pos = get_pos(Node),
    P = list_prefix(Node),
    S = case list_suffix(Node) of
	    none ->
		revert_nil(set_pos(nil(), Pos));
	    S1 ->
		S1
	end,
    lists:foldr(fun (X, A) ->
			{cons, Pos, X, A}
		end,
		S, P).

%% =====================================================================
%% @doc Creates an abstract empty list. The result represents
%% "`[]'". The empty list is traditionally called "nil".
%%
%% @see list/2
%% @see is_list_skeleton/1

%% type(Node) = nil
%% data(Node) = term()
%%
%% `erl_parse' representation:
%%
%% {nil, Pos}

-spec nil() -> syntaxTree().

nil() ->
    tree(nil).

revert_nil(Node) ->
    Pos = get_pos(Node),
    {nil, Pos}.


%% =====================================================================
%% @doc Returns the prefix element subtrees of a `list' node.
%% If `Node' represents "<code>[<em>E1</em>, ...,
%% <em>En</em>]</code>" or "<code>[<em>E1</em>, ..., <em>En</em> |
%% <em>Tail</em>]</code>", the returned value is `[E1, ...,
%% En]'.
%%
%% @see list/2

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


%% =====================================================================
%% @doc Returns the suffix subtree of a `list' node, if one
%% exists. If `Node' represents "<code>[<em>E1</em>, ...,
%% <em>En</em> | <em>Tail</em>]</code>", the returned value is
%% `Tail', otherwise, i.e., if `Node' represents
%% "<code>[<em>E1</em>, ..., <em>En</em>]</code>", `none' is
%% returned.
%%
%% Note that even if this function returns some `Tail'
%% that is not `none', the type of `Tail' can be
%% `nil', if the tail has been given explicitly, and the list
%% skeleton has not been compacted (see {@link compact_list/1}).
%%
%% @see list/2
%% @see nil/0
%% @see compact_list/1

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


%% =====================================================================
%% @doc "Optimising" list skeleton cons operation. Creates an abstract
%% list skeleton whose first element is `Head' and whose tail
%% corresponds to `Tail'. This is similar to
%% `list([Head], Tail)', except that `Tail' may
%% not be `none', and that the result does not necessarily
%% represent exactly "<code>[<em>Head</em> | <em>Tail</em>]</code>", but
%% may depend on the `Tail' subtree. E.g., if
%% `Tail' represents `[X, Y]', the result may
%% represent "<code>[<em>Head</em>, X, Y]</code>", rather than
%% "<code>[<em>Head</em> | [X, Y]]</code>". Annotations on
%% `Tail' itself may be lost if `Tail' represents
%% a list skeleton, but comments on `Tail' are propagated to
%% the result.
%%
%% @see list/2
%% @see list_head/1
%% @see list_tail/1

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


%% =====================================================================
%% @doc Returns the head element subtree of a `list' node. If
%% `Node' represents "<code>[<em>Head</em> ...]</code>", the
%% result will represent "<code><em>Head</em></code>".
%%
%% @see list/2
%% @see list_tail/1
%% @see cons/2

-spec list_head(syntaxTree()) -> syntaxTree().

list_head(Node) ->
    hd(list_prefix(Node)).


%% =====================================================================
%% @doc Returns the tail of a `list' node. If
%% `Node' represents a single-element list
%% "<code>[<em>E</em>]</code>", then the result has type
%% `nil', representing "`[]'". If
%% `Node' represents "<code>[<em>E1</em>, <em>E2</em>
%% ...]</code>", the result will represent "<code>[<em>E2</em>
%% ...]</code>", and if `Node' represents
%% "<code>[<em>Head</em> | <em>Tail</em>]</code>", the result will
%% represent "<code><em>Tail</em></code>".
%%
%% @see list/2
%% @see list_head/1
%% @see cons/2

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


%% =====================================================================
%% @doc Returns `true' if `Node' has type
%% `list' or `nil', otherwise `false'.
%%
%% @see list/2
%% @see nil/0

-spec is_list_skeleton(syntaxTree()) -> boolean().

is_list_skeleton(Node) ->
    case type(Node) of
	list -> true;
	nil -> true;
	_ -> false
    end.


%% =====================================================================
%% @doc Returns `true' if `Node' represents a
%% proper list, and `false' otherwise. A proper list is a
%% list skeleton either on the form "`[]'" or
%% "<code>[<em>E1</em>, ..., <em>En</em>]</code>", or "<code>[... |
%% <em>Tail</em>]</code>" where recursively `Tail' also
%% represents a proper list.
%%
%% Note: Since `Node' is a syntax tree, the actual
%% run-time values corresponding to its subtrees may often be partially
%% or completely unknown. Thus, if `Node' represents e.g.
%% "`[... | Ns]'" (where `Ns' is a variable), then
%% the function will return `false', because it is not known
%% whether `Ns' will be bound to a list at run-time. If
%% `Node' instead represents e.g. "`[1, 2, 3]'" or
%% "`[A | []]'", then the function will return
%% `true'.
%%
%% @see list/2

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


%% =====================================================================
%% @doc Returns the list of element subtrees of a list skeleton.
%% `Node' must represent a proper list. E.g., if
%% `Node' represents "<code>[<em>X1</em>, <em>X2</em> |
%% [<em>X3</em>, <em>X4</em> | []]</code>", then
%% `list_elements(Node)' yields the list `[X1, X2, X3, X4]'.
%%
%% @see list/2
%% @see is_proper_list/1

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


%% =====================================================================
%% @doc Returns the number of element subtrees of a list skeleton.
%% `Node' must represent a proper list. E.g., if
%% `Node' represents "`[X1 | [X2, X3 | [X4, X5,
%% X6]]]'", then `list_length(Node)' returns the
%% integer 6.
%%
%% Note: this is equivalent to
%% `length(list_elements(Node))', but potentially more
%% efficient.
%%
%% @see list/2
%% @see is_proper_list/1
%% @see list_elements/1

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


%% =====================================================================
%% @doc Expands an abstract list skeleton to its most explicit form. If
%% `Node' represents "<code>[<em>E1</em>, ..., <em>En</em> |
%% <em>Tail</em>]</code>", the result represents "<code>[<em>E1</em> |
%% ... [<em>En</em> | <em>Tail1</em>] ... ]</code>", where
%% `Tail1' is the result of
%% `normalize_list(Tail)'. If `Node' represents
%% "<code>[<em>E1</em>, ..., <em>En</em>]</code>", the result simply
%% represents "<code>[<em>E1</em> | ... [<em>En</em> | []] ...
%% ]</code>". If `Node' does not represent a list skeleton,
%% `Node' itself is returned.
%%
%% @see list/2
%% @see compact_list/1

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


%% =====================================================================
%% @doc Yields the most compact form for an abstract list skeleton. The
%% result either represents "<code>[<em>E1</em>, ..., <em>En</em> |
%% <em>Tail</em>]</code>", where `Tail' is not a list
%% skeleton, or otherwise simply "<code>[<em>E1</em>, ...,
%% <em>En</em>]</code>". Annotations on subtrees of `Node'
%% that represent list skeletons may be lost, but comments will be
%% propagated to the result. Returns `Node' itself if
%% `Node' does not represent a list skeleton.
%%
%% @see list/2
%% @see normalize_list/1

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


%% =====================================================================
%% @doc Creates an abstract binary-object template. If
%% `Fields' is `[F1, ..., Fn]', the result
%% represents "<code>&lt;&lt;<em>F1</em>, ...,
%% <em>Fn</em>&gt;&gt;</code>".
%%
%% @see binary_fields/1
%% @see binary_field/2

%% type(Node) = binary
%% data(Node) = Fields
%%
%%	Fields = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {bin, Pos, Fields}
%%
%%	Fields = [Field]
%%	Field = {bin_element, ...}
%%
%%	See `binary_field' for documentation on `erl_parse' binary
%%	fields (or "elements").

-spec binary([syntaxTree()]) -> syntaxTree().

binary(List) ->
    tree(binary, List).

revert_binary(Node) ->
    Pos = get_pos(Node),
    {bin, Pos, binary_fields(Node)}.


%% =====================================================================
%% @doc Returns the list of field subtrees of a `binary' node.
%%
%% @see binary/1
%% @see binary_field/2

-spec binary_fields(syntaxTree()) -> [syntaxTree()].

binary_fields(Node) ->
    case unwrap(Node) of
	{bin, _, List} ->
	    List;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @equiv binary_field(Body, [])

-spec binary_field(syntaxTree()) -> syntaxTree().

binary_field(Body) ->
    binary_field(Body, []).


%% =====================================================================
%% @doc Creates an abstract binary template field.
%% If `Size' is `none', this is equivalent to
%% "`binary_field(Body, Types)'", otherwise it is
%% equivalent to "`binary_field(size_qualifier(Body, Size),
%% Types)'".
%%
%% (This is a utility function.)
%%
%% @see binary/1
%% @see binary_field/2
%% @see size_qualifier/2

-spec binary_field(syntaxTree(), 'none' | syntaxTree(), [syntaxTree()]) ->
        syntaxTree().

binary_field(Body, none, Types) ->
    binary_field(Body, Types);
binary_field(Body, Size, Types) ->
    binary_field(size_qualifier(Body, Size), Types).


%% =====================================================================
%% @doc Creates an abstract binary template field. If
%% `Types' is the empty list, the result simply represents
%% "<code><em>Body</em></code>", otherwise, if `Types' is
%% `[T1, ..., Tn]', the result represents
%% "<code><em>Body</em>/<em>T1</em>-...-<em>Tn</em></code>".
%%
%% @see binary/1
%% @see binary_field/1
%% @see binary_field/3
%% @see binary_field_body/1
%% @see binary_field_types/1
%% @see binary_field_size/1

-record(binary_field, {body :: syntaxTree(), types :: [syntaxTree()]}).

%% type(Node) = binary_field
%% data(Node) = #binary_field{body :: Body, types :: Types}
%%
%%	    Body = syntaxTree()
%%	    Types = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {bin_element, Pos, Expr, Size, TypeList}
%%
%%	Expr = erl_parse()
%%	Size = default | erl_parse()
%%	TypeList = default | [Type] \ []
%%	Type = atom() | {atom(), integer()}

-spec binary_field(syntaxTree(), [syntaxTree()]) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the body subtree of a `binary_field'.
%%
%% @see binary_field/2

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


%% =====================================================================
%% @doc Returns the list of type-specifier subtrees of a
%% `binary_field' node. If `Node' represents
%% "<code>.../<em>T1</em>, ..., <em>Tn</em></code>", the result is
%% `[T1, ..., Tn]', otherwise the result is the empty list.
%%
%% @see binary_field/2

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


%% =====================================================================
%% @doc Returns the size specifier subtree of a
%% `binary_field' node, if any. If `Node'
%% represents "<code><em>Body</em>:<em>Size</em></code>" or
%% "<code><em>Body</em>:<em>Size</em>/<em>T1</em>, ...,
%% <em>Tn</em></code>", the result is `Size', otherwise
%% `none' is returned.
%%
%% (This is a utility function.)
%%
%% @see binary_field/2
%% @see binary_field/3

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


%% =====================================================================
%% @doc Creates an abstract size qualifier. The result represents
%% "<code><em>Body</em>:<em>Size</em></code>".
%%
%% @see size_qualifier_body/1
%% @see size_qualifier_argument/1

-record(size_qualifier, {body :: syntaxTree(), size :: syntaxTree()}).

%% type(Node) = size_qualifier
%% data(Node) = #size_qualifier{body :: Body, size :: Size}
%%
%%	Body = Size = syntaxTree()

-spec size_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().

size_qualifier(Body, Size) ->
    tree(size_qualifier,
	 #size_qualifier{body = Body, size = Size}).


%% =====================================================================
%% @doc Returns the body subtree of a `size_qualifier' node.
%%
%% @see size_qualifier/2

-spec size_qualifier_body(syntaxTree()) -> syntaxTree().

size_qualifier_body(Node) ->
    (data(Node))#size_qualifier.body.


%% =====================================================================
%% @doc Returns the argument subtree (the size) of a
%% `size_qualifier' node.
%%
%% @see size_qualifier/2

-spec size_qualifier_argument(syntaxTree()) -> syntaxTree().

size_qualifier_argument(Node) ->
    (data(Node))#size_qualifier.size.


%% =====================================================================
%% @doc Creates an abstract error marker. The result represents an
%% occurrence of an error in the source code, with an associated Erlang
%% I/O ErrorInfo structure given by `Error' (see module
%% {@link //stdlib/io} for details). Error markers are regarded as source
%% code forms, but have no defined lexical form.
%%
%% Note: this is supported only for backwards compatibility with
%% existing parsers and tools.
%%
%% @see error_marker_info/1
%% @see warning_marker/1
%% @see eof_marker/0
%% @see is_form/1

%% type(Node) = error_marker
%% data(Node) = term()
%%
%% `erl_parse' representation:
%%
%% {error, Error}
%%
%%	Error = term()
%%
%%	Note that there is no position information for the node
%%	itself: `get_pos' and `set_pos' handle this as a special case.

-spec error_marker(term()) -> syntaxTree().

error_marker(Error) ->
    tree(error_marker, Error).

revert_error_marker(Node) ->
    %% Note that the position information of the node itself is not
    %% preserved.
    {error, error_marker_info(Node)}.


%% =====================================================================
%% @doc Returns the ErrorInfo structure of an `error_marker' node.
%%
%% @see error_marker/1

-spec error_marker_info(syntaxTree()) -> term().

error_marker_info(Node) ->
    case unwrap(Node) of
	{error, Error} ->
	    Error;
	T ->
	    data(T)
    end.


%% =====================================================================
%% @doc Creates an abstract warning marker. The result represents an
%% occurrence of a possible problem in the source code, with an
%% associated Erlang I/O ErrorInfo structure given by `Error'
%% (see module {@link //stdlib/io} for details). Warning markers are
%% regarded as source code forms, but have no defined lexical form.
%%
%% Note: this is supported only for backwards compatibility with
%% existing parsers and tools.
%%
%% @see warning_marker_info/1
%% @see error_marker/1
%% @see eof_marker/0
%% @see is_form/1

%% type(Node) = warning_marker
%% data(Node) = term()
%%
%% `erl_parse' representation:
%%
%% {warning, Error}
%%
%%	Error = term()
%%
%%	Note that there is no position information for the node
%%	itself: `get_pos' and `set_pos' handle this as a special case.

-spec warning_marker(term()) -> syntaxTree().

warning_marker(Warning) ->
    tree(warning_marker, Warning).

revert_warning_marker(Node) ->
    %% Note that the position information of the node itself is not
    %% preserved.
    {warning, warning_marker_info(Node)}.


%% =====================================================================
%% @doc Returns the ErrorInfo structure of a `warning_marker' node.
%%
%% @see warning_marker/1

-spec warning_marker_info(syntaxTree()) -> term().

warning_marker_info(Node) ->
    case unwrap(Node) of
	{warning, Error} ->
	    Error;
	T ->
	    data(T)
    end.


%% =====================================================================
%% @doc Creates an abstract end-of-file marker. This represents the
%% end of input when reading a sequence of source code forms. An
%% end-of-file marker is itself regarded as a source code form
%% (namely, the last in any sequence in which it occurs). It has no
%% defined lexical form.
%%
%% Note: this is retained only for backwards compatibility with
%% existing parsers and tools.
%%
%% @see error_marker/1
%% @see warning_marker/1
%% @see is_form/1

%% type(Node) = eof_marker
%% data(Node) = term()
%%
%% `erl_parse' representation:
%%
%% {eof, Pos}

-spec eof_marker() -> syntaxTree().

eof_marker() ->
    tree(eof_marker).

revert_eof_marker(Node) ->
    Pos = get_pos(Node),
    {eof, Pos}.


%% =====================================================================
%% @equiv attribute(Name, none)

-spec attribute(syntaxTree()) -> syntaxTree().

attribute(Name) ->
    attribute(Name, none).


%% =====================================================================
%% @doc Creates an abstract program attribute. If
%% `Arguments' is `[A1, ..., An]', the result
%% represents "<code>-<em>Name</em>(<em>A1</em>, ...,
%% <em>An</em>).</code>". Otherwise, if `Arguments' is
%% `none', the result represents
%% "<code>-<em>Name</em>.</code>". The latter form makes it possible
%% to represent preprocessor directives such as
%% "`-endif.'". Attributes are source code forms.
%%
%% Note: The preprocessor macro definition directive
%% "<code>-define(<em>Name</em>, <em>Body</em>).</code>" has relatively
%% few requirements on the syntactical form of `Body' (viewed
%% as a sequence of tokens). The `text' node type can be used
%% for a `Body' that is not a normal Erlang construct.
%%
%% @see attribute/1
%% @see attribute_name/1
%% @see attribute_arguments/1
%% @see text/1
%% @see is_form/1

-record(attribute, {name :: syntaxTree(), args :: 'none' | [syntaxTree()]}).

%% type(Node) = attribute
%% data(Node) = #attribute{name :: Name, args :: Arguments}
%%
%%	Name = syntaxTree()
%%	Arguments = none | [syntaxTree()]
%%
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

-spec attribute(syntaxTree(), 'none' | [syntaxTree()]) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the name subtree of an `attribute' node.
%%
%% @see attribute/1

-spec attribute_name(syntaxTree()) -> syntaxTree().

attribute_name(Node) ->
    case unwrap(Node) of
	{attribute, Pos, Name, _} ->
	    set_pos(atom(Name), Pos);
	Node1 ->
	    (data(Node1))#attribute.name
    end.


%% =====================================================================
%% @doc Returns the list of argument subtrees of an
%% `attribute' node, if any. If `Node'
%% represents "<code>-<em>Name</em>.</code>", the result is
%% `none'. Otherwise, if `Node' represents
%% "<code>-<em>Name</em>(<em>E1</em>, ..., <em>En</em>).</code>",
%% `[E1, ..., E1]' is returned.
%%
%% @see attribute/1

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


%% =====================================================================
%% @doc Creates an abstract arity qualifier. The result represents
%% "<code><em>Body</em>/<em>Arity</em></code>".
%%
%% @see arity_qualifier_body/1
%% @see arity_qualifier_argument/1

-record(arity_qualifier, {body :: syntaxTree(), arity :: syntaxTree()}).

%% type(Node) = arity_qualifier
%% data(Node) = #arity_qualifier{body :: Body, arity :: Arity}
%%
%%	Body = Arity = syntaxTree()

-spec arity_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().

arity_qualifier(Body, Arity) ->
    tree(arity_qualifier,
	 #arity_qualifier{body = Body, arity = Arity}).


%% =====================================================================
%% @doc Returns the body subtree of an `arity_qualifier' node.
%%
%% @see arity_qualifier/2

-spec arity_qualifier_body(syntaxTree()) -> syntaxTree().

arity_qualifier_body(Node) ->
    (data(Node))#arity_qualifier.body.


%% =====================================================================
%% @doc Returns the argument (the arity) subtree of an
%% `arity_qualifier' node.
%%
%% @see arity_qualifier/2

-spec arity_qualifier_argument(syntaxTree()) -> syntaxTree().

arity_qualifier_argument(Node) ->
    (data(Node))#arity_qualifier.arity.


%% =====================================================================
%% @doc Creates an abstract module qualifier. The result represents
%% "<code><em>Module</em>:<em>Body</em></code>".
%%
%% @see module_qualifier_argument/1
%% @see module_qualifier_body/1

-record(module_qualifier, {module :: syntaxTree(), body :: syntaxTree()}).

%% type(Node) = module_qualifier
%% data(Node) = #module_qualifier{module :: Module, body :: Body}
%%
%%	Module = Body = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {remote, Pos, Module, Arg}
%%
%%	Module = Arg = erl_parse()

-spec module_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().

module_qualifier(Module, Body) ->
    tree(module_qualifier,
	 #module_qualifier{module = Module, body = Body}).

revert_module_qualifier(Node) ->
    Pos = get_pos(Node),
    Module = module_qualifier_argument(Node),
    Body = module_qualifier_body(Node),
    {remote, Pos, Module, Body}.


%% =====================================================================
%% @doc Returns the argument (the module) subtree of a
%% `module_qualifier' node.
%%
%% @see module_qualifier/2

-spec module_qualifier_argument(syntaxTree()) -> syntaxTree().

module_qualifier_argument(Node) ->
    case unwrap(Node) of
	{remote, _, Module, _} ->
	    Module;
	Node1 ->
	    (data(Node1))#module_qualifier.module
    end.


%% =====================================================================
%% @doc Returns the body subtree of a `module_qualifier' node.
%%
%% @see module_qualifier/2

-spec module_qualifier_body(syntaxTree()) -> syntaxTree().

module_qualifier_body(Node) ->
    case unwrap(Node) of
	{remote, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#module_qualifier.body
    end.


%% =====================================================================
%% @doc Creates an abstract function definition. If `Clauses'
%% is `[C1, ..., Cn]', the result represents
%% "<code><em>Name</em> <em>C1</em>; ...; <em>Name</em>
%% <em>Cn</em>.</code>". More exactly, if each `Ci'
%% represents "<code>(<em>Pi1</em>, ..., <em>Pim</em>) <em>Gi</em> ->
%% <em>Bi</em></code>", then the result represents
%% "<code><em>Name</em>(<em>P11</em>, ..., <em>P1m</em>) <em>G1</em> ->
%% <em>B1</em>; ...; <em>Name</em>(<em>Pn1</em>, ..., <em>Pnm</em>)
%% <em>Gn</em> -> <em>Bn</em>.</code>". Function definitions are source
%% code forms.
%%
%% @see function_name/1
%% @see function_clauses/1
%% @see function_arity/1
%% @see is_form/1

%% Don't use the name 'function' for this record, to avoid confusion with
%% the tuples on the form {function,Name,Arity} used by erl_parse.
-record(func, {name :: syntaxTree(), clauses :: [syntaxTree()]}).

%% type(Node) = function
%% data(Node) = #func{name :: Name, clauses :: Clauses}
%%
%%	Name = syntaxTree()
%%	Clauses = [syntaxTree()]
%%
%%	(There's no real point in precomputing and storing the arity,
%%	and passing it as a constructor argument makes it possible to
%%	end up with an inconsistent value. Besides, some people might
%%	want to check all clauses, and not just the first, so the
%%	computation is not obvious.)
%%
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

-spec function(syntaxTree(), [syntaxTree()]) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the name subtree of a `function' node.
%%
%% @see function/2

-spec function_name(syntaxTree()) -> syntaxTree().

function_name(Node) ->
    case unwrap(Node) of
	{function, Pos, Name, _, _} ->
	    set_pos(atom(Name), Pos);
	Node1 ->
	    (data(Node1))#func.name
    end.


%% =====================================================================
%% @doc Returns the list of clause subtrees of a `function' node.
%%
%% @see function/2

-spec function_clauses(syntaxTree()) -> [syntaxTree()].

function_clauses(Node) ->
    case unwrap(Node) of
	{function, _, _, _, Clauses} ->
	    Clauses;
	Node1 ->
	    (data(Node1))#func.clauses
    end.


%% =====================================================================
%% @doc Returns the arity of a `function' node. The result
%% is the number of parameter patterns in the first clause of the
%% function; subsequent clauses are ignored.
%%
%% An exception is thrown if `function_clauses(Node)'
%% returns an empty list, or if the first element of that list is not
%% a syntax tree `C' of type `clause' such that
%% `clause_patterns(C)' is a nonempty list.
%%
%% @see function/2
%% @see function_clauses/1
%% @see clause/3
%% @see clause_patterns/1

-spec function_arity(syntaxTree()) -> arity().

function_arity(Node) ->
    %% Note that this never accesses the arity field of `erl_parse'
    %% function nodes.
    length(clause_patterns(hd(function_clauses(Node)))).


%% =====================================================================
%% @equiv clause([], Guard, Body)

-type guard() :: 'none' | syntaxTree() | [syntaxTree()] | [[syntaxTree()]].

-spec clause(guard(), [syntaxTree()]) -> syntaxTree().

clause(Guard, Body) ->
    clause([], Guard, Body).


%% =====================================================================
%% @doc Creates an abstract clause. If `Patterns' is
%% `[P1, ..., Pn]' and `Body' is `[B1, ...,
%% Bm]', then if `Guard' is `none', the
%% result represents "<code>(<em>P1</em>, ..., <em>Pn</em>) ->
%% <em>B1</em>, ..., <em>Bm</em></code>", otherwise, unless
%% `Guard' is a list, the result represents
%% "<code>(<em>P1</em>, ..., <em>Pn</em>) when <em>Guard</em> ->
%% <em>B1</em>, ..., <em>Bm</em></code>".
%%
%% For simplicity, the `Guard' argument may also be any
%% of the following:
%% <ul>
%%   <li>An empty list `[]'. This is equivalent to passing
%%       `none'.</li>
%%   <li>A nonempty list `[E1, ..., Ej]' of syntax trees.
%%       This is equivalent to passing `conjunction([E1, ...,
%%       Ej])'.</li>
%%   <li>A nonempty list of lists of syntax trees `[[E1_1, ...,
%%       E1_k1], ..., [Ej_1, ..., Ej_kj]]', which is equivalent
%%       to passing `disjunction([conjunction([E1_1, ...,
%%       E1_k1]), ..., conjunction([Ej_1, ..., Ej_kj])])'.</li>
%% </ul>
%%
%% @see clause/2
%% @see clause_patterns/1
%% @see clause_guard/1
%% @see clause_body/1

-record(clause, {patterns :: [syntaxTree()],
		 guard    :: guard(),
		 body     :: [syntaxTree()]}).

%% type(Node) = clause
%% data(Node) = #clause{patterns :: Patterns, guard :: Guard,
%%		        body :: Body}
%%
%%	Patterns = [syntaxTree()]
%%	Guard = syntaxTree() | none
%%	Body = [syntaxTree()]
%%
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

-spec clause([syntaxTree()], guard(), [syntaxTree()]) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the list of pattern subtrees of a `clause' node.
%%
%% @see clause/3

-spec clause_patterns(syntaxTree()) -> [syntaxTree()].

clause_patterns(Node) ->
    case unwrap(Node) of
	{clause, _, Patterns, _, _} ->
	    Patterns;
	Node1 ->
	    (data(Node1))#clause.patterns
    end.


%% =====================================================================
%% @doc Returns the guard subtree of a `clause' node, if
%% any. If `Node' represents "<code>(<em>P1</em>, ...,
%% <em>Pn</em>) when <em>Guard</em> -> <em>B1</em>, ...,
%% <em>Bm</em></code>", `Guard' is returned. Otherwise, the
%% result is `none'.
%%
%% @see clause/3

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


%% =====================================================================
%% @doc Return the list of body subtrees of a `clause' node.
%%
%% @see clause/3

-spec clause_body(syntaxTree()) -> [syntaxTree()].

clause_body(Node) ->
    case unwrap(Node) of
	{clause, _, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#clause.body
    end.


%% =====================================================================
%% @doc Creates an abstract disjunction. If `List' is
%% `[E1, ..., En]', the result represents
%% "<code><em>E1</em>; ...; <em>En</em></code>".
%%
%% @see disjunction_body/1
%% @see conjunction/1

%% type(Node) = disjunction
%% data(Node) = [syntaxTree()]

-spec disjunction([syntaxTree()]) -> syntaxTree().

disjunction(Tests) ->
    tree(disjunction, Tests).


%% =====================================================================
%% @doc Returns the list of body subtrees of a
%% `disjunction' node.
%%
%% @see disjunction/1

-spec disjunction_body(syntaxTree()) -> [syntaxTree()].

disjunction_body(Node) ->
    data(Node).


%% =====================================================================
%% @doc Creates an abstract conjunction. If `List' is
%% `[E1, ..., En]', the result represents
%% "<code><em>E1</em>, ..., <em>En</em></code>".
%%
%% @see conjunction_body/1
%% @see disjunction/1

%% type(Node) = conjunction
%% data(Node) = [syntaxTree()]

-spec conjunction([syntaxTree()]) -> syntaxTree().

conjunction(Tests) ->
    tree(conjunction, Tests).


%% =====================================================================
%% @doc Returns the list of body subtrees of a
%% `conjunction' node.
%%
%% @see conjunction/1

-spec conjunction_body(syntaxTree()) -> [syntaxTree()].

conjunction_body(Node) ->
    data(Node).


%% =====================================================================
%% @doc Creates an abstract catch-expression. The result represents
%% "<code>catch <em>Expr</em></code>".
%%
%% @see catch_expr_body/1

%% type(Node) = catch_expr
%% data(Node) = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {'catch', Pos, Expr}
%%
%%	Expr = erl_parse()

-spec catch_expr(syntaxTree()) -> syntaxTree().

catch_expr(Expr) ->
    tree(catch_expr, Expr).

revert_catch_expr(Node) ->
    Pos = get_pos(Node),
    Expr = catch_expr_body(Node),
    {'catch', Pos, Expr}.


%% =====================================================================
%% @doc Returns the body subtree of a `catch_expr' node.
%%
%% @see catch_expr/1

-spec catch_expr_body(syntaxTree()) -> syntaxTree().

catch_expr_body(Node) ->
    case unwrap(Node) of
	{'catch', _, Expr} ->
	    Expr;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Creates an abstract match-expression. The result represents
%% "<code><em>Pattern</em> = <em>Body</em></code>".
%%
%% @see match_expr_pattern/1
%% @see match_expr_body/1

-record(match_expr, {pattern :: syntaxTree(), body :: syntaxTree()}).

%% type(Node) = match_expr
%% data(Node) = #match_expr{pattern :: Pattern, body :: Body}
%%
%%	Pattern = Body = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {match, Pos, Pattern, Body}
%%
%%	Pattern = Body = erl_parse()

-spec match_expr(syntaxTree(), syntaxTree()) -> syntaxTree().

match_expr(Pattern, Body) ->
    tree(match_expr, #match_expr{pattern = Pattern, body = Body}).

revert_match_expr(Node) ->
    Pos = get_pos(Node),
    Pattern = match_expr_pattern(Node),
    Body = match_expr_body(Node),
    {match, Pos, Pattern, Body}.


%% =====================================================================
%% @doc Returns the pattern subtree of a `match_expr' node.
%%
%% @see match_expr/2

-spec match_expr_pattern(syntaxTree()) -> syntaxTree().

match_expr_pattern(Node) ->
    case unwrap(Node) of
	{match, _, Pattern, _} ->
	    Pattern;
	Node1 ->
	    (data(Node1))#match_expr.pattern
    end.


%% =====================================================================
%% @doc Returns the body subtree of a `match_expr' node.
%%
%% @see match_expr/2

-spec match_expr_body(syntaxTree()) -> syntaxTree().

match_expr_body(Node) ->
    case unwrap(Node) of
	{match, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#match_expr.body
    end.


%% =====================================================================
%% @doc Creates an abstract operator. The name of the operator is the
%% character sequence represented by `Name'. This is
%% analogous to the print name of an atom, but an operator is never
%% written within single-quotes; e.g., the result of
%% `operator('++')' represents "`++'" rather
%% than "`'++''".
%%
%% @see operator_name/1
%% @see operator_literal/1
%% @see atom/1

%% type(Node) = operator
%% data(Node) = atom()

-spec operator(atom() | string()) -> syntaxTree().

operator(Name) when is_atom(Name) ->
    tree(operator, Name);
operator(Name) ->
    tree(operator, list_to_atom(Name)).


%% =====================================================================
%% @doc Returns the name of an `operator' node. Note that
%% the name is returned as an atom.
%%
%% @see operator/1

-spec operator_name(syntaxTree()) -> atom().

operator_name(Node) ->
    data(Node).


%% =====================================================================
%% @doc Returns the literal string represented by an
%% `operator' node. This is simply the operator name as a string.
%%
%% @see operator/1

-spec operator_literal(syntaxTree()) -> string().

operator_literal(Node) ->
    atom_to_list(operator_name(Node)).


%% =====================================================================
%% @doc Creates an abstract infix operator expression. The result
%% represents "<code><em>Left</em> <em>Operator</em>
%% <em>Right</em></code>".
%%
%% @see infix_expr_left/1
%% @see infix_expr_right/1
%% @see infix_expr_operator/1
%% @see prefix_expr/2

-record(infix_expr, {operator :: syntaxTree(),
		     left     :: syntaxTree(),
		     right    :: syntaxTree()}).

%% type(Node) = infix_expr
%% data(Node) = #infix_expr{left :: Left, operator :: Operator,
%%		            right :: Right}
%%
%%	Left = Operator = Right = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {op, Pos, Operator, Left, Right}
%%
%%	Operator = atom()
%%	Left = Right = erl_parse()

-spec infix_expr(syntaxTree(), syntaxTree(), syntaxTree()) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the left argument subtree of an
%% `infix_expr' node.
%%
%% @see infix_expr/3

-spec infix_expr_left(syntaxTree()) -> syntaxTree().

infix_expr_left(Node) ->
    case unwrap(Node) of
	{op, _, _, Left, _} ->
	    Left;
	Node1 ->
	    (data(Node1))#infix_expr.left
    end.


%% =====================================================================
%% @doc Returns the operator subtree of an `infix_expr' node.
%%
%% @see infix_expr/3

-spec infix_expr_operator(syntaxTree()) -> syntaxTree().

infix_expr_operator(Node) ->
    case unwrap(Node) of
	{op, Pos, Operator, _, _} ->
	    set_pos(operator(Operator), Pos);
	Node1 ->
	    (data(Node1))#infix_expr.operator
    end.


%% =====================================================================
%% @doc Returns the right argument subtree of an
%% `infix_expr' node.
%%
%% @see infix_expr/3

-spec infix_expr_right(syntaxTree()) -> syntaxTree().

infix_expr_right(Node) ->
    case unwrap(Node) of
	{op, _, _, _, Right} ->
	    Right;
	Node1 ->
	    (data(Node1))#infix_expr.right
    end.


%% =====================================================================
%% @doc Creates an abstract prefix operator expression. The result
%% represents "<code><em>Operator</em> <em>Argument</em></code>".
%%
%% @see prefix_expr_argument/1
%% @see prefix_expr_operator/1
%% @see infix_expr/3

-record(prefix_expr, {operator :: syntaxTree(), argument :: syntaxTree()}).

%% type(Node) = prefix_expr
%% data(Node) = #prefix_expr{operator :: Operator,
%%		             argument :: Argument}
%%
%%	Operator = Argument = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {op, Pos, Operator, Arg}
%%
%%	Operator = atom()
%%	Argument = erl_parse()

-spec prefix_expr(syntaxTree(), syntaxTree()) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the operator subtree of a `prefix_expr' node.
%%
%% @see prefix_expr/2

-spec prefix_expr_operator(syntaxTree()) -> syntaxTree().

prefix_expr_operator(Node) ->
    case unwrap(Node) of
	{op, Pos, Operator, _} ->
	    set_pos(operator(Operator), Pos);
	Node1 ->
	    (data(Node1))#prefix_expr.operator
    end.


%% =====================================================================
%% @doc Returns the argument subtree of a `prefix_expr' node.
%%
%% @see prefix_expr/2

-spec prefix_expr_argument(syntaxTree()) -> syntaxTree().

prefix_expr_argument(Node) ->
    case unwrap(Node) of
	{op, _, _, Argument} ->
	    Argument;
	Node1 ->
	    (data(Node1))#prefix_expr.argument
    end.


%% =====================================================================
%% @equiv record_field(Name, none)

-spec record_field(syntaxTree()) -> syntaxTree().

record_field(Name) ->
    record_field(Name, none).


%% =====================================================================
%% @doc Creates an abstract record field specification. If
%% `Value' is `none', the result represents
%% simply "<code><em>Name</em></code>", otherwise it represents
%% "<code><em>Name</em> = <em>Value</em></code>".
%%
%% @see record_field_name/1
%% @see record_field_value/1
%% @see record_expr/3

-record(record_field, {name :: syntaxTree(), value :: 'none' | syntaxTree()}).

%% type(Node) = record_field
%% data(Node) = #record_field{name :: Name, value :: Value}
%%
%%	Name = syntaxTree()
%%	Value = none | syntaxTree()

-spec record_field(syntaxTree(), 'none' | syntaxTree()) -> syntaxTree().

record_field(Name, Value) ->
    tree(record_field, #record_field{name = Name, value = Value}).


%% =====================================================================
%% @doc Returns the name subtree of a `record_field' node.
%%
%% @see record_field/2

-spec record_field_name(syntaxTree()) -> syntaxTree().

record_field_name(Node) ->
    (data(Node))#record_field.name.


%% =====================================================================
%% @doc Returns the value subtree of a `record_field' node,
%% if any. If `Node' represents
%% "<code><em>Name</em></code>", `none' is
%% returned. Otherwise, if `Node' represents
%% "<code><em>Name</em> = <em>Value</em></code>", `Value'
%% is returned.
%%
%% @see record_field/2

-spec record_field_value(syntaxTree()) -> 'none' | syntaxTree().

record_field_value(Node) ->
    (data(Node))#record_field.value.


%% =====================================================================
%% @doc Creates an abstract record field index expression. The result
%% represents "<code>#<em>Type</em>.<em>Field</em></code>".
%%
%% (Note: the function name `record_index/2' is reserved
%% by the Erlang compiler, which is why that name could not be used
%% for this constructor.)
%%
%% @see record_index_expr_type/1
%% @see record_index_expr_field/1
%% @see record_expr/3

-record(record_index_expr, {type :: syntaxTree(), field :: syntaxTree()}).

%% type(Node) = record_index_expr
%% data(Node) = #record_index_expr{type :: Type, field :: Field}
%%
%%	Type = Field = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {record_index, Pos, Type, Field}
%%
%%	Type = atom()
%%	Field = erl_parse()

-spec record_index_expr(syntaxTree(), syntaxTree()) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the type subtree of a `record_index_expr' node.
%%
%% @see record_index_expr/2

-spec record_index_expr_type(syntaxTree()) -> syntaxTree().

record_index_expr_type(Node) ->
    case unwrap(Node) of
	{record_index, Pos, Type, _} ->
	    set_pos(atom(Type), Pos);
	Node1 ->
	    (data(Node1))#record_index_expr.type
    end.


%% =====================================================================
%% @doc Returns the field subtree of a `record_index_expr' node.
%%
%% @see record_index_expr/2

-spec record_index_expr_field(syntaxTree()) -> syntaxTree().

record_index_expr_field(Node) ->
    case unwrap(Node) of
	{record_index, _, _, Field} ->
	    Field;
	Node1 ->
	    (data(Node1))#record_index_expr.field
    end.


%% =====================================================================
%% @doc Creates an abstract record field access expression. The result
%% represents "<code><em>Argument</em>#<em>Type</em>.<em>Field</em></code>".
%%
%% @see record_access_argument/1
%% @see record_access_type/1
%% @see record_access_field/1
%% @see record_expr/3

-record(record_access, {argument :: syntaxTree(),
			type     :: syntaxTree(),
			field    :: syntaxTree()}).

%% type(Node) = record_access
%% data(Node) = #record_access{argument :: Argument, type :: Type,
%%			       field :: Field}
%%
%%	Argument = Type = Field = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {record_field, Pos, Argument, Type, Field}
%%
%%	Argument = Field = erl_parse()
%%	Type = atom()

-spec record_access(syntaxTree(), syntaxTree(), syntaxTree()) ->
        syntaxTree().

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


%% =====================================================================
%% @doc Returns the argument subtree of a `record_access' node.
%%
%% @see record_access/3

-spec record_access_argument(syntaxTree()) -> syntaxTree().

record_access_argument(Node) ->
    case unwrap(Node) of
	{record_field, _, Argument, _, _} ->
	    Argument;
	Node1 ->
	    (data(Node1))#record_access.argument
    end.


%% =====================================================================
%% @doc Returns the type subtree of a `record_access' node.
%%
%% @see record_access/3

-spec record_access_type(syntaxTree()) -> syntaxTree().

record_access_type(Node) ->
    case unwrap(Node) of
	{record_field, Pos, _, Type, _} ->
	    set_pos(atom(Type), Pos);
	Node1 ->
	    (data(Node1))#record_access.type
    end.


%% =====================================================================
%% @doc Returns the field subtree of a `record_access' node.
%%
%% @see record_access/3

-spec record_access_field(syntaxTree()) -> syntaxTree().

record_access_field(Node) ->
    case unwrap(Node) of
	{record_field, _, _, _, Field} ->
	    Field;
	Node1 ->
	    (data(Node1))#record_access.field
    end.


%% =====================================================================
%% @equiv record_expr(none, Type, Fields)

-spec record_expr(syntaxTree(), [syntaxTree()]) -> syntaxTree().

record_expr(Type, Fields) ->
    record_expr(none, Type, Fields).


%% =====================================================================
%% @doc Creates an abstract record expression. If `Fields' is
%% `[F1, ..., Fn]', then if `Argument' is
%% `none', the result represents
%% "<code>#<em>Type</em>{<em>F1</em>, ..., <em>Fn</em>}</code>",
%% otherwise it represents
%% "<code><em>Argument</em>#<em>Type</em>{<em>F1</em>, ...,
%% <em>Fn</em>}</code>".
%%
%% @see record_expr/2
%% @see record_expr_argument/1
%% @see record_expr_fields/1
%% @see record_expr_type/1
%% @see record_field/2
%% @see record_index_expr/2
%% @see record_access/3

-record(record_expr, {argument :: 'none' | syntaxTree(),
		      type     :: syntaxTree(),
		      fields   :: [syntaxTree()]}).

%% type(Node) = record_expr
%% data(Node) = #record_expr{argument :: Argument, type :: Type,
%%			     fields :: Fields}
%%
%%	Argument = none | syntaxTree()
%%	Type = syntaxTree
%%	Fields = [syntaxTree()]
%%
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

-spec record_expr('none' | syntaxTree(), syntaxTree(), [syntaxTree()]) ->
        syntaxTree().

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


%% =====================================================================
%% @doc Returns the argument subtree of a `record_expr' node,
%% if any. If `Node' represents
%% "<code>#<em>Type</em>{...}</code>", `none' is returned.
%% Otherwise, if `Node' represents
%% "<code><em>Argument</em>#<em>Type</em>{...}</code>",
%% `Argument' is returned.
%%
%% @see record_expr/3

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


%% =====================================================================
%% @doc Returns the type subtree of a `record_expr' node.
%%
%% @see record_expr/3

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


%% =====================================================================
%% @doc Returns the list of field subtrees of a
%% `record_expr' node.
%%
%% @see record_expr/3

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


%% =====================================================================
%% @doc Creates an abstract function application expression. If
%% `Module' is `none', this is call is equivalent
%% to `application(Function, Arguments)', otherwise it is
%% equivalent to `application(module_qualifier(Module, Function),
%% Arguments)'.
%%
%% (This is a utility function.)
%%
%% @see application/2
%% @see module_qualifier/2

-spec application('none' | syntaxTree(), syntaxTree(), [syntaxTree()]) ->
        syntaxTree().

application(none, Name, Arguments) ->
    application(Name, Arguments);
application(Module, Name, Arguments) ->
    application(module_qualifier(Module, Name), Arguments).


%% =====================================================================
%% @doc Creates an abstract function application expression. If
%% `Arguments' is `[A1, ..., An]', the result
%% represents "<code><em>Operator</em>(<em>A1</em>, ...,
%% <em>An</em>)</code>".
%%
%% @see application_operator/1
%% @see application_arguments/1
%% @see application/3

-record(application, {operator :: syntaxTree(), arguments :: [syntaxTree()]}).

%% type(Node) = application
%% data(Node) = #application{operator :: Operator,
%%			     arguments :: Arguments}
%%
%%	Operator = syntaxTree()
%%	Arguments = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {call, Pos, Operator, Args}
%%
%%	Operator = erl_parse()
%%	Arguments = [erl_parse()]

-spec application(syntaxTree(), [syntaxTree()]) -> syntaxTree().

application(Operator, Arguments) ->
    tree(application, #application{operator = Operator,
				   arguments = Arguments}).

revert_application(Node) ->
    Pos = get_pos(Node),
    Operator = application_operator(Node),
    Arguments = application_arguments(Node),
    {call, Pos, Operator, Arguments}.


%% =====================================================================
%% @doc Returns the operator subtree of an `application' node.
%%
%% Note: if `Node' represents
%% "<code><em>M</em>:<em>F</em>(...)</code>", then the result is the
%% subtree representing "<code><em>M</em>:<em>F</em></code>".
%%
%% @see application/2
%% @see module_qualifier/2

-spec application_operator(syntaxTree()) -> syntaxTree().

application_operator(Node) ->
    case unwrap(Node) of
	{call, _, Operator, _} ->
	    Operator;
	Node1 ->
	    (data(Node1))#application.operator
    end.


%% =====================================================================
%% @doc Returns the list of argument subtrees of an
%% `application' node.
%%
%% @see application/2

-spec application_arguments(syntaxTree()) -> [syntaxTree()].

application_arguments(Node) ->
    case unwrap(Node) of
	{call, _, _, Arguments} ->
	    Arguments;
	Node1 ->
	    (data(Node1))#application.arguments
    end.

%% =====================================================================
%% @doc Creates an abstract annotated type expression. The result
%% represents "<code><em>Name</em> :: <em>Type</em></code>".
%%
%% @see annotated_type_name/1
%% @see annotated_type_body/1

-record(annotated_type, {name :: syntaxTree(), body :: syntaxTree()}).

%% type(Node) = annotated_type
%% data(Node) = #annotated_type{name :: Name,
%%                              body :: Type}
%%
%%      Name = syntaxTree()
%%      Type = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {ann_type, Pos, [Name, Type]}
%%
%%      Name = erl_parse()
%%      Type = erl_parse()

-spec annotated_type(syntaxTree(), syntaxTree()) -> syntaxTree().

annotated_type(Name, Type) ->
    tree(annotated_type, #annotated_type{name = Name, body = Type}).

revert_annotated_type(Node) ->
    Pos = get_pos(Node),
    Name = annotated_type_name(Node),
    Type = annotated_type_body(Node),
    {ann_type, Pos, [Name, Type]}.


%% =====================================================================
%% @doc Returns the name subtree of an `annotated_type' node.
%%
%% @see annotated_type/2

-spec annotated_type_name(syntaxTree()) -> syntaxTree().

annotated_type_name(Node) ->
    case unwrap(Node) of
        {ann_type, _, [Name, _]} ->
            Name;
        Node1 ->
            (data(Node1))#annotated_type.name
    end.


%% =====================================================================
%% @doc Returns the type subtrees of an `annotated_type' node.
%%
%% @see annotated_type/2

-spec annotated_type_body(syntaxTree()) -> syntaxTree().

annotated_type_body(Node) ->
    case unwrap(Node) of
        {ann_type, _, [_, Type]} ->
            Type;
        Node1 ->
            (data(Node1))#annotated_type.body
    end.


%% =====================================================================
%% @doc Creates an abstract fun of any type. The result represents
%% "<code>fun()</code>".

%% type(Node) = fun_type
%%
%% `erl_parse' representation:
%%
%% {type, Pos, 'fun', []}

-spec fun_type() -> syntaxTree().

fun_type() ->
    tree(fun_type).

revert_fun_type(Node) ->
    Pos = get_pos(Node),
    {type, Pos, 'fun', []}.


%% =====================================================================
%% @doc Creates an abstract type application expression. If
%% `Module' is `none', this is call is equivalent
%% to `type_application(TypeName, Arguments)', otherwise it is
%% equivalent to `type_application(module_qualifier(Module, TypeName),
%% Arguments)'.
%%
%% (This is a utility function.)
%%
%% @see type_application/2
%% @see module_qualifier/2

-spec type_application('none' | syntaxTree(), syntaxTree(), [syntaxTree()]) ->
        syntaxTree().

type_application(none, TypeName, Arguments) ->
    type_application(TypeName, Arguments);
type_application(Module, TypeName, Arguments) ->
    type_application(module_qualifier(Module, TypeName), Arguments).


%% =====================================================================
%% @doc Creates an abstract type application expression. If `Arguments' is
%% `[T1, ..., Tn]', the result represents
%% "<code><em>TypeName</em>(<em>T1</em>, ...<em>Tn</em>)</code>".
%%
%% @see user_type_application/2
%% @see type_application/3
%% @see type_application_name/1
%% @see type_application_arguments/1

-record(type_application, {type_name :: syntaxTree(),
                           arguments :: [syntaxTree()]}).

%% type(Node) = type_application
%% data(Node) = #type_application{type_name :: TypeName,
%%                                arguments :: Arguments}
%%
%%      TypeName = syntaxTree()
%%      Arguments = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {remote, Pos, [Module, Name, Arguments]} |
%% {type, Pos, Name, Arguments}
%%
%%      Module = erl_parse()
%%      Name = atom()
%%      Arguments = [erl_parse()]

-spec type_application(syntaxTree(), [syntaxTree()]) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the type name subtree of a `type_application' node.
%%
%% @see type_application/2

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


%% =====================================================================
%% @doc Returns the arguments subtrees of a `type_application' node.
%%
%% @see type_application/2

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


%% =====================================================================
%% @doc Creates an abstract bitstring type. The result represents
%% "<code><em>&lt;&lt;_:M, _:_*N&gt;&gt;</em></code>".
%%
%% @see bitstring_type_m/1
%% @see bitstring_type_n/1

-record(bitstring_type, {m :: syntaxTree(), n :: syntaxTree()}).

%% type(Node) = bitstring_type
%% data(Node) = #bitstring_type{m :: M, n :: N}
%%
%%      M = syntaxTree()
%%      N = syntaxTree()
%%

-spec bitstring_type(syntaxTree(), syntaxTree()) -> syntaxTree().

bitstring_type(M, N) ->
    tree(bitstring_type, #bitstring_type{m = M, n =N}).

revert_bitstring_type(Node) ->
    Pos = get_pos(Node),
    M = bitstring_type_m(Node),
    N = bitstring_type_n(Node),
    {type, Pos, binary, [M, N]}.

%% =====================================================================
%% @doc Returns the number of start bits, `M',  of a `bitstring_type' node.
%%
%% @see bitstring_type/2

-spec bitstring_type_m(syntaxTree()) -> syntaxTree().

bitstring_type_m(Node) ->
    case unwrap(Node) of
        {type, _, binary, [M, _]} ->
            M;
        Node1 ->
            (data(Node1))#bitstring_type.m
    end.

%% =====================================================================
%% @doc Returns the segment size, `N', of a `bitstring_type' node.
%%
%% @see bitstring_type/2

-spec bitstring_type_n(syntaxTree()) -> syntaxTree().

bitstring_type_n(Node) ->
    case unwrap(Node) of
        {type, _, binary, [_, N]} ->
            N;
        Node1 ->
            (data(Node1))#bitstring_type.n
    end.


%% =====================================================================
%% @doc Creates an abstract constrained function type.
%% If `FunctionConstraint' is `[C1, ..., Cn]', the result represents
%% "<code><em>FunctionType</em> when <em>C1</em>, ...<em>Cn</em></code>".
%%
%% @see constrained_function_type_body/1
%% @see constrained_function_type_argument/1

-record(constrained_function_type, {body :: syntaxTree(),
                                    argument :: syntaxTree()}).

%% type(Node) = constrained_function_type
%% data(Node) = #constrained_function_type{body :: FunctionType,
%%                                         argument :: FunctionConstraint}
%%
%%      FunctionType = syntaxTree()
%%      FunctionConstraint = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {type, Pos, bounded_fun, [FunctionType, FunctionConstraint]}
%%
%%      FunctionType = erl_parse()
%%      FunctionConstraint = [erl_parse()]

-spec constrained_function_type(syntaxTree(), [syntaxTree()]) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the function type subtree of a
%% `constrained_function_type' node.
%%
%% @see constrained_function_type/2

-spec constrained_function_type_body(syntaxTree()) -> syntaxTree().

constrained_function_type_body(Node) ->
    case unwrap(Node) of
        {type, _, bounded_fun, [FunctionType, _]} ->
            FunctionType;
        Node1 ->
            (data(Node1))#constrained_function_type.body
    end.

%% =====================================================================
%% @doc Returns the function constraint subtree of a
%% `constrained_function_type' node.
%%
%% @see constrained_function_type/2

-spec constrained_function_type_argument(syntaxTree()) -> syntaxTree().

constrained_function_type_argument(Node) ->
    case unwrap(Node) of
        {type, _, bounded_fun, [_, FunctionConstraint]} ->
            conjunction(FunctionConstraint);
        Node1 ->
            (data(Node1))#constrained_function_type.argument
    end.


%% =====================================================================
%% @equiv function_type(any_arity, Type)

function_type(Type) ->
    function_type(any_arity, Type).

%% =====================================================================
%% @doc Creates an abstract function type. If `Arguments' is
%% `[T1, ..., Tn]', then if it occurs within a function
%% specification, the result represents
%% "<code>(<em>T1</em>, ...<em>Tn</em>) -> <em>Return</em></code>"; otherwise
%% it represents
%% "<code>fun((<em>T1</em>, ...<em>Tn</em>) -> <em>Return</em>)</code>".
%% If `Arguments' is `any_arity', it represents
%% "<code>fun((...) -> <em>Return</em>)</code>".
%%
%% Note that the `erl_parse' representation is identical for
%% "<code><em>FunctionType</em></code>" and
%% "<code>fun(<em>FunctionType</em>)</code>".
%%
%% @see function_type_arguments/1
%% @see function_type_return/1

-record(function_type, {arguments :: any_arity | [syntaxTree()],
                        return :: syntaxTree()}).

%% type(Node) = function_type
%% data(Node) = #function_type{arguments :: any | Arguments,
%%                             return :: Type}
%%
%%      Arguments = [syntaxTree()]
%%      Type = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {type, Pos, 'fun', [{type, Pos, product, Arguments}, Type]}
%% {type, Pos, 'fun', [{type, Pos, any}, Type]}
%%
%%      Arguments = [erl_parse()]
%%      Type = erl_parse()

-spec function_type('any_arity' | syntaxTree(), syntaxTree()) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the argument types subtrees of a `function_type' node.
%% If `Node' represents "<code>fun((...) -> <em>Return</em>)</code>",
%% `any_arity' is returned; otherwise, if `Node' represents
%% "<code>(<em>T1</em>, ...<em>Tn</em>) -> <em>Return</em></code>" or
%% "<code>fun((<em>T1</em>, ...<em>Tn</em>) -> <em>Return</em>)</code>",
%% `[T1, ..., Tn]' is returned.

%%
%% @see function_type/1
%% @see function_type/2

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

%% =====================================================================
%% @doc Returns the return type subtrees of a `function_type' node.
%%
%% @see function_type/1
%% @see function_type/2

-spec function_type_return(syntaxTree()) -> syntaxTree().

function_type_return(Node) ->
    case unwrap(Node) of
        {type, _, 'fun', [_, Type]} ->
            Type;
        Node1 ->
            (data(Node1))#function_type.return
    end.


%% =====================================================================
%% @doc Creates an abstract (subtype) constraint. The result represents
%% "<code><em>Name</em> :: <em>Type</em></code>".
%%
%% @see constraint_argument/1
%% @see constraint_body/1

-record(constraint, {name :: syntaxTree(),
                     types :: [syntaxTree()]}).

%% type(Node) = constraint
%% data(Node) = #constraint{name :: Name,
%%                          types :: [Type]}
%%
%%      Name = syntaxTree()
%%      Type = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {type, Pos, constraint, [Name, [Var, Type]]}
%%
%%      Name = {atom, Pos, is_subtype}
%%      Var = erl_parse()
%%      Type = erl_parse()

-spec constraint(syntaxTree(), [syntaxTree()]) -> syntaxTree().

constraint(Name, Types) ->
    tree(constraint,
         #constraint{name = Name, types = Types}).

revert_constraint(Node) ->
    Pos = get_pos(Node),
    Name = constraint_argument(Node),
    Types = constraint_body(Node),
    {type, Pos, constraint, [Name, Types]}.


%% =====================================================================
%% @doc Returns the name subtree of a `constraint' node.
%%
%% @see constraint/2

-spec constraint_argument(syntaxTree()) -> syntaxTree().

constraint_argument(Node) ->
    case unwrap(Node) of
        {type, _, constraint, [Name, _]} ->
            Name;
        Node1 ->
            (data(Node1))#constraint.name
    end.

%% =====================================================================
%% @doc Returns the type subtree of a `constraint' node.
%%
%% @see constraint/2

-spec constraint_body(syntaxTree()) -> [syntaxTree()].

constraint_body(Node) ->
    case unwrap(Node) of
        {type, _, constraint, [_, Types]} ->
            Types;
        Node1 ->
            (data(Node1))#constraint.types
    end.


%% =====================================================================
%% @doc Creates an abstract map type assoc field. The result represents
%% "<code><em>Name</em> => <em>Value</em></code>".
%%
%% @see map_type_assoc_name/1
%% @see map_type_assoc_value/1
%% @see map_type/1

-record(map_type_assoc, {name :: syntaxTree(), value :: syntaxTree()}).

%% `erl_parse' representation:
%%
%% {type, Pos, map_field_assoc, [Name, Value]}

-spec map_type_assoc(syntaxTree(), syntaxTree()) -> syntaxTree().

map_type_assoc(Name, Value) ->
    tree(map_type_assoc, #map_type_assoc{name = Name, value = Value}).

revert_map_type_assoc(Node) ->
    Pos = get_pos(Node),
    Name = map_type_assoc_name(Node),
    Value = map_type_assoc_value(Node),
    {type, Pos, map_field_assoc, [Name, Value]}.


%% =====================================================================
%% @doc Returns the name subtree of a `map_type_assoc' node.
%%
%% @see map_type_assoc/2

-spec map_type_assoc_name(syntaxTree()) -> syntaxTree().

map_type_assoc_name(Node) ->
    case unwrap(Node) of
        {type, _, map_field_assoc, [Name, _]} ->
            Name;
        Node1 ->
            (data(Node1))#map_type_assoc.name
    end.


%% =====================================================================
%% @doc Returns the value subtree of a `map_type_assoc' node.
%%
%% @see map_type_assoc/2

-spec map_type_assoc_value(syntaxTree()) -> syntaxTree().

map_type_assoc_value(Node) ->
    case unwrap(Node) of
        {type, _, map_field_assoc, [_, Value]} ->
            Value;
        Node1 ->
            (data(Node1))#map_type_assoc.value
    end.


%% =====================================================================
%% @doc Creates an abstract map type exact field. The result represents
%% "<code><em>Name</em> := <em>Value</em></code>".
%%
%% @see map_type_exact_name/1
%% @see map_type_exact_value/1
%% @see map_type/1

-record(map_type_exact, {name :: syntaxTree(), value :: syntaxTree()}).

%% `erl_parse' representation:
%%
%% {type, Pos, map_field_exact, [Name, Value]}

-spec map_type_exact(syntaxTree(), syntaxTree()) -> syntaxTree().

map_type_exact(Name, Value) ->
    tree(map_type_exact, #map_type_exact{name = Name, value = Value}).

revert_map_type_exact(Node) ->
    Pos = get_pos(Node),
    Name = map_type_exact_name(Node),
    Value = map_type_exact_value(Node),
    {type, Pos, map_field_exact, [Name, Value]}.


%% =====================================================================
%% @doc Returns the name subtree of a `map_type_exact' node.
%%
%% @see map_type_exact/2

-spec map_type_exact_name(syntaxTree()) -> syntaxTree().

map_type_exact_name(Node) ->
    case unwrap(Node) of
        {type, _, map_field_exact, [Name, _]} ->
            Name;
        Node1 ->
            (data(Node1))#map_type_exact.name
    end.


%% =====================================================================
%% @doc Returns the value subtree of a `map_type_exact' node.
%%
%% @see map_type_exact/2

-spec map_type_exact_value(syntaxTree()) -> syntaxTree().

map_type_exact_value(Node) ->
    case unwrap(Node) of
        {type, _, map_field_exact, [_, Value]} ->
            Value;
        Node1 ->
            (data(Node1))#map_type_exact.value
    end.


%% =====================================================================
%% @equiv map_type(any_size)

map_type() ->
    map_type(any_size).

%% =====================================================================
%% @doc Creates an abstract type map. If `Fields' is
%% `[F1, ..., Fn]', the result represents
%% "<code>#{<em>F1</em>, ..., <em>Fn</em>}</code>";
%% otherwise, if `Fields' is `any_size', it represents
%% "<code>map()</code>".
%%
%% @see map_type_fields/1

%% type(Node) = map_type
%% data(Node) = Fields
%%
%%      Fields = any_size | [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {type, Pos, map, [Field]}
%% {type, Pos, map, any}
%%
%%      Field = erl_parse()

-spec map_type('any_size' | [syntaxTree()]) -> syntaxTree().

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

%% =====================================================================
%% @doc Returns the list of field subtrees of a `map_type' node.
%% If `Node' represents "<code>map()</code>", `any_size' is returned;
%% otherwise, if `Node' represents
%% "<code>#{<em>F1</em>, ..., <em>Fn</em>}</code>",
%% `[F1, ..., Fn]' is returned.
%%
%% @see map_type/0
%% @see map_type/1

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
%% @doc Creates an abstract range type. The result represents
%% "<code><em>Low</em> .. <em>High</em></code>".
%%
%% @see integer_range_type_low/1
%% @see integer_range_type_high/1

-record(integer_range_type, {low :: syntaxTree(),
                             high :: syntaxTree()}).

%% type(Node) = integer_range_type
%% data(Node) = #integer_range_type{low :: Low, high :: High}
%%
%%      Low = syntaxTree()
%%      High = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {type, Pos, range, [Low, High]}
%%
%%      Low = erl_parse()
%%      High = erl_parse()

-spec integer_range_type(syntaxTree(), syntaxTree()) -> syntaxTree().

integer_range_type(Low, High) ->
    tree(integer_range_type, #integer_range_type{low = Low, high = High}).

revert_integer_range_type(Node) ->
    Pos = get_pos(Node),
    Low = integer_range_type_low(Node),
    High = integer_range_type_high(Node),
    {type, Pos, range, [Low, High]}.


%% =====================================================================
%% @doc Returns the low limit of an `integer_range_type' node.
%%
%% @see integer_range_type/2

-spec integer_range_type_low(syntaxTree()) -> syntaxTree().

integer_range_type_low(Node) ->
    case unwrap(Node) of
        {type, _, range, [Low, _]} ->
            Low;
        Node1 ->
            (data(Node1))#integer_range_type.low
    end.

%% =====================================================================
%% @doc Returns the high limit of an `integer_range_type' node.
%%
%% @see integer_range_type/2

-spec integer_range_type_high(syntaxTree()) -> syntaxTree().

integer_range_type_high(Node) ->
    case unwrap(Node) of
        {type, _, range, [_, High]} ->
            High;
        Node1 ->
            (data(Node1))#integer_range_type.high
    end.


%% =====================================================================
%% @doc Creates an abstract record type. If `Fields' is
%% `[F1, ..., Fn]', the result represents
%% "<code>#<em>Name</em>{<em>F1</em>, ..., <em>Fn</em>}</code>".
%%
%% @see record_type_name/1
%% @see record_type_fields/1

-record(record_type, {name :: syntaxTree(),
                      fields :: [syntaxTree()]}).

%% type(Node) = record_type
%% data(Node) = #record_type{name = Name, fields = Fields}
%%
%%      Name = syntaxTree()
%%      Fields = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {type, Pos, record, [Name|Fields]}
%%
%%      Name = erl_parse()
%%      Fields = [erl_parse()]

-spec record_type(syntaxTree(), [syntaxTree()]) -> syntaxTree().

record_type(Name, Fields) ->
    tree(record_type, #record_type{name = Name, fields = Fields}).

revert_record_type(Node) ->
    Pos = get_pos(Node),
    Name = record_type_name(Node),
    Fields = record_type_fields(Node),
    {type, Pos, record, [Name | Fields]}.


%% =====================================================================
%% @doc Returns the name subtree of a `record_type' node.
%%
%% @see record_type/2

-spec record_type_name(syntaxTree()) -> syntaxTree().

record_type_name(Node) ->
    case unwrap(Node) of
        {type, _, record, [Name|_]} ->
            Name;
        Node1 ->
            (data(Node1))#record_type.name
    end.

%% =====================================================================
%% @doc Returns the fields subtree of a `record_type' node.
%%
%% @see record_type/2

-spec record_type_fields(syntaxTree()) -> [syntaxTree()].

record_type_fields(Node) ->
    case unwrap(Node) of
        {type, _, record, [_|Fields]} ->
            Fields;
        Node1 ->
            (data(Node1))#record_type.fields
    end.


%% =====================================================================
%% @doc Creates an abstract record type field. The result represents
%% "<code><em>Name</em> :: <em>Type</em></code>".
%%
%% @see record_type_field_name/1
%% @see record_type_field_type/1

-record(record_type_field, {name :: syntaxTree(),
                            type :: syntaxTree()}).

%% type(Node) = record_type_field
%% data(Node) = #record_type_field{name = Name, type = Type}
%%
%%      Name = syntaxTree()
%%      Type = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {type, Pos, field_type, [Name, Type]}
%%
%%      Name = erl_parse()
%%      Type = erl_parse()

-spec record_type_field(syntaxTree(), syntaxTree()) -> syntaxTree().

record_type_field(Name, Type) ->
    tree(record_type_field, #record_type_field{name = Name, type = Type}).

revert_record_type_field(Node) ->
    Pos = get_pos(Node),
    Name = record_type_field_name(Node),
    Type = record_type_field_type(Node),
    {type, Pos, field_type, [Name, Type]}.


%% =====================================================================
%% @doc Returns the name subtree of a `record_type_field' node.
%%
%% @see record_type_field/2

-spec record_type_field_name(syntaxTree()) -> syntaxTree().

record_type_field_name(Node) ->
    case unwrap(Node) of
        {type, _, field_type, [Name, _]} ->
            Name;
        Node1 ->
            (data(Node1))#record_type_field.name
    end.

%% =====================================================================
%% @doc Returns the type subtree of a `record_type_field' node.
%%
%% @see record_type_field/2

-spec record_type_field_type(syntaxTree()) -> syntaxTree().

record_type_field_type(Node) ->
    case unwrap(Node) of
        {type, _, field_type, [_, Type]} ->
            Type;
        Node1 ->
            (data(Node1))#record_type_field.type
    end.


%% =====================================================================
%% @equiv tuple_type(any_size)

tuple_type() ->
    tuple_type(any_size).

%% =====================================================================
%% @doc Creates an abstract type tuple. If `Elements' is
%% `[T1, ..., Tn]', the result represents
%% "<code>{<em>T1</em>, ..., <em>Tn</em>}</code>";
%% otherwise, if `Elements' is `any_size', it represents
%% "<code>tuple()</code>".
%%
%% @see tuple_type_elements/1

%% type(Node) = tuple_type
%% data(Node) = Elements
%%
%%      Elements = any_size | [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {type, Pos, tuple, [Element]}
%% {type, Pos, tuple, any}
%%
%%      Element = erl_parse()

-spec tuple_type(any_size | [syntaxTree()]) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the list of type element subtrees of a `tuple_type' node.
%% If `Node' represents "<code>tuple()</code>", `any_size' is returned;
%% otherwise, if `Node' represents
%% "<code>{<em>T1</em>, ..., <em>Tn</em>}</code>",
%% `[T1, ..., Tn]' is returned.
%%
%% @see tuple_type/0
%% @see tuple_type/1

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
%% @doc Creates an abstract type union. If `Types' is
%% `[T1, ..., Tn]', the result represents
%% "<code><em>T1</em> | ... | <em>Tn</em></code>".
%%
%% @see type_union_types/1

%% type(Node) = type_union
%% data(Node) = Types
%%
%%      Types = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {type, Pos, union, Elements}
%%
%%      Elements = [erl_parse()]

-spec type_union([syntaxTree()]) -> syntaxTree().

type_union(Types) ->
    tree(type_union, Types).

revert_type_union(Node) ->
    Pos = get_pos(Node),
    {type, Pos, union, type_union_types(Node)}.


%% =====================================================================
%% @doc Returns the list of type subtrees of a `type_union' node.
%%
%% @see type_union/1

-spec type_union_types(syntaxTree()) -> [syntaxTree()].

type_union_types(Node) ->
    case unwrap(Node) of
        {type, _, union, Types} when is_list(Types) ->
            Types;
        Node1 ->
            data(Node1)
    end.


%% =====================================================================
%% @doc Creates an abstract user type. If `Arguments' is
%% `[T1, ..., Tn]', the result represents
%% "<code><em>TypeName</em>(<em>T1</em>, ...<em>Tn</em>)</code>".
%%
%% @see type_application/2
%% @see user_type_application_name/1
%% @see user_type_application_arguments/1

-record(user_type_application, {type_name :: syntaxTree(),
                                arguments :: [syntaxTree()]}).

%% type(Node) = user_type_application
%% data(Node) = #user_type_application{type_name :: TypeName,
%%                                     arguments :: Arguments}
%%
%%      TypeName = syntaxTree()
%%      Arguments = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {user_type, Pos, Name, Arguments}
%%
%%      Name = erl_parse()
%%      Arguments = [Type]
%%      Type = erl_parse()

-spec user_type_application(syntaxTree(), [syntaxTree()]) -> syntaxTree().

user_type_application(TypeName, Arguments) ->
    tree(user_type_application,
         #user_type_application{type_name = TypeName, arguments = Arguments}).

revert_user_type_application(Node) ->
    Pos = get_pos(Node),
    TypeName = user_type_application_name(Node),
    Arguments = user_type_application_arguments(Node),
    {user_type, Pos, atom_value(TypeName), Arguments}.


%% =====================================================================
%% @doc Returns the type name subtree of a `user_type_application' node.
%%
%% @see user_type_application/2

-spec user_type_application_name(syntaxTree()) -> syntaxTree().

user_type_application_name(Node) ->
    case unwrap(Node) of
        {user_type, Pos, Name, _} ->
            set_pos(atom(Name), Pos);
        Node1 ->
            (data(Node1))#user_type_application.type_name
    end.


%% =====================================================================
%% @doc Returns the arguments subtrees of a `user_type_application' node.
%%
%% @see user_type_application/2

-spec user_type_application_arguments(syntaxTree()) -> [syntaxTree()].

user_type_application_arguments(Node) ->
    case unwrap(Node) of
        {user_type, _, _, Arguments} ->
            Arguments;
        Node1 ->
            (data(Node1))#user_type_application.arguments
    end.


%% =====================================================================
%% @doc Creates an abstract typed record field specification. The
%% result represents "<code><em>Field</em> :: <em>Type</em></code>".
%%
%% @see typed_record_field_body/1
%% @see typed_record_field_type/1

-record(typed_record_field, {body :: syntaxTree(),
                             type :: syntaxTree()}).

%% type(Node) = typed_record_field
%% data(Node) = #typed_record_field{body :: Field
%%                                  type = Type}
%%
%%      Field = syntaxTree()
%%      Type = syntaxTree()

-spec typed_record_field(syntaxTree(), syntaxTree()) -> syntaxTree().

typed_record_field(Field, Type) ->
    tree(typed_record_field,
         #typed_record_field{body = Field, type = Type}).


%% =====================================================================
%% @doc Returns the field subtree of a `typed_record_field' node.
%%
%% @see typed_record_field/2

-spec typed_record_field_body(syntaxTree()) -> syntaxTree().

typed_record_field_body(Node) ->
    (data(Node))#typed_record_field.body.


%% =====================================================================
%% @doc Returns the type subtree of a `typed_record_field' node.
%%
%% @see typed_record_field/2

-spec typed_record_field_type(syntaxTree()) -> syntaxTree().

typed_record_field_type(Node) ->
    (data(Node))#typed_record_field.type.


%% =====================================================================
%% @doc Creates an abstract list comprehension. If `Body' is
%% `[E1, ..., En]', the result represents
%% "<code>[<em>Template</em> || <em>E1</em>, ..., <em>En</em>]</code>".
%%
%% @see list_comp_template/1
%% @see list_comp_body/1
%% @see generator/2

-record(list_comp, {template :: syntaxTree(), body :: [syntaxTree()]}).

%% type(Node) = list_comp
%% data(Node) = #list_comp{template :: Template, body :: Body}
%%
%%	Template = Node = syntaxTree()
%%	Body = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {lc, Pos, Template, Body}
%%
%%	Template = erl_parse()
%%	Body = [erl_parse()] \ []

-spec list_comp(syntaxTree(), [syntaxTree()]) -> syntaxTree().

list_comp(Template, Body) ->
    tree(list_comp, #list_comp{template = Template, body = Body}).

revert_list_comp(Node) ->
    Pos = get_pos(Node),
    Template = list_comp_template(Node),
    Body = list_comp_body(Node),
    {lc, Pos, Template, Body}.


%% =====================================================================
%% @doc Returns the template subtree of a `list_comp' node.
%%
%% @see list_comp/2

-spec list_comp_template(syntaxTree()) -> syntaxTree().

list_comp_template(Node) ->
    case unwrap(Node) of
	{lc, _, Template, _} ->
	    Template;
	Node1 ->
	    (data(Node1))#list_comp.template
    end.


%% =====================================================================
%% @doc Returns the list of body subtrees of a `list_comp' node.
%%
%% @see list_comp/2

-spec list_comp_body(syntaxTree()) -> [syntaxTree()].

list_comp_body(Node) ->
    case unwrap(Node) of
	{lc, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#list_comp.body
    end.

%% =====================================================================
%% @doc Creates an abstract binary comprehension. If `Body' is
%% `[E1, ..., En]', the result represents
%% "<code>&lt;&lt;<em>Template</em> || <em>E1</em>, ..., <em>En</em>&gt;&gt;</code>".
%%
%% @see binary_comp_template/1
%% @see binary_comp_body/1
%% @see generator/2

-record(binary_comp, {template :: syntaxTree(), body :: [syntaxTree()]}).

%% type(Node) = binary_comp
%% data(Node) = #binary_comp{template :: Template, body :: Body}
%%
%%	Template = Node = syntaxTree()
%%	Body = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {bc, Pos, Template, Body}
%%
%%	Template = erl_parse()
%%	Body = [erl_parse()] \ []

-spec binary_comp(syntaxTree(), [syntaxTree()]) -> syntaxTree().

binary_comp(Template, Body) ->
    tree(binary_comp, #binary_comp{template = Template, body = Body}).

revert_binary_comp(Node) ->
    Pos = get_pos(Node),
    Template = binary_comp_template(Node),
    Body = binary_comp_body(Node),
    {bc, Pos, Template, Body}.


%% =====================================================================
%% @doc Returns the template subtree of a `binary_comp' node.
%%
%% @see binary_comp/2

-spec binary_comp_template(syntaxTree()) -> syntaxTree().

binary_comp_template(Node) ->
    case unwrap(Node) of
	{bc, _, Template, _} ->
	    Template;
	Node1 ->
	    (data(Node1))#binary_comp.template
    end.


%% =====================================================================
%% @doc Returns the list of body subtrees of a `binary_comp' node.
%%
%% @see binary_comp/2

-spec binary_comp_body(syntaxTree()) -> [syntaxTree()].

binary_comp_body(Node) ->
    case unwrap(Node) of
	{bc, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#binary_comp.body
    end.


%% =====================================================================
%% @doc Creates an abstract generator. The result represents
%% "<code><em>Pattern</em> &lt;- <em>Body</em></code>".
%%
%% @see generator_pattern/1
%% @see generator_body/1
%% @see list_comp/2
%% @see binary_comp/2

-record(generator, {pattern :: syntaxTree(), body :: syntaxTree()}).

%% type(Node) = generator
%% data(Node) = #generator{pattern :: Pattern, body :: Body}
%%
%%	Pattern = Argument = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {generate, Pos, Pattern, Body}
%%
%%	Pattern = Body = erl_parse()

-spec generator(syntaxTree(), syntaxTree()) -> syntaxTree().

generator(Pattern, Body) ->
    tree(generator, #generator{pattern = Pattern, body = Body}).

revert_generator(Node) ->
    Pos = get_pos(Node),
    Pattern = generator_pattern(Node),
    Body = generator_body(Node),
    {generate, Pos, Pattern, Body}.


%% =====================================================================
%% @doc Returns the pattern subtree of a `generator' node.
%%
%% @see generator/2

-spec generator_pattern(syntaxTree()) -> syntaxTree().

generator_pattern(Node) ->
    case unwrap(Node) of
	{generate, _, Pattern, _} ->
	    Pattern;
	Node1 ->
	    (data(Node1))#generator.pattern
    end.


%% =====================================================================
%% @doc Returns the body subtree of a `generator' node.
%%
%% @see generator/2

-spec generator_body(syntaxTree()) -> syntaxTree().

generator_body(Node) ->
    case unwrap(Node) of
	{generate, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#generator.body
    end.


%% =====================================================================
%% @doc Creates an abstract binary_generator. The result represents
%% "<code><em>Pattern</em> &lt;- <em>Body</em></code>".
%%
%% @see binary_generator_pattern/1
%% @see binary_generator_body/1
%% @see list_comp/2
%% @see binary_comp/2

-record(binary_generator, {pattern :: syntaxTree(), body :: syntaxTree()}).

%% type(Node) = binary_generator
%% data(Node) = #binary_generator{pattern :: Pattern, body :: Body}
%%
%%	Pattern = Argument = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {b_generate, Pos, Pattern, Body}
%%
%%	Pattern = Body = erl_parse()

-spec binary_generator(syntaxTree(), syntaxTree()) -> syntaxTree().

binary_generator(Pattern, Body) ->
    tree(binary_generator, #binary_generator{pattern = Pattern, body = Body}).

revert_binary_generator(Node) ->
    Pos = get_pos(Node),
    Pattern = binary_generator_pattern(Node),
    Body = binary_generator_body(Node),
    {b_generate, Pos, Pattern, Body}.


%% =====================================================================
%% @doc Returns the pattern subtree of a `generator' node.
%%
%% @see binary_generator/2

-spec binary_generator_pattern(syntaxTree()) -> syntaxTree().

binary_generator_pattern(Node) ->
    case unwrap(Node) of
	{b_generate, _, Pattern, _} ->
	    Pattern;
	Node1 ->
	    (data(Node1))#binary_generator.pattern
    end.


%% =====================================================================
%% @doc Returns the body subtree of a `generator' node.
%%
%% @see binary_generator/2

-spec binary_generator_body(syntaxTree()) -> syntaxTree().

binary_generator_body(Node) ->
    case unwrap(Node) of
	{b_generate, _, _, Body} ->
	    Body;
	Node1 ->
	    (data(Node1))#binary_generator.body
    end.


%% =====================================================================
%% @doc Creates an abstract block expression. If `Body' is
%% `[B1, ..., Bn]', the result represents "<code>begin
%% <em>B1</em>, ..., <em>Bn</em> end</code>".
%%
%% @see block_expr_body/1

%% type(Node) = block_expr
%% data(Node) = Body
%%
%%	Body = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {block, Pos, Body}
%%
%%	    Body = [erl_parse()] \ []

-spec block_expr([syntaxTree()]) -> syntaxTree().

block_expr(Body) ->
    tree(block_expr, Body).

revert_block_expr(Node) ->
    Pos = get_pos(Node),
    Body = block_expr_body(Node),
    {block, Pos, Body}.


%% =====================================================================
%% @doc Returns the list of body subtrees of a `block_expr' node.
%%
%% @see block_expr/1

-spec block_expr_body(syntaxTree()) -> [syntaxTree()].

block_expr_body(Node) ->
    case unwrap(Node) of
	{block, _, Body} ->
	    Body;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Creates an abstract if-expression. If `Clauses' is
%% `[C1, ..., Cn]', the result represents "<code>if
%% <em>C1</em>; ...; <em>Cn</em> end</code>". More exactly, if each
%% `Ci' represents "<code>() <em>Gi</em> ->
%% <em>Bi</em></code>", then the result represents "<code>if
%% <em>G1</em> -> <em>B1</em>; ...; <em>Gn</em> -> <em>Bn</em>
%% end</code>".
%%
%% @see if_expr_clauses/1
%% @see clause/3
%% @see case_expr/2

%% type(Node) = if_expr
%% data(Node) = Clauses
%%
%%	Clauses = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {'if', Pos, Clauses}
%%
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%
%%	See `clause' for documentation on `erl_parse' clauses.

-spec if_expr([syntaxTree()]) -> syntaxTree().

if_expr(Clauses) ->
    tree(if_expr, Clauses).

revert_if_expr(Node) ->
    Pos = get_pos(Node),
    Clauses = [revert_clause(C) || C <- if_expr_clauses(Node)],
    {'if', Pos, Clauses}.


%% =====================================================================
%% @doc Returns the list of clause subtrees of an `if_expr' node.
%%
%% @see if_expr/1

-spec if_expr_clauses(syntaxTree()) -> [syntaxTree()].

if_expr_clauses(Node) ->
    case unwrap(Node) of
	{'if', _, Clauses} ->
	    Clauses;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Creates an abstract case-expression. If `Clauses' is
%% `[C1, ..., Cn]', the result represents "<code>case
%% <em>Argument</em> of <em>C1</em>; ...; <em>Cn</em> end</code>". More
%% exactly, if each `Ci' represents "<code>(<em>Pi</em>)
%% <em>Gi</em> -> <em>Bi</em></code>", then the result represents
%% "<code>case <em>Argument</em> of <em>P1</em> <em>G1</em> ->
%% <em>B1</em>; ...; <em>Pn</em> <em>Gn</em> -> <em>Bn</em> end</code>".
%%
%% @see case_expr_clauses/1
%% @see case_expr_argument/1
%% @see clause/3
%% @see if_expr/1
%% @see cond_expr/1

-record(case_expr, {argument :: syntaxTree(), clauses :: [syntaxTree()]}).

%% type(Node) = case_expr
%% data(Node) = #case_expr{argument :: Argument,
%%			   clauses :: Clauses}
%%
%%	Argument = syntaxTree()
%%	Clauses = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {'case', Pos, Argument, Clauses}
%%
%%	Argument = erl_parse()
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%
%%	See `clause' for documentation on `erl_parse' clauses.

-spec case_expr(syntaxTree(), [syntaxTree()]) -> syntaxTree().

case_expr(Argument, Clauses) ->
    tree(case_expr, #case_expr{argument = Argument,
			       clauses = Clauses}).

revert_case_expr(Node) ->
    Pos = get_pos(Node),
    Argument = case_expr_argument(Node),
    Clauses = [revert_clause(C) || C <- case_expr_clauses(Node)],
    {'case', Pos, Argument, Clauses}.


%% =====================================================================
%% @doc Returns the argument subtree of a `case_expr' node.
%%
%% @see case_expr/2

-spec case_expr_argument(syntaxTree()) -> syntaxTree().

case_expr_argument(Node) ->
    case unwrap(Node) of
	{'case', _, Argument, _} ->
	    Argument;
	Node1 ->
	    (data(Node1))#case_expr.argument
    end.


%% =====================================================================
%% @doc Returns the list of clause subtrees of a `case_expr' node.
%%
%% @see case_expr/2

-spec case_expr_clauses(syntaxTree()) -> [syntaxTree()].

case_expr_clauses(Node) ->
    case unwrap(Node) of
	{'case', _, _, Clauses} ->
	    Clauses;
	Node1 ->
	    (data(Node1))#case_expr.clauses
    end.


%% =====================================================================
%% @doc Creates an abstract cond-expression. If `Clauses' is
%% `[C1, ..., Cn]', the result represents "<code>cond
%% <em>C1</em>; ...; <em>Cn</em> end</code>". More exactly, if each
%% `Ci' represents "<code>() <em>Ei</em> ->
%% <em>Bi</em></code>", then the result represents "<code>cond
%% <em>E1</em> -> <em>B1</em>; ...; <em>En</em> -> <em>Bn</em>
%% end</code>".
%%
%% @see cond_expr_clauses/1
%% @see clause/3
%% @see case_expr/2

%% type(Node) = cond_expr
%% data(Node) = Clauses
%%
%%	Clauses = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {'cond', Pos, Clauses}
%%
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%
%%	See `clause' for documentation on `erl_parse' clauses.

-spec cond_expr([syntaxTree()]) -> syntaxTree().

cond_expr(Clauses) ->
    tree(cond_expr, Clauses).

revert_cond_expr(Node) ->
    Pos = get_pos(Node),
    Clauses = [revert_clause(C) || C <- cond_expr_clauses(Node)],
    {'cond', Pos, Clauses}.


%% =====================================================================
%% @doc Returns the list of clause subtrees of a `cond_expr' node.
%%
%% @see cond_expr/1

-spec cond_expr_clauses(syntaxTree()) -> [syntaxTree()].

cond_expr_clauses(Node) ->
    case unwrap(Node) of
	{'cond', _, Clauses} ->
	    Clauses;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @equiv receive_expr(Clauses, none, [])

-spec receive_expr([syntaxTree()]) -> syntaxTree().

receive_expr(Clauses) ->
    receive_expr(Clauses, none, []).


%% =====================================================================
%% @doc Creates an abstract receive-expression. If `Timeout'
%% is `none', the result represents "<code>receive
%% <em>C1</em>; ...; <em>Cn</em> end</code>" (the `Action'
%% argument is ignored). Otherwise, if `Clauses' is
%% `[C1, ..., Cn]' and `Action' is `[A1, ...,
%% Am]', the result represents "<code>receive <em>C1</em>; ...;
%% <em>Cn</em> after <em>Timeout</em> -> <em>A1</em>, ..., <em>Am</em>
%% end</code>". More exactly, if each `Ci' represents
%% "<code>(<em>Pi</em>) <em>Gi</em> -> <em>Bi</em></code>", then the
%% result represents "<code>receive <em>P1</em> <em>G1</em> ->
%% <em>B1</em>; ...; <em>Pn</em> <em>Gn</em> -> <em>Bn</em> ...
%% end</code>".
%%
%% Note that in Erlang, a receive-expression must have at least one
%% clause if no timeout part is specified.
%%
%% @see receive_expr_clauses/1
%% @see receive_expr_timeout/1
%% @see receive_expr_action/1
%% @see receive_expr/1
%% @see clause/3
%% @see case_expr/2

-record(receive_expr, {clauses :: [syntaxTree()],
		       timeout :: 'none' | syntaxTree(),
		       action  :: [syntaxTree()]}).

%% type(Node) = receive_expr
%% data(Node) = #receive_expr{clauses :: Clauses,
%%			      timeout :: Timeout,
%%			      action :: Action}
%%
%%	Clauses = [syntaxTree()]
%%	Timeout = none | syntaxTree()
%%	Action = [syntaxTree()]
%%
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

-spec receive_expr([syntaxTree()], 'none' | syntaxTree(), [syntaxTree()]) ->
        syntaxTree().

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


%% =====================================================================
%% @doc Returns the list of clause subtrees of a
%% `receive_expr' node.
%%
%% @see receive_expr/3

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


%% =====================================================================
%% @doc Returns the timeout subtree of a `receive_expr' node,
%% if any. If `Node' represents "<code>receive <em>C1</em>;
%% ...; <em>Cn</em> end</code>", `none' is returned.
%% Otherwise, if `Node' represents "<code>receive
%% <em>C1</em>; ...; <em>Cn</em> after <em>Timeout</em> -> ... end</code>",
%% `Timeout' is returned.
%%
%% @see receive_expr/3

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


%% =====================================================================
%% @doc Returns the list of action body subtrees of a
%% `receive_expr' node. If `Node' represents
%% "<code>receive <em>C1</em>; ...; <em>Cn</em> end</code>", this is the
%% empty list.
%%
%% @see receive_expr/3

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
%% @equiv try_expr(Body, [], Handlers)

-spec try_expr([syntaxTree()], [syntaxTree()]) -> syntaxTree().

try_expr(Body, Handlers) ->
    try_expr(Body, [], Handlers).


%% =====================================================================
%% @equiv try_expr(Body, Clauses, Handlers, [])

-spec try_expr([syntaxTree()], [syntaxTree()], [syntaxTree()]) -> syntaxTree().

try_expr(Body, Clauses, Handlers) ->
    try_expr(Body, Clauses, Handlers, []).


%% =====================================================================
%% @equiv try_expr(Body, [], [], After)

-spec try_after_expr([syntaxTree()], [syntaxTree()]) -> syntaxTree().

try_after_expr(Body, After) ->
    try_expr(Body, [], [], After).


%% =====================================================================
%% @doc Creates an abstract try-expression. If `Body' is
%% `[B1, ..., Bn]', `Clauses' is `[C1, ...,
%% Cj]', `Handlers' is `[H1, ..., Hk]', and
%% `After' is `[A1, ..., Am]', the result
%% represents "<code>try <em>B1</em>, ..., <em>Bn</em> of <em>C1</em>;
%% ...; <em>Cj</em> catch <em>H1</em>; ...; <em>Hk</em> after
%% <em>A1</em>, ..., <em>Am</em> end</code>". More exactly, if each
%% `Ci' represents "<code>(<em>CPi</em>) <em>CGi</em> ->
%% <em>CBi</em></code>", and each `Hi' represents
%% "<code>(<em>HPi</em>) <em>HGi</em> -> <em>HBi</em></code>", then the
%% result represents "<code>try <em>B1</em>, ..., <em>Bn</em> of
%% <em>CP1</em> <em>CG1</em> -> <em>CB1</em>; ...; <em>CPj</em>
%% <em>CGj</em> -> <em>CBj</em> catch <em>HP1</em> <em>HG1</em> ->
%% <em>HB1</em>; ...; <em>HPk</em> <em>HGk</em> -> <em>HBk</em> after
%% <em>A1</em>, ..., <em>Am</em> end</code>"; see
%% {@link case_expr/2}. If `Clauses' is the empty list,
%% the `of ...' section is left out. If `After' is
%% the empty list, the `after ...' section is left out. If
%% `Handlers' is the empty list, and `After' is
%% nonempty, the `catch ...' section is left out.
%%
%% @see try_expr_body/1
%% @see try_expr_clauses/1
%% @see try_expr_handlers/1
%% @see try_expr_after/1
%% @see try_expr/2
%% @see try_expr/3
%% @see try_after_expr/2
%% @see clause/3
%% @see class_qualifier/2
%% @see case_expr/2

-record(try_expr, {body     :: [syntaxTree()],
		   clauses  :: [syntaxTree()],
		   handlers :: [syntaxTree()],
		   'after'  :: [syntaxTree()]}).

%% type(Node) = try_expr
%% data(Node) = #try_expr{body :: Body,
%%			  clauses :: Clauses,
%%			  handlers :: Clauses,
%%			  after :: Body}
%%
%%	Body = syntaxTree()
%%	Clauses = [syntaxTree()]
%%
%% `erl_parse' representation:
%%
%% {'try', Pos, Body, Clauses, Handlers, After}
%%
%%	Body = [erl_parse()]
%%	Clauses = [Clause]
%%	Handlers = [Clause] \ []
%%	Clause = {clause, ...}
%%	After = [erl_parse()]
%%
%%	See `clause' for documentation on `erl_parse' clauses.

-spec try_expr([syntaxTree()], [syntaxTree()],
	       [syntaxTree()], [syntaxTree()]) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the list of body subtrees of a `try_expr' node.
%%
%% @see try_expr/4

-spec try_expr_body(syntaxTree()) -> [syntaxTree()].

try_expr_body(Node) ->
    case unwrap(Node) of
	{'try', _, Body, _, _, _} ->
	    Body;
	Node1 ->
	    (data(Node1))#try_expr.body
    end.


%% =====================================================================
%% @doc Returns the list of case-clause subtrees of a
%% `try_expr' node. If `Node' represents
%% "<code>try <em>Body</em> catch <em>H1</em>; ...; <em>Hn</em>
%% end</code>", the result is the empty list.
%%
%% @see try_expr/4

-spec try_expr_clauses(syntaxTree()) -> [syntaxTree()].

try_expr_clauses(Node) ->
    case unwrap(Node) of
	{'try', _, _, Clauses, _, _} ->
	    Clauses;
	Node1 ->
	    (data(Node1))#try_expr.clauses
    end.


%% =====================================================================
%% @doc Returns the list of handler-clause subtrees of a
%% `try_expr' node.
%%
%% @see try_expr/4

-spec try_expr_handlers(syntaxTree()) -> [syntaxTree()].

try_expr_handlers(Node) ->
    case unwrap(Node) of
	{'try', _, _, _, Handlers, _} ->
	    unfold_try_clauses(Handlers);
	Node1 ->
	    (data(Node1))#try_expr.handlers
    end.


%% =====================================================================
%% @doc Returns the list of "after" subtrees of a `try_expr' node.
%%
%% @see try_expr/4

-spec try_expr_after(syntaxTree()) -> [syntaxTree()].

try_expr_after(Node) ->
    case unwrap(Node) of
	{'try', _, _, _, _, After} ->
	    After;
	Node1 ->
	    (data(Node1))#try_expr.'after'
    end.


%% =====================================================================
%% @doc Creates an abstract class qualifier. The result represents
%% "<code><em>Class</em>:<em>Body</em></code>".
%%
%% @see class_qualifier_argument/1
%% @see class_qualifier_body/1
%% @see class_qualifier_stacktrace/1
%% @see try_expr/4

-record(class_qualifier, {class :: syntaxTree(),
                          body :: syntaxTree(),
                          stacktrace :: syntaxTree()}).

%% type(Node) = class_qualifier
%% data(Node) = #class_qualifier{class :: Class, body :: Body}
%%
%%	Class = Body = syntaxTree()

-spec class_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().

class_qualifier(Class, Body) ->
    Underscore = {var, get_pos(Body), '_'},
    tree(class_qualifier,
	 #class_qualifier{class = Class, body = Body,
                          stacktrace = Underscore}).

%% =====================================================================
%% @doc Creates an abstract class qualifier. The result represents
%% "<code><em>Class</em>:<em>Body</em>:<em>Stacktrace</em></code>".
%%
%% @see class_qualifier_argument/1
%% @see class_qualifier_body/1
%% @see try_expr/4

-spec class_qualifier(syntaxTree(), syntaxTree(), syntaxTree()) ->
                             syntaxTree().

class_qualifier(Class, Body, Stacktrace) ->
    tree(class_qualifier,
	 #class_qualifier{class = Class,
                          body = Body,
                          stacktrace = Stacktrace}).


%% =====================================================================
%% @doc Returns the argument (the class) subtree of a
%% `class_qualifier' node.
%%
%% @see class_qualifier/2

-spec class_qualifier_argument(syntaxTree()) -> syntaxTree().

class_qualifier_argument(Node) ->
    (data(Node))#class_qualifier.class.


%% =====================================================================
%% @doc Returns the body subtree of a `class_qualifier' node.
%%
%% @see class_qualifier/2

-spec class_qualifier_body(syntaxTree()) -> syntaxTree().

class_qualifier_body(Node) ->
    (data(Node))#class_qualifier.body.

%% =====================================================================
%% @doc Returns the stacktrace subtree of a `class_qualifier' node.
%%
%% @see class_qualifier/2

-spec class_qualifier_stacktrace(syntaxTree()) -> syntaxTree().

class_qualifier_stacktrace(Node) ->
    (data(Node))#class_qualifier.stacktrace.


%% =====================================================================
%% @doc Creates an abstract "implicit fun" expression. If
%% `Arity' is `none', this is equivalent to
%% `implicit_fun(Name)', otherwise it is equivalent to
%% `implicit_fun(arity_qualifier(Name, Arity))'.
%%
%% (This is a utility function.)
%%
%% @see implicit_fun/1
%% @see implicit_fun/3

-spec implicit_fun(syntaxTree(), 'none' | syntaxTree()) -> syntaxTree().

implicit_fun(Name, none) ->
    implicit_fun(Name);
implicit_fun(Name, Arity) ->
    implicit_fun(arity_qualifier(Name, Arity)).


%% =====================================================================
%% @doc Creates an abstract module-qualified "implicit fun" expression.
%% If `Module' is `none', this is equivalent to
%% `implicit_fun(Name, Arity)', otherwise it is equivalent to
%% `implicit_fun(module_qualifier(Module, arity_qualifier(Name,
%% Arity))'.
%%
%% (This is a utility function.)
%%
%% @see implicit_fun/1
%% @see implicit_fun/2

-spec implicit_fun('none' | syntaxTree(), syntaxTree(), syntaxTree()) ->
        syntaxTree().

implicit_fun(none, Name, Arity) ->
    implicit_fun(Name, Arity);
implicit_fun(Module, Name, Arity) ->
    implicit_fun(module_qualifier(Module, arity_qualifier(Name, Arity))).


%% =====================================================================
%% @doc Creates an abstract "implicit fun" expression. The result
%% represents "<code>fun <em>Name</em></code>". `Name' should
%% represent either <code><em>F</em>/<em>A</em></code> or
%% <code><em>M</em>:<em>F</em>/<em>A</em></code>
%%
%% @see implicit_fun_name/1
%% @see implicit_fun/2
%% @see implicit_fun/3
%% @see arity_qualifier/2
%% @see module_qualifier/2

%% type(Node) = implicit_fun
%% data(Node) = syntaxTree()
%%
%% `erl_parse' representation:
%%
%% {'fun', Pos, {function, Name, Arity}}
%% {'fun', Pos, {function, Module, Name, Arity}}
%%
%%	Module = atom()
%%	Name = atom()
%%	Arity = arity()

-spec implicit_fun(syntaxTree()) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the name subtree of an `implicit_fun' node.
%%
%% Note: if `Node' represents "<code>fun
%% <em>N</em>/<em>A</em></code>" or "<code>fun
%% <em>M</em>:<em>N</em>/<em>A</em></code>", then the result is the
%% subtree representing "<code><em>N</em>/<em>A</em></code>" or
%% "<code><em>M</em>:<em>N</em>/<em>A</em></code>", respectively.
%%
%% @see implicit_fun/1
%% @see arity_qualifier/2
%% @see module_qualifier/2

-spec implicit_fun_name(syntaxTree()) -> syntaxTree().

implicit_fun_name(Node) ->
    case unwrap(Node) of
	{'fun', Pos, {function, Atom, Arity}} ->
	    arity_qualifier(set_pos(atom(Atom), Pos),
			    set_pos(integer(Arity), Pos));
	{'fun', Pos, {function, Module, Atom, Arity}}
	  when is_atom(Module), is_atom(Atom), is_integer(Arity) ->
	    %% Backward compatibility with pre-R15 abstract format.
	    module_qualifier(set_pos(atom(Module), Pos),
			     arity_qualifier(
			       set_pos(atom(Atom), Pos),
			       set_pos(integer(Arity), Pos)));
	{'fun', _Pos, {function, Module, Atom, Arity}} ->
	    %% New in R15: fun M:F/A.
	    %% XXX: Perhaps set position for this as well?
	    module_qualifier(Module, arity_qualifier(Atom, Arity));
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Creates an abstract fun-expression. If `Clauses' is
%% `[C1, ..., Cn]', the result represents "<code>fun
%% <em>C1</em>; ...; <em>Cn</em> end</code>". More exactly, if each
%% `Ci' represents "<code>(<em>Pi1</em>, ..., <em>Pim</em>)
%% <em>Gi</em> -> <em>Bi</em></code>", then the result represents
%% "<code>fun (<em>P11</em>, ..., <em>P1m</em>) <em>G1</em> ->
%% <em>B1</em>; ...; (<em>Pn1</em>, ..., <em>Pnm</em>) <em>Gn</em> ->
%% <em>Bn</em> end</code>".
%%
%% @see fun_expr_clauses/1
%% @see fun_expr_arity/1

%% type(Node) = fun_expr
%% data(Node) = Clauses
%%
%%	Clauses = [syntaxTree()]
%%
%%	(See `function' for notes; e.g. why the arity is not stored.)
%%
%% `erl_parse' representation:
%%
%% {'fun', Pos, {clauses, Clauses}}
%%
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%
%%	See `clause' for documentation on `erl_parse' clauses.

-spec fun_expr([syntaxTree()]) -> syntaxTree().

fun_expr(Clauses) ->
    tree(fun_expr, Clauses).

revert_fun_expr(Node) ->
    Clauses = [revert_clause(C) || C <- fun_expr_clauses(Node)],
    Pos = get_pos(Node),
    {'fun', Pos, {clauses, Clauses}}.


%% =====================================================================
%% @doc Returns the list of clause subtrees of a `fun_expr' node.
%%
%% @see fun_expr/1

-spec fun_expr_clauses(syntaxTree()) -> [syntaxTree()].

fun_expr_clauses(Node) ->
    case unwrap(Node) of
	{'fun', _, {clauses, Clauses}} ->
	    Clauses;
	Node1 ->
	    data(Node1)
    end.


%% =====================================================================
%% @doc Returns the arity of a `fun_expr' node. The result is
%% the number of parameter patterns in the first clause of the
%% fun-expression; subsequent clauses are ignored.
%%
%% An exception is thrown if `fun_expr_clauses(Node)'
%% returns an empty list, or if the first element of that list is not a
%% syntax tree `C' of type `clause' such that
%% `clause_patterns(C)' is a nonempty list.
%%
%% @see fun_expr/1
%% @see fun_expr_clauses/1
%% @see clause/3
%% @see clause_patterns/1

-spec fun_expr_arity(syntaxTree()) -> arity().

fun_expr_arity(Node) ->
    length(clause_patterns(hd(fun_expr_clauses(Node)))).


%% =====================================================================
%% @doc Creates an abstract named fun-expression. If `Clauses' is
%% `[C1, ..., Cn]', the result represents "<code>fun
%% <em>Name</em> <em>C1</em>; ...; <em>Name</em> <em>Cn</em> end</code>".
%% More exactly, if each `Ci' represents
%% "<code>(<em>Pi1</em>, ..., <em>Pim</em>) <em>Gi</em> -> <em>Bi</em></code>",
%% then the result represents
%% "<code>fun <em>Name</em>(<em>P11</em>, ..., <em>P1m</em>) <em>G1</em> ->
%% <em>B1</em>; ...; <em>Name</em>(<em>Pn1</em>, ..., <em>Pnm</em>)
%% <em>Gn</em> -> <em>Bn</em> end</code>".
%%
%% @see named_fun_expr_name/1
%% @see named_fun_expr_clauses/1
%% @see named_fun_expr_arity/1

-record(named_fun_expr, {name :: syntaxTree(), clauses :: [syntaxTree()]}).

%% type(Node) = named_fun_expr
%% data(Node) = #named_fun_expr{name :: Name, clauses :: Clauses}
%%
%%	Name = syntaxTree()
%%	Clauses = [syntaxTree()]
%%
%%	(See `function' for notes; e.g. why the arity is not stored.)
%%
%% `erl_parse' representation:
%%
%% {named_fun, Pos, Name, Clauses}
%%
%%	Clauses = [Clause] \ []
%%	Clause = {clause, ...}
%%
%%	See `clause' for documentation on `erl_parse' clauses.

-spec named_fun_expr(syntaxTree(), [syntaxTree()]) -> syntaxTree().

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


%% =====================================================================
%% @doc Returns the name subtree of a `named_fun_expr' node.
%%
%% @see named_fun_expr/2

-spec named_fun_expr_name(syntaxTree()) -> syntaxTree().

named_fun_expr_name(Node) ->
    case unwrap(Node) of
	{named_fun, Pos, Name, _} ->
	    set_pos(variable(Name), Pos);
	Node1 ->
	    (data(Node1))#named_fun_expr.name
    end.


%% =====================================================================
%% @doc Returns the list of clause subtrees of a `named_fun_expr' node.
%%
%% @see named_fun_expr/2

-spec named_fun_expr_clauses(syntaxTree()) -> [syntaxTree()].

named_fun_expr_clauses(Node) ->
    case unwrap(Node) of
	{named_fun, _, _, Clauses} ->
	    Clauses;
	Node1 ->
	    (data(Node1))#named_fun_expr.clauses
    end.


%% =====================================================================
%% @doc Returns the arity of a `named_fun_expr' node. The result is
%% the number of parameter patterns in the first clause of the
%% named fun-expression; subsequent clauses are ignored.
%%
%% An exception is thrown if `named_fun_expr_clauses(Node)'
%% returns an empty list, or if the first element of that list is not a
%% syntax tree `C' of type `clause' such that
%% `clause_patterns(C)' is a nonempty list.
%%
%% @see named_fun_expr/2
%% @see named_fun_expr_clauses/1
%% @see clause/3
%% @see clause_patterns/1

-spec named_fun_expr_arity(syntaxTree()) -> arity().

named_fun_expr_arity(Node) ->
    length(clause_patterns(hd(named_fun_expr_clauses(Node)))).


%% =====================================================================
%% @doc Creates an abstract parenthesised expression. The result
%% represents "<code>(<em>Body</em>)</code>", independently of the
%% context.
%%
%% @see parentheses_body/1

%% type(Node) = parentheses
%% data(Node) = syntaxTree()

-spec parentheses(syntaxTree()) -> syntaxTree().

parentheses(Expr) ->
    tree(parentheses, Expr).

revert_parentheses(Node) ->
    parentheses_body(Node).


%% =====================================================================
%% @doc Returns the body subtree of a `parentheses' node.
%%
%% @see parentheses/1

-spec parentheses_body(syntaxTree()) -> syntaxTree().

parentheses_body(Node) ->
    data(Node).


%% =====================================================================
%% @equiv macro(Name, none)

-spec macro(syntaxTree()) -> syntaxTree().

macro(Name) ->
    macro(Name, none).


%% =====================================================================
%% @doc Creates an abstract macro application. If `Arguments'
%% is `none', the result represents
%% "<code>?<em>Name</em></code>", otherwise, if `Arguments'
%% is `[A1, ..., An]', the result represents
%% "<code>?<em>Name</em>(<em>A1</em>, ..., <em>An</em>)</code>".
%%
%% Notes: if `Arguments' is the empty list, the result
%% will thus represent "<code>?<em>Name</em>()</code>", including a pair
%% of matching parentheses.
%%
%% The only syntactical limitation imposed by the preprocessor on the
%% arguments to a macro application (viewed as sequences of tokens) is
%% that they must be balanced with respect to parentheses, brackets,
%% `begin ... end', `case ... end', etc. The
%% `text' node type can be used to represent arguments which
%% are not regular Erlang constructs.
%%
%% @see macro_name/1
%% @see macro_arguments/1
%% @see macro/1
%% @see text/1

-record(macro, {name :: syntaxTree(), arguments :: 'none' | [syntaxTree()]}).

%% type(Node) = macro
%% data(Node) = #macro{name :: Name, arguments :: Arguments}
%%
%%	Name = syntaxTree()
%%	Arguments = none | [syntaxTree()]

-spec macro(syntaxTree(), 'none' | [syntaxTree()]) -> syntaxTree().

macro(Name, Arguments) ->
    tree(macro, #macro{name = Name, arguments = Arguments}).


%% =====================================================================
%% @doc Returns the name subtree of a `macro' node.
%%
%% @see macro/2

-spec macro_name(syntaxTree()) -> syntaxTree().

macro_name(Node) ->
    (data(Node))#macro.name.


%% =====================================================================
%% @doc Returns the list of argument subtrees of a `macro'
%% node, if any. If `Node' represents
%% "<code>?<em>Name</em></code>", `none' is returned.
%% Otherwise, if `Node' represents
%% "<code>?<em>Name</em>(<em>A1</em>, ..., <em>An</em>)</code>",
%% `[A1, ..., An]' is returned.
%%
%% @see macro/2

-spec macro_arguments(syntaxTree()) -> 'none' | [syntaxTree()].

macro_arguments(Node) ->
    (data(Node))#macro.arguments.


%% =====================================================================
%% @doc Returns the syntax tree corresponding to an Erlang term.
%% `Term' must be a literal term, i.e., one that can be
%% represented as a source code literal. Thus, it may not contain a
%% process identifier, port, reference or function value as a
%% subterm. The function recognises printable strings, in order to get a
%% compact and readable representation. Evaluation fails with reason
%% `badarg' if `Term' is not a literal term.
%%
%% @see concrete/1
%% @see is_literal/1

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


%% =====================================================================
%% @doc Returns the Erlang term represented by a syntax tree. Evaluation
%% fails with reason `badarg' if `Node' does not
%% represent a literal term.
%%
%% Note: Currently, the set of syntax trees which have a concrete
%% representation is larger than the set of trees which can be built
%% using the function {@link abstract/1}. An abstract character
%% will be concretised as an integer, while {@link abstract/1} does
%% not at present yield an abstract character for any input. (Use the
%% {@link char/1} function to explicitly create an abstract
%% character.)
%%
%% Note: `arity_qualifier' nodes are recognized. This is to follow The
%% Erlang Parser when it comes to wild attributes: both {F, A} and F/A
%% are recognized, which makes it possible to turn wild attributes
%% into recognized attributes without at the same time making it
%% impossible to compile files using the new syntax with the old
%% version of the Erlang Compiler.
%%
%% @see abstract/1
%% @see is_literal/1
%% @see char/1

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
				   end, [], true),
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


%% =====================================================================
%% @doc Returns `true' if `Node' represents a
%% literal term, otherwise `false'. This function returns
%% `true' if and only if the value of
%% `concrete(Node)' is defined.
%%
%% @see abstract/1
%% @see concrete/1

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

%% =====================================================================
%% @doc Returns an `erl_parse'-compatible representation of a
%% syntax tree, if possible. If `Tree' represents a
%% well-formed Erlang program or expression, the conversion should work
%% without problems. Typically, {@link is_tree/1} yields
%% `true' if conversion failed (i.e., the result is still an
%% abstract syntax tree), and `false' otherwise.
%%
%% The {@link is_tree/1} test is not completely foolproof. For a
%% few special node types (e.g. `arity_qualifier'), if such a
%% node occurs in a context where it is not expected, it will be left
%% unchanged as a non-reverted subtree of the result. This can only
%% happen if `Tree' does not actually represent legal Erlang
%% code.
%%
%% @see revert_forms/1
%% @see //stdlib/erl_parse

-spec revert(syntaxTree()) -> syntaxTree().

revert(Node) ->
    case is_tree(Node) of
	false ->
	    %% Just remove any wrapper. `erl_parse' nodes never contain
	    %% abstract syntax tree nodes as subtrees.
	    unwrap(Node);
	true ->
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
	    end
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
	case_expr ->
	    revert_case_expr(Node);
	catch_expr ->
	    revert_catch_expr(Node);
	char ->
	    revert_char(Node);
	clause ->
	    revert_clause(Node);
	cond_expr ->
	    revert_cond_expr(Node);
        constrained_function_type ->
            revert_constrained_function_type(Node);
        constraint ->
            revert_constraint(Node);
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
        map_expr ->
            revert_map_expr(Node);
        map_field_assoc ->
            revert_map_field_assoc(Node);
        map_field_exact ->
            revert_map_field_exact(Node);
        map_type ->
            revert_map_type(Node);
        map_type_assoc ->
            revert_map_type_assoc(Node);
        map_type_exact ->
            revert_map_type_exact(Node);
	match_expr ->
	    revert_match_expr(Node);
	module_qualifier ->
	    revert_module_qualifier(Node);
	named_fun_expr ->
	    revert_named_fun_expr(Node);
	nil ->
	    revert_nil(Node);
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
	_ ->
	    %% Non-revertible new-form node
	    Node
    end.


%% =====================================================================
%% @doc Reverts a sequence of Erlang source code forms. The sequence can
%% be given either as a `form_list' syntax tree (possibly
%% nested), or as a list of "program form" syntax trees. If successful,
%% the corresponding flat list of `erl_parse'-compatible
%% syntax trees is returned (see {@link revert/1}). If some program
%% form could not be reverted, `{error, Form}' is thrown.
%% Standalone comments in the form sequence are discarded.
%%
%% @see revert/1
%% @see form_list/1
%% @see is_form/1

-type forms() :: syntaxTree() | [syntaxTree()].

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
	    case is_tree(T1) of
		true ->
		    throw({error, T1});
		false ->
		    [T1 | revert_forms_1(Ts)]
	    end
    end;
revert_forms_1([]) ->
    [].


%% =====================================================================
%% @doc Returns the grouped list of all subtrees of a syntax tree. If
%% `Node' is a leaf node (see {@link is_leaf/1}), this
%% is the empty list, otherwise the result is always a nonempty list,
%% containing the lists of subtrees of `Node', in
%% left-to-right order as they occur in the printed program text, and
%% grouped by category. Often, each group contains only a single
%% subtree.
%%
%% Depending on the type of `Node', the size of some
%% groups may be variable (e.g., the group consisting of all the
%% elements of a tuple), while others always contain the same number of
%% elements - usually exactly one (e.g., the group containing the
%% argument expression of a case-expression). Note, however, that the
%% exact structure of the returned list (for a given node type) should
%% in general not be depended upon, since it might be subject to change
%% without notice.
%%
%% The function {@link subtrees/1} and the constructor functions
%% {@link make_tree/2} and {@link update_tree/2} can be a
%% great help if one wants to traverse a syntax tree, visiting all its
%% subtrees, but treat nodes of the tree in a uniform way in most or all
%% cases. Using these functions makes this simple, and also assures that
%% your code is not overly sensitive to extensions of the syntax tree
%% data type, because any node types not explicitly handled by your code
%% can be left to a default case.
%%
%% For example:
%% ```postorder(F, Tree) ->
%%       F(case subtrees(Tree) of
%%           [] -> Tree;
%%           List -> update_tree(Tree,
%%                               [[postorder(F, Subtree)
%%                                 || Subtree &lt;- Group]
%%                                || Group &lt;- List])
%%         end).'''
%% maps the function `F' on `Tree' and all its
%% subtrees, doing a post-order traversal of the syntax tree. (Note the
%% use of {@link update_tree/2} to preserve node attributes.) For a
%% simple function like:
%% ```f(Node) ->
%%       case type(Node) of
%%           atom -> atom("a_" ++ atom_name(Node));
%%           _ -> Node
%%       end.'''
%% the call `postorder(fun f/1, Tree)' will yield a new
%% representation of `Tree' in which all atom names have been
%% extended with the prefix "a_", but nothing else (including comments,
%% annotations and line numbers) has been changed.
%%
%% @see make_tree/2
%% @see type/1
%% @see is_leaf/1
%% @see copy_attrs/2

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
		catch_expr ->
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
		cond_expr ->
		    [cond_expr_clauses(T)];
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
		module_qualifier ->
		    [[module_qualifier_argument(T)],
		     [module_qualifier_body(T)]];
		named_fun_expr ->
			[[named_fun_expr_name(T)],
			 named_fun_expr_clauses(T)];
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
                     user_type_application_arguments(T)]
	    end
    end.


%% =====================================================================
%% @doc Creates a syntax tree with the same type and attributes as the
%% given tree. This is equivalent to `copy_attrs(Node,
%% make_tree(type(Node), Groups))'.
%%
%% @see make_tree/2
%% @see copy_attrs/2
%% @see type/1

-spec update_tree(syntaxTree(), [[syntaxTree()]]) -> syntaxTree().

update_tree(Node, Groups) ->
    copy_attrs(Node, make_tree(type(Node), Groups)).


%% =====================================================================
%% @doc Creates a syntax tree with the given type and subtrees.
%% `Type' must be a node type name (see {@link type/1})
%% that does not denote a leaf node type (see {@link is_leaf/1}).
%% `Groups' must be a <em>nonempty</em> list of groups of
%% syntax trees, representing the subtrees of a node of the given type,
%% in left-to-right order as they would occur in the printed program
%% text, grouped by category as done by {@link subtrees/1}.
%%
%% The result of `copy_attrs(Node, make_tree(type(Node),
%% subtrees(Node)))' (see {@link update_tree/2}) represents
%% the same source code text as the original `Node', assuming
%% that `subtrees(Node)' yields a nonempty list. However, it
%% does not necessarily have the same data representation as
%% `Node'.
%%
%% @see update_tree/2
%% @see subtrees/1
%% @see type/1
%% @see is_leaf/1
%% @see copy_attrs/2

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
make_tree(cond_expr, [C]) -> cond_expr(C);
make_tree(conjunction, [E]) -> conjunction(E);
make_tree(constrained_function_type, [[F],C]) ->
    constrained_function_type(F, C);
make_tree(constraint, [[N], Ts]) -> constraint(N, Ts);
make_tree(disjunction, [E]) -> disjunction(E);
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
make_tree(map_expr, [Fs]) -> map_expr(Fs);
make_tree(map_expr, [[E], Fs]) -> map_expr(E, Fs);
make_tree(map_field_assoc, [[K], [V]]) -> map_field_assoc(K, V);
make_tree(map_field_exact, [[K], [V]]) -> map_field_exact(K, V);
make_tree(map_type, [Fs]) -> map_type(Fs);
make_tree(map_type_assoc, [[N],[V]]) -> map_type_assoc(N, V);
make_tree(map_type_exact, [[N],[V]]) -> map_type_exact(N, V);
make_tree(match_expr, [[P], [E]]) -> match_expr(P, E);
make_tree(named_fun_expr, [[N], C]) -> named_fun_expr(N, C);
make_tree(module_qualifier, [[M], [N]]) -> module_qualifier(M, N);
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
make_tree(user_type_application, [[N], Ts]) -> user_type_application(N, Ts).


%% =====================================================================
%% @doc Creates a meta-representation of a syntax tree. The result
%% represents an Erlang expression "<code><em>MetaTree</em></code>"
%% which, if evaluated, will yield a new syntax tree representing the
%% same source code text as `Tree' (although the actual data
%% representation may be different). The expression represented by
%% `MetaTree' is <em>implementation independent</em> with
%% regard to the data structures used by the abstract syntax tree
%% implementation. Comments attached to nodes of `Tree' will
%% be preserved, but other attributes are lost.
%%
%% Any node in `Tree' whose node type is
%% `variable' (see {@link type/1}), and whose list of
%% annotations (see {@link get_ann/1}) contains the atom
%% `meta_var', will remain unchanged in the resulting tree,
%% except that exactly one occurrence of `meta_var' is
%% removed from its annotation list.
%%
%% The main use of the function `meta/1' is to transform a
%% data structure `Tree', which represents a piece of program
%% code, into a form that is <em>representation independent when
%% printed</em>. E.g., suppose `Tree' represents a variable
%% named "V". Then (assuming a function `print/1' for
%% printing syntax trees), evaluating `print(abstract(Tree))'
%% - simply using {@link abstract/1} to map the actual data
%% structure onto a syntax tree representation - would output a string
%% that might look something like "`{tree, variable, ..., "V",
%% ...}'", which is obviously dependent on the implementation of
%% the abstract syntax trees. This could e.g. be useful for caching a
%% syntax tree in a file. However, in some situations like in a program
%% generator generator (with two "generator"), it may be unacceptable.
%% Using `print(meta(Tree))' instead would output a
%% <em>representation independent</em> syntax tree generating
%% expression; in the above case, something like
%% "`erl_syntax:variable("V")'".
%%
%% @see abstract/1
%% @see type/1
%% @see get_ann/1

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
%% @equiv tree(Type, [])

-spec tree(atom()) -> #tree{}.

tree(Type) ->
    tree(Type, []).

%% =====================================================================
%% @doc <em>For special purposes only</em>. Creates an abstract syntax
%% tree node with type tag `Type' and associated data
%% `Data'.
%%
%% This function and the related {@link is_tree/1} and
%% {@link data/1} provide a uniform way to extend the set of
%% `erl_parse' node types. The associated data is any term,
%% whose format may depend on the type tag.
%%
%% === Notes: ===
%% <ul>
%%  <li>Any nodes created outside of this module must have type tags
%%      distinct from those currently defined by this module; see
%%      {@link type/1} for a complete list.</li>
%%  <li>The type tag of a syntax tree node may also be used
%%      as a primary tag by the `erl_parse' representation;
%%      in that case, the selector functions for that node type
%%      <em>must</em> handle both the abstract syntax tree and the
%%      `erl_parse' form. The function `type(T)'
%%      should return the correct type tag regardless of the
%%      representation of `T', so that the user sees no
%%      difference between `erl_syntax' and
%%      `erl_parse' nodes.</li>
%% </ul>
%%
%% @see is_tree/1
%% @see data/1
%% @see type/1

-spec tree(atom(), term()) -> #tree{}.

tree(Type, Data) ->
    #tree{type = Type, data = Data}.


%% =====================================================================
%% @doc <em>For special purposes only</em>. Returns `true' if
%% `Tree' is an abstract syntax tree and `false'
%% otherwise.
%%
%% <em>Note</em>: this function yields `false' for all
%% "old-style" `erl_parse'-compatible "parse trees".
%%
%% @see tree/2

-spec is_tree(syntaxTree()) -> boolean().

is_tree(#tree{}) ->
    true;
is_tree(_) ->
    false.


%% =====================================================================
%% @doc <em>For special purposes only</em>. Returns the associated data
%% of a syntax tree node. Evaluation fails with reason
%% `badarg' if `is_tree(Node)' does not yield
%% `true'.
%%
%% @see tree/2

-spec data(syntaxTree()) -> term().

data(#tree{data = D}) -> D;
data(T) -> erlang:error({badarg, T}).


%% =====================================================================
%% Primitives for backwards compatibility; for internal use only
%% =====================================================================


%% =====================================================================
%% @doc Creates a wrapper structure around an `erl_parse'
%% "parse tree".
%%
%% This function and the related {@link unwrap/1} and
%% {@link is_wrapper/1} provide a uniform way to attach arbitrary
%% information to an `erl_parse' tree. Some information about
%% the encapsuled tree may be cached in the wrapper, such as the node
%% type. All functions on syntax trees must behave so that the user sees
%% no difference between wrapped and non-wrapped `erl_parse'
%% trees. <em>Attaching a wrapper onto another wrapper structure is an
%% error</em>.

-spec wrap(erl_parse()) -> #wrapper{}.

wrap(Node) ->
    %% We assume that Node is an old-school `erl_parse' tree.
    #wrapper{type = type(Node), attr = #attr{pos = get_pos(Node)},
	     tree = Node}.


%% =====================================================================
%% @doc Removes any wrapper structure, if present. If `Node'
%% is a wrapper structure, this function returns the wrapped
%% `erl_parse' tree; otherwise it returns `Node'
%% itself.

-spec unwrap(syntaxTree()) -> #tree{} | erl_parse().

unwrap(#wrapper{tree = Node}) -> Node;
unwrap(Node) -> Node.	 % This could also be a new-form node.


%% =====================================================================
%% @doc Returns `true' if the argument is a wrapper
%% structure, otherwise `false'.

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
