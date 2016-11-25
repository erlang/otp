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

%% @doc Inserting comments into abstract Erlang syntax trees
%%
%% <p>This module contains functions for inserting comments, described
%% by position, indentation and text, as attachments on an abstract
%% syntax tree, at the correct places.</p>

-module(erl_recomment).

-export([recomment_forms/2, quick_recomment_forms/2, recomment_tree/2]).


%% @type syntaxTree() = erl_syntax:syntaxTree(). An abstract syntax
%% tree. See the {@link erl_syntax} module for details.

%% =====================================================================
%% @spec quick_recomment_forms(Forms, Comments::[Comment]) ->
%%           syntaxTree()
%%
%%	    Forms = syntaxTree() | [syntaxTree()]
%%	    Comment = {Line, Column, Indentation, Text}
%%	    Line = integer()
%%	    Column = integer()
%%	    Indentation = integer()
%%	    Text = [string()]
%%
%% @doc Like {@link recomment_forms/2}, but only inserts top-level
%% comments. Comments within function definitions or declarations
%% ("forms") are simply ignored.

-spec quick_recomment_forms(erl_syntax:forms(), [erl_comment_scan:comment()]) ->
        erl_syntax:syntaxTree().

quick_recomment_forms(Tree, Cs) ->
    recomment_forms(Tree, Cs, false).


%% =====================================================================
%% @spec recomment_forms(Forms, Comments::[Comment]) -> syntaxTree()
%%
%%	    Forms = syntaxTree() | [syntaxTree()]
%%	    Comment = {Line, Column, Indentation, Text}
%%	    Line = integer()
%%	    Column = integer()
%%	    Indentation = integer()
%%	    Text = [string()]
%%
%% @doc Attaches comments to the syntax tree/trees representing a
%% program. The given <code>Forms</code> should be a single syntax tree
%% of type <code>form_list</code>, or a list of syntax trees
%% representing "program forms". The syntax trees must contain valid
%% position information (for details, see
%% <code>recomment_tree/2</code>). The result is a corresponding syntax
%% tree of type <code>form_list</code> in which all comments in the list
%% <code>Comments</code> have been attached at the proper places.
%%
%% <p>Assuming <code>Forms</code> represents a program (or any sequence
%% of "program forms"), any comments whose first lines are not directly
%% associated with a specific program form will become standalone
%% comments inserted between the neighbouring program forms.
%% Furthermore, comments whose column position is less than or equal to
%% one will not be attached to a program form that begins at a
%% conflicting line number (this can happen with preprocessor-generated
%% <code>line</code>-attributes).</p>
%%
%% <p>If <code>Forms</code> is a syntax tree of some other type than
%% <code>form_list</code>, the comments will be inserted directly using
%% <code>recomment_tree/2</code>, and any comments left over from that
%% process are added as postcomments on the result.</p>
%%
%% <p>Entries in <code>Comments</code> represent multi-line comments.
%% For each entry, <code>Line</code> is the line number and
%% <code>Column</code> the left column of the comment (the column of the
%% first comment-introducing "<code>%</code>" character).
%% <code>Indentation</code> is the number of character positions between
%% the last non-whitespace character before the comment (or the left
%% margin) and the left column of the comment. <code>Text</code> is a
%% list of strings representing the consecutive comment lines in
%% top-down order, where each string contains all characters following
%% (but not including) the comment-introducing "<code>%</code>" and up
%% to (but not including) the terminating newline. (Cf. module
%% <code>erl_comment_scan</code>.)</p>
%%
%% <p>Evaluation exits with reason <code>{bad_position, Pos}</code> if
%% the associated position information <code>Pos</code> of some subtree
%% in the input does not have a recognizable format, or with reason
%% <code>{bad_tree, L, C}</code> if insertion of a comment at line
%% <code>L</code>, column <code>C</code>, fails because the tree
%% structure is ill-formed.</p>
%%
%% @see erl_comment_scan
%% @see recomment_tree/2
%% @see quick_recomment_forms/2

-spec recomment_forms(erl_syntax:forms(), [erl_comment_scan:comment()]) ->
        erl_syntax:syntaxTree().

recomment_forms(Tree, Cs) ->
    recomment_forms(Tree, Cs, true).

recomment_forms(Tree, Cs, Insert) when is_list(Tree) ->
    recomment_forms(erl_syntax:form_list(Tree), Cs, Insert);
recomment_forms(Tree, Cs, Insert) ->
    case erl_syntax:type(Tree) of
	form_list ->
	    Tree1 = erl_syntax:flatten_form_list(Tree),
	    Node = build_tree(Tree1),
	    %% Here we make a small assumption about the substructure of
	    %% a `form_list' tree: it has exactly one group of subtrees.
	    [Node1] = node_subtrees(Node),
	    List = filter_forms(node_subtrees(Node1)),
	    List1 = recomment_forms_1(Cs, List, Insert),
	    revert_tree(set_node_subtrees(Node,
					  [set_node_subtrees(Node1,
							     List1)]));
	_ ->
	    %% Not a form list - just call `recomment_tree' and
	    %% append any leftover comments.
	    {Tree1, Cs1} = recomment_tree(Tree, Cs),
	    revert_tree(append_comments(Cs1, Tree1))
    end.

append_comments([C | Cs], Tree) ->
    append_comments(Cs, node_add_postcomment(C, Tree));
append_comments([], Tree) ->
    Tree.

%% This part goes over each comment in turn and inserts it into the
%% proper place in the given list of program forms:

recomment_forms_1([C | Cs], Ns, Insert) ->
    Ns1 = recomment_forms_2(C, Ns, Insert),
    recomment_forms_1(Cs, Ns1, Insert);
recomment_forms_1([], Ns, _Insert) ->
    Ns.

recomment_forms_2(C, [N | Ns] = Nodes, Insert) ->
    {L, Col, Ind, Text} = C,
    Min = node_min(N),
    Max = node_max(N),
    Delta = comment_delta(Text),
    Trailing = 
	case Ns of
	    [] -> true;
	    [Next | _] -> L + Delta < node_min(Next) - 2
	end,
    if L > Max + 1 ; L =:= Max + 1, not Trailing ->
	    [N | recomment_forms_2(C, Ns, Insert)];
       L + Delta < Min - 1 ->
	    %% At least one empty line between the current form
	    %% and the comment, so we make it a standalone.
	    [standalone_comment(C) | Nodes];
       L < Min ->
	    %% The comment line should be above this node.
	    %% (This duplicates what insert/5 would have done.)
	    [node_add_precomment(C, N) | Ns];
       Col =< 1, L =< Min, L + Delta >= Min ->
	    %% This is a conflict - the "first" token of the node
	    %% overlaps with some comment line, but the comment
	    %% started at column 1.
	    N1 = standalone_comment(C),
	    if L < Min ->
		    [N1 | Nodes];
	       true ->
		    [N, N1 | Ns]
	    end;
       Insert =:= true ->
	    [insert(N, L, Col, Ind, C) | Ns];
       true ->
	    Nodes    % skipping non-toplevel comment
    end;
recomment_forms_2(C, [], _Top) ->
    [standalone_comment(C)].

%% Creating a leaf node for a standalone comment. Note that we try to
%% preserve the original starting column rather than the indentation.

standalone_comment({L, Col, _Ind, Text}) ->
    leaf_node(L, L + comment_delta(Text),
	      erl_syntax:set_pos(erl_syntax:comment(Col - 1, Text), L)).

%% Compute delta between first and last line of a comment, given
%% the lines of text.

comment_delta(Text) ->
    case length(Text) of
	N when N > 0 ->
	    N - 1;
	_ ->
	    0    % avoid negative delta
    end.

%% This kills line information for program forms that do not come from
%% the source file itself, but have been included by preprocessing. This
%% way, comments will not be inserted into such parts by mistake.

-record(filter, {file = undefined :: file:filename() | 'undefined',
		 line = 0         :: integer()}).

filter_forms(Fs) ->
    filter_forms(Fs, false, #filter{}).

filter_forms([F | Fs], Kill, S) ->
    case check_file_attr(F) of
	{true, A1, A2} ->
	    S1 = case S#filter.file of
		     undefined ->
			 S#filter{file = A1, line = A2};
		     _ ->
			 S
		 end,
	    if S1#filter.file =:= A1,
	       S1#filter.line =< A2 ->
		    [F | filter_forms(Fs, false,
				      S1#filter{line = A2})];
	       Kill =:= true ->
		    [node_kill_range(F)
		     | filter_forms(Fs, true, S1)];
	       true ->
		    [F | filter_forms(Fs, true, S1)]
	    end;
	false ->
	    case Kill of
		true ->
		    [node_kill_range(F)
		     | filter_forms(Fs, Kill, S)];
		false ->
		    [F | filter_forms(Fs, Kill, S)]
	    end
    end;
filter_forms([], _, _) ->
    [].

%% This structure matching gets a bit painful...

check_file_attr(F) ->
    case node_type(F) of
	tree_node ->
	    case tree_node_type(F) of
		attribute ->
		    case node_subtrees(F) of
			[L1, L2 | _] ->
			    check_file_attr_1(L1, L2);
			_ ->
			    false
		    end;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

check_file_attr_1(L1, L2) ->
    case node_subtrees(L1) of
	[N1 | _] ->
	    N2 = leaf_node_value(N1),
	    case erl_syntax:type(N2) of
		atom ->
		    case erl_syntax:atom_value(N2) of
			file ->
			    check_file_attr_2(L2);
			_ ->
			    false
		    end;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

check_file_attr_2(L) ->
    case node_subtrees(L) of
	[N1, N2 | _] ->
	    T1 = erl_syntax:concrete(revert_tree(N1)),
	    T2 = erl_syntax:concrete(revert_tree(N2)),
	    {true, T1, T2};
	_ ->
	    false
    end.


%% =====================================================================
%% @spec recomment_tree(Tree::syntaxTree(), Comments::[Comment]) ->
%%           {syntaxTree(), [Comment]}
%%
%%	    Comment = {Line, Column, Indentation, Text}
%%	    Line = integer()
%%	    Column = integer()
%%	    Indentation = integer()
%%	    Text = [string()]
%%
%% @doc Attaches comments to a syntax tree. The result is a pair
%% <code>{NewTree, Remainder}</code> where <code>NewTree</code> is the
%% given <code>Tree</code> where comments from the list
%% <code>Comments</code> have been attached at the proper places.
%% <code>Remainder</code> is the list of entries in
%% <code>Comments</code> which have not been inserted, because their
%% line numbers are greater than those of any node in the tree. The
%% entries in <code>Comments</code> are inserted in order; if two
%% comments become attached to the same node, they will appear in the
%% same order in the program text.
%%
%% <p>The nodes of the syntax tree must contain valid position
%% information. This can be single integers, assumed to represent a line
%% number, or 2- or 3-tuples where the first or second element is an
%% integer, in which case the leftmost integer element is assumed to
%% represent the line number. Line numbers less than one are ignored
%% (usually, the default line number for newly created nodes is
%% zero).</p>
%%
%% <p>For details on the <code>Line</code>, <code>Column</code> and
%% <code>Indentation</code> fields, and the behaviour in case of errors,
%% see <code>recomment_forms/2</code>.</p>
%%
%% @see recomment_forms/2

-spec recomment_tree(erl_syntax:syntaxTree(), [erl_comment_scan:comment()]) ->
        {erl_syntax:syntaxTree(), [erl_comment_scan:comment()]}.

recomment_tree(Tree, Cs) ->
    {Tree1, Cs1} = insert_comments(Cs, build_tree(Tree)),
    {revert_tree(Tree1), Cs1}.

%% Comments are inserted in the tree one at a time. Note that this
%% part makes no assumptions about how tree nodes and list nodes
%% are nested; only `build_tree' and `revert_tree' knows about
%% such things.
    
insert_comments(Cs, Node) ->
    insert_comments(Cs, Node, []).

insert_comments([C | Cs], Node, Cs1) ->
    {L, Col, Ind, _Text} = C,
    Max = node_max(Node),
    if L =< Max ->
	    insert_comments(Cs, insert(Node, L, Col, Ind, C),
			    Cs1);
       true ->
	    insert_comments(Cs, Node, [C | Cs1])
    end;
insert_comments([], Node, Cs) ->
    {Node, lists:reverse(Cs)}.

%% Here, we assume that the comment is located on some line not
%% below the last element of the given node.

insert(Node, L, Col, Ind, C) ->
    case node_type(Node) of
	list_node ->
	    %% We cannot attach comments directly to a list node.
	    set_node_subtrees(Node,
			      insert_in_list(node_subtrees(Node),
					     L, Col, Ind, C));
	_ ->
	    %% We check if the comment belongs before, or inside
	    %% the range of the current node.
	    Min = node_min(Node),
	    Max = node_max(Node),
	    if L < Min ->
		    %% The comment line should be above this node.
		    node_add_precomment(C, Node);
	       Min =:= Max  ->
		    %% The whole node is on a single line (this
		    %% should usually catch all leaf nodes), so we
		    %% postfix the comment.
		    node_add_postcomment(C, Node);
	       true ->
		    %% The comment should be inserted in the
		    %% subrange of the node, i.e., attached either
		    %% to the node itself, or to one of its
		    %% subtrees.
		    insert_1(Node, L, Col, Ind, C)
	    end
    end.

insert_1(Node, L, Col, Ind, C) ->
    case node_type(Node) of
	tree_node ->
	    %% Insert in one of the subtrees.
	    set_node_subtrees(Node,
			      insert_in_list(node_subtrees(Node),
					     L, Col, Ind, C));
	leaf_node ->
	    %% Odd case: no components, but not on a single line.
	    %% (Never mind anyway - just postfix the comment.)
	    node_add_postcomment(C, Node)
    end.

%% We assume that there exists at least one tree node in some tree
%% in the list; since we have decided to insert here, we're
%% screwed if there isn't one.

insert_in_list([Node | Ns], L, Col, Ind, C) ->
    Max = node_max(Node),
    
    %% Get the `Min' of the next node that follows in the
    %% flattened left-to-right order, or -1 (minus one) if no such
    %% tree node exists.
    NextMin = next_min_in_list(Ns),
    
    %% `NextMin' could be less than `Max', in inconsistent trees.
    if NextMin < 0 ->
	    %% There is no following leaf/tree node, so we try
	    %% to insert at this node.
	    insert_here(Node, L, Col, Ind, C, Ns);
       L >= NextMin, NextMin >= Max ->
	    %% Tend to select the later node, in case the next
	    %% node should also match.
	    insert_later(Node, L, Col, Ind, C, Ns);
       L =< Max ->
	    insert_here(Node, L, Col, Ind, C, Ns);
       true ->
	    insert_later(Node, L, Col, Ind, C, Ns)
    end;
insert_in_list([], L, Col, _, _) ->
    exit({bad_tree, L, Col}).

%% The comment belongs to the current subrange

insert_here(Node, L, Col, Ind, C, Ns) ->
    [insert(Node, L, Col, Ind, C) | Ns].

%% The comment should be inserted later

insert_later(Node, L, Col, Ind, C, Ns) ->
    [Node | insert_in_list(Ns, L, Col, Ind, C)].

%% `next_min_in_list' returns the `Min' field of the leftmost tree
%% or leaf node in the given node list, or the integer -1 (minus
%% one) if no such element exists.

next_min_in_list(Ts) ->
    next_min_in_list(Ts, []).

next_min_in_list([T | Ts], Ack) ->
    next_min_in_node(T, [Ts | Ack]);
next_min_in_list([], [T | Ts]) ->
    next_min_in_list(T, Ts);
next_min_in_list([], []) ->
    -1.

next_min_in_node(Node, Ack) ->
    case node_type(Node) of
	leaf_node ->
	    node_min(Node);
	tree_node ->
	    node_min(Node);
	list_node ->
	    next_min_in_list(node_subtrees(Node), Ack)
    end.

%% Building an extended syntax tree from an `erl_syntax' abstract
%% syntax tree.

build_tree(Node) ->
    L = get_line(Node),
    case erl_syntax:subtrees(Node) of
	[] ->
	    %% This guarantees that Min =< Max for the base case.
	    leaf_node(L, L, Node);
	Ts ->
	    %% `Ts' is a list of lists of abstract terms.
	    {Subtrees, Min, Max} = build_list_list(Ts),
	    
	    %% Include L, while preserving Min =< Max.
	    tree_node(minpos(L, Min),
		      erlang:max(L, Max),
		      erl_syntax:type(Node),
		      erl_syntax:get_attrs(Node),
		      Subtrees)
    end.

%% Since `erl_syntax:subtrees' yields the components in
%% left-to-right textual order, the line numbers should grow
%% monotonically as the list is traversed, and the maximum line
%% number of the list should therefore be the dito of the last
%% component. However, we do not want to make such a strong
%% assumption about the consistency of the line numbering, so we
%% take the trouble to find the maximum line number in the subtree
%% taken over all its elements.

build_list(Ts) ->
    build_list(Ts, 0, 0, []).

build_list([T | Ts], Min, Max, Ack) ->
    Node = build_tree(T),
    Min1 = minpos(node_min(Node), Min),
    Max1 = erlang:max(node_max(Node), Max),
    build_list(Ts, Min1, Max1, [Node | Ack]);
build_list([], Min, Max, Ack) ->
    list_node(Min, Max, lists:reverse(Ack)).

build_list_list(Ls) ->
    build_list_list(Ls, 0, 0, []).

build_list_list([L | Ls], Min, Max, Ack) ->
    Node = build_list(L),
    Min1 = minpos(node_min(Node), Min),
    Max1 = erlang:max(node_max(Node), Max),
    build_list_list(Ls, Min1, Max1, [Node | Ack]);
build_list_list([], Min, Max, Ack) ->
    {lists:reverse(Ack), Min, Max}.

%% Reverting to an abstract syntax tree from the extended form.
%% Note that the new comments are inserted after the original
%% attributes are restored.

revert_tree(Node) ->
    case node_type(Node) of
	leaf_node ->
	    add_comments(Node, leaf_node_value(Node));
	tree_node ->
	    add_comments(Node,
			 erl_syntax:set_attrs(
			   erl_syntax:make_tree(
			     tree_node_type(Node),
			     revert_list(node_subtrees(Node))),
			   tree_node_attrs(Node)));
	list_node ->
	    revert_list(node_subtrees(Node))
    end.

revert_list([T | Ts]) ->
    [revert_tree(T) | revert_list(Ts)];
revert_list([]) ->
    [].

add_comments(Node, Tree) ->
    case node_precomments(Node) of
	[] ->
	    add_comments_1(Node, Tree);
	Cs ->
	    Cs1 = lists:reverse(expand_comments(Cs)),
	    add_comments_1(Node,
			   erl_syntax:add_precomments(Cs1, Tree))
    end.

add_comments_1(Node, Tree) ->
    case node_postcomments(Node) of
	[] ->
	    Tree;
	Cs ->
	    Cs1 = lists:reverse(expand_comments(Cs)),
	    erl_syntax:add_postcomments(Cs1, Tree)
    end.

expand_comments([C | Cs]) ->
    [expand_comment(C) | expand_comments(Cs)];
expand_comments([]) ->
    [].

expand_comment(C) ->
    {L, _Col, Ind, Text} = C,
    erl_syntax:set_pos(erl_syntax:comment(Ind, Text), L).


%% =====================================================================
%% Abstract data type for extended syntax trees.
%%
%% These explicitly distinguish between leaf and tree nodes, both
%% corresponding to a single abstract syntax tree, and list nodes,
%% corresponding to a left-to-right ordered sequence of such trees.
%%
%% All nodes have `min' and `max' fields, containing the first and last
%% source lines, respectively, over which the tree extends.
%%
%% Tree nodes and list nodes have a `subtrees' field, containing the
%% (extended) subtrees of the node. Tree nodes also have a `type' field,
%% containing the atom returned by `erl_syntax:type' for the
%% corresponding abstract syntax tree, and an `attrs' field, containing
%% the value of `erl_syntax:get_attrs' for the abstract syntax tree.
%%
%% Leaf nodes and tree nodes also have `precomments' and `postcomments'
%% fields. The comment fields are lists of comment structures (in
%% top-down order); the representation of comments has no consequence to
%% the tree representation.
%%
%% Leaf nodes, lastly, have a `value' field containing the abstract
%% syntax tree for any such tree that can have no subtrees, i.e., such
%% that `erl_syntax:is_leaf' yields `true'.

-record(leaf, {min = 0           :: integer(),
	       max = 0           :: integer(),
	       precomments  = [] :: [erl_comment_scan:comment()],
	       postcomments = [] :: [erl_comment_scan:comment()],
	       value             :: erl_syntax:syntaxTree()}).

-record(tree, {min = 0           :: integer(),
	       max = 0           :: integer(),
	       type              :: atom(),
	       attrs             :: erl_syntax:syntaxTreeAttributes(),
	       precomments  = [] :: [erl_comment_scan:comment()],
	       postcomments = [] :: [erl_comment_scan:comment()],
	       subtrees     = [] :: [extendedSyntaxTree()]}).


-record(list, {min = 0           :: integer(),
	       max = 0           :: integer(),
	       subtrees = []     :: [erl_syntax:syntaxTree()]}).

-type extendedSyntaxTree() :: #tree{} | #leaf{} | #list{}.

leaf_node(Min, Max, Value) ->
    #leaf{min = Min,
	  max = Max,
	  value = Value}.

tree_node(Min, Max, Type, Attrs, Subtrees) ->
    #tree{min = Min,
	  max = Max,
	  type = Type,
	  attrs = Attrs,
	  subtrees = Subtrees}.

list_node(Min, Max, Subtrees) ->
    #list{min = Min,
	  max = Max,
	  subtrees = Subtrees}.

node_type(#leaf{}) ->
    leaf_node;
node_type(#tree{}) ->
    tree_node;
node_type(#list{}) ->
    list_node.

node_min(#leaf{min = Min}) ->
    Min;
node_min(#tree{min = Min}) ->
    Min;
node_min(#list{min = Min}) ->
    Min.

node_max(#leaf{max = Max}) ->
    Max;
node_max(#tree{max = Max}) ->
    Max;
node_max(#list{max = Max}) ->
    Max.

node_kill_range(Node) ->
    case Node of
	#leaf{} ->
	    Node#leaf{min = -1, max = -1};
	#tree{} ->
	    Node#tree{min = -1, max = -1};
	#list{} ->
	    Node#list{min = -1, max = -1}
    end.

node_precomments(#leaf{precomments = Cs}) ->
    Cs;
node_precomments(#tree{precomments = Cs}) ->
    Cs.

node_add_precomment(C, Node) ->
    case Node of
	#leaf{} ->
	    Node#leaf{precomments = [C | Node#leaf.precomments]};
	#tree{} ->
	    Node#tree{precomments = [C | Node#tree.precomments]}
    end.

node_postcomments(#leaf{postcomments = Cs}) ->
    Cs;
node_postcomments(#tree{postcomments = Cs}) ->
    Cs.

node_add_postcomment(C, Node) ->
    case Node of
	#leaf{} ->
	    Node#leaf{postcomments =
		      [C | Node#leaf.postcomments]};
	#tree{} ->
	    Node#tree{postcomments =
		      [C | Node#tree.postcomments]}
    end.

node_subtrees(#tree{subtrees = Subtrees}) ->
    Subtrees;
node_subtrees(#list{subtrees = Subtrees}) ->
    Subtrees.

leaf_node_value(#leaf{value = Value}) ->
    Value.

tree_node_type(#tree{type = Type}) ->
    Type.

set_node_subtrees(Node, Subtrees) ->
    case Node of
	#tree{} ->
	    Node#tree{subtrees = Subtrees};
	#list{} ->
	    Node#list{subtrees = Subtrees}
    end.

tree_node_attrs(#tree{attrs = Attrs}) ->
    Attrs.


%% =====================================================================
%% General utility functions

%% Return the least positive integer of X and Y, or zero if none of them
%% are positive. (This is necessary for computing minimum source line
%% numbers, since zero (or negative) numbers may occur, but they
%% represent the "undefined" line number.)

minpos(X, Y) when X < Y ->
    minpos1(X, Y);
minpos(X, Y) ->
    minpos1(Y, X).

minpos1(X, Y) when X < 1 ->
    minpos2(Y);
minpos1(X, _) ->
    X.

minpos2(X) when X < 1 ->
    0;
minpos2(X) ->
    X.

get_line(Node) ->
    case erl_syntax:get_pos(Node) of
	L when is_integer(L) ->
	    L;
	{L, _} when is_integer(L) ->
	    L;
	{_, L} when is_integer(L) ->
	    L;
	{L, _, _} when is_integer(L) ->
	    L;
	{_, L, _} when is_integer(L) ->
	    L;
	Pos ->
            try erl_anno:line(Pos) of
                Line ->
                    Line
            catch
                _:_ ->
                    exit({bad_position, Pos})
            end
    end.


%% =====================================================================
