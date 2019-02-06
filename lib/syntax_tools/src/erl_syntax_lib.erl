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

%% @doc Support library for abstract Erlang syntax trees.
%%
%% This module contains utility functions for working with the
%% abstract data type defined in the module {@link erl_syntax}.
%%
%% @type syntaxTree() = erl_syntax:syntaxTree(). An abstract syntax
%% tree. See the {@link erl_syntax} module for details.

-module(erl_syntax_lib).

-export([analyze_application/1, analyze_attribute/1,
         analyze_export_attribute/1, analyze_file_attribute/1,
         analyze_form/1, analyze_forms/1, analyze_function/1,
         analyze_function_name/1, analyze_implicit_fun/1,
         analyze_import_attribute/1, analyze_module_attribute/1,
         analyze_record_attribute/1, analyze_record_expr/1,
         analyze_record_field/1, analyze_wild_attribute/1, annotate_bindings/1,
         analyze_type_application/1, analyze_type_name/1,
         annotate_bindings/2, fold/3, fold_subtrees/3, foldl_listlist/3,
         function_name_expansions/1, is_fail_expr/1, limit/2, limit/3,
         map/2, map_subtrees/2, mapfold/3, mapfold_subtrees/3,
         mapfoldl_listlist/3, new_variable_name/1, new_variable_name/2,
         new_variable_names/2, new_variable_names/3, strip_comments/1,
         to_comment/1, to_comment/2, to_comment/3, variables/1]).

-export_type([info_pair/0]).

%% =====================================================================
%% @spec map(Function, Tree::syntaxTree()) -> syntaxTree()
%%
%%          Function = (syntaxTree()) -> syntaxTree()
%% 
%% @doc Applies a function to each node of a syntax tree. The result of
%% each application replaces the corresponding original node. The order
%% of traversal is bottom-up.
%%
%% @see map_subtrees/2

-spec map(fun((erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree()),
	  erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().

map(F, Tree) ->
    case erl_syntax:subtrees(Tree) of
        [] ->
            F(Tree);
        Gs ->
            Tree1 = erl_syntax:make_tree(erl_syntax:type(Tree),
                                         [[map(F, T) || T <- G]
                                          || G <- Gs]),
            F(erl_syntax:copy_attrs(Tree, Tree1))
    end.


%% =====================================================================
%% @spec map_subtrees(Function, syntaxTree()) -> syntaxTree()
%%
%%          Function = (Tree) -> Tree1
%%         
%% @doc Applies a function to each immediate subtree of a syntax tree.
%% The result of each application replaces the corresponding original
%% node.
%%
%% @see map/2

-spec map_subtrees(fun((erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree()),
		   erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().

map_subtrees(F, Tree) ->
    case erl_syntax:subtrees(Tree) of
        [] ->
            Tree;
        Gs ->
            Tree1 = erl_syntax:make_tree(erl_syntax:type(Tree),
                                         [[F(T) || T <- G] || G <- Gs]),
            erl_syntax:copy_attrs(Tree, Tree1)
    end.


%% =====================================================================
%% @spec fold(Function, Start::term(), Tree::syntaxTree()) -> term()
%%
%%          Function = (syntaxTree(), term()) -> term()
%%
%% @doc Folds a function over all nodes of a syntax tree. The result is
%% the value of `Function(X1, Function(X2, ... Function(Xn, Start)
%% ... ))', where `[X1, X2, ..., Xn]' are the nodes of
%% `Tree' in a post-order traversal.
%%
%% @see fold_subtrees/3
%% @see foldl_listlist/3

-spec fold(fun((erl_syntax:syntaxTree(), term()) -> term()),
	   term(), erl_syntax:syntaxTree()) -> term().

fold(F, S, Tree) ->
    case erl_syntax:subtrees(Tree) of
        [] ->
            F(Tree, S);
        Gs ->
            F(Tree, fold_1(F, S, Gs))
    end.

fold_1(F, S, [L | Ls]) ->
    fold_1(F, fold_2(F, S, L), Ls);
fold_1(_, S, []) ->
    S.

fold_2(F, S, [T | Ts]) ->
    fold_2(F, fold(F, S, T), Ts);
fold_2(_, S, []) ->
    S.


%% =====================================================================
%% @spec fold_subtrees(Function, Start::term(), Tree::syntaxTree()) ->
%%           term()
%%
%%          Function = (syntaxTree(), term()) -> term()
%%
%% @doc Folds a function over the immediate subtrees of a syntax tree.
%% This is similar to `fold/3', but only on the immediate
%% subtrees of `Tree', in left-to-right order; it does not
%% include the root node of `Tree'.
%%
%% @see fold/3

-spec fold_subtrees(fun((erl_syntax:syntaxTree(), term()) -> term()),
		    term(), erl_syntax:syntaxTree()) -> term().

fold_subtrees(F, S, Tree) ->
    foldl_listlist(F, S, erl_syntax:subtrees(Tree)).


%% =====================================================================
%% @spec foldl_listlist(Function, Start::term(), [[term()]]) -> term()
%%
%%          Function = (term(), term()) -> term()
%%
%% @doc Like `lists:foldl/3', but over a list of lists.
%%
%% @see fold/3
%% @see //stdlib/lists:foldl/3

-spec foldl_listlist(fun((term(), term()) -> term()),
		     term(), [[term()]]) -> term().

foldl_listlist(F, S, [L | Ls]) ->
    foldl_listlist(F, foldl(F, S, L), Ls);
foldl_listlist(_, S, []) ->
    S.

foldl(F, S, [T | Ts]) ->
    foldl(F, F(T, S), Ts);
foldl(_, S, []) ->
    S.


%% =====================================================================
%% @spec mapfold(Function, Start::term(), Tree::syntaxTree()) ->
%%           {syntaxTree(), term()}
%%
%%          Function = (syntaxTree(), term()) -> {syntaxTree(), term()}
%%
%% @doc Combines map and fold in a single operation. This is similar to
%% `map/2', but also propagates an extra value from each
%% application of the `Function' to the next, while doing a
%% post-order traversal of the tree like `fold/3'. The value
%% `Start' is passed to the first function application, and
%% the final result is the result of the last application.
%%
%% @see map/2
%% @see fold/3

-spec mapfold(fun((erl_syntax:syntaxTree(), term()) -> {erl_syntax:syntaxTree(), term()}),
	      term(), erl_syntax:syntaxTree()) -> {erl_syntax:syntaxTree(), term()}.

mapfold(F, S, Tree) ->
    case erl_syntax:subtrees(Tree) of
        [] ->
            F(Tree, S);
        Gs ->
            {Gs1, S1} = mapfold_1(F, S, Gs),
            Tree1 = erl_syntax:make_tree(erl_syntax:type(Tree), Gs1),
            F(erl_syntax:copy_attrs(Tree, Tree1), S1)
    end.

mapfold_1(F, S, [L | Ls]) ->
    {L1, S1} = mapfold_2(F, S, L),
    {Ls1, S2} = mapfold_1(F, S1, Ls),
    {[L1 | Ls1], S2};
mapfold_1(_, S, []) ->
    {[], S}.

mapfold_2(F, S, [T | Ts]) ->
    {T1, S1} = mapfold(F, S, T),
    {Ts1, S2} = mapfold_2(F, S1, Ts),
    {[T1 | Ts1], S2};
mapfold_2(_, S, []) ->
    {[], S}.


%% =====================================================================
%% @spec mapfold_subtrees(Function, Start::term(),
%%                        Tree::syntaxTree()) -> {syntaxTree(), term()}
%%
%%          Function = (syntaxTree(), term()) -> {syntaxTree(), term()}
%%
%% @doc Does a mapfold operation over the immediate subtrees of a syntax
%% tree. This is similar to `mapfold/3', but only on the
%% immediate subtrees of `Tree', in left-to-right order; it
%% does not include the root node of `Tree'.
%%
%% @see mapfold/3

-spec mapfold_subtrees(fun((erl_syntax:syntaxTree(), term()) ->
			      {erl_syntax:syntaxTree(), term()}),
		       term(), erl_syntax:syntaxTree()) ->
        {erl_syntax:syntaxTree(), term()}.

mapfold_subtrees(F, S, Tree) ->
    case erl_syntax:subtrees(Tree) of
        [] ->
            {Tree, S};
        Gs ->
            {Gs1, S1} = mapfoldl_listlist(F, S, Gs),
            Tree1 = erl_syntax:make_tree(erl_syntax:type(Tree), Gs1),
            {erl_syntax:copy_attrs(Tree, Tree1), S1}
    end.


%% =====================================================================
%% @spec mapfoldl_listlist(Function, State, [[term()]]) ->
%%           {[[term()]], term()}
%%
%%          Function = (term(), term()) -> {term(), term()}
%%
%% @doc Like `lists:mapfoldl/3', but over a list of lists.
%% The list of lists in the result has the same structure as the given
%% list of lists.

-spec mapfoldl_listlist(fun((term(), term()) -> {term(), term()}),
			term(), [[term()]]) -> {[[term()]], term()}.

mapfoldl_listlist(F, S, [L | Ls]) ->
    {L1, S1} = mapfoldl(F, S, L),
    {Ls1, S2} = mapfoldl_listlist(F, S1, Ls),
    {[L1 | Ls1], S2};
mapfoldl_listlist(_, S, []) ->
    {[], S}.

mapfoldl(F, S, [L | Ls]) ->
    {L1, S1} = F(L, S),
    {Ls1, S2} = mapfoldl(F, S1, Ls),
    {[L1 | Ls1], S2};
mapfoldl(_, S, []) ->
    {[], S}.


%% =====================================================================
%% @spec variables(syntaxTree()) -> set(atom())
%%
%% @type set(T) = //stdlib/sets:set(T)
%%
%% @doc Returns the names of variables occurring in a syntax tree, The
%% result is a set of variable names represented by atoms. Macro names
%% are not included.
%%
%% @see //stdlib/sets

-spec variables(erl_syntax:syntaxTree()) -> sets:set(atom()).

variables(Tree) ->
    variables(Tree, sets:new()).

variables(T, S) ->
    case erl_syntax:type(T) of
	variable ->
	    sets:add_element(erl_syntax:variable_name(T), S);
	macro ->
	    %% macro names are ignored, even if represented by variables
	    case erl_syntax:macro_arguments(T) of
		none -> S;
		As ->
		    variables_2(As, S)
	    end;
	_ ->
	    case erl_syntax:subtrees(T) of
		[] ->
		    S;
		Gs ->
		    variables_1(Gs, S)
	    end
    end.

variables_1([L | Ls], S) ->
    variables_1(Ls, variables_2(L, S));
variables_1([], S) ->
    S.

variables_2([T | Ts], S) ->
    variables_2(Ts, variables(T, S));
variables_2([], S) ->
    S.


-define(MINIMUM_RANGE, 100).
-define(START_RANGE_FACTOR, 100).
-define(MAX_RETRIES, 3).    % retries before enlarging range
-define(ENLARGE_ENUM, 8).   % range enlargment enumerator
-define(ENLARGE_DENOM, 1).  % range enlargment denominator

default_variable_name(N) ->
    list_to_atom("V" ++ integer_to_list(N)).

%% =====================================================================
%% @spec new_variable_name(Used::set(atom())) -> atom()
%%
%% @doc Returns an atom which is not already in the set `Used'. This is
%% equivalent to `new_variable_name(Function, Used)', where `Function'
%% maps a given integer `N' to the atom whose name consists of "`V'"
%% followed by the numeral for `N'.
%%
%% @see new_variable_name/2

-spec new_variable_name(sets:set(atom())) -> atom().

new_variable_name(S) ->
    new_variable_name(fun default_variable_name/1, S).

%% =====================================================================
%% @spec new_variable_name(Function, Used::set(atom())) -> atom()
%%
%%          Function = (integer()) -> atom()
%%
%% @doc Returns a user-named atom which is not already in the set
%% `Used'. The atom is generated by applying the given
%% `Function' to a generated integer. Integers are generated
%% using an algorithm which tries to keep the names randomly distributed
%% within a reasonably small range relative to the number of elements in
%% the set.
%%
%% This function uses the module `rand' to generate new
%% keys. The seed it uses may be initialized by calling
%% `rand:seed/1' or `rand:seed/2' before this
%% function is first called.
%%
%% @see new_variable_name/1
%% @see //stdlib/sets
%% @see //stdlib/random

-spec new_variable_name(fun((integer()) -> atom()), sets:set(atom())) -> atom().

new_variable_name(F, S) ->
    R = start_range(S),
    new_variable_name(R, F, S).

new_variable_name(R, F, S) ->
    new_variable_name(generate(R, R), R, 0, F, S).

new_variable_name(N, R, T, F, S) when T < ?MAX_RETRIES ->
    A = F(N),
    case sets:is_element(A, S) of
        true ->
            new_variable_name(generate(N, R), R, T + 1, F, S);
        false ->
            A
    end;
new_variable_name(N, R, _T, F, S) ->
    %% Too many retries - enlarge the range and start over.
    R1 = (R * ?ENLARGE_ENUM) div ?ENLARGE_DENOM,
    new_variable_name(generate(N, R1), R1, 0, F, S).

%% Note that we assume that it is very cheap to take the size of
%% the given set. This should be valid for the stdlib
%% implementation of `sets'.

start_range(S) ->
    erlang:max(sets:size(S) * ?START_RANGE_FACTOR, ?MINIMUM_RANGE).

%% The previous number might or might not be used to compute the
%% next number to be tried. It is currently not used.
%%
%% It is important that this function does not generate values in
%% order, but (pseudo-)randomly distributed over the range.

generate(_Key, Range) ->
    _ = case rand:export_seed() of
	    undefined ->
		rand:seed(exsplus, {753,8,73});
	    _ ->
		ok
	end,
    rand:uniform(Range).			% works well


%% =====================================================================
%% @spec new_variable_names(N::integer(), Used::set(atom())) -> [atom()]
%%
%% @doc Like `new_variable_name/1', but generates a list of
%% `N' new names.
%% 
%% @see new_variable_name/1

-spec new_variable_names(integer(), sets:set(atom())) -> [atom()].

new_variable_names(N, S) ->
    new_variable_names(N, fun default_variable_name/1, S).

%% =====================================================================
%% @spec new_variable_names(N::integer(), Function,
%%                          Used::set(atom())) -> [atom()]
%%
%%          Function = (integer()) -> atom()
%%
%% @doc Like `new_variable_name/2', but generates a list of
%% `N' new names.
%% 
%% @see new_variable_name/2

-spec new_variable_names(integer(), fun((integer()) -> atom()), sets:set(atom())) ->
	[atom()].

new_variable_names(N, F, S) when is_integer(N) ->
    R = start_range(S),
    new_variable_names(N, [], R, F, S).

new_variable_names(N, Names, R, F, S) when N > 0 ->
    Name = new_variable_name(R, F, S),
    S1 = sets:add_element(Name, S),
    new_variable_names(N - 1, [Name | Names], R, F, S1);
new_variable_names(0, Names, _, _, _) ->
    Names.


%% =====================================================================
%% @spec annotate_bindings(Tree::syntaxTree(),
%%                         Bindings::ordset(atom())) -> syntaxTree()
%%
%% @type ordset(T) = //stdlib/ordsets:ordset(T)
%%
%% @doc Adds or updates annotations on nodes in a syntax tree.
%% `Bindings' specifies the set of bound variables in the
%% environment of the top level node. The following annotations are
%% affected:
%% <ul>
%%     <li>`{env, Vars}', representing the input environment
%%     of the subtree.</li>
%%
%%     <li>`{bound, Vars}', representing the variables that
%%     are bound in the subtree.</li>
%%
%%     <li>`{free, Vars}', representing the free variables in
%%     the subtree.</li>
%% </ul>
%% `Bindings' and `Vars' are ordered-set lists
%% (cf. module `ordsets') of atoms representing variable
%% names.
%%
%% @see annotate_bindings/1
%% @see //stdlib/ordsets

-spec annotate_bindings(erl_syntax:syntaxTree(), ordsets:ordset(atom())) ->
        erl_syntax:syntaxTree().

annotate_bindings(Tree, Env) ->
    {Tree1, _, _} = vann(Tree, Env),
    Tree1.

%% =====================================================================
%% @spec annotate_bindings(Tree::syntaxTree()) -> syntaxTree()
%%
%% @doc Adds or updates annotations on nodes in a syntax tree.
%% Equivalent to `annotate_bindings(Tree, Bindings)' where
%% the top-level environment `Bindings' is taken from the
%% annotation `{env, Bindings}' on the root node of
%% `Tree'. An exception is thrown if no such annotation
%% should exist.
%%
%% @see annotate_bindings/2

-spec annotate_bindings(erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().

annotate_bindings(Tree) ->
    As = erl_syntax:get_ann(Tree),
    case lists:keyfind(env, 1, As) of
        {env, InVars} ->
            annotate_bindings(Tree, InVars);
        _ ->
            erlang:error(badarg)
    end.

vann(Tree, Env) ->
    case erl_syntax:type(Tree) of
        variable ->
            %% Variable use
            Bound = [],
            Free = [erl_syntax:variable_name(Tree)],
            {ann_bindings(Tree, Env, Bound, Free), Bound, Free};
        match_expr ->
            vann_match_expr(Tree, Env);
        case_expr ->
            vann_case_expr(Tree, Env);
        if_expr ->
            vann_if_expr(Tree, Env);
        cond_expr ->
            vann_cond_expr(Tree, Env);
        receive_expr ->
            vann_receive_expr(Tree, Env);
        catch_expr ->
            vann_catch_expr(Tree, Env);
        try_expr ->
            vann_try_expr(Tree, Env);
        function ->
            vann_function(Tree, Env);
        fun_expr ->
            vann_fun_expr(Tree, Env);
        list_comp ->
            vann_list_comp(Tree, Env);
        binary_comp ->
            vann_binary_comp(Tree, Env);
        generator ->
            vann_generator(Tree, Env);
        binary_generator ->
            vann_binary_generator(Tree, Env);
        block_expr ->
            vann_block_expr(Tree, Env);
        macro ->
            vann_macro(Tree, Env);
        _Type ->
            F = vann_list_join(Env),
            {Tree1, {Bound, Free}} = mapfold_subtrees(F, {[], []},
                                                      Tree),
            {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}
    end.

vann_list_join(Env) ->
    fun (T, {Bound, Free}) ->
            {T1, Bound1, Free1} = vann(T, Env),
            {T1, {ordsets:union(Bound, Bound1),
                  ordsets:union(Free, Free1)}}
    end.

vann_list(Ts, Env) ->
    lists:mapfoldl(vann_list_join(Env), {[], []}, Ts).

vann_function(Tree, Env) ->
    Cs = erl_syntax:function_clauses(Tree),
    {Cs1, {_, Free}} = vann_clauses(Cs, Env),
    N = erl_syntax:function_name(Tree),
    {N1, _, _} = vann(N, Env),
    Tree1 = rewrite(Tree, erl_syntax:function(N1, Cs1)),
    Bound = [],
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_fun_expr(Tree, Env) ->
    Cs = erl_syntax:fun_expr_clauses(Tree),
    {Cs1, {_, Free}} = vann_clauses(Cs, Env),
    Tree1 = rewrite(Tree, erl_syntax:fun_expr(Cs1)),
    Bound = [],
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_match_expr(Tree, Env) ->
    E = erl_syntax:match_expr_body(Tree),
    {E1, Bound1, Free1} = vann(E, Env),
    Env1 = ordsets:union(Env, Bound1),
    P = erl_syntax:match_expr_pattern(Tree),
    {P1, Bound2, Free2} = vann_pattern(P, Env1),
    Bound = ordsets:union(Bound1, Bound2),
    Free = ordsets:union(Free1, Free2),
    Tree1 = rewrite(Tree, erl_syntax:match_expr(P1, E1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_case_expr(Tree, Env) ->
    E = erl_syntax:case_expr_argument(Tree),
    {E1, Bound1, Free1} = vann(E, Env),
    Env1 = ordsets:union(Env, Bound1),
    Cs = erl_syntax:case_expr_clauses(Tree),
    {Cs1, {Bound2, Free2}} = vann_clauses(Cs, Env1),
    Bound = ordsets:union(Bound1, Bound2),
    Free = ordsets:union(Free1, Free2),
    Tree1 = rewrite(Tree, erl_syntax:case_expr(E1, Cs1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_if_expr(Tree, Env) ->
    Cs = erl_syntax:if_expr_clauses(Tree),
    {Cs1, {Bound, Free}} = vann_clauses(Cs, Env),
    Tree1 = rewrite(Tree, erl_syntax:if_expr(Cs1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_cond_expr(_Tree, _Env) ->
    erlang:error({not_implemented,cond_expr}).

vann_catch_expr(Tree, Env) ->
    E = erl_syntax:catch_expr_body(Tree),
    {E1, _, Free} = vann(E, Env),
    Tree1 = rewrite(Tree, erl_syntax:catch_expr(E1)),
    Bound = [],
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_try_expr(Tree, Env) ->
    Es = erl_syntax:try_expr_body(Tree),
    {Es1, {Bound1, Free1}} = vann_body(Es, Env),
    Cs = erl_syntax:try_expr_clauses(Tree),
    %% bindings in the body should be available in the success case,
    {Cs1, {_, Free2}} = vann_clauses(Cs, ordsets:union(Env, Bound1)),
    Hs = erl_syntax:try_expr_handlers(Tree),
    {Hs1, {_, Free3}} = vann_clauses(Hs, Env),
    %% the after part does not export anything, yet; this might change
    As = erl_syntax:try_expr_after(Tree),
    {As1, {_, Free4}} = vann_body(As, Env),
    Tree1 = rewrite(Tree, erl_syntax:try_expr(Es1, Cs1, Hs1, As1)),
    Bound = [],
    Free = ordsets:union(Free1, ordsets:union(Free2, ordsets:union(Free3, Free4))),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_receive_expr(Tree, Env) ->
    %% The timeout action is treated as an extra clause.
    %% Bindings in the expiry expression are local only.
    Cs = erl_syntax:receive_expr_clauses(Tree),
    Es = erl_syntax:receive_expr_action(Tree),
    C = erl_syntax:clause([], Es),
    {[C1 | Cs1], {Bound, Free1}} = vann_clauses([C | Cs], Env),
    Es1 = erl_syntax:clause_body(C1),
    {T1, _, Free2} = case erl_syntax:receive_expr_timeout(Tree) of
                         none ->
                             {none, [], []};
                         T ->
                             vann(T, Env)
                     end,
    Free = ordsets:union(Free1, Free2),
    Tree1 = rewrite(Tree, erl_syntax:receive_expr(Cs1, T1, Es1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_list_comp(Tree, Env) ->
    Es = erl_syntax:list_comp_body(Tree),
    {Es1, {Bound1, Free1}} = vann_list_comp_body(Es, Env),
    Env1 = ordsets:union(Env, Bound1),
    T = erl_syntax:list_comp_template(Tree),
    {T1, _, Free2} = vann(T, Env1),
    Free = ordsets:union(Free1, ordsets:subtract(Free2, Bound1)),
    Bound = [],
    Tree1 = rewrite(Tree, erl_syntax:list_comp(T1, Es1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_list_comp_body_join() ->
    fun (T, {Env, Bound, Free}) ->
            {T1, Bound1, Free1} = case erl_syntax:type(T) of
                                      binary_generator ->
				          vann_binary_generator(T,Env);
				      generator ->
                                          vann_generator(T, Env);
                                      _ ->
                                          %% Bindings in filters are not
                                          %% exported to the rest of the
                                          %% body.
                                          {T2, _, Free2} = vann(T, Env),
                                          {T2, [], Free2}
                                  end,
            Env1 = ordsets:union(Env, Bound1),
            {T1, {Env1, ordsets:union(Bound, Bound1),
                  ordsets:union(Free, 
                                ordsets:subtract(Free1, Bound))}}
    end.

vann_list_comp_body(Ts, Env) ->
    F = vann_list_comp_body_join(),
    {Ts1, {_, Bound, Free}} = lists:mapfoldl(F, {Env, [], []}, Ts),
    {Ts1, {Bound, Free}}.

vann_binary_comp(Tree, Env) ->
    Es = erl_syntax:binary_comp_body(Tree),
    {Es1, {Bound1, Free1}} = vann_binary_comp_body(Es, Env),
    Env1 = ordsets:union(Env, Bound1),
    T = erl_syntax:binary_comp_template(Tree),
    {T1, _, Free2} = vann(T, Env1),
    Free = ordsets:union(Free1, ordsets:subtract(Free2, Bound1)),
    Bound = [],
    Tree1 = rewrite(Tree, erl_syntax:binary_comp(T1, Es1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_binary_comp_body_join() ->
    fun (T, {Env, Bound, Free}) ->
            {T1, Bound1, Free1} = case erl_syntax:type(T) of
                                    binary_generator ->
				          vann_binary_generator(T, Env);
				    generator ->
                                          vann_generator(T, Env);
                                      _ ->
                                          %% Bindings in filters are not
                                          %% exported to the rest of the
                                          %% body.
                                          {T2, _, Free2} = vann(T, Env),
                                          {T2, [], Free2}
                                  end,
            Env1 = ordsets:union(Env, Bound1),
            {T1, {Env1, ordsets:union(Bound, Bound1),
                  ordsets:union(Free, 
                                ordsets:subtract(Free1, Bound))}}
    end.

vann_binary_comp_body(Ts, Env) ->
    F = vann_binary_comp_body_join(),
    {Ts1, {_, Bound, Free}} = lists:mapfoldl(F, {Env, [], []}, Ts),
    {Ts1, {Bound, Free}}.

%% In list comprehension generators, the pattern variables are always
%% viewed as new occurrences, shadowing whatever is in the input
%% environment (thus, the pattern contains no variable uses, only
%% bindings). Bindings in the generator body are not exported.

vann_generator(Tree, Env) ->
    P = erl_syntax:generator_pattern(Tree),
    {P1, Bound, _} = vann_pattern(P, []),
    E = erl_syntax:generator_body(Tree),
    {E1, _, Free} = vann(E, Env),
    Tree1 = rewrite(Tree, erl_syntax:generator(P1, E1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_binary_generator(Tree, Env) ->
    P = erl_syntax:binary_generator_pattern(Tree),
    {P1, Bound, _} = vann_pattern(P, Env),
    E = erl_syntax:binary_generator_body(Tree),
    {E1, _, Free} = vann(E, Env),
    Tree1 = rewrite(Tree, erl_syntax:binary_generator(P1, E1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_block_expr(Tree, Env) ->
    Es = erl_syntax:block_expr_body(Tree),
    {Es1, {Bound, Free}} = vann_body(Es, Env),
    Tree1 = rewrite(Tree, erl_syntax:block_expr(Es1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_body_join() ->
    fun (T, {Env, Bound, Free}) ->
            {T1, Bound1, Free1} = vann(T, Env),
            Env1 = ordsets:union(Env, Bound1),
            {T1, {Env1, ordsets:union(Bound, Bound1),
                  ordsets:union(Free,
                                ordsets:subtract(Free1, Bound))}}
    end.

vann_body(Ts, Env) ->
    {Ts1, {_, Bound, Free}} = lists:mapfoldl(vann_body_join(),
                                             {Env, [], []}, Ts),
    {Ts1, {Bound, Free}}.

%% Macro names must be ignored even if they happen to be variables,
%% lexically speaking.

vann_macro(Tree, Env) ->
    {As, {Bound, Free}} = case erl_syntax:macro_arguments(Tree) of
                              none ->
                                  {none, {[], []}};
                              As1 ->
                                  vann_list(As1, Env)
                          end,
    N = erl_syntax:macro_name(Tree),
    Tree1 = rewrite(Tree, erl_syntax:macro(N, As)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_pattern(Tree, Env) ->
    case erl_syntax:type(Tree) of
        variable ->
            V = erl_syntax:variable_name(Tree),
            case ordsets:is_element(V, Env) of
                true ->
                    %% Variable use
                    Bound = [],
                    Free = [V],
                    {ann_bindings(Tree, Env, Bound, Free), Bound, Free};
                false ->
                    %% Variable binding
                    Bound = [V],
                    Free = [],
                    {ann_bindings(Tree, Env, Bound, Free), Bound, Free}
            end;
        match_expr ->
            %% Alias pattern
            P = erl_syntax:match_expr_pattern(Tree),
            {P1, Bound1, Free1} = vann_pattern(P, Env),
            E = erl_syntax:match_expr_body(Tree),
            {E1, Bound2, Free2} = vann_pattern(E, Env),
            Bound = ordsets:union(Bound1, Bound2),
            Free = ordsets:union(Free1, Free2),
            Tree1 = rewrite(Tree, erl_syntax:match_expr(P1, E1)),
            {ann_bindings(Tree1, Env, Bound, Free), Bound, Free};
        macro ->
            %% The macro name must be ignored. The arguments are treated
            %% as patterns.
            {As, {Bound, Free}} =
                case erl_syntax:macro_arguments(Tree) of
                    none ->
                        {none, {[], []}};
                    As1 ->
                        vann_patterns(As1, Env)
                end,
            N = erl_syntax:macro_name(Tree),
            Tree1 = rewrite(Tree, erl_syntax:macro(N, As)),
            {ann_bindings(Tree1, Env, Bound, Free), Bound, Free};
        _Type ->
            F = vann_patterns_join(Env),
            {Tree1, {Bound, Free}} = mapfold_subtrees(F, {[], []},
                                                      Tree),
            {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}
    end.

vann_patterns_join(Env) ->
    fun (T, {Bound, Free}) ->
            {T1, Bound1, Free1} = vann_pattern(T, Env),
            {T1, {ordsets:union(Bound, Bound1),
                  ordsets:union(Free, Free1)}}
    end.

vann_patterns(Ps, Env) ->
    lists:mapfoldl(vann_patterns_join(Env), {[], []}, Ps).

vann_clause(C, Env) ->
    {Ps, {Bound1, Free1}} = vann_patterns(erl_syntax:clause_patterns(C),
                                          Env),
    Env1 = ordsets:union(Env, Bound1),
    %% Guards cannot add bindings
    {G1, _, Free2} = case erl_syntax:clause_guard(C) of
                         none ->
                             {none, [], []};
                         G ->
                             vann(G, Env1)
                     end,
    {Es, {Bound2, Free3}} = vann_body(erl_syntax:clause_body(C), Env1),
    Bound = ordsets:union(Bound1, Bound2),
    Free = ordsets:union(Free1,
                         ordsets:subtract(ordsets:union(Free2, Free3),
                                          Bound1)),
    Tree1 = rewrite(C, erl_syntax:clause(Ps, G1, Es)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_clauses_join(Env) ->
    fun (C, {Bound, Free}) ->
            {C1, Bound1, Free1} = vann_clause(C, Env),
            {C1, {ordsets:intersection(Bound, Bound1),
                  ordsets:union(Free, Free1)}}
    end.

vann_clauses([C | Cs], Env) ->
    {C1, Bound, Free} = vann_clause(C, Env),
    {Cs1, BF} = lists:mapfoldl(vann_clauses_join(Env), {Bound, Free}, Cs),
    {[C1 | Cs1], BF};
vann_clauses([], _Env) ->
    {[], {[], []}}.

ann_bindings(Tree, Env, Bound, Free) ->
    As0 = erl_syntax:get_ann(Tree),
    As1 = [{env, Env},
           {bound, Bound},
           {free, Free}
           | delete_binding_anns(As0)],
    erl_syntax:set_ann(Tree, As1).

delete_binding_anns([{env, _} | As]) ->
    delete_binding_anns(As);
delete_binding_anns([{bound, _} | As]) ->
    delete_binding_anns(As);
delete_binding_anns([{free, _} | As]) ->
    delete_binding_anns(As);
delete_binding_anns([A | As]) ->
    [A | delete_binding_anns(As)];
delete_binding_anns([]) ->
    [].


%% =====================================================================
%% @spec is_fail_expr(Tree::syntaxTree()) -> boolean()
%%
%% @doc Returns `true' if `Tree' represents an
%% expression which never terminates normally. Note that the reverse
%% does not apply. Currently, the detected cases are calls to
%% `exit/1', `throw/1',
%% `erlang:error/1' and `erlang:error/2'.
%%
%% @see //erts/erlang:exit/1
%% @see //erts/erlang:throw/1
%% @see //erts/erlang:error/1
%% @see //erts/erlang:error/2

-spec is_fail_expr(erl_syntax:syntaxTree()) -> boolean().

is_fail_expr(E) ->          
    case erl_syntax:type(E) of
        application ->
            N = length(erl_syntax:application_arguments(E)),
            F = erl_syntax:application_operator(E),
            case catch {ok, analyze_function_name(F)} of
                syntax_error ->
                    false;
                {ok, exit} when N =:= 1 ->
                    true;
                {ok, throw} when N =:= 1 ->
                    true;
                {ok, {erlang, exit}} when N =:= 1 ->
                    true;
                {ok, {erlang, throw}} when N =:= 1 ->
                    true;
                {ok, {erlang, error}} when N =:= 1 ->
                    true;
                {ok, {erlang, error}} when N =:= 2 ->
                    true;
                {ok, {erlang, fault}} when N =:= 1 ->
                    true;
                {ok, {erlang, fault}} when N =:= 2 ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.


%% =====================================================================
%% @spec analyze_forms(Forms) -> [{Key, term()}]
%%
%%          Forms = syntaxTree() | [syntaxTree()]
%%          Key = attributes | errors | exports | functions | imports
%%                | module | records | warnings
%%
%% @doc Analyzes a sequence of "program forms". The given
%% `Forms' may be a single syntax tree of type
%% `form_list', or a list of "program form" syntax trees. The
%% returned value is a list of pairs `{Key, Info}', where
%% each value of `Key' occurs at most once in the list; the
%% absence of a particular key indicates that there is no well-defined
%% value for that key.
%%
%% Each entry in the resulting list contains the following
%% corresponding information about the program forms:
%% <dl>
%%     <dt>`{attributes, Attributes}'</dt>
%%       <dd><ul>
%% 	   <li>`Attributes = [{atom(), term()}]'</li>
%%       </ul>
%% 	 `Attributes' is a list of pairs representing the
%% 	 names and corresponding values of all so-called "wild"
%% 	 attributes (as e.g. "`-compile(...)'") occurring in
%% 	 `Forms' (cf. `analyze_wild_attribute/1').
%% 	 We do not guarantee that each name occurs at most once in the
%% 	 list. The order of listing is not defined.</dd>
%%
%%     <dt>`{errors, Errors}'</dt>
%%       <dd><ul>
%% 	   <li>`Errors = [term()]'</li>
%%       </ul>
%% 	 `Errors' is the list of error descriptors of all
%% 	 `error_marker' nodes that occur in
%% 	 `Forms'. The order of listing is not defined.</dd>
%%
%%     <dt>`{exports, Exports}'</dt>
%%       <dd><ul>
%% 	    <li>`Exports = [FunctionName]'</li>
%% 	    <li>`FunctionName = atom()
%%                    | {atom(), integer()}
%% 		      | {ModuleName, FunctionName}'</li>
%% 	    <li>`ModuleName = atom()'</li>
%%       </ul>
%% 	 `Exports' is a list of representations of those
%% 	 function names that are listed by export declaration attributes
%% 	 in `Forms' (cf.
%% 	 `analyze_export_attribute/1'). We do not guarantee
%% 	 that each name occurs at most once in the list. The order of
%% 	 listing is not defined.</dd>
%%
%%     <dt>`{functions, Functions}'</dt>
%%       <dd><ul>
%% 	    <li>`Functions = [{atom(), integer()}]'</li>
%%       </ul>
%% 	 `Functions' is a list of the names of the functions
%% 	 that are defined in `Forms' (cf.
%% 	 `analyze_function/1'). We do not guarantee that each
%% 	 name occurs at most once in the list. The order of listing is
%% 	 not defined.</dd>
%%
%%     <dt>`{imports, Imports}'</dt>
%%       <dd><ul>
%% 	    <li>`Imports = [{Module, Names}]'</li>
%% 	    <li>`Module = atom()'</li>
%% 	    <li>`Names = [FunctionName]'</li>
%% 	    <li>`FunctionName = atom()
%%                    | {atom(), integer()}
%% 		      | {ModuleName, FunctionName}'</li>
%% 	    <li>`ModuleName = atom()'</li>
%%       </ul>
%% 	 `Imports' is a list of pairs representing those
%% 	 module names and corresponding function names that are listed
%% 	 by import declaration attributes in `Forms' (cf.
%% 	 `analyze_import_attribute/1'), where each
%% 	 `Module' occurs at most once in
%% 	 `Imports'. We do not guarantee that each name occurs
%% 	 at most once in the lists of function names. The order of
%% 	 listing is not defined.</dd>
%%
%%     <dt>`{module, ModuleName}'</dt>
%%       <dd><ul>
%% 	    <li>`ModuleName = atom()'</li>
%%       </ul>
%% 	 `ModuleName' is the name declared by a module
%% 	 attribute in `Forms'. If no module name is defined
%% 	 in `Forms', the result will contain no entry for the
%% 	 `module' key. If multiple module name declarations
%% 	 should occur, all but the first will be ignored.</dd>
%%
%%     <dt>`{records, Records}'</dt>
%%       <dd><ul>
%% 	    <li>`Records = [{atom(), Fields}]'</li>
%% 	    <li>`Fields = [{atom(), {Default, Type}}]'</li>
%% 	    <li>`Default = none | syntaxTree()'</li>
%% 	    <li>`Type = none | syntaxTree()'</li>
%%       </ul>
%% 	 `Records' is a list of pairs representing the names
%% 	 and corresponding field declarations of all record declaration
%% 	 attributes occurring in `Forms'. For fields declared
%% 	 without a default value, the corresponding value for
%% 	 `Default' is the atom `none'. Similarly, for fields declared
%%       without a type, the corresponding value for `Type' is the
%%       atom `none' (cf.
%% 	 `analyze_record_attribute/1'). We do not guarantee
%% 	 that each record name occurs at most once in the list. The
%% 	 order of listing is not defined.</dd>
%%
%%     <dt>`{warnings, Warnings}'</dt>
%%       <dd><ul>
%% 	    <li>`Warnings = [term()]'</li>
%%       </ul>
%% 	 `Warnings' is the list of error descriptors of all
%% 	 `warning_marker' nodes that occur in
%% 	 `Forms'. The order of listing is not defined.</dd>
%% </dl>
%%
%% The evaluation throws `syntax_error' if an ill-formed
%% Erlang construct is encountered.
%%
%% @see analyze_wild_attribute/1
%% @see analyze_export_attribute/1
%% @see analyze_function/1
%% @see analyze_import_attribute/1
%% @see analyze_record_attribute/1
%% @see erl_syntax:error_marker_info/1
%% @see erl_syntax:warning_marker_info/1

-type key() :: 'attributes' | 'errors' | 'exports' | 'functions' | 'imports'
             | 'module' | 'records' | 'warnings'.
-type info_pair() :: {key(), term()}.

-spec analyze_forms(erl_syntax:forms()) -> [info_pair()].

analyze_forms(Forms) when is_list(Forms) ->
    finfo_to_list(lists:foldl(fun collect_form/2, new_finfo(), Forms));
analyze_forms(Forms) ->
    analyze_forms(
      erl_syntax:form_list_elements(
        erl_syntax:flatten_form_list(Forms))).

collect_form(Node, Info) ->
    case analyze_form(Node) of
        {attribute, {Name, Data}} ->
            collect_attribute(Name, Data, Info);
        {attribute, preprocessor} ->
            Info;
        {function, Name} ->
            finfo_add_function(Name, Info);
        {error_marker, Data} ->
            finfo_add_error(Data, Info);
        {warning_marker, Data} ->
            finfo_add_warning(Data, Info);
        _ ->
            Info
    end.

collect_attribute(module, M, Info) ->
    finfo_set_module(M, Info);
collect_attribute(export, L, Info) ->
    finfo_add_exports(L, Info);
collect_attribute(import, {M, L}, Info) ->
    finfo_add_imports(M, L, Info);
collect_attribute(import, M, Info) ->
    finfo_add_module_import(M, Info);
collect_attribute(file, _, Info) ->
    Info;
collect_attribute(record, {R, L}, Info) ->
    finfo_add_record(R, L, Info);
collect_attribute(_, {N, V}, Info) ->
    finfo_add_attribute(N, V, Info).

%% Abstract datatype for collecting module information.

-record(forms, {module         = none :: 'none' | {'value', atom()},
		exports        = []   :: [{atom(), arity()}],
		module_imports = []   :: [atom()],
		imports        = []   :: [{atom(), [{atom(), arity()}]}],
		attributes     = []   :: [{atom(), term()}],
		records        = []   :: [{atom(), [{atom(),
                                                     field_default(),
                                                     field_type()}]}],
		errors         = []   :: [term()],
		warnings       = []   :: [term()],
		functions      = []   :: [{atom(), arity()}]}).

-type field_default() :: 'none' | erl_syntax:syntaxTree().
-type field_type()    :: 'none' | erl_syntax:syntaxTree().

new_finfo() ->
    #forms{}.

finfo_set_module(Name, Info) ->
    case Info#forms.module of
        none ->
            Info#forms{module = {value, Name}};
        {value, _} ->
            Info
    end.

finfo_add_exports(L, Info) ->
    Info#forms{exports = L ++ Info#forms.exports}.

finfo_add_module_import(M, Info) ->
    Info#forms{module_imports = [M | Info#forms.module_imports]}.

finfo_add_imports(M, L, Info) ->
    Es = Info#forms.imports,
    case lists:keyfind(M, 1, Es) of
        {_, L1} ->
            Es1 = lists:keyreplace(M, 1, Es, {M, L ++ L1}),
            Info#forms{imports = Es1};
        false ->
            Info#forms{imports = [{M, L} | Es]}
    end.

finfo_add_attribute(Name, Val, Info) ->
    Info#forms{attributes = [{Name, Val} | Info#forms.attributes]}.

finfo_add_record(R, L, Info) ->
    Info#forms{records = [{R, L} | Info#forms.records]}.

finfo_add_error(R, Info) ->
    Info#forms{errors = [R | Info#forms.errors]}.

finfo_add_warning(R, Info) ->
    Info#forms{warnings = [R | Info#forms.warnings]}.

finfo_add_function(F, Info) ->
    Info#forms{functions = [F | Info#forms.functions]}.

finfo_to_list(Info) ->
    [{Key, Value}
     || {Key, {value, Value}} <-
            [{module, Info#forms.module},
             {exports, list_value(Info#forms.exports)},
             {imports, list_value(Info#forms.imports)},
             {module_imports, list_value(Info#forms.module_imports)},
             {attributes, list_value(Info#forms.attributes)},
             {records, list_value(Info#forms.records)},
             {errors, list_value(Info#forms.errors)},
             {warnings, list_value(Info#forms.warnings)},
             {functions, list_value(Info#forms.functions)}
            ]].

list_value([]) ->
    none;
list_value(List) ->
    {value, List}.


%% =====================================================================
%% @spec analyze_form(Node::syntaxTree()) -> {atom(), term()} | atom()
%%
%% @doc Analyzes a "source code form" node. If `Node' is a
%% "form" type (cf. `erl_syntax:is_form/1'), the returned
%% value is a tuple `{Type, Info}' where `Type' is
%% the node type and `Info' depends on `Type', as
%% follows:
%% <dl>
%%   <dt>`{attribute, Info}'</dt>
%%
%%      <dd>where `Info = analyze_attribute(Node)'.</dd>
%%
%%   <dt>`{error_marker, Info}'</dt>
%%
%% 	<dd>where `Info =
%% 	erl_syntax:error_marker_info(Node)'.</dd>
%%
%%   <dt>`{function, Info}'</dt>
%%
%% 	    <dd>where `Info = analyze_function(Node)'.</dd>
%%
%%   <dt>`{warning_marker, Info}'</dt>
%%
%% 	    <dd>where `Info =
%% 	    erl_syntax:warning_marker_info(Node)'.</dd>
%% </dl>
%% For other types of forms, only the node type is returned.
%%
%% The evaluation throws `syntax_error' if
%% `Node' is not well-formed.
%%
%% @see analyze_attribute/1
%% @see analyze_function/1
%% @see erl_syntax:is_form/1
%% @see erl_syntax:error_marker_info/1
%% @see erl_syntax:warning_marker_info/1

-spec analyze_form(erl_syntax:syntaxTree()) -> {atom(), term()} | atom().

analyze_form(Node) ->
    case erl_syntax:type(Node) of
        attribute ->
            {attribute, analyze_attribute(Node)};
        function ->
            {function, analyze_function(Node)};
        error_marker ->
            {error_marker, erl_syntax:error_marker_info(Node)};
        warning_marker ->
            {warning_marker, erl_syntax:warning_marker_info(Node)};
        _ ->
            case erl_syntax:is_form(Node) of
                true ->
                    erl_syntax:type(Node);
                false ->
                    throw(syntax_error)
            end
    end.

%% =====================================================================
%% @spec analyze_attribute(Node::syntaxTree()) ->
%%           preprocessor | {atom(), atom()}
%%
%% @doc Analyzes an attribute node. If `Node' represents a
%% preprocessor directive, the atom `preprocessor' is
%% returned. Otherwise, if `Node' represents a module
%% attribute "`-<em>Name</em>...'", a tuple `{Name,
%% Info}' is returned, where `Info' depends on
%% `Name', as follows:
%% <dl>
%%     <dt>`{module, Info}'</dt>
%%
%% 	    <dd>where `Info =
%% 	    analyze_module_attribute(Node)'.</dd>
%%
%%     <dt>`{export, Info}'</dt>
%%
%% 	    <dd>where `Info =
%% 	    analyze_export_attribute(Node)'.</dd>
%%
%%     <dt>`{import, Info}'</dt>
%%
%% 	    <dd>where `Info =
%% 	    analyze_import_attribute(Node)'.</dd>
%%
%%     <dt>`{file, Info}'</dt>
%%
%% 	    <dd>where `Info =
%% 	    analyze_file_attribute(Node)'.</dd>
%%
%%     <dt>`{record, Info}'</dt>
%%
%% 	    <dd>where `Info =
%% 	    analyze_record_attribute(Node)'.</dd>
%%
%%     <dt>`{Name, Info}'</dt>
%%
%% 	    <dd>where `{Name, Info} =
%% 	    analyze_wild_attribute(Node)'.</dd>
%% </dl>
%% The evaluation throws `syntax_error' if `Node'
%% does not represent a well-formed module attribute.
%%
%% @see analyze_module_attribute/1
%% @see analyze_export_attribute/1
%% @see analyze_import_attribute/1
%% @see analyze_file_attribute/1
%% @see analyze_record_attribute/1
%% @see analyze_wild_attribute/1

-spec analyze_attribute(erl_syntax:syntaxTree()) ->
        'preprocessor' | {atom(), term()}.  % XXX: underspecified

analyze_attribute(Node) ->
    Name = erl_syntax:attribute_name(Node),
    case erl_syntax:type(Name) of
        atom ->
            case erl_syntax:atom_value(Name) of
                define -> preprocessor;
                undef -> preprocessor;
                include -> preprocessor;
                include_lib -> preprocessor;
                ifdef -> preprocessor;
                ifndef -> preprocessor;
                'if' -> preprocessor;
                elif -> preprocessor;
                else -> preprocessor;
                endif -> preprocessor;
                A ->
                    {A, analyze_attribute(A, Node)}
            end;
        _ ->
            throw(syntax_error)
    end.

analyze_attribute(module, Node) ->
    analyze_module_attribute(Node);
analyze_attribute(export, Node) ->
    analyze_export_attribute(Node);
analyze_attribute(import, Node) ->
    analyze_import_attribute(Node);
analyze_attribute(file, Node) ->
    analyze_file_attribute(Node);
analyze_attribute(record, Node) ->
    analyze_record_attribute(Node);
analyze_attribute(_, Node) ->
    %% A "wild" attribute (such as e.g. a `compile' directive).
    analyze_wild_attribute(Node).


%% =====================================================================
%% @spec analyze_module_attribute(Node::syntaxTree()) ->
%%           Name::atom() | {Name::atom(), Variables::[atom()]}
%%
%% @doc Returns the module name and possible parameters declared by a
%% module attribute. If the attribute is a plain module declaration such
%% as `-module(name)', the result is the module name. If the attribute
%% is a parameterized module declaration, the result is a tuple
%% containing the module name and a list of the parameter variable
%% names.
%%
%% The evaluation throws `syntax_error' if `Node' does not represent a
%% well-formed module attribute.
%%
%% @see analyze_attribute/1

-spec analyze_module_attribute(erl_syntax:syntaxTree()) ->
        atom() | {atom(), [atom()]}.

analyze_module_attribute(Node) ->
    case erl_syntax:type(Node) of
        attribute ->
            case erl_syntax:attribute_arguments(Node) of
                [M] ->
                    module_name_to_atom(M);
                [M, L] ->
		    M1 = module_name_to_atom(M),
		    L1 = analyze_variable_list(L),
		    {M1, L1};
                _ ->
                    throw(syntax_error)
            end;
        _ ->
            throw(syntax_error)
    end.

analyze_variable_list(Node) ->
    case erl_syntax:is_proper_list(Node) of
        true ->
            [erl_syntax:variable_name(V)
	     || V <- erl_syntax:list_elements(Node)];
        false ->
            throw(syntax_error)
    end.


%% =====================================================================
%% @spec analyze_export_attribute(Node::syntaxTree()) -> [FunctionName]
%%
%%          FunctionName = atom() | {atom(), integer()}
%%                       | {ModuleName, FunctionName}
%%          ModuleName = atom()
%%
%% @doc Returns the list of function names declared by an export
%% attribute. We do not guarantee that each name occurs at most once in
%% the list. The order of listing is not defined.
%%
%% The evaluation throws `syntax_error' if `Node' does not represent a
%% well-formed export attribute.
%%
%% @see analyze_attribute/1

-type functionN()    :: atom() | {atom(), arity()}.
-type functionName() :: functionN() | {atom(), functionN()}.

-spec analyze_export_attribute(erl_syntax:syntaxTree()) -> [functionName()].

analyze_export_attribute(Node) ->
    case erl_syntax:type(Node) of
        attribute ->
            case erl_syntax:attribute_arguments(Node) of
                [L] ->
                    analyze_function_name_list(L);
                _ ->
                    throw(syntax_error)
            end;
        _ ->
            throw(syntax_error)
    end.

analyze_function_name_list(Node) ->
    case erl_syntax:is_proper_list(Node) of
        true ->
            [analyze_function_name(F)
             || F <- erl_syntax:list_elements(Node)];
        false ->
            throw(syntax_error)
    end.


%% =====================================================================
%% @spec analyze_function_name(Node::syntaxTree()) -> FunctionName
%%
%%          FunctionName = atom() | {atom(), integer()}
%%                       | {ModuleName, FunctionName}
%%          ModuleName = atom()
%%
%% @doc Returns the function name represented by a syntax tree. If
%% `Node' represents a function name, such as
%% "`foo/1'" or "`bloggs:fred/2'", a uniform
%% representation of that name is returned. Different nestings of arity
%% and module name qualifiers in the syntax tree does not affect the
%% result.
%%
%% The evaluation throws `syntax_error' if
%% `Node' does not represent a well-formed function name.

-spec analyze_function_name(erl_syntax:syntaxTree()) -> functionName().

analyze_function_name(Node) ->
    case erl_syntax:type(Node) of
        atom ->
            erl_syntax:atom_value(Node);
        arity_qualifier ->
            A = erl_syntax:arity_qualifier_argument(Node),
            case erl_syntax:type(A) of
                integer ->
                    F = erl_syntax:arity_qualifier_body(Node),
                    F1 = analyze_function_name(F),
                    append_arity(erl_syntax:integer_value(A), F1);
                _ ->
                    throw(syntax_error)
            end;
        module_qualifier ->
            M = erl_syntax:module_qualifier_argument(Node),
            case erl_syntax:type(M) of
                atom ->
                    F = erl_syntax:module_qualifier_body(Node),
                    F1 = analyze_function_name(F),
                    {erl_syntax:atom_value(M), F1};
                _ ->
                    throw(syntax_error)
            end;
        _ ->
            throw(syntax_error)
    end.

append_arity(A, {Module, Name}) ->
    {Module, append_arity(A, Name)};
append_arity(A, Name) when is_atom(Name) ->
    {Name, A};
append_arity(A, A) ->
    A;
append_arity(_A, Name) ->
    Name.    % quietly drop extra arity in case of conflict


%% =====================================================================
%% @spec analyze_import_attribute(Node::syntaxTree()) ->
%%           {atom(), [FunctionName]} | atom()
%%
%%          FunctionName = atom() | {atom(), integer()}
%%                       | {ModuleName, FunctionName}
%%          ModuleName = atom()
%%
%% @doc Returns the module name and (if present) list of function names
%% declared by an import attribute. The returned value is an atom
%% `Module' or a pair `{Module, Names}', where
%% `Names' is a list of function names declared as imported
%% from the module named by `Module'. We do not guarantee
%% that each name occurs at most once in `Names'. The order
%% of listing is not defined.
%%
%% The evaluation throws `syntax_error' if `Node' does not represent a
%% well-formed import attribute.
%%
%% @see analyze_attribute/1

-spec analyze_import_attribute(erl_syntax:syntaxTree()) ->
        {atom(), [functionName()]} | atom().

analyze_import_attribute(Node) ->
    case erl_syntax:type(Node) of
        attribute ->
            case erl_syntax:attribute_arguments(Node) of
		[M] ->
		    module_name_to_atom(M);
		[M, L] ->
		    M1 = module_name_to_atom(M),
		    L1 = analyze_function_name_list(L),
		    {M1, L1};
                _ ->
                    throw(syntax_error)
            end;
        _ ->
            throw(syntax_error)
    end.


%% =====================================================================
%% @spec analyze_type_name(Node::syntaxTree()) -> TypeName
%%
%%          TypeName = atom()
%%                   | {atom(), integer()}
%%                   | {ModuleName, {atom(), integer()}}
%%          ModuleName = atom()
%%
%% @doc Returns the type name represented by a syntax tree. If
%% `Node' represents a type name, such as
%% "`foo/1'" or "`bloggs:fred/2'", a uniform
%% representation of that name is returned.
%%
%% The evaluation throws `syntax_error' if
%% `Node' does not represent a well-formed type name.

-spec analyze_type_name(erl_syntax:syntaxTree()) -> typeName().

analyze_type_name(Node) ->
    case erl_syntax:type(Node) of
        atom ->
            erl_syntax:atom_value(Node);
        arity_qualifier ->
            A = erl_syntax:arity_qualifier_argument(Node),
            N = erl_syntax:arity_qualifier_body(Node),

            case ((erl_syntax:type(A) =:= integer)
                  and (erl_syntax:type(N) =:= atom))
            of
                true ->
                    append_arity(erl_syntax:integer_value(A),
                                 erl_syntax:atom_value(N));
                _ ->
                    throw(syntax_error)
            end;
        module_qualifier ->
            M = erl_syntax:module_qualifier_argument(Node),
            case erl_syntax:type(M) of
                atom ->
                    N = erl_syntax:module_qualifier_body(Node),
                    N1 = analyze_type_name(N),
                    {erl_syntax:atom_value(M), N1};
                _ ->
                    throw(syntax_error)
            end;
        _ ->
            throw(syntax_error)
    end.

%% =====================================================================
%% @spec analyze_wild_attribute(Node::syntaxTree()) -> {atom(), term()}
%%
%% @doc Returns the name and value of a "wild" attribute. The result is
%% the pair `{Name, Value}', if `Node' represents "`-Name(Value)'".
%%
%% Note that no checking is done whether `Name' is a
%% reserved attribute name such as `module' or
%% `export': it is assumed that the attribute is "wild".
%%
%% The evaluation throws `syntax_error' if `Node' does not represent a
%% well-formed wild attribute.
%%
%% @see analyze_attribute/1

-spec analyze_wild_attribute(erl_syntax:syntaxTree()) -> {atom(), term()}.

analyze_wild_attribute(Node) ->
    case erl_syntax:type(Node) of
        attribute ->
            N = erl_syntax:attribute_name(Node),
            case erl_syntax:type(N) of
                atom ->
                    case erl_syntax:attribute_arguments(Node) of
                        [V] ->
                            %% Note: does not work well with macros.
			    case catch {ok, erl_syntax:concrete(V)} of
				{ok, Val} ->
				    {erl_syntax:atom_value(N), Val};
				_ ->
				    throw(syntax_error)
			    end;
                        _ ->
                            throw(syntax_error)
                    end;
                _ ->
                    throw(syntax_error)
            end;
        _ ->
            throw(syntax_error)
    end.


%% =====================================================================
%% @spec analyze_record_attribute(Node::syntaxTree()) ->
%%           {atom(), Fields}
%%
%% 	    Fields = [{atom(), {Default, Type}}]
%% 	    Default = none | syntaxTree()
%% 	    Type = none | syntaxTree()
%%
%% @doc Returns the name and the list of fields of a record declaration
%% attribute. The result is a pair `{Name, Fields}', if
%% `Node' represents "`-record(Name, {...}).'",
%% where `Fields' is a list of pairs `{Label,
%% {Default, Type}}' for each field "`Label'", "`Label =
%% <em>Default</em>'", "`Label :: <em>Type</em>'", or
%% "`Label = <em>Default</em> :: <em>Type</em>'" in the declaration,
%% listed in left-to-right
%% order. If the field has no default-value declaration, the value for
%% `Default' will be the atom `none'. If the field has no type declaration,
%% the value for `Type' will be the atom `none'. We do not
%% guarantee that each label occurs at most once in the list.
%%
%% The evaluation throws `syntax_error' if
%% `Node' does not represent a well-formed record declaration
%% attribute.
%%
%% @see analyze_attribute/1
%% @see analyze_record_field/1

-type field() :: {atom(), {field_default(), field_type()}}.

-type fields() :: [field()].

-spec analyze_record_attribute(erl_syntax:syntaxTree()) -> {atom(), fields()}.

analyze_record_attribute(Node) ->
    case erl_syntax:type(Node) of
        attribute ->
            case erl_syntax:attribute_arguments(Node) of
                [R, T] ->
                    case erl_syntax:type(R) of
                        atom ->
                            Es = analyze_record_attribute_tuple(T),
                            {erl_syntax:atom_value(R), Es};
                        _ ->
                            throw(syntax_error)
                    end;
                _ ->
                    throw(syntax_error)
            end;
        _ ->
            throw(syntax_error)
    end.

analyze_record_attribute_tuple(Node) ->
    case erl_syntax:type(Node) of
        tuple ->
            [analyze_record_field(F)
	     || F <- erl_syntax:tuple_elements(Node)];
        _ ->
            throw(syntax_error)
    end.


%% =====================================================================
%% @spec analyze_record_expr(Node::syntaxTree()) ->
%%     {atom(), Info} | atom()
%%
%%    Info = {atom(), [{atom(), Value}]} | {atom(), atom()} | atom()
%%    Value = syntaxTree()
%%
%% @doc Returns the record name and field name/names of a record
%% expression. If `Node' has type `record_expr',
%% `record_index_expr' or `record_access', a pair
%% `{Type, Info}' is returned, otherwise an atom
%% `Type' is returned. `Type' is the node type of
%% `Node', and `Info' depends on
%% `Type', as follows:
%% <dl>
%%   <dt>`record_expr':</dt>
%%     <dd>`{atom(), [{atom(), Value}]}'</dd>
%%   <dt>`record_access':</dt>
%%     <dd>`{atom(), atom()}'</dd>
%%   <dt>`record_index_expr':</dt>
%%     <dd>`{atom(), atom()}'</dd>
%% </dl>
%%
%% For a `record_expr' node, `Info' represents
%% the record name and the list of descriptors for the involved fields,
%% listed in the order they appear. A field descriptor is a pair
%% `{Label, Value}', if `Node' represents "`Label = <em>Value</em>'".
%% For a `record_access' node,
%% `Info' represents the record name and the field name. For a
%% `record_index_expr' node, `Info' represents the
%% record name and the name field name.
%%
%% The evaluation throws `syntax_error' if
%% `Node' represents a record expression that is not
%% well-formed.
%%
%% @see analyze_record_attribute/1
%% @see analyze_record_field/1

-type info() :: {atom(), [{atom(), erl_syntax:syntaxTree()}]}
              | {atom(), atom()} | atom().

-spec analyze_record_expr(erl_syntax:syntaxTree()) -> {atom(), info()} | atom().

analyze_record_expr(Node) ->
    case erl_syntax:type(Node) of
	record_expr ->
            A = erl_syntax:record_expr_type(Node),
            case erl_syntax:type(A) of
                atom ->
                    Fs0 = [analyze_record_field(F)
                           || F <- erl_syntax:record_expr_fields(Node)],
                    Fs = [{N, D} || {N, {D, _T}} <- Fs0],
                    {record_expr, {erl_syntax:atom_value(A), Fs}};
                _ ->
                    throw(syntax_error)
            end;
	record_access ->
	    F = erl_syntax:record_access_field(Node),
	    case erl_syntax:type(F) of
		atom ->
		    A = erl_syntax:record_access_type(Node),
                    case erl_syntax:type(A) of
                        atom ->
                            {record_access,
                             {erl_syntax:atom_value(A),
                              erl_syntax:atom_value(F)}};
                        _ ->
                            throw(syntax_error)
		    end;
		_ ->
		    throw(syntax_error)
	    end;
	record_index_expr ->
	    F = erl_syntax:record_index_expr_field(Node),
	    case erl_syntax:type(F) of
		atom ->
		    A = erl_syntax:record_index_expr_type(Node),
		    case erl_syntax:type(A) of
			atom ->
			    {record_index_expr,
			     {erl_syntax:atom_value(A),
			      erl_syntax:atom_value(F)}};
			_ ->
			    throw(syntax_error)
		    end;
		_ ->
		    throw(syntax_error)
	    end;
	Type ->
	    Type
    end.

%% =====================================================================
%% @spec analyze_record_field(Node::syntaxTree()) -> {atom(), {Default, Type}}
%%
%%          Default = none | syntaxTree()
%%          Type = none | syntaxTree()
%%
%% @doc Returns the label, value-expression, and type of a record field
%% specifier. The result is a pair `{Label, {Default, Type}}', if
%% `Node' represents "`Label'", "`Label = <em>Default</em>'",
%% "`Label :: <em>Type</em>'", or
%%  "`Label = <em>Default</em> :: <em>Type</em>'".
%% If the field has no value-expression, the value for
%% `Default' will be the atom `none'. If the field has no type,
%% the value for `Type' will be the atom `none'. 
%%
%% The evaluation throws `syntax_error' if
%% `Node' does not represent a well-formed record field
%% specifier.
%%
%% @see analyze_record_attribute/1
%% @see analyze_record_expr/1

-spec analyze_record_field(erl_syntax:syntaxTree()) -> field().

analyze_record_field(Node) ->
    case erl_syntax:type(Node) of
        record_field ->
            A = erl_syntax:record_field_name(Node),
            case erl_syntax:type(A) of
                atom ->
                    T = erl_syntax:record_field_value(Node),
                    {erl_syntax:atom_value(A), {T, none}};
                _ ->
                    throw(syntax_error)
            end;
        typed_record_field ->
            F = erl_syntax:typed_record_field_body(Node),
            {N, {V, _none}} = analyze_record_field(F),
            T = erl_syntax:typed_record_field_type(Node),
            {N, {V, T}};
        _ ->
            throw(syntax_error)
    end.


%% =====================================================================
%% @spec analyze_file_attribute(Node::syntaxTree()) ->
%%           {string(), integer()}
%%
%% @doc Returns the file name and line number of a `file'
%% attribute. The result is the pair `{File, Line}' if
%% `Node' represents "`-file(File, Line).'".
%%
%% The evaluation throws `syntax_error' if
%% `Node' does not represent a well-formed `file'
%% attribute.
%%
%% @see analyze_attribute/1

-spec analyze_file_attribute(erl_syntax:syntaxTree()) -> {string(), integer()}.

analyze_file_attribute(Node) ->
    case erl_syntax:type(Node) of
        attribute ->
            case erl_syntax:attribute_arguments(Node) of
                [F, N] ->
                    case (erl_syntax:type(F) =:= string)
                        and (erl_syntax:type(N) =:= integer) of
                        true ->
                            {erl_syntax:string_value(F),
                             erl_syntax:integer_value(N)};
                        false ->
                            throw(syntax_error)
                    end;
                _ ->
                    throw(syntax_error)
            end;
        _ ->
            throw(syntax_error)
    end.


%% =====================================================================
%% @spec analyze_function(Node::syntaxTree()) -> {atom(), integer()}
%%
%% @doc Returns the name and arity of a function definition. The result
%% is a pair `{Name, A}' if `Node' represents a
%% function definition "`Name(<em>P_1</em>, ..., <em>P_A</em>) ->
%% ...'".
%%
%% The evaluation throws `syntax_error' if
%% `Node' does not represent a well-formed function
%% definition.

-spec analyze_function(erl_syntax:syntaxTree()) -> {atom(), arity()}.

analyze_function(Node) ->
    case erl_syntax:type(Node) of
        function ->
            N = erl_syntax:function_name(Node),
            case erl_syntax:type(N) of
                atom ->
                    {erl_syntax:atom_value(N),
                     erl_syntax:function_arity(Node)};
                _ ->
                    throw(syntax_error)
            end;
        _ ->
            throw(syntax_error)
    end.


%% =====================================================================
%% @spec analyze_implicit_fun(Node::syntaxTree()) -> FunctionName
%%
%%          FunctionName = atom() | {atom(), integer()}
%%                       | {ModuleName, FunctionName}
%%          ModuleName = atom()
%%      
%% @doc Returns the name of an implicit fun expression "`fun
%% <em>F</em>'". The result is a representation of the function
%% name `F'. (Cf. `analyze_function_name/1'.)
%%
%% The evaluation throws `syntax_error' if
%% `Node' does not represent a well-formed implicit fun.
%%
%% @see analyze_function_name/1

-spec analyze_implicit_fun(erl_syntax:syntaxTree()) -> functionName().

analyze_implicit_fun(Node) ->
    case erl_syntax:type(Node) of
        implicit_fun ->
            analyze_function_name(erl_syntax:implicit_fun_name(Node));
        _ ->
            throw(syntax_error)
    end.


%% =====================================================================
%% @spec analyze_application(Node::syntaxTree()) -> FunctionName | Arity
%%
%%          FunctionName = {atom(), Arity}
%%                       | {ModuleName, FunctionName}
%%          Arity = integer()
%%          ModuleName = atom()
%%
%% @doc Returns the name of a called function. The result is a
%% representation of the name of the applied function `F/A',
%% if `Node' represents a function application
%% "`<em>F</em>(<em>X_1</em>, ..., <em>X_A</em>)'". If the
%% function is not explicitly named (i.e., `F' is given by
%% some expression), only the arity `A' is returned.
%%
%% The evaluation throws `syntax_error' if `Node' does not represent a
%% well-formed application expression.
%%
%% @see analyze_function_name/1

-type appFunName() :: {atom(), arity()} | {atom(), {atom(), arity()}}.

-spec analyze_application(erl_syntax:syntaxTree()) -> appFunName() | arity().

analyze_application(Node) ->
    case erl_syntax:type(Node) of
        application ->
            A = length(erl_syntax:application_arguments(Node)),
            F = erl_syntax:application_operator(Node),
            case catch {ok, analyze_function_name(F)} of
                syntax_error ->
                    A;
                {ok, N} ->
                    append_arity(A, N);
                _ ->
                    throw(syntax_error)
            end;
        _ ->
            throw(syntax_error)
    end.


%% =====================================================================
%% @spec analyze_type_application(Node::syntaxTree()) -> TypeName
%%
%%          TypeName = {atom(), integer()}
%%                   | {ModuleName, {atom(), integer()}}
%%          ModuleName = atom()
%%
%% @doc Returns the name of a used type. The result is a
%% representation of the name of the used pre-defined or local type `N/A',
%% if `Node' represents a local (user) type application
%% "`<em>N</em>(<em>T_1</em>, ..., <em>T_A</em>)'", or
%% a representation of the name of the used remote type `M:N/A'
%% if `Node' represents a remote user type application
%% "`<em>M</em>:<em>N</em>(<em>T_1</em>, ..., <em>T_A</em>)'".
%%
%% The evaluation throws `syntax_error' if `Node' does not represent a
%% well-formed (user) type application expression.
%%
%% @see analyze_type_name/1

-type typeName() :: atom() | {module(), {atom(), arity()}} | {atom(), arity()}.

-spec analyze_type_application(erl_syntax:syntaxTree()) -> typeName().

analyze_type_application(Node) ->
    case erl_syntax:type(Node) of
        type_application ->
            A = length(erl_syntax:type_application_arguments(Node)),
            N = erl_syntax:type_application_name(Node),
            case catch {ok, analyze_type_name(N)} of
                {ok, TypeName} ->
                    append_arity(A, TypeName);
                _ ->
                    throw(syntax_error)
            end;
        user_type_application ->
            A = length(erl_syntax:user_type_application_arguments(Node)),
            N = erl_syntax:user_type_application_name(Node),
            case catch {ok, analyze_type_name(N)} of
                {ok, TypeName} ->
                    append_arity(A, TypeName);
                _ ->
                    throw(syntax_error)
            end;
        _ ->
            throw(syntax_error)
    end.


%% =====================================================================
%% @spec function_name_expansions(Names::[Name]) -> [{ShortName, Name}]
%%
%%          Name = ShortName | {atom(), Name}
%%          ShortName = atom() | {atom(), integer()}
%%
%% @doc Creates a mapping from corresponding short names to full
%% function names. Names are represented by nested tuples of atoms and
%% integers (cf. `analyze_function_name/1'). The result is a
%% list containing a pair `{ShortName, Name}' for each
%% element `Name' in the given list, where the corresponding
%% `ShortName' is the rightmost-innermost part of
%% `Name'. The list thus represents a finite mapping from
%% unqualified names to the corresponding qualified names.
%%
%% Note: the resulting list can contain more than one tuple
%% `{ShortName, Name}' for the same `ShortName',
%% possibly with different values for `Name', depending on
%% the given list.
%%
%% @see analyze_function_name/1

-type shortname() :: atom() | {atom(), arity()}.
-type name()      :: shortname() | {atom(), shortname()}.

-spec function_name_expansions([name()]) -> [{shortname(), name()}].

function_name_expansions(Fs) ->
    function_name_expansions(Fs, []).

function_name_expansions([F | Fs], Ack) ->
    function_name_expansions(Fs,
                             function_name_expansions(F, F, Ack));
function_name_expansions([], Ack) ->
    Ack.

function_name_expansions({A, N}, Name, Ack) when is_integer(N) ->
    [{{A, N}, Name} | Ack];
function_name_expansions({_, N}, Name, Ack) ->
    function_name_expansions(N, Name,  Ack);
function_name_expansions(A, Name, Ack) ->
    [{A, Name} | Ack].


%% =====================================================================
%% @spec strip_comments(Tree::syntaxTree()) -> syntaxTree()
%%
%% @doc Removes all comments from all nodes of a syntax tree. All other
%% attributes (such as position information) remain unchanged.
%% Standalone comments in form lists are removed; any other standalone
%% comments are changed into null-comments (no text, no indentation).

-spec strip_comments(erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().

strip_comments(Tree) ->
    map(fun strip_comments_1/1, Tree).

strip_comments_1(T) ->
    case erl_syntax:type(T) of
	form_list ->
	    Es = erl_syntax:form_list_elements(T),
	    Es1 = [E || E <- Es, erl_syntax:type(E) /= comment],
	    T1 = erl_syntax:copy_attrs(T, erl_syntax:form_list(Es1)),
	    erl_syntax:remove_comments(T1);
	comment ->
	    erl_syntax:comment([]);
	_ ->
	    erl_syntax:remove_comments(T)
    end.

%% =====================================================================
%% @spec to_comment(Tree) -> syntaxTree()
%% @equiv to_comment(Tree, "% ")

-spec to_comment(erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().

to_comment(Tree) ->
    to_comment(Tree, "% ").

%% =====================================================================
%% @spec to_comment(Tree::syntaxTree(), Prefix::string()) ->
%%           syntaxTree()
%%
%% @doc Equivalent to `to_comment(Tree, Prefix, F)' for a
%% default formatting function `F'. The default
%% `F' simply calls `erl_prettypr:format/1'.
%%
%% @see to_comment/3
%% @see erl_prettypr:format/1

-spec to_comment(erl_syntax:syntaxTree(), string()) -> erl_syntax:syntaxTree().

to_comment(Tree, Prefix) ->
    F = fun (T) -> erl_prettypr:format(T) end,
    to_comment(Tree, Prefix, F).

%% =====================================================================
%% @spec to_comment(Tree::syntaxTree(), Prefix::string(), Printer) ->
%%           syntaxTree()
%%
%%          Printer = (syntaxTree()) -> string()
%%
%% @doc Transforms a syntax tree into an abstract comment. The lines of
%% the comment contain the text for `Node', as produced by
%% the given `Printer' function. Each line of the comment is
%% prefixed by the string `Prefix' (this does not include the
%% initial "`%'" character of the comment line).
%%
%% For example, the result of
%% `to_comment(erl_syntax:abstract([a,b,c]))' represents
%% <pre>
%%         %% [a,b,c]</pre>
%% (cf. `to_comment/1').
%%
%% Note: the text returned by the formatting function will be split
%% automatically into separate comment lines at each line break. No
%% extra work is needed.
%%
%% @see to_comment/1
%% @see to_comment/2

-spec to_comment(erl_syntax:syntaxTree(), string(),
		 fun((erl_syntax:syntaxTree()) -> string())) ->
        erl_syntax:syntaxTree().

to_comment(Tree, Prefix, F) ->
    erl_syntax:comment(split_lines(F(Tree), Prefix)).


%% =====================================================================
%% @spec limit(Tree, Depth) -> syntaxTree()
%%
%% @doc Equivalent to `limit(Tree, Depth, Text)' using the
%% text `"..."' as default replacement.
%%
%% @see limit/3
%% @see erl_syntax:text/1

-spec limit(erl_syntax:syntaxTree(), integer()) -> erl_syntax:syntaxTree().

limit(Tree, Depth) ->
    limit(Tree, Depth, erl_syntax:text("...")).

%% =====================================================================
%% @spec limit(Tree::syntaxTree(), Depth::integer(),
%%             Node::syntaxTree()) -> syntaxTree()
%%
%% @doc Limits a syntax tree to a specified depth. Replaces all non-leaf
%% subtrees in `Tree' at the given `Depth' by
%% `Node'. If `Depth' is negative, the result is
%% always `Node', even if `Tree' has no subtrees.
%%
%% When a group of subtrees (as e.g., the argument list of an
%% `application' node) is at the specified depth, and there
%% are two or more subtrees in the group, these will be collectively
%% replaced by `Node' even if they are leaf nodes. Groups of
%% subtrees that are above the specified depth will be limited in size,
%% as if each subsequent tree in the group were one level deeper than
%% the previous. E.g., if `Tree' represents a list of
%% integers "`[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]'", the result
%% of `limit(Tree, 5)' will represent `[1, 2, 3, 4,
%% ...]'.
%%
%% The resulting syntax tree is typically only useful for
%% pretty-printing or similar visual formatting.
%%
%% @see limit/2

-spec limit(erl_syntax:syntaxTree(), integer(), erl_syntax:syntaxTree()) ->
        erl_syntax:syntaxTree().

limit(_Tree, Depth, Node) when Depth < 0 ->
    Node;
limit(Tree, Depth, Node) ->
    limit_1(Tree, Depth, Node).

limit_1(Tree, Depth, Node) ->
    %% Depth is nonnegative here.
    case erl_syntax:subtrees(Tree) of
        [] ->
            if Depth > 0 ->
                    Tree;
               true ->
                    case is_simple_leaf(Tree) of
                        true ->
                            Tree;
                        false ->
                            Node
                    end
            end;
        Gs ->
            if Depth > 1 ->
                    Gs1 = [[limit_1(T, Depth - 1, Node)
                            || T <- limit_list(G, Depth, Node)]
                           || G <- Gs],
                    rewrite(Tree,
                            erl_syntax:make_tree(erl_syntax:type(Tree),
                                                 Gs1));
               Depth =:= 0 ->
                    %% Depth is zero, and this is not a leaf node
                    %% so we always replace it.
                    Node;
               true ->
                    %% Depth is 1, so all subtrees are to be cut.
                    %% This is done groupwise.
                    Gs1 = [cut_group(G, Node) || G <- Gs],
                    rewrite(Tree,
                            erl_syntax:make_tree(erl_syntax:type(Tree),
                                                 Gs1))
            end
    end.

cut_group([], _Node) ->
    [];
cut_group([T], Node) ->
    %% Only if the group contains a single subtree do we try to
    %% preserve it if suitable.
    [limit_1(T, 0, Node)];
cut_group(_Ts, Node) ->
    [Node].

is_simple_leaf(Tree) ->
    case erl_syntax:type(Tree) of
        atom -> true;
        char -> true;
        float -> true;
        integer -> true;
        nil -> true;
        operator -> true;
        tuple -> true;
        underscore -> true;
        variable -> true;
        _ -> false
    end.

%% If list has more than N elements, take the N - 1 first and
%% append Node; otherwise return list as is.

limit_list(Ts, N, Node) ->
    if length(Ts) > N ->
            limit_list_1(Ts, N - 1, Node);
       true ->
            Ts
    end.

limit_list_1([T | Ts], N, Node) ->
    if N > 0 ->
            [T | limit_list_1(Ts, N - 1, Node)];
       true ->
            [Node]
    end;
limit_list_1([], _N, _Node) ->
    [].


%% =====================================================================
%% Utility functions

rewrite(Tree, Tree1) ->
    erl_syntax:copy_attrs(Tree, Tree1).

module_name_to_atom(M) ->
    case erl_syntax:type(M) of
	atom ->
	    erl_syntax:atom_value(M);
	_ ->
	    throw(syntax_error)
    end.

%% This splits lines at line terminators and expands tab characters to
%% spaces. The width of a tab is assumed to be 8.

% split_lines(Cs) ->
%     split_lines(Cs, "").

split_lines(Cs, Prefix) ->
    split_lines(Cs, Prefix, 0).

split_lines(Cs, Prefix, N) ->
    lists:reverse(split_lines(Cs, N, [], [], Prefix)).

split_lines([$\r, $\n | Cs], _N, Cs1, Ls, Prefix) ->
    split_lines_1(Cs, Cs1, Ls, Prefix);
split_lines([$\r | Cs], _N, Cs1, Ls, Prefix) ->
    split_lines_1(Cs, Cs1, Ls, Prefix);
split_lines([$\n | Cs], _N, Cs1, Ls, Prefix) ->
    split_lines_1(Cs, Cs1, Ls, Prefix);
split_lines([$\t | Cs], N, Cs1, Ls, Prefix) ->
    split_lines(Cs, 0, push(8 - (N rem 8), $\040, Cs1), Ls,
                Prefix);
split_lines([C | Cs], N, Cs1, Ls, Prefix) ->
    split_lines(Cs, N + 1, [C | Cs1], Ls, Prefix);
split_lines([], _, [], Ls, _) ->
    Ls;
split_lines([], _N, Cs, Ls, Prefix) ->
    [Prefix ++ lists:reverse(Cs) | Ls].

split_lines_1(Cs, Cs1, Ls, Prefix) ->
    split_lines(Cs, 0, [], [Prefix ++ lists:reverse(Cs1) | Ls],
                Prefix).

push(N, C, Cs) when N > 0 ->
    push(N - 1, C, [C | Cs]);
push(0, _, Cs) ->
    Cs.

