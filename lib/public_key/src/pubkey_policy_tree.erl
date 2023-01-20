%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023-2023. All Rights Reserved.
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
-module(pubkey_policy_tree).

-include("../include/public_key.hrl").

%% API
-export([add_leaves/2,
         add_siblings/2,
         any_leaf/2,
         collect_qualifiers/2,
         constraint_policy_node_set/1,
         empty/0,
         in_set/2,
         is_empty/1,
         map_leaves/2,
         policy_node/3,
         prune_leaves/2,
         prune_tree/1,
         prune_invalid_nodes/2,
         root/0,
         valid_policy_node_set/1
        ]).

-export_type([policy_tree/0]).

-type policy_node() ::
        #{valid_policy := public_key:oid(),
          qualifier_set := [#'PolicyInformation'{}],
          expected_policy_set := [public_key:oid()]}.

-type policy_tree_node()   :: {policy_node(), [policy_tree_node()] |
                               [policy_node()]}.

-opaque policy_tree()        :: {} | policy_tree_node().

%%%===================================================================
%%% Internal API
%%%===================================================================

%%--------------------------------------------------------------------
-spec add_leaves(policy_tree(), LeafFun::function()) -> policy_tree().
%%
%% Add leaves specified by calling LeafFun with the current leaves
%% as input
%%--------------------------------------------------------------------
add_leaves({Parent, []}, LeafFun) ->
    {Parent, LeafFun(Parent)};
add_leaves(Tree, LeafFun0) ->
    LeafFun = fun(Leaf) ->
                      NewLeaves = LeafFun0(Leaf),
                      {Leaf, NewLeaves}
              end,
    map_leaves(Tree, LeafFun).

%%--------------------------------------------------------------------
-spec add_siblings(policy_tree(), SiblingFun::function()) -> policy_tree().
%%
%% Add sibling leaves if SiblingFun returns a list of policy nodes
%% for the leaf parent.
%%--------------------------------------------------------------------
add_siblings({Parent,[{_, _}|_] = SubTree}, SiblingFun) ->
    {Parent, lists:map(fun(Children)->
                               add_siblings(Children, SiblingFun)
                       end, SubTree)};
add_siblings({Parent, Leaves} = Node, SiblingFun) ->
    case SiblingFun(Parent) of
        no_sibling ->
            Node;
        Siblings ->
            {Parent, Leaves ++ Siblings}
    end.

%%--------------------------------------------------------------------
-spec any_leaf(policy_tree(), Depth::integer()) -> no_node | public_key:policy_node().
%%
%% Find a leaf node at tree <Depth>  with valid_policy = ?anyPolicy if it exists
%%--------------------------------------------------------------------
any_leaf({}, _) ->
    no_node;
any_leaf({_Root, SubTree}, Depth) ->
    find_any_leaf(SubTree, Depth-1).

%%--------------------------------------------------------------------
-spec collect_qualifiers(policy_tree(), public_key:oid()) -> [{uri, string()} | #'UserNotice'{}].
%%
%% Collect qualifiers from tree branch asserting policy
%%--------------------------------------------------------------------
collect_qualifiers({_, SubTree}, Policy) ->
    FormatQualifier =
        fun(#'PolicyQualifierInfo'{policyQualifierId = ?'id-qt-unotice',
                                   qualifier = Qualifier}) ->
                try public_key:der_decode('UserNotice', Qualifier) of
                    Notice ->
                        Notice
                catch error:_ ->
                        workaround_to_long_notice(Qualifier)
                end;
           (#'PolicyQualifierInfo'{policyQualifierId = ?'id-qt-cps',
                                   qualifier = Qualifier}) ->
                {uri, public_key:der_decode('CPSuri', Qualifier)}
        end,
    Collect = fun(#{qualifier_set := QSet}) ->
                      lists:map(FormatQualifier, QSet)
              end,
    case collect_subtree_qualifiers(Collect, SubTree, Policy) of
        [] ->
            collect_subtree_qualifiers(Collect, SubTree, ?anyPolicy);
        QSet ->
            QSet
    end.

%% --------------------------------------------------------------------
-spec constraint_policy_node_set(policy_tree()) -> [public_key:policy_node()].

%%
%% From: PKITS.pdf:
%% If the valid_policy_tree includes a leaf node with a valid_policy
%% of anyPolicy, then the user-constrained-policy-set is
%% any-policy. Otherwise, the user-constrained-policy-set consists of
%% the set containing the valid_policy from each node in the
%% valid_policy_tree in which the valid_policy is not anyPolicy and
%% the valid_policy of that node's parent is anyPolicy.  The
%% authorities-constrained-policy-set may be computed using the same
%% procedure on the valid_policy_tree before its intersection with the
%% user-initial-policy-set has calculated
%%
%% That is this function caclualates the valid policy node
%% set after being constrained either by the user or only by the
%% authorities (certificate extensions).
%% --------------------------------------------------------------------
constraint_policy_node_set({}) ->
    [];
constraint_policy_node_set(Tree) ->
    case any_leaves(Tree) of
        [] ->
            constrain(Tree);
        AnyLeaves ->
            AnyLeaves
    end.

%%--------------------------------------------------------------------
-spec empty() -> policy_tree().
%%--------------------------------------------------------------------
empty() ->
    {}.

%%--------------------------------------------------------------------
-spec in_set(Policy::public_key:oid(), Set::[public_key:policy_node()]) -> boolean().
%%
%% Is there a node with <Policy> in <Set>
%%--------------------------------------------------------------------
in_set(_, []) ->
    false;
in_set(Policy, [#{valid_policy := Policy} |_]) ->
    true;
in_set(Policy, [_ | Rest]) ->
    in_set(Policy, Rest).

%%--------------------------------------------------------------------
-spec is_empty(policy_tree()) -> boolean().
%%--------------------------------------------------------------------
is_empty({}) ->
    true;
is_empty(_) ->
    false.

%%--------------------------------------------------------------------
-spec map_leaves(policy_tree(), LeafFun::function()) -> policy_tree().
%%
%% Update all leaves as determined by <LeafFun>
%%--------------------------------------------------------------------
map_leaves({Parent, [{_, _}|_] = SubTree}, LeafFun) ->
    {Parent, lists:map(fun(Children)->
                               map_leaves(Children, LeafFun)
                       end, SubTree)};
map_leaves({Parent, Leaves}, LeafFun) ->
    {Parent, lists:map(LeafFun, Leaves)}.

%%--------------------------------------------------------------------
-spec prune_leaves(policy_tree(), Policy::public_key:oid()) -> policy_tree().
%%
%% Delete all leaves with the valid_policy <Policy>
%%--------------------------------------------------------------------
prune_leaves({} = Empty, _) ->
    Empty;
prune_leaves({_, _} = Tree, Policy) ->
    LeafFun = fun(#{valid_policy := ValidPolicy} = Node) ->
                      case ValidPolicy of
                          Policy ->
                              false;
                          _ ->
                              {true, Node}
                      end
              end,
    filter_leaves(Tree, LeafFun).

%%--------------------------------------------------------------------
-spec prune_tree(policy_tree()) -> policy_tree().
%%
%% Delete branches that are shorher than the total depth of the tree.
%%--------------------------------------------------------------------
prune_tree({} = Empty) ->
    Empty;
prune_tree({_, []}) ->
    empty();
prune_tree({Root, SubTree}) ->
    case prune_subtree(SubTree) of
        [] ->
            empty();
        NewSubTree ->
            {Root, NewSubTree}
    end.

%%--------------------------------------------------------------------
-spec prune_invalid_nodes(policy_tree(), [public_key:policy_node()]) -> policy_tree().
%%
%% Delete all invalid policy nodes and thier children.
%%--------------------------------------------------------------------
prune_invalid_nodes(Tree, []) ->
    Tree;
prune_invalid_nodes({Root, SubTree}, InvalidNodes) ->
    case prune_invalid_nodes_subtree(SubTree, InvalidNodes) of
        [] -> %% No leaves left the tree becomes empty
            empty();
        NewSubTree -> %% Keep root that is always ?anyPolicy
            {Root, NewSubTree}
    end.
%%--------------------------------------------------------------------
-spec policy_node(public_key:oid(), term(), [public_key:oid()]) -> public_key:policy_node().
%%
%% Creates a policy node
%%--------------------------------------------------------------------
policy_node(ValidPolicy, Qualifiers, ExpPolicySet) ->
    QualifierSet = 
        case Qualifiers of
            asn1_NOVALUE ->
                [];
            _ ->
                Qualifiers
        end,
    #{valid_policy => ValidPolicy,
      qualifier_set => QualifierSet,
      expected_policy_set => ExpPolicySet}.

%%--------------------------------------------------------------------
-spec root() -> policy_tree().
%%
%% Create initial root node used as start value when building the policy tree.
%%--------------------------------------------------------------------
root() ->
    {any_node(), []}.

%%--------------------------------------------------------------------
-spec valid_policy_node_set(policy_tree()) -> [public_key:policy_node()].
%%
%% Determine the set of policy nodes whose parent nodes have a
%% valid_policy of anyPolicy.  This is the valid_policy_node_set.
%%--------------------------------------------------------------------
valid_policy_node_set({#{valid_policy := ?anyPolicy}, [{_, _}| _] = SubTree}) ->
    Parents = [Parent || {Parent, _} <- SubTree],
    Parents ++ lists:foldl(fun(Child, Acc) ->
                                   valid_policy_node_set(Child) ++ Acc
                           end, [], SubTree);
valid_policy_node_set({#{valid_policy := ?anyPolicy}, Leaves}) when is_list(Leaves) ->
    Leaves;
valid_policy_node_set(_) ->
    [].

%%%===================================================================
%%% Internal functions
%%%===================================================================
any_node() ->
    policy_node(?anyPolicy, [], [?anyPolicy]).

any_leaves({_,[{_, _}|_] = SubTree}) ->
    lists:flatmap(fun(Children)->
                          any_leaves(Children)
                  end, SubTree);
any_leaves({_, Leaves}) ->
    AnyLeaf = fun(#{valid_policy := ?anyPolicy} = Node) ->
                      {true, Node};
                 (_) ->
                      false
              end,
    lists:filtermap(AnyLeaf, Leaves).

collect_subtree_qualifiers(_, [], _) ->
    [];
collect_subtree_qualifiers(Collect, [{#{expected_policy_set := Set}, _} = Branch | SubTree], Policy) ->
    case lists:member(Policy, Set) of
        true ->
            lists:flatten(subtree_collect(Collect, Branch));
        false ->
            collect_subtree_qualifiers(Collect, SubTree, Policy)
    end;
collect_subtree_qualifiers(Collect, Children, _) ->
    lists:flatten(subtree_collect(Collect, Children)).

subtree_collect(Collect, {Parent, SubTree})  ->
    Collect(Parent) ++ subtree_collect(Collect, SubTree);
subtree_collect(Collect, [ {_,_} = Branch | SubTree]) ->
    subtree_collect(Collect, Branch) ++ subtree_collect(Collect, SubTree);
subtree_collect(Collect, Leaves) ->
    lists:map(Collect, Leaves).

constrain({#{valid_policy := ?anyPolicy}, [{_, _}| _] = SubTree})  ->
    Parents = [Parent || {Parent, _} <- SubTree],
    lists:filtermap(fun(#{valid_policy := ?anyPolicy}) ->
                            false;
                       (Node) ->
                            {true, Node}
                    end, Parents) ++
        lists:foldl(fun(Child, Acc) ->
                            constrain(Child) ++ Acc
                    end, [], SubTree);
constrain({#{valid_policy := ?anyPolicy}, Leaves}) when is_list(Leaves) ->
    lists:filtermap(fun(#{valid_policy := ?anyPolicy}) ->
                            false;
                       (Node) ->
                            {true, Node}
                    end, Leaves);
constrain(_) ->
    [].

filter_leaves({Parent,[{_, _}|_] = SubTree}, LeafFun) ->
    {Parent, lists:map(fun(Children)->
                               filter_leaves(Children, LeafFun)
                       end, SubTree)};
filter_leaves({Parent, Leaves}, LeafFun) ->
    {Parent, lists:filtermap(LeafFun, Leaves)}.

find_any_leaf(Children, N) when  N > 0->
    find_any_leaf(Children, N-1);
find_any_leaf([{_, Children}], 0) ->
    find_any_leaf(Children);
find_any_leaf(Children, 0) ->
    find_any_leaf(Children).

find_any_leaf([]) ->
    no_node;
find_any_leaf([#{valid_policy := ?anyPolicy} = Node| _]) ->
    Node;
find_any_leaf([_ | Rest]) ->
    find_any_leaf(Rest).

keep_policy_node(#{valid_policy := ?anyPolicy} = Node, _) ->
    {true, Node};
keep_policy_node(Node, InvalidNodes) ->
    case lists:member(Node, InvalidNodes) of
        true ->
            false;
        false ->
            {true, Node}
    end.

prune_subtree(SubTree) when is_list(SubTree) ->
    lists:filtermap(fun({Parent, Children}) ->
                            case prune_nodes(Parent, Children) of
                                {Parent, []} ->
                                    false;
                                {Parent, NewChildren} ->
                                    {true, {Parent, NewChildren}}
                            end;
                       (Leaves) ->
                            {true, Leaves}
                    end, SubTree).

prune_nodes(Parent, Children) ->
    {Parent, prune_subtree(Children)}.

prune_invalid_nodes_subtree(SubTree, InvalidNodes) when is_list(SubTree)->
    lists:filtermap(fun({Parent, Children}) ->
                            case keep_policy_node(Parent, InvalidNodes) of
                                false -> %% Prune branch
                                    false;
                                {true, #{valid_policy := ?anyPolicy} = Parent} -> %% Keep Parent
                                    {true, {Parent, prune_invalid_nodes_subtree(Children, InvalidNodes)}};
                                {true, Parent} ->
                                    {true, {Parent, Children}} %% Keep branch
                            end;
                       (#{} = Child) -> % Possibly prune leaf
                            keep_policy_node(Child, InvalidNodes)
                    end, SubTree).

workaround_to_long_notice(Qualifier) ->
    %% RFC 3280 states that certificate users SHOULD gracefully handle
    %% explicitText with more than 200 characters.
    try public_key:der_decode('OTPUserNotice', Qualifier) of
        #'OTPUserNotice'{noticeRef = Ref,
                         explicitText = DispText} ->
            #'UserNotice'{noticeRef = Ref,
                          explicitText = DispText}
    catch error:_ ->
            #'UserNotice'{noticeRef = asn1_NOVALUE,
                          explicitText = "User Notice much too long, so value is ignored"}
    end.
