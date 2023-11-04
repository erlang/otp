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
         add_leaf_siblings/2,
         any_leaves/1,
         all_leaves/1,
         collect_qualifiers/2,
         constrained_policy_node_set/1,
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
          qualifier_set := [#'PolicyQualifierInfo'{}],
          expected_policy_set := [public_key:oid()]}.

-type policy_tree_node()   :: {policy_node(), [policy_tree_node()] |
                               [policy_node()]}.

-opaque policy_tree()        :: {} | policy_tree_node().

%%%===================================================================
%%% Internal API
%%%===================================================================

%%--------------------------------------------------------------------
-spec add_leaves(policy_tree(), LeafFun) -> policy_tree() when
      LeafFun :: fun((policy_tree_node()) -> [policy_node()]).

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
-spec add_leaf_siblings(policy_tree(), SiblingFun) -> policy_tree() when
      SiblingFun ::fun((policy_tree_node()) -> no_sibling | [policy_node()]).

%%
%% Add sibling leaves if SiblingFun returns a list of policy nodes
%% for the leaf parent.
%%--------------------------------------------------------------------
add_leaf_siblings({Parent,[{_, _}|_] = ChildNodes}, SiblingFun) ->
    {Parent, lists:map(fun(ChildNode)->
                               add_leaf_siblings(ChildNode, SiblingFun)
                       end, ChildNodes)};
add_leaf_siblings({Parent, Leaves} = Node, SiblingFun) ->
    case SiblingFun(Parent) of
        no_sibling ->
            Node;
        Siblings ->
            {Parent, Leaves ++ Siblings}
    end.

%%--------------------------------------------------------------------
-spec any_leaves(policy_tree()) -> [policy_node()].
%%
%% Find leaf policy nodes with valid_policy = ?anyPolicy if they exists
%%--------------------------------------------------------------------
any_leaves({}) ->
    [];
any_leaves({_,[{_, _}|_] = ChildNodes}) ->
    lists:flatmap(fun(ChildNode)->
                          any_leaves(ChildNode)
                  end, ChildNodes);
any_leaves({_, Leaves}) ->
    AnyLeaf = fun(#{valid_policy := ?anyPolicy} = Node) ->
                      {true, Node};
                 (_) ->
                      false
              end,
    lists:filtermap(AnyLeaf, Leaves).

%%--------------------------------------------------------------------
-spec all_leaves(policy_tree()) -> [policy_node()].
%%
%% Return list of all leaves
%%--------------------------------------------------------------------
all_leaves({}) ->
    [];
all_leaves({_,[{_, _}|_] = ChildNodes}) ->
    lists:flatmap(fun(ChildNode)->
                          all_leaves(ChildNode)
                  end, ChildNodes);
all_leaves({_, Leaves}) ->
    Leaves.

%%--------------------------------------------------------------------
-spec collect_qualifiers(policy_tree(), Policy::public_key:oid()) ->
          [{uri, string()} | #'UserNotice'{}].
%%
%% Collect qualifiers from tree branch asserting Policy
%%--------------------------------------------------------------------
collect_qualifiers({_, ChildNodes}, Policy) ->
    FormatQualifier =
        fun(#'PolicyQualifierInfo'{policyQualifierId = ?'id-qt-unotice',
                                   qualifier = Qualifier}) ->
                try public_key:der_decode('UserNotice', Qualifier) of
                    Notice ->
                        Notice
                catch error:_ ->
                        handle_too_long_notice(Qualifier)
                end;
           (#'PolicyQualifierInfo'{policyQualifierId = ?'id-qt-cps',
                                   qualifier = Qualifier}) ->
                {uri, public_key:der_decode('CPSuri', Qualifier)}
        end,
    Collect = fun(#{qualifier_set := QSet}) ->
                      lists:map(FormatQualifier, QSet)
              end,
    case collect_children_qualifiers(Collect, ChildNodes, Policy) of
        [] ->
            collect_children_qualifiers(Collect, ChildNodes, ?anyPolicy);
        QSet ->
            QSet
    end.

%% --------------------------------------------------------------------
-spec constrained_policy_node_set(policy_tree()) -> [policy_node()].

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
%% user-initial-policy-set has been computed  (in step g of section 6.1.5). 
%%
%% That is this function calculates the valid policy node
%% set after being constrained either by the user or only by the
%% authorities (certificate extensions).
%% --------------------------------------------------------------------
constrained_policy_node_set({}) ->
    [];
constrained_policy_node_set(Tree) ->
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
    %% The empty tree is refered to as NULL in RFC 5280
    {}.

%%--------------------------------------------------------------------
-spec in_set(Policy::public_key:oid(), Set::[policy_node()]) -> boolean().
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
map_leaves({Parent, [{_, _}|_] = ChildNodes}, LeafFun) ->
    {Parent, lists:map(fun(ChildNode)->
                               map_leaves(ChildNode, LeafFun)
                       end, ChildNodes)};
map_leaves({Parent, Leaves}, LeafFun) ->
    {Parent, lists:map(LeafFun, Leaves)}.

%%--------------------------------------------------------------------
-spec prune_leaves(policy_tree(), Policy::public_key:oid()) -> policy_tree().
%%
%% Delete all leaves with the valid_policy <Policy> in a pruned policy tree
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
prune_tree({Root, ChildNodes}) ->
    case prune_children(ChildNodes) of
        [] ->
            empty();
        NewChildNodes ->
            {Root, NewChildNodes}
    end.

%%--------------------------------------------------------------------
-spec prune_invalid_nodes(policy_tree(), [policy_node()]) -> policy_tree().
%%
%% Delete all invalid policy nodes and their children.
%%--------------------------------------------------------------------
prune_invalid_nodes(Tree, []) ->
    Tree;
prune_invalid_nodes({Root, ChildNodes}, InvalidNodes) ->
    case prune_invalid_nodes_children(ChildNodes, InvalidNodes) of
        [] -> %% No leaves left the tree becomes empty
            empty();
        NewChildNodes -> %% Keep root that is always ?anyPolicy
            {Root, NewChildNodes}
    end.
%%--------------------------------------------------------------------
-spec policy_node(public_key:oid(), term(), [public_key:oid()]) -> policy_node().
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
    {any_policy_node(), []}.

%%--------------------------------------------------------------------
-spec valid_policy_node_set(policy_tree()) -> [policy_node()].
%%
%% Determine the set of policy nodes whose parent nodes have a
%% valid_policy of anyPolicy.  This is the valid_policy_node_set.
%%--------------------------------------------------------------------
valid_policy_node_set({#{valid_policy := ?anyPolicy}, [{_, _}| _] = ChildNodes}) ->
    Parents = [Parent || {Parent, _} <- ChildNodes],
    Parents ++ lists:foldl(fun(ChildNode, Acc) ->
                                   valid_policy_node_set(ChildNode) ++ Acc
                           end, [], ChildNodes);
valid_policy_node_set({#{valid_policy := ?anyPolicy}, Leaves}) when is_list(Leaves) ->
    Leaves;
valid_policy_node_set(_) ->
    [].

%%%===================================================================
%%% Internal functions
%%%===================================================================
any_policy_node() ->
    policy_node(?anyPolicy, [], [?anyPolicy]).

collect_children_qualifiers(_, [], _) ->
    [];
collect_children_qualifiers(Collect, [{#{expected_policy_set := Set}, _} = ChildNode | ChildNodes],
                            Policy) ->
    case lists:member(Policy, Set) of
        true ->
            lists:flatten(children_collect(Collect, ChildNode));
        false ->
            collect_children_qualifiers(Collect, ChildNodes, Policy)
    end;
collect_children_qualifiers(Collect, ChildNodes, _) ->
    lists:flatten(children_collect(Collect, ChildNodes)).

children_collect(Collect, {Parent, ChildNodes})  ->
    Collect(Parent) ++ children_collect(Collect, ChildNodes);
children_collect(Collect, [ {_,_} = ChildNode | ChildNodes]) ->
    children_collect(Collect, ChildNode) ++ children_collect(Collect, ChildNodes);
children_collect(Collect, Leaves) ->
    lists:map(Collect, Leaves).

constrain({#{valid_policy := ?anyPolicy}, [{_, _}| _] = ChildNodes})  ->
    Parents = [Parent || {Parent, _} <- ChildNodes],
    lists:filtermap(fun(#{valid_policy := ?anyPolicy}) ->
                            false;
                       (Node) ->
                            {true, Node}
                    end, Parents) ++
        lists:foldl(fun(Child, Acc) ->
                            constrain(Child) ++ Acc
                    end, [], ChildNodes);
constrain({#{valid_policy := ?anyPolicy}, Leaves}) when is_list(Leaves) ->
    lists:filtermap(fun(#{valid_policy := ?anyPolicy}) ->
                            false;
                       (Node) ->
                            {true, Node}
                    end, Leaves);
constrain(_) ->
    [].

filter_leaves({Parent,[{_, _}|_] = ChildNodes}, LeafFun) ->
    {Parent, lists:map(fun(ChildNode)->
                               filter_leaves(ChildNode, LeafFun)
                       end, ChildNodes)};
filter_leaves({Parent, Leaves}, LeafFun) ->
    {Parent, lists:filtermap(LeafFun, Leaves)}.

keep_policy_node(#{valid_policy := ?anyPolicy} = Node, _) ->
    {true, Node};
keep_policy_node(Node, InvalidNodes) ->
    case lists:member(Node, InvalidNodes) of
        true ->
            false;
        false ->
            {true, Node}
    end.

prune_children(ChildNodes) when is_list(ChildNodes) ->
    lists:filtermap(fun({Parent, Children}) ->
                            case prune_nodes(Parent, Children) of
                                {Parent, []} ->
                                    false;
                                {Parent, NewChildren} ->
                                    {true, {Parent, NewChildren}}
                            end;
                       (Leaf) ->
                            {true, Leaf}
                    end, ChildNodes).

prune_nodes(Parent, Children) ->
    {Parent, prune_children(Children)}.

prune_invalid_nodes_children(ChildNodes, InvalidNodes) when is_list(ChildNodes)->
    lists:filtermap(fun({Parent, Children}) ->
                            case keep_policy_node(Parent, InvalidNodes) of
                                false -> %% Prune subtree
                                    false;
                                {true, #{valid_policy := ?anyPolicy} = Parent} -> %% Keep Parent
                                    {true, {Parent,
                                            prune_invalid_nodes_children(Children, InvalidNodes)}};
                                {true, Parent} ->
                                    {true, {Parent, Children}} %% Keep subtree
                            end;
                       (#{} = Child) -> % Possibly prune leaf
                            keep_policy_node(Child, InvalidNodes)
                    end, ChildNodes).

handle_too_long_notice(Qualifier) ->
    %% RFC 3280 states that certificate users SHOULD gracefully handle
    %% explicitText with more than 200 characters.
    try public_key:der_decode('OTPUserNotice', Qualifier) of % Allow real value up to 350
        #'OTPUserNotice'{noticeRef = Ref,
                         explicitText = DispText} ->
            #'UserNotice'{noticeRef = Ref,
                          explicitText = DispText}
    catch error:_ -> %% Otherwhise return  gracefully default
            #'UserNotice'{noticeRef = asn1_NOVALUE,
                          explicitText = "User Notice much too long, so value is ignored"}
    end.
