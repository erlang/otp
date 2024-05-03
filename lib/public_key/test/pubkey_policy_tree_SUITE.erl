-module(pubkey_policy_tree_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(PRE_SCRIPT, "<pre class=\"mermaid\">~n").
-define(POST_SCRIPT, "~n</pre><script type=\"module\">import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';</script>").

%% -define(DEBUG, true).

-ifdef(DEBUG).
-define(FWR(Fmt, Args),
        begin
            io:fwrite(user, "dbg: " ++ Fmt, Args)
        end).
-else.
-define(FWR(Fmt, Args), ok).
-endif.

-define(PAL_MMD(MMD),
       begin
           %% ?FWR("~s~n", [MMD]),
           ct:log(MMD),
           ?FWR("~n", [])
       end).
-define(ROOT_TN, pubkey_policy_tree:root()).
-define(EMPTY_VPT, {}).
-define(ROOT_PN,
        begin
            {AnyPolicyNode, _} = ?ROOT_TN,
            AnyPolicyNode
        end).
-define(PN(VP), pubkey_policy_tree:policy_node(VP, [], [])).
-define(PN(VP, EPS), pubkey_policy_tree:policy_node(VP, [], EPS)).

%% Note: trees used for testing below might not make much sense from
%% RFC5280 perspective and can be improved for sure; but can be good
%% enough for checking some general tree handling code

all() -> [add_leaves,
          add_leaf_siblings,
          all_leaves,
          prune_leaves,
          prune_tree_and_leaves,
          constrained_policy_node_set,
          valid_policy_node_set,
          any_leaves,
          prune_tree_shorter_branch,
          prune_invalid_nodes].

any_leaves(_Config) ->
    AL =
        fun(Tree) ->
                pubkey_policy_tree:any_leaves(Tree)
        end,
    ?assertEqual([], AL(?EMPTY_VPT)),
    log_tree_diagram("tree_with_any_policy_leaf()", tree_with_any_policy_leaf()),
    ?assertEqual([?PN(?anyPolicy)], AL(tree_with_any_policy_leaf())),
    log_tree_diagram("tree_with_any_policy_node1()", tree_with_any_policy_node1()),
    ?assertEqual([], AL(tree_with_any_policy_node1())),
    ok.

constrained_policy_node_set(_Config) ->
    CS =
        fun(Tree) ->
                pubkey_policy_tree:constrained_policy_node_set(Tree)
        end,
    ?assertEqual([], CS(?EMPTY_VPT)),
    ?assertEqual([?PN(?anyPolicy)], CS(tree_with_any_policy_leaf())),
    ?assertEqual([?PN("SILVER", ["A"]), ?PN("GOLD")], CS(tree_with_any_policy_node1())),
    ?assertEqual([?PN("GOLD", ["GOLD", "SILVER"])], CS(tree_with_any_policy_node2())),
    ok.

valid_policy_node_set(_Config) ->
    VS =
        fun(Tree) ->
                pubkey_policy_tree:valid_policy_node_set(Tree)
        end,
    ?assertEqual([], VS(?EMPTY_VPT)),
    log_tree_diagram("tree_with_any_policy_leaf()", tree_with_any_policy_leaf()),
    ?assertEqual([?PN("GOLD")], VS(tree_with_any_policy_leaf())),
    log_tree_diagram("tree_with_any_policy_node1()", tree_with_any_policy_node1()),
    ?assertEqual([?PN(?anyPolicy), ?PN("SILVER", ["A"]), ?PN("GOLD")],
                 VS(tree_with_any_policy_node1())),
    log_tree_diagram("tree_with_any_policy_node2()", tree_with_any_policy_node2()),
    ?assertEqual([?PN(?anyPolicy), ?PN("GOLD", ["GOLD", "SILVER"])],
                 VS(tree_with_any_policy_node2())),
    ok.

prune_invalid_nodes(_Config) ->
    Children0 = [{?PN("GOLD"), [?PN("GOLD")]},
                 {?PN("BLUE"), []}, % policy_tree_node type terminating branch - pruning target
                 ?PN("SILVER")], % leaf of policy_node type
    Tree0 = {?ROOT_PN, Children0 ++
                 [{?PN("GOLD"), [?ROOT_PN]}]},
    Tree0 = pubkey_policy_tree:prune_invalid_nodes(Tree0, []),
    {ok, Tree1} = explain(Tree0,
                          [{prune_invalid_nodes, [[?PN("SILVER")]]},
                           {prune_invalid_nodes, [[?PN("BLUE")]]},
                           {prune_invalid_nodes, [[?PN("GOLD")]]}]),
    ?EMPTY_VPT = Tree1,
    %% prune node
    Tree2 = {?ROOT_PN, Children0 ++
                 [{?PN(?anyPolicy), [?ROOT_PN]}]},
    {ok, Tree3} = explain(Tree2,
                          [{prune_invalid_nodes, [[?PN("SILVER")]]},
                           {prune_invalid_nodes, [[?PN("BLUE")]]},
                           {prune_invalid_nodes, [[?PN("GOLD")]]},
                           {prune_invalid_nodes, [[?PN(?anyPolicy)]]},
                           {prune_invalid_nodes, [[?ROOT_PN]]}]),
    Expected = {?ROOT_PN, [{?PN(?anyPolicy),
                            [?ROOT_PN]}]},
    ?assertEqual(Expected, Tree3),
    ok.

prune_leaves(_Config) ->
    NullVPT = pubkey_policy_tree:empty(),
    ?assertEqual(NullVPT, pubkey_policy_tree:prune_leaves(NullVPT, null)),
    Tree0 = {?ROOT_PN,
             [{?PN("GOLD"), [?PN("GOLD"), ?PN("GOLD2")]},
              {?PN("GOLD"), [?PN(?anyPolicy), ?PN(?anyPolicy)]}]},
    {ok, Tree} = explain(Tree0, [{prune_leaves, ["GOLD"]},
                                  {prune_leaves, [?anyPolicy]}]),
    ?assertEqual({?ROOT_PN,
             [{?PN("GOLD"), [?PN("GOLD2")]},
              {?PN("GOLD"), []}]}, Tree),
    ok.

prune_tree_and_leaves(_Config) ->
    Tree0 = {?ROOT_PN,
             [{?PN("SILVER", ["SILVER"]),
               []},
              {?PN("GOLD", ["GOLD"]),
               [?PN("GOLD", ["GOLD"])]}]},
    Instructions = [{prune_tree, []},
                    {prune_leaves, ["GOLD"]}],
    {ok, Tree} = explain(Tree0, Instructions),
    Expected = {?ROOT_PN,
                [{?PN("GOLD", ["GOLD"]), []}]},
    Expected = Tree,
    ok.

prune_tree_shorter_branch(_Config) ->
    %% shorter branch ending with leaf is not expected to be pruned
    Tree0 = {?ROOT_PN,
          [{?PN("GOLD"), [?PN("GOLD")]},
           ?PN("SILVER")]},
    Instructions = [{prune_tree, []}],
    {ok, Tree0} = explain(Tree0, Instructions),
    ok.

all_leaves(_Config) ->
    Tree1 = {?ROOT_PN,
             [{?PN("GOLD"),
               [{?PN("GOLD"), []},
                {?PN("SILVER"), [?PN("GOLD"), ?PN("SILVER")]}]},
              {?PN("SILVER"),
               [{?PN("GOLD"), []},
                {?PN("SILVER"), [?PN("GOLD"), ?PN("SILVER")]}]}]},
    log_tree_diagram("Tree1", Tree1),
    ?assertEqual([?PN("GOLD"), ?PN("SILVER"), ?PN("GOLD"), ?PN("SILVER")],
                 pubkey_policy_tree:all_leaves(Tree1)),
    ok.

add_leaves(_Config) ->
    RootTree = pubkey_policy_tree:root(),
    AddLeavesFun1 =
        fun(_) -> [?PN("GOLD"), ?PN("SILVER")] end,
    AddLeavesFun2 =
        fun(#{valid_policy := "SILVER"}) ->
                [?PN("GOLD"), ?PN("SILVER")];
           (_) ->
                []
        end,
    Instructions = [{add_leaves, [AddLeavesFun1]},
                    {add_leaves, [AddLeavesFun1]},
                    {add_leaves, [AddLeavesFun2]}],
    {ok, Tree} = explain(RootTree, Instructions),
    ?assertEqual({?ROOT_PN,
                  [{?PN("GOLD"),
                    [{?PN("GOLD"), []},
                     {?PN("SILVER"), [?PN("GOLD"), ?PN("SILVER")]}]},
                   {?PN("SILVER"),
                    [{?PN("GOLD"), []},
                     {?PN("SILVER"), [?PN("GOLD"), ?PN("SILVER")]}]}]},
                 Tree),
    ok.

add_leaf_siblings(_Config) ->
    AddLeavesFun1 =
        fun(_) -> [?PN("GOLD"), ?PN("SILVER")] end,
    AddLeavesFun2 =
        fun(#{valid_policy := ?anyPolicy}) ->
                [?PN("PINK")];
           (#{valid_policy := "SILVER"}) ->
                [?PN("PURPLE")];
           (_) ->
                []
        end,
    Instructions = [{add_leaf_siblings, [AddLeavesFun1]},
                    {add_leaf_siblings, [AddLeavesFun1]},
                    {add_leaf_siblings, [AddLeavesFun2]}
                   ],
    {ok, Tree} = explain(tree_with_any_policy_node1(), Instructions),
    ?assertEqual({?ROOT_PN,
                  [{?PN(?anyPolicy),
                    [?PN("GOLD"), ?PN("GOLD"), ?PN("SILVER"), ?PN("GOLD"), ?PN("SILVER"), ?PN("PINK")]},
                   {?PN("SILVER", ["A"]),
                    [?PN("SILVER", ["B"]), ?PN("GOLD"), ?PN("SILVER"), ?PN("GOLD"), ?PN("SILVER"), ?PN("PURPLE")]}]},
                 Tree),
    ok.

%%--------------------------------------------------------------------
%% Internal API
%%--------------------------------------------------------------------
explain(InitTree, Instructions) ->
    ct:log("=============================================~nSTEP: 0)"),
    ct:log("Instructions=~n~p", [Instructions]),
    ?PAL_MMD(to_mmd("0) Initial tree", InitTree)),
    explain(InitTree, Instructions, 1).

explain(Tree, [], _) ->
    {ok, Tree};
explain(Tree0, [{FunctionName, Args} | Rest], N) ->
    Title = io_lib:format("~p) pubkey_policy_tree:~p()", [N, FunctionName]),
    ct:log("=============================================~nSTEP: ~s", [Title]),
    Tree = apply(pubkey_policy_tree, FunctionName, [Tree0 | Args]),
    ?PAL_MMD(to_mmd(Title, Tree)),
    explain(Tree, Rest, N+1).

log_tree_diagram(Title, Tree) ->
    ?PAL_MMD(to_mmd(Title, Tree)),
    ok.

to_mmd(Title, Tree) ->
    ct:log("Diagram input tree~n~p", [Tree]),
    {NodeInfo, Arrows} = traverse(Tree),
    ?PRE_SCRIPT ++
        "---~ntitle: " ++ Title ++ "~n---~n" ++
        "flowchart TD~n" ++
         NodeInfo ++ "~n" ++ Arrows ++ "~n"
        ?POST_SCRIPT.

traverse(TreeNode = {_PolicyNode, _NoChildren = []}) ->
    Path = [1],
    NodeId = nid(Path),
    ?FWR("Node (no children): ~p~n", [lists:reverse(Path)]),
    {"", io_lib:format("~s~s~n", [NodeId, node_desc(NodeId, TreeNode)])};
traverse(TreeNode = {}) ->
    Path = [1],
    NodeId = nid(Path),
    ?FWR("NULL tree: ~p~n", [lists:reverse(Path)]),
    {"", io_lib:format("~s~s~n", [NodeId, node_desc(NodeId, TreeNode)])};
traverse(Tree) ->
    traverse(Tree, [1], {#{}, []}).

traverse(#{}, _Path, Acc) ->
    ?FWR("Node (leaf found): ~p~n", [lists:reverse(_Path)]),
    Acc;
traverse({_Node, _NoChildren = []}, _Path = [1], _Acc = {NodeInfo0, Arrows}) ->
    %% traverse end
    ?FWR("END Node (no children): ~p~n", [lists:reverse(_Path)]),
    NodeInfo = maps:fold(fun(K, V, Acc) ->
                                 Acc ++ K ++ V ++ "~n"
                         end,
                         "", NodeInfo0),
    {NodeInfo, Arrows};
traverse({_Node, _NoChildren = []}, _Path, Acc) ->
    ?FWR("Node (no children): ~p~n", [lists:reverse(_Path)]),
    Acc;
traverse(TreeNode = {PolicyNode, Children = [Child | Rest]}, Path, _Acc = {NodeInfo0, Arrows0}) ->
    NodeId = nid(Path),
    ChildId = nid([length(Children) | Path]),
    Fmt = "~s --> ~s~n",
    Args = [NodeId, ChildId],
    Arrows1 = Arrows0 ++ io_lib:format(Fmt, Args),
    NodeInfo = store_new_nodes([{NodeId, TreeNode}, {ChildId, Child}], NodeInfo0),
    Acc2 = traverse(Child, [length(Children) | Path], {NodeInfo, Arrows1}), % traverse vertically
    traverse({PolicyNode, Rest}, Path, Acc2). % traverse horizontally

store_new_nodes([], NodeInfo) ->
    NodeInfo;
store_new_nodes([{Id, Node} | Rest] , NodeInfo0) ->
    NodeInfo = case maps:is_key(Id, NodeInfo0) of
                   true ->
                       NodeInfo0;
                   false ->
                       NodeDesc = node_desc(Id, Node),
                       NodeInfo0#{Id => NodeDesc}
               end,
    store_new_nodes(Rest, NodeInfo).

node_desc(Path, {}) ->
    io_lib:format("[\"(NULL) ~s\"]", [Path]);
node_desc(Path, #{valid_policy := VP, expected_policy_set := EPS}) ->
    io_lib:format("[\"(L) ~s\nvp: ~s\neps: ~s\"]", [Path, p(VP), ps(EPS)]);
node_desc(Path, {#{valid_policy := VP, expected_policy_set := EPS}, _}) ->
    io_lib:format("[\"(N) ~s\nvp: ~s\neps: ~s\"]", [Path, p(VP), ps(EPS)]).

nid(Path) ->
    nid(Path, "").

nid([], Acc) ->
    "N_" ++ lists:reverse(Acc);
nid([Node], Acc) ->
    nid([], Acc ++ integer_to_list(Node));
nid([Node | Rest], Acc) ->
    nid(Rest, Acc ++ integer_to_list(Node) ++ "-").

ps(ExpectedPolicySet) ->
    [io_lib:format("~s ", [p(P)]) || P <- ExpectedPolicySet].

p(?anyPolicy) ->
    "anyPolicy";
p(P) ->
    P.

tree_with_any_policy_leaf() ->
    {?ROOT_PN,
     [{?PN("GOLD"),
       [?PN(?anyPolicy)]}]}.

tree_with_any_policy_node1() ->
    {?ROOT_PN,
     [{?PN(?anyPolicy),
       [?PN("GOLD")]},
      {?PN("SILVER", ["A"]),
       [?PN("SILVER", ["B"])]}]}.

tree_with_any_policy_node2() ->
    {?ROOT_PN,
     [{?PN(?anyPolicy),
       [{?PN("GOLD", ["GOLD", "SILVER"]),
         [?PN("SILVER")]}]}]}.
