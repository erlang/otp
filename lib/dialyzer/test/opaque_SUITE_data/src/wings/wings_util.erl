%%
%%  wings_util.erl --
%%
%%     Various utility functions that not obviously fit somewhere else.
%%

-module(wings_util).

-export([gb_trees_smallest_key/1, gb_trees_largest_key/1,
	 gb_trees_map/2, rel2fam/1]).

-include("wings.hrl").

rel2fam(Rel) ->
    sofs:to_external(sofs:relation_to_family(sofs:relation(Rel))).

%% a definition that does not violate the opacity of gb_trees:tree()
gb_trees_smallest_key(Tree) ->
    {Key, _V} = gb_trees:smallest(Tree),
    Key.

%% a definition that violates the opacity of gb_trees:tree()
gb_trees_largest_key({_, Tree}) ->
    largest_key1(Tree).

largest_key1({Key, _Value, _Smaller, nil}) ->
    Key;
largest_key1({_Key, _Value, _Smaller, Larger}) ->
    largest_key1(Larger).

gb_trees_map(F, {Size,Tree}) ->
    {Size,gb_trees_map_1(F, Tree)}.

gb_trees_map_1(_, nil) -> nil;
gb_trees_map_1(F, {K,V,Smaller,Larger}) ->
    {K,F(K, V),
     gb_trees_map_1(F, Smaller),
     gb_trees_map_1(F, Larger)}.
