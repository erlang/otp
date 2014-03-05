-module(predef2).

-export([array/1, dict/1, digraph/1, digraph2/1, gb_set/1, gb_tree/1,
         queue/1, set/1, tid/0, tid2/0]).

-export_type([array/0, digraph/0, gb_set/0]).

-spec array(array()) -> array:array().

array(A) ->
    array:relax(A).

-spec dict(dict()) -> dict:dict().

dict(D) ->
    dict:store(1, a, D).

-spec digraph(digraph()) -> [digraph:edge()].

digraph(G) ->
    digraph:edges(G).

-spec digraph2(digraph:graph()) -> [digraph:edge()].

digraph2(G) ->
    digraph:edges(G).

-spec gb_set(gb_set()) -> gb_sets:set().

gb_set(S) ->
    gb_sets:balance(S).

-spec gb_tree(gb_tree()) -> gb_trees:tree().

gb_tree(S) ->
    gb_trees:balance(S).

-spec queue(queue()) -> queue:queue().

queue(Q) ->
    queue:reverse(Q).

-spec set(set()) -> sets:set().

set(S) ->
    sets:union([S]).

-spec tid() -> tid().

tid() ->
    ets:new(tid, []).

-spec tid2() -> ets:tid().

tid2() ->
    ets:new(tid, []).
