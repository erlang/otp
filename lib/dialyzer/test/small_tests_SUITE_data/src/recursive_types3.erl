-module(recursive_types3).

-export([test/1]).

-record(tree,   {node         :: atom(),
		 kid = nil    :: 'nil' | tree()}).

-type tree() :: #tree{}.

-spec test(tree()) -> tree().

test(Tree) ->
  case Tree of
    #tree{node = root, kid=#tree{}} -> Tree
  end.
