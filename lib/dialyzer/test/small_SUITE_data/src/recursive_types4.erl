-module(recursive_types4).

-export([test/0]).

-record(tree,   {node         :: atom(),
		 kid = nil    :: 'nil' | tree()}).

-type tree() :: #tree{}.

-spec test() -> tree().

test() ->
  #tree{node = root, kid = #tree{}}.
