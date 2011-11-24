-module(recursive_types5).

-export([test/0]).

-type tree() :: 'nil' | {non_neg_integer(), tree(), tree()}.

-record(tree,   {node         :: atom(),
		 kid = 'nil'  :: tree()}).

-spec test() -> #tree{}.

test() ->
  #tree{node = root, kid = {42, {42, nil, nil}, {42, nil, nil}}}.
