-module(recursive_types2).

-export([test/0]).

-type tree() :: 'nil' | {non_neg_integer(), subtree(), subtree()}.

-type subtree() :: tree().

-spec test() -> {42, tree(), tree()}.

test() ->
  {42, {42, nil, nil}, {42, {42, nil, nil}, {42, nil, nil}}}.
