-module(recursive_types7).

-export([test/0]).

-type tree() :: 'nil' | {non_neg_integer(), recursive_types7:tree(),
                                            recursive_types7:tree()}.

-export_type([tree/0]).

-spec test() -> {42, tree(), tree()}.

test() ->
  {42, {42, nil, nil}, {42, {42, nil, nil}, {42, nil, nil}}}.
