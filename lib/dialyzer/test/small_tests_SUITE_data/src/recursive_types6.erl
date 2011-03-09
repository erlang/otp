-module(recursive_types6).

-export([test/0]).

-record(tree,   {node      :: non_neg_integer(),
		 kid = nil :: child()}).

-type tree() :: #tree{}.

-record(child,  {tree      :: 'nil' | tree()}).

-type child() :: #child{}.

-spec test() -> tree().

test() ->
  #tree{node = 42, kid = #child{tree = #tree{node = 42, kid = #child{tree = nil}}}}.
