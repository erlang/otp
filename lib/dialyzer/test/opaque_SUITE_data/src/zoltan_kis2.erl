-module(zoltan_kis2).

-export([get/2]).

-opaque data() :: gb_trees:tree().

-spec get(term(), data()) -> term().

get(Key, Data) ->
  %% Should unopaque data for remote calls
  case gb_trees:lookup(Key, Data) of
    'none' -> 'undefined';
    {'value', Val} -> Val
  end.
