-module(type_deps).

-export([func/1, get_num/0]).

-export_type([number_like/0, my_opaque/1, list_like/1]).

-type number_like() :: number().
-type list_like(X) :: [X].
-opaque my_opaque(X) :: {X,X}.

-callback quux(number()) -> number().

-spec func(T) -> T.
func(X) ->
  X + X.

get_num() ->
  3.
