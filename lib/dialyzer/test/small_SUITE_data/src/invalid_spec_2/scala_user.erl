-module(scala_user).

-export([is_list/2]).

-spec is_list(atom(), scala_data:data()) -> boolean().

is_list( List,Data) when   is_list(List) -> true;
is_list(Tuple,Data) when is_tuple(Tuple) -> false.
