-module(a).
-export([to_map/1, to_map/2]).
-type t() :: #{type := b:t()}.

-spec to_map(t()) -> map().
to_map(Resource) -> to_map(Resource, #{}).

-spec to_map(t(), map()) -> map().
to_map(_, Map) when is_map(Map) -> #{}.
