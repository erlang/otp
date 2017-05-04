-module(a).
-type t() :: #{type := b:t()}.
-export([to_map/1, to_map/2]).

-spec to_map(t()) -> map().
to_map(Resource) -> to_map(Resource, #{}).

-spec to_map(t(), map()) -> map().
to_map(_, Map) when is_map(Map) -> #{}.
