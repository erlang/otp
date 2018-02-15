-module(same_type).

-export([t/2]).

-export_type([st/1]).

%% When unopaqued all specializations of st/1 are equal.
-opaque st(_A) :: {st, tuple()}.

-spec t(_, st(_)) -> _.

t(K, V) ->
    {K, V}.
