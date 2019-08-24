-module(opaque_bif).
-export([o1/1]).
-export_type([opaque_any_map/0]).
-opaque opaque_any_map() :: map().

%% ERL-249: A bug with opaque arguments to maps:merge/2
%% Reported by Felipe Ripoll on 6/9/2016
-spec o1(opaque_any_map()) -> opaque_any_map().
o1(Map) ->
    maps:merge(o1_c(), Map).

-spec o1_c() -> opaque_any_map().
o1_c() -> #{}.
