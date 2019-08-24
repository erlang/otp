-module(remote_tuple_set).

-export([parse_cidr/0]).

-spec parse_cidr() -> {inet:address_family(),1,2} | {error}.

parse_cidr() ->
    {inet,1,2}.
