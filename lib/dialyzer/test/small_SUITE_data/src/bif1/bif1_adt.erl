-module(bif1_adt).

-export([opaque_string/0]).

-export_type([s/0]).

-opaque s() :: string().

-spec opaque_string() -> s().

opaque_string() ->
    "string".
