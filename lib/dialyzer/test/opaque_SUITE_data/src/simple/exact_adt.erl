-module(exact_adt).

-export([exact_adt_set_type/1, exact_adt_set_type2/1]).

-export_type([exact_adt/0]).

-record(exact_adt, {}).

-opaque exact_adt() :: #exact_adt{}.

-spec exact_adt_set_type(_) -> exact_adt().

exact_adt_set_type(G) -> G.

-spec exact_adt_set_type2(exact_adt()) -> exact_adt().

exact_adt_set_type2(G) -> G.
