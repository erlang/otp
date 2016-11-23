-module(exact_api).

-export([new/0, exact_api_test/1, exact_api_new/1,
         exact_adt_test/1, exact_adt_new/1]).

-export_type([exact_api/0]).

-record(digraph, {vtab = notable :: ets:tab(),
		  etab = notable :: ets:tab(),
		  ntab = notable :: ets:tab(),
	          cyclic = true  :: boolean()}).

-spec new() -> digraph:graph().

new() ->
    A = #digraph{},
    set_type(A), % does not have an opaque term as 1st argument
    A.

-spec set_type(digraph:graph()) -> true.

set_type(G) ->
    digraph:delete(G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% The derived spec of exact_api_new() is
%%% -spec exact_api_new(exact_api:exact_api()) -> exact_api:exact_api().
%%% This won't happen unless dialyzer_typesig uses
%%% t_is_exactly_equal() rather than t_is_equal().
%%% [As of R17B the latter considers two types equal if nothing but
%%%  their ?opaque tags differ.]

-record(exact_api, {}).

-opaque exact_api() :: #exact_api{}.

exact_api_test(X) ->
    #exact_api{} = exact_api_set_type(X). % OK

exact_api_new(A) ->
    A = #exact_api{},
    _ = exact_api_set_type(A), % OK (the opaque type is local)
    A.

-spec exact_api_set_type(exact_api()) -> exact_api().

exact_api_set_type(#exact_api{}=E) -> E.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(exact_adt, {}).

exact_adt_test(X) ->
    #exact_adt{} = exact_adt:exact_adt_set_type(X). % breaks the opacity

exact_adt_new(A) ->
    A = #exact_adt{},
    _ = exact_adt:exact_adt_set_type2(A), % does not have an opaque term as 1st argument
    A.
