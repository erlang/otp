-module(no_doc_attributes).

-type yes() :: integer().
-type no() :: atom().

-export([foo/1, baz/1]).

-record(name, {field = undefined :: atom()}).

-callback me(X :: yes()) -> no();
            (no()) -> yes();
            (term()) -> #name{ }.

-spec foo(yes()) -> {yes(), yes()} | yes();
              (no()) -> no().
foo(X) ->
    _ = #name{field = none},
    X.


-spec baz(Z) -> Z when Z :: yes();
         (no()) -> no().
baz(X) ->
    X.
