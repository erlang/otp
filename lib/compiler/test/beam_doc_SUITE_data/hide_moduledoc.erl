-module(hide_moduledoc).

-export([main/0]).

-moduledoc false.

-doc "
Doc test module
".
main() ->
    ok().

-doc #{since => "1.0"}.
ok() ->
    ok.
