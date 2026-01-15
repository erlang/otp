-module(singleton_docformat).

-export([main/0]).

-moduledoc #{format => <<"text/asciidoc">>,
             since => ~"1.0",
             deprecated => ~"Use something else"}.
-moduledoc "
Moduledoc test module
".


-doc #{ authors => [<<"Beep Bop">>] }.
-doc #{ equiv => main/3 }.
-doc "
Doc test module

More info here
".
main() ->
    ok.
