-module(bad).
-export([foo/0]).

-doc """
'foo is an atom
' does work
""".

foo() ->
    ok.
