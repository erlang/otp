-module(valid).
-export([bar/0]).
-doc """
This is a 'quoted
extension' in a multiline
""".
bar() -> ok.
