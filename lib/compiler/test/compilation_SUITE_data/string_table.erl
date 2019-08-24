-module(string_table).
-export([f/1, g/1]).

f(<<"string">>) -> string;
f(<<"stringtable">>) -> stringtable.

g(<<"stringtable">>) -> stringtable;
g(<<"table">>) -> table.
