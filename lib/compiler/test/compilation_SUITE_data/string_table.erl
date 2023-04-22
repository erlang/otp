-module(string_table).
-export([f/1, g/1]).

f(<<"abcdefghi">>) -> string;
f(<<"abcdefghiABCDEFGHI">>) -> stringtable.

g(<<"abcdefghiABCDEFGHI">>) -> stringtable;
g(<<"ABCDEFGHI">>) -> table.
