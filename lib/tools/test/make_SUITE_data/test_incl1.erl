-module(test_incl1).
-compile(export_all).
-include("test_incl.hrl").

f1() ->
    ?d.

f2() ->
    true.
