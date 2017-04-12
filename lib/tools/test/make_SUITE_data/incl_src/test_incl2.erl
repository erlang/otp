-module(test_incl2).
-compile(export_all).
-include("test_incl.hrl").

f1() ->
    ?d.

f2() ->
    true.
