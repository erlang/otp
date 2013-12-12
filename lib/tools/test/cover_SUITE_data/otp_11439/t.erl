-module(t).
-export([exported/0]).
-include("cover_inc.hrl").
-ifdef(BOOL).
macro() ->
    ?MACRO.
-endif.
exported() ->
    ok.
nonexported() ->
    ok.
