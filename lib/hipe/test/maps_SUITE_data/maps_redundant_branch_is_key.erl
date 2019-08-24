-module(maps_redundant_branch_is_key).
-export([test/0]).

test() ->
    ok = thingy(#{a => 1}),
    ok = thingy(#{a => 2}),
    ok.

thingy(Map) ->
    try
	#{a := _} = Map,
	ok
    catch _ -> error
    end.
