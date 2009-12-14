-module(on_load_error).
-on_load(on_load/0).
-export([main/0]).

on_load() ->
    ?MASTER ! {?MODULE,self()},
    receive
	fail -> erlang:error(failed);
	Ret -> Ret
    end.

main() ->
    ok.
