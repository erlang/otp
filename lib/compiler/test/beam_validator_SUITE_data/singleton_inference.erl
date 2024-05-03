-module(singleton_inference).
-export([test/0]).

test() ->
    {'EXIT',{{badmatch,true}, _}} =
        catch [0 || (X = (true or (X = is_port(node()))))],
    ok.
