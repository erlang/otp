-module(singleton_inference).
-export([test/0]).

test() ->
    {'EXIT',{{badmatch,true}, _}} =
        catch case X = (true or (X = is_port(node()))) of
                  true -> 1;
                  false -> 0
              end,
    ok.
