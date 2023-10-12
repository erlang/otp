-module(ttimesout).

-include_lib("eunit/include/eunit.hrl").

times_out_test_() ->
    {timeout, 1, fun() -> timer:sleep(20_000) end}.
