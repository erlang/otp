-module(tc0).

-include_lib("eunit/include/eunit.hrl").

'c0_bad_output_test_'() ->
    [{integer_to_list(C), fun() -> io:format("'~c'", [C]) end}
     || C <- lists:seq(0, 31)].

'c0_bad_description_test_'() ->
    [{[C], fun() -> ok end}
     || C <- lists:seq(0, 31)].

'c0_bad_name__test'() ->
    ok.
