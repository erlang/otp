-module(call_to_missing_example).
-export([call_to_missing_test/0]).

call_to_missing_test() ->
    previously_defined:function().
