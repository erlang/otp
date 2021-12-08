-module(unknown_function_example).
-export([unknown_function_test/0]).

unknown_function_test() ->
    does_not_exist:function().
