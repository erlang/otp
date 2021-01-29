-module(module_func_wrong_arg_type_number).
-export([func/1]).

func(2) -> 3;
func(4) -> 6.
