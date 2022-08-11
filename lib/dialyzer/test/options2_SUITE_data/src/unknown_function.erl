-module(unknown_function).

-export([
    unknown_function_on_unknown_module/0,
    unknown_function_on_known_module/0
  ]).

-spec unknown_function_on_known_module() -> ok.
unknown_function_on_known_module() ->
    unknown_function:function().

-spec unknown_function_on_unknown_module() -> ok.
unknown_function_on_unknown_module() ->
    unknown:function().
