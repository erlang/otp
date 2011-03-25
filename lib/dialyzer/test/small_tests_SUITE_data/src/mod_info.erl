-module(mod_info).
-export([test/0]).

test() ->
  {module_info(), module_info(compile)}.
