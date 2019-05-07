-module(fun_app_args).

-export([t/1]).

-type ft() :: fun((a, []) -> any()).

-record(r, {
    h = c :: c | ft()
}).

t(#r{h = H}) ->
    fun(_) -> (H)(b, []) end.
