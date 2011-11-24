-module(nowarnunused).

-compile({nowarn_unused_function, return_error/2}).

-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).
