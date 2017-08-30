-module(subtract_value_flip).

-export([t1/1]).

t1(#{type := _Smth} = Map) ->
    case Map of
	#{type := a} -> ok;
	#{type := b} -> error
    end.
