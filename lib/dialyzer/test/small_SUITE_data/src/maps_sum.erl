-module(maps_sum).
-export([correct1/1,
	 wrong1/1,
	 wrong2/1]).

-spec correct1(#{atom() => term()}) -> integer().

correct1(Data) ->
    maps:fold(fun (_Key, Value, Acc) when is_integer(Value) ->
                      Acc + Value;
                  (_Key, _Value, Acc) ->
                      Acc
              end, 0, Data).

-spec wrong1([{atom(),term()}]) -> integer().

wrong1(Data) ->
    maps:fold(fun (_Key, Value, Acc) when is_integer(Value) ->
                      Acc + Value;
                  (_Key, _Value, Acc) ->
                      Acc
              end, 0, Data).

-spec wrong2(#{atom() => term()}) -> integer().

wrong2(Data) ->
    lists:foldl(fun (_Key, Value, Acc) when is_integer(Value) ->
                      Acc + Value;
                  (_Key, _Value, Acc) ->
                      Acc
		end, 0, Data).
