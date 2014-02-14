-module(map_module).

-export([foo1/1,foo2/3]).

%% @type wazzup() = integer()
%% @type some_type() = map()
%% @type some_other_type() = {a, #{ list() => term()}}

-type some_type() :: map().
-type some_other_type() :: {'a', #{ list() => term()} }.
-type wazzup() :: integer().

-spec foo1(Map :: #{ 'a' => integer(), 'b' => term()}) -> term().

%% @doc Gets value from map.

foo1(#{ a:= 1, b := V}) -> V.

%% @spec foo2(some_type(), Type2 :: some_other_type(), map()) -> Value
%% @doc Gets value from map.

-spec foo2(
    Type1 :: some_type(),
    Type2 :: some_other_type(),
    Map :: #{ get => 'value', 'value' => binary()}) -> binary().

foo2(Type1, {a,#{ "a" := _}}, #{get := value, value := B}) when is_map(Type1) -> B.
