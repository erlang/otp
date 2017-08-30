-module(weird_warning1).
-export([public_func/0]).

-record(a, {
          d = dict:new() :: dict:dict()
         }).

-record(b, {
          q = queue:new() :: queue:queue()
         }).

public_func() ->
    add_element(#b{}, my_key, my_value).

add_element(#a{d = Dict}, Key, Value) ->
    dict:store(Key, Value, Dict);
add_element(#b{q = Queue}, Key, Value) ->
    queue:in({Key, Value}, Queue).
