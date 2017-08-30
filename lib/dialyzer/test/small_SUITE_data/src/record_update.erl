-module(record_update).

-export([quux/2]).

-record(foo, {bar :: atom()}).

-spec quux(#foo{}, string()) -> #foo{}.

quux(Foo, NotBar) ->
  Foo#foo{ bar = NotBar }.
