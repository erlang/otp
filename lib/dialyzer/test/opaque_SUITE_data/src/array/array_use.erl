-module(array_use).

-export([ok1/0, wrong1/0, wrong2/0]).

ok1() ->
  array:set(17, gazonk, array:new()).

wrong1() ->
  {array, _, _, undefined, _} = array:new(42).

wrong2() ->
  case is_tuple(array:new(42)) of
    true -> structure_is_exposed;
    false -> cannot_possibly_be
  end.
