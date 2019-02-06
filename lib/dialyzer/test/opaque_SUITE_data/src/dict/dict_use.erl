-module(dict_use).

-export([ok1/0, ok2/0, ok3/0, ok4/0, ok5/0, ok6/0]).
-export([middle/0]).
-export([w1/0, w2/0, w3/0, w4/1, w5/0, w6/0, w7/0, w8/1, w9/0]).

-define(DICT, dict).

%%---------------------------------------------------------------------
%% Cases that are OK
%%---------------------------------------------------------------------

ok1() ->
  dict:new().

ok2() ->
  case dict:new() of X -> X end.

ok3() ->
  Dict1 = dict:new(),
  Dict2 = dict:new(),
  Dict1 =:= Dict2.

ok4() ->
  dict:fetch(foo, dict:new()).

ok5() ->  % this is OK since some_mod:new/0 might be returning a dict:dict()
  dict:fetch(foo, some_mod:new()).

ok6() ->
  dict:store(42, elli, dict:new()).

middle() ->
  {w1(), w2()}.

%%---------------------------------------------------------------------
%% Cases that are problematic w.r.t. opacity of types
%%---------------------------------------------------------------------

w1() ->
  gazonk = dict:new().

w2() ->
  case dict:new() of
    [] -> nil;
    42 -> weird
  end.

w3() ->
  try dict:new() of
    [] -> nil;
    42 -> weird
  catch
    _:_ -> exception
  end.

w4(Dict) when is_list(Dict) ->
  Dict =:= dict:new();
w4(Dict) when is_atom(Dict) ->
  Dict =/= dict:new().

w5() ->
  case dict:new() of
    D when length(D) =/= 42 -> weird;
    D when is_atom(D) -> weirder;
    D when is_list(D) -> gazonk
  end.

w6() ->
  is_list(dict:new()).

w7() ->
  dict:fetch(foo, [1,2,3]).

w8(Fun) ->
  dict:merge(Fun, 42, [1,2]).

w9() ->
  dict:store(42, elli,
	     {dict,0,16,16,8,80,48,
	           {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
	           {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}).
