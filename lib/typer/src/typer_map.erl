%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(typer_map).

-export([new/0, insert/2, lookup/2, from_list/1, remove/2, fold/3]).

-spec new() -> dict().
new() ->
  dict:new().

-spec insert({term(), term()}, dict()) -> dict().
insert(Object, Dict) ->
  {Key, Value} = Object,
  dict:store(Key, Value, Dict).

-spec lookup(term(), dict()) -> any().
lookup(Key, Dict) ->
  try dict:fetch(Key, Dict) catch error:_ -> none end.

-spec from_list([{term(), term()}]) -> dict().
from_list(List) ->
  dict:from_list(List).

-spec remove(term(), dict()) -> dict().
remove(Key, Dict) ->
  dict:erase(Key, Dict).

-spec fold(fun((term(), term(), term()) -> term()), term(), dict()) -> term().
fold(Fun, Acc0, Dict) -> 
  dict:fold(Fun, Acc0, Dict).
