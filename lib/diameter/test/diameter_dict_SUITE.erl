%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%
%% Tests of the dict-like diameter_dict.
%%

-module(diameter_dict_SUITE).

-export([suite/0,
         all/0,
         groups/0]).

%% testcases
-export([append/1,
         fetch/1,
         fetch_keys/1,
         filter/1,
         find/1,
         fold/1,
         is_key/1,
         map/1,
         merge/1,
         update/1,
         update_counter/1]).

-include("diameter_ct.hrl").

-define(dict, diameter_dict).
-define(util, diameter_util).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [{group, all},
     {group, all, [parallel]}].

groups() ->
    [{all, [], tc()}].

tc() ->
    [append,
     fetch,
     fetch_keys,
     filter,
     find,
     fold,
     is_key,
     map,
     merge,
     update,
     update_counter].

%% ===========================================================================

-define(KV100, [{N,[N]} || N <- lists:seq(1,100)]).

append(_) ->
    D = ?dict:append(k, v, ?dict:new()),
    [{k,[v,v]}] = ?dict:to_list(?dict:append(k, v, D)).

fetch(_) ->
    D = ?dict:from_list(?KV100),
    [50] = ?dict:fetch(50, D),
    Ref = make_ref(),
    Ref = try ?dict:fetch(Ref, D) catch _:_ -> Ref end.

fetch_keys(_) ->
    L = ?KV100,
    D = ?dict:from_list(L),
    L = [{N,[N]} || N <- lists:sort(?dict:fetch_keys(D))].

filter(_) ->
    L = ?KV100,
    F = fun(K,[_]) -> 0 == K rem 2 end,
    D = ?dict:filter(F, ?dict:from_list(L)),
    true = [T || {K,V} = T <- L, F(K,V)] == lists:sort(?dict:to_list(D)).

find(_) ->
    D = ?dict:from_list(?KV100),
    {ok, [50]} = ?dict:find(50, D),
    error = ?dict:find(make_ref(), D).

fold(_) ->
    L = ?KV100,
    S = lists:sum([N || {N,_} <- L]),
    S = ?dict:fold(fun(K,[_],A) -> K + A end, 0, ?dict:from_list(L)).

is_key(_) ->
    L = ?KV100,
    D = ?dict:from_list(L),
    true = lists:all(fun({N,_}) -> ?dict:is_key(N,D) end, L),
    false = ?dict:is_key(make_ref(), D).

map(_) ->
    L = ?KV100,
    F = fun(_,V) -> [N] = V, N*2 end,
    D = ?dict:map(F, ?dict:from_list(L)),
    M = [{K, F(K,V)} || {K,V} <- L],
    M = lists:sort(?dict:to_list(D)).

merge(_) ->
    L = ?KV100,
    F = fun(_,V1,V2) -> V1 ++ V2 end,
    D = ?dict:merge(F, ?dict:from_list(L), ?dict:from_list(L)),
    M = [{K, F(K,V,V)} || {K,V} <- L],
    M = lists:sort(?dict:to_list(D)).

update(_) ->
    L = ?KV100,
    F = fun([V]) -> 2*V end,
    D = ?dict:update(50, F, ?dict:from_list(L)),
    100 = ?dict:fetch(50, D),
    Ref = make_ref(),
    Ref = try ?dict:update(Ref, F, D) catch _:_ -> Ref end,
    [Ref] = ?dict:fetch(Ref, ?dict:update(Ref,
                                          fun(_,_) -> ?ERROR(i_think_not) end,
                                          [Ref],
                                          D)).

update_counter(_) ->
    L = [{N,2*N} || {N,_} <- ?KV100],
    D = ?dict:update_counter(50, 20, ?dict:from_list(L)),
    120 = ?dict:fetch(50,D),
    2 = ?dict:fetch(1,D).
