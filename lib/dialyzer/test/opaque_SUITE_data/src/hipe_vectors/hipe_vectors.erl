%% -*- erlang-indent-level: 2 -*-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%			  VECTORS IN ERLANG
%%
%% Abstract interface to vectors, indexed from 0 to size-1.

-module(hipe_vectors).
-export([new/2,
	 set/3,
	 get/2,
	 size/1,
	 vector_to_list/1,
	 %% list_to_vector/1,
	 list/1]).

%%-define(USE_TUPLES, true).
%%-define(USE_GBTREES, true).
-define(USE_ARRAYS, true).

-type vector() :: vector(_).
-export_type([vector/0, vector/1]).

-spec new(non_neg_integer(), V) -> vector(E) when V :: E.
-spec set(vector(E), non_neg_integer(), V :: E) -> vector(E).
-spec get(vector(E), non_neg_integer()) -> E.
-spec size(vector(_)) -> non_neg_integer().
-spec vector_to_list(vector(E)) -> [E].
%% -spec list_to_vector([E]) -> vector(E).
-spec list(vector(E)) -> [{non_neg_integer(), E}].

%% ---------------------------------------------------------------------

-ifdef(USE_TUPLES).
-opaque vector(_) :: tuple().

new(N, V) ->
    erlang:make_tuple(N, V).

size(V) -> erlang:tuple_size(V).

list(Vec) ->
    index(tuple_to_list(Vec), 0).

index([X|Xs],N) ->
    [{N,X} | index(Xs,N+1)];
index([],_) ->
    [].

%% list_to_vector(Xs) ->
%%     list_to_tuple(Xs).

vector_to_list(V) ->
    tuple_to_list(V).

set(Vec, Ix, V) ->
    setelement(Ix+1, Vec, V).

get(Vec, Ix) -> element(Ix+1, Vec).

-endif. %% ifdef USE_TUPLES

%% ---------------------------------------------------------------------

-ifdef(USE_GBTREES).
-opaque vector(E) :: gb_trees:tree(non_neg_integer(), E).

new(N, V) when is_integer(N), N >= 0 ->
    gb_trees:from_orddict(mklist(N, V)).

mklist(N, V) ->
    mklist(0, N, V).

mklist(M, N, V) when M < N ->
    [{M, V} | mklist(M+1, N, V)];
mklist(_, _, _) ->
    [].

size(V) -> gb_trees:size(V).

list(Vec) ->
    gb_trees:to_list(Vec).

%% list_to_vector(Xs) ->
%%     gb_trees:from_orddict(index(Xs, 0)).
%% 
%% index([X|Xs], N) ->
%%     [{N, X} | index(Xs, N+1)];
%% index([],_) ->
%%     [].

vector_to_list(V) ->
    gb_trees:values(V).

set(Vec, Ix, V) ->
    gb_trees:update(Ix, V, Vec).

get(Vec, Ix) ->
    gb_trees:get(Ix, Vec).

-endif. %% ifdef USE_GBTREES

%% ---------------------------------------------------------------------

-ifdef(USE_ARRAYS).
-opaque vector(E) :: array:array(E).
%%-type vector(E) :: array:array(E). % Work around dialyzer bug

new(N, V) -> array:new(N, {default, V}).
size(V) -> array:size(V).
list(Vec) -> array:to_orddict(Vec).
%% list_to_vector(Xs) -> array:from_list(Xs).
vector_to_list(V) -> array:to_list(V).
set(Vec, Ix, V) -> array:set(Ix, V, Vec).
get(Vec, Ix) -> array:get(Ix, Vec).

-endif. %% ifdef USE_ARRAYS
