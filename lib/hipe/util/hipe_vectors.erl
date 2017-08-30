%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

-include("hipe_vectors.hrl").

%% ---------------------------------------------------------------------

-ifdef(USE_TUPLES).

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

-spec new(non_neg_integer(), _) -> hipe_vector().
new(N, V) when is_integer(N), N >= 0 ->
    gb_trees:from_orddict(mklist(N, V)).

mklist(N, V) ->
    mklist(0, N, V).

mklist(M, N, V) when M < N ->
    [{M, V} | mklist(M+1, N, V)];
mklist(_, _, _) ->
    [].

-spec size(hipe_vector()) -> non_neg_integer().
size(V) -> gb_trees:size(V).

-spec list(hipe_vector()) -> [{_, _}].
list(Vec) ->
    gb_trees:to_list(Vec).

%% -spec list_to_vector([_]) -> hipe_vector().
%% list_to_vector(Xs) ->
%%     gb_trees:from_orddict(index(Xs, 0)).
%% 
%% index([X|Xs], N) ->
%%     [{N, X} | index(Xs, N+1)];
%% index([],_) ->
%%     [].

-spec vector_to_list(hipe_vector()) -> [_].
vector_to_list(V) ->
    gb_trees:values(V).

-spec set(hipe_vector(), non_neg_integer(), _) -> hipe_vector().
set(Vec, Ix, V) ->
    gb_trees:update(Ix, V, Vec).

-spec get(hipe_vector(), non_neg_integer()) -> any().
get(Vec, Ix) ->
    gb_trees:get(Ix, Vec).

-endif. %% ifdef USE_GBTREES
