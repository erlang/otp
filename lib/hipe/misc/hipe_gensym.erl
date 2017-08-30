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
%%=======================================================================
%% File        : hipe_gensym.erl
%% Author      : Eric Johansson and Kostis Sagonas
%% Description : Generates unique symbols and fresh integer counts.
%%=======================================================================
%% $Id$
%%=======================================================================
%% Notes: Written while we were in Montreal, Canada for PPDP-2000 as an
%%        exercise in Principles and Practice of Declarative Programming!
%%=======================================================================

-module(hipe_gensym).

-export([%% init/0, new_var/0, new_label/0,
	 %% update_lblrange/1, update_vrange/1, var_range/0, label_range/0,
	 set_var/1, get_var/0, get_next_var/0,
	 set_label/1, get_label/0, get_next_label/0]).
-export([init/1, new_var/1, new_label/1,
	 update_vrange/2, update_lblrange/2, var_range/1, label_range/1,
	 set_var_range/3, set_label_range/3,
	 set_var/2, get_var/1, get_next_var/1,
	 set_label/2, get_label/1, get_next_label/1]).

%%-----------------------------------------------------------------------
%% Types of allowable entities to set global variables for
%%-----------------------------------------------------------------------

-type gvarname() :: 'icode' | 'rtl' | 'arm' | 'ppc' | 'sparc' | 'x86' | 'llvm'.

%%-----------------------------------------------------------------------

%% init() ->
%%   put(var_count, 0),
%%   put(label_count, 0),
%%   put(var_min, 0),
%%   put(var_max, 0),
%%   put(lbl_min, 1),
%%   put(lbl_max, 1),
%%   ok.

-spec init(gvarname()) -> 'ok'.

init(What) ->
  put({What,var_count}, 0),
  put({What,label_count}, 0),
  put({What,var_min}, 0),
  put({What,var_max}, 0),
  put({What,lbl_min}, 1),
  put({What,lbl_max}, 1),
  ok.

%% new_var() ->
%%   V = get(var_count),
%%   put(var_count, V+1),
%%   V.

-spec new_var(gvarname()) -> non_neg_integer().

new_var(What) ->
  T = {What, var_count},
  V = get(T),
  put(T, V+1),
  V.

%% new_label() ->
%%   L = get(label_count),
%%   put(label_count, L+1),
%%   L.

-spec new_label(gvarname()) -> non_neg_integer().

new_label(What) ->
  T = {What, label_count},
  L = get(T),
  put(T, L+1),
  L.

%% update_vrange(V) ->
%%   Vmax = get(var_max),
%%   Vmin = get(var_min),
%%   put(var_min, erlang:min(V, Vmin)),
%%   put(var_max, erlang:max(V, Vmax)),
%%   ok.

-spec update_vrange(gvarname(), non_neg_integer()) -> 'ok'.
update_vrange(What, V) ->
  Tmin = {What, var_min},
  Tmax = {What, var_max},
  Vmax = get(Tmax),
  Vmin = get(Tmin),
  put(Tmin, erlang:min(V, Vmin)),
  put(Tmax, erlang:max(V, Vmax)),
  ok.

%% update_lblrange(L) ->
%%   Lmax = get(lbl_max),
%%   Lmin = get(lbl_min),
%%   put(lbl_min, erlang:min(L, Lmin)),
%%   put(lbl_max, erlang:max(L, Lmax)),
%%   ok.

-spec update_lblrange(gvarname(), non_neg_integer()) -> 'ok'.

update_lblrange(What, L) ->
  Tmin = {What, lbl_min},
  Tmax = {What, lbl_max},
  Lmax = get(Tmax),
  Lmin = get(Tmin),
  put(Tmin, erlang:min(L, Lmin)),
  put(Tmax, erlang:max(L, Lmax)),
  ok.

%% var_range() ->
%%   {get(var_min), get(var_max)}.

-spec var_range(gvarname()) -> {non_neg_integer(), non_neg_integer()}.

var_range(What) ->
  {get({What,var_min}), get({What,var_max})}.

-spec set_var_range(gvarname(), non_neg_integer(), non_neg_integer()) -> 'ok'.

set_var_range(What, Min, Max) ->
  put({What,var_min}, Min),
  put({What,var_max}, Max),
  ok.

%% label_range() ->
%%   {get(lbl_min), get(lbl_max)}.

-spec label_range(gvarname()) -> {non_neg_integer(), non_neg_integer()}.

label_range(What) ->
  {get({What,lbl_min}), get({What,lbl_max})}.
 
-spec set_label_range(gvarname(), non_neg_integer(), non_neg_integer()) -> 'ok'.

set_label_range(What, Min, Max) ->
  put({What,lbl_min}, Min),
  put({What,lbl_max}, Max),
  ok.
  
%%-----------------------------------------------------------------------
%% Variable counter
%%-----------------------------------------------------------------------

-spec set_var(non_neg_integer()) -> 'ok'.

set_var(X) ->
  put(var_max, X),
  ok.

-spec set_var(gvarname(), non_neg_integer()) -> 'ok'.

set_var(What, X) ->
  put({What,var_max}, X),
  ok.

-spec get_var() -> non_neg_integer().

get_var() ->
  get(var_max).

-spec get_var(gvarname()) -> non_neg_integer().

get_var(What) ->
  get({What,var_max}).

-spec get_next_var() -> non_neg_integer().

get_next_var() ->
  C = get(var_max),
  put(var_max, C+1),
  C+1.

-spec get_next_var(gvarname()) -> non_neg_integer().

get_next_var(What) ->
  T = {What, var_max},
  C = get(T),
  put(T, C+1),
  C+1.

%%-----------------------------------------------------------------------
%% Label counter
%%-----------------------------------------------------------------------

-spec set_label(non_neg_integer()) -> 'ok'.

set_label(X) ->
  put(lbl_max, X),
  ok.

-spec set_label(gvarname(), non_neg_integer()) -> 'ok'.

set_label(What, X) ->
  put({What,lbl_max}, X),
  ok.

-spec get_label() -> non_neg_integer().

get_label() ->
  get(lbl_max).

-spec get_label(gvarname()) -> non_neg_integer().

get_label(What) ->
  get({What,lbl_max}).

-spec get_next_label() -> non_neg_integer().

get_next_label() ->
  C = get(lbl_max),
  put(lbl_max, C+1),
  C+1.

-spec get_next_label(gvarname()) -> non_neg_integer().

get_next_label(What) ->
  T = {What, lbl_max},
  C = get(T),
  put(T, C+1),
  C+1.

%%-----------------------------------------------------------------------
