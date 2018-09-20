%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-module(counters_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [basic, bad, limits].

basic(Config) when is_list(Config) ->
    Ref = counters:new(10,[]),
    0 = counters:get(Ref, 7),
    ok = counters:put(Ref, 7, 3),
    ok = counters:add(Ref, 7, 14),
    17 = counters:get(Ref, 7),
    20 = counters:add_get(Ref, 7, 3),
    -3 = counters:add_get(Ref, 7, -23),
    17 = counters:add_get(Ref, 7, 20),
    ok = counters:sub(Ref, 7, 4),
    13 = counters:get(Ref, 7),
    -7 = counters:sub_get(Ref, 7, 20),
    3 = counters:sub_get(Ref, 7, -10),
    ok.

bad(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch counters:new(0,[])),
    {'EXIT',{badarg,_}} = (catch counters:new(10,[bad])),
    Ref = counters:new(10,[]),
    {'EXIT',{badarg,_}} = (catch counters:get(1742, 7)),
    {'EXIT',{badarg,_}} = (catch counters:get(make_ref(), 7)),
    {'EXIT',{badarg,_}} = (catch counters:get(Ref, -1)),
    {'EXIT',{badarg,_}} = (catch counters:get(Ref, 10)),
    {'EXIT',{badarg,_}} = (catch counters:get(Ref, 7.0)),
    ok.


limits(Config) when is_list(Config) ->
    Ref = counters:new(1,[]),

    Max = counters:max(Ref),
    Min = counters:min(Ref),
    0 = counters:get(Ref, 0),
    Max = counters:add_get(Ref, 0, Max),
    Min = counters:add_get(Ref, 0, 1),
    Max = counters:sub_get(Ref, 0, 1),

    {'EXIT',{badarg,_}} = (catch counters:add(Ref, 0, Max+1)),
    {'EXIT',{badarg,_}} = (catch counters:add(Ref, 0, Min-1)),

    ok.
