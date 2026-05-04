%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

-module(t1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


all() ->
    [{group, group1},
     {group, group2},
     test1a,
     test1b,
     test1c,
     test1d,
     test2a,
     test2b,
     test2c,
     test2d].

groups() ->
    [{group1, [], [{group, subgroup1a},
                   {group, subgroup1b}]},
     {group2, [], [{group, subgroup2a},
                   {group, subgroup2b}]},
     {subgroup1a, [], [test1a, test1b]},
     {subgroup1b, [], [test1c, test1d]},
     {subgroup2a, [], [test2a, test2b]},
     {subgroup2b, [], [test2c, test2d]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(test1a, _Config) ->
    {skip, <<"Reason">>};
init_per_testcase(test1b, _Config) ->
    throw(skip);
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

test1a(_Config) -> ok.
test1b(_Config) -> ok.
test1c(_Config) -> ok.
test1d(_Config) -> ok.
test2a(_Config) -> ok.
test2b(_Config) -> ok.
test2c(_Config) -> ok.
test2d(_Config) -> ok.
