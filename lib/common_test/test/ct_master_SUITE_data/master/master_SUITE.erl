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

%%%-------------------------------------------------------------------
%%% File: master_SUITE
%%%
%%% Description:
%%% Test suite for common_test which tests the ct_master functionality
%%%-------------------------------------------------------------------
-module(master_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("test.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

all() -> [first_testcase, second_testcase, third_testcase,
	  env_vars].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

first_testcase(_)->
    b = ct:get_config(a).

second_testcase(_)->
    d = ct:get_config(c).

third_testcase(_)->
    A = 4,
    A = 2*2.

env_vars(_) ->
    io:format("~p\n", [os:getenv()]),
    "yes" = os:getenv("THIS_MUST_BE_SET"),
    "value" = os:getenv("SO_MUST_THIS"),
    ok.
