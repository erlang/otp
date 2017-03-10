%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-module(ct_no_init_config_SUITE).

-compile(export_all).

-include("ct.hrl").

%%% This suite is used to verify that all
%%% pre/post_init_per_group/testcase callbacks are called with correct
%%% SuiteName even if no init_per_group/testcase function exist in the
%%% suite, and that the non-exported config functions fail with 'undef'.

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

end_per_group(_Group,Config) ->
    Config.

end_per_testcase(_TC,Config) ->
    Config.

all() ->
    [test_case_1, {group,test_group}].

groups() ->
    [{test_group,[],[test_case_2]}].

test_case_1(Config) ->
    ok.

test_case_2(Config) ->
    ok.
