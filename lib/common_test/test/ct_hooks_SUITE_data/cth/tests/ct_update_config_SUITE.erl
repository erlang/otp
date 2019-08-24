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

-module(ct_update_config_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

-define(now, ct_test_support:unique_timestamp()).

%% Test server callback functions
init_per_suite(Config) ->
    [{init_per_suite,?now}|Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    [{init_per_testcase,?now}|Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

init_per_group(GroupName, Config) ->
    [{init_per_group,?now}|Config].

end_per_group(GroupName, Config) ->
    ok.

all() ->
    [{group,group1}].

groups() ->
    [{group1,[],[test_case]}].
    
%% Test cases starts here.
test_case(Config) when is_list(Config) ->
    ok.
