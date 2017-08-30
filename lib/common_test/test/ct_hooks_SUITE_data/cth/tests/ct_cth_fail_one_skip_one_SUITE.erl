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

-module(ct_cth_fail_one_skip_one_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

%% Test server callback functions
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group,Config) ->
    Config.

end_per_group(_Group,_Config) ->
    ok.

init_per_testcase(test_case2, Config) ->
    {skip,"skip it"};
init_per_testcase(test_case3, Config) ->
    {skipped,"skip it"};
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [{group1,[parallel],
      [{group2,[parallel],
	[test_case1,test_case2,test_case3,test_case4]}]}].

all() ->
    [{group,group1}].

%% Test cases starts here.
test_case1(Config) ->
    ok = nok.

test_case2(Config) ->
    ok.

test_case3(Config) ->
    ok.

test_case4(Config) ->
    ok.
