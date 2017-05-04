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

-module(all_hook_callbacks_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

%% Test server callback functions
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(Config) ->
    Config.

end_per_group(_Config) ->
    ok.

init_per_testcase(skip_case, Config) ->
    {skip,"Skipped in init_per_testcase/2"};
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [{group,test_group},test_case,skip_case].

groups() ->
    [{test_group,[test_case]}].

%% Test cases starts here.
test_case(Config) ->
    ok.

skip_case(Config) ->
    ok.
