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

-module(curr_tc_SUITE).

-suite_defaults([{timetrap, {seconds, 3}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

init_per_suite(Config) ->
    [{?MODULE,?FUNCTION_NAME}] = ct_util:get_testdata(curr_tc),
    Config.

end_per_suite(Config) ->
    [{?MODULE,?FUNCTION_NAME}] = ct_util:get_testdata(curr_tc),
    ok.

init_per_group(_Group,Config) ->
    [{?MODULE,?FUNCTION_NAME}] = ct_util:get_testdata(curr_tc),
    Config.

end_per_group(_Group,Config) ->
    [{?MODULE,?FUNCTION_NAME}] = ct_util:get_testdata(curr_tc),
    ok.

all() ->
    [tc1,tc2,{group,g}].

groups() ->
    [{g,[tc1,tc2]}].

%% Test cases starts here.
tc1(_Config) ->
    [{?MODULE,?FUNCTION_NAME}] = ct_util:get_testdata(curr_tc),
    ok.

tc2(_Config) ->
    [{?MODULE,?FUNCTION_NAME}] = ct_util:get_testdata(curr_tc),
    ok.
