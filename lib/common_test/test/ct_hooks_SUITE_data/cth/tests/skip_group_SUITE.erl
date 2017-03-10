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

-module(skip_group_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    ok.

group(test_group_1) ->
    [{require,whatever}];
group(test_group_2) ->
    faulty_return_value;
group(_) ->
    [].

init_per_group(test_group_3,Config) ->
    {skip,"Skipped in init_per_group/2"};
init_per_group(_,Config) ->
    ct:fail("This shall never be run due to auto_skip from group/1").

end_per_group(_,_) ->
    ct:fail("This shall never be run").

all() ->
    [{group,test_group_1},
     {group,test_group_2},
     {group,test_group_3}].

groups() ->
    [{test_group_1,[test_case]},
     {test_group_2,[test_case]},
     {test_group_3,[test_case]}].

%% Test cases starts here.
test_case(_Config) ->    
    ct:fail("This test case shall never be run due to skip on group level").

