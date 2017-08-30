%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
-module(groups_search_dummy_2_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


all() ->
    [{group,top1},
     {group,top2}].

groups() ->
    [{top1,[],[top1_tc1,top_tc2,tc3,
	       {sub11,[],[sub11_tc1,sub_tc2,tc3]},
	       {sub12,[],[sub12_tc1,sub_tc2,tc3,
			  {sub121,[],[sub121_tc1,sub_tc2,tc3]}]}]},
     
     {top2,[],[{group,sub21},top2_tc1,top_tc2,tc3,{group,sub22}]},
     {sub21,[],[sub21_tc1,sub_tc2,tc3,{group,sub2xx}]},
     {sub22,[],[{group,sub221},sub22_tc1,sub_tc2,tc3,{group,sub2xx}]},
     {sub221,[],[sub221_tc1,sub_tc2,tc3]},
     {sub2xx,[],[sub2xx_tc1,sub_tc2,tc3]}].

%%%-----------------------------------------------------------------
%%% CONFIG FUNCS
%%%-----------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

%%%------------------------------------------------------------------
%%% TEST CASES
%%%------------------------------------------------------------------

top1_tc1(_) ->
    ok.

top_tc2(_) ->
    ok.

tc3(_) ->
    ok.

sub_tc2(_) ->
    ok.

sub11_tc1(_) ->
    ok.

sub12_tc1(_) ->
    ok.

sub121_tc1(_) ->
    ok.
     
top2_tc1(_) ->
    ok.

sub21_tc1(_) ->
    ok.

sub22_tc1(_) ->
    ok.

sub221_tc1(_) ->
    ok.

sub2xx_tc1(_) ->
    ok.
