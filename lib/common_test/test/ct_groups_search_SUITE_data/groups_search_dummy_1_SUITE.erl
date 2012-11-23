%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(groups_search_dummy_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


all() ->
    [{group,top1},
     {group,top2}].

groups() ->
    [{top1,[],[top1_tc1,top1_tc2,{sub1,[],[sub1_tc1,sub1_tc2]}]},
     {top2,[],[{group,sub2},top2_tc1,top2_tc2]},
     {sub2,[],[sub2_tc1,sub2_tc2]}].

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

%%%-----------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------

top1_tc1(_) ->
    ok.

top1_tc2(_) ->
    ok.

sub1_tc1(_) ->
    ok.

sub1_tc2(_) ->
    ok.

top2_tc1(_) ->
    ok.

top2_tc2(_) ->
    ok.

sub2_tc1(_) ->
    ok.

sub2_tc2(_) ->
    ok.
