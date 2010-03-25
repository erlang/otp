%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File: config_static_SUITE
%%%
%%% Description:
%%% Test suite for common_test which tests the get_config and require
%%% functionality
%%%-------------------------------------------------------------------
-module(config_static_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

% The config contains variables:
% x - atom
% gen_cfg - list with two key-values tagged with a and b
% gen_cfg2 - list of five key-values tagged with c, d, e, f and g
% gen_cfg3 - list of two complex key-values taggen with:
%	h: three elements inside - i, j and k
%	l: m inside, contains n and o

suite() ->
    [
     {timetrap, {seconds,10}},
     %% x1 doesn't exist in cfg-file!
     {require, x1, x},
     {require, gen_cfg3},
     {require, alias, gen_cfg},
     %% x1 default value
     {x1, {x,suite}}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

all() -> [test_get_config_simple, test_get_config_nested, test_default_suitewide,
	  test_config_name_already_in_use1, test_default_tclocal,
	  test_config_name_already_in_use2, test_alias_tclocal,
	  test_get_config_undefined].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

%% test getting a simple value
test_get_config_simple(_)->
    suite = ct:get_config(x),
    ok.

%% test getting a nested value
test_get_config_nested(_)->
    a_value = ct:get_config({gen_cfg, a}),
    ok.

%% test suite-wide default value
test_default_suitewide(_)->
    suite = ct:get_config(x1),
    ok.

%% should get skipped
test_config_name_already_in_use1() ->
    [{timetrap, {seconds,2}},
     {require, x1, x},
     {x1, {x,test2}}].
test_config_name_already_in_use1(_) ->
    ct:fail("Test should've been skipped, you shouldn't see this!"),
    ok.

%% test defaults in a testcase
test_default_tclocal() ->
    [{timetrap, {seconds,3}},
     {require, y1, y},
     {y1, {y,test3}}].
test_default_tclocal(_) ->
    test3 = ct:get_config(y1),
    ok.

%% should get skipped
test_config_name_already_in_use2() ->
    [{require,alias,something},
     {alias,{something,else}},
     {require, x1, x},
     {x1, {x,test4}}].
test_config_name_already_in_use2(_) ->
    ct:fail("Test should've been skipped, you shouldn't see this!"),
    ok.

%% test aliases
test_alias_tclocal() ->
    [{require,newalias,gen_cfg}].
test_alias_tclocal(_) ->
    A = [{a,a_value},{b,b_value}] = ct:get_config(newalias),
    A = ct:get_config(gen_cfg),
    ok.

%% test for getting undefined variables
test_get_config_undefined(_) ->
    undefined = ct:get_config(y1),
    ok.
