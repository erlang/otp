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
     {default_config, x1, {x,suite}}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

all() -> [test_get_config_simple, test_get_config_nested,
	  test_get_config_deep_nested, test_default_suitewide,
	  test_config_name_already_in_use1, test_default_tclocal,
	  test_config_name_already_in_use2, test_alias_tclocal,
	  test_get_config_undefined,
	  test_require_subvals,test_require_subvals2,test_require_deep_config,
	  test_shadow_all,test_element,test_shadow_all_element,
	  test_internal_deep, test_alias_tclocal_nested,
	  test_alias_tclocal_nested_backward_compat,
	  test_alias_tclocal_nested_backward_compat_subvals,
	  test_config_same_name_already_in_use
].

init_per_testcase(_,Config) ->
    Config.

end_per_testcase(test_alias_tclocal_nested_backward_compat, _) ->
    os:putenv("COMMON_TEST_ALIAS_TOP",""),
    ok;
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

%% test getting a deep nested value
test_get_config_deep_nested(_)->
    d_value = ct:get_config({gen_cfg, c, d}),
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


test_config_same_name_already_in_use() ->
    [].
test_config_same_name_already_in_use(_) ->
    ok = ct:require(x2,{gen_cfg,c}),
    ok = ct:require(x2,{gen_cfg,c}).

%% test aliases
test_alias_tclocal() ->
    [{require,newalias,gen_cfg}].
test_alias_tclocal(C) when is_list(C) ->
    test_alias_tclocal(newalias);
test_alias_tclocal(Alias) when is_atom(Alias) ->
    A = [{a,a_value},{b,b_value},{c,[{d,d_value}]}] = ct:get_config(Alias),
    A = ct:get_config(gen_cfg),
    B = b_value = ct:get_config({Alias,b}),
    B = ct:get_config({gen_cfg,b}),
    ok.

%% test nested aliases
test_alias_tclocal_nested() ->
    [{require,newalias2,{gen_cfg,c}}].
test_alias_tclocal_nested(_) ->
    A = [{d,d_value}] = ct:get_config(newalias2),
    A = ct:get_config({gen_cfg,c}),
    B = d_value = ct:get_config({newalias2,d}),
    B = ct:get_config({gen_cfg,c,d}),
    ok.

%% test nested aliases backward compat option
test_alias_tclocal_nested_backward_compat() ->
    os:putenv("COMMON_TEST_ALIAS_TOP","true"),
    [{require,newalias3,{gen_cfg,c}}].
test_alias_tclocal_nested_backward_compat(_) ->
    test_alias_tclocal(newalias3).

%% test nested aliases backward compat option
test_alias_tclocal_nested_backward_compat_subvals() ->
    [{require,newalias4,{gen_cfg,[c]}}].
test_alias_tclocal_nested_backward_compat_subvals(_) ->
    test_alias_tclocal(newalias4).

%% test for getting undefined variables
test_get_config_undefined(_) ->
    undefined = ct:get_config(y1),
    ok.

test_require_subvals() ->
    [{require, {gen_cfg,[a,b,c]}}].
test_require_subvals(_) ->
    ok.

test_require_subvals2() ->
    [{require, {gen_cfg,[a,b,c,d]}}].
test_require_subvals2(_) ->
    ct:fail("Test should've been skipped, you shouldn't see this!"),
    ok.

test_require_deep_config() ->
    [{require, {gen_cfg3, m, n}}].
test_require_deep_config(_) ->
    ok.


test_shadow_all(_) ->
    ["n","N"] = ct:get_config({gen_cfg3,l, m, n}, [], [all]).

test_element(_) ->
    {{gen_cfg3,l, m, n},"n"} = ct:get_config({gen_cfg3,l, m, n}, [], [element]).

test_shadow_all_element(_) ->
    [{{gen_cfg3,l, m, n},"n"},{{gen_cfg3,l, m, n},"N"}] =
	ct:get_config({gen_cfg3,l, m, n}, [], [all,element]).

%% The tests below are needed to verify that things like ct:telnet can use
%% nested configs
test_internal_deep(_) ->
    "n" = ct:get_config({{gen_cfg3,l,m},n}),
    a_value = ct:get_config({{gen_cfg},a}),
    undefined = ct:get_config({{gen_cfg3,l,m},p}).
