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

-module(verify_config_cth).

-include_lib("common_test/src/ct_util.hrl").

%% CT Hooks
-compile(export_all).

-define(val(K, L), proplists:get_value(K, L)).

id(Opts) ->
    ?MODULE.

init(Id, Opts) ->
    {ok, State} = empty_cth:init(Id, Opts),
    {ok, State}.

pre_init_per_suite(Suite, Config, State) ->
    ct_no_config_SUITE = ct:get_config(suite_cfg),
    empty_cth:pre_init_per_suite(Suite,
				 [{pre_init_per_suite,true} | Config],
				 State).

post_init_per_suite(Suite,Config,Return,State) ->
    true = ?val(pre_init_per_suite, Return),
    ct_no_config_SUITE = ct:get_config(suite_cfg),
    empty_cth:post_init_per_suite(Suite,
				  Config,
				  [{post_init_per_suite,true} | Return],
				  State).

pre_end_per_suite(Suite,Config,State) ->
    true = ?val(post_init_per_suite, Config),
    ct_no_config_SUITE = ct:get_config(suite_cfg),
    empty_cth:pre_end_per_suite(Suite,
				[{pre_end_per_suite,true} | Config],
				State).

post_end_per_suite(Suite,Config,Return,State) ->
    true = ?val(pre_end_per_suite, Config),
    ct_no_config_SUITE = ct:get_config(suite_cfg),
    empty_cth:post_end_per_suite(Suite,Config,Return,State).

pre_init_per_group(Suite,Group,Config,State) ->
    true = ?val(post_init_per_suite, Config),
    ct_no_config_SUITE = ct:get_config(suite_cfg),
    test_group = ct:get_config(group_cfg),
    empty_cth:pre_init_per_group(Suite,Group,
				 [{pre_init_per_group,true} | Config],
				 State).

post_init_per_group(Suite,Group,Config,Return,State) ->
    true = ?val(pre_init_per_group, Return),
    test_group = ct:get_config(group_cfg),
    empty_cth:post_init_per_group(Suite,Group,
				  Config,
				  [{post_init_per_group,true} | Return],
				  State).

pre_end_per_group(Suite,Group,Config,State) ->
    true = ?val(post_init_per_group, Config),
    ct_no_config_SUITE = ct:get_config(suite_cfg),
    test_group = ct:get_config(group_cfg),
    empty_cth:pre_end_per_group(Suite,Group,
				[{pre_end_per_group,true} | Config],
				State).

post_end_per_group(Suite,Group,Config,Return,State) ->
    true = ?val(pre_end_per_group, Config),
    ct_no_config_SUITE = ct:get_config(suite_cfg),
    test_group = ct:get_config(group_cfg),
    empty_cth:post_end_per_group(Suite,Group,Config,Return,State).

pre_init_per_testcase(Suite,TC,Config,State) ->
    true = ?val(post_init_per_suite, Config),
    case ?val(name, ?val(tc_group_properties, Config)) of
	undefined -> 
	    ok;
	_ ->
	    true = ?val(post_init_per_group, Config),
	    test_group = ct:get_config(group_cfg)
    end,
    ct_no_config_SUITE = ct:get_config(suite_cfg),
    CfgKey = list_to_atom(atom_to_list(TC) ++ "_cfg"),
    TC = ct:get_config(CfgKey),
    empty_cth:pre_init_per_testcase(Suite,TC,
				    [{pre_init_per_testcase,true} | Config],
				    State).

%%! TODO: Verify Config also in post_init and pre_end!

post_init_per_testcase(Suite,TC,Config,Return,State) ->
    empty_cth:post_init_per_testcase(Suite,TC,Config,Return,State).

pre_end_per_testcase(Suite,TC,Config,State) ->
    empty_cth:pre_end_per_testcase(Suite,TC,Config,State).

post_end_per_testcase(Suite,TC,Config,Return,State) ->
    true = ?val(post_init_per_suite, Config),
    true = ?val(pre_init_per_testcase, Config),
    case ?val(name, ?val(tc_group_properties, Config)) of
	undefined -> 
	    ok;
	_ ->
	    true = ?val(post_init_per_group, Config),	    
	    test_group = ct:get_config(group_cfg)
    end,
    ct_no_config_SUITE = ct:get_config(suite_cfg),
    CfgKey = list_to_atom(atom_to_list(TC) ++ "_cfg"),
    TC = ct:get_config(CfgKey),
    empty_cth:post_end_per_testcase(Suite,TC,Config,Return,State).

on_tc_fail(Suite,TC, Reason, State) ->
    empty_cth:on_tc_fail(Suite,TC,Reason,State).

on_tc_skip(Suite,TC, Reason, State) ->
    empty_cth:on_tc_skip(Suite,TC,Reason,State).

terminate(State) ->
    empty_cth:terminate(State).
