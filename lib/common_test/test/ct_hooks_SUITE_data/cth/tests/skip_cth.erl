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


-module(skip_cth).


-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").

%% Send a cth_error event if a callback is called with unexpected arguments
-define(fail(Info),
        gen_event:notify(
          ?CT_EVMGR_REF, 
          #event{ name = cth_error,
                  node = node(),
                  data = {illegal_hook_callback,{?MODULE,?FUNCTION_NAME,Info}}})).

%% CT Hooks
-compile(export_all).

id(Opts) ->
    empty_cth:id(Opts).

init(Id, Opts) ->
    empty_cth:init(Id, Opts).

pre_init_per_suite(Suite, Config, State) ->
    Suite==skip_init_SUITE
        orelse Suite==skip_group_SUITE
        orelse Suite==skip_case_SUITE
        orelse Suite==seq_SUITE
        orelse Suite==repeat_SUITE
        orelse Suite==config_clash_SUITE
        orelse ?fail(Suite),
    empty_cth:pre_init_per_suite(Suite,Config,State).

post_init_per_suite(Suite,Config,Return,State) ->
    Suite==skip_init_SUITE
        orelse Suite==skip_group_SUITE
        orelse Suite==skip_case_SUITE
        orelse Suite==seq_SUITE
        orelse Suite==repeat_SUITE
        orelse Suite==config_clash_SUITE
        orelse ?fail(Suite),
    empty_cth:post_init_per_suite(Suite,Config,Return,State).

pre_end_per_suite(Suite,Config,State) ->
    Suite==skip_case_SUITE
        orelse Suite==skip_group_SUITE
        orelse Suite==seq_SUITE
        orelse Suite==repeat_SUITE
        orelse Suite==config_clash_SUITE
        orelse ?fail(Suite),
    empty_cth:pre_end_per_suite(Suite,Config,State).

post_end_per_suite(Suite,Config,Return,State) ->
    Suite==skip_case_SUITE
        orelse Suite==skip_group_SUITE
        orelse Suite==seq_SUITE
        orelse Suite==repeat_SUITE
        orelse Suite==config_clash_SUITE
        orelse ?fail(Suite),
    empty_cth:post_end_per_suite(Suite,Config,Return,State).

pre_init_per_group(Suite,Group,Config,State) ->
    (Suite==skip_group_SUITE andalso Group==test_group_3)
        orelse ?fail({Suite,Group}),
    empty_cth:pre_init_per_group(Suite,Group,Config,State).

post_init_per_group(Suite,Group,Config,Return,State) ->
    (Suite==skip_group_SUITE andalso Group==test_group_3)
        orelse ?fail({Suite,Group}),
    empty_cth:post_init_per_group(Suite,Group,Config,Return,State).

pre_end_per_group(Suite,Group,Config,State) ->
    ?fail({Suite,Group}),
    empty_cth:pre_end_per_group(Suite,Group,Config,State).

post_end_per_group(Suite,Group,Config,Return,State) ->
    ?fail({Suite,Group}),
    empty_cth:post_end_per_group(Suite,Group,Config,Return,State).

pre_init_per_testcase(Suite,TC,Config,State) ->
    (Suite==skip_case_SUITE andalso (TC==skip_in_init
                                     orelse TC==fail_in_init
                                     orelse TC==exit_in_init
                                     orelse TC==fail_in_end
                                     orelse TC==exit_in_end
                                     orelse TC==skip_in_case))
        orelse (Suite==seq_SUITE andalso TC==test_case_1)
        orelse (Suite==repeat_SUITE andalso TC==test_case_1)
        orelse ?fail({Suite,TC}),
    empty_cth:pre_init_per_testcase(Suite,TC,Config,State).

post_init_per_testcase(Suite,TC,Config,Return,State) ->
    (Suite==skip_case_SUITE andalso (TC==skip_in_init
                                     orelse TC==fail_in_init
                                     orelse TC==exit_in_init
                                     orelse TC==fail_in_end
                                     orelse TC==exit_in_end
                                     orelse TC==skip_in_case))
        orelse (Suite==seq_SUITE andalso TC==test_case_1)
        orelse (Suite==repeat_SUITE andalso TC==test_case_1)
        orelse ?fail({Suite,TC}),
    empty_cth:post_init_per_testcase(Suite,TC,Config,Return,State).

pre_end_per_testcase(Suite,TC,Config,State) ->
    (Suite==skip_case_SUITE andalso (TC==skip_in_case
                                     orelse TC==fail_in_end
                                     orelse TC==exit_in_end))
        orelse (Suite==seq_SUITE andalso TC==test_case_1)
        orelse (Suite==repeat_SUITE andalso TC==test_case_1)
        orelse ?fail({Suite,TC}),
    empty_cth:pre_end_per_testcase(Suite,TC,Config,State).

post_end_per_testcase(Suite,TC,Config,Return,State) ->
    (Suite==skip_case_SUITE andalso (TC==skip_in_case
                                     orelse TC==fail_in_end
                                     orelse TC==exit_in_end))
        orelse (Suite==seq_SUITE andalso TC==test_case_1)
        orelse (Suite==repeat_SUITE andalso TC==test_case_1)
        orelse ?fail({Suite,TC}),
    empty_cth:post_end_per_testcase(Suite,TC,Config,Return,State).

on_tc_fail(Suite,TC,Reason,State) ->
    (Suite==seq_SUITE andalso TC==test_case_1)
        orelse (Suite==config_clash_SUITE andalso TC==test_case_1)
        orelse ?fail({Suite,TC}),
    empty_cth:on_tc_fail(Suite,TC,Reason,State).

on_tc_skip(all_hook_callbacks_SUITE=Suite,all=TC, Reason, State) ->
    empty_cth:on_tc_skip(Suite,TC,Reason,State);
on_tc_skip(Suite,TC,Reason,State)
  when (Suite==skip_init_SUITE
        orelse Suite==skip_req_SUITE
        orelse Suite==skip_fail_SUITE)
       andalso
       (TC==init_per_suite
        orelse TC==test_case
        orelse TC==end_per_suite) ->
    empty_cth:on_tc_skip(Suite,TC,Reason,State);
on_tc_skip(skip_group_SUITE=Suite,TC={C,G},Reason,State)
  when (C==init_per_group orelse C==test_case orelse C==end_per_group) andalso
       (G==test_group_1 orelse G==test_group_2 orelse G==test_group_3) ->
    empty_cth:on_tc_skip(Suite,TC,Reason,State);
on_tc_skip(skip_case_SUITE=Suite,TC,Reason,State)
  when TC==skip_in_spec;
       TC==skip_in_init;
       TC==fail_in_init;
       TC==exit_in_init;
       TC==skip_in_case;
       TC==req_auto_skip;
       TC==fail_auto_skip ->
    empty_cth:on_tc_skip(Suite,TC,Reason,State);
on_tc_skip(Suite,TC,Reason,State)
  when (Suite==seq_SUITE andalso TC==test_case_2)
       orelse (Suite==repeat_SUITE andalso TC==test_case_2) ->
    empty_cth:on_tc_skip(Suite,TC,Reason,State);
on_tc_skip(Suite,TC,Reason,State) ->
    ?fail({Suite,TC}),
    empty_cth:on_tc_skip(Suite,TC,Reason,State).

terminate(State) ->
    empty_cth:terminate(State).
