%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2011-2025. All Rights Reserved.
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
-module(cth_events).
-moduledoc false.

%%% This module verifies logger events.

%% CTH Callbacks
-export([id/1, init/2,
	 %% pre_init_per_suite/3, pre_end_per_suite/3, post_end_per_suite/4,
	 %% pre_init_per_group/4, post_init_per_group/5,
	 %% pre_end_per_group/4, post_end_per_group/5,
	 pre_init_per_testcase/4, %post_init_per_testcase/5,
	 %% pre_end_per_testcase/4,
         post_end_per_testcase/5]).

-behaviour(ct_hooks).

id(_Opts) ->
    ?MODULE.

init(?MODULE, Opts) ->
    GetValue =
        fun(Property, PropList, Default) ->
                case proplists:get_value(Property, PropList) of
                    undefined -> Default;
                    V -> V
                end
        end,

    DefaultVerifyFun =
        fun(_, 0) ->
                ok;
           (_, EventNumber) when EventNumber > 0 ->
                {fail, lists:flatten(
                         io_lib:format("unexpected event cnt: ~s",
                                       [integer_to_list(EventNumber)]))}
        end,
    VerifyFun = GetValue(verify_fun, Opts, DefaultVerifyFun),
    SkipTc = GetValue(skip_tc, Opts, []),
    ct_util:mark_process(), % ??
    {ok, #{verify_fun => VerifyFun, skip_tc => SkipTc}}.

%% FIXME in parallel executions (e.g. ssh_basic_SUITE:p_basic group) this setup does not
%% work log handlers are uniq per testcase, but they all receive same
%% logger events; so if one testcase fails due to logger events, rest
%% of group might fail as well
pre_init_per_testcase(_Suite, TestCase, Config0, State = #{skip_tc := SkipTc}) ->
    case lists:member(TestCase, SkipTc) of
        false ->
            Config = ssh_test_lib:add_log_handler(TestCase, Config0),
            {Config, State};
        true ->
            {Config0, State}
    end.

post_end_per_testcase(_Suite, TestCase, Config, Result,
                      State = #{skip_tc := SkipTc,
                                verify_fun := VerifyFun}) ->
    case lists:member(TestCase, SkipTc) of
        false ->
            {ok, Events} = ssh_test_lib:get_log_events(
                             proplists:get_value(log_handler_ref, Config)),
            EventCnt = length(Events),
            {ok, InterestingEventCnt} = ssh_test_lib:analyze_events(Events, EventCnt),
            VerificationResult = VerifyFun(TestCase, InterestingEventCnt),
            ssh_test_lib:rm_log_handler(TestCase),
            case VerificationResult of
                ok ->
                    {Result, State};
                _ ->
                    {VerificationResult, State}
            end;
        true ->
            {Result, State}
    end.


