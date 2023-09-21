%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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


-module(ct_hooks_order_b_cth).

-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(now, ct_test_support:unique_timestamp()).
-define(ADD_LOC(L), [{self(), ?MODULE, ?FUNCTION_NAME} | L]).

%% CT Hooks
-compile([export_all, nowarn_export_all]).

init(Id, Opts) ->
    empty_cth:init(Id, Opts).

pre_init_per_suite(Suite, Config, State) ->
    empty_cth:pre_init_per_suite(Suite,Config,?ADD_LOC(State)),
    {[{pre_ips_b,?now}|Config],State}.

post_init_per_suite(Suite,Config,Return,State) ->
    empty_cth:post_init_per_suite(Suite,Config,Return,?ADD_LOC(State)),
    {[{post_ips_b,?now}|Return],State}.

pre_end_per_suite(Suite,Config,State) ->
    empty_cth:pre_end_per_suite(Suite,Config,?ADD_LOC(State)),
    {[{pre_eps_b,?now}|Config],State}.

post_end_per_suite(Suite,Config,Return,State) ->
    empty_cth:post_end_per_suite(Suite,Config,Return,?ADD_LOC(State)),
    {[{post_eps_b,?now}|Config],State}.

pre_init_per_group(Suite, Group,Config,State) ->
    empty_cth:pre_init_per_group(Suite,Group,Config,?ADD_LOC(State)),
    {[{pre_ipg_b,?now}|Config],State}.

post_init_per_group(Suite,Group,Config,Return,State) ->
    empty_cth:post_init_per_group(Suite,Group,Config,Return,?ADD_LOC(State)),
    {[{post_ipg_b,?now}|Return],State}.

pre_end_per_group(Suite,Group,Config,State) ->
    empty_cth:pre_end_per_group(Suite,Group,Config,?ADD_LOC(State)),
    {[{pre_epg_b,?now}|Config],State}.

post_end_per_group(Suite,Group,Config,Return,State) ->
    empty_cth:post_end_per_group(Suite,Group,Config,Return,?ADD_LOC(State)),
    {[{post_epg_b,?now}|Config],State}.

pre_init_per_testcase(Suite,TC,Config,State) ->
    empty_cth:pre_init_per_testcase(Suite,TC,Config,?ADD_LOC(State)),
    {[{pre_ipt_b,?now}|Config],State}.

post_init_per_testcase(Suite,TC,Config,Return,State) ->
    empty_cth:post_init_per_testcase(Suite,TC,Config,Return,?ADD_LOC(State)),
    Data = case Return of
               ok ->
                   Config;
               Return when is_list(Return) ->
                   Return
           end,
    {[{post_ipt_b,?now}|Data],State}.

pre_end_per_testcase(Suite,TC,Config,State) ->
    empty_cth:pre_end_per_testcase(Suite,TC,Config,?ADD_LOC(State)),
    {[{pre_ept_b,?now}|Config],State}.

post_end_per_testcase(Suite,TC,Config,Return,State) ->
    empty_cth:post_end_per_testcase(Suite,TC,Config,Return,?ADD_LOC(State)),
    {[{post_ept_b,?now}|Config],State}.

on_tc_fail(Suite,TC, Reason, State) ->
    empty_cth:on_tc_fail(Suite,TC,Reason,?ADD_LOC(State)).

on_tc_skip(Suite,TC, Reason, State) ->
    empty_cth:on_tc_skip(Suite,TC,Reason,?ADD_LOC(State)).

terminate(State) ->
    empty_cth:terminate(?ADD_LOC(State)).
