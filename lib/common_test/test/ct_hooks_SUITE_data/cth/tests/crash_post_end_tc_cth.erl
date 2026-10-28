%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

%% CTH crashing in post_end_per_testcase with a long, nested exit
%% reason (gen_server:call timeout with a large request term). Used to
%% verify how such a reason is presented in the "=== Reason:" line.

-module(crash_post_end_tc_cth).


-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").


%% CT Hooks
-compile(export_all).

init(Id, Opts) ->
    empty_cth:init(Id, Opts).

pre_init_per_suite(Suite, Config, State) ->
    empty_cth:pre_init_per_suite(Suite,Config,State).

post_init_per_suite(Suite,Config,Return,State) ->
    empty_cth:post_init_per_suite(Suite,Config,Return,State).

pre_end_per_suite(Suite,Config,State) ->
    empty_cth:pre_end_per_suite(Suite,Config,State).

post_end_per_suite(Suite,Config,Return,State) ->
    empty_cth:post_end_per_suite(Suite,Config,Return,State).

pre_init_per_group(Suite,Group,Config,State) ->
    empty_cth:pre_init_per_group(Suite,Group,Config,State).

post_init_per_group(Suite,Group,Config,Return,State) ->
    empty_cth:post_init_per_group(Suite,Group,Config,Return,State).

pre_end_per_group(Suite,Group,Config,State) ->
    empty_cth:pre_end_per_group(Suite,Group,Config,State).

post_end_per_group(Suite,Group,Config,Return,State) ->
    empty_cth:post_end_per_group(Suite,Group,Config,Return,State).

pre_init_per_testcase(Suite,TC,Config,State) ->
    empty_cth:pre_init_per_testcase(Suite,TC,Config,State).

post_init_per_testcase(Suite,TC,Config,Return,State) ->
    empty_cth:post_init_per_testcase(Suite,TC,Config,Return,State).

pre_end_per_testcase(Suite,TC,Config,State) ->
    empty_cth:pre_end_per_testcase(Suite,TC,Config,State).

post_end_per_testcase(Suite,TC,Config,Return,State) ->
    empty_cth:post_end_per_testcase(Suite,TC,Config,Return,State),
    Name = ?MODULE,
    Pid = spawn(fun() -> receive stop -> ok end end),
    true = register(Name, Pid),
    Request = {assert_no_failures,
               [{"SOME_REFERENCE",
                 "related to XXYYZZ"}]},
    try
        [gen_server:call(Name, R, 100) || R <- [Request]]
    after
        Pid ! stop
    end.

on_tc_fail(Suite,TC, Reason, State) ->
    empty_cth:on_tc_fail(Suite,TC,Reason,State).

on_tc_skip(Suite,TC, Reason, State) ->
    empty_cth:on_tc_skip(Suite,TC,Reason,State).

terminate(State) ->
    empty_cth:terminate(State).
