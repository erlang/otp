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

-ifndef(CT_RESULTS_PARSER_HRL_).
-define(CT_RESULTS_PARSER_HRL_, 1).

%% Table row in ct_logs/index.html and ct_logs/ct_run.*/index.html
-record(test, {
    test_name :: string(),
    suite_log_link :: string(),
    label :: atom() | undefined,
    start_date :: calendar:datetime(),
    ok :: non_neg_integer(),
    failed :: non_neg_integer(),
    skipped :: non_neg_integer(),
    user_skipped :: non_neg_integer(),
    auto_skipped :: non_neg_integer(),
    missing_suites :: non_neg_integer(),
    node :: node(),
    ct_log_link :: string(),
    old_runs_link :: string() | link | undefined,
    elapsed_time :: calendar:time()
}).

%% Total row in ct_logs/index.html and ct_logs/ct_run.*/index.html
-record(total, {
    ok :: non_neg_integer(),
    failed :: non_neg_integer(),
    skipped :: non_neg_integer(),
    user_skipped :: non_neg_integer(),
    auto_skipped :: non_neg_integer(),
    missing_suites :: non_neg_integer(),
    elapsed_time :: calendar:time()
}).

%% Test name above table in ct_logs/ct_run.*/*.logs/run.*/suite.log.html
-record(suite_test_name, {
    test_name :: string()
}).

%% Table row in ct_logs/ct_run.*/*.logs/run.*/suite.log.html
-record(test_case, {
    num :: pos_integer() | undefined,
    module :: module(),
    group :: atom() | undefined,
    tc :: atom(),
    tc_link :: string(),
    top_log :: string(),
    end_log :: string(),
    time :: calendar:time(),
    result :: atom(),
    comment :: [string()] | undefined
}).

%% Total row in ct_logs/ct_run.*/*.logs/run.*/suite.log.html
-record(test_cases_total, {
    time :: calendar:time(),
    result :: atom(),
    ok :: non_neg_integer(),
    failed :: non_neg_integer(),
    skipped :: non_neg_integer(),
    total :: non_neg_integer(),
    elapsed_time :: calendar:time()
}).

%% Table row in ct_logs/all_runs.html

-record(old_run, {
    start_date :: calendar:datetime(),
    link :: string(),
    node :: atom(),
    label :: atom() | undefined,
    tests :: non_neg_integer(),
    test_names :: string(),
    total :: non_neg_integer(),
    ok :: non_neg_integer(),
    failed :: non_neg_integer(),
    skipped :: non_neg_integer(),
    user_skipped :: non_neg_integer(),
    auto_skipped :: non_neg_integer(),
    missing_suites :: non_neg_integer()
}).

-endif.
