%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
-module(ct_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [app_file, appup_file, capture_sends_no_messages].

%%%-----------------------------------------------------------------
%%% Test cases

app_file(_Config) ->
    ok = test_server:app_test(common_test),
    ok.

appup_file(_Config) ->
    ok = test_server:appup_test(common_test).

% See GitHub issue #6016, we shouldn't abuse the process mailbox to store the
% captured messages in such that receive won't return unexpected messages or
% worse, even throw them away.
capture_sends_no_messages(_Config) ->
    ct:capture_start(),
    io:fwrite("Hello from ~s", [?FUNCTION_NAME]),
    ct:capture_stop(),
    receive
        Anything -> ct:fail("Received unwanted data: ~w", [Anything])
    after
        0 -> ok
    end,
    [_Message] = ct:capture_get().
