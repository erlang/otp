%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-module(test_server_cover_SUITE).

-export([all/1, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([tc1/1, tc2/1]).

-include_lib("common_test/include/ct.hrl").

all(suite) ->
    [tc1,tc2].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case,Config) ->
    Dog = ?t:timetrap({minutes,10}),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case,Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.


%%%-----------------------------------------------------------------
%%% Test cases
tc1(Config) when is_list(Config) ->
    cover_helper:foo(),
    ok.

tc2(Config) when is_list(Config) ->
    cover_helper:bar(),
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions

