%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2011. All Rights Reserved.
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
-module(format_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([hang_1/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("test_server/include/test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [hang_1].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


hang_1(doc) ->
    ["Bad args can hang (OTP-2400)"];
hang_1(suite) ->
    [];
hang_1(Config) when is_list(Config) ->
    ?line _ = (catch io:format(a, "", [])),
    ?line _ = (catch io:format({}, "", [])),
    ok.
