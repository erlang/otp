%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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
-module(os_mon_SUITE).
-include_lib("test_server/include/test_server.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([app_file/1, appup_file/1, config/1]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case test_server:os_type() of
	{unix, sunos} -> [app_file, appup_file, config];
	_OS -> [app_file, appup_file]
    end.

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


app_file(suite) ->
    [];
app_file(doc) ->
    ["Testing .app file"];
app_file(Config) when is_list(Config) ->
    ?line ok = test_server:app_test(os_mon),
    ok.

appup_file(Config) when is_list(Config) ->
    ok = test_server:appup_test(os_mon).

config(suite) ->
    [];
config(doc) ->
    ["Test OS_Mon configuration"];
config(Config) when is_list(Config) ->

    IsReg = fun(Name) -> is_pid(whereis(Name)) end,
    IsNotReg = fun(Name) -> undefined == whereis(Name) end,

    ?line ok = application:start(os_mon),
    ?line true = lists:all(IsReg, [cpu_sup, disksup, memsup]),
    ?line ok = application:stop(os_mon),

    ?line ok = application:set_env(os_mon, start_cpu_sup, false),
    ?line ok = application:start(os_mon),
    ?line true = lists:all(IsReg, [disksup, memsup]),
    ?line true = IsNotReg(cpu_sup),
    ?line ok = application:stop(os_mon),
    ?line ok = application:set_env(os_mon, start_cpu_sup, true),

    ?line ok = application:set_env(os_mon, start_disksup, false),
    ?line ok = application:start(os_mon),
    ?line true = lists:all(IsReg, [cpu_sup, memsup]),
    ?line true = IsNotReg(disksup),
    ?line ok = application:stop(os_mon),
    ?line ok = application:set_env(os_mon, start_disksup, true),

    ?line ok = application:set_env(os_mon, start_memsup, false),
    ?line ok = application:start(os_mon),
    ?line true = lists:all(IsReg, [cpu_sup, disksup]),
    ?line true = IsNotReg(memsup),
    ?line ok = application:stop(os_mon),
    ?line ok = application:set_env(os_mon, start_memsup, true),

    ok.
