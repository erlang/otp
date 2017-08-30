%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([app_file/1, appup_file/1, config/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    case test_server:os_type() of
        {unix, sunos} -> [app_file, appup_file, config];
        _OS -> [app_file, appup_file]
    end.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    application:stop(os_mon),
    ok.

%% Testing .app file
app_file(Config) when is_list(Config) ->
    ok = test_server:app_test(os_mon),
    ok.

appup_file(Config) when is_list(Config) ->
    ok = test_server:appup_test(os_mon).

%% Test OS_Mon configuration
config(Config) when is_list(Config) ->

    IsReg = fun(Name) -> is_pid(whereis(Name)) end,
    IsNotReg = fun(Name) -> undefined == whereis(Name) end,

    ok = application:start(os_mon),
    true = lists:all(IsReg, [cpu_sup, disksup, memsup]),
    ok = application:stop(os_mon),

    ok = application:set_env(os_mon, start_cpu_sup, false),
    ok = application:start(os_mon),
    true = lists:all(IsReg, [disksup, memsup]),
    true = IsNotReg(cpu_sup),
    ok = application:stop(os_mon),
    ok = application:set_env(os_mon, start_cpu_sup, true),

    ok = application:set_env(os_mon, start_disksup, false),
    ok = application:start(os_mon),
    true = lists:all(IsReg, [cpu_sup, memsup]),
    true = IsNotReg(disksup),
    ok = application:stop(os_mon),
    ok = application:set_env(os_mon, start_disksup, true),

    ok = application:set_env(os_mon, start_memsup, false),
    ok = application:start(os_mon),
    true = lists:all(IsReg, [cpu_sup, disksup]),
    true = IsNotReg(memsup),
    ok = application:stop(os_mon),
    ok = application:set_env(os_mon, start_memsup, true),

    ok.
