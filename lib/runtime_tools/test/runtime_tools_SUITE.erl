%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
-module(runtime_tools_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0]).

%% Test cases
-export([app_file/1, appup_file/1, start_stop_app/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [app_file,
     appup_file,
     start_stop_app].


app_file(_Config) ->
    ok = test_server:app_test(runtime_tools),
    ok.

appup_file(_Config) ->
    ok = test_server:appup_test(runtime_tools).

start_stop_app(_Config) ->
    ok = application:start(runtime_tools),
    Sup = whereis(runtime_tools_sup),
    true = is_pid(Sup),
    Ref = erlang:monitor(process,Sup),
    ok = application:stop(runtime_tools),
    receive {'DOWN', Ref, process, Sup, shutdown} -> ok end.
