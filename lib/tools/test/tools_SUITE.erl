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
-module(tools_SUITE).

-include_lib("common_test/include/ct.hrl").

-define(application, tools).

%% Test server specific exports
-export([all/0, suite/0]).

%% Test cases must be exported.
-export([app_test/1, appup_test/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [app_test, appup_test].

%%%
%%% Test cases starts here.
%%%

%% Test that the .app file does not contain any `basic' errors
app_test(Config) when is_list(Config) ->
    test_server:app_test(tools, tolerant).

%% Test that the .appup file does not contain any `basic' errors
appup_test(Config) when is_list(Config) ->
    ok = test_server:appup_test(tools).
