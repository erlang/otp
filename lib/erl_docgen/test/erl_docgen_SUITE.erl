%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
-module(erl_docgen_SUITE).

-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks, [ts_install_cth]}].

all() ->
    [app, appup].

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

%% Test that the erl_docgen app file is ok
app(Config) when is_list(Config) ->
    ok = test_server:app_test(erl_docgen).

%% Test that the erl_docgen appup file is ok
appup(Config) when is_list(Config) ->
    ok = test_server:appup_test(erl_docgen).
