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
-module(et_SUITE).

-export([suite/0, all/0]).
-export([app/1, appup/1]).
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks, [ts_install_cth]}].

all() ->
    [app, appup].

%% Test that the et app file is ok
app(Config) when is_list(Config) ->
    ok = ?t:app_test(et).

%% Test that the et appup file is ok
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(et).
