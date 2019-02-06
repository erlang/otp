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
-module(hipe_SUITE).

-export([all/0, groups/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
	 app/0, app/1, appup/0, appup/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [app, appup].

groups() ->
    [].

init_per_suite(Config) ->
    case erlang:system_info(hipe_architecture) of
        undefined -> {skip, "HiPE not available or enabled"};
        _ -> Config
    end.

end_per_suite(_Config) ->
     ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

app() ->
    [{doc, "Test that the hipe app file is ok"}].
app(Config) when is_list(Config) ->
    ok = ?t:app_test(hipe, tolerant).

appup() ->
    [{doc, "Test that the hipe appup file is ok"}].
appup(Config) when is_list(Config) ->
    AppupFile = "hipe.appup",
    AppupPath = filename:join([code:lib_dir(hipe), "ebin", AppupFile]),
    {ok, [{_Vsn, [], []}]} = file:consult(AppupPath).
