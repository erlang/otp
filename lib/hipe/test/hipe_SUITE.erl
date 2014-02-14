%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
-module(hipe_SUITE).

-include_lib("test_server/include/test_server.hrl").

%% Test server specific exports
-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2,end_per_group/2]).

%% Test cases
-export([app/1, appup/1]).

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

app(suite) ->
    [];
app(doc) ->
    ["Test that the hipe app file is ok"];
app(Config) when is_list(Config) ->
    ok = ?t:app_test(hipe, tolerant).

appup(suite) ->
    [];
appup(doc) ->
    ["Test that the hipe appup file is ok"];
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(hipe).
