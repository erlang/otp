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
-module(edoc_SUITE).

-include_lib("test_server/include/test_server.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

%% Test cases
-export([app/1,appup/1,build_std/1,build_map_module/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [app,appup,build_std,build_map_module].

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

%% Test that the .app file does not contain any `basic' errors
app(Config) when is_list(Config) ->
    ok = ?t:app_test(edoc).

%% Test that the .appup file does not contain any `basic' errors
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(edoc).

build_std(suite) -> [];
build_std(doc) -> ["Build some documentation using standard EDoc layout"];
build_std(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Overview1 = filename:join(DataDir, "overview.edoc"),
    Overview2 = filename:join(DataDir, "overview.syntax_tools"),
    PrivDir = ?config(priv_dir, Config),

    ok = edoc:application(edoc, [{overview, Overview1},
	    {def, {vsn,"TEST"}},
	    {dir, PrivDir}]),

    ok = edoc:application(syntax_tools, [{overview, Overview2},
	    {def, {vsn,"TEST"}},
	    {dir, PrivDir}]),

    ok = edoc:application(xmerl, [{dir, PrivDir}]),
    ok.

build_map_module(Config) when is_list(Config) ->
    DataDir  = ?config(data_dir, Config),
    PrivDir  = ?config(priv_dir, Config),
    Filename = filename:join(DataDir, "map_module.erl"),
    ok = edoc:file(Filename, [{dir, PrivDir}]),
    ok.
