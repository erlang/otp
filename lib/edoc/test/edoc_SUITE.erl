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
%%     $Id$
%%
-module(edoc_SUITE).

-include("test_server.hrl").

%% Test server specific exports
-export([all/1]).

%% Test cases
-export([build_std/1]).

all(suite) ->
    [build_std].

build_std(suite) ->
    [];
build_std(doc) ->
    ["Build some documentation using standard EDoc layout"];
build_std(Config) when is_list(Config) ->

    ?line DataDir = ?config(data_dir, Config),
    ?line Overview1 = filename:join(DataDir, "overview.edoc"),
    ?line Overview2 = filename:join(DataDir, "overview.syntax_tools"),
    ?line PrivDir = ?config(priv_dir, Config),

    ?line ok = edoc:application(edoc, [{overview, Overview1},
				       {def, {vsn,"TEST"}},
				       {dir, PrivDir}]),

    ?line ok = edoc:application(syntax_tools, [{overview, Overview2},
					       {def, {vsn,"TEST"}},
					       {dir, PrivDir}]),

    ?line ok = edoc:application(xmerl, [{dir, PrivDir}]),

    ok.
