%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(tools_SUITE).

-include("test_server.hrl").

%% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(application, tools).

%% Test server specific exports
-export([all/1]).
-export([init_per_testcase/2, fin_per_testcase/2]).

%% Test cases must be exported.
-export([app_test/1]).

all(doc) ->
    [];
all(suite) ->
    [app_test].

init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
fin_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%%
%%% Test cases starts here.
%%%

app_test(doc) ->
    ["Test that the .app file does not contain any `basic' errors"];
app_test(suite) ->
    [];
app_test(Config) when is_list(Config) ->
    ?line ?t:app_test(tools, tolerant).
