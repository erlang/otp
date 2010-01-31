%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2010. All Rights Reserved.
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
-module(format_SUITE).
-export([all/1]).

-export([hang_1/1]).

-export([init_per_testcase/2, fin_per_testcase/2]).

-include("test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
fin_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

all(doc) ->
    ["Test cases for io:format/[2,3]."];
all(suite) ->
    [hang_1].

hang_1(doc) ->
    ["Bad args can hang (OTP-2400)"];
hang_1(suite) ->
    [];
hang_1(Config) when is_list(Config) ->
    ?line _ = (catch io:format(a, "", [])),
    ?line _ = (catch io:format({}, "", [])),
    ok.
