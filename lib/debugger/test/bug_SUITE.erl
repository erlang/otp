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

%%
-module(bug_SUITE).

-include("test_server.hrl").

-export([all/1]).

-export([ticket_tests/1]).

-export([otp2163/1, otp4845/1]).

all(suite) -> [ticket_tests].

ticket_tests(doc) -> ["Tests tickets regarding bugs"];
ticket_tests(suite) -> [otp2163, otp4845].

otp2163(doc) -> ["BIF exit reason"];
otp2163(suite) -> [];
otp2163(Config) when list(Config) ->
    ?line DataDir = ?config(data_dir, Config),

    %% First compile and get the expected results:

    ?line FileName = filename:join(DataDir, "otp2163"),
    ?line {module,otp2163} = code:load_abs(FileName),

    ?line {'EXIT',{badarg,[ApplyRes|_]}} = (catch otp2163:apply_test()),
    ?line {'EXIT',{badarg,[ListRes|_]}} = (catch otp2163:list_to_atom_test()),

    %% Then interpret, and check if the results are OK.
    ?line {module,otp2163} = int:i(FileName),

    ?line ok = io:format("Expecting ~p", [ApplyRes]),
    ?line {'EXIT',{badarg,[ApplyRes|_]}} = (catch otp2163:apply_test()),
    ?line ok = io:format("Expecting ~p", [ListRes]),
    ?line {'EXIT',{badarg,[ListRes|_]}} = (catch otp2163:list_to_atom_test()),
    ok.


otp4845(doc) -> ["BIF not loading and not bug compatible, OTP-4845 OTP-4859"];
otp4845(suite) -> [];
otp4845(Config) when list(Config) ->
    ?line DataDir = ?config(data_dir, Config),

    %% First compile and get the expected results:

    ?line FileName = filename:join(DataDir, "otp4845"),
    ?line {module,otp4845} = code:load_abs(FileName),

    ?line CompiledRes = (catch otp4845:test()),
    ?line ok = io:format("Compiled ~p", [CompiledRes]),

    %% Then interpret, and check if the results are OK.
    ?line {module,otp4845} = int:i(FileName),

    ?line IntRes = (catch otp4845:test()),
    ?line ok = io:format("Interpreted ~p", [IntRes]),

    ?line CompiledRes = IntRes,
    ok.
