%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2011. All Rights Reserved.
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

%%
-module(bug_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([otp2163/1, otp4845/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, ticket_tests}].

groups() -> 
    [{ticket_tests, [], [otp2163, otp4845]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.



otp2163(doc) -> ["BIF exit reason"];
otp2163(suite) -> [];
otp2163(Config) when is_list(Config) ->
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
otp4845(Config) when is_list(Config) ->
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
