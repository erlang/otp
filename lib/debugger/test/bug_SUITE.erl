%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([otp2163/1, otp4845/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

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



%% BIF exit reason.
otp2163(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    %% First compile and get the expected results:

    FileName = filename:join(DataDir, "otp2163"),
    {module,otp2163} = code:load_abs(FileName),

    {'EXIT',{badarg,[ApplyRes|_]}} = (catch otp2163:apply_test()),
    {'EXIT',{badarg,[ListRes|_]}} = (catch otp2163:list_to_atom_test()),

    %% Then interpret, and check if the results are OK.
    {module,otp2163} = int:i(FileName),

    ok = io:format("Expecting ~p", [ApplyRes]),
    {'EXIT',{badarg,[ApplyRes|_]}} = (catch otp2163:apply_test()),
    ok = io:format("Expecting ~p", [ListRes]),
    {'EXIT',{badarg,[ListRes|_]}} = (catch otp2163:list_to_atom_test()),
    ok.


%% BIF not loading and not bug compatible, OTP-4845 OTP-4859.
otp4845(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    %% First compile and get the expected results:

    FileName = filename:join(DataDir, "otp4845"),
    {module,otp4845} = code:load_abs(FileName),

    CompiledRes = (catch otp4845:test()),
    ok = io:format("Compiled ~p", [CompiledRes]),

    %% Then interpret, and check if the results are OK.
    {module,otp4845} = int:i(FileName),

    IntRes = (catch otp4845:test()),
    ok = io:format("Interpreted ~p", [IntRes]),

    CompiledRes = IntRes,
    ok.
