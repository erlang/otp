%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : eh_11_SUITE.erl
%%% Description : Used by ct_event_handler_SUITE to test event handling.
%%%-------------------------------------------------------------------
-module(eh_11_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [
     {timetrap,{seconds,10}}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    %% should report ok as result to event handler
    done.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    %% should report ok as result to event handler
    void.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    true.

groups() ->
    [{g1, [], [tc1, tc2, tc3, tc4, tc5]}].

all() -> 
    [{group,g1}].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

tc1(_Config) -> 
    ok.

tc2(_Config) ->
    %% should report ok as result to event handler
    42.

tc3(_Config) -> 
    {skip,"Skip"}.

tc4(_Config) -> 
    {skipped,"Skipped"}.

tc5(_Config) ->
    exit('Failing').
