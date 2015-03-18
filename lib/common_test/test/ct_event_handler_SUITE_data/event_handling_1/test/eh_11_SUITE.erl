%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
