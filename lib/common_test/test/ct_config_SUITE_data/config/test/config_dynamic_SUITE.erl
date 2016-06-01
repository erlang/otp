%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%%% File: config_dynamic_SUITE
%%%
%%% Description:
%%% Test suite for common_test which tests the userconfig functionality
%%%-------------------------------------------------------------------
-module(config_dynamic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%% This suite will be run with dynamic userconfig
%% test_driver.erl is compliant to ct_config_* callback
%% and test_server is simple server for getting runtime-changing data
%% which will return the list with the following variables:
%% localtime = the erlang:localtime() result in list [{date, Date}, {time, Time}]
%% node = erlang:node() - can be compared in the testcase
%% now = os:timestamp() - easier to compare than localtime()
%% config_server_pid - pid of the config server, should NOT change!
%% config_server_vsn - .19
%% config_server_iteration - a number of iteration config_server's loop done
%% disappearable_variable - hereAmI - will be absent on even iterations

suite() ->
    [
     {timetrap, {seconds,10}}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

all() -> [test_get_known_variable, test_localtime_update,
	  test_server_pid, test_disappearable_variable,
	  test_disappearable_variable_alias].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

% test that usual config works
test_get_known_variable(_)->
    Node = erlang:node(),
    0.19 = ct:get_config(config_server_vsn),
    Node = ct:get_config(node),
    ok.

% localtime will be updated in 5 seconds, check that
test_localtime_update(_)->
    Seconds = 5,
    LT1 = ct:reload_config(localtime),
    timer:sleep(Seconds*1000), % don't want scaling of this timer
    LT2 = ct:reload_config(localtime),
    case is_diff_ok(LT1, LT2, Seconds) of
	{false, Actual, Exp}->
	    ct:fail(io_lib:format("Time difference ~p is not ok, expected ~p", [Actual, Exp]));
	true->
	    ok
    end.

% server pid should not change
test_server_pid()->
    [{require, cfvsn, config_server_vsn}].
test_server_pid(_)->
    Pid = ct:get_config(config_server_pid),
    Pid = ct:reload_config(config_server_pid),
    Vsn = ct:get_config(config_server_vsn),
    % aliases remain after config reloading
    Vsn = ct:get_config(cfvsn),
    ok.

% test that variables may disappear from the config_2_SUITE
test_disappearable_variable(_)->
    % ask CT for config_server_iteration variable
    Iter = ct:reload_config(config_server_iteration),
    % here we should reload this variable in case it's odd
    if Iter rem 2 == 1->
	Iter2 = ct:reload_config(config_server_iteration),
	Iter2 = Iter+1;
	true->ok
    end,
    % now disappearable_variable should be in place
    hereAmI = ct:get_config(disappearable_variable),
    % and now it should disappear
    undefined = ct:reload_config(disappearable_variable).

% alias of disappearable_variable should disappear too
test_disappearable_variable_alias(_)->
    % the same rules apply for this testcase as for previous one
    Iter = ct:reload_config(config_server_iteration),
    Iter2 = if
	Iter rem 2 == 1 ->
	    NewIter = ct:reload_config(config_server_iteration),
	    NewIter = Iter+1;
	true->
	    Iter
    end,
    ct:require(diav, disappearable_variable),
    hereAmI = ct:get_config(disappearable_variable),
    hereAmI = ct:get_config(diav),
    ct:reload_config(disappearable_variable),
    undefined = ct:get_config(disappearable_variable),
    % after reloading, it's even again
    Iter3=ct:get_config(config_server_iteration),
    Iter3 = Iter2+1,
    % and alias does not exist
    undefined = ct:get_config(diav).

my_dt_to_datetime([{date, D},{time, T}])->
    {D, T}.

is_diff_ok(DT1, DT2, Seconds)->
    GS1 = calendar:datetime_to_gregorian_seconds(my_dt_to_datetime(DT1)),
    GS2 = calendar:datetime_to_gregorian_seconds(my_dt_to_datetime(DT2)),
    ct:log("Checking diff~n"
	   "DT1: ~p, gregorian seconds: ~p~n"
	   "DT2: ~p, gregorian seconds: ~p~n"
	   "Diff: ~p",
	   [DT1,GS1,DT2,GS2,GS2-GS1]),
    if
	GS2-GS1 > Seconds+Seconds/2;
	GS2-GS1 < Seconds-Seconds/2->
	    {false, GS2-GS1, Seconds};
	true->
	    true
    end.
