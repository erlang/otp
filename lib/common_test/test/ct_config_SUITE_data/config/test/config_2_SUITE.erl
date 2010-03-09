%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
%%% File: ct_config_SUITE
%%%
%%% Description:
%%% Test suite for common_test which tests the userconfig functionality
%%%-------------------------------------------------------------------
-module(config_2_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%% This suite will be run with dynamic userconfig
%% test_driver.erl is compliant to ct_config_* callback
%% and test_server is simple server for getting runtime-changing data
%% which will return the list with the following variables:
%% localtime = the erlang:localtime() result in list [{date, Date}, {time, Time}]
%% node = erlang:node() - can be compared in the testcase
%% now = erlang:now() - easier to compare than localtime()
%% config_server_pid - pid of the config server, should NOT change!
%% config_server_vsn - .19

suite() ->
    [
     {timetrap, {seconds,10}}
    ].

% to get it running on development branch (without userconfig features)
% function to print full config is in the ct_util, for me it's moved to ct_config
% two following functions are only for the design period
% after merging of userconfigs to the main branch ct_config:get_all_config/0
% should be called instead
is_exported(Module, Function, Arity)->
    Exports = Module:module_info(exports),
    case lists:keyfind(Function, 1, Exports) of
	false->
	    false;
	{Function, Arity}->
	    true;
	{Function, _OtherArity}->
	    false
    end.

get_all_config()->
    case is_exported(ct_util, get_all_config, 0) of
	true->
	    {ct_util, ct_util:get_all_config()};
	false->
	    {ct_config, ct_config:get_all_config()}
    end.

init_per_suite(Config) ->
    %{Module, Cfg} = get_all_config(),
    %ct:pal("CONFIG (handled by ~p):~n~p", [Module, Cfg]),
    Config.

end_per_suite(_) ->
    ok.

all() -> [test_get_known_variable, test_localtime_update,
	  test_server_pid].

init_per_testcase(_, Config) ->
    %{Module, Cfg} = get_all_config(),
    %ct:pal("CONFIG (handled by ~p):~n~p", [Module, Cfg]),
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
    LT1 = ct:get_config(localtime),
    timer:sleep(Seconds*1000),
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

my_dt_to_datetime([{date, D},{time, T}])->
    {D, T}.

is_diff_ok(DT1, DT2, Seconds)->
    GS1 = calendar:datetime_to_gregorian_seconds(my_dt_to_datetime(DT1)),
    GS2 = calendar:datetime_to_gregorian_seconds(my_dt_to_datetime(DT2)),
    if
	GS2-GS1 > Seconds+Seconds/2;
	GS2-GS1 < Seconds-Seconds/2->
	    {false, GS2-GS1, Seconds};
	true->
	    true
    end.
