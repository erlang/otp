%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%%----------------------------------------------------------------
%%% Purpose: Test the support for application upgrade/code_change test
%%%-----------------------------------------------------------------
-module(release_test_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(APP,runtime_tools). % "randomly" selected 'application under test'

%%
%% all/1
%%
all() ->
    [minor,
     major,
     major_fail_init,
     major_fail_upgraded,
     major_fail_downgraded,
     major_fail_no_init].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(major_fail_no_init, Config) ->
    Config;
init_per_testcase(_Case, Config) ->
    ct_release_test:init(Config).
end_per_testcase(_Case, Config) ->
    ct_release_test:cleanup(Config).

%%%-----------------------------------------------------------------
%%% Test cases
minor(Config) ->
    ct_release_test:upgrade(?APP,minor,{?MODULE,[]},Config).

major(Config) ->
    ct_release_test:upgrade(?APP,major,{?MODULE,[]},Config).

major_fail_init(Config) ->
    try ct_release_test:upgrade(?APP,major,{?MODULE,fail_init},Config)
    catch exit:{test_case_failed,
		{test_upgrade_callback,_Mod,_Func,_Args,
		 {'EXIT',{test_case_failed,upgrade_init_failed}}}} ->
	    ok
    end.

major_fail_upgraded(Config) ->
    try ct_release_test:upgrade(?APP,major,{?MODULE,fail_upgraded},Config)
    catch exit:{test_case_failed,
		{test_upgrade_callback,_Mod,_Func,_Args,
		 {'EXIT',{test_case_failed,upgrade_upgraded_failed}}}} ->
	    ok
    end.

major_fail_downgraded(Config) ->
    try ct_release_test:upgrade(?APP,major,{?MODULE,fail_downgraded},Config)
    catch exit:{test_case_failed,
		{test_upgrade_callback,_Mod,_Func,_Args,
		 {'EXIT',{test_case_failed,upgrade_downgraded_failed}}}} ->
	    ok
    end.

major_fail_no_init(Config) ->
    try ct_release_test:upgrade(?APP,major,[],Config)
    catch exit:{test_case_failed,"ct_release_test:init/1 not run"} ->
	    ok
    end.

%%%-----------------------------------------------------------------
%%% ct_release_test callbacks

%% Version numbers are checked by ct_release_test, so there is nothing
%% more to check here...
upgrade_init(CtData,fail_init) ->
    ct:fail(upgrade_init_failed);
upgrade_init(CtData,State) ->
    {ok,{FromVsn,ToVsn}} = ct_release_test:get_app_vsns(CtData,?APP),
    case ct_release_test:get_appup(CtData,?APP) of
	{ok,{FromVsn,ToVsn,UpInstrs,DownInstrs}} ->
	    io:format("Upgrade/downgrade ~p: ~p <--> ~p~n"
		      "Upgrade instructions:   ~p~n"
		      "Downgrade instructions: ~p",
		      [?APP,FromVsn,ToVsn,UpInstrs,DownInstrs]);
	{error,{vsn_not_found,_}} when FromVsn==ToVsn ->
	    io:format("No upgrade test for ~p, same version",[?APP])
    end,
    State.
upgrade_upgraded(CtData,fail_upgraded) ->
    ct:fail(upgrade_upgraded_failed);
upgrade_upgraded(_CtData,State) ->
    State.
upgrade_downgraded(CtData,fail_downgraded) ->
    ct:fail(upgrade_downgraded_failed);
upgrade_downgraded(_CtData,State) ->
    State.
