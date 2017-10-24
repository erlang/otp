%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(ct_auto_clean_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) -> Config1 | {skip,Reason}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    CTHs = filelib:wildcard(filename:join(DataDir,"cth_*.erl")),
    ct:pal("CTHs: ~p",[CTHs]),
    [ct:pal("Compiling ~p: ~p",
	    [FileName,compile:file(FileName,[{outdir,DataDir},debug_info])]) ||
	FileName <- CTHs],
    ct_test_support:init_per_suite([{path_dirs,[DataDir]} | Config]).

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> void()
%%
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) -> Config1 |
%%                                                   {skip,Reason}
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> void()
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

%%--------------------------------------------------------------------
%% Function: all(Clause) -> Descr | TestCases | {skip,Reason}
%%
%% Clause = doc | suite
%%   Indicates expected return value.
%% Descr = [string()] | []
%%   String that describes the test suite.
%% TestCases = [TestCase] 
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping the test suite.
%%
%% Description: Returns a description of the test suite (doc) and a
%%              list of all test cases in the suite (suite).
%%--------------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [clean].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: TestCase(Arg) -> Descr | Spec | ok | exit() | {skip,Reason}
%%
%% Arg = doc | suite | Config
%%   Indicates expected behaviour and return value.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Descr = [string()] | []
%%   String that describes the test case.
%% Spec = [tuple()] | []
%%   A test specification.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Test case function. Returns a description of the test
%%              case (doc), then returns a test specification (suite),
%%              or performs the actual test (Config).
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% 

clean(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    ACSuite = filename:join(DataDir, "ac_SUITE"),
    Opts0 = ct_test_support:get_opts(Config),
    Opts = eh_opts(Config) ++ Opts0 ++ [{suite,ACSuite},
                                        {ct_hooks,[cth_auto_clean]}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),
    ct_test_support:log_events(?FUNCTION_NAME, 
			       ct_test_support:reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),
    TestEvents = events_to_check(?FUNCTION_NAME),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).


%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

eh_opts(Config) ->        
    Level = ?config(trace_level, Config),
    [{event_handler,{?eh,[{cbm,ct_test_support},{trace_level,Level}]}}].

events_to_check(Test) ->
    %% 2 tests (ct:run_test + script_start) is default
    events_to_check(Test, 2).

events_to_check(_, 0) ->
    [];
events_to_check(Test, N) ->
    events(Test) ++ events_to_check(Test, N-1).

events(clean) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,9}},

     {?eh,tc_start,{ac_SUITE,init_per_suite}},
     {?eh,tc_done,{ac_SUITE,init_per_suite,ok}},

     {?eh,tc_start,{ac_SUITE,tc1}},
     {?eh,tc_done,{ac_SUITE,tc1,ok}},

     {?eh,test_stats,{1,0,{0,0}}},

     {?eh,tc_start,{ac_SUITE,tc2}},
     {?eh,tc_done,{ac_SUITE,tc2,ok}},

     {?eh,test_stats,{2,0,{0,0}}},

     [{?eh,tc_start,{ac_SUITE,{init_per_group,s1,[]}}},
      {?eh,tc_done,{ac_SUITE,{init_per_group,s1,[]},ok}},

      {?eh,tc_start,{ac_SUITE,stc1}},
      {?eh,tc_done,{ac_SUITE,stc1,ok}},

      {?eh,test_stats,{3,0,{0,0}}},

      {?eh,tc_start,{ac_SUITE,stc2}},
      {?eh,tc_done,{ac_SUITE,stc2,ok}},

      {?eh,test_stats,{4,0,{0,0}}},

      {?eh,tc_start,{ac_SUITE,{end_per_group,s1,[]}}},
      {?eh,tc_done,{ac_SUITE,{end_per_group,s1,[]},ok}}],

     {parallel,
      [{?eh,tc_start,{ac_SUITE,{init_per_group,p1,[parallel]}}},
       {?eh,tc_done,{ac_SUITE,{init_per_group,p1,[parallel]},ok}},

       {?eh,tc_start,{ac_SUITE,ptc1}},
       {?eh,tc_start,{ac_SUITE,ptc2}},
       {?eh,tc_done,{ac_SUITE,ptc1,ok}},
       {?eh,test_stats,{5,0,{0,0}}},
       {?eh,tc_done,{ac_SUITE,ptc2,ok}},
       {?eh,test_stats,{6,0,{0,0}}},

       {?eh,tc_start,{ac_SUITE,{end_per_group,p1,[parallel]}}},
       {?eh,tc_done,{ac_SUITE,{end_per_group,p1,[parallel]},ok}}]},

     [{?eh,tc_start,{ac_SUITE,{init_per_group,s2,[]}}},
      {?eh,tc_done,{ac_SUITE,{init_per_group,s2,[]},ok}},

      {?eh,tc_start,{ac_SUITE,stc1}},
      {?eh,tc_done,{ac_SUITE,stc1,ok}},

      {?eh,test_stats,{7,0,{0,0}}},

      {?eh,tc_start,{ac_SUITE,stc2}},
      {?eh,tc_done,{ac_SUITE,stc2,ok}},

      {?eh,test_stats,{8,0,{0,0}}},

      {?eh,tc_start,{ac_SUITE,{end_per_group,s2,[]}}},
      {?eh,tc_done,{ac_SUITE,{end_per_group,s2,[]},ok}}],

     {?eh,tc_start,{ac_SUITE,tc1}},
     {?eh,tc_done,{ac_SUITE,tc1,ok}},

     {?eh,test_stats,{9,0,{0,0}}},

     {?eh,tc_start,{ac_SUITE,end_per_suite}},
     {?eh,tc_done,{ac_SUITE,end_per_suite,ok}},

     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].
