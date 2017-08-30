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
%%% File: ct_smoke_test_SUITE.erl
%%%
%%% Description: The purpose of this suite is to test that Common Test
%%% can be started properly and that simple dummy test suites are
%%% executed without unexpected crashes or hangings. The suites used
%%% for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_smoke_test_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

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
    Config1 = ct_test_support:init_per_suite(Config),
    Config1.

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
    [dir1, dir2, dir1_2, suite11, suite21, suite11_21,
     tc111, tc211, tc111_112].

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

dir1(doc) -> 
    [];
dir1(suite) -> 
    [];
dir1(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Happy1 = filename:join(DataDir, "happy_1"),
    Happy1Cfg = filename:join(Happy1, "cfg/config1.cfg"),

    Opts0 = ct_test_support:get_opts(Config),
    Opts = eh_opts(Config) ++ Opts0 ++ [{config,Happy1Cfg}, {dir,Happy1}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(dir1, 
			       ct_test_support:reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(dir1),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% 

dir2(doc) -> 
    [];
dir2(suite) -> 
    [];
dir2(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Happy2 = filename:join(DataDir, "happy_2_test"),
    Happy2Cfg = filename:join(DataDir, "happy_2_cfg/config1.cfg"),

    Opts0 = ct_test_support:get_opts(Config),
    Opts = eh_opts(Config) ++ Opts0 ++ [{config,Happy2Cfg}, {dir,Happy2}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),
    
    ct_test_support:log_events(dir2, 
			       ct_test_support:reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(dir2),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% 

dir1_2(doc) -> 
    [];
dir1_2(suite) -> 
    [];
dir1_2(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Happy1 = filename:join(DataDir, "happy_1"),
    Happy2 = filename:join(DataDir, "happy_2_test"),
    Happy1Cfg = filename:join(Happy1, "cfg/config1.cfg"),

    Opts0 = ct_test_support:get_opts(Config),
    Opts = eh_opts(Config) ++ Opts0 ++ [{config,Happy1Cfg}, {dir,[Happy1,Happy2]}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),
    
    ct_test_support:log_events(dir1_2, 
			       ct_test_support:reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(dir1_2),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% 

suite11(doc) -> 
    [];
suite11(suite) -> 
    [];
suite11(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Happy1 = filename:join(DataDir, "happy_1"),
    Suite = filename:join(Happy1, "test/happy_11_SUITE"),
    Happy1Cfg = filename:join(Happy1, "cfg/config1.cfg"),

    Opts0 = ct_test_support:get_opts(Config),
    Opts = eh_opts(Config) ++ Opts0 ++ [{config,Happy1Cfg}, {suite,Suite}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),
    
    ct_test_support:log_events(suite11, 
			       ct_test_support:reformat(Events, ?eh),
			       ?config(priv_dir, Config), Opts),

    TestEvents = events_to_check(suite11),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% 

suite21(doc) -> 
    [];
suite21(suite) -> 
    [];
suite21(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Suite = filename:join(DataDir, "happy_2_test/happy_21_SUITE"),
    Happy2Cfg = filename:join(DataDir, "happy_2_cfg/config1.cfg"),

    Opts0 = ct_test_support:get_opts(Config),
    Opts = eh_opts(Config) ++ Opts0 ++ [{config,Happy2Cfg}, {suite,Suite}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),
    
    ct_test_support:log_events(suite21, 
			       ct_test_support:reformat(Events, ?eh),
			       ?config(priv_dir, Config), Opts),

    TestEvents = events_to_check(suite21),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% 

suite11_21(doc) -> 
    [];
suite11_21(suite) -> 
    [];
suite11_21(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Happy1 = filename:join(DataDir, "happy_1"),
    Suite11 = filename:join(Happy1, "test/happy_11_SUITE"),
    Happy1Cfg = filename:join(Happy1, "cfg/config1.cfg"),
    Suite21 = filename:join(DataDir, "happy_2_test/happy_21_SUITE"),

    Opts0 = ct_test_support:get_opts(Config),
    Opts = eh_opts(Config) ++ Opts0 ++ [{config,Happy1Cfg}, {suite,[Suite11,Suite21]}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),
    
    ct_test_support:log_events(suite11_21, 
			       ct_test_support:reformat(Events, ?eh),
			       ?config(priv_dir, Config), Opts),

    TestEvents = events_to_check(suite11_21),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% 

tc111(doc) -> 
    [];
tc111(suite) -> 
    [];
tc111(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Happy1 = filename:join(DataDir, "happy_1"),
    Suite = filename:join(Happy1, "test/happy_11_SUITE"),
    Happy1Cfg = filename:join(Happy1, "cfg/config1.cfg"),

    Opts0 = ct_test_support:get_opts(Config),
    Opts = eh_opts(Config) ++ Opts0 ++ [{config,Happy1Cfg}, {suite,Suite}, 
					{testcase,tc1}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),
    
    ct_test_support:log_events(tc111, 
			       ct_test_support:reformat(Events, ?eh),
			       ?config(priv_dir, Config), Opts),

    TestEvents = events_to_check(tc111),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% 

tc211(doc) -> 
    [];
tc211(suite) -> 
    [];
tc211(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Suite = filename:join(DataDir, "happy_2_test/happy_21_SUITE"),
    Happy2Cfg = filename:join(DataDir, "happy_2_cfg/config1.cfg"),

    Opts0 = ct_test_support:get_opts(Config),
    Opts = eh_opts(Config) ++ Opts0 ++ [{config,Happy2Cfg}, {suite,Suite}, 
					{testcase,tc1}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),
    
    ct_test_support:log_events(tc211, 
			       ct_test_support:reformat(Events, ?eh),
			       ?config(priv_dir, Config), Opts),

    TestEvents = events_to_check(tc211),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% 

tc111_112(doc) -> 
    [];
tc111_112(suite) -> 
    [];
tc111_112(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    Happy1 = filename:join(DataDir, "happy_1"),
    Suite = filename:join(Happy1, "test/happy_11_SUITE"),
    Happy1Cfg = filename:join(Happy1, "cfg/config1.cfg"),

    Opts0 = ct_test_support:get_opts(Config),
    Opts = eh_opts(Config) ++ Opts0 ++ [{config,Happy1Cfg}, {suite,Suite}, 
					{testcase,[tc1,tc2]}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),
    
    Events = ct_test_support:get_events(ERPid, Config),
    
    ct_test_support:log_events(tc111_112, 
			       ct_test_support:reformat(Events, ?eh),
			       ?config(priv_dir, Config), Opts),

    TestEvents = events_to_check(tc111_112),
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

events(Test) when Test == dir1 ; Test == dir2 ;
		       Test == suite11 ; Test == suite21 ->
    Suite = if Test == dir1 ; Test == suite11 -> happy_11_SUITE;
	       true -> happy_21_SUITE
	    end,
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,8}},
     {?eh,tc_start,{Suite,init_per_suite}},
     {?eh,tc_done,{Suite,init_per_suite,ok}},
     {?eh,tc_start,{Suite,tc1}},
     {?eh,tc_done,{Suite,tc1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{Suite,tc2}},
     {?eh,tc_done,{Suite,tc2,ok}},
     {?eh,test_stats,{2,0,{0,0}}},
     {?eh,tc_start,{Suite,seq1_tc1}},
     {?eh,tc_done,{Suite,seq1_tc1,ok}},
     {?eh,test_stats,{3,0,{0,0}}},
     {?eh,tc_start,{Suite,seq1_tc2}},
     {?eh,tc_done,{Suite,seq1_tc2,ok}},
     {?eh,test_stats,{4,0,{0,0}}},
     {?eh,tc_start,{Suite,tc3}},
     {?eh,tc_done,{Suite,tc3,ok}},
     {?eh,test_stats,{5,0,{0,0}}},
     {?eh,tc_start,{Suite,seq2_tc1}},
     {?eh,tc_done,{Suite,seq2_tc1,ok}},
     {?eh,test_stats,{6,0,{0,0}}},
     {?eh,tc_start,{Suite,seq2_tc2}},
     {?eh,tc_done,{Suite,seq2_tc2,ok}},
     {?eh,test_stats,{7,0,{0,0}}},
     {?eh,tc_start,{Suite,tc4}},
     {?eh,tc_done,
      {Suite,tc4,{skipped,"Skipping this one"}}},
     {?eh,test_stats,{7,0,{1,0}}},
     {?eh,tc_start,{Suite,end_per_suite}},
     {?eh,tc_done,{Suite,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];
events(Test) when Test == dir1_2 ; Test == suite11_21 ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,16}},
     {?eh,tc_start,{happy_11_SUITE,init_per_suite}},
     {?eh,tc_done,{happy_11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{happy_11_SUITE,tc1}},
     {?eh,tc_done,{happy_11_SUITE,tc1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{happy_11_SUITE,tc2}},
     {?eh,tc_done,{happy_11_SUITE,tc2,ok}},
     {?eh,test_stats,{2,0,{0,0}}},
     {?eh,tc_start,{happy_11_SUITE,seq1_tc1}},
     {?eh,tc_done,{happy_11_SUITE,seq1_tc1,ok}},
     {?eh,test_stats,{3,0,{0,0}}},
     {?eh,tc_start,{happy_11_SUITE,seq1_tc2}},
     {?eh,tc_done,{happy_11_SUITE,seq1_tc2,ok}},
     {?eh,test_stats,{4,0,{0,0}}},
     {?eh,tc_start,{happy_11_SUITE,tc3}},
     {?eh,tc_done,{happy_11_SUITE,tc3,ok}},
     {?eh,test_stats,{5,0,{0,0}}},
     {?eh,tc_start,{happy_11_SUITE,seq2_tc1}},
     {?eh,tc_done,{happy_11_SUITE,seq2_tc1,ok}},
     {?eh,test_stats,{6,0,{0,0}}},
     {?eh,tc_start,{happy_11_SUITE,seq2_tc2}},
     {?eh,tc_done,{happy_11_SUITE,seq2_tc2,ok}},
     {?eh,test_stats,{7,0,{0,0}}},
     {?eh,tc_start,{happy_11_SUITE,tc4}},
     {?eh,tc_done,
      {happy_11_SUITE,tc4,{skipped,"Skipping this one"}}},
     {?eh,test_stats,{7,0,{1,0}}},
     {?eh,tc_start,{happy_11_SUITE,end_per_suite}},
     {?eh,tc_done,{happy_11_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{happy_21_SUITE,init_per_suite}},
     {?eh,tc_done,{happy_21_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{happy_21_SUITE,tc1}},
     {?eh,tc_done,{happy_21_SUITE,tc1,ok}},
     {?eh,test_stats,{8,0,{1,0}}},
     {?eh,tc_start,{happy_21_SUITE,tc2}},
     {?eh,tc_done,{happy_21_SUITE,tc2,ok}},
     {?eh,test_stats,{9,0,{1,0}}},
     {?eh,tc_start,{happy_21_SUITE,seq1_tc1}},
     {?eh,tc_done,{happy_21_SUITE,seq1_tc1,ok}},
     {?eh,test_stats,{10,0,{1,0}}},
     {?eh,tc_start,{happy_21_SUITE,seq1_tc2}},
     {?eh,tc_done,{happy_21_SUITE,seq1_tc2,ok}},
     {?eh,test_stats,{11,0,{1,0}}},
     {?eh,tc_start,{happy_21_SUITE,tc3}},
     {?eh,tc_done,{happy_21_SUITE,tc3,ok}},
     {?eh,test_stats,{12,0,{1,0}}},
     {?eh,tc_start,{happy_21_SUITE,seq2_tc1}},
     {?eh,tc_done,{happy_21_SUITE,seq2_tc1,ok}},
     {?eh,test_stats,{13,0,{1,0}}},
     {?eh,tc_start,{happy_21_SUITE,seq2_tc2}},
     {?eh,tc_done,{happy_21_SUITE,seq2_tc2,ok}},
     {?eh,test_stats,{14,0,{1,0}}},
     {?eh,tc_start,{happy_21_SUITE,tc4}},
     {?eh,tc_done,
      {happy_21_SUITE,tc4,{skipped,"Skipping this one"}}},
     {?eh,test_stats,{14,0,{2,0}}},
     {?eh,tc_start,{happy_21_SUITE,end_per_suite}},
     {?eh,tc_done,{happy_21_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

events(Test) when Test == tc111 ; Test == tc211 ->
    Suite = if Test == tc111 -> happy_11_SUITE; true -> happy_21_SUITE end,
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{Suite,init_per_suite}},
     {?eh,tc_done,{Suite,init_per_suite,ok}},
     {?eh,tc_start,{Suite,tc1}},
     {?eh,tc_done,{Suite,tc1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{Suite,end_per_suite}},
     {?eh,tc_done,{Suite,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

events(tc111_112) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,2}},
     {?eh,tc_start,{happy_11_SUITE,init_per_suite}},
     {?eh,tc_done,{happy_11_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{happy_11_SUITE,tc1}},
     {?eh,tc_done,{happy_11_SUITE,tc1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{happy_11_SUITE,tc2}},
     {?eh,tc_done,{happy_11_SUITE,tc2,ok}},
     {?eh,test_stats,{2,0,{0,0}}},
     {?eh,tc_start,{happy_11_SUITE,end_per_suite}},
     {?eh,tc_done,{happy_11_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].
