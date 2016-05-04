%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
%%% File: ct_surefire_SUITE
%%%
%%% Description:
%%% Test cth_surefire hook
%%%
%%%-------------------------------------------------------------------
-module(ct_surefire_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").

-define(eh, ct_test_support_eh).

-define(url_base,"http://my.host.com/").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    DataDir = ?config(data_dir,Config),
    Hook = "fail_pre_init_per_suite.erl",
    io:format("Compiling ~p: ~p~n",
        [Hook, compile:file(Hook,[{outdir,DataDir},debug_info])]),
    ct_test_support:init_per_suite([{path_dirs,[DataDir]}|Config]).

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     default,
     absolute_path,
     relative_path,
     url,
     logdir,
     fail_pre_init_per_suite
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
default(Config) when is_list(Config) ->
    run(default,[cth_surefire],"junit_report.xml",Config).

absolute_path(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir,Config),
    Path = filename:join(PrivDir,"abspath.xml"),
    run(absolute_path,[{cth_surefire,[{path,Path}]}],Path,Config).

relative_path(Config) when is_list(Config) ->
    Path = "relpath.xml",
    run(relative_path,[{cth_surefire,[{path,Path}]}],Path,Config).

url(Config) when is_list(Config) ->
    Path = "url.xml",
    run(url,[{cth_surefire,[{url_base,?url_base},{path,Path}]}],
	Path,Config).

logdir(Config) when is_list(Config) ->
    Opts = ct_test_support:get_opts(Config),
    LogDir =
	case lists:keyfind(logdir,1,Opts) of
	    {logdir,LD} -> LD;
	    false -> ?config(priv_dir,Config)
	end,
    MyLogDir = filename:join(LogDir,"specific_logdir"),
    ensure_exists_empty(MyLogDir),
    Path = "logdir.xml",
    run(logdir,[{cth_surefire,[{path,Path}]}],Path,Config,[{logdir,MyLogDir}]).

fail_pre_init_per_suite(Config) when is_list(Config) ->
    DataDir = ?config(data_dir,Config),
    Suites = [filename:join(DataDir,"pass_SUITE"),
              filename:join(DataDir,"fail_SUITE")],
    Path = "fail_pre_init_per_suite.xml",
    run(fail_pre_init_per_suite,[fail_pre_init_per_suite,
        {cth_surefire,[{path,Path}]}],Path,Config,[],Suites).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------
run(Case,CTHs,Report,Config) ->
    run(Case,CTHs,Report,Config,[]).
run(Case,CTHs,Report,Config,ExtraOpts) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "surefire_SUITE"),
    run(Case,CTHs,Report,Config,ExtraOpts,Suite).
run(Case,CTHs,Report,Config,ExtraOpts,Suite) ->
    {Opts,ERPid} = setup([{suite,Suite},{ct_hooks,CTHs},{label,Case}|ExtraOpts],
			 Config),
    ok = execute(Case, Opts, ERPid, Config),
    LogDir =
	case lists:keyfind(logdir,1,Opts) of
	    {logdir,LD} -> LD;
	    false -> ?config(priv_dir,Config)
	end,
    Re = filename:join([LogDir,"*",Report]),
    check_xml(Case,Re).

setup(Test, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Opts1 =
	case lists:keymember(logdir,1,Test) of
	    true -> lists:keydelete(logdir,1,Opts0);
	    false -> Opts0
	end,
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts1 ++ [{event_handler,{?eh,EvHArgs}}|Test],
    ERPid = ct_test_support:start_event_receiver(Config),
    {Opts,ERPid}.

execute(Name, Opts, ERPid, Config) ->
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),
    ct_test_support:log_events(Name,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(Name),
    ct_test_support:verify_events(TestEvents, Events, Config).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
events_to_check(Test) ->
    %% 2 tests (ct:run_test + script_start) is default
    events_to_check(Test, 2).

events_to_check(_, 0) ->
    [];
events_to_check(Test, N) ->
    test_events(Test) ++ events_to_check(Test, N-1).

test_suite_events(fail_SUITE, TestStat) ->
     [{?eh,tc_start,{ct_framework,init_per_suite}},
     {?eh,tc_done,{ct_framework,init_per_suite,
                   {failed,{error,pre_init_per_suite}}}},
     {?eh,tc_auto_skip,
      {fail_SUITE,test_case,
       {failed,{ct_framework,init_per_suite,{failed,pre_init_per_suite}}}}},
     {?eh,test_stats,TestStat},
     {?eh,tc_auto_skip,
      {ct_framework,end_per_suite,
       {failed,{ct_framework,init_per_suite,{failed,pre_init_per_suite}}}}}].

test_suite_events(fail_SUITE) ->
    test_suite_events(fail_SUITE, {0,0,{0,1}});
test_suite_events(pass_SUITE) ->
     [{?eh,tc_start,{ct_framework,init_per_suite}},
     {?eh,tc_done,{ct_framework,init_per_suite,ok}},
     {?eh,tc_start,{pass_SUITE,test_case}},
     {?eh,tc_done,{pass_SUITE,test_case,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{ct_framework,end_per_suite}},
     {?eh,tc_done,{ct_framework,end_per_suite,ok}}];
test_suite_events(_) ->
    [{?eh,tc_start,{surefire_SUITE,init_per_suite}},
     {?eh,tc_done,{surefire_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{surefire_SUITE,tc_ok}},
     {?eh,tc_done,{surefire_SUITE,tc_ok,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{surefire_SUITE,tc_fail}},
     {?eh,tc_done,{surefire_SUITE,tc_fail,
		   {failed,{error,{test_case_failed,"this test should fail"}}}}},
     {?eh,test_stats,{1,1,{0,0}}},
     {?eh,tc_start,{surefire_SUITE,tc_skip}},
     {?eh,tc_done,{surefire_SUITE,tc_skip,{skipped,"this test is skipped"}}},
     {?eh,test_stats,{1,1,{1,0}}},
     {?eh,tc_start,{surefire_SUITE,tc_autoskip_require}},
     {?eh,tc_done,{surefire_SUITE,tc_autoskip_require,
		   {auto_skipped,{require_failed,'_'}}}},
     {?eh,test_stats,{1,1,{1,1}}},
     [{?eh,tc_start,{surefire_SUITE,{init_per_group,g,[]}}},
      {?eh,tc_done,{surefire_SUITE,{init_per_group,g,[]},ok}},
      {?eh,tc_start,{surefire_SUITE,tc_ok}},
      {?eh,tc_done,{surefire_SUITE,tc_ok,ok}},
      {?eh,test_stats,{2,1,{1,1}}},
      {?eh,tc_start,{surefire_SUITE,tc_fail}},
      {?eh,tc_done,{surefire_SUITE,tc_fail,
		    {failed,{error,{test_case_failed,"this test should fail"}}}}},
      {?eh,test_stats,{2,2,{1,1}}},
      {?eh,tc_start,{surefire_SUITE,tc_skip}},
      {?eh,tc_done,{surefire_SUITE,tc_skip,{skipped,"this test is skipped"}}},
      {?eh,test_stats,{2,2,{2,1}}},
      {?eh,tc_start,{surefire_SUITE,tc_autoskip_require}},
      {?eh,tc_done,{surefire_SUITE,tc_autoskip_require,
		    {auto_skipped,{require_failed,'_'}}}},
      {?eh,test_stats,{2,2,{2,2}}},
      {?eh,tc_start,{surefire_SUITE,{end_per_group,g,[]}}},
      {?eh,tc_done,{surefire_SUITE,{end_per_group,g,[]},ok}}],
     [{?eh,tc_start,{surefire_SUITE,{init_per_group,g_fail,[]}}},
      {?eh,tc_done,{surefire_SUITE,{init_per_group,g_fail,[]},
		    {failed,{error,all_cases_should_be_skipped}}}},
      {?eh,tc_auto_skip,{surefire_SUITE,{tc_ok,g_fail},
			 {failed,
			  {surefire_SUITE,init_per_group,
			   {'EXIT',all_cases_should_be_skipped}}}}},
      {?eh,test_stats,{2,2,{2,3}}},
      {?eh,tc_auto_skip,{surefire_SUITE,{end_per_group,g_fail},
			 {failed,
			  {surefire_SUITE,init_per_group,
			   {'EXIT',all_cases_should_be_skipped}}}}}],
     {?eh,tc_start,{surefire_SUITE,end_per_suite}},
     {?eh,tc_done,{surefire_SUITE,end_per_suite,ok}}].

test_events(fail_pre_init_per_suite) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,start_info,{2,2,2}}] ++
     test_suite_events(pass_SUITE) ++
     test_suite_events(fail_SUITE, {1,0,{0,1}}) ++
     [{?eh,stop_logging,[]}];
test_events(Test) ->
    [{?eh,start_logging,'_'}, {?eh,start_info,{1,1,9}}] ++
    test_suite_events(Test) ++
    [{?eh,stop_logging,[]}].

%%%-----------------------------------------------------------------
%%% Check generated xml log files
check_xml(Case,XmlRe) ->
    case filelib:wildcard(XmlRe) of
	[] ->
	    ct:fail("No xml files found with regexp ~p~n", [XmlRe]);
	[_] = Xmls when Case==absolute_path ->
	    do_check_xml(Case,Xmls);
	[_,_] = Xmls ->
	    do_check_xml(Case,Xmls)
    end.

%% Allowed structure:
%% <testsuites>
%%  <testsuite>
%%   <properties>
%%    <property/>
%%    ...
%%   </properties>
%%   <testcase>
%%    [<failure/> | <error/> | <skipped/> ]
%%   </testcase>
%%   ...
%%  </testsuite>
%%  ...
%% </testsuites>
do_check_xml(Case,[Xml|Xmls]) ->
    ct:log("Checking <a href=~p>~s</a>~n",[Xml,Xml]),
    {E,_} = xmerl_scan:file(Xml),
    Expected = events_to_result(lists:flatten(test_events(Case))),
    ParseResult = testsuites(Case,E),
    ct:log("Expecting: ~p~n",[Expected]),
    ct:log("Actual   : ~p~n",[ParseResult]),
    Expected = ParseResult,
    do_check_xml(Case,Xmls);
do_check_xml(_,[]) ->
    ok.

%% Scanning the XML to get the same type of result as events_to_result/1
testsuites(Case,#xmlElement{name=testsuites,content=TS}) ->
    %% OTP-10589 - move properties element to <testsuite>
    false = lists:keytake(properties,#xmlElement.name,TS),
    testsuite(Case,TS).

testsuite(Case,[#xmlElement{name=testsuite,content=TC,attributes=A}|TS]) ->
    TestSuiteEvents = test_suite_events(get_ts_name(A)),
    {ET,EF,ES} = events_to_numbers(lists:flatten(TestSuiteEvents)),
    {T,E,F,S} = get_numbers_from_attrs(A,false,false,false,false),
    ct:log("Expecting total:~p, error:~p, failure:~p, skipped:~p~n",[ET,0,EF,ES]),
    ct:log("Actual    total:~p, error:~p, failure:~p, skipped:~p~n",[T,E,F,S]),
    {ET,0,EF,ES} = {T,E,F,S},

    %% properties should only be there if given a options to hook
    false = lists:keytake(properties,#xmlElement.name,TC),
    %% system-out and system-err is not used by common_test
    false = lists:keytake('system-out',#xmlElement.name,TC),
    false = lists:keytake('system-err',#xmlElement.name,TC),
    R=testcase(Case,TC),
    [R|testsuite(Case,TS)];
testsuite(_Case,[]) ->
    [].

testcase(url=Case,[#xmlElement{name=testcase,attributes=A,content=C}|TC]) ->
    R = failed_or_skipped(C),
    case R of
	[s] ->
	    case lists:keyfind(url,#xmlAttribute.name,A) of
		false -> ok;
		#xmlAttribute{value=UrlAttr} ->
		    lists:keyfind(url,#xmlAttribute.name,A),
		    true = lists:prefix(?url_base,UrlAttr)
	    end;
	_ ->
	    #xmlAttribute{value=UrlAttr} =
		lists:keyfind(url,#xmlAttribute.name,A),
	    true = lists:prefix(?url_base,UrlAttr)
    end,
    [R|testcase(Case,TC)];
testcase(Case,[#xmlElement{name=testcase,attributes=A,content=C}|TC]) ->
    false = lists:keyfind(url,#xmlAttribute.name,A),
    R = failed_or_skipped(C),
    [R|testcase(Case,TC)];
testcase(_Case,[]) ->
    [].

failed_or_skipped([#xmlElement{name=failure}|E]) ->
    [f|failed_or_skipped(E)];
failed_or_skipped([#xmlElement{name=error}|E]) ->
    [e|failed_or_skipped(E)];
failed_or_skipped([#xmlElement{name=skipped}|E]) ->
    [s|failed_or_skipped(E)];
failed_or_skipped([]) ->
    [].

%% Using the expected events to produce the expected result of the XML scanning.
%% The result is a list of test suites:
%% Testsuites = [Testsuite]
%% Testsuite = [Testcase]
%% Testcase = [] | [f] | [s], indicating ok, failed and skipped respectively
events_to_result(E) ->
    events_to_result(E, []).

events_to_result([{?eh,tc_auto_skip,{_Suite,init_per_suite,_}}|E], Result) ->
    {Suite,Rest} = events_to_result1(E),
    events_to_result(Rest, [[[s]|Suite]|Result]);
events_to_result([{?eh,tc_done,{_Suite,init_per_suite,R}}|E], Result) ->
    {Suite,Rest} = events_to_result1(E),
    events_to_result(Rest, [[result(R)|Suite]|Result]);
events_to_result([_|E], Result) ->
    events_to_result(E, Result);
events_to_result([], Result) ->
    Result.

events_to_result1([{?eh,tc_auto_skip,{_Suite, end_per_suite,_}}|E]) ->
    {[[s]],E};
events_to_result1([{?eh,tc_done,{_Suite, end_per_suite,R}}|E]) ->
    {[result(R)],E};
events_to_result1([{?eh,tc_done,{_Suite,_Case,R}}|E]) ->
    {Suite,Rest} = events_to_result1(E),
    {[result(R)|Suite],Rest};
events_to_result1([{?eh,tc_auto_skip,_}|E]) ->
    {Suite,Rest} = events_to_result1(E),
    {[[s]|Suite],Rest};
events_to_result1([_|E]) ->
    events_to_result1(E).

result(ok) ->[];
result({skipped,_}) -> [s];
result({auto_skipped,_}) -> [s];
result({failed,_}) -> [f].

%% Using the expected events' last test_stats element to produce the
%% expected number of totla, errors, failed and skipped testcases.
events_to_numbers(E) ->
    RevE = lists:reverse(E),
    {?eh,test_stats,{Ok,F,{US,AS}}} = lists:keyfind(test_stats,2,RevE),
    {Ok+F+US+AS,F,US+AS}.

get_numbers_from_attrs([#xmlAttribute{name=tests,value=X}|A],false,E,F,S) ->
    get_numbers_from_attrs(A,list_to_integer(X),E,F,S);
get_numbers_from_attrs([#xmlAttribute{name=errors,value=X}|A],T,false,F,S) ->
    get_numbers_from_attrs(A,T,list_to_integer(X),F,S);
get_numbers_from_attrs([#xmlAttribute{name=failures,value=X}|A],T,E,false,S) ->
    get_numbers_from_attrs(A,T,E,list_to_integer(X),S);
get_numbers_from_attrs([#xmlAttribute{name=skipped,value=X}|A],T,E,F,false) ->
    get_numbers_from_attrs(A,T,E,F,list_to_integer(X));
get_numbers_from_attrs([_|A],T,E,F,S) ->
    get_numbers_from_attrs(A,T,E,F,S);
get_numbers_from_attrs([],T,E,F,S) ->
    {T,E,F,S}.

ensure_exists_empty(Dir) ->
    case file:list_dir(Dir) of
	{error,enoent} ->
	    file:make_dir(Dir);
	{ok,Files} ->
	    del_files(Dir,Files)
    end.

del_files(Dir,[F0|Fs] ) ->
    F = filename:join(Dir,F0),
    case file:read_file_info(F) of
	{ok,#file_info{type=directory}} ->
	    {ok,Files} = file:list_dir(F),
	    del_files(F,Files),
	    file:del_dir(F),
	    del_files(Dir,Fs);
	_ ->
	    file:delete(F),
	    del_files(Dir,Fs)
    end;
del_files(_,[]) ->
    ok.

get_ts_name(Attributes) ->
    {_,name,_,_,_,_,_,_,Name,_} = lists:keyfind(name, 2, Attributes),
    list_to_atom(Name).
