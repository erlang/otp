%%--------------------------------------------------------------------
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
%%--------------------------------------------------------------------

%%% @doc Common Test Framework functions handling test specifications.
%%%
%%% <p>This module creates a junit report of the test run if plugged in
%%% as a suite_callback.</p>

-module(cth_surefire).

%% Suite Callbacks
-export([id/1, init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_init_per_group/4]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3]).

-export([terminate/1]).

-record(state, { filepath, axis, properties, package, hostname,
		 curr_suite, curr_suite_ts, curr_group = [],
		 curr_log_dir, timer, tc_log, url_base,
		 test_cases = [],
		 test_suites = [] }).

-record(testcase, { log, url, group, classname, name, time, result, timestamp }).
-record(testsuite, { errors, failures, skipped, hostname, name, tests,
		     time, timestamp, id, package,
		     properties, testcases, log, url }).

-define(default_report,"junit_report.xml").
-define(suite_log,"suite.log.html").

-define(now, os:timestamp()).

%% Number of dirs from log root to testcase log file.
%% ct_run.<node>.<timestamp>/<test_name>/run.<timestamp>/<tc_log>.html
-define(log_depth,3).

id(Opts) ->
    case proplists:get_value(path, Opts) of
	undefined -> ?default_report;
	Path -> filename:absname(Path)
    end.

init(Path, Opts) ->
    {ok, Host} = inet:gethostname(),
    #state{ filepath = Path,
	    hostname = proplists:get_value(hostname,Opts,Host),
	    package = proplists:get_value(package,Opts),
	    axis = proplists:get_value(axis,Opts,[]),
	    properties = proplists:get_value(properties,Opts,[]),
	    url_base = proplists:get_value(url_base,Opts),
	    timer = ?now }.

pre_init_per_suite(Suite,SkipOrFail,#state{ test_cases = [] } = State)
  when is_tuple(SkipOrFail) ->
    {SkipOrFail, init_tc(State#state{curr_suite = Suite,
				     curr_suite_ts = ?now},
			 SkipOrFail) };
pre_init_per_suite(Suite,Config,#state{ test_cases = [] } = State) ->
    TcLog = proplists:get_value(tc_logfile,Config),
    CurrLogDir = filename:dirname(TcLog),
    Path =
	case State#state.filepath of
	    ?default_report ->
		RootDir = get_test_root(TcLog),
		filename:join(RootDir,?default_report);
	    P ->
		P
	end,
    {Config, init_tc(State#state{ filepath = Path,
				  curr_suite = Suite,
				  curr_suite_ts = ?now,
				  curr_log_dir = CurrLogDir},
		     Config) };
pre_init_per_suite(Suite,Config,State) ->
    %% Have to close the previous suite
    pre_init_per_suite(Suite,Config,close_suite(State)).

post_init_per_suite(_Suite,Config, Result, State) ->
    {Result, end_tc(init_per_suite,Config,Result,State)}.

pre_end_per_suite(_Suite,Config,State) ->
    {Config, init_tc(State, Config)}.

post_end_per_suite(_Suite,Config,Result,State) ->
    {Result, end_tc(end_per_suite,Config,Result,State)}.

pre_init_per_group(Group,Config,State) ->
    {Config, init_tc(State#state{ curr_group = [Group|State#state.curr_group]},
		     Config)}.

post_init_per_group(_Group,Config,Result,State) ->
    {Result, end_tc(init_per_group,Config,Result,State)}.

pre_end_per_group(_Group,Config,State) ->
    {Config, init_tc(State, Config)}.

post_end_per_group(_Group,Config,Result,State) ->
    NewState = end_tc(end_per_group, Config, Result, State),
    {Result, NewState#state{ curr_group = tl(NewState#state.curr_group)}}.

pre_init_per_testcase(_TC,Config,State) ->
    {Config, init_tc(State, Config)}.

post_end_per_testcase(TC,Config,Result,State) ->
    {Result, end_tc(TC,Config, Result,State)}.

on_tc_fail(_TC, _Res, State = #state{test_cases = []}) ->
    State;
on_tc_fail(_TC, Res, State) ->
    TCs = State#state.test_cases,
    TC = hd(TCs),
    NewTC = TC#testcase{
	      result =
		  {fail,lists:flatten(io_lib:format("~p",[Res]))} },
    State#state{ test_cases = [NewTC | tl(TCs)]}.

on_tc_skip({ConfigFunc,_GrName},{Type,_Reason} = Res, State0)
  when Type == tc_auto_skip; Type == tc_user_skip ->
    on_tc_skip(ConfigFunc, Res, State0);
on_tc_skip(Tc,{Type,_Reason} = Res, State0) when Type == tc_auto_skip ->
    TcStr = atom_to_list(Tc),
    State =
	case State0#state.test_cases of
	    [#testcase{name=TcStr}|TCs] ->
		State0#state{test_cases=TCs};
	    _ ->
		State0
	end,
    do_tc_skip(Res, end_tc(Tc,[],Res,init_tc(State,[])));
on_tc_skip(_Tc, _Res, State = #state{test_cases = []}) ->
    State;
on_tc_skip(_Tc, Res, State) ->
    do_tc_skip(Res, State).

do_tc_skip(Res, State) ->
    TCs = State#state.test_cases,
    TC = hd(TCs),
    NewTC = TC#testcase{
	      result =
		  {skipped,lists:flatten(io_lib:format("~p",[Res]))} },
    State#state{ test_cases = [NewTC | tl(TCs)]}.

init_tc(State, Config) when is_list(Config) == false ->
    State#state{ timer = ?now, tc_log =  "" };
init_tc(State, Config) ->
    State#state{ timer = ?now,
		 tc_log =  proplists:get_value(tc_logfile, Config, [])}.

end_tc(Func, Config, Res, State) when is_atom(Func) ->
    end_tc(atom_to_list(Func), Config, Res, State);
end_tc(Name, _Config, _Res, State = #state{ curr_suite = Suite,
					    curr_group = Groups,
					    curr_log_dir = CurrLogDir,
					    timer = TS,
					    tc_log = Log0,
					    url_base = UrlBase } ) ->
    Log =
	case Log0 of
	    "" ->
		LowerSuiteName = string:to_lower(atom_to_list(Suite)),
		filename:join(CurrLogDir,LowerSuiteName++"."++Name++".html");
	    _ ->
		Log0
	end,
    Url = make_url(UrlBase,Log),
    ClassName = atom_to_list(Suite),
    PGroup = string:join([ atom_to_list(Group)||
			     Group <- lists:reverse(Groups)],"."),
    TimeTakes = io_lib:format("~f",[timer:now_diff(?now,TS) / 1000000]),
    State#state{ test_cases = [#testcase{ log = Log,
					  url = Url,
					  timestamp = now_to_string(TS),
					  classname = ClassName,
					  group = PGroup,
					  name = Name,
					  time = TimeTakes,
					  result = passed }|
			       State#state.test_cases],
		 tc_log = ""}. % so old tc_log is not set if next is on_tc_skip
close_suite(#state{ test_cases = [] } = State) ->
    State;
close_suite(#state{ test_cases = TCs, url_base = UrlBase } = State) ->
    {Total,Fail,Skip} = count_tcs(TCs,0,0,0),
    TimeTaken = timer:now_diff(?now,State#state.curr_suite_ts) / 1000000,
    SuiteLog = filename:join(State#state.curr_log_dir,?suite_log),
    SuiteUrl = make_url(UrlBase,SuiteLog),
    Suite = #testsuite{ name = atom_to_list(State#state.curr_suite),
			package = State#state.package,
			hostname = State#state.hostname,
			time = io_lib:format("~f",[TimeTaken]),
			timestamp = now_to_string(State#state.curr_suite_ts),
			errors = 0,
			failures = Fail,
			skipped = Skip,
			tests = Total,
			testcases = lists:reverse(TCs),
			log = SuiteLog,
			url = SuiteUrl},
    State#state{ test_cases = [],
		 test_suites = [Suite | State#state.test_suites]}.

terminate(State = #state{ test_cases = [] }) ->
    {ok,D} = file:open(State#state.filepath,[write,{encoding,utf8}]),
    io:format(D, "<?xml version=\"1.0\" encoding= \"UTF-8\" ?>", []),
    io:format(D, to_xml(State), []),
    catch file:sync(D),
    catch file:close(D);
terminate(State) ->
    %% Have to close the last suite
    terminate(close_suite(State)).



to_xml(#testcase{ group = Group, classname = CL, log = L, url = U, name = N, time = T, timestamp = TS, result = R}) ->
    ["<testcase ",
     [["group=\"",Group,"\" "]||Group /= ""],
     "name=\"",N,"\" "
     "time=\"",T,"\" "
     "timestamp=\"",TS,"\" ",
     [["url=\"",U,"\" "]||U /= undefined],
     "log=\"",L,"\">",
     case R of
	 passed ->
	     [];
	 {skipped,Reason} ->
	     ["<skipped type=\"skip\" message=\"Test ",N," in ",CL,
	      " skipped!\">", sanitize(Reason),"</skipped>"];
	 {fail,Reason} ->
	     ["<failure message=\"Test ",N," in ",CL," failed!\" type=\"crash\">",
	      sanitize(Reason),"</failure>"]
     end,"</testcase>"];
to_xml(#testsuite{ package = P, hostname = H, errors = E, failures = F,
		   skipped = S, time = Time, timestamp = TS, tests = T, name = N,
		   testcases = Cases, log = Log, url = Url }) ->
    ["<testsuite ",
     [["package=\"",P,"\" "]||P /= undefined],
     "hostname=\"",H,"\" "
     "name=\"",N,"\" "
     "time=\"",Time,"\" "
     "timestamp=\"",TS,"\" "
     "errors=\"",integer_to_list(E),"\" "
     "failures=\"",integer_to_list(F),"\" "
     "skipped=\"",integer_to_list(S),"\" "
     "tests=\"",integer_to_list(T),"\" ",
     [["url=\"",Url,"\" "]||Url /= undefined],
     "log=\"",Log,"\">",
     [to_xml(Case) || Case <- Cases],
     "</testsuite>"];
to_xml(#state{ test_suites = TestSuites, axis = Axis, properties = Props }) ->
    ["<testsuites>",properties_to_xml(Axis,Props),
     [to_xml(TestSuite) || TestSuite <- TestSuites],"</testsuites>"].

properties_to_xml([],[]) ->
    [];
properties_to_xml(Axis,Props) ->
    ["<properties>",
     [["<property name=\"",Name,"\" axis=\"yes\" value=\"",Value,"\" />"] || {Name,Value} <- Axis],
     [["<property name=\"",Name,"\" value=\"",Value,"\" />"] || {Name,Value} <- Props],
     "</properties>"
    ].

sanitize([$>|T]) ->
    "&gt;" ++ sanitize(T);
sanitize([$<|T]) ->
    "&lt;" ++ sanitize(T);
sanitize([$"|T]) ->
    "&quot;" ++ sanitize(T);
sanitize([$'|T]) ->
    "&apos;" ++ sanitize(T);
sanitize([$&|T]) ->
    "&amp;" ++ sanitize(T);
sanitize([H|T]) ->
    [H|sanitize(T)];
sanitize([]) ->
    [].

now_to_string(Now) ->
    {{YY,MM,DD},{HH,Mi,SS}} = calendar:now_to_local_time(Now),
    io_lib:format("~w-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",[YY,MM,DD,HH,Mi,SS]).

make_url(undefined,_) ->
    undefined;
make_url(_,[]) ->
    undefined;
make_url(UrlBase0,Log) ->
    UrlBase = string:strip(UrlBase0,right,$/),
    RelativeLog = get_relative_log_url(Log),
    string:join([UrlBase,RelativeLog],"/").

get_test_root(Log) ->
    LogParts = filename:split(Log),
    filename:join(lists:sublist(LogParts,1,length(LogParts)-?log_depth)).

get_relative_log_url(Log) ->
    LogParts = filename:split(Log),
    Start = length(LogParts)-?log_depth,
    Length = ?log_depth+1,
    string:join(lists:sublist(LogParts,Start,Length),"/").

count_tcs([#testcase{name=ConfCase}|TCs],Ok,F,S)
  when ConfCase=="init_per_suite";
       ConfCase=="end_per_suite";
       ConfCase=="init_per_group";
       ConfCase=="end_per_group" ->
    count_tcs(TCs,Ok,F,S);
count_tcs([#testcase{result=passed}|TCs],Ok,F,S) ->
    count_tcs(TCs,Ok+1,F,S);
count_tcs([#testcase{result={fail,_}}|TCs],Ok,F,S) ->
    count_tcs(TCs,Ok,F+1,S);
count_tcs([#testcase{result={skipped,_}}|TCs],Ok,F,S) ->
    count_tcs(TCs,Ok,F,S+1);
count_tcs([#testcase{result={auto_skipped,_}}|TCs],Ok,F,S) ->
    count_tcs(TCs,Ok,F,S+1);
count_tcs([],Ok,F,S) ->
    {Ok+F+S,F,S}.
