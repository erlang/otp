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
		 curr_suite, curr_suite_ts, curr_group = [], curr_tc,
		 curr_log_dir, timer, tc_log,
		 test_cases = [],
		 test_suites = [] }).

-record(testcase, { log, group, classname, name, time, failure, timestamp }).
-record(testsuite, { errors, failures, hostname, name, tests,
		     time, timestamp, id, package,
		     properties, testcases }).

id(Opts) ->
    filename:absname(proplists:get_value(path, Opts, "junit_report.xml")).

init(Path, Opts) ->
    {ok, Host} = inet:gethostname(),
    #state{ filepath = Path,
	    hostname = proplists:get_value(hostname,Opts,Host),
	    package = proplists:get_value(package,Opts),
	    axis = proplists:get_value(axis,Opts,[]),
	    properties = proplists:get_value(properties,Opts,[]),
	    timer = now() }.

pre_init_per_suite(Suite,Config,#state{ test_cases = [] } = State) ->
    {Config, init_tc(State#state{ curr_suite = Suite, curr_suite_ts = now() },
		     Config) };
pre_init_per_suite(Suite,Config,State) ->
    %% Have to close the previous suite
    pre_init_per_suite(Suite,Config,close_suite(State)).

post_init_per_suite(_Suite,Config, Result, State) ->
    {Result, end_tc(init_per_suite,Config,Result,State)}.

pre_end_per_suite(_Suite,Config,State) -> {Config, init_tc(State, Config)}.

post_end_per_suite(_Suite,Config,Result,State) ->
    {Result, end_tc(end_per_suite,Config,Result,State)}.

pre_init_per_group(Group,Config,State) ->
    {Config, init_tc(State#state{ curr_group = [Group|State#state.curr_group]},
		     Config)}.

post_init_per_group(_Group,Config,Result,State) ->
    {Result, end_tc(init_per_group,Config,Result,State)}.

pre_end_per_group(_Group,Config,State) -> {Config, init_tc(State, Config)}.

post_end_per_group(_Group,Config,Result,State) ->
    NewState = end_tc(end_per_group, Config, Result, State),
    {Result, NewState#state{ curr_group = tl(NewState#state.curr_group)}}.

pre_init_per_testcase(_TC,Config,State) -> {Config, init_tc(State, Config)}.

post_end_per_testcase(TC,Config,Result,State) ->
    {Result, end_tc(TC,Config, Result,State)}.

on_tc_fail(_TC, _Res, State = #state{test_cases = []}) ->
    State;
on_tc_fail(_TC, Res, State) ->
    TCs = State#state.test_cases,
    TC = hd(TCs),
    NewTC = TC#testcase{
	      failure =
		  {fail,lists:flatten(io_lib:format("~p",[Res]))} },
    State#state{ test_cases = [NewTC | tl(TCs)]}.

on_tc_skip(Tc,{Type,_Reason} = Res, State) when Type == tc_auto_skip ->
    do_tc_skip(Res, end_tc(Tc,[],Res,init_tc(State,[])));
on_tc_skip(_Tc, _Res, State = #state{test_cases = []}) ->
    State;
on_tc_skip(_Tc, Res, State) ->
    do_tc_skip(Res, State).

do_tc_skip(Res, State) ->
    TCs = State#state.test_cases,
    TC = hd(TCs),
    NewTC = TC#testcase{
	      failure =
		  {skipped,lists:flatten(io_lib:format("~p",[Res]))} },
    State#state{ test_cases = [NewTC | tl(TCs)]}.

init_tc(State, Config) when is_list(Config) == false ->
    State#state{ timer = now(), tc_log =  "" };
init_tc(State, Config) ->
    State#state{ timer = now(),
		 tc_log =  proplists:get_value(tc_logfile, Config, [])}.

end_tc(Func, Config, Res, State) when is_atom(Func) ->
    end_tc(atom_to_list(Func), Config, Res, State);
end_tc(Name, _Config, _Res, State = #state{ curr_suite = Suite,
					    curr_group = Groups,
					    timer = TS, tc_log = Log } ) ->
    ClassName = atom_to_list(Suite),
    PGroup = string:join([ atom_to_list(Group)||
			     Group <- lists:reverse(Groups)],"."),
    TimeTakes = io_lib:format("~f",[timer:now_diff(now(),TS) / 1000000]),
    State#state{ test_cases = [#testcase{ log = Log,
					  timestamp = now_to_string(TS),
					  classname = ClassName,
					  group = PGroup,
					  name = Name,
					  time = TimeTakes,
					  failure = passed }| State#state.test_cases]}.
close_suite(#state{ test_cases = [] } = State) ->
    State;
close_suite(#state{ test_cases = TCs } = State) ->
    Total = length(TCs),
    Succ = length(lists:filter(fun(#testcase{ failure = F }) ->
				       F == passed
			       end,TCs)),
    Fail = Total - Succ,
    TimeTaken = timer:now_diff(now(),State#state.curr_suite_ts) / 1000000,
    Suite = #testsuite{ name = atom_to_list(State#state.curr_suite),
			package = State#state.package,
			time = io_lib:format("~f",[TimeTaken]),
			timestamp = now_to_string(State#state.curr_suite_ts),
			errors = Fail, tests = Total,
			testcases = lists:reverse(TCs) },
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



to_xml(#testcase{ group = Group, classname = CL, log = L, name = N, time = T, timestamp = TS, failure = F}) ->
    ["<testcase ",
     [["group=\"",Group,"\""]||Group /= ""]," "
     "name=\"",N,"\" "
     "time=\"",T,"\" "
     "timestamp=\"",TS,"\" "
     "log=\"",L,"\">",
     case F of
	 passed ->
	     [];
	 {skipped,Reason} ->
	     ["<skipped type=\"skip\" message=\"Test ",N," in ",CL,
	      " skipped!\">", sanitize(Reason),"</skipped>"];
	 {fail,Reason} ->
	     ["<failure message=\"Test ",N," in ",CL," failed!\" type=\"crash\">",
	      sanitize(Reason),"</failure>"]
     end,"</testcase>"];
to_xml(#testsuite{ package = P, hostname = H, errors = E, time = Time,
		   timestamp = TS, tests = T, name = N, testcases = Cases }) ->
    ["<testsuite ",
     [["package=\"",P,"\" "]||P /= undefined],
     [["hostname=\"",P,"\" "]||H /= undefined],
     [["name=\"",N,"\" "]||N /= undefined],
     [["time=\"",Time,"\" "]||Time /= undefined],
     [["timestamp=\"",TS,"\" "]||TS /= undefined],
     "errors=\"",integer_to_list(E),"\" "
     "tests=\"",integer_to_list(T),"\">",
     [to_xml(Case) || Case <- Cases],
     "</testsuite>"];
to_xml(#state{ test_suites = TestSuites, axis = Axis, properties = Props }) ->
    ["<testsuites>",properties_to_xml(Axis,Props),
     [to_xml(TestSuite) || TestSuite <- TestSuites],"</testsuites>"].

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
    io_lib:format("~p-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",[YY,MM,DD,HH,Mi,SS]).
