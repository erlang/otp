%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2012-2025. All Rights Reserved.
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

%%% Common Test Framework functions handling test specifications.
%%%
%%% This module creates a junit report of the test run if plugged in
%%% as a suite_callback.

-module(cth_surefire).
-moduledoc false.

%% Suite Callbacks
-export([id/1, init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/4]).
-export([post_init_per_group/5]).
-export([pre_end_per_group/4]).
-export([post_end_per_group/5]).

-export([pre_init_per_testcase/4]).
-export([post_end_per_testcase/5]).

-export([on_tc_fail/4]).
-export([on_tc_skip/4]).

-export([terminate/1]).

%% Gen server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-behaviour(gen_server).
-behaviour(ct_hooks).

-record(state, { filepath, axis, properties, package, hostname,
		 curr_suite, curr_suite_file, curr_suite_ast, curr_suite_ts, curr_group = [],
		 curr_log_dir, timer, tc_log, url_base,
                 test_cases = [], test_suites = []}).

-record(testcase, { log, url, group, file, line, classname, name, time, result, timestamp }).
-record(testsuite, { errors, failures, skipped, hostname, name, tests,
		     time, timestamp, id, package,
		     properties, testcases, log, url }).

-define(default_report,"junit_report.xml").
-define(suite_log,"suite.log.html").

-define(now, os:timestamp()).

%% Number of dirs from log root to testcase log file.
%% ct_run.<node>.<timestamp>/<test_name>/run.<timestamp>/<tc_log>.html
-define(log_depth,3).

%% The gen server proxy wrapper API
%% The state of this hook can become very large for large test suites
%%    for example diameter_traffic_SUITE
%% so we keep the state in a separate process in order to not have to
%% copy the full state to each testcase process. Doing it this way cuts
%% the execution time of diameter_traffic_SUITE from 30 min to 5 min.
init(Path, Opts) ->
    {ok, Pid} = gen_server:start(?MODULE, [Path, Opts], []),
    {ok, Pid}.

init([Path, Opts]) ->
    ct_util:mark_process(),
    {ok, Host} = inet:gethostname(),
    {ok, #state{ filepath = Path,
                 hostname = proplists:get_value(hostname,Opts,Host),
                 package = proplists:get_value(package,Opts),
                 axis = proplists:get_value(axis,Opts,[]),
                 properties = proplists:get_value(properties,Opts,[]),
                 url_base = proplists:get_value(url_base,Opts),
                 timer = ?now }}.

handle_call({terminate, Args}, _From, State) ->
    Res = apply(?MODULE, terminate, Args ++ [State]),
    {stop, normal, Res, State};
handle_call({Function, Args}, _From, State)
  when Function =:= on_tc_fail;
       Function =:= on_tc_skip ->
    NewState = apply(?MODULE, Function, Args ++ [State]),
    {reply, ok, NewState};
handle_call({Function, Args}, _From, State) ->
    {Reply,NewState} = apply(?MODULE, Function, Args ++ [State]),
    {reply,Reply,NewState}.

%% Ignore any cast
handle_cast(_What, State) ->
    {noreply, State}.

id(Opts) ->
    case proplists:get_value(path, Opts) of
	undefined -> ?default_report;
	Path -> filename:absname(Path)
    end.

pre_init_per_suite(Suite,SkipOrFail,Proxy) when is_pid(Proxy) ->
    {gen_server:call(Proxy,{?FUNCTION_NAME, [Suite, SkipOrFail]}),Proxy};
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
    Ast =
        case beam_lib:chunks(code:which(Suite),[debug_info]) of
            {ok,{Suite,[{debug_info,
                         {debug_info_v1,
                          erl_abstract_code,
                          {Abstr,_Opts}}}]}} ->
                if Abstr =:= none ->
                        undefined;
                   true ->
                        Abstr
                end;
            _ ->
                undefined
        end,
    {Config, init_tc(State#state{ filepath = Path,
				  curr_suite = Suite,
                                  curr_suite_file = get_file(Suite),
                                  curr_suite_ast = Ast,
				  curr_suite_ts = ?now,
				  curr_log_dir = CurrLogDir},
		     Config) };
pre_init_per_suite(Suite,Config,State) ->
    %% Have to close the previous suite
    pre_init_per_suite(Suite,Config,close_suite(State)).

get_file(Suite) ->
    case beam_lib:chunks(code:which(Suite),["CInf"]) of
        {ok,{_,[{"CInf",Bin}]}} ->
            Source = proplists:get_value(source,binary_to_term(Bin)),
            case filelib:is_file(Source) of
                true ->
                    Source;
                false ->
                    undefined
            end;
        _ ->
            undefined
    end.

post_init_per_suite(Suite,Config, Result, Proxy) when is_pid(Proxy) ->
    {gen_server:call(Proxy,{?FUNCTION_NAME, [Suite, Config, Result]}),Proxy};
post_init_per_suite(_Suite,Config, Result, State) ->
    {Result, end_tc(init_per_suite,Config,Result,State)}.

pre_end_per_suite(Suite,Config,Proxy) when is_pid(Proxy) ->
    {gen_server:call(Proxy,{?FUNCTION_NAME, [Suite, Config]}),Proxy};
pre_end_per_suite(_Suite,Config,State) ->
    {Config, init_tc(State, Config)}.

post_end_per_suite(Suite,Config,Result,Proxy) when is_pid(Proxy) ->
    {gen_server:call(Proxy,{?FUNCTION_NAME, [Suite, Config, Result]}),Proxy};
post_end_per_suite(_Suite,Config,Result,State) ->
    {Result, end_tc(end_per_suite,Config,Result,State)}.

pre_init_per_group(Suite,Group,Config,Proxy) when is_pid(Proxy) ->
    {gen_server:call(Proxy,{?FUNCTION_NAME, [Suite, Group, Config]}),Proxy};
pre_init_per_group(_Suite,Group,Config,State) ->
    {Config, init_tc(State#state{ curr_group = [Group|State#state.curr_group]},
		     Config)}.

post_init_per_group(Suite,Group,Config,Result,Proxy) when is_pid(Proxy) ->
    {gen_server:call(Proxy,{?FUNCTION_NAME, [Suite, Group, Config, Result]}),Proxy};
post_init_per_group(_Suite,_Group,Config,Result,State) ->
    NewState = end_tc(init_per_group,Config,Result,State),
    case Result of
        {skip, _} ->
            %% on_tc_skip will be called which will re-add this group
            {Result, NewState#state{ curr_group = tl(NewState#state.curr_group) }};
        _ ->
            {Result, NewState}
    end.

pre_end_per_group(Suite,Group,Config,Proxy) when is_pid(Proxy) ->
    {gen_server:call(Proxy,{?FUNCTION_NAME, [Suite, Group, Config]}),Proxy};
pre_end_per_group(_Suite,_Group,Config,State) ->
    {Config, init_tc(State, Config)}.

post_end_per_group(Suite,Group,Config,Result,Proxy) when is_pid(Proxy) ->
    {gen_server:call(Proxy,{?FUNCTION_NAME, [Suite,Group,Config,Result]}),Proxy};
post_end_per_group(_Suite,_Group,Config,Result,State) ->
    NewState = end_tc(end_per_group, Config, Result, State),
    {Result, NewState#state{ curr_group = tl(NewState#state.curr_group)}}.

pre_init_per_testcase(Suite,TC,Config,Proxy) when is_pid(Proxy) ->
    {gen_server:call(Proxy,{?FUNCTION_NAME, [Suite,TC,Config]}),Proxy};
pre_init_per_testcase(_Suite,_TC,Config,State) ->
    {Config, init_tc(State, Config)}.

post_end_per_testcase(Suite,TC,Config,Result,Proxy) when is_pid(Proxy) ->
    {gen_server:call(Proxy,{?FUNCTION_NAME, [Suite, TC, Config, Result]}),Proxy};
post_end_per_testcase(_Suite,TC,Config,Result,State) ->
    {Result, end_tc(TC,Config, Result,State)}.

on_tc_fail(Suite,TC,Result,Proxy) when is_pid(Proxy) ->
    _ = gen_server:call(Proxy,{?FUNCTION_NAME, [Suite, TC, Result]}),
    Proxy;
on_tc_fail(_Suite,_TC, _Res, State = #state{test_cases = []}) ->
    State;
on_tc_fail(Suite, _TC, Res, State) ->
    TCs = State#state.test_cases,
    TC = hd(TCs),
    Line = case get_line_from_result(Suite, Res) of
               undefined ->
                   TC#testcase.line;
               L -> L
           end,
    NewTC = TC#testcase{
              line = Line,
	      result =
		  {fail,lists:flatten(io_lib:format("~tp",[Res]))} },
    State#state{ test_cases = [NewTC | tl(TCs)]}.

get_line_from_result(Suite, {_Error, [{__M,__F,__A,__I}|_] = StackTrace}) ->
    case lists:filter(fun({Mod, _Func, _Arity, _Info}) ->
                               Mod =:= Suite
                       end, StackTrace) of
        [{Suite,_F,_A, Info} | _ ] ->
            proplists:get_value(line, Info);
        _ ->
            undefined
    end;
get_line_from_result(_, _) ->
    undefined.

on_tc_skip(Suite,TC,Result,Proxy) when is_pid(Proxy) ->
    _ = gen_server:call(Proxy,{?FUNCTION_NAME, [Suite,TC,Result]}),
    Proxy;
on_tc_skip(Suite,{init_per_group,GrName}, Res, State) ->
    on_tc_skip(Suite,init_per_group, Res, State#state{ curr_group = [GrName | State#state.curr_group]});
on_tc_skip(Suite,{end_per_group,_GrName}, Res, State) ->
    NewState = on_tc_skip(Suite,end_per_group, Res, State),
    NewState#state{ curr_group = tl(State#state.curr_group)};
on_tc_skip(Suite,{ConfigFunc,GrName}, Res, State) ->
    if GrName =:= hd(State#state.curr_group) ->
            on_tc_skip(Suite,ConfigFunc, Res, State);
       true ->
            NewState = on_tc_skip(Suite,ConfigFunc, Res,
                                  State#state{ curr_group = [GrName | State#state.curr_group]}),
            NewState#state{ curr_group = tl(NewState#state.curr_group)}
    end;                          
on_tc_skip(Suite,Tc, Res, State0) ->
    TcStr = atom_to_list(Tc),
    CurrGroup = make_group_string(State0#state.curr_group),
    State1 =
	case State0#state.test_cases of
	    [#testcase{name=TcStr,group=CurrGroup}|TCs] ->
		State0#state{test_cases=TCs};
	    _ ->
		State0
	end,
    State = end_tc(Tc,[],Res,init_tc(set_suite(Suite,State1),[])),
    do_tc_skip(Res, State).

do_tc_skip(Res, State) ->
    TCs = State#state.test_cases,
    TC = hd(TCs),
    NewTC = TC#testcase{
	      result =
		  {skipped,lists:flatten(io_lib:format("~tp",[Res]))} },
    State#state{ test_cases = [NewTC | tl(TCs)]}.

init_tc(State, Config) when is_list(Config) == false ->
    State#state{ timer = ?now, tc_log =  "" };
init_tc(State, Config) ->
    State#state{ timer = ?now,
		 tc_log = proplists:get_value(tc_logfile, Config, [])}.

end_tc(Func, Config, Res, State) when is_atom(Func) ->
    end_tc(atom_to_list(Func), Config, Res, State);
end_tc(Func, Config, Res, State = #state{ tc_log = "" }) ->
    end_tc(Func, Config, Res, State#state{ tc_log = proplists:get_value(tc_logfile, Config) });
end_tc(Name, _Config, _Res, State = #state{ curr_suite = Suite,
					    curr_group = Groups,
					    curr_log_dir = CurrLogDir,
					    timer = TS,
					    url_base = UrlBase } ) ->
    Log =
	case State#state.tc_log of
	    undefined ->
		LowerSuiteName = string:lowercase(atom_to_list(Suite)),
		case filelib:wildcard(filename:join(CurrLogDir,LowerSuiteName++"."++Name++".*html")) of
                    [] ->
                        "";
                    [LogFile|_] ->
                        LogFile
                end;
	    LogFile ->
		LogFile
	end,
    Url = make_url(UrlBase,Log),
    ClassName = atom_to_list(Suite),
    PGroup = make_group_string(Groups),
    TimeTakes = io_lib:format("~f",[timer:now_diff(?now,TS) / 1000000]),
    State#state{ test_cases = [#testcase{ log = Log,
					  url = Url,
					  timestamp = now_to_string(TS),
					  classname = ClassName,
					  group = PGroup,
					  name = Name,
					  time = TimeTakes,
                                          file = State#state.curr_suite_file,
                                          line = get_line_from_suite(
                                                   State#state.curr_suite_ast, Name),
					  result = passed }|
			       State#state.test_cases],
		 tc_log = ""}. % so old tc_log is not set if next is on_tc_skip

make_group_string(Groups) ->
    lists:concat(lists:join(".",lists:reverse(Groups))).

set_suite(Suite,#state{curr_suite=undefined}=State) ->
    State#state{curr_suite=Suite, curr_suite_ts=?now};
set_suite(_,State) ->
    State.

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
    State#state{ curr_suite = undefined,
                 test_cases = [],
		 test_suites = [Suite | State#state.test_suites]}.

terminate(Proxy) when is_pid(Proxy) ->
    gen_server:call(Proxy,{?FUNCTION_NAME, []}),
    ok;
terminate(State = #state{ test_cases = [] }) ->
    {ok,D} = file:open(State#state.filepath,[write,{encoding,utf8}]),
    io:format(D, "<?xml version=\"1.0\" encoding= \"UTF-8\" ?>", []),
    io:format(D, "~ts", [to_xml(State)]),
    catch file:sync(D),
    catch file:close(D);
terminate(State) ->
    %% Have to close the last suite
    terminate(close_suite(State)).

get_line_from_suite(undefined, _TC) ->
    undefined;
get_line_from_suite(Abstr, TC) ->
    case [Anno || {function,Anno,Name,1,_} <- Abstr, TC =:= atom_to_list(Name)] of
        [{Line,_Col}] ->
            Line;
        _ ->
            case [Anno || {function,Anno,Name,_,_} <- Abstr, TC =:= atom_to_list(Name)] of
                [{Line,_}|_] ->
                    Line;
                _ ->
                    undefined
            end
    end.


to_xml(#testcase{ group = Group, classname = CL, log = L, url = U,
                  file = File, line = Line,
                  name = N, time = T, timestamp = TS, result = R}) ->
    ["<testcase ",
     [["group=\"",Group,"\" "]||Group /= ""],
     "name=\"",N,"\" "
     "time=\"",T,"\" "
     "timestamp=\"",TS,"\" ",
     [["url=\"",U,"\" "]||U /= undefined],
     [["file=\"",File,"\" "]||File /= undefined],
     [["line=\"",integer_to_list(Line),"\" "]||Line /= undefined],
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
    UrlBase = string:trim(UrlBase0,trailing,[$/]),
    RelativeLog = get_relative_log_url(Log),
    lists:flatten(lists:join($/,[UrlBase,RelativeLog])).

get_test_root(Log) ->
    LogParts = filename:split(Log),
    filename:join(lists:sublist(LogParts,1,length(LogParts)-?log_depth)).

get_relative_log_url(Log) ->
    LogParts = filename:split(Log),
    Start = length(LogParts)-?log_depth,
    Length = ?log_depth+1,
    lists:flatten(lists:join($/,lists:sublist(LogParts,Start,Length))).

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
