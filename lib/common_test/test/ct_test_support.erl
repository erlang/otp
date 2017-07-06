%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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

%%% @doc Test support functions
%%%
%%% <p>This is a support module for testing the Common Test Framework.</p>
%%%
-module(ct_test_support).

-include_lib("common_test/include/ct_event.hrl").
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, init_per_suite/2, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2,
	 write_testspec/2, write_testspec/3,
	 run/2, run/3, run/4, run_ct_run_test/2, run_ct_script_start/2,
	 get_opts/1, wait_for_ct_stop/1]).

-export([handle_event/2, start_event_receiver/1, get_events/2,
	 verify_events/3, verify_events/4, reformat/2, log_events/4,
	 join_abs_dirs/2]).

-export([start_slave/3, slave_stop/1]).

-export([ct_test_halt/1, ct_rpc/2]).

-export([random_error/1]).

-export([unique_timestamp/0]).

-export([rm_dir/1]).

-include_lib("kernel/include/file.hrl").

%%%-----------------------------------------------------------------
%%% init_per_suite/1

init_per_suite(Config) ->
    init_per_suite(Config, 50).

init_per_suite(Config, Level) ->
    ScaleFactor = test_server:timetrap_scale_factor(),
    case os:type() of
	{win32, _} ->
	    %% Extend timeout to 1 hour for windows as starting node
	    %% can take a long time there
	    test_server:timetrap( 60*60*1000 * ScaleFactor );
	_ ->
	    ok
    end,
    case delete_old_logs(os:type(), Config) of
	{'EXIT',DelLogsReason} ->
	    test_server:format(0, "Failed to delete old log directories: ~tp~n",
			       [DelLogsReason]);
	_ ->
	    ok
    end,

    {Mult,Scale} = test_server_ctrl:get_timetrap_parameters(),
    test_server:format(Level, "Timetrap multiplier: ~w~n", [Mult]),
    if Scale == true ->
	    test_server:format(Level, "Timetrap scale factor: ~w~n",
			       [ScaleFactor]);
       true ->
	    ok
    end,

    start_slave(Config, Level).

start_slave(Config, Level) ->
    start_slave(ct, Config, Level).

start_slave(NodeName, Config, Level) ->
    [_,Host] = string:lexemes(atom_to_list(node()), "@"),
    test_server:format(0, "Trying to start ~s~n",
		       [atom_to_list(NodeName)++"@"++Host]),
    PR = proplists:get_value(printable_range,Config,io:printable_range()),
    case slave:start(Host, NodeName, "+pc " ++ atom_to_list(PR)) of
	{error,Reason} ->
	    test_server:fail(Reason);
	{ok,CTNode} ->
	    test_server:format(0, "Node ~p started~n", [CTNode]),
	    IsCover = test_server:is_cover(),
	    if IsCover ->
		    cover:start(CTNode);
	       true ->
		    ok
	    end,

	    DataDir = proplists:get_value(data_dir, Config),
	    PrivDir = proplists:get_value(priv_dir, Config),

	    %% PrivDir as well as directory of Test Server suites
	    %% have to be in code path on Common Test node.
	    [_ | Parts] = lists:reverse(filename:split(DataDir)),
	    TSDir = filename:join(lists:reverse(Parts)),
	    AddPathDirs = case proplists:get_value(path_dirs, Config) of
			      undefined -> [];
			      Ds -> Ds
			  end,
	    TestSupDir = filename:dirname(code:which(?MODULE)),
	    PathDirs = [PrivDir,TSDir,TestSupDir | AddPathDirs],
	    [true = rpc:call(CTNode, code, add_patha, [D]) || D <- PathDirs],
	    test_server:format(Level, "Dirs added to code path (on ~w):~n",
			       [CTNode]),
	    [io:format("~ts~n", [D]) || D <- PathDirs],
	    
	    case proplists:get_value(start_sasl, Config) of
		true ->
		    rpc:call(CTNode, application, start, [sasl]),
		    test_server:format(Level, "SASL started on ~w~n", [CTNode]);
		_ ->
		    ok
	    end,
	    TraceFile = filename:join(DataDir, "ct.trace"),
	    case file:read_file_info(TraceFile) of
		{ok,_} -> 
		    [{trace_level,0},
		     {ct_opts,[{ct_trace,TraceFile}]},
		     {ct_node,CTNode} | Config];
		_ -> 
		    [{trace_level,Level},
		     {ct_opts,[]},
		     {ct_node,CTNode} | Config]     
	    end
    end.

%%%-----------------------------------------------------------------
%%% end_per_suite/1

end_per_suite(Config) ->
    CTNode = proplists:get_value(ct_node, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    true = rpc:call(CTNode, code, del_path, [filename:join(PrivDir,"")]),
    slave_stop(CTNode),
    ok.

%%%-----------------------------------------------------------------
%%% init_per_testcase/2

init_per_testcase(_TestCase, Config) ->
    Opts = get_opts(Config),
    NetDir = proplists:get_value(net_dir, Opts),
    LogDir = join_abs_dirs(NetDir, proplists:get_value(logdir, Opts)),
    case lists:keysearch(master, 1, Config) of
	false->
	    test_server:format("See Common Test logs here:\n\n"
			       "<a href=\"file://~ts/all_runs.html\">~ts/all_runs.html</a>\n"
			       "<a href=\"file://~ts/index.html\">~ts/index.html</a>",
			       [LogDir,LogDir,LogDir,LogDir]);
	{value, _}->
	    test_server:format("See CT Master Test logs here:\n\n"
		       "<a href=\"file://~ts/master_runs.html\">~ts/master_runs.html</a>",
		       [LogDir,LogDir])
    end,
    Config.

%%%-----------------------------------------------------------------
%%% end_per_testcase/2

end_per_testcase(_TestCase, Config) ->
    CTNode = proplists:get_value(ct_node, Config),
    case wait_for_ct_stop(CTNode) of
	%% Common test was not stopped to we restart node.
	false ->
	    slave_stop(CTNode),
	    start_slave(Config,proplists:get_value(trace_level,Config)),
	    {fail, "Could not stop common_test"};
	true ->
	    ok
    end.

%%%-----------------------------------------------------------------
%%% 
write_testspec(TestSpec, Dir, Name) ->
    write_testspec(TestSpec, filename:join(Dir, Name)).

write_testspec(TestSpec, TSFile) ->
    {ok,Dev} = file:open(TSFile, [write,{encoding,utf8}]),
    [io:format(Dev, "~tp.~n", [Entry]) || Entry <- TestSpec],
    file:close(Dev),
    io:format("Test specification written to: ~tp~n", [TSFile]),
    io:format(user, "Test specification written to: ~tp~n", [TSFile]),
    TSFile.
    

%%%-----------------------------------------------------------------
%%% 

get_opts(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    TempDir = case os:getenv("TMP") of
		  false -> 
		      case os:getenv("TEMP") of
			  false ->
			      undefined;
			  Tmp ->
			      create_tmp_logdir(Tmp)
		      end;
		  Tmp -> 
		      create_tmp_logdir(Tmp)
	      end,
    LogDir =
	case os:getenv("CT_USE_TMP_DIR") of
	    false ->
		case os:type() of
		    {win32,_} ->		
			if TempDir == undefined -> PrivDir;
			   true -> TempDir
			end;
		    _ ->
			PrivDir
		end;
	    _ ->
		TempDir
	end,

    %% Copy test variables to app environment on new node
    CtTestVars =
	case init:get_argument(ct_test_vars) of
	    {ok,[Vars]} ->
		[begin {ok,Ts,_} = erl_scan:string(Str++"."),
		       {ok,Expr} = erl_parse:parse_term(Ts),
		       Expr
		 end || Str <- Vars];
	    _ ->
		[]
	end,
    %% test_server:format("Test variables added to Config: ~p\n\n",
    %%                    [CtTestVars]),
    InitOpts =
	case proplists:get_value(ct_opts, Config) of
	    undefined -> [];
	    CtOpts -> CtOpts
	end,
    [{logdir,LogDir} | InitOpts ++ CtTestVars].


%%%-----------------------------------------------------------------
%%% 
run(Opts0, Config) when is_list(Opts0) ->
    Opts =
	%% read (and override) opts from env variable, the form expected:
	%% "[{some_key1,SomeVal2}, {some_key2,SomeVal2}]"
	case os:getenv("CT_TEST_OPTS") of
	    false -> Opts0;
	    ""    -> Opts0;
	    Terms ->
		case erl_scan:string(Terms++".", 0) of
		    {ok,Tokens,_} ->
			case erl_parse:parse_term(Tokens) of
			    {ok,OROpts} ->
				Override =
				    fun(O={Key,_}, Os) ->
					    io:format(user, "ADDING START "
						      "OPTION: ~tp~n", [O]),
					    [O | lists:keydelete(Key, 1, Os)]
				    end,
				lists:foldl(Override, Opts0, OROpts);
			    _ ->
				Opts0
			end;
		    _ ->
			Opts0
		end
	end,

    %% use ct interface
    CtRunTestResult=run_ct_run_test(Opts,Config),
    %% use run_test interface (simulated)
    ExitStatus=run_ct_script_start(Opts,Config),

    check_result(CtRunTestResult,ExitStatus,Opts).

run_ct_run_test(Opts,Config) ->
    CTNode = proplists:get_value(ct_node, Config),
    Level = proplists:get_value(trace_level, Config),
    test_server:format(Level, "~n[RUN #1] Calling ct:run_test(~tp) on ~p~n",
		       [Opts, CTNode]),
    
    T0 = erlang:monotonic_time(),
    CtRunTestResult = rpc:call(CTNode, ct, run_test, [Opts]),
    T1 = erlang:monotonic_time(),
    Elapsed = erlang:convert_time_unit(T1-T0, native, milli_seconds),
    test_server:format(Level, "~n[RUN #1] Got return value ~tp after ~p ms~n",
		       [CtRunTestResult,Elapsed]),
    case rpc:call(CTNode, erlang, whereis, [ct_util_server]) of
	undefined ->
	    ok;
	_ ->
	    test_server:format(Level,
			       "ct_util_server not stopped on ~p yet, waiting 5 s...~n",
			       [CTNode]),
	    timer:sleep(5000),
	    undefined = rpc:call(CTNode, erlang, whereis, [ct_util_server])
    end,
    CtRunTestResult.

run_ct_script_start(Opts, Config) ->
    CTNode = proplists:get_value(ct_node, Config),
    Level = proplists:get_value(trace_level, Config),
    Opts1 = [{halt_with,{?MODULE,ct_test_halt}} | Opts],
    test_server:format(Level, "Saving start opts on ~p: ~tp~n",
		       [CTNode, Opts1]),
    rpc:call(CTNode, application, set_env,
	     [common_test, run_test_start_opts, Opts1]),
    test_server:format(Level, "[RUN #2] Calling ct_run:script_start() on ~p~n",
		       [CTNode]),
    T0 = erlang:monotonic_time(),
    ExitStatus = rpc:call(CTNode, ct_run, script_start, []),
    T1 = erlang:monotonic_time(),
    Elapsed = erlang:convert_time_unit(T1-T0, native, milli_seconds),
    test_server:format(Level, "[RUN #2] Got exit status value ~tp after ~p ms~n",
		       [ExitStatus,Elapsed]),
    ExitStatus.

check_result({_Ok,Failed,{_UserSkipped,_AutoSkipped}},1,_Opts)
  when Failed > 0 ->
    ok;
check_result({_Ok,0,{_UserSkipped,AutoSkipped}},ExitStatus,Opts)
  when AutoSkipped > 0 ->
    case proplists:get_value(exit_status, Opts) of
	ignore_config when ExitStatus == 1 ->
	    {error,{wrong_exit_status,ExitStatus}};
	_ ->
	    ok
    end;
check_result({error,_}=Error,2,_Opts) ->
    Error;
check_result({error,_},ExitStatus,_Opts) ->
    {error,{wrong_exit_status,ExitStatus}};
check_result({_Ok,0,{_UserSkipped,_AutoSkipped}},0,_Opts) ->
    ok;
check_result(CtRunTestResult,ExitStatus,Opts)
  when is_list(CtRunTestResult) -> % repeated testruns
    try check_result(sum_testruns(CtRunTestResult,0,0,0,0),ExitStatus,Opts)
    catch _:_ ->
	    {error,{unexpected_return_value,{CtRunTestResult,ExitStatus}}}
    end;
check_result(done,0,_Opts) ->
    %% refresh_logs return
    ok;
check_result(CtRunTestResult,ExitStatus,_Opts) ->
    {error,{unexpected_return_value,{CtRunTestResult,ExitStatus}}}.

sum_testruns([{O,F,{US,AS}}|T],Ok,Failed,UserSkipped,AutoSkipped) ->
    sum_testruns(T,Ok+O,Failed+F,UserSkipped+US,AutoSkipped+AS);
sum_testruns([],Ok,Failed,UserSkipped,AutoSkipped) ->
    {Ok,Failed,{UserSkipped,AutoSkipped}}.

run(M, F, A, Config) ->
    run({M,F,A}, [], Config).

run({M,F,A}, InitCalls, Config) ->
    CTNode = proplists:get_value(ct_node, Config),
    Level = proplists:get_value(trace_level, Config),
    lists:foreach(
      fun({IM,IF,IA}) ->
	      test_server:format(Level, "~nInit call ~w:~tw(~tp) on ~p...~n",
				 [IM, IF, IA, CTNode]),
	      Result = rpc:call(CTNode, IM, IF, IA),
	      test_server:format(Level, "~n...with result: ~tp~n", [Result])
      end, InitCalls),
    test_server:format(Level, "~nStarting test with ~w:~tw(~tp) on ~p~n",
		       [M, F, A, CTNode]),
    rpc:call(CTNode, M, F, A).

%% this is the last function that ct_run:script_start() calls, so the
%% return value here is what rpc:call/4 above returns
ct_test_halt(ExitStatus) ->
    ExitStatus.	    

%%%-----------------------------------------------------------------
%%% wait_for_ct_stop/1

wait_for_ct_stop(CTNode) ->
    %% Give CT at least 15 sec to stop (in case of bad make).
    wait_for_ct_stop(5, CTNode).

wait_for_ct_stop(0, CTNode) ->
    test_server:format(0, "Giving up! Stopping ~p.", [CTNode]),
    false;
wait_for_ct_stop(Retries, CTNode) ->
    case rpc:call(CTNode, erlang, whereis, [ct_util_server]) of
	undefined ->
	    true;
	Pid ->
	    Info = (catch process_info(Pid)),
	    test_server:format(0, "Waiting for CT (~p) to finish (~p)...", 
			       [Pid,Retries]),
	    test_server:format(0, "Process info for ~p:~n~tp", [Pid,Info]),
	    timer:sleep(5000),
	    wait_for_ct_stop(Retries-1, CTNode)
    end.

%%%-----------------------------------------------------------------
%%% ct_rpc/1
ct_rpc({M,F,A}, Config) ->
    CTNode = proplists:get_value(ct_node, Config),
    Level = proplists:get_value(trace_level, Config),
    test_server:format(Level, "~nCalling ~w:~tw(~tp) on ~p...",
		       [M,F,A, CTNode]),
    rpc:call(CTNode, M, F, A).


%%%-----------------------------------------------------------------
%%% random_error/1
random_error(Config) when is_list(Config) ->
    rand:seed(exsplus),
    Gen = fun(0,_) -> ok; (N,Fun) -> Fun(N-1, Fun) end,
    Gen(rand:uniform(100), Gen),

    ErrorTypes = ['BADMATCH','BADARG','CASE_CLAUSE','FUNCTION_CLAUSE',
		  'EXIT','THROW','UNDEF'],
    Type = lists:nth(rand:uniform(length(ErrorTypes)), ErrorTypes),
    Where = case rand:uniform(2) of
		1 ->
		    io:format("ct_test_support *returning* error of type ~w",
			      [Type]),
		    tc;
		2 ->
		    io:format("ct_test_support *generating* error of type ~w",
			      [Type]),
		    lib
	    end,
    ErrorFun =
	fun() ->
		case Type of
		    'BADMATCH' ->
			ok = proplists:get_value(undefined, Config);
		    'BADARG' ->
			size(proplists:get_value(priv_dir, Config));
		    'FUNCTION_CLAUSE' ->
			random_error(x);
		    'EXIT' ->
			spawn_link(fun() ->
					   undef_proc ! hello,
					   ok
				   end);
		    'THROW' ->
			PrivDir = proplists:get_value(priv_dir, Config),
			if is_list(PrivDir) -> throw(generated_throw) end;
		    'UNDEF' ->
			apply(?MODULE, random_error, [])
		end
	end,
    %% either call the fun here or return it to the caller (to be
    %% executed in a test case instead)
    case Where of
	tc -> ErrorFun;
	lib -> ErrorFun()
    end.
    

%%%-----------------------------------------------------------------
%%% EVENT HANDLING

handle_event(EH, Event) ->
    event_receiver ! {self(),{event,EH,Event}},
    receive {event_receiver,ok} -> ok end,
    ok.
    
start_event_receiver(Config) ->
    CTNode = proplists:get_value(ct_node, Config),
    Level = proplists:get_value(trace_level, Config),
    ER = spawn_link(CTNode, fun() -> er() end),
    test_server:format(Level, "~nEvent receiver ~w started!~n", [ER]),
    ER.

get_events(_, Config) ->
    CTNode = proplists:get_value(ct_node, Config),
    Level = proplists:get_value(trace_level, Config),
    {event_receiver,CTNode} ! {self(),get_events},
    Events = receive {event_receiver,Evs} -> Evs end,
    test_server:format(Level, "Stopping event receiver!~n", []),
    {event_receiver,CTNode} ! {self(),stop},
    receive {event_receiver,stopped} -> ok end,
    Events.

er() ->
    register(event_receiver, self()),
    er_loop([]).

er_loop(Evs) ->
    receive
	{From,{event,EH,Ev}} ->
	    From ! {event_receiver,ok},
	    er_loop([{EH,Ev} | Evs]);
	{From,get_events} ->
	    From ! {event_receiver,lists:reverse(Evs)},
	    er_loop(Evs);
	{From,stop} ->
	    unregister(event_receiver),
	    From ! {event_receiver,stopped},
	    ok
    end.

verify_events(TEvs, Evs, Config) ->
    Node = proplists:get_value(ct_node, Config),
    case catch verify_events1(TEvs, Evs, Node, Config) of
	{'EXIT',Reason} ->
	    Reason;
	_ ->
	    ok
    end.

verify_events(TEvs, Evs, Node, Config) ->
    case catch verify_events1(TEvs, Evs, Node, Config) of
	{'EXIT',Reason} ->
	    Reason;
	_ ->
	    ok
    end.

verify_events1([TestEv|_], [{TEH,#event{name=stop_logging,node=Node,data=_}}|_], Node, _)
  when element(1,TestEv) == TEH, element(2,TestEv) =/= stop_logging ->
    test_server:format("Failed to find ~tp in the list of events!~n", [TestEv]),
    exit({event_not_found,TestEv});

verify_events1(TEvs = [TestEv | TestEvs], Evs = [_|Events], Node, Config) ->
    case catch locate(TestEv, Node, Evs, Config) of
	nomatch ->
	    verify_events1(TEvs, Events, Node, Config);
	{'EXIT',Reason} ->
	    test_server:format("Failed to find ~tp in ~tp~n"
			       "Reason: ~tp~n", [TestEv,Evs,Reason]),
	    exit(Reason);
	{Config1,Events1} ->
	    if is_list(TestEv) ->
		    ok;
	       element(1,TestEv) == parallel ; element(1,TestEv) == shuffle ->
		    ok;
	       true ->
		    test_server:format("Found ~tp!", [TestEv])
	    end,
	    verify_events1(TestEvs, Events1, Node, Config1)
    end;

verify_events1([TestEv|_], [], _, _) ->
    test_server:format("Failed to find ~tp in the list of events!~n", [TestEv]),
    exit({event_not_found,TestEv});

verify_events1([], Evs, _, Config) ->
    {Config,Evs}.

%%%----------------------------------------------------------------------------
%%% locate({TEHandler,TEName,TEData}, TENode, Events, Config) -> {Config1,Evs1}
%%%
%%% A group is represented as either:
%%% {parallel,ListOfCasesAndGroups}, 
%%% {shuffle,ListOfCasesAndGroups}, or
%%% ListOfCasesAndGroups.
%%%
%%% The two first and two last events in a group *may* be tc_start and tc_done
%%% for init_per_group and end_per_group.

%% group (not parallel or shuffle)
locate(TEvs, Node, Evs, Config) when is_list(TEvs) ->
    case TEvs of
	[InitStart = {TEH,tc_start,{M,{init_per_group,GroupName,Props}}},
	 InitDone  = {TEH,tc_done,{M,{init_per_group,GroupName,Props},R}} | TEvs1] ->
	    case Evs of		
		[{TEH,#event{name=tc_start, 
			     node=Node, 
			     data={M,{init_per_group,GroupName,Props}}}},
		 {TEH,#event{name=tc_done, 
			     node=Node, 
			     data={M,{init_per_group,GroupName,Props},Res}}} | Evs1] ->
		    case result_match(R, Res) of
			false ->
			    nomatch;
			true ->
			    test_server:format("Found ~tp!", [InitStart]),
			    test_server:format("Found ~tp!", [InitDone]),
			    verify_events1(TEvs1, Evs1, Node, Config)
		    end;
		_ ->
		    nomatch
	    end;
	_ ->
	    verify_events1(TEvs, Evs, Node, Config)
    end;

%% Parallel events: Each test case in the group should be specified in a list
%% with the tc_start, followed by the tc_done event. The order of the cases
%% is irrelevant, but it must be checked that every test case exists and
%% that tc_done comes after tc_start.
locate({parallel,TEvs}, Node, Evs, Config) ->
    Start =
	case TEvs of
	    [InitStart = {TEH,tc_start,{M,{init_per_group,GroupName,Props}}},
	     InitDone  = {TEH,tc_done,{M,{init_per_group,GroupName,Props},R}} | TEs] ->
		case Evs of		
		    [{TEH,#event{name=tc_start, 
				 node=Node, 
				 data={M,{init_per_group,
					  GroupName,Props}}}}|Es] ->
			%% Use dropwhile here as a tc_done from a
			%% previous testcase might sneak in here
			EvsG = lists:dropwhile(
				fun({EH,#event{name=tc_done, 
						node=EvNode, 
						data={EvM,{init_per_group,
							   EvGroupName,
							   EvProps},EvR}}})
				   when TEH == EH, EvNode == Node, EvM == M,
					EvGroupName == GroupName,
					EvProps == Props ->
					case result_match(R, EvR) of
					    true -> false;
					    false -> true
					end;
				   ({EH,#event{name=stop_logging,
						node=EvNode,data=_}})
				   when EH == TEH, EvNode == Node ->
					exit({group_init_done_not_found,
					      GroupName,Props});
				   (_) ->
					true
				end, Es),	      
			
			test_server:format("Found ~tp!", [InitStart]),
			test_server:format("Found ~tp!", [InitDone]),
			{TEs,EvsG};
		    _ ->
			nomatch
		end;
	    _ ->
		{TEvs,Evs}
	end,
    case Start of
	nomatch ->
	    nomatch;
	{TEvs1,Evs1} ->
	    {TcDoneEvs,RemainEvs,_} =
		lists:foldl(
		  %% tc_start event for a parallel test case
		  fun(TEv={TEH,tc_start,{M,F}}, {Done,RemEvs,RemSize}) ->
			  %% drop events until TEv is found
			  Evs2 = lists:dropwhile(
				   fun({EH,#event{name=tc_start,
						  node=EvNode,
						  data={Mod,Func}}}) when 
					     EH == TEH, EvNode == Node, 
					     Mod == M, Func == F ->
					   false;
				      ({EH,#event{name=stop_logging,
						  node=EvNode,data=_}}) when
					     EH == TEH, EvNode == Node ->
					   exit({tc_start_not_found,TEv});
				      (_) ->
					   true
				   end, Evs1),
			  %% split the list at the tc_done event and record the smallest
			  %% list of remaining events (Evs) as possible
			  RemEvs1 =
			      lists:dropwhile(
				fun({EH,#event{name=tc_done,
					       node=EvNode,
					       data={Mod,Func,_}}}) when 
					  EH == TEH, EvNode == Node, 
					  Mod == M, Func == F ->
					false;
				   ({EH,#event{name=stop_logging,
					       node=EvNode,data=_}}) when
					  EH == TEH, EvNode == Node ->
					exit({tc_done_not_found,TEv});
				   (_) ->
					true
				end, Evs2),
			  case RemEvs1 of
			      [] when Evs2 == [] ->
				  exit({unmatched,TEv});
			      [] ->
				  test_server:format("Found ~tp!", [TEv]),
				  exit({tc_done_not_found,TEv});
			      [TcDone|Evs3] ->
				  test_server:format("Found ~tp!", [TEv]),
				  RemSize1 = length(Evs3),
				  if RemSize1 < RemSize ->
					  {[TcDone|Done],Evs3,RemSize1};
				     true ->
					  {[TcDone|Done],RemEvs,RemSize}
				  end
			  end;
		     %% tc_done event for a parallel test case
		     (TEv={TEH,tc_done,{M,F,R}}, {Done,RemEvs,RemSize}) ->
			  case [E || E={EH,#event{name=tc_done,
						  node=EvNode,
						  data={Mod,Func,Result}}} <- Done, 
				     EH == TEH, EvNode == Node, Mod == M, 
				     Func == F, result_match(R, Result)] of
			      [TcDone|_] ->
				  test_server:format("Found ~tp!", [TEv]),
				  {lists:delete(TcDone, Done),RemEvs,RemSize};
			      [] ->
				  exit({unmatched,TEv})
			  end;
		     %% tc_start event for end_per_group
		     (TEv={TEH,tc_start,{M,{end_per_group,GroupName,Props}}}, 
		      {Done,RemEvs,_RemSize}) ->
			  RemEvs1 = 
			      lists:dropwhile(
				fun({EH,#event{name=tc_start,
					       node=EvNode,
					       data={Mod,{end_per_group,
							  EvGName,EvProps}}}}) when 
					  EH == TEH, EvNode == Node, Mod == M,
					  EvGName == GroupName, EvProps == Props ->
					false;
				   ({EH,#event{name=stop_logging,
					       node=EvNode,data=_}}) when
					  EH == TEH, EvNode == Node ->
					exit({tc_start_not_found,TEv});
				   (_) ->
					true
				end, RemEvs),
			  case RemEvs1 of
			      [] -> 
				  exit({end_per_group_not_found,TEv});
			      [_ | RemEvs2] ->
				  test_server:format("Found ~tp!", [TEv]),
				  {Done,RemEvs2,length(RemEvs2)}
			  end;
		     %% tc_done event for end_per_group
		     (TEv={TEH,tc_done,{M,{end_per_group,GroupName,Props},R}}, 
		      {Done,RemEvs,_RemSize}) ->
			  RemEvs1 = 
			      lists:dropwhile(
				fun({EH,#event{name=tc_done,
					       node=EvNode,
					       data={Mod,{end_per_group,
							  EvGName,EvProps},Res}}}) when 
					  EH == TEH, EvNode == Node, Mod == M,
					  EvGName == GroupName, EvProps == Props ->
					case result_match(R, Res) of
					    true ->
						false;
					    false ->
						true
					end;
				   ({EH,#event{name=stop_logging,
					       node=EvNode,data=_}}) when
					  EH == TEH, EvNode == Node ->
					exit({tc_done_not_found,TEv});
				   (_) ->
					true
				end, RemEvs),
			  case RemEvs1 of
			      [] -> 
				  exit({end_per_group_not_found,TEv});
			      [_ | RemEvs2] ->
				  test_server:format("Found ~tp!", [TEv]),
				  {Done,RemEvs2,length(RemEvs2)}
			  end;
		     %% end_per_group auto- or user skipped
		     (TEv={TEH,AutoOrUserSkip,{M,{end_per_group,G},R}}, {Done,RemEvs,_RemSize})
			when AutoOrUserSkip == tc_auto_skip;
			     AutoOrUserSkip == tc_user_skip ->
			  RemEvs1 = 
			      lists:dropwhile(
				fun({EH,#event{name=tc_auto_skip,
					       node=EvNode,
					       data={Mod,{end_per_group,EvGroupName},Reason}}}) when
				   EH == TEH, EvNode == Node, Mod == M, EvGroupName == G ->
					case match_data(R, Reason) of
					    match -> false;
					    _ -> true
					end;
				   ({EH,#event{name=tc_user_skip,
					       node=EvNode,
					       data={Mod,{end_per_group,EvGroupName},Reason}}}) when
				   EH == TEH, EvNode == Node, Mod == M, EvGroupName == G ->
					case match_data(R, Reason) of
					    match -> false;
					    _ -> true
					end;
				   ({EH,#event{name=stop_logging,
					       node=EvNode,data=_}}) when
					  EH == TEH, EvNode == Node ->
					exit({tc_auto_or_user_skip_not_found,TEv});
				   (_) ->
					true
				end, RemEvs),
			  case RemEvs1 of
			      [] -> 
				  exit({end_per_group_not_found,TEv});
			      [_AutoSkip | RemEvs2] ->
				  {Done,RemEvs2,length(RemEvs2)}
			  end;
		     (TEv={TEH,N,D}, Acc) ->
			  case [E || E={EH,#event{name=Name,
						  node=EvNode,
						  data=Data}} <- Evs1, 
				     EH == TEH, EvNode == Node, Name == N,
				     match == match_data(D,Data)] of
			      [] ->
				  exit({unmatched,TEv});
			      _ ->
				  test_server:format("Found ~tp!", [TEv]),
				  Acc
			  end;
		     %% start of a sub-group
		     (SubGroupTEvs, Acc) when is_list(SubGroupTEvs) ->
			  verify_events1(SubGroupTEvs, Evs1, Node, Config),
			  Acc;
		     (TEv={Prop,_SubGroupTEvs}, Acc) when 
			    Prop == shuffle ; Prop == parallel ->
			  verify_events1([TEv], Evs1, Node, Config),
			  Acc
		  end, {[],Evs1,length(Evs1)}, TEvs1),
	    case TcDoneEvs of
		[] ->
		    test_server:format("Found all parallel events!", []),
		    {Config,RemainEvs};
		_ ->
		    exit({unexpected_events,TcDoneEvs})
	    end
    end;

%% Shuffled events: Each test case in the group should be specified in a list
%% with the tc_start, followed by the tc_done event. The order of the cases
%% is irrelevant, but it must be checked that every test case exists and
%% that the tc_done event follows the tc_start.
locate({shuffle,TEvs}, Node, Evs, Config) ->
   Start =
	case TEvs of
	    [InitStart = {TEH,tc_start,{M,{init_per_group,GroupName,Props}}},
	     InitDone  = {TEH,tc_done,{M,{init_per_group,GroupName,Props},R}} | TEs] ->
		case Evs of		
		    [{TEH,#event{name=tc_start, 
				 node=Node, 
				 data={M,{init_per_group,GroupName,EvProps}}}},
		     {TEH,#event{name=tc_done, 
				 node=Node, 
				 data={M,{init_per_group,GroupName,EvProps},Res}}} | Es] ->
			case result_match(R, Res) of
			    true ->
				case proplists:get_value(shuffle, Props) of
				    '_' ->
					case proplists:get_value(shuffle, EvProps) of
					    false ->
						exit({no_shuffle_prop_found,
						      {M,init_per_group,
						       GroupName,EvProps}});
					    _ ->
						PropsCmp = proplists:delete(shuffle, EvProps),
						PropsCmp = proplists:delete(shuffle, Props)
					end;
				    _ ->
					Props = EvProps
				end,
				test_server:format("Found ~tp!", [InitStart]),
				test_server:format("Found ~tp!", [InitDone]),
				{TEs,Es};
			    false ->
				nomatch
			end;
		    _ ->
			nomatch
		end;
	    _ ->
		{TEvs,Evs}
	end,
    case Start of
	nomatch ->
	    nomatch;
	{TEvs1,Evs1} ->
	    {TcDoneEvs,RemainEvs,_} =
		lists:foldl(
		  %% tc_start event for a test case
		  fun(TEv={TEH,tc_start,{M,F}}, {Done,RemEvs,RemSize}) ->
			  %% drop events until TEv is found
			  Evs2 = lists:dropwhile(
				   fun({EH,#event{name=tc_start,
						  node=EvNode,
						  data={Mod,Func}}}) when 
					     EH == TEH, EvNode == Node, 
					     Mod == M, Func == F ->
					   false;
				      ({EH,#event{name=stop_logging,
						  node=EvNode,data=_}}) when
					     EH == TEH, EvNode == Node ->
					   exit({tc_start_not_found,TEv});
				      (_) ->
					   true
				   end, Evs1),
			  %% verify the tc_done event comes next in Evs
			  case Evs2 of
			      [] ->
				  exit({unmatched,TEv});
			      [_TcStart, TcDone={TEH,#event{name=tc_done,
							    node=Node,
							    data={M,F,_}}} | Evs3] ->
				  test_server:format("Found ~tp!", [TEv]),
				  RemSize1 = length(Evs3),
				  if RemSize1 < RemSize -> 
					  {[TcDone|Done],Evs3,RemSize1};
				     true ->
					  {[TcDone|Done],RemEvs,RemSize}
				  end
			  end;
		     %% tc_done event for a test case
		     (TEv={TEH,tc_done,{M,F,R}}, {Done,RemEvs,RemSize}) ->
			  case [E || E={EH,#event{name=tc_done,
						  node=EvNode,
						  data={Mod,Func,Result}}} <- Done, 
				     EH == TEH, EvNode == Node, Mod == M, 
				     Func == F, result_match(R, Result)] of
			      [TcDone|_] ->
				  test_server:format("Found ~tp!", [TEv]),
				  {lists:delete(TcDone, Done),RemEvs,RemSize};
			      [] ->
				  exit({unmatched,TEv})
			  end;
		     %% tc_start event for end_per_group
		     (TEv={TEH,tc_start,{M,{end_per_group,GroupName,Props}}}, 
		      {Done,RemEvs,_RemSize}) ->
			  RemEvs1 = 
			      lists:dropwhile(
				fun({EH,#event{name=tc_start,
					       node=EvNode,
					       data={Mod,{end_per_group,
							  EvGName,_}}}}) when 
					  EH == TEH, EvNode == Node, Mod == M,
					  EvGName == GroupName ->
					false;
				   ({EH,#event{name=stop_logging,
					       node=EvNode,data=_}}) when
					  EH == TEH, EvNode == Node ->
					exit({tc_start_not_found,TEv});
				   (_) ->
					true
				end, RemEvs),
			  case RemEvs1 of
			      [] -> 
				  exit({end_per_group_not_found,TEv});
			      [{_,#event{data={_,{_,_,EvProps1}}}} | RemEvs2] ->
				  case proplists:get_value(shuffle, Props) of
				      '_' ->
					  case proplists:get_value(shuffle, EvProps1) of
					      false ->
						  exit({no_shuffle_prop_found,
							{M,end_per_group,GroupName,EvProps1}});
					      _ ->
						  PropsCmp1 = proplists:delete(shuffle, EvProps1),
						  PropsCmp1 = proplists:delete(shuffle, Props)
					  end;
				      _ ->
					  Props = EvProps1
				  end,
				  test_server:format("Found ~tp!", [TEv]),
				  {Done,RemEvs2,length(RemEvs2)}
			  end;
		     %% tc_done event for end_per_group
		     (TEv={TEH,tc_done,{M,{end_per_group,GroupName,Props},R}}, 
		      {Done,RemEvs,_RemSize}) ->
			  RemEvs1 = 
			      lists:dropwhile(
				fun({EH,#event{name=tc_done,
					       node=EvNode,
					       data={Mod,{end_per_group,
							  EvGName,_},Res}}}) when 
					  EH == TEH, EvNode == Node, Mod == M,
					  EvGName == GroupName ->
					case result_match(R, Res) of
					    true ->
						false;
					    false ->
						true
					end;
				   ({EH,#event{name=stop_logging,
					       node=EvNode,data=_}}) when
					  EH == TEH, EvNode == Node ->
					exit({tc_done_not_found,TEv});
				   (_) ->
					true
				end, RemEvs),
			  case RemEvs1 of
			      [] -> 
				  exit({end_per_group_not_found,TEv});
			      [{_,#event{data={_,{_,_,EvProps1},_}}} | RemEvs2] ->
				  case proplists:get_value(shuffle, Props) of
				      '_' ->
					  case proplists:get_value(shuffle, EvProps1) of
					      false ->
						  exit({no_shuffle_prop_found,
							{M,end_per_group,GroupName,EvProps1}});
					      _ ->
						  PropsCmp1 = proplists:delete(shuffle, EvProps1),
						  PropsCmp1 = proplists:delete(shuffle, Props)
					  end;
				      _ ->
					  Props = EvProps1
				  end,				  
				  test_server:format("Found ~tp!", [TEv]),
				  {Done,RemEvs2,length(RemEvs2)}
			  end;
		     %% end_per_group auto-or user skipped
		     (TEv={TEH,AutoOrUserSkip,{M,{end_per_group,G},R}}, {Done,RemEvs,_RemSize})
			when AutoOrUserSkip == tc_auto_skip;
			     AutoOrUserSkip == tc_user_skip ->
			  RemEvs1 = 
			      lists:dropwhile(
				fun({EH,#event{name=tc_auto_skip,
					       node=EvNode,
					       data={Mod,{end_per_group,EvGroupName},Reason}}}) when
				   EH == TEH, EvNode == Node, Mod == M, EvGroupName == G, Reason == R ->
					false;
				   ({EH,#event{name=tc_user_skip,
					       node=EvNode,
					       data={Mod,{end_per_group,EvGroupName},Reason}}}) when
				   EH == TEH, EvNode == Node, Mod == M, EvGroupName == G, Reason == R ->
					false;
				   ({EH,#event{name=stop_logging,
					       node=EvNode,data=_}}) when
					  EH == TEH, EvNode == Node ->
					exit({tc_auto_skip_not_found,TEv});
				   (_) ->
					true
				end, RemEvs),
			  case RemEvs1 of
			      [] -> 
				  exit({end_per_group_not_found,TEv});
			      [_AutoSkip | RemEvs2] ->
				  {Done,RemEvs2,length(RemEvs2)}
			  end;
		     %% match other event than test case
		     (TEv={TEH,N,D}, Acc) when D == '_' ->
			  case [E || E={EH,#event{name=Name,
						  node=EvNode,
						  data=_}} <- Evs1, 
				     EH == TEH, EvNode == Node, Name == N] of
			      [] ->
				  exit({unmatched,TEv});
			      _ ->
				  test_server:format("Found ~tp!", [TEv]),
				  Acc
			  end;
		     (TEv={TEH,N,D}, Acc) ->
			  case [E || E={EH,#event{name=Name,
						  node=EvNode,
						  data=Data}} <- Evs1, 
				     EH == TEH, EvNode == Node, Name == N, Data == D] of
			      [] ->
				  exit({unmatched,TEv});
			      _ ->
				  test_server:format("Found ~tp!", [TEv]),
				  Acc
			  end;
		     %% start of a sub-group
		     (SubGroupTEvs, Acc) when is_list(SubGroupTEvs) ->
			  verify_events1(SubGroupTEvs, Evs1, Node, Config),
			  Acc;
		     (TEv={Prop,_SubGroupTEvs}, Acc) when 
			    Prop == shuffle ; Prop == parallel ->
			  verify_events1([TEv], Evs1, Node, Config),
			  Acc
		  end, {[],Evs1,length(Evs1)}, TEvs1),
	    case TcDoneEvs of
		[] ->
		    test_server:format("Found all shuffled events!", []),
		    {Config,RemainEvs};
		_ ->
		    exit({unexpected_events,TcDoneEvs})
	    end
    end;

locate({TEH,Name,{'DEF','RUNDIR'}}, Node, [Ev|Evs], Config) ->
    case Ev of
	{TEH,#event{name=Name, node=Node, data=EvData}} ->
	    {_,{_,LogDir}} = lists:keysearch(logdir, 1, get_opts(Config)),
	    D = filename:join(LogDir, "ct_run." ++ atom_to_list(Node)),
	    case string:find(EvData, D) of
		nomatch -> exit({badmatch,EvData});
		_ -> ok	    
	    end,
	    {Config,Evs};
	_ ->
	    nomatch
    end;

locate({TEH,Name,{'DEF',{'START_TIME','LOGDIR'}}}, Node, [Ev|Evs], Config) ->
    case Ev of
	{TEH,#event{name=Name, node=Node, data=EvData}} ->
	    case EvData of
		{DT={{_,_,_},{_,_,_}},Dir} when is_list(Dir) ->
		    {_,{_,LogDir}} = lists:keysearch(logdir, 1, get_opts(Config)),
		    D = filename:join(LogDir, "ct_run." ++ atom_to_list(Node)),
		    case string:find(Dir, D) of
			nomatch -> exit({badmatch,Dir});
			_ -> ok	    
		    end,
		    {[{start_time,DT}|Config],Evs};
		Data ->
		    exit({badmatch,Data})
	    end;
	_ ->
	    nomatch
    end;

locate({TEH,Name,{'DEF','STOP_TIME'}}, Node, [Ev|Evs], Config) ->
    case Ev of
	{TEH,#event{name=Name, node=Node, data=EvData}} ->
	    case EvData of
		DT={{_,_,_},{_,_,_}} ->
		    {[{stop_time,DT}|Config],Evs};
		Data ->
		    exit({badmatch,Data})
	    end;
	_ ->
	    nomatch
    end;

%% to match variable data as a result of an aborted test case
locate({TEH,tc_done,{undefined,undefined,{testcase_aborted,
					  {abort_current_testcase,Func},'_'}}},
       Node, [Ev|Evs], Config) ->
    case Ev of
	{TEH,#event{name=tc_done, node=Node,
		    data={undefined,undefined,
			  {testcase_aborted,{abort_current_testcase,Func},_}}}} ->
	    {Config,Evs};
	_ ->
	    nomatch
    end;

%% to match variable data as a result of a failed test case
locate({TEH,tc_done,{Mod,Func,R={SkipOrFail,{_ErrInd,ErrInfo}}}},
       Node, [Ev|Evs], Config) when ((SkipOrFail == skipped) or
				     (SkipOrFail == failed)) and
				    ((size(ErrInfo) == 2) or
				     (size(ErrInfo) == 3)) ->
    case Ev of
	{TEH,#event{name=tc_done, node=Node, 
		    data={Mod,Func,Result}}} ->
	    case result_match(R, Result) of
		true ->
		    {Config,Evs};
		false ->
		    nomatch
	    end;
	_ ->
	    nomatch
    end;

%% Negative matching: Given two events, the first should not be present before
%% the other is matched. 
locate({negative,NotMatch, Match} = Neg, Node, Evs, Config) ->
    case locate(NotMatch, Node, Evs, Config) of
	nomatch ->
	    locate(Match, Node, Evs, Config);
	_ ->
	    exit({found_negative_event,Neg})
    end;

%% matches any event of type Name
locate({TEH,Name,Data}, Node, [{TEH,#event{name=Name,
					   data = EvData,
					   node = Node}}|Evs],
       Config) ->
    case match_data(Data, EvData) of
	match ->
	    {Config,Evs};
	_ ->
	    nomatch
    end;

locate({_TEH,_Name,_Data}, _Node, [_|_Evs], _Config) ->
    nomatch.

match_data(Data, EvData) ->
    try do_match_data(Data, EvData)
    catch _:_ ->
	    nomatch
    end.

do_match_data(D,D) ->
    match;
do_match_data('_',_) ->
    match;
do_match_data(Fun,Data) when is_function(Fun) ->
    Fun(Data);
do_match_data('$proplist',Proplist) ->
    do_match_data(
      fun(List) ->
	      lists:foreach(fun({_,_}) -> ok end,List)
      end,Proplist);
do_match_data([H1|MatchT],[H2|ValT]) ->
    do_match_data(H1,H2),
    do_match_data(MatchT,ValT);
do_match_data(Tuple1,Tuple2) when is_tuple(Tuple1),is_tuple(Tuple2) ->
    do_match_data(tuple_to_list(Tuple1),tuple_to_list(Tuple2));
do_match_data([],[]) ->
    match.

result_match({SkipOrFail,{ErrorInd,{Why,'_'}}},
	    {SkipOrFail,{ErrorInd,{Why,_Stack}}}) ->
    true;
result_match({SkipOrFail,{ErrorInd,{EMod,EFunc,{Why,'_'}}}},
	    {SkipOrFail,{ErrorInd,{EMod,EFunc,{Why,_Stack}}}}) ->
    true;
result_match({failed,{timetrap_timeout,{'$approx',Num}}},
	     {failed,{timetrap_timeout,Value}}) ->
    if Value >= trunc(Num-0.05*Num),
       Value =< trunc(Num+0.05*Num) -> true;
       true -> false
    end;
result_match({user_timetrap_error,{Why,'_'}},
	     {user_timetrap_error,{Why,_Stack}}) ->
    true;
result_match(Result, Result) ->
    true;
result_match(_, _) ->
    false.

log_events(TC, Events, EvLogDir, Opts) ->
    LogFile = filename:join(EvLogDir, atom_to_list(TC)++".events"),
    {ok,Dev} = file:open(LogFile, [write,{encoding,utf8}]),
    io:format(Dev, "[~n", []),
    log_events1(Events, Dev, " "),
    file:close(Dev),
    FullLogFile = join_abs_dirs(proplists:get_value(net_dir, Opts),
				LogFile),
    ct:log("Events written to logfile: <a href=\"file://~ts\">~ts</a>~n",
	   [FullLogFile,FullLogFile],[no_css]),
    io:format(user, "Events written to logfile: ~tp~n", [LogFile]).

log_events1(Evs, Dev, "") ->
    log_events1(Evs, Dev, " ");
log_events1([E={_EH,tc_start,{_M,{init_per_group,_GrName,Props}}} | Evs], Dev, Ind) ->
    case get_prop(Props) of
	undefined ->
	    io:format(Dev, "~s[~tp,~n", [Ind,E]),
	    log_events1(Evs, Dev, Ind++" ");	    
	Prop ->
	    io:format(Dev, "~s{~w,~n~s[~tp,~n", [Ind,Prop,Ind++" ",E]),
	    log_events1(Evs, Dev, Ind++"  ")
    end;
log_events1([E={_EH,tc_done,{_M,{init_per_group,_GrName,_Props},_R}} | Evs], Dev, Ind) ->
    io:format(Dev, "~s~tp,~n", [Ind,E]),
    log_events1(Evs, Dev, Ind++" ");
log_events1([E={_EH,tc_start,{_M,{end_per_group,_GrName,_Props}}} | Evs], Dev, Ind) ->
    Ind1 = Ind -- " ",
    io:format(Dev, "~s~tp,~n", [Ind1,E]),
    log_events1(Evs, Dev, Ind1);
log_events1([E={_EH,tc_done,{_M,{end_per_group,_GrName,Props},_R}} | Evs], Dev, Ind) ->
    case get_prop(Props) of
	undefined ->
	    io:format(Dev, "~s~tp],~n", [Ind,E]),
	    log_events1(Evs, Dev, Ind--" ");
	_Prop ->
	    io:format(Dev, "~s~tp]},~n", [Ind,E]),
	    log_events1(Evs, Dev, Ind--"  ")
    end;
log_events1([E={_EH,tc_auto_skip,{_M,{end_per_group,_GrName},_Reason}} | Evs], Dev, Ind) ->
    io:format(Dev, "~s~tp],~n", [Ind,E]),
    log_events1(Evs, Dev, Ind--" ");
log_events1([E={_EH,tc_user_skip,{_M,{end_per_group,_GrName},_Reason}} | Evs], Dev, Ind) ->
    io:format(Dev, "~s~tp],~n", [Ind,E]),
    log_events1(Evs, Dev, Ind--" ");
log_events1([E], Dev, Ind) ->
    io:format(Dev, "~s~tp~n].~n", [Ind,E]),
    ok;
log_events1([E | Evs], Dev, Ind) ->
    io:format(Dev, "~s~tp,~n", [Ind,E]),
    log_events1(Evs, Dev, Ind);
log_events1([], _Dev, _Ind) ->
    ok.

get_prop(Props) ->
    case lists:member(parallel, Props) of
	true -> parallel;
	false -> case lists:member(shuffle, Props) of
		     true -> shuffle;
		     false -> case lists:keysearch(shuffle, 1, Props) of
				  {value,_} -> shuffle;
				  _ -> undefined
			      end
		 end
    end.			    

reformat([{_EH,#event{name=start_write_file,data=_}} | Events], EH) ->
    reformat(Events, EH);
reformat([{_EH,#event{name=finished_write_file,data=_}} | Events], EH) ->
    reformat(Events, EH);
reformat([{_EH,#event{name=start_make,data=_}} | Events], EH) ->
    reformat(Events, EH);
reformat([{_EH,#event{name=finished_make,data=_}} | Events], EH) ->
    reformat(Events, EH);
reformat([{_EH,#event{name=start_logging,data=_}} | Events], EH) ->
    [{EH,start_logging,{'DEF','RUNDIR'}} | reformat(Events, EH)];
reformat([{_EH,#event{name=test_start,data=_}} | Events], EH) ->
    [{EH,test_start,{'DEF',{'START_TIME','LOGDIR'}}} | reformat(Events, EH)];
reformat([{_EH,#event{name=test_done,data=_}} | Events], EH) ->
    [{EH,test_done,{'DEF','STOP_TIME'}} | reformat(Events, EH)];
reformat([{_EH,#event{name=tc_logfile,data=_}} | Events], EH) ->
    reformat(Events, EH);
reformat([{_EH,#event{name=test_stats,data=Data}} | Events], EH) ->
    [{EH,test_stats,Data} | reformat(Events, EH)];
%% use this to only print the last test_stats event:
%%    case [N || {_,#event{name=N}} <- Events, N == test_stats] of
%%	[] ->					% last stats event
%%	    [{EH,test_stats,Data} | reformat(Events, EH)];
%%	_ ->
%%	    reformat(Events, EH)
%%    end;
reformat([{_EH,#event{name=Name,data=Data}} | Events], EH) ->
    [{EH,Name,Data} | reformat(Events, EH)];
reformat([], _EH) ->
    [].


%%%-----------------------------------------------------------------
%%% MISC HELP FUNCTIONS

join_abs_dirs(undefined, Dir2) ->
    Dir2;
join_abs_dirs(Dir1, Dir2) ->
    case filename:pathtype(Dir2) of
	relative ->
	    filename:join(Dir1, Dir2);
	_ ->
	    [_Abs|Parts] = filename:split(Dir2),
	    filename:join(Dir1, filename:join(Parts))
    end.

create_tmp_logdir(Tmp) ->
    LogDir = filename:join(Tmp,"ct"),
    file:make_dir(LogDir),
    LogDir.

delete_old_logs({win32,_}, Config) ->
    case {proplists:get_value(priv_dir, Config),
	  proplists:get_value(logdir, get_opts(Config))} of
	{LogDir,LogDir} ->
	    ignore;
	{_,LogDir} ->				% using tmp for logs
	    catch delete_dirs(LogDir)
    end;

delete_old_logs(_, Config) ->
    case os:getenv("CT_USE_TMP_DIR") of
	false ->
	    ignore;
	_ ->
	    catch delete_dirs(proplists:get_value(logdir,
						  get_opts(Config)))
    end.

delete_dirs(LogDir) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    SaveTime = list_to_integer(os:getenv("CT_SAVE_OLD_LOGS", "28800")),
    Deadline = Now - SaveTime,
    Dirs = filelib:wildcard(filename:join(LogDir,"ct_run*")),
    Dirs2Del =
	lists:foldl(fun(Dir, Del) ->
			    [S,Mi,H,D,Mo,Y|_] = 
				lists:reverse(string:lexemes(Dir, [$.,$-,$_])),
			    S2I = fun(Str) -> list_to_integer(Str) end,
			    DT = {{S2I(Y),S2I(Mo),S2I(D)}, {S2I(H),S2I(Mi),S2I(S)}},
			    Then = calendar:datetime_to_gregorian_seconds(DT),
			    if Then > Deadline ->
				    Del;
			       true ->
				    [Dir | Del]
			    end 
		    end, [], Dirs),
    case length(Dirs2Del) of
	0 ->
	    test_server:format(0, "No log directories older than ~w secs.", [SaveTime]);
        N ->
	    test_server:format(0, "Deleting ~w directories older than ~w secs.", [N,SaveTime])
    end,
    delete_dirs(LogDir, Dirs2Del).

delete_dirs(_, []) ->
    ok;
delete_dirs(LogDir, [Dir | Dirs]) ->
    test_server:format(0, "Removing old log directory: ~ts", [Dir]),
    case catch rm_rec(Dir) of
	{_,Reason} ->
	    test_server:format(0, "Delete failed! (~tp)", [Reason]);
	ok ->
	    ok
    end,
    delete_dirs(LogDir, Dirs).
    
rm_rec(Dir) ->
    %% ensure we're removing the ct_run directory
    case lists:reverse(filename:split(Dir)) of
	[[$c,$t,$_,$r,$u,$n,$.|_]|_] ->
	    rm_dir(filename:absname(Dir));
	_ ->
	    {error,{invalid_logdir,Dir}}
    end.

rm_dir(Dir) ->
    case file:list_dir(Dir) of
	{error,Errno} ->
	    exit({ls_failed,Dir,Errno});
	{ok,Files} ->
	    rm_files([filename:join(Dir, F) || F <- Files]),
	    file:del_dir(Dir)
    end.

rm_files([F | Fs]) ->
    Base = filename:basename(F),
    if Base == "." ; Base == ".." ->
	    rm_files(Fs);
       true ->
	    case file:read_file_info(F) of
		{ok,#file_info{type=directory}} ->
		    rm_dir(F),
		    rm_files(Fs);
		{ok,_Regular} ->
		    case file:delete(F) of
			ok ->
			    rm_files(Fs);
			{error,Errno} ->
			    exit({del_failed,F,Errno})
		    end
	    end
    end;
rm_files([]) ->
    ok.

unique_timestamp() ->
    unique_timestamp(os:timestamp(), 100000).

unique_timestamp(TS, 0) ->
    TS;
unique_timestamp(TS0, N) ->
    case os:timestamp() of
	TS0 ->
	    timer:sleep(1),
	    unique_timestamp(TS0, N-1);
	TS1 ->
	    TS1
    end.

%%%-----------------------------------------------------------------
%%%
slave_stop(Node) ->
    Cover = test_server:is_cover(),
    if Cover-> cover:flush(Node);
       true -> ok
    end,
    erlang:monitor_node(Node, true),
    slave:stop(Node),
    receive
	{nodedown, Node} ->
	    if Cover -> cover:stop(Node);
	       true -> ok
	    end
    after 5000 ->
	    erlang:monitor_node(Node, false),
	    receive {nodedown, Node} -> ok after 0 -> ok end %flush
    end,
    ok.
