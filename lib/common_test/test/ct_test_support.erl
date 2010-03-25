%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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

%%% @doc Test support functions
%%%
%%% <p>This is a support module for testing the Common Test Framework.</p>
%%%
-module(ct_test_support).

-include("test_server.hrl").
-include_lib("common_test/include/ct_event.hrl").

-export([init_per_suite/1, init_per_suite/2, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2, write_testspec/3, 
	 run/4, get_opts/1, wait_for_ct_stop/1]).

-export([handle_event/2, start_event_receiver/1, get_events/2,
	 verify_events/3, reformat/2, log_events/3]).

-include_lib("kernel/include/file.hrl").

%%%-----------------------------------------------------------------
%%% init_per_suite/1

init_per_suite(Config) ->
    init_per_suite(Config, 50).

init_per_suite(Config, Level) ->
    case delete_old_logs(os:type(), Config) of
	{'EXIT',DelLogsReason} ->
	    test_server:format(0, "Failed to delete old log directories: ~p~n", 
			       [DelLogsReason]);
	_ ->
	    ok
    end,
    [_,Host] = string:tokens(atom_to_list(node()), "@"),
    case slave:start(Host, ct, []) of
	{error,Reason} ->
	    test_server:fail(Reason);
	{ok,CTNode} ->
	    test_server:format(0, "Node ~p started~n", [CTNode]),
	    IsCover = test_server:is_cover(),
	    if IsCover ->
		cover:start(CTNode);
	    true->
		ok
	    end,
	    DataDir = ?config(data_dir, Config),
	    PrivDir = ?config(priv_dir, Config),

	    %% PrivDir as well as directory of Test Server suites
	    %% have to be in code path on Common Test node.
	    true = rpc:call(CTNode, code, add_patha, [PrivDir]),
	    [_ | Parts] = lists:reverse(filename:split(DataDir)),
	    TSDir = filename:join(lists:reverse(Parts)),
	    true = rpc:call(CTNode, code, add_patha, [TSDir]),
	    test_server:format(Level, "Dirs added to code path (on ~w):~n"
			       "~s~n~s~n", [CTNode,TSDir,PrivDir]),

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
    CTNode = ?config(ct_node, Config),
    PrivDir = ?config(priv_dir, Config),
    true = rpc:call(CTNode, code, del_path, [filename:join(PrivDir,"")]),
    cover:stop(CTNode),
    slave:stop(CTNode),
    ok.

%%%-----------------------------------------------------------------
%%% init_per_testcase/2

init_per_testcase(_TestCase, Config) ->
    {_,{_,LogDir}} = lists:keysearch(logdir, 1, get_opts(Config)),
    case lists:keysearch(master, 1, Config) of
	false->
	    test_server:format("See Common Test logs here:\n"
		       "<a href=\"file://~s/all_runs.html\">~s/all_runs.html</a>",
		       [LogDir,LogDir]);
	{value, _}->
	    test_server:format("See CT Master Test logs here:\n"
		       "<a href=\"file://~s/master_runs.html\">~s/master_runs.html</a>",
		       [LogDir,LogDir])
    end,
    Config.

%%%-----------------------------------------------------------------
%%% end_per_testcase/2

end_per_testcase(_TestCase, Config) ->
    CTNode = ?config(ct_node, Config),
    wait_for_ct_stop(CTNode),
    ok.


%%%-----------------------------------------------------------------
%%% 

write_testspec(TestSpec, Dir, Name) ->
    TSFile = filename:join(Dir, Name),
    {ok,Dev} = file:open(TSFile, [write]),
    [io:format(Dev, "~p.~n", [Entry]) || Entry <- TestSpec],
    file:close(Dev),
    io:format("Test specification written to: ~p~n", [TSFile]),
    io:format(user, "Test specification written to: ~p~n", [TSFile]),
    TSFile.
    

%%%-----------------------------------------------------------------
%%% 

get_opts(Config) ->
    PrivDir = ?config(priv_dir, Config),
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
    InitOpts = ?config(ct_opts, Config),
    [{logdir,LogDir} | InitOpts].


%%%-----------------------------------------------------------------
%%% 
run(M, F, A, Config) ->
    CTNode = ?config(ct_node, Config),
    Level = ?config(trace_level, Config),
    test_server:format(Level, "Calling ~w:~w(~p) on ~p~n", 
		       [M, F, A, CTNode]),
    rpc:call(CTNode, M, F, A).


%%%-----------------------------------------------------------------
%%% wait_for_ct_stop/1

wait_for_ct_stop(CTNode) ->
    %% Give CT at least 15 sec to stop (in case of bad make).
    wait_for_ct_stop(5, CTNode).

wait_for_ct_stop(0, CTNode) ->
    test_server:format(0, "Giving up! Stopping ~p.", [CTNode]),
    ok;
wait_for_ct_stop(Retries, CTNode) ->
    case rpc:call(CTNode, erlang, whereis, [ct_util_server]) of
	undefined ->
	    ok;
	Pid ->
	    test_server:format(0, "Waiting for CT (~p) to finish (~p)...", 
			       [Pid,Retries]),
	    timer:sleep(5000),
	    wait_for_ct_stop(Retries-1, CTNode)
    end.

%%%-----------------------------------------------------------------
%%% EVENT HANDLING

handle_event(EH, Event) ->
    event_receiver ! {self(),{event,EH,Event}},
    receive {event_receiver,ok} -> ok end,
    ok.
    
start_event_receiver(Config) ->
    CTNode = ?config(ct_node, Config),
    spawn_link(CTNode, fun() -> er() end).

get_events(_, Config) ->
    CTNode = ?config(ct_node, Config),
    {event_receiver,CTNode} ! {self(),get_events},
    Events = receive {event_receiver,Evs} -> Evs end,
    {event_receiver,CTNode} ! stop,
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
	stop ->
	    ok
    end.

verify_events(TEvs, Evs, Config) ->
    Node = ?config(ct_node, Config),    
    case catch verify_events1(TEvs, Evs, Node, Config) of
	{'EXIT',Reason} ->
	    Reason;
	_ ->
	    ok
    end.

verify_events1(TEvs = [TestEv | TestEvs], Evs = [_|Events], Node, Config) ->
%%    test_server:format("Next expected event: ~p~n", [TestEv]),
    case catch locate(TestEv, Node, Evs, Config) of
	nomatch ->
	    verify_events1(TEvs, Events, Node, Config);
	{'EXIT',Reason} ->
	    test_server:format("Failed to find ~p in ~p~n"
			       "Reason: ~p~n", [TestEv,Evs,Reason]),
	    exit(Reason);
	{Config1,Events1} ->
	    if is_list(TestEv) ->
		    ok;
	       element(1,TestEv) == parallel ; element(1,TestEv) == shuffle ->
		    ok;
	       true ->
		    test_server:format("Found ~p!", [TestEv])
	    end,
	    verify_events1(TestEvs, Events1, Node, Config1)
    end;

verify_events1([TestEv|_], [], _, _) ->
    test_server:format("Failed to find ~p in the list of events!~n", [TestEv]),
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
			     data={M,{init_per_group,GroupName,Props},R}}} | Evs1] ->
		    test_server:format("Found ~p!", [InitStart]),
		    test_server:format("Found ~p!", [InitDone]),
		    verify_events1(TEvs1, Evs1, Node, Config);
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
				 data={M,{init_per_group,GroupName,Props}}}},
		     {TEH,#event{name=tc_done, 
				 node=Node, 
				 data={M,{init_per_group,GroupName,Props},R}}} | Es] ->
			test_server:format("Found ~p!", [InitStart]),
			test_server:format("Found ~p!", [InitDone]),
			{TEs,Es};
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
				   (_) ->
					true
				end, Evs2),
			  case RemEvs1 of
			      [] when Evs2 == [] ->
				  exit({unmatched,TEv});
			      [] ->
				  test_server:format("Found ~p!", [TEv]),
				  exit({tc_done_not_found,TEv});
			      [TcDone|Evs3] ->
				  test_server:format("Found ~p!", [TEv]),
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
				     Func == F, Result == R] of
			      [TcDone|_] ->
				  test_server:format("Found ~p!", [TEv]),
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
				   (_) ->
					true
				end, RemEvs),
			  case RemEvs1 of
			      [] -> 
				  exit({end_per_group_not_found,TEv});
			      [_ | RemEvs2] ->
				  test_server:format("Found ~p!", [TEv]),
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
					  EvGName == GroupName, EvProps == Props, Res == R ->
					false;
				   (_) ->
					true
				end, RemEvs),
			  case RemEvs1 of
			      [] -> 
				  exit({end_per_group_not_found,TEv});
			      [_ | RemEvs2] ->
				  test_server:format("Found ~p!", [TEv]),
				  {Done,RemEvs2,length(RemEvs2)}
			  end;
		     %% end_per_group auto skipped
		     (TEv={TEH,tc_auto_skip,{M,end_per_group,R}}, {Done,RemEvs,_RemSize}) ->
			  RemEvs1 = 
			      lists:dropwhile(
				fun({EH,#event{name=tc_auto_skip,
					       node=EvNode,
					       data={Mod,end_per_group,Reason}}}) when
				   EH == TEH, EvNode == Node, Mod == M, Reason == R ->
					false;
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
				  test_server:format("Found ~p!", [TEv]),
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
				  test_server:format("Found ~p!", [TEv]),
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
				 data={M,{init_per_group,GroupName,EvProps},R}}} | Es] ->
			case proplists:get_value(shuffle, Props) of
			    '_' ->
				case proplists:get_value(shuffle, EvProps) of
				    false ->
					exit({no_shuffle_prop_found,{M,init_per_group,
								     GroupName,EvProps}});
				    _ ->
					PropsCmp = proplists:delete(shuffle, EvProps),
					PropsCmp = proplists:delete(shuffle, Props)
				end;
			    _ ->
				Props = EvProps
			end,
			test_server:format("Found ~p!", [InitStart]),
			test_server:format("Found ~p!", [InitDone]),
			{TEs,Es};
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
				  test_server:format("Found ~p!", [TEv]),
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
				     Func == F, Result == R] of
			      [TcDone|_] ->
				  test_server:format("Found ~p!", [TEv]),
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
				  test_server:format("Found ~p!", [TEv]),
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
					  EvGName == GroupName, Res == R ->
					false;
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
				  test_server:format("Found ~p!", [TEv]),
				  {Done,RemEvs2,length(RemEvs2)}
			  end;
		     %% end_per_group auto skipped
		     (TEv={TEH,tc_auto_skip,{M,end_per_group,R}}, {Done,RemEvs,_RemSize}) ->
			  RemEvs1 = 
			      lists:dropwhile(
				fun({EH,#event{name=tc_auto_skip,
					       node=EvNode,
					       data={Mod,end_per_group,Reason}}}) when
				   EH == TEH, EvNode == Node, Mod == M, Reason == R ->
					false;
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
				  test_server:format("Found ~p!", [TEv]),
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
				  test_server:format("Found ~p!", [TEv]),
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
	    case string:str(EvData, D) of
		0 -> exit({badmatch,EvData});
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
		    case string:str(Dir, D) of
			0 -> exit({badmatch,Dir});
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

%% to match variable data as a result of a failed test case
locate({TEH,tc_done,{Mod,Func,{failed,{error,{Slogan,'_'}}}}}, Node, [Ev|Evs], Config) ->
    case Ev of
	{TEH,#event{name=tc_done, node=Node, 
		    data={Mod,Func,{failed,{error,{Slogan,_}}}}}} ->
	    {Config,Evs};
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

%% matches any event of type Name
locate({TEH,Name,Data}, Node, [Ev|Evs], Config) when Data == '_' ->
    case Ev of
	{TEH,#event{name=Name, node=Node}} ->
	    {Config,Evs};
	_ ->
	    nomatch
    end;

locate({TEH,Name,Data}, Node, [Ev|Evs], Config) ->
    case Ev of
	{TEH,#event{name=Name, node=Node, data=Data}} ->
	    {Config,Evs};
	_ ->
	    nomatch
    end.

log_events(TC, Events, PrivDir) ->
    LogFile = filename:join(PrivDir, atom_to_list(TC)++".events"),
    {ok,Dev} = file:open(LogFile, [write]),
    io:format(Dev, "[~n", []),
    log_events1(Events, Dev, " "),
    file:close(Dev),
    io:format("Events written to logfile: ~p~n", [LogFile]),
    io:format(user, "Events written to logfile: ~p~n", [LogFile]).

log_events1(Evs, Dev, "") ->
    log_events1(Evs, Dev, " ");
log_events1([E={_EH,tc_start,{_M,{init_per_group,_GrName,Props}}} | Evs], Dev, Ind) ->
    case get_prop(Props) of
	undefined ->
	    io:format(Dev, "~s[~p,~n", [Ind,E]),
	    log_events1(Evs, Dev, Ind++" ");	    
	Prop ->
	    io:format(Dev, "~s{~w,~n~s[~p,~n", [Ind,Prop,Ind++" ",E]),
	    log_events1(Evs, Dev, Ind++"  ")
    end;
log_events1([E={_EH,tc_done,{_M,{init_per_group,_GrName,_Props},_R}} | Evs], Dev, Ind) ->
    io:format(Dev, "~s~p,~n", [Ind,E]),
    log_events1(Evs, Dev, Ind++" ");
log_events1([E={_EH,tc_start,{_M,{end_per_group,_GrName,_Props}}} | Evs], Dev, Ind) ->
    Ind1 = Ind -- " ",
    io:format(Dev, "~s~p,~n", [Ind1,E]),
    log_events1(Evs, Dev, Ind1);
log_events1([E={_EH,tc_done,{_M,{end_per_group,_GrName,Props},_R}} | Evs], Dev, Ind) ->
    case get_prop(Props) of
	undefined ->
	    io:format(Dev, "~s~p],~n", [Ind,E]),
	    log_events1(Evs, Dev, Ind--" ");
	_Prop ->
	    io:format(Dev, "~s~p]},~n", [Ind,E]),
	    log_events1(Evs, Dev, Ind--"  ")
    end;
log_events1([E={_EH,tc_auto_skip,{_M,end_per_group,_Reason}} | Evs], Dev, Ind) ->
    io:format(Dev, "~s~p],~n", [Ind,E]),
    log_events1(Evs, Dev, Ind--" ");
log_events1([E], Dev, Ind) ->
    io:format(Dev, "~s~p~n].~n", [Ind,E]),
    ok;
log_events1([E | Evs], Dev, Ind) ->
    io:format(Dev, "~s~p,~n", [Ind,E]),
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

create_tmp_logdir(Tmp) ->
    LogDir = filename:join(Tmp,"ct"),
    file:make_dir(LogDir),
    LogDir.

delete_old_logs({win32,_}, Config) ->
    case {?config(priv_dir, Config),?config(logdir, get_opts(Config))} of
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
	    catch delete_dirs(?config(logdir, get_opts(Config)))
    end.

delete_dirs(LogDir) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    SaveTime = case os:getenv("CT_SAVE_OLD_LOGS") of
		   false ->
		       28800;
		   SaveTime0 ->
		       list_to_integer(SaveTime0)
	       end,
    Deadline = Now - SaveTime,
    Dirs = filelib:wildcard(filename:join(LogDir,"ct_run*")),
    Dirs2Del =
	lists:foldl(fun(Dir, Del) ->
			    [S,Mi,H,D,Mo,Y|_] = 
				lists:reverse(string:tokens(Dir, [$.,$-,$_])),
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
    test_server:format(0, "Removing old log directory: ~s", [Dir]),
    case catch rm_rec(Dir) of
	{_,Reason} ->
	    test_server:format(0, "Delete failed! (~p)", [Reason]);
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
    
