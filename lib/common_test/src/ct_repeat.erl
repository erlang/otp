%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

%%% @doc Common Test Framework module that handles repeated test runs
%%%
%%% <p>This module exports functions for repeating tests. The following
%%% start flags (or equivalent ct:run_test/1 options) are supported:
%%% -until <StopTime>, StopTime = YYMoMoDDHHMMSS | HHMMSS
%%% -duration <DurTime>, DurTime = HHMMSS
%%% -force_stop [skip_rest]
%%% -repeat <N>, N = integer()</p>

-module(ct_repeat).

%% Script interface
-export([loop_test/2]).
-export([log_loop_info/1]).

%%----------------------------------------------------------
%% Flags:
%%----------------------------------------------------------

loop_test(If,Args) when is_list(Args) ->
    {ok,Cwd} = file:get_cwd(),
    case get_loop_info(Args) of
	no_loop ->
	    false;
	E = {error,_} ->
	    io:format("Common Test error: ~p\n\n",[E]),
	    ok = file:set_cwd(Cwd),
	    E;
	{repeat,N} ->
	    io:format("\nCommon Test: Will repeat tests ~w times.\n\n",[N]),
	    Args1 = [{loop_info,[{repeat,1,N}]} | Args],
	    Result = loop(If,repeat,0,N,undefined,Args1,undefined,[]),
	    ok = file:set_cwd(Cwd),
	    Result;
	{stop_time,StopTime} ->
	    Result =
		case remaining_time(StopTime) of
		    0 ->
			io:format("\nCommon Test: "
				  "No time left to run tests.\n\n",[]),
			{error,not_enough_time};
		    Secs ->
			io:format("\nCommon Test: "
				  "Will repeat tests for ~s.\n\n",[ts(Secs)]),
			TPid =
			    case proplists:get_value(force_stop,Args) of
				False when False==false; False==undefined ->
				    undefined;
				ForceStop ->
				    CtrlPid = self(),
				    spawn(
				      fun() ->
					      stop_after(CtrlPid,Secs,ForceStop)
				      end)
			    end,
			Args1 = [{loop_info,[{stop_time,Secs,StopTime,1}]} | Args],
			loop(If,stop_time,0,Secs,StopTime,Args1,TPid,[])
		end,
	    ok = file:set_cwd(Cwd),
	    Result
    end.
    
loop(_,repeat,N,N,_,_Args,_,AccResult) ->
    lists:reverse(AccResult);

loop(If,Type,N,Data0,Data1,Args,TPid,AccResult) ->
    Pid = spawn_tester(If,self(),Args),
    receive 
	{'EXIT',Pid,Reason} ->
	    case Reason of
		{user_error,What} ->
		    io:format("\nTest run failed!\nReason: ~p\n\n\n", [What]),
		    cancel(TPid),
		    {error,What};			
		_ ->
		    io:format("Test run crashed! This could be an internal error "
			      "- please report!\n\n"
			      "~p\n\n\n",[Reason]),
		    cancel(TPid),
		    {error,Reason}
	    end;
	{Pid,{error,Reason}} ->
	    io:format("\nTest run failed!\nReason: ~p\n\n\n",[Reason]),
	    cancel(TPid),
	    {error,Reason};
	{Pid,Result} ->
	    if Type == repeat ->
		    io:format("\nTest run ~w(~w) complete.\n\n\n",[N+1,Data0]),
		    lists:keydelete(loop_info,1,Args),
		    Args1 = [{loop_info,[{repeat,N+2,Data0}]} | Args],
		    loop(If,repeat,N+1,Data0,Data1,Args1,TPid,[Result|AccResult]);
	       Type == stop_time ->
		    case remaining_time(Data1) of
			0 ->
			    io:format("\nTest time (~s) has run out.\n\n\n",
				      [ts(Data0)]),
			    cancel(TPid),
			    lists:reverse([Result|AccResult]);
			Secs ->
			    io:format("\n~s of test time remaining, " 
				      "starting run #~w...\n\n\n",
				      [ts(Secs),N+2]),
			    lists:keydelete(loop_info,1,Args),			    
			    ST = {stop_time,Data0,Data1,N+2},
			    Args1 = [{loop_info,[ST]} | Args],
			    loop(If,stop_time,N+1,Data0,Data1,Args1,TPid,
				 [Result|AccResult])
		    end
	    end
    end.

spawn_tester(script,Ctrl,Args) ->
    spawn_link(fun() -> ct_run:script_start1(Ctrl,Args) end);

spawn_tester(func,Ctrl,Opts) ->
    Tester = fun() ->
		     case catch ct_run:run_test2(Opts) of
			 {'EXIT',Reason} ->
			     exit(Reason);
			 Result ->
			     Ctrl ! {self(),Result}
		     end
	     end,
    spawn_link(Tester).

remaining_time(StopTime) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    Diff = StopTime - Now,
    if Diff > 0 ->
	    Diff;
       true ->
	    0
    end.

get_loop_info(Args) when is_list(Args) ->
    case lists:keysearch(until,1,Args) of
	{value,{until,Time}} ->
	    Time1 = delistify(Time),
	    case catch get_stop_time(until,Time1) of
		{'EXIT',_} ->
		    {error,{bad_time_format,Time1}};
		Stop ->
		    {stop_time,Stop}
	    end;
	false ->
	    case lists:keysearch(duration,1,Args) of
		{value,{duration,Time}} ->
		    Time1 = delistify(Time),		    
		    case catch get_stop_time(duration,Time1) of
			{'EXIT',_} ->
			    {error,{bad_time_format,Time1}};
			Stop ->
			    {stop_time,Stop}
		    end;
		false -> 
		    case lists:keysearch(repeat,1,Args) of
			{value,{repeat,R}} ->
			    case R of
				N when is_integer(N), N>0 -> 
				    {repeat,N};
				[Str] -> 
				    case catch list_to_integer(Str) of
					N when is_integer(N), N>0 ->
					    {repeat,N};
					_ ->
					    {error,{invalid_repeat_value,Str}}
				    end;
				_ ->
				    {error,{invalid_repeat_value,R}}
			    end;
			false ->
			    no_loop
		    end
	    end
    end.

get_stop_time(until,[Y1,Y2,Mo1,Mo2,D1,D2,H1,H2,Mi1,Mi2,S1,S2]) ->
    Date =
	case [Mo1,Mo2] of
	    "00" ->
		date();
	    _ ->
		Y = list_to_integer([Y1,Y2]),
		Mo = list_to_integer([Mo1,Mo2]),
		D = list_to_integer([D1,D2]),
		{YNow,_,_} = date(),
		Dec = trunc(YNow/100),
		Year =
		    if Y < (YNow-Dec*100) -> (Dec+1)*100 + Y;
		       true -> Dec*100 + Y
		    end,
		{Year,Mo,D}
	end,
    Time = {list_to_integer([H1,H2]),
	    list_to_integer([Mi1,Mi2]),
	    list_to_integer([S1,S2])},
    calendar:datetime_to_gregorian_seconds({Date,Time});

get_stop_time(until,Time=[_,_,_,_,_,_]) ->
    get_stop_time(until,"000000"++Time);

get_stop_time(duration,[H1,H2,Mi1,Mi2,S1,S2]) ->
    Secs = 
	list_to_integer([H1,H2]) * 3600 + 
	list_to_integer([Mi1,Mi2]) * 60 + 
	list_to_integer([S1,S2]),
    calendar:datetime_to_gregorian_seconds(calendar:local_time()) + Secs.

cancel(Pid) ->
    catch exit(Pid,kill).

%% After Secs, abort will make the test_server finish the current
%% job, then empty the job queue and stop.
stop_after(_CtrlPid,Secs,ForceStop) ->
    timer:sleep(Secs*1000),
    case ForceStop of
	SkipRest when SkipRest==skip_rest; SkipRest==["skip_rest"] ->
	    ct_util:set_testdata({skip_rest,true});
	_ ->
	    ok
    end,
    test_server_ctrl:abort().


%% Callback from ct_run to print loop info to system log.
log_loop_info(Args) ->
    case lists:keysearch(loop_info,1,Args) of
	false ->
	    ok;
	{value,{_,[{repeat,C,N}]}} ->
	    ct_logs:log("Test loop info","Test run ~w of ~w",[C,N]); 
	{value,{_,[{stop_time,Secs0,StopTime,N}]}} ->
	    LogStr1 = 
		case lists:keysearch(duration,1,Args) of
		    {value,{_,Dur}} ->
			io_lib:format("Specified test duration: ~s (~w secs)\n",
				      [delistify(Dur),Secs0]);
		_ ->
		    case lists:keysearch(until,1,Args) of
			{value,{_,Until}} ->
			    io_lib:format("Specified end time: ~s (duration ~w secs)\n",
					  [delistify(Until),Secs0]);
			_ ->
			    ok
		    end
	    end,
	    LogStr2 = io_lib:format("Test run #~w\n", [N]),
	    Secs = remaining_time(StopTime),
	    LogStr3 =
		io_lib:format("Test time remaining: ~w secs (~w%)\n",
			      [Secs,trunc((Secs/Secs0)*100)]),
	    LogStr4 =
		case proplists:get_value(force_stop,Args) of
		    False when False==false; False==undefined ->
			"";
		    ForceStop ->
			io_lib:format("force_stop is set to: ~w",[ForceStop])
		end,			
	    ct_logs:log("Test loop info",LogStr1++LogStr2++LogStr3++LogStr4,[])
    end.

ts(Secs) ->
    integer_to_list(Secs) ++ " secs".

delistify([X]) ->
    X;
delistify(X) ->
    X.
