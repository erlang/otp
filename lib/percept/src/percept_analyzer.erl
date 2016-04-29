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

%% @doc Utility functions to operate on percept data. These functions should
%%	be considered experimental. Behaviour may change in future releases.

-module(percept_analyzer).
-export([	
	minmax/1,
	waiting_activities/1,
	activities2count/2,
	activities2count/3,
	activities2count2/2,
	analyze_activities/2,
	runnable_count/1,
	runnable_count/2,
	seconds2ts/2,
	minmax_activities/2,
	mean/1
	]).

-include("percept.hrl").

%%==========================================================================
%%
%% 		Interface functions
%%
%%==========================================================================


%% @spec minmax([{X, Y}]) -> {MinX, MinY, MaxX, MaxY}
%%	X = number()
%%	Y = number()
%%	MinX = number()
%%	MinY = number()
%%	MaxX = number()
%%	MaxY = number()
%% @doc Returns the min and max of a set of 2-dimensional numbers.

minmax(Data) ->
    Xs = [ X || {X,_Y} <- Data],
    Ys = [ Y || {_X, Y} <- Data],
    {lists:min(Xs), lists:min(Ys), lists:max(Xs), lists:max(Ys)}.

%% @spec mean([number()]) -> {Mean, StdDev, N}
%%	Mean = float()
%%	StdDev = float()
%%	N = integer()
%% @doc Calculates the mean and the standard deviation of a set of
%%	numbers.

mean([])      -> {0, 0, 0}; 
mean([Value]) -> {Value, 0, 1};
mean(List)    -> mean(List, {0, 0, 0}).

mean([], {Sum, SumSquare, N}) -> 
    Mean   = Sum / N,
    StdDev = math:sqrt((SumSquare - Sum*Sum/N)/(N - 1)),
    {Mean, StdDev, N};
mean([Value | List], {Sum, SumSquare, N}) -> 
    mean(List, {Sum + Value, SumSquare + Value*Value, N + 1}).



activities2count2(Acts, StartTs) -> 
    Start = inactive_start_states(Acts),
    activities2count2(Acts, StartTs, Start, []).

activities2count2([], _, _, Out) -> lists:reverse(Out);
activities2count2([#activity{ id = Id, timestamp = Ts, state = active} | Acts], StartTs, {Proc,Port}, Out) when is_pid(Id) ->
    activities2count2(Acts, StartTs, {Proc + 1, Port}, [{?seconds(Ts, StartTs), Proc + 1, Port}|Out]);
activities2count2([#activity{ id = Id, timestamp = Ts, state = inactive} | Acts], StartTs, {Proc,Port}, Out) when is_pid(Id) ->
    activities2count2(Acts, StartTs, {Proc - 1, Port}, [{?seconds(Ts, StartTs), Proc - 1, Port}|Out]);
activities2count2([#activity{ id = Id, timestamp = Ts, state = active} | Acts], StartTs, {Proc,Port}, Out) when is_port(Id) ->
    activities2count2(Acts, StartTs, {Proc, Port + 1}, [{?seconds(Ts, StartTs), Proc, Port + 1}|Out]);
activities2count2([#activity{ id = Id, timestamp = Ts, state = inactive} | Acts], StartTs, {Proc,Port}, Out) when is_port(Id) ->
    activities2count2(Acts, StartTs, {Proc, Port - 1}, [{?seconds(Ts, StartTs), Proc, Port - 1}|Out]).


inactive_start_states(Acts) -> 
    D = activity_start_states(Acts, dict:new()),
    dict:fold(fun
        (K, inactive, {Procs, Ports}) when is_pid(K)  -> {Procs + 1, Ports};
        (K, inactive, {Procs, Ports}) when is_port(K) -> {Procs, Ports + 1};
        (_, _, {Procs, Ports})                        -> {Procs, Ports}
    end, {0,0}, D).
activity_start_states([], D) -> D;
activity_start_states([#activity{id = Id, state = State}|Acts], D) ->
    case dict:is_key(Id, D) of
        true  -> activity_start_states(Acts, D);
        false -> activity_start_states(Acts, dict:store(Id, State, D))
    end.




%% @spec activities2count(#activity{}, timestamp()) -> Result
%%	Result = [{Time, ProcessCount, PortCount}]
%%	Time = float()
%%	ProcessCount = integer()
%%	PortCount = integer()
%% @doc Calculate the resulting active processes and ports during
%%	the activity interval.
%%	Also checks active/inactive consistency.
%%	A task will always begin with an active state and end with an inactive state.

activities2count(Acts, StartTs) when is_list(Acts) -> activities2count(Acts, StartTs, separated).

activities2count(Acts, StartTs, Type) when is_list(Acts) -> activities2count_loop(Acts, {StartTs, {0,0}}, Type, []).

activities2count_loop([], _, _, Out) -> lists:reverse(Out);
activities2count_loop(
	[#activity{ timestamp = Ts, id = Id, runnable_count = Rc} | Acts], 
	{StartTs, {Procs, Ports}}, separated, Out) ->
    
    Time = ?seconds(Ts, StartTs),
    case Id of 
	Id when is_port(Id) ->
	    Entry = {Time, Procs, Rc},
	    activities2count_loop(Acts, {StartTs, {Procs, Rc}}, separated, [Entry | Out]);
	Id when is_pid(Id) ->
	    Entry = {Time, Rc, Ports},
	    activities2count_loop(Acts, {StartTs, {Rc, Ports}}, separated, [Entry | Out]);
	_ ->
   	    activities2count_loop(Acts, {StartTs,{Procs, Ports}}, separated, Out)
    end;
activities2count_loop(
	[#activity{ timestamp = Ts, id = Id, runnable_count = Rc} | Acts], 
	{StartTs, {Procs, Ports}}, summated, Out) ->
	
    Time = ?seconds(Ts, StartTs), 
    case Id of 
	Id when is_port(Id) ->
	    Entry = {Time, Procs + Rc},
	    activities2count_loop(Acts, {StartTs, {Procs, Rc}}, summated, [Entry | Out]);
	Id when is_pid(Id) ->
	    Entry = {Time, Rc + Ports},
	    activities2count_loop(Acts, {StartTs, {Rc, Ports}}, summated, [Entry | Out])
    end.

%% @spec waiting_activities([#activity{}]) -> FunctionList
%%	FunctionList = [{Seconds, Mfa, {Mean, StdDev, N}}]
%%	Seconds = float()
%%	Mfa = mfa()
%%	Mean = float()
%%	StdDev = float()
%%	N = integer()
%% @doc Calculates the time, both average and total, that a process has spent
%%	in a receive state at specific function. However, if there are multiple receives
%%	in a function it cannot differentiate between them.

waiting_activities(Activities) ->
    ListedMfas = waiting_activities_mfa_list(Activities, []),
    Unsorted = lists:foldl(
    	fun (Mfa, MfaList) ->
	    {Total, WaitingTimes} = get({waiting_mfa, Mfa}),
	    
	    % cleanup
	    erlang:erase({waiting_mfa, Mfa}),
	    
	    % statistics of receive waiting places
	    Stats = mean(WaitingTimes),

	    [{Total, Mfa, Stats} | MfaList]
	end, [], ListedMfas),
    lists:sort(fun ({A,_,_},{B,_,_}) ->
	if 
	    A > B -> true;
	    true -> false 
	end
    end, Unsorted).
    	

%% Generate lists of receive waiting times per mfa
%% Out:
%%	ListedMfas = [mfa()]
%% Intrisnic:
%%	get({waiting, mfa()}) ->
%%	[{waiting, mfa()}, {Total, [WaitingTime]})
%%	WaitingTime = float()

waiting_activities_mfa_list([], ListedMfas) -> ListedMfas;
waiting_activities_mfa_list([Activity|Activities], ListedMfas) ->
    #activity{id = Pid, state = Act, timestamp = Time, where = MFA} = Activity,
    case Act of 
    	active ->
	    waiting_activities_mfa_list(Activities, ListedMfas);
	inactive ->
	    % Want to know how long the wait is in a receive,
	    % it is given via the next activity
	    case Activities of
	    	[] -> 
		    [Info] = percept_db:select(information, Pid),
		    case Info#information.stop of
			undefined ->
			% get profile end time
			    Waited = ?seconds(
				percept_db:select({system,stop_ts}),
				Time);
			Time2 ->
			    Waited = ?seconds(Time2, Time)
		    end,
		    case get({waiting_mfa, MFA}) of 
			undefined ->
			    put({waiting_mfa, MFA}, {Waited, [Waited]}),
			    [MFA | ListedMfas];
		    	{Total, TimedMfa} ->
			    put({waiting_mfa, MFA}, {Total + Waited, [Waited | TimedMfa]}),
			    ListedMfas
		    end;
		[#activity{timestamp=Time2, id = Pid, state = active} | _ ] ->
		    % Calculate waiting time
		    Waited = ?seconds(Time2, Time),
		    % Get previous entry

		    case get({waiting_mfa, MFA}) of
		    	undefined ->
			    % add entry to list
			    put({waiting_mfa, MFA}, {Waited, [Waited]}),
			    waiting_activities_mfa_list(Activities, [MFA|ListedMfas]);
			{Total, TimedMfa} ->
			    put({waiting_mfa, MFA}, {Total + Waited, [Waited | TimedMfa]}),
			    waiting_activities_mfa_list(Activities, ListedMfas)
		    end;
		 _ -> error
	    end
    end.

%% seconds2ts(Seconds, StartTs) -> TS
%% In:
%%	Seconds = float()
%%	StartTs = timestamp()
%% Out:
%%	TS = timestamp()

%% @spec seconds2ts(float(), StartTs::{integer(),integer(),integer()}) -> timestamp()
%% @doc Calculates a timestamp given a duration in seconds and a starting timestamp. 

seconds2ts(Seconds, {Ms, S, Us}) ->
    % Calculate mega seconds integer
    MsInteger = trunc(Seconds) div 1000000 ,

    % Calculate the reminder for seconds
    SInteger  = trunc(Seconds),

    % Calculate the reminder for micro seconds
    UsInteger = trunc((Seconds - SInteger) * 1000000),

    % Wrap overflows

    UsOut = (UsInteger + Us) rem 1000000,
    SOut  = ((SInteger + S) + (UsInteger + Us) div 1000000) rem 1000000,
    MsOut = (MsInteger+ Ms) + ((SInteger + S) + (UsInteger + Us) div 1000000) div 1000000,

    {MsOut, SOut, UsOut}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Analyze interval for concurrency
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec analyze_activities(integer(), [#activity{}]) -> [{integer(),#activity{}}]
%% @hidden

analyze_activities(Threshold, Activities) ->
    RunnableCount = runnable_count(Activities, 0),
    analyze_runnable_activities(Threshold, RunnableCount).


%% runnable_count(Activities, StartValue) -> RunnableCount
%% In:
%% 	Activities = [activity()]
%%	StartValue = integer()
%% Out:
%%	RunnableCount = [{integer(), activity()}]
%% Purpose:
%%	Calculate the runnable count of a given interval of generic
%% 	activities.

%% @spec runnable_count([#activity{}]) -> [{integer(),#activity{}}]
%% @hidden

runnable_count(Activities) ->
    Threshold = runnable_count_threshold(Activities),
    runnable_count(Activities, Threshold, []).

runnable_count_threshold(Activities) ->
    CountedActs = runnable_count(Activities, 0),
    Counts      = [C || {C, _} <- CountedActs],
    Min         = lists:min(Counts),
    0 - Min.
%% @spec runnable_count([#activity{}],integer()) -> [{integer(),#activity{}}]
%% @hidden

runnable_count(Activities, StartCount) when is_integer(StartCount) ->
    runnable_count(Activities, StartCount, []).
runnable_count([], _ , Out) ->
    lists:reverse(Out);
runnable_count([A | As], PrevCount, Out) ->
    case A#activity.state of 
	active ->
	    runnable_count(As, PrevCount + 1, [{PrevCount + 1, A} | Out]);
	inactive ->
	    runnable_count(As, PrevCount - 1, [{PrevCount - 1, A} | Out])
    end.

%% In:
%%	Threshold = integer(),
%%	RunnableActivities = [{Rc, activity()}]
%%	Rc = integer()

analyze_runnable_activities(Threshold, RunnableActivities) ->
    analyze_runnable_activities(Threshold, RunnableActivities, []).

analyze_runnable_activities( _z, [], Out) -> 
    lists:reverse(Out);
analyze_runnable_activities(Threshold, [{Rc, Act} | RunnableActs], Out) ->
    if 
	Rc =< Threshold ->
	    analyze_runnable_activities(Threshold, RunnableActs, [{Rc,Act} | Out]);
	true ->
	    analyze_runnable_activities(Threshold, RunnableActs, Out)
    end.

%% minmax_activity(Activities, Count) -> {Min, Max}
%% In:
%%	Activities = [activity()]
%%	InitialCount = non_neg_integer()
%% Out:
%%	{Min, Max}
%%	Min = non_neg_integer()
%%	Max = non_neg_integer()
%% Purpose:
%% 	Minimal and maximal activity during an activity interval.
%%	Initial activity count needs to be supplied.	 

%% @spec minmax_activities([#activity{}], integer()) -> {integer(), integer()}
%% @doc	Calculates the minimum and maximum of runnable activites (processes
%	and ports) during the interval of reffered by the activity list.

minmax_activities(Activities, Count) ->
    minmax_activities(Activities, Count, {Count, Count}).
minmax_activities([], _, Out) -> 
    Out;
minmax_activities([A|Acts], Count, {Min, Max}) ->
    case A#activity.state of
	active ->
	   minmax_activities(Acts, Count + 1, {Min, lists:max([Count + 1, Max])});
	inactive ->
	   minmax_activities(Acts, Count - 1, {lists:min([Count - 1, Min]), Max})
    end.
