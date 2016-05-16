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
%% @doc Percept database.
%%	
%% 

-module(percept_db).

-export([start/0,
         stop/0,
         insert/1,
         select/2,
         select/1,
         consolidate/0]).

-include("percept.hrl").
-define(STOP_TIMEOUT, 1000).
%%==========================================================================
%% Type definitions
%%==========================================================================

%% @type activity_option() = 
%%	{ts_min, timestamp()} | 
%%	{ts_max, timestamp()} | 
%%	{ts_exact, bool()} | 
%%	{mfa, {atom(), atom(), byte()}} | 
%%	{state, active | inactive} | 
%%	{id, all | procs | ports | pid() | port()}

%% @type scheduler_option() =
%%	{ts_min, timestamp()} | 
%%	{ts_max, timestamp()} |
%%	{ts_exact, bool()} |
%%	{id, scheduler_id()}

%% @type system_option() = start_ts | stop_ts

%% @type information_option() =
%%	all | procs | ports | pid() | port()




%%==========================================================================
%% Interface functions
%%==========================================================================

%% @spec start() -> ok | {started, Pid} | {restarted, Pid}
%%	Pid = pid()
%% @doc Starts or restarts the percept database.

-spec start() -> {'started', pid()} | {'restarted', pid()}.

start() ->
    case erlang:whereis(percept_db) of
    	undefined ->
	    {started, do_start()};
	PerceptDB ->
	    {restarted, restart(PerceptDB)}
    end.

%% @spec restart(pid()) -> pid()
%% @private
%% @doc restarts the percept database.

-spec restart(pid())-> pid().

restart(PerceptDB)->
        stop_sync(PerceptDB),
        do_start().

%% @spec do_start() -> pid()
%% @private
%% @doc starts the percept database.

-spec do_start()-> pid().

do_start()->
    Pid = spawn(fun() -> init_percept_db() end),
    erlang:register(percept_db, Pid),
    Pid.

%% @spec stop() -> not_started | {stopped, Pid}
%%	Pid = pid()
%% @doc Stops the percept database.

-spec stop() -> 'not_started' | {'stopped', pid()}.

stop() ->
    case erlang:whereis(percept_db) of
    	undefined -> 
	    not_started;
	Pid -> 
	    Pid ! {action, stop},
	    {stopped, Pid}
    end.

%% @spec stop_sync(pid()) -> true
%% @private
%% @doc Stops the percept database, with a synchronous call.

-spec stop_sync(pid()) -> true.

stop_sync(Pid) ->
    MonitorRef = erlang:monitor(process, Pid),
    _ = stop(),
    receive
        {'DOWN', MonitorRef, _Type, Pid, _Info}->
            true
    after ?STOP_TIMEOUT->
              erlang:demonitor(MonitorRef, [flush]),
              exit(Pid, kill)
    end.

%% @spec insert(tuple()) -> ok
%% @doc Inserts a trace or profile message to the database.  

insert(Trace) -> 
    percept_db ! {insert, Trace},
    ok.


%% @spec select({atom(), Options}) -> Result
%% @doc Synchronous call. Selects information based on a query.
%% 
%% <p>Queries:</p>
%% <pre>
%% {system, Option}
%%	Option = system_option()
%%	Result = timestamp() 
%% {information, Options}
%%	Options = [information_option()]
%%	Result = [#information{}] 
%% {scheduler, Options}
%%	Options = [sceduler_option()]
%%	Result = [#activity{}]
%% {activity, Options}
%%	Options = [activity_option()]
%%	Result = [#activity{}]
%% </pre>
%% <p>
%% Note: selection of Id's are always OR all other options are considered AND.
%% </p>

select(Query) -> 
    percept_db ! {select, self(), Query},
    receive {result, Match} -> Match end.

%% @spec select(atom(), list()) -> Result
%% @equiv select({Table,Options}) 

select(Table, Options) -> 
    percept_db ! {select, self(), {Table, Options}},
    receive {result, Match} -> Match end.

%% @spec consolidate() -> Result
%% @doc Checks timestamp and state-flow inconsistencies in the
%%	the database.

consolidate() ->
    percept_db ! {action, consolidate},
    ok.

%%==========================================================================
%% Database loop
%%==========================================================================

init_percept_db() ->
    % Proc and Port information
    pdb_info = ets:new(pdb_info, [named_table, private, {keypos, #information.id}, set]),

    % Scheduler runnability
    pdb_scheduler = ets:new(pdb_scheduler, [named_table, private, {keypos, #activity.timestamp}, ordered_set]),
    
    % Process and Port runnability
    pdb_activity = ets:new(pdb_activity, [named_table, private, {keypos, #activity.timestamp}, ordered_set]),
    
    % System status
    pdb_system = ets:new(pdb_system, [named_table, private, {keypos, 1}, set]),
    
    % System warnings
    pdb_warnings = ets:new(pdb_warnings, [named_table, private, {keypos, 1}, ordered_set]),
    put(debug, 0),
    loop_percept_db().

loop_percept_db() ->
    receive
	{insert, Trace} ->
	    insert_trace(clean_trace(Trace)),
	    loop_percept_db();
	{select, Pid, Query} ->
	    Pid ! {result, select_query(Query)},
	    loop_percept_db();
	{action, stop} -> 
	    stopped;
	{action, consolidate} ->
	    consolidate_db(),
	    loop_percept_db();
        {operate, Pid, {Table, {Fun, Start}}} ->
	    Result = ets:foldl(Fun, Start, Table),
	    Pid ! {result, Result},
	    loop_percept_db();
	Unhandled -> 
	    io:format("loop_percept_db, unhandled query: ~p~n", [Unhandled]),
	    loop_percept_db()
    end.

%%==========================================================================
%% Auxiliary functions
%%==========================================================================

%% cleans trace messages from external pids

clean_trace(Trace) when is_tuple(Trace) -> list_to_tuple(clean_trace(tuple_to_list(Trace)));
clean_trace(Trace) when is_list(Trace) -> clean_list(Trace, []);
clean_trace(Trace) when is_pid(Trace) ->
    PidStr = pid_to_list(Trace),
    [_,P2,P3p] = string:tokens(PidStr,"."),
    P3 = lists:sublist(P3p, 1, length(P3p) - 1),
    erlang:list_to_pid("<0." ++ P2 ++ "." ++ P3 ++ ">");
clean_trace(Trace) -> Trace.

clean_list([], Out) -> lists:reverse(Out);
clean_list([Element|Trace], Out) ->
    clean_list(Trace, [clean_trace(Element)|Out]).


insert_trace(Trace) ->
    case Trace of
	{profile_start, Ts} ->
	    update_system_start_ts(Ts),
	    ok;
	{profile_stop, Ts} ->
	    update_system_stop_ts(Ts),
	    ok; 
        %%% erlang:system_profile, option: runnable_procs
    	%%% ---------------------------------------------
    	{profile, Id, State, Mfa, TS} when is_pid(Id) ->
	    % Update runnable count in activity and db
	    
	    case check_activity_consistency(Id, State) of
		invalid_state -> 
		    ignored;
		ok ->
		    Rc = get_runnable_count(procs, State),
		    % Update registered procs
		    % insert proc activity
		    update_activity(#activity{
		    	id = Id, 
			state = State, 
			timestamp = TS, 
			runnable_count = Rc,
			where = Mfa}),
		    ok
            end;
	%%% erlang:system_profile, option: runnable_ports
    	%%% ---------------------------------------------
	{profile, Id, State, Mfa, TS} when is_port(Id) ->
	    case check_activity_consistency(Id, State) of
		invalid_state -> 
		    ignored;
		ok ->
		    % Update runnable count in activity and db
		    Rc = get_runnable_count(ports, State),
	    
		    % Update registered ports
		    % insert port activity
		    update_activity(#activity{
		    	id = Id, 
			state = State, 
			timestamp = TS, 
			runnable_count = Rc,
			where = Mfa}),

		    ok
	    end;
	%%% erlang:system_profile, option: scheduler
	{profile, scheduler, Id, State, Scheds, Ts} ->
	    % insert scheduler activity
	    update_scheduler(#activity{
	    	id = {scheduler, Id}, 
		state = State, 
		timestamp = Ts, 
		where = Scheds}),
	    ok;

	%%% erlang:trace, option: procs
	%%% ---------------------------
	{trace_ts, Parent, spawn, Pid, Mfa, TS} ->
	    InformativeMfa = mfa2informative(Mfa),
	    % Update id_information
	    update_information(#information{id = Pid, start = TS, parent = Parent, entry = InformativeMfa}),
	    update_information_child(Parent, Pid),
	    ok;
	{trace_ts, Pid, exit, _Reason, TS} ->
	    % Update registered procs
	    
	    % Update id_information
	    update_information(#information{id = Pid, stop = TS}),
	    
	    ok;
	{trace_ts, Pid, register, Name, _Ts} when is_pid(Pid) ->
	    % Update id_information
	    update_information(#information{id = Pid, name = Name}),
	    ok;
	{trace_ts, Pid, register, Name, _Ts} when is_pid(Pid) ->
	    % Update id_information
	    update_information(#information{id = Pid, name = Name}),
	    ok;
	{trace_ts, _Pid, unregister, _Name, _Ts} -> 
	    % Not implemented
	    ok;
	{trace_ts, Pid, getting_unlinked, _Id, _Ts} when is_pid(Pid) ->
	    % Update id_information
	    ok;
	{trace_ts, Pid, getting_linked, _Id, _Ts} when is_pid(Pid)->
	    % Update id_information
	    ok;
	{trace_ts, Pid, link, _Id, _Ts} when is_pid(Pid)->
	    % Update id_information
	    ok;
	{trace_ts, Pid, unlink, _Id, _Ts} when is_pid(Pid) ->
	    % Update id_information
	    ok;

	%%% erlang:trace, option: ports
	%%% ----------------------------
	{trace_ts, Caller, open, Port, Driver, TS} ->
	    % Update id_information
	    update_information(#information{
	    	id = Port, entry = Driver, start = TS, parent = Caller}),
	    ok;
	{trace_ts, Port, closed, _Reason, Ts} ->
	    % Update id_information
	    update_information(#information{id = Port, stop = Ts}),
	    ok;

	Unhandled ->
	    io:format("insert_trace, unhandled: ~p~n", [Unhandled])
    end.

mfa2informative({erlang, apply, [M, F, Args]})  -> mfa2informative({M, F,Args});
mfa2informative({erlang, apply, [Fun, Args]}) ->
    FunInfo = erlang:fun_info(Fun), 
    M = case proplists:get_value(module, FunInfo, undefined) of
	    []        -> undefined_fun_module;
	    undefined -> undefined_fun_module;
	    Module    -> Module
	end,
    F = case proplists:get_value(name, FunInfo, undefined) of
	    []        -> undefined_fun_function;
	    undefined -> undefined_fun_function;
	    Function  -> Function
	end,
    mfa2informative({M, F, Args});
mfa2informative(Mfa) -> Mfa.

%% consolidate_db() -> bool()
%% Purpose:
%%	Check start/stop time
%%	Activity consistency

consolidate_db() ->
    io:format("Consolidating...~n"),
    % Check start/stop timestamps
    case select_query({system, start_ts}) of
	undefined ->
	    Min = lists:min(list_all_ts()),
	    update_system_start_ts(Min);
	_ -> ok
    end,
    case select_query({system, stop_ts}) of
	undefined ->
	    Max = lists:max(list_all_ts()),
	    update_system_stop_ts(Max);
	_ -> ok
    end,
    consolidate_runnability(),
    ok.

consolidate_runnability() ->
    put({runnable, procs}, undefined),
    put({runnable, ports}, undefined),
    consolidate_runnability_loop(ets:first(pdb_activity)).

consolidate_runnability_loop('$end_of_table') -> ok;
consolidate_runnability_loop(Key) ->
    case ets:lookup(pdb_activity, Key) of
	[#activity{id = Id, state = State } = A] when is_pid(Id) ->
	    Rc = get_runnable_count(procs, State),
	    ets:insert(pdb_activity, A#activity{ runnable_count = Rc});
	[#activity{id = Id, state = State } = A] when is_port(Id) ->
	    Rc = get_runnable_count(ports, State),
	    ets:insert(pdb_activity, A#activity{ runnable_count = Rc});
	_ -> throw(consolidate)
    end,
    consolidate_runnability_loop(ets:next(pdb_activity, Key)).

list_all_ts() ->
    ATs = [Act#activity.timestamp || Act <- select_query({activity, []})],
    STs = [Act#activity.timestamp || Act <- select_query({scheduler, []})],
    ITs = lists:flatten([
	[I#information.start, 
	 I#information.stop] || 
	 I <- select_query({information, all})]),
    %% Filter out all undefined (non ts)
    [Elem || Elem = {_,_,_} <- ATs ++ STs ++ ITs].

%% get_runnable_count(Type, State) -> RunnableCount
%% In: 
%%	Type = procs | ports
%%	State = active | inactive
%% Out:
%%	RunnableCount = integer()
%% Purpose:
%%	Keep track of the number of runnable ports and processes
%%	during the profile duration.

get_runnable_count(Type, State) ->
    case {get({runnable, Type}), State} of 
    	{undefined, active} -> 
	    put({runnable, Type}, 1),
	    1;
	{N, active} ->
	    put({runnable, Type}, N + 1),
	    N + 1;
	{N, inactive} ->
	    put({runnable, Type}, N - 1),
	    N - 1;
	Unhandled ->
	    io:format("get_runnable_count, unhandled ~p~n", [Unhandled]),
	    Unhandled
    end.

check_activity_consistency(Id, State) ->
    case get({previous_state, Id}) of
	State ->
	    io:format("check_activity_consistency, state flow invalid.~n"),
	    invalid_state;
	undefined when State == inactive -> 
	    invalid_state;
	_ ->
	    put({previous_state, Id}, State),
	    ok
    end.
%%%
%%% select_query
%%% In:
%%%	Query = {Table, Option}
%%%	Table = system | activity | scheduler | information


select_query(Query) ->
    case Query of
	{system, _ } -> 
	    select_query_system(Query);
	{activity, _ } -> 
	    select_query_activity(Query);
    	{scheduler, _} ->
	    select_query_scheduler(Query);
	{information, _ } -> 
	    select_query_information(Query);
	Unhandled ->
	    io:format("select_query, unhandled: ~p~n", [Unhandled]),
	    []
    end.

%%% select_query_information

select_query_information(Query) ->
    case Query of
    	{information, all} -> 
	    ets:select(pdb_info, [{
		#information{ _ = '_'},
		[],
		['$_']
		}]);
	{information, procs} ->
	    ets:select(pdb_info, [{
		#information{ id = '$1', _ = '_'},
		[{is_pid, '$1'}],
		['$_']
		}]);
	{information, ports} ->
	    ets:select(pdb_info, [{
		#information{ id = '$1', _ = '_'},
		[{is_port, '$1'}],
		['$_']
		}]);
	{information, Id} when is_port(Id) ; is_pid(Id) -> 
	    ets:select(pdb_info, [{
		#information{ id = Id, _ = '_'},
		[],
		['$_']
		}]);
	Unhandled ->
	    io:format("select_query_information, unhandled: ~p~n", [Unhandled]),
	    []
    end.

%%% select_query_scheduler

select_query_scheduler(Query) ->
    case Query of
	{scheduler, Options} when is_list(Options) ->
	    Head = #activity{
	    	timestamp = '$1',
		id = '$2',
		state = '$3',
		where = '$4',
		_ = '_'},
	    Body = ['$_'],
	    % We don't need id's
	    {Constraints, _ } = activity_ms_and(Head, Options, [], []),
	    ets:select(pdb_scheduler, [{Head, Constraints, Body}]);
	Unhandled ->
	    io:format("select_query_scheduler, unhandled: ~p~n", [Unhandled]),
	    []
    end.

%%% select_query_system

select_query_system(Query) ->
    case Query of
    	{system, start_ts} ->
    	    case ets:lookup(pdb_system, {system, start_ts}) of
	    	[] -> undefined;
		[{{system, start_ts}, StartTS}] -> StartTS
	    end;
    	{system, stop_ts} ->
    	    case ets:lookup(pdb_system, {system, stop_ts}) of
	    	[] -> undefined;
		[{{system, stop_ts}, StopTS}] -> StopTS
	    end;
	Unhandled ->
	    io:format("select_query_system, unhandled: ~p~n", [Unhandled]),
	    []
    end.

%%% select_query_activity

select_query_activity(Query) ->
    case Query of
    	{activity, Options} when is_list(Options) ->
	    case lists:member({ts_exact, true},Options) of
		true ->
		    case catch select_query_activity_exact_ts(Options) of
			{'EXIT', Reason} ->
	    		    io:format(" - select_query_activity [ catch! ]: ~p~n", [Reason]),
			    [];
		    	Match ->
			    Match
		    end;		    
		false ->
		    MS = activity_ms(Options),
		    case catch ets:select(pdb_activity, MS) of
			{'EXIT', Reason} ->
	    		    io:format(" - select_query_activity [ catch! ]: ~p~n", [Reason]),
			    [];
		    	Match ->
			    Match
		    end
	    end;
	Unhandled ->
	    io:format("select_query_activity, unhandled: ~p~n", [Unhandled]),
    	    []
    end.

select_query_activity_exact_ts(Options) ->
    case { proplists:get_value(ts_min, Options, undefined), proplists:get_value(ts_max, Options, undefined) } of
	{undefined, undefined} -> [];
	{undefined, _        } -> [];
	{_        , undefined} -> [];
	{TsMin    , TsMax    } ->
	    % Remove unwanted options
	    Opts = lists_filter([ts_exact], Options),
	    Ms = activity_ms(Opts),
	    case ets:select(pdb_activity, Ms) of
		% no entries within interval
		[] -> 
		    Opts2 = lists_filter([ts_max, ts_min], Opts) ++ [{ts_min, TsMax}],
		    Ms2   = activity_ms(Opts2),
		    case ets:select(pdb_activity, Ms2, 1) of
			'$end_of_table' -> [];
			{[E], _}  -> 
			    [PrevAct] = ets:lookup(pdb_activity, ets:prev(pdb_activity, E#activity.timestamp)),
			    [PrevAct#activity{ timestamp = TsMin} , E] 
		    end;
		Acts ->
		    [Head| _] = Acts,
		    if
			Head#activity.timestamp == TsMin -> Acts;
			true ->
			    PrevTs = ets:prev(pdb_activity, Head#activity.timestamp),
			    case ets:lookup(pdb_activity, PrevTs) of
				[] -> Acts;
				[PrevAct] -> [PrevAct#activity{timestamp = TsMin}|Acts]
			    end
		    end
	    end
    end.

lists_filter([], Options) -> Options;
lists_filter([D|Ds], Options) ->
    lists_filter(Ds, lists:filter(
	fun ({Pred, _}) ->
	    if 
		Pred == D -> false;
		true      -> true
	    end
	end, Options)).

% Options:
% {ts_min, timestamp()}
% {ts_max, timestamp()}
% {mfa, mfa()}
% {state, active | inactive}
% {id, all | procs | ports | pid() | port()}
%
% All options are regarded as AND expect id which are regarded as OR
% For example: [{ts_min, TS1}, {ts_max, TS2}, {id, PID1}, {id, PORT1}] would be
% ({ts_min, TS1} and {ts_max, TS2} and {id, PID1}) or
% ({ts_min, TS1} and {ts_max, TS2} and {id, PORT1}).

activity_ms(Opts) ->
    % {activity, Timestamp, State, Mfa}
    Head = #activity{
    	timestamp = '$1',
	id = '$2',
	state = '$3',
	where = '$4',
	_ = '_'},

    {Conditions, IDs} = activity_ms_and(Head, Opts, [], []),
    Body = ['$_'],
    
    lists:foldl(
    	fun (Option, MS) ->
	    case Option of
		{id, ports} ->
	    	    [{Head, [{is_port, Head#activity.id} | Conditions], Body} | MS];
		{id, procs} ->
	    	    [{Head,[{is_pid, Head#activity.id} | Conditions], Body} | MS];
		{id, ID} when is_pid(ID) ; is_port(ID) ->
	    	    [{Head,[{'==', Head#activity.id, ID} | Conditions], Body} | MS];
		{id, all} ->
	    	    [{Head, Conditions,Body} | MS];
		_ ->
	    	    io:format("activity_ms id dropped ~p~n", [Option]),
	    	    MS
	    end
	end, [], IDs).

activity_ms_and(_, [], Constraints, []) ->
    {Constraints, [{id, all}]};
activity_ms_and(_, [], Constraints, IDs) -> 
    {Constraints, IDs};
activity_ms_and(Head, [Opt|Opts], Constraints, IDs) ->
    case Opt of
	{ts_min, Min} ->
	    activity_ms_and(Head, Opts, 
		[{'>=', Head#activity.timestamp, {Min}} | Constraints], IDs);
	{ts_max, Max} ->
	    activity_ms_and(Head, Opts, 
		[{'=<', Head#activity.timestamp, {Max}} | Constraints], IDs);
	{id, ID} ->
	    activity_ms_and(Head, Opts, 
		Constraints, [{id, ID} | IDs]);
	{state, State} ->
	    activity_ms_and(Head, Opts, 
		[{'==', Head#activity.state, State} | Constraints], IDs);
	{mfa, Mfa} ->
	    activity_ms_and(Head, Opts, 
		[{'==', Head#activity.where, {Mfa}}| Constraints], IDs);
	_ -> 
	    io:format("activity_ms_and option dropped ~p~n", [Opt]),
	    activity_ms_and(Head, Opts, Constraints, IDs)
    end.

% Information = information()

%%%
%%% update_information
%%%


update_information(#information{id = Id} = NewInfo) ->
    case ets:lookup(pdb_info, Id) of
    	[] ->
	    ets:insert(pdb_info, NewInfo),
	    ok;
	[Info] ->
	    % Remake NewInfo and Info to lists then substitute
	    % old values for new values that are not undefined or empty lists.
	    
	    {_, Result} = lists:foldl(
	    	fun (InfoElem, {[NewInfoElem | Tail], Out}) ->
		    case NewInfoElem of
		    	undefined ->
			    {Tail, [InfoElem | Out]};
		    	[] ->
			    {Tail, [InfoElem | Out]};
			NewInfoElem ->
			    {Tail, [NewInfoElem | Out]}
		    end
		end, {tuple_to_list(NewInfo), []}, tuple_to_list(Info)),
	    ets:insert(pdb_info, list_to_tuple(lists:reverse(Result))),
	    ok
    end.

update_information_child(Id, Child) -> 
    case ets:lookup(pdb_info, Id) of
    	[] ->
	    ets:insert(pdb_info,#information{
	    	id = Id,
		children = [Child]}),
	    ok;
	[I] ->
	    ets:insert(pdb_info,I#information{children = [Child | I#information.children]}),
	    ok
    end.

%%%
%%% update_activity
%%%
update_scheduler(Activity) ->
    ets:insert(pdb_scheduler, Activity).

update_activity(Activity) ->
    ets:insert(pdb_activity, Activity). 

%%%
%%% update_system_ts
%%%

update_system_start_ts(TS) ->
    case ets:lookup(pdb_system, {system, start_ts}) of
    	[] ->
	    ets:insert(pdb_system, {{system, start_ts}, TS});
	[{{system, start_ts}, StartTS}] ->
	    DT = ?seconds(StartTS, TS),
	    if 
		DT > 0.0 -> ets:insert(pdb_system, {{system, start_ts}, TS});
	    	true -> ok
	    end;
	Unhandled -> 
	    io:format("update_system_start_ts, unhandled ~p ~n", [Unhandled])
    end.
	
update_system_stop_ts(TS) ->
    case ets:lookup(pdb_system, {system, stop_ts}) of
    	[] ->
	    ets:insert(pdb_system, {{system, stop_ts}, TS});
	[{{system, stop_ts}, StopTS}] ->
	    DT = ?seconds(StopTS, TS),
	    if 
		DT < 0.0 -> ets:insert(pdb_system, {{system, stop_ts}, TS});
	  	true -> ok
	    end;
	Unhandled -> 
	    io:format("update_system_stop_ts, unhandled ~p ~n", [Unhandled])
    end.
