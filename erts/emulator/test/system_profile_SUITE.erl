%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2017. All Rights Reserved.
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


%%% Purpose : Tests system_profile BIF

-module(system_profile_SUITE).

-export([all/0, suite/0,
	system_profile_on_and_off/1,
	runnable_procs/1, runnable_ports/1,
	dont_profile_profiler/1,
	scheduler/1, sane_location/1]).

-export([profiler_process/1, ring_loop/1, port_echo_start/0, 
	 list_load/0, run_load/2]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [system_profile_on_and_off, runnable_procs,
     runnable_ports, scheduler, dont_profile_profiler,
     sane_location].

%% No specification clause needed for an init function in a conf case!!!

%% Test switching system_profiling on and off.
system_profile_on_and_off(Config) when is_list(Config) ->
    Pid = start_profiler_process(),
    
    % Test runnable_ports on and off
    undefined = erlang:system_profile(Pid, [runnable_ports]),
    {Pid, [runnable_ports]} = erlang:system_profile(),
    {Pid, [runnable_ports]} = erlang:system_profile(undefined, []),

    % Test runnable_procs on and off
    undefined = erlang:system_profile(Pid, [runnable_procs]),
    {Pid, [runnable_procs]} = erlang:system_profile(),
    {Pid, [runnable_procs]} = erlang:system_profile(undefined, []),

    % Test scheduler on and off
    undefined = erlang:system_profile(Pid, [scheduler]),
    {Pid, [scheduler]} = erlang:system_profile(),
    {Pid, [scheduler]} = erlang:system_profile(undefined, []),

    % Test combined runnable_ports, runnable_procs, scheduler; on and off
    undefined = erlang:system_profile(Pid, [scheduler, runnable_procs, runnable_ports]),
    {Pid, [scheduler,runnable_procs,runnable_ports]} = erlang:system_profile(),
    {Pid, [scheduler,runnable_procs,runnable_ports]} = erlang:system_profile(undefined, []),

    % Test turned off and kill process
    undefined = erlang:system_profile(),
    exit(Pid,kill),
    ok.

%% Tests system_profiling with runnable_procs.
runnable_procs(Config) when is_list(Config) ->
    lists:foreach(fun (TsType) ->
			  Arg = case TsType of
				    no_timestamp ->
					{timestamp, []};
				    _ ->
					{TsType, [TsType]}
				end,
			  do_runnable_procs(Arg),
			  receive after 1000 -> ok end
		  end,
		  [no_timestamp, timestamp, monotonic_timestamp,
		   strict_monotonic_timestamp]).

do_runnable_procs({TsType, TsTypeFlag}) ->
    Pid = start_profiler_process(),
    % start a ring of processes
    % FIXME: Set #laps and #nodes in config file
    Nodes = 10,
    Laps = 10,
    Master = ring(Nodes),
    undefined = erlang:system_profile(Pid, [runnable_procs]++TsTypeFlag),
    % loop a message
    ok = ring_message(Master, message, Laps),
    Events = get_profiler_events(),
    kill_em_all = kill_ring(Master),
    erlang:system_profile(undefined, []),
    put(master, Master),
    put(laps, Laps),
    true = has_runnable_event(TsType, Events),
    Pids = sort_events_by_pid(Events),
    ok = check_events(TsType, Pids),
    erase(),
    exit(Pid,kill),
    ok.

%% Tests system_profiling with runnable_port.
runnable_ports(Config) when is_list(Config) ->
    lists:foreach(fun (TsType) ->
			  Arg = case TsType of
				    no_timestamp ->
					{timestamp, []};
				    _ ->
					{TsType, [TsType]}
				end,
			  do_runnable_ports(Arg, Config),
			  receive after 1000 -> ok end
		  end,
		  [no_timestamp, timestamp, monotonic_timestamp,
		   strict_monotonic_timestamp]).

do_runnable_ports({TsType, TsTypeFlag}, Config) ->
    Pid = start_profiler_process(),
    undefined = erlang:system_profile(Pid, [runnable_ports]++TsTypeFlag),
    EchoPid = echo(Config),
    % FIXME: Set config to number_of_echos
    Laps = 10,
    put(laps, Laps),
    ok = echo_message(EchoPid, Laps, message),
    Events = get_profiler_events(),
    kill_em_all = kill_echo(EchoPid),
    erlang:system_profile(undefined, []),
    true = has_runnable_event(TsType, Events),
    Pids = sort_events_by_pid(Events),
    ok = check_events(TsType, Pids),
    erase(),
    exit(Pid,kill),
    ok.

%% Tests system_profiling with scheduler.
scheduler(Config) when is_list(Config) ->
    case erlang:system_info(schedulers_online) of
	1 -> {skipped, "No need for scheduler test when only one scheduler online."};
	_ ->
	    Nodes = 10,
	    lists:foreach(fun (TsType) ->
				  Arg = case TsType of
					    no_timestamp ->
						{timestamp, []};
					    _ ->
						{TsType, [TsType]}
					end,
				  ok = check_block_system(Arg, Nodes),
				  ok = check_multi_scheduling_block(Arg, Nodes),
				  receive after 1000 -> ok end
			      end,
			  [no_timestamp, timestamp, monotonic_timestamp,
			   strict_monotonic_timestamp])
    end.

%% Ensure system profiler process is not profiled.
dont_profile_profiler(Config) when is_list(Config) ->
    Pid = start_profiler_process(),

    Nodes = 10,
    Laps = 10,
    Master = ring(Nodes),
    undefined = erlang:system_profile(Pid, [runnable_procs]),
    % loop a message
    ok = ring_message(Master, message, Laps),
    erlang:system_profile(undefined, []),
    kill_em_all = kill_ring(Master),
    Events = get_profiler_events(),
    false  = has_profiler_pid_event(Events, Pid),

    exit(Pid,kill),
    ok.

%% Check sane location (of exits)
sane_location(Config) when is_list(Config) ->
    Check = spawn_link(fun() -> flush_sane_location() end),
    erlang:system_profile(Check, [runnable_procs]),
    Me = self(),
    Pids = [spawn_link(fun() -> wat(Me) end) || _ <- lists:seq(1,100)],
    [receive {Pid,ok} -> ok end || Pid <- Pids],
    Check ! {Me, done},
    receive {Check,ok} -> ok end,
    ok.

wat(Pid) ->
    Pid ! {self(), ok}.

flush_sane_location() ->
    receive
        {profile,_,_,{M,F,A},_} when is_atom(M), is_atom(F),
                                     is_integer(A) ->
            flush_sane_location();
        {profile,_,_,0,_} ->
            flush_sane_location();
        {Pid,done} when is_pid(Pid) ->
            Pid ! {self(), ok};
        M ->
            ct:fail({badness,M})
    end.


%%% Check scheduler profiling

check_multi_scheduling_block({TsType, TsTypeFlag}, Nodes) ->
    Pid = start_profiler_process(),
    undefined = erlang:system_profile(Pid, [scheduler]++TsTypeFlag),
    {ok, Supervisor} = start_load(Nodes),
    wait(600),
    erlang:system_flag(multi_scheduling, block),
    wait(600),
    erlang:system_flag(multi_scheduling, unblock),
    {Pid, [scheduler]} = erlang:system_profile(undefined, []),
    Events = get_profiler_events(),
    true = has_scheduler_event(TsType, Events),
    stop_load(Supervisor),
    exit(Pid,kill),
    erase(),
    ok.

check_block_system({TsType, TsTypeFlag}, Nodes) ->
    Dummy = spawn(?MODULE, profiler_process, [[]]),
    Pid = start_profiler_process(),
    undefined = erlang:system_profile(Pid, [scheduler]++TsTypeFlag),
    {ok, Supervisor} = start_load(Nodes),
    wait(300),
    undefined = erlang:system_monitor(Dummy, [busy_port]),
    {Dummy, [busy_port]} = erlang:system_monitor(undefined, []),
    {Pid, [scheduler]} = erlang:system_profile(undefined, []),
    Events = get_profiler_events(),
    true = has_scheduler_event(TsType, Events),
    stop_load(Supervisor),
    exit(Pid,kill),
    exit(Dummy,kill),
    erase(),
    ok.

%%% Check events

check_events(_TsType, []) -> ok;
check_events(TsType, [Pid | Pids]) ->
    Master = get(master),
    Laps = get(laps),
    CheckPids = get(pids),
    {Events, N} = get_pid_events(Pid),
    ok = check_event_flow(Events),
    ok = check_event_ts(TsType, Events),
    IsMember = lists:member(Pid, CheckPids),
    case Pid of
    	Master ->
	    io:format("Expected ~p and got ~p profile events from ~p: ok~n", [Laps*2+2, N, Pid]),
	    N = Laps*2 + 2,
    	    check_events(TsType, Pids);
	Pid when IsMember == true ->
	    io:format("Expected ~p and got ~p profile events from ~p: ok~n", [Laps*2, N, Pid]),
	    N = Laps*2,
    	    check_events(TsType, Pids);
	Pid ->
	    check_events(TsType, Pids)
    end.

%% timestamp consistency check for descending timestamps

check_event_ts(TsType, Events) ->
    check_event_ts(TsType, Events, undefined).
check_event_ts(_TsType, [], _) -> ok;
check_event_ts(TsType, [Event | Events], undefined) ->
    check_event_ts(TsType, Events, Event);
check_event_ts(TsType, [{Pid, _, _, TS1}=Event | Events], {Pid,_,_,TS0}) ->
    Time = case TsType of
	       timestamp ->
		   timer:now_diff(TS1, TS0);
	       monotonic_timestamp ->
		   TS1 - TS0;
	       strict_monotonic_timestamp ->
		   {MT1, _} = TS1,
		   {MT0, _} = TS0,
		   MT1 - MT0
	   end,
    if 
    	Time < 0.0 -> timestamp_error;
    	true -> check_event_ts(TsType, Events, Event)
    end.

%% consistency check for active vs. inactive activity (runnable)

check_event_flow(Events) ->
    check_event_flow(Events, undefined).
check_event_flow([], _) -> ok;
check_event_flow([Event | PidEvents], undefined) ->
    check_event_flow(PidEvents, Event);
check_event_flow([{Pid,Act,_,_}=Event | Events], PrevEvent) ->
    case PrevEvent of
    	{Pid, Act, _MFA, _TS} -> consistency_error;
	_ -> check_event_flow(Events, Event)
    end.
    


get_pid_events(Pid) -> 
    Events = get({pid_events, Pid}),
    {Events, length(Events)}.

sort_events_by_pid(Events) ->
    sort_events_by_pid(lists:reverse(Events), []).
sort_events_by_pid([], Pids) -> Pids;
sort_events_by_pid([Event | Events],Pids) ->
    case Event of 
    	{profile,Pid,Act,MFA,TS} ->
	    case get({pid_events, Pid}) of 
	    	undefined ->
		    put({pid_events, Pid}, [{Pid,Act,MFA,TS}]),
		    sort_events_by_pid(Events, [Pid | Pids]);
		PidEvents ->
		    put({pid_events, Pid}, [{Pid,Act,MFA,TS}|PidEvents]),
		    sort_events_by_pid(Events, Pids)
	    end
    end.


%%%
%% Process ring
%%%

%% API

% Returns master pid
ring(N) -> 
    Pids = build_ring(N, []),
    put(pids, Pids),
    setup_ring(Pids).

ring_message(Master, Message, Laps) ->
    Master ! {message, Master, Laps, Message},
    receive
    	{laps_complete, Master} -> ok
    end.

kill_ring(Master) -> Master ! kill_em_all.

%% Process ring helpers

build_ring(0, Pids) -> Pids;
build_ring(N, Pids) ->
    build_ring(N - 1, [spawn_link(?MODULE, ring_loop, [undefined]) | Pids]).

setup_ring([Master | Relayers]) ->
    % Relayers may not include the master pid
    Master ! {setup_ring, Relayers, self()},
    receive
        {setup_complete, Master} -> Master
    end.

ring_loop(RelayTo) ->
    receive
        kill_em_all ->
            RelayTo ! kill_em_all;
        {setup_ring, [Pid | Pids], Supervisor} ->
            put(supervisor, Supervisor),
            Pid ! {relay_to, Pids, self()},
            ring_loop(Pid);
        {setup_complete, _} ->
            get(supervisor) ! {setup_complete, self()},
            ring_loop(RelayTo);
        {relay_to, [], Master} ->
            Master ! {setup_complete, self()},
            ring_loop(Master);
        {relay_to, [Pid | Pids], Master} ->
            Pid ! {relay_to, Pids, Master},
            ring_loop(Pid);
        {message, Master, Lap, Msg}=Message ->
            case {self(), Lap} of
                {Master, 0} ->
                    get(supervisor) ! {laps_complete, self()},
                    ring_loop(RelayTo);
                {Master, Lap} ->
                    RelayTo ! {message, Master, Lap - 1, Msg},
                    ring_loop(RelayTo);
                _ ->
                    RelayTo ! Message,
                    ring_loop(RelayTo)
            end
    end.

%%%
%% Echo driver
%%%

%% API

echo(Config) ->
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:load_driver(Path, echo_drv),
    Pid = spawn_link(?MODULE, port_echo_start, []),
    Pid ! {self(), get_ports},
    receive
	{port, Port} ->
	    put(pids, [Port]),
    	    put(master, Port),
	    Pid
    end.

echo_message(Pid, N, Msg) -> 
    Pid ! {start_echo, self(), N, Msg},
    receive
	{echo_complete, Pid} -> ok
    end.

kill_echo(Pid) -> Pid ! kill_em_all.


%% Echo driver helpers
port_echo_start() ->
    Port = open_port({spawn,echo_drv}, [eof,binary]),
    receive
	{Pid, get_ports} ->
    	    Pid ! {port, Port},
	    port_echo_loop(Port)
    end.

port_echo_loop(Port) ->
    receive
	{start_echo, Pid, Echos, Msg} ->
	    port_command(Port, term_to_binary({Pid, Echos, Msg})),
	    port_echo_loop(Port);
	{Port, {data, Data}} ->
	    {Pid, Echos, Msg} = binary_to_term(Data),
	    case Echos of
	    	0 ->
		    Pid ! {echo_complete, self()},
		    port_echo_loop(Port);
		Echos ->
	    	    port_command(Port, term_to_binary({Pid, Echos - 1, Msg})),
		    port_echo_loop(Port)
	    end;
	kill_em_all -> 
	    port_close(Port),
	    ok
    end.



%%%
%% Helpers
%%%

check_ts(no_timestamp, Ts) ->
    try
	no_timestamp = Ts
    catch
	_ : _ ->
	    ct:fail({unexpected_timestamp, Ts})
    end,
    ok;
check_ts(timestamp, Ts) ->
    try
	{Ms,S,Us} = Ts,
	true = is_integer(Ms),
	true = is_integer(S),
	true = is_integer(Us)
    catch
	_ : _ ->
	    ct:fail({unexpected_timestamp, Ts})
    end,
    ok;
check_ts(monotonic_timestamp, Ts) ->
    try
	true = is_integer(Ts)
    catch
	_ : _ ->
	    ct:fail({unexpected_timestamp, Ts})
    end,
    ok;
check_ts(strict_monotonic_timestamp, Ts) ->
    try
	{MT, UMI} = Ts,
	true = is_integer(MT),
	true = is_integer(UMI)
    catch
	_ : _ ->
	    ct:fail({unexpected_timestamp, Ts})
    end,
    ok.

start_load(N) ->
   Pid = spawn_link(?MODULE, run_load, [N, []]),
   {ok, Pid}.


stop_load(Supervisor) ->
    erlang:unlink(Supervisor),
    exit(Supervisor, kill).

run_load(0, _Pids) ->
    receive
	    % wait for an exit signal or a message then kill
	    % all associated processes.
	    _ -> exit(annihilated)
    end;
run_load(N, Pids) ->
    Pid = spawn_link(?MODULE, list_load, []),
    run_load(N - 1, [Pid | Pids]).

list_load() -> 
    ok = case math:sin(rand:uniform(32451)) of
    	A when is_float(A) -> ok;
	_ -> ok
    end,
    list_load().

has_scheduler_event(TsType, Events) ->
    lists:any(
    	fun (Pred) ->
	    case Pred of 
	    	{profile, scheduler, _ID, _Activity, _NR, TS} ->
		    check_ts(TsType, TS),
		    true;
		_ -> false
	    end
	end, Events).

has_runnable_event(TsType, Events) ->
    lists:any(
    	fun (Pred) ->
	    case Pred of
	    	{profile, _Pid, _Activity, _MFA, TS} ->
		    check_ts(TsType, TS),
		    true;
	    	_ -> false
	    end
        end, Events).

has_profiler_pid_event([], _) ->
    false;
has_profiler_pid_event([{profile, Pid, _Activity, _MFA, _TS}|_Events], Pid) ->
    true;
has_profiler_pid_event([_|Events], Pid) ->
    has_profiler_pid_event(Events, Pid).


wait(Time) -> receive after Time -> ok end.

%%%
%%  Receivers
%%%

%% Process receiver


get_profiler_events() ->
    Pid = get(profiler),
    Pid ! {self(), get_events},
    receive
    	Events -> Events
    end.

start_profiler_process() ->
    Pid = spawn(?MODULE, profiler_process, [[]]),
    put(profiler, Pid),
    Pid.

profiler_process(Events) ->
    receive 
	{Pid, get_events} -> 
	    Ref = erlang:trace_delivered(all),
	    profiler_process_followup(Pid, Events, Ref);
	Event -> 
	    profiler_process([Event | Events])
    end.

profiler_process_followup(Pid, Events, Ref) ->
    receive
	{trace_delivered,all,Ref} ->
	    Pid ! lists:reverse(Events);
	Event -> 
	    profiler_process_followup(Pid, [Event | Events], Ref)
    end.

%% Port receiver


