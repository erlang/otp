%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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


%%% Purpose : Tests system_profile BIF

-module(system_profile_SUITE).

-export([all/1,
	system_profile_on_and_off/1,
	runnable_procs/1,
	runnable_ports/1,
	scheduler/1
        ]).

-export([init_per_testcase/2, fin_per_testcase/2]).

-export([profiler_process/1, ring_loop/1, port_echo_start/0, list_load/0, run_load/2]).

-include("test_server.hrl").

-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog=?t:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
fin_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

all(suite) -> 
    %% Test specification on test suite level
    [system_profile_on_and_off,
    runnable_procs,
    runnable_ports,
    scheduler].

%% No specification clause needed for an init function in a conf case!!!

%% Test switching system_profiling on and off.
system_profile_on_and_off(suite) ->
    [];
system_profile_on_and_off(doc) ->
    ["Tests switching system_profiling on and off."];
system_profile_on_and_off(Config) when is_list(Config) ->
    ?line Pid = start_profiler_process(),
    
    % Test runnable_ports on and off
    ?line undefined = erlang:system_profile(Pid, [runnable_ports]),
    ?line {Pid, [runnable_ports]} = erlang:system_profile(),
    ?line {Pid, [runnable_ports]} = erlang:system_profile(undefined, []),

    % Test runnable_procs on and off
    ?line undefined = erlang:system_profile(Pid, [runnable_procs]),
    ?line {Pid, [runnable_procs]} = erlang:system_profile(),
    ?line {Pid, [runnable_procs]} = erlang:system_profile(undefined, []),

    % Test scheduler on and off
    ?line undefined = erlang:system_profile(Pid, [scheduler]),
    ?line {Pid, [scheduler]} = erlang:system_profile(),
    ?line {Pid, [scheduler]} = erlang:system_profile(undefined, []),

    % Test combined runnable_ports, runnable_procs, scheduler; on and off
    ?line undefined = erlang:system_profile(Pid, [scheduler, runnable_procs, runnable_ports]),
    ?line {Pid, [scheduler,runnable_procs,runnable_ports]} = erlang:system_profile(),
    ?line {Pid, [scheduler,runnable_procs,runnable_ports]} = erlang:system_profile(undefined, []),

    % Test turned off and kill process
    ?line undefined = erlang:system_profile(),
    ?line exit(Pid,kill),
    ok.

%% Test runnable_procs

runnable_procs(suite) ->
    [];
runnable_procs(doc) ->
    ["Tests system_profiling with runnable_procs."];
runnable_procs(Config) when is_list(Config) ->
    ?line Pid = start_profiler_process(),
    % start a ring of processes
    % FIXME: Set #laps and #nodes in config file
    Nodes = 10,
    Laps = 10,
    ?line Master = ring(Nodes),
    ?line undefined = erlang:system_profile(Pid, [runnable_procs]),
    % loop a message
    ?line ok = ring_message(Master, message, Laps),
    ?line Events = get_profiler_events(),
    ?line kill_em_all = kill_ring(Master),
    ?line erlang:system_profile(undefined, []),
    put(master, Master),
    put(laps, Laps),
    ?line true = has_runnable_event(Events),
    Pids = sort_events_by_pid(Events),
    ?line ok = check_events(Pids),
    erase(),
    ?line exit(Pid,kill),
    ok.

runnable_ports(suite) ->
    [];
runnable_ports(doc) ->
    ["Tests system_profiling with runnable_port."];
runnable_ports(Config) when is_list(Config) ->
    ?line Pid = start_profiler_process(),
    ?line undefined = erlang:system_profile(Pid, [runnable_ports]),
    ?line EchoPid = echo(Config),
    % FIXME: Set config to number_of_echos
    Laps = 10,
    put(laps, Laps),
    ?line ok = echo_message(EchoPid, Laps, message),
    ?line Events = get_profiler_events(),
    ?line kill_em_all = kill_echo(EchoPid),
    ?line erlang:system_profile(undefined, []),
    ?line true = has_runnable_event(Events),
    Pids = sort_events_by_pid(Events),
    ?line ok = check_events(Pids),
    erase(),
    ?line exit(Pid,kill),
    ok.

scheduler(suite) ->
    [];
scheduler(doc) ->
    ["Tests system_profiling with scheduler."];
scheduler(Config) when is_list(Config) ->
    case {erlang:system_info(smp_support), erlang:system_info(schedulers_online)} of
	{false,_} -> ?line {skipped, "No need for scheduler test when smp support is disabled."};
	{_,    1} -> ?line {skipped, "No need for scheduler test when only one scheduler online."};
	_ ->
	    Nodes = 10,
	    ?line ok = check_block_system(Nodes),
	    ?line ok = check_multi_scheduling_block(Nodes),
	    ok
    end.

%%% Check scheduler profiling

check_multi_scheduling_block(Nodes) ->
    ?line Pid = start_profiler_process(),
    ?line undefined = erlang:system_profile(Pid, [scheduler]),
    ?line {ok, Supervisor} = start_load(Nodes),
    ?line erlang:system_flag(multi_scheduling, block),
    ?line erlang:system_flag(multi_scheduling, unblock),
    ?line {Pid, [scheduler]} = erlang:system_profile(undefined, []),
    ?line Events = get_profiler_events(),
    ?line true = has_scheduler_event(Events),
    stop_load(Supervisor),
    ?line exit(Pid,kill),
    erase(),
    ok.

check_block_system(Nodes) ->
    ?line Dummy = spawn(?MODULE, profiler_process, [[]]),
    ?line Pid = start_profiler_process(),
    ?line undefined = erlang:system_profile(Pid, [scheduler]),
    ?line {ok, Supervisor} = start_load(Nodes),
    % FIXME: remove wait !!
    wait(300),
    ?line undefined = erlang:system_monitor(Dummy, [busy_port]),
    ?line {Dummy, [busy_port]} = erlang:system_monitor(undefined, []),
    ?line {Pid, [scheduler]} = erlang:system_profile(undefined, []),
    ?line Events = get_profiler_events(),
    ?line true = has_scheduler_event(Events),
    stop_load(Supervisor),
    ?line exit(Pid,kill),
    ?line exit(Dummy,kill),
    erase(),
    ok.

%%% Check events

check_events([]) -> ok;
check_events([Pid | Pids]) ->
    Master = get(master),
    Laps = get(laps),
    CheckPids = get(pids),
    {Events, N} = get_pid_events(Pid),
    ?line ok = check_event_flow(Events),
    ?line ok = check_event_ts(Events),
    IsMember = lists:member(Pid, CheckPids),
    case Pid of
    	Master ->
	    io:format("Expected ~p and got ~p profile events from ~p: ok~n", [Laps*2+2, N, Pid]),
	    ?line N = Laps*2 + 2,
    	    check_events(Pids);
	Pid when IsMember == true ->
	    io:format("Expected ~p and got ~p profile events from ~p: ok~n", [Laps*2, N, Pid]),
	    ?line N = Laps*2,
    	    check_events(Pids);
	Pid ->
	    check_events(Pids)
    end.

%% timestamp consistency check for descending timestamps

check_event_ts(Events) ->
    check_event_ts(Events, undefined).
check_event_ts([], _) -> ok;
check_event_ts([Event | Events], undefined) ->
    check_event_ts(Events, Event);
check_event_ts([{Pid, _, _, TS1}=Event | Events], {Pid,_,_,TS0}) ->
    Time = timer:now_diff(TS1, TS0),
    if 
    	Time < 0.0 -> timestamp_error;
    	true -> check_event_ts(Events, Event)
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
    Path = ?config(data_dir, Config),
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
    ok = case math:sin(random:uniform(32451)) of
    	A when is_float(A) -> ok;
	_ -> ok
    end,
    list_load().


has_scheduler_event(Events) ->
    lists:any(
    	fun (Pred) ->
	    case Pred of 
	    	{profile, scheduler, _ID, _Activity, _NR, _TS} -> true;
		_ -> false
	    end
	end, Events).

has_runnable_event(Events) ->
    lists:any(
    	fun (Pred) ->
	    case Pred of
	    	{profile, _Pid, _Activity, _MFA, _TS} -> true;
	    	_ -> false
	    end
        end, Events).

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


