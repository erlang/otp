%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2021-2022. All Rights Reserved.
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

-module(kernel_test_global_sys_monitor).

-export([start/0, stop/0,
         reset_events/0,
         events/0, events/1,
         log/1]).
-export([timestamp/0, format_timestamp/1]).
-export([init/1]).

-include("kernel_test_lib.hrl").

-define(NAME,    ?MODULE).
-define(TIMEOUT, timer:seconds(6)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    Parent = self(),
    %% The only "real" reason this would fail to start is if
    %% global has issues.
    %% So, to catch any problems, we add a timeout of 15 seconds.
    %% This should be long enough. If it has not started then,
    %% we just give up...
    proc_lib:start(?MODULE, init, [Parent], ?SECS(15)).

stop() ->
    cast(stop).

%% This does not reset the global counter but the "collector"
%% See events for more info.
reset_events() ->
    call(reset_events, ?TIMEOUT).

events() ->
    events(infinity).

events(Timeout) when (Timeout =:= infinity) orelse
                     (is_integer(Timeout) andalso (Timeout > 0)) ->
    call(events, Timeout).

log(Event) ->
    cast({node(), Event}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timestamp() ->
    erlang:timestamp().

format_timestamp({_N1, _N2, N3} = TS) ->
    {Date, Time}   = calendar:now_to_local_time(TS),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~.3.0w",
                      [YYYY, MM, DD, Hour, Min, Sec, N3 div 1000]),  
    lists:flatten(FormatDate).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Parent) ->
    process_flag(priority, high),
    case global:register_name(?NAME, self()) of
        yes ->
            info_msg("Starting as ~p (on ~p)", [self(), node()]),
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(#{parent => Parent, ev_cnt => 0, evs => []});
        no ->
            warning_msg("Already started", []),
            proc_lib:init_ack(Parent, {error, already_started}),
            exit(normal)
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(State) ->
    receive
        {?MODULE, stop} ->
            warning_msg("Stopping with ~w events counted",
                        [maps:get(ev_cnt, State)]),
            exit(normal);

        {?MODULE, Ref, From, reset_events} ->
            TotEvCnt = maps:get(ev_cnt, State),
            EvCnt    = length(maps:get(evs, State)),
            info_msg("Reset events when"
                     "~n   Total Number of Events:   ~p"
                     "~n   Current Number of Events: ~p",
                     [TotEvCnt, EvCnt]),
            From ! {?MODULE, Ref, {ok, {TotEvCnt, EvCnt}}},
            loop(State#{evs => []});

        {?MODULE, Ref, From, events} ->
            Evs = maps:get(evs, State),
            From ! {?MODULE, Ref, lists:reverse(Evs)},
            loop(State);

        {?MODULE, {Node, Event}} ->
            State2 = process_event(State, Node, Event),
            loop(State2);

        {nodedown = Event, Node} ->
            State2 = process_event(State, Node, Event),
            loop(State2);            

        _ ->
            loop(State)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_event(State, Node, {Pid, TS, Tag, Info}) ->
    process_system_event(State, Node, Pid, TS, Tag, Info);

process_event(State, Node, {TS, starting}) ->
    FTS = format_timestamp(TS),
    info_msg("System Monitor starting on node ~p at ~s", [Node, FTS]),
    if
        (Node =/= node()) ->
            erlang:monitor_node(Node, true);
        true ->
            ok
    end,
    State;

process_event(State, Node, {TS, stopping}) ->
    FTS = format_timestamp(TS),
    info_msg("System Monitor stopping on node ~p at ~s", [Node, FTS]),
    if
        (Node =/= node()) ->
            erlang:monitor_node(Node, false);
        true ->
            ok
    end,
    State;

process_event(State, Node, {TS, already_started}) ->
    FTS = format_timestamp(TS),
    info_msg("System Monitor already started on node ~p at ~s", [Node, FTS]),
    State;

process_event(State, Node, nodedown) ->
    info_msg("Node ~p down", [Node]),
    State;

process_event(State, Node, Event) ->
    warning_msg("Received unknown event from node ~p:"
                "~n   ~p", [Node, Event]),
    State.


%% System Monitor events
%% We only *count* system events
process_system_event(#{ev_cnt := Cnt, evs := Evs} = State,
                     Node, Pid, TS, long_gc = Ev, Info) ->
    print_system_event(f("Long GC (~w)", [length(Evs)]), Node, Pid, TS, Info),
    State#{ev_cnt => Cnt + 1, evs => [{Node, Ev} | Evs]};
process_system_event(#{ev_cnt := Cnt, evs := Evs} = State,
                     Node, Pid, TS, long_schedule = Ev, Info) ->
    print_system_event(f("Long Schedule (~w)", [length(Evs)]), Node, Pid, TS, Info),
    State#{ev_cnt => Cnt + 1, evs => [{Node, Ev} | Evs]};
process_system_event(#{ev_cnt := Cnt, evs := Evs} = State,
                     Node, Pid, TS, large_heap = Ev, Info) ->
    print_system_event(f("Large Heap (~w)", [length(Evs)]), Node, Pid, TS, Info),
    State#{ev_cnt => Cnt + 1, evs => [{Node, Ev} | Evs]};
process_system_event(#{ev_cnt := Cnt, evs := Evs} = State,
                     Node, Pid, TS, busy_port = Ev, Info) ->
    print_system_event(f("Busy port (~w)", [length(Evs)]), Node, Pid, TS, Info),
    State#{ev_cnt => Cnt + 1, evs => [{Node, Ev} | Evs]};
process_system_event(#{ev_cnt := Cnt, evs := Evs} = State,
                     Node, Pid, TS, busy_dist_port = Ev, Info) ->
    print_system_event(f("Busy dist port (~w)", [length(Evs)]),
                       Node, Pid, TS, Info),
    State#{ev_cnt => Cnt + 1, evs => [{Node, Ev} | Evs]};

%% And everything else
process_system_event(State, Node, Pid, TS, Tag, Info) ->
    Pre = f("Unknown Event '~p'", [Tag]),
    print_system_event(Pre, Node, Pid, TS, Info),
    State.


print_system_event(Pre, Node, Pid, TS, Info) ->
    FTS = format_timestamp(TS),
    warning_msg("~s from ~p (~p) at ~s:"
                "~n   ~p", [Pre, Node, Pid, FTS, Info]).

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cast(Msg) ->
    try global:send(?NAME, {?MODULE, Msg}) of
        Pid when is_pid(Pid) ->
            ok
    catch
        C:E:_ ->
            {error, {caught, C, E}}
    end.


call(Req, Timeout) when (Timeout =:= infinity) ->
    call(Req, Timeout, Timeout);
call(Req, Timeout) when is_integer(Timeout) andalso (Timeout > 2000) ->
    call(Req, Timeout, Timeout - 1000);
call(Req, Timeout) when is_integer(Timeout) andalso (Timeout > 1000) ->
    call(Req, Timeout, Timeout - 500);
call(Req, Timeout) when is_integer(Timeout) ->
    call(Req, Timeout, Timeout div 2).

%% This peace of weirdness is because on some machines this call has
%% hung (in a call during end_per_testcase, which had a 1 min timeout,
%% or if that was the total time for the test case).
%% But because it hung there, we don't really know where it got stuck.
%% So, by making the call in a tmp process, that we supervise, we can
%% keep control. Also, we change the default timeout from infinity to an
%% actual time (16 seconds).
call(Req, Timeout1, Timeout2) ->
    F = fun() ->
                Ref = make_ref(),
                try global:send(?NAME, {?MODULE, Ref, self(), Req}) of
                    NamePid when is_pid(NamePid) ->
                        receive
                            {?MODULE, Ref, Rep} ->
                                exit(Rep)
                        after Timeout2 ->
                                exit({error, timeout})
                        end
                catch
                    C:E:S ->
                        exit({error, {caught, C, E, S}})
                end
        end,
    {Pid, Mon} = spawn_monitor(F),
    receive
        {'DOWN', Mon, process, Pid, Result} ->
            Result
    after Timeout1 ->
            PInfo = process_info(Pid),
            exit(Pid, kill),
            {error, {timeout, PInfo}}
    end.
            


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info_msg(F, A) ->
    error_logger:info_msg(format_msg(F, A), []).

warning_msg(F, A) ->
    error_logger:warning_msg(format_msg(F, A), []).

                             
format_msg(F, A) ->
    f("~n" ++ 
          "****** KERNEL TEST GLOBAL SYSTEM MONITOR ******~n~n" ++ 
          F ++ 
          "~n~n",
      A).
   
