%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2017. All Rights Reserved.
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


%%%-------------------------------------------------------------------
%%% File    : long_timer_test.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created : 21 Aug 2006 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------

-define(HIGH_CPU_INFO, "Ignored due to high CPU utilization.").
-define(MISSING_CPU_INFO, "Ignored due to missing CPU utilization information.").

-define(MAX_TIMEOUT, 60). % Minutes
-define(MAX_LATE_MS, 1000). % Milliseconds
-define(REG_NAME, '___LONG___TIMERS___TEST___SERVER___').

-define(HIGH_UTIL, 96.0).
-define(UTIL_INTERVAL, 10000).

-define(DRV_NAME, timer_driver).

% First byte in communication with the timer driver
-define(START_TIMER, 0).
-define(CANCEL_TIMER, 1).
-define(DELAY_START_TIMER, 2).
-define(TIMER, 3).
-define(CANCELLED, 4).

-module(long_timers_test).

-export([start/1, check_result/0]).

-record(timeout_rec,{pid, type, timeout, timeout_diff}).

start(DrvDir) when is_list(DrvDir) ->
    Starter = self(),
    StartDone = make_ref(),
    stop_node(full_node_name(?REG_NAME)),
    Node = start_node(?REG_NAME),
    Test = spawn(Node, fun () -> test(Starter, DrvDir, StartDone) end),
    Mon = erlang:monitor(process, Test),
    receive
	StartDone ->
	    erlang:demonitor(Mon),
	    net_kernel:disconnect(Node),
	    receive {'DOWN',Mon,_,_,_} -> ok after 0 -> ok end;
	{'DOWN',Mon,_,_,Reason} ->
	    stop_node(full_node_name(?REG_NAME)),
	    {error, Reason}
    end.

check_result() ->
    Node = full_node_name(?REG_NAME),
    LTTS = {?REG_NAME, Node},
    Mon = erlang:monitor(process, LTTS),
    (catch LTTS ! {get_result, ?REG_NAME, self()}),
    receive
	{'DOWN', Mon, process, _, Reason} ->
	    {?REG_NAME, 'DOWN', Reason};
	{result, ?REG_NAME, TORs, Start, End, UtilData} ->
	    erlang:demonitor(Mon),
	    receive {'DOWN', Mon, _, _, _} -> ok after 0 -> ok end,
	    stop_node(Node),
	    Res = check(TORs, Start, End, UtilData, ms((End - Start) - max_late()), ok),
	    io:format("Start = ~p~n End = ~p~n UtilData = ~p~n", [Start, End, UtilData]),
	    Res
    end.

res(New, Old) when New == failed; Old == failed ->
    failed;
res(New, Old) when New == missing_cpu_info; Old == missing_cpu_info ->
    missing_cpu_info;
res(New, Old) when New == high_cpu; Old == high_cpu ->
    high_cpu;
res(New, _Old) ->
    New.

check([#timeout_rec{timeout = Timeout,
		    type = Type,
		    timeout_diff = undefined} | TORs],
      Start,
      End,
      UtilData,
      NeedRes,
      Ok) when Timeout < NeedRes ->
    {NewOk, HCPU} = case had_high_cpu_util(Start,
					   Timeout,
					   End - Timeout*1000,
					   UtilData) of
			yes -> {res(high_cpu, Ok), ?HIGH_CPU_INFO};
			no -> {res(failed, Ok), ""};
			missing -> {res(missing_cpu_info, Ok), "FAILED", ?MISSING_CPU_INFO}
		    end,
    io:format("~p timeout = ~p ms FAILED! No timeout. ~s~n",
	      [Type, Timeout, HCPU]),
    check(TORs, Start, End, UtilData, NeedRes, NewOk);
check([#timeout_rec{timeout_diff = undefined} | TORs],
      Start,
      End,
      UtilData,
      NeedRes,
      Ok) ->
    check(TORs, Start, End, UtilData, NeedRes, Ok);
check([#timeout_rec{timeout = Timeout,
		    type = Type,
		    timeout_diff = {error, Reason}} | TORs],
      Start,
      End,
      UtilData,
      NeedRes,
      _Ok) ->
    io:format("~p timeout = ~p ms FAILED! exit reason ~p~n",
	      [Type, Timeout, Reason]),
    check(TORs, Start, End, UtilData, NeedRes, failed);
check([#timeout_rec{timeout = Timeout,
		    type = Type,
		    timeout_diff = TimeoutDiff} | TORs],
      Start,
      End,
      UtilData,
      NeedRes,
      Ok) ->
    {NewOk, SuccessStr, HCPU} = case {(0 =< TimeoutDiff),
				     (TimeoutDiff =< max_late())} of
				    {true, true} ->
					{res(ok, Ok), "succeeded", ""};
				    {false, _} ->
					{res(failed, Ok), "FAILED", ""};
				    _ ->
					case had_high_cpu_util(Start,
							       Timeout,
							       TimeoutDiff,
							       UtilData) of
					    yes -> {res(high_cpu, Ok), "FAILED", ?HIGH_CPU_INFO};
					    no -> {res(failed, Ok), "FAILED", ""};
					    missing -> {res(missing_cpu_info, Ok), "FAILED", ?MISSING_CPU_INFO}
					end
			  end,
    io:format("~s timeout = ~s ms ~s! timeout diff = ~s. ~s~n",
	      [type_str(Type),
	       time_str(Timeout),
	       SuccessStr,
	       time_str(TimeoutDiff, 1000000),
	       HCPU]),
    check(TORs, Start, End, UtilData, NeedRes, NewOk);
check([],_Start,_End,_UtilData,_NeedRes, Ok) ->
    Ok.

% TargetTimeout in ms, other in us.
had_high_cpu_util(StartTime,
		  TargetTimeout,
		  TimeoutDiff,
		  UtilData) ->
    TargetTo = StartTime + TargetTimeout*1000,
    ActTo = TargetTo + TimeoutDiff,
    hcpu(ActTo, TargetTo, UtilData).

hcpu(_ActTo, _TargetTo, [{_UT, 0} | _]) ->
    missing; %% Util is the integer zero when not supported...
%% UT2 =:= UT1
hcpu(ActTo, TargetTo, [{UT, _}, {UT, _} | _] = UD) ->
    hcpu(ActTo, TargetTo, tl(UD));
%% UT2 > UT1 > ActTo > TargetTo
hcpu(ActTo, TargetTo, [{_UT2, _}, {UT1, _} | _] = UD) when UT1 > ActTo ->
    hcpu(ActTo, TargetTo, tl(UD));
%% UT2 >= ActTo > TargetTo >= UT1
hcpu(ActTo, TargetTo,
     [{UT2, U}, {UT1, _} | _]) when UT2 >= ActTo,
				    TargetTo >= UT1 ->
    case U >= (((ActTo - TargetTo) / (UT2 - UT1))
	       * (?HIGH_UTIL/100.0)) of
	true -> yes;
	false -> no
    end;
%% UT2 >= ActTo >= UT1 > TargetTo
hcpu(ActTo, TargetTo,
     [{UT2, U}, {UT1, _} | _] = UD) when UT2 >= ActTo,
					 ActTo >= UT1,
					 UT1 > TargetTo ->
    case U >= (((ActTo - UT1) / (UT2 - UT1))
	       * (?HIGH_UTIL/100.0)) of
	true -> hcpu(ActTo, TargetTo, tl(UD));
	false -> no
    end;
%% ActTo > UT2 >= TargetTo >= UT1
hcpu(ActTo, TargetTo,
     [{UT2, U}, {UT1, _} | _]) when ActTo > UT2,
				    TargetTo >= UT1 ->
    case U >= (((UT2 - TargetTo) / (UT2 - UT1))
	       * (?HIGH_UTIL/100.0)) of
	true -> yes;
	false -> no
    end;
%% ActTo > UT2 > UT1 > TargetTo
hcpu(ActTo, TargetTo,
     [{UT2, U}, {UT1, _} | _] = UD) when ActTo > UT2,
					 UT1 > TargetTo ->
    case U >= ?HIGH_UTIL of
	true -> hcpu(ActTo, TargetTo, tl(UD));
	false -> no
    end.

type_str(receive_after) -> "receive ... after";
type_str(bif_timer) -> "BIF timer";
type_str(driver) -> "driver".

time_str(Time, Unit) ->
    lists:flatten([time_str(Time), " ", unit_str(Unit)]).

time_str(Time) ->
    lists:reverse(conv_time_str(lists:reverse(integer_to_list(Time)))).

conv_time_str([X,Y,Z,C|Cs]) when C /= $- ->
    [X,Y,Z,$`|conv_time_str([C|Cs])];
conv_time_str(Cs) ->
    Cs.

unit_str(1) -> "s";
unit_str(1000) -> "ms";
unit_str(1000000) -> "us";
unit_str(1000000000) -> "ns";
unit_str(Res) when is_integer(Res) -> ["/ ", integer_to_list(Res), " s"];
unit_str(Res) -> Res.

to_diff(Timeout, Start, Stop) ->
    %% 'Timeout' in milli seconds
    %% 'Start', 'Stop', and result in micro seconds
    (Stop - Start) - Timeout*1000.

ms(Time) ->
    erlang:convert_time_unit(Time, microsecond, millisecond).

max_late() ->
    erlang:convert_time_unit(?MAX_LATE_MS, millisecond, microsecond).

receive_after(Timeout) ->
    Start = erlang:monotonic_time(microsecond),
    receive
	{get_result, ?REG_NAME} ->
	    ?REG_NAME ! #timeout_rec{pid = self(),
				     type = receive_after,
				     timeout = Timeout}
    after Timeout ->
	    Stop = erlang:monotonic_time(microsecond),
	    receive
		{get_result, ?REG_NAME} ->
		    ?REG_NAME ! #timeout_rec{pid = self(),
					     type = receive_after,
					     timeout = Timeout,
					     timeout_diff = to_diff(Timeout,
								    Start,
								    Stop)}
	    end
    end.

driver(Timeout) ->
    Port = open_port({spawn, ?DRV_NAME},[]),
    link(Port),
    Start = erlang:monotonic_time(microsecond),
    erlang:port_command(Port, <<?START_TIMER, Timeout:32>>),
    receive
	{get_result, ?REG_NAME} ->
	    ?REG_NAME ! #timeout_rec{pid = self(),
				     type = driver,
				     timeout = Timeout};
	{Port,{data,[?TIMER]}} ->
	    Stop = erlang:monotonic_time(microsecond),
	    unlink(Port),
	    true = erlang:port_close(Port),
	    receive
		{get_result, ?REG_NAME} ->
		    ?REG_NAME ! #timeout_rec{pid = self(),
					     type = driver,
					     timeout = Timeout,
					     timeout_diff = to_diff(Timeout,
								    Start,
								    Stop)}
	    end
    end.

bif_timer(Timeout) ->
    Start = erlang:monotonic_time(microsecond),
    Tmr = erlang:start_timer(Timeout, self(), ok),
    receive
	{get_result, ?REG_NAME} ->
	    ?REG_NAME ! #timeout_rec{pid = self(),
				     type = bif_timer,
				     timeout = Timeout};
	{timeout, Tmr, ok} ->
	    Stop = erlang:monotonic_time(microsecond),
	    receive
		{get_result, ?REG_NAME} ->
		    ?REG_NAME ! #timeout_rec{pid = self(),
					     type = bif_timer,
					     timeout = Timeout,
					     timeout_diff = to_diff(Timeout,
								    Start,
								    Stop)}
	    end
    end.

test(Starter, DrvDir, StartDone) ->
    process_flag(priority, high),
    erl_ddll:start(),
    ok = load_driver(DrvDir, ?DRV_NAME),
    process_flag(trap_exit, true),
    register(?REG_NAME, self()),
    {group_leader, GL} = process_info(whereis(net_kernel),group_leader),
    group_leader(GL, self()),
    try
	application:start(sasl),
	application:start(os_mon)
    catch
	_ : _ ->
	    ok
    end,
    UtilData = new_util(),
    Start = erlang:monotonic_time(microsecond),
    TORs = lists:map(fun (Min) ->
			     TO = Min*60*1000,
			     [#timeout_rec{pid = spawn_opt(
						   fun () ->
							   receive_after(TO)
						   end,
						   [link, {priority, high}]),
					   type = receive_after,
					   timeout = TO},
			      #timeout_rec{pid = spawn_opt(
						   fun () ->
							   driver(TO)
						   end,
						   [link, {priority, high}]),
					   type = driver,
					   timeout = TO},
			      #timeout_rec{pid = spawn_opt(
						   fun () ->
							   bif_timer(TO)
						   end,
						   [link, {priority, high}]),
					   type = bif_timer,
					   timeout = TO}]
		     end,
		     lists:seq(1, ?MAX_TIMEOUT)),
    FlatTORs = lists:flatten(TORs),
    Starter ! StartDone,
    test_loop(FlatTORs, Start, UtilData).

new_util() ->
    new_util([]).

new_util(UtilData) ->
    Util = cpu_sup:util(),
    Time = erlang:monotonic_time(microsecond),
    [{Time, Util} | UtilData].

test_loop(TORs, Start, UtilData) ->
    receive
	{get_result, ?REG_NAME, Pid} ->
	    End = erlang:monotonic_time(microsecond),
	    EndUtilData = new_util(UtilData),
	    Pid ! {result, ?REG_NAME, get_test_results(TORs), Start, End, EndUtilData},
	    erl_ddll:unload_driver(?DRV_NAME),
	    erl_ddll:stop(),
	    exit(bye)
    after ?UTIL_INTERVAL ->
	    test_loop(TORs, Start, new_util(UtilData))
    end.

get_test_results(TORs) ->
    lists:foreach(fun (#timeout_rec{pid = Pid}) ->
			  Pid ! {get_result, ?REG_NAME}
		  end,
		  TORs),
    get_test_results(TORs, []).

get_test_results([#timeout_rec{pid = Pid,
			       timeout = Timeout} = TOR | TORs], NewTORs) ->
    receive
	#timeout_rec{pid = Pid, timeout = Timeout} = NewTOR ->
	    get_test_results(TORs, [NewTOR | NewTORs]);
	#timeout_rec{pid = Pid} = NewTOR ->
	    exit({timeout_mismatch, TOR, NewTOR});
	{'EXIT', Pid, Reason} ->
	    get_test_results(TORs,
			     [TOR#timeout_rec{timeout_diff = {error, Reason}}
			      | NewTORs])
	end;
get_test_results([], NewTORs) ->
    lists:reverse(NewTORs).

mk_node_cmdline(Name) ->
    Static = "-detached -noinput",
    Pa = filename:dirname(code:which(?MODULE)),
    Prog = case catch init:get_argument(progname) of
	       {ok,[[P]]} -> P;
	       _ -> exit(no_progname_argument_found)
	   end,
    NameSw = case net_kernel:longnames() of
		 false -> "-sname ";
		 true -> "-name ";
		 _ -> exit(not_distributed_node)
	     end,
    {ok, Pwd} = file:get_cwd(),
    NameStr = atom_to_list(Name),
    Prog ++ " "
	++ Static ++ " "
	++ NameSw ++ " " ++ NameStr ++ " "
	++ "-pa " ++ Pa ++ " "
	++ "-env ERL_CRASH_DUMP " ++ Pwd ++ "/erl_crash_dump." ++ NameStr ++ " "
	++ "-setcookie " ++ atom_to_list(erlang:get_cookie()).

full_node_name(PreName) ->
    HostSuffix = lists:dropwhile(fun ($@) -> false; (_) -> true end,
				 atom_to_list(node())),
    list_to_atom(atom_to_list(PreName) ++ HostSuffix).

ping_node(_Node, 0) ->
    pang;
ping_node(Node, N) when is_integer(N), N > 0 ->
    case catch net_adm:ping(Node) of
	pong -> pong;
	_ ->
	    receive after 100 -> ok end,
	    ping_node(Node, N-1)
    end.

start_node(Name) ->
    FullName = full_node_name(Name),
    CmdLine = mk_node_cmdline(Name),
    io:format("Starting node ~p: ~s~n", [FullName, CmdLine]),
    case open_port({spawn, CmdLine}, []) of
	Port when is_port(Port) ->
	    unlink(Port),
	    erlang:port_close(Port),
	    case ping_node(FullName, 50) of
		pong -> FullName;
		Other -> exit({failed_to_start_node, FullName, Other})
	    end;
	Error ->
	    exit({failed_to_start_node, FullName, Error})
    end.

stop_node(Node) ->
    monitor_node(Node, true),
    spawn(Node, fun () -> halt() end),
    receive {nodedown, Node} -> ok end.
	    
load_driver(Dir, Driver) ->
    case erl_ddll:load_driver(Dir, Driver) of
	ok -> ok;
	{error, Error} = Res ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    Res
    end.
