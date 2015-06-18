%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
-module(memsup_SUITE).
-include_lib("test_server/include/test_server.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([api/1, alarm1/1, alarm2/1, process/1]).
-export([config/1, timeout/1, unavailable/1, port/1]).
-export([otp_5910/1]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(1)).

init_per_suite(Config) when is_list(Config) ->
    ?line ok = application:start(os_mon),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ?line ok = application:stop(os_mon),
    Config.

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{watchdog,Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    Config.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    All = case test_server:os_type() of
	      {unix, sunos} ->
		  [api, alarm1, alarm2, process, config, timeout,
		   unavailable, port];
	      {unix, linux} ->
		  [api, alarm1, alarm2, process, timeout];
	      _OS -> [api, alarm1, alarm2, process]
	  end,
    Bugs = [otp_5910],
    All ++ Bugs.

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


api(suite) ->
    [];
api(doc) ->
    ["Test of API functions"];
api(Config) when is_list(Config) ->

    %% get_memory_data()
    ?line RegMemData = memsup:get_memory_data(),
    case RegMemData of
	{TotMem, AllBytes, {Pid, PidBytes}} when is_integer(TotMem),
						 is_integer(AllBytes),
						 is_pid(Pid),
						 is_integer(PidBytes) ->
	    ok;
	{0, 0, _WorstPid} ->
	    ?line ?t:fail(first_data_collection_failed);
	_ ->
	    ?line ?t:fail({bad_return, RegMemData})
    end,

    %% get_system_memory_data()
    ?line ExtMemData = memsup:get_system_memory_data(),
    Tags = [	total_memory,
		free_memory,
		system_total_memory,
		largest_free,
		number_of_free,
		free_swap,
		total_swap,
		cached_memory,
		buffered_memory,
		shared_memory],

    ?line true = lists:all(fun({Tag,Value}) when is_atom(Tag),
						 is_integer(Value) ->
				   lists:member(Tag, Tags);
			      (_) ->
				   false
			   end,
			   ExtMemData),

    %% get_os_wordsize()
    ?line ok = case memsup:get_os_wordsize() of
	32             -> ok;
	64             -> ok;
	unsupported_os -> ok;
	_ -> error
    end,

    %% get_check_interval()
    ?line 60000 = memsup:get_check_interval(),

    %% set_check_interval(Minutes)
    ?line ok = memsup:set_check_interval(2),
    ?line 120000 = memsup:get_check_interval(),
    ?line {'EXIT',{badarg,_}} =
	(catch memsup:set_check_interval(0.2)),
    ?line 120000 = memsup:get_check_interval(),
    ?line ok = memsup:set_check_interval(1),

    %% get_procmem_high_watermark()
    ?line 5 = memsup:get_procmem_high_watermark(),

    %% set_procmem_high_watermark()
    ?line ok = memsup:set_procmem_high_watermark(0.1),
    ?line 10 = memsup:get_procmem_high_watermark(),
    ?line {'EXIT',{badarg,_}} =
	(catch memsup:set_procmem_high_watermark(-0.1)),
    ?line 10 = memsup:get_procmem_high_watermark(),
    ?line ok = memsup:set_procmem_high_watermark(0.05),

    %% get_sysmem_high_watermark()
    ?line 80 = memsup:get_sysmem_high_watermark(),

    %% set_sysmem_high_watermark()
    ?line ok = memsup:set_sysmem_high_watermark(0.9),
    ?line 90 = memsup:get_sysmem_high_watermark(),
    ?line {'EXIT',{badarg,_}} =
	(catch memsup:set_sysmem_high_watermark(-0.9)),
    ?line 90 = memsup:get_sysmem_high_watermark(),
    ?line ok = memsup:set_sysmem_high_watermark(0.8),

    %% get|set_helper_timeout
    ?line 30 = memsup:get_helper_timeout(),
    ?line ok = memsup:set_helper_timeout(29),
    ?line 29 = memsup:get_helper_timeout(),
    ?line {'EXIT',{badarg,_}} = (catch memsup:set_helper_timeout(31.0)),
    ?line 29 = memsup:get_helper_timeout(),
    ok.

%%----------------------------------------------------------------------
%% NOTE: The test case is a bit weak as it will fail if the memory
%% usage changes too much during its course.
%%----------------------------------------------------------------------
alarm1(suite) ->
    [];
alarm1(doc) ->
    ["Test alarms when memsup_system_only==false"];
alarm1(Config) when is_list(Config) ->

    %% If system memory usage is too high, the testcase cannot
    %% be run correctly
    ?line {Total, Alloc, {_Pid,_PidAlloc}} = memsup:get_memory_data(),
    io:format("alarm1: Total: ~p, Alloc: ~p~n", [Total, Alloc]),
    ?line SysUsage = Alloc/Total,
    if
	SysUsage>0.99 ->
	    {skip, sys_mem_too_high};
	true ->
	    alarm1(Config, SysUsage)
    end.

alarm1(_Config, SysUsage) ->
    %% Set a long memory check interval, we will force memory checks
    %% instead
    ?line ok = memsup:set_check_interval(60),

    %% Check thresholds
    ?line SysThreshold = (memsup:get_sysmem_high_watermark()/100),
    ?line ProcThreshold = (memsup:get_procmem_high_watermark()/100),

    %% Check if a system alarm already should be set or not
    SysP = if
	       SysUsage>SysThreshold -> true;
	       SysUsage=<SysThreshold -> false
	   end,

    %% If system memory is higher than threshold, make sure the system
    %% alarm is set. Otherwise, make sure it is not set
    case alarm_set(system_memory_high_watermark) of
	{true, []} when SysP ->
	    ok;
	false when not SysP ->
	    ok;
	_ ->
	    ?line ?t:fail({sys_alarm, SysUsage, SysThreshold})
    end,

    %% Lower/raise the threshold to clear/set the alarm
    NewSysThreshold = if
        SysP ->
            Value = 1.1*SysUsage,
            if
                Value > 0.99 -> 0.99;
                true -> Value
            end;
        not SysP -> 0.9*SysUsage
    end,

    ?line ok = memsup:set_sysmem_high_watermark(NewSysThreshold),

    %% Initiate and wait for a new data collection
    ?line ok = force_collection(),

    %% Make sure the alarm is cleared/set
    ?t:sleep(?t:seconds(5)),
    case alarm_set(system_memory_high_watermark) of
	{true, []} when not SysP ->
	    ok;
	false when SysP ->
	    ok;
	_ ->
	    ?line ?t:fail({sys_alarm, SysUsage, NewSysThreshold})
    end,

    %% Reset the threshold to set/clear the alarm again
    ?line ok = memsup:set_sysmem_high_watermark(SysThreshold),
    ?line ok = force_collection(),
    ?t:sleep(?t:seconds(1)),
    case alarm_set(system_memory_high_watermark) of
	{true, []} when SysP ->
	    ok;
	false when not SysP ->
	    ok;
	_ ->
	    ?line ?t:fail({sys_alarm, SysUsage, SysThreshold})
    end,

    %% Check memory usage
    ?line {Total2, _, {WorstPid, PidAlloc}} = memsup:get_memory_data(),

    %% Check if a process alarm already should be set or not
    PidUsage = PidAlloc/Total2,
    ProcP = if
		PidUsage>ProcThreshold -> true;
		PidUsage=<ProcThreshold -> false
	    end,

    %% Make sure the process alarm is set/not set accordingly
    case alarm_set(process_memory_high_watermark) of
	{true, WorstPid} when ProcP ->
	    ok;
	false when not ProcP ->
	    ok;
	{true, BadPid1} when ProcP ->
	    ?line ?t:fail({proc_alarm, WorstPid, BadPid1});
	_ ->
	    ?line ?t:fail({proc_alarm, PidUsage, ProcThreshold})
    end,

    %% Lower/raise the threshold to clear/set the alarm
    NewProcThreshold = if
			   ProcP -> 1.1*PidUsage;
			   not ProcP -> 0.9*PidUsage
		       end,
    ?line ok = memsup:set_procmem_high_watermark(NewProcThreshold),
    ?line ok = force_collection(),
    ?t:sleep(?t:seconds(1)),
    case alarm_set(process_memory_high_watermark) of
	{true, WorstPid} when not ProcP ->
	    ok;
	false when ProcP ->
	    ok;
	{true, BadPid2} when not ProcP ->
	    ?line test_server:fail({proc_alarm, WorstPid, BadPid2});
	_ ->
	    ?line ?t:fail({proc_alarm, PidUsage, ProcThreshold})
    end,

    %% Reset the threshold to clear/set the alarm
    ?line ok = memsup:set_procmem_high_watermark(ProcThreshold),
    ?line ok = force_collection(),
    ?t:sleep(?t:seconds(1)),
    case alarm_set(process_memory_high_watermark) of
	{true, WorstPid} when ProcP ->
	    ok;
	false when not ProcP ->
	    ok;
	{true, BadPid3} when ProcP ->
	    ?line test_server:fail({proc_alarm, WorstPid, BadPid3});
	_ ->
	    ?line ?t:fail({proc_alarm, PidUsage, ProcThreshold})
    end,

    %% Reset memory check interval
    ?line ok = memsup:set_check_interval(1),
    ok.

alarm2(suite) ->
    [];
alarm2(doc) ->
    ["Test alarms when memsup_system_only==true"];
alarm2(Config) when is_list(Config) ->

    %% If system memory usage is too high, the testcase cannot
    %% be run correctly
    ?line {Total, Alloc, {_Pid,_PidAlloc}} = memsup:get_memory_data(),
    ?line SysUsage = Alloc/Total,
    if
	SysUsage>0.99 ->
	    {skip, sys_mem_too_high};
	true ->
	    alarm2(Config, SysUsage)
    end.

alarm2(_Config, _SysUsage) ->

    %% Change memsup_system_only and restart memsup
    ?line ok = application:set_env(os_mon, memsup_system_only, true),
    ?line ok = supervisor:terminate_child(os_mon_sup, memsup),
    ?line {ok, _Memsup1} = supervisor:restart_child(os_mon_sup, memsup),

    %% Set a long memory check interval, we will force memory checks
    %% instead
    ?line ok = memsup:set_check_interval(60),

    %% Check data and thresholds
    ?line {Total, Alloc, undefined} = memsup:get_memory_data(),
    ?line SysThreshold = (memsup:get_sysmem_high_watermark()/100),
    ?line true = is_integer(memsup:get_procmem_high_watermark()),

    %% Check if a system alarm already should be set or not
    ?line SysUsage = Alloc/Total,
    SysP = if
	       SysUsage>SysThreshold -> true;
	       SysUsage=<SysThreshold -> false
	   end,

    %% If system memory is higher than threshold, make sure the system
    %% alarm is set. Otherwise, make sure it is not set
    case alarm_set(system_memory_high_watermark) of
	{true, []} when SysP ->
	    ok;
	false when not SysP ->
	    ok;
	_ ->
	    ?line ?t:fail({sys_alarm, SysUsage, SysThreshold})
    end,

    %% Lower/raise the threshold to clear/set the alarm
    NewSysThreshold = if
        SysP ->
            Value = 1.1*SysUsage,
            if
                Value > 0.99 -> 0.99;
                true -> Value
            end;
        not SysP -> 0.9*SysUsage
    end,

    ?line ok = memsup:set_sysmem_high_watermark(NewSysThreshold),

    %% Initiate and wait for a new data collection
    ?line ok = force_collection(),

    %% Make sure the alarm is cleared/set
    ?t:sleep(?t:seconds(1)),
    case alarm_set(system_memory_high_watermark) of
	{true, []} when not SysP ->
	    ok;
	false when SysP ->
	    ok;
	_ ->
	    ?line ?t:fail({sys_alarm, SysUsage, NewSysThreshold})
    end,

    %% Reset the threshold to set/clear the alarm again
    ?line ok = memsup:set_sysmem_high_watermark(SysThreshold),
    ?line ok = force_collection(),
    ?t:sleep(?t:seconds(1)),
    case alarm_set(system_memory_high_watermark) of
	{true, []} when SysP ->
	    ok;
	false when not SysP ->
	    ok;
	_ ->
	    ?line ?t:fail({sys_alarm, SysUsage, SysThreshold})
    end,

    %% Reset memsup_system_only and restart memsup
    %% (memory check interval is then automatically reset)
    ?line ok = application:set_env(os_mon, memsup_system_only, false),
    ?line ok = supervisor:terminate_child(os_mon_sup, memsup),
    ?line {ok, _Memsup2} = supervisor:restart_child(os_mon_sup, memsup),

    ok.

alarm_set(Alarm) ->
    alarm_set(Alarm, alarm_handler:get_alarms()).
alarm_set(Alarm, [{Alarm,Data}|_]) ->
    {true,Data};
alarm_set(Alarm, [_|T]) ->
    alarm_set(Alarm, T);
alarm_set(_Alarm, []) ->
    false.

process(suite) ->
    [];
process(doc) ->
    ["Make sure memsup discovers a process grown very large"];
process(Config) when is_list(Config) ->

    %% Set a long memory check interval, we will force memory checks
    %% instead
    ?line ok = memsup:set_check_interval(60),

    %% Collect data
    MemData = memsup:get_memory_data(),
    io:format("process: memsup:get_memory_data() = ~p~n", [MemData]),
    ?line {_Total,_Free,{_,Bytes}} = MemData,

    %% Start a new process larger than Worst
    ?line WorsePid = spawn(fun() -> new_hog(Bytes) end),
    ?t:sleep(?t:seconds(1)),

    %% Initiate and wait for a new data collection
    ?line ok = force_collection(),

    %% Check that get_memory_data() returns updated result
    ?line case memsup:get_memory_data() of
	      {_, _, {WorsePid, _MoreBytes}} ->
		  ok;
	      {_, _, BadWorst} ->
		  ?line ?t:fail({worst_pid, BadWorst})
	  end,

    %% Reset memory check interval
    ?line exit(WorsePid, done),
    ?line ok = memsup:set_check_interval(1),
    ok.

new_hog(Bytes) ->
    WordSize = erlang:system_info(wordsize),
    N = (Bytes+200) div WordSize div 2,
    List = lists:duplicate(N, a),
    new_hog_1(List).

new_hog_1(List) ->
    receive
	_Any -> exit(List)
    end.

config(suite) ->
    [];
config(doc) ->
    ["Test configuration"];
config(Config) when is_list(Config) ->

    %% Change configuration parameters and make sure change is reflected
    %% when memsup is restarted
    ?line ok = application:set_env(os_mon, memory_check_interval, 2),
    ?line ok =
	application:set_env(os_mon, system_memory_high_watermark, 0.9),
    ?line ok =
	application:set_env(os_mon, process_memory_high_watermark, 0.1),
    ?line ok = application:set_env(os_mon, memsup_helper_timeout, 35),
    ?line ok = application:set_env(os_mon, memsup_system_only, true),

    ?line ok = supervisor:terminate_child(os_mon_sup, memsup),
    ?line {ok, _Child1} = supervisor:restart_child(os_mon_sup, memsup),

    ?line 120000 = memsup:get_check_interval(),
    ?line 90 = memsup:get_sysmem_high_watermark(),
    ?line 10 = memsup:get_procmem_high_watermark(),
    ?line 35 = memsup:get_helper_timeout(),

    %% Also try this with bad parameter values, should be ignored
    ?line ok = application:set_env(os_mon, memory_check_interval, 0.2),
    ?line ok =
	application:set_env(os_mon, system_memory_high_watermark, -0.9),
    ?line ok =
	application:set_env(os_mon, process_memory_high_watermark,-0.1),
    ?line ok = application:set_env(os_mon, memsup_helper_timeout, 0.35),
    ?line ok = application:set_env(os_mon, memsup_system_only, arne),

    ?line ok = supervisor:terminate_child(os_mon_sup, memsup),
    ?line {ok, _Child2} = supervisor:restart_child(os_mon_sup, memsup),

    ?line 60000 = memsup:get_check_interval(),
    ?line 80 = memsup:get_sysmem_high_watermark(),
    ?line 5 = memsup:get_procmem_high_watermark(),
    ?line 30 = memsup:get_helper_timeout(),

    %% Reset configuration parameters
    ?line ok = application:set_env(os_mon, memory_check_interval, 1),
    ?line ok =
	application:set_env(os_mon, system_memory_high_watermark, 0.8),
    ?line ok =
	application:set_env(os_mon, process_memory_high_watermark,0.05),
    ?line ok = application:set_env(os_mon, memsup_helper_timeout, 30),
    ?line ok = application:set_env(os_mon, memsup_system_only, false),

    ok.

unavailable(suite) ->
    [];
unavailable(doc) ->
    ["Test correct behaviour when service is unavailable"];
unavailable(Config) when is_list(Config) ->

    %% Close memsup
    ?line ok = application:set_env(os_mon, start_memsup, false),
    ?line ok = supervisor:terminate_child(os_mon_sup, memsup),

    %% Make sure all API functions return their dummy values
    ?line {0,0,{_Pid,0}} = memsup:get_memory_data(),
    ?line ok = application:set_env(os_mon, memsup_system_only, true),
    ?line {0,0,undefined} = memsup:get_memory_data(),
    ?line ok = application:set_env(os_mon, memsup_system_only, false),
    ?line [] = memsup:get_system_memory_data(),
    ?line 0  = memsup:get_os_wordsize(),
    ?line 60000 = memsup:get_check_interval(),
    ?line ok = memsup:set_check_interval(2),
    ?line 5 = memsup:get_procmem_high_watermark(),
    ?line ok = memsup:set_procmem_high_watermark(0.10),
    ?line 80 = memsup:get_sysmem_high_watermark(),
    ?line ok = memsup:set_sysmem_high_watermark(0.90),
    ?line 30 = memsup:get_helper_timeout(),
    ?line ok = memsup:set_helper_timeout(35),

    %% Start memsup again,
    ?line ok = application:set_env(os_mon, start_memsup, true),
    ?line {ok, _Child} = supervisor:restart_child(os_mon_sup, memsup),

    ok.

timeout(suite) ->
    [];
timeout(doc) ->
    ["Test stability of memsup when data collection times out"];
timeout(Config) when is_list(Config) ->

    %% Set a long memory check interval and memsup_helper timeout,
    %% we will force memory checks instead and fake timeouts
    ?line ok = memsup:set_check_interval(60),
    ?line ok = memsup:set_helper_timeout(3600),

    %% Provoke a timeout during memory collection
    ?line memsup ! time_to_collect,
    ?line memsup ! reg_collection_timeout,

    %% Not much we can check though, except that memsup is still running
    ?line {_,_,_} = memsup:get_memory_data(),

    %% Provoke a timeout during extensive memory collection
    %% We fake a gen_server:call/2 to be able to send a timeout message
    %% while the request is being handled

    %% Linux should be handled the same way as solaris.

%    TimeoutMsg = case ?t:os_type() of
%		     {unix, sunos} -> ext_collection_timeout;
%		     {unix, linux} -> reg_collection_timeout
%		 end,

    TimeoutMsg = ext_collection_timeout,

    ?line Pid = whereis(memsup),
    ?line Mref = erlang:monitor(process, Pid),
    ?line Pid ! {'$gen_call', {self(), Mref}, get_system_memory_data},
    ?line Pid ! TimeoutMsg,
    receive
	{Mref, []} ->
	    erlang:demonitor(Mref),
	    ?line ok;
	{Mref, Res} ->
	    erlang:demonitor(Mref),
	    ?line ?t:fail({unexpected_result, Res});
	{'DOWN', Mref, _, _, _} ->
	    ?line ?t:fail(no_result)
    end,

    %% Reset memory check interval and memsup_helper timeout
    ?line ok = memsup:set_check_interval(1),
    ?line ok = memsup:set_helper_timeout(30),
    ?line memsup ! time_to_collect,

    ?line [_|_] = memsup:get_system_memory_data(),

    ok.

port(suite) ->
    [];
port(doc) ->
    ["Test that memsup handles a terminating port program"];
port(Config) when is_list(Config) ->
    ?line Str = os:cmd("ps -e | grep '[m]emsup'"),
    case io_lib:fread("~s", Str) of
	{ok, [Pid], _Rest} ->

	    %% Monitor memsup
	    ?line MonRef = erlang:monitor(process, memsup),
	    ?line {Total1,_Alloc1,_Worst1} = memsup:get_memory_data(),
	    ?line true = Total1>0,

	    %% Kill the port program
	    case os:cmd("kill -9 " ++ Pid) of
		[] ->

		    %% memsup should now terminate
		    receive
			{'DOWN', MonRef, _, _, {port_died, _Reason}} ->
			    ok;
			{'DOWN', MonRef, _, _, Reason} ->
			    ?line ?t:fail({unexpected_exit_reason, Reason})
		    after
			3000 ->
			    ?line ?t:fail(still_alive)
		    end,

		    %% Give os_mon_sup time to restart memsup
		    ?t:sleep(?t:seconds(3)),
		    ?line {Total2,_Alloc2,_Worst2} =
			memsup:get_memory_data(),
		    ?line true = Total2>0,

		    ok;

		Line ->
		    erlang:demonitor(MonRef),
		    {skip, {not_killed, Line}}
	    end;
	_ ->
	    {skip, {os_pid_not_found, Str}}
    end.

otp_5910(suite) ->
    [];
otp_5910(doc) ->
    ["Test that alarms are cleared and not set twice"];
otp_5910(Config) when is_list(Config) ->
    Alarms =
	[system_memory_high_watermark, process_memory_high_watermark],

    %% Make sure memsup sets both alarms
    ?line ok = application:set_env(os_mon, memory_check_interval, 60),
    ?line ok = memsup:set_check_interval(60),
    ?line SysThreshold = (memsup:get_sysmem_high_watermark()/100),
    ?line ProcThreshold = (memsup:get_procmem_high_watermark()/100),

    MemData = memsup:get_memory_data(),

    io:format("otp_5910: memsup:get_memory_data() = ~p~n", [MemData]),
    ?line {Total, Alloc, {_Pid, _Bytes}} = MemData,
    ?line Pid = spawn_opt(fun() ->
				  receive
				      die -> ok
				  end
			  end, [{min_heap_size, 1000}]),
    %% Create a process guaranteed to live, be constant and
    %% break memsup process limit
    ?line {memory, Bytes} = erlang:process_info(Pid,memory),
    ?line SysUsage = Alloc/Total,
    ?line ProcUsage = Bytes/Total,

    if
	SysUsage>SysThreshold ->
	    ok;
	SysUsage=<SysThreshold ->
	    ?line ok = application:set_env(os_mon,
					   sys_mem_high_watermark,
					   0.5 * SysUsage),
	    ?line ok = memsup:set_sysmem_high_watermark(0.5 * SysUsage)
    end,
    if
	ProcUsage>ProcThreshold ->
	    ok;
	ProcUsage=<ProcThreshold ->
	    ?line ok = application:set_env(os_mon,
					   proc_mem_high_watermark,
					   0.5 * ProcUsage),
	    ?line ok = memsup:set_procmem_high_watermark(0.5 *ProcUsage)
    end,
    ?line ok = force_collection(),
    ?t:sleep(?t:seconds(1)),
    lists:foreach(fun(AlarmId) ->
			  case alarm_set(AlarmId) of
			      {true, _} -> ok;
			      false ->
				  ?line ?t:fail({alarm_not_set,
						 AlarmId})
			  end
		  end,
		  Alarms),

    %% Kill guaranteed process...
    Pid ! die,
    %% Kill memsup
    exit(whereis(memsup), faked_memsup_crash),
    %% Wait a little to make sure memsup has been restarted,
    %% then make sure the alarms are set once, but not twice
    ?t:sleep(?t:seconds(1)),
    ?line MemUsage = memsup:get_memory_data(),
    SetAlarms = alarm_handler:get_alarms(),
    case lists:foldl(fun(system_memory_high_watermark, {S, P}) ->
			     {S+1, P};
			(process_memory_high_watermark, {S, P}) ->
			     {S, P+1};
			(_AlarmId, Acc0) ->
			     Acc0
		     end,
		     {0, 0},
		     SetAlarms) of
	{0, 0} ->
	    ok;
	_ ->
	    ?line ?t:fail({bad_number_of_alarms, SetAlarms, MemUsage})
    end,

    %% Stop OS_Mon and make sure all memsup alarms are cleared
    ?line ok = application:stop(os_mon),
    ?t:sleep(?t:seconds(1)),
    lists:foreach(fun(AlarmId) ->
			  case alarm_set(AlarmId) of
			      false -> ok;
			      {true, _} ->
				  ?line ?t:fail({alarm_is_set, AlarmId})
			  end
		  end,
		  Alarms),

    %% Reset configuration and restart OS_Mon
    ?line ok = application:set_env(os_mon,memory_check_interval,1),
    ?line ok = application:set_env(os_mon,sys_mem_high_watermark,0.8),
    ?line ok = application:set_env(os_mon,proc_mem_high_watermark,0.05),
    ?line ok = application:start(os_mon),

    ok.

%%----------------------------------------------------------------------
%% Auxiliary
%%----------------------------------------------------------------------

force_collection() ->
    erlang:trace(whereis(memsup), true, ['receive']),
    memsup ! time_to_collect,
    TimerRef = erlang:send_after(5000, self(), timeout),
    force_collection(TimerRef).

force_collection(TimerRef) ->
    receive
	{trace, _Pid, 'receive', {collected_sys, _Sys}} ->
	    erlang:cancel_timer(TimerRef),
	    erlang:trace(whereis(memsup), false, ['receive']),
	    flush(),
	    ok;
	{trace, _Pid, 'receive', reg_collection_timeout} ->
	    erlang:cancel_timer(TimerRef),
	    erlang:trace(whereis(memsup), false, ['receive']),
	    flush(),
	    collection_timeout;
	timout ->
	    erlang:trace(whereis(memsup), false, ['receive']),
	    flush(),
	    timeout;
	_Msg ->
	    force_collection(TimerRef)
    end.

flush() ->
    receive
	{trace, _, _, _} ->
	    flush();
	timeout ->
	    flush()
    after 0 ->
	    ok
    end.
