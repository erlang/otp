%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
-module(disksup_SUITE).
-include("test_server.hrl").

%% Test server specific exports
-export([all/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([api/1, config/1, alarm/1]).
-export([port/1]).
-export([terminate/1, unavailable/1, restart/1]).
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
    ok.

all(suite) ->
    Bugs = [otp_5910],
    case ?t:os_type() of
	{unix, sunos} ->
	    [api, config, alarm, port,
	     {conf, terminate, [unavailable], restart}] ++ Bugs;
	{unix, _OSname} ->
	    [api, alarm] ++ Bugs;
	{win32, _OSname} ->
	    [api, alarm] ++ Bugs;
	_OS ->
	    [unavailable]
    end.

api(suite) ->
    [];
api(doc) ->
    ["Test of API functions"];
api(Config) when is_list(Config) ->

    %% get_disk_data()
    ?line [{Id, KByte, Capacity}|_] = disksup:get_disk_data(),
    ?line true = io_lib:printable_list(Id),
    ?line true = is_integer(KByte),
    ?line true = is_integer(Capacity),
    ?line true = KByte>0,
    ?line true = Capacity>0,

    %% get_check_interval()
    ?line 1800000 = disksup:get_check_interval(),

    %% set_check_interval(Minutes)
    ?line ok = disksup:set_check_interval(20),
    ?line 1200000 = disksup:get_check_interval(),
    ?line {'EXIT',{badarg,_}} = (catch disksup:set_check_interval(0.5)),
    ?line 1200000 = disksup:get_check_interval(),
    ?line ok = disksup:set_check_interval(30),

    %% get_almost_full_threshold()
    ?line 80 = disksup:get_almost_full_threshold(),

    %% set_almost_full_threshold(Float)
    ?line ok = disksup:set_almost_full_threshold(0.90),
    ?line 90 = disksup:get_almost_full_threshold(),
    ?line {'EXIT',{badarg,_}} =
	(catch disksup:set_almost_full_threshold(-0.5)),
    ?line 90 = disksup:get_almost_full_threshold(),
    ?line ok = disksup:set_almost_full_threshold(0.80),

    ok.

config(suite) ->
    [];
config(doc) ->
    ["Test configuration"];
config(Config) when is_list(Config) ->

    %% Change configuration parameters and make sure change is reflected
    %% when disksup is restarted
    ?line ok =
	application:set_env(os_mon, disk_space_check_interval, 29),
    ?line ok =
	application:set_env(os_mon, disk_almost_full_threshold, 0.81),

    ?line ok = supervisor:terminate_child(os_mon_sup, disksup),
    ?line {ok, _Child1} = supervisor:restart_child(os_mon_sup, disksup),

    ?line 1740000 = disksup:get_check_interval(),
    ?line 81 = disksup:get_almost_full_threshold(),

    %% Also try this with bad parameter values, should be ignored
    ?line ok =
	application:set_env(os_mon, disk_space_check_interval, 0.5),
    ?line ok =
	application:set_env(os_mon, disk_almost_full_threshold, -0.81),

    ?line ok = supervisor:terminate_child(os_mon_sup, disksup),
    ?line {ok, _Child2} = supervisor:restart_child(os_mon_sup, disksup),

    ?line 1800000 = disksup:get_check_interval(),
    ?line 80 = disksup:get_almost_full_threshold(),

    %% Reset configuration parameters
    ?line ok =
	application:set_env(os_mon, disk_space_check_interval, 30),
    ?line ok =
	application:set_env(os_mon, disk_almost_full_threshold, 0.80),

    ok.

%%----------------------------------------------------------------------
%% NOTE: The test case is a bit weak as it will fail if the disk usage
%% changes too much during its course, or if there are timing problems
%% with the alarm_handler receiving the alarms too late
%%----------------------------------------------------------------------
alarm(suite) ->
    [];
alarm(doc) ->
    ["Test that alarms are set and cleared"];
alarm(Config) when is_list(Config) ->

    %% Find out how many disks exceed the threshold
    %% and make sure the corresponding number of alarms is set
    ?line Threshold1 = disksup:get_almost_full_threshold(), % 80
    ?line Data1 = disksup:get_disk_data(),
    ?line Over1 = over_threshold(Data1, Threshold1),
    ?line Alarms1 = get_alarms(),
    if
	Over1==length(Alarms1) ->
	    ?line true;
	true ->
	    dump_info(),
	    ?line ?t:fail({bad_alarms, Threshold1, Data1, Alarms1})
    end,

    %% Try to find a disk with space usage below Threshold1,
    %% lower the threshold accordingly and make sure new alarms are set
    Fun1 = fun({_Id, _Kbyte, Capacity}) ->
		   if
		       Capacity>0, Capacity<Threshold1 -> true;
		       true -> false
		   end
	   end,
    ?line case until(Fun1, Data1) of
	      {_, _, Cap1} ->
		  Threshold2 = Cap1-1,
		  ?line ok =
		      disksup:set_almost_full_threshold(Threshold2/100),
		  ?line disksup ! timeout, % force a disk check
		  ?line Data2 = disksup:get_disk_data(),
		  ?line Over2 = over_threshold(Data2, Threshold2),
		  ?line Alarms2 = get_alarms(),
		  if
		      Over2==length(Alarms2), Over2>Over1 ->
			  ?line true;
		      true ->
			  dump_info(),
			  ?line ?t:fail({bad_alarms, Threshold2, Data2, Alarms2})
		  end;
	      false ->
		  ?line ignore
	  end,

    %% Find out the highest space usage among all disks
    %% and try to raise the threshold above this value,
    %% make sure all alarms are cleared
    Fun2 = fun({_Id, _Kbyte, Capacity}, MaxAcc) ->
		   if
		       Capacity>MaxAcc -> Capacity;
		       true -> MaxAcc
		   end
	   end,
    ?line case lists:foldl(Fun2, 0, Data1) of
	      Max when Max<100 ->
		  Threshold3 = Max+1,
		  ?line ok =
		      disksup:set_almost_full_threshold(Threshold3/100),
		  ?line disksup ! timeout, % force a disk check
		  ?line Data3 = disksup:get_disk_data(),
		  ?line Over3 = over_threshold(Data3, Threshold3),
		  ?line Alarms3 = get_alarms(),
		  if
		      Over3==0, length(Alarms3)==0 ->
			  ?line ok;
		      true ->
			  dump_info(),
			  ?line ?t:fail({bad_alarms, Threshold3, Data3, Alarms3})
		  end;
	      100 ->
		  ?line ignore
	  end,

    %% Reset threshold
    ?line ok = disksup:set_almost_full_threshold(Threshold1/100),

    ok.

over_threshold(Data, Threshold) ->
    Data2 = remove_duplicated_disks(lists:keysort(1, Data)),
    lists:foldl(fun({_Id, _Kbyte, Cap}, N) when Cap>=Threshold ->
			N+1;
		   (_DiskData, N) ->
			N
		end,
		0,
		Data2).

%% On some platforms (for example MontaVista) data for one disk can be
%% "duplicated":
%%  Linux ppb 2.4.20_mvl31-pcore680 #1 Sun Feb 1 23:12:56 PST 2004 ppc unknown
%%
%%  MontaVista(R) Linux(R) Professional Edition 3.1
%%
%%  [ppb:~]> /bin/df -lk
%%  Filesystem           1k-blocks      Used Available Use% Mounted on
%%  rootfs                 8066141   3023763   4961717  38% /
%%  /dev/root              8066141   3023763   4961717  38% /
%%  tmpfs                   192892         0    192892   0% /dev/shm
%%
%% disksup:
%%  [{"/",8066141,38}, {"/",8066141,38}, {"/dev/shm",192892,0}]
%%
%% disksup will only set ONE alarm for "/".
%% Therefore the list of disk data must be sorted and duplicated disk
%% tuples removed before calculating how many alarms should be set, or
%% the testcase will fail erroneously.
remove_duplicated_disks([{Id, _, _}, {Id, Kbyte, Cap}|T]) ->
    remove_duplicated_disks([{Id, Kbyte, Cap}|T]);
remove_duplicated_disks([H|T]) ->
    [H|remove_duplicated_disks(T)];
remove_duplicated_disks([]) ->
    [].

get_alarms() ->
    lists:filter(fun({{disk_almost_full, _Disk},_}) -> true;
		    (_) -> false
		 end,
		 alarm_handler:get_alarms()).

until(Fun, [H|T]) ->
    case Fun(H) of
	true -> H;
	false ->
	    until(Fun, T)
    end;
until(_Fun, []) ->
    false.

port(suite) ->
    [];
port(doc) ->
    ["Test that disksup handles a terminating port program"];
port(Config) when is_list(Config) ->
    ?line Str = os:cmd("ps -ef | grep '[d]isksup'"),
    case io_lib:fread("~s ~s", Str) of
	 {ok, [_Uid,Pid], _Rest} ->

	    %% Monitor disksup
	    ?line MonRef = erlang:monitor(process, disksup),
	    ?line [{_Disk1,Kbyte1,_Cap1}|_] = disksup:get_disk_data(),
	    ?line true = Kbyte1>0,

	    %% Kill the port program
	    case os:cmd("kill -9 " ++ Pid) of
		[] ->

		    %% disksup should now terminate
		    receive
			{'DOWN', MonRef, _, _, {port_died, _Reason}} ->
			    ok;
			{'DOWN', MonRef, _, _, Reason} ->
			    ?line ?t:fail({unexpected_exit_reason, Reason})
		    after
			3000 ->
			    ?line ?t:fail({still_alive, Str})
		    end,

		    %% Give os_mon_sup time to restart disksup
		    ?t:sleep(?t:seconds(3)),
		    ?line [{_Disk2,Kbyte2,_Cap2}|_] =
			disksup:get_disk_data(),
		    ?line true = Kbyte2>0,

		    ok;

		Line ->
		    erlang:demonitor(MonRef),
		    {skip, {not_killed, Line}}
	    end;
	_ ->
	    {skip, {os_pid_not_found, Str}}
    end.

terminate(suite) ->
    [];
terminate(Config) when is_list(Config) ->
    ?line ok = application:set_env(os_mon, start_disksup, false),
    ?line ok = supervisor:terminate_child(os_mon_sup, disksup),
    ok.

unavailable(suite) ->
    [];
unavailable(doc) ->
    ["Test correct behaviour when service is unavailable"];
unavailable(Config) when is_list(Config) ->

    %% Make sure all API functions return their dummy values
    ?line [{"none",0,0}] = disksup:get_disk_data(),
    ?line 1800000 = disksup:get_check_interval(),
    ?line ok = disksup:set_check_interval(5),
    ?line 80 = disksup:get_almost_full_threshold(),
    ?line ok = disksup:set_almost_full_threshold(0.9),

    ok.

restart(suite) ->
    [];
restart(Config) when is_list(Config) ->
    ?line ok = application:set_env(os_mon, start_disksup, true),
    ?line {ok, _Pid} = supervisor:restart_child(os_mon_sup, disksup),
    ok.

otp_5910(suite) ->
    [];
otp_5910(doc) ->
    ["Test that alarms are cleared if disksup crashes or "
     "if OS_Mon is stopped"];
otp_5910(Config) when is_list(Config) ->

    %% Make sure disksup sets at least one alarm
    ?line Data = disksup:get_disk_data(),
    ?line Threshold0 = disksup:get_almost_full_threshold(),
    ?line Threshold = case over_threshold(Data, Threshold0) of
			  0 ->
			      [{_Id,_Kbyte,Cap}|_] = Data,
			      ?line ok = disksup:set_almost_full_threshold((Cap-1)/100),
			      Cap-1;
			  _N ->
			      Threshold0
		      end,
    ?line ok = application:set_env(os_mon,
				   disk_almost_full_threshold,
				   Threshold/100),
    ?line disksup ! timeout, % force a disk check
    ?line Data2 = disksup:get_disk_data(),
    ?line Over = over_threshold(Data2, Threshold),
    ?line Alarms = get_alarms(),
    if
	Over==0 ->
	    ?line ?t:fail({threshold_too_low, Data2, Threshold});
	Over==length(Alarms) ->
	    ok;
	true ->
	    dump_info(),
	    ?line ?t:fail({bad_alarms, Threshold, Data2, Alarms})
    end,

    %% Kill disksup
    exit(whereis(disksup), faked_disksup_crash),

    %% Wait a little to make sure disksup has been restarted,
    %% then make sure the alarms are set once, but not twice
    ?t:sleep(?t:seconds(1)),
    ?line Data3 = disksup:get_disk_data(),
    ?line Alarms2 = get_alarms(),
    if
	length(Alarms2)==length(Alarms) ->
	    ok;
	true ->
	    dump_info(),
	    ?line ?t:fail({bad_alarms, Threshold, Data3, Alarms,Alarms2})
    end,

    %% Stop OS_Mon and make sure all disksup alarms are cleared
    ?line ok = application:stop(os_mon),
    ?t:sleep(?t:seconds(1)),
    ?line Alarms3 = get_alarms(),
    if
	length(Alarms3)==0 ->
	    ok;
	true ->
	    ?line ?t:fail({alarms_not_cleared, Alarms3})
    end,

    %% Reset threshold and restart OS_Mon
    ?line ok = application:set_env(os_mon,
				   disksup_almost_full_threshold, 0.8),
    ?line ok = disksup:set_almost_full_threshold(0.8),
    ?line ok = application:start(os_mon),

    ok.

dump_info() ->
    io:format("Status: ~p~n", [sys:get_status(disksup)]).
