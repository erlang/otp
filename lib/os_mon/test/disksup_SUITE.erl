%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(disksup_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([api/1, config/1, alarm/1]).
-export([port/1]).
-export([terminate/1, unavailable/1, restart/1]).
-export([otp_5910/1]).
-export([posix_only/1, parse_df_output_posix/1, parse_df_output_susv3/1]).

init_per_suite(Config) when is_list(Config) ->
    ok = application:start(os_mon),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok = application:stop(os_mon),
    Config.

init_per_testcase(unavailable, Config) ->
    terminate(Config),
    init_per_testcase(dummy, Config);
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(TC, Config) when TC =:= unavailable;
                                  TC =:= posix_only ->
    restart(Config),
    end_per_testcase(dummy, Config);
end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    Bugs = [otp_5910],
    Always = [api, config, alarm, port, posix_only, unavailable,
              parse_df_output_posix, parse_df_output_susv3] ++ Bugs,
    case test_server:os_type() of
	{unix, _OSname} -> Always;
	{win32, _OSname} -> Always;
	_OS -> [unavailable]
    end.

%% Test of API functions
api(Config) when is_list(Config) ->

    %% get_disk_data()
    ok = check_get_disk_data(),

    %% get_check_interval()
    1800000 = disksup:get_check_interval(),

    %% set_check_interval(Minutes)
    ok = disksup:set_check_interval(20),
    1200000 = disksup:get_check_interval(),
    {'EXIT',{badarg,_}} = (catch disksup:set_check_interval(0.5)),
    1200000 = disksup:get_check_interval(),
    ok = disksup:set_check_interval(30),

    %% get_almost_full_threshold()
    80 = disksup:get_almost_full_threshold(),

    %% set_almost_full_threshold(Float)
    ok = disksup:set_almost_full_threshold(0.90),
    90 = disksup:get_almost_full_threshold(),
    {'EXIT',{badarg,_}} =
	(catch disksup:set_almost_full_threshold(-0.5)),
    90 = disksup:get_almost_full_threshold(),
    ok = disksup:set_almost_full_threshold(0.80),

    ok.

%% Test configuration
config(Config) when is_list(Config) ->

    %% Change configuration parameters and make sure change is reflected
    %% when disksup is restarted
    ok = application:set_env(os_mon, disk_space_check_interval, 29),
    ok = application:set_env(os_mon, disk_almost_full_threshold, 0.81),

    ok = supervisor:terminate_child(os_mon_sup, disksup),
    {ok, _Child1} = supervisor:restart_child(os_mon_sup, disksup),

    1740000 = disksup:get_check_interval(),
    81 = disksup:get_almost_full_threshold(),

    %% Also try this with bad parameter values, should be ignored
    ok =
	application:set_env(os_mon, disk_space_check_interval, 0.5),
    ok =
	application:set_env(os_mon, disk_almost_full_threshold, -0.81),

    ok = supervisor:terminate_child(os_mon_sup, disksup),
    {ok, _Child2} = supervisor:restart_child(os_mon_sup, disksup),

    1800000 = disksup:get_check_interval(),
    80 = disksup:get_almost_full_threshold(),

    %% Reset configuration parameters
    ok = application:set_env(os_mon, disk_space_check_interval, 30),
    ok = application:set_env(os_mon, disk_almost_full_threshold, 0.80),
    ok.

%%----------------------------------------------------------------------
%% NOTE: The test case is a bit weak as it will fail if the disk usage
%% changes too much during its course, or if there are timing problems
%% with the alarm_handler receiving the alarms too late
%%----------------------------------------------------------------------

%% Test that alarms are set and cleared
alarm(Config) when is_list(Config) ->

    %% Find out how many disks exceed the threshold
    %% and make sure the corresponding number of alarms is set
    Threshold1 = disksup:get_almost_full_threshold(), % 80
    Data1 = disksup:get_disk_data(),
    Over1 = over_threshold(Data1, Threshold1),
    Alarms1 = get_alarms(),
    if
	Over1==length(Alarms1) ->
	    true;
	true ->
	    dump_info(),
	    ct:fail({bad_alarms, Threshold1, Data1, Alarms1})
    end,

    %% Try to find a disk with space usage below Threshold1,
    %% lower the threshold accordingly and make sure new alarms are set
    Fun1 = fun({_Id, _Kbyte, Capacity}) ->
		   if
		       Capacity>0, Capacity<Threshold1 -> true;
		       true -> false
		   end
	   end,
    case until(Fun1, Data1) of
	      {_, _, Cap1} ->
		  Threshold2 = Cap1-1,
		  ok =
		      disksup:set_almost_full_threshold(Threshold2/100),
		  disksup ! timeout, % force a disk check
		  Data2 = disksup:get_disk_data(),
		  Over2 = over_threshold(Data2, Threshold2),
		  Alarms2 = get_alarms(),
		  if
		      Over2==length(Alarms2), Over2>Over1 ->
			  true;
		      true ->
			  dump_info(),
			  ct:fail({bad_alarms, Threshold2, Data2, Alarms2})
		  end;
	      false ->
		  ignore
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
    case lists:foldl(Fun2, 0, Data1) of
	      Max when Max<100 ->
		  Threshold3 = Max+1,
		  ok = disksup:set_almost_full_threshold(Threshold3/100),
		  disksup ! timeout, % force a disk check
		  Data3   = disksup:get_disk_data(),
		  Over3   = over_threshold(Data3, Threshold3),
		  Alarms3 = get_alarms(),
		  if
		      Over3==0, length(Alarms3)==0 ->
			  ok;
		      true ->
			  dump_info(),
			  ct:fail({bad_alarms, Threshold3, Data3, Alarms3})
		  end;
	      100 ->
		  ignore
	  end,

    %% Reset threshold
    ok = disksup:set_almost_full_threshold(Threshold1/100),
    ok.

over_threshold(Data, Threshold) ->
    Data2 = remove_duplicated_disks(lists:keysort(1, Data)),
    lists:foldl(fun
	    ({_Id, _Kbyte, Cap}, N) when Cap>=Threshold -> N+1;
	    (_DiskData, N) -> N
	end, 0, Data2).

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
    lists:filter(fun
	    ({{disk_almost_full, _Disk},_}) -> true;
	    (_) -> false
	end, alarm_handler:get_alarms()).

until(Fun, [H|T]) ->
    case Fun(H) of
	true -> H;
	false -> until(Fun, T)
    end;
until(_Fun, []) -> false.

%% Test that disksup handles a terminating port program
port(Config) when is_list(Config) ->
    Str = os:cmd("ps -ef | grep '[d]isksup'"),
    case io_lib:fread("~s ~s", Str) of
	{ok, [_Uid,Pid], _Rest} ->

	    %% Monitor disksup
	    MonRef = erlang:monitor(process, disksup),
	    [{_Disk1,Kbyte1,_Cap1}|_] = disksup:get_disk_data(),
	    true = Kbyte1>0,

	    %% Kill the port program
	    case os:cmd("kill -9 " ++ Pid) of
		[] ->

		    %% disksup should now terminate
		    receive
			{'DOWN', MonRef, _, _, {port_died, _Reason}} ->
			    ok;
			{'DOWN', MonRef, _, _, Reason} ->
			    ct:fail({unexpected_exit_reason, Reason})
		    after
			3000 ->
			    ct:fail({still_alive, Str})
		    end,

		    %% Give os_mon_sup time to restart disksup
		    ct:sleep({seconds,3}),
		    [{_Disk2,Kbyte2,_Cap2}|_] = disksup:get_disk_data(),
		    true = Kbyte2>0,

		    ok;

		Line ->
		    erlang:demonitor(MonRef),
		    {skip, {not_killed, Line}}
	    end;
	_ ->
	    {skip, {os_pid_not_found, Str}}
    end.

terminate(Config) when is_list(Config) ->
    ok = application:set_env(os_mon, start_disksup, false),
    ok = supervisor:terminate_child(os_mon_sup, disksup),
    ok.

%% Test correct behaviour when service is unavailable
unavailable(Config) when is_list(Config) ->

    %% Make sure all API functions return their dummy values
    [{"none",0,0}] = disksup:get_disk_data(),
    1800000 = disksup:get_check_interval(),
    ok = disksup:set_check_interval(5),
    80 = disksup:get_almost_full_threshold(),
    ok = disksup:set_almost_full_threshold(0.9),
    ok.

restart(Config) when is_list(Config) ->
    ok = application:set_env(os_mon, start_disksup, true),
    ok = application:set_env(os_mon, disksup_posix_only, false),
    case supervisor:restart_child(os_mon_sup, disksup) of
        {ok, _Pid} -> ok;
        {error, running} -> ok
    end.

%% Test that alarms are cleared if disksup crashes or
%% if OS_Mon is stopped
otp_5910(Config) when is_list(Config) ->

    %% Make sure disksup sets at least one alarm
    Data = lists:sort(disksup:get_disk_data()),
    Threshold0 = disksup:get_almost_full_threshold(),
    Threshold  = case over_threshold(Data, Threshold0) of
		     0 ->
			 [{_Id,_Kbyte,Cap}|_] = Data,
			 io:format("Data ~p Threshold ~p ~n",[Data, Cap-1]),
			 ok = disksup:set_almost_full_threshold((Cap-1)/100),
			 Cap-1;
		     _N -> Threshold0
		 end,
    ok = application:set_env(os_mon, disk_almost_full_threshold, Threshold/100),
    disksup ! timeout, % force a disk check
    Data2 = disksup:get_disk_data(),
    Over = over_threshold(Data2, Threshold),
    Alarms = get_alarms(),
    if
	Over==0 ->
	    ct:fail({threshold_too_low, Data2, Threshold});
	Over==length(Alarms) ->
	    ok;
	true ->
	    dump_info(),
	    ct:fail({bad_alarms, Threshold, Data2, Alarms})
    end,

    %% Kill disksup
    exit(whereis(disksup), faked_disksup_crash),

    %% Wait a little to make sure disksup has been restarted,
    %% then make sure the alarms are set once, but not twice
    ct:sleep({seconds,1}),
    Data3   = disksup:get_disk_data(),
    Alarms2 = get_alarms(),
    if
	length(Alarms2)==length(Alarms) -> ok;
	true ->
	    dump_info(),
	    ct:fail({bad_alarms,Threshold,Data3,Alarms,Alarms2})
    end,

    %% Stop OS_Mon and make sure all disksup alarms are cleared
    ok = application:stop(os_mon),
    ct:sleep({seconds,1}),
    Alarms3 = get_alarms(),
    case get_alarms() of
	[] -> ok;
	_  -> ct:fail({alarms_not_cleared, Alarms3})
    end,

    %% Reset threshold and restart OS_Mon
    ok = application:set_env(os_mon, disksup_almost_full_threshold, 0.8),
    ok = disksup:set_almost_full_threshold(0.8),
    ok = application:start(os_mon),
    ok.

%% Test disksup_posix_only option
posix_only(Config) when is_list(Config) ->
    %% Set option and restart disksup
    ok = application:set_env(os_mon, disksup_posix_only, true),
    ok = supervisor:terminate_child(os_mon_sup, disksup),
    {ok, _Child1} = supervisor:restart_child(os_mon_sup, disksup),

    ok = check_get_disk_data().

dump_info() ->
    io:format("Status: ~p~n", [sys:get_status(disksup)]).

check_get_disk_data() ->
    [{Id,KByte,Capacity}|_] = get_disk_data(),
    true = io_lib:printable_list(Id),
    true = is_integer(KByte),
    true = is_integer(Capacity),
    true = Capacity>0,
    true = KByte>0,
    ok.

% filter get_disk_data and remove entriew with zero capacity
% "non-normal" filesystems report zero capacity
% - Perhaps errorneous 'df -k -l'?
% - Always list filesystems by type '-t ufs,zfs,..' instead?
% It is unclear what the intention was from the beginning.
get_disk_data() ->
    get_disk_data(disksup:get_disk_data()).

get_disk_data([{"none",0,0}=E]) -> [E];
get_disk_data([{_,_,0}|Es]) -> get_disk_data(Es);
get_disk_data([E|Es]) -> [E|get_disk_data(Es)];
get_disk_data([]) -> [].

%% @doc Test various expected inputs to 'df' command output (Linux/POSIX)
parse_df_output_posix(Config) when is_list(Config) ->
    PosixHdr = "Filesystem     1K-blocks     Used Available Use% Mounted on\n",
    {error, _} = disksup:parse_df(PosixHdr, posix),
    {error, _} = disksup:parse_df("", posix),
    {error, _} = disksup:parse_df("\n\n", posix),

    %% Have a simple example with no funny spaces in mount path
    Posix1 = "tmpfs             498048     7288    490760   2% /run\n",
    {ok, {498048, 2, "/run"}, ""} = disksup:parse_df(Posix1, posix),

    %% Have a mount path with some spaces in it
    Posix2 = "tmpfs             498048     7288    490760   2% /spaces 1 2\n",
    {ok, {498048, 2, "/spaces 1 2"}, ""} = disksup:parse_df(Posix2, posix).

%% @doc Test various expected inputs to 'df' command output (Darwin/SUSv3)
parse_df_output_susv3(Config) when is_list(Config) ->
    DarwinHdr = "Filesystem 1024-blocks      Used Available Capacity " ++
                "iused      ifree %iused  Mounted on",
    {error, _} = disksup:parse_df(DarwinHdr, susv3),
    {error, _} = disksup:parse_df("", susv3),
    {error, _} = disksup:parse_df("\n\n", susv3),

    %% Have a simple example with no funny spaces in mount path
    Darwin1 = "/dev/disk1   243949060 157002380  86690680    65% 2029724 " ++
              "4292937555    0%   /\n",
    {ok, {243949060, 65, "/"}, ""} = disksup:parse_df(Darwin1, susv3),

    %% Have a mount path with some spaces in it
    Darwin2 = "/dev/disk1   243949060 157002380  86690680    65% 2029724 " ++
              "4292937555    0%   /spaces 1 2\n",
    {ok, {243949060, 65, "/spaces 1 2"}, ""} = disksup:parse_df(Darwin2, susv3).
