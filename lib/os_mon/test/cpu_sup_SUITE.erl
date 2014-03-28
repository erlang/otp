%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2014. All Rights Reserved.
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
-module(cpu_sup_SUITE).
-include_lib("test_server/include/test_server.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([load_api/1]).
-export([util_api/1, util_values/1]).
-export([port/1]).
-export([terminate/1, unavailable/1, restart/1]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(1)).

init_per_suite(Config) when is_list(Config) ->
    ?line ok = application:start(os_mon),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ?line ok = application:stop(os_mon),
    Config.

init_per_testcase(unavailable, Config) ->
    terminate(Config),
    init_per_testcase(dummy, Config);
init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].

end_per_testcase(unavailable, Config) ->
    restart(Config),
    end_per_testcase(dummy, Config);
end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case test_server:os_type() of
	{unix, sunos} ->
	    [load_api, util_api, util_values, port, unavailable];
	{unix, linux} ->
	    [load_api, util_api, util_values, port, unavailable];
	{unix, _OSname} -> [load_api];
	_OS -> [unavailable]
    end.

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


load_api(suite) ->
    [];
load_api(doc) ->
    ["Test of load API functions"];
load_api(Config) when is_list(Config) ->

    %% nprocs()
    ?line N = cpu_sup:nprocs(),
    ?line true = is_integer(N),
    ?line true = N>0,
    ?line true = N<1000000,

    %% avg1()
    ?line Load1 = cpu_sup:avg1(),
    ?line true = is_integer(Load1),
    ?line true = Load1>0,

    %% avg5()
    ?line Load5 = cpu_sup:avg5(),
    ?line true = is_integer(Load5),
    ?line true = Load5>0,

    %% avg15()
    ?line Load15 = cpu_sup:avg15(),
    ?line true = is_integer(Load15),
    ?line true = Load15>0,

    ok.

util_api(suite) ->
    [];
util_api(doc) ->
    ["Test of utilization API functions"];
util_api(Config) when is_list(Config) ->
    %% Some useful funs when testing util/1
    BusyP = fun({user, _Share}) -> true;
	       ({nice_user, _Share}) -> true;
	       ({kernel, _Share}) -> true;
	       ({hard_irq, _Share}) -> true;
	       ({soft_irq, _Share}) -> true;
	       (_) -> false
	    end,
    NonBusyP = fun({wait, _Share}) -> true;
		  ({idle, _Share}) -> true;
		  ({steal, _Share}) -> true;
		  (_) -> false
	       end,
    Sum = fun({_Tag, X}, Acc) -> Acc+X end,

    %% util()
    ?line Util1 = cpu_sup:util(),
    ?line true = is_number(Util1),
    ?line true = Util1>0,
    ?line Util2 = cpu_sup:util(),
    ?line true = is_number(Util2),
    ?line true = Util2>0,

    %% util([])
    ?line {all, Busy1, NonBusy1, []} = cpu_sup:util([]),
    ?line 100.00 = Busy1 + NonBusy1,

    %% util([detailed])
    ?line {Cpus2, Busy2, NonBusy2, []} = cpu_sup:util([detailed]),
    ?line true = lists:all(fun(X) -> is_integer(X) end, Cpus2),
    ?line true = lists:all(BusyP, Busy2),
    ?line true = lists:all(NonBusyP, NonBusy2),
    ?line 100.00 = lists:foldl(Sum,0,Busy2)+lists:foldl(Sum,0,NonBusy2),

    %% util([per_cpu])
    ?line [{Cpu3, Busy3, NonBusy3, []}|_] = cpu_sup:util([per_cpu]),
    ?line true = is_integer(Cpu3),
    ?line 100.00 = Busy3 + NonBusy3,

    %% util([detailed, per_cpu])
    ?line [{Cpu4, Busy4, NonBusy4, []}|_] =
	cpu_sup:util([detailed, per_cpu]),
    ?line true = is_integer(Cpu4),
    ?line true = lists:all(BusyP, Busy2),
    ?line true = lists:all(NonBusyP, NonBusy2),
    ?line 100.00 = lists:foldl(Sum,0,Busy4)+lists:foldl(Sum,0,NonBusy4),

    %% bad util/1 calls
    ?line {'EXIT',{badarg,_}} = (catch cpu_sup:util(detailed)),
    ?line {'EXIT',{badarg,_}} = (catch cpu_sup:util([detialed])),

    ok.

-define(SPIN_TIME, 1000).

util_values(suite) ->
    [];
util_values(doc) ->
    ["Test utilization values"];
util_values(Config) when is_list(Config) ->

    Tester = self(),
    Ref = make_ref(),
    Loop = fun (L) -> L(L) end,
    Spinner = fun () ->
		      Looper = spawn_link(fun () -> Loop(Loop) end),
		      receive after ?SPIN_TIME -> ok end,
		      unlink(Looper),
		      exit(Looper, kill),
		      Tester ! Ref
	      end,

    ?line cpu_sup:util(),

    ?line spawn_link(Spinner),
    ?line receive Ref -> ok end,
    ?line HighUtil1 = cpu_sup:util(),

    ?line receive after ?SPIN_TIME -> ok end,
    ?line LowUtil1 = cpu_sup:util(),

    ?line spawn_link(Spinner),
    ?line receive Ref -> ok end,
    ?line HighUtil2 = cpu_sup:util(),

    ?line receive after ?SPIN_TIME -> ok end,
    ?line LowUtil2 = cpu_sup:util(),

    Utils = [{high1,HighUtil1}, {low1,LowUtil1},
	     {high2,HighUtil2}, {low2,LowUtil2}],
    ?t:format("Utils: ~p~n", [Utils]),

    ?line false = LowUtil1 > HighUtil1,
    ?line false = LowUtil1 > HighUtil2,
    ?line false = LowUtil2 > HighUtil1,
    ?line false = LowUtil2 > HighUtil2,

    ok.


% Outdated
% The portprogram is now restarted if killed, and not by os_mon...

port(suite) ->
    [];
port(doc) ->
    ["Test that cpu_sup handles a terminating port program"];
port(Config) when is_list(Config) ->
    case cpu_sup_os_pid() of
	{ok, PidStr} ->
	    %% Monitor cpu_sup
	    ?line MonRef = erlang:monitor(process, cpu_sup),
	    ?line N1 = cpu_sup:nprocs(),
	    ?line true = N1>0,

	    %% Kill the port program
	    case os:cmd("kill -9 " ++ PidStr) of
		[] ->
		    %% cpu_sup should not terminate
		    receive
			{'DOWN', MonRef, _, _, Reason} ->
			    ?line ?t:fail({unexpected_exit_reason, Reason})
		    after 3000 ->
			ok
		    end,

		    %% Give cpu_sup time to restart cpu_sup port
		    ?t:sleep(?t:seconds(3)),
		    ?line N2 = cpu_sup:nprocs(),
		    ?line true = N2>0,

		    erlang:demonitor(MonRef),
		    ok;

		Line ->
		    erlang:demonitor(MonRef),
		    {skip, {not_killed, Line}}
	    end;
	_ ->
	    {skip, os_pid_not_found }
    end.

terminate(suite) ->
    [];
terminate(Config) when is_list(Config) ->
    ok = application:set_env(os_mon, start_cpu_sup, false),
    _ = supervisor:terminate_child(os_mon_sup, cpu_sup),
    ok.

unavailable(suite) ->
    [];
unavailable(doc) ->
    ["Test correct behaviour when service is unavailable"];
unavailable(Config) when is_list(Config) ->

    %% Make sure all API functions return their dummy values
    ?line 0 = cpu_sup:nprocs(),
    ?line 0 = cpu_sup:avg1(),
    ?line 0 = cpu_sup:avg5(),
    ?line 0 = cpu_sup:avg15(),
    ?line 0 = cpu_sup:util(),
    ?line {all,0,0,[]} = cpu_sup:util([]),
    ?line {all,0,0,[]} = cpu_sup:util([detailed]),
    ?line {all,0,0,[]} = cpu_sup:util([per_cpu]),
    ?line {all,0,0,[]} = cpu_sup:util([detailed,per_cpu]),

    ok.

restart(suite) ->
    [];
restart(Config) when is_list(Config) ->
    ?line ok = application:set_env(os_mon, start_cpu_sup, true),
    ?line {ok, _Pid} = supervisor:restart_child(os_mon_sup, cpu_sup),
    ok.

%% Aux

cpu_sup_os_pid() ->
    Str = os:cmd("ps -e | grep '[c]pu_sup'"),
    case io_lib:fread("~s", Str) of
	{ok, [Pid], _Rest} -> {ok, Pid};
	_ -> {error, pid_not_found}
    end.
