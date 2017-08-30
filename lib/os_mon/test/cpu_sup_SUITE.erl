%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
-module(cpu_sup_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([load_api/1]).
-export([util_api/1, util_values/1]).
-export([port/1]).
-export([terminate/1, unavailable/1, restart/1]).

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

end_per_testcase(unavailable, Config) ->
    restart(Config),
    end_per_testcase(dummy, Config);
end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    case test_server:os_type() of
        {unix, sunos} ->
            [load_api, util_api, util_values, port, unavailable];
        {unix, linux} ->
            [load_api, util_api, util_values, port, unavailable];
        {unix, freebsd} ->
            [load_api, util_api, util_values, port, unavailable];
        {unix, darwin} ->
            [load_api, util_api, util_values, port, unavailable];
        {unix, _OSname} -> [load_api];
        _OS -> [unavailable]
    end.

%% Test of load API functions
load_api(Config) when is_list(Config) ->

    %% nprocs()
    N = cpu_sup:nprocs(),
    true = is_integer(N),
    true = N>0,
    true = N<1000000,

    %% avg1()
    Load1 = cpu_sup:avg1(),
    true = is_integer(Load1),
    true = Load1>0,

    %% avg5()
    Load5 = cpu_sup:avg5(),
    true = is_integer(Load5),
    true = Load5>0,

    %% avg15()
    Load15 = cpu_sup:avg15(),
    true = is_integer(Load15),
    true = Load15>0,

    ok.

%% Test of utilization API functions
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
    Util1 = cpu_sup:util(),
    true = is_number(Util1),
    true = Util1>0,
    Util2 = cpu_sup:util(),
    true = is_number(Util2),
    true = Util2>0,

    %% util([])
    {all, Busy1, NonBusy1, []} = cpu_sup:util([]),
    100.00 = Busy1 + NonBusy1,

    %% util([detailed])
    {Cpus2, Busy2, NonBusy2, []} = cpu_sup:util([detailed]),
    true = lists:all(fun(X) -> is_integer(X) end, Cpus2),
    true = lists:all(BusyP, Busy2),
    true = lists:all(NonBusyP, NonBusy2),
    100.00 = lists:foldl(Sum,0,Busy2)+lists:foldl(Sum,0,NonBusy2),

    %% util([per_cpu])
    [{Cpu3, Busy3, NonBusy3, []}|_] = cpu_sup:util([per_cpu]),
    true = is_integer(Cpu3),
    100.00 = Busy3 + NonBusy3,

    %% util([detailed, per_cpu])
    [{Cpu4, Busy4, NonBusy4, []}|_] =
    cpu_sup:util([detailed, per_cpu]),
    true = is_integer(Cpu4),
    true = lists:all(BusyP, Busy2),
    true = lists:all(NonBusyP, NonBusy2),
    100.00 = lists:foldl(Sum,0,Busy4)+lists:foldl(Sum,0,NonBusy4),

    %% bad util/1 calls
    {'EXIT',{badarg,_}} = (catch cpu_sup:util(detailed)),
    {'EXIT',{badarg,_}} = (catch cpu_sup:util([detialed])),

    ok.

-define(SPIN_TIME, 1000).

%% Test utilization values
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

    cpu_sup:util(),

    spawn_link(Spinner),
    receive Ref -> ok end,
    HighUtil1 = cpu_sup:util(),

    receive after ?SPIN_TIME -> ok end,
    LowUtil1 = cpu_sup:util(),

    spawn_link(Spinner),
    receive Ref -> ok end,
    HighUtil2 = cpu_sup:util(),

    receive after ?SPIN_TIME -> ok end,
    LowUtil2 = cpu_sup:util(),

    Utils = [{high1,HighUtil1}, {low1,LowUtil1},
             {high2,HighUtil2}, {low2,LowUtil2}],
    io:format("Utils: ~p~n", [Utils]),

    false = LowUtil1 > HighUtil1,
    false = LowUtil1 > HighUtil2,
    false = LowUtil2 > HighUtil1,
    false = LowUtil2 > HighUtil2,

    ok.


% Outdated
% The portprogram is now restarted if killed, and not by os_mon...

%% Test that cpu_sup handles a terminating port program
port(Config) when is_list(Config) ->
    case cpu_sup_os_pid() of
        {ok, PidStr} ->
            %% Monitor cpu_sup
            MonRef = erlang:monitor(process, cpu_sup),
            N1 = cpu_sup:nprocs(),
            true = N1>0,

            %% Kill the port program
            case os:cmd("kill -9 " ++ PidStr) of
                [] ->
                    %% cpu_sup should not terminate
                    receive
                        {'DOWN', MonRef, _, _, Reason} ->
                            ct:fail({unexpected_exit_reason, Reason})
                    after 3000 ->
                              ok
                    end,

                    %% Give cpu_sup time to restart cpu_sup port
                    ct:sleep({seconds, 3}),
                    N2 = cpu_sup:nprocs(),
                    true = N2>0,

                    erlang:demonitor(MonRef),
                    ok;

                Line ->
                    erlang:demonitor(MonRef),
                    {skip, {not_killed, Line}}
            end;
        _ ->
            {skip, os_pid_not_found }
    end.

terminate(Config) when is_list(Config) ->
    ok = application:set_env(os_mon, start_cpu_sup, false),
    _ = supervisor:terminate_child(os_mon_sup, cpu_sup),
    ok.

%% Test correct behaviour when service is unavailable
unavailable(Config) when is_list(Config) ->

    %% Make sure all API functions return their dummy values
    0 = cpu_sup:nprocs(),
    0 = cpu_sup:avg1(),
    0 = cpu_sup:avg5(),
    0 = cpu_sup:avg15(),
    0 = cpu_sup:util(),
    {all,0,0,[]} = cpu_sup:util([]),
    {all,0,0,[]} = cpu_sup:util([detailed]),
    {all,0,0,[]} = cpu_sup:util([per_cpu]),
    {all,0,0,[]} = cpu_sup:util([detailed,per_cpu]),

    ok.

restart(Config) when is_list(Config) ->
    ok = application:set_env(os_mon, start_cpu_sup, true),
    supervisor:restart_child(os_mon_sup, cpu_sup),
    ok.

%% Aux

cpu_sup_os_pid() ->
    Str = os:cmd("ps -e | grep '[c]pu_sup'"),
    case io_lib:fread("~s", Str) of
        {ok, [Pid], _Rest} -> {ok, Pid};
        _ -> {error, pid_not_found}
    end.
