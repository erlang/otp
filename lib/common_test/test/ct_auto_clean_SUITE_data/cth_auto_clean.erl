%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(cth_auto_clean).

%% CTH Callbacks
-export([id/1, init/2,
	 pre_init_per_suite/3, post_init_per_suite/4,
         pre_end_per_suite/3, post_end_per_suite/4,
	 pre_init_per_group/4, post_init_per_group/5,
	 pre_end_per_group/4, post_end_per_group/5,
	 pre_init_per_testcase/4, post_init_per_testcase/5,
	 pre_end_per_testcase/4, post_end_per_testcase/5]).

id(_Opts) ->
    ?MODULE.

init(?MODULE, _Opts) ->
    ok.

pre_init_per_suite(_Suite, Config, State) ->
    identify(?FUNCTION_NAME),
    SharedGL = test_server_io:get_gl(true),
    SharedGL = find_and_kill(),
    do_until(fun() -> ct:remaining_test_procs() end, {[],SharedGL,[]}),
    %% get status of processes at startup, to be compared with end result
    {Config, [{all_procs,processes()} | State]}.

post_init_per_suite(_Suite, _Config, Return, State) ->
    identify(?FUNCTION_NAME),
    SharedGL = find_and_kill(),
    do_until(fun() -> ct:remaining_test_procs() end, {[],SharedGL,[]}),
    {Return, State}.

pre_end_per_suite(_Suite, Config, State) ->
    identify(?FUNCTION_NAME),
    SharedGL = find_and_kill(),
    do_until(fun() -> ct:remaining_test_procs() end, {[],SharedGL,[]}),
    {Config, State}.

post_end_per_suite(_Suite, _Config, Return, State) ->
    identify(?FUNCTION_NAME),
    SharedGL = find_and_kill(),
    do_until(fun() -> ct:remaining_test_procs() end, {[],SharedGL,[]}),
    AllProcs = processes(),
    Remaining = AllProcs--proplists:get_value(all_procs, State),
    ct:pal("Final remaining processes = ~p", [Remaining]),
    %% only the end_per_suite process shoud remain at this point!
    Remaining = [self()],
    {Return, State}.

pre_init_per_group(_Suite, _Group, Config, State) ->
    identify(?FUNCTION_NAME),
    SharedGL = find_and_kill(procs_and_gls),
    do_until(fun() -> ct:remaining_test_procs() end, {[],SharedGL,[]}),
    {Config, State}.

post_init_per_group(_Suite, _Group, _Config, Result, State) -> 
    identify(?FUNCTION_NAME),
    SharedGL = find_and_kill(procs_and_gls),
    do_until(fun() -> ct:remaining_test_procs() end, {[],SharedGL,[]}),
    {Result, State}.

pre_init_per_testcase(_Suite, _TC, Config, State) ->
    identify(?FUNCTION_NAME),
    ThisGL = group_leader(),
    find_and_kill(proc, ThisGL),
    case proplists:get_value(tc_group_properties, Config) of
        [{name,_},parallel] ->
            timer:sleep(1000);
        _ ->
            do_until(fun() -> element(1,ct:remaining_test_procs()) end, [])
    end,
    {Config, State}.

post_init_per_testcase(_Suite, _TC, Config, Return, State) ->
    identify(?FUNCTION_NAME),
    ThisGL = group_leader(),
    find_and_kill(proc, ThisGL),
    case proplists:get_value(tc_group_properties, Config) of
        [{name,_},parallel] ->
            timer:sleep(1000);
        _ ->
            do_until(fun() -> element(1,ct:remaining_test_procs()) end, [])
    end,
    {Return, State}.

pre_end_per_testcase(_Suite, _TC, Config, State) ->
    identify(?FUNCTION_NAME),
    ThisGL = group_leader(),
    find_and_kill(proc, ThisGL),
    case proplists:get_value(tc_group_properties, Config) of
        [{name,_},parallel] ->
            timer:sleep(1000);
        _ ->
            do_until(fun() -> element(1,ct:remaining_test_procs()) end, [])
    end,
    {Config, State}.

post_end_per_testcase(_Suite, _TC, Config, Result, State) ->
    identify(?FUNCTION_NAME),
    ThisGL = group_leader(),
    find_and_kill(proc, ThisGL),
    case proplists:get_value(tc_group_properties, Config) of
        [{name,_},parallel] ->
            timer:sleep(1000);
        _ ->
            do_until(fun() -> element(1,ct:remaining_test_procs()) end, [])
    end,
    {Result, State}.

pre_end_per_group(_Suite, _Group, Config, State) ->
    identify(?FUNCTION_NAME),
    SharedGL = find_and_kill(procs_and_gls),
    do_until(fun() -> ct:remaining_test_procs() end, {[],SharedGL,[]}),
    {Config, State}.

post_end_per_group(_Suite, _Group, _Config, Return, State) ->
    identify(?FUNCTION_NAME),
    SharedGL = find_and_kill(procs_and_gls),
    do_until(fun() -> ct:remaining_test_procs() end, {[],SharedGL,[]}),
    {Return, State}.


%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

identify(Func) ->
    ct:pal("********** THIS IS ~w on ~w", [Func, self()]),
    ok.

find_and_kill() ->
    find_and_kill(procs).

find_and_kill(procs) ->
    {Procs,SharedGL,_ParallelGLs} = ct:remaining_test_procs(),
    ct:pal("Remaining test processes = ~p", [pi(Procs)]),
    [pkill(P, kill) || {P,_GL} <- Procs],
    SharedGL;

find_and_kill(procs_and_gls) ->
    {Procs,SharedGL,GLs} = ct:remaining_test_procs(),
    ct:pal("Remaining test processes = ~p", [pi(Procs)]),
    [pkill(P, kill) || {P,_GL} <- Procs],
    ct:pal("Remaining group leaders = ~p", [pi(GLs)]),
    [pkill(GL, kill) || GL <- GLs, GL /= SharedGL],
    SharedGL.

find_and_kill(proc, ProcGL) ->
    {Procs,SharedGL,GLs} = ct:remaining_test_procs(),
    ct:pal("Remaining test processes = ~p", [pi(Procs++GLs)]),
    [pkill(P, kill) || {P,GL} <- Procs, GL == ProcGL],
    SharedGL.

pi([{P,_GL}|Ps]) ->
    pi([P|Ps]);
pi([P|Ps]) ->
    case node() == node(P) of
        true ->
            {_,GL} = process_info(P,group_leader),
            {_,CF} = process_info(P,current_function),
            {_,IC} = process_info(P,initial_call),
            {_,D} = process_info(P,dictionary),
            Shared = test_server_io:get_gl(true),
            User = whereis(user),
            if (GL /= P) and (GL /= Shared) and (GL /= User) ->
                    [{P,GL,CF,IC,D} | pi([GL|Ps])];
               true ->
                    [{P,GL,CF,IC,D} | pi(Ps)]
            end;
        false ->
            pi(Ps)
    end;
pi([]) ->
    [].

do_until(Fun, Until) ->
    io:format("Will do until ~p~n", [Until]),
    do_until(Fun, Until, 1000).

do_until(_, Until, 0) ->
    io:format("Couldn't get ~p~n", [Until]),
    exit({not_reached,Until});

do_until(Fun, Until, N) ->
    case Fun() of
        Until ->
            ok;
        _Tmp ->
            do_until(Fun, Until, N-1)
    end.

pkill(P, How) ->
    ct:pal("KILLING ~w NOW!", [P]),
    exit(P, How).

