%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

-module(lcnt_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([t_load/1,
         t_conflicts/1,
         t_locations/1,
         t_swap_keys/1,
         t_implicit_start/1,
         t_crash_before_collect/1,
         smoke_lcnt/1]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    catch lcnt:stop(),
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,4}}].

all() ->
    [t_load, t_conflicts, t_locations, t_swap_keys, t_implicit_start,
     t_crash_before_collect, smoke_lcnt].

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

%% Load data from file.
t_load(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Files = [filename:join([Path,"big_bang_40.lcnt"]),
	     filename:join([Path,"ehb_3_3_hist.lcnt"])],
    ok = t_load_file(Files),
    ok.

t_load_file([]) -> ok;
t_load_file([File|Files]) ->
    {ok, _} = lcnt:start(),
    ok = lcnt:load(File),
    ok = lcnt:stop(),
    t_load_file(Files).

%% API: conflicts
t_conflicts(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Files = [filename:join([Path,"big_bang_40.lcnt"]),
	     filename:join([Path,"ehb_3_3_hist.lcnt"])],
    ok = t_conflicts_file(Files),
    ok.

t_conflicts_file([]) -> ok;
t_conflicts_file([File|Files]) ->
    {ok, _} = lcnt:start(),
    ok = lcnt:load(File),
    ok = lcnt:conflicts(),
    THs   = [-1, 5],
    Print = [name , id , type , entry , tries , colls , ratio , time , duration],
    Opts  = [
	[{sort, Sort}, {reverse, Rev}, {max_locks, ML}, {combine, Combine}, {thresholds, [TH]}, {print, [Print]}] ||
	    Sort    <- [name , type , tries , colls , ratio , time],
	    ML      <- [none, 32],
	    Combine <- [true, false],
	    TH      <- [{tries, Tries} || Tries <- THs] ++ [{colls, Colls} || Colls <- THs] ++ [{time, Time} || Time <- THs],
	    Rev     <- [true, false]
	],
    ok = test_conflicts_opts(Opts),
    ok = lcnt:stop(),
    t_conflicts_file(Files).


test_conflicts_opts([]) -> ok;
test_conflicts_opts([Opt|Opts]) ->
    ok = lcnt:conflicts(Opt),
    test_conflicts_opts(Opts).

%% API: locations
t_locations(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Files = [filename:join([Path,"big_bang_40.lcnt"]),
	     filename:join([Path,"ehb_3_3_hist.lcnt"])],
    ok = t_locations_file(Files),
    ok.

t_locations_file([]) -> ok;
t_locations_file([File|Files]) ->
    {ok, _} = lcnt:start(),
    ok = lcnt:load(File),
    ok = lcnt:locations(),
    THs   = [-1, 0, 100],
    Print = [name , id , type , entry , tries , colls , ratio , time , duration],
    Opts  = [
	[{full_id, Id}, {sort, Sort}, {max_locks, ML}, {combine, Combine}, {thresholds, [TH]}, {print, Print}] ||
	    Sort    <- [name , id , type , tries , colls , ratio , time , entry],
	    ML      <- [none, 64],
	    Combine <- [true, false],
	    TH      <- [{tries, Tries} || Tries <- THs] ++ [{colls, Colls} || Colls <- THs] ++ [{time, Time} || Time <- THs],
	    Id      <- [true, false]
	],
    ok = test_locations_opts(Opts),
    ok = lcnt:stop(),
    t_locations_file(Files).

test_locations_opts([]) -> ok;
test_locations_opts([Opt|Opts]) ->
    ok = lcnt:locations(Opt),
    test_locations_opts(Opts).

%% Test interchanging port/process id with class
t_swap_keys(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Files = [filename:join([Path,"big_bang_40.lcnt"]),
	     filename:join([Path,"ehb_3_3_hist.lcnt"])],
    ok = t_swap_keys_file(Files),
    ok.

t_swap_keys_file([]) -> ok;
t_swap_keys_file([File|Files]) ->
    {ok, _} = lcnt:start(),
    ok = lcnt:load(File),
    ok = lcnt:conflicts(),
    ok = lcnt:swap_pid_keys(),
    ok = lcnt:conflicts(),
    ok = lcnt:stop(),
    t_swap_keys_file(Files).

%% Prior to OTP-14913 this would crash with 'noproc' as the lcnt server hadn't
%% been started yet.
t_implicit_start(Config) when is_list(Config) ->
    ok = lcnt:conflicts().

t_crash_before_collect(Config) when is_list(Config) ->
    {ok, _} = lcnt:start(),
    ok = lcnt:information().

%% Simple smoke test of actual lock-counting, if running on
%% a run-time with lock-counting enabled.
smoke_lcnt(Config) ->
    case catch erlang:system_info(lock_counting) of
        true ->
            do_smoke_lcnt(Config);
        _ ->
            {skip,"Lock counting is not enabled"}
    end.

do_smoke_lcnt(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    SaveFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME)),
    {Time,ok} = timer:tc(fun() -> lcnt:apply(fun() -> big_bang(200) end) end),
    io:format("~p ms\n", [Time]),
    ok = lcnt:conflicts(),
    ok = lcnt:save(SaveFile),
    ok = lcnt:load(SaveFile),
    ok = lcnt:conflicts(),
    lcnt:stop().


%%%
%%% A slightly modified version of Rickard Green's Big Bang benchmark.
%%%

big_bang(N) when is_integer(N) ->
    Procs = spawn_procs(N),
    RMsgs = lists:map(fun (P) -> {done, P} end, Procs),
    send_procs(Procs, {procs, Procs, self()}),
    receive_msgs(RMsgs),
    lists:foreach(fun (P) -> exit(P, normal) end, Procs).

pinger([], [], true) ->
    receive
        {procs, Procs, ReportTo} ->
            pinger(Procs, [], ReportTo)
    end;
pinger([], [], false) ->
    receive {ping, From} -> From ! {pong, self()} end,
    pinger([],[],false);
pinger([], [], ReportTo) ->
    ReportTo ! {done, self()},
    pinger([],[],false);
pinger([],[Po|Pos] = Pongers, ReportTo) ->
    receive
        {ping, From} ->
            From ! {pong, self()},
            pinger([], Pongers, ReportTo);
        {pong, Po} ->
            pinger([], Pos, ReportTo)
    end;
pinger([Pi|Pis], Pongers, ReportTo) ->
    receive {ping, From} -> From ! {pong, self()}
    after 0 -> ok
    end,
    Pi ! {ping, self()},
    pinger(Pis, [Pi|Pongers], ReportTo).

spawn_procs(N) when N =< 0 ->
    [];
spawn_procs(N) ->
    [spawn_link(fun () -> pinger([], [], true) end) | spawn_procs(N-1)].

send_procs([], Msg) ->
    Msg;
send_procs([P|Ps], Msg) ->
    P ! Msg,
    send_procs(Ps, Msg).

receive_msgs([]) ->
    ok;
receive_msgs([M|Ms]) ->
    receive
        M ->
            receive_msgs(Ms)
    end.
