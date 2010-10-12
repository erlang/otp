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
-module(eprof_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0,groups/0,init_per_group/2,end_per_group/2,tiny/1,eed/1,basic/1]).

all() -> 
[basic, tiny, eed].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


basic(suite) -> [];
basic(Config) when is_list(Config) ->

    %% load eprof_test and change directory

    ?line {ok, OldCurDir} = file:get_cwd(),
    Datadir = ?config(data_dir, Config),
    Privdir = ?config(priv_dir, Config),
    ?line {ok,eprof_test} = compile:file(filename:join(Datadir, "eprof_test"),
					       [trace,{outdir, Privdir}]),
    ?line ok = file:set_cwd(Privdir),
    ?line code:purge(eprof_test),
    ?line {module,eprof_test} = code:load_file(eprof_test),

    %% rootset profiling

    ?line ensure_eprof_stopped(),
    ?line profiling = eprof:profile([self()]),
    ?line {error, already_profiling} = eprof:profile([self()]),
    ?line profiling_stopped = eprof:stop_profiling(),
    ?line profiling_already_stopped = eprof:stop_profiling(),
    ?line profiling = eprof:start_profiling([self(),self(),self()]),
    ?line profiling_stopped = eprof:stop_profiling(),

    %% with patterns

    ?line profiling = eprof:start_profiling([self()], {?MODULE, '_', '_'}),
    ?line {error, already_profiling} = eprof:start_profiling([self()], {?MODULE, '_', '_'}),
    ?line profiling_stopped = eprof:stop_profiling(),
    ?line profiling = eprof:start_profiling([self()], {?MODULE, start_stop, '_'}),
    ?line profiling_stopped = eprof:stop_profiling(),
    ?line profiling = eprof:start_profiling([self()], {?MODULE, start_stop, 1}),
    ?line profiling_stopped = eprof:stop_profiling(),

    %% with fun

    ?line {ok, _} = eprof:profile(fun() -> eprof_test:go(10) end),
    ?line profiling = eprof:profile([self()]),
    ?line {error, already_profiling} = eprof:profile(fun() -> eprof_test:go(10) end),
    ?line profiling_stopped = eprof:stop_profiling(),
    ?line {ok, _} = eprof:profile(fun() -> eprof_test:go(10) end),
    ?line {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end),
    ?line Pid     = whereis(eprof),
    ?line {ok, _} = eprof:profile(erlang:processes() -- [Pid], fun() -> eprof_test:go(10) end),
    ?line {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end, {eprof_test, '_', '_'}),
    ?line {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end, {eprof_test, go, '_'}),
    ?line {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end, {eprof_test, go, 1}),
    ?line {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end, {eprof_test, dec, 1}),

    %% error case

    ?line error     = eprof:profile([Pid], fun() -> eprof_test:go(10) end),
    ?line Pid       = whereis(eprof),
    ?line error     = eprof:profile([Pid], fun() -> eprof_test:go(10) end),
    ?line A         = spawn(fun() -> receive _ -> ok end end),
    ?line profiling = eprof:profile([A]),
    ?line true      = exit(A, kill_it),
    ?line profiling_stopped = eprof:stop_profiling(),
    ?line {error,_} = eprof:profile(fun() -> a = b end),

    %% with mfa

    ?line {ok, _} = eprof:profile([], eprof_test, go, [10]),
    ?line {ok, _} = eprof:profile([], eprof_test, go, [10], {eprof_test, dec, 1}),

    %% dump

    ?line {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end, {eprof_test, '_', '_'}),
    ?line [{_, Mfas}] = eprof:dump(),
    ?line Dec_mfa = {eprof_test, dec, 1},
    ?line Go_mfa  = {eprof_test, go,  1},
    ?line {value, {Go_mfa,  { 1, _Time1}}} = lists:keysearch(Go_mfa,  1, Mfas),
    ?line {value, {Dec_mfa, {11, _Time2}}} = lists:keysearch(Dec_mfa, 1, Mfas),

    %% change current working directory

    ?line ok = file:set_cwd(OldCurDir),
    ?line stopped = eprof:stop(),
    ok.

tiny(suite) -> [];
tiny(Config) when is_list(Config) -> 
    ?line ensure_eprof_stopped(),
    ?line {ok, OldCurDir} = file:get_cwd(),
    Datadir = ?config(data_dir, Config),
    Privdir = ?config(priv_dir, Config),
    ?line TTrap=?t:timetrap(60*1000),
    % (Trace)Compile to priv_dir and make sure the correct version is loaded.
    ?line {ok,eprof_suite_test} = compile:file(filename:join(Datadir,
							     "eprof_suite_test"),
					       [trace,{outdir, Privdir}]),
    ?line ok = file:set_cwd(Privdir),
    ?line code:purge(eprof_suite_test),
    ?line {module,eprof_suite_test} = code:load_file(eprof_suite_test),
    ?line {ok,_Pid} = eprof:start(),
    ?line nothing_to_analyze = eprof:analyze(),
    ?line nothing_to_analyze = eprof:analyze(total),
    ?line eprof:profile([], eprof_suite_test, test, [Config]),
    ?line ok = eprof:analyze(),
    ?line ok = eprof:analyze(total),
    ?line ok = eprof:log("eprof_SUITE_logfile"),
    ?line stopped = eprof:stop(),
    ?line ?t:timetrap_cancel(TTrap),
    ?line ok = file:set_cwd(OldCurDir),
    ok.

eed(suite) -> [];
eed(Config) when is_list(Config) ->
    ?line ensure_eprof_stopped(),
    ?line Datadir = ?config(data_dir, Config),
    ?line Privdir = ?config(priv_dir, Config),
    ?line TTrap=?t:timetrap(5*60*1000),

    %% (Trace)Compile to priv_dir and make sure the correct version is loaded.
    ?line code:purge(eed),
    ?line {ok,eed} = c:c(filename:join(Datadir, "eed"), [trace,{outdir,Privdir}]),
    ?line {ok,_Pid} = eprof:start(),
    ?line Script = filename:join(Datadir, "ed.script"),
    ?line ok = file:set_cwd(Datadir),
    ?line {T1,_} = statistics(runtime),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line {T2,_} = statistics(runtime),
    ?line {ok,ok} = eprof:profile([], eed, file, [Script]),
    ?line {T3,_} = statistics(runtime),
    ?line profiling_already_stopped = eprof:stop_profiling(),
    ?line ok = eprof:analyze(),
    ?line ok = eprof:analyze(total),
    ?line ok = eprof:log("eprof_SUITE_logfile"),
    ?line stopped = eprof:stop(),
    ?line ?t:timetrap_cancel(TTrap),
    S = lists:flatten(io_lib:format("~p times slower", [10*(T3-T2)/(T2-T1)])),
    {comment,S}.

ensure_eprof_stopped() ->
    Pid = whereis(eprof),
    case whereis(eprof) of
	undefined ->
	    ok;
	Pid ->
	    ?line stopped=eprof:stop()
    end.
