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
-module(eprof_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0]).
-export([tiny/1,eed/1,basic/1,basic_option/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,60}}].

all() -> 
    [basic, basic_option, tiny, eed].

basic(Config) when is_list(Config) ->

    %% load eprof_test and change directory

    {ok, OldCurDir} = file:get_cwd(),
    Datadir = proplists:get_value(data_dir, Config),
    Privdir = proplists:get_value(priv_dir, Config),
    {ok,eprof_test} = compile:file(filename:join(Datadir, "eprof_test"),
					       [trace,{outdir, Privdir}]),
    ok = file:set_cwd(Privdir),
    code:purge(eprof_test),
    {module,eprof_test} = code:load_file(eprof_test),

    %% rootset profiling

    ensure_eprof_stopped(),
    profiling = eprof:profile([self()]),
    {error, already_profiling} = eprof:profile([self()]),
    profiling_stopped = eprof:stop_profiling(),
    profiling_already_stopped = eprof:stop_profiling(),
    profiling = eprof:start_profiling([self(),self(),self()]),
    profiling_stopped = eprof:stop_profiling(),

    %% with patterns

    profiling = eprof:start_profiling([self()], {?MODULE, '_', '_'}),
    {error, already_profiling} = eprof:start_profiling([self()], {?MODULE, '_', '_'}),
    profiling_stopped = eprof:stop_profiling(),
    profiling = eprof:start_profiling([self()], {?MODULE, start_stop, '_'}),
    profiling_stopped = eprof:stop_profiling(),
    profiling = eprof:start_profiling([self()], {?MODULE, start_stop, 1}),
    profiling_stopped = eprof:stop_profiling(),

    %% with fun

    {ok, _} = eprof:profile(fun() -> eprof_test:go(10) end),
    profiling = eprof:profile([self()]),
    {error, already_profiling} = eprof:profile(fun() -> eprof_test:go(10) end),
    profiling_stopped = eprof:stop_profiling(),
    {ok, _} = eprof:profile(fun() -> eprof_test:go(10) end),
    {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end),
    Pid     = whereis(eprof),
    {ok, _} = eprof:profile(erlang:processes() -- [Pid], fun() -> eprof_test:go(10) end),
    {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end, {eprof_test, '_', '_'}),
    {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end, {eprof_test, go, '_'}),
    {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end, {eprof_test, go, 1}),
    {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end, {eprof_test, dec, 1}),

    %% error case

    A         = spawn(fun() -> receive _ -> ok end end),
    profiling = eprof:profile([A]),
    true      = exit(A, kill_it),
    profiling_stopped = eprof:stop_profiling(),
    {error,_} = eprof:profile(fun() -> a = id(b) end),

    %% with mfa

    {ok, _} = eprof:profile([], eprof_test, go, [10]),
    {ok, _} = eprof:profile([], eprof_test, go, [10], {eprof_test, dec, 1}),

    %% dump

    {ok, _} = eprof:profile([], fun() -> eprof_test:go(10) end, {eprof_test, '_', '_'}),
    [{_, Mfas}] = eprof:dump(),
    Dec_mfa = {eprof_test, dec, 1},
    Go_mfa  = {eprof_test, go,  1},
    {value, {Go_mfa,  { 1, _Time1}}} = lists:keysearch(Go_mfa,  1, Mfas),
    {value, {Dec_mfa, {11, _Time2}}} = lists:keysearch(Dec_mfa, 1, Mfas),

    %% change current working directory

    ok = file:set_cwd(OldCurDir),
    stopped = eprof:stop(),
    ok.

basic_option(Config) when is_list(Config) ->
    %% Eprof is not supported on native-compile code.
    case lists:module_info(native_addresses) of
	[] -> basic_option_1(Config);
	[_|_] -> {skip,"lists is native-compiled"}
    end.

basic_option_1(Config) ->

    %% load eprof_test and change directory

    {ok, OldCurDir} = file:get_cwd(),
    Datadir = proplists:get_value(data_dir, Config),
    Privdir = proplists:get_value(priv_dir, Config),
    {ok,eprof_test} = compile:file(filename:join(Datadir, "eprof_test"),
					       [trace,{outdir, Privdir}]),
    ok = file:set_cwd(Privdir),
    code:purge(eprof_test),
    {module,eprof_test} = code:load_file(eprof_test),

    % vanilla
    {ok, _} = eprof:profile(fun() -> eprof_test:do(10) end, [{set_on_spawn, true}]),

    Mfas1 = lists:foldl(fun({_,Mfas},Out) -> Mfas ++ Out end, [], eprof:dump()),

    {value, {_, {11, _}}} = lists:keysearch({eprof_test,dec,1},  1, Mfas1),
    {value, {_, { 1, _}}} = lists:keysearch({eprof_test, go,1},  1, Mfas1),
    {value, {_, { 9, _}}} = lists:keysearch({lists, split_2,5},  1, Mfas1),
    {value, {_, { 4, _}}} = lists:keysearch({lists, seq_loop,3}, 1, Mfas1),

    {ok, _} = eprof:profile(fun() -> eprof_test:do(10) end, [set_on_spawn]),

    Mfas2 = lists:foldl(fun({_,Mfas},Out) -> Mfas ++ Out end, [], eprof:dump()),
    {value, {_, {11, _}}} = lists:keysearch({eprof_test,dec,1},  1, Mfas2),
    {value, {_, { 1, _}}} = lists:keysearch({eprof_test, go,1},  1, Mfas2),
    {value, {_, { 9, _}}} = lists:keysearch({lists, split_2,5},  1, Mfas2),
    {value, {_, { 4, _}}} = lists:keysearch({lists, seq_loop,3}, 1, Mfas2),

    % disable trace set_on_spawn
    {ok, _} = eprof:profile(fun() -> eprof_test:do(10) end, []),
    [{_, Mfas3}] = eprof:dump(),
    {value, {_, {11, _}}} = lists:keysearch({eprof_test,dec,1}, 1, Mfas3),
    {value, {_, { 1, _}}} = lists:keysearch({eprof_test, go,1}, 1, Mfas3),
    false = lists:keysearch({lists, split_2,5},  1, Mfas3),
    false = lists:keysearch({lists, seq_loop,3}, 1, Mfas3),

    %% change current working directory
    ok = file:set_cwd(OldCurDir),
    stopped = eprof:stop(),
    ok.

tiny(Config) when is_list(Config) -> 
    ensure_eprof_stopped(),
    {ok, OldCurDir} = file:get_cwd(),
    Datadir = proplists:get_value(data_dir, Config),
    Privdir = proplists:get_value(priv_dir, Config),
    % (Trace)Compile to priv_dir and make sure the correct version is loaded.
    {ok,eprof_suite_test} = compile:file(filename:join(Datadir,
							     "eprof_suite_test"),
					       [trace,{outdir, Privdir}]),
    ok = file:set_cwd(Privdir),
    code:purge(eprof_suite_test),
    {module,eprof_suite_test} = code:load_file(eprof_suite_test),
    {ok,_Pid} = eprof:start(),
    nothing_to_analyze = eprof:analyze(),
    nothing_to_analyze = eprof:analyze(total),
    eprof:profile([], eprof_suite_test, test, [Config]),
    ok = eprof:analyze(),
    ok = eprof:analyze(total),
    ok = eprof:log("eprof_SUITE_logfile"),
    stopped = eprof:stop(),
    ok = file:set_cwd(OldCurDir),
    ok.

eed(Config) when is_list(Config) ->
    ensure_eprof_stopped(),
    Datadir = proplists:get_value(data_dir, Config),
    Privdir = proplists:get_value(priv_dir, Config),
    ct:timetrap({minutes, 5}),

    %% (Trace)Compile to priv_dir and make sure the correct version is loaded.
    code:purge(eed),
    {ok,eed} = c:c(filename:join(Datadir, "eed"), [trace,{outdir,Privdir}]),
    {ok,_Pid} = eprof:start(),
    Script = filename:join(Datadir, "ed.script"),
    ok = file:set_cwd(Datadir),
    {T1,_} = statistics(runtime),
    ok = eed:file(Script),
    ok = eed:file(Script),
    ok = eed:file(Script),
    ok = eed:file(Script),
    ok = eed:file(Script),
    ok = eed:file(Script),
    ok = eed:file(Script),
    ok = eed:file(Script),
    ok = eed:file(Script),
    ok = eed:file(Script),
    {T2,_} = statistics(runtime),
    {ok,ok} = eprof:profile([], eed, file, [Script]),
    {T3,_} = statistics(runtime),
    profiling_already_stopped = eprof:stop_profiling(),
    ok = eprof:analyze(),
    ok = eprof:analyze(total),
    ok = eprof:log("eprof_SUITE_logfile"),
    stopped = eprof:stop(),
    try
	S = lists:flatten(io_lib:format("~p times slower",
					[10*(T3-T2)/(T2-T1)])),
	{comment,S}
    catch
	error:badarith ->
	    {comment,"No time elapsed. Bad clock? Fast computer?"}
    end.

ensure_eprof_stopped() ->
    Pid = whereis(eprof),
    case whereis(eprof) of
	undefined ->
	    ok;
	Pid ->
	    stopped=eprof:stop()
    end.

id(I) -> I.
