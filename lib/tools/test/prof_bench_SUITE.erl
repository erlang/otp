%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2022. All Rights Reserved.
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
-module(prof_bench_SUITE).

-include_lib("common_test/include/ct_event.hrl").

%% Test server framework exports
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

-export([overhead/1]).

%%%---------------------------------------------------------------------
%%% Test suites
%%%---------------------------------------------------------------------


suite() ->
    [{timetrap,{minutes,30}}].

all() ->
    [overhead].

init_per_suite(Config) ->
    case whereis(cover_server) of
        undefined ->
            application:ensure_all_started(os_mon),
            Free = disk_free(proplists:get_value(priv_dir, Config)),
            if
                Free >= 16_000_000 ->
                    %% The size of the log files is about 4 Gb.
                    %% The disk has at least 4 times that amount free.
                    Config;
                true ->
                    %% There might not be sufficient disk space free.
                    io:format("Free disk space: ~p Kb\n", [Free]),
                    {skip, "Insufficient free disk space"}
            end;
        Pid when is_pid(Pid) ->
            {skip, "Cover is running"}
    end.

end_per_suite(Config) ->
    LogFile = filename:join(proplists:get_value(priv_dir, Config), "fprof.trace"),
    file:delete(LogFile),
    ok.

%% Return amount disk space free in Kbs for the disk that Path
%% is located on.
disk_free(Path) ->
    Data = disksup:get_disk_data(),

    %% What partitions could Data be mounted on?
    Partitions =
        [D || {P, _Tot, _Perc}=D <- Data,
         lists:prefix(filename:nativename(P), filename:nativename(Path))],

    %% Sorting in descending order places the partition with the most specific
    %% path first.
    case lists:sort(fun erlang:'>='/2, Partitions) of
        [{_,Tot, Perc} | _] -> round(Tot * (1-(Perc/100)));
        [] -> error
    end.

%%%---------------------------------------------------------------------

%% ct:run_test([{suite, prof_bench_SUITE}]).
overhead(Config) ->
    LogFile = filename:join(proplists:get_value(priv_dir, Config), "fprof.trace"),
    SofsCopy = filename:join(proplists:get_value(data_dir, Config), "sofs_copy.erl"),
    TC = fun() -> compile:file(SofsCopy, [binary]) end,
    _Warmup = timer:tc(TC),

    {NormTime,{ok, sofs_copy, _}} = timer:tc(TC),
    {FProfTime,{ok,sofs_copy,_}} = fprof:apply(timer, tc, [TC], [{file, LogFile}]),
    ct:pal("FProf: ~p Norm: ~p Ratio: ~p",[FProfTime, NormTime, NormTime / FProfTime * 100]),
    {ok,{EProfTime,{ok,sofs_copy,_}}} = eprof:profile([], timer, tc, [TC]),
    ct:pal("EProf: ~p Norm: ~p Ratio: ~p",[EProfTime, NormTime, NormTime / EProfTime * 100]),
    {CProfTime,{ok,sofs_copy,_}} = cprof_apply(timer, tc, [TC]),
    ct:pal("CProf: ~p Norm: ~p Ratio: ~p",[CProfTime, NormTime, NormTime / CProfTime * 100]),
    {CoverTime,{ok,sofs_copy,_}} = cover_apply(timer, tc, [TC]),
    ct:pal("Cover: ~p Norm: ~p Ratio: ~p",[CoverTime, NormTime, NormTime / CoverTime * 100]),

    ct_event:notify(#event{name = benchmark_data,
                           data = [{name, fprof_overhead},
                                   {value, NormTime / FProfTime * 100}]}),
    ct_event:notify(#event{name = benchmark_data,
                           data = [{name, eprof_overhead},
                                   {value, NormTime / EProfTime * 100}]}),
    ct_event:notify(#event{name = benchmark_data,
                           data = [{name, cprof_overhead},
                                   {value, NormTime / CProfTime * 100}]}),
    ct_event:notify(#event{name = benchmark_data,
                           data = [{name, cover_overhead},
                                   {value, NormTime / CoverTime * 100}]}).

cprof_apply(M, F, A) ->
    cprof:start(),
    Res = apply(M, F, A),
    cprof:stop(),
    Res.

cover_apply(M, F, A) ->
    cover:start(),
    catch cover:local_only(),
    Modules = modules(),
    [code:unstick_mod(Mod) || Mod <- Modules],
    cover:compile_beam(Modules),
    [code:stick_mod(Mod) || Mod <- Modules],
    Res = apply(M, F, A),
    cover:stop(),
    Res.

modules() ->
    application:load(compiler),
    {ok, CompilerModules} = application:get_key(compiler, modules),
    %% Only cover compile a subset of the stdlib modules
    StdlibModules = [erl_parse, erl_expand_records, erl_lint, gb_trees, gb_sets, sofs,
                     beam_lib, dict, epp, erl_anno, erl_bits,
                     orddict, ordsets, sets, string, unicode, unicode_util],
    CompilerModules ++ StdlibModules.
