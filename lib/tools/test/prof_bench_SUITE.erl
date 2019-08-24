%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2017. All Rights Reserved.
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
    [{timetrap,{minutes,10}}].

all() ->
    [overhead].

init_per_suite(Config) ->
    case {test_server:is_native(fprof_SUITE) or
          (lists:any(fun(M) -> test_server:is_native(M) end, modules())) or
          (whereis(cover_server) =/= undefined),
          erlang:system_info(wordsize)}
    of
        {true, _} -> {skip, "Native or cover code"};
        {_, 4} -> {skip, "Can't run on 32-bit as files will be large"};
        {false, 8} -> Config
    end.

end_per_suite(Config) ->
    LogFile = filename:join(proplists:get_value(priv_dir, Config), "fprof.trace"),
    file:delete(LogFile),
    ok.

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

%% overhead(Config) ->
%%     LogFile = filename:join(proplists:get_value(priv_dir, Config), "fprof.trace"),
%%     SofsCopy = filename:join(proplists:get_value(data_dir, Config), "sofs_copy.erl"),
%%     TC = fun() -> compile:file(SofsCopy, [binary]) end,
%%     _Warmup = timer:tc(TC),

%%     [{ok,{EProfTime,{ok,sofs_copy,_}}} = eprof:profile([], timer, tc, [TC])
%%      || _ <- lists:seq(1,10)],
%% %%    [fprof:apply(timer, tc, [TC], [{file, LogFile}]) || _ <- lists:seq(1,10)],
%%     {FProfTime,{ok,sofs_copy,_}} = fprof:apply(timer, tc, [TC], [{file, LogFile}]),
%%     {NormTime,{ok, sofs_copy, _}} = timer:tc(TC),

    %% ct:pal("FProf: ~p Norm: ~p Ratio: ~p",[FProfTime, NormTime, FProfTime / NormTime]).

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
