%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023-2024. All Rights Reserved.
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
-module(code_coverage_SUITE).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([toggle_modes/1,
         get_coverage/1,
         error_info/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    case code:coverage_support() of
        true ->
            Config;
        false ->
            {skip, "This runtime system does not support coverage"}
    end.

end_per_suite(Config) ->
    Config.

all() ->
    [toggle_modes,get_coverage,error_info].

toggle_modes(_Config) ->
    none = code:get_coverage_mode(?MODULE),
    OldMode = code:get_coverage_mode(),
    try
        do_toggle_modes(OldMode)
    after
        code:set_coverage_mode(OldMode)
    end.

do_toggle_modes(CurrentMode) ->
    Modes = [none,line,line_counters,function,function_counters],
    Last = lists:last(Modes),
    Last = do_toggle_modes_1(CurrentMode, Modes),
    ok.

do_toggle_modes_1(Current, [Mode|Modes]) ->
    Current = code:set_coverage_mode(Mode),
    none = code:get_coverage_mode(?MODULE),
    do_toggle_modes_1(Mode, Modes);
do_toggle_modes_1(Current, []) ->
    Current.

get_coverage(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    OldMode = code:get_coverage_mode(),

    try
        do_get_coverage(PrivDir)
    after
        code:set_coverage_mode(OldMode)
    end.

do_get_coverage(PrivDir) ->
    M = get_coverage_test,
    S = ~"""
        -module(get_coverage_test).
        -export([fact/1,fib/1]).

        fact(N) when is_integer(N), N >= 0 ->   %4
            fact(N, 1).                         %5

        fact(0, P) ->
            P;                                  %8
        fact(N, P) ->
            fact(N - 1, P * N).                 %10

        fib(N) ->
            fib(N, 0, 1).                       %13

        fib(0, _, B) ->
            B;                                  %16
        fib(N, A, B) ->
            fib(N - 1, B, A + B).               %18
        """,

    ErlFile = filename:join(PrivDir, atom_to_list(M) ++ ".erl"),
    ok = file:write_file(ErlFile, S),
    {ok,M,Beam} = compile:file(ErlFile, [report,binary,line_coverage]),

    Run1 = fun() -> ok end,
    Result1 = {[{{fact,1},0}, {{fact,2},0}, {{fib,1},0}, {{fib,3},0}],
               [{5,0},{8,0},{10,0},{13,0},{16,0},{18,0}]},
    do_get_coverage(M, Beam, Run1, Result1),

    Run2 = fun() -> M:fib(5) end,
    Result2 = {[{{fact,1},0}, {{fact,2},0}, {{fib,1},1}, {{fib,3},6}],
               [{5,0},{8,0},{10,0},{13,1},{16,1},{18,5}]},
    do_get_coverage(M, Beam, Run2, Result2),

    %% Test cover_id_line used by cover.
    _ = code:set_coverage_mode(line_counters),
    {module,M} = code:load_binary(M, "", Beam),
    line_counters = code:set_coverage_mode(none),
    _ = M:fib(5),
    [{1,0},{2,0},{3,0},{4,1},{5,1},{6,5}] =
        code:get_coverage(cover_id_line, M),
    unload(M),

    %% Compile without line_coverage.
    {ok,M,BeamFun} = compile:file(ErlFile, [report,binary]),
    do_get_function_coverage(M, BeamFun, Run1, Result1),
    do_ensure_no_line_coverage(M, BeamFun),

    none = code:get_coverage_mode(?MODULE),

    ok.

do_get_coverage(M, Beam, RunFun, Result) ->
    {FunctionResult,LineCoverage} = Result,
    FunctionResultBool = [{F,N =/= 0} || {F,N} <- FunctionResult],

    %% Test function coverage.

    do_get_function_coverage(M, Beam, RunFun, Result),

    %% Test line.

    _ = code:set_coverage_mode(line),
    {module,M} = code:load_binary(M, "", Beam),
    _ = code:set_coverage_mode(none),
    RunFun(),

    line = code:get_coverage_mode(M),

    LineCoverageBool = [{F,N =/= 0} || {F,N} <- LineCoverage],
    FunctionResultBool = code:get_coverage(function, M),
    LineCoverageBool = code:get_coverage(line, M),

    LineCoverageBoolReset = [{F,false} || {F,_} <- LineCoverage],
    code:reset_coverage(M),
    LineCoverageBoolReset = code:get_coverage(line, M),

    unload(M),

    %% Test line_counters.

    _ = code:set_coverage_mode(line_counters),
    {module,M} = code:load_binary(M, "", Beam),
    _ = code:set_coverage_mode(none),
    RunFun(),

    line_counters = code:get_coverage_mode(M),

    FunctionResultBool = code:get_coverage(function, M),
    LineCoverage = code:get_coverage(line, M),

    LineCoverageZero = [{F,0} || {F,_} <- LineCoverage],
    code:reset_coverage(M),
    LineCoverageZero = code:get_coverage(line, M),

    unload(M),

    {'EXIT',{badarg,_}} = catch code:get_coverage(function, M),
    {'EXIT',{badarg,_}} = catch code:get_coverage(line, M),
    {'EXIT',{badarg,_}} = catch code:get_coverage_mode(M),

    ok.

do_get_function_coverage(M, Beam, RunFun, Result) ->
    {FunctionResult,_LineCoverage} = Result,
    FunctionResultBool = [{F,N =/= 0} || {F,N} <- FunctionResult],
    FunctionResultBoolReset = [{F,false} || {F,_} <- FunctionResult],
    FunctionResultBoolZero = [{F,0} || {F,_} <- FunctionResult],

    %% Test function mode.

    _ = code:set_coverage_mode(function),
    {module,M} = code:load_binary(M, "", Beam),
    _ = code:set_coverage_mode(none),
    RunFun(),

    function = code:get_coverage_mode(M),

    FunctionResultBool = code:get_coverage(function, M),
    {'EXIT',{badarg,_}} = catch code:get_coverage(line, M),

    code:reset_coverage(M),
    FunctionResultBoolReset = code:get_coverage(function, M),

    unload(M),

    {'EXIT',{badarg,_}} = catch code:get_coverage_mode(M),
    {'EXIT',{badarg,_}} = catch code:get_coverage(function, M),
    {'EXIT',{badarg,_}} = catch code:get_coverage(line, M),

    %% Test function_counters mode.

    _ = code:set_coverage_mode(function_counters),
    {module,M} = code:load_binary(M, "", Beam),
    _ = code:set_coverage_mode(none),
    RunFun(),

    function_counters = code:get_coverage_mode(M),

    FunctionResult = code:get_coverage(function, M),
    {'EXIT',{badarg,_}} = catch code:get_coverage(line, M),

    code:reset_coverage(M),
    FunctionResultBoolZero = code:get_coverage(function, M),

    unload(M),

    {'EXIT',{badarg,_}} = catch code:get_coverage_mode(M),
    {'EXIT',{badarg,_}} = catch code:get_coverage(function, M),
    {'EXIT',{badarg,_}} = catch code:get_coverage(line, M),

    ok.

do_ensure_no_line_coverage(M, Beam) ->
    %% Test line mode.
    _ = code:set_coverage_mode(line),
    {module,M} = code:load_binary(M, "", Beam),
    _ = code:set_coverage_mode(none),
    none = code:get_coverage_mode(M),
    {'EXIT',{badarg,_}} = catch code:get_coverage(function, M),
    {'EXIT',{badarg,_}} = catch code:get_coverage(line, M),

    unload(M),

    %% Test line counters mode.
    _ = code:set_coverage_mode(line_counters),
    {module,M} = code:load_binary(M, "", Beam),
    _ = code:set_coverage_mode(none),
    none = code:get_coverage_mode(M),
    {'EXIT',{badarg,_}} = catch code:get_coverage(function, M),
    {'EXIT',{badarg,_}} = catch code:get_coverage(line, M),

    unload(M),

    ok.

unload(M) ->
    true = code:delete(M),
    _ = code:purge(M),
    ok.

error_info(_Config) ->
    %% An atom referring that does not refer to a loaded module.
    NotLoaded = not__a__loaded__module__I__hope,

    L = [{get_coverage_mode, [42]},
         {get_coverage_mode, [NotLoaded]},

         {get_coverage, [line,42]},
         {get_coverage, [line,NotLoaded]},
         {get_coverage, [line,?MODULE]},
         {get_coverage, [whatever,?MODULE]},

         {reset_coverage, [42]},
         {reset_coverage, [NotLoaded]},
         {reset_coverage, [?MODULE]},

         {set_coverage_mode, [42]},
         {set_coverage_mode, [xyz]}],

    error_info_lib:test_error_info(code, L, [snifs_only]).
