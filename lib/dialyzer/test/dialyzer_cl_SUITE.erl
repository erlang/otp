%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2021 Facebook, Inc. and its affiliates.
%% Copyright Ericsson AB 2023-2026. All Rights Reserved.
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
-module(dialyzer_cl_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("dialyzer/src/dialyzer.hrl").

%% Test server specific exports
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

%% Test cases must be exported.
-export([
    can_add_multiple_plts_to_another_plt/1,
    unknown_function_warning_includes_callsite/1,
    call_to_missing_warning_includes_callsite/1,
    bad_pa_dir_returns_error/1
]).

suite() -> [{timetrap, {minutes, 3}}].

all() ->
    [
        can_add_multiple_plts_to_another_plt,
        unknown_function_warning_includes_callsite,
        call_to_missing_warning_includes_callsite,
        bad_pa_dir_returns_error
    ].

init_per_suite(Config) ->
    %% Prime PLT with common stuff so we don't get errors about get_module_info
    %% being unknown, etc.
    PrivDir = proplists:get_value(priv_dir,Config),
    PltBase = plt_base_file(PrivDir),
    _ = dialyzer:run([{analysis_type, plt_build},
                      {apps, [erts]},
                      {output_plt, PltBase}]),
    Config.

end_per_suite(Config) ->
    Config.

plt_base_file(PrivDir) ->
    filename:join(PrivDir, "dialyzer_cl_base.plt").

%%%
%%% Test cases starts here.
%%%

%% Test running Dialyzer programatically can yield call to missing
%% errors with information on both the callsite and the unknown call
%% itself.
%% Missing function logic is tested elsewhere, but here we're specifically
%% interested in the details of the error that are accessible from the
%% dialyzer_cl API
call_to_missing_warning_includes_callsite(Config) when is_list(Config) ->

    PrivDir = proplists:get_value(priv_dir,Config),
    PltBase = plt_base_file(PrivDir),
    Plt = filename:join(PrivDir, "previously_defined.plt"),

    {ok, BeamFileForPlt} = compile(Config, previously_defined, []),
    [] = dialyzer:run([{analysis_type, plt_build},
                      {files, [BeamFileForPlt]},
                      {output_plt, Plt},
                       {warnings, [no_unknown]}]),

    {ok, Beam} = compile(Config, call_to_missing_example, []),
    Opts =
        #options{
            analysis_type = succ_typings,
            init_plts = [Plt, PltBase],
            output_file = none,
            get_warnings = true,
            legal_warnings = ordsets:from_list([warn_unknown, warn_callgraph]),
            erlang_mode = true,
            files = [Beam]
        },
    Res = dialyzer_cl:start(Opts),

    ?assertMatch(
        {2, [
            {warn_callgraph, {_Filename, {5, 5}}, {
                call_to_missing, [
                    previously_defined, function, 0
                ]
            }}
        ]},
        Res),

    ok.

%% Test running Dialyzer programatically can yield unknown function
%% errors with information on both the callsite and the unknown call
%% itself.
%% Missing function logic is tested elsewhere, but here we're specifically
%% interested in the details of the error that are accessible from the
%% dialyzer_cl API
unknown_function_warning_includes_callsite(Config) when is_list(Config) ->

    PrivDir = proplists:get_value(priv_dir,Config),
    PltBase = plt_base_file(PrivDir),

    {ok, Beam} = compile(Config, unknown_function_example, []),
    Opts =
        #options{
            analysis_type = succ_typings,
            init_plts = [PltBase],
            output_file = none,
            get_warnings = true,
            legal_warnings = ordsets:from_list([warn_unknown, warn_callgraph]),
            erlang_mode = true,
            files = [Beam]
        },
    Res = dialyzer_cl:start(Opts),

    ?assertMatch(
        {2, [
            {warn_unknown, {_Filename, {5,5}},
                {unknown_function, {
                    does_not_exist, function, 0
                }}
            }
        ]},
        Res),

    ok.

% See GitHub issue erlang/OTP #6850
can_add_multiple_plts_to_another_plt(Config) when is_list(Config) ->

    PrivDir = proplists:get_value(priv_dir,Config),

    StdlibPlt = filename:join(PrivDir, "stdlib.plt"),
    ErtsPlt = filename:join(PrivDir, "erts.plt"),
    OutputPlt = filename:join(PrivDir, "merged.plt"),

    _ = dialyzer:run([{analysis_type, plt_build},
                      {apps, [stdlib]},
                      {output_plt, StdlibPlt}]),
    _ = dialyzer:run([{analysis_type, plt_build},
                      {apps, [erts]},
                      {output_plt, ErtsPlt}]),
    ?assertEqual(
       [],
       dialyzer:run([{analysis_type, plt_add},
                     {apps, [erts, stdlib]},
                     {plts, [ErtsPlt, StdlibPlt]},
                     {output_plt, OutputPlt}])),

    ok.

%% Regression test: a throw raised in the `of` body of the try/of in
%% dialyzer_cl_parse:start/1 used to escape the surrounding catch,
%% because a try/of only runs its catch clauses over the guarded
%% expression, not over the `of` body. A non-existent -pa directory
%% makes postprocess_side_effects/1 call cl_error/1, which throws
%% {dialyzer_cl_parse_error, _} from the `of` body. Before the fix this
%% escaped as {nocatch, ...} and crashed the CLI with a non-standard
%% exit status instead of the documented graceful error.
%%
%% start/1 takes the argument list directly, so we can drive the exact
%% parsing path without spawning a node, and assert that it returns
%% {error, Msg} (which the CLI turns into a clean "dialyzer: <msg>"
%% diagnostic) rather than raising.
bad_pa_dir_returns_error(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    BadDir = filename:join(PrivDir, "this_dir_does_not_exist"),
    %% Sanity: the directory really must not exist for the -pa side
    %% effect to fail with {error, bad_directory}.
    false = filelib:is_dir(BadDir),

    Res = dialyzer_cl_parse:start(["-pa", BadDir, "some_file.beam"]),

    %% Before the fix this call raised {dialyzer_cl_parse_error, _};
    %% now it must return a clean error tuple.
    ?assertMatch({error, _}, Res),
    {error, Msg} = Res,
    ?assert(string:find(Msg, "Bad directory for -pa") =/= nomatch),
    ok.

compile(Config, Module, CompileOpts) ->
    Source = lists:concat([Module, ".erl"]),
    PrivDir = proplists:get_value(priv_dir,Config),
    DataDir = proplists:get_value(data_dir,Config),
    SrcFilename = filename:join([DataDir, Source]),
    Opts = [{outdir, PrivDir}, debug_info | CompileOpts],
    {ok, Module} = compile:file(SrcFilename, Opts),
    {ok, filename:join([PrivDir, lists:concat([Module, ".beam"])])}.
