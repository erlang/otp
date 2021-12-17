-module(dialyzer_cl_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("dialyzer/src/dialyzer.hrl").

%% Test server specific exports
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

%% Test cases must be exported.
-export([
    unknown_function_warning_includes_callsite/1,
    call_to_missing_warning_includes_callsite/1
]).

suite() -> [{timetrap, {minutes, 1}}].

all() ->
    [
        unknown_function_warning_includes_callsite,
        call_to_missing_warning_includes_callsite
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
                      {output_plt, Plt}]),

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

compile(Config, Module, CompileOpts) ->
    Source = lists:concat([Module, ".erl"]),
    PrivDir = proplists:get_value(priv_dir,Config),
    DataDir = proplists:get_value(data_dir,Config),
    SrcFilename = filename:join([DataDir, Source]),
    Opts = [{outdir, PrivDir}, debug_info | CompileOpts],
    {ok, Module} = compile:file(SrcFilename, Opts),
    {ok, filename:join([PrivDir, lists:concat([Module, ".beam"])])}.
