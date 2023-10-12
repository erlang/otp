%% This suite tests the incremental mode which for now woirks as follows:
%% You can specify which files you want in your plt, incremental mode will
%% do the analysis needed to get the PLT into a good state and report the
%% warnings from the modules which it re-analyzed.

-module(incremental_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0,
         all/0,
         report_new_plt_test/1,
         report_degree_of_incrementality_test/1,
         report_no_stored_warnings_test/1,
         report_stored_warnings_no_files_changed_test/1,
         report_stored_warnings_only_files_safely_removed_test/1,
         report_old_plt_version_number_test/1,
         report_old_plt_version_hash_test/1,
         report_legal_warnings_added/1,
         report_legal_warnings_removed/1,
         incremental_test/1,
         incremental_select_warnings_test/1,
         add_and_remove_test/1,
         add_and_remove_test_with_unknown_warnings/1,
         verify_plt_info/1,
         verify_plt_info_with_unknown_warnings/1,
         fixing_all_warnings/1,
         default_apps_config_xdg/1,
         default_apps_config_env_var/1,
         default_apps_config_env_var_prioritised_over_xdg/1,
         legal_warnings_config_xdg/1,
         paths_config_xdg/1,
         multiple_plts_unsupported_in_incremental_mode/1]).

suite() ->
  [{timetrap, ?plt_timeout}].

all() -> [report_new_plt_test,
          report_degree_of_incrementality_test,
          report_no_stored_warnings_test,
          report_stored_warnings_no_files_changed_test,
          report_stored_warnings_only_files_safely_removed_test,
          report_old_plt_version_number_test,
          report_old_plt_version_hash_test,
          report_legal_warnings_added,
          report_legal_warnings_removed,
          incremental_test,
          incremental_select_warnings_test,
          add_and_remove_test,
          add_and_remove_test_with_unknown_warnings,
          verify_plt_info,
          verify_plt_info_with_unknown_warnings,
          fixing_all_warnings,
          default_apps_config_xdg,
          default_apps_config_env_var,
          default_apps_config_env_var_prioritised_over_xdg,
          legal_warnings_config_xdg,
          paths_config_xdg,
          multiple_plts_unsupported_in_incremental_mode].

erlang_module() ->
   case code:where_is_file("erlang.beam") of
       non_existing ->
           filename:join([code:root_dir(),
                         "erts", "preloaded", "ebin",
                         "erlang.beam"]);
      EBeam ->
          EBeam
   end.

verify_plt_info_with_unknown_warnings(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" ||
                                 Module <- Modules] end,
    Plt = filename:join(PrivDir, "verify_plt_info.iplt"),
    compile_all(DataDir, PrivDir),
    ErlangModule = erlang_module(),
    Opts = [{init_plt, [Plt]}, {warnings, [unknown]}],
    AllSubSets = all_subsets(all_mods()),
    Run = fun(Mods) -> run_dialyzer(incremental, [ErlangModule | ToPath(Mods)], Opts) end,
    [run_and_verify_plt_info_with_unknown_warnings(Subset, Run, Plt) ||
        Subset <- AllSubSets].

run_and_verify_plt_info_with_unknown_warnings(Subset, Run, PltFile) ->
    _ = Run(Subset),
    {Plt, {iplt_info, _Md5, _ModDeps, Warnings, _LegalWarnings}} =
        dialyzer_iplt:plt_and_info_from_file(PltFile),
    dialyzer_plt:delete(Plt),
    verify_unknown_warnings(Subset, Warnings).

verify_unknown_warnings(Subset, Warnings) ->
    Unused = all_mods() -- Subset,
    lists:foreach(fun(Mod) -> check_unknown_warnings(Mod, Unused, Warnings) end, Subset),
    lists:foreach(fun(Mod) -> check_unused_mod_warnings(Mod, Warnings) end, Unused).

check_unknown_warnings(Mod, Unused, Warnings) ->
    Unknown = [unknown_function_warning(M) ||
                  M <- Unused, lists:member(Mod, depends_on(M))],
    Other = [warning_for_module(Mod)],
    Expected = lists:sort(Unknown ++ Other),
    Stored = lists:sort(replace_location(maps:get(Mod, Warnings))),
    Expected = Stored.

unknown_function_warning(M) ->
    {warn_unknown, loc, {unknown_function, {M, id, 1}}}.

verify_plt_info(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" ||
                                 Module <- Modules] end,
    Plt = filename:join(PrivDir, "verify_plt_info.iplt"),
    compile_all(DataDir, PrivDir),
    ErlangModule = erlang_module(),
    Opts = [{init_plt, [Plt]}],
    AllSubSets = all_subsets(all_mods()),
    Run = fun(Mods) -> run_dialyzer(incremental, [ErlangModule | ToPath(Mods)], Opts) end,
    [run_and_verify_plt_info(Subset, Run, Plt) || Subset <- AllSubSets].

run_and_verify_plt_info(Subset, Run, PltFile) ->
    _ = Run(Subset),
    {Plt, {iplt_info, Md5, ModDeps, Warnings, _LegalWarnings}} =
        dialyzer_iplt:plt_and_info_from_file(PltFile),
    dialyzer_plt:delete(Plt),
    verify_md5_list([erlang|Subset], Md5),
    verify_mod_deps(Subset, ModDeps),
    verify_warnings(Subset, Warnings).

verify_md5_list(AllModules, Md5) ->
    ExpectedModules = lists:sort(AllModules),
    ActualModules = lists:sort([ModuleName || {ModuleName, _CheckSum} <- Md5]),
    case ExpectedModules =:= ActualModules of
        true ->
            ok;
        false ->
            ct:pal("Expected modules: ~p~nActual Modules:~p~n",
                   [ExpectedModules, ActualModules]),
            ExpectedModules = ActualModules
    end.

verify_mod_deps(Subset, ModDeps) ->
    Unused = all_mods() -- Subset,
    DependsOn = fun(Mod) -> depends_on(Mod) -- Unused end,
    lists:foreach(fun(Mod) -> check_one_mod(Mod, ModDeps, DependsOn) end, Subset).

check_one_mod(Mod, ModDeps, DependsOn) ->
    StoredDeps = lists:sort(rewrite_ans(dict:find(Mod, ModDeps))),
    ExpectedDeps = lists:sort(DependsOn(Mod)),
    case ExpectedDeps =:= StoredDeps of
        true ->
            ok;
        false ->
            ct:pal("Expected deps: ~p~nStored deps:~p~n",
                        [ExpectedDeps, StoredDeps]),
            ExpectedDeps = StoredDeps
    end.

rewrite_ans({ok, Value}) -> Value;
rewrite_ans(error) -> [].

verify_warnings(Subset, Warnings) ->
    Unused = all_mods() -- Subset,
    lists:foreach(fun(Mod) -> check_warnings(Mod, Warnings) end, Subset),
    lists:foreach(fun(Mod) -> check_unused_mod_warnings(Mod, Warnings) end, Unused).

check_warnings(Mod, Warnings) ->
    StoredWarnings = lists:sort(replace_location(maps:get(Mod, Warnings))),
    ExpectedWarnings = [warning_for_module(Mod)],
    ExpectedWarnings = StoredWarnings.

check_unused_mod_warnings(Mod, Warnings) ->
    false = maps:is_key(Mod, Warnings).

add_and_remove_test_with_unknown_warnings(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" ||
                                 Module <- Modules] end,
    Plt = filename:join(PrivDir, "add_and_remove.iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [Plt]}, {warnings, [unknown]}, {report_mode, verbose}],
    Run = fun(Mods) ->
            run_dialyzer_for_modules_analyzed(incremental,
                         [erlang_module() | ToPath(Mods)],
                         [{warning_files, ToPath(Mods)} | Opts])
          end,
    %% We ignore empty Mods, since no warning modules modules is
    %% interpreted as a request to show all warning modules (for
    %% convenience). However, if all module warnings are shown and
    %% 'unknown' warnings are enabled, we get a load of 'unknown'
    %% warnings from the built-in Erlang module, so we exclude that
    %% case here
    AllSubSets = [Mods || Mods <- all_subsets(all_mods()), Mods =/= []],
    %Pairs = [{I, N} || I <- AllSubSets, N <- AllSubSets], % All pairs takes ~ 10 mins to run
    Pairs = some_pairs(AllSubSets),
    [run_one_add_and_remove_test(Initial, New, Run, true) || {Initial, New} <- Pairs],
    ok.

%% Run the tests to add and remove modules for some combinations
add_and_remove_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" || Module <- Modules] end,
    Plt = filename:join(PrivDir, "add_and_remove.iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [Plt]}, {report_mode, verbose}, {warnings, [no_unknown]}],
    Run = fun(Mods) -> run_dialyzer_for_modules_analyzed(incremental, [erlang_module() | ToPath(Mods)], Opts) end,
    AllSubSets = [Mods || Mods <- all_subsets(all_mods())],
    %Pairs = [{I, N} || I <- AllSubSets, N <- AllSubSets], % All pairs takes ~ 10 mins to run
    Pairs = some_pairs(AllSubSets),
    [run_one_add_and_remove_test(Initial, New, Run, false) || {Initial, New} <- Pairs],
    ok.


%% Runs a test where we first run the analysis on the InitialSet, and
%% then run the analysis on the newset, checking that we get the
%% warnings we expect given this change of modules. Supports running
%% with unknown function and types and also with getting warnings for
%% all modules, not just the re-analyzed ones.
run_one_add_and_remove_test(InitialSet, NewSet, Run, WithUnknown) ->
    {ExpectedErrors, ExpectedAnalyzed} =
        expected_warnings_and_analyzed_add_and_remove(InitialSet, NewSet, WithUnknown),
    _ = Run(InitialSet),
    {ErrorsRaw, Analyzed} = Run(NewSet),
    Errors = ordsets:from_list(replace_location(ErrorsRaw)),

    if Errors =/= ExpectedErrors ->
            ct:pal("Initial modules: ~p~nNew Modules:~p~nModules analyzed:~p~nModules expected to be analyzed:~p~n"
                   "Returned Warnings:~p~nExpected Warnings:~p~n",
                   [InitialSet, NewSet, Analyzed, ExpectedAnalyzed, Errors, ExpectedErrors]),
            {Errors, Analyzed} = {ExpectedErrors, ExpectedAnalyzed};
        true ->
            ok
    end.

%% Just create some pairs.
some_pairs([A, B | Rest]) ->
    [{A, B} | some_pairs([B|Rest])];
some_pairs(_) -> [].


%% We expect incrementality to minimise the number of modules
%% re-analysed, but still report warnings for modules which didn't
%% need to be re-analysed because their warnings were cached.  We
%% expect a module to be re-analyzed when it is freshly added or when
%% one of its transistive dependencies have been added or removed,
%% using the dependency graph from the previous run.  Adds unknown
%% function warnings when they are supported, and generates all
%% warnings when that feature is turned on.
expected_warnings_and_analyzed_add_and_remove(Initial, New, WithUnknown) ->
    InitialSet = ordsets:from_list(Initial),
    NewSet = ordsets:from_list(New),
    Removed = ordsets:from_list(InitialSet -- NewSet),
    Added = ordsets:from_list(NewSet -- InitialSet),
    Changed = ordsets:union(Removed, Added),
    All = ordsets:from_list(all_mods()),
    Missing = ordsets:subtract(All, NewSet),
    DependsOn = fun(Mod) -> depends_on(Mod, fun(X) -> lists:member(X, InitialSet) end) end,
    Dependencies = ordsets:from_list([Mod || C <- Changed, Mod <- depends_on_transitively(C, DependsOn)]),
    Analyzed = ordsets:intersection(ordsets:union(Added, Dependencies), NewSet),
    WarnModules = NewSet,
    UnknownWarnings =
        [unknown_function_warning(M) ||
            WithUnknown,
            Mod <- WarnModules,
            M <- Missing,
            lists:member(Mod, depends_on(M))],
    {ordsets:from_list([warning_for_module(Mod) ||
                           Mod <- WarnModules] ++ UnknownWarnings), Analyzed}.

%% Run the incremental test for all combination of the 6 modules,
%% checking that we get the expected level of incrementality, i.e. we
%% only reanalyze what we need to given what has changed
incremental_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" ||
                                 Module <- Modules] end,
    Change = fun(M) -> change_module(DataDir, PrivDir, M) end,
    Plt = filename:join(PrivDir, "incremental.iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [Plt]}, {report_mode, verbose}, {warnings, [no_unknown]}],
    AllWarnings = run_dialyzer(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    Run = fun() ->
                  run_dialyzer_for_modules_analyzed(incremental,
                                                    [erlang_module() | ToPath(all_mods())],
                                                    Opts)
          end,
    [run_one_incremental_test(SubSet, Change, Run, AllWarnings) ||
        SubSet <- lists:reverse(all_subsets(all_mods()))],
    ok.

%% Run the incremental test for all combination of the 6 modules,
%% checking that with or without touching a file, we get the expected
%% warnings
incremental_select_warnings_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" ||
                                 Module <- Modules] end,
    Plt = filename:join(PrivDir, "select_warnings.iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [Plt]}],
    _ = run_dialyzer(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    RunWithoutChange =
        fun(ExtraOpt) ->
            run_dialyzer(incremental, [erlang_module() | ToPath(all_mods())], [ExtraOpt|Opts])
        end,
    RunWithChange =
        fun(ExtraOpt) ->
             change_module(DataDir, PrivDir, m4),
             run_dialyzer(incremental, [erlang_module() | ToPath(all_mods())], [ExtraOpt|Opts])
        end,
    TestSets = [Mods || Mods <- all_subsets(all_mods()), Mods =/= []],
    [run_one_select_warnings_test(SubSet, RunWithoutChange, ToPath) || SubSet <- TestSets],
    [run_one_select_warnings_test(SubSet, RunWithChange, ToPath) || SubSet <- TestSets].

run_one_select_warnings_test(WarningMods, Run, ToPath) ->
    ExpectedWarnings = lists:sort([warning_for_module(M) || M <- WarningMods]),
    ActualWarnings = lists:sort(replace_location(Run({warning_files, ToPath(WarningMods)}))),
    case ExpectedWarnings =/= ActualWarnings of
        true ->
            ct:pal("Warning modules: ~p~nReturned Warnings:~p~nExpected Warnings:~p~n",
                   [WarningMods, ActualWarnings, ExpectedWarnings]),
            ExpectedWarnings = ActualWarnings;
        false ->
            ok
    end.

%% Recompiles a set of modules and then runs the analysis verifying that we re-analyze
%% the changed modules and their transitive dependencies.
run_one_incremental_test(ChangedMods, Change, Run, ExpectedWarnings) ->
    lists:foreach(Change, ChangedMods),
    ExpectedModulesAnalyzed = expected_modules_reanalyzed_incremental(ChangedMods),
    {Warnings, ModulesAnalyzed} = Run(),
    if Warnings =/= ExpectedWarnings ->
            ct:pal("Changed modules: ~p~nModules reanalyzed:~p~nExpected modules reanalyzed:~p~nReturned Warnings:~p~nExpected Warnings:~p~n",
                   [ChangedMods, ModulesAnalyzed, ExpectedModulesAnalyzed, Warnings, ExpectedWarnings]),
            {Warnings, ModulesAnalyzed} = {ExpectedWarnings, ExpectedModulesAnalyzed};
        true ->
            ok
    end.

check_metrics_file_contents(MetricsFileName, ExpectedMetricsFileStr) ->
    {ok, MetricsFileBin} = file:read_file(MetricsFileName),
    MetricsFileStr = unicode:characters_to_list(MetricsFileBin),
    if MetricsFileStr =/= ExpectedMetricsFileStr ->
        ct:pal("Metrics file didn't contain expected contents. Expected:~n~p~nActual:~n~p~n",
                [ExpectedMetricsFileStr, MetricsFileStr]),
        ExpectedMetricsFileStr = MetricsFileStr;
      true ->
        ok
    end.

report_new_plt_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    MetricsFile = PrivDir ++ "my_incrementality_metrics.log",
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" || Module <- Modules] end,
    Plt = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [Plt]}, {report_mode, verbose}, {metrics_file, MetricsFile}],
    {_Warnings, Stdout} = run_dialyzer_capture(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    CandidateLines =
        lists:filter(
          fun (Line) ->
            case string:prefix(Line, "PLT does not yet exist at") of
                nomatch -> false;
                _ -> true
            end
          end,
          Stdout),
    ExpectedLine = "PLT does not yet exist at " ++ Plt ++ ", so an analysis must be run for 7 modules to populate it\n",
    if CandidateLines =/= [ExpectedLine] ->
        ct:pal("Failed to find expected log line in stdout.~nExpected:~n  ~p~nLines that look close:  ~n~p~n",
                [ExpectedLine, CandidateLines]),
        CandidateLines = [ExpectedLine];
      true ->
        ok
    end,
    ExpectedMetricsFileStr =
        "total_modules: 7\n"
        "analysed_modules: 7\n"
        "reason: new_plt_file\n",
    check_metrics_file_contents(MetricsFile, ExpectedMetricsFileStr).

report_degree_of_incrementality_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    MetricsFile = PrivDir ++ "my_incrementality_metrics.log",
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" || Module <- Modules] end,
    Plt = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [Plt]}, {report_mode, verbose}, {metrics_file, MetricsFile}],
    _WarningsBeforeTouchingFile = run_dialyzer(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    change_module(DataDir, PrivDir, m1),
    change_module(DataDir, PrivDir, m5),
    {_WarningsAfterTouchingFile, Stdout} = run_dialyzer_capture(incremental, [erlang_module() | ToPath(all_mods())], Opts),

    ExpectedLine =
        "    Of the 7 files being tracked, 2 have been changed or removed, "
        "resulting in 5 requiring analysis because they depend on those changes\n",
    case lists:any(fun (Line) -> Line =:= ExpectedLine end, Stdout) of
      false ->
        ct:pal("Failed to find expected log line in stdout.~nExpected:~n  ~p~nStdout:  ~n~p~n",
                [ExpectedLine, Stdout]),
        ct:fail("Dialyzer failed to print the expected line to stdout");
      true ->
        ok
    end,
    ExpectedMetricsFileStr =
        "total_modules: 7\n"
        "analysed_modules: 5\n"
        "reason: incremental_changes\n"
        "changed_or_removed_modules: 2\n",
    check_metrics_file_contents(MetricsFile, ExpectedMetricsFileStr).

report_stored_warnings_only_files_safely_removed_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    MetricsFile = PrivDir ++ "my_incrementality_metrics.log",
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" || Module <- Modules] end,
    Plt = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [Plt]}, {report_mode, verbose}, {metrics_file, MetricsFile}],
    _WarningsBeforeTouchingFile = run_dialyzer(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    delete_module_beam_file(PrivDir, m1),
    {_WarningsAfterTouchingFile, Stdout} = run_dialyzer_capture(incremental, [erlang_module() | ToPath(all_mods() -- [m1])], Opts),

    ExpectedLine =
        "PLT has fully cached the request because nothing depended on the file removed, so no additional analysis is needed\n",
    case lists:any(fun (Line) -> Line =:= ExpectedLine end, Stdout) of
      false ->
        ct:pal("Failed to find expected log line in stdout.~nExpected:~n  ~p~nStdout:  ~n~p~n",
                [ExpectedLine, Stdout]),
        ct:fail("Dialyzer failed to print the expected line to stdout");
      true ->
        ok
    end,
    ExpectedMetricsFileStr =
        "total_modules: 6\n"
        "analysed_modules: 0\n"
        "reason: incremental_changes\n"
        "changed_or_removed_modules: 1\n",
    check_metrics_file_contents(MetricsFile, ExpectedMetricsFileStr).

report_stored_warnings_no_files_changed_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    MetricsFile = PrivDir ++ "my_incrementality_metrics.log",
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" || Module <- Modules] end,
    Plt = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [Plt]}, {report_mode, verbose}, {metrics_file, MetricsFile}],
    _WarningsBeforeTouchingFile = run_dialyzer(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    {_WarningsAfterTouchingFile, Stdout} = run_dialyzer_capture(incremental, [erlang_module() | ToPath(all_mods())], Opts),

    ExpectedLine =
        "PLT has fully cached the request, so no additional analysis is needed\n",
    case lists:any(fun (Line) -> Line =:= ExpectedLine end, Stdout) of
      false ->
        ct:pal("Failed to find expected log line in stdout.~nExpected:~n  ~p~nStdout:  ~n~p~n",
                [ExpectedLine, Stdout]),
        ct:fail("Dialyzer failed to print the expected line to stdout");
      true ->
        ok
    end,
    ExpectedMetricsFileStr =
        "total_modules: 7\n"
        "analysed_modules: 0\n"
        "reason: incremental_changes\n"
        "changed_or_removed_modules: 0\n",
    check_metrics_file_contents(MetricsFile, ExpectedMetricsFileStr).

report_old_plt_version_number_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    MetricsFile = PrivDir ++ "my_incrementality_metrics.log",
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" || Module <- Modules] end,
    PltFileName = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [PltFileName]}, {report_mode, verbose}, {metrics_file, MetricsFile}],

    % Set up "old" PLT file
    Plt = dialyzer_plt:new(),
    ModDeps = dict:new(),
    ValidButEmptyPltInfo =
      {iplt_info, [], dict:new(), #{}, ordsets:new()},
    OldVsn = "0.0.1",
    dialyzer_iplt:to_file_custom_vsn(PltFileName, Plt, ModDeps, ValidButEmptyPltInfo, OldVsn, none),

    {_WarningsAfterTouchingFile, Stdout} = run_dialyzer_capture(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    ExpectedLine =
        "PLT is for a different Dialyzer version, so an analysis must be run for 7 modules to rebuild it\n",
    case lists:any(fun (Line) -> Line =:= ExpectedLine end, Stdout) of
      false ->
        ct:pal("Failed to find expected log line in stdout.~nExpected:~n  ~p~nStdout:  ~n~p~n",
                [ExpectedLine, Stdout]),
        ct:fail("Dialyzer failed to print the expected line to stdout");
      true ->
        ok
    end,
    ExpectedMetricsFileStr =
        "total_modules: 7\n"
        "analysed_modules: 7\n"
        "reason: plt_built_with_different_version\n",
    check_metrics_file_contents(MetricsFile, ExpectedMetricsFileStr).

report_old_plt_version_hash_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    MetricsFile = PrivDir ++ "my_incrementality_metrics.log",
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" || Module <- Modules] end,
    PltFileName = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [PltFileName]}, {report_mode, verbose}, {metrics_file, MetricsFile}],

    %% Set up "old" PLT file
    Plt = dialyzer_plt:new(),
    ModDeps = dict:new(),
    ValidButEmptyPltInfo =
      {iplt_info, [], dict:new(), #{}, ordsets:new()},
    FakeImplMd5 = [
        {erl_types, erlang:md5([$f, $o, $o])},
        {erl_bif_types, erlang:md5([$b, $a, $r])}
    ],
    dialyzer_iplt:to_file_custom_vsn(PltFileName, Plt, ModDeps, ValidButEmptyPltInfo, none, FakeImplMd5),

    {_WarningsAfterTouchingFile, Stdout} = run_dialyzer_capture(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    ExpectedLine =
        "PLT is for a different Dialyzer version, so an analysis must be run for 7 modules to rebuild it\n",
    case lists:any(fun (Line) -> Line =:= ExpectedLine end, Stdout) of
      false ->
        ct:pal("Failed to find expected log line in stdout.~nExpected:~n  ~p~nStdout:  ~n~p~n",
                [ExpectedLine, Stdout]),
        ct:fail("Dialyzer failed to print the expected line to stdout");
      true ->
        ok
    end,
    ExpectedMetricsFileStr =
        "total_modules: 7\n"
        "analysed_modules: 7\n"
        "reason: plt_built_with_different_version\n",
    check_metrics_file_contents(MetricsFile, ExpectedMetricsFileStr).

report_no_stored_warnings_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    MetricsFile = PrivDir ++ "my_incrementality_metrics.log",
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" || Module <- Modules] end,
    PltFileName = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [PltFileName]}, {report_mode, verbose}, {metrics_file, MetricsFile}],

    % Run Dialyzer, read the PLT, blank the warning map, then write it back
    _ = run_dialyzer(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    {Plt, PltInfo} = dialyzer_iplt:plt_and_info_from_file(PltFileName),
    {iplt_info, Md5, ModDeps, _Warnings, LegalWarnings} = PltInfo,
    MissingWarningMap = none,
    ValidPltInfoWithNoWarningMap =
      {iplt_info, Md5, ModDeps, MissingWarningMap, LegalWarnings},
    dialyzer_iplt:to_file(PltFileName, Plt, ModDeps, ValidPltInfoWithNoWarningMap),

    {_, Stdout} = run_dialyzer_capture(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    ExpectedLine =
        "PLT does not contain cached warnings, so an analysis must be run for 7 modules to rebuild it\n",
    case lists:any(fun (Line) -> Line =:= ExpectedLine end, Stdout) of
      false ->
        ct:pal("Failed to find expected log line in stdout.~nExpected:~n  ~p~nStdout:  ~n~p~n",
                [ExpectedLine, Stdout]),
        ct:fail("Dialyzer failed to print the expected line to stdout");
      true ->
        ok
    end,
    ExpectedMetricsFileStr =
        "total_modules: 7\n"
        "analysed_modules: 7\n"
        "reason: no_stored_warnings_in_plt\n",
    check_metrics_file_contents(MetricsFile, ExpectedMetricsFileStr).

report_legal_warnings_added(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    MetricsFile = PrivDir ++ "my_incrementality_metrics.log",
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" || Module <- Modules] end,
    Plt = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [Plt]}, {report_mode, verbose}, {warnings, [no_unknown]}],
    _WarningsBeforeTouchingFile = run_dialyzer(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    Opts1 = [{init_plt, [Plt]}, {report_mode, verbose}, {warnings, [unknown]}, {metrics_file, MetricsFile}],
    {_WarningsAfterTouchingFile, Stdout} = run_dialyzer_capture(incremental, [erlang_module() | ToPath(all_mods())], Opts1),

    ExpectedLine =
        "PLT was built for a different set of enabled warnings, so an analysis "
        "must be run for 7 modules to rebuild it\n",
    case lists:any(fun (Line) -> Line =:= ExpectedLine end, Stdout) of
      false ->
        ct:pal("Failed to find expected log line in stdout.~nExpected:~n  ~p~nStdout:  ~n~p~n",
                [ExpectedLine, Stdout]),
        ct:fail("Dialyzer failed to print the expected line to stdout");
      true ->
        ok
    end,
    ExpectedMetricsFileStr =
        "total_modules: 7\n"
        "analysed_modules: 7\n"
        "reason: warnings_changed\n",
    check_metrics_file_contents(MetricsFile, ExpectedMetricsFileStr).

report_legal_warnings_removed(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    MetricsFile = PrivDir ++ "my_incrementality_metrics.log",
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" || Module <- Modules] end,
    Plt = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
    compile_all(DataDir, PrivDir),
    Opts = [{init_plt, [Plt]}, {report_mode, verbose}, {warnings, [unknown]}],
    _ = run_dialyzer(incremental, [erlang_module() | ToPath(all_mods())], Opts),
    Opts1 = [{init_plt, [Plt]}, {report_mode, verbose}, {metrics_file, MetricsFile}, {warnings, [no_unknown]}],
    {_, Stdout} = run_dialyzer_capture(incremental, [erlang_module() | ToPath(all_mods())], Opts1),

    ExpectedLine =
        "PLT was built for a different set of enabled warnings, so an analysis "
        "must be run for 7 modules to rebuild it\n",
    case lists:any(fun (Line) -> Line =:= ExpectedLine end, Stdout) of
      false ->
        ct:pal("Failed to find expected log line in stdout.~nExpected:~n  ~p~nStdout:  ~n~p~n",
                [ExpectedLine, Stdout]),
        ct:fail("Dialyzer failed to print the expected line to stdout");
      true ->
        ok
    end,
    ExpectedMetricsFileStr =
        "total_modules: 7\n"
        "analysed_modules: 7\n"
        "reason: warnings_changed\n",
    check_metrics_file_contents(MetricsFile, ExpectedMetricsFileStr).

fixing_all_warnings(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    ToPath = fun(Modules) -> [PrivDir ++ atom_to_list(Module) ++ ".beam" || Module <- Modules] end,
    Plt = filename:join(PrivDir, "verify_plt_info.iplt"),
    compile_all(DataDir, PrivDir),
    ErlangModule = erlang_module(),
    Opts = [{init_plt, [Plt]}, {warning_files, ToPath([fix, got_fixed])}],
    [] = run_dialyzer(incremental, [ErlangModule | ToPath([fix, got_fixed])], Opts),
    break_module(DataDir, PrivDir),
    [_Error1, _Error2] = run_dialyzer(incremental, [ErlangModule | ToPath([fix, got_fixed])], Opts),
    fix_module(DataDir, PrivDir),
    [] = run_dialyzer(incremental, [ErlangModule | ToPath([fix, got_fixed])], Opts).

break_module(DataDir, PrivDir) ->
    compile:file(filename:join(DataDir, "fix.erl"),
                 [{outdir, PrivDir},
                  debug_info,
                  {d, error, true}]).

fix_module(DataDir, PrivDir) ->
    compile:file(filename:join(DataDir, "fix.erl"),
                 [{outdir, PrivDir},
                  debug_info]).

%% There is one warning per module which will be reported when that module is re-analyzed
%% We expect a module to be re-analyzed when it has changed, or when one of its transitive
%% dependencies have changed.
expected_modules_reanalyzed_incremental(ChangedMods) ->
    DependsOn = fun depends_on/1,
    ordsets:from_list([Mod || C <- ChangedMods,
                              Mod <- [C|depends_on_transitively(C, DependsOn)]]).

change_module(DataDir, PrivDir, Mod) ->
    compile:file(filename:join(DataDir, atom_to_list(Mod)++".erl"),
        [{outdir, PrivDir}, debug_info, {d, Mod, erlang:monotonic_time()}]).

delete_module_beam_file(PrivDir, Mod) ->
    ModuleBeamPath = PrivDir ++ atom_to_list(Mod) ++ ".beam",
    ok = file:delete(ModuleBeamPath).

%% Common utility functions

replace_location(Warnings) ->
    [{Type, loc, Warning} || {Type, _Loc, Warning} <- Warnings].

warning_for_module(Mod) -> {warn_contract_types,loc,
                       {invalid_contract,
                           [Mod,wrong,1,{[1],true},"(float()) -> float()","(integer()) -> integer()"]}}.

compile_all(DataDir, PrivDir) ->
    [{ok, _} = compile:file(File, [{outdir, PrivDir}, debug_info]) || File <- filelib:wildcard(DataDir ++ "*.erl")].

%% This describes the dependencies between modules m1->m6 in
%% incremental_SUITE_data/ If you change thos deps update here.
%% Don't add cycles (yet) or at least without changing depends on transitively
depends_on(m1) -> [];
depends_on(m2) -> [m1];
depends_on(m3) -> [];
depends_on(m4) -> [m2,m3];
depends_on(m5) -> [m4];
depends_on(m6) -> [m4].

depends_on(X, IsInPlt) ->
    [D || D <- depends_on(X), IsInPlt(D)].

depends_on_transitively(M, DependsOn) ->
    Val = DependsOn(M),
    Val ++ [Dep || V <- Val, Dep <- [V|depends_on_transitively(V, DependsOn)]].

all_subsets([X|Rest]) ->
    [Res || SubSet <- all_subsets(Rest), Res <- [[X|SubSet], SubSet]];
all_subsets([]) -> [[]].

all_mods() -> [m1,m2,m3,m4,m5,m6].

run_dialyzer(Analysis, Files, Opts) ->
    dialyzer:run([{analysis_type, Analysis},
      {files, Files},
      {from, byte_code},
      {warnings, [no_unknown]}|
      Opts]).

run_dialyzer_for_modules_analyzed(Analysis, Files, Opts) ->
    dialyzer:run_report_modules_analyzed([{analysis_type, Analysis},
		  {files, Files},
		  {from, byte_code}|
		  Opts]).

default_apps_config_xdg(Config) ->
    TestHome = filename:join(?config(priv_dir, Config), ?FUNCTION_NAME),

    %% We change the $HOME of the emulator to run this test
    HomeEnv =
        case os:type() of
            {win32, _} ->
                [Drive | Path] = filename:split(TestHome),
                [{"APPDATA", filename:join(TestHome, "AppData")},
                 {"HOMEDRIVE", Drive},
                 {"HOMEPATH", filename:join(Path)}];
            _ ->
                [{"HOME", TestHome}]
        end,

    io:format("~p~n", [HomeEnv]),

    PrivDir = ?config(priv_dir, Config),
    PltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),

    {ok, Peer, Node} = ?CT_PEER(#{ env => HomeEnv }),

    erpc:call(
      Node,
      fun() ->
              %% Find out the path of the config file
              HomeConfigFilename =
                  filename:join(filename:basedir(user_config, "erlang"),
                                "dialyzer.config"),
              io:format("~ts\n", [HomeConfigFilename]),
              ok = filelib:ensure_dir(HomeConfigFilename),

              %% Write configuration file
              HomeConfig =
                  {incremental, {default_apps, [stdlib, kernel, erts,
                                                compiler, mnesia, ftp]}},
              ok = file:write_file(HomeConfigFilename,
                                   io_lib:format("~p.~n", [HomeConfig])),

              %% Run dialyzer and check result
              _ = dialyzer:run([{analysis_type, incremental},
                                {init_plt,PltFile},
                                {output_plt, PltFile}]),
              {ok, {incremental, [{modules, Modules}]}} = dialyzer:plt_info(PltFile),

              ExpectedModules = [gb_sets, erlang, compile, mnesia, ftp],

              %% Assert PLT info contains modules from the apps given in the config
              ?assertMatch([], ordsets:subtract(
                                 ordsets:from_list(ExpectedModules),
                                 ordsets:from_list(Modules)))
      end),

    peer:stop(Peer).


default_apps_config_env_var(Config) ->
    TestDir = filename:join(?config(priv_dir, Config), ?FUNCTION_NAME),

    ConfigFilename =
      filename:join(
       [TestDir, "some_custom_location", "dialyzer.config"]),
    ok = filelib:ensure_dir(ConfigFilename),

    Env = [{"DIALYZER_CONFIG", ConfigFilename}],

    DialyzerConfig =
      {incremental, {default_apps, [stdlib, kernel, erts, compiler, mnesia, ftp]}},

    ok = file:write_file(ConfigFilename, io_lib:format("~p.~n", [DialyzerConfig])),

    PrivDir = ?config(priv_dir, Config),
    PltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),

    {ok, Peer, Node} = ?CT_PEER(#{ env => Env }),

    erpc:call(
      Node,
      fun() ->
        _ = dialyzer:run([{analysis_type, incremental},
                          {init_plt,PltFile},
                          {output_plt, PltFile}]),
        {ok, {incremental, [{modules, Modules}]}} = dialyzer:plt_info(PltFile),

        ExpectedModules = [gb_sets, erlang, compile, mnesia, ftp],

        % Assert PLT info contains modules from the apps given in the config
        ?assertMatch([], sets:to_list(sets:subtract(
                           sets:from_list(ExpectedModules),
                           sets:from_list(Modules))))
      end),

    peer:stop(Peer).

default_apps_config_env_var_prioritised_over_xdg(Config) ->
    TestHome = filename:join(?config(priv_dir, Config), ?FUNCTION_NAME),

    ConfigSubDirs =
        case os:type() of
            {unix, darwin} ->
                ["Library","Application Support"];
            {win32, _} ->
                [];
            _ ->
                [".config"]
        end,

    HomeConfigFilename =
      filename:join([TestHome] ++ ConfigSubDirs ++ ["erlang", "dialyzer.config"]),
    ok = filelib:ensure_dir(HomeConfigFilename),

    HomeConfig =
      {incremental, {default_apps, [stdlib, kernel, erts]}},

    ok = file:write_file(HomeConfigFilename, io_lib:format("~p.~n", [HomeConfig])),

    ConfigFilename =
      filename:join(
       [TestHome, "some_custom_location", "dialyzer.config"]),
    ok = filelib:ensure_dir(ConfigFilename),


    %% We change the $HOME of the emulator to run this test
    HomeEnv = case os:type() of
                  {win32, _} ->
                      [Drive | Path] = filename:split(TestHome),
                      [{"APPDATA", filename:join(TestHome,"AppData")},
                       {"HOMEDRIVE", Drive}, {"HOMEPATH", Path}];
                  _ ->
                      [{"HOME", TestHome}]
              end,

    Env = HomeEnv ++ [{"DIALYZER_CONFIG", ConfigFilename}],

    EnvVarConfig =
      {incremental, {default_apps, [compiler, mnesia, ftp]}},

    ok = file:write_file(ConfigFilename, io_lib:format("~p.~n", [EnvVarConfig])),

    PrivDir = ?config(priv_dir, Config),
    PltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),

    {ok, Peer, Node} = ?CT_PEER(#{ env => Env }),

    erpc:call(
      Node,
      fun() ->
        _ = dialyzer:run([{analysis_type, incremental},
                          {init_plt,PltFile},
                          {output_plt, PltFile}]),
        {ok, {incremental, [{modules, Modules}]}} = dialyzer:plt_info(PltFile),

        ExpectedModules = [compile, mnesia, ftp],
        UnexpectedModules = [gb_sets],

        % Assert PLT info contains modules from the apps given in the env var config
        ?assertMatch([], sets:to_list(sets:subtract(
                           sets:from_list(ExpectedModules),
                           sets:from_list(Modules)))),

        % Assert PLT info does not contain modules from the apps given in the xdg config
        ?assertMatch([], sets:to_list(sets:intersection(
                           sets:from_list(UnexpectedModules),
                           sets:from_list(Modules))))
      end),

    peer:stop(Peer).

legal_warnings_config_xdg(Config) ->
    TestHome = filename:join(?config(priv_dir, Config), ?FUNCTION_NAME),

    %% We change the $HOME of the emulator to run this test
    HomeEnv =
        case os:type() of
            {win32, _} ->
                [Drive | Path] = filename:split(TestHome),
                [{"APPDATA", filename:join(TestHome, "AppData")},
                 {"HOMEDRIVE", Drive},
                 {"HOMEPATH", filename:join(Path)}];
            _ ->
                [{"HOME", TestHome}]
        end,

    io:format("~p~n", [HomeEnv]),

    {ok, Peer, Node} = ?CT_PEER(#{ env => HomeEnv }),

    SrcWithImproperList = <<"
      -module(my_improper_list_module).
      -export([g/0]).

      g() -> [a|b]. % Improper list: Last element is not the empty list
      ">>,

    {ok, BeamFileWithImproperList} =
      compile(Config, SrcWithImproperList, my_improper_list_module, []),

    AppsConfig =
        {incremental, {default_apps, [stdlib, kernel, erts, compiler, mnesia, ftp]}},

    erpc:call(
      Node,
      fun() ->
          %% Find out the path of the config file
          HomeConfigFilename =
              filename:join(filename:basedir(user_config, "erlang"),
                            "dialyzer.config"),
          io:format("~ts\n", [HomeConfigFilename]),
          ok = filelib:ensure_dir(HomeConfigFilename),

          %% Write configuration file
          WarningsConfig1 =
              {warnings, [no_unknown, no_improper_lists]},
          ok = file:write_file(HomeConfigFilename,
                               io_lib:format("~p.~n~p.~n", [AppsConfig, WarningsConfig1])),
          WarningsWithConfigSet =
            dialyzer:run([{analysis_type, incremental},
                          {files, [BeamFileWithImproperList]},
                          {from, byte_code}]),
          ?assertEqual([], WarningsWithConfigSet),

          %% Write alternative configuration file
          WarningsConfig2 =
              {warnings, [no_unknown]},
          ok = file:write_file(HomeConfigFilename,
                               io_lib:format("~p.~n~p.~n", [AppsConfig, WarningsConfig2])),
          WarningsWithoutConfigSet =
            dialyzer:run([{analysis_type, incremental},
                          {files, [BeamFileWithImproperList]},
                          {from, byte_code}]),
          ?assertMatch([{warn_non_proper_list, _Loc, _Msg}], WarningsWithoutConfigSet)
      end),

    peer:stop(Peer).

paths_config_xdg(Config) ->
    TestHome = filename:join(?config(priv_dir, Config), ?FUNCTION_NAME),

    %% We change the $HOME of the emulator to run this test
    HomeEnv =
        case os:type() of
            {win32, _} ->
                [Drive | Path] = filename:split(TestHome),
                [{"APPDATA", filename:join(TestHome, "AppData")},
                 {"HOMEDRIVE", Drive},
                 {"HOMEPATH", filename:join(Path)}];
            _ ->
                [{"HOME", TestHome}]
        end,

    io:format("~p~n", [HomeEnv]),

    ExtraModulesDirOrig =
      filename:join(?config(data_dir, Config), "extra_modules"),
    ExtraModulesDir =
      filename:join(?config(priv_dir, Config), "extra_modules"),
    ok = filelib:ensure_path(ExtraModulesDir),
    ok = filelib:ensure_path(filename:join(ExtraModulesDir,"ebin")),
    ok = filelib:ensure_path(filename:join(ExtraModulesDir,"src")),

    {ok, _} =
      file:copy(
        filename:join([ExtraModulesDirOrig, "src", "extra_modules.app.src"]),
        filename:join([ExtraModulesDir, "src", "extra_modules.app.src"])
      ),
    {ok, _} =
      file:copy(
        filename:join([ExtraModulesDirOrig, "src", "extra_module.erl"]),
        filename:join([ExtraModulesDir, "src", "extra_module.erl"])
      ),

    {ok, Peer, Node} = ?CT_PEER(#{ env => HomeEnv }),
    {ok, _} =
      compile:file(
        filename:join([ExtraModulesDir,"src","extra_module.erl"]),
        [{outdir, filename:join([ExtraModulesDir,"ebin"])}, debug_info]
      ),

    AppsConfig =
        {incremental,
          {default_apps,
            [stdlib,
             kernel,
             erts,
             extra_modules
            ]
          },
          {default_warning_apps,
            [extra_modules % Only on path if added explicitly via config below
            ]
          }
        },

    WarningsConfig =
        {warnings, [no_unknown]},

    erpc:call(
      Node,
      fun() ->
          %% Find out the path of the config file
          HomeConfigFilename =
              filename:join(filename:basedir(user_config, "erlang"),
                            "dialyzer.config"),
          io:format("~ts~n", [HomeConfigFilename]),
          ok = filelib:ensure_dir(HomeConfigFilename),

          %% Write configuration file
          PathConfig = {add_pathsa, [ExtraModulesDir, filename:join(ExtraModulesDir, "ebin"), filename:join(ExtraModulesDir, "src")]},
          ok =
            file:write_file(
              HomeConfigFilename,
              io_lib:format(
                "~p.~n~p.~n~p.~n",
                [AppsConfig, WarningsConfig, PathConfig])),

          {Warnings, ModAnalyzed} =
            % Will analyse apps from config, including the `extra_modules` app
            % which contains a Dialyzer error
            dialyzer:run_report_modules_analyzed([
                {analysis_type, incremental},
                {from, byte_code}]),

          % Check we did actually analyze the module
          ?assert(
             lists:member(extra_module, ModAnalyzed),
             lists:flatten(io_lib:format("Looking for 'extra_module' in ~tp~n", [ModAnalyzed]))),

          % Check we got the warnings we expected from modules
          % added to the path
          ?assertMatch(
             [ {warn_contract_types, {_,_}, {invalid_contract, [extra_module,f,1, {[1],true}, "(atom()) -> string()", "(integer()) -> nonempty_improper_list(integer(),3)"]}},
               {warn_non_proper_list, {_,_}, {improper_list_constr,["3"]}}
             ],
             Warnings)
      end),

    peer:stop(Peer).

multiple_plts_unsupported_in_incremental_mode(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    BazPltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ "-baz.iplt"),
    QuuxPltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ "-quux.iplt"),

    BazSrc = <<"
      -module(baz).

      f() -> ok.
      ">>,
    QuuxSrc = <<"
      -module(quux).

      g() -> undefined.
      ">>,

    {ok, BazBeamFile} = compile(Config, BazSrc, baz, []),
    {ok, QuuxBeamFile} = compile(Config, QuuxSrc, quux, []),
    _ = run_dialyzer(incremental, [BazBeamFile], [{output_plt, BazPltFile}]),
    _ = run_dialyzer(incremental, [QuuxBeamFile], [{output_plt, QuuxPltFile}]),

    ?assertThrow(
       {dialyzer_error,"Incremental mode does not support multiple PLT files (" ++ _},
       run_dialyzer(incremental,
                    [BazBeamFile, QuuxBeamFile],
                    [{plts, [BazPltFile, QuuxPltFile]}])).

compile(Config, Prog, Module, CompileOpts) ->
    Source = lists:concat([Module, ".erl"]),
    PrivDir = proplists:get_value(priv_dir,Config),
    Filename = filename:join([PrivDir, Source]),
    ok = file:write_file(Filename, Prog),
    Opts = [{outdir, PrivDir}, debug_info | CompileOpts],
    {ok, Module} = compile:file(Filename, Opts),
    {ok, filename:join([PrivDir, lists:concat([Module, ".beam"])])}.

%% https://erlang.org/doc/man/ct.html#capture_get-1 seems to work by redirecting
%% writes to stdout to be messages to the current pid. Those messages get queued up,
%% with the intent to read them back later to get stdout.
%%
%% At various points, Dialyzer dequeues messages from its worker child processes,
%% sometimes throwing them away knowing it doesn't need certain results from them.
%%
%% Sadly, these two things interact badly and Dialyzer ends up dequeuing (and hence,
%% deleting) some of what was written to stdout. The solution is to run Dialyzer in
%% a sub-process, so the messages go to the parent process's mailbox and Dialyzer is
%% free to do what it wants with its own messages.
%%
%% Fundamentally this is an issue with ct:capture_get, since it seems to presume it's
%% fine to send messages to the test case's pid and they'll never be read by the code
%% under test!
run_dialyzer_capture(Analysis, Files, Opts) ->
    ct:capture_start(),
    TestCasePid = self(),
    RunDialyzer = fun() ->
        Ret = run_dialyzer(Analysis, Files, Opts),
        TestCasePid ! {dialyzer_done, Ret}
    end,
    _DialyzerPid = spawn_link(RunDialyzer),
    DialyzerRet =
        receive
            {dialyzer_done, Ret} -> Ret
        end,
    ct:capture_stop(),
    Stdout = ct:capture_get([]),
    {DialyzerRet, Stdout}.
