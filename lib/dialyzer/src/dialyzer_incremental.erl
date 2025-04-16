%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright (c) Meta Platforms, Inc. and affiliates.
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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

-module(dialyzer_incremental).
-moduledoc false.

-export([start/1, start_report_modules_analyzed/1, start_report_modules_changed_and_analyzed/1]).

-include("dialyzer.hrl").
-include_lib("kernel/include/file.hrl").  % needed for #file_info{}

-record(incremental_state,
    {backend_pid                      :: pid() | 'undefined',
     code_server     = none           :: 'none' | dialyzer_codeserver:codeserver(),
     erlang_mode     = false          :: boolean(),
     external_calls  = []             :: [{mfa(), warning_info()}],
     external_types  = []             :: [{mfa(), warning_info()}],
     legal_warnings  = ordsets:new()  :: [dial_warn_tag()],
     mod_deps        = dict:new()     :: dialyzer_callgraph:mod_deps(),
     output          = standard_io    :: io:device(),
     output_format   = formatted      :: format(),
     filename_opt    = basename       :: filename_opt(),
     error_location  = ?ERROR_LOCATION :: error_location(),
     indent_opt      = ?INDENT_OPT    :: iopt(),
     output_plt      = none           :: file:filename(),
     plt_info        = none           :: 'none' | #iplt_info{},
     report_mode     = normal         :: rep_mode(),
     return_status   = ?RET_NOTHING_SUSPICIOUS  :: dial_ret(),
     warning_modules = []             :: [module()],
     stored_warnings = []             :: [raw_warning()]
    }).

-type incrementality_reason() ::
    no_stored_warnings_in_plt
    | plt_built_with_different_version
    | new_plt_file
    | warnings_changed
    | {incremental_changes, NumModulesChangedOrRemoved :: non_neg_integer()}.

-record(incrementality_metrics,
    {total_modules :: non_neg_integer(),
     analysed_modules :: non_neg_integer(),
     reason :: incrementality_reason()
    }).

%%--------------------------------------------------------------------

-spec start(#options{}) -> {dial_ret(), [dial_warning()]}.

start(Opts) ->
    {{Ret,Warns}, _ModulesAnalyzed} = start_report_modules_analyzed(Opts),
    {Ret,Warns}.

-spec start_report_modules_analyzed(#options{}) ->
  {{dial_ret(), [dial_warning()]}, [module()]}.

start_report_modules_analyzed(#options{analysis_type = incremental} = Options) ->
    {{Ret, Warn}, _Changed, Analyzed} =
        start_report_modules_changed_and_analyzed(Options),
    {{Ret, Warn}, Analyzed}.

-spec start_report_modules_changed_and_analyzed(#options{}) ->
    {{dial_ret(), [dial_warning()]},
     Changed :: undefined | [module()],
     Analyzed :: [module()]}.

start_report_modules_changed_and_analyzed( #options{analysis_type = incremental} = Options) ->
    Opts1 = init_opts_for_incremental(Options),
    assert_metrics_file_valid(Opts1),
    #options{init_plts = [InitPlt], legal_warnings = LegalWarnings} = Opts1,
    Files = get_files_from_opts(Opts1),
    case dialyzer_iplt:check_incremental_plt(InitPlt, Opts1, Files) of
        {ok, #iplt_info{files = Md5, warning_map = none}, ModuleToPathLookup} ->
            report_no_stored_warnings(Opts1, Md5),
            PltInfo = #iplt_info{files = Md5, legal_warnings = LegalWarnings},
            write_module_to_path_lookup(Opts1, ModuleToPathLookup),
            enrich_with_modules_changed(do_analysis(maps:values(ModuleToPathLookup), Opts1, dialyzer_plt:new(), PltInfo), []);
        {ok, #iplt_info{warning_map=WarningMap, files = Md5}, ModuleToPathLookup} ->
            report_stored_warnings_no_changes(Opts1, Md5),
            write_module_to_path_lookup(Opts1, ModuleToPathLookup),
            enrich_with_modules_changed(return_existing_errors(Opts1, WarningMap), []);
        {old_version, Md5, ModuleToPathLookup} ->
            report_different_plt_version(Opts1, Md5),
            PltInfo = #iplt_info{files = Md5, legal_warnings = LegalWarnings},
            write_module_to_path_lookup(Opts1, ModuleToPathLookup),
            enrich_with_modules_changed(do_analysis(maps:values(ModuleToPathLookup), Opts1, dialyzer_plt:new(), PltInfo), undefined);
        {new_file, Md5, ModuleToPathLookup} ->
            report_new_plt_file(Opts1, InitPlt, Md5),
            PltInfo = #iplt_info{files = Md5, legal_warnings = LegalWarnings},
            write_module_to_path_lookup(Opts1, ModuleToPathLookup),
            enrich_with_modules_changed(do_analysis(maps:values(ModuleToPathLookup), Opts1, dialyzer_plt:new(), PltInfo), undefined);
        {differ, Md5, _DiffMd5, _ModDeps, none, ModuleToPathLookup} ->
            report_no_stored_warnings(Opts1, Md5),
            PltInfo = #iplt_info{files = Md5, legal_warnings = LegalWarnings},
            AllFiles = maps:values(ModuleToPathLookup),
            write_module_to_path_lookup(Opts1, ModuleToPathLookup),
            enrich_with_modules_changed(do_analysis(AllFiles, Opts1, dialyzer_plt:new(), PltInfo), undefined);
        {differ, Md5, DiffMd5, ModDeps, WarningMap, ModuleToPathLookup} ->
            report_incremental_analysis_needed(Opts1, DiffMd5),
            {AnalFiles, ModsToRemove, ModDepsInRemainingPlt} =
                expand_dependent_modules(Md5, DiffMd5, ModDeps, ModuleToPathLookup),
            WarningsInRemainingPlt =
                sets:fold(fun(Mod, Acc) -> maps:remove(Mod, Acc) end,
                              WarningMap, ModsToRemove),
            Plt = clean_plt(InitPlt, ModsToRemove),

            PltInfo = #iplt_info{files = Md5,
                                 mod_deps = ModDepsInRemainingPlt,
                                 warning_map = WarningsInRemainingPlt,
                                 legal_warnings = LegalWarnings},
            ChangedOrRemovedMods = [ChangedOrRemovedMod || {_, ChangedOrRemovedMod} <- DiffMd5],
            case AnalFiles =:= [] of
                true ->
                    %% Only removed stuff that's unused. Just write the PLT.
                    report_stored_warnings_only_safe_removals(Opts1, Md5, DiffMd5),
                    dialyzer_iplt:to_file(Opts1#options.output_plt, Plt, ModDepsInRemainingPlt, PltInfo),
                    write_module_to_path_lookup(Opts1, ModuleToPathLookup),
                    enrich_with_modules_changed(return_existing_errors(Opts1, WarningsInRemainingPlt), ChangedOrRemovedMods);
                false ->
                    report_degree_of_incrementality(Opts1, Md5, DiffMd5, AnalFiles),
                    write_module_to_path_lookup(Opts1, ModuleToPathLookup),
                    enrich_with_modules_changed(do_analysis(AnalFiles, Opts1, Plt, PltInfo), ChangedOrRemovedMods)
            end;
        {legal_warnings_changed, Md5, ModuleToPathLookup} ->
            report_change_in_legal_warnings(Opts1, Md5),
            PltInfo = #iplt_info{files = Md5, legal_warnings = LegalWarnings},
            AllFiles = maps:values(ModuleToPathLookup),
            write_module_to_path_lookup(Opts1, ModuleToPathLookup),
            enrich_with_modules_changed(do_analysis(AllFiles, Opts1, dialyzer_plt:new(), PltInfo), undefined);
        {error, not_valid} ->
            Msg = io_lib:format("The file: ~ts is not a valid PLT file\n~s",
                [InitPlt, default_plt_error_msg()]),
            cl_error(Msg);
        {error, read_error} ->
            Msg = io_lib:format("Could not read the PLT: ~ts\n~s",
                [InitPlt, default_plt_error_msg()]),
            cl_error(Msg)
    end.

-spec enrich_with_modules_changed({{Ret :: dial_ret(), Warns :: [dial_warning()]}, Analyzed :: [module()]}, Changed :: undefined | [module()]) ->
    {{dial_ret(), [dial_warning()]}, Changed :: undefined | [module()], Analyzed :: [module()]}.

enrich_with_modules_changed({{Ret,Warns}, Analyzed}, Changed) ->
    {{Ret,Warns}, Changed, Analyzed}.

default_plt_error_msg() ->
    "Remove the broken PLT file or point to the correct location.\n".

init_opts_for_incremental(Opts) ->
  InitPlt =
      case Opts#options.init_plts of
          []-> dialyzer_iplt:get_default_iplt_filename();
          [Plt] -> Plt;
          Plts ->
              Msg =
                  io_lib:format("Incremental mode does not support multiple PLT files (~ts)\n",
                                [format_plts(Plts)]),
                  cl_error(Msg)
      end,
  OutputPlt =
      case Opts#options.output_plt of
          none -> InitPlt;
          ExplicitlySetOutputPlt -> ExplicitlySetOutputPlt
      end,
  Opts#options{
         analysis_type = incremental,
         defines       = [],
         from          = byte_code,
         init_plts     = [InitPlt],
         include_dirs  = [],
         output_plt    = OutputPlt,
         use_contracts = true,
         get_warnings = true
   }.

assert_metrics_file_valid(#options{metrics_file = none}) ->
    ok;

assert_metrics_file_valid(#options{metrics_file = MetricsFile}) ->
    case check_if_writable(MetricsFile) of
        true -> ok;
        false ->
            Msg = io_lib:format("    The metrics file ~ts is not writable", [MetricsFile]),
            cl_error(Msg)
    end.

write_metrics_file(#options{metrics_file = none}, _Format, _Args) ->
    ok;

write_metrics_file(#options{metrics_file = MetricsFile}, Format, Args) ->
    case file:write_file(MetricsFile, io_lib:fwrite(Format, Args)) of
        ok -> ok;
        {error, Reason} ->
            Msg = io_lib:format("Could not write metrics file ~ts: ~w\n",
                [MetricsFile, Reason]),
            throw({dialyzer_error, Msg})
    end.

write_metrics_file(
    Opts,
    #incrementality_metrics{
        total_modules = NumTotalModules,
        analysed_modules = NumAnalysedModules,
        reason = Reason}
    ) ->

    ReasonDescription =
        case Reason of
            no_stored_warnings_in_plt -> "no_stored_warnings_in_plt";
            plt_built_with_different_version -> "plt_built_with_different_version";
            new_plt_file -> "new_plt_file";
            warnings_changed -> "warnings_changed";
            {incremental_changes, NumChangedModules} ->
                io_lib:format("incremental_changes\nchanged_or_removed_modules: ~B", [NumChangedModules])
        end,

    write_metrics_file(
        Opts,
        "total_modules: ~B\nanalysed_modules: ~B\nreason: ~s\n",
        [NumTotalModules, NumAnalysedModules, ReasonDescription]).

write_module_to_path_lookup(#options{module_lookup_file = none}, _ModuleToPathLookup) ->
    ok;

write_module_to_path_lookup(#options{module_lookup_file = LookupFile}, ModuleToPathLookup) ->
    Output = [io_lib:fwrite("~ts, ~ts\n", [atom_to_list(ModuleName), ModulePath]) ||
                 ModuleName := ModulePath <- ModuleToPathLookup],
    case file:write_file(LookupFile, Output) of
        ok -> ok;
        {error, Reason} ->
            Msg = io_lib:format("Could not write module lookup file ~ts: ~w\n",
                [LookupFile, Reason]),
            throw({dialyzer_error, Msg})
    end.


report_new_plt_file(#options{report_mode = ReportMode} = Opts, InitPlt, Md5) ->
    NumTotalModules = length(Md5),
    Metrics =
        #incrementality_metrics{
            total_modules = NumTotalModules,
            analysed_modules = NumTotalModules,
            reason = new_plt_file
        },
    case ReportMode of
        quiet -> ok;
        normal -> ok;
        verbose -> io:format("PLT does not yet exist at ~s, so an analysis must be run for ~w modules to populate it\n", [InitPlt, NumTotalModules])
    end,
    write_metrics_file(Opts, Metrics).

report_different_plt_version(#options{report_mode = ReportMode} = Opts, Md5) ->
    NumTotalModules = length(Md5),
    Metrics =
        #incrementality_metrics{
            total_modules = NumTotalModules,
            analysed_modules = NumTotalModules,
            reason = plt_built_with_different_version
        },
    case ReportMode of
        quiet -> ok;
        normal -> ok;
        verbose -> io:format("PLT is for a different Dialyzer version, so an analysis must be run for ~w modules to rebuild it\n", [NumTotalModules])
    end,
    write_metrics_file(Opts, Metrics).

report_stored_warnings_no_changes(#options{report_mode = ReportMode} = Opts, Md5) ->
    NumTotalModules = length(Md5),
    Metrics =
        #incrementality_metrics{
            total_modules = NumTotalModules,
            analysed_modules = 0,
            reason = {incremental_changes, 0}
        },
    case ReportMode of
        quiet -> ok;
        normal -> ok;
        verbose -> io:format("PLT has fully cached the request, so no additional analysis is needed\n", [])
    end,
    write_metrics_file(Opts, Metrics).

report_stored_warnings_only_safe_removals(#options{report_mode = ReportMode} = Opts, Md5, Removed) ->
    NumTotalModules = length(Md5),
    NumRemovedModuled = length(Removed),
    Metrics =
        #incrementality_metrics{
            total_modules = NumTotalModules,
            analysed_modules = 0,
            reason = {incremental_changes, NumRemovedModuled}
        },
    case ReportMode of
        quiet -> ok;
        normal -> ok;
        verbose -> io:format("PLT has fully cached the request because nothing depended on the file removed, so no additional analysis is needed\n", [])
    end,
    write_metrics_file(Opts, Metrics).

report_no_stored_warnings(#options{report_mode = ReportMode} = Opts, Md5) ->
    NumTotalModules = length(Md5),
    Metrics =
        #incrementality_metrics{
            total_modules = NumTotalModules,
            analysed_modules = NumTotalModules,
            reason = no_stored_warnings_in_plt
        },
    case ReportMode of
        quiet -> ok;
        normal -> ok;
        verbose ->
            io:format(
                "PLT does not contain cached warnings, so an analysis must be run for ~w modules to rebuild it\n",
                [length(Md5)]
            )
    end,
    write_metrics_file(Opts, Metrics).

report_incremental_analysis_needed(#options{report_mode = ReportMode}, DiffMd5) ->
    case ReportMode of
        quiet -> ok;
        normal -> io:format("There have been changes to analyze\n", []);
        verbose -> report_md5_diff(DiffMd5)
    end.

report_degree_of_incrementality(#options{report_mode = ReportMode} = Opts, Md5, ChangedOrRemovedFiles, FilesThatNeedAnalysis) ->
    NumTotalModules = length(Md5),
    NumChangedOrRemovedModules = length(ChangedOrRemovedFiles),
    NumAnalysedModules = length(FilesThatNeedAnalysis),
    Metrics =
        #incrementality_metrics{
            total_modules = NumTotalModules,
            analysed_modules = NumAnalysedModules,
            reason = {incremental_changes, NumChangedOrRemovedModules}
        },
    ReportFun = fun () ->
            io:format(
                "    Of the ~B files being tracked, ~B have been changed or removed, "
                "resulting in ~B requiring analysis because they depend on those changes\n",
                [NumTotalModules, NumChangedOrRemovedModules, NumAnalysedModules])
        end,
    case ReportMode of
        quiet -> ok;
        normal -> ReportFun();
        verbose ->
            ReportFun(),
            io:format("    Modules which will be analysed: ~p\n", [[path_to_mod(P) || P <- FilesThatNeedAnalysis]])
    end,
    write_metrics_file(Opts, Metrics).

report_change_in_legal_warnings(#options{report_mode = ReportMode} = Opts, Md5) ->
    NumTotalModules = length(Md5),
    Metrics =
        #incrementality_metrics{
            total_modules = NumTotalModules,
            analysed_modules = NumTotalModules,
            reason = warnings_changed
        },
    ReportFun = fun () ->
        io:format(
            "PLT was built for a different set of enabled warnings, so an analysis must be run for ~w modules to rebuild it\n",
            [NumTotalModules]
        )
    end,
    case ReportMode of
        quiet -> ok;
        normal -> ReportFun();
        verbose -> ReportFun()
    end,
    write_metrics_file(Opts, Metrics).

report_analysis_start(#options{report_mode = quiet}) -> ok;
report_analysis_start(_) ->
    io:format("Proceeding with incremental analysis...").

report_elapsed_time(T1, T2, #options{report_mode = ReportMode}) ->
  case ReportMode of
      quiet -> ok;
      _ ->
          ElapsedTime = T2 - T1,
          Mins = ElapsedTime div 60000,
          Secs = (ElapsedTime rem 60000) / 1000,
          io:format(" done in ~wm~.2fs\n", [Mins, Secs])
  end.

report_md5_diff(List) ->
    io:format("    The PLT information is not up to date:\n", []),
    case [Mod || {removed, Mod} <- List] of
        [] -> ok;
        RemovedMods -> io:format("    Removed modules: ~p\n", [RemovedMods])
    end,
    case [Mod || {differ, Mod} <- List] of
        [] -> ok;
        ChangedMods -> io:format("    Changed modules: ~p\n", [ChangedMods])
    end.
%%--------------------------------------------------------------------


format_plts([Plt]) -> Plt;
format_plts([Plt|Plts]) ->
    Plt ++ ", " ++ format_plts(Plts).

%%--------------------------------------------------------------------

do_analysis(Files, Options, Plt, PltInfo) ->
    assert_writable(Options#options.output_plt),
    report_analysis_start(Options),
    State1 = init_output(Options),
    State2 = State1#incremental_state{
        legal_warnings = Options#options.legal_warnings,
        output_plt = Options#options.output_plt,
        plt_info = PltInfo,
        erlang_mode = Options#options.erlang_mode,
        report_mode = Options#options.report_mode,
        warning_modules = get_warning_modules_from_opts(Options)
    },
    InitAnalysis = #analysis{
         type = succ_typings,
         defines = Options#options.defines,
         include_dirs = Options#options.include_dirs,
         files = Files,
         start_from = Options#options.from,
         timing = Options#options.timing,
         plt = Plt,
         use_contracts = Options#options.use_contracts,
         callgraph_file = Options#options.callgraph_file,
         mod_deps_file = Options#options.mod_deps_file,
         solvers = Options#options.solvers
    },
    State3 = start_analysis(State2, InitAnalysis),
    {T1, _} = statistics(wall_clock),
    RetAndWarns = cl_loop(State3),
    {T2, _} = statistics(wall_clock),
    report_elapsed_time(T1, T2, Options),
    {RetAndWarns, lists:usort([path_to_mod(F) || F <- Files])}.

%%--------------------------------------------------------------------


assert_writable(PltFile) ->
    case check_if_writable(PltFile) of
        true -> ok;
        false ->
            Msg = io_lib:format("    The PLT file ~ts is not writable", [PltFile]),
            cl_error(Msg)
    end.

check_if_writable(PltFile) ->
    case filelib:is_regular(PltFile) of
        true -> is_writable_file_or_dir(PltFile);
        false ->
            case filelib:is_dir(PltFile) of
                true ->
                    false;
                false ->
                    DirName = filename:dirname(PltFile),
                    case filelib:is_dir(DirName) of
                        false ->
                            case filelib:ensure_dir(PltFile) of
                                ok ->
                                    true;
                                {error, _} ->
                                    false
                            end;
                        true ->
                            is_writable_file_or_dir(DirName)
                    end
            end
    end.

is_writable_file_or_dir(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{access = A}} ->
            (A =:= write) orelse (A =:= read_write);
        {error, _} ->
            false
    end.

%%--------------------------------------------------------------------

clean_plt(PltFile, RemovedMods) ->
    %% Clean the plt from the removed modules.
    Plt = dialyzer_iplt:from_file(PltFile),
    sets:fold(fun(M, AccPlt) -> dialyzer_plt:delete_module(AccPlt, M) end,
    Plt, RemovedMods).

expand_dependent_modules(_Md5, DiffMd5, ModDeps, ModuleToPathLookup) ->
    ChangedMods = sets:from_list([M || {differ, M} <- DiffMd5]),
    RemovedMods = sets:from_list([M || {removed, M} <- DiffMd5]),
    BigSet = sets:union(ChangedMods, RemovedMods),
    BigList = sets:to_list(BigSet),
    ExpandedSet = expand_dependent_modules_1(BigList, BigSet, ModDeps),
    NewModDeps = dialyzer_callgraph:strip_module_deps(ModDeps, BigSet),
    AnalyzeMods = sets:subtract(ExpandedSet, RemovedMods),
    FilterFun = fun(File) ->
            Mod = path_to_mod(File),
            sets:is_element(Mod, AnalyzeMods)
        end,
    {[F || F <- maps:values(ModuleToPathLookup), FilterFun(F)], ExpandedSet, NewModDeps}.

expand_dependent_modules_1([Mod|Mods], Included, ModDeps) ->
    case dict:find(Mod, ModDeps) of
        {ok, Deps} ->
            NewDeps = sets:subtract(sets:from_list(Deps), Included),
            case sets:size(NewDeps) of
                0 -> expand_dependent_modules_1(Mods, Included, ModDeps);
                _ ->
                    NewIncluded = sets:union(Included, NewDeps),
                    expand_dependent_modules_1(sets:to_list(NewDeps) ++ Mods,
                                               NewIncluded, ModDeps)
            end;
        error ->
            expand_dependent_modules_1(Mods, Included, ModDeps)
    end;
expand_dependent_modules_1([], Included, _ModDeps) ->
    Included.

path_to_mod(File) ->
    list_to_atom(filename:basename(File, ".beam")).

init_output(#options{output_file = OutFile,
           output_plt = OutPlt,
           output_format = OutFormat,
           filename_opt = FOpt,
           indent_opt = IOpt,
           error_location = EOpt} = Opts) ->
    State = #incremental_state{output_format = OutFormat,
                               output_plt = OutPlt,
                               filename_opt = FOpt,
                               indent_opt = IOpt,
                               error_location = EOpt,
                               warning_modules = get_warning_modules_from_opts(Opts)},
    case OutFile =:= none of
        true ->
            State;
        false ->
            case file:open(OutFile, [write]) of
                {ok, File} ->
                    %% Warnings and errors can include Unicode characters.
                    ok = io:setopts(File, [{encoding, unicode}]),
                    State#incremental_state{output = File};
                {error, Reason} ->
                    Msg = io_lib:format("Could not open output file ~tp, Reason: ~p\n",
                              [OutFile, Reason]),
                    cl_error(State, lists:flatten(Msg))
            end
    end.

-spec maybe_close_output_file(#incremental_state{}, boolean()) -> 'ok'.

maybe_close_output_file(State, OutputPltInUse) ->
    case State#incremental_state.output of
        standard_io -> ok;
        File when OutputPltInUse -> ok = file:close(File);
        _File -> ok
    end.

%% ----------------------------------------------------------------
%%
%%  Main Loop
%%

-define(LOG_CACHE_SIZE, 10).

%%-spec cl_loop(#incremental_state{}) ->
cl_loop(State) ->
    cl_loop(State, []).

cl_loop(State, LogCache) ->
    BackendPid = State#incremental_state.backend_pid,
    receive
        {BackendPid, log, LogMsg} ->
            cl_loop(State, lists:sublist([LogMsg|LogCache], ?LOG_CACHE_SIZE));
        {BackendPid, warnings, Warnings} ->
            NewState = store_warnings(State, Warnings),
            cl_loop(NewState, LogCache);
        {BackendPid, cserver, CodeServer, _Plt} -> % Plt is ignored
            NewState = State#incremental_state{code_server = CodeServer},
            cl_loop(NewState, LogCache);
        {BackendPid, done, NewPlt, _NewDocPlt} ->
            return_value(State, NewPlt);
        {BackendPid, ext_calls, ExtCalls} ->
            cl_loop(State#incremental_state{external_calls = ExtCalls}, LogCache);
        {BackendPid, ext_types, ExtTypes} ->
            cl_loop(State#incremental_state{external_types = ExtTypes}, LogCache);
        {BackendPid, mod_deps, ModDeps} ->
            NewState = State#incremental_state{mod_deps = ModDeps},
            cl_loop(NewState, LogCache);
        {'EXIT', BackendPid, {error, Reason}} ->
            Msg = failed_anal_msg(Reason, LogCache),
            cl_error(State, Msg);
        {'EXIT', BackendPid, Reason} when Reason =/= 'normal' ->
            Msg = failed_anal_msg(io_lib:format("~p", [Reason]), LogCache),
            cl_error(State, Msg);
        _Other ->
            cl_loop(State, LogCache)
    end.

-spec failed_anal_msg(string(), [_]) -> nonempty_string().

failed_anal_msg(Reason, LogCache) ->
    Msg = "Analysis failed with error:\n" ++ lists:flatten(Reason) ++ "\n",
    case LogCache =:= [] of
        true -> Msg;
        false ->
            Msg ++ "Last messages in the log cache:\n  " ++ format_log_cache(LogCache)
    end.

%%
%% formats the log cache (treating it as a string) for pretty-printing
%%
format_log_cache(LogCache) ->
    Str = lists:append(lists:reverse(LogCache)),
    lists:join("\n  ", string:lexemes(Str, "\n")).

-spec store_warnings(#incremental_state{}, [raw_warning()]) -> #incremental_state{}.

store_warnings(#incremental_state{stored_warnings = StoredWarnings} = St, Warnings) ->
    St#incremental_state{stored_warnings = StoredWarnings ++ Warnings}.

-spec cl_error(string()) -> no_return().

cl_error(Msg) ->
    throw({dialyzer_error, lists:flatten(Msg)}).

-spec cl_error(#incremental_state{}, string()) -> no_return().

cl_error(State, Msg) ->
    case State#incremental_state.output of
        standard_io -> ok;
        Outfile -> io:format(Outfile, "\n~ts\n", [Msg])
    end,
    maybe_close_output_file(State, true),
    throw({dialyzer_error, lists:flatten(Msg)}).

return_value(
    State =
        #incremental_state{
            code_server = CodeServer,
            mod_deps = ModDeps,
            output_plt = OutputPlt,
            plt_info = PltInfo,
            stored_warnings = NewPLTWarnings
        },
    Plt) ->
    %% Just for now:
    case CodeServer =:= none of
        true ->
            ok;
        false ->
            dialyzer_codeserver:delete(CodeServer)
    end,
    OldPltWarnings = PltInfo#iplt_info.warning_map,
    PLTUnknownWarnings = unknown_warnings_by_module(State),
    PLTWarningMap = dialyzer_iplt:merge_warnings(NewPLTWarnings, PLTUnknownWarnings, OldPltWarnings),
    PLTWarningList =
        [Warn || Mod <- maps:keys(PLTWarningMap), Warn <- maps:get(Mod, PLTWarningMap, [])],
    NewState = State#incremental_state{stored_warnings=PLTWarningList},
    % Write warnings for all modules to the PLT, even if we're not reporting
    %   them now, so subsequent runs can read them from the PLT
    dialyzer_iplt:to_file(OutputPlt, Plt, ModDeps, PltInfo#iplt_info{warning_map=PLTWarningMap}),
    handle_return_and_print(NewState, PLTWarningMap, true).

handle_return_and_print(State, AllWarningsMap, OutputPltInUse) ->
    WarningModules = State#incremental_state.warning_modules,
    WarningsToReport =
        case WarningModules =:= [] of
            true ->
                [Warn || Mod <- maps:keys(AllWarningsMap), Warn <- maps:get(Mod, AllWarningsMap, [])];
            false ->
                [Warn || Mod <- WarningModules, Warn <- maps:get(Mod, AllWarningsMap, [])]
            end,
    RetValue =
        case WarningsToReport =:= [] of
                true -> ?RET_NOTHING_SUSPICIOUS;
                false -> ?RET_DISCREPANCIES
        end,
    case State#incremental_state.erlang_mode of
        false ->
            #incremental_state{
                output = Output,
                output_format = Format,
                filename_opt = FOpt,
                indent_opt = IOpt,
                error_location = EOpt} = State,
            print_warnings(WarningsToReport, Output, Format, FOpt, IOpt, EOpt),
            maybe_close_output_file(State, OutputPltInUse),
            {RetValue, []};
        true ->
            {RetValue, set_warning_id(process_warnings(WarningsToReport),
                                      State#incremental_state.error_location)}
    end.

return_existing_errors(Opts, PltWarnings) ->
    State = init_output(Opts),
    State1 = State#incremental_state{erlang_mode = Opts#options.erlang_mode},
    State2 = State1#incremental_state{warning_modules = get_warning_modules_from_opts(Opts)},
    ModulesAnalyzed = [], % No modules analyzed - we read straight from the cache
    {handle_return_and_print(State2, PltWarnings, false), ModulesAnalyzed}.

unknown_warnings_by_module(#incremental_state{legal_warnings = LegalWarnings, external_calls=Calls, external_types=Types}) ->
    case ordsets:is_element(?WARN_UNKNOWN, LegalWarnings) of
        true ->
            unknown_functions(Calls) ++ unknown_types(Types);
        false -> []
    end.

unknown_functions(Calls) ->
    [{Mod, {?WARN_UNKNOWN, WarningInfo, {unknown_function, MFA}}} || {MFA, WarningInfo = {_, _, {Mod, _, _}}} <- Calls].

unknown_types(Types) ->
    [{Mod, {?WARN_UNKNOWN, WarningInfo, {unknown_type, MFA}}} ||
        {MFA, WarningInfo = {_, _, {Mod, _, _}}} <- Types].

set_warning_id(Warnings, EOpt) ->
    lists:map(fun({Tag, {File, Location, _MorMFA}, Msg}) ->
        {Tag, {File, set_location(Location, EOpt)}, Msg}
    end, Warnings).

set_location({Line, _}, line) ->
    Line;
set_location(Location, _EOpt) ->
    Location.

print_warnings([], _, _, _, _, _) ->
    ok;
print_warnings(Warnings, Output, Format, FOpt, IOpt, EOpt) ->
    PrWarnings = process_warnings(Warnings),
    case PrWarnings of
        [] -> ok;
        [_|_] ->
            PrWarningsId = set_warning_id(PrWarnings, EOpt),
            S = case Format of
                formatted ->
                    Opts = [{filename_opt, FOpt},
                            {indent_opt, IOpt},
                            {error_location, EOpt}],
                    [dialyzer:format_warning(W, Opts) || W <- PrWarningsId];
                raw ->
                    [io_lib:format("~tp. \n", [W]) ||
                        W <- set_warning_id(PrWarningsId, EOpt)]
        end,
            io:format(Output, "\n~ts", [S])
    end.

-spec process_warnings([raw_warning()]) -> [raw_warning()].

process_warnings(Warnings) ->
    Warnings1 = lists:keysort(3, Warnings), %% First sort on Warning
    Warnings2 = lists:keysort(2, Warnings1), %% Sort on file/line (and m/mfa..)
    remove_duplicate_warnings(Warnings2, []).

remove_duplicate_warnings([Duplicate, Duplicate|Left], Acc) ->
    remove_duplicate_warnings([Duplicate|Left], Acc);
remove_duplicate_warnings([NotDuplicate|Left], Acc) ->
    remove_duplicate_warnings(Left, [NotDuplicate|Acc]);
remove_duplicate_warnings([], Acc) ->
    lists:reverse(Acc).

get_files_from_opts(Options) ->
    Files1 = add_files(Options#options.files),
    Files2 = add_files_rec(Options#options.files_rec),
    ordsets:union(Files1, Files2).

get_warning_modules_from_opts(Options) ->
    Files1 = add_files(Options#options.warning_files),
    Files2 = add_files_rec(Options#options.warning_files_rec),
    [path_to_mod(File) || File <- ordsets:union(Files1, Files2)].

add_files_rec(Files) ->
    add_files(Files, true).

add_files(Files) ->
    add_files(Files, false).

add_files(Files, Rec) ->
    Files1 = [filename:absname(F) || F <- Files],
    Files2 = ordsets:from_list(Files1),
    Dirs = ordsets:filter(fun(X) -> filelib:is_dir(X) end, Files2),
    Files3 = ordsets:subtract(Files2, Dirs),
    Extension = ".beam",
    Fun = add_file_fun(Extension),
    lists:foldl(
        fun(Dir, Acc) ->
          filelib:fold_files(Dir, Extension, Rec, Fun, Acc)
        end,
        Files3,
        Dirs
    ).

add_file_fun(Extension) ->
    fun(File, AccFiles) ->
        case filename:extension(File) =:= Extension of
            true ->
                AbsName = filename:absname(File),
                ordsets:add_element(AbsName, AccFiles);
            false -> AccFiles
        end
    end.

-spec start_analysis(#incremental_state{}, #analysis{}) -> #incremental_state{}.

start_analysis(State, Analysis) ->
    Self = self(),
    LegalWarnings = State#incremental_state.legal_warnings,
    Fun = fun() ->
        dialyzer_analysis_callgraph:start(Self, LegalWarnings, Analysis)
    end,
    BackendPid = spawn_link(Fun),
    State#incremental_state{backend_pid = BackendPid}.
