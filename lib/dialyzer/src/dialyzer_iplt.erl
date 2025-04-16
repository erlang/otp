%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2004-2010 held by the authors. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_iplt.erl
%%% Description : Interface to display information in the incremental
%%%               persistent lookup tables stored in files.
%%%               Incremental PLTs index files by module name, rather
%%%               than absolute path, for portability, and track warnings
%%%               generated per module, in order to cache them for later
%%%               analyses.
%%%-------------------------------------------------------------------
-module(dialyzer_iplt).
-moduledoc false.

-export([check_incremental_plt/3,
         included_modules/1,
         from_file/1,
         get_default_iplt_filename/0,
         merge_warnings/3,
         plt_and_info_from_file/1,
         to_file/4,
         is_iplt/1,
         to_file_custom_vsn/6 % Used for testing certain kinds of backwards compatibility
        ]).

%% Debug utilities
-export([pp_non_returning/0, pp_mod/1]).

-export_type([module_md5/0, warning_map/0]).

-include_lib("stdlib/include/ms_transform.hrl").

%%----------------------------------------------------------------------

-type deep_string() :: string() | [deep_string()].

%%----------------------------------------------------------------------

-record(incremental_data, {mod_deps :: dialyzer_callgraph:mod_deps(),
                           warning_map = none :: warning_map(),
                           legal_warnings = none  :: none | dial_warn_tags()}).

-include("dialyzer.hrl").

-type warning_map() :: none | #{module() := [raw_warning()]}.
-type module_md5() :: {module(), binary()}.
-type module_file_path_lookup() :: #{module() => file:filename()}.

-record(ifile_plt,
        {version = ""            :: string(),
         module_md5_list = []                 :: [module_md5()],
         info = term_to_binary(#{})           :: binary(), %% encoded map()
         contracts = term_to_binary(#{})      :: binary(), %% encoded map()
         callbacks = term_to_binary(#{})      :: binary(), %% encoded map()
         types = term_to_binary(#{})          :: binary(), %% encoded map()
         exported_types = term_to_binary(#{}) :: binary(), %% encoded sets:set()
         incremental_data = term_to_binary(#incremental_data{}) :: #incremental_data{} | binary(), %% encoded #incremental_data{}
         implementation_md5 = []              :: [module_md5()]}).

%%----------------------------------------------------------------------


-spec get_default_iplt_filename() -> file:filename().

get_default_iplt_filename() ->
  case os:getenv("DIALYZER_IPLT") of
    false ->
      CacheDir = filename:basedir(user_cache, "erlang"),
      filename:join(CacheDir, ".dialyzer_iplt");
    UserSpecPlt -> UserSpecPlt
  end.

-spec plt_and_info_from_file(file:filename()) -> {dialyzer_plt:plt(), #iplt_info{}}.

plt_and_info_from_file(FileName) ->
  from_file(FileName, true).

-spec from_file(file:filename()) -> dialyzer_plt:plt().

from_file(FileName) ->
  from_file(FileName, false).

from_file(FileName, ReturnInfo) ->
  Plt = dialyzer_plt:new(),
  Fun = fun() -> from_file1(Plt, FileName, ReturnInfo) end,
  case subproc(Fun) of
    {ok, Return} ->
      Return;
    {error, Msg} ->
      dialyzer_plt:delete(Plt),
      plt_error(Msg)
  end.

from_file1(Plt, FileName, ReturnInfo) ->
  case get_record_from_file(FileName) of
    {ok, Rec} ->
      case check_version(Rec) of
        error ->
          Msg = io_lib:format("Old IPLT file ~ts\n", [FileName]),
          {error, Msg};
        ok ->
          #ifile_plt{info = CompressedInfo,
                     contracts = CompressedContracts,
                     callbacks = CompressedCallbacks,
                     types = CompressedTypes,
                     exported_types = CompressedExpTypes} = Rec,
          FileInfo = binary_to_term(CompressedInfo),
          FileContracts = binary_to_term(CompressedContracts),
          FileCallbacks = binary_to_term(CompressedCallbacks),
          FileTypes = binary_to_term(CompressedTypes),
          FileExpTypes = binary_to_term(CompressedExpTypes),
          CallbacksList = maps:to_list(FileCallbacks),
          CallbacksByModule =
            [{M, [Cb || {{M1,_,_},_} = Cb <- CallbacksList, M1 =:= M]} ||
              M <- lists:usort([M || {{M,_,_},_} <- CallbacksList])],
          #plt{info = ETSInfo,
               types = ETSTypes,
               contracts = ETSContracts,
               callbacks = ETSCallbacks,
               exported_types = ETSExpTypes} = Plt,
          [true, true, true] =
            [ets:insert(ETS, Data) ||
              {ETS, Data} <- [{ETSInfo, maps:to_list(FileInfo)},
                              {ETSTypes, FileTypes},
                              {ETSContracts, maps:to_list(FileContracts)}]],
          true = ets:insert(ETSCallbacks, CallbacksByModule),
          true = ets:insert(ETSExpTypes, [{ET} ||
                                           ET <- sets:to_list(FileExpTypes)]),
          case ReturnInfo of
            false -> {ok, Plt};
            true ->
              IncrementalData = get_incremental_data(Rec),
              PltInfo =
                #iplt_info{files = Rec#ifile_plt.module_md5_list,
                           mod_deps = IncrementalData#incremental_data.mod_deps,
                           warning_map = IncrementalData#incremental_data.warning_map,
                           legal_warnings = IncrementalData#incremental_data.legal_warnings},
              {ok, {Plt, PltInfo}}
          end
      end;
    {error, Reason} ->
      Msg = io_lib:format("Could not read IPLT file ~ts: ~p\n",
                          [FileName, Reason]),
      {error, Msg}
  end.

-spec get_incremental_data(#ifile_plt{}) -> #incremental_data{}.
get_incremental_data(#ifile_plt{incremental_data = Data}) ->
  case Data of
    CompressedData when is_binary(CompressedData) ->
      binary_to_term(CompressedData);
    UncompressedData = #incremental_data{} -> % To support older PLTs that didn't have this field compressed
      UncompressedData
  end.

-type err_rsn() :: 'not_valid' | 'no_such_file' | 'read_error'.

-spec included_modules(file:filename()) -> {'ok', [module()]}
          |  {'error', err_rsn()}.

included_modules(FileName) ->
  Fun = fun() -> included_modules1(FileName) end,
  subproc(Fun).

included_modules1(FileName) ->
  case get_record_from_file(FileName) of
    {ok, #ifile_plt{module_md5_list = Md5}} ->
      {ok, [ModuleName || {ModuleName, _} <- Md5]};
    {error, _What} = Error ->
      Error
  end.

check_version(#ifile_plt{version = ?VSN, implementation_md5 = ImplMd5}) ->
  case compute_new_md5(ImplMd5, [], [], implementation_module_paths()) of
    ok -> ok;
    {differ, _, _} -> error;
    {error, _} -> error
  end;
check_version(#ifile_plt{}) -> error.

get_record_from_file(FileName) ->
  case file:read_file(FileName) of
    {ok, Bin} ->
      try binary_to_term(Bin) of
        #ifile_plt{} = FilePLT -> {ok, FilePLT};
        _ -> {error, not_valid}
      catch
        _:_ -> {error, not_valid}
      end;
    {error, enoent} ->
      {error, no_such_file};
    {error, _} ->
      {error, read_error}
  end.

-spec is_iplt(file:filename()) -> boolean().
is_iplt(FileName) ->
  case get_record_from_file(FileName) of
    {ok, _} -> true;
    {error, _} -> false
  end.

-spec to_file(file:filename(), dialyzer_plt:plt(), dialyzer_callgraph:mod_deps(), #iplt_info{}) -> 'ok'.

%% Write the PLT to file, and deletes the PLT.
to_file(FileName, Plt, ModDeps, PLTInfo) ->
  Fun = fun() -> to_file1(FileName, Plt, ModDeps, PLTInfo) end,
  Return = subproc(Fun),
  dialyzer_plt:delete(Plt),
  case Return of
    ok -> ok;
    {error, Msg} -> plt_error(Msg)
  end.

to_file1(FileName, Plt, ModDeps, PltInfo) ->
  to_file_custom_vsn(FileName, Plt, ModDeps, PltInfo, none, none).

-spec to_file_custom_vsn(
        file:filename(),
        dialyzer_plt:plt(),
        dialyzer_callgraph:mod_deps(),
        #iplt_info{},
        none | string(),
        none | [module_md5()]) -> 'ok'.

to_file_custom_vsn(FileName, Plt, ModDeps, PltInfo, none, ImplMd5) ->
  to_file_custom_vsn(FileName, Plt, ModDeps, PltInfo, ?VSN, ImplMd5);

to_file_custom_vsn(FileName, Plt, ModDeps, PltInfo, Vsn, none) ->
  to_file_custom_vsn(FileName, Plt, ModDeps, PltInfo, Vsn, compute_implementation_md5());

to_file_custom_vsn(
  FileName,
  #plt{info = ETSInfo, types = ETSTypes, contracts = ETSContracts,
       callbacks = ETSCallbacks, exported_types = ETSExpTypes},
  NewModDeps,
  #iplt_info{files = MD5, mod_deps = OldModDeps, warning_map=NewWarningMap, legal_warnings=LegalWarnings},
  Vsn,
  ImplMd5) ->
  CombinedModDeps = dict:merge(fun(_Key, OldVal, NewVal) ->
                                   ordsets:union(OldVal, NewVal)
                               end,
                               OldModDeps, NewModDeps),
  IncrementalData =
    #incremental_data{mod_deps=CombinedModDeps, warning_map=NewWarningMap, legal_warnings=LegalWarnings},
  Callbacks =
    #{K => V ||
      {_M, Cbs} <- dialyzer_utils:ets_tab2list(ETSCallbacks),
      {K, V} <- Cbs},
  Info = maps:from_list(dialyzer_utils:ets_tab2list(ETSInfo)),
  Types = dialyzer_utils:ets_tab2list(ETSTypes),
  Contracts = maps:from_list(dialyzer_utils:ets_tab2list(ETSContracts)),
  ExpTypes = sets:from_list([E || {E} <- dialyzer_utils:ets_tab2list(ETSExpTypes)]),
  Record = #ifile_plt{version = Vsn,
                      module_md5_list = MD5,
                      info = term_to_binary(Info, [{compressed,9}]),
                      contracts = term_to_binary(Contracts, [{compressed,9}]),
                      callbacks = term_to_binary(Callbacks, [{compressed,9}]),
                      types = term_to_binary(Types, [{compressed,9}]),
                      exported_types = term_to_binary(ExpTypes, [{compressed,9}]),
                      incremental_data = term_to_binary(IncrementalData, [{compressed,9}]),
                      implementation_md5 = ImplMd5},
  Bin = term_to_binary(Record),
  case file:write_file(FileName, Bin) of
    ok -> ok;
    {error, Reason} ->
      Msg = io_lib:format("Could not write IPLT file ~ts: ~w\n",
                          [FileName, Reason]),
      {error, Msg}
  end.

-spec merge_warnings(none | [raw_warning], [{module(), raw_warning()}], none | warning_map()) -> none | warning_map().
merge_warnings(none, _, OldWarningMap) -> OldWarningMap;
merge_warnings(NewWarnings, UnknownWarnings, none) ->
  convert_to_warning_map(NewWarnings, UnknownWarnings);
merge_warnings(NewWarnings, UnknownWarnings, OldWarningMap) ->
  maps:merge(convert_to_warning_map(NewWarnings, UnknownWarnings), OldWarningMap).

convert_to_warning_map(WarningList, UnknownWarnings) ->
  Temp = lists:foldl(
           fun({_, {_, _, MorMFA}, _} = Warn, Acc) ->
               Update = fun(Old) -> [Warn|Old] end,
               maps:update_with(get_module(MorMFA), Update, [Warn], Acc)
           end,
           #{},
           WarningList),
  lists:foldl(fun({M, Warn}, Acc) ->
                  Update = fun(Old) -> [Warn|Old] end,
                  maps:update_with(M, Update, [Warn], Acc)
              end,
              Temp,
              UnknownWarnings).

get_module({M,_F,_A}) -> M;
get_module(M) -> M.

-type md5_diff()    :: [{'differ', atom()} | {'removed', atom()}].
-type check_error() :: err_rsn() | {'no_file_to_remove', file:filename()}.

-spec check_incremental_plt(file:filename(), #options{}, [file:filename()]) ->
        {'ok', #iplt_info{}, module_file_path_lookup()} |
        {'old_version', [module_md5()], module_file_path_lookup()} |
        {'new_file', [module_md5()], module_file_path_lookup()} |
        {'differ', [module_md5()], md5_diff(), dialyzer_callgraph:mod_deps(), warning_map(), module_file_path_lookup()} |
        {'legal_warnings_changed', [module_md5()], module_file_path_lookup()} |
        {'error', check_error()}.
check_incremental_plt(FileName, Opts, PltFiles) ->
  Fun = fun() -> check_incremental_plt1(FileName, Opts, PltFiles) end,
  subproc(Fun).

check_incremental_plt1(FileName, Opts, PltFiles) ->
  PltModulePathLookup = #{beam_file_to_module(PltFile) => PltFile || PltFile <- PltFiles},
  case get_record_from_file(FileName) of
    {ok, #ifile_plt{module_md5_list = Md5} = Rec} ->
      {RemoveModules, AddModules} = find_files_to_remove_and_add(Md5, maps:keys(PltModulePathLookup)),
      IncrementalData = get_incremental_data(Rec),
      PltLegalWarnings = IncrementalData#incremental_data.legal_warnings,
      LegalWarnings = Opts#options.legal_warnings,
      LegalWarningsMatch = PltLegalWarnings /= none andalso lists:usort(PltLegalWarnings) =:= lists:usort(LegalWarnings),
      case check_version_and_compute_md5(Rec, RemoveModules, AddModules, PltModulePathLookup) of
        ok when not LegalWarningsMatch ->
          {legal_warnings_changed, Md5, PltModulePathLookup};
        {differ, NewMd5, _, _, _} when not LegalWarningsMatch ->
          {legal_warnings_changed, NewMd5, PltModulePathLookup};
        ok ->
          {ok, #iplt_info{files = Md5,
                          mod_deps = IncrementalData#incremental_data.mod_deps,
                          warning_map = IncrementalData#incremental_data.warning_map,
                          legal_warnings = IncrementalData#incremental_data.legal_warnings},
           PltModulePathLookup};
        {old_version, Md5} ->
          {old_version, Md5};
        {differ, NewMd5, DiffMd5, ModDeps, _} ->
          {differ, NewMd5, DiffMd5, ModDeps, IncrementalData#incremental_data.warning_map, PltModulePathLookup};
        {old_version, NewMd5} ->
          {old_version, NewMd5, PltModulePathLookup};
        {error, Error} ->
          {error, Error}
      end;
    {error, no_such_file} ->
      {new_file, compute_md5_from_files(PltModulePathLookup), PltModulePathLookup};
    Error -> Error
  end.

find_files_to_remove_and_add(Md5, PltModules) ->
  OldPltFiles = gb_sets:from_list([Name || {Name,_Md5Bin} <- Md5]),
  NewPltFiles = gb_sets:from_list(PltModules),
  {gb_sets:to_list(gb_sets:subtract(OldPltFiles, NewPltFiles)),
   gb_sets:to_list(gb_sets:subtract(NewPltFiles, OldPltFiles))}.

check_version_and_compute_md5(Rec, RemoveFiles, AddFiles, ModuleToPathLookup) ->
  Md5 = Rec#ifile_plt.module_md5_list,
  case check_version(Rec) of
    ok ->
      case compute_new_md5(Md5, RemoveFiles, AddFiles, ModuleToPathLookup) of
        ok -> ok;
        {differ, NewMd5, DiffMd5} ->
          IncrementalData = get_incremental_data(Rec),
          {differ,
           NewMd5,
           DiffMd5,
           IncrementalData#incremental_data.mod_deps,
           IncrementalData#incremental_data.warning_map};
        {error, _What} = Err -> Err
      end;
    error ->
      case compute_new_md5(Md5, RemoveFiles, AddFiles, ModuleToPathLookup) of
        ok -> {old_version, Md5};
        {differ, NewMd5, _DiffMd5} -> {old_version, NewMd5};
        {error, _What} = Err -> Err
      end
  end.

compute_new_md5(Md5, [], [], ModuleToPathLookup) ->
  compute_new_md5_1(Md5, [], ModuleToPathLookup);
compute_new_md5(Md5, RemoveFiles0, AddFiles0, ModuleToPathLookup) ->
  %% Assume that files are first removed and then added. Files that
  %% are both removed and added will be checked for consistency in the
  %% normal way.
  RemoveFiles = RemoveFiles0 -- AddFiles0,
  AddFiles = AddFiles0 -- RemoveFiles0,
  InitDiffList = init_diff_list(RemoveFiles, AddFiles),
  case init_md5_list(Md5, RemoveFiles, AddFiles) of
    {ok, NewMd5} -> compute_new_md5_1(NewMd5, InitDiffList, ModuleToPathLookup);
    {error, _What} = Error -> Error
  end.

compute_new_md5_1(Entries, InitDiffs, ModuleToPathLookup) ->
  Modules = [Module || {Module, _Md5} <- Entries],
  ExistingHashes = [Md5 || {_Module, Md5} <- Entries],
  Files = [maps:get(Module, ModuleToPathLookup) || Module <- Modules],
  NewHashes = dialyzer_utils:p_map(fun compute_md5_from_file/1, Files),
  Diffs = InitDiffs ++
    [{differ, Module} ||
      Module <- Modules &&
        BeforeHash <- ExistingHashes &&
        AfterHash <- NewHashes,
      BeforeHash =/= AfterHash],
  case Diffs of
    [] ->
      ok;
    _ ->
      ModuleHashes = lists:zip(Modules, NewHashes),
      {differ, lists:sort(ModuleHashes), Diffs}
  end.

-spec implementation_module_paths() -> module_file_path_lookup().
implementation_module_paths() ->
  Dir = code:lib_dir(dialyzer),
  Files1 = ["erl_bif_types.beam", "erl_types.beam"],
  Files2 = [filename:join([Dir, "ebin", F]) || F <- Files1],
  #{beam_file_to_module(File) => File || File <- Files2}.

-spec compute_implementation_md5() -> [module_md5()].

compute_implementation_md5() ->
  Modules = implementation_module_paths(),
  compute_md5_from_files(Modules).

-spec compute_md5_from_files(module_file_path_lookup()) -> [module_md5()].

compute_md5_from_files(ModuleToPathLookup) ->
  {Modules,Files} = lists:unzip(maps:to_list(ModuleToPathLookup)),
  Hashes = dialyzer_utils:p_map(fun compute_md5_from_file/1, Files),
  lists:keysort(1, lists:zip(Modules, Hashes)).

compute_md5_from_file(File) ->
  case beam_lib:chunks(File, [debug_info]) of
    {ok, {ModuleName, [{debug_info, {debug_info_v1, Backend, Data}}]}} ->
      %% We cannot use beam_lib:md5 because it includes
      %% non-portable or otherwise irrelvant data that would
      %% cause the PLT to be invalidated needlessly too often
      case Backend:debug_info(erlang_v1, ModuleName, Data, []) of
        {ok, Code} ->
          StabilisedCode = lists:filtermap(fun (Form) -> make_stable(ModuleName, Form) end, Code),
          StabilisedCodeBin = erlang:term_to_binary(StabilisedCode),
          erlang:md5(StabilisedCodeBin);
        {error, Reason} ->
          Msg = io_lib:format("Could not compute MD5 for .beam (debug_info error) - did you forget to set the debug_info compilation option? ~ts ~tw\n", [File, Reason]),
          throw({dialyzer_error, Msg})
      end;
    {ok, {_, [{debug_info, no_debug_info}]}} ->
      Msg = io_lib:format("Could not compute MD5 for .beam (debug_info missing): ~ts\n", [File]),
      throw({dialyzer_error, Msg});
    {error, beam_lib, {file_error, _, enoent}} ->
      Msg = io_lib:format("File not found: ~ts\n", [File]),
      plt_error(Msg);
    {error, beam_lib, _} ->
      Msg = io_lib:format("Could not compute MD5 for .beam: ~ts\n", [File]),
      plt_error(Msg)
  end.

%% Absolute paths in -file attributes make beam file hashes brittle, since the
%% same beam built elsewhere will contain a different absolute path, despite
%% being semantically identical.
%%
%% Here, we replace the full path with just the basename. This is very similar
%% to the effect of the +deterministic option, by by doing this rewriting here,
%% we gain some of those determinism benefits even when the build is not run
%% with +deterministic.
make_stable(_, {attribute, Anno, file, {SrcFilePath, Line}}) ->
  {true, {attribute, Anno, file, {filename:basename(SrcFilePath), Line}}};

make_stable(_, Attr) ->
  {true, Attr}.

init_diff_list(RemoveFiles, AddFiles) ->
  RemoveSet0 = sets:from_list([beam_file_to_module(F) || F <- RemoveFiles]),
  AddSet0 = sets:from_list([beam_file_to_module(F) || F <- AddFiles]),
  DiffSet = sets:intersection(AddSet0, RemoveSet0),
  RemoveSet = sets:subtract(RemoveSet0, DiffSet),
  %% Added files and diff files will appear as diff files from the md5 check.
  [{removed, F} || F <- sets:to_list(RemoveSet)].

init_md5_list(Md5, RemoveFiles, AddFiles) ->
  Mods = [{remove, beam_file_to_module(F)} || F <- RemoveFiles] ++ [{add, beam_file_to_module(F)} || F <- AddFiles],
  DiffMods = lists:keysort(2, Mods),
  Md5Sorted = lists:keysort(1, Md5),
  init_md5_list_1(Md5Sorted, DiffMods, []).

init_md5_list_1([{Mod, _Md5}|Md5Left], [{remove, Mod}|DiffLeft], Acc) ->
  init_md5_list_1(Md5Left, DiffLeft, Acc);
init_md5_list_1([{Mod, _Md5} = Entry|Md5Left], [{add, Mod}|DiffLeft], Acc) ->
  init_md5_list_1(Md5Left, DiffLeft, [Entry|Acc]);
init_md5_list_1([{Mod1, _Md5} = Entry|Md5Left] = Md5List,
                [{Tag, Mod2}|DiffLeft] = DiffList, Acc) ->
  case Mod1 < Mod2 of
    true -> init_md5_list_1(Md5Left, DiffList, [Entry|Acc]);
    false ->
      %% Just an assert.
      true = Mod1 > Mod2,
      case Tag of
        add -> init_md5_list_1(Md5List, DiffLeft, [{Mod2, <<>>}|Acc]);
        remove -> {error, {no_file_to_remove, Mod2}}
      end
  end;
init_md5_list_1([], DiffList, Acc) ->
  AddMods = [{M, <<>>} || {add, M} <- DiffList],
  {ok, lists:reverse(Acc, AddMods)};
init_md5_list_1(Md5List, [], Acc) ->
  {ok, lists:reverse(Acc, Md5List)}.


subproc(Fun) ->
  F = fun() ->
          exit(try Fun()
               catch throw:T ->
                   {thrown, T}
               end)
      end,
  {Pid, Ref} = erlang:spawn_monitor(F),
  receive {'DOWN', Ref, process, Pid, Return} ->
      case Return of
        {thrown, T} -> throw(T);
        _ -> Return
      end
  end.


beam_file_to_module(Filename) ->
  list_to_atom(filename:basename(Filename, ".beam")).


-spec plt_error(deep_string()) -> no_return().

plt_error(Msg) ->
  throw({dialyzer_error, lists:flatten(Msg)}).


%%---------------------------------------------------------------------------
%% Debug utilities.

-spec pp_non_returning() -> 'ok'.

pp_non_returning() ->
  PltFile = get_default_iplt_filename(),
  Plt = from_file(PltFile),
  List = ets:tab2list(Plt#plt.info),
  Unit = [{MFA, erl_types:t_fun(Args, Ret)} || {MFA, {Ret, Args}} <- List,
                                               erl_types:t_is_unit(Ret)],
  None = [{MFA, erl_types:t_fun(Args, Ret)} || {MFA, {Ret, Args}} <- List,
                                               erl_types:t_is_none(Ret)],
  io:format("=========================================\n"),
  io:format("=                Loops                  =\n"),
  io:format("=========================================\n\n"),
  lists:foreach(fun({{M, F, _}, Type}) ->
                    io:format("~w:~tw~ts.\n",
                              [M, F, dialyzer_utils:format_sig(Type)])
                end, lists:sort(Unit)),
  io:format("\n"),
  io:format("=========================================\n"),
  io:format("=                Errors                 =\n"),
  io:format("=========================================\n\n"),
  lists:foreach(fun({{M, F, _}, Type}) ->
                    io:format("~w:~w~s.\n",
                              [M, F, dialyzer_utils:format_sig(Type)])
                end, lists:sort(None)),
  dialyzer_plt:delete(Plt).

-spec pp_mod(atom()) -> 'ok'.

pp_mod(Mod) when is_atom(Mod) ->
  PltFile = get_default_iplt_filename(),
  Plt = from_file(PltFile),
  case dialyzer_plt:lookup_module(Plt, Mod) of
    {value, List} ->
      lists:foreach(fun({{_, F, _}, Ret, Args}) ->
                        T = erl_types:t_fun(Args, Ret),
                        S = dialyzer_utils:format_sig(T),
                        io:format("-spec ~tw~ts.\n", [F, S])
                    end, lists:sort(List));
    none ->
      io:format("dialyzer: Found no module named '~s' in the IPLT\n", [Mod])
  end,
  dialyzer_plt:delete(Plt).
