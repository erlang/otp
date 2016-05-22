%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_analysis_callgraph.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description :
%%%
%%% Created :  5 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------

-module(dialyzer_analysis_callgraph).

-export([start/3]).

%%% Export for dialyzer_coordinator...
-export([compile_init_result/0,
	 add_to_result/4]).
%%% ... and export for dialyzer_worker.
-export([start_compilation/2,
	 continue_compilation/2]).

-export_type([compile_init_data/0,
              one_file_mid_error/0,
              one_file_result_ok/0,
              compile_result/0]).

-include("dialyzer.hrl").

-record(analysis_state,
	{
	  codeserver                    :: dialyzer_codeserver:codeserver(),
	  analysis_type  = succ_typings :: anal_type(),
	  defines        = []           :: [dial_define()],
	  doc_plt                       :: dialyzer_plt:plt(),
	  include_dirs   = []           :: [file:filename()],
	  parent                        :: pid(),
          legal_warnings                :: % command line options
                                           [dial_warn_tag()],
	  plt                           :: dialyzer_plt:plt(),
	  start_from     = byte_code    :: start_from(),
	  use_contracts  = true         :: boolean(),
	  timing_server                 :: dialyzer_timing:timing_server(),
          solvers                       :: [solver()]
	 }).

-record(server_state,
        {
          parent :: pid()
         }).

%%--------------------------------------------------------------------
%% Main
%%--------------------------------------------------------------------

-spec start(pid(), [dial_warn_tag()], #analysis{}) -> 'ok'.

start(Parent, LegalWarnings, Analysis) ->
  TimingServer = dialyzer_timing:init(Analysis#analysis.timing),
  RacesOn = ordsets:is_element(?WARN_RACE_CONDITION, LegalWarnings),
  Analysis0 =
    Analysis#analysis{race_detection = RacesOn, timing_server = TimingServer},
  Analysis1 = expand_files(Analysis0),
  Analysis2 = run_analysis(Analysis1, LegalWarnings),
  State = #server_state{parent = Parent},
  loop(State, Analysis2, none),
  dialyzer_timing:stop(TimingServer).

run_analysis(Analysis, LegalWarnings) ->
  Self = self(),
  Fun = fun() -> analysis_start(Self, Analysis, LegalWarnings) end,
  Analysis#analysis{analysis_pid = spawn_link(Fun)}.

loop(#server_state{parent = Parent} = State,
     #analysis{analysis_pid = AnalPid} = Analysis, ExtCalls) ->
  receive
    {AnalPid, log, LogMsg} ->
      send_log(Parent, LogMsg),
      loop(State, Analysis, ExtCalls);
    {AnalPid, warnings, Warnings} ->
      send_warnings(Parent, Warnings),
      loop(State, Analysis, ExtCalls);
    {AnalPid, cserver, CServer, Plt} ->
      send_codeserver_plt(Parent, CServer, Plt),
      loop(State, Analysis, ExtCalls);
    {AnalPid, done, Plt, DocPlt} ->
      send_ext_calls(Parent, ExtCalls),
      send_analysis_done(Parent, Plt, DocPlt);
    {AnalPid, ext_calls, NewExtCalls} ->
      loop(State, Analysis, NewExtCalls);
    {AnalPid, ext_types, ExtTypes} ->
      send_ext_types(Parent, ExtTypes),
      loop(State, Analysis, ExtCalls);
    {AnalPid, mod_deps, ModDeps} ->
      send_mod_deps(Parent, ModDeps),
      loop(State, Analysis, ExtCalls);
    {Parent, stop} ->
      exit(AnalPid, kill),
      ok
  end.

%%--------------------------------------------------------------------
%% The Analysis
%%--------------------------------------------------------------------

analysis_start(Parent, Analysis, LegalWarnings) ->
  CServer = dialyzer_codeserver:new(),
  Plt = Analysis#analysis.plt,
  State = #analysis_state{codeserver = CServer,
			  analysis_type = Analysis#analysis.type,
			  defines = Analysis#analysis.defines,
			  doc_plt = Analysis#analysis.doc_plt,
			  include_dirs = Analysis#analysis.include_dirs,
			  plt = Plt,
			  parent = Parent,
                          legal_warnings = LegalWarnings,
			  start_from = Analysis#analysis.start_from,
			  use_contracts = Analysis#analysis.use_contracts,
			  timing_server = Analysis#analysis.timing_server,
                          solvers = Analysis#analysis.solvers
			 },
  Files = ordsets:from_list(Analysis#analysis.files),
  {Callgraph, TmpCServer0} = compile_and_store(Files, State),
  %% Remote type postprocessing
  NewCServer =
    try
      NewRecords = dialyzer_codeserver:get_temp_records(TmpCServer0),
      NewExpTypes = dialyzer_codeserver:get_temp_exported_types(TmpCServer0),
      OldRecords = dialyzer_plt:get_types(Plt),
      OldExpTypes0 = dialyzer_plt:get_exported_types(Plt),
      MergedRecords = dialyzer_utils:merge_records(NewRecords, OldRecords),
      RemMods =
        [case Analysis#analysis.start_from of
           byte_code -> list_to_atom(filename:basename(F, ".beam"));
           src_code -> list_to_atom(filename:basename(F, ".erl"))
         end || F <- Files],
      OldExpTypes1 = dialyzer_utils:sets_filter(RemMods, OldExpTypes0),
      MergedExpTypes = sets:union(NewExpTypes, OldExpTypes1),
      TmpCServer1 = dialyzer_codeserver:set_temp_records(MergedRecords, TmpCServer0),
      TmpCServer2 =
        dialyzer_codeserver:finalize_exported_types(MergedExpTypes, TmpCServer1),
      ?timing(State#analysis_state.timing_server, "remote",
              begin
                TmpCServer3 =
                  dialyzer_utils:process_record_remote_types(TmpCServer2),
                dialyzer_contracts:process_contract_remote_types(TmpCServer3)
              end)
    catch
      throw:{error, _ErrorMsg} = Error -> exit(Error)
    end,
  NewPlt0 = dialyzer_plt:insert_types(Plt, dialyzer_codeserver:get_records(NewCServer)),
  ExpTypes = dialyzer_codeserver:get_exported_types(NewCServer),
  NewPlt1 = dialyzer_plt:insert_exported_types(NewPlt0, ExpTypes),
  State0 = State#analysis_state{plt = NewPlt1},
  dump_callgraph(Callgraph, State0, Analysis),
  State1 = State0#analysis_state{codeserver = NewCServer},
  %% Remove all old versions of the files being analyzed
  AllNodes = dialyzer_callgraph:all_nodes(Callgraph),
  Plt1 = dialyzer_plt:delete_list(NewPlt1, AllNodes),
  Exports = dialyzer_codeserver:get_exports(NewCServer),
  NewCallgraph =
    case Analysis#analysis.race_detection of
      true -> dialyzer_callgraph:put_race_detection(true, Callgraph);
      false -> Callgraph
    end,
  State2 = analyze_callgraph(NewCallgraph, State1#analysis_state{plt = Plt1}),
  dialyzer_callgraph:dispose_race_server(NewCallgraph),
  rcv_and_send_ext_types(Parent),
  NonExports = sets:subtract(sets:from_list(AllNodes), Exports),
  NonExportsList = sets:to_list(NonExports),
  Plt2 = dialyzer_plt:delete_list(State2#analysis_state.plt, NonExportsList),
  send_codeserver_plt(Parent, CServer, State2#analysis_state.plt),
  send_analysis_done(Parent, Plt2, State2#analysis_state.doc_plt).

analyze_callgraph(Callgraph, #analysis_state{codeserver = Codeserver,
					     doc_plt = DocPlt,
					     timing_server = TimingServer,
					     parent = Parent,
                                             solvers = Solvers} = State) ->
  Plt = dialyzer_plt:insert_callbacks(State#analysis_state.plt, Codeserver),
  {NewPlt, NewDocPlt} =
    case State#analysis_state.analysis_type of
      plt_build ->
	NewPlt0 =
	  dialyzer_succ_typings:analyze_callgraph(Callgraph, Plt, Codeserver,
						  TimingServer, Solvers, Parent),
	{NewPlt0, DocPlt};
      succ_typings ->
	{Warnings, NewPlt0, NewDocPlt0} =
	  dialyzer_succ_typings:get_warnings(Callgraph, Plt, DocPlt, Codeserver,
					     TimingServer, Solvers, Parent),
        Warnings1 = filter_warnings(Warnings, Codeserver),
	send_warnings(State#analysis_state.parent, Warnings1),
	{NewPlt0, NewDocPlt0}
    end,
  dialyzer_callgraph:delete(Callgraph),
  State#analysis_state{plt = NewPlt, doc_plt = NewDocPlt}.

%%--------------------------------------------------------------------
%% Build the callgraph and fill the codeserver.
%%--------------------------------------------------------------------

-record(compile_init,{
	  callgraph                 :: dialyzer_callgraph:callgraph(),
	  codeserver                :: dialyzer_codeserver:codeserver(),
	  defines       = []        :: [dial_define()],
	  include_dirs  = []        :: [file:filename()],
	  start_from    = byte_code :: start_from(),
	  use_contracts = true      :: boolean(),
          legal_warnings            :: [dial_warn_tag()]
	 }).

make_compile_init(#analysis_state{codeserver = Codeserver,
				  defines = Defs,
				  include_dirs = Dirs,
				  use_contracts = UseContracts,
                                  legal_warnings = LegalWarnings,
				  start_from = StartFrom}, Callgraph) ->
  #compile_init{callgraph = Callgraph,
		codeserver = Codeserver,
		defines = [{d, Macro, Val} || {Macro, Val} <- Defs],
		include_dirs = [{i, D} || D <- Dirs],
		use_contracts = UseContracts,
                legal_warnings = LegalWarnings,
		start_from = StartFrom}.

compile_and_store(Files, #analysis_state{codeserver = CServer,
					 timing_server = Timing,
					 parent = Parent} = State) ->
  send_log(Parent, "Reading files and computing callgraph... "),
  {T1, _} = statistics(wall_clock),
  Callgraph = dialyzer_callgraph:new(),
  CompileInit = make_compile_init(State, Callgraph),
  %% Spawn a worker per file - where each worker calls
  %% start_compilation on its file, asks next label to coordinator and
  %% calls continue_compilation - and let coordinator aggregate
  %% results using (compile_init_result and) add_to_result.
  {{Failed, Modules}, NextLabel} =
    ?timing(Timing, "compile", _C1,
	    dialyzer_coordinator:parallel_job(compile, Files,
					      CompileInit, Timing)),
  CServer2 = dialyzer_codeserver:set_next_core_label(NextLabel, CServer),
  case Failed =:= [] of
    true ->
      ModDict =
        lists:foldl(fun(F, Dict) ->
                        ModFile = lists:last(filename:split(F)),
                        Mod = filename:basename(ModFile, ".beam"),
                        dict:append(Mod, F, Dict)
                    end,
                    dict:new(), Files),
      check_for_duplicate_modules(ModDict);
    false ->
      Msg = io_lib:format("Could not scan the following file(s):~n~s",
      			  [[Reason || {_Filename, Reason} <- Failed]]),
      exit({error, Msg})
  end,
  {T2, _} = statistics(wall_clock),
  Msg1 = io_lib:format("done in ~.2f secs\nRemoving edges... ", [(T2-T1)/1000]),
  send_log(Parent, Msg1),
  Callgraph =
    ?timing(Timing, "clean", _C2,
	    cleanup_callgraph(State, CServer2, Callgraph, Modules)),
  {T3, _} = statistics(wall_clock),
  Msg2 = io_lib:format("done in ~.2f secs\n", [(T3-T2)/1000]),
  send_log(Parent, Msg2),
  {Callgraph, CServer2}.

-opaque compile_init_data()  :: #compile_init{}.
-type error_reason()         :: string().
-opaque compile_result()     :: {[{file:filename(), error_reason()}],
                                 [module()]}.
-type one_file_mid_error()   :: {error, error_reason()}.
-opaque one_file_result_ok() :: {ok, [dialyzer_callgraph:callgraph_edge()],
                                 [mfa_or_funlbl()], module()}.
-type one_file_result()      :: one_file_mid_error() |
                                one_file_result_ok().
-type compile_mid_data()     :: {module(), cerl:cerl(),
                                 dialyzer_callgraph:callgraph(),
                                 dialyzer_codeserver:codeserver()}.

-spec compile_init_result() -> compile_result().

compile_init_result() -> {[], []}.

-spec add_to_result(file:filename(), one_file_result(), compile_result(),
		    compile_init_data()) -> compile_result().

add_to_result(File, NewData, {Failed, Mods}, InitData) ->
  case NewData of
    {error, Reason} ->
      {[{File, Reason}|Failed], Mods};
    {ok, V, E, Mod} ->
      Callgraph = InitData#compile_init.callgraph,
      dialyzer_callgraph:add_edges(E, V, Callgraph),
      {Failed, [Mod|Mods]}
  end.

-spec start_compilation(file:filename(), compile_init_data()) ->
	{error, error_reason()} |{ok, integer(), compile_mid_data()}.

start_compilation(File,
		  #compile_init{callgraph = Callgraph, codeserver = Codeserver,
				defines = Defines, include_dirs = IncludeD,
				use_contracts = UseContracts,
                                legal_warnings = LegalWarnings,
				start_from = StartFrom}) ->
  case StartFrom of
    src_code ->
      compile_src(File, IncludeD, Defines, Callgraph, Codeserver,
                  UseContracts, LegalWarnings);
    byte_code ->
      compile_byte(File, Callgraph, Codeserver, UseContracts, LegalWarnings)
  end.

cleanup_callgraph(#analysis_state{plt = InitPlt, parent = Parent,
				  codeserver = CodeServer
				 },
		  CServer, Callgraph, Modules) ->
  ModuleDeps = dialyzer_callgraph:module_deps(Callgraph),
  send_mod_deps(Parent, ModuleDeps),
  {Callgraph1, ExtCalls} = dialyzer_callgraph:remove_external(Callgraph),
  ExtCalls1 = [Call || Call = {_From, To} <- ExtCalls,
		       not dialyzer_plt:contains_mfa(InitPlt, To)],
  {BadCalls1, RealExtCalls} =
    if ExtCalls1 =:= [] -> {[], []};
       true ->
	ModuleSet = sets:from_list(Modules),
	PltModuleSet = dialyzer_plt:all_modules(InitPlt),
	AllModules = sets:union(ModuleSet, PltModuleSet),
	Pred = fun({_From, {M, _F, _A}}) -> sets:is_element(M, AllModules) end,
	lists:partition(Pred, ExtCalls1)
    end,
  NonLocalCalls = dialyzer_callgraph:non_local_calls(Callgraph1),
  BadCalls2 = [Call || Call = {_From, To} <- NonLocalCalls,
		       not dialyzer_codeserver:is_exported(To, CServer)],
  case BadCalls1 ++ BadCalls2 of
    [] -> ok;
    BadCalls -> send_bad_calls(Parent, BadCalls, CodeServer)
  end,
  if RealExtCalls =:= [] -> ok;
     true ->
      send_ext_calls(Parent, lists:usort([To || {_From, To} <- RealExtCalls]))
  end,
  Callgraph1.

compile_src(File, Includes, Defines, Callgraph, CServer, UseContracts,
            LegalWarnings) ->
  DefaultIncludes = default_includes(filename:dirname(File)),
  SrcCompOpts = dialyzer_utils:src_compiler_opts(),
  CompOpts = SrcCompOpts ++ Includes ++ Defines ++ DefaultIncludes,
  case dialyzer_utils:get_abstract_code_from_src(File, CompOpts) of
    {error, _Msg} = Error -> Error;
    {ok, AbstrCode} ->
      compile_common(File, AbstrCode, CompOpts, Callgraph, CServer,
                     UseContracts, LegalWarnings)
  end.

compile_byte(File, Callgraph, CServer, UseContracts, LegalWarnings) ->
  case dialyzer_utils:get_abstract_code_from_beam(File) of
    error ->
      {error, "  Could not get abstract code for: " ++ File ++ "\n" ++
	 "  Recompile with +debug_info or analyze starting from source code"};
    {ok, AbstrCode} ->
      compile_byte(File, AbstrCode, Callgraph, CServer, UseContracts,
                   LegalWarnings)
  end.

compile_byte(File, AbstrCode, Callgraph, CServer, UseContracts,
             LegalWarnings) ->
  case dialyzer_utils:get_compile_options_from_beam(File) of
    error ->
      {error, "  Could not get compile options for: " ++ File ++ "\n" ++
	 "  Recompile or analyze starting from source code"};
    {ok, CompOpts} ->
      compile_common(File, AbstrCode, CompOpts, Callgraph, CServer,
                     UseContracts, LegalWarnings)
  end.

compile_common(File, AbstrCode, CompOpts, Callgraph, CServer,
               UseContracts, LegalWarnings) ->
  case dialyzer_utils:get_core_from_abstract_code(AbstrCode, CompOpts) of
    error -> {error, "  Could not get core Erlang code for: " ++ File};
    {ok, Core} ->
      Mod = cerl:concrete(cerl:module_name(Core)),
      case dialyzer_utils:get_record_and_type_info(AbstrCode) of
	{error, _} = Error -> Error;
	{ok, RecInfo} ->
	  CServer1 =
	    dialyzer_codeserver:store_temp_records(Mod, RecInfo, CServer),
          MetaFunInfo =
            dialyzer_utils:get_fun_meta_info(Mod, AbstrCode, LegalWarnings),
          CServer2 =
            dialyzer_codeserver:insert_fun_meta_info(MetaFunInfo, CServer1),
	  case UseContracts of
	    true ->
	      case dialyzer_utils:get_spec_info(Mod, AbstrCode, RecInfo) of
		{error, _} = Error -> Error;
		{ok, SpecInfo, CallbackInfo} ->
		  CServer3 =
		    dialyzer_codeserver:store_temp_contracts(Mod, SpecInfo,
							     CallbackInfo,
							     CServer2),
		  store_core(Mod, Core, Callgraph, CServer3)
	      end;
	    false ->
	      store_core(Mod, Core, Callgraph, CServer2)
	  end
      end
  end.

store_core(Mod, Core, Callgraph, CServer) ->
  Exp = get_exports_from_core(Core),
  ExpTypes = get_exported_types_from_core(Core),
  CServer = dialyzer_codeserver:insert_exports(Exp, CServer),
  CServer = dialyzer_codeserver:insert_temp_exported_types(ExpTypes, CServer),
  CoreTree = cerl:from_records(Core),
  CoreSize = cerl_trees:size(CoreTree),
  {ok, CoreSize, {Mod, CoreTree, Callgraph, CServer}}.

-spec continue_compilation(integer(), compile_mid_data()) ->
                              one_file_result_ok().

continue_compilation(NextLabel, {Mod, CoreTree, Callgraph, CServer}) ->
  {LabeledTree, _NewNextLabel} = cerl_trees:label(CoreTree, NextLabel),
  LabeledCore = cerl:to_records(LabeledTree),
  store_code_and_build_callgraph(Mod, LabeledCore, Callgraph, CServer).

get_exported_types_from_core(Core) ->
  Attrs = cerl:module_attrs(Core),
  ExpTypes1 = [cerl:concrete(L2) || {L1, L2} <- Attrs, cerl:is_literal(L1),
                                    cerl:is_literal(L2),
                                    cerl:concrete(L1) =:= 'export_type'],
  ExpTypes2 = lists:flatten(ExpTypes1),
  M = cerl:atom_val(cerl:module_name(Core)),
  sets:from_list([{M, F, A} || {F, A} <- ExpTypes2]).

get_exports_from_core(Core) ->
  Tree = cerl:from_records(Core),
  Exports1 = cerl:module_exports(Tree),
  Exports2 = [cerl:var_name(V) || V <- Exports1],
  M = cerl:atom_val(cerl:module_name(Tree)),
  [{M, F, A} || {F, A} <- Exports2].

store_code_and_build_callgraph(Mod, Core, Callgraph, CServer) ->
  CoreTree = cerl:from_records(Core),
  {Vertices, Edges} = dialyzer_callgraph:scan_core_tree(CoreTree, Callgraph),
  CServer = dialyzer_codeserver:insert(Mod, CoreTree, CServer),
  {ok, Vertices, Edges, Mod}.

%%--------------------------------------------------------------------
%% Utilities
%%--------------------------------------------------------------------

expand_files(Analysis = #analysis{files = Files, start_from = StartFrom}) ->
  Ext = case StartFrom of
	  byte_code -> ".beam";
	  src_code -> ".erl"
	end,
  case expand_files(Files, Ext, []) of
    [] ->
      Msg = "No " ++ Ext ++ " files to analyze" ++
	case StartFrom of
	  byte_code -> " (no --src specified?)";
	  src_code -> ""
	end,
      exit({error, Msg});
    NewFiles ->
      Analysis#analysis{files = NewFiles}
  end.

expand_files([File|Left], Ext, FileAcc) ->
  case filelib:is_dir(File) of
    true ->
      {ok, List} = file:list_dir(File),
      NewFiles = lists:foldl(fun (X, Acc) ->
				 case filename:extension(X) =:= Ext of
				   true -> [filename:join(File, X)|Acc];
				   false -> Acc
				 end
			     end, FileAcc, List),
      expand_files(Left, Ext, NewFiles);
    false ->
      expand_files(Left, Ext, [File|FileAcc])
  end;
expand_files([], _Ext, FileAcc) ->
  FileAcc.

check_for_duplicate_modules(ModDict) ->
  Duplicates = dict:filter(fun(_, [_]) -> false;
			      (_, _Files) -> true
			   end, ModDict),
  case dict:size(Duplicates) =:= 0 of
    true ->
      ok;
    false ->
      Mods = [X || {_, X} <- dict:to_list(Duplicates)],
      Msg = io_lib:format("Duplicate modules: ~p", [Mods]),
      exit({error, Msg})
  end.

default_includes(Dir) ->
  L1 = ["..", "../incl", "../inc", "../include"],
  [{i, filename:join(Dir, X)} || X <- L1].

%%-------------------------------------------------------------------
%% Handle Messages
%%-------------------------------------------------------------------

rcv_and_send_ext_types(Parent) ->
  Self = self(),
  Self ! {Self, done},
  case rcv_ext_types(Self, []) of
    [] -> ok;
    ExtTypes ->
      Parent ! {Self, ext_types, ExtTypes},
      ok
  end.

rcv_ext_types(Self, ExtTypes) ->
  receive
    {Self, ext_types, ExtType} ->
      rcv_ext_types(Self, [ExtType|ExtTypes]);
    {Self, done} -> lists:usort(ExtTypes)
  end.

send_log(Parent, Msg) ->
  Parent ! {self(), log, Msg},
  ok.

send_warnings(_Parent, []) ->
  ok;
send_warnings(Parent, Warnings) ->
  Parent ! {self(), warnings, Warnings},
  ok.

filter_warnings(Warnings, Codeserver) ->
  [TWW || {Tag, WarningInfo, _Warning} = TWW <- Warnings,
          is_ok_fun(WarningInfo, Codeserver),
          is_ok_tag(Tag, WarningInfo, Codeserver)].

is_ok_fun({_F, _L, Module}, _Codeserver) when is_atom(Module) ->
  true;
is_ok_fun({_Filename, _Line, {_M, _F, _A} = MFA}, Codeserver) ->
  not dialyzer_utils:is_suppressed_fun(MFA, Codeserver).

is_ok_tag(Tag, {_F, _L, MorMFA}, Codeserver) ->
  not dialyzer_utils:is_suppressed_tag(MorMFA, Tag, Codeserver).
  
send_analysis_done(Parent, Plt, DocPlt) ->
  Parent ! {self(), done, Plt, DocPlt},
  ok.

send_ext_calls(_Parent, none) ->
  ok;
send_ext_calls(Parent, ExtCalls) ->
  Parent ! {self(), ext_calls, ExtCalls},
  ok.

send_ext_types(Parent, ExtTypes) ->
  Parent ! {self(), ext_types, ExtTypes},
  ok.

send_codeserver_plt(Parent, CServer, Plt ) ->
  Parent ! {self(), cserver, CServer, Plt},
  ok.

send_bad_calls(Parent, BadCalls, CodeServer) ->
  FormatedBadCalls = format_bad_calls(BadCalls, CodeServer, []),
  Warnings = filter_warnings(FormatedBadCalls, CodeServer),
  send_warnings(Parent, Warnings).

send_mod_deps(Parent, ModuleDeps) ->
  Parent ! {self(), mod_deps, ModuleDeps},
  ok.

format_bad_calls([{{_, _, _}, {_, module_info, A}}|Left], CodeServer, Acc)
  when A =:= 0; A =:= 1 ->
  format_bad_calls(Left, CodeServer, Acc);
format_bad_calls([{FromMFA, {M, F, A} = To}|Left], CodeServer, Acc) ->
  {_Var, FunCode} = dialyzer_codeserver:lookup_mfa_code(FromMFA, CodeServer),
  Msg = {call_to_missing, [M, F, A]},
  {File, Line} = find_call_file_and_line(FunCode, To),
  WarningInfo = {File, Line, FromMFA},
  NewAcc = [{?WARN_CALLGRAPH, WarningInfo, Msg}|Acc],
  format_bad_calls(Left, CodeServer, NewAcc);
format_bad_calls([], _CodeServer, Acc) ->
  Acc.

find_call_file_and_line(Tree, MFA) ->
  Fun =
    fun(SubTree, Acc) ->
	case cerl:is_c_call(SubTree) of
	  true ->
	    M = cerl:call_module(SubTree),
	    F = cerl:call_name(SubTree),
	    A = cerl:call_arity(SubTree),
	    case cerl:is_c_atom(M) andalso cerl:is_c_atom(F) of
	      true ->
		case {cerl:concrete(M), cerl:concrete(F), A} of
		  MFA ->
		    Ann = cerl:get_ann(SubTree),
		    [{get_file(Ann), get_line(Ann)}|Acc];
		  {erlang, make_fun, 3} ->
		    [CA1, CA2, CA3] = cerl:call_args(SubTree),
		    case
		      cerl:is_c_atom(CA1) andalso
		      cerl:is_c_atom(CA2) andalso
		      cerl:is_c_int(CA3)
		    of
		      true ->
			case
			  {cerl:concrete(CA1),
			   cerl:concrete(CA2),
			   cerl:concrete(CA3)}
			of
			  MFA ->
			    Ann = cerl:get_ann(SubTree),
			    [{get_file(Ann), get_line(Ann)}|Acc];
			  _ ->
			    Acc
			end;
		      false ->
			Acc
		    end;
		  _ -> Acc
		end;
	      false -> Acc
	    end;
	  false -> Acc
	end
    end,
  hd(cerl_trees:fold(Fun, [], Tree)).

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|Tail]) -> get_line(Tail);
get_line([]) -> -1.

get_file([{file, File}|_]) -> File;
get_file([_|Tail]) -> get_file(Tail).

-spec dump_callgraph(dialyzer_callgraph:callgraph(), #analysis_state{}, #analysis{}) ->
  'ok'.

dump_callgraph(_CallGraph, _State, #analysis{callgraph_file = ""}) -> ok;
dump_callgraph(CallGraph, State, #analysis{callgraph_file = File} = Analysis) ->
  Extension = filename:extension(File),
  Start_Msg = io_lib:format("Dumping the callgraph... ", []),
  send_log(State#analysis_state.parent, Start_Msg),
  {T1, _} = statistics(wall_clock),
  dump_callgraph(CallGraph, State, Analysis, Extension),
  {T2, _} = statistics(wall_clock),
  Finish_Msg = io_lib:format("done in ~2f secs\n", [(T2-T1)/1000]),
  send_log(State#analysis_state.parent, Finish_Msg),
  ok.

dump_callgraph(CallGraph, _State, #analysis{callgraph_file = File}, ".dot") ->
  dialyzer_callgraph:to_dot(CallGraph, File);
dump_callgraph(CallGraph, _State, #analysis{callgraph_file = File}, ".ps") ->
  Args = "-Gratio=compress -Gsize=\"100,100\"",
  dialyzer_callgraph:to_ps(CallGraph, File, Args);
dump_callgraph(CallGraph, State, #analysis{callgraph_file = File}, _Ext) ->
  case file:open(File, [write]) of
    {ok, Fd} ->
      io:format(Fd, "~p", [CallGraph]),
      ok = file:close(Fd);
    {error, Reason} ->
      Msg = io_lib:format("Could not open output file ~p, Reason: ~p\n",
			  [File, Reason]),
      send_log(State#analysis_state.parent, Msg)
  end.
