%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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

-include("dialyzer.hrl").

-record(analysis_state, 
	{
	  codeserver                    :: dialyzer_codeserver:codeserver(),
	  analysis_type  = succ_typings :: anal_type(),
	  defines        = []           :: [dial_define()],
	  doc_plt                       :: dialyzer_plt:plt(),
	  include_dirs   = []           :: [file:filename()],
	  no_warn_unused                :: set(),
	  parent                        :: pid(),
	  plt                           :: dialyzer_plt:plt(),
	  start_from     = byte_code    :: start_from(),
	  use_contracts  = true         :: boolean()
	 }).

-record(server_state, {parent :: pid(), legal_warnings :: [dial_warn_tag()]}).

%%--------------------------------------------------------------------
%% Main
%%--------------------------------------------------------------------

-spec start(pid(), [dial_warn_tag()], #analysis{}) -> 'ok'.

start(Parent, LegalWarnings, Analysis) ->
  RacesOn = ordsets:is_element(?WARN_RACE_CONDITION, LegalWarnings),
  Analysis0 = Analysis#analysis{race_detection = RacesOn},
  Analysis1 = expand_files(Analysis0),
  Analysis2 = run_analysis(Analysis1),
  State = #server_state{parent = Parent, legal_warnings = LegalWarnings},
  loop(State, Analysis2, none).

run_analysis(Analysis) ->
  Self = self(),
  Fun = fun() -> analysis_start(Self, Analysis) end,
  Analysis#analysis{analysis_pid = spawn_link(Fun)}.

loop(#server_state{parent = Parent, legal_warnings = LegalWarnings} = State,
     #analysis{analysis_pid = AnalPid} = Analysis, ExtCalls) ->
  receive
    {AnalPid, log, LogMsg} ->
      send_log(Parent, LogMsg),
      loop(State, Analysis, ExtCalls);
    {AnalPid, warnings, Warnings} ->
      case filter_warnings(LegalWarnings, Warnings) of
	[] -> ok;
	SendWarnings ->
	  send_warnings(Parent, SendWarnings)
      end,
      loop(State, Analysis, ExtCalls);
    {AnalPid, cserver, CServer, Plt} -> 
      send_codeserver_plt(Parent, CServer, Plt),
      loop(State, Analysis, ExtCalls);
    {AnalPid, done, Plt, DocPlt} ->      
      case ExtCalls =:= none of
	true ->
	  send_analysis_done(Parent, Plt, DocPlt);
	false ->
	  send_ext_calls(Parent, ExtCalls),
	  send_analysis_done(Parent, Plt, DocPlt)
      end;
    {AnalPid, ext_calls, NewExtCalls} ->
      loop(State, Analysis, NewExtCalls);
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

analysis_start(Parent, Analysis) ->
  CServer = dialyzer_codeserver:new(),
  Plt = Analysis#analysis.plt,
  State = #analysis_state{codeserver = CServer,
			  analysis_type = Analysis#analysis.type,
			  defines = Analysis#analysis.defines,
			  doc_plt = Analysis#analysis.doc_plt,
			  include_dirs = Analysis#analysis.include_dirs,
			  plt = Plt,
			  parent = Parent,
			  start_from = Analysis#analysis.start_from,
			  use_contracts = Analysis#analysis.use_contracts
			 },
  Files = ordsets:from_list(Analysis#analysis.files),
  {Callgraph, NoWarn, TmpCServer0} = compile_and_store(Files, State),
  %% Remote type postprocessing
  NewCServer =
    try
      NewRecords = dialyzer_codeserver:get_temp_records(TmpCServer0),
      OldRecords = dialyzer_plt:get_types(State#analysis_state.plt),
      MergedRecords = dialyzer_utils:merge_records(NewRecords, OldRecords),
      TmpCServer1 = dialyzer_codeserver:set_temp_records(MergedRecords, TmpCServer0),
      TmpCServer2 = dialyzer_utils:process_record_remote_types(TmpCServer1),
      dialyzer_contracts:process_contract_remote_types(TmpCServer2)
    catch
      throw:{error, _ErrorMsg} = Error -> exit(Error)
    end,
  NewPlt = dialyzer_plt:insert_types(Plt, dialyzer_codeserver:get_records(NewCServer)),
  State0 = State#analysis_state{plt = NewPlt},
  dump_callgraph(Callgraph, State0, Analysis),
  State1 = State0#analysis_state{codeserver = NewCServer},
  State2 = State1#analysis_state{no_warn_unused = NoWarn},
  %% Remove all old versions of the files being analyzed
  AllNodes = dialyzer_callgraph:all_nodes(Callgraph),
  Plt1 = dialyzer_plt:delete_list(NewPlt, AllNodes),
  Exports = dialyzer_codeserver:get_exports(NewCServer),
  NewCallgraph =
    case Analysis#analysis.race_detection of
      true -> dialyzer_callgraph:put_race_detection(true, Callgraph);
      false -> Callgraph
    end,
  State3 = analyze_callgraph(NewCallgraph, State2#analysis_state{plt = Plt1}),
  NonExports = sets:subtract(sets:from_list(AllNodes), Exports),
  NonExportsList = sets:to_list(NonExports),
  Plt3 = dialyzer_plt:delete_list(State3#analysis_state.plt, NonExportsList),
  Plt4 = dialyzer_plt:delete_contract_list(Plt3, NonExportsList),
  send_codeserver_plt(Parent, CServer, State3#analysis_state.plt), 
  send_analysis_done(Parent, Plt4, State3#analysis_state.doc_plt).

analyze_callgraph(Callgraph, State) ->
  Plt = State#analysis_state.plt,
  Codeserver = State#analysis_state.codeserver,
  Parent = State#analysis_state.parent,
  case State#analysis_state.analysis_type of
    plt_build ->
      Callgraph1 = dialyzer_callgraph:finalize(Callgraph),
      NewPlt = dialyzer_succ_typings:analyze_callgraph(Callgraph1, Plt, 
						       Codeserver, Parent),
      dialyzer_callgraph:delete(Callgraph1),
      State#analysis_state{plt = NewPlt};
    succ_typings ->
      NoWarn = State#analysis_state.no_warn_unused,
      DocPlt = State#analysis_state.doc_plt,
      Callgraph1 = dialyzer_callgraph:finalize(Callgraph),
      {Warnings, NewPlt, NewDocPlt} = 
	dialyzer_succ_typings:get_warnings(Callgraph1, Plt, DocPlt,
					   Codeserver, NoWarn, Parent),
      dialyzer_callgraph:delete(Callgraph1),
      send_warnings(State#analysis_state.parent, Warnings),
      State#analysis_state{plt = NewPlt, doc_plt = NewDocPlt}
  end.

%%--------------------------------------------------------------------
%% Build the callgraph and fill the codeserver.
%%--------------------------------------------------------------------

compile_and_store(Files, #analysis_state{codeserver = CServer,
					 defines = Defs,
					 include_dirs = Dirs,
					 parent = Parent,
					 use_contracts = UseContracts,
					 start_from = StartFrom} = State) ->
  send_log(Parent, "Reading files and computing callgraph... "),
  {T1, _} = statistics(runtime),
  Includes = [{i, D} || D <- Dirs],
  Defines = [{d, Macro, Val} || {Macro, Val} <- Defs],
  Callgraph = dialyzer_callgraph:new(),
  Fun = case StartFrom of
	  src_code ->
	    fun(File, {TmpCG, TmpCServer, TmpFailed, TmpNoWarn, TmpMods}) ->
		case compile_src(File, Includes, Defines, TmpCG,
				 TmpCServer, UseContracts) of
		  {error, Reason} ->
		    {TmpCG, TmpCServer, [{File, Reason}|TmpFailed], TmpNoWarn,
                     TmpMods};
		  {ok, NewCG, NoWarn, NewCServer, Mod} -> 
		    {NewCG, NewCServer, TmpFailed, NoWarn++TmpNoWarn,
                     [Mod|TmpMods]}
		end
	    end;
	  byte_code ->
	    fun(File, {TmpCG, TmpCServer, TmpFailed, TmpNoWarn, TmpMods}) -> 
		case compile_byte(File, TmpCG, TmpCServer, UseContracts) of
		  {error, Reason} ->
		    {TmpCG, TmpCServer, [{File, Reason}|TmpFailed], TmpNoWarn,
                     TmpMods};
		  {ok, NewCG, NoWarn, NewCServer, Mod} -> 
		    {NewCG, NewCServer, TmpFailed, NoWarn++TmpNoWarn,
                     [Mod|TmpMods]}
		end
	    end
	end,
  {NewCallgraph1, NewCServer, Failed, NoWarn, Modules} = 
    lists:foldl(Fun, {Callgraph, CServer, [], [], []}, Files),
  case Failed =:= [] of
    true ->
      NewFiles = lists:zip(lists:reverse(Modules), Files),
      ModDict =
        lists:foldl(fun({Mod, F}, Dict) -> dict:append(Mod, F, Dict) end,
                    dict:new(), NewFiles),
      check_for_duplicate_modules(ModDict);
    false -> 
      Msg = io_lib:format("Could not scan the following file(s): ~p",
			  [lists:flatten(Failed)]),
      exit({error, Msg})
  end,
  {T2, _} = statistics(runtime),
  Msg1 = io_lib:format("done in ~.2f secs\nRemoving edges... ", [(T2-T1)/1000]),
  send_log(Parent, Msg1),
  NewCallgraph2 = cleanup_callgraph(State, NewCServer, NewCallgraph1, Modules),
  {T3, _} = statistics(runtime),
  Msg2 = io_lib:format("done in ~.2f secs\n", [(T3-T2)/1000]),
  send_log(Parent, Msg2),  
  {NewCallgraph2, sets:from_list(NoWarn), NewCServer}.

cleanup_callgraph(#analysis_state{plt = InitPlt, parent = Parent, 
				  codeserver = CodeServer},
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
	lists:partition(fun({_From, {M, _F, _A}}) -> 
			    sets:is_element(M, ModuleSet) orelse
			      dialyzer_plt:contains_module(InitPlt, M)
			end, ExtCalls1)
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

compile_src(File, Includes, Defines, Callgraph, CServer, UseContracts) ->
  DefaultIncludes = default_includes(filename:dirname(File)),
  SrcCompOpts = dialyzer_utils:src_compiler_opts(),
  CompOpts = SrcCompOpts ++ Includes ++ Defines ++ DefaultIncludes,
  case dialyzer_utils:get_abstract_code_from_src(File, CompOpts) of
    {error, _Msg} = Error -> Error;
    {ok, AbstrCode} ->
      case dialyzer_utils:get_core_from_abstract_code(AbstrCode, CompOpts) of
	error -> {error, "  Could not find abstract code for: " ++ File};
	{ok, Core} ->
          Mod = cerl:concrete(cerl:module_name(Core)),
	  NoWarn = abs_get_nowarn(AbstrCode, Mod),
	  case dialyzer_utils:get_record_and_type_info(AbstrCode) of
	    {error, _} = Error -> Error;
	    {ok, RecInfo} ->
	      CServer2 =
		dialyzer_codeserver:store_temp_records(Mod, RecInfo, CServer),
	      case UseContracts of
		true ->
		  case dialyzer_utils:get_spec_info(Mod, AbstrCode, RecInfo) of
		    {error, _} = Error -> Error;
		    {ok, SpecInfo} ->
		      CServer3 =
			dialyzer_codeserver:store_temp_contracts(Mod,
								 SpecInfo,
								 CServer2),
		      store_core(Mod, Core, NoWarn, Callgraph, CServer3)
		  end;
		false ->
		  store_core(Mod, Core, NoWarn, Callgraph, CServer2)
	      end
	  end
      end
  end.

compile_byte(File, Callgraph, CServer, UseContracts) ->
  case dialyzer_utils:get_abstract_code_from_beam(File) of
    error ->
      {error, "  Could not get abstract code for: " ++ File ++ "\n" ++
       "  Recompile with +debug_info or analyze starting from source code"};
    {ok, AbstrCode} ->
      case dialyzer_utils:get_core_from_abstract_code(AbstrCode) of
	error -> {error, "  Could not get core for: "++File};
	{ok, Core} ->
          Mod = cerl:concrete(cerl:module_name(Core)),
          NoWarn = abs_get_nowarn(AbstrCode, Mod),
	  case dialyzer_utils:get_record_and_type_info(AbstrCode) of
	    {error, _} = Error -> Error;
	    {ok, RecInfo} ->
	      CServer1 = 
		dialyzer_codeserver:store_temp_records(Mod, RecInfo, CServer),
	      case UseContracts of
		true ->
		  case dialyzer_utils:get_spec_info(Mod, AbstrCode, RecInfo) of
		    {error, _} = Error -> Error;
		    {ok, SpecInfo} ->
		      CServer2 = 
			dialyzer_codeserver:store_temp_contracts(Mod, SpecInfo,
								 CServer1),
		      store_core(Mod, Core, NoWarn, Callgraph, CServer2)
		  end;
		false ->
		  store_core(Mod, Core, NoWarn, Callgraph, CServer1)
	      end
	  end
      end
  end.

store_core(Mod, Core, NoWarn, Callgraph, CServer) ->
  Exp = get_exports_from_core(Core),
  CServer1 = dialyzer_codeserver:insert_exports(Exp, CServer),
  {LabeledCore, CServer2} = label_core(Core, CServer1),
  store_code_and_build_callgraph(Mod, LabeledCore, Callgraph, CServer2, NoWarn).

abs_get_nowarn(Abs, M) ->
  [{M, F, A} 
   || {attribute, _, compile, {nowarn_unused_function, {F, A}}} <- Abs].

get_exports_from_core(Core) ->
  Tree = cerl:from_records(Core),
  Exports1 = cerl:module_exports(Tree),
  Exports2 = [cerl:var_name(V) || V <- Exports1],
  M = cerl:atom_val(cerl:module_name(Tree)),
  [{M, F, A} || {F, A} <- Exports2].

label_core(Core, CServer) ->
  NextLabel = dialyzer_codeserver:get_next_core_label(CServer),
  CoreTree = cerl:from_records(Core),
  {LabeledTree, NewNextLabel} = cerl_trees:label(CoreTree, NextLabel),
  {cerl:to_records(LabeledTree), 
   dialyzer_codeserver:set_next_core_label(NewNextLabel, CServer)}.

store_code_and_build_callgraph(Mod, Core, Callgraph, CServer, NoWarn) ->
  CoreTree = cerl:from_records(Core),
  NewCallgraph = dialyzer_callgraph:scan_core_tree(CoreTree, Callgraph),
  CServer2 = dialyzer_codeserver:insert(Mod, CoreTree, CServer),
  {ok, NewCallgraph, NoWarn, CServer2, Mod}.

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
      NewFiles =
        [filename:join(File, X) || X <- List, filename:extension(X) =:= Ext],
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

send_log(Parent, Msg) ->
  Parent ! {self(), log, Msg},
  ok.

send_warnings(_Parent, []) ->
  ok;
send_warnings(Parent, Warnings) ->
  Parent ! {self(), warnings, Warnings},
  ok.

filter_warnings(LegalWarnings, Warnings) ->
  [TIW || {Tag, _Id, _Warning} = TIW <- Warnings,
	  ordsets:is_element(Tag, LegalWarnings)].

send_analysis_done(Parent, Plt, DocPlt) ->
  Parent ! {self(), done, Plt, DocPlt},
  ok.
  
send_ext_calls(Parent, ExtCalls) ->
  Parent ! {self(), ext_calls, ExtCalls},
  ok.

send_codeserver_plt(Parent, CServer, Plt ) ->
  Parent ! {self(), cserver, CServer, Plt},
  ok.

send_bad_calls(Parent, BadCalls, CodeServer) ->
  send_warnings(Parent, format_bad_calls(BadCalls, CodeServer, [])).

send_mod_deps(Parent, ModuleDeps) ->
  Parent ! {self(), mod_deps, ModuleDeps},
  ok.

format_bad_calls([{{_, _, _}, {_, module_info, A}}|Left], CodeServer, Acc) 
  when A =:= 0; A =:= 1 ->
  format_bad_calls(Left, CodeServer, Acc);
format_bad_calls([{FromMFA, {M, F, A} = To}|Left], CodeServer, Acc) ->
  {_Var, FunCode} = dialyzer_codeserver:lookup_mfa_code(FromMFA, CodeServer),
  Msg = {call_to_missing, [M, F, A]},
  FileLine = find_call_file_and_line(FunCode, To),
  NewAcc = [{?WARN_CALLGRAPH, FileLine, Msg}|Acc],
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
  {T1, _} = statistics(runtime),
  dump_callgraph(CallGraph, State, Analysis, Extension),
  {T2, _} = statistics(runtime),
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
