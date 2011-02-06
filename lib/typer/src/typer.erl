%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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

%%-----------------------------------------------------------------------
%% File        : typer.erl
%% Author(s)   : The first version of typer was written by Bingwen He
%%               with guidance from Kostis Sagonas and Tobias Lindahl.
%%               Since June 2008 typer is maintained by Kostis Sagonas.
%% Description : An Erlang/OTP application that shows type information
%%               for Erlang modules to the user. Additionally, it can
%%               annotates the code of files with such type information.
%%-----------------------------------------------------------------------

-module(typer).

-export([start/0]).
-export([fatal_error/1]).	% for error reporting
-export([map__new/0, map__insert/2, map__lookup/2, map__from_list/1, map__remove/2, map__fold/3]).

%%-----------------------------------------------------------------------

-define(SHOW, show).
-define(SHOW_EXPORTED, show_exported).
-define(ANNOTATE, annotate).
-define(ANNOTATE_INC_FILES, annotate_inc_files).

-type mode() :: ?SHOW | ?SHOW_EXPORTED | ?ANNOTATE | ?ANNOTATE_INC_FILES.

%%-----------------------------------------------------------------------

-type files() :: [file:filename()].

-record(typer_analysis,
	{mode					:: mode(),
	 macros      = []			:: [{atom(), term()}], % {macro_name, value}
	 includes    = []			:: files(),
	 %% --- for dialyzer ---
	 code_server = dialyzer_codeserver:new():: dialyzer_codeserver:codeserver(),
	 callgraph   = dialyzer_callgraph:new() :: dialyzer_callgraph:callgraph(),
	 ana_files   = []			:: files(),   % absolute names
	 plt         = none			:: 'none' | file:filename(),
	 no_spec     = false                    :: boolean(),
	 %% --- for typer ---
	 t_files     = []			:: files(),
	 %% For choosing between specs or edoc @spec comments
	 edoc        = false			:: boolean(),
	 %% Files in 'final_files' are compilable with option 'to_pp'; we keep
	 %% them as {FileName, ModuleName} in case the ModuleName is different
	 final_files = []			:: [{file:filename(), module()}],
	 ex_func     = map__new()		:: map(),
	 record      = map__new()		:: map(),
	 func        = map__new()		:: map(),
	 inc_func    = map__new()		:: map(),
	 trust_plt   = dialyzer_plt:new()	:: dialyzer_plt:plt()}).
-type analysis() :: #typer_analysis{}.

-record(args, {files   = [] :: files(),
	       files_r = [] :: files(),
	       trusted = [] :: files()}).

%%--------------------------------------------------------------------

-spec start() -> no_return().

start() ->
  {Args, Analysis} = process_cl_args(),
  %% io:format("Args: ~p\n", [Args]),
  %% io:format("Analysis: ~p\n", [Analysis]),
  TrustedFiles = filter_fd(Args#args.trusted, [], fun is_erl_file/1),
  Analysis1 = Analysis#typer_analysis{t_files = TrustedFiles},
  Analysis2 = extract(Analysis1),
  All_Files = get_all_files(Args),
  %% io:format("All_Files: ~p\n", [All_Files]),
  Analysis3 = Analysis2#typer_analysis{ana_files = All_Files},
  Analysis4 = collect_info(Analysis3),
  %% io:format("Final: ~p\n", [Analysis4#typer_analysis.final_files]),
  TypeInfo = get_type_info(Analysis4),
  typer_annotator:annotate(TypeInfo),
  %% io:format("\nTyper analysis finished\n"),
  erlang:halt(0).

%%--------------------------------------------------------------------

-spec extract(analysis()) -> analysis().

extract(#typer_analysis{macros = Macros, includes = Includes,
			t_files = TFiles, trust_plt = TrustPLT} = Analysis) ->
  %% io:format("--- Extracting trusted typer_info... "),
  Ds = [{d, Name, Value} || {Name, Value} <- Macros],
  CodeServer = dialyzer_codeserver:new(),
  Fun =
    fun(File, CS) ->
	%% We include one more dir; the one above the one we are trusting
	%% E.g, for /home/tests/typer_ann/test.ann.erl, we should include
	%% /home/tests/ rather than /home/tests/typer_ann/
	AllIncludes = [filename:dirname(filename:dirname(File)) | Includes],
	Is = [{i, Dir} || Dir <- AllIncludes],
	CompOpts = dialyzer_utils:src_compiler_opts() ++ Is ++ Ds,
	case dialyzer_utils:get_abstract_code_from_src(File, CompOpts) of
	  {ok, AbstractCode} -> 
	    case dialyzer_utils:get_record_and_type_info(AbstractCode) of
	      {ok, RecDict} ->
		Mod = list_to_atom(filename:basename(File, ".erl")),
		case dialyzer_utils:get_spec_info(Mod, AbstractCode, RecDict) of
		  {ok, SpecDict} ->
		    CS1 = dialyzer_codeserver:store_temp_records(Mod, RecDict, CS),
		    dialyzer_codeserver:store_temp_contracts(Mod, SpecDict, CS1);
		  {error, Reason} -> compile_error([Reason])
		end;
	      {error, Reason} -> compile_error([Reason])
	    end;
	  {error, Reason} -> compile_error(Reason)
	end
    end,
  CodeServer1 = lists:foldl(Fun, CodeServer, TFiles),
  %% Process remote types
  NewCodeServer =
    try
      NewRecords = dialyzer_codeserver:get_temp_records(CodeServer1),
      OldRecords = dialyzer_plt:get_types(TrustPLT), % XXX change to the PLT?
      MergedRecords = dialyzer_utils:merge_records(NewRecords, OldRecords),
      CodeServer2 = dialyzer_codeserver:set_temp_records(MergedRecords, CodeServer1),
      CodeServer3 = dialyzer_utils:process_record_remote_types(CodeServer2),
      dialyzer_contracts:process_contract_remote_types(CodeServer3)
    catch
      throw:{error, ErrorMsg} ->
	compile_error(ErrorMsg)
    end,
  %% Create TrustPLT
  Contracts = dialyzer_codeserver:get_contracts(NewCodeServer),
  Modules = dict:fetch_keys(Contracts),
  FoldFun =
    fun(Module, TmpPlt) ->
	{ok, ModuleContracts} = dict:find(Module, Contracts),
	SpecList = [{MFA, Contract} 
		    || {MFA, {_FileLine, Contract}} <- dict:to_list(ModuleContracts)],
	dialyzer_plt:insert_contract_list(TmpPlt, SpecList)
    end,
  NewTrustPLT = lists:foldl(FoldFun, TrustPLT, Modules),
  Analysis#typer_analysis{trust_plt = NewTrustPLT}.

%%--------------------------------------------------------------------

-spec get_type_info(analysis()) -> analysis().

get_type_info(#typer_analysis{callgraph = CallGraph,
			      trust_plt = TrustPLT,
			      code_server = CodeServer} = Analysis) ->
  StrippedCallGraph = remove_external(CallGraph, TrustPLT),
  %% io:format("--- Analyzing callgraph... "),
  try 
    NewPlt = dialyzer_succ_typings:analyze_callgraph(StrippedCallGraph, 
						     TrustPLT, CodeServer),
    Analysis#typer_analysis{callgraph = StrippedCallGraph, trust_plt = NewPlt}
  catch
    error:What ->
      fatal_error(io_lib:format("Analysis failed with message: ~p", 
				[{What, erlang:get_stacktrace()}]));
    throw:{dialyzer_succ_typing_error, Msg} ->
      fatal_error(io_lib:format("Analysis failed with message: ~s", [Msg]))
  end.

-spec remove_external(dialyzer_callgraph:callgraph(), dialyzer_plt:plt()) -> dialyzer_callgraph:callgraph().

remove_external(CallGraph, PLT) ->
  {StrippedCG0, Ext} = dialyzer_callgraph:remove_external(CallGraph),
  StrippedCG = dialyzer_callgraph:finalize(StrippedCG0),
  case get_external(Ext, PLT) of
    [] -> ok;
    Externals ->
      msg(io_lib:format(" Unknown functions: ~p\n", [lists:usort(Externals)])),
      ExtTypes = rcv_ext_types(),
      case ExtTypes of
        [] -> ok;
        _ -> msg(io_lib:format(" Unknown types: ~p\n", [ExtTypes]))
      end
  end,
  StrippedCG.

-spec get_external([{mfa(), mfa()}], dialyzer_plt:plt()) -> [mfa()].

get_external(Exts, Plt) ->
  Fun = fun ({_From, To = {M, F, A}}, Acc) ->
	    case dialyzer_plt:contains_mfa(Plt, To) of
	      false ->
		case erl_bif_types:is_known(M, F, A) of
		  true -> Acc;
		  false -> [To|Acc]
		end;
	      true -> Acc
	    end
	end,
  lists:foldl(Fun, [], Exts).

%%--------------------------------------------------------------------
%% Processing of command-line options and arguments.
%%--------------------------------------------------------------------

-spec process_cl_args() -> {#args{}, #typer_analysis{}}.

process_cl_args() ->
  ArgList = init:get_plain_arguments(),
  %% io:format("Args is ~p\n", [ArgList]),
  {Args, Analysis} = analyze_args(ArgList, #args{}, #typer_analysis{}),
  %% if the mode has not been set, set it to the default mode (show)
  {Args, case Analysis#typer_analysis.mode of
	   undefined -> Analysis#typer_analysis{mode = ?SHOW};
	   Mode when is_atom(Mode) -> Analysis
	 end}.

analyze_args([], Args, Analysis) ->
  {Args, Analysis};
analyze_args(ArgList, Args, Analysis) ->
  {Result, Rest} = cl(ArgList),
  {NewArgs, NewAnalysis} = analyze_result(Result, Args, Analysis),
  analyze_args(Rest, NewArgs, NewAnalysis).

cl(["-h"|_])     -> help_message();
cl(["--help"|_]) -> help_message();
cl(["-v"|_])        -> version_message();
cl(["--version"|_]) -> version_message();
cl(["--edoc"|Opts]) -> {edoc, Opts};
cl(["--show"|Opts]) -> {{mode, ?SHOW}, Opts};
cl(["--show_exported"|Opts]) -> {{mode, ?SHOW_EXPORTED}, Opts};
cl(["--show-exported"|Opts]) -> {{mode, ?SHOW_EXPORTED}, Opts};
cl(["--annotate"|Opts]) -> {{mode, ?ANNOTATE}, Opts};
cl(["--annotate-inc-files"|Opts]) -> {{mode, ?ANNOTATE_INC_FILES}, Opts};
cl(["--no_spec"|Opts]) -> {no_spec, Opts};
cl(["--plt",Plt|Opts]) -> {{plt, Plt}, Opts};
cl(["-D"++Def|Opts]) ->
  case Def of
    "" -> fatal_error("no variable name specified after -D");
    _ ->
      DefPair = process_def_list(re:split(Def, "=", [{return, list}])),
      {{def, DefPair}, Opts}
  end;
cl(["-I",Dir|Opts]) -> {{inc, Dir}, Opts};
cl(["-I"++Dir|Opts]) ->
  case Dir of
    "" -> fatal_error("no include directory specified after -I");
    _ -> {{inc, Dir}, Opts}
  end;
cl(["-T"|Opts]) ->
  {Files, RestOpts} = dialyzer_cl_parse:collect_args(Opts),
  case Files of
    [] -> fatal_error("no file or directory specified after -T");
    [_|_] -> {{trusted, Files}, RestOpts}
  end;
cl(["-r"|Opts]) ->
  {Files, RestOpts} = dialyzer_cl_parse:collect_args(Opts),
  {{files_r, Files}, RestOpts};
cl(["-"++H|_]) -> fatal_error("unknown option -"++H);
cl(Opts) -> 
  {Files, RestOpts} = dialyzer_cl_parse:collect_args(Opts),
  {{files, Files}, RestOpts}.

process_def_list(L) ->
  case L of
    [Name, Value] ->
      {ok, Tokens, _} = erl_scan:string(Value ++ "."),
      {ok, ErlValue} = erl_parse:parse_term(Tokens),
      {list_to_atom(Name), ErlValue};
    [Name] ->
      {list_to_atom(Name), true}
  end.

%% Get information about files that the user trusts and wants to analyze
analyze_result({files, Val}, Args, Analysis) ->
  NewVal = Args#args.files ++ Val,
  {Args#args{files = NewVal}, Analysis};
analyze_result({files_r, Val}, Args, Analysis) ->
  NewVal = Args#args.files_r ++ Val,
  {Args#args{files_r = NewVal}, Analysis};
analyze_result({trusted, Val}, Args, Analysis) ->
  NewVal = Args#args.trusted ++ Val,
  {Args#args{trusted = NewVal}, Analysis};
analyze_result(edoc, Args, Analysis) ->
  {Args, Analysis#typer_analysis{edoc = true}};
%% Get useful information for actual analysis
analyze_result({mode, Mode}, Args, Analysis) ->
  case Analysis#typer_analysis.mode of
    undefined -> {Args, Analysis#typer_analysis{mode = Mode}};
    OldMode -> mode_error(OldMode, Mode)
  end;
analyze_result({def, Val}, Args, Analysis) ->
  NewVal = Analysis#typer_analysis.macros ++ [Val],
  {Args, Analysis#typer_analysis{macros = NewVal}};
analyze_result({inc, Val}, Args, Analysis) ->
  NewVal = Analysis#typer_analysis.includes ++ [Val],
  {Args, Analysis#typer_analysis{includes = NewVal}};
analyze_result({plt, Plt}, Args, Analysis) ->
  {Args, Analysis#typer_analysis{plt = Plt}};
analyze_result(no_spec, Args, Analysis) ->
  {Args, Analysis#typer_analysis{no_spec = true}}.

%%--------------------------------------------------------------------
%% File processing.
%%--------------------------------------------------------------------

-spec get_all_files(#args{}) -> files().

get_all_files(#args{files = Fs, files_r = Ds}) ->
  case filter_fd(Fs, Ds, fun test_erl_file_exclude_ann/1) of
    [] -> fatal_error("no file(s) to analyze");
    AllFiles -> AllFiles
  end.

-spec test_erl_file_exclude_ann(file:filename()) -> boolean().

test_erl_file_exclude_ann(File) ->
  case is_erl_file(File) of
    true -> %% Exclude files ending with ".ann.erl"
      case re:run(File, "[\.]ann[\.]erl$") of
	{match, _} -> false;
	nomatch -> true
      end;
    false -> false
  end.

-spec is_erl_file(file:filename()) -> boolean().

is_erl_file(File) ->
  filename:extension(File) =:= ".erl".

-type test_file_fun() :: fun((file:filename()) -> boolean()).

-spec filter_fd(files(), files(), test_file_fun()) -> files().

filter_fd(File_Dir, Dir_R, Fun) ->
  All_File_1 = process_file_and_dir(File_Dir, Fun),
  All_File_2 = process_dir_rec(Dir_R, Fun),
  remove_dup(All_File_1 ++ All_File_2).

-spec process_file_and_dir(files(), test_file_fun()) -> files().

process_file_and_dir(File_Dir, TestFun) ->
  Fun =
    fun (Elem, Acc) ->
	case filelib:is_regular(Elem) of
	  true  -> process_file(Elem, TestFun, Acc);
	  false -> check_dir(Elem, false, Acc, TestFun)
	end
    end,
  lists:foldl(Fun, [], File_Dir).

-spec process_dir_rec(files(), test_file_fun()) -> files().

process_dir_rec(Dirs, TestFun) ->
  Fun = fun (Dir, Acc) -> check_dir(Dir, true, Acc, TestFun) end,
  lists:foldl(Fun, [], Dirs).

-spec check_dir(file:filename(), boolean(), files(), test_file_fun()) -> files().

check_dir(Dir, Recursive, Acc, Fun) ->
  case file:list_dir(Dir) of
    {ok, Files} ->
      {TmpDirs, TmpFiles} = split_dirs_and_files(Files, Dir),
      case Recursive of
	false ->
	  FinalFiles = process_file_and_dir(TmpFiles, Fun),
	  Acc ++ FinalFiles;
	true ->
	  TmpAcc1 = process_file_and_dir(TmpFiles, Fun),
	  TmpAcc2 = process_dir_rec(TmpDirs, Fun),
	  Acc ++ TmpAcc1 ++ TmpAcc2
      end;
    {error, eacces} ->
      fatal_error("no access permission to dir \""++Dir++"\"");
    {error, enoent} ->
      fatal_error("cannot access "++Dir++": No such file or directory");
    {error, _Reason} ->
      fatal_error("error involving a use of file:list_dir/1")
  end.

%% Same order as the input list
-spec process_file(file:filename(), test_file_fun(), files()) -> files().

process_file(File, TestFun, Acc) ->
  case TestFun(File) of
    true  -> Acc ++ [File];
    false -> Acc
  end.

%% Same order as the input list
-spec split_dirs_and_files(files(), file:filename()) -> {files(), files()}.

split_dirs_and_files(Elems, Dir) ->
  Test_Fun =
    fun (Elem, {DirAcc, FileAcc}) ->
	File = filename:join(Dir, Elem),
	case filelib:is_regular(File) of
	  false -> {[File|DirAcc], FileAcc}; 
	  true  -> {DirAcc, [File|FileAcc]}
	end
    end,
  {Dirs, Files} = lists:foldl(Test_Fun, {[], []}, Elems),
  {lists:reverse(Dirs), lists:reverse(Files)}.  

%% Removes duplicate filenames but keeps the order of the input list
-spec remove_dup(files()) -> files().

remove_dup(Files) ->
  Test_Dup = fun (File, Acc) ->
		 case lists:member(File, Acc) of
		   true  -> Acc;
		   false -> [File|Acc]
		 end
	     end,
  Reversed_Elems = lists:foldl(Test_Dup, [], Files),
  lists:reverse(Reversed_Elems).

%%--------------------------------------------------------------------
%% Collect information.
%%--------------------------------------------------------------------

-type func_info() :: {non_neg_integer(), atom(), arity()}.
-type inc_file_info() :: {file:filename(), func_info()}.

-record(tmpAcc, {file		  :: file:filename(),
		 module		  :: atom(),
		 funcAcc = []	  :: [func_info()],
		 incFuncAcc = []  :: [inc_file_info()],
		 dialyzerObj = [] :: [{mfa(), {_, _}}]}).

-spec collect_info(analysis()) -> analysis().

collect_info(Analysis) ->
  NewPlt =
    try get_dialyzer_plt(Analysis) of
	DialyzerPlt ->
	dialyzer_plt:merge_plts([Analysis#typer_analysis.trust_plt, DialyzerPlt])
    catch
      throw:{dialyzer_error,_Reason} ->
	fatal_error("Dialyzer's PLT is missing or is not up-to-date; please (re)create it")
    end,
  NewAnalysis = lists:foldl(fun collect_one_file_info/2, 
			    Analysis#typer_analysis{trust_plt = NewPlt}, 
			    Analysis#typer_analysis.ana_files),
  %% Process Remote Types
  TmpCServer = NewAnalysis#typer_analysis.code_server,
  NewCServer =
    try
      NewRecords = dialyzer_codeserver:get_temp_records(TmpCServer),
      NewExpTypes = dialyzer_codeserver:get_temp_exported_types(TmpCServer),
      OldRecords = dialyzer_plt:get_types(NewPlt),
      OldExpTypes = dialyzer_plt:get_exported_types(NewPlt),
      MergedRecords = dialyzer_utils:merge_records(NewRecords, OldRecords),
      MergedExpTypes = sets:union(NewExpTypes, OldExpTypes),
      %% io:format("Merged Records ~p",[MergedRecords]),
      TmpCServer1 = dialyzer_codeserver:set_temp_records(MergedRecords, TmpCServer),
      TmpCServer2 =
        dialyzer_codeserver:insert_temp_exported_types(MergedExpTypes,
                                                       TmpCServer1),
      TmpCServer3 = dialyzer_utils:process_record_remote_types(TmpCServer2),
      dialyzer_contracts:process_contract_remote_types(TmpCServer3)
    catch
      throw:{error, ErrorMsg} ->
	fatal_error(ErrorMsg)
    end,
  NewAnalysis#typer_analysis{code_server = NewCServer}.

collect_one_file_info(File, Analysis) ->
  Ds = [{d,Name,Val} || {Name,Val} <- Analysis#typer_analysis.macros],
  %% Current directory should also be included in "Includes".
  Includes = [filename:dirname(File)|Analysis#typer_analysis.includes],
  Is = [{i,Dir} || Dir <- Includes],
  Options = dialyzer_utils:src_compiler_opts() ++ Is ++ Ds,
  case dialyzer_utils:get_abstract_code_from_src(File, Options) of
    {error, Reason} ->
      %% io:format("File=~p\n,Options=~p\n,Error=~p\n", [File,Options,Reason]),
      compile_error(Reason);
    {ok, AbstractCode} ->
      case dialyzer_utils:get_core_from_abstract_code(AbstractCode, Options) of
	error -> compile_error(["Could not get core erlang for "++File]);
	{ok, Core} ->
	  case dialyzer_utils:get_record_and_type_info(AbstractCode) of
	    {error, Reason} -> compile_error([Reason]);
	    {ok, Records} ->
	      Mod = cerl:concrete(cerl:module_name(Core)),
	      case dialyzer_utils:get_spec_info(Mod, AbstractCode, Records) of
		{error, Reason} -> compile_error([Reason]);
		{ok, SpecInfo} ->
                  ExpTypes = get_exported_types_from_core(Core),
		  analyze_core_tree(Core, Records, SpecInfo, ExpTypes,
                                    Analysis, File)
	      end
	  end
      end
  end.

analyze_core_tree(Core, Records, SpecInfo, ExpTypes, Analysis, File) ->
  Module = cerl:concrete(cerl:module_name(Core)),
  TmpTree = cerl:from_records(Core),
  CS1 = Analysis#typer_analysis.code_server,
  NextLabel = dialyzer_codeserver:get_next_core_label(CS1),
  {Tree, NewLabel} = cerl_trees:label(TmpTree, NextLabel),
  CS2 = dialyzer_codeserver:insert(Module, Tree, CS1),
  CS3 = dialyzer_codeserver:set_next_core_label(NewLabel, CS2),
  CS4 = dialyzer_codeserver:store_temp_records(Module, Records, CS3),
  CS5 =
    case Analysis#typer_analysis.no_spec of
      true -> CS4;
      false -> dialyzer_codeserver:store_temp_contracts(Module, SpecInfo, CS4)
    end,
  OldExpTypes = dialyzer_codeserver:get_temp_exported_types(CS5),
  MergedExpTypes = sets:union(ExpTypes, OldExpTypes),
  CS6 = dialyzer_codeserver:insert_temp_exported_types(MergedExpTypes, CS5),
  Ex_Funcs = [{0,F,A} || {_,_,{F,A}} <- cerl:module_exports(Tree)],
  TmpCG = Analysis#typer_analysis.callgraph,
  CG = dialyzer_callgraph:scan_core_tree(Tree, TmpCG),
  Fun = fun analyze_one_function/2,
  All_Defs = cerl:module_defs(Tree),
  Acc = lists:foldl(Fun, #tmpAcc{file=File, module=Module}, All_Defs),
  Exported_FuncMap = map__insert({File, Ex_Funcs},
				 Analysis#typer_analysis.ex_func),
  %% NOTE: we must sort all functions in the file which
  %% originate from this file by *numerical order* of lineNo
  Sorted_Functions = lists:keysort(1, Acc#tmpAcc.funcAcc),
  FuncMap = map__insert({File, Sorted_Functions}, Analysis#typer_analysis.func),
  %% NOTE: However we do not need to sort functions
  %% which are imported from included files.
  IncFuncMap = map__insert({File, Acc#tmpAcc.incFuncAcc}, 
			   Analysis#typer_analysis.inc_func),
  Final_Files = Analysis#typer_analysis.final_files ++ [{File, Module}],
  RecordMap = map__insert({File, Records}, Analysis#typer_analysis.record),
  Analysis#typer_analysis{final_files=Final_Files,
			  callgraph=CG,
			  code_server=CS6,
			  ex_func=Exported_FuncMap,
			  inc_func=IncFuncMap,
			  record=RecordMap,
			  func=FuncMap}.

analyze_one_function({Var, FunBody} = Function, Acc) ->
  F = cerl:fname_id(Var),
  A = cerl:fname_arity(Var),
  TmpDialyzerObj = {{Acc#tmpAcc.module, F, A}, Function},
  NewDialyzerObj = Acc#tmpAcc.dialyzerObj ++ [TmpDialyzerObj],  
  [_, LineNo, {file, FileName}] = cerl:get_ann(FunBody),
  BaseName = filename:basename(FileName),
  FuncInfo = {LineNo, F, A},
  OriginalName = Acc#tmpAcc.file,
  {FuncAcc, IncFuncAcc} =
    case (FileName =:= OriginalName) orelse (BaseName =:= OriginalName) of
      true -> %% Coming from original file
	%% io:format("Added function ~p\n", [{LineNo, F, A}]),
	{Acc#tmpAcc.funcAcc ++ [FuncInfo], Acc#tmpAcc.incFuncAcc};
      false ->
	%% Coming from other sourses, including:
	%%     -- .yrl (yecc-generated file)
	%%     -- yeccpre.hrl (yecc-generated file)
	%%     -- other cases
	{Acc#tmpAcc.funcAcc, Acc#tmpAcc.incFuncAcc ++ [{FileName, FuncInfo}]}
    end,
  Acc#tmpAcc{funcAcc = FuncAcc,
	     incFuncAcc = IncFuncAcc,
	     dialyzerObj = NewDialyzerObj}.

get_dialyzer_plt(#typer_analysis{plt = PltFile0}) ->
  PltFile =
    case PltFile0 =:= none of
      true -> dialyzer_plt:get_default_plt();
      false -> PltFile0
    end,
  dialyzer_plt:from_file(PltFile).


%% Exported Types

get_exported_types_from_core(Core) ->
  Attrs = cerl:module_attrs(Core),
  ExpTypes1 = [cerl:concrete(L2) || {L1, L2} <- Attrs,
                                    cerl:is_literal(L1),
                                    cerl:is_literal(L2),
                                    cerl:concrete(L1) =:= 'export_type'],
  ExpTypes2 = lists:flatten(ExpTypes1),
  M = cerl:atom_val(cerl:module_name(Core)),
  sets:from_list([{M, F, A} || {F, A} <- ExpTypes2]).

%%--------------------------------------------------------------------
%% Utilities for error reporting.
%%--------------------------------------------------------------------

-spec fatal_error(string()) -> no_return().

fatal_error(Slogan) ->
  msg(io_lib:format("typer: ~s\n", [Slogan])),
  erlang:halt(1).

-spec mode_error(mode(), mode()) -> no_return().
mode_error(OldMode, NewMode) ->
  Msg = io_lib:format("Mode was previously set to '~s'; "
		      "can not set it to '~s' now",
		      [OldMode, NewMode]),
  fatal_error(Msg).

-spec compile_error([string()]) -> no_return().

compile_error(Reason) ->
  JoinedString = lists:flatten([X ++ "\n" || X <- Reason]),
  Msg = "Analysis failed with error report:\n" ++ JoinedString,
  fatal_error(Msg).

-spec msg(string()) -> 'ok'.

msg(Msg) ->
  case os:type() of
    {unix, _} -> % Output a message on 'stderr', if possible
      P = open_port({fd, 0, 2}, [out]),
      port_command(P, Msg),
      true = port_close(P),
      ok;
    _ ->  % win32, vxworks
      io:format("~s", [Msg])
  end.

%%--------------------------------------------------------------------
%% Version and help messages.
%%--------------------------------------------------------------------

-spec version_message() -> no_return().
version_message() ->
  io:format("TypEr version "++?VSN++"\n"),
  erlang:halt(0).

-spec help_message() -> no_return().
help_message() ->
  S = " Usage: typer [--help] [--version] [--plt PLT] [--edoc]
              [--show | --show-exported | --annotate | --annotate-inc-files]
              [-Ddefine]* [-I include_dir]* [-T application]* [-r] file*

 Options:
   -r dir*
       search directories recursively for .erl files below them
   --show
       Prints type specifications for all functions on stdout.
       (this is the default behaviour; this option is not really needed)
   --show-exported (or --show_exported)
       Same as --show, but prints specifications for exported functions only
       Specs are displayed sorted alphabetically on the function's name
   --annotate
       Annotates the specified files with type specifications
   --annotate-inc-files
       Same as --annotate but annotates all -include() files as well as
       all .erl files (use this option with caution - has not been tested much)
   --edoc
       Prints type information as Edoc @spec comments, not as type specs
   --plt PLT
       Use the specified dialyzer PLT file rather than the default one
   -T file*
       The specified file(s) already contain type specifications and these
       are to be trusted in order to print specs for the rest of the files
       (Multiple files or dirs, separated by spaces, can be specified.)
   -Dname (or -Dname=value)
       pass the defined name(s) to TypEr
       (The syntax of defines is the same as that used by \"erlc\".)
   -I include_dir
       pass the include_dir to TypEr
       (The syntax of includes is the same as that used by \"erlc\".)
   --version (or -v)
       prints the Typer version and exits
   --help (or -h)
       prints this message and exits

 Note:
   * denotes that multiple occurrences of these options are possible.
",
  io:put_chars(S),
  erlang:halt(0).

%%--------------------------------------------------------------------
%% Handle messages.                         
%%-------------------------------------------------------------------- 

rcv_ext_types() ->
  Self = self(),
  Self ! {Self, done},
  rcv_ext_types(Self, []).

rcv_ext_types(Self, ExtTypes) ->
  receive
    {Self, ext_types, ExtType} ->
      rcv_ext_types(Self, [ExtType|ExtTypes]);
    {Self, done} ->
      lists:usort(ExtTypes)
  end.

%%--------------------------------------------------------------------
%% A convenient abstraction of a Key-Value mapping data structure
%%--------------------------------------------------------------------

-type map() :: dict().

-spec map__new() -> map().
map__new() ->
  dict:new().

-spec map__insert({term(), term()}, map()) -> map().
map__insert(Object, Map) ->
  {Key, Value} = Object,
  dict:store(Key, Value, Map).

-spec map__lookup(term(), map()) -> term().
map__lookup(Key, Map) ->
  try dict:fetch(Key, Map) catch error:_ -> none end.

-spec map__from_list([{term(), term()}]) -> map().
map__from_list(List) ->
  dict:from_list(List).

-spec map__remove(term(), map()) -> map().
map__remove(Key, Dict) ->
  dict:erase(Key, Dict).

-spec map__fold(fun((term(), term(), term()) -> term()), term(), map()) -> term().
map__fold(Fun, Acc0, Dict) -> 
  dict:fold(Fun, Acc0, Dict).
