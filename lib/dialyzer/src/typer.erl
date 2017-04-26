%% -*- erlang-indent-level: 2 -*-
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

%%-----------------------------------------------------------------------
%% File        : typer.erl
%% Author(s)   : The first version of typer was written by Bingwen He
%%               with guidance from Kostis Sagonas and Tobias Lindahl.
%%               Since June 2008 typer is maintained by Kostis Sagonas.
%% Description : An Erlang/OTP application that shows type information
%%               for Erlang modules to the user.  Additionally, it can
%%               annotate the code of files with such type information.
%%-----------------------------------------------------------------------

-module(typer).

-export([start/0]).

%%-----------------------------------------------------------------------

-define(SHOW, show).
-define(SHOW_EXPORTED, show_exported).
-define(ANNOTATE, annotate).
-define(ANNOTATE_INC_FILES, annotate_inc_files).

-type mode() :: ?SHOW | ?SHOW_EXPORTED | ?ANNOTATE | ?ANNOTATE_INC_FILES.

%%-----------------------------------------------------------------------

-type files()      :: [file:filename()].
-type callgraph()  :: dialyzer_callgraph:callgraph().
-type codeserver() :: dialyzer_codeserver:codeserver().
-type plt()        :: dialyzer_plt:plt().

-record(analysis,
	{mode				 :: mode() | 'undefined',
	 macros     = []		 :: [{atom(), term()}],
	 includes   = []		 :: files(),
	 codeserver = dialyzer_codeserver:new():: codeserver(),
	 callgraph  = dialyzer_callgraph:new() :: callgraph(),
	 files      = []		 :: files(),   % absolute names
	 plt        = none		 :: 'none' | file:filename(),
	 no_spec    = false              :: boolean(),
	 show_succ  = false              :: boolean(),
	 %% For choosing between specs or edoc @spec comments
	 edoc       = false		 :: boolean(),
	 %% Files in 'fms' are compilable with option 'to_pp'; we keep them
	 %% as {FileName, ModuleName} in case the ModuleName is different
	 fms        = []		 :: [{file:filename(), module()}],
	 ex_func    = map__new()	 :: map_dict(),
	 record     = map__new()	 :: map_dict(),
	 func       = map__new()	 :: map_dict(),
	 inc_func   = map__new()	 :: map_dict(),
	 trust_plt  = dialyzer_plt:new() :: plt()}).
-type analysis() :: #analysis{}.

-record(args, {files   = [] :: files(),
	       files_r = [] :: files(),
	       trusted = [] :: files()}).
-type args() :: #args{}.

%%--------------------------------------------------------------------

-spec start() -> no_return().

start() ->
  {Args, Analysis} = process_cl_args(),
  %% io:format("Args: ~p\n", [Args]),
  %% io:format("Analysis: ~p\n", [Analysis]),
  Timer = dialyzer_timing:init(false),
  TrustedFiles = filter_fd(Args#args.trusted, [], fun is_erl_file/1),
  Analysis2 = extract(Analysis, TrustedFiles),
  All_Files = get_all_files(Args),
  %% io:format("All_Files: ~p\n", [All_Files]),
  Analysis3 = Analysis2#analysis{files = All_Files},
  Analysis4 = collect_info(Analysis3),
  %% io:format("Final: ~p\n", [Analysis4#analysis.fms]),
  TypeInfo = get_type_info(Analysis4),
  dialyzer_timing:stop(Timer),
  show_or_annotate(TypeInfo),
  %% io:format("\nTyper analysis finished\n"),
  erlang:halt(0).

%%--------------------------------------------------------------------

-spec extract(analysis(), files()) -> analysis().

extract(#analysis{macros = Macros,
		  includes = Includes,
		  trust_plt = TrustPLT} = Analysis, TrustedFiles) ->
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
	case dialyzer_utils:get_core_from_src(File, CompOpts) of
	  {ok, Core} ->
	    case dialyzer_utils:get_record_and_type_info(Core) of
	      {ok, RecDict} ->
		Mod = list_to_atom(filename:basename(File, ".erl")),
		case dialyzer_utils:get_spec_info(Mod, Core, RecDict) of
		  {ok, SpecDict, CbDict} ->
		    CS1 = dialyzer_codeserver:store_temp_records(Mod, RecDict, CS),
		    dialyzer_codeserver:store_temp_contracts(Mod, SpecDict, CbDict, CS1);
		  {error, Reason} -> compile_error([Reason])
		end;
	      {error, Reason} -> compile_error([Reason])
	    end;
	  {error, Reason} -> compile_error(Reason)
	end
    end,
  CodeServer1 = lists:foldl(Fun, CodeServer, TrustedFiles),
  %% Process remote types
  NewCodeServer =
    try
      CodeServer2 =
        dialyzer_utils:merge_types(CodeServer1,
                                   TrustPLT), % XXX change to the PLT?
      NewExpTypes = dialyzer_codeserver:get_temp_exported_types(CodeServer1),
      case sets:size(NewExpTypes) of 0 -> ok end,
      CodeServer3 = dialyzer_codeserver:finalize_exported_types(NewExpTypes, CodeServer2),
      CodeServer4 = dialyzer_utils:process_record_remote_types(CodeServer3),
      dialyzer_contracts:process_contract_remote_types(CodeServer4)
    catch
      throw:{error, ErrorMsg} ->
	compile_error(ErrorMsg)
    end,
  %% Create TrustPLT
  ContractsDict = dialyzer_codeserver:get_contracts(NewCodeServer),
  Contracts = orddict:from_list(dict:to_list(ContractsDict)),
  NewTrustPLT = dialyzer_plt:insert_contract_list(TrustPLT, Contracts),
  Analysis#analysis{trust_plt = NewTrustPLT}.

%%--------------------------------------------------------------------

-spec get_type_info(analysis()) -> analysis().

get_type_info(#analysis{callgraph = CallGraph,
			trust_plt = TrustPLT,
			codeserver = CodeServer} = Analysis) ->
  StrippedCallGraph = remove_external(CallGraph, TrustPLT),
  %% io:format("--- Analyzing callgraph... "),
  try 
    NewMiniPlt = dialyzer_succ_typings:analyze_callgraph(StrippedCallGraph,
                                                         TrustPLT,
                                                         CodeServer),
    NewPlt = dialyzer_plt:restore_full_plt(NewMiniPlt),
    Analysis#analysis{callgraph = StrippedCallGraph, trust_plt = NewPlt}
  catch
    error:What ->
      fatal_error(io_lib:format("Analysis failed with message: ~p", 
				[{What, erlang:get_stacktrace()}]));
    throw:{dialyzer_succ_typing_error, Msg} ->
      fatal_error(io_lib:format("Analysis failed with message: ~s", [Msg]))
  end.

-spec remove_external(callgraph(), plt()) -> callgraph().

remove_external(CallGraph, PLT) ->
  {StrippedCG0, Ext} = dialyzer_callgraph:remove_external(CallGraph),
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
  StrippedCG0.

-spec get_external([{mfa(), mfa()}], plt()) -> [mfa()].

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
%% Showing type information or annotating files with such information.
%%--------------------------------------------------------------------

-define(TYPER_ANN_DIR, "typer_ann").

-type line()      :: non_neg_integer().
-type fa()        :: {atom(), arity()}.
-type func_info() :: {line(), atom(), arity()}.

-record(info, {records = maps:new() :: erl_types:type_table(),
	       functions = []       :: [func_info()],
	       types = map__new()   :: map_dict(),
	       edoc = false	    :: boolean()}).
-record(inc, {map = map__new() :: map_dict(), filter = [] :: files()}).
-type inc() :: #inc{}.

-spec show_or_annotate(analysis()) -> 'ok'.

show_or_annotate(#analysis{mode = Mode, fms = Files} = Analysis) ->
  case Mode of
    ?SHOW -> show(Analysis);
    ?SHOW_EXPORTED -> show(Analysis);
    ?ANNOTATE ->
      Fun = fun ({File, Module}) ->
		Info = get_final_info(File, Module, Analysis),
		write_typed_file(File, Info)
	    end,
      lists:foreach(Fun, Files);
    ?ANNOTATE_INC_FILES ->
      IncInfo = write_and_collect_inc_info(Analysis),
      write_inc_files(IncInfo)
  end.

write_and_collect_inc_info(Analysis) ->
  Fun = fun ({File, Module}, Inc) ->
	    Info = get_final_info(File, Module, Analysis),
	    write_typed_file(File, Info),
	    IncFuns = get_functions(File, Analysis),
	    collect_imported_functions(IncFuns, Info#info.types, Inc)
	end,
  NewInc = lists:foldl(Fun, #inc{}, Analysis#analysis.fms),
  clean_inc(NewInc).

write_inc_files(Inc) ->
  Fun =
    fun (File) ->
	Val = map__lookup(File, Inc#inc.map),
	%% Val is function with its type info
	%% in form [{{Line,F,A},Type}]
	Functions = [Key || {Key, _} <- Val],
	Val1 = [{{F,A},Type} || {{_Line,F,A},Type} <- Val],
	Info = #info{types = map__from_list(Val1),
		     records = maps:new(),
		     %% Note we need to sort functions here!
		     functions = lists:keysort(1, Functions)},
	%% io:format("Types ~p\n", [Info#info.types]),
	%% io:format("Functions ~p\n", [Info#info.functions]),
	%% io:format("Records ~p\n", [Info#info.records]),
	write_typed_file(File, Info)
    end,
  lists:foreach(Fun, dict:fetch_keys(Inc#inc.map)).

show(Analysis) ->
  Fun = fun ({File, Module}) ->
	    Info = get_final_info(File, Module, Analysis),
	    show_type_info(File, Info)
	end,
  lists:foreach(Fun, Analysis#analysis.fms).

get_final_info(File, Module, Analysis) ->
  Records = get_records(File, Analysis),
  Types = get_types(Module, Analysis, Records),
  Functions = get_functions(File, Analysis),
  Edoc = Analysis#analysis.edoc,
  #info{records = Records, functions = Functions, types = Types, edoc = Edoc}.

collect_imported_functions(Functions, Types, Inc) ->
  %% Coming from other sourses, including:
  %% FIXME: How to deal with yecc-generated file????
  %%     --.yrl (yecc-generated file)???
  %%     -- yeccpre.hrl (yecc-generated file)???
  %%     -- other cases
  Fun = fun ({File, _} = Obj, I) ->
	    case is_yecc_gen(File, I) of
	      {true, NewI} -> NewI;
	      {false, NewI} ->
		check_imported_functions(Obj, NewI, Types)
	    end
	end,
  lists:foldl(Fun, Inc, Functions).

-spec is_yecc_gen(file:filename(), inc()) -> {boolean(), inc()}.

is_yecc_gen(File, #inc{filter = Fs} = Inc) ->
  case lists:member(File, Fs) of
    true -> {true, Inc};
    false ->
      case filename:extension(File) of
	".yrl" ->
	  Rootname = filename:rootname(File, ".yrl"),
	  Obj = Rootname ++ ".erl",
	  case lists:member(Obj, Fs) of
	    true -> {true, Inc};
	    false ->
	      NewInc = Inc#inc{filter = [Obj|Fs]},
	      {true, NewInc}
	  end;
	_ ->
	  case filename:basename(File) of
	    "yeccpre.hrl" -> {true, Inc};
	    _ -> {false, Inc}
	  end
      end
  end.

check_imported_functions({File, {Line, F, A}}, Inc, Types) ->
  IncMap = Inc#inc.map,
  FA = {F, A},
  Type = get_type_info(FA, Types),
  case map__lookup(File, IncMap) of
    none -> %% File is not added. Add it
      Obj = {File,[{FA, {Line, Type}}]},
      NewMap = map__insert(Obj, IncMap),
      Inc#inc{map = NewMap};
    Val -> %% File is already in. Check.
      case lists:keyfind(FA, 1, Val) of
	false ->
	  %% Function is not in; add it
	  Obj = {File, Val ++ [{FA, {Line, Type}}]},
	  NewMap = map__insert(Obj, IncMap),
	  Inc#inc{map = NewMap};
	Type ->
	  %% Function is in and with same type
	  Inc;
	_ ->
	  %% Function is in but with diff type
	  inc_warning(FA, File),
	  Elem = lists:keydelete(FA, 1, Val),
	  NewMap = case Elem of
		     [] -> map__remove(File, IncMap);
		     _  -> map__insert({File, Elem}, IncMap)
		   end,
	  Inc#inc{map = NewMap}
      end
  end.

inc_warning({F, A}, File) ->
  io:format("      ***Warning: Skip function ~p/~p ", [F, A]),
  io:format("in file ~p because of inconsistent type\n", [File]).

clean_inc(Inc) ->
  Inc1 = remove_yecc_generated_file(Inc),
  normalize_obj(Inc1).

remove_yecc_generated_file(#inc{filter = Filter} = Inc) ->
  Fun = fun (Key, #inc{map = Map} = I) ->
	    I#inc{map = map__remove(Key, Map)}
	end,
  lists:foldl(Fun, Inc, Filter).

normalize_obj(TmpInc) ->
  Fun = fun (Key, Val, Inc) ->
	    NewVal = [{{Line,F,A},Type} || {{F,A},{Line,Type}} <- Val],
	    map__insert({Key, NewVal}, Inc)
	end,
  TmpInc#inc{map = map__fold(Fun, map__new(), TmpInc#inc.map)}.

get_records(File, Analysis) ->
  map__lookup(File, Analysis#analysis.record).

get_types(Module, Analysis, Records) ->
  TypeInfoPlt = Analysis#analysis.trust_plt,
  TypeInfo = 
    case dialyzer_plt:lookup_module(TypeInfoPlt, Module) of
      none -> [];
      {value, List} -> List
    end,
  CodeServer = Analysis#analysis.codeserver,
  TypeInfoList =
    case Analysis#analysis.show_succ of
      true ->
	[convert_type_info(I) || I <- TypeInfo];
      false ->
	[get_type(I, CodeServer, Records) || I <- TypeInfo]
    end,
  map__from_list(TypeInfoList).

convert_type_info({{_M, F, A}, Range, Arg}) ->
  {{F, A}, {Range, Arg}}.

get_type({{M, F, A} = MFA, Range, Arg}, CodeServer, Records) ->
  case dialyzer_codeserver:lookup_mfa_contract(MFA, CodeServer) of
    error ->
      {{F, A}, {Range, Arg}};
    {ok, {_FileLine, Contract, _Xtra}} ->
      Sig = erl_types:t_fun(Arg, Range),
      case dialyzer_contracts:check_contract(Contract, Sig) of
	ok -> {{F, A}, {contract, Contract}};
	{error, {extra_range, _, _}} ->
	  {{F, A}, {contract, Contract}};
	{error, {overlapping_contract, []}} ->
	  {{F, A}, {contract, Contract}};
	{error, invalid_contract} ->
	  CString = dialyzer_contracts:contract_to_string(Contract),
	  SigString = dialyzer_utils:format_sig(Sig, Records),
	  Msg = io_lib:format("Error in contract of function ~w:~w/~w\n" 
			      "\t The contract is: " ++ CString ++ "\n" ++
			      "\t but the inferred signature is: ~s",
			      [M, F, A, SigString]),
	  fatal_error(Msg);
	{error, ErrorStr} when is_list(ErrorStr) -> % ErrorStr is a string()
	  Msg = io_lib:format("Error in contract of function ~w:~w/~w: ~s",
			      [M, F, A, ErrorStr]),
	  fatal_error(Msg)
      end
  end.

get_functions(File, Analysis) ->
  case Analysis#analysis.mode of
    ?SHOW ->
      Funcs = map__lookup(File, Analysis#analysis.func),
      Inc_Funcs = map__lookup(File, Analysis#analysis.inc_func),
      remove_module_info(Funcs) ++ normalize_incFuncs(Inc_Funcs);
    ?SHOW_EXPORTED ->
      Ex_Funcs = map__lookup(File, Analysis#analysis.ex_func),
      remove_module_info(Ex_Funcs);
    ?ANNOTATE ->
      Funcs = map__lookup(File, Analysis#analysis.func),
      remove_module_info(Funcs);
    ?ANNOTATE_INC_FILES ->
      map__lookup(File, Analysis#analysis.inc_func)
  end.

normalize_incFuncs(Functions) ->
  [FunInfo || {_FileName, FunInfo} <- Functions].

-spec remove_module_info([func_info()]) -> [func_info()].

remove_module_info(FunInfoList) ->
  F = fun ({_,module_info,0}) -> false;
	  ({_,module_info,1}) -> false;
	  ({Line,F,A}) when is_integer(Line), is_atom(F), is_integer(A) -> true
      end,
  lists:filter(F, FunInfoList).

write_typed_file(File, Info) ->
  io:format("      Processing file: ~p\n", [File]),
  Dir = filename:dirname(File),
  RootName = filename:basename(filename:rootname(File)),
  Ext = filename:extension(File),
  TyperAnnDir = filename:join(Dir, ?TYPER_ANN_DIR),
  TmpNewFilename = lists:concat([RootName, ".ann", Ext]),
  NewFileName = filename:join(TyperAnnDir, TmpNewFilename),
  case file:make_dir(TyperAnnDir) of
    {error, Reason} ->
      case Reason of
	eexist -> %% TypEr dir exists; remove old typer files if they exist
	  case file:delete(NewFileName) of
	    ok -> ok;
	    {error, enoent} -> ok;
	    {error, _} ->
	      Msg = io_lib:format("Error in deleting file ~s\n", [NewFileName]),
	      fatal_error(Msg)
	  end,
	  write_typed_file(File, Info, NewFileName);
	enospc ->
	  Msg = io_lib:format("Not enough space in ~p\n", [Dir]),
	  fatal_error(Msg);
	eacces ->
	  Msg = io_lib:format("No write permission in ~p\n", [Dir]),
	  fatal_error(Msg);
	_ ->
	  Msg = io_lib:format("Unhandled error ~s when writing ~p\n",
			      [Reason, Dir]),
	  fatal_error(Msg)
      end;
    ok -> %% Typer dir does NOT exist
      write_typed_file(File, Info, NewFileName)
  end.

write_typed_file(File, Info, NewFileName) ->
  {ok, Binary} = file:read_file(File),
  Chars = binary_to_list(Binary),
  write_typed_file(Chars, NewFileName, Info, 1, []),
  io:format("             Saved as: ~p\n", [NewFileName]).

write_typed_file(Chars, File, #info{functions = []}, _LNo, _Acc) ->
  ok = file:write_file(File, list_to_binary(Chars), [append]);
write_typed_file([Ch|Chs] = Chars, File, Info, LineNo, Acc) ->
  [{Line,F,A}|RestFuncs] = Info#info.functions,
  case Line of
    1 -> %% This will happen only for inc files
      ok = raw_write(F, A, Info, File, []),
      NewInfo = Info#info{functions = RestFuncs},
      NewAcc = [],
      write_typed_file(Chars, File, NewInfo, Line, NewAcc);
    _ ->
      case Ch of
	10 ->
	  NewLineNo = LineNo + 1,
	  {NewInfo, NewAcc} =
	    case NewLineNo of
	      Line ->
		ok = raw_write(F, A, Info, File, [Ch|Acc]),
		{Info#info{functions = RestFuncs}, []};
	      _ ->
		{Info, [Ch|Acc]}
	    end,
	  write_typed_file(Chs, File, NewInfo, NewLineNo, NewAcc);
	_ ->
	  write_typed_file(Chs, File, Info, LineNo, [Ch|Acc])
      end
  end.

raw_write(F, A, Info, File, Content) ->
  TypeInfo = get_type_string(F, A, Info, file),
  ContentList = lists:reverse(Content) ++ TypeInfo ++ "\n",
  ContentBin = list_to_binary(ContentList),
  file:write_file(File, ContentBin, [append]).

get_type_string(F, A, Info, Mode) ->
  Type = get_type_info({F,A}, Info#info.types),
  TypeStr =
    case Type of
      {contract, C} -> 
        dialyzer_contracts:contract_to_string(C);
      {RetType, ArgType} ->
	Sig = erl_types:t_fun(ArgType, RetType),
        dialyzer_utils:format_sig(Sig, Info#info.records)
    end,
  case Info#info.edoc of
    false ->
      case {Mode, Type} of
	{file, {contract, _}} -> "";
	_ ->
	  Prefix = lists:concat(["-spec ", erl_types:atom_to_string(F)]),
	  lists:concat([Prefix, TypeStr, "."])
      end;
    true ->
      Prefix = lists:concat(["%% @spec ", F]),
      lists:concat([Prefix, TypeStr, "."])
  end.
 
show_type_info(File, Info) ->
  io:format("\n%% File: ~p\n%% ", [File]),
  OutputString = lists:concat(["~.", length(File)+8, "c~n"]),
  io:fwrite(OutputString, [$-]),
  Fun = fun ({_LineNo, F, A}) ->
	    TypeInfo = get_type_string(F, A, Info, show),
	    io:format("~s\n", [TypeInfo])
	end,
  lists:foreach(Fun, Info#info.functions).

get_type_info(Func, Types) ->
  case map__lookup(Func, Types) of
    none ->
      %% Note: Typeinfo of any function should exist in
      %% the result offered by dialyzer, otherwise there 
      %% *must* be something wrong with the analysis
      Msg = io_lib:format("No type info for function: ~p\n", [Func]),
      fatal_error(Msg);
    {contract, _Fun} = C -> C;
    {_RetType, _ArgType} = RA -> RA 
  end.

%%--------------------------------------------------------------------
%% Processing of command-line options and arguments.
%%--------------------------------------------------------------------

-spec process_cl_args() -> {args(), analysis()}.

process_cl_args() ->
  ArgList = init:get_plain_arguments(),
  %% io:format("Args is ~p\n", [ArgList]),
  {Args, Analysis} = analyze_args(ArgList, #args{}, #analysis{}),
  %% if the mode has not been set, set it to the default mode (show)
  {Args, case Analysis#analysis.mode of
	   undefined -> Analysis#analysis{mode = ?SHOW};
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
cl(["--show_success_typings"|Opts]) -> {show_succ, Opts};
cl(["--show-success-typings"|Opts]) -> {show_succ, Opts};
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
cl(["-pa",Dir|Opts]) -> {{pa,Dir}, Opts};
cl(["-pz",Dir|Opts]) -> {{pz,Dir}, Opts};
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
  {Args, Analysis#analysis{edoc = true}};
%% Get useful information for actual analysis
analyze_result({mode, Mode}, Args, Analysis) ->
  case Analysis#analysis.mode of
    undefined -> {Args, Analysis#analysis{mode = Mode}};
    OldMode -> mode_error(OldMode, Mode)
  end;
analyze_result({def, Val}, Args, Analysis) ->
  NewVal = Analysis#analysis.macros ++ [Val],
  {Args, Analysis#analysis{macros = NewVal}};
analyze_result({inc, Val}, Args, Analysis) ->
  NewVal = Analysis#analysis.includes ++ [Val],
  {Args, Analysis#analysis{includes = NewVal}};
analyze_result({plt, Plt}, Args, Analysis) ->
  {Args, Analysis#analysis{plt = Plt}};
analyze_result(show_succ, Args, Analysis) ->
  {Args, Analysis#analysis{show_succ = true}};
analyze_result(no_spec, Args, Analysis) ->
  {Args, Analysis#analysis{no_spec = true}};
analyze_result({pa, Dir}, Args, Analysis) ->
  true = code:add_patha(Dir),
  {Args, Analysis};
analyze_result({pz, Dir}, Args, Analysis) ->
  true = code:add_pathz(Dir),
  {Args, Analysis}.

%%--------------------------------------------------------------------
%% File processing.
%%--------------------------------------------------------------------

-spec get_all_files(args()) -> [file:filename(),...].

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
	dialyzer_plt:merge_plts([Analysis#analysis.trust_plt, DialyzerPlt])
    catch
      throw:{dialyzer_error,_Reason} ->
	fatal_error("Dialyzer's PLT is missing or is not up-to-date; please (re)create it")
    end,
  NewAnalysis = lists:foldl(fun collect_one_file_info/2, 
			    Analysis#analysis{trust_plt = NewPlt}, 
			    Analysis#analysis.files),
  %% Process Remote Types
  TmpCServer = NewAnalysis#analysis.codeserver,
  NewCServer =
    try
      TmpCServer1 = dialyzer_utils:merge_types(TmpCServer, NewPlt),
      NewExpTypes = dialyzer_codeserver:get_temp_exported_types(TmpCServer),
      OldExpTypes = dialyzer_plt:get_exported_types(NewPlt),
      MergedExpTypes = sets:union(NewExpTypes, OldExpTypes),
      TmpCServer2 =
        dialyzer_codeserver:finalize_exported_types(MergedExpTypes, TmpCServer1),
      TmpCServer3 = dialyzer_utils:process_record_remote_types(TmpCServer2),
      dialyzer_contracts:process_contract_remote_types(TmpCServer3)
    catch
      throw:{error, ErrorMsg} ->
	fatal_error(ErrorMsg)
    end,
  NewAnalysis#analysis{codeserver = NewCServer}.

collect_one_file_info(File, Analysis) ->
  Ds = [{d,Name,Val} || {Name,Val} <- Analysis#analysis.macros],
  %% Current directory should also be included in "Includes".
  Includes = [filename:dirname(File)|Analysis#analysis.includes],
  Is = [{i,Dir} || Dir <- Includes],
  Options = dialyzer_utils:src_compiler_opts() ++ Is ++ Ds,
  case dialyzer_utils:get_core_from_src(File, Options) of
    {error, Reason} ->
      %% io:format("File=~p\n,Options=~p\n,Error=~p\n", [File,Options,Reason]),
      compile_error(Reason);
    {ok, Core} ->
      case dialyzer_utils:get_record_and_type_info(Core) of
	{error, Reason} -> compile_error([Reason]);
	{ok, Records} ->
	  Mod = cerl:concrete(cerl:module_name(Core)),
	  case dialyzer_utils:get_spec_info(Mod, Core, Records) of
	    {error, Reason} -> compile_error([Reason]);
	    {ok, SpecInfo, CbInfo} ->
	      ExpTypes = get_exported_types_from_core(Core),
	      analyze_core_tree(Core, Records, SpecInfo, CbInfo,
				ExpTypes, Analysis, File)
          end
      end
  end.

analyze_core_tree(Core, Records, SpecInfo, CbInfo, ExpTypes, Analysis, File) ->
  Module = cerl:concrete(cerl:module_name(Core)),
  TmpTree = cerl:from_records(Core),
  CS1 = Analysis#analysis.codeserver,
  NextLabel = dialyzer_codeserver:get_next_core_label(CS1),
  {Tree, NewLabel} = cerl_trees:label(TmpTree, NextLabel),
  CS2 = dialyzer_codeserver:insert(Module, Tree, CS1),
  CS3 = dialyzer_codeserver:set_next_core_label(NewLabel, CS2),
  CS4 = dialyzer_codeserver:store_temp_records(Module, Records, CS3),
  CS5 =
    case Analysis#analysis.no_spec of
      true -> CS4;
      false ->
	dialyzer_codeserver:store_temp_contracts(Module, SpecInfo, CbInfo, CS4)
    end,
  OldExpTypes = dialyzer_codeserver:get_temp_exported_types(CS5),
  MergedExpTypes = sets:union(ExpTypes, OldExpTypes),
  CS6 = dialyzer_codeserver:insert_temp_exported_types(MergedExpTypes, CS5),
  Ex_Funcs = [{0,F,A} || {_,_,{F,A}} <- cerl:module_exports(Tree)],
  CG = Analysis#analysis.callgraph,
  {V, E} = dialyzer_callgraph:scan_core_tree(Tree, CG),
  dialyzer_callgraph:add_edges(E, V, CG),
  Fun = fun analyze_one_function/2,
  All_Defs = cerl:module_defs(Tree),
  Acc = lists:foldl(Fun, #tmpAcc{file = File, module = Module}, All_Defs),
  Exported_FuncMap = map__insert({File, Ex_Funcs}, Analysis#analysis.ex_func),
  %% we must sort all functions in the file which
  %% originate from this file by *numerical order* of lineNo
  Sorted_Functions = lists:keysort(1, Acc#tmpAcc.funcAcc),
  FuncMap = map__insert({File, Sorted_Functions}, Analysis#analysis.func),
  %% we do not need to sort functions which are imported from included files
  IncFuncMap = map__insert({File, Acc#tmpAcc.incFuncAcc},
			   Analysis#analysis.inc_func),
  FMs = Analysis#analysis.fms ++ [{File, Module}],
  RecordMap = map__insert({File, Records}, Analysis#analysis.record),
  Analysis#analysis{fms = FMs,
		    callgraph = CG,
		    codeserver = CS6,
		    ex_func = Exported_FuncMap,
		    inc_func = IncFuncMap,
		    record = RecordMap,
		    func = FuncMap}.

analyze_one_function({Var, FunBody} = Function, Acc) ->
  F = cerl:fname_id(Var),
  A = cerl:fname_arity(Var),
  TmpDialyzerObj = {{Acc#tmpAcc.module, F, A}, Function},
  NewDialyzerObj = Acc#tmpAcc.dialyzerObj ++ [TmpDialyzerObj],  
  Anno = cerl:get_ann(FunBody),
  LineNo = get_line(Anno),
  FileName = get_file(Anno),
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

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.

get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> "no_file". % should not happen

-spec get_dialyzer_plt(analysis()) -> plt().

get_dialyzer_plt(#analysis{plt = PltFile0}) ->
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
  io:format(standard_error, "~s", [Msg]).

%%--------------------------------------------------------------------
%% Version and help messages.
%%--------------------------------------------------------------------

-spec version_message() -> no_return().

version_message() ->
  io:format("TypEr version "++?VSN++"\n"),
  erlang:halt(0).

-spec help_message() -> no_return().

help_message() ->
  S = <<" Usage: typer [--help] [--version] [--plt PLT] [--edoc]
              [--show | --show-exported | --annotate | --annotate-inc-files]
              [-Ddefine]* [-I include_dir]* [-pa dir]* [-pz dir]*
              [-T application]* [-r] file*

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
   -pa dir
   -pz dir
       Set code path options to TypEr
       (This is useful for files that use parse tranforms.)
   --version (or -v)
       prints the Typer version and exits
   --help (or -h)
       prints this message and exits

 Note:
   * denotes that multiple occurrences of these options are possible.
">>,
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
%% specialized for the uses in this module
%%--------------------------------------------------------------------

-type map_dict() :: dict:dict().

-spec map__new() -> map_dict().
map__new() ->
  dict:new().

-spec map__insert({term(), term()}, map_dict()) -> map_dict().
map__insert(Object, Map) ->
  {Key, Value} = Object,
  dict:store(Key, Value, Map).

-spec map__lookup(term(), map_dict()) -> term().
map__lookup(Key, Map) ->
  try dict:fetch(Key, Map) catch error:_ -> none end.

-spec map__from_list([{fa(), term()}]) -> map_dict().
map__from_list(List) ->
  dict:from_list(List).

-spec map__remove(term(), map_dict()) -> map_dict().
map__remove(Key, Dict) ->
  dict:erase(Key, Dict).

-spec map__fold(fun((term(), term(), term()) -> map_dict()), map_dict(), map_dict()) -> map_dict().
map__fold(Fun, Acc0, Dict) -> 
  dict:fold(Fun, Acc0, Dict).
