%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2004-2010 held by the authors. All Rights Reserved.
%% Copyright Ericsson AB 2022-2025. All Rights Reserved.
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

%%-----------------------------------------------------------------------
%% File        : typer_core.erl
%% Author(s)   : The first version of typer was written by Bingwen He
%%               with guidance from Kostis Sagonas and Tobias Lindahl.
%%               Since June 2008 typer is maintained by Kostis Sagonas.
%%               On 2022, Brujo Benavides, Pablo Costas, and Mackenzie
%%               Morgan from NextRoll started working on the rebar3
%%               plugin for typer and, with that in mind, splitted typer
%%               and typer_core apart.
%% Description : An Erlang/OTP module that shows type information
%%               for Erlang modules to the user.  Additionally, it can
%%               annotate the code of files with such type information.
%%-----------------------------------------------------------------------

-module(typer_core).
-moduledoc false.

-export([run/1]).

%%-----------------------------------------------------------------------

-define(SHOW, show).
-define(SHOW_EXPORTED, show_exported).
-define(ANNOTATE, annotate).
-define(ANNOTATE_IN_PLACE, annotate_in_place).
-define(ANNOTATE_INC_FILES, annotate_inc_files).

-type mode() :: ?SHOW | ?SHOW_EXPORTED | ?ANNOTATE | ?ANNOTATE_IN_PLACE | ?ANNOTATE_INC_FILES.

%%-----------------------------------------------------------------------

-type files()      :: [file:filename()].
-type callgraph()  :: dialyzer_callgraph:callgraph().
-type codeserver() :: dialyzer_codeserver:codeserver().
-type plt()        :: dialyzer_plt:plt().
-type printer(Return) :: fun((io:format(), [term()]) -> Return).
-type io() ::
    #{debug := printer(_),
      info := printer(_),
      warn := printer(_),
      abort := printer(no_return())}.
-type opts() ::
    #{mode := mode(),
      show_succ => boolean(),
      no_spec => boolean(),
      edoc => boolean(),
      plt => file:filename(),
      trusted => files(),
      files => files(),
      files_r => files(),
      macros => [{atom(), term()}],
      includes => files(),
      io => io()}.

-export_type([mode/0, opts/0]).

-record(analysis,
        {mode                            :: mode() | 'undefined',
         macros     = []                 :: [{atom(), term()}],
         includes   = []                 :: files(),
         codeserver = dialyzer_codeserver:new():: codeserver(),
         callgraph  = dialyzer_callgraph:new() :: callgraph(),
         files      = []                 :: files(),   % absolute names
         plt        = none               :: 'none' | file:filename(),
         no_spec    = false              :: boolean(),
         show_succ  = false              :: boolean(),
         %% For choosing between specs or edoc @spec comments
         edoc       = false              :: boolean(),
         %% Files in 'fms' are compilable with option 'to_pp'; we keep them
         %% as {FileName, ModuleName} in case the ModuleName is different
         fms        = []                 :: [{file:filename(), module()}],
         ex_func = maps:new() :: #{file:filename() => [func_info()]},
         record = maps:new() :: #{file:filename() => erl_types:type_table()},
         func = maps:new() :: #{file:filename() => [func_info()]},
         inc_func = maps:new() :: #{file:filename() => [inc_file_info()]},
         trust_plt  = dialyzer_plt:new() :: plt(),
         io         = default_io()       :: io()}).
-type analysis() :: #analysis{}.

-record(args, {files   = [] :: files(),
               files_r = [] :: files(),
               trusted = [] :: files()}).
-type args() :: #args{}.

%%--------------------------------------------------------------------

-spec run(opts()) -> ok.

run(Opts) ->
  {Args, Analysis} = process_cl_args(Opts),
  msg(debug, "Opts: ~p\nArgs: ~p\nAnalysis: ~p", [Opts, Args, Analysis], Analysis),
  Timer = dialyzer_timing:init(false),
  TrustedFiles = filter_fd(Args#args.trusted, [], fun is_erl_file/1, Analysis),
  Analysis2 = extract(Analysis, TrustedFiles),
  All_Files = get_all_files(Args, Analysis2),
  msg(debug, "All_Files: ~tp", [All_Files], Analysis2),
  Analysis3 = Analysis2#analysis{files = All_Files},
  Analysis4 = collect_info(Analysis3),
  msg(debug, "Final: ~p", [Analysis4#analysis.fms], Analysis3),
  TypeInfo = get_type_info(Analysis4),
  dialyzer_timing:stop(Timer),
  show_or_annotate(TypeInfo),
  msg(debug, "\nTyper analysis finished", [], Analysis4),
  ok.

%%--------------------------------------------------------------------

-spec extract(analysis(), files()) -> analysis().

extract(#analysis{macros = Macros,
                  includes = Includes,
                  trust_plt = TrustPLT} = Analysis, TrustedFiles) ->
  msg(debug, "Extracting trusted typer info...", [], Analysis),
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
                  {error, Reason} -> compile_error([Reason], Analysis)
                end;
              {error, Reason} -> compile_error([Reason], Analysis)
            end;
          {error, Reason} -> compile_error(Reason, Analysis)
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
        compile_error(ErrorMsg, Analysis)
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
  StrippedCallGraph = remove_external(CallGraph, TrustPLT, Analysis),
  msg(debug, "Analyizing callgraph...", [], Analysis),
  try
    NewPlt = dialyzer_succ_typings:analyze_callgraph(StrippedCallGraph,
                                                     TrustPLT,
                                                     CodeServer,
                                                     none, []),
    Analysis#analysis{callgraph = StrippedCallGraph, trust_plt = NewPlt}
  catch
    error:What:Stacktrace ->
      fatal_error(io_lib:format("Analysis failed with message: ~tp",
                                [{What, Stacktrace}]), Analysis);
    throw:{dialyzer_succ_typing_error, Msg} ->
      fatal_error(io_lib:format("Analysis failed with message: ~ts", [Msg]), Analysis)
  end.

-spec remove_external(callgraph(), plt(), analysis()) -> callgraph().

remove_external(CallGraph, PLT, Analysis) ->
  {StrippedCG0, Ext} = dialyzer_callgraph:remove_external(CallGraph),
  case get_external(Ext, PLT) of
    [] -> ok;
    Externals ->
      msg(warn, " Unknown functions: ~tp", [lists:usort(Externals)], Analysis),
      ExtTypes = rcv_ext_types(),
      case ExtTypes of
        [] -> ok;
        _ -> msg(warn, " Unknown types: ~tp", [ExtTypes], Analysis)
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
-type func_info() :: {line(), atom(), arity()}.
-type func_name() :: {atom(), arity()}.
-type func_type() ::
    {contract, dialyzer_contract()} |
    {erl_types:erl_type(), [erl_types:erl_type()] | arity()}.
-type func_types() :: #{func_name() => func_type()}.
%% This is a record (#contract{}) defined in dialyzer.hrl
%% but we can't include_lib that file since it also defines
%% #analysis
-type dialyzer_contract() :: tuple().

-record(info, {records = maps:new() :: erl_types:type_table(),
               functions = []       :: [func_info()],
               types = maps:new() :: func_types(),
               edoc = false         :: boolean()}).
-record(inc,
        {map = maps:new() :: #{file:filename() => [{func_name(), {pos_integer(), func_type()}}]},
         filter = [] :: [file:filename()]}).
-type inc() :: #inc{}.

-spec show_or_annotate(analysis()) -> 'ok'.

show_or_annotate(#analysis{mode = Mode, fms = Files} = Analysis) ->
  case Mode of
    ?SHOW -> show(Analysis);
    ?SHOW_EXPORTED -> show(Analysis);
    Mode when Mode == ?ANNOTATE; Mode == ?ANNOTATE_IN_PLACE ->
      Fun = fun ({File, Module}) ->
                Info = get_final_info(File, Module, Analysis),
                write_typed_file(File, Info, Analysis)
            end,
      lists:foreach(Fun, Files);
    ?ANNOTATE_INC_FILES ->
      IncInfo = write_and_collect_inc_info(Analysis),
      write_inc_files(IncInfo, Analysis)
  end.

write_and_collect_inc_info(Analysis) ->
  Fun = fun ({File, Module}, Inc) ->
            Info = get_final_info(File, Module, Analysis),
            write_typed_file(File, Info, Analysis),
            IncFuns = get_inc_functions(File, Analysis),
            collect_imported_functions(IncFuns, Info#info.types, Inc, Analysis)
        end,
  NewInc = lists:foldl(Fun, #inc{}, Analysis#analysis.fms),
  clean_inc(NewInc).

write_inc_files(Inc, Analysis) ->
  Fun =
    fun (File) ->
        Val = maps:get(File, Inc#inc.map, none),
        %% Val is function with its type info
        %% in form [{{Line,F,A},Type}]
        Functions = [Key || {Key, _} <- Val],
        Val1 = [{{F,A},Type} || {{_Line,F,A},Type} <- Val],
        Info = #info{types = maps:from_list(Val1),
                     records = maps:new(),
                     %% Note we need to sort functions here!
                     functions = lists:keysort(1, Functions)},
        msg(debug, "Types ~tp", [Info#info.types], Analysis),
        msg(debug, "Functions ~tp", [Info#info.functions], Analysis),
        msg(debug, "Records ~tp", [Info#info.records], Analysis),
        write_typed_file(File, Info, Analysis)
    end,
  lists:foreach(Fun, maps:keys(Inc#inc.map)).

show(Analysis) ->
  Fun = fun ({File, Module}) ->
            Info = get_final_info(File, Module, Analysis),
            show_type_info(File, Info, Analysis)
        end,
  lists:foreach(Fun, Analysis#analysis.fms).

get_final_info(File, Module, Analysis) ->
  Records = get_records(File, Analysis),
  Types = get_types(Module, Analysis, Records),
  Functions = get_functions(File, Analysis),
  Edoc = Analysis#analysis.edoc,
  #info{records = Records, functions = Functions, types = Types, edoc = Edoc}.

collect_imported_functions(Functions, Types, Inc, Analysis) ->
  %% Coming from other sourses, including:
  %% FIXME: How to deal with yecc-generated file????
  %%     --.yrl (yecc-generated file)???
  %%     -- yeccpre.hrl (yecc-generated file)???
  %%     -- other cases
  Fun = fun ({File, _} = Obj, I) ->
            case is_yecc_gen(File, I) of
              {true, NewI} -> NewI;
              {false, NewI} ->
                check_imported_functions(Obj, NewI, Types, Analysis)
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

check_imported_functions({File, {Line, F, A}}, Inc, Types, Analysis) ->
  IncMap = Inc#inc.map,
  FA = {F, A},
  Type = get_type_info(FA, Types, Analysis),
  case IncMap of
      #{File := Val} -> %% File is already in. Check.
          case lists:keyfind(FA, 1, Val) of
              false ->
                  %% Function is not in; add it
                  Inc#inc{map = IncMap#{File => Val ++ [{FA, {Line, Type}}]}};
              {FA, {_, Type}} ->
                  %% Function is in and with same type
                  Inc;
              _ ->
                  %% Function is in but with diff type
                  inc_warning(FA, File, Analysis),
                  Elem = lists:keydelete(FA, 1, Val),
                  NewMap =
                      case Elem of
                          [] -> maps:remove(File, IncMap);
                          _  -> IncMap#{File => Elem}
                      end,
                  Inc#inc{map = NewMap}
          end;
      _ -> %% File is not added. Add it
          NewMap = IncMap#{File => [{FA, {Line, Type}}]},
          Inc#inc{map = NewMap}
  end.

inc_warning({F, A}, File, Analysis) ->
    msg(warn,
        "      ***Warning: Skip function ~tp/~p "
        "in file ~tp because of inconsistent type",
        [F, A, File],
        Analysis).

clean_inc(Inc) ->
  Inc1 = remove_yecc_generated_file(Inc),
  normalize_obj(Inc1).

remove_yecc_generated_file(#inc{filter = Filter} = Inc) ->
  Fun = fun(Key, #inc{map = Map} = I) -> I#inc{map = maps:remove(Key, Map)} end,
  lists:foldl(Fun, Inc, Filter).

normalize_obj(TmpInc) ->
  Fun = fun (Key, Val, Inc) ->
            NewVal = [{{Line,F,A},Type} || {{F,A},{Line,Type}} <- Val],
            Inc#{Key => NewVal}
        end,
  TmpInc#inc{map = maps:fold(Fun, maps:new(), TmpInc#inc.map)}.

get_records(File, Analysis) ->
  maps:get(File, Analysis#analysis.record, none).

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
        [get_type(I, CodeServer, Records, Analysis) || I <- TypeInfo]
    end,
  maps:from_list(TypeInfoList).

convert_type_info({{_M, F, A}, Range, Arg}) ->
  {{F, A}, {Range, Arg}}.

get_type({{M, F, A} = MFA, Range, Arg}, CodeServer, Records, Analysis) ->
  case dialyzer_codeserver:lookup_mfa_contract(MFA, CodeServer) of
    error ->
      {{F, A}, {Range, Arg}};
    {ok, {_FileLine, Contract, _Xtra}} ->
      Sig = erl_types:t_fun(Arg, Range),
      case dialyzer_contracts:check_contract(Contract, Sig, M) of
        ok -> {{F, A}, {contract, Contract}};
        {range_warnings, _} ->
          {{F, A}, {contract, Contract}};
        {error, {overlapping_contract, []}} ->
          {{F, A}, {contract, Contract}};
        {error, {invalid_contract, _}} ->
          CString = dialyzer_contracts:contract_to_string(Contract),
          SigString = dialyzer_utils:format_sig(Sig, Records),
          Msg = io_lib:format("Error in contract of function ~w:~tw/~w\n"
                              "\t The contract is: " ++ CString ++ "\n" ++
                              "\t but the inferred signature is: ~ts",
                              [M, F, A, SigString]),
          fatal_error(Msg, Analysis);
        {error, ErrorStr} when is_list(ErrorStr) -> % ErrorStr is a string()
          Msg = io_lib:format("Error in contract of function ~w:~tw/~w: ~ts",
                              [M, F, A, ErrorStr]),
          fatal_error(Msg, Analysis)
      end
  end.

get_functions(File, Analysis) ->
  case Analysis#analysis.mode of
    ?SHOW ->
      Funcs = maps:get(File, Analysis#analysis.func, none),
      IncFuncs = maps:get(File, Analysis#analysis.inc_func, none),
      remove_module_info(Funcs) ++ normalize_inc_funcs(IncFuncs);
    ?SHOW_EXPORTED ->
      ExFuncs = maps:get(File, Analysis#analysis.ex_func, none),
      remove_module_info(ExFuncs);
    Mode when Mode == ?ANNOTATE; Mode == ?ANNOTATE_IN_PLACE ->
      Funcs = maps:get(File, Analysis#analysis.func, none),
      remove_module_info(Funcs);
    ?ANNOTATE_INC_FILES ->
      normalize_inc_funcs(maps:get(File, Analysis#analysis.inc_func, none))
  end.

-spec get_inc_functions(file:filename(), analysis()) -> [{file:filename(), func_info()}].
get_inc_functions(File, Analysis) ->
    case Analysis#analysis.mode of
        show ->
            Funcs = maps:get(File, Analysis#analysis.func, none),
            IncFuncs = maps:get(File, Analysis#analysis.inc_func, none),
            extend_functions(File, remove_module_info(Funcs)) ++ IncFuncs;
        show_exported ->
            ExFuncs = maps:get(File, Analysis#analysis.ex_func, none),
            extend_functions(File, remove_module_info(ExFuncs));
        Mode when Mode =:= annotate orelse Mode =:= annotate_in_place ->
            Funcs = maps:get(File, Analysis#analysis.func, none),
            extend_functions(File, remove_module_info(Funcs));
        annotate_inc_files ->
            maps:get(File, Analysis#analysis.inc_func, none)
    end.

extend_functions(FileName, Functions) ->
    [{FileName, FunInfo} || FunInfo <- Functions].

normalize_inc_funcs(Functions) ->
  [FunInfo || {_FileName, FunInfo} <- Functions].

-spec remove_module_info([func_info()]) -> [func_info()].

remove_module_info(FunInfoList) ->
  F = fun ({_,module_info,0}) -> false;
          ({_,module_info,1}) -> false;
          ({Line,F,A}) when is_integer(Line), is_atom(F), is_integer(A) -> true
      end,
  lists:filter(F, FunInfoList).

write_typed_file(File, Info, Analysis) ->
  msg(info, "      Processing file: ~tp", [File], Analysis),
  Dir = filename:dirname(File),
  RootName = filename:basename(filename:rootname(File)),
  Ext = filename:extension(File),
  case Analysis#analysis.mode of
    ?ANNOTATE_IN_PLACE ->
      write_typed_file(File, Info, File, Analysis);
    _ ->
        TyperAnnDir = filename:join(Dir, ?TYPER_ANN_DIR),
        TmpNewFilename = lists:concat([RootName, ".ann", Ext]),
        NewFileName = filename:join(TyperAnnDir, TmpNewFilename),
        case file:make_dir(TyperAnnDir) of
          {error, Reason} ->
            case Reason of
          eexist -> %% TypEr dir exists; remove old typer files if they exist
            delete_file(NewFileName, Analysis),
            write_typed_file(File, Info, NewFileName, Analysis);
          enospc ->
            Msg = io_lib:format("Not enough space in ~tp", [Dir]),
            fatal_error(Msg, Analysis);
          eacces ->
            Msg = io_lib:format("No write permission in ~tp", [Dir]),
            fatal_error(Msg, Analysis);
          _ ->
            Msg = io_lib:format("Unhandled error ~ts when writing ~tp",
                              [Reason, Dir]),
            fatal_error(Msg, Analysis)
              end;
            ok -> %% Typer dir does NOT exist
              write_typed_file(File, Info, NewFileName, Analysis)
        end
  end.

-spec delete_file(file:filename_all(), analysis()) -> ok.
delete_file(File, Analysis) ->
  case file:delete(File) of
    ok ->
      ok;
    {error, enoent} ->
      ok;
    {error, _} ->
      Msg = io_lib:format("Error in deleting file ~ts", [File]),
      fatal_error(Msg, Analysis)
  end.

write_typed_file(File, Info, NewFileName, Analysis) ->
  {ok, Binary} = file:read_file(File),
  case Analysis#analysis.mode of
    ?ANNOTATE_IN_PLACE ->
      delete_file(NewFileName, Analysis);
    _ ->
      ok
  end,
  Chars = unicode:characters_to_list(Binary),
  write_typed_file(Chars, NewFileName, Info, 1, [], Analysis),
  msg(info, "             Saved as: ~tp", [NewFileName], Analysis).

write_typed_file(Chars, File, #info{functions = []}, _LNo, _Acc, _Analysis) ->
  ok = file:write_file(File, unicode:characters_to_binary(Chars), [append]);
write_typed_file([Ch|Chs] = Chars, File, Info, LineNo, Acc, Analysis) ->
  [{Line,F,A}|RestFuncs] = Info#info.functions,
  case Line of
    1 -> %% This will happen only for inc files
      ok = raw_write(F, A, Info, File, [], Analysis),
      NewInfo = Info#info{functions = RestFuncs},
      NewAcc = [],
      write_typed_file(Chars, File, NewInfo, Line, NewAcc, Analysis);
    _ ->
      case Ch of
        10 ->
          NewLineNo = LineNo + 1,
          {NewInfo, NewAcc} =
            case NewLineNo of
              Line ->
                ok = raw_write(F, A, Info, File, [Ch|Acc], Analysis),
                {Info#info{functions = RestFuncs}, []};
              _ ->
                {Info, [Ch|Acc]}
            end,
          write_typed_file(Chs, File, NewInfo, NewLineNo, NewAcc, Analysis);
        _ ->
          write_typed_file(Chs, File, Info, LineNo, [Ch|Acc], Analysis)
      end
  end.

raw_write(F, A, Info, File, Content, #analysis{mode = Mode} = Analysis) ->
  TypeInfo = get_type_string(F, A, Info, file, Analysis),
  ContentList =
    case {TypeInfo, Mode} of
      %% TypeInfo will be an empty string for functions that already have specs.
      %% In this case, when annotating directly on the files (with annotate_in_place),
      %% we don't want to add a newline character on the spec line, as presumably it should
      %% already have one.
      {"", ?ANNOTATE_IN_PLACE} ->
        lists:reverse(Content) ++ TypeInfo;
      _ ->
        lists:reverse(Content) ++ TypeInfo ++ "\n"
    end,
  ContentBin = unicode:characters_to_binary(ContentList),
  file:write_file(File, ContentBin, [append]).

get_type_string(F, A, Info, Mode, Analysis) ->
  Type = get_type_info({F,A}, Info#info.types, Analysis),
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

show_type_info(File, Info, Analysis) ->
  msg(info, "\n%% File: ~tp", [File], Analysis),
  OutputString = lists:concat(["~.", length(File) + 8, "c"]),
  msg(info, [$%, $%, $\s | OutputString], [$-], Analysis),
  Fun = fun ({_LineNo, F, A}) ->
            TypeInfo = get_type_string(F, A, Info, show, Analysis),
            msg(info, "~ts", [TypeInfo], Analysis)
        end,
  lists:foreach(Fun, Info#info.functions).

get_type_info(Func, Types, Analysis) ->
  case Types of
        #{Func := {contract, _Fun} = C} ->
            C;
        #{Func := {_RetType, _ArgType} = RA} ->
            RA;
        _ ->
      %% Note: Typeinfo of any function should exist in
      %% the result offered by dialyzer, otherwise there
      %% *must* be something wrong with the analysis
      Msg = io_lib:format("No type info for function: ~tp\n", [Func]),
            fatal_error(Msg, Analysis)
  end.

%%--------------------------------------------------------------------
%% Processing of command-line options and arguments.
%%--------------------------------------------------------------------

-spec process_cl_args(opts()) -> {args(), analysis()}.

process_cl_args(Opts) ->
    analyze_args(maps:to_list(Opts), #args{}, #analysis{}).

analyze_args([], Args, Analysis) ->
  {Args, Analysis};
analyze_args([Result | Rest], Args, Analysis) ->
  {NewArgs, NewAnalysis} = analyze_result(Result, Args, Analysis),
  analyze_args(Rest, NewArgs, NewAnalysis).

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
analyze_result({edoc, Value}, Args, Analysis) ->
  {Args, Analysis#analysis{edoc = Value}};
analyze_result({io, Val}, Args, Analysis) ->
  {Args, Analysis#analysis{io = Val}};
%% Get useful information for actual analysis
analyze_result({mode, Mode}, Args, Analysis) ->
  {Args, Analysis#analysis{mode = Mode}};
analyze_result({macros, Macros}, Args, Analysis) ->
  {Args, Analysis#analysis{macros = Macros}};
analyze_result({includes, Includes}, Args, Analysis) ->
  {Args, Analysis#analysis{includes = Includes}};
analyze_result({plt, Plt}, Args, Analysis) ->
  {Args, Analysis#analysis{plt = Plt}};
analyze_result({show_succ, Value}, Args, Analysis) ->
  {Args, Analysis#analysis{show_succ = Value}};
analyze_result({no_spec, Value}, Args, Analysis) ->
  {Args, Analysis#analysis{no_spec = Value}}.

%%--------------------------------------------------------------------
%% File processing.
%%--------------------------------------------------------------------

-spec get_all_files(args(), analysis()) -> [file:filename(), ...].

get_all_files(#args{files = Fs, files_r = Ds}, Analysis) ->
    case filter_fd(Fs, Ds, fun test_erl_file_exclude_ann/1, Analysis) of
        [] ->
            fatal_error("no file(s) to analyze", Analysis);
        AllFiles ->
            AllFiles
    end.

-spec test_erl_file_exclude_ann(file:filename()) -> boolean().

test_erl_file_exclude_ann(File) ->
  case is_erl_file(File) of
    true -> %% Exclude files ending with ".ann.erl"
      case re:run(File, "[\.]ann[\.]erl$", [unicode]) of
        {match, _} -> false;
        nomatch -> true
      end;
    false -> false
  end.

-spec is_erl_file(file:filename()) -> boolean().

is_erl_file(File) ->
  filename:extension(File) =:= ".erl".

-type test_file_fun() :: fun((file:filename()) -> boolean()).

-spec filter_fd(files(), files(), test_file_fun(), analysis()) -> files().

filter_fd(File_Dir, Dir_R, Fun, Analysis) ->
  All_File_1 = process_file_and_dir(File_Dir, Fun, Analysis),
  All_File_2 = process_dir_rec(Dir_R, Fun, Analysis),
  remove_dup(All_File_1 ++ All_File_2).

-spec process_file_and_dir(files(), test_file_fun(), analysis()) -> files().

process_file_and_dir(File_Dir, TestFun, Analysis) ->
  Fun =
    fun (Elem, Acc) ->
        case filelib:is_regular(Elem) of
          true  -> process_file(Elem, TestFun, Acc);
          false -> check_dir(Elem, false, Acc, TestFun, Analysis)
        end
    end,
  lists:foldl(Fun, [], File_Dir).

-spec process_dir_rec(files(), test_file_fun(), analysis()) -> files().

process_dir_rec(Dirs, TestFun, Analysis) ->
  Fun = fun (Dir, Acc) -> check_dir(Dir, true, Acc, TestFun, Analysis) end,
  lists:foldl(Fun, [], Dirs).

-spec check_dir(file:filename(), boolean(), files(), test_file_fun(), analysis()) -> files().

check_dir(Dir, Recursive, Acc, Fun, Analysis) ->
  case file:list_dir(Dir) of
    {ok, Files} ->
      {TmpDirs, TmpFiles} = split_dirs_and_files(Files, Dir),
      case Recursive of
        false ->
          FinalFiles = process_file_and_dir(TmpFiles, Fun, Analysis),
          Acc ++ FinalFiles;
        true ->
          TmpAcc1 = process_file_and_dir(TmpFiles, Fun, Analysis),
          TmpAcc2 = process_dir_rec(TmpDirs, Fun, Analysis),
          Acc ++ TmpAcc1 ++ TmpAcc2
      end;
    {error, eacces} ->
      fatal_error("no access permission to dir \""++Dir++"\"", Analysis);
    {error, enoent} ->
      fatal_error("cannot access "++Dir++": No such file or directory", Analysis);
    {error, _Reason} ->
      fatal_error("error involving a use of file:list_dir/1", Analysis)
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

-record(tmpAcc, {file             :: file:filename(),
                 module           :: atom(),
                 funcAcc = []     :: [func_info()],
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
        fatal_error("Dialyzer's PLT is missing or is not up-to-date; please (re)create it",
                    Analysis)
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
        fatal_error(ErrorMsg, NewAnalysis)
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
      msg(debug, "File=~tp\n,Options=~p\n,Error=~p", [File, Options, Reason], Analysis),
      compile_error(Reason, Analysis);
    {ok, Core} ->
      case dialyzer_utils:get_record_and_type_info(Core) of
        {error, Reason} -> compile_error([Reason], Analysis);
        {ok, Records} ->
          Mod = cerl:concrete(cerl:module_name(Core)),
          case dialyzer_utils:get_spec_info(Mod, Core, Records) of
            {error, Reason} -> compile_error([Reason], Analysis);
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
  ExFuncs = [{0,F,A} || {_,_,{F,A}} <- cerl:module_exports(Tree)],
  CG = Analysis#analysis.callgraph,
  {V, E} = dialyzer_callgraph:scan_core_tree(Tree, CG),
  dialyzer_callgraph:add_edges(E, V, CG),
  Fun = fun analyze_one_function/2,
  All_Defs = cerl:module_defs(Tree),
  Acc = lists:foldl(Fun, #tmpAcc{file = File, module = Module}, All_Defs),
  ExportedFuncMap = maps:put(File, ExFuncs, Analysis#analysis.ex_func),
  %% we must sort all functions in the file which
  %% originate from this file by *numerical order* of lineNo
  SortedFunctions = lists:keysort(1, Acc#tmpAcc.funcAcc),
  FuncMap = maps:put(File, SortedFunctions, Analysis#analysis.func),
  %% we do not need to sort functions which are imported from included files
  IncFuncMap = maps:put(File, Acc#tmpAcc.incFuncAcc, Analysis#analysis.inc_func),
  FMs = Analysis#analysis.fms ++ [{File, Module}],
  RecordMap = maps:put(File, Records, Analysis#analysis.record),
  Analysis#analysis{fms = FMs,
                    callgraph = CG,
                    codeserver = CS6,
                    ex_func = ExportedFuncMap,
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
get_line([{Line, _Column} | _Tail]) when is_integer(Line) -> Line;
get_line([_|Tail]) -> get_line(Tail);
get_line([]) -> -1.

get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> "no_file". % should not happen

-spec get_dialyzer_plt(analysis()) -> plt().

get_dialyzer_plt(#analysis{plt = PltFile0}=Analysis) ->
  PltFile =
    case PltFile0 =:= none of
      true ->
        case filelib:is_regular(dialyzer_cplt:get_default_cplt_filename()) of
          true -> dialyzer_cplt:get_default_cplt_filename();
          false ->
            case filelib:is_regular(dialyzer_iplt:get_default_iplt_filename()) of
              true -> dialyzer_iplt:get_default_iplt_filename();
              false ->
                fatal_error(
                  "No PLT file given, and no existing PLT was found at default locations " ++
                    dialyzer_cplt:get_default_cplt_filename() ++
                    " and " ++
                    dialyzer_iplt:get_default_iplt_filename(),
                  Analysis)
            end
        end;
      false -> PltFile0
    end,
  case dialyzer_plt:plt_kind(PltFile) of
    cplt -> dialyzer_cplt:from_file(PltFile);
    iplt -> dialyzer_iplt:from_file(PltFile);
    bad_file -> fatal_error("Invalid PLT file at path " ++ PltFile, Analysis);
    no_file -> fatal_error("No PLT file found at path " ++ PltFile, Analysis)
  end.

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

-spec default_io() -> io().

default_io() ->
  #{debug => fun swallow_output/2,
    info => fun format/2,
    warn => fun format_on_stderr/2,
    abort => fun format_and_halt/2}.

-spec fatal_error(string(), analysis()) -> no_return().

fatal_error(Slogan, Analysis) ->
  msg(abort, "typer: ~ts", [Slogan], Analysis).

-spec compile_error([string()], analysis()) -> no_return().

compile_error(Reason, Analysis) ->
  JoinedString = lists:flatten([X ++ "\n" || X <- Reason]),
  Msg = "Analysis failed with error report:\n" ++ JoinedString,
  fatal_error(Msg, Analysis).

-spec msg(debug | info | warn | abort, io:format(), [term()], analysis()) -> _.

msg(Level, Format, Data, #analysis{io = Io}) ->
  Printer = maps:get(Level, Io, fun swallow_output/2),
  Printer(Format, Data).

-spec format(io:format(), [term()]) -> ok.

format(Format, Data) ->
  io:format(Format ++ "\n", Data).

-spec swallow_output(io:format(), [term()]) -> ok.

swallow_output(_Format, _Data) ->
  ok.

-spec format_on_stderr(io:format(), [term()]) -> ok.

format_on_stderr(Format, Data) ->
  io:format(standard_error, Format ++ "\n", Data).

-spec format_and_halt(io:format(), [term()]) -> no_return().

format_and_halt(Format, Data) ->
  format_on_stderr(Format, Data),
  erlang:halt(1).

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
