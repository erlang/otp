%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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
%%============================================================================
%% File    : typer_annotator.erl
%% Author  : Bingwen He <hebingwen@hotmail.com>
%% Description : 
%%    If file 'FILENAME' has been analyzed, then the output of
%%    command "diff -B FILENAME.erl typer_ann/FILENAME.ann.erl"
%%    should be exactly what TypEr has added, namely type info.
%%============================================================================

-module(typer_annotator).

-export([annotate/1]).

%%----------------------------------------------------------------------------

-include("typer.hrl").

 %%----------------------------------------------------------------------------

-define(TYPER_ANN_DIR, "typer_ann").

-type fun_info() :: {non_neg_integer(), atom(), arity()}.

-record(info, {records = typer_map:new() :: dict(),
	       functions = []            :: [fun_info()],
	       types                     :: dict(),
	       no_comment_specs = true	 :: boolean()}).
-record(inc, {map    = typer_map:new() :: dict(),
	      filter = []              :: [file:filename()]}).

%%----------------------------------------------------------------------------

-spec annotate(#typer_analysis{}) -> 'ok'.

annotate(#typer_analysis{mode = Mode, final_files = Files} = Analysis) ->
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
  NewInc = lists:foldl(Fun, #inc{}, Analysis#typer_analysis.final_files),
  clean_inc(NewInc).

write_inc_files(Inc) ->
  Fun =
    fun (File) ->
	Val = typer_map:lookup(File,Inc#inc.map),
	%% Val is function with its type info
	%% in form [{{Line,F,A},Type}]
	Functions = [Key || {Key,_} <- Val],
	Val1 = [{{F,A},Type} || {{_Line,F,A},Type} <- Val],
	Info = #info{types = typer_map:from_list(Val1),
		     records = typer_map:new(),
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
  lists:foreach(Fun, Analysis#typer_analysis.final_files).

get_final_info(File, Module, Analysis) ->
  Records = get_records(File, Analysis),
  Types = get_types(Module, Analysis, Records),
  Functions = get_functions(File, Analysis),
  Bool = Analysis#typer_analysis.contracts,
  #info{records = Records, functions = Functions,
	types = Types, no_comment_specs = Bool}.

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

-spec is_yecc_gen(file:filename(), #inc{}) -> {boolean(), #inc{}}.

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
  case typer_map:lookup(File, IncMap) of
    none -> %% File is not added. Add it
      Obj = {File,[{FA, {Line, Type}}]},
      NewMap = typer_map:insert(Obj, IncMap),
      Inc#inc{map = NewMap};
    Val -> %% File is already in. Check.
      case lists:keyfind(FA, 1, Val) of
	false ->
	  %% Function is not in; add it
	  Obj = {File, Val ++ [{FA, {Line, Type}}]},
	  NewMap = typer_map:insert(Obj, IncMap),
	  Inc#inc{map = NewMap};
	Type ->
	  %% Function is in and with same type
	  Inc;
	_ ->
	  %% Function is in but with diff type
	  inc_warning(FA, File),
	  Elem = lists:keydelete(FA, 1, Val),
	  NewMap = case Elem of
		     [] ->
		       typer_map:remove(File, IncMap);
		     _  ->
		       typer_map:insert({File, Elem}, IncMap)
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
	    I#inc{map = typer_map:remove(Key, Map)}
	end,
  lists:foldl(Fun, Inc, Filter).

normalize_obj(TmpInc) ->
  Fun = fun (Key, Val, Inc) ->
	    NewVal = [{{Line,F,A},Type} || {{F,A},{Line,Type}} <- Val],
	    typer_map:insert({Key,NewVal}, Inc)
	end,
  NewMap = typer_map:fold(Fun, typer_map:new(), TmpInc#inc.map),
  TmpInc#inc{map = NewMap}.

get_records(File, Analysis) ->
  typer_map:lookup(File, Analysis#typer_analysis.record).

get_types(Module, Analysis, Records) ->
  TypeInfoPlt = Analysis#typer_analysis.trust_plt,
  TypeInfo = 
    case dialyzer_plt:lookup_module(TypeInfoPlt, Module) of
      none -> [];
      {value, List} -> List
    end,
  CodeServer = Analysis#typer_analysis.code_server,
  TypeInfoList = [get_type(I, CodeServer, Records) || I <- TypeInfo],
  typer_map:from_list(TypeInfoList).

get_type({{M, F, A} = MFA, Range, Arg}, CodeServer, Records) ->
  case dialyzer_codeserver:lookup_mfa_contract(MFA, CodeServer) of
    error ->
      {{F, A}, {Range, Arg}};
    {ok, {_FileLine, Contract}} ->
      Sig = erl_types:t_fun(Arg, Range),
      case dialyzer_contracts:check_contract(Contract, Sig) of
	ok -> {{F, A}, {contract, Contract}};
	{error, {extra_range, _, _}} ->
	  {{F, A}, {contract, Contract}};
	{error, invalid_contract} ->
	  CString = dialyzer_contracts:contract_to_string(Contract),
	  SigString = dialyzer_utils:format_sig(Sig, Records),
	  typer:error(
	    io_lib:format("Error in contract of function ~w:~w/~w\n" 
			  "\t The contract is: " ++ CString ++ "\n" ++
			  "\t but the inferred signature is: ~s",
			  [M, F, A, SigString]));
	{error, Msg} when is_list(Msg) -> % Msg is a string()
	  typer:error(
	    io_lib:format("Error in contract of function ~w:~w/~w: ~s",
			  [M, F, A, Msg]))
      end
  end.

get_functions(File, Analysis) ->
  case Analysis#typer_analysis.mode of
    ?SHOW ->
      Funcs = typer_map:lookup(File, Analysis#typer_analysis.func),
      Inc_Funcs = typer_map:lookup(File, Analysis#typer_analysis.inc_func),
      remove_module_info(Funcs) ++ normalize_incFuncs(Inc_Funcs);
    ?SHOW_EXPORTED ->
      Ex_Funcs = typer_map:lookup(File, Analysis#typer_analysis.ex_func),
      remove_module_info(Ex_Funcs);
    ?ANNOTATE ->
      Funcs = typer_map:lookup(File, Analysis#typer_analysis.func),
      remove_module_info(Funcs);
    ?ANNOTATE_INC_FILES ->
      typer_map:lookup(File, Analysis#typer_analysis.inc_func)
  end.

normalize_incFuncs(Functions) ->
  [FunInfo || {_FileName, FunInfo} <- Functions].

-spec remove_module_info([fun_info()]) -> [fun_info()].

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
	eexist -> %% TypEr dir exists; remove old typer files
	  ok = file:delete(NewFileName),
	  write_typed_file(File, Info, NewFileName);
	enospc ->
	  io:format("  Not enough space in ~p\n", [Dir]);
	eacces ->
	  io:format("  No write permission in ~p\n", [Dir]);
	_ ->
	  io:format("Unhandled error ~s when writing ~p\n", [Reason, Dir]),
	  halt()
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
  case Info#info.no_comment_specs of
    true ->
      case {Mode, Type} of
	{file, {contract, _}} -> "";
	_ ->
	  Prefix = lists:concat(["-spec ", F]),
	  lists:concat([Prefix, TypeStr, "."])
      end;
    false ->
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
  case typer_map:lookup(Func, Types) of
    none ->
      %% Note: Typeinfo of any function should exist in
      %% the result offered by dialyzer, otherwise there 
      %% *must* be something wrong with the analysis
      io:format("No type info for function: ~p\n", [Func]),
      halt();
    {contract, _Fun} = C -> C;
    {_RetType, _ArgType} = RA -> RA 
  end.
