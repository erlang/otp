%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

-type func_info() :: {non_neg_integer(), atom(), arity()}.

-record(info, {recMap = typer_map:new() :: dict(),
	       funcs = []               :: [func_info()],
	       typeMap                  :: dict(),
	       contracts                :: boolean()}).
-record(inc, {map    = typer_map:new() :: dict(),
	      filter = []              :: [string()]}).

%%----------------------------------------------------------------------------

-spec annotate(#typer_analysis{}) -> 'ok'.

annotate(Analysis) ->
  case Analysis#typer_analysis.mode of
    ?SHOW -> show(Analysis);
    ?SHOW_EXPORTED -> show(Analysis);
    ?ANNOTATE ->
      Fun = fun({File, Module}) ->
		Info = get_final_info(File, Module, Analysis),
		write_typed_file(File, Info)
	    end,
      lists:foreach(Fun, Analysis#typer_analysis.final_files);
    ?ANNOTATE_INC_FILES ->
      IncInfo = write_and_collect_inc_info(Analysis),
      write_inc_files(IncInfo)
  end.

write_and_collect_inc_info(Analysis) ->
  Fun = fun({File, Module}, Inc) ->
	    Info = get_final_info(File, Module, Analysis),
	    write_typed_file(File, Info),
	    IncFuns = get_functions(File, Analysis),
	    collect_imported_funcs(IncFuns, Info#info.typeMap, Inc)
	end,
  NewInc = lists:foldl(Fun,#inc{}, Analysis#typer_analysis.final_files),
  clean_inc(NewInc).

write_inc_files(Inc) ->
  Fun =
    fun (File) ->
	Val = typer_map:lookup(File,Inc#inc.map),
	%% Val is function with its type info
	%% in form [{{Line,F,A},Type}]
	Functions = [Key || {Key,_} <- Val],
	Val1 = [{{F,A},Type} || {{_Line,F,A},Type} <- Val],
	Info = #info{typeMap = typer_map:from_list(Val1),
		     recMap = typer_map:new(),
		     %% Note we need to sort functions here!
		     funcs = lists:keysort(1, Functions)},
	%% io:format("TypeMap ~p\n", [Info#info.typeMap]),
	%% io:format("Funcs ~p\n", [Info#info.funcs]),
	%% io:format("RecMap ~p\n", [Info#info.recMap]),
	write_typed_file(File, Info)
    end,
  lists:foreach(Fun, dict:fetch_keys(Inc#inc.map)).

show(Analysis) ->
  Fun = fun({File, Module}) -> 
	    Info = get_final_info(File, Module, Analysis),
	    show_type_info_only(File, Info)
	end,
  lists:foreach(Fun, Analysis#typer_analysis.final_files).

get_final_info(File, Module, Analysis) ->
  RecMap = get_recMap(File, Analysis),
  TypeMap = get_typeMap(Module, Analysis,RecMap),
  Functions = get_functions(File, Analysis),
  Contracts = Analysis#typer_analysis.contracts,
  #info{recMap=RecMap, funcs=Functions, typeMap=TypeMap, contracts=Contracts}.

collect_imported_funcs(Funcs, TypeMap, TmpInc) ->
  %% Coming from other sourses, including:
  %% FIXME: How to deal with yecc-generated file????
  %%     --.yrl (yecc-generated file)???
  %%     -- yeccpre.hrl (yecc-generated file)???
  %%     -- other cases
  Fun = fun({File,_} = Obj, Inc) ->
	    case is_yecc_file(File, Inc) of
	      {yecc_generated, NewInc} -> NewInc;
	      {not_yecc, NewInc} ->
		check_imported_funcs(Obj, NewInc, TypeMap)
	    end
	end,
  lists:foldl(Fun, TmpInc, Funcs).

-spec is_yecc_file(string(), #inc{}) -> {'not_yecc', #inc{}}
				      | {'yecc_generated', #inc{}}.
is_yecc_file(File, Inc) ->
  case lists:member(File, Inc#inc.filter) of
    true -> {yecc_generated, Inc};
    false ->
      case filename:extension(File) of
	".yrl" ->
	  Rootname = filename:rootname(File, ".yrl"),
	  Obj = Rootname ++ ".erl",
	  case lists:member(Obj, Inc#inc.filter) of
	    true -> {yecc_generated, Inc};
	    false ->
	      NewFilter = [Obj|Inc#inc.filter],
	      NewInc = Inc#inc{filter = NewFilter},
	      {yecc_generated, NewInc}
	  end;
	_ ->
	  case filename:basename(File) of
	    "yeccpre.hrl" -> {yecc_generated, Inc};
	    _ -> {not_yecc, Inc}
	  end
      end
  end.

check_imported_funcs({File, {Line, F, A}}, Inc, TypeMap) ->
  IncMap = Inc#inc.map,
  FA = {F, A},
  Type = get_type_info(FA, TypeMap),
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

remove_yecc_generated_file(TmpInc) ->
  Fun = fun(Key, Inc) ->
	    NewMap = typer_map:remove(Key, Inc#inc.map),
	    Inc#inc{map = NewMap}
	end,
  lists:foldl(Fun, TmpInc, TmpInc#inc.filter).
  
normalize_obj(TmpInc) ->
  Fun = fun(Key, Val, Inc) ->
	    NewVal = [{{Line,F,A},Type} || {{F,A},{Line,Type}} <- Val],
	    typer_map:insert({Key,NewVal}, Inc)
	end,
  NewMap = typer_map:fold(Fun, typer_map:new(), TmpInc#inc.map),
  TmpInc#inc{map = NewMap}.

get_recMap(File, Analysis) ->
  typer_map:lookup(File, Analysis#typer_analysis.record).

get_typeMap(Module, Analysis, RecMap) ->
  TypeInfoPlt = Analysis#typer_analysis.trust_plt,
  TypeInfo = 
    case dialyzer_plt:lookup_module(TypeInfoPlt, Module) of
      none -> [];
      {value, List} -> List
    end,
  CodeServer = Analysis#typer_analysis.code_server,
  TypeInfoList = [get_type(I, CodeServer, RecMap) || I <- TypeInfo],
  typer_map:from_list(TypeInfoList).

get_type({{M, F, A} = MFA, Range, Arg}, CodeServer, RecMap) ->
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
	  SigString = dialyzer_utils:format_sig(Sig, RecMap),
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

normalize_incFuncs(Funcs) ->
  [FuncInfo || {_FileName, FuncInfo} <- Funcs].

-spec remove_module_info([func_info()]) -> [func_info()].

remove_module_info(FuncInfoList) ->
  F = fun ({_,module_info,0}) -> false;
	  ({_,module_info,1}) -> false;
	  ({Line,F,A}) when is_integer(Line), is_atom(F), is_integer(A) -> true
      end,
  lists:filter(F, FuncInfoList).

write_typed_file(File, Info) ->
  io:format("      Processing file: ~p\n", [File]),
  Dir = filename:dirname(File),
  RootName = filename:basename(filename:rootname(File)),
  Ext = filename:extension(File),
  TyperAnnDir = filename:join(Dir, ?TYPER_ANN_DIR),
  TmpNewFilename = lists:concat([RootName,".ann",Ext]),
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
	  io:format("Unknown error when writing ~p\n", [Dir]),
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

write_typed_file(Chars, File, #info{funcs = []}, _LNo, _Acc) ->
  ok = file:write_file(File, list_to_binary(Chars), [append]);
write_typed_file([Ch|Chs] = Chars, File, Info, LineNo, Acc) ->
  [{Line,F,A}|RestFuncs] = Info#info.funcs,
  case Line of
    1 -> %% This will happen only for inc files
      ok = raw_write(F, A, Info, File, []),
      NewInfo = Info#info{funcs = RestFuncs},
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
		{Info#info{funcs = RestFuncs}, []};
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
  Type = get_type_info({F,A}, Info#info.typeMap),
  TypeStr =
    case Type of
      {contract, C} -> 
        dialyzer_contracts:contract_to_string(C);
      {RetType, ArgType} ->
        dialyzer_utils:format_sig(erl_types:t_fun(ArgType, RetType),
				  Info#info.recMap)
    end,
  case Info#info.contracts of
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
 
show_type_info_only(File, Info) ->
  io:format("\n%% File: ~p\n%% ", [File]),
  OutputString = lists:concat(["~.", length(File)+8, "c~n"]),
  io:fwrite(OutputString, [$-]),
  Fun = fun ({_LineNo, F, A}) ->
	    TypeInfo = get_type_string(F, A, Info, show),
	    io:format("~s\n", [TypeInfo])
	end,
  lists:foreach(Fun, Info#info.funcs).

get_type_info(Func, TypeMap) ->
  case typer_map:lookup(Func, TypeMap) of
    none ->
      %% Note: Typeinfo of any function should exist in
      %% the result offered by dialyzer, otherwise there 
      %% *must* be something wrong with the analysis
      io:format("No type info for function: ~p\n", [Func]),
      halt();
    {contract, _Fun} = C -> C;
    {_RetType, _ArgType} = RA -> RA 
  end.
