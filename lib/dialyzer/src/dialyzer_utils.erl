%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
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
%%% File    : dialyzer_utils.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description :
%%%
%%% Created :  5 Dec 2006 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_utils).

-export([
	 format_sig/1,
	 format_sig/2,
	 get_abstract_code_from_beam/1,
 	 get_compile_options_from_beam/1,
	 get_abstract_code_from_src/1,
	 get_abstract_code_from_src/2,
	 get_core_from_abstract_code/1,
	 get_core_from_abstract_code/2,
	 get_core_from_src/1,
	 get_core_from_src/2,
	 get_record_and_type_info/1,
	 get_spec_info/3,
         get_fun_meta_info/3,
         is_suppressed_fun/2,
         is_suppressed_tag/3,
	 merge_records/2,
	 pp_hook/0,
	 process_record_remote_types/1,
         sets_filter/2,
	 src_compiler_opts/0,
	 refold_pattern/1,
	 parallelism/0,
         family/1
	]).

-include("dialyzer.hrl").

%%-define(DEBUG, true).

-ifdef(DEBUG).
print_types(RecDict) ->
  Keys = dict:fetch_keys(RecDict),
  print_types1(Keys, RecDict).

print_types1([], _) ->
  ok;
print_types1([{type, _Name, _NArgs} = Key|T], RecDict) ->
  {ok, {{_Mod, _FileLine, _Form, _Args}, Type}} = dict:find(Key, RecDict),
  io:format("\n~w: ~w\n", [Key, Type]),
  print_types1(T, RecDict);
print_types1([{opaque, _Name, _NArgs} = Key|T], RecDict) ->
  {ok, {{_Mod, _FileLine, _Form, _Args}, Type}} = dict:find(Key, RecDict),
  io:format("\n~w: ~w\n", [Key, Type]),
  print_types1(T, RecDict);
print_types1([{record, _Name} = Key|T], RecDict) ->
  {ok, {_FileLine, [{_Arity, _Fields} = AF]}} = dict:find(Key, RecDict),
  io:format("~w: ~w\n\n", [Key, AF]),
  print_types1(T, RecDict).
-define(debug(D_), print_types(D_)).
-else.
-define(debug(D_), ok).
-endif.

%% ----------------------------------------------------------------------------

-type abstract_code() :: [erl_parse:abstract_form()].
-type comp_options()  :: [compile:option()].
-type mod_or_fname()  :: module() | file:filename().
-type fa()            :: {atom(), arity()}.
-type codeserver()    :: dialyzer_codeserver:codeserver().

%% ============================================================================
%%
%%  Compilation utils
%%
%% ============================================================================

-spec get_abstract_code_from_src(mod_or_fname()) ->
	{'ok', abstract_code()} | {'error', [string()]}.

get_abstract_code_from_src(File) ->
  get_abstract_code_from_src(File, src_compiler_opts()).

-spec get_abstract_code_from_src(mod_or_fname(), comp_options()) ->
	{'ok', abstract_code()} | {'error', [string()]}.

get_abstract_code_from_src(File, Opts) ->
  case compile:noenv_file(File, [to_pp, binary|Opts]) of
    error -> {error, []};
    {error, Errors, _} -> {error, format_errors(Errors)};
    {ok, _, AbstrCode} -> {ok, AbstrCode}
  end.

-type get_core_from_src_ret() :: {'ok', cerl:c_module()} | {'error', string()}.

-spec get_core_from_src(file:filename()) -> get_core_from_src_ret().

get_core_from_src(File) ->
  get_core_from_src(File, []).

-spec get_core_from_src(file:filename(), comp_options()) -> get_core_from_src_ret().

get_core_from_src(File, Opts) ->
  case get_abstract_code_from_src(File, Opts) of
    {error, _} = Error -> Error;
    {ok, AbstrCode} ->
      case get_core_from_abstract_code(AbstrCode, Opts) of
	error -> {error, "  Could not get Core Erlang code from abstract code"};
	{ok, _Core} = C -> C
      end
  end.

-spec get_abstract_code_from_beam(file:filename()) -> 'error' | {'ok', abstract_code()}.

get_abstract_code_from_beam(File) ->
  case beam_lib:chunks(File, [abstract_code]) of
    {ok, {_, List}} ->
      case lists:keyfind(abstract_code, 1, List) of
	{abstract_code, {raw_abstract_v1, Abstr}} -> {ok, Abstr};
	_ -> error
      end;
    _ ->
      %% No or unsuitable abstract code.
      error
  end.

-spec get_compile_options_from_beam(file:filename()) -> 'error' | {'ok', [compile:option()]}.

get_compile_options_from_beam(File) ->
  case beam_lib:chunks(File, [compile_info]) of
    {ok, {_, List}} ->
      case lists:keyfind(compile_info, 1, List) of
	{compile_info, CompInfo} -> compile_info_to_options(CompInfo);
	_ -> error
      end;
    _ ->
      %% No or unsuitable compile info.
      error
  end.

compile_info_to_options(CompInfo) ->
  case lists:keyfind(options, 1, CompInfo) of
    {options, CompOpts} -> {ok, CompOpts};
    _ -> error
  end.

-type get_core_from_abs_ret() :: {'ok', cerl:c_module()} | 'error'.

-spec get_core_from_abstract_code(abstract_code()) -> get_core_from_abs_ret().

get_core_from_abstract_code(AbstrCode) ->
  get_core_from_abstract_code(AbstrCode, []).

-spec get_core_from_abstract_code(abstract_code(), comp_options()) -> get_core_from_abs_ret().

get_core_from_abstract_code(AbstrCode, Opts) ->
  %% We do not want the parse_transforms around since we already
  %% performed them. In some cases we end up in trouble when
  %% performing them again.
  AbstrCode1 = cleanup_parse_transforms(AbstrCode),
  %% Remove parse_transforms (and other options) from compile options.
  Opts2 = cleanup_compile_options(Opts),
  try compile:noenv_forms(AbstrCode1, Opts2 ++ src_compiler_opts()) of
      {ok, _, Core} -> {ok, Core};
      _What -> error
  catch
    error:_ -> error
  end.

%% ============================================================================
%%
%%  Typed Records
%%
%% ============================================================================

-type type_table() :: erl_types:type_table().
-type mod_records()   :: dict:dict(module(), type_table()).

-spec get_record_and_type_info(abstract_code()) ->
	{'ok', type_table()} | {'error', string()}.

get_record_and_type_info(AbstractCode) ->
  Module = get_module(AbstractCode),
  get_record_and_type_info(AbstractCode, Module, dict:new()).

-spec get_record_and_type_info(abstract_code(), module(), type_table()) ->
	{'ok', type_table()} | {'error', string()}.

get_record_and_type_info(AbstractCode, Module, RecDict) ->
  get_record_and_type_info(AbstractCode, Module, RecDict, "nofile").

get_record_and_type_info([{attribute, A, record, {Name, Fields0}}|Left],
			 Module, RecDict, File) ->
  {ok, Fields} = get_record_fields(Fields0, RecDict),
  Arity = length(Fields),
  FN = {File, erl_anno:line(A)},
  NewRecDict = dict:store({record, Name}, {FN, [{Arity,Fields}]}, RecDict),
  get_record_and_type_info(Left, Module, NewRecDict, File);
get_record_and_type_info([{attribute, A, type, {{record, Name}, Fields0, []}}
			  |Left], Module, RecDict, File) ->
  %% This overrides the original record declaration.
  {ok, Fields} = get_record_fields(Fields0, RecDict),
  Arity = length(Fields),
  FN = {File, erl_anno:line(A)},
  NewRecDict = dict:store({record, Name}, {FN, [{Arity, Fields}]}, RecDict),
  get_record_and_type_info(Left, Module, NewRecDict, File);
get_record_and_type_info([{attribute, A, Attr, {Name, TypeForm}}|Left],
			 Module, RecDict, File)
               when Attr =:= 'type'; Attr =:= 'opaque' ->
  FN = {File, erl_anno:line(A)},
  try add_new_type(Attr, Name, TypeForm, [], Module, FN, RecDict) of
    NewRecDict ->
      get_record_and_type_info(Left, Module, NewRecDict, File)
  catch
    throw:{error, _} = Error -> Error
  end;
get_record_and_type_info([{attribute, A, Attr, {Name, TypeForm, Args}}|Left],
			 Module, RecDict, File)
               when Attr =:= 'type'; Attr =:= 'opaque' ->
  FN = {File, erl_anno:line(A)},
  try add_new_type(Attr, Name, TypeForm, Args, Module, FN, RecDict) of
    NewRecDict ->
      get_record_and_type_info(Left, Module, NewRecDict, File)
  catch
    throw:{error, _} = Error -> Error
  end;
get_record_and_type_info([{attribute, _, file, {IncludeFile, _}}|Left],
                         Module, RecDict, _File) ->
  get_record_and_type_info(Left, Module, RecDict, IncludeFile);
get_record_and_type_info([_Other|Left], Module, RecDict, File) ->
  get_record_and_type_info(Left, Module, RecDict, File);
get_record_and_type_info([], _Module, RecDict, _File) ->
  {ok, RecDict}.

add_new_type(TypeOrOpaque, Name, TypeForm, ArgForms, Module, FN,
             RecDict) ->
  Arity = length(ArgForms),
  case erl_types:type_is_defined(TypeOrOpaque, Name, Arity, RecDict) of
    true ->
      Msg = flat_format("Type ~s/~w already defined\n", [Name, Arity]),
      throw({error, Msg});
    false ->
      try erl_types:t_var_names(ArgForms) of
        ArgNames ->
	  dict:store({TypeOrOpaque, Name, Arity},
                     {{Module, FN, TypeForm, ArgNames},
                      erl_types:t_any()}, RecDict)
      catch
        _:_ ->
	  throw({error, flat_format("Type declaration for ~w does not "
				    "have variables as parameters", [Name])})
      end
  end.

get_record_fields(Fields, RecDict) ->
  Fs = get_record_fields(Fields, RecDict, []),
  {ok, [{Name, Form, erl_types:t_any()} || {Name, Form} <- Fs]}.

get_record_fields([{typed_record_field, OrdRecField, TypeForm}|Left],
		  RecDict, Acc) ->
  Name =
    case OrdRecField of
      {record_field, _Line, Name0} -> erl_parse:normalise(Name0);
      {record_field, _Line, Name0, _Init} -> erl_parse:normalise(Name0)
    end,
  get_record_fields(Left, RecDict, [{Name, TypeForm}|Acc]);
get_record_fields([{record_field, _Line, Name}|Left], RecDict, Acc) ->
  A = erl_anno:set_generated(true, erl_anno:new(1)),
  NewAcc = [{erl_parse:normalise(Name), {var, A, '_'}}|Acc],
  get_record_fields(Left, RecDict, NewAcc);
get_record_fields([{record_field, _Line, Name, _Init}|Left], RecDict, Acc) ->
  A = erl_anno:set_generated(true, erl_anno:new(1)),
  NewAcc = [{erl_parse:normalise(Name), {var, A, '_'}}|Acc],
  get_record_fields(Left, RecDict, NewAcc);
get_record_fields([], _RecDict, Acc) ->
  lists:reverse(Acc).

-spec process_record_remote_types(codeserver()) -> codeserver().

%% The field types are cached. Used during analysis when handling records.
process_record_remote_types(CServer) ->
  TempRecords = dialyzer_codeserver:get_temp_records(CServer),
  ExpTypes = dialyzer_codeserver:get_exported_types(CServer),
  Cache = erl_types:cache__new(),
  {TempRecords1, Cache1} =
    process_opaque_types0(TempRecords, ExpTypes, Cache),
  %% A cache (not the field type cache) is used for speeding things up a bit.
  VarTable = erl_types:var_table__new(),
  ModuleFun =
    fun({Module, Record}, C0) ->
        RecordFun =
          fun({Key, Value}, C2) ->
              case Key of
                {record, Name} ->
                  FieldFun =
                    fun({Arity, Fields}, C4) ->
                        Site = {record, {Module, Name, Arity}},
                        {Fields1, C7} =
                          lists:mapfoldl(fun({FieldName, Field, _}, C5) ->
                                             {FieldT, C6} =
                                               erl_types:t_from_form
                                                 (Field, ExpTypes, Site,
                                                  TempRecords1, VarTable,
                                                  C5),
                                          {{FieldName, Field, FieldT}, C6}
                                      end, C4, Fields),
                        {{Arity, Fields1}, C7}
                    end,
                  {FileLine, Fields} = Value,
                  {FieldsList, C3} =
                    lists:mapfoldl(FieldFun, C2, orddict:to_list(Fields)),
                  {{Key, {FileLine, orddict:from_list(FieldsList)}}, C3};
                _Other -> {{Key, Value}, C2}
              end
          end,
        {RecordList, C1} =
          lists:mapfoldl(RecordFun, C0, dict:to_list(Record)),
        {{Module, dict:from_list(RecordList)}, C1}
    end,
  {NewRecordsList, C1} =
    lists:mapfoldl(ModuleFun, Cache1, dict:to_list(TempRecords1)),
  NewRecords = dict:from_list(NewRecordsList),
  _C8 = check_record_fields(NewRecords, ExpTypes, C1),
  dialyzer_codeserver:finalize_records(NewRecords, CServer).

%% erl_types:t_from_form() substitutes the declaration of opaque types
%% for the expanded type in some cases. To make sure the initial type,
%% any(), is not used, the expansion is done twice.
%% XXX: Recursive opaque types are not handled well.
process_opaque_types0(TempRecords0, TempExpTypes, Cache) ->
  {TempRecords1, NewCache} =
    process_opaque_types(TempRecords0, TempExpTypes, Cache),
  process_opaque_types(TempRecords1, TempExpTypes, NewCache).

process_opaque_types(TempRecords, TempExpTypes, Cache) ->
  VarTable = erl_types:var_table__new(),
  ModuleFun =
    fun({Module, Record}, C0) ->
        RecordFun =
          fun({Key, Value}, C2) ->
              case Key of
                {opaque, Name, NArgs} ->
                  {{_Module, _FileLine, Form, _ArgNames}=F, _Type} = Value,
                  Site = {type, {Module, Name, NArgs}},
                  {Type, C3} =
                    erl_types:t_from_form(Form, TempExpTypes, Site,
                                          TempRecords, VarTable, C2),
                  {{Key, {F, Type}}, C3};
                _Other -> {{Key, Value}, C2}
              end
          end,
        {RecordList, C1} =
          lists:mapfoldl(RecordFun, C0, dict:to_list(Record)),
        {{Module, dict:from_list(RecordList)}, C1}
        %% dict:map(RecordFun, Record)
    end,
  {TempRecordList, NewCache} =
    lists:mapfoldl(ModuleFun, Cache, dict:to_list(TempRecords)),
  {dict:from_list(TempRecordList), NewCache}.
  %% dict:map(ModuleFun, TempRecords).

check_record_fields(Records, TempExpTypes, Cache) ->
  VarTable = erl_types:var_table__new(),
  CheckFun =
    fun({Module, Element}, C0) ->
        CheckForm = fun(Form, Site, C1) ->
                        erl_types:t_check_record_fields(Form, TempExpTypes,
                                                        Site, Records,
                                                        VarTable, C1)
                  end,
        ElemFun =
          fun({Key, Value}, C2) ->
              case Key of
                {record, Name} ->
                  FieldFun =
                    fun({Arity, Fields}, C3) ->
                        Site = {record, {Module, Name, Arity}},
                        lists:foldl(fun({_, Field, _}, C4) ->
                                        CheckForm(Field, Site, C4)
                                    end, C3, Fields)
                    end,
                  {FileLine, Fields} = Value,
                  Fun = fun() -> lists:foldl(FieldFun, C2, Fields) end,
                  msg_with_position(Fun, FileLine);
                {_OpaqueOrType, Name, NArgs} ->
                  Site = {type, {Module, Name, NArgs}},
                  {{_Module, FileLine, Form, _ArgNames}, _Type} = Value,
                  Fun = fun() -> CheckForm(Form, Site, C2) end,
                  msg_with_position(Fun, FileLine)
              end
          end,
        lists:foldl(ElemFun, C0, dict:to_list(Element))
    end,
  lists:foldl(CheckFun, Cache, dict:to_list(Records)).

msg_with_position(Fun, FileLine) ->
  try Fun()
  catch
    throw:{error, Msg} ->
      {File, Line} = FileLine,
      BaseName = filename:basename(File),
      NewMsg = io_lib:format("~s:~p: ~s", [BaseName, Line, Msg]),
      throw({error, NewMsg})
  end.

-spec merge_records(mod_records(), mod_records()) -> mod_records().

merge_records(NewRecords, OldRecords) ->
  dict:merge(fun(_Key, NewVal, _OldVal) -> NewVal end, NewRecords, OldRecords).

%% ============================================================================
%%
%%  Spec info
%%
%% ============================================================================

-type spec_dict()     :: dict:dict().
-type callback_dict() :: dict:dict().

-spec get_spec_info(module(), abstract_code(), type_table()) ->
        {'ok', spec_dict(), callback_dict()} | {'error', string()}.

get_spec_info(ModName, AbstractCode, RecordsDict) ->
  OptionalCallbacks0 = get_optional_callbacks(AbstractCode, ModName),
  OptionalCallbacks = gb_sets:from_list(OptionalCallbacks0),
  get_spec_info(AbstractCode, dict:new(), dict:new(),
		RecordsDict, ModName, OptionalCallbacks, "nofile").

get_optional_callbacks(Abs, ModName) ->
  [{ModName, F, A} || {F, A} <- get_optional_callbacks(Abs)].

get_optional_callbacks(Abs) ->
    L = [O ||
            {attribute, _, optional_callbacks, O} <- Abs,
            is_fa_list(O)],
    lists:append(L).

%% TypeSpec is a list of conditional contracts for a function.
%% Each contract is of the form {[Argument], Range, [Constraint]} where
%%  - Argument and Range are in erl_types:erl_type() format and
%%  - Constraint is of the form {subtype, T1, T2} where T1 and T2
%%    are erl_types:erl_type()

get_spec_info([{attribute, Anno, Contract, {Id, TypeSpec}}|Left],
	      SpecDict, CallbackDict, RecordsDict, ModName, OptCb, File)
  when ((Contract =:= 'spec') or (Contract =:= 'callback')),
       is_list(TypeSpec) ->
  Ln = erl_anno:line(Anno),
  MFA = case Id of
	  {_, _, _} = T -> T;
	  {F, A} -> {ModName, F, A}
	end,
  Xtra = [optional_callback || gb_sets:is_member(MFA, OptCb)],
  ActiveDict =
    case Contract of
      spec     -> SpecDict;
      callback -> CallbackDict
    end,
  try dict:find(MFA, ActiveDict) of
    error ->
      SpecData = {TypeSpec, Xtra},
      NewActiveDict =
	dialyzer_contracts:store_tmp_contract(MFA, {File, Ln}, SpecData,
					      ActiveDict, RecordsDict),
      {NewSpecDict, NewCallbackDict} =
	case Contract of
	  spec     -> {NewActiveDict, CallbackDict};
	  callback -> {SpecDict, NewActiveDict}
	end,
      get_spec_info(Left, NewSpecDict, NewCallbackDict,
		    RecordsDict, ModName, OptCb, File);
    {ok, {{OtherFile, L}, _D}} ->
      {Mod, Fun, Arity} = MFA,
      Msg = flat_format("  Contract/callback for function ~w:~w/~w "
			"already defined in ~s:~w\n",
			[Mod, Fun, Arity, OtherFile, L]),
      throw({error, Msg})
  catch
    throw:{error, Error} ->
      {error, flat_format("  Error while parsing contract in line ~w: ~s\n",
			  [Ln, Error])}
  end;
get_spec_info([{attribute, _, file, {IncludeFile, _}}|Left],
	      SpecDict, CallbackDict, RecordsDict, ModName, OptCb, _File) ->
  get_spec_info(Left, SpecDict, CallbackDict,
		RecordsDict, ModName, OptCb, IncludeFile);
get_spec_info([_Other|Left], SpecDict, CallbackDict,
	      RecordsDict, ModName, OptCb, File) ->
  get_spec_info(Left, SpecDict, CallbackDict,
                RecordsDict, ModName, OptCb, File);
get_spec_info([], SpecDict, CallbackDict,
              _RecordsDict, _ModName, _OptCb, _File) ->
  {ok, SpecDict, CallbackDict}.

-spec get_fun_meta_info(module(), abstract_code(), [dial_warn_tag()]) ->
                           dialyzer_codeserver:fun_meta_info().

get_fun_meta_info(M, Abs, LegalWarnings) ->
  NoWarn = get_nowarn_unused_function(M, Abs),
  FuncSupp = get_func_suppressions(M, Abs),
  Warnings0 = get_options(Abs, LegalWarnings),
  Warnings = ordsets:to_list(Warnings0),
  ModuleWarnings = [{M, W} || W <- Warnings],
  RawProps = lists:append([NoWarn, FuncSupp, ModuleWarnings]),
  process_options(dialyzer_utils:family(RawProps), Warnings0).

process_options([{M, _}=Mod|Left], Warnings) when is_atom(M) ->
  [Mod|process_options(Left, Warnings)];
process_options([{{_M, _F, _A}=MFA, Opts}|Left], Warnings) ->
  WL = case lists:member(nowarn_function, Opts) of
         true -> [{nowarn_function, func}]; % takes precedence
         false ->
           Ws = dialyzer_options:build_warnings(Opts, Warnings),
           ModOnly = [{W, mod} || W <- ordsets:subtract(Warnings, Ws)],
           FunOnly = [{W, func} || W <- ordsets:subtract(Ws, Warnings)],
           ordsets:union(ModOnly, FunOnly)
       end,
  case WL of
    [] -> process_options(Left, Warnings);
    _ -> [{MFA, WL}|process_options(Left, Warnings)]
  end;
process_options([], _Warnings) -> [].

-spec get_nowarn_unused_function(module(), abstract_code()) ->
                                    [{mfa(), 'no_unused'}].

get_nowarn_unused_function(M, Abs) ->
  Opts = get_options_with_tag(compile, Abs),
  Warn = erl_lint:bool_option(warn_unused_function, nowarn_unused_function,
                              true, Opts),
  Functions = [{F, A} || {function, _, F, A, _} <- Abs],
  AttrFile = collect_attribute(Abs, compile),
  TagsFaList = check_fa_list(AttrFile, nowarn_unused_function, Functions),
  FAs = case Warn of
          false -> Functions;
          true ->
            [FA || {{nowarn_unused_function,_L,_File}, FA} <- TagsFaList]
        end,
  [{{M, F, A}, no_unused} || {F, A} <- FAs].

-spec get_func_suppressions(module(), abstract_code()) ->
                            [{mfa(), 'nowarn_function' | dial_warn_tag()}].

get_func_suppressions(M, Abs) ->
  Functions = [{F, A} || {function, _, F, A, _} <- Abs],
  AttrFile = collect_attribute(Abs, dialyzer),
  TagsFAs = check_fa_list(AttrFile, '*', Functions),
  %% Check the options:
  Fun = fun({{nowarn_function, _L, _File}, _FA}) -> ok;
           ({OptLFile, _FA}) ->
            _ = get_options1([OptLFile], ordsets:new())
        end,
  lists:foreach(Fun, TagsFAs),
  [{{M, F, A}, W} || {{W, _L, _File}, {F, A}} <- TagsFAs].

-spec get_options(abstract_code(), [dial_warn_tag()]) ->
                     ordsets:ordset(dial_warn_tag()).

get_options(Abs, LegalWarnings) ->
  AttrFile = collect_attribute(Abs, dialyzer),
  get_options1(AttrFile, LegalWarnings).

get_options1([{Args, L, File}|Left], Warnings) ->
  Opts = [O ||
           O <- lists:flatten([Args]),
           is_atom(O)],
  try dialyzer_options:build_warnings(Opts, Warnings) of
    NewWarnings ->
      get_options1(Left, NewWarnings)
  catch
    throw:{dialyzer_options_error, Msg} ->
      Msg1 = flat_format("  ~s:~w: ~s", [File, L, Msg]),
      throw({error, Msg1})
  end;
get_options1([], Warnings) ->
  Warnings.

-type collected_attribute() ::
        {Args :: term(), erl_anno:line(), file:filename()}.

collect_attribute(Abs, Tag) ->
  collect_attribute(Abs, Tag, "nofile").

collect_attribute([{attribute, L, Tag, Args}|Left], Tag, File) ->
  CollAttr = {Args, L, File},
  [CollAttr | collect_attribute(Left, Tag, File)];
collect_attribute([{attribute, _, file, {IncludeFile, _}}|Left], Tag, _) ->
  collect_attribute(Left, Tag, IncludeFile);
collect_attribute([_Other|Left], Tag, File) ->
  collect_attribute(Left, Tag, File);
collect_attribute([], _Tag, _File) -> [].

-spec is_suppressed_fun(mfa(), codeserver()) -> boolean().

is_suppressed_fun(MFA, CodeServer) ->
  lookup_fun_property(MFA, nowarn_function, CodeServer).

-spec is_suppressed_tag(mfa() | module(), dial_warn_tag(), codeserver()) ->
                           boolean().

is_suppressed_tag(MorMFA, Tag, Codeserver) ->
  not lookup_fun_property(MorMFA, Tag, Codeserver).

lookup_fun_property({M, _F, _A}=MFA, Property, CodeServer) ->
  MFAPropList = dialyzer_codeserver:lookup_meta_info(MFA, CodeServer),
  case proplists:get_value(Property, MFAPropList, no) of
    mod -> false; % suppressed in function
    func -> true; % requested in function
    no -> lookup_fun_property(M, Property, CodeServer)
  end;
lookup_fun_property(M, Property, CodeServer) when is_atom(M) ->
  MPropList = dialyzer_codeserver:lookup_meta_info(M, CodeServer),
  proplists:is_defined(Property, MPropList).

%% ============================================================================
%%
%% Exported types
%%
%% ============================================================================

-spec sets_filter([module()], sets:set()) -> sets:set().

sets_filter([], ExpTypes) ->
  ExpTypes;
sets_filter([Mod|Mods], ExpTypes) ->
  NewExpTypes = sets:filter(fun({M, _F, _A}) -> M =/= Mod end, ExpTypes),
  sets_filter(Mods, NewExpTypes).

%% ============================================================================
%%
%%  Util utils
%%
%% ============================================================================

-spec src_compiler_opts() -> [compile:option(),...].

src_compiler_opts() ->
  [no_copt, to_core, binary, return_errors,
   no_inline, strict_record_tests, strict_record_updates,
   dialyzer].

-spec get_module(abstract_code()) -> module().

get_module([{attribute, _, module, {M, _As}} | _]) -> M;
get_module([{attribute, _, module, M} | _]) -> M;
get_module([_ | Rest]) -> get_module(Rest).

-spec cleanup_parse_transforms(abstract_code()) -> abstract_code().

cleanup_parse_transforms([{attribute, _, compile, {parse_transform, _}}|Left]) ->
  cleanup_parse_transforms(Left);
cleanup_parse_transforms([Other|Left]) ->
  [Other|cleanup_parse_transforms(Left)];
cleanup_parse_transforms([]) ->
  [].

-spec cleanup_compile_options([compile:option()]) -> [compile:option()].

cleanup_compile_options(Opts) ->
  lists:filter(fun keep_compile_option/1, Opts).

%% Using abstract, not asm or core.
keep_compile_option(from_asm) -> false;
keep_compile_option(from_core) -> false;
%% The parse transform will already have been applied, may cause
%% problems if it is re-applied.
keep_compile_option({parse_transform, _}) -> false;
keep_compile_option(warnings_as_errors) -> false;
keep_compile_option(_) -> true.

-spec format_errors([{module(), string()}]) -> [string()].

format_errors([{Mod, Errors}|Left]) ->
  FormatedError =
    [io_lib:format("~s:~w: ~s\n", [Mod, Line, M:format_error(Desc)])
     || {Line, M, Desc} <- Errors],
  [lists:flatten(FormatedError) | format_errors(Left)];
format_errors([]) ->
  [].

-spec format_sig(erl_types:erl_type()) -> string().

format_sig(Type) ->
  format_sig(Type, dict:new()).

-spec format_sig(erl_types:erl_type(), type_table()) -> string().

format_sig(Type, RecDict) ->
  "fun(" ++ Sig = lists:flatten(erl_types:t_to_string(Type, RecDict)),
  ")" ++ RevSig = lists:reverse(Sig),
  lists:reverse(RevSig).

flat_format(Fmt, Lst) ->
  lists:flatten(io_lib:format(Fmt, Lst)).

-spec get_options_with_tag(atom(), abstract_code()) -> [term()].

get_options_with_tag(Tag, Abs) ->
  lists:flatten([O || {attribute, _, Tag0, O} <- Abs, Tag =:= Tag0]).

%% Check F/A, and collect (unchecked) warning tags with line and file.
-spec check_fa_list([collected_attribute()], atom(), [fa()]) ->
                       [{{atom(), erl_anno:line(), file:filename()},fa()}].

check_fa_list(AttrFile, Tag, Functions) ->
  FuncTab = gb_sets:from_list(Functions),
  check_fa_list1(AttrFile, Tag, FuncTab).

check_fa_list1([{Args, L, File}|Left], Tag, Funcs) ->
  TermsL = [{{Tag0, L, File}, Term} ||
             {Tags, Terms0} <- lists:flatten([Args]),
             Tag0 <- lists:flatten([Tags]),
             Tag =:= '*' orelse Tag =:= Tag0,
             Term <- lists:flatten([Terms0])],
  case lists:dropwhile(fun({_, T}) -> is_fa(T) end, TermsL) of
    [] -> ok;
    [{_, Bad}|_] ->
      Msg1 = flat_format("  Bad function ~w in line ~s:~w",
                         [Bad, File, L]),
      throw({error, Msg1})
  end,
  case lists:dropwhile(fun({_, FA}) -> is_known(FA, Funcs) end, TermsL) of
    [] -> ok;
    [{_, {F, A}}|_] ->
      Msg2 = flat_format("  Unknown function ~w/~w in line ~s:~w",
                         [F, A, File, L]),
      throw({error, Msg2})
  end,
  TermsL ++ check_fa_list1(Left, Tag, Funcs);
check_fa_list1([], _Tag, _Funcs) -> [].

is_known(FA, Funcs) ->
  gb_sets:is_element(FA, Funcs).

-spec is_fa_list(term()) -> boolean().

is_fa_list([E|L]) -> is_fa(E) andalso is_fa_list(L);
is_fa_list([]) -> true;
is_fa_list(_) -> false.

-spec is_fa(term()) -> boolean().

is_fa({FuncName, Arity})
  when is_atom(FuncName), is_integer(Arity), Arity >= 0 -> true;
is_fa(_) -> false.

%%-------------------------------------------------------------------
%% Author      : Per Gustafsson <pergu@it.uu.se>
%% Description : Provides better printing of binaries.
%% Created     : 5 March 2007
%%-------------------------------------------------------------------

-spec pp_hook() -> fun((cerl:cerl(), _, _) -> term()).
pp_hook() ->
  fun pp_hook/3.

pp_hook(Node, Ctxt, Cont) ->
  case cerl:type(Node) of
    binary ->
      pp_binary(Node, Ctxt, Cont);
    bitstr ->
      pp_segment(Node, Ctxt, Cont);
    map ->
      pp_map(Node, Ctxt, Cont);
    literal ->
      case is_map(cerl:concrete(Node)) of
	true -> pp_map(Node, Ctxt, Cont);
	false -> Cont(Node, Ctxt)
      end;
    _ ->
      Cont(Node, Ctxt)
  end.

pp_binary(Node, Ctxt, Cont) ->
  prettypr:beside(prettypr:text("<<"),
		  prettypr:beside(pp_segments(cerl:binary_segments(Node),
					      Ctxt, Cont),
				  prettypr:text(">>"))).

pp_segments([Seg], Ctxt, Cont) ->
  pp_segment(Seg, Ctxt, Cont);
pp_segments([], _Ctxt, _Cont) ->
  prettypr:text("");
pp_segments([Seg|Rest], Ctxt, Cont) ->
  prettypr:beside(pp_segment(Seg, Ctxt, Cont),
		  prettypr:beside(prettypr:text(","),
				  pp_segments(Rest, Ctxt, Cont))).

pp_segment(Node, Ctxt, Cont) ->
  Val = cerl:bitstr_val(Node),
  Size = cerl:bitstr_size(Node),
  Unit = cerl:bitstr_unit(Node),
  Type = cerl:bitstr_type(Node),
  Flags = cerl:bitstr_flags(Node),
  prettypr:beside(Cont(Val, Ctxt),
		  prettypr:beside(pp_size(Size, Ctxt, Cont),
				  prettypr:beside(pp_opts(Type, Flags),
						  pp_unit(Unit, Ctxt, Cont)))).

pp_size(Size, Ctxt, Cont) ->
  case cerl:is_c_atom(Size) of
    true ->
      prettypr:text("");
    false ->
      prettypr:beside(prettypr:text(":"), Cont(Size, Ctxt))
  end.

pp_opts(Type, Flags) ->
  FinalFlags =
    case cerl:atom_val(Type) of
      binary -> [];
      float -> keep_endian(cerl:concrete(Flags));
      integer -> keep_all(cerl:concrete(Flags));
      utf8 -> [];
      utf16 -> [];
      utf32 -> []
    end,
  prettypr:beside(prettypr:text("/"),
		  prettypr:beside(pp_atom(Type),
				  pp_flags(FinalFlags))).

pp_flags([]) ->
  prettypr:text("");
pp_flags([Flag|Flags]) ->
  prettypr:beside(prettypr:text("-"),
		  prettypr:beside(pp_atom(Flag),
				  pp_flags(Flags))).

keep_endian(Flags) ->
  [cerl:c_atom(X) || X <- Flags, (X =:= little) or (X =:= native)].

keep_all(Flags) ->
  [cerl:c_atom(X) || X <- Flags,
		     (X =:= little) or (X =:= native) or (X =:= signed)].

pp_unit(Unit, Ctxt, Cont) ->
  case cerl:concrete(Unit) of
    N when is_integer(N) ->
      prettypr:beside(prettypr:text("-"),
		      prettypr:beside(prettypr:text("unit:"),
				      Cont(Unit, Ctxt)));
    _ -> % Other value: e.g. 'undefined' when UTF
      prettypr:text("")
  end.

pp_atom(Atom) ->
  String = atom_to_list(cerl:atom_val(Atom)),
  prettypr:text(String).

pp_map(Node, Ctxt, Cont) ->
  Arg = cerl:map_arg(Node),
  Before = case cerl:is_c_map_empty(Arg) of
	     true -> prettypr:floating(prettypr:text("#{"));
	     false ->
	       prettypr:beside(Cont(Arg,Ctxt),
			       prettypr:floating(prettypr:text("#{")))
	   end,
  prettypr:beside(
    Before, prettypr:beside(
	      prettypr:par(seq(cerl:map_es(Node),
			       prettypr:floating(prettypr:text(",")),
			       Ctxt, Cont)),
	      prettypr:floating(prettypr:text("}")))).

seq([H | T], Separator, Ctxt, Fun) ->
  case T of
    [] -> [Fun(H, Ctxt)];
    _  -> [prettypr:beside(Fun(H, Ctxt), Separator)
	   | seq(T, Separator, Ctxt, Fun)]
  end;
seq([], _, _, _) ->
  [prettypr:empty()].

%%------------------------------------------------------------------------------

-spec refold_pattern(cerl:cerl()) -> cerl:cerl().

refold_pattern(Pat) ->
  %% Avoid the churn of unfolding and refolding
  case cerl:is_literal(Pat) andalso find_map(cerl:concrete(Pat)) of
    true ->
      Tree = refold_concrete_pat(cerl:concrete(Pat)),
      PatAnn = cerl:get_ann(Pat),
      case proplists:is_defined(label, PatAnn) of
	%% Literals are not normally annotated with a label, but can be if, for
	%% example, they were created by cerl:fold_literal/1.
	true -> cerl:set_ann(Tree, PatAnn);
	false ->
	  [{label, Label}] = cerl:get_ann(Tree),
	  cerl:set_ann(Tree, [{label, Label}|PatAnn])
      end;
    false -> Pat
  end.

find_map(#{}) -> true;
find_map(Tuple) when is_tuple(Tuple) -> find_map(tuple_to_list(Tuple));
find_map([H|T]) -> find_map(H) orelse find_map(T);
find_map(_) -> false.

refold_concrete_pat(Val) ->
  case Val of
    _ when is_tuple(Val) ->
      Els = lists:map(fun refold_concrete_pat/1, tuple_to_list(Val)),
      case lists:all(fun cerl:is_literal/1, Els) of
	true -> cerl:abstract(Val);
	false -> label(cerl:c_tuple_skel(Els))
      end;
    [H|T] ->
      case  cerl:is_literal(HP=refold_concrete_pat(H))
	and cerl:is_literal(TP=refold_concrete_pat(T))
      of
	true -> cerl:abstract(Val);
	false -> label(cerl:c_cons_skel(HP, TP))
      end;
    M when is_map(M) ->
      %% Map patterns are not generated by the parser(!), but they have a
      %% property we want, namely that they are never folded into literals.
      %% N.B.: The key in a map pattern is an expression, *not* a pattern.
      label(cerl:c_map_pattern([cerl:c_map_pair_exact(cerl:abstract(K),
						      refold_concrete_pat(V))
				|| {K, V} <- maps:to_list(M)]));
    _ ->
      cerl:abstract(Val)
  end.

label(Tree) ->
      %% Sigh
      Label = -erlang:unique_integer([positive]),
      cerl:set_ann(Tree, [{label, Label}]).

%%------------------------------------------------------------------------------

-spec parallelism() -> integer().

parallelism() ->
  CPUs = erlang:system_info(logical_processors_available),
  Schedulers = erlang:system_info(schedulers),
  min(CPUs, Schedulers).

-spec family([{K,V}]) -> [{K,[V]}].

family(L) ->
    sofs:to_external(sofs:rel2fam(sofs:relation(L))).
