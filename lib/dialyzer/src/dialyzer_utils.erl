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
	 get_core_from_src/1,
	 get_core_from_src/2,
         get_core_from_beam/1,
         get_core_from_beam/2,
	 get_record_and_type_info/1,
	 get_spec_info/3,
         get_fun_meta_info/3,
         is_suppressed_fun/2,
         is_suppressed_tag/3,
	 pp_hook/0,
	 process_record_remote_types/1,
         merge_types/2,
         sets_filter/2,
	 src_compiler_opts/0,
	 refold_pattern/1,
         ets_tab2list/1,
         ets_move/2,
	 parallelism/0,
         family/1
	]).

-include("dialyzer.hrl").
-include("../../compiler/src/core_parse.hrl").

%%-define(DEBUG, true).

-ifdef(DEBUG).
print_types(RecDict) ->
  Keys = dict:fetch_keys(RecDict),
  print_types1(Keys, RecDict).

print_types1([], _) ->
  ok;
print_types1([{type, _Name, _NArgs} = Key|T], RecDict) ->
  {ok, {{_Mod, _FileLine, _Form, _Args}, Type}} = dict:find(Key, RecDict),
  io:format("\n~tw: ~tw\n", [Key, Type]),
  print_types1(T, RecDict);
print_types1([{opaque, _Name, _NArgs} = Key|T], RecDict) ->
  {ok, {{_Mod, _FileLine, _Form, _Args}, Type}} = dict:find(Key, RecDict),
  io:format("\n~tw: ~tw\n", [Key, Type]),
  print_types1(T, RecDict);
print_types1([{record, _Name} = Key|T], RecDict) ->
  {ok, {_FileLine, [{_Arity, _Fields} = AF]}} = dict:find(Key, RecDict),
  io:format("~tw: ~tw\n\n", [Key, AF]),
  print_types1(T, RecDict).
-define(debug(D_), print_types(D_)).
-else.
-define(debug(D_), ok).
-endif.

%% ----------------------------------------------------------------------------

-type comp_options()  :: [compile:option()].
-type fa()            :: {atom(), arity()}.
-type codeserver()    :: dialyzer_codeserver:codeserver().

%% ============================================================================
%%
%%  Compilation utils
%%
%% ============================================================================

-type get_core_from_src_ret() :: {'ok', cerl:c_module()} | {'error', string()}.

-spec get_core_from_src(file:filename()) -> get_core_from_src_ret().

get_core_from_src(File) ->
  get_core_from_src(File, []).

-spec get_core_from_src(file:filename(), comp_options()) -> get_core_from_src_ret().

get_core_from_src(File, Opts) ->
  case compile:noenv_file(File, Opts ++ src_compiler_opts()) of
    error -> {error, []};
    {error, Errors, _} -> {error, format_errors(Errors)};
    {ok, _, Core} -> {ok, Core}
  end.

-type get_core_from_beam_ret() :: {'ok', cerl:c_module()} | {'error', string()}.

-spec get_core_from_beam(file:filename()) -> get_core_from_beam_ret().

get_core_from_beam(File) ->
  get_core_from_beam(File, []).

-spec get_core_from_beam(file:filename(), comp_options()) -> get_core_from_beam_ret().

get_core_from_beam(File, Opts) ->
  case beam_lib:chunks(File, [debug_info]) of
    {ok, {Module, [{debug_info, {debug_info_v1, Backend, Metadata}}]}} ->
      case Backend:debug_info(core_v1, Module, Metadata, Opts ++ src_compiler_opts()) of
	{ok, Core} ->
	  {ok, Core};
	{error, _} ->
	  {error, "  Could not get Core Erlang code for: " ++ File ++ "\n"}
      end;
    _ ->
      {error, "  Could not get Core Erlang code for: " ++ File ++ "\n" ++
        "  Recompile with +debug_info or analyze starting from source code"}
  end.

%% ============================================================================
%%
%%  Typed Records
%%
%% ============================================================================

-type type_table() :: erl_types:type_table().

-spec get_record_and_type_info(cerl:c_module()) ->
        {'ok', type_table()} | {'error', string()}.

get_record_and_type_info(Core) ->
  Module = cerl:concrete(cerl:module_name(Core)),
  Tuples = core_to_attr_tuples(Core),
  get_record_and_type_info(Tuples, Module, maps:new(), "nofile").

get_record_and_type_info([{record, Line, [{Name, Fields0}]}|Left],
			 Module, RecDict, File) ->
  {ok, Fields} = get_record_fields(Fields0, RecDict),
  Arity = length(Fields),
  FN = {File, Line},
  NewRecDict = maps:put({record, Name}, {FN, [{Arity,Fields}]}, RecDict),
  get_record_and_type_info(Left, Module, NewRecDict, File);
get_record_and_type_info([{type, Line, [{{record, Name}, Fields0, []}]}
			  |Left], Module, RecDict, File) ->
  %% This overrides the original record declaration.
  {ok, Fields} = get_record_fields(Fields0, RecDict),
  Arity = length(Fields),
  FN = {File, Line},
  NewRecDict = maps:put({record, Name}, {FN, [{Arity, Fields}]}, RecDict),
  get_record_and_type_info(Left, Module, NewRecDict, File);
get_record_and_type_info([{Attr, Line, [{Name, TypeForm}]}|Left],
			 Module, RecDict, File)
               when Attr =:= 'type'; Attr =:= 'opaque' ->
  FN = {File, Line},
  try add_new_type(Attr, Name, TypeForm, [], Module, FN, RecDict) of
    NewRecDict ->
      get_record_and_type_info(Left, Module, NewRecDict, File)
  catch
    throw:{error, _} = Error -> Error
  end;
get_record_and_type_info([{Attr, Line, [{Name, TypeForm, Args}]}|Left],
			 Module, RecDict, File)
               when Attr =:= 'type'; Attr =:= 'opaque' ->
  FN = {File, Line},
  try add_new_type(Attr, Name, TypeForm, Args, Module, FN, RecDict) of
    NewRecDict ->
      get_record_and_type_info(Left, Module, NewRecDict, File)
  catch
    throw:{error, _} = Error -> Error
  end;
get_record_and_type_info([{file, _, [{IncludeFile, _}]}|Left],
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
      Msg = flat_format("Type ~ts/~w already defined\n", [Name, Arity]),
      throw({error, Msg});
    false ->
      try erl_types:t_var_names(ArgForms) of
        ArgNames ->
	  maps:put({TypeOrOpaque, Name, Arity},
                   {{Module, FN, TypeForm, ArgNames},
                    erl_types:t_any()}, RecDict)
      catch
        _:_ ->
	  throw({error, flat_format("Type declaration for ~tw does not "
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
  ExpTypes = dialyzer_codeserver:get_exported_types(CServer),
  Mods = dialyzer_codeserver:all_temp_modules(CServer),
  process_opaque_types0(Mods, CServer, ExpTypes),
  VarTable = erl_types:var_table__new(),
  RecordTable = dialyzer_codeserver:get_temp_records_table(CServer),
  ModuleFun =
    fun(Module) ->
        RecordMap = dialyzer_codeserver:lookup_temp_mod_records(Module, CServer),
        RecordFun =
          fun({Key, Value}, C2) ->
              case Key of
                {record, Name} ->
                  FieldFun =
                    fun({Arity, Fields}, C4) ->
                        MRA = {Module, Name, Arity},
                        Site = {record, MRA},
                        {Fields1, C7} =
                          lists:mapfoldl(fun({FieldName, Field, _}, C5) ->
                                             check_remote(Field, ExpTypes,
                                                          MRA, RecordTable),
                                             {FieldT, C6} =
                                               erl_types:t_from_form
                                                 (Field, ExpTypes, Site,
                                                  RecordTable, VarTable,
                                                  C5),
                                          {{FieldName, Field, FieldT}, C6}
                                      end, C4, Fields),
                        {{Arity, Fields1}, C7}
                    end,
                  {FileLine, Fields} = Value,
                  {FieldsList, C3} =
                    lists:mapfoldl(FieldFun, C2, orddict:to_list(Fields)),
                  {{Key, {FileLine, orddict:from_list(FieldsList)}}, C3};
                {_TypeOrOpaque, Name, NArgs} ->
                  %% Make sure warnings about unknown types are output
                  %% also for types unused by specs.
                  MTA = {Module, Name, NArgs},
                  {{_Module, _FileLine, Form, _ArgNames}, _Type} = Value,
                  check_remote(Form, ExpTypes, MTA, RecordTable),
                  {{Key, Value}, C2}
              end
          end,
        Cache = erl_types:cache__new(),
        {RecordList, _NewCache} =
          lists:mapfoldl(RecordFun, Cache, maps:to_list(RecordMap)),
        dialyzer_codeserver:store_temp_records(Module,
                                               maps:from_list(RecordList),
                                               CServer)
    end,
  lists:foreach(ModuleFun, Mods),
  check_record_fields(Mods, CServer, ExpTypes),
  dialyzer_codeserver:finalize_records(CServer).

%% erl_types:t_from_form() substitutes the declaration of opaque types
%% for the expanded type in some cases. To make sure the initial type,
%% any(), is not used, the expansion is done twice.
%% XXX: Recursive opaque types are not handled well.
process_opaque_types0(AllModules, CServer, TempExpTypes) ->
  process_opaque_types(AllModules, CServer, TempExpTypes),
  process_opaque_types(AllModules, CServer, TempExpTypes).

process_opaque_types(AllModules, CServer, TempExpTypes) ->
  VarTable = erl_types:var_table__new(),
  RecordTable = dialyzer_codeserver:get_temp_records_table(CServer),
  ModuleFun =
    fun(Module) ->
        RecordMap = dialyzer_codeserver:lookup_temp_mod_records(Module, CServer),
        RecordFun =
          fun({Key, Value}, C2) ->
              case Key of
                {opaque, Name, NArgs} ->
                  {{_Module, _FileLine, Form, _ArgNames}=F, _Type} = Value,
                  Site = {type, {Module, Name, NArgs}},
                  {Type, C3} =
                    erl_types:t_from_form(Form, TempExpTypes, Site,
                                          RecordTable, VarTable, C2),
                  {{Key, {F, Type}}, C3};
                {type, _Name, _NArgs} ->
                  {{Key, Value}, C2};
                {record, _RecName} ->
                  {{Key, Value}, C2}
              end
          end,
        C0 = erl_types:cache__new(),
        {RecordList, _NewCache} =
          lists:mapfoldl(RecordFun, C0, maps:to_list(RecordMap)),
        dialyzer_codeserver:store_temp_records(Module,
                                               maps:from_list(RecordList),
                                               CServer)
    end,
  lists:foreach(ModuleFun, AllModules).

check_record_fields(AllModules, CServer, TempExpTypes) ->
  VarTable = erl_types:var_table__new(),
  RecordTable = dialyzer_codeserver:get_temp_records_table(CServer),
  CheckFun =
    fun(Module) ->
        CheckForm = fun(Form, Site, C1) ->
                        erl_types:t_check_record_fields(Form, TempExpTypes,
                                                        Site, RecordTable,
                                                        VarTable, C1)
                  end,
        RecordMap = dialyzer_codeserver:lookup_temp_mod_records(Module, CServer),
        RecordFun =
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
        C0 = erl_types:cache__new(),
        _ = lists:foldl(RecordFun, C0, maps:to_list(RecordMap))
    end,
  lists:foreach(CheckFun, AllModules).

msg_with_position(Fun, FileLine) ->
  try Fun()
  catch
    throw:{error, Msg} ->
      {File, Line} = FileLine,
      BaseName = filename:basename(File),
      NewMsg = io_lib:format("~ts:~p: ~ts", [BaseName, Line, Msg]),
      throw({error, NewMsg})
  end.

check_remote(Form, ExpTypes, What, RecordTable) ->
  erl_types:t_from_form_check_remote(Form, ExpTypes, What, RecordTable).

-spec merge_types(codeserver(), dialyzer_plt:plt()) -> codeserver().

merge_types(CServer, Plt) ->
  AllNewModules = dialyzer_codeserver:all_temp_modules(CServer),
  AllNewModulesSet = sets:from_list(AllNewModules),
  AllOldModulesSet = dialyzer_plt:all_modules(Plt),
  AllModulesSet = sets:union(AllNewModulesSet, AllOldModulesSet),
  ModuleFun =
    fun(Module) ->
        KeepOldFun =
          fun() ->
              case dialyzer_plt:get_module_types(Plt, Module) of
                none -> no;
                {value, OldRecords} ->
                  case sets:is_element(Module, AllNewModulesSet) of
                    true -> no;
                    false -> {yes, OldRecords}
                  end
              end
          end,
        Records =
          case KeepOldFun() of
            no ->
              dialyzer_codeserver:lookup_temp_mod_records(Module, CServer);
            {yes, OldRecords} ->
              OldRecords
          end,
        dialyzer_codeserver:store_temp_records(Module, Records, CServer)
    end,
  lists:foreach(ModuleFun, sets:to_list(AllModulesSet)),
  CServer.

%% ============================================================================
%%
%%  Spec info
%%
%% ============================================================================

-type spec_map()     :: dialyzer_codeserver:contracts().
-type callback_map() :: dialyzer_codeserver:contracts().

-spec get_spec_info(module(), cerl:c_module(), type_table()) ->
        {'ok', spec_map(), callback_map()} | {'error', string()}.

get_spec_info(ModName, Core, RecordsMap) ->
  Tuples = core_to_attr_tuples(Core),
  OptionalCallbacks0 = get_optional_callbacks(Tuples, ModName),
  OptionalCallbacks = gb_sets:from_list(OptionalCallbacks0),
  get_spec_info(Tuples, maps:new(), maps:new(),
		RecordsMap, ModName, OptionalCallbacks, "nofile").

get_optional_callbacks(Tuples, ModName) ->
  [{ModName, F, A} || {optional_callbacks, _, O} <- Tuples, is_fa_list(O), {F, A} <- O].

%% TypeSpec is a list of conditional contracts for a function.
%% Each contract is of the form {[Argument], Range, [Constraint]} where
%%  - Argument and Range are in erl_types:erl_type() format and
%%  - Constraint is of the form {subtype, T1, T2} where T1 and T2
%%    are erl_types:erl_type()

get_spec_info([{Contract, Ln, [{Id, TypeSpec}]}|Left],
	      SpecMap, CallbackMap, RecordsMap, ModName, OptCb, File)
  when ((Contract =:= 'spec') or (Contract =:= 'callback')),
       is_list(TypeSpec) ->
  MFA = case Id of
	  {_, _, _} = T -> T;
	  {F, A} -> {ModName, F, A}
	end,
  Xtra = [optional_callback || gb_sets:is_member(MFA, OptCb)],
  ActiveMap =
    case Contract of
      spec     -> SpecMap;
      callback -> CallbackMap
    end,
  try maps:find(MFA, ActiveMap) of
    error ->
      SpecData = {TypeSpec, Xtra},
      NewActiveMap =
	dialyzer_contracts:store_tmp_contract(ModName, MFA, {File, Ln},
                                              SpecData, ActiveMap,
                                              RecordsMap),
      {NewSpecMap, NewCallbackMap} =
	case Contract of
	  spec     -> {NewActiveMap, CallbackMap};
	  callback -> {SpecMap, NewActiveMap}
	end,
      get_spec_info(Left, NewSpecMap, NewCallbackMap,
		    RecordsMap, ModName, OptCb, File);
    {ok, {{OtherFile, L}, _D}} ->
      {Mod, Fun, Arity} = MFA,
      Msg = flat_format("  Contract/callback for function ~w:~tw/~w "
			"already defined in ~ts:~w\n",
			[Mod, Fun, Arity, OtherFile, L]),
      throw({error, Msg})
  catch
    throw:{error, Error} ->
      {error, flat_format("  Error while parsing contract in line ~w: ~ts\n",
			  [Ln, Error])}
  end;
get_spec_info([{file, _, [{IncludeFile, _}]}|Left],
	      SpecMap, CallbackMap, RecordsMap, ModName, OptCb, _File) ->
  get_spec_info(Left, SpecMap, CallbackMap,
		RecordsMap, ModName, OptCb, IncludeFile);
get_spec_info([_Other|Left], SpecMap, CallbackMap,
	      RecordsMap, ModName, OptCb, File) ->
  get_spec_info(Left, SpecMap, CallbackMap,
                RecordsMap, ModName, OptCb, File);
get_spec_info([], SpecMap, CallbackMap,
              _RecordsMap, _ModName, _OptCb, _File) ->
  {ok, SpecMap, CallbackMap}.

core_to_attr_tuples(Core) ->
  [{cerl:concrete(Key), get_core_line(cerl:get_ann(Key)), cerl:concrete(Value)} ||
   {Key, Value} <- cerl:module_attrs(Core)].

get_core_line([L | _As]) when is_integer(L) -> L;
get_core_line([_ | As]) -> get_core_line(As);
get_core_line([]) -> undefined.

-spec get_fun_meta_info(module(), cerl:c_module(), [dial_warn_tag()]) ->
                dialyzer_codeserver:fun_meta_info() | {'error', string()}.

get_fun_meta_info(M, Core, LegalWarnings) ->
  Functions = lists:map(fun cerl:var_name/1, cerl:module_vars(Core)),
  try
    {get_nowarn_unused_function(M, Core, Functions),
     get_func_suppressions(M, Core, Functions)}
  of
    {NoWarn, FuncSupp} ->
      Warnings0 = get_options(Core, LegalWarnings),
      Warnings = ordsets:to_list(Warnings0),
      ModuleWarnings = [{M, W} || W <- Warnings],
      RawProps = lists:append([NoWarn, FuncSupp, ModuleWarnings]),
      process_options(dialyzer_utils:family(RawProps), Warnings0)
  catch throw:{error, _} = Error ->
      Error
  end.

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

-spec get_nowarn_unused_function(module(), cerl:c_module(), [fa()]) ->
                                    [{mfa(), 'no_unused'}].

get_nowarn_unused_function(M, Core, Functions) ->
  Opts = get_options_with_tag(compile, Core),
  Warn = erl_lint:bool_option(warn_unused_function, nowarn_unused_function,
                              true, Opts),
  AttrFile = collect_attribute(Core, compile),
  TagsFaList = check_fa_list(AttrFile, nowarn_unused_function, Functions),
  FAs = case Warn of
          false -> Functions;
          true ->
            [FA || {{[nowarn_unused_function],_L,_File}, FA} <- TagsFaList]
        end,
  [{{M, F, A}, no_unused} || {F, A} <- FAs].

-spec get_func_suppressions(module(), cerl:c_module(), [fa()]) ->
                            [{mfa(), 'nowarn_function' | dial_warn_tag()}].

get_func_suppressions(M, Core, Functions) ->
  AttrFile = collect_attribute(Core, dialyzer),
  TagsFAs = check_fa_list(AttrFile, '*', Functions),
  %% Check the options:
  Fun = fun({{[nowarn_function], _L, _File}, _FA}) -> ok;
           ({OptLFile, _FA}) ->
            _ = get_options1([OptLFile], ordsets:new())
        end,
  lists:foreach(Fun, TagsFAs),
  [{{M, F, A}, W} || {{Warnings, _L, _File}, {F, A}} <- TagsFAs, W <- Warnings].

-spec get_options(cerl:c_module(), [dial_warn_tag()]) ->
                     ordsets:ordset(dial_warn_tag()).

get_options(Core, LegalWarnings) ->
  AttrFile = collect_attribute(Core, dialyzer),
  get_options1(AttrFile, LegalWarnings).

get_options1([{Args, L, File}|Left], Warnings) ->
  Opts = [O || O <- Args, is_atom(O)],
  try dialyzer_options:build_warnings(Opts, Warnings) of
    NewWarnings ->
      get_options1(Left, NewWarnings)
  catch
    throw:{dialyzer_options_error, Msg} ->
      Msg1 = flat_format("  ~ts:~w: ~ts", [File, L, Msg]),
      throw({error, Msg1})
  end;
get_options1([], Warnings) ->
  Warnings.

-type collected_attribute() ::
        {Args :: [term()], erl_anno:line(), file:filename()}.

collect_attribute(Core, Tag) ->
  collect_attribute(cerl:module_attrs(Core), Tag, "nofile").

collect_attribute([{Key, Value}|T], Tag, File) ->
  case cerl:concrete(Key) of
    Tag ->
      [{cerl:concrete(Value), get_core_line(cerl:get_ann(Key)), File} |
       collect_attribute(T, Tag, File)];
    file ->
      [{IncludeFile, _}] = cerl:concrete(Value),
      collect_attribute(T, Tag, IncludeFile);
    _ ->
      collect_attribute(T, Tag, File)
  end;
collect_attribute([], _Tag, _File) ->
  [].

-spec is_suppressed_fun(mfa(), codeserver()) -> boolean().

is_suppressed_fun(MFA, CodeServer) ->
  lookup_fun_property(MFA, nowarn_function, CodeServer, false).

-spec is_suppressed_tag(mfa() | module(), dial_warn_tag(), codeserver()) ->
                           boolean().

is_suppressed_tag(MorMFA, Tag, Codeserver) ->
  not lookup_fun_property(MorMFA, Tag, Codeserver, true).

lookup_fun_property({M, _F, _A}=MFA, Property, CodeServer, NoInfoReturn) ->
  case dialyzer_codeserver:lookup_meta_info(MFA, CodeServer) of
    error ->
      lookup_fun_property(M, Property, CodeServer, NoInfoReturn);
    {ok, MFAPropList} ->
      case proplists:get_value(Property, MFAPropList, no) of
        mod -> false; % suppressed in function
        func -> true; % requested in function
        no -> lookup_fun_property(M, Property, CodeServer, NoInfoReturn)
      end
  end;
lookup_fun_property(M, Property, CodeServer, NoInfoReturn) when is_atom(M) ->
  case dialyzer_codeserver:lookup_meta_info(M, CodeServer) of
    error ->
      NoInfoReturn;
    {ok, MPropList} ->
      proplists:is_defined(Property, MPropList)
  end.

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
   dialyzer, no_spawn_compiler_process].

-spec format_errors([{module(), string()}]) -> [string()].

format_errors([{Mod, Errors}|Left]) ->
  FormatedError =
    [io_lib:format("~ts:~w: ~ts\n", [Mod, Line, M:format_error(Desc)])
     || {Line, M, Desc} <- Errors],
  [lists:flatten(FormatedError) | format_errors(Left)];
format_errors([]) ->
  [].

-spec format_sig(erl_types:erl_type()) -> string().

format_sig(Type) ->
  format_sig(Type, maps:new()).

-spec format_sig(erl_types:erl_type(), type_table()) -> string().

format_sig(Type, RecDict) ->
  "fun(" ++ Sig = lists:flatten(erl_types:t_to_string(Type, RecDict)),
  ")" ++ RevSig = lists:reverse(Sig),
  lists:reverse(RevSig).

flat_format(Fmt, Lst) ->
  lists:flatten(io_lib:format(Fmt, Lst)).

-spec get_options_with_tag(atom(), cerl:c_module()) -> [term()].

get_options_with_tag(Tag, Core) ->
  [O || {Key, Value} <- cerl:module_attrs(Core),
        cerl:concrete(Key) =:= Tag,
        O <- cerl:concrete(Value)].

%% Check F/A, and collect (unchecked) warning tags with line and file.
-spec check_fa_list([collected_attribute()], atom(), [fa()]) ->
                       [{{atom(), erl_anno:line(), file:filename()},fa()}].

check_fa_list(AttrFile, Tag, Functions) ->
  FuncTab = gb_sets:from_list(Functions),
  check_fa_list1(AttrFile, Tag, FuncTab).

check_fa_list1([{Args, L, File}|Left], Tag, Funcs) ->
  TermsL = [{{[Tag0], L, File}, Term} ||
             {Tags, Terms0} <- Args,
             Tag0 <- lists:flatten([Tags]),
             Tag =:= '*' orelse Tag =:= Tag0,
             Term <- lists:flatten([Terms0])],
  case lists:dropwhile(fun({_, T}) -> is_fa(T) end, TermsL) of
    [] -> ok;
    [{_, Bad}|_] ->
      Msg1 = flat_format("  Bad function ~tw in line ~ts:~w",
                         [Bad, File, L]),
      throw({error, Msg1})
  end,
  case lists:dropwhile(fun({_, FA}) -> is_known(FA, Funcs) end, TermsL) of
    [] -> ok;
    [{_, {F, A}}|_] ->
      Msg2 = flat_format("  Unknown function ~tw/~w in line ~ts:~w",
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
      case cerl:concrete(Node) of
        Map when is_map(Map) ->
          pp_map(Node, Ctxt, Cont);
        Bitstr when is_bitstring(Bitstr) ->
          pp_binary(Node, Ctxt, Cont);
        _ ->
          Cont(Node, Ctxt)
      end;
    _ ->
      Cont(Node, Ctxt)
  end.

pp_binary(Node, Ctxt, Cont) ->
  prettypr:beside(prettypr:text("<<"),
		  prettypr:beside(pp_segments(cerl_binary_segments(Node),
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
  RestPP =
    case {concrete(Unit), concrete(Type), concrete(Flags)} of
      {1, integer, [unsigned, big]} -> % Simplify common cases.
        case concrete(Size) of
          8 -> prettypr:text("");
          _ -> pp_size(Size, Ctxt, Cont)
        end;
      {8, binary, [unsigned, big]} ->
        SizePP = pp_size(Size, Ctxt, Cont),
        prettypr:beside(SizePP,
                        prettypr:beside(prettypr:text("/"), pp_atom(Type)));
      _What ->
        SizePP = pp_size(Size, Ctxt, Cont),
        UnitPP = pp_unit(Unit, Ctxt, Cont),
        OptsPP = pp_opts(Type, Flags),
        prettypr:beside(SizePP, prettypr:beside(OptsPP, UnitPP))
    end,
  prettypr:beside(Cont(Val, Ctxt), RestPP).

concrete(Cerl) ->
  try cerl:concrete(Cerl)
  catch _:_ -> anything_unexpected
  end.

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

cerl_binary_segments(#c_literal{val = B}) when is_bitstring(B) ->
  segs_from_bitstring(B);
cerl_binary_segments(CBinary) ->
  cerl:binary_segments(CBinary).

%% Copied from core_pp. The function cerl:binary_segments/2 should/could
%% be extended to handle literals, but then the cerl module cannot be
%% HiPE-compiled as of Erlang/OTP 22.0 (due to <<I:N>>).
segs_from_bitstring(<<H,T/bitstring>>) ->
    [#c_bitstr{val=#c_literal{val=H},
	       size=#c_literal{val=8},
	       unit=#c_literal{val=1},
	       type=#c_literal{val=integer},
	       flags=#c_literal{val=[unsigned,big]}}|segs_from_bitstring(T)];
segs_from_bitstring(<<>>) ->
    [];
segs_from_bitstring(Bitstring) ->
    N = bit_size(Bitstring),
    <<I:N>> = Bitstring,
    [#c_bitstr{val=#c_literal{val=I},
	      size=#c_literal{val=N},
	      unit=#c_literal{val=1},
	      type=#c_literal{val=integer},
	      flags=#c_literal{val=[unsigned,big]}}].

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

-spec ets_tab2list(ets:tid()) -> list().

%% Deletes the contents of the table. Use:
%%  ets_tab2list(T), ets:delete(T)
%% instead of:
%%  ets:tab2list(T), ets:delete(T)
%% to save some memory at the expense of somewhat longer execution time.
ets_tab2list(T) ->
  F = fun(Vs, A) -> Vs ++ A end,
  ets_take(ets:first(T), T, F, []).

-spec ets_move(From :: ets:tid(), To :: ets:tid()) -> 'ok'.

ets_move(T1, T2) ->
  F = fun(Es, A) -> true = ets:insert(T2, Es), A end,
  [] = ets_take(ets:first(T1), T1, F, []),
  ok.

ets_take('$end_of_table', T, F, A) ->
  case ets:first(T) of % no safe_fixtable()...
    '$end_of_table' -> A;
    Key -> ets_take(Key, T, F, A)
  end;
ets_take(Key, T, F, A) ->
  Vs = ets:lookup(T, Key),
  Key1 = ets:next(T, Key),
  true = ets:delete(T, Key),
  ets_take(Key1, T, F, F(Vs, A)).

-spec parallelism() -> integer().

parallelism() ->
  erlang:system_info(schedulers_online).

-spec family([{K,V}]) -> [{K,[V]}].

family(L) ->
    sofs:to_external(sofs:rel2fam(sofs:relation(L))).
