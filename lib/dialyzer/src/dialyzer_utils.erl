%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2014. All Rights Reserved.
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
	 merge_records/2,
	 pp_hook/0,
	 process_record_remote_types/1,
         sets_filter/2,
	 src_compiler_opts/0,
	 parallelism/0
	]).

-include("dialyzer.hrl").

%%-define(DEBUG, true).

-ifdef(DEBUG).
print_types(RecDict) ->
  Keys = dict:fetch_keys(RecDict),
  print_types1(Keys, RecDict).

print_types1([], _) ->
  ok;
print_types1([{type, _Name} = Key|T], RecDict) ->
  {ok, {_Mod, Form, _Args}} = dict:find(Key, RecDict),
  io:format("\n~w: ~w\n", [Key, erl_types:t_from_form(Form, RecDict)]),
  print_types1(T, RecDict);
print_types1([{opaque, _Name} = Key|T], RecDict) ->
  {ok, {_Mod, Form, _Args}} = dict:find(Key, RecDict),
  io:format("\n~w: ~w\n", [Key, erl_types:t_from_form(Form, RecDict)]),
  print_types1(T, RecDict);
print_types1([{record, _Name} = Key|T], RecDict) ->
  {ok, [{_Arity, _Fields} = AF]} = dict:find(Key, RecDict),
  io:format("~w: ~w\n\n", [Key, AF]),
  print_types1(T, RecDict).
-define(debug(D_), print_types(D_)).
-else.
-define(debug(D_), ok).
-endif.

%% ----------------------------------------------------------------------------

-type abstract_code() :: [tuple()]. %% XXX: import from somewhere
-type comp_options()  :: [compile:option()].
-type mod_or_fname()  :: atom() | file:filename().

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

-spec get_record_and_type_info(abstract_code()) ->
	{'ok', dict:dict()} | {'error', string()}.

get_record_and_type_info(AbstractCode) ->
  Module = get_module(AbstractCode),
  get_record_and_type_info(AbstractCode, Module, dict:new()).

-spec get_record_and_type_info(abstract_code(), module(), dict:dict()) ->
	{'ok', dict:dict()} | {'error', string()}.

get_record_and_type_info(AbstractCode, Module, RecDict) ->
  get_record_and_type_info(AbstractCode, Module, [], RecDict).

get_record_and_type_info([{attribute, _, record, {Name, Fields0}}|Left],
			 Module, Records, RecDict) ->
  {ok, Fields} = get_record_fields(Fields0, RecDict),
  Arity = length(Fields),
  NewRecDict = dict:store({record, Name}, [{Arity, Fields}], RecDict),
  get_record_and_type_info(Left, Module, [{record, Name}|Records], NewRecDict);
get_record_and_type_info([{attribute, _, type, {{record, Name}, Fields0, []}}
			  |Left], Module, Records, RecDict) ->
  %% This overrides the original record declaration.
  {ok, Fields} = get_record_fields(Fields0, RecDict),
  Arity = length(Fields),
  NewRecDict = dict:store({record, Name}, [{Arity, Fields}], RecDict),
  get_record_and_type_info(Left, Module, Records, NewRecDict);
get_record_and_type_info([{attribute, _, Attr, {Name, TypeForm}}|Left],
			 Module, Records, RecDict) when Attr =:= 'type';
                                                        Attr =:= 'opaque' ->
  try
    NewRecDict = add_new_type(Attr, Name, TypeForm, [], Module, RecDict),
    get_record_and_type_info(Left, Module, Records, NewRecDict)
  catch
    throw:{error, _} = Error -> Error
  end;
get_record_and_type_info([{attribute, _, Attr, {Name, TypeForm, Args}}|Left],
			 Module, Records, RecDict) when Attr =:= 'type';
                                                        Attr =:= 'opaque' ->
  try
    NewRecDict = add_new_type(Attr, Name, TypeForm, Args, Module, RecDict),
    get_record_and_type_info(Left, Module, Records, NewRecDict)
  catch
    throw:{error, _} = Error -> Error
  end;
get_record_and_type_info([_Other|Left], Module, Records, RecDict) ->
  get_record_and_type_info(Left, Module, Records, RecDict);
get_record_and_type_info([], _Module, Records, RecDict) ->
  case type_record_fields(lists:reverse(Records), RecDict) of
    {ok, _NewRecDict} = Ok ->
      ?debug(_NewRecDict),
      Ok;
    {error, Name, Error} ->
      {error, flat_format("  Error while parsing #~w{}: ~s\n", [Name, Error])}
  end.

add_new_type(TypeOrOpaque, Name, TypeForm, ArgForms, Module, RecDict) ->
  Arity = length(ArgForms),
  case erl_types:type_is_defined(TypeOrOpaque, Name, Arity, RecDict) of
    true ->
      Msg = flat_format("Type ~s/~w already defined\n", [Name, Arity]),
      throw({error, Msg});
    false ->
      ArgTypes = [erl_types:t_from_form(X) || X <- ArgForms],
      case lists:all(fun erl_types:t_is_var/1, ArgTypes) of
	true ->
	  ArgNames = [erl_types:t_var_name(X) || X <- ArgTypes],
	  dict:store({TypeOrOpaque, Name, Arity},
                     {Module, TypeForm, ArgNames}, RecDict);
	false ->
	  throw({error, flat_format("Type declaration for ~w does not "
				    "have variables as parameters", [Name])})
      end
  end.

get_record_fields(Fields, RecDict) ->
  get_record_fields(Fields, RecDict, []).

get_record_fields([{typed_record_field, OrdRecField, TypeForm}|Left],
		  RecDict, Acc) ->
  Name =
    case OrdRecField of
      {record_field, _Line, Name0} -> erl_parse:normalise(Name0);
      {record_field, _Line, Name0, _Init} -> erl_parse:normalise(Name0)
    end,
    get_record_fields(Left, RecDict, [{Name, TypeForm}|Acc]);
get_record_fields([{record_field, _Line, Name}|Left], RecDict, Acc) ->
  NewAcc = [{erl_parse:normalise(Name), {var, -1, '_'}}|Acc],
  get_record_fields(Left, RecDict, NewAcc);
get_record_fields([{record_field, _Line, Name, _Init}|Left], RecDict, Acc) ->
  NewAcc = [{erl_parse:normalise(Name), {var, -1, '_'}}|Acc],
  get_record_fields(Left, RecDict, NewAcc);
get_record_fields([], _RecDict, Acc) ->
  {ok, lists:reverse(Acc)}.

type_record_fields([], RecDict) ->
  {ok, RecDict};
type_record_fields([RecKey|Recs], RecDict) ->
  {ok, [{Arity, Fields}]} = dict:find(RecKey, RecDict),
  try
    TypedFields =
      [{FieldName, erl_types:t_from_form(FieldTypeForm, RecDict)}
       || {FieldName, FieldTypeForm} <- Fields],
    RecDict1 = dict:store(RecKey, [{Arity, TypedFields}], RecDict),
    Fun = fun(OldOrdDict) ->
              orddict:store(Arity, TypedFields, OldOrdDict)
          end,
    RecDict2 = dict:update(RecKey, Fun, RecDict1),
    type_record_fields(Recs, RecDict2)
  catch
    throw:{error, Error} ->
      {record, Name} = RecKey,
      {error, Name, Error}
  end.

-spec process_record_remote_types(dialyzer_codeserver:codeserver()) -> dialyzer_codeserver:codeserver().

process_record_remote_types(CServer) ->
  TempRecords = dialyzer_codeserver:get_temp_records(CServer),
  TempExpTypes = dialyzer_codeserver:get_temp_exported_types(CServer),
  RecordFun =
    fun(Key, Value) ->
	case Key of
	  {record, _Name} ->
	    FieldFun =
	      fun(_Arity, Fields) ->
		  [{Name, erl_types:t_solve_remote(Field, TempExpTypes,
                                                   TempRecords)}
                   || {Name, Field} <- Fields]
	      end,
	    orddict:map(FieldFun, Value);
	  _Other -> Value
	end
    end,
  ModuleFun =
    fun(_Module, Record) ->
	dict:map(RecordFun, Record)
    end,
  NewRecords = dict:map(ModuleFun, TempRecords),
  CServer1 = dialyzer_codeserver:finalize_records(NewRecords, CServer),
  dialyzer_codeserver:finalize_exported_types(TempExpTypes, CServer1).

-spec merge_records(dict:dict(), dict:dict()) -> dict:dict().

merge_records(NewRecords, OldRecords) ->
  dict:merge(fun(_Key, NewVal, _OldVal) -> NewVal end, NewRecords, OldRecords).

%% ============================================================================
%%
%%  Spec info
%%
%% ============================================================================

-type spec_dict()     :: dict:dict().
-type callback_dict() :: dict:dict().

-spec get_spec_info(atom(), abstract_code(), dict:dict()) ->
        {'ok', spec_dict(), callback_dict()} | {'error', string()}.

get_spec_info(ModName, AbstractCode, RecordsDict) ->
  get_spec_info(AbstractCode, dict:new(), dict:new(),
		RecordsDict, ModName, "nofile").

%% TypeSpec is a list of conditional contracts for a function.
%% Each contract is of the form {[Argument], Range, [Constraint]} where
%%  - Argument and Range are in erl_types:erl_type() format and
%%  - Constraint is of the form {subtype, T1, T2} where T1 and T2
%%    are erl_types:erl_type()

get_spec_info([{attribute, Ln, Contract, {Id, TypeSpec}}|Left],
	      SpecDict, CallbackDict, RecordsDict, ModName, File)
  when ((Contract =:= 'spec') or (Contract =:= 'callback')),
       is_list(TypeSpec) ->
  MFA = case Id of
	  {_, _, _} = T -> T;
	  {F, A} -> {ModName, F, A}
	end,
  ActiveDict =
    case Contract of
      spec     -> SpecDict;
      callback -> CallbackDict
    end,
  try dict:find(MFA, ActiveDict) of
    error ->
      NewActiveDict =
	dialyzer_contracts:store_tmp_contract(MFA, {File, Ln}, TypeSpec,
					      ActiveDict, RecordsDict),
      {NewSpecDict, NewCallbackDict} =
	case Contract of
	  spec     -> {NewActiveDict, CallbackDict};
	  callback -> {SpecDict, NewActiveDict}
	end,
      get_spec_info(Left, NewSpecDict, NewCallbackDict,
		    RecordsDict, ModName,File);
    {ok, {{OtherFile, L},_C}} ->
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
	      SpecDict, CallbackDict, RecordsDict, ModName, _File) ->
  get_spec_info(Left, SpecDict, CallbackDict,
		RecordsDict, ModName, IncludeFile);
get_spec_info([_Other|Left], SpecDict, CallbackDict,
	      RecordsDict, ModName, File) ->
  get_spec_info(Left, SpecDict, CallbackDict, RecordsDict, ModName, File);
get_spec_info([], SpecDict, CallbackDict, _RecordsDict, _ModName, _File) ->
  {ok, SpecDict, CallbackDict}.

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
keep_compile_option(asm) -> false;
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

-spec format_sig(erl_types:erl_type(), dict:dict()) -> string().

format_sig(Type, RecDict) ->
  "fun(" ++ Sig = lists:flatten(erl_types:t_to_string(Type, RecDict)),
  ")" ++ RevSig = lists:reverse(Sig),
  lists:reverse(RevSig).

flat_format(Fmt, Lst) ->
  lists:flatten(io_lib:format(Fmt, Lst)).

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

%%------------------------------------------------------------------------------

-spec parallelism() -> integer().

parallelism() ->
  CPUs = erlang:system_info(logical_processors_available),
  Schedulers = erlang:system_info(schedulers),
  min(CPUs, Schedulers).
