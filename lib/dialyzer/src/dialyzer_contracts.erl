%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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

-module(dialyzer_contracts).

-export([check_contract/2,
	 check_contracts/3,
	 contracts_without_fun/3,
	 contract_to_string/1,
	 get_invalid_contract_warnings/3,
	 get_contract_args/1,
	 get_contract_return/1,
	 get_contract_return/2,
	 %% get_contract_signature/1,
	 is_overloaded/1,
	 process_contract_remote_types/1,
	 store_tmp_contract/5]).

-export_type([file_contract/0, plt_contracts/0]).

%%-----------------------------------------------------------------------

-include("dialyzer.hrl").

%%-----------------------------------------------------------------------
%% Types used in other parts of the system below
%%-----------------------------------------------------------------------

-type file_contract() :: {file_line(), #contract{}}.

-type plt_contracts() :: [{mfa(), #contract{}}]. % actually, an orddict()

%%-----------------------------------------------------------------------
%% Internal record for contracts whose components have not been processed
%% to expand records and/or remote types that they might contain.
%%-----------------------------------------------------------------------

-type tmp_contract_fun() :: fun((set(), dict()) -> contract_pair()).

-record(tmp_contract, {contract_funs = [] :: [tmp_contract_fun()],
		       forms	     = [] :: [{_, _}]}).

%%-----------------------------------------------------------------------

%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(X__, Y__), io:format(X__, Y__)).
-else.
-define(debug(X__, Y__), ok).
-endif.

%%-----------------------------------------------------------------------

-spec get_contract_return(#contract{}) -> erl_types:erl_type().

get_contract_return(#contract{contracts = Cs, args = GenArgs}) ->
  process_contracts(Cs, GenArgs).

-spec get_contract_return(#contract{}, [erl_types:erl_type()]) -> erl_types:erl_type().

get_contract_return(#contract{contracts = Cs}, Args) ->
  process_contracts(Cs, Args).

-spec get_contract_args(#contract{}) -> [erl_types:erl_type()].

get_contract_args(#contract{args = Args}) ->
  Args.

-spec get_contract_signature(#contract{}) -> erl_types:erl_type().

get_contract_signature(#contract{contracts = Cs, args = GeneralDomain}) ->
  Range = process_contracts(Cs, GeneralDomain),
  erl_types:t_fun(GeneralDomain, Range).

-spec is_overloaded(#contract{}) -> boolean().

is_overloaded(#contract{contracts = Cs}) ->
  case Cs of
    [_] -> true;
    [_,_|_] -> false
  end.

-spec contract_to_string(#contract{}) -> string().

contract_to_string(#contract{forms = Forms}) ->
  contract_to_string_1(Forms).

contract_to_string_1([{Contract, []}]) ->
  strip_fun(erl_types:t_form_to_string(Contract));
contract_to_string_1([{Contract, []}|Rest]) ->
  strip_fun(erl_types:t_form_to_string(Contract)) ++ "\n    ; "
    ++ contract_to_string_1(Rest);
contract_to_string_1([{Contract, Constraints}]) ->
  strip_fun(erl_types:t_form_to_string(Contract)) ++ " when "
    ++ constraints_to_string(Constraints);
contract_to_string_1([{Contract, Constraints}|Rest]) ->
  strip_fun(erl_types:t_form_to_string(Contract)) ++ " when "
    ++ constraints_to_string(Constraints) ++ ";" ++
    contract_to_string_1(Rest).

strip_fun("fun(" ++ String) ->
  butlast(String).

butlast([]) -> [];
butlast([_]) -> [];
butlast([H|T]) -> [H|butlast(T)].

constraints_to_string([]) ->
  "";
constraints_to_string([{type, _, constraint, [{atom, _, What}, Types]}]) ->
  atom_to_list(What) ++ "(" ++
    sequence([erl_types:t_form_to_string(T) || T <- Types], ",") ++ ")";
constraints_to_string([{type, _, constraint, [{atom, _, What}, Types]}|Rest]) ->
  atom_to_list(What) ++ "(" ++
    sequence([erl_types:t_form_to_string(T) || T <- Types], ",")
    ++ "), " ++ constraints_to_string(Rest).

sequence([], _Delimiter) -> "";
sequence([H], _Delimiter) -> H;
sequence([H|T], Delimiter) -> H ++ Delimiter ++ sequence(T, Delimiter).

-spec process_contract_remote_types(dialyzer_codeserver:codeserver()) ->
	  dialyzer_codeserver:codeserver().

process_contract_remote_types(CodeServer) ->
  {TmpContractDict, TmpCallbackDict} =
    dialyzer_codeserver:get_temp_contracts(CodeServer),
  ExpTypes = dialyzer_codeserver:get_exported_types(CodeServer),
  RecordDict = dialyzer_codeserver:get_records(CodeServer),
  ContractFun =
    fun({_M, _F, _A}, {File, #tmp_contract{contract_funs = CFuns, forms = Forms}}) ->
	NewCs = [CFun(ExpTypes, RecordDict) || CFun <- CFuns],
	Args = general_domain(NewCs),
	{File, #contract{contracts = NewCs, args = Args, forms = Forms}}
    end,
  ModuleFun =
    fun(_ModuleName, ContractDict) ->
	dict:map(ContractFun, ContractDict)
    end,
  NewContractDict = dict:map(ModuleFun, TmpContractDict),
  NewCallbackDict = dict:map(ModuleFun, TmpCallbackDict),
  dialyzer_codeserver:finalize_contracts(NewContractDict, NewCallbackDict,
					 CodeServer).

-spec check_contracts([{mfa(), file_contract()}],
		      dialyzer_callgraph:callgraph(), dict()) -> plt_contracts().

check_contracts(Contracts, Callgraph, FunTypes) ->
  FoldFun =
    fun(Label, Type, NewContracts) ->
	case dialyzer_callgraph:lookup_name(Label, Callgraph) of
	  {ok, {M,F,A} = MFA} ->
	    case orddict:find(MFA, Contracts) of
	      {ok, {_FileLine, Contract}} ->
		case check_contract(Contract, Type) of
		  ok ->
		    case erl_bif_types:is_known(M, F, A) of
		      true ->
			%% Disregard the contracts since
			%% this is a known function.
			NewContracts;
		      false ->
			[{MFA, Contract}|NewContracts]
		    end;
		  {error, _Error} -> NewContracts
		end;
	      error -> NewContracts
	    end;
	  error -> NewContracts
	end
    end,
  dict:fold(FoldFun, [], FunTypes).

%% Checks all components of a contract
-spec check_contract(#contract{}, erl_types:erl_type()) -> 'ok' | {'error', term()}.

check_contract(#contract{contracts = Contracts}, SuccType) ->
  try
    Contracts1 = [{Contract, insert_constraints(Constraints, dict:new())}
		  || {Contract, Constraints} <- Contracts],
    Contracts2 = [erl_types:t_subst(Contract, Dict)
		  || {Contract, Dict} <- Contracts1],
    GenDomains = [erl_types:t_fun_args(C) || C <- Contracts2],
    case check_domains(GenDomains) of
      error ->
	{error, {overlapping_contract, []}};
      ok ->
	InfList = [erl_types:t_inf(Contract, SuccType, opaque)
		   || Contract <- Contracts2],
	case check_contract_inf_list(InfList, SuccType) of
	  {error, _} = Invalid -> Invalid;
	  ok -> check_extraneous(Contracts2, SuccType)
	end
    end
  catch
    throw:{error, _} = Error -> Error
  end.

check_domains([_]) -> ok;
check_domains([Dom|Doms]) ->
  Fun = fun(D) ->
	    erl_types:any_none_or_unit(erl_types:t_inf_lists(Dom, D, opaque))
	end,
  case lists:all(Fun, Doms) of
    true -> check_domains(Doms);
    false -> error
  end.

%% Allow a contract if one of the overloaded contracts is possible.
%% We used to be more strict, e.g., all overloaded contracts had to be
%% possible.
check_contract_inf_list([FunType|Left], SuccType) ->
  FunArgs = erl_types:t_fun_args(FunType),
  case lists:any(fun erl_types:t_is_none_or_unit/1, FunArgs) of
    true -> check_contract_inf_list(Left, SuccType);
    false ->
      STRange = erl_types:t_fun_range(SuccType),
      case erl_types:t_is_none_or_unit(STRange) of
	true -> ok;
	false ->
	  Range = erl_types:t_fun_range(FunType),
	  case erl_types:t_is_none(erl_types:t_inf(STRange, Range, opaque)) of
	    true -> check_contract_inf_list(Left, SuccType);
	    false -> ok
	  end
      end
  end;
check_contract_inf_list([], _SuccType) ->
  {error, invalid_contract}.

check_extraneous([], _SuccType) -> ok;
check_extraneous([C|Cs], SuccType) ->
  case check_extraneous_1(C, SuccType) of
    ok -> check_extraneous(Cs, SuccType);
    Error -> Error
  end.

check_extraneous_1(Contract, SuccType) ->
  CRng = erl_types:t_fun_range(Contract),
  CRngs = erl_types:t_elements(CRng),
  STRng = erl_types:t_fun_range(SuccType),
  ?debug("CR = ~p\nSR = ~p\n", [CRngs, STRng]),
  case [CR || CR <- CRngs,
              erl_types:t_is_none(erl_types:t_inf(CR, STRng, opaque))] of
    [] ->
      CRngList = list_part(CRng),
      STRngList = list_part(STRng),
      case is_not_nil_list(CRngList) andalso is_not_nil_list(STRngList) of
        false -> ok;
        true ->
          CRngElements = erl_types:t_list_elements(CRngList),
          STRngElements = erl_types:t_list_elements(STRngList),
          Inf = erl_types:t_inf(CRngElements, STRngElements, opaque),
          case erl_types:t_is_none(Inf) of
            true -> {error, invalid_contract};
            false -> ok
          end
      end;
    CRs -> {error, {extra_range, erl_types:t_sup(CRs), STRng}}
  end.

list_part(Type) ->
  erl_types:t_inf(erl_types:t_list(), Type, opaque).

is_not_nil_list(Type) ->
  erl_types:t_is_list(Type) andalso not erl_types:t_is_nil(Type).

%% This is the heart of the "range function"
-spec process_contracts([contract_pair()], [erl_types:erl_type()]) -> erl_types:erl_type().

process_contracts(OverContracts, Args) ->
  process_contracts(OverContracts, Args, erl_types:t_none()).

process_contracts([OverContract|Left], Args, AccRange) ->
  NewAccRange =
    case process_contract(OverContract, Args) of
      error -> AccRange;
      {ok, Range} -> erl_types:t_sup(AccRange, Range)
    end,
  process_contracts(Left, Args, NewAccRange);
process_contracts([], _Args, AccRange) ->
  AccRange.

-spec process_contract(contract_pair(), [erl_types:erl_type()]) -> 'error' | {'ok', erl_types:erl_type()}.

process_contract({Contract, Constraints}, CallTypes0) ->
  CallTypesFun = erl_types:t_fun(CallTypes0, erl_types:t_any()),
  ContArgsFun = erl_types:t_fun(erl_types:t_fun_args(Contract),
				erl_types:t_any()),
  ?debug("Instance: Contract:  ~s\n          Arguments: ~s\n",
	 [erl_types:t_to_string(ContArgsFun),
	  erl_types:t_to_string(CallTypesFun)]),
  case solve_constraints(ContArgsFun, CallTypesFun, Constraints) of
    {ok, VarDict} ->
      {ok, erl_types:t_subst(erl_types:t_fun_range(Contract), VarDict)};
    error -> error
  end.

solve_constraints(Contract, Call, Constraints) ->
  %% First make sure the call follows the constraints
  CDict = insert_constraints(Constraints, dict:new()),
  Contract1 = erl_types:t_subst(Contract, CDict),
  %% Just a safe over-approximation.
  %% TODO: Find the types for type variables properly
  ContrArgs = erl_types:t_fun_args(Contract1),
  CallArgs = erl_types:t_fun_args(Call),
  InfList = erl_types:t_inf_lists(ContrArgs, CallArgs),
  case erl_types:any_none_or_unit(InfList) of
    true -> error;
    false -> {ok, CDict}
  end.
  %%Inf = erl_types:t_inf(Contract1, Call),
  %% Then unify with the constrained call type.
  %%  ?debug("Call: ~s\n", [erl_types:t_to_string(Call)]),
  %%  ?debug("Contract: ~s\n", [erl_types:t_to_string(Contract)]),
  %%  ?debug("Contract1: ~s\n", [erl_types:t_to_string(Contract1)]),
  %%  ?debug("Inf: ~s\n", [erl_types:t_to_string(Inf)]),
  %%  erl_types:t_assign_variables_to_subtype(Contract, Inf).

%% Checks the contracts for functions that are not implemented
-spec contracts_without_fun(dict(), [_], dialyzer_callgraph:callgraph()) -> [dial_warning()].

contracts_without_fun(Contracts, AllFuns0, Callgraph) ->
  AllFuns1 = [{dialyzer_callgraph:lookup_name(Label, Callgraph), Arity}
	      || {Label, Arity} <- AllFuns0],
  AllFuns2 = [{M, F, A} || {{ok, {M, F, _}}, A} <- AllFuns1],
  AllContractMFAs = dict:fetch_keys(Contracts),
  ErrorContractMFAs = AllContractMFAs -- AllFuns2,
  [warn_spec_missing_fun(MFA, Contracts) || MFA <- ErrorContractMFAs].

warn_spec_missing_fun({M, F, A} = MFA, Contracts) ->
  {FileLine, _Contract} = dict:fetch(MFA, Contracts),
  {?WARN_CONTRACT_SYNTAX, FileLine, {spec_missing_fun, [M, F, A]}}.

%% This treats the "when" constraints. It will be extended, we hope.
insert_constraints([{subtype, Type1, Type2}|Left], Dict) ->
  case erl_types:t_is_var(Type1) of
    true ->
      Name = erl_types:t_var_name(Type1),
      Dict1 = case dict:find(Name, Dict) of
		error ->
		  dict:store(Name, Type2, Dict);
		{ok, VarType} ->
		  dict:store(Name, erl_types:t_inf(VarType, Type2), Dict)
	      end,
      insert_constraints(Left, Dict1);
    false ->
      %% A lot of things should change to add supertypes
      throw({error, io_lib:format("First argument of is_subtype constraint "
				  "must be a type variable: ~p\n", [Type1])})
  end;
insert_constraints([], Dict) -> Dict.

-spec store_tmp_contract(mfa(), file_line(), [_], dict(), dict()) -> dict().

store_tmp_contract(MFA, FileLine, TypeSpec, SpecDict, RecordsDict) ->
  %% io:format("contract from form: ~p\n", [TypeSpec]),
  TmpContract = contract_from_form(TypeSpec, RecordsDict, FileLine),
  %% io:format("contract: ~p\n", [Contract]),
  dict:store(MFA, {FileLine, TmpContract}, SpecDict).

contract_from_form(Forms, RecDict, FileLine) ->
  {CFuns, Forms1} = contract_from_form(Forms, RecDict, FileLine, [], []),
  #tmp_contract{contract_funs = CFuns, forms = Forms1}.

contract_from_form([{type, _, 'fun', [_, _]} = Form | Left], RecDict,
		   FileLine, TypeAcc, FormAcc) ->
  TypeFun =
    fun(ExpTypes, AllRecords) ->
	Type =
	  try
	    erl_types:t_from_form(Form, RecDict)
	  catch
	    throw:{error, Msg} ->
	      {File, Line} = FileLine,
	      NewMsg = io_lib:format("~s:~p: ~s", [filename:basename(File),
						     Line, Msg]),
	      throw({error, NewMsg})
	  end,
	NewType = erl_types:t_solve_remote(Type, ExpTypes, AllRecords),
	{NewType, []}
    end,
  NewTypeAcc = [TypeFun | TypeAcc],
  NewFormAcc = [{Form, []} | FormAcc],
  contract_from_form(Left, RecDict, FileLine, NewTypeAcc, NewFormAcc);
contract_from_form([{type, _L1, bounded_fun,
		     [{type, _L2, 'fun', [_, _]} = Form, Constr]}| Left],
		   RecDict, FileLine, TypeAcc, FormAcc) ->
  TypeFun =
    fun(ExpTypes, AllRecords) ->
	{Constr1, VarDict} =
	  process_constraints(Constr, RecDict, ExpTypes, AllRecords),
	Type = erl_types:t_from_form(Form, RecDict, VarDict),
	NewType = erl_types:t_solve_remote(Type, ExpTypes, AllRecords),
	{NewType, Constr1}
    end,
  NewTypeAcc = [TypeFun | TypeAcc],
  NewFormAcc = [{Form, Constr} | FormAcc],
  contract_from_form(Left, RecDict, FileLine, NewTypeAcc, NewFormAcc);
contract_from_form([], _RecDict, _FileLine, TypeAcc, FormAcc) ->
  {lists:reverse(TypeAcc), lists:reverse(FormAcc)}.

process_constraints(Constrs, RecDict, ExpTypes, AllRecords) ->
  Init = initialize_constraints(Constrs, RecDict, ExpTypes, AllRecords),
  constraints_fixpoint(Init, RecDict, ExpTypes, AllRecords).

initialize_constraints(Constrs, RecDict, ExpTypes, AllRecords) ->
  initialize_constraints(Constrs, RecDict, ExpTypes, AllRecords, []).

initialize_constraints([], _RecDict, _ExpTypes, _AllRecords, Acc) ->
  Acc;
initialize_constraints([Constr|Rest], RecDict, ExpTypes, AllRecords, Acc) ->
  case Constr of
    {type, _, constraint, [{atom, _, is_subtype}, [Type1, Type2]]} ->
      T1 = final_form(Type1, RecDict, ExpTypes, AllRecords, dict:new()),
      Entry = {T1, Type2},
      initialize_constraints(Rest, RecDict, ExpTypes, AllRecords, [Entry|Acc]);
    {type, _, constraint, [{atom,_,Name}, List]} ->
      N = length(List),
      throw({error,
	     io_lib:format("Unsupported type guard ~w/~w\n", [Name, N])})
  end.

constraints_fixpoint(Constrs, RecDict, ExpTypes, AllRecords) ->
  VarDict =
    constraints_to_dict(Constrs, RecDict, ExpTypes, AllRecords, dict:new()),
  constraints_fixpoint(VarDict, Constrs, RecDict, ExpTypes, AllRecords).

constraints_fixpoint(OldVarDict, Constrs, RecDict, ExpTypes, AllRecords) ->
  NewVarDict =
    constraints_to_dict(Constrs, RecDict, ExpTypes, AllRecords, OldVarDict),
  case NewVarDict of
    OldVarDict ->
      DictFold =
	fun(Key, Value, Acc) ->
	    [{subtype, erl_types:t_var(Key), Value}|Acc]
	end,
      FinalConstrs = dict:fold(DictFold, [], NewVarDict),
      {FinalConstrs, NewVarDict};
    _Other ->
      constraints_fixpoint(NewVarDict, Constrs, RecDict, ExpTypes, AllRecords)
  end.

-define(TYPE_LIMIT, 4).

final_form(Form, RecDict, ExpTypes, AllRecords, VarDict) ->
  T1 = erl_types:t_from_form(Form, RecDict, VarDict),
  T2 = erl_types:t_solve_remote(T1, ExpTypes, AllRecords),
  erl_types:t_limit(T2, ?TYPE_LIMIT).

constraints_to_dict(Constrs, RecDict, ExpTypes, AllRecords, VarDict) ->
  Subtypes =
    constraints_to_subs(Constrs, RecDict, ExpTypes, AllRecords, VarDict, []),
  insert_constraints(Subtypes, dict:new()).

constraints_to_subs([], _RecDict, _ExpTypes, _AllRecords, _VarDict, Acc) ->
  Acc;
constraints_to_subs([C|Rest], RecDict, ExpTypes, AllRecords, VarDict, Acc) ->
  {T1, Form2} = C,
  T2 = final_form(Form2, RecDict, ExpTypes, AllRecords, VarDict),
  NewAcc = [{subtype, T1, T2}|Acc],
  constraints_to_subs(Rest, RecDict, ExpTypes, AllRecords, VarDict, NewAcc).

%% Gets the most general domain of a list of domains of all
%% the overloaded contracts

general_domain(List) ->
  general_domain(List, erl_types:t_none()).

general_domain([{Sig, Constraints}|Left], AccSig) ->
  Dict = insert_constraints(Constraints, dict:new()),
  Sig1 = erl_types:t_subst(Sig, Dict),
  general_domain(Left, erl_types:t_sup(AccSig, Sig1));
general_domain([], AccSig) ->
  %% Get rid of all variables in the domain.
  AccSig1 = erl_types:subst_all_vars_to_any(AccSig),
  erl_types:t_fun_args(AccSig1).

-spec get_invalid_contract_warnings([module()], dialyzer_codeserver:codeserver(), dialyzer_plt:plt()) -> [dial_warning()].

get_invalid_contract_warnings(Modules, CodeServer, Plt) ->
  get_invalid_contract_warnings_modules(Modules, CodeServer, Plt, []).

get_invalid_contract_warnings_modules([Mod|Mods], CodeServer, Plt, Acc) ->
  Contracts1 = dialyzer_codeserver:lookup_mod_contracts(Mod, CodeServer),
  Contracts2 = dict:to_list(Contracts1),
  Records = dialyzer_codeserver:lookup_mod_records(Mod, CodeServer),
  NewAcc = get_invalid_contract_warnings_funs(Contracts2, Plt, Records, Acc),
  get_invalid_contract_warnings_modules(Mods, CodeServer, Plt, NewAcc);
get_invalid_contract_warnings_modules([], _CodeServer, _Plt, Acc) ->
  Acc.

get_invalid_contract_warnings_funs([{MFA, {FileLine, Contract}}|Left],
				   Plt, RecDict, Acc) ->
  case dialyzer_plt:lookup(Plt, MFA) of
    none ->
      %% This must be a contract for a non-available function. Just accept it.
      get_invalid_contract_warnings_funs(Left, Plt, RecDict, Acc);
    {value, {Ret, Args}} ->
      Sig = erl_types:t_fun(Args, Ret),
      NewAcc =
	case check_contract(Contract, Sig) of
	  {error, invalid_contract} ->
	    [invalid_contract_warning(MFA, FileLine, Sig, RecDict)|Acc];
	  {error, {overlapping_contract, []}} ->
	    [overlapping_contract_warning(MFA, FileLine)|Acc];
	  {error, {extra_range, ExtraRanges, STRange}} ->
	    Warn =
	      case t_from_forms_without_remote(Contract#contract.forms,
					       RecDict) of
		{ok, NoRemoteType} ->
		  CRet = erl_types:t_fun_range(NoRemoteType),
		  erl_types:t_is_subtype(ExtraRanges, CRet);
		unsupported ->
		  true
	      end,
	    case Warn of
	      true ->
		[extra_range_warning(MFA, FileLine, ExtraRanges, STRange)|Acc];
	      false ->
		Acc
	    end;
	  {error, Msg} ->
	    [{?WARN_CONTRACT_SYNTAX, FileLine, Msg}|Acc];
	  ok ->
	    {M, F, A} = MFA,
	    CSig0 = get_contract_signature(Contract),
	    CSig = erl_types:subst_all_vars_to_any(CSig0),
	    case erl_bif_types:is_known(M, F, A) of
	      true ->
		%% This is strictly for contracts of functions also in
		%% erl_bif_types
		BifArgs = erl_bif_types:arg_types(M, F, A),
		BifRet = erl_bif_types:type(M, F, A),
		BifSig = erl_types:t_fun(BifArgs, BifRet),
		case check_contract(Contract, BifSig) of
		  {error, _} ->
		    [invalid_contract_warning(MFA, FileLine, BifSig, RecDict)
		     |Acc];
		  ok ->
		    picky_contract_check(CSig, BifSig, MFA, FileLine,
					 Contract, RecDict, Acc)
		end;
	      false ->
		picky_contract_check(CSig, Sig, MFA, FileLine, Contract,
				     RecDict, Acc)
	    end
	end,
      get_invalid_contract_warnings_funs(Left, Plt, RecDict, NewAcc)
  end;
get_invalid_contract_warnings_funs([], _Plt, _RecDict, Acc) ->
  Acc.

invalid_contract_warning({M, F, A}, FileLine, SuccType, RecDict) ->
  SuccTypeStr = dialyzer_utils:format_sig(SuccType, RecDict),
  {?WARN_CONTRACT_TYPES, FileLine, {invalid_contract, [M, F, A, SuccTypeStr]}}.

overlapping_contract_warning({M, F, A}, FileLine) ->
  {?WARN_CONTRACT_TYPES, FileLine, {overlapping_contract, [M, F, A]}}.

extra_range_warning({M, F, A}, FileLine, ExtraRanges, STRange) ->
  ERangesStr = erl_types:t_to_string(ExtraRanges),
  STRangeStr = erl_types:t_to_string(STRange),
  {?WARN_CONTRACT_SUPERTYPE, FileLine,
   {extra_range, [M, F, A, ERangesStr, STRangeStr]}}.

picky_contract_check(CSig0, Sig0, MFA, FileLine, Contract, RecDict, Acc) ->
  CSig = erl_types:t_abstract_records(CSig0, RecDict),
  Sig = erl_types:t_abstract_records(Sig0, RecDict),
  case erl_types:t_is_equal(CSig, Sig) of
    true -> Acc;
    false ->
      case (erl_types:t_is_none(erl_types:t_fun_range(Sig)) andalso
	    erl_types:t_is_unit(erl_types:t_fun_range(CSig))) of
	true -> Acc;
	false ->
	  case extra_contract_warning(MFA, FileLine, Contract,
				      CSig, Sig, RecDict) of
	    no_warning -> Acc;
	    {warning, Warning} -> [Warning|Acc]
	  end
      end
  end.

extra_contract_warning({M, F, A}, FileLine, Contract, CSig, Sig, RecDict) ->
  SigString = lists:flatten(dialyzer_utils:format_sig(Sig, RecDict)),
  ContractString0 = lists:flatten(dialyzer_utils:format_sig(CSig, RecDict)),
  %% The only difference is in record fields containing 'undefined' or not.
  IsUndefRecordFieldsRelated = SigString =:= ContractString0,
  {IsRemoteTypesRelated, SubtypeRelation} =
    is_remote_types_related(Contract, CSig, Sig, RecDict),
  case IsUndefRecordFieldsRelated orelse IsRemoteTypesRelated of
    true ->
      no_warning;
    false ->
      ContractString = contract_to_string(Contract),
      {Tag, Msg} =
	case SubtypeRelation of
	  contract_is_subtype ->
	    {?WARN_CONTRACT_SUBTYPE,
	     {contract_subtype, [M, F, A, ContractString, SigString]}};
	  contract_is_supertype ->
	    {?WARN_CONTRACT_SUPERTYPE,
	     {contract_supertype, [M, F, A, ContractString, SigString]}};
	  neither ->
	    {?WARN_CONTRACT_NOT_EQUAL,
	     {contract_diff, [M, F, A, ContractString, SigString]}}
	end,
      {warning, {Tag, FileLine, Msg}}
  end.

is_remote_types_related(Contract, CSig, Sig, RecDict) ->
  case erl_types:t_is_subtype(CSig, Sig) of
    true ->
      {false, contract_is_subtype};
    false ->
      case erl_types:t_is_subtype(Sig, CSig) of
	true ->
	  case t_from_forms_without_remote(Contract#contract.forms, RecDict) of
	    {ok, NoRemoteTypeSig} ->
	      case blame_remote(CSig, NoRemoteTypeSig, Sig) of
		true ->
		  {true, neither};
		false ->
		  {false, contract_is_supertype}
	      end;
	    unsupported ->
	      {false, contract_is_supertype}
	  end;
	false ->
	  {false, neither}
      end
  end.

t_from_forms_without_remote([{FType, []}], RecDict) ->
  Type0 = erl_types:t_from_form(FType, RecDict),
  Map =
    fun(Type) ->
	case erl_types:t_is_remote(Type) of
	  true -> erl_types:t_none();
	  false -> Type
	end
    end,
  {ok, erl_types:t_map(Map, Type0)};
t_from_forms_without_remote([{_FType, _Constrs}], _RecDict) ->
  %% 'When' constraints
  unsupported;
t_from_forms_without_remote(_Forms, _RecDict) ->
  %% Lots of forms
  unsupported.

blame_remote(ContractSig, NoRemoteContractSig, Sig) ->
  CArgs  = erl_types:t_fun_args(ContractSig),
  CRange = erl_types:t_fun_range(ContractSig),
  NRArgs = erl_types:t_fun_args(NoRemoteContractSig),
  NRRange = erl_types:t_fun_range(NoRemoteContractSig),
  SArgs = erl_types:t_fun_args(Sig),
  SRange = erl_types:t_fun_range(Sig),
  blame_remote_list([CRange|CArgs], [NRRange|NRArgs], [SRange|SArgs]).

blame_remote_list([], [], []) ->
  true;
blame_remote_list([CArg|CArgs], [NRArg|NRArgs], [SArg|SArgs]) ->
  case erl_types:t_is_equal(CArg, NRArg) of
    true ->
      case not erl_types:t_is_equal(CArg, SArg) of
	true  -> false;
	false -> blame_remote_list(CArgs, NRArgs, SArgs)
      end;
    false ->
      case erl_types:t_is_subtype(SArg, NRArg)
	andalso not erl_types:t_is_subtype(NRArg, SArg) of
	true  -> false;
	false -> blame_remote_list(CArgs, NRArgs, SArgs)
      end
  end.
