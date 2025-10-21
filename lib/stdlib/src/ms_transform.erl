%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2002-2025. All Rights Reserved.
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
-module(ms_transform).
-moduledoc({file, "../doc/src/ms_transform.md"}).

-compile(nowarn_deprecated_catch).

-export([format_error/1,transform_from_shell/3,
         parse_transform/2,parse_transform_info/0]).

%% Error codes.
-define(ERROR_BASE_GUARD,0).
-define(ERROR_BASE_BODY,100).
-define(ERR_NOFUN,1).
-define(ERR_ETS_HEAD,2).
-define(ERR_DBG_HEAD,3).
-define(ERR_HEADMATCH,4).
-define(ERR_SEMI_GUARD,5).
-define(ERR_UNBOUND_VARIABLE,6).
-define(ERR_HEADBADREC,7).
-define(ERR_HEADBADFIELD,8).
-define(ERR_HEADMULTIFIELD,9).
-define(ERR_HEADDOLLARATOM,10).
-define(ERR_HEADBINMATCH,11).
-define(ERR_GENMATCH,16).
-define(ERR_GENLOCALCALL,17).
-define(ERR_GENELEMENT,18).
-define(ERR_GENBADFIELD,19).
-define(ERR_GENBADREC,20).
-define(ERR_GENMULTIFIELD,21).
-define(ERR_GENREMOTECALL,22).
-define(ERR_GENBINCONSTRUCT,23).
-define(ERR_GENDISALLOWEDOP,24).
-define(WARN_SHADOW_VAR,50).
-define(ERR_GUARDMATCH,?ERR_GENMATCH+?ERROR_BASE_GUARD).
-define(ERR_BODYMATCH,?ERR_GENMATCH+?ERROR_BASE_BODY).
-define(ERR_GUARDLOCALCALL,?ERR_GENLOCALCALL+?ERROR_BASE_GUARD).
-define(ERR_BODYLOCALCALL,?ERR_GENLOCALCALL+?ERROR_BASE_BODY).
-define(ERR_GUARDELEMENT,?ERR_GENELEMENT+?ERROR_BASE_GUARD).
-define(ERR_BODYELEMENT,?ERR_GENELEMENT+?ERROR_BASE_BODY).
-define(ERR_GUARDBADFIELD,?ERR_GENBADFIELD+?ERROR_BASE_GUARD).
-define(ERR_BODYBADFIELD,?ERR_GENBADFIELD+?ERROR_BASE_BODY).
-define(ERR_GUARDBADREC,?ERR_GENBADREC+?ERROR_BASE_GUARD).
-define(ERR_BODYBADREC,?ERR_GENBADREC+?ERROR_BASE_BODY).
-define(ERR_GUARDMULTIFIELD,?ERR_GENMULTIFIELD+?ERROR_BASE_GUARD).
-define(ERR_BODYMULTIFIELD,?ERR_GENMULTIFIELD+?ERROR_BASE_BODY).
-define(ERR_GUARDREMOTECALL,?ERR_GENREMOTECALL+?ERROR_BASE_GUARD).
-define(ERR_BODYREMOTECALL,?ERR_GENREMOTECALL+?ERROR_BASE_BODY).
-define(ERR_GUARDBINCONSTRUCT,?ERR_GENBINCONSTRUCT+?ERROR_BASE_GUARD).
-define(ERR_BODYBINCONSTRUCT,?ERR_GENBINCONSTRUCT+?ERROR_BASE_BODY).
-define(ERR_GUARDDISALLOWEDOP,?ERR_GENDISALLOWEDOP+?ERROR_BASE_GUARD).
-define(ERR_BODYDISALLOWEDOP,?ERR_GENDISALLOWEDOP+?ERROR_BASE_BODY).

%%
%% Called by compiler or ets/dbg:fun2ms when errors/warnings occur
%%

-doc """
Takes an error code returned by one of the other functions in the module and
creates a textual description of the error.
""".
-spec(format_error(Error) -> Chars when
      Error :: {error, module(), term()},
      Chars :: io_lib:chars()).

format_error({?WARN_SHADOW_VAR,Name}) ->
    lists:flatten(
      io_lib:format("variable ~p shadowed in ms_transform fun head",
		    [Name]));

format_error(?ERR_NOFUN) ->	    
    "Parameter of ets/dbg:fun2ms/1 is not a literal fun";
format_error(?ERR_ETS_HEAD) ->	    
    "ets:fun2ms requires fun with single variable or tuple parameter";
format_error(?ERR_DBG_HEAD) ->	    
    "dbg:fun2ms requires fun with single variable or list parameter";
format_error(?ERR_HEADMATCH) ->	    
    "in fun head, only matching (=) on toplevel can be translated into match_spec";
format_error(?ERR_SEMI_GUARD) ->	    
    "fun with semicolon (;) in guard cannot be translated into match_spec";
format_error(?ERR_GUARDMATCH) ->	    
    "fun with guard matching ('=' in guard) is illegal as match_spec as well";
format_error({?ERR_GUARDLOCALCALL, Name, Arithy}) ->	    
    lists:flatten(io_lib:format("fun containing the local function call "
				"'~tw/~w' (called in guard) "
				"cannot be translated into match_spec",
				[Name, Arithy]));
format_error({?ERR_GUARDREMOTECALL, Module, Name, Arithy}) ->	    
    lists:flatten(io_lib:format("fun containing the remote function call "
				"'~w:~tw/~w' (called in guard) "
				"cannot be translated into match_spec",
				[Module,Name,Arithy]));
format_error({?ERR_GUARDELEMENT, Str}) ->
    lists:flatten(
      io_lib:format("the language element ~ts (in guard) cannot be translated "
		    "into match_spec", [Str]));
format_error({?ERR_GUARDBINCONSTRUCT, Var}) ->
    lists:flatten(
      io_lib:format("bit syntax construction with variable ~w (in guard) "
		    "cannot be translated "
		    "into match_spec", [Var]));
format_error({?ERR_GUARDDISALLOWEDOP, Operator}) ->
    %% There is presently no operators that are allowed in bodies but
    %% not in guards.
    lists:flatten(
      io_lib:format("the operator ~w is not allowed in guards", [Operator]));
format_error(?ERR_BODYMATCH) ->	    
    "fun with body matching ('=' in body) is illegal as match_spec";
format_error({?ERR_BODYLOCALCALL, Name, Arithy}) ->	    
    lists:flatten(io_lib:format("fun containing the local function "
				"call '~tw/~w' (called in body) "
				"cannot be translated into match_spec",
				[Name,Arithy]));
format_error({?ERR_BODYREMOTECALL, Module, Name, Arithy}) ->	    
    lists:flatten(io_lib:format("fun containing the remote function call "
				"'~w:~tw/~w' (called in body) "
				"cannot be translated into match_spec",
				[Module,Name,Arithy]));
format_error({?ERR_BODYELEMENT, Str}) ->
    lists:flatten(
      io_lib:format("the language element ~ts (in body) cannot be translated "
		    "into match_spec", [Str]));
format_error({?ERR_BODYBINCONSTRUCT, Var}) ->
    lists:flatten(
      io_lib:format("bit syntax construction with variable ~w (in body) "
		    "cannot be translated "
		    "into match_spec", [Var]));
format_error({?ERR_BODYDISALLOWEDOP, Operator}) -> 
    %% This will probably never happen, Are there op's that are allowed in 
    %% guards but not in bodies? Not at time of writing anyway...
    lists:flatten(
      io_lib:format("the operator ~w is not allowed in function bodies", 
		    [Operator]));

format_error({?ERR_UNBOUND_VARIABLE, Str}) ->
    lists:flatten(
      io_lib:format("the variable ~s is unbound, cannot translate "
		    "into match_spec", [Str]));
format_error({?ERR_HEADBADREC,Name}) ->	    
    lists:flatten(
      io_lib:format("fun head contains unknown record type ~tw",[Name]));
format_error({?ERR_HEADBADFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun head contains reference to unknown field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error({?ERR_HEADMULTIFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun head contains already defined field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error({?ERR_HEADDOLLARATOM,Atom}) ->	    
    lists:flatten(
      io_lib:format("fun head contains atom ~w, which conflics with reserved "
		    "atoms in match_spec heads",[Atom]));
format_error({?ERR_HEADBINMATCH,Atom}) ->	    
    lists:flatten(
      io_lib:format("fun head contains bit syntax matching of variable ~w, "
		    "which cannot be translated into match_spec", [Atom]));
format_error({?ERR_GUARDBADREC,Name}) ->	    
    lists:flatten(
      io_lib:format("fun guard contains unknown record type ~tw",[Name]));
format_error({?ERR_GUARDBADFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun guard contains reference to unknown field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error({?ERR_GUARDMULTIFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun guard contains already defined field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error({?ERR_BODYBADREC,Name}) ->	    
    lists:flatten(
      io_lib:format("fun body contains unknown record type ~tw",[Name]));
format_error({?ERR_BODYBADFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun body contains reference to unknown field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error({?ERR_BODYMULTIFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun body contains already defined field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error(Else) ->
    lists:flatten(io_lib:format("Unknown error code ~tw",[Else])).

-doc false.
-spec parse_transform_info() -> #{'error_location' => 'column'}.

parse_transform_info() ->
    #{error_location => column}.

%%
%% Called when translating in shell
%%

-doc """
Implements the transformation when the `fun2ms/1` functions are called from the
shell. In this case, the abstract form is for one single fun (parsed by the
Erlang shell). All imported variables are to be in the key-value list passed as
`BoundEnvironment`. The result is a term, normalized, that is, not in abstract
format.
""".
-spec transform_from_shell(Dialect, Clauses, BoundEnvironment) -> term() when
      Dialect :: ets | dbg,
      Clauses :: [erl_parse:abstract_clause()],
      BoundEnvironment :: erl_eval:binding_struct().

transform_from_shell(Dialect, Clauses, BoundEnvironment) ->
    SaveFilename = setup_filename(),
    case catch ms_clause_list(1,Clauses,Dialect,gb_sets:new()) of
	{'EXIT',Reason} ->
	    cleanup_filename(SaveFilename),
	    exit(Reason);
	{error,AnnoOrUnknown,R} ->
	    {error, [{cleanup_filename(SaveFilename),
		      [{location(AnnoOrUnknown), ?MODULE, R}]}], []};
	Else ->
            case (catch fixup_environment(Else,BoundEnvironment)) of
                {error,AnnoOrUnknown1,R1} ->
                    {error, [{cleanup_filename(SaveFilename),
                             [{location(AnnoOrUnknown1), ?MODULE, R1}]}], []};
                Else1 ->
		    Ret = normalise(Else1),
                    cleanup_filename(SaveFilename),
		    Ret
            end
    end.
    

%%
%% Called when translating during compiling
%%

-doc """
Implements the transformation at compile time. This function is called by the
compiler to do the source code transformation if and when header file
`ms_transform.hrl` is included in the source code.

For information about how to use this parse transformation, see `m:ets` and
`dbg:fun2ms/1`.

For a description of match specifications, see section
[Match Specification in Erlang](`e:erts:match_spec.md`) in ERTS User's Guide.
""".
-spec parse_transform(Forms, Options) -> Forms2 | Errors | Warnings when
      Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
      Forms2 :: [erl_parse:abstract_form() | erl_parse:form_info()],
      Options :: term(),
      Errors :: {error, ErrInfo :: [tuple()], WarnInfo :: []},
      Warnings :: {warning, Forms2, WarnInfo :: [tuple()]}.

parse_transform(Forms, _Options) ->
    SaveFilename = setup_filename(),
    %io:format("Forms: ~p~n",[Forms]),
    case catch forms(Forms) of
	{'EXIT',Reason} ->
	    cleanup_filename(SaveFilename),
	    exit(Reason);
	{error,AnnoOrUnknown,R} ->
	    {error, [{cleanup_filename(SaveFilename),
		      [{location(AnnoOrUnknown), ?MODULE, R}]}], []};
	Else ->
	    %io:format("Transformed into: ~p~n",[Else]),
	    case get_warnings() of
		[] ->
		    cleanup_filename(SaveFilename),
		    Else;
		WL ->
		    FName = cleanup_filename(SaveFilename) ,
		    WList = [ {FName, [{L, ?MODULE, R}]} || {L,R} <- WL ],
		    {warning, Else, WList}
	    end
    end.

location(unknown) ->
    none;
location(Anno) ->
    erl_anno:location(Anno).

get_warnings() ->
    case get(warnings) of
	undefined ->
	    [];
	Else ->
	    Else
    end.

add_warning(Location,R) ->
    put(warnings,[{Location,R}| get_warnings()]).

setup_filename() ->
    {erase(filename),erase(records),erase(warnings)}.

put_filename(Name) ->
    put(filename,Name).

put_records(R) ->
    put(records,R),
    ok.

get_records() ->
    case get(records) of
	undefined ->
	    [];
	Else ->
	    Else
    end.

get_record(RName) ->
    case lists:keyfind(RName, 1, get_records()) of
        {RName, FieldList} ->
            put(records_replaced_by_tuples,
                [RName|get(records_replaced_by_tuples)]),
            FieldList;
        false ->
            not_found
    end.

cleanup_filename({Old,OldRec,OldWarnings}) ->
    Ret = case erase(filename) of
	      undefined ->
		  "TOP_LEVEL";
	      X ->
		  X
	  end,
    case OldRec of
	undefined ->
	    erase(records);
	Rec ->
	    put(records,Rec)
    end,
    case OldWarnings of
	undefined ->
	    erase(warnings);
	Warn ->
	    put(warnings,Warn)
    end,
    case Old of
	undefined ->
	    Ret;
	Y ->
	    put(filename,Y),
	    Ret
    end.

add_record_definition({Name,FieldList}) ->
    {KeyList,_} = lists:foldl(
                    fun(F, {L,C}) -> {[record_field(F, C)|L],C+1} end,
		    {[],2},
		    FieldList),
    put_records([{Name,KeyList}|get_records()]).

record_field({record_field,_,{atom,Anno0,FieldName}}, C) ->
    {FieldName,C,{atom,Anno0,undefined}};
record_field({record_field,_,{atom,_,FieldName},Def}, C) ->
    {FieldName,C,Def};
record_field({typed_record_field,Field,_Type}, C) ->
    record_field(Field, C).

forms(Forms0) ->
    put(records_replaced_by_tuples, []),
    try
        Forms = [form(F) || F <- Forms0],
        %% Add `-compile({nowarn_unused_record, RecordNames}).', where
        %% RecordNames is the names of all records replaced by tuples,
        %% in order to silence the code linter's warnings about unused
        %% records.
        case get(records_replaced_by_tuples) of
            [] ->
                Forms;
            RNames ->
                NoWarn = {nowarn_unused_record,[lists:usort(RNames)]},
                [{attribute,erl_anno:new(0),compile,NoWarn}] ++ Forms
        end
    after
        erase(records_replaced_by_tuples)
    end.

form({attribute,_,file,{Filename,_}}=Form) ->
    put_filename(Filename),
    Form;
form({attribute,_,record,Definition}=Form) -> 
    add_record_definition(Definition),
    Form;
form({function,Anno,Name0,Arity0,Clauses0}) ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0),
    {function,Anno,Name,Arity,Clauses};
form(AnyOther) ->
    AnyOther.

function(Name, Arity, Clauses0) ->
    Clauses1 = clauses(Clauses0),
    {Name,Arity,Clauses1}.

clauses([C0|Cs]) ->
    C1 = clause(C0,gb_sets:new()),
    C2 = clauses(Cs),
    [C1|C2];
clauses([]) -> [].

clause({clause,Anno,H0,G0,B0},Bound) ->
    {H1,Bound1} = copy(H0,Bound),
    {B1,_Bound2} = copy(B0,Bound1),
    {clause,Anno,H1,G0,B1}.

copy({call,Anno,{remote,_Anno2,{atom,_Anno3,ets},{atom,_Anno4,fun2ms}},
      As0},Bound) ->
    {transform_call(ets,Anno,As0,Bound),Bound};
copy({call,Anno,{remote,_Anno2,{atom,_Anno3,dbg},{atom,_Anno4,fun2ms}},
      As0},Bound) ->
    {transform_call(dbg,Anno,As0,Bound),Bound};
copy({match,Anno,A,B},Bound) ->
    {B1,Bound1} = copy(B,Bound),
    {A1,Bound2} = copy(A,Bound),
    {{match,Anno,A1,B1},gb_sets:union(Bound1,Bound2)};
copy({var,_Anno,'_'} = VarDef,Bound) ->
    {VarDef,Bound};
copy({var,_Anno,Name} = VarDef,Bound) ->
    Bound1 = gb_sets:add(Name,Bound),
    {VarDef,Bound1};
copy({'fun',Anno,{clauses,Clauses}},Bound) -> % Dont export bindings from funs
    {NewClauses,_IgnoredBindings} = copy_list(Clauses,Bound),
    {{'fun',Anno,{clauses,NewClauses}},Bound};
copy({named_fun,Anno,Name,Clauses},Bound) -> % Dont export bindings from funs
    Bound1 = case Name of
                 '_' -> Bound;
                 Name -> gb_sets:add(Name,Bound)
             end,
    {NewClauses,_IgnoredBindings} = copy_list(Clauses,Bound1),
    {{named_fun,Anno,Name,NewClauses},Bound};
copy({'case',Anno,Of,ClausesList},Bound) -> % Dont export bindings from funs
    {NewOf,NewBind0} = copy(Of,Bound),
    {NewClausesList,NewBindings} = copy_case_clauses(ClausesList,NewBind0,[]),
    {{'case',Anno,NewOf,NewClausesList},NewBindings};
copy(T,Bound) when is_tuple(T) ->
    {L,Bound1} = copy_list(tuple_to_list(T),Bound),
    {list_to_tuple(L),Bound1};
copy(L,Bound) when is_list(L) ->
    copy_list(L,Bound);
copy(AnyOther,Bound) ->
    {AnyOther,Bound}.

copy_case_clauses([],Bound,AddSets) ->
    ReallyAdded = gb_sets:intersection(AddSets),
    {[],gb_sets:union(Bound,ReallyAdded)};
copy_case_clauses([{clause,Anno,Match,Guard,Clauses}|T],Bound,AddSets) ->
    {NewMatch,MatchBinds} = copy(Match,Bound),
    {NewGuard,GuardBinds} = copy(Guard,MatchBinds), %% Really no new binds
    {NewClauses,AllBinds} = copy(Clauses,GuardBinds),
    %% To limit the setsizes, I subtract what I had before the case clause
    %% and add it in the end
    AddedBinds = gb_sets:subtract(AllBinds,Bound),
    {NewTail,ExportedBindings} =
	copy_case_clauses(T,Bound,[AddedBinds | AddSets]),
    {[{clause,Anno,NewMatch,NewGuard,NewClauses}|NewTail],ExportedBindings}.

copy_list([H|T],Bound) ->
    {C1,Bound1} = copy(H,Bound),
    {C2,Bound2} = copy_list(T,Bound1),
    {[C1|C2],Bound2};
copy_list([],Bound) ->
    {[],Bound}.

transform_call(Type,_Anno,[{'fun',Anno2,{clauses, ClauseList}}],Bound) ->
    ms_clause_list(Anno2, ClauseList,Type,Bound);
transform_call(_Type,Anno,_NoAbstractFun,_) ->
    throw({error,Anno,?ERR_NOFUN}).

% Fixup semicolons in guards
ms_clause_expand({clause, Anno, Parameters, Guard = [_,_|_], Body}) ->
    [ {clause, Anno, Parameters, [X], Body} || X <- Guard ];
ms_clause_expand(_Other) ->
    false.

ms_clause_list(Anno,[H|T],Type,Bound) ->
    case ms_clause_expand(H) of
	NewHead when is_list(NewHead) ->
	    ms_clause_list(Anno,NewHead ++ T, Type, Bound);
	false ->
	    {cons, Anno, ms_clause(H, Type, Bound),
	     ms_clause_list(Anno, T, Type, Bound)}
    end;
ms_clause_list(Anno,[],_,_) ->
    {nil,Anno}.
ms_clause({clause, Anno, Parameters, Guards, Body},Type,Bound) ->
    check_type(Anno,Parameters,Type),
    {MSHead,Bindings} = transform_head(Parameters,Bound),
    MSGuards = transform_guards(Anno, Guards, Bindings),
    MSBody = transform_body(Anno,Body,Bindings),
    {tuple, Anno, [MSHead,MSGuards,MSBody]}.


check_type(_,[{var,_,_}],_) ->
    ok;
check_type(_,[{tuple,_,_}],ets) ->
    ok;
check_type(_,[{record,_,_,_}],ets) ->
    ok;
check_type(_,[{cons,_,_,_}],dbg) ->
    ok;
check_type(_,[{nil,_}],dbg) ->
    ok;
check_type(Anno0,[{match,_,{var,_,_},X}],Any) ->
    check_type(Anno0,[X],Any);
check_type(Anno0,[{match,_,X,{var,_,_}}],Any) ->
    check_type(Anno0,[X],Any);
check_type(Anno,_Type,ets) ->
    throw({error,Anno,?ERR_ETS_HEAD});
check_type(Anno,_,dbg) ->
    throw({error,Anno,?ERR_DBG_HEAD}).

-record(tgd,{ b, %Bindings 
	      p, %Part of spec
	      eb %Error code base, 0 for guards, 100 for bodies
	     }).

transform_guards(Anno,[],_Bindings) ->
    {nil,Anno};
transform_guards(Anno,[G],Bindings) ->
    B = #tgd{b = Bindings, p = guard, eb = ?ERROR_BASE_GUARD},
    tg0(Anno,G,B);
transform_guards(Anno,_,_) ->
    throw({error,Anno,?ERR_SEMI_GUARD}).
    
transform_body(Anno,Body,Bindings) ->
    B = #tgd{b = Bindings, p = body, eb = ?ERROR_BASE_BODY},
    tg0(Anno,Body,B).
    

guard_top_trans({call,Anno0,{atom,Anno1,OldTest},Params}) ->
    case old_bool_test(OldTest,length(Params)) of
	undefined ->
	    {call,Anno0,{atom,Anno1,OldTest},Params};
	Trans ->
	    {call,Anno0,{atom,Anno1,Trans},Params}
    end;
guard_top_trans(Else) ->
    Else.

tg0(Anno,[],_) ->
    {nil,Anno};
tg0(Anno,[H0|T],B) when B#tgd.p =:= guard ->
    H = guard_top_trans(H0),
    {cons,Anno, tg(H,B), tg0(Anno,T,B)};
tg0(Anno,[H|T],B) ->
    {cons,Anno, tg(H,B), tg0(Anno,T,B)}.
    

tg({match,Anno,_,_},B) ->
    throw({error,Anno,?ERR_GENMATCH+B#tgd.eb});
tg({op, Anno, Operator, O1, O2}=Expr, B) ->
    case erl_eval:partial_eval(Expr) of
        Expr ->
            {tuple, Anno, [{atom, Anno, Operator}, tg(O1, B), tg(O2, B)]};
        Value ->
            Value
    end;
tg({op, Anno, Operator, O1}=Expr, B) ->
    case erl_eval:partial_eval(Expr) of
        Expr ->
            {tuple, Anno, [{atom, Anno, Operator}, tg(O1, B)]};
        Value ->
            Value
    end;
tg({call, _Anno, {atom, Anno2, bindings},[]},_B) ->
    {atom, Anno2, '$*'};
tg({call, _Anno, {atom, Anno2, object},[]},_B) ->
    {atom, Anno2, '$_'};
tg({call, Anno, {atom, _, is_record}=Call,[Object, {atom,Anno3,RName}=R]},B) ->
    MSObject = tg(Object,B),
    case get_record(RName) of
	FieldList when is_list(FieldList) ->
	    RSize = length(FieldList)+1,
	    {tuple, Anno, [Call, MSObject, R, {integer, Anno3, RSize}]};
	not_found ->
	    throw({error,Anno3,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;
tg({call, Anno, {atom, Anno2, FunName},ParaList},B) ->
    case is_ms_function(FunName,length(ParaList), B#tgd.p) of
	true ->
	    {tuple, Anno, [{atom, Anno2, FunName} |
			   lists:map(fun(X) -> tg(X,B) end, ParaList)]};
	_ ->
	    throw({error,Anno,{?ERR_GENLOCALCALL+B#tgd.eb,
			       FunName,length(ParaList)}}) 
    end;
tg({call, Anno, {remote,_,{atom,_,erlang},{atom, Anno2, FunName}},ParaList},
   B) ->
    L = length(ParaList),
    case is_imported_from_erlang(FunName,L,B#tgd.p) of
	true ->
	    case is_operator(FunName,L,B#tgd.p) of
		false ->
		    tg({call, Anno, {atom, Anno2, FunName},ParaList},B);
		true ->
		    tg(list_to_tuple([op,Anno2,FunName | ParaList]),B)
		end;
	_ ->
	    throw({error,Anno,{?ERR_GENREMOTECALL+B#tgd.eb,erlang,
			       FunName,length(ParaList)}}) 
    end;
tg({call, Anno, {remote,_,{atom,_,ModuleName},
		 {atom, _, FunName}},ParaList},B) ->
    throw({error,Anno,{?ERR_GENREMOTECALL+B#tgd.eb,ModuleName,FunName,length(ParaList)}});
tg({cons,Anno, H, T},B) ->
    {cons, Anno, tg(H,B), tg(T,B)};
tg({nil, Anno},_B) ->
    {nil, Anno};
tg({tuple,Anno,L},B) ->
    {tuple,Anno,[{tuple,Anno,lists:map(fun(X) -> tg(X,B) end, L)}]};
tg({integer,Anno,I},_) ->
    {integer,Anno,I};
tg({char,Anno,C},_) ->
    {char,Anno,C};
tg({float, Anno,F},_) ->
    {float,Anno,F};
tg({atom,Anno,A},_) ->
    case atom_to_list(A) of
	[$$|_] ->
	   {tuple, Anno,[{atom, Anno, 'const'},{atom,Anno,A}]};
	_ ->
	    {atom,Anno,A}
    end;
tg({string,Anno,S},_) ->
    {string,Anno,S};
tg({var,Anno,VarName},B) ->
    case lkup_bind(VarName, B#tgd.b) of
	undefined ->
	    {tuple, Anno,[{atom, Anno, 'const'},{var,Anno,VarName}]};
	AtomName ->
	    {atom, Anno, AtomName}
    end;
tg({record_field,Anno,Object,RName,{atom,_Anno1,KeyName}},B) ->
    case get_record(RName) of
	FieldList when is_list(FieldList) ->
	    case lists:keysearch(KeyName,1, FieldList) of
		{value, {KeyName,Position,_}} ->
		    NewObject = tg(Object,B),
		    {tuple, Anno, [{atom, Anno, 'element'},
				   {integer, Anno, Position}, NewObject]};
		_ ->
		    throw({error,Anno,{?ERR_GENBADFIELD+B#tgd.eb, RName,
				       KeyName}})
	    end;
	not_found ->
	    throw({error,Anno,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;

tg({record,Anno,RName,RFields},B) ->
    KeyList0 = lists:foldl(fun({record_field,_,{atom,_,Key},Value},
				     L) ->
					 NV = tg(Value,B),
					 [{Key,NV}|L];
				    ({record_field,_,{var,_,'_'},Value},
				     L) ->
					 NV = tg(Value,B),
					 [{{default},NV}|L];
				    (_,_) ->
					 throw({error,Anno,
						{?ERR_GENBADREC+B#tgd.eb,
						 RName}})
				 end,
				 [],
				 RFields),
    DefValue = case lists:keysearch({default},1,KeyList0) of
		   {value,{{default},OverriddenDefValue}} ->
		       {true,OverriddenDefValue};
		   _ ->
		       false
	       end,
    KeyList = lists:keydelete({default},1,KeyList0),
    case lists:keysearch({default},1,KeyList) of
	{value,{{default},_}} ->
	    throw({error,Anno,{?ERR_GENMULTIFIELD+B#tgd.eb,RName,'_'}});
	_ ->
	    ok
    end,
    case get_record(RName) of
	FieldList0 when is_list(FieldList0) ->
	    FieldList1 = lists:foldl(
			   fun({FN,_,Def},Acc) ->
				   El = case lists:keysearch(FN,1,KeyList) of
					    {value, {FN, X0}} ->
						X0;
					    _ ->
						case DefValue of 
						    {true,Overridden} ->
							Overridden;
						    false ->
							Def
						end
					end,
				   [El | Acc]
			   end,
			   [],
			   FieldList0),
	    check_multi_field(RName,Anno,KeyList,
				 ?ERR_GENMULTIFIELD+B#tgd.eb),
	    check_undef_field(RName,Anno,KeyList,FieldList0,
			      ?ERR_GENBADFIELD+B#tgd.eb),
	    {tuple,Anno,[{tuple,Anno,[{atom,Anno,RName}|FieldList1]}]};
	not_found ->
	    throw({error,Anno,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;

tg({record_index,Anno,RName,{atom,Anno2,KeyName}},B) ->
    case get_record(RName) of
	FieldList when is_list(FieldList) ->
	    case lists:keysearch(KeyName,1, FieldList) of
		{value, {KeyName,Position,_}} ->
		    {integer, Anno2, Position};
		_ ->
		    throw({error,Anno2,{?ERR_GENBADFIELD+B#tgd.eb, RName,
				       KeyName}})
	    end;
	not_found ->
	    throw({error,Anno,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;

tg({record,Anno,{var,Anno2,_VName}=AVName, RName,RFields},B) ->
    MSVName = tg(AVName,B),
    KeyList = lists:foldl(fun({record_field,_,{atom,_,Key},Value},
				     L) ->
					 NV = tg(Value,B),
					 [{Key,NV}|L];
				    (_,_) ->
					 throw({error,Anno,?ERR_HEADBADREC})
				 end,
				 [],
				 RFields),
    case get_record(RName) of
	FieldList0 when is_list(FieldList0) ->
	    FieldList1 = lists:foldl(
			   fun({FN,Pos,_},Acc) ->
				   El = case lists:keysearch(FN,1,KeyList) of
					    {value, {FN, X0}} ->
						X0;
					    _ ->
						{tuple, Anno2,
						 [{atom, Anno2, element},
						  {integer, Anno2, Pos},
						  MSVName]}
					end,
				   [El | Acc]
			   end,
			   [],
			   FieldList0),
	    check_multi_field(RName,Anno,KeyList,
				 ?ERR_GENMULTIFIELD+B#tgd.eb),
	    check_undef_field(RName,Anno,KeyList,FieldList0,
			      ?ERR_GENBADFIELD+B#tgd.eb),
	    {tuple,Anno,[{tuple,Anno,[{atom,Anno,RName}|FieldList1]}]};
	not_found ->
	    throw({error,Anno,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;

tg({bin_element,_Anno0,{var, Anno, A},_,_} = Whole,B) ->
    case lkup_bind(A, B#tgd.b) of
	undefined ->
	    Whole; % exists in environment hopefully
	_AtomName ->
	    throw({error,Anno,{?ERR_GENBINCONSTRUCT+B#tgd.eb,A}})
    end;    
tg(default,_B) ->
    default;
tg({bin_element,Anno,X,Y,Z},B) ->
    {bin_element, Anno, tg(X,B), tg(Y,B), Z};

tg({bin,Anno,List},B) ->
    {bin,Anno,[tg(X,B) || X <- List]};

tg({map_field_assoc, Anno, Field, Value}, B) ->
    {map_field_assoc, Anno, tg(Field, B), tg(Value, B)};
tg({map, Anno, List}, B) ->
    {map, Anno, [tg(X, B) || X <- List]};

tg(T,B) when is_tuple(T), tuple_size(T) >= 2 ->
    Element = element(1,T),
    Anno = element(2,T),
    throw({error,Anno,{?ERR_GENELEMENT+B#tgd.eb,
		       translate_language_element(Element)}}); 
tg(Other,B) ->
    Element = io_lib:format("unknown element ~tw", [Other]),
    throw({error,erl_anno:new(0),{?ERR_GENELEMENT+B#tgd.eb,Element}}).

transform_head([V],OuterBound) ->
    Bind = cre_bind(),
    {NewV,NewBind} = toplevel_head_match(V,Bind,OuterBound),
    th(NewV,NewBind,OuterBound).


toplevel_head_match({match,_,{var,Anno,VName},Expr},B,OB) ->
    warn_var_clash(Anno,VName,OB),
    {Expr,new_bind({VName,'$_'},B)};
toplevel_head_match({match,_,Expr,{var,Anno,VName}},B,OB) ->
    warn_var_clash(Anno,VName,OB),
    {Expr,new_bind({VName,'$_'},B)};
toplevel_head_match(Other,B,_OB) ->
    {Other,B}.

th({record,Anno,RName,RFields},B,OB) ->
    % youch...
    {KeyList0,NewB} = lists:foldl(fun({record_field,_,{atom,_,Key},Value},
				     {L,B0}) ->
					 {NV,B1} = th(Value,B0,OB),
					 {[{Key,NV}|L],B1};
				    ({record_field,_,{var,_,'_'},Value},
				     {L,B0}) ->
					 {NV,B1} = th(Value,B0,OB),
					 {[{{default},NV}|L],B1};
				    (_,_) ->
					 throw({error,Anno,{?ERR_HEADBADREC,
							    RName}})
				 end,
				 {[],B},
				 RFields),
    DefValue = case lists:keysearch({default},1,KeyList0) of
		   {value,{{default},OverriddenDefValue}} ->
		       OverriddenDefValue;
		   _ ->
		       {atom,Anno,'_'}
	       end,
    KeyList = lists:keydelete({default},1,KeyList0),
    case lists:keysearch({default},1,KeyList) of
	{value,{{default},_}} ->
	    throw({error,Anno,{?ERR_HEADMULTIFIELD,RName,'_'}});
	_ ->
	    ok
    end,
    case get_record(RName) of
	FieldList0 when is_list(FieldList0) ->
	    FieldList1 = lists:foldl(
			   fun({FN,_,_},Acc) ->
				   El = case lists:keysearch(FN,1,KeyList) of
					    {value, {FN, X0}} ->
						X0;
					    _ ->
						DefValue
					end,
				   [El | Acc]
			   end,
			   [],
			   FieldList0),
	    check_multi_field(RName,Anno,KeyList,
				 ?ERR_HEADMULTIFIELD),
	    check_undef_field(RName,Anno,KeyList,FieldList0,
			      ?ERR_HEADBADFIELD),
	    {{tuple,Anno,[{atom,Anno,RName}|FieldList1]},NewB};
	not_found ->
	    throw({error,Anno,{?ERR_HEADBADREC,RName}})
    end;
th({match,Anno,_,_},_,_) ->
    throw({error,Anno,?ERR_HEADMATCH});
th({atom,Anno,A},B,_OB) ->
    case atom_to_list(A) of
	[$$|NL] ->
	    case (catch list_to_integer(NL)) of
		N when is_integer(N) ->
		    throw({error,Anno,{?ERR_HEADDOLLARATOM,A}});
		_ ->
		    {{atom,Anno,A},B}
	    end;
	_ ->
	    {{atom,Anno,A},B}
    end;
th({bin_element,_Anno0,{var, Anno, A},_,_},_,_) ->
    throw({error,Anno,{?ERR_HEADBINMATCH,A}});

th({var,Anno,Name},B,OB) ->
    warn_var_clash(Anno,Name,OB),
    case lkup_bind(Name,B) of
	undefined ->
	    NewB = new_bind(Name,B),
	    {{atom,Anno,lkup_bind(Name,NewB)},NewB};
	Trans ->
	    {{atom,Anno,Trans},B}
    end;
th({map_field_exact,Anno,Field,Value},B,OB) ->
    {[NField, NValue], NB} = th([Field, Value], B, OB),
    {{map_field_assoc,Anno,NField,NValue}, NB};
th([H|T],B,OB) ->
    {NH,NB} = th(H,B,OB),
    {NT,NNB} = th(T,NB,OB),
    {[NH|NT],NNB};
th(T,B,OB) when is_tuple(T) ->
    {L,NB} = th(tuple_to_list(T),B,OB),
    {list_to_tuple(L),NB};
th(Nonstruct,B,_OB) ->
    {Nonstruct,B}.

warn_var_clash(Anno,Name,OuterBound) ->
    case gb_sets:is_member(Name,OuterBound) of
	true ->
            Location = erl_anno:location(Anno),
	    add_warning(Location,{?WARN_SHADOW_VAR,Name});
	_ ->
	    ok
    end.

%% Could be more efficient...
check_multi_field(_, _, [], _) ->
    ok;
check_multi_field(RName, Anno, [{Key,_}|T], ErrCode) ->
    case lists:keymember(Key,1,T) of
	true ->
	    throw({error,Anno,{ErrCode,RName,Key}});
	false ->
	    check_multi_field(RName, Anno, T, ErrCode)
    end.
check_undef_field(_, _, [], _, _) ->
    ok;
check_undef_field(RName, Anno, [{Key,_}|T], FieldList, ErrCode) ->
    case lists:keymember(Key, 1, FieldList) of
	true ->
	    check_undef_field(RName, Anno, T, FieldList, ErrCode);
	false ->
	    throw({error,Anno,{ErrCode,RName,Key}})
    end.

cre_bind() ->
    {1,[{'_','_'}]}.

lkup_bind(Name,{_,List}) ->
    case lists:keysearch(Name,1,List) of
	{value, {Name, Trans}} ->
	    Trans;
	_ ->
	    undefined
    end.

new_bind({Name,Trans},{Next,L}) ->
    {Next,[{Name,Trans}|L]};
new_bind(Name,{Next,L}) ->
    Trans = list_to_atom([$$|integer_to_list(Next)]),
    {Next+1,[{Name,Trans}|L]}.

translate_language_element(Atom) ->
    Transtab = [
		{lc,"list comprehension"},
		{bc,"binary comprehension"},
		{block, "begin/end block"},
		{'if', "if"},
		{'case', "case"},
		{'receive', "receive"},
		{'try', "try"},
		{'catch', "catch"},
		{'match', "match (=)"},
		{remote, "external function call"}
	       ],
    case lists:keysearch(Atom,1,Transtab) of
	{value,{Atom, String}} ->
	    String;
	_ ->
	    atom_to_list(Atom)
    end.

old_bool_test(atom,1) -> is_atom;
old_bool_test(float,1) -> is_float;
old_bool_test(integer,1) -> is_integer;
old_bool_test(list,1) -> is_list;
old_bool_test(number,1) -> is_number;
old_bool_test(pid,1) -> is_pid;
old_bool_test(port,1) -> is_port;
old_bool_test(reference,1) -> is_reference;
old_bool_test(tuple,1) -> is_tuple;
old_bool_test(binary,1) -> is_binary;
old_bool_test(function,1) -> is_function;
old_bool_test(record,2) -> is_record;
old_bool_test(_,_) -> undefined.

bool_test(is_atom,1) -> true;
bool_test(is_float,1) -> true;
bool_test(is_integer,1) -> true;
bool_test(is_list,1) -> true;
bool_test(is_number,1) -> true;
bool_test(is_pid,1) -> true;
bool_test(is_port,1) -> true;
bool_test(is_reference,1) -> true;
bool_test(is_tuple,1) -> true;
bool_test(is_map,1) -> true;
bool_test(is_map_key, 2) -> true;
bool_test(is_binary,1) -> true;
bool_test(is_function,1) -> true;
bool_test(is_record,2) -> true;
bool_test(is_seq_trace,0) -> true;
bool_test(_,_) -> false.

real_guard_function(abs,1) -> true;
real_guard_function(element,2) -> true;
real_guard_function(hd,1) -> true;
real_guard_function(length,1) -> true;
real_guard_function(max,2) -> true;
real_guard_function(min,2) -> true;
real_guard_function(node,0) -> true;
real_guard_function(node,1) -> true;
real_guard_function(round,1) -> true;
real_guard_function(size,1) -> true;
real_guard_function(bit_size,1) -> true;
real_guard_function(byte_size,1) -> true;
real_guard_function(map_size,1) -> true;
real_guard_function(map_get,2) -> true;
real_guard_function(binary_part,2) -> true;
real_guard_function(binary_part,3) -> true;
real_guard_function(tl,1) -> true;
real_guard_function(trunc,1) -> true;
real_guard_function(self,0) -> true;
real_guard_function(float,1) -> true;
real_guard_function(_,_) -> false.

pseudo_guard_function(get_tcw,0) -> true;
pseudo_guard_function(_,_) -> false.

guard_function(X,A) ->
    real_guard_function(X,A) or pseudo_guard_function(X,A).

action_function(set_seq_token,2) -> true;
action_function(get_seq_token,0) -> true;
action_function(message,1) -> true;
action_function(return_trace,0) -> true;
action_function(exception_trace,0) -> true;
action_function(process_dump,0) -> true;
action_function(enable_trace,1) -> true;
action_function(enable_trace,2) -> true;
action_function(disable_trace,1) -> true;
action_function(disable_trace,2) -> true;
action_function(display,1) -> true;
action_function(caller,0) -> true;
action_function(set_tcw,1) -> true;
action_function(silent,1) -> true;
action_function(trace,2) -> true;
action_function(trace,3) -> true;
action_function(caller_line,0) -> true;
action_function(current_stacktrace,0) -> true;
action_function(current_stacktrace,1) -> true;
action_function(_,_) -> false.

bool_operator('and',2) ->
    true;
bool_operator('or',2) ->
    true;
bool_operator('xor',2) ->
    true;
bool_operator('not',1) ->
    true;
bool_operator('andalso',2) ->
    true;
bool_operator('orelse',2) ->
    true;
bool_operator(_,_) ->
    false.

arith_operator('+',1) ->
    true;
arith_operator('+',2) ->
    true;
arith_operator('-',1) ->
    true;
arith_operator('-',2) ->
    true;
arith_operator('*',2) ->
    true;
arith_operator('/',2) ->
    true;
arith_operator('div',2) ->
    true;
arith_operator('rem',2) ->
    true;
arith_operator('band',2) ->
    true;
arith_operator('bor',2) ->
    true;
arith_operator('bxor',2) ->
    true;
arith_operator('bnot',1) ->
    true;
arith_operator('bsl',2) ->
    true;
arith_operator('bsr',2) ->
    true;
arith_operator(_,_) ->
    false.

cmp_operator('>',2) ->
    true;
cmp_operator('>=',2) ->
    true;
cmp_operator('<',2) ->
    true;
cmp_operator('=<',2) ->
    true;
cmp_operator('==',2) ->
    true;
cmp_operator('=:=',2) ->
    true;
cmp_operator('/=',2) -> 
    true;
cmp_operator('=/=',2) ->
    true;
cmp_operator(_,_) ->
    false.

is_operator(X,A,_) ->
    bool_operator(X,A) or arith_operator(X,A) or cmp_operator(X,A).

is_imported_from_erlang(X,A,_) ->
    real_guard_function(X,A) or bool_test(X,A) or bool_operator(X,A) or
    arith_operator(X,A) or cmp_operator(X,A).

is_ms_function(X,A,body) ->
    action_function(X,A) or guard_function(X,A) or bool_test(X,A);

is_ms_function(X,A,guard) ->
    guard_function(X,A) or bool_test(X,A).

fixup_environment(L,B) when is_list(L) ->    
    lists:map(fun(X) ->
		      fixup_environment(X,B) 
	      end,
	      L);
fixup_environment({var,Anno,Name},B) ->
    case lists:keysearch(Name,1,B) of
	{value,{Name,Value}} -> 
	    freeze(Anno,Value);
	_ ->
	    throw({error,Anno,{?ERR_UNBOUND_VARIABLE,atom_to_list(Name)}})
    end;
fixup_environment(T,B) when is_tuple(T) ->
    list_to_tuple(
      lists:map(fun(X) ->
			fixup_environment(X,B) 
		end,
		tuple_to_list(T)));
fixup_environment(Other,_B) ->
    Other.
    
freeze(Anno,Term) ->
    {frozen,Anno,Term}.

%% Most of this is bluntly stolen from erl_parse.

normalise({frozen,_,Term}) ->
    Term;
normalise({char,_,C}) -> C;
normalise({integer,_,I}) -> I;
normalise({float,_,F}) -> F;
normalise({atom,_,A}) -> A;
normalise({string,_,S}) -> S;
normalise({nil,_}) -> [];
normalise({bin,_,Fs}) ->
    {value, B, _} =
	eval_bits:expr_grp(Fs, [],
			   fun(E, _) ->
				   {value, normalise(E), []}
			   end),
    B;
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({op,_,'++',A,B}) ->
    normalise(A) ++ normalise(B);
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
normalise({map,_,Pairs}) ->
    maps:from_list(lists:map(fun
		%% only allow '=>'
		({map_field_assoc,_,K,V}) -> {normalise(K),normalise(V)}
	    end, Pairs));
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;		% Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F.

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].
