%% vim: tabstop=8:shiftwidth=4
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%
-module(asn1ct_check).

%% Main Module for ASN.1 compile time functions

%-compile(export_all).
-export([check/2,storeindb/2,format_error/1]).
%-define(debug,1).
-include("asn1_records.hrl").
%%% The tag-number for universal types
-define(N_BOOLEAN, 1). 
-define(N_INTEGER, 2). 
-define(N_BIT_STRING, 3).
-define(N_OCTET_STRING, 4).
-define(N_NULL, 5). 
-define(N_OBJECT_IDENTIFIER, 6). 
-define(N_OBJECT_DESCRIPTOR, 7). 
-define(N_EXTERNAL, 8). % constructed
-define(N_INSTANCE_OF,8).
-define(N_REAL, 9). 
-define(N_ENUMERATED, 10).   
-define(N_EMBEDDED_PDV, 11). % constructed
-define(N_UTF8String, 12).
-define('N_RELATIVE-OID',13).
-define(N_SEQUENCE, 16). 
-define(N_SET, 17). 
-define(N_NumericString, 18).
-define(N_PrintableString, 19).
-define(N_TeletexString, 20).
-define(N_VideotexString, 21).
-define(N_IA5String, 22).
-define(N_UTCTime, 23). 
-define(N_GeneralizedTime, 24). 
-define(N_GraphicString, 25).
-define(N_VisibleString, 26).
-define(N_GeneralString, 27).
-define(N_UniversalString, 28).
-define(N_CHARACTER_STRING, 29). % constructed
-define(N_BMPString, 30).

-define(TAG_PRIMITIVE(Num),
	#tag{class='UNIVERSAL',number=Num,type='IMPLICIT',form=0}).
-define(TAG_CONSTRUCTED(Num),
	#tag{class='UNIVERSAL',number=Num,type='IMPLICIT',form=32}).

-record(newt,{type=unchanged,tag=unchanged,constraint=unchanged,inlined=no}). % used in check_type to update type and tag
 
check(S,{Types,Values,ParameterizedTypes,Classes,Objects,ObjectSets}) ->
    %%Predicates used to filter errors
    TupleIs = fun({T,_},T) -> true;
		 (_,_) -> false
	      end,
    IsClass = fun(X) -> TupleIs(X,asn1_class) end,
    IsObjSet = fun(X) -> TupleIs(X,objectsetdef) end,
    IsPObjSet = fun(X) -> TupleIs(X,pobjectsetdef) end,
    IsObject = fun(X) -> TupleIs(X,objectdef) end,
    IsValueSet = fun(X) -> TupleIs(X,valueset) end,
    Element2 = fun(X) -> element(2,X) end,
    Element1 = fun(X) -> element(1,X) end,

    %% initialize internal book keeping
    save_asn1db_uptodate(S,S#state.erule,S#state.mname),
    put(top_module,S#state.mname),

    ParamError = checkp(S, ParameterizedTypes), %must do this before the templates are used
    
    %% table to save instances of parameterized objects,object sets
    asn1ct_table:new(parameterized_objects),
    asn1ct_table:new(inlined_objects),


    Terror = checkt(S, Types),
    ?dbg("checkt finished with errors:~n~p~n~n",[Terror]),

    %% get parameterized object sets sent to checkt/3
    %% and update Terror

    {PObjSetNames1,Terror2} = filter_errors(IsPObjSet,Terror),

    Verror = checkv(S, Values ++ ObjectSets), %value sets may be parsed as object sets
    ?dbg("checkv finished with errors:~n~p~n~n",[Verror]),
     %% get information object classes wrongly sent to checkt/3
     %% and update Terror2

    {AddClasses,Terror3} = filter_errors(IsClass,Terror2),

    NewClasses = Classes++AddClasses,

    Cerror = checkc(S, NewClasses),
    ?dbg("checkc finished with errors:~n~p~n~n",[Cerror]),
     %% get object sets incorrectly sent to checkv/3
     %% and update Verror

    {ObjSetNames,Verror2} = filter_errors(IsObjSet,Verror),

     %% get parameterized object sets incorrectly sent to checkv/3
     %% and update Verror2

    {PObjSetNames,Verror3} = filter_errors(IsPObjSet,Verror2),

     %% get objects incorrectly sent to checkv/3
     %% and update Verror3

    {ObjectNames,Verror4} = filter_errors(IsObject,Verror3),

    NewObjects = Objects++ObjectNames,
    NewObjectSets = ObjSetNames ++ PObjSetNames ++ PObjSetNames1,

     %% get value sets
     %% and update Verror4

    {ValueSetNames,Verror5} = filter_errors(IsValueSet,Verror4),

    {Oerror,ExclO,ExclOS} = checko(S,NewObjects ++
				   NewObjectSets,
				   [],[],[]),
    ?dbg("checko finished with errors:~n~p~n~n",[Oerror]),
    InlinedObjTuples = asn1ct_table:to_list(inlined_objects),
    InlinedObjects = lists:map(Element2,InlinedObjTuples),
    asn1ct_table:delete(inlined_objects),
    ParameterizedElems = asn1ct_table:to_list(parameterized_objects),
    ParObjectSets = lists:filter(fun({_OSName,objectset,_}) -> true;
				    (_)-> false end,ParameterizedElems),
    ParObjectSetNames = lists:map(Element1,ParObjectSets),
    ParTypes = lists:filter(fun({_TypeName,type,_}) -> true;
			       (_) -> false end, ParameterizedElems),
    ParTypesNames = lists:map(Element1,ParTypes),
    asn1ct_table:delete(parameterized_objects),
    put(asn1_reference,undefined),

    Exporterror = check_exports(S,S#state.module),
    ImportError = check_imports(S,S#state.module),

    AllErrors = lists:flatten([ParamError,Terror3,Verror5,Cerror,
			       Oerror,Exporterror,ImportError]),
    case AllErrors of
	[] ->
	    ContextSwitchTs = context_switch_in_spec(),
	    InstanceOf = instance_of_in_spec(S#state.mname),
	    NewTypes = lists:subtract(Types,AddClasses) ++ ContextSwitchTs
		++ InstanceOf ++ ParTypesNames,
	    NewValues = lists:subtract(Values,PObjSetNames++ObjectNames++
				       ValueSetNames),
	    {ok,
	     {NewTypes,NewValues,ParameterizedTypes,
	      NewClasses,NewObjects,NewObjectSets},
	     {NewTypes,NewValues,ParameterizedTypes,NewClasses,
	      lists:subtract(NewObjects,ExclO)++InlinedObjects,
	      lists:subtract(NewObjectSets,ExclOS)++ParObjectSetNames}};
	_ ->
	    {error,AllErrors}
    end.

context_switch_in_spec() ->
    L = [{external,'EXTERNAL'},
	 {embedded_pdv,'EMBEDDED PDV'},
	 {character_string,'CHARACTER STRING'}],
    F = fun({T,TName},Acc) ->
		case get(T) of
		    generate -> erase(T),
				[TName|Acc];
		    _ -> Acc
		end
	end,
    lists:foldl(F,[],L).

instance_of_in_spec(ModName) ->
    case get(instance_of) of
	L when is_list(L) ->
	    case lists:member(ModName,L) of
		true ->
		    erase(instance_of),
		    ['INSTANCE OF'];
		_ ->
		    erase(instance_of),
		    []
	    end;
	_ ->
	    []
    end.
instance_of_decl(ModName) ->
    Mods = get_instance_of(),
    case lists:member(ModName,Mods) of
	true ->
	    ok;
	_ ->
	    put(instance_of,[ModName|Mods])
    end.
get_instance_of() ->
    case get(instance_of) of
	undefined ->
	    [];
	L ->
	    L
    end.

put_once(T,State) ->
    %% state is one of undefined, unchecked, generate
    %% undefined > unchecked > generate
    case get(T) of
	PrevS when PrevS > State ->
	    put(T,State);
	_ ->
	    ok
    end.

filter_errors(Pred,ErrorList) ->
    Element2 = fun(X) -> element(2,X) end,
    RemovedTupleElements = lists:filter(Pred,ErrorList),
    RemovedNames = lists:map(Element2,RemovedTupleElements),
    %% remove value set name tuples from Verror
    RestErrors = lists:subtract(ErrorList,RemovedTupleElements),
    {RemovedNames,RestErrors}.

    
check_exports(S,Module = #module{}) ->
    case Module#module.exports of
	{exports,[]} ->
	    [];
	{exports,all} ->
	    [];
	{exports,ExportList} when is_list(ExportList) ->
	    IsNotDefined =
		fun(X) ->
			try
			    _ = get_referenced_type(S,X),
			    false
			catch {error,_} ->
				true
			end
		end,
	    [return_asn1_error(S, Ext, {undefined_export, Undef}) ||
		Ext = #'Externaltypereference'{type=Undef} <- ExportList,
		IsNotDefined(Ext)]
    end.

check_imports(S, #module{imports={imports,Imports}}) ->
    check_imports_1(S, Imports, []).

check_imports_1(_S, [], Acc) ->
    Acc;
check_imports_1(S, [#'SymbolsFromModule'{symbols=Imports,module=ModuleRef}|SFMs], Acc) ->
    Module = name_of_def(ModuleRef),
    Refs = [{try get_referenced_type(S, Ref)
	     catch throw:Error -> Error end,
	     Ref}
	    || Ref <- Imports],
    CreateError = fun(Ref) ->
			  Error = {undefined_import,name_of_def(Ref),Module},
			  return_asn1_error(S, Ref, Error)
		  end,
    Errors = [CreateError(Ref) || {{error, _}, Ref} <- Refs],
    check_imports_1(S, SFMs, Errors ++ Acc).

checkt(S0, Names) ->
    Check = fun do_checkt/3,

    %% NOTE: check_type/3 will store information in the process
    %% dictionary if context switching types are encountered;
    %% therefore we must force the evaluation order.
    Types = check_fold(S0, Names, Check),
    CtxtSwitch = check_contextswitchingtypes(S0, []),
    check_fold(S0, lists:reverse(CtxtSwitch), Check) ++ Types.

do_checkt(S, Name, #typedef{typespec=TypeSpec}=Type0) ->
    NewS = S#state{tname=Name},
    try check_type(NewS, Type0, TypeSpec) of
	#type{}=Ts ->
	    case Type0#typedef.checked of
		true ->			 %already checked and updated
		    ok;
		_ ->
		    Type = Type0#typedef{checked=true,
					 typespec=Ts},
		    asn1_db:dbput(NewS#state.mname,
				  Name, Type),
		    ok
	    end
    catch
	{error,Reason} ->
	    Reason;
	{asn1_class,_ClassDef} ->
	    {asn1_class,Name};
	pobjectsetdef ->
	    {pobjectsetdef,Name};
	pvalueset ->
	    {pvalueset,Name}
    end.

check_contextswitchingtypes(S,Acc) ->
    CSTList=[{external,'EXTERNAL'},
	     {embedded_pdv,'EMBEDDED PDV'},
	     {character_string,'CHARACTER STRING'}],
    check_contextswitchingtypes(S,CSTList,Acc).

check_contextswitchingtypes(S,[{T,TName}|Ts],Acc) ->
     case get(T) of
	unchecked ->
	    put(T,generate),
	    check_contextswitchingtypes(S,Ts,[TName|Acc]);
	_ ->
	    check_contextswitchingtypes(S,Ts,Acc)
     end;
check_contextswitchingtypes(_,[],Acc) ->
    Acc.

checkv(S, Names) ->
    check_fold(S, Names, fun do_checkv/3).

do_checkv(S, Name, Value)
  when is_record(Value, valuedef);
       is_record(Value, typedef); %Value set may be parsed as object set.
       is_record(Value, pvaluedef);
       is_record(Value, pvaluesetdef) ->
    try check_value(S, Value) of
	{valueset,VSet} ->
	    Pos = asn1ct:get_pos_of_def(Value),
	    CheckedVSDef = #typedef{checked=true,pos=Pos,
				    name=Name,typespec=VSet},
	    asn1_db:dbput(S#state.mname, Name, CheckedVSDef),
	    {valueset,Name};
	V ->
	    %% update the valuedef
	    asn1_db:dbput(S#state.mname, Name, V),
	    ok
    catch
	{error,Reason} ->
	    Reason;
	{pobjectsetdef} ->
	    {pobjectsetdef,Name};
	{objectsetdef} ->
	    {objectsetdef,Name};
	{asn1_class, _} ->
	    %% this is an object, save as typedef
	    #valuedef{checked=C,pos=Pos,name=N,type=Type,
		      value=Def} = Value,
	    ClassName = Type#type.def,
	    NewSpec = #'Object'{classname=ClassName,def=Def},
	    NewDef = #typedef{checked=C,pos=Pos,name=N,typespec=NewSpec},
	    asn1_db:dbput(S#state.mname, Name, NewDef),
	    {objectdef,Name}
    end.

%% Check parameterized types.
checkp(S, Names) ->
    check_fold(S, Names, fun do_checkp/3).

do_checkp(S0, Name, #ptypedef{typespec=TypeSpec}=Type0) ->
    S = S0#state{tname=Name},
    try check_ptype(S, Type0, TypeSpec) of
	#type{}=Ts ->
	    Type = Type0#ptypedef{checked=true,typespec=Ts},
	    asn1_db:dbput(S#state.mname, Name, Type),
	    ok
    catch
	{error,Reason} ->
	    Reason;
	{asn1_class,_ClassDef} ->
	    {asn1_class,Name};
	{asn1_param_class,_} ->
	    ok
    end.

%% Check class definitions.
checkc(S, Names) ->
    check_fold(S, Names, fun do_checkc/3).

do_checkc(S, Name, Class) ->
    try
	case is_classname(Name) of
	    false ->
		asn1_error(S, {illegal_class_name,Name});
	    true ->
		do_checkc_1(S, Name, Class)
	end
    catch {error,Reason} -> Reason
    end.

do_checkc_1(S, Name, #classdef{}=Class) ->
    C = check_class(S, Class),
    store_class(S, true, Class#classdef{typespec=C}, Name),
    ok;
do_checkc_1(S, Name, #typedef{typespec=#type{def=Def}=TS}) ->
    C = check_class(S, TS),
    {Mod,Pos} = case Def of
		    #'Externaltypereference'{module=M, pos=P} ->
			{M,P};
		    {pt, #'Externaltypereference'{module=M, pos=P}, _} ->
			{M,P}
		end,
    Class = #classdef{name=Name, typespec=C, pos=Pos, module=Mod},
    store_class(S, true, Class, Name),
    ok.

%% is_classname(Atom) -> true|false.
is_classname(Name) when is_atom(Name) ->
    lists:all(fun($-) -> true;
		 (D) when $0 =< D, D =< $9 -> true;
		 (UC) when $A =< UC, UC =< $Z -> true;
		 (_) -> false
	      end, atom_to_list(Name)).
    
checko(S0,[Name|Os],Acc,ExclO,ExclOS) ->
    Item = asn1_db:dbget(S0#state.mname, Name),
    S = S0#state{error_context=Item},
    try checko_1(S, Item, Name, ExclO, ExclOS) of
	{NewExclO,NewExclOS} ->
	    checko(S, Os, Acc, NewExclO, NewExclOS)
    catch
	throw:{error, Error} ->
	    checko(S, Os, [Error|Acc], ExclO, ExclOS)
    end;
checko(_S,[],Acc,ExclO,ExclOS) ->
    {lists:reverse(Acc),lists:reverse(ExclO),lists:reverse(ExclOS)}.

checko_1(S, #typedef{typespec=TS}=Object, Name, ExclO, ExclOS) ->
    NewS = S#state{tname=Name},
    O = check_object(NewS, Object, TS),
    NewObj = Object#typedef{checked=true,typespec=O},
    asn1_db:dbput(NewS#state.mname, Name, NewObj),
    case O of
	#'Object'{gen=true} ->
	    {ExclO,ExclOS};
	#'Object'{gen=false} ->
	    {[Name|ExclO],ExclOS};
	#'ObjectSet'{gen=true} ->
	    {ExclO,ExclOS};
	#'ObjectSet'{gen=false} ->
	    {ExclO,[Name|ExclOS]}
    end;
checko_1(S, #pobjectdef{}=PObject, Name, ExclO, ExclOS) ->
    NewS = S#state{tname=Name},
    PO = check_pobject(NewS, PObject),
    NewPObj = PObject#pobjectdef{def=PO},
    asn1_db:dbput(NewS#state.mname, Name, NewPObj),
    {[Name|ExclO],ExclOS};
checko_1(S, #pvaluesetdef{}=PObjSet, Name, ExclO, ExclOS) ->
    NewS = S#state{tname=Name},
    POS = check_pobjectset(NewS, PObjSet),
    asn1_db:dbput(NewS#state.mname, Name, POS),
    {ExclO,[Name|ExclOS]}.

check_class(S,CDef=#classdef{checked=Ch,name=Name,typespec=TS}) ->
    case Ch of
	true -> TS;
	idle -> TS;
	_ ->
	    store_class(S,idle,CDef,Name),
	    CheckedTS = check_class(S,TS),
	    store_class(S,true,CDef#classdef{typespec=CheckedTS},Name),
	    CheckedTS
    end;
check_class(S = #state{mname=M,tname=T},ClassSpec) 
  when is_record(ClassSpec,type) ->
    Def = ClassSpec#type.def,
    case Def of
	#'Externaltypereference'{module=M,type=T} ->
	    #objectclass{fields=Def}; % in case of recursive definitions
	Tref = #'Externaltypereference'{type=TName} ->
	    {MName,RefType} = get_referenced_type(S,Tref),
	    #classdef{} = CD = get_class_def(S, RefType),
	    NewState = update_state(S#state{tname=TName}, MName),
	    check_class(NewState, CD);
	{pt,ClassRef,Params} ->
	    %% parameterized class
	    {_,PClassDef} = get_referenced_type(S,ClassRef),
	    NewParaList = match_parameters(S, Params),
	    instantiate_pclass(S,PClassDef,NewParaList)
    end;
check_class(S, #objectclass{}=C) ->
    check_objectclass(S, C);
check_class(S,ClassName) ->
    {RefMod,Def} = get_referenced_type(S,ClassName),
    case Def of
	ClassDef when is_record(ClassDef,classdef) ->
	    case ClassDef#classdef.checked of
		true ->
		    ClassDef#classdef.typespec;
		idle ->
		    ClassDef#classdef.typespec;
		false ->
		    Name=ClassName#'Externaltypereference'.type,
		    store_class(S,idle,ClassDef,Name),
		    NewS = update_state(S#state{tname=Name}, RefMod),
		    CheckedTS = check_class(NewS,ClassDef#classdef.typespec),
		    store_class(S,true,ClassDef#classdef{typespec=CheckedTS},Name),
		    CheckedTS
	    end;
	TypeDef when is_record(TypeDef,typedef) ->
	    %% this case may occur when a definition is a reference 
	    %% to a class definition.
	    case TypeDef#typedef.typespec of
		#type{def=Ext} when is_record(Ext,'Externaltypereference') ->
		    check_class(S,Ext)
	    end
    end.

check_objectclass(S, #objectclass{fields=Fs0,syntax=Syntax0}=C) ->
    Fs = check_class_fields(S, Fs0),
    case Syntax0 of
	{'WITH SYNTAX',Syntax1} ->
	    Syntax = preprocess_syntax(S, Syntax1, Fs),
	    C#objectclass{fields=Fs,syntax={preprocessed_syntax,Syntax}};
	_ ->
	    C#objectclass{fields=Fs}
    end.

instantiate_pclass(S=#state{parameters=_OldArgs},PClassDef,Params) ->
    #ptypedef{args=Args,typespec=Type} = PClassDef,
    MatchedArgs = match_args(S,Args, Params, []),
    NewS = S#state{parameters=MatchedArgs,abscomppath=[]},
    check_class(NewS,#classdef{name=S#state.tname,typespec=Type}).

store_class(S,Mode,ClassDef,ClassName) ->
    NewCDef = ClassDef#classdef{checked=Mode},
    asn1_db:dbput(S#state.mname,ClassName,NewCDef).

check_class_fields(S,Fields) ->
    check_class_fields(S,Fields,[]).

check_class_fields(S,[F|Fields],Acc) ->
    NewField = 
	case element(1,F) of
	    fixedtypevaluefield ->
		{_,Name,Type,Unique,OSpec} = F,
		case {Unique,OSpec} of
		    {'UNIQUE',{'DEFAULT',_}} ->
			asn1_error(S, {unique_and_default,Name});
		    {_,_} ->
			ok
		end,
		RefType = check_type(S,#typedef{typespec=Type},Type),
		{fixedtypevaluefield,Name,RefType,Unique,OSpec};
	    object_or_fixedtypevalue_field ->
		{_,Name,Type,Unique,OSpec} = F,
		Type2 = maybe_unchecked_OCFT(S,Type),
		Cat = 
		    case asn1ct_gen:type(asn1ct_gen:get_inner(Type2#type.def)) of
			Def when is_record(Def,'Externaltypereference') ->
			    {_,D} = get_referenced_type(S, Def, true),
			    D;
			{undefined,user} -> 
			    %% neither of {primitive,bif} or {constructed,bif}
				    
			    {_,D} = get_referenced_type(S,#'Externaltypereference'{module=S#state.mname,type=Type#type.def}),
			    D;
			_ ->
			    Type
		    end,
		case Cat of
		    Class when is_record(Class,classdef) ->
			%% Type must be a referenced type => change it
			%% to an external reference.
			ToExt = fun(#type{def= CE = #'Externaltypereference'{}}) -> CE; (T) -> T end,
			{objectfield,Name,ToExt(Type),Unique,OSpec};
		    _ ->
			RefType = check_type(S,#typedef{typespec=Type},Type),	
			{fixedtypevaluefield,Name,RefType,Unique,OSpec}
		end;
	    objectset_or_fixedtypevalueset_field ->
		{_,Name,Type,OSpec} = F,
		RefType =
		    try check_type(S,#typedef{typespec=Type},Type) of
			#type{} = CheckedType ->
			    CheckedType
		    catch {asn1_class,_ClassDef} ->
			    case if_current_checked_type(S,Type) of
				true -> Type#type.def;
				_ ->    check_class(S,Type)
			    end
		    end,
		if
		    is_record(RefType,'Externaltypereference') ->
			{objectsetfield,Name,Type,OSpec};
		    is_record(RefType,classdef) ->
			{objectsetfield,Name,Type,OSpec};
		    is_record(RefType,objectclass) ->
			{objectsetfield,Name,Type,OSpec};
		    true ->
			{fixedtypevaluesetfield,Name,RefType,OSpec}
		end;
	    typefield ->
		case F of
		    {TF,Name,{'DEFAULT',Type}} ->
			{TF,Name,{'DEFAULT',check_type(S,#typedef{typespec=Type},Type)}};
		    _ -> F
		end;
	    _ -> F
	end,
    check_class_fields(S,Fields,[NewField|Acc]);
check_class_fields(_S,[],Acc) ->
    lists:reverse(Acc).

maybe_unchecked_OCFT(S,Type) ->
    case Type#type.def of
	#'ObjectClassFieldType'{type=undefined} ->
	    check_type(S,#typedef{typespec=Type},Type);
	_ ->
	    Type
    end.

if_current_checked_type(S,#type{def=Def}) ->
    CurrentModule = S#state.mname,
    CurrentCheckedName = S#state.tname,
    MergedModules = S#state.inputmodules,
 %   CurrentCheckedModule = S#state.mname,
    case Def of
	#'Externaltypereference'{module=CurrentModule,
				 type=CurrentCheckedName} ->
	    true;
	#'Externaltypereference'{module=ModuleName,
				 type=CurrentCheckedName} ->
	    case MergedModules of
		undefined ->
		    false;
		_ ->
		    lists:member(ModuleName,MergedModules)
	    end;
	_ ->
	    false
    end.

			  

check_pobject(_S,PObject) when is_record(PObject,pobjectdef) ->
    Def = PObject#pobjectdef.def,
    Def.
    

check_pobjectset(S,PObjSet) ->
    #pvaluesetdef{pos=Pos,name=Name,args=Args,type=Type,
		  valueset=ValueSet}=PObjSet,
    {Mod,Def} = get_referenced_type(S,Type#type.def),
    case Def of
	#classdef{} ->
	    ClassName = #'Externaltypereference'{module=Mod,
						 type=get_datastr_name(Def)},
	    {valueset,Set} = ValueSet,
%	    ObjectSet = #'ObjectSet'{class={objectclassname,ClassName},
	    ObjectSet = #'ObjectSet'{class=ClassName,
				     set=Set},
	    #pobjectsetdef{pos=Pos,name=Name,args=Args,class=Type#type.def,
			   def=ObjectSet};
	_ ->
	    PObjSet
    end.

-record(osi,					%Object set information.
	{st,
	 classref,
	 uniq,
	 ext
	}).

check_object(_S,ObjDef,ObjSpec) when (ObjDef#typedef.checked == true) ->
    ObjSpec;
check_object(S,_ObjDef,#'Object'{classname=ClassRef,def=ObjectDef}) ->
    ?dbg("check_object ~p~n",[ObjectDef]),
    _ = check_externaltypereference(S,ClassRef),
    {ClassDef, NewClassRef} =
	case get_referenced_type(S, ClassRef, true) of
	    {MName,#classdef{checked=false, name=CLName}=ClDef} ->
		Type = ClassRef#'Externaltypereference'.type,
		NewState = update_state(S#state{tname=Type}, MName),
		ObjClass = check_class(NewState, ClDef),
		{ClDef#classdef{checked=true, typespec=ObjClass},
		 #'Externaltypereference'{module=MName, type=CLName}};
	    {MName,#classdef{name=CLName}=ClDef} ->
		{ClDef, #'Externaltypereference'{module=MName, type=CLName}};
	    _ ->
		asn1_error(S, illegal_object)
	end,
    NewObj =
	case ObjectDef of
	    {object,_,_}=Def ->
		NewSettingList = check_objectdefn(S,Def,ClassDef),
		#'Object'{def=NewSettingList};
	    {po,{object,DefObj},ArgsList} ->
		{_,Object} = get_referenced_type(S,DefObj),%DefObj is a 
		%%#'Externalvaluereference' or a #'Externaltypereference'
		%% Maybe this call should be catched and in case of an exception
		%% a not initialized parameterized object should be returned.
		instantiate_po(S,ClassDef,Object,ArgsList);
	    {pv,{simpledefinedvalue,ObjRef},ArgList} ->
		{_,Object} = get_referenced_type(S,ObjRef),
		instantiate_po(S,ClassDef,Object,ArgList);
	    #'Externalvaluereference'{} ->
		{_,Object} = get_referenced_type(S,ObjectDef),
		check_object(S, Object, object_to_check(S, Object));
	    [] -> 
		%% An object with no fields (parsed as a value).
		Def = {object,defaultsyntax,[]},
		NewSettingList = check_objectdefn(S, Def, ClassDef),
		#'Object'{def=NewSettingList};
	    _ ->
		asn1_error(S, illegal_object)
	end,
    Fields = (ClassDef#classdef.typespec)#objectclass.fields,
    Gen = gen_incl(S,NewObj#'Object'.def, Fields),
    NewObj#'Object'{classname=NewClassRef,gen=Gen};
check_object(S, _, #'ObjectSet'{class=ClassRef0,set=Set0}=ObjSet0) ->
    {_,ClassDef} = get_referenced_type(S, ClassRef0),
    ClassRef = check_externaltypereference(S, ClassRef0),
    {UniqueFieldName,UniqueInfo} =
	case get_unique_fieldname(S, ClassDef) of
	    no_unique -> {{unique,undefined},{unique,undefined}};
	    Other -> {element(1,Other),Other}
	end,
    OSI0 = #osi{st=S,classref=ClassRef,uniq=UniqueInfo,ext=false},
    {Set1,OSI1} = if
		      is_list(Set0) ->
			  check_object_set_list(Set0, OSI0);
		      true ->
			  check_object_set(Set0, OSI0)
		  end,
    Ext = case Set1 of
	      [] ->
		  %% FIXME: X420 does not compile unless we force
		  %% empty sets to be extensible. There should be
		  %% a better way.
		  true;
	      [_|_] ->
		  OSI1#osi.ext
	  end,
    Set2 = remove_duplicate_objects(S, Set1),
    Set = case Ext of
	      false -> Set2;
	      true -> Set2 ++ ['EXTENSIONMARK']
	  end,
    ObjSet = ObjSet0#'ObjectSet'{uniquefname=UniqueFieldName,set=Set},
    Gen = gen_incl_set(S, Set, ClassDef),
    ObjSet#'ObjectSet'{class=ClassRef,gen=Gen}.

check_object_set({element_set,Root0,Ext0}, OSI0) ->
    OSI = case Ext0 of
	      none -> OSI0;
	      _ -> OSI0#osi{ext=true}
	  end,
    case {Root0,Ext0} of
	{empty,empty} -> {[],OSI};
	{empty,Ext} -> check_object_set(Ext, OSI);
	{Root,none} -> check_object_set(Root, OSI);
	{Root,empty} -> check_object_set(Root, OSI);
	{Root,Ext} -> check_object_set_list([Root,Ext], OSI)
    end;
check_object_set(#'Externaltypereference'{}=Ref, #osi{st=S}=OSI) ->
    {_,#typedef{typespec=OSdef}=OS} = get_referenced_type(S, Ref),
    ObjectSet = check_object(S, OS, OSdef),
    check_object_set_objset(ObjectSet, OSI);
check_object_set(#'Externalvaluereference'{}=Ref, #osi{st=S}=OSI) ->
    {RefedMod,ObjName,#'Object'{def=Def}} = check_referenced_object(S, Ref),
    ObjList = check_object_set_mk(RefedMod, ObjName, Def, OSI),
    {ObjList,OSI};
check_object_set({'EXCEPT',Incl0,Excl0}, OSI) ->
    {Incl1,_} = check_object_set(Incl0, OSI),
    {Excl1,_} = check_object_set(Excl0, OSI),
    Exclude = sofs:set([N || {N,_} <- Excl1], [name]),
    Incl2 = [{Name,Obj} || {Name,_,_}=Obj <- Incl1],
    Incl3 = sofs:relation(Incl2, [{name,object}]),
    Incl4 = sofs:drestriction(Incl3, Exclude),
    Incl5 = sofs:to_external(Incl4),
    Incl = [Obj || {_,Obj} <- Incl5],
    {Incl,OSI};
check_object_set({object,_,_}=Obj0, OSI) ->
    #osi{st=S,classref=ClassRef} = OSI,
    #'Object'{def=Def} =
	check_object(S, #typedef{typespec=Obj0},
		     #'Object'{classname=ClassRef,def=Obj0}),
    ObjList = check_object_set_mk(Def, OSI),
    {ObjList,OSI};
check_object_set(#'ObjectClassFieldType'{classname=ObjName,
					 fieldname=FieldNames},
		 #osi{st=S}=OSI) ->
    Set = check_ObjectSetFromObjects(S, ObjName, FieldNames),
    check_object_set_objset_list(Set, OSI);
check_object_set({'ObjectSetFromObjects',Obj,FieldNames}, #osi{st=S}=OSI) ->
    ObjName = element(tuple_size(Obj), Obj),
    Set = check_ObjectSetFromObjects(S, ObjName, FieldNames),
    check_object_set_objset_list(Set, OSI);
check_object_set({pt,DefinedObjSet,ParamList0}, OSI) ->
    #osi{st=S,classref=ClassRef} = OSI,
    {_,PObjSetDef} = get_referenced_type(S, DefinedObjSet),
    ParamList = match_parameters(S, ParamList0),
    ObjectSet = instantiate_pos(S, ClassRef, PObjSetDef, ParamList),
    check_object_set_objset(ObjectSet, OSI);
check_object_set({pos,{objectset,_,DefinedObjSet},Params0}, OSI) ->
    #osi{st=S,classref=ClassRef} = OSI,
    {_,PObjSetDef} = get_referenced_type(S, DefinedObjSet),
    Params = match_parameters(S, Params0),
    ObjectSet = instantiate_pos(S, ClassRef, PObjSetDef, Params),
    check_object_set_objset(ObjectSet, OSI);
check_object_set({pv,{simpledefinedvalue,DefinedObject},Params}=PV, OSI) ->
    #osi{st=S,classref=ClassRef} = OSI,
    Args = match_parameters(S, Params),
    #'Object'{def=Def} =
	check_object(S, PV,
		     #'Object'{classname=ClassRef ,
			       def={po,{object,DefinedObject},Args}}),
    ObjList = check_object_set_mk(Def, OSI),
    {ObjList,OSI};
check_object_set({'SingleValue',Val}, OSI) ->
    check_object_set(Val, OSI);
check_object_set({'ValueFromObject',{object,Object},FieldNames}, OSI) ->
    #osi{st=S} = OSI,
    case extract_field(S, Object, FieldNames) of
	#'Object'{def=Def} ->
	    ObjList = check_object_set_mk(Def, OSI),
	    {ObjList,OSI};
	_ ->
	    asn1_error(S, illegal_object)
    end;
check_object_set(#type{def=Def}, OSI) ->
    check_object_set(Def, OSI);
check_object_set({union,A0,B0}, OSI0) ->
    {A,OSI1} = check_object_set(A0, OSI0),
    {B,OSI} = check_object_set(B0, OSI1),
    {A++B,OSI}.

check_object_set_list([H|T], OSI0) ->
    {Set0,OSI1} = check_object_set(H, OSI0),
    {Set1,OSI2} = check_object_set_list(T, OSI1),
    {Set0++Set1,OSI2};
check_object_set_list([], OSI) ->
    {[],OSI}.

check_object_set_objset(#'ObjectSet'{set=Set}, OSI) ->
    check_object_set_objset_list(Set, OSI).

check_object_set_objset_list(Set, OSI) ->
    check_object_set_objset_list_1(Set, OSI, []).

check_object_set_objset_list_1(['EXTENSIONMARK'|T], OSI, Acc) ->
    check_object_set_objset_list_1(T, OSI#osi{ext=true}, Acc);
check_object_set_objset_list_1([H|T], OSI, Acc) ->
    check_object_set_objset_list_1(T, OSI, [H|Acc]);
check_object_set_objset_list_1([], OSI, Acc) ->
    {Acc,OSI}.

check_object_set_mk(Fields, OSI) ->
    check_object_set_mk(no_mod, no_name, Fields, OSI).

check_object_set_mk(M, N, Def, #osi{uniq={unique,undefined}}) ->
    {_,_,Fields} = Def,
    [{{M,N},no_unique_value,Fields}];
check_object_set_mk(M, N, Def, #osi{uniq={UniqField,_}}) ->
    {_,_,Fields} = Def,
    case lists:keyfind(UniqField, 1, Fields) of
	{UniqField,#valuedef{value=Val}} ->
	    [{{M,N},Val,Fields}];
	false ->
	    case Fields of
		[{_,#typedef{typespec=#'ObjectSet'{set=['EXTENSIONMARK']}}}] ->
		    %% FIXME: If object is missing the unique field and
		    %% only contains a reference to an empty object set,
		    %% we will remove the entire object as a workaround
		    %% to get X420 to compile. There should be a better
		    %% way.
		    [];
		_ ->
		    [{{M,N},no_unique_value,Fields}]
	    end
    end.

%% remove_duplicate_objects/1 remove duplicates of objects.
%% For instance may Set contain objects of same class from
%% different object sets that in fact might be duplicates.
remove_duplicate_objects(S, Set0) when is_list(Set0) ->
    Set1 = [{Id,Orig} || {_,Id,_}=Orig <- Set0],
    Set2 = sofs:relation(Set1),
    Set3 = sofs:relation_to_family(Set2),
    Set = sofs:to_external(Set3),
    remove_duplicate_objects_1(S, Set).

remove_duplicate_objects_1(S, [{no_unique_value,Objs}|T]) ->
    Objs ++ remove_duplicate_objects_1(S, T);
remove_duplicate_objects_1(S, [{_,[_]=Objs}|T]) ->
    Objs ++ remove_duplicate_objects_1(S, T);
remove_duplicate_objects_1(S, [{Id,[_|_]=Objs}|T]) ->
    MakeSortable = fun(What) -> sortable_type(S, What) end,
    Tagged = order_tag_set(Objs, MakeSortable),
    case lists:ukeysort(1, Tagged) of
	[{_,Obj}] ->
	    [Obj|remove_duplicate_objects_1(S, T)];
	[_|_] ->
	    asn1_error(S, {non_unique_object,Id})
    end;
remove_duplicate_objects_1(_, []) ->
    [].

order_tag_set([{_, _, Fields}=Orig|Fs], Fun) ->
    Pair = {[{FId, traverse(F, Fun)} || {FId, F} <- Fields], Orig},
    [Pair|order_tag_set(Fs, Fun)];
order_tag_set([], _) -> [].

sortable_type(S, #'Externaltypereference'{}=ERef) ->
    try get_referenced_type(S, ERef) of
	 {_,#typedef{}=OI} ->
	    OI#typedef{pos=undefined,name=undefined}
    catch
	_:_ ->
	    ERef
    end;
sortable_type(_, #typedef{}=TD) ->
    asn1ct:unset_pos_mod(TD#typedef{name=undefined});
sortable_type(_, Type) ->
    asn1ct:unset_pos_mod(Type).

traverse(Structure0, Fun) ->
    Structure = Fun(Structure0),
    traverse_1(Structure, Fun).

traverse_1(#typedef{typespec=TS0} = TD, Fun) ->
    TS = traverse(TS0, Fun),
    TD#typedef{typespec=TS};
traverse_1(#valuedef{type=TS0} = VD, Fun) ->
    TS = traverse(TS0, Fun),
    VD#valuedef{type=TS};
traverse_1(#type{def=TS0} = TD, Fun) ->
    TS = traverse(TS0, Fun),
    TD#type{def=TS};
traverse_1(#'SEQUENCE'{components=Cs0} = Seq, Fun) ->
    Cs = traverse_seq_set(Cs0, Fun),
    Seq#'SEQUENCE'{components=Cs};
traverse_1({'SEQUENCE OF',Type0}, Fun) ->
    Type = traverse(Type0, Fun),
    {'SEQUENCE OF',Type};
traverse_1({'SET OF',Type0}, Fun) ->
    Type = traverse(Type0, Fun),
    {'SET OF',Type};
traverse_1(#'SET'{components=Cs0} = Set, Fun) ->
    Cs = traverse_seq_set(Cs0, Fun),
    Set#'SET'{components=Cs};
traverse_1({'CHOICE', Cs0}, Fun) ->
    Cs = traverse_seq_set(Cs0, Fun),
    {'CHOICE', Cs};
traverse_1(Leaf, _) ->
    Leaf.

traverse_seq_set(List, Fun) when is_list(List) ->
    traverse_seq_set_1(List, Fun);
traverse_seq_set({Set, Ext}, Fun) ->
    {traverse_seq_set_1(Set, Fun), traverse_seq_set_1(Ext, Fun)};
traverse_seq_set({Set1, Set2, Set3}, Fun) ->
    {traverse_seq_set_1(Set1, Fun),
     traverse_seq_set_1(Set2, Fun),
     traverse_seq_set_1(Set3, Fun)}.

traverse_seq_set_1([#'ComponentType'{} = CT0|Cs], Fun) ->
    CT = #'ComponentType'{typespec=TS0} = Fun(CT0),
    TS = traverse(TS0, Fun),
    [CT#'ComponentType'{typespec=TS}|traverse_seq_set_1(Cs, Fun)];
traverse_seq_set_1([{'COMPONENTS OF', _} = CO0|Cs], Fun) ->
    {'COMPONENTS OF', TS0} = Fun(CO0),
    TS = traverse(TS0, Fun),
    [{'COMPONENTS OF', TS}|traverse_seq_set_1(Cs, Fun)];
traverse_seq_set_1([], _) ->
    [].

object_to_check(_, #typedef{typespec=ObjDef}) ->
    ObjDef;
object_to_check(S, #valuedef{type=Class,value=ObjectRef}) ->
    %% If the object definition is parsed as an object the ClassName
    %% is parsed as a type.
    case Class of
	#type{def=#'Externaltypereference'{}=Def} ->
	    #'Object'{classname=Def,def=ObjectRef};
	_ ->
	    asn1_error(S, illegal_object)
    end.

check_referenced_object(S,ObjRef) 
  when is_record(ObjRef,'Externalvaluereference')->
    case get_referenced_type(S,ObjRef) of
	{RefedMod,ObjectDef} when is_record(ObjectDef,valuedef) ->	
	    ?dbg("Externalvaluereference, ObjectDef: ~p~n",[ObjectDef]),
	    #type{def=ClassRef} = ObjectDef#valuedef.type,
	    Def = ObjectDef#valuedef.value,
	    {RefedMod,get_datastr_name(ObjectDef),
	     check_object(update_state(S,RefedMod),ObjectDef,#'Object'{classname=ClassRef,
						def=Def})};
	{RefedMod,ObjectDef} when is_record(ObjectDef,typedef) ->
	    {RefedMod,get_datastr_name(ObjectDef),
	     check_object(update_state(S,RefedMod),ObjectDef,ObjectDef#typedef.typespec)}
    end.

check_ObjectSetFromObjects(S, ObjName, Fields) ->
    {_,Obj0} = get_referenced_type(S, ObjName),
    case check_object(S, Obj0, Obj0#typedef.typespec) of
	#'ObjectSet'{}=Obj1 ->
	    get_fieldname_set(S, Obj1, Fields);
	#'Object'{classname=Class,
		  def={object,_,ObjFs}} ->
	    ObjSet = #'ObjectSet'{class=Class,
				  set=[{'_','_',ObjFs}]},
	    get_fieldname_set(S, ObjSet, Fields)
    end.

%%  get_type_from_object(State, ObjectOrObjectSet, [{RefType,FieldName}]) ->
%%    Type
get_type_from_object(S, Object, FieldNames)
  when is_record(Object, 'Externaltypereference');
       is_record(Object, 'Externalvaluereference') ->
    extract_field(S, Object, FieldNames).

%% get_value_from_object(State, ObjectOrObjectSet, [{RefType,FieldName}]) ->
%%    UntaggedValue
get_value_from_object(S, Def, FieldNames) ->
    case extract_field(S, Def, FieldNames) of
	#valuedef{value=Val} ->
	    Val;
	{valueset,_}=Val ->
	    Val;
	_ ->
	    asn1_error(S, illegal_value)
    end.

%% extract_field(State, ObjectOrObjectSet, [{RefType,FieldName}])
%%   RefType = typefieldreference | valuefieldreference
%%
%%   Get the type, value, object, object set, or value set from the
%%   referenced object or object set. The list of field name tuples
%%   may have more than one element.  All field names but the last
%%   refers to either an object or object set.

extract_field(S, Def0, FieldNames) ->
    {_,Def1} = get_referenced_type(S, Def0),
    Def2 = check_object(S, Def1, Def1#typedef.typespec),
    Def = Def1#typedef{typespec=Def2},
    get_fieldname_element(S, Def, FieldNames).

%% get_fieldname_element(State, Element, [{RefType,FieldName}]
%%   RefType = typefieldreference | valuefieldreference
%%
%%   Get the type, value, object, object set, or value set from the referenced
%%   element. The list of field name tuples may have more than one element.
%%   All field names but the last refers to either an object or object set.

get_fieldname_element(S, Object0, [{_RefType,FieldName}|Fields]) ->
    Object = case Object0 of
		 #typedef{typespec=#'Object'{def=Obj}} -> Obj;
		 {_,_,_}=Obj -> Obj
	     end,
    case check_fieldname_element(S, FieldName, Object) of
	#'Object'{def=D} when Fields =/= [] ->
	    get_fieldname_element(S, D, Fields);
	#'ObjectSet'{}=Set ->
	    get_fieldname_set(S, Set, Fields);
	Result when Fields =:= [] ->
	    Result
    end;
get_fieldname_element(_S, Def, []) ->
    Def.

get_fieldname_set(S, #'ObjectSet'{set=Set0}, T) ->
    get_fieldname_set_1(S, Set0, T, []).

get_fieldname_set_1(S, ['EXTENSIONMARK'=Ext|T], Fields, Acc) ->
    get_fieldname_set_1(S, T, Fields, [Ext|Acc]);
get_fieldname_set_1(S, [H|T], Fields, Acc) ->
    try get_fieldname_element(S, H, Fields) of
	L when is_list(L) ->
	    get_fieldname_set_1(S, T, Fields, L++Acc);
	{valueset,L} ->
	    get_fieldname_set_1(S, T, Fields, L++Acc);
	Other ->
	    get_fieldname_set_1(S, T, Fields, [Other|Acc])
    catch
	throw:{error,_} ->
	    get_fieldname_set_1(S, T, Fields, Acc)
    end;
get_fieldname_set_1(_, [], _Fields, Acc) ->
    case Acc of
	[#valuedef{}|_] ->
	    {valueset,Acc};
	_ ->
	    Acc
    end.

check_fieldname_element(S, Name, {_,_,Fields}) ->
    case lists:keyfind(Name, 1, Fields) of
	{Name,Def} ->
	    check_fieldname_element_1(S, Def);
	false ->
	    asn1_error(S, {undefined_field,Name})
    end.

check_fieldname_element_1(S, #typedef{typespec=Ts}=TDef) ->
    case Ts of
	#'Object'{} ->
	    check_object(S, TDef, Ts);
	_ ->
	    check_type(S, TDef, Ts)
    end;
check_fieldname_element_1(S, #valuedef{}=VDef) ->
    try
	check_value(S, VDef)
    catch
	throw:{asn1_class, _} ->
	    #valuedef{checked=C,pos=Pos,name=N,type=Type,
		      value=Def} = VDef,
	    ClassName = Type#type.def,
	    NewSpec = #'Object'{classname=ClassName,def=Def},
	    NewDef = #typedef{checked=C,pos=Pos,name=N,typespec=NewSpec},
	    check_fieldname_element_1(S, NewDef)
    end;
check_fieldname_element_1(_S, {value_tag,Val}) ->
    #valuedef{value=Val};
check_fieldname_element_1(S, Eref)
  when is_record(Eref, 'Externaltypereference');
       is_record(Eref, 'Externalvaluereference') ->
    {_,TDef} = get_referenced_type(S, Eref),
    check_fieldname_element_1(S, TDef).
    
%% instantiate_po/4
%% ClassDef is the class of Object,
%% Object is the Parameterized object, which is referenced,
%% ArgsList is the list of actual parameters
%% returns an #'Object' record.
instantiate_po(S=#state{parameters=_OldArgs},_ClassDef,Object,ArgsList) when is_record(Object,pobjectdef) ->
    FormalParams = get_pt_args(Object),
    MatchedArgs = match_args(S,FormalParams,ArgsList,[]),
    NewS = S#state{parameters=MatchedArgs},
    check_object(NewS,Object,#'Object'{classname=Object#pobjectdef.class,
				    def=Object#pobjectdef.def}).

%% instantiate_pos/4
%% ClassDef is the class of ObjectSetDef,
%% ObjectSetDef is the Parameterized object set, which is referenced 
%% on the right side of the assignment,
%% ArgsList is the list of actual parameters, i.e. real objects
instantiate_pos(S=#state{parameters=_OldArgs},ClassRef,ObjectSetDef,ArgsList) ->
    FormalParams = get_pt_args(ObjectSetDef),
    OSet = case get_pt_spec(ObjectSetDef) of
	       {valueset,Set} -> #'ObjectSet'{class=ClassRef,set=Set};
	       Set when is_record(Set,'ObjectSet') -> Set;
	       _ -> asn1_error(S, invalid_objectset)
	   end,
    MatchedArgs = match_args(S,FormalParams,ArgsList,[]),
    NewS = S#state{parameters=MatchedArgs},
    check_object(NewS,ObjectSetDef,OSet).

    
%% gen_incl -> boolean()
%% If object with Fields has any of the corresponding class' typefields
%% then return value is true otherwise it is false.
%% If an object lacks a typefield but the class has a type field that
%% is OPTIONAL then we want gen to be true
gen_incl(S,{_,_,Fields},CFields)->
    gen_incl1(S,Fields,CFields).

gen_incl1(_,_,[]) ->
    false;
gen_incl1(S,Fields,[C|CFields]) ->
    case element(1,C) of
	typefield ->
	    true; %% should check that field is OPTIONAL or DEFUALT if
                  %% the object lacks this field
	objectfield ->
	    case lists:keysearch(element(2,C),1,Fields) of
		{value,Field} ->
		    ClassRef = case element(3,C) of
			      #type{def=Ref} -> Ref;
			      Eref when is_record(Eref,'Externaltypereference') ->
				  Eref
			  end,
		    ClassFields = get_objclass_fields(S,ClassRef),
		    ObjDef = 
			case element(2,Field) of
			    TDef when is_record(TDef,typedef) -> 
				check_object(S,TDef,TDef#typedef.typespec);
			    ERef ->
				{_,T} = get_referenced_type(S,ERef),
				check_object(S, T, object_to_check(S, T))
			end,
		    case gen_incl(S,ObjDef#'Object'.def,
				  ClassFields) of
			true ->
			    true;
			_ ->
			    gen_incl1(S,Fields,CFields)
		    end;
		_ ->
		    gen_incl1(S,Fields,CFields)
	    end;
	_ -> 
	    gen_incl1(S,Fields,CFields)
    end.

get_objclass_fields(S,Eref=#'Externaltypereference'{}) ->
    {_,ClassDef} = get_referenced_type(S,Eref, true),
    get_objclass_fields(S,ClassDef);
get_objclass_fields(S,CD=#classdef{typespec=#'Externaltypereference'{}}) ->
    get_objclass_fields(S,CD#classdef.typespec);
get_objclass_fields(_,#classdef{typespec=CDef}) 
  when is_record(CDef,objectclass) ->
    CDef#objectclass.fields.
    

%% first if no unique field in the class return false.(don't generate code)
gen_incl_set(S,Fields,#typedef{typespec=#type{def=Eref}}) 
  when is_record(Eref,'Externaltypereference') ->
    %% When a Defined class is a reference toanother class definition
    {_,CDef} = get_referenced_type(S,Eref),
    gen_incl_set(S,Fields,CDef);
gen_incl_set(S,Fields,ClassDef) ->
    case get_unique_fieldname(S, ClassDef) of
	no_unique ->
	    false;
	{_, _} ->
	    gen_incl_set1(S,Fields,
			  (ClassDef#classdef.typespec)#objectclass.fields)
    end.


%% if any of the existing or potentially existing objects has a typefield
%% then return true.
gen_incl_set1(_,[],_CFields)->
    false;
gen_incl_set1(_,['EXTENSIONMARK'],_) ->
    true;
%% Fields are the fields of an object in the object set.
%% CFields are the fields of the class of the object set.
gen_incl_set1(_,['EXTENSIONMARK'|_],_) ->
    true;
gen_incl_set1(S,[Object|Rest],CFields)->
    Fields = element(tuple_size(Object), Object),
    case gen_incl1(S,Fields,CFields) of
	true ->
	    true;
	false ->
	    gen_incl_set1(S,Rest,CFields)
    end.


%%%
%%% Check an object definition.
%%%

check_objectdefn(S, Def, #classdef{typespec=ObjClass}) ->
    #objectclass{syntax=Syntax0,fields=ClassFields} = ObjClass,
    case Def of
	{object,defaultsyntax,Fields} ->
	    check_defaultfields(S, Fields, ClassFields);
	{object,definedsyntax,Fields} ->
	    Syntax = get_syntax(S, Syntax0, ClassFields),
	    case match_syntax(S, Syntax, Fields, []) of
		{match,NewFields,[]} ->
		    {object,defaultsyntax,NewFields};
		{match,_,[What|_]} ->
		    syntax_match_error(S, What);
		{nomatch,[What|_]} ->
		    syntax_match_error(S, What);
		{nomatch,[]} ->
		    syntax_match_error(S)
	    end
    end.


%%%
%%% Pre-process the simplified syntax so that it can be more
%%% easily matched.
%%%

get_syntax(_, {preprocessed_syntax,Syntax}, _) ->
    Syntax;
get_syntax(S, {'WITH SYNTAX',Syntax}, ClassFields) ->
    preprocess_syntax(S, Syntax, ClassFields).

preprocess_syntax(S, Syntax0, Cs) ->
    Syntax = preprocess_syntax_1(S, Syntax0, Cs, true),
    Present0 = preprocess_get_fields(Syntax, []),
    Present1 = lists:sort(Present0),
    Present = ordsets:from_list(Present1),
    case Present =:= Present1 of
	false ->
	    Dupl = Present1 -- Present,
	    asn1_error(S, {syntax_duplicated_fields,Dupl});
	true ->
	    ok
    end,
    Mandatory0 = get_mandatory_class_fields(Cs),
    Mandatory = ordsets:from_list(Mandatory0),
    case ordsets:subtract(Mandatory, Present) of
	[] ->
	    Syntax;
	[_|_]=Missing ->
	    asn1_error(S, {syntax_missing_mandatory_fields,Missing})
    end.

preprocess_syntax_1(S, [H|T], Cs, Mandatory) when is_list(H) ->
    [{optional,preprocess_syntax_1(S, H, Cs, false)}|
     preprocess_syntax_1(S, T, Cs, Mandatory)];
preprocess_syntax_1(S, [{valuefieldreference,Name}|T], Cs, Mandatory) ->
    F = preprocess_check_field(S, Name, Cs, Mandatory),
    [F|preprocess_syntax_1(S, T, Cs, Mandatory)];
preprocess_syntax_1(S, [{typefieldreference,Name}|T], Cs, Mandatory) ->
    F = preprocess_check_field(S, Name, Cs, Mandatory),
    [F|preprocess_syntax_1(S, T, Cs, Mandatory)];
preprocess_syntax_1(S,[{Token,_}|T], Cs, Mandatory) when is_atom(Token) ->
    [{token,Token}|preprocess_syntax_1(S, T, Cs, Mandatory)];
preprocess_syntax_1(S, [Token|T], Cs, Mandatory) when is_atom(Token) ->
    [{token,Token}|preprocess_syntax_1(S, T, Cs, Mandatory)];
preprocess_syntax_1(_, [], _, _) -> [].

preprocess_check_field(S, Name, Cs, Mandatory) ->
    case lists:keyfind(Name, 2, Cs) of
	Tuple when is_tuple(Tuple) ->
	    case not Mandatory andalso is_mandatory_class_field(Tuple) of
		true ->
		    asn1_error(S, {syntax_mandatory_in_optional_group,Name});
		false ->
		    {field,Tuple}
	    end;
	false ->
	    asn1_error(S, {syntax_undefined_field,Name})
    end.

preprocess_get_fields([{field,F}|T], Acc) ->
    Name = element(2, F),
    preprocess_get_fields(T, [Name|Acc]);
preprocess_get_fields([{optional,L}|T], Acc) ->
    preprocess_get_fields(T, preprocess_get_fields(L, Acc));
preprocess_get_fields([_|T], Acc) ->
    preprocess_get_fields(T, Acc);
preprocess_get_fields([], Acc) ->
    Acc.

%%%
%%% Match the actual fields in the object definition to
%%% the pre-processed simplified syntax.
%%%

match_syntax(S, [{token,Token}|T], [A|As]=Args, Acc) ->
    case A of
	{word_or_setting,_,#'Externaltypereference'{type=Token}} ->
	    match_syntax(S, T, As, Acc);
	{Token,Line} when is_integer(Line) ->
	    match_syntax(S, T, As, Acc);
	_ ->
	    {nomatch,Args}
    end;
match_syntax(S, [{field,Field}|T]=Fs, [A|As0]=Args0, Acc) ->
    try match_syntax_type(S, Field, A) of
	{match,Match} ->
	    match_syntax(S, T, As0, lists:reverse(Match)++Acc);
	{params,_Name,#ptypedef{args=Params}=P,Ref} ->
	    {Args,As} = lists:split(length(Params), As0),
	    Val = match_syntax_params(S, P, Ref, Args),
	    match_syntax(S, Fs, [Val|As], Acc)
    catch
	_:_ ->
	    {nomatch,Args0}
    end;
match_syntax(S, [{optional,L}|T], As0, Acc)         ->
    case match_syntax(S, L, As0, []) of
	{match,Match,As} ->
	    match_syntax(S, T, As, lists:reverse(Match)++Acc);
	{nomatch,As0} ->
	    match_syntax(S, T, As0, Acc);
	{nomatch,_}=NoMatch ->
	    NoMatch
    end;
match_syntax(_, [_|_], [], _Acc)                    ->
    {nomatch,[]};
match_syntax(_, [], As, Acc)                        ->
    {match,Acc,As}.

match_syntax_type(S, Type, {value_tag,Val})         ->
    match_syntax_type(S, Type, Val);
match_syntax_type(S, Type, {setting,_,Val})         ->
    match_syntax_type(S, Type, Val);
match_syntax_type(S, Type, {word_or_setting,_,Val}) ->
    match_syntax_type(S, Type, Val);
match_syntax_type(_S, _Type, {Atom,Line})
  when is_atom(Atom), is_integer(Line) ->
    throw(nomatch);
match_syntax_type(S, {fixedtypevaluefield,Name,#type{}=T,_,_}=Type,
		  #'Externalvaluereference'{}=ValRef0) ->
    try get_referenced_type(S, ValRef0) of
	{M,#valuedef{}=ValDef} ->
	    match_syntax_type(update_state(S, M), Type, ValDef)
    catch
	throw:{error,_} ->
	    ValRef = #valuedef{name=Name,
			       type=T,
			       value=ValRef0,
			       module=S#state.mname},
	    match_syntax_type(S, Type, ValRef)
    end;
match_syntax_type(S, {fixedtypevaluefield,Name,#type{},_,_}, #valuedef{}=Val0) ->
    Val = check_value(S, Val0),
    {match,[{Name,Val}]};
match_syntax_type(S, {fixedtypevaluefield,Name,#type{},_,_},
		  {'ValueFromObject',{object,Object},FieldNames}) ->
    Val = extract_field(S, Object, FieldNames),
    {match,[{Name,Val}]};
match_syntax_type(S, {fixedtypevaluefield,Name,#type{}=T,_,_}=Type, Any) ->
    ValDef = #valuedef{name=Name,type=T,value=Any,module=S#state.mname},
    match_syntax_type(S, Type, ValDef);
match_syntax_type(_S, {fixedtypevaluesetfield,Name,#type{},_}, Any) ->
    {match,[{Name,Any}]};
match_syntax_type(S, {objectfield,Name,_,_,_}, #'Externalvaluereference'{}=Ref) ->
    {M,Obj} = get_referenced_type(S, Ref),
    check_object(S, Obj, object_to_check(S, Obj)),
    {match,[{Name,Ref#'Externalvaluereference'{module=M}}]};
match_syntax_type(S, {objectfield,Name,Class,_,_}, {object,_,_}=ObjDef) ->
    InlinedObjName = list_to_atom(lists:concat([S#state.tname,
						'_',Name])),
    ObjSpec = #'Object'{classname=Class,def=ObjDef},
    CheckedObj = check_object(S, #typedef{typespec=ObjSpec}, ObjSpec),
    InlObj = #typedef{checked=true,name=InlinedObjName,typespec=CheckedObj},
    ObjKey = {InlinedObjName, InlinedObjName},
    insert_once(S, inlined_objects, ObjKey),
    %% Which module to use here? Could it be other than top_module?
    asn1_db:dbput(get(top_module), InlinedObjName, InlObj),
    {match,[{Name,InlObj}]};
match_syntax_type(_S, {objectfield,Name,_,_,_}, Any) ->
    {match,[{Name,Any}]};
match_syntax_type(S, {objectsetfield,Name,CDef0,_}, Any) ->
    CDef = case CDef0 of
	       #type{def=CDef1} -> CDef1;
	       CDef1 -> CDef1
	   end,
    case match_syntax_objset(S, Any, CDef) of
	#typedef{typespec=#'ObjectSet'{}=Ts0}=Def ->
	    Ts = check_object(S, Def, Ts0),
	    {match,[{Name,Def#typedef{checked=true,typespec=Ts}}]};
	_ ->
	    syntax_match_error(S, Any)
    end;
match_syntax_type(S, {typefield,Name0,_}, #type{def={pt,_,_}=Def}=Actual) ->
    %% This is an inlined type. If constructed type, save in data base.
    T = check_type(S, #typedef{typespec=Actual}, Actual),
    #'Externaltypereference'{type=PtName} = element(2, Def),
    NameList = [PtName,S#state.tname],
    Name = list_to_atom(asn1ct_gen:list2name(NameList)),
    NewTDef = #typedef{checked=true,name=Name,typespec=T},
    asn1_db:dbput(S#state.mname, Name, NewTDef),
    insert_once(S, parameterized_objects, {Name,type,NewTDef}),
    {match,[{Name0,NewTDef}]};
match_syntax_type(S, {typefield,Name,_}, #type{def=#'ObjectClassFieldType'{}}=Actual) ->
    T = check_type(S, #typedef{typespec=Actual}, Actual),
    {match,[{Name,ocft_def(T)}]};
match_syntax_type(S, {typefield,Name,_}, #type{def=#'Externaltypereference'{}=Ref}) ->
    match_syntax_external(S, Name, Ref);
match_syntax_type(S, {typefield,Name,_}, #type{def=Def}=Actual) ->
    T = check_type(S, #typedef{typespec=Actual}, Actual),
    TypeName = asn1ct_gen:type(asn1ct_gen:get_inner(Def)),
    {match,[{Name,#typedef{checked=true,name=TypeName,typespec=T}}]};
match_syntax_type(S, {typefield,Name,_}, #'Externaltypereference'{}=Ref) ->
    match_syntax_external(S, Name, Ref);
match_syntax_type(_S, {variabletypevaluefield,Name,_,_}, Any) ->
    {match,[{Name,Any}]};
match_syntax_type(_S, {variabletypevaluesetfield,Name,_,_}, Any) ->
    {match,[{Name,Any}]};
match_syntax_type(_S, _Type, _Actual) ->
    throw(nomatch).

match_syntax_params(S0, #ptypedef{name=Name}=PtDef,
		    #'Externaltypereference'{module=M,type=N}=ERef0, Args) ->
    S = S0#state{mname=M,module=load_asn1_module(S0, M),tname=Name},
    Type = check_type(S, PtDef, #type{def={pt,ERef0,Args}}),
    ERefName = new_reference_name(N),
    ERef = #'Externaltypereference'{type=ERefName,module=S0#state.mname},
    TDef = #typedef{checked=true,name=ERefName,typespec=Type},
    insert_once(S0, parameterized_objects, {ERefName,type,TDef}),
    asn1_db:dbput(S0#state.mname, ERef#'Externaltypereference'.type, TDef),
    ERef.

match_syntax_external(#state{mname=Mname}=S0, Name, Ref0) ->
    {M,T0} = get_referenced_type(S0, Ref0),
    Ref1 = Ref0#'Externaltypereference'{module=M},
    case T0 of
	#ptypedef{} ->
	    {params,Name,T0,Ref1};
	#typedef{checked=false}=TDef0 when Mname =/= M  ->
	    %% This typedef is an imported type (or maybe a set.asn
	    %% compilation).
	    S = S0#state{mname=M,module=load_asn1_module(S0, M),
			 tname=get_datastr_name(TDef0)},
	    Type = check_type(S, TDef0, TDef0#typedef.typespec),
	    TDef = TDef0#typedef{checked=true,typespec=Type},
	    asn1_db:dbput(M, get_datastr_name(TDef), TDef),
	    {match,[{Name,merged_name(S, Ref1)}]};
	TDef ->
	    %% This might be a renamed type in a set of specs,
	    %% so rename the ref.
	    Type = asn1ct:get_name_of_def(TDef),
	    Ref = Ref1#'Externaltypereference'{type=Type},
	    {match,[{Name,Ref}]}
    end.

match_syntax_objset(_S, {element_set,_,_}=Set, ClassDef) ->
    make_objset(ClassDef, Set);
match_syntax_objset(S, #'Externaltypereference'{}=Ref, _) ->
    {_,T} = get_referenced_type(S, Ref),
    T;
match_syntax_objset(S, #'Externalvaluereference'{}=Ref, _) ->
    {_,T} = get_referenced_type(S, Ref),
    T;
match_syntax_objset(_, [_|_]=Set, ClassDef) ->
    make_objset(ClassDef, Set);
match_syntax_objset(S, {object,definedsyntax,Words}, ClassDef) ->
    case Words of
	[Word] ->
	    match_syntax_objset_1(S, Word, ClassDef);
	[_|_] ->
	    %% More than one word does not make sense.
	    none
    end;
match_syntax_objset(S, #type{def=#'Externaltypereference'{}=Set}, ClassDef) ->
    match_syntax_objset(S, Set, ClassDef);
match_syntax_objset(_, #type{}, _) ->
    none.

match_syntax_objset_1(S, {setting,_,Set}, ClassDef) ->
    %% Word that starts with an uppercase letter.
    match_syntax_objset(S, Set, ClassDef);
match_syntax_objset_1(S, {word_or_setting,_,Set}, ClassDef) ->
    %% Word in uppercase/hyphens only.
    match_syntax_objset(S, Set, ClassDef);
match_syntax_objset_1(S, #type{def={'TypeFromObject', {object,Object}, FNs}},
		      ClassDef) ->
    Set = extract_field(S, Object, FNs),
    [_|_] = Set,
    #typedef{checked=true,typespec=#'ObjectSet'{class=ClassDef,set=Set}};
match_syntax_objset_1(_, #type{def=#'ObjectClassFieldType'{}}=Set, ClassDef) ->
    make_objset(ClassDef, Set);
match_syntax_objset_1(_, {object,_,_}=Object, ClassDef) ->
    make_objset(ClassDef, [Object]).

make_objset(ClassDef, Set) ->
    #typedef{typespec=#'ObjectSet'{class=ClassDef,set=Set}}.

-spec syntax_match_error(_) -> no_return().
syntax_match_error(S) ->
    asn1_error(S, syntax_nomatch).

-spec syntax_match_error(_, _) -> no_return().
syntax_match_error(S, What0) ->
    What = printable_string(What0),
    asn1_error(S, {syntax_nomatch,What}).

printable_string(Def) ->
    printable_string_1(Def).

printable_string_1({word_or_setting,_,Def}) ->
    printable_string_1(Def);
printable_string_1({value_tag,V}) ->
    printable_string_1(V);
printable_string_1({#seqtag{val=Val1},Val2}) ->
    atom_to_list(Val1) ++ " " ++ printable_string_1(Val2);
printable_string_1(#type{def=Def}) ->
    atom_to_list(asn1ct_gen:get_inner(Def));
printable_string_1(#'Externaltypereference'{type=Type}) ->
    atom_to_list(Type);
printable_string_1(#'Externalvaluereference'{value=Type}) ->
    atom_to_list(Type);
printable_string_1({Atom,Line}) when is_atom(Atom), is_integer(Line) ->
    q(Atom);
printable_string_1({object,definedsyntax,L}) ->
    q(string:join([printable_string_1(Item) || Item <- L], " "));
printable_string_1([_|_]=Def) ->
    case lists:all(fun is_integer/1, Def) of
	true ->
	    lists:flatten(io_lib:format("~p", [Def]));
	false ->
	    q(string:join([printable_string_1(Item) || Item <- Def], " "))
    end;
printable_string_1(Def) ->
    lists:flatten(io_lib:format("~p", [Def])).

q(S) ->
    lists:concat(["\"",S,"\""]).

check_defaultfields(S, Fields, ClassFields) ->
    Present = ordsets:from_list([F || {F,_} <- Fields]),
    Mandatory0 = get_mandatory_class_fields(ClassFields),
    Mandatory = ordsets:from_list(Mandatory0),
    All = ordsets:from_list([element(2, F) || F <- ClassFields]),
    #state{tname=Obj} = S,
    case ordsets:subtract(Present, All) of
	[] ->
	    ok;
	[_|_]=Invalid ->
	    asn1_error(S, {invalid_fields,Invalid,Obj})
    end,
    case ordsets:subtract(Mandatory, Present) of
	[] ->
	    check_defaultfields_1(S, Fields, ClassFields, []);
	[_|_]=Missing ->
	    asn1_error(S, {missing_mandatory_fields,Missing,Obj})
    end.

check_defaultfields_1(_S, [], _ClassFields, Acc) ->
    {object,defaultsyntax,lists:reverse(Acc)};
check_defaultfields_1(S, [{FName,Spec}|Fields], ClassFields, Acc) ->
    CField = lists:keyfind(FName, 2, ClassFields),
    {match,Match} = match_syntax_type(S, CField, Spec),
    check_defaultfields_1(S, Fields, ClassFields, Match++Acc).

get_mandatory_class_fields(ClassFields) ->
    [element(2, F) || F <- ClassFields,
		      is_mandatory_class_field(F)].

is_mandatory_class_field({fixedtypevaluefield,_,_,_,'MANDATORY'}) ->
    true;
is_mandatory_class_field({objectfield,_,_,_,'MANDATORY'}) ->
    true;
is_mandatory_class_field({objectsetfield,_,_,'MANDATORY'}) ->
    true;
is_mandatory_class_field({typefield,_,'MANDATORY'}) ->
    true;
is_mandatory_class_field({variabletypevaluefield,_,_,'MANDATORY'}) ->
    true;
is_mandatory_class_field({variabletypevaluesetfield,_,_,'MANDATORY'}) ->
    true;
is_mandatory_class_field(_) ->
    false.

merged_name(#state{inputmodules=[]},ERef) ->
    ERef;
merged_name(S,ERef=#'Externaltypereference'{module=M}) ->
    case {S#state.mname,lists:member(M,S#state.inputmodules)} of
	{M,_} ->
	    ERef;
	{MergeM,true} -> 
	    %% maybe the reference is renamed
	    NewName = renamed_reference(S,ERef),
	    ERef#'Externaltypereference'{module=MergeM,type=NewName};
	{_,_} -> % i.e. M /= MergeM, not an inputmodule
	    ERef
    end.

ocft_def(#type{def=#'ObjectClassFieldType'{type=OCFT}}=T) ->
    case OCFT of
	{fixedtypevaluefield,_,InnerType} ->
	    case asn1ct_gen:type(asn1ct_gen:get_inner(InnerType#type.def)) of
		Bif when Bif =:= {primitive,bif}; Bif =:= {constructed,bif} ->
		    #typedef{checked=true,name=Bif,typespec=InnerType};
		#'Externaltypereference'{}=Ref ->
		    Ref
	    end;
	'ASN1_OPEN_TYPE' ->
	    #typedef{checked=true,typespec=T#type{def='ASN1_OPEN_TYPE'}}
    end.

check_value(OldS,V) when is_record(V,pvaluesetdef) ->
    #pvaluesetdef{checked=Checked,type=Type} = V,
    case Checked of
	true -> V;
	{error,_} -> V;
	false ->
	    case get_referenced_type(OldS,Type#type.def) of
		{_,Class} when is_record(Class,classdef) ->
		    throw({pobjectsetdef});
		_ -> continue
	    end
    end;
check_value(_OldS,V) when is_record(V,pvaluedef) ->
    %% Fix this case later
    V;
check_value(OldS,V) when is_record(V,typedef) ->
    %% This case when a value set has been parsed as an object set.
    %% It may be a value set
    ?dbg("check_value, V: ~p~n",[V]),
    #typedef{typespec=TS} = V,
    case TS of 
	#'ObjectSet'{class=ClassRef} ->
	    {_RefM,TSDef} = get_referenced_type(OldS, ClassRef),
	    case TSDef of
		#classdef{} -> throw({objectsetdef});
		#typedef{typespec=#type{def=Eref}} when 
		      is_record(Eref,'Externaltypereference') ->
		    %% This case if the class reference is a defined
		    %% reference to class
		    check_value(OldS,V#typedef{typespec=TS#'ObjectSet'{class=Eref}});
		#typedef{typespec=HostType} ->
		    % an ordinary value set with a type in #typedef.typespec
		    ValueSet0 = TS#'ObjectSet'.set,
		    Constr = check_constraints(OldS, HostType, [ValueSet0]),
		    Type = check_type(OldS,TSDef,TSDef#typedef.typespec),
		    {valueset,Type#type{constraint=Constr}}
	    end;
	_ ->
	    throw({objectsetdef})
    end;
check_value(S,#valuedef{pos=Pos,name=Name,type=Type,
			  value={valueset,Constr}}) ->
    NewType = Type#type{constraint=[Constr]},
    {valueset,
     check_type(S,#typedef{pos=Pos,name=Name,typespec=NewType},NewType)};
check_value(S, #valuedef{}=V) ->
    ?dbg("check_value, V: ~p~n",[V0]),
    case V of
	#valuedef{checked=true} ->
	    V;
	#valuedef{checked=false} ->
	    check_valuedef(S, V)
    end.

check_valuedef(#state{recordtopname=TopName}=S0, V0) ->
    #valuedef{name=Name,type=Vtype0,value=Value,module=ModName} = V0,
    V = V0#valuedef{checked=true},
    Vtype = check_type(S0, #typedef{name=Name,typespec=Vtype0},Vtype0),
    Def = Vtype#type.def,
    S1 = S0#state{tname=Def},
    SVal = update_state(S1, ModName),
    case Def of
	#'Externaltypereference'{type=RecName}=Ext ->
	    {RefM,Type} = get_referenced_type(S1, Ext),
	    %% If V isn't a value but an object Type is a #classdef{}
	    S2 = update_state(S1, RefM),
	    case Type of
		#typedef{typespec=TypeSpec0}=TypeDef ->
		    TypeSpec = check_type(S2, TypeDef, TypeSpec0),
		    S3 = case is_contextswitchtype(Type) of
			     true ->
				 S2;
			     false ->
				 S2#state{recordtopname=[RecName|TopName]}
			 end,
		    #valuedef{value=CheckedVal} =
			check_value(S3, V0#valuedef{type=TypeSpec}),
		    V#valuedef{value=CheckedVal};
		#type{} ->
		    %% A parameter that couldn't be categorized.
		    #valuedef{value=CheckedVal} =
			check_value(S2#state{recordtopname=[RecName|TopName]},
				    V#valuedef{type=Type}),
		    V#valuedef{value=CheckedVal}
	    end;
	'ASN1_OPEN_TYPE' ->
	    {opentypefieldvalue,ANYType,ANYValue} = Value,
	    CheckedV = check_value(SVal,#valuedef{name=Name,
						  type=ANYType,
						  value=ANYValue,
						  module=ModName}),
	    V#valuedef{value=CheckedV#valuedef.value};
	'INTEGER' ->
	    V#valuedef{value=normalize_value(SVal, Vtype, Value, [])};
	{'INTEGER',_NamedNumberList} ->
	    V#valuedef{value=normalize_value(SVal, Vtype, Value, [])};
	#'SEQUENCE'{} ->
	    {ok,SeqVal} = convert_external(SVal, Vtype, Value),
	    V#valuedef{value=normalize_value(SVal, Vtype, SeqVal, TopName)};
	_ ->
	    V#valuedef{value=normalize_value(SVal, Vtype, Value, TopName)}
    end.

is_contextswitchtype(#typedef{name='EXTERNAL'})->
    true;
is_contextswitchtype(#typedef{name='EMBEDDED PDV'}) ->
    true;
is_contextswitchtype(#typedef{name='CHARACTER STRING'}) ->
    true;
is_contextswitchtype(_) ->
    false.

%%%
%%% Start of OBJECT IDENTFIER/RELATIVE-OID validation.
%%%

validate_objectidentifier(S, OidType, #'Externalvaluereference'{}=Id) ->
    %% Must be an OBJECT IDENTIFIER or RELATIVE-OID depending on OidType.
    get_oid_value(S, OidType, false, Id);
validate_objectidentifier(S, OidType, {'ValueFromObject',{object,Obj},Fields}) ->
    %% Must be an OBJECT IDENTIFIER/RELATIVE-OID depending on OidType.
    case extract_field(S, Obj, Fields) of
	#valuedef{checked=true,value=Value,type=Type} when is_tuple(Value) ->
	    _ = get_oid_type(S, OidType, Type),
	    Value;
	_ ->
	    asn1_error(S, {illegal_oid,OidType})
    end;
validate_objectidentifier(S, OidType,
			  [{#seqtag{module=Mod,pos=Pos,val=Atom},Val}]) ->
    %% This case is when an OBJECT IDENTIFIER value has been parsed as a
    %% SEQUENCE value.
    Rec = #'Externalvaluereference'{pos=Pos,
				    module=Mod,
				    value=Atom},
    validate_oid(S, OidType, [Rec,Val], []);
validate_objectidentifier(S, OidType, [_|_]=L0) ->
    validate_oid(S, OidType, L0, []);
validate_objectidentifier(S, OidType, _) ->
    asn1_error(S, {illegal_oid,OidType}).

get_oid_value(S, OidType, AllowInteger, #'Externalvaluereference'{}=Id) ->
    case get_referenced_type(S, Id) of
	{_,#valuedef{checked=Checked,type=Type,value=V}} ->
	    case get_oid_type(S, OidType, Type) of
		'INTEGER' when not AllowInteger ->
		    asn1_error(S, {illegal_oid,OidType});
		_ when Checked ->
		    V;
		'INTEGER' ->
		    V;
		_ ->
		    validate_objectidentifier(S, OidType, V)
	    end;
	_ ->
	    asn1_error(S, {illegal_oid,OidType})
    end.

validate_oid(S, OidType, [], Acc) ->
    Oid = lists:reverse(Acc),
    validate_oid_path(S, OidType, Oid),
    list_to_tuple(Oid);
validate_oid(S, OidType, [Value|Vrest], Acc) when is_integer(Value) ->
    validate_oid(S, OidType, Vrest, [Value|Acc]);
validate_oid(S, OidType, [{'NamedNumber',_Name,Value}|Vrest], Acc)
  when is_integer(Value) ->
    validate_oid(S, OidType, Vrest, [Value|Acc]);
validate_oid(S, OidType, [#'Externalvaluereference'{}=Id|Vrest], Acc) ->
    NeededOidType = case Acc of
			[] -> o_id;
			[_|_] -> rel_oid
		    end,
    try get_oid_value(S, NeededOidType, true, Id) of
	Val when is_integer(Val) ->
	    validate_oid(S, OidType, Vrest, [Val|Acc]);
	Val when is_tuple(Val) ->
	    L = tuple_to_list(Val),
	    validate_oid(S, OidType, Vrest, lists:reverse(L, Acc))
    catch
	_:_ ->
	    case reserved_objectid(Id#'Externalvaluereference'.value, Acc) of
		Value when is_integer(Value) ->
		    validate_oid(S, OidType,Vrest, [Value|Acc]);
		false ->
		    asn1_error(S, {illegal_oid,OidType})
	    end
    end;
validate_oid(S, OidType, _V, _Acc) ->
    asn1_error(S, {illegal_oid,OidType}).

get_oid_type(S, OidType, #type{def=Def}) ->
    get_oid_type(S, OidType, Def);
get_oid_type(S, OidType, #'Externaltypereference'{}=Id) ->
    {_,OI} = get_referenced_type(S, Id),
    get_oid_type(S, OidType, OI#typedef.typespec);
get_oid_type(_S, o_id, 'OBJECT IDENTIFIER'=T) ->
    T;
get_oid_type(_S, rel_oid, 'RELATIVE-OID'=T) ->
    T;
get_oid_type(_S, _, 'INTEGER'=T) ->
    T;
get_oid_type(S, OidType, _) ->
    asn1_error(S, {illegal_oid,OidType}).

%% ITU-T Rec. X.680 Annex B - D
reserved_objectid('itu-t',[]) -> 0;
reserved_objectid('ccitt',[]) -> 0;
%% arcs below "itu-t" 
reserved_objectid('recommendation',[0]) -> 0;
reserved_objectid('question',[0]) -> 1;
reserved_objectid('administration',[0]) -> 2;
reserved_objectid('network-operator',[0]) -> 3;
reserved_objectid('identified-organization',[0]) -> 4;
%% arcs below "recommendation"
reserved_objectid('a',[0,0]) -> 1;
reserved_objectid('b',[0,0]) -> 2;
reserved_objectid('c',[0,0]) -> 3;
reserved_objectid('d',[0,0]) -> 4;
reserved_objectid('e',[0,0]) -> 5;
reserved_objectid('f',[0,0]) -> 6;
reserved_objectid('g',[0,0]) -> 7;
reserved_objectid('h',[0,0]) -> 8;
reserved_objectid('i',[0,0]) -> 9;
reserved_objectid('j',[0,0]) -> 10;
reserved_objectid('k',[0,0]) -> 11;
reserved_objectid('l',[0,0]) -> 12;
reserved_objectid('m',[0,0]) -> 13;
reserved_objectid('n',[0,0]) -> 14;
reserved_objectid('o',[0,0]) -> 15;
reserved_objectid('p',[0,0]) -> 16;
reserved_objectid('q',[0,0]) -> 17;
reserved_objectid('r',[0,0]) -> 18;
reserved_objectid('s',[0,0]) -> 19;
reserved_objectid('t',[0,0]) -> 20;
reserved_objectid('u',[0,0]) -> 21;
reserved_objectid('v',[0,0]) -> 22;
reserved_objectid('w',[0,0]) -> 23;
reserved_objectid('x',[0,0]) -> 24;
reserved_objectid('y',[0,0]) -> 25;
reserved_objectid('z',[0,0]) -> 26;

reserved_objectid(iso,[]) -> 1;
%% arcs below "iso", note that number 1 is not used
reserved_objectid('standard',[1]) -> 0;
reserved_objectid('member-body',[1]) -> 2;
reserved_objectid('identified-organization',[1]) -> 3;

reserved_objectid('joint-iso-itu-t',[]) -> 2;
reserved_objectid('joint-iso-ccitt',[]) -> 2;

reserved_objectid(_,_) -> false.

validate_oid_path(_, rel_oid, _) ->
    ok;
validate_oid_path(_, o_id, [0,I|_]) when 0 =< I, I =< 9 ->
    ok;
validate_oid_path(_, o_id, [1,I|_]) when 0 =< I, I =< 3 ->
    ok;
validate_oid_path(_, o_id, [2|_]) ->
    ok;
validate_oid_path(S, o_id=OidType, _) ->
    asn1_error(S, {illegal_oid,OidType}).

%%%
%%% End of OBJECT IDENTFIER/RELATIVE-OID validation.
%%%

convert_external(S, Vtype, Value) ->
    case Vtype of
	#type{tag=[{tag,'UNIVERSAL',8,'IMPLICIT',32}]} ->
	    %% this is an 'EXTERNAL' (or INSTANCE OF)
	    case Value of
		[{#seqtag{val=identification},_}|_] ->
		    {ok,to_EXTERNAL1990(S, Value)};
		_ ->
		    {ok,Value}
	    end;
	_ ->
	    {ok,Value}
    end.

to_EXTERNAL1990(S, [{#seqtag{val=identification}=T,
		     {'CHOICE',{syntax,Stx}}}|Rest]) ->
    to_EXTERNAL1990(S, Rest, [{T#seqtag{val='direct-reference'},Stx}]);
to_EXTERNAL1990(S, [{#seqtag{val=identification}=T,
		     {'CHOICE',{'presentation-context-id',I}}}|Rest]) ->
    to_EXTERNAL1990(S, Rest, [{T#seqtag{val='indirect-reference'},I}]);
to_EXTERNAL1990(S, [{#seqtag{val=identification}=T,
		     {'CHOICE',{'context-negotiation',[{_,PCid},{_,TrStx}]}}}|Rest]) ->
    to_EXTERNAL1990(S, Rest, [{T#seqtag{val='indirect-reference'},PCid},
			      {T#seqtag{val='direct-reference'},TrStx}]);
to_EXTERNAL1990(S, _) ->
    asn1_error(S, illegal_external_value).

to_EXTERNAL1990(S, [V={#seqtag{val='data-value-descriptor'},_}|Rest], Acc) ->
    to_EXTERNAL1990(S, Rest, [V|Acc]);
to_EXTERNAL1990(_S, [{#seqtag{val='data-value'}=T,Val}], Acc) ->
    Encoding = {T#seqtag{val=encoding},{'CHOICE',{'octet-aligned',Val}}},
    lists:reverse([Encoding|Acc]);
to_EXTERNAL1990(S, _, _) ->
    asn1_error(S, illegal_external_value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Functions to normalize the default values of SEQUENCE 
%% and SET components into Erlang valid format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
normalize_value(_,_,mandatory,_) ->
    mandatory;
normalize_value(_,_,'OPTIONAL',_) ->
    'OPTIONAL';
normalize_value(S, Type, {'DEFAULT',Value}, NameList) ->
    case catch get_canonic_type(S,Type,NameList) of
	{'BOOLEAN',CType,_} ->
	    normalize_boolean(S,Value,CType);
	{'INTEGER',CType,_} ->
	    normalize_integer(S, Value, CType);
	{'BIT STRING',CType,_} ->
	    normalize_bitstring(S,Value,CType);
	{'OCTET STRING',_,_} ->
	    normalize_octetstring(S, Value);
	{'NULL',_CType,_} ->
	    %%normalize_null(Value);
	    'NULL';
	{'RELATIVE-OID',_,_} ->
	    normalize_relative_oid(S,Value);
	{'OBJECT IDENTIFIER',_,_} ->
	    normalize_objectidentifier(S,Value);
	{'ObjectDescriptor',_,_} ->
	    normalize_objectdescriptor(Value);
	{'REAL',_,_} ->
	    normalize_real(Value);
	{'ENUMERATED',CType,_} ->
	    normalize_enumerated(S,Value,CType);
	{'CHOICE',CType,NewNameList} ->
	    normalize_choice(S,Value,CType,NewNameList);
	{'SEQUENCE',CType,NewNameList} ->
	    normalize_sequence(S,Value,CType,NewNameList);
	{'SEQUENCE OF',CType,NewNameList} ->
	    normalize_seqof(S,Value,CType,NewNameList);
	{'SET',CType,NewNameList} ->
	    normalize_set(S,Value,CType,NewNameList);
	{'SET OF',CType,NewNameList} ->
	    normalize_setof(S,Value,CType,NewNameList);
	{restrictedstring,CType,_} ->
	    normalize_restrictedstring(S,Value,CType);
	{'ASN1_OPEN_TYPE',{typefield,_TF},NL} -> %an open type
	    normalize_objectclassfieldvalue(S,Value,NL);
	Err ->
	    asn1ct:warning("could not check default value ~p~nType:~n~p~nNameList:~n~p~n",
			   [Value,Type,Err],S,"could not check default value"),
	    Value
    end;
normalize_value(S,Type,Val,NameList) ->
    normalize_value(S,Type,{'DEFAULT',Val},NameList).

normalize_boolean(_,true,_) ->
    true;
normalize_boolean(_,false,_) ->
    false;
normalize_boolean(S,Bool=#'Externalvaluereference'{},CType) ->
    get_normalized_value(S,Bool,CType,fun normalize_boolean/3,[]);
normalize_boolean(S, _, _) ->
    asn1_error(S, {illegal_value, "BOOLEAN"}).

normalize_integer(_S, Int, _) when is_integer(Int) ->
    Int;
normalize_integer(S, #'Externalvaluereference'{value=Name}=Ref, NNL) ->
    case lists:keyfind(Name, 1, NNL) of
	{Name,Val} ->
	    Val;
	false ->
	    try get_referenced_value(S, Ref) of
		Val when is_integer(Val) ->
		    Val;
		_ ->
		    asn1_error(S, illegal_integer_value)
	    catch
		throw:_ ->
		    asn1_error(S, illegal_integer_value)
	    end
    end;
normalize_integer(S, {'ValueFromObject',{object,Obj},FieldNames}, _) ->
    case extract_field(S, Obj, FieldNames) of
	#valuedef{value=Val} when is_integer(Val) ->
	    Val;
	_ ->
	    asn1_error(S, illegal_integer_value)
    end;
normalize_integer(S, _, _) ->
    asn1_error(S, illegal_integer_value).

%% normalize_bitstring(S, Value, Type) -> bitstring()
%%  Convert a literal value for a BIT STRING to an Erlang bit string.
%%
normalize_bitstring(S, Value, Type)->
    case Value of
	{hstring,String} when is_list(String) ->
	    hstring_to_bitstring(String);
	{bstring,String} when is_list(String) ->
	    bstring_to_bitstring(String);
	#'Externalvaluereference'{} ->
	    Val = get_referenced_value(S, Value),
	    normalize_bitstring(S, Val, Type);
	{'ValueFromObject',{object,Obj},FieldNames} ->
	    case extract_field(S, Obj, FieldNames) of
		#valuedef{value=Val} ->
		    normalize_bitstring(S, Val, Type);
		_ ->
		    asn1_error(S, {illegal_value, "BIT STRING"})
	    end;
	RecList when is_list(RecList) ->
	    [normalize_bs_item(S, Item, Type) || Item <- RecList];
	Bs when is_bitstring(Bs) ->
	    %% Already normalized.
	    Bs;
	_ ->
	    asn1_error(S, {illegal_value, "BIT STRING"})
    end.

normalize_bs_item(S, #'Externalvaluereference'{value=Name}, Type) ->
    case lists:keymember(Name, 1, Type) of
	true -> Name;
	false -> asn1_error(S, {illegal_value, "BIT STRING"})
    end;
normalize_bs_item(_, Atom, _) when is_atom(Atom) ->
    Atom;
normalize_bs_item(S, _, _) ->
    asn1_error(S, {illegal_value, "BIT STRING"}).

hstring_to_binary(L) ->
    byte_align(hstring_to_bitstring(L)).

bstring_to_binary(L) ->
    byte_align(bstring_to_bitstring(L)).

byte_align(Bs) ->
    case bit_size(Bs) rem 8 of
	0 -> Bs;
	N -> <<Bs/bitstring,0:(8-N)>>
    end.

hstring_to_bitstring(L) ->
    << <<(hex_to_int(D)):4>> || D <- L >>.

bstring_to_bitstring(L) ->
    << <<(D-$0):1>> || D <- L >>.

hex_to_int(D) when $0 =< D, D =< $9 -> D - $0;
hex_to_int(D) when $A =< D, D =< $F -> D - ($A - 10).

%% normalize_octetstring/1 changes representation of input Value to a 
%% list of octets.
%% Format of Value is one of:
%% {bstring,String} each element in String corresponds to one bit in an octet
%% {hstring,String} each element in String corresponds to one byte in an octet
%% #'Externalvaluereference'
normalize_octetstring(S, Value) ->
    case Value of
	{bstring,String} ->
	    bstring_to_binary(String);
	{hstring,String} ->
	    hstring_to_binary(String);
	#'Externalvaluereference'{} ->
	    case get_referenced_value(S, Value) of
		String when is_binary(String) ->
		    String;
		Other ->
		    normalize_octetstring(S, Other)
	    end;
	{'ValueFromObject',{object,Obj},FieldNames} ->
	    case extract_field(S, Obj, FieldNames) of
		#valuedef{value=Val} when is_binary(Val) ->
		    Val;
		_ ->
		    asn1_error(S, illegal_octet_string_value)
	    end;
	_ ->
	    asn1_error(S, illegal_octet_string_value)
    end.

normalize_objectidentifier(S, Value) ->
    validate_objectidentifier(S, o_id, Value).

normalize_relative_oid(S, Value) ->
    validate_objectidentifier(S, rel_oid, Value).

normalize_objectdescriptor(Value) ->
    Value.

normalize_real(Value) ->
    Value.

normalize_enumerated(S, Id0, NNL) ->
    {Id,_} = lookup_enum_value(S, Id0, NNL),
    Id.

lookup_enum_value(S, Id, {Base,Ext}) ->
    %% Extensible ENUMERATED.
    lookup_enum_value(S, Id, Base++Ext);
lookup_enum_value(S, #'Externalvaluereference'{value=Id}, NNL) ->
    lookup_enum_value(S, Id, NNL);
lookup_enum_value(S, Id, NNL) when is_atom(Id) ->
    case lists:keyfind(Id, 1, NNL) of
	{_,_}=Ret ->
	    Ret;
	false ->
	    asn1_error(S, {undefined,Id})
    end.

normalize_choice(S, {'CHOICE',{C,V}}, CType, NameList)
  when is_atom(C) ->
    case lists:keyfind(C, #'ComponentType'.name, CType) of
	#'ComponentType'{typespec=CT,name=Name} ->
	    {C,normalize_value(S, CT, {'DEFAULT',V}, [Name|NameList])};
	false ->
	    asn1_error(S, {illegal_id,C})
    end;
normalize_choice(S,CV={Name,_ChoiceVal},CType,NameList)
  when is_atom(Name) ->
    normalize_choice(S,{'CHOICE',CV},CType,NameList);
normalize_choice(S, V, _CType, _NameList) ->
    asn1_error(S, {illegal_id, error_value(V)}).

normalize_sequence(S,Value,Components,NameList) 
  when is_tuple(Components) ->
    normalize_sequence(S,Value,lists:flatten(tuple_to_list(Components)),
		       NameList); 
normalize_sequence(S,{Name,Value},Components,NameList) 
  when is_atom(Name),is_list(Value) ->
    normalize_sequence(S,Value,Components,NameList);
normalize_sequence(S,Value,Components,NameList) ->
    normalized_record('SEQUENCE',S,Value,Components,NameList).

normalize_set(S,Value,Components,NameList) when is_tuple(Components) ->
    normalize_set(S,Value,lists:flatten(tuple_to_list(Components)),NameList);
normalize_set(S,{Name,Value},Components,NameList) 
  when is_atom(Name),is_list(Value) ->
    normalized_record('SET',S,Value,Components,NameList);
normalize_set(S,Value,Components,NameList) ->
    NewName = list_to_atom(asn1ct_gen:list2name(NameList)),
    case is_record_normalized(S,NewName,Value,length(Components)) of
	true ->
	    Value;
	_ ->
	    SortedVal = sort_value(Components,Value),
	    normalized_record('SET',S,SortedVal,Components,NameList)
    end.

sort_value(Components, Value0) when is_list(Value0) ->
    {Keys0,_} = lists:mapfoldl(fun(#'ComponentType'{name=N}, I) ->
				       {{N,I},I+1}
			       end, 0, Components),
    Keys = gb_trees:from_orddict(orddict:from_list(Keys0)),
    Value1 = [{case gb_trees:lookup(N, Keys) of
		   {value,K} -> K;
		   none -> 'end'
	       end,Pair} || {#seqtag{val=N},_}=Pair <- Value0],
    Value = lists:sort(Value1),
    [Pair || {_,Pair} <- Value];
sort_value(_Components, #'Externalvaluereference'{}=Value) ->
    %% Sort later.
    Value.

sort_val_if_set(['SET'|_],Val,Type) ->
    sort_value(Type,Val);
sort_val_if_set(_,Val,_) ->
    Val.

normalized_record(SorS,S,Value,Components,NameList) ->
    NewName = list_to_atom(lists:concat([get_record_prefix_name(S),
					 asn1ct_gen:list2name(NameList)])),
    case is_record_normalized(S,NewName,Value,length(Components)) of
	true ->
	    Value;
	_ ->
	    NoComps = length(Components),
	    ListOfVals = normalize_seq_or_set(SorS,S,Value,Components,NameList,[]),
	    NoComps = length(ListOfVals), %% Assert
	    list_to_tuple([NewName|ListOfVals])
    end.
is_record_normalized(S,Name,V = #'Externalvaluereference'{},NumComps) ->
    case get_referenced_type(S,V) of
	{_M,#valuedef{type=_T1,value=V2}} ->
	    is_record_normalized(S,Name,V2,NumComps);
	_ -> false
    end;
is_record_normalized(_S,Name,Value,NumComps) when is_tuple(Value) ->
    (tuple_size(Value) =:= (NumComps + 1)) andalso (element(1, Value) =:= Name);
is_record_normalized(_,_,_,_) ->
    false.

normalize_seq_or_set(SorS, S,
		     [{#seqtag{val=Cname},V}|Vs],
		     [#'ComponentType'{name=Cname,typespec=TS}|Cs],
		     NameList, Acc) ->
    NewNameList =
	case TS#type.def of
	    #'Externaltypereference'{type=TName} ->
		[TName];
	    _ -> [Cname|NameList]
	end,
    NVal = normalize_value(S,TS,{'DEFAULT',V},NewNameList),
    normalize_seq_or_set(SorS,S,Vs,Cs,NameList,[NVal|Acc]);
normalize_seq_or_set(SorS, S,
		     Values=[{#seqtag{val=Cname0},_V}|_Vs],
		     [#'ComponentType'{prop='OPTIONAL'}|Cs],
		     NameList, Acc) ->
    verify_valid_component(S, Cname0, Cs),
    normalize_seq_or_set(SorS,S,Values,Cs,NameList,[asn1_NOVALUE|Acc]);
normalize_seq_or_set(SorS, S,
		     Values=[{#seqtag{val=Cname0},_V}|_Vs],
		     [#'ComponentType'{name=Cname,typespec=TS,
				       prop={'DEFAULT',Value}}|Cs],
		    NameList, Acc) ->
    verify_valid_component(S, Cname0, Cs),
    NewNameList =
	case TS#type.def of
	    #'Externaltypereference'{type=TName} ->
		[TName];
	    _ -> [Cname|NameList]
	end,
    NVal =  normalize_value(S,TS,{'DEFAULT',Value},NewNameList),
    normalize_seq_or_set(SorS,S,Values,Cs,NameList,[NVal|Acc]);
%% If default value is {} ComponentTypes in SEQUENCE are marked DEFAULT 
%% or OPTIONAL (or the type is defined SEQUENCE{}, which is handled by
%% the previous case).
normalize_seq_or_set(SorS,S,[],
		     [#'ComponentType'{name=Name,typespec=TS,
				       prop={'DEFAULT',Value}}|Cs],
		     NameList,Acc) ->
    NewNameList =
	case TS#type.def of
	    #'Externaltypereference'{type=TName} ->
		[TName];
	    _ -> [Name|NameList]
	end,
    NVal =  normalize_value(S,TS,{'DEFAULT',Value},NewNameList),
    normalize_seq_or_set(SorS,S,[],Cs,NameList,[NVal|Acc]);
normalize_seq_or_set(SorS,S,[],[#'ComponentType'{prop='OPTIONAL'}|Cs],
		     NameList,Acc) ->
    normalize_seq_or_set(SorS,S,[],Cs,NameList,[asn1_NOVALUE|Acc]);
normalize_seq_or_set(SorS,S,Value=#'Externalvaluereference'{},
		     Cs,NameList,Acc) ->
    get_normalized_value(S,Value,Cs,fun normalize_seq_or_set/6,
			 [SorS,NameList,Acc]);
normalize_seq_or_set(_SorS, _S, [], [], _, Acc) ->
    lists:reverse(Acc);
normalize_seq_or_set(_SorS, S, V, Cs, _, _) ->
    case V of
	[{#seqtag{val=Name},_}|_] ->
	    asn1_error(S, {illegal_id,error_value(Name)});
	[] ->
	    [#'ComponentType'{name=Name}|_] = Cs,
	    asn1_error(S, {missing_id,error_value(Name)})
    end.

verify_valid_component(S, Name, Cs) ->
    case lists:keyfind(Name, #'ComponentType'.name, Cs) of
	false -> asn1_error(S, {illegal_id,error_value(Name)});
	#'ComponentType'{} -> ok
    end.

normalize_seqof(S,Value,Type,NameList) ->
    normalize_s_of('SEQUENCE OF',S,Value,Type,NameList).

normalize_setof(S,Value,Type,NameList) ->
    normalize_s_of('SET OF',S,Value,Type,NameList).

normalize_s_of(SorS,S,Value,Type,NameList) when is_list(Value) ->
    DefValueList = lists:map(fun(X) -> {'DEFAULT',X} end,Value),
    Suffix = asn1ct_gen:constructed_suffix(SorS,Type),
    Def = Type#type.def,
    InnerType = asn1ct_gen:get_inner(Def),
    WhatKind = asn1ct_gen:type(InnerType),
    NewNameList =
	case WhatKind of
	    {constructed,bif} ->
		[Suffix|NameList];
	    #'Externaltypereference'{type=Name} ->
		[Name];
	    _ -> []
	end,
    NormFun = 	fun (X) -> normalize_value(S,Type,X,
					   NewNameList) end,
    case catch lists:map(NormFun, DefValueList) of
	List when is_list(List) ->
	    List;
	_ ->
	    asn1ct:warning("~p could not handle value ~p~n",[SorS,Value],S,
			   "could not handle value"),
	    Value
    end;
normalize_s_of(SorS,S,Value,Type,NameList) 
  when is_record(Value,'Externalvaluereference') ->
    get_normalized_value(S,Value,Type,fun normalize_s_of/5,
			 [SorS,NameList]).


%% normalize_restrictedstring handles all format of restricted strings.
%% tuple case
% normalize_restrictedstring(_S,[Int1,Int2],_) when is_integer(Int1),is_integer(Int2) ->
%     {Int1,Int2};
% %% quadruple case
% normalize_restrictedstring(_S,[Int1,Int2,Int3,Int4],_) when is_integer(Int1),
% 							   is_integer(Int2),
% 							   is_integer(Int3),
% 							   is_integer(Int4) ->
%     {Int1,Int2,Int3,Int4};
%% character string list case
normalize_restrictedstring(S,[H|T],CType) when is_list(H);is_tuple(H) ->
    [normalize_restrictedstring(S,H,CType)|normalize_restrictedstring(S,T,CType)];
%% character sting case
normalize_restrictedstring(_S,CString,_) when is_list(CString) ->
    CString;
%% definedvalue case or argument in a parameterized type
normalize_restrictedstring(S,ERef,CType) when is_record(ERef,'Externalvaluereference') ->
    get_normalized_value(S,ERef,CType,
			 fun normalize_restrictedstring/3,[]).

normalize_objectclassfieldvalue(S,{opentypefieldvalue,Type,Value},NameList) ->
    %% An open type has per definition no type. Thus should the type
    %% information of the default type be available at
    %% encode/decode. But as encoding the default value causes special
    %% treatment (no encoding) whatever type is used the type
    %% information is not necessary in encode/decode.
    normalize_value(S,Type,Value,NameList);
normalize_objectclassfieldvalue(_S,Other,_NameList) ->
    %% If the type info was thrown away in an earlier step the value
    %% is already normalized.
     Other.

get_normalized_value(S,Val,Type,Func,AddArg) ->
    case catch get_referenced_type(S,Val) of
	{ExtM,_VDef = #valuedef{type=_T1,value=V}} -> 
	    %% should check that Type and T equals
 	    V2 = sort_val_if_set(AddArg,V,Type),
	    call_Func(update_state(S,ExtM),V2,Type,Func,AddArg);
	{error,_} ->
	    asn1ct:warning("default value not comparable ~p~n",[Val],S),
	    Val;
	{ExtM,NewVal} ->
	    V2 = sort_val_if_set(AddArg,NewVal,Type),
	    call_Func(update_state(S,ExtM),V2,Type,Func,AddArg);
	_ ->
	    asn1ct:warning("default value not comparable ~p~n",[Val],S,
			   "default value not comparable"),
	    Val
    end.

call_Func(S,Val,Type,Func,ArgList) ->	    
    case ArgList of
	[] ->
	    Func(S,Val,Type);
	[LastArg] ->
	    Func(S,Val,Type,LastArg);
	[Arg1,LastArg1] ->
	    Func(Arg1,S,Val,Type,LastArg1);
	[Arg1,LastArg1,LastArg2] ->
	    Func(Arg1,S,Val,Type,LastArg1,LastArg2)
    end.

    
get_canonic_type(S,Type,NameList) ->
    {InnerType,NewType,NewNameList} =
	case Type#type.def of
	    'INTEGER'=Name ->
		{Name,[],NameList};
	    Name when is_atom(Name) ->
		{Name,Type,NameList};
	    Ref when is_record(Ref,'Externaltypereference') ->
		{_,#typedef{name=Name,typespec=RefedType}} =
		    get_referenced_type(S,Ref),
		get_canonic_type(S,RefedType,[Name]);
	    {Name,T} when is_atom(Name) -> 
		{Name,T,NameList};
	    Seq when is_record(Seq,'SEQUENCE') -> 
		{'SEQUENCE',Seq#'SEQUENCE'.components,NameList};
	    Set when is_record(Set,'SET') -> 
		{'SET',Set#'SET'.components,NameList};
	    #'ObjectClassFieldType'{type=T} ->
		{'ASN1_OPEN_TYPE',T,NameList}
	end,
    {asn1ct_gen:unify_if_string(InnerType),NewType,NewNameList}.



check_ptype(S,Type,Ts) when is_record(Ts,type) ->
    check_formal_parameters(S, Type#ptypedef.args),
    Def = Ts#type.def,
    NewDef= 
	case Def of 
	    Seq when is_record(Seq,'SEQUENCE') ->
		Components = expand_components(S,Seq#'SEQUENCE'.components),
		#newt{type=Seq#'SEQUENCE'{pname=get_datastr_name(Type),
					  components = Components}};
	    Set when is_record(Set,'SET') ->
		Components = expand_components(S,Set#'SET'.components),
		#newt{type=Set#'SET'{pname=get_datastr_name(Type),
				     components = Components}};
	    _Other ->
		#newt{}
	end,
    Ts2 = case NewDef of
	      #newt{type=unchanged} ->
		  Ts;
	      #newt{type=TDef}->
		  Ts#type{def=TDef}
	  end,
    Ts2;
%parameterized class
check_ptype(_S,_PTDef,Ts) when is_record(Ts,objectclass) ->
    throw({asn1_param_class,Ts}).

check_formal_parameters(S, Args) ->
    _ = [check_formal_parameter(S, A) || A <- Args],
    ok.

check_formal_parameter(_, {_,_}) ->
    ok;
check_formal_parameter(_, #'Externaltypereference'{}) ->
    ok;
check_formal_parameter(S, #'Externalvaluereference'{value=Name}) ->
    asn1_error(S, {illegal_typereference,Name}).

% check_type(S,Type,ObjSpec={{objectclassname,_},_}) ->
 %     check_class(S,ObjSpec);
check_type(_S,Type,Ts) when is_record(Type,typedef),
			   (Type#typedef.checked==true) ->
    Ts;
check_type(_S,Type,Ts) when is_record(Type,typedef),
			   (Type#typedef.checked==idle) -> % the check is going on
    Ts;
check_type(S=#state{recordtopname=TopName},Type,Ts) when is_record(Ts,type) ->
    {Def,Tag,Constr,IsInlined} = 
	case match_parameter(S, Ts#type.def) of
	    #type{tag=PTag,constraint=_Ctmp,def=Dtmp,inlined=Inl} ->
		{Dtmp,merge_tags(Ts#type.tag,PTag),Ts#type.constraint,Inl};
	    #typedef{typespec=#type{tag=PTag,def=Dtmp,inlined=Inl}} ->
		{Dtmp,merge_tags(Ts#type.tag,PTag),Ts#type.constraint,Inl};
	    Dtmp ->
		{Dtmp,Ts#type.tag,Ts#type.constraint,Ts#type.inlined}
	end,
    TempNewDef = #newt{type=Def,tag=Tag,constraint=Constr,
		       inlined=IsInlined},
    TestFun = 
	fun(Tref) ->
		{_, MaybeChoice} = get_referenced_type(S, Tref, true),
		case catch((MaybeChoice#typedef.typespec)#type.def) of
		    {'CHOICE',_} ->
			maybe_illicit_implicit_tag(S, choice, Tag);
		    'ANY' ->
			maybe_illicit_implicit_tag(S, open_type, Tag);
		    'ANY DEFINED BY' ->
			maybe_illicit_implicit_tag(S, open_type, Tag);
		    'ASN1_OPEN_TYPE' ->
			maybe_illicit_implicit_tag(S, open_type, Tag);
		    _ ->
			Tag
		end
	end,
    NewDef= 
	case Def of 
	    Ext when is_record(Ext,'Externaltypereference') ->
		{RefMod,RefTypeDef,IsParamDef} = 
		    case get_referenced_type(S, Ext) of
			{undefined,TmpTDef} -> %% A parameter
			    {get(top_module),TmpTDef,true};
			{TmpRefMod,TmpRefDef} ->
			    {TmpRefMod,TmpRefDef,false}
		    end,
		case get_class_def(S, RefTypeDef) of
		    none -> ok;
		    #classdef{} -> throw({asn1_class,RefTypeDef})
		end,
		Ct = TestFun(Ext),
		{RefType,ExtRef} = 
		    case RefTypeDef#typedef.checked of
			true ->
			    {RefTypeDef#typedef.typespec,Ext};
			_ ->  
			    %% Put as idle to prevent recursive loops
			    NewRefTypeDef1 = RefTypeDef#typedef{checked=idle},
			    asn1_db:dbput(RefMod,
					  get_datastr_name(NewRefTypeDef1),
					  NewRefTypeDef1), 
			    NewS = S#state{mname=RefMod,
					   module=load_asn1_module(S,RefMod),
					   tname=get_datastr_name(NewRefTypeDef1),
					   abscomppath=[],recordtopname=[]},
			    RefType1 = 
				check_type(NewS,RefTypeDef,RefTypeDef#typedef.typespec),
			    %% update the type and mark as checked
			    NewRefTypeDef2 = 
				RefTypeDef#typedef{checked=true,typespec = RefType1},
			    TmpName = get_datastr_name(NewRefTypeDef2),
			    asn1_db:dbput(RefMod,
					  TmpName,
					  NewRefTypeDef2),
			    case {RefMod == get(top_module),IsParamDef} of
				{true,true} ->
				    Key = {TmpName,
					   type,
					   NewRefTypeDef2},
				    asn1ct_gen:insert_once(parameterized_objects,
							   Key);
				_ -> ok
			    end,
			    Pos = Ext#'Externaltypereference'.pos,
			    {RefType1,#'Externaltypereference'{module=RefMod,
							       pos=Pos,
							       type=TmpName}}
		    end,

		case asn1ct_gen:prim_bif(asn1ct_gen:get_inner(RefType#type.def)) of
		    true ->
			%% Here we expand to a built in type and inline it
			NewC = check_constraints(S, RefType, Constr ++
						     RefType#type.constraint),
			TempNewDef#newt{
			  type = RefType#type.def, 
			  tag = merge_tags(Ct,RefType#type.tag),
			  constraint = NewC};
		    _ ->
			%% Here we only expand the tags and keep the ext ref.
			    
			NewExt = ExtRef#'Externaltypereference'{module=merged_mod(S,RefMod,Ext)},
			TempNewDef#newt{
			  type = check_externaltypereference(S,NewExt),
			  tag = merge_tags(Ct,RefType#type.tag)}
		end;
	    'ANY' ->
		Ct = maybe_illicit_implicit_tag(S, open_type, Tag),
		TempNewDef#newt{type='ASN1_OPEN_TYPE',tag=Ct};
	    {'ANY_DEFINED_BY',_} ->
		Ct = maybe_illicit_implicit_tag(S, open_type, Tag),
		TempNewDef#newt{type='ASN1_OPEN_TYPE',tag=Ct};
	    'INTEGER' ->
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_INTEGER))};

	    {'INTEGER',NamedNumberList} ->
		TempNewDef#newt{type={'INTEGER',check_integer(S,NamedNumberList)},
				tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_INTEGER))};
	    'REAL' ->
		check_real(S,Constr),

		TempNewDef#newt{tag=merge_tags(Tag,?TAG_PRIMITIVE(?N_REAL))};
	    {'BIT STRING',NamedNumberList} ->
		NewL = check_bitstring(S, NamedNumberList),
		TempNewDef#newt{type={'BIT STRING',NewL},
				tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_BIT_STRING))};
	    'NULL' ->
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_NULL))};
	    'OBJECT IDENTIFIER' ->
		check_objectidentifier(S,Constr),
		TempNewDef#newt{tag=
			       merge_tags(Tag,?TAG_PRIMITIVE(?N_OBJECT_IDENTIFIER))};
	    'ObjectDescriptor' ->
		TempNewDef#newt{tag=
			       merge_tags(Tag,?TAG_PRIMITIVE(?N_OBJECT_DESCRIPTOR))};
	    'EXTERNAL' ->
		put_once(external,unchecked),
		TempNewDef#newt{type=
				#'Externaltypereference'{module=S#state.mname,
							 type='EXTERNAL'},
				tag=
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_EXTERNAL))};
	    {'INSTANCE OF',DefinedObjectClass,Constraint} ->
		%% check that DefinedObjectClass is of TYPE-IDENTIFIER class
		%% If Constraint is empty make it the general INSTANCE OF type
		%% If Constraint is not empty make an inlined type
		%% convert INSTANCE OF to the associated type
		IOFDef=check_instance_of(S,DefinedObjectClass,Constraint),
		TempNewDef#newt{type=IOFDef,
				tag=merge_tags(Tag,?TAG_CONSTRUCTED(?N_INSTANCE_OF))};
	    {'ENUMERATED',NamedNumberList} ->
		TempNewDef#newt{type=
				{'ENUMERATED',
				 check_enumerated(S, NamedNumberList)},
				tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_ENUMERATED)),
				constraint=[]};
	    'EMBEDDED PDV' ->
		put_once(embedded_pdv,unchecked),
		TempNewDef#newt{type=
				#'Externaltypereference'{module=S#state.mname,
							 type='EMBEDDED PDV'},
				tag= 
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_EMBEDDED_PDV))};
	    'BOOLEAN'->
		check_boolean(S,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_BOOLEAN))};
	    'OCTET STRING' ->
		check_octetstring(S,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_OCTET_STRING))};
	    'NumericString' ->
		check_restrictedstring(S,Def,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_NumericString))};
	    TString when TString =:= 'TeletexString'; 
			 TString =:= 'T61String' ->
		check_restrictedstring(S,Def,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_TeletexString))};
	    'VideotexString' ->
		check_restrictedstring(S,Def,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_VideotexString))};
	    'UTCTime' ->
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_UTCTime))};
	    'GeneralizedTime' ->
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_GeneralizedTime))};
	    'GraphicString' ->
		check_restrictedstring(S,Def,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_GraphicString))};
	    'VisibleString' ->
		check_restrictedstring(S,Def,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_VisibleString))};
	    'GeneralString' ->
		check_restrictedstring(S,Def,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_GeneralString))};
	    'PrintableString' ->
		check_restrictedstring(S,Def,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_PrintableString))};
	    'IA5String' ->
		check_restrictedstring(S,Def,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_IA5String))};
	    'BMPString' ->
		check_restrictedstring(S,Def,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_BMPString))};
	    'UniversalString' ->
		check_restrictedstring(S,Def,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_UniversalString))};
	    'UTF8String' ->
		check_restrictedstring(S,Def,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_UTF8String))};
	    'RELATIVE-OID' ->
		check_relative_oid(S,Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?'N_RELATIVE-OID'))};
	    'CHARACTER STRING' ->
		put_once(character_string,unchecked),
		TempNewDef#newt{type=
				#'Externaltypereference'{module=S#state.mname,
							 type='CHARACTER STRING'},
				tag=
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_CHARACTER_STRING))};
	    Seq when is_record(Seq,'SEQUENCE') ->
		RecordName = 
		    case TopName of
			[] ->
			    [get_datastr_name(Type)];
%			    [Type#typedef.name];
			_ -> 
			    TopName
		    end,
		{TableCInf,Components} =
		    check_sequence(S#state{recordtopname=
					   RecordName},
					   Type,Seq#'SEQUENCE'.components),
		TempNewDef#newt{type=Seq#'SEQUENCE'{tablecinf=tablecinf_choose(Seq,TableCInf),
					  components=Components},
				tag=
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_SEQUENCE))};
	    {'SEQUENCE OF',Components} ->
		TempNewDef#newt{type={'SEQUENCE OF',check_sequenceof(S,Type,Components)},
				tag=
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_SEQUENCE))};
	    {'CHOICE',Components} ->
		Ct = maybe_illicit_implicit_tag(S, choice, Tag),
		TempNewDef#newt{type={'CHOICE',check_choice(S,Type,Components)},tag=Ct};
	    Set when is_record(Set,'SET') ->
		RecordName=
		    case TopName of
			[] ->
			    [get_datastr_name(Type)];
%			    [Type#typedef.name];
			_ -> 
			    TopName
		    end,
		{Sorted,TableCInf,Components} =
		    check_set(S#state{recordtopname=RecordName},
			      Type,Set#'SET'.components),
		TempNewDef#newt{type=Set#'SET'{sorted=Sorted,
				     tablecinf=tablecinf_choose(Set,TableCInf),
				     components=Components},
				tag=
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_SET))};
	    {'SET OF',Components} ->
		TempNewDef#newt{type={'SET OF',check_setof(S,Type,Components)},
				tag=
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_SET))};

	    {pt,Ptype,ParaList} ->
		%% Ptype might be a parameterized - type, object set or
		%% value set. If it isn't a parameterized type notify the
		%% calling function.
		{_RefMod,Ptypedef} = get_referenced_type(S,Ptype),
		notify_if_not_ptype(S,Ptypedef),
		NewParaList = match_parameters(S, ParaList),
		Instance = instantiate_ptype(S,Ptypedef,NewParaList),
		TempNewDef#newt{type=Instance#type.def,
				tag=merge_tags(Tag,Instance#type.tag),
				constraint=Instance#type.constraint,
				inlined=yes};

	    #'ObjectClassFieldType'{classname=ClRef0}=OCFT0 ->
		%% this case occures in a SEQUENCE when 
		%% the type of the component is a ObjectClassFieldType
		ClRef = match_parameter(S, ClRef0),
		OCFT = OCFT0#'ObjectClassFieldType'{classname=ClRef},
		ClassSpec = check_class(S,ClRef),
		NewTypeDef = 
		    maybe_open_type(S,ClassSpec,
				    OCFT#'ObjectClassFieldType'{class=ClassSpec},Constr),
		InnerTag = get_innertag(S,NewTypeDef),
		MergedTag = merge_tags(Tag,InnerTag),
		Ct =
		    case is_open_type(NewTypeDef) of
			true ->
			    maybe_illicit_implicit_tag(S, open_type, MergedTag);
			_ ->
			    MergedTag
		    end,
		case TopName of
		    [] when Type#typedef.name =/= undefined ->
			%% This is a top-level type.
			#type{constraint=C,def=Simplified} =
			    simplify_type(#type{def=NewTypeDef,
						constraint=Constr}),
			TempNewDef#newt{type=Simplified,tag=Ct,
					constraint=C};
		    _ ->
			TempNewDef#newt{type=NewTypeDef,tag=Ct}
		end;

	    {'TypeFromObject',{object,Object},TypeField} ->
		CheckedT = get_type_from_object(S,Object,TypeField),
		TempNewDef#newt{tag=merge_tags(Tag,CheckedT#type.tag),
				type=CheckedT#type.def};

	    {'SelectionType',Name,T} ->
		CheckedT = check_selectiontype(S,Name,T),
		TempNewDef#newt{tag=merge_tags(Tag,CheckedT#type.tag),
				type=CheckedT#type.def};
	    'ASN1_OPEN_TYPE' ->
		TempNewDef
	end,
    #newt{type=TDef,tag=NewTags,constraint=NewConstr,inlined=Inlined} = NewDef,
    Ts#type{def=TDef,
	    inlined=Inlined,
	    constraint=check_constraints(S, #type{def=TDef}, NewConstr),
	    tag=lists:map(fun(#tag{type={default,TTx}}=TempTag) ->
				  TempTag#tag{type=TTx};
			     (Other) -> Other
			  end, NewTags)}.


%%
%% Simplify the backends by getting rid of an #'ObjectClassFieldType'{}
%% with a type known at compile time.
%%

simplify_comps(Comps) ->
    [simplify_comp(Comp) || Comp <- Comps].

simplify_comp(#'ComponentType'{typespec=Type0}=C) ->
    Type = simplify_type(Type0),
    C#'ComponentType'{typespec=Type};
simplify_comp(Other) -> Other.

simplify_type(#type{tag=Tag,def=Inner,constraint=Constr0}=T) ->
    case Inner of
	#'ObjectClassFieldType'{type={fixedtypevaluefield,_,Type}}=OCFT ->
	    Constr = [{ocft,OCFT}|Type#type.constraint++Constr0],
	    Type#type{tag=Tag,constraint=Constr};
	_ ->
	    T
    end.

%% tablecinf_choose. A SEQUENCE or SET may be inserted in another
%% SEQUENCE or SET by the COMPONENTS OF directive. If this inserted
%% type is a referenced type that already has been checked it already
%% has its tableconstraint information. Furthermore this information
%% may be lost in the analysis in the new environment. Assume this
%% SEQUENCE/SET has a simpletable constraint and a componentrelation
%% constraint whose atlist points to the outermost component of its
%% "standalone" definition. This will cause the analysis to fail as it
%% will not find the right atlist component in the outermost
%% environment in the new inlined environment.
tablecinf_choose(SetOrSeq,false) ->
    tablecinf_choose(SetOrSeq);
tablecinf_choose(_, TableCInf) ->
    TableCInf.
tablecinf_choose(#'SET'{tablecinf=TCI}) -> 
    TCI;
tablecinf_choose(#'SEQUENCE'{tablecinf=TCI}) ->
    TCI.

get_innertag(_S,#'ObjectClassFieldType'{type=Type}) ->
    case Type of
%	#type{tag=Tag} -> Tag;
%	{fixedtypevaluefield,_,#type{tag=[]}=T} -> get_taglist(S,T);
	{fixedtypevaluefield,_,#type{tag=Tag}} -> Tag;
	{TypeFieldName,_} when is_atom(TypeFieldName) -> [];
	_ -> []
    end.
    
%% get_class_def(S, Type) -> #classdef{} | 'none'.
get_class_def(S, #typedef{typespec=#type{def=#'Externaltypereference'{}=Eref}}) ->
    {_,NextDef} = get_referenced_type(S, Eref, true),
    get_class_def(S, NextDef);
get_class_def(S, #'Externaltypereference'{}=Eref) ->
    {_,NextDef} = get_referenced_type(S, Eref, true),
    get_class_def(S, NextDef);
get_class_def(_S, #classdef{}=CD) ->
    CD;
get_class_def(_S, _) ->
    none.
    
maybe_illicit_implicit_tag(S, Kind, Tag) ->
    case Tag of
	[#tag{type='IMPLICIT'}|_T] ->
	    asn1_error(S, {implicit_tag_before,Kind});
	[ChTag = #tag{type={default,_}}|T] -> 
	    case Kind of
		open_type ->
		    [ChTag#tag{type='EXPLICIT',form=32}|T]; %X.680 30.6c, X.690 8.14.2
		choice ->
		    [ChTag#tag{type='EXPLICIT',form=32}|T] % X.680 28.6 c, 30.6c
	    end;
	_ -> 
	    Tag % unchanged
    end.


merged_mod(S,RefMod,Ext) ->
    case S of
	#state{inputmodules=[]} ->
	    RefMod;
	_ ->
	    Ext#'Externaltypereference'.module
    end.

%% maybe_open_type/2 -> #ObjectClassFieldType with updated fieldname and
%% type 
%% if the FieldRefList points out a typefield and the class don't have
%% any UNIQUE field, so that a component relation constraint cannot specify
%% the type of a typefield, return 'ASN1_OPEN_TYPE'.
%% 
maybe_open_type(_, _, #'ObjectClassFieldType'{fieldname={_,_}}=OCFT, _) ->
    %% Already converted.
    OCFT;
maybe_open_type(S, #objectclass{fields=Fs}=ClassSpec,
		#'ObjectClassFieldType'{fieldname=FieldRefList}=OCFT,
		Constr) ->
    Type = get_OCFType(S, Fs, FieldRefList),
    FieldNames = get_referenced_fieldname(FieldRefList),
    case lists:last(FieldRefList) of
	{valuefieldreference,_} ->
	    OCFT#'ObjectClassFieldType'{fieldname=FieldNames,
					type=Type};
	{typefieldreference,_} ->
	    %% Note: The constraints have not been checked yet,
	    %% so we must use a special lookup routine.
	    case {get_unique_fieldname(S, #classdef{typespec=ClassSpec}),
		  get_componentrelation(Constr)} of
		{no_unique,_} ->
		    OCFT#'ObjectClassFieldType'{fieldname=FieldNames,
						type='ASN1_OPEN_TYPE'};
		{_,no} ->
		    OCFT#'ObjectClassFieldType'{fieldname=FieldNames,
						type='ASN1_OPEN_TYPE'};
		_ ->
		    OCFT#'ObjectClassFieldType'{fieldname=FieldNames,
						type=Type}
	    end
    end.

get_componentrelation([{element_set,{componentrelation,_,_}=Cr,none}|_]) ->
    Cr;
get_componentrelation([_|T]) ->
    get_componentrelation(T);
get_componentrelation([]) ->
    no.

is_open_type(#'ObjectClassFieldType'{type='ASN1_OPEN_TYPE'}) ->
    true;
is_open_type(#'ObjectClassFieldType'{}) ->
    false.


notify_if_not_ptype(S,#pvaluesetdef{type=Type}) ->
    case Type#type.def of
	Ref when is_record(Ref,'Externaltypereference') ->
	    case get_referenced_type(S,Ref) of
		{_,#classdef{}} ->
		    throw(pobjectsetdef);
		{_,#typedef{}} ->
		    throw(pvalueset)
	    end;
	T when is_record(T,type) -> % this must be a value set
	    throw(pvalueset)
    end;
notify_if_not_ptype(_S,PT=#ptypedef{}) ->
    %% this may be a parameterized CLASS, in that case throw an
    %% asn1_class exception
    case PT#ptypedef.typespec of
	#objectclass{} -> throw({asn1_class,PT});
	_ -> ok
    end;
notify_if_not_ptype(S,#pobjectsetdef{class=Cl}) ->
    case Cl of
	#'Externaltypereference'{} ->
	    case get_referenced_type(S,Cl) of
		{_,#classdef{}} ->
		    throw(pobjectsetdef);
		{_,#typedef{}} ->
		    throw(pvalueset)
	    end;
	_ ->
	    throw(pobjectsetdef)
    end;
notify_if_not_ptype(S, PT) ->
    asn1_error(S, {param_bad_type, error_value(PT)}).

instantiate_ptype(S,Ptypedef,ParaList) ->
    #ptypedef{args=Args,typespec=Type} = Ptypedef,
    NewType = check_ptype(S,Ptypedef,Type#type{inlined=yes}),    
    MatchedArgs = match_args(S,Args, ParaList, []),
    OldArgs = S#state.parameters,
    NewS = S#state{parameters=MatchedArgs++OldArgs,abscomppath=[]},
    check_type(NewS, Ptypedef#ptypedef{typespec=NewType}, NewType).

get_datastr_name(Type) ->
    asn1ct:get_name_of_def(Type).

get_pt_args(#ptypedef{args=Args}) ->
    Args;
get_pt_args(#pvaluesetdef{args=Args}) ->
    Args;
get_pt_args(#pvaluedef{args=Args}) ->
    Args;
get_pt_args(#pobjectdef{args=Args}) ->
    Args;
get_pt_args(#pobjectsetdef{args=Args}) ->
    Args.

get_pt_spec(#ptypedef{typespec=Type}) ->
    Type;
get_pt_spec(#pvaluedef{value=Value}) ->
    Value;
get_pt_spec(#pvaluesetdef{valueset=VS}) ->
    VS;
get_pt_spec(#pobjectdef{def=Def}) ->
    Def;
get_pt_spec(#pobjectsetdef{def=Def}) ->
    Def.
	   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% match_args(S,FormalArgs, ActualArgs, Accumulator) -> Result
%% S = #state{}
%% FormalArgs = [term()] | [{Governor,Parameter}]
%% ActualArgs = [term()]
%% Accumulator = [term()]
%% Result = [{term(),term()}] | throw()
%% Governor = #type{} | Reference | 'TYPE-IDENTIFIER' | 'ABSTRACT-SYNTAX'
%% Parameter = Reference | {Governor,Reference}
%% Reference = #'Externaltypereference'{} | #'Externalvaluerference'{}
%%
%% Different categories of parameters and governors (Dubuisson p.382) 
%% +----------------+-------------------------------+----------------------+
%% |Governor is	    | 	Parameter name style   	    |  	Parameter is	   |
%% +----------------+-------------------------------+----------------------+
%% | absent    	    |  	begins with uppercase,(bu)  |  	a type 		   |
%% |   	       	    |  	       	       	       	    |  	       		   |
%% | a type	    | 	begins with a lowercase,(bl)|  	a value		   |
%% |   	       	    |  	       	       	       	    |  	       		   |
%% | a type	    | 	begins with an uppercase    | 	a value set	   |
%% |		    | 				    |  		   	   |
%% | absent    	    |  	entirely in uppercase, (eu) |  	a class (or type)  |
%% |		    | 				    |  			   |
%% | a class name   |  	begins with a lowercase	    |  	an object      	   |
%% |		    | 				    |  		 	   |
%% | a class name   |  	begins with an uppercase    |  	an object set	   |
%% +----------------+-------------------------------+----------------------+
%%
%% Matches each of the formal parameters to corresponding actual
%% parameter, and changes format of the actual parameter according to
%% above table if necessary.
match_args(S,FA = [FormArg|Ft], AA = [ActArg|At], Acc) ->
    OldParams = S#state.parameters,
    case categorize_arg(S,FormArg,ActArg) of
	[CategorizedArg] -> 
	    match_args(S#state{parameters=
			       [{FormArg,CategorizedArg}|OldParams]},
		       Ft, At, [{FormArg,CategorizedArg}|Acc]);
	CategorizedArgs ->
	    match_args(S#state{parameters=CategorizedArgs++OldParams},
		       FA, CategorizedArgs ++ AA, Acc)
    end;
match_args(_S,[], [], Acc) ->
    lists:reverse(Acc);
match_args(S, _, _, _) ->
    asn1_error(S, param_wrong_number_of_arguments).

%%%%%%%%%%%%%%%%%
%% categorize_arg(S,FormalArg,ActualArg) -> {FormalArg,CatgorizedActualArg}
%%
categorize_arg(S,{Governor,Param},ActArg) ->
    case {governor_category(S, Governor),parameter_name_style(Param)} of
	{type,beginning_lowercase} ->		%a value
	    categorize(S, value, Governor, ActArg);
	{type,beginning_uppercase} ->		%a value set
	    categorize(ActArg);
	{{class,ClassRef},beginning_lowercase} -> 
	    categorize(S, object, ActArg, ClassRef);
	{{class,ClassRef},beginning_uppercase} ->
	    categorize(S, object_set, ActArg, ClassRef)
    end;
categorize_arg(_S, _FormalArg, ActualArg) ->
    %% Governor is absent -- must be a type or a class. We have already
    %% checked that the FormalArg begins with an uppercase letter.
    categorize(ActualArg).

%% governor_category(S, Item) -> type | {class,#'Externaltypereference'{}}
%%  Determine whether Item is a type or a class.
governor_category(S, #type{def=#'Externaltypereference'{}=Eref}) ->
    governor_category(S, Eref);
governor_category(_S, #type{}) ->
    type;
governor_category(S, #'Externaltypereference'{}=Ref) ->
    case get_class_def(S, Ref) of
	#classdef{pos=Pos,module=Mod,name=Name} ->
	    {class,#'Externaltypereference'{pos=Pos,module=Mod,type=Name}};
	none ->
	    type
    end.

%% parameter_name_style(Param,Data) -> Result
%% gets the Parameter and the name of the Data and if it exists tells
%% whether it begins with a lowercase letter or is partly or entirely
%% spelled with uppercase letters. Otherwise returns undefined
%%
parameter_name_style(#'Externaltypereference'{}) ->
    beginning_uppercase;
parameter_name_style(#'Externalvaluereference'{}) ->
    beginning_lowercase.

%% categorize(Parameter) -> CategorizedParameter
%% If Parameter has an abstract syntax of another category than
%% Category, transform it to a known syntax.
categorize({object,_,Type}) ->
    %% One example of this case is an object with a parameterized type
    %% having a locally defined type as parameter.
    Def = fun(D = #type{}) ->
		  #typedef{name = new_reference_name("type_argument"),
			   typespec = D#type{inlined=yes}};
	     ({setting,_,Eref}) when is_record(Eref,'Externaltypereference') ->
		  Eref;
	     (D) ->
		  D
	  end,
    [Def(X)||X<-Type];
categorize(#type{}=Def) ->
    [#typedef{name = new_reference_name("type_argument"),
	      typespec = Def#type{inlined=yes}}];
categorize(Def) ->
    [Def].

categorize(S,object_set,Def,ClassRef) ->
    NewObjSetSpec = 
	check_object(S,Def,#'ObjectSet'{class = ClassRef,
					set = parse_objectset(Def)}),
    Name = new_reference_name("object_set_argument"),
    [save_object_set_instance(S,Name,NewObjSetSpec)];
categorize(_S,object,Def,_ClassRef) ->
    %% should be handled
    [Def];
categorize(_S,value,_Type,Value) when is_record(Value,valuedef) ->
    [Value];
categorize(S,value,Type,Value) ->
%%    [check_value(S,#valuedef{type=Type,value=Value})].
    [#valuedef{type=Type,value=Value,module=S#state.mname}].


parse_objectset({valueset,#type{def=#'Externaltypereference'{}=Ref}}) ->
    Ref;
parse_objectset({valueset,Set}) ->
    Set;
parse_objectset(#type{def=Ref}) when is_record(Ref,'Externaltypereference') ->
    Ref;
parse_objectset(Set) ->
    %% extend this later
    Set.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Check and simplify constraints.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_constraints(_S, _HostType, []) ->
    [];
check_constraints(S, HostType0, [_|_]=Cs0) ->
    HostType = get_real_host_type(HostType0, Cs0),
    Cs1 = top_level_intersections(Cs0),
    Cs2 = [coalesce_constraints(C) || C <- Cs1],
    {_,Cs3} = filter_extensions(Cs2),
    Cs = simplify_element_sets(S, HostType, Cs3),
    finish_constraints(Cs).

get_real_host_type(HostType, Cs) ->
    case lists:keyfind(ocft, 1, Cs) of
	false -> HostType;
	{_,OCFT} -> HostType#type{def=OCFT}
    end.

top_level_intersections([{element_set,{intersection,_,_}=C,none}]) ->
    top_level_intersections_1(C);
top_level_intersections(Cs) ->
    Cs.

top_level_intersections_1({intersection,A,B}) ->
    [{element_set,A,none}|top_level_intersections_1(B)];
top_level_intersections_1(Other) ->
    [{element_set,Other,none}].

coalesce_constraints({element_set,
		  {Tag,{element_set,A,_}},
		  {Tag,{element_set,B,_}}}) ->
    %% (SIZE (C1), ..., (SIZE (C2)) => (SIZE (C1, ..., C2))
    {element_set,{Tag,{element_set,A,B}},none};
coalesce_constraints(Other) ->
    Other.

%% Remove all outermost extensions except the last.

filter_extensions([H0|T0]) ->
    case filter_extensions(T0) of
	{true,T} ->
	    H = remove_extension(H0),
	    {true,[H|T]};
	{false,T} ->
	    {any_extension(H0),[H0|T]}
    end;
filter_extensions([]) ->
    {false,[]}.

remove_extension({element_set,Root,_}) ->
    {element_set,remove_extension(Root),none};
remove_extension(Tuple) when is_tuple(Tuple) ->
    L = [remove_extension(El) || El <- tuple_to_list(Tuple)],
    list_to_tuple(L);
remove_extension(Other) -> Other.

any_extension({element_set,_,Ext}) when Ext =/= none ->
    true;
any_extension(Tuple) when is_tuple(Tuple) ->
    any_extension_tuple(1, Tuple);
any_extension(_) -> false.

any_extension_tuple(I, T) when I =< tuple_size(T) ->
    any_extension(element(I, T)) orelse any_extension_tuple(I+1, T);
any_extension_tuple(_, _) -> false.

simplify_element_sets(S, HostType, [{element_set,R0,E0}|T0]) ->
    R1 = simplify_element_set(S, HostType, R0),
    E1 = simplify_element_set(S, HostType, E0),
    case simplify_element_sets(S, HostType, T0) of
	[{element_set,R2,E2}] ->
	    [{element_set,cs_intersection(S, R1, R2),
	      cs_intersection(S, E1, E2)}];
	L when is_list(L) ->
	    [{element_set,R1,E1}|L]
    end;
simplify_element_sets(S, HostType, [H|T]) ->
    [H|simplify_element_sets(S, HostType, T)];
simplify_element_sets(_, _, []) ->
    [].

simplify_element_set(_S, _HostType, empty) ->
    {set,[]};
simplify_element_set(S, HostType, {'SingleValue',Vs0}) when is_list(Vs0) ->
    Vs1 = [resolve_value(S, HostType, V) || V <- Vs0],
    Vs = make_constr_set_vs(Vs1),
    simplify_element_set(S, HostType, Vs);
simplify_element_set(S, HostType, {'SingleValue',V0}) ->
    V1 = resolve_value(S, HostType, V0),
    V = {set,[{range,V1,V1}]},
    simplify_element_set(S, HostType, V);
simplify_element_set(S, HostType, {'ValueRange',{Lb0,Ub0}}) ->
    Lb = resolve_value(S, HostType, Lb0),
    Ub = resolve_value(S, HostType, Ub0),
    V = make_constr_set(S, Lb, Ub),
    simplify_element_set(S, HostType, V);
simplify_element_set(S, HostType, {'ALL-EXCEPT',Set0}) ->
    Set = simplify_element_set(S, HostType, Set0),
    {'ALL-EXCEPT',Set};
simplify_element_set(S, HostType, {intersection,A0,B0}) ->
    A = simplify_element_set(S, HostType, A0),
    B = simplify_element_set(S, HostType, B0),
    cs_intersection(S, A, B);
simplify_element_set(S, HostType, {union,A0,B0}) ->
    A = simplify_element_set(S, HostType, A0),
    B = simplify_element_set(S, HostType, B0),
    cs_union(S, A, B);
simplify_element_set(S, HostType, {simpletable,{element_set,Type,_}}) ->
    check_simpletable(S, HostType, Type);
simplify_element_set(S, _, {componentrelation,R,Id}) ->
    check_componentrelation(S, R, Id);
simplify_element_set(S, HostType, {Tag,{element_set,_,_}=El0}) ->
    [El1] = simplify_element_sets(S, HostType, [El0]),
    {Tag,El1};
simplify_element_set(S, HostType, #type{}=Type) ->
    simplify_element_set_type(S, HostType, Type);
simplify_element_set(_, _, C) ->
    C.

simplify_element_set_type(S, HostType, #type{def=Def0}=Type0) ->
    #'Externaltypereference'{} = Def0,		%Assertion.
    case get_referenced_type(S, Def0) of
	{_,#valuedef{checked=false,value={valueset,Vs0}}} ->
	    [Vs1] = simplify_element_sets(S, HostType, [Vs0]),
	    case Vs1 of
		{element_set,Set,none} ->
		    Set;
		{element_set,Set,{set,[]}} ->
		    Set
	    end;
	{_,{valueset,#type{def=#'Externaltypereference'{}}=Type}} ->
	    simplify_element_set_type(S, HostType, Type);
	_ ->
	    case HostType of
		#type{def=#'ObjectClassFieldType'{}} ->
		    %% Open type.
		    #type{def=Def} = check_type(S, HostType, Type0),
		    Def;
		_ ->
		    #type{constraint=Cs} = check_type(S, HostType, Type0),
		    C = convert_back(Cs),
		    simplify_element_set(S, HostType, C)
	    end
    end.

convert_back([H1,H2|T]) ->
   {intersection,H1,convert_back([H2|T])};
convert_back([H]) ->
    H;
convert_back([]) ->
    none.

check_simpletable(S, HostType, Type) ->
    case HostType of
	#type{def=#'ObjectClassFieldType'{}} ->
	    ok;
	_ ->
	    %% Table constraints may only be applied to
	    %% CLASS.&field constructs.
	    asn1_error(S, illegal_table_constraint)
    end,
    Def = case Type of
	      #type{def=D} -> D;
	      {'SingleValue',#'Externalvaluereference'{}=ObjRef} ->
		  ObjRef;
	      _ ->
		  asn1_error(S, invalid_table_constraint)
	  end,
    C = match_parameter(S, Def),
    case C of
	#'Externaltypereference'{} ->
	    ERef = check_externaltypereference(S, C),
	    {simpletable,ERef#'Externaltypereference'.type};
	#'Externalvaluereference'{} ->
	    %% This is an object set with a referenced object
	    {_,TorVDef} = get_referenced_type(S, C),
	    Set = case TorVDef of
		      #typedef{typespec=#'Object'{classname=ClassName}} ->
			  #'ObjectSet'{class=ClassName,
				       set={'SingleValue',C}};
		      #valuedef{type=#type{def=ClassDef},
				value=#'Externalvaluereference'{}=Obj} ->
			  %% an object might reference another object
			  #'ObjectSet'{class=ClassDef,
				       set={'SingleValue',Obj}}
		  end,
	    {simpletable,check_object(S, Type, Set)};
	{'ValueFromObject',{_,Object},FieldNames} ->
	    %% This is an ObjectFromObject.
	    {simpletable,extract_field(S, Object, FieldNames)}
    end.

check_componentrelation(S, {objectset,Opos,Objset0}, Id) ->
    %% Objset is an 'Externaltypereference' record, since Objset is
    %% a DefinedObjectSet.
    ObjSet = match_parameter(S, Objset0),
    Ext = check_externaltypereference(S, ObjSet),
    {componentrelation,{objectset,Opos,Ext},Id}.

%%%
%%% Internal set representation.
%%%
%%% We represent sets as a union of strictly disjoint ranges:
%%%
%%%   {set,[Range]}
%%%
%%% A range is represented as:
%%%
%%%   Range = {a_range,UpperBound} | {range,LowerBound,UpperBound}
%%%
%%% We don't use the atom 'MIN' to represent MIN, because atoms
%%% compare higher than integer. Instead we use {a_range,UpperBound}
%%% to represent MIN..UpperBound. We represent MAX as 'MAX' because
%%% 'MAX' compares higher than any integer.
%%%
%%% The ranges are sorted in term order. The ranges must not overlap
%%% or be adjacent to each other. This invariant is established when
%%% creating sets, and maintained by the intersection and union
%%% operators.
%%%
%%% Example of invalid set representaions:
%%%
%%%   [{range,0,10},{range,5,10}]    %Overlapping ranges
%%%   [{range,0,5},{range,6,10}]     %Adjancent ranges
%%%   [{range,10,20},{a_range,100}]  %Not sorted
%%%

make_constr_set(_, 'MIN', Ub) ->
    {set,[{a_range,make_constr_set_val(Ub)}]};
make_constr_set(_, Lb, Ub) when Lb =< Ub ->
    {set,[{range,make_constr_set_val(Lb),
	   make_constr_set_val(Ub)}]};
make_constr_set(S, _, _) ->
    asn1_error(S, reversed_range).

make_constr_set_val([C]) when is_integer(C) -> C;
make_constr_set_val(Val) -> Val.

make_constr_set_vs(Vs) ->
    {set,make_constr_set_vs_1(Vs)}.

make_constr_set_vs_1([]) ->
    [];
make_constr_set_vs_1([V]) ->
    [{range,V,V}];
make_constr_set_vs_1([V0|Vs]) ->
    V1 = make_constr_set_vs_1(Vs),
    range_union([{range,V0,V0}], V1).

%%%
%%% Set operators.
%%%

cs_intersection(_S, Other, none) ->
    Other;
cs_intersection(_S, none, Other) ->
    Other;
cs_intersection(_S, {set,SetA}, {set,SetB}) ->
    {set,range_intersection(SetA, SetB)};
cs_intersection(_S, A, B) ->
    {intersection,A,B}.

range_intersection([], []) ->
    [];
range_intersection([_|_], []) ->
    [];
range_intersection([], [_|_]) ->
    [];
range_intersection([H1|_]=A, [H2|_]=B) when H1 > H2 ->
    range_intersection(B, A);
range_intersection([H1|T1], [H2|T2]=B) ->
    %% Now H1 =< H2.
    case {H1,H2} of
	{{a_range,Ub0},{a_range,Ub1}} when Ub0 < Ub1 ->
	    %% Ub0 =/= 'MAX'
	    [H1|range_intersection(T1, [{range,Ub0+1,Ub1}|T2])];
	{{a_range,_},{a_range,_}} ->
	    %% Must be equal.
	    [H1|range_intersection(T1, T2)];
	{{a_range,Ub0},{range,Lb1,_Ub1}} when Ub0 < Lb1 ->
	    %% No intersection.
	    range_intersection(T1, B);
	{{a_range,Ub0},{range,Lb1,Ub1}} when Ub0 < Ub1 ->
	    %% Ub0 =/= 'MAX'
	    [{range,Lb1,Ub0}|range_intersection(T1, [{range,Ub0+1,Ub1}|T2])];
	{{a_range,Ub},{range,_Lb1,Ub}} ->
	    %% The first range covers the second range, but does not
	    %% go beyond. We handle this case specially because Ub may
	    %% be 'MAX', and evaluating 'MAX'+1 will fail.
	    [H2|range_intersection(T1, T2)];
	{{a_range,Ub0},{range,_Lb1,Ub1}} ->
	    %% Ub0 > Ub1, Ub1 =/= 'MAX'. The first range completely
	    %% covers and extends beyond the second range.
	    [H2|range_intersection([{range,Ub1+1,Ub0}|T1], T2)];
	{{range,_Lb0,Ub0},{range,Lb1,_Ub1}} when Ub0 < Lb1 ->
	    %% Lb0 < Lb1. No intersection.
	    range_intersection(T1, B);
	{{range,_Lb0,Ub0},{range,Lb1,Ub1}} when Ub0 < Ub1 ->
	    %% Ub0 >= Lb1, Ub0 =/= 'MAX'. Partial overlap.
	    [{range,Lb1,Ub0}|range_intersection(T1, [{range,Ub0+1,Ub1}|T2])];
	{{range,_Lb0,Ub},{range,_Lb1,Ub}} ->
	    %% The first range covers the second range, but does not
	    %% go beyond. We handle this case specially because Ub may
	    %% be 'MAX', and evaluating 'MAX'+1 will fail.
	    [H2|range_intersection(T1, T2)];
	{{range,_Lb0,Ub0},{range,_Lb1,Ub1}} ->
	    %% Ub1 =/= MAX. The first range completely covers and
	    %% extends beyond the second.
	    [H2|range_intersection([{range,Ub1+1,Ub0}|T1], T2)]
    end.

cs_union(_S, {set,SetA}, {set,SetB}) ->
    {set,range_union(SetA, SetB)};
cs_union(_S, A, B) ->
    {union,A,B}.

range_union(A, B) ->
    range_union_1(lists:merge(A, B)).

range_union_1([{a_range,Ub0},{a_range,Ub1}|T]) ->
    range_union_1([{a_range,max(Ub0, Ub1)}|T]);
range_union_1([{a_range,Ub0},{range,Lb1,Ub1}|T]) when Lb1-1 =< Ub0 ->
    range_union_1([{a_range,max(Ub0, Ub1)}|T]);
range_union_1([{a_range,_}=H|T]) ->
    %% Ranges are disjoint.
    [H|range_union_1(T)];
range_union_1([{range,Lb0,Ub0},{range,Lb1,Ub1}|T]) when Lb1-1 =< Ub0 ->
    range_union_1([{range,Lb0,max(Ub0, Ub1)}|T]);
range_union_1([{range,_,_}=H|T]) ->
    %% Ranges are disjoint.
    [H|range_union_1(T)];
range_union_1([]) ->
    [].

%%%
%%% Finish up constrains, making them suitable for the back-ends.
%%%
%%% A 'PermittedAlphabet' (FROM) constraint will be reduced to:
%%%
%%%   {'SingleValue',[integer()]}
%%%
%%% A 'SizeConstraint' (SIZE) constraint will be reduced to:
%%%
%%%   {Lb,Ub}
%%%
%%% All other constraints will be reduced to:
%%%
%%%   {'SingleValue',[integer()]} | {'ValueRange',Lb,Ub}
%%%

finish_constraints(Cs) ->
    finish_constraints_1(Cs, fun smart_collapse/1).

finish_constraints_1([{element_set,{Tag,{element_set,_,_}=Set0},none}|T],
		     Collapse0) ->
    Collapse = collapse_fun(Tag),
    case finish_constraints_1([Set0], Collapse) of
	[] ->
	    finish_constraints_1(T, Collapse0);
	[Set] ->
	    [{Tag,Set}|finish_constraints_1(T, Collapse0)]
    end;
finish_constraints_1([{element_set,{set,[{a_range,'MAX'}]},_}|T], Collapse) ->
    finish_constraints_1(T, Collapse);
finish_constraints_1([{element_set,{intersection,A0,B0},none}|T], Collapse) ->
    A = {element_set,A0,none},
    B = {element_set,B0,none},
    finish_constraints_1([A,B|T], Collapse);
finish_constraints_1([{element_set,Root,Ext}|T], Collapse) ->
    case finish_constraint(Root, Ext, Collapse) of
	none ->
	    finish_constraints_1(T, Collapse);
	Constr ->
	    [Constr|finish_constraints_1(T, Collapse)]
    end;
finish_constraints_1([H|T], Collapse) ->
    [H|finish_constraints_1(T, Collapse)];
finish_constraints_1([], _) ->
    [].

finish_constraint({set,Root0}, Ext, Collapse) ->
    case Collapse(Root0) of
	none -> none;
	Root -> finish_constraint(Root, Ext, Collapse)
    end;
finish_constraint(Root, Ext, _Collapse) ->
    case Ext of
	none -> Root;
	_ -> {Root,[]}
    end.

collapse_fun('SizeConstraint') ->
    fun size_constraint_collapse/1;
collapse_fun('PermittedAlphabet') ->
    fun single_value_collapse/1.

single_value_collapse(V) ->
    {'SingleValue',ordsets:from_list(single_value_collapse_1(V))}.

single_value_collapse_1([{range,Lb,Ub}|T]) when is_integer(Lb),
					      is_integer(Ub) ->
    lists:seq(Lb, Ub) ++ single_value_collapse_1(T);
single_value_collapse_1([]) ->
    [].

smart_collapse([{a_range,Ub}]) ->
    {'ValueRange',{'MIN',Ub}};
smart_collapse([{a_range,_}|T]) ->
    {range,_,Ub} = lists:last(T),
    {'ValueRange',{'MIN',Ub}};
smart_collapse([{range,Lb,Ub}]) ->
    {'ValueRange',{Lb,Ub}};
smart_collapse([_|_]=L) ->
    V = lists:foldr(fun({range,Lb,Ub}, A) ->
			    seq(Lb, Ub) ++ A
		    end, [], L),
    {'SingleValue',V}.

size_constraint_collapse([{range,0,'MAX'}]) ->
    none;
size_constraint_collapse(Root) ->
    [{range,Lb,_}|_] = Root,
    {range,_,Ub} = lists:last(Root),
    {Lb,Ub}.

seq(Same, Same) ->
    [Same];
seq(Lb, Ub) when is_integer(Lb), is_integer(Ub) ->
    lists:seq(Lb, Ub).

%%%-----------------------------------------
%% If the constraint value is a defined value the valuename
%% is replaced by the actual value
%%
resolve_value(S, HostType, Val) ->
    Id = match_parameter(S, Val),
    resolve_value1(S, HostType, Id).

resolve_value1(S, HostType, #'Externalvaluereference'{value=Name}=ERef) ->
    case resolve_namednumber(S, HostType, Name) of
	V when is_integer(V) ->
	    V;
	not_named ->
	    resolve_value1(S, HostType, get_referenced_value(S, ERef))
    end;
resolve_value1(S, HostType, {gt,V}) ->
    case resolve_value1(S, HostType, V) of
	Int when is_integer(Int) ->
	    Int + 1;
	_Other ->
	    asn1_error(S, illegal_integer_value)
    end;
resolve_value1(S, HostType, {lt,V}) ->
    case resolve_value1(S, HostType, V) of
	Int when is_integer(Int) ->
	    Int - 1;
	_Other ->
	    asn1_error(S, illegal_integer_value)
    end;
resolve_value1(S, _HostType, {'ValueFromObject',{object,Object},FieldName}) ->
    get_value_from_object(S, Object, FieldName);
resolve_value1(_, _, #valuedef{checked=true,value=V}) ->
    V;
resolve_value1(S, _, #valuedef{value={'ValueFromObject',
				     {object,Object},FieldName}}) ->
    get_value_from_object(S, Object, FieldName);
resolve_value1(S, _HostType, #valuedef{}=VDef) ->
    #valuedef{value=Val} = check_value(S,VDef),
    Val;
resolve_value1(_, _, V) ->
    V.

resolve_namednumber(S, #type{def=Def}, Name) ->
    case Def of
	{'ENUMERATED',NameList} ->
	    resolve_namednumber_1(S, Name, NameList);
	{'INTEGER',NameList} ->
	    resolve_namednumber_1(S, Name, NameList);
	_ ->
	    not_named
    end.

resolve_namednumber_1(S, Name, NameList) ->
    try
	NamedNumberList = check_enumerated(S, NameList),
	{_,N} = lookup_enum_value(S, Name, NamedNumberList),
	N
    catch _:_ ->
	    not_named
    end.

%%%
%%% End of constraint handling.
%%%

check_imported(S,Imodule,Name) ->
    check_imported(S,Imodule,Name,false).
check_imported(S,Imodule,Name,IsParsed) ->
    case asn1_db:dbget(Imodule,'MODULE') of
	undefined when IsParsed == true ->
	    ErrStr = io_lib:format("Type ~s imported from non existing module ~s~n",[Name,Imodule]),
	    error({imported,ErrStr,S});
	undefined ->
	    parse_and_save(S,Imodule),
	    check_imported(S,Imodule,Name,true);
	Im when is_record(Im,module) ->
	    case is_exported(Im,Name) of
		false ->
		    ErrStr = io_lib:format("Imported type ~s not exported from module ~s~n",[Name,Imodule]),
		    error({imported,ErrStr,S});
		_ ->
		    ok
	    end
    end,
    ok.

is_exported(Module,Name) when is_record(Module,module) ->
    {exports,Exports} = Module#module.exports,
    case Exports of
	all ->
	    true;
	[] ->
	    false;
	L when is_list(L) -> 
	    case lists:keysearch(Name,#'Externaltypereference'.type,Exports) of
		false -> false;
		_ -> true
	    end
    end.
    

check_externaltypereference(S,Etref=#'Externaltypereference'{module=Emod})->
    Currmod = S#state.mname,
    MergedMods = S#state.inputmodules,
    case Emod of
	Currmod ->
	    %% reference to current module or to imported reference
		check_reference(S,Etref);
	 _ ->
	    %% io:format("Type ~s IMPORTED FROM ~s~n",[Etype,Emod]),
	    case lists:member(Emod,MergedMods) of
		true ->
		    check_reference(S,Etref);
		false ->
		    {NewMod,_} = get_referenced_type(S,Etref),
		    Etref#'Externaltypereference'{module=NewMod}
	    end
    end.

check_reference(S,#'Externaltypereference'{pos=Pos,module=Emod,type=Name}) ->
    ModName = S#state.mname,
    case asn1_db:dbget(ModName,Name) of
	undefined ->
	    case imported(S,Name) of
		{ok,Imodule} ->
		    check_imported(S,Imodule,Name),
		    #'Externaltypereference'{module=Imodule,type=Name};
%% 		    case check_imported(S,Imodule,Name) of
%% 			ok ->
%% 			    #'Externaltypereference'{module=Imodule,type=Name};
%% 			Err ->
%% 			    Err
%% 		    end;
		_ ->
		    %may be a renamed type in multi file compiling!
		    {M,T}=get_renamed_reference(S,Name,Emod),
		    NewName = asn1ct:get_name_of_def(T),
		    NewPos = asn1ct:get_pos_of_def(T),
		    #'Externaltypereference'{pos=NewPos,
					     module=M,
					     type=NewName}
	    end;
	_ ->
	    %% cannot do check_type here due to recursive definitions, like
	    %% S ::= SEQUENCE {a INTEGER, b S}. This implies that references
	    %% that appear before the definition will be an 
	    %% Externaltypereference in the abstract syntax tree
	    #'Externaltypereference'{pos=Pos,module=ModName,type=Name}
    end.

get_referenced_value(S, T) ->
    case get_referenced_type(S, T) of
	{ExtMod,#valuedef{value=#'Externalvaluereference'{}=Ref}} ->
	    get_referenced_value(update_state(S, ExtMod), Ref);
	{_,#valuedef{value=Val}} ->
	    Val
    end.

get_referenced_type(S, T) ->
    get_referenced_type(S, T, false).

get_referenced_type(S, T, Recurse) ->
    case do_get_referenced_type(S, T) of
	{_,#typedef{typespec=#type{def=#'Externaltypereference'{}=ERef}}}
	  when Recurse ->
	    get_referenced_type(S, ERef, Recurse);
	{_,_}=Res ->
	    Res
    end.

do_get_referenced_type(S, T0) ->
    case match_parameter(S, T0) of
	T0 ->
	    do_get_ref_type_1(S, T0);
	T ->
	    do_get_referenced_type(S, T)
    end.

do_get_ref_type_1(S, #'Externaltypereference'{pos=P,
					      module=M,
					      type=T}) ->
    do_get_ref_type_2(S, P, M, T);
do_get_ref_type_1(S, #'Externalvaluereference'{pos=P,
					       module=M,
					       value=V}) ->
    do_get_ref_type_2(S, P, M, V);
do_get_ref_type_1(_, T) ->
    {undefined,T}.

do_get_ref_type_2(#state{mname=Current,inputmodules=Modules}=S,
		  Pos, M, T) ->
    case M =:= Current orelse lists:member(M, Modules) of
	true ->
	    get_referenced1(S, M, T, Pos);
	false ->
	    get_referenced(S, M, T, Pos)
    end.

%% get_referenced/3
%% The referenced entity Ename may in case of an imported parameterized
%% type reference imported entities in the other module, which implies that
%% asn1_db:dbget will fail even though the referenced entity exists. Thus
%% Emod may be the module that imports the entity Ename and not holds the
%% data about Ename. 
get_referenced(S,Emod,Ename,Pos) ->
    ?dbg("get_referenced: ~p~n",[Ename]),
    parse_and_save(S,Emod),
    ?dbg("get_referenced,parse_and_save ~n",[]),
    case asn1_db:dbget(Emod,Ename) of
	undefined ->
	    %% May be an imported entity in module Emod or Emod may not exist
	    case asn1_db:dbget(Emod,'MODULE') of
		undefined ->
		    asn1_error(S, {undefined_import, Ename, Emod});
		_ ->
		    NewS = update_state(S,Emod),
		    get_imported(NewS,Ename,Emod,Pos)
	    end;
	T when is_record(T,typedef) ->
	    ?dbg("get_referenced T: ~p~n",[T]),
	    {Emod,T};	    % should add check that T is exported here
	V ->
	    ?dbg("get_referenced V: ~p~n",[V]),
	    {Emod,V}
    end.

get_referenced1(S,ModuleName,Name,Pos) ->
    case asn1_db:dbget(S#state.mname,Name) of
	undefined ->
	    %% ModuleName may be other than S#state.mname when 
	    %% multi file compiling is used.
	    get_imported(S,Name,ModuleName,Pos);
	T ->
	    {S#state.mname,T}
    end.

get_imported(S,Name,Module,Pos) ->
    ?dbg("get_imported, Module: ~p, Name: ~p~n",[Module,Name]),
    case imported(S,Name) of
	{ok,Imodule} ->
	    parse_and_save(S,Imodule),
	    case asn1_db:dbget(Imodule,'MODULE') of
		undefined ->
		    asn1_error(S, {undefined_import, Name, Module});
		Im when is_record(Im,module) ->
		    case is_exported(Im,Name) of
			false ->
			    asn1_error(S, {undefined_export, Name});
			_ ->
			    ?dbg("get_imported, is_exported ~p, ~p~n",[Imodule,Name]),
			    get_referenced_type(S,
						#'Externaltypereference'
						{module=Imodule,
						 type=Name,pos=Pos})
		    end
	    end;
	_ ->
	    get_renamed_reference(S,Name,Module)
    end.

save_object_set_instance(S,Name,ObjSetSpec) 
  when is_record(ObjSetSpec,'ObjectSet') ->
    NewObjSet = #typedef{checked=true,name=Name,typespec=ObjSetSpec},
    asn1_db:dbput(S#state.mname,Name,NewObjSet),
    case ObjSetSpec of
	#'ObjectSet'{uniquefname={unique,undefined}} ->
	    ok;
	_ ->
	    %% Should be generated iff 
	    %% ObjSpec#'ObjectSet'.uniquefname /= {unique,undefined}
	    ObjSetKey = {Name,objectset,NewObjSet},
	    %% asn1ct_gen:insert_once(parameterized_objects,ObjSetKey)
	    insert_once(S,parameterized_objects,ObjSetKey)
    end,
    #'Externaltypereference'{module=S#state.mname,type=Name}.
    
%% load_asn1_module do not check that the module is saved.
%% If get_referenced_type is called before the module must
%% be saved. 
load_asn1_module(#state{mname=M,module=Mod},M)->
    Mod;
load_asn1_module(_,M) ->
    asn1_db:dbget(M,'MODULE').

parse_and_save(S,Module) when is_record(S,state) ->
    Erule = S#state.erule,
    case asn1db_member(S,Erule,Module) of
	true ->
	    ok;
	_ ->
	    case asn1ct:parse_and_save(Module,S) of
		ok ->
		    save_asn1db_uptodate(S,Erule,Module);    
		Err ->
		    Err
	    end
    end.

asn1db_member(S,Erule,Module) ->
    Asn1dbUTL = get_asn1db_uptodate(S),
    lists:member({Erule,Module},Asn1dbUTL).

save_asn1db_uptodate(S,Erule,Module) ->
    Asn1dbUTL = get_asn1db_uptodate(S),
    Asn1dbUTL2 = lists:keydelete(Module,2,Asn1dbUTL),
    put_asn1db_uptodate([{Erule,Module}|Asn1dbUTL2]).
	    
get_asn1db_uptodate(S) ->
    case get(asn1db_uptodate) of
	undefined -> [{S#state.erule,S#state.mname}]; %initialize
	L -> L
    end.

put_asn1db_uptodate(L) ->
    put(asn1db_uptodate,L).

update_state(S,undefined) ->
    S;
update_state(S=#state{mname=ModuleName},ModuleName) ->
    S;
update_state(S,ModuleName) ->
    case lists:member(ModuleName,S#state.inputmodules) of
	true ->
	    S;
	_ ->
	    parse_and_save(S,ModuleName),
	    Mod = #module{} = asn1_db:dbget(ModuleName,'MODULE'),
	    S#state{mname=ModuleName,module=Mod}
    end.

get_renamed_reference(S,Name,Module) ->
    case renamed_reference(S,Name,Module) of
	undefined ->
	    asn1_error(S, {undefined, Name});
	NewTypeName when NewTypeName =/= Name ->
	    get_referenced1(S,Module,NewTypeName,undefined)
    end.
renamed_reference(S,#'Externaltypereference'{type=Name,module=Module}) ->
    case renamed_reference(S,Name,Module) of
	undefined ->
	    Name;
	Other ->
	    Other
    end.
renamed_reference(S,Name,Module) ->
    %% first check if there is a renamed type in this module
    %% second check if any type was imported with this name
    case asn1ct_table:exists(renamed_defs) of
	false -> undefined;
	true ->
	    case asn1ct_table:match(renamed_defs, {'$1',Name,Module}) of
		[] -> 
		    case asn1ct_table:exists(original_imports) of
			false ->
			    undefined;
			true  ->
			    case asn1ct_table:match(original_imports, {Module,'$1'}) of
				[] ->
				    undefined;
				[[ImportsList]] ->
				    case get_importmoduleoftype(ImportsList,Name) of
					undefined ->
					    undefined;
					NextMod ->
					    renamed_reference(S,Name,NextMod)
				    end
			    end
		    end;
		[[NewTypeName]] ->
		    NewTypeName
	    end
    end.

get_importmoduleoftype([I|Is],Name) ->
    Index = #'Externaltypereference'.type,
    case lists:keysearch(Name,Index,I#'SymbolsFromModule'.symbols) of
	{value,_Ref} ->
	    (I#'SymbolsFromModule'.module)#'Externaltypereference'.type;
	_ ->
	    get_importmoduleoftype(Is,Name)
    end;
get_importmoduleoftype([],_) ->
    undefined.
		     
match_parameters(S, Names) ->
    [match_parameter(S, Name) || Name <- Names].

match_parameter(#state{parameters=Ps}=S, Name) ->
    match_parameter(S, Name, Ps).

match_parameter(_S, Name, []) ->
    Name;
match_parameter(S, {valueset,{element_set,#type{}=Ts,none}}, Ps) ->
    match_parameter(S, {valueset,Ts}, Ps);
match_parameter(_S, #'Externaltypereference'{type=Name},
		[{#'Externaltypereference'{type=Name},NewName}|_T]) ->
    NewName;
match_parameter(_S, #'Externaltypereference'{type=Name},
		[{{_,#'Externaltypereference'{type=Name}},NewName}|_T]) ->
    NewName;
match_parameter(_S, #'Externalvaluereference'{value=Name},
		[{#'Externalvaluereference'{value=Name},NewName}|_T]) ->
    NewName;
match_parameter(_S, #'Externalvaluereference'{value=Name},
		[{{_,#'Externalvaluereference'{value=Name}},NewName}|_T]) ->
    NewName;
match_parameter(_S, #type{def=#'Externaltypereference'{module=M,type=Name}},
		[{#'Externaltypereference'{module=M,type=Name},Type}]) ->
    Type;
match_parameter(_S, {valueset,#type{def=#'Externaltypereference'{type=Name}}},
		[{{_,#'Externaltypereference'{type=Name}},
		  {valueset,#type{def=NewName}}}|_T]) ->
    NewName;
match_parameter(_S, {valueset,#type{def=#'Externaltypereference'{type=Name}}},
		[{{_,#'Externaltypereference'{type=Name}},
		  NewName=#type{def=#'Externaltypereference'{}}}|_T]) ->
    NewName#type.def;
match_parameter(_S, {valueset,#type{def=#'Externaltypereference'{type=Name}}},
		[{{_,#'Externaltypereference'{type=Name}},NewName}|_T]) ->
    NewName;
%% When a parameter is a parameterized element it has to be
%% instantiated now!
match_parameter(S, {valueset,T=#type{def={pt,_,_Args}}}, _Ps) ->
    try check_type(S,#typedef{name=S#state.tname,typespec=T},T) of
	#type{def=Ts} ->
	    Ts
    catch pobjectsetdef ->
 	    {_,ObjRef,_Params} = T#type.def,
 	    {_,ObjDef}=get_referenced_type(S,ObjRef),
	    %%ObjDef is a pvaluesetdef where the type field holds the class
	    ClassRef = 
		case ObjDef of
		    #pvaluesetdef{type=TDef} ->
			TDef#type.def;
		    #pobjectsetdef{class=ClRef} -> ClRef
		end,
	    %% The reference may not have the home module of the class
	    {HomeMod,_} = get_referenced_type(S,ClassRef),
	    RightClassRef = 
		ClassRef#'Externaltypereference'{module=HomeMod},

	    ObjectSet = #'ObjectSet'{class=RightClassRef,set=T},
 	    ObjSpec = check_object(S,#typedef{typespec=ObjectSet},ObjectSet),
	    Name = list_to_atom(asn1ct_gen:list2name([get_datastr_name(ObjDef)|S#state.recordtopname])),	 
	    save_object_set_instance(S,Name,ObjSpec)
    end;

%% same as previous, only depends on order of parsing
match_parameter(S, {valueset,{pos,{objectset,_,POSref},Args}}, Ps) ->
    match_parameter(S, {valueset,#type{def={pt,POSref,Args}}}, Ps);
match_parameter(S, Name, [_H|T]) ->
    %%io:format("match_parameter(~p,~p)~n",[Name,[H|T]]),
    match_parameter(S, Name, T).

imported(S,Name) ->
    {imports,Ilist} = (S#state.module)#module.imports,
    imported1(Name,Ilist).

imported1(Name,
	  [#'SymbolsFromModule'{symbols=Symlist,
				module=#'Externaltypereference'{type=ModuleName}}|T]) ->
    case lists:keysearch(Name,#'Externaltypereference'.type,Symlist) of
	{value,_V} ->
	    {ok,ModuleName};
	_ ->
	    imported1(Name,T)
    end;
imported1(_Name,[]) ->
    false.

%% Check the named number list for an INTEGER or a BIT STRING.
check_named_number_list(_S, []) ->
    [];
check_named_number_list(_S, [{_,_}|_]=NNL) ->
    %% The named number list has already been checked.
    NNL;
check_named_number_list(S, NNL0) ->
    %% Check that the names are unique.
    case check_unique(NNL0, 2) of
	[] ->
	    NNL1 = [{Id,resolve_valueref(S, Val)} || {'NamedNumber',Id,Val} <- NNL0],
	    NNL = lists:keysort(2, NNL1),
	    case check_unique(NNL, 2) of
		[] ->
		    NNL;
		[Val|_] ->
		    asn1_error(S, {value_reused,Val})
	    end;
	[H|_] ->
	    asn1_error(S, {namelist_redefinition,H})
    end.

resolve_valueref(S, #'Externalvaluereference'{} = T) ->
    get_referenced_value(S, T);
resolve_valueref(_, Val) when is_integer(Val) ->
    Val.

check_integer(S, NNL) ->
    check_named_number_list(S, NNL).

check_bitstring(S, NNL0) ->
    NNL = check_named_number_list(S, NNL0),
    _ = [asn1_error(S, {invalid_bit_number,Bit}) ||
	    {_,Bit} <- NNL, Bit < 0],
    NNL.

check_real(_S,_Constr) ->
    ok.
    
%% Check INSTANCE OF
%% check that DefinedObjectClass is of TYPE-IDENTIFIER class
%% If Constraint is empty make it the general INSTANCE OF type
%% If Constraint is not empty make an inlined type
%% convert INSTANCE OF to the associated type
check_instance_of(S,DefinedObjectClass,Constraint) ->
    check_type_identifier(S,DefinedObjectClass),
    iof_associated_type(S,Constraint).
    
check_type_identifier(S, Eref=#'Externaltypereference'{type=Class}) ->
    case get_referenced_type(S, Eref) of
	{_,#classdef{name='TYPE-IDENTIFIER'}} ->
	    ok;
	{_,#classdef{typespec=#'Externaltypereference'{}=NextEref}} ->
	    check_type_identifier(S, NextEref);
	{_,TD=#typedef{typespec=#type{def=#'Externaltypereference'{}}}} ->
	    check_type_identifier(S, (TD#typedef.typespec)#type.def);
	_ ->
	    asn1_error(S, {illegal_instance_of,Class})
    end.

iof_associated_type(S,[]) ->
    %% in this case encode/decode functions for INSTANCE OF must be
    %% generated
    case get(instance_of) of
	undefined ->
	    AssociateSeq = iof_associated_type1(S,[]),
	    Tag = [?TAG_CONSTRUCTED(?N_INSTANCE_OF)],
	    TypeDef=#typedef{checked=true,
			     name='INSTANCE OF',
			     typespec=#type{tag=Tag,
					    def=AssociateSeq}},
	    asn1_db:dbput(S#state.mname,'INSTANCE OF',TypeDef),
	    instance_of_decl(S#state.mname);
%%	    put(instance_of,{generate,S#state.mname});
	_ ->
	    instance_of_decl(S#state.mname),
	    ok
    end,
    #'Externaltypereference'{module=S#state.mname,type='INSTANCE OF'};
iof_associated_type(S,C) ->
    iof_associated_type1(S,C).

iof_associated_type1(S,C) ->
    {TableCInf,Comp1Cnstr,Comp2Cnstr,Comp2tablecinf}=
	instance_of_constraints(S,C),

    ModuleName = S#state.mname,
    Typefield_type=
	case C of
	    [] -> 'ASN1_OPEN_TYPE';
	    _ -> {typefield,'Type'}
	end,
    ObjIdTag = [{'UNIVERSAL',8}],
    C1TypeTag = [#tag{class='UNIVERSAL',
		      number=6,
		      type='IMPLICIT',
		      form=0}],
    TypeIdentifierRef=#'Externaltypereference'{module=ModuleName,
					       type='TYPE-IDENTIFIER'},
    ObjectIdentifier =
	#'ObjectClassFieldType'{classname=TypeIdentifierRef,
				class=[],
%%				fieldname=[{valuefieldreference,id}],
				fieldname={id,[]},
				type={fixedtypevaluefield,id,
				      #type{def='OBJECT IDENTIFIER'}}},
    Typefield =
	#'ObjectClassFieldType'{classname=TypeIdentifierRef,
				class=[],
%%				fieldname=[{typefieldreference,'Type'}],
				fieldname={'Type',[]},
				type=Typefield_type},
    IOFComponents =
	[#'ComponentType'{name='type-id',
			  typespec=#type{tag=C1TypeTag,
					 def=ObjectIdentifier,
					 constraint=Comp1Cnstr},
			  prop=mandatory,
			  tags=ObjIdTag},
	 #'ComponentType'{name=value,
			  typespec=#type{tag=[#tag{class='CONTEXT',
						   number=0,
						   type='EXPLICIT',
						   form=32}],
					 def=Typefield,
					 constraint=Comp2Cnstr,
					 tablecinf=Comp2tablecinf},
			  prop=mandatory,
			  tags=[{'CONTEXT',0}]}],
    #'SEQUENCE'{tablecinf=TableCInf,
		components=simplify_comps(IOFComponents)}.
	   

%% returns the leading attribute, the constraint of the components and
%% the tablecinf value for the second component.
instance_of_constraints(_, []) ->
    {false,[],[],[]};
instance_of_constraints(S, [{element_set,{simpletable,C},none}]) ->
    {element_set,Type,none} = C,
    instance_of_constraints_1(S, Type).

instance_of_constraints_1(S, Type) ->
    #type{def=#'Externaltypereference'{type=Name}} = Type,
    ModuleName = S#state.mname,
    ObjectSetRef=#'Externaltypereference'{module=ModuleName,
					  type=Name},
    CRel=[{componentrelation,{objectset,
			      undefined, %% pos
			      ObjectSetRef},
			      [{innermost,
				[#'Externalvaluereference'{module=ModuleName,
							   value=type}]}]}],
    Mod = S#state.mname,
    TableCInf=#simpletableattributes{objectsetname={Mod,Name},
				     c_name='type-id',
				     c_index=1,
				     usedclassfield=id,
				     uniqueclassfield=id,
				     valueindex=[]},
    {TableCInf,[{simpletable,Name}],CRel,[{objfun,ObjectSetRef}]}.

%%%
%%% Check ENUMERATED.
%%%

check_enumerated(_S, [{Name,Number}|_]=NNL)
  when is_atom(Name), is_integer(Number) ->
    %% Already checked.
    NNL;
check_enumerated(_S, {[{Name,Number}|_],L}=NNL)
  when is_atom(Name), is_integer(Number), is_list(L) ->
    %% Already checked (with extension).
    NNL;
check_enumerated(S, NNL) ->
    check_enum_ids(S, NNL, gb_sets:empty()),
    check_enum(S, NNL, gb_sets:empty(), []).

check_enum_ids(S, [{'NamedNumber',Id,_}|T], Ids0) ->
    Ids = check_enum_update_ids(S, Id, Ids0),
    check_enum_ids(S, T, Ids);
check_enum_ids(S, ['EXTENSIONMARK'|T], Ids) ->
    check_enum_ids(S, T, Ids);
check_enum_ids(S, [Id|T], Ids0) when is_atom(Id) ->
    Ids = check_enum_update_ids(S, Id, Ids0),
    check_enum_ids(S, T, Ids);
check_enum_ids(_, [], _) ->
    ok.

check_enum(S, [{'NamedNumber',Id,N}|T], Used0, Acc) ->
    Used = check_enum_update_used(S, Id, N, Used0),
    check_enum(S, T, Used, [{Id,N}|Acc]);
check_enum(S, ['EXTENSIONMARK'|Ext0], Used0, Acc0) ->
    Acc = lists:reverse(Acc0),
    {Root,Used,Cnt} = check_enum_number_root(Acc, Used0, 0, []),
    Ext = check_enum_ext(S, Ext0, Used, Cnt, []),
    {Root,Ext};
check_enum(S, [Id|T], Used, Acc) when is_atom(Id) ->
    check_enum(S, T, Used, [Id|Acc]);
check_enum(_, [], Used, Acc0) ->
    Acc = lists:reverse(Acc0),
    {Root,_,_} = check_enum_number_root(Acc, Used, 0, []),
    lists:keysort(2, Root).

check_enum_number_root([Id|T]=T0, Used0, Cnt, Acc) when is_atom(Id) ->
    case gb_sets:is_element(Cnt, Used0) of
	false ->
	    Used = gb_sets:insert(Cnt, Used0),
	    check_enum_number_root(T, Used, Cnt+1, [{Id,Cnt}|Acc]);
	true ->
	    check_enum_number_root(T0, Used0, Cnt+1, Acc)
    end;
check_enum_number_root([H|T], Used, Cnt, Acc) ->
    check_enum_number_root(T, Used, Cnt, [H|Acc]);
check_enum_number_root([], Used, Cnt, Acc) ->
    {lists:keysort(2, Acc),Used,Cnt}.

check_enum_ext(S, [{'NamedNumber',Id,N}|T], Used0, C, Acc) ->
    Used = check_enum_update_used(S, Id, N, Used0),
    if
	N < C ->
	    asn1_error(S, {enum_not_ascending,Id,N,C-1});
	true ->
	    ok
    end,
    check_enum_ext(S, T, Used, N+1, [{Id,N}|Acc]);
check_enum_ext(S, [Id|T]=T0, Used0, C, Acc) when is_atom(Id) ->
    case gb_sets:is_element(C, Used0) of
	true ->
	    check_enum_ext(S, T0, Used0, C+1, Acc);
	false ->
	    Used = gb_sets:insert(C, Used0),
	    check_enum_ext(S, T, Used, C+1, [{Id,C}|Acc])
    end;
check_enum_ext(_, [], _, _, Acc) ->
    lists:keysort(2, Acc).

check_enum_update_ids(S, Id, Ids) ->
    case gb_sets:is_element(Id, Ids) of
	false ->
	    gb_sets:insert(Id, Ids);
	true ->
	    asn1_error(S, {enum_illegal_redefinition,Id})
    end.

check_enum_update_used(S, Id, N, Used) ->
    case gb_sets:is_element(N, Used) of
	false ->
	    gb_sets:insert(N, Used);
	true ->
	    asn1_error(S, {enum_reused_value,Id,N})
    end.

%%%
%%% End of ENUMERATED checking.
%%%

check_boolean(_S,_Constr) ->
    ok.

check_octetstring(_S,_Constr) ->
    ok.

% check all aspects of a SEQUENCE
% - that all component names are unique
% - that all TAGS are ok (when TAG default is applied)
% - that each component is of a valid type
% - that the extension marks are valid

check_sequence(S,Type,Comps)  ->
    Components = expand_components(S,Comps),    
    case check_unique([C||C <- Components ,is_record(C,'ComponentType')]
		      ,#'ComponentType'.name) of
	[] ->
	    %% sort_canonical(Components),
	    Components2 = maybe_automatic_tags(S,Components),
	    %% check the table constraints from here. The outermost type
	    %% is Type, the innermost is Comps (the list of components)
	    NewComps = check_each_component2(S,Type,Components2),
	    check_unique_sequence_tags(S,NewComps),

	    %% CRelInf is the "leading attribute" information
	    %% necessary for code generating of the look up in the
	    %% object set table,
	    %% i.e. getenc_ObjectSet/getdec_ObjectSet.
	    %% {objfun,ERef} tuple added in NewComps2 in tablecinf
	    %% field in type record of component relation constrained
	    %% type
	    {CRelInf,NewComps2} = componentrelation_leadingattr(S,NewComps),

	    %% CompListWithTblInf has got a lot unecessary info about
	    %% the involved class removed, as the class of the object
	    %% set.
	    CompListWithTblInf = get_tableconstraint_info(S,Type,NewComps2),

	    NewComps3 = textual_order(CompListWithTblInf),
	    NewComps4 = simplify_comps(NewComps3),
	    CompListTuple = complist_as_tuple(NewComps4),
	    {CRelInf,CompListTuple};
	Dupl ->
	    asn1_error(S, {duplicate_identifier, error_value(hd(Dupl))})
    end.

complist_as_tuple(CompList) ->
    complist_as_tuple(CompList, [], [], [], root).

complist_as_tuple([#'EXTENSIONMARK'{}|T], Acc, Ext, Acc2, root) ->
    complist_as_tuple(T, Acc, Ext, Acc2, ext);
complist_as_tuple([#'EXTENSIONMARK'{}|T], Acc, Ext, Acc2, ext) ->
    complist_as_tuple(T, Acc, Ext, Acc2, root2);
complist_as_tuple([C|T], Acc, Ext, Acc2, root) ->
    complist_as_tuple(T, [C|Acc], Ext, Acc2, root);
complist_as_tuple([C|T], Acc, Ext, Acc2, ext) ->
    complist_as_tuple(T, Acc, [C|Ext], Acc2, ext);
complist_as_tuple([C|T], Acc, Ext, Acc2, root2) ->
    complist_as_tuple(T, Acc, Ext, [C|Acc2], root2);
complist_as_tuple([], Acc, _Ext, _Acc2, root) ->
    lists:reverse(Acc);
complist_as_tuple([], Acc, Ext, _Acc2, ext) ->
    {lists:reverse(Acc),lists:reverse(Ext)};
complist_as_tuple([], Acc, Ext, Acc2, root2) ->
    {lists:reverse(Acc),lists:reverse(Ext),lists:reverse(Acc2)}.

expand_components(S, [{'COMPONENTS OF',Type}|T]) ->
    CompList = expand_components2(S,get_referenced_type(S,Type#type.def)),
    expand_components(S,CompList) ++ expand_components(S,T);
expand_components(S,[H|T]) ->
    [H|expand_components(S,T)];
expand_components(_,[]) ->
    [].
expand_components2(_S,{_,#typedef{typespec=#type{def=Seq}}}) 
  when is_record(Seq,'SEQUENCE') ->
    case Seq#'SEQUENCE'.components of
	{R1,_Ext,R2} -> R1 ++ R2;
	{Root,_Ext} -> Root;
	Root -> take_only_rootset(Root)
    end;
expand_components2(_S,{_,#typedef{typespec=#type{def=Set}}})
  when is_record(Set,'SET') ->
    case Set#'SET'.components of
	{R1,_Ext,R2} -> R1 ++ R2;
	{Root,_Ext} -> Root;
	Root -> take_only_rootset(Root)
    end;
expand_components2(_S,{_,#typedef{typespec=RefType=#type{def=#'Externaltypereference'{}}}}) ->
    [{'COMPONENTS OF',RefType}];
expand_components2(S,{_,PT={pt,_,_}}) ->
    PTType = check_type(S,PT,#type{def=PT}),
    expand_components2(S,{dummy,#typedef{typespec=PTType}});
expand_components2(S,{_,OCFT = #'ObjectClassFieldType'{}}) ->
    UncheckedType = #type{def=OCFT},
    Type = check_type(S,#typedef{typespec=UncheckedType},UncheckedType),
    expand_components2(S, {undefined,ocft_def(Type)});
expand_components2(S,{_,ERef}) when is_record(ERef,'Externaltypereference') ->
    expand_components2(S,get_referenced_type(S,ERef));
expand_components2(S,{_, What}) ->
    asn1_error(S, {illegal_COMPONENTS_OF, error_value(What)}).

take_only_rootset([])->
    [];
take_only_rootset([#'EXTENSIONMARK'{}|_T])->
    [];
take_only_rootset([H|T]) ->
    [H|take_only_rootset(T)].

check_unique_sequence_tags(S,CompList) ->
    TagComps = case complist_as_tuple(CompList) of
		   {R1,Ext,R2} ->
		       R1 ++ [C#'ComponentType'{prop='OPTIONAL'}||
				 C = #'ComponentType'{} <- Ext]++R2;
		   {R1,Ext} ->
		       R1 ++ [C#'ComponentType'{prop='OPTIONAL'}||
				 C = #'ComponentType'{} <- Ext];
		   _ ->
		       CompList
	       end,
    check_unique_sequence_tags0(S,TagComps).

check_unique_sequence_tags0(S,[#'ComponentType'{prop=mandatory}|Rest]) ->
    check_unique_sequence_tags0(S,Rest);
check_unique_sequence_tags0(S,[C=#'ComponentType'{}|Rest]) ->
    check_unique_sequence_tags1(S,Rest,[C]);% optional or default
check_unique_sequence_tags0(S,[_ExtensionMarker|Rest]) ->
    check_unique_sequence_tags0(S,Rest);
check_unique_sequence_tags0(_S,[]) ->
    true.

check_unique_sequence_tags1(S,[C|Rest],Acc) when is_record(C,'ComponentType') ->
    case C#'ComponentType'.prop of
	mandatory ->
	    check_unique_tags(S,lists:reverse([C|Acc])),
	    check_unique_sequence_tags(S,Rest);
	_  ->
	    check_unique_sequence_tags1(S,Rest,[C|Acc]) % default or optional
    end;
check_unique_sequence_tags1(S,[H|Rest],Acc) ->
    check_unique_sequence_tags1(S,Rest,[H|Acc]);
check_unique_sequence_tags1(S,[],Acc) ->
    check_unique_tags(S,lists:reverse(Acc)).

check_sequenceof(S,Type,Component) when is_record(Component,type) ->
    simplify_type(check_type(S, Type, Component)).

check_set(S,Type,Components) ->
    {TableCInf,NewComponents} = check_sequence(S,Type,Components),
    check_unique_tags(S, collect_components(NewComponents), []),
    case {lists:member(der,S#state.options),S#state.erule} of
	{true,_} ->
	    {Sorted,SortedComponents} = sort_components(der,S,NewComponents),
	    {Sorted,TableCInf,SortedComponents};
	{_,PER} when PER =:= per; PER =:= uper ->
	    {Sorted,SortedComponents} = sort_components(per,S,NewComponents),
	    {Sorted,TableCInf,SortedComponents};
	_ ->
	    {false,TableCInf,NewComponents}
    end.

collect_components({C1,C2,C3}) ->
    collect_components(C1++C2++C3);
collect_components({C1,C2}) ->
    collect_components(C1++C2);
collect_components(Cs) ->
    %% Assert that tags are not empty
    [] = [EmptyTag || EmptyTag = #'ComponentType'{tags=[]} <- Cs],
    Cs.

%% sorting in canonical order according to X.680 8.6, X.691 9.2
%% DER: all components shall be sorted in canonical order.
%% PER: only root components shall be sorted in canonical order. The
%%      extension components shall remain in textual order.
%%
sort_components(der, S, Components) ->
    {R1,Ext,R2} = extension(textual_order(Components)),
    CompsList = case Ext of
		    noext -> R1;
		    _ -> R1 ++ Ext ++ R2
		end,
    case {untagged_choice(S,CompsList),Ext} of
	{false,noext} ->
	    {true,sort_components1(CompsList)};
	{false,_} ->
	    {true,{sort_components1(CompsList),[]}};
	{true,noext} ->
	    %% sort in run-time
	    {dynamic,R1};
	_ ->
	    {dynamic,{R1, Ext, R2}}
    end;
sort_components(per, S, Components) ->
    {R1,Ext,R2} = extension(textual_order(Components)),
    Root = tag_untagged_choice(S,R1++R2),
    case Ext of
	noext ->
	    {true,sort_components1(Root)};
	_ ->
	    {true,{sort_components1(Root),Ext}}
    end.

sort_components1(Cs0) ->
    Cs1 = [{tag_key(Tag),C} || #'ComponentType'{tags=[Tag|_]}=C <- Cs0],
    Cs = lists:sort(Cs1),
    [C || {_,C} <- Cs].

tag_key({'UNIVERSAL',Tag}) ->   {0,Tag};
tag_key({'APPLICATION',Tag}) -> {1,Tag};
tag_key({'CONTEXT',Tag}) ->     {2,Tag};
tag_key({'PRIVATE',Tag}) ->     {3,Tag}.

untagged_choice(_S,[#'ComponentType'{typespec=#type{tag=[],def={'CHOICE',_}}}|_Rest]) ->
    true;
untagged_choice(S,[#'ComponentType'{typespec=#type{tag=[],def=ExRef}}|Rest])
  when is_record(ExRef,'Externaltypereference')->
    case get_referenced_type(S,ExRef) of
	{_,#typedef{typespec=#type{tag=[],
				   def={'CHOICE',_}}}} -> true;
	_ -> untagged_choice(S,Rest)
    end;
untagged_choice(S,[_|Rest]) ->
    untagged_choice(S,Rest);
untagged_choice(_,[]) ->
    false.
    
    
tag_untagged_choice(S,Cs) ->
    tag_untagged_choice(S,Cs,[]).
tag_untagged_choice(S,[C = #'ComponentType'{typespec=#type{tag=[],def={'CHOICE',_}}}|Rest],Acc) ->
    TagList = C#'ComponentType'.tags,
    TaggedC = C#'ComponentType'{tags=get_least_tag(TagList)},
    tag_untagged_choice(S,Rest,[TaggedC|Acc]);
tag_untagged_choice(S,[C = #'ComponentType'{typespec=#type{tag=[],def=ExRef}}|Rest],Acc) when is_record(ExRef,'Externaltypereference') ->
    case get_referenced_type(S,ExRef) of
	{_,#typedef{typespec=#type{tag=[],
				   def={'CHOICE',_}}}} -> 
	    TagList = C#'ComponentType'.tags,
	    TaggedC = C#'ComponentType'{tags = get_least_tag(TagList)},
	    tag_untagged_choice(S,Rest,[TaggedC|Acc]);
	_ -> 
	    tag_untagged_choice(S,Rest,[C|Acc])
    end;
tag_untagged_choice(S,[C|Rest],Acc) ->
    tag_untagged_choice(S,Rest,[C|Acc]);
tag_untagged_choice(_S,[],Acc) ->
    Acc.
get_least_tag([]) ->
    [];
get_least_tag(TagList) ->
    %% The smallest tag 'PRIVATE' < 'CONTEXT' < 'APPLICATION' < 'UNIVERSAL'
    Pred = fun({'PRIVATE',_},{'CONTEXT',_}) -> true;
	      ({'CONTEXT',_},{'APPLICATION',_}) -> true; 
	      ({'APPLICATION',_},{'UNIVERSAL',_}) -> true; 
	      ({A,T1},{A,T2}) when T1 =< T2 -> true; (_,_) -> false 
	   end,
    [T|_] = lists:sort(Pred,TagList),
    [T].

%% adds the textual order to the components to keep right order of
%% components in the asn1-value.
textual_order(Cs) ->
    Fun = fun(C=#'ComponentType'{},Index) ->
		  {C#'ComponentType'{textual_order=Index},Index+1};
	     (Other,Index) ->
		  {Other,Index}
	  end,
    {NewCs,_} = textual_order(Cs,Fun,1),
    NewCs.
textual_order(Cs,Fun,IxIn) when is_list(Cs) ->
    lists:mapfoldl(Fun,IxIn,Cs);
textual_order({Root,Ext},Fun,IxIn) ->
    {NewRoot,IxR} = textual_order(Root,Fun,IxIn),
    {NewExt,_} = textual_order(Ext,Fun,IxR),
    {{NewRoot,NewExt},dummy};
textual_order({Root1,Ext,Root2},Fun,IxIn) ->
    {NewRoot1,IxR} = textual_order(Root1,Fun,IxIn),
    {NewExt,IxE} = textual_order(Ext,Fun,IxR),
    {NewRoot2,_} = textual_order(Root2,Fun,IxE),
    {{NewRoot1,NewExt,NewRoot2},dummy}.

extension(Components) when is_list(Components) ->
    {Components,noext,[]};
extension({Root,ExtList}) ->
    ToOpt = fun(mandatory) ->
		    'OPTIONAL';
	       (X) -> X
	    end,
    {Root, [X#'ComponentType'{prop=ToOpt(Y)}||
	       X = #'ComponentType'{prop=Y}<-ExtList],[]};
extension({Root1,ExtList,Root2}) ->
    ToOpt = fun(mandatory) ->
		    'OPTIONAL';
	       (X) -> X
	    end,
    {Root1, [X#'ComponentType'{prop=ToOpt(Y)}||
		X = #'ComponentType'{prop=Y}<-ExtList], Root2}.

check_setof(S,Type,Component) when is_record(Component,type) ->
    simplify_type(check_type(S, Type, Component)).

check_selectiontype(S,Name,#type{def=Eref}) 
  when is_record(Eref,'Externaltypereference') ->
    {RefMod,TypeDef} = get_referenced_type(S,Eref),
    NewS = S#state{module=load_asn1_module(S,RefMod),
		   mname=RefMod,
		   tname=get_datastr_name(TypeDef)},
    check_selectiontype2(NewS,Name,TypeDef);
check_selectiontype(S,Name,Type=#type{def={pt,_,_}}) ->
    TName = case S#state.recordtopname of
		[] -> S#state.tname;
		N -> N
	    end,
    TDef = #typedef{name=TName,typespec=Type},
    check_selectiontype2(S,Name,TDef);
check_selectiontype(S, _Name, Type) ->
    asn1_error(S, {illegal_choice_type, error_value(Type)}).

check_selectiontype2(S,Name,TypeDef) ->
    NewS = S#state{recordtopname=get_datastr_name(TypeDef)},
    Components =
	try
	    CheckedType = check_type(NewS,TypeDef,TypeDef#typedef.typespec),
	    get_choice_components(S,CheckedType#type.def)
	catch error:_ ->
		asn1_error(S, {illegal_choice_type, error_value(TypeDef)})
	end,
    case lists:keyfind(Name, #'ComponentType'.name, Components) of
	#'ComponentType'{typespec=TS} -> TS;
	false -> asn1_error(S, {illegal_id, error_value(Name)})
    end.


get_choice_components(_S,{'CHOICE',Components}) when is_list(Components)->
    Components;
get_choice_components(_S,{'CHOICE',{C1,C2}}) when is_list(C1),is_list(C2) ->
    C1++C2;
get_choice_components(S,ERef=#'Externaltypereference'{}) ->
    {_RefMod,TypeDef}=get_referenced_type(S,ERef),
    #typedef{typespec=TS} = TypeDef,
    get_choice_components(S,TS#type.def).


	    
check_restrictedstring(_S,_Def,_Constr) ->
    ok.

check_objectidentifier(_S,_Constr) ->
    ok.

check_relative_oid(_S,_Constr) ->
    ok.
% check all aspects of a CHOICE
% - that all alternative names are unique
% - that all TAGS are ok (when TAG default is applied)
% - that each alternative is of a valid type
% - that the extension marks are valid
check_choice(S,Type,Components) when is_list(Components) ->
    Components1 = [C||C = #'ComponentType'{} <- Components],
    case check_unique(Components1,#'ComponentType'.name) of
	[] -> 
    %%    sort_canonical(Components),
	    Components2 = maybe_automatic_tags(S,Components),
	    NewComps = check_each_alternative2(S,Type,Components2),
	    %% ExtensionAdditionGroup markers i.e '[[' ']]' are not
	    %% significant for encoding/decoding a choice
	    %% therefore we remove them here
	    NewComps2 = lists:filter(fun(#'ExtensionAdditionGroup'{}) -> false;
					('ExtensionAdditionGroupEnd') -> false;
					(_) -> true
				     end,NewComps),
	    NewComps3 = simplify_comps(NewComps2),
	    check_unique_tags(S, NewComps3),
	    complist_as_tuple(NewComps3);
	Dupl ->
	    asn1_error(S, {duplicate_identifier,error_value(hd(Dupl))})
    end;
check_choice(_S,_,[]) -> 
    [].

maybe_automatic_tags(S,C) ->
    TagNos = tag_nums(C),
    case (S#state.module)#module.tagdefault of
	'AUTOMATIC' ->
	    generate_automatic_tags(S,C,TagNos);
	_ ->
	    %% maybe is the module a multi file module were only some of
	    %% the modules have defaulttag AUTOMATIC TAGS then the names
	    %% of those types are saved in the table automatic_tags
	    Name= S#state.tname,
	    case is_automatic_tagged_in_multi_file(Name) of
		true ->
		    generate_automatic_tags(S,C,TagNos);
		false ->
		    C
	    end
    end.

%% Pos == 1 for Root1, 2 for Ext, 3 for Root2
tag_nums(Cl) ->
    tag_nums(Cl,0,0).
tag_nums([#'EXTENSIONMARK'{}|Rest],Ext,Root2) ->
    tag_nums_ext(Rest,Ext,Root2);
tag_nums([_|Rest],Ext,Root2) ->
    tag_nums(Rest,Ext+1,Root2+1);
tag_nums([],Ext,Root2) ->
    [0,Ext,Root2].
tag_nums_ext([#'EXTENSIONMARK'{}|Rest],Ext,Root2) ->
    tag_nums_root2(Rest,Ext,Root2);
tag_nums_ext([_|Rest],Ext,Root2) ->
    tag_nums_ext(Rest,Ext,Root2);
tag_nums_ext([],Ext,_Root2) ->
    [0,Ext,0].
tag_nums_root2([_|Rest],Ext,Root2) ->
    tag_nums_root2(Rest,Ext+1,Root2);
tag_nums_root2([],Ext,Root2) ->
    [0,Ext,Root2].
    
is_automatic_tagged_in_multi_file(Name) ->
    case asn1ct_table:exists(automatic_tags) of
	false ->
	    %% this case when not multifile compilation
	    false;
	true ->
	    case asn1ct_table:lookup(automatic_tags, Name) of
		[] -> false;
		_ -> true
	    end
    end.

generate_automatic_tags(_S,C,TagNo) ->
    case any_manual_tag(C) of
	true ->
	    C;
	false ->
	    generate_automatic_tags1(C,TagNo)
    end.

generate_automatic_tags1([H|T],[TagNo|TagNos]) when is_record(H,'ComponentType') ->
    #'ComponentType'{typespec=Ts} = H,
    NewTs = Ts#type{tag=[#tag{class='CONTEXT',
			     number=TagNo,
			     type={default,'IMPLICIT'},
			     form= 0 }]}, % PRIMITIVE
    [H#'ComponentType'{typespec=NewTs}|generate_automatic_tags1(T,[TagNo+1|TagNos])];
generate_automatic_tags1([ExtMark = #'EXTENSIONMARK'{}|T],[_TagNo|TagNos]) -> 
    [ExtMark | generate_automatic_tags1(T,TagNos)];
generate_automatic_tags1([H|T],TagList) -> % ExtensionAdditionGroup etc are just ignored
    [H | generate_automatic_tags1(T,TagList)];
generate_automatic_tags1([],_) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns true if there is at least one ComponentType with a manually 
%% specified tag. No manual tag is indicated by typespec=#type{tag=[]}
%% so we check if we find a tag =/= [] and return true in that case
%% all other things in the componentlist like (EXTENSIONMARK, 
%% ExtensionAdditionGroup,...) except ComponentType is simply
%% ignored/skipped
any_manual_tag([#'ComponentType'{typespec=#type{tag=Tag}}|_Rest]) 
  when Tag =/= []->
    true;
any_manual_tag([_|Rest]) ->
    any_manual_tag(Rest);
any_manual_tag([]) ->
    false.


check_unique_tags(S,C) ->
    case (S#state.module)#module.tagdefault of
	'AUTOMATIC' ->
	    case any_manual_tag(C) of
		false ->
		    true;
		true ->
		    check_unique_tags(S, C, [])
	    end;
	_ ->
	    check_unique_tags(S, C, [])
    end.

check_unique_tags(S, [#'ComponentType'{name=Name,tags=Tags0}|T], Acc) ->
    Tags = [{Tag,Name} || Tag <- Tags0],
    check_unique_tags(S, T, Tags ++ Acc);
check_unique_tags(S, [_|T], Acc) ->
    check_unique_tags(S, T, Acc);
check_unique_tags(S, [], Acc) ->
    R0 = sofs:relation(Acc),
    R1 = sofs:relation_to_family(R0),
    R2 = sofs:to_external(R1),
    Dup = [Els || {_,[_,_|_]=Els} <- R2],
    case Dup of
	[] ->
	    ok;
	[FirstDupl|_] ->
	    asn1_error(S, {duplicate_tags,FirstDupl})
    end.

check_unique(L,Pos) ->
    Slist = lists:keysort(Pos,L),
    check_unique2(Slist,Pos,[]).

check_unique2([A,B|T],Pos,Acc) when element(Pos,A) == element(Pos,B) ->
    check_unique2([B|T],Pos,[element(Pos,B)|Acc]);
check_unique2([_|T],Pos,Acc) ->
    check_unique2(T,Pos,Acc);
check_unique2([],_,Acc) ->
    lists:reverse(Acc).


%% Replaces check_each_component and does the same work except that
%% it keeps the complist as a flat list and does not create a tuple with root and
%% extensions separated
check_each_component2(S,Type,Components) ->
    check_each_component2(S,Type,Components,[]).

check_each_component2(S = #state{abscomppath=Path,recordtopname=TopName},
		     Type,
		     [C = #'ComponentType'{name=Cname,typespec=Ts,prop=Prop}|Ct],
		     Acc) ->
    NewAbsCPath = 
	case Ts#type.def of
	    #'Externaltypereference'{} -> [];
	    _ -> [Cname|Path]
	end,%%XXX Cname = 'per-message-indicators'
    CheckedTs = check_type(S#state{abscomppath=NewAbsCPath,
				   recordtopname=[Cname|TopName]},Type,Ts),
    NewTags = get_taglist(S,CheckedTs),

    NewProp =
	case normalize_value(S,CheckedTs,Prop,[Cname|TopName]) of
	    mandatory -> mandatory;
	    'OPTIONAL' -> 'OPTIONAL';
	    DefaultValue -> {'DEFAULT',DefaultValue}
	end,
    NewC = C#'ComponentType'{typespec=CheckedTs,prop=NewProp,tags=NewTags},
    check_each_component2(S,Type,Ct,[NewC|Acc]);

check_each_component2(S,Type,[OtherMarker|Ct],Acc) ->
    %% let 'EXTENSIONMARK' and 'ExtensionAdditionGroup' markers pass through as is
    check_each_component2(S,Type,Ct,[OtherMarker|Acc]);
check_each_component2(_S,_,[],Acc) ->
    lists:reverse(Acc).


%% check_each_alternative2(S,Type,{Rlist,ExtList}) ->
%%     {check_each_alternative(S,Type,Rlist),
%%      check_each_alternative(S,Type,ExtList)};
check_each_alternative2(S,Type,[C|Ct]) ->
    check_each_alternative2(S,Type,[C|Ct],[]).

check_each_alternative2(S=#state{abscomppath=Path,recordtopname=TopName},
		       Type,
		       [C = #'ComponentType'{name=Cname,typespec=Ts}|Ct],
		       Acc) ->
    NewAbsCPath = 
	case Ts#type.def of
	    #'Externaltypereference'{} -> [];
	    _ -> [Cname|Path]
	end,
    CheckedTs = check_type(S#state{abscomppath=NewAbsCPath,
				   recordtopname=[Cname|TopName]},Type,Ts),
    NewTags = get_taglist(S,CheckedTs),

    NewC = C#'ComponentType'{typespec=CheckedTs,tags=NewTags},
    check_each_alternative2(S,Type,Ct,[NewC|Acc]);
	    
check_each_alternative2(S,Type,[OtherMarker|Ct],Acc) ->
    %% let 'EXTENSIONMARK' and 'ExtensionAdditionGroup' markers pass through as is
    check_each_alternative2(S,Type,Ct,[OtherMarker|Acc]);
check_each_alternative2(_S,_,[],Acc) ->
    lists:reverse(Acc).


%% componentrelation_leadingattr/2 searches the structure for table
%% constraints, if any is found componentrelation_leadingattr/5 is
%% called.
componentrelation_leadingattr(S,CompList) ->

    %% get_simple_table_if_used/2 should find out whether there are any
    %% component relation constraints in the entire tree of Cs1 that
    %% relates to this level. It returns information about the simple
    %% table constraint necessary for the the call to
    %% componentrelation_leadingattr/6. The step when the leading
    %% attribute and the syntax tree is modified to support the code
    %% generating.
    case get_simple_table_if_used(S,CompList) of
	[] -> {false,CompList};
	_ ->
	    componentrelation_leadingattr(S,CompList,CompList,[],[])
    end.


%%FIXME expand_ExtAddGroups([C#'ExtensionAdditionGroup'{components=ExtAdds}|T],
%% 		    CurrPos,PosAcc,CompAcc) ->
%%     expand_ExtAddGroups(T,CurrPos+ L = lenght(ExtAdds),[{CurrPos,L}|PosAcc],ExtAdds++CompAcc);
%% expand_ExtAddGroups([C|T],CurrPos,PosAcc,CompAcc) ->
%%     expand_ExtAddGroups(T,CurrPos+ 1,PosAcc,[C|CompAcc]);
%% expand_ExtAddGroups([],_CurrPos,PosAcc,CompAcc) ->
%%     {lists:reverse(PosAcc),lists:reverse(CompAcc)}.


%% componentrelation_leadingattr/6 when all components are searched
%% the new modified components are returned together with the "leading
%% attribute" information, which later is stored in the tablecinf
%% field in the SEQUENCE/SET record. The "leading attribute"
%% information is used to generate the lookup in the object set
%% table. The other information gathered in the #type.tablecinf field
%% is used in code generating phase too, to recognice the proper
%% components for "open type" encoding and to propagate the result of
%% the object set lookup when needed.
componentrelation_leadingattr(_,[],_CompList,[],NewCompList) ->
    {false,lists:reverse(NewCompList)};
componentrelation_leadingattr(_,[],_CompList,LeadingAttr,NewCompList) ->
    {lists:last(LeadingAttr),lists:reverse(NewCompList)}; %send all info in Ts later
componentrelation_leadingattr(S,[C= #'ComponentType'{}|Cs],CompList,Acc,CompAcc) ->
    {LAAcc,NewC} =
	case catch componentrelation1(S,C#'ComponentType'.typespec,
				      [C#'ComponentType'.name]) of
	    {'EXIT',_} ->
		{[],C};
	    {CRI=[{_A1,_B1,_C1,_D1}|_Rest],NewTSpec} ->
		%% {ObjectSet,AtPath,ClassDef,Path}
		%% _A1 is a reference to the object set of the
		%% component relation constraint.
		%% _B1 is the path of names in the at-list of the
		%% component relation constraint.
		%% _C1 is the class definition of the
		%% ObjectClassFieldType.
		%% _D1 is the path of components that was traversed to
		%% find this constraint.
		case leading_attr_index(S,CompList,CRI,
					lists:reverse(S#state.abscomppath),[]) of
		    [] ->
			{[],C};
		    [{ObjSet,Attr,N,ClassDef,_Path,ValueIndex}|_NewRest] ->
			OS = object_set_mod_name(S,ObjSet),
			UniqFN = get_unique_fieldname(S,
						      #classdef{typespec=ClassDef}),
			%% Res should be done differently: even though
			%% a unique field name exists it is not
			%% certain that the ObjectClassFieldType of
			%% the simple table constraint picks that
			%% class field.
			Res = #simpletableattributes{objectsetname=OS,
						     c_name=Attr,
						     c_index=N,
						     usedclassfield=UniqFN,
						     uniqueclassfield=UniqFN,
						     valueindex=ValueIndex},
			{[Res],C#'ComponentType'{typespec=NewTSpec}}
		end;
	    _ ->
		%% no constraint was found
		{[],C}
	end,
    componentrelation_leadingattr(S,Cs,CompList,LAAcc++Acc,
				  [NewC|CompAcc]);
componentrelation_leadingattr(S,[NotComponentType|Cs],CompList,LeadingAttr,NewCompList) ->
    componentrelation_leadingattr(S,Cs,CompList,LeadingAttr,[NotComponentType|NewCompList]).


object_set_mod_name(_S,ObjSet) when is_atom(ObjSet) ->
    ObjSet;
object_set_mod_name(#state{mname=M},
		    #'Externaltypereference'{module=M,type=T}) ->
    {M,T};
object_set_mod_name(S,#'Externaltypereference'{module=M,type=T}) ->
    case lists:member(M,S#state.inputmodules) of
	true -> 
	    T;
	false ->
	    {M,T}
    end.


%% get_simple_table_if_used/2 searches the structure of Cs for any
%% component relation constraints due to the present level of the
%% structure. If there are any, the necessary information for code
%% generation of the look up functionality in the object set table are
%% returned.
get_simple_table_if_used(S,Cs) ->
    CNames = [Name||#'ComponentType'{name=Name}<-Cs],
    JustComponents = [C || C = #'ComponentType'{}<-Cs],
    RefedSimpleTable=any_component_relation(S,JustComponents,CNames,[],[]),
    get_simple_table_info(S,Cs,remove_doubles(RefedSimpleTable)).

remove_doubles(L) ->
    remove_doubles(L,[]).
remove_doubles([H|T],Acc) ->
    NewT = remove_doubles1(H,T),
    remove_doubles(NewT,[H|Acc]);
remove_doubles([],Acc) ->
    Acc.

remove_doubles1(El,L) ->
    case lists:delete(El,L) of
	L -> L;
	NewL -> remove_doubles1(El,NewL)
    end.

%% get_simple_table_info searches the components Cs by the path from
%% an at-list (third argument), and follows into a component of it if
%% necessary, to get information needed for code generating.
%%
%% Returns a list of tuples with three elements. It holds a list of
%% atoms that is the path, the name of the field of the class that are
%% referred to in the ObjectClassFieldType, and the name of the unique
%% field of the class of the ObjectClassFieldType. 
%%
% %% The level information outermost/innermost must be kept. There are
% %% at least two possibilities to cover here for an outermost case: 1)
% %% Both the simple table and the component relation have a common path
% %% at least one step below the outermost level, i.e. the leading
% %% information shall be on a sub level. 2) They don't have any common
% %% path.
get_simple_table_info(S, Cs, AtLists) ->
    [get_simple_table_info1(S, Cs, AtList, []) || AtList <- AtLists].

get_simple_table_info1(S, Cs, [Cname|Cnames], Path) ->
    #'ComponentType'{} = C =
	lists:keyfind(Cname, #'ComponentType'.name, Cs),
    get_simple_table_info2(S, C, Cnames, [Cname|Path]).

get_simple_table_info2(S, #'ComponentType'{name=Name,typespec=TS}, [], Path) ->
    OCFT = simple_table_get_ocft(S, Name, TS),
    case lists:keymember(simpletable, 1, TS#type.constraint) of
	true ->
	    simple_table_info(S, OCFT, Path);
	false ->
	    asn1_error(S, {missing_table_constraint,Name})
    end;
get_simple_table_info2(S, #'ComponentType'{typespec=TS}, Cnames, Path) ->
    Components = get_atlist_components(TS#type.def),
    get_simple_table_info1(S, Components, Cnames, Path).

simple_table_get_ocft(_, _, #type{def=#'ObjectClassFieldType'{}=OCFT}) ->
    OCFT;
simple_table_get_ocft(S, Component, #type{constraint=Constr}) ->
    case lists:keyfind(ocft, 1, Constr) of
	{ocft,OCFT} ->
	    OCFT;
	false ->
	    asn1_error(S, {missing_ocft,Component})
    end.

simple_table_info(S,#'ObjectClassFieldType'{classname=ClRef,
					    class=ObjectClass,
					  fieldname=FieldName},Path) ->

    ObjectClassFieldName =
	case FieldName of
	    {LastFieldName,[]} -> LastFieldName;
	    {_FirstFieldName,FieldNames} ->
		lists:last(FieldNames)
	end,
    %%ObjectClassFieldName is the last element in the dotted
    %%list of the ObjectClassFieldType. The last element may
    %%be of another class, that is referenced from the class
    %%of the ObjectClassFieldType
    ClassDef =
	case ObjectClass of
	    [] ->
		{_,CDef}=get_referenced_type(S,ClRef),
		CDef;
	    _ -> #classdef{typespec=ObjectClass}
	end,
    UniqueName = get_unique_fieldname(S, ClassDef),
    {lists:reverse(Path),ObjectClassFieldName,UniqueName}.

%% any_component_relation searches for all component relation
%% constraints that refers to the actual level and returns a list of
%% the "name path" in the at-list to the component relation constraint
%% that must refer to a simple table constraint. The list is empty if
%% no component relation constraints were found.
%% 
%% NamePath has the names of all components that are followed from the
%% beginning of the search. CNames holds the names of all components
%% of the start level, this info is used if an outermost at-notation
%% is found to check the validity of the at-list.
any_component_relation(S,[#'ComponentType'{name=CName,typespec=Type}|Cs],CNames,NamePath,Acc) ->
    CRelPath =
	case lists:keyfind(componentrelation, 1, Type#type.constraint) of
	    {_,_,AtNotation} ->
		%% Found component relation constraint, now check
		%% whether this constraint is relevant for the level
		%% where the search started
		AtNot = extract_at_notation(AtNotation),
		%% evaluate_atpath returns the relative path to the
		%% simple table constraint from where the component
		%% relation is found.
		evaluate_atpath(S,NamePath,CNames,AtNot);
	    false ->
		[]
	end,
    InnerAcc =
	case {Type#type.inlined,
	      asn1ct_gen:type(asn1ct_gen:get_inner(Type#type.def))} of
	    {no,{constructed,bif}} ->

		{InnerCs,NewNamePath} = 
		    case get_components(Type#type.def) of
			T when is_record(T,type) -> {T,NamePath};
			IC -> {IC,[CName|NamePath]}
		    end,
		%% here we are interested in components of an
		%% SEQUENCE/SET OF as well as SEQUENCE, SET and CHOICE
		any_component_relation(S,InnerCs,CNames,NewNamePath,[]);
	    _ ->
		[]
	end,
    any_component_relation(S,Cs,CNames,NamePath,InnerAcc++CRelPath++Acc);
any_component_relation(S,Type,CNames,NamePath,Acc) when is_record(Type,type) ->
    CRelPath =
	case lists:keyfind(componentrelation, 1, Type#type.constraint) of
	    {_,_,AtNotation} ->
		AtNot = extract_at_notation(AtNotation),
		evaluate_atpath(S,NamePath,CNames,AtNot);
	    false ->
		[]
	end,
    InnerAcc =
	case {Type#type.inlined,
	      asn1ct_gen:type(asn1ct_gen:get_inner(Type#type.def))} of
	    {no,{constructed,bif}} ->
		InnerCs = get_components(Type#type.def),
		any_component_relation(S,InnerCs,CNames,NamePath,[]);
	    _ ->
		[]
	end,
    InnerAcc  ++ CRelPath ++ Acc;
%% Just skip the markers for ExtensionAdditionGroup start and end
%% in this function
any_component_relation(S,[#'ExtensionAdditionGroup'{}|Cs],CNames,NamePath,Acc) ->
    any_component_relation(S,Cs,CNames,NamePath,Acc);
any_component_relation(S,['ExtensionAdditionGroupEnd'|Cs],CNames,NamePath,Acc) ->
    any_component_relation(S,Cs,CNames,NamePath,Acc);
any_component_relation(_,[],_,_,Acc) ->
    Acc.

%% evaluate_atpath/4 finds out whether the at notation refers to the
%% search level. The list of referenced names in the AtNot list shall
%% begin with a name that exists on the level it refers to. If the
%% found AtPath is refering to the same sub-branch as the simple table
%% has, then there shall not be any leading attribute info on this
%% level.
evaluate_atpath(_,[],Cnames,{innermost,AtPath=[Ref|_Refs]}) ->
    %% any innermost constraint found deeper in the structure is
    %% ignored.
    case lists:member(Ref,Cnames) of
	true -> [AtPath];
	false -> []
    end;
%% In this case must check that the AtPath doesn't step any step of
%% the NamePath, in that case the constraint will be handled in an
%% inner level.
evaluate_atpath(S=#state{abscomppath=TopPath},NamePath,Cnames,{outermost,AtPath=[_Ref|_Refs]}) ->
    AtPathBelowTop =
	case TopPath of
	    [] -> AtPath;
	    _ ->
		case lists:prefix(TopPath,AtPath) of
		    true ->
			lists:subtract(AtPath,TopPath);
		    _ -> []
		end
	end,
    case {NamePath,AtPathBelowTop} of
	{[H|_T1],[H|_T2]} -> []; % this must be handled in lower level
	{_,[]} -> [];% this must be handled in an above level
	{_,[H|_T]} ->
	    case lists:member(H,Cnames) of
		true -> [AtPathBelowTop];
		_ -> asn1_error(S, {invalid_at_path, AtPath})
	    end
    end;
evaluate_atpath(_,_,_,_) ->
    [].
    
%% Type may be any of SEQUENCE, SET, CHOICE, SEQUENCE OF, SET OF but
%% only the three first have valid components.
get_atlist_components(Def) ->
    get_components(atlist,Def).

get_components(Def) ->
    get_components(any,Def).

get_components(_,#'SEQUENCE'{components=Cs}) ->
    tuple2complist(Cs);
get_components(_,#'SET'{components=Cs}) ->
    tuple2complist(Cs);
get_components(_,{'CHOICE',Cs}) ->
    tuple2complist(Cs);
%do not step in inlined structures
get_components(any,{'SEQUENCE OF',T = #type{def=_Def,inlined=no}}) ->
%    get_components(any,Def);
    T;
get_components(any,{'SET OF',T = #type{def=_Def,inlined=no}}) ->
%    get_components(any,Def);
    T;
get_components(_,_) ->
    [].

tuple2complist({R,E}) ->
    R ++ E;
tuple2complist({R1,E,R2}) ->
    R1 ++ E ++ R2;
tuple2complist(List) when is_list(List) ->
    List.

extract_at_notation([{Level,ValueRefs}]) ->
    {Level,[Name || #'Externalvaluereference'{value=Name} <- ValueRefs]}.
    
%% componentrelation1/1 identifies all componentrelation constraints
%% that exist in C or in the substructure of C. Info about the found
%% constraints are returned in a list. It is ObjectSet, the reference
%% to the object set, AttrPath, the name atoms extracted from the
%% at-list in the component relation constraint, ClassDef, the
%% objectclass record of the class of the ObjectClassFieldType, Path,
%% that is the component name "path" from the searched level to this
%% constraint.
%%
%% The function is called with one component of the type in turn and
%% with the component name in Path at the first call. When called from
%% within, the name of the inner component is added to Path.
componentrelation1(S,C = #type{def=Def,constraint=Constraint,tablecinf=TCI},
		   Path) ->
    Ret =
%	case Constraint of
%	    [{componentrelation,{_,_,ObjectSet},AtList}|_Rest] ->
	case lists:keyfind(componentrelation, 1, Constraint) of
	    {_,{_,_,ObjectSet},AtList} ->
		[{_,AL=[#'Externalvaluereference'{}|_R1]}|_R2] = AtList,
		%% Note: if Path is longer than one,i.e. it is within
		%% an inner type of the actual level, then the only
		%% relevant at-list is of "outermost" type.
%%		#'ObjectClassFieldType'{class=ClassDef} = Def,
		ClassDef = get_ObjectClassFieldType_classdef(S,Def),
		AtPath = 
		    lists:map(fun(#'Externalvaluereference'{value=V})->V end,
			      AL),
		{[{ObjectSet,AtPath,ClassDef,Path}],Def};
	    false ->
		%% check the inner type of component
		innertype_comprel(S,Def,Path)
	end,
    case Ret of
	nofunobj ->
	    nofunobj; %% ignored by caller
	{CRelI=[{ObjSet,_,_,_}],NewDef} -> %%
	    TCItmp = lists:subtract(TCI,[{objfun,ObjSet}]),
	    {CRelI,C#type{tablecinf=[{objfun,ObjSet}|TCItmp],def=NewDef}};
	{CompRelInf,NewDef} -> %% more than one tuple in CompRelInf
	    TCItmp = lists:subtract(TCI,[{objfun,anyset}]),
	    {CompRelInf,C#type{tablecinf=[{objfun,anyset}|TCItmp],def=NewDef}}
    end.

innertype_comprel(S,{'SEQUENCE OF',Type},Path) ->
    case innertype_comprel1(S,Type,Path) of
	nofunobj ->
	    nofunobj;
	{CompRelInf,NewType} ->
	    {CompRelInf,{'SEQUENCE OF',NewType}}
    end;
innertype_comprel(S,{'SET OF',Type},Path) ->
    case innertype_comprel1(S,Type,Path) of
	nofunobj ->
	    nofunobj;
	{CompRelInf,NewType} ->
	    {CompRelInf,{'SET OF',NewType}}
    end;
innertype_comprel(S,{'CHOICE',CTypeList},Path) ->
    case componentlist_comprel(S,CTypeList,[],Path,[]) of
	nofunobj ->
	    nofunobj;
	{CompRelInf,NewCs} ->
	    {CompRelInf,{'CHOICE',NewCs}}
    end;
innertype_comprel(S,Seq = #'SEQUENCE'{components=Cs},Path) ->
    case componentlist_comprel(S,Cs,[],Path,[]) of
	nofunobj ->
	    nofunobj;
	{CompRelInf,NewCs} ->
	    {CompRelInf,Seq#'SEQUENCE'{components=NewCs}}
    end;
innertype_comprel(S,Set = #'SET'{components=Cs},Path) ->
    case componentlist_comprel(S,Cs,[],Path,[]) of
	nofunobj ->
	    nofunobj;
	{CompRelInf,NewCs} ->
	    {CompRelInf,Set#'SET'{components=NewCs}}
    end;
innertype_comprel(_,_,_) ->
    nofunobj.

componentlist_comprel(S,[C = #'ComponentType'{name=Name,typespec=Type}|Cs],
		      Acc,Path,NewCL) ->
    case catch componentrelation1(S,Type,Path++[Name]) of
	{'EXIT',_} ->
	    componentlist_comprel(S,Cs,Acc,Path,[C|NewCL]);
	nofunobj ->
	    componentlist_comprel(S,Cs,Acc,Path,[C|NewCL]);
	{CRelInf,NewType} ->
	    componentlist_comprel(S,Cs,CRelInf++Acc,Path,
				  [C#'ComponentType'{typespec=NewType}|NewCL])
    end;
componentlist_comprel(_,[],Acc,_,NewCL) ->
    case Acc of
	[] ->
	    nofunobj;
	_ ->
	    {Acc,lists:reverse(NewCL)}
    end.

innertype_comprel1(S,T = #type{def=Def,constraint=Cons,tablecinf=TCI},Path) ->
    Ret =
	case lists:keyfind(componentrelation, 1, Cons) of
	    {_,{_,_,ObjectSet},AtList} ->
		%% This AtList must have an "outermost" at sign to be
		%% relevent here.
		[{_,AL=[#'Externalvaluereference'{value=_Attr}|_R1]}|_R2] 
		    = AtList,
%%		#'ObjectClassFieldType'{class=ClassDef} = Def,
		ClassDef = get_ObjectClassFieldType_classdef(S,Def),
		AtPath = 
		    lists:map(fun(#'Externalvaluereference'{value=V})->V end,
			      AL),
		[{ObjectSet,AtPath,ClassDef,Path}];
	    false ->
		innertype_comprel(S,Def,Path)
	end,
    case Ret of
	nofunobj -> nofunobj;
	L = [{ObjSet,_,_,_}] ->
	    TCItmp = lists:subtract(TCI,[{objfun,ObjSet}]),
	    {L,T#type{tablecinf=[{objfun,ObjSet}|TCItmp]}};
	{CRelInf,NewDef} ->
	    TCItmp = lists:subtract(TCI,[{objfun,anyset}]),
	    {CRelInf,T#type{def=NewDef,tablecinf=[{objfun,anyset}|TCItmp]}}
    end.


%% leading_attr_index counts the index and picks the name of the
%% component that is at the actual level in the at-list of the
%% component relation constraint (AttrP).  AbsP is the path of
%% component names from the top type level to the actual level. AttrP
%% is a list with the atoms from the at-list.
leading_attr_index(S,Cs,[H={_,AttrP,_,_}|T],AbsP,Acc) ->
    AttrInfo = 
	case lists:prefix(AbsP,AttrP) of
	    %% why this ?? It is necessary when in same situation as
	    %% TConstrChoice, there is an inner structure with an
	    %% outermost at-list and the "leading attribute" code gen
	    %% may be at a level some steps below the outermost level.
	    true ->
		RelativAttrP = lists:subtract(AttrP,AbsP),
		%% The header is used to calculate the index of the
		%% component and to give the fun, received from the
		%% object set look up, an unique name. The tail is
		%% used to match the proper value input to the fun.
		{hd(RelativAttrP),tl(RelativAttrP)};
	    false ->
		{hd(AttrP),tl(AttrP)}
	end,
    case leading_attr_index1(S,Cs,H,AttrInfo,1) of
	0 ->
	    leading_attr_index(S,Cs,T,AbsP,Acc);
	Res ->
	    leading_attr_index(S,Cs,T,AbsP,[Res|Acc])
    end;
leading_attr_index(_,_Cs,[],_,Acc) ->
    lists:reverse(Acc).

leading_attr_index1(_,[],_,_,_) ->
    0;
leading_attr_index1(S,[C|Cs],Arg={ObjectSet,_,CDef,P},
		    AttrInfo={Attr,SubAttr},N) ->
    case C#'ComponentType'.name of
	Attr ->
	    ValueMatch = value_match(S,C,Attr,SubAttr),
	    {ObjectSet,Attr,N,CDef,P,ValueMatch};
	_ ->
	    leading_attr_index1(S,Cs,Arg,AttrInfo,N+1)
    end.

%% value_math gathers information for a proper value match in the
%% generated encode function. For a SEQUENCE or a SET the index of the
%% component is counted. For a CHOICE the index is 2.
value_match(S,C,Name,SubAttr) ->
    value_match(S,C,Name,SubAttr,[]). % C has name Name
value_match(_S,#'ComponentType'{},_Name,[],Acc) ->
    Acc;% do not reverse, indexes in reverse order
value_match(S,#'ComponentType'{typespec=Type},Name,[At|Ats],Acc) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    Components =
	case get_atlist_components(Type#type.def) of
	    [] -> asn1_error(S, {invalid_element, Name});
	    Comps -> Comps
	end,
    {Index,ValueIndex} = component_value_index(S,InnerType,At,Components),
    value_match(S,lists:nth(Index,Components),At,Ats,[ValueIndex|Acc]).

component_value_index(S,'CHOICE',At,Components) ->
    {component_index(S,At,Components),2};
component_value_index(S,_,At,Components) ->
    %% SEQUENCE or SET
    Index = component_index(S,At,Components),
    {Index,{Index+1,At}}.

component_index(S,Name,Components) ->
    component_index1(S,Name,Components,1).
component_index1(_S,Name,[#'ComponentType'{name=Name}|_Cs],N) ->
    N;
component_index1(S,Name,[_C|Cs],N) ->
    component_index1(S,Name,Cs,N+1);
component_index1(S,Name,[],_) ->
    asn1_error(S, {invalid_at_list, Name}).

get_unique_fieldname(S, #classdef{typespec=TS}) ->
    Fields = TS#objectclass.fields,
    get_unique_fieldname1(S, Fields, []);
get_unique_fieldname(S,#typedef{typespec=#type{def=ClassRef}}) ->
    %% A class definition may be referenced as
    %% REFED-CLASS ::= DEFINED-CLASS and then REFED-CLASS is a typedef
    {_M,ClassDef} = get_referenced_type(S,ClassRef),
    get_unique_fieldname(S,ClassDef).

get_unique_fieldname1(S, [{fixedtypevaluefield,Name,_,'UNIQUE',Opt}|T], Acc) ->
    get_unique_fieldname1(S, T, [{Name,Opt}|Acc]);
get_unique_fieldname1(S, [_|T], Acc) ->
    get_unique_fieldname1(S, T, Acc);
get_unique_fieldname1(S, [], Acc) ->
    case Acc of
	[] -> no_unique;
	[Name] -> Name;
	[_|_] -> asn1_error(S, multiple_uniqs)
    end.

get_tableconstraint_info(S,Type,{CheckedTs,EComps,CheckedTs2}) ->
    {get_tableconstraint_info(S,Type,CheckedTs,[]),
     get_tableconstraint_info(S,Type,EComps,[]),
     get_tableconstraint_info(S,Type,CheckedTs2,[])};
get_tableconstraint_info(S,Type,{CheckedTs,EComps}) ->
    {get_tableconstraint_info(S,Type,CheckedTs,[]),
     get_tableconstraint_info(S,Type,EComps,[])};
get_tableconstraint_info(S,Type,CheckedTs) ->
    get_tableconstraint_info(S,Type,CheckedTs,[]).

get_tableconstraint_info(_S,_Type,[],Acc) ->
    lists:reverse(Acc);
get_tableconstraint_info(S,Type,[C=#'ComponentType'{typespec=CheckedTs}|Cs],Acc) ->
    AccComp = 
	case CheckedTs#type.def of 
	    %% ObjectClassFieldType
	    OCFT=#'ObjectClassFieldType'{} ->
		NewOCFT =
		    OCFT#'ObjectClassFieldType'{class=[]},
		C#'ComponentType'{typespec=
				  CheckedTs#type{
				    def=NewOCFT
				    }};
%				    constraint=[{tableconstraint_info,
%						 FieldRef}]}};
	    {'SEQUENCE OF',SOType} when is_record(SOType,type),
					(element(1,SOType#type.def)=='CHOICE') ->
		CTypeList = element(2,SOType#type.def),
		NewInnerCList = 
		    get_tableconstraint_info(S,Type,CTypeList),
		C#'ComponentType'{typespec=
				  CheckedTs#type{
				    def={'SEQUENCE OF',
					 SOType#type{def={'CHOICE',
							  NewInnerCList}}}}};
	    {'SET OF',SOType} when is_record(SOType,type),
				   (element(1,SOType#type.def)=='CHOICE') ->
		CTypeList = element(2,SOType#type.def),
		NewInnerCList = 
		    get_tableconstraint_info(S,Type,CTypeList),
		C#'ComponentType'{typespec=
				  CheckedTs#type{
				    def={'SET OF',
					 SOType#type{def={'CHOICE',
							  NewInnerCList}}}}};
	    _ ->
		C
	end,
    get_tableconstraint_info(S,Type,Cs,[AccComp|Acc]);
get_tableconstraint_info(S,Type,[C|Cs],Acc) ->
    get_tableconstraint_info(S,Type,Cs,[C|Acc]).

get_referenced_fieldname([{_,FirstFieldname}]) ->
    {FirstFieldname,[]};
get_referenced_fieldname([{_,FirstFieldname}|T]) ->
    {FirstFieldname,[element(2, X) || X <- T]}.

%% get_ObjectClassFieldType_classdef gets the def of the class of the
%% ObjectClassFieldType, i.e. the objectclass record. If the type has
%% been checked (it may be a field type of an internal SEQUENCE) the
%% class field = [], then the classdef has to be fetched by help of
%% the class reference in the classname field.
get_ObjectClassFieldType_classdef(S,#'ObjectClassFieldType'{classname=Name,class=[]}) ->
    {_,#classdef{typespec=TS}} = get_referenced_type(S,Name),
    TS;
get_ObjectClassFieldType_classdef(_,#'ObjectClassFieldType'{class=Cl}) ->
    Cl.

get_OCFType(S,Fields,FieldnameList=[{_FieldType,_PrimFieldName}|_]) ->
    get_OCFType(S,Fields,[PFN||{_,PFN} <- FieldnameList]);
get_OCFType(S,Fields,[PrimFieldName|Rest]) ->
    case lists:keysearch(PrimFieldName,2,Fields) of
	{value,{fixedtypevaluefield,_,Type,_Unique,_OptSpec}} ->
	    {fixedtypevaluefield,PrimFieldName,Type};
	{value,{objectfield,_,ClassRef,_Unique,_OptSpec}} ->
	    {MName,ClassDef} = get_referenced_type(S,ClassRef),
	    NewS = update_state(S#state{tname=get_datastr_name(ClassDef)},
				MName),
	    CheckedCDef = check_class(NewS,ClassDef),
	    get_OCFType(S,CheckedCDef#objectclass.fields,Rest);
	{value,{objectsetfield,_,Type,_OptSpec}} ->
	    {MName,ClassDef} = get_referenced_type(S,Type#type.def),
	    NewS = update_state(S#state{tname=get_datastr_name(ClassDef)},
				MName),
	    CheckedCDef = check_class(NewS,ClassDef),
	    get_OCFType(S,CheckedCDef#objectclass.fields,Rest);

	{value,Other} ->
	    {element(1,Other),PrimFieldName};
	_  ->
	    asn1_error(S, {illegal_object_field, PrimFieldName})
    end.

get_taglist(S,Ext) when is_record(Ext,'Externaltypereference') ->
    {_,T} = get_referenced_type(S,Ext),
    get_taglist(S,T#typedef.typespec);
get_taglist(S,Type) when is_record(Type,type) ->
    case Type#type.tag of
	[] ->
	    get_taglist(S,Type#type.def);
	[Tag|_]  ->
	    [asn1ct_gen:def_to_tag(Tag)]
    end;
get_taglist(S,{'CHOICE',{Rc,Ec}}) ->
    get_taglist1(S,Rc ++ Ec);
get_taglist(S,{'CHOICE',{R1,E,R2}}) ->
    get_taglist1(S,R1 ++ E ++ R2);
get_taglist(S,{'CHOICE',Components}) ->
    get_taglist1(S,Components);
%% ObjectClassFieldType OTP-4390
get_taglist(_S,#'ObjectClassFieldType'{type={typefield,_}}) ->
    [];
get_taglist(S,#'ObjectClassFieldType'{type={fixedtypevaluefield,_,Type}}) ->
    get_taglist(S,Type);
get_taglist(_, _) ->
    [].

get_taglist1(S,[#'ComponentType'{name=_Cname,tags=TagL}|Rest]) when is_list(TagL) -> 
    %% tag_list has been here , just return TagL and continue with next alternative
    TagL ++ get_taglist1(S,Rest);
get_taglist1(S,[#'ComponentType'{typespec=Ts,tags=undefined}|Rest]) ->
    get_taglist(S,Ts) ++ get_taglist1(S,Rest);
get_taglist1(S,[_H|Rest]) -> % skip EXTENSIONMARK
    get_taglist1(S,Rest);
get_taglist1(_S,[]) ->
    [].

%% def_to_tag(S,Def) ->
%%     case asn1ct_gen:def_to_tag(Def) of
%% 	{'UNIVERSAL',T} ->
%% 	    case asn1ct_gen:prim_bif(T) of
%% 		true ->
%% 		    ?TAG_PRIMITIVE(tag_number(T));
%% 		_ ->
%% 		    ?TAG_CONSTRUCTED(tag_number(T))
%% 	    end;
%% 	_ -> []
%%     end.
%% tag_number('BOOLEAN') -> 1;
%% tag_number('INTEGER') -> 2;
%% tag_number('BIT STRING') -> 3;
%% tag_number('OCTET STRING') -> 4;
%% tag_number('NULL') -> 5;
%% tag_number('OBJECT IDENTIFIER') -> 6;
%% tag_number('ObjectDescriptor') -> 7;
%% tag_number('EXTERNAL') -> 8;
%% tag_number('INSTANCE OF') -> 8;
%% tag_number('REAL') -> 9;
%% tag_number('ENUMERATED') -> 10;
%% tag_number('EMBEDDED PDV') -> 11;
%% tag_number('UTF8String') -> 12;
%% %%tag_number('RELATIVE-OID') -> 13;
%% tag_number('SEQUENCE') -> 16;
%% tag_number('SEQUENCE OF') -> 16;
%% tag_number('SET') -> 17;
%% tag_number('SET OF') -> 17;
%% tag_number('NumericString') -> 18;
%% tag_number('PrintableString') -> 19;
%% tag_number('TeletexString') -> 20;
%% %%tag_number('T61String') -> 20;
%% tag_number('VideotexString') -> 21;
%% tag_number('IA5String') -> 22;
%% tag_number('UTCTime') -> 23;
%% tag_number('GeneralizedTime') -> 24;
%% tag_number('GraphicString') -> 25;
%% tag_number('VisibleString') -> 26;
%% %%tag_number('ISO646String') -> 26;
%% tag_number('GeneralString') -> 27;
%% tag_number('UniversalString') -> 28;
%% tag_number('CHARACTER STRING') -> 29;
%% tag_number('BMPString') -> 30.

merge_tags(T1, T2) when is_list(T2) ->
    merge_tags2(T1 ++ T2, []);
merge_tags(T1, T2) ->
    merge_tags2(T1 ++ [T2], []).

merge_tags2([T1= #tag{type='IMPLICIT'}, T2 |Rest], Acc) ->
    merge_tags2([T1#tag{type=T2#tag.type, form=T2#tag.form}|Rest],Acc);
merge_tags2([T1= #tag{type={default,'IMPLICIT'}}, T2 |Rest], Acc) ->
    merge_tags2([T1#tag{type=T2#tag.type, form=T2#tag.form}|Rest],Acc);
merge_tags2([T1= #tag{type={default,'AUTOMATIC'}}, T2 |Rest], Acc) ->
    merge_tags2([T1#tag{type=T2#tag.type, form=T2#tag.form}|Rest],Acc);
merge_tags2([H|T],Acc) ->
    merge_tags2(T, [H|Acc]);
merge_tags2([], Acc) ->
    lists:reverse(Acc).

storeindb(S0, #module{name=ModName,typeorval=TVlist0}=M) ->
    S = S0#state{mname=ModName},
    TVlist1 = [{asn1ct:get_name_of_def(Def),Def} || Def <- TVlist0],
    case check_duplicate_defs(S, TVlist1) of
	ok ->
	    storeindb_1(S, M, TVlist0, TVlist1);
	{error,_}=Error ->
	    Error
    end.

storeindb_1(S, #module{name=ModName}=M, TVlist0, TVlist) ->
    NewM = M#module{typeorval=findtypes_and_values(TVlist0)},
    asn1_db:dbnew(ModName, S#state.erule),
    asn1_db:dbput(ModName, 'MODULE',  NewM),
    asn1_db:dbput(ModName, TVlist),
    include_default_class(S, NewM#module.name),
    include_default_type(NewM#module.name),
    ok.

check_duplicate_defs(S, Defs) ->
    Set0 = sofs:relation(Defs),
    Set1 = sofs:relation_to_family(Set0),
    Set = sofs:to_external(Set1),
    case [duplicate_def(S, N, Dup) || {N,[_,_|_]=Dup} <- Set] of
	[] ->
	    ok;
	[_|_]=E ->
	    {error,lists:append(E)}
    end.

duplicate_def(S, Name, Dups0) ->
    Dups1 = [{asn1ct:get_pos_of_def(Def),Def} || Def <- Dups0],
    [{Prev,_}|Dups] = lists:sort(Dups1),
    duplicate_def_1(S, Dups, Name, Prev).

duplicate_def_1(S, [{_,Def}|T], Name, Prev) ->
    E = return_asn1_error(S, Def, {already_defined,Name,Prev}),
    [E|duplicate_def_1(S, T, Name, Prev)];
duplicate_def_1(_, [], _, _) ->
    [].

findtypes_and_values(TVList) ->
    findtypes_and_values(TVList,[],[],[],[],[],[]).%% Types,Values,
%% Parameterizedtypes,Classes,Objects and ObjectSets

findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when is_record(H,typedef),is_record(H#typedef.typespec,'Object') ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,[H#typedef.name|Oacc],OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when is_record(H,typedef),is_record(H#typedef.typespec,'ObjectSet') ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,Oacc,[H#typedef.name|OSacc]);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when is_record(H,typedef) ->
    findtypes_and_values(T,[H#typedef.name|Tacc],Vacc,Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when is_record(H,valuedef) ->
    findtypes_and_values(T,Tacc,[H#valuedef.name|Vacc],Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when is_record(H,ptypedef) ->
    findtypes_and_values(T,Tacc,Vacc,[H#ptypedef.name|Pacc],Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when is_record(H,classdef) ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,[H#classdef.name|Cacc],Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when is_record(H,pvaluedef) ->
    findtypes_and_values(T,Tacc,[H#pvaluedef.name|Vacc],Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when is_record(H,pvaluesetdef) ->
    findtypes_and_values(T,Tacc,[H#pvaluesetdef.name|Vacc],Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when is_record(H,pobjectdef) ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,[H#pobjectdef.name|Oacc],OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when is_record(H,pobjectsetdef) ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,Oacc,[H#pobjectsetdef.name|OSacc]);
findtypes_and_values([],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) ->
    {lists:reverse(Tacc),lists:reverse(Vacc),lists:reverse(Pacc),
     lists:reverse(Cacc),lists:reverse(Oacc),lists:reverse(OSacc)}.
    
return_asn1_error(#state{error_context=Context}=S, Error) ->
    return_asn1_error(S, Context, Error).

return_asn1_error(#state{mname=Where}, Item, Error) ->
    Pos = asn1ct:get_pos_of_def(Item),
    {structured_error,{Where,Pos},?MODULE,Error}.

-spec asn1_error(_, _) -> no_return().
asn1_error(S, Error) ->
    throw({error,return_asn1_error(S, Error)}).

format_error({already_defined,Name,PrevLine}) ->
    io_lib:format("the name ~p has already been defined at line ~p",
		  [Name,PrevLine]);
format_error({duplicate_identifier,Ids}) ->
    io_lib:format("the identifier '~p' has already been used", [Ids]);
format_error({duplicate_tags,Elements}) ->
    io_lib:format("duplicate tags in the elements: ~s",
		  [format_elements(Elements)]);
format_error({enum_illegal_redefinition,Id}) ->
    io_lib:format("'~s' must not be redefined", [Id]);
format_error({enum_not_ascending,Id,N,Prev}) ->
    io_lib:format("the values for enumerations which follow '...' must "
		  "be in ascending order, but '~p(~p)' is less than the "
		  "previous value '~p'", [Id,N,Prev]);
format_error({enum_reused_value,Id,Val}) ->
    io_lib:format("'~s' has the value '~p' which is used more than once",
		  [Id,Val]);
format_error({illegal_id, Id}) ->
    io_lib:format("illegal identifier: ~p", [Id]);
format_error({illegal_choice_type, Ref}) ->
    io_lib:format("expecting a CHOICE type: ~p", [Ref]);
format_error({illegal_class_name,Class}) ->
    io_lib:format("the class name '~s' is illegal (it must start with an uppercase letter and only contain uppercase letters, digits, or hyphens)", [Class]);
format_error({illegal_COMPONENTS_OF, Ref}) ->
    io_lib:format("expected a SEQUENCE or SET got: ~p", [Ref]);
format_error(illegal_external_value) ->
    "illegal value in EXTERNAL type";
format_error({illegal_instance_of,Class}) ->
    io_lib:format("using INSTANCE OF on class '~s' is illegal, "
		  "because INSTANCE OF may only be used on the class TYPE-IDENTIFIER",
		  [Class]);
format_error(illegal_integer_value) ->
    "expecting an integer value";
format_error(illegal_object) ->
    "expecting an object";
format_error({illegal_object_field, Id}) ->
    io_lib:format("expecting a class field: ~p",[Id]);
format_error({illegal_oid,o_id}) ->
    "illegal OBJECT IDENTIFIER";
format_error({illegal_oid,rel_oid}) ->
    "illegal RELATIVE-OID";
format_error(illegal_octet_string_value) ->
    "expecting a bstring or an hstring as value for an OCTET STRING";
format_error({illegal_typereference,Name}) ->
    io_lib:format("'~p' is used as a typereference, but does not start with an uppercase letter", [Name]);
format_error(illegal_table_constraint) ->
    "table constraints may only be applied to CLASS.&field constructs";
format_error(illegal_value) ->
    "expecting a value";
format_error({illegal_value, TYPE}) ->
    io_lib:format("expecting a ~s value", [TYPE]);
format_error({invalid_fields,Fields,Obj}) ->
    io_lib:format("invalid ~s in ~p", [format_fields(Fields),Obj]);
format_error({invalid_bit_number,Bit}) ->
    io_lib:format("the bit number '~p' is invalid", [Bit]);
format_error(invalid_table_constraint) ->
    "the table constraint is not an object set";
format_error(invalid_objectset) ->
    "expecting an object set";
format_error({implicit_tag_before,Kind}) ->
    "illegal implicit tag before " ++
	case Kind of
	    choice -> "'CHOICE'";
	    open_type -> "open type"
	end;
format_error({missing_mandatory_fields,Fields,Obj}) ->
    io_lib:format("missing mandatory ~s in ~p",
		  [format_fields(Fields),Obj]);
format_error({missing_table_constraint,Component}) ->
    io_lib:format("the component '~s' is referenced by a component relation constraint using the '@field-name' notation, but does not have a table constraint",
		  [Component]);
format_error({missing_id,Id}) ->
    io_lib:format("expected the mandatory component '~p'", [Id]);
format_error({missing_ocft,Component}) ->
    io_lib:format("the component '~s' must be an ObjectClassFieldType (CLASSNAME.&field-name)", [Component]);
format_error(multiple_uniqs) ->
    "implementation limitation: only one UNIQUE field is allowed in CLASS";
format_error({namelist_redefinition,Name}) ->
    io_lib:format("the name '~s' can not be redefined", [Name]);
format_error({param_bad_type, Ref}) ->
    io_lib:format("'~p' is not a parameterized type", [Ref]);
format_error(param_wrong_number_of_arguments) ->
    "wrong number of arguments";
format_error(reversed_range) ->
    "ranges must be given in increasing order";
format_error({syntax_duplicated_fields,Fields}) ->
    io_lib:format("~s must only occur once in the syntax list",
		  [format_fields(Fields)]);
format_error(syntax_nomatch) ->
    "unexpected end of object definition";
format_error({syntax_mandatory_in_optional_group,Name}) ->
    io_lib:format("the field '&~s' must not be within an optional group since it is not optional",
		  [Name]);
format_error({syntax_missing_mandatory_fields,Fields}) ->
    io_lib:format("missing mandatory ~s in the syntax list",
		  [format_fields(Fields)]);
format_error({syntax_nomatch,Actual}) ->
    io_lib:format("~s is not the next item allowed according to the defined syntax",
		  [Actual]);
format_error({syntax_undefined_field,Field}) ->
    io_lib:format("'&~s' is not a field of the class being defined",
		  [Field]);
format_error({undefined,Name}) ->
    io_lib:format("'~s' is referenced, but is not defined", [Name]);
format_error({undefined_export,Ref}) ->
    io_lib:format("'~s' is exported but is not defined", [Ref]);
format_error({undefined_field,FieldName}) ->
    io_lib:format("the field '&~s' is undefined", [FieldName]);
format_error({undefined_import,Ref,Module}) ->
    io_lib:format("'~s' is not exported from ~s", [Ref,Module]);
format_error({unique_and_default,Field}) ->
    io_lib:format("the field '&~s' must not have both 'UNIQUE' and 'DEFAULT'",
		  [Field]);
format_error({value_reused,Val}) ->
    io_lib:format("the value '~p' is used more than once", [Val]);
format_error({non_unique_object,Id}) ->
    io_lib:format("object set with a UNIQUE field value of '~p' is used more than once", [Id]);
format_error(Other) ->
    io_lib:format("~p", [Other]).

format_fields([F]) ->
    io_lib:format("field '&~s'", [F]);
format_fields([H|T]) ->
    [io_lib:format("fields '&~s'", [H])|
     [io_lib:format(", '&~s'", [F]) || F <- T]].

format_elements([H1,H2|T]) ->
    [io_lib:format("~p, ", [H1])|format_elements([H2|T])];
format_elements([H]) ->
    io_lib:format("~p", [H]).

include_default_type(Module) ->
    NameAbsList = default_type_list(),
    include_default_type1(Module,NameAbsList).

include_default_type1(_,[]) ->
    ok;
include_default_type1(Module,[{Name,TS}|Rest]) ->
    case asn1_db:dbget(Module,Name) of
	undefined ->
	    T = #typedef{name=Name,
			 typespec=TS},
		asn1_db:dbput(Module,Name,T);
	_ -> ok
    end,
    include_default_type1(Module,Rest).

default_type_list() ->
    %% The EXTERNAL type is represented, according to ASN.1 1997, 
    %% as a SEQUENCE with components: identification, data-value-descriptor
    %% and data-value.
    Syntax = 
	#'ComponentType'{name=syntax,
			 typespec=#type{def='OBJECT IDENTIFIER'},
			 prop=mandatory},
    Presentation_Cid =
	#'ComponentType'{name='presentation-context-id',
			 typespec=#type{def='INTEGER'},
			 prop=mandatory},
    Transfer_syntax =
	#'ComponentType'{name='transfer-syntax',
			 typespec=#type{def='OBJECT IDENTIFIER'},
			 prop=mandatory},
    Negotiation_items = 
	#type{def=
	      #'SEQUENCE'{components=
			  [Presentation_Cid,
			   Transfer_syntax#'ComponentType'{prop=mandatory}]}},
    Context_negot = 
	#'ComponentType'{name='context-negotiation',
			 typespec=Negotiation_items,
			 prop=mandatory},

    Data_value_descriptor =
	#'ComponentType'{name='data-value-descriptor',
			 typespec=#type{def='ObjectDescriptor'},
			 prop='OPTIONAL'},
    Data_value =
	#'ComponentType'{name='data-value',
			 typespec=#type{def='OCTET STRING'},
			 prop=mandatory},

    %% The EXTERNAL type is represented, according to ASN.1 1990, 
    %% as a SEQUENCE with components: direct-reference, indirect-reference,
    %% data-value-descriptor and encoding.

    Direct_reference = 
	#'ComponentType'{name='direct-reference',
			 typespec=#type{def='OBJECT IDENTIFIER'},
			 prop='OPTIONAL',
			 tags=[{'UNIVERSAL',6}]},

    Indirect_reference = 
	#'ComponentType'{name='indirect-reference',
			 typespec=#type{def='INTEGER'},
			 prop='OPTIONAL',
			 tags=[{'UNIVERSAL',2}]},

    Single_ASN1_type =
	#'ComponentType'{name='single-ASN1-type',
			 typespec=#type{tag=[{tag,'CONTEXT',0,
					      'EXPLICIT',32}],
					def='ANY'},
			 prop=mandatory,
			 tags=[{'CONTEXT',0}]},

    Octet_aligned =
	#'ComponentType'{name='octet-aligned',
			 typespec=#type{tag=[{tag,'CONTEXT',1,
					      'IMPLICIT',0}],
					def='OCTET STRING'},
			 prop=mandatory,
			 tags=[{'CONTEXT',1}]},

    Arbitrary =
	#'ComponentType'{name=arbitrary,
			 typespec=#type{tag=[{tag,'CONTEXT',2,
					      'IMPLICIT',0}],
					def={'BIT STRING',[]}},
			 prop=mandatory,
			 tags=[{'CONTEXT',2}]},

    Encoding =
	#'ComponentType'{name=encoding,
			 typespec=#type{def={'CHOICE',
					     [Single_ASN1_type,Octet_aligned,
					      Arbitrary]}},
			 prop=mandatory},

    EXTERNAL_components1990 =
	[Direct_reference,Indirect_reference,Data_value_descriptor,Encoding],
    
    %% The EMBEDDED PDV type is represented by a SEQUENCE type
    %% with components: identification and data-value
    Abstract = 
	#'ComponentType'{name=abstract,
			 typespec=#type{def='OBJECT IDENTIFIER'},
			 prop=mandatory},
    Transfer =
	#'ComponentType'{name=transfer,
			 typespec=#type{def='OBJECT IDENTIFIER'},
			 prop=mandatory},
    AbstractTrSeq =
	#'SEQUENCE'{components=[Abstract,Transfer]},
    Syntaxes =
	#'ComponentType'{name=syntaxes,
			 typespec=#type{def=AbstractTrSeq},
			 prop=mandatory},
    Fixed = #'ComponentType'{name=fixed,
			     typespec=#type{def='NULL'},
			     prop=mandatory},
    Negotiations =
	[Syntaxes,Syntax,Presentation_Cid,Context_negot,
	 Transfer_syntax,Fixed],
    Identification2 =
	#'ComponentType'{name=identification,
			 typespec=#type{def={'CHOICE',Negotiations}},
			 prop=mandatory},
    EmbeddedPdv_components =
	[Identification2,Data_value],
    
    %% The CHARACTER STRING type is represented by a SEQUENCE type
    %% with components: identification and string-value
    String_value = 
	#'ComponentType'{name='string-value',
			 typespec=#type{def='OCTET STRING'},
			 prop=mandatory},
    CharacterString_components =
	[Identification2,String_value],

    [{'EXTERNAL',
      #type{tag=[#tag{class='UNIVERSAL',
		      number=8,
		      type='IMPLICIT',
		      form=32}],
	    def=#'SEQUENCE'{components=
			    EXTERNAL_components1990}}},
     {'EMBEDDED PDV',
      #type{tag=[#tag{class='UNIVERSAL',
		      number=11,
		      type='IMPLICIT',
		      form=32}],
	    def=#'SEQUENCE'{components=EmbeddedPdv_components}}},
     {'CHARACTER STRING',
      #type{tag=[#tag{class='UNIVERSAL',
		      number=29,
		      type='IMPLICIT',
		      form=32}],
	    def=#'SEQUENCE'{components=CharacterString_components}}}
     ].


include_default_class(S, Module) ->
    _ = [include_default_class1(S, Module, ClassDef) ||
	    ClassDef <- default_class_list()],
	ok.

include_default_class1(S, Module, {Name,Ts0}) ->
    case asn1_db:dbget(Module, Name) of
	undefined ->
	    #objectclass{fields=Fields,
			 syntax={'WITH SYNTAX',Syntax0}} = Ts0,
	    Syntax = preprocess_syntax(S, Syntax0, Fields),
	    Ts = Ts0#objectclass{syntax={preprocessed_syntax,Syntax}},
	    C = #classdef{checked=true,module=Module,
			  name=Name,typespec=Ts},
	    asn1_db:dbput(Module, Name, C);
	_ ->
	    ok
    end.

default_class_list() ->
    [{'TYPE-IDENTIFIER',
      #objectclass{fields=[{fixedtypevaluefield,
			    id,
			    #type{tag=[?TAG_PRIMITIVE(?N_OBJECT_IDENTIFIER)],
				  def='OBJECT IDENTIFIER'},
			    'UNIQUE',
			    'MANDATORY'},
			   {typefield,'Type','MANDATORY'}],
		   syntax={'WITH SYNTAX',
			   [{typefieldreference,'Type'},
			    'IDENTIFIED',
			    'BY',
			    {valuefieldreference,id}]}}},
     {'ABSTRACT-SYNTAX',
      #objectclass{fields=[{fixedtypevaluefield,
			    id,
			    #type{tag=[?TAG_PRIMITIVE(?N_OBJECT_IDENTIFIER)],
				  def='OBJECT IDENTIFIER'},
			    'UNIQUE',
			    'MANDATORY'},
			   {typefield,'Type','MANDATORY'},
			   {fixedtypevaluefield,
			    property,
			    #type{tag=[?TAG_PRIMITIVE(?N_BIT_STRING)],
				  def={'BIT STRING',[]}},
			    undefined,
			    {'DEFAULT',
			     [0,1,0]}}],
		   syntax={'WITH SYNTAX',
			   [{typefieldreference,'Type'},
			    'IDENTIFIED',
			    'BY',
			    {valuefieldreference,id},
			    ['HAS',
			     'PROPERTY',
			     {valuefieldreference,property}]]}}}].

new_reference_name(Name) ->
    case get(asn1_reference) of
	undefined ->
	    put(asn1_reference,1),
	    list_to_atom(lists:concat([internal_,Name,"_",1]));
	Num when is_integer(Num) ->
	    put(asn1_reference,Num+1),
	    list_to_atom(lists:concat([internal_,Name,"_",Num+1]))
    end.

get_record_prefix_name(S) ->
    case lists:keysearch(record_name_prefix,1,S#state.options) of
	{value,{_,Prefix}} ->
	    Prefix;
	_ ->
	    ""
    end.

insert_once(S,Tab,Key) ->
    case get(top_module) of
	M when M == S#state.mname ->
	    asn1ct_gen:insert_once(Tab,Key),
	    ok;
	_ ->
	    skipped
    end.

check_fold(S0, [H|T], Check) ->
    Type = asn1_db:dbget(S0#state.mname, H),
    S = S0#state{error_context=Type},
    case Check(S, H, Type) of
	ok ->
	    check_fold(S, T, Check);
	Error ->
	    [Error|check_fold(S, T, Check)]
    end;
check_fold(_, [], Check) when is_function(Check, 3) -> [].

error_value(Value) when is_integer(Value) -> Value;
error_value(Value) when is_atom(Value) -> Value;
error_value(#type{def=Value}) when is_atom(Value) -> Value;
error_value(#type{def=Value}) -> error_value(Value);
error_value(RefOrType) ->
    try name_of_def(RefOrType) of
	Name -> Name
    catch _:_ ->
	    case get_datastr_name(RefOrType) of
		undefined -> RefOrType;
		Name -> Name
	    end
    end.

name_of_def(#'Externaltypereference'{type=N}) -> N;
name_of_def(#'Externalvaluereference'{value=N}) -> N.
