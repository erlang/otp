%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: asn1ct_check.erl,v 1.1 2008/12/17 09:53:29 mikpe Exp $
-module(asn1ct_check).

%% Main Module for ASN.1 compile time functions

%-compile(export_all).
-export([check/2,storeindb/1]).
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
	case S#state.erule of
	    ber_bin_v2 ->
		#tag{class='UNIVERSAL',number=Num,type='IMPLICIT',form=0};
	    _ -> []
	end).
-define(TAG_CONSTRUCTED(Num),
	case S#state.erule of
	    ber_bin_v2 ->
		#tag{class='UNIVERSAL',number=Num,type='IMPLICIT',form=32};
	    _ -> []
	end).

-record(newt,{type=unchanged,tag=unchanged,constraint=unchanged,inlined=no}). % used in check_type to update type and tag
-record(newv,{type=unchanged,value=unchanged}). % used in check_value to update type and value

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

    _Perror = checkp(S,ParameterizedTypes,[]), % must do this before the templates are used
    Terror = checkt(S,Types,[]),

    %% get parameterized object sets sent to checkt/3
    %% and update Terror

    {PObjSetNames1,Terror2} = filter_errors(IsPObjSet,Terror),

    Verror = checkv(S,Values ++ ObjectSets,[]), %value sets may be parsed as object sets

     %% get information object classes wrongly sent to checkt/3
     %% and update Terror2

    {AddClasses,Terror3} = filter_errors(IsClass,Terror2),

    NewClasses = Classes++AddClasses,

    Cerror = checkc(S,NewClasses,[]),

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

    asn1ct:create_ets_table(inlined_objects,[named_table]),
    {Oerror,ExclO,ExclOS} = checko(S,NewObjects ++
				   NewObjectSets,
				   [],[],[]),
    InlinedObjTuples = ets:tab2list(inlined_objects),
    InlinedObjects = lists:map(Element2,InlinedObjTuples),
    ets:delete(inlined_objects),

    Exporterror = check_exports(S,S#state.module),
    case {Terror3,Verror5,Cerror,Oerror,Exporterror} of
	{[],[],[],[],[]} ->
	    ContextSwitchTs = context_switch_in_spec(),
	    InstanceOf = instance_of_in_spec(),
	    NewTypes = lists:subtract(Types,AddClasses) ++ ContextSwitchTs
		++ InstanceOf,
	    NewValues = lists:subtract(Values,PObjSetNames++ObjectNames++
				       ValueSetNames),
	    {ok,
	     {NewTypes,NewValues,ParameterizedTypes,
	      NewClasses,NewObjects,NewObjectSets},
	     {NewTypes,NewValues,ParameterizedTypes,NewClasses,
	      lists:subtract(NewObjects,ExclO)++InlinedObjects,
	      lists:subtract(NewObjectSets,ExclOS)}};
	_ ->{error,{asn1,lists:flatten([Terror3,Verror5,Cerror,
					Oerror,Exporterror])}}
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

instance_of_in_spec() ->
    case get(instance_of) of
	generate ->
	    erase(instance_of),
	    ['INSTANCE OF'];
	_ ->
	    []
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
	{exports,ExportList} when list(ExportList) ->
	    IsNotDefined =
		fun(X) ->
			case catch get_referenced_type(S,X) of
			    {error,{asn1,_}} ->
				true;
			    _ -> false
			end
		end,
	    case lists:filter(IsNotDefined,ExportList) of
		[] ->
		    [];
		NoDefExp ->
		    GetName =
			fun(T = #'Externaltypereference'{type=N})->
				%%{exported,undefined,entity,N}
				NewS=S#state{type=T,tname=N},
				error({export,"exported undefined entity",NewS})
			end,
		    lists:map(GetName,NoDefExp)
	    end
    end.

checkt(S,[Name|T],Acc) ->
    %%io:format("check_typedef:~p~n",[Name]),
    Result =
	case asn1_db:dbget(S#state.mname,Name) of
	    undefined ->
		error({type,{internal_error,'???'},S});
	    Type when record(Type,typedef) ->
		NewS = S#state{type=Type,tname=Name},
		case catch(check_type(NewS,Type,Type#typedef.typespec)) of
		    {error,Reason} ->
			error({type,Reason,NewS});
		    {'EXIT',Reason} ->
			error({type,{internal_error,Reason},NewS});
		    {asn1_class,_ClassDef} ->
			{asn1_class,Name};
		    pobjectsetdef ->
			{pobjectsetdef,Name};
		    pvalueset ->
			{pvalueset,Name};
		    Ts ->
			case Type#typedef.checked of
			    true -> % already checked and updated
				ok;
			    _ ->
				NewTypeDef = Type#typedef{checked=true,typespec = Ts},
				%io:format("checkt:dbput:~p, ~p~n",[S#state.mname,NewTypeDef#typedef.name]),
				asn1_db:dbput(NewS#state.mname,Name,NewTypeDef), % update the type
				ok
			end
		end
	end,
    case Result of
	ok ->
	    checkt(S,T,Acc);
	_ ->
	    checkt(S,T,[Result|Acc])
    end;
checkt(S,[],Acc) ->
    case check_contextswitchingtypes(S,[]) of
	[] ->
	    lists:reverse(Acc);
	L ->
	    checkt(S,L,Acc)
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

checkv(S,[Name|T],Acc) ->
    %%io:format("check_valuedef:~p~n",[Name]),
    Result = case asn1_db:dbget(S#state.mname,Name) of
		 undefined -> error({value,{internal_error,'???'},S});
		 Value when record(Value,valuedef);
			    record(Value,typedef); %Value set may be parsed as object set.
			    record(Value,pvaluedef);
			    record(Value,pvaluesetdef) ->
		     NewS = S#state{value=Value},
		     case catch(check_value(NewS,Value)) of
			 {error,Reason} ->
			     error({value,Reason,NewS});
			 {'EXIT',Reason} ->
			     error({value,{internal_error,Reason},NewS});
			 {pobjectsetdef} ->
			     {pobjectsetdef,Name};
			 {objectsetdef} ->
			     {objectsetdef,Name};
			 {objectdef} ->
			     %% this is an object, save as typedef
			     #valuedef{checked=C,pos=Pos,name=N,type=Type,
				       value=Def}=Value,
%			     Currmod = S#state.mname,
%			     #type{def=
%				   #'Externaltypereference'{module=Mod,
%							    type=CName}} = Type,
			     ClassName =
				 Type#type.def,
% 				 case Mod of
% 				     Currmod ->
% 					 {objectclassname,CName};
% 				     _ ->
% 					 {objectclassname,Mod,CName}
% 				 end,
			     NewSpec = #'Object'{classname=ClassName,
						 def=Def},
			     NewDef = #typedef{checked=C,pos=Pos,name=N,
					       typespec=NewSpec},
			     asn1_db:dbput(NewS#state.mname,Name,NewDef),
			     {objectdef,Name};
			 {valueset,VSet} ->
			     Pos = asn1ct:get_pos_of_def(Value),
			     CheckedVSDef = #typedef{checked=true,pos=Pos,
						     name=Name,typespec=VSet},
			     asn1_db:dbput(NewS#state.mname,Name,CheckedVSDef),
			     {valueset,Name};
			 V ->
			     %% update the valuedef
			     asn1_db:dbput(NewS#state.mname,Name,V),
			     ok
		     end
	     end,
    case Result of
	ok ->
	    checkv(S,T,Acc);
	_ ->
	    checkv(S,T,[Result|Acc])
    end;
checkv(_S,[],Acc) ->
    lists:reverse(Acc).


checkp(S,[Name|T],Acc) ->
    %io:format("check_ptypedef:~p~n",[Name]),
    Result = case asn1_db:dbget(S#state.mname,Name) of
	undefined ->
	    error({type,{internal_error,'???'},S});
	Type when record(Type,ptypedef) ->
	    NewS = S#state{type=Type,tname=Name},
	    case catch(check_ptype(NewS,Type,Type#ptypedef.typespec)) of
		{error,Reason} ->
		    error({type,Reason,NewS});
		{'EXIT',Reason} ->
		    error({type,{internal_error,Reason},NewS});
		{asn1_class,_ClassDef} ->
		    {asn1_class,Name};
		Ts ->
		    NewType = Type#ptypedef{checked=true,typespec = Ts},
		    asn1_db:dbput(NewS#state.mname,Name,NewType), % update the type
		    ok
	    end
	     end,
    case Result of
	ok ->
	    checkp(S,T,Acc);
	_ ->
	    checkp(S,T,[Result|Acc])
    end;
checkp(_S,[],Acc) ->
    lists:reverse(Acc).




checkc(S,[Name|Cs],Acc) ->
    Result =
	case asn1_db:dbget(S#state.mname,Name) of
	    undefined ->
		error({class,{internal_error,'???'},S});
	    Class  ->
		ClassSpec = if
			       record(Class,classdef) ->
				   Class#classdef.typespec;
			       record(Class,typedef) ->
				   Class#typedef.typespec
			   end,
		NewS = S#state{type=Class,tname=Name},
		case catch(check_class(NewS,ClassSpec)) of
		    {error,Reason} ->
			error({class,Reason,NewS});
		    {'EXIT',Reason} ->
			error({class,{internal_error,Reason},NewS});
		    C ->
			%% update the classdef
			NewClass =
			    if
				record(Class,classdef) ->
				    Class#classdef{checked=true,typespec=C};
				record(Class,typedef) ->
				    #classdef{checked=true,name=Name,typespec=C}
			    end,
			asn1_db:dbput(NewS#state.mname,Name,NewClass),
			ok
		end
	end,
    case Result of
	ok ->
	    checkc(S,Cs,Acc);
	_ ->
	    checkc(S,Cs,[Result|Acc])
    end;
checkc(_S,[],Acc) ->
%%    include_default_class(S#state.mname),
    lists:reverse(Acc).

checko(S,[Name|Os],Acc,ExclO,ExclOS) ->
    Result =
	case asn1_db:dbget(S#state.mname,Name) of
	    undefined ->
		error({type,{internal_error,'???'},S});
	    Object when record(Object,typedef) ->
		NewS = S#state{type=Object,tname=Name},
		case catch(check_object(NewS,Object,Object#typedef.typespec)) of
		    {error,Reason} ->
			error({type,Reason,NewS});
		    {'EXIT',Reason} ->
			error({type,{internal_error,Reason},NewS});
		    {asn1,Reason} ->
			error({type,Reason,NewS});
		    O ->
			NewObj = Object#typedef{checked=true,typespec=O},
			asn1_db:dbput(NewS#state.mname,Name,NewObj),
			if
			    record(O,'Object') ->
				case O#'Object'.gen of
				    true ->
					{ok,ExclO,ExclOS};
				    false ->
					{ok,[Name|ExclO],ExclOS}
				end;
			    record(O,'ObjectSet') ->
				case O#'ObjectSet'.gen of
				    true ->
					{ok,ExclO,ExclOS};
				    false ->
					{ok,ExclO,[Name|ExclOS]}
				end
			end
		end;
	    PObject when record(PObject,pobjectdef) ->
		NewS = S#state{type=PObject,tname=Name},
		case (catch check_pobject(NewS,PObject)) of
		    {error,Reason} ->
			error({type,Reason,NewS});
		    {'EXIT',Reason} ->
			error({type,{internal_error,Reason},NewS});
		    {asn1,Reason} ->
			error({type,Reason,NewS});
		    PO ->
			NewPObj = PObject#pobjectdef{def=PO},
			asn1_db:dbput(NewS#state.mname,Name,NewPObj),
			{ok,[Name|ExclO],ExclOS}
		end;
	    PObjSet when record(PObjSet,pvaluesetdef) ->
		%% this is a parameterized object set. Might be a parameterized
		%% value set, couldn't it?
		NewS = S#state{type=PObjSet,tname=Name},
		case (catch check_pobjectset(NewS,PObjSet)) of
		    {error,Reason} ->
			error({type,Reason,NewS});
		    {'EXIT',Reason} ->
			error({type,{internal_error,Reason},NewS});
		    {asn1,Reason} ->
			error({type,Reason,NewS});
		    POS ->
			%%NewPObjSet = PObjSet#pvaluesetdef{valueset=POS},
			asn1_db:dbput(NewS#state.mname,Name,POS),
			{ok,ExclO,[Name|ExclOS]}
		end
	end,
    case Result of
	{ok,NewExclO,NewExclOS} ->
	    checko(S,Os,Acc,NewExclO,NewExclOS);
	_ ->
	    checko(S,Os,[Result|Acc],ExclO,ExclOS)
    end;
checko(_S,[],Acc,ExclO,ExclOS) ->
    {lists:reverse(Acc),lists:reverse(ExclO),lists:reverse(ExclOS)}.

check_class(S,CDef=#classdef{checked=Ch,name=Name,typespec=TS}) ->
    case Ch of
	true -> TS;
	idle -> TS;
	_ ->
	    NewCDef = CDef#classdef{checked=idle},
	    asn1_db:dbput(S#state.mname,Name,NewCDef),
	    CheckedTS = check_class(S,TS),
	    asn1_db:dbput(S#state.mname,Name,
			  NewCDef#classdef{checked=true,
					   typespec=CheckedTS}),
	    CheckedTS
    end;
check_class(S = #state{mname=M,tname=T},ClassSpec)
  when record(ClassSpec,type) ->
    Def = ClassSpec#type.def,
    case Def of
	#'Externaltypereference'{module=M,type=T} ->
	    #objectclass{fields=Def}; % in case of recursive definitions
	Tref when record(Tref,'Externaltypereference') ->
	    {_,RefType} = get_referenced_type(S,Tref),
% 	    case RefType of
% 		RefClass when record(RefClass,classdef) ->
% 		    check_class(S,RefClass#classdef.typespec)
% 	    end
	    case is_class(S,RefType) of
		true ->
		    check_class(S,get_class_def(S,RefType));
		_ ->
		    error({class,{internal_error,RefType},S})
	    end
    end;
% check_class(S,{objectclassname,ModuleName,ClassName}) when atom(ModuleName),atom(ClassName) ->
%     'fix this';
check_class(S,C) when record(C,objectclass) ->
    NewFieldSpec = check_class_fields(S,C#objectclass.fields),
    C#objectclass{fields=NewFieldSpec};
%check_class(S,{objectclassname,ClassName}) ->
check_class(S,ClassName) ->
    {_,Def} = get_referenced_type(S,ClassName),
    case Def of
	ClassDef when record(ClassDef,classdef) ->
	    case ClassDef#classdef.checked of
		true ->
		    ClassDef#classdef.typespec;
		idle ->
		    ClassDef#classdef.typespec;
		false ->
		    check_class(S,ClassDef#classdef.typespec)
	    end;
	TypeDef when record(TypeDef,typedef) ->
	    %% this case may occur when a definition is a reference
	    %% to a class definition.
	    case TypeDef#typedef.typespec of
		#type{def=Ext} when record(Ext,'Externaltypereference') ->
		    check_class(S,Ext)
	    end
    end;
check_class(_S,{poc,_ObjSet,_Params}) ->
    'fix this later'.

check_class_fields(S,Fields) ->
    check_class_fields(S,Fields,[]).

check_class_fields(S,[F|Fields],Acc) ->
    NewField =
	case element(1,F) of
	    fixedtypevaluefield ->
		{_,Name,Type,Unique,OSpec} = F,
		RefType = check_type(S,#typedef{typespec=Type},Type),
		{fixedtypevaluefield,Name,RefType,Unique,OSpec};
	    object_or_fixedtypevalue_field ->
		{_,Name,Type,Unique,OSpec} = F,
		Cat =
		    case asn1ct_gen:type(asn1ct_gen:get_inner(Type#type.def)) of
			Def when record(Def,typereference);
				 record(Def,'Externaltypereference') ->
			    {_,D} = get_referenced_type(S,Def),
			    D;
			{undefined,user} ->
			    %% neither of {primitive,bif} or {constructed,bif}
%%			    {_,D} = get_referenced_type(S,#typereference{val=Type#type.def}),
			    {_,D} = get_referenced_type(S,#'Externaltypereference'{module=S#state.mname,type=Type#type.def}),
			    D;
			_ ->
			    Type
		    end,
		case Cat of
		    Class when record(Class,classdef) ->
			{objectfield,Name,Type,Unique,OSpec};
		    _ ->
			RefType = check_type(S,#typedef{typespec=Type},Type),
			{fixedtypevaluefield,Name,RefType,Unique,OSpec}
		end;
	    objectset_or_fixedtypevalueset_field ->
		{_,Name,Type,OSpec} = F,
%%		RefType = check_type(S,#typedef{typespec=Type},Type),
		RefType =
		    case (catch check_type(S,#typedef{typespec=Type},Type)) of
			{asn1_class,_ClassDef} ->
			    case if_current_checked_type(S,Type) of
				true ->
				    Type#type.def;
				_ ->
				    check_class(S,Type)
			    end;
			CheckedType when record(CheckedType,type) ->
			    CheckedType;
			_ ->
			    error({class,"internal error, check_class_fields",S})
		    end,
		if
		    record(RefType,'Externaltypereference') ->
			{objectsetfield,Name,Type,OSpec};
		    record(RefType,classdef) ->
			{objectsetfield,Name,Type,OSpec};
		    record(RefType,objectclass) ->
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

if_current_checked_type(S,#type{def=Def}) ->
    CurrentCheckedName = S#state.tname,
    MergedModules = S#state.inputmodules,
 %   CurrentCheckedModule = S#state.mname,
    case Def of
	#'Externaltypereference'{module=CurrentCheckedName,
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



check_pobject(_S,PObject) when record(PObject,pobjectdef) ->
    Def = PObject#pobjectdef.def,
    Def.


check_pobjectset(S,PObjSet) ->
    #pvaluesetdef{pos=Pos,name=Name,args=Args,type=Type,
		  valueset=ValueSet}=PObjSet,
    {Mod,Def} = get_referenced_type(S,Type#type.def),
    case Def of
	#classdef{} ->
	    ClassName = #'Externaltypereference'{module=Mod,
						 type=Def#classdef.name},
	    {valueset,Set} = ValueSet,
%	    ObjectSet = #'ObjectSet'{class={objectclassname,ClassName},
	    ObjectSet = #'ObjectSet'{class=ClassName,
				     set=Set},
	    #pobjectsetdef{pos=Pos,name=Name,args=Args,class=Type#type.def,
			   def=ObjectSet};
	_ ->
	    PObjSet
    end.

check_object(_S,ObjDef,ObjSpec) when (ObjDef#typedef.checked == true) ->
    ObjSpec;
check_object(S,_ObjDef,#'Object'{classname=ClassRef,def=ObjectDef}) ->
    {_,_ClassDef} = get_referenced_type(S,ClassRef),
    NewClassRef = check_externaltypereference(S,ClassRef),
    ClassDef =
	case _ClassDef#classdef.checked of
	    false ->
		#classdef{checked=true,
			  typespec=check_class(S,_ClassDef#classdef.typespec)};
	    _ ->
		_ClassDef
	end,
    NewObj =
	case ObjectDef of
	    Def when tuple(Def), (element(1,Def)==object) ->
		NewSettingList = check_objectdefn(S,Def,ClassDef),
		#'Object'{def=NewSettingList};
%	    Def when tuple(Def), (element(1,Def)=='ObjectFromObject') ->
%		fixa;
	    {po,{object,DefObj},ArgsList} ->
		{_,Object} = get_referenced_type(S,DefObj),%DefObj is a
		%%#'Externalvaluereference' or a #'Externaltypereference'
		%% Maybe this call should be catched and in case of an exception
		%% an nonallocated parameterized object should be returned.
		instantiate_po(S,ClassDef,Object,ArgsList);
	    #'Externalvaluereference'{} ->
		{_,Object} = get_referenced_type(S,ObjectDef),
		check_object(S,Object,Object#typedef.typespec);
	    _  ->
		exit({error,{no_object,ObjectDef},S})
	end,
    Gen = gen_incl(S,NewObj#'Object'.def,
		   (ClassDef#classdef.typespec)#objectclass.fields),
    NewObj#'Object'{classname=NewClassRef,gen=Gen};

%%check_object(S,ObjSetDef,ObjSet=#type{def={pt,ObjSetRef,Args}}) ->
    %% A parameterized

check_object(S,
	     _ObjSetDef,
	     ObjSet=#'ObjectSet'{class=ClassRef}) ->
    {_,ClassDef} = get_referenced_type(S,ClassRef),
    NewClassRef = check_externaltypereference(S,ClassRef),
    UniqueFieldName =
	case (catch get_unique_fieldname(ClassDef)) of
	    {error,'__undefined_'} -> {unique,undefined};
	    {asn1,Msg,_} -> error({class,Msg,S});
	    Other -> Other
	end,
    NewObjSet=
	case ObjSet#'ObjectSet'.set of
	    {'SingleValue',Set} when list(Set) ->
		CheckedSet = check_object_list(S,NewClassRef,Set),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueFieldName),
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=NewSet};
	    {'SingleValue',{definedvalue,ObjName}} ->
		{_,ObjDef} = get_referenced_type(S,#identifier{val=ObjName}),
		#'Object'{def=CheckedObj} =
		    check_object(S,ObjDef,ObjDef#typedef.typespec),
		NewSet = get_unique_valuelist(S,[{ObjDef#typedef.name,
						  CheckedObj}],
					      UniqueFieldName),
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=NewSet};
	    {'SingleValue',#'Externalvaluereference'{value=ObjName}} ->
		{_,ObjDef} = get_referenced_type(S,#identifier{val=ObjName}),
		#'Object'{def=CheckedObj} =
		    check_object(S,ObjDef,ObjDef#typedef.typespec),
		NewSet = get_unique_valuelist(S,[{ObjDef#typedef.name,
						  CheckedObj}],
					      UniqueFieldName),
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=NewSet};
	    ['EXTENSIONMARK'] ->
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=['EXTENSIONMARK']};
	    Set when list(Set) ->
		CheckedSet = check_object_list(S,NewClassRef,Set),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueFieldName),
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=NewSet};
	    {Set,Ext} when list(Set) ->
		CheckedSet = check_object_list(S,NewClassRef,Set++Ext),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueFieldName),
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=NewSet++['EXTENSIONMARK']};
	    {{'SingleValue',Set},Ext} ->
		CheckedSet = check_object_list(S,NewClassRef,
					       merge_sets(Set,Ext)),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueFieldName),
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=NewSet++['EXTENSIONMARK']};
	    {Type,{'EXCEPT',Exclusion}} when record(Type,type) ->
		{_,TDef} = get_referenced_type(S,Type#type.def),
		OS = TDef#typedef.typespec,
		NewSet = reduce_objectset(OS#'ObjectSet'.set,Exclusion),
		NewOS = OS#'ObjectSet'{set=NewSet},
		check_object(S,TDef#typedef{typespec=NewOS},
			     NewOS);
	    #type{def={pt,DefinedObjSet,ParamList}} ->
		{_,PObjSetDef} = get_referenced_type(S,DefinedObjSet),
		instantiate_pos(S,ClassDef,PObjSetDef,ParamList);
	    {ObjDef={object,definedsyntax,_ObjFields},_Ext} ->
		CheckedSet = check_object_list(S,NewClassRef,[ObjDef]),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueFieldName),
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=NewSet++['EXTENSIONMARK']}
	end,
    Gen = gen_incl_set(S,NewObjSet#'ObjectSet'.set,
		       ClassDef),
    NewObjSet#'ObjectSet'{class=NewClassRef,gen=Gen}.


merge_sets(Set,Ext) when list(Set),list(Ext) ->
    Set ++ Ext;
merge_sets(Set,Ext) when list(Ext) ->
    [Set|Ext];
merge_sets(Set,{'SingleValue',Ext}) when list(Set) ->
    Set ++ [Ext];
merge_sets(Set,{'SingleValue',Ext}) ->
    [Set] ++ [Ext].

reduce_objectset(ObjectSet,Exclusion) ->
    case Exclusion of
	{'SingleValue',#'Externalvaluereference'{value=Name}} ->
	    case lists:keysearch(Name,1,ObjectSet) of
		{value,El} ->
		    lists:subtract(ObjectSet,[El]);
		_ ->
		    ObjectSet
	    end
    end.

%% Checks a list of objects or object sets and returns a list of selected
%% information for the code generation.
check_object_list(S,ClassRef,ObjectList) ->
    check_object_list(S,ClassRef,ObjectList,[]).

check_object_list(S,ClassRef,[ObjOrSet|Objs],Acc) ->
    case ObjOrSet of
	ObjDef when tuple(ObjDef),(element(1,ObjDef)==object) ->
	    Def =
		check_object(S,#typedef{typespec=ObjDef},
%			     #'Object'{classname={objectclassname,ClassRef},
			     #'Object'{classname=ClassRef,
				       def=ObjDef}),
	    check_object_list(S,ClassRef,Objs,[{no_name,Def#'Object'.def}|Acc]);
	{'SingleValue',{definedvalue,ObjName}} ->
	    {_,ObjectDef} = get_referenced_type(S,#identifier{val=ObjName}),
	    #'Object'{def=Def} = check_object(S,ObjectDef,ObjectDef#typedef.typespec),
	    check_object_list(S,ClassRef,Objs,[{ObjectDef#typedef.name,Def}|Acc]);
	{'SingleValue',Ref = #'Externalvaluereference'{}} ->
	    {_,ObjectDef} = get_referenced_type(S,Ref),
	    #'Object'{def=Def} = check_object(S,ObjectDef,ObjectDef#typedef.typespec),
	    check_object_list(S,ClassRef,Objs,[{ObjectDef#typedef.name,Def}|Acc]);
	ObjRef when record(ObjRef,'Externalvaluereference') ->
	    {_,ObjectDef} = get_referenced_type(S,ObjRef),
	    #'Object'{def=Def} = check_object(S,ObjectDef,ObjectDef#typedef.typespec),
	    check_object_list(S,ClassRef,Objs,
%%			      [{ObjRef#'Externalvaluereference'.value,Def}|Acc]);
			      [{ObjectDef#typedef.name,Def}|Acc]);
	{'ValueFromObject',{_,Object},FieldName} ->
	    {_,Def} = get_referenced_type(S,Object),
%%	    TypeOrVal = get_fieldname_element(S,Def,FieldName);%% this must result in an object set
	    TypeDef = get_fieldname_element(S,Def,FieldName),
	    (TypeDef#typedef.typespec)#'ObjectSet'.set;
	ObjSet when record(ObjSet,type) ->
	    ObjSetDef =
		case ObjSet#type.def of
		    Ref when record(Ref,typereference);
			     record(Ref,'Externaltypereference') ->
			{_,D} = get_referenced_type(S,ObjSet#type.def),
			D;
		    Other ->
			throw({asn1_error,{'unknown objecset',Other,S}})
		end,
	    #'ObjectSet'{set=ObjectsInSet} =
		check_object(S,ObjSetDef,ObjSetDef#typedef.typespec),
	    AccList = transform_set_to_object_list(ObjectsInSet,[]),
	    check_object_list(S,ClassRef,Objs,AccList++Acc);
	union ->
	    check_object_list(S,ClassRef,Objs,Acc);
	Other ->
	    exit({error,{'unknown object',Other},S})
    end;
%% Finally reverse the accumulated list and if there are any extension
%% marks in the object set put one indicator of that in the end of the
%% list.
check_object_list(_,_,[],Acc) ->
    lists:reverse(Acc).
%%    case lists:member('EXTENSIONMARK',RevAcc) of
%%	true ->
%%	    ExclRevAcc = lists:filter(fun(X)->X /= 'EXTENSIONMARK' end,
%%				      RevAcc),
%%	    ExclRevAcc ++ ['EXTENSIONMARK'];
%%	false ->
%%	    RevAcc
%%    end.


%%  get_fieldname_element/3
%%  gets the type/value/object/... of the referenced element in FieldName
%%  FieldName is a list and may have more than one element.
%%  Each element in FieldName can be either {typefieldreference,AnyFieldName}
%%  or {valuefieldreference,AnyFieldName}
%%  Def is the def of the first object referenced by FieldName
get_fieldname_element(S,Def,[{_RefType,FieldName}]) when record(Def,typedef) ->
    {_,_,ObjComps} = (Def#typedef.typespec)#'Object'.def,
    case lists:keysearch(FieldName,1,ObjComps) of
	{value,{_,TDef}} when record(TDef,typedef) ->
	    %%    ORec = TDef#typedef.typespec, %% XXX This must be made general
% 	    case TDef#typedef.typespec of
% 		ObjSetRec when record(ObjSetRec,'ObjectSet') ->
% 		    ObjSet = ObjSetRec#'ObjectSet'.set;
% 		ObjRec when record(ObjRec,'Object') ->
% 		    %% now get the field in ObjRec that RestFName points out
% 		    %ObjRec
% 		    TDef
% 	    end;
	    TDef;
	{value,{_,VDef}} when record(VDef,valuedef) ->
	    check_value(S,VDef);
	_ ->
	    throw({assigned_object_error,"not_assigned_object",S})
    end;
get_fieldname_element(_S,Def,[{_RefType,_FieldName}|_RestFName])
  when record(Def,typedef) ->
    ok.

transform_set_to_object_list([{Name,_UVal,Fields}|Objs],Acc) ->
    transform_set_to_object_list(Objs,[{Name,{object,generatesyntax,Fields}}|Acc]);
transform_set_to_object_list(['EXTENSIONMARK'|Objs],Acc) ->
%%    transform_set_to_object_list(Objs,['EXTENSIONMARK'|Acc]);
    transform_set_to_object_list(Objs,Acc);
transform_set_to_object_list([],Acc) ->
    Acc.

get_unique_valuelist(_S,ObjSet,{unique,undefined}) -> % no unique field in object
    lists:map(fun({N,{_,_,F}})->{N,F};
		 (V={_,_,_}) ->V end, ObjSet);
get_unique_valuelist(S,ObjSet,UFN) ->
    get_unique_vlist(S,ObjSet,UFN,[]).

get_unique_vlist(S,[],_,Acc) ->
    case catch check_uniqueness(Acc) of
	{asn1_error,_} ->
%	    exit({error,Reason,S});
	    error({'ObjectSet',"not unique objects in object set",S});
	true ->
	    lists:reverse(Acc)
    end;
get_unique_vlist(S,[{ObjName,Obj}|Rest],UniqueFieldName,Acc) ->
    {_,_,Fields} = Obj,
    VDef = get_unique_value(S,Fields,UniqueFieldName),
    get_unique_vlist(S,Rest,UniqueFieldName,
		     [{ObjName,VDef#valuedef.value,Fields}|Acc]);
get_unique_vlist(S,[V={_,_,_}|Rest],UniqueFieldName,Acc) ->
    get_unique_vlist(S,Rest,UniqueFieldName,[V|Acc]).

get_unique_value(S,Fields,UniqueFieldName) ->
    Module = S#state.mname,
    case lists:keysearch(UniqueFieldName,1,Fields) of
	{value,Field} ->
	    case element(2,Field) of
		VDef when record(VDef,valuedef) ->
		    VDef;
		{definedvalue,ValName} ->
		    ValueDef = asn1_db:dbget(Module,ValName),
		    case ValueDef of
			VDef when record(VDef,valuedef) ->
			    ValueDef;
			undefined ->
			    #valuedef{value=ValName}
		    end;
		{'ValueFromObject',Object,Name} ->
		    case Object of
			{object,Ext} when record(Ext,'Externaltypereference') ->
			    OtherModule = Ext#'Externaltypereference'.module,
			    ExtObjName = Ext#'Externaltypereference'.type,
			    ObjDef = asn1_db:dbget(OtherModule,ExtObjName),
			    ObjSpec = ObjDef#typedef.typespec,
			    get_unique_value(OtherModule,element(3,ObjSpec),Name);
			{object,{_,_,ObjName}} ->
			    ObjDef = asn1_db:dbget(Module,ObjName),
			    ObjSpec = ObjDef#typedef.typespec,
			    get_unique_value(Module,element(3,ObjSpec),Name);
			{po,Object,_Params} ->
			    exit({error,{'parameterized object not implemented yet',
					 Object},S})
		    end;
		Value when atom(Value);number(Value) ->
		    #valuedef{value=Value};
		{'CHOICE',{_,Value}} when atom(Value);number(Value) ->
		    #valuedef{value=Value}
	    end;
	false ->
	    exit({error,{'no unique value',Fields,UniqueFieldName},S})
%%	    io:format("WARNING: no unique value in object"),
%%	    exit(uniqueFieldName)
    end.

check_uniqueness(NameValueList) ->
    check_uniqueness1(lists:keysort(2,NameValueList)).

check_uniqueness1([]) ->
    true;
check_uniqueness1([_]) ->
    true;
check_uniqueness1([{_,N,_},{_,N,_}|_Rest]) ->
    throw({asn1_error,{'objects in set must have unique values in UNIQUE fields',N}});
check_uniqueness1([_|Rest]) ->
    check_uniqueness1(Rest).

%% instantiate_po/4
%% ClassDef is the class of Object,
%% Object is the Parameterized object, which is referenced,
%% ArgsList is the list of actual parameters
%% returns an #'Object' record.
instantiate_po(S,_ClassDef,Object,ArgsList) when record(Object,pobjectdef) ->
    FormalParams = get_pt_args(Object),
    MatchedArgs = match_args(FormalParams,ArgsList,[]),
    NewS = S#state{type=Object,parameters=MatchedArgs},
    check_object(NewS,Object,#'Object'{classname=Object#pobjectdef.class,
				    def=Object#pobjectdef.def}).

%% instantiate_pos/4
%% ClassDef is the class of ObjectSetDef,
%% ObjectSetDef is the Parameterized object set, which is referenced
%% on the right side of the assignment,
%% ArgsList is the list of actual parameters, i.e. real objects
instantiate_pos(S,ClassDef,ObjectSetDef,ArgsList) ->
    ClassName = ClassDef#classdef.name,
    FormalParams = get_pt_args(ObjectSetDef),
    Set = case get_pt_spec(ObjectSetDef) of
	      {valueset,_Set} -> _Set;
	      _Set -> _Set
	  end,
    MatchedArgs = match_args(FormalParams,ArgsList,[]),
    NewS = S#state{type=ObjectSetDef,parameters=MatchedArgs},
    check_object(NewS,ObjectSetDef,
		 #'ObjectSet'{class=name2Extref(S#state.mname,ClassName),
			      set=Set}).


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
% 	    case lists:keymember(element(2,C),1,Fields) of
% 		true ->
% 		    true;
% 		false ->
% 		    gen_incl1(S,Fields,CFields)
% 	    end;
	    true; %% should check that field is OPTIONAL or DEFUALT if
                  %% the object lacks this field
	objectfield ->
	    case lists:keysearch(element(2,C),1,Fields) of
		{value,Field} ->
		    Type = element(3,C),
		    {_,ClassDef} = get_referenced_type(S,Type#type.def),
%		    {_,ClassFields,_} = ClassDef#classdef.typespec,
		    #objectclass{fields=ClassFields} =
			ClassDef#classdef.typespec,
		    ObjTDef = element(2,Field),
		    case gen_incl(S,(ObjTDef#typedef.typespec)#'Object'.def,
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

%% first if no unique field in the class return false.(don't generate code)
gen_incl_set(S,Fields,ClassDef) ->
    case catch get_unique_fieldname(ClassDef) of
	Tuple when tuple(Tuple) ->
	    false;
	_ ->
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
gen_incl_set1(S,[Object|Rest],CFields)->
    Fields = element(size(Object),Object),
    case gen_incl1(S,Fields,CFields) of
	true ->
	    true;
	false ->
	    gen_incl_set1(S,Rest,CFields)
    end.

check_objectdefn(S,Def,CDef) when record(CDef,classdef) ->
    WithSyntax = (CDef#classdef.typespec)#objectclass.syntax,
    ClassFields = (CDef#classdef.typespec)#objectclass.fields,
    case Def of
	{object,defaultsyntax,Fields} ->
	    check_defaultfields(S,Fields,ClassFields);
	{object,definedsyntax,Fields} ->
	    {_,WSSpec} = WithSyntax,
	    NewFields =
		case catch( convert_definedsyntax(S,Fields,WSSpec,
						  ClassFields,[])) of
		    {asn1,{_ErrorType,ObjToken,ClassToken}} ->
			throw({asn1,{'match error in object',ObjToken,
				     'found in object',ClassToken,'found in class'}});
		    Err={asn1,_} -> throw(Err);
		    Err={'EXIT',_} -> throw(Err);
		    DefaultFields when list(DefaultFields) ->
			DefaultFields
		end,
	    {object,defaultsyntax,NewFields};
	{object,_ObjectId} -> % This is a DefinedObject
	    fixa;
	Other ->
	    exit({error,{objectdefn,Other}})
    end.

check_defaultfields(S,Fields,ClassFields) ->
    check_defaultfields(S,Fields,ClassFields,[]).

check_defaultfields(_S,[],_ClassFields,Acc) ->
    {object,defaultsyntax,lists:reverse(Acc)};
check_defaultfields(S,[{FName,Spec}|Fields],ClassFields,Acc) ->
    case lists:keysearch(FName,2,ClassFields) of
	{value,CField} ->
	    NewField = convert_to_defaultfield(S,FName,Spec,CField),
	    check_defaultfields(S,Fields,ClassFields,[NewField|Acc]);
	_ ->
	    throw({error,{asn1,{'unvalid field in object',FName}}})
    end.
%%    {object,defaultsyntax,Fields}.

convert_definedsyntax(_S,[],[],_ClassFields,Acc) ->
    lists:reverse(Acc);
convert_definedsyntax(S,Fields,WithSyntax,ClassFields,Acc) ->
    case match_field(S,Fields,WithSyntax,ClassFields) of
	{MatchedField,RestFields,RestWS} ->
	    if
		list(MatchedField) ->
		    convert_definedsyntax(S,RestFields,RestWS,ClassFields,
					  lists:append(MatchedField,Acc));
		true ->
		    convert_definedsyntax(S,RestFields,RestWS,ClassFields,
					  [MatchedField|Acc])
	    end
%%	    throw({error,{asn1,{'unvalid syntax in object',WorS}}})
    end.

match_field(S,Fields,WithSyntax,ClassFields) ->
    match_field(S,Fields,WithSyntax,ClassFields,[]).

match_field(S,Fields,[W|Ws],ClassFields,Acc) when list(W) ->
    case catch(match_optional_field(S,Fields,W,ClassFields,[])) of
	{'EXIT',_} ->
	    match_field(Fields,Ws,ClassFields,Acc); %% add S
%%	{[Result],RestFields} ->
%%	    {Result,RestFields,Ws};
	{Result,RestFields} when list(Result) ->
	    {Result,RestFields,Ws};
	_ ->
	    match_field(S,Fields,Ws,ClassFields,Acc)
    end;
match_field(S,Fields,WithSyntax,ClassFields,_Acc) ->
    match_mandatory_field(S,Fields,WithSyntax,ClassFields,[]).

match_optional_field(_S,RestFields,[],_,Ret) ->
    {Ret,RestFields};
%% An additional optional field within an optional field
match_optional_field(S,Fields,[W|Ws],ClassFields,Ret) when list(W) ->
    case catch match_optional_field(S,Fields,W,ClassFields,[]) of
	{'EXIT',_} ->
	    {Ret,Fields};
	{asn1,{optional_matcherror,_,_}} ->
	    {Ret,Fields};
	{OptionalField,RestFields} ->
	    match_optional_field(S,RestFields,Ws,ClassFields,
				 lists:append(OptionalField,Ret))
    end;
%% identify and skip word
%match_optional_field(S,[#'Externaltypereference'{type=WorS}|Rest],
match_optional_field(S,[{_,_,WorS}|Rest],
		     [WorS|Ws],ClassFields,Ret) ->
    match_optional_field(S,Rest,Ws,ClassFields,Ret);
match_optional_field(S,[],_,ClassFields,Ret) ->
    match_optional_field(S,[],[],ClassFields,Ret);
%% identify and skip comma
match_optional_field(S,[{WorS,_}|Rest],[{WorS,_}|Ws],ClassFields,Ret) ->
    match_optional_field(S,Rest,Ws,ClassFields,Ret);
%% identify and save field data
match_optional_field(S,[Setting|Rest],[{_,W}|Ws],ClassFields,Ret) ->
    WorS =
	case Setting of
	    Type when record(Type,type) -> Type;
%%	    #'Externalvaluereference'{value=WordOrSetting} -> WordOrSetting;
	    {'ValueFromObject',_,_} -> Setting;
	    {object,_,_} -> Setting;
	    {_,_,WordOrSetting} -> WordOrSetting;
%%	    Atom when atom(Atom) -> Atom
	    Other -> Other
	end,
    case lists:keysearch(W,2,ClassFields) of
	false ->
	    throw({asn1,{optional_matcherror,WorS,W}});
	{value,CField} ->
	    NewField = convert_to_defaultfield(S,W,WorS,CField),
	    match_optional_field(S,Rest,Ws,ClassFields,[NewField|Ret])
    end;
match_optional_field(_S,[WorS|_Rest],[W|_Ws],_ClassFields,_Ret) ->
    throw({asn1,{optional_matcherror,WorS,W}}).

match_mandatory_field(_S,[],[],_,[Acc]) ->
    {Acc,[],[]};
match_mandatory_field(_S,[],[],_,Acc) ->
    {Acc,[],[]};
match_mandatory_field(S,[],[H|T],CF,Acc) when list(H) ->
    match_mandatory_field(S,[],T,CF,Acc);
match_mandatory_field(_S,[],WithSyntax,_,_Acc) ->
    throw({asn1,{mandatory_matcherror,[],WithSyntax}});
%match_mandatory_field(_S,Fields,WithSyntax=[W|_Ws],_ClassFields,[Acc]) when list(W) ->
match_mandatory_field(_S,Fields,WithSyntax=[W|_Ws],_ClassFields,Acc) when list(W), length(Acc) >= 1 ->
    {Acc,Fields,WithSyntax};
%% identify and skip word
match_mandatory_field(S,[{_,_,WorS}|Rest],
		      [WorS|Ws],ClassFields,Acc) ->
    match_mandatory_field(S,Rest,Ws,ClassFields,Acc);
%% identify and skip comma
match_mandatory_field(S,[{WorS,_}|Rest],[{WorS,_}|Ws],ClassFields,Ret) ->
    match_mandatory_field(S,Rest,Ws,ClassFields,Ret);
%% identify and save field data
match_mandatory_field(S,[Setting|Rest],[{_,W}|Ws],ClassFields,Acc) ->
    WorS =
	case Setting of
%%	    Atom when atom(Atom) -> Atom;
%%	    #'Externalvaluereference'{value=WordOrSetting} -> WordOrSetting;
	    {object,_,_} -> Setting;
	    {_,_,WordOrSetting} -> WordOrSetting;
	    Type when record(Type,type) -> Type;
	    Other -> Other
	end,
    case lists:keysearch(W,2,ClassFields) of
	false ->
	    throw({asn1,{mandatory_matcherror,WorS,W}});
	{value,CField} ->
	    NewField = convert_to_defaultfield(S,W,WorS,CField),
	    match_mandatory_field(S,Rest,Ws,ClassFields,[NewField|Acc])
    end;

match_mandatory_field(_S,[WorS|_Rest],[W|_Ws],_ClassFields,_Acc) ->
    throw({asn1,{mandatory_matcherror,WorS,W}}).

%% Converts a field of an object from defined syntax to default syntax
convert_to_defaultfield(S,ObjFieldName,ObjFieldSetting,CField)->
    CurrMod = S#state.mname,
    case element(1,CField) of
	typefield ->
	    TypeDef=
		case ObjFieldSetting of
		    TypeRec when record(TypeRec,type) -> TypeRec#type.def;
		    TDef when record(TDef,typedef) ->
			TDef#typedef{typespec=check_type(S,TDef,
							 TDef#typedef.typespec)};
		    _ -> ObjFieldSetting
		end,
	    Type =
		if
		    record(TypeDef,typedef) -> TypeDef;
		    true ->
			case asn1ct_gen:type(asn1ct_gen:get_inner(TypeDef)) of
			    ERef = #'Externaltypereference'{module=CurrMod} ->
				{_,T} = get_referenced_type(S,ERef),
				T#typedef{checked=true,
					  typespec=check_type(S,T,
							      T#typedef.typespec)};
			    ERef = #'Externaltypereference'{module=ExtMod} ->
				{_,T} = get_referenced_type(S,ERef),
				#typedef{name=Name} = T,
				check_type(S,T,T#typedef.typespec),
				#typedef{checked=true,
					 name={ExtMod,Name},
					 typespec=ERef};
			    Bif when Bif=={primitive,bif};Bif=={constructed,bif} ->
				T = check_type(S,#typedef{typespec=ObjFieldSetting},
					       ObjFieldSetting),
				#typedef{checked=true,name=Bif,typespec=T};
			    _ ->
				{Mod,T} =
				    %% get_referenced_type(S,#typereference{val=ObjFieldSetting}),
				    get_referenced_type(S,#'Externaltypereference'{module=S#state.mname,type=ObjFieldSetting}),
				case Mod of
				    CurrMod ->
					T;
				    ExtMod ->
					#typedef{name=Name} = T,
					T#typedef{name={ExtMod,Name}}
				end
			end
		end,
	    {ObjFieldName,Type};
	fixedtypevaluefield ->
	    case ObjFieldName of
		Val when atom(Val) ->
		    %% ObjFieldSetting can be a value,an objectidentifiervalue,
		    %% an element in an enumeration or namednumberlist etc.
		    ValRef =
			case ObjFieldSetting of
			    #'Externalvaluereference'{} -> ObjFieldSetting;
			    {'ValueFromObject',{_,ObjRef},FieldName} ->
				{_,Object} = get_referenced_type(S,ObjRef),
				ChObject = check_object(S,Object,
							Object#typedef.typespec),
				get_fieldname_element(S,Object#typedef{typespec=ChObject},
						      FieldName);
			    #valuedef{} ->
				ObjFieldSetting;
			    _ ->
				#identifier{val=ObjFieldSetting}
			end,
		    case ValRef of
			#valuedef{} ->
			    {ObjFieldName,check_value(S,ValRef)};
			_ ->
			    ValDef =
				case catch get_referenced_type(S,ValRef) of
				    {error,_} ->
					check_value(S,#valuedef{name=Val,
								type=element(3,CField),
								value=ObjFieldSetting});
				    {_,VDef} when record(VDef,valuedef) ->
					check_value(S,VDef);%% XXX
				    {_,VDef} ->
					check_value(S,#valuedef{name=Val,
								type=element(3,CField),
								value=VDef})
				end,
			    {ObjFieldName,ValDef}
		    end;
		Val ->
		    {ObjFieldName,Val}
	    end;
	fixedtypevaluesetfield ->
	    {ObjFieldName,ObjFieldSetting};
	objectfield ->
	    ObjectSpec =
		case ObjFieldSetting of
		    Ref when record(Ref,typereference);record(Ref,identifier);
			     record(Ref,'Externaltypereference');
			     record(Ref,'Externalvaluereference') ->
			{_,R} = get_referenced_type(S,ObjFieldSetting),
			R;
		    {'ValueFromObject',{_,ObjRef},FieldName} ->
			%% This is an ObjectFromObject
			{_,Object} = get_referenced_type(S,ObjRef),
			ChObject = check_object(S,Object,
						Object#typedef.typespec),
			_ObjFromObj=
			    get_fieldname_element(S,Object#typedef{
						      typespec=ChObject},
						  FieldName);
			%%ClassName = ObjFromObj#'Object'.classname,
			%%#typedef{name=,
			%%	 typespec=
			%%	 ObjFromObj#'Object'{classname=
			%%			     {objectclassname,ClassName}}};
		    {object,_,_} ->
			%% An object defined inlined in another object
			#type{def=Ref} = element(3,CField),
% 			CRef = case Ref of
% 				   #'Externaltypereference'{module=CurrMod,
% 							    type=CName} ->
% 				       CName;
% 				    #'Externaltypereference'{module=ExtMod,
% 							    type=CName} ->
% 				       {ExtMod,CName}
% 			       end,
			InlinedObjName=
			    list_to_atom(lists:concat([S#state.tname]++
						      ['_',ObjFieldName])),
%			ObjSpec = #'Object'{classname={objectclassname,CRef},
			ObjSpec = #'Object'{classname=Ref,
					    def=ObjFieldSetting},
			CheckedObj=
			    check_object(S,#typedef{typespec=ObjSpec},ObjSpec),
			InlObj = #typedef{checked=true,name=InlinedObjName,
					  typespec=CheckedObj},
			asn1ct_gen:insert_once(inlined_objects,{InlinedObjName,
								InlinedObjName}),
			asn1_db:dbput(S#state.mname,InlinedObjName,InlObj),
			InlObj;
		    #type{def=Eref} when record(Eref,'Externaltypereference') ->
			{_,R} = get_referenced_type(S,Eref),
			R;
		    _ ->
%%			{_,R} = get_referenced_type(S,#typereference{val=ObjFieldSetting}),
			{_,R} = get_referenced_type(S,#'Externaltypereference'{module=S#state.mname,type=ObjFieldSetting}),
			R
		end,
	    {ObjFieldName,
	     ObjectSpec#typedef{checked=true,
		      typespec=check_object(S,ObjectSpec,
					    ObjectSpec#typedef.typespec)}};
	variabletypevaluefield ->
	    {ObjFieldName,ObjFieldSetting};
	variabletypevaluesetfield ->
	    {ObjFieldName,ObjFieldSetting};
	objectsetfield ->
	    {_,ObjSetSpec} =
		case ObjFieldSetting of
		    Ref when record(Ref,'Externaltypereference');
			     record(Ref,'Externalvaluereference') ->
			get_referenced_type(S,ObjFieldSetting);
		    ObjectList when list(ObjectList) ->
			%% an objctset defined in the object,though maybe
			%% parsed as a SequenceOfValue
			%% The ObjectList may be a list of references to
			%% objects, a ValueFromObject
			{_,_,Type,_} = CField,
			ClassDef = Type#type.def,
			case ClassDef#'Externaltypereference'.module of
			    CurrMod ->
				ClassDef#'Externaltypereference'.type;
			    ExtMod ->
				{ExtMod,
				 ClassDef#'Externaltypereference'.type}
			end,
			{no_name,
			 #typedef{typespec=
				  #'ObjectSet'{class=
%					       {objectclassname,ClassRef},
					       ClassDef,
					       set=ObjectList}}};
		    ObjectSet={'SingleValue',_} ->
			%% a Union of defined objects
			{_,_,Type,_} = CField,
			ClassDef = Type#type.def,
% 			ClassRef =
% 			    case ClassDef#'Externaltypereference'.module of
% 				CurrMod ->
% 				    ClassDef#'Externaltypereference'.type;
% 				ExtMod ->
% 				    {ExtMod,
% 				     ClassDef#'Externaltypereference'.type}
% 			    end,
			{no_name,
%			 #typedef{typespec=#'ObjectSet'{class={objectclassname,ClassRef},
			 #typedef{typespec=#'ObjectSet'{class=ClassDef,
							set=ObjectSet}}};
		    {object,_,[#type{def={'TypeFromObject',
					 {object,RefedObj},
					 FieldName}}]} ->
			%% This case occurs when an ObjectSetFromObjects
			%% production is used
			{M,Def} = get_referenced_type(S,RefedObj),
			{M,get_fieldname_element(S,Def,FieldName)};
		    #type{def=Eref} when
			  record(Eref,'Externaltypereference') ->
			get_referenced_type(S,Eref);
		    _ ->
%%			get_referenced_type(S,#typereference{val=ObjFieldSetting})
			get_referenced_type(S,#'Externaltypereference'{module=S#state.mname,type=ObjFieldSetting})
		end,
	    {ObjFieldName,
	     ObjSetSpec#typedef{checked=true,
				typespec=check_object(S,ObjSetSpec,
						      ObjSetSpec#typedef.typespec)}}
    end.

check_value(OldS,V) when record(V,pvaluesetdef) ->
    #pvaluesetdef{checked=Checked,type=Type} = V,
    case Checked of
	true -> V;
	{error,_} -> V;
	false ->
	    case get_referenced_type(OldS,Type#type.def) of
		{_,Class} when record(Class,classdef) ->
		    throw({pobjectsetdef});
		_ -> continue
	    end
    end;
check_value(_OldS,V) when record(V,pvaluedef) ->
    %% Fix this case later
    V;
check_value(OldS,V) when record(V,typedef) ->
    %% This case when a value set has been parsed as an object set.
    %% It may be a value set
    #typedef{typespec=TS} = V,
    case TS of
	#'ObjectSet'{class=ClassRef} ->
	    {_,TSDef} = get_referenced_type(OldS,ClassRef),
	    %%IsObjectSet(TSDef);
	    case TSDef of
		#classdef{} -> throw({objectsetdef});
		#typedef{typespec=#type{def=Eref}} when
		      record(Eref,'Externaltypereference') ->
		    %% This case if the class reference is a defined
		    %% reference to class
		    check_value(OldS,V#typedef{typespec=TS#'ObjectSet'{class=Eref}});
		#typedef{} ->
		    % an ordinary value set with a type in #typedef.typespec
		    ValueSet = TS#'ObjectSet'.set,
		    Type=check_type(OldS,TSDef,TSDef#typedef.typespec),
		    Value = check_value(OldS,#valuedef{type=Type,
						       value=ValueSet}),
		    {valueset,Type#type{constraint=Value#valuedef.value}}
	    end;
	_ ->
	    throw({objectsetdef})
    end;
check_value(S,#valuedef{pos=Pos,name=Name,type=Type,
			  value={valueset,Constr}}) ->
    NewType = Type#type{constraint=[Constr]},
    {valueset,
     check_type(S,#typedef{pos=Pos,name=Name,typespec=NewType},NewType)};
check_value(OldS=#state{recordtopname=TopName},V) when record(V,valuedef) ->
    #valuedef{name=Name,checked=Checked,type=Vtype,value=Value} = V,
    case Checked of
	true ->
	    V;
	{error,_} ->
	    V;
	false ->
	    Def = Vtype#type.def,
	    Constr = Vtype#type.constraint,
	    S = OldS#state{type=Vtype,tname=Def,value=V,vname=Name},
	    NewDef =
		case Def of
		    Ext when record(Ext,'Externaltypereference') ->
			RecName = Ext#'Externaltypereference'.type,
			{_,Type} = get_referenced_type(S,Ext),
			%% If V isn't a value but an object Type is a #classdef{}
			case Type of
			    #classdef{} ->
				throw({objectdef});
			    #typedef{} ->
				case is_contextswitchtype(Type) of
				    true ->
					#valuedef{value=CheckedVal}=
					    check_value(S,V#valuedef{type=Type#typedef.typespec}),
					#newv{value=CheckedVal};
				    _ ->
					#valuedef{value=CheckedVal}=
					    check_value(S#state{recordtopname=[RecName|TopName]},
							V#valuedef{type=Type#typedef.typespec}),
					#newv{value=CheckedVal}
				end
			end;
		    'ANY' ->
			throw({error,{asn1,{'cant check value of type',Def}}});
		    'INTEGER' ->
			validate_integer(S,Value,[],Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    {'INTEGER',NamedNumberList} ->
			validate_integer(S,Value,NamedNumberList,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    {'BIT STRING',NamedNumberList} ->
			validate_bitstring(S,Value,NamedNumberList,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'NULL' ->
			validate_null(S,Value,Constr),
			#newv{};
		    'OBJECT IDENTIFIER' ->
			validate_objectidentifier(S,Value,Constr),
			#newv{value = normalize_value(S,Vtype,Value,[])};
		    'ObjectDescriptor' ->
			validate_objectdescriptor(S,Value,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    {'ENUMERATED',NamedNumberList} ->
			validate_enumerated(S,Value,NamedNumberList,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'BOOLEAN'->
			validate_boolean(S,Value,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'OCTET STRING' ->
			validate_octetstring(S,Value,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'NumericString' ->
			validate_restrictedstring(S,Value,Def,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'TeletexString' ->
			validate_restrictedstring(S,Value,Def,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'VideotexString' ->
			validate_restrictedstring(S,Value,Def,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'UTCTime' ->
			#newv{value=normalize_value(S,Vtype,Value,[])};
%			exit({'cant check value of type' ,Def});
		    'GeneralizedTime' ->
			#newv{value=normalize_value(S,Vtype,Value,[])};
%			exit({'cant check value of type' ,Def});
		    'GraphicString' ->
			validate_restrictedstring(S,Value,Def,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'VisibleString' ->
			validate_restrictedstring(S,Value,Def,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'GeneralString' ->
			validate_restrictedstring(S,Value,Def,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'PrintableString' ->
			validate_restrictedstring(S,Value,Def,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'IA5String' ->
			validate_restrictedstring(S,Value,Def,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
		    'BMPString' ->
			validate_restrictedstring(S,Value,Def,Constr),
			#newv{value=normalize_value(S,Vtype,Value,[])};
%%		    'UniversalString' -> %added 6/12 -00
%%			#newv{value=validate_restrictedstring(S,Value,Def,Constr)};
		    Seq when record(Seq,'SEQUENCE') ->
			SeqVal = validate_sequence(S,Value,
						   Seq#'SEQUENCE'.components,
						   Constr),
			#newv{value=normalize_value(S,Vtype,SeqVal,TopName)};
		    {'SEQUENCE OF',Components} ->
			validate_sequenceof(S,Value,Components,Constr),
			#newv{value=normalize_value(S,Vtype,Value,TopName)};
		    {'CHOICE',Components} ->
			validate_choice(S,Value,Components,Constr),
			#newv{value=normalize_value(S,Vtype,Value,TopName)};
		    Set when record(Set,'SET') ->
			validate_set(S,Value,Set#'SET'.components,
					      Constr),
			#newv{value=normalize_value(S,Vtype,Value,TopName)};
		    {'SET OF',Components} ->
			validate_setof(S,Value,Components,Constr),
			#newv{value=normalize_value(S,Vtype,Value,TopName)};
		    Other ->
			exit({'cant check value of type' ,Other})
		end,
	    case NewDef#newv.value of
		unchanged ->
		    V#valuedef{checked=true,value=Value};
		ok ->
		    V#valuedef{checked=true,value=Value};
		{error,Reason} ->
		    V#valuedef{checked={error,Reason},value=Value};
		_V ->
		    V#valuedef{checked=true,value=_V}
	    end
    end.

is_contextswitchtype(#typedef{name='EXTERNAL'})->
    true;
is_contextswitchtype(#typedef{name='EMBEDDED PDV'}) ->
    true;
is_contextswitchtype(#typedef{name='CHARACTER STRING'}) ->
    true;
is_contextswitchtype(_) ->
    false.

% validate_integer(S,{identifier,Pos,Id},NamedNumberList,Constr) ->
%     case lists:keysearch(Id,1,NamedNumberList) of
% 	{value,_} -> ok;
% 	false -> error({value,"unknown NamedNumber",S})
%     end;
%% This case occurs when there is a valuereference
validate_integer(S=#state{mname=M},
		 #'Externalvaluereference'{module=M,value=Id},
		 NamedNumberList,_Constr) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> error({value,"unknown NamedNumber",S})
    end;
validate_integer(S,Id,NamedNumberList,_Constr) when atom(Id) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> error({value,"unknown NamedNumber",S})
    end;
validate_integer(_S,Value,_NamedNumberList,Constr) when integer(Value) ->
    check_integer_range(Value,Constr).

check_integer_range(Int,Constr) when list(Constr) ->
    NewConstr = [X || #constraint{c=X} <- Constr],
    check_constr(Int,NewConstr);

check_integer_range(_Int,_Constr) ->
    %%io:format("~p~n",[Constr]),
    ok.

check_constr(Int,[{'ValueRange',Lb,Ub}|T]) when Int >= Lb, Int =< Ub ->
    check_constr(Int,T);
check_constr(_Int,[]) ->
    ok.

validate_bitstring(_S,_Value,_NamedNumberList,_Constr) ->
    ok.

validate_null(_S,'NULL',_Constr) ->
    ok.

%%------------
%% This can be removed when the old parser is removed
%% The function removes 'space' atoms from the list

is_space_list([H],Acc) ->
    lists:reverse([H|Acc]);
is_space_list([H,space|T],Acc) ->
    is_space_list(T,[H|Acc]);
is_space_list([],Acc) ->
    lists:reverse(Acc);
is_space_list([H|T],Acc) ->
    is_space_list(T,[H|Acc]).

validate_objectidentifier(S,L,_) ->
    case is_space_list(L,[]) of
	NewL when list(NewL) ->
	    case validate_objectidentifier1(S,NewL) of
		NewL2 when list(NewL2) ->
		    list_to_tuple(NewL2);
		Other -> Other
	    end;
	{error,_} ->
	    error({value, "illegal OBJECT IDENTIFIER", S})
    end.

validate_objectidentifier1(S, [Id|T]) when record(Id,'Externalvaluereference') ->
    case catch get_referenced_type(S,Id) of
	{_,V} when record(V,valuedef) ->
	    case check_value(S,V) of
		#valuedef{type=#type{def='OBJECT IDENTIFIER'},
			  checked=true,value=Value} when tuple(Value) ->
		    validate_objectid(S, T, lists:reverse(tuple_to_list(Value)));
		_ ->
		    error({value, "illegal OBJECT IDENTIFIER", S})
	    end;
	_ ->
	    validate_objectid(S, [Id|T], [])
    end;
validate_objectidentifier1(S,V) ->
    validate_objectid(S,V,[]).

validate_objectid(_, [], Acc) ->
    lists:reverse(Acc);
validate_objectid(S, [Value|Vrest], Acc) when integer(Value) ->
    validate_objectid(S, Vrest, [Value|Acc]);
validate_objectid(S, [{'NamedNumber',_Name,Value}|Vrest], Acc)
  when integer(Value) ->
    validate_objectid(S, Vrest, [Value|Acc]);
validate_objectid(S, [Id|Vrest], Acc)
  when record(Id,'Externalvaluereference') ->
    case catch get_referenced_type(S, Id) of
	{_,V} when record(V,valuedef) ->
	    case check_value(S, V) of
		#valuedef{checked=true,value=Value} when integer(Value) ->
		    validate_objectid(S, Vrest, [Value|Acc]);
		_ ->
		    error({value, "illegal OBJECT IDENTIFIER", S})
	    end;
	_ ->
	    case reserved_objectid(Id#'Externalvaluereference'.value, Acc) of
		Value when integer(Value) ->
		    validate_objectid(S, Vrest, [Value|Acc]);
		false ->
		    error({value, "illegal OBJECT IDENTIFIER", S})
	    end
    end;
validate_objectid(S, [{Atom,Value}],[]) when atom(Atom),integer(Value) ->
    %% this case when an OBJECT IDENTIFIER value has been parsed as a
    %% SEQUENCE value
    Rec = #'Externalvaluereference'{module=S#state.mname,
				    value=Atom},
    validate_objectidentifier1(S,[Rec,Value]);
validate_objectid(S, [{Atom,EVRef}],[])
  when atom(Atom),record(EVRef,'Externalvaluereference') ->
    %% this case when an OBJECT IDENTIFIER value has been parsed as a
    %% SEQUENCE value OTP-4354
    Rec = #'Externalvaluereference'{module=S#state.mname,
				    value=Atom},
    validate_objectidentifier1(S,[Rec,EVRef]);
validate_objectid(S, _V, _Acc) ->
    error({value, "illegal OBJECT IDENTIFIER",S}).


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





validate_objectdescriptor(_S,_Value,_Constr) ->
    ok.

validate_enumerated(S,Id,NamedNumberList,_Constr) when atom(Id) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> error({value,"unknown ENUMERATED",S})
    end;
validate_enumerated(S,{identifier,_Pos,Id},NamedNumberList,_Constr) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> error({value,"unknown ENUMERATED",S})
    end;
validate_enumerated(S,#'Externalvaluereference'{value=Id},
		    NamedNumberList,_Constr) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> error({value,"unknown ENUMERATED",S})
    end.

validate_boolean(_S,_Value,_Constr) ->
    ok.

validate_octetstring(_S,_Value,_Constr) ->
    ok.

validate_restrictedstring(_S,_Value,_Def,_Constr) ->
    ok.

validate_sequence(S=#state{type=Vtype},Value,_Components,_Constr) ->
    case Vtype of
	#type{tag=[{tag,'UNIVERSAL',8,'IMPLICIT',32}]} ->
	    %% this is an 'EXTERNAL' (or INSTANCE OF)
	    case Value of
		[{identification,_}|_RestVal] ->
		    to_EXTERNAL1990(S,Value);
		_ ->
		    Value
	    end;
	_ ->
	    Value
    end.

validate_sequenceof(_S,_Value,_Components,_Constr) ->
    ok.

validate_choice(_S,_Value,_Components,_Constr) ->
    ok.

validate_set(_S,_Value,_Components,_Constr) ->
    ok.

validate_setof(_S,_Value,_Components,_Constr) ->
    ok.

to_EXTERNAL1990(S,[{identification,{'CHOICE',{syntax,Stx}}}|Rest]) ->
    to_EXTERNAL1990(S,Rest,[{'direct-reference',Stx}]);
to_EXTERNAL1990(S,[{identification,{'CHOICE',{'presentation-context-id',I}}}|Rest]) ->
    to_EXTERNAL1990(S,Rest,[{'indirect-reference',I}]);
to_EXTERNAL1990(S,[{identification,{'CHOICE',{'context-negotiation',[{_,PCid},{_,TrStx}]}}}|Rest]) ->
    to_EXTERNAL1990(S,Rest,[{'indirect-reference',PCid},{'direct-reference',TrStx}]);
to_EXTERNAL1990(S,_) ->
    error({value,"illegal value in EXTERNAL type",S}).

to_EXTERNAL1990(S,[V={'data-value-descriptor',_}|Rest],Acc) ->
    to_EXTERNAL1990(S,Rest,[V|Acc]);
to_EXTERNAL1990(_S,[{'data-value',Val}],Acc) ->
    Encoding = {encoding,{'CHOICE',{'octet-aligned',Val}}},
    lists:reverse([Encoding|Acc]);
to_EXTERNAL1990(S,_,_) ->
    error({value,"illegal value in EXTERNAL type",S}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Functions to normalize the default values of SEQUENCE
%% and SET components into Erlang valid format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
normalize_value(_,_,mandatory,_) ->
    mandatory;
normalize_value(_,_,'OPTIONAL',_) ->
    'OPTIONAL';
normalize_value(S,Type,{'DEFAULT',Value},NameList) ->
    case catch get_canonic_type(S,Type,NameList) of
	{'BOOLEAN',CType,_} ->
	    normalize_boolean(S,Value,CType);
	{'INTEGER',CType,_} ->
	    normalize_integer(S,Value,CType);
	{'BIT STRING',CType,_} ->
	    normalize_bitstring(S,Value,CType);
	{'OCTET STRING',CType,_} ->
	    normalize_octetstring(S,Value,CType);
	{'NULL',_CType,_} ->
	    %%normalize_null(Value);
	    'NULL';
	{'OBJECT IDENTIFIER',_,_} ->
	    normalize_objectidentifier(S,Value);
	{'ObjectDescriptor',_,_} ->
	    normalize_objectdescriptor(Value);
	{'REAL',_,_} ->
	    normalize_real(Value);
	{'ENUMERATED',CType,_} ->
	    normalize_enumerated(Value,CType);
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
	_ ->
	    io:format("WARNING: could not check default value ~p~n",[Value]),
	    Value
    end;
normalize_value(S,Type,Val,NameList) ->
    normalize_value(S,Type,{'DEFAULT',Val},NameList).

normalize_boolean(S,{Name,Bool},CType) when atom(Name) ->
    normalize_boolean(S,Bool,CType);
normalize_boolean(_,true,_) ->
    true;
normalize_boolean(_,false,_) ->
    false;
normalize_boolean(S,Bool=#'Externalvaluereference'{},CType) ->
    get_normalized_value(S,Bool,CType,fun normalize_boolean/3,[]);
normalize_boolean(_,Other,_) ->
    throw({error,{asn1,{'invalid default value',Other}}}).

normalize_integer(_S,Int,_) when integer(Int) ->
    Int;
normalize_integer(_S,{Name,Int},_) when atom(Name),integer(Int) ->
    Int;
normalize_integer(S,{Name,Int=#'Externalvaluereference'{}},
		  Type) when atom(Name) ->
    normalize_integer(S,Int,Type);
normalize_integer(S,Int=#'Externalvaluereference'{value=Name},Type) ->
    case Type of
	NNL when list(NNL) ->
	    case lists:keysearch(Name,1,NNL) of
		{value,{Name,Val}} ->
		    Val;
		false ->
		    get_normalized_value(S,Int,Type,
					 fun normalize_integer/3,[])
	    end;
	_ ->
	    get_normalized_value(S,Int,Type,fun normalize_integer/3,[])
    end;
normalize_integer(_,Int,_) ->
    exit({'Unknown INTEGER value',Int}).

normalize_bitstring(S,Value,Type)->
    %% There are four different Erlang formats of BIT STRING:
    %% 1 - a list of ones and zeros.
    %% 2 - a list of atoms.
    %% 3 - as an integer, for instance in hexadecimal form.
    %% 4 - as a tuple {Unused, Binary} where Unused is an integer
    %%   and tells how many bits of Binary are unused.
    %%
    %% normalize_bitstring/3 transforms Value according to:
    %% A to 3,
    %% B to 1,
    %% C to 1 or 3
    %% D to 2,
    %% Value can be on format:
    %% A - {hstring, String}, where String is a hexadecimal string.
    %% B - {bstring, String}, where String is a string on bit format
    %% C - #'Externalvaluereference'{value=V}, where V is a defined value
    %% D - list of #'Externalvaluereference', where each value component
    %%     is an identifier corresponing to NamedBits in Type.
    case Value of
	{hstring,String} when list(String) ->
	    hstring_to_int(String);
	{bstring,String} when list(String) ->
	    bstring_to_bitlist(String);
	Rec when record(Rec,'Externalvaluereference') ->
	    get_normalized_value(S,Value,Type,
				 fun normalize_bitstring/3,[]);
	RecList when list(RecList) ->
	    case Type of
		NBL when list(NBL) ->
		    F = fun(#'Externalvaluereference'{value=Name}) ->
				case lists:keysearch(Name,1,NBL) of
				    {value,{Name,_}} ->
					Name;
				    Other ->
					throw({error,Other})
				end;
			   (Other) ->
				throw({error,Other})
			end,
		    case catch lists:map(F,RecList) of
			{error,Reason} ->
			    io:format("WARNING: default value not "
				      "compatible with type definition ~p~n",
				      [Reason]),
			    Value;
			NewList ->
			    NewList
		    end;
		_ ->
		    io:format("WARNING: default value not "
			      "compatible with type definition ~p~n",
			      [RecList]),
		    Value
	    end;
	{Name,String} when atom(Name) ->
	    normalize_bitstring(S,String,Type);
	Other ->
	    io:format("WARNING: illegal default value ~p~n",[Other]),
	    Value
    end.

hstring_to_int(L) when list(L) ->
    hstring_to_int(L,0).
hstring_to_int([H|T],Acc) when H >= $A, H =< $F ->
    hstring_to_int(T,(Acc bsl 4) + (H - $A + 10) ) ;
hstring_to_int([H|T],Acc) when H >= $0, H =< $9 ->
    hstring_to_int(T,(Acc bsl 4) + (H - $0));
hstring_to_int([],Acc) ->
    Acc.

bstring_to_bitlist([H|T]) when H == $0; H == $1 ->
    [H - $0 | bstring_to_bitlist(T)];
bstring_to_bitlist([]) ->
    [].

%% normalize_octetstring/1 changes representation of input Value to a
%% list of octets.
%% Format of Value is one of:
%% {bstring,String} each element in String corresponds to one bit in an octet
%% {hstring,String} each element in String corresponds to one byte in an octet
%% #'Externalvaluereference'
normalize_octetstring(S,Value,CType) ->
    case Value of
	{bstring,String} ->
	    bstring_to_octetlist(String);
	{hstring,String} ->
	    hstring_to_octetlist(String);
	Rec when record(Rec,'Externalvaluereference') ->
	    get_normalized_value(S,Value,CType,
				 fun normalize_octetstring/3,[]);
	{Name,String} when atom(Name) ->
	    normalize_octetstring(S,String,CType);
	List when list(List) ->
	    %% check if list elements are valid octet values
	    lists:map(fun([])-> ok;
			 (H)when H > 255->
			      io:format("WARNING: not legal octet value ~p in OCTET STRING, ~p~n",[H,List]);
			 (_)-> ok
		      end, List),
	    List;
	Other ->
	    io:format("WARNING: unknown default value ~p~n",[Other]),
	    Value
    end.


bstring_to_octetlist([]) ->
    [];
bstring_to_octetlist([H|T]) when H == $0 ; H == $1 ->
    bstring_to_octetlist(T,6,[(H - $0) bsl 7]).
bstring_to_octetlist([H|T],0,[Hacc|Tacc]) when H == $0; H == $1 ->
    bstring_to_octetlist(T, 7, [0,Hacc + (H -$0)| Tacc]);
bstring_to_octetlist([H|T],BSL,[Hacc|Tacc]) when H == $0; H == $1 ->
    bstring_to_octetlist(T, BSL-1, [Hacc + ((H - $0) bsl BSL)| Tacc]);
bstring_to_octetlist([],7,[0|Acc]) ->
    lists:reverse(Acc);
bstring_to_octetlist([],_,Acc) ->
    lists:reverse(Acc).

hstring_to_octetlist([]) ->
    [];
hstring_to_octetlist(L) ->
    hstring_to_octetlist(L,4,[]).
hstring_to_octetlist([H|T],0,[Hacc|Tacc]) when H >= $A, H =< $F ->
    hstring_to_octetlist(T,4,[Hacc + (H - $A + 10)|Tacc]);
hstring_to_octetlist([H|T],BSL,Acc) when H >= $A, H =< $F ->
    hstring_to_octetlist(T,0,[(H - $A + 10) bsl BSL|Acc]);
hstring_to_octetlist([H|T],0,[Hacc|Tacc]) when H >= $0; H =< $9 ->
    hstring_to_octetlist(T,4,[Hacc + (H - $0)|Tacc]);
hstring_to_octetlist([H|T],BSL,Acc) when H >= $0; H =< $9 ->
    hstring_to_octetlist(T,0,[(H - $0) bsl BSL|Acc]);
hstring_to_octetlist([],_,Acc) ->
    lists:reverse(Acc).

normalize_objectidentifier(S,Value) ->
    validate_objectidentifier(S,Value,[]).

normalize_objectdescriptor(Value) ->
    Value.

normalize_real(Value) ->
    Value.

normalize_enumerated(#'Externalvaluereference'{value=V},CType)
  when list(CType) ->
    normalize_enumerated2(V,CType);
normalize_enumerated(Value,CType) when atom(Value),list(CType) ->
    normalize_enumerated2(Value,CType);
normalize_enumerated({Name,EnumV},CType) when atom(Name) ->
    normalize_enumerated(EnumV,CType);
normalize_enumerated(Value,{CType1,CType2}) when list(CType1), list(CType2)->
    normalize_enumerated(Value,CType1++CType2);
normalize_enumerated(V,CType) ->
    io:format("WARNING: Enumerated unknown type ~p~n",[CType]),
    V.
normalize_enumerated2(V,Enum) ->
    case lists:keysearch(V,1,Enum) of
	{value,{Val,_}} -> Val;
	_ ->
	    io:format("WARNING: Enumerated value is not correct ~p~n",[V]),
	    V
    end.

normalize_choice(S,{'CHOICE',{C,V}},CType,NameList) when atom(C) ->
    Value =
	case V of
	    Rec when record(Rec,'Externalvaluereference') ->
		get_normalized_value(S,V,CType,
				     fun normalize_choice/4,
				     [NameList]);
	    _ -> V
	end,
    case catch lists:keysearch(C,#'ComponentType'.name,CType) of
	{value,#'ComponentType'{typespec=CT,name=Name}} ->
	    {C,normalize_value(S,CT,{'DEFAULT',Value},
			       [Name|NameList])};
	Other ->
	    io:format("WARNING: Wrong format of type/value ~p/~p~n",
		      [Other,Value]),
	    {C,Value}
    end;
normalize_choice(S,{'DEFAULT',ValueList},CType,NameList) ->
    lists:map(fun(X)-> normalize_choice(S,X,CType,NameList) end, ValueList);
normalize_choice(S,Val=#'Externalvaluereference'{},CType,NameList) ->
    {_,#valuedef{value=V}}=get_referenced_type(S,Val),
    normalize_choice(S,{'CHOICE',V},CType,NameList);
%    get_normalized_value(S,Val,CType,fun normalize_choice/4,[NameList]);
normalize_choice(S,{Name,ChoiceVal},CType,NameList)
  when atom(Name) ->
    normalize_choice(S,ChoiceVal,CType,NameList).

normalize_sequence(S,{Name,Value},Components,NameList)
  when atom(Name),list(Value) ->
    normalize_sequence(S,Value,Components,NameList);
normalize_sequence(S,Value,Components,NameList) ->
    normalized_record('SEQUENCE',S,Value,Components,NameList).

normalize_set(S,{Name,Value},Components,NameList)
  when atom(Name),list(Value) ->
    normalized_record('SET',S,Value,Components,NameList);
normalize_set(S,Value,Components,NameList) ->
    normalized_record('SET',S,Value,Components,NameList).

normalized_record(SorS,S,Value,Components,NameList) ->
    NewName = list_to_atom(asn1ct_gen:list2name(NameList)),
    NoComps = length(Components),
    case normalize_seq_or_set(SorS,S,Value,Components,NameList,[]) of
	ListOfVals when length(ListOfVals) == NoComps ->
	    list_to_tuple([NewName|ListOfVals]);
	_ ->
	    error({type,{illegal,default,value,Value},S})
    end.

normalize_seq_or_set(SorS,S,[{Cname,V}|Vs],
		     [#'ComponentType'{name=Cname,typespec=TS}|Cs],
		     NameList,Acc) ->
    NewNameList =
	case TS#type.def of
	    #'Externaltypereference'{type=TName} ->
		[TName];
	    _ -> [Cname|NameList]
	end,
    NVal = normalize_value(S,TS,{'DEFAULT',V},NewNameList),
    normalize_seq_or_set(SorS,S,Vs,Cs,NameList,[NVal|Acc]);
normalize_seq_or_set(SorS,S,Values=[{_Cname1,_V}|_Vs],
		     [#'ComponentType'{prop='OPTIONAL'}|Cs],
		     NameList,Acc) ->
    normalize_seq_or_set(SorS,S,Values,Cs,NameList,[asn1_NOVALUE|Acc]);
normalize_seq_or_set(SorS,S,Values=[{_Cname1,_V}|_Vs],
		    [#'ComponentType'{name=Cname2,typespec=TS,
				      prop={'DEFAULT',Value}}|Cs],
		    NameList,Acc) ->
    NewNameList =
	case TS#type.def of
	    #'Externaltypereference'{type=TName} ->
		[TName];
	    _ -> [Cname2|NameList]
	end,
    NVal =  normalize_value(S,TS,{'DEFAULT',Value},NewNameList),
    normalize_seq_or_set(SorS,S,Values,Cs,NameList,[NVal|Acc]);
normalize_seq_or_set(_SorS,_S,[],[],_,Acc) ->
    lists:reverse(Acc);
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
normalize_seq_or_set(_SorS,S,V,_,_,_) ->
    error({type,{illegal,default,value,V},S}).

normalize_seqof(S,Value,Type,NameList) ->
    normalize_s_of('SEQUENCE OF',S,Value,Type,NameList).

normalize_setof(S,Value,Type,NameList) ->
    normalize_s_of('SET OF',S,Value,Type,NameList).

normalize_s_of(SorS,S,Value,Type,NameList) when list(Value) ->
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
	List when list(List) ->
	    List;
	_ ->
	    io:format("WARNING: ~p could not handle value ~p~n",
		      [SorS,Value]),
	    Value
    end;
normalize_s_of(SorS,S,Value,Type,NameList)
  when record(Value,'Externalvaluereference') ->
    get_normalized_value(S,Value,Type,fun normalize_s_of/5,
			 [SorS,NameList]).
%     case catch get_referenced_type(S,Value) of
% 	{_,#valuedef{value=V}} ->
% 	    normalize_s_of(SorS,S,V,Type);
% 	{error,Reason} ->
% 	    io:format("WARNING: ~p could not handle value ~p~n",
% 		      [SorS,Value]),
% 	    Value;
% 	{_,NewVal} ->
% 	    normalize_s_of(SorS,S,NewVal,Type);
% 	_ ->
% 	    io:format("WARNING: ~p could not handle value ~p~n",
% 		      [SorS,Value]),
% 	    Value
%     end.


%% normalize_restrictedstring handles all format of restricted strings.
%% tuple case
normalize_restrictedstring(_S,[Int1,Int2],_) when integer(Int1),integer(Int2) ->
    {Int1,Int2};
%% quadruple case
normalize_restrictedstring(_S,[Int1,Int2,Int3,Int4],_) when integer(Int1),
							   integer(Int2),
							   integer(Int3),
							   integer(Int4) ->
    {Int1,Int2,Int3,Int4};
%% character string list case
normalize_restrictedstring(S,[H|T],CType) when list(H);tuple(H) ->
    [normalize_restrictedstring(S,H,CType)|normalize_restrictedstring(S,T,CType)];
%% character sting case
normalize_restrictedstring(_S,CString,_) when list(CString) ->
    Fun =
	fun(X) ->
		if
		    $X =< 255, $X >= 0 ->
			ok;
		    true ->
			io:format("WARNING: illegal character in string"
				  " ~p~n",[X])
		end
	end,
    lists:foreach(Fun,CString),
    CString;
%% definedvalue case or argument in a parameterized type
normalize_restrictedstring(S,ERef,CType) when record(ERef,'Externalvaluereference') ->
    get_normalized_value(S,ERef,CType,
			 fun normalize_restrictedstring/3,[]);
%%
normalize_restrictedstring(S,{Name,Val},CType) when atom(Name) ->
    normalize_restrictedstring(S,Val,CType).


get_normalized_value(S,Val,Type,Func,AddArg) ->
    case catch get_referenced_type(S,Val) of
	{_,#valuedef{type=_T,value=V}} ->
	    %% should check that Type and T equals
	    call_Func(S,V,Type,Func,AddArg);
	{error,_} ->
	    io:format("WARNING: default value not "
		      "comparable ~p~n",[Val]),
	    Val;
	{_,NewVal} ->
	    call_Func(S,NewVal,Type,Func,AddArg);
	_ ->
	    io:format("WARNING: default value not "
		      "comparable ~p~n",[Val]),
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
	    Name when atom(Name) ->
		{Name,Type,NameList};
	    Ref when record(Ref,'Externaltypereference') ->
		{_,#typedef{name=Name,typespec=RefedType}} =
		    get_referenced_type(S,Ref),
		get_canonic_type(S,RefedType,[Name]);
	    {Name,T} when atom(Name) ->
		{Name,T,NameList};
	    Seq when record(Seq,'SEQUENCE') ->
		{'SEQUENCE',Seq#'SEQUENCE'.components,NameList};
	    Set when record(Set,'SET') ->
		{'SET',Set#'SET'.components,NameList}
	end,
    {asn1ct_gen:unify_if_string(InnerType),NewType,NewNameList}.



check_ptype(_S,Type,Ts) when record(Ts,type) ->
    %Tag = Ts#type.tag,
    %Constr = Ts#type.constraint,
    Def = Ts#type.def,
    NewDef=
	case Def of
	    Seq when record(Seq,'SEQUENCE') ->
		#newt{type=Seq#'SEQUENCE'{pname=Type#ptypedef.name}};
	    Set when record(Set,'SET') ->
		#newt{type=Set#'SET'{pname=Type#ptypedef.name}};
	    _Other ->
		#newt{}
	end,
    Ts2 = case NewDef of
	      #newt{type=unchanged} ->
		  Ts;
	      #newt{type=TDef}->
		  Ts#type{def=TDef}
	  end,
    Ts2.


% check_type(S,Type,ObjSpec={{objectclassname,_},_}) ->
%     check_class(S,ObjSpec);
check_type(_S,Type,Ts) when record(Type,typedef),
			   (Type#typedef.checked==true) ->
    Ts;
check_type(_S,Type,Ts) when record(Type,typedef),
			   (Type#typedef.checked==idle) -> % the check is going on
    Ts;
check_type(S=#state{recordtopname=TopName},Type,Ts) when record(Ts,type) ->
    {Def,Tag,Constr} =
	case match_parameters(Ts#type.def,S#state.parameters) of
	    #type{constraint=_Ctmp,def=Dtmp} ->
		{Dtmp,Ts#type.tag,Ts#type.constraint};
	    Dtmp ->
		{Dtmp,Ts#type.tag,Ts#type.constraint}
	end,
    TempNewDef = #newt{type=Def,tag=Tag,constraint=Constr},
    TestFun =
	fun(Tref) ->
		{_,MaybeChoice} = get_referenced_type(S,Tref),
		case catch((MaybeChoice#typedef.typespec)#type.def) of
		    {'CHOICE',_} ->
			maybe_illicit_implicit_tag(choice,Tag);
		    'ANY' ->
			maybe_illicit_implicit_tag(open_type,Tag);
		    'ANY DEFINED BY' ->
			maybe_illicit_implicit_tag(open_type,Tag);
		    'ASN1_OPEN_TYPE' ->
			maybe_illicit_implicit_tag(open_type,Tag);
		    _ ->
			Tag
		end
	end,
    NewDef=
	case Def of
	    Ext when record(Ext,'Externaltypereference') ->
		{_,RefTypeDef} = get_referenced_type(S,Ext),
% 		case RefTypeDef of
% 		    Class when record(Class,classdef) ->
% 			throw({asn1_class,Class});
% 		    _ -> ok
% 		end,
		case is_class(S,RefTypeDef) of
		    true -> throw({asn1_class,RefTypeDef});
		    _ -> ok
		end,
		Ct = TestFun(Ext),
		RefType =
%case  S#state.erule of
%			      ber_bin_v2 ->
		    case RefTypeDef#typedef.checked of
			true ->
			    RefTypeDef#typedef.typespec;
			_ ->
			    NewRefTypeDef1 = RefTypeDef#typedef{checked=idle},
			    asn1_db:dbput(S#state.mname,
					  NewRefTypeDef1#typedef.name,NewRefTypeDef1),
			    RefType1 =
				check_type(S,RefTypeDef,RefTypeDef#typedef.typespec),
			    NewRefTypeDef2 =
				RefTypeDef#typedef{checked=true,typespec = RefType1},
			    asn1_db:dbput(S#state.mname,
					  NewRefTypeDef2#typedef.name,NewRefTypeDef2),
			    %% update the type and mark as checked
			    RefType1
		    end,
%			      _ -> RefTypeDef#typedef.typespec
%			  end,

		case asn1ct_gen:prim_bif(asn1ct_gen:get_inner(RefType#type.def)) of
		    true ->
			%% Here we expand to a built in type and inline it
			TempNewDef#newt{
			  type=
			  RefType#type.def,
			  tag=
			  merge_tags(Ct,RefType#type.tag),
			  constraint=
			  merge_constraints(check_constraints(S,Constr),
					    RefType#type.constraint)};
		    _ ->
			%% Here we only expand the tags and keep the ext ref

			TempNewDef#newt{
			  type=
			  check_externaltypereference(S,Ext),
			  tag =
			  case S#state.erule of
			      ber_bin_v2 ->
				  merge_tags(Ct,RefType#type.tag);
			      _ ->
				  Ct
			  end
			 }
		end;
	    'ANY' ->
		Ct=maybe_illicit_implicit_tag(open_type,Tag),
		TempNewDef#newt{type='ASN1_OPEN_TYPE',tag=Ct};
	    {'ANY_DEFINED_BY',_} ->
		Ct=maybe_illicit_implicit_tag(open_type,Tag),
		TempNewDef#newt{type='ASN1_OPEN_TYPE',tag=Ct};
	    'INTEGER' ->
		check_integer(S,[],Constr),
		TempNewDef#newt{tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_INTEGER))};

	    {'INTEGER',NamedNumberList} ->
		TempNewDef#newt{type={'INTEGER',check_integer(S,NamedNumberList,Constr)},
				tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_INTEGER))};
	    {'BIT STRING',NamedNumberList} ->
		NewL = check_bitstring(S,NamedNumberList,Constr),
%%		erlang:display({asn1ct_check,NamedNumberList,NewL}),
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
%%		AssociatedType = asn1_db:dbget(S#state.mname,'EXTERNAL'),
%%		#newt{type=check_type(S,Type,AssociatedType)};
		put(external,unchecked),
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
				 check_enumerated(S,NamedNumberList,Constr)},
				tag=
				merge_tags(Tag,?TAG_PRIMITIVE(?N_ENUMERATED))};
	    'EMBEDDED PDV' ->
%		AssociatedType = asn1_db:dbget(S#state.mname,'EMBEDDED PDV'),
%		CheckedType = check_type(S,Type,
%					 AssociatedType#typedef.typespec),
		put(embedded_pdv,unchecked),
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
	    'TeletexString' ->
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
	    'CHARACTER STRING' ->
%		AssociatedType = asn1_db:dbget(S#state.mname,
%					       'CHARACTER STRING'),
%		CheckedType = check_type(S,Type,
%					 AssociatedType#typedef.typespec),
		put(character_string,unchecked),
		TempNewDef#newt{type=
				#'Externaltypereference'{module=S#state.mname,
							 type='CHARACTER STRING'},
				tag=
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_CHARACTER_STRING))};
	    Seq when record(Seq,'SEQUENCE') ->
		RecordName =
		    case TopName of
			[] ->
			    [Type#typedef.name];
			_ ->
			    TopName
		    end,
		{TableCInf,Components} =
		    check_sequence(S#state{recordtopname=
					   RecordName},
					   Type,Seq#'SEQUENCE'.components),
		TempNewDef#newt{type=Seq#'SEQUENCE'{tablecinf=TableCInf,
					  components=Components},
				tag=
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_SEQUENCE))};
	    {'SEQUENCE OF',Components} ->
		TempNewDef#newt{type={'SEQUENCE OF',check_sequenceof(S,Type,Components)},
				tag=
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_SEQUENCE))};
	    {'CHOICE',Components} ->
		Ct = maybe_illicit_implicit_tag(choice,Tag),
		TempNewDef#newt{type={'CHOICE',check_choice(S,Type,Components)},tag=Ct};
	    Set when record(Set,'SET') ->
		RecordName=
		    case TopName of
			[] ->
			    [Type#typedef.name];
			_ ->
			    TopName
		    end,
		{Sorted,TableCInf,Components} =
		    check_set(S#state{recordtopname=RecordName},
			      Type,Set#'SET'.components),
		TempNewDef#newt{type=Set#'SET'{sorted=Sorted,
				     tablecinf=TableCInf,
				     components=Components},
				tag=
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_SET))};
	    {'SET OF',Components} ->
		TempNewDef#newt{type={'SET OF',check_setof(S,Type,Components)},
				tag=
				merge_tags(Tag,?TAG_CONSTRUCTED(?N_SET))};
	    %% This is a temporary hack until the full Information Obj Spec
	    %% in X.681 is supported
	    {{typereference,_,'TYPE-IDENTIFIER'},[{typefieldreference,_,'Type'}]} ->
		Ct=maybe_illicit_implicit_tag(open_type,Tag),
		TempNewDef#newt{type='ASN1_OPEN_TYPE',tag=Ct};

	    {#'Externaltypereference'{type='TYPE-IDENTIFIER'},
	     [{typefieldreference,_,'Type'}]} ->
		Ct=maybe_illicit_implicit_tag(open_type,Tag),
		TempNewDef#newt{type='ASN1_OPEN_TYPE',tag=Ct};

	    {pt,Ptype,ParaList} ->
		%% Ptype might be a parameterized - type, object set or
		%% value set. If it isn't a parameterized type notify the
		%% calling function.
		{_,Ptypedef} = get_referenced_type(S,Ptype),
		notify_if_not_ptype(S,Ptypedef),
		NewParaList = [match_parameters(TmpParam,S#state.parameters)||
				  TmpParam <- ParaList],
		Instance = instantiate_ptype(S,Ptypedef,NewParaList),
		TempNewDef#newt{type=Instance#type.def,
				tag=merge_tags(Tag,Instance#type.tag),
				constraint=Instance#type.constraint,
				inlined=yes};

%	    {ClRef,FieldRefList} when record(ClRef,'Externaltypereference') ->
	    OCFT=#'ObjectClassFieldType'{class=ClRef} ->
		%% this case occures in a SEQUENCE when
		%% the type of the component is a ObjectClassFieldType
		ClassSpec = check_class(S,ClRef),
		NewTypeDef = maybe_open_type(S,ClassSpec,OCFT,Constr),
		InnerTag = get_innertag(S,NewTypeDef),
		MergedTag = merge_tags(Tag,InnerTag),
		Ct =
		    case is_open_type(NewTypeDef) of
			true ->
			    maybe_illicit_implicit_tag(open_type,MergedTag);
			_ ->
			    MergedTag
		    end,
		TempNewDef#newt{type=NewTypeDef,tag=Ct};
	    {valueset,Vtype} ->
		TempNewDef#newt{type={valueset,check_type(S,Type,Vtype)}};
	    Other ->
		exit({'cant check' ,Other})
	end,
    Ts2 = case NewDef of
	      #newt{type=unchanged} ->
		  Ts#type{def=Def};
	      #newt{type=TDef}->
		  Ts#type{def=TDef}
	  end,
    NewTag = case NewDef of
		 #newt{tag=unchanged} ->
		     Tag;
		 #newt{tag=TT} ->
		     TT
	     end,
    T3 = Ts2#type{tag = lists:map(fun(TempTag = #tag{type={default,TTx}}) ->
					  TempTag#tag{type=TTx};
				     (Else) -> Else end, NewTag)},
    T4 = case NewDef of
	     #newt{constraint=unchanged} ->
		 T3#type{constraint=Constr};
	     #newt{constraint=NewConstr} ->
		 T3#type{constraint=NewConstr}
	 end,
    T5 = T4#type{inlined=NewDef#newt.inlined},
    T5#type{constraint=check_constraints(S,T5#type.constraint)}.


get_innertag(_S,#'ObjectClassFieldType'{type=Type}) ->
    case Type of
	#type{tag=Tag} -> Tag;
	{fixedtypevaluefield,_,#type{tag=Tag}} -> Tag;
	{TypeFieldName,_} when atom(TypeFieldName) -> [];
	_ -> []
    end;
get_innertag(_S,_) ->
    [].

is_class(_S,#classdef{}) ->
    true;
is_class(S,#typedef{typespec=#type{def=Eref}})
  when record(Eref,'Externaltypereference')->
    {_,NextDef} = get_referenced_type(S,Eref),
    is_class(S,NextDef);
is_class(_,_) ->
    false.

get_class_def(_S,CD=#classdef{}) ->
    CD;
get_class_def(S,#typedef{typespec=#type{def=Eref}})
  when record(Eref,'Externaltypereference') ->
    {_,NextDef} = get_referenced_type(S,Eref),
    get_class_def(S,NextDef).

maybe_illicit_implicit_tag(Kind,Tag) ->
    case Tag of
	[#tag{type='IMPLICIT'}|_T] ->
	    throw({error,{asn1,{implicit_tag_before,Kind}}});
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

%% maybe_open_type/2 -> {ClassSpec,FieldRefList} | 'ASN1_OPEN_TYPE'
%% if the FieldRefList points out a typefield and the class don't have
%% any UNIQUE field, so that a component relation constraint cannot specify
%% the type of a typefield, return 'ASN1_OPEN_TYPE', otherwise return
%% {ClassSpec,FieldRefList}.
maybe_open_type(S,ClassSpec=#objectclass{fields=Fs},
		OCFT=#'ObjectClassFieldType'{fieldname=FieldRefList},
		Constr) ->
    Type = get_ObjectClassFieldType(S,Fs,FieldRefList),
    FieldNames=get_referenced_fieldname(FieldRefList),
    case lists:last(FieldRefList) of
	{valuefieldreference,_} ->
	    OCFT#'ObjectClassFieldType'{class=ClassSpec,
					fieldname=FieldNames,
					type=Type};
	{typefieldreference,_} ->
	    case {catch get_unique_fieldname(#classdef{typespec=ClassSpec}),
		  asn1ct_gen:get_constraint(Constr,componentrelation)}of
		{Tuple,_} when tuple(Tuple) ->
		    OCFT#'ObjectClassFieldType'{class=ClassSpec,
						fieldname=FieldNames,
						type='ASN1_OPEN_TYPE'};
		{_,no} ->
		    OCFT#'ObjectClassFieldType'{class=ClassSpec,
						fieldname=FieldNames,
						type='ASN1_OPEN_TYPE'};
		_ ->
		    OCFT#'ObjectClassFieldType'{class=ClassSpec,
						fieldname=FieldNames,
						type=Type}
	    end
    end.

is_open_type(#'ObjectClassFieldType'{type='ASN1_OPEN_TYPE'}) ->
    true;
is_open_type(#'ObjectClassFieldType'{}) ->
    false.


notify_if_not_ptype(S,#pvaluesetdef{type=Type}) ->
    case Type#type.def of
	Ref when record(Ref,'Externaltypereference') ->
	    case get_referenced_type(S,Ref) of
		{_,#classdef{}} ->
		    throw(pobjectsetdef);
		{_,#typedef{}} ->
		    throw(pvalueset)
	    end;
	T when record(T,type) -> % this must be a value set
	    throw(pvalueset)
    end;
notify_if_not_ptype(_S,#ptypedef{}) ->
    ok.

% fix me
instantiate_ptype(S,Ptypedef,ParaList) ->
    #ptypedef{args=Args,typespec=Type} = Ptypedef,
%    Args = get_pt_args(Ptypedef),
%    Type = get_pt_spec(Ptypedef),
    MatchedArgs = match_args(Args, ParaList, []),
    NewS = S#state{type=Type,parameters=MatchedArgs,abscomppath=[]},
    %The abscomppath must be empty since a table constraint in a
    %parameterized type only can refer to components within the type
    check_type(NewS, Ptypedef, Type).

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



match_args([FormArg|Ft], [ActArg|At], Acc) ->
    match_args(Ft, At, [{FormArg,ActArg}|Acc]);
match_args([], [], Acc) ->
    lists:reverse(Acc);
match_args(_, _, _) ->
    throw({error,{asn1,{wrong_number_of_arguments}}}).

check_constraints(S,C) when list(C) ->
    check_constraints(S, C, []);
check_constraints(S,C) when record(C,constraint) ->
    check_constraints(S, C#constraint.c, []).


resolv_tuple_or_list(S,List) when list(List) ->
    lists:map(fun(X)->resolv_value(S,X) end, List);
resolv_tuple_or_list(S,{Lb,Ub}) ->
    {resolv_value(S,Lb),resolv_value(S,Ub)}.

%%%-----------------------------------------
%% If the constraint value is a defined value the valuename
%% is replaced by the actual value
%%
resolv_value(S,Val) ->
    case match_parameters(Val, S#state.parameters) of
	Id -> % unchanged
	    resolv_value1(S,Id);
	Other ->
	    resolv_value(S,Other)
    end.

resolv_value1(S = #state{mname=M,inputmodules=InpMods},
	      V=#'Externalvaluereference'{pos=Pos,module=ExtM,value=Name}) ->
    case ExtM of
	M -> resolv_value2(S,M,Name,Pos);
	_ ->
	    case lists:member(ExtM,InpMods) of
		true ->
		    resolv_value2(S,M,Name,Pos);
		false ->
		    V
	    end
    end;
resolv_value1(S,{gt,V}) ->
    case V of
	Int when integer(Int) ->
	    V + 1;
	#valuedef{value=Int} ->
	    1 + resolv_value(S,Int);
	Other ->
	    throw({error,{asn1,{undefined_type_or_value,Other}}})
    end;
resolv_value1(S,{lt,V}) ->
    case V of
	Int when integer(Int) ->
	    V - 1;
	#valuedef{value=Int} ->
	    resolv_value(S,Int) - 1;
	Other ->
	    throw({error,{asn1,{undefined_type_or_value,Other}}})
    end;
resolv_value1(S,{'ValueFromObject',{object,Object},[{valuefieldreference,
						     FieldName}]}) ->
    %% FieldName can hold either a fixed-type value or a variable-type value
    %% Object is a DefinedObject, i.e. a #'Externaltypereference'
    {_,ObjTDef} = get_referenced_type(S,Object),
    TS = check_object(S,ObjTDef,ObjTDef#typedef.typespec),
    {_,_,Components} = TS#'Object'.def,
    case lists:keysearch(FieldName,1,Components) of
	{value,{_,#valuedef{value=Val}}} ->
	    Val;
	_ ->
	    error({value,"illegal value in constraint",S})
    end;
% resolv_value1(S,{'ValueFromObject',{po,Object,Params},FieldName}) ->
%     %% FieldName can hold either a fixed-type value or a variable-type value
%     %% Object is a ParameterizedObject
resolv_value1(_,V) ->
    V.

resolv_value2(S,ModuleName,Name,Pos) ->
    case asn1_db:dbget(ModuleName,Name) of
	undefined ->
	    case imported(S,Name) of
		{ok,Imodule} ->
		    {_,V2} = get_referenced(S,Imodule,Name,Pos),
		    V2#valuedef.value;
		_  ->
		    throw({error,{asn1,{undefined_type_or_value,Name}}})
	    end;
	Val ->
	    Val#valuedef.value
    end.

check_constraints(S,[{'ContainedSubtype',Type} | Rest], Acc) ->
    {_,CTDef} = get_referenced_type(S,Type#type.def),
    CType = check_type(S,S#state.tname,CTDef#typedef.typespec),
    check_constraints(S,Rest,CType#type.constraint ++ Acc);
check_constraints(S,[C | Rest], Acc) ->
    check_constraints(S,Rest,[check_constraint(S,C) | Acc]);
check_constraints(S,[],Acc) ->
%    io:format("Acc: ~p~n",[Acc]),
    C = constraint_merge(S,lists:reverse(Acc)),
%    io:format("C: ~p~n",[C]),
    lists:flatten(C).


range_check(F={FixV,FixV}) ->
%    FixV;
    F;
range_check(VR={Lb,Ub}) when Lb < Ub ->
    VR;
range_check(Err={_,_}) ->
    throw({error,{asn1,{illegal_size_constraint,Err}}});
range_check(Value) ->
    Value.

check_constraint(S,Ext) when record(Ext,'Externaltypereference') ->
    check_externaltypereference(S,Ext);


check_constraint(S,{'SizeConstraint',{Lb,Ub}})
  when list(Lb);tuple(Lb),size(Lb)==2 ->
    case Lb of
	#'Externalvaluereference'{} ->
	    check_constraint(S,{'SizeConstraint',{resolv_value(S,Lb),Ub}});
	_ ->
	    NewLb = range_check(resolv_tuple_or_list(S,Lb)),
	    NewUb = range_check(resolv_tuple_or_list(S,Ub)),
	    {'SizeConstraint',{NewLb,NewUb}}
    end;
check_constraint(S,{'SizeConstraint',{Lb,Ub}}) ->
    case {resolv_value(S,Lb),resolv_value(S,Ub)} of
	{FixV,FixV} ->
	    {'SizeConstraint',FixV};
	{Low,High} when Low < High ->
	    {'SizeConstraint',{Low,High}};
	Err ->
	    throw({error,{asn1,{illegal_size_constraint,Err}}})
    end;
check_constraint(S,{'SizeConstraint',Lb}) ->
    {'SizeConstraint',resolv_value(S,Lb)};

check_constraint(S,{'SingleValue', L}) when list(L) ->
    F = fun(A) -> resolv_value(S,A) end,
    {'SingleValue',lists:map(F,L)};

check_constraint(S,{'SingleValue', V}) when integer(V) ->
    Val = resolv_value(S,V),
%%    [{'SingleValue',Val},{'ValueRange',{Val,Val}}]; % Why adding value range?
    {'SingleValue',Val};
check_constraint(S,{'SingleValue', V}) ->
    {'SingleValue',resolv_value(S,V)};

check_constraint(S,{'ValueRange', {Lb, Ub}}) ->
    {'ValueRange',{resolv_value(S,Lb),resolv_value(S,Ub)}};

%%check_constraint(S,{'ContainedSubtype',Type}) ->
%%    #typedef{typespec=TSpec} =
%%	check_type(S,S#state.tname,get_referenced_type(S,Type#type.def)),
%%    [C] = TSpec#type.constraint,
%%    C;

check_constraint(S,{valueset,Type}) ->
    {valueset,check_type(S,S#state.tname,Type)};

check_constraint(S,{simpletable,Type}) ->
    OSName = (Type#type.def)#'Externaltypereference'.type,
    C = match_parameters(Type#type.def,S#state.parameters),
    case C of
	#'Externaltypereference'{} ->
	     Type#type{def=check_externaltypereference(S,C)},
	    {simpletable,OSName};
	_ ->
	    check_type(S,S#state.tname,Type),
	    {simpletable,OSName}
    end;

check_constraint(S,{componentrelation,{objectset,Opos,Objset},Id}) ->
    %% Objset is an 'Externaltypereference' record, since Objset is
    %% a DefinedObjectSet.
    RealObjset = match_parameters(Objset,S#state.parameters),
    Ext = check_externaltypereference(S,RealObjset),
    {componentrelation,{objectset,Opos,Ext},Id};

check_constraint(S,Type) when record(Type,type) ->
    #type{def=Def} = check_type(S,S#state.tname,Type),
    Def;

check_constraint(S,C) when list(C) ->
    lists:map(fun(X)->check_constraint(S,X) end,C);
% else keep the constraint unchanged
check_constraint(_S,Any) ->
%    io:format("Constraint = ~p~n",[Any]),
    Any.

%% constraint_merge/2
%% Compute the intersection of the outermost level of the constraint list.
%% See Dubuisson second paragraph and fotnote on page 285.
%% If constraints with extension are included in combined constraints. The
%% resulting combination will have the extension of the last constraint. Thus,
%% there will be no extension if the last constraint is without extension.
%% The rootset of all constraints are considered in the "outermoust
%% intersection". See section 13.1.2 in Dubuisson.
constraint_merge(_S,C=[H])when tuple(H) ->
    C;
constraint_merge(_S,[]) ->
    [];
constraint_merge(S,C) ->
    %% skip all extension but the last
    C1 = filter_extensions(C),
    %% perform all internal level intersections, intersections first
    %% since they have precedence over unions
    C2 = lists:map(fun(X)when list(X)->constraint_intersection(S,X);
		      (X) -> X end,
		   C1),
    %% perform all internal level unions
    C3 = lists:map(fun(X)when list(X)->constraint_union(S,X);
		      (X) -> X end,
		   C2),

    %% now get intersection of the outermost level
    %% get the least common single value constraint
    SVs = get_constraints(C3,'SingleValue'),
    CombSV = intersection_of_sv(S,SVs),
    %% get the least common value range constraint
    VRs = get_constraints(C3,'ValueRange'),
    CombVR = intersection_of_vr(S,VRs),
    %% get the least common size constraint
    SZs = get_constraints(C3,'SizeConstraint'),
    CombSZ = intersection_of_size(S,SZs),
    CminusSVs=ordsets:subtract(ordsets:from_list(C3),ordsets:from_list(SVs)),
    % CminusSVsVRs = ordsets:subtract(ordsets:from_list(CminusSVs),
% 				    ordsets:from_list(VRs)),
    RestC = ordsets:subtract(ordsets:from_list(CminusSVs),
			     ordsets:from_list(SZs)),
    %% get the least common combined constraint. That is the union of each
    %% deep costraint and merge of single value and value range constraints
    combine_constraints(S,CombSV,CombVR,CombSZ++RestC).

%% constraint_union(S,C) takes a list of constraints as input and
%% merge them to a union. Unions are performed when two
%% constraints is found with an atom union between.
%% The list may be nested. Fix that later !!!
constraint_union(_S,[]) ->
    [];
constraint_union(_S,C=[_E]) ->
    C;
constraint_union(S,C) when list(C) ->
    case lists:member(union,C) of
	true ->
	    constraint_union1(S,C,[]);
	_ ->
	    C
    end;
%     SV = get_constraints(C,'SingleValue'),
%     SV1 = constraint_union_sv(S,SV),
%     VR = get_constraints(C,'ValueRange'),
%     VR1 = constraint_union_vr(VR),
%     RestC = ordsets:filter(fun({'SingleValue',_})->false;
% 			      ({'ValueRange',_})->false;
% 			      (_) -> true end,ordsets:from_list(C)),
%     SV1++VR1++RestC;
constraint_union(_S,C) ->
    [C].

constraint_union1(S,[A={'ValueRange',_},union,B={'ValueRange',_}|Rest],Acc) ->
    AunionB = constraint_union_vr([A,B]),
    constraint_union1(S,Rest,AunionB++Acc);
constraint_union1(S,[A={'SingleValue',_},union,B={'SingleValue',_}|Rest],Acc) ->
    AunionB = constraint_union_sv(S,[A,B]),
    constraint_union1(S,Rest,AunionB++Acc);
constraint_union1(S,[A={'SingleValue',_},union,B={'ValueRange',_}|Rest],Acc) ->
    AunionB = union_sv_vr(S,A,B),
    constraint_union1(S,Rest,AunionB++Acc);
constraint_union1(S,[A={'ValueRange',_},union,B={'SingleValue',_}|Rest],Acc) ->
    AunionB = union_sv_vr(S,B,A),
    constraint_union1(S,Rest,AunionB++Acc);
constraint_union1(S,[union|Rest],Acc) -> %skip when unsupported constraints
    constraint_union1(S,Rest,Acc);
constraint_union1(S,[A|Rest],Acc) ->
    constraint_union1(S,Rest,[A|Acc]);
constraint_union1(_S,[],Acc) ->
    lists:reverse(Acc).

constraint_union_sv(_S,SV) ->
    Values=lists:map(fun({_,V})->V end,SV),
    case ordsets:from_list(Values) of
	[] -> [];
	[N] -> [{'SingleValue',N}];
	L -> [{'SingleValue',L}]
    end.

%% REMOVE????
%%constraint_union(S,VR,'ValueRange') ->
%%    constraint_union_vr(VR).

%% constraint_union_vr(VR)
%% VR = [{'ValueRange',{Lb,Ub}},...]
%% Lb = 'MIN' | integer()
%% Ub = 'MAX' | integer()
%% Returns if possible only one ValueRange tuple with a range that
%% is a union of all ranges in VR.
constraint_union_vr(VR) ->
    %% Sort VR by Lb in first hand and by Ub in second hand
    Fun=fun({_,{'MIN',_B1}},{_,{A2,_B2}}) when integer(A2)->true;
	   ({_,{A1,_B1}},{_,{'MAX',_B2}}) when integer(A1) -> true;
	   ({_,{A1,_B1}},{_,{A2,_B2}}) when integer(A1),integer(A2),A1<A2 -> true;
	   ({_,{A,B1}},{_,{A,B2}}) when B1=<B2->true;
	   (_,_)->false end,
    constraint_union_vr(lists:usort(Fun,VR),[]).

constraint_union_vr([],Acc) ->
    lists:reverse(Acc);
constraint_union_vr([C|Rest],[]) ->
    constraint_union_vr(Rest,[C]);
constraint_union_vr([{_,{Lb,Ub2}}|Rest],[{_,{Lb,_Ub1}}|Acc]) -> %Ub2 > Ub1
    constraint_union_vr(Rest,[{'ValueRange',{Lb,Ub2}}|Acc]);
constraint_union_vr([{_,{_,Ub}}|Rest],A=[{_,{_,Ub}}|_Acc]) ->
    constraint_union_vr(Rest,A);
constraint_union_vr([{_,{Lb2,Ub2}}|Rest],[{_,{Lb1,Ub1}}|Acc]) when Lb2=<Ub1,
								   Ub2>Ub1->
    constraint_union_vr(Rest,[{'ValueRange',{Lb1,Ub2}}|Acc]);
constraint_union_vr([{_,{_,Ub2}}|Rest],A=[{_,{_,Ub1}}|_Acc]) when Ub2=<Ub1->
    constraint_union_vr(Rest,A);
constraint_union_vr([VR|Rest],Acc) ->
    constraint_union_vr(Rest,[VR|Acc]).

union_sv_vr(_S,[],B) ->
    [B];
union_sv_vr(_S,A,[]) ->
    [A];
union_sv_vr(_S,C1={'SingleValue',SV},C2={'ValueRange',VR={Lb,Ub}})
  when integer(SV) ->
    case is_int_in_vr(SV,C2) of
	true -> [C2];
	_ ->
	    case VR of
		{'MIN',Ub} when SV==Ub+1 -> [{'ValueRange',{'MIN',SV}}];
		{Lb,'MAX'} when SV==Lb-1 -> [{'ValueRange',{SV,'MAX'}}];
		{Lb,Ub} when SV==Ub+1 -> [{'ValueRange',{Lb,SV}}];
		{Lb,Ub} when SV==Lb-1 -> [{'ValueRange',{SV,Ub}}];
		_ ->
		    [C1,C2]
	    end
    end;
union_sv_vr(_S,C1={'SingleValue',SV},C2={'ValueRange',{_Lb,_Ub}})
  when list(SV) ->
    case lists:filter(fun(X)->is_int_in_vr(X,C2) end,SV) of
	[] -> [C2];
	L ->
	    case expand_vr(L,C2) of
		{[],C3} -> [C3];
		{L,C2} -> [C1,C2];
		{[Val],C3} -> [{'SingleValue',Val},C3];
		{L2,C3} -> [{'SingleValue',L2},C3]
	    end
    end.

expand_vr(L,VR={_,{Lb,Ub}}) ->
    case lower_Lb(L,Lb) of
	false ->
	    case higher_Ub(L,Ub) of
		false ->
		    {L,VR};
		{L1,UbNew} ->
		    expand_vr(L1,{'ValueRange',{Lb,UbNew}})
	    end;
	{L1,LbNew} ->
	    expand_vr(L1,{'ValueRange',{LbNew,Ub}})
    end.

lower_Lb(_,'MIN') ->
    false;
lower_Lb(L,Lb) ->
    remove_val_from_list(Lb - 1,L).

higher_Ub(_,'MAX') ->
    false;
higher_Ub(L,Ub) ->
    remove_val_from_list(Ub + 1,L).

remove_val_from_list(List,Val) ->
    case lists:member(Val,List) of
	true ->
	    {lists:delete(Val,List),Val};
	false ->
	    false
    end.

%% get_constraints/2
%% Arguments are a list of constraints, which has the format {key,value},
%% and a constraint type
%% Returns a list of constraints only of the requested type or the atom
%% 'no' if no such constraints were found
get_constraints(L=[{CType,_}],CType) ->
    L;
get_constraints(C,CType) ->
   keysearch_allwithkey(CType,1,C).

%% keysearch_allwithkey(Key,Ix,L)
%% Types:
%% Key = atom()
%% Ix = integer()
%% L  = [TwoTuple]
%% TwoTuple = [{atom(),term()}|...]
%% Returns a List that contains all
%% elements from L that has a key Key as element Ix
keysearch_allwithkey(Key,Ix,L) ->
    lists:filter(fun(X) when tuple(X) ->
			 case element(Ix,X) of
			     Key -> true;
			     _ -> false
			 end;
		    (_) -> false
		 end, L).


%% filter_extensions(C)
%% takes a list of constraints as input and
%% returns a list with the intersection of all extension roots
%% and only the extension of the last constraint kept if any
%% extension in the last constraint
filter_extensions([]) ->
    [];
filter_extensions(C=[_H]) ->
    C;
filter_extensions(C) when list(C) ->
    filter_extensions(C,[]).

filter_extensions([C],Acc) ->
    lists:reverse([C|Acc]);
filter_extensions([{C,_E},H2|T],Acc) when tuple(C) ->
    filter_extensions([H2|T],[C|Acc]);
filter_extensions([{'SizeConstraint',{A,_B}},H2|T],Acc)
  when list(A);tuple(A) ->
    filter_extensions([H2|T],[{'SizeConstraint',A}|Acc]);
filter_extensions([H1,H2|T],Acc) ->
    filter_extensions([H2|T],[H1|Acc]).

%% constraint_intersection(S,C) takes a list of constraints as input and
%% performs intersections. Intersecions are performed when an
%% atom intersection is found between two constraints.
%% The list may be nested. Fix that later !!!
constraint_intersection(_S,[]) ->
    [];
constraint_intersection(_S,C=[_E]) ->
    C;
constraint_intersection(S,C) when list(C) ->
%    io:format("constraint_intersection: ~p~n",[C]),
    case lists:member(intersection,C) of
	true ->
	    constraint_intersection1(S,C,[]);
	_ ->
	    C
    end;
constraint_intersection(_S,C) ->
    [C].

constraint_intersection1(S,[A,intersection,B|Rest],Acc) ->
    AisecB = c_intersect(S,A,B),
    constraint_intersection1(S,Rest,AisecB++Acc);
constraint_intersection1(S,[A|Rest],Acc) ->
    constraint_intersection1(S,Rest,[A|Acc]);
constraint_intersection1(_,[],Acc) ->
    lists:reverse(Acc).

c_intersect(S,C1={'SingleValue',_},C2={'SingleValue',_}) ->
    intersection_of_sv(S,[C1,C2]);
c_intersect(S,C1={'ValueRange',_},C2={'ValueRange',_}) ->
    intersection_of_vr(S,[C1,C2]);
c_intersect(S,C1={'ValueRange',_},C2={'SingleValue',_}) ->
    intersection_sv_vr(S,[C2],[C1]);
c_intersect(S,C1={'SingleValue',_},C2={'ValueRange',_}) ->
    intersection_sv_vr(S,[C1],[C2]);
c_intersect(_S,C1,C2) ->
    [C1,C2].

%% combine_constraints(S,SV,VR,CComb)
%% Types:
%% S = record(state,S)
%% SV = [] | [SVC]
%% VR = [] | [VRC]
%% CComb = [] | [Lists]
%% SVC = {'SingleValue',integer()} | {'SingleValue',[integer(),...]}
%% VRC = {'ValueRange',{Lb,Ub}}
%% Lists = List of lists containing any constraint combination
%% Lb = 'MIN' | integer()
%% Ub = 'MAX' | integer()
%% Returns a combination of the least common constraint among SV,VR and all
%% elements in CComb
combine_constraints(_S,[],VR,CComb) ->
    VR ++ CComb;
%    combine_combined_cnstr(S,VR,CComb);
combine_constraints(_S,SV,[],CComb) ->
    SV ++ CComb;
%    combine_combined_cnstr(S,SV,CComb);
combine_constraints(S,SV,VR,CComb) ->
    C=intersection_sv_vr(S,SV,VR),
    C ++ CComb.
%    combine_combined_cnstr(S,C,CComb).

intersection_sv_vr(_,[],_VR) ->
    [];
intersection_sv_vr(_,_SV,[]) ->
    [];
intersection_sv_vr(_S,[C1={'SingleValue',SV}],[C2={'ValueRange',{_Lb,_Ub}}])
  when integer(SV) ->
    case is_int_in_vr(SV,C2) of
	true -> [C1];
	_ -> %%error({type,{"asn1 illegal constraint",C1,C2},S})
	    throw({error,{"asn1 illegal constraint",C1,C2}})
    end;
intersection_sv_vr(_S,[C1={'SingleValue',SV}],[C2])
  when list(SV) ->
    case lists:filter(fun(X)->is_int_in_vr(X,C2) end,SV) of
	[] ->
	    %%error({type,{"asn1 illegal constraint",C1,C2},S});
	    throw({error,{"asn1 illegal constraint",C1,C2}});
	[V] -> [{'SingleValue',V}];
	L -> [{'SingleValue',L}]
    end.



intersection_of_size(_,[]) ->
    [];
intersection_of_size(_,C=[_SZ]) ->
    C;
intersection_of_size(S,[SZ,SZ|Rest]) ->
    intersection_of_size(S,[SZ|Rest]);
intersection_of_size(S,C=[C1={_,Int},{_,Range}|Rest])
  when integer(Int),tuple(Range) ->
    case Range of
	{Lb,Ub} when Int >= Lb,
		     Int =< Ub ->
	    intersection_of_size(S,[C1|Rest]);
	_ ->
	    throw({error,{asn1,{illegal_size_constraint,C}}})
    end;
intersection_of_size(S,[C1={_,Range},C2={_,Int}|Rest])
  when integer(Int),tuple(Range) ->
    intersection_of_size(S,[C2,C1|Rest]);
intersection_of_size(S,[{_,{Lb1,Ub1}},{_,{Lb2,Ub2}}|Rest]) ->
    Lb=greatest_LB(ordsets:from_list([Lb1,Lb2])),
    Ub=smallest_UB(ordsets:from_list([Ub1,Ub2])),
    intersection_of_size(S,[{'SizeConstraint',{Lb,Ub}}|Rest]);
intersection_of_size(_,SZ) ->
    throw({error,{asn1,{illegal_size_constraint,SZ}}}).

intersection_of_vr(_,[]) ->
    [];
intersection_of_vr(_,VR=[_C]) ->
    VR;
intersection_of_vr(S,[{_,{Lb1,Ub1}},{_,{Lb2,Ub2}}|Rest]) ->
    Lb=greatest_LB(ordsets:from_list([Lb1,Lb2])),
    Ub=smallest_UB(ordsets:from_list([Ub1,Ub2])),
    intersection_of_vr(S,[{'ValueRange',{Lb,Ub}}|Rest]);
intersection_of_vr(_S,VR) ->
    %%error({type,{asn1,{illegal_value_range_constraint,VR}},S});
    throw({error,{asn1,{illegal_value_range_constraint,VR}}}).

intersection_of_sv(_,[]) ->
    [];
intersection_of_sv(_,SV=[_C]) ->
    SV;
intersection_of_sv(S,[SV,SV|Rest]) ->
    intersection_of_sv(S,[SV|Rest]);
intersection_of_sv(S,[{_,Int},{_,SV}|Rest]) when integer(Int),
						 list(SV) ->
    SV2=intersection_of_sv1(S,Int,SV),
    intersection_of_sv(S,[SV2|Rest]);
intersection_of_sv(S,[{_,SV},{_,Int}|Rest]) when integer(Int),
						 list(SV) ->
    SV2=intersection_of_sv1(S,Int,SV),
    intersection_of_sv(S,[SV2|Rest]);
intersection_of_sv(S,[{_,SV1},{_,SV2}|Rest]) when list(SV1),
						  list(SV2) ->
    SV3=common_set(SV1,SV2),
    intersection_of_sv(S,[SV3|Rest]);
intersection_of_sv(_S,SV) ->
    %%error({type,{asn1,{illegal_single_value_constraint,SV}},S}).
    throw({error,{asn1,{illegal_single_value_constraint,SV}}}).

intersection_of_sv1(_S,Int,SV) when integer(Int),list(SV) ->
    case lists:member(Int,SV) of
	true -> {'SingleValue',Int};
	_ ->
	    %%error({type,{asn1,{illegal_single_value_constraint,Int,SV}},S})
	    throw({error,{asn1,{illegal_single_value_constraint,Int,SV}}})
    end;
intersection_of_sv1(_S,SV1,SV2) ->
    %%error({type,{asn1,{illegal_single_value_constraint,SV1,SV2}},S}).
    throw({error,{asn1,{illegal_single_value_constraint,SV1,SV2}}}).

greatest_LB([H]) ->
    H;
greatest_LB(L) ->
    greatest_LB1(lists:reverse(L)).
greatest_LB1(['MIN',H2|_T])->
    H2;
greatest_LB1([H|_T]) ->
    H.
smallest_UB(L) ->
    hd(L).

common_set(SV1,SV2) ->
    lists:filter(fun(X)->lists:member(X,SV1) end,SV2).

is_int_in_vr(Int,{_,{'MIN','MAX'}}) when integer(Int) ->
    true;
is_int_in_vr(Int,{_,{'MIN',Ub}}) when integer(Int),Int =< Ub ->
    true;
is_int_in_vr(Int,{_,{Lb,'MAX'}}) when integer(Int),Int >= Lb ->
    true;
is_int_in_vr(Int,{_,{Lb,Ub}}) when integer(Int),Int >= Lb,Int =< Ub ->
    true;
is_int_in_vr(_,_) ->
    false.



check_imported(_S,Imodule,Name) ->
    case asn1_db:dbget(Imodule,'MODULE') of
	undefined ->
	    io:format("~s.asn1db not found~n",[Imodule]),
	    io:format("Type ~s imported from non existing module ~s~n",[Name,Imodule]);
	Im when record(Im,module) ->
	    case is_exported(Im,Name) of
		false ->
		    io:format("Imported type ~s not exported from module ~s~n",[Name,Imodule]);
		_ ->
		    ok
	    end
    end,
    ok.

is_exported(Module,Name) when record(Module,module) ->
    {exports,Exports} = Module#module.exports,
    case Exports of
	all ->
	    true;
	[] ->
	    false;
	L when list(L) ->
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
		    Etref
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
		_ ->
		    %may be a renamed type in multi file compiling!
		    {_,T}=renamed_reference(S,Name,Emod),
		    NewName = asn1ct:get_name_of_def(T),
		    NewPos = asn1ct:get_pos_of_def(T),
		    #'Externaltypereference'{pos=NewPos,
					     module=ModName,
					     type=NewName}
	    end;
	_ ->
	    %% cannot do check_type here due to recursive definitions, like
	    %% S ::= SEQUENCE {a INTEGER, b S}. This implies that references
	    %% that appear before the definition will be an
	    %% Externaltypereference in the abstract syntax tree
	    #'Externaltypereference'{pos=Pos,module=ModName,type=Name}
    end.


name2Extref(_Mod,Name) when record(Name,'Externaltypereference') ->
    Name;
name2Extref(Mod,Name) ->
    #'Externaltypereference'{module=Mod,type=Name}.

get_referenced_type(S,Ext) when record(Ext,'Externaltypereference') ->
    case match_parameters(Ext, S#state.parameters) of
	Ext ->
	    #'Externaltypereference'{pos=Pos,module=Emod,type=Etype} = Ext,
	    case S#state.mname of
		Emod -> % a local reference in this module
		    get_referenced1(S,Emod,Etype,Pos);
		_ ->% always when multi file compiling
		    case lists:member(Emod,S#state.inputmodules) of
			true ->
			    get_referenced1(S,Emod,Etype,Pos);
			false ->
			    get_referenced(S,Emod,Etype,Pos)
		    end
	    end;
	Other ->
	    {undefined,Other}
    end;
get_referenced_type(S=#state{mname=Emod},
		    ERef=#'Externalvaluereference'{pos=P,module=Emod,
						   value=Eval}) ->
    case match_parameters(ERef,S#state.parameters) of
	ERef ->
	    get_referenced1(S,Emod,Eval,P);
	OtherERef when record(OtherERef,'Externalvaluereference') ->
	    get_referenced_type(S,OtherERef);
	Value ->
	    {Emod,Value}
    end;
get_referenced_type(S,ERef=#'Externalvaluereference'{pos=Pos,module=Emod,
						value=Eval}) ->
    case match_parameters(ERef,S#state.parameters) of
	ERef ->
	    case lists:member(Emod,S#state.inputmodules) of
		true ->
		    get_referenced1(S,Emod,Eval,Pos);
		false ->
		    get_referenced(S,Emod,Eval,Pos)
	    end;
	OtherERef  ->
	    get_referenced_type(S,OtherERef)
    end;
get_referenced_type(S,#identifier{val=Name,pos=Pos}) ->
    get_referenced1(S,undefined,Name,Pos);
get_referenced_type(_S,Type) ->
    {undefined,Type}.

%% get_referenced/3
%% The referenced entity Ename may in case of an imported parameterized
%% type reference imported entities in the other module, which implies that
%% asn1_db:dbget will fail even though the referenced entity exists. Thus
%% Emod may be the module that imports the entity Ename and not holds the
%% data about Ename.
get_referenced(S,Emod,Ename,Pos) ->
    case asn1_db:dbget(Emod,Ename) of
	undefined ->
	    %% May be an imported entity in module Emod
%	    throw({error,{asn1,{undefined_type_or_value,{Emod,Ename}}}});
	    NewS = S#state{module=asn1_db:dbget(Emod,'MODULE')},
	    get_imported(NewS,Ename,Emod,Pos);
	T when record(T,typedef) ->
	    Spec = T#typedef.typespec,
	    case Spec#type.def of
		Tref when record(Tref,typereference) ->
		    Def = #'Externaltypereference'{module=Emod,
					     type=Tref#typereference.val,
					     pos=Tref#typereference.pos},


		    {Emod,T#typedef{typespec=Spec#type{def=Def}}};
		_ ->
		    {Emod,T} % should add check that T is exported here
	    end;
	V -> {Emod,V}
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
    case imported(S,Name) of
	{ok,Imodule} ->
	    case asn1_db:dbget(Imodule,'MODULE') of
		undefined ->
		    throw({error,{asn1,{module_not_found,Imodule}}});
		Im when record(Im,module) ->
		    case is_exported(Im,Name) of
			false ->
			    throw({error,
				   {asn1,{not_exported,{Im,Name}}}});
			_ ->
			    get_referenced_type(S,
						#'Externaltypereference'
						{module=Imodule,
						 type=Name,pos=Pos})
		    end
	    end;
	_ ->
	    renamed_reference(S,Name,Module)
    end.

renamed_reference(S,Name,Module) ->
    %% first check if there is a renamed type in this module
    %% second check if any type was imported with this name
    case ets:info(renamed_defs) of
	undefined -> throw({error,{asn1,{undefined_type,Name}}});
	_ ->
	    case ets:match(renamed_defs,{'$1',Name,Module}) of
		[] ->
		    case ets:info(original_imports) of
			undefined ->
			    throw({error,{asn1,{undefined_type,Name}}});
			_  ->
			    case ets:match(original_imports,{Module,'$1'}) of
				[] ->
				    throw({error,{asn1,{undefined_type,Name}}});
				[[ImportsList]] ->
				    case get_importmoduleoftype(ImportsList,Name) of
					undefined ->
					    throw({error,{asn1,{undefined_type,Name}}});
					NextMod ->
					    renamed_reference(S,Name,NextMod)
				    end
			    end
		    end;
		[[NewTypeName]] ->
		    get_referenced1(S,Module,NewTypeName,undefined)
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


match_parameters(Name,[]) ->
    Name;

match_parameters(#'Externaltypereference'{type=Name},[{#'Externaltypereference'{type=Name},NewName}|_T]) ->
    NewName;
match_parameters(#'Externaltypereference'{type=Name},[{{_,#'Externaltypereference'{type=Name}},NewName}|_T]) ->
    NewName;
% match_parameters(#'Externaltypereference'{type=Name},[{#typereference{val=Name},NewName}|T]) ->
%     NewName;
% match_parameters(#'Externaltypereference'{type=Name},[{{_,#typereference{val=Name}},NewName}|T]) ->
%     NewName;
%match_parameters(#typereference{val=Name},[{#typereference{val=Name},NewName}|T]) ->
%    NewName;
match_parameters(#'Externalvaluereference'{value=Name},[{#'Externalvaluereference'{value=Name},NewName}|_T]) ->
    NewName;
match_parameters(#'Externalvaluereference'{value=Name},[{{_,#'Externalvaluereference'{value=Name}},NewName}|_T]) ->
    NewName;
% match_parameters(#identifier{val=Name},[{#identifier{val=Name},NewName}|T]) ->
%     NewName;
% match_parameters(#identifier{val=Name},[{{_,#identifier{val=Name}},NewName}|T]) ->
%     NewName;
match_parameters({valueset,#type{def=#'Externaltypereference'{type=Name}}},
		 [{{_,#'Externaltypereference'{type=Name}},{valueset,#type{def=NewName}}}|_T]) ->
    NewName;
match_parameters({valueset,#type{def=#'Externaltypereference'{type=Name}}},
		 [{{_,#'Externaltypereference'{type=Name}},NewName}|_T]) ->
    NewName;
% match_parameters({valueset,#type{def=#'Externaltypereference'{type=Name}}},
% 		 [{{_,#typereference{val=Name}},{valueset,#type{def=NewName}}}|T]) ->
%     NewName;
% match_parameters({valueset,#type{def=#'Externaltypereference'{type=Name}}},
% 		 [{{_,#typereference{val=Name}},NewName}|T]) ->
%     NewName;

match_parameters(Name, [_H|T]) ->
    %%io:format("match_parameters(~p,~p)~n",[Name,[H|T]]),
    match_parameters(Name,T).

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


check_integer(_S,[],_C) ->
    ok;
check_integer(S,NamedNumberList,_C) ->
    case check_unique(NamedNumberList,2) of
	[] ->
	    check_int(S,NamedNumberList,[]);
	L when list(L) ->
	    error({type,{duplicates,L},S}),
	    unchanged

    end.

check_int(S,[{'NamedNumber',Id,Num}|T],Acc) when integer(Num) ->
    check_int(S,T,[{Id,Num}|Acc]);
check_int(S,[{'NamedNumber',Id,{identifier,_,Name}}|T],Acc) ->
    Val = dbget_ex(S,S#state.mname,Name),
    check_int(S,[{'NamedNumber',Id,Val#valuedef.value}|T],Acc);
check_int(_S,[],Acc) ->
    lists:keysort(2,Acc).



check_bitstring(_S,[],_Constr) ->
    [];
check_bitstring(S,NamedNumberList,_Constr) ->
    case check_unique(NamedNumberList,2) of
	[] ->
	    check_bitstr(S,NamedNumberList,[]);
	L when list(L) ->
	    error({type,{duplicates,L},S}),
	    unchanged
    end.

check_bitstr(S,[{'NamedNumber',Id,Num}|T],Acc)when integer(Num) ->
    check_bitstr(S,T,[{Id,Num}|Acc]);
check_bitstr(S,[{'NamedNumber',Id,Name}|T],Acc) when atom(Name) ->
%%check_bitstr(S,[{'NamedNumber',Id,{identifier,_,Name}}|T],Acc) ->
%%    io:format("asn1ct_check:check_bitstr/3 hej hop ~w~n",[Name]),
    Val = dbget_ex(S,S#state.mname,Name),
%%    io:format("asn1ct_check:check_bitstr/3: ~w~n",[Val]),
    check_bitstr(S,[{'NamedNumber',Id,Val#valuedef.value}|T],Acc);
check_bitstr(S,[],Acc) ->
    case check_unique(Acc,2) of
	[] ->
	    lists:keysort(2,Acc);
	L when list(L) ->
	    error({type,{duplicate_values,L},S}),
	    unchanged
    end.

%%check_bitstring(S,NamedNumberList,Constr) ->
%%    NamedNumberList.

%% Check INSTANCE OF
%% check that DefinedObjectClass is of TYPE-IDENTIFIER class
%% If Constraint is empty make it the general INSTANCE OF type
%% If Constraint is not empty make an inlined type
%% convert INSTANCE OF to the associated type
check_instance_of(S,DefinedObjectClass,Constraint) ->
    check_type_identifier(S,DefinedObjectClass),
    iof_associated_type(S,Constraint).


check_type_identifier(_S,'TYPE-IDENTIFIER') ->
    ok;
check_type_identifier(S,Eref=#'Externaltypereference'{}) ->
    case get_referenced_type(S,Eref) of
	{_,#classdef{name='TYPE-IDENTIFIER'}} -> ok;
	{_,TD=#typedef{typespec=#type{def=#'Externaltypereference'{}}}} ->
	    check_type_identifier(S,(TD#typedef.typespec)#type.def);
	_ ->
	    error({type,{"object set in type INSTANCE OF "
			 "not of class TYPE-IDENTIFIER",Eref},S})
    end.

iof_associated_type(S,[]) ->
    %% in this case encode/decode functions for INSTANCE OF must be
    %% generated
    case get(instance_of) of
	undefined ->
	    AssociateSeq = iof_associated_type1(S,[]),
	    Tag =
		case S#state.erule of
		    ber_bin_v2 ->
			[?TAG_CONSTRUCTED(?N_INSTANCE_OF)];
		    _ -> []
		end,
	    TypeDef=#typedef{checked=true,
			     name='INSTANCE OF',
			     typespec=#type{tag=Tag,
					    def=AssociateSeq}},
	    asn1_db:dbput(S#state.mname,'INSTANCE OF',TypeDef),
	    put(instance_of,generate);
	_ ->
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
    {ObjIdTag,C1TypeTag}=
	case S#state.erule of
	    ber_bin_v2 ->
		{[{'UNIVERSAL',8}],
		 [#tag{class='UNIVERSAL',
		       number=6,
		       type='IMPLICIT',
		       form=0}]};
	    _ -> {[{'UNIVERSAL','INTEGER'}],[]}
	end,
    TypeIdentifierRef=#'Externaltypereference'{module=ModuleName,
					       type='TYPE-IDENTIFIER'},
    ObjectIdentifier =
	#'ObjectClassFieldType'{classname=TypeIdentifierRef,
				class=[],
				fieldname={id,[]},
				type={fixedtypevaluefield,id,
				      #type{def='OBJECT IDENTIFIER'}}},
    Typefield =
	#'ObjectClassFieldType'{classname=TypeIdentifierRef,
				class=[],
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
		components=IOFComponents}.


%% returns the leading attribute, the constraint of the components and
%% the tablecinf value for the second component.
instance_of_constraints(_,[]) ->
    {false,[],[],[]};
instance_of_constraints(S,#constraint{c={simpletable,Type}}) ->
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
    TableCInf=#simpletableattributes{objectsetname=Name,
				     c_name='type-id',
				     c_index=1,
				     usedclassfield=id,
				     uniqueclassfield=id,
				     valueindex=[]},
    {TableCInf,[{simpletable,Name}],CRel,[{objfun,ObjectSetRef}]}.

%% Check ENUMERATED
%% ****************************************
%% Check that all values are unique
%% assign values to un-numbered identifiers
%% check that the constraints are allowed and correct
%% put the updated info back into database
check_enumerated(_S,[{Name,Number}|Rest],_Constr) when atom(Name), integer(Number)->
    %% already checked , just return the same list
    [{Name,Number}|Rest];
check_enumerated(S,NamedNumberList,_Constr) ->
    check_enum(S,NamedNumberList,[],[]).

%% identifiers are put in Acc2
%% returns either [{Name,Number}] or {[{Name,Number}],[{ExtName,ExtNumber}]}
%% the latter is returned if the ENUMERATION contains EXTENSIONMARK
check_enum(S,[{'NamedNumber',Id,Num}|T],Acc1,Acc2) when integer(Num) ->
    check_enum(S,T,[{Id,Num}|Acc1],Acc2);
check_enum(S,[{'NamedNumber',Id,{identifier,_,Name}}|T],Acc1,Acc2) ->
    Val = dbget_ex(S,S#state.mname,Name),
    check_enum(S,[{'NamedNumber',Id,Val#valuedef.value}|T],Acc1,Acc2);
check_enum(S,['EXTENSIONMARK'|T],Acc1,Acc2) ->
    NewAcc2 = lists:keysort(2,Acc1),
    NewList = enum_number(lists:reverse(Acc2),NewAcc2,0,[]),
    { NewList, check_enum(S,T,[],[])};
check_enum(S,[Id|T],Acc1,Acc2) when atom(Id) ->
    check_enum(S,T,Acc1,[Id|Acc2]);
check_enum(_S,[],Acc1,Acc2) ->
    NewAcc2 = lists:keysort(2,Acc1),
    enum_number(lists:reverse(Acc2),NewAcc2,0,[]).


% assign numbers to identifiers , numbers from 0 ... but must not
% be the same as already assigned to NamedNumbers
enum_number([H|T],[{Id,Num}|T2],Cnt,Acc) when Num > Cnt ->
    enum_number(T,[{Id,Num}|T2],Cnt+1,[{H,Cnt}|Acc]);
enum_number([H|T],[{Id,Num}|T2],Cnt,Acc) when Num < Cnt -> % negative Num
    enum_number(T,T2,Cnt+1,[{H,Cnt},{Id,Num}|Acc]);
enum_number([],L2,_Cnt,Acc) ->
    lists:concat([lists:reverse(Acc),L2]);
enum_number(L,[{Id,Num}|T2],Cnt,Acc) -> % Num == Cnt
    enum_number(L,T2,Cnt+1,[{Id,Num}|Acc]);
enum_number([H|T],[],Cnt,Acc) ->
    enum_number(T,[],Cnt+1,[{H,Cnt}|Acc]).


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
    case check_unique([C||C <- Components ,record(C,'ComponentType')]
		      ,#'ComponentType'.name) of
	[] ->
	    %% sort_canonical(Components),
	    Components2 = maybe_automatic_tags(S,Components),
	    %% check the table constraints from here. The outermost type
	    %% is Type, the innermost is Comps (the list of components)
	    NewComps =
		case check_each_component(S,Type,Components2) of
		    NewComponents when list(NewComponents) ->
			check_unique_sequence_tags(S,NewComponents),
			NewComponents;
		    Ret = {NewComponents,NewEcomps} ->
			TagComps = NewComponents ++
			    [Comp#'ComponentType'{prop='OPTIONAL'}|| Comp <- NewEcomps],
			%% extension components are like optionals when it comes to tagging
			check_unique_sequence_tags(S,TagComps),
			Ret
		end,
	    %% CRelInf is the "leading attribute" information
	    %% necessary for code generating of the look up in the
	    %% object set table,
	    %% i.e. getenc_ObjectSet/getdec_ObjectSet.
	    %% {objfun,ERef} tuple added in NewComps2 in tablecinf
	    %% field in type record of component relation constrained
	    %% type
%	    io:format("NewComps: ~p~n",[NewComps]),
	    {CRelInf,NewComps2} = componentrelation_leadingattr(S,NewComps),
%	    io:format("CRelInf: ~p~n",[CRelInf]),
%	    io:format("NewComps2: ~p~n",[NewComps2]),
	    %% CompListWithTblInf has got a lot unnecessary info about
	    %% the involved class removed, as the class of the object
	    %% set.
	    CompListWithTblInf = get_tableconstraint_info(S,Type,NewComps2),
%	    io:format("CompListWithTblInf: ~p~n",[CompListWithTblInf]),
	    {CRelInf,CompListWithTblInf};
	Dupl ->
		throw({error,{asn1,{duplicate_components,Dupl}}})
    end.

expand_components(S, [{'COMPONENTS OF',Type}|T]) ->
    CompList =
	case get_referenced_type(S,Type#type.def) of
	    {_,#typedef{typespec=#type{def=Seq}}} when record(Seq,'SEQUENCE') ->
		case Seq#'SEQUENCE'.components of
		    {Root,_Ext} -> Root;
		    Root -> Root
		end;
	    Err -> throw({error,{asn1,{illegal_COMPONENTS_OF,Err}}})
	end,
    expand_components(S,CompList) ++ expand_components(S,T);
expand_components(S,[H|T]) ->
    [H|expand_components(S,T)];
expand_components(_,[]) ->
    [].

check_unique_sequence_tags(S,[#'ComponentType'{prop=mandatory}|Rest]) ->
    check_unique_sequence_tags(S,Rest);
check_unique_sequence_tags(S,[C|Rest]) when record(C,'ComponentType') ->
    check_unique_sequence_tags1(S,Rest,[C]);% optional or default
check_unique_sequence_tags(S,[_ExtensionMarker|Rest]) ->
    check_unique_sequence_tags(S,Rest);
check_unique_sequence_tags(_S,[]) ->
    true.

check_unique_sequence_tags1(S,[C|Rest],Acc) when record(C,'ComponentType') ->
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

check_sequenceof(S,Type,Component) when record(Component,type) ->
    check_type(S,Type,Component).

check_set(S,Type,Components) ->
    {TableCInf,NewComponents} = check_sequence(S,Type,Components),
    case lists:member(der,S#state.options) of
	true when S#state.erule == ber;
		  S#state.erule == ber_bin ->
	    {Sorted,SortedComponents} =
		sort_components(S#state.tname,
				(S#state.module)#module.tagdefault,
				NewComponents),
	    {Sorted,TableCInf,SortedComponents};
	_ ->
	    {false,TableCInf,NewComponents}
    end.

sort_components(_TypeName,'AUTOMATIC',Components) ->
    {true,Components};
sort_components(TypeName,_TagDefault,Components) ->
    case untagged_choice(Components) of
	false ->
	    {true,sort_components1(TypeName,Components,[],[],[],[])};
	true ->
	    {dynamic,Components} % sort in run-time
    end.

sort_components1(TypeName,[C=#'ComponentType'{tags=[{'UNIVERSAL',_}|_R]}|Cs],
		 UnivAcc,ApplAcc,ContAcc,PrivAcc) ->
    sort_components1(TypeName,Cs,[C|UnivAcc],ApplAcc,ContAcc,PrivAcc);
sort_components1(TypeName,[C=#'ComponentType'{tags=[{'APPLICATION',_}|_R]}|Cs],
		 UnivAcc,ApplAcc,ContAcc,PrivAcc) ->
    sort_components1(TypeName,Cs,UnivAcc,[C|ApplAcc],ContAcc,PrivAcc);
sort_components1(TypeName,[C=#'ComponentType'{tags=[{'CONTEXT',_}|_R]}|Cs],
		 UnivAcc,ApplAcc,ContAcc,PrivAcc) ->
    sort_components1(TypeName,Cs,UnivAcc,ApplAcc,[C|ContAcc],PrivAcc);
sort_components1(TypeName,[C=#'ComponentType'{tags=[{'PRIVATE',_}|_R]}|Cs],
		 UnivAcc,ApplAcc,ContAcc,PrivAcc) ->
    sort_components1(TypeName,Cs,UnivAcc,ApplAcc,ContAcc,[C|PrivAcc]);
sort_components1(TypeName,[],UnivAcc,ApplAcc,ContAcc,PrivAcc) ->
    I = #'ComponentType'.tags,
    ascending_order_check(TypeName,sort_universal_type(UnivAcc)) ++
	ascending_order_check(TypeName,lists:keysort(I,ApplAcc)) ++
	ascending_order_check(TypeName,lists:keysort(I,ContAcc)) ++
	ascending_order_check(TypeName,lists:keysort(I,PrivAcc)).

ascending_order_check(TypeName,Components) ->
    ascending_order_check1(TypeName,Components),
    Components.

ascending_order_check1(TypeName,
		       [C1 = #'ComponentType'{tags=[{_,T}|_]},
			C2 = #'ComponentType'{tags=[{_,T}|_]}|Rest]) ->
    io:format("WARNING: Indistinct tag ~p in SET ~p, components ~p and ~p~n",
	      [T,TypeName,C1#'ComponentType'.name,C2#'ComponentType'.name]),
    ascending_order_check1(TypeName,[C2|Rest]);
ascending_order_check1(TypeName,
		       [C1 = #'ComponentType'{tags=[{'UNIVERSAL',T1}|_]},
			C2 = #'ComponentType'{tags=[{'UNIVERSAL',T2}|_]}|Rest]) ->
    case (asn1ct_gen_ber:decode_type(T1) == asn1ct_gen_ber:decode_type(T2)) of
	true ->
	    io:format("WARNING: Indistinct tags ~p and ~p in"
		      " SET ~p, components ~p and ~p~n",
		      [T1,T2,TypeName,C1#'ComponentType'.name,
		       C2#'ComponentType'.name]),
	    ascending_order_check1(TypeName,[C2|Rest]);
	_ ->
	    ascending_order_check1(TypeName,[C2|Rest])
    end;
ascending_order_check1(N,[_|Rest]) ->
    ascending_order_check1(N,Rest);
ascending_order_check1(_,[_]) ->
    ok;
ascending_order_check1(_,[]) ->
    ok.

sort_universal_type(Components) ->
    List = lists:map(fun(C) ->
			     #'ComponentType'{tags=[{_,T}|_]} = C,
			     {asn1ct_gen_ber:decode_type(T),C}
		     end,
		     Components),
    SortedList = lists:keysort(1,List),
    lists:map(fun(X)->element(2,X) end,SortedList).

untagged_choice([#'ComponentType'{typespec=#type{tag=[],def={'CHOICE',_}}}|_Rest]) ->
    true;
untagged_choice([_|Rest]) ->
    untagged_choice(Rest);
untagged_choice([]) ->
    false.

check_setof(S,Type,Component) when record(Component,type) ->
    check_type(S,Type,Component).

check_restrictedstring(_S,_Def,_Constr) ->
    ok.

check_objectidentifier(_S,_Constr) ->
    ok.

% check all aspects of a CHOICE
% - that all alternative names are unique
% - that all TAGS are ok (when TAG default is applied)
% - that each alternative is of a valid type
% - that the extension marks are valid
check_choice(S,Type,Components) when list(Components) ->
    case check_unique([C||C <- Components,
			  record(C,'ComponentType')],#'ComponentType'.name) of
	[] ->
    %%    sort_canonical(Components),
	    Components2 = maybe_automatic_tags(S,Components),
	    %NewComps =
	    case check_each_alternative(S,Type,Components2) of
		{NewComponents,NewEcomps} ->
		    check_unique_tags(S,NewComponents ++ NewEcomps),
		    {NewComponents,NewEcomps};
		NewComponents ->
		    check_unique_tags(S,NewComponents),
		    NewComponents
	    end;
%%	    CompListWithTblInf = get_tableconstraint_info(S,Type,NewComps);
	Dupl ->
	    throw({error,{asn1,{duplicate_choice_alternatives,Dupl}}})
    end;
check_choice(_S,_,[]) ->
    [].

%% probably dead code that should be removed
%%maybe_automatic_tags(S,{Rc,Ec}) ->
%%    {maybe_automatic_tags1(S,Rc,0),maybe_automatic_tags1(S,Ec,length(Rc))};
maybe_automatic_tags(#state{erule=per},C) ->
    C;
maybe_automatic_tags(#state{erule=per_bin},C) ->
    C;
maybe_automatic_tags(S,C) ->
    maybe_automatic_tags1(S,C,0).

maybe_automatic_tags1(S,C,TagNo) ->
    case (S#state.module)#module.tagdefault of
	'AUTOMATIC' ->
	    generate_automatic_tags(S,C,TagNo);
	_ ->
	    %% maybe is the module a multi file module were only some of
	    %% the modules have defaulttag AUTOMATIC TAGS then the names
	    %% of those types are saved in the table automatic_tags
	    Name= S#state.tname,
	    case is_automatic_tagged_in_multi_file(Name) of
		true ->
		    generate_automatic_tags(S,C,TagNo);
		false ->
		    C
	    end
    end.

is_automatic_tagged_in_multi_file(Name) ->
    case ets:info(automatic_tags) of
	undefined ->
	    %% this case when not multifile compilation
	    false;
	_ ->
	    case ets:member(automatic_tags,Name) of
		true ->
		     true;
		_ ->
		    false
	    end
    end.

generate_automatic_tags(_S,C,TagNo) ->
    case any_manual_tag(C) of
	true ->
	    C;
	false ->
	    generate_automatic_tags1(C,TagNo)
    end.

generate_automatic_tags1([H|T],TagNo) when record(H,'ComponentType') ->
    #'ComponentType'{typespec=Ts} = H,
    NewTs = Ts#type{tag=[#tag{class='CONTEXT',
			     number=TagNo,
			     type={default,'IMPLICIT'},
			     form= 0 }]}, % PRIMITIVE
    [H#'ComponentType'{typespec=NewTs}|generate_automatic_tags1(T,TagNo+1)];
generate_automatic_tags1([ExtMark|T],TagNo) -> % EXTENSIONMARK
    [ExtMark | generate_automatic_tags1(T,TagNo)];
generate_automatic_tags1([],_) ->
    [].

any_manual_tag([#'ComponentType'{typespec=#type{tag=[]}}|Rest]) ->
    any_manual_tag(Rest);
any_manual_tag([{'EXTENSIONMARK',_,_}|Rest]) ->
    any_manual_tag(Rest);
any_manual_tag([_|_Rest]) ->
    true;
any_manual_tag([]) ->
    false.


check_unique_tags(S,C) ->
    case (S#state.module)#module.tagdefault of
	'AUTOMATIC' ->
	    case any_manual_tag(C) of
		false -> true;
		_ -> collect_and_sort_tags(C,[])
	    end;
	_ ->
	    collect_and_sort_tags(C,[])
    end.

collect_and_sort_tags([C|Rest],Acc) when record(C,'ComponentType') ->
    collect_and_sort_tags(Rest,C#'ComponentType'.tags ++ Acc);
collect_and_sort_tags([_|Rest],Acc) ->
    collect_and_sort_tags(Rest,Acc);
collect_and_sort_tags([],Acc) ->
    {Dupl,_}= lists:mapfoldl(fun(El,El)->{{dup,El},El};(El,_Prev)-> {El,El} end,notag,lists:sort(Acc)),
    Dupl2 = [Dup|| {dup,Dup} <- Dupl],
    if
	length(Dupl2) > 0 ->
	    throw({error,{asn1,{duplicates_of_the_tags,Dupl2}}});
	true ->
	    true
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

check_each_component(S,Type,{Rlist,ExtList}) ->
    {check_each_component(S,Type,Rlist),
     check_each_component(S,Type,ExtList)};
check_each_component(S,Type,Components) ->
    check_each_component(S,Type,Components,[],[],noext).

check_each_component(S = #state{abscomppath=Path,recordtopname=TopName},Type,
		     [C|Ct],Acc,Extacc,Ext) when record(C,'ComponentType') ->
    #'ComponentType'{name=Cname,typespec=Ts,prop=Prop} = C,
    NewAbsCPath =
	case Ts#type.def of
	    #'Externaltypereference'{} -> [];
	    _ -> [Cname|Path]
	end,
    CheckedTs = check_type(S#state{abscomppath=NewAbsCPath,
				   recordtopname=[Cname|TopName]},Type,Ts),
    NewTags = get_taglist(S,CheckedTs),

    NewProp =
%	case lists:member(der,S#state.options) of
%	    true ->
%	    True ->
	case normalize_value(S,CheckedTs,Prop,[Cname|TopName]) of
	    mandatory -> mandatory;
	    'OPTIONAL' -> 'OPTIONAL';
	    DefaultValue -> {'DEFAULT',DefaultValue}
	end,
%	    _ ->
%		Prop
%	end,
    NewC = C#'ComponentType'{typespec=CheckedTs,prop=NewProp,tags=NewTags},
    case Ext of
	noext ->
	    check_each_component(S,Type,Ct,[NewC|Acc],Extacc,Ext);
	ext ->
	    check_each_component(S,Type,Ct,Acc,[NewC|Extacc],Ext)
    end;
check_each_component(S,Type,[_|Ct],Acc,Extacc,noext) -> % skip 'EXTENSIONMARK'
    check_each_component(S,Type,Ct,Acc,Extacc,ext);
check_each_component(_S,_,[_C|_Ct],_,_,ext) -> % skip 'EXTENSIONMARK'
    throw({error,{asn1,{too_many_extension_marks}}});
check_each_component(_S,_,[],Acc,Extacc,ext) ->
    {lists:reverse(Acc),lists:reverse(Extacc)};
check_each_component(_S,_,[],Acc,_,noext) ->
    lists:reverse(Acc).

check_each_alternative(S,Type,{Rlist,ExtList}) ->
    {check_each_alternative(S,Type,Rlist),
     check_each_alternative(S,Type,ExtList)};
check_each_alternative(S,Type,[C|Ct]) ->
    check_each_alternative(S,Type,[C|Ct],[],[],noext).

check_each_alternative(S=#state{abscomppath=Path,recordtopname=TopName},Type,[C|Ct],
		       Acc,Extacc,Ext) when record(C,'ComponentType') ->
    #'ComponentType'{name=Cname,typespec=Ts,prop=_Prop} = C,
    NewAbsCPath =
	case Ts#type.def of
	    #'Externaltypereference'{} -> [];
	    _ -> [Cname|Path]
	end,
    NewState =
	S#state{abscomppath=NewAbsCPath,recordtopname=[Cname|TopName]},
    CheckedTs = check_type(NewState,Type,Ts),
    NewTags = get_taglist(S,CheckedTs),
    NewC = C#'ComponentType'{typespec=CheckedTs,tags=NewTags},
    case Ext of
	noext ->
	    check_each_alternative(S,Type,Ct,[NewC|Acc],Extacc,Ext);
	ext ->
	    check_each_alternative(S,Type,Ct,Acc,[NewC|Extacc],Ext)
    end;

check_each_alternative(S,Type,[_|Ct],Acc,Extacc,noext) -> % skip 'EXTENSIONMARK'
    check_each_alternative(S,Type,Ct,Acc,Extacc,ext);
check_each_alternative(_S,_,[_C|_Ct],_,_,ext) -> % skip 'EXTENSIONMARK'
    throw({error,{asn1,{too_many_extension_marks}}});
check_each_alternative(_S,_,[],Acc,Extacc,ext) ->
    {lists:reverse(Acc),lists:reverse(Extacc)};
check_each_alternative(_S,_,[],Acc,_,noext) ->
    lists:reverse(Acc).

%% componentrelation_leadingattr/2 searches the structure for table
%% constraints, if any is found componentrelation_leadingattr/5 is
%% called.
componentrelation_leadingattr(S,CompList) ->
%    {Cs1,Cs2} =
    Cs =
	case CompList of
	    {Components,EComponents} when list(Components) ->
%		{Components,Components};
		Components ++ EComponents;
	    CompList when list(CompList) ->
%		{CompList,CompList}
		CompList
	end,
%    case any_simple_table(S,Cs1,[]) of

    %% get_simple_table_if_used/2 should find out whether there are any
    %% component relation constraints in the entire tree of Cs1 that
    %% relates to this level. It returns information about the simple
    %% table constraint necessary for the the call to
    %% componentrelation_leadingattr/6. The step when the leading
    %% attribute and the syntax tree is modified to support the code
    %% generating.
    case get_simple_table_if_used(S,Cs) of
	[] -> {false,CompList};
	STList ->
%	    componentrelation_leadingattr(S,Cs1,Cs2,STList,[],[])
	    componentrelation_leadingattr(S,Cs,Cs,STList,[],[])
    end.

%% componentrelation_leadingattr/6 when all components are searched
%% the new modified components are returned together with the "leading
%% attribute" information, which later is stored in the tablecinf
%% field in the SEQUENCE/SET record. The "leading attribute"
%% information is used to generate the lookup in the object set
%% table. The other information gathered in the #type.tablecinf field
%% is used in code generating phase too, to recognice the proper
%% components for "open type" encoding and to propagate the result of
%% the object set lookup when needed.
componentrelation_leadingattr(_,[],_CompList,_,[],NewCompList) ->
    {false,lists:reverse(NewCompList)};
componentrelation_leadingattr(_,[],_CompList,_,LeadingAttr,NewCompList) ->
    {lists:last(LeadingAttr),lists:reverse(NewCompList)}; %send all info in Ts later
componentrelation_leadingattr(S,[C|Cs],CompList,STList,Acc,CompAcc) ->
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
			UniqueFieldName =
			    case (catch get_unique_fieldname(#classdef{typespec=ClassDef})) of
				{error,'__undefined_'} ->
				    no_unique;
				{asn1,Msg,_} ->
				    error({type,Msg,S});
				Other -> Other
			    end,
%			UsedFieldName = get_used_fieldname(S,Attr,STList),
			%% Res should be done differently: even though
			%% a unique field name exists it is not
			%% certain that the ObjectClassFieldType of
			%% the simple table constraint picks that
			%% class field.
			Res = #simpletableattributes{objectsetname=OS,
%%						     c_name=asn1ct_gen:un_hyphen_var(Attr),
						     c_name=Attr,
						     c_index=N,
						     usedclassfield=UniqueFieldName,
						     uniqueclassfield=UniqueFieldName,
						     valueindex=ValueIndex},
			{[Res],C#'ComponentType'{typespec=NewTSpec}}
		end;
	    _ ->
		%% no constraint was found
		{[],C}
	end,
    componentrelation_leadingattr(S,Cs,CompList,STList,LAAcc++Acc,
				  [NewC|CompAcc]).

object_set_mod_name(_S,ObjSet) when atom(ObjSet) ->
    ObjSet;
object_set_mod_name(#state{mname=M},
		    #'Externaltypereference'{module=M,type=T}) ->
    T;
object_set_mod_name(S,#'Externaltypereference'{module=M,type=T}) ->
    case lists:member(M,S#state.inputmodules) of
	true ->
	    T;
	false ->
	    {M,T}
    end.

%% get_used_fieldname gets the used field of the class referenced by
%% the ObjectClassFieldType construct in the simple table constraint
%% corresponding to the component relation constraint that depends on
%% it.
% get_used_fieldname(_S,CName,[{[CName|_Rest],_,ClFieldName}|_RestSimpleT]) ->
%     ClFieldName;
% get_used_fieldname(S,CName,[_SimpleTC|Rest]) ->
%     get_used_fieldname(S,CName,Rest);
% get_used_fieldname(S,_,[]) ->
%     error({type,"Error in Simple table constraint",S}).

%% any_simple_table/3 checks if any of the components on this level is
%% constrained by a simple table constraint. It returns a list of
%% tuples with three elements. It is a name path to the place in the
%% type structure where the constraint is, and the name of the object
%% set and the referenced field in the class.
% any_simple_table(S = #state{mname=M,abscomppath=Path},
% 		 [#'ComponentType'{name=Name,typespec=Type}|Cs],Acc) ->
%     Constraint = Type#type.constraint,
%     case lists:keysearch(simpletable,1,Constraint) of
% 	{value,{_,#type{def=Ref}}} ->
% 	    %% This ObjectClassFieldType, which has a simple table
% 	    %% constraint, must pick a fixed type value, mustn't it ?
% 	    {ClassDef,[{_,ClassFieldName}]} = Type#type.def,
% 	    ST =
% 		case Ref of
% 		    #'Externaltypereference'{module=M,type=ObjSetName} ->
% 			{[Name|Path],ObjSetName,ClassFieldName};
% 		    _ ->
% 			{[Name|Path],Ref,ClassFieldName}
% 		end,
% 	    any_simple_table(S,Cs,[ST|Acc]);
% 	false ->
% 	    any_simple_table(S,Cs,Acc)
%     end;
% any_simple_table(_,[],Acc) ->
%     lists:reverse(Acc);
% any_simple_table(S,[_|Cs],Acc) ->
%     any_simple_table(S,Cs,Acc).

%% get_simple_table_if_used/2 searches the structure of Cs for any
%% component relation constraints due to the present level of the
%% structure. If there are any, the necessary information for code
%% generation of the look up functionality in the object set table are
%% returned.
get_simple_table_if_used(S,Cs) ->
    CNames = lists:map(fun(#'ComponentType'{name=Name}) -> Name;
			  (_) -> [] %% in case of extension marks
		       end,
		       Cs),
    RefedSimpleTable=any_component_relation(S,Cs,CNames,[],[]),
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

%% get_simple_table_info searches the commponents Cs by the path from
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
get_simple_table_info(S,Cs,[AtList|Rest]) ->
%%    [get_simple_table_info1(S,Cs,AtList,S#state.abscomppath)|get_simple_table_info(S,Cs,Rest)];
    [get_simple_table_info1(S,Cs,AtList,[])|get_simple_table_info(S,Cs,Rest)];
get_simple_table_info(_,_,[]) ->
    [].
get_simple_table_info1(S,Cs,[Cname|Cnames],Path) when list(Cs) ->
    case lists:keysearch(Cname,#'ComponentType'.name,Cs) of
	{value,C} ->
	    get_simple_table_info1(S,C,Cnames,[Cname|Path]);
        _ ->
	    error({type,"Missing expected simple table constraint",S})
    end;
get_simple_table_info1(S,#'ComponentType'{typespec=TS},[],Path) ->
    %% In this component there must be a simple table constraint
    %% o.w. the asn1 code is wrong.
    #type{def=OCFT,constraint=Cnstr} = TS,
    case Cnstr of
	[{simpletable,_OSRef}] ->
	    #'ObjectClassFieldType'{classname=ClRef,
				    class=ObjectClass,
				    fieldname=FieldName} = OCFT,
%	    #'ObjectClassFieldType'{ObjectClass,FieldType} = ObjectClassFieldType,
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
	    UniqueName =
		case (catch get_unique_fieldname(ClassDef)) of
		    {error,'__undefined_'} -> no_unique;
		    {asn1,Msg,_} ->
			error({type,Msg,S});
		    Other -> Other
		end,
	    {lists:reverse(Path),ObjectClassFieldName,UniqueName};
	_ ->
	    error({type,{asn1,"missing expected simple table constraint",
			 Cnstr},S})
    end;
get_simple_table_info1(S,#'ComponentType'{typespec=TS},Cnames,Path) ->
    Components = get_atlist_components(TS#type.def),
    get_simple_table_info1(S,Components,Cnames,Path).

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
any_component_relation(S,[C|Cs],CNames,NamePath,Acc) ->
    CName = C#'ComponentType'.name,
    Type = C#'ComponentType'.typespec,
    CRelPath =
	case Type#type.constraint of
	    [{componentrelation,_,AtNotation}] ->
		%% Found component relation constraint, now check
		%% whether this constraint is relevant for the level
		%% where the search started
		AtNot = extract_at_notation(AtNotation),
		%% evaluate_atpath returns the relative path to the
		%% simple table constraint from where the component
		%% relation is found.
		evaluate_atpath(S#state.abscomppath,NamePath,CNames,AtNot);
	    _ ->
		[]
	end,
    InnerAcc =
	case {Type#type.inlined,
	      asn1ct_gen:type(asn1ct_gen:get_inner(Type#type.def))} of
	    {no,{constructed,bif}} ->
		InnerCs =
		    case get_components(Type#type.def) of
			{IC1,_IC2} -> IC1 ++ IC1;
			IC -> IC
		    end,
		%% here we are interested in components of an
		%% SEQUENCE/SET OF as well as SEQUENCE, SET and CHOICE
		any_component_relation(S,InnerCs,CNames,[CName|NamePath],[]);
	    _ ->
		[]
	end,
    any_component_relation(S,Cs,CNames,NamePath,InnerAcc++CRelPath++Acc);
any_component_relation(_,[],_,_,Acc) ->
    Acc.

%% evaluate_atpath/4 finds out whether the at notation refers to the
%% search level. The list of referenced names in the AtNot list shall
%% begin with a name that exists on the level it refers to. If the
%% found AtPath is referring to the same sub-branch as the simple table
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
evaluate_atpath(TopPath,NamePath,Cnames,{outermost,AtPath=[_Ref|_Refs]}) ->
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
		_ -> error({type,{asn1,"failed to analyze at-path",AtPath}})
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
    Cs;
get_components(_,#'SET'{components=Cs}) ->
    Cs;
get_components(_,{'CHOICE',Cs}) ->
    Cs;
get_components(any,{'SEQUENCE OF',#type{def=Def}}) ->
    get_components(any,Def);
get_components(any,{'SET OF',#type{def=Def}}) ->
    get_components(any,Def);
get_components(_,_) ->
    [].


extract_at_notation([{Level,[#'Externalvaluereference'{value=Name}|Rest]}]) ->
    {Level,[Name|extract_at_notation1(Rest)]};
extract_at_notation(At) ->
    exit({error,{asn1,{at_notation,At}}}).
extract_at_notation1([#'Externalvaluereference'{value=Name}|Rest]) ->
    [Name|extract_at_notation1(Rest)];
extract_at_notation1([]) ->
    [].

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
	case Constraint of
	    [{componentrelation,{_,_,ObjectSet},AtList}|_Rest] ->
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
	    _Other ->
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
	case Cons of
	    [{componentrelation,{_,_,ObjectSet},AtList}|_Rest] ->
		%% This AtList must have an "outermost" at sign to be
		%% relevant here.
		[{_,AL=[#'Externalvaluereference'{value=_Attr}|_R1]}|_R2]
		    = AtList,
%%		#'ObjectClassFieldType'{class=ClassDef} = Def,
		ClassDef = get_ObjectClassFieldType_classdef(S,Def),
		AtPath =
		    lists:map(fun(#'Externalvaluereference'{value=V})->V end,
			      AL),
		[{ObjectSet,AtPath,ClassDef,Path}];
	    _ ->
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
	    [] -> error({type,{asn1,"element in at list must be a "
			       "SEQUENCE, SET or CHOICE.",Name},S});
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
    error({type,{asn1,"component of at-list was not"
		 " found in substructure",Name},S}).

get_unique_fieldname(ClassDef) ->
%%    {_,Fields,_} = ClassDef#classdef.typespec,
    Fields = (ClassDef#classdef.typespec)#objectclass.fields,
    get_unique_fieldname(Fields,[]).

get_unique_fieldname([],[]) ->
    throw({error,'__undefined_'});
get_unique_fieldname([],[Name]) ->
    Name;
get_unique_fieldname([],Acc) ->
    throw({asn1,'only one UNIQUE field is allowed in CLASS',Acc});
get_unique_fieldname([{fixedtypevaluefield,Name,_,'UNIQUE',_}|Rest],Acc) ->
    get_unique_fieldname(Rest,[Name|Acc]);
get_unique_fieldname([_H|T],Acc) ->
    get_unique_fieldname(T,Acc).

get_tableconstraint_info(S,Type,{CheckedTs,EComps}) ->
    {get_tableconstraint_info(S,Type,CheckedTs,[]),
     get_tableconstraint_info(S,Type,EComps,[])};
get_tableconstraint_info(S,Type,CheckedTs) ->
    get_tableconstraint_info(S,Type,CheckedTs,[]).

get_tableconstraint_info(_S,_Type,[],Acc) ->
    lists:reverse(Acc);
get_tableconstraint_info(S,Type,[C|Cs],Acc) ->
    CheckedTs = C#'ComponentType'.typespec,
    AccComp =
	case CheckedTs#type.def of
	    %% ObjectClassFieldType
	    OCFT=#'ObjectClassFieldType'{class=#objectclass{},
					 type=_AType} ->
%		AType = get_ObjectClassFieldType(S,Fields,FieldRef),
%		RefedFieldName =
%		    get_referencedclassfield(CheckedTs#type.def),%is probably obsolete
		NewOCFT =
		    OCFT#'ObjectClassFieldType'{class=[]},
		C#'ComponentType'{typespec=
				  CheckedTs#type{
%				    def=AType,
				    def=NewOCFT
				    }};
%				    constraint=[{tableconstraint_info,
%						 FieldRef}]}};
	    {'SEQUENCE OF',SOType} when record(SOType,type),
					(element(1,SOType#type.def)=='CHOICE') ->
		CTypeList = element(2,SOType#type.def),
		NewInnerCList =
		    get_tableconstraint_info(S,Type,CTypeList,[]),
		C#'ComponentType'{typespec=
				  CheckedTs#type{
				    def={'SEQUENCE OF',
					 SOType#type{def={'CHOICE',
							  NewInnerCList}}}}};
	    {'SET OF',SOType} when record(SOType,type),
				   (element(1,SOType#type.def)=='CHOICE') ->
		CTypeList = element(2,SOType#type.def),
		NewInnerCList =
		    get_tableconstraint_info(S,Type,CTypeList,[]),
		C#'ComponentType'{typespec=
				  CheckedTs#type{
				    def={'SET OF',
					 SOType#type{def={'CHOICE',
							  NewInnerCList}}}}};
	    _ ->
		C
	end,
    get_tableconstraint_info(S,Type,Cs,[AccComp|Acc]).

get_referenced_fieldname([{_,FirstFieldname}]) ->
    {FirstFieldname,[]};
get_referenced_fieldname([{_,FirstFieldname}|Rest]) ->
    {FirstFieldname,lists:map(fun(X)->element(2,X) end,Rest)};
get_referenced_fieldname(Def) ->
    {no_type,Def}.

%% get_ObjectClassFieldType extracts the type from the chain of
%% objects that leads to a final type.
get_ObjectClassFieldType(S,ERef,PrimFieldNameList) when
  record(ERef,'Externaltypereference') ->
    {_,Type} = get_referenced_type(S,ERef),
    ClassSpec = check_class(S,Type),
    Fields = ClassSpec#objectclass.fields,
    get_ObjectClassFieldType(S,Fields,PrimFieldNameList);
get_ObjectClassFieldType(S,Fields,L=[_PrimFieldName1|_Rest]) ->
    check_PrimitiveFieldNames(S,Fields,L),
    get_OCFType(S,Fields,L).

check_PrimitiveFieldNames(_S,_Fields,_) ->
    ok.

%% get_ObjectClassFieldType_classdef gets the def of the class of the
%% ObjectClassFieldType, i.e. the objectclass record. If the type has
%% been checked (it may be a field type of an internal SEQUENCE) the
%% class field = [], then the classdef has to be fetched by help of
%% the class reference in the classname field.
get_ObjectClassFieldType_classdef(S,#'ObjectClassFieldType'{classname=Name,
							  class=[]}) ->
    {_,#classdef{typespec=TS}} = get_referenced_type(S,Name),
    TS;
get_ObjectClassFieldType_classdef(_,#'ObjectClassFieldType'{class=Cl}) ->
    Cl.

get_OCFType(S,Fields,[{_FieldType,PrimFieldName}|Rest]) ->
    case lists:keysearch(PrimFieldName,2,Fields) of
	{value,{fixedtypevaluefield,_,Type,_Unique,_OptSpec}} ->
	    {fixedtypevaluefield,PrimFieldName,Type};
	{value,{objectfield,_,Type,_Unique,_OptSpec}} ->
	    {_,ClassDef} = get_referenced_type(S,Type#type.def),
	    CheckedCDef = check_class(S#state{type=ClassDef,
					      tname=ClassDef#classdef.name},
				      ClassDef#classdef.typespec),
	    get_OCFType(S,CheckedCDef#objectclass.fields,Rest);
	{value,{objectsetfield,_,Type,_OptSpec}} ->
	    {_,ClassDef} = get_referenced_type(S,Type#type.def),
	    CheckedCDef = check_class(S#state{type=ClassDef,
					      tname=ClassDef#classdef.name},
				      ClassDef#classdef.typespec),
	    get_OCFType(S,CheckedCDef#objectclass.fields,Rest);

	{value,Other} ->
	    {element(1,Other),PrimFieldName};
	_  ->
	    error({type,"undefined FieldName in ObjectClassFieldType",S})
    end.

get_taglist(#state{erule=per},_) ->
    [];
get_taglist(#state{erule=per_bin},_) ->
    [];
get_taglist(S,Ext) when record(Ext,'Externaltypereference') ->
    {_,T} = get_referenced_type(S,Ext),
    get_taglist(S,T#typedef.typespec);
get_taglist(S,Tref) when record(Tref,typereference) ->
    {_,T} = get_referenced_type(S,Tref),
    get_taglist(S,T#typedef.typespec);
get_taglist(S,Type) when record(Type,type) ->
    case Type#type.tag of
	[] ->
	    get_taglist(S,Type#type.def);
	[Tag|_]  ->
% 	    case lists:member(S#state.erule,[ber,ber_bin]) of
% 		true ->
% 		   lists:map(fun(Tx) -> asn1ct_gen:def_to_tag(Tx) end,Type#type.tag);
% 		_ ->
	    [asn1ct_gen:def_to_tag(Tag)]
%	    end
    end;
get_taglist(S,{'CHOICE',{Rc,Ec}}) ->
    get_taglist(S,{'CHOICE',Rc ++ Ec});
get_taglist(S,{'CHOICE',Components}) ->
    get_taglist1(S,Components);
%% ObjectClassFieldType OTP-4390
get_taglist(_S,#'ObjectClassFieldType'{type={typefield,_}}) ->
    [];
get_taglist(S,#'ObjectClassFieldType'{type={fixedtypevaluefield,_,Type}}) ->
    get_taglist(S,Type);
get_taglist(S,{ERef=#'Externaltypereference'{},FieldNameList})
  when list(FieldNameList) ->
    case get_ObjectClassFieldType(S,ERef,FieldNameList) of
	Type when record(Type,type) ->
	    get_taglist(S,Type);
	{fixedtypevaluefield,_,Type} -> get_taglist(S,Type);
	{TypeFieldName,_} when atom(TypeFieldName) -> []%should check if allowed
    end;
get_taglist(S,{ObjCl,FieldNameList}) when record(ObjCl,objectclass),
					  list(FieldNameList) ->
    case get_ObjectClassFieldType(S,ObjCl#objectclass.fields,FieldNameList) of
	Type when record(Type,type) ->
	    get_taglist(S,Type);
	{fixedtypevaluefield,_,Type} -> get_taglist(S,Type);
	{TypeFieldName,_} when atom(TypeFieldName) -> []%should check if allowed
    end;
get_taglist(S,Def) ->
    case lists:member(S#state.erule,[ber_bin_v2]) of
	false ->
	    case Def of
		'ASN1_OPEN_TYPE' -> % open_type has no UNIVERSAL tag as such
		    [];
		_ ->
		    [asn1ct_gen:def_to_tag(Def)]
	    end;
	_ ->
	    []
    end.

get_taglist1(S,[#'ComponentType'{name=_Cname,tags=TagL}|Rest]) when list(TagL) ->
    %% tag_list has been here , just return TagL and continue with next alternative
    TagL ++ get_taglist1(S,Rest);
get_taglist1(S,[#'ComponentType'{typespec=Ts,tags=undefined}|Rest]) ->
    get_taglist(S,Ts) ++ get_taglist1(S,Rest);
get_taglist1(S,[_H|Rest]) -> % skip EXTENSIONMARK
    get_taglist1(S,Rest);
get_taglist1(_S,[]) ->
    [].

dbget_ex(_S,Module,Key) ->
    case asn1_db:dbget(Module,Key) of
	undefined ->

	    throw({error,{asn1,{undefined,{Module,Key}}}}); % this is catched on toplevel type or value
	T -> T
    end.

merge_tags(T1, T2) when list(T2) ->
    merge_tags2(T1 ++ T2, []);
merge_tags(T1, T2) ->
    merge_tags2(T1 ++ [T2], []).

merge_tags2([T1= #tag{type='IMPLICIT'}, T2 |Rest], Acc) ->
    merge_tags2([T1#tag{type=T2#tag.type, form=T2#tag.form}|Rest],Acc);
merge_tags2([T1= #tag{type={default,'IMPLICIT'}}, T2 |Rest], Acc) ->
    merge_tags2([T1#tag{type=T2#tag.type, form=T2#tag.form}|Rest],Acc);
merge_tags2([H|T],Acc) ->
    merge_tags2(T, [H|Acc]);
merge_tags2([], Acc) ->
    lists:reverse(Acc).

merge_constraints(C1, []) ->
    C1;
merge_constraints([], C2) ->
    C2;
merge_constraints(C1, C2) ->
    {SList,VList,PAList,Rest} = splitlist(C1++C2,[],[],[],[]),
    SizeC = merge_constraints(SList),
    ValueC = merge_constraints(VList),
    PermAlphaC = merge_constraints(PAList),
    case Rest of
        [] ->
            SizeC ++ ValueC ++ PermAlphaC;
        _ ->
            throw({error,{asn1,{not_implemented,{merge_constraints,Rest}}}})
    end.

merge_constraints([]) -> [];
merge_constraints([C1 = {_,{Low1,High1}},{_,{Low2,High2}}|Rest]) when Low1 >= Low2,
                                                                      High1 =< High2 ->
    merge_constraints([C1|Rest]);
merge_constraints([C1={'PermittedAlphabet',_},C2|Rest]) ->
    [C1|merge_constraints([C2|Rest])];
merge_constraints([C1 = {_,{_Low1,_High1}},C2 = {_,{_Low2,_High2}}|_Rest]) ->
    throw({error,asn1,{conflicting_constraints,{C1,C2}}});
merge_constraints([C]) ->
    [C].

splitlist([C={'SizeConstraint',_}|Rest],Sacc,Vacc,PAacc,Restacc) ->
    splitlist(Rest,[C|Sacc],Vacc,PAacc,Restacc);
splitlist([C={'ValueRange',_}|Rest],Sacc,Vacc,PAacc,Restacc) ->
    splitlist(Rest,Sacc,[C|Vacc],PAacc,Restacc);
splitlist([C={'PermittedAlphabet',_}|Rest],Sacc,Vacc,PAacc,Restacc) ->
    splitlist(Rest,Sacc,Vacc,[C|PAacc],Restacc);
splitlist([C|Rest],Sacc,Vacc,PAacc,Restacc) ->
    splitlist(Rest,Sacc,Vacc,PAacc,[C|Restacc]);
splitlist([],Sacc,Vacc,PAacc,Restacc) ->
    {lists:reverse(Sacc),
     lists:reverse(Vacc),
     lists:reverse(PAacc),
     lists:reverse(Restacc)}.



storeindb(M) when record(M,module) ->
    TVlist = M#module.typeorval,
    NewM = M#module{typeorval=findtypes_and_values(TVlist)},
    asn1_db:dbnew(NewM#module.name),
    asn1_db:dbput(NewM#module.name,'MODULE',  NewM),
    Res = storeindb(NewM#module.name,TVlist,[]),
    include_default_class(NewM#module.name),
    include_default_type(NewM#module.name),
    Res.

storeindb(Module,[H|T],ErrAcc) when record(H,typedef) ->
    storeindb(Module,H#typedef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,valuedef) ->
    storeindb(Module,H#valuedef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,ptypedef) ->
    storeindb(Module,H#ptypedef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,classdef) ->
    storeindb(Module,H#classdef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,pvaluesetdef) ->
    storeindb(Module,H#pvaluesetdef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,pobjectdef) ->
    storeindb(Module,H#pobjectdef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,pvaluedef) ->
    storeindb(Module,H#pvaluedef.name,H,T,ErrAcc);
storeindb(_,[],[]) -> ok;
storeindb(_,[],ErrAcc) ->
    {error,ErrAcc}.

storeindb(Module,Name,H,T,ErrAcc) ->
    case asn1_db:dbget(Module,Name) of
	undefined ->
	    asn1_db:dbput(Module,Name,H),
	    storeindb(Module,T,ErrAcc);
	_ ->
	    case H of
		_Type when record(H,typedef) ->
		    error({type,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		_Type when record(H,valuedef) ->
		    error({value,"already defined",
			   #state{mname=Module,value=H,vname=Name}});
		_Type when record(H,ptypedef) ->
		    error({ptype,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		_Type when record(H,pobjectdef) ->
		    error({ptype,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		_Type when record(H,pvaluesetdef) ->
		    error({ptype,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		_Type when record(H,pvaluedef) ->
		    error({ptype,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		_Type when record(H,classdef) ->
		    error({class,"already defined",
			   #state{mname=Module,value=H,vname=Name}})
	    end,
	    storeindb(Module,T,[H|ErrAcc])
    end.

findtypes_and_values(TVList) ->
    findtypes_and_values(TVList,[],[],[],[],[],[]).%% Types,Values,
%% Parameterizedtypes,Classes,Objects and ObjectSets

findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,typedef),record(H#typedef.typespec,'Object') ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,[H#typedef.name|Oacc],OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,typedef),record(H#typedef.typespec,'ObjectSet') ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,Oacc,[H#typedef.name|OSacc]);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,typedef) ->
    findtypes_and_values(T,[H#typedef.name|Tacc],Vacc,Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,valuedef) ->
    findtypes_and_values(T,Tacc,[H#valuedef.name|Vacc],Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,ptypedef) ->
    findtypes_and_values(T,Tacc,Vacc,[H#ptypedef.name|Pacc],Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,classdef) ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,[H#classdef.name|Cacc],Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,pvaluedef) ->
    findtypes_and_values(T,Tacc,[H#pvaluedef.name|Vacc],Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,pvaluesetdef) ->
    findtypes_and_values(T,Tacc,[H#pvaluesetdef.name|Vacc],Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,pobjectdef) ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,[H#pobjectdef.name|Oacc],OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,pobjectsetdef) ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,Oacc,[H#pobjectsetdef.name|OSacc]);
findtypes_and_values([],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) ->
    {lists:reverse(Tacc),lists:reverse(Vacc),lists:reverse(Pacc),
     lists:reverse(Cacc),lists:reverse(Oacc),lists:reverse(OSacc)}.



error({export,Msg,#state{mname=Mname,type=Ref,tname=Typename}}) ->
    Pos = Ref#'Externaltypereference'.pos,
    io:format("asn1error:~p:~p:~p ~p~n",[Pos,Mname,Typename,Msg]),
    {error,{export,Pos,Mname,Typename,Msg}};
error({type,Msg,#state{mname=Mname,type=Type,tname=Typename}})
  when record(Type,typedef) ->
    io:format("asn1error:~p:~p:~p ~p~n",
	      [Type#typedef.pos,Mname,Typename,Msg]),
    {error,{type,Type#typedef.pos,Mname,Typename,Msg}};
error({type,Msg,#state{mname=Mname,type=Type,tname=Typename}})
  when record(Type,ptypedef) ->
    io:format("asn1error:~p:~p:~p ~p~n",
	      [Type#ptypedef.pos,Mname,Typename,Msg]),
    {error,{type,Type#ptypedef.pos,Mname,Typename,Msg}};
error({type,Msg,#state{mname=Mname,value=Value,vname=Valuename}})
  when record(Value,valuedef) ->
    io:format("asn1error:~p:~p:~p ~p~n",[Value#valuedef.pos,Mname,Valuename,Msg]),
    {error,{type,Value#valuedef.pos,Mname,Valuename,Msg}};
error({type,Msg,#state{mname=Mname,type=Type,tname=Typename}})
  when record(Type,pobjectdef) ->
    io:format("asn1error:~p:~p:~p ~p~n",
	      [Type#pobjectdef.pos,Mname,Typename,Msg]),
    {error,{type,Type#pobjectdef.pos,Mname,Typename,Msg}};
error({value,Msg,#state{mname=Mname,value=Value,vname=Valuename}}) ->
    io:format("asn1error:~p:~p:~p ~p~n",[Value#valuedef.pos,Mname,Valuename,Msg]),
    {error,{value,Value#valuedef.pos,Mname,Valuename,Msg}};
error({Other,Msg,#state{mname=Mname,value=#valuedef{pos=Pos},vname=Valuename}}) ->
    io:format("asn1error:~p:~p:~p ~p~n",[Pos,Mname,Valuename,Msg]),
    {error,{Other,Pos,Mname,Valuename,Msg}};
error({Other,Msg,#state{mname=Mname,type=#typedef{pos=Pos},tname=Typename}}) ->
    io:format("asn1error:~p:~p:~p ~p~n",[Pos,Mname,Typename,Msg]),
    {error,{Other,Pos,Mname,Typename,Msg}};
error({Other,Msg,#state{mname=Mname,type=#classdef{pos=Pos},tname=Typename}}) ->
    io:format("asn1error:~p:~p:~p ~p~n",[Pos,Mname,Typename,Msg]),
    {error,{Other,Pos,Mname,Typename,Msg}}.

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
			 prop='OPTIONAL'},

    Indirect_reference =
	#'ComponentType'{name='indirect-reference',
			 typespec=#type{def='INTEGER'},
			 prop='OPTIONAL'},

    Single_ASN1_type =
	#'ComponentType'{name='single-ASN1-type',
			 typespec=#type{tag=[{tag,'CONTEXT',0,
					      'EXPLICIT',32}],
					def='ANY'},
			 prop=mandatory},

    Octet_aligned =
	#'ComponentType'{name='octet-aligned',
			 typespec=#type{tag=[{tag,'CONTEXT',1,
					      'IMPLICIT',32}],
					def='OCTET STRING'},
			 prop=mandatory},

    Arbitrary =
	#'ComponentType'{name=arbitrary,
			 typespec=#type{tag=[{tag,'CONTEXT',2,
					      'IMPLICIT',32}],
					def={'BIT STRING',[]}},
			 prop=mandatory},

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


include_default_class(Module) ->
    NameAbsList = default_class_list(),
    include_default_class1(Module,NameAbsList).

include_default_class1(_,[]) ->
    ok;
include_default_class1(Module,[{Name,TS}|_Rest]) ->
    case asn1_db:dbget(Module,Name) of
	undefined ->
	    C = #classdef{checked=true,name=Name,
			  typespec=TS},
	    asn1_db:dbput(Module,Name,C);
	_ -> ok
    end.

default_class_list() ->
    [{'TYPE-IDENTIFIER',
      {objectclass,
       [{fixedtypevaluefield,
	 id,
	 {type,[],'OBJECT IDENTIFIER',[]},
	 'UNIQUE',
	 'MANDATORY'},
	{typefield,'Type','MANDATORY'}],
       {'WITH SYNTAX',
	[{typefieldreference,'Type'},
	 'IDENTIFIED',
	 'BY',
	 {valuefieldreference,id}]}}},
     {'ABSTRACT-SYNTAX',
      {objectclass,
       [{fixedtypevaluefield,
	 id,
	 {type,[],'OBJECT IDENTIFIER',[]},
	 'UNIQUE',
	 'MANDATORY'},
	{typefield,'Type','MANDATORY'},
	{fixedtypevaluefield,
	 property,
	 {type,
	  [],
	  {'BIT STRING',[]},
	  []},
	 undefined,
	 {'DEFAULT',
	  [0,1,0]}}],
       {'WITH SYNTAX',
	[{typefieldreference,'Type'},
	 'IDENTIFIED',
	 'BY',
	 {valuefieldreference,id},
	 ['HAS',
	  'PROPERTY',
	  {valuefieldreference,property}]]}}}].
