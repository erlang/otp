%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
%%
-module(asn1ct_check).

%% Main Module for ASN.1 compile time functions

%-compile(export_all).
-export([check/2,storeindb/2]).
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
    Element1 = fun(X) -> element(1,X) end,

    %% initialize internal book keeping
    save_asn1db_uptodate(S,S#state.erule,S#state.mname),
    put(top_module,S#state.mname),

    _Perror = checkp(S,ParameterizedTypes,[]), % must do this before the templates are used
    
    %% table to save instances of parameterized objects,object sets
    asn1ct:create_ets_table(parameterized_objects,[named_table]),
    asn1ct:create_ets_table(inlined_objects,[named_table]),


    Terror = checkt(S,Types,[]),
    ?dbg("checkt finished with errors:~n~p~n~n",[Terror]),

    %% get parameterized object sets sent to checkt/3
    %% and update Terror

    {PObjSetNames1,Terror2} = filter_errors(IsPObjSet,Terror),

    Verror = checkv(S,Values ++ ObjectSets,[]), %value sets may be parsed as object sets
    ?dbg("checkv finished with errors:~n~p~n~n",[Verror]),
     %% get information object classes wrongly sent to checkt/3
     %% and update Terror2

    {AddClasses,Terror3} = filter_errors(IsClass,Terror2),

    NewClasses = Classes++AddClasses,

    Cerror = checkc(S,NewClasses,[]),
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
    InlinedObjTuples = ets:tab2list(inlined_objects),
    InlinedObjects = lists:map(Element2,InlinedObjTuples),
    ets:delete(inlined_objects),
    ParameterizedElems = ets:tab2list(parameterized_objects),
    ParObjectSets = lists:filter(fun({_OSName,objectset,_}) -> true;
				    (_)-> false end,ParameterizedElems),
    ParObjectSetNames = lists:map(Element1,ParObjectSets),
    ParTypes = lists:filter(fun({_TypeName,type,_}) -> true;
			       (_) -> false end, ParameterizedElems),
    ParTypesNames = lists:map(Element1,ParTypes),
    ets:delete(parameterized_objects),
    put(asn1_reference,undefined),

    Exporterror = check_exports(S,S#state.module),
    ImportError = check_imports(S,S#state.module),

    case {Terror3,Verror5,Cerror,Oerror,Exporterror,ImportError} of
	{[],[],[],[],[],[]} -> 
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
	_ ->{error,{asn1,lists:flatten([Terror3,Verror5,Cerror,
					Oerror,Exporterror,ImportError])}}
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

check_imports(S,Module = #module{ }) ->
    case Module#module.imports of
	{imports,[]} ->
	    [];
	{imports,ImportList} when is_list(ImportList) ->
	    check_imports2(S,ImportList,[]);
	_ ->
	    []
    end.
check_imports2(_S,[],Acc) ->
    Acc;
check_imports2(S,[#'SymbolsFromModule'{symbols=Imports,module=ModuleRef}|SFMs],Acc) ->
    NameOfDef =
	fun(#'Externaltypereference'{type=N}) -> N;
	   (#'Externalvaluereference'{value=N}) -> N
	end,
    Module = NameOfDef(ModuleRef),
    Refs = [{M,R}||{{M,_},R} <- [{catch get_referenced_type(S,Ref),Ref}||Ref <- Imports]],
    {Illegal,Other} = lists:splitwith(fun({error,_}) -> true;(_) -> false end,
				      Refs),
    ChainedRefs = [R||{M,R} <- Other, M =/= Module],
    IllegalRefs = [R||{error,R} <- Illegal] ++ 
	[R||{M,R} <- ChainedRefs, 
	    ok =/= chained_import(S,Module,M,NameOfDef(R))],
    ReportError =
	fun(Ref) ->
		NewS=S#state{type=Ref,tname=NameOfDef(Ref)},
		error({import,"imported undefined entity",NewS})
	end,
    check_imports2(S,SFMs,[ReportError(Err)||Err <- IllegalRefs]++Acc).

chained_import(S,ImpMod,DefMod,Name) ->
    %% Name is a referenced structure that is not defined in ImpMod,
    %% but must be present in the Imports list of ImpMod. The chain of
    %% imports of Name must end in DefMod.
    NameOfDef =
	fun(#'Externaltypereference'{type=N}) -> N;
	   (#'Externalvaluereference'{value=N}) -> N;
	   (Other) -> Other
	end,
    GetImports =
	fun(_M_) ->
		case asn1_db:dbget(_M_,'MODULE') of
		    #module{imports={imports,ImportList}} ->
			ImportList;
		    _ -> []
		end
	end,
    FindNameInImports =
	fun([],N,_) -> {no_mod,N};
	   ([#'SymbolsFromModule'{symbols=Imports,module=ModuleRef}|SFMs],N,F) ->
		case [NameOfDef(X)||X <- Imports, NameOfDef(X) =:= N] of
		    [] -> F(SFMs,N,F);
		    [N] -> {NameOfDef(ModuleRef),N}
		end
	end,
    case GetImports(ImpMod) of
	[] ->
	    error;
	Imps ->
	    case FindNameInImports(Imps,Name,FindNameInImports) of
		{no_mod,_} ->
		    error;
		{DefMod,_} -> ok;
		{OtherMod,_} ->
		    chained_import(S,OtherMod,DefMod,Name)
	    end
    end.
			  
checkt(S,[Name|T],Acc) ->
    ?dbg("Checking type ~p~n",[Name]),
    Result = 
	case asn1_db:dbget(S#state.mname,Name) of
	    undefined ->
		error({type,{internal_error,'???'},S});
	    Type when is_record(Type,typedef) ->
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
    ?dbg("Checking valuedef ~p~n",[Name]),
    Result = case asn1_db:dbget(S#state.mname,Name) of
		 undefined -> error({value,{internal_error,'???'},S});
		 Value when is_record(Value,valuedef);
			    is_record(Value,typedef); %Value set may be parsed as object set.
			    is_record(Value,pvaluedef);
			    is_record(Value,pvaluesetdef) ->
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
			     ClassName = Type#type.def,
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
	Type when is_record(Type,ptypedef) ->
	    NewS = S#state{type=Type,tname=Name},
	    case catch(check_ptype(NewS,Type,Type#ptypedef.typespec)) of
		{error,Reason} ->
		    error({type,Reason,NewS});
		{'EXIT',Reason} ->
		    error({type,{internal_error,Reason},NewS});
		{asn1_class,_ClassDef} ->
		    {asn1_class,Name};
		{asn1_param_class,_} -> ok;
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
			       is_record(Class,classdef) ->
%				   Class#classdef.typespec;
				   Class;
			       is_record(Class,typedef) ->
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
				is_record(Class,classdef) ->
				    Class#classdef{checked=true,typespec=C};
				is_record(Class,typedef) ->
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
    ?dbg("Checking object ~p~n",[Name]),
    Result = 
	case asn1_db:dbget(S#state.mname,Name) of
	    undefined ->
		error({type,{internal_error,'???'},S});
	    Object when is_record(Object,typedef) ->
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
			    is_record(O,'Object') ->
				case O#'Object'.gen of
				    true ->
					{ok,ExclO,ExclOS};
				    false ->
					{ok,[Name|ExclO],ExclOS}
				end;
			    is_record(O,'ObjectSet') ->
				case O#'ObjectSet'.gen of
				    true ->
					{ok,ExclO,ExclOS};
				    false ->
					{ok,ExclO,[Name|ExclOS]}
				end
			end
		end;
	    PObject when is_record(PObject,pobjectdef) ->
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
	    PObjSet when is_record(PObjSet,pvaluesetdef) ->
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
	    case is_class(S,RefType) of
		true ->
		    NewState = update_state(S#state{type=RefType,
						    tname=TName},MName),
 		    check_class(NewState,get_class_def(S,RefType));
		_ ->
		    error({class,{internal_error,RefType},S})
	    end;
	{pt,ClassRef,Params} ->
	    %% parameterized class
	    {_,PClassDef} = get_referenced_type(S,ClassRef),
	    NewParaList = 
		[match_parameters(S,TmpParam,S#state.parameters)|| 
		    TmpParam <- Params],
	    instantiate_pclass(S,PClassDef,NewParaList)
    end;
check_class(S,C) when is_record(C,objectclass) ->
    NewFieldSpec = check_class_fields(S,C#objectclass.fields),
    C#objectclass{fields=NewFieldSpec};
check_class(_S,{poc,_ObjSet,_Params}) ->
    'fix this later';
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
%		    NewS = S#state{mname=RefMod,type=Def,tname=Name},
		    NewS = update_state(S#state{type=Def,tname=Name},RefMod),
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

instantiate_pclass(S=#state{parameters=_OldArgs},PClassDef,Params) ->
    #ptypedef{args=Args,typespec=Type} = PClassDef,
    MatchedArgs = match_args(S,Args, Params, []),
%    NewS = S#state{type=Type,parameters=MatchedArgs++OldArgs,abscomppath=[]},
    NewS = S#state{type=Type,parameters=MatchedArgs,abscomppath=[]},
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
		RefType = check_type(S,#typedef{typespec=Type},Type),
		{fixedtypevaluefield,Name,RefType,Unique,OSpec};
	    object_or_fixedtypevalue_field ->
		{_,Name,Type,Unique,OSpec} = F,
		Type2 = maybe_unchecked_OCFT(S,Type),
		Cat = 
		    case asn1ct_gen:type(asn1ct_gen:get_inner(Type2#type.def)) of
			Def when is_record(Def,typereference);
				 is_record(Def,'Externaltypereference') ->
			    {_,D} = get_referenced_type(S,Def),
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
		    case (catch check_type(S,#typedef{typespec=Type},Type)) of
			{asn1_class,_ClassDef} ->
			    case if_current_checked_type(S,Type) of
				true ->
				    Type#type.def;
				_ ->
				    check_class(S,Type)
			    end;
			CheckedType when is_record(CheckedType,type) ->
			    CheckedType;
			_ ->
			    error({class,"internal error, check_class_fields",S})
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

check_object(_S,ObjDef,ObjSpec) when (ObjDef#typedef.checked == true) ->
    ObjSpec;
check_object(S,_ObjDef,#'Object'{classname=ClassRef,def=ObjectDef}) ->
    ?dbg("check_object ~p~n",[ObjectDef]),
%%    io:format("check_object,object: ~p~n",[ObjectDef]),
%    {MName,_ClassDef} = get_referenced_type(S,ClassRef),
    NewClassRef = check_externaltypereference(S,ClassRef),
    ClassDef =
	case get_referenced_type(S,ClassRef) of
	    {MName,ClDef=#classdef{checked=false}} ->
		NewState = update_state(S#state{type=ClDef,
				   tname=ClassRef#'Externaltypereference'.type},MName),
		ObjClass= 
		    check_class(NewState,ClDef),
		#classdef{checked=true,
			  typespec=ObjClass};
	    {_,_ClDef} when is_record(_ClDef,classdef) ->		
		_ClDef;
	    {MName,_TDef=#typedef{checked=false,pos=Pos,
				   name=_TName,typespec=TS}} ->
		ClDef = #classdef{pos=Pos,name=_TName,typespec=TS},
		NewState = update_state(S#state{type=_TDef,
			tname=ClassRef#'Externaltypereference'.type},MName),	   
		ObjClass =
		    check_class(NewState,ClDef),
		ClDef#classdef{checked=true,typespec=ObjClass};
	    {_,_ClDef} ->
		_ClDef
	end,
    NewObj =
	case ObjectDef of
	    Def when is_tuple(Def), (element(1,Def)==object) ->
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
		check_object(S,Object,Object#typedef.typespec);
	    [] -> 
		%% An object with no fields. All class fields must be
		%% optional or default.  Check that all fields in
		%% class are 'OPTIONAL' or 'DEFAULT'
		class_fields_optional_check(S,ClassDef),
		#'Object'{def={object,defaultsyntax,[]}};
	    _  ->
		exit({error,{no_object,ObjectDef},S})
	end,
    Gen = gen_incl(S,NewObj#'Object'.def,
		   (ClassDef#classdef.typespec)#objectclass.fields),
    NewObj#'Object'{classname=NewClassRef,gen=Gen};


check_object(S,
	     _ObjSetDef,
	     ObjSet=#'ObjectSet'{class=ClassRef}) ->
%%    io:format("check_object,SET: ~p~n",[ObjSet#'ObjectSet'.set]),
    ?dbg("check_object set: ~p~n",[ObjSet#'ObjectSet'.set]),
    {_,ClassDef} = get_referenced_type(S,ClassRef),
    NewClassRef = check_externaltypereference(S,ClassRef),
    %% XXXXXXXXXX
    case ObjSet of
	#'ObjectSet'{set={'Externaltypereference',undefined,'MSAccessProtocol',
                     'AllOperations'}} ->
	    ok;
	_ ->
	    ok
    end,
    {UniqueFieldName,UniqueInfo} = 
	case (catch get_unique_fieldname(S,ClassDef)) of
	    {error,'__undefined_',_} -> 
		{{unique,undefined},{unique,undefined}};
	    {asn1,Msg,_} -> error({class,Msg,S});
	    {'EXIT',Msg} -> error({class,{internal_error,Msg},S});
	    Other -> {element(1,Other),Other}
	end,
    NewObjSet=
	case prepare_objset(ObjSet#'ObjectSet'.set) of
	    {set,SET,EXT} ->
		CheckedSet = check_object_list(S,NewClassRef,SET),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueInfo),
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=extensionmark(NewSet,EXT)};

	    {'SingleValue',ERef = #'Externalvaluereference'{}} ->
		{RefedMod,ObjDef} = get_referenced_type(S,ERef),
		#'Object'{def=CheckedObj} = 
		    check_object(S,ObjDef,ObjDef#typedef.typespec),

		NewSet = get_unique_valuelist(S,[{{RefedMod,get_datastr_name(ObjDef)},
						  CheckedObj}],
					      UniqueInfo),
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=NewSet};
	    ['EXTENSIONMARK'] ->
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=['EXTENSIONMARK']};

	    OSref when is_record(OSref,'Externaltypereference') ->
		{_,OS=#typedef{typespec=OSdef}} = get_referenced_type(S,OSref),
		check_object(S,OS,OSdef);

	    {Type,{'EXCEPT',Exclusion}} when is_record(Type,type) ->
		{_,TDef} = get_referenced_type(S,Type#type.def),
		OS = TDef#typedef.typespec,
		NewSet = reduce_objectset(OS#'ObjectSet'.set,Exclusion),
		NewOS = OS#'ObjectSet'{set=NewSet},
		check_object(S,TDef#typedef{typespec=NewOS},
			     NewOS);
	    #type{def={pt,DefinedObjSet,ParamList}} ->
		{_,PObjSetDef} = get_referenced_type(S,DefinedObjSet),
		NewParamList = 
		    [match_parameters(S,TmpParam,S#state.parameters)|| 
			TmpParam <- ParamList],
		instantiate_pos(S,ClassRef,PObjSetDef,NewParamList);

	    %% actually this is an ObjectSetFromObjects construct, it
	    %% is when the object set is retrieved from an object
	    %% field.
	    #type{def=#'ObjectClassFieldType'{classname=ObjName,
					      fieldname=FieldName}} ->
		{RefedObjMod,TDef} = get_referenced_type(S,ObjName),
		OS=TDef#typedef.typespec,
		%% should get the right object set here. Get the field
		%% FieldName out of the object set OS of class
		%% OS#'ObjectSet'.class
		OS2=check_object(S,TDef,OS),
		NewSet=object_set_from_objects(S,RefedObjMod,FieldName,OS2),
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=NewSet};
	    {'ObjectSetFromObjects',{_,_,ObjName},FieldName} ->
		{RefedObjMod,TDef} = get_referenced_type(S,ObjName),
		OS=TDef#typedef.typespec,
		%% should get the right object set here. Get the field
		%% FieldName out of the object set OS of class
		%% OS#'ObjectSet'.class
		OS2=check_object(S,TDef,OS),
		NewSet=object_set_from_objects(S,RefedObjMod,FieldName,OS2),
		ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=NewSet};
	     {'ObjectSetFromObjects',{_,ObjName},FieldName} ->
		%% This is a ObjectSetFromObjects, i.e.
		%% ObjectSetFromObjects ::= ReferencedObjects "." FieldName
		%% with a defined object as ReferencedObjects. And
		%% the FieldName of the Class (object) contains an object set.
		{RefedObjMod,TDef} = get_referenced_type(S,ObjName),
		O1 = TDef#typedef.typespec,
		O2 = check_object(S,TDef,O1),
		NewSet = object_set_from_objects(S,RefedObjMod,FieldName,O2),
		OS2=ObjSet#'ObjectSet'{uniquefname=UniqueFieldName,
				   set=NewSet},
		%%io:format("ObjectSet: ~p~n",[OS2]),
		OS2;
	    {pos,{objectset,_,DefinedObjSet},Params} ->
		{_,PObjSetDef} = get_referenced_type(S,DefinedObjSet),
		NewParamList = 
		    [match_parameters(S,TmpParam,S#state.parameters)|| 
			TmpParam <- Params],
		instantiate_pos(S,ClassRef,PObjSetDef,NewParamList);
	    Unknown ->
		exit({error,{unknown_object_set,Unknown},S})
	end,
    NewSet2 = remove_duplicate_objects(NewObjSet#'ObjectSet'.set),
    NewObjSet2 = NewObjSet#'ObjectSet'{set=NewSet2},
    Gen = gen_incl_set(S,NewObjSet2#'ObjectSet'.set,
		       ClassDef),
    ?dbg("check_object done~n",[]),
    NewObjSet2#'ObjectSet'{class=NewClassRef,gen=Gen}.

%% remove_duplicate_objects/1 remove duplicates of objects.
%% For instance may Set contain objects of same class from
%% different object sets that in fact might be duplicates.
remove_duplicate_objects(Set) when is_list(Set) ->
    Pred = fun({A,B,_},{A,C,_}) when B =< C -> true;
	      ({A,_,_},{B,_,_}) when A < B -> true;
	      ('EXTENSIONMARK','EXTENSIONMARK') -> true;
	      (T,A) when is_tuple(T),is_atom(A) -> true;% EXTENSIONMARK last in list
	      (_,_) -> false 
	   end,
    lists:usort(Pred,Set).

%%
extensionmark(L,true) ->
    case lists:member('EXTENSIONMARK',L) of
	true -> L;
	_ -> L ++ ['EXTENSIONMARK']
    end;
extensionmark(L,_) ->
    L.

object_to_check(#typedef{typespec=ObjDef}) ->
    ObjDef;
object_to_check(#valuedef{type=ClassName,value=ObjectRef}) ->
    %% If the object definition is parsed as an object the ClassName
    %% is parsed as a type
    #'Object'{classname=ClassName#type.def,def=ObjectRef}.

prepare_objset({'SingleValue',Set}) when is_list(Set) ->
    {set,Set,false};
prepare_objset(L=['EXTENSIONMARK']) ->
    L;
prepare_objset(Set) when is_list(Set) ->
    {set,Set,false};
prepare_objset({{'SingleValue',Set},Ext}) ->
    {set,merge_sets(Set,Ext),true};
%%prepare_objset({Set,Ext}) when is_list(Set),is_list(Ext) ->
%%    {set,lists:append([Set,Ext]),true};
prepare_objset({Set,Ext}) when is_list(Set) ->
    {set,merge_sets(Set,Ext),true};
prepare_objset({ObjDef={object,definedsyntax,_ObjFields},_Ext}) ->
    {set,[ObjDef],true};
prepare_objset(ObjDef={object,definedsyntax,_ObjFields}) ->
    {set,[ObjDef],false};
prepare_objset({ObjDef=#type{},Ext}) when is_list(Ext) ->
    {set,[ObjDef|Ext],true};
prepare_objset(Ret) ->
    Ret.

class_fields_optional_check(S,#classdef{typespec=ClassSpec}) ->
    Fields = ClassSpec#objectclass.fields,
    class_fields_optional_check1(S,Fields).

class_fields_optional_check1(_S,[]) ->
    ok;
class_fields_optional_check1(S,[{typefield,_,'OPTIONAL'}|Rest]) ->
    class_fields_optional_check1(S,Rest);
class_fields_optional_check1(S,[{fixedtypevaluefield,_,_,_,'OPTIONAL'}|Rest]) ->
    class_fields_optional_check1(S,Rest);
class_fields_optional_check1(S,[{fixedtypevaluesetfield,_,_,'OPTIONAL'}|Rest]) ->
    class_fields_optional_check1(S,Rest);
class_fields_optional_check1(S,[{objectfield,_,_,_,'OPTIONAL'}|Rest]) ->
    class_fields_optional_check1(S,Rest);
class_fields_optional_check1(S,[{objectsetfield,_,_,'OPTIONAL'}|Rest]) ->
    class_fields_optional_check1(S,Rest).

%% ObjectSetFromObjects functionality

%% The fieldname is a list of field names.They may be objects or
%% object sets. If ObjectSet is an object set the resulting object set
%% is the union of object sets if the last field name is an object
%% set.  If the last field is an object the resulting object set is
%% the set of objects in ObjectSet.
object_set_from_objects(S,RefedObjMod,FieldName,ObjectSet) ->
    object_set_from_objects(S,RefedObjMod,FieldName,ObjectSet,[]).
object_set_from_objects(S,RefedObjMod,FieldName,ObjectSet,InterSect)
  when is_record(ObjectSet,'ObjectSet') ->
    #'ObjectSet'{class=Cl,set=Set} = ObjectSet,
    {_,ClassDef} = get_referenced_type(S,Cl),
    object_set_from_objects(S,RefedObjMod,ClassDef,FieldName,Set,InterSect,[]);
object_set_from_objects(S,RefedObjMod,FieldName,Object,InterSect) 
  when is_record(Object,'Object') ->
    #'Object'{classname=Cl,def=Def}=Object,
    object_set_from_objects(S,RefedObjMod,Cl,FieldName,[Def],InterSect,[]).
object_set_from_objects(S,RefedObjMod,ClassDef,FieldName,['EXTENSIONMARK'|Os],
			InterSect,Acc) ->
    object_set_from_objects(S,RefedObjMod,ClassDef,FieldName,Os,InterSect,%%Acc);
			    ['EXTENSIONMARK'|Acc]);
object_set_from_objects(S,RefedObjMod,ClassDef,FieldName,[O|Os],InterSect,Acc) ->
    case object_set_from_objects2(S,mod_of_obj(RefedObjMod,element(1,O)),
				  ClassDef,FieldName,element(3,O),InterSect) of
	ObjS when is_list(ObjS) ->
	    object_set_from_objects(S,RefedObjMod,ClassDef,FieldName,Os,InterSect,ObjS++Acc);
	Obj ->
	    object_set_from_objects(S,RefedObjMod,ClassDef,FieldName,Os,InterSect,[Obj|Acc])
    end;
object_set_from_objects(_S,_RefedObjMod,_ClassDef,_FieldName,[],InterSect,Acc) ->
    %% For instance may Acc contain objects of same class from
    %% different object sets that in fact might be duplicates.
    remove_duplicate_objects(osfo_intersection(InterSect,Acc)).
%%    Acc.
object_set_from_objects2(S,RefedObjMod,ClassDef,[{valuefieldreference,OName}],
			 Fields,_InterSect) ->
    %% this is an object
    case lists:keysearch(OName,1,Fields) of
	{value,{_,TDef}} ->
	    mk_object_set_from_object(S,RefedObjMod,TDef,ClassDef);
	_ ->
	    [] % it may be an absent optional field
    end;
object_set_from_objects2(S,RefedObjMod,ClassDef,[{typefieldreference,OSName}],
			Fields,_InterSect) ->
    %% this is an object set
    case lists:keysearch(OSName,1,Fields) of
	{value,{_,TDef}} ->
	    case TDef#typedef.typespec of
		#'ObjectSet'{class=_NextClName,set=NextSet} ->%% = TDef#typedef.typespec,
		    NextSet;
		#'Object'{def=_ObjDef} ->
		    mk_object_set_from_object(S,RefedObjMod,TDef,ClassDef)
%%		    ObjDef
		    %% error({error,{internal,unexpected_object,TDef}})
	    end;
	_ ->
	    [] % it may be an absent optional field
    end;
object_set_from_objects2(S,RefedObjMod,_ClassDef,[{valuefieldreference,OName}|Rest],
			 Fields,InterSect) ->
    %% this is an object
    case lists:keysearch(OName,1,Fields) of
	{value,{_,TDef}} ->
	    #'Object'{classname=NextClName,def=ODef}=TDef#typedef.typespec,
	    {_,_,NextFields}=ODef,
	    {_,NextClass} = get_referenced_type(S,NextClName),
	    object_set_from_objects2(S,RefedObjMod,NextClass,Rest,NextFields,InterSect);
	_ ->
	    []
    end;
object_set_from_objects2(S,RefedObjMod,_ClassDef,[{typefieldreference,OSName}|Rest],
			Fields,InterSect) ->
    %% this is an object set
    Next = {NextClName,NextSet} = 
	case lists:keysearch(OSName,1,Fields) of
	    {value,{_,TDef}} when is_record(TDef,'ObjectSet') ->    
		#'ObjectSet'{class=NextClN,set=NextS} = TDef,
		{NextClN,NextS};
	    {value,{_,#typedef{typespec=OS}}} ->
		%% objectsets in defined syntax will come here as typedef{}
		%% #'ObjectSet'{class=NextClN,set=NextS} = OS,
		case OS of
		    #'ObjectSet'{class=NextClN,set=NextS} ->
			{NextClN,NextS};
		    #'Object'{classname=NextClN,def=NextDef} ->
			{NextClN,[NextDef]}
		end;
	_ ->
	    {[],[]}
    end,
    case Next of
	{[],[]} ->
	    [];
	_ ->
	    {_,NextClass} = get_referenced_type(S,NextClName),
	    object_set_from_objects(S,RefedObjMod,NextClass,Rest,NextSet,InterSect,[])
    end.
		
mk_object_set_from_object(S,RefedObjMod,TDef,Class) -> 
    #'Object'{classname=_NextClName,def=ODef} = TDef#typedef.typespec,
    {_,_,NextFields}=ODef,

    UniqueFieldName = 
	case (catch get_unique_fieldname(S,Class)) of
	    {error,'__undefined_',_} -> {unique,undefined};
	    {asn1,Msg,_} -> error({class,Msg,S});
	    {'EXIT',Msg} -> error({class,{internal_error,Msg},S});
	    {Other,_} -> Other
	end,
    VDef = get_unique_value(S,NextFields,UniqueFieldName),
    %% XXXXXXXXXXX
    case VDef of
	[] ->
	    ['EXTENSIONMARK'];
	_ ->
	    {{RefedObjMod,get_datastr_name(TDef)},VDef,NextFields}    
    end.
    
    
mod_of_obj(_RefedObjMod,{NewMod,ObjName}) 
  when is_atom(NewMod),is_atom(ObjName) ->
    NewMod;
mod_of_obj(RefedObjMod,_) ->
    RefedObjMod.
    

merge_sets(Root,{'SingleValue',Ext}) ->
    merge_sets(Root,Ext);
merge_sets(Root,Ext) when is_list(Root),is_list(Ext) ->
    Root ++ Ext;
merge_sets(Root,Ext) when is_list(Ext) ->
    [Root|Ext];
merge_sets(Root,Ext) when is_list(Root) ->
    Root++[Ext];
merge_sets(Root,Ext) ->
    [Root]++[Ext].

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
    ?dbg("check_object_list: ~p~n",[ObjOrSet]),
    case ObjOrSet of
	ObjDef when is_tuple(ObjDef),(element(1,ObjDef)==object) ->
	    Def = 
		check_object(S,#typedef{typespec=ObjDef},
%			     #'Object'{classname={objectclassname,ClassRef},
			     #'Object'{classname=ClassRef,
				       def=ObjDef}),
	    check_object_list(S,ClassRef,Objs,[{{no_mod,no_name},Def#'Object'.def}|Acc]);
	{'SingleValue',Ref = #'Externalvaluereference'{}} ->
	    ?dbg("{SingleValue,Externalvaluereference}~n",[]),
	    {RefedMod,ObjName,
	     #'Object'{def=Def}} = check_referenced_object(S,Ref),
	    check_object_list(S,ClassRef,Objs,[{{RefedMod,ObjName},Def}|Acc]);
	ObjRef when is_record(ObjRef,'Externalvaluereference') ->
	    ?dbg("Externalvaluereference~n",[]),
	    {RefedMod,ObjName,
	     #'Object'{def=Def}} = check_referenced_object(S,ObjRef),
	     check_object_list(S,ClassRef,Objs,[{{RefedMod,ObjName},Def}|Acc]);
	{'ValueFromObject',{_,Object},FieldName} ->
	    {_,Def} = get_referenced_type(S,Object),
	    TypeDef = get_fieldname_element(S,Def,FieldName),
	    (TypeDef#typedef.typespec)#'ObjectSet'.set;
	ObjSet when is_record(ObjSet,type) ->
	    ObjSetDef = 
		case ObjSet#type.def of
		    Ref when is_record(Ref,typereference);
			     is_record(Ref,'Externaltypereference') ->
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
	{pos,{objectset,_,DefinedObjectSet},Params} ->
	    OSDef = #type{def={pt,DefinedObjectSet,Params}},
	    #'ObjectSet'{set=Set} =
		check_object(S,ObjOrSet,#'ObjectSet'{class=ClassRef,
						     set=OSDef}),
	    check_object_list(S,ClassRef,Objs,Set ++ Acc);
	{pv,{simpledefinedvalue,DefinedObject},Params} ->
	    Args = [match_parameters(S,Param,S#state.parameters)||
		       Param<-Params],
	    #'Object'{def=Def} =
		check_object(S,ObjOrSet,
			     #'Object'{classname=ClassRef ,
				       def={po,{object,DefinedObject},
					    Args}}),
	    check_object_list(S,ClassRef,Objs,[{{no_mod,no_name},Def}|Acc]);
	{'ObjectSetFromObjects',Os,FieldName} when is_tuple(Os) ->
	    NewSet =
		check_ObjectSetFromObjects(S,element(size(Os),Os),
					   FieldName,[]),
	    check_object_list(S,ClassRef,Objs,NewSet++Acc);
	{{'ObjectSetFromObjects',Os,FieldName},InterSection}
	when is_tuple(Os) ->
	    NewSet =
		check_ObjectSetFromObjects(S, element(size(Os),Os),
					   FieldName,InterSection),
	    check_object_list(S,ClassRef,Objs,NewSet++Acc);
	Other ->
	    exit({error,{'unknown object',Other},S})
    end;
%% Finally reverse the accumulated list and if there are any extension
%% marks in the object set put one indicator of that in the end of the
%% list.
check_object_list(_,_,[],Acc) ->
    lists:reverse(Acc).

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

check_ObjectSetFromObjects(S,ObjName,FieldName,InterSection) ->
    {RefedMod,TDef} = get_referenced_type(S,ObjName),
    ObjOrSet = check_object(update_state(S,RefedMod),TDef,TDef#typedef.typespec),
    InterSec = prepare_intersection(S,InterSection),
    _NewSet = object_set_from_objects(S,RefedMod,FieldName,ObjOrSet,InterSec).

prepare_intersection(_S,[]) ->
    [];
prepare_intersection(S,{'EXCEPT',ObjRef}) ->
    except_names(S,ObjRef);
prepare_intersection(_S,T) ->
    exit({error,{internal_error,not_implemented,object_set_from_objects,T}}).
except_names(_S,{'SingleValue',#'Externalvaluereference'{value=ObjName}})  ->
    [{except,ObjName}];
except_names(_,T) ->
    exit({error,{internal_error,not_implemented,object_set_from_objects,T}}).

osfo_intersection(InterSect,ObjList) ->
    Res = [X|| X = {{_,N},_,_} <- ObjList,
	       lists:member({except,N},InterSect) == false],
    case lists:member('EXTENSIONMARK',ObjList) of
	true ->
	    Res ++ ['EXTENSIONMARK'];
	_ ->
	    Res
    end.

%%  get_fieldname_element/3
%%  gets the type/value/object/... of the referenced element in FieldName
%%  FieldName is a list and may have more than one element.
%%  Each element in FieldName can be either {typefieldreference,AnyFieldName}
%%  or {valuefieldreference,AnyFieldName}
%%  Def is the def of the first object referenced by FieldName
get_fieldname_element(S,Def,[{_RefType,FieldName}]) when is_record(Def,typedef) ->
    {_,_,ObjComps} = (Def#typedef.typespec)#'Object'.def,
    check_fieldname_element(S,lists:keysearch(FieldName,1,ObjComps));
get_fieldname_element(S,Def,[{_RefType,FieldName}|Rest])
  when is_record(Def,typedef) ->
    %% As FieldName is followd by other FieldNames it has to be an
    %% object or objectset.
    {_,_,ObjComps} = (Def#typedef.typespec)#'Object'.def,
    NewDef = check_fieldname_element(S,lists:keysearch(FieldName,1,ObjComps)),
    ObjDef = fun(#'Object'{def=D}) -> D;
		(#'ObjectSet'{set=Set}) -> Set
	     end 
	       (NewDef),
    case ObjDef of
	L when is_list(L) ->
	    [get_fieldname_element(S,X,Rest) || X <- L];
	_ ->
	    get_fieldname_element(S,ObjDef,Rest)
    end;
get_fieldname_element(S,{object,_,Fields},[{_RefType,FieldName}|Rest]) ->
    NewDef = check_fieldname_element(S,lists:keysearch(FieldName,1,Fields)),
    get_fieldname_element(S,NewDef,Rest);
get_fieldname_element(_S,Def,[]) -> 
    Def;
get_fieldname_element(_S,Def,[{_RefType,_FieldName}|_RestFName]) 
  when is_record(Def,typedef) ->
    ok.

check_fieldname_element(S,{value,{_,Def}}) ->
    check_fieldname_element(S,Def);
check_fieldname_element(S,TDef)  when is_record(TDef,typedef) ->
    check_type(S,TDef,TDef#typedef.typespec);
check_fieldname_element(S,VDef) when is_record(VDef,valuedef) ->
    check_value(S,VDef);
check_fieldname_element(S,Eref)
  when is_record(Eref,'Externaltypereference');
       is_record(Eref,'Externalvaluereference') ->
    {_,TDef}=get_referenced_type(S,Eref),
    check_fieldname_element(S,TDef);
check_fieldname_element(S,Other) ->
    throw({error,{assigned_object_error,"not_assigned_object",Other,S}}).
    
transform_set_to_object_list([{Name,_UVal,Fields}|Objs],Acc) ->
    transform_set_to_object_list(Objs,[{Name,{object,generatesyntax,Fields}}|Acc]);
transform_set_to_object_list(['EXTENSIONMARK'|Objs],Acc) ->
%%    transform_set_to_object_list(Objs,['EXTENSIONMARK'|Acc]);
    transform_set_to_object_list(Objs,Acc);
transform_set_to_object_list([],Acc) ->
    Acc.

get_unique_valuelist(_S,ObjSet,{unique,undefined}) -> % no unique field in object
    lists:map(fun({N,{_,_,F}})->{N,no_unique_value,F};
		 (V={_,_,_}) ->V;
		 ({A,B}) -> {A,no_unique_value,B} 
	      end, ObjSet);
get_unique_valuelist(S,ObjSet,{UFN,Opt}) ->
    get_unique_vlist(S,ObjSet,UFN,Opt,[]).


get_unique_vlist(_S,[],_,_,[]) ->
    ['EXTENSIONMARK'];
get_unique_vlist(S,[],_,Opt,Acc) ->
    case catch check_uniqueness(remove_duplicate_objects(Acc)) of
	{asn1_error,_} when Opt =/= 'OPTIONAL' ->
	    error({'ObjectSet',"not unique objects in object set",S});
	{asn1_error,_} ->
	    lists:reverse(Acc);
	_ ->
	    lists:reverse(Acc)
    end;
get_unique_vlist(S,['EXTENSIONMARK'|Rest],UniqueFieldName,Opt,Acc) ->
    get_unique_vlist(S,Rest,UniqueFieldName,Opt,Acc);
get_unique_vlist(S,[{ObjName,Obj}|Rest],UniqueFieldName,Opt,Acc) ->
    {_,_,Fields} = Obj,
    NewObjInf = 
	case get_unique_value(S,Fields,UniqueFieldName) of
	    #valuedef{value=V} -> [{ObjName,V,Fields}];
	    [] -> []; % maybe the object only was a reference to an
                     % empty object set.
	    no_unique_value -> [{ObjName,no_unique_value,Fields}]
	end,
    get_unique_vlist(S,Rest,UniqueFieldName,Opt,NewObjInf++Acc);

get_unique_vlist(S,[V={_,_,_}|Rest],UniqueFieldName,Opt,Acc) ->
    get_unique_vlist(S,Rest,UniqueFieldName,Opt,[V|Acc]).

get_unique_value(S,Fields,UniqueFieldName) ->
    Module = S#state.mname,
    case lists:keysearch(UniqueFieldName,1,Fields) of
	{value,Field} ->
	    case element(2,Field) of
		VDef when is_record(VDef,valuedef) ->
		    VDef;
		{'ValueFromObject',Object,Name} ->
		    case Object of
			{object,Ext} when is_record(Ext,'Externaltypereference') ->
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
		Value when is_atom(Value);is_number(Value) ->
		    #valuedef{value=Value,module=Module};
		{'CHOICE',{C,Value}} when is_atom(C) ->
		    %% #valuedef{value=normalize_value(S,element(3,Field),VDef,[])}
		    case Value of
			Scalar when is_atom(Scalar);is_number(Scalar) ->
			    #valuedef{value=Value,module=Module};
			Eref = #'Externalvaluereference'{} ->
			    element(2,get_referenced_type(S,Eref))
		    end
	    end;
	false ->
	    case Fields of
		[{_,#typedef{typespec=#'ObjectSet'{set=['EXTENSIONMARK']}}}] -> 
		    [];
		_ ->
		    no_unique_value
	    end
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
instantiate_po(S=#state{parameters=_OldArgs},_ClassDef,Object,ArgsList) when is_record(Object,pobjectdef) ->
    FormalParams = get_pt_args(Object),
    MatchedArgs = match_args(S,FormalParams,ArgsList,[]),
%    NewS = S#state{type=Object,parameters=MatchedArgs++OldArgs},
    NewS = S#state{type=Object,parameters=MatchedArgs},
    check_object(NewS,Object,#'Object'{classname=Object#pobjectdef.class,
				    def=Object#pobjectdef.def}).

%% instantiate_pos/4
%% ClassDef is the class of ObjectSetDef,
%% ObjectSetDef is the Parameterized object set, which is referenced 
%% on the right side of the assignment,
%% ArgsList is the list of actual parameters, i.e. real objects
instantiate_pos(S=#state{parameters=_OldArgs},ClassRef,ObjectSetDef,ArgsList) ->
%    ClassName = ClassDef#classdef.name,
    FormalParams = get_pt_args(ObjectSetDef),
    OSet = case get_pt_spec(ObjectSetDef) of
	      {valueset,Set} -> 
% 		   #'ObjectSet'{class=name2Extref(S#state.mname,
% 						  ClassName),set=Set};
		   #'ObjectSet'{class=ClassRef,set=Set};
	      Set when is_record(Set,'ObjectSet') -> Set;
	       _ ->
		   error({type,"parameterized object set failure",S})
	   end,
    MatchedArgs = match_args(S,FormalParams,ArgsList,[]),
%    NewS = S#state{type=ObjectSetDef,parameters=MatchedArgs++OldArgs},
    NewS = S#state{type=ObjectSetDef,parameters=MatchedArgs},
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
				check_object(S,T,object_to_check(T))
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
    {_,ClassDef} = get_referenced_type(S,Eref),
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
    case catch get_unique_fieldname(S,ClassDef) of
	Tuple when is_tuple(Tuple), size(Tuple) =:= 3 ->
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
gen_incl_set1(_,['EXTENSIONMARK'|_],_) ->
    true;
gen_incl_set1(S,[Object|Rest],CFields)->
    Fields = element(size(Object),Object),
    case gen_incl1(S,Fields,CFields) of
	true ->
	    true;
	false ->
	    gen_incl_set1(S,Rest,CFields)
    end.

check_objectdefn(S,Def,CDef) when is_record(CDef,classdef) ->
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
		    DefaultFields when is_list(DefaultFields) ->
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
	    {NewField,RestFields} = 
		convert_to_defaultfield(S,FName,[Spec|Fields],CField),
	    check_defaultfields(S,RestFields,ClassFields,[NewField|Acc]);
	_ ->
	    throw({error,{asn1,{'unvalid field in object',FName}}})
    end.
%%    {object,defaultsyntax,Fields}.

convert_definedsyntax(_S,[],[],_ClassFields,Acc) ->
    lists:reverse(Acc);
convert_definedsyntax(S,Fields,WithSyntax,ClassFields,Acc) ->
    {MatchedField,RestFields,RestWS} =
	match_field(S,Fields,WithSyntax,ClassFields),
    if
	is_list(MatchedField) ->
	    convert_definedsyntax(S,RestFields,RestWS,ClassFields,
				  lists:append(MatchedField,Acc));
	true ->
	    convert_definedsyntax(S,RestFields,RestWS,ClassFields,
				  [MatchedField|Acc])
    end.

match_field(S,Fields,WithSyntax,ClassFields) ->
    match_field(S,Fields,WithSyntax,ClassFields,[]).

match_field(S,Fields,[W|Ws],ClassFields,Acc) when is_list(W) ->
    case catch(match_optional_field(S,Fields,W,ClassFields,[])) of
	{'EXIT',_} ->
	    match_field(Fields,Ws,ClassFields,Acc); %% add S
%%	{[Result],RestFields} ->
%%	    {Result,RestFields,Ws};
	{Result,RestFields} when is_list(Result) ->
	    {Result,RestFields,Ws};
	_ ->
	    match_field(S,Fields,Ws,ClassFields,Acc)
    end;
match_field(S,Fields,WithSyntax,ClassFields,_Acc) ->
    match_mandatory_field(S,Fields,WithSyntax,ClassFields,[]).

match_optional_field(_S,RestFields,[],_,Ret) ->
    {Ret,RestFields};
%% An additional optional field within an optional field
match_optional_field(S,Fields,[W|Ws],ClassFields,Ret) when is_list(W) ->
    case catch match_optional_field(S,Fields,W,ClassFields,[]) of
	{'EXIT',_} when length(Ws) > 0 ->
	    match_optional_field(S,Fields,Ws,ClassFields,Ret);
	{'EXIT',_} ->
	    {Ret,Fields};
	{asn1,{optional_matcherror,_,_}} when length(Ws) > 0 ->
	    match_optional_field(S,Fields,Ws,ClassFields,Ret);
	{asn1,{optional_matcherror,_,_}} ->
	    {Ret,Fields};
	{OptionalField,RestFields} ->
	    match_optional_field(S,RestFields,Ws,ClassFields,
				 lists:append(OptionalField,Ret))
    end;
%% identify and skip word
match_optional_field(S,[{_,_,#'Externaltypereference'{type=WorS}}|Rest],
		     [WorS|Ws],ClassFields,Ret) ->
    match_optional_field(S,Rest,Ws,ClassFields,Ret);
match_optional_field(S,[],_,ClassFields,Ret) ->
    match_optional_field(S,[],[],ClassFields,Ret);
%% identify and skip comma
match_optional_field(S,[{WorS,_}|Rest],[{WorS,_}|Ws],ClassFields,Ret) ->
    match_optional_field(S,Rest,Ws,ClassFields,Ret);
%% am optional setting inside another optional setting may be "double-listed"
match_optional_field(S,[Setting],DefinedSyntax,ClassFields,Ret) 
  when is_list(Setting) ->
    match_optional_field(S,Setting,DefinedSyntax,ClassFields,Ret);
%% identify and save field data
match_optional_field(S,[Setting|Rest],[{_,W}|Ws],ClassFields,Ret) ->
    ?dbg("matching optional field setting: ~p with user friendly syntax: ~p~n",[Setting,W]),
    WorS =
	case Setting of
	    Type when is_record(Type,type) -> Type;
	    {'ValueFromObject',_,_} -> Setting;
	    {object,_,_} -> Setting;
	    {_,_,WordOrSetting} -> WordOrSetting;
	    Other -> Other
	end,
    case lists:keysearch(W,2,ClassFields) of
	false ->
	    throw({asn1,{optional_matcherror,WorS,W}});
	{value,CField} ->
	    {NewField,RestFields} = 
		convert_to_defaultfield(S,W,[WorS|Rest],CField),
	    match_optional_field(S,RestFields,Ws,ClassFields,[NewField|Ret])
    end;
match_optional_field(_S,[WorS|_Rest],[W|_Ws],_ClassFields,_Ret) ->
    throw({asn1,{optional_matcherror,WorS,W}}).

match_mandatory_field(_S,[],[],_,[Acc]) ->
    {Acc,[],[]};
match_mandatory_field(_S,[],[],_,Acc) ->
    {Acc,[],[]};
match_mandatory_field(S,[],[H|T],CF,Acc) when is_list(H) ->
    match_mandatory_field(S,[],T,CF,Acc);
match_mandatory_field(_S,[],WithSyntax,_,_Acc) ->
    throw({asn1,{mandatory_matcherror,[],WithSyntax}});
%match_mandatory_field(_S,Fields,WithSyntax=[W|_Ws],_ClassFields,[Acc]) when is_list(W) ->
match_mandatory_field(_S,Fields,WithSyntax=[W|_Ws],_ClassFields,Acc) when is_list(W), length(Acc) >= 1 ->
    {Acc,Fields,WithSyntax};
%% identify and skip word
%%match_mandatory_field(S,[{_,_,WorS}|Rest],
match_mandatory_field(S,[{_,_,#'Externaltypereference'{type=WorS}}|Rest],
		      [WorS|Ws],ClassFields,Acc) ->
    match_mandatory_field(S,Rest,Ws,ClassFields,Acc);
%% identify and skip comma
match_mandatory_field(S,[{WorS,_}|Rest],[{WorS,_}|Ws],ClassFields,Ret) ->
    match_mandatory_field(S,Rest,Ws,ClassFields,Ret);
%% identify and save field data
match_mandatory_field(S,[Setting|Rest],[{_,W}|Ws],ClassFields,Acc) ->
    ?dbg("matching field setting: ~p with user friendly syntax: ~p~n",[Setting,W]),
    WorS = 
	case Setting of
	    {object,_,_} -> Setting;
	    {_,_,WordOrSetting} -> WordOrSetting;
	    Type when is_record(Type,type) -> Type;
	    Other -> Other
	end,
    case lists:keysearch(W,2,ClassFields) of
	false ->
	    throw({asn1,{mandatory_matcherror,WorS,W}});
	{value,CField} ->
	    {NewField,RestFields} = 
		convert_to_defaultfield(S,W,[WorS|Rest],CField),
	    match_mandatory_field(S,RestFields,Ws,ClassFields,[NewField|Acc])
    end;
 	    
match_mandatory_field(_S,[WorS|_Rest],[W|_Ws],_ClassFields,_Acc) ->
    throw({asn1,{mandatory_matcherror,WorS,W}}).

%% Converts a field of an object from defined syntax to default syntax
%% A field may be a type, a fixed type value, an object, an objectset,
%% 
convert_to_defaultfield(S,ObjFieldName,[OFS|RestOFS],CField)->
    ?dbg("convert field: ~p of type: ~p~n",[ObjFieldName,element(1,CField)]),
    CurrMod = S#state.mname,
    Strip_value_tag = 
	fun({value_tag,ValueSetting}) -> ValueSetting;
	   (VS) -> VS
	end,
    ObjFieldSetting = Strip_value_tag(OFS),
    RestSettings = [Strip_value_tag(X)||X <- RestOFS],
    case element(1,CField) of
	typefield ->
	    TypeDef=
		case ObjFieldSetting of
		    TypeRec when is_record(TypeRec,type) -> TypeRec#type.def;
		    TDef when is_record(TDef,typedef) -> 
			TDef#typedef{checked=true,
				     typespec=check_type(S,TDef,
							 TDef#typedef.typespec)};
		    _ -> ObjFieldSetting
		end,
	    {Type,SettingsLeft} = 
		if
		    is_record(TypeDef,typedef) -> {TypeDef,RestSettings};
		    is_record(TypeDef,'ObjectClassFieldType') ->
			T=check_type(S,#typedef{typespec=ObjFieldSetting},ObjFieldSetting),
			{oCFT_def(S,T),RestSettings};
%			#typedef{checked=true,name=Name,typespec=IT};
		    is_tuple(TypeDef), element(1,TypeDef) == pt  ->
			%% this is an inlined type. If constructed
			%% type save in data base
			T=check_type(S,#typedef{typespec=ObjFieldSetting},ObjFieldSetting),
			#'Externaltypereference'{type=PtName} = 
			    element(2,TypeDef),
			NameList = [PtName,S#state.tname],
			NewName = list_to_atom(asn1ct_gen:list2name(NameList)),
			NewTDef=#typedef{checked=true,name=NewName,
					 typespec=T},
			asn1_db:dbput(S#state.mname,NewName,NewTDef),
			%%asn1ct_gen:insert_once(parameterized_objects,{NewName,type,NewTDef}),
			insert_once(S,parameterized_objects,
				    {NewName,type,NewTDef}),
			{NewTDef,RestSettings};
		    is_tuple(TypeDef), element(1,TypeDef)=='SelectionType'  ->
			T=check_type(S,#typedef{typespec=ObjFieldSetting},
				     ObjFieldSetting),
			Name = type_name(S,T),
			{#typedef{checked=true,name=Name,typespec=T},RestSettings};
		    true ->
			case asn1ct_gen:type(asn1ct_gen:get_inner(TypeDef)) of
			    ERef = #'Externaltypereference'{module=CurrMod} ->
				{RefMod,T} = get_referenced_type(S,ERef),
				check_and_save(S,ERef#'Externaltypereference'{module=RefMod},T,RestSettings);

			    ERef = #'Externaltypereference'{} ->
				{RefMod,T} = get_referenced_type(S,ERef),
				check_and_save(S,ERef#'Externaltypereference'{module=RefMod},T,RestSettings);
			    Bif when Bif=={primitive,bif};Bif=={constructed,bif} ->
				T = check_type(S,#typedef{typespec=ObjFieldSetting},
					       ObjFieldSetting),
				{#typedef{checked=true,name=Bif,typespec=T},RestSettings};
			    _OCFT = #'ObjectClassFieldType'{} ->
				T=check_type(S,#typedef{typespec=ObjFieldSetting},ObjFieldSetting),
				%%io:format("OCFT=~p~n,T=~p~n",[OCFT,T]),
				{#typedef{checked=true,typespec=T},RestSettings};
			    _ ->
				%this case should not happen any more
				{Mod,T} = 
				    get_referenced_type(S,#'Externaltypereference'{module=S#state.mname,type=ObjFieldSetting}),
				case Mod of
				    CurrMod ->
					{T,RestSettings};
				    ExtMod ->
					#typedef{name=Name} = T,
					{T#typedef{name={ExtMod,Name}},RestSettings}
				end
			end
		end,
	    {{ObjFieldName,Type},SettingsLeft};
	fixedtypevaluefield ->
	    case ObjFieldName of
		Val when is_atom(Val) ->
		    %% ObjFieldSetting can be a value,an objectidentifiervalue,
		    %% an element in an enumeration or namednumberlist etc.
		    ValRef =
			case ObjFieldSetting of
			    ValSetting=#'Externalvaluereference'{} ->
				ValSetting;
			    {'ValueFromObject',{_,ObjRef},FieldName} ->
				{_,Object} = get_referenced_type(S,ObjRef),
				ChObject = check_object(S,Object,
							Object#typedef.typespec),
				get_fieldname_element(S,Object#typedef{typespec=ChObject},
						      FieldName);
			    ValSetting = #valuedef{} ->
				ValSetting;
			    ValSetting = {'CHOICE',{Alt,_ChVal}} when is_atom(Alt) ->
					#valuedef{type=element(3,CField),
						  value=ValSetting,
						  module=S#state.mname};
			    ValSetting ->
				#identifier{val=ValSetting}
			end,
		    ?dbg("fixedtypevaluefield ValRef: ~p~n",[ValRef]),
		    case ValRef of
			#valuedef{} ->
			    {{ObjFieldName,check_value(S,ValRef)},RestSettings};
			_ ->
			    ValDef =
				case catch get_referenced_type(S,ValRef) of
				    {error,_} ->
					NewValDef =
					    #valuedef{name=Val,
						      type=element(3,CField),
						      value=ObjFieldSetting,
						      module=S#state.mname},
					check_value(S,NewValDef);
				    {M,VDef} when is_record(VDef,valuedef) ->
					check_value(update_state(S,M),
						    %%S#state{mname=M},
						    VDef);%% XXX
				    {M,VDef} ->
					check_value(update_state(S,M),
						    %%S#state{mname=M},
						    #valuedef{name=Val,
							      type=element(3,CField),
							      value=VDef,
							      module=M})
				end,
			    {{ObjFieldName,ValDef},RestSettings}
		    end;
		Val ->
		    {{ObjFieldName,Val},RestSettings}
	    end;
	fixedtypevaluesetfield ->
	    {{ObjFieldName,ObjFieldSetting},RestSettings};
	objectfield ->
	    CheckObject = 
		fun(O) ->
			O#typedef{checked=true,typespec=
				  check_object(S,O,O#typedef.typespec)}
		end,
	    ObjectSpec = 
		case ObjFieldSetting of
		    Ref when is_record(Ref,'Externalvaluereference') ->
			%% The object O might be a #valuedef{} if
			%% e.g. the definition looks like 
			%% myobj SOMECLASS ::= referencedObject
			{M,O} = get_referenced_type(S,Ref),
			check_object(S,O,object_to_check(O)),
			Ref#'Externalvaluereference'{module=M};

		    {'ValueFromObject',{_,ObjRef},FieldName} ->
			%% This is an ObjectFromObject
			{_,Object} = get_referenced_type(S,ObjRef),
			ChObject = check_object(S,Object,
						Object#typedef.typespec),
			ObjFromObj=
			    get_fieldname_element(S,Object#typedef{
						      typespec=ChObject},
						  FieldName),
			CheckObject(ObjFromObj);
		    ObjDef={object,_,_} ->
			%% An object defined inlined in another object
			%% class is an objectfield, that implies that 
			%% {objectsetfield,TypeFieldName,DefinedObjecClass,
			%%  OptionalitySpec}
			%% DefinedObjecClass = #'Externaltypereference'{}|
			%% 'TYPE-IDENTIFIER' | 'ABSTRACT-SYNTAX'
			ClassName = element(3,CField),
			InlinedObjName=
			    list_to_atom(lists:concat([S#state.tname]++
						      ['_',ObjFieldName])),

			ObjSpec = #'Object'{classname=ClassName,
					    def=ObjDef},
			CheckedObj=
			    check_object(S,#typedef{typespec=ObjSpec},ObjSpec),
			InlObj = #typedef{checked=true,name=InlinedObjName,
					  typespec=CheckedObj},
			ObjKey = {InlinedObjName,InlinedObjName},
			%% asn1ct_gen:insert_once(inlined_objects,ObjKey),
			insert_once(S,inlined_objects,ObjKey),
			%% Which module to use here? Could it be other than top_module ?
			%% asn1_db:dbput(S#state.mname,InlinedObjName,InlObj),
			asn1_db:dbput(get(top_module),InlinedObjName,InlObj),
			InlObj;
		    #type{def=Eref} when is_record(Eref,'Externaltypereference') ->
			{_,O} = get_referenced_type(S,Eref),
			CheckObject(O);
		    Other ->
			{_,O} = get_referenced_type(S,#'Externaltypereference'{module=S#state.mname,type=Other}),
			CheckObject(O)
		end,
	    {{ObjFieldName,ObjectSpec},RestSettings};
	variabletypevaluefield ->
	    {{ObjFieldName,ObjFieldSetting},RestSettings};
	variabletypevaluesetfield ->
	    {{ObjFieldName,ObjFieldSetting},RestSettings};
%% 	objectset_or_fixedtypevalueset_field ->
%% 	    ok;
	objectsetfield ->
	    ObjSetSpec = get_objectset_def(S,ObjFieldSetting,CField),
	    ?dbg("objectsetfield, ObjSetSpec:~p~n",[ObjSetSpec]),
	    {{ObjFieldName,
	      ObjSetSpec#typedef{checked=true,
				 typespec=check_object(S,ObjSetSpec,
						       ObjSetSpec#typedef.typespec)}},RestSettings}
    end.

get_objectset_def(S,Ref,CField)
  when is_record(Ref,'Externaltypereference');
       is_record(Ref,'Externalvaluereference') ->
    {_M,T}=get_referenced_type(S,Ref),
    get_objectset_def2(S,T,CField);
get_objectset_def(S,ObjectList,CField) when is_list(ObjectList) ->
    %% an objctset defined in the object,though maybe
    %% parsed as a SequenceOfValue 
    %% The ObjectList may be a list of references to
    %% objects, a ValueFromObject
    ?dbg("objectsetfield: ~p~n",[CField]),
    get_objectset_def2(S,ObjectList,CField);
get_objectset_def(S,'EXTENSIONMARK',CField) ->
    ?dbg("objectsetfield: ~p~n",[CField]),
    get_objectset_def2(S,['EXTENSIONMARK'],CField);
get_objectset_def(_S,ObjFieldSetting={'SingleValue',_},CField) ->
    %% a Union of defined objects
    ?dbg("objectsetfield, SingleValue~n",[]),
    union_of_defed_objs(CField,ObjFieldSetting);
get_objectset_def(_S,ObjFieldSetting={{'SingleValue',_},_},CField) ->
    %% a Union of defined objects
    ?dbg("objectsetfield, SingleValue~n",[]),
    union_of_defed_objs(CField,ObjFieldSetting);
get_objectset_def(S,{object,_,[#type{def={'TypeFromObject',
					  {object,RefedObj},
					  FieldName}}]},_CField) ->
    %% This case occurs when an ObjectSetFromObjects 
    %% production is used
    {_M,Def} = get_referenced_type(S,RefedObj),
    get_fieldname_element(S,Def,FieldName);
get_objectset_def(S,{object,_,[{setting,_,ERef}]},CField)
  when is_record(ERef,'Externaltypereference') ->
    {_,T} = get_referenced_type(S,ERef),
    get_objectset_def2(S,T,CField);
get_objectset_def(S,#type{def=ERef},_CField) 
  when is_record(ERef,'Externaltypereference') ->
    {_,T} = get_referenced_type(S,ERef),
    T;
get_objectset_def(S,ObjFieldSetting,CField)
  when is_atom(ObjFieldSetting) ->
    ERef = #'Externaltypereference'{module=S#state.mname,
				    type=ObjFieldSetting},
    {_,T} = get_referenced_type(S,ERef),
    get_objectset_def2(S,T,CField).

get_objectset_def2(_S,T = #typedef{typespec=#'Object'{}},_CField) ->
    #typedef{typespec=#'Object'{classname=Class,def=Def}} = T,
    T#typedef{typespec=#'ObjectSet'{class=Class,set=[Def]}};
get_objectset_def2(_S,Set,CField) when is_list(Set) ->
    {_,_,Type,_} = CField,
    ClassDef = Type#type.def,
    #typedef{typespec=#'ObjectSet'{class=ClassDef,
				   set=Set}};
get_objectset_def2(_S,T = #typedef{typespec=#'ObjectSet'{}},_CField) ->
    T;
get_objectset_def2(S,T,_CField) ->
    asn1ct:warning("get_objectset_def2: uncontrolled object set structure:~n~p~n",
		   [T],S).
    
type_name(S,#type{def=Def}) ->
    CurrMod = S#state.mname,
    case asn1ct_gen:type(asn1ct_gen:get_inner(Def)) of
	#'Externaltypereference'{module=CurrMod,type=Name} ->
	    Name;
	#'Externaltypereference'{module=Mod,type=Name} ->
	    {Mod,Name};
	Bif when Bif=={primitive,bif};Bif=={constructed,bif} ->
	    Bif
    end.

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

oCFT_def(S,T) ->
    case get_OCFT_inner(S,T) of
	ERef=#'Externaltypereference'{} -> ERef;
	{Name,Type} -> #typedef{checked=true,name=Name,typespec=Type};
	'ASN1_OPEN_TYPE' -> 
	    #typedef{checked=true,typespec=T#type{def='ASN1_OPEN_TYPE'}}
    end.
	    
get_OCFT_inner(_S,T) ->
%    Module=S#state.mname,
    Def = T#type.def,
    case Def#'ObjectClassFieldType'.type of
	{fixedtypevaluefield,_,InnerType} ->
	    case asn1ct_gen:type(asn1ct_gen:get_inner(InnerType#type.def)) of
		Bif when Bif=={primitive,bif};Bif=={constructed,bif} ->
		    {Bif,InnerType};
		ERef = #'Externaltypereference'{} ->
		    ERef
	    end;
	'ASN1_OPEN_TYPE' -> 'ASN1_OPEN_TYPE'
    end.
	    
	
	
union_of_defed_objs({_,_,_ObjClass=#type{def=ClassDef},_},ObjFieldSetting) -> 
    #typedef{typespec=#'ObjectSet'{class = ClassDef,
				   set = ObjFieldSetting}};
union_of_defed_objs({_,_,DefObjClassRef,_},ObjFieldSetting)
  when is_record(DefObjClassRef,'Externaltypereference') ->
    #typedef{typespec=#'ObjectSet'{class = DefObjClassRef,
				   set = ObjFieldSetting}}.
    

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
	    {RefM,TSDef} = get_referenced_type(OldS,ClassRef),
	    %%IsObjectSet(TSDef);
	    case TSDef of
		#classdef{} -> throw({objectsetdef});
		#typedef{typespec=#type{def=Eref}} when 
		      is_record(Eref,'Externaltypereference') ->
		    %% This case if the class reference is a defined
		    %% reference to class
		    check_value(OldS,V#typedef{typespec=TS#'ObjectSet'{class=Eref}});
		#typedef{} ->
		    % an ordinary value set with a type in #typedef.typespec
		    ValueSet = TS#'ObjectSet'.set,
		    Type=check_type(OldS,TSDef,TSDef#typedef.typespec),
		    Value = check_value(OldS,#valuedef{type=Type,
						       value=ValueSet,
						       module=RefM}),
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
check_value(OldS=#state{recordtopname=TopName},V) when is_record(V,valuedef) ->
    #valuedef{name=Name,checked=Checked,type=Vtype,
	      value=Value,module=ModName} = V,
    ?dbg("check_value, V: ~p~n",[V]),
    case Checked of
	true -> 
	    V;
	{error,_} ->
	    V;
	false ->
	    Def = Vtype#type.def,
	    Constr = Vtype#type.constraint,
	    S = OldS#state{type=Vtype,tname=Def,value=V,vname=Name},
	    SVal = update_state(S,ModName),
	    NewDef = 
		case Def of
		    Ext when is_record(Ext,'Externaltypereference') ->
			RecName = Ext#'Externaltypereference'.type,
			{RefM,Type} = get_referenced_type(S,Ext),
			%% If V isn't a value but an object Type is a #classdef{}
			%%NewS = S#state{mname=RefM},
			NewS = update_state(S,RefM),
			case Type of
			    #classdef{} ->
				throw({objectdef});
			    #typedef{} ->
				case is_contextswitchtype(Type) of
				    true ->
					#valuedef{value=CheckedVal}=
					    check_value(NewS,V#valuedef{type=Type#typedef.typespec}),
					#newv{value=CheckedVal};
				    _ ->
					#valuedef{value=CheckedVal}=
					    check_value(NewS#state{recordtopname=[RecName|TopName]},
							V#valuedef{type=Type#typedef.typespec}),
					#newv{value=CheckedVal}
				end;
			    #type{} ->
				%% A parameter that couldn't be categorized.
				#valuedef{value=CheckedVal}=
				    check_value(NewS#state{recordtopname=[RecName|TopName]},
						V#valuedef{type=Type}),
				#newv{value=CheckedVal}
			end;
		    'ANY' ->
			case Value of
			    {opentypefieldvalue,ANYType,ANYValue} ->
				CheckedV=
				    check_value(SVal,#valuedef{name=Name,
							       type=ANYType,
							       value=ANYValue,
							       module=ModName}),
				#newv{value=CheckedV#valuedef.value};
			    _ ->
				throw({error,{asn1,{'cant check value of type',Def}}})
			end;
		    'INTEGER' ->
			ok=validate_integer(SVal,Value,[],Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    {'INTEGER',NamedNumberList} ->
			ok=validate_integer(SVal,Value,NamedNumberList,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    {'BIT STRING',NamedNumberList} ->
			ok=validate_bitstring(SVal,Value,NamedNumberList,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'NULL' ->
			ok=validate_null(SVal,Value,Constr),
			#newv{};
		    'OBJECT IDENTIFIER' ->
			{ok,_}=validate_objectidentifier(SVal,Value,Constr),
			#newv{value = normalize_value(SVal,Vtype,Value,[])};
		    'RELATIVE-OID' ->
			{ok,_}=validate_relative_oid(SVal,Value,Constr),
			#newv{value = Value};
		    'ObjectDescriptor' ->
			ok=validate_objectdescriptor(SVal,Value,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'REAL' ->
			ok = validate_real(SVal,Value,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    {'ENUMERATED',NamedNumberList} ->
			ok=validate_enumerated(SVal,Value,NamedNumberList,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'BOOLEAN'->
			ok=validate_boolean(SVal,Value,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'OCTET STRING' ->
			ok=validate_octetstring(SVal,Value,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'NumericString' ->
			ok=validate_restrictedstring(SVal,Value,Def,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    TString when TString =:= 'TeletexString';
				 TString =:= 'T61String' ->
			ok=validate_restrictedstring(SVal,Value,Def,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'VideotexString' ->
			ok=validate_restrictedstring(SVal,Value,Def,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'UTCTime' ->
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
%			exit({'cant check value of type' ,Def});
		    'GeneralizedTime' ->
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
%			exit({'cant check value of type' ,Def});
		    'GraphicString' ->
			ok=validate_restrictedstring(SVal,Value,Def,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'VisibleString' ->
			ok=validate_restrictedstring(SVal,Value,Def,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'GeneralString' ->
			ok=validate_restrictedstring(SVal,Value,Def,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'PrintableString' ->
			ok=validate_restrictedstring(SVal,Value,Def,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'IA5String' ->
			ok=validate_restrictedstring(SVal,Value,Def,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'BMPString' ->
			ok=validate_restrictedstring(SVal,Value,Def,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'UTF8String' ->
			ok = validate_restrictedstring(SVal,Vtype,Value,Constr),
			%%io:format("Vtype: ~p~nValue: ~p~n",[Vtype,Value]);
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    'UniversalString' -> %added 6/12 -00
			ok = validate_restrictedstring(SVal,Value,Def,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,[])};
		    Seq when is_record(Seq,'SEQUENCE') ->
			{ok,SeqVal} = validate_sequence(SVal,Value,
						   Seq#'SEQUENCE'.components,
						   Constr),
			#newv{value=normalize_value(SVal,Vtype,SeqVal,TopName)};
		    {'SEQUENCE OF',Components} ->
			ok=validate_sequenceof(SVal,Value,Components,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,TopName)};
		    {'CHOICE',Components} ->
			ok=validate_choice(SVal,Value,Components,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,TopName)};
		    Set when is_record(Set,'SET') ->
			ok=validate_set(SVal,Value,Set#'SET'.components,
					      Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,TopName)};
		    {'SET OF',Components} ->
			ok=validate_setof(SVal,Value,Components,Constr),
			#newv{value=normalize_value(SVal,Vtype,Value,TopName)};
		    {'SelectionType',SelName,SelT} ->
			CheckedT = check_selectiontype(SVal,SelName,SelT),
			NewV = V#valuedef{type=CheckedT},
			SelVDef=check_value(S#state{value=NewV},NewV),
			#newv{value=SelVDef#valuedef.value};
		    Other ->
			exit({'cannot check value of type' ,Other})
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
%% validate_integer(S=#state{mname=M},
%% 		 #'Externalvaluereference'{module=M,value=Id}=Ref,
validate_integer(S,#'Externalvaluereference'{value=Id}=Ref,
		 NamedNumberList,Constr) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> validate_integer_ref(S,Ref,NamedNumberList,Constr)
	    %%error({value,"unknown NamedNumber",S})
    end;
validate_integer(S,Id,NamedNumberList,Constr) when is_atom(Id) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> validate_integer_ref(S,Id,NamedNumberList,Constr)
		     %error({value,"unknown NamedNumber",S})
    end;
validate_integer(_S,Value,_NamedNumberList,Constr) when is_integer(Value) ->
    check_integer_range(Value,Constr).

validate_integer_ref(S,Id,_,_) when is_atom(Id) ->
    error({value,"unknown integer referens",S});
validate_integer_ref(S,Ref,NamedNumberList,Constr) ->
    case get_referenced_type(S,Ref) of
	{M,V} when is_record(V,valuedef) -> 
	    NewS = update_state(S,M),
	    case check_value(NewS,V) of
		#valuedef{type=#type{def='INTEGER'},value=Value} ->
		    validate_integer(NewS,Value,NamedNumberList,Constr);
		_Err -> error({value,"unknown integer referens",S})
	    end;
	_ ->
	    error({value,"unknown integer referens",S})
    end.
	    
		

check_integer_range(Int,Constr) when is_list(Constr) ->
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

validate_objectidentifier(S,ERef,C) ->
    validate_objectidentifier(S,o_id,ERef,C).

validate_objectidentifier(S,OID,ERef,C) 
  when is_record(ERef,'Externalvaluereference') ->
    validate_objectidentifier(S,OID,[ERef],C);
validate_objectidentifier(S,OID,Tup,C) when is_tuple(Tup) ->
    validate_objectidentifier(S,OID,tuple_to_list(Tup),C);
validate_objectidentifier(S,OID,L,_) ->
    NewL = is_space_list(L,[]),
    case validate_objectidentifier1(S,OID,NewL) of
	NewL2 when is_list(NewL2) ->{ok,list_to_tuple(NewL2)};
	Other -> {ok,Other}
    end.

validate_objectidentifier1(S, OID, [Id|T])
  when is_record(Id,'Externalvaluereference') ->
    case catch get_referenced_type(S,Id) of
	{M,V} when is_record(V,valuedef) -> 
	    NewS = update_state(S,M),
	    case check_value(NewS,V) of
		#valuedef{type=#type{def=ERef},checked=true,
			  value=Value} when is_tuple(Value) ->
		    case is_object_id(OID,NewS,ERef) of 
			true ->
			    %% T must be a RELATIVE-OID
			    validate_oid(true,NewS, rel_oid, T, lists:reverse(tuple_to_list(Value)));
			_ ->
			    error({value, {"illegal "++to_string(OID),[Id|T]}, S})
		    end;
		_ -> 
		    error({value, {"illegal "++to_string(OID),[Id|T]}, S})
	    end;
	_ ->
	    validate_oid(true,S, OID, [Id|T], [])
    end;
validate_objectidentifier1(S,OID,V) ->
    validate_oid(true,S,OID,V,[]).

validate_oid(false, S, OID, V, Acc) ->
    error({value, {"illegal "++to_string(OID), V,Acc}, S});
validate_oid(_,_, _, [], Acc) ->
    lists:reverse(Acc);
validate_oid(_, S, OID, [Value|Vrest], Acc) when is_integer(Value) ->
    validate_oid(valid_objectid(OID,Value,Acc),S, OID, Vrest, [Value|Acc]);
validate_oid(_, S, OID, [{'NamedNumber',_Name,Value}|Vrest], Acc) 
  when is_integer(Value) ->
    validate_oid(valid_objectid(OID,Value,Acc), S, OID, Vrest, [Value|Acc]);
validate_oid(_, S, OID, [Id|Vrest], Acc) 
  when is_record(Id,'Externalvaluereference') ->
    case catch get_referenced_type(S, Id) of
	{M,V} when is_record(V,valuedef) ->
	    NewS = update_state(S,M),
	    NewVal = case check_value(NewS, V) of
			 #valuedef{checked=true,value=Value} ->
			     fun(Int) when is_integer(Int) ->  [Int];
				(L) when is_list(L) -> L;
				(T) when is_tuple(T) -> tuple_to_list(T)
			     end (Value);
			 _ ->
			     error({value, {"illegal "++to_string(OID),
					    [Id|Vrest],Acc}, S})
		     end,
	    case NewVal of
		List when is_list(List) ->
		    validate_oid(valid_objectid(OID,NewVal,Acc), NewS, 
				 OID, Vrest,lists:reverse(NewVal)++Acc);
		_ ->
		    NewVal
	    end;
	_ ->
	    case reserved_objectid(Id#'Externalvaluereference'.value, Acc) of
		Value when is_integer(Value) ->
		    validate_oid(valid_objectid(OID,Value,Acc),
				 S, OID,Vrest, [Value|Acc]);
		false ->
		    error({value, {"illegal "++to_string(OID),[Id,Vrest],Acc}, S})
	    end
    end;
validate_oid(_, S, OID, [{Atom,Value}],[]) 
  when is_atom(Atom),is_integer(Value) ->
    %% this case when an OBJECT IDENTIFIER value has been parsed as a 
    %% SEQUENCE value
    Rec = #'Externalvaluereference'{module=S#state.mname,
				    value=Atom},
    validate_objectidentifier1(S, OID, [Rec,Value]);
validate_oid(_, S, OID, [{Atom,EVRef}],[]) 
  when is_atom(Atom),is_record(EVRef,'Externalvaluereference') ->
    %% this case when an OBJECT IDENTIFIER value has been parsed as a 
    %% SEQUENCE value OTP-4354
    Rec = #'Externalvaluereference'{module=EVRef#'Externalvaluereference'.module,
				    value=Atom},
    validate_objectidentifier1(S, OID, [Rec,EVRef]);
validate_oid(_, S, OID, [Atom|Rest],Acc) when is_atom(Atom) ->
    Rec = #'Externalvaluereference'{module=S#state.mname,
				    value=Atom},
    validate_oid(true,S, OID, [Rec|Rest],Acc);
validate_oid(_, S, OID, V, Acc) ->
    error({value, {"illegal "++to_string(OID),V,Acc},S}).

validate_relative_oid(S,Value,Constr) ->
    validate_objectidentifier(S,rel_oid,Value,Constr).

is_object_id(OID,S,ERef=#'Externaltypereference'{}) ->
    {_,OI} = get_referenced_type(S,ERef),
    is_object_id(OID,S,OI#typedef.typespec);
is_object_id(o_id,_S,'OBJECT IDENTIFIER') ->
    true;
is_object_id(rel_oid,_S,'RELATIVE-OID') ->
    true;
is_object_id(_,_S,'INTEGER') ->
    true;
is_object_id(OID,S,#type{def=Def}) ->
    is_object_id(OID,S,Def);
is_object_id(_,_S,_) ->
    false.

to_string(o_id) ->
    "OBJECT IDENTIFIER";
to_string(rel_oid) ->
    "RELATIVE-OID".

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

valid_objectid(_OID,[],_Acc) ->
    true;
valid_objectid(OID,[H|T],Acc) ->
    case valid_objectid(OID, H, Acc) of
	true ->
	    valid_objectid(OID,T,[H|Acc]);
	_ ->
	    false
    end;
valid_objectid(o_id,I,[]) when I =:= 0; I =:= 1; I =:= 2 -> true;
valid_objectid(o_id,_I,[]) -> false;
valid_objectid(o_id,I,[0]) when I >= 0; I =< 4 -> true;
valid_objectid(o_id,_I,[0]) -> false;
valid_objectid(o_id,I,[1]) when I =:= 0; I =:= 2; I =:= 3 -> true;
valid_objectid(o_id,_I,[1]) -> false;
valid_objectid(o_id,_I,[2]) -> true;
valid_objectid(_,_,_) -> true.



		 
	    

validate_objectdescriptor(_S,_Value,_Constr) ->
    ok.

validate_real(_S,_Value,_Constr) ->
    ok.

validate_enumerated(S,Id,NamedNumberList,_Constr) when is_atom(Id) ->
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
		    {ok,to_EXTERNAL1990(S,Value)};
		_ ->
		    {ok,Value}
	    end;
	_ ->
	    {ok,Value}
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
			   [Value,Type,Err],S),
	    Value
    end;
normalize_value(S,Type,Val,NameList) ->
    normalize_value(S,Type,{'DEFAULT',Val},NameList).

normalize_boolean(S,{Name,Bool},CType) when is_atom(Name) ->
    normalize_boolean(S,Bool,CType);
normalize_boolean(_,true,_) ->
    true;
normalize_boolean(_,false,_) ->
    false;
normalize_boolean(S,Bool=#'Externalvaluereference'{},CType) ->
    get_normalized_value(S,Bool,CType,fun normalize_boolean/3,[]);
normalize_boolean(_,Other,_) ->
    throw({error,{asn1,{'invalid default value',Other}}}).

normalize_integer(_S,Int,_) when is_integer(Int) ->
    Int;
normalize_integer(_S,{Name,Int},_) when is_atom(Name),is_integer(Int) ->
    Int;
normalize_integer(S,{Name,Int=#'Externalvaluereference'{}},
		  Type) when is_atom(Name) ->
    normalize_integer(S,Int,Type);
normalize_integer(S,Int=#'Externalvaluereference'{value=Name},Type) ->
    case Type of
	NNL when is_list(NNL) ->
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
    %% E - list of ones and zeros, if Value already is normalized.
    case Value of
	{hstring,String} when is_list(String) ->
	    hstring_to_int(String);
	{bstring,String} when is_list(String) ->
	    bstring_to_bitlist(String);
	Rec when is_record(Rec,'Externalvaluereference') ->
	    get_normalized_value(S,Value,Type,
				 fun normalize_bitstring/3,[]);
	RecList when is_list(RecList) ->
	    case Type of
		NBL when is_list(NBL) ->
		    F = fun(#'Externalvaluereference'{value=Name}) ->
				case lists:keysearch(Name,1,NBL) of
				    {value,{Name,_}} ->
					Name;
				    Other ->
					throw({error,Other})
				end;
			   (I) when I =:= 1; I =:= 0 ->
				I;
			   (Other) ->
				throw({error,Other})
			end,
		    case catch lists:map(F,RecList) of
			{error,Reason} ->
			    asn1ct:warning("default value not "
				      "compatible with type definition ~p~n",
				      [Reason],S),
			    Value;
			NewList ->
			    NewList
		    end;
		_ ->
		    asn1ct:warning("default value not "
			      "compatible with type definition ~p~n",
			      [RecList],S),
		    Value
	    end;
	{Name,String} when is_atom(Name) ->
	    normalize_bitstring(S,String,Type);
	Other ->
	    asn1ct:warning("illegal default value ~p~n",[Other],S),
	    Value
    end.

hstring_to_int(L) when is_list(L) ->
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
	Rec when is_record(Rec,'Externalvaluereference') ->
	    get_normalized_value(S,Value,CType,
				 fun normalize_octetstring/3,[]);
	{Name,String} when is_atom(Name) ->
	    normalize_octetstring(S,String,CType);
	List when is_list(List) ->
	    %% check if list elements are valid octet values
	    lists:map(fun([])-> ok;
			 (H)when H > 255->
			      asn1ct:warning("not legal octet value ~p in OCTET STRING, ~p~n",
					     [H,List],S);
			 (_)-> ok
		      end, List),
	    List;
	Other ->
	    asn1ct:warning("unknown default value ~p~n",[Other],S),
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
    {ok,Val}=validate_objectidentifier(S,Value,[]),
    Val.

normalize_relative_oid(S,Value) ->
    {ok,Val} = validate_relative_oid(S,Value,[]),
    Val.

normalize_objectdescriptor(Value) ->
    Value.

normalize_real(Value) ->
    Value.

normalize_enumerated(S,#'Externalvaluereference'{value=V},CType)
  when is_list(CType) ->
    normalize_enumerated2(S,V,CType);
normalize_enumerated(S,Value,CType) when is_atom(Value),is_list(CType) ->
    normalize_enumerated2(S,Value,CType);
normalize_enumerated(S,{Name,EnumV},CType) when is_atom(Name) ->
    normalize_enumerated(S,EnumV,CType);
normalize_enumerated(S,Value,{CType1,CType2}) when is_list(CType1), is_list(CType2)->
    normalize_enumerated(S,Value,CType1++CType2);
normalize_enumerated(S,V,CType) ->
    asn1ct:warning("Enumerated unknown type ~p~n",[CType],S),
    V.
normalize_enumerated2(S,V,Enum) ->
    case lists:keysearch(V,1,Enum) of
	{value,{Val,_}} -> Val;
	_ -> 
	    asn1ct:warning("Enumerated value is not correct ~p~n",[V],S),
	    V
    end.


normalize_choice(S,{'CHOICE',{C,V}},CType,NameList) when is_atom(C) ->
    case catch lists:keysearch(C,#'ComponentType'.name,CType) of
	{value,#'ComponentType'{typespec=CT,name=Name}} ->
	    {C,normalize_value(S,CT,{'DEFAULT',V},
			       [Name|NameList])};
	Other ->
	    asn1ct:warning("Wrong format of type/value ~p/~p~n",[Other,V],S),
	    {C,V}
    end;
normalize_choice(S,{'DEFAULT',ValueList},CType,NameList) when is_list(ValueList) ->
    lists:map(fun(X)-> normalize_choice(S,X,CType,NameList) end, ValueList);
normalize_choice(S,Val=#'Externalvaluereference'{},CType,NameList) ->
    {M,#valuedef{value=V}}=get_referenced_type(S,Val),
    normalize_choice(update_state(S,M),{'CHOICE',V},CType,NameList);
%    get_normalized_value(S,Val,CType,fun normalize_choice/4,[NameList]);
normalize_choice(S,CV={Name,_ChoiceVal},CType,NameList) 
  when is_atom(Name) ->
%    normalize_choice(S,ChoiceVal,CType,NameList).
    normalize_choice(S,{'CHOICE',CV},CType,NameList);
normalize_choice(_S,V,_CType,_NameList) ->
    exit({error,{bad_choice_value,V}}).

%% normalize_choice(NameList,S,CVal = {'CHOICE',{_,_}},CType,_) ->
%%     normalize_choice(S,CVal,CType,NameList);
%% normalize_choice(NameList,S,CVal={'DEFAULT',VL},CType,_) when is_list(VL)->
%%     normalize_choice(S,CVal,CType,NameList);
%% normalize_choice(NameList,S,CV={Name,_CV},CType,_) when is_atom(Name)->
%%     normalize_choice(S,{'CHOICE',CV},CType,NameList);
%% normalize_choice(_,_S,V,_,_) ->
%%     V.

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

sort_value(Components,Value) ->
    ComponentNames = lists:map(fun(#'ComponentType'{name=Cname}) -> Cname end,
			       Components),
    sort_value1(ComponentNames,Value,[]).
sort_value1(_,V=#'Externalvaluereference'{},_) ->
    %% sort later, get the value in normalize_seq_or_set
    V;
sort_value1([N|Ns],Value,Acc) ->
    case lists:keysearch(N,1,Value) of
	{value,V} ->sort_value1(Ns,Value,[V|Acc]);
	_ -> sort_value1(Ns,Value,Acc)
    end;
sort_value1([],_,Acc) ->
    lists:reverse(Acc).

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
	    case normalize_seq_or_set(SorS,S,Value,Components,NameList,[]) of
		ListOfVals when length(ListOfVals) == NoComps ->
		    list_to_tuple([NewName|ListOfVals]);
		_ ->
		    error({type,{illegal,default,value,Value},S})
	    end
    end.
is_record_normalized(S,Name,V = #'Externalvaluereference'{},NumComps) ->
    case get_referenced_type(S,V) of
	{_M,#valuedef{type=_T1,value=V2}} ->
	    is_record_normalized(S,Name,V2,NumComps);
	_ -> false
    end;
is_record_normalized(_S,Name,Value,NumComps) when is_tuple(Value) ->
    (size(Value) =:= (NumComps + 1)) andalso (element(1,Value)=:=Name);
is_record_normalized(_,_,_,_) ->
    false.

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
	    asn1ct:warning("~p could not handle value ~p~n",[SorS,Value],S),
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
			 fun normalize_restrictedstring/3,[]);
%% 
normalize_restrictedstring(S,{Name,Val},CType) when is_atom(Name) ->
    normalize_restrictedstring(S,Val,CType).

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
	    asn1ct:warning("default value not comparable ~p~n",[Val],S),
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
    %Tag = Ts#type.tag,
    %Constr = Ts#type.constraint,
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
	case match_parameters(S,Ts#type.def,S#state.parameters) of
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
	    Ext when is_record(Ext,'Externaltypereference') ->
		{RefMod,RefTypeDef,IsParamDef} = 
		    case get_referenced_type(S,Ext) of
			{undefined,TmpTDef} -> %% A parameter
			    {get(top_module),TmpTDef,true};
			{TmpRefMod,TmpRefDef} ->
			    {TmpRefMod,TmpRefDef,false}
		    end,
		case is_class(S,RefTypeDef) of
		    true -> throw({asn1_class,RefTypeDef});
		    _ -> ok
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
					   type=NewRefTypeDef1,
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
			    {RefType1,#'Externaltypereference'{module=RefMod,
							       type=TmpName}}
		    end,

		case asn1ct_gen:prim_bif(asn1ct_gen:get_inner(RefType#type.def)) of
		    true ->
			%% Here we expand to a built in type and inline it
			NewS2 = S#state{type=#typedef{typespec=RefType}},
			NewC = 
			    constraint_merge(NewS2,
					     check_constraints(NewS2,Constr)++
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
			  tag = case S#state.erule of
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
	    'REAL' ->
		check_real(S,Constr),
		TempNewDef#newt{tag=merge_tags(Tag,?TAG_PRIMITIVE(?N_REAL))};
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
				 check_enumerated(S,NamedNumberList,Constr)},
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
		Ct = maybe_illicit_implicit_tag(choice,Tag),
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
		{_RefMod,Ptypedef} = get_referenced_type(S,Ptype),
		notify_if_not_ptype(S,Ptypedef),
		NewParaList = 
		    [match_parameters(S,TmpParam,S#state.parameters)|| 
				  TmpParam <- ParaList],
		Instance = instantiate_ptype(S,Ptypedef,NewParaList),
		TempNewDef#newt{type=Instance#type.def,
				tag=merge_tags(Tag,Instance#type.tag),
				constraint=Instance#type.constraint,
				inlined=yes};

	    OCFT=#'ObjectClassFieldType'{classname=ClRef} ->
		%% this case occures in a SEQUENCE when 
		%% the type of the component is a ObjectClassFieldType
		ClassSpec = check_class(S,ClRef),
		NewTypeDef = 
		    maybe_open_type(S,ClassSpec,
				    OCFT#'ObjectClassFieldType'{class=ClassSpec},Constr),
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

	    {'TypeFromObject',{object,Object},TypeField} ->
		CheckedT = get_type_from_object(S,Object,TypeField),
		TempNewDef#newt{tag=merge_tags(Tag,CheckedT#type.tag),
				type=CheckedT#type.def};

	    {valueset,Vtype} ->
		TempNewDef#newt{type={valueset,check_type(S,Type,Vtype)}};
	    {'SelectionType',Name,T} ->
		CheckedT = check_selectiontype(S,Name,T),
		TempNewDef#newt{tag=merge_tags(Tag,CheckedT#type.tag),
				type=CheckedT#type.def};
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
    T5#type{constraint=check_constraints(S,T5#type.constraint)};
check_type(_S,Type,Ts) ->
    exit({error,{asn1,internal_error,Type,Ts}}).

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
    
get_type_from_object(S,Object,TypeField)
  when is_record(Object,'Externaltypereference');
       is_record(Object,'Externalvaluereference') ->
    {_,ObjectDef} = get_referenced_type(S,Object),
    ObjSpec = check_object(S,ObjectDef,ObjectDef#typedef.typespec),
    get_fieldname_element(S,ObjectDef#typedef{typespec=ObjSpec},TypeField).
    
is_class(_S,#classdef{}) ->
    true;
is_class(S,#typedef{typespec=#type{def=Eref}}) 
  when is_record(Eref,'Externaltypereference')->
    is_class(S,Eref);
is_class(S,Eref) when is_record(Eref,'Externaltypereference')->
    {_,NextDef} = get_referenced_type(S,Eref),
    is_class(S,NextDef);
is_class(_,_) ->
    false.

get_class_def(_S,CD=#classdef{}) ->
    CD;
get_class_def(S,#typedef{typespec=#type{def=Eref}})
  when is_record(Eref,'Externaltypereference') ->
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
maybe_open_type(S,ClassSpec=#objectclass{fields=Fs},
		OCFT=#'ObjectClassFieldType'{fieldname=FieldRefList},
		Constr) ->
    Type = get_ObjectClassFieldType(S,Fs,FieldRefList),
    FieldNames=get_referenced_fieldname(FieldRefList),
    case last_fieldname(FieldRefList) of
	{valuefieldreference,_} ->
	    OCFT#'ObjectClassFieldType'{fieldname=FieldNames,
					type=Type};
	{typefieldreference,_} ->
	    case {catch get_unique_fieldname(S,#classdef{typespec=ClassSpec}),
		  asn1ct_gen:get_constraint(Constr,componentrelation)}of
		{Tuple,_} when is_tuple(Tuple), size(Tuple) =:= 3 ->
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

last_fieldname(FieldRefList) when is_list(FieldRefList) ->
    lists:last(FieldRefList);
last_fieldname({FieldName,_}) when is_atom(FieldName) ->
    [A|_] = atom_to_list(FieldName),
    case is_lowercase(A) of
	true ->
	    {valuefieldreference,FieldName};
	_ ->
	    {typefieldreference,FieldName}
    end.

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
notify_if_not_ptype(_S,PT) ->
    throw({error,{"supposed to be a parameterized type",PT}}).
% fix me
instantiate_ptype(S,Ptypedef,ParaList) ->
    #ptypedef{args=Args,typespec=Type} = Ptypedef,
    NewType = check_ptype(S,Ptypedef,Type#type{inlined=yes}),    
    MatchedArgs = match_args(S,Args, ParaList, []),
    OldArgs = S#state.parameters,
    NewS = S#state{type=NewType,parameters=MatchedArgs++OldArgs,abscomppath=[]},
%%    NewS = S#state{type=NewType,parameters=MatchedArgs,abscomppath=[]},
    check_type(NewS, Ptypedef#ptypedef{typespec=NewType}, NewType).

get_datastr_name(#typedef{name=N}) ->
    N;
get_datastr_name(#classdef{name=N}) ->
    N;
get_datastr_name(#valuedef{name=N}) ->
    N;
get_datastr_name(#ptypedef{name=N}) ->
    N;
get_datastr_name(#pvaluedef{name=N}) ->
    N;
get_datastr_name(#pvaluesetdef{name=N}) ->
    N;
get_datastr_name(#pobjectdef{name=N}) ->
    N;
get_datastr_name(#pobjectsetdef{name=N}) ->
    N.


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
match_args(_,_, _, _) ->
    throw({error,{asn1,{wrong_number_of_arguments}}}).

%%%%%%%%%%%%%%%%%
%% categorize_arg(S,FormalArg,ActualArg) -> {FormalArg,CatgorizedActualArg}
%%
categorize_arg(S,{Governor,Param},ActArg) ->
    case {governor_category(S,Governor),parameter_name_style(Param,ActArg)} of
%% 	{absent,beginning_uppercase} -> %% a type
%% 	    categorize(S,type,ActArg);
	{type,beginning_lowercase} -> %% a value
	    categorize(S,value,Governor,ActArg);
	{type,beginning_uppercase} -> %% a value set
	    categorize(S,value_set,ActArg);
%% 	{absent,entirely_uppercase} -> %% a class
%% 	    categorize(S,class,ActArg);
	{{class,ClassRef},beginning_lowercase} -> 
	    categorize(S,object,ActArg,ClassRef);
	{{class,ClassRef},beginning_uppercase} ->
	    categorize(S,object_set,ActArg,ClassRef);
	_ ->
	    [ActArg]
    end;
categorize_arg(S,FormalArg,ActualArg) ->
    %% governor is absent => a type or a class
    case FormalArg of
	#'Externaltypereference'{type=Name} ->
	    case is_class_name(Name) of
		true ->
		    categorize(S,class,ActualArg);
		_ ->
		    categorize(S,type,ActualArg)
	    end;
	FA ->
	    throw({error,{unexpected_formal_argument,FA}})
    end.

governor_category(S,#type{def=Eref}) 
  when is_record(Eref,'Externaltypereference') ->   
    governor_category(S,Eref);
governor_category(_S,#type{}) ->
    type;
governor_category(S,Ref) when is_record(Ref,'Externaltypereference') ->
    case is_class(S,Ref) of
	true ->
	    {class,Ref};
	_ ->
	    type
    end;
governor_category(_,Class) 
  when Class == 'TYPE-IDENTIFIER'; Class == 'ABSTRACT-SYNTAX' ->
    class.
%% governor_category(_,_) ->
%%     absent.

%% parameter_name_style(Param,Data) -> Result
%% gets the Parameter and the name of the Data and if it exists tells
%% whether it begins with a lowercase letter or is partly or entirely
%% spelled with uppercase letters. Otherwise returns undefined
%%
parameter_name_style(_,#'Externaltypereference'{type=Name}) ->
    name_category(Name);
parameter_name_style(_,#'Externalvaluereference'{value=Name}) ->
    name_category(Name);
parameter_name_style(_,{valueset,_}) ->
    %% It is a object set or value set
    beginning_uppercase;
parameter_name_style(#'Externalvaluereference'{},_) ->
    beginning_lowercase;
parameter_name_style(#'Externaltypereference'{type=Name},_) ->
    name_category(Name);
parameter_name_style(_,_) ->
    undefined.

name_category(Atom) when is_atom(Atom) ->
    name_category(atom_to_list(Atom));
name_category([H|T]) ->
    case is_lowercase(H) of
	true ->
	    beginning_lowercase;
	_ ->
	    case is_class_name(T) of
		true ->
		    entirely_uppercase;
		_ ->
		    beginning_uppercase
	    end
    end;
name_category(_) ->
    undefined.

is_lowercase(X) when X >= $A,X =< $W ->
    false;
is_lowercase(_) ->
    true.

is_class_name(Name) when is_atom(Name) ->
    is_class_name(atom_to_list(Name));
is_class_name(Name) ->
    case [X||X <- Name, X >= $a,X =< $w] of
	[] ->
	    true;
	_ ->
	    false
    end.
		
%% categorize(S,Category,Parameter) -> CategorizedParameter
%% If Parameter has an abstract syntax of another category than
%% Category, transform it to a known syntax.
categorize(_S,type,{object,_,Type}) ->
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
categorize(_S,type,Def) when is_record(Def,type) ->
    [#typedef{name = new_reference_name("type_argument"),
	      typespec = Def#type{inlined=yes}}];
categorize(_,_,Def) ->
    [Def].
categorize(S,object_set,Def,ClassRef) ->
    %% XXXXXXXXXX
    case Def of
	{'Externaltypereference',undefined,'MSAccessProtocol','AllOperations'} ->
	    ok;
	_ ->
	    ok
    end,
    NewObjSetSpec = 
	check_object(S,Def,#'ObjectSet'{class = ClassRef,
					set = parse_objectset(Def)}),
    Name = new_reference_name("object_set_argument"),
    %% XXXXXXXXXX
    case Name of
	internal_object_set_argument_78 ->
	    ok;
	internal_object_set_argument_77 ->
	    ok;
	_ ->
	    ok
    end,
    [save_object_set_instance(S,Name,NewObjSetSpec)];
categorize(_S,object,Def,_ClassRef) ->
    %% should be handled
    [Def];
categorize(_S,value,_Type,Value) when is_record(Value,valuedef) ->
    [Value];
categorize(S,value,Type,Value) ->
%%    [check_value(S,#valuedef{type=Type,value=Value})].
    [#valuedef{type=Type,value=Value,module=S#state.mname}].


parse_objectset({valueset,T=#type{}}) ->
    [T];
parse_objectset({valueset,Set}) ->
    Set;
parse_objectset(#type{def=Ref}) when is_record(Ref,'Externaltypereference') ->
    Ref;
parse_objectset(Set) ->
    %% extend this later
    Set.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% check_constraints/2
%%    
check_constraints(S,C) when is_list(C) -> 
    check_constraints(S, C, []);
check_constraints(S,C) when is_record(C,constraint) -> 
    check_constraints(S, C#constraint.c, []).

resolv_tuple_or_list(S,List) when is_list(List) ->
    lists:map(fun(X)->resolv_value(S,X) end, List);
resolv_tuple_or_list(S,{Lb,Ub}) ->
    {resolv_value(S,Lb),resolv_value(S,Ub)}.

%%%-----------------------------------------
%% If the constraint value is a defined value the valuename
%% is replaced by the actual value
%%
resolv_value(S,Val) ->
    Id = match_parameters(S,Val, S#state.parameters),
    resolv_value1(S,Id).

resolv_value1(S, ERef = #'Externalvaluereference'{value=Name}) ->
    case catch resolve_namednumber(S,S#state.type,Name) of
	V when is_integer(V) -> V;
	_ ->
	    case get_referenced_type(S,ERef) of
		{Err,_Reason} when Err == error; Err == 'EXIT' ->
		    throw({error,{asn1,{undefined_type_or_value,
				Name}}});
		{_M,VDef} ->
		    resolv_value1(S,VDef)
	    end
    end;
resolv_value1(S,{gt,V}) ->
    case V of
	Int when is_integer(Int) ->
	    V + 1;
	#valuedef{value=Int} ->
	    1 + resolv_value(S,Int);
	Other ->
	    throw({error,{asn1,{undefined_type_or_value,Other}}})
    end;
resolv_value1(S,{lt,V}) ->
    case V of
	Int when is_integer(Int) ->
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
    resolve_value_from_object(S,Object,FieldName);
resolv_value1(_,#valuedef{checked=true,value=V}) ->
    V;
resolv_value1(S,#valuedef{type=_T,
			  value={'ValueFromObject',{object,Object},
				 [{valuefieldreference,
				   FieldName}]}}) ->
    resolve_value_from_object(S,Object,FieldName);
resolv_value1(S,VDef = #valuedef{}) ->
    #valuedef{value=Val} = check_value(S,VDef),
    Val;
resolv_value1(_,V) ->
    V.
resolve_value_from_object(S,Object,FieldName) ->
    {_,ObjTDef} = get_referenced_type(S,Object),
    TS = check_object(S,ObjTDef,ObjTDef#typedef.typespec),
    {_,_,Components} = TS#'Object'.def,
    case lists:keysearch(FieldName,1,Components) of
	{value,{_,#valuedef{value=Val}}} ->
	    Val;
	_ ->
	    error({value,"illegal value in constraint",S})
    end.



resolve_namednumber(S,#typedef{typespec=Type},Name) ->
    case Type#type.def of
	{'ENUMERATED',NameList} ->
	    NamedNumberList=check_enumerated(S,NameList,Type#type.constraint),
	    N = normalize_enumerated(S,Name,NamedNumberList),
	    {value,{_,V}} = lists:keysearch(N,1,NamedNumberList),
	    V;
	{'INTEGER',NameList} ->
	    NamedNumberList = check_enumerated(S,NameList,Type#type.constraint),
	    {value,{_,V}} = lists:keysearch(Name,1,NamedNumberList),
	    V;
	_ ->
	    not_enumerated
    end.
    
check_constraints(S,[{'ContainedSubtype',Type} | Rest], Acc) ->
    {RefMod,CTDef} = get_referenced_type(S,Type#type.def),
    NewS = S#state{module=load_asn1_module(S,RefMod),mname=RefMod,
		   type=CTDef,tname=get_datastr_name(CTDef)},
    CType = check_type(NewS,S#state.tname,CTDef#typedef.typespec),    
    check_constraints(S,Rest,CType#type.constraint ++ Acc);
check_constraints(S,[C | Rest], Acc) ->
    check_constraints(S,Rest,[check_constraint(S,C) | Acc]);
check_constraints(S,[],Acc) ->
    constraint_merge(S,Acc).


range_check(F={FixV,FixV}) ->
%    FixV;
    F;
range_check(VR={Lb,Ub}) when Lb < Ub ->
    VR;
range_check(Err={_,_}) ->
    throw({error,{asn1,{illegal_size_constraint,Err}}});
range_check(Value) ->
    Value.

check_constraint(S,Ext) when is_record(Ext,'Externaltypereference') ->
    check_externaltypereference(S,Ext);


check_constraint(S,{'SizeConstraint',{Lb,Ub}}) 
  when is_list(Lb);is_tuple(Lb),size(Lb)==2 ->
    NewLb = range_check(resolv_tuple_or_list(S,Lb)),
    NewUb = range_check(resolv_tuple_or_list(S,Ub)),
    {'SizeConstraint',{NewLb,NewUb}};
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

check_constraint(S,{'SingleValue', L}) when is_list(L) ->
    F = fun(A) -> resolv_value(S,A) end,
    {'SingleValue',lists:map(F,L)};
    
check_constraint(S,{'SingleValue', V}) when is_integer(V) ->
    Val = resolv_value(S,V),
%%    [{'SingleValue',Val},{'ValueRange',{Val,Val}}]; % Why adding value range?
    {'SingleValue',Val};
check_constraint(S,{'SingleValue', V}) ->
    {'SingleValue',resolv_value(S,V)};

check_constraint(S,{'ValueRange', {Lb, Ub}}) ->
    {'ValueRange',{resolv_value(S,Lb),resolv_value(S,Ub)}};
%% In case of a constraint with extension marks like (1..Ub,...)
check_constraint(S,{VR={'ValueRange', {_Lb, _Ub}},Rest}) ->
    {check_constraint(S,VR),Rest};
check_constraint(_S,{'PermittedAlphabet',PA}) ->
    {'PermittedAlphabet',permitted_alphabet_cnstr(PA)};

check_constraint(S,{valueset,Type}) ->
    {valueset,check_type(S,S#state.tname,Type)};

check_constraint(_S,ST={simpletable,Type}) when is_atom(Type) ->
    %% An already checked constraint
    ST;
check_constraint(S,{simpletable,Type}) ->
    Def = case Type of
	      #type{def=D} -> D;
	      {'SingleValue',ObjRef = #'Externalvaluereference'{}} ->
		  ObjRef
	  end,
    C = match_parameters(S,Def,S#state.parameters),
    case C of
	#'Externaltypereference'{} ->
	    ERef = check_externaltypereference(S,C),
	    {simpletable,ERef#'Externaltypereference'.type};
	#type{def=#'Externaltypereference'{type=T}} ->
	    check_externaltypereference(S,C#type.def),
	    {simpletable,T};
	{valueset,#type{def=ERef=#'Externaltypereference'{}}} -> % this is an object set
	    {_,TDef} = get_referenced_type(S,ERef),
	    case TDef#typedef.typespec of 
		#'ObjectSet'{} ->
		    check_object(S,TDef,TDef#typedef.typespec),
		    {simpletable,ERef#'Externaltypereference'.type};
		Err ->
		    exit({error,{internal_error,Err}})
	    end;
	#'Externalvaluereference'{} ->
	    %% This is an object set with a referenced object
	    {_,TorVDef} = get_referenced_type(S,C),
	    GetObjectSet = 
		fun(#typedef{typespec=O}) when is_record(O,'Object') ->
			#'ObjectSet'{class=O#'Object'.classname,
				     set={'SingleValue',C}};
		   (#valuedef{type=Cl,value=O}) 
		   when is_record(O,'Externalvaluereference'),
			is_record(Cl,type) ->
			%% an object might reference another object
			#'ObjectSet'{class=Cl#type.def,
				     set={'SingleValue',O}};
		   (Err) -> 
			exit({error,{internal_error,simpletable_constraint,Err}})
		end,
	    ObjSet = GetObjectSet(TorVDef),
	    {simpletable,check_object(S,Type,ObjSet)};
	#'ObjectSet'{} ->
	    io:format("ALERT: simpletable forbidden case!~n",[]),
	    {simpletable,check_object(S,Type,C)};
	{'ValueFromObject',{_,ORef},FieldName} ->
	    %% This is an ObjectFromObject
	    {_,Object} = get_referenced_type(S,ORef),
	    ChObject = check_object(S,Object,
				    Object#typedef.typespec),
	    ObjFromObj=
		get_fieldname_element(S,Object#typedef{
					  typespec=ChObject},
				      FieldName),
	    {simpletable,ObjFromObj};
%% 	     ObjFromObj#typedef{checked=true,typespec=
%% 				check_object(S,ObjFromObj,
%% 					     ObjFromObj#typedef.typespec)}};
	_ -> 
	    check_type(S,S#state.tname,Type),%% this seems stupid.
	    OSName = Def#'Externaltypereference'.type,
	    {simpletable,OSName}
    end;

check_constraint(S,{componentrelation,{objectset,Opos,Objset},Id}) ->
    %% Objset is an 'Externaltypereference' record, since Objset is
    %% a DefinedObjectSet.
    RealObjset = match_parameters(S,Objset,S#state.parameters),
    ObjSetRef =
	case RealObjset of
	    #'Externaltypereference'{} -> RealObjset;
	    #type{def=#'Externaltypereference'{}} -> RealObjset#type.def;
	    {valueset,OS = #type{def=#'Externaltypereference'{}}} -> OS#type.def
	end,
    Ext = check_externaltypereference(S,ObjSetRef),
    {componentrelation,{objectset,Opos,Ext},Id};

check_constraint(S,Type) when is_record(Type,type) ->
    #type{def=Def} = check_type(S,S#state.tname,Type),
    Def;

check_constraint(S,C) when is_list(C) ->
    lists:map(fun(X)->check_constraint(S,X) end,C);
% else keep the constraint unchanged
check_constraint(_S,Any) ->
%    io:format("Constraint = ~p~n",[Any]),
    Any.

permitted_alphabet_cnstr(T) when is_tuple(T) ->
    permitted_alphabet_cnstr([T]);
permitted_alphabet_cnstr(L) when is_list(L) ->
    VRexpand = fun({'ValueRange',{A,B}}) ->
		       {'SingleValue',expand_valuerange(A,B)};
		  (Other) ->
		       Other
	       end,
    L2 = lists:map(VRexpand,L),
    %% first perform intersection
    L3 = permitted_alphabet_intersection(L2),
    [Res] = permitted_alphabet_union(L3),
    Res.

expand_valuerange([A],[A]) ->
    [A];
expand_valuerange([A],[B]) when A < B ->
    [A|expand_valuerange([A+1],[B])].

permitted_alphabet_intersection(C) ->
    permitted_alphabet_merge(C,intersection, []).

permitted_alphabet_union(C) ->
    permitted_alphabet_merge(C,union, []).

permitted_alphabet_merge([],_,Acc) ->
    lists:reverse(Acc);
permitted_alphabet_merge([{'SingleValue',L1},
			  UorI,
			  {'SingleValue',L2}|Rest],UorI,Acc)
  when is_list(L1),is_list(L2) ->
    UI = ordsets:UorI([ordsets:from_list(L1),ordsets:from_list(L2)]),
    permitted_alphabet_merge([{'SingleValue',UI}|Rest],UorI,Acc);
permitted_alphabet_merge([C1|Rest],UorI,Acc) ->
    permitted_alphabet_merge(Rest,UorI,[C1|Acc]).


%% constraint_merge/2
%% Compute the intersection of the outermost level of the constraint list.
%% See Dubuisson second paragraph and fotnote on page 285.
%% If constraints with extension are included in combined constraints. The 
%% resulting combination will have the extension of the last constraint. Thus,
%% there will be no extension if the last constraint is without extension.
%% The rootset of all constraints are considered in the "outermoust 
%% intersection". See section 13.1.2 in Dubuisson.
constraint_merge(_S,C=[H])when is_tuple(H) ->
    C;
constraint_merge(_S,[]) ->
    [];
constraint_merge(S,C) ->
    %% skip all extension but the last extension
    C1 = filter_extensions(C),
    %% perform all internal level intersections, intersections first
    %% since they have precedence over unions
    C2 = lists:map(fun(X)when is_list(X)->constraint_intersection(S,X);
		      (X) -> X end,
		   C1),
    %% perform all internal level unions
    C3 = lists:map(fun(X)when is_list(X)->constraint_union(S,X);
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
    NewCs = combine_constraints(S,CombSV,CombVR,CombSZ++RestC),
    [X||X <- lists:flatten(NewCs),
	X /= intersection,
	X /= union].

%% constraint_union(S,C) takes a list of constraints as input and
%% merge them to a union. Unions are performed when two
%% constraints is found with an atom union between. 
%% The list may be nested. Fix that later !!!
constraint_union(_S,[]) ->
    [];
constraint_union(_S,C=[_E]) ->
    C;
constraint_union(S,C) when is_list(C) ->
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
    Fun=fun({_,{'MIN',_B1}},{_,{A2,_B2}}) when is_integer(A2)->true;
	   ({_,{A1,_B1}},{_,{'MAX',_B2}}) when is_integer(A1) -> true;
	   ({_,{A1,_B1}},{_,{A2,_B2}}) when is_integer(A1),is_integer(A2),A1<A2 -> true;
	   ({_,{A,B1}},{_,{A,B2}}) when B1=<B2->true;
	   (_,_)->false end,
    % sort and remove duplicates
    SortedVR = lists:sort(Fun,VR),
    RemoveDup = fun([],_) ->[];
		   ([H],_) -> [H];
		   ([H,H|T],F) -> F([H|T],F);
		   ([H|T],F) -> [H|F(T,F)]
		end,
    
    constraint_union_vr(RemoveDup(SortedVR,RemoveDup),[]).

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

union_sv_vr(_S,C1={'SingleValue',SV},C2={'ValueRange',VR={Lb,Ub}}) 
  when is_integer(SV) ->
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
  when is_list(SV) ->
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

remove_val_from_list(Val,List) ->
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
%% Key = is_atom()
%% Ix = integer()
%% L  = [TwoTuple]
%% TwoTuple = [{atom(),term()}|...]
%% Returns a List that contains all 
%% elements from L that has a key Key as element Ix
keysearch_allwithkey(Key,Ix,L) ->
    lists:filter(fun(X) when is_tuple(X) ->
			 case element(Ix,X) of
			     Key -> true;
			     _ -> false
			 end;
		    (_) -> false
		 end, L).


%% filter_extensions(C)
%% takes a list of constraints as input and returns a list with the
%% constraints and all extensions but the last are removed.
filter_extensions([L]) when is_list(L) ->
    [filter_extensions(L)];
filter_extensions(C=[_H]) ->
    C;
filter_extensions(C) when is_list(C) ->
    filter_extensions(C,[], []).

filter_extensions([],Acc,[]) ->
    Acc;
filter_extensions([],Acc,[EC|ExtAcc]) ->
    CwoExt = remove_extension(ExtAcc,[]),
    CwoExt ++ [EC|Acc];
filter_extensions([C={A,_E}|T],Acc,ExtAcc) when is_tuple(A) ->
    filter_extensions(T,Acc,[C|ExtAcc]);
filter_extensions([C={'SizeConstraint',{A,_B}}|T],Acc,ExtAcc) 
  when is_list(A);is_tuple(A) ->
    filter_extensions(T,Acc,[C|ExtAcc]);
filter_extensions([C={'PermittedAlphabet',{{'SingleValue',_},E}}|T],Acc,ExtAcc)
  when is_tuple(E); is_list(E) ->
    filter_extensions(T,Acc,[C|ExtAcc]);
filter_extensions([H|T],Acc,ExtAcc) ->
    filter_extensions(T,[H|Acc],ExtAcc).

remove_extension([],Acc) ->
    Acc;
remove_extension([{'SizeConstraint',{A,_B}}|R],Acc) ->
    remove_extension(R,[{'SizeConstraint',A}|Acc]);
remove_extension([{C,_E}|R],Acc) when is_tuple(C) ->
    remove_extension(R,[C|Acc]);
remove_extension([{'PermittedAlphabet',{A={'SingleValue',_},
					E}}|R],Acc) 
  when is_tuple(E);is_list(E) ->
    remove_extension(R,[{'PermittedAlphabet',A}|Acc]).

%% constraint_intersection(S,C) takes a list of constraints as input and
%% performs intersections. Intersecions are performed when an 
%% atom intersection is found between two constraints. 
%% The list may be nested. Fix that later !!!
constraint_intersection(_S,[]) ->
    [];
constraint_intersection(_S,C=[_E]) ->
    C;
constraint_intersection(S,C) when is_list(C) ->
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
%% S = is_record(state,S)
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

intersection_sv_vr(_S,[C1={'SingleValue',SV}],[C2={'ValueRange',{_Lb,_Ub}}]) 
  when is_integer(SV) ->
    case is_int_in_vr(SV,C2) of
	true -> [C1];
	_ -> %%error({type,{"asn1 illegal constraint",C1,C2},S})
	    %throw({error,{"asn1 illegal constraint",C1,C2}})
	    %io:format("warning: could not analyze constraint ~p~n",[[C1,C2]]),
	    [C1,C2]
    end;
intersection_sv_vr(_S,[C1={'SingleValue',SV}],[C2]) 
  when is_list(SV) ->
    case lists:filter(fun(X)->is_int_in_vr(X,C2) end,SV) of
	[] ->
	    %%error({type,{"asn1 illegal constraint",C1,C2},S});
	    %throw({error,{"asn1 illegal constraint",C1,C2}});
	    %io:format("warning: could not analyze constraint ~p~n",[[C1,C2]]),
	    [C1,C2];
	[V] -> [{'SingleValue',V}];
	L -> [{'SingleValue',L}]
    end.


%% Size constraint [{'SizeConstraint',1},{'SizeConstraint',{{1,64},[]}}]

intersection_of_size(_,[]) ->
    [];
intersection_of_size(_,C=[_SZ]) ->
    C;
intersection_of_size(S,[SZ,SZ|Rest]) ->
    intersection_of_size(S,[SZ|Rest]);
intersection_of_size(S,C=[C1={_,Int},{_,Range}|Rest]) 
  when is_integer(Int),is_tuple(Range) ->
    case Range of
	{Lb,Ub} when Int >= Lb,
		     Int =< Ub ->
	    intersection_of_size(S,[C1|Rest]);
	{{Lb,Ub},Ext} when is_list(Ext),Int >= Lb,Int =< Ub ->
	    intersection_of_size(S,[C1|Rest]);
	_ ->
	    throw({error,{asn1,{illegal_size_constraint,C}}})
    end;
intersection_of_size(S,[C1={_,Range},C2={_,Int}|Rest]) 
  when is_integer(Int),is_tuple(Range) ->
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
intersection_of_sv(S,[{_,Int},{_,SV}|Rest]) when is_integer(Int),
						 is_list(SV) ->
    SV2=intersection_of_sv1(S,Int,SV),
    intersection_of_sv(S,[SV2|Rest]);
intersection_of_sv(S,[{_,SV},{_,Int}|Rest]) when is_integer(Int),
						 is_list(SV) ->
    SV2=intersection_of_sv1(S,Int,SV),
    intersection_of_sv(S,[SV2|Rest]);
intersection_of_sv(S,[{_,SV1},{_,SV2}|Rest]) when is_list(SV1),
						  is_list(SV2) ->
    SV3=common_set(SV1,SV2),
    intersection_of_sv(S,[SV3|Rest]);
intersection_of_sv(_S,SV) ->
    %%error({type,{asn1,{illegal_single_value_constraint,SV}},S}).
    throw({error,{asn1,{illegal_single_value_constraint,SV}}}).

intersection_of_sv1(_S,Int,SV) when is_integer(Int),is_list(SV) ->
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

is_int_in_vr(Int,{_,{'MIN','MAX'}}) when is_integer(Int) ->
    true;
is_int_in_vr(Int,{_,{'MIN',Ub}}) when is_integer(Int),Int =< Ub ->
    true;
is_int_in_vr(Int,{_,{Lb,'MAX'}}) when is_integer(Int),Int >= Lb ->
    true;
is_int_in_vr(Int,{_,{Lb,Ub}}) when is_integer(Int),Int >= Lb,Int =< Ub ->
    true;
is_int_in_vr(_,_) ->
    false.
    

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


get_referenced_type(S,Ext) when is_record(Ext,'Externaltypereference') ->
    case match_parameters(S,Ext, S#state.parameters) of
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
	ERef = #'Externaltypereference'{} ->
	    get_referenced_type(S,ERef);
	Other ->
	    {undefined,Other}
    end;
get_referenced_type(S=#state{mname=Emod},
		    ERef=#'Externalvaluereference'{pos=P,module=Emod,
						   value=Eval}) ->
    case match_parameters(S,ERef,S#state.parameters) of
	ERef ->
	    get_referenced1(S,Emod,Eval,P);
	OtherERef when is_record(OtherERef,'Externalvaluereference') ->
	    get_referenced_type(S,OtherERef);
	Value ->
	    {Emod,Value}
    end;
get_referenced_type(S,ERef=#'Externalvaluereference'{pos=Pos,module=Emod,
						value=Eval}) ->
    case match_parameters(S,ERef,S#state.parameters) of
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
    ?dbg("get_referenced: ~p~n",[Ename]),
    parse_and_save(S,Emod),
    ?dbg("get_referenced,parse_and_save ~n",[]),
    case asn1_db:dbget(Emod,Ename) of
	undefined ->
	    %% May be an imported entity in module Emod or Emod may not exist
	    case asn1_db:dbget(Emod,'MODULE') of
		undefined ->
		    case parse_and_save(S,Emod) of
			ok ->
			    get_referenced(S,Emod,Ename,Pos);
			_ ->
			    throw({error,{asn1,{module_not_found,Emod}}})
		    end;
		_ ->
		    NewS = update_state(S,Emod),
		    get_imported(NewS,Ename,Emod,Pos)
	    end;
	T when is_record(T,typedef) ->
	    ?dbg("get_referenced T: ~p~n",[T]),
	    Spec = T#typedef.typespec, %% XXXX Spec may be something else than #type
	    case Spec of
		#type{def=#typereference{}} ->
		    Tref = Spec#type.def,
		    Def = #'Externaltypereference'{module=Emod,
						   type=Tref#typereference.val,
						   pos=Tref#typereference.pos},
			    
		    
		    {Emod,T#typedef{typespec=Spec#type{def=Def}}};
		_ ->
		    {Emod,T} % should add check that T is exported here
	    end;
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
		    case parse_and_save(S,Imodule) of
			ok ->
			    %% check with cover
			    get_referenced(S,Module,Name,Pos);
			_ ->
			    throw({error,{asn1,{module_not_found,Imodule}}})
		    end;
		Im when is_record(Im,module) ->
		    case is_exported(Im,Name) of
			false ->
			    throw({error,
				   {asn1,{not_exported,{Im,Name}}}});
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

check_and_save(S,#'Externaltypereference'{module=M}=ERef,#typedef{checked=false}=TDef,Settings)
  when S#state.mname /= M ->
    %% This ERef is an imported type (or maybe a set.asn compilation)
    NewS = S#state{mname=M,module=load_asn1_module(S,M),
		   type=TDef,tname=get_datastr_name(TDef)},
    Type=check_type(NewS,TDef,TDef#typedef.typespec),%XXX
    CheckedTDef = TDef#typedef{checked=true,
			       typespec=Type},
    asn1_db:dbput(M,get_datastr_name(TDef),CheckedTDef),
    {merged_name(S,ERef),Settings};
check_and_save(S,#'Externaltypereference'{module=M,type=N}=Eref,
	       #ptypedef{name=Name,args=Params} = PTDef,Settings) ->
    %% instantiate a parameterized type
    %% The parameterized type should be saved as a type in the module
    %% it was instantiated.
    NewS = S#state{mname=M,module=load_asn1_module(S,M),
		   type=PTDef,tname=Name},
    {Args,RestSettings} = lists:split(length(Params),Settings),
    Type = check_type(NewS,PTDef,#type{def={pt,Eref,Args}}),
    ERefName = new_reference_name(N),
    ERefNew = #'Externaltypereference'{type=ERefName,module=S#state.mname},
    NewTDef=#typedef{checked=true,name=ERefName,
		     typespec=Type},
    insert_once(S,parameterized_objects,{ERefName,type,NewTDef}),
    asn1_db:dbput(S#state.mname,ERefNew#'Externaltypereference'.type,
		  NewTDef),
    {ERefNew,RestSettings};
check_and_save(_S,ERef,TDef,Settings) ->
    %% This might be a renamed type in a set of specs, so rename the ERef
    {ERef#'Externaltypereference'{type=asn1ct:get_name_of_def(TDef)},Settings}.

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
	    case asn1_db:dbget(ModuleName,'MODULE') of
		RefedMod when is_record(RefedMod,module) ->
		    S#state{mname=ModuleName,module=RefedMod};
		_ -> throw({error,{asn1,{module_does_not_exist,ModuleName}}})
	    end
    end.


get_renamed_reference(S,Name,Module) ->
    case renamed_reference(S,Name,Module) of
	undefined ->
	    throw({error,{asn1,{undefined_type,Name}}});
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
    case ets:info(renamed_defs) of
	undefined -> undefined;
	_ ->
	    case ets:match(renamed_defs,{'$1',Name,Module}) of
		[] -> 
		    case ets:info(original_imports) of
			undefined ->
			    undefined;
			_  ->
			    case ets:match(original_imports,{Module,'$1'}) of
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
		     

match_parameters(_S,Name,[]) ->
    Name;

match_parameters(_S,#'Externaltypereference'{type=Name},[{#'Externaltypereference'{type=Name},NewName}|_T]) ->
    NewName;
match_parameters(_S,#'Externaltypereference'{type=Name},[{{_,#'Externaltypereference'{type=Name}},NewName}|_T]) ->
    NewName;
match_parameters(_S,#'Externalvaluereference'{value=Name},[{#'Externalvaluereference'{value=Name},NewName}|_T]) ->
    NewName;
match_parameters(_S,#'Externalvaluereference'{value=Name},[{{_,#'Externalvaluereference'{value=Name}},NewName}|_T]) ->
    NewName;
match_parameters(_S,#type{def=#'Externaltypereference'{module=M,type=Name}},
		 [{#'Externaltypereference'{module=M,type=Name},Type}]) ->
    Type;
match_parameters(_S,{valueset,#type{def=#'Externaltypereference'{type=Name}}},
		 [{{_,#'Externaltypereference'{type=Name}},{valueset,#type{def=NewName}}}|_T]) ->
    NewName;
match_parameters(_S,{valueset,#type{def=#'Externaltypereference'{type=Name}}},
		 [{{_,#'Externaltypereference'{type=Name}},
		   NewName=#type{def=#'Externaltypereference'{}}}|_T]) ->
    NewName#type.def;
match_parameters(_S,{valueset,#type{def=#'Externaltypereference'{type=Name}}},
		 [{{_,#'Externaltypereference'{type=Name}},NewName}|_T]) ->
    NewName;
%% When a parameter is a parameterized element it has to be
%% instantiated now!
match_parameters(S,{valueset,T=#type{def={pt,_,_Args}}},_Parameters) ->
    case catch check_type(S,#typedef{name=S#state.tname,typespec=T},T) of
	pobjectsetdef ->

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
	    save_object_set_instance(S,Name,ObjSpec);
	pvaluesetdef -> error({pvaluesetdef,"parameterized valueset",S});
	{error,_Reason} -> error({type,"error in parameter",S});
	Ts when is_record(Ts,type) -> Ts#type.def
    end;
%% same as previous, only depends on order of parsing
match_parameters(S,{valueset,{pos,{objectset,_,POSref},Args}},Parameters) ->
    match_parameters(S,{valueset,#type{def={pt,POSref,Args}}},Parameters);
match_parameters(S,Name, [_H|T]) ->
    %%io:format("match_parameters(~p,~p)~n",[Name,[H|T]]),
    match_parameters(S,Name,T).

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
    [];
check_integer(S,NamedNumberList,_C) ->
    case [X||X<-NamedNumberList,is_tuple(X),size(X)=:=2] of
	NamedNumberList ->
	    %% An already checked integer with NamedNumberList
	    NamedNumberList;
	_ ->
	    case check_unique(NamedNumberList,2) of
		[] -> 
		    check_int(S,NamedNumberList,[]);
		L when is_list(L) ->
		    error({type,{duplicates,L},S}),
		    unchanged
	    end
    end.

    
check_int(S,[{'NamedNumber',Id,Num}|T],Acc) when is_integer(Num) ->
    check_int(S,T,[{Id,Num}|Acc]);
check_int(S,[{'NamedNumber',Id,{identifier,_,Name}}|T],Acc) ->
    Val = dbget_ex(S,S#state.mname,Name),
    check_int(S,[{'NamedNumber',Id,Val#valuedef.value}|T],Acc);
check_int(_S,[],Acc) ->
    lists:keysort(2,Acc).

check_real(_S,_Constr) ->
    ok.

check_bitstring(_S,[],_Constr) ->
    [];
check_bitstring(S,NamedNumberList,_Constr) ->
    case check_unique(NamedNumberList,2) of
	[] ->
	    check_bitstr(S,NamedNumberList,[]);
	L when is_list(L) ->
	    error({type,{duplicates,L},S}),
	    unchanged
    end.

check_bitstr(S,[{'NamedNumber',Id,Num}|T],Acc)when is_integer(Num) ->
    check_bitstr(S,T,[{Id,Num}|Acc]);
check_bitstr(S,[{'NamedNumber',Id,Name}|T],Acc) when is_atom(Name) ->
%%check_bitstr(S,[{'NamedNumber',Id,{identifier,_,Name}}|T],Acc) -> 
%%    io:format("asn1ct_check:check_bitstr/3 hej hop ~w~n",[Name]),
    Val = dbget_ex(S,S#state.mname,Name),
%%    io:format("asn1ct_check:check_bitstr/3: ~w~n",[Val]),
    check_bitstr(S,[{'NamedNumber',Id,Val#valuedef.value}|T],Acc);
check_bitstr(S,[],Acc) ->
    case check_unique(Acc,2) of
	[] ->
	    lists:keysort(2,Acc);
	L when is_list(L) ->
	    error({type,{duplicate_values,L},S}),
	    unchanged
    end;
%% When a BIT STRING already is checked, for instance a COMPONENTS OF S
%% where S is a sequence that has a component that is a checked BS, the
%% NamedNumber list is a list of {atom(),integer()} elements.
check_bitstr(S,[El={Id,Num}|Rest],Acc) when is_atom(Id),is_integer(Num) ->
    check_bitstr(S,Rest,[El|Acc]).
    
    
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
	{_,#classdef{typespec=NextEref}} 
	when is_record(NextEref,'Externaltypereference') ->
	    check_type_identifier(S,NextEref);
	{_,TD=#typedef{typespec=#type{def=#'Externaltypereference'{}}}} ->
	    check_type_identifier(S,(TD#typedef.typespec)#type.def);
	Err ->
	    error({type,{"object set in type INSTANCE OF "
			 "not of class TYPE-IDENTIFIER",Eref,Err},S})
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
check_enumerated(_S,[{Name,Number}|_Rest]= NNList,_Constr) when is_atom(Name), is_integer(Number)->
    %% already checked , just return the same list
    NNList;
check_enumerated(_S,{[{Name,Number}|_Rest],L}= NNList,_Constr) when is_atom(Name), is_integer(Number), is_list(L)->
    %% already checked , contains extension marker, just return the same lists
    NNList;
check_enumerated(S,NamedNumberList,_Constr) ->
    check_enum(S,NamedNumberList,[],[],[]).

%% identifiers are put in Acc2
%% returns either [{Name,Number}] or {[{Name,Number}],[{ExtName,ExtNumber}]}
%% the latter is returned if the ENUMERATION contains EXTENSIONMARK
check_enum(S,[{'NamedNumber',Id,Num}|T],Acc1,Acc2,Root) when is_integer(Num) ->
    check_enum(S,T,[{Id,Num}|Acc1],Acc2,Root);
check_enum(S,[{'NamedNumber',Id,{identifier,_,Name}}|T],Acc1,Acc2,Root) ->
    Val = dbget_ex(S,S#state.mname,Name),
    check_enum(S,[{'NamedNumber',Id,Val#valuedef.value}|T],Acc1,Acc2,Root);
check_enum(S,['EXTENSIONMARK'|T],Acc1,Acc2,_Root) ->
    NewAcc2 = lists:keysort(2,Acc1),
    NewList = enum_number(lists:reverse(Acc2),NewAcc2,0,[],[]),
    { NewList, check_enum(S,T,[],[],enum_counts(NewList))};
check_enum(S,[Id|T],Acc1,Acc2,Root) when is_atom(Id) ->
    check_enum(S,T,Acc1,[Id|Acc2],Root);
check_enum(_S,[],Acc1,Acc2,Root) ->
    NewAcc2 = lists:keysort(2,Acc1),
    enum_number(lists:reverse(Acc2),NewAcc2,0,[],Root).


% assign numbers to identifiers , numbers from 0 ... but must not
% be the same as already assigned to NamedNumbers
enum_number(Identifiers,NamedNumbers,Cnt,Acc,[]) ->
    enum_number(Identifiers,NamedNumbers,Cnt,Acc);
enum_number(Identifiers,NamedNumbers,_Cnt,Acc,CountL) ->
    enum_extnumber(Identifiers,NamedNumbers,Acc,CountL).

enum_number([H|T],[{Id,Num}|T2],Cnt,Acc) when Num > Cnt ->
    enum_number(T,[{Id,Num}|T2],Cnt+1,[{H,Cnt}|Acc]);
enum_number([H|T],[{Id,Num}|T2],Cnt,Acc) when Num < Cnt -> % negative Num
    enum_number(T,T2,Cnt+1,[{H,Cnt},{Id,Num}|Acc]);
enum_number([],L2,_Cnt,Acc) ->
    lists:append([lists:reverse(Acc),L2]);
enum_number(L,[{Id,Num}|T2],Cnt,Acc) -> % Num == Cnt
    enum_number(L,T2,Cnt+1,[{Id,Num}|Acc]);
enum_number([H|T],[],Cnt,Acc) ->
    enum_number(T,[],Cnt+1,[{H,Cnt}|Acc]).
    
enum_extnumber(Identifiers,NamedNumbers,Acc,[C]) ->
    check_add_enum_numbers(NamedNumbers,[C]),
    enum_number(Identifiers,NamedNumbers,C,Acc);
enum_extnumber([H|T],[{Id,Num}|T2],Acc,[C|Counts]) when Num > C ->
    enum_extnumber(T,[{Id,Num}|T2],[{H,C}|Acc],Counts);
enum_extnumber([],L2,Acc,Cnt) ->
    check_add_enum_numbers(L2, Cnt),
    lists:concat([lists:reverse(Acc),L2]);
enum_extnumber(_Identifiers,[{Id,Num}|_T2],_Acc,[C|_]) when Num < C ->
%%    enum_extnumber(Identifiers,T2,[{Id,Num}|Acc],Counts);
    exit({error,{asn1,"AdditionalEnumeration element with same number as root element",{Id,Num}}});
enum_extnumber(Identifiers,[{Id,Num}|T2],Acc,[_C|Counts]) -> % Num =:= C
    enum_extnumber(Identifiers,T2,[{Id,Num}|Acc],Counts);
enum_extnumber([H|T],[],Acc,[C|Counts]) ->
    enum_extnumber(T,[],[{H,C}|Acc],Counts).

enum_counts([]) ->
    [0];
enum_counts(L) ->
    Used=[I||{_,I}<-L],
    AddEnumLb = lists:max(Used) + 1,
    lists:foldl(fun(El,AccIn)->lists:delete(El,AccIn) end, 
		lists:seq(0,AddEnumLb),
		Used).
check_add_enum_numbers(L, Cnt) ->
    Max = lists:max(Cnt),
    Fun = fun({_,N}=El) when N < Max ->
		  case lists:member(N,Cnt) of
		      false ->
			  exit({error,{asn1,"AdditionalEnumeration element with same number as root element",El}});
		      _ ->
			  ok
		  end;
	     (_) ->
		  ok
	  end,
    lists:foreach(Fun,L).


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
	    NewComps = 
		case check_each_component(S,Type,Components2) of
		    NewComponents when is_list(NewComponents) ->
			check_unique_sequence_tags(S,NewComponents),
			NewComponents;
		    Ret = {NewComponents,NewEcomps} ->
			TagComps = NewComponents ++ 
			    [Comp#'ComponentType'{prop='OPTIONAL'}|| Comp <- NewEcomps], 
			%% extension components are like optionals when it comes to tagging
			check_unique_sequence_tags(S,TagComps),
			Ret;
		    Ret = {Root1,NewE,Root2} ->
			TagComps = Root1 ++ [Comp#'ComponentType'{prop='OPTIONAL'}|| Comp <- NewE]++Root2,
			%% This is not correct handling if Extension
			%% contains ExtensionAdditionGroups
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
	    {CRelInf,NewComps2} = componentrelation_leadingattr(S,NewComps),

	    %% CompListWithTblInf has got a lot unecessary info about
	    %% the involved class removed, as the class of the object
	    %% set.
	    CompListWithTblInf = get_tableconstraint_info(S,Type,NewComps2),

	    {CRelInf,CompListWithTblInf};
	Dupl ->
		throw({error,{asn1,{duplicate_components,Dupl}}})
    end.

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
    expand_components2(S,{undefined,oCFT_def(S,Type)});
expand_components2(S,{_,ERef}) when is_record(ERef,'Externaltypereference') ->
    expand_components2(S,get_referenced_type(S,ERef));
expand_components2(_S,Err) ->
    throw({error,{asn1,{illegal_COMPONENTS_OF,Err}}}).

take_only_rootset([])->
    [];
take_only_rootset([#'EXTENSIONMARK'{}|_T])->
    [];
take_only_rootset([H|T]) ->
    [H|take_only_rootset(T)].

check_unique_sequence_tags(S,[#'ComponentType'{prop=mandatory}|Rest]) ->
    check_unique_sequence_tags(S,Rest);
check_unique_sequence_tags(S,[C|Rest]) when is_record(C,'ComponentType') ->
    check_unique_sequence_tags1(S,Rest,[C]);% optional or default
check_unique_sequence_tags(S,[_ExtensionMarker|Rest]) ->
    check_unique_sequence_tags(S,Rest);
check_unique_sequence_tags(_S,[]) ->
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
    check_type(S,Type,Component).

check_set(S,Type,Components) ->
    {TableCInf,NewComponents} = check_sequence(S,Type,Components),
    check_distinct_tags(NewComponents,[]),
    case {lists:member(der,S#state.options),S#state.erule} of
	{true,_} ->
	    {Sorted,SortedComponents} = sort_components(der,S,NewComponents),
	    {Sorted,TableCInf,SortedComponents};
	{_,PER} when PER =:= per; PER =:= per_bin; PER =:= uper_bin ->
	    {Sorted,SortedComponents} = sort_components(per,S,NewComponents),
	    {Sorted,TableCInf,SortedComponents};
	_ ->
	    {false,TableCInf,NewComponents}
    end.


%% check that all tags are distinct according to X.680 26.3
check_distinct_tags({C1,C2,C3},Acc) when is_list(C1),is_list(C2),is_list(C3) ->
    check_distinct_tags(C1++C2++C3,Acc);
check_distinct_tags({C1,C2},Acc) when is_list(C1),is_list(C2) ->
    check_distinct_tags(C1++C2,Acc);
check_distinct_tags([#'ComponentType'{tags=[T]}|Cs],Acc) ->
    check_distinct(T,Acc),
    check_distinct_tags(Cs,[T|Acc]);
check_distinct_tags([C=#'ComponentType'{tags=[T|Ts]}|Cs],Acc) ->
    check_distinct(T,Acc),
    check_distinct_tags([C#'ComponentType'{tags=Ts}|Cs],[T|Acc]);
check_distinct_tags([#'ComponentType'{tags=[]}|_Cs],_Acc) ->
    throw({error,"Not distinct tags in SET"});
check_distinct_tags([],_) ->
    ok.
check_distinct(T,Acc) ->
    case lists:member(T,Acc) of
	true ->
	    throw({error,"Not distinct tags in SET"});
	_ -> ok
    end.

%% sorting in canonical order according to X.680 8.6, X.691 9.2
%% DER: all components shall be sorted in canonical order.
%% PER: only root components shall be sorted in canonical order. The
%%      extension components shall remain in textual order.
%%
sort_components(der,S=#state{tname=TypeName},Components) ->
    {R1,Ext,R2} = extension(textual_order(Components)),
    CompsList = case Ext of
		    noext -> R1;
		    _ -> R1 ++ Ext ++ R2
		end,
    case {untagged_choice(S,CompsList),Ext} of
	{false,noext} ->
	    {true,sort_components1(S,TypeName,CompsList,[],[],[],[])};
	{false,_} ->
	    {true,{sort_components1(S,TypeName,CompsList,[],[],[],[]), []}};
	{true,noext} ->
	    %% sort in run-time
	    {dynamic,R1};
	_ ->
	    {dynamic,{R1, Ext, R2}}
    end;
sort_components(per,S=#state{tname=TypeName},Components) ->
    {R1,Ext,R2} = extension(textual_order(Components)),
    Root = tag_untagged_choice(S,R1++R2),
    case Ext of
	noext ->
	    {true,sort_components1(S,TypeName,Root,[],[],[],[])};
	_ ->
	    {true,{sort_components1(S,TypeName,Root,[],[],[],[]),
		   Ext}}
    end.

sort_components1(S,TypeName,[C=#'ComponentType'{tags=[{'UNIVERSAL',_}|_R]}|Cs],
		 UnivAcc,ApplAcc,ContAcc,PrivAcc) ->
    sort_components1(S,TypeName,Cs,[C|UnivAcc],ApplAcc,ContAcc,PrivAcc);
sort_components1(S,TypeName,[C=#'ComponentType'{tags=[{'APPLICATION',_}|_R]}|Cs],
		 UnivAcc,ApplAcc,ContAcc,PrivAcc) ->
    sort_components1(S,TypeName,Cs,UnivAcc,[C|ApplAcc],ContAcc,PrivAcc);
sort_components1(S,TypeName,[C=#'ComponentType'{tags=[{'CONTEXT',_}|_R]}|Cs],
		 UnivAcc,ApplAcc,ContAcc,PrivAcc) ->
    sort_components1(S,TypeName,Cs,UnivAcc,ApplAcc,[C|ContAcc],PrivAcc);
sort_components1(S,TypeName,[C=#'ComponentType'{tags=[{'PRIVATE',_}|_R]}|Cs],
		 UnivAcc,ApplAcc,ContAcc,PrivAcc) ->
    sort_components1(S,TypeName,Cs,UnivAcc,ApplAcc,ContAcc,[C|PrivAcc]);
sort_components1(S,TypeName,[],UnivAcc,ApplAcc,ContAcc,PrivAcc) ->
    I = #'ComponentType'.tags,
    ascending_order_check(S,TypeName,sort_universal_type(UnivAcc)) ++
	ascending_order_check(S,TypeName,lists:keysort(I,ApplAcc)) ++
	ascending_order_check(S,TypeName,lists:keysort(I,ContAcc)) ++
	ascending_order_check(S,TypeName,lists:keysort(I,PrivAcc)).

ascending_order_check(S,TypeName,Components) ->
    ascending_order_check1(S,TypeName,Components),
    Components.

ascending_order_check1(S,TypeName,
		       [C1 = #'ComponentType'{tags=[{_,T}|_]},
			C2 = #'ComponentType'{tags=[{_,T}|_]}|Rest]) ->
    asn1ct:warning("Indistinct tag ~p in SET ~p, components ~p and ~p~n",
	      [T,TypeName,C1#'ComponentType'.name,C2#'ComponentType'.name],S),
    ascending_order_check1(S,TypeName,[C2|Rest]);
ascending_order_check1(S,TypeName,
		       [C1 = #'ComponentType'{tags=[{'UNIVERSAL',T1}|_]},
			C2 = #'ComponentType'{tags=[{'UNIVERSAL',T2}|_]}|Rest]) ->
    case (decode_type(T1) == decode_type(T2)) of
	true ->
	    asn1ct:warning("Indistinct tags ~p and ~p in"
		      " SET ~p, components ~p and ~p~n",
		      [T1,T2,TypeName,C1#'ComponentType'.name,
		       C2#'ComponentType'.name],S),
	    ascending_order_check1(S,TypeName,[C2|Rest]);
	_ ->
	    ascending_order_check1(S,TypeName,[C2|Rest])
    end;
ascending_order_check1(S,N,[_|Rest]) ->
    ascending_order_check1(S,N,Rest);
ascending_order_check1(_,_,[]) ->
    ok.
    
sort_universal_type(Components) ->
    List = lists:map(fun(C) ->
			     #'ComponentType'{tags=[{_,T}|_]} = C,
			     {decode_type(T),C}
		     end,
		     Components),
    SortedList = lists:keysort(1,List),
    lists:map(fun(X)->element(2,X) end,SortedList).

decode_type(I) when is_integer(I) ->
    I;
decode_type(T) ->
    asn1ct_gen_ber:decode_type(T).

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
    Fun = fun(C,Index) ->
		  {C#'ComponentType'{textual_order=Index},Index+1}
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
    check_type(S,Type,Component).

check_selectiontype(S,Name,#type{def=Eref}) 
  when is_record(Eref,'Externaltypereference') ->
    {RefMod,TypeDef} = get_referenced_type(S,Eref),
    NewS = S#state{module=load_asn1_module(S,RefMod),
		   mname=RefMod,
		   type=TypeDef,
		   tname=get_datastr_name(TypeDef)},
    check_selectiontype2(NewS,Name,TypeDef);
check_selectiontype(S,Name,Type=#type{def={pt,_,_}}) ->
    TName =
	case S#state.recordtopname of
	    [] ->
		S#state.tname;
	    N -> N
	end,
    TDef = #typedef{name=TName,typespec=Type},
    check_selectiontype2(S,Name,TDef);
check_selectiontype(S,Name,Type) ->
    Msg = lists:flatten(io_lib:format("SelectionType error: ~w < ~w must be a reference to a CHOICE.",[Name,Type])),
    error({type,Msg,S}).

check_selectiontype2(S,Name,TypeDef) ->
    NewS = S#state{recordtopname=get_datastr_name(TypeDef)},
    CheckedType = check_type(NewS,TypeDef,TypeDef#typedef.typespec),
    Components = get_choice_components(S,CheckedType#type.def),
    case lists:keysearch(Name,#'ComponentType'.name,Components) of
	{value,C} -> 
	    %% The selected type will have the tag of the selected type.
	    _T = C#'ComponentType'.typespec;
%	    T#type{tag=def_to_tag(NewS,T#type.def)};
	_ -> 
	    Msg = lists:flatten(io_lib:format("error checking SelectionType: ~w~n",[Name])),
	    error({type,Msg,S})
    end.
	    

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
	    %NewComps =
	    case check_each_alternative(S,Type,Components2) of
		{NewComponents,NewEcomps} ->		    
		    check_unique_tags(S,NewComponents ++ NewEcomps),
		    {NewComponents,NewEcomps};
		NewComponents ->
		    check_unique_tags(S,NewComponents),
		    NewComponents
	    end;

	Dupl ->
	    throw({error,{asn1,{duplicate_choice_alternatives,Dupl}}})
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
    case ets:info(automatic_tags) of
	undefined ->
	    %% this case when not multifile compilation
	    false;
	_ ->
%	    case ets:member(automatic_tags,Name) of
	    case ets:lookup(automatic_tags,Name) of
% 		true ->
% 		     true;
% 		_ ->
% 		    false
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
generate_automatic_tags1([ExtMark|T],[_TagNo|TagNos]) -> % EXTENSIONMARK
    [ExtMark | generate_automatic_tags1(T,TagNos)];
generate_automatic_tags1([],_) ->
    [].

any_manual_tag([#'ComponentType'{typespec=#type{tag=[]}}|Rest]) ->
    any_manual_tag(Rest);
any_manual_tag([#'EXTENSIONMARK'{}|Rest]) ->
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

collect_and_sort_tags([C|Rest],Acc) when is_record(C,'ComponentType') ->
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

check_each_component(S,Type,Components) ->
    check_each_component(S,Type,Components,[],[],[],root1).

check_each_component(S = #state{abscomppath=Path,recordtopname=TopName},Type,
		     [C|Ct],Acc,Extacc,Acc2,Ext) when is_record(C,'ComponentType') ->
    #'ComponentType'{name=Cname,typespec=Ts,prop=Prop} = C,
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
    case Ext of
	root1 ->
	    check_each_component(S,Type,Ct,[NewC|Acc],Extacc,Acc2,Ext);
	ext ->
	    check_each_component(S,Type,Ct,Acc,[NewC|Extacc],Acc2,Ext);
	root2 ->
	    check_each_component(S,Type,Ct,Acc,Extacc,[NewC|Acc2],Ext)
    end;
check_each_component(S,Type,[_|Ct],Acc,Extacc,Acc2,root1) -> % skip 'EXTENSIONMARK'
    check_each_component(S,Type,Ct,Acc,Extacc,Acc2,ext);
check_each_component(S,Type,[_|Ct],Acc,Extacc,Acc2,ext) -> % skip 'EXTENSIONMARK'
    check_each_component(S,Type,Ct,Acc,Extacc,Acc2,root2);
check_each_component(_S,_,[_C|_Ct],_,_,_,root2) -> % 'EXTENSIONMARK'
    throw({error,{asn1,{too_many_extension_marks}}});
check_each_component(_S,_,[],Acc,Extacc,_,ext) ->
    {lists:reverse(Acc),lists:reverse(Extacc)};
check_each_component(_S,_,[],Acc1,ExtAcc,Acc2,root2) ->
    {lists:reverse(Acc1),lists:reverse(ExtAcc),lists:reverse(Acc2)};
check_each_component(_S,_,[],Acc,_,_,root1) ->
    lists:reverse(Acc).

%% check_each_alternative(S,Type,{Rlist,ExtList}) ->
%%     {check_each_alternative(S,Type,Rlist),
%%      check_each_alternative(S,Type,ExtList)};
check_each_alternative(S,Type,[C|Ct]) ->
    check_each_alternative(S,Type,[C|Ct],[],[],noext).

check_each_alternative(S=#state{abscomppath=Path,recordtopname=TopName},Type,[C|Ct],
		       Acc,Extacc,Ext) when is_record(C,'ComponentType') ->
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
    Cs =
	case CompList of
	    {Comp1, EComps, Comp2} ->
		Comp1++EComps++Comp2;
	    {Components,EComponents} when is_list(Components) ->
		Components ++ EComponents;
	    CompList when is_list(CompList) ->
		CompList
	end,

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
			    case (catch get_unique_fieldname(S,#classdef{typespec=ClassDef})) of
				{error,'__undefined_',_} ->
				    no_unique;
				{asn1,Msg,_} ->
				    error({type,Msg,S});
				{'EXIT',Msg} ->
				    error({type,{internal_error,Msg},S});
				{Other,_} -> Other
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
    [get_simple_table_info1(S,Cs,AtList,[])|get_simple_table_info(S,Cs,Rest)];
get_simple_table_info(_,_,[]) ->
    [].
get_simple_table_info1(S,Cs,[Cname|Cnames],Path) when is_list(Cs) ->
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
    case constraint_member(simpletable,Cnstr) of
	{true,{simpletable,_OSRef}} ->
	    simple_table_info(S,OCFT,Path);
	_ ->
	    error({type,{"missing expected simple table constraint",
			 Cnstr},S})
    end;
get_simple_table_info1(S,#'ComponentType'{typespec=TS},Cnames,Path) ->
    Components = get_atlist_components(TS#type.def),
    get_simple_table_info1(S,Components,Cnames,Path).


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
    UniqueName =
	case (catch get_unique_fieldname(S,ClassDef)) of
	    {error,'__undefined_',_} -> no_unique;
	    {asn1,Msg,_} ->
		error({type,Msg,S});
	    {'EXIT',Msg} ->
		error({type,{internal_error,Msg},S});
	    {Other,_} -> Other
	end,
    {lists:reverse(Path),ObjectClassFieldName,UniqueName};
simple_table_info(S,Type,_) ->
    error({type,{"the type referenced by a componentrelation constraint must be a ObjectClassFieldType",Type},S}).


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
	case constraint_member(componentrelation,Type#type.constraint) of
%%	    [{componentrelation,_,AtNotation}] ->
	    {true,{_,_,AtNotation}} ->
		%% Found component relation constraint, now check
		%% whether this constraint is relevant for the level
		%% where the search started
		AtNot = extract_at_notation(AtNotation),
		%% evaluate_atpath returns the relative path to the
		%% simple table constraint from where the component
		%% relation is found.
		evaluate_atpath(S,NamePath,CNames,AtNot);
	    _ ->
		[]
	end,
    InnerAcc =
	case {Type#type.inlined,
	      asn1ct_gen:type(asn1ct_gen:get_inner(Type#type.def))} of
	    {no,{constructed,bif}} ->
		{InnerCs,NewNamePath} = 
		    case get_components(Type#type.def) of
			{IC1,_IC2} -> {IC1 ++ IC1,[CName|NamePath]};
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
	case constraint_member(componentrelation,Type#type.constraint) of
	    {true,{_,_,AtNotation}} ->
		AtNot = extract_at_notation(AtNotation),
		evaluate_atpath(S,NamePath,CNames,AtNot);
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
		any_component_relation(S,InnerCs,CNames,NamePath,[]);
	    _ ->
		[]
	end,
    InnerAcc  ++ CRelPath ++ Acc;
any_component_relation(_,[],_,_,Acc) ->
    Acc.

constraint_member(componentrelation,[CRel={componentrelation,_,_}|_Rest]) ->
    {true,CRel};
constraint_member(simpletable,[ST={simpletable,_}|_Rest]) ->
    {true,ST};
constraint_member(Key,[_H|T]) ->
    constraint_member(Key,T);
constraint_member(_,[]) ->
    false.
    
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
		_ -> 
		 %% error({type,{asn1,"failed to analyze at-path",AtPath},S})
		    throw({type,{asn1,"failed to analyze at-path",AtPath},S})
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
%do not step in inlined structures
get_components(any,{'SEQUENCE OF',T = #type{def=_Def,inlined=no}}) ->
%    get_components(any,Def);
    T;
get_components(any,{'SET OF',T = #type{def=_Def,inlined=no}}) ->
%    get_components(any,Def);
    T;
get_components(_,_) ->
    [].

get_choice_components(_S,{'CHOICE',Components}) when is_list(Components)->
    Components;
get_choice_components(_S,{'CHOICE',{C1,C2}}) when is_list(C1),is_list(C2) ->
    C1++C2;
get_choice_components(S,ERef=#'Externaltypereference'{}) ->
    {_RefMod,TypeDef}=get_referenced_type(S,ERef),
    #typedef{typespec=TS} = TypeDef,
    get_choice_components(S,TS#type.def).

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
%	case Constraint of
%	    [{componentrelation,{_,_,ObjectSet},AtList}|_Rest] ->
	case constraint_member(componentrelation,Constraint) of
	    {true,{_,{_,_,ObjectSet},AtList}} ->
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
	    _ ->
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
%	case Cons of
%	    [{componentrelation,{_,_,ObjectSet},AtList}|_Rest] ->
	case constraint_member(componentrelation,Cons) of
	    {true,{_,{_,_,ObjectSet},AtList}} ->
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

get_unique_fieldname(_S,ClassDef) when is_record(ClassDef,classdef) ->
%%    {_,Fields,_} = ClassDef#classdef.typespec,
    Fields = (ClassDef#classdef.typespec)#objectclass.fields,
    get_unique_fieldname1(Fields,[]);
get_unique_fieldname(S,#typedef{typespec=#type{def=ClassRef}}) ->
    %% A class definition may be referenced as
    %% REFED-CLASS ::= DEFINED-CLASS and then REFED-CLASS is a typedef
    {_M,ClassDef} = get_referenced_type(S,ClassRef),
    get_unique_fieldname(S,ClassDef).

get_unique_fieldname1([],[]) ->
    throw({error,'__undefined_',[]});
get_unique_fieldname1([],[Name]) ->
    Name;
get_unique_fieldname1([],Acc) ->
    throw({asn1,'only one UNIQUE field is allowed in CLASS',Acc});
get_unique_fieldname1([{fixedtypevaluefield,Name,_,'UNIQUE',Opt}|Rest],Acc) ->
    get_unique_fieldname1(Rest,[{Name,Opt}|Acc]);
get_unique_fieldname1([_H|T],Acc) ->
    get_unique_fieldname1(T,Acc).

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
get_tableconstraint_info(S,Type,[C|Cs],Acc) ->
    CheckedTs = C#'ComponentType'.typespec,
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
    get_tableconstraint_info(S,Type,Cs,[AccComp|Acc]).

get_referenced_fieldname([{_,FirstFieldname}]) ->
    {FirstFieldname,[]};
get_referenced_fieldname([{_,FirstFieldname}|Rest]) ->
    {FirstFieldname,lists:map(fun(X)->element(2,X) end,Rest)};
get_referenced_fieldname(Def={FieldName,RestFieldName}) when is_atom(FieldName),is_list(RestFieldName)->
    Def;
get_referenced_fieldname(Def) ->
    {no_type,Def}.

%% get_ObjectClassFieldType extracts the type from the chain of
%% objects that leads to a final type.
get_ObjectClassFieldType(S,ERef,PrimFieldNameList) when
  is_record(ERef,'Externaltypereference') ->
    {MName,Type} = get_referenced_type(S,ERef),
    NewS = update_state(S#state{type=Type,
		   tname=ERef#'Externaltypereference'.type},MName),
    ClassSpec = check_class(NewS,Type),
    Fields = ClassSpec#objectclass.fields,
    get_ObjectClassFieldType(S,Fields,PrimFieldNameList);
get_ObjectClassFieldType(S,Fields,L=[_PrimFieldName1|_Rest]) ->
    check_PrimitiveFieldNames(S,Fields,L),
    get_OCFType(S,Fields,L);
get_ObjectClassFieldType(S,ERef,{FieldName,Rest}) ->
    get_ObjectClassFieldType(S,ERef,Rest ++ [FieldName]).

check_PrimitiveFieldNames(_S,_Fields,_) ->
    ok.

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
	    NewS = update_state(S#state{type=ClassDef,
					tname=get_datastr_name(ClassDef)},
				MName),
	    CheckedCDef = check_class(NewS,ClassDef),
	    get_OCFType(S,CheckedCDef#objectclass.fields,Rest);
	{value,{objectsetfield,_,Type,_OptSpec}} ->
	    {MName,ClassDef} = get_referenced_type(S,Type#type.def),
	    NewS = update_state(S#state{type=ClassDef,
					tname=get_datastr_name(ClassDef)},
				MName),
	    CheckedCDef = check_class(NewS,ClassDef),
	    get_OCFType(S,CheckedCDef#objectclass.fields,Rest);

	{value,Other} ->
	    {element(1,Other),PrimFieldName};
	_  ->
	    throw({error,lists:flatten(io_lib:format("undefined FieldName in ObjectClassFieldType: ~w",[PrimFieldName]))})
    end.

get_taglist(S,Ext) when is_record(Ext,'Externaltypereference') ->
    {_,T} = get_referenced_type(S,Ext),
    get_taglist(S,T#typedef.typespec);
get_taglist(S,Tref) when is_record(Tref,typereference) ->
    {_,T} = get_referenced_type(S,Tref),
    get_taglist(S,T#typedef.typespec);
get_taglist(S,Type) when is_record(Type,type) ->
    case Type#type.tag of
	[] ->
	    get_taglist(S,Type#type.def);
	[Tag|_]  ->
	    [asn1ct_gen:def_to_tag(Tag)]
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
  when is_list(FieldNameList) ->
    case get_ObjectClassFieldType(S,ERef,FieldNameList) of
	{fixedtypevaluefield,_,Type} -> get_taglist(S,Type);
	{TypeFieldName,_} when is_atom(TypeFieldName) -> []%should check if allowed
    end;
get_taglist(S,{ObjCl,FieldNameList}) when is_record(ObjCl,objectclass),
					  is_list(FieldNameList) ->
    case get_ObjectClassFieldType(S,ObjCl#objectclass.fields,FieldNameList) of
	{fixedtypevaluefield,_,Type} -> get_taglist(S,Type);
	{TypeFieldName,_} when is_atom(TypeFieldName) -> []%should check if allowed
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


dbget_ex(_S,Module,Key) ->
    case asn1_db:dbget(Module,Key) of
	undefined ->
	    
	    throw({error,{asn1,{undefined,{Module,Key}}}}); % this is catched on toplevel type or value
	T -> T
    end.

merge_tags(T1, T2) when is_list(T2) ->
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

%% merge_constraints(C1, []) ->
%%     C1;
%% merge_constraints([], C2) ->
%%     C2;
%% merge_constraints(C1, C2) ->
%%     {SList,VList,PAList,Rest} = splitlist(C1++C2,[],[],[],[]),
%%     SizeC = merge_constraints(SList),
%%     ValueC = merge_constraints(VList),
%%     PermAlphaC = merge_constraints(PAList),
%%     case Rest of
%%         [] ->
%%             SizeC ++ ValueC ++ PermAlphaC;
%%         _ ->
%%             throw({error,{asn1,{not_implemented,{merge_constraints,Rest}}}})
%%     end.
    
%% merge_constraints([]) -> [];
%% merge_constraints([C1 = {_,{Low1,High1}},{_,{Low2,High2}}|Rest]) when Low1 >= Low2,
%%                                                                       High1 =< High2 ->
%%     merge_constraints([C1|Rest]);
%% merge_constraints([C1={'PermittedAlphabet',_},C2|Rest]) ->
%%     [C1|merge_constraints([C2|Rest])];
%% merge_constraints([C1 = {_,{_Low1,_High1}},C2 = {_,{_Low2,_High2}}|_Rest]) ->
%%     throw({error,asn1,{conflicting_constraints,{C1,C2}}});
%% merge_constraints([C]) ->
%%     [C].

%% splitlist([C={'SizeConstraint',_}|Rest],Sacc,Vacc,PAacc,Restacc) ->
%%     splitlist(Rest,[C|Sacc],Vacc,PAacc,Restacc);
%% splitlist([C={'ValueRange',_}|Rest],Sacc,Vacc,PAacc,Restacc) ->
%%     splitlist(Rest,Sacc,[C|Vacc],PAacc,Restacc);
%% splitlist([C={'PermittedAlphabet',_}|Rest],Sacc,Vacc,PAacc,Restacc) ->
%%     splitlist(Rest,Sacc,Vacc,[C|PAacc],Restacc);
%% splitlist([C|Rest],Sacc,Vacc,PAacc,Restacc) ->
%%     splitlist(Rest,Sacc,Vacc,PAacc,[C|Restacc]);
%% splitlist([],Sacc,Vacc,PAacc,Restacc) ->
%%     {lists:reverse(Sacc),
%%      lists:reverse(Vacc),
%%      lists:reverse(PAacc),
%%      lists:reverse(Restacc)}.



storeindb(S,M) when is_record(M,module) ->
    TVlist = M#module.typeorval,
    NewM = M#module{typeorval=findtypes_and_values(TVlist)},
    asn1_db:dbnew(NewM#module.name),
    asn1_db:dbput(NewM#module.name,'MODULE',  NewM),
    Res = storeindb(NewM#module.name,TVlist,[]),
    include_default_class(S,NewM#module.name),
    include_default_type(NewM#module.name),
    Res.

storeindb(Module,[H|T],ErrAcc) when is_record(H,typedef) ->
    storeindb(Module,H#typedef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when is_record(H,valuedef) ->
    storeindb(Module,H#valuedef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when is_record(H,ptypedef) ->
    storeindb(Module,H#ptypedef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when is_record(H,classdef) ->
    storeindb(Module,H#classdef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when is_record(H,pvaluesetdef) ->
    storeindb(Module,H#pvaluesetdef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when is_record(H,pobjectdef) ->
    storeindb(Module,H#pobjectdef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when is_record(H,pvaluedef) ->
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
		_Type when is_record(H,typedef) ->
		    error({type,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		_Type when is_record(H,valuedef) ->
		    error({value,"already defined",
			   #state{mname=Module,value=H,vname=Name}});
		_Type when is_record(H,ptypedef) ->
		    error({ptype,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		_Type when is_record(H,pobjectdef) ->
		    error({ptype,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		_Type when is_record(H,pvaluesetdef) ->
		    error({ptype,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		_Type when is_record(H,pvaluedef) ->
		    error({ptype,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		_Type when is_record(H,classdef) ->
		    error({class,"already defined",
			   #state{mname=Module,value=H,vname=Name}})
	    end,
	    storeindb(Module,T,[H|ErrAcc])
    end.

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
    


error({export,Msg,#state{mname=Mname,type=Ref,tname=Typename}}) ->
    Pos = Ref#'Externaltypereference'.pos,
    io:format("asn1error:~p:~p:~p~n~p~n",[Pos,Mname,Typename,Msg]),
    {error,{export,Pos,Mname,Typename,Msg}};
error({import,Msg,#state{mname=Mname,type=Ref,tname=Typename}}) ->
    PosOfDef =
	fun(#'Externaltypereference'{pos=P}) -> P;
	   (#'Externalvaluereference'{pos=P}) -> P
	end,
    Pos = PosOfDef(Ref),
    io:format("asn1error:~p:~p:~p~n~p~n",[Pos,Mname,Typename,Msg]),
    {error,{import,Pos,Mname,Typename,Msg}};
% error({type,{Msg1,Msg2},#state{mname=Mname,type=Type,tname=Typename}}) 
%   when is_record(Type,typedef) ->
%     io:format("asn1error:~p:~p:~p ~p~n",
% 	      [Type#typedef.pos,Mname,Typename,Msg1]),
%     {error,{type,Type#typedef.pos,Mname,Typename,Msg1,Msg2}};
error({type,Msg,#state{mname=Mname,type=Type,tname=Typename}}) 
  when is_record(Type,type) ->
    io:format("asn1error:~p:~p~n~p~n",
	      [Mname,Typename,Msg]),
    {error,{type,Mname,Typename,Msg}};
error({type,Msg,#state{mname=Mname,type=Type,tname=Typename}}) 
  when is_record(Type,typedef) ->
    io:format("asn1error:~p:~p:~p~n~p~n",
	      [Type#typedef.pos,Mname,Typename,Msg]),
    {error,{type,Type#typedef.pos,Mname,Typename,Msg}};
error({type,Msg,#state{mname=Mname,type=Type,tname=Typename}}) 
  when is_record(Type,ptypedef) ->
    io:format("asn1error:~p:~p:~p~n~p~n",
	      [Type#ptypedef.pos,Mname,Typename,Msg]),
    {error,{type,Type#ptypedef.pos,Mname,Typename,Msg}};
error({type,Msg,#state{mname=Mname,value=Value,vname=Valuename}})
  when is_record(Value,valuedef) ->
    io:format("asn1error:~p:~p:~p~n~p~n",[Value#valuedef.pos,Mname,Valuename,Msg]),
    {error,{type,Value#valuedef.pos,Mname,Valuename,Msg}};
error({type,Msg,#state{mname=Mname,type=Type,tname=Typename}}) 
  when is_record(Type,pobjectdef) ->
    io:format("asn1error:~p:~p:~p~n~p~n",
	      [Type#pobjectdef.pos,Mname,Typename,Msg]),
    {error,{type,Type#pobjectdef.pos,Mname,Typename,Msg}};
error({value,Msg,#state{mname=Mname,value=Value,vname=Valuename}}) 
  when is_record(Value,valuedef) ->
    io:format("asn1error:~p:~p:~p~n~p~n",[Value#valuedef.pos,Mname,Valuename,Msg]),
    {error,{value,Value#valuedef.pos,Mname,Valuename,Msg}};
error({Other,Msg,#state{mname=Mname,value=#valuedef{pos=Pos},vname=Valuename}}) ->
    io:format("asn1error:~p:~p:~p~n~p~n",[Pos,Mname,Valuename,Msg]),
    {error,{Other,Pos,Mname,Valuename,Msg}};
error({Other,Msg,#state{mname=Mname,type=#typedef{pos=Pos},tname=Typename}}) ->
    io:format("asn1error:~p:~p:~p~n~p~n",[Pos,Mname,Typename,Msg]),
    {error,{Other,Pos,Mname,Typename,Msg}};
error({Other,Msg,#state{mname=Mname,type=#classdef{pos=Pos},tname=Typename}}) ->
    io:format("asn1error:~p:~p:~p~n~p~n",[Pos,Mname,Typename,Msg]),
    {error,{Other,Pos,Mname,Typename,Msg}};
error({Other,Msg,#state{mname=Mname,type=Type,tname=Typename}}) ->
    io:format("asn1error:~p:~p:~p~n~p~n",[asn1ct:get_pos_of_def(Type),Mname,Typename,Msg]),
    {error,{Other,asn1ct:get_pos_of_def(Type),Mname,Typename,Msg}}.

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


include_default_class(S,Module) ->
    NameAbsList = default_class_list(S),
    include_default_class1(Module,NameAbsList).

include_default_class1(_,[]) ->
    ok;
include_default_class1(Module,[{Name,TS}|Rest]) ->
    case asn1_db:dbget(Module,Name) of
	undefined ->
	    C = #classdef{checked=true,name=Name,
			  typespec=TS},
	    asn1_db:dbput(Module,Name,C);
	_ -> ok
    end,
    include_default_class1(Module,Rest).

default_class_list(S) ->
    [{'TYPE-IDENTIFIER',
      {objectclass,
       [{fixedtypevaluefield,
	 id,
	 #type{tag=?TAG_PRIMITIVE(?N_OBJECT_IDENTIFIER),
	       def='OBJECT IDENTIFIER'},
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
	 #type{tag=?TAG_PRIMITIVE(?N_OBJECT_IDENTIFIER),
	       def='OBJECT IDENTIFIER'},
	 'UNIQUE',
	 'MANDATORY'},
	{typefield,'Type','MANDATORY'},
	{fixedtypevaluefield,
	 property,
	 #type{tag=?TAG_PRIMITIVE(?N_BIT_STRING),
	       def={'BIT STRING',[]}},
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
