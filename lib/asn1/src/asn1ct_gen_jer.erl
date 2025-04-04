%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2019-2025. All Rights Reserved.
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
-module(asn1ct_gen_jer).
-moduledoc false.

%% Generate erlang module which handles (PER) encode and decode for
%% all types in an ASN.1 module

-include("asn1_records.hrl").

-export([gen_encode/2,gen_encode/3,gen_decode/2,gen_decode/3]).
-export([gen_encode_prim/3]).
-export([gen_dec_prim/2]).
-export([gen_objectset_code/2, gen_obj_code/3]).
-export([gen_inc_decode/2,gen_decode_selected/3]).
-export([extaddgroup2sequence/1]).
-export([dialyzer_suppressions/1]).

-export([gen_encode_constructed/4]).
-export([gen_encode_sequence/3]).
-export([gen_decode_sequence/3]).
-export([gen_encode_set/3]).
-export([gen_decode_set/3]).
-export([gen_encode_sof/4]).
-export([gen_decode_sof/4]).
-export([gen_encode_choice/3]).
-export([gen_decode_choice/3]).


-import(asn1ct_gen, [emit/1]).



%%==========================================================================
%%  Encode/decode SEQUENCE (and SET)
%%==========================================================================

gen_encode_sequence(Gen, Typename, #type{}=D) ->
    
    {_SeqOrSet,TableConsInfo,CompList0} =
	case D#type.def of
	    #'SEQUENCE'{tablecinf=TCI,components=CL} -> 
		{'SEQUENCE',TCI,CL};
	    #'SET'{tablecinf=TCI,components=CL} -> 
		{'SET',TCI,CL}
	end,
    %% filter away extensionAdditiongroup markers
    CompList = filter_complist(CompList0),
    CompList1 = case CompList of
		    {Rl1,El,Rl2} -> Rl1 ++ El ++ Rl2;
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,

    %%    enc_match_input(Gen, ValName, CompList1),

    EncObj =
	case TableConsInfo of
	    #simpletableattributes{usedclassfield=Used,
				   uniqueclassfield=Unique} when Used /= Unique ->
		false;
	    %% ObjectSet, name of the object set in constraints
	    #simpletableattributes{objectsetname=ObjectSetRef,
				   c_name=AttrN,
				   c_index=N,
				   usedclassfield=UniqueFieldName,
				   uniqueclassfield=UniqueFieldName,
				   valueindex=_ValueIndex} -> %% N is index of attribute that determines constraint
		{ObjSetMod,ObjSetName} = ObjectSetRef,
		OSDef = asn1_db:dbget(ObjSetMod, ObjSetName),
		case (OSDef#typedef.typespec)#'ObjectSet'.gen of
		    true ->
			{AttrN,N};
		    _ ->
			false
		end;
	    _ ->
		case D#type.tablecinf of
		    [{objfun,_}|_] ->
			%% when the simpletableattributes was at an outer
			%% level and the objfun has been passed through the
			%% function call
			{"got objfun through args","ObjFun"};
		    _ ->
			false
		end
	end,
    CompTypes = gen_enc_comptypes(Gen, Typename, CompList1, 1, EncObj, []),
    Prefix = asn1ct_gen:get_record_name_prefix(Gen),
    {SequenceTag,NewCompTypes} = case Gen#gen.pack of
        map -> {sequence_map,[{BinName,binary_to_atom(BinName),Type,OptOrDefault}||{BinName,Type,OptOrDefault} <- CompTypes]};
        _ -> {sequence,CompTypes} 
    end,
    {SequenceTag,
     list_to_atom(lists:concat([Prefix,asn1ct_gen:list2name(Typename)])),
     length(CompList1),NewCompTypes}.

gen_decode_sequence(_,_,_) -> ok.


%%============================================================================
%%  Encode/decode SET
%%============================================================================

gen_encode_set(Erules,Typename,D) when is_record(D,type) ->
    gen_encode_sequence(Erules,Typename,D).

gen_decode_set(_,_,_) -> ok.


%%===============================================================================
%%  Encode/decode SEQUENCE OF and SET OF
%%===============================================================================

gen_encode_sof(Erules,Typename,InnerTypename,D) when is_record(D,type) ->
    asn1ct_name:start(),
    {_SeqOrSetOf, Cont} = D#type.def,

%%    Objfun = case D#type.tablecinf of
%%		 [{objfun,_}|_R] ->
%%		     ", ObjFun";
%%		 _ ->
%%		     ""
%%	     end,

%%    emit(["   EncV = 'enc_",asn1ct_gen:list2name(Typename),
%%	  "_components'(Val",Objfun,",[]).",nl,nl]),
    NameSuffix = asn1ct_gen:constructed_suffix(InnerTypename,D#type.def),
    {sof,gen_typeinfo(Erules,[NameSuffix|Typename],Cont)}.
    
gen_decode_sof(_,_,_,_) -> ok.

%%============================================================================
%%  Encode/decode CHOICE
%%
%%============================================================================

gen_encode_choice(Erules,TypeName,D) when is_record(D,type) ->
    {'CHOICE',CompList} = D#type.def,
    CompList1 = case CompList of
		    {Rl1,El,Rl2} -> Rl1 ++ El ++ Rl2;
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    {choice,#{AltName => AltType ||
                {AltName,AltType,_OptOrMand} <-
                    gen_enc_comptypes(Erules,TypeName,CompList1,0,0,[])}}.

gen_decode_choice(_,_,_) -> ok.


%%============================================================================
%%  Encode SEQUENCE
%%
%%============================================================================

gen_enc_comptypes(Erules,TopType,[#'ComponentType'{name=Cname,typespec=Type,prop=Prop}|Rest],Pos,EncObj,Acc) ->
    TypeInfo = 
        gen_enc_line(Erules,TopType,Cname,Type,"Dummy",
                                    3,Prop,EncObj),
    gen_enc_comptypes(Erules,TopType,Rest,Pos,EncObj,[{atom_to_binary(Cname,utf8),TypeInfo,Prop}|Acc]);
gen_enc_comptypes(_,_,[],_,_,Acc) ->
    lists:reverse(Acc).

gen_enc_classtypes(Erules,TopType,[{TName,#typedef{typespec=TSpec}}|Rest],Acc) ->
    TypeInfo = 
        gen_enc_line(Erules,TopType,TName,TSpec,"Dummy",
                                    3,mandatory,false),
    gen_enc_classtypes(Erules,TopType,Rest,[{TName,TypeInfo}|Acc]);
gen_enc_classtypes(_,_,[],Acc) ->
    lists:reverse(Acc).

%%============================================================================
%%  Decode SEQUENCE
%%
%%============================================================================

gen_enc_line(Erules,TopType,Cname,
	     Type=#type{constraint=C,
			def=#'ObjectClassFieldType'{type={typefield,_}}},
	     Element,Indent,OptOrMand=mandatory,EncObj) 
  when is_list(Element) ->
    case asn1ct_gen:get_constraint(C,componentrelation) of
	{componentrelation,_,_} ->
	    gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,
			 ["{",{curr,tmpBytes},",_} = "],EncObj);
	_ ->
	    gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,
			 ["{",{curr,encBytes},",",{curr,encLen},"} = "],
			 EncObj)
    end;
gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,EncObj) 
  when is_list(Element) ->
    gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,
		 [{curr,encV}," = "],EncObj).

gen_enc_line(Erules,TopType,Cname,Type,Element,_Indent,_OptOrMand,_Assign,EncObj)
  when is_list(Element) ->
    InnerType = case Type of
                    #type{def=Def} -> 
                        asn1ct_gen:get_inner(Def);
                    #'ObjectSet'{class=ExtRef} ->
                        asn1ct_gen:get_inner(ExtRef)
                end,
    WhatKind = asn1ct_gen:type(InnerType),
%%    emit(IndDeep),
%%    emit(Assign),
%%    gen_optormand_case(OptOrMand, Erules, TopType, Cname, Type, Element),
        CR = case Type of 
                 #type{constraint = Constraint} ->
                     asn1ct_gen:get_constraint(Constraint,componentrelation);
                 _ ->
                     []
             end,
    TypeInfo =
        case {Type,CR} of
            {#type{def=#'ObjectClassFieldType'{type={typefield,_},
                                               fieldname=RefedFieldName}},
             {componentrelation,_,_}} ->
                {Name,_RestFieldNames} = RefedFieldName,
                true = is_atom(Name),                %Assertion.
                {'ObjClassFieldType',EncObj,CR};
            _ ->
                case WhatKind of
                    {primitive,bif} ->
                        gen_encode_prim(jer, Type, Element);
                    'ASN1_OPEN_TYPE' ->
                        case Type#type.def of
                            #'ObjectClassFieldType'{} -> %Open Type
                                gen_encode_prim(jer,#type{def='ASN1_OPEN_TYPE'},Element);
                            _ ->
                                gen_encode_prim(jer,Type, Element)
                        end;
                    {constructed,bif} ->
                        Typename = [Cname|TopType],
                        gen_encode_constructed(Erules,Typename,InnerType,Type);
                    #'Externaltypereference'{module=Mod,type=EType} ->
                        {typeinfo,{Mod,typeinfo_func(EType)}}

%%                     _ ->

%% %%                            mkfuncname(TopType,Cname,InnerType,WhatKind,"typeinfo_",""),
%%                         case {WhatKind,Type#type.tablecinf,EncObj} of
%%                             {{constructed,bif},[{objfun,_}|_R],{_,Fun}} ->
%%                                 emit([EncFunName,"(",Element,
%%                                       ", ",Fun,")"]);
%%                             _ ->
%%                                 {typeinfo,EncFunName}
%%
%%                        end
                end
        end,
    TypeInfo.

%%------------------------------------------------------
%% General and special help functions (not exported)
%%------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% filter away ExtensionAdditionGroup start and end marks since these
%% have no significance for the JER encoding
%%
filter_complist(CompList) when is_list(CompList) ->
    lists:filter(fun(#'ExtensionAdditionGroup'{}) ->
			 false;
		    ('ExtensionAdditionGroupEnd') ->
			 false;
		    (_) ->
			 true
		 end, CompList);
filter_complist({Root,Ext}) ->
    {Root,filter_complist(Ext)};
filter_complist({Root1,Ext,Root2}) ->
    {Root1,filter_complist(Ext),Root2}.

%%name2bin(TypeName) ->
%%    NameAsList = asn1ct_gen:list2name(TypeName),
%%    list_to_binary(NameAsList).

gen_encode_constructed(Erules,Typename,InnerType,D) when is_record(D,type) ->
    case InnerType of
	'SET' ->
	    gen_encode_set(Erules,Typename,D);
	'SEQUENCE' ->
	    gen_encode_sequence(Erules,Typename,D);
	'CHOICE' ->
	    gen_encode_choice(Erules,Typename,D);
	'SEQUENCE OF' ->
	    gen_encode_sof(Erules,Typename,InnerType,D);
	'SET OF' ->
	    gen_encode_sof(Erules,Typename,InnerType,D)
    end.


%% empty_lb(#gen{erule=jer}) ->
%%     null.

%% value_match(#gen{pack=record}, VIs, Value) ->
%%     value_match_rec(VIs, Value);
%% value_match(#gen{pack=map}, VIs, Value) ->
%%     value_match_map(VIs, Value).

%% value_match_rec([], Value) ->
%%     Value;
%% value_match_rec([{VI,_}|VIs], Value0) ->
%%     Value = value_match_rec(VIs, Value0),
%%     lists:concat(["element(",VI,", ",Value,")"]).

%% value_match_map([], Value) ->
%%     Value;
%% value_match_map([{_,Name}|VIs], Value0) ->
%%     Value = value_match_map(VIs, Value0),
%%     lists:concat(["maps:get(",Name,", ",Value,")"]).

%% call(F, Args) ->
%%     asn1ct_func:call(jer, F, Args).

%%===============================================================================
%%===============================================================================
%%===============================================================================
%% Generate ENCODING
%%===============================================================================
%%===============================================================================
%%===============================================================================

dialyzer_suppressions(_) ->
    case asn1ct:use_legacy_types() of
	false -> ok;
	true -> suppress({ber,encode_bit_string,4})
    end,
    suppress({ber,decode_selective,2}),
    emit(["    ok.",nl]).

suppress({M,F,A}=MFA) ->
    case asn1ct_func:is_used(MFA) of
	false ->
	    ok;
	true ->
	    Args = [lists:concat(["element(",I,", Arg)"]) || I <- lists:seq(1, A)],
	    emit(["    ",{call,M,F,Args},com,nl])
    end.

%%===============================================================================
%% encode #{typedef, {pos, name, typespec}}
%%===============================================================================

gen_encode(Erules, #typedef{}=D) ->
    gen_encode_user(Erules, D, true).

%%===============================================================================
%% encode #{type, {tag, def, constraint}}
%%===============================================================================

gen_encode(Erules,Typename,Type) when is_record(Type,type) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    ObjFun =
	case lists:keysearch(objfun,1,Type#type.tablecinf) of
	    {value,{_,_Name}} ->
		", ObjFun";
	    false ->
		""
	end,

    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
            Func = {asis,enc_func(asn1ct_gen:list2name(Typename))},
	    emit([nl,nl,nl,"%%================================",nl,
                  "%%  ",asn1ct_gen:list2name(Typename),nl,
                  "%%================================",nl,
                  Func,"(Val",ObjFun,") ->",nl,
                  "   "]),
	    TypeInfo = gen_encode_constructed(Erules,Typename,InnerType,Type),
            emit([{asis,TypeInfo},".",nl]);
	_ ->
	    true
    end;

%%===============================================================================
%% encode ComponentType
%%===============================================================================

gen_encode(Erules,Tname,#'ComponentType'{name=Cname,typespec=Type}) ->
    NewTname = [Cname|Tname],
    %% The tag is set to [] to avoid that it is
    %% taken into account twice, both as a component/alternative (passed as
    %% argument to the encode decode function and within the encode decode
    %% function it self.
    NewType = Type#type{tag=[]},
    gen_encode(Erules,NewTname,NewType).

gen_encode_user(Erules, #typedef{}=D, _Wrapper) ->
    Typename = [D#typedef.name],
    Type = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    emit([typeinfo,"('",asn1ct_gen:list2name(Typename),"') ->",nl]),
    CurrentMod = get(currmod),
    TypeInfo = 
        case asn1ct_gen:type(InnerType) of
            {constructed,bif} ->
                gen_encode_constructed(Erules,Typename,InnerType,Type);
            {primitive,bif} ->
                gen_encode_prim(jer,Type,"Val");
            #'Externaltypereference'{module=CurrentMod,type=Etype} ->
                {typeinfo,{CurrentMod,typeinfo_func(Etype)}};
            #'Externaltypereference'{module=Emod,type=Etype} ->
                {typeinfo,{Emod,typeinfo_func(Etype)}};
            'ASN1_OPEN_TYPE' ->	    
                gen_encode_prim(jer,
                                Type#type{def='ASN1_OPEN_TYPE'},
                                "Val")
        end,
    emit(["  ",{asis,TypeInfo},";",nl]).

gen_typeinfo(Erules, Typename, Type) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    CurrentMod = get(currmod),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    gen_encode_constructed(Erules,Typename,InnerType,Type);
	{primitive,bif} ->
	    gen_encode_prim(jer,Type,"Val");
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    {typeinfo,{CurrentMod,typeinfo_func(Etype)}};
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    {typeinfo,{Emod,typeinfo_func(Etype)}};
	'ASN1_OPEN_TYPE' ->
	    gen_encode_prim(jer,
			    Type#type{def='ASN1_OPEN_TYPE'},
			    "Val")
    end.

gen_encode_prim(Erules, #type{constraint=C}=D, _Value) ->
    SizeConstraint = get_size_constraint(C),
    IntConstr = int_constr(C),
    Containing = containing_constraint(Erules, C),

    asn1ct_name:new(enumval),
    Type = case D#type.def of
	       'OCTET STRING'    -> maybe_legacy_octet_string(SizeConstraint);
               'UTF8String'      -> string;
	       'ObjectDescriptor'-> string;
	       'NumericString'   -> string;
	       'TeletexString'   -> string;
	       'T61String'       -> string;
	       'VideotexString'  -> string;
	       'GraphicString'   -> string;
	       'VisibleString'   -> string;
	       'GeneralString'   -> string;
	       'PrintableString' -> string;
	       'IA5String'       -> string;
	       'UTCTime'         -> string;
	       'GeneralizedTime' -> string;
               B1 = 'BIT STRING' -> maybe_legacy_bit_string(B1,SizeConstraint);
               B2 = {'BIT STRING',_NNL} -> 
                   maybe_legacy_bit_string(B2,SizeConstraint);
               {'INTEGER',NNL} -> {'INTEGER_NNL',NNL};
               {'ENUMERATED',{NNL,Ext}} -> {'ENUMERATED_EXT',maps:from_list(NNL++Ext)};
               {'ENUMERATED',NNL} -> {'ENUMERATED',maps:from_list(NNL)};
	       Other             -> Other
	   end,
    case {IntConstr,Containing} of
        {[],[]} ->
            Type;
        {_,[]} ->
            {Type,IntConstr};
        {[],_} ->
            {container,Type,Containing}
    end.

maybe_legacy_octet_string(SizeConstraint) ->
    Type = case asn1ct:use_legacy_types() of
               true ->
                   legacy_octet_string;
               false ->
                   octet_string
           end,
    case SizeConstraint of
        [] ->
            Type;
        _ ->
            {Type,SizeConstraint}
    end.

maybe_legacy_bit_string(BitStrType,SizeConstraint) ->
    Type = case asn1ct:get_bit_string_format() of
               bitstring ->
                   bit_string;
               compact ->
                   compact_bit_string;
               legacy ->
                   legacy_bit_string
           end,
    Type1 = case BitStrType of
                {'BIT STRING',[]} -> 
                    Type;
                'BIT STRING' -> 
                    Type;
                {'BIT STRING',NNL} -> 
                    {list_to_atom(lists:concat([Type,"_nnl"])),NNL}
            end,
    case SizeConstraint of
        [] ->
            Type1;
        _ ->
            {Type1,SizeConstraint}
    end.

%%===========================================================================
%% Generate DECODING
%%===========================================================================
%% dummy functions because we don't generate anything special for decode 
%%===========================================================================

gen_decode(_,_) -> ok.

gen_inc_decode(_Erules,_Type) -> ok.

%% gen_decode_selected exported function for selected decode
%% Is not supported and should not be called for JER
gen_decode_selected(_Erules,_Type,_FuncName) -> ok. 

gen_decode(_,_,_) -> ok.

gen_dec_prim(_Att, _BytesVar) -> ok.

int_constr(C) ->
    case asn1ct_imm:effective_constraint(integer, C) of
	[{_,[]}] ->
	    %% Extension - ignore constraint.
	    [];
	[{'ValueRange',{'MIN',_}}] ->
	    %% Tricky to implement efficiently - ignore it.
	    [];
	[{'ValueRange',{_,_}=Range}] ->
	    Range;
	[{'SingleValue',Sv}] ->
	    Sv;
	[] ->
	    []
    end.

containing_constraint(Erules, [{contentsconstraint,#type{def=Def}=OuterType,[]}|_]) ->
    %% This is a CONTAINING constraint without an ENCODED BY clause.
    InnerType = asn1ct_gen:get_inner(Def),
    case asn1ct_gen:type(InnerType) of
        #'Externaltypereference'{module=Mod,type=TypeName} ->
            {typeinfo,{Mod,typeinfo_func(TypeName)}};
        {primitive,bif} ->
	    gen_encode_prim(Erules, OuterType, "Val");
        _ ->
            []
    end;
containing_constraint(Erules, [_|Cs]) ->
    containing_constraint(Erules, Cs);
containing_constraint(_Erules, []) ->
    [].

gen_obj_code(_Erules,_Module,_Obj) -> ok.

gen_objectset_code(Erules,ObjSet) ->
    ObjSetName = ObjSet#typedef.name,
    Def = ObjSet#typedef.typespec,
    Set = Def#'ObjectSet'.set,
    TypeName = {asis,typeinfo_func(asn1ct_gen:list2name([ObjSetName]))},
    SelectValMap =
        maps:from_list([{SelectVal,
                         maps:from_list(
                           gen_enc_classtypes(Erules,ObjSetName,
                                             [TNameType || TNameType = {_TypeName,#typedef{}} <-TypeList],
                                             []))} || {_,SelectVal,TypeList} <- Set]),
    emit([typeinfo,"(",TypeName,") ->",nl]),
    emit(["  ",{asis,SelectValMap},";",nl]).


get_size_constraint(C) ->
    case lists:keyfind('SizeConstraint', 1, C) of
	false -> [];
	{_,{_,[]}} -> [];			%Extensible.
	{_,{Sv,Sv}} -> Sv;
	{_,{_,_}=Tc} -> Tc
    end.
 

%% For BER the ExtensionAdditionGroup notation has no impact on the
%% encoding/decoding. Therefore we can filter away the
%% ExtensionAdditionGroup start and end markers.
extaddgroup2sequence(ExtList) when is_list(ExtList) ->
    lists:filter(fun(#'ExtensionAdditionGroup'{}) ->
			 false;
		    ('ExtensionAdditionGroupEnd') ->
			 false;
		    (_) ->
			 true
		 end, ExtList).

typeinfo_func(Tname) ->
    Tname.

enc_func(Tname) ->
    list_to_atom(lists:concat(["enc_",Tname])).
