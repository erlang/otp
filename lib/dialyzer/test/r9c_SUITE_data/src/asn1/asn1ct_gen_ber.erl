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
%%     $Id: asn1ct_gen_ber.erl,v 1.1 2008/12/17 09:53:29 mikpe Exp $
-module(asn1ct_gen_ber).

%% Generate erlang module which handles (PER) encode and decode for
%% all types in an ASN.1 module

-include("asn1_records.hrl").

-export([pgen/4]).
-export([decode_class/1, decode_type/1]).
-export([add_removed_bytes/0]).
-export([gen_encode/2,gen_encode/3,gen_decode/2,gen_decode/3]).
-export([gen_encode_prim/4]).
-export([gen_dec_prim/8]).
-export([gen_objectset_code/2, gen_obj_code/3]).
-export([re_wrap_erule/1]).
-export([unused_var/2]).

-import(asn1ct_gen, [emit/1,demit/1]).

						% the encoding of class of tag bits 8 and 7
-define(UNIVERSAL,   0).
-define(APPLICATION, 16#40).
-define(CONTEXT,     16#80).
-define(PRIVATE,     16#C0).

						% primitive or constructed encoding % bit 6
-define(PRIMITIVE,   0).
-define(CONSTRUCTED, 2#00100000).


-define(T_ObjectDescriptor, ?UNIVERSAL bor ?PRIMITIVE bor 7).
						% restricted character string types
-define(T_NumericString,    ?UNIVERSAL bor ?PRIMITIVE bor 18). %can be constructed
-define(T_PrintableString,  ?UNIVERSAL bor ?PRIMITIVE bor 19). %can be constructed
-define(T_TeletexString,    ?UNIVERSAL bor ?PRIMITIVE bor 20). %can be constructed
-define(T_VideotexString,   ?UNIVERSAL bor ?PRIMITIVE bor 21). %can be constructed
-define(T_IA5String,        ?UNIVERSAL bor ?PRIMITIVE bor 22). %can be constructed
-define(T_GraphicString,    ?UNIVERSAL bor ?PRIMITIVE bor 25). %can be constructed
-define(T_VisibleString,    ?UNIVERSAL bor ?PRIMITIVE bor 26). %can be constructed
-define(T_GeneralString,    ?UNIVERSAL bor ?PRIMITIVE bor 27). %can be constructed

%% pgen(Erules, Module, TypeOrVal)
%% Generate Erlang module (.erl) and (.hrl) file corresponding to an ASN.1 module
%% .hrl file is only generated if necessary
%% Erules = per | ber
%% Module = atom()
%% TypeOrVal = {TypeList,ValueList,PTypeList}
%% TypeList = ValueList = [atom()]

pgen(OutFile,Erules,Module,TypeOrVal) ->
    asn1ct_gen:pgen_module(OutFile,Erules,Module,TypeOrVal,true).


%%===============================================================================
%%===============================================================================
%%===============================================================================
%% Generate ENCODING
%%===============================================================================
%%===============================================================================
%%===============================================================================

%%===============================================================================
%% encode #{typedef, {pos, name, typespec}}
%%===============================================================================

gen_encode(Erules,Type) when record(Type,typedef) ->
    gen_encode_user(Erules,Type).

%%===============================================================================
%% encode #{type, {tag, def, constraint}}
%%===============================================================================

gen_encode(Erules,Typename,Type) when record(Type,type) ->
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
	    emit([nl,nl,nl,"%%================================"]),
	    emit([nl,"%%  ",asn1ct_gen:list2name(Typename)]),
	    emit([nl,"%%================================",nl]),
	    case lists:member(InnerType,['SET','SEQUENCE']) of
		true ->
		    case get(asn_keyed_list) of
			true ->
			    CompList =
				case Type#type.def of
				    #'SEQUENCE'{components=Cl} -> Cl;
				    #'SET'{components=Cl} -> Cl
				end,
			    emit([nl,"'enc_",asn1ct_gen:list2name(Typename),
				  "'(Val, TagIn",ObjFun,
				  ") when list(Val) ->",nl]),
			    emit(["    'enc_",asn1ct_gen:list2name(Typename),
				  "'(?RT_BER:fixoptionals(",
				  {asis,optionals(CompList)},
				  ",Val), TagIn",ObjFun,");",nl,nl]);
			_ -> true
		    end;
		_ ->
		    emit([nl,"'enc_",asn1ct_gen:list2name(Typename),
			  "'({'",asn1ct_gen:list2name(Typename),
			  "',Val}, TagIn",ObjFun,") ->",nl]),
		    emit(["   'enc_",asn1ct_gen:list2name(Typename),
			  "'(Val, TagIn",ObjFun,");",nl,nl])
	    end,
	    emit(["'enc_",asn1ct_gen:list2name(Typename),
		  "'(Val, TagIn",ObjFun,") ->",nl,"   "]),
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end;

%%===============================================================================
%% encode ComponentType
%%===============================================================================

gen_encode(Erules,Tname,{'ComponentType',_Pos,Cname,Type,_,_}) ->
    NewTname = [Cname|Tname],
    %% The tag is set to [] to avoid that it is
    %% taken into account twice, both as a component/alternative (passed as
    %% argument to the encode decode function and within the encode decode
    %% function it self.
    NewType = Type#type{tag=[]},
    gen_encode(Erules,NewTname,NewType).

gen_encode_user(Erules,D) when record(D,typedef) ->
    Typename = [D#typedef.name],
    Type = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    OTag = Type#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    emit([nl,nl,"%%================================"]),
    emit([nl,"%%  ",Typename]),
    emit([nl,"%%================================",nl]),
    case lists:member(InnerType,['SET','SEQUENCE']) of
	true ->
	    case get(asn_keyed_list) of
		true ->
		    CompList =
			case Type#type.def of
			    #'SEQUENCE'{components=Cl} -> Cl;
			    #'SET'{components=Cl} -> Cl
			end,

		    emit([nl,"'enc_",asn1ct_gen:list2name(Typename),
			  "'(Val, TagIn) when list(Val) ->",nl]),
		    emit(["    'enc_",asn1ct_gen:list2name(Typename),
			  "'(?RT_BER:fixoptionals(",
			  {asis,optionals(CompList)},
			  ",Val), TagIn);",nl,nl]);
		_ -> true
	    end;
	_ ->
	    emit({nl,"'enc_",asn1ct_gen:list2name(Typename),
		  "'({'",asn1ct_gen:list2name(Typename),"',Val}, TagIn) ->",nl}),
	    emit({"   'enc_",asn1ct_gen:list2name(Typename),"'(Val, TagIn);",nl,nl})
    end,
    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(",
	  unused_var("Val",Type#type.def),", TagIn) ->",nl}),
    CurrentMod = get(currmod),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,D);
	{primitive,bif} ->
	    asn1ct_gen_ber:gen_encode_prim(ber,Type,["TagIn ++ ",
						     {asis,Tag}],"Val"),
	    emit([".",nl]);
	#typereference{val=Ename} ->
	    emit(["   'enc_",Ename,"'(Val, TagIn ++ ",{asis,Tag},").",nl]);
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    emit(["   'enc_",Etype,"'(Val, TagIn ++ ",
		  {asis,Tag},").",nl]);
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'enc_",Etype,"'(Val, TagIn ++ ",
		  {asis,Tag},").",nl]);
	'ASN1_OPEN_TYPE' ->
	    emit(["%% OPEN TYPE",nl]),
	    asn1ct_gen_ber:gen_encode_prim(ber,
					   Type#type{def='ASN1_OPEN_TYPE'},
					   ["TagIn ++ ",
					    {asis,Tag}],"Val"),
	    emit([".",nl])
    end.

unused_var(Var,#'SEQUENCE'{components=Cl}) ->
    unused_var1(Var,Cl);
unused_var(Var,#'SET'{components=Cl}) ->
    unused_var1(Var,Cl);
unused_var(Var,_) ->
    Var.
unused_var1(Var,Cs) when Cs == []; Cs == {[],[]} ->
    lists:concat(["_",Var]);
unused_var1(Var,_) ->
    Var.

unused_optormand_var(Var,Def) ->
    case asn1ct_gen:type(asn1ct_gen:get_inner(Def)) of
	'ASN1_OPEN_TYPE' ->
	    lists:concat(["_",Var]);
	_ ->
	    Var
    end.


gen_encode_prim(_Erules,D,DoTag,Value) when record(D,type) ->

%%% Currently not used for BER (except for BitString) and therefore replaced
%%% with [] as a placeholder
    BitStringConstraint = D#type.constraint,
    Constraint = [],
    asn1ct_name:new(enumval),
    case D#type.def of
	'BOOLEAN' ->
	    emit_encode_func('boolean',Value,DoTag);
	'INTEGER' ->
	    emit_encode_func('integer',Constraint,Value,DoTag);
	{'INTEGER',NamedNumberList} ->
	    emit_encode_func('integer',Constraint,Value,
			     NamedNumberList,DoTag);
	{'ENUMERATED',NamedNumberList={_,_}} ->

	    emit(["case (case ",Value," of {asn1_enum,_}->",Value,";{_,_}->element(2,",Value,");_->",
		  Value," end) of",nl]),
	    emit_enc_enumerated_cases(NamedNumberList,DoTag);
	{'ENUMERATED',NamedNumberList} ->

	    emit(["case (case ",Value," of {_,_}->element(2,",Value,");_->",
		  Value," end) of",nl]),
	    emit_enc_enumerated_cases(NamedNumberList,DoTag);

	{'BIT STRING',NamedNumberList} ->
	    emit_encode_func('bit_string',BitStringConstraint,Value,
			     NamedNumberList,DoTag);
	'ANY' ->
	    emit_encode_func('open_type', Value,DoTag);
	'NULL' ->
	    emit_encode_func('null',Value,DoTag);
	'OBJECT IDENTIFIER' ->
	    emit_encode_func("object_identifier",Value,DoTag);
	'ObjectDescriptor' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_ObjectDescriptor,DoTag);
	'OCTET STRING' ->
	    emit_encode_func('octet_string',Constraint,Value,DoTag);
	'NumericString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_NumericString,DoTag);
	'TeletexString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_TeletexString,DoTag);
	'VideotexString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_VideotexString,DoTag);
	'GraphicString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_GraphicString,DoTag);
	'VisibleString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_VisibleString,DoTag);
	'GeneralString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_GeneralString,DoTag);
	'PrintableString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_PrintableString,DoTag);
	'IA5String' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_IA5String,DoTag);
	'UniversalString' ->
	    emit_encode_func('universal_string',Constraint,Value,DoTag);
	'BMPString' ->
	    emit_encode_func('BMP_string',Constraint,Value,DoTag);
	'UTCTime' ->
	    emit_encode_func('utc_time',Constraint,Value,DoTag);
	'GeneralizedTime' ->
	    emit_encode_func('generalized_time',Constraint,Value,DoTag);
	'ASN1_OPEN_TYPE' ->
	    emit_encode_func('open_type', Value,DoTag);
	XX ->
	    exit({'cannot encode' ,XX})
    end.


emit_encode_func(Name,Value,Tags) when atom(Name) ->
    emit_encode_func(atom_to_list(Name),Value,Tags);
emit_encode_func(Name,Value,Tags) ->
    Fname = "?RT_BER:encode_" ++ Name,
    emit([Fname,"(",Value,", ",Tags,")"]).

emit_encode_func(Name,Constraint,Value,Tags) when atom(Name) ->
    emit_encode_func(atom_to_list(Name),Constraint,Value,Tags);
emit_encode_func(Name,Constraint,Value,Tags) ->
    Fname = "?RT_BER:encode_" ++ Name,
    emit([Fname,"(",{asis,Constraint},", ",Value,", ",Tags,")"]).

emit_encode_func(Name,Constraint,Value,Asis,Tags) when atom(Name) ->
    emit_encode_func(atom_to_list(Name),Constraint,Value,Asis,Tags);
emit_encode_func(Name,Constraint,Value,Asis,Tags) ->
    Fname = "?RT_BER:encode_" ++ Name,
    emit([Fname,"(",{asis,Constraint},", ",Value,
	  ", ",{asis,Asis},
	  ", ",Tags,")"]).

emit_enc_enumerated_cases({L1,L2}, Tags) ->
    emit_enc_enumerated_cases(L1++L2, Tags, ext);
emit_enc_enumerated_cases(L, Tags) ->
    emit_enc_enumerated_cases(L, Tags, noext).

emit_enc_enumerated_cases([{EnumName,EnumVal},H2|T], Tags, Ext) ->
    emit([{asis,EnumName}," -> ?RT_BER:encode_enumerated(",EnumVal,",",Tags,");",nl]),
%%    emit(["'",{asis,EnumName},"' -> ?RT_BER:encode_enumerated(",EnumVal,",",Tags,");",nl]),
    emit_enc_enumerated_cases([H2|T], Tags, Ext);
emit_enc_enumerated_cases([{EnumName,EnumVal}], Tags, Ext) ->
    emit([{asis,EnumName}," -> ?RT_BER:encode_enumerated(",EnumVal,",",Tags,")"]),
%%    emit(["'",{asis,EnumName},"' -> ?RT_BER:encode_enumerated(",EnumVal,",",Tags,")"]),
    case Ext of
	noext -> emit([";",nl]);
	ext ->
	    emit([";",nl,"{asn1_enum,",{curr,enumval},"} -> ",
		     "?RT_BER:encode_enumerated(",{curr,enumval},",",Tags,");",nl]),
	    asn1ct_name:new(enumval)
    end,
    emit([{curr,enumval}," -> exit({error,{asn1, {enumerated_not_in_range,",{curr, enumval},"}}})"]),
    emit([nl,"end"]).


%%===============================================================================
%%===============================================================================
%%===============================================================================
%% Generate DECODING
%%===============================================================================
%%===============================================================================
%%===============================================================================

%%===============================================================================
%% decode #{typedef, {pos, name, typespec}}
%%===============================================================================

gen_decode(Erules,Type) when record(Type,typedef) ->
    D = Type,
    emit({nl,nl}),
    emit({"'dec_",Type#typedef.name,"'(Bytes, OptOrMand) ->",nl}),
    emit({"   'dec_",Type#typedef.name,"'(Bytes, OptOrMand, []).",nl,nl}),
    emit({"'dec_",Type#typedef.name,"'(Bytes, ",
	  unused_optormand_var("OptOrMand",(Type#typedef.typespec)#type.def),", TagIn) ->",nl}),
    dbdec(Type#typedef.name),
    gen_decode_user(Erules,D).


%%===============================================================================
%% decode #{type, {tag, def, constraint}}
%%===============================================================================

gen_decode(Erules,Tname,Type) when record(Type,type) ->
    Typename = Tname,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    ObjFun =
		case Type#type.tablecinf of
		    [{objfun,_}|_R] ->
			", ObjFun";
		    _ ->
			""
		end,
	    emit({"'dec_",asn1ct_gen:list2name(Typename),"'(Bytes, OptOrMand, TagIn",ObjFun,") ->",nl}),
	    dbdec(Typename),
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end;


%%===============================================================================
%% decode ComponentType
%%===============================================================================

gen_decode(Erules,Tname,{'ComponentType',_Pos,Cname,Type,_,_}) ->
    NewTname = [Cname|Tname],
    %% The tag is set to [] to avoid that it is
    %% taken into account twice, both as a component/alternative (passed as
    %% argument to the encode decode function and within the encode decode
    %% function it self.
    NewType = Type#type{tag=[]},
    gen_decode(Erules,NewTname,NewType).


gen_decode_user(Erules,D) when record(D,typedef) ->
    Typename = [D#typedef.name],
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    InnerTag = Def#type.tag ,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- InnerTag],
    case asn1ct_gen:type(InnerType) of
	'ASN1_OPEN_TYPE' ->
	    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
	    asn1ct_name:new(len),
	    gen_dec_prim(Erules, Def#type{def='ASN1_OPEN_TYPE'},
			 BytesVar, Tag, "TagIn",no_length,
			 ?PRIMITIVE,"OptOrMand"),
	    emit({".",nl,nl});
	{primitive,bif} ->
	    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
	    asn1ct_name:new(len),
	    gen_dec_prim(Erules, Def, BytesVar, Tag, "TagIn",no_length,
			 ?PRIMITIVE,"OptOrMand"),
	    emit({".",nl,nl});
	{constructed,bif} ->
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,D);
	TheType ->
	    DecFunName = mkfuncname(TheType,dec),
	    emit({DecFunName,"(",{curr,bytes},
		  ", OptOrMand, TagIn++",{asis,Tag},")"}),
	    emit({".",nl,nl})
    end.


gen_dec_prim(Erules,Att,BytesVar,DoTag,TagIn,Length,_Form,OptOrMand) ->
    Typename = Att#type.def,
%% Currently not used for BER replaced with [] as place holder
%%    Constraint = Att#type.constraint,
%% Constraint = [],
    Constraint =
	case get_constraint(Att#type.constraint,'SizeConstraint') of
	    no -> [];
	    Tc -> Tc
	end,
    ValueRange =
	case get_constraint(Att#type.constraint,'ValueRange') of
	    no -> [];
	    Tv -> Tv
	end,
    SingleValue =
	case get_constraint(Att#type.constraint,'SingleValue') of
	    no -> [];
	    Sv -> Sv
	end,
    AsBin = case get(binary_strings) of
		true -> "_as_bin";
		_ -> ""
	    end,
    NewTypeName = case Typename of
		      'ANY' -> 'ASN1_OPEN_TYPE';
		      _ -> Typename
		  end,
    DoLength =
	case NewTypeName of
	    'BOOLEAN'->
		emit({"?RT_BER:decode_boolean(",BytesVar,","}),
		false;
	    'INTEGER' ->
		emit({"?RT_BER:decode_integer(",BytesVar,",",
		      {asis,int_constr(SingleValue,ValueRange)},","}),
		false;
	    {'INTEGER',NamedNumberList} ->
		emit({"?RT_BER:decode_integer(",BytesVar,",",
		      {asis,int_constr(SingleValue,ValueRange)},",",
		      {asis,NamedNumberList},","}),
		false;
	    {'ENUMERATED',NamedNumberList} ->
		emit({"?RT_BER:decode_enumerated(",BytesVar,",",
		      {asis,Constraint},",",
		      {asis,NamedNumberList},","}),
		false;
	    {'BIT STRING',NamedNumberList} ->
		case get(compact_bit_string) of
		    true ->
			emit({"?RT_BER:decode_compact_bit_string(",
			      BytesVar,",",{asis,Constraint},",",
			      {asis,NamedNumberList},","});
		    _ ->
			emit({"?RT_BER:decode_bit_string(",BytesVar,",",
			      {asis,Constraint},",",
			      {asis,NamedNumberList},","})
		end,
		true;
	    'NULL' ->
		emit({"?RT_BER:decode_null(",BytesVar,","}),
		false;
	    'OBJECT IDENTIFIER' ->
		emit({"?RT_BER:decode_object_identifier(",BytesVar,","}),
		false;
	    'ObjectDescriptor' ->
		emit({"?RT_BER:decode_restricted_string(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_ObjectDescriptor},","}),
		true;
	    'OCTET STRING' ->
		emit({"?RT_BER:decode_octet_string",AsBin,"(",BytesVar,",",{asis,Constraint},","}),
		true;
	    'NumericString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_NumericString},","}),true;
	    'TeletexString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_TeletexString},","}),
		true;
	    'VideotexString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_VideotexString},","}),
		true;
	    'GraphicString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_GraphicString},","})
		    ,true;
	    'VisibleString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_VisibleString},","}),
		true;
	    'GeneralString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_GeneralString},","}),
		true;
	    'PrintableString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_PrintableString},","}),
		true;
	    'IA5String' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_IA5String},","}),
		true;
	    'UniversalString' ->
		emit({"?RT_BER:decode_universal_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},","}),
		true;
	    'BMPString' ->
		emit({"?RT_BER:decode_BMP_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},","}),
		true;
	    'UTCTime' ->
		emit({"?RT_BER:decode_utc_time",AsBin,"(",
		      BytesVar,",",{asis,Constraint},","}),
		true;
	    'GeneralizedTime' ->
		emit({"?RT_BER:decode_generalized_time",AsBin,"(",
		      BytesVar,",",{asis,Constraint},","}),
		true;
	    'ASN1_OPEN_TYPE' ->
		emit(["?RT_BER:decode_open_type(",re_wrap_erule(Erules),",",
		      BytesVar,","]),
		false;
	    Other ->
		exit({'cannot decode' ,Other})
	end,

    NewLength = case DoLength of
		    true -> [", ", Length];
		    false -> ""
		end,
    NewOptOrMand = case OptOrMand of
		       _ when list(OptOrMand) -> OptOrMand;
		       mandatory -> {asis,mandatory};
		       _ -> {asis,opt_or_default}
		   end,
    case {TagIn,NewTypeName} of
	{[],'ASN1_OPEN_TYPE'} ->
	    emit([{asis,DoTag},")"]);
	{_,'ASN1_OPEN_TYPE'} ->
	    emit([TagIn,"++",{asis,DoTag},")"]);
	{[],_} ->
	    emit([{asis,DoTag},NewLength,", ",NewOptOrMand,")"]);
	_ when list(TagIn) ->
	    emit([TagIn,"++",{asis,DoTag},NewLength,", ",NewOptOrMand,")"])
    end.


int_constr([],[]) ->
    [];
int_constr([],ValueRange) ->
    ValueRange;
int_constr(SingleValue,[]) ->
    SingleValue;
int_constr(SV,VR) ->
    [SV,VR].

%% Object code generating for encoding and decoding
%% ------------------------------------------------

gen_obj_code(Erules,_Module,Obj) when record(Obj,typedef) ->
    ObjName = Obj#typedef.name,
    Def = Obj#typedef.typespec,
    #'Externaltypereference'{module=M,type=ClName} = Def#'Object'.classname,
    Class = asn1_db:dbget(M,ClName),

    {object,_,Fields} = Def#'Object'.def,
    emit({nl,nl,nl,"%%================================"}),
    emit({nl,"%%  ",ObjName}),
    emit({nl,"%%================================",nl}),
    EncConstructed =
	gen_encode_objectfields(ClName,get_class_fields(Class),
				ObjName,Fields,[]),
    emit(nl),
    gen_encode_constr_type(Erules,EncConstructed),
    emit(nl),
    DecConstructed =
	gen_decode_objectfields(ClName,get_class_fields(Class),
				ObjName,Fields,[]),
    emit(nl),
    gen_decode_constr_type(Erules,DecConstructed);
gen_obj_code(_Erules,_Module,Obj) when record(Obj,pobjectdef) ->
    ok.


gen_encode_objectfields(ClassName,[{typefield,Name,OptOrMand}|Rest],
			ObjName,ObjectFields,ConstrAcc) ->
    EmitFuncClause =
	fun(Args) ->
		emit(["'enc_",ObjName,"'(",{asis,Name},
		      ", ",Args,", _RestPrimFieldName) ->",nl])
	end,
%     emit(["'enc_",ObjName,"'(",{asis,Name},
% 	  ", Val, TagIn, _RestPrimFieldName) ->",nl]),
    MaybeConstr=
	case {get_object_field(Name,ObjectFields),OptOrMand} of
	    {false,'MANDATORY'} -> %% this case is illegal
		exit({error,{asn1,{"missing mandatory field in object",
				   ObjName}}});
	    {false,'OPTIONAL'} ->
		EmitFuncClause("_, _"),
		emit(["   {[],0}"]),
		[];
	    {false,{'DEFAULT',DefaultType}} ->
		EmitFuncClause("Val, TagIn"),
		gen_encode_default_call(ClassName,Name,DefaultType);
	    {{Name,TypeSpec},_} ->
		%% A specified field owerwrites any 'DEFAULT' or
		%% 'OPTIONAL' field in the class
		EmitFuncClause("Val, TagIn"),
		gen_encode_field_call(ObjName,Name,TypeSpec)
	end,
    case more_genfields(Rest) of
	true ->
	    emit([";",nl]);
	false ->
	    emit([".",nl])
    end,
    gen_encode_objectfields(ClassName,Rest,ObjName,ObjectFields,
			    MaybeConstr++ConstrAcc);
gen_encode_objectfields(ClassName,[{objectfield,Name,_,_,OptOrMand}|Rest],
			ObjName,ObjectFields,ConstrAcc) ->
    EmitFuncClause =
	fun(Args) ->
		emit(["'enc_",ObjName,"'(",{asis,Name},
		      ", ",Args,") ->",nl])
	end,
%     emit(["'enc_",ObjName,"'(",{asis,Name},
% 	  ", Val, TagIn, [H|T]) ->",nl]),
    case {get_object_field(Name,ObjectFields),OptOrMand} of
	{false,'MANDATORY'} ->
	    exit({error,{asn1,{"missing mandatory field in object",
			       ObjName}}});
	{false,'OPTIONAL'} ->
	    EmitFuncClause("_,_,_"),
	    emit(["  exit({error,{'use of missing field in object', ",Name,
		  "}})"]);
	{false,{'DEFAULT',_DefaultObject}} ->
	    exit({error,{asn1,{"not implemented yet",Name}}});
	{{Name,TypeSpec},_} ->
	    EmitFuncClause(" Val, TagIn, [H|T]"),
	    case TypeSpec#typedef.name of
		{ExtMod,TypeName} ->
		    emit({indent(3),"'",ExtMod,"':'enc_",TypeName,
			  "'(H, Val, TagIn, T)"});
		TypeName ->
		    emit({indent(3),"'enc_",TypeName,"'(H, Val, TagIn, T)"})
	    end
    end,
    case more_genfields(Rest) of
	true ->
	    emit([";",nl]);
	false ->
	    emit([".",nl])
    end,
    gen_encode_objectfields(ClassName,Rest,ObjName,ObjectFields,ConstrAcc);
gen_encode_objectfields(ClassName,[_|Cs],O,OF,Acc) ->
    gen_encode_objectfields(ClassName,Cs,O,OF,Acc);
gen_encode_objectfields(_,[],_,_,Acc) ->
    Acc.


% gen_encode_objectfields(Class,ObjName,[{FieldName,Type}|Rest],ConstrAcc) ->
%     Fields = Class#objectclass.fields,
%     MaybeConstr=
% 	case is_typefield(Fields,FieldName) of
% 	    true ->
% 		Def = Type#typedef.typespec,
% 		OTag = Def#type.tag,
% 		Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
% 		emit({"'enc_",ObjName,"'(",{asis,FieldName},
% 		      ", Val, TagIn, RestPrimFieldName) ->",nl}),
% 		CAcc=
% 		case Type#typedef.name of
% 		    {primitive,bif} ->
% 			gen_encode_prim(ber,Def,["TagIn ++ ",{asis,Tag}],
% 					"Val"),
% 			[];
% 		    {constructed,bif} ->
% 			%%InnerType = asn1ct_gen:get_inner(Def#type.def),
% 			%%asn1ct_gen:gen_encode_constructed(ber,[ObjName],
% 			%%                            InnerType,Def);
% 			emit({"   'enc_",ObjName,'_',FieldName,
% 			      "'(Val, TagIn ++ ",{asis,Tag},")"}),
% 			[{['enc_',ObjName,'_',FieldName],Def}];
% 		    {ExtMod,TypeName} ->
% 			emit({"   '",ExtMod,"':'enc_",TypeName,
% 			      "'(Val, TagIn ++ ",{asis,Tag},")"}),
% 			[];
% 		    TypeName ->
% 			emit({"   'enc_",TypeName,"'(Val, TagIn ++ ",
% 			      {asis,Tag},")"}),
% 			[]
% 		end,
% 		case more_genfields(Fields,Rest) of
% 		    true ->
% 			emit({";",nl});
% 		    false ->
% 			emit({".",nl})
% 		end,
% 		CAcc;
% 	{false,objectfield} ->
% 	    emit({"'enc_",ObjName,"'(",{asis,FieldName},
% 		  ", Val, TagIn, [H|T]) ->",nl}),
% 	    case Type#typedef.name of
% 		{ExtMod,TypeName} ->
% 		    emit({indent(3),"'",ExtMod,"':'enc_",TypeName,
% 			  "'(H, Val, TagIn, T)"});
% 		TypeName ->
% 		    emit({indent(3),"'enc_",TypeName,"'(H, Val, TagIn, T)"})
% 	    end,
% 	    case more_genfields(Fields,Rest) of
% 		true ->
% 		    emit({";",nl});
% 		false ->
% 		    emit({".",nl})
% 	    end,
% 	    [];
% 	{false,_} -> []
%     end,
%     gen_encode_objectfields(Class,ObjName,Rest,MaybeConstr ++ ConstrAcc);
% gen_encode_objectfields(C,O,[H|T],Acc) ->
%     gen_encode_objectfields(C,O,T,Acc);
% gen_encode_objectfields(_,_,[],Acc) ->
%     Acc.

% gen_encode_constr_type([{Name,Def}|Rest]) ->
%     emit({Name,"(Val,TagIn) ->",nl}),
%     InnerType = asn1ct_gen:get_inner(Def#type.def),
%     asn1ct_gen:gen_encode_constructed(ber,Name,InnerType,Def),
%     gen_encode_constr_type(Rest);
gen_encode_constr_type(Erules,[TypeDef|Rest]) when record(TypeDef,typedef) ->
    case is_already_generated(enc,TypeDef#typedef.name) of
	true -> ok;
	_ -> gen_encode_user(Erules,TypeDef)
    end,
    gen_encode_constr_type(Erules,Rest);
gen_encode_constr_type(_,[]) ->
    ok.

gen_encode_field_call(ObjName,FieldName,Type) ->
    Def = Type#typedef.typespec,
    OTag = Def#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    case Type#typedef.name of
	{primitive,bif} -> %%tag should be the primitive tag
	    gen_encode_prim(ber,Def,["TagIn ++ ",{asis,Tag}],
			    "Val"),
	    [];
	{constructed,bif} ->
	    emit({"   'enc_",ObjName,'_',FieldName,
		  "'(Val, TagIn ++",{asis,Tag},")"}),
	    [Type#typedef{name=list_to_atom(lists:concat([ObjName,'_',FieldName]))}];
	{ExtMod,TypeName} ->
	    emit({"   '",ExtMod,"':'enc_",TypeName,
		  "'(Val, TagIn ++ ",{asis,Tag},")"}),
	    [];
	TypeName ->
	    emit({"   'enc_",TypeName,"'(Val, TagIn ++ ",{asis,Tag},")"}),
	    []
    end.

gen_encode_default_call(ClassName,FieldName,Type) ->
    CurrentMod = get(currmod),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    OTag = Type#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
%%	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,Type);
	    emit(["   'enc_",ClassName,'_',FieldName,"'(Bytes, TagIn ++ ",
		  {asis,Tag},")"]),
	    [#typedef{name=list_to_atom(lists:concat([ClassName,'_',FieldName])),
		      typespec=Type}];
	{primitive,bif} ->
	    gen_encode_prim(ber,Type,["TagIn ++ ",{asis,Tag}],"Val"),
	    [];
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    emit(["   'enc_",Etype,"'(Val, TagIn ++ ",{asis,Tag},")",nl]),
	    [];
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'enc_",Etype,"'(Val, TagIn ++ ",{asis,Tag},")",nl]),
	    []
    end.



gen_decode_objectfields(ClassName,[{typefield,Name,OptOrMand}|Rest],
			ObjName,ObjectFields,ConstrAcc) ->
    EmitFuncClause =
	fun(Args) ->
		emit(["'dec_",ObjName,"'(",{asis,Name},
		      ", ",Args,"_) ->",nl])
	end,
%     emit(["'dec_",ObjName,"'(",{asis,Name},
% 	  ", Bytes, TagIn, RestPrimFieldName) ->",nl]),
    MaybeConstr=
	case {get_object_field(Name,ObjectFields),OptOrMand} of
	    {false,'MANDATORY'} -> %% this case is illegal
		exit({error,{asn1,{"missing mandatory field in object",
				   ObjName}}});
	    {false,'OPTIONAL'} ->
		EmitFuncClause("_, _,"),
		emit(["   asn1_NOVALUE"]),
		[];
	    {false,{'DEFAULT',DefaultType}} ->
		EmitFuncClause("Bytes, TagIn,"),
		gen_decode_default_call(ClassName,Name,"Bytes",DefaultType);
	    {{Name,TypeSpec},_} ->
		%% A specified field owerwrites any 'DEFAULT' or
		%% 'OPTIONAL' field in the class
		EmitFuncClause("Bytes, TagIn,"),
		gen_decode_field_call(ObjName,Name,"Bytes",TypeSpec)
	end,
    case more_genfields(Rest) of
	true ->
	    emit([";",nl]);
	false ->
	    emit([".",nl])
    end,
    gen_decode_objectfields(ClassName,Rest,ObjName,ObjectFields,MaybeConstr++ConstrAcc);
gen_decode_objectfields(ClassName,[{objectfield,Name,_,_,OptOrMand}|Rest],
			ObjName,ObjectFields,ConstrAcc) ->
    EmitFuncClause =
	fun(Args) ->
		emit(["'dec_",ObjName,"'(",{asis,Name},
		      ", ",Args,") ->",nl])
	end,
%     emit(["'dec_",ObjName,"'(",{asis,Name},
% 	  ", Bytes,TagIn,[H|T]) ->",nl]),
    case {get_object_field(Name,ObjectFields),OptOrMand} of
	{false,'MANDATORY'} ->
	    exit({error,{asn1,{"missing mandatory field in object",
			       ObjName}}});
	{false,'OPTIONAL'} ->
	    EmitFuncClause("_,_,_"),
	    emit(["  exit({error,{'illegal use of missing field in object', ",Name,
		  "}})"]);
	{false,{'DEFAULT',_DefaultObject}} ->
	    exit({error,{asn1,{"not implemented yet",Name}}});
	{{Name,TypeSpec},_} ->
	    EmitFuncClause("Bytes,TagIn,[H|T]"),
	    case TypeSpec#typedef.name of
		{ExtMod,TypeName} ->
		    emit({indent(3),"'",ExtMod,"':'dec_",TypeName,
			  "'(H, Bytes, TagIn, T)"});
		TypeName ->
		    emit({indent(3),"'dec_",TypeName,"'(H, Bytes, TagIn, T)"})
	    end
    end,
    case more_genfields(Rest) of
	true ->
	    emit([";",nl]);
	false ->
	    emit([".",nl])
    end,
    gen_decode_objectfields(ClassName,Rest,ObjName,ObjectFields,ConstrAcc);
gen_decode_objectfields(CN,[_|Cs],O,OF,CAcc) ->
    gen_decode_objectfields(CN,Cs,O,OF,CAcc);
gen_decode_objectfields(_,[],_,_,CAcc) ->
    CAcc.



% gen_decode_objectfields(Erules,Class,ObjName,[{FieldName,Type}|Rest],ConstrAcc) ->
%     Fields = Class#objectclass.fields,
%     MaybeConstr =
%     case is_typefield(Fields,FieldName) of
% 	true ->
% 	    Def = Type#typedef.typespec,
% 	    emit({"'dec_",ObjName,"'(",{asis,FieldName},
% 		  ", Bytes, TagIn, RestPrimFieldName) ->",nl}),
% 	    OTag = Def#type.tag,
% 	    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
% 	    Prop =
% 		case get_optionalityspec(Fields,FieldName) of
% 		    'OPTIONAL' -> opt_or_default;
% 		    {'DEFAULT',_} -> opt_or_default;
% 		    _ -> mandatory
% 		end,
% 	    CAcc =
% 	    case Type#typedef.name of
% 		{primitive,bif} ->
% 		    gen_dec_prim(Erules,Def,"Bytes",Tag,"TagIn",no_length,
% 				 ?PRIMITIVE,Prop),
% 		    [];
% 		{constructed,bif} ->
% 		    emit({"   'dec_",ObjName,'_',FieldName,"'(Bytes,",
% 			  {asis,Prop},", TagIn ++ ",{asis,Tag},")"}),
% 		    [{['dec_',ObjName,'_',FieldName],Def}];
% 		{ExtMod,TypeName} ->
% 		    emit({"   '",ExtMod,"':'dec_",TypeName,"'(Bytes, ",
% 			  {asis,Prop},", TagIn ++ ",{asis,Tag},")"}),
% 		    [];
% 		TypeName ->
% 		    emit({"   'dec_",TypeName,"'(Bytes, ",{asis,Prop},
% 			  ", TagIn ++ ",{asis,Tag},")"}),
% 		    []
% 	    end,
% 	    case more_genfields(Fields,Rest) of
% 		true ->
% 		    emit({";",nl});
% 		false ->
% 		    emit({".",nl})
% 	    end,
% 	    CAcc;
% 	{false,objectfield} ->
% 	    emit({"'dec_",ObjName,"'(",{asis,FieldName},
% 		  ", Bytes, TagIn, [H|T]) ->",nl}),
% 	    case Type#typedef.name of
% 		{ExtMod,TypeName} ->
% 		    emit({indent(3),"'",ExtMod,"':'dec_",TypeName,
% 			  "'(H, Bytes, TagIn, T)"});
% 		TypeName ->
% 		    emit({indent(3),"'dec_",TypeName,
% 			  "'(H, Bytes, TagIn, T)"})
% 	    end,
% 	    case more_genfields(Fields,Rest) of
% 		true ->
% 		    emit({";",nl});
% 		false ->
% 		    emit({".",nl})
% 	    end,
% 	    [];
% 	{false,_} ->
% 	    []
%     end,
%     gen_decode_objectfields(Erules,Class,ObjName,Rest,MaybeConstr ++ ConstrAcc);
% gen_decode_objectfields(Erules,C,O,[H|T],CAcc) ->
%     gen_decode_objectfields(Erules,C,O,T,CAcc);
% gen_decode_objectfields(_,_,_,[],CAcc) ->
%     CAcc.

gen_decode_constr_type(Erules,[{Name,Def}|Rest]) ->
%%    emit({Name,"(Bytes, OptOrMand) ->",nl}),
%%    emit({"   ",Name,"(Bytes, OptOrMand, []).",nl,nl}),
    emit({Name,"(Bytes, OptOrMand, TagIn) ->",nl}),
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    asn1ct_gen:gen_decode_constructed(ber,Name,InnerType,Def),
    gen_decode_constr_type(Erules,Rest);
gen_decode_constr_type(Erules,[TypeDef|Rest]) when record(TypeDef,typedef) ->
    case is_already_generated(dec,TypeDef#typedef.name) of
	true -> ok;
	_ ->
	    gen_decode(Erules,TypeDef)
    end,
    gen_decode_constr_type(Erules,Rest);
gen_decode_constr_type(_,[]) ->
    ok.

gen_decode_field_call(ObjName,FieldName,Bytes,Type) ->
    Def = Type#typedef.typespec,
    OTag = Def#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    case Type#typedef.name of
	{primitive,bif} -> %%tag should be the primitive tag
	    gen_dec_prim(ber,Def,Bytes,Tag,"TagIn",no_length,
			 ?PRIMITIVE,opt_or_default),
	    [];
	{constructed,bif} ->
	    emit({"   'dec_",ObjName,'_',FieldName,
		  "'(",Bytes,",opt_or_default, TagIn ++ ",{asis,Tag},")"}),
	    [Type#typedef{name=list_to_atom(lists:concat([ObjName,'_',FieldName]))}];
	{ExtMod,TypeName} ->
	    emit({"   '",ExtMod,"':'dec_",TypeName,
		  "'(",Bytes,", opt_or_default,TagIn ++ ",{asis,Tag},")"}),
	    [];
	TypeName ->
	    emit({"   'dec_",TypeName,"'(",Bytes,
		  ", opt_or_default,TagIn ++ ",{asis,Tag},")"}),
	    []
    end.

gen_decode_default_call(ClassName,FieldName,Bytes,Type) ->
    CurrentMod = get(currmod),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    OTag = Type#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    emit(["   'dec_",ClassName,'_',FieldName,"'(",Bytes,
		  ",opt_or_default, TagIn ++ ",{asis,Tag},")"]),
	    [#typedef{name=list_to_atom(lists:concat([ClassName,'_',FieldName])),
		      typespec=Type}];
	{primitive,bif} ->
	    gen_dec_prim(ber,Type,Bytes,Tag,"TagIn",no_length,
			 ?PRIMITIVE,opt_or_default),
	    [];
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    emit(["   'dec_",Etype,"'(",Bytes,
		  " ,opt_or_default, TagIn ++ ",{asis,Tag},")",nl]),
	    [];
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'dec_",Etype,"'(",Bytes,
		  ", opt_or_defualt, TagIn ++ ",{asis,Tag},")",nl]),
	    []
    end.


more_genfields([]) ->
    false;
more_genfields([Field|Fields]) ->
    case element(1,Field) of
	typefield ->
	    true;
	objectfield ->
	    true;
	_ ->
	    more_genfields(Fields)
    end.



%% Object Set code generating for encoding and decoding
%% ----------------------------------------------------
gen_objectset_code(Erules,ObjSet) ->
    ObjSetName = ObjSet#typedef.name,
    Def = ObjSet#typedef.typespec,
%    {ClassName,ClassDef} = Def#'ObjectSet'.class,
    #'Externaltypereference'{module=ClassModule,
			     type=ClassName} = Def#'ObjectSet'.class,
    ClassDef = asn1_db:dbget(ClassModule,ClassName),
    UniqueFName = Def#'ObjectSet'.uniquefname,
    Set = Def#'ObjectSet'.set,
    emit({nl,nl,nl,"%%================================"}),
    emit({nl,"%%  ",ObjSetName}),
    emit({nl,"%%================================",nl}),
    case ClassName of
	{_Module,ExtClassName} ->
	    gen_objset_code(Erules,ObjSetName,UniqueFName,Set,
			    ExtClassName,ClassDef);
	_ ->
	    gen_objset_code(Erules,ObjSetName,UniqueFName,Set,
			    ClassName,ClassDef)
    end,
    emit(nl).

gen_objset_code(Erules,ObjSetName,UniqueFName,Set,ClassName,ClassDef)->
    ClassFields = (ClassDef#classdef.typespec)#objectclass.fields,
    InternalFuncs=gen_objset_enc(ObjSetName,UniqueFName,Set,ClassName,ClassFields,1,[]),
    gen_objset_dec(Erules,ObjSetName,UniqueFName,Set,ClassName,ClassFields,1),
    gen_internal_funcs(Erules,InternalFuncs).

%% gen_objset_enc iterates over the objects of the object set
gen_objset_enc(_,{unique,undefined},_,_,_,_,_) ->
    %% There is no unique field in the class of this object set
    %% don't bother about the constraint
    [];
gen_objset_enc(ObjSName,UniqueName,
	       [{ObjName,Val,Fields},T|Rest],ClName,ClFields,NthObj,Acc)->
    emit({"'getenc_",ObjSName,"'(",{asis,UniqueName},",",{asis,Val},") ->",nl}),
    {InternalFunc,NewNthObj}=
	case ObjName of
	    no_name ->
		gen_inlined_enc_funs(Fields,ClFields,ObjSName,NthObj);
	    _Other ->
		emit({"    fun 'enc_",ObjName,"'/4"}),
		{[],NthObj}
	end,
    emit({";",nl}),
    gen_objset_enc(ObjSName,UniqueName,[T|Rest],ClName,ClFields,
		   NewNthObj,InternalFunc ++ Acc);
gen_objset_enc(ObjSetName,UniqueName,
	       [{ObjName,Val,Fields}],_ClName,ClFields,NthObj,Acc) ->
    emit({"'getenc_",ObjSetName,"'(",{asis,UniqueName},",",{asis,Val},") ->",nl}),
    {InternalFunc,_}=
	case ObjName of
	    no_name ->
		gen_inlined_enc_funs(Fields,ClFields,ObjSetName,NthObj);
	    _Other ->
		emit({"    fun 'enc_",ObjName,"'/4"}),
		{[],NthObj}
	end,
    emit({".",nl,nl}),
    InternalFunc ++ Acc;
%% See X.681 Annex E for the following case
gen_objset_enc(ObjSetName,_UniqueName,['EXTENSIONMARK'],
	       _ClName,_ClFields,_NthObj,Acc) ->
    emit({"'getenc_",ObjSetName,"'(_, _) ->",nl}),
    emit({indent(3),"fun(_Attr, Val, _TagIn, _RestPrimFieldName) ->",nl}),
    emit({indent(6),"Len = case Val of",nl,indent(9),
	  "Bin when binary(Bin) -> size(Bin);",nl,indent(9),
	  "_ -> length(Val)",nl,indent(6),"end,"}),
    emit({indent(6),"{Val,Len}",nl}),
    emit({indent(3),"end.",nl,nl}),
    Acc;
gen_objset_enc(_,_,[],_,_,_,Acc) ->
    Acc.

%% gen_inlined_enc_funs for each object iterates over all fields of a
%% class, and for each typefield it checks if the object has that
%% field and emits the proper code.
gen_inlined_enc_funs(Fields,[{typefield,Name,_}|Rest],ObjSetName,
		     NthObj) ->
    InternalDefFunName = asn1ct_gen:list2name([NthObj,Name,ObjSetName]),
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when record(Type,type) ->
	    emit({indent(3),"fun(Type, Val, TagIn, _RestPrimFieldName) ->",nl,
		  indent(6),"case Type of",nl}),
	    {Ret,N} = emit_inner_of_fun(Type,InternalDefFunName),
	    gen_inlined_enc_funs1(Fields,Rest,ObjSetName,NthObj+N,Ret);
	{value,{_,Type}} when record(Type,typedef) ->
	    emit({indent(3),"fun(Type, Val, TagIn, _RestPrimFieldName) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    {Ret,N} = emit_inner_of_fun(Type,InternalDefFunName),
	    gen_inlined_enc_funs1(Fields,Rest,ObjSetName,NthObj+N,Ret);
	false ->
	    gen_inlined_enc_funs(Fields,Rest,ObjSetName,NthObj)
    end;
gen_inlined_enc_funs(Fields,[_H|Rest],ObjSetName,NthObj) ->
    gen_inlined_enc_funs(Fields,Rest,ObjSetName,NthObj);
gen_inlined_enc_funs(_,[],_,NthObj) ->
    {[],NthObj}.

gen_inlined_enc_funs1(Fields,[{typefield,Name,_}|Rest],ObjSetName,
		      NthObj,Acc) ->
    InternalDefFunName = asn1ct_gen:list2name([NthObj,Name,ObjSetName]),
    {Acc2,NAdd}=
	case lists:keysearch(Name,1,Fields) of
	    {value,{_,Type}} when record(Type,type) ->
		emit({";",nl}),
		{Ret,N}=emit_inner_of_fun(Type,InternalDefFunName),
		{Ret++Acc,N};
	    {value,{_,Type}} when record(Type,typedef) ->
		emit({";",nl,indent(9),{asis,Name}," ->",nl}),
		{Ret,N}=emit_inner_of_fun(Type,InternalDefFunName),
		{Ret++Acc,N};
	    false ->
		{Acc,0}
	end,
    gen_inlined_enc_funs1(Fields,Rest,ObjSetName,NthObj+NAdd,Acc2);
gen_inlined_enc_funs1(Fields,[_H|Rest],ObjSetName,NthObj,Acc)->
    gen_inlined_enc_funs1(Fields,Rest,ObjSetName,NthObj,Acc);
gen_inlined_enc_funs1(_,[],_,NthObj,Acc) ->
    emit({nl,indent(6),"end",nl}),
    emit({indent(3),"end"}),
    {Acc,NthObj}.


emit_inner_of_fun(TDef = #typedef{name={ExtMod,Name},typespec=Type},
		  InternalDefFunName) ->
    OTag = Type#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    case {ExtMod,Name} of
	{primitive,bif} ->
	    emit(indent(12)),
	    gen_encode_prim(ber,Type,["TagIn ++ ",{asis,Tag}],"Val"),
	    {[],0};
	{constructed,bif} ->
	    emit([indent(12),"'enc_",
		  InternalDefFunName,"'(Val,TagIn ++ ",
		  {asis,Tag},")"]),
	    {[TDef#typedef{name=InternalDefFunName}],1};
	_ ->
	    emit({indent(12),"'",ExtMod,"':'enc_",Name,"'(Val, TagIn ++ ",
		  {asis,Tag},")"}),
	    {[],0}
    end;
emit_inner_of_fun(#typedef{name=Name,typespec=Type},_) ->
    OTag = Type#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    emit({indent(12),"'enc_",Name,"'(Val, TagIn ++ ",{asis,Tag},")"}),
    {[],0};
emit_inner_of_fun(Type,_) when record(Type,type) ->
    CurrMod = get(currmod),
    OTag = Type#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    case Type#type.def of
	Def when atom(Def) ->
	    emit({indent(9),Def," ->",nl,indent(12)}),
	    gen_encode_prim(ber,Type,["TagIn ++ ",{asis,Tag}],"Val");
	TRef when record(TRef,typereference) ->
	    T = TRef#typereference.val,
	    emit({indent(9),T," ->",nl,indent(12),"'enc_",T,
		  "'(Val, TagIn ++ ",{asis,Tag},")"});
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),"'enc_",T,
		  "'(Val, TagIn ++ ",{asis,Tag},")"});
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),ExtMod,":'enc_",
		  T,"'(Val, TagIn ++ ",{asis,Tag},")"})
    end,
    {[],0}.

indent(N) ->
    lists:duplicate(N,32). % 32 = space


gen_objset_dec(_,_,{unique,undefined},_,_,_,_) ->
    %% There is no unique field in the class of this object set
    %% don't bother about the constraint
    ok;
gen_objset_dec(Erules,ObjSName,UniqueName,[{ObjName,Val,Fields},T|Rest],
	       ClName,ClFields,NthObj)->
    emit({"'getdec_",ObjSName,"'(",{asis,UniqueName},",",{asis,Val},
	  ") ->",nl}),
    NewNthObj=
	case ObjName of
	    no_name ->
		gen_inlined_dec_funs(Erules,Fields,ClFields,ObjSName,
				     NthObj);
	    _Other ->
		emit({"    fun 'dec_",ObjName,"'/4"}),
		NthObj
	end,
    emit({";",nl}),
    gen_objset_dec(Erules,ObjSName,UniqueName,[T|Rest],ClName,ClFields,
		   NewNthObj);
gen_objset_dec(Erules,ObjSetName,UniqueName,[{ObjName,Val,Fields}],_ClName,
	       ClFields,NthObj) ->
    emit({"'getdec_",ObjSetName,"'(",{asis,UniqueName},",",{asis,Val},") ->",nl}),
    case ObjName of
	no_name ->
	    gen_inlined_dec_funs(Erules,Fields,ClFields,ObjSetName,
				 NthObj);
	_Other ->
	    emit({"    fun 'dec_",ObjName,"'/4"})
    end,
    emit({".",nl,nl});
gen_objset_dec(_,ObjSetName,_UniqueName,['EXTENSIONMARK'],_ClName,_ClFields,
	      _NthObj) ->
    emit({"'getdec_",ObjSetName,"'(_, _) ->",nl}),
    emit({indent(3),"fun(_, Bytes, _, _) ->",nl}),
    emit({indent(6),"Len = case Bytes of",nl,indent(9),
	  "Bin when binary(Bin) -> size(Bin);",nl,indent(9),
	  "_ -> length(Bytes)",nl,indent(6),"end,"}),
    emit({indent(6),"{Bytes,[],Len}",nl}),
    emit({indent(3),"end.",nl,nl}),
    ok;
gen_objset_dec(_,_,_,[],_,_,_) ->
    ok.

gen_inlined_dec_funs(Erules,Fields,[{typefield,Name,Prop}|Rest],
		     ObjSetName,NthObj) ->
    DecProp = case Prop of
		  'OPTIONAL' -> opt_or_default;
		  {'DEFAULT',_} -> opt_or_default;
		  _ -> mandatory
	      end,
    InternalDefFunName = [NthObj,Name,ObjSetName],
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when record(Type,type) ->
	    emit({indent(3),"fun(Type, Bytes, TagIn, _RestPrimFieldName) ->",
		  nl,indent(6),"case Type of",nl}),
	    N=emit_inner_of_decfun(Erules,Type,DecProp,InternalDefFunName),
	    gen_inlined_dec_funs1(Erules,Fields,Rest,ObjSetName,NthObj+N);
	{value,{_,Type}} when record(Type,typedef) ->
	    emit({indent(3),"fun(Type, Bytes, TagIn, _RestPrimFieldName) ->",
		  nl,indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    N=emit_inner_of_decfun(Erules,Type,DecProp,InternalDefFunName),
	    gen_inlined_dec_funs1(Erules,Fields,Rest,ObjSetName,NthObj+N);
	false ->
	    gen_inlined_dec_funs(Erules,Fields,Rest,ObjSetName,NthObj)
    end;
gen_inlined_dec_funs(Erules,Fields,[_H|Rest],ObjSetName,NthObj) ->
    gen_inlined_dec_funs(Erules,Fields,Rest,ObjSetName,NthObj);
gen_inlined_dec_funs(_,_,[],_,NthObj) ->
    NthObj.

gen_inlined_dec_funs1(Erules,Fields,[{typefield,Name,Prop}|Rest],
		      ObjSetName,NthObj) ->
    DecProp = case Prop of
		  'OPTIONAL' -> opt_or_default;
		  {'DEFAULT',_} -> opt_or_default;
		  _ -> mandatory
	      end,
    InternalDefFunName = [NthObj,Name,ObjSetName],
    N=
	case lists:keysearch(Name,1,Fields) of
	    {value,{_,Type}} when record(Type,type) ->
		emit({";",nl}),
		emit_inner_of_decfun(Erules,Type,DecProp,InternalDefFunName);
	    {value,{_,Type}} when record(Type,typedef) ->
		emit({";",nl,indent(9),{asis,Name}," ->",nl}),
		emit_inner_of_decfun(Erules,Type,DecProp,InternalDefFunName);
	false ->
		0
	end,
    gen_inlined_dec_funs1(Erules,Fields,Rest,ObjSetName,NthObj+N);
gen_inlined_dec_funs1(Erules,Fields,[_H|Rest],ObjSetName,NthObj)->
    gen_inlined_dec_funs1(Erules,Fields,Rest,ObjSetName,NthObj);
gen_inlined_dec_funs1(_,_,[],_,NthObj) ->
    emit({nl,indent(6),"end",nl}),
    emit({indent(3),"end"}),
    NthObj.

emit_inner_of_decfun(Erules,#typedef{name={ExtName,Name},typespec=Type},
		     Prop,InternalDefFunName) ->
    OTag = Type#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    case {ExtName,Name} of
	{primitive,bif} ->
	    emit(indent(12)),
	    gen_dec_prim(Erules,Type,"Bytes",Tag,"TagIn",no_length,
			 ?PRIMITIVE,Prop),
	    0;
	{constructed,bif} ->
	    emit({indent(12),"'dec_",
		  asn1ct_gen:list2name(InternalDefFunName),"'(Bytes, ",Prop,
		  ", TagIn ++ ",{asis,Tag},")"}),
	    1;
	_ ->
	    emit({indent(12),"'",ExtName,"':'dec_",Name,"'(Bytes, ",Prop,
		  ", TagIn ++ ",{asis,Tag},")"}),
	    0
    end;
emit_inner_of_decfun(_,#typedef{name=Name,typespec=Type},Prop,_) ->
    OTag = Type#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    emit({indent(12),"'dec_",Name,"'(Bytes, ",Prop,", TagIn ++ ",
	  {asis,Tag},")"}),
    0;
emit_inner_of_decfun(Erules,Type,Prop,_) when record(Type,type) ->
    OTag = Type#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    CurrMod = get(currmod),
    Def = Type#type.def,
    InnerType = asn1ct_gen:get_inner(Def),
    WhatKind = asn1ct_gen:type(InnerType),
    case WhatKind of
	{primitive,bif} ->
	    emit({indent(9),Def," ->",nl,indent(12)}),
	    gen_dec_prim(Erules,Type,"Bytes",Tag,"TagIn",no_length,
			 ?PRIMITIVE,Prop);
%	TRef when record(TRef,typereference) ->
%	    T = TRef#typereference.val,
%	    emit({indent(9),T," ->",nl,indent(12),"'dec_",T,"'(Val)"});
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),"'dec_",T,
		  "'(Bytes, ",Prop,", TagIn ++ ",{asis,Tag},")"});
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),ExtMod,":'dec_",
		  T,"'(Bytes, ",Prop,", TagIn ++ ",{asis,Tag},")"})
    end,
    0.


gen_internal_funcs(_,[]) ->
    ok;
gen_internal_funcs(Erules,[TypeDef|Rest]) ->
    gen_encode_user(Erules,TypeDef),
    emit({"'dec_",TypeDef#typedef.name,"'(Bytes, ",
	  unused_optormand_var("OptOrMand",(TypeDef#typedef.typespec)#type.def),", TagIn) ->",nl}),
    gen_decode_user(Erules,TypeDef),
    gen_internal_funcs(Erules,Rest).


dbdec(Type) ->
    demit({"io:format(\"decoding: ",{asis,Type},"~w~n\",[Bytes]),",nl}).


decode_class('UNIVERSAL') ->
    ?UNIVERSAL;
decode_class('APPLICATION') ->
    ?APPLICATION;
decode_class('CONTEXT') ->
    ?CONTEXT;
decode_class('PRIVATE') ->
    ?PRIVATE.

decode_type('BOOLEAN') -> 1;
decode_type('INTEGER') -> 2;
decode_type('BIT STRING') -> 3;
decode_type('OCTET STRING') -> 4;
decode_type('NULL') -> 5;
decode_type('OBJECT IDENTIFIER') -> 6;
decode_type('OBJECT DESCRIPTOR') -> 7;
decode_type('EXTERNAL') -> 8;
decode_type('REAL') -> 9;
decode_type('ENUMERATED') -> 10;
decode_type('EMBEDDED_PDV') -> 11;
decode_type('SEQUENCE') -> 16;
decode_type('SEQUENCE OF') -> 16;
decode_type('SET') -> 17;
decode_type('SET OF') -> 17;
decode_type('NumericString') -> 18;
decode_type('PrintableString') -> 19;
decode_type('TeletexString') -> 20;
decode_type('VideotexString') -> 21;
decode_type('IA5String') -> 22;
decode_type('UTCTime') -> 23;
decode_type('GeneralizedTime') -> 24;
decode_type('GraphicString') -> 25;
decode_type('VisibleString') -> 26;
decode_type('GeneralString') -> 27;
decode_type('UniversalString') -> 28;
decode_type('BMPString') -> 30;
decode_type('CHOICE') -> 'CHOICE'; % choice gets the tag from the actual alternative
decode_type(Else) -> exit({error,{asn1,{unrecognized_type,Else}}}).

add_removed_bytes() ->
    asn1ct_name:delete(rb),
    add_removed_bytes(asn1ct_name:all(rb)).

add_removed_bytes([H,T1|T]) ->
    emit({{var,H},"+"}),
    add_removed_bytes([T1|T]);
add_removed_bytes([H|T]) ->
    emit({{var,H}}),
    add_removed_bytes(T);
add_removed_bytes([]) ->
    true.

mkfuncname(WhatKind,DecOrEnc) ->
    case WhatKind of
	#'Externaltypereference'{module=Mod,type=EType} ->
	    CurrMod = get(currmod),
	    case CurrMod of
		Mod ->
		    lists:concat(["'",DecOrEnc,"_",EType,"'"]);
		_ ->
% 		    io:format("CurrMod: ~p, Mod: ~p~n",[CurrMod,Mod]),
		    lists:concat(["'",Mod,"':'",DecOrEnc,"_",EType,"'"])
	    end;
	#'typereference'{val=EType} ->
	    lists:concat(["'",DecOrEnc,"_",EType,"'"]);
	'ASN1_OPEN_TYPE' ->
	    lists:concat(["'",DecOrEnc,"_",WhatKind,"'"])

    end.

optionals(L) -> optionals(L,[],1).

optionals([{'EXTENSIONMARK',_,_}|Rest],Acc,Pos) ->
    optionals(Rest,Acc,Pos); % optionals in extension are currently not handled
optionals([#'ComponentType'{name=Name,prop='OPTIONAL'}|Rest],Acc,Pos) ->
		 optionals(Rest,[{Name,Pos}|Acc],Pos+1);
optionals([#'ComponentType'{name=Name,prop={'DEFAULT',_}}|Rest],Acc,Pos) ->
		 optionals(Rest,[{Name,Pos}|Acc],Pos+1);
optionals([#'ComponentType'{}|Rest],Acc,Pos) ->
		 optionals(Rest,Acc,Pos+1);
optionals([],Acc,_) ->
    lists:reverse(Acc).

get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	     no;
	{value,{_,V}} ->
	    V
    end.

%% if the original option was ber and it has been wrapped to ber_bin
%% turn it back to ber
re_wrap_erule(ber_bin) ->
    case get(encoding_options) of
	Options when list(Options) ->
	    case lists:member(ber,Options) of
		true -> ber;
		_ -> ber_bin
	    end;
	_ -> ber_bin
    end;
re_wrap_erule(Erule) ->
    Erule.

is_already_generated(Operation,Name) ->
    case get(class_default_type) of
	undefined ->
	    put(class_default_type,[{Operation,Name}]),
	    false;
	GeneratedList ->
	    case lists:member({Operation,Name},GeneratedList) of
		true ->
		    true;
		false ->
		    put(class_default_type,[{Operation,Name}|GeneratedList]),
		    false
	    end
    end.

get_class_fields(#classdef{typespec=ObjClass}) ->
    ObjClass#objectclass.fields;
get_class_fields(#objectclass{fields=Fields}) ->
    Fields;
get_class_fields(_) ->
    [].

get_object_field(Name,ObjectFields) ->
    case lists:keysearch(Name,1,ObjectFields) of
	{value,Field} -> Field;
	false -> false
    end.
