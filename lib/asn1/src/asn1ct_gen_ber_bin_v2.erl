%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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
-module(asn1ct_gen_ber_bin_v2).

%% Generate erlang module which handles (PER) encode and decode for
%% all types in an ASN.1 module

-include("asn1_records.hrl").

-export([pgen/4]).
-export([decode_class/1, decode_type/1]).
-export([add_removed_bytes/0]).
-export([gen_encode/2,gen_encode/3,gen_decode/2,gen_decode/3]).
-export([gen_encode_prim/4]).
-export([gen_dec_prim/7]).
-export([gen_objectset_code/2, gen_obj_code/3]).
-export([encode_tag_val/3]).
-export([gen_inc_decode/2,gen_decode_selected/3]).
-export([extaddgroup2sequence/1]).

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
    asn1ct_gen:pgen_module(OutFile,Erules,Module,TypeOrVal,[],true).


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

gen_encode(Erules,Type) when is_record(Type,typedef) ->
    gen_encode_user(Erules,Type).

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
	    emit([nl,nl,nl,"%%================================"]),
	    emit([nl,"%%  ",asn1ct_gen:list2name(Typename)]),
	    emit([nl,"%%================================",nl]),
	    case length(Typename) of
		1 -> % top level type
		    emit(["'enc_",asn1ct_gen:list2name(Typename),
			  "'(Val",ObjFun,") ->",nl]),
		    emit(["    'enc_",asn1ct_gen:list2name(Typename),
			  "'(Val, ", {asis,lists:reverse(Type#type.tag)},ObjFun,").",nl,nl]);
		_ -> % embedded type with constructed name
		    true
	    end,
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
				  ") when is_list(Val) ->",nl]),
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

gen_encode(Erules,Tname,#'ComponentType'{name=Cname,typespec=Type}) ->
    NewTname = [Cname|Tname],
    %% The tag is set to [] to avoid that it is
    %% taken into account twice, both as a component/alternative (passed as
    %% argument to the encode decode function and within the encode decode
    %% function it self.
    NewType = Type#type{tag=[]},
    gen_encode(Erules,NewTname,NewType).

gen_encode_user(Erules,D) when is_record(D,typedef) ->
    Typename = [D#typedef.name],
    Type = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    OTag = Type#type.tag,
    Tag = [encode_tag_val(decode_class(X#tag.class),X#tag.form,X#tag.number)|| X <- OTag],
    emit([nl,nl,"%%================================"]),
    emit([nl,"%%  ",Typename]),
    emit([nl,"%%================================",nl]),
    emit(["'enc_",asn1ct_gen:list2name(Typename),
	  "'(Val",") ->",nl]),
    emit(["    'enc_",asn1ct_gen:list2name(Typename),
	  "'(Val, ", {asis,lists:reverse(Tag)},").",nl,nl]),

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
			  "'(Val, TagIn) when is_list(Val) ->",nl]),
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
    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(Val, TagIn) ->",nl}),
    CurrentMod = get(currmod),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,D);
	{primitive,bif} ->
	    gen_encode_prim(ber,Type,"TagIn","Val"),
	    emit([".",nl]);
	#typereference{val=Ename} ->
	    emit(["   'enc_",Ename,"'(Val, TagIn).",nl]);
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    emit(["   'enc_",Etype,"'(Val, TagIn).",nl]);
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'enc_",Etype,"'(Val, TagIn).",nl]);
	'ASN1_OPEN_TYPE' ->
	    emit(["%% OPEN TYPE",nl]),
	    gen_encode_prim(ber,
			    Type#type{def='ASN1_OPEN_TYPE'},
			    "TagIn","Val"),
	    emit([".",nl])
    end.

gen_encode_prim(Erules,D,DoTag,Value) when is_record(D,type) ->
    
%%% Constraint is currently not used for BER (except for BitString) and therefore replaced
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
	    
	    emit(["case ",Value," of",nl]),
	    emit_enc_enumerated_cases(NamedNumberList,DoTag);
	{'ENUMERATED',NamedNumberList} ->
	    
	    emit(["case ",Value," of",nl]),
	    emit_enc_enumerated_cases(NamedNumberList,DoTag);

	'REAL' ->
	    emit_encode_func('real',Constraint,Value,DoTag);

	{'BIT STRING',NamedNumberList} ->
	    emit_encode_func('bit_string',BitStringConstraint,Value,
			     NamedNumberList,DoTag);
	'ANY' ->
	    emit_encode_func('open_type', Value,DoTag);
	'NULL' ->
	    emit_encode_func('null',Value,DoTag);
	'OBJECT IDENTIFIER' ->
	    emit_encode_func("object_identifier",Value,DoTag);
	'RELATIVE-OID' ->
	    emit_encode_func("relative_oid",Value,DoTag);
	'ObjectDescriptor' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_ObjectDescriptor,DoTag);
	'OCTET STRING' ->
	    emit_encode_func('octet_string',Constraint,Value,DoTag);
	'NumericString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_NumericString,DoTag);
	TString when TString == 'TeletexString';
		     TString == 'T61String' ->
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
	'UTF8String' ->
	    emit_encode_func('UTF8_string',Constraint,Value,DoTag);
	'BMPString' ->
	    emit_encode_func('BMP_string',Constraint,Value,DoTag);
	'UTCTime' ->
	    emit_encode_func('utc_time',Constraint,Value,DoTag);
	'GeneralizedTime' ->
	    emit_encode_func('generalized_time',Constraint,Value,DoTag);
	'ASN1_OPEN_TYPE' ->
	    emit_encode_func('open_type', Value,DoTag);
	#'ObjectClassFieldType'{} ->
	    case asn1ct_gen:get_inner(D#type.def) of
		{fixedtypevaluefield,_,InnerType} -> 
		    gen_encode_prim(Erules,InnerType,DoTag,Value);
		'ASN1_OPEN_TYPE' ->
		    emit_encode_func('open_type', Value,DoTag);
		XX ->
		    exit({'can not encode' ,XX})
	    end;
	XX ->
	    exit({'can not encode' ,XX})
    end.


emit_encode_func(Name,Value,Tags) when is_atom(Name) ->
    emit_encode_func(atom_to_list(Name),Value,Tags);
emit_encode_func(Name,Value,Tags) ->
    Fname = "?RT_BER:encode_" ++ Name,
    emit([Fname,"(",Value,", ",Tags,")"]).

emit_encode_func(Name,Constraint,Value,Tags) when is_atom(Name) ->
    emit_encode_func(atom_to_list(Name),Constraint,Value,Tags);
emit_encode_func(Name,Constraint,Value,Tags) ->
    Fname = "?RT_BER:encode_" ++ Name,
    emit([Fname,"(",{asis,Constraint},", ",Value,", ",Tags,")"]).

emit_encode_func(Name,Constraint,Value,Asis,Tags) when is_atom(Name) ->
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
	    emit([";",nl])
%% 	    emit([";",nl,"{asn1_enum,",{curr,enumval},"} -> ",
%% 		     "?RT_BER:encode_enumerated(",{curr,enumval},",",Tags,");",nl]),
%%	    asn1ct_name:new(enumval)
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

gen_decode(Erules,Type) when is_record(Type,typedef) ->
    Def = Type#typedef.typespec,
    InnerTag = Def#type.tag ,

    Tag = [(decode_class(X#tag.class) bsl 10) + X#tag.number || X <- InnerTag],

    FunctionName =
	case {asn1ct:get_gen_state_field(active),
	      asn1ct:get_gen_state_field(prefix)} of
	    {true,Pref} -> 
		%% prevent duplicated function definitions
%		Pattern = asn1ct:get_gen_state_field(namelist),
%		FuncName=asn1ct:maybe_rename_function(Type#typedef.name,
%						      Pattern),
		case asn1ct:current_sindex() of
		    I when  is_integer(I),I>0 ->
			lists:concat([Pref,Type#typedef.name,"_",I]);
		    _->
			lists:concat([Pref,Type#typedef.name])
		end; % maybe the current_sindex must be reset
	    _ -> lists:concat(["dec_",Type#typedef.name])
	end,
    emit({nl,nl}),
    emit(["'",FunctionName,"'(Tlv) ->",nl]),
    emit(["   '",FunctionName,"'(Tlv, ",{asis,Tag},").",nl,nl]),
    emit(["'",FunctionName,"'(Tlv, TagIn) ->",nl]),
    dbdec(Type#typedef.name,"Tlv"),
    gen_decode_user(Erules,Type).

gen_inc_decode(Erules,Type) when is_record(Type,typedef) ->
    Prefix = asn1ct:get_gen_state_field(prefix),
    Suffix = asn1ct_gen:index2suffix(asn1ct:current_sindex()),
    emit({nl,nl}),
    emit(["'",Prefix,Type#typedef.name,Suffix,"'(Tlv, TagIn) ->",nl]),
    gen_decode_user(Erules,Type).

%% gen_decode_selected exported function for selected decode
gen_decode_selected(Erules,Type,FuncName) ->
    emit([FuncName,"(Bin) ->",nl]),
%    Pattern = asn1ct:get_gen_state_field(tag_pattern),
    Patterns = asn1ct:read_config_data(partial_decode),
    Pattern = 
	case lists:keysearch(FuncName,1,Patterns) of
	    {value,{_,P}} -> P;
	    false -> exit({error,{internal,no_pattern_saved}})
	end,
    emit(["  case ?RT_BER:decode_selective(",{asis,Pattern},",Bin) of",nl,
	  "    {ok,Bin2} when is_binary(Bin2) ->",nl,
	  "      {Tlv,_} = ?RT_BER:decode(Bin2",asn1ct_gen:nif_parameter(),"),",nl]),
    emit("{ok,"),
    gen_decode_selected_type(Erules,Type),
    emit(["};",nl,"    Err -> exit({error,{selctive_decode,Err}})",nl,
	  "  end.",nl]).

gen_decode_selected_type(_Erules,TypeDef) ->
    Def = TypeDef#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    BytesVar = "Tlv",
    Tag = [(decode_class(X#tag.class) bsl 10) + X#tag.number ||
	    X <- Def#type.tag],
    case asn1ct_gen:type(InnerType) of
	'ASN1_OPEN_TYPE' ->
	    asn1ct_name:new(len),
	    gen_dec_prim(ber, Def#type{def='ASN1_OPEN_TYPE'}, 
			 BytesVar,Tag, [] , 
			 ?PRIMITIVE,"OptOrMand");
%	    emit({";",nl});
	{primitive,bif} ->
	    asn1ct_name:new(len),
	    gen_dec_prim(ber, Def, BytesVar,Tag,[] , 
			 ?PRIMITIVE,"OptOrMand");
%	    emit([";",nl]);
	{constructed,bif} ->
	    TopType = case TypeDef#typedef.name of
			  A when is_atom(A) -> [A];
			  N -> N
		      end,
	    DecFunName = lists:concat(["'",dec,"_",
				       asn1ct_gen:list2name(TopType),"'"]),
	    emit([DecFunName,"(",BytesVar,
		  ", ",{asis,Tag},")"]);
%	    emit([";",nl]);
	TheType ->
	    DecFunName = mkfuncname(TheType,dec),
	    emit([DecFunName,"(",BytesVar,
		  ", ",{asis,Tag},")"])
%	    emit([";",nl])
    end.

%%===============================================================================
%% decode #{type, {tag, def, constraint}}
%%===============================================================================

%% This gen_decode is called by the gen_decode/3 that decodes
%% ComponentType and the type of a SEQUENCE OF/SET OF for an inner
%% type of an exclusive decode top type..
gen_decode(Erules,Typename,Type) when is_record(Type,type) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    FunctionName =
	case asn1ct:get_gen_state_field(active) of
	    true -> 
%		Suffix = asn1ct_gen:index2suffix(SIndex),
		Pattern = asn1ct:get_gen_state_field(namelist),
		Suffix = 
		    case asn1ct:maybe_saved_sindex(Typename,Pattern) of
			I when is_integer(I),I>0 ->
			    lists:concat(["_",I]);
			_ -> ""
		    end,
		lists:concat(["'dec-inc-",
			      asn1ct_gen:list2name(Typename),Suffix]);
	    _ -> 
		lists:concat(["'dec_",asn1ct_gen:list2name(Typename)])
	end,
%    io:format("Typename: ~p,~n",[Typename]),
%    io:format("FunctionName: ~p~n",[FunctionName]),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    ObjFun =
		case Type#type.tablecinf of
		    [{objfun,_}|_R] ->
			", ObjFun";
		    _ ->
			""
		end,
%	    emit([Prefix,asn1ct_gen:list2name(Typename),"'(Tlv, TagIn",ObjFun,") ->",nl]),
	    emit([FunctionName,"'(Tlv, TagIn",ObjFun,") ->",nl]),
	    dbdec(Typename,"Tlv"),
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,Type);
	Rec when is_record(Rec,'Externaltypereference') ->
	    case {Typename,asn1ct:get_gen_state_field(namelist)} of
		{[Cname|_],[{Cname,_}|_]} -> %% 
		    %% This referenced type must only be generated
		    %% once as incomplete partial decode. Therefore we
		    %% have to check whether this function already is
		    %% generated.
		    case asn1ct:is_function_generated(Typename) of
			true ->
			    ok;
			_ ->
			    asn1ct:generated_refed_func(Typename),
			    #'Externaltypereference'{module=M,type=Name}=Rec,
			    TypeDef = asn1_db:dbget(M,Name),
			    gen_decode(Erules,TypeDef)
		    end;
		_ ->
		    true
	    end;
	_ ->
	    true
    end;


%%===============================================================================
%% decode ComponentType
%%===============================================================================

gen_decode(Erules,Tname,#'ComponentType'{name=Cname,typespec=Type}) ->
    NewTname = [Cname|Tname],
    %% The tag is set to [] to avoid that it is
    %% taken into account twice, both as a component/alternative (passed as
    %% argument to the encode decode function and within the encode decode
    %% function it self.
    NewType = Type#type{tag=[]},
    case {asn1ct:get_gen_state_field(active),
	  asn1ct:get_tobe_refed_func(NewTname)} of
	{true,{_,NameList}} ->
	    asn1ct:update_gen_state(namelist,NameList),
	    %% remove to gen_refed_funcs list from tobe_refed_funcs later
	    gen_decode(Erules,NewTname,NewType);
	{No,_} when No == false; No == undefined ->
	    gen_decode(Erules,NewTname,NewType);
	_ ->
	    ok
    end.


gen_decode_user(Erules,D) when is_record(D,typedef) ->
    Typename = [D#typedef.name],
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    BytesVar = "Tlv",
    case asn1ct_gen:type(InnerType) of
	'ASN1_OPEN_TYPE' ->
	    asn1ct_name:new(len),
	    gen_dec_prim(ber, Def#type{def='ASN1_OPEN_TYPE'}, 
			 BytesVar,{string,"TagIn"}, [] , 
			 ?PRIMITIVE,"OptOrMand"),
	    emit({".",nl,nl});
	{primitive,bif} ->
	    asn1ct_name:new(len),
	    gen_dec_prim(ber, Def, BytesVar,{string,"TagIn"},[] , 
			 ?PRIMITIVE,"OptOrMand"),
	    emit([".",nl,nl]);
	{constructed,bif} ->
	    asn1ct:update_namelist(D#typedef.name),
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,D);
	TheType ->
	    DecFunName = mkfuncname(TheType,dec),
	    emit([DecFunName,"(",BytesVar,
		  ", TagIn)"]),
	    emit([".",nl,nl])
    end.


gen_dec_prim(Erules,Att,BytesVar,DoTag,TagIn,Form,OptOrMand) ->
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
%    DoLength = 
    case NewTypeName of
	'BOOLEAN'->
	    emit({"?RT_BER:decode_boolean(",BytesVar,","}),
	    add_func({decode_boolean,2});
	'INTEGER' ->
	    emit({"?RT_BER:decode_integer(",BytesVar,",",
		  {asis,int_constr(SingleValue,ValueRange)},","}),
	    add_func({decode_integer,3});
	{'INTEGER',NamedNumberList} ->
	    emit({"?RT_BER:decode_integer(",BytesVar,",",
		  {asis,int_constr(SingleValue,ValueRange)},",",
		  {asis,NamedNumberList},","}),
	    add_func({decode_integer,4});
	{'ENUMERATED',NamedNumberList} ->
	    emit({"?RT_BER:decode_enumerated(",BytesVar,",",
		  {asis,Constraint},",",
		  {asis,NamedNumberList},","}),
	    add_func({decode_enumerated,4});
	'REAL' ->
	    emit({"?RT_BER:decode_real(",BytesVar,","}),
	    add_func({decode_real,3});
	{'BIT STRING',NamedNumberList} ->
	    case get(compact_bit_string) of
		true ->
		    emit({"?RT_BER:decode_compact_bit_string(",
			  BytesVar,",",{asis,Constraint},",",
			  {asis,NamedNumberList},","}),
		    add_func({decode_compact_bit_string,4});
		_ ->
		    emit({"?RT_BER:decode_bit_string(",BytesVar,",",
			  {asis,Constraint},",",
			  {asis,NamedNumberList},","}),
		    add_func({decode_bit_string,4})
	    end;
	'NULL' ->
	    emit({"?RT_BER:decode_null(",BytesVar,","}),
	    add_func({decode_null,2});
	'OBJECT IDENTIFIER' ->
	    emit({"?RT_BER:decode_object_identifier(",BytesVar,","}),
	    add_func({decode_object_identifier,2});
	'RELATIVE-OID' ->
	    emit({"?RT_BER:decode_relative_oid(",BytesVar,","}),
	    add_func({decode_relative_oid,2});
	'ObjectDescriptor' ->
	    emit({"?RT_BER:decode_restricted_string(",
		  BytesVar,",",{asis,Constraint},",",{asis,?T_ObjectDescriptor},","}),
	    add_func({decode_restricted_string,4});
	'OCTET STRING' ->
	    emit({"?RT_BER:decode_octet_string",AsBin,"(",BytesVar,",",{asis,Constraint},","}),
	    add_func({decode_octet_string,3});
	'NumericString' ->
	    emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		  BytesVar,",",{asis,Constraint},",",{asis,?T_NumericString},","}),
	    add_func({decode_restricted_string,4});
	TString when TString == 'TeletexString';
		     TString == 'T61String' ->
	    emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		  BytesVar,",",{asis,Constraint},",",{asis,?T_TeletexString},","}),
	    add_func({decode_restricted_string,4});
	'VideotexString' ->
	    emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		  BytesVar,",",{asis,Constraint},",",{asis,?T_VideotexString},","}),
	    add_func({decode_restricted_string,4});
	'GraphicString' ->
	    emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		  BytesVar,",",{asis,Constraint},",",{asis,?T_GraphicString},","}),
	    add_func({decode_restricted_string,4});
	'VisibleString' ->
	    emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		  BytesVar,",",{asis,Constraint},",",{asis,?T_VisibleString},","}),
	    add_func({decode_restricted_string,4});
	'GeneralString' ->
	    emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		  BytesVar,",",{asis,Constraint},",",{asis,?T_GeneralString},","}),
	    add_func({decode_restricted_string,4});
	'PrintableString' ->
	    emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		  BytesVar,",",{asis,Constraint},",",{asis,?T_PrintableString},","}),
	    add_func({decode_restricted_string,4});
	'IA5String' ->
	    emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		  BytesVar,",",{asis,Constraint},",",{asis,?T_IA5String},","}),
	    add_func({decode_restricted_string,4}) ;
	'UniversalString' ->
	    emit({"?RT_BER:decode_universal_string",AsBin,"(",
		  BytesVar,",",{asis,Constraint},","}),
	    add_func({decode_universal_string,3});
	'UTF8String' ->
	    emit({"?RT_BER:decode_UTF8_string",AsBin,"(",
		  BytesVar,","}),
	    add_func({decode_UTF8_string,2});
	'BMPString' ->
	    emit({"?RT_BER:decode_BMP_string",AsBin,"(",
		  BytesVar,",",{asis,Constraint},","}),
	    add_func({decode_BMP_string,3});
	'UTCTime' ->
	    emit({"?RT_BER:decode_utc_time",AsBin,"(",
		  BytesVar,",",{asis,Constraint},","}),
	    add_func({decode_utc_time,3});
	'GeneralizedTime' ->
	    emit({"?RT_BER:decode_generalized_time",AsBin,"(",
		  BytesVar,",",{asis,Constraint},","}),
	    add_func({decode_generalized_time,3});
	'ASN1_OPEN_TYPE' ->
	    emit(["?RT_BER:decode_open_type_as_binary(",
		  BytesVar,","]),
	    add_func({decode_open_type_as_binary,3});
	#'ObjectClassFieldType'{} ->
		case asn1ct_gen:get_inner(Att#type.def) of
		    {fixedtypevaluefield,_,InnerType} -> 
			gen_dec_prim(Erules,InnerType,BytesVar,DoTag,TagIn,Form,OptOrMand);
		    'ASN1_OPEN_TYPE' ->
			emit(["?RT_BER:decode_open_type_as_binary(",
			      BytesVar,","]),
			add_func({decode_open_type_as_binary,3});
		    Other ->
			exit({'can not decode' ,Other})
		end;
	Other ->
	    exit({'can not decode' ,Other})
    end,

    case {DoTag,NewTypeName} of
	{_,#'ObjectClassFieldType'{}} ->
	    case asn1ct_gen:get_inner(Att#type.def) of
		'ASN1_OPEN_TYPE' ->
		    emit([{asis,DoTag},asn1ct_gen:nif_parameter(),")"]);
		_ -> ok
	    end;
	{{string,TagStr},'ASN1_OPEN_TYPE'} ->
	    emit([TagStr,asn1ct_gen:nif_parameter(),")"]);
	{_,'ASN1_OPEN_TYPE'} ->
	    emit([{asis,DoTag},asn1ct_gen:nif_parameter(),")"]);
	{{string,TagStr},_} ->
	    emit([TagStr,")"]);
	_ when is_list(DoTag) ->
	    emit([{asis,DoTag},")"])
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

gen_obj_code(Erules,_Module,Obj) when is_record(Obj,typedef) ->
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
    gen_decode_constr_type(Erules,DecConstructed),
    emit_tlv_format_function();
gen_obj_code(_Erules,_Module,Obj) when is_record(Obj,pobjectdef) ->
    ok.

gen_encode_objectfields(ClassName,[{typefield,Name,OptOrMand}|Rest],
			ObjName,ObjectFields,ConstrAcc) ->
    EmitFuncClause =
	fun(Arg) ->
		emit(["'enc_",ObjName,"'(",{asis,Name},
		      ", ",Arg,", _RestPrimFieldName) ->",nl])
	end,
%     emit(["'enc_",ObjName,"'(",{asis,Name},
% 	  ", Val, RestPrimFieldName) ->",nl]),
    MaybeConstr=
	case {get_object_field(Name,ObjectFields),OptOrMand} of
	    {false,'MANDATORY'} -> %% this case is illegal
		exit({error,{asn1,{"missing mandatory field in object",
				   ObjName}}});
	    {false,'OPTIONAL'} ->
		EmitFuncClause("Val"),
		emit(["   {Val,0}"]),
		[];
	    {false,{'DEFAULT',DefaultType}} ->
		EmitFuncClause("Val"),
		gen_encode_default_call(ClassName,Name,DefaultType);
	    {{Name,TypeSpec},_} ->
		%% A specified field owerwrites any 'DEFAULT' or
		%% 'OPTIONAL' field in the class
		EmitFuncClause("Val"),
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
    CurrentMod = get(currmod),
    EmitFuncClause =
	fun(Args) ->
		emit(["'enc_",ObjName,"'(",{asis,Name},
		      ", ",Args,") ->",nl])
	end,
%     emit(["'enc_",ObjName,"'(",{asis,Name},
% 	  ", Val,[H|T]) ->",nl]),
    case {get_object_field(Name,ObjectFields),OptOrMand} of
	{false,'MANDATORY'} ->
	    exit({error,{asn1,{"missing mandatory field in object",
			       ObjName}}});
	{false,'OPTIONAL'} ->
	    EmitFuncClause("_,_"),
	    emit(["  exit({error,{'use of missing field in object', ",{asis,Name},
		  "}})"]);
	{false,{'DEFAULT',_DefaultObject}} ->
	    exit({error,{asn1,{"not implemented yet",Name}}});
	{{Name,#'Externalvaluereference'{module=CurrentMod,
					 value=TypeName}},_} ->
	    EmitFuncClause(" Val, [H|T]"),
	    emit({indent(3),"'enc_",TypeName,"'(H, Val, T)"});
	{{Name,#'Externalvaluereference'{module=M,value=TypeName}},_} ->
	    EmitFuncClause(" Val, [H|T]"),
	    emit({indent(3),"'",M,"':'enc_",TypeName,"'(H, Val, T)"});
	{{Name,TypeSpec},_} ->
	    EmitFuncClause(" Val, [H|T]"),
	    case TypeSpec#typedef.name of
		{ExtMod,TypeName} ->
		    emit({indent(3),"'",ExtMod,"':'enc_",TypeName,
			  "'(H, Val, T)"});
		TypeName ->
		    emit({indent(3),"'enc_",TypeName,"'(H, Val, T)"})
	    end
    end,
    case more_genfields(Rest) of
	true ->
	    emit([";",nl]);
	false ->
	    emit([".",nl])
    end,
    gen_encode_objectfields(ClassName,Rest,ObjName,ObjectFields,ConstrAcc);
    

gen_encode_objectfields(ClassName,[_C|Cs],O,OF,Acc) ->
    gen_encode_objectfields(ClassName,Cs,O,OF,Acc);
gen_encode_objectfields(_,[],_,_,Acc) ->
    Acc.

gen_encode_constr_type(Erules,[TypeDef|Rest]) when is_record(TypeDef,typedef) ->
    case is_already_generated(enc,TypeDef#typedef.name) of
	true -> ok;
	_ -> gen_encode_user(Erules,TypeDef)
    end,
    gen_encode_constr_type(Erules,Rest);
gen_encode_constr_type(_,[]) ->
    ok.

gen_encode_field_call(_ObjName,_FieldName,
		      #'Externaltypereference'{module=M,type=T}) ->
    CurrentMod = get(currmod),
    TDef = asn1_db:dbget(M,T),
    Def = TDef#typedef.typespec,
    OTag = Def#type.tag,
    Tag = [encode_tag_val(decode_class(X#tag.class),
			  X#tag.form,X#tag.number)||
	      X <- OTag],
    if
	M == CurrentMod ->
	    emit({"   'enc_",T,"'(Val, ",{asis,Tag},")"}),
	    [];
	true ->
	    emit({"   '",M,"':'enc_",T,"'(Val, ",{asis,Tag},")"}),
	    []
    end;
gen_encode_field_call(ObjName,FieldName,Type) ->
    Def = Type#typedef.typespec,
    OTag = Def#type.tag,
    Tag = [encode_tag_val(decode_class(X#tag.class),
			  X#tag.form,X#tag.number)||
	      X <- OTag],
    case Type#typedef.name of
	{primitive,bif} -> %%tag should be the primitive tag
% 	    OTag = Def#type.tag,
% 	    Tag = [encode_tag_val(decode_class(X#tag.class),
% 				  X#tag.form,X#tag.number)||
% 		      X <- OTag],
	    gen_encode_prim(ber,Def,{asis,lists:reverse(Tag)},
			    "Val"),
	    [];
	{constructed,bif} ->
	    emit({"   'enc_",ObjName,'_',FieldName,
		  "'(Val,",{asis,Tag},")"}),
	    [Type#typedef{name=list_to_atom(lists:concat([ObjName,'_',FieldName]))}];
	{ExtMod,TypeName} ->
	    emit({"   '",ExtMod,"':'enc_",TypeName,
		  "'(Val,",{asis,Tag},")"}),
	    [];
	TypeName ->
	    emit({"   'enc_",TypeName,"'(Val,",{asis,Tag},")"}),
	    []
    end.

gen_encode_default_call(ClassName,FieldName,Type) ->
    CurrentMod = get(currmod),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    OTag = Type#type.tag,
    Tag = [encode_tag_val(decode_class(X#tag.class),X#tag.form,X#tag.number)|| X <- OTag],
    case asn1ct_gen:type(InnerType) of
    	{constructed,bif} ->
%%	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,Type);
	    emit(["   'enc_",ClassName,'_',FieldName,"'(Bytes)"]),
	    [#typedef{name=list_to_atom(lists:concat([ClassName,'_',FieldName])),
		      typespec=Type}];
	{primitive,bif} ->
	    gen_encode_prim(ber,Type,{asis,lists:reverse(Tag)},"Val"),
	    [];
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    emit(["   'enc_",Etype,"'(Val, ",{asis,Tag},")",nl]),
	    [];
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'enc_",Etype,"'(Val, ",{asis,Tag},")",nl]),
	    []
% 	'ASN1_OPEN_TYPE' ->
% 	    emit(["%% OPEN TYPE",nl]),
% 	    gen_encode_prim(ber,
% 			    Type#type{def='ASN1_OPEN_TYPE'},
% 			    "TagIn","Val"),
% 	    emit([".",nl])
    end.
    
%%%%%%%%%%%%%%%%

gen_decode_objectfields(ClassName,[{typefield,Name,OptOrMand}|Rest],
			ObjName,ObjectFields,ConstrAcc) ->
    EmitFuncClause =
	fun(Arg) ->
		emit(["'dec_",ObjName,"'(",{asis,Name},
		      ", ",Arg,",_) ->",nl])
	end,
%     emit(["'dec_",ObjName,"'(",{asis,Name},
% 	  ", Bytes, RestPrimFieldName) ->",nl]),
    MaybeConstr=
	case {get_object_field(Name,ObjectFields),OptOrMand} of
	    {false,'MANDATORY'} -> %% this case is illegal
		exit({error,{asn1,{"missing mandatory field in object",
				   ObjName}}});
	    {false,'OPTIONAL'} ->
		EmitFuncClause(" Bytes"),
		emit(["   Bytes"]),
		[];
	    {false,{'DEFAULT',DefaultType}} ->
		EmitFuncClause("Bytes"),
		emit_tlv_format("Bytes"),
		gen_decode_default_call(ClassName,Name,"Tlv",DefaultType);
	    {{Name,TypeSpec},_} ->
		%% A specified field owerwrites any 'DEFAULT' or
		%% 'OPTIONAL' field in the class
		EmitFuncClause("Bytes"),
		emit_tlv_format("Bytes"),
		gen_decode_field_call(ObjName,Name,"Tlv",TypeSpec)
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
    CurrentMod = get(currmod),
    EmitFuncClause =
	fun(Args) ->
		emit(["'dec_",ObjName,"'(",{asis,Name},
		      ", ",Args,") ->",nl])
	end,
%     emit(["'dec_",ObjName,"'(",{asis,Name},
% 	  ", Bytes,[H|T]) ->",nl]),
%     emit_tlv_format("Bytes"),
    case {get_object_field(Name,ObjectFields),OptOrMand} of
	{false,'MANDATORY'} ->
	    exit({error,{asn1,{"missing mandatory field in object",
			       ObjName}}});
	{false,'OPTIONAL'} ->
	    EmitFuncClause("_,_"),
	    emit(["  exit({error,{'illegal use of missing field in object', ",{asis,Name},
		  "}})"]);
	{false,{'DEFAULT',_DefaultObject}} ->
	    exit({error,{asn1,{"not implemented yet",Name}}});
	{{Name,#'Externalvaluereference'{module=CurrentMod,
					 value=TypeName}},_} ->
	    EmitFuncClause("Bytes,[H|T]"),
	    emit({indent(3),"'dec_",TypeName,"'(H, Bytes, T)"});
	{{Name,#'Externalvaluereference'{module=M,value=TypeName}},_} ->
	    EmitFuncClause("Bytes,[H|T]"),
	    emit({indent(3),"'",M,"':'dec_",TypeName,
		  "'(H, Bytes, T)"});
	{{Name,TypeSpec},_} ->
	    EmitFuncClause("Bytes,[H|T]"),
%	    emit_tlv_format("Bytes"),
	    case TypeSpec#typedef.name of
		{ExtMod,TypeName} ->
		    emit({indent(3),"'",ExtMod,"':'dec_",TypeName,
			  "'(H, Bytes, T)"});
		TypeName ->
		    emit({indent(3),"'dec_",TypeName,"'(H, Bytes, T)"})
	    end
    end,
    case more_genfields(Rest) of
	true ->
	    emit([";",nl]);
	false ->
	    emit([".",nl])
    end,
    gen_decode_objectfields(ClassName,Rest,ObjName,ObjectFields,ConstrAcc);
gen_decode_objectfields(CN,[_C|Cs],O,OF,CAcc) ->
    gen_decode_objectfields(CN,Cs,O,OF,CAcc);
gen_decode_objectfields(_,[],_,_,CAcc) ->
    CAcc.

emit_tlv_format(Bytes) ->
    notice_tlv_format_gen(), % notice for generating of tlv_format/1
    emit(["  Tlv = tlv_format(",Bytes,"),",nl]).

notice_tlv_format_gen() ->
    Module = get(currmod),
%    io:format("Noticed: ~p~n",[Module]),
    case get(tlv_format) of
	{done,Module} ->
	    ok;
	_ -> % true or undefined
	    put(tlv_format,true)
    end.

emit_tlv_format_function() ->
    Module = get(currmod),
%    io:format("Tlv formated: ~p",[Module]),
    case get(tlv_format) of
	true ->
%	    io:format(" YES!~n"),
	    emit_tlv_format_function1(),
	    put(tlv_format,{done,Module});
	_ ->
%	    io:format(" NO!~n"),
	    ok
    end.
emit_tlv_format_function1() ->
    emit(["tlv_format(Bytes) when is_binary(Bytes) ->",nl,
	  "  {Tlv,_}=?RT_BER:decode(Bytes",asn1ct_gen:nif_parameter(),"),",nl,
	  "  Tlv;",nl,
	  "tlv_format(Bytes) ->",nl,
	  "  Bytes.",nl]).


gen_decode_constr_type(Erules,[TypeDef|Rest]) when is_record(TypeDef,typedef) ->
    case is_already_generated(dec,TypeDef#typedef.name) of
	true -> ok;
	_ ->
	    gen_decode(Erules,TypeDef)
    end,
    gen_decode_constr_type(Erules,Rest);
gen_decode_constr_type(_,[]) ->
    ok.

%%%%%%%%%%%
gen_decode_field_call(_ObjName,_FieldName,Bytes,
		      #'Externaltypereference'{module=M,type=T}) ->
    CurrentMod = get(currmod),
    TDef = asn1_db:dbget(M,T),
    Def = TDef#typedef.typespec,
    OTag = Def#type.tag,
    Tag = [(decode_class(X#tag.class) bsl 10) + X#tag.number || 
	      X <- OTag],
    if
	M == CurrentMod ->
	    emit({"   'dec_",T,"'(",Bytes,
		  ", ",{asis,Tag},")"}),
	    [];
	true ->
	    emit({"   '",M,"':'dec_",T,
		  "'(",Bytes,", ",{asis,Tag},")"}),
	    []
    end;
gen_decode_field_call(ObjName,FieldName,Bytes,Type) ->
    Def = Type#typedef.typespec,
    OTag = Def#type.tag,
    Tag = [(decode_class(X#tag.class) bsl 10) + X#tag.number || 
	      X <- OTag],
    case Type#typedef.name of
	{primitive,bif} -> %%tag should be the primitive tag
	    gen_dec_prim(ber,Def,Bytes,Tag,"TagIn",?PRIMITIVE,
			 opt_or_default),
	    [];
	{constructed,bif} ->
	    emit({"   'dec_",ObjName,'_',FieldName,
		  "'(",Bytes,",",{asis,Tag},")"}),
	    [Type#typedef{name=list_to_atom(lists:concat([ObjName,'_',FieldName]))}];
	{ExtMod,TypeName} ->
	    emit({"   '",ExtMod,"':'dec_",TypeName,
		  "'(",Bytes,",",{asis,Tag},")"}),
	    [];
	TypeName ->
	    emit({"   'dec_",TypeName,"'(",Bytes,",",{asis,Tag},")"}),
	    []
    end.

gen_decode_default_call(ClassName,FieldName,Bytes,Type) ->
    CurrentMod = get(currmod),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    OTag = Type#type.tag,
    Tag = [(decode_class(X#tag.class) bsl 10) + X#tag.number || X <- OTag],
    case asn1ct_gen:type(InnerType) of
    	{constructed,bif} ->
	    emit(["   'dec_",ClassName,'_',FieldName,"'(",Bytes,",",
		  {asis,Tag},")"]),
	    [#typedef{name=list_to_atom(lists:concat([ClassName,'_',
						      FieldName])),
		      typespec=Type}];
	{primitive,bif} ->
	    gen_dec_prim(ber,Type,Bytes,Tag,"TagIn",
			 ?PRIMITIVE,opt_or_default),
	    [];
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    emit(["   'dec_",Etype,"'(",Bytes, " ,",{asis,Tag},")",nl]),
	    [];
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'dec_",Etype,"'(",Bytes,", ",
		  {asis,Tag},")",nl]),
	    []
% 	'ASN1_OPEN_TYPE' ->
% 	    emit(["%% OPEN TYPE",nl]),
% 	    gen_encode_prim(ber,
% 			    Type#type{def='ASN1_OPEN_TYPE'},
% 			    "TagIn","Val"),
% 	    emit([".",nl])
    end.
%%%%%%%%%%%

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
	    gen_objset_code(Erules,ObjSetName,UniqueFName,Set,ExtClassName,ClassDef);
	_ ->
	    gen_objset_code(Erules,ObjSetName,UniqueFName,Set,ClassName,ClassDef)
    end,
    emit(nl).

gen_objset_code(Erules,ObjSetName,UniqueFName,Set,ClassName,ClassDef)->
    ClassFields = get_class_fields(ClassDef),
    InternalFuncs=gen_objset_enc(Erules,ObjSetName,UniqueFName,Set,
				 ClassName,ClassFields,1,[]),
    gen_objset_dec(Erules,ObjSetName,UniqueFName,Set,ClassName,ClassFields,1),
    gen_internal_funcs(Erules,InternalFuncs).

%% gen_objset_enc iterates over the objects of the object set
gen_objset_enc(_,_,{unique,undefined},_,_,_,_,_) ->
    %% There is no unique field in the class of this object set
    %% don't bother about the constraint
    [];
gen_objset_enc(Erules,ObjSName,UniqueName,
	       [{ObjName,Val,Fields},T|Rest],ClName,ClFields,
	       NthObj,Acc)->
    emit({"'getenc_",ObjSName,"'(",{asis,UniqueName},",",{asis,Val},
	  ") ->",nl}),
    CurrMod = get(currmod),
    {InternalFunc,NewNthObj}=
	case ObjName of
	    {no_mod,no_name} ->
		gen_inlined_enc_funs(Fields,ClFields,ObjSName,NthObj);
	    {CurrMod,Name} ->
		emit({"    fun 'enc_",Name,"'/3"}),
		{[],NthObj};
	    {ModuleName,Name} ->
		emit_ext_fun(enc,ModuleName,Name),
%		emit(["    {'",ModuleName,"', 'enc_",Name,"'}"]),
		{[],NthObj};
	    _ ->
		emit({"    fun 'enc_",ObjName,"'/3"}),
		{[],NthObj}
	end,
    emit({";",nl}),
    gen_objset_enc(Erules,ObjSName,UniqueName,[T|Rest],ClName,ClFields,
		   NewNthObj,InternalFunc ++ Acc);
gen_objset_enc(_,ObjSetName,UniqueName,
	       [{ObjName,Val,Fields}],_ClName,ClFields,NthObj,Acc) ->
    emit({"'getenc_",ObjSetName,"'(",{asis,UniqueName},",",
	  {asis,Val},") ->",nl}),
    CurrMod = get(currmod),
    {InternalFunc,_} =
	case ObjName of
	    {no_mod,no_name} ->
		gen_inlined_enc_funs(Fields,ClFields,ObjSetName,NthObj);
	    {CurrMod,Name} ->
		emit({"    fun 'enc_",Name,"'/3"}),
		{[],NthObj};
	    {ModuleName,Name} ->
		emit_ext_fun(enc,ModuleName,Name),
%		emit(["    {'",ModuleName,"', 'enc_",Name,"'}"]),
		{[],NthObj};
	    _ ->
		emit({"    fun 'enc_",ObjName,"'/3"}),
		{[],NthObj}
	end,
    emit([";",nl]),
    emit_default_getenc(ObjSetName,UniqueName),
    emit({".",nl,nl}),
    InternalFunc ++ Acc;
%% See X.681 Annex E for the following case
gen_objset_enc(_,ObjSetName,_UniqueName,['EXTENSIONMARK'],_ClName,
	       _ClFields,_NthObj,Acc) ->
    emit({"'getenc_",ObjSetName,"'(_, _) ->",nl}),
    emit({indent(3),"fun(_, Val, _RestPrimFieldName) ->",nl}),
    emit({indent(6),"Len = case Val of",nl,indent(9),
 	  "Bin when is_binary(Bin) -> size(Bin);",nl,indent(9),
 	  "_ -> length(Val)",nl,indent(6),"end,"}),
    emit({indent(6),"{Val,Len}",nl}),
    emit({indent(3),"end.",nl,nl}),
    Acc;
gen_objset_enc(_,_,_,[],_,_,_,Acc) ->
    Acc.

emit_ext_fun(EncDec,ModuleName,Name) ->
    emit([indent(3),"fun(T,V,O) -> '",ModuleName,"':'",EncDec,"_",
	  Name,"'(T,V,O) end"]).
    
emit_default_getenc(ObjSetName,UniqueName) ->
    emit(["'getenc_",ObjSetName,"'(",{asis,UniqueName},", ErrV) ->",nl]),
    emit([indent(3),"fun(C,V,_) -> exit({'Type not compatible with table constraint',{component,C},{value,V}, {unique_name_and_value,",{asis,UniqueName},", ErrV}}) end"]).

%% gen_inlined_enc_funs for each object iterates over all fields of a
%% class, and for each typefield it checks if the object has that
%% field and emits the proper code.
gen_inlined_enc_funs(Fields,[{typefield,Name,_}|Rest],
		     ObjSetName,NthObj) ->
    CurrMod = get(currmod),
    InternalDefFunName = asn1ct_gen:list2name([NthObj,Name,ObjSetName]),
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when is_record(Type,type) ->
	    emit({indent(3),"fun(Type, Val, _RestPrimFieldName) ->",nl,
		  indent(6),"case Type of",nl}),
	    {Ret,N}=emit_inner_of_fun(Type,InternalDefFunName),
	    gen_inlined_enc_funs1(Fields,Rest,ObjSetName,NthObj+N,Ret);
	{value,{_,Type}} when is_record(Type,typedef) ->
	    emit({indent(3),"fun(Type, Val, _RestPrimFieldName) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    {Ret,N}=emit_inner_of_fun(Type,InternalDefFunName),
	    gen_inlined_enc_funs1(Fields,Rest,ObjSetName,NthObj+N,Ret);
	{value,{_,#'Externaltypereference'{module=M,type=T}}} ->
	    emit([indent(3),"fun(Type, Val, _RestPrimFieldName) ->",nl,
		  indent(6),"case Type of",nl]),
	    emit([indent(9),{asis,Name}," ->",nl]),
	    if
		M == CurrMod ->
		    emit([indent(12),"'enc_",T,"'(Val)"]);
		true ->
		    #typedef{typespec=Type} = asn1_db:dbget(M,T),
		    OTag = Type#type.tag,
%% 		    Tag = [encode_tag_val((decode_class(X#tag.class) bsl 10) +
%% 					  X#tag.number) ||
%% 			      X <- OTag],
 		    Tag = [encode_tag_val(decode_class(X#tag.class),
 					  X#tag.form,X#tag.number) ||
 			      X <- OTag],
		    emit([indent(12),"'",M,"':'enc_",T,"'(Val, ",{asis,Tag},")"])
	    end,
	    gen_inlined_enc_funs1(Fields,Rest,ObjSetName,NthObj,[]);
	false ->
	    %% This field was not present in the object thus there
	    %% were no type in the table and we therefore generate
	    %% code that returns the input for application treatment.
	    emit([indent(3),"fun(Type, Val, _RestPrimFieldName) ->",nl,
		  indent(6),"case Type of",nl,
		  indent(9),{asis,Name}," ->",nl,
		  indent(12),"Len = case Val of",nl,
		  indent(15),"B when is_binary(B) -> size(B);",nl,
		  indent(15),"_ -> length(Val)",nl,
		  indent(12),"end,",nl,
		  indent(12),"{Val,Len}"]),
	    gen_inlined_enc_funs1(Fields,Rest,ObjSetName,NthObj,[])
    end;
gen_inlined_enc_funs(Fields,[_|Rest],ObjSetName,NthObj) ->
    gen_inlined_enc_funs(Fields,Rest,ObjSetName,NthObj);
gen_inlined_enc_funs(_,[],_,NthObj) ->
    {[],NthObj}.

gen_inlined_enc_funs1(Fields,[{typefield,Name,_}|Rest],ObjSetName,
		      NthObj,Acc) ->
    CurrMod = get(currmod),
    InternalDefFunName = asn1ct_gen:list2name([NthObj,Name,ObjSetName]),
    {Acc2,NAdd}=
	case lists:keysearch(Name,1,Fields) of
	    {value,{_,Type}} when is_record(Type,type) ->
		emit({";",nl}),
		{Ret,N}=emit_inner_of_fun(Type,InternalDefFunName),
		{Ret++Acc,N};
	    {value,{_,Type}} when is_record(Type,typedef) ->
		emit({";",nl,indent(9),{asis,Name}," ->",nl}),
		{Ret,N}=emit_inner_of_fun(Type,InternalDefFunName),
		{Ret++Acc,N};
	    {value,{_,#'Externaltypereference'{module=M,type=T}}} ->
		emit({";",nl,indent(9),{asis,Name}," ->",nl}),
		if
		    M == CurrMod ->
			emit([indent(12),"'enc_",T,"'(Val)"]);
		    true ->
			#typedef{typespec=Type} = asn1_db:dbget(M,T),
			OTag = Type#type.tag,
			Tag = [encode_tag_val(decode_class(X#tag.class), 
					      X#tag.form,X#tag.number) ||
				  X <- OTag],
			emit([indent(12),"'",M,"':'enc_",T,"'(Val, ",{asis,Tag},")"])
		end,
		{Acc,0};
	    false ->
		%% This field was not present in the object thus there
		%% were no type in the table and we therefore generate
		%% code that returns the input for application
		%% treatment.
		emit([";",nl,indent(9),{asis,Name}," ->",nl]),
		emit([indent(12),"Len = case Val of",nl,
		      indent(15),"Bin when is_binary(Bin) -> size(Bin);",nl,
		      indent(15),"_ -> length(Val)",nl,indent(12),"end,",nl,
		      indent(12),"{Val,Len}"]),
		{Acc,0}
	end,
    gen_inlined_enc_funs1(Fields,Rest,ObjSetName,NthObj+NAdd,Acc2);
gen_inlined_enc_funs1(Fields,[_|Rest],ObjSetName,NthObj,Acc)->
    gen_inlined_enc_funs1(Fields,Rest,ObjSetName,NthObj,Acc);
gen_inlined_enc_funs1(_,[],_,NthObj,Acc) ->
    emit({nl,indent(6),"end",nl}),
    emit({indent(3),"end"}),
    {Acc,NthObj}.

emit_inner_of_fun(TDef=#typedef{name={ExtMod,Name},typespec=Type},
		  InternalDefFunName) ->
    OTag = Type#type.tag,
    Tag = [encode_tag_val(decode_class(X#tag.class),X#tag.form,X#tag.number)|| X <- OTag],
% remove    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    case {ExtMod,Name} of
	{primitive,bif} ->
	    emit(indent(12)),
	    gen_encode_prim(ber,Type,[{asis,lists:reverse(Tag)}],"Val"),
	    {[],0};
	{constructed,bif} ->
	    emit([indent(12),"'enc_",
		  InternalDefFunName,"'(Val, ",{asis,Tag},")"]),
	    {[TDef#typedef{name=InternalDefFunName}],1};
	_ ->
	    emit({indent(12),"'",ExtMod,"':'enc_",Name,"'(Val",{asis,Tag},")"}),
	    {[],0}
    end;
emit_inner_of_fun(#typedef{name=Name},_) ->
%    OTag = Type#type.tag,
% remove    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
%    Tag = [encode_tag_val(decode_class(X#tag.class),X#tag.form,X#tag.number)|| X <- OTag],
    emit({indent(12),"'enc_",Name,"'(Val)"}),
    {[],0};
emit_inner_of_fun(Type,_) when is_record(Type,type) ->
    CurrMod = get(currmod),
%    OTag = Type#type.tag,
% remove    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
%    Tag = [encode_tag_val(decode_class(X#tag.class),X#tag.form,X#tag.number)|| X <- OTag],
    case Type#type.def of
	Def when is_atom(Def) ->
	    OTag = Type#type.tag,
	    Tag = [encode_tag_val(decode_class(X#tag.class),
				  X#tag.form,X#tag.number)||X <- OTag],
	    emit([indent(9),Def," ->",nl,indent(12)]),
	    gen_encode_prim(ber,Type,{asis,lists:reverse(Tag)},"Val");
	TRef when is_record(TRef,typereference) ->
	    T = TRef#typereference.val,
	    emit([indent(9),T," ->",nl,indent(12),"'enc_",T,
		  "'(Val)"]);
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit([indent(9),T," ->",nl,indent(12),"'enc_",T,
		  "'(Val)"]);
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    #typedef{typespec=ExtType} = asn1_db:dbget(ExtMod,T),
	    OTag = ExtType#type.tag,
	    Tag = [encode_tag_val(decode_class(X#tag.class),
				  X#tag.form,X#tag.number) ||
		      X <- OTag],
	    emit([indent(9),T," ->",nl,indent(12),ExtMod,":'enc_",
		  T,"'(Val, ",{asis,Tag},")"])
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
    emit(["'getdec_",ObjSName,"'(",{asis,UniqueName},",",
	  {asis,Val},") ->",nl]),
    CurrMod = get(currmod),
    NewNthObj=
	case ObjName of
	    {no_mod,no_name} ->
		gen_inlined_dec_funs(Fields,ClFields,ObjSName,NthObj);
	    {CurrMod,Name} ->
		emit(["    fun 'dec_",Name,"'/3"]),
		NthObj;
	    {ModuleName,Name} ->
		emit_ext_fun(dec,ModuleName,Name),
%		emit(["    {'",ModuleName,"', 'dec_",Name,"'}"]),
		NthObj;
	    _ ->
		emit(["    fun 'dec_",ObjName,"'/3"]),
		NthObj
	end,
    emit([";",nl]),
    gen_objset_dec(Erules,ObjSName,UniqueName,[T|Rest],ClName,
		   ClFields,NewNthObj);
gen_objset_dec(_,ObjSetName,UniqueName,[{ObjName,Val,Fields}],
	       _ClName,ClFields,NthObj) ->
    emit(["'getdec_",ObjSetName,"'(",{asis,UniqueName},",",
	  {asis,Val},") ->",nl]),
    CurrMod = get(currmod),
    case ObjName of
	{no_mod,no_name} ->
	    gen_inlined_dec_funs(Fields,ClFields,ObjSetName,NthObj);
	{CurrMod,Name} ->
	    emit(["    fun 'dec_",Name,"'/3"]);
	{ModuleName,Name} ->
	    emit_ext_fun(dec,ModuleName,Name);
%		emit(["    {'",ModuleName,"', 'dec_",Name,"'}"]);
	_ ->
	    emit(["    fun 'dec_",ObjName,"'/3"])
    end,
    emit([";",nl]),
    emit_default_getdec(ObjSetName,UniqueName),
    emit([".",nl,nl]),
    ok;
gen_objset_dec(Erules,ObjSetName,_UniqueName,['EXTENSIONMARK'],_ClName,
	       _ClFields,_NthObj) ->
    emit(["'getdec_",ObjSetName,"'(_, _) ->",nl]),
    emit([indent(2),"fun(_,Bytes, _RestPrimFieldName) ->",nl]),
    
    case Erules of
	ber_bin_v2 ->
	    emit([indent(4),"case Bytes of",nl,
		  indent(6),"Bin when is_binary(Bin) -> ",nl,
		  indent(8),"Bin;",nl,
		  indent(6),"_ ->",nl,
		  indent(8),"?RT_BER:encode(Bytes",driver_parameter(),")",nl,
		  indent(4),"end",nl]);
	_ ->
	    emit([indent(6),"Len = case Bytes of",nl,indent(9),
		  "Bin when is_binary(Bin) -> size(Bin);",nl,indent(9),
		  "_ -> length(Bytes)",nl,indent(6),"end,"]),
	    emit([indent(4),"{Bytes,[],Len}",nl])
    end,
    emit([indent(2),"end.",nl,nl]),
    ok;
gen_objset_dec(_,_,_,[],_,_,_) ->
    ok.

driver_parameter() ->
    Options = get(encoding_options),
    case {lists:member(driver,Options),lists:member(nif,Options)} of
	{true,_} -> ",nif";
	{_,true} -> ",nif";
	_ ->  ",erlang"
    end.

emit_default_getdec(ObjSetName,UniqueName) ->
    emit(["'getdec_",ObjSetName,"'(",{asis,UniqueName},", ErrV) ->",nl]),
    emit([indent(2), "fun(C,V,_) -> exit({{component,C},{value,V},{unique_name_and_value,",{asis,UniqueName},", ErrV}}) end"]).

gen_inlined_dec_funs(Fields,[{typefield,Name,Prop}|Rest],
		     ObjSetName,NthObj) ->
    DecProp = case Prop of
		  'OPTIONAL' -> opt_or_default;
		  {'DEFAULT',_} -> opt_or_default;
		  _ -> mandatory
	      end,
    CurrMod = get(currmod),
    InternalDefFunName = [NthObj,Name,ObjSetName],
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when is_record(Type,type) ->
	    emit([indent(3),"fun(Type, Bytes, _RestPrimFieldName) ->",
		  nl,indent(6),"case Type of",nl]),
	    N=emit_inner_of_decfun(Type,DecProp,InternalDefFunName),
	    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj+N);
	{value,{_,Type}} when is_record(Type,typedef) ->
	    emit([indent(3),"fun(Type, Bytes, _RestPrimFieldName) ->",
		  nl,indent(6),"case Type of",nl]),
	    emit([indent(9),{asis,Name}," ->",nl]),
	    N=emit_inner_of_decfun(Type,DecProp,InternalDefFunName),
	    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj+N);
	{value,{_,#'Externaltypereference'{module=M,type=T}}} ->
	    emit([indent(3),"fun(Type, Bytes, _RestPrimFieldName) ->",
		  nl,indent(6),"case Type of",nl]),
	    emit([indent(9),{asis,Name}," ->",nl]),
	    if
		M == CurrMod ->
		    emit([indent(12),"'dec_",T,"'(Bytes)"]);
		true ->
		    #typedef{typespec=Type} = asn1_db:dbget(M,T),
		    OTag = Type#type.tag,
		    Tag = [(decode_class(X#tag.class) bsl 10) + X#tag.number ||
			      X <- OTag],
		    emit([indent(12),"'",M,"':'dec_",T,"'(Bytes, ",{asis,Tag},")"])
	    end,
	    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj);
	false ->
	    emit([indent(3),"fun(Type, Bytes, _RestPrimFieldName) ->",
		  nl,indent(6),"case Type of",nl,
		  indent(9),{asis,Name}," ->",nl,
		  indent(12),"Len = case Bytes of",nl,
		  indent(15),"B when is_binary(B) -> size(B);",nl,
		  indent(15),"_ -> length(Bytes)",nl,
		  indent(12),"end,",nl,
		  indent(12),"{Bytes,[],Len}"]),
	    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj)
    end;
gen_inlined_dec_funs(Fields,[_H|Rest],ObjSetName,NthObj) ->
    gen_inlined_dec_funs(Fields,Rest,ObjSetName,NthObj);
gen_inlined_dec_funs(_,[],_,NthObj) ->
    NthObj.

gen_inlined_dec_funs1(Fields,[{typefield,Name,Prop}|Rest],
		      ObjSetName,NthObj) ->
    DecProp = case Prop of
		  'OPTIONAL' -> opt_or_default;
		  {'DEFAULT',_} -> opt_or_default;
		  _ -> mandatory
	      end,
    CurrMod = get(currmod),
    InternalDefFunName = [NthObj,Name,ObjSetName],
    N=
	case lists:keysearch(Name,1,Fields) of
	    {value,{_,Type}} when is_record(Type,type) ->
		emit([";",nl]),
		emit_inner_of_decfun(Type,DecProp,InternalDefFunName);
	    {value,{_,Type}} when is_record(Type,typedef) ->
		emit([";",nl,indent(9),{asis,Name}," ->",nl]),
		emit_inner_of_decfun(Type,DecProp,InternalDefFunName);
	    {value,{_,#'Externaltypereference'{module=M,type=T}}} ->
		emit([";",nl,indent(9),{asis,Name}," ->",nl]),
		if
		    M == CurrMod ->
			emit([indent(12),"'dec_",T,"'(Bytes)"]);
		    true ->
			#typedef{typespec=Type} = asn1_db:dbget(M,T),
			OTag = Type#type.tag,
			Tag = [(decode_class(X#tag.class) bsl 10) + 
			       X#tag.number || X <- OTag],
			emit([indent(12),"'",M,"':'dec_",T,"'(Bytes, ",{asis,Tag},")"])
		end,
		0;
	    false ->
		emit([";",nl,
		      indent(9),{asis,Name}," ->",nl,
		      indent(12),"Len = case Bytes of",nl,
		      indent(15),"B when is_binary(B) -> size(B);",nl,
		      indent(15),"_ -> length(Bytes)",nl,
		      indent(12),"end,",nl,
		      indent(12),"{Bytes,[],Len}"]),
		0
    end,
    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj+N);
gen_inlined_dec_funs1(Fields,[_|Rest],ObjSetName,NthObj)->
    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj);
gen_inlined_dec_funs1(_,[],_,NthObj) ->
    emit([nl,indent(6),"end",nl]),
    emit([indent(3),"end"]),
    NthObj.

emit_inner_of_decfun(#typedef{name={ExtName,Name},typespec=Type},Prop,
		     InternalDefFunName) ->
    OTag = Type#type.tag,
%%    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    Tag = [(decode_class(X#tag.class) bsl 10) + X#tag.number || X <- OTag],
    case {ExtName,Name} of
	{primitive,bif} ->
	    emit(indent(12)),
	    gen_dec_prim(ber,Type,"Bytes",Tag,"TagIn",
			 ?PRIMITIVE,Prop),
	    0;
	{constructed,bif} ->
	    emit([indent(12),"'dec_",
% 		  asn1ct_gen:list2name(InternalDefFunName),"'(Bytes, ",Prop,
% 		  ", ",{asis,Tag},")"]),
 		  asn1ct_gen:list2name(InternalDefFunName),"'(Bytes, ",
		  {asis,Tag},")"]),
	    1;
	_ ->
	    emit([indent(12),"'",ExtName,"':'dec_",Name,"'(Bytes, ",
		  {asis,Tag},")"]),
	    0
    end;
emit_inner_of_decfun(#typedef{name=Name},_Prop,_) ->
    emit([indent(12),"'dec_",Name,"'(Bytes)"]),
    0;
emit_inner_of_decfun(Type,Prop,_) when is_record(Type,type) ->
    OTag = Type#type.tag,
%%    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    Tag = [(decode_class(X#tag.class) bsl 10) + X#tag.number || X <- OTag],
    CurrMod = get(currmod),
    Def = Type#type.def,
    InnerType = asn1ct_gen:get_inner(Def),
    WhatKind = asn1ct_gen:type(InnerType),
    case WhatKind of
	{primitive,bif} -> 
	    emit([indent(9),Def," ->",nl,indent(12)]),
	    gen_dec_prim(ber,Type,"Bytes",Tag,"TagIn",
			 ?PRIMITIVE,Prop);
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit([indent(9),T," ->",nl,indent(12),"'dec_",T,
%		  "'(Bytes, ",Prop,")"]);
		  "'(Bytes)"]);
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    emit([indent(9),T," ->",nl,indent(12),ExtMod,":'dec_",
%		  T,"'(Bytes, ",Prop,")"])
		  T,"'(Bytes, ",{asis,Tag},")"])
    end,
    0.

gen_internal_funcs(_,[]) ->
    ok;
gen_internal_funcs(Erules,[TypeDef|Rest]) ->
    gen_encode_user(Erules,TypeDef),
    emit([nl,nl,"'dec_",TypeDef#typedef.name,
%	  "'(Tlv, OptOrMand, TagIn) ->",nl]),
	  "'(Tlv, TagIn) ->",nl]),
    gen_decode_user(Erules,TypeDef),
    gen_internal_funcs(Erules,Rest).


dbdec(Type,Arg) ->
    demit({"io:format(\"decoding: ",{asis,Type},"~w~n\",[",Arg,"]),",nl}).


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
decode_type('ObjectDescriptor') -> 7;
decode_type('EXTERNAL') -> 8;
decode_type('REAL') -> 9;
decode_type('ENUMERATED') -> 10;
decode_type('EMBEDDED_PDV') -> 11;
decode_type('UTF8String') -> 12;
decode_type('RELATIVE-OID') -> 13;
decode_type('SEQUENCE') -> 16;
decode_type('SEQUENCE OF') -> 16;
decode_type('SET') -> 17;
decode_type('SET OF') -> 17;
decode_type('NumericString') -> 18;  
decode_type('PrintableString') -> 19;  
decode_type('TeletexString') -> 20;  
decode_type('T61String') -> 20;
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

%%encode_tag(TagClass(?UNI, APP etc), Form (?PRIM etx), TagInteger) ->  
%%     8bit Int | binary 
encode_tag_val(Class, Form, TagNo) when (TagNo =< 30) -> 
    <<(Class bsr 6):2,(Form bsr 5):1,TagNo:5>>;

encode_tag_val(Class, Form, TagNo) -> 
    {Octets,_Len} = mk_object_val(TagNo),
    BinOct = list_to_binary(Octets),
    <<(Class bsr 6):2, (Form bsr 5):1, 31:5,BinOct/binary>>.

%%%%%%%%%%% 
%% mk_object_val(Value) -> {OctetList, Len} 
%% returns a Val as a list of octets, the 8 bit is allways set to one except 
%% for the last octet, where its 0 
%% 

 
mk_object_val(Val) when Val =< 127 -> 
    {[255 band Val], 1}; 
mk_object_val(Val) -> 
    mk_object_val(Val bsr 7, [Val band 127], 1).  
mk_object_val(0, Ack, Len) -> 
    {Ack, Len}; 
mk_object_val(Val, Ack, Len) -> 
    mk_object_val(Val bsr 7, [((Val band 127) bor 128) | Ack], Len + 1). 

add_func(F={_Func,_Arity}) ->
    ets:insert(asn1_functab,{F}).

%% For BER the ExtensionAdditionGroup notation has no impact on the encoding/decoding
%% and therefore we only filter away the ExtensionAdditionGroup start and end markers
extaddgroup2sequence(ExtList) when is_list(ExtList) ->
    lists:filter(fun(#'ExtensionAdditionGroup'{}) ->
			 false;
		    ('ExtensionAdditionGroupEnd') ->
			 false;
		    (_) ->
			 true
		 end, ExtList).


