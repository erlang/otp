%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(asn1ct_gen_per).

%% Generate erlang module which handles (PER) encode and decode for
%% all types in an ASN.1 module

-include("asn1_records.hrl").

-export([gen_dec_imm/2]).
-export([gen_dec_prim/3,gen_encode_prim_imm/3]).
-export([gen_obj_code/3,gen_objectset_code/2]).
-export([gen_decode/2, gen_decode/3]).
-export([gen_encode/2, gen_encode/3]).
-export([gen_dec_external/2]).
-export([extaddgroup2sequence/1]).
-export([dialyzer_suppressions/1]).

-import(asn1ct_gen, [emit/1]).
-import(asn1ct_func, [call/3]).


%%****************************************
%% Generate ENCODING
%%****************************************

dialyzer_suppressions(#gen{erule=per,aligned=Aligned}) ->
    Mod = case Aligned of
              false -> uper;
              true -> per
          end,
    case asn1ct_func:is_used({Mod,complete,1}) of
	false ->
	    ok;
	true ->
	    emit(["    _ = complete(Arg),",nl])
    end,
    emit(["    ok.",nl]).


gen_encode(Erules,Type) when is_record(Type,typedef) ->
    gen_encode_user(Erules,Type).

gen_encode(Erules,Typename,#'ComponentType'{name=Cname,typespec=Type}) ->
    NewTypename = [Cname|Typename],
    gen_encode(Erules,NewTypename,Type);

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
            Func = enc_func(asn1ct_gen:list2name(Typename)),
            emit([{asis,Func},"(Val",ObjFun,") ->",nl]),
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end.


gen_encode_user(Erules,D) when is_record(D,typedef) ->
    CurrMod = get(currmod),
    Typename = [D#typedef.name],
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    Func = enc_func(asn1ct_gen:list2name(Typename)),
    emit([{asis,Func},"(Val) ->",nl]),
    case asn1ct_gen:type(InnerType) of
	{primitive,bif} ->
	    gen_encode_prim(Erules, Def),
	    emit([".",nl]);
	'ASN1_OPEN_TYPE' ->
	    gen_encode_prim(Erules, Def#type{def='ASN1_OPEN_TYPE'}),
	    emit([".",nl]);
	{constructed,bif} ->
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,D);
	#'Externaltypereference'{module=CurrMod,type=Etype} ->
            emit([{asis,enc_func(Etype)},"(Val).",nl]);
	#'Externaltypereference'{module=Emod,type=Etype} ->
            emit([{asis,Emod},":",{asis,enc_func(Etype)},"(Val).",nl])
    end.


gen_encode_prim(Erules, D) ->
    Value = {var,atom_to_list(asn1ct_gen:mk_var(asn1ct_name:curr(val)))},
    gen_encode_prim(Erules, D, Value).

gen_encode_prim(#gen{erule=per,aligned=Aligned}, #type{}=D, Value) ->
    Imm = gen_encode_prim_imm(Value, D, Aligned),
    asn1ct_imm:enc_cg(Imm, Aligned).

gen_encode_prim_imm(Val, #type{def=Type0,constraint=Constraint}, Aligned) ->
    case simplify_type(Type0) of
	k_m_string ->
	    Type = case Type0 of
		       'GeneralizedTime' -> 'VisibleString';
		       'UTCTime' -> 'VisibleString';
		       _ -> Type0
		   end,
	    asn1ct_imm:per_enc_k_m_string(Val, Type, Constraint, Aligned);
	restricted_string ->
	    ToBinary = {erlang,iolist_to_binary},
	    asn1ct_imm:per_enc_restricted_string(Val, ToBinary, Aligned);
	{'ENUMERATED',NNL} ->
	    asn1ct_imm:per_enc_enumerated(Val, NNL, Aligned);
	'INTEGER' ->
	    asn1ct_imm:per_enc_integer(Val, Constraint, Aligned);
	{'INTEGER',NNL} ->
	    asn1ct_imm:per_enc_integer(Val, NNL, Constraint, Aligned);
	'REAL' ->
	    ToBinary = {real_common,encode_real},
	    asn1ct_imm:per_enc_restricted_string(Val, ToBinary, Aligned);
	{'BIT STRING',NNL} ->
	    case asn1ct:use_legacy_types() of
		false ->
		    asn1ct_imm:per_enc_bit_string(Val, NNL,
						  Constraint, Aligned);
		true ->
		    asn1ct_imm:per_enc_legacy_bit_string(Val, NNL,
							 Constraint, Aligned)
	    end;
	'NULL' ->
	    asn1ct_imm:per_enc_null(Val, Aligned);
	'OBJECT IDENTIFIER' ->
	    ToBinary = {per_common,encode_oid},
	    asn1ct_imm:per_enc_restricted_string(Val, ToBinary, Aligned);
	'RELATIVE-OID' ->
	    ToBinary = {per_common,encode_relative_oid},
	    asn1ct_imm:per_enc_restricted_string(Val, ToBinary, Aligned);
	'BOOLEAN' ->
	    asn1ct_imm:per_enc_boolean(Val, Aligned);
	'OCTET STRING' ->
	    case asn1ct:use_legacy_types() of
		false ->
		    asn1ct_imm:per_enc_octet_string(Val, Constraint, Aligned);
		true ->
		    asn1ct_imm:per_enc_legacy_octet_string(Val, Constraint,
							   Aligned)
	    end;
	'ASN1_OPEN_TYPE' ->
	    case Constraint of
		[#'Externaltypereference'{type=Tname}] ->
		    EncFunc = enc_func(Tname),
		    Imm = [{apply,{local,EncFunc,[]},[Val]}],
		    asn1ct_imm:per_enc_open_type(Imm, Aligned);
		[] ->
		    Imm = [{call,erlang,iolist_to_binary,[Val]}],
		    asn1ct_imm:per_enc_open_type(Imm, Aligned)
	    end
    end.

dec_func(Tname) ->
    list_to_atom(lists:concat(["dec_",Tname])).

enc_func(Tname) ->
    list_to_atom(lists:concat(["enc_",Tname])).

simplify_type(Type) ->
    case Type of
	'BMPString'       -> k_m_string;
	'IA5String'       -> k_m_string;
	'NumericString'   -> k_m_string;
	'PrintableString' -> k_m_string;
	'VisibleString'   -> k_m_string;
	'UniversalString' -> k_m_string;
	'GeneralizedTime' -> k_m_string;
	'UTCTime'         -> k_m_string;
	'TeletexString'   -> restricted_string;
	'T61String'       -> restricted_string;
	'VideotexString'  -> restricted_string;
	'GraphicString'   -> restricted_string;
	'GeneralString'   -> restricted_string;
	'UTF8String'      -> restricted_string;
	'ObjectDescriptor' -> restricted_string;
	Other             -> Other
    end.

%% Object code generating for encoding and decoding
%% ------------------------------------------------

gen_obj_code(_Erules, _Module, #typedef{}) ->
    ok.

%% Object Set code generating for encoding and decoding
%% ----------------------------------------------------
gen_objectset_code(_Erules, _ObjSet) ->
    ok.

%% DECODING *****************************
%%***************************************

gen_decode(Erules, #typedef{}=Type) ->
    DecFunc = dec_func(Type#typedef.name),
    emit([nl,nl,{asis,DecFunc},"(Bytes) ->",nl]),
    gen_decode_user(Erules, Type).

gen_decode(Erules,Tname,#'ComponentType'{name=Cname,typespec=Type}) ->
    NewTname = [Cname|Tname],
    gen_decode(Erules,NewTname,Type);

gen_decode(Erules,Typename,Type) when is_record(Type,type) ->
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
	    emit([nl,
		  {asis,dec_func(asn1ct_gen:list2name(Typename))},
		  "(Bytes",ObjFun,") ->",nl]),
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end.

gen_decode_user(Erules,D) when is_record(D,typedef) ->
    Typename = [D#typedef.name],
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    case asn1ct_gen:type(InnerType) of
	{primitive,bif} ->
	    gen_dec_prim(Erules,Def,"Bytes"),
	    emit([".",nl,nl]);
	'ASN1_OPEN_TYPE' ->
	    gen_dec_prim(Erules,Def#type{def='ASN1_OPEN_TYPE'},"Bytes"),
	    emit([".",nl,nl]);
	{constructed,bif} ->
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,D);
	#'Externaltypereference'{}=Etype ->
	    gen_dec_external(Etype, "Bytes"),
	    emit([".",nl,nl])
    end.

gen_dec_external(Ext, BytesVar) ->
    CurrMod = get(currmod),
    #'Externaltypereference'{module=Mod,type=Type} = Ext,
    emit([case CurrMod of
	      Mod -> [];
	      _ -> [{asis,Mod},":"]
	  end,{asis,dec_func(Type)},"(",BytesVar,")"]).

gen_dec_imm(#gen{erule=per,aligned=Aligned}, #type{def=Name,constraint=C}) ->
    gen_dec_imm_1(Name, C, Aligned).

gen_dec_imm_1('ASN1_OPEN_TYPE', Constraint, Aligned) ->
    imm_decode_open_type(Constraint, Aligned);
gen_dec_imm_1({'BIT STRING',NNL}, Constr0, Aligned) ->
    Constr = asn1ct_imm:effective_constraint(bitstring, Constr0),
    Imm = asn1ct_imm:per_dec_raw_bitstring(Constr, Aligned),
    case NNL of
	[] ->
	    case asn1ct:get_bit_string_format() of
		compact ->
		    gen_dec_bit_string(decode_compact_bit_string,
				       Imm);
		legacy ->
		    gen_dec_bit_string(decode_legacy_bit_string,
				       Imm);
		bitstring ->
		    gen_dec_copy_bitstring(Imm)
	    end;
	[_|_] ->
	    D = fun(V, Buf) ->
			As = [V,{asis,NNL}],
			Call = {call,per_common,decode_named_bit_string,As},
			emit(["{",Call,com,Buf,"}"])
		end,
	    {call,D,Imm}
    end;
gen_dec_imm_1('NULL', _Constr, _Aligned) ->
    {value,'NULL'};
gen_dec_imm_1('BOOLEAN', _Constr, _Aligned) ->
    asn1ct_imm:per_dec_boolean();
gen_dec_imm_1({'ENUMERATED',{Base,Ext}}, _Constr, Aligned) ->
    asn1ct_imm:per_dec_enumerated(Base, Ext, Aligned);
gen_dec_imm_1({'ENUMERATED',NamedNumberList}, _Constr, Aligned) ->
    asn1ct_imm:per_dec_enumerated(NamedNumberList, Aligned);
gen_dec_imm_1('INTEGER', Constr, Aligned) ->
    asn1ct_imm:per_dec_integer(Constr, Aligned);
gen_dec_imm_1({'INTEGER',NamedNumberList}, Constraint, Aligned) ->
    asn1ct_imm:per_dec_named_integer(Constraint,
				     NamedNumberList,
				     Aligned);
gen_dec_imm_1('BMPString'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('NumericString'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('PrintableString'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('VisibleString'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('IA5String'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('UniversalString'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('UTCTime', Constraint, Aligned) ->
    gen_dec_k_m_string('VisibleString', Constraint, Aligned);
gen_dec_imm_1('GeneralizedTime', Constraint, Aligned) ->
    gen_dec_k_m_string('VisibleString', Constraint, Aligned);
gen_dec_imm_1('OCTET STRING', Constraint, Aligned) ->
    SzConstr = asn1ct_imm:effective_constraint(bitstring, Constraint),
    Imm = asn1ct_imm:per_dec_octet_string(SzConstr, Aligned),
    case asn1ct:use_legacy_types() of
	false -> {convert,{binary,copy},Imm};
	true -> {convert,binary_to_list,Imm}
    end;
gen_dec_imm_1('TeletexString', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('T61String', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('VideotexString', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('GraphicString', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('GeneralString', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('ObjectDescriptor', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('OBJECT IDENTIFIER', _Constraint, Aligned) ->
    Dec = fun(V, Buf) ->
		  emit(["{",{call,per_common,decode_oid,[V]},com,
			Buf,"}"])
	  end,
    {call,Dec,gen_dec_restricted_string(Aligned)};
gen_dec_imm_1('RELATIVE-OID', _Constraint, Aligned) ->
    Dec = fun(V, Buf) ->
		  emit(["{",{call,per_common,decode_relative_oid,[V]},com,
			Buf,"}"])
	  end,
    {call,Dec,gen_dec_restricted_string(Aligned)};
gen_dec_imm_1('UTF8String', _Constraint, Aligned) ->
    asn1ct_imm:per_dec_restricted_string(Aligned);
gen_dec_imm_1('REAL', _Constraint, Aligned) ->
    asn1ct_imm:per_dec_real(Aligned).

gen_dec_bit_string(F, Imm) ->
    D = fun(V, Buf) ->
		emit(["{",{call,per_common,F,[V]},com,Buf,"}"])
	end,
    {call,D,Imm}.

gen_dec_copy_bitstring(Imm) ->
    D = fun(V, Buf) ->
		emit(["{list_to_bitstring([",V,"]),",Buf,"}"])
	end,
    {call,D,Imm}.

gen_dec_k_m_string(Type, Constraint, Aligned) ->
    asn1ct_imm:per_dec_k_m_string(Type, Constraint, Aligned).

gen_dec_restricted_string(Aligned) ->
    Imm = asn1ct_imm:per_dec_restricted_string(Aligned),
    {convert,binary_to_list,Imm}.

gen_dec_prim(Erule, Type, BytesVar) ->
    Imm = gen_dec_imm(Erule, Type),
    asn1ct_imm:dec_code_gen(Imm, BytesVar).


%% For PER the ExtensionAdditionGroup notation has significance for
%% the encoding and decoding. The components within the
%% ExtensionAdditionGroup is treated in a similar way as if they have
%% been specified within a SEQUENCE. Therefore we construct a fake
%% sequence type here so that we can generate code for it.
extaddgroup2sequence(ExtList) ->
    extaddgroup2sequence(ExtList,0,[]).

extaddgroup2sequence([{'ExtensionAdditionGroup',Number0}|T],ExtNum,Acc) ->
    Number = case Number0 of undefined -> 1; _ -> Number0 end,
    {ExtGroupComps,['ExtensionAdditionGroupEnd'|T2]} =
     lists:splitwith(fun(Elem) -> is_record(Elem,'ComponentType') end,T),
    extaddgroup2sequence(T2,ExtNum+1,
			 [#'ComponentType'{
			     name=list_to_atom("ExtAddGroup"++
						   integer_to_list(ExtNum+1)),
			     typespec=#type{def=#'SEQUENCE'{
					      extaddgroup=Number,
					      components=ExtGroupComps}},
			     prop='OPTIONAL'}|Acc]);
extaddgroup2sequence([C|T],ExtNum,Acc) ->
    extaddgroup2sequence(T,ExtNum,[C|Acc]);
extaddgroup2sequence([],_,Acc) ->
    lists:reverse(Acc).

imm_decode_open_type([#'Externaltypereference'{type=Tname}], Aligned) ->
    imm_dec_open_type_1(Tname, Aligned);
imm_decode_open_type([#type{def=#'Externaltypereference'{type=Tname}}],
		     Aligned) ->
    imm_dec_open_type_1(Tname, Aligned);
imm_decode_open_type(_, Aligned) ->
    asn1ct_imm:per_dec_open_type(Aligned).

imm_dec_open_type_1(Type, Aligned) ->
    D = fun(OpenType, Buf) ->
		asn1ct_name:new(tmpval),
		emit(["begin",nl,
		      "{",{curr,tmpval},",_} = ",
		      {asis,dec_func(Type)},"(",OpenType,"),",nl,
		      "{",{curr,tmpval},com,Buf,"}",nl,
		      "end"])
	end,
    {call,D,asn1ct_imm:per_dec_open_type(Aligned)}.
