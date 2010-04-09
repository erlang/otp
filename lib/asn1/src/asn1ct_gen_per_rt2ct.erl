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
-module(asn1ct_gen_per_rt2ct).

%% Generate erlang module which handles (PER) encode and decode for
%% all types in an ASN.1 module

-include("asn1_records.hrl").
%-compile(export_all).

-export([pgen/4,gen_dec_prim/3,gen_encode_prim/4]).
-export([gen_obj_code/3,gen_objectset_code/2]).
-export([gen_decode/2, gen_decode/3]).
-export([gen_encode/2, gen_encode/3]).

-import(asn1ct_gen, [emit/1,demit/1]).
-import(asn1ct_gen_per, [is_already_generated/2,more_genfields/1,
			 get_class_fields/1,get_object_field/2]).

%% pgen(Erules, Module, TypeOrVal)
%% Generate Erlang module (.erl) and (.hrl) file corresponding to an ASN.1 module
%% .hrl file is only generated if necessary
%% Erules = per | ber
%% Module = atom()
%% TypeOrVal = {TypeList,ValueList}
%% TypeList = ValueList = [atom()]

pgen(OutFile,Erules,Module,TypeOrVal) ->
    asn1ct_gen:pgen_module(OutFile,Erules,Module,TypeOrVal,[],true).


%% Generate ENCODING ******************************
%%****************************************x


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
	    case InnerType of
		'SET' ->
		    true;
		'SEQUENCE' ->
		    true;
		_ ->
		    emit({nl,"'enc_",asn1ct_gen:list2name(Typename),
			  "'({'",asn1ct_gen:list2name(Typename),
			  "',Val}",ObjFun,") ->",nl}),
		    emit({"'enc_",asn1ct_gen:list2name(Typename),
			  "'(Val",ObjFun,");",nl,nl})
	    end,
	    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(Val",ObjFun,
		  ") ->",nl}),
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end.


gen_encode_user(Erules,D) when is_record(D,typedef) ->
    CurrMod = get(currmod),
    Typename = [D#typedef.name],
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    case InnerType of
	'SET' -> true;
	'SEQUENCE' -> true;
	_ ->
	    emit({nl,"'enc_",asn1ct_gen:list2name(Typename),"'({'",asn1ct_gen:list2name(Typename),"',Val}) ->",nl}),
	    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(Val);",nl,nl})
    end,
    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(Val) ->",nl}),
    case asn1ct_gen:type(InnerType) of
	{primitive,bif} ->
	    gen_encode_prim(Erules,Def,"false"),
	    emit({".",nl});
	'ASN1_OPEN_TYPE' ->
	    gen_encode_prim(Erules,Def#type{def='ASN1_OPEN_TYPE'},"false"),
	    emit({".",nl});
	{constructed,bif} ->
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,D);
	#'Externaltypereference'{module=CurrMod,type=Etype} ->
	    emit({"'enc_",Etype,"'(Val).",nl,nl});
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit({"'",Emod,"':'enc_",Etype,"'(Val).",nl,nl});
	#typereference{val=Ename} ->
	    emit({"'enc_",Ename,"'(Val).",nl,nl});
	{notype,_} ->
	    emit({"'enc_",InnerType,"'(Val).",nl,nl})
    end.


gen_encode_prim(Erules,D,DoTag) ->
    Value = case asn1ct_name:active(val) of
		true ->
		    asn1ct_gen:mk_var(asn1ct_name:curr(val));
		false ->
		    "Val"
	    end,
    gen_encode_prim(Erules,D,DoTag,Value).





gen_encode_prim(Erules,D,DoTag,Value) when is_record(D,type) ->
    Constraint = D#type.constraint,
    case D#type.def of
	'INTEGER' ->
	    EffectiveConstr = effective_constraint(integer,Constraint),
	    emit(["  %%INTEGER with effective constraint: ",
		  {asis,EffectiveConstr},nl]),
	    emit_enc_integer(Erules,EffectiveConstr,Value);
	{'INTEGER',NamedNumberList} ->
	    EffectiveConstr = effective_constraint(integer,Constraint),
	    %% maybe an emit_enc_NNL_integer
	    emit(["  %%INTEGER with effective constraint: ",
		  {asis,EffectiveConstr},nl]),
	    emit_enc_integer_NNL(Erules,EffectiveConstr,Value,NamedNumberList);
	{'ENUMERATED',{Nlist1,Nlist2}} ->
	    NewList = lists:append([[{0,X}||{X,_} <- Nlist1],['EXT_MARK'],[{1,X}||{X,_} <- Nlist2]]),
	    NewC = [{'ValueRange',{0,length(Nlist1)-1}}],
	    emit(["case ",Value," of",nl]),
%%	    emit_enc_enumerated_cases(Erules,NewC, NewList++[{asn1_enum,length(Nlist1)-1}], 0);
	    emit_enc_enumerated_cases(Erules,NewC, NewList, 0);
	{'ENUMERATED',NamedNumberList} ->
	    NewList = [X||{X,_} <- NamedNumberList],
	    NewC = effective_constraint(integer,
					[{'ValueRange',
					  {0,length(NewList)-1}}]),
	    NewVal = enc_enum_cases(Value,NewList),
	    emit_enc_integer(Erules,NewC,NewVal);

	'REAL' ->
	    emit({"?RT_PER:encode_real(",Value,")"});

	{'BIT STRING',NamedNumberList} ->
	    EffectiveC = effective_constraint(bitstring,Constraint),
	    case EffectiveC of
		0 -> emit({"[]"});
		_ ->
		    emit({"?RT_PER:encode_bit_string(",
			  {asis,EffectiveC},",",Value,",",
			  {asis,NamedNumberList},")"})
	    end;
	'NULL' ->
	    emit({"?RT_PER:encode_null(",Value,")"});
	'OBJECT IDENTIFIER' ->
	    emit({"?RT_PER:encode_object_identifier(",Value,")"});
	'RELATIVE-OID' ->
	    emit({"?RT_PER:encode_relative_oid(",Value,")"});
	'ObjectDescriptor' ->
	    emit({"?RT_PER:encode_ObjectDescriptor(",{asis,Constraint},
		  ",",Value,")"});
	'BOOLEAN' ->
	    emit({"case ",Value," of",nl,
		  "  true -> [1];",nl,
		  "  false -> [0];",nl,
		  "  _ -> exit({error,{asn1,{encode_boolean,",Value,"}}})",nl,
		  "end"});
	'OCTET STRING' ->
	    emit_enc_octet_string(Erules,Constraint,Value);

	'NumericString' ->
	    emit_enc_known_multiplier_string('NumericString',Constraint,Value);
	TString when TString == 'TeletexString';
		     TString == 'T61String' ->
	    emit({"?RT_PER:encode_TeletexString(",{asis,Constraint},",",Value,")"});
	'VideotexString' ->
	    emit({"?RT_PER:encode_VideotexString(",{asis,Constraint},",",Value,")"});
	'UTCTime' ->
	    emit_enc_known_multiplier_string('VisibleString',Constraint,Value);
	'GeneralizedTime' ->
	    emit_enc_known_multiplier_string('VisibleString',Constraint,Value);
	'GraphicString' ->
	    emit({"?RT_PER:encode_GraphicString(",{asis,Constraint},",",Value,")"});
	'VisibleString' ->
	    emit_enc_known_multiplier_string('VisibleString',Constraint,Value);
	'GeneralString' ->
	    emit({"?RT_PER:encode_GeneralString(",{asis,Constraint},",",Value,")"});
	'PrintableString' ->
	    emit_enc_known_multiplier_string('PrintableString',Constraint,Value);
	'IA5String' ->
	    emit_enc_known_multiplier_string('IA5String',Constraint,Value);
	'BMPString' ->
	    emit_enc_known_multiplier_string('BMPString',Constraint,Value);
	'UniversalString' ->
	    emit_enc_known_multiplier_string('UniversalString',Constraint,Value);
	'UTF8String' ->
	    emit({"?RT_PER:encode_UTF8String(",Value,")"});
	'ANY' ->
	    emit(["?RT_PER:encode_open_type(", {asis,Constraint}, ",", 
		  Value, ")"]);
	'ASN1_OPEN_TYPE' ->
	    NewValue = case Constraint of
			   [#'Externaltypereference'{type=Tname}] ->
			     io_lib:format(
			       "?RT_PER:complete(enc_~s(~s))",[Tname,Value]);
			   [#type{def=#'Externaltypereference'{type=Tname}}] ->
			       io_lib:format(
				 "?RT_PER:complete(enc_~s(~s))",[Tname,Value]);
			 _ -> Value
		     end,
	    emit(["?RT_PER:encode_open_type(", {asis,Constraint}, ",", 
		  NewValue, ")"]);
	#'ObjectClassFieldType'{} ->
	    case asn1ct_gen:get_inner(D#type.def) of
		{fixedtypevaluefield,_,InnerType} -> 
		    gen_encode_prim(Erules,InnerType,DoTag,Value);
		T -> %% 'ASN1_OPEN_TYPE'
		    gen_encode_prim(Erules,D#type{def=T},DoTag,Value)
	    end;
	XX ->
	    exit({asn1_error,nyi,XX})
    end.

emit_enc_known_multiplier_string(StringType,C,Value) ->
    SizeC = 
	case get_constraint(C,'SizeConstraint') of
	    L when is_list(L) -> {lists:min(L),lists:max(L)};
	    L -> L
	end,
    PAlphabC = get_constraint(C,'PermittedAlphabet'),
    case {StringType,PAlphabC} of
	{'UniversalString',{_,_}} ->
	    exit({error,{asn1,{'not implemented',"UniversalString with "
			       "PermittedAlphabet constraint"}}});
	{'BMPString',{_,_}} ->
	    exit({error,{asn1,{'not implemented',"BMPString with "
			       "PermittedAlphabet constraint"}}});
	_ -> ok
    end,
    NumBits = get_NumBits(C,StringType),
    CharOutTab = get_CharOutTab(C,StringType),
    %% NunBits and CharOutTab for chars_encode
    emit_enc_k_m_string(StringType,SizeC,NumBits,CharOutTab,Value).

emit_enc_k_m_string(_StringType,0,_NumBits,_CharOutTab,_Value) ->
    emit({"[]"});
emit_enc_k_m_string(StringType,SizeC,NumBits,CharOutTab,Value) ->
    emit({"?RT_PER:encode_known_multiplier_string(",{asis,StringType},",",
	  {asis,SizeC},",",NumBits,",",{asis,CharOutTab},",",Value,")"}).

emit_dec_known_multiplier_string(StringType,C,BytesVar) ->
    SizeC = get_constraint(C,'SizeConstraint'),
    PAlphabC = get_constraint(C,'PermittedAlphabet'),
    case {StringType,PAlphabC} of
	{'BMPString',{_,_}} ->
	    exit({error,{asn1,
			 {'not implemented',
			  "BMPString with PermittedAlphabet "
			  "constraint"}}});
	_ ->
	    ok
    end,
    NumBits = get_NumBits(C,StringType),
    CharInTab = get_CharInTab(C,StringType),
    case SizeC of
	0 ->
	    emit({"{[],",BytesVar,"}"});
	_ ->
	    emit({"?RT_PER:decode_known_multiplier_string(",
		  {asis,StringType},",",{asis,SizeC},",",NumBits,
		  ",",{asis,CharInTab},",",BytesVar,")"})
    end.


%% copied from run time module

get_CharOutTab(C,StringType) ->
    get_CharTab(C,StringType,out).

get_CharInTab(C,StringType) ->
    get_CharTab(C,StringType,in).

get_CharTab(C,StringType,InOut) ->
    case get_constraint(C,'PermittedAlphabet') of
	{'SingleValue',Sv} ->
	    get_CharTab2(C,StringType,hd(Sv),lists:max(Sv),Sv,InOut);
	no ->
	    case StringType of
		'IA5String' ->
		    {0,16#7F,notab};
		'VisibleString' ->
		    get_CharTab2(C,StringType,16#20,16#7F,notab,InOut);
		'PrintableString' ->
		    Chars = lists:sort(
			      " '()+,-./0123456789:=?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"),
		    get_CharTab2(C,StringType,hd(Chars),lists:max(Chars),Chars,InOut);
		'NumericString' ->
		    get_CharTab2(C,StringType,16#20,$9," 0123456789",InOut);
		'UniversalString' ->
		    {0,16#FFFFFFFF,notab};
		'BMPString' ->
		    {0,16#FFFF,notab}
	    end
    end.

get_CharTab2(C,StringType,Min,Max,Chars,InOut) ->
    BitValMax = (1 bsl get_NumBits(C,StringType))-1,
    if
	Max =< BitValMax ->
	    {0,Max,notab};
	true ->
	    case InOut of
		out ->
		    {Min,Max,create_char_tab(Min,Chars)};
		in  ->
		    {Min,Max,list_to_tuple(Chars)}
	    end
    end.

create_char_tab(Min,L) ->
    list_to_tuple(create_char_tab(Min,L,0)).
create_char_tab(Min,[Min|T],V) ->
    [V|create_char_tab(Min+1,T,V+1)];
create_char_tab(_Min,[],_V) ->
    [];
create_char_tab(Min,L,V) ->
    [false|create_char_tab(Min+1,L,V)].

get_NumBits(C,StringType) ->
    case get_constraint(C,'PermittedAlphabet') of
	{'SingleValue',Sv} ->
	    charbits(length(Sv),aligned);
	no ->
	    case StringType of
		'IA5String' ->
		    charbits(128,aligned); % 16#00..16#7F
		'VisibleString' ->
		    charbits(95,aligned); % 16#20..16#7E
		'PrintableString' ->
		    charbits(74,aligned); % [$\s,$',$(,$),$+,$,,$-,$.,$/,"0123456789",$:,$=,$?,$A..$Z,$a..$z
		'NumericString' ->
		    charbits(11,aligned); % $ ,"0123456789"
		'UniversalString' ->
		    32;
		'BMPString' ->
		    16
	    end
    end.

charbits(NumOfChars,aligned) ->
    case charbits(NumOfChars) of
	1 -> 1;
	2 -> 2;
	B when B =< 4 -> 4;
	B when B =< 8 -> 8;
	B when B =< 16 -> 16; 
	B when B =< 32 -> 32
    end.

charbits(NumOfChars) when NumOfChars =< 2 -> 1;
charbits(NumOfChars) when NumOfChars =< 4 -> 2;
charbits(NumOfChars) when NumOfChars =< 8 -> 3;
charbits(NumOfChars) when NumOfChars =< 16 -> 4;
charbits(NumOfChars) when NumOfChars =< 32 -> 5;
charbits(NumOfChars) when NumOfChars =< 64 -> 6;
charbits(NumOfChars) when NumOfChars =< 128 -> 7;
charbits(NumOfChars) when NumOfChars =< 256 -> 8;
charbits(NumOfChars) when NumOfChars =< 512 -> 9;
charbits(NumOfChars) when NumOfChars =< 1024 -> 10;
charbits(NumOfChars) when NumOfChars =< 2048 -> 11;
charbits(NumOfChars) when NumOfChars =< 4096 -> 12;
charbits(NumOfChars) when NumOfChars =< 8192 -> 13;
charbits(NumOfChars) when NumOfChars =< 16384 -> 14;
charbits(NumOfChars) when NumOfChars =< 32768 -> 15;
charbits(NumOfChars) when NumOfChars =< 65536 -> 16;
charbits(NumOfChars) when is_integer(NumOfChars) ->
    16 + charbits1(NumOfChars bsr 16).

charbits1(0) ->
    0;
charbits1(NumOfChars) ->
    1 + charbits1(NumOfChars bsr 1).

%% copied from run time module

emit_enc_octet_string(_Erules,Constraint,Value) ->
    case get_constraint(Constraint,'SizeConstraint') of
	0 ->
	    emit({"  []"});
	1 ->
	    asn1ct_name:new(tmpval),
	    emit({"  begin",nl}),
	    emit({"    [",{curr,tmpval},"] = ",Value,",",nl}),
	    emit({"    [10,8,",{curr,tmpval},"]",nl}),
	    emit("  end");
	2 ->
	    asn1ct_name:new(tmpval),
	    emit({"  begin",nl}),
	    emit({"    [",{curr,tmpval},",",{next,tmpval},"] = ",
		  Value,",",nl}),
	    emit({"    [[10,8,",{curr,tmpval},"],[10,8,",
		  {next,tmpval},"]]",nl}),
	    emit("  end"),
	    asn1ct_name:new(tmpval);
	Sv when is_integer(Sv),Sv =< 256  ->
	    asn1ct_name:new(tmpval),
	    emit({"  begin",nl}),
	    emit({"    case length(",Value,") of",nl}),
	    emit(["      ",{curr,tmpval}," when ",{curr,tmpval}," == ",Sv," ->"]),
	    emit([" [2,20,",{curr,tmpval},",",Value,"];",nl]),
	    emit({"      _ -> exit({error,{value_out_of_bounds,",
		  Value,"}})", nl,"    end",nl}),
	    emit("  end");
	Sv when is_integer(Sv),Sv =< 65535  ->
	    asn1ct_name:new(tmpval),
	    emit({"  begin",nl}),
	    emit({"    case length(",Value,") of",nl}),
	    emit(["      ",{curr,tmpval}," when ",{curr,tmpval}," == ",Sv," ->"]),
	    emit([" [2,21,",{curr,tmpval},",",Value,"];",nl]),
	    emit({"      _ -> exit({error,{value_out_of_bounds,",
		  Value,"}})",nl,"    end",nl}),
	    emit("  end");
	C ->
	    emit({"  ?RT_PER:encode_octet_string(",{asis,C},",false,",Value,")",nl})
    end.

emit_dec_octet_string(Constraint,BytesVar) ->
    case get_constraint(Constraint,'SizeConstraint') of
	0 ->
	    emit({"  {[],",BytesVar,"}",nl});
	{_,0} ->
	    emit({"  {[],",BytesVar,"}",nl});
	C ->
	    emit({"  ?RT_PER:decode_octet_string(",BytesVar,",",
		  {asis,C},",false)",nl})
    end.

emit_enc_integer_case(Value) ->
    case get(component_type) of
	{true,#'ComponentType'{prop=Prop}} ->
	    emit({"  begin",nl}),
	    case Prop of
		Opt when Opt=='OPTIONAL';
			 is_tuple(Opt),element(1,Opt)=='DEFAULT' ->
		    emit({"  case ",Value," of",nl}),
		    ok;
		_ ->
		    emit({"  ",{curr,tmpval},"=",Value,",",nl}),
		    emit({"  case ",{curr,tmpval}," of",nl}),
		    asn1ct_name:new(tmpval)
	    end;
%	    asn1ct_name:new(tmpval);
	_ ->
	    emit({" case ",Value," of ",nl})
    end.
emit_enc_integer_end_case() ->    
    case get(component_type) of
	{true,_} ->
	    emit({nl,"  end"}); % end of begin ... end
	_ -> ok
    end.


emit_enc_integer_NNL(Erules,C,Value,NNL) ->
    EncVal = enc_integer_NNL_cases(Value,NNL),
    emit_enc_integer(Erules,C,EncVal).
    
enc_integer_NNL_cases(Value,NNL) ->
    asn1ct_name:new(tmpval),
    TmpVal = asn1ct_gen:mk_var(asn1ct_name:curr(tmpval)),
    Cases=enc_integer_NNL_cases1(NNL),
    lists:flatten(io_lib:format("(case ~s of "++Cases++
		  "~s when is_atom(~s)->exit({error,{asn1,{namednumber,~s}}});_->~s end)",[Value,TmpVal,TmpVal,TmpVal,Value])).

enc_integer_NNL_cases1([{NNo,No}|Rest]) ->
    io_lib:format("~w->~w;",[NNo,No])++enc_integer_NNL_cases1(Rest);
enc_integer_NNL_cases1([]) ->
    "".

emit_enc_integer(_Erule,[{'SingleValue',Int}],Value) ->
    asn1ct_name:new(tmpval),
    emit_enc_integer_case(Value),%    emit(["  case ",Value," of",nl]),
    emit(["    ",Int," -> [];",nl]),
    emit(["    ",{curr,tmpval}," ->",nl]),
    emit(["      exit({error,{value_out_of_bounds,",{curr,tmpval},"}})",
	  nl," end",nl]),
    emit_enc_integer_end_case();

emit_enc_integer(_Erule,[{_,{Lb,Ub},_Range,{bits,NoBs}}],Value) -> % Range =< 255
    asn1ct_name:new(tmpval),
    emit_enc_integer_case(Value),
    emit(["    ",{curr,tmpval}," when ",{curr,tmpval},"=<",Ub,",",
	  {curr,tmpval},">=",Lb," ->",nl]),
    emit(["      [10,",NoBs,",",{curr,tmpval},"- ",Lb,"];",nl]),
    emit(["    ",{curr,tmpval}," ->",nl]),
    emit(["      exit({error,{value_out_of_bounds,",
	  {curr,tmpval},"}})",nl,"  end",nl]),
    emit_enc_integer_end_case();

emit_enc_integer(_Erule,[{_,{Lb,Ub},Range,_}],Value) when Range =< 256 ->
    asn1ct_name:new(tmpval),
    emit_enc_integer_case(Value),
    emit(["    ",{curr,tmpval}," when ",{curr,tmpval},"=<",Ub,",",
	  {curr,tmpval},">=",Lb," ->",nl]),
    emit(["      [20,1,",{curr,tmpval},"- ",Lb,"];",nl]),
    emit(["    ",{curr,tmpval}," ->",nl]),
    emit(["      exit({error,{value_out_of_bounds,",{curr,tmpval},"}})",
	  nl,"  end",nl]),
    emit_enc_integer_end_case();

emit_enc_integer(_Erule,[{_,{Lb,Ub},Range,_}],Value) when Range =< 65536 ->
    asn1ct_name:new(tmpval),
    emit_enc_integer_case(Value),
    emit(["    ",{curr,tmpval}," when ",{curr,tmpval},"=<",Ub,",",
	  {curr,tmpval},">=",Lb," ->",nl]),
    emit(["      [20,2,<<(",{curr,tmpval},"- ",Lb,"):16>>];",nl]),
    emit(["    ",{curr,tmpval}," ->",nl]),
    emit(["      exit({error,{value_out_of_bounds,",{curr,tmpval},"}})",
	  nl,"  end",nl]),
    emit_enc_integer_end_case();


emit_enc_integer(_Erule,C,Value) ->
    emit({"  ?RT_PER:encode_integer(",{asis,C},",",Value,")"}).




enc_enum_cases(Value,NewList) ->
    asn1ct_name:new(tmpval),
    TmpVal = asn1ct_gen:mk_var(asn1ct_name:curr(tmpval)),
    Cases=enc_enum_cases1(NewList),
    lists:flatten(io_lib:format("(case ~s of "++Cases++
				"~s ->exit({error,"
				"{asn1,{enumerated,~s}}})"
				" end)",
				[Value,TmpVal,TmpVal])).
enc_enum_cases1(NNL) ->
    enc_enum_cases1(NNL,0).
enc_enum_cases1([H|T],Index) ->
    io_lib:format("~w->~w;",[H,Index])++enc_enum_cases1(T,Index+1);
enc_enum_cases1([],_) ->
    "".


emit_enc_enumerated_cases(Erule, C, [H], Count) ->
    emit_enc_enumerated_case(Erule, C, H, Count),
    case H of
	'EXT_MARK' ->
	    ok;
	_ ->
	    emit([";",nl])
    end,
    emit([nl,"EnumVal -> exit({error,{asn1, {enumerated_not_in_range, EnumVal}}})"]),
    emit([nl,"end"]);
emit_enc_enumerated_cases(Erule, C, ['EXT_MARK'|T], _Count) ->
    emit_enc_enumerated_cases(Erule, C, T, 0);
emit_enc_enumerated_cases(Erule, C, [H1,H2|T], Count) ->
    emit_enc_enumerated_case(Erule, C, H1, Count),
    emit([";",nl]),
    emit_enc_enumerated_cases(Erule, C, [H2|T], Count+1).


%% The function clauses matching on tuples with first element 
%% asn1_enum, 1 or 0 and the atom 'EXT_MARK' are for ENUMERATED
%% with extension mark.
%% emit_enc_enumerated_case(_Erule,_C, {asn1_enum,High}, _) -> 
%%     %% ENUMERATED with extensionmark
%%     %% value higher than the extension base and not 
%%     %% present in the extension range.
%%     emit(["{asn1_enum,EnumV} when is_integer(EnumV), EnumV > ",High," -> ",
%% 	  "[1,?RT_PER:encode_small_number(EnumV)]"]);
emit_enc_enumerated_case(_Erule,_C, {1,EnumName}, Count) ->
    %% ENUMERATED with extensionmark
    %% values higher than extension root
    emit(["'",EnumName,"' -> [1,?RT_PER:encode_small_number(",Count,")]"]);
emit_enc_enumerated_case(_Erule,C, {0,EnumName}, Count) ->
    %% ENUMERATED with extensionmark
    %% values within extension root
%%    emit(["'",EnumName,"' -> [0,?RT_PER:encode_integer(",{asis,C},", ",Count,")]"]);
    emit(["'",EnumName,"' -> ",{asis,[0|asn1rt_per_bin_rt2ct:encode_integer(C,Count)]}]);
emit_enc_enumerated_case(_Erule, _C, 'EXT_MARK', _Count) ->
    true.
%% %% This clause is invoked in case of an ENUMERATED without extension mark
%% emit_enc_enumerated_case(_Erule,_C, EnumName, Count) ->
%%     emit(["'",EnumName,"' -> ",Count]).


get_constraint([{Key,V}],Key) ->
    V;
get_constraint([],_) ->
    no;
get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	    no;
	{value,{_,V}} -> 
	    V
    end.

get_constraints(L=[{Key,_}],Key) ->
    L;
get_constraints([],_) ->
    [];
get_constraints(C,Key) ->
    {value,L} = keysearch_allwithkey(Key,1,C,[]),
    L.

keysearch_allwithkey(Key,Ix,C,Acc) ->
    case lists:keysearch(Key,Ix,C) of
	false ->
	    {value,Acc};
	{value,T} ->
	    RestC = lists:delete(T,C),
	    keysearch_allwithkey(Key,Ix,RestC,[T|Acc])
    end.

%% effective_constraint(Type,C)
%% Type = atom()
%% C = [C1,...]
%% C1 = {'SingleValue',SV} | {'ValueRange',VR} | {atom(),term()}
%% SV = integer() | [integer(),...]
%% VR = {Lb,Ub}
%% Lb = 'MIN' | integer()
%% Ub = 'MAX' | integer()
%% Returns a single value if C only has a single value constraint, and no
%% value range constraints, that constrains to a single value, otherwise 
%% returns a value range that has the lower bound set to the lowest value 
%% of all single values and lower bound values in C and the upper bound to
%% the greatest value.
effective_constraint(integer,[C={{_,_},_}|_Rest]) -> % extension
    [C]; %% [C|effective_constraint(integer,Rest)]; XXX what is possible ???
effective_constraint(integer,C) ->
    SVs = get_constraints(C,'SingleValue'),
    SV = effective_constr('SingleValue',SVs),
    VRs = get_constraints(C,'ValueRange'),
    VR = effective_constr('ValueRange',VRs),
    CRange = greatest_common_range(SV,VR),
    pre_encode(integer,CRange);
effective_constraint(bitstring,C) ->
    get_constraint(C,'SizeConstraint').

effective_constr(_,[]) ->
    [];
effective_constr('SingleValue',List) ->
    SVList = lists:flatten(lists:map(fun(X)->element(2,X)end,List)),
    % sort and remove duplicates
    RemoveDup = fun([],_) ->[];
		   ([H],_) -> [H];
		   ([H,H|T],F) -> F([H|T],F);
		   ([H|T],F) -> [H|F(T,F)]
		end,
    
    case RemoveDup(SVList,RemoveDup) of
	[N] ->
	    [{'SingleValue',N}];
	L when is_list(L) -> 
	    [{'ValueRange',{hd(L),lists:last(L)}}]
    end;
effective_constr('ValueRange',List) ->
    LBs = lists:map(fun({_,{Lb,_}})-> Lb end,List),
    UBs = lists:map(fun({_,{_,Ub}})-> Ub end,List),
    Lb = least_Lb(LBs),
    [{'ValueRange',{Lb,lists:max(UBs)}}].

greatest_common_range([],VR) ->
    VR;
greatest_common_range(SV,[]) ->
    SV;
greatest_common_range([{_,Int}],[{_,{'MIN',Ub}}]) when is_integer(Int),
						       Int > Ub ->
    [{'ValueRange',{'MIN',Int}}];
greatest_common_range([{_,Int}],[{_,{Lb,Ub}}]) when is_integer(Int),
						    Int < Lb ->
    [{'ValueRange',{Int,Ub}}];
greatest_common_range([{_,Int}],VR=[{_,{_Lb,_Ub}}]) when is_integer(Int) ->
    VR;
greatest_common_range([{_,L}],[{_,{Lb,Ub}}]) when is_list(L) ->
    Min = least_Lb([Lb|L]),
    Max = greatest_Ub([Ub|L]),
    [{'ValueRange',{Min,Max}}].
    

least_Lb(L) ->
    case lists:member('MIN',L) of
	true -> 'MIN';
	_ -> lists:min(L)
    end.

greatest_Ub(L) ->
    case lists:member('MAX',L) of
	true -> 'MAX';
	_ -> lists:max(L)
    end.




pre_encode(integer,[]) ->
    [];
pre_encode(integer,C=[{'SingleValue',_}]) ->
    C;
pre_encode(integer,C=[{'ValueRange',VR={Lb,Ub}}]) when is_integer(Lb),is_integer(Ub)->
    Range = Ub-Lb+1,
    if
	Range =< 255 ->
	    NoBits = no_bits(Range),
	    [{'ValueRange',VR,Range,{bits,NoBits}}];
	Range =< 256 ->
	    [{'ValueRange',VR,Range,{octets,1}}];
	Range =< 65536 ->
	    [{'ValueRange',VR,Range,{octets,2}}];
	true ->
	    C
    end;
pre_encode(integer,C) ->
    C.

no_bits(2) -> 1;
no_bits(N) when N=<4 -> 2;
no_bits(N) when N=<8 -> 3;
no_bits(N) when N=<16 -> 4;
no_bits(N) when N=<32 -> 5;
no_bits(N) when N=<64 -> 6;
no_bits(N) when N=<128 -> 7;
no_bits(N) when N=<255 -> 8.

%% Object code generating for encoding and decoding
%% ------------------------------------------------

gen_obj_code(Erules,_Module,Obj) when is_record(Obj,typedef) ->
    ObjName = Obj#typedef.name,
    Def = Obj#typedef.typespec,
    #'Externaltypereference'{module=Mod,type=ClassName} = 
	Def#'Object'.classname,
    Class = asn1_db:dbget(Mod,ClassName),
    {object,_,Fields} = Def#'Object'.def,
    emit({nl,nl,nl,"%%================================"}),
    emit({nl,"%%  ",ObjName}),
    emit({nl,"%%================================",nl}),
    EncConstructed =
	gen_encode_objectfields(Erules,ClassName,get_class_fields(Class),
				ObjName,Fields,[]),
    emit(nl),
    gen_encode_constr_type(Erules,EncConstructed),
    emit(nl),
    DecConstructed =
	gen_decode_objectfields(ClassName,get_class_fields(Class),
				ObjName,Fields,[]),
    emit(nl),
    gen_decode_constr_type(Erules,DecConstructed),
    emit(nl);
gen_obj_code(_Erules,_Module,Obj) when is_record(Obj,pobjectdef) ->
    ok.

gen_encode_objectfields(Erules,ClassName,[{typefield,Name,OptOrMand}|Rest],
			ObjName,ObjectFields,ConstrAcc) ->
    EmitFuncClause = 
	fun(V) ->
		emit(["'enc_",ObjName,"'(",{asis,Name},
		      ",",V,",_RestPrimFieldName) ->",nl])
	end,

    MaybeConstr =
	case {get_object_field(Name,ObjectFields),OptOrMand} of
	    {false,'MANDATORY'} -> %% this case is illegal
		exit({error,{asn1,{"missing mandatory field in object",
				   ObjName}}});
	    {false,'OPTIONAL'} ->
		EmitFuncClause("Val"),
		emit(["   if",nl,
		      "     is_list(Val) ->",nl,
		      "       NewVal = list_to_binary(Val),",nl,
		      "       [20,size(NewVal),NewVal];",nl,
		      "     is_binary(Val) ->",nl,
		      "       [20,size(Val),Val]",nl,
		      "   end"]),
		[];
	    {false,{'DEFAULT',DefaultType}} ->
		EmitFuncClause("Val"),
		gen_encode_default_call(Erules,ClassName,Name,DefaultType);
	    {{Name,TypeSpec},_} ->
		%% A specified field owerwrites any 'DEFAULT' or
		%% 'OPTIONAL' field in the class
		EmitFuncClause("Val"),
		gen_encode_field_call(Erules,ObjName,Name,TypeSpec)
	end,
    case more_genfields(Rest) of
	true ->
	    emit([";",nl]);
	false ->
	    emit([".",nl])
    end,
    gen_encode_objectfields(Erules,ClassName,Rest,ObjName,ObjectFields,
			    MaybeConstr++ConstrAcc);
gen_encode_objectfields(Erules,ClassName,[{objectfield,Name,_,_,OptOrMand}|Rest],
			ObjName,ObjectFields,ConstrAcc) ->
    CurrentMod = get(currmod),
    EmitFuncClause =
	fun(Attrs) ->
		emit(["'enc_",ObjName,"'(",{asis,Name},
		      ",",Attrs,") ->",nl])
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
	    EmitFuncClause("Val,[H|T]"),
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
    gen_encode_objectfields(Erules,ClassName,Rest,ObjName,ObjectFields,ConstrAcc);
gen_encode_objectfields(Erules,ClassName,[_C|Cs],O,OF,Acc) ->
    gen_encode_objectfields(Erules,ClassName,Cs,O,OF,Acc);
gen_encode_objectfields(_Erules,_,[],_,_,Acc) ->
    Acc.



gen_encode_constr_type(Erules,[TypeDef|Rest]) when is_record(TypeDef,typedef) ->
    case is_already_generated(enc,TypeDef#typedef.name) of
	true -> ok;
	_ ->
	    Name = lists:concat(["enc_",TypeDef#typedef.name]),
	    emit({Name,"(Val) ->",nl}),
	    Def = TypeDef#typedef.typespec,
	    InnerType = asn1ct_gen:get_inner(Def#type.def),
	    asn1ct_gen:gen_encode_constructed(Erules,Name,InnerType,Def),
	    gen_encode_constr_type(Erules,Rest)
    end;
gen_encode_constr_type(_,[]) ->
    ok.

gen_encode_field_call(_Erule,_ObjName,_FieldName,
		      #'Externaltypereference'{module=M,type=T}) ->
    CurrentMod = get(currmod),
    if
	M == CurrentMod ->
	    emit({"   'enc_",T,"'(Val)"}),
	    [];
	true ->
	    emit({"   '",M,"':'enc_",T,"'(Val)"}),
	    []
    end;
gen_encode_field_call(Erule,ObjName,FieldName,Type) ->
    Def = Type#typedef.typespec,
    case Type#typedef.name of
	{primitive,bif} -> 
	    gen_encode_prim(Erule,Def,"false",
			    "Val"),
	    [];
	{constructed,bif} ->
	    emit({"   'enc_",ObjName,'_',FieldName,
		  "'(Val)"}),
	    [Type#typedef{name=list_to_atom(lists:concat([ObjName,'_',FieldName]))}];
	{ExtMod,TypeName} ->
	    emit({"   '",ExtMod,"':'enc_",TypeName,
		  "'(Val)"}),
	    [];
	TypeName ->
	    emit({"   'enc_",TypeName,"'(Val)"}),
	    []
    end.

gen_encode_default_call(Erules,ClassName,FieldName,Type) ->
    CurrentMod = get(currmod),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
    	{constructed,bif} ->
%%	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,Type);
	    emit(["   'enc_",ClassName,'_',FieldName,"'(Val)"]),
	    [#typedef{name=list_to_atom(lists:concat([ClassName,'_',FieldName])),
		      typespec=Type}];
	{primitive,bif} ->
	    gen_encode_prim(Erules,Type,"false","Val"),
	    [];
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    emit(["   'enc_",Etype,"'(Val)",nl]),
	    [];
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'enc_",Etype,"'(Val)",nl]),
	    []
    end.
    


gen_decode_objectfields(ClassName,[{typefield,Name,OptOrMand}|Rest],
			ObjName,ObjectFields,ConstrAcc) ->
    EmitFuncClause = 
	fun(Bytes) ->
		emit(["'dec_",ObjName,"'(",{asis,Name},",",Bytes,
		      ",_,_RestPrimFieldName) ->",nl])
	end,
%     emit(["'dec_",ObjName,"'(",{asis,Name},
% 	  ", Bytes, _, RestPrimFieldName) ->",nl]),
    MaybeConstr=
	case {get_object_field(Name,ObjectFields),OptOrMand} of
	    {false,'MANDATORY'} -> %% this case is illegal
		exit({error,{asn1,{"missing mandatory field in object",
				   ObjName}}});
	    {false,'OPTIONAL'} ->
		EmitFuncClause("Bytes"),
		emit(["   {Bytes,[]}"]),
		[];
	    {false,{'DEFAULT',DefaultType}} ->
		EmitFuncClause("Bytes"),
		gen_decode_default_call(ClassName,Name,"Bytes",DefaultType);
	    {{Name,TypeSpec},_} ->
		%% A specified field owerwrites any 'DEFAULT' or
		%% 'OPTIONAL' field in the class
		EmitFuncClause("Bytes"),
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
    CurrentMod = get(currmod),
     EmitFuncClause =
	fun(Attrs) ->
		emit(["'dec_",ObjName,"'(",{asis,Name},
		      ",",Attrs,") ->",nl])
	end,
%     emit(["'dec_",ObjName,"'(",{asis,Name},
% 	  ", Bytes,_,[H|T]) ->",nl]),
    case {get_object_field(Name,ObjectFields),OptOrMand} of
	{false,'MANDATORY'} ->
	    exit({error,{asn1,{"missing mandatory field in object",
			       ObjName}}});
	{false,'OPTIONAL'} ->
	    EmitFuncClause("_,_,_"),
	    emit(["  exit({error,{'illegal use of missing field in object', ",{asis,Name},
		  "}})"]);
	{false,{'DEFAULT',_DefaultObject}} ->
	    exit({error,{asn1,{"not implemented yet",Name}}});
	{{Name,#'Externalvaluereference'{module=CurrentMod,
					 value=TypeName}},_} ->
	    EmitFuncClause("Bytes,_,[H|T]"),
	    emit({indent(3),"'dec_",TypeName,"'(H, Bytes, telltype, T)"});
	{{Name,#'Externalvaluereference'{module=M,value=TypeName}},_} ->
	    EmitFuncClause("Bytes,_,[H|T]"),
	    emit({indent(3),"'",M,"':'dec_",TypeName,
		  "'(H, Bytes, telltype, T)"});
	{{Name,TypeSpec},_} ->
	    EmitFuncClause("Bytes,_,[H|T]"),
	    case TypeSpec#typedef.name of
		{ExtMod,TypeName} ->
		    emit({indent(3),"'",ExtMod,"':'dec_",TypeName,
			  "'(H, Bytes, telltype, T)"});
		TypeName ->
		    emit({indent(3),"'dec_",TypeName,"'(H, Bytes, telltype, T)"})
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


gen_decode_field_call(_ObjName,_FieldName,Bytes,
		      #'Externaltypereference'{module=M,type=T}) ->
    CurrentMod = get(currmod),
    if
	M == CurrentMod ->
	    emit(["   'dec_",T,"'(",Bytes,", telltype)"]),
	    [];
	true ->
	    emit(["   '",M,"':'dec_",T,"'(",Bytes,", telltype)"]),
	    []
    end;
gen_decode_field_call(ObjName,FieldName,Bytes,Type) ->
    Def = Type#typedef.typespec,
    case Type#typedef.name of
	{primitive,bif} -> 
	    gen_dec_prim(per,Def,Bytes),
	    [];
	{constructed,bif} ->
	    emit({"   'dec_",ObjName,'_',FieldName,
		  "'(",Bytes,",telltype)"}),
	    [Type#typedef{name=list_to_atom(lists:concat([ObjName,'_',FieldName]))}];
	{ExtMod,TypeName} ->
	    emit({"   '",ExtMod,"':'dec_",TypeName,
		  "'(",Bytes,", telltype)"}),
	    [];
	TypeName ->
	    emit({"   'dec_",TypeName,"'(",Bytes,", telltype)"}),
	    []
    end.

gen_decode_default_call(ClassName,FieldName,Bytes,Type) ->
    CurrentMod = get(currmod),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
    	{constructed,bif} ->
	    emit(["   'dec_",ClassName,'_',FieldName,"'(",Bytes,", telltype)"]),
	    [#typedef{name=list_to_atom(lists:concat([ClassName,'_',FieldName])),
		      typespec=Type}];
	{primitive,bif} ->
	    gen_dec_prim(per,Type,Bytes),
	    [];
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    emit(["   'dec_",Etype,"'(",Bytes,", telltype)",nl]),
	    [];
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'dec_",Etype,"'(",Bytes,", telltype)",nl]),
	    []
    end.

%%%%%%%%%%%%%%%


gen_decode_constr_type(Erules,[TypeDef|Rest]) when is_record(TypeDef,typedef) ->
    case is_already_generated(dec,TypeDef#typedef.name) of
	true -> ok;
	_ ->
	    gen_decode(Erules,TypeDef)
    end,
    gen_decode_constr_type(Erules,Rest);
gen_decode_constr_type(_,[]) ->
    ok.

%% Object Set code generating for encoding and decoding
%% ----------------------------------------------------
gen_objectset_code(Erules,ObjSet) ->
    ObjSetName = ObjSet#typedef.name,
    Def = ObjSet#typedef.typespec,
%%    {ClassName,ClassDef} = Def#'ObjectSet'.class,
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

gen_objset_code(Erule,ObjSetName,UniqueFName,Set,ClassName,ClassDef)->
    ClassFields = (ClassDef#classdef.typespec)#objectclass.fields,
    InternalFuncs=
	gen_objset_enc(Erule,ObjSetName,UniqueFName,Set,ClassName,
		       ClassFields,1,[]),
    gen_objset_dec(ObjSetName,UniqueFName,Set,ClassName,ClassFields,1),
    gen_internal_funcs(Erule,InternalFuncs).

gen_objset_enc(_Erule,_,{unique,undefined},_,_,_,_,_) ->
    %% There is no unique field in the class of this object set
    %% don't bother about the constraint
    [];
gen_objset_enc(Erule,ObjSName,UniqueName,[{ObjName,Val,Fields},T|Rest],
	       ClName,ClFields,NthObj,Acc)->
    emit({"'getenc_",ObjSName,"'(",{asis,UniqueName},",",
	  {asis,Val},") ->",nl}),
    CurrMod = get(currmod),
    {InternalFunc,NewNthObj}=
	case ObjName of
	    {no_mod,no_name} ->
		gen_inlined_enc_funs(Erule,Fields,ClFields,ObjSName,NthObj);
	    {CurrMod,Name} ->
		emit({"    fun 'enc_",Name,"'/3"}),
		{[],NthObj};
	    {ModName,Name} ->
		emit_ext_encfun(ModName,Name),
%		emit(["    {'",ModName,"', 'enc_",Name,"'}"]),
		{[],NthObj};
	    _ ->
		emit({"    fun 'enc_",ObjName,"'/3"}),
		{[],NthObj}
	end,
    emit({";",nl}),
    gen_objset_enc(Erule,ObjSName,UniqueName,[T|Rest],ClName,ClFields,
		  NewNthObj,InternalFunc++Acc);
gen_objset_enc(Erule,ObjSetName,UniqueName,
	       [{ObjName,Val,Fields}],_ClName,ClFields,NthObj,Acc) ->

    emit({"'getenc_",ObjSetName,"'(",{asis,UniqueName},",",
	  {asis,Val},") ->",nl}),
    CurrMod = get(currmod),
    {InternalFunc,_}=
	case ObjName of
	    {no_mod,no_name} ->
		gen_inlined_enc_funs(Erule,Fields,ClFields,ObjSetName,NthObj);
	    {CurrMod,Name} ->
		emit({"    fun 'enc_",Name,"'/3"}),
		{[],NthObj};
	    {ModName,Name} ->
		emit_ext_encfun(ModName,Name),
%		emit(["    {'",ModName,"', 'enc_",Name,"'}"]),
		{[],NthObj};
	    _ ->
		emit({"    fun 'enc_",ObjName,"'/3"}),
		{[],NthObj}
	end,
    emit([";",nl]),
    emit_default_getenc(ObjSetName,UniqueName),
    emit({".",nl,nl}),
    InternalFunc++Acc;
gen_objset_enc(_Erule,ObjSetName,_UniqueName,['EXTENSIONMARK'],_ClName,
	       _ClFields,_NthObj,Acc) ->
    emit({"'getenc_",ObjSetName,"'(_, _) ->",nl}),
    emit({indent(3),"fun(_, Val, _) ->",nl}),
    emit({indent(6),"BinVal = if",nl}),
    emit({indent(9),"is_list(Val) -> list_to_binary(Val);",nl}),
    emit({indent(9),"true -> Val",nl}),
    emit({indent(6),"end,",nl}),
    emit({indent(6),"Size = size(BinVal),",nl}),
    emit({indent(6),"if",nl}),
    emit({indent(9),"Size < 256 ->",nl}),
    emit({indent(12),"[20,Size,BinVal];",nl}),
    emit({indent(9),"true ->",nl}),
    emit({indent(12),"[21,<<Size:16>>,Val]",nl}),
    emit({indent(6),"end",nl}),
    emit({indent(3),"end.",nl,nl}),
    Acc;
gen_objset_enc(_Erule,_,_,[],_,_,_,Acc) ->
    Acc.

emit_ext_encfun(ModuleName,Name) ->
    emit([indent(4),"fun(T,V,O) -> '",ModuleName,"':'enc_",
	  Name,"'(T,V,O) end"]).

emit_default_getenc(ObjSetName,UniqueName) ->
    emit(["'getenc_",ObjSetName,"'(",{asis,UniqueName},", ErrV) ->",nl]),
    emit([indent(4),"fun(C,V,_) -> exit({'Type not compatible with table constraint',{component,C},{value,V},{unique_name_and_value,",{asis,UniqueName},",ErrV}}) end"]).


%% gen_inlined_enc_funs for each object iterates over all fields of a
%% class, and for each typefield it checks if the object has that
%% field and emits the proper code.
gen_inlined_enc_funs(Erule,Fields,[{typefield,Name,_}|Rest],ObjSetName,NthObj) ->
    CurrMod = get(currmod),
    InternalDefFunName=asn1ct_gen:list2name([NthObj,Name,ObjSetName]),
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when is_record(Type,type) ->
	    emit({indent(3),"fun(Type, Val, _) ->",nl,
		  indent(6),"case Type of",nl}),
	    {Ret,N}=emit_inner_of_fun(Erule,Type,InternalDefFunName),
	    gen_inlined_enc_funs1(Erule,Fields,Rest,ObjSetName,NthObj+N,Ret);
	{value,{_,Type}} when is_record(Type,typedef) ->
	    emit({indent(3),"fun(Type, Val, _) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    {Ret,N}=emit_inner_of_fun(Erule,Type,InternalDefFunName),
	    gen_inlined_enc_funs1(Erule,Fields,Rest,ObjSetName,NthObj+N,Ret);
	{value,{_,#'Externaltypereference'{module=CurrMod,type=T}}} ->
	    emit({indent(3),"fun(Type, Val, _) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    emit([indent(12),"'enc_",T,"'(Val)"]),
%	    {Ret,N} = emit_inner_of_fun(Erule,TDef,InternalDefFunName),
	    gen_inlined_enc_funs1(Erule,Fields,Rest,ObjSetName,NthObj,[]);
	{value,{_,#'Externaltypereference'{module=M,type=T}}} ->
	    emit({indent(3),"fun(Type, Val, _) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    emit([indent(12),"'",M,"'",":'enc_",T,"'(Val)"]),
	    gen_inlined_enc_funs1(Erule,Fields,Rest,ObjSetName,NthObj,[]);
	false ->
	    emit([indent(3),"fun(Type, Val, _) ->",nl,
		  indent(6),"case Type of",nl,
		  indent(9),{asis,Name}," ->",nl,
		  indent(12),"Size = case Val of",nl,
		  indent(15),"B when is_binary(B) -> size(B);",nl,
		  indent(15),"_ -> length(Val)",nl,
		  indent(12),"end,",nl,
		  indent(12),"if",nl,
		  indent(15),"Size < 256 -> [20,Size,Val];",nl,
		  indent(15),"true -> [21,<<Size:16>>,Val]",nl,
		  indent(12),"end"]),
	    gen_inlined_enc_funs1(Erule,Fields,Rest,ObjSetName,NthObj,[])
    end;
gen_inlined_enc_funs(Erule,Fields,[_|Rest],ObjSetName,NthObj) ->
    gen_inlined_enc_funs(Erule,Fields,Rest,ObjSetName,NthObj);
gen_inlined_enc_funs(_Erule,_,[],_,NthObj) ->
    {[],NthObj}.

gen_inlined_enc_funs1(Erule,Fields,[{typefield,Name,_}|Rest],ObjSetName,
		      NthObj,Acc) ->
    CurrentMod = get(currmod),
    InternalDefFunName = asn1ct_gen:list2name([NthObj,Name,ObjSetName]),
    {Acc2,NAdd}=
	case lists:keysearch(Name,1,Fields) of
	    {value,{_,Type}} when is_record(Type,type) ->
		emit({";",nl}),
		{Ret,N}=emit_inner_of_fun(Erule,Type,InternalDefFunName),
		{Ret++Acc,N};
	    {value,{_,Type}} when is_record(Type,typedef) ->
		emit({";",nl,indent(9),{asis,Name}," ->",nl}),
		{Ret,N}=emit_inner_of_fun(Erule,Type,InternalDefFunName),
		{Ret++Acc,N};
	    {value,{_,#'Externaltypereference'{module=CurrentMod,type=T}}} ->
		emit({";",nl,indent(9),{asis,Name}," ->",nl}),
		emit([indent(12),"'enc_",T,"'(Val)"]),
		{Acc,0};
	    {value,{_,#'Externaltypereference'{module=M,type=T}}} ->
		emit({";",nl,indent(9),{asis,Name}," ->",nl}),
		emit([indent(12),"'",M,"'",":'enc_",T,"'(Val)"]),
		{Acc,0};
	    false ->
		emit([";",nl,
		      indent(9),{asis,Name}," ->",nl,
		      indent(12),"Size = case Val of",nl,
		      indent(15),"B when is_binary(B) -> size(B);",nl,
		      indent(15),"_ -> length(Val)",nl,
		      indent(12),"end,",nl,
		      indent(12),"if",nl,
		      indent(15),"Size < 256 -> [20,Size,Val];",nl,
		      indent(15),"true -> [21,<<Size:16>>,Val]",nl,
		      indent(12),"end"]),
		{Acc,0}
	end,
    gen_inlined_enc_funs1(Erule,Fields,Rest,ObjSetName,NthObj+NAdd,Acc2);
gen_inlined_enc_funs1(Erule,Fields,[_|Rest],ObjSetName,NthObj,Acc)->
    gen_inlined_enc_funs1(Erule,Fields,Rest,ObjSetName,NthObj,Acc);
gen_inlined_enc_funs1(_Erule,_,[],_,NthObj,Acc) ->
    emit({nl,indent(6),"end",nl}),
    emit({indent(3),"end"}),
    {Acc,NthObj}.

emit_inner_of_fun(Erule,TDef=#typedef{name={ExtMod,Name},typespec=Type},
		  InternalDefFunName) ->
    case {ExtMod,Name} of
	{primitive,bif} ->
	    emit(indent(12)),
	    gen_encode_prim(Erule,Type,dotag,"Val"),
	    {[],0};
	{constructed,bif} ->
	    emit([indent(12),"'enc_",
		  InternalDefFunName,"'(Val)"]),
	    {[TDef#typedef{name=InternalDefFunName}],1};
	_ ->
	    emit({indent(12),"'",ExtMod,"':'enc_",Name,"'(Val)"}),
	    {[],0}
    end;
emit_inner_of_fun(_Erule,#typedef{name=Name},_) ->
    emit({indent(12),"'enc_",Name,"'(Val)"}),
    {[],0};
emit_inner_of_fun(Erule,Type,_) when is_record(Type,type) ->
    CurrMod = get(currmod),
    case Type#type.def of
	Def when is_atom(Def) ->
	    emit({indent(9),Def," ->",nl,indent(12)}),
	    gen_encode_prim(Erule,Type,dotag,"Val");
	TRef when is_record(TRef,typereference) ->
	    T = TRef#typereference.val,
	    emit({indent(9),T," ->",nl,indent(12),"'enc_",T,"'(Val)"});
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),"'enc_",T,"'(Val)"});
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),ExtMod,":'enc_",
		  T,"'(Val)"})
    end,
    {[],0}.

indent(N) ->
    lists:duplicate(N,32). % 32 = space


gen_objset_dec(_,{unique,undefined},_,_,_,_) ->
    %% There is no unique field in the class of this object set
    %% don't bother about the constraint
    ok;
gen_objset_dec(ObjSName,UniqueName,[{ObjName,Val,Fields},T|Rest],ClName,
	       ClFields,NthObj)->

    emit({"'getdec_",ObjSName,"'(",{asis,UniqueName},",",
	  {asis,Val},") ->",nl}),
    CurrMod = get(currmod),
    NewNthObj=
	case ObjName of
	    {no_mod,no_name} ->
		gen_inlined_dec_funs(Fields,ClFields,ObjSName,NthObj);
	    {CurrMod,Name} ->
		emit(["    fun 'dec_",Name,"'/4"]),
		NthObj;
	    {ModName,Name} ->
		emit_ext_decfun(ModName,Name),
%		emit(["    {'",ModName,"', 'dec_",Name,"'}"]),
		NthObj;
	    _ ->
		emit({"    fun 'dec_",ObjName,"'/4"}),
		NthObj
	end,
    emit({";",nl}),
    gen_objset_dec(ObjSName,UniqueName,[T|Rest],ClName,ClFields,NewNthObj);
gen_objset_dec(ObjSetName,UniqueName,[{ObjName,Val,Fields}],_ClName,
	       ClFields,NthObj) ->

    emit({"'getdec_",ObjSetName,"'(",{asis,UniqueName},",",
	  {asis,Val},") ->",nl}),
    CurrMod=get(currmod),
    case ObjName of
	{no_mod,no_name} ->
	    gen_inlined_dec_funs(Fields,ClFields,ObjSetName,NthObj);
	{CurrMod,Name} ->
	    emit(["    fun 'dec_",Name,"'/4"]);
	{ModName,Name} ->
	    emit_ext_decfun(ModName,Name);
%	    emit(["    {'",ModName,"', 'dec_",Name,"'}"]);
	_ ->
	    emit({"    fun 'dec_",ObjName,"'/4"})
    end,
    emit([";",nl]),
    emit_default_getdec(ObjSetName,UniqueName),
    emit({".",nl,nl}),
    ok;
gen_objset_dec(ObjSetName,_,['EXTENSIONMARK'],_ClName,_ClFields,
	       _NthObj) ->
    emit({"'getdec_",ObjSetName,"'(_, _) ->",nl}),
    emit({indent(3),"fun(Attr1, Bytes, _, _) ->",nl}),
    %%    emit({indent(6),"?RT_PER:decode_open_type(Bytes,[])",nl}),
    emit({indent(6),"{Bytes,Attr1}",nl}),
    emit({indent(3),"end.",nl,nl}),
    ok;
gen_objset_dec(_,_,[],_,_,_) ->
    ok.

emit_ext_decfun(ModuleName,Name) ->
    emit([indent(3),"fun(T,V,O1,O2) -> '",ModuleName,"':'dec_",
	  Name,"'(T,V,O1,O2) end"]).

emit_default_getdec(ObjSetName,UniqueName) ->
    emit(["'getdec_",ObjSetName,"'(",{asis,UniqueName},", ErrV) ->",nl]),
    emit([indent(2), "fun(C,V,_,_) -> exit({{component,C},{value,V},{unique_name_and_value,",{asis,UniqueName},",ErrV}}) end"]).


gen_inlined_dec_funs(Fields,[{typefield,Name,_}|Rest],
		     ObjSetName,NthObj) ->
    CurrMod = get(currmod),
    InternalDefFunName = [NthObj,Name,ObjSetName],
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when is_record(Type,type) ->
	    emit({indent(3),"fun(Type, Val, _, _) ->",nl,
		  indent(6),"case Type of",nl}),
	    N=emit_inner_of_decfun(Type,InternalDefFunName),
	    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj+N);
	{value,{_,Type}} when is_record(Type,typedef) ->
	    emit({indent(3),"fun(Type, Val, _, _) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    N=emit_inner_of_decfun(Type,InternalDefFunName),
	    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj+N);
	{value,{_,#'Externaltypereference'{module=CurrMod,type=T}}} ->
	    emit({indent(3),"fun(Type, Val, _, _) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    emit([indent(12),"'dec_",T,"'(Val, telltype)"]),
	    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj);
	{value,{_,#'Externaltypereference'{module=M,type=T}}} ->
	    emit({indent(3),"fun(Type, Val, _, _) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    emit([indent(12),"'",M,"':'dec_",T,"'(Val, telltype)"]),
	    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj);
	false ->
	    emit([indent(3),"fun(Type, Val, _, _) ->",nl,
		  indent(6),"case Type of",nl,
		  indent(9),{asis,Name}," -> {Val,Type}"]),
	    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj)
    end;
gen_inlined_dec_funs(Fields,[_|Rest],ObjSetName,NthObj) ->
    gen_inlined_dec_funs(Fields,Rest,ObjSetName,NthObj);
gen_inlined_dec_funs(_,[],_,NthObj) ->
    NthObj.

gen_inlined_dec_funs1(Fields,[{typefield,Name,_}|Rest],
		     ObjSetName,NthObj) ->
    CurrentMod = get(currmod),
    InternalDefFunName = [NthObj,Name,ObjSetName],
    N=
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when is_record(Type,type) ->
	    emit({";",nl}),
	    emit_inner_of_decfun(Type,InternalDefFunName);
	{value,{_,Type}} when is_record(Type,typedef) ->
	    emit({";",nl,indent(9),{asis,Name}," ->",nl}),
	    emit_inner_of_decfun(Type,InternalDefFunName);
	{value,{_,#'Externaltypereference'{module=CurrentMod,type=T}}} ->
	    emit([";",nl,indent(9),{asis,Name}," ->",nl]),
	    emit([indent(12),"'dec_",T,"'(Val,telltype)"]),
	    0;
	{value,{_,#'Externaltypereference'{module=M,type=T}}} ->
	    emit([";",nl,indent(9),{asis,Name}," ->",nl]),
	    emit([indent(12),"'",M,"':'dec_",T,"'(Val,telltype)"]),
	    0;
	false ->
	    emit([";",nl,
		  indent(9),{asis,Name}," -> {Val,Type}"]),
	    0
    end,
    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj+N);
gen_inlined_dec_funs1(Fields,[_|Rest],ObjSetName,NthObj)->
    gen_inlined_dec_funs1(Fields,Rest,ObjSetName,NthObj);
gen_inlined_dec_funs1(_,[],_,NthObj) ->
    emit({nl,indent(6),"end",nl}),
    emit({indent(3),"end"}),
    NthObj.

emit_inner_of_decfun(#typedef{name={ExtName,Name},typespec=Type},
		     InternalDefFunName) ->
    case {ExtName,Name} of
	{primitive,bif} ->
	    emit(indent(12)),
	    gen_dec_prim(per,Type,"Val"),
	    0;
	{constructed,bif} ->
	    emit({indent(12),"'dec_",
		  asn1ct_gen:list2name(InternalDefFunName),"'(Val)"}),
	    1;
	_ ->
	    emit({indent(12),"'",ExtName,"':'dec_",Name,
		  "'(Val, telltype)"}),
	    0
    end;
emit_inner_of_decfun(#typedef{name=Name},_) ->
    emit({indent(12),"'dec_",Name,"'(Val, telltype)"}),
    0;
emit_inner_of_decfun(Type,_) when is_record(Type,type) ->
    CurrMod = get(currmod),
    case Type#type.def of
	Def when is_atom(Def) ->
	    emit({indent(9),Def," ->",nl,indent(12)}),
	    gen_dec_prim(erules,Type,"Val");
	TRef when is_record(TRef,typereference) ->
	    T = TRef#typereference.val,
	    emit({indent(9),T," ->",nl,indent(12),"'dec_",T,"'(Val)"});
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),"'dec_",T,"'(Val)"});
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),ExtMod,":'dec_",
		  T,"'(Val)"})
    end,
    0.


gen_internal_funcs(_Erules,[]) ->
    ok;
gen_internal_funcs(Erules,[TypeDef|Rest]) ->
    gen_encode_user(Erules,TypeDef),
    emit([nl,nl,"'dec_",TypeDef#typedef.name,"'(Bytes) ->",nl]),
    gen_decode_user(Erules,TypeDef),
    gen_internal_funcs(Erules,Rest).



%% DECODING *****************************
%%***************************************


gen_decode(Erules,Type) when is_record(Type,typedef) ->
    D = Type,
    emit({nl,nl}),
    emit({"'dec_",Type#typedef.name,"'(Bytes,_) ->",nl}),
    dbdec(Type#typedef.name),
    gen_decode_user(Erules,D).

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
	    emit({nl,"'dec_",asn1ct_gen:list2name(Typename),
		  "'(Bytes,_",ObjFun,") ->",nl}),
	    dbdec(Typename),
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end.

dbdec(Type) when is_list(Type)->
    demit({"io:format(\"decoding: ",asn1ct_gen:list2name(Type),"~w~n\",[Bytes]),",nl});
dbdec(Type) ->
    demit({"io:format(\"decoding: ",{asis,Type},"~w~n\",[Bytes]),",nl}).

gen_decode_user(Erules,D) when is_record(D,typedef) ->
    CurrMod = get(currmod),
    Typename = [D#typedef.name],
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    case asn1ct_gen:type(InnerType) of
	{primitive,bif} ->
	    gen_dec_prim(Erules,Def,"Bytes"),
	    emit({".",nl,nl});
	'ASN1_OPEN_TYPE' ->
	    gen_dec_prim(Erules,Def#type{def='ASN1_OPEN_TYPE'},"Bytes"),
	    emit({".",nl,nl});
	{constructed,bif} ->
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,D);
	#typereference{val=Dname} ->
	    emit({"'dec_",Dname,"'(Bytes,telltype)"}),
	    emit({".",nl,nl});
	#'Externaltypereference'{module=CurrMod,type=Etype} ->
	    emit({"'dec_",Etype,"'(Bytes,telltype).",nl,nl});
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit({"'",Emod,"':'dec_",Etype,"'(Bytes,telltype).",nl,nl});
	Other ->
	    exit({error,{asn1,{unknown,Other}}})
    end.



gen_dec_prim(Erules,Att,BytesVar) ->
    Typename = Att#type.def,
    Constraint = Att#type.constraint,
    case Typename of
	'INTEGER' ->
	    EffectiveConstr = effective_constraint(integer,Constraint),
	    emit_dec_integer(EffectiveConstr,BytesVar);
% 	    emit({"?RT_PER:decode_integer(",BytesVar,",",
% 		  {asis,EffectiveConstr},")"});
	{'INTEGER',NamedNumberList} ->
	    EffectiveConstr = effective_constraint(integer,Constraint),
	    emit_dec_integer(EffectiveConstr,BytesVar,NamedNumberList);
% 	    emit({"?RT_PER:decode_integer(",BytesVar,",",
% 		  {asis,EffectiveConstr},",",
% 		  {asis,NamedNumberList},")"});

	'REAL' ->
	    emit(["?RT_PER:decode_real(",BytesVar,")"]);

	{'BIT STRING',NamedNumberList} ->
	    case get(compact_bit_string) of
		true ->
		    emit({"?RT_PER:decode_compact_bit_string(",
			  BytesVar,",",{asis,Constraint},",",
			  {asis,NamedNumberList},")"});
		_ ->
		    emit({"?RT_PER:decode_bit_string(",BytesVar,",",
			  {asis,Constraint},",",
			  {asis,NamedNumberList},")"})
	    end;
	'NULL' ->
	    emit({"?RT_PER:decode_null(",
		  BytesVar,")"});
	'OBJECT IDENTIFIER' ->
	    emit({"?RT_PER:decode_object_identifier(",
		  BytesVar,")"});
	'RELATIVE-OID' ->
	    emit({"?RT_PER:decode_relative_oid(",
		  BytesVar,")"});
	'ObjectDescriptor' ->
	    emit({"?RT_PER:decode_ObjectDescriptor(",
		  BytesVar,")"});
	{'ENUMERATED',{NamedNumberList1,NamedNumberList2}} ->
	    NewTup = {list_to_tuple([X||{X,_} <- NamedNumberList1]),
		      list_to_tuple([X||{X,_} <- NamedNumberList2])},
	    NewC = [{'ValueRange',{0,size(element(1,NewTup))-1}}],
	    emit({"?RT_PER:decode_enumerated(",BytesVar,",",
		  {asis,NewC},",",
		  {asis,NewTup},")"});
	{'ENUMERATED',NamedNumberList} ->
	    NewNNL = [X||{X,_} <- NamedNumberList],
	    NewC = effective_constraint(integer,
					[{'ValueRange',{0,length(NewNNL)-1}}]),
	    emit_dec_enumerated(BytesVar,NewC,NewNNL);
	'BOOLEAN'->
	    emit({"?RT_PER:decode_boolean(",BytesVar,")"});

	'OCTET STRING' ->
	    emit_dec_octet_string(Constraint,BytesVar);

	'NumericString' ->
	    emit_dec_known_multiplier_string('NumericString',
					     Constraint,BytesVar);
	TString when TString == 'TeletexString';
		     TString == 'T61String' ->
	    emit({"?RT_PER:decode_TeletexString(",BytesVar,",",
		  {asis,Constraint},")"});

	'VideotexString' ->
	    emit({"?RT_PER:decode_VideotexString(",BytesVar,",",
		  {asis,Constraint},")"});

	'UTCTime' ->
	    emit_dec_known_multiplier_string('VisibleString',
					     Constraint,BytesVar);
	'GeneralizedTime' ->
	    emit_dec_known_multiplier_string('VisibleString',
					     Constraint,BytesVar);
	'GraphicString' ->
	    emit({"?RT_PER:decode_GraphicString(",BytesVar,",",
		  {asis,Constraint},")"});

	'VisibleString' ->
	    emit_dec_known_multiplier_string('VisibleString',
					     Constraint,BytesVar);
	'GeneralString' ->
	    emit({"?RT_PER:decode_GeneralString(",BytesVar,",",
		  {asis,Constraint},")"});

	'PrintableString' ->
	    emit_dec_known_multiplier_string('PrintableString',
					     Constraint,BytesVar);
	'IA5String' ->
	    emit_dec_known_multiplier_string('IA5String',Constraint,BytesVar);

	'BMPString' ->
	    emit_dec_known_multiplier_string('BMPString',Constraint,BytesVar);

	'UniversalString' ->
	    emit_dec_known_multiplier_string('UniversalString',
					     Constraint,BytesVar);

	'UTF8String' ->
	    emit({"?RT_PER:decode_UTF8String(",BytesVar,")"});
	'ANY' ->
	    emit(["?RT_PER:decode_open_type(",BytesVar,",", 
		  {asis,Constraint}, ")"]); 
	'ASN1_OPEN_TYPE' ->
	    case Constraint of
		[#'Externaltypereference'{type=Tname}] ->
		    emit(["fun(FBytes) ->",nl,
			  "   {XTerm,XBytes} = "]),
		    emit(["?RT_PER:decode_open_type(FBytes,[]),",nl]),
		    emit(["   {YTerm,_} = dec_",Tname,"(XTerm,mandatory),",nl]),
		    emit(["   {YTerm,XBytes} end(",BytesVar,")"]);
		[#type{def=#'Externaltypereference'{type=Tname}}] ->
		    emit(["fun(FBytes) ->",nl,
			  "   {XTerm,XBytes} = "]),
		    emit(["?RT_PER:decode_open_type(FBytes,[]),",nl]),
		    emit(["   {YTerm,_} = dec_",Tname,"(XTerm,mandatory),",nl]),
		    emit(["   {YTerm,XBytes} end(",BytesVar,")"]);
		_ ->
		    emit(["?RT_PER:decode_open_type(",BytesVar,",[])"])
	    end;
	#'ObjectClassFieldType'{} ->
		case asn1ct_gen:get_inner(Att#type.def) of
		    {fixedtypevaluefield,_,InnerType} -> 
			gen_dec_prim(Erules,InnerType,BytesVar);
		    T ->
			gen_dec_prim(Erules,Att#type{def=T},BytesVar)
		end;
	Other ->
	    exit({'cant decode' ,Other})
    end.


emit_dec_integer(C,BytesVar,NNL) ->
    asn1ct_name:new(tmpterm),
    asn1ct_name:new(buffer),
    Tmpterm = asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm)),
    Buffer = asn1ct_gen:mk_var(asn1ct_name:curr(buffer)),
    emit({" begin {",{curr,tmpterm},",",{curr,buffer},"} = ",nl}),
    emit_dec_integer(C,BytesVar),
    emit({",",nl," case ",Tmpterm," of",nl}),
    lists:map(fun({Name,Int})->emit({"   ",Int," -> {",{asis,Name},",",
				     Buffer,"};",nl});
		 (_)-> exit({error,{asn1,{"error in named number list",NNL}}})
	      end,
	      NNL),
    emit({"   _ -> {",Tmpterm,",",Buffer,"}",nl}),
    emit({" end",nl}), % end of case
    emit(" end"). % end of begin

emit_dec_integer([{'SingleValue',Int}],BytesVar) when is_integer(Int) -> 
    emit(["{",Int,",",BytesVar,"}"]);
emit_dec_integer([{_,{Lb,_Ub},_Range,{BitsOrOctets,N}}],BytesVar) ->
    GetBorO = 
	case BitsOrOctets of
	    bits -> "getbits";
	    _ -> "getoctets"
	end,
    asn1ct_name:new(tmpterm),
    asn1ct_name:new(tmpremain),
    emit({"  begin",nl,"    {",{curr,tmpterm},",",{curr,tmpremain},"}=",
	  "?RT_PER:",GetBorO,"(",BytesVar,",",N,"),",nl}),
    emit({"    {",{curr,tmpterm},"+",Lb,",",{curr,tmpremain},"}",nl,
	  "  end"});
emit_dec_integer([{_,{'MIN',_}}],BytesVar) ->
    emit({"?RT_PER:decode_unconstrained_number(",BytesVar,")"});
emit_dec_integer([{_,{Lb,'MAX'}}],BytesVar) ->
    emit({"?RT_PER:decode_semi_constrained_number(",BytesVar,",",Lb,")"});
emit_dec_integer([{'ValueRange',VR={Lb,Ub}}],BytesVar) ->
    Range = Ub-Lb+1,
     emit({"?RT_PER:decode_constrained_number(",BytesVar,",",
	   {asis,VR},",",Range,")"});
emit_dec_integer(C=[{Rc,_}],BytesVar) when is_tuple(Rc) ->
    emit({"?RT_PER:decode_integer(",BytesVar,",",{asis,C},")"});
emit_dec_integer(_,BytesVar) ->
    emit({"?RT_PER:decode_unconstrained_number(",BytesVar,")"}).
    

emit_dec_enumerated(BytesVar,C,NamedNumberList) ->
    emit_dec_enumerated_begin(),% emits a begin if component
    asn1ct_name:new(tmpterm),
    Tmpterm = asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm)),
    asn1ct_name:new(tmpremain),
    Tmpremain = asn1ct_gen:mk_var(asn1ct_name:curr(tmpremain)),
    emit({"    {",{curr,tmpterm},",",{curr,tmpremain},"} =",nl}),
    emit_dec_integer(C,BytesVar),
    emit({",",nl,"    case ",Tmpterm," of "}),

    Cases=lists:flatten(dec_enumerated_cases(NamedNumberList,Tmpremain,0)),
    emit({Cases++"_->exit({error,{asn1,{decode_enumerated,{",Tmpterm,
	  ",",{asis,NamedNumberList},"}}}}) end",nl}),
    emit_dec_enumerated_end().
	     
emit_dec_enumerated_begin() ->
    case get(component_type) of
	{true,_} ->
	    emit({"  begin",nl});
	_ -> ok
    end.

emit_dec_enumerated_end() ->
    case get(component_type) of
	{true,_} ->
	    emit("  end");
	_ -> ok
    end.


dec_enumerated_cases([Name|Rest],Tmpremain,No) ->
    io_lib:format("~w->{~w,~s};",[No,Name,Tmpremain])++
	dec_enumerated_cases(Rest,Tmpremain,No+1);
dec_enumerated_cases([],_,_) ->
    "".
