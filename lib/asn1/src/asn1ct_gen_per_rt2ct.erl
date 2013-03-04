%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2013. All Rights Reserved.
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

%% Handle encoding of primitives for aligned PER.

-include("asn1_records.hrl").

-export([gen_encode_prim/4]).

-import(asn1ct_gen, [emit/1,demit/1]).
-import(asn1ct_func, [call/3]).

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
	'REAL' ->
	    emit_enc_real(Erules, Value);

	{'BIT STRING',NamedNumberList} ->
	    EffectiveC = effective_constraint(bitstring,Constraint),
	    case EffectiveC of
		0 ->
		    emit({"[]"});
		_ ->
		    call(Erules, encode_bit_string,
			 [{asis,EffectiveC},Value,
			  {asis,NamedNumberList}])
	    end;
	'NULL' ->
	    emit("[]");
	'OBJECT IDENTIFIER' ->
	    call(Erules, encode_object_identifier, [Value]);
	'RELATIVE-OID' ->
	    call(Erules, encode_relative_oid, [Value]);
	'ObjectDescriptor' ->
	    call(Erules, encode_ObjectDescriptor,
		 [{asis,Constraint},Value]);
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
	    call(Erules, encode_TeletexString, [{asis,Constraint},Value]);
	'VideotexString' ->
	    call(Erules, encode_VideotexString, [{asis,Constraint},Value]);
	'UTCTime' ->
	    emit_enc_known_multiplier_string('VisibleString',Constraint,Value);
	'GeneralizedTime' ->
	    emit_enc_known_multiplier_string('VisibleString',Constraint,Value);
	'GraphicString' ->
	    call(Erules, encode_GraphicString, [{asis,Constraint},Value]);
	'VisibleString' ->
	    emit_enc_known_multiplier_string('VisibleString',Constraint,Value);
	'GeneralString' ->
	    call(Erules, encode_GeneralString, [{asis,Constraint},Value]);
	'PrintableString' ->
	    emit_enc_known_multiplier_string('PrintableString',Constraint,Value);
	'IA5String' ->
	    emit_enc_known_multiplier_string('IA5String',Constraint,Value);
	'BMPString' ->
	    emit_enc_known_multiplier_string('BMPString',Constraint,Value);
	'UniversalString' ->
	    emit_enc_known_multiplier_string('UniversalString',Constraint,Value);
	'UTF8String' ->
	    call(Erules, encode_UTF8String, [Value]);
	'ASN1_OPEN_TYPE' ->
	    NewValue = case Constraint of
			   [#'Externaltypereference'{type=Tname}] ->
			       asn1ct_func:need({Erules,complete,1}),
			       io_lib:format(
				 "complete(enc_~s(~s))",[Tname,Value]);
			   [#type{def=#'Externaltypereference'{type=Tname}}] ->
			       asn1ct_func:need({Erules,complete,1}),
			       io_lib:format(
				 "complete(enc_~s(~s))",
				 [Tname,Value]);
			 _ -> Value
		     end,
	    call(Erules, encode_open_type, [NewValue]);
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

emit_enc_real(Erules, Real) ->
    asn1ct_name:new(tmpval),
    asn1ct_name:new(tmplen),
    emit(["begin",nl,
	  "{",{curr,tmpval},com,{curr,tmplen},"} = ",
	  {call,real_common,encode_real,[Real]},com,nl,
	  "[",{call,Erules,encode_length,[{curr,tmplen}]},",",nl,
	  {call,Erules,octets_to_complete,
	   [{curr,tmplen},{curr,tmpval}]},"]",nl,
	  "end"]).

emit_enc_known_multiplier_string(StringType,C,Value) ->
    SizeC = effective_constraint(bitstring, C),
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
    emit_enc_k_m_string(SizeC, NumBits, CharOutTab, Value).

emit_enc_k_m_string(0, _NumBits, _CharOutTab, _Value) ->
    emit({"[]"});
emit_enc_k_m_string(SizeC, NumBits, CharOutTab, Value) ->
    call(per, encode_known_multiplier_string,
	 [{asis,SizeC},NumBits,{asis,CharOutTab},Value]).


%% copied from run time module

get_CharOutTab(C, StringType) ->
    case get_constraint(C,'PermittedAlphabet') of
	{'SingleValue',Sv} ->
	    get_CharTab2(C, StringType, hd(Sv), lists:max(Sv), Sv);
	no ->
	    case StringType of
		'IA5String' ->
		    {0,16#7F,notab};
		'VisibleString' ->
		    get_CharTab2(C, StringType, 16#20, 16#7F, notab);
		'PrintableString' ->
		    Chars = lists:sort(
			      " '()+,-./0123456789:=?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"),
		    get_CharTab2(C, StringType, hd(Chars),
				 lists:max(Chars), Chars);
		'NumericString' ->
		    get_CharTab2(C, StringType, 16#20, $9, " 0123456789");
		'UniversalString' ->
		    {0,16#FFFFFFFF,notab};
		'BMPString' ->
		    {0,16#FFFF,notab}
	    end
    end.

get_CharTab2(C, StringType, Min, Max, Chars) ->
    BitValMax = (1 bsl get_NumBits(C,StringType))-1,
    if
	Max =< BitValMax ->
	    {0,Max,notab};
	true ->
	    {Min,Max,create_char_tab(Min,Chars)}
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

emit_enc_octet_string(Erules, Constraint, Value) ->
    case effective_constraint(bitstring, Constraint) of
	0 ->
	    emit({"  []"});
	1 ->
	    asn1ct_name:new(tmpval),
	    emit({"  begin",nl}),
	    emit({"    [",{curr,tmpval},"] = ",Value,",",nl}),
	    emit(["    [[10,8],",{curr,tmpval},"]",nl]),
	    emit("  end");
	2 ->
	    asn1ct_name:new(tmpval),
	    emit(["  begin",nl,
		  "    ",{curr,tmpval}," = ",Value,",",nl,
		  "    case length(",{curr,tmpval},") of",nl,
		  "      2 ->",nl,
		  "        [[45,16,2]|",{curr,tmpval},"];",nl,
		  "      _ ->",nl,
		  "        exit({error,{value_out_of_bounds,",
		  {curr,tmpval},"}})",nl,
		  "    end",nl,
		  "  end"]);
	Sv when is_integer(Sv), Sv < 256  ->
	    asn1ct_name:new(tmpval),
	    asn1ct_name:new(tmplen),
	    emit(["  begin",nl,
		  "    ",{curr,tmpval}," = ",Value,",",nl,
		  "    case length(",{curr,tmpval},") of",nl,
		  "      ",Sv,"=",{curr,tmplen}," ->",nl,
		  "       [20,",{curr,tmplen},"|",{curr,tmpval},"];",nl,
		  "      _ ->",nl,
		  "       exit({error,{value_out_of_bounds,",
		  {curr,tmpval},"}})",nl,
		  "    end",nl,
		  "  end"]);
	Sv when is_integer(Sv),Sv =< 65535  ->
	    asn1ct_name:new(tmpval),
	    asn1ct_name:new(tmplen),
	    emit(["  begin",nl,
		  "    ",{curr,tmpval}," = ",Value,",",nl,
		  "    case length(",{curr,tmpval},") of",nl,
		  "      ",Sv,"=",{curr,tmplen}," ->",nl,
		  "        [<<21,",{curr,tmplen},":16>>|",Value,"];",nl,
		  "      _ ->",nl,
		  "        exit({error,{value_out_of_bounds,",
		  {curr,tmpval},"}})",nl,
		  "    end",nl,
		  "  end"]);
	C ->
	    call(Erules, encode_octet_string,
		 [{asis,C},Value])
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

emit_enc_integer(Erule, [{'ValueRange',{Lb,Ub}=VR}], Value)
  when is_integer(Lb), is_integer(Ub) ->
    call(Erule, encode_constrained_number, [{asis,VR},Value]);

emit_enc_integer(Erule, C, Value) ->
    call(Erule, encode_integer, [{asis,C},Value]).


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
    pre_encode(integer, asn1ct_imm:effective_constraint(integer, C));
effective_constraint(bitstring,C) ->
    asn1ct_imm:effective_constraint(bitstring, C).

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
