%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-module(asn1ct_imm).
-export([per_dec_raw_bitstring/2,
	 per_dec_boolean/0,per_dec_enumerated/2,per_dec_enumerated/3,
	 per_dec_extension_map/1,
	 per_dec_integer/2,per_dec_k_m_string/3,
	 per_dec_length/3,per_dec_named_integer/3,
	 per_dec_octet_string/2,per_dec_open_type/1,per_dec_real/1,
	 per_dec_restricted_string/1]).
-export([per_dec_constrained/3,per_dec_normally_small_number/1]).
-export([per_enc_bit_string/4,per_enc_legacy_bit_string/4,
	 per_enc_boolean/2,
	 per_enc_choice/3,per_enc_enumerated/3,
	 per_enc_integer/3,per_enc_integer/4,
	 per_enc_null/2,
	 per_enc_k_m_string/4,per_enc_octet_string/3,
	 per_enc_legacy_octet_string/3,
	 per_enc_open_type/2,
	 per_enc_restricted_string/3,
	 per_enc_small_number/2]).
-export([per_enc_extension_bit/2,per_enc_extensions/4,per_enc_optional/3]).
-export([per_enc_sof/5]).
-export([enc_absent/3,enc_append/1,enc_element/2]).
-export([enc_cg/2]).
-export([optimize_alignment/1,optimize_alignment/2,
	 dec_slim_cg/2,dec_code_gen/2]).
-export([effective_constraint/2]).
-import(asn1ct_gen, [emit/1]).

-record(st, {var,
	     base}).

dec_slim_cg(Imm0, BytesVar) ->
    {Imm,_} = optimize_alignment(Imm0),
    asn1ct_name:new(v),
    [H|T] = atom_to_list(asn1ct_name:curr(v)) ++ "@",
    VarBase = [H-($a-$A)|T],
    St0 = #st{var=0,base=VarBase},
    {Res,Pre,_} = flatten(Imm, BytesVar, St0),
    dcg_list_outside(Pre),
    Res.

dec_code_gen(Imm, BytesVar) ->
    emit(["begin",nl]),
    {Dst,DstBuf} = dec_slim_cg(Imm, BytesVar),
    emit([",",nl,
	  "{",Dst,",",DstBuf,"}",nl,
	  "end"]),
    ok.

optimize_alignment(Imm) ->
    opt_al(Imm, unknown).

optimize_alignment(Imm, Al) ->
    opt_al(Imm, Al).


per_dec_boolean() ->
    {map,{get_bits,1,[1]},[{0,false},{1,true}]}.

per_dec_enumerated([{V,_}], _Aligned) ->
    {value,V};
per_dec_enumerated(NamedList0, Aligned) ->
    Ub = length(NamedList0) - 1,
    Constraint = [{'ValueRange',{0,Ub}}],
    Int = per_dec_integer(Constraint, Aligned),
    NamedList = per_dec_enumerated_fix_list(NamedList0, [enum_error], 0),
    {map,Int,opt_map(NamedList, Int)}.

per_dec_enumerated(BaseNamedList, NamedListExt0, Aligned) ->
    Base = per_dec_enumerated(BaseNamedList, Aligned),
    NamedListExt = per_dec_enumerated_fix_list(NamedListExt0,
					       [enum_default], 0),
    Ext = {map,per_dec_normally_small_number(Aligned),NamedListExt},
    bit_case(Base, Ext).

per_dec_extension_map(Aligned) ->
    Len = per_dec_normally_small_length(Aligned),
    {get_bits,Len,[1,bitstring]}.

per_dec_integer(Constraint0, Aligned) ->
    Constraint = effective_constraint(integer, Constraint0),
    per_dec_integer_1(Constraint, Aligned).

per_dec_length(SingleValue, _, _Aligned) when is_integer(SingleValue) ->
    {value,SingleValue};
per_dec_length({{Fixed,Fixed},[]}, AllowZero, Aligned) ->
    bit_case(per_dec_length(Fixed, AllowZero, Aligned),
	     per_dec_length(no, AllowZero, Aligned));
per_dec_length({{_,_}=Constr,[]}, AllowZero, Aligned) ->
    bit_case(per_dec_length(Constr, AllowZero, Aligned),
	     per_dec_length(no, AllowZero, Aligned));
per_dec_length({Lb,Ub}, _AllowZero, Aligned) when is_integer(Lb),
						  is_integer(Lb) ->
    per_dec_constrained(Lb, Ub, Aligned);
per_dec_length(no, AllowZero, Aligned) ->
    decode_unconstrained_length(AllowZero, Aligned).

per_dec_named_integer(Constraint, NamedList0, Aligned) ->
    Int = per_dec_integer(Constraint, Aligned),
    NamedList = [{K,V} || {V,K} <- NamedList0] ++ [integer_default],
    {map,Int,opt_map(NamedList, Int)}.

per_dec_k_m_string(StringType, Constraint, Aligned) ->
    SzConstr = effective_constraint(bitstring, Constraint),
    N = string_num_bits(StringType, Constraint, Aligned),
    Imm = dec_string(SzConstr, N, Aligned, k_m_string),
    Chars = char_tab(Constraint, StringType, N),
    convert_string(N, Chars, Imm).

per_dec_octet_string(Constraint, Aligned) ->
    dec_string(Constraint, 8, Aligned, 'OCTET STRING').

per_dec_raw_bitstring(Constraint, Aligned) ->
    dec_string(Constraint, 1, Aligned, 'BIT STRING').

per_dec_open_type(Aligned) ->
    dec_string(no, 8, Aligned, open_type).

per_dec_real(Aligned) ->
    Dec = fun(V, Buf) ->
		  emit(["{",{call,real_common,decode_real,[V]},
			com,Buf,"}"])
	  end,
    {call,Dec,
     {get_bits,decode_unconstrained_length(true, Aligned),
      [8,binary,{align,Aligned}]}}.

per_dec_restricted_string(Aligned) ->
    DecLen = decode_unconstrained_length(true, Aligned),
    {get_bits,DecLen,[8,binary]}.

%%%
%%% Encoding.
%%%

per_enc_bit_string(Val, [], Constraint0, Aligned) ->
    {B,[[],Bits]} = mk_vars([], [bits]),
    Constraint = effective_constraint(bitstring, Constraint0),
    B ++ [{call,erlang,bit_size,[Val],Bits}|
	  per_enc_length(Val, 1, Bits, Constraint, Aligned, 'BIT STRING')];
per_enc_bit_string(Val0, NNL0, Constraint0, Aligned) ->
    {B,[Val,Bs,Bits,Positions]} = mk_vars(Val0, [bs,bits,positions]),
    NNL = lists:keysort(2, NNL0),
    Constraint = effective_constraint(bitstring, Constraint0),
    ExtraArgs = case constr_min_size(Constraint) of
		    no -> [];
		    Lb -> [Lb]
		end,
    ToBs = case ExtraArgs of
	       [] ->
		   {call,per_common,bs_drop_trailing_zeroes,[Val]};
	       [0] ->
		   {call,per_common,bs_drop_trailing_zeroes,[Val]};
	       [Lower] ->
		   {call,per_common,adjust_trailing_zeroes,[Val,Lower]}
	   end,
    B ++ [{'try',
	   [bit_string_name2pos_fun(NNL, Val)],
	   {Positions,
	    [{call,per_common,bitstring_from_positions,
	      [Positions|ExtraArgs]}]},
	   [ToBs],Bs},
	  {call,erlang,bit_size,[Bs],Bits}|
	  per_enc_length(Bs, 1, Bits, Constraint, Aligned, 'BIT STRING')].

per_enc_legacy_bit_string(Val0, [], Constraint0, Aligned) ->
    {B,[Val,Bs,Bits]} = mk_vars(Val0, [bs,bits]),
    Constraint = effective_constraint(bitstring, Constraint0),
    ExtraArgs = case constr_min_size(Constraint) of
		    no -> [];
		    Lb -> [Lb]
		end,
    B ++ [{call,per_common,to_bitstring,[Val|ExtraArgs],Bs},
	  {call,erlang,bit_size,[Bs],Bits}|
	  per_enc_length(Bs, 1, Bits, Constraint, Aligned, 'BIT STRING')];
per_enc_legacy_bit_string(Val0, NNL0, Constraint0, Aligned) ->
    {B,[Val,Bs,Bits,Positions]} = mk_vars(Val0, [bs,bits,positions]),
    NNL = lists:keysort(2, NNL0),
    Constraint = effective_constraint(bitstring, Constraint0),
    ExtraArgs = case constr_min_size(Constraint) of
		    no -> [];
		    0 -> [];
		    Lb -> [Lb]
		end,
    B ++ [{'try',
	   [bit_string_name2pos_fun(NNL, Val)],
	   {Positions,
	    [{call,per_common,bitstring_from_positions,
	      [Positions|ExtraArgs]}]},
	   [{call,per_common,to_named_bitstring,[Val|ExtraArgs]}],Bs},
	  {call,erlang,bit_size,[Bs],Bits}|
	  per_enc_length(Bs, 1, Bits, Constraint, Aligned, 'BIT STRING')].

per_enc_boolean(Val0, _Aligned) ->
    {B,[Val]} = mk_vars(Val0, []),
    B++build_cond([[{eq,Val,false},{put_bits,0,1,[1]}],
		   [{eq,Val,true},{put_bits,1,1,[1]}]]).

per_enc_choice(Val0, Cs0, _Aligned) ->
    {B,[Val]} = mk_vars(Val0, []),
    Cs = [[{eq,Val,Tag}|opt_choice(Imm)] || {Tag,Imm} <- Cs0],
    B++build_cond(Cs).

per_enc_enumerated(Val0, {Root,Ext}, Aligned) ->
    {B,[Val]} = mk_vars(Val0, []),
    Constr = enumerated_constraint(Root),
    RootCs = per_enc_enumerated_root(Root, [{put_bits,0,1,[1]}],
				     Val, Constr, Aligned),
    ExtCs = per_enc_enumerated_ext(Ext, Val, Aligned),
    B++[{'cond',RootCs++ExtCs++enumerated_error(Val)}];
per_enc_enumerated(Val0, Root, Aligned) ->
    {B,[Val]} = mk_vars(Val0, []),
    Constr = enumerated_constraint(Root),
    Cs = per_enc_enumerated_root(Root, [], Val, Constr, Aligned),
    B++[{'cond',Cs++enumerated_error(Val)}].

enumerated_error(Val) ->
    [['_',{error,Val}]].

per_enc_integer(Val0, Constraint0, Aligned) ->
    {B,[Val]} = mk_vars(Val0, []),
    Constraint = effective_constraint(integer, Constraint0),
    B ++ per_enc_integer_1(Val, Constraint, Aligned).

per_enc_integer(Val0, NNL, Constraint0, Aligned) ->
    {B,[Val]} = mk_vars(Val0, []),
    Constraint = effective_constraint(integer, Constraint0),
    Cs = [[{eq,Val,N}|per_enc_integer_1(V, Constraint, Aligned)] ||
	     {N,V} <- NNL],
    case per_enc_integer_1(Val, Constraint, Aligned) of
	[{'cond',IntCs}] ->
	    B ++ [{'cond',Cs++IntCs}];
	Other ->
	    B ++ [{'cond',Cs++[['_'|Other]]}]
    end.

per_enc_null(_Val, _Aligned) ->
    [].

per_enc_k_m_string(Val0, StringType, Constraint, Aligned) ->
    {B,[Val,Bin,Len]} = mk_vars(Val0, [bin,len]),
    SzConstraint = effective_constraint(bitstring, Constraint),
    Unit = string_num_bits(StringType, Constraint, Aligned),
    Chars0 = char_tab(Constraint, StringType, Unit),
    Enc = case Unit of
	      16 ->
		  {call,per_common,encode_chars_16bit,[Val],Bin};
	      32 ->
		  {call,per_common,encode_big_chars,[Val],Bin};
	      8 ->
		  {call,erlang,list_to_binary,[Val],Bin};
	      _ ->
		  case enc_char_tab(Chars0) of
		      notab ->
			  {call,per_common,encode_chars,[Val,Unit],Bin};
		      {tab,Tab} ->
			  {call,per_common,encode_chars,[Val,Unit,Tab],Bin};
		      {compact_map,Map} ->
			  {call,per_common,encode_chars_compact_map,
			   [Val,Unit,Map],Bin}
		  end
	  end,
    case Unit of
	8 ->
	    B ++ [Enc,{call,erlang,byte_size,[Bin],Len}];
	_ ->
	    B ++ [{call,erlang,length,[Val],Len},Enc]
    end ++ per_enc_length(Bin, Unit, Len, SzConstraint, Aligned, k_m_string).

per_enc_open_type(Imm0, Aligned) ->
    Imm = case Aligned of
	      true ->
		  %% Temporarily make the implicit 'align' done by
		  %% complete/1 explicit to facilitate later
		  %% optimizations: the absence of 'align' can be used
		  %% as an indication that complete/1 can be replaced
		  %% with a cheaper operation such as
		  %% iolist_to_binary/1. The redundant 'align' will be
		  %% optimized away later.
		  Imm0 ++ [{put_bits,0,0,[1,align]}];
	      false ->
		  Imm0
	  end,
    {[],[[],Val,Len,Bin]} = mk_vars([], [output,len,bin]),
    [{list,Imm,Val},
     {call,enc_mod(Aligned),complete,[Val],Bin},
     {call,erlang,byte_size,[Bin],Len}|
     per_enc_length(Bin, 8, Len, Aligned)].

per_enc_octet_string(Bin, Constraint0, Aligned) ->
    {B,[[],Len]} = mk_vars([], [len]),
    Constraint = effective_constraint(bitstring, Constraint0),
    B ++ [{call,erlang,byte_size,[Bin],Len}|
	  per_enc_length(Bin, 8, Len, Constraint, Aligned, 'OCTET STRING')].

per_enc_legacy_octet_string(Val0, Constraint0, Aligned) ->
    {B,[Val,Bin,Len]} = mk_vars(Val0, [bin,len]),
    Constraint = effective_constraint(bitstring, Constraint0),
    B ++ [{call,erlang,iolist_to_binary,[Val],Bin},
	  {call,erlang,byte_size,[Bin],Len}|
	  per_enc_length(Bin, 8, Len, Constraint, Aligned, 'OCTET STRING')].

per_enc_restricted_string(Val0, {M,F}, Aligned) ->
    {B,[Val,Bin,Len]} = mk_vars(Val0, [bin,len]),
    B ++ [{call,M,F,[Val],Bin},
	  {call,erlang,byte_size,[Bin],Len}|
	  per_enc_length(Bin, 8, Len, Aligned)].

per_enc_small_number(Val, Aligned) ->
    build_cond([[{lt,Val,64},{put_bits,Val,7,[1]}],
		['_',{put_bits,1,1,[1]}|
		 per_enc_unsigned(Val, Aligned)]]).

per_enc_extension_bit(Val0, _Aligned) ->
    {B,[Val]} = mk_vars(Val0, []),
    B++build_cond([[{eq,Val,[]},{put_bits,0,1,[1]}],
		   ['_',{put_bits,1,1,[1]}]]).

per_enc_extensions(Val0, Pos0, NumBits, Aligned) when NumBits > 0 ->
    Pos = Pos0 + 1,
    {B,[Val,Bitmap]} = mk_vars(Val0, [bitmap]),
    Length = per_enc_small_length(NumBits, Aligned),
    PutBits = case NumBits of
		  1 -> [{put_bits,1,1,[1]}];
		  _ -> [{put_bits,Bitmap,NumBits,[1]}]
	      end,
    B++[{call,per_common,extension_bitmap,[Val,Pos,Pos+NumBits],Bitmap},
	{list,[{'cond',[[{eq,Bitmap,0}],
			['_'|Length ++ PutBits]]}],
	 {var,"Extensions"}}].

per_enc_optional(Val0, {Pos,DefVals}, _Aligned) when is_integer(Pos),
						     is_list(DefVals) ->
    {B,Val} = enc_element(Pos, Val0),
    Zero = {put_bits,0,1,[1]},
    One = {put_bits,1,1,[1]},
    B++[{'cond',
	 [[{eq,Val,DefVal},Zero] || DefVal <- DefVals] ++ [['_',One]]}];
per_enc_optional(Val0, {Pos,{call,M,F,A}}, _Aligned) when is_integer(Pos) ->
    {B,Val} = enc_element(Pos, Val0),
    {[],[[],Tmp]} = mk_vars([], [tmp]),
    Zero = {put_bits,0,1,[1]},
    One = {put_bits,1,1,[1]},
    B++[{call,M,F,[Val|A],Tmp},
	{'cond',
	 [[{eq,Tmp,true},Zero],['_',One]]}];
per_enc_optional(Val0, Pos, _Aligned) when is_integer(Pos) ->
    {B,Val} = enc_element(Pos, Val0),
    Zero = {put_bits,0,1,[1]},
    One = {put_bits,1,1,[1]},
    B++[{'cond',[[{eq,Val,asn1_NOVALUE},Zero],
		 ['_',One]]}].

per_enc_sof(Val0, Constraint, ElementVar, ElementImm, Aligned) ->
    {B,[Val,Len]} = mk_vars(Val0, [len]),
    SzConstraint = effective_constraint(bitstring, Constraint),
    LenImm = enc_length(Len, SzConstraint, Aligned),
    Lc0 = [{lc,ElementImm,{var,atom_to_list(ElementVar)},Val}],
    Lc = opt_lc(Lc0, LenImm),
    PreBlock = B ++ [{call,erlang,length,[Val],Len}],
    case LenImm of
	[{'cond',[[C|Action]]}] ->
	    PreBlock ++ [{'cond',[[C|Action++Lc]]}];
	[{sub,_,_,_}=Sub,{'cond',[[C|Action]]}] ->
	    PreBlock ++
		[Sub,{'cond',[[C|Action++Lc]]}];
	EncLen ->
	    PreBlock ++ EncLen ++ Lc
    end.

enc_absent(Val0, {call,M,F,A}, Body) ->
    {B,[Var,Tmp]} = mk_vars(Val0, [tmp]),
    B++[{call,M,F,[Var|A],Tmp},
	{'cond',
	 [[{eq,Tmp,true}],['_'|Body]]}];
enc_absent(Val0, AbsVals, Body) when is_list(AbsVals) ->
    {B,[Var]} = mk_vars(Val0, []),
    Cs = [[{eq,Var,Aval}] || Aval <- AbsVals] ++ [['_'|Body]],
    B++build_cond(Cs).

enc_append([[]|T]) ->
    enc_append(T);
enc_append([[{put_bits,_,_,_}|_]=Pb|[Imm|T]=T0]) ->
    case opt_choice(Pb++Imm) of
	[{put_bits,_,_,_}|_] ->
	    [{block,Pb}|enc_append(T0)];
	Opt ->
	    enc_append([Opt|T])
    end;
enc_append([Imm0|[Imm1|T]=T0]) ->
    try combine_imms(Imm0, Imm1) of
	Imm ->
	    enc_append([Imm|T])
    catch
	throw:impossible ->
	    [{block,Imm0}|enc_append(T0)]
    end;
enc_append([H|T]) ->
    [{block,H}|enc_append(T)];
enc_append([]) -> [].

enc_element(N, Val0) ->
    {[],[Val,Dst]} = mk_vars(Val0, [element]),
    {[{call,erlang,element,[N,Val],Dst}],Dst}.

enc_cg(Imm0, false) ->
    Imm1 = enc_cse(Imm0),
    Imm2 = enc_pre_cg(Imm1),
    Imm = enc_opt(Imm2),
    enc_cg(Imm);
enc_cg(Imm0, true) ->
    Imm1 = enc_cse(Imm0),
    Imm2 = enc_hoist_align(Imm1),
    Imm3 = enc_opt_al(Imm2),
    Imm4 = per_fixup(Imm3),
    Imm5 = enc_pre_cg(Imm4),
    Imm = enc_opt(Imm5),
    enc_cg(Imm).

%%%
%%% Local functions.
%%%

%% is_aligned(StringType, LowerBound, UpperBound) -> boolean()
%%     StringType = 'OCTET STRING' | 'BIT STRING' | k_m_string
%%     LowerBound = UpperBound = number of bits
%%  Determine whether a string should be aligned in PER.

is_aligned(T, Lb, Ub) when T =:= 'OCTET STRING'; T =:= 'BIT STRING' ->
    %% OCTET STRINGs and BIT STRINGs are aligned to a byte boundary
    %% unless the size is fixed and less than or equal to 16 bits.
    Lb =/= Ub orelse Lb > 16;
is_aligned(k_m_string, _Lb, Ub) ->
    %% X.691 (07/2002) 27.5.7 says if the upper bound times the number
    %% of bits is greater than or equal to 16, then the bit field should
    %% be aligned.
    Ub >= 16.

%%%
%%% Generating the intermediate format format for decoding.
%%%

dec_string(Sv, U, Aligned0, T) when is_integer(Sv) ->
    Bits = U*Sv,
    Aligned = Aligned0 andalso is_aligned(T, Bits, Bits),
    {get_bits,Sv,[U,binary,{align,Aligned}]};
dec_string({{Sv,Sv},[]}, U, Aligned, T) ->
    bit_case(dec_string(Sv, U, Aligned, T),
	     dec_string(no, U, Aligned, T));
dec_string({{_,_}=C,[]}, U, Aligned, T) ->
    bit_case(dec_string(C, U, Aligned, T),
	     dec_string(no, U, Aligned, T));
dec_string({Lb,Ub}, U, Aligned0, T) ->
    Len = per_dec_constrained(Lb, Ub, Aligned0),
    Aligned = Aligned0 andalso is_aligned(T, Lb*U, Ub*U),
    {get_bits,Len,[U,binary,{align,Aligned}]};
dec_string(_, U, Aligned, _T) ->
    Al = [{align,Aligned}],
    DecRest = fun(V, Buf) ->
		      asn1ct_func:call(per_common,
				       decode_fragmented,
				       [V,Buf,U])
	      end,
    {'case',[{test,{get_bits,1,[1|Al]},0,
	      {value,{get_bits,
		      {get_bits,7,[1]},
		      [U,binary]}}},
	     {test,{get_bits,1,[1|Al]},1,
	      {test,{get_bits,1,[1]},0,
	       {value,{get_bits,
		       {get_bits,14,[1]},
		       [U,binary]}}}},
	     {test,{get_bits,1,[1|Al]},1,
	      {test,{get_bits,1,[1]},1,
	       {value,{call,DecRest,{get_bits,6,[1]}}}}}]}.

per_dec_enumerated_fix_list([{V,_}|T], Tail, N) ->
    [{N,V}|per_dec_enumerated_fix_list(T, Tail, N+1)];
per_dec_enumerated_fix_list([], Tail, _) -> Tail.

per_dec_integer_1([{'SingleValue',Value}], _Aligned) ->
    {value,Value};
per_dec_integer_1([{'ValueRange',{'MIN',_}}], Aligned) ->
    per_dec_unconstrained(Aligned);
per_dec_integer_1([{'ValueRange',{Lb,'MAX'}}], Aligned) when is_integer(Lb) ->
    per_decode_semi_constrained(Lb, Aligned);
per_dec_integer_1([{'ValueRange',{Lb,Ub}}], Aligned) when is_integer(Lb),
						    is_integer(Ub) ->
    per_dec_constrained(Lb, Ub, Aligned);
per_dec_integer_1([{{_,_}=Constr0,_}], Aligned) ->
    Constr = effective_constraint(integer, [Constr0]),
    bit_case(per_dec_integer(Constr, Aligned),
	     per_dec_unconstrained(Aligned));
per_dec_integer_1([], Aligned) ->
    per_dec_unconstrained(Aligned).

per_dec_unconstrained(Aligned) ->
    {get_bits,decode_unconstrained_length(false, Aligned),[8,signed]}.

per_dec_constrained(Lb, Ub, false) ->
    Range = Ub - Lb + 1,
    Get = {get_bits,uper_num_bits(Range),[1]},
    add_lb(Lb, Get);
per_dec_constrained(Lb, Ub, true) ->
    Range = Ub - Lb + 1,
    Get = if
	      Range =< 255 ->
		  {get_bits,per_num_bits(Range),[1,unsigned]};
	      Range == 256 ->
		  {get_bits,1,[8,unsigned,{align,true}]};
	      Range =< 65536 ->
		  {get_bits,2,[8,unsigned,{align,true}]};
	      true ->
		  RangeOctLen = byte_size(binary:encode_unsigned(Range - 1)),
		  {get_bits,per_dec_length({1,RangeOctLen}, false, true),
		   [8,unsigned,{align,true}]}
	  end,
    add_lb(Lb, Get).

add_lb(0, Get) -> Get;
add_lb(Lb, Get) -> {add,Get,Lb}.

per_dec_normally_small_number(Aligned) ->
    Small = {get_bits,6,[1]},
    Unlimited = per_decode_semi_constrained(0, Aligned),
    bit_case(Small, Unlimited).

per_dec_normally_small_length(Aligned) ->
    Small = {add,{get_bits,6,[1]},1},
    Unlimited = decode_unconstrained_length(false, Aligned),
    bit_case(Small, Unlimited).

per_decode_semi_constrained(Lb, Aligned) ->
    add_lb(Lb, {get_bits,decode_unconstrained_length(false, Aligned),[8]}).

bit_case(Base, Ext) ->
    {'case',[{test,{get_bits,1,[1]},0,Base},
	     {test,{get_bits,1,[1]},1,Ext}]}.

decode_unconstrained_length(AllowZero, Aligned) ->
    Al = [{align,Aligned}],
    Zero = case AllowZero of
	       false -> [non_zero];
	       true -> []
	   end,
    {'case',[{test,{get_bits,1,[1|Al]},0,
	      {value,{get_bits,7,[1|Zero]}}},
	     {test,{get_bits,1,[1|Al]},1,
	      {test,{get_bits,1,[1]},0,
	       {value,{get_bits,14,[1|Zero]}}}}]}.

uper_num_bits(N) ->
    uper_num_bits(N, 1, 0).

uper_num_bits(N, T, B) when N =< T -> B;
uper_num_bits(N, T, B) -> uper_num_bits(N, T bsl 1, B+1).

per_num_bits(2) -> 1;
per_num_bits(N) when N =< 4 -> 2;
per_num_bits(N) when N =< 8 -> 3;
per_num_bits(N) when N =< 16 -> 4;
per_num_bits(N) when N =< 32 -> 5;
per_num_bits(N) when N =< 64 -> 6;
per_num_bits(N) when N =< 128 -> 7;
per_num_bits(N) when N =< 255 -> 8.

opt_map(Map, Imm) ->
    case matched_range(Imm) of
	unknown -> Map;
	{Lb,Ub} -> opt_map_1(Map, Lb, Ub)
    end.

opt_map_1([{I,_}=Pair|T], Lb, Ub) ->
    if
	I =:= Lb, I =< Ub ->
	    [Pair|opt_map_1(T, Lb+1, Ub)];
	Lb < I, I =< Ub ->
	    [Pair|opt_map_1(T, Lb, Ub)];
	true ->
	    opt_map_1(T, Lb, Ub)
    end;
opt_map_1(Map, Lb, Ub) ->
    if
	Lb =< Ub ->
	    Map;
	true ->
	    []
    end.

matched_range({get_bits,Bits0,[U|Flags]}) when is_integer(U) ->
    case not lists:member(signed, Flags) andalso is_integer(Bits0) of
	true ->
	    Bits = U*Bits0,
	    {0,(1 bsl Bits) - 1};
	false ->
	    unknown
    end;
matched_range({add,Imm,Add}) ->
    case matched_range(Imm) of
	unknown -> unknown;
	{Lb,Ub} -> {Lb+Add,Ub+Add}
    end;
matched_range(_Op) -> unknown.

string_num_bits(StringType, Constraint, Aligned) ->
    case get_constraint(Constraint, 'PermittedAlphabet') of
	{'SingleValue',Sv} ->
	    charbits(length(Sv), Aligned);
	no ->
	    case StringType of
		'IA5String' ->
		    charbits(128, Aligned);
		'VisibleString' ->
		    charbits(95, Aligned);
		'PrintableString' ->
		    charbits(74, Aligned);
		'NumericString' ->
		    charbits(11, Aligned);
		'UniversalString' ->
		    32;
		'BMPString' ->
		    16
	    end
    end.

charbits(NumChars, false) ->
    uper_num_bits(NumChars);
charbits(NumChars, true) ->
    1 bsl uper_num_bits(uper_num_bits(NumChars)).

convert_string(8, notab, Imm) ->
    {convert,binary_to_list,Imm};
convert_string(NumBits, notab, Imm) when NumBits < 8 ->
    Dec = fun(V, Buf) ->
		  emit(["{",{call,per_common,decode_chars,
			     [V,NumBits]},com,Buf,"}"])
	  end,
    {call,Dec,Imm};
convert_string(NumBits, notab, Imm) when NumBits =:= 16 ->
    Dec = fun(V, Buf) ->
		  emit(["{",{call,per_common,decode_chars_16bit,
			     [V]},com,Buf,"}"])
	  end,
    {call,Dec,Imm};
convert_string(NumBits, notab, Imm) ->
    Dec = fun(V, Buf) ->
		  emit(["{",{call,per_common,decode_big_chars,
			     [V,NumBits]},com,Buf,"}"])
	  end,
    {call,Dec,Imm};
convert_string(NumBits, Chars, Imm) ->
    Dec = fun(V, Buf) ->
		  emit(["{",{call,per_common,decode_chars,
			     [V,NumBits,{asis,Chars}]},com,Buf,"}"])
	  end,
    {call,Dec,Imm}.

char_tab(C, StringType, NumBits) ->
    case get_constraint(C, 'PermittedAlphabet') of
	{'SingleValue',Sv} ->
	    char_tab_1(Sv, NumBits);
	no ->
	    case StringType of
		'IA5String' ->
		    notab;
		'VisibleString' ->
		    notab;
		'PrintableString' ->
		    Chars = " '()+,-./0123456789:=?"
			"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			"abcdefghijklmnopqrstuvwxyz",
		    char_tab_1(Chars, NumBits);
		'NumericString' ->
		    char_tab_1(" 0123456789", NumBits);
		'UniversalString' ->
		    notab;
		'BMPString' ->
		    notab
	    end
    end.

char_tab_1(Chars, NumBits) ->
    Max = lists:max(Chars),
    BitValMax = (1 bsl NumBits) - 1,
    if
	Max =< BitValMax ->
	    notab;
	true ->
	    list_to_tuple(lists:sort(Chars))
    end.

%%%
%%% Remove unnecessary aligning to octet boundaries.
%%%

opt_al({get_bits,E0,Opts0}, A0) ->
    {E,A1} = opt_al(E0, A0),
    Opts = opt_al_1(A1, Opts0),
    A = update_al(A1, E, Opts),
    {{get_bits,E,Opts},A};
opt_al({call,Fun,E0}, A0) ->
    {E,A} = opt_al(E0, A0),
    {{call,Fun,E},A};
opt_al({convert,Op,E0}, A0) ->
    {E,A} = opt_al(E0, A0),
    {{convert,Op,E},A};
opt_al({value,V}=Term, A) when is_integer(V); is_atom(V) ->
    {Term,A};
opt_al({value,E0}, A0) ->
    {E,A} = opt_al(E0, A0),
    {{value,E},A};
opt_al({add,E0,I}, A0) when is_integer(I) ->
    {E,A} = opt_al(E0, A0),
    {{add,E,I},A};
opt_al({test,E0,V,B0}, A0) ->
    {E,A1} = opt_al(E0, A0),
    {B,A2} = opt_al(B0, A1),
    {{test,E,V,B},A2};
opt_al({'case',Cs0}, A0) ->
    {Cs,A} = opt_al_cs(Cs0, A0),
    {{'case',Cs},A};
opt_al({map,E0,Cs}, A0) ->
    {E,A} = opt_al(E0, A0),
    {{map,E,Cs},A};
opt_al(I, A) when is_integer(I) ->
    {I,A}.

opt_al_cs([C0|Cs0], A0) ->
    {C,A1} = opt_al(C0, A0),
    {Cs,A2} = opt_al_cs(Cs0, A0),
    {[C|Cs],merge_al(A1, A2)};
opt_al_cs([], _) -> {[],none}.

merge_al(unknown, _) -> unknown;
merge_al(Other, none) -> Other;
merge_al(_, unknown) -> unknown;
merge_al(I0, I1) ->
    case {I0 rem 8,I1 rem 8} of
	{I,I} -> I;
	{_,_} -> unknown
    end.

opt_al_1(unknown, Opts) ->
    Opts;
opt_al_1(A, Opts0) ->
    case alignment(Opts0) of
	none ->
	    Opts0;
	full ->
	    case A rem 8 of
		0 ->
		    %% Already in alignment.
		    proplists:delete(align, Opts0);
		Bits ->
		    %% Cheaper alignment with a constant padding.
		    Opts1 = proplists:delete(align, Opts0),
		    [{align,8-Bits }|Opts1]
	    end;
	A ->					%Assertion.
	    Opts0
    end.

update_al(A0, E, Opts) ->
    A = case alignment(Opts) of
	    none -> A0;
	    full -> 0;
	    Bits when is_integer(A0) ->
		0  = (A0 + Bits) rem 8;		%Assertion.
	    _ ->
		0
	end,
    [U] = [U || U <- Opts, is_integer(U)],
    if
	U rem 8 =:= 0 -> A;
	is_integer(A), is_integer(E) -> A + U*E;
	true -> unknown
    end.

%%%
%%% Flatten the intermediate format and assign temporaries.
%%%

flatten({get_bits,I,U}, Buf0, St0) when is_integer(I) ->
    {Dst,St} = new_var_pair(St0),
    Gb = {get_bits,{I,Buf0},U,Dst},
    flatten_align(Gb, [], St);
flatten({get_bits,E0,U}, Buf0, St0) ->
    {E,Pre,St1} = flatten(E0, Buf0, St0),
    {Dst,St2} = new_var_pair(St1),
    Gb = {get_bits,E,U,Dst},
    flatten_align(Gb, Pre, St2);
flatten({test,{get_bits,I,U},V,E0}, Buf0, St0) when is_integer(I) ->
    {DstBuf0,St1} = new_var("Buf", St0),
    Gb = {get_bits,{I,Buf0},U,{V,DstBuf0}},
    {{_Dst,DstBuf},Pre0,St2} = flatten_align(Gb, [], St1),
    {E,Pre1,St3} = flatten(E0, DstBuf, St2),
    {E,Pre0++Pre1,St3};
flatten({add,E0,I}, Buf0, St0) ->
    {{Src,Buf},Pre,St1} = flatten(E0, Buf0, St0),
    {Dst,St} = new_var("Add", St1),
    {{Dst,Buf},Pre++[{add,Src,I,Dst}],St};
flatten({'case',Cs0}, Buf0, St0) ->
    {Dst,St1} = new_var_pair(St0),
    {Cs1,St} = flatten_cs(Cs0, Buf0, St1),
    {Al,Cs2} = flatten_hoist_align(Cs1),
    {Dst,Al++[{'case',Buf0,Cs2,Dst}],St};
flatten({map,E0,Cs0}, Buf0, St0) ->
    {{E,DstBuf},Pre,St1} = flatten(E0, Buf0, St0),
    {Dst,St2} = new_var("Int", St1),
    Cs = flatten_map_cs(Cs0, E),
    {{Dst,DstBuf},Pre++[{'map',E,Cs,{Dst,DstBuf}}],St2};
flatten({value,V}, Buf0, St0) when is_atom(V) ->
    {{"'"++atom_to_list(V)++"'",Buf0},[],St0};
flatten({value,V0}, Buf0, St0) when is_integer(V0) ->
    {{V0,Buf0},[],St0};
flatten({value,V0}, Buf0, St0) ->
    flatten(V0, Buf0, St0);
flatten({convert,Op,E0}, Buf0, St0) ->
    {{E,Buf},Pre,St1} = flatten(E0, Buf0, St0),
    {Dst,St2} = new_var("Conv", St1),
    {{Dst,Buf},Pre++[{convert,Op,E,Dst}],St2};
flatten({call,Fun,E0}, Buf0, St0) ->
    {Src,Pre,St1} = flatten(E0, Buf0, St0),
    {Dst,St2} = new_var_pair(St1),
    {Dst,Pre++[{call,Fun,Src,Dst}],St2}.

flatten_cs([C0|Cs0], Buf, St0) ->
    {C,Pre,St1} = flatten(C0, Buf, St0),
    {Cs,St2} = flatten_cs(Cs0, Buf, St0),
    St3 = St2#st{var=max(St1#st.var, St2#st.var)},
    {[Pre++[{return,C}]|Cs],St3};
flatten_cs([], _, St) -> {[],St}.

flatten_map_cs(Cs, Var) ->
    flatten_map_cs_1(Cs, {Var,Cs}).

flatten_map_cs_1([{K,V}|Cs], DefData) ->
    [{{asis,K},{asis,V}}|flatten_map_cs_1(Cs, DefData)];
flatten_map_cs_1([integer_default], {Int,_}) ->
    [{'_',Int}];
flatten_map_cs_1([enum_default], {Int,_}) ->
    [{'_',["{asn1_enum,",Int,"}"]}];
flatten_map_cs_1([enum_error], {Var,Cs}) ->
    Vs = [V || {_,V} <- Cs],
    [{'_',["exit({error,{asn1,{decode_enumerated,{",Var,",",
	   {asis,Vs},"}}}})"]}];
flatten_map_cs_1([], _) -> [].

flatten_hoist_align([[{align_bits,_,_}=Ab|T]|Cs]) ->
    flatten_hoist_align_1(Cs, Ab, [T]);
flatten_hoist_align(Cs) -> {[],Cs}.

flatten_hoist_align_1([[Ab|T]|Cs], Ab, Acc) ->
    flatten_hoist_align_1(Cs, Ab, [T|Acc]);
flatten_hoist_align_1([], Ab, Acc) ->
    {[Ab],lists:reverse(Acc)}.

flatten_align({get_bits,{SrcBits,SrcBuf},U,Dst}=Gb0, Pre, St0) ->
    case alignment(U) of
	none ->
	    flatten_align_1(U, Dst, Pre++[Gb0], St0);
	full ->
	    {PadBits,St1} = new_var("Pad", St0),
	    {DstBuf,St2} = new_var("Buf", St1),
	    Ab = {align_bits,SrcBuf,PadBits},
	    Agb = {get_bits,{PadBits,SrcBuf},[1],{'_',DstBuf}},
	    Gb = {get_bits,{SrcBits,DstBuf},U,Dst},
	    flatten_align_1(U, Dst, Pre++[Ab,Agb,Gb], St2);
	PadBits when is_integer(PadBits), PadBits > 0 ->
	    {DstBuf,St1} = new_var("Buf", St0),
	    Agb = {get_bits,{PadBits,SrcBuf},[1],{'_',DstBuf}},
	    Gb = {get_bits,{SrcBits,DstBuf},U,Dst},
	    flatten_align_1(U, Dst, Pre++[Agb,Gb], St1)
    end.

flatten_align_1(U, {D,_}=Dst, Pre, St) ->
    case is_non_zero(U) of
	false ->
	    {Dst,Pre,St};
	true ->
	    {Dst,Pre++[{non_zero,D}],St}
    end.

new_var_pair(St0) ->
    {Var,St1} = new_var("V", St0),
    {Buf,St2} = new_var("Buf", St1),
    {{Var,Buf},St2}.

new_var(Tag, #st{base=VarBase,var=N}=St) ->
    {VarBase++Tag++integer_to_list(N),St#st{var=N+1}}.

alignment([{align,false}|_]) -> none;
alignment([{align,true}|_]) -> full;
alignment([{align,Bits}|_]) -> Bits;
alignment([_|T]) -> alignment(T);
alignment([]) -> none.

is_non_zero(Fl) ->
    lists:member(non_zero, Fl).

%%%
%%% Generate Erlang code from the flattened intermediate format.
%%%

dcg_list_outside([{align_bits,Buf,SzVar}|T]) ->
    emit([SzVar," = bit_size(",Buf,") band 7"]),
    iter_dcg_list_outside(T);
dcg_list_outside([{'case',Buf,Cs,Dst}|T]) ->
    dcg_case(Buf, Cs, Dst),
    iter_dcg_list_outside(T);
dcg_list_outside([{'map',Val,Cs,Dst}|T]) ->
    dcg_map(Val, Cs, Dst),
    iter_dcg_list_outside(T);
dcg_list_outside([{add,S1,S2,Dst}|T]) ->
    emit([Dst," = ",S1," + ",S2]),
    iter_dcg_list_outside(T);
dcg_list_outside([{return,{V,Buf}}|T]) ->
    emit(["{",V,",",Buf,"}"]),
    iter_dcg_list_outside(T);
dcg_list_outside([{call,Fun,{V,Buf},{Dst,DstBuf}}|T]) ->
    emit(["{",Dst,",",DstBuf,"}  = "]),
    Fun(V, Buf),
    iter_dcg_list_outside(T);
dcg_list_outside([{convert,{M,F},V,Dst}|T]) ->
    emit([Dst," = ",{asis,M},":",{asis,F},"(",V,")"]),
    iter_dcg_list_outside(T);
dcg_list_outside([{convert,Op,V,Dst}|T]) ->
    emit([Dst," = ",Op,"(",V,")"]),
    iter_dcg_list_outside(T);
dcg_list_outside([{get_bits,{_,Buf0},_,_}|_]=L0) ->
    emit("<<"),
    {L,Buf} = dcg_list_inside(L0, buf),
    emit([Buf,"/bitstring>> = ",Buf0]),
    iter_dcg_list_outside(L);
dcg_list_outside([]) ->
    emit("ignore"),
    ok.

iter_dcg_list_outside([_|_]=T) ->
    emit([",",nl]),
    dcg_list_outside(T);
iter_dcg_list_outside([]) -> ok.

dcg_case(Buf, Cs, {Dst,DstBuf}) ->
    emit(["{",Dst,",",DstBuf,"} = case ",Buf," of",nl]),
    dcg_case_cs(Cs),
    emit("end").

dcg_case_cs([C|Cs]) ->
    emit("<<"),
    {T0,DstBuf} = dcg_list_inside(C, buf),
    emit([DstBuf,"/bitstring>>"]),
    T1 = dcg_guard(T0),
    dcg_list_outside(T1),
    case Cs of
	[] -> emit([nl]);
	[_|_] -> emit([";",nl])
    end,
    dcg_case_cs(Cs);
dcg_case_cs([]) -> ok.

dcg_guard([{non_zero,Src}|T]) ->
    emit([" when ",Src," =/= 0 ->",nl]),
    T;
dcg_guard(T) ->
    emit([" ->",nl]),
    T.

dcg_map(Val, Cs, {Dst,_}) ->
    emit([Dst," = case ",Val," of",nl]),
    dcg_map_cs(Cs),
    emit("end").

dcg_map_cs([{K,V}]) ->
    emit([K," -> ",V,nl]);
dcg_map_cs([{K,V}|Cs]) ->
    emit([K," -> ",V,";",nl]),
    dcg_map_cs(Cs).

dcg_list_inside([{get_bits,{Sz,_},Fl0,{Dst,DstBuf}}|T], _) ->
    Fl = bit_flags(Fl0, []),
    emit([mk_dest(Dst),":",Sz,Fl,","]),
    dcg_list_inside(T, DstBuf);
dcg_list_inside(L, Dst) -> {L,Dst}.

bit_flags([{align,_}|T], Acc) ->
    bit_flags(T, Acc);
bit_flags([non_zero|T], Acc) ->
    bit_flags(T, Acc);
bit_flags([U|T], Acc) when is_integer(U) ->
    bit_flags(T, ["unit:"++integer_to_list(U)|Acc]);
bit_flags([H|T], Acc) ->
    bit_flags(T, [atom_to_list(H)|Acc]);
bit_flags([], []) ->
    "";
bit_flags([], Acc) ->
    case "/" ++ bit_flags_1(Acc, "") of
	"/unit:1" -> [];
	Opts -> Opts
    end.


bit_flags_1([H|T], Sep) ->
    Sep ++ H ++ bit_flags_1(T, "-");
bit_flags_1([], _) -> [].

mk_dest(I) when is_integer(I) ->
    integer_to_list(I);
mk_dest(S) -> S.

%%%
%%% Constructing the intermediate format for encoding.
%%%

split_off_nonbuilding(Imm) ->
    lists:splitwith(fun is_nonbuilding/1, Imm).

is_nonbuilding({assign,_,_}) -> true;
is_nonbuilding({call,_,_,_,_}) -> true;
is_nonbuilding({lc,_,_,_,_}) -> true;
is_nonbuilding({set,_,_}) -> true;
is_nonbuilding({list,_,_}) -> true;
is_nonbuilding({sub,_,_,_}) -> true;
is_nonbuilding({'try',_,_,_,_}) -> true;
is_nonbuilding(_) -> false.

mk_vars(Input0, Temps) ->
    asn1ct_name:new(enc),
    Curr = asn1ct_name:curr(enc),
    [H|T] = atom_to_list(Curr),
    Base = [H - ($a - $A)|T ++ "@"],
    case Input0 of
	{var,Name} when is_list(Name) ->
	    {[],[Input0|mk_vars_1(Base, Temps)]};
	[] ->
	    {[],[Input0|mk_vars_1(Base, Temps)]};
	_ when is_integer(Input0) ->
	    {[],[Input0|mk_vars_1(Base, Temps)]}
    end.

mk_vars_1(Base, Vars) ->
    [mk_var(Base, V) || V <- Vars].

mk_var(Base, V) ->
    {var,Base ++ atom_to_list(V)}.

per_enc_integer_1(Val, [], Aligned) ->
    [{'cond',[['_'|per_enc_unconstrained(Val, Aligned)]]}];
per_enc_integer_1(Val, [{{'SingleValue',[_|_]=Svs}=Constr,[]}], Aligned) ->
    %% An extensible constraint such as (1|17, ...).
    %%
    %% A subtle detail is that the extension root as described in the
    %% ASN.1 spec should be used to determine whether a particular value
    %% belongs to the extension root (as opposed to the effective
    %% constraint, which will be used for the actual encoding).
    %%
    %% So for the example above, only the integers 1 and 17 should be
    %% encoded as root values (extension bit = 0).

    [{'ValueRange',{Lb,Ub}}] = effective_constraint(integer, [Constr]),
    Root = [begin
		{[],_,Put} = per_enc_constrained(Sv, Lb, Ub, Aligned),
		[{eq,Val,Sv},{put_bits,0,1,[1]}|Put]
	    end || Sv <- Svs],
    Cs = Root ++ [['_',{put_bits,1,1,[1]}|
		   per_enc_unconstrained(Val, Aligned)]],
    build_cond(Cs);
per_enc_integer_1(Val0, [{{_,_}=Constr,[]}], Aligned) ->
    {Prefix,Check,Action} = per_enc_integer_2(Val0, Constr, Aligned),
    Prefix++build_cond([[Check,{put_bits,0,1,[1]}|Action],
			['_',{put_bits,1,1,[1]}|
			 per_enc_unconstrained(Val0, Aligned)]]);
per_enc_integer_1(Val0, [Constr], Aligned) ->
    {Prefix,Check,Action} = per_enc_integer_2(Val0, Constr, Aligned),
    Prefix++build_cond([[Check|Action],
			['_',{error,Val0}]]).

per_enc_integer_2(Val, {'SingleValue',Sv}, Aligned) when is_integer(Sv) ->
    per_enc_constrained(Val, Sv, Sv, Aligned);
per_enc_integer_2(Val, {'ValueRange',{'MIN',Ub}}, Aligned)
  when is_integer(Ub) ->
    {[],{lt,Val,Ub+1},per_enc_unconstrained(Val, Aligned)};
per_enc_integer_2(Val0, {'ValueRange',{Lb,'MAX'}}, Aligned)
  when is_integer(Lb) ->
    {Prefix,Val} = sub_lb(Val0, Lb),
    {Prefix,{ge,Val,0},per_enc_unsigned(Val, Aligned)};
per_enc_integer_2(Val, {'ValueRange',{Lb,Ub}}, Aligned)
  when is_integer(Lb), is_integer(Ub) ->
    per_enc_constrained(Val, Lb, Ub, Aligned).

per_enc_constrained(Val, Sv, Sv, _Aligned) ->
    {[],{eq,Val,Sv},[]};
per_enc_constrained(Val0, Lb, Ub, false) ->
    {Prefix,Val} = sub_lb(Val0, Lb),
    Range = Ub - Lb + 1,
    NumBits = uper_num_bits(Range),
    Check = {ult,Val,Range},
    Put = [{put_bits,Val,NumBits,[1]}],
    {Prefix,Check,Put};
per_enc_constrained(Val0, Lb, Ub, true) ->
    {Prefix,Val} = sub_lb(Val0, Lb),
    Range = Ub - Lb + 1,
    Check = {ult,Val,Range},
    if
	Range < 256 ->
	    NumBits = per_num_bits(Range),
	    Put = [{put_bits,Val,NumBits,[1]}],
	    {Prefix,Check,Put};
	Range =:= 256 ->
	    NumBits = 8,
	    Put = [{put_bits,Val,NumBits,[1,align]}],
	    {Prefix,Check,Put};
	Range =< 65536 ->
	    Put = [{put_bits,Val,16,[1,align]}],
	    {Prefix,Check,Put};
	true ->
	    RangeOctsLen = byte_size(binary:encode_unsigned(Range - 1)),
	    BitsNeeded = per_num_bits(RangeOctsLen),
	    {Prefix,Check,per_enc_constrained_huge(BitsNeeded, Val)}
    end.

per_enc_constrained_huge(BitsNeeded, {var,VarBase}=Val) ->
    Bin = {var,VarBase++"@bin"},
    BinSize0 = {var,VarBase++"@bin_size0"},
    BinSize = {var,VarBase++"@bin_size"},
    [{call,binary,encode_unsigned,[Val],Bin},
     {call,erlang,byte_size,[Bin],BinSize0},
     {sub,BinSize0,1,BinSize},
     {'cond',[['_',
	       {put_bits,BinSize,BitsNeeded,[1]},
	       {put_bits,Bin,binary,[8,align]}]]}];
per_enc_constrained_huge(BitsNeeded, Val) when is_integer(Val) ->
    Bin = binary:encode_unsigned(Val),
    BinSize = erlang:byte_size(Bin),
    [{put_bits,BinSize-1,BitsNeeded,[1]},
     {put_bits,Val,8*BinSize,[1,align]}].

per_enc_unconstrained(Val, Aligned) ->
    case Aligned of
	false -> [];
	true -> [{put_bits,0,0,[1,align]}]
    end ++ [{call,per_common,encode_unconstrained_number,[Val]}].

per_enc_unsigned(Val, Aligned) ->
    case is_integer(Val) of
	false ->
	    {var,VarBase} = Val,
	    Bin = {var,VarBase++"@bin"},
	    BinSize = {var,VarBase++"@bin_size"},
	    [{call,binary,encode_unsigned,[Val],Bin},
	     {call,erlang,byte_size,[Bin],BinSize}|
	     per_enc_length(Bin, 8, BinSize, Aligned)];
	true ->
	    Bin = binary:encode_unsigned(Val),
	    Len = byte_size(Bin),
	    per_enc_length(Bin, 8, Len, Aligned)
    end.

%% Encode a length field without any constraint.
per_enc_length(Bin, Unit, Len, Aligned) ->
    U = unit(1, Aligned),
    PutBits = put_bits_binary(Bin, Unit, Aligned),
    EncFragmented = {call,per_common,encode_fragmented,[Bin,Unit]},
    Al = case Aligned of
	     false -> [];
	     true -> [{put_bits,0,0,[1,align]}]
	 end,
    build_cond([[{lt,Len,128},
		 {put_bits,Len,8,U},PutBits],
		[{lt,Len,16384},
		 {put_bits,2,2,U},{put_bits,Len,14,[1]},PutBits],
		['_'|Al++[EncFragmented]]]).

per_enc_length(Bin, Unit, Len, no, Aligned, _Type) ->
    per_enc_length(Bin, Unit, Len, Aligned);
per_enc_length(Bin, Unit, Len, {{Lb,Ub},[]}, Aligned, Type) ->
    {Prefix,Check,PutLen} = per_enc_constrained(Len, Lb, Ub, Aligned),
    NoExt = {put_bits,0,1,[1]},
    U = unit(Unit, Aligned, Type, Lb*Unit, Ub*Unit),
    PutBits = [{put_bits,Bin,binary,U}],
    [{'cond',ExtConds0}] = per_enc_length(Bin, Unit, Len, Aligned),
    Ext = {put_bits,1,1,[1]},
    ExtConds = prepend_to_cond(ExtConds0, Ext),
    build_length_cond(Prefix, [[Check,NoExt|PutLen++PutBits]|ExtConds]);
per_enc_length(Bin, Unit, Len, {Lb,Ub}, Aligned, Type)
  when is_integer(Lb) ->
    {Prefix,Check,PutLen} = per_enc_constrained(Len, Lb, Ub, Aligned),
    U = unit(Unit, Aligned, Type, Lb*Unit, Ub*Unit),
    PutBits = [{put_bits,Bin,binary,U}],
    build_length_cond(Prefix, [[Check|PutLen++PutBits]]);
per_enc_length(Bin, Unit0, Len, Sv, Aligned, Type) when is_integer(Sv) ->
    NumBits = Sv*Unit0,
    Unit = case NumBits rem 8 of
	       0 ->
		   %% Help out the alignment optimizer.
		   8;
	       _ ->
		   Unit0
	   end,
    U = unit(Unit, Aligned, Type, NumBits, NumBits),
    Pb = {put_bits,Bin,binary,U},
    [{'cond',[[{eq,Len,Sv},Pb]]}].

enc_length(Len, no, Aligned) ->
    U = unit(1, Aligned),
    build_cond([[{lt,Len,128},
		 {put_bits,Len,8,U}],
		[{lt,Len,16384},
		 {put_bits,2,2,U},{put_bits,Len,14,[1]}]]);
enc_length(Len, {{Lb,Ub},[]}, Aligned) ->
    {Prefix,Check,PutLen} = per_enc_constrained(Len, Lb, Ub, Aligned),
    NoExt = {put_bits,0,1,[1]},
    [{'cond',ExtConds0}] = enc_length(Len, no, Aligned),
    Ext = {put_bits,1,1,[1]},
    ExtConds = prepend_to_cond(ExtConds0, Ext),
    build_length_cond(Prefix, [[Check,NoExt|PutLen]|ExtConds]);
enc_length(Len, {Lb,Ub}, Aligned) when is_integer(Lb) ->
    {Prefix,Check,PutLen} = per_enc_constrained(Len, Lb, Ub, Aligned),
    build_length_cond(Prefix, [[Check|PutLen]]);
enc_length(Len, Sv, _Aligned) when is_integer(Sv) ->
    [{'cond',[[{eq,Len,Sv}]]}].

put_bits_binary(Bin, _Unit, Aligned) when is_binary(Bin) ->
    Sz = byte_size(Bin),
    <<Int:Sz/unit:8>> = Bin,
    {put_bits,Int,8*Sz,unit(1, Aligned)};
put_bits_binary(Bin, Unit, Aligned) ->
    {put_bits,Bin,binary,unit(Unit, Aligned)}.

sub_lb(Val, 0) ->
    {[],Val};
sub_lb({var,Var}=Val0, Lb) ->
    Val = {var,Var++"@sub"},
    {[{sub,Val0,Lb,Val}],Val};
sub_lb(Val, Lb) when is_integer(Val) ->
    {[],Val-Lb}.

build_length_cond([{sub,Var0,Base,Var}]=Prefix, Cs) ->
    %% Non-zero lower bound, such as: SIZE (50..200, ...)
    Prefix++[{'cond',opt_length_nzlb(Cs, {Var0,Var,Base}, 0)}];
build_length_cond([], Cs) ->
    %% Zero lower bound, such as: SIZE (0..200, ...)
    [{'cond',opt_length_zlb(Cs, 0)}].

opt_length_zlb([[{ult,Var,Val}|Actions]|T], Ub) ->
    %% Since the SIZE constraint is zero-based, Var
    %% must be greater than zero, and we can use
    %% the slightly cheaper signed less than operator.
    opt_length_zlb([[{lt,Var,Val}|Actions]|T], Ub);
opt_length_zlb([[{lt,_,Val}|_]=H|T], Ub) ->
    if
	Val =< Ub ->
	    %% A previous test has already matched.
	    opt_length_zlb(T, Ub);
	true ->
	    [H|opt_length_zlb(T, max(Ub, Val))]
    end;
opt_length_zlb([H|T], Ub) ->
    [H|opt_length_zlb(T, Ub)];
opt_length_zlb([], _) -> [].

opt_length_nzlb([[{ult,Var,Val}|_]=H|T], {_,Var,Base}=St, _Ub) ->
    [H|opt_length_nzlb(T, St, Base+Val)];
opt_length_nzlb([[{lt,Var0,Val}|_]=H|T], {Var0,_,_}=St, Ub) ->
    if
	Val =< Ub ->
	    %% A previous test has already matched.
	    opt_length_nzlb(T, St, Ub);
	true ->
	    [H|opt_length_nzlb(T, St, Val)]
    end;
opt_length_nzlb([H|T], St, Ub) ->
    [H|opt_length_nzlb(T, St, Ub)];
opt_length_nzlb([], _, _) -> [].

build_cond(Conds0) ->
    case eval_cond(Conds0, gb_sets:empty()) of
	[['_'|Actions]] ->
	    Actions;
	Conds ->
	    [{'cond',Conds}]
    end.

eval_cond([['_',{'cond',Cs}]], Seen) ->
    eval_cond(Cs, Seen);
eval_cond([[Cond|Actions]=H|T], Seen0) ->
    case gb_sets:is_element(Cond, Seen0) of
	false ->
	    Seen = gb_sets:insert(Cond, Seen0),
	    case eval_cond_1(Cond) of
		false ->
		    eval_cond(T, Seen);
		true ->
		    [['_'|Actions]];
		maybe ->
		    [H|eval_cond(T, Seen)]
	    end;
	true ->
	    eval_cond(T, Seen0)
    end;
eval_cond([], _) -> [].

eval_cond_1({ult,I,N}) when is_integer(I), is_integer(N) ->
    0 =< I andalso I < N;
eval_cond_1({eq,[],[]}) ->
    true;
eval_cond_1({eq,I,N}) when is_integer(I), is_integer(N) ->
    I =:= N;
eval_cond_1({ge,I,N}) when is_integer(I), is_integer(N) ->
    I >= N;
eval_cond_1({lt,I,N}) when is_integer(I), is_integer(N) ->
    I < N;
eval_cond_1(_) -> maybe.

prepend_to_cond([H|T], Code) ->
    [prepend_to_cond_1(H, Code)|prepend_to_cond(T, Code)];
prepend_to_cond([], _) -> [].

prepend_to_cond_1([Check|T], Code) ->
    [Check,Code|T].

enc_char_tab(notab) ->
    notab;
enc_char_tab(Tab0) ->
    Tab1 = tuple_to_list(Tab0),
    First = hd(Tab1),
    Tab = enc_char_tab_1(Tab1, First, 0),
    case lists:member(ill, Tab) of
	false ->
	    {compact_map,{First,tuple_size(Tab0)}};
	true ->
	    {tab,{First-1,list_to_tuple(Tab)}}
    end.

enc_char_tab_1([H|T], H, I) ->
    [I|enc_char_tab_1(T, H+1, I+1)];
enc_char_tab_1([_|_]=T, H, I) ->
    [ill|enc_char_tab_1(T, H+1, I)];
enc_char_tab_1([], _, _) -> [].

enumerated_constraint([_]) ->
    [{'SingleValue',0}];
enumerated_constraint(Root) ->
    [{'ValueRange',{0,length(Root)-1}}].

per_enc_enumerated_root(NNL, Prefix, Val, Constr, Aligned) ->
    per_enc_enumerated_root_1(NNL, Prefix, Val, Constr, Aligned, 0).

per_enc_enumerated_root_1([{H,_}|T], Prefix, Val, Constr, Aligned, N) ->
    [[{eq,Val,H}|Prefix++per_enc_integer_1(N, Constr, Aligned)]|
     per_enc_enumerated_root_1(T, Prefix, Val, Constr, Aligned, N+1)];
per_enc_enumerated_root_1([], _, _, _, _, _) -> [].

per_enc_enumerated_ext(NNL, Val, Aligned) ->
    per_enc_enumerated_ext_1(NNL, Val, Aligned, 0).

per_enc_enumerated_ext_1([{H,_}|T], Val, Aligned, N) ->
    [[{eq,Val,H},{put_bits,1,1,[1]}|per_enc_small_number(N, Aligned)]|
     per_enc_enumerated_ext_1(T, Val, Aligned, N+1)];
per_enc_enumerated_ext_1([], _, _, _) -> [].

per_enc_small_length(Val0, Aligned) ->
    {Sub,Val} = sub_lb(Val0, 1),
    U = unit(1, Aligned),
    Sub ++ build_cond([[{lt,Val,64},{put_bits,Val,7,[1]}],
		       [{lt,Val0,128},{put_bits,1,1,[1]},
			{put_bits,Val0,8,U}],
		       ['_',{put_bits,1,1,[1]},
			{put_bits,2,2,U},{put_bits,Val0,14,[1]}]]).

constr_min_size(no) -> no;
constr_min_size({{Lb,_},[]}) when is_integer(Lb) -> Lb;
constr_min_size({Lb,_}) when is_integer(Lb) -> Lb;
constr_min_size(Sv) when is_integer(Sv) -> Sv.

enc_mod(false) -> uper;
enc_mod(true) -> per.

unit(U, false) -> [U];
unit(U, true) -> [U,align].

unit(U, Aligned, Type, Lb, Ub) ->
    case Aligned andalso is_aligned(Type, Lb, Ub) of
	true -> [U,align];
	false -> [U]
    end.

opt_choice(Imm) ->
    {Pb,T0} = lists:splitwith(fun({put_bits,V,_,_}) when is_integer(V) ->
				      true;
				 (_) ->
				      false
			      end, Imm),
    try
	{Prefix,T} = split_off_nonbuilding(T0),
	Prefix ++ opt_choice_1(T, Pb)
    catch
	throw:impossible ->
	    Imm
    end.

opt_choice_1([{'cond',Cs0}], Pb) ->
    case Cs0 of
	[[C|Act]] ->
	    [{'cond',[[C|Pb++Act]]}];
	[[C|Act],['_',{error,_}]=Error] ->
	    [{'cond',[[C|Pb++Act],Error]}];
	_ ->
	    [{'cond',opt_choice_2(Cs0, Pb)}]
    end;
opt_choice_1(_, _) -> throw(impossible).

opt_choice_2([[C|[{put_bits,_,_,_}|_]=Act]|T], Pb) ->
    [[C|Pb++Act]|opt_choice_2(T, Pb)];
opt_choice_2([[_,{error,_}]=H|T], Pb) ->
    [H|opt_choice_2(T, Pb)];
opt_choice_2([_|_], _) ->
    throw(impossible);
opt_choice_2([], _) -> [].

%%%
%%% Optimize list comprehensions (SEQUENCE OF/SET OF).
%%%

opt_lc([{lc,[{call,erlang,iolist_to_binary,[Var],Bin},
	       {call,erlang,byte_size,[Bin],LenVar},
	       {'cond',[[{eq,LenVar,Len},{put_bits,Bin,_,[_|Align]}]]}],
	   Var,Val}]=Lc, LenImm) ->
    %% Given a sequence of a fixed length string, such as
    %% SEQUENCE OF OCTET STRING (SIZE (4)), attempt to rewrite to
    %% a list comprehension that just checks the size, followed by
    %% a conversion to binary:
    %%
    %%   _ = [if length(Comp) =:= 4; byte_size(Comp) =:= 4 -> [] end ||
    %%           Comp <- Sof],
    %%   [align|iolist_to_binary(Sof)]

    CheckImm = [{'cond',[[{eq,{expr,"length("++mk_val(Var)++")"},Len}],
			 [{eq,{expr,"byte_size("++mk_val(Var)++")"},Len}]]}],
    Al = case Align of
	     [] ->
		 [];
	     [align] ->
		 [{put_bits,0,0,[1|Align]}]
	 end,
    case Al =:= [] orelse
	is_end_aligned(LenImm) orelse
	lb_is_nonzero(LenImm) of
	false ->
	    %% Not possible because an empty SEQUENCE OF would be
	    %% improperly aligned. Example:
	    %%
	    %%    SEQUENCE (SIZE (0..3)) OF ...

	    Lc;
	true ->
	    %% Examples:
	    %%
	    %% SEQUENCE (SIZE (1..4)) OF ...
	    %%   (OK because there must be at least one element)
	    %%
	    %% SEQUENCE OF ...
	    %%   (OK because the length field will force alignment)
	    %%
	    Al ++ [{lc,CheckImm,Var,Val,{var,"_"}},
		   {call,erlang,iolist_to_binary,[Val]}]
    end;
opt_lc([{lc,ElementImm0,V,L}]=Lc, LenImm) ->
    %% Attempt to hoist the alignment, putting after the length
    %% and before the list comprehension:
    %%
    %%   [Length,
    %%    align,
    %%    [Encode(Comp) || Comp <- Sof]]
    %%

    case enc_opt_al_1(ElementImm0, 0) of
	{ElementImm,0} ->
	    case is_end_aligned(LenImm) orelse
		(is_beginning_aligned(ElementImm0) andalso
		 lb_is_nonzero(LenImm)) of
		false ->
		    %% Examples:
		    %%
		    %% SEQUENCE (SIZE (0..3)) OF OCTET STRING
		    %%   (An empty SEQUENCE OF would be improperly aligned)
		    %%
		    %% SEQUENCE (SIZE (1..3)) OF OCTET STRING (SIZE (0..4))
		    %%   (There would be an improper alignment before the
		    %%   first element)

		    Lc;
		true ->
		    %% Examples:
		    %%
		    %% SEQUENCE OF INTEGER
		    %% SEQUENCE (SIZE (1..4)) OF INTEGER
		    %% SEQUENCE (SIZE (1..4)) OF INTEGER (0..256)

		    [{put_bits,0,0,[1,align]},{lc,ElementImm,V,L}]
	    end;
	_ ->
	    %% Unknown alignment, no alignment, or not aligned at the end.
	    %% Examples:
	    %%
	    %% SEQUENCE OF SomeConstructedType
	    %% SEQUENCE OF INTEGER (0..15)

	    Lc
    end.

is_beginning_aligned([{'cond',Cs}]) ->
    lists:all(fun([_|Act]) -> is_beginning_aligned(Act) end, Cs);
is_beginning_aligned([{error,_}|_]) -> true;
is_beginning_aligned([{put_bits,_,_,U}|_]) ->
    case U of
	[_,align] -> true;
	[_] -> false
    end;
is_beginning_aligned(Imm0) ->
    case split_off_nonbuilding(Imm0) of
	{[],_} -> false;
	{[_|_],Imm} -> is_beginning_aligned(Imm)
    end.

is_end_aligned(Imm) ->
    case enc_opt_al_1(Imm, unknown) of
	{_,0} -> true;
	{_,_} -> false
    end.

lb_is_nonzero([{sub,_,_,_}|_]) -> true;
lb_is_nonzero(_) -> false.

%%%
%%% Attempt to combine two chunks of intermediate code.
%%%

combine_imms(ImmA0, ImmB0) ->
    {Prefix0,ImmA} = split_off_nonbuilding(ImmA0),
    {Prefix1,ImmB} = split_off_nonbuilding(ImmB0),
    Prefix = Prefix0 ++ Prefix1,
    Combined = do_combine(ImmA ++ ImmB, 3.0),
    Prefix ++ Combined.

do_combine([{error,_}=Imm|_], _Budget) ->
    [Imm];
do_combine([{'cond',Cs0}|T], Budget0) ->
    Budget = debit(Budget0, num_clauses(Cs0, 0)),
    Cs = [[C|do_combine(Act++T, Budget)] || [C|Act] <- Cs0],
    [{'cond',Cs}];
do_combine([{put_bits,V,_,_}|_]=L, Budget) when is_integer(V) ->
    {Pb,T} = collect_put_bits(L),
    do_combine_put_bits(Pb, T,Budget);
do_combine(_, _) ->
    throw(impossible).

do_combine_put_bits(Pb, [], _Budget) ->
    Pb;
do_combine_put_bits(Pb, [{'cond',Cs0}|T], Budget) ->
    Cs = [case Act of
	      [{error,_}] ->
		  [C|Act];
	      _ ->
		  [C|do_combine(Pb++Act, Budget)]
	  end || [C|Act] <- Cs0],
    do_combine([{'cond',Cs}|T], Budget);
do_combine_put_bits(_, _, _) ->
    throw(impossible).

debit(Budget0, Alternatives) ->
    case Budget0 - math:log2(Alternatives) of
	Budget when Budget > 0.0 ->
	    Budget;
	_ ->
	    throw(impossible)
    end.

num_clauses([[_,{error,_}]|T], N) ->
    num_clauses(T, N);
num_clauses([_|T], N) ->
    num_clauses(T, N+1);
num_clauses([], N) -> N.


collect_put_bits(Imm) ->
    lists:splitwith(fun({put_bits,V,_,_}) when is_integer(V) -> true;
		       (_) -> false
		    end, Imm).

%%%
%%% Simple common subexpression elimination to avoid fetching
%%% the same element twice.
%%%

enc_cse([{call,erlang,element,Args,V}=H|T]) ->
    [H|enc_cse_1(T, Args, V)];
enc_cse(Imm) -> Imm.

enc_cse_1([{call,erlang,element,Args,Dst}|T], Args, V) ->
    [{set,V,Dst}|enc_cse_1(T, Args, V)];
enc_cse_1([{block,Bl}|T], Args, V) ->
    [{block,enc_cse_1(Bl, Args, V)}|enc_cse_1(T, Args, V)];
enc_cse_1([H|T], Args, V) ->
    [H|enc_cse_1(T, Args, V)];
enc_cse_1([], _, _) -> [].


%%%
%%% Pre-process the intermediate code to simplify code generation.
%%%

enc_pre_cg(Imm) ->
    enc_pre_cg_1(Imm, outside_list, in_seq).

enc_pre_cg_1([], _StL, _StB) ->
    nil;
enc_pre_cg_1([H], StL, StB) ->
    enc_pre_cg_2(H, StL, StB);
enc_pre_cg_1([H0|T0], StL, StB) ->
    case is_nonbuilding(H0) of
	true ->
	    H = enc_pre_cg_nonbuilding(H0, StL),
	    Seq = {seq,H,enc_pre_cg_1(T0, StL, in_seq)},
	    case StB of
		outside_seq -> {block,Seq};
		in_seq -> Seq
	    end;
	false ->
	    H = enc_pre_cg_2(H0, in_head, outside_seq),
	    T = enc_pre_cg_1(T0, in_tail, outside_seq),
	    enc_make_cons(H, T)
    end.

enc_pre_cg_2(align, StL, _StB) ->
    case StL of
	in_head -> align;
	in_tail -> {cons,align,nil}
    end;
enc_pre_cg_2({apply,_,_}=Imm, _, _) ->
    Imm;
enc_pre_cg_2({block,Bl0}, StL, StB) ->
    enc_pre_cg_1(Bl0, StL, StB);
enc_pre_cg_2({call,_,_,_}=Imm, _, _) ->
    Imm;
enc_pre_cg_2({call_gen,_,_,_,_,_}=Imm, _, _) ->
    Imm;
enc_pre_cg_2({'cond',Cs0}, StL, _StB) ->
    Cs = [{C,enc_pre_cg_1(Act, StL, outside_seq)} || [C|Act] <- Cs0],
    {'cond',Cs};
enc_pre_cg_2({error,_}=E, _, _) ->
    E;
enc_pre_cg_2({lc,B0,V,L}, StL, _StB) ->
    B = enc_pre_cg_1(B0, StL, outside_seq),
    {lc,B,V,L};
enc_pre_cg_2({put_bits,V,8,[1]}, StL, _StB) ->
    case StL of
	in_head -> {integer,V};
	in_tail -> {cons,{integer,V},nil};
	outside_list -> {cons,{integer,V},nil}
    end;
enc_pre_cg_2({put_bits,V,binary,_}, _StL, _StB) ->
    V;
enc_pre_cg_2({put_bits,_,_,[_]}=PutBits, _StL, _StB) ->
    {binary,[PutBits]};
enc_pre_cg_2({var,_}=Imm, _, _) -> Imm.

enc_make_cons({binary,H}, {binary,T}) ->
    {binary,H++T};
enc_make_cons({binary,H0}, {cons,{binary,H1},T}) ->
    enc_make_cons({binary,H0++H1}, T);
enc_make_cons({binary,H}, {cons,{integer,Int},T}) ->
    enc_make_cons({binary,H++[{put_bits,Int,8,[1]}]}, T);
enc_make_cons({integer,Int}, {binary,T}) ->
    {binary,[{put_bits,Int,8,[1]}|T]};
enc_make_cons({integer,Int}, {cons,{binary,H},T}) ->
    enc_make_cons({binary,[{put_bits,Int,8,[1]}|H]}, T);
enc_make_cons(H, T) ->
    {cons,H,T}.

enc_pre_cg_nonbuilding({lc,B0,Var,List,Dst}, StL) ->
    B = enc_pre_cg_1(B0, StL, outside_seq),
    {lc,B,Var,List,Dst};
enc_pre_cg_nonbuilding({list,List0,Dst}, _StL) ->
    List = enc_pre_cg_1(List0, outside_list, outside_seq),
    {list,List,Dst};
enc_pre_cg_nonbuilding({'try',Try0,{P,Succ0},Else0,Dst}, StL) ->
    Try = enc_pre_cg_1(Try0, StL, outside_seq),
    Succ = enc_pre_cg_1(Succ0, StL, outside_seq),
    Else = enc_pre_cg_1(Else0, StL, outside_seq),
    {'try',Try,{P,Succ},Else,Dst};
enc_pre_cg_nonbuilding(Imm, _) -> Imm.

%%%
%%% Optimize calls to complete/1 and surrounding code. There are
%%% several opportunities for optimizations.
%%%
%%% It may be possible to replace the call to complete/1 with
%%% something cheaper (most important for the PER back-end which has
%%% an expensive complete/1 implementation). If we can be sure that
%%% complete/1 will be called with an iolist (no 'align' atoms or
%%% bitstrings in the list), we can call iolist_to_binary/1
%%% instead. If the list may include bitstrings, we can can call
%%% list_to_bitstring/1 (note that list_to_bitstring/1 does not accept
%%% a binary or bitstring, so we MUST be sure that we only pass it a
%%% list).  If complete/1 is called with a binary, we can omit the
%%% call altogether.
%%%
%%% A call to byte_size/1 that follows complete/1 can be eliminated
%%% if the size of the binary produced by complete/1 can be determined
%%% and is constant.
%%%
%%% The code that encodes the length descriptor (a 'cond' instruction)
%%% for a binary produced by complete/1 can be simplified if the lower
%%% and upper bounds for the size of the binary are known.
%%%

-record(ost,
	{sym,
	 t
	}).

enc_opt(Imm0) ->
    {Imm,_} = enc_opt(Imm0, #ost{sym=gb_trees:empty()}),
    Imm.

enc_opt(align, St) ->
    {align,St#ost{t=t_align({0,7})}};
enc_opt({apply,What,As}, St) ->
    {{apply,What,subst_list(As, St)},St#ost{t=t_any()}};
enc_opt({assign,_,_}=Imm, St) ->
    {Imm,St};
enc_opt({binary,PutBits0}, St) ->
    PutBits = [{put_bits,subst(V, St),Sz,F} ||
		  {put_bits,V,Sz,F} <- PutBits0],
    NumBits = lists:foldl(fun({put_bits,_,Bits,_}, Sum) ->
				  Sum+Bits
			  end, 0, PutBits),
    {{binary,PutBits},St#ost{t=t_bitstring(NumBits)}};
enc_opt({block,Bl0}, St0) ->
    {Bl,St} = enc_opt(Bl0, St0),
    {{block,Bl},St};
enc_opt({call,binary,encode_unsigned,[Int],Bin}=Imm, St0) ->
    Type = get_type(Int, St0),
    St = case t_range(Type) of
	     any ->
		 set_type(Bin, t_binary(), St0);
	     {Lb0,Ub0} ->
		 Lb = bit_size(binary:encode_unsigned(Lb0)),
		 Ub = bit_size(binary:encode_unsigned(Ub0)),
		 set_type(Bin, t_binary({Lb,Ub}), St0)
	 end,
    {Imm,St};
enc_opt({call,erlang,bit_size,[Bin],Dst}=Imm0, St0) ->
    Type = get_type(Bin, St0),
    case t_range(Type) of
	any ->
	    St1 = set_type(Bin, t_bitstring(), St0),
	    St = propagate(Dst,
			   fun(T, S) ->
				   bit_size_propagate(Bin, T, S)
			   end, St1),
	    {Imm0,St};
	{Lb,Ub}=Range ->
	    St = set_type(Dst, t_integer(Range), St0),
	    Imm = case Lb of
		      Ub -> none;
		      _ -> Imm0
		  end,
	    {Imm,St}
    end;
enc_opt({call,erlang,byte_size,[Bin],Dst}=Imm0, St0) ->
    Type = get_type(Bin, St0),
    case t_range(Type) of
	any ->
	    St1 = set_type(Bin, t_binary(), St0),
	    St = propagate(Dst,
			   fun(T, S) ->
				   byte_size_propagate(Bin, T, S)
			   end, St1),
	    {Imm0,St};
	{Lb0,Ub0} ->
	    Lb = (Lb0+7) div 8,
	    Ub = (Ub0+7) div 8,
	    St = set_type(Dst, t_integer({Lb,Ub}), St0),
	    Imm = case Lb of
		      Ub -> none;
		      _ -> Imm0
		  end,
	    {Imm,St}
    end;
enc_opt({call,erlang,iolist_to_binary,_}=Imm, St) ->
    {Imm,St#ost{t=t_binary()}};
enc_opt({call,erlang,length,[List],Dst}=Imm0, St0) ->
    St1 = propagate(Dst,
		    fun(T, S) ->
			    length_propagate(List, T, S)
		    end, St0),
    {Imm0,St1};
enc_opt({call,per,complete,[Data],Dst}, St0) ->
    Type = get_type(Data, St0),
    St = set_type(Dst, t_binary(t_range(Type)), St0),
    case t_type(Type) of
	binary ->
	    {{set,Data,Dst},St};
	bitlist ->
	    %% We KNOW that list_to_bitstring/1 will construct
	    %% a binary (the number of bits is divisible by 8)
	    %% because per_enc_open_type/2 added an 'align' atom
	    %% at the end. If that 'align' atom had not been
	    %% optimized away, the type would have been 'align'
	    %% instead of 'bitlist'.
	    {{call,erlang,list_to_bitstring,[Data],Dst},St};
	iolist ->
	    {{call,erlang,iolist_to_binary,[Data],Dst},St};
	nil ->
	    Imm = {list,{binary,[{put_bits,0,8,[1]}]},Dst},
	    enc_opt(Imm, St0);
	_ ->
	    {{call,per,complete,[Data],Dst},St}
    end;
enc_opt({call,uper,complete,[Data],Dst}, St0) ->
    Type = get_type(Data, St0),
    St = set_type(Dst, t_binary(t_range(Type)), St0),
    case t_type(Type) of
	binary ->
	    {{set,Data,Dst},St0};
	iolist ->
	    {{call,erlang,iolist_to_binary,[Data],Dst},St};
	nil ->
	    Imm = {list,{binary,[{put_bits,0,8,[1]}]},Dst},
	    enc_opt(Imm, St0);
	_ ->
	    %% 'bitlist' or 'any'.
	    {{call,uper,complete,[Data],Dst},St}
    end;
enc_opt({call,per_common,encode_chars,[List,NumBits|_],Dst}=Imm, St0) ->
    %% Note: Never used when NumBits =:= 8 (list_to_binary/1 will
    %% be used instead).
    St1 = set_type(Dst, t_bitstring(), St0),
    St = propagate(List,
		   fun(T, S) ->
			   char_propagate(Dst, T, NumBits, S)
		   end, St1),
    {Imm,St};
enc_opt({call,per_common,encode_chars_16bit,[List],Dst}=Imm, St0) ->
    St1 = set_type(Dst, t_binary(), St0),
    St = propagate(List,
		   fun(T, S) ->
			   char_propagate(Dst, T, 16, S)
		   end, St1),
    {Imm,St};
enc_opt({call,per_common,encode_big_chars,[List],Dst}=Imm, St0) ->
    St1 = set_type(Dst, t_binary(), St0),
    St = propagate(List,
		   fun(T, S) ->
			   char_propagate(Dst, T, 32, S)
		   end, St1),
    {Imm,St};
enc_opt({call,per_common,encode_fragmented,[_,Unit]}=Imm, St) ->
    T = case Unit rem 8 of
	    0 -> t_iolist();
	    _ -> t_bitlist()
	end,
    {Imm,St#ost{t=T}};
enc_opt({call,per_common,encode_unconstrained_number,_}=Imm, St) ->
    {Imm,St#ost{t=t_iolist()}};
enc_opt({call,per_common,bitstring_from_positions,_}=Imm, St) ->
    {Imm,St#ost{t=t_bitstring()}};
enc_opt({call,per_common,to_named_bitstring,_}=Imm, St) ->
    {Imm,St#ost{t=t_bitstring()}};
enc_opt({call,_,_,_}=Imm, St) ->
    {Imm,St#ost{t=t_any()}};
enc_opt({call,_,_,_,_}=Imm, St) ->
    {Imm,St#ost{t=undefined}};
enc_opt({call_gen,N,K,F,L,As}, St) ->
    {{call_gen,N,K,F,L,subst(As, St)},St#ost{t=t_any()}};
enc_opt({'cond',Cs0}, St0) ->
    case enc_opt_cs(Cs0, St0) of
	[{'_',Imm,Type}] ->
	    {Imm,St0#ost{t=Type}};
	[{Cond,Imm,Type0}|Cs1] ->
	    {Cs,Type} = enc_opt_cond_1(Cs1, Type0, [{Cond,Imm}]),
	    {{'cond',Cs},St0#ost{t=Type}}
    end;
enc_opt({cons,H0,T0}, St0) ->
    {H,#ost{t=TypeH}=St1} = enc_opt(H0, St0),
    {T,#ost{t=TypeT}=St} = enc_opt(T0, St1),
    {{cons,H,T},St#ost{t=t_cons(TypeH, TypeT)}};
enc_opt({error,_}=Imm, St) ->
    {Imm,St#ost{t=t_any()}};
enc_opt({integer,V}, St) ->
    {{integer,subst(V, St)},St#ost{t=t_integer()}};
enc_opt({lc,E0,B,C}, St) ->
    {E,_} = enc_opt(E0, St),
    {{lc,E,B,C},St#ost{t=t_any()}};
enc_opt({lc,E0,B,C,Dst}, St) ->
    {E,_} = enc_opt(E0, St),
    {{lc,E,B,C,Dst},St#ost{t=undefined}};
enc_opt({list,Imm0,Dst}, St0) ->
    {Imm,#ost{t=Type}=St1} = enc_opt(Imm0, St0),
    St = set_type(Dst, Type, St1),
    {{list,Imm,Dst},St#ost{t=undefined}};
enc_opt(nil, St) ->
    {nil,St#ost{t=t_nil()}};
enc_opt({seq,H0,T0}, St0) ->
    {H,St1} = enc_opt(H0, St0),
    {T,St} = enc_opt(T0, St1),
    {enc_opt_seq(H, T),St};
enc_opt({set,_,_}=Imm, St) ->
    {Imm,St#ost{t=undefined}};
enc_opt({sub,Src0,Int,Dst}, St0) ->
    Src = subst(Src0, St0),
    Type = get_type(Src, St0),
    St = case t_range(Type) of
	     any ->
		 propagate(Dst,
			   fun(T, S) ->
				   set_type(Src, t_add(T, Int), S)
			   end,
			   St0);
	     {Lb,Ub} ->
		 set_type(Dst, t_integer({Lb-Int,Ub-Int}), St0)
	 end,
    {{sub,Src,Int,Dst},St#ost{t=undefined}};
enc_opt({'try',Try0,{P,Succ0},Else0,Dst}, St0) ->
    {Try,_} = enc_opt(Try0, St0),
    {Succ,_} = enc_opt(Succ0, St0),
    {Else,_} = enc_opt(Else0, St0),
    {{'try',Try,{P,Succ},Else,Dst},St0#ost{t=undefined}};
enc_opt({var,_}=Imm, St) ->
    Type = get_type(Imm, St),
    {subst(Imm, St),St#ost{t=Type}}.

remove_trailing_align({block,Bl}) ->
    {block,remove_trailing_align(Bl)};
remove_trailing_align({cons,H,{cons,align,nil}}) ->
    H;
remove_trailing_align({seq,H,T}) ->
    {seq,H,remove_trailing_align(T)};
remove_trailing_align(Imm) -> Imm.

enc_opt_seq(none, T) ->
    T;
enc_opt_seq({list,Imm,Data}, {seq,{call,per,complete,[Data],_},_}=T) ->
    %% Get rid of any explicit 'align' added by per_enc_open_type/2.
    {seq,{list,remove_trailing_align(Imm),Data},T};
enc_opt_seq({call,_,_,_,{var,_}=Dst}=H, T) ->
    case is_var_unused(Dst, T) of
	false -> {seq,H,T};
	true -> T
    end;
enc_opt_seq(H, T) ->
    {seq,H,T}.

is_var_unused(_, align) ->
    true;
is_var_unused(V, {call,_,_,Args}) ->
    not lists:member(V, Args);
is_var_unused(V, {cons,H,T}) ->
    is_var_unused(V, H) andalso is_var_unused(V, T);
is_var_unused(_, _) ->
    false.

bit_size_propagate(Bin, Type, St) ->
    case t_range(Type) of
	any ->
	    St;
	{Lb,Ub} ->
	    set_type(Bin, t_bitstring({Lb,Ub}), St)
    end.

byte_size_propagate(Bin, Type, St) ->
    case t_range(Type) of
	any ->
	    St;
	{Lb,Ub} ->
	    set_type(Bin, t_binary({Lb*8,Ub*8}), St)
    end.

char_propagate(Dst, T, NumBits, St) ->
    case t_range(T) of
	any ->
	    St;
	{Sz,Sz} when Sz*NumBits rem 8 =:= 0 ->
	    Bits = Sz*NumBits,
	    set_type(Dst, t_binary({Bits,Bits}), St);
	{Lb,Ub} ->
	    Range = {Lb*NumBits,Ub*NumBits},
	    case NumBits rem 8 of
		0 ->
		    set_type(Dst, t_binary(Range), St);
		_ ->
		    set_type(Dst, t_bitstring(Range), St)
	    end
    end.

length_propagate(List, Type, St) ->
    set_type(List, t_list(t_range(Type)), St).

enc_opt_cond_1([{Cond,{error,_}=Imm,_}|T], St, Acc) ->
    enc_opt_cond_1(T, St, [{Cond,Imm}|Acc]);
enc_opt_cond_1([{Cond,Imm,Curr0}|T], Curr1, Acc) ->
    Curr = t_join(Curr0, Curr1),
    enc_opt_cond_1(T, Curr, [{Cond,Imm}|Acc]);
enc_opt_cond_1([], St, Acc) ->
    {lists:reverse(Acc),St}.

enc_opt_cs([{Cond,Imm0}|T], St0) ->
    case eo_eval_cond(Cond, St0) of
	false ->
	    enc_opt_cs(T, St0);
	true ->
	    {Imm,#ost{t=Type}} = enc_opt(Imm0, St0),
	    [{'_',Imm,Type}];
	maybe ->
	    St = update_type_info(Cond, St0),
	    {Imm,#ost{t=Type}} = enc_opt(Imm0, St),
	    [{Cond,Imm,Type}|enc_opt_cs(T, St0)]
    end;
enc_opt_cs([], _) -> [].

eo_eval_cond('_', _) ->
    true;
eo_eval_cond({Op,{var,_}=Var,Val}, St) ->
    Type = get_type(Var, St),
    case t_range(Type) of
	any -> maybe;
	{_,_}=Range -> eval_cond_range(Op, Range, Val)
    end;
eo_eval_cond({_Op,{expr,_},_Val}, _St) -> maybe.

eval_cond_range(lt, {Lb,Ub}, Val) ->
    if
	Ub < Val -> true;
	Val =< Lb -> false;
	true -> maybe
    end;
eval_cond_range(_Op, _Range, _Val) -> maybe.

update_type_info({ult,{var,_}=Var,Val}, St) ->
    Int = t_integer({0,Val-1}),
    Type = t_meet(get_type(Var, St), Int),
    set_type(Var, Type, St);
update_type_info({lt,{var,_}=Var,Val}, St) ->
    Int = t_integer({0,Val-1}),
    Type = t_meet(get_type(Var, St), Int),
    set_type(Var, Type, St);
update_type_info({eq,{var,_}=Var,Val}, St) when is_integer(Val) ->
    Int = t_integer(Val),
    Type = t_meet(get_type(Var, St), Int),
    set_type(Var, Type, St);
update_type_info({eq,_,_}, St) ->
    St;
update_type_info({ge,_,_}, St) -> St.

subst_list(As, St) ->
    [subst(A, St) || A <- As].

subst({var,_}=Var, St) ->
    Type = get_type(Var, St),
    case t_type(Type) of
	integer ->
	    case t_range(Type) of
		any -> Var;
		{Val,Val} -> Val;
		{_,_} -> Var
	    end;
	_ ->
	    Var
    end;
subst(V, _St) -> V.

set_type({var,Var}, {_,_}=Type, #ost{sym=Sym0}=St0) ->
    Sym1 = gb_trees:enter(Var, Type, Sym0),
    case gb_trees:lookup({propagate,Var}, Sym1) of
	none ->
	    St0#ost{sym=Sym1};
	{value,Propagate} ->
	    Sym = gb_trees:delete({propagate,Var}, Sym1),
	    St = St0#ost{sym=Sym},
	    Propagate(Type, St)
    end.

get_type({var,V}, #ost{sym=Sym}) ->
    case gb_trees:lookup(V, Sym) of
	none -> t_any();
	{value,T} -> T
    end.

propagate({var,Var}, Propagate, #ost{sym=Sym0}=St) when is_function(Propagate, 2) ->
    Sym = gb_trees:enter({propagate,Var}, Propagate, Sym0),
    St#ost{sym=Sym}.

%%%
%%% A simple type system.
%%%
%%% Each type descriptions is a tuple {Type,Range}.
%%% Type is one of the following atoms:
%%%
%%% Type name   Description
%%% ---------   -----------
%%% any         Anything.
%%%
%%% align       Basically iodata, but the list may contain bitstrings
%%%             and the the atom 'align'. Can be passed to complete/1
%%%             to construct a binary. Only used for aligned PER (per).
%%%
%%% bitstring   An Erlang bitstring.
%%%
%%% bitlist     A list that may be passed to list_to_bitstring/1 to
%%%             construct a bitstring.
%%%             NOTE: When analysing aligned PER (per), the number
%%%             of bits in the bitlist is always divisible by 8 (if
%%%             not, the type will be 'align' instead).
%%%
%%% binary      An Erlang binary (the number of bits is divisible by 8).
%%%
%%% iolist      An Erlang iolist.
%%%
%%% nil         []
%%%
%%% integer     An integer.
%%%
%%%
%%% Range is one of:
%%%
%%%     any
%%%     {LowerBound,UpperBound}
%%%
%%%

t_align(Range) ->
    {align,t__range(Range)}.

t_any() ->
    {any,any}.

t_binary() ->
    {binary,any}.

t_binary(Range) ->
    {binary,t__range(Range)}.

t_bitlist() ->
    {bitlist,any}.

t_bitstring() ->
    {bitstring,any}.

t_bitstring(Range0) ->
    case t__range(Range0) of
	{Bits,Bits}=Range when Bits rem 8 =:= 0 ->
	    {binary,Range};
	Range ->
	    {bitstring,Range}
    end.

t_add({integer,{Lb,Ub}}, N) ->
    {integer,{Lb+N,Ub+N}}.

t_cons({_,_}=T1, {_,_}=T2) ->
    T = case {t__cons_type(T1),t__cons_type(T2)} of
	    {_,any} -> any;
	    {any,_} -> any;
	    {align,_} -> align;
	    {_,align} -> align;
	    {binary,binary} -> iolist;
	    {binary,bitstring} -> bitlist;
	    {bitstring,binary} -> bitlist;
	    {bitstring,bitstring} -> bitlist
	end,
    {T,t__cons_ranges(t__cons_range(T1), t__cons_range(T2))}.

t_integer() ->
    {integer,any}.

t_integer(Range) ->
    {integer,t__range(Range)}.

t_iolist() ->
    {iolist,any}.

t_list(Range) ->
    {list,t__range(Range)}.

t_nil() ->
    {nil,{0,0}}.

t_meet({T1,Range1}, {T2,Range2}) ->
    {t_meet_types(T1, T2),t_meet_ranges(Range1, Range2)}.

t_meet_types(integer, integer) -> integer;
t_meet_types(any, integer) -> integer.

t_meet_ranges(any, Range) ->
    Range;
t_meet_ranges({Lb1,Ub1}, {Lb2,Ub2}) ->
    if
	Lb1 =< Ub2, Lb2 =< Ub1 ->
	    {max(Lb1, Lb2),Ub1};
	Lb2 =< Ub1, Lb1 =< Ub2 ->
	    {max(Lb1, Lb2),Ub2}
    end.

t_join({T1,Range1}, {T2,Range2}) ->
    T = t_join_types(lists:sort([T1,T2])),
    Range = t_join_ranges(Range1, Range2),
    {T,Range}.

t_join_ranges({Lb1,Ub1}, {Lb2,Ub2}) ->
    {min(Lb1, Lb2),max(Ub1, Ub2)};
t_join_ranges(any, _) -> any;
t_join_ranges(_, any) -> any.

t_join_types([T,T]) -> T;
t_join_types([align,any]) -> any;
t_join_types([align,_]) -> align;
t_join_types([any,_]) -> any;
t_join_types([bitlist,bitstring]) -> any;
t_join_types([bitlist,integer]) -> any;
t_join_types([bitlist,iolist]) -> bitlist;
t_join_types([bitlist,nil]) -> bitlist;
t_join_types([binary,bitlist]) -> bitlist;
t_join_types([binary,bitstring]) -> bitstring;
t_join_types([binary,integer]) -> binary;
t_join_types([binary,iolist]) -> iolist;
t_join_types([binary,nil]) -> iolist;
t_join_types([bitstring,integer]) -> any;
t_join_types([bitstring,iolist]) -> any;
t_join_types([bitstring,nil]) -> any;
t_join_types([integer,_]) -> any;
t_join_types([iolist,nil]) -> iolist.

t_type({T,_}) -> T.

t_range({_,Range}) -> Range.

t__cons_type({align,_}) -> align;
t__cons_type({any,_}) -> any;
t__cons_type({binary,_}) -> binary;
t__cons_type({bitstring,_}) -> bitstring;
t__cons_type({bitlist,_}) -> bitstring;
t__cons_type({integer,_}) -> binary;
t__cons_type({iolist,_}) -> binary;
t__cons_type({nil,_}) -> binary.

t__cons_range({integer,_}) -> {8,8};
t__cons_range({_,Range}) -> Range.

t__cons_ranges({Lb1,Ub1}, {Lb2,Ub2}) ->
    {Lb1+Lb2,Ub1+Ub2};
t__cons_ranges(any, _) -> any;
t__cons_ranges(_, any) -> any.

t__range({Lb,Ub}=Range) when is_integer(Lb), is_integer(Ub) ->
    Range;
t__range(any) ->
    any;
t__range(Val) when is_integer(Val) ->
    {Val,Val}.


%%%
%%% Code generation for encoding.
%%%

enc_cg({cons,_,_}=Cons) ->
    enc_cg_cons(Cons);
enc_cg({block,Imm}) ->
    emit(["begin",nl]),
    enc_cg(Imm),
    emit([nl,
	  "end"]);
enc_cg({seq,First,Then}) ->
    enc_cg(First),
    emit([com,nl]),
    enc_cg(Then);
enc_cg(align) ->
    emit(align);
enc_cg({apply,F0,As0}) ->
    As = enc_call_args(As0, ""),
    case F0 of
	{local,F,_} when is_atom(F) ->
	    emit([{asis,F},"(",As,")"]);
	{M,F,_} ->
	    emit([{asis,M},":",{asis,F},"(",As,")"])
    end;
enc_cg({assign,Dst0,Expr}) ->
    Dst = mk_val(Dst0),
    emit([Dst," = ",Expr]);
enc_cg({binary,PutBits}) ->
    emit(["<<",enc_cg_put_bits(PutBits, ""),">>"]);
enc_cg({call,M,F,As0}) ->
    As = [mk_val(A) || A <- As0],
    asn1ct_func:call(M, F, As);
enc_cg({call,M,F,As0,Dst}) ->
    As = [mk_val(A) || A <- As0],
    emit([mk_val(Dst)," = "]),
    asn1ct_func:call(M, F, As);
enc_cg({call_gen,Prefix,Key,Gen,_,As0}) ->
    As = [mk_val(A) || A <- As0],
    asn1ct_func:call_gen(Prefix, Key, Gen, As);
enc_cg({'cond',Cs}) ->
    enc_cg_cond(Cs);
enc_cg({error,Error}) when is_function(Error, 0) ->
    Error();
enc_cg({error,Var0}) ->
    Var = mk_val(Var0),
    emit(["exit({error,{asn1,{illegal_value,",Var,"}}})"]);
enc_cg({integer,Int}) ->
    emit(mk_val(Int));
enc_cg({lc,Body,Var,List}) ->
    emit("["),
    enc_cg(Body),
    emit([" || ",mk_val(Var)," <- ",mk_val(List),"]"]);
enc_cg({lc,Body,Var,List,Dst}) ->
    emit([mk_val(Dst)," = ["]),
    enc_cg(Body),
    emit([" || ",mk_val(Var)," <- ",mk_val(List),"]"]);
enc_cg({list,List,Dst}) ->
    emit([mk_val(Dst)," = "]),
    enc_cg(List);
enc_cg(nil) ->
    emit("[]");
enc_cg({sub,Src0,Int,Dst0}) ->
    Src = mk_val(Src0),
    Dst = mk_val(Dst0),
    emit([Dst," = ",Src," - ",Int]);
enc_cg({set,{var,Src},{var,Dst}}) ->
    emit([Dst," = ",Src]);
enc_cg({'try',Try,{P,Succ},Else,Dst}) ->
    emit([mk_val(Dst)," = try "]),
    enc_cg(Try),
    emit([" of",nl,
	  mk_val(P)," ->",nl]),
    enc_cg(Succ),
    emit([nl,
	  "catch throw:invalid ->",nl]),
    enc_cg(Else),
    emit([nl,
	  "end"]);
enc_cg({var,V}) ->
    emit(V).

enc_cg_cons(Cons) ->
    emit("["),
    enc_cg_cons_1(Cons),
    emit("]").

enc_cg_cons_1({cons,H,{cons,_,_}=T}) ->
    enc_cg(H),
    emit([com,nl]),
    enc_cg_cons_1(T);
enc_cg_cons_1({cons,H,nil}) ->
    enc_cg(H);
enc_cg_cons_1({cons,H,T}) ->
    enc_cg(H),
    emit("|"),
    enc_cg(T).

enc_call_args([A|As], Sep) ->
    [Sep,mk_val(A)|enc_call_args(As, ", ")];
enc_call_args([], _) -> [].

enc_cg_cond(Cs) ->
    emit("if "),
    enc_cg_cond(Cs, ""),
    emit([nl,
	  "end"]).

enc_cg_cond([C|Cs], Sep) ->
    emit(Sep),
    enc_cg_cond_1(C),
    enc_cg_cond(Cs, [";",nl]);
enc_cg_cond([], _) -> ok.

enc_cg_cond_1({Cond,Action}) ->
    enc_cond_term(Cond),
    emit([" ->",nl]),
    enc_cg(Action).

enc_cond_term('_') ->
    emit("true");
enc_cond_term({ult,Var0,Int}) ->
    Var = mk_val(Var0),
    N = uper_num_bits(Int),
    case 1 bsl N of
	Int ->
	    emit([Var," bsr ",N," =:= 0"]);
	_ ->
	    emit(["0 =< ",Var,", ",Var," < ",Int])
    end;
enc_cond_term({eq,Var0,Term}) ->
    Var = mk_val(Var0),
    emit([Var," =:= ",{asis,Term}]);
enc_cond_term({ge,Var0,Int}) ->
    Var = mk_val(Var0),
    emit([Var," >= ",Int]);
enc_cond_term({lt,Var0,Int}) ->
    Var = mk_val(Var0),
    emit([Var," < ",Int]).

enc_cg_put_bits([{put_bits,Val0,N,[1]}|T], Sep) ->
    Val = mk_val(Val0),
    [[Sep,Val,":",integer_to_list(N)]|enc_cg_put_bits(T, ",")];
enc_cg_put_bits([], _) -> [].

mk_val({var,Str}) -> Str;
mk_val({expr,Str}) -> Str;
mk_val(Int) when is_integer(Int) -> integer_to_list(Int);
mk_val(Other) -> {asis,Other}.

%%%
%%% Generate a function that maps a name of a bit position
%%%  to the bit position.
%%%

bit_string_name2pos_fun(NNL, Src) ->
    {call_gen,"bit_string_name2pos_",NNL,
     fun(Fd, Name) -> gen_name2pos(Fd, Name, NNL) end,[],[Src]}.

gen_name2pos(Fd, Name, Names) ->
    Cs0 = gen_name2pos_cs(Names, Name),
    Cs = Cs0 ++ [bit_clause(Name),nil_clause(),invalid_clause()],
    F0 = {function,1,Name,1,Cs},
    F = erl_parse:new_anno(F0),
    file:write(Fd, [erl_pp:function(F)]).

gen_name2pos_cs([{K,V}|T], Name) ->
    P = [{cons,0,{atom,0,K},{var,0,'T'}}],
    B = [{cons,0,{integer,0,V},{call,0,{atom,0,Name},[{var,0,'T'}]}}],
    [{clause,0,P,[],B}|gen_name2pos_cs(T, Name)];
gen_name2pos_cs([], _) -> [].

bit_clause(Name) ->
    VarT = {var,0,'T'},
    VarPos = {var,0,'Pos'},
    P = [{cons,0,{tuple,0,[{atom,0,bit},VarPos]},VarT}],
    G = [[{call,0,{atom,0,is_integer},[VarPos]}]],
    B = [{cons,0,VarPos,{call,0,{atom,0,Name},[VarT]}}],
    {clause,0,P,G,B}.

nil_clause() ->
    P = B = [{nil,0}],
    {clause,0,P,[],B}.

invalid_clause() ->
    P = [{var,0,'_'}],
    B = [{call,0,{atom,0,throw},[{atom,0,invalid}]}],
    {clause,0,P,[],B}.

%%%
%%% Hoist alignment to reduce the number of list elements in
%%% encode. Fewer lists elements means faster traversal in
%%% complete/{2,3}.
%%%
%%% For example, the following data sequence:
%%%
%%%   [align,<<1:1,0:1>>,[align,<<Len:16>>|Data]]
%%%
%%% can be rewritten to:
%%%
%%%   [align,<<1:1,0:1,0:6>>,[<<Len:16>>|Data]]
%%%
%%% The change from the literal <<1:1,0:1>> to <<1:1,0:1,0:6>>
%%% comes for free, and we have eliminated one element of the
%%% sub list.
%%%
%%% We must be careful not to rewrite:
%%%
%%%   [<<1:1,0:1>>,[align,<<Len:16>>|Data]]
%%%
%%% to:
%%%
%%%   [[<<1:1,0:1>>,align],[<<Len:16>>|Data]]
%%%
%%% because even though [<<1:0,0:1>>,align] is a literal and does
%%% not add any additional construction cost, there is one more
%%% sub list that needs to be traversed.
%%%

enc_hoist_align(Imm0) ->
    Imm = enc_hoist_align_reverse(Imm0, []),
    enc_hoist_align(Imm, false, []).

enc_hoist_align_reverse([H|T], Acc) ->
    case enc_opt_al_1([H], 0) of
	{[H],_} ->
	    enc_hoist_align_reverse(T, [H|Acc]);
	{_,_} ->
	    lists:reverse(T, [H,stop|Acc])
    end;
enc_hoist_align_reverse([], Acc) -> Acc.

enc_hoist_align([stop|T], _Aligned, Acc) ->
    lists:reverse(T, Acc);
enc_hoist_align([{block,Bl0}|T], Aligned, Acc) ->
    Bl = case Aligned of
	     false -> Bl0;
	     true -> enc_hoist_block(Bl0)
	 end,
    case is_beginning_aligned(Bl) of
	false ->
	    enc_hoist_align(T, false, [{block,Bl}|Acc]);
	true ->
	    enc_hoist_align(T, true, [{put_bits,0,0,[1,align]},
				      {block,Bl}|Acc])
    end;
enc_hoist_align([H|T], _, Acc) ->
    enc_hoist_align(T, false, [H|Acc]);
enc_hoist_align([], _, Acc) -> Acc.

enc_hoist_block(Bl) ->
    try
	enc_hoist_block_1(lists:reverse(Bl))
    catch
	throw:impossible ->
	    Bl
    end.

enc_hoist_block_1([{'cond',Cs0}|T]) ->
    Cs = [[C|enc_hoist_block_2(Act)] || [C|Act] <- Cs0],
    H = {'cond',Cs},
    lists:reverse(T, [H]);
enc_hoist_block_1(_) ->
    throw(impossible).

enc_hoist_block_2([{'cond',_}|_]=L) ->
    enc_hoist_block(L);
enc_hoist_block_2([{error,_}]=L) ->
    L;
enc_hoist_block_2([]) ->
    [{put_bits,0,0,[1,align]}];
enc_hoist_block_2(L) ->
    case lists:last(L) of
	{put_bits,_,_,_} ->
	    L ++ [{put_bits,0,0,[1,align]}];
	_ ->
	    throw(impossible)
    end.

%%%
%%% Optimize alignment for encoding.
%%%

enc_opt_al(Imm0) ->
    {Imm,_} = enc_opt_al_1(Imm0, unknown),
    Imm.

enc_opt_al_1([H0|T0], Al0) ->
    {H,Al1} = enc_opt_al(H0, Al0),
    {T,Al} = enc_opt_al_1(T0, Al1),
    {H++T,Al};
enc_opt_al_1([], Al) -> {[],Al}.

enc_opt_al({assign,_,_}=Imm, Al) ->
    {[Imm],Al};
enc_opt_al({block,Bl0}, Al0) ->
    {Bl,Al} = enc_opt_al_1(Bl0, Al0),
    {[{block,Bl}],Al};
enc_opt_al({call,erlang,iolist_to_binary,[_]}=Imm, Al) ->
    {[Imm],Al};
enc_opt_al({call,per_common,encode_fragmented,[_,U]}=Call, Al) ->
    case U rem 8 of
	0 -> {[Call],Al};
	_ -> {[Call],unknown}
    end;
enc_opt_al({call,per_common,encode_unconstrained_number,[_]}=Call, _) ->
    {[Call],0};
enc_opt_al({call,_,_,_,_}=Call, Al) ->
    {[Call],Al};
enc_opt_al({'cond',Cs0}, Al0) ->
    {Cs,Al} = enc_opt_al_cond(Cs0, Al0),
    {[{'cond',Cs}],Al};
enc_opt_al({error,_}=Imm, Al) ->
    {[Imm],Al};
enc_opt_al({list,Imm0,Dst}, Al) ->
    Imm1 = enc_opt_hoist_align(Imm0),
    {Imm,_} = enc_opt_al_1(Imm1, 0),
    {[{list,Imm,Dst}],Al};
enc_opt_al({put_bits,V,N,[U,align]}, Al0) when Al0 rem 8 =:= 0 ->
    Al = if
	     is_integer(N) -> N*U;
	     N =:= binary, U rem 8 =:= 0 -> 0;
	     true -> unknown
	 end,
    {[{put_bits,V,N,[U]}],Al};
enc_opt_al({put_bits,V,binary,[U,align]}, Al0) when is_integer(Al0) ->
    N = 8 - (Al0 rem 8),
    Al = case U rem 8 of
	     0 -> 0;
	     _ -> unknown
	 end,
    {[{put_bits,0,N,[1]},{put_bits,V,binary,[U]}],Al};
enc_opt_al({put_bits,V,N0,[U,align]}, Al0) when is_integer(N0), is_integer(Al0) ->
    N = N0 + (8 - Al0 rem 8),
    Al = N0*U,
    {[{put_bits,V,N,[1]}],Al};
enc_opt_al({put_bits,_,N,[U,align]}=PutBits, _) when is_integer(N) ->
    {[PutBits],N*U};
enc_opt_al({put_bits,_,binary,[U,align]}=PutBits, _) when U rem 8 =:= 0 ->
    {[PutBits],0};
enc_opt_al({put_bits,_,N,[U]}=PutBits, Al) when is_integer(N), is_integer(Al) ->
    {[PutBits],Al+N*U};
enc_opt_al({put_bits,_,binary,[U]}=PutBits, Al) when U rem 8 =:= 0 ->
    {[PutBits],Al};
enc_opt_al({set,_,_}=Imm, Al) ->
    {[Imm],Al};
enc_opt_al({sub,_,_,_}=Imm, Al) ->
    {[Imm],Al};
enc_opt_al({'try',_,_,_,_}=Imm, Al) ->
    {[Imm],Al};
enc_opt_al(Imm, _) ->
    {[Imm],unknown}.

enc_opt_al_cond(Cs0, Al0) ->
    enc_opt_al_cond_1(Cs0, Al0, [], []).

enc_opt_al_cond_1([['_',{error,_}]=C|Cs], Al, CAcc, AAcc) ->
    enc_opt_al_cond_1(Cs, Al, [C|CAcc], AAcc);
enc_opt_al_cond_1([[C|Act0]|Cs0], Al0, CAcc, AAcc) ->
    {Act,Al1} = enc_opt_al_1(Act0, Al0),
    Al = if
	     Al1 =:= unknown -> Al1;
	     true -> Al1 rem 8
	 end,
    enc_opt_al_cond_1(Cs0, Al0, [[C|Act]|CAcc], [Al|AAcc]);
enc_opt_al_cond_1([], _, CAcc, AAcc) ->
    Al = case lists:usort(AAcc) of
	     [] -> unknown;
	     [Al0] -> Al0;
	     [_|_] -> unknown
	 end,
    {lists:reverse(CAcc),Al}.

enc_opt_hoist_align([{'cond',Cs0},{put_bits,0,0,[1,align]}]=Imm) ->
    try
	Cs = [insert_align_last(C) || C <- Cs0],
	[{'cond',Cs}]
    catch
	throw:impossible ->
	    Imm
    end;
enc_opt_hoist_align(Imm) -> Imm.

insert_align_last([_,{error,_}]=C) ->
    C;
insert_align_last([H|T]) ->
    case lists:last(T) of
	{put_bits,_,_,_} ->
	    [H|T ++ [{put_bits,0,0,[1,align]}]];
	_ ->
	    throw(impossible)
    end.

%%%
%%% For the aligned PER format, fix up the intermediate format
%%% before code generation. Code generation will be somewhat
%%% easier if 'align' appear as a separate instruction.
%%%

per_fixup([{apply,_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{block,Block}|T]) ->
    [{block,per_fixup(Block)}|per_fixup(T)];
per_fixup([{'assign',_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{'cond',Cs0}|T]) ->
    Cs = [[C|per_fixup(Act)] || [C|Act] <- Cs0],
    [{'cond',Cs}|per_fixup(T)];
per_fixup([{call,_,_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{call,_,_,_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{call_gen,_,_,_,_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{error,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{lc,B,V,L}|T]) ->
    [{lc,per_fixup(B),V,L}|per_fixup(T)];
per_fixup([{lc,B,V,L,Dst}|T]) ->
    [{lc,per_fixup(B),V,L,Dst}|per_fixup(T)];
per_fixup([{list,Imm,Dst}|T]) ->
    [{list,per_fixup(Imm),Dst}|per_fixup(T)];
per_fixup([{set,_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{sub,_,_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{'try',Try0,{P,Succ0},Else0,Dst}|T]) ->
    Try = per_fixup(Try0),
    Succ = per_fixup(Succ0),
    Else = per_fixup(Else0),
    [{'try',Try,{P,Succ},Else,Dst}|per_fixup(T)];
per_fixup([{put_bits,_,_,_}|_]=L) ->
    fixup_put_bits(L);
per_fixup([{var,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([]) -> [].

fixup_put_bits([{put_bits,0,0,[_,align]}|T]) ->
    [align|fixup_put_bits(T)];
fixup_put_bits([{put_bits,0,0,_}|T]) ->
    fixup_put_bits(T);
fixup_put_bits([{put_bits,V,N,[U,align]}|T]) ->
    [align,{put_bits,V,N,[U]}|fixup_put_bits(T)];
fixup_put_bits([{put_bits,_,_,_}=H|T]) ->
    [H|fixup_put_bits(T)];
fixup_put_bits(Other) -> per_fixup(Other).

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
effective_constraint(integer, [{{_,_}=Root,_}|_Rest]) ->
    %% Normalize extension. Note that any range given for the
    %% extension should be ignored anyway.
    [{Root,[]}];
effective_constraint(integer, C) ->
    SVs = get_constraints(C, 'SingleValue'),
    SV = effective_constr('SingleValue', SVs),
    VRs = get_constraints(C, 'ValueRange'),
    VR = effective_constr('ValueRange', VRs),
    greatest_common_range(SV, VR);
effective_constraint(bitstring, C) ->
    case get_constraint(C, 'SizeConstraint') of
	{{Lb,Ub},[]}=Range when is_integer(Lb) ->
	    if
		is_integer(Ub), Ub < 16#10000 ->
		    Range;
		true ->
		    no
	    end;
	{Lb,Ub}=Range when is_integer(Lb) ->
	    if
		is_integer(Ub), Ub < 16#10000 ->
		    if
			Lb =:= Ub -> Lb;
			true -> Range
		    end;
		true ->
		    no
	    end;
	no ->
	    no
    end.

effective_constr(_, []) -> [];
effective_constr('SingleValue', List) ->
    SVList = lists:flatten(lists:map(fun(X) -> element(2, X) end, List)),
    %% Sort and remove duplicates before generating SingleValue or ValueRange
    %% In case of ValueRange, also check for 'MIN and 'MAX'
    case lists:usort(SVList) of
	[N] ->
	    [{'SingleValue',N}];
	[_|_]=L ->
	    [{'ValueRange',{least_Lb(L),greatest_Ub(L)}}]
    end;
effective_constr('ValueRange', List) ->
    LBs = lists:map(fun({_,{Lb,_}}) -> Lb end, List),
    UBs = lists:map(fun({_,{_,Ub}}) -> Ub end, List),
    Lb = least_Lb(LBs),
    [{'ValueRange',{Lb,lists:max(UBs)}}].

greatest_common_range([], VR) ->
    VR;
greatest_common_range(SV, []) ->
    SV;
greatest_common_range([{_,Int}], [{_,{'MIN',Ub}}])
  when is_integer(Int), Int > Ub ->
    [{'ValueRange',{'MIN',Int}}];
greatest_common_range([{_,Int}],[{_,{Lb,Ub}}])
  when is_integer(Int), Int < Lb ->
    [{'ValueRange',{Int,Ub}}];
greatest_common_range([{_,Int}],VR=[{_,{_Lb,_Ub}}]) when is_integer(Int) ->
    VR;
greatest_common_range([{_,L}],[{_,{Lb,Ub}}]) when is_list(L) ->
    Min = least_Lb([Lb|L]),
    Max = greatest_Ub([Ub|L]),
    [{'ValueRange',{Min,Max}}];
greatest_common_range([{_,{Lb1,Ub1}}], [{_,{Lb2,Ub2}}]) ->
    Min = least_Lb([Lb1,Lb2]),
    Max = greatest_Ub([Ub1,Ub2]),
    [{'ValueRange',{Min,Max}}].


least_Lb(L) ->
    case lists:member('MIN', L) of
	true -> 'MIN';
	false -> lists:min(L)
    end.

greatest_Ub(L) ->
    case lists:member('MAX', L) of
	true -> 'MAX';
	false -> lists:max(L)
    end.

get_constraint(C, Key) ->
    case lists:keyfind(Key, 1, C) of
	false -> no;
	{_,V} -> V
    end.

get_constraints([{Key,_}=Pair|T], Key) ->
    [Pair|get_constraints(T, Key)];
get_constraints([_|T], Key) ->
    get_constraints(T, Key);
get_constraints([], _) -> [].
