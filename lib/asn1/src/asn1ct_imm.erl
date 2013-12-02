%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2013. All Rights Reserved.
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
-module(asn1ct_imm).
-export([per_dec_raw_bitstring/2,
	 per_dec_boolean/0,per_dec_enumerated/2,per_dec_enumerated/3,
	 per_dec_extension_map/1,
	 per_dec_integer/2,per_dec_k_m_string/3,
	 per_dec_length/3,per_dec_named_integer/3,
	 per_dec_octet_string/2,per_dec_open_type/1,per_dec_real/1,
	 per_dec_restricted_string/1]).
-export([per_dec_constrained/3,per_dec_normally_small_number/1]).
-export([per_enc_bit_string/4,per_enc_boolean/2,
	 per_enc_choice/3,per_enc_enumerated/3,
	 per_enc_integer/3,per_enc_integer/4,
	 per_enc_null/2,
	 per_enc_k_m_string/4,per_enc_octet_string/3,
	 per_enc_open_type/2,
	 per_enc_restricted_string/3,
	 per_enc_small_number/2]).
-export([per_enc_extension_bit/2,per_enc_extensions/4,per_enc_optional/3]).
-export([per_enc_sof/5]).
-export([enc_absent/3,enc_append/1,enc_bind_var/1]).
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
    EnumTail = case matched_range(Int) of
		   {0,Ub} ->
		       %% The error case can never happen.
		       [];
		   _ ->
		       [enum_error]
	       end,
    NamedList = per_dec_enumerated_fix_list(NamedList0, EnumTail, 0),
    {map,Int,NamedList}.

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
    {map,Int,NamedList}.

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

per_enc_bit_string(Val0, [], Constraint0, Aligned) ->
    {B,[Val,Bs,Bits]} = mk_vars(Val0, [bs,bits]),
    Constraint = effective_constraint(bitstring, Constraint0),
    ExtraArgs = case constr_min_size(Constraint) of
		    no -> [];
		    Lb -> [Lb]
		end,
    B ++ [{call,per_common,to_bitstring,[Val|ExtraArgs],Bs},
	  {call,erlang,bit_size,[Bs],Bits}|
	  per_enc_length(Bs, 1, Bits, Constraint, Aligned, 'BIT STRING')];
per_enc_bit_string(Val0, NNL0, Constraint0, Aligned) ->
    {B,[Val,Bs,Bits,Positions]} = mk_vars(Val0, [bs,bits,positions]),
    NNL = lists:keysort(2, NNL0),
    Constraint = effective_constraint(bitstring, Constraint0),
    ExtraArgs = case constr_min_size(Constraint) of
		    no -> [];
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
    Args = case enc_char_tab(Chars0) of
	       notab -> [Val,Unit];
	       Chars -> [Val,Unit,Chars]
	   end,
    Enc = case Unit of
	      16 ->
		  {call,per_common,encode_chars_16bit,[Val],Bin};
	      32 ->
		  {call,per_common,encode_big_chars,[Val],Bin};
	      8 ->
		  {call,erlang,list_to_binary,[Val],Bin};
	      _ ->
		  {call,per_common,encode_chars,Args,Bin}
	  end,
    case Unit of
	8 ->
	    B ++ [Enc,{call,erlang,byte_size,[Bin],Len}];
	_ ->
	    B ++ [{call,erlang,length,[Val],Len},Enc]
    end ++ per_enc_length(Bin, Unit, Len, SzConstraint, Aligned, k_m_string).

per_enc_open_type([], Aligned) ->
    [{put_bits,1,8,unit(1, Aligned)},{put_bits,0,8,[1]}];
per_enc_open_type([{'cond',
		    [['_',
		      {put_bits,0,0,_},
		      {call,per_common,encode_unconstrained_number,_}=Call]]}],
		  Aligned) ->
    %% We KNOW that encode_unconstrained_number/1 will return an IO list;
    %% therefore the call to complete/1 can be replaced with a cheaper
    %% call to iolist_to_binary/1.
    {Dst,Imm} = per_enc_open_type_output([Call], []),
    ToBin = {erlang,iolist_to_binary},
    Imm ++ per_enc_open_type(Dst, ToBin, Aligned);
per_enc_open_type([{call,erlang,iolist_to_binary,Args}], Aligned) ->
    {_,[_,Bin,Len]} = mk_vars('dummy', [bin,len]),
    [{call,erlang,iolist_to_binary,Args,Bin},
     {call,erlang,byte_size,[Bin],Len}|per_enc_length(Bin, 8, Len, Aligned)];
per_enc_open_type(Imm0, Aligned) ->
    try
	{Prefix,Imm1} = split_off_nonbuilding(Imm0),
	Prefix ++ enc_open_type(Imm1, Aligned)
    catch
	throw:impossible ->
	    {Dst,Imm} = per_enc_open_type_output(Imm0, []),
	    ToBin = {enc_mod(Aligned),complete},
	    Imm ++ per_enc_open_type(Dst, ToBin, Aligned)
    end.

per_enc_octet_string(Val0, Constraint0, Aligned) ->
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
	{'cond',[[{eq,Bitmap,0}],
		 ['_'|Length ++ PutBits]],{var,"Extensions"}}].

per_enc_optional(Val0, {Pos,DefVals}, _Aligned) when is_integer(Pos),
						     is_list(DefVals) ->
    Val1 = lists:concat(["element(",Pos,", ",Val0,")"]),
    {B,[Val]} = mk_vars(Val1, []),
    Zero = {put_bits,0,1,[1]},
    One = {put_bits,1,1,[1]},
    B++[{'cond',
	 [[{eq,Val,DefVal},Zero] || DefVal <- DefVals] ++ [['_',One]]}];
per_enc_optional(Val0, {Pos,{call,M,F,A}}, _Aligned) when is_integer(Pos) ->
    Val1 = lists:concat(["element(",Pos,", ",Val0,")"]),
    {B,[Val,Tmp]} = mk_vars(Val1, [tmp]),
    Zero = {put_bits,0,1,[1]},
    One = {put_bits,1,1,[1]},
    B++[{call,M,F,[Val|A],Tmp},
	{'cond',
	 [[{eq,Tmp,true},Zero],['_',One]]}];
per_enc_optional(Val0, Pos, _Aligned) when is_integer(Pos) ->
    Val1 = lists:concat(["element(",Pos,", ",Val0,")"]),
    {B,[Val]} = mk_vars(Val1, []),
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

enc_bind_var(Val) ->
    {B,[{var,Var}]} = mk_vars(Val, []),
    {B,list_to_atom(Var)}.

enc_cg(Imm0, false) ->
    Imm1 = enc_cse(Imm0),
    Imm = enc_pre_cg(Imm1),
    enc_cg(Imm);
enc_cg(Imm0, true) ->
    Imm1 = enc_cse(Imm0),
    Imm2 = enc_hoist_align(Imm1),
    Imm3 = enc_opt_al(Imm2),
    Imm4 = per_fixup(Imm3),
    Imm = enc_pre_cg(Imm4),
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

matched_range({get_bits,Bits0,[U|Flags]}) when is_integer(U) ->
    case lists:member(signed, Flags) of
	false ->
	    Bits = U*Bits0,
	    {0,(1 bsl Bits) - 1};
	true ->
	    unknown
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

is_nonbuilding({apply,_,_,_}) -> true;
is_nonbuilding({assign,_,_}) -> true;
is_nonbuilding({call,_,_,_,_}) -> true;
is_nonbuilding({'cond',_,_}) -> true;
is_nonbuilding({lc,_,_,_,_}) -> true;
is_nonbuilding({sub,_,_,_}) -> true;
is_nonbuilding({'try',_,_,_,_}) -> true;
is_nonbuilding(_) -> false.

mk_vars(Input0, Temps) ->
    asn1ct_name:new(enc),
    Curr = asn1ct_name:curr(enc),
    [H|T] = atom_to_list(Curr),
    Base = [H - ($a - $A)|T ++ "@"],
    if
	is_atom(Input0) ->
	    Input = {var,atom_to_list(Input0)},
	    {[],[Input|mk_vars_1(Base, Temps)]};
	is_integer(Input0) ->
	    {[],[Input0|mk_vars_1(Base, Temps)]};
	Input0 =:= [] ->
	    {[],[Input0|mk_vars_1(Base, Temps)]};
	true ->
	    Input = mk_var(Base, input),
	    {[{assign,Input,Input0}],[Input|mk_vars_1(Base, Temps)]}
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
    if
	Range < 256 ->
	    NumBits = per_num_bits(Range),
	    Check = {ult,Val,Range},
	    Put = [{put_bits,Val,NumBits,[1]}],
	    {Prefix,Check,Put};
	Range =:= 256 ->
	    NumBits = 8,
	    Check = {ult,Val,Range},
	    Put = [{put_bits,Val,NumBits,[1,align]}],
	    {Prefix,Check,Put};
	Range =< 65536 ->
	    Check = {ult,Val,Range},
	    Put = [{put_bits,Val,16,[1,align]}],
	    {Prefix,Check,Put};
	true ->
	    {var,VarBase} = Val,
	    Bin = {var,VarBase++"@bin"},
	    BinSize0 = {var,VarBase++"@bin_size0"},
	    BinSize = {var,VarBase++"@bin_size"},
	    Check = {ult,Val,Range},
	    RangeOctsLen = byte_size(binary:encode_unsigned(Range - 1)),
	    BitsNeeded = per_num_bits(RangeOctsLen),
	    Enc = [{call,binary,encode_unsigned,[Val],Bin},
		   {call,erlang,byte_size,[Bin],BinSize0},
		   {sub,BinSize0,1,BinSize},
		   {'cond',[['_',
			     {put_bits,BinSize,BitsNeeded,[1]},
			     {put_bits,Bin,binary,[8,align]}]]}],
	    {Prefix,Check,Enc}
    end.

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
per_enc_length(Bin, Unit, Len, Sv, Aligned, Type) when is_integer(Sv) ->
    NumBits = Sv*Unit,
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
    Tab = tuple_to_list(Tab0),
    First = hd(Tab),
    {First-1,list_to_tuple(enc_char_tab_1(Tab, First, 0))}.

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
%%% Helper functions for code generation of open types.
%%%

per_enc_open_type(Val0, {ToBinMod,ToBinFunc}, Aligned) ->
    {B,[Val,Len,Bin]} = mk_vars(Val0, [len,bin]),
    B ++ [{call,ToBinMod,ToBinFunc,[Val],Bin},
	  {call,erlang,byte_size,[Bin],Len}|
	  per_enc_length(Bin, 8, Len, Aligned)].

enc_open_type([{'cond',Cs}], Aligned) ->
    [{'cond',[[C|enc_open_type_1(Act, Aligned)] || [C|Act] <- Cs]}];
enc_open_type(_, _) ->
    throw(impossible).

enc_open_type_1([{error,_}]=Imm, _) ->
    Imm;
enc_open_type_1(Imm, Aligned) ->
    NumBits = num_bits(Imm, 0),
    Pad = case 8 - (NumBits rem 8) of
	      8 -> [];
	      Pad0 -> [{put_bits,0,Pad0,[1]}]
	  end,
    NumBytes = (NumBits+7) div 8,
    enc_length(NumBytes, no, Aligned) ++ Imm ++ Pad.

num_bits([{put_bits,_,N,[U|_]}|T], Sum) when is_integer(N) ->
    num_bits(T, Sum+N*U);
num_bits([_|_], _) ->
    throw(impossible);
num_bits([], Sum) -> Sum.

per_enc_open_type_output([{apply,F,A}], Acc) ->
    Dst = output_var(),
    {Dst,lists:reverse(Acc, [{apply,F,A,{var,atom_to_list(Dst)}}])};
per_enc_open_type_output([{call,M,F,A}], Acc) ->
    Dst = output_var(),
    {Dst,lists:reverse(Acc, [{call,M,F,A,{var,atom_to_list(Dst)}}])};
per_enc_open_type_output([{'cond',Cs}], Acc) ->
    Dst = output_var(),
    {Dst,lists:reverse(Acc, [{'cond',Cs,{var,atom_to_list(Dst)}}])};
per_enc_open_type_output([H|T], Acc) ->
    per_enc_open_type_output(T, [H|Acc]).

output_var() ->
    asn1ct_name:new(enc),
    Curr = asn1ct_name:curr(enc),
    [H|T] = atom_to_list(Curr),
    list_to_atom([H - ($a - $A)|T ++ "@output"]).


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
    case Budget0 - log2(Alternatives) of
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

log2(N) ->
    math:log(N) / math:log(2.0).

collect_put_bits(Imm) ->
    lists:splitwith(fun({put_bits,V,_,_}) when is_integer(V) -> true;
		       (_) -> false
		    end, Imm).

%%%
%%% Simple common subexpression elimination to avoid fetching
%%% the same element twice.
%%%

enc_cse([{assign,{var,V},E}=H|T]) ->
    [H|enc_cse_1(T, E, V)];
enc_cse(Imm) -> Imm.

enc_cse_1([{assign,Dst,E}|T], E, V) ->
    [{assign,Dst,V}|enc_cse_1(T, E, V)];
enc_cse_1([{block,Bl}|T], E, V) ->
    [{block,enc_cse_1(Bl, E, V)}|enc_cse_1(T, E, V)];
enc_cse_1([H|T], E, V) ->
    [H|enc_cse_1(T, E, V)];
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
enc_pre_cg_2({call_gen,_,_,_,_}=Imm, _, _) ->
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
    {cons,{binary,H0++H1},T};
enc_make_cons({integer,Int}, {binary,T}) ->
    {binary,[{put_bits,Int,8,[1]}|T]};
enc_make_cons(H, T) ->
    {cons,H,T}.

enc_pre_cg_nonbuilding({'cond',Cs0,Dst}, StL) ->
    Cs = [{C,enc_pre_cg_1(Act, StL, outside_seq)} || [C|Act] <- Cs0],
    {'cond',Cs,Dst};
enc_pre_cg_nonbuilding({lc,B0,Var,List,Dst}, StL) ->
    B = enc_pre_cg_1(B0, StL, outside_seq),
    {lc,B,Var,List,Dst};
enc_pre_cg_nonbuilding({'try',Try0,{P,Succ0},Else0,Dst}, StL) ->
    Try = enc_pre_cg_1(Try0, StL, outside_seq),
    Succ = enc_pre_cg_1(Succ0, StL, outside_seq),
    Else = enc_pre_cg_1(Else0, StL, outside_seq),
    {'try',Try,{P,Succ},Else,Dst};
enc_pre_cg_nonbuilding(Imm, _) -> Imm.


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
	{M,F} ->
	    emit([{asis,M},":",{asis,F},"(",As,")"]);
	F when is_atom(F) ->
	    emit([{asis,F},"(",As,")"])
    end;
enc_cg({apply,F0,As0,Dst}) ->
    As = enc_call_args(As0, ""),
    emit([mk_val(Dst)," = "]),
    case F0 of
	{M,F} ->
	    emit([{asis,M},":",{asis,F},"(",As,")"]);
	F when is_atom(F) ->
	    emit([{asis,F},"(",As,")"])
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
enc_cg({call_gen,Prefix,Key,Gen,As0}) ->
    As = [mk_val(A) || A <- As0],
    asn1ct_func:call_gen(Prefix, Key, Gen, As);
enc_cg({'cond',Cs}) ->
    enc_cg_cond(Cs);
enc_cg({'cond',Cs,Dst0}) ->
    Dst = mk_val(Dst0),
    emit([Dst," = "]),
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
enc_cg(nil) ->
    emit("[]");
enc_cg({sub,Src0,Int,Dst0}) ->
    Src = mk_val(Src0),
    Dst = mk_val(Dst0),
    emit([Dst," = ",Src," - ",Int]);
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

enc_cg_cond([{'_',Action}]) ->
    enc_cg(Action);
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
     fun(Fd, Name) -> gen_name2pos(Fd, Name, NNL) end,[Src]}.

gen_name2pos(Fd, Name, Names) ->
    Cs0 = gen_name2pos_cs(Names, Name),
    Cs = Cs0 ++ [bit_clause(Name),nil_clause(),invalid_clause()],
    F = {function,1,Name,1,Cs},
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

enc_opt_al_1([{'cond',Cs0,Dst},{call,per,complete,[Dst],Bin}|T0], Al0) ->
    {Cs1,{M,F}} = enc_opt_al_prepare_cond(Cs0),
    {Cs,_} = enc_opt_al_cond(Cs1, 0),
    {T,Al} = enc_opt_al_1([{call,M,F,[Dst],Bin}|T0], Al0),
    {[{'cond',Cs,Dst}|T],Al};
enc_opt_al_1([H0|T0], Al0) ->
    {H,Al1} = enc_opt_al(H0, Al0),
    {T,Al} = enc_opt_al_1(T0, Al1),
    {H++T,Al};
enc_opt_al_1([], Al) -> {[],Al}.

enc_opt_al({apply,_,_,_}=Imm, Al) ->
    {[Imm],Al};
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
enc_opt_al({sub,_,_,_}=Imm, Al) ->
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

enc_opt_al_prepare_cond(Cs0) ->
    try enc_opt_al_prepare_cond_1(Cs0) of
	Cs ->
	    {Cs,{erlang,iolist_to_binary}}
    catch
	throw:impossible ->
	    {Cs0,{per,complete}}
    end.

enc_opt_al_prepare_cond_1(Cs) ->
    [[C|enc_opt_al_prepare_cond_2(Act)] || [C|Act] <- Cs].

enc_opt_al_prepare_cond_2([{put_bits,_,binary,[U|_]}|_]) when U rem 8 =/= 0 ->
    throw(impossible);
enc_opt_al_prepare_cond_2([{put_bits,_,_,_}=H|T]) ->
    [H|enc_opt_al_prepare_cond_2(T)];
enc_opt_al_prepare_cond_2([{call,per_common,encode_fragmented,_}=H|T]) ->
    [H|enc_opt_al_prepare_cond_2(T)];
enc_opt_al_prepare_cond_2([_|_]) ->
    throw(impossible);
enc_opt_al_prepare_cond_2([]) ->
    [{put_bits,0,0,[1,align]}].


%%%
%%% For the aligned PER format, fix up the intermediate format
%%% before code generation. Code generation will be somewhat
%%% easier if 'align' appear as a separate instruction.
%%%

per_fixup([{apply,_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{apply,_,_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{block,Block}|T]) ->
    [{block,per_fixup(Block)}|per_fixup(T)];
per_fixup([{'assign',_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{'cond',Cs0}|T]) ->
    Cs = [[C|per_fixup(Act)] || [C|Act] <- Cs0],
    [{'cond',Cs}|per_fixup(T)];
per_fixup([{'cond',Cs0,Dst}|T]) ->
    Cs = [[C|per_fixup(Act)] || [C|Act] <- Cs0],
    [{'cond',Cs,Dst}|per_fixup(T)];
per_fixup([{call,_,_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{call,_,_,_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{call_gen,_,_,_,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{error,_}=H|T]) ->
    [H|per_fixup(T)];
per_fixup([{lc,B,V,L}|T]) ->
    [{lc,per_fixup(B),V,L}|per_fixup(T)];
per_fixup([{lc,B,V,L,Dst}|T]) ->
    [{lc,per_fixup(B),V,L,Dst}|per_fixup(T)];
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
