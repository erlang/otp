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
    Len = {add,per_dec_normally_small_number(Aligned),1},
    {get_bits,Len,[1,bitstring]}.

per_dec_integer(Constraint0, Aligned) ->
    Constraint = effective_constraint(integer, Constraint0),
    per_dec_integer_1(Constraint, Aligned).

per_dec_length(SingleValue, _, _Aligned) when is_integer(SingleValue) ->
    {value,SingleValue};
per_dec_length({S,S}, _, _Aligned) when is_integer(S) ->
    {value,S};
per_dec_length({{_,_}=Constr,_}, AllowZero, Aligned) ->
    bit_case(per_dec_length(Constr, AllowZero, Aligned),
	     per_dec_length(undefined, AllowZero, Aligned));
per_dec_length({Lb,Ub}, _AllowZero, Aligned) when is_integer(Lb),
						  is_integer(Lb),
						  Ub =< 65535 ->
    per_dec_constrained(Lb, Ub, Aligned);
per_dec_length({_,_}, AllowZero, Aligned) ->
    decode_unconstrained_length(AllowZero, Aligned);
per_dec_length(undefined, AllowZero, Aligned) ->
    decode_unconstrained_length(AllowZero, Aligned).

per_dec_named_integer(Constraint, NamedList0, Aligned) ->
    Int = per_dec_integer(Constraint, Aligned),
    NamedList = [{K,V} || {V,K} <- NamedList0] ++ [integer_default],
    {map,Int,NamedList}.

per_dec_k_m_string(StringType, Constraint, Aligned) ->
    SzConstr = get_constraint(Constraint, 'SizeConstraint'),
    N = string_num_bits(StringType, Constraint, Aligned),
    Imm = dec_string(SzConstr, N, Aligned),
    Chars = char_tab(Constraint, StringType, N),
    convert_string(N, Chars, Imm).

per_dec_octet_string(Constraint, Aligned) ->
    dec_string(Constraint, 8, Aligned).

per_dec_raw_bitstring(Constraint, Aligned) ->
    dec_string(Constraint, 1, Aligned).

per_dec_open_type(Aligned) ->
    {get_bits,decode_unconstrained_length(true, Aligned),
     [8,binary,{align,Aligned}]}.

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
%%% Local functions.
%%%

dec_string(Sv, U, _Aligned) when is_integer(Sv), U*Sv =< 16 ->
    {get_bits,Sv,[U,binary]};
dec_string(Sv, U, Aligned) when is_integer(Sv), Sv < 16#10000 ->
    {get_bits,Sv,[U,binary,{align,Aligned}]};
dec_string([_|_]=C, U, Aligned) when is_list(C) ->
    dec_string({hd(C),lists:max(C)}, U, Aligned);
dec_string({Sv,Sv}, U, Aligned) ->
    dec_string(Sv, U, Aligned);
dec_string({{_,_}=C,_}, U, Aligned) ->
    bit_case(dec_string(C, U, Aligned),
	     dec_string(no, U, Aligned));
dec_string({Lb,Ub}, U, Aligned) when Ub < 16#10000 ->
    Len = per_dec_constrained(Lb, Ub, Aligned),
    {get_bits,Len,[U,binary,{align,Aligned}]};
dec_string(_, U, Aligned) ->
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
    per_dec_unconstrained(Aligned);
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
opt_al('NULL'=Null, A) ->
    {Null,A};
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
flatten({value,'NULL'}, Buf0, St0) ->
    {{"'NULL'",Buf0},[],St0};
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
    [C];
effective_constraint(integer, C) ->
    SVs = get_constraints(C, 'SingleValue'),
    SV = effective_constr('SingleValue', SVs),
    VRs = get_constraints(C, 'ValueRange'),
    VR = effective_constr('ValueRange', VRs),
    greatest_common_range(SV, VR);
effective_constraint(bitstring, C) ->
    get_constraint(C, 'SizeConstraint').

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
