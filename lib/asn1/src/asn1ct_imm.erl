%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
-export([per_dec_boolean/0,per_dec_enumerated/2,per_dec_enumerated/3,
	 per_dec_integer/2,per_dec_length/3,per_dec_named_integer/3,
	 dec_slim_cg/2,dec_code_gen/2]).
-export([effective_constraint/2]).
-import(asn1ct_gen, [emit/1]).

-record(st, {var,
	     base}).

dec_slim_cg(Imm, BytesVar) ->
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

per_dec_boolean() ->
    {map,{get_bits,1,[1]},[{0,false},{1,true}]}.

per_dec_enumerated(NamedList0, Aligned) ->
    Constraint = [{'ValueRange',{0,length(NamedList0)-1}}],
    NamedList = per_dec_enumerated_fix_list(NamedList0, [enum_error], 0),
    Int = per_dec_integer(Constraint, Aligned),
    {map,Int,NamedList}.

per_dec_enumerated(BaseNamedList, NamedListExt0, Aligned) ->
    Base = per_dec_enumerated(BaseNamedList, Aligned),
    NamedListExt = per_dec_enumerated_fix_list(NamedListExt0,
					       [enum_default], 0),
    Ext = {map,per_dec_normally_small_number(Aligned),NamedListExt},
    bit_case(Base, Ext).

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

%%%
%%% Local functions.
%%%

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
flatten({value,V0}, Buf0, St0) when is_integer(V0) ->
    {{V0,Buf0},[],St0};
flatten({value,V0}, Buf0, St0) ->
    flatten(V0, Buf0, St0).

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
    case is_aligned(U) of
	false ->
	    flatten_align_1(U, Dst, Pre++[Gb0], St0);
	true ->
	    {PadBits,St1} = new_var("Pad", St0),
	    {DstBuf,St2} = new_var("Buf", St1),
	    Ab = {align_bits,SrcBuf,PadBits},
	    Agb = {get_bits,{PadBits,SrcBuf},[1],{'_',DstBuf}},
	    Gb = {get_bits,{SrcBits,DstBuf},U,Dst},
	    flatten_align_1(U, Dst, Pre++[Ab,Agb,Gb], St2)
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

is_aligned(Fl) ->
    proplists:get_bool(align, Fl).

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

bit_flags([1|T], Acc) ->
    bit_flags(T, Acc);
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
    "/" ++ bit_flags_1(Acc, "").

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
