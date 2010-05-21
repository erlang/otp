%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2010. All Rights Reserved.
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
%% Purpose : Type-based optimisations.

-module(beam_type).

-export([module/2]).

-import(lists, [foldl/3,reverse/1,filter/2]).

module({Mod,Exp,Attr,Fs0,Lc}, _Opts) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Asm0}) ->
    Asm1 = beam_utils:live_opt(Asm0),
    Asm2 = opt(Asm1, [], tdb_new()),
    Asm = beam_utils:delete_live_annos(Asm2),
    {function,Name,Arity,CLabel,Asm}.

%% opt([Instruction], Accumulator, TypeDb) -> {[Instruction'],TypeDb'}
%%  Keep track of type information; try to simplify.

opt([{block,Body1}|Is], [{block,Body0}|Acc], Ts0) ->
    {Body2,Ts} = simplify(Body1, Ts0),
    Body = merge_blocks(Body0, Body2),
    opt(Is, [{block,Body}|Acc], Ts);
opt([{block,Body0}|Is], Acc, Ts0) ->
    {Body,Ts} = simplify(Body0, Ts0),
    opt(Is, [{block,Body}|Acc], Ts);
opt([I0|Is], Acc, Ts0) ->
    case simplify_basic([I0], Ts0) of
	{[],Ts} -> opt(Is, Acc, Ts);
	{[I],Ts} -> opt(Is, [I|Acc], Ts)
    end;
opt([], Acc, _) -> reverse(Acc).

%% simplify(Instruction, TypeDb) -> NewInstruction
%%  Simplify an instruction using type information (this is
%%  technically a "strength reduction").

simplify(Is0, TypeDb0) ->
    {Is,_} = BasicRes = simplify_basic(Is0, TypeDb0),
    case simplify_float(Is, TypeDb0) of
	not_possible -> BasicRes;
	{_,_}=Res -> Res
    end.

%% simplify_basic([Instruction], TypeDatabase) -> {[Instruction],TypeDatabase'}
%%  Basic simplification, mostly tuples, no floating point optimizations.

simplify_basic(Is, Ts) ->
    simplify_basic_1(Is, Ts, []).
    
simplify_basic_1([{set,[D],[{integer,Index},Reg],{bif,element,_}}=I0|Is], Ts0, Acc) ->
    I = case max_tuple_size(Reg, Ts0) of
	    Sz when 0 < Index, Index =< Sz ->
		{set,[D],[Reg],{get_tuple_element,Index-1}};
	    _Other -> I0
    end,
    Ts = update(I, Ts0),
    simplify_basic_1(Is, Ts, [I|Acc]);
simplify_basic_1([{set,[D],[TupleReg],{get_tuple_element,0}}=I|Is0], Ts0, Acc) ->
    case tdb_find(TupleReg, Ts0) of
	{tuple,_,[Contents]} ->
	    simplify_basic_1([{set,[D],[Contents],move}|Is0], Ts0, Acc);
	_ ->
	    Ts = update(I, Ts0),
	    simplify_basic_1(Is0, Ts, [I|Acc])
    end;
simplify_basic_1([{set,_,_,{'catch',_}}=I|Is], _Ts, Acc) ->
    simplify_basic_1(Is, tdb_new(), [I|Acc]);
simplify_basic_1([{test,is_tuple,_,[R]}=I|Is], Ts, Acc) ->
    case tdb_find(R, Ts) of
	{tuple,_,_} -> simplify_basic_1(Is, Ts, Acc);
	_ -> simplify_basic_1(Is, Ts, [I|Acc])
    end;
simplify_basic_1([{test,test_arity,_,[R,Arity]}=I|Is], Ts0, Acc) ->
    case tdb_find(R, Ts0) of
	{tuple,Arity,_} ->
	    simplify_basic_1(Is, Ts0, Acc);
	_Other ->
	    Ts = update(I, Ts0),
	    simplify_basic_1(Is, Ts, [I|Acc])
    end;
simplify_basic_1([{test,is_eq_exact,Fail,[R,{atom,_}=Atom]}=I|Is0], Ts0, Acc0) ->
    Acc = case tdb_find(R, Ts0) of
	      {atom,_}=Atom -> Acc0;
	      {atom,_} -> [{jump,Fail}|Acc0];
	      _ -> [I|Acc0]
	  end,
    Ts = update(I, Ts0),
    simplify_basic_1(Is0, Ts, Acc);
simplify_basic_1([{test,is_record,_,[R,{atom,_}=Tag,{integer,Arity}]}=I|Is], Ts0, Acc) ->
    case tdb_find(R, Ts0) of
	{tuple,Arity,[Tag]} ->
	    simplify_basic_1(Is, Ts0, Acc);
	_Other ->
	    Ts = update(I, Ts0),
	    simplify_basic_1(Is, Ts, [I|Acc])
    end;
simplify_basic_1([I|Is], Ts0, Acc) ->
    Ts = update(I, Ts0),
    simplify_basic_1(Is, Ts, [I|Acc]);
simplify_basic_1([], Ts, Acc) ->
    Is = reverse(Acc),
    {Is,Ts}.

%% simplify_float([Instruction], TypeDatabase) ->
%%                 {[Instruction],TypeDatabase'} | not_possible
%%  Simplify floating point operations in blocks.
%%
simplify_float(Is0, Ts0) ->
    {Is1,Ts} = simplify_float_1(Is0, Ts0, [], []),
    Is2 = flt_need_heap(Is1),
    try
	{flt_liveness(Is2),Ts}
    catch
	throw:not_possible -> not_possible
    end.

simplify_float_1([{set,[D0],[A],{alloc,_,{gc_bif,'-',{f,0}}}}=I|Is]=Is0, Ts0, Rs0, Acc0) ->
    case tdb_find(A, Ts0) of
	float ->
	    {Rs1,Acc1} = load_reg(A, Ts0, Rs0, Acc0),
	    {D,Rs} = find_dest(D0, Rs1),
	    Areg = fetch_reg(A, Rs),
	    Acc = [{set,[D],[Areg],{bif,fnegate,{f,0}}}|clearerror(Acc1)],
	    Ts = tdb_update([{D0,float}], Ts0),
	    simplify_float_1(Is, Ts, Rs, Acc);
	_Other ->
	    Ts = update(I, Ts0),
	    {Rs,Acc} = flush(Rs0, Is0, Acc0),
	    simplify_float_1(Is, Ts, Rs, [I|checkerror(Acc)])
    end;
simplify_float_1([{set,[D0],[A,B],{alloc,_,{gc_bif,Op0,{f,0}}}}=I|Is]=Is0, Ts0, Rs0, Acc0) ->
    case float_op(Op0, A, B, Ts0) of
	no ->
	    Ts = update(I, Ts0),
	    {Rs,Acc} = flush(Rs0, Is0, Acc0),
	    simplify_float_1(Is, Ts, Rs, [I|checkerror(Acc)]);
	{yes,Op} ->
	    {Rs1,Acc1} = load_reg(A, Ts0, Rs0, Acc0),
	    {Rs2,Acc2} = load_reg(B, Ts0, Rs1, Acc1),
	    {D,Rs} = find_dest(D0, Rs2),
	    Areg = fetch_reg(A, Rs),
	    Breg = fetch_reg(B, Rs),
	    Acc = [{set,[D],[Areg,Breg],{bif,Op,{f,0}}}|clearerror(Acc2)],
	    Ts = tdb_update([{D0,float}], Ts0),
	    simplify_float_1(Is, Ts, Rs, Acc)
    end;
simplify_float_1([{set,_,_,{'catch',_}}=I|Is]=Is0, _Ts, Rs0, Acc0) ->
    Acc = flush_all(Rs0, Is0, Acc0),
    simplify_float_1(Is, tdb_new(), Rs0, [I|Acc]);
simplify_float_1([I|Is]=Is0, Ts0, Rs0, Acc0) ->
    Ts = update(I, Ts0),
    {Rs,Acc} = flush(Rs0, Is0, Acc0),
    simplify_float_1(Is, Ts, Rs, [I|checkerror(Acc)]);
simplify_float_1([], Ts, Rs, Acc0) ->
    Acc = checkerror(Acc0),
    Is0 = reverse(flush_all(Rs, [], Acc)),
    Is = opt_fmoves(Is0, []),
    {Is,Ts}.

opt_fmoves([{set,[{x,_}=R],[{fr,_}]=Src,fmove}=I1,
	    {set,[_]=Dst,[{x,_}=R],move}=I2|Is], Acc) ->
    case beam_utils:is_killed_block(R, Is) of
	false -> opt_fmoves(Is, [I2,I1|Acc]);
	true -> opt_fmoves(Is, [{set,Dst,Src,fmove}|Acc])
    end;
opt_fmoves([I|Is], Acc) ->
    opt_fmoves(Is, [I|Acc]);
opt_fmoves([], Acc) -> reverse(Acc).

clearerror(Is) ->
    clearerror(Is, Is).

clearerror([{set,[],[],fclearerror}|_], OrigIs) -> OrigIs;
clearerror([{set,[],[],fcheckerror}|_], OrigIs) -> [{set,[],[],fclearerror}|OrigIs];
clearerror([_|Is], OrigIs) -> clearerror(Is, OrigIs);
clearerror([], OrigIs) -> [{set,[],[],fclearerror}|OrigIs].

%% merge_blocks(Block1, Block2) -> Block.
%%  Combine two blocks and eliminate any move instructions that assign
%%  to registers that are killed later in the block.
%%
merge_blocks(B1, [{'%live',_}|B2]) ->
    merge_blocks_1(B1++[{set,[],[],stop_here}|B2]).

merge_blocks_1([{set,[],_,stop_here}|Is]) -> Is;
merge_blocks_1([{set,[D],_,move}=I|Is]) ->
    case beam_utils:is_killed_block(D, Is) of
	true -> merge_blocks_1(Is);
	false -> [I|merge_blocks_1(Is)]
    end;
merge_blocks_1([I|Is]) -> [I|merge_blocks_1(Is)].

%% flt_need_heap([Instruction]) -> [Instruction]
%%  Insert need heap allocation instructions in the instruction stream
%%  to properly account for both inserted floating point operations and
%%  normal term build operations (such as put_list/3).
%%
%%  Ignore old heap allocation instructions (except if they allocate a stack
%%  frame too), as they may be in the wrong place (because gc_bif instructions
%%  could have been converted to floating point operations).

flt_need_heap(Is) ->
    flt_need_heap_1(reverse(Is), 0, 0, []).

flt_need_heap_1([{set,[],[],{alloc,_,Alloc}}|Is], H, Fl, Acc) ->
    case Alloc of
	{_,nostack,_,_} ->
	    %% Remove any existing test_heap/2 instruction.
	    flt_need_heap_1(Is, H, Fl, Acc);
	{Z,Stk,_,Inits} when is_integer(Stk) ->
	    %% Keep any allocate*/2 instruction and recalculate heap need.
	    I = {set,[],[],{alloc,regs,{Z,Stk,build_alloc(H, Fl),Inits}}},
	    flt_need_heap_1(Is, 0, 0, [I|Acc])
    end;
flt_need_heap_1([I|Is], H0, Fl0, Acc) ->
    {Ns,H1,Fl1} = flt_need_heap_2(I, H0, Fl0),
    flt_need_heap_1(Is, H1, Fl1, [I|Ns]++Acc);
flt_need_heap_1([], H, Fl, Acc) ->
    flt_alloc(H, Fl) ++ Acc.

%% First come all instructions that build. We pass through, while we
%% add to the need for heap words and floats on the heap.
flt_need_heap_2({set,[_],[{fr,_}],fmove}, H, Fl) ->
    {[],H,Fl+1};
flt_need_heap_2({set,_,_,put_list}, H, Fl) ->
    {[],H+2,Fl};
flt_need_heap_2({set,_,_,{put_tuple,_}}, H, Fl) ->
    {[],H+1,Fl};
flt_need_heap_2({set,_,_,put}, H, Fl) ->
    {[],H+1,Fl};
%% Then the "neutral" instructions. We just pass them.
flt_need_heap_2({set,[{fr,_}],_,_}, H, Fl) ->
    {[],H,Fl};
flt_need_heap_2({set,[],[],fclearerror}, H, Fl) ->
    {[],H,Fl};
flt_need_heap_2({set,[],[],fcheckerror}, H, Fl) ->
    {[],H,Fl};
flt_need_heap_2({set,_,_,{bif,_,_}}, H, Fl) ->
    {[],H,Fl};
flt_need_heap_2({set,_,_,move}, H, Fl) ->
    {[],H,Fl};
flt_need_heap_2({set,_,_,{get_tuple_element,_}}, H, Fl) ->
    {[],H,Fl};
flt_need_heap_2({set,_,_,get_list}, H, Fl) ->
    {[],H,Fl};
flt_need_heap_2({set,_,_,{'catch',_}}, H, Fl) ->
    {[],H,Fl};
%% All other instructions should cause the insertion of an allocation
%% instruction if needed.
flt_need_heap_2(_, H, Fl) ->
    {flt_alloc(H, Fl),0,0}.

flt_alloc(0, 0) ->
    [];
flt_alloc(H, 0) ->
    [{set,[],[],{alloc,regs,{nozero,nostack,H,[]}}}];
flt_alloc(H, F) ->
    [{set,[],[],{alloc,regs,{nozero,nostack,
			     build_alloc(H, F),[]}}}].

build_alloc(Words, 0) -> Words;
build_alloc(Words, Floats) -> {alloc,[{words,Words},{floats,Floats}]}.


%% flt_liveness([Instruction]) -> [Instruction]
%%  (Re)calculate the number of live registers for each heap allocation
%%  function. We base liveness of the number of live registers at
%%  entry to the instruction sequence.
%%
%%  A 'not_possible' term will be thrown if the set of live registers
%%  is not continous at an allocation function (e.g. if {x,0} and {x,2}
%%  are live, but not {x,1}).

flt_liveness([{'%live',Live}=LiveInstr|Is]) ->
    flt_liveness_1(Is, init_regs(Live), [LiveInstr]).

flt_liveness_1([{set,Ds,Ss,{alloc,_,Alloc}}|Is], Regs0, Acc) ->
    Live = live_regs(Regs0),
    I = {set,Ds,Ss,{alloc,Live,Alloc}},
    Regs = foldl(fun(R, A) -> set_live(R, A) end, Regs0, Ds),
    flt_liveness_1(Is, Regs, [I|Acc]);
flt_liveness_1([{set,Ds,_,_}=I|Is], Regs0, Acc) ->
    Regs = foldl(fun(R, A) -> set_live(R, A) end, Regs0, Ds),
    flt_liveness_1(Is, Regs, [I|Acc]);
flt_liveness_1([{'%live',_}=I|Is], Regs, Acc) ->
    flt_liveness_1(Is, Regs, [I|Acc]);
flt_liveness_1([], _Regs, Acc) -> reverse(Acc).

init_regs(Live) ->
    (1 bsl Live) - 1.

live_regs(Regs) ->
    live_regs_1(Regs, 0).

live_regs_1(0, N) -> N;
live_regs_1(R, N) ->
    case R band 1 of
	0 -> throw(not_possible);
	1 -> live_regs_1(R bsr 1, N+1)
    end.

set_live({x,X}, Regs) -> Regs bor (1 bsl X);
set_live(_, Regs) -> Regs.

%% update(Instruction, TypeDb) -> NewTypeDb
%%  Update the type database to account for executing an instruction.
%%
%%  First the cases for instructions inside basic blocks.
update({'%live',_}, Ts) -> Ts;
update({set,[D],[S],move}, Ts) ->
    tdb_copy(S, D, Ts);
update({set,[D],[{integer,I},Reg],{bif,element,_}}, Ts0) ->
    tdb_update([{Reg,{tuple,I,[]}},{D,kill}], Ts0);
update({set,[D],[_Index,Reg],{bif,element,_}}, Ts0) ->
    tdb_update([{Reg,{tuple,0,[]}},{D,kill}], Ts0);
update({set,[D],[S],{get_tuple_element,0}}, Ts) ->
    tdb_update([{D,{tuple_element,S,0}}], Ts);
update({set,[D],[S],{alloc,_,{gc_bif,float,{f,0}}}}, Ts0) ->
    %% Make sure we reject non-numeric literal argument.
    case possibly_numeric(S) of
	true ->  tdb_update([{D,float}], Ts0);
	false -> Ts0
    end;
update({set,[D],[S1,S2],{alloc,_,{gc_bif,'/',{f,0}}}}, Ts0) ->
    %% Make sure we reject non-numeric literals.
    case possibly_numeric(S1) andalso possibly_numeric(S2) of
	true ->  tdb_update([{D,float}], Ts0);
	false -> Ts0
    end;
update({set,[D],[S1,S2],{alloc,_,{gc_bif,Op,{f,0}}}}, Ts0) ->
    case arith_op(Op) of
	no ->
	    tdb_update([{D,kill}], Ts0);
	{yes,_} ->
	    case {tdb_find(S1, Ts0),tdb_find(S2, Ts0)} of
		{float,_} -> tdb_update([{D,float}], Ts0);
		{_,float} -> tdb_update([{D,float}], Ts0);
		{_,_} -> tdb_update([{D,kill}], Ts0)
	    end
    end;
update({set,[],_Src,_Op}, Ts0) -> Ts0;
update({set,[D],_Src,_Op}, Ts0) ->
    tdb_update([{D,kill}], Ts0);
update({set,[D1,D2],_Src,_Op}, Ts0) ->
    tdb_update([{D1,kill},{D2,kill}], Ts0);
update({kill,D}, Ts) ->
    tdb_update([{D,kill}], Ts);

%% Instructions outside of blocks.
update({test,is_float,_Fail,[Src]}, Ts0) ->
    tdb_update([{Src,float}], Ts0);
update({test,test_arity,_Fail,[Src,Arity]}, Ts0) ->
    tdb_update([{Src,{tuple,Arity,[]}}], Ts0);
update({test,is_eq_exact,_,[Reg,{atom,_}=Atom]}, Ts) ->
    case tdb_find(Reg, Ts) of
	error ->
	    Ts;
	{tuple_element,TupleReg,0} ->
	    tdb_update([{TupleReg,{tuple,1,[Atom]}}], Ts);
	_ ->
	    Ts
    end;
update({test,is_record,_Fail,[Src,Tag,{integer,Arity}]}, Ts) ->
    tdb_update([{Src,{tuple,Arity,[Tag]}}], Ts);
update({test,_Test,_Fail,_Other}, Ts) ->
    Ts;
update({call_ext,Ar,{extfunc,math,Math,Ar}}, Ts) ->
    case is_math_bif(Math, Ar) of
	true -> tdb_update([{{x,0},float}], Ts);
	false -> tdb_kill_xregs(Ts)
    end;
update({call_ext,3,{extfunc,erlang,setelement,3}}, Ts0) ->
    Op = case tdb_find({x,1}, Ts0) of
	     error -> kill;
	     Info -> Info
	 end,
    Ts1 = tdb_kill_xregs(Ts0),
    tdb_update([{{x,0},Op}], Ts1);
update({call,_Arity,_Func}, Ts) -> tdb_kill_xregs(Ts);
update({call_ext,_Arity,_Func}, Ts) -> tdb_kill_xregs(Ts);
update({make_fun2,_,_,_,_}, Ts) -> tdb_kill_xregs(Ts);

%% The instruction is unknown.  Kill all information.
update(_I, _Ts) -> tdb_new().

is_math_bif(cos, 1) -> true;
is_math_bif(cosh, 1) -> true;
is_math_bif(sin, 1) -> true;
is_math_bif(sinh, 1) -> true;
is_math_bif(tan, 1) -> true;
is_math_bif(tanh, 1) -> true;
is_math_bif(acos, 1) -> true;
is_math_bif(acosh, 1) -> true;
is_math_bif(asin, 1) -> true;
is_math_bif(asinh, 1) -> true;
is_math_bif(atan, 1) -> true;
is_math_bif(atanh, 1) -> true;
is_math_bif(erf, 1) -> true;
is_math_bif(erfc, 1) -> true;
is_math_bif(exp, 1) -> true;
is_math_bif(log, 1) -> true;
is_math_bif(log10, 1) -> true;
is_math_bif(sqrt, 1) -> true;
is_math_bif(atan2, 2) -> true;
is_math_bif(pow, 2) -> true;
is_math_bif(pi, 0) -> true;
is_math_bif(_, _) -> false.

%% Reject non-numeric literals.
possibly_numeric({x,_}) -> true;
possibly_numeric({y,_}) -> true;
possibly_numeric({integer,_}) -> true;
possibly_numeric({float,_}) -> true;
possibly_numeric(_) -> false.

max_tuple_size(Reg, Ts) ->
    case tdb_find(Reg, Ts) of
	{tuple,Sz,_} -> Sz;
	_Other -> 0
    end.

float_op('/', A, B, _) ->
    case possibly_numeric(A) andalso possibly_numeric(B) of
	true -> {yes,fdiv};
	false -> no
    end;
float_op(Op, {float,_}, B, _) ->
    case possibly_numeric(B) of
	true -> arith_op(Op);
	false -> no
    end;
float_op(Op, A, {float,_}, _) ->
    case possibly_numeric(A) of
	true -> arith_op(Op);
	false -> no
    end;
float_op(Op, A, B, Ts) ->
    case {tdb_find(A, Ts),tdb_find(B, Ts)} of
	{float,_} -> arith_op(Op);
	{_,float} -> arith_op(Op);
	{_,_} -> no
    end.

find_dest(V, Rs0) ->
    case find_reg(V, Rs0) of
	{ok,FR} ->
	    {FR,mark(V, Rs0, dirty)};
	error ->
	    Rs = put_reg(V, Rs0, dirty),
	    {ok,FR} = find_reg(V, Rs),
	    {FR,Rs}
    end.

load_reg({float,_}=F, _, Rs0, Is0) ->
    Rs = put_reg(F, Rs0, clean),
    {ok,FR} = find_reg(F, Rs),
    Is = [{set,[FR],[F],fmove}|Is0],
    {Rs,Is};
load_reg(V, Ts, Rs0, Is0) ->
    case find_reg(V, Rs0) of
	{ok,_FR} -> {Rs0,Is0};
	error ->
	    Rs = put_reg(V, Rs0, clean),
	    {ok,FR} = find_reg(V, Rs),
	    Op = case tdb_find(V, Ts) of
		     float -> fmove;
		     _ -> fconv
		 end,
	    Is = [{set,[FR],[V],Op}|Is0],
	    {Rs,Is}
    end.

arith_op('+') -> {yes,fadd};
arith_op('-') -> {yes,fsub};
arith_op('*') -> {yes,fmul};
arith_op('/') -> {yes,fdiv};
arith_op(_) -> no.

flush(Rs, [{set,[_],[],{put_tuple,_}}|_]=Is0, Acc0) ->
    Acc = flush_all(Rs, Is0, Acc0),
    {[],Acc};
flush(Rs0, [{set,Ds,Ss,_Op}|_], Acc0) ->
    Save = gb_sets:from_list(Ss),
    Acc = save_regs(Rs0, Save, Acc0),
    Rs1 = foldl(fun(S, A) -> mark(S, A, clean) end, Rs0, Ss),
    Kill = gb_sets:from_list(Ds),
    Rs = kill_regs(Rs1, Kill),
    {Rs,Acc};
flush(Rs0, Is, Acc0) ->
    Acc = flush_all(Rs0, Is, Acc0),
    {[],Acc}.

flush_all([{_,{float,_},_}|Rs], Is, Acc) ->
    flush_all(Rs, Is, Acc);
flush_all([{I,V,dirty}|Rs], Is, Acc0) ->
    Acc = checkerror(Acc0),
    case beam_utils:is_killed_block(V, Is) of
	true  -> flush_all(Rs, Is, Acc);
	false -> flush_all(Rs, Is, [{set,[V],[{fr,I}],fmove}|Acc])
    end;
flush_all([{_,_,clean}|Rs], Is, Acc) -> flush_all(Rs, Is, Acc);
flush_all([free|Rs], Is, Acc) -> flush_all(Rs, Is, Acc);
flush_all([], _, Acc) -> Acc.

save_regs(Rs, Save, Acc) ->
    foldl(fun(R, A) -> save_reg(R, Save, A) end, Acc, Rs).

save_reg({I,V,dirty}, Save, Acc) ->
    case gb_sets:is_member(V, Save) of
	true -> [{set,[V],[{fr,I}],fmove}|checkerror(Acc)];
	false -> Acc
    end;
save_reg(_, _, Acc) -> Acc.

kill_regs(Rs, Kill) ->
    [kill_reg(R, Kill) || R <- Rs].

kill_reg({_,V,_}=R, Kill) ->
    case gb_sets:is_member(V, Kill) of
	true -> free;
	false -> R
    end;
kill_reg(R, _) -> R.

mark(V, [{I,V,_}|Rs], Mark) -> [{I,V,Mark}|Rs];
mark(V, [R|Rs], Mark) -> [R|mark(V, Rs, Mark)];
mark(_, [], _) -> [].

fetch_reg(V, [{I,V,_}|_]) -> {fr,I};
fetch_reg(V, [_|SRs]) -> fetch_reg(V, SRs).

find_reg(V, [{I,V,_}|_]) -> {ok,{fr,I}};
find_reg(V, [_|SRs]) -> find_reg(V, SRs);
find_reg(_, []) -> error.

put_reg(V, Rs, Dirty) -> put_reg_1(V, Rs, Dirty, 0).

put_reg_1(V, [free|Rs], Dirty, I) -> [{I,V,Dirty}|Rs];
put_reg_1(V, [R|Rs], Dirty, I) -> [R|put_reg_1(V, Rs, Dirty, I+1)];
put_reg_1(V, [], Dirty, I) -> [{I,V,Dirty}].

checkerror(Is) ->
    checkerror_1(Is, Is).

checkerror_1([{set,[],[],fcheckerror}|_], OrigIs) -> OrigIs;
checkerror_1([{set,[],[],fclearerror}|_], OrigIs) -> OrigIs;
checkerror_1([{set,_,_,{bif,fadd,_}}|_], OrigIs) -> checkerror_2(OrigIs);
checkerror_1([{set,_,_,{bif,fsub,_}}|_], OrigIs) -> checkerror_2(OrigIs);
checkerror_1([{set,_,_,{bif,fmul,_}}|_], OrigIs) -> checkerror_2(OrigIs);
checkerror_1([{set,_,_,{bif,fdiv,_}}|_], OrigIs) -> checkerror_2(OrigIs);
checkerror_1([{set,_,_,{bif,fnegate,_}}|_], OrigIs) -> checkerror_2(OrigIs);
checkerror_1([_|Is], OrigIs) -> checkerror_1(Is, OrigIs);
checkerror_1([], OrigIs) -> OrigIs.

checkerror_2(OrigIs) -> [{set,[],[],fcheckerror}|OrigIs].


%%% Routines for maintaining a type database.  The type database 
%%% associates type information with registers.
%%%
%%% {tuple,Size,First} means that the corresponding register contains a
%%% tuple with *at least* Size elements.  An tuple with unknown
%%% size is represented as {tuple,0}. First is either [] (meaning that
%%% the tuple's first element is unknown) or [FirstElement] (the contents
%%% of the first element).
%%%
%%% 'float' means that the register contains a float.

%% tdb_new() -> EmptyDataBase
%%  Creates a new, empty type database.

tdb_new() -> [].

%% tdb_find(Register, Db) -> Information|error
%%  Returns type information or the atom error if there is no type
%%  information available for Register.

tdb_find({x,_}=K, Ts) -> tdb_find_1(K, Ts);
tdb_find({y,_}=K, Ts) -> tdb_find_1(K, Ts);
tdb_find(_, _) -> error.

tdb_find_1(K, Ts) ->
    case orddict:find(K, Ts) of
	{ok,Val} -> Val;
	error -> error
    end.

%% tdb_copy(Source, Dest, Db) -> Db'
%%  Update the type information for Dest to have the same type
%%  as the Source.

tdb_copy({Tag,_}=S, D, Ts) when Tag =:= x; Tag =:= y ->
    case tdb_find(S, Ts) of
	error -> orddict:erase(D, Ts);
	Type -> orddict:store(D, Type, Ts)
    end;
tdb_copy(Literal, D, Ts) -> orddict:store(D, Literal, Ts).

%% tdb_update([UpdateOp], Db) -> NewDb
%%        UpdateOp = {Register,kill}|{Register,NewInfo}
%%  Updates a type database.  If a 'kill' operation is given, the type
%%  information for that register will be removed from the database.
%%  A kill operation takes precedence over other operations for the same
%%  register (i.e. [{{x,0},kill},{{x,0},{tuple,5}}] means that the
%%  the existing type information, if any, will be discarded, and the
%%  the '{tuple,5}' information ignored.
%%
%%  If NewInfo information is given and there exists information about
%%  the register, the old and new type information will be merged.
%%  For instance, {tuple,5} and {tuple,10} will be merged to produce
%%  {tuple,10}.

tdb_update(Uis0, Ts0) ->
    Uis1 = filter(fun ({{x,_},_Op}) -> true;
		      ({{y,_},_Op}) -> true;
		      (_) -> false
		  end, Uis0),
    tdb_update1(lists:sort(Uis1), Ts0).

tdb_update1([{Key,kill}|Ops], [{K,_Old}|_]=Db) when Key < K ->
    tdb_update1(remove_key(Key, Ops), Db);
tdb_update1([{Key,_New}=New|Ops], [{K,_Old}|_]=Db) when Key < K ->
    [New|tdb_update1(Ops, Db)];
tdb_update1([{Key,kill}|Ops], [{Key,_}|Db]) ->
    tdb_update1(remove_key(Key, Ops), Db);
tdb_update1([{Key,NewInfo}|Ops], [{Key,OldInfo}|Db]) ->
    [{Key,merge_type_info(NewInfo, OldInfo)}|tdb_update1(Ops, Db)];
tdb_update1([{_,_}|_]=Ops, [Old|Db]) ->
    [Old|tdb_update1(Ops, Db)];
tdb_update1([{Key,kill}|Ops], []) ->
    tdb_update1(remove_key(Key, Ops), []);
tdb_update1([{_,_}=New|Ops], []) ->
    [New|tdb_update1(Ops, [])];
tdb_update1([], Db) -> Db.

%% tdb_kill_xregs(Db) -> NewDb
%%  Kill all information about x registers. Also kill all tuple_element
%%  dependencies from y registers to x registers.

tdb_kill_xregs([{{x,_},_Type}|Db]) -> tdb_kill_xregs(Db);
tdb_kill_xregs([{{y,_},{tuple_element,{x,_},_}}|Db]) -> tdb_kill_xregs(Db);
tdb_kill_xregs([Any|Db]) -> [Any|tdb_kill_xregs(Db)];
tdb_kill_xregs([]) -> [].
    
remove_key(Key, [{Key,_Op}|Ops]) -> remove_key(Key, Ops);
remove_key(_, Ops) -> Ops.
    
merge_type_info(I, I) -> I;
merge_type_info({tuple,Sz1,Same}, {tuple,Sz2,Same}=Max) when Sz1 < Sz2 ->
    Max;
merge_type_info({tuple,Sz1,Same}=Max, {tuple,Sz2,Same}) when Sz1 > Sz2 ->
    Max;
merge_type_info({tuple,Sz1,[]}, {tuple,_Sz2,First}=Tuple2) ->
    merge_type_info({tuple,Sz1,First}, Tuple2);
merge_type_info({tuple,_Sz1,First}=Tuple1, {tuple,Sz2,_}) ->
    merge_type_info(Tuple1, {tuple,Sz2,First});
merge_type_info(NewType, _) ->
    verify_type(NewType),
    NewType.

verify_type({tuple,Sz,[]}) when is_integer(Sz) -> ok;
verify_type({tuple,Sz,[_]}) when is_integer(Sz) -> ok;
verify_type({tuple_element,_,_}) -> ok;
verify_type(float) -> ok.
