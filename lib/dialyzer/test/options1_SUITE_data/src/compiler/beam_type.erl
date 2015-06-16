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
%%     $Id: beam_type.erl,v 1.1 2008/12/17 09:53:41 mikpe Exp $
%% Purpose : Type-based optimisations.

-module(beam_type).

-export([module/2]).

-import(lists, [map/2,foldl/3,reverse/1,reverse/2,filter/2,member/2]).

module({Mod,Exp,Attr,Fs0,Lc}, Opt) ->
    AllowFloatOpts = not member(no_float_opt, Opt),
    Fs = map(fun(F) -> function(F, AllowFloatOpts) end, Fs0),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Asm0}, AllowFloatOpts) ->
    Asm = opt(Asm0, AllowFloatOpts, [], tdb_new()),
    {function,Name,Arity,CLabel,Asm}.

%% opt([Instruction], AllowFloatOpts, Accumulator, TypeDb) -> {[Instruction'],TypeDb'}
%%  Keep track of type information; try to simplify.

opt([{block,Body1}|Is], AllowFloatOpts, [{block,Body0}|Acc], Ts0) ->
    {Body2,Ts} = simplify(Body1, Ts0, AllowFloatOpts),
    Body = beam_block:merge_blocks(Body0, Body2),
    opt(Is, AllowFloatOpts, [{block,Body}|Acc], Ts);
opt([{block,Body0}|Is], AllowFloatOpts, Acc, Ts0) ->
    {Body,Ts} = simplify(Body0, Ts0, AllowFloatOpts),
    opt(Is, AllowFloatOpts, [{block,Body}|Acc], Ts);
opt([I0|Is], AllowFloatOpts, Acc, Ts0) ->
    case simplify([I0], Ts0, AllowFloatOpts) of
	{[],Ts} -> opt(Is, AllowFloatOpts, Acc, Ts);
	{[I],Ts} -> opt(Is, AllowFloatOpts, [I|Acc], Ts)
    end;
opt([], _, Acc, _) -> reverse(Acc).

%% simplify(Instruction, TypeDb, AllowFloatOpts) -> NewInstruction
%%  Simplify an instruction using type information (this is
%%  technically a "strength reduction").

simplify(Is, TypeDb, false) ->
    simplify(Is, TypeDb, no_float_opt, []);
simplify(Is, TypeDb, true) ->
    case are_live_regs_determinable(Is) of
	false -> simplify(Is, TypeDb, no_float_opt, []);
	true -> simplify(Is, TypeDb, [], [])
    end.

simplify([{set,[D],[{integer,Index},Reg],{bif,element,_}}=I0|Is]=Is0, Ts0, Rs0, Acc0) ->
    I = case max_tuple_size(Reg, Ts0) of
	    Sz when 0 < Index, Index =< Sz ->
		{set,[D],[Reg],{get_tuple_element,Index-1}};
	    _Other -> I0
    end,
    Ts = update(I, Ts0),
    {Rs,Acc} = flush(Rs0, Is0, Acc0),
    simplify(Is, Ts, Rs, [I|checkerror(Acc)]);
simplify([{set,[D0],[A],{bif,'-',{f,0}}}=I|Is]=Is0, Ts0, Rs0, Acc0)
  when Rs0 =/= no_float_opt ->
    case tdb_find(A, Ts0) of
	float ->
	    {Rs1,Acc1} = load_reg(A, Ts0, Rs0, Acc0),
	    {D,Rs} = find_dest(D0, Rs1),
	    Areg = fetch_reg(A, Rs),
	    Acc = [{set,[D],[Areg],{bif,fnegate,{f,0}}}|clearerror(Acc1)],
	    Ts = tdb_update([{D0,float}], Ts0),
	    simplify(Is, Ts, Rs, Acc);
	_Other ->
	    Ts = update(I, Ts0),
	    {Rs,Acc} = flush(Rs0, Is0, Acc0),
	    simplify(Is, Ts, Rs, [I|checkerror(Acc)])
    end;
simplify([{set,[_],[_],{bif,_,{f,0}}}=I|Is]=Is0, Ts0, Rs0, Acc0) ->
    Ts = update(I, Ts0),
    {Rs,Acc} = flush(Rs0, Is0, Acc0),
    simplify(Is, Ts, Rs, [I|checkerror(Acc)]);
simplify([{set,[D0],[A,B],{bif,Op0,{f,0}}}=I|Is]=Is0, Ts0, Rs0, Acc0)
  when Rs0 =/= no_float_opt ->
    case float_op(Op0, A, B, Ts0) of
	no ->
	    Ts = update(I, Ts0),
	    {Rs,Acc} = flush(Rs0, Is0, Acc0),
	    simplify(Is, Ts, Rs, [I|checkerror(Acc)]);
	{yes,Op} ->
	    {Rs1,Acc1} = load_reg(A, Ts0, Rs0, Acc0),
	    {Rs2,Acc2} = load_reg(B, Ts0, Rs1, Acc1),
	    {D,Rs} = find_dest(D0, Rs2),
	    Areg = fetch_reg(A, Rs),
	    Breg = fetch_reg(B, Rs),
	    Acc = [{set,[D],[Areg,Breg],{bif,Op,{f,0}}}|clearerror(Acc2)],
	    Ts = tdb_update([{D0,float}], Ts0),
	    simplify(Is, Ts, Rs, Acc)
    end;
simplify([{set,[D],[TupleReg],{get_tuple_element,0}}=I|Is0], Ts0, Rs0, Acc0) ->
    case tdb_find(TupleReg, Ts0) of
	{tuple,_,[Contents]} ->
	    Ts = tdb_update([{D,Contents}], Ts0),
	    {Rs,Acc} = flush(Rs0, Is0, Acc0),
	    simplify(Is0, Ts, Rs, [{set,[D],[Contents],move}|Acc]);
	_ ->
	    Ts = update(I, Ts0),
	    {Rs,Acc} = flush(Rs0, Is0, Acc0),
	    simplify(Is0, Ts, Rs, [I|checkerror(Acc)])
    end;
simplify([{set,_,_,{'catch',_}}=I|Is]=Is0, _Ts, Rs0, Acc0) ->
    Acc = flush_all(Rs0, Is0, Acc0),
    simplify(Is, tdb_new(), Rs0, [I|Acc]);
simplify([{test,is_tuple,_,[R]}=I|Is], Ts, Rs, Acc) ->
    case tdb_find(R, Ts) of
	{tuple,_,_} -> simplify(Is, Ts, Rs, Acc);
	_ ->
	    simplify(Is, Ts, Rs, [I|Acc])
    end;
simplify([{test,test_arity,_,[R,Arity]}=I|Is], Ts0, Rs, Acc) ->
    case tdb_find(R, Ts0) of
	{tuple,Arity,_} ->
	    simplify(Is, Ts0, Rs, Acc);
	_Other ->
	    Ts = update(I, Ts0),
	    simplify(Is, Ts, Rs, [I|Acc])
    end;
simplify([{test,is_eq_exact,Fail,[R,{atom,_}=Atom]}=I|Is0], Ts0, Rs0, Acc0) ->
    Acc1 = case tdb_find(R, Ts0) of
	       {atom,_}=Atom -> Acc0;
	       {atom,_} -> [{jump,Fail}|Acc0];
	       _ -> [I|Acc0]
	   end,
    Ts = update(I, Ts0),
    {Rs,Acc} = flush(Rs0, Is0, Acc1),
    simplify(Is0, Ts, Rs, Acc);
simplify([I|Is]=Is0, Ts0, Rs0, Acc0) ->
    Ts = update(I, Ts0),
    {Rs,Acc} = flush(Rs0, Is0, Acc0),
    simplify(Is, Ts, Rs, [I|Acc]);
simplify([], Ts, Rs, Acc) ->
    Is0 = reverse(flush_all(Rs, [], Acc)),
    Is1 = opt_fmoves(Is0, []),
    Is = add_ftest_heap(Is1),
    {Is,Ts}.

opt_fmoves([{set,[{x,_}=R],[{fr,_}]=Src,fmove}=I1,
	    {set,[{y,_}]=Dst,[{x,_}=R],move}=I2|Is], Acc) ->
    case beam_block:is_killed(R, Is) of
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

%% update(Instruction, TypeDb) -> NewTypeDb
%%  Update the type database to account for executing an instruction.
%%
%%  First the cases for instructions inside basic blocks.
update({set,[D],[S],move}, Ts0) ->
    Ops = case tdb_find(S, Ts0) of
	      error -> [{D,kill}];
	      Info -> [{D,Info}]
	  end,
    tdb_update(Ops, Ts0);
update({set,[D],[{integer,I},Reg],{bif,element,_}}, Ts0) ->
    tdb_update([{Reg,{tuple,I,[]}},{D,kill}], Ts0);
update({set,[D],[_Index,Reg],{bif,element,_}}, Ts0) ->
    tdb_update([{Reg,{tuple,0,[]}},{D,kill}], Ts0);
update({set,[D],[S],{get_tuple_element,0}}, Ts) ->
    tdb_update([{D,{tuple_element,S,0}}], Ts);
update({set,[D],[S],{bif,float,{f,0}}}, Ts0) ->
    %% Make sure we reject non-numeric literal argument.
    case possibly_numeric(S) of
	true ->  tdb_update([{D,float}], Ts0);
	false -> Ts0
    end;
update({set,[D],[S1,S2],{bif,'/',{f,0}}}, Ts0) ->
    %% Make sure we reject non-numeric literals.
    case possibly_numeric(S1) andalso possibly_numeric(S2) of
	true ->  tdb_update([{D,float}], Ts0);
	false -> Ts0
    end;
update({set,[D],[S1,S2],{bif,Op,{f,0}}}, Ts0) ->
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
update({allocate,_,_}, Ts) -> Ts;
update({init,D}, Ts) ->
    tdb_update([{D,kill}], Ts);
update({kill,D}, Ts) ->
    tdb_update([{D,kill}], Ts);
update({'%live',_}, Ts) -> Ts;

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
update({test,_Test,_Fail,_Other}, Ts) -> Ts;
update({call_ext,1,{extfunc,math,Math,1}}, Ts) ->
    case is_math_bif(Math, 1) of
	true -> tdb_update([{{x,0},float}], Ts);
	false -> tdb_kill_xregs(Ts)
    end;
update({call_ext,2,{extfunc,math,Math,2}}, Ts) ->
    case is_math_bif(Math, 2) of
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

flush(no_float_opt, _, Acc) -> {no_float_opt,Acc};
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

flush_all(no_float_opt, _, Acc) -> Acc;
flush_all([{_,{float,_},_}|Rs], Is, Acc) ->
    flush_all(Rs, Is, Acc);
flush_all([{I,V,dirty}|Rs], Is, Acc0) ->
    Acc = checkerror(Acc0),
    case beam_block:is_killed(V, Is) of
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
    map(fun(R) -> kill_reg(R, Kill) end, Rs).

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

add_ftest_heap(Is) ->
    add_ftest_heap_1(reverse(Is), 0, []).

add_ftest_heap_1([{set,_,[{fr,_}],fmove}=I|Is], Floats, Acc) ->
    add_ftest_heap_1(Is, Floats+1, [I|Acc]);
add_ftest_heap_1([{allocate,_,_}=I|Is], 0, Acc) ->
    reverse(Is, [I|Acc]);
add_ftest_heap_1([{allocate,Regs,{Z,Stk,Heap,Inits}}|Is], Floats, Acc) ->
    reverse(Is, [{allocate,Regs,{Z,Stk,Heap,Floats,Inits}}|Acc]);
add_ftest_heap_1([I|Is], Floats, Acc) ->
    add_ftest_heap_1(Is, Floats, [I|Acc]);
add_ftest_heap_1([], 0, Acc) ->
    Acc;
add_ftest_heap_1([], Floats, Is) ->
    Regs = beam_block:live_at_entry(Is),
    [{allocate,Regs,{nozero,nostack,0,Floats,[]}}|Is].

are_live_regs_determinable([{allocate,_,_}|_]) -> true;
are_live_regs_determinable([{'%live',_}|_]) -> true;
are_live_regs_determinable([_|Is]) -> are_live_regs_determinable(Is);
are_live_regs_determinable([]) -> false.


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
%%  Returns type information or the atom error if there are no type
%%  information available for Register.

tdb_find(Key, [{K,_}|_]) when Key < K -> error;
tdb_find(Key, [{Key,Info}|_]) -> Info;
tdb_find(Key, [_|Db]) -> tdb_find(Key, Db);
tdb_find(_, []) -> error.

%% tdb_update([UpdateOp], Db) -> NewDb
%%        UpdateOp = {Register,kill}|{Register,NewInfo}
%%  Updates a type database.  If a 'kill' operation is given, the type
%%  information for that register will be removed from the database.
%%  A kill operation takes precende over other operations for the same
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
merge_type_info({tuple,Sz1,[]}, {tuple,Sz2,First}) ->
    merge_type_info({tuple,Sz1,First}, {tuple,Sz2,First});
merge_type_info({tuple,Sz1,First}, {tuple,Sz2,_}) ->
    merge_type_info({tuple,Sz1,First}, {tuple,Sz2,First});
merge_type_info(NewType, _) ->
    verify_type(NewType),
    NewType.

verify_type({tuple,Sz,[]}) when is_integer(Sz) -> ok;
verify_type({tuple,Sz,[_]}) when is_integer(Sz) -> ok;
verify_type({tuple_element,_,_}) -> ok;
verify_type(float) -> ok;
verify_type({atom,_}) -> ok.
