%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2017. All Rights Reserved.
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
%% Purpose: Type-based optimisations. See the comment for verified_type/1
%% the very end of this file for a description of the types in the
%% type database.

-module(beam_type).

-export([module/2]).

-import(lists, [foldl/3,member/2,reverse/1,reverse/2,sort/1]).

-define(UNICODE_INT, {integer,{0,16#10FFFF}}).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opts) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Asm0}) ->
    try
	Asm1 = beam_utils:live_opt(Asm0),
	Asm2 = opt(Asm1, [], tdb_new()),
	Asm3 = beam_utils:live_opt(Asm2),
	Asm = beam_utils:delete_annos(Asm3),
	{function,Name,Arity,CLabel,Asm}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

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
    simplify_basic(Is, Ts, []).

simplify_basic([I0|Is], Ts0, Acc) ->
    case simplify_instr(I0, Ts0) of
        [] ->
            simplify_basic(Is, Ts0, Acc);
        [I] ->
            Ts = update(I, Ts0),
            simplify_basic(Is, Ts, [I|Acc])
    end;
simplify_basic([], Ts, Acc) ->
    {reverse(Acc),Ts}.

%% simplify_instr(Instruction, Ts) -> [Instruction].

%%  Simplify a simple instruction using type information. Return an
%%  empty list if the instruction should be removed, or a list with
%%  the original or modified instruction.

simplify_instr({set,[D],[{integer,Index},Reg],{bif,element,_}}=I, Ts) ->
    case max_tuple_size(Reg, Ts) of
        Sz when 0 < Index, Index =< Sz ->
            [{set,[D],[Reg],{get_tuple_element,Index-1}}];
        _ -> [I]
    end;
simplify_instr({test,Test,Fail,[R]}=I, Ts) ->
    case tdb_find(R, Ts) of
        any ->
            [I];
        Type ->
            case will_succeed(Test, Type) of
                yes -> [];
                no -> [{jump,Fail}];
                maybe -> [I]
            end
    end;
simplify_instr({set,[D],[TupleReg],{get_tuple_element,0}}=I, Ts) ->
    case tdb_find(TupleReg, Ts) of
        {tuple,_,_,[Contents]} ->
            [{set,[D],[Contents],move}];
        _ ->
            [I]
    end;
simplify_instr({test,test_arity,_,[R,Arity]}=I, Ts) ->
    case tdb_find(R, Ts) of
        {tuple,exact_size,Arity,_} -> [];
        _ -> [I]
    end;
simplify_instr({test,is_eq_exact,Fail,[R,{atom,A}=Atom]}=I, Ts) ->
    case tdb_find(R, Ts) of
        {atom,_}=Atom -> [];
        boolean when is_boolean(A) -> [I];
        any -> [I];
        _ -> [{jump,Fail}]
    end;
simplify_instr({test,is_record,_,[R,{atom,_}=Tag,{integer,Arity}]}=I, Ts) ->
    case tdb_find(R, Ts) of
        {tuple,exact_size,Arity,[Tag]} -> [];
        _ -> [I]
    end;
simplify_instr({select,select_val,Reg,_,_}=I, Ts) ->
    [case tdb_find(Reg, Ts) of
         {integer,Range} ->
             simplify_select_val_int(I, Range);
         boolean ->
             simplify_select_val_bool(I);
         _ ->
             I
     end];
simplify_instr({test,bs_test_unit,_,[Src,Unit]}=I, Ts) ->
    case tdb_find(Src, Ts) of
        {binary,U} when U rem Unit =:= 0 -> [];
        _ -> [I]
    end;
simplify_instr(I, _) -> [I].

simplify_select_val_int({select,select_val,R,_,L0}=I, {Min,Max}) ->
    Vs = sort([V || {integer,V} <- L0]),
    case eq_ranges(Vs, Min, Max) of
	false -> I;
	true -> simplify_select_val_1(L0, {integer,Max}, R, [])
    end.

simplify_select_val_bool({select,select_val,R,_,L}=I) ->
    Vs = sort([V || {atom,V} <- L]),
    case Vs of
	[false,true] ->
	    simplify_select_val_1(L, {atom,false}, R, []);
	_ ->
	    I
    end.

simplify_select_val_1([Val,F|T], Val, R, Acc) ->
    L = reverse(Acc, T),
    {select,select_val,R,F,L};
simplify_select_val_1([V,F|T], Val, R, Acc) ->
    simplify_select_val_1(T, Val, R, [F,V|Acc]).

eq_ranges([H], H, H) -> true;
eq_ranges([H|T], H, Max) -> eq_ranges(T, H+1, Max);
eq_ranges(_, _, _) -> false.

%% will_succeed(TestOperation, Type) -> yes|no|maybe.
%%  Test whether TestOperation applied to an argument of type Type
%%  will succeed.  Return yes, no, or maybe.
%%
%%  Type is a type as described in the comment for verified_type/1 at
%%  the very end of this file, but it will *never* be 'any'.

will_succeed(is_atom, Type) ->
    case Type of
        {atom,_} -> yes;
        boolean -> yes;
        _ -> no
    end;
will_succeed(is_binary, Type) ->
    case Type of
        {binary,U} when U rem 8 =:= 0 -> yes;
        {binary,_} -> maybe;
        _ -> no
    end;
will_succeed(is_bitstr, Type) ->
    case Type of
        {binary,_} -> yes;
        _ -> no
    end;
will_succeed(is_integer, Type) ->
    case Type of
        integer -> yes;
        {integer,_} -> yes;
        _ -> no
    end;
will_succeed(is_map, Type) ->
    case Type of
        map -> yes;
        _ -> no
    end;
will_succeed(is_nonempty_list, Type) ->
    case Type of
        nonempty_list -> yes;
        _ -> no
    end;
will_succeed(is_tuple, Type) ->
    case Type of
        {tuple,_,_,_} -> yes;
        _ -> no
    end;
will_succeed(_, _) -> maybe.

%% simplify_float([Instruction], TypeDatabase) ->
%%                 {[Instruction],TypeDatabase'} | not_possible
%%  Simplify floating point operations in blocks.
%%
simplify_float(Is0, Ts0) ->
    {Is1,Ts} = simplify_float_1(Is0, Ts0, [], []),
    Is2 = opt_fmoves(Is1, []),
    Is3 = flt_need_heap(Is2),
    try
	{flt_liveness(Is3),Ts}
    catch
	throw:not_possible -> not_possible
    end.

simplify_float_1([{set,[],[],fclearerror}|Is], Ts, Rs, Acc) ->
    simplify_float_1(Is, Ts, Rs, clearerror(Acc));
simplify_float_1([{set,[],[],fcheckerror}|Is], Ts, Rs, Acc) ->
    simplify_float_1(Is, Ts, Rs, checkerror(Acc));
simplify_float_1([{set,[{fr,_}],_,_}=I|Is], Ts, Rs, Acc) ->
    simplify_float_1(Is, Ts, Rs, [I|Acc]);
simplify_float_1([{set,[D0],[A0],{alloc,_,{gc_bif,'-',{f,0}}}}=I|Is]=Is0,
		 Ts0, Rs0, Acc0) ->
    case tdb_find(A0, Ts0) of
	float ->
	    A = coerce_to_float(A0),
	    {Rs1,Acc1} = load_reg(A, Ts0, Rs0, Acc0),
	    {D,Rs} = find_dest(D0, Rs1),
	    Areg = fetch_reg(A, Rs),
	    Acc = [{set,[D],[Areg],{bif,fnegate,{f,0}}}|clearerror(Acc1)],
	    Ts = tdb_store(D0, float, Ts0),
	    simplify_float_1(Is, Ts, Rs, Acc);
	_Other ->
	    Ts = update(I, Ts0),
	    {Rs,Acc} = flush(Rs0, Is0, Acc0),
	    simplify_float_1(Is, Ts, Rs, [I|checkerror(Acc)])
    end;
simplify_float_1([{set,[D0],[A0,B0],{alloc,_,{gc_bif,Op0,{f,0}}}}=I|Is]=Is0,
		 Ts0, Rs0, Acc0) ->
    case float_op(Op0, A0, B0, Ts0) of
	no ->
	    Ts = update(I, Ts0),
	    {Rs,Acc} = flush(Rs0, Is0, Acc0),
	    simplify_float_1(Is, Ts, Rs, [I|checkerror(Acc)]);
	{yes,Op} ->
	    A = coerce_to_float(A0),
	    B = coerce_to_float(B0),
	    {Rs1,Acc1} = load_reg(A, Ts0, Rs0, Acc0),
	    {Rs2,Acc2} = load_reg(B, Ts0, Rs1, Acc1),
	    {D,Rs} = find_dest(D0, Rs2),
	    Areg = fetch_reg(A, Rs),
	    Breg = fetch_reg(B, Rs),
	    Acc = [{set,[D],[Areg,Breg],{bif,Op,{f,0}}}|clearerror(Acc2)],
	    Ts = tdb_store(D0, float, Ts0),
	    simplify_float_1(Is, Ts, Rs, Acc)
    end;
simplify_float_1([{set,_,_,{try_catch,_,_}}=I|Is]=Is0, _Ts, Rs0, Acc0) ->
    Acc = flush_all(Rs0, Is0, Acc0),
    simplify_float_1(Is, tdb_new(), Rs0, [I|Acc]);
simplify_float_1([{set,_,_,{line,_}}=I|Is], Ts, Rs, Acc) ->
    simplify_float_1(Is, Ts, Rs, [I|Acc]);
simplify_float_1([I|Is], Ts0, [], Acc) ->
    Ts = update(I, Ts0),
    simplify_float_1(Is, Ts, [], [I|Acc]);
simplify_float_1([I|Is]=Is0, Ts0, Rs0, Acc0) ->
    Ts = update(I, Ts0),
    {Rs,Acc} = flush(Rs0, Is0, Acc0),
    simplify_float_1(Is, Ts, Rs, [I|checkerror(Acc)]);
simplify_float_1([], Ts, [], Acc) ->
    Is = reverse(Acc),
    {Is,Ts}.

coerce_to_float({integer,I}=Int) ->
    try float(I) of
	F ->
	    {float,F}
    catch _:_ ->
	    %% Let the overflow happen at run-time.
	    Int
    end;
coerce_to_float(Other) -> Other.

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
merge_blocks(B1, [{'%anno',_}|B2]) ->
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
%% The following instructions cause the insertion of an allocation
%% instruction if needed.
flt_need_heap_2({set,_,_,{alloc,_,_}}, H, Fl) ->
    {flt_alloc(H, Fl),0,0};
flt_need_heap_2({set,_,_,{set_tuple_element,_}}, H, Fl) ->
    {flt_alloc(H, Fl),0,0};
flt_need_heap_2({'%anno',_}, H, Fl) ->
    {flt_alloc(H, Fl),0,0};
%% All other instructions are "neutral". We just pass them.
flt_need_heap_2(_, H, Fl) ->
    {[],H,Fl}.

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
%%  function. We base liveness of the number of register map at the
%%  beginning of the instruction sequence.
%%
%%  A 'not_possible' term will be thrown if the set of live registers
%%  is not continous at an allocation function (e.g. if {x,0} and {x,2}
%%  are live, but not {x,1}).

flt_liveness([{'%anno',{used,Regs}}=LiveInstr|Is]) ->
    flt_liveness_1(Is, Regs, [LiveInstr]).

flt_liveness_1([{set,Ds,Ss,{alloc,Live0,Alloc}}|Is], Regs0, Acc) ->
    Live = min(Live0, live_regs(Regs0)),
    I = {set,Ds,Ss,{alloc,Live,Alloc}},
    Regs1 = init_regs(Live),
    Regs = x_live(Ds, Regs1),
    flt_liveness_1(Is, Regs, [I|Acc]);
flt_liveness_1([{set,Ds,_,_}=I|Is], Regs0, Acc) ->
    Regs = x_live(Ds, Regs0),
    flt_liveness_1(Is, Regs, [I|Acc]);
flt_liveness_1([{'%anno',_}], _Regs, Acc) ->
    reverse(Acc).

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

x_live([{x,N}|Rs], Regs) -> x_live(Rs, Regs bor (1 bsl N));
x_live([_|Rs], Regs) -> x_live(Rs, Regs);
x_live([], Regs) -> Regs.

%% update(Instruction, TypeDb) -> NewTypeDb
%%  Update the type database to account for executing an instruction.
%%
%%  First the cases for instructions inside basic blocks.
update({'%anno',_}, Ts) ->
    Ts;
update({set,[D],[S],move}, Ts) ->
    tdb_copy(S, D, Ts);
update({set,[D],[Index,Reg],{bif,element,_}}, Ts0) ->
    MinSize = case Index of
                  {integer,I} -> I;
                  _ -> 0
              end,
    Ts = tdb_meet(Reg, {tuple,min_size,MinSize,[]}, Ts0),
    tdb_store(D, any, Ts);
update({set,[D],Args,{bif,N,_}}, Ts) ->
    Ar = length(Args),
    BoolOp = erl_internal:new_type_test(N, Ar) orelse
	erl_internal:comp_op(N, Ar) orelse
	erl_internal:bool_op(N, Ar),
    Type = case BoolOp of
               true -> boolean;
               false -> unary_op_type(N)
           end,
    tdb_store(D, Type, Ts);
update({set,[D],[S],{get_tuple_element,0}}, Ts0) ->
    if
        D =:= S ->
            tdb_store(D, any, Ts0);
        true ->
            Ts = tdb_store(D, {tuple_element,S,0}, Ts0),
            tdb_store(S, {tuple,min_size,1,[]}, Ts)
    end;
update({set,[D],[S],{alloc,_,{gc_bif,float,{f,0}}}}, Ts0) ->
    %% Make sure we reject non-numeric literal argument.
    case possibly_numeric(S) of
        true ->  tdb_store(D, float, Ts0);
        false -> Ts0
    end;
update({set,[D],[S1,S2],{alloc,_,{gc_bif,'band',{f,0}}}}, Ts) ->
    Type = band_type(S1, S2, Ts),
    tdb_store(D, Type, Ts);
update({set,[D],[S1,S2],{alloc,_,{gc_bif,'/',{f,0}}}}, Ts) ->
    %% Make sure we reject non-numeric literals.
    case possibly_numeric(S1) andalso possibly_numeric(S2) of
        true -> tdb_store(D, float, Ts);
        false -> Ts
    end;
update({set,[D],[S1,S2],{alloc,_,{gc_bif,Op,{f,0}}}}, Ts0) ->
    case op_type(Op) of
	integer ->
	    tdb_store(D, integer, Ts0);
        {float,_} ->
            case {tdb_find(S1, Ts0),tdb_find(S2, Ts0)} of
                {float,_} -> tdb_store(D, float, Ts0);
                {_,float} -> tdb_store(D, float, Ts0);
                {_,_} -> tdb_store(D, any, Ts0)
            end;
        Type ->
            tdb_store(D, Type, Ts0)
    end;
update({set,[D],[_],{alloc,_,{gc_bif,Op,{f,0}}}}, Ts) ->
    tdb_store(D, unary_op_type(Op), Ts);
update({set,[],_Src,_Op}, Ts) ->
    Ts;
update({set,[D],_Src,_Op}, Ts) ->
    tdb_store(D, any, Ts);
update({kill,D}, Ts) ->
    tdb_store(D, any, Ts);

%% Instructions outside of blocks.
update({test,test_arity,_Fail,[Src,Arity]}, Ts) ->
    tdb_meet(Src, {tuple,exact_size,Arity,[]}, Ts);
update({get_map_elements,_,Src,{list,Elems0}}, Ts0) ->
    Ts1 = tdb_meet(Src, map, Ts0),
    {_Ss,Ds} = beam_utils:split_even(Elems0),
    foldl(fun(Dst, A) -> tdb_store(Dst, any, A) end, Ts1, Ds);
update({test,is_eq_exact,_,[Reg,{atom,_}=Atom]}, Ts0) ->
    Ts = case tdb_find_source_tuple(Reg, Ts0) of
             {source_tuple,TupleReg} ->
                 tdb_meet(TupleReg, {tuple,min_size,1,[Atom]}, Ts0);
             none ->
                 Ts0
         end,
    tdb_meet(Reg, Atom, Ts);
update({test,is_record,_Fail,[Src,Tag,{integer,Arity}]}, Ts) ->
    tdb_meet(Src, {tuple,exact_size,Arity,[Tag]}, Ts);

%% Binaries and binary matching.

update({test,bs_get_integer2,_,_,Args,Dst}, Ts) ->
    tdb_store(Dst, get_bs_integer_type(Args), Ts);
update({test,bs_get_utf8,_,_,_,Dst}, Ts) ->
    tdb_store(Dst, ?UNICODE_INT, Ts);
update({test,bs_get_utf16,_,_,_,Dst}, Ts) ->
    tdb_store(Dst, ?UNICODE_INT, Ts);
update({test,bs_get_utf32,_,_,_,Dst}, Ts) ->
    tdb_store(Dst, ?UNICODE_INT, Ts);
update({bs_init,_,{bs_init2,_,_},_,_,Dst}, Ts) ->
    tdb_store(Dst, {binary,8}, Ts);
update({bs_init,_,_,_,_,Dst}, Ts) ->
    tdb_store(Dst, {binary,1}, Ts);
update({bs_put,_,_,_}, Ts) ->
    Ts;
update({bs_save2,_,_}, Ts) ->
    Ts;
update({bs_restore2,_,_}, Ts) ->
    Ts;
update({bs_context_to_binary,Dst}, Ts) ->
    tdb_store(Dst, {binary,1}, Ts);
update({test,bs_start_match2,_,_,[Src,_],Dst}, Ts0) ->
    Ts = tdb_meet(Src, {binary,1}, Ts0),
    tdb_copy(Src, Dst, Ts);
update({test,bs_get_binary2,_,_,[_,_,Unit,_],Dst}, Ts) ->
    true = is_integer(Unit),                    %Assertion.
    tdb_store(Dst, {binary,Unit}, Ts);
update({test,bs_get_float2,_,_,_,Dst}, Ts) ->
    tdb_store(Dst, float, Ts);
update({test,bs_test_unit,_,[Src,Unit]}, Ts) ->
    tdb_meet(Src, {binary,Unit}, Ts);

%% Other test instructions
update({test,Test,_Fail,[Src]}, Ts) ->
    Type = case Test of
               is_binary -> {binary,8};
               is_bitstr -> {binary,1};
               is_boolean -> boolean;
               is_float -> float;
               is_integer -> integer;
               is_map -> map;
               is_nonempty_list -> nonempty_list;
               _ -> any
           end,
    tdb_meet(Src, Type, Ts);
update({test,_Test,_Fail,_Other}, Ts) ->
    Ts;

%% Calls

update({call_ext,Ar,{extfunc,math,Math,Ar}}, Ts) ->
    case is_math_bif(Math, Ar) of
	true -> tdb_store({x,0}, float, Ts);
	false -> tdb_kill_xregs(Ts)
    end;
update({call_ext,3,{extfunc,erlang,setelement,3}}, Ts0) ->
    Ts = tdb_kill_xregs(Ts0),
    case tdb_find({x,1}, Ts0) of
	{tuple,SzKind,Sz,_}=T0 ->
	    T = case tdb_find({x,0}, Ts0) of
		    {integer,{I,I}} when I > 1 ->
			%% First element is not changed. The result
			%% will have the same type.
			T0;
		    _ ->
			%% Position is 1 or unknown. May change the
			%% first element of the tuple.
			{tuple,SzKind,Sz,[]}
		end,
            tdb_store({x,0}, T, Ts);
	_ ->
	    Ts
    end;
update({call,_Arity,_Func}, Ts) -> tdb_kill_xregs(Ts);
update({call_ext,_Arity,_Func}, Ts) -> tdb_kill_xregs(Ts);
update({make_fun2,_,_,_,_}, Ts) -> tdb_kill_xregs(Ts);
update({call_fun, _}, Ts) -> tdb_kill_xregs(Ts);
update({apply, _}, Ts) -> tdb_kill_xregs(Ts);

update({line,_}, Ts) -> Ts;
update({'%',_}, Ts) -> Ts;

%% The instruction is unknown.  Kill all information.
update(_I, _Ts) -> tdb_new().

band_type({integer,Int}, Other, Ts) ->
    band_type_1(Int, Other, Ts);
band_type(Other, {integer,Int}, Ts) ->
    band_type_1(Int, Other, Ts);
band_type(_, _, _) -> integer.

band_type_1(Int, OtherSrc, Ts) ->
    Type = band_type_2(Int, 0),
    OtherType = tdb_find(OtherSrc, Ts),
    meet(Type, OtherType).

band_type_2(N, Bits) when Bits < 64 ->
    case 1 bsl Bits of
	P when P =:= N + 1 ->
	    {integer,{0,N}};
	P when P > N + 1 ->
	    integer;
	_ ->
	    band_type_2(N, Bits+1)
    end;
band_type_2(_, _) ->
    %% Negative or large positive number. Give up.
    integer.

get_bs_integer_type([_,{integer,N},U,{field_flags,Fl}])
  when N*U < 64 ->
    NumBits = N*U,
    case member(unsigned, Fl) of
	true ->
	    {integer,{0,(1 bsl NumBits)-1}};
	false ->
	    %% Signed integer. Don't bother.
	    integer
    end;
get_bs_integer_type(_) ->
    %% Avoid creating ranges with a huge upper limit.
    integer.

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
is_math_bif(log2, 1) -> true;
is_math_bif(log10, 1) -> true;
is_math_bif(sqrt, 1) -> true;
is_math_bif(atan2, 2) -> true;
is_math_bif(pow, 2) -> true;
is_math_bif(ceil, 1) -> true;
is_math_bif(floor, 1) -> true;
is_math_bif(fmod, 2) -> true;
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
	{tuple,_,Sz,_} -> Sz;
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

arith_op(Op) ->
    case op_type(Op) of
	{float,Instr} -> {yes,Instr};
	_ -> no
    end.

op_type('+') -> {float,fadd};
op_type('-') -> {float,fsub};
op_type('*') -> {float,fmul};
%% '/' and 'band' are specially handled.
op_type('bor') -> integer;
op_type('bxor') -> integer;
op_type('bsl') -> integer;
op_type('bsr') -> integer;
op_type('div') -> integer;
op_type(_) -> any.

unary_op_type(bit_size) -> integer;
unary_op_type(byte_size) -> integer;
unary_op_type(length) -> integer;
unary_op_type(map_size) -> integer;
unary_op_type(size) -> integer;
unary_op_type(tuple_size) -> integer;
unary_op_type(_) -> any.

flush(Rs, [{set,[_],[_,_,_],{bif,is_record,_}}|_]=Is0, Acc0) ->
    Acc = flush_all(Rs, Is0, Acc0),
    {[],Acc};
flush(Rs, [{set,[_],[],{put_tuple,_}}|_]=Is0, Acc0) ->
    Acc = flush_all(Rs, Is0, Acc0),
    {[],Acc};
flush(Rs0, [{set,Ds,Ss,_Op}|_], Acc0) ->
    Save = cerl_sets:from_list(Ss),
    Acc = save_regs(Rs0, Save, Acc0),
    Rs1 = foldl(fun(S, A) -> mark(S, A, clean) end, Rs0, Ss),
    Kill = cerl_sets:from_list(Ds),
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
    case cerl_sets:is_element(V, Save) of
	true -> [{set,[V],[{fr,I}],fmove}|checkerror(Acc)];
	false -> Acc
    end;
save_reg(_, _, Acc) -> Acc.

kill_regs(Rs, Kill) ->
    [kill_reg(R, Kill) || R <- Rs].

kill_reg({_,V,_}=R, Kill) ->
    case cerl_sets:is_element(V, Kill) of
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
%%% See the comment for verified_type/1 at the end of module for
%%% a description of the possible types.

%% tdb_new() -> EmptyDataBase
%%  Creates a new, empty type database.

tdb_new() -> [].

%% tdb_find(Register, Db) -> Type
%%  Returns type information or the atom error if there is no type
%%  information available for Register.
%%
%%  See the comment for verified_type/1 at the end of module for
%%  a description of the possible types.

tdb_find(Reg, Ts) ->
    case tdb_find_raw(Reg, Ts) of
        {tuple_element,_,_} -> any;
        Type -> Type
    end.

%% tdb_find_source_tuple(Register, Ts) -> {source_tuple,Register} | 'none'.
%%  Find the tuple whose first element was fetched to the register Register.

tdb_find_source_tuple(Reg, Ts) ->
    case tdb_find_raw(Reg, Ts) of
        {tuple_element,Src,0} ->
            {source_tuple,Src};
        _ ->
            none
    end.

%% tdb_copy(Source, Dest, Db) -> Db'
%%  Update the type information for Dest to have the same type
%%  as the Source.

tdb_copy({Tag,_}=S, D, Ts) when Tag =:= x; Tag =:= y ->
    case tdb_find_raw(S, Ts) of
        any -> orddict:erase(D, Ts);
        Type -> orddict:store(D, Type, Ts)
    end;
tdb_copy(Literal, D, Ts) ->
    Type = case Literal of
	       {atom,_} -> Literal;
	       {float,_} -> float;
	       {integer,Int} -> {integer,{Int,Int}};
	       {literal,[_|_]} -> nonempty_list;
	       {literal,#{}} -> map;
	       {literal,Tuple} when tuple_size(Tuple) >= 1 ->
		   Lit = tag_literal(element(1, Tuple)),
		   {tuple,exact_size,tuple_size(Tuple),[Lit]};
	       _ -> any
	   end,
    tdb_store(D, verified_type(Type), Ts).

%% tdb_store(Register, Type, Ts0) -> Ts.
%%  Store a new type for register Register. Return the update type
%%  database. Use this function when a new value is assigned to
%%  a register.
%%
%%  See the comment for verified_type/1 at the end of module for
%%  a description of the possible types.

tdb_store(Reg, any, Ts) ->
    erase(Reg, Ts);
tdb_store(Reg, Type, Ts) ->
    store(Reg, verified_type(Type), Ts).

store(Key, New, [{K,_}|_]=Dict) when Key < K ->
    [{Key,New}|Dict];
store(Key, New, [{K,Val}=E|Dict]) when Key > K ->
    case Val of
        {tuple_element,Key,_} -> store(Key, New, Dict);
        _ -> [E|store(Key, New, Dict)]
    end;
store(Key, New, [{_K,Old}|Dict]) ->		%Key == K
    case Old of
        {tuple,_,_,_} ->
            [{Key,New}|erase_tuple_element(Key, Dict)];
        _ ->
            [{Key,New}|Dict]
    end;
store(Key, New, []) -> [{Key,New}].

erase(Key, [{K,_}=E|Dict]) when Key < K ->
    [E|Dict];
erase(Key, [{K,Val}=E|Dict]) when Key > K ->
    case Val of
        {tuple_element,Key,_} -> erase(Key, Dict);
        _ -> [E|erase(Key, Dict)]
    end;
erase(Key, [{_K,Val}|Dict]) ->                 %Key == K
    case Val of
        {tuple,_,_,_} -> erase_tuple_element(Key, Dict);
        _ -> Dict
    end;
erase(_, []) -> [].

erase_tuple_element(Key, [{_,{tuple_element,Key,_}}|Dict]) ->
    erase_tuple_element(Key, Dict);
erase_tuple_element(Key, [E|Dict]) ->
    [E|erase_tuple_element(Key, Dict)];
erase_tuple_element(_Key, []) -> [].

%% tdb_meet(Register, Type, Ts0) -> Ts.
%%  Update information of a register that is used as the source for an
%%  instruction. The type Type will be combined using the meet operation
%%  with the previous type information for the register, resulting in
%%  narrower (more specific) type.
%%
%%  For example, if the previous type is {tuple,min_size,2,[]} and the
%%  the new type is {tuple,exact_size,5,[]}, the meet of the types will
%%  be {tuple,exact_size,5,[]}.
%%
%%  See the comment for verified_type/1 at the end of module for
%%  a description of the possible types.

tdb_meet(Reg, NewType, Ts) ->
    Update = fun(Type0) -> meet(Type0, NewType) end,
    orddict:update(Reg, Update, NewType, Ts).

%%%
%%% Here follows internal helper functions for accessing and
%%% updating the type database.
%%%

tdb_find_raw({x,_}=K, Ts) -> tdb_find_raw_1(K, Ts);
tdb_find_raw({y,_}=K, Ts) -> tdb_find_raw_1(K, Ts);
tdb_find_raw(_, _) -> any.

tdb_find_raw_1(K, Ts) ->
    case orddict:find(K, Ts) of
	{ok,Val} -> Val;
	error -> any
    end.

tag_literal(A) when is_atom(A) -> {atom,A};
tag_literal(F) when is_float(F) -> {float,F};
tag_literal(I) when is_integer(I) -> {integer,I};
tag_literal([]) -> nil;
tag_literal(Lit) -> {literal,Lit}.

%% tdb_kill_xregs(Db) -> NewDb
%%  Kill all information about x registers. Also kill all tuple_element
%%  dependencies from y registers to x registers.

tdb_kill_xregs([{{x,_},_Type}|Db]) -> tdb_kill_xregs(Db);
tdb_kill_xregs([{{y,_},{tuple_element,{x,_},_}}|Db]) -> tdb_kill_xregs(Db);
tdb_kill_xregs([Any|Db]) -> [Any|tdb_kill_xregs(Db)];
tdb_kill_xregs([]) -> [].

%% meet(Type1, Type2) -> Type
%%  Returns the "meet" of Type1 and Type2. The meet is a narrower
%%  type than Type1 and Type2. For example:
%%
%%     meet(integer, {integer,{0,3}}) -> {integer,{0,3}}
%%
%%  The meet for two different types result in 'none', which is
%%  the bottom element for our type lattice:
%%
%%     meet(integer, map) -> none

meet(T, T) ->
    T;
meet({integer,_}=T, integer) ->
    T;
meet(integer, {integer,_}=T) ->
    T;
meet({integer,{Min1,Max1}}, {integer,{Min2,Max2}}) ->
    {integer,{max(Min1, Min2),min(Max1, Max2)}};
meet({tuple,min_size,Sz1,Same}, {tuple,min_size,Sz2,Same}=Max) when Sz1 < Sz2 ->
    Max;
meet({tuple,min_size,Sz1,Same}=Max, {tuple,min_size,Sz2,Same}) when Sz1 > Sz2 ->
    Max;
meet({tuple,exact_size,_,Same}=Exact, {tuple,_,_,Same}) ->
    Exact;
meet({tuple,_,_,Same},{tuple,exact_size,_,Same}=Exact) ->
    Exact;
meet({tuple,SzKind1,Sz1,[]}, {tuple,_SzKind2,_Sz2,First}=Tuple2) ->
    meet({tuple,SzKind1,Sz1,First}, Tuple2);
meet({tuple,_SzKind1,_Sz1,First}=Tuple1, {tuple,SzKind2,Sz2,_}) ->
    meet(Tuple1, {tuple,SzKind2,Sz2,First});
meet({binary,U1}, {binary,U2}) ->
    {binary,max(U1, U2)};
meet(T1, T2) ->
    case is_any(T1) of
        true ->
            verified_type(T2);
        false ->
            case is_any(T2) of
                true ->
                    verified_type(T1);
                false ->
                    none                        %The bottom element.
            end
    end.

is_any(any) -> true;
is_any({tuple_element,_,_}) -> true;
is_any(_) -> false.

%% verified_type(Type) -> Type
%%  Returns the passed in type if it is one of the defined types.
%%  Crashes if there is anything wrong with the type.
%%
%%  Here are all possible types:
%%
%%  any                  Any Erlang term (top element for the type lattice).
%%
%%  {atom,Atom}          The specific atom Atom.
%%  {binary,Unit}        Binary/bitstring aligned to unit Unit.
%%  boolean              'true' | 'false'
%%  float                Floating point number.
%%  integer              Integer.
%%  {integer,{Min,Max}}  Integer in the inclusive range Min through Max.
%%  map                  Map.
%%  nonempty_list        Nonempty list.
%%  {tuple,_,_,_}        Tuple (see below).
%%
%%  none                 No type (bottom element for the type lattice).
%%
%%  {tuple,min_size,Size,First} means that the corresponding register
%%  contains a tuple with *at least* Size elements (conversely,
%%  {tuple,exact_size,Size,First} means that it contains a tuple with
%%  *exactly* Size elements). An tuple with unknown size is
%%  represented as {tuple,min_size,0,[]}. First is either [] (meaning
%%  that the tuple's first element is unknown) or [FirstElement] (the
%%  contents of the first element).
%%
%%  There is also a pseudo-type called {tuple_element,_,_}:
%%
%%    {tuple_element,SrcTuple,ElementNumber}
%%
%%  that does not provide any information about the type of the
%%  register itself, but provides a link back to the source tuple that
%%  the register got its value from.
%%
%%  Note that {tuple_element,_,_} will *never* be returned by tdb_find/2.
%%  Use tdb_find_source_tuple/2 to locate the source tuple for a register.

verified_type(any=T) -> T;
verified_type({atom,_}=T) -> T;
verified_type({binary,U}=T) when is_integer(U) -> T;
verified_type(boolean=T) -> T;
verified_type(integer=T) -> T;
verified_type({integer,{Min,Max}}=T)
  when is_integer(Min), is_integer(Max) -> T;
verified_type(map=T) -> T;
verified_type(nonempty_list=T) -> T;
verified_type({tuple,_,Sz,[]}=T) when is_integer(Sz) -> T;
verified_type({tuple,_,Sz,[_]}=T) when is_integer(Sz) -> T;
verified_type({tuple_element,_,_}=T) -> T;
verified_type(float=T) -> T.
