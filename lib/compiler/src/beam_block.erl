%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
%% Purpose : Partitions assembly instructions into basic blocks and
%% optimizes them.

-module(beam_block).

-export([module/2]).
-import(lists, [reverse/1,reverse/2,member/2]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, Opts) ->
    Blockify = not member(no_blockify, Opts),
    Fs = [function(F, Blockify) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}, Blockify) ->
    try
	%% Collect basic blocks and optimize them.
        Is2 = case Blockify of
                  true ->
                      Is1 = blockify(Is0),
                      embed_lines(Is1);
                  false ->
                      Is0
              end,
        Is3 = beam_utils:anno_defs(Is2),
        Is4 = move_allocates(Is3),
        Is5 = beam_utils:live_opt(Is4),
        Is6 = opt_blocks(Is5),
        Is7 = beam_utils:delete_annos(Is6),
        Is = opt_allocs(Is7),

        %% Done.
        {function,Name,Arity,CLabel,Is}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

%% blockify(Instructions0) -> Instructions
%%  Collect sequences of instructions to basic blocks.
%%  Also do some simple optimations on instructions outside the blocks.

blockify(Is) ->
    blockify(Is, []).

blockify([{loop_rec,{f,Fail},{x,0}},{loop_rec_end,_Lbl},{label,Fail}|Is], Acc) ->
    %% Useless instruction sequence.
    blockify(Is, Acc);
blockify([I|Is0]=IsAll, Acc) ->
    case collect(I) of
	error -> blockify(Is0, [I|Acc]);
	Instr when is_tuple(Instr) ->
	    {Block,Is} = collect_block(IsAll),
	    blockify(Is, [{block,Block}|Acc])
    end;
blockify([], Acc) -> reverse(Acc).

collect_block(Is) ->
    collect_block(Is, []).

collect_block([{allocate,N,R}|Is0], Acc) ->
    {Inits,Is} = lists:splitwith(fun ({init,{y,_}}) -> true;
                                     (_) -> false
                                 end, Is0),
    collect_block(Is, [{set,[],[],{alloc,R,{nozero,N,0,Inits}}}|Acc]);
collect_block([{allocate_zero,Ns,R},{test_heap,Nh,R}|Is], Acc) ->
    collect_block(Is, [{set,[],[],{alloc,R,{zero,Ns,Nh,[]}}}|Acc]);
collect_block([I|Is]=Is0, Acc) ->
    case collect(I) of
	error -> {reverse(Acc),Is0};
	Instr -> collect_block(Is, [Instr|Acc])
    end;
collect_block([], Acc) ->
    {reverse(Acc),[]}.

collect({allocate,N,R})      -> {set,[],[],{alloc,R,{nozero,N,0,[]}}};
collect({allocate_zero,N,R}) -> {set,[],[],{alloc,R,{zero,N,0,[]}}};
collect({allocate_heap,Ns,Nh,R}) -> {set,[],[],{alloc,R,{nozero,Ns,Nh,[]}}};
collect({allocate_heap_zero,Ns,Nh,R}) -> {set,[],[],{alloc,R,{zero,Ns,Nh,[]}}};
collect({init,D})            -> {set,[D],[],init};
collect({test_heap,N,R})     -> {set,[],[],{alloc,R,{nozero,nostack,N,[]}}};
collect({bif,N,F,As,D})      -> {set,[D],As,{bif,N,F}};
collect({gc_bif,N,F,R,As,D}) -> {set,[D],As,{alloc,R,{gc_bif,N,F}}};
collect({move,S,D})          -> {set,[D],[S],move};
collect({put_list,S1,S2,D})  -> {set,[D],[S1,S2],put_list};
collect({put_tuple,A,D})     -> {set,[D],[],{put_tuple,A}};
collect({put,S})             -> {set,[],[S],put};
collect({get_tuple_element,S,I,D}) -> {set,[D],[S],{get_tuple_element,I}};
collect({set_tuple_element,S,D,I}) -> {set,[],[S,D],{set_tuple_element,I}};
collect({get_list,S,D1,D2})  -> {set,[D1,D2],[S],get_list};
collect(remove_message)      -> {set,[],[],remove_message};
collect({put_map,F,Op,S,D,R,{list,Puts}}) ->
    {set,[D],[S|Puts],{alloc,R,{put_map,Op,F}}};
collect({'catch'=Op,R,L}) ->
    {set,[R],[],{try_catch,Op,L}};
collect({'try'=Op,R,L}) ->
    {set,[R],[],{try_catch,Op,L}};
collect(fclearerror)         -> {set,[],[],fclearerror};
collect({fcheckerror,{f,0}}) -> {set,[],[],fcheckerror};
collect({fmove,S,D})         -> {set,[D],[S],fmove};
collect({fconv,S,D})         -> {set,[D],[S],fconv};
collect(_)                   -> error.

%% embed_lines([Instruction]) -> [Instruction]
%%  Combine blocks that would be split by line/1 instructions.
%%  Also move a line instruction before a block into the block,
%%  but leave the line/1 instruction after a block outside.

embed_lines(Is) ->
    embed_lines(reverse(Is), []).

embed_lines([{block,B2},{line,_}=Line,{block,B1}|T], Acc) ->
    B = {block,B1++[{set,[],[],Line}]++B2},
    embed_lines([B|T], Acc);
embed_lines([{block,B1},{line,_}=Line|T], Acc) ->
    B = {block,[{set,[],[],Line}|B1]},
    embed_lines([B|T], Acc);
embed_lines([I|Is], Acc) ->
    embed_lines(Is, [I|Acc]);
embed_lines([], Acc) -> Acc.

opt_blocks([{block,Bl0}|Is]) ->
    %% The live annotation at the beginning is not useful.
    [{'%anno',_}|Bl] = Bl0,
    [{block,opt_block(Bl)}|opt_blocks(Is)];
opt_blocks([I|Is]) ->
    [I|opt_blocks(Is)];
opt_blocks([]) -> [].

opt_block(Is0) ->
    find_fixpoint(fun(Is) ->
                          opt_tuple_element(opt(Is))
                  end, Is0).

find_fixpoint(OptFun, Is0) ->
    case OptFun(Is0) of
	Is0 -> Is0;
	Is1 -> find_fixpoint(OptFun, Is1)
    end.

%% move_allocates(Is0) -> Is
%%  Move allocate instructions upwards in the instruction stream
%%  (within the same block), in the hope of getting more possibilities
%%  for optimizing away moves later.
%%
%%  For example, we can transform the following instructions:
%%
%%     get_tuple_element x(1) Element => x(2)
%%     allocate_zero StackSize 3    %% x(0), x(1), x(2) are live
%%
%%  to the following instructions:
%%
%%     allocate_zero StackSize 2    %% x(0) and x(1) are live
%%     get_tuple_element x(1) Element => x(2)
%%
%%  NOTE: Since the beam_reorder pass has been run, it is no longer
%%  safe to assume that if x(N) is initialized, then all lower-numbered
%%  x registers are also initialized.
%%
%%  For example, we must be careful when transforming the following
%%  instructions:
%%
%%     get_tuple_element x(0) Element => x(1)
%%     allocate_zero StackSize 3    %x(0), x(1), x(2) are live
%%
%%  to the following instructions:
%%
%%     allocate_zero StackSize 3
%%     get_tuple_element x(0) Element => x(1)
%%
%%  The transformation is safe if and only if x(1) has been
%%  initialized previously.  We will use the annotations added by
%%  beam_utils:anno_defs/1 to determine whether x(a) has been
%%  initialized.

move_allocates([{block,Bl0}|Is]) ->
    Bl = move_allocates_1(reverse(Bl0), []),
    [{block,Bl}|move_allocates(Is)];
move_allocates([I|Is]) ->
    [I|move_allocates(Is)];
move_allocates([]) -> [].

move_allocates_1([{'%anno',_}|Is], Acc) ->
    move_allocates_1(Is, Acc);
move_allocates_1([I|Is], [{set,[],[],{alloc,Live0,Info}}|Acc]=Acc0) ->
    case alloc_may_pass(I) of
        false ->
            move_allocates_1(Is, [I|Acc0]);
        true ->
            case alloc_live_regs(I, Is, Live0) of
                not_possible ->
                    move_allocates_1(Is, [I|Acc0]);
                Live when is_integer(Live) ->
                    A = {set,[],[],{alloc,Live,Info}},
                    move_allocates_1(Is, [A,I|Acc])
            end
    end;
move_allocates_1([I|Is], Acc) ->
    move_allocates_1(Is, [I|Acc]);
move_allocates_1([], Acc) -> Acc.

alloc_may_pass({set,_,_,{alloc,_,_}}) -> false;
alloc_may_pass({set,_,_,{set_tuple_element,_}}) -> false;
alloc_may_pass({set,_,_,put_list}) -> false;
alloc_may_pass({set,_,_,put}) -> false;
alloc_may_pass({set,_,_,_}) -> true.

%% opt([Instruction]) -> [Instruction]
%%  Optimize the instruction stream inside a basic block.

opt([{set,[X],[X],move}|Is]) -> opt(Is);
opt([{set,[X],_,move},{set,[X],_,move}=I|Is]) ->
    opt([I|Is]);
opt([{set,[{x,0}],[S1],move}=I1,{set,[D2],[{x,0}],move}|Is]) ->
    opt([I1,{set,[D2],[S1],move}|Is]);
opt([{set,[{x,0}],[S1],move}=I1,{set,[D2],[S2],move}|Is0]) when S1 =/= D2 ->
    %% Place move S x0 at the end of move sequences so that
    %% loader can merge with the following instruction
    {Ds,Is} = opt_moves([D2], Is0),
    [{set,Ds,[S2],move}|opt([I1|Is])];
opt([{set,_,_,{line,_}}=Line1,
     {set,[D1],[{integer,Idx1},Reg],{bif,element,{f,0}}}=I1,
     {set,_,_,{line,_}}=Line2,
     {set,[D2],[{integer,Idx2},Reg],{bif,element,{f,0}}}=I2|Is])
  when Idx1 < Idx2, D1 =/= D2, D1 =/= Reg, D2 =/= Reg ->
    opt([Line2,I2,Line1,I1|Is]);
opt([{set,[D1],[{integer,Idx1},Reg],{bif,element,{f,L}}}=I1,
     {set,[D2],[{integer,Idx2},Reg],{bif,element,{f,L}}}=I2|Is])
  when Idx1 < Idx2, D1 =/= D2, D1 =/= Reg, D2 =/= Reg ->
    opt([I2,I1|Is]);
opt([{set,Ds0,Ss,Op}|Is0]) ->
    {Ds,Is} = opt_moves(Ds0, Is0),
    [{set,Ds,Ss,Op}|opt(Is)];
opt([{'%anno',_}=I|Is]) ->
    [I|opt(Is)];
opt([]) -> [].

%% opt_moves([Dest], [Instruction]) -> {[Dest],[Instruction]}
%%  For each Dest, does the optimization described in opt_move/2.

opt_moves([], Is0) -> {[],Is0};
opt_moves([D0]=Ds, Is0) ->
    case opt_move(D0, Is0) of
	not_possible -> {Ds,Is0};
	{D1,Is} -> {[D1],Is}
    end;
opt_moves([X0,Y0], Is0) ->
    {X,Is2} = case opt_move(X0, Is0) of
		  not_possible -> {X0,Is0};
		  {Y0,_} -> {X0,Is0};
		  {_X1,_Is1} = XIs1 -> XIs1
	      end,
    case opt_move(Y0, Is2) of
	not_possible -> {[X,Y0],Is2};
	{X,_} -> {[X,Y0],Is2};
	{Y,Is} -> {[X,Y],Is}
    end.

%% opt_move(Dest, [Instruction]) -> {UpdatedDest,[Instruction]} | not_possible
%%  If there is a {move,Dest,FinalDest} instruction
%%  in the instruction stream, remove the move instruction
%%  and let FinalDest be the destination.

opt_move(Dest, Is) ->
    opt_move_1(Dest, Is, []).

opt_move_1(R, [{set,[D],[R],move}|Is0], Acc) ->
    %% Provided that the source register is killed by instructions
    %% that follow, the optimization is safe.
    case eliminate_use_of_from_reg(Is0, R, D, []) of
	{yes,Is} -> opt_move_rev(D, Acc, Is);
	no -> not_possible
    end;
opt_move_1(_R, [{set,_,_,{alloc,_,_}}|_], _) ->
    %% The optimization is either not possible or not safe.
    %%
    %% If R is an X register killed by allocation, the optimization is
    %% not safe. On the other hand, if the X register is killed, there
    %% will not follow a 'move' instruction with this X register as
    %% the source.
    %%
    %% If R is a Y register, the optimization is still not safe
    %% because the new target register is an X register that cannot
    %% safely pass the alloc instruction.
    not_possible;
opt_move_1(R, [{set,_,_,_}=I|Is], Acc) ->
    %% If the source register is either killed or used by this
    %% instruction, the optimimization is not possible.
    case is_killed_or_used(R, I) of
	true -> not_possible;
	false -> opt_move_1(R, Is, [I|Acc])
    end;
opt_move_1(_, _, _) ->
    not_possible.

%% opt_tuple_element([Instruction]) -> [Instruction]
%%  If possible, move get_tuple_element instructions forward
%%  in the instruction stream to a move instruction, eliminating
%%  the move instruction. Example:
%%
%%    get_tuple_element Tuple Pos Dst1
%%    ...
%%    move Dst1 Dst2
%%
%%  This code may be possible to rewrite to:
%%
%%    %%(Moved get_tuple_element instruction)
%%    ...
%%    get_tuple_element Tuple Pos Dst2
%%

opt_tuple_element([{set,[D],[S],{get_tuple_element,_}}=I|Is0]) ->
    case opt_tuple_element_1(Is0, I, {S,D}, []) of
	no ->
	    [I|opt_tuple_element(Is0)];
	{yes,Is} ->
	    opt_tuple_element(Is)
    end;
opt_tuple_element([I|Is]) ->
    [I|opt_tuple_element(Is)];
opt_tuple_element([]) -> [].

opt_tuple_element_1([{set,_,_,{alloc,_,_}}|_], _, _, _) ->
    no;
opt_tuple_element_1([{set,_,_,{try_catch,_,_}}|_], _, _, _) ->
    no;
opt_tuple_element_1([{set,[D],[S],move}|Is0], I0, {_,S}, Acc) ->
    case eliminate_use_of_from_reg(Is0, S, D, []) of
	no ->
	    no;
	{yes,Is} ->
	    {set,[S],Ss,Op} = I0,
	    I = {set,[D],Ss,Op},
	    {yes,reverse(Acc, [I|Is])}
    end;
opt_tuple_element_1([{set,Ds,Ss,_}=I|Is], MovedI, {S,D}=Regs, Acc) ->
    case member(S, Ds) orelse member(D, Ss) of
	true ->
	    no;
	false ->
	    opt_tuple_element_1(Is, MovedI, Regs, [I|Acc])
    end;
opt_tuple_element_1(_, _, _, _) -> no.

%% Reverse the instructions, while checking that there are no
%% instructions that would interfere with using the new destination
%% register (D).

opt_move_rev(D, [I|Is], Acc) ->
    case is_killed_or_used(D, I) of
	true -> not_possible;
	false -> opt_move_rev(D, Is, [I|Acc])
    end;
opt_move_rev(D, [], Acc) -> {D,Acc}.

%% is_killed_or_used(Register, {set,_,_,_}) -> bool()
%%  Test whether the register is used by the instruction.

is_killed_or_used(R, {set,Ss,Ds,_}) ->
    member(R, Ds) orelse member(R, Ss).

%% eliminate_use_of_from_reg([Instruction], FromRegister, ToRegister, Acc) ->
%%       {yes,Is} | no
%%  Eliminate any use of FromRegister in the instruction sequence
%%  by replacing uses of FromRegister with ToRegister. If FromRegister
%%  is referenced by an allocation instruction, return 'no' to indicate
%%  that FromRegister is still used and that the optimization is not
%%  possible.

eliminate_use_of_from_reg([{set,_,_,{alloc,Live,_}}|_]=Is0, {x,X}, _, Acc) ->
    if
	X < Live ->
	    no;
	true ->
	    {yes,reverse(Acc, Is0)}
    end;
eliminate_use_of_from_reg([{set,Ds,Ss0,Op}=I0|Is], From, To, Acc) ->
    I = case member(From, Ss0) of
	    true ->
		Ss = [case S of
			  From -> To;
			  _ -> S
		      end || S <- Ss0],
		{set,Ds,Ss,Op};
	    false ->
		I0
	end,
    case member(From, Ds) of
	true ->
	    {yes,reverse(Acc, [I|Is])};
	false ->
	    eliminate_use_of_from_reg(Is, From, To, [I|Acc])
    end;
eliminate_use_of_from_reg([I]=Is, From, _To, Acc) ->
    case beam_utils:is_killed_block(From, [I]) of
	true ->
	    {yes,reverse(Acc, Is)};
	false ->
	    no
    end.

%% opt_allocs(Instructions) -> Instructions.  Optimize allocate
%%  instructions inside blocks. If safe, replace an allocate_zero
%%  instruction with the slightly cheaper allocate instruction.

opt_allocs(Is) ->
    D = beam_utils:index_labels(Is),
    opt_allocs_1(Is, D).

opt_allocs_1([{block,Bl0}|Is], D) ->
    Bl = opt_alloc(Bl0, {D,Is}),
    [{block,Bl}|opt_allocs_1(Is, D)];
opt_allocs_1([I|Is], D) ->
    [I|opt_allocs_1(Is, D)];
opt_allocs_1([], _) -> [].

%% opt_alloc(Instructions) -> Instructions'
%%  Optimises all allocate instructions.

opt_alloc([{set,[],[],{alloc,Live0,Info0}},
           {set,[],[],{alloc,Live,Info}}|Is], D) ->
    Live = Live0,				%Assertion.
    Alloc = combine_alloc(Info0, Info),
    I = {set,[],[],{alloc,Live,Alloc}},
    opt_alloc([I|Is], D);
opt_alloc([{set,[],[],{alloc,R,{_,Ns,Nh,[]}}}|Is], D) ->
    [{set,[],[],opt_alloc(Is, D, Ns, Nh, R)}|Is];
opt_alloc([I|Is], D) -> [I|opt_alloc(Is, D)];
opt_alloc([], _) -> [].

combine_alloc({_,Ns,Nh1,Init}, {_,nostack,Nh2,[]})  ->
    {zero,Ns,beam_utils:combine_heap_needs(Nh1, Nh2),Init}.

%% opt_alloc(Instructions, FrameSize, HeapNeed, LivingRegs) -> [Instr]
%%  Generates the optimal sequence of instructions for
%%  allocating and initalizing the stack frame and needed heap.

opt_alloc(_Is, _D, nostack, Nh, LivingRegs) ->
    {alloc,LivingRegs,{nozero,nostack,Nh,[]}};
opt_alloc(Bl, {D,OuterIs}, Ns, Nh, LivingRegs) ->
    Is = [{block,Bl}|OuterIs],
    InitRegs = init_yregs(Ns, Is, D),
    case count_ones(InitRegs) of
	N when N*2 > Ns ->
	    {alloc,LivingRegs,{nozero,Ns,Nh,gen_init(Ns, InitRegs)}};
	_ ->
	    {alloc,LivingRegs,{zero,Ns,Nh,[]}}
    end.

gen_init(Fs, Regs) -> gen_init(Fs, Regs, 0, []).

gen_init(SameFs, _Regs, SameFs, Acc) -> reverse(Acc);
gen_init(Fs, Regs, Y, Acc) when Regs band 1 =:= 0 ->
    gen_init(Fs, Regs bsr 1, Y+1, [{init,{y,Y}}|Acc]);
gen_init(Fs, Regs, Y, Acc) ->
    gen_init(Fs, Regs bsr 1, Y+1, Acc).

init_yregs(Y, Is, D) when Y >= 0 ->
    case beam_utils:is_killed({y,Y}, Is, D) of
        true ->
            (1 bsl Y) bor init_yregs(Y-1, Is, D);
        false ->
            init_yregs(Y-1, Is, D)
    end;
init_yregs(_, _, _) -> 0.

count_ones(Bits) -> count_ones(Bits, 0).
count_ones(0, Acc) -> Acc;
count_ones(Bits, Acc) ->
    count_ones(Bits bsr 1, Acc + (Bits band 1)).

%% Calculate the new number of live registers when we move an allocate
%% instruction upwards, passing a 'set' instruction.

alloc_live_regs({set,Ds,Ss,_}, Is, Regs0) ->
    Rset = x_live(Ss, x_dead(Ds, (1 bsl Regs0)-1)),
    Live = live_regs(0, Rset),
    case ensure_contiguous(Rset, Live) of
        not_possible ->
            %% Liveness information (looking forward in the
            %% instruction stream) can't prove that moving this
            %% allocation instruction is safe. Now use the annotation
            %% of defined registers at the beginning of the current
            %% block to see whether moving would be safe.
            Def0 = defined_regs(Is, 0),
            Def = Def0 band ((1 bsl Live) - 1),
            ensure_contiguous(Rset bor Def, Live);
        Live ->
            %% Safe based on liveness information.
            Live
    end.

live_regs(N, 0) ->
    N;
live_regs(N, Regs) ->
    live_regs(N+1, Regs bsr 1).

ensure_contiguous(Regs, Live) ->
    case (1 bsl Live) - 1 of
        Regs -> Live;
        _ -> not_possible
    end.

x_dead([{x,N}|Rs], Regs) -> x_dead(Rs, Regs band (bnot (1 bsl N)));
x_dead([_|Rs], Regs) -> x_dead(Rs, Regs);
x_dead([], Regs) -> Regs.

x_live([{x,N}|Rs], Regs) -> x_live(Rs, Regs bor (1 bsl N));
x_live([_|Rs], Regs) -> x_live(Rs, Regs);
x_live([], Regs) -> Regs.

%% defined_regs(ReversedInstructions) -> RegBitmap.
%%  Given a reversed instruction stream, determine the
%%  the registers that are defined.

defined_regs([{'%anno',{def,Def}}|_], Regs) ->
    Def bor Regs;
defined_regs([{set,Ds,_,{alloc,Live,_}}|_], Regs) ->
    x_live(Ds, Regs bor ((1 bsl Live) - 1));
defined_regs([{set,Ds,_,_}|Is], Regs) ->
    defined_regs(Is, x_live(Ds, Regs)).
