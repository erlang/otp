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
%%     $Id: beam_block.erl,v 1.1 2008/12/17 09:53:41 mikpe Exp $
%%
%% Purpose : Partitions assembly instructions into basic blocks and
%% optimizes them.

-module(beam_block).

-export([module/2]).
-export([live_at_entry/1]).			%Used by beam_type, beam_bool.
-export([is_killed/2]).				%Used by beam_dead, beam_type, beam_bool.
-export([is_not_used/2]).			%Used by beam_bool.
-export([merge_blocks/2]).			%Used by beam_jump.
-import(lists, [map/2,mapfoldr/3,reverse/1,reverse/2,foldl/3,
		member/2,sort/1,all/2]).
-define(MAXREG, 1024).

module({Mod,Exp,Attr,Fs,Lc}, _Opt) ->
    {ok,{Mod,Exp,Attr,map(fun function/1, Fs),Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    %% Collect basic blocks and optimize them.
    Is = blockify(Is0),

    %% Done.
    {function,Name,Arity,CLabel,Is}.

%% blockify(Instructions0) -> Instructions
%%  Collect sequences of instructions to basic blocks and
%%  optimize the contents of the blocks. Also do some simple
%%  optimations on instructions outside the blocks.

blockify(Is) ->
    blockify(Is, []).

blockify([{loop_rec,{f,Fail},{x,0}},{loop_rec_end,_Lbl},{label,Fail}|Is], Acc) ->
    %% Useless instruction sequence.
    blockify(Is, Acc);
blockify([{test,bs_test_tail,F,[Bits]}|Is],
	 [{test,bs_skip_bits,F,[{integer,I},Unit,_Flags]}|Acc]) ->
    blockify(Is, [{test,bs_test_tail,F,[Bits+I*Unit]}|Acc]);
blockify([{test,bs_skip_bits,F,[{integer,I1},Unit1,_]}|Is],
	 [{test,bs_skip_bits,F,[{integer,I2},Unit2,Flags]}|Acc]) ->
    blockify(Is, [{test,bs_skip_bits,F,
		   [{integer,I1*Unit1+I2*Unit2},1,Flags]}|Acc]);
blockify([{test,is_atom,{f,Fail},[Reg]}=I|
	  [{select_val,Reg,{f,Fail},
	    {list,[{atom,false},{f,_}=BrFalse,
		   {atom,true}=AtomTrue,{f,_}=BrTrue]}}|Is]=Is0],
	 [{block,Bl}|_]=Acc) ->
    case is_last_bool(Bl, Reg) of
	false ->
	    blockify(Is0, [I|Acc]);
	true ->
	    blockify(Is, [{jump,BrTrue},
			  {test,is_eq_exact,BrFalse,[Reg,AtomTrue]}|Acc])
    end;
blockify([{test,is_atom,{f,Fail},[Reg]}=I|
	  [{select_val,Reg,{f,Fail},
	    {list,[{atom,true}=AtomTrue,{f,_}=BrTrue,
		   {atom,false},{f,_}=BrFalse]}}|Is]=Is0],
	 [{block,Bl}|_]=Acc) ->
    case is_last_bool(Bl, Reg) of
	false ->
	    blockify(Is0, [I|Acc]);
	true ->
	    blockify(Is, [{jump,BrTrue},
			  {test,is_eq_exact,BrFalse,[Reg,AtomTrue]}|Acc])
    end;
blockify([I|Is0]=IsAll, Acc) ->
    case is_bs_put(I) of
	true ->
	    {BsPuts0,Is} = collect_bs_puts(IsAll),
	    BsPuts = opt_bs_puts(BsPuts0),
	    blockify(Is, reverse(BsPuts, Acc));
	false ->
	    case collect(I) of
		error -> blockify(Is0, [I|Acc]);
		Instr when is_tuple(Instr) ->
		    {Block0,Is} = collect_block(IsAll),
		    Block = opt_block(Block0),
		    blockify(Is, [{block,Block}|Acc])
	    end
    end;
blockify([], Acc) -> reverse(Acc).

is_last_bool([I,{'%live',_}], Reg) ->
    is_last_bool([I], Reg);
is_last_bool([{set,[Reg],As,{bif,N,_}}], Reg) ->
    Ar = length(As),
    erl_internal:new_type_test(N, Ar) orelse erl_internal:comp_op(N, Ar)
	orelse erl_internal:bool_op(N, Ar);
is_last_bool([_|Is], Reg) -> is_last_bool(Is, Reg);
is_last_bool([], _) -> false.

collect_block(Is) ->
    collect_block(Is, []).

collect_block([{allocate_zero,Ns,R},{test_heap,Nh,R}|Is], Acc) ->
    collect_block(Is, [{allocate,R,{no_opt,Ns,Nh,[]}}|Acc]);
collect_block([I|Is]=Is0, Acc) ->
    case collect(I) of
	error -> {reverse(Acc),Is0};
	Instr -> collect_block(Is, [Instr|Acc])
    end;
collect_block([], Acc) -> {reverse(Acc),[]}.

collect({allocate_zero,N,R}) -> {allocate,R,{zero,N,0,[]}};
collect({test_heap,N,R})     -> {allocate,R,{nozero,nostack,N,[]}};
collect({bif,N,nofail,As,D}) -> {set,[D],As,{bif,N}};
collect({bif,N,F,As,D})      -> {set,[D],As,{bif,N,F}};
collect({move,S,D})          -> {set,[D],[S],move};
collect({put_list,S1,S2,D})  -> {set,[D],[S1,S2],put_list};
collect({put_tuple,A,D})     -> {set,[D],[],{put_tuple,A}};
collect({put,S})             -> {set,[],[S],put};
collect({put_string,L,S,D})  -> {set,[D],[],{put_string,L,S}};
collect({get_tuple_element,S,I,D}) -> {set,[D],[S],{get_tuple_element,I}};
collect({set_tuple_element,S,D,I}) -> {set,[],[S,D],{set_tuple_element,I}};
collect({get_list,S,D1,D2})  -> {set,[D1,D2],[S],get_list};
collect(remove_message)      -> {set,[],[],remove_message};
collect({'catch',R,L})       -> {set,[R],[],{'catch',L}};
collect({'%live',_}=Live)    -> Live;
collect(_)                   -> error.

opt_block(Is0) ->
    %% We explicitly move any allocate instruction upwards before optimising
    %% moves, to avoid any potential problems with the calculation of live
    %% registers.
    Is1 = find_fixpoint(fun move_allocates/1, Is0),
    Is2 = find_fixpoint(fun opt/1, Is1),
    Is = opt_alloc(Is2),
    share_floats(Is).

find_fixpoint(OptFun, Is0) ->
    case OptFun(Is0) of
	Is0 -> Is0;
	Is1 -> find_fixpoint(OptFun, Is1)
    end.

move_allocates([{set,_Ds,_Ss,{set_tuple_element,_}}|_]=Is) -> Is;
move_allocates([{set,Ds,Ss,_Op}=Set,{allocate,R,Alloc}|Is]) when is_integer(R) ->
    [{allocate,live_regs(Ds, Ss, R),Alloc},Set|Is];
move_allocates([{allocate,R1,Alloc1},{allocate,R2,Alloc2}|Is]) ->
    R1 = R2,					% Assertion.
    move_allocates([{allocate,R1,combine_alloc(Alloc1, Alloc2)}|Is]);
move_allocates([I|Is]) ->
    [I|move_allocates(Is)];
move_allocates([]) -> [].

combine_alloc({_,Ns,Nh1,Init}, {_,nostack,Nh2,[]}) ->
    {zero,Ns,Nh1+Nh2,Init}.

merge_blocks([{allocate,R,{Attr,Ns,Nh1,Init}}|B1],
	     [{allocate,_,{_,nostack,Nh2,[]}}|B2]) ->
    Alloc = {allocate,R,{Attr,Ns,Nh1+Nh2,Init}},
    [Alloc|merge_blocks(B1, B2)];
merge_blocks(B1, B2) -> merge_blocks_1(B1++[{set,[],[],stop_here}|B2]).

merge_blocks_1([{set,[],_,stop_here}|Is]) -> Is;
merge_blocks_1([{set,[D],_,move}=I|Is]) ->
    case is_killed(D, Is) of
	true -> merge_blocks_1(Is);
	false -> [I|merge_blocks_1(Is)]
    end;
merge_blocks_1([I|Is]) -> [I|merge_blocks_1(Is)].

opt([{set,[Dst],As,{bif,Bif,Fail}}=I1,
     {set,[Dst],[Dst],{bif,'not',Fail}}=I2|Is]) ->
    %% Get rid of the 'not' if the operation can be inverted.
    case inverse_comp_op(Bif) of
	none -> [I1,I2|opt(Is)];
	RevBif -> [{set,[Dst],As,{bif,RevBif,Fail}}|opt(Is)]
    end;
opt([{set,[X],[X],move}|Is]) -> opt(Is);
opt([{set,[D1],[{integer,Idx1},Reg],{bif,element,{f,0}}}=I1,
     {set,[D2],[{integer,Idx2},Reg],{bif,element,{f,0}}}=I2|Is])
  when Idx1 < Idx2, D1 =/= D2, D1 =/= Reg, D2 =/= Reg ->
    opt([I2,I1|Is]);
opt([{set,Ds0,Ss,Op}|Is0]) ->
    {Ds,Is} =  opt_moves(Ds0, Is0),
    [{set,Ds,Ss,Op}|opt(Is)];
opt([I|Is]) -> [I|opt(Is)];
opt([]) -> [].

opt_moves([], Is0) -> {[],Is0};
opt_moves([D0], Is0) ->
    {D1,Is1} = opt_move(D0, Is0),
    {[D1],Is1};
opt_moves([X0,Y0]=Ds, Is0) ->
    {X1,Is1} = opt_move(X0, Is0),
    case opt_move(Y0, Is1) of
	{Y1,Is2} when X1 =/= Y1 -> {[X1,Y1],Is2};
	_Other when X1 =/= Y0 -> {[X1,Y0],Is1};
	_Other -> {Ds,Is0}
    end.

opt_move(R, [{set,[D],[R],move}|Is]=Is0) ->
    case is_killed(R, Is) of
	true -> {D,Is};
	false -> {R,Is0}
    end;
opt_move(R, [I|Is0]) ->
    case is_transparent(R, I) of
	true ->
	    {D,Is1} = opt_move(R, Is0),
	    case is_transparent(D, I) of
		true ->  {D,[I|Is1]};
		false -> {R,[I|Is0]}
	    end;
	false -> {R,[I|Is0]}
    end;
opt_move(R, []) -> {R,[]}.

is_transparent(R, {set,Ds,Ss,_Op}) ->
    case member(R, Ds) of
	true -> false;
	false -> not member(R, Ss)
    end;
is_transparent(_, _) -> false.

%% is_killed(Register, [Instruction]) -> true|false
%%  Determine whether a register is killed by the instruction sequence.
%%  If true is returned, it means that the register will not be
%%  referenced in ANY way (not even indirectly by an allocate instruction);
%%  i.e. it is OK to enter the instruction sequence with Register
%%  containing garbage.

is_killed({x,N}=R, [{block,Blk}|Is]) ->
    case is_killed(R, Blk) of
	true -> true;
	false ->
	    %% Before looking beyond the block, we must be
	    %% sure that the register is not referenced by
	    %% any allocate instruction in the block.
	    case all(fun({allocate,Live,_}) when N < Live -> false;
			(_) -> true
		     end, Blk) of
		true -> is_killed(R, Is);
		false -> false
	    end
    end;
is_killed(R, [{block,Blk}|Is]) ->
    case is_killed(R, Blk) of
	true -> true;
	false -> is_killed(R, Is)
    end;
is_killed(R, [{set,Ds,Ss,_Op}|Is]) ->
    case member(R, Ss) of
	true -> false;
	false ->
	    case member(R, Ds) of
		true -> true;
		false -> is_killed(R, Is)
	    end
    end;
is_killed(R, [{case_end,Used}|_]) -> R =/= Used;
is_killed(R, [{badmatch,Used}|_]) -> R =/= Used;
is_killed(_, [if_end|_]) -> true;
is_killed(R, [{func_info,_,_,Ar}|_]) ->
    case R of
	{x,X} when X < Ar -> false;
	_ -> true
    end;
is_killed(R, [{kill,R}|_]) -> true;
is_killed(R, [{kill,_}|Is]) -> is_killed(R, Is);
is_killed(R, [{bs_init2,_,_,_,_,_,Dst}|Is]) ->
    if
	R =:= Dst -> true;
	true -> is_killed(R, Is)
    end;
is_killed(R, [{bs_put_string,_,_}|Is]) -> is_killed(R, Is);
is_killed({x,R}, [{'%live',Live}|_]) when R >= Live -> true;
is_killed({x,R}, [{'%live',_}|Is]) -> is_killed(R, Is);
is_killed({x,R}, [{allocate,Live,_}|_]) ->
    %% Note: To be safe here, we must return either true or false,
    %% not looking further at the instructions beyond the allocate
    %% instruction.
    R >= Live;
is_killed({x,R}, [{call,Live,_}|_]) when R >= Live -> true;
is_killed({x,R}, [{call_last,Live,_,_}|_]) when R >= Live -> true;
is_killed({x,R}, [{call_only,Live,_}|_]) when R >= Live -> true;
is_killed({x,R}, [{call_ext,Live,_}|_]) when R >= Live -> true;
is_killed({x,R}, [{call_ext_last,Live,_,_}|_]) when R >= Live -> true;
is_killed({x,R}, [{call_ext_only,Live,_}|_]) when R >= Live -> true;
is_killed({x,R}, [return|_]) when R > 0 -> true;
is_killed(_, _) -> false.

%% is_not_used(Register, [Instruction]) -> true|false
%%  Determine whether a register is used by the instruction sequence.
%%  If true is returned, it means that the register will not be
%%  referenced directly, but it may be referenced by an allocate
%%  instruction (meaning that it is NOT allowed to contain garbage).

is_not_used(R, [{block,Blk}|Is]) ->
    case is_not_used(R, Blk) of
	true -> true;
	false -> is_not_used(R, Is)
    end;
is_not_used({x,R}=Reg, [{allocate,Live,_}|Is]) ->
    if
	R >= Live -> true;
	true -> is_not_used(Reg, Is)
    end;
is_not_used(R, [{set,Ds,Ss,_Op}|Is]) ->
    case member(R, Ss) of
	true -> false;
	false ->
	    case member(R, Ds) of
		true -> true;
		false -> is_not_used(R, Is)
	    end
    end;
is_not_used(R, Is) -> is_killed(R, Is).

%% opt_alloc(Instructions) -> Instructions'
%%  Optimises all allocate instructions.

opt_alloc([{allocate,R,{_,Ns,Nh,[]}}|Is]) ->
    [opt_alloc(Is, Ns, Nh, R)|opt(Is)];
opt_alloc([I|Is]) -> [I|opt_alloc(Is)];
opt_alloc([]) -> [].

%% opt_alloc(Instructions, FrameSize, HeapNeed, LivingRegs) -> [Instr]
%%  Generates the optimal sequence of instructions for
%%  allocating and initalizing the stack frame and needed heap.

opt_alloc(_Is, nostack, Nh, LivingRegs) ->
    {allocate,LivingRegs,{nozero,nostack,Nh,[]}};
opt_alloc(Is, Ns, Nh, LivingRegs) ->
    InitRegs = init_yreg(Is, 0),
    case count_ones(InitRegs) of
	N when N*2 > Ns ->
	    {allocate,LivingRegs,{nozero,Ns,Nh,gen_init(Ns, InitRegs)}};
	_ ->
	    {allocate,LivingRegs,{zero,Ns,Nh,[]}}
    end.

gen_init(Fs, Regs) -> gen_init(Fs, Regs, 0, []).

gen_init(SameFs, _Regs, SameFs, Acc) -> reverse(Acc);
gen_init(Fs, Regs, Y, Acc) when Regs band 1 == 0 ->
    gen_init(Fs, Regs bsr 1, Y+1, [{init, {y,Y}}|Acc]);
gen_init(Fs, Regs, Y, Acc) ->
    gen_init(Fs, Regs bsr 1, Y+1, Acc).

%% init_yreg(Instructions, RegSet) -> RegSetInitialized
%%  Calculate the set of initialized y registers.

init_yreg([{set,_,_,{bif,_,_}}|_], Reg) -> Reg;
init_yreg([{set,Ds,_,_}|Is], Reg) -> init_yreg(Is, add_yregs(Ds, Reg));
init_yreg(_Is, Reg) -> Reg.

add_yregs(Ys, Reg) -> foldl(fun(Y, R0) -> add_yreg(Y, R0) end, Reg, Ys).

add_yreg({y,Y}, Reg) -> Reg bor (1 bsl Y);
add_yreg(_, Reg)     -> Reg.

count_ones(Bits) -> count_ones(Bits, 0).
count_ones(0, Acc) -> Acc;
count_ones(Bits, Acc) ->
    count_ones(Bits bsr 1, Acc + (Bits band 1)).

%% live_at_entry(Is) -> NumberOfRegisters
%%  Calculate the number of register live at the entry to the code
%%  sequence.

live_at_entry([{block,[{allocate,R,_}|_]}|_]) ->
    R;
live_at_entry([{label,_}|Is]) ->
    live_at_entry(Is);
live_at_entry([{block,Bl}|_]) ->
    live_at_entry(Bl);
live_at_entry([{func_info,_,_,Ar}|_]) ->
    Ar;
live_at_entry(Is0) ->
    case reverse(Is0) of
	[{'%live',Regs}|Is] -> live_at_entry_1(Is, (1 bsl Regs)-1);
	_ -> unknown
    end.

live_at_entry_1([{set,Ds,Ss,_}|Is], Rset0) ->
    Rset = x_live(Ss, x_dead(Ds, Rset0)),
    live_at_entry_1(Is, Rset);
live_at_entry_1([{allocate,_,_}|Is], Rset) ->
    live_at_entry_1(Is, Rset);
live_at_entry_1([], Rset) -> live_regs_1(0, Rset).

%% Calculate the new number of live registers when we move an allocate
%% instruction upwards, passing a 'set' instruction.

live_regs(Ds, Ss, Regs0) ->
    Rset = x_live(Ss, x_dead(Ds, (1 bsl Regs0)-1)),
    live_regs_1(0, Rset).

live_regs_1(N, 0) -> N;
live_regs_1(N, Regs) -> live_regs_1(N+1, Regs bsr 1).

x_dead([{x,N}|Rs], Regs) -> x_dead(Rs, Regs band (bnot (1 bsl N)));
x_dead([_|Rs], Regs) -> x_dead(Rs, Regs);
x_dead([], Regs) -> Regs.

x_live([{x,N}|Rs], Regs) -> x_live(Rs, Regs bor (1 bsl N));
x_live([_|Rs], Regs) -> x_live(Rs, Regs);
x_live([], Regs) -> Regs.

%%
%% If a floating point literal occurs more than once, move it into
%% a free register and re-use it.
%%

share_floats([{allocate,_,_}=Alloc|Is]) ->
    [Alloc|share_floats(Is)];
share_floats(Is0) ->
    All = get_floats(Is0, []),
    MoreThanOnce0 =  more_than_once(sort(All), gb_sets:empty()),
    case gb_sets:is_empty(MoreThanOnce0) of
	true -> Is0;
	false ->
	    MoreThanOnce = gb_sets:to_list(MoreThanOnce0),
	    FreeX = highest_used(Is0, -1) + 1,
	    Regs0 = make_reg_map(MoreThanOnce, FreeX, []),
	    Regs = gb_trees:from_orddict(Regs0),
	    Is = map(fun({set,Ds,[{float,F}],Op}=I) ->
			     case gb_trees:lookup(F, Regs) of
				 none -> I;
				 {value,R} -> {set,Ds,[R],Op}
			     end;
			(I) -> I
		     end, Is0),
	    [{set,[R],[{float,F}],move} || {F,R} <- Regs0] ++ Is
    end.

get_floats([{set,_,[{float,F}],_}|Is], Acc) ->
    get_floats(Is, [F|Acc]);
get_floats([_|Is], Acc) ->
    get_floats(Is, Acc);
get_floats([], Acc) -> Acc.

more_than_once([F,F|Fs], Set) ->
    more_than_once(Fs, gb_sets:add(F, Set));
more_than_once([_|Fs], Set) ->
    more_than_once(Fs, Set);
more_than_once([], Set) -> Set.

highest_used([{set,Ds,Ss,_}|Is], High) ->
    highest_used(Is, highest(Ds, highest(Ss, High)));
highest_used([{'%live',Live}|Is], High) when Live > High ->
    highest_used(Is, Live);
highest_used([_|Is], High) ->
    highest_used(Is, High);
highest_used([], High) -> High.

highest([{x,R}|Rs], High) when R > High ->
    highest(Rs, R);
highest([_|Rs], High) ->
    highest(Rs, High);
highest([], High) -> High.

make_reg_map([F|Fs], R, Acc) when R < ?MAXREG ->
    make_reg_map(Fs, R+1, [{F,{x,R}}|Acc]);
make_reg_map(_, _, Acc) -> sort(Acc).

%% inverse_comp_op(Op) -> none|RevOp

inverse_comp_op('=:=') -> '=/=';
inverse_comp_op('=/=') -> '=:=';
inverse_comp_op('==') -> '/=';
inverse_comp_op('/=') -> '==';
inverse_comp_op('>') -> '=<';
inverse_comp_op('<') -> '>=';
inverse_comp_op('>=') -> '<';
inverse_comp_op('=<') -> '>';
inverse_comp_op(_) -> none.

%%%
%%% Evaluation of constant bit fields.
%%%

is_bs_put({bs_put_integer,_,_,_,_,_}) -> true;
is_bs_put({bs_put_float,_,_,_,_,_}) -> true;
is_bs_put(_) -> false.

collect_bs_puts(Is) ->
    collect_bs_puts_1(Is, []).

collect_bs_puts_1([I|Is]=Is0, Acc) ->
    case is_bs_put(I) of
	false -> {reverse(Acc),Is0};
	true -> collect_bs_puts_1(Is, [I|Acc])
    end;
collect_bs_puts_1([], Acc) -> {reverse(Acc),[]}.

opt_bs_puts(Is) ->
    opt_bs_1(Is, []).

opt_bs_1([{bs_put_float,Fail,{integer,Sz},1,Flags0,Src}=I0|Is], Acc) ->
    case catch eval_put_float(Src, Sz, Flags0) of
	{'EXIT',_} ->
	    opt_bs_1(Is, [I0|Acc]);
	<<Int:Sz>> ->
	    Flags = force_big(Flags0),
	    I = {bs_put_integer,Fail,{integer,Sz},1,Flags,{integer,Int}},
	    opt_bs_1([I|Is], Acc)
    end;
opt_bs_1([{bs_put_integer,_,{integer,8},1,_,{integer,_}}|_]=IsAll, Acc0) ->
    {Is,Acc} = bs_collect_string(IsAll, Acc0),
    opt_bs_1(Is, Acc);
opt_bs_1([{bs_put_integer,Fail,{integer,Sz},1,F,{integer,N}}=I|Is0], Acc) when Sz > 8 ->
    case field_endian(F) of
	big ->
	    case bs_split_int(N, Sz, Fail, Is0) of
		no_split -> opt_bs_1(Is0, [I|Acc]);
		Is -> opt_bs_1(Is, Acc)
	    end;
	little ->
	    case catch <<N:Sz/little>> of
		{'EXIT',_} ->
		    opt_bs_1(Is0, [I|Acc]);
		<<Int:Sz>> ->
		    Flags = force_big(F),
		    Is = [{bs_put_integer,Fail,{integer,Sz},1,
			   Flags,{integer,Int}}|Is0],
		    opt_bs_1(Is, Acc)
	    end;
	native -> opt_bs_1(Is0, [I|Acc])
    end;
opt_bs_1([{Op,Fail,{integer,Sz},U,F,Src}|Is], Acc) when U > 1 ->
    opt_bs_1([{Op,Fail,{integer,U*Sz},1,F,Src}|Is], Acc);
opt_bs_1([I|Is], Acc) ->
    opt_bs_1(Is, [I|Acc]);
opt_bs_1([], Acc) -> reverse(Acc).

eval_put_float(Src, Sz, Flags) ->
    Val = value(Src),
    case field_endian(Flags) of
	little -> <<Val:Sz/little-float-unit:1>>;
	big -> <<Val:Sz/big-float-unit:1>>
        %% native intentionally not handled here - we can't optimize it.
    end.

value({integer,I}) -> I;
value({float,F}) -> F;
value({atom,A}) -> A.

bs_collect_string(Is, [{bs_put_string,Len,{string,Str}}|Acc]) ->
    bs_coll_str_1(Is, Len, reverse(Str), Acc);
bs_collect_string(Is, Acc) ->
    bs_coll_str_1(Is, 0, [], Acc).

bs_coll_str_1([{bs_put_integer,_,{integer,Sz},U,_,{integer,V}}|Is],
	      Len, StrAcc, IsAcc) when U*Sz =:= 8 ->
    Byte = V band 16#FF,
    bs_coll_str_1(Is, Len+1, [Byte|StrAcc], IsAcc);
bs_coll_str_1(Is, Len, StrAcc, IsAcc) ->
    {Is,[{bs_put_string,Len,{string,reverse(StrAcc)}}|IsAcc]}.

field_endian({field_flags,F}) -> field_endian_1(F).

field_endian_1([big=E|_]) -> E;
field_endian_1([little=E|_]) -> E;
field_endian_1([native=E|_]) -> E;
field_endian_1([_|Fs]) -> field_endian_1(Fs).

force_big({field_flags,F}) ->
    {field_flags,force_big_1(F)}.

force_big_1([big|_]=Fs) -> Fs;
force_big_1([little|Fs]) -> [big|Fs];
force_big_1([F|Fs]) -> [F|force_big_1(Fs)].

bs_split_int(0, Sz, _, _) when Sz > 64 ->
    %% We don't want to split in this case because the
    %% string will consist of only zeroes.
    no_split;
bs_split_int(N, Sz, Fail, Acc) ->
    FirstByteSz = case Sz rem 8 of
		      0 -> 8;
		      Rem -> Rem
		  end,
    bs_split_int_1(N, FirstByteSz, Sz, Fail, Acc).

bs_split_int_1(N, ByteSz, Sz, Fail, Acc) when Sz > 0 ->
    Mask = (1 bsl ByteSz) - 1,
    I = {bs_put_integer,Fail,{integer,ByteSz},1,
	 {field_flags,[big]},{integer,N band Mask}},
    bs_split_int_1(N bsr ByteSz, 8, Sz-ByteSz, Fail, [I|Acc]);
bs_split_int_1(_, _, _, _, Acc) -> Acc.
