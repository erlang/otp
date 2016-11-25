%% -*- erlang-indent-level: 2 -*-
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
%% Floating point handling.

-ifdef(HIPE_AMD64).
-define(HIPE_X86_X87,       hipe_amd64_x87).
-define(HIPE_X86_DEFUSE,    hipe_amd64_defuse).
-define(HIPE_X86_LIVENESS,  hipe_amd64_liveness).
-define(HIPE_X86_REGISTERS, hipe_amd64_registers).
-else.
-define(HIPE_X86_X87,       hipe_x86_x87).
-define(HIPE_X86_DEFUSE,    hipe_x86_defuse).
-define(HIPE_X86_LIVENESS,  hipe_x86_liveness).
-define(HIPE_X86_REGISTERS, hipe_x86_registers).
-endif.

-module(?HIPE_X86_X87).

-export([map/1]).

-include("../x86/hipe_x86.hrl").
-include("../main/hipe.hrl").

%%----------------------------------------------------------------------

map(CFG0) ->
  %% hipe_x86_cfg:pp(CFG0),
  Liveness = ?HIPE_X86_LIVENESS:analyse(CFG0),
  StartLabel = hipe_x86_cfg:start_label(CFG0),
  {CFG1,_} = do_blocks([], [StartLabel], CFG0, Liveness, [], gb_trees:empty()),
  CFG1.

do_blocks(Pred, [Lbl|Lbls], CFG, Liveness, Map, BlockMap) ->
  case gb_trees:lookup(Lbl, BlockMap) of
    none ->
      %% This block has not been visited.
      Block = hipe_x86_cfg:bb(CFG, Lbl),
      Succ = hipe_x86_cfg:succ(CFG, Lbl),
      NewBlockMap = gb_trees:insert(Lbl, Map, BlockMap),
      LiveOut = [X || X <- ?HIPE_X86_LIVENESS:liveout(Liveness, Lbl),
			   is_fp(X)],
      Code = hipe_bb:code(Block),
      ReverseCode = lists:reverse(Code),
      {NewCode0, NewMap, NewBlockMap1, Dirty} = 
	do_block(ReverseCode, LiveOut, Map, NewBlockMap),
      NewCFG1 =
	case Dirty of
	  true ->
	    NewBlock = hipe_bb:code_update(Block, NewCode0),
	    hipe_x86_cfg:bb_add(CFG, Lbl, NewBlock);
	  _ ->
	    CFG
	end,
      {NewCFG3, NewBlockMap2} =
	do_blocks(Lbl, Succ, NewCFG1, Liveness, NewMap, NewBlockMap1),
      do_blocks(Pred, Lbls, NewCFG3, Liveness, Map, NewBlockMap2);
    {value, fail} ->
      %% Don't have to follow this trace any longer.
      do_blocks(Pred,Lbls, CFG, Liveness, Map, BlockMap);
    {value, ExistingMap} ->
      %% This block belongs to a trace already handled.
      %% The Map coming in must be identical to the one used
      %% when the block was processed.
      if ExistingMap =:= Map -> 
	  do_blocks(Pred, Lbls, CFG, Liveness, Map, BlockMap);
	 true ->
	  NewCFG = do_shuffle(Pred, Lbl, CFG, Map, ExistingMap),
	  do_blocks(Pred, Lbls, NewCFG, Liveness, Map, BlockMap)
      end
  end;
do_blocks(_Pred, [], CFG, _Liveness, _Map, BlockMap) ->
  {CFG, BlockMap}.

do_block(Ins, LiveOut, Map, BlockMap) ->
  do_block(Ins, LiveOut, Map, BlockMap, false).

do_block([I|Is], LiveOut, Map, BlockMap, Dirty) ->
  case handle_insn(I) of
    false -> 
      {NewCode, NewMap, NewBlockMap, NewDirty} = 
	do_block(Is, LiveOut, Map, BlockMap, Dirty),
      {NewCode++[I], NewMap, NewBlockMap, NewDirty};
    true ->
      Def = ordsets:from_list(?HIPE_X86_DEFUSE:insn_def(I)),
      Use = ordsets:from_list(?HIPE_X86_DEFUSE:insn_use(I)),
      NewLiveOut = 
	ordsets:filter(fun(X) -> is_fp(X) end,
		       ordsets:union(ordsets:subtract(LiveOut, Def), Use)),
      {NewCode, NewMap, NewBlockMap, NewDirty} = 
	do_block(Is, NewLiveOut, Map, BlockMap, Dirty),
      {NewI, NewMap1, NewBlockMap1} = 
	do_insn(I, LiveOut, NewMap, NewBlockMap),
      NewDirty1 =
	if NewDirty =:= true -> true;
	   NewI =:= [I] -> false;
	   true -> true
	end,
      {NewCode++NewI, NewMap1, NewBlockMap1, NewDirty1}
  end;
do_block([], LiveOut, Map, BlockMap, Dirty) ->
  case [X || X <- Map, not lists:member(X, LiveOut)] of
    [] ->
      {[], Map, BlockMap, Dirty}; 
    Pop ->
      {PopIns, NewMap} = pop_dead(Pop, Map),
      {PopIns, NewMap, BlockMap, true}
  end.

do_shuffle(Pred, Lbl, CFG, OldMap, NewMap) ->
  %% First make sure both maps have the same members.
  Push = NewMap -- OldMap,
  Pop = OldMap -- NewMap,
  {PopInsn, OldMap0} = pop_dead(Pop, OldMap),
  {PushInsn, OldMap1} = 
    case Push of
      []-> {[], OldMap0};
      _-> push_list(lists:reverse(Push), OldMap0)
    end,
  Code =
    if OldMap1 =:= NewMap ->
	%% It was enough to push and pop.
	PopInsn ++ PushInsn ++ [hipe_x86:mk_jmp_label(Lbl)];
       true ->
	%% Shuffle the positions so the maps match
	Cycles = find_swap_cycles(OldMap1, NewMap),
	SwitchInsns = do_switching(Cycles),
	PopInsn ++ PushInsn ++ SwitchInsns ++ [hipe_x86:mk_jmp_label(Lbl)]
    end,
  %% Update the CFG.
  NewLabel = hipe_gensym:get_next_label(x86),
  NewCFG1 = hipe_x86_cfg:bb_add(CFG, NewLabel, hipe_bb:mk_bb(Code)),
  OldPred = hipe_x86_cfg:bb(NewCFG1, Pred),
  PredCode = hipe_bb:code(OldPred),
  NewLast = redirect(lists:last(PredCode), Lbl,NewLabel),
  NewPredCode = butlast(PredCode) ++ [NewLast],
  NewPredBB = hipe_bb:code_update(OldPred, NewPredCode),
  hipe_x86_cfg:bb_add(NewCFG1, Pred, NewPredBB).

find_swap_cycles(OldMap, NewMap) ->
  Moves = [get_pos(X, NewMap, 1) || X <- OldMap],
  find_swap_cycles(OldMap, Moves, lists:seq(1, length(OldMap)), []).

find_swap_cycles(OldMap, Moves, NotHandled, Cycles) ->
  if NotHandled =:= [] -> Cycles;
     true -> 
      Cycle = find_cycle(Moves, [hd(NotHandled)]),
      NewNotHandled = NotHandled -- Cycle,
      case lists:member(1, Cycle) of
	true ->
	  %% The cycle that contains the first element on the stack
	  %% must be processed last.
	  NewCycle = format_cycle(Cycle),
	  find_swap_cycles(OldMap, Moves, NewNotHandled, Cycles ++ [NewCycle]);
	_ ->
	  NewCycle = format_cycle(Cycle),
	  find_swap_cycles(OldMap, Moves, NewNotHandled, [NewCycle|Cycles])
      end
  end.

find_cycle(Moves, Cycle) ->
  To = lists:nth(lists:last(Cycle), Moves),
  if To =:= hd(Cycle) -> Cycle;
     true -> find_cycle(Moves, Cycle ++ [To])
  end.

format_cycle(C) ->
  %% The position numbers start with 1 - should start with 0.
  %% If position 0 is in the cycle it will be permuted until
  %% the 0 is first and then remove it.
  %% Otherwise the first element is also added last.
  NewCycle = [X - 1 || X <- C],
  case lists:member(0, NewCycle) of
    true -> format_cycle(NewCycle, []);
    _ -> NewCycle ++ [hd(NewCycle)]
  end.

format_cycle([H|T], NewCycle) ->
  case H of
    0 -> T ++ NewCycle;
    _ -> format_cycle(T, NewCycle ++ [H])
  end.

do_switching(Cycles) ->
  do_switching(Cycles, []).

do_switching([C|Cycles], Insns) ->
  NewInsns = Insns ++ [hipe_x86:mk_fp_unop(fxch, mk_st(X)) || X <- C],
  do_switching(Cycles, NewInsns);
do_switching([], Insns) ->
  Insns.

redirect(Insn, OldLbl, NewLbl) ->
  case Insn of
    #pseudo_call{contlab = ContLab, sdesc = SDesc} ->
      #x86_sdesc{exnlab = ExnLab} = SDesc,
      if ContLab =:= OldLbl -> 
	  Insn#pseudo_call{contlab = NewLbl};
	 ExnLab =:= OldLbl ->
	  Insn#pseudo_call{sdesc = SDesc#x86_sdesc{exnlab = NewLbl}}
      end;
    _ -> 
      hipe_x86_cfg:redirect_jmp(Insn, OldLbl, NewLbl)
  end.

do_insn(I, LiveOut, Map, BlockMap) ->
  case I of
    #pseudo_call{'fun' = Fun, contlab = ContLab} ->
      case Fun of
	%% We don't want to spill anything if an exception has been thrown.
	{_, 'handle_fp_exception'} ->
	  NewBlockMap = 
	    case gb_trees:lookup(ContLab, BlockMap) of
	      {value, fail} ->
		BlockMap;
	      {value, _} ->
		gb_trees:update(ContLab, fail, BlockMap);
	      none ->
		gb_trees:insert(ContLab, fail, BlockMap)
	    end,
	  {[I], [], NewBlockMap};
	_ ->
	  {pop_all(Map)++[I],[],BlockMap}
      end;
    #fp_unop{op = 'fwait'} ->
      Store = pseudo_pop(Map),
      {Store ++ [I], Map, BlockMap};
    #fp_unop{} ->
      {NewI, NewMap} = do_fp_unop(I, LiveOut, Map),
      {NewI, NewMap, BlockMap};
    #fp_binop{} ->
      {NewI, NewMap} = do_fp_binop(I, LiveOut, Map),
      {NewI, NewMap, BlockMap};
    #fmove{src = Src, dst = Dst} ->
      if Src =:= Dst ->
	  %% Don't need to keep this instruction!
	  %% However, we may need to pop from the stack.
	  case is_liveOut(Src, LiveOut) of
	    true->
	      {[], Map, BlockMap};
	    false ->
	      {SwitchInsn, NewMap0} = switch_first(Dst, Map),
	      NewMap = pop(NewMap0),
	      {SwitchInsn++pop_insn(), NewMap, BlockMap}
	  end;
	 true -> 
	  {NewI, NewMap} = do_fmove(Src, Dst, LiveOut, Map),
	  {NewI, NewMap, BlockMap}
      end;
    _ ->
      {[I], Map, BlockMap}
  end.

do_fmove(Src, Dst = #x86_mem{}, LiveOut, Map) ->
  %% Storing a float from the stack into memory.
  {SwitchInsn, NewMap0} = switch_first(Src, Map),
  case is_liveOut(Src, LiveOut) of
    true ->
      {SwitchInsn ++ [hipe_x86:mk_fp_unop(fst, Dst)], NewMap0};
    _ ->
      NewMap1 = pop(NewMap0),
      {SwitchInsn ++ [hipe_x86:mk_fp_unop(fstp, Dst)], NewMap1}
  end;
do_fmove(Src = #x86_mem{}, Dst, _LiveOut, Map) ->
  %% Pushing a float into the stack.
  case in_map(Dst, Map) of
    true -> ?EXIT({loadingExistingFpVariable,{Src,Dst}});
    _ -> ok
  end,
  {PushOp, [_|NewMap0]} = push(Src, Map),
  %% We want Dst in the map rather than Src.
  NewMap = [Dst|NewMap0],
  {PushOp, NewMap};
do_fmove(Src, Dst, LiveOut, Map) ->
  %% Copying a float that either is spilled or is on the fp stack,
  %% or converting a fixnum in a temp to a float on the fp stack.
  case in_map(Dst, Map) of
    true -> ?EXIT({copyingToExistingFpVariable,{Src,Dst}});
    _ -> ok
  end,
  IsConv =
    case Src of
      #x86_temp{type = Type} -> Type =/= 'double';
      _ -> false
    end,
  case IsConv of
    true ->
      do_conv(Src, Dst, Map);
    _ ->
      %% Copying.
      case {is_liveOut(Src, LiveOut), in_map(Src, Map)} of
	{false, true} ->
	  %% Just remap Dst to Src
	  {Head, [_|T]} = lists:splitwith(fun(X) -> X =/= Src end, Map),
	  {[], Head ++ [Dst|T]};
	_ ->
	  {PushOp, [_|NewMap0]} = push(Src, Map),
	  %% We want Dst in the map rather than Src.
	  NewMap = [Dst|NewMap0],
	  {PushOp, NewMap}
      end
  end.

do_conv(Src = #x86_temp{reg = Reg}, Dst, Map) ->
  %% Converting. Src must not be a register, so we 
  %% might have to put it into memory in between.
  {Move, NewSrc} = 
    case ?HIPE_X86_REGISTERS:is_precoloured(Reg) of
      true ->
	Temp = hipe_x86:mk_new_temp('untagged'),
	{[hipe_x86:mk_move(Src,Temp)], Temp};
      _ ->
	{[], Src}
    end,
  {PushOp, [_|NewMap0]} = push(NewSrc, Map),
  %% We want Dst in the map rather than NewSrc.
  NewMap = [Dst|NewMap0],
  case length(PushOp) of
    1 -> %% No popping of memory object on fpstack
      {Move ++ [hipe_x86:mk_fp_unop(fild, NewSrc)], NewMap};
    _ -> %% H contains pop instructions. Must be kept!
      Head = butlast(PushOp),
      {Move ++ Head ++ [hipe_x86:mk_fp_unop(fild, NewSrc)], NewMap}
  end.

do_fp_unop(I = #fp_unop{arg = Arg, op = fchs}, Liveout, Map) ->
  %% This is fchs, the only operation without a
  %% popping version. Needs special handling.
  case is_liveOut(Arg, Liveout) of
    true ->
      {SwitchIns, NewMap} = switch_first(Arg, Map),
      {SwitchIns ++ [I#fp_unop{arg = []}], NewMap};
    false ->
      %% Don't need to keep this instruction!
      %% However, we may need to pop Src from the stack.
      case in_map(Arg, Map) of
	true ->
	  {SwitchInsn, NewMap0} = switch_first(Arg, Map),
	  NewMap = pop(NewMap0),
	  {SwitchInsn ++ pop_insn(), NewMap};
	_ ->
	  {[],Map}
      end
  end.

do_fp_binop(#fp_binop{src = Src, dst = Dst, op = Op}, LiveOut, Map) ->
  case {is_liveOut(Src, LiveOut), is_liveOut(Dst, LiveOut)} of
    {true, true} ->
      keep_both(Op, Src, Dst, Map);
    {true, false} ->
      keep_src(Op, Src, Dst, Map);
    {false, true} ->
      keep_dst(Op, Src, Dst, Map);
    {false, false} ->
      %% Both Dst and Src are popped.
      keep_none(Op, Src, Dst, Map)
  end.

keep_both(Op, Src, Dst, Map) ->
  %% Keep both Dst and Src if it is there.
  {SwitchInsn, NewMap} = switch_first(Dst, Map),
  NewSrc = get_new_opnd(Src, NewMap),
  Insn = format_fp_binop(Op, NewSrc, mk_st(0)),
  {SwitchInsn++Insn, NewMap}.

keep_src(Op, Src, Dst, Map) ->
  %% Pop Dst but keep Src in stack if it is there.
  {SwitchInsn, NewMap0} = switch_first(Dst, Map),
  NewSrc = get_new_opnd(Src, NewMap0),
  NewMap = pop(NewMap0),
  Insn = format_fp_binop(Op, NewSrc, mk_st(0)),
  {SwitchInsn ++ Insn ++ pop_insn(), NewMap}.

keep_dst(Op, Src, Dst, Map) ->
  %% Keep Dst but pop Src.
  %% Dst must be in stack.
  DstInMap = in_map(Dst, Map),
  SrcInMap = in_map(Src, Map),
  case SrcInMap of
    true ->
      case DstInMap of
	true ->
	  %% Src must be popped. If Dst is on top of the stack we can
	  %% alter the operation rather than shuffle the stack.
	  {SwitchInsn, Insn, NewMap} =
	    if hd(Map) =:= Dst ->
		NewOp = mk_op_pop(reverse_op(Op)),
		NewDst = get_new_opnd(Src, Map),
		TmpMap = lists:map(fun(X) ->
				     if X =:= Src -> Dst; true -> X end
				   end, Map),
		{[], format_fp_binop(NewOp, mk_st(0), NewDst), pop(TmpMap)};
	       true ->
		{SwitchInsn1, NewMap0} = switch_first(Src, Map),
		NewDst = get_new_opnd(Dst,NewMap0),
		NewOp = mk_op_pop(Op),
		{SwitchInsn1,format_fp_binop(NewOp, mk_st(0), NewDst), pop(NewMap0)}
	    end,
	  {SwitchInsn ++ Insn, NewMap};
	_ ->
	  %% Src is on the stack, but Dst isn't. Use memory command to avoid
	  %% unnecessary loading instructions.
	  {SwitchInsn, NewMap0} = switch_first(Src, Map),
	  NewOp = reverse_op(Op),
	  NewMap = [Dst] ++ tl(NewMap0),
	  Insn = format_fp_binop(NewOp, Dst, mk_st(0)),
	  {SwitchInsn ++ Insn, NewMap}
      end;
    _ ->
      %% Src isn't in the map so it doesn't have to be popped.
      {SwitchInsn, NewMap} = switch_first(Dst, Map),
      {SwitchInsn ++ [#fp_unop{arg = Src, op = Op}], NewMap}
  end.

keep_none(Op, Src, Dst, Map) ->
  %% Dst must be on stack.
  {PushInsn, NewMap0} = 
    case in_map(Dst, Map) of
      true -> {[], Map};
      _ -> push(Dst, Map)
    end,
  case in_map(Src, NewMap0) of
    true ->
      %% Src must be popped.
      {SwitchInsn1, NewMap1} = switch_first(Src, NewMap0),
      NewOp = mk_op_pop(Op),
      NewDst = get_new_opnd(Dst,NewMap1),
      NewMap2 = pop(NewMap1),
      %% Then Dst has to be popped.
      {PopInsn, NewMap} = pop_member(Dst, NewMap2),
      Insn = format_fp_binop(NewOp, mk_st(0), NewDst),
      {PushInsn ++ SwitchInsn1 ++ Insn ++ PopInsn, NewMap};
    _ ->
      %% Src isn't in the map so it doesn't have to be popped.
      {SwitchInsn, NewMap1} = switch_first(Dst, NewMap0),
      NewMap = pop(NewMap1),
      {SwitchInsn ++ [#fp_unop{arg = Src, op = Op}] ++ pop_insn(), NewMap}
  end.

format_fp_binop(Op, Src = #x86_temp{}, Dst = #x86_fpreg{reg = Reg}) ->
  %% Handle that st(0) is sometimes implicit.
  if Reg =:= 0 -> [hipe_x86:mk_fp_unop(Op, Src)];
     true -> [hipe_x86:mk_fp_binop(Op, Src, Dst)]
  end;
format_fp_binop(Op, Src, Dst) ->
  [hipe_x86:mk_fp_binop(Op, Src, Dst)].

in_map(X, Map) ->
  lists:member(X, Map).

push_list(L, Map) ->
  push_list(L, Map, []).
push_list([H|T], Map, Acc) ->
  {Insn, NewMap} = push(H,Map),
  push_list(T, NewMap, Acc++Insn);
push_list([], Map, Acc) ->
  {Acc, Map}.

push(X, Map0) ->
  {PopInsn, Map} = 
    if length(Map0) > 7 -> pop_a_temp(Map0);
       true -> {[], Map0}
    end,
  NewX = get_new_opnd(X,Map),
  NewMap = [X | Map],
  PushOp = [hipe_x86:mk_fp_unop(fld, NewX)],
  {PopInsn ++ PushOp, NewMap}.

pop([_|Map]) ->
  Map.

pop_insn() ->
  [hipe_x86:mk_fp_unop('fstp',mk_st(0))].

pop_dead(Dead, Map) ->
  Dead0 = [X || X <- Map, lists:member(X,Dead)],
  pop_dead(Dead0, Map, []).

pop_dead([D|Dead], Map, Code) ->
  {I, NewMap0} = switch_first(D, Map),
  NewMap = pop(NewMap0),
  Store = case D of
	    #x86_temp{} -> [hipe_x86:mk_fp_unop('fstp', D)];
	    _ -> pop_insn()
	  end,
  pop_dead(Dead, NewMap, Code++I++Store);
pop_dead([], Map, Code) ->
  {Code,Map}.

pop_all(Map) ->
  {Code, _} = pop_dead(Map, Map),
  Code.

pop_member(Member, Map) ->
  {Head,[_|T]} = lists:splitwith(fun(X)-> X =/= Member end, Map),
  {[hipe_x86:mk_fp_unop('fstp', mk_st(get_pos(Member, Map, 0)))],
   Head++T}.

pop_a_temp(Map) ->
  Temp = find_a_temp(Map),
  {SwitchInsn, NewMap0} = switch_first(Temp, Map),
  NewMap = pop(NewMap0),
  {SwitchInsn ++ [hipe_x86:mk_fp_unop('fstp', Temp)], NewMap}.

find_a_temp([H = #x86_temp{}|_]) ->
  H;
find_a_temp([_|T]) ->
  find_a_temp(T);
find_a_temp([]) ->
  ?EXIT({noTempOnFPStack,{}}).

switch_first(X, Map = [H|_]) ->
  Pos = get_pos(X, Map, 0),
  case Pos of
    0 -> 
      {[], Map};
    notFound ->
      push(X, Map);
    _ ->	    
      {[_|Head], [_|Tail]} = lists:splitwith(fun(Y)-> Y =/= X end, Map),
      NewMap = [X|Head] ++ [H|Tail],
      Ins = hipe_x86:mk_fp_unop(fxch, mk_st(Pos)),
      {[Ins], NewMap}
  end;
switch_first(X, Map) ->
  push(X, Map).

get_pos(X, [H|T], Pos) ->
  if X =:= H -> Pos;
     true -> get_pos(X, T, Pos+1)
  end;
get_pos(_, [], _) ->
  notFound.

get_new_opnd(X, Map) ->
  I = get_pos(X, Map, 0),
  case I of
    notFound ->
      %% The operand is probably a spilled float.
      X;
    _ ->
      mk_st(I)
  end.

is_fp(#x86_fpreg{}) ->
  true;
is_fp(#x86_mem{type = Type}) ->
  Type =:= 'double';
is_fp(#x86_temp{type = Type}) ->
  Type =:= 'double'.

handle_insn(I) ->
  case I of
    #fmove{} -> true;
    #fp_unop{} -> true;
    #fp_binop{} -> true;
    #pseudo_call{} ->true;
    %% #ret{} -> true;
    _ -> false
  end.

is_liveOut(X, LiveOut) ->
  ordsets:is_element(X, LiveOut).

mk_st(X) ->
  hipe_x86:mk_fpreg(X, false).

reverse_op(Op) ->
  case Op of
    'fsub' -> 'fsubr';
    'fdiv' -> 'fdivr';
    'fsubr'-> 'fsub';
    'fdivr' -> 'fdiv';
    _ -> Op
  end.

mk_op_pop(Op) ->
  case Op of
    'fadd'-> 'faddp';
    'fdiv' -> 'fdivp';
    'fdivr' -> 'fdivrp';
    'fmul' -> 'fmulp';
    'fsub' -> 'fsubp';
    'fsubr' -> 'fsubrp';
    _ -> ?EXIT({operandHasNoPopVariant,{Op}})
  end.

butlast([X|Xs]) -> butlast(Xs,X).

butlast([],_) -> [];
butlast([X|Xs],Y) -> [Y|butlast(Xs,X)].

%%pp_insn(Op, Src, Dst) ->
%%  pp([hipe_x86:mk_fp_binop(Op, Src, Dst)]).

%%pp([I|Ins]) ->
%%  hipe_x86_pp:pp_insn(I),
%%  pp(Ins);
%%pp([]) ->
%%  [].

pseudo_pop(Map) when length(Map) > 0 ->
  Dst = hipe_x86:mk_new_temp('double'),
  pseudo_pop(Dst, length(Map), []);
pseudo_pop(_) ->
  [].

pseudo_pop(Dst, St, Acc) when St > 1 ->
  %% Store all members of the stack to a single temporary to force 
  %% any floating point overflow exceptions to occur even though we
  %% don't have overflow for the extended double precision in the x87.
  pseudo_pop(Dst, St-1, 
	     [hipe_x86:mk_fp_unop('fxch', mk_st(St-1)),
	      hipe_x86:mk_fp_unop('fst', Dst),
	      hipe_x86:mk_fp_unop('fxch', mk_st(St-1))
	      |Acc]);
pseudo_pop(Dst, _St, Acc) ->
  [hipe_x86:mk_fp_unop('fst', Dst)|Acc].
