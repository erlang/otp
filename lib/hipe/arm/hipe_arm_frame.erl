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

-module(hipe_arm_frame).
-export([frame/1]).

-include("hipe_arm.hrl").
-include("../rtl/hipe_literals.hrl").

-define(LIVENESS_ALL, hipe_arm_liveness_gpr). % since we have no FP yet

frame(CFG) ->
  Formals = fix_formals(hipe_arm_cfg:params(CFG)),
  Temps0 = all_temps(CFG, Formals),
  MinFrame = defun_minframe(CFG),
  Temps = ensure_minframe(MinFrame, Temps0),
  ClobbersLR = clobbers_lr(CFG),
  Liveness = ?LIVENESS_ALL:analyse(CFG),
  do_body(CFG, Liveness, Formals, Temps, ClobbersLR).

fix_formals(Formals) ->
  fix_formals(hipe_arm_registers:nr_args(), Formals).

fix_formals(0, Rest) -> Rest;
fix_formals(N, [_|Rest]) -> fix_formals(N-1, Rest);
fix_formals(_, []) -> [].

do_body(CFG0, Liveness, Formals, Temps, ClobbersLR) ->
  Context = mk_context(Liveness, Formals, Temps, ClobbersLR),
  CFG1 = do_blocks(CFG0, Context),
  do_prologue(CFG1, Context).

do_blocks(CFG, Context) ->
  hipe_arm_cfg:map_bbs(fun(Lbl, BB) -> do_block(Lbl, BB, Context) end, CFG).

do_block(Label, Block, Context) ->
  Liveness = context_liveness(Context),
  LiveOut = ?LIVENESS_ALL:liveout(Liveness, Label),
  Code = hipe_bb:code(Block),
  NewCode = do_block(Code, LiveOut, Context, context_framesize(Context), []),
  hipe_bb:code_update(Block, NewCode).

do_block([I|Insns], LiveOut, Context, FPoff0, RevCode) ->
  {NewIs, FPoff1} = do_insn(I, LiveOut, Context, FPoff0),
  do_block(Insns, LiveOut, Context, FPoff1, lists:reverse(NewIs, RevCode));
do_block([], _, Context, FPoff, RevCode) ->
  FPoff0 = context_framesize(Context),
  FPoff0 = FPoff,
  lists:reverse(RevCode, []).

do_insn(I, LiveOut, Context, FPoff) ->
  case I of
    #pseudo_blr{} ->
      {do_pseudo_blr(I, Context, FPoff), context_framesize(Context)};
    #pseudo_call{} ->
      do_pseudo_call(I, LiveOut, Context, FPoff);
    #pseudo_call_prepare{} ->
      do_pseudo_call_prepare(I, FPoff);
    #pseudo_move{} ->
      {do_pseudo_move(I, Context, FPoff), FPoff};
    #pseudo_spill_move{} ->
      {do_pseudo_spill_move(I, Context, FPoff), FPoff};
    #pseudo_tailcall{} ->
      {do_pseudo_tailcall(I, Context), context_framesize(Context)};
    _ ->
      {[I], FPoff}
  end.

%%%
%%% Moves, with Dst or Src possibly a pseudo
%%%

do_pseudo_move(I, Context, FPoff) ->
  Dst = hipe_arm:pseudo_move_dst(I),
  Src = hipe_arm:pseudo_move_src(I),
  case temp_is_pseudo(Dst) of
    true ->
      Offset = pseudo_offset(Dst, FPoff, Context),
      mk_store('str', Src, Offset, mk_sp(), []);
    _ ->
      case temp_is_pseudo(Src) of
	true ->
	  Offset = pseudo_offset(Src, FPoff, Context),
	  mk_load('ldr', Dst, Offset, mk_sp(), []);
	_ ->
	  [hipe_arm:mk_move(Dst, Src)]
      end
  end.

pseudo_offset(Temp, FPoff, Context) ->
  FPoff + context_offset(Context, Temp).

%%%
%%% Moves from one spill slot to another
%%%

do_pseudo_spill_move(I, Context, FPoff) ->
  #pseudo_spill_move{dst=Dst, temp=Temp, src=Src} = I,
  case temp_is_pseudo(Src) andalso temp_is_pseudo(Dst) of
    false -> % Register allocator changed its mind, turn back to move
      do_pseudo_move(hipe_arm:mk_pseudo_move(Dst, Src), Context, FPoff);
    true ->
      SrcOffset = pseudo_offset(Src, FPoff, Context),
      DstOffset = pseudo_offset(Dst, FPoff, Context),
      case SrcOffset =:= DstOffset of
	true -> []; % omit move-to-self
	false ->
	  mk_load('ldr', Temp, SrcOffset, mk_sp(),
		  mk_store('str', Temp, DstOffset, mk_sp(), []))
      end
  end.

%%%
%%% Return - deallocate frame and emit 'ret $N' insn.
%%%

do_pseudo_blr(I, Context, FPoff) ->
  %% XXX: perhaps use explicit pseudo_move;mtlr,
  %% avoiding the need to hard-code Temp1 here
  %% XXX: typically only one instruction between
  %% the mtlr and the blr, ouch
  restore_lr(FPoff, Context,
	     adjust_sp(FPoff + word_size() * context_arity(Context),
		       [I])).

restore_lr(FPoff, Context, Rest) ->
  case context_clobbers_lr(Context) of
    false -> Rest;
    true ->
      LR = hipe_arm:mk_lr(),
      mk_load('ldr', LR, FPoff - word_size(), mk_sp(),
	      Rest)
  end.

adjust_sp(N, Rest) ->
  if N =:= 0 ->
      Rest;
     true ->
      SP = mk_sp(),
      hipe_arm:mk_addi(SP, SP, N, Rest)
  end.

%%%
%%% Recursive calls.
%%%

do_pseudo_call_prepare(I, FPoff0) ->
  %% Create outgoing arguments area on the stack.
  NrStkArgs = hipe_arm:pseudo_call_prepare_nrstkargs(I),
  Offset = NrStkArgs * word_size(),
  {adjust_sp(-Offset, []), FPoff0 + Offset}.

do_pseudo_call(I, LiveOut, Context, FPoff0) ->
  #arm_sdesc{exnlab=ExnLab,arity=OrigArity} = hipe_arm:pseudo_call_sdesc(I),
  FunV = hipe_arm:pseudo_call_funv(I),
  LiveTemps = [Temp || Temp <- LiveOut, temp_is_pseudo(Temp)],
  SDesc = mk_sdesc(ExnLab, Context, LiveTemps),
  ContLab = hipe_arm:pseudo_call_contlab(I),
  Linkage = hipe_arm:pseudo_call_linkage(I),
  CallCode = [hipe_arm:mk_pseudo_call(FunV, SDesc, ContLab, Linkage)],
  StkArity = erlang:max(0, OrigArity - hipe_arm_registers:nr_args()),
  context_need_stack(Context, stack_need(FPoff0, StkArity, FunV)),
  ArgsBytes = word_size() * StkArity,
  {CallCode, FPoff0 - ArgsBytes}.

stack_need(FPoff, StkArity, FunV) ->
  case FunV of
    #arm_prim{} -> FPoff;
    #arm_mfa{m=M,f=F,a=A} ->
      case erlang:is_builtin(M, F, A) of
	true -> FPoff;
	false -> stack_need_general(FPoff, StkArity)
      end;
    _ -> stack_need_general(FPoff, StkArity)
  end.

stack_need_general(FPoff, StkArity) ->
  erlang:max(FPoff, FPoff + (?ARM_LEAF_WORDS - StkArity) * word_size()).

%%%
%%% Create stack descriptors for call sites.
%%%

mk_sdesc(ExnLab, Context, Temps) ->	% for normal calls
  Temps0 = only_tagged(Temps),
  Live = mk_live(Context, Temps0),
  Arity = context_arity(Context),
  FSize = context_framesize(Context),
  hipe_arm:mk_sdesc(ExnLab, (FSize div word_size())-1, Arity,
                    list_to_tuple(Live)).

only_tagged(Temps)->
  [X || X <- Temps, hipe_arm:temp_type(X) =:= 'tagged'].

mk_live(Context, Temps) ->
  lists:sort([temp_to_slot(Context, Temp) || Temp <- Temps]).

temp_to_slot(Context, Temp) ->
  (context_framesize(Context) + context_offset(Context, Temp))
    div word_size().

mk_minimal_sdesc(Context) ->		% for inc_stack_0 calls
  hipe_arm:mk_sdesc([], 0, context_arity(Context), {}).

%%%
%%% Tailcalls.
%%%

do_pseudo_tailcall(I, Context) -> % always at FPoff=context_framesize(Context)
  Arity = context_arity(Context),
  Args = hipe_arm:pseudo_tailcall_stkargs(I),
  FunV = hipe_arm:pseudo_tailcall_funv(I),
  Linkage = hipe_arm:pseudo_tailcall_linkage(I),
  {Insns, FPoff1} = do_tailcall_args(Args, Context),
  context_need_stack(Context, FPoff1),
  StkArity = length(Args),
  FPoff2 = FPoff1 + (Arity - StkArity) * word_size(),
  context_need_stack(Context, stack_need(FPoff2, StkArity, FunV)),
  I2 =
    case FunV of
      #arm_temp{} ->
	hipe_arm:mk_bx(FunV);
      Fun ->
	hipe_arm:mk_b_fun(Fun, Linkage)
    end,
  %% XXX: break out the LR restore, just like for pseudo_blr?
  restore_lr(context_framesize(Context), Context,
	     Insns ++ adjust_sp(FPoff2, [I2])).

do_tailcall_args(Args, Context) ->
  FPoff0 = context_framesize(Context),
  Arity = context_arity(Context),
  FrameTop = word_size()*Arity,
  DangerOff = FrameTop - word_size()*length(Args),
  %%
  Moves = mk_moves(Args, FrameTop, []),
  %%
  {Stores, Simple, Conflict} =
    split_moves(Moves, Context, DangerOff, [], [], []),
  %% sanity check (shouldn't trigger any more)
  if DangerOff < -FPoff0 ->
      exit({?MODULE,do_tailcall_args,DangerOff,-FPoff0});
     true -> []
  end,
  FPoff1 = FPoff0,
  %%
  {Pushes, Pops, FPoff2} = split_conflict(Conflict, FPoff1, [], []),
  %%
  TempReg = hipe_arm_registers:temp1(),
  %%
  {adjust_sp(-(FPoff2 - FPoff1),
	     simple_moves(Pushes, FPoff2, TempReg,
			  store_moves(Stores, FPoff2, TempReg,
				      simple_moves(Simple, FPoff2, TempReg,
						   simple_moves(Pops, FPoff2, TempReg,
								[]))))),
   FPoff2}.

mk_moves([Arg|Args], Off, Moves) ->
  Off1 = Off - word_size(),
  mk_moves(Args, Off1, [{Arg,Off1}|Moves]);
mk_moves([], _, Moves) ->
  Moves.

split_moves([Move|Moves], Context, DangerOff, Stores, Simple, Conflict) ->
  {Src,DstOff} = Move,
  case src_is_pseudo(Src) of
    false ->
      split_moves(Moves, Context, DangerOff, [Move|Stores],
		  Simple, Conflict);
    true ->
      SrcOff = context_offset(Context, Src),
      Type = typeof_temp(Src),
      if SrcOff =:= DstOff ->
	  split_moves(Moves, Context, DangerOff, Stores,
		      Simple, Conflict);
	 SrcOff >= DangerOff ->
	  split_moves(Moves, Context, DangerOff, Stores,
		      Simple, [{SrcOff,DstOff,Type}|Conflict]);
	 true ->
	  split_moves(Moves, Context, DangerOff, Stores,
		      [{SrcOff,DstOff,Type}|Simple], Conflict)
      end
  end;
split_moves([], _, _, Stores, Simple, Conflict) ->
  {Stores, Simple, Conflict}.

split_conflict([{SrcOff,DstOff,Type}|Conflict], FPoff, Pushes, Pops) ->
  FPoff1 = FPoff + word_size(),
  Push = {SrcOff,-FPoff1,Type},
  Pop = {-FPoff1,DstOff,Type},
  split_conflict(Conflict, FPoff1, [Push|Pushes], [Pop|Pops]);
split_conflict([], FPoff, Pushes, Pops) ->
  {lists:reverse(Pushes), Pops, FPoff}.

simple_moves([{SrcOff,DstOff,Type}|Moves], FPoff, TempReg, Rest) ->
  Temp = hipe_arm:mk_temp(TempReg, Type),
  SP = mk_sp(),
  LoadOff = FPoff+SrcOff,
  StoreOff = FPoff+DstOff,
  simple_moves(Moves, FPoff, TempReg,
	       mk_load('ldr', Temp, LoadOff, SP,
		       mk_store('str', Temp, StoreOff, SP,
				Rest)));
simple_moves([], _, _, Rest) ->
  Rest.

store_moves([{Src,DstOff}|Moves], FPoff, TempReg, Rest) ->
  %%Type = typeof_temp(Src),
  SP = mk_sp(),
  StoreOff = FPoff+DstOff,
  {NewSrc,FixSrc} =
    case hipe_arm:is_temp(Src) of
      true ->
	{Src, []};
      _ ->
	Temp = hipe_arm:mk_temp(TempReg, 'untagged'),
	{Temp, hipe_arm:mk_li(Temp, Src)}
    end,
  store_moves(Moves, FPoff, TempReg,
	      FixSrc ++ mk_store('str', NewSrc, StoreOff, SP, Rest));
store_moves([], _, _, Rest) ->
  Rest.

%%%
%%% Contexts
%%%

-record(context, {liveness, framesize, arity, map, clobbers_lr, ref_maxstack}).

mk_context(Liveness, Formals, Temps, ClobbersLR) ->
  {Map, MinOff} = mk_temp_map(Formals, ClobbersLR, Temps),
  FrameSize = (-MinOff),
  RefMaxStack = hipe_bifs:ref(FrameSize),
  #context{liveness=Liveness,
	   framesize=FrameSize, arity=length(Formals),
	   map=Map, clobbers_lr=ClobbersLR, ref_maxstack=RefMaxStack}.

context_need_stack(#context{ref_maxstack=RM}, N) ->
  M = hipe_bifs:ref_get(RM),
  if N > M -> hipe_bifs:ref_set(RM, N);
     true -> []
  end.

context_maxstack(#context{ref_maxstack=RM}) ->
  hipe_bifs:ref_get(RM).

context_arity(#context{arity=Arity}) ->
  Arity.

context_framesize(#context{framesize=FrameSize}) ->
  FrameSize.

context_liveness(#context{liveness=Liveness}) ->
  Liveness.

context_offset(#context{map=Map}, Temp) ->
  tmap_lookup(Map, Temp).

context_clobbers_lr(#context{clobbers_lr=ClobbersLR}) -> ClobbersLR.

mk_temp_map(Formals, ClobbersLR, Temps) ->
  {Map, 0} = enter_vars(Formals, word_size() * length(Formals),
			tmap_empty()),
  TempsList = tset_to_list(Temps),
  AllTemps =
    case ClobbersLR of
      false -> TempsList;
      true ->
	RA = hipe_arm:mk_new_temp('untagged'),
	[RA|TempsList]
    end,
  enter_vars(AllTemps, 0, Map).

enter_vars([V|Vs], PrevOff, Map) ->
  Off =
    case hipe_arm:temp_type(V) of
      'double' -> PrevOff - 2*word_size();
      _ -> PrevOff - word_size()
    end,
  enter_vars(Vs, Off, tmap_bind(Map, V, Off));
enter_vars([], Off, Map) ->
  {Map, Off}.

tmap_empty() ->
  gb_trees:empty().

tmap_bind(Map, Key, Val) ->
  gb_trees:insert(Key, Val, Map).

tmap_lookup(Map, Key) ->
  gb_trees:get(Key, Map).

%%%
%%% do_prologue: prepend stack frame allocation code.
%%%
%%% NewStart:
%%%	temp1 = *(P + P_SP_LIMIT)
%%%	temp2 = SP - MaxStack
%%%	cmp temp2, temp1
%%%	if (ltu) goto IncStack else goto AllocFrame
%%% AllocFrame:
%%%	SP = temp2		[if FrameSize == MaxStack]
%%%	SP -= FrameSize		[if FrameSize != MaxStack]
%%%	*(SP + FrameSize-WordSize) = LR		[if ClobbersLR]
%%%	goto OldStart
%%% OldStart:
%%%	...
%%% IncStack:
%%%	temp1 = LR
%%%	bl inc_stack
%%%	LR = temp1
%%%	goto NewStart

do_prologue(CFG, Context) ->
  MaxStack = context_maxstack(Context),
  if MaxStack > 0 ->
      FrameSize = context_framesize(Context),
      OldStartLab = hipe_arm_cfg:start_label(CFG),
      NewStartLab = hipe_gensym:get_next_label(arm),
      %%
      P = hipe_arm:mk_temp(hipe_arm_registers:proc_pointer(), 'untagged'),
      Temp1 = mk_temp1(),
      SP = mk_sp(),
      %%
      LR = hipe_arm:mk_lr(),
      ClobbersLR = context_clobbers_lr(Context),
      GotoOldStartCode = [hipe_arm:mk_b_label(OldStartLab)],
      AllocFrameCodeTail =
	case ClobbersLR of
	  false -> GotoOldStartCode;
	  true -> mk_store('str', LR, FrameSize-word_size(), SP, GotoOldStartCode)
	end,
      %%
      Arity = context_arity(Context),
      Guaranteed = erlang:max(0, (?ARM_LEAF_WORDS - Arity) * word_size()),
      %%
      {CFG1,NewStartCode} =
	if MaxStack =< Guaranteed ->
	    %% io:format("~w: MaxStack ~w =< Guaranteed ~w :-)\n", [?MODULE,MaxStack,Guaranteed]),
	    AllocFrameCode = adjust_sp(-FrameSize, AllocFrameCodeTail),
	    NewStartCode0 = AllocFrameCode, % no mflr needed
	    {CFG,NewStartCode0};
	   true ->
	    %% io:format("~w: MaxStack ~w > Guaranteed ~w :-(\n", [?MODULE,MaxStack,Guaranteed]),
	    AllocFrameLab = hipe_gensym:get_next_label(arm),
	    IncStackLab = hipe_gensym:get_next_label(arm),
	    Temp2 = mk_temp2(),
	    %%
	    NewStartCodeTail2 =
	      [hipe_arm:mk_pseudo_bc('lo', IncStackLab, AllocFrameLab, 0.01)],
	    NewStartCodeTail1 = NewStartCodeTail2, % no mflr needed
	    NewStartCode0 =
	      mk_load('ldr', Temp1, ?P_NSP_LIMIT, P,
		      hipe_arm:mk_addi(Temp2, SP, -MaxStack,
				       [hipe_arm:mk_cmp('cmp', Temp2, Temp1) |
					NewStartCodeTail1])),
	    %%
	    AllocFrameCode =
	      if MaxStack =:= FrameSize ->
		  %% io:format("~w: MaxStack =:= FrameSize =:= ~w :-)\n", [?MODULE,MaxStack]),
		  [hipe_arm:mk_move(SP, Temp2) |
		   AllocFrameCodeTail];
		 true ->
		  %% io:format("~w: MaxStack ~w =/= FrameSize ~w :-(\n", [?MODULE,MaxStack,FrameSize]),
		  adjust_sp(-FrameSize, AllocFrameCodeTail)
	      end,
	    %%
	    IncStackCodeTail =
	      [hipe_arm:mk_bl(hipe_arm:mk_prim('inc_stack_0'),
			      mk_minimal_sdesc(Context), not_remote),
	       hipe_arm:mk_mtlr(Temp1),
	       hipe_arm:mk_b_label(NewStartLab)],
	    IncStackCode =
	      [hipe_arm:mk_mflr(Temp1) | IncStackCodeTail], % mflr always needed
	    %%
	    CFG0a = hipe_arm_cfg:bb_add(CFG, AllocFrameLab,
					hipe_bb:mk_bb(AllocFrameCode)),
	    CFG0b = hipe_arm_cfg:bb_add(CFG0a, IncStackLab,
					hipe_bb:mk_bb(IncStackCode)),
	    %%
	    {CFG0b,NewStartCode0}
	end,
      %%
      CFG2 = hipe_arm_cfg:bb_add(CFG1, NewStartLab,
                                 hipe_bb:mk_bb(NewStartCode)),
      hipe_arm_cfg:start_label_update(CFG2, NewStartLab);
     true ->
      CFG
  end.

%%% Create a load instruction.
%%% May clobber Dst early for large offsets. In principle we could
%%% clobber TEMP2 if Dst =:= Base, but Dst =/= Base here in frame.

mk_load(LdOp, Dst, Offset, Base, Rest) ->
  hipe_arm:mk_load(LdOp, Dst, Base, Offset, 'error', Rest).

%%% Create a store instruction.
%%% May clobber TEMP2 for large offsets.

mk_store(StOp, Src, Offset, Base, Rest) ->
  hipe_arm:mk_store(StOp, Src, Base, Offset, 'temp2', Rest).

%%% typeof_temp -- what's temp's type?

typeof_temp(Temp) ->
  hipe_arm:temp_type(Temp).

%%% Cons up an 'SP' Temp.

mk_sp() ->
  hipe_arm:mk_temp(hipe_arm_registers:stack_pointer(), 'untagged').

%%% Cons up a 'TEMP1' Temp.

mk_temp1() ->
  hipe_arm:mk_temp(hipe_arm_registers:temp1(), 'untagged').

%%% Cons up a 'TEMP2' Temp.

mk_temp2() ->
  hipe_arm:mk_temp(hipe_arm_registers:temp2(), 'untagged').

%%% Check if an operand is a pseudo-Temp.

src_is_pseudo(Src) ->
  hipe_arm:is_temp(Src) andalso temp_is_pseudo(Src).

temp_is_pseudo(Temp) ->
  not(hipe_arm:temp_is_precoloured(Temp)).

%%%
%%% Detect if a Defun's body clobbers LR.
%%%

clobbers_lr(CFG) ->
  LRreg = hipe_arm_registers:lr(),
  LRtagged = hipe_arm:mk_temp(LRreg, 'tagged'),
  LRuntagged = hipe_arm:mk_temp(LRreg, 'untagged'),
  any_insn(fun(I) ->
	       Defs = hipe_arm_defuse:insn_def_gpr(I),
	       lists:member(LRtagged, Defs)
		 orelse lists:member(LRuntagged, Defs)
	   end, CFG).

any_insn(Pred, CFG) ->
  %% Abuse fold to do an efficient "any"-operation using nonlocal control flow
  FoundSatisfying = make_ref(),
  try fold_insns(fun (I, _) ->
		     case Pred(I) of
		       true -> throw(FoundSatisfying);
		       false -> false
		     end
		 end, false, CFG)
  of _ -> false
  catch FoundSatisfying -> true
  end.

%%%
%%% Build the set of all temps used in a Defun's body.
%%%

all_temps(CFG, Formals) ->
  S0 = fold_insns(fun find_temps/2, tset_empty(), CFG),
  S1 = tset_del_list(S0, Formals),
  tset_filter(S1, fun(T) -> temp_is_pseudo(T) end).

find_temps(I, S0) ->
  S1 = tset_add_list(S0, hipe_arm_defuse:insn_def_all(I)),
  tset_add_list(S1, hipe_arm_defuse:insn_use_all(I)).

fold_insns(Fun, InitAcc, CFG) ->
  hipe_arm_cfg:fold_bbs(
    fun(_, BB, Acc0) -> lists:foldl(Fun, Acc0, hipe_bb:code(BB)) end,
    InitAcc, CFG).

tset_empty() ->
  gb_sets:new().

tset_size(S) ->
  gb_sets:size(S).

tset_insert(S, T) ->
  gb_sets:add_element(T, S).

tset_add_list(S, Ts) ->
  gb_sets:union(S, gb_sets:from_list(Ts)).

tset_del_list(S, Ts) ->
  gb_sets:subtract(S, gb_sets:from_list(Ts)).

tset_filter(S, F) ->
  gb_sets:filter(F, S).

tset_to_list(S) ->
  gb_sets:to_list(S).

%%%
%%% Compute minimum permissible frame size, ignoring spilled temps.
%%% This is done to ensure that we won't have to adjust the frame size
%%% in the middle of a tailcall.
%%%

defun_minframe(CFG) ->
  MaxTailArity = fold_insns(fun insn_mta/2, 0, CFG),
  MyArity = length(fix_formals(hipe_arm_cfg:params(CFG))),
  erlang:max(MaxTailArity - MyArity, 0).

insn_mta(I, MTA) ->
  case I of
    #pseudo_tailcall{arity=Arity} ->
      erlang:max(MTA, Arity - hipe_arm_registers:nr_args());
    _ -> MTA
  end.

%%%
%%% Ensure that we have enough temps to satisfy the minimum frame size,
%%% if necessary by prepending unused dummy temps.
%%%

ensure_minframe(MinFrame, Temps) ->
  ensure_minframe(MinFrame, tset_size(Temps), Temps).

ensure_minframe(MinFrame, Frame, Temps) ->
  if MinFrame > Frame ->
      Temp = hipe_arm:mk_new_temp('untagged'),
      ensure_minframe(MinFrame, Frame+1, tset_insert(Temps, Temp));
     true -> Temps
  end.

word_size() ->
  4.
