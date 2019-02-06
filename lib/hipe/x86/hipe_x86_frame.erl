%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% x86 stack frame handling
%%%
%%% - map non-register temps to stack slots
%%% - add explicit stack management code to prologue and epilogue,
%%%   and at calls and tailcalls
%%%
%%% TODO:
%%% - Compute max stack in a pre-pass? (get rid of ref cell updates)
%%% - Merge all_temps and defun_minframe to a single
%%%   pass, for compile-time efficiency reasons.

-ifdef(HIPE_AMD64).
-define(HIPE_X86_FRAME,     hipe_amd64_frame).
-define(HIPE_X86_REGISTERS, hipe_amd64_registers).
-define(HIPE_X86_LIVENESS,  hipe_amd64_liveness).
-define(LEAF_WORDS,	    ?AMD64_LEAF_WORDS).
-else.
-define(HIPE_X86_FRAME,     hipe_x86_frame).
-define(HIPE_X86_REGISTERS, hipe_x86_registers).
-define(HIPE_X86_LIVENESS,  hipe_x86_liveness).
-define(LEAF_WORDS,	    ?X86_LEAF_WORDS).
-endif.

-module(?HIPE_X86_FRAME).
-export([frame/2]).
-include("../x86/hipe_x86.hrl").
-include("../rtl/hipe_literals.hrl").

frame(CFG0, _Options) ->
  Formals = fix_formals(hipe_x86_cfg:params(CFG0)),
  Temps0 = all_temps(CFG0, Formals),
  MinFrame = defun_minframe(CFG0),
  Temps = ensure_minframe(MinFrame, Temps0),
  Liveness = ?HIPE_X86_LIVENESS:analyse(CFG0),
  do_body(CFG0, Liveness, Formals, Temps).

fix_formals(Formals) ->
  fix_formals(?HIPE_X86_REGISTERS:nr_args(), Formals).

fix_formals(0, Rest) -> Rest;
fix_formals(N, [_|Rest]) -> fix_formals(N-1, Rest);
fix_formals(_, []) -> [].

do_body(CFG0, Liveness, Formals, Temps) ->
  Context = mk_context(Liveness, Formals, Temps),
  CFG1 = do_blocks(CFG0, Context),
  do_prologue(CFG1, Context).

do_blocks(CFG, Context) ->
  hipe_x86_cfg:map_bbs(fun(Lbl, BB) -> do_block(Lbl, BB, Context) end, CFG).

do_block(Label, Block, Context) ->
  Liveness = context_liveness(Context),
  LiveOut = ?HIPE_X86_LIVENESS:liveout(Liveness, Label),
  Code = hipe_bb:code(Block),
  NewCode = do_block(Code, LiveOut, Context, context_framesize(Context), []),
  hipe_bb:code_update(Block, NewCode).

do_block([I|Insns], LiveOut, Context, FPoff0, RevCode) ->
  {NewIs, FPoff1} = do_insn(I, LiveOut, Context, FPoff0),
  do_block(Insns, LiveOut, Context, FPoff1, lists:reverse(NewIs, RevCode));
do_block([], _, Context, FPoff, RevCode) ->
  FPoff0 = context_framesize(Context),
  if FPoff =:= FPoff0 -> [];
     true -> exit({?MODULE,do_block,FPoff})
  end,
  lists:reverse(RevCode, []).

do_insn(I, LiveOut, Context, FPoff) ->
  case I of
    #alu{} ->
      {[do_alu(I, Context, FPoff)], FPoff};
    #cmp{} ->
      {[do_cmp(I, Context, FPoff)], FPoff};
    #fp_unop{} ->
      {do_fp_unop(I, Context, FPoff), FPoff};
    #fp_binop{} ->
      {do_fp_binop(I, Context, FPoff), FPoff};
    #fmove{} ->
      {[do_fmove(I, Context, FPoff)], FPoff};
    #imul{} ->
      {[do_imul(I, Context, FPoff)], FPoff};
    #move{} ->
      {do_move(I, Context, FPoff), FPoff};
    #movsx{} ->
      {[do_movsx(I, Context, FPoff)], FPoff};
    #movzx{} ->
      {[do_movzx(I, Context, FPoff)], FPoff};
    #pseudo_call{} ->
      do_pseudo_call(I, LiveOut, Context, FPoff);
    #pseudo_spill_fmove{} ->
      {do_pseudo_spill_fmove(I, Context, FPoff), FPoff};
    #pseudo_spill_move{} ->
      {do_pseudo_spill_move(I, Context, FPoff), FPoff};
    #pseudo_tailcall{} ->
      {do_pseudo_tailcall(I, Context), context_framesize(Context)};
    #push{} ->
      {[do_push(I, Context, FPoff)], FPoff+word_size()};
    #ret{} ->
      {do_ret(I, Context, FPoff), context_framesize(Context)};
    #shift{} ->
      {[do_shift(I, Context, FPoff)], FPoff};
    #test{} ->
      {[do_test(I, Context, FPoff)], FPoff};
    _ ->	% comment, jmp, label, pseudo_jcc, pseudo_tailcall_prepare
      {[I], FPoff}
  end.

%%%
%%% Convert any pseudo-temp operand in a binary (alu, cmp, move)
%%% or unary (push) instruction to an explicit x86_mem operand.
%%%

do_alu(I, Context, FPoff) ->
  #alu{src=Src0,dst=Dst0} = I,
  Src = conv_opnd(Src0, FPoff, Context),
  Dst = conv_opnd(Dst0, FPoff, Context),
  I#alu{src=Src,dst=Dst}.

do_cmp(I, Context, FPoff) ->
  #cmp{src=Src0,dst=Dst0} = I,
  Src = conv_opnd(Src0, FPoff, Context),
  Dst = conv_opnd(Dst0, FPoff, Context),
  I#cmp{src=Src,dst=Dst}.

do_fp_unop(I, Context, FPoff) ->
  #fp_unop{arg=Arg0} = I,    
  Arg = conv_opnd(Arg0, FPoff, Context),
  [I#fp_unop{arg=Arg}].

do_fp_binop(I, Context, FPoff) ->
  #fp_binop{src=Src0,dst=Dst0} = I,    
  Src = conv_opnd(Src0, FPoff, Context),
  Dst = conv_opnd(Dst0, FPoff, Context),
  [I#fp_binop{src=Src,dst=Dst}].

do_fmove(I0, Context, FPoff) ->
  #fmove{src=Src0,dst=Dst0} = I0,
  Src = conv_opnd(Src0, FPoff, Context),
  Dst = conv_opnd(Dst0, FPoff, Context),
  I = I0#fmove{src=Src,dst=Dst},
  case Src =:= Dst of
    true -> []; % omit move-to-self
    false -> [I]
  end.

do_pseudo_spill_fmove(I0, Context, FPoff) ->
  #pseudo_spill_fmove{src=Src0,temp=Temp0,dst=Dst0} = I0,
  Src = conv_opnd(Src0, FPoff, Context),
  Temp = conv_opnd(Temp0, FPoff, Context),
  Dst = conv_opnd(Dst0, FPoff, Context),
  case Src =:= Dst of
    true -> []; % omit move-to-self
    false -> [#fmove{src=Src, dst=Temp}, #fmove{src=Temp, dst=Dst}]
  end.

do_imul(I, Context, FPoff) ->
  #imul{src=Src0} = I,
  Src = conv_opnd(Src0, FPoff, Context),
  I#imul{src=Src}.

do_move(I0, Context, FPoff) ->
  #move{src=Src0,dst=Dst0} = I0,
  Src = conv_opnd(Src0, FPoff, Context),
  Dst = conv_opnd(Dst0, FPoff, Context),
  I = I0#move{src=Src,dst=Dst},
  case Src =:= Dst of
    true -> []; % omit move-to-self
    false -> [I]
  end.

do_pseudo_spill_move(I0, Context, FPoff) ->
  #pseudo_spill_move{src=Src0,temp=Temp0,dst=Dst0} = I0,
  Src = conv_opnd(Src0, FPoff, Context),
  Temp = conv_opnd(Temp0, FPoff, Context),
  Dst = conv_opnd(Dst0, FPoff, Context),
  case Src =:= Dst of
    true -> []; % omit move-to-self
    false -> [#move{src=Src, dst=Temp}, #move{src=Temp, dst=Dst}]
  end.

do_movsx(I, Context, FPoff) ->
  #movsx{src=Src0,dst=Dst0} = I,
  Src = conv_opnd(Src0, FPoff, Context),
  Dst = conv_opnd(Dst0, FPoff, Context),
  I#movsx{src=Src,dst=Dst}.

do_movzx(I, Context, FPoff) ->
  #movzx{src=Src0,dst=Dst0} = I,
  Src = conv_opnd(Src0, FPoff, Context),
  Dst = conv_opnd(Dst0, FPoff, Context),
  I#movzx{src=Src,dst=Dst}.

do_push(I, Context, FPoff) ->
  #push{src=Src0} = I,
  Src = conv_opnd(Src0, FPoff, Context),
  I#push{src=Src}.

do_shift(I, Context, FPoff) ->
  #shift{src=Src0,dst=Dst0} = I,
  Src = conv_opnd(Src0, FPoff, Context),
  Dst = conv_opnd(Dst0, FPoff, Context),
  I#shift{src=Src,dst=Dst}.

do_test(I, Context, FPoff) ->
  #test{src=Src0,dst=Dst0} = I,
  Src = conv_opnd(Src0, FPoff, Context),
  Dst = conv_opnd(Dst0, FPoff, Context),
  I#test{src=Src,dst=Dst}.

conv_opnd(Opnd, FPoff, Context) ->
  case opnd_is_pseudo(Opnd) of
    false ->
      Opnd;
    true ->
      conv_pseudo(Opnd, FPoff, Context)
  end.

conv_pseudo(Temp, FPoff, Context) ->
  Off = FPoff + context_offset(Context, Temp),
  conv_pseudo(Temp, Off).

conv_pseudo(Temp, Off) ->
  hipe_x86:mk_mem(mk_sp(), hipe_x86:mk_imm(Off), hipe_x86:temp_type(Temp)).

%%%
%%% Return - deallocate frame and emit 'ret $N' insn.
%%%

do_ret(_I, Context, FPoff) ->
  %% XXX: this conses up a new ret insn, ignoring the one rtl->x86 made
  adjust_sp(FPoff, [hipe_x86:mk_ret(word_size()*context_arity(Context))]).

adjust_sp(N, Rest) ->
  if N =:= 0 ->
      Rest;
     true ->
      [hipe_x86:mk_alu('add', hipe_x86:mk_imm(N), mk_sp()) | Rest]
  end.

%%%
%%% Recursive calls.
%%%

do_pseudo_call(I, LiveOut, Context, FPoff0) ->
  #x86_sdesc{exnlab=ExnLab,arity=OrigArity} = hipe_x86:pseudo_call_sdesc(I),
  Fun0 = hipe_x86:pseudo_call_fun(I),
  Fun1 = conv_opnd(Fun0, FPoff0, Context),
  LiveTemps = [Temp || Temp <- LiveOut, temp_is_pseudo(Temp)],
  SDesc = mk_sdesc(ExnLab, Context, LiveTemps),
  ContLab = hipe_x86:pseudo_call_contlab(I),
  Linkage = hipe_x86:pseudo_call_linkage(I),
  CallCode = [hipe_x86:mk_pseudo_call(Fun1, SDesc, ContLab, Linkage)],
  %% +word_size() for our RA and +word_size() for callee's RA should
  %% it need to call inc_stack
  StkArity = erlang:max(0, OrigArity - ?HIPE_X86_REGISTERS:nr_args()),
  context_need_stack(Context, stack_need(FPoff0 + 2*word_size(), StkArity, Fun1)),
  ArgsBytes = word_size() * StkArity,
  {CallCode, FPoff0 - ArgsBytes}.

stack_need(FPoff, StkArity, Fun) ->
  case Fun of
    #x86_prim{} -> FPoff;
    #x86_mfa{m=M,f=F,a=A} ->
      case erlang:is_builtin(M, F, A) of
	true -> FPoff;
	false -> stack_need_general(FPoff, StkArity)
      end;
    #x86_temp{} -> stack_need_general(FPoff, StkArity);
    #x86_mem{} -> stack_need_general(FPoff, StkArity)
  end.

stack_need_general(FPoff, StkArity) ->
  erlang:max(FPoff, FPoff + (?LEAF_WORDS - 2 - StkArity) * word_size()).

%%%
%%% Create stack descriptors for call sites.
%%%

mk_sdesc(ExnLab, Context, Temps) ->	% for normal calls
  Temps0 = only_tagged(Temps),
  Live = mk_live(Context, Temps0),
  Arity = context_arity(Context),
  FSize = context_framesize(Context),
  hipe_x86:mk_sdesc(ExnLab, FSize div word_size(), Arity,
                    list_to_tuple(Live)).

only_tagged(Temps)->
  [X || X <- Temps, hipe_x86:temp_type(X) =:= 'tagged'].

mk_live(Context, Temps) ->
  lists:sort([temp_to_slot(Context, Temp) || Temp <- Temps]).

temp_to_slot(Context, Temp) ->
  (context_framesize(Context) + context_offset(Context, Temp))
    div word_size().

mk_minimal_sdesc(Context) ->		% for inc_stack_0 calls
  hipe_x86:mk_sdesc([], 0, context_arity(Context), {}).

%%%
%%% Tailcalls.
%%%

do_pseudo_tailcall(I, Context) ->	% always at FPoff=context_framesize(Context)
  Arity = context_arity(Context),
  Args = hipe_x86:pseudo_tailcall_stkargs(I) ++ [context_ra(Context)],
  Fun0 = hipe_x86:pseudo_tailcall_fun(I),
  {Insns, FPoff1, Fun1} = do_tailcall_args(Args, Context, Fun0),
  context_need_stack(Context, FPoff1),
  FPoff2 = FPoff1 + word_size()+word_size()*Arity - word_size()*length(Args),
  %% +word_size() for callee's inc_stack RA
  StkArity = length(hipe_x86:pseudo_tailcall_stkargs(I)),
  context_need_stack(Context, stack_need(FPoff2 + word_size(), StkArity, Fun1)),
  I2 = hipe_x86:mk_jmp_fun(Fun1, hipe_x86:pseudo_tailcall_linkage(I)),
  Insns ++ adjust_sp(FPoff2, [I2]).

do_tailcall_args(Args, Context, Fun0) ->
  FPoff0 = context_framesize(Context),
  Arity = context_arity(Context),
  FrameTop = word_size() + word_size()*Arity,
  DangerOff = FrameTop - word_size()*length(Args),
  Moves = mk_moves(Args, FrameTop, []),
  {Stores, Simple, Conflict} =
    split_moves(Moves, Context, DangerOff, [], [], []),
  %% sanity check (shouldn't trigger any more)
  if DangerOff < -FPoff0 ->
      exit({?MODULE,do_tailcall_args,DangerOff,-FPoff0});
     true -> []
  end,
  FPoff1 = FPoff0,
  %%
  {Pushes, MoreSimple, FPoff2} = split_conflict(Conflict, FPoff1, [], []),
  %%
  {PushFun0, FPoff3, LoadFun1, Fun1} =
    case opnd_is_pseudo(Fun0) of
      false ->
	{[], FPoff2, [], Fun0};
      true ->
	Type = hipe_x86:temp_type(Fun0),
	Temp1 = mk_temp1(Type),
	Fun0Off = context_offset(Context, Fun0),
	MEM0 = conv_pseudo(Fun0, FPoff2 + Fun0Off),
	if Fun0Off >= DangerOff ->
	    Fun1Off = hipe_x86:mk_imm(0),
	    MEM1 = hipe_x86:mk_mem(mk_sp(), Fun1Off, Type),
	    {[hipe_x86:mk_push(MEM0)],
	     FPoff2 + word_size(),
	     [hipe_x86:mk_move(MEM1, Temp1)],
	     Temp1};
	   true ->
	    {[], FPoff2, [hipe_x86:mk_move(MEM0, Temp1)], Temp1}
	end
    end,
  %%
  RegTemp0 = ?HIPE_X86_REGISTERS:temp0(),
  TempReg =
    case hipe_x86:is_temp(Fun1) of
      true ->
	RegFun1 = hipe_x86:temp_reg(Fun1),
	if RegFun1 =/= RegTemp0 -> RegTemp0;
	   true -> ?HIPE_X86_REGISTERS:temp1()
	end;
      false ->
	RegTemp0
    end,
  %%
  {Pushes ++ PushFun0 ++
   store_moves(Stores, FPoff3, LoadFun1 ++
	       simple_moves(Simple, FPoff3, TempReg,
			    simple_moves(MoreSimple, FPoff3, TempReg,
					 []))),
   FPoff3, Fun1}.

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
      Type = typeof_src(Src),
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

split_conflict([{SrcOff,DstOff,Type}|Conflict], FPoff, Pushes, Simple) ->
  Push = hipe_x86:mk_push(
           hipe_x86:mk_mem(mk_sp(), hipe_x86:mk_imm(FPoff+SrcOff), Type)),
  split_conflict(Conflict, FPoff+word_size(), [Push|Pushes],
                 [{-(FPoff+word_size()),DstOff,Type}|Simple]);
split_conflict([], FPoff, Pushes, Simple) ->
  {lists:reverse(Pushes), Simple, FPoff}.

simple_moves([{SrcOff,DstOff,Type}|Moves], FPoff, TempReg, Rest) ->
  Temp = hipe_x86:mk_temp(TempReg, Type),
  SP = mk_sp(),
  LoadOff = hipe_x86:mk_imm(FPoff+SrcOff),
  LD = hipe_x86:mk_move(hipe_x86:mk_mem(SP, LoadOff, Type), Temp),
  StoreOff = hipe_x86:mk_imm(FPoff+DstOff),
  ST = hipe_x86:mk_move(Temp, hipe_x86:mk_mem(SP, StoreOff, Type)),
  simple_moves(Moves, FPoff, TempReg, [LD, ST | Rest]);
simple_moves([], _, _, Rest) ->
  Rest.

store_moves([{Src,DstOff}|Moves], FPoff, Rest) ->
  Type = typeof_src(Src),
  SP = mk_sp(),
  StoreOff = hipe_x86:mk_imm(FPoff+DstOff),
  ST = hipe_x86:mk_move(Src, hipe_x86:mk_mem(SP, StoreOff, Type)),
  store_moves(Moves, FPoff, [ST | Rest]);
store_moves([], _, Rest) ->
  Rest.

%%%
%%% Contexts
%%%

-record(context, {liveness, framesize, arity, map, ra, ref_maxstack}).

mk_context(Liveness, Formals, Temps) ->
  RA = hipe_x86:mk_new_temp('untagged'),
  {Map, MinOff}  = mk_temp_map(Formals, RA, Temps),
  FrameSize = (-MinOff),
  RefMaxStack = hipe_bifs:ref(FrameSize),
  Context = #context{liveness=Liveness,
		     framesize=FrameSize, arity=length(Formals),
		     map=Map, ra=RA, ref_maxstack=RefMaxStack},
  Context.

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

context_ra(#context{ra=RA}) ->
  RA.

mk_temp_map(Formals, RA, Temps) ->
  {Map, _} = enter_vars(Formals, word_size() * (length(Formals)+1),
			tmap_bind(tmap_empty(), RA, 0)),
  enter_vars(tset_to_list(Temps), 0, Map).

enter_vars([V|Vs], PrevOff, Map) ->
  Off =
    case hipe_x86:temp_type(V) of
      'double' -> PrevOff - float_size();
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
%%%	temp0 = sp - MaxStack
%%%	if( temp0 < SP_LIMIT(P) ) goto IncStack else goto AllocFrame
%%% AllocFrame:
%%%	sp -= FrameSize
%%%	goto OldStart
%%% OldStart:
%%%	...
%%% IncStack:
%%%	call inc_stack
%%%	goto NewStart

do_prologue(CFG, Context) ->
  do_check_stack(do_alloc_frame(CFG, Context), Context).

do_alloc_frame(CFG, Context) ->
  case context_framesize(Context) of
    0 ->
      CFG;
    FrameSize ->
      OldStartLab = hipe_x86_cfg:start_label(CFG),
      AllocFrameLab = hipe_gensym:get_next_label(x86),
      SP = mk_sp(),
      AllocFrameCode =
	[hipe_x86:mk_alu('sub', hipe_x86:mk_imm(FrameSize), SP),
	 hipe_x86:mk_jmp_label(OldStartLab)],
      CFG1 = hipe_x86_cfg:bb_add(CFG, AllocFrameLab,
				 hipe_bb:mk_bb(AllocFrameCode)),
      hipe_x86_cfg:start_label_update(CFG1, AllocFrameLab)
  end.

do_check_stack(CFG, Context) ->
  MaxStack = context_maxstack(Context),
  Arity = context_arity(Context),
  Guaranteed = erlang:max(0, (?LEAF_WORDS - 1 - Arity) * word_size()),
  if MaxStack =< Guaranteed ->
      %% io:format("~w: MaxStack ~w =< Guaranteed ~w :-)\n", [?MODULE,MaxStack,Guaranteed]),
      CFG;
     true ->
      %% io:format("~w: MaxStack ~w > Guaranteed ~w :-(\n", [?MODULE,MaxStack,Guaranteed]),
      AllocFrameLab = hipe_x86_cfg:start_label(CFG),
      NewStartLab = hipe_gensym:get_next_label(x86),
      IncStackLab = hipe_gensym:get_next_label(x86),
      %%
      Type = 'untagged',
      Preg = ?HIPE_X86_REGISTERS:proc_pointer(),
      Pbase = hipe_x86:mk_temp(Preg, Type),
      SP_LIMIT_OFF = hipe_x86:mk_imm(
                        ?HIPE_X86_REGISTERS:sp_limit_offset()),
      Temp0 = mk_temp0(Type),
      SP = mk_sp(),
      NewStartCode =
	%% hopefully this lea is faster than the mov;sub it replaced
	[hipe_x86:mk_lea(
           hipe_x86:mk_mem(SP, hipe_x86:mk_imm(-MaxStack), 'untagged'),
           Temp0),
	 hipe_x86:mk_cmp(
           hipe_x86:mk_mem(Pbase, SP_LIMIT_OFF, Type), Temp0),
	 hipe_x86:mk_pseudo_jcc('b', IncStackLab, AllocFrameLab, 0.01)],
      IncStackCode =
	[hipe_x86:mk_call(hipe_x86:mk_prim('inc_stack_0'),
			  mk_minimal_sdesc(Context), not_remote),
	 hipe_x86:mk_jmp_label(NewStartLab)],
      %%
      CFG1 = hipe_x86_cfg:bb_add(CFG, NewStartLab,
                                 hipe_bb:mk_bb(NewStartCode)),
      CFG2 = hipe_x86_cfg:bb_add(CFG1, IncStackLab,
				 hipe_bb:mk_bb(IncStackCode)),
      hipe_x86_cfg:start_label_update(CFG2, NewStartLab)
  end.

%%% typeof_src -- what's src's type?

typeof_src(Src) ->
  case Src of
    #x86_imm{} ->
      'untagged';
    #x86_temp{} ->
      hipe_x86:temp_type(Src);
    #x86_mem{} ->
      hipe_x86:mem_type(Src)
  end.

%%% Cons up an '%sp' Temp.

mk_sp() ->
  hipe_x86:mk_temp(?HIPE_X86_REGISTERS:sp(), 'untagged').

%%% Cons up a '%temp0' Temp.

mk_temp0(Type) ->
  hipe_x86:mk_temp(?HIPE_X86_REGISTERS:temp0(), Type).

%%% Cons up a '%temp1' Temp.

mk_temp1(Type) ->
  hipe_x86:mk_temp(?HIPE_X86_REGISTERS:temp1(), Type).

%%% Check if an operand is a pseudo-Temp.

src_is_pseudo(Src) ->
  opnd_is_pseudo(Src).

opnd_is_pseudo(Opnd) ->
  case hipe_x86:is_temp(Opnd) of
    true -> temp_is_pseudo(Opnd);
    false -> false
  end.

temp_is_pseudo(Temp) ->
  case hipe_x86:is_temp(Temp) of
    true -> 
      not(?HIPE_X86_REGISTERS:is_precoloured(hipe_x86:temp_reg(Temp)));
    false -> 
      false
  end.


%%%
%%% Build the set of all temps used in a Defun's body.
%%%

all_temps(CFG, Formals) ->
  S0 = fold_insns(fun find_temps/2, tset_empty(), CFG),
  S1 = tset_del_list(S0, Formals),
  S2 = tset_filter(S1, fun(T) -> temp_is_pseudo(T) end),
  S2.

find_temps(I, S0) ->
  S1 = tset_add_list(S0, hipe_x86_defuse:insn_def(I)),
  tset_add_list(S1, hipe_x86_defuse:insn_use(I)).

fold_insns(Fun, InitAcc, CFG) ->
  hipe_x86_cfg:fold_bbs(
    fun(_, BB, Acc0) -> lists:foldl(Fun, Acc0, hipe_bb:code(BB)) end,
    InitAcc, CFG).

-compile({inline, [tset_empty/0, tset_size/1, tset_insert/2,
		   tset_filter/2, tset_to_list/1]}).

tset_empty() ->
  #{}.

tset_size(S) ->
  map_size(S).

tset_insert(S, T) ->
  S#{T => []}.

tset_add_list(S, []) -> S;
tset_add_list(S, [T|Ts]) ->
  tset_add_list(S#{T => []}, Ts).

tset_del_list(S, []) -> S;
tset_del_list(S, [T|Ts]) ->
  tset_del_list(maps:remove(T,S), Ts).

tset_filter(S, F) ->
  maps:filter(fun(K, _V) -> F(K) end, S).

tset_to_list(S) ->
  maps:keys(S).

%%%
%%% Compute minimum permissible frame size, ignoring spilled temps.
%%% This is done to ensure that we won't have to adjust the frame size
%%% in the middle of a tailcall.
%%%

defun_minframe(CFG) ->
  MaxTailArity = fold_insns(fun insn_mta/2, 0, CFG),
  MyArity = length(fix_formals(hipe_x86_cfg:params(CFG))),
  erlang:max(MaxTailArity - MyArity, 0).

insn_mta(I, MTA) ->
  case I of
    #pseudo_tailcall{arity=Arity} ->
      erlang:max(MTA, Arity - ?HIPE_X86_REGISTERS:nr_args());
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
      Temp = hipe_x86:mk_new_temp('untagged'),
      ensure_minframe(MinFrame, Frame+1, tset_insert(Temps, Temp));
     true -> Temps
  end.

word_size() ->
  ?HIPE_X86_REGISTERS:wordsize().

float_size() ->
  ?HIPE_X86_REGISTERS:float_size().
