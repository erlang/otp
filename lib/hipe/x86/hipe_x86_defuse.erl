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
%%% compute def/use sets for x86 insns
%%%
%%% TODO:
%%% - represent EFLAGS (condition codes) use/def by a virtual reg?
%%% - should push use/def %esp?

-ifdef(HIPE_AMD64).
-define(HIPE_X86_DEFUSE,	hipe_amd64_defuse).
-define(HIPE_X86_REGISTERS,	hipe_amd64_registers).
-define(RV,			rax).
-else.
-define(HIPE_X86_DEFUSE,	hipe_x86_defuse).
-define(HIPE_X86_REGISTERS,	hipe_x86_registers).
-define(RV,			eax).
-endif.

-module(?HIPE_X86_DEFUSE).
-export([insn_def/1, insn_defs_all/1, insn_use/1]). %% src_use/1]).
-include("../x86/hipe_x86.hrl").

%%%
%%% insn_def(Insn) -- Return set of temps defined by an instruction.
%%%

insn_def(I) ->
  case I of
    #alu{dst=Dst} -> dst_def(Dst);
    #cmovcc{dst=Dst} -> dst_def(Dst);
    #fmove{dst=Dst} -> dst_def(Dst);
    #fp_binop{dst=Dst} -> dst_def(Dst);
    #fp_unop{arg=Arg} -> dst_def(Arg);
    #imul{temp=Temp} -> [Temp];
    #lea{temp=Temp} -> [Temp];
    #move{dst=Dst} -> dst_def(Dst);
    #move64{dst=Dst} -> dst_def(Dst);
    #movsx{dst=Dst} -> dst_def(Dst);
    #movzx{dst=Dst} -> dst_def(Dst);
    #pseudo_call{} -> call_clobbered();
    #pseudo_spill{} -> [];
    #pseudo_spill_fmove{temp=Temp, dst=Dst} -> [Temp, Dst];
    #pseudo_spill_move{temp=Temp, dst=Dst} -> [Temp, Dst];
    #pseudo_tailcall_prepare{} -> tailcall_clobbered();
    #shift{dst=Dst} -> dst_def(Dst);
    %% call, cmp, comment, jcc, jmp_fun, jmp_label, jmp_switch, label
    %% pseudo_jcc, pseudo_tailcall, push, ret, test
    _ -> []
  end.


%% @doc Answers whether instruction I defines all allocatable registers. Used by
%% hipe_regalloc_prepass.
-spec insn_defs_all(_) -> boolean().
insn_defs_all(I) ->
  case I of
    #pseudo_call{} -> true;
    _ -> false
  end.

dst_def(Dst) ->
  case Dst of
    #x86_temp{} -> [Dst];
    #x86_fpreg{} -> [Dst];
    _ -> []
  end.

call_clobbered() ->
  [hipe_x86:mk_temp(R, T)
   || {R,T} <- ?HIPE_X86_REGISTERS:call_clobbered()].

tailcall_clobbered() ->
  [hipe_x86:mk_temp(R, T)
   || {R,T} <- ?HIPE_X86_REGISTERS:tailcall_clobbered()].

%%%
%%% insn_use(Insn) -- Return set of temps used by an instruction.
%%%

insn_use(I) ->
  case I of
    #alu{src=Src,dst=Dst} -> addtemp(Src, addtemp(Dst, []));
    #call{'fun'=Fun} -> addtemp(Fun, []);
    #cmovcc{src=Src, dst=Dst} -> addtemp(Src, dst_use(Dst));
    #cmp{src=Src, dst=Dst} -> addtemp(Src, addtemp(Dst, []));
    #fmove{src=Src,dst=Dst} -> addtemp(Src, dst_use(Dst));
    #fp_unop{arg=Arg} -> addtemp(Arg, []);
    #fp_binop{src=Src,dst=Dst} -> addtemp(Src, addtemp(Dst, []));
    #imul{imm_opt=ImmOpt,src=Src,temp=Temp} ->
      addtemp(Src, case ImmOpt of [] -> addtemp(Temp, []); _ -> [] end);
    #jmp_fun{'fun'=Fun} -> addtemp(Fun, []);
    #jmp_switch{temp=Temp, jtab=JTab} -> addtemp(Temp, addtemp(JTab, []));
    #lea{mem=Mem} -> addtemp(Mem, []);
    #move{src=Src,dst=Dst} -> addtemp(Src, dst_use(Dst));
    #move64{} -> [];
    #movsx{src=Src,dst=Dst} -> addtemp(Src, dst_use(Dst));
    #movzx{src=Src,dst=Dst} -> addtemp(Src, dst_use(Dst));
    #pseudo_call{'fun'=Fun,sdesc=#x86_sdesc{arity=Arity}} ->
      addtemp(Fun, arity_use(Arity));
    #pseudo_spill{args=Args} -> Args;
    #pseudo_spill_fmove{src=Src} -> [Src];
    #pseudo_spill_move{src=Src} -> [Src];
    #pseudo_tailcall{'fun'=Fun,arity=Arity,stkargs=StkArgs} ->
      addtemp(Fun, addtemps(StkArgs, addtemps(tailcall_clobbered(),
					      arity_use(Arity))));
    #push{src=Src} -> addtemp(Src, []);
    #ret{} -> [hipe_x86:mk_temp(?HIPE_X86_REGISTERS:?RV(), 'tagged')];
    #shift{src=Src,dst=Dst} -> addtemp(Src, addtemp(Dst, []));
    #test{src=Src, dst=Dst} -> addtemp(Src, addtemp(Dst, []));
    %% comment, jcc, jmp_label, label, pseudo_jcc, pseudo_tailcall_prepare
    _ -> []
  end.

arity_use(Arity) ->
  [hipe_x86:mk_temp(R, 'tagged')
   || R <- ?HIPE_X86_REGISTERS:args(Arity)].

dst_use(Dst) ->
  case Dst of
    #x86_mem{base=Base,off=Off} -> addbase(Base, addtemp(Off, []));
    _ -> []
  end.

%%%
%%% src_use(Src) -- Return set of temps used by a source operand.
%%%

%% src_use(Src) ->
%%   addtemp(Src, []).

%%%
%%% Auxiliary operations on sets of temps
%%%

addtemps([Arg|Args], Set) ->
  addtemps(Args, addtemp(Arg, Set));
addtemps([], Set) ->
  Set.

addtemp(Arg, Set) ->
  case Arg of
    #x86_temp{} -> add(Arg, Set);
    #x86_mem{base=Base,off=Off} -> addtemp(Off, addbase(Base, Set));
    #x86_fpreg{} -> add(Arg, Set);
    _ -> Set
  end.

addbase(Base, Set) ->
  case Base of
    [] -> Set;
    _ -> addtemp(Base, Set)
  end.

add(Arg, Set) ->
  case lists:member(Arg, Set) of
    false -> [Arg|Set];
    _ -> Set
  end.
