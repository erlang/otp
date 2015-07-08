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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.
%%=====================================================================
%%  Filename : 	hipe_rtl_arch.erl
%%  History  :	* 2001-04-10 Erik Johansson (happi@it.uu.se): Created.
%%=====================================================================
%% @doc
%%
%%   This module contains interface functions whose semantics and
%%   implementation depend on the target architecture.
%%
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_arch).

-export([first_virtual_reg/0,
	 heap_pointer/0,
	 heap_limit/0,
	 fcalls/0,
	 reg_name/1,
	 is_precoloured/1,
	 call_defined/0,
	 call_used/0,
	 tailcall_used/0,
	 return_used/0,
	 live_at_return/0,
	 endianess/0,
	 load_big_2/4,
	 load_little_2/4,
	 load_big_4/4,
	 load_little_4/4,
	 %% store_4/3,
	 eval_alu/3,
	 %% eval_alub/4,
	 eval_cond/3,
	 eval_cond_bits/5,
	 fwait/0,
	 handle_fp_exception/0,
	 pcb_load/2,
	 pcb_load/3,
	 pcb_store/2,
	 pcb_store/3,
	 pcb_address/2,
	 call_bif/5,
	 %% alignment/0,
	 nr_of_return_regs/0,
         log2_word_size/0,
         word_size/0,
	 mk_fp_check_result/1
	]).

-include("hipe_literals.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ____________________________________________________________________
%%
%% ARCH-specific stuff
%% ____________________________________________________________________
%%
%%
%% XXX: x86 might not _have_ real registers for some of these things
%%

first_virtual_reg() ->
  case get(hipe_target_arch) of
    ultrasparc ->
      hipe_sparc_registers:first_virtual();
    powerpc ->
      hipe_ppc_registers:first_virtual();
    ppc64 ->
      hipe_ppc_registers:first_virtual();
    arm ->
      hipe_arm_registers:first_virtual();
    x86 ->
      hipe_x86_registers:first_virtual();
    amd64 ->
      hipe_amd64_registers:first_virtual()
  end.

heap_pointer() ->	% {GetHPInsn, HPReg, PutHPInsn}
  case get(hipe_target_arch) of
    ultrasparc ->
      heap_pointer_from_reg(hipe_sparc_registers:heap_pointer());
    powerpc ->
      heap_pointer_from_reg(hipe_ppc_registers:heap_pointer());
    ppc64 ->
      heap_pointer_from_reg(hipe_ppc_registers:heap_pointer());
    arm ->
      heap_pointer_from_reg(hipe_arm_registers:heap_pointer());
    x86 ->
      x86_heap_pointer();
    amd64 ->
      amd64_heap_pointer()
  end.

heap_pointer_from_reg(Reg) ->
  {hipe_rtl:mk_comment('get_heap_pointer'),
   hipe_rtl:mk_reg(Reg),
   hipe_rtl:mk_comment('put_heap_pointer')}.

-ifdef(AMD64_HP_IN_REGISTER).
amd64_heap_pointer() ->
  heap_pointer_from_reg(hipe_amd64_registers:heap_pointer()).
-else.
-define(HEAP_POINTER_FROM_PCB_NEEDED,1).
amd64_heap_pointer() ->
  heap_pointer_from_pcb().
-endif.

-ifdef(X86_HP_IN_ESI).
x86_heap_pointer() ->
  heap_pointer_from_reg(hipe_x86_registers:heap_pointer()).
-else.
-define(HEAP_POINTER_FROM_PCB_NEEDED,1).
x86_heap_pointer() ->
  heap_pointer_from_pcb().
-endif.

-ifdef(HEAP_POINTER_FROM_PCB_NEEDED).
heap_pointer_from_pcb() ->
  Reg = hipe_rtl:mk_new_reg(),
  {pcb_load(Reg, ?P_HP), Reg, pcb_store(?P_HP, Reg)}.
-endif.

heap_limit() ->	% {GetHLIMITInsn, HLIMITReg}
  case get(hipe_target_arch) of
    ultrasparc ->
      heap_limit_from_pcb();
    powerpc ->
      heap_limit_from_pcb();
    ppc64 ->
      heap_limit_from_pcb();
    arm ->
      heap_limit_from_pcb();
    x86 ->
      heap_limit_from_reg(hipe_x86_registers:heap_limit());
    amd64 ->
      heap_limit_from_reg(hipe_amd64_registers:heap_limit())
  end.

heap_limit_from_reg(Reg) ->
  {hipe_rtl:mk_comment('get_heap_limit'),
   hipe_rtl:mk_reg(Reg)}.

heap_limit_from_pcb() ->
  Reg = hipe_rtl:mk_new_reg(),
  {pcb_load(Reg, ?P_HP_LIMIT), Reg}.

fcalls() ->	% {GetFCallsInsn, FCallsReg, PutFCallsInsn}
  case get(hipe_target_arch) of
    ultrasparc ->
      fcalls_from_pcb();
    powerpc ->
      fcalls_from_pcb();
    ppc64 ->
      fcalls_from_pcb();
    arm ->
      fcalls_from_pcb();
    x86 ->
      fcalls_from_reg(hipe_x86_registers:fcalls());
    amd64 ->
      fcalls_from_reg(hipe_amd64_registers:fcalls())
  end.

fcalls_from_reg(Reg) ->
  {hipe_rtl:mk_comment('get_fcalls'),
   hipe_rtl:mk_reg(Reg),
   hipe_rtl:mk_comment('put_fcalls')}.

fcalls_from_pcb() ->
  Reg = hipe_rtl:mk_new_reg(),
  {pcb_load(Reg, ?P_FCALLS), Reg, pcb_store(?P_FCALLS, Reg)}.

reg_name(Reg) ->
  case get(hipe_target_arch) of
    ultrasparc ->
      hipe_sparc_registers:reg_name_gpr(Reg);
    powerpc ->
      hipe_ppc_registers:reg_name_gpr(Reg);
    ppc64 ->
      hipe_ppc_registers:reg_name_gpr(Reg);
    arm ->
      hipe_arm_registers:reg_name_gpr(Reg);
    x86 ->
      hipe_x86_registers:reg_name(Reg);
    amd64 ->
      hipe_amd64_registers:reg_name(Reg)
  end.

%% @spec is_precoloured(rtl_arg()) -> boolean()
%%
%% @doc  Succeeds if Arg is mapped to a precoloured register in the target.
%%
is_precoloured(Arg) ->
  case hipe_rtl:is_reg(Arg) of
    true ->
      is_precolored_regnum(hipe_rtl:reg_index(Arg));
    false ->
      hipe_rtl:is_var(Arg) andalso
	is_precolored_regnum(hipe_rtl:var_index(Arg))
  end.

is_precolored_regnum(RegNum) ->
  case get(hipe_target_arch) of
    ultrasparc ->
      hipe_sparc_registers:is_precoloured_gpr(RegNum);
    powerpc ->
      hipe_ppc_registers:is_precoloured_gpr(RegNum);
    ppc64 ->
      hipe_ppc_registers:is_precoloured_gpr(RegNum);
    arm ->
      hipe_arm_registers:is_precoloured_gpr(RegNum);
    x86 ->
      hipe_x86_registers:is_precoloured(RegNum);
    amd64 ->
      hipe_amd64_registers:is_precoloured(RegNum)
  end.

call_defined() ->
  call_used().

call_used() ->
  live_at_return().

tailcall_used() ->
  call_used().

return_used() ->
  tailcall_used().

live_at_return() ->
  case get(hipe_target_arch) of
    ultrasparc ->
      ordsets:from_list([hipe_rtl:mk_reg(R)
			 || {R,_} <- hipe_sparc_registers:live_at_return()]);
    powerpc ->
      ordsets:from_list([hipe_rtl:mk_reg(R)
			 || {R,_} <- hipe_ppc_registers:live_at_return()]);
    ppc64 ->
      ordsets:from_list([hipe_rtl:mk_reg(R)
			 || {R,_} <- hipe_ppc_registers:live_at_return()]);
    arm ->
      ordsets:from_list([hipe_rtl:mk_reg(R)
			 || {R,_} <- hipe_arm_registers:live_at_return()]);
    x86 ->
      ordsets:from_list([hipe_rtl:mk_reg(R)
			 || {R,_} <- hipe_x86_registers:live_at_return()]);
    amd64 ->
      ordsets:from_list([hipe_rtl:mk_reg(R)
			 || {R,_} <- hipe_amd64_registers:live_at_return()])
  end.

%% @spec word_size() -> integer()
%%
%% @doc Returns the target's word size.
%%
word_size() ->
  case get(hipe_target_arch) of
    ultrasparc -> 4;
    powerpc    -> 4;
    ppc64      -> 8;
    arm	       -> 4;
    x86        -> 4;
    amd64      -> 8
  end.

%% alignment() ->
%%   case get(hipe_target_arch) of
%%     ultrasparc -> 4;
%%     powerpc    -> 4;
%%     arm	  -> 4;
%%     x86        -> 4;
%%     amd64      -> 8
%%   end.

%% @spec log2_word_size() -> integer()
%%
%% @doc Returns log2 of the target's word size.
%%
log2_word_size() ->
  case get(hipe_target_arch) of
    ultrasparc -> 2;
    powerpc    -> 2;
    ppc64      -> 3;
    arm	       -> 2;
    x86        -> 2;
    amd64      -> 3
  end.

%% @spec endianess() -> big | little
%%
%% @doc Returns the target's endianess.
%%
endianess() ->
  case get(hipe_target_arch) of
    ultrasparc -> big;
    powerpc    -> big;
    ppc64      -> big;
    x86        -> little;
    amd64      -> little;
    arm        -> ?ARM_ENDIANESS
  end.

%%%------------------------------------------------------------------------
%%% Reading integers from binaries, in various sizes and endianesses.
%%% Operand-sized alignment is NOT guaranteed, only byte alignment.
%%%------------------------------------------------------------------------

%%% Load a 2-byte big-endian integer from a binary.
%%% Increment Offset by 2.
load_big_2(Dst, Base, Offset, Signedness) ->
  case get(hipe_target_arch) of
    powerpc ->
      load_2_directly(Dst, Base, Offset, Signedness);
    ppc64 ->
      load_2_directly(Dst, Base, Offset, Signedness);
    %% Note: x86 could use a "load;xchgb" or "load;rol $8,<16-bit reg>"
    %% sequence here. This has been implemented, but unfortunately didn't
    %% make consistent improvements to our benchmarks.
    _ ->
      load_big_2_in_pieces(Dst, Base, Offset, Signedness)
    end.

%%% Load a 2-byte little-endian integer from a binary.
%%% Increment Offset by 2.
load_little_2(Dst, Base, Offset, Signedness) ->
  case get(hipe_target_arch) of
    x86 ->
      load_2_directly(Dst, Base, Offset, Signedness);
    powerpc ->
      [hipe_rtl:mk_call([Dst], 'lhbrx', [Base,Offset], [], [], not_remote),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(2)) |
       case Signedness of
	 unsigned -> [];
	 signed -> [hipe_rtl:mk_call([Dst], 'extsh', [Dst], [], [], not_remote)]
       end];
    ppc64 ->
      [hipe_rtl:mk_call([Dst], 'lhbrx', [Base,Offset], [], [], not_remote),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(2)) |
       case Signedness of
	 unsigned -> [];
	 signed -> [hipe_rtl:mk_call([Dst], 'extsh', [Dst], [], [], not_remote)]
       end];
    _ ->
      load_little_2_in_pieces(Dst, Base, Offset, Signedness)
  end.

load_2_directly(Dst, Base, Offset, Signedness) ->
  [hipe_rtl:mk_load(Dst, Base, Offset, int16, Signedness),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(2))].

load_big_2_in_pieces(Dst, Base, Offset, Signedness) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_load(Dst, Base, Offset, byte, Signedness),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))].

load_little_2_in_pieces(Dst, Base, Offset, Signedness) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_load(Dst, Base, Offset, byte, unsigned),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_load(Tmp1, Base, Offset, byte, Signedness),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))].

%%% Load a 4-byte big-endian integer from a binary.
%%% Increment Offset by 4.
load_big_4(Dst, Base, Offset, Signedness) ->
  case get(hipe_target_arch) of
    powerpc ->
      load_4_directly(Dst, Base, Offset, Signedness);
    ppc64 ->
      load_4_directly(Dst, Base, Offset, Signedness);
    %% Note: x86 could use a "load;bswap" sequence here.
    %% This has been implemented, but unfortunately didn't
    %% make any noticeable improvements in our benchmarks.
    arm ->
      %% When loading 4 bytes into a 32-bit register, the
      %% signedness of the high-order byte doesn't matter.
      %% ARM prefers unsigned byte loads so we'll use that.
      load_big_4_in_pieces(Dst, Base, Offset, unsigned);
    _ ->
      load_big_4_in_pieces(Dst, Base, Offset, Signedness)
  end.

%%% Load a 4-byte little-endian integer from a binary.
%%% Increment Offset by 4.
load_little_4(Dst, Base, Offset, Signedness) ->
  case get(hipe_target_arch) of
    x86 ->
      load_4_directly(Dst, Base, Offset, Signedness);
    powerpc ->
      [hipe_rtl:mk_call([Dst], 'lwbrx', [Base,Offset], [], [], not_remote),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(4))];
    ppc64 ->
      [hipe_rtl:mk_call([Dst], 'lwbrx', [Base,Offset], [], [], not_remote),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(4)) |
       case Signedness of
	 unsigned -> [];
	 signed -> [hipe_rtl:mk_call([Dst], 'extsw', [Dst], [], [], not_remote)]
       end];
    arm ->
      %% When loading 4 bytes into a 32-bit register, the
      %% signedness of the high-order byte doesn't matter.
      %% ARM prefers unsigned byte loads so we'll use that.
      load_little_4_in_pieces(Dst, Base, Offset, unsigned);
    _ ->
      load_little_4_in_pieces(Dst, Base, Offset, Signedness)
  end.

load_4_directly(Dst, Base, Offset, Signedness) ->
  [hipe_rtl:mk_load(Dst, Base, Offset, int32, Signedness),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(4))].

load_big_4_in_pieces(Dst, Base, Offset, Signedness) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_load(Dst, Base, Offset, byte, Signedness),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))].

load_little_4_in_pieces(Dst, Base, Offset, Signedness) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_load(Dst, Base, Offset, byte, unsigned),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(16)),
   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_load(Tmp1, Base, Offset, byte, Signedness),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(24)),
   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))].

-ifdef(STORE_4_NEEDED).
store_4(Base, Offset, Src) ->
  case get(hipe_target_arch) of
    x86 ->
      store_4_directly(Base, Offset, Src);
    powerpc ->
      store_4_directly(Base, Offset, Src);
    ppc64 ->
      store_4_directly(Base, Offset, Src);
    arm ->
      store_big_4_in_pieces(Base, Offset, Src);
    ultrasparc ->
      store_big_4_in_pieces(Base, Offset, Src);
    amd64 ->
      store_4_directly(Base, Offset, Src)
  end.

store_4_directly(Base, Offset, Src) ->
  [hipe_rtl:mk_store(Base, Offset, Src, int32),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(4))].

store_big_4_in_pieces(Base, Offset, Src) ->
  [hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_store(Base, Offset, Src, byte),
   hipe_rtl:mk_alu(Offset, Offset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Src, Src, srl, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_store(Base, Offset, Src, byte),
   hipe_rtl:mk_alu(Offset, Offset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Src, Src, srl, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_store(Base, Offset, Src, byte),
   hipe_rtl:mk_alu(Offset, Offset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Src, Src, srl, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_store(Base, Offset, Src, byte),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(4))].
-endif.

%%----------------------------------------------------------------------
%% Handling of arithmetic -- depends on the size of word.
%%----------------------------------------------------------------------

eval_alu(Op, Arg1, Arg2) ->
  %% io:format("Evaluated alu: ~w ~w ~w = ",[Arg1, Op, Arg2]),
  Res = case word_size() of
	  4 ->
	    hipe_rtl_arith_32:eval_alu(Op, Arg1, Arg2);
	  8 ->
	    hipe_rtl_arith_64:eval_alu(Op, Arg1, Arg2)
	end,
  %% io:format("~w~n ",[Res]),
  Res.

-ifdef(EVAL_ALUB_NEEDED).
eval_alub(Op, Cond, Arg1, Arg2) ->
  %% io:format("Evaluated alub: ~w ~w ~w cond ~w = ",[Arg1, Op, Arg2, Cond]),
  Res = case word_size() of
	  4 ->
	    hipe_rtl_arith_32:eval_alub(Op, Cond, Arg1, Arg2);
	  8 ->
	    hipe_rtl_arith_64:eval_alub(Op, Cond, Arg1, Arg2)
	end,
  %% io:format("~w~n ",[Res]),
  Res.
-endif.

eval_cond(Cond, Arg1, Arg2) ->
  %% io:format("Evaluated cond: ~w ~w ~w = ",[Arg1, Cond, Arg2]),
  Res = case word_size() of
	  4 ->
	    hipe_rtl_arith_32:eval_cond(Cond, Arg1, Arg2);
	  8 ->
	    hipe_rtl_arith_64:eval_cond(Cond, Arg1, Arg2)
	end,
  %% io:format("~w~n ",[Res]),
  Res.

eval_cond_bits(Cond, N, Z, V, C) ->
  %% io:format("Evaluated cond: ~w ~w ~w = ",[Arg1, Cond, Arg2]),
  Res = case word_size() of
	  4 ->
	    hipe_rtl_arith_32:eval_cond_bits(Cond, N, Z, V, C);
	  8 ->
	    hipe_rtl_arith_64:eval_cond_bits(Cond, N, Z, V, C)
	end,
  %% io:format("~w~n ",[Res]),
  Res.

%%----------------------------------------------------------------------

fwait() ->
    case ?ERTS_NO_FPE_SIGNALS of
	1 -> [];
	0 -> fwait_real()
    end.

fwait_real() ->
  case get(hipe_target_arch) of
    x86 -> [hipe_rtl:mk_call([], 'fwait', [], [], [], not_remote)];
    amd64 -> [hipe_rtl:mk_call([], 'fwait', [], [], [], not_remote)];
    arm -> [];
    powerpc -> [];
    ppc64 -> [];
    ultrasparc -> []
  end.

%% @spec handle_fp_exception() -> [term()]
%%
%% @doc
%%   Returns RTL code to restore the FPU after a floating-point exception.
%% @end
handle_fp_exception() ->
    case ?ERTS_NO_FPE_SIGNALS of
	1 -> [];
	0 -> handle_real_fp_exception()
    end.

handle_real_fp_exception() ->
  case get(hipe_target_arch) of
    x86 ->
      ContLbl = hipe_rtl:mk_new_label(),
      [hipe_rtl:mk_call([], handle_fp_exception, [],
			hipe_rtl:label_name(ContLbl), [], not_remote),
       ContLbl];
    amd64 ->
      ContLbl = hipe_rtl:mk_new_label(),
      [hipe_rtl:mk_call([], handle_fp_exception, [],
			hipe_rtl:label_name(ContLbl), [], not_remote),
       ContLbl];
    arm ->
      [];
    powerpc ->
      [];
    ppc64 ->
      [];
    ultrasparc ->
      []
  end.

%%
%% PCB accesses.
%% Wrapped to avoid leaking the PCB pointer to the wrong places.
%%

pcb_load(Dst, Off) -> pcb_load(Dst, Off, word).

pcb_load(Dst, Off, Size) ->
  hipe_rtl:mk_load(Dst, proc_pointer(), hipe_rtl:mk_imm(Off), Size, unsigned).

pcb_store(Off, Src) -> pcb_store(Off, Src, word).

pcb_store(Off, Src, Size) ->
  hipe_rtl:mk_store(proc_pointer(), hipe_rtl:mk_imm(Off), Src, Size).

pcb_address(Dst, Off) ->
  hipe_rtl:mk_alu(Dst, proc_pointer(), 'add', hipe_rtl:mk_imm(Off)).

proc_pointer() ->	% must not be exported
  case get(hipe_target_arch) of
    ultrasparc ->
      hipe_rtl:mk_reg_gcsafe(hipe_sparc_registers:proc_pointer());
    powerpc ->
      hipe_rtl:mk_reg_gcsafe(hipe_ppc_registers:proc_pointer());
    ppc64 ->
      hipe_rtl:mk_reg_gcsafe(hipe_ppc_registers:proc_pointer());
    arm ->
      hipe_rtl:mk_reg_gcsafe(hipe_arm_registers:proc_pointer());
    x86 ->
      hipe_rtl:mk_reg_gcsafe(hipe_x86_registers:proc_pointer());
    amd64 ->
      hipe_rtl:mk_reg_gcsafe(hipe_amd64_registers:proc_pointer())
  end.

%%
%% Special BIF calls.
%% Wrapped to avoid leaking the PCB pointer to the wrong places,
%% and to allow ARCH-specific expansion.
%%

call_bif(Dst, Name, Args, Cont, Fail) ->
  hipe_rtl:mk_call(Dst, Name, Args, Cont, Fail, not_remote).

nr_of_return_regs() ->
  case get(hipe_target_arch) of
    ultrasparc ->
      1;
    %% hipe_sparc_registers:nr_rets();
    powerpc ->
      1;
    ppc64 ->
      1;
    %% hipe_ppc_registers:nr_rets();
    arm ->
      1;
    x86 ->
      hipe_x86_registers:nr_rets();
    amd64 ->
      1
    %% hipe_amd64_registers:nr_rets();
  end.


mk_fp_check_result(Result) ->
    case ?ERTS_NO_FPE_SIGNALS of
	0 ->
	    [];
	1 ->
	    [hipe_rtl:mk_fstore(proc_pointer(),
				hipe_rtl:mk_imm(?P_FLOAT_RESULT),
				Result),
	     hipe_rtl:mk_call([], emulate_fpe, [], [], [], not_remote)]
    end.
