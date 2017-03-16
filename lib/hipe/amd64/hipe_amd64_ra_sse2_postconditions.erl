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

-module(hipe_amd64_ra_sse2_postconditions).

-export([check_and_rewrite/2, check_and_rewrite/3]).

-include("../x86/hipe_x86.hrl").
-define(HIPE_INSTRUMENT_COMPILER, true).
-include("../main/hipe.hrl").
-define(count_temp(T), ?cons_counter(counter_mfa_mem_temps, T)).


check_and_rewrite(AMD64CFG, Coloring) ->
  check_and_rewrite(AMD64CFG, Coloring, 'normal').

check_and_rewrite(AMD64CFG, Coloring, Strategy) ->
  %%io:format("Converting\n"),
  TempMap = hipe_temp_map:cols2tuple(Coloring,hipe_amd64_specific_sse2,no_context),
  %%io:format("Rewriting\n"),
  do_bbs(hipe_x86_cfg:labels(AMD64CFG), TempMap, Strategy, AMD64CFG, false).

do_bbs([], _, _, CFG, DidSpill) -> {CFG, DidSpill};
do_bbs([Lbl|Lbls], TempMap, Strategy, CFG0, DidSpill0) ->
  Code0 = hipe_bb:code(BB = hipe_x86_cfg:bb(CFG0, Lbl)),
  {Code, DidSpill} = do_insns(Code0, TempMap, Strategy, [], DidSpill0),
  CFG = hipe_x86_cfg:bb_add(CFG0, Lbl, hipe_bb:code_update(BB, Code)),
  do_bbs(Lbls, TempMap, Strategy, CFG, DidSpill).

do_insns([I|Insns], TempMap, Strategy, Accum, DidSpill0) ->
  {NewIs, DidSpill1} = do_insn(I, TempMap, Strategy),
  do_insns(Insns, TempMap, Strategy, lists:reverse(NewIs, Accum),
	   DidSpill0 or DidSpill1);
do_insns([], _TempMap, _Strategy, Accum, DidSpill) ->
  {lists:reverse(Accum), DidSpill}.

do_insn(I, TempMap, Strategy) ->	% Insn -> {Insn list, DidSpill}
  case I of
    #fmove{} ->
      do_fmove(I, TempMap, Strategy);
    #fp_unop{} ->
      do_fp_unop(I, TempMap, Strategy);
    #fp_binop{} ->
      do_fp_binop(I, TempMap, Strategy);
    #pseudo_spill_fmove{} ->
      do_pseudo_spill_fmove(I, TempMap, Strategy);
    _ ->
      %% All non sse2 ops
      {[I], false}
  end.

%%% Fix an fp_binop.
do_fp_binop(I, TempMap, Strategy) ->
  #fp_binop{src=Src,dst=Dst} = I,
  case is_mem_opnd(Dst, TempMap) of
    true ->
      Tmp = clone(Dst, Strategy),
      {[#fmove{src=Dst, dst=Tmp},
	I#fp_binop{src=Src,dst=Tmp},
	#fmove{src=Tmp,dst=Dst}],
       true};
    false ->
      {[I], false}
  end.

do_fp_unop(I, TempMap, Strategy) ->
  #fp_unop{arg=Arg} = I,
  case is_mem_opnd(Arg, TempMap) of
    true ->
      Tmp = clone(Arg, Strategy),
      {[#fmove{src=Arg, dst=Tmp},
	I#fp_unop{arg=Tmp},
	#fmove{src=Tmp,dst=Arg}],
       true};
    false ->
      {[I], false}
  end.

%%% Fix an fmove op.
do_fmove(I, TempMap, Strategy) ->
  #fmove{src=Src,dst=Dst} = I,
  case
    (is_mem_opnd(Src, TempMap) andalso is_mem_opnd(Dst, TempMap))
    orelse (is_mem_opnd(Src, TempMap) andalso (not is_float_temp(Dst)))
    orelse ((not is_float_temp(Src)) andalso is_mem_opnd(Dst, TempMap))
  of
    true ->
      Tmp = spill_temp(double, Strategy),
      %% pseudo_spill_fmove allows spill slot move coalescing, but must not
      %% contain memory operands (except for spilled temps)
      Is = case is_float_temp(Src) andalso is_float_temp(Dst) of
	     true -> [#pseudo_spill_fmove{src=Src, temp=Tmp, dst=Dst}];
	     false -> [#fmove{src=Src, dst=Tmp},I#fmove{src=Tmp,dst=Dst}]
	   end,
      {Is, true};
    false ->
      {[I], false}
  end.

is_float_temp(#x86_temp{type=Type}) -> Type =:= double;
is_float_temp(#x86_mem{}) -> false.

%%% Fix an pseudo_spill_fmove op.
do_pseudo_spill_fmove(I = #pseudo_spill_fmove{temp=Temp}, TempMap, _Strategy) ->
  %% Temp is above the low water mark and must not have been spilled
  false = is_mem_opnd(Temp, TempMap),
  {[I], false}. % nothing to do

%%% Check if an operand denotes a memory cell (mem or pseudo).

is_mem_opnd(Opnd, TempMap) ->
  case Opnd of
    #x86_mem{} -> true;
    #x86_temp{type=double} ->
      Reg = hipe_x86:temp_reg(Opnd),
      case hipe_x86:temp_is_allocatable(Opnd) of
	true ->
	  case hipe_temp_map:is_spilled(Reg, TempMap) of
	    true ->
	      ?count_temp(Reg),
	      true;
	    false -> false
	  end;
	false -> true
      end;
    _ -> false
  end.

%%% Make Reg a clone of Dst (attach Dst's type to Reg).

clone(Dst, Strategy) ->
  Type =
    case Dst of
      #x86_mem{} -> hipe_x86:mem_type(Dst);
      #x86_temp{} -> hipe_x86:temp_type(Dst)
    end,
  spill_temp(Type, Strategy).

spill_temp(Type, 'normal') ->
  hipe_x86:mk_new_temp(Type);
spill_temp(double, 'linearscan') ->
  hipe_x86:mk_temp(hipe_amd64_specific_sse2:temp0(no_context), double);
spill_temp(Type, 'linearscan') when Type =:= tagged; Type =/= untagged ->
  %% We can make a new temp here since we have yet to allocate registers for
  %% these types
  hipe_x86:mk_new_temp(Type).

%%% Make a certain reg into a clone of Dst

%% clone2(Dst, Reg) ->
%%   Type =
%%     case Dst of
%%       #x86_mem{} -> hipe_x86:mem_type(Dst);
%%       #x86_temp{} -> hipe_x86:temp_type(Dst)
%%     end,
%%   hipe_x86:mk_temp(Reg,Type).
