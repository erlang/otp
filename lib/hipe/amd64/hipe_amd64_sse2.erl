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
%% Fix {mem, mem} floating point operations that result from linear scan
%% allocated floats.

-module(hipe_amd64_sse2).

-export([map/1]).

-include("../x86/hipe_x86.hrl").
-include("../main/hipe.hrl").

%%----------------------------------------------------------------------

map(CFG) ->
  hipe_x86_cfg:map_bbs(fun do_bb/2, CFG).

do_bb(_Lbl, BB) ->
  Code = do_insns(hipe_bb:code(BB), []),
  hipe_bb:code_update(BB, Code).

do_insns([I|Insns], Accum) ->
  NewIs = do_insn(I),
  do_insns(Insns, lists:reverse(NewIs, Accum));
do_insns([], Accum) ->
  lists:reverse(Accum).

do_insn(I) ->
  case I of
    #fp_binop{} -> do_fp_binop(I);
    #fmove{}    -> do_fmove(I);
    _           -> [I]
  end.

do_fp_binop(I = #fp_binop{src=Src0,dst=Dst}) ->
  {FixSrc, Src} = fix_binary(Src0, Dst),
  FixSrc ++ [I#fp_binop{src=Src}].

do_fmove(I = #fmove{src=Src0,dst=Dst}) ->
  {FixSrc, Src} = fix_binary(Src0, Dst),
  FixSrc ++ [I#fmove{src=Src}].

fix_binary(Src0, Dst) ->
  case is_mem_opnd(Src0) of
    false -> {[], Src0};
    true ->
      case is_mem_opnd(Dst) of
	false -> {[], Src0};
	true ->
	  Src1 = spill_temp(),
	  {[hipe_x86:mk_fmove(Src0, Src1)], Src1}
      end
  end.

is_mem_opnd(#x86_fpreg{reg=Reg}) ->
  not hipe_amd64_registers:is_precoloured_sse2(Reg);
is_mem_opnd(#x86_temp{type=double, reg=Reg}) ->
  not hipe_amd64_registers:is_precoloured_sse2(Reg);
is_mem_opnd(#x86_temp{type=_, reg=Reg}) ->
  not hipe_amd64_registers:is_precoloured(Reg);
is_mem_opnd(#x86_mem{}) -> true.

spill_temp() ->
  hipe_x86:mk_temp(hipe_amd64_registers:sse2_temp0(), double).
