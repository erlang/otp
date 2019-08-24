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
%%-------------------------------------------------------------------
%% File        : hipe_rtl_symbolic.erl
%% Author      : Per Gustafsson <pergu@it.uu.se>
%% Description : Expansion of symbolic instructions.
%%
%% Created     : 18 May 2004 by Per Gustafsson <pergu@it.uu.se>
%%-------------------------------------------------------------------

-module(hipe_rtl_symbolic).

-export([expand/1]).

-include("hipe_rtl.hrl").
-include("hipe_literals.hrl").
-include("../icode/hipe_icode_primops.hrl").

expand(Cfg) ->
  Linear = hipe_rtl_cfg:linearize(Cfg),
  Code = hipe_rtl:rtl_code(Linear),
  NonFlatCode = [expand_instr(Instr) || Instr <- Code],
  NewCode = lists:flatten(NonFlatCode),
  Linear1 = hipe_rtl:rtl_code_update(Linear, NewCode),
  hipe_rtl_cfg:init(Linear1).

expand_instr(Instr) ->
  case Instr of
    #fixnumop{} ->
     expand_fixnumop(Instr);
    #gctest{} ->
      expand_gctest(Instr);
    _ ->
      Instr
  end.

expand_fixnumop(Instr) ->
  case hipe_rtl:fixnumop_type(Instr) of
    untag ->
      Dst = hipe_rtl:fixnumop_dst(Instr),
      Src = hipe_rtl:fixnumop_src(Instr),
      hipe_tagscheme:realuntag_fixnum(Dst, Src);
    tag ->
      Dst = hipe_rtl:fixnumop_dst(Instr),
      Src = hipe_rtl:fixnumop_src(Instr),
      hipe_tagscheme:realtag_fixnum(Dst, Src)
  end.

expand_gctest(Instr) ->
  HeapNeed = hipe_rtl:gctest_words(Instr),
  {GetHPInsn, HP, _PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  {GetHLIMITInsn, H_LIMIT} = hipe_rtl_arch:heap_limit(),
  ContLabel = hipe_rtl:mk_new_label(),
  GCLabel = hipe_rtl:mk_new_label(),
  ContLabelName = hipe_rtl:label_name(ContLabel),
  GCLabelName = hipe_rtl:label_name(GCLabel),
  Tmp = hipe_rtl:mk_new_reg(), % diff between two gc-unsafe pointers
  StartCode = 
    [GetHPInsn, 
     GetHLIMITInsn,
     hipe_rtl:mk_alu(Tmp, H_LIMIT, 'sub', HP)],
  {SeparateCode, GCAmount, HPAmount} =
    case hipe_rtl:is_reg(HeapNeed) of	
      true ->
	GA = hipe_rtl:mk_new_reg_gcsafe(),
	HA = hipe_rtl:mk_new_reg_gcsafe(),
	{[hipe_rtl:mk_alu(HA, HeapNeed, sll, 
			  hipe_rtl:mk_imm(hipe_rtl_arch:log2_word_size()))|
	  hipe_tagscheme:realtag_fixnum(GA, HeapNeed)], GA, HA};
      false ->
	WordsNeeded = hipe_rtl:imm_value(HeapNeed),
	GA = hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(WordsNeeded)),
	HA = hipe_rtl:mk_imm(WordsNeeded*hipe_rtl_arch:word_size()),
	{[], GA, HA}
    end,
  EndCode =
    [hipe_rtl:mk_branch(Tmp, 'lt', HPAmount, GCLabelName, ContLabelName, 0.01),
     GCLabel,
     hipe_rtl:mk_call([], 'gc_1', [GCAmount], ContLabelName, [], not_remote),
     ContLabel],
  StartCode ++ SeparateCode ++ EndCode.

