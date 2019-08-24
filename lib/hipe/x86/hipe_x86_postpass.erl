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
%%%----------------------------------------------------------------------
%%% File    : hipe_x86_postpass.erl
%%% Author  : Christoffer Vikström <chvi3471@student.uu.se>
%%% Purpose : Contain postpass optimisations for x86-assembler code.
%%% Created : 5 Aug 2003 by Christoffer Vikström <chvi3471@student.uu.se>
%%%----------------------------------------------------------------------

-ifndef(HIPE_X86_POSTPASS).
-define(HIPE_X86_POSTPASS, hipe_x86_postpass).
-endif.

-module(?HIPE_X86_POSTPASS).
-export([postpass/2]).
-include("../x86/hipe_x86.hrl").

%%>----------------------------------------------------------------------<
%  Procedure : postpass/2
%  Purpose   : Function that performs a nr of postpass optimizations on
%              the hipe x86-assembler code before it is encoded and loaded.
%%>----------------------------------------------------------------------<
postpass(#defun{code=Code0}=Defun, Options) ->
  Code1 = pseudo_insn_expansion(Code0),
  Code2 = case proplists:get_bool(peephole, Options) of
	    true  -> peephole_optimization(Code1);
	    false -> Code1
	  end,
  Code3 = trivial_goto_elimination(Code2),
  Defun#defun{code=Code3}.


%%>----------------------------------------------------------------------<
%  Procedure : peep/1
%  Purpose   : Function that does peephole optimizations. It works by 
%              moving a window over the code and looking at a sequence of 
%              a few instructions. Replaces long sequences of instructions
%              with shorter ones and removes unnecesary ones.
%  Arguments : Insns   - List of pseudo x86-assembler records.
%              Res     - Returned list of pseudo x86-assembler records. 
%                        Kept reversed, until it is returned.
%  Return    : An optimized list of pseudo x86-assembler records with 
%              (hopefully) fewer or faster instructions.
%%>----------------------------------------------------------------------< 
peephole_optimization(Insns) -> 
  peep(Insns, [], []).


%% MoveSelf related peep-opts
%% ------------------------------
peep([#fmove{src=Src, dst=Src} | Insns], Res,Lst) ->
    peep(Insns, Res, [moveSelf1|Lst]);
peep([I=#fmove{src=Src, dst=Dst}, 
      #fmove{src=Dst, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, [I|Res], [moveSelf2|Lst]);
peep([#movsx{src=Src, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, Res, [moveSelf3|Lst]);
peep([I=#movsx{src=Src, dst=Dst}, 
      #movsx{src=Dst, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, [I|Res], [moveSelf4|Lst]);
peep([#movzx{src=Src, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, Res, [moveSelf5|Lst]);
peep([I=#movzx{src=Src, dst=Dst}, 
      #movzx{src=Dst, dst=Src} | Insns], Res,Lst) ->
    peep(Insns, [I|Res], [moveSelf6|Lst]);
peep([#cmovcc{src=Src, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, Res, [moveSelf7|Lst]);
peep([I=#cmovcc{src=Src, dst=Dst}, 
      #cmovcc{src=Dst, dst=Src}|Insns], Res,Lst) -> 
    peep(Insns, [I|Res], [moveSelf8|Lst]);
peep([#move{src=#x86_temp{reg=X}, 
	    dst=#x86_temp{reg=X}} | Insns], Res,Lst) -> 
    peep(Insns, Res, [moveSelf9|Lst]);
peep([I=#move{src=#x86_temp{reg=Src}, dst=#x86_temp{reg=Dst}}, 
      #move{src=#x86_temp{reg=Dst}, dst=#x86_temp{reg=Src}} | Insns], Res,Lst) -> 
    peep(Insns, [I|Res], [moveSelf0|Lst]);


%% ElimBinALMDouble
%% ----------------
peep([Move=#move{src=Src, dst=Dst}, Alu=#alu{src=Src, dst=Dst}|Insns], Res, Lst)
  when not is_record(Dst, x86_mem) ->
  peep([Alu#alu{src=Dst}|Insns], [Move|Res], [elimBinALMDouble|Lst]);


%% ElimFBinDouble
%% --------------
peep([Move=#fmove{src=Src, dst=Dst}, 
      BinOp=#fp_binop{src=Src, dst=Dst}|Insns], Res, Lst) ->
  peep([BinOp#fp_binop{src=Dst}|Insns], [Move|Res], [elimFBinDouble|Lst]);


%% CommuteBinALMD
%% --------------
peep([#move{src=Src1, dst=Dst}, 
      #alu{aluop=Op,src=Src2,dst=Dst}|Insns], Res, Lst)
  when (Src1 =:= #x86_imm{}) and (Src2 =/= #x86_imm{}) and 
       ((Op =:= 'add') or (Op =:= 'and') or (Op =:= 'or') or (Op =:= 'xor'))  ->
  peep(Insns, [#alu{aluop=Op,src=Src1,dst=Dst},
	       #move{src=Src2, dst=Dst}|Res], 
       [commuteBinALMD|Lst]);


%% ElimCmp0
%% --------
peep([#cmp{src=#x86_imm{value=0}, dst=Dst=#x86_temp{}}|Insns],Res,Lst) ->
  %% TEST leaves the adjust flag undefined, whereas CMP sets it properly (in
  %% this case to 0). However, since HiPE does not use any instructions that
  %% read the adjust flag, we can do this transform safely.
  peep(Insns, [#test{src=Dst, dst=Dst} | Res], [elimCmp0_1|Lst]);
peep([#cmp{src=Src=#x86_temp{}, dst=#x86_imm{value=0}},
      J=#jcc{cc=Cond}|Insns],Res,Lst)
  when Cond =:= 'e'; Cond =:= 'ne' -> % We're commuting the comparison
  peep(Insns, [J, #test{src=Src, dst=Src} | Res], [elimCmp0_2|Lst]);

%% ElimCmpTest
%% -----------
peep([I|Insns],Res,Lst) when (I =:= #cmp{}) or (I =:= #test{}) -> 
    case check(Insns) of
	#jcc{} ->
	    peep(Insns, [I|Res], Lst);
	#jmp_fun{} ->
	    peep(Insns, [I|Res], Lst);
	#jmp_label{} ->
	    peep(Insns, [I|Res], Lst);
	#jmp_switch{} ->
	    peep(Insns, [I|Res], Lst);
	#cmovcc{} ->
	    peep(Insns, [I|Res], Lst);
	#ret{} ->
	    peep(Insns, [I|Res], Lst);
	_ ->
	    peep(Insns, Res, [elimCmpTest|Lst])
    end;


%% ElimPushPop
%% -----------
peep([#push{src=Opr}, #pop{dst=Opr} | Insns], Res, Lst) ->
    peep(Insns, Res, [elimPushPop|Lst]);


% %% ElimIFF
% %% -------
peep([#jcc{label=Lab}, I=#label{label=Lab}|Insns], Res, Lst) ->
     peep(Insns, [I, #jmp_label{label=Lab}|Res], [elimIFF|Lst]);


%% ElimSet0
%% --------
peep([#move{src=#x86_imm{value=0},dst=Dst=#x86_temp{}}|Insns],Res,Lst) ->
  peep(Insns, [#alu{aluop='xor', src=Dst, dst=Dst}|Res], [elimSet0|Lst]);    

%% ElimMDPow2
%% ----------
peep([B = #alu{aluop=Op,src=#x86_imm{value=Val},dst=Dst}|Insns], Res, Lst) ->
    {IsLog2, Size, Sign} = log2(Val),
    case ((Op =:= imul) or (Op =:= idiv)) and IsLog2 of
	true ->
	    Sh = case Sign of positive -> 'bsl'; negative -> 'bsr' end,
	    peep(Insns, 
		 [#shift{shiftop=Sh, src=#x86_imm{value=Size}, dst=Dst}|Res], 
		 [elimMDPow2|Lst]);
	false ->
	    peep(Insns, [B|Res], Lst)
    end;

%% LeaToAdd
%% This rule transforms lea into add when the destination is the same as one of
%% the operands. Sound because lea is never used where the condition codes are
%% live (and would be clobbered by add).
%% ----------
peep([#lea{mem=#x86_mem{base=#x86_temp{reg=DstR},off=Src},
	   temp=Dst=#x86_temp{reg=DstR}}|Insns], Res, Lst) ->
     peep(Insns, [#alu{aluop='add',src=Src,dst=Dst}|Res], [leaToAdd|Lst]);
peep([#lea{mem=#x86_mem{base=Src,off=#x86_temp{reg=DstR}},
	   temp=Dst=#x86_temp{reg=DstR}}|Insns], Res, Lst) ->
     peep(Insns, [#alu{aluop='add',src=Src,dst=Dst}|Res], [leaToAdd|Lst]);

%% SubToDec
%% This rule turns "subl $1,Dst; jl Lab" into "decl Dst; jl Lab", which
%% changes reduction counter tests to use decl instead of subl.
%% However, on Athlon64 this leads to a small but measurable decrease
%% in performance. The use of dec is also not recommended on P4, so
%% this transformation is disabled.
%% peep([#alu{aluop='sub',src=#x86_imm{value=1},dst=Dst},J=#jcc{cc='l'}|Insns], Res, Lst) ->
%%   peep(Insns, [J, #dec{dst=Dst} | Res], [subToDec|Lst]);

%% Standard list recursion clause
%% ------------------------------
peep([I | Insns], Res, Lst) ->
     peep(Insns, [I|Res], Lst);
peep([], Res, _Lst) ->
    lists:reverse(Res). 

%% Simple goto elimination
%% -----------------------
trivial_goto_elimination(Insns) -> goto_elim(Insns, []).

goto_elim([#jmp_label{label=Label}, I = #label{label=Label}|Insns], Res) ->
  goto_elim([I|Insns], Res);
goto_elim([#jcc{cc=CC, label=Label} = IJCC,
	   #jmp_label{label=BranchTgt},
	   #label{label=Label} = ILBL|Insns], Res) ->
  goto_elim([IJCC#jcc{cc=hipe_x86:neg_cc(CC), label=BranchTgt},
	     ILBL|Insns], Res);
goto_elim([I | Insns], Res) ->
  goto_elim(Insns, [I|Res]);
goto_elim([], Res) ->
  lists:reverse(Res). 


%%>----------------------------------------------------------------------<
%%  Procedure : expand/1
%%  Purpose   : Expands pseudo instructions.
%%  Arguments : Insns - An x86-instruction list.
%%  Return    : An expanded instruction list.
%%  Notes     : 
%%>----------------------------------------------------------------------<
pseudo_insn_expansion(Insns) -> expand(Insns, []).
expand([I|Tail], Res) ->
    case I of
	#pseudo_jcc{cc=Cc,true_label=TrueLab,false_label=FalseLab} ->
	    expand(Tail, [hipe_x86:mk_jmp_label(FalseLab),
			  hipe_x86:mk_jcc(Cc, TrueLab) | Res]);
	#pseudo_tailcall_prepare{} ->
	    expand(Tail, Res);
	#pseudo_call{'fun'=Fun,sdesc=SDesc,contlab=ContLab,linkage=Linkage} ->
	    expand(Tail, [hipe_x86:mk_jmp_label(ContLab),
			  hipe_x86:mk_call(Fun, SDesc, Linkage) | Res]);
	_ ->
	    expand(Tail, [I|Res])
    end;
expand([], Res) -> lists:reverse(Res).

%% Log2 function
%% -------------
%% Used by ElimMDPow2 clause of peep(..)
log2(Nr) -> log2(Nr, 0).
log2(0, _) -> {false, 0, positive};
log2(Nr, I) ->
    case (Nr band 1) =:= 1 of
	true ->
	    case Nr of
		1 ->
		    {true, I, positive};
		-1 ->
		    {true, I, negative};
		_ ->
		    {false, 0, positive}
	    end;
	false ->
	    log2((Nr bsr 1), I+1)
    end.

%% Skips through all comments and move instructions and returns the next one
%% -------------------------------------------------------------------------
%% Used by ElimCmpTest above.
check([I|Ins]) ->
    case I of
	#comment{} ->
	    check(Ins);
	#move{} ->
	    check(Ins);
	#fmove{} ->
	    check(Ins);
	#movsx{} ->
	    check(Ins);
	#movzx{} ->
	    check(Ins);
	OtherI ->
	    OtherI
    end.
