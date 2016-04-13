%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% %CopyrightEnd%
%%
%% representation of 2-address pseudo-amd64 code

-module(hipe_x86).

-include("hipe_x86.hrl").

%% Commented out are interface functions which are currently not used.
-export([mk_temp/2,
	 %% mk_nonallocatable_temp/2,
	 mk_new_temp/1,
	 mk_new_nonallocatable_temp/1,
	 is_temp/1,
	 temp_reg/1,
	 temp_type/1,
	 temp_is_allocatable/1,

	 mk_imm/1,
	 mk_imm_from_addr/2,
	 mk_imm_from_atom/1,
	 is_imm/1,
	 %% imm_value/1,

	 mk_mem/3,
	 %% is_mem/1,
	 %% mem_base/1,
	 %% mem_off/1,
	 mem_type/1,

	 mk_fpreg/1,
 	 mk_fpreg/2,
	 %% is_fpreg/1,
	 %% fpreg_is_pseudo/1,
	 %% fpreg_reg/1,

	 mk_mfa/3,
	 %% is_mfa/1,

	 mk_prim/1,
	 is_prim/1,
	 prim_prim/1,

	 mk_sdesc/4,

	 %% insn_type/1,

	 mk_alu/3,
	 %% is_alu/1,
	 alu_op/1,
	 alu_src/1,
	 alu_dst/1,

	 mk_call/3,
	 %% is_call/1,
	 call_fun/1,
	 call_sdesc/1,
	 call_linkage/1,

	 %% mk_cmovcc/3,
	 %% is_cmovcc/1,
	 cmovcc_cc/1,
	 cmovcc_src/1,
	 cmovcc_dst/1,

	 mk_cmp/2,
	 %% is_cmp/1,
	 cmp_src/1,
	 cmp_dst/1,

	 mk_comment/1,
	 %% is_comment/1,
	 %% comment_term/1,

	 mk_fmove/2,
	 is_fmove/1,
	 fmove_src/1,
	 fmove_dst/1,

	 mk_fp_unop/2,
 	 %% is_fp_unop/1,
	 fp_unop_arg/1,
	 fp_unop_op/1,

	 mk_fp_binop/3,
	 %% is_fp_binop/1,
	 fp_binop_src/1,
	 fp_binop_dst/1,
	 fp_binop_op/1,

	 mk_imul/3,
	 imul_imm_opt/1,
	 imul_src/1,
	 imul_temp/1,

	 mk_jcc/2,
	 %% is_jcc/1,
	 jcc_cc/1,
	 jcc_label/1,

	 mk_jmp_fun/2,
	 %% is_jmp_fun/1,
	 jmp_fun_fun/1,
	 jmp_fun_linkage/1,

	 mk_jmp_label/1,
	 %% is_jmp_label/1,
	 jmp_label_label/1,

	 mk_jmp_switch/3,
	 %% is_jmp_switch/1,
	 jmp_switch_temp/1,
	 jmp_switch_jtab/1,
	 %% jmp_switch_labels/1,

	 mk_label/1,
	 is_label/1,
	 label_label/1,

	 mk_lea/2,
	 %% is_lea/1,
	 lea_mem/1,
	 lea_temp/1,

	 mk_move/2,
	 is_move/1,
	 move_src/1,
	 move_dst/1,
	 mk_move64/2,
	 %% is_move64/1,
	 move64_src/1,
	 move64_dst/1,

	 mk_movsx/2,
	 %% is_movsx/1,
	 movsx_src/1,
	 movsx_dst/1,

	 mk_movzx/2,
	 %% is_movzx/1,
	 movzx_src/1,
	 movzx_dst/1,

	 mk_pseudo_call/4,
 	 %% is_pseudo_call/1,
	 pseudo_call_fun/1,
	 pseudo_call_sdesc/1,
	 pseudo_call_contlab/1,
	 pseudo_call_linkage/1,

	 mk_pseudo_jcc/4,
	 %% is_pseudo_jcc/1,
	 %% pseudo_jcc_cc/1,
	 %% pseudo_jcc_true_label/1,
	 %% pseudo_jcc_false_label/1,
	 %% pseudo_jcc_pred/1,

     mk_pseudo_spill/1,

	 mk_pseudo_tailcall/4,
	 %% is_pseudo_tailcall/1,
	 pseudo_tailcall_fun/1,
	 %% pseudo_tailcall_arity/1,
	 pseudo_tailcall_stkargs/1,
	 pseudo_tailcall_linkage/1,

	 mk_pseudo_tailcall_prepare/0,
	 %% is_pseudo_tailcall_prepare/1,

	 mk_push/1,
	 %% is_push/1,
	 push_src/1,

	 %% mk_pop/1,
	 pop_dst/1,

	 mk_ret/1,
	 %% is_ret/1,
	 ret_npop/1,

	 mk_shift/3,
	 %% is_shift/1,
	 shift_op/1,
	 shift_src/1,
	 shift_dst/1,

	 %% mk_test/2,
	 test_src/1,
	 test_dst/1,

	 mk_defun/8,
	 defun_mfa/1,
	 defun_formals/1,
	 defun_is_closure/1,
	 defun_is_leaf/1,
	 defun_code/1,
	 defun_data/1,
	 defun_var_range/1
	 %% defun_label_range/1,

	 %% highest_temp/1
	]).

%%%
%%% Low-level accessors.
%%%

mk_temp(Reg, Type) when is_integer(Reg) ->
    #x86_temp{reg=Reg, type=Type, allocatable=true}.
mk_nonallocatable_temp(Reg, Type) when is_integer(Reg) ->
    #x86_temp{reg=Reg, type=Type, allocatable=false}.
mk_new_temp(Type) ->
    mk_temp(hipe_gensym:get_next_var(x86), Type).
mk_new_nonallocatable_temp(Type) ->
   mk_nonallocatable_temp(hipe_gensym:get_next_var(x86), Type).
is_temp(X) -> case X of #x86_temp{} -> true; _ -> false end.
temp_reg(#x86_temp{reg=Reg}) when is_integer(Reg) -> Reg.
temp_type(#x86_temp{type=Type}) -> Type.
temp_is_allocatable(#x86_temp{allocatable=A}) -> A.

mk_imm(Value) -> #x86_imm{value=Value}.
mk_imm_from_addr(Addr, Type) ->
    mk_imm({Addr, Type}).
mk_imm_from_atom(Atom) ->
    mk_imm(Atom).
is_imm(X) -> case X of #x86_imm{} -> true; _ -> false end.
%% imm_value(#x86_imm{value=Value}) -> Value.

mk_mem(Base, Off, Type) -> #x86_mem{base=Base, off=Off, type=Type}.
%% is_mem(X) -> case X of #x86_mem{} -> true; _ -> false end.
%% mem_base(#x86_mem{base=Base}) -> Base.
%% mem_off(#x86_mem{off=Off}) -> Off.
mem_type(#x86_mem{type=Type}) -> Type.

mk_fpreg(Reg) -> #x86_fpreg{reg=Reg, pseudo=true}.
mk_fpreg(Reg, Pseudo) -> #x86_fpreg{reg=Reg, pseudo=Pseudo}.
%% is_fpreg(F) -> case F of #x86_fpreg{} -> true;_ -> false end.
%% fpreg_is_pseudo(#x86_fpreg{pseudo=Pseudo}) -> Pseudo.
%% fpreg_reg(#x86_fpreg{reg=Reg}) -> Reg.

mk_mfa(M, F, A) -> #x86_mfa{m=M, f=F, a=A}.
%% is_mfa(X) -> case X of #x86_mfa{} -> true; _ -> false end.

mk_prim(Prim) -> #x86_prim{prim=Prim}.
is_prim(X) -> case X of #x86_prim{} -> true; _ -> false end.
prim_prim(#x86_prim{prim=Prim}) -> Prim.

mk_sdesc(ExnLab, FSize, Arity, Live) ->
    #x86_sdesc{exnlab=ExnLab, fsize=FSize, arity=Arity, live=Live}.

insn_type(Insn) ->
    element(1, Insn).

is_insn_type(Insn, Type) ->
    case insn_type(Insn) of
	Type -> true;
	_ -> false
    end.

mk_alu(Op, Src, Dst) -> #alu{aluop=Op, src=Src, dst=Dst}.
%% is_alu(Insn) -> is_insn_type(Insn, alu).
alu_op(#alu{aluop=Op}) -> Op.
alu_src(#alu{src=Src}) -> Src.
alu_dst(#alu{dst=Dst}) -> Dst.

mk_call(Fun, SDesc, Linkage) ->
    check_linkage(Linkage),
    #call{'fun'=Fun, sdesc=SDesc, linkage=Linkage}.
%% is_call(Insn) -> is_insn_type(Insn, call).
call_fun(#call{'fun'=Fun}) -> Fun.
call_sdesc(#call{sdesc=SDesc}) -> SDesc.
call_linkage(#call{linkage=Linkage}) -> Linkage.

check_linkage(Linkage) ->
    case Linkage of
	remote -> [];
	not_remote -> []
    end.

%% mk_cmovcc(Cc, Src, Dst) -> #cmovcc{cc=Cc, src=Src, dst=Dst}.
%% is_cmovcc(Insn) -> is_insn_type(Insn, cmovcc).
cmovcc_cc(#cmovcc{cc=Cc}) -> Cc.
cmovcc_src(#cmovcc{src=Src}) -> Src.
cmovcc_dst(#cmovcc{dst=Dst}) -> Dst.

mk_cmp(Src, Dst) -> #cmp{src=Src, dst=Dst}.
%% is_cmp(Insn) -> is_insn_type(Insn, cmp).
cmp_src(#cmp{src=Src}) -> Src.
cmp_dst(#cmp{dst=Dst}) -> Dst.

%% mk_test(Src, Dst) -> #test{src=Src, dst=Dst}.
test_src(#test{src=Src}) -> Src.
test_dst(#test{dst=Dst}) -> Dst.

mk_comment(Term) -> #comment{term=Term}.
%% is_comment(Insn) -> is_insn_type(Insn, comment).
%% comment_term(#comment{term=Term}) -> Term.

mk_fmove(Src, Dst) -> #fmove{src=Src, dst=Dst}.
is_fmove(F) -> is_insn_type(F, fmove).
fmove_src(#fmove{src=Src}) -> Src.
fmove_dst(#fmove{dst=Dst}) -> Dst.

mk_fp_unop(Op, Arg) -> #fp_unop{op=Op, arg=Arg}.
%% is_fp_unop(F) -> is_insn_type(F, fp_unop).
fp_unop_arg(#fp_unop{arg=Arg}) -> Arg.
fp_unop_op(#fp_unop{op=Op}) -> Op.

mk_fp_binop(Op, Src, Dst) -> #fp_binop{op=Op, src=Src, dst=Dst}.
%% is_fp_binop(F) -> is_insn_type(F, fp_binop).
fp_binop_src(#fp_binop{src=Src}) -> Src.
fp_binop_dst(#fp_binop{dst=Dst}) -> Dst.
fp_binop_op(#fp_binop{op=Op}) -> Op.

mk_imul(ImmOpt, Src, Temp) -> #imul{imm_opt=ImmOpt, src=Src, temp=Temp}.
imul_imm_opt(#imul{imm_opt=ImmOpt}) -> ImmOpt.
imul_src(#imul{src=Src}) -> Src.
imul_temp(#imul{temp=Temp}) -> Temp.

mk_jcc(Cc, Label) -> #jcc{cc=Cc, label=Label}.
%% is_jcc(Insn) -> is_insn_type(Insn, jcc).
jcc_cc(#jcc{cc=Cc}) -> Cc.
jcc_label(#jcc{label=Label}) -> Label.

mk_jmp_fun(Fun, Linkage) ->
    check_linkage(Linkage),
    #jmp_fun{'fun'=Fun, linkage=Linkage}.
%% is_jmp_fun(Insn) -> is_insn_type(Insn, jmp_fun).
jmp_fun_fun(#jmp_fun{'fun'=Fun}) -> Fun.
jmp_fun_linkage(#jmp_fun{linkage=Linkage}) -> Linkage.

mk_jmp_label(Label) -> #jmp_label{label=Label}.
%% is_jmp_label(Insn) -> is_insn_type(Insn, jmp_label).
jmp_label_label(#jmp_label{label=Label}) -> Label.

mk_jmp_switch(Temp, JTab, Labels) ->
    #jmp_switch{temp=Temp, jtab=JTab, labels=Labels}.
%% is_jmp_switch(Insn) -> is_insn_type(Insn, jmp_switch).
jmp_switch_temp(#jmp_switch{temp=Temp}) -> Temp.
jmp_switch_jtab(#jmp_switch{jtab=JTab}) -> JTab.
%% jmp_switch_labels(#jmp_switch{labels=Labels}) -> Labels.

mk_label(Label) -> #label{label=Label}.
is_label(Insn) -> is_insn_type(Insn, label).
label_label(#label{label=Label}) -> Label.

mk_lea(Mem, Temp) -> #lea{mem=Mem, temp=Temp}.
%% is_lea(Insn) -> is_insn_type(Insn, lea).
lea_mem(#lea{mem=Mem}) -> Mem.
lea_temp(#lea{temp=Temp}) -> Temp.

mk_move(Src, Dst) -> #move{src=Src, dst=Dst}.
is_move(Insn) -> is_insn_type(Insn, move).
move_src(#move{src=Src}) -> Src.
move_dst(#move{dst=Dst}) -> Dst.

mk_move64(Imm, Dst) -> #move64{imm=Imm, dst=Dst}.
%% is_move64(Insn) -> is_insn_type(Insn, move64).
move64_src(#move64{imm=Imm}) -> Imm.
move64_dst(#move64{dst=Dst}) -> Dst.

mk_movsx(Src, Dst) -> #movsx{src=Src, dst=Dst}.
%% is_movsx(Insn) -> is_insn_type(Insn, movsx).
movsx_src(#movsx{src=Src}) -> Src.
movsx_dst(#movsx{dst=Dst}) -> Dst.

mk_movzx(Src, Dst) -> #movzx{src=Src, dst=Dst}.
%% is_movzx(Insn) -> is_insn_type(Insn, movzx).
movzx_src(#movzx{src=Src}) -> Src.
movzx_dst(#movzx{dst=Dst}) -> Dst.

mk_pseudo_call(Fun, SDesc, ContLab, Linkage) ->
    check_linkage(Linkage),
    #pseudo_call{'fun'=Fun, sdesc=SDesc, contlab=ContLab, linkage=Linkage}.
%% is_pseudo_call(Insn) -> is_insn_type(Insn, pseudo_call).
pseudo_call_fun(#pseudo_call{'fun'=Fun}) -> Fun.
pseudo_call_sdesc(#pseudo_call{sdesc=SDesc}) -> SDesc.
pseudo_call_contlab(#pseudo_call{contlab=ContLab}) -> ContLab.
pseudo_call_linkage(#pseudo_call{linkage=Linkage}) -> Linkage.

mk_pseudo_jcc(Cc, TrueLabel, FalseLabel, Pred) ->	% 'smart' constructor
    if Pred >= 0.5 ->
	    mk_pseudo_jcc_simple(neg_cc(Cc), FalseLabel, TrueLabel, 1.0-Pred);
       true ->
	    mk_pseudo_jcc_simple(Cc, TrueLabel, FalseLabel, Pred)
    end.
neg_cc(Cc) ->
    case Cc of
	'e'	-> 'ne';	% ==, !=
	'ne'	-> 'e';		% !=, ==
	'g'	-> 'le';	% >, <=
	'a'	-> 'be';	% >u, <=u
	'ge'	-> 'l';		% >=, <
	'ae'	-> 'b';		% >=u, <u
	'l'	-> 'ge';	% <, >=
	'b'	-> 'ae';	% <u, >=u
	'le'	-> 'g';		% <=, >
	'be'	-> 'a';		% <=u, >u
	'o'	-> 'no';	% overflow, not_overflow
	'no'	-> 'o';		% not_overflow, overflow
	_	-> exit({?MODULE, {"unknown cc", Cc}})
    end.
mk_pseudo_jcc_simple(Cc, TrueLabel, FalseLabel, Pred) ->
    #pseudo_jcc{cc=Cc, true_label=TrueLabel, false_label=FalseLabel, pred=Pred}.
%% is_pseudo_jcc(Insn) -> is_insn_type(Insn, pseudo_jcc).
%% pseudo_jcc_cc(#pseudo_jcc{cc=Cc}) -> Cc.
%% pseudo_jcc_true_label(#pseudo_jcc{true_label=TrueLabel}) -> TrueLabel.
%% pseudo_jcc_false_label(#pseudo_jcc{false_label=FalseLabel}) -> FalseLabel.
%% pseudo_jcc_pred(#pseudo_jcc{pred=Pred}) -> Pred.

mk_pseudo_spill(List) ->
    #pseudo_spill{args=List}.

mk_pseudo_tailcall(Fun, Arity, StkArgs, Linkage) ->
    check_linkage(Linkage),
    #pseudo_tailcall{'fun'=Fun, arity=Arity, stkargs=StkArgs, linkage=Linkage}.
%% is_pseudo_tailcall(Insn) -> is_insn_type(Insn, pseudo_tailcall).
pseudo_tailcall_fun(#pseudo_tailcall{'fun'=Fun}) -> Fun.
%% pseudo_tailcall_arity(#pseudo_tailcall{arity=Arity}) -> Arity.
pseudo_tailcall_stkargs(#pseudo_tailcall{stkargs=StkArgs}) -> StkArgs.
pseudo_tailcall_linkage(#pseudo_tailcall{linkage=Linkage}) -> Linkage.

mk_pseudo_tailcall_prepare() -> #pseudo_tailcall_prepare{}.
%% is_pseudo_tailcall_prepare(Insn) -> is_insn_type(Insn, pseudo_tailcall_prepare).

mk_push(Src) -> #push{src=Src}.
%% is_push(Insn) -> is_insn_type(Insn, push).
push_src(#push{src=Src}) -> Src.

%% mk_pop(Dst) -> #pop{dst=Dst}.
%% is_push(Insn) -> is_insn_type(Insn, push).
pop_dst(#pop{dst=Dst}) -> Dst.

mk_ret(NPop) -> #ret{npop=NPop}.
%% is_ret(Insn) -> is_insn_type(Insn, ret).
ret_npop(#ret{npop=NPop}) -> NPop.

mk_shift(ShiftOp, Src, Dst) ->
  #shift{shiftop=ShiftOp, src=Src, dst=Dst}.
%% is_shift(Insn) -> is_insn_type(Insn, shift).
shift_op(#shift{shiftop=ShiftOp}) -> ShiftOp.
shift_src(#shift{src=Src}) -> Src.
shift_dst(#shift{dst=Dst}) -> Dst.

mk_defun(MFA, Formals, IsClosure, IsLeaf, Code, Data, VarRange, LabelRange) ->
  #defun{mfa=MFA, formals=Formals, code=Code, data=Data,
	  isclosure=IsClosure, isleaf=IsLeaf,
	  var_range=VarRange, label_range=LabelRange}.
defun_mfa(#defun{mfa=MFA}) -> MFA.
defun_formals(#defun{formals=Formals}) -> Formals.
defun_is_closure(#defun{isclosure=IsClosure}) -> IsClosure.
defun_is_leaf(#defun{isleaf=IsLeaf}) -> IsLeaf.
defun_code(#defun{code=Code}) -> Code.
defun_data(#defun{data=Data}) -> Data.
defun_var_range(#defun{var_range=VarRange}) -> VarRange.
%% defun_label_range(#defun{label_range=LabelRange}) -> LabelRange.

%% highest_temp(Code) ->
%%   highest_temp(Code,0).
%%
%% highest_temp([I|Is],Max) ->
%%   Defs = hipe_x86_defuse:insn_def(I),
%%   Uses = hipe_x86_defuse:insn_use(I),
%%   highest_temp(Is,new_max(Defs++Uses,Max));
%% highest_temp([],Max) ->
%%   Max.
%%
%% new_max([V|Vs],Max) ->
%%   case is_temp(V) of
%%     true ->
%%       TReg = temp_reg(V),
%%       if TReg > Max ->
%% 	  new_max(Vs, TReg);
%% 	 true ->
%% 	  new_max(Vs, Max)
%%       end;
%%     false ->
%%       new_max(Vs, Max)
%%   end;
%% new_max([],Max) -> Max.
