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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%
%% Provides an abstract datatype for HiPE's RTL (Register Transfer Language).
%%
%% <h3> RTL - Register Transfer Language </h3>
%%
%% Consists of the instructions:
%% <ul>
%%   <li> {alu, Dst, Src1, Op, Src2} </li>
%%   <li> {alub, Dst, Src1, Op, Src2, RelOp, TrueLabel, FalseLabel, P} </li>
%%   <li> {branch, Src1, Src2, RelOp, TrueLabel, FalseLabel, P} </li>
%%   <li> {call, DsListt, Fun, ArgList, Type, Continuation, FailContinuation, NormalContinuation}
%%           Type is one of {local, remote, primop, closure} </li>
%%   <li> {comment, Text} </li>
%%   <li> {enter, Fun, ArgList, Type}
%%           Type is one of {local, remote, primop, closure} </li>
%%   <li> {fconv, Dst, Src} </li>
%%   <li> {fload, Dst, Src, Offset} </li>
%%   <li> {fmove, Dst, Src} </li>
%%   <li> {fp, Dst, Src1, Op, Src2} </li>
%%   <li> {fp_unop, Dst, Src, Op} </li>
%%   <li> {fstore, Base, Offset, Src} </li>
%%   <li> {gctest, Words} </li>
%%   <li> {goto, Label} </li>
%%   <li> {goto_index, Block, Index, LabelList} </li>
%%   <li> {label, Name} </li>
%%   <li> {load, Dst, Src, Offset, Size, Sign} </li>
%%   <li> {load_address, Dst, Addr, Type} </li>
%%   <li> {load_atom, Dst, Atom} </li>
%%   <li> {load_word_index, Dst, Block, Index} </li>
%%   <li> {move, Dst, Src} </li>
%%   <li> {multimove, [Dst1, ..., DstN], [Src1, ..., SrcN]} </li>
%%   <li> {phi, Dst, Id, [Src1, ..., SrcN]} </li>
%%   <li> {return, VarList} </li>
%%   <li> {store, Base, Offset, Src, Size} </li>
%%   <li> {switch, Src1, Labels, SortedBy} </li>
%% </ul>
%%
%% There are three kinds of 'registers' in RTL.
%% <ol>
%%    <li> Variables containing tagged data that are traced by the GC. </li>
%%    <li> Registers that are ignored by the GC. </li>
%%    <li> Floating point registers. </li>
%% </ol>
%% These registers all share the same namespace.
%%
%% IMPORTANT: 
%%
%%     The variables contain tagged Erlang terms, the registers
%%     contain untagged values (that can be all sorts of things) and
%%     the floating point registers contain untagged floating point
%%     values. This means that the different kinds of 'registers' are
%%     incompatible and CANNOT be assigned to each other unless the
%%     proper conversions are made.
%%
%%     When performing optimizations, it is reasonably safe to move
%%     values stored in variables. However, when moving around untagged
%%     values from either registers or floating point registers make
%%     sure you know what you are doing. 
%%
%%     Example 1: A register might contain the untagged pointer to
%%                something on the heap. If this value is moved across
%%                a program point where a garbage collection might
%%                occur, the pointer can be invalid. If you are lucky
%%                you will end up with a segmentation fault; if unlucky,
%%                you will be stuck on a wild goose chase.
%%
%%     Example 2: Floating point arithmetic instructions must occur in
%%                a floating point block. Otherwise, exceptions can be
%%                masked.
%%
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl).
-include("../main/hipe.hrl").

-export([mk_rtl/8,
	 rtl_fun/1,
	 rtl_params/1,
	 rtl_is_closure/1,
	 rtl_is_leaf/1, 
	 rtl_code/1,
	 rtl_code_update/2,
	 rtl_data/1,
	 %% rtl_data_update/2,
	 %% rtl_var_range/1,
	 %% rtl_var_range_update/2,
	 rtl_label_range/1,
	 %% rtl_label_range_update/2,
	 rtl_info/1,
	 rtl_info_update/2]).

-export([mk_move/2,
	 move_dst/1,
	 move_src/1,
	 %% move_src_update/2,
	 %% is_move/1,

	 mk_multimove/2,
	 multimove_dstlist/1,
	 multimove_srclist/1,
	 %% multimove_srclist_update/2,
	 %% is_multimove/1,

	 mk_phi/1,
	 phi_dst/1,
	 phi_id/1,
	 phi_arg/2,
	 phi_arglist/1,
	 is_phi/1,
	 phi_enter_pred/3,
	 phi_remove_pred/2,

	 mk_alu/4,
	 alu_dst/1,
	 alu_src1/1,
	 alu_src1_update/2,
	 alu_src2/1,
	 alu_src2_update/2,
	 alu_op/1,
	 %% is_alu_op/1,
	 is_shift_op/1,

	 mk_load/3,
	 mk_load/5,
	 load_dst/1,
	 load_src/1,
	 load_offset/1,
	 load_size/1,
	 load_sign/1,

	 mk_load_atom/2,
	 load_atom_dst/1,
	 load_atom_atom/1,

	 mk_load_word_index/3,
	 load_word_index_dst/1,
	 %% load_word_index_index/1,
	 %% load_word_index_block/1,

	 mk_goto_index/3,
	 goto_index_index/1,
	 %% goto_index_block/1,
	 goto_index_labels/1,

	 mk_load_address/3,
	 load_address_dst/1,
	 %% load_address_dst_update/2,
	 load_address_addr/1,
	 load_address_addr_update/2,
	 load_address_type/1,
	 %% load_address_type_update/2,

	 mk_store/3,
	 mk_store/4,
	 store_base/1,
	 store_src/1,
	 store_offset/1,
	 store_size/1,

	 mk_label/1,
	 mk_new_label/0,
	 label_name/1,
	 is_label/1,

	 mk_branch/5,
	 mk_branch/6,
	 mk_branch/7,
	 %% is_branch/1,
	 %% branch_true_label_update/2,
	 %% branch_false_label_update/2,

	 mk_alub/7,
	 mk_alub/8,
	 alub_has_dst/1,
	 alub_dst/1,
	 alub_src1/1,
	 alub_op/1,
	 alub_src2/1,
	 alub_cond/1,
	 alub_true_label/1,
	 %% alub_true_label_update/2,
	 alub_false_label/1,
	 %% alub_false_label_update/2,
	 alub_pred/1,
	 %% is_alub/1,

	 mk_switch/2,
	 %% mk_switch/3,
	 mk_sorted_switch/3,
	 switch_src/1,
	 %% switch_src_update/2,
	 switch_labels/1,
	 %% switch_labels_update/2,
	 switch_sort_order/1,
	 %% switch_sort_order_update/2,

	 mk_goto/1,
	 goto_label/1,
	 is_goto/1,
	 %% goto_label_update/2,

	 mk_call/6,
	 mk_call/7,
	 call_fun/1,
	 call_dstlist/1,
	 call_dstlist_update/2,
	 call_arglist/1,
	 call_continuation/1,
	 call_fail/1,
	 call_type/1,
	 call_normal/1,
	 call_normal_update/2,
	 %% call_continuation_update/2,
	 call_fail_update/2,
	 is_call/1,

	 mk_enter/3,
	 enter_fun/1,
	 enter_arglist/1,
	 enter_type/1,

	 mk_return/1,
	 return_varlist/1,

	 mk_gctest/1,
	 gctest_words/1,
	 
	 mk_comment/1,
	 comment_text/1,
	 is_comment/1,

	 mk_fload/3,
	 fload_dst/1,
	 fload_src/1,
	 %% fload_src_update/2,
	 fload_offset/1,
	 %% fload_offset_update/2,

	 mk_fstore/3,
	 fstore_base/1,
	 fstore_src/1,
	 fstore_offset/1,

	 mk_fp/4,
	 fp_dst/1,
	 fp_src1/1,
	 %% fp_src1_update/2,
	 fp_src2/1,
	 %% fp_src2_update/2,
	 fp_op/1,

	 mk_fp_unop/3,
	 fp_unop_dst/1,
	 fp_unop_src/1,
	 %% fp_unop_src_update/2,
	 fp_unop_op/1,

	 mk_fmove/2,
	 fmove_dst/1,
	 fmove_src/1,
	 %% fmove_src_update/2,
	 %% is_fmove/1,

	 mk_fconv/2,
	 fconv_dst/1,
	 fconv_src/1,
	 %% fconv_src_update/2,
	 %% is_fconv/1,

	 mk_var/1,
	 mk_var/2,
	 mk_new_var/0,
	 is_var/1,
	 var_index/1,
	 var_liveness/1,
	 var_liveness_update/2,

	 %% change_vars_to_regs/1,
	 
	 mk_fixnumop/3,
	 fixnumop_dst/1,
	 fixnumop_src/1,
	 fixnumop_type/1,

	 mk_reg/1,		% assumes non gc-safe
	 mk_reg_gcsafe/1,
	 mk_new_reg/0,		% assumes non gc-safe
	 mk_new_reg_gcsafe/0,
	 is_reg/1,
	 reg_index/1,
	 reg_is_gcsafe/1,

	 %% mk_fpreg/1,
	 mk_new_fpreg/0,
	 is_fpreg/1,
	 fpreg_index/1,

	 mk_imm/1,
	 is_imm/1,
	 imm_value/1,

	 mk_const_label/1,
	 const_label_label/1,
	 is_const_label/1,

	 args/1,
	 uses/1,
	 %% subst/2,
	 subst_uses/2,
	 subst_defines/2,
	 defines/1,
	 redirect_jmp/3,
	 is_safe/1,
	 reduce_unused/1,
	 %% highest_var/1,
	 pp/1,
	 pp/2,
	 pp_block/1,

	 %% FIXME _dst_update command. Ok to export these?
	 alu_dst_update/2,
	 fconv_dst_update/2,
	 fload_dst_update/2,
	 %% fmove_dst_update/2,
	 fp_dst_update/2,
	 fp_unop_dst_update/2,
	 load_dst_update/2,
	 load_address_dst_update/2,
	 load_atom_dst_update/2,
	 load_word_index_dst_update/2,
	 %% move_dst_update/2,
	 fixnumop_dst_update/2,
	 pp_instr/2,
	 %% Uber hack!
	 pp_var/2,
	 pp_reg/2,
	 pp_arg/2,
         phi_arglist_update/2,
         phi_redirect_pred/3]).

-export([subst_uses_llvm/2]).

-export_type([alub_cond/0, rtl/0]).

%%
%% RTL
%%

-record(rtl, {'fun',        %% Name of the function (MFA)
	      arglist,      %% List of argument names (formals)
	      is_closure,   %% True if this is code for a closure.
	      is_leaf,      %% True if this is a leaf function.
	      code,         %% Linear list of RTL-instructions.
	      data,         %% Data segment
	      var_range,    %% {Min,Max} First and last name used for
	                    %%           regs, fpregs, or vars. 
	                    %%           (they use a common namespace)
	      label_range,  %% {Min,Max} First and last name used for labels
	      info=[]       %% A keylist with arbitrary information.
	     }).
-opaque rtl() :: #rtl{}.

mk_rtl(Fun, ArgList, Closure, Leaf, Code, Data, VarRange, LabelRange) ->
  #rtl{'fun'=Fun, arglist=ArgList, code=Code, 
       data=Data, is_closure=Closure, is_leaf=Leaf,
       var_range=VarRange, label_range=LabelRange}.
rtl_fun(#rtl{'fun'=Fun}) -> Fun.
rtl_params(#rtl{arglist=ArgList}) -> ArgList.
rtl_is_closure(#rtl{is_closure=Closure}) -> Closure.
rtl_is_leaf(#rtl{is_leaf=Leaf}) -> Leaf.
rtl_code(#rtl{code=Code}) -> Code.
rtl_code_update(Rtl, Code) -> Rtl#rtl{code=Code}.
rtl_data(#rtl{data=Data}) -> Data.
%% rtl_data_update(Rtl, Data) -> Rtl#rtl{data=Data}.
%% rtl_var_range(#rtl{var_range=VarRange}) -> VarRange.
%% rtl_var_range_update(Rtl, VarRange) -> Rtl#rtl{var_range=VarRange}.
rtl_label_range(#rtl{label_range=LabelRange}) -> LabelRange.
%% rtl_label_range_update(Rtl, LabelRange) -> Rtl#rtl{label_range=LabelRange}.
rtl_info(#rtl{info=Info}) -> Info.
rtl_info_update(Rtl, Info) -> Rtl#rtl{info=Info}.

%%-----------------------------------------------------------------------------

-include("hipe_rtl.hrl").

%%-----------------------------------------------------------------------------

%%
%% move
%%

mk_move(Dst, Src) ->
  false = is_fpreg(Dst), false = is_fpreg(Src),
  #move{dst=Dst, src=Src}.
move_dst(#move{dst=Dst}) -> Dst.
move_dst_update(M, NewDst) -> false = is_fpreg(NewDst), M#move{dst=NewDst}.
move_src(#move{src=Src}) -> Src.
move_src_update(M, NewSrc) -> false = is_fpreg(NewSrc), M#move{src=NewSrc}.
%% is_move(#move{}) -> true;
%% is_move(_) -> false.

%%
%% multimove
%%

mk_multimove(DstList, SrcList) -> 
  case length(DstList) =:= length(SrcList) of
    true -> true;
    false ->
      exit({?MODULE,mk_multimove,
	    {"different arities",{dstlist,DstList},{srclist,SrcList}}})
  end,
  #multimove{dstlist=DstList, srclist=SrcList}.
multimove_dstlist(#multimove{dstlist=DstList}) -> DstList.
multimove_dstlist_update(M, NewDstList) -> M#multimove{dstlist=NewDstList}.
multimove_srclist(#multimove{srclist=SrcList}) -> SrcList.
multimove_srclist_update(M, NewSrcList) -> M#multimove{srclist=NewSrcList}.
%% is_multimove(#multimove{}) -> true;
%% is_multimove(_) -> false.

%%
%% phi
%%

%% The id field is not entirely redundant. It is used in mappings
%% in the SSA pass since the dst field can change.
mk_phi(Var) -> #phi{dst = Var, id = Var, arglist = []}.
%% mk_phi(Var, ArgList) -> #phi{dst = Var, id = Var, arglist = ArgList}.
phi_dst(#phi{dst=Dst}) -> Dst.
phi_dst_update(Phi, NewDst) -> Phi#phi{dst = NewDst}.
phi_id(#phi{id=Id}) -> Id.
phi_args(Phi) -> [X || {_,X} <- phi_arglist(Phi)].
phi_arg(Phi, Pred) ->
  case lists:keyfind(Pred, 1, phi_arglist(Phi)) of
    false ->
      exit({?MODULE,phi_arg,{"Uknown Phi predecessor",Phi,{pred,Pred}}});
    {_, Var} -> Var
  end.
phi_arglist(#phi{arglist=ArgList}) -> ArgList.
phi_arglist_update(P,NewArgList) ->P#phi{arglist=NewArgList}.
is_phi(#phi{}) -> true;
is_phi(_) -> false.
phi_enter_pred(Phi, Pred, Var) ->
  Phi#phi{arglist=[{Pred,Var}|lists:keydelete(Pred, 1, phi_arglist(Phi))]}.
phi_remove_pred(Phi, Pred) ->
  NewArgList = lists:keydelete(Pred, 1, phi_arglist(Phi)),
  case NewArgList of
    [Arg] -> %% the phi should be turned into a move instruction
      {_Label,Var} = Arg,
      Dst = phi_dst(Phi),
      case {is_fpreg(Dst), is_fpreg(Var)} of
	{true, true} -> mk_fmove(Dst, Var);
	{false, false} -> mk_move(Dst, Var)
      end;
  %%    io:format("~nPhi (~w) turned into move (~w) when removing pred ~w~n",[Phi,Move,Pred]),
    [_|_] ->
      Phi#phi{arglist=NewArgList}
  end.
phi_argvar_subst(Phi, Subst) ->
  NewArgList = [{Pred,subst1(Subst, Var)} || {Pred,Var} <- phi_arglist(Phi)],
  Phi#phi{arglist=NewArgList}.
phi_redirect_pred(P, OldPred, NewPred)->
  Subst = [{OldPred, NewPred}],
  NewArgList = [{subst1(Subst, Pred), Var} || {Pred,Var} <- phi_arglist(P)],
  P#phi{arglist=NewArgList}.


%%
%% alu
%%

mk_alu(Dst, Src1, Op, Src2) -> 
  #alu{dst=Dst, src1=Src1, op=Op, src2=Src2}.
alu_dst(#alu{dst=Dst}) -> Dst.
alu_dst_update(Alu, NewDst) -> Alu#alu{dst=NewDst}.
alu_src1(#alu{src1=Src1}) -> Src1.
alu_src1_update(Alu, NewSrc) -> Alu#alu{src1=NewSrc}.
alu_src2(#alu{src2=Src2}) -> Src2.
alu_src2_update(Alu, NewSrc) -> Alu#alu{src2=NewSrc}.
alu_op(#alu{op=Op}) -> Op.

%%
%% load
%%

mk_load(Dst, Src, Offset) -> mk_load(Dst, Src, Offset, word, unsigned).
mk_load(Dst, Src, Offset, Size, Sign) ->
  ?ASSERT((Sign =:= unsigned) orelse (Sign =:= signed)),
  ?ASSERT((Size =:= word) orelse (Size =:= int32) orelse
          (Size =:= int16) orelse (Size =:= byte)),
  #load{dst=Dst, src=Src, offset=Offset, size=Size, sign=Sign}.
load_dst(#load{dst=Dst}) -> Dst.
load_dst_update(L, NewDst) -> L#load{dst=NewDst}.
load_src(#load{src=Src}) -> Src.
load_src_update(L, NewSrc) -> L#load{src=NewSrc}.
load_offset(#load{offset=Offset}) -> Offset.
load_offset_update(L, NewOffset) -> L#load{offset=NewOffset}.
load_size(#load{size=Size}) -> Size.
load_sign(#load{sign=Sign}) -> Sign.

%%
%% load_atom
%%

mk_load_atom(Dst, Atom) -> #load_atom{dst=Dst,atom=Atom}.
load_atom_dst(#load_atom{dst=Dst}) -> Dst.
load_atom_dst_update(L, NewDst) -> L#load_atom{dst=NewDst}.
load_atom_atom(#load_atom{atom=Atom}) -> Atom.

mk_load_word_index(Dst, Block, Index) -> 
  #load_word_index{dst=Dst, block=Block, index=Index}.
load_word_index_dst(#load_word_index{dst=Dst}) -> Dst.
load_word_index_dst_update(L, NewDst) -> L#load_word_index{dst=NewDst}.
load_word_index_block(#load_word_index{block=Block}) -> Block.
load_word_index_index(#load_word_index{index=Index}) -> Index.

mk_goto_index(Block, Index, Labels) -> 
  #goto_index{block=Block, index=Index, labels=Labels}.
goto_index_block(#goto_index{block=Block}) -> Block.
goto_index_index(#goto_index{index=Index}) -> Index.
goto_index_labels(#goto_index{labels=Labels}) -> Labels.

%%
%% load_address
%%

mk_load_address(Dst, Addr, Type) -> 
  #load_address{dst=Dst, addr=Addr, type=Type}.
load_address_dst(#load_address{dst=Dst}) -> Dst.
load_address_dst_update(LA, NewDst) -> LA#load_address{dst=NewDst}.
load_address_addr(#load_address{addr=Addr}) -> Addr.
load_address_addr_update(LoadAddress, NewAdr) -> 
  LoadAddress#load_address{addr=NewAdr}.
load_address_type(#load_address{type=Type}) -> Type.
%% load_address_type_update(LA, NewType) -> LA#load_address{type=NewType}.

%%
%% store
%%

mk_store(Base, Offset, Src) -> mk_store(Base, Offset, Src, word).
mk_store(Base, Offset, Src, Size) ->
  ?ASSERT((Size =:= word) orelse (Size =:= int32) orelse
          (Size =:= int16) orelse (Size =:= byte)),
  #store{base=Base, src=Src, offset=Offset, size=Size}.
store_base(#store{base=Base}) -> Base.
store_base_update(S, NewBase) -> S#store{base=NewBase}.
store_offset(#store{offset=Offset}) -> Offset.
store_offset_update(S, NewOffset) -> S#store{offset=NewOffset}.
store_src(#store{src=Src}) -> Src.
store_src_update(S, NewSrc) -> S#store{src=NewSrc}.
store_size(#store{size=Size}) -> Size.

%%
%% label
%%

mk_label(Name) -> #label{name=Name}.
mk_new_label() -> mk_label(hipe_gensym:get_next_label(rtl)).
label_name(#label{name=Name}) -> Name.
is_label(#label{}) -> true;
is_label(_) -> false.

%%
%% alub
%%

-type alub_cond() :: 'eq' | 'ne' | 'ge' | 'geu' | 'gt' | 'gtu' | 'le'
                   | 'leu' | 'lt' | 'ltu' | 'overflow' | 'not_overflow'.

mk_branch(Src1, Cond, Src2, True, False) ->
  mk_branch(Src1, Cond, Src2, True, False, 0.5).
mk_branch(Src1, Cond, Src2, True, False, P) ->
  mk_branch(Src1, 'sub', Src2, Cond, True, False, P).
mk_branch(Src1, Op, Src2, Cond, True, False, P) ->
  mk_alub([], Src1, Op, Src2, Cond, True, False, P).

mk_alub(Dst, Src1, Op, Src2, Cond, True, False) ->
  mk_alub(Dst, Src1, Op, Src2, Cond, True, False, 0.5).
mk_alub(Dst, Src1, Op, Src2, Cond, True, False, P) ->
  #alub{dst=Dst, src1=Src1, op=Op, src2=Src2, 'cond'=Cond,
	true_label=True, false_label=False, p=P}.
alub_has_dst(#alub{dst=Dst}) -> Dst =/= [].
alub_dst(#alub{dst=Dst}) -> Dst.
alub_dst_update(A, NewDst) -> A#alub{dst=NewDst}.
alub_src1(#alub{src1=Src1}) -> Src1.
alub_src1_update(A, NewSrc) -> A#alub{src1=NewSrc}.
alub_op(#alub{op=Op}) -> Op.
alub_src2(#alub{src2=Src2}) -> Src2.
alub_src2_update(A, NewSrc) -> A#alub{src2=NewSrc}.
alub_cond(#alub{'cond'=Cond}) -> Cond.
alub_true_label(#alub{true_label=TrueLbl}) -> TrueLbl.
alub_true_label_update(A, NewTrue) -> A#alub{true_label=NewTrue}.
alub_false_label(#alub{false_label=FalseLbl}) -> FalseLbl.
alub_false_label_update(A, NewFalse) -> A#alub{false_label=NewFalse}.
alub_pred(#alub{p=P}) -> P.

%%
%% switch
%%

mk_switch(Src, Labels) -> #switch{src=Src, labels=Labels}.
mk_sorted_switch(Src, Labels, Order) ->
  #switch{src=Src, labels=Labels, sorted_by=Order}.
switch_src(#switch{src=Src}) -> Src.
switch_src_update(I, N) -> I#switch{src=N}.
switch_labels(#switch{labels=Labels}) -> Labels.
switch_labels_update(I,N) -> I#switch{labels=N}.
switch_sort_order(#switch{sorted_by=Order}) -> Order.
%% switch_sort_order_update(I,N) -> I#switch{sorted_by=N}.

%%
%% goto
%%

mk_goto(Label) -> #goto{label=Label}.
goto_label(#goto{label=Label}) -> Label.
goto_label_update(I, NewLabel) -> 
  I#goto{label=NewLabel}.
is_goto(#goto{}) -> true;
is_goto(_) -> false.

%%
%% call
%%

%% LLVM: Call with normal continuation
mk_call(DstList, Fun, ArgList, Continuation, FailContinuation,
    NormalContinuation, Type) ->
  case Type of
    remote -> ok;
    not_remote -> ok
  end,
  #call{dstlist=DstList, 'fun'=Fun, arglist=ArgList, type=Type,
    continuation=Continuation, failcontinuation=FailContinuation,
    normalcontinuation=NormalContinuation}.

mk_call(DstList, Fun, ArgList, Continuation, FailContinuation, Type) ->
  case Type of
    remote -> ok;
    not_remote -> ok
  end,
  #call{dstlist=DstList, 'fun'=Fun, arglist=ArgList, type=Type,
	continuation=Continuation,
	failcontinuation=FailContinuation}.

call_normal(#call{normalcontinuation=NormalContinuation}) -> NormalContinuation.
call_normal_update(C, NewNormalContinuation) ->
  C#call{normalcontinuation=NewNormalContinuation}.
call_dstlist(#call{dstlist=DstList}) -> DstList.
call_dstlist_update(C, NewDstList) -> C#call{dstlist=NewDstList}.
call_fun(#call{'fun'=Fun}) -> Fun.
call_fun_update(C, F) -> C#call{'fun'=F}.
call_arglist(#call{arglist=ArgList}) -> ArgList.
call_arglist_update(C, NewArgList) -> C#call{arglist=NewArgList}.
call_continuation(#call{continuation=Continuation}) -> Continuation.
call_fail(#call{failcontinuation=FailContinuation}) -> FailContinuation.
call_type(#call{type=Type}) -> Type.
call_continuation_update(C, NewCont) -> C#call{continuation=NewCont}.
call_fail_update(C, NewCont) -> C#call{failcontinuation=NewCont}.
is_call(#call{}) -> true;
is_call(_) -> false.
call_is_known(C) ->
  Fun = call_fun(C),
  call_or_enter_fun_is_known(Fun).

call_or_enter_fun_is_known(Fun) ->
  case is_atom(Fun) of
    true -> true; %% make the expected common case fast
    false ->
      case is_reg(Fun) of
	true -> false;
	false ->
	  case is_var(Fun) of
	    true -> false;
	    false ->
	      case Fun of
		{M,F,A} when is_atom(M), is_atom(F), is_integer(A), A >= 0 ->
		  true;
		{F,A} when is_atom(F), is_integer(A), A >= 0 ->
		  true;
		_ -> %% colored versions of rtl_reg or rtl_var (used in SSA)
		  false
	      end
	  end
      end
  end.

%%
%% enter
%%

mk_enter(Fun, ArgList, Type) ->
  case Type of
    remote -> ok;
    not_remote -> ok % {local,primop,closure,pointer}
  end,
  #enter{'fun'=Fun, arglist=ArgList, type=Type}.
enter_fun(#enter{'fun'=Fun}) -> Fun.
enter_fun_update(I, F) -> I#enter{'fun' = F}.
enter_arglist(#enter{arglist=ArgList}) -> ArgList.
enter_arglist_update(E, NewArgList) -> E#enter{arglist=NewArgList}.
enter_type(#enter{type=Type}) -> Type.
enter_is_known(E) ->
  Fun = enter_fun(E),
  call_or_enter_fun_is_known(Fun).

%%
%% return
%%

mk_return(VarList) -> #return{varlist=VarList}.
return_varlist(#return{varlist=VarList}) -> VarList.
return_varlist_update(R, NewVarList) -> R#return{varlist=NewVarList}.

%%
%% gctests
%%

mk_gctest(Words) when is_integer(Words) -> #gctest{words=mk_imm(Words)};
mk_gctest(Reg) -> #gctest{words=Reg}.  % This handles rtl_regs and rtl_vars
gctest_words(#gctest{words=Words}) -> Words.
gctest_words_update(S, NewWords) -> S#gctest{words=NewWords}.


%%
%% fixnumop
%%

mk_fixnumop(Dst, Src, Type) ->
  #fixnumop{dst=Dst, src=Src, type=Type}.
fixnumop_dst(#fixnumop{dst=Dst}) -> Dst.
fixnumop_dst_update(S, Dst) -> S#fixnumop{dst=Dst}.
fixnumop_src(#fixnumop{src=Src}) -> Src.
fixnumop_src_update(S, Src) -> S#fixnumop{src=Src}.
fixnumop_type(#fixnumop{type=Type}) -> Type.

%%
%% comments
%%

mk_comment(Text) -> #comment{text=Text}.
comment_text(#comment{text=Text}) -> Text.
is_comment(#comment{}) -> true;
is_comment(_) -> false.

%%-------------------------------------------------------------------------
%% Floating point stuff.
%%-------------------------------------------------------------------------

%%
%% fload
%%

mk_fload(Dst, Src, Offset) -> #fload{dst=Dst, src=Src, offset=Offset}.
fload_dst(#fload{dst=Dst}) -> Dst.
fload_dst_update(L, NewDst) -> L#fload{dst=NewDst}.
fload_src(#fload{src=Src}) -> Src.
fload_src_update(L, NewSrc) -> L#fload{src=NewSrc}.
fload_offset(#fload{offset=Offset}) -> Offset.
fload_offset_update(L, NewOffset) -> L#fload{offset=NewOffset}.

%%
%% fstore
%%

mk_fstore(Base, Offset, Src) -> 
  #fstore{base=Base, offset=Offset, src=Src}.
fstore_base(#fstore{base=Base}) -> Base.
fstore_base_update(F, NewBase) -> F#fstore{base=NewBase}.
fstore_offset(#fstore{offset=Offset}) -> Offset.
fstore_offset_update(F, NewOff) -> F#fstore{offset=NewOff}.
fstore_src(#fstore{src=Src}) -> Src.
fstore_src_update(F, NewSrc) -> F#fstore{src=NewSrc}.

%%
%% fp
%%

    
mk_fp(Dst, Src1, Op, Src2) -> 
    [#fp{dst=Dst, src1=Src1, op=Op, src2=Src2}
     | hipe_rtl_arch:mk_fp_check_result(Dst)].

fp_dst(#fp{dst=Dst}) -> Dst.
fp_dst_update(Fp, NewDst) -> Fp#fp{dst=NewDst}.
fp_src1(#fp{src1=Src1}) -> Src1.
fp_src1_update(Fp, NewSrc) -> Fp#fp{src1=NewSrc}.
fp_src2(#fp{src2=Src2}) -> Src2.
fp_src2_update(Fp, NewSrc) -> Fp#fp{src2=NewSrc}.
fp_op(#fp{op=Op}) -> Op.

%%
%% fp_unop
%%

mk_fp_unop(Dst, Src, Op) -> 
  #fp_unop{dst=Dst, src=Src, op=Op}.
fp_unop_dst(#fp_unop{dst=Dst}) -> Dst.
fp_unop_dst_update(Fp, NewDst) -> Fp#fp_unop{dst=NewDst}.
fp_unop_src(#fp_unop{src=Src}) -> Src.
fp_unop_src_update(Fp, NewSrc) -> Fp#fp_unop{src=NewSrc}.
fp_unop_op(#fp_unop{op=Op}) -> Op.

%%
%% fmove
%%

mk_fmove(X, Y) -> true = is_fpreg(X), true = is_fpreg(Y), #fmove{dst=X, src=Y}.
fmove_dst(#fmove{dst=Dst}) -> Dst.
fmove_dst_update(M, NewDst) -> true = is_fpreg(NewDst), M#fmove{dst=NewDst}.
fmove_src(#fmove{src=Src}) -> Src.
fmove_src_update(M, NewSrc) -> true = is_fpreg(NewSrc), M#fmove{src=NewSrc}.

%%
%% fconv
%%

mk_fconv(X, Y) -> #fconv{dst=X, src=Y}.
fconv_dst(#fconv{dst=Dst}) -> Dst.
fconv_dst_update(C, NewDst) -> C#fconv{dst=NewDst}.
fconv_src(#fconv{src=Src}) -> Src.
fconv_src_update(C, NewSrc) -> C#fconv{src=NewSrc}.

%%
%% The values
%%
%% change_vars_to_regs(Vars) ->
%%   change_vars_to_regs(Vars, []).
%% change_vars_to_regs([Var|Rest], Acc) ->
%%   change_vars_to_regs(Rest,[change_var_to_reg(Var)|Acc]);
%% change_vars_to_regs([], Acc) ->
%%   lists:reverse(Acc).
%% 
%% change_var_to_reg(Var) ->
%%   mk_reg(var_index(Var)).

-record(rtl_reg, {index	     :: integer(),
		  is_gc_safe :: boolean()}).

mk_reg(Num, IsGcSafe) when is_integer(Num), Num >= 0 ->
  #rtl_reg{index=Num,is_gc_safe=IsGcSafe}.
mk_reg(Num) -> mk_reg(Num, false).
mk_reg_gcsafe(Num) -> mk_reg(Num, true).
mk_new_reg() -> mk_reg(hipe_gensym:get_next_var(rtl), false).
mk_new_reg_gcsafe() -> mk_reg(hipe_gensym:get_next_var(rtl), true).
reg_index(#rtl_reg{index=Index}) -> Index.
reg_is_gcsafe(#rtl_reg{is_gc_safe=IsGcSafe}) -> IsGcSafe.
is_reg(#rtl_reg{}) -> true;
is_reg(_) -> false.

-record(rtl_var, {index :: non_neg_integer(), liveness=live :: dead | live}).

mk_var(Num) when is_integer(Num), Num >= 0 -> #rtl_var{index=Num}.
mk_var(Num, Liveness) when is_integer(Num), Num>=0 -> #rtl_var{index=Num, liveness=Liveness}.
mk_new_var() -> mk_var(hipe_gensym:get_next_var(rtl)).
var_index(#rtl_var{index=Index}) -> Index.
var_liveness(#rtl_var{liveness=Liveness}) -> Liveness.
var_liveness_update(RtlVar, Liveness) -> RtlVar#rtl_var{liveness=Liveness}.
is_var(#rtl_var{}) -> true;
is_var(_) -> false.

-record(rtl_fpreg, {index :: non_neg_integer()}).

mk_fpreg(Num) when is_integer(Num), Num >= 0 -> #rtl_fpreg{index=Num}.
mk_new_fpreg() -> mk_fpreg(hipe_gensym:get_next_var(rtl)).
fpreg_index(#rtl_fpreg{index=Index}) -> Index.
is_fpreg(#rtl_fpreg{}) -> true;
is_fpreg(_) -> false.

-record(rtl_imm, {value}).

mk_imm(Value) -> #rtl_imm{value=Value}.
imm_value(#rtl_imm{value=Value}) -> Value.
is_imm(#rtl_imm{}) -> true;
is_imm(_) -> false.

-record(rtl_const_lbl, {label}).

mk_const_label(Label) -> #rtl_const_lbl{label=Label}.
const_label_label(#rtl_const_lbl{label=Label}) -> Label.
is_const_label(#rtl_const_lbl{}) -> true;
is_const_label(_) -> false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Utilities - no representation visible below this point
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc Returns the list of variables, constant labels and immediates
%% an RTL instruction uses.
%%

uses(I) ->
  remove_imms_and_const_lbls(args(I)).

%%
%% @doc Returns the list of variables an RTL instruction uses.
%%

args(I) ->
  case I of
    #alu{} -> [alu_src1(I), alu_src2(I)];
    #alub{} -> [alub_src1(I), alub_src2(I)];
    #call{} ->
      Args = call_arglist(I) ++ hipe_rtl_arch:call_used(),
      case call_is_known(I) of
	false -> [call_fun(I) | Args];
	true -> Args
      end;
    #comment{} -> [];
    #enter{} ->
      Args = enter_arglist(I) ++ hipe_rtl_arch:tailcall_used(),
      case enter_is_known(I) of
	false -> [enter_fun(I) | Args];
	true -> Args
      end;
    #fconv{} -> [fconv_src(I)];
    #fixnumop{} -> [fixnumop_src(I)];
    #fload{} -> [fload_src(I), fload_offset(I)];
    #fmove{} -> [fmove_src(I)];
    #fp{} -> [fp_src1(I), fp_src2(I)];
    #fp_unop{} -> [fp_unop_src(I)];
    #fstore{} -> [fstore_base(I), fstore_offset(I), fstore_src(I)];
    #goto{} -> [];
    #goto_index{} -> [];
    #gctest{} -> [gctest_words(I)];
    #label{} -> [];
    #load{} -> [load_src(I), load_offset(I)];
    #load_address{} -> [];
    #load_atom{} -> [];
    #load_word_index{} -> [];
    #move{} -> [move_src(I)];
    #multimove{} -> multimove_srclist(I);
    #phi{} -> phi_args(I);
    #return{} -> return_varlist(I) ++ hipe_rtl_arch:return_used();
    #store{} -> [store_base(I), store_offset(I), store_src(I)];
    #switch{} -> [switch_src(I)]
  end.

%%
%% @doc Returns a list of variables that an RTL instruction defines.
%%

defines(Instr) ->
  Defs = case Instr of
	   #alu{} -> [alu_dst(Instr)];
	   #alub{dst=[]} -> [];
	   #alub{} -> [alub_dst(Instr)];
	   #call{} -> call_dstlist(Instr) ++ hipe_rtl_arch:call_defined();
	   #comment{} -> [];
	   #enter{} -> [];
	   #fconv{} -> [fconv_dst(Instr)];
	   #fixnumop{} -> [fixnumop_dst(Instr)];
	   #fload{} -> [fload_dst(Instr)];
	   #fmove{} -> [fmove_dst(Instr)];
	   #fp{} -> [fp_dst(Instr)];
	   #fp_unop{} -> [fp_unop_dst(Instr)];
	   #fstore{} -> [];
	   #gctest{} -> [];
	   #goto{} -> [];
	   #goto_index{} -> [];
	   #label{} -> [];
	   #load{} -> [load_dst(Instr)];
	   #load_address{} -> [load_address_dst(Instr)];
	   #load_atom{} -> [load_atom_dst(Instr)];
	   #load_word_index{} -> [load_word_index_dst(Instr)];
	   #move{} -> [move_dst(Instr)];
	   #multimove{} -> multimove_dstlist(Instr);
	   #phi{} -> [phi_dst(Instr)];
	   #return{} -> [];
	   #store{} -> [];
	   #switch{} -> []
	 end,
  remove_imms_and_const_lbls(Defs).

%% @spec remove_imms_and_const_lbls([rtl_argument()]) -> [rtl_argument()]
%%
%% @doc Removes all RTL immediates and constant labels from a list of arguments.

remove_imms_and_const_lbls([]) ->
  [];
remove_imms_and_const_lbls([Arg|Args]) ->
  case is_imm(Arg) orelse is_const_label(Arg) of
    true -> remove_imms_and_const_lbls(Args);
    false -> [Arg | remove_imms_and_const_lbls(Args)]
  end.

%%
%% Substitution: replace occurrences of X by Y if {X,Y} is in Subst.
%%
%% subst(Subst, X) ->
%%   subst_defines(Subst, subst_uses(Subst,X)).

subst_uses(Subst, I) ->
  case I of
    #alu{} ->
      I0 = alu_src1_update(I, subst1(Subst, alu_src1(I))),
      alu_src2_update(I0, subst1(Subst, alu_src2(I)));
    #alub{} ->
      I0 = alub_src1_update(I, subst1(Subst, alub_src1(I))),
      alub_src2_update(I0, subst1(Subst, alub_src2(I)));
    #call{} ->
      case call_is_known(I) of
	false ->
	  I0 = call_fun_update(I, subst1(Subst, call_fun(I))),
	  call_arglist_update(I0, subst_list(Subst, call_arglist(I0)));
	true ->
	  call_arglist_update(I, subst_list(Subst, call_arglist(I)))
      end;
    #comment{} ->
      I;
    #enter{} ->
      case enter_is_known(I) of
	false -> 
	  I0 = enter_fun_update(I, subst1(Subst, enter_fun(I))),
	  enter_arglist_update(I0, subst_list(Subst, enter_arglist(I0)));
	true ->
	  enter_arglist_update(I, subst_list(Subst, enter_arglist(I)))
      end;
    #fconv{} ->
      fconv_src_update(I, subst1(Subst, fconv_src(I)));
    #fixnumop{} ->
      fixnumop_src_update(I, subst1(Subst, fixnumop_src(I)));
    #fload{} ->
      I0 = fload_src_update(I, subst1(Subst, fload_src(I))),
      fload_offset_update(I0, subst1(Subst, fload_offset(I)));
    #fmove{} -> 
      fmove_src_update(I, subst1(Subst, fmove_src(I)));
    #fp{} ->
      I0 = fp_src1_update(I, subst1(Subst, fp_src1(I))),
      fp_src2_update(I0, subst1(Subst, fp_src2(I)));
    #fp_unop{} ->
      fp_unop_src_update(I, subst1(Subst, fp_unop_src(I)));
    #fstore{} ->
      I0 = fstore_src_update(I, subst1(Subst, fstore_src(I))),
      I1 = fstore_base_update(I0, subst1(Subst, fstore_base(I))),
      fstore_offset_update(I1, subst1(Subst, fstore_offset(I)));
    #goto{} ->
      I;
    #goto_index{} ->
      I;
    #gctest{} ->
      gctest_words_update(I, subst1(Subst, gctest_words(I)));
    #label{} ->
      I;
    #load{} ->
      I0 = load_src_update(I, subst1(Subst, load_src(I))),
      load_offset_update(I0, subst1(Subst, load_offset(I)));
    #load_address{} ->
      I;
    #load_atom{} ->
      I;
    #load_word_index{} ->
      I;
    #move{} ->
      move_src_update(I, subst1(Subst, move_src(I)));
    #multimove{} -> 
      multimove_srclist_update(I, subst_list(Subst, multimove_srclist(I)));
    #phi{} ->
      phi_argvar_subst(I, Subst);
    #return{} ->
      return_varlist_update(I, subst_list(Subst, return_varlist(I)));
    #store{} ->
      I0 = store_src_update(I, subst1(Subst, store_src(I))),
      I1 = store_base_update(I0, subst1(Subst, store_base(I))),
      store_offset_update(I1, subst1(Subst, store_offset(I)));
    #switch{} ->
      switch_src_update(I, subst1(Subst, switch_src(I)))
  end.

subst_uses_llvm(Subst, I) ->
  case I of
    #alu{} ->
      {NewSrc2, Subst1} = subst1_llvm(Subst, alu_src2(I)),
      {NewSrc1, _ } = subst1_llvm(Subst1, alu_src1(I)),
      I0 =  alu_src1_update(I, NewSrc1),
      alu_src2_update(I0, NewSrc2);
    #alub{} ->
      {NewSrc2, Subst1} = subst1_llvm(Subst, alub_src2(I)),
      {NewSrc1, _ } = subst1_llvm(Subst1, alub_src1(I)),
      I0 =  alub_src1_update(I, NewSrc1),
      alub_src2_update(I0, NewSrc2);
    #call{} ->
      case call_is_known(I) of
        false ->
          {NewFun, Subst1} = subst1_llvm(Subst, call_fun(I)),
          {NewArgList, _} = subst_list_llvm(Subst1, call_arglist(I)),
          I0 = call_fun_update(I, NewFun),
          call_arglist_update(I0, NewArgList);
        true ->
          {NewArgList, _} = subst_list_llvm(Subst, call_arglist(I)),
          call_arglist_update(I, NewArgList)
      end;
    #comment{} ->
      I;
    #enter{} ->
      case enter_is_known(I) of
        false ->
          {NewFun, Subst1} = subst1_llvm(Subst, enter_fun(I)),
          {NewArgList, _} = subst_list_llvm(Subst1, enter_arglist(I)),
          I0 = enter_fun_update(I, NewFun),
          enter_arglist_update(I0, NewArgList);
        true ->
          {NewArgList, _} = subst_list_llvm(Subst, enter_arglist(I)),
          enter_arglist_update(I, NewArgList)
      end;
    #fconv{} ->
      {NewSrc, _ } = subst1_llvm(Subst, fconv_src(I)),
      fconv_src_update(I, NewSrc);
    #fixnumop{} ->
      {NewSrc, _ } = subst1_llvm(Subst, fixnumop_src(I)),
      fixnumop_src_update(I, NewSrc);
    #fload{} ->
      {NewSrc, Subst1} = subst1_llvm(Subst, fload_src(I)),
      {NewOffset, _ } = subst1_llvm(Subst1, fload_offset(I)),
      I0 = fload_src_update(I, NewSrc),
      fload_offset_update(I0, NewOffset);
    #fmove{} ->
      {NewSrc, _ } = subst1_llvm(Subst, fmove_src(I)),
      fmove_src_update(I, NewSrc);
    #fp{} ->
      {NewSrc2, Subst1} = subst1_llvm(Subst, fp_src2(I)),
      {NewSrc1, _ } = subst1_llvm(Subst1, fp_src1(I)),
      I0 = fp_src1_update(I, NewSrc1),
      fp_src2_update(I0, NewSrc2);
    #fp_unop{} ->
      {NewSrc, _ } = subst1_llvm(Subst, fp_unop_src(I)),
      fp_unop_src_update(I, NewSrc);
    #fstore{} ->
      {NewSrc, Subst1} = subst1_llvm(Subst, fstore_src(I)),
      {NewBase, Subst2} = subst1_llvm(Subst1, fstore_base(I)),
      {NewOffset, _ } = subst1_llvm(Subst2, fstore_offset(I)),
      I0 = fstore_src_update(I, NewSrc),
      I1 = fstore_base_update(I0, NewBase),
      fstore_offset_update(I1, NewOffset);
    #goto{} ->
      I;
    #goto_index{} ->
      I;
    #gctest{} ->
      {NewWords, _ } = subst1_llvm(Subst, gctest_words(I)),
      gctest_words_update(I, NewWords);
    #label{} ->
      I;
    #load{} ->
      {NewSrc, Subst1} = subst1_llvm(Subst, load_src(I)),
      {NewOffset, _ } = subst1_llvm(Subst1, load_offset(I)),
      I0 = load_src_update(I, NewSrc),
      load_offset_update(I0, NewOffset);
    #load_address{} ->
      I;
    #load_atom{} ->
      I;
    #load_word_index{} ->
      I;
    #move{} ->
      {NewSrc, _ } = subst1_llvm(Subst, move_src(I)),
      move_src_update(I, NewSrc);
    #multimove{} ->
      {NewSrcList, _} = subst_list_llvm(Subst, multimove_srclist(I)),
      multimove_srclist_update(I, NewSrcList);
    #phi{} ->
      phi_argvar_subst(I, Subst);
    #return{} ->
      {NewVarList, _} = subst_list_llvm(Subst, return_varlist(I)),
      return_varlist_update(I, NewVarList);
    #store{} ->
      {NewSrc, Subst1} = subst1_llvm(Subst, store_src(I)),
      {NewBase, Subst2} = subst1_llvm(Subst1, store_base(I)),
      {NewOffset, _ } = subst1_llvm(Subst2, store_offset(I)),
      I0 = store_src_update(I, NewSrc),
      I1 = store_base_update(I0, NewBase),
      store_offset_update(I1, NewOffset);
    #switch{} ->
      {NewSrc, _ } = subst1_llvm(Subst, switch_src(I)),
      switch_src_update(I, NewSrc)
  end.

subst_list_llvm(S,X) -> subst_list_llvm(S, lists:reverse(X), []).
subst_list_llvm(S, [], Acc) -> {Acc, S};
subst_list_llvm(S, [X|Xs], Acc) ->
  {NewX, RestS} = subst1_llvm(S, X),
  subst_list_llvm(RestS, Xs, [NewX|Acc]).

subst1_llvm(A,B) -> subst1_llvm(A,B,[]).

subst1_llvm([], X, Acc) -> {X, Acc};
subst1_llvm([{X,Y}|Rs], X, Acc) -> {Y, Acc++Rs};
subst1_llvm([R|Xs], X, Acc) -> subst1_llvm(Xs,X,[R|Acc]).

subst_defines(Subst, I)->
  case I of
    #alu{} ->
      alu_dst_update(I, subst1(Subst, alu_dst(I)));
    #alub{dst=[]} ->
      I;
    #alub{} ->
      alub_dst_update(I, subst1(Subst, alub_dst(I)));
    #call{} ->
      call_dstlist_update(I, subst_list(Subst, call_dstlist(I)));
    #comment{} ->
      I;
    #enter{} ->
      I;
    #fconv{} ->
      fconv_dst_update(I, subst1(Subst, fconv_dst(I)));
    #fixnumop{} ->
      fixnumop_dst_update(I, subst1(Subst, fixnumop_dst(I)));
    #fload{} ->
      fload_dst_update(I, subst1(Subst, fload_dst(I)));
    #fmove{} ->
      fmove_dst_update(I, subst1(Subst, fmove_dst(I)));
    #fp{} ->
      fp_dst_update(I, subst1(Subst, fp_dst(I)));
    #fp_unop{} ->
      fp_unop_dst_update(I, subst1(Subst, fp_unop_dst(I)));
    #fstore{} ->
      I;
    #gctest{} ->
      I;
    #goto{} ->
      I;
    #goto_index{} ->
      I;
    #label{} ->
      I;
    #load{} ->
      load_dst_update(I, subst1(Subst, load_dst(I)));
    #load_address{} ->
      load_address_dst_update(I, subst1(Subst, load_address_dst(I)));
    #load_atom{} ->
      load_atom_dst_update(I, subst1(Subst, load_atom_dst(I)));
    #load_word_index{} ->
      load_word_index_dst_update(I, subst1(Subst, load_word_index_dst(I)));
    #move{} ->
      move_dst_update(I, subst1(Subst, move_dst(I)));
    #multimove{} ->
      multimove_dstlist_update(I, subst_list(Subst, multimove_dstlist(I)));
    #phi{} ->
      phi_dst_update(I, subst1(Subst, phi_dst(I)));
    #return{} ->
      I;
    #store{} ->
      I;
    #switch{} ->
      I
  end.

subst_list(S, Xs) ->
  [subst1(S, X) || X <- Xs].

subst1([], X) -> X;
subst1([{X,Y}|_], X) -> Y;
subst1([_|Xs], X) -> subst1(Xs,X).

%% @spec is_safe(rtl_instruction()) -> boolean()
%%
%% @doc Succeeds if an RTL instruction is safe and can be deleted if the
%% result is not used.

is_safe(Instr) ->
  case Instr of
    #alu{} -> true;
    #alub{} -> false;
    #call{} -> false;
    #comment{} -> false;
    #enter{} -> false;
    #fconv{} -> true;
    #fixnumop{} -> true;
    #fload{} -> true;
    #fmove{} -> true;
    #fp{} -> false;
    #fp_unop{} -> false;
    #fstore{} -> false;
    #gctest{} -> false;
    #goto{} -> false;
    #goto_index{} -> false;  % ???
    #label{} -> true;
    #load{} -> true;
    #load_address{} -> true;
    #load_atom{} -> true;
    #load_word_index{} -> true;
    #move{} -> true;
    #multimove{} -> true;
    #phi{} -> true;
    #return{} -> false;
    #store{} -> false;
    #switch{} -> false %% Maybe this is safe...
  end.

%% @spec reduce_unused(rtl_instruction())
%%           -> false | [rtl_instruction()].
%%
%% @doc Produces a simplified instruction sequence that is equivalent to [Instr]
%% under the assumption that all results of Instr are unused, or 'false' if
%% there is no such sequence (other than [Instr] itself).

reduce_unused(Instr) ->
  case Instr of
    #alub{dst=Dst} when Dst =/= [] ->
      [Instr#alub{dst=[]}];
    _ ->
      case is_safe(Instr) of
	true -> [];
	false -> false
      end
  end.

%%
%% True if argument is an alu-operator
%%

%% is_alu_op(add) -> true;
%% is_alu_op(sub) -> true;
%% is_alu_op('or') -> true;
%% is_alu_op('and') -> true;
%% is_alu_op('xor') -> true;
%% is_alu_op(andnot) -> true;
%% is_alu_op(sll) -> true;
%% is_alu_op(srl) -> true;
%% is_alu_op(sra) -> true;
%% is_alu_op(_) -> false.

%% @spec is_shift_op(rtl_operator()) -> boolean()
%%
%% @doc  Succeeds if its argument is an RTL operator.
is_shift_op(sll) -> true;
is_shift_op(srl) -> true;
is_shift_op(sra) -> true;
is_shift_op(_) -> false.


%%
%% True if argument is an relational operator
%%

%% is_rel_op(eq) -> true;
%% is_rel_op(ne) -> true;
%% is_rel_op(gt) -> true;
%% is_rel_op(gtu) -> true;
%% is_rel_op(ge) -> true;
%% is_rel_op(geu) -> true;
%% is_rel_op(lt) -> true;
%% is_rel_op(ltu) -> true;
%% is_rel_op(le) -> true;
%% is_rel_op(leu) -> true;
%% is_rel_op(overflow) -> true;
%% is_rel_op(not_overflow) -> true;
%% is_rel_op(_) -> false.

redirect_jmp(Jmp, ToOld, ToNew) ->
  %% OBS: In a jmp instruction more than one labels may be identical
  %%      and thus need redirection!
  case Jmp of
    #switch{} ->
      NewLbls = [case Lbl =:= ToOld of
		   true -> ToNew;
		   false -> Lbl
		 end || Lbl <- switch_labels(Jmp)],
      switch_labels_update(Jmp, NewLbls);
    #alub{} ->
      TmpJmp = case alub_true_label(Jmp) of
		 ToOld -> alub_true_label_update(Jmp, ToNew);
		 _ -> Jmp
	       end,
      case alub_false_label(TmpJmp) of
	ToOld -> alub_false_label_update(TmpJmp, ToNew);
	_ -> TmpJmp
      end;
    #goto{} ->
      case goto_label(Jmp) of
	ToOld -> goto_label_update(Jmp, ToNew);
	_ -> Jmp
      end;
    #call{} ->
      TmpJmp = case call_continuation(Jmp) of
		 ToOld -> call_continuation_update(Jmp, ToNew);
		 _ -> Jmp
	       end,
      case call_fail(TmpJmp) of
	ToOld -> call_fail_update(TmpJmp, ToNew);
	_ -> TmpJmp
      end;
    _ ->
      Jmp
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% highest_var(Code) ->
%%   highest_var(Code,0).
%% 
%% highest_var([I|Is],Max) ->
%%   Defs = defines(I),
%%   Uses = uses(I),
%%   highest_var(Is,new_max(Defs++Uses,Max));
%% highest_var([],Max) ->
%%   Max.
%% 
%% new_max([V|Vs],Max) ->
%%   VName = 
%%     case is_var(V) of
%%       true ->
%% 	var_index(V);
%%       false ->
%% 	case is_fpreg(V) of
%% 	  true ->
%% 	    fpreg_index(V);
%% 	  _ ->
%% 	    reg_index(V)
%% 	end
%%     end,
%%   if VName > Max ->
%%       new_max(Vs, VName);
%%      true ->
%%       new_max(Vs, Max)
%%   end;
%% new_max([],Max) ->
%%   Max. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Pretty-printer for RTL.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pp(Rtl) ->
  pp(standard_io, Rtl).

pp_block(Instrs) ->
  pp_instrs(standard_io, Instrs).

pp(Dev, Rtl) ->
  io:format(Dev, "~w(", [rtl_fun(Rtl)]),
  pp_args(Dev, rtl_params(Rtl)),
  io:format(Dev, ") ->~n", []),
  case rtl_is_closure(Rtl) of
    true ->
      io:format(Dev, ";; Closure\n", []);
    false -> ok
  end,
  case rtl_is_leaf(Rtl) of
    true ->
      io:format(Dev, ";; Leaf function\n", []);
    false -> ok
  end,
  io:format(Dev, ";; Info: ~w\n", [rtl_info(Rtl)]),
  io:format(Dev, ".DataSegment\n", []),
  hipe_data_pp:pp(Dev, rtl_data(Rtl), rtl, ""), 
  io:format(Dev, ".CodeSegment\n", []),
  pp_instrs(Dev, rtl_code(Rtl)).

pp_instrs(_Dev, []) ->
  ok;
pp_instrs(Dev, [I|Is]) ->
  try pp_instr(Dev, I)
  catch _:_ -> io:format("*** ~w ***\n", [I])
  end,
  pp_instrs(Dev, Is).

pp_instr(Dev, I) ->
  case I of
    #phi{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, phi_dst(I)),
      io:format(Dev, " <- phi(", []),
      pp_phi_args(Dev, phi_arglist(I)),
      io:format(Dev, ")~n", []);
    #move{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, move_dst(I)),
      io:format(Dev, " <- ", []),
      pp_arg(Dev, move_src(I)),
      io:format(Dev, "~n", []);
    #multimove{} ->
      io:format(Dev, "    ", []),
      pp_args(Dev, multimove_dstlist(I)),
      io:format(Dev, " <= ", []),
      pp_args(Dev, multimove_srclist(I)),
      io:format(Dev, "~n", []);
    #alu{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, alu_dst(I)),
      io:format(Dev, " <- ", []),
      pp_arg(Dev, alu_src1(I)),
      io:format(Dev, " ~w ", [alu_op(I)]),
      pp_arg(Dev, alu_src2(I)),
      io:format(Dev, "~n", []);
    #load{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, load_dst(I)),
      io:format(Dev, " <- [", []),
      pp_arg(Dev, load_src(I)),
      io:format(Dev, "+", []),
      pp_arg(Dev, load_offset(I)),
      io:format(Dev, "]", []),
      case load_sign(I) of
	signed -> io:format(Dev, " -signed",[]);
	_ -> ok
      end,
      case load_size(I) of
	byte -> io:format(Dev, " -byte",[]);
	int16 -> io:format(Dev, " -int16",[]);
	int32 -> io:format(Dev, " -int32",[]);
	_ -> ok
      end,
      io:format(Dev, "~n", []);
    #load_atom{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, load_atom_dst(I)),
      io:format(Dev, " <- atom_no(\'~s\')~n", [load_atom_atom(I)]);
    #load_word_index{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, load_word_index_dst(I)),
      io:format(Dev, " <- word_index_no( DL~p[~p] )~n",
		[load_word_index_block(I),load_word_index_index(I)]);
    #goto_index{} ->
      io:format(Dev, "    ", []),
      io:format(Dev, "goto_index DL~p[~p]~n",
		[goto_index_block(I), goto_index_index(I)]);
    #load_address{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, load_address_dst(I)),
      case load_address_type(I) of
	constant ->
	  io:format(Dev, " <- DL~p~n", [load_address_addr(I)]);
	closure ->
	  io:format(Dev, " <- L~p [closure]~n", [load_address_addr(I)]);
	Type ->
	  io:format(Dev, " <- L~p [~p]~n", [load_address_addr(I),Type])
      end;
    #store{} ->
      io:format(Dev, "    [", []),
      pp_arg(Dev, store_base(I)),
      io:format(Dev, "+", []),
      pp_arg(Dev, store_offset(I)),
      io:format(Dev, "] <- ", []),
      pp_arg(Dev, store_src(I)),
       case store_size(I) of
	byte -> io:format(Dev, " -byte",[]);
	int16 -> io:format(Dev, " -int16",[]);
	int32 -> io:format(Dev, " -int32",[]);
	_ -> ok
      end,
      io:format(Dev, "~n", []);
    #label{} ->
      io:format(Dev, "L~w:~n", [label_name(I)]);
    #switch{} ->
      io:format(Dev, "    switch (", []),
      pp_arg(Dev, switch_src(I)),
      io:format(Dev, ") <", []),
      pp_switch_labels(Dev, switch_labels(I)),
      io:format(Dev, ">\n", []);
    #alub{} ->
      io:format(Dev, "    ", []),
      case alub_has_dst(I) of
	true -> pp_arg(Dev, alub_dst(I));
	false -> io:format(Dev, "_", [])
      end,
      io:format(Dev, " <- ", []),
      pp_arg(Dev, alub_src1(I)),
      io:format(Dev, " ~w ", [alub_op(I)]),
      pp_arg(Dev, alub_src2(I)),
      io:format(Dev, " if",[]),
      io:format(Dev, " ~w ", [alub_cond(I)]),
      io:format(Dev, "then L~w (~.2f) else L~w~n", 
		[alub_true_label(I), alub_pred(I), alub_false_label(I)]);
    #goto{} ->
      io:format(Dev, "    goto L~w~n", [goto_label(I)]);
    #call{} ->
      io:format(Dev, "    ", []),
      pp_args(Dev, call_dstlist(I)),
      io:format(Dev, " <- ", []),
      case call_is_known(I) of
	true ->
	  case call_fun(I) of
	    F when is_atom(F) ->
	      io:format(Dev, "~w(", [F]);
	    {M,F,A} when is_atom(M), is_atom(F), is_integer(A), A >= 0 ->
	      io:format(Dev, "~w:~w(", [M, F]);
	    {F,A} when is_atom(F), is_integer(A), A >=0 ->
	      io:format(Dev, "~w(", [F])
	  end;
	false ->
	  io:format(Dev, "(",[]),
	  pp_arg(Dev, call_fun(I)),
	  io:format(Dev, ")(",[])
      end,
      pp_args(Dev, call_arglist(I)),
      io:format(Dev, ")", []),
      case call_continuation(I) of
	[] -> true;
	CC ->
	  io:format(Dev, " then L~w", [CC])
      end,
      case call_fail(I) of
	[] -> true;
	L ->
	  io:format(Dev, " fail to L~w", [L])
      end,
      io:format(Dev, "~n", []);
    #enter{} ->
      io:format(Dev, "    ", []),
      case enter_is_known(I) of
	true ->
	  case enter_fun(I) of
	    F when is_atom(F) ->
	      io:format(Dev, "~w(", [F]);
	    {M,F,A} when is_atom(M), is_atom(F), is_integer(A), A >= 0 ->
	      io:format(Dev, "~w:~w(", [M, F]);
	    {F,A} when is_atom(F), is_integer(A), A >= 0 ->
	      io:format(Dev, "~w(", [F])
	  end;
	false ->
	  io:format(Dev, "(",[]),
	  pp_arg(Dev, enter_fun(I)),
	  io:format(Dev, ")(",[])
      end,
      pp_args(Dev, enter_arglist(I)),
      io:format(Dev, ")~n", []);
    #return{} ->
      io:format(Dev, "    return(", []),
      pp_args(Dev, return_varlist(I)),
      io:format(Dev, ")~n", []);
    #comment{} ->
      io:format(Dev, "    ;; ~p~n", [comment_text(I)]);
    #fixnumop{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, fixnumop_dst(I)),
      io:format(Dev, " <- ", []),
      case fixnumop_type(I) of
	tag ->
	  io:format(Dev, "fixnum_tag(", []);
	untag ->
	  io:format(Dev, "fixnum_untag(", [])
      end,
      pp_arg(Dev, fixnumop_src(I)),
      io:format(Dev, ")~n", []);
    #gctest{} ->
      io:format(Dev, "    gctest(", []),
      pp_arg(Dev, gctest_words(I)),
      io:format(Dev, ")~n", []);
    %% Floating point handling instructions below
    #fload{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, fload_dst(I)),
      io:format(Dev, " <-f [", []),
      pp_arg(Dev, fload_src(I)),
      io:format(Dev, "+", []),
      pp_arg(Dev, fload_offset(I)),
      io:format(Dev, "]~n", []);
    #fstore{} ->
      io:format(Dev, "    [", []),
      pp_arg(Dev, fstore_base(I)),
      io:format(Dev, "+", []),
      pp_arg(Dev, fstore_offset(I)),
      io:format(Dev, "] <- ", []),
      pp_arg(Dev, fstore_src(I)),
      io:format(Dev, "~n", []);
    #fp{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, fp_dst(I)),
      io:format(Dev, " <- ", []),
      pp_arg(Dev, fp_src1(I)),
      io:format(Dev, " ~w ", [fp_op(I)]),
      pp_arg(Dev, fp_src2(I)),
      io:format(Dev, "~n", []);
    #fp_unop{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, fp_unop_dst(I)),
      io:format(Dev, " <- ", []),
      io:format(Dev, " ~w ", [fp_unop_op(I)]),
      pp_arg(Dev, fp_unop_src(I)),
      io:format(Dev, "~n", []);
    #fmove{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, fmove_dst(I)),
      io:format(Dev, " <- ", []),
      pp_arg(Dev, fmove_src(I)),
      io:format(Dev, "~n", []);
    #fconv{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, fconv_dst(I)),
      io:format(Dev, " <-fconv ", []),
      pp_arg(Dev, fconv_src(I)),
      io:format(Dev, "~n", []);
    Other ->
      exit({?MODULE,pp_instr,{"unknown RTL instruction",Other}})
  end.

pp_args(_Dev, []) ->
  ok;
pp_args(Dev, [A]) ->
  pp_arg(Dev, A);
pp_args(Dev, [A|As]) ->
  pp_arg(Dev, A),
  io:format(Dev, ", ", []),
  pp_args(Dev, As).

pp_phi_args(_Dev, []) -> ok;
pp_phi_args(Dev, [{Pred,A}]) ->
  io:format(Dev, "{~w, ", [Pred]),
  pp_arg(Dev, A),
  io:format(Dev, "}", []);
pp_phi_args(Dev, [{Pred,A}|Args]) ->
  io:format(Dev, "{~w, ", [Pred]),
  pp_arg(Dev, A),
  io:format(Dev, "}, ", []),
  pp_phi_args(Dev, Args);
pp_phi_args(Dev, Args) ->
  pp_args(Dev, Args).

pp_hard_reg(Dev, N) ->
  io:format(Dev, "~s", [hipe_rtl_arch:reg_name(N)]).

pp_reg(Dev, Arg) ->
  case hipe_rtl_arch:is_precoloured(Arg) of
    true ->
      pp_hard_reg(Dev, reg_index(Arg));
    false ->
      case reg_is_gcsafe(Arg) of
        true -> io:format(Dev, "rs~w", [reg_index(Arg)]);
        false -> io:format(Dev, "r~w", [reg_index(Arg)])
      end
  end.

pp_var(Dev, Arg) ->
  case hipe_rtl_arch:is_precoloured(Arg) of
    true ->
      pp_hard_reg(Dev, var_index(Arg));
    false ->
      io:format(Dev, "v~w", [var_index(Arg)]),
      case var_liveness(Arg) of
        dead -> io:format(Dev, "(dead)", []);
        _ -> ok
      end
  end.

pp_arg(Dev, A) ->
  case is_var(A) of
    true -> 
      pp_var(Dev, A);
    false ->
      case is_reg(A) of
	true ->
	  pp_reg(Dev, A);
	false ->
	  case is_imm(A) of
	    true ->
	      io:format(Dev, "~w", [imm_value(A)]);
	    false ->
	      case is_fpreg(A) of
		true ->
		  io:format(Dev, "f~w", [fpreg_index(A)]);
		false ->
		  case is_const_label(A) of
		    true ->
		      io:format(Dev, "DL~w", [const_label_label(A)]);
		    false ->
		      exit({?MODULE,pp_arg,{"bad RTL arg",A}})
		  end
	      end
	  end
      end
  end.

pp_switch_labels(Dev,Lbls) -> 
  pp_switch_labels(Dev,Lbls,1).

pp_switch_labels(Dev, [L], _Pos) -> 
  io:format(Dev, "L~w", [L]);
pp_switch_labels(Dev, [L|Ls], Pos) -> 
  io:format(Dev, "L~w, ", [L]),
  NewPos = 
    case Pos of
      5 -> io:format(Dev, "\n              ",[]),
	   0;
      N -> N + 1
    end,
  pp_switch_labels(Dev, Ls, NewPos);
pp_switch_labels(_Dev, [], _) ->
  ok.
