%%% -*- erlang-indent-level: 2 -*-
%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
%%% %CopyrightEnd%
%%%
%%% Encode symbolic ARM instructions to binary form.
%%% Copyright (C) 2005  Mikael Pettersson
%%%
%%% Implementation Notes:
%%% - The Thumb instruction set is a different entity, and is
%%%   not and never will be supported by this module.
%%% - Instructions and instruction forms that are unpredictable
%%%   or useless in User mode are not supported. They include:
%%%   + Data Processing Instructions with S=1 and Rd=15.
%%%   + The LDM(2), LDM(3), and STM(2) instructions.
%%%   + MRS instructions that access the SPSR.
%%%   + MSR instructions that access the SPSR.
%%%   + The LDBRT, LDRT, STBRT, and STRT instructions.
%%%
%%% Instruction Operands:
%%%
%%% S		    ::= {s,0} | {s,1}
%%% L		    ::= {l,0} | {l,1}
%%% R		    ::= {r,RNum}
%%% CR		    ::= {cr,CRNum}
%%%
%%% Cond	    ::= {cond,CondName}
%%% CondName	    ::= eq | ne | cs | hs | cc | lo | mi | pl | vs
%%%		      | vc | hi | ls | ge | lt | gt | ge | al
%%%
%%% Imm<N>	    ::= {imm<N>,<N bits>} for N in 4, 5, 8, 12, 16, 24, and 25
%%%
%%% Am1ShifterOperand
%%%		    ::=	{Imm8,Imm4}
%%%		      | Rm
%%%		      | {Rm,Am1ShiftOp}
%%% Am1ShiftOp	    ::= {ShiftOp,Imm5}
%%%		      | {ShiftOp,Rs}
%%%		      | rrx
%%% ShiftOp	    ::= lsl | lsr | asr | ror
%%%
%%% Am2LSWUBOperand ::= {immediate_offset,Rn,Sign,Imm12}
%%%		      | {register_offset,Rn,Sign,Rm} // redundant
%%%		      | {scaled_register_offset,Rn,Sign,Rm,Am2ShiftOp}
%%%		      | {immediate_pre_indexed,Rn,Sign,Imm12}
%%%		      | {register_pre_indexed,Rn,Sign,Rm} // redundant
%%%		      | {scaled_register_pre_indexed,Rn,Sign,Rm,Am2ShiftOp}
%%%		      | {immediate_post_indexed,Rn,Sign,Imm12}
%%%		      | {register_post_indexed,Rn,Sign,Rm} // redundant
%%%		      | {scaled_register_post_indexed,Rn,Sign,Rm,Am2ShiftOp}
%%% Am2ShiftOp	    ::= {ShiftOp,Imm5}
%%%		      | rrx
%%% Sign	    ::= + | -
%%%
%%% Am3MiscLSOperand::= {immediate_offset,Rn,Sign,Imm8}
%%%		      | {register_offset,Rn,Sign,Rm}
%%%		      | {immediate_pre_indexed,Rn,Sign,Imm8}
%%%		      | {register_pre_indexed,Rn,Sign,Rm}
%%%		      | {immediate_post_indexed,Rn,Sign,Imm8}
%%%		      | {register_post_indexed,Rn,Sign,Rm}
%%%
%%% Am4LSMultiple   ::= ia | ib | da | db
%%%		      | fd | ed | fa | ea
%%%
%%% Am5LSCoprocessor::= {offset,Rn,Sign,Imm8}
%%%		      | {pre_indexed,Rn,Sign,Imm8}
%%%		      | {post_indexed,Rn,Sign,Imm8}
%%%		      | {unindexed,Rn,Imm8}

-module(hipe_arm_encode).

-export([insn_encode/2]).

%%-define(TESTING,1).
-ifdef(TESTING).
-export([dotest/0, dotest/1]).
-endif.

-define(ASSERT(G),
	if G -> [];
	   true -> exit({assertion_failed,?MODULE,?LINE,??G})
	end).

bf(LeftBit, RightBit, Value) ->
  ?ASSERT(32 > LeftBit),
  ?ASSERT(LeftBit >= RightBit),
  ?ASSERT(RightBit >= 0),
  ?ASSERT(Value >= 0),
  ?ASSERT(Value < (1 bsl ((LeftBit - RightBit) + 1))),
  Value bsl RightBit.

-define(BF(LB,RB,V), bf(LB,RB,V)).
-define(BIT(Pos,Val), ?BF(Pos,Pos,Val)).
%%-define(BITS(N,Val), ?BF(N,0,Val)).

%%%
%%% Addressing Modes
%%%

am1_shifter_operand(Rn, Rd, ShifterOperand) ->
  case ShifterOperand of
    {{imm8,Imm8},{imm4,RotImm4}} ->
      ?BIT(25,1) bor ?BF(11,8,RotImm4) bor ?BF(7,0,Imm8);
    {r,Rm} ->
      %% same as Rm LSL #0
      ?BF(3,0,Rm);
    {{r,Rm},ShiftOp} ->
      am1_shift_op(Rn, Rd, Rm, ShiftOp) bor ?BF(3,0,Rm)
  end.

am1_shift_op(_Rn, _Rd, _Rm, {ShiftOp,{imm5,ShiftImm5}}) ->
  case ShiftOp of
    'ror' -> ?ASSERT(ShiftImm5 =/= 0);	% denotes RRX form
    _ -> []
  end,
  ?BF(11,7,ShiftImm5) bor shift_op_bits65(ShiftOp);
am1_shift_op(Rn, Rd, Rm, {ShiftOp,{r,Rs}}) ->
  ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rs =/= 15),	% UNPREDICTABLE
  ?BF(11,8,Rs) bor shift_op_bits65(ShiftOp) bor ?BIT(4,1);
am1_shift_op(_Rn, _Rd, _Rm, 'rrx') ->
  ?BF(6,5,2#11).

shift_op_bits65(ShiftOp) ->
  case ShiftOp of
    'lsl' -> ?BF(6,5,2#00);
    'lsr' -> ?BF(6,5,2#01);
    'asr' -> ?BF(6,5,2#10);
    'ror' -> ?BF(6,5,2#11)
  end.

sign('+') -> ?BIT(23,1);
sign('-') -> 0.

am2_lswub(Rd, AddressingMode) ->
  case AddressingMode of
    {immediate_offset,{r,Rn},Sign,{imm12,Imm12}} ->
      ?BIT(24,1) bor sign(Sign) bor ?BF(19,16,Rn) bor ?BF(11,0,Imm12);
    {register_offset,{r,Rn},Sign,{r,Rm}} ->
      %% same as scaled_register_offset LSL #0
      ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
      ?BIT(25,1) bor ?BIT(24,1) bor sign(Sign) bor ?BF(19,16,Rn) bor ?BF(3,0,Rm);
    {scaled_register_offset,{r,Rn},Sign,{r,Rm},ShiftOp} ->
      ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
      ?BIT(25,1) bor ?BIT(24,1) bor sign(Sign) bor ?BF(19,16,Rn) bor am2_shift_op(ShiftOp) bor ?BF(3,0,Rm);
    {immediate_pre_indexed,{r,Rn},Sign,{imm12,Imm12}} ->
      ?ASSERT(Rd =/= Rn),	% UNPREDICTABLE
      ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
      ?BIT(24,1) bor sign(Sign) bor ?BIT(21,1) bor ?BF(19,16,Rn) bor ?BF(11,0,Imm12);
    {register_pre_indexed,{r,Rn},Sign,{r,Rm}} ->
      %% same as scaled_register_pre_indexed LSL #0
      ?ASSERT(Rd =/= Rn),	% UNPREDICTABLE
      ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
      ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
      ?ASSERT(Rn =/= Rm),	% UNPREDICTABLE
      ?BIT(25,1) bor ?BIT(24,1) bor sign(Sign) bor ?BIT(21,1) bor ?BF(19,16,Rn) bor ?BF(3,0,Rm);
    {scaled_register_pre_indexed,{r,Rn},Sign,{r,Rm},ShiftOp} ->
      ?ASSERT(Rd =/= Rn),	% UNPREDICTABLE
      ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
      ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
      ?ASSERT(Rn =/= Rm),	% UNPREDICTABLE
      ?BIT(25,1) bor ?BIT(24,1) bor sign(Sign) bor ?BIT(21,1) bor ?BF(19,16,Rn) bor am2_shift_op(ShiftOp) bor ?BF(3,0,Rm);
    {immediate_post_indexed,{r,Rn},Sign,{imm12,Imm12}} ->
      ?ASSERT(Rd =/= Rn),	% UNPREDICTABLE
      ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
      sign(Sign) bor ?BF(19,16,Rn) bor ?BF(11,0,Imm12);
    {register_post_indexed,{r,Rn},Sign,{r,Rm}} ->
      %% same as scaled_register_post_indexed LSL #0
      ?ASSERT(Rd =/= Rn),	% UNPREDICTABLE
      ?BIT(25,1) bor sign(Sign) bor ?BF(19,6,Rn) bor ?BF(3,0,Rm);
    {scaled_register_post_indexed,{r,Rn},Sign,{r,Rm},ShiftOp} ->
      ?ASSERT(Rd =/= Rn),	% UNPREDICTABLE
      ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
      ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
      ?ASSERT(Rn =/= Rm),	% UNPREDICTABLE
      ?BIT(25,1) bor sign(Sign) bor ?BF(19,16,Rn) bor am2_shift_op(ShiftOp) bor ?BF(3,0,Rm)
  end.

am2_shift_op({ShiftOp,{imm5,ShiftImm5}}) ->
  case ShiftOp of
    'ror' -> ?ASSERT(ShiftImm5 =/= 0);	% denotes RRX form
    _ -> []
  end,
  ?BF(11,7,ShiftImm5) bor shift_op_bits65(ShiftOp);
am2_shift_op('rrx') ->
  ?BF(6,5,2#11).

am3_miscls(Rd, AddressingMode) ->
  case AddressingMode of
    {immediate_offset,{r,Rn},Sign,{imm8,Imm8}} ->
      ?BIT(24,1) bor sign(Sign) bor ?BF(22,21,2#10) bor ?BF(19,16,Rn) bor ?BF(11,8,Imm8 bsr 4) bor ?BF(3,0,Imm8 band 2#1111);
    {register_offset,{r,Rn},Sign,{r,Rm}} ->
      ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
      ?BIT(24,1) bor sign(Sign) bor ?BF(22,21,2#00) bor ?BF(19,16,Rn) bor ?BF(3,0,Rm);
    {immediate_pre_indexed,{r,Rn},Sign,{imm8,Imm8}} ->
      ?ASSERT(Rd =/= Rn),	% UNPREDICTABLE
      ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
      ?BIT(24,1) bor sign(Sign) bor ?BF(22,21,2#11) bor ?BF(19,16,Rn) bor ?BF(11,8,Imm8 bsr 4) bor ?BF(3,0,Imm8 band 2#1111);
    {register_pre_indexed,{r,Rn},Sign,{r,Rm}} ->
      ?ASSERT(Rd =/= Rn),	% UNPREDICTABLE
      ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
      ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
      ?ASSERT(Rm =/= Rn),	% UNPREDICTABLE
      ?BIT(24,1) bor sign(Sign) bor ?BF(22,21,2#01) bor ?BF(19,16,Rn) bor ?BF(3,0,Rm);
    {immediate_post_indexed,{r,Rn},Sign,{imm8,Imm8}} ->
      ?ASSERT(Rd =/= Rn),	% UNPREDICTABLE
      ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
      ?BIT(24,0) bor sign(Sign) bor ?BF(22,21,2#10) bor ?BF(19,16,Rn) bor ?BF(11,8,Imm8 bsr 4) bor ?BF(3,0,Imm8 band 2#1111);
    {register_post_indexed,{r,Rn},Sign,{r,Rm}} ->
      ?ASSERT(Rd =/= Rn),	% UNPREDICTABLE
      ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
      ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
      ?ASSERT(Rm =/= Rn),	% UNPREDICTABLE
      ?BIT(24,0) bor sign(Sign) bor ?BF(22,21,2#00) bor ?BF(19,16,Rn) bor ?BF(3,0,Rm)
  end.

am4_ls_multiple(L, AddressingMode) ->
  case AddressingMode of
    'ia' -> ?BF(24,23,2#01);
    'ib' -> ?BF(24,23,2#11);
    'da' -> ?BF(24,23,2#00);
    'db' -> ?BF(24,23,2#10);
    _ ->
      %% context-sensitive alias crap
      case {L,AddressingMode} of
	{1,'fa'} -> ?BF(24,23,2#00);
	{1,'fd'} -> ?BF(24,23,2#01);
	{1,'ea'} -> ?BF(24,23,2#10);
	{1,'ed'} -> ?BF(24,23,2#11);
	{0,'ed'} -> ?BF(24,23,2#00);
	{0,'ea'} -> ?BF(24,23,2#01);
	{0,'fd'} -> ?BF(24,23,2#10);
	{0,'fa'} -> ?BF(24,23,2#11)
      end
  end.

am5_ls_coprocessor(AddressingMode) ->
  case AddressingMode of
    {offset,{r,Rn},Sign,{imm8,Imm8}} ->
      ?BIT(24,1) bor sign(Sign) bor ?BF(19,16,Rn) bor ?BF(7,0,Imm8);
    {pre_indexed,{r,Rn},Sign,{imm8,Imm8}} ->
      ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
      ?BIT(24,1) bor sign(Sign) bor ?BIT(21,1) bor ?BF(19,16,Rn) bor ?BF(7,0,Imm8);
    {post_indexed,{r,Rn},Sign,{imm8,Imm8}} ->
      ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
      sign(Sign) bor ?BIT(21,1) bor ?BF(19,16,Rn) bor ?BF(7,0,Imm8);
    {unindexed,{r,Rn},{imm8,Imm8}} ->
      ?BIT(23,1) bor ?BF(19,16,Rn) bor ?BF(7,0,Imm8)
  end.

%%%

'cond'(Cond) ->
  case Cond of
    'eq' -> ?BF(31,28,2#0000);	% equal
    'ne' -> ?BF(31,28,2#0001);	% not equal
    'cs' -> ?BF(31,28,2#0010);	% carry set
    'hs' -> ?BF(31,28,2#0010);	% unsigned higher or same
    'cc' -> ?BF(31,28,2#0011);	% carry clear
    'lo' -> ?BF(31,28,2#0011);	% unsigned lower
    'mi' -> ?BF(31,28,2#0100);	% minus/negative
    'pl' -> ?BF(31,28,2#0101);	% plus/positive or zero
    'vs' -> ?BF(31,28,2#0110);	% overflow
    'vc' -> ?BF(31,28,2#0111);	% no overflow
    'hi' -> ?BF(31,28,2#1000);	% unsigned higher
    'ls' -> ?BF(31,28,2#1001);	% unsigned lower or same
    'ge' -> ?BF(31,28,2#1010);	% signed greater than or equal
    'lt' -> ?BF(31,28,2#1011);	% signed less than
    'gt' -> ?BF(31,28,2#1100);	% signed greater than
    'le' -> ?BF(31,28,2#1101);	% signed less than or equal
    'al' -> ?BF(31,28,2#1110)	% always
  end.

%%%
%%% ARM Instructions
%%%

data_processing_form(Cond, OpCode, S, Rn, Rd, ShifterOperand) ->
  case S of
    1 -> ?ASSERT(Rd =/= 15);	% UNPREDICTABLE in User or System mode
    _ -> []
  end,
  'cond'(Cond) bor ?BF(24,21,OpCode) bor ?BIT(20,S) bor ?BF(19,16,Rn) bor ?BF(15,12,Rd) bor am1_shifter_operand(Rn,Rd,ShifterOperand).

data_processing_form(OpCode, {{'cond',Cond},{s,S},{r,Rd},{r,Rn},ShifterOperand}) ->
  data_processing_form(Cond, OpCode, S, Rn, Rd, ShifterOperand).

adc(Opnds) -> data_processing_form(2#0101, Opnds).
add(Opnds) -> data_processing_form(2#0100, Opnds).
'and'(Opnds) -> data_processing_form(2#0000, Opnds).
bic(Opnds) -> data_processing_form(2#1110, Opnds).
eor(Opnds) -> data_processing_form(2#0001, Opnds).
orr(Opnds) -> data_processing_form(2#1100, Opnds).
rsb(Opnds) -> data_processing_form(2#0011, Opnds).
rsc(Opnds) -> data_processing_form(2#0111, Opnds).
sbc(Opnds) -> data_processing_form(2#0110, Opnds).
sub(Opnds) -> data_processing_form(2#0010, Opnds).

cmp_form(OpCode, {{'cond',Cond},{r,Rn},ShifterOperand}) ->
  data_processing_form(Cond, OpCode, 1, Rn, 0, ShifterOperand).

cmn(Opnds) -> cmp_form(2#1011, Opnds).
cmp(Opnds) -> cmp_form(2#1010, Opnds).
teq(Opnds) -> cmp_form(2#1001, Opnds).
tst(Opnds) -> cmp_form(2#1000, Opnds).

mov_form(OpCode, {{'cond',Cond},{s,S},{r,Rd},ShifterOperand}) ->
  data_processing_form(Cond, OpCode, S, 0, Rd, ShifterOperand).

mov(Opnds) -> mov_form(2#1101, Opnds).
mvn(Opnds) -> mov_form(2#1111, Opnds).

%%%

b_form(L, {{'cond',Cond},{imm24,Imm24}}) ->
  'cond'(Cond) bor ?BF(27,25,2#101) bor ?BIT(24,L) bor ?BF(23,0,Imm24).

b(Opnds) -> b_form(0, Opnds).
bl(Opnds) -> b_form(1, Opnds).

bkpt({{imm16,Imm16}}) ->
  ?BF(31,28,2#1110) bor ?BF(27,20,2#00010010) bor ?BF(19,8,Imm16 bsr 4) bor ?BF(7,4,2#0111) bor ?BF(3,0,Imm16 band 2#1111).

bx_form(SubOpcode, {{'cond',Cond},{r,Rm}}, IsBlx) ->
  case IsBlx of
    true -> ?ASSERT(Rm =/= 15);	% UNPREDICTABLE
    _ -> []
  end,
  'cond'(Cond) bor ?BF(27,20,2#00010010) bor ?BF(19,16,2#1111) bor ?BF(15,12,2#1111) bor ?BF(11,8,2#1111) bor ?BF(7,4,SubOpcode) bor ?BF(3,0,Rm).

blx(Opnds) ->
  case Opnds of
    {{imm25,Imm25}} ->	% u16-offset!
      ?BF(31,28,2#1111) bor ?BF(27,25,2#101) bor ?BIT(24,Imm25 band 1) bor ?BF(23,0,Imm25 bsr 1);
    _ ->
      bx_form(2#0011, Opnds, true)
  end.

bx(Opnds) -> bx_form(2#0001, Opnds, false).

cdp_form(Cond, CpOp4, CRn, CRd, CpNum, CpOp3, CRm) ->
  Cond bor ?BF(27,24,2#1110) bor ?BF(23,20,CpOp4) bor ?BF(19,16,CRn) bor ?BF(15,12,CRd) bor ?BF(11,8,CpNum) bor ?BF(7,5,CpOp3) bor ?BF(3,0,CRm).

cdp({{'cond',Cond},{cpnum,CpNum},{cpop4,CpOp4},{cr,CRd},{cr,CRn},{cr,CRm},{cpop3,CpOp3}}) ->
  cdp_form('cond'(Cond), CpOp4, CRn, CRd, CpNum, CpOp3, CRm).

cdp2({{cpnum,CpNum},{cpop4,CpOp4},{cr,CRd},{cr,CRn},{cr,CRm},{cpop3,CpOp3}}) ->
  cdp_form(?BF(31,28,2#1111), CpOp4, CRn, CRd, CpNum, CpOp3, CRm).

clz({{'cond',Cond},{r,Rd},{r,Rm}}) ->
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
  'cond'(Cond) bor ?BF(27,20,2#00010110) bor ?BF(19,16,2#1111) bor ?BF(15,12,Rd) bor ?BF(11,8,2#1111) bor ?BF(7,4,2#0001) bor ?BF(3,0,Rm).

ldstc_form(Cond, L, B20, CRd, CpNum, AddressingMode) ->
  Cond bor ?BF(27,25,2#110) bor ?BIT(22,L) bor ?BIT(20,B20) bor ?BF(15,12,CRd) bor ?BF(11,8,CpNum) bor am5_ls_coprocessor(AddressingMode).

ldstc(B20, {{'cond',Cond},{l,L},{cpnum,CpNum},{cr,CRd},AddressingMode}) ->
  ldstc_form('cond'(Cond), L, B20, CRd, CpNum, AddressingMode).

ldc(Opnds) -> ldstc(1, Opnds).
stc(Opnds) -> ldstc(0, Opnds).

ldstc2(B20, {{l,L},{cpnum,CpNum},{cr,CRd},AddressingMode}) ->
  ldstc_form(?BF(31,28,2#1111), L, B20, CRd, CpNum, AddressingMode).

ldc2(Opnds) -> ldstc2(1, Opnds).
stc2(Opnds) -> ldstc2(0, Opnds).

ldstm_form(Cond, AddressingMode, W, L, Rn, Registers) ->
  RegisterList = register_list(Registers),
  ?ASSERT(RegisterList =/= 0),	% UNPREDICTABLE
  ?ASSERT(Rn =/= 15),		% UNPREDICTABLE
  case W of
    1 ->
      BitRn = 1 bsl Rn,
      case L of
	1 ->
	  %% LDM! Rn in Registers is UNPREDICTABLE
	  ?ASSERT((RegisterList band BitRn) =:= 0);
	0 ->
	  %% STM! Rn in Registers and not lowest is UNPREDICTABLE
	  case RegisterList band BitRn of
	    0 -> [];
	    _ ->
	      ?ASSERT((RegisterList band (-RegisterList)) =:= BitRn)
	  end
      end;
    _ -> []
  end,
  'cond'(Cond) bor ?BF(27,25,2#100) bor am4_ls_multiple(L, AddressingMode) bor ?BIT(21,W) bor ?BIT(20,L) bor ?BF(19,16,Rn) bor ?BF(15,0,RegisterList).

register_list(Registers) -> register_list(Registers, 0).

register_list([{r,R}|Rs], Mask) -> register_list(Rs, Mask bor (1 bsl R));
register_list([], Mask) -> Mask.

ldstm(L, Opnds) ->
  case Opnds of
    {{'cond',Cond},AddressingMode,{r,Rn},'!',Registers} ->
      ldstm_form(Cond, AddressingMode, 1, L, Rn, Registers);
    {{'cond',Cond},AddressingMode,{r,Rn},Registers} ->
      ldstm_form(Cond, AddressingMode, 0, L, Rn, Registers)
    %% the ldm(2), ldm(3), and stm(2) forms are UNPREDICTABLE
    %% in User or System mode
  end.

ldm(Opnds) -> ldstm(1, Opnds).
stm(Opnds) -> ldstm(0, Opnds).

ldstr_form2(B, L, {{'cond',Cond},{r,Rd},AddressingMode}) ->
  'cond'(Cond) bor ?BF(27,26,2#01) bor am2_lswub(Rd, AddressingMode) bor ?BIT(22,B) bor ?BIT(20,L) bor ?BF(15,12,Rd).

ldr(Opnds) -> ldstr_form2(0, 1, Opnds).
ldrb(Opnds) -> ldstr_form2(1, 1, Opnds).
str(Opnds) -> ldstr_form2(0, 0, Opnds).
strb(Opnds) -> ldstr_form2(1, 0, Opnds).

ldstr_form3(L, SubOpcode, {{'cond',Cond},{r,Rd},AddressingMode}) ->
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  'cond'(Cond) bor am3_miscls(Rd, AddressingMode) bor ?BIT(20,L) bor ?BF(15,12,Rd) bor ?BF(7,4,SubOpcode).

ldrh(Opnds) -> ldstr_form3(1, 2#1011, Opnds).
ldrsb(Opnds) -> ldstr_form3(1, 2#1101, Opnds).
ldrsh(Opnds) -> ldstr_form3(1, 2#1111, Opnds).
strh(Opnds) -> ldstr_form3(0, 2#1011, Opnds).

mcr_form(Cond, OP1, CRn, Rd, CpNum, OP2, CRm) ->
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  Cond bor ?BF(27,24,2#1110) bor ?BF(23,21,OP1) bor ?BF(19,16,CRn) bor ?BF(15,12,Rd) bor ?BF(11,8,CpNum) bor ?BF(7,5,OP2) bor ?BIT(4,1) bor ?BF(3,0,CRm).

mcr({{'cond',Cond},{cpnum,CpNum},{cpop3,OP1},{r,Rd},{cr,CRn},{cr,CRm},{cpop3,OP2}}) ->
  mcr_form('cond'(Cond), OP1, CRn, Rd, CpNum, OP2, CRm).

mcr2({{cpnum,CpNum},{cpop3,OP1},{r,Rd},{cr,CRn},{cr,CRm},{cpop3,OP2}}) ->
  mcr_form(?BF(31,28,2#1111), OP1, CRn, Rd, CpNum, OP2, CRm).

mla({{'cond',Cond},{s,S},{r,Rd},{r,Rm},{r,Rs},{r,Rn}}) ->
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rs =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rd =/= Rm),	% UNPREDICTABLE
  'cond'(Cond) bor ?BIT(21,1) bor ?BIT(20,S) bor ?BF(19,16,Rd) bor ?BF(15,12,Rn) bor ?BF(11,8,Rs) bor ?BF(7,4,2#1001) bor ?BF(3,0,Rm).

mrc_form(Cond, OP1, CRn, Rd, CpNum, OP2, CRm) ->
  Cond bor ?BF(27,24,2#1110) bor ?BF(23,21,OP1) bor ?BIT(20,1) bor ?BF(19,16,CRn) bor ?BF(15,12,Rd) bor ?BF(11,8,CpNum) bor ?BF(7,5,OP2) bor ?BIT(4,1) bor ?BF(3,0,CRm).

mrc({{'cond',Cond},{cpnum,CpNum},{cpop3,OP1},{r,Rd},{cr,CRn},{cr,CRm},{cpop3,OP2}}) ->
  mrc_form('cond'(Cond), OP1, CRn, Rd, CpNum, OP2, CRm).

mrc2({{cpnum,CpNum},{cpop3,OP1},{r,Rd},{cr,CRn},{cr,CRm},{cpop3,OP2}}) ->
  mrc_form(?BF(31,28,2#1111), OP1, CRn, Rd, CpNum, OP2, CRm).

mrs({{'cond',Cond},{r,Rd},'cpsr'}) ->
  %% the SPSR form is UNPREDICTABLE in User or System mode
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  'cond'(Cond) bor ?BIT(24,1) bor ?BF(19,16,2#1111) bor ?BF(15,12,Rd).

msr_form(Cond, FieldMask4, Operand) ->
  'cond'(Cond) bor ?BIT(24,1) bor ?BIT(21,1) bor ?BF(19,16,FieldMask4) bor ?BF(15,12,2#1111) bor Operand.

msr(Opnds) ->
  %% the SPSR form is UNPREDICTABLE in User or System mode
  case Opnds of
    {{'cond',Cond},'cpsr',{field_mask,FieldMask4},{imm8,Imm8},{imm4,RotImm4}} ->
      msr_form(Cond, FieldMask4, ?BIT(25,1) bor ?BF(11,8,RotImm4) bor ?BF(7,0,Imm8));
    {{'cond',Cond},'cpsr',{field_mask,FieldMask4},{r,Rm}} ->
      msr_form(Cond, FieldMask4, ?BF(3,0,Rm))
  end.

mul({{'cond',Cond},{s,S},{r,Rd},{r,Rm},{r,Rs}}) ->
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rs =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rd =/= Rm),	% UNPREDICTABLE
  'cond'(Cond) bor ?BIT(20,S) bor ?BF(19,16,Rd) bor ?BF(11,8,Rs) bor ?BF(7,4,2#1001) bor ?BF(3,0,Rm).

ml_form2(OpCode, Cond, S, RdLo, RdHi, Rm, Rs) ->
  ?ASSERT(RdHi =/= 15),	% UNPREDICTABLE
  ?ASSERT(RdLo =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rs =/= 15),	% UNPREDICTABLE
  ?ASSERT(RdHi =/= RdLo),% UNPREDICTABLE
  ?ASSERT(RdHi =/= Rm),	% UNPREDICTABLE
  ?ASSERT(RdLo =/= Rm),	% UNPREDICTABLE
  'cond'(Cond) bor ?BF(27,21,OpCode) bor ?BIT(20,S) bor ?BF(19,16,RdHi) bor ?BF(15,12,RdLo) bor ?BF(11,8,Rs) bor ?BF(7,4,2#1001) bor ?BF(3,0,Rm).

ml_form(OpCode, {{'cond',Cond},{s,S},{r,RdLo},{r,RdHi},{r,Rm},{r,Rs}}) ->
  ml_form2(OpCode, Cond, S, RdLo, RdHi, Rm, Rs).

%%smlal(Opnds) -> ml_form(2#0000111, Opnds).
smull(Opnds) -> ml_form(2#0000110, Opnds).
umlal(Opnds) -> ml_form(2#0000101, Opnds).
umull(Opnds) -> ml_form(2#0000100, Opnds).

swi({{'cond',Cond},{imm24,Imm24}}) ->
  'cond'(Cond) bor ?BF(27,24,2#1111) bor ?BF(23,0,Imm24).

swp_form(B22, {{'cond',Cond},{r,Rd},{r,Rm},{r,Rn}}) ->
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rn =/= Rm),	% UNPREDICTABLE
  ?ASSERT(Rn =/= Rd),	% UNPREDICTABLE
  'cond'(Cond) bor ?BIT(24,1) bor ?BIT(22,B22) bor ?BF(19,16,Rn) bor ?BF(15,12,Rd) bor ?BF(7,4,2#1001) bor ?BF(3,0,Rm).

swp(Opnds) -> swp_form(0, Opnds).
swpb(Opnds) -> swp_form(1, Opnds).

%%%
%%% Enhanced DSP Extension Instructions
%%%

ldstrd_form(OpCode, {{'cond',Cond},{r,Rd},AddressingMode}) ->
  ?ASSERT(Rd =/= 14),		% UNPREDICTABLE
  ?ASSERT((Rd band 1) =:= 0),	% UNDEFINED
  %% XXX: unpredictable if write-back and base reg Rn equals Rd or Rd+1
  %% XXX: if is load then unpredictable if index reg Rm and Rm equals Rd or Rd+1
  'cond'(Cond) bor am3_miscls(Rd, AddressingMode) bor ?BF(15,12,Rd) bor ?BF(7,4,OpCode).

ldrd(Opnds) -> ldstrd_form(2#1101, Opnds).
strd(Opnds) -> ldstrd_form(2#1111, Opnds).

mcrr({{'cond',Cond},{cpnum,CpNum},{cpop4,OP},{r,Rd},{r,Rn},{cr,CRm}}) ->
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
  'cond'(Cond) bor ?BF(27,20,2#11000100) bor ?BF(19,16,Rn) bor ?BF(15,12,Rd) bor ?BF(11,8,CpNum) bor ?BF(7,4,OP) bor ?BF(3,0,CRm).

mrrc({{'cond',Cond},{cpnum,CpNum},{cpop4,OP},{r,Rd},{r,Rn},{cr,CRm}}) ->
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rd =/= Rn),	% UNPREDICTABLE
  'cond'(Cond) bor ?BF(27,20,2#11000101) bor ?BF(19,16,Rn) bor ?BF(15,12,Rd) bor ?BF(11,8,CpNum) bor ?BF(7,4,OP) bor ?BF(3,0,CRm).

pld({AddressingMode}) ->
  AM = am2_lswub(42, AddressingMode), % 42 is a dummy reg nr
  %% not all adressing modes are allowed: bit 24 must be 1
  %% and bit 21 must be 0
  ?ASSERT(((AM bsr 21) band 2#1001) =:= 2#1000),
  16#F550F000 bor AM.

q_form(OpCode, {{'cond',Cond},{r,Rd},{r,Rm},{r,Rn}}) ->
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
  'cond'(Cond) bor ?BF(27,20,OpCode) bor ?BF(19,16,Rn) bor ?BF(15,11,Rd) bor ?BF(7,4,2#0101) bor ?BF(3,0,Rm).

qadd(Opnds) -> q_form(2#00010000, Opnds).
qdadd(Opnds) -> q_form(2#00010100, Opnds).
qdsub(Opnds) -> q_form(2#00010110, Opnds).
qsub(Opnds) -> q_form(2#00010010, Opnds).

smlaxy_form(Cond, OpCode, Rd, Rn, Rs, Y, X, Rm) ->
  ?ASSERT(Rd =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rm =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rs =/= 15),	% UNPREDICTABLE
  ?ASSERT(Rn =/= 15),	% UNPREDICTABLE
  'cond'(Cond) bor ?BF(27,20,OpCode) bor ?BF(19,16,Rd) bor ?BF(15,12,Rn) bor ?BF(11,8,Rs) bor ?BIT(7,1) bor ?BIT(6,Y) bor ?BIT(5,X) bor ?BF(3,0,Rm).

smla({{bt,X},{bt,Y},{'cond',Cond},{r,Rd},{r,Rm},{r,Rs},{r,Rn}}) ->
  smlaxy_form(Cond, 2#00010000, Rd, Rn, Rs, Y, X, Rm).

smlal(Opnds) -> % may be regular ARM or DSP insn :-(
  case Opnds of
    {{'cond',Cond},{s,S},{r,RdLo},{r,RdHi},{r,Rm},{r,Rs}} ->
      ml_form2(2#0000111, Cond, S, RdLo, RdHi, Rm, Rs);
    {{bt,X},{bt,Y},{'cond',Cond},{r,RdLo},{r,RdHi},{r,Rm},{r,Rs}} ->
      ?ASSERT(RdLo =/= RdHi),	% UNPREDICTABLE
      smlaxy_form(Cond, 2#00010100, RdHi, RdLo, Rs, Y, X, Rm)
  end.

smlaw({{bt,Y},{'cond',Cond},{r,Rd},{r,Rm},{r,Rs},{r,Rn}}) ->
  smlaxy_form(Cond, 2#00010010, Rd, Rn, Rs, Y, 0, Rm).
  
smul({{bt,X},{bt,Y},{'cond',Cond},{r,Rd},{r,Rm},{r,Rs}}) ->
  smlaxy_form(Cond, 2#00010110, Rd, 0, Rs, Y, X, Rm).

smulw({{bt,Y},{'cond',Cond},{r,Rd},{r,Rm},{r,Rs}}) ->
  smlaxy_form(Cond, 2#00010010, Rd, 0, Rs, Y, 1, Rm).

%%%
%%% Main Encode Dispatch
%%%

insn_encode(Op, Opnds) ->
  case Op of
    'adc' -> adc(Opnds);
    'add' -> add(Opnds);
    'and' -> 'and'(Opnds);
    'b' -> b(Opnds);
    'bic' -> bic(Opnds);
    'bkpt' -> bkpt(Opnds);
    'bl' -> bl(Opnds);
    'blx' -> blx(Opnds);
    'bx' -> bx(Opnds);
    'cdp' -> cdp(Opnds);
    'cdp2' -> cdp2(Opnds);
    'clz' -> clz(Opnds);
    'cmn' -> cmn(Opnds);
    'cmp' -> cmp(Opnds);
    'eor' -> eor(Opnds);
    'ldc' -> ldc(Opnds);
    'ldc2' -> ldc2(Opnds);
    'ldm' -> ldm(Opnds);
    'ldr' -> ldr(Opnds);
    'ldrb' -> ldrb(Opnds);
    'ldrd' -> ldrd(Opnds);
    %% ldrbt: omitted
    'ldrh' -> ldrh(Opnds);
    'ldrsb' -> ldrsb(Opnds);
    'ldrsh' -> ldrsh(Opnds);
    %% ldrt: omitted
    'mcr' -> mcr(Opnds);
    'mcr2' -> mcr2(Opnds);
    'mcrr' -> mcrr(Opnds);
    'mla' -> mla(Opnds);
    'mov' -> mov(Opnds);
    'mrc' -> mrc(Opnds);
    'mrc2' -> mrc2(Opnds);
    'mrrc' -> mrrc(Opnds);
    'mrs' -> mrs(Opnds);
    'msr' -> msr(Opnds);
    'mul' -> mul(Opnds);
    'mvn' -> mvn(Opnds);
    'orr' -> orr(Opnds);
    'pld' -> pld(Opnds);
    'qadd' -> qadd(Opnds);
    'qdadd' -> qdadd(Opnds);
    'qdsub' -> qdsub(Opnds);
    'qsub' -> qsub(Opnds);
    'rsb' -> rsb(Opnds);
    'rsc' -> rsc(Opnds);
    'sbc' -> sbc(Opnds);
    'smla' -> smla(Opnds);
    'smlal' -> smlal(Opnds); % may be regular ARM or DSP insn :-(
    'smlaw' -> smlaw(Opnds);
    'smull' -> smull(Opnds);
    'smul' -> smul(Opnds);
    'smulw' -> smulw(Opnds);
    'stc' -> stc(Opnds);
    'stc2' -> stc2(Opnds);
    'stm' -> stm(Opnds);
    'str' -> str(Opnds);
    'strb' -> strb(Opnds);
    %% strbt: omitted
    'strd' -> strd(Opnds);
    'strh' -> strh(Opnds);
    %% strt: omitted
    'sub' -> sub(Opnds);
    'swi' -> swi(Opnds);
    'swp' -> swp(Opnds);
    'swpb' -> swpb(Opnds);
    'teq' -> teq(Opnds);
    'tst' -> tst(Opnds);
    'umlal' -> umlal(Opnds);
    'umull' -> umull(Opnds);
    _ -> exit({?MODULE,insn_encode,Op})
  end.

%%%
%%% Testing Interface
%%%

-ifdef(TESTING).

say(OS, Str) ->
  file:write(OS, Str).

hex_digit(Dig0) ->
  Dig = Dig0 band 16#F,
  if Dig >= 16#A -> $A + (Dig - 16#A);
     true -> $0 + Dig
  end.

say_byte(OS, Byte) ->
  say(OS, [hex_digit(Byte bsr 4)]),
  say(OS, [hex_digit(Byte)]).

say_word(OS, Word) ->
  say(OS, "0x"),
  say_byte(OS, Word bsr 24),
  say_byte(OS, Word bsr 16),
  say_byte(OS, Word bsr 8),
  say_byte(OS, Word).

t(OS, Op, Opnds) ->
  Word = insn_encode(Op, Opnds),
  say(OS, "\t.long "),
  say_word(OS, Word),
  say(OS, "\n").

dotest1(OS) ->
  say(OS, "\t.text\n\t.align 4\n"),
  %%
  Rn = {r,9},
  Rd = {r,8},	% must be even and less than 14 for some insns
  Rm = {r,7},
  Rs = {r,6},
  RdLo = Rn,
  RdHi = Rd,
  Registers = [Rm,Rs,Rd],	% must exclude Rn for some insns
  CRd = {cr,15},
  CRn = {cr,14},
  CRm = {cr,13},
  BT0 = {bt,0},
  BT1 = {bt,1},
  CpNum = {cpnum,15},
  CpOp3 = {cpop3,16#3},
  CpOp4 = {cpop4,16#F},
  L0 = {l,0},
  L1 = {l,1},
  S0 = {s,0},
  S1 = {s,1},
  FieldMask4 = {field_mask,16#F},
  Imm4 = {imm4,16#F},
  Imm5 = {imm5,16#1F},
  Imm8 = {imm8,16#FF},
  Imm12 = {imm12,16#FFF},
  Imm16 = {imm16,16#FFFF},
  Imm24 = {imm24,16#FFFFF},
  Imm25 = {imm25,16#FFFFF1},
  %%
  AM1_1 = {Imm8,Imm4},
  AM1_2 = Rm,
  AM1_3_1 = {Rm,{'lsl',Imm5}},
  AM1_3_2 = {Rm,{'lsr',Imm5}},
  AM1_3_3 = {Rm,{'asr',Imm5}},
  AM1_3_4 = {Rm,{'ror',Imm5}},
  AM1_3_5 = {Rm,{'lsl',Rs}},
  AM1_3_6 = {Rm,{'lsr',Rs}},
  AM1_3_7 = {Rm,{'asr',Rs}},
  AM1_3_8 = {Rm,{'ror',Rs}},
  AM1_3_9 = {Rm,'rrx'},
  %%
  AM2ShiftOp1 = {'lsl',Imm5},
  AM2ShiftOp2 = {'lsr',Imm5},
  AM2ShiftOp3 = {'asr',Imm5},
  AM2ShiftOp4 = {'ror',Imm5},
  AM2ShiftOp5 = 'rrx',
  SignP = '+',
  SignM = '-',
  AM2_1_1 = {immediate_offset,Rn,SignP,Imm12},
  AM2_1_2 = {immediate_offset,Rn,SignM,Imm12},
  AM2_2_1 = {register_offset,Rn,SignP,Rm},
  AM2_2_2 = {register_offset,Rn,SignM,Rm},
  AM2_3_1 = {scaled_register_offset,Rn,SignP,Rm,AM2ShiftOp1},
  AM2_3_2 = {scaled_register_offset,Rn,SignM,Rm,AM2ShiftOp2},
  AM2_3_3 = {scaled_register_offset,Rn,SignP,Rm,AM2ShiftOp3},
  AM2_3_4 = {scaled_register_offset,Rn,SignM,Rm,AM2ShiftOp4},
  AM2_3_5 = {scaled_register_offset,Rn,SignP,Rm,AM2ShiftOp5},
  AM2_4_1 = {immediate_pre_indexed,Rn,SignP,Imm12},
  AM2_4_2 = {immediate_pre_indexed,Rn,SignM,Imm12},
  AM2_5_1 = {register_pre_indexed,Rn,SignP,Rm},
  AM2_5_2 = {register_pre_indexed,Rn,SignM,Rm},
  AM2_6_1 = {scaled_register_pre_indexed,Rn,SignP,Rm,AM2ShiftOp1},
  AM2_6_2 = {scaled_register_pre_indexed,Rn,SignM,Rm,AM2ShiftOp2},
  AM2_6_3 = {scaled_register_pre_indexed,Rn,SignP,Rm,AM2ShiftOp3},
  AM2_6_4 = {scaled_register_pre_indexed,Rn,SignM,Rm,AM2ShiftOp4},
  AM2_6_5 = {scaled_register_pre_indexed,Rn,SignP,Rm,AM2ShiftOp5},
  AM2_7_1 = {immediate_post_indexed,Rn,SignP,Imm12},
  AM2_7_2 = {immediate_post_indexed,Rn,SignM,Imm12},
  AM2_8_1 = {register_post_indexed,Rn,SignP,Rm},
  AM2_8_2 = {register_post_indexed,Rn,SignM,Rm},
  AM2_9_1 = {scaled_register_post_indexed,Rn,SignP,Rm,AM2ShiftOp1},
  AM2_9_2 = {scaled_register_post_indexed,Rn,SignM,Rm,AM2ShiftOp2},
  AM2_9_3 = {scaled_register_post_indexed,Rn,SignP,Rm,AM2ShiftOp3},
  AM2_9_4 = {scaled_register_post_indexed,Rn,SignM,Rm,AM2ShiftOp4},
  AM2_9_5 = {scaled_register_post_indexed,Rn,SignP,Rm,AM2ShiftOp5},
  %%
  AM3_1_1 = {immediate_offset,Rn,SignP,Imm8},
  AM3_1_2 = {immediate_offset,Rn,SignM,Imm8},
  AM3_2_1 = {register_offset,Rn,SignP,Rm},
  AM3_2_2 = {register_offset,Rn,SignM,Rm},
  AM3_3_1 = {immediate_pre_indexed,Rn,SignP,Imm8},
  AM3_3_2 = {immediate_pre_indexed,Rn,SignM,Imm8},
  AM3_4_1 = {register_pre_indexed,Rn,SignP,Rm},
  AM3_4_2 = {register_pre_indexed,Rn,SignM,Rm},
  AM3_5_1 = {immediate_post_indexed,Rn,SignP,Imm8},
  AM3_5_2 = {immediate_post_indexed,Rn,SignM,Imm8},
  AM3_6_1 = {register_post_indexed,Rn,SignP,Rm},
  AM3_6_2 = {register_post_indexed,Rn,SignM,Rm},
  %%
  AM4_1 = 'ia',
  AM4_2 = 'ib',
  AM4_3 = 'da',
  AM4_4 = 'db',
  AM4_5 = 'fa',
  AM4_6 = 'fd',
  AM4_7 = 'ea',
  AM4_8 = 'ed',
  %%
  AM5_1_1 = {offset,Rn,SignP,Imm8},
  AM5_1_2 = {offset,Rn,SignM,Imm8},
  AM5_2_1 = {pre_indexed,Rn,SignP,Imm8},
  AM5_2_2 = {pre_indexed,Rn,SignM,Imm8},
  AM5_3_1 = {post_indexed,Rn,SignP,Imm8},
  AM5_3_2 = {post_indexed,Rn,SignM,Imm8},
  AM5_4 = {unindexed,Rn,Imm8},
  %%
  Cond_eq = {'cond','eq'},
  Cond_ne = {'cond','ne'},
  Cond_cs = {'cond','cs'},
  Cond_hs = {'cond','hs'},
  Cond_cc = {'cond','cc'},
  Cond_lo = {'cond','lo'},
  Cond_mi = {'cond','mi'},
  Cond_pl = {'cond','pl'},
  Cond_vs = {'cond','vs'},
  Cond_vc = {'cond','vc'},
  Cond_hi = {'cond','hi'},
  Cond_ls = {'cond','ls'},
  Cond_ge = {'cond','ge'},
  Cond_lt = {'cond','lt'},
  Cond_gt = {'cond','gt'},
  Cond_le = {'cond','le'},
  Cond_al = {'cond','al'},
  %%
  t(OS,'adc',{Cond_al,S0,Rd,Rn,AM1_1}),		% test all AM1 operands
  t(OS,'adc',{Cond_al,S0,Rd,Rn,AM1_2}),
  t(OS,'adc',{Cond_al,S0,Rd,Rn,AM1_3_1}),
  t(OS,'adc',{Cond_al,S0,Rd,Rn,AM1_3_2}),
  t(OS,'adc',{Cond_al,S0,Rd,Rn,AM1_3_3}),
  t(OS,'adc',{Cond_al,S0,Rd,Rn,AM1_3_4}),
  t(OS,'adc',{Cond_al,S0,Rd,Rn,AM1_3_5}),
  t(OS,'adc',{Cond_al,S0,Rd,Rn,AM1_3_6}),
  t(OS,'adc',{Cond_al,S0,Rd,Rn,AM1_3_7}),
  t(OS,'adc',{Cond_al,S0,Rd,Rn,AM1_3_8}),
  t(OS,'adc',{Cond_al,S0,Rd,Rn,AM1_3_9}),
  t(OS,'add',{Cond_al,S0,Rd,Rn,AM1_1}),		% test all S operands
  t(OS,'add',{Cond_al,S1,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_eq,S0,Rd,Rn,AM1_1}),		% test all Cond operands
  t(OS,'and',{Cond_ne,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_cs,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_hs,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_cc,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_lo,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_mi,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_pl,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_vs,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_vc,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_hi,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_ls,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_ge,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_lt,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_gt,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_le,S0,Rd,Rn,AM1_1}),
  t(OS,'and',{Cond_al,S0,Rd,Rn,AM1_1}),
  t(OS,'b',{Cond_al,Imm24}),
  t(OS,'bic',{Cond_al,S0,Rd,Rn,AM1_1}),
  t(OS,'bkpt',{Imm16}),
  t(OS,'bl',{Cond_al,Imm24}),
  t(OS,'blx',{Imm25}),
  t(OS,'blx',{Cond_al,Rm}),
  t(OS,'bx',{Cond_al,Rm}),
  t(OS,'cdp',{Cond_al,CpNum,CpOp4,CRd,CRn,CRm,CpOp3}),
  t(OS,'cdp2',{CpNum,CpOp4,CRd,CRn,CRm,CpOp3}),
  t(OS,'clz',{Cond_al,Rd,Rm}),
  t(OS,'cmn',{Cond_al,Rn,AM1_1}),
  t(OS,'cmp',{Cond_al,Rn,AM1_1}),
  t(OS,'eor',{Cond_al,S0,Rd,Rn,AM1_1}),
  t(OS,'ldc',{Cond_al,L0,CpNum,CRd,AM5_1_1}),	% test all AM5 operands
  t(OS,'ldc',{Cond_al,L1,CpNum,CRd,AM5_1_2}),
  t(OS,'ldc',{Cond_al,L0,CpNum,CRd,AM5_2_1}),
  t(OS,'ldc',{Cond_al,L1,CpNum,CRd,AM5_2_2}),
  t(OS,'ldc',{Cond_al,L0,CpNum,CRd,AM5_3_1}),
  t(OS,'ldc',{Cond_al,L1,CpNum,CRd,AM5_3_2}),
  t(OS,'ldc',{Cond_al,L0,CpNum,CRd,AM5_4}),
  t(OS,'ldc2',{L0,CpNum,CRd,AM5_1_1}),
  t(OS,'ldm',{Cond_al,AM4_1,Rn,'!',Registers}),
  t(OS,'ldm',{Cond_al,AM4_1,Rn,Registers}),	% test all AM4 operands
  t(OS,'ldm',{Cond_al,AM4_2,Rn,Registers}),	% test all AM4 operands
  t(OS,'ldm',{Cond_al,AM4_3,Rn,Registers}),	% test all AM4 operands
  t(OS,'ldm',{Cond_al,AM4_4,Rn,Registers}),	% test all AM4 operands
  t(OS,'ldm',{Cond_al,AM4_5,Rn,Registers}),	% test all AM4 operands
  t(OS,'ldm',{Cond_al,AM4_6,Rn,Registers}),	% test all AM4 operands
  t(OS,'ldm',{Cond_al,AM4_7,Rn,Registers}),	% test all AM4 operands
  t(OS,'ldm',{Cond_al,AM4_8,Rn,Registers}),	% test all AM4 operands
  t(OS,'ldr',{Cond_al,Rd,AM2_1_1}),		% test all AM2 operands
  t(OS,'ldr',{Cond_al,Rd,AM2_1_2}),
  t(OS,'ldr',{Cond_al,Rd,AM2_2_1}),
  t(OS,'ldr',{Cond_al,Rd,AM2_2_2}),
  t(OS,'ldr',{Cond_al,Rd,AM2_3_1}),
  t(OS,'ldr',{Cond_al,Rd,AM2_3_2}),
  t(OS,'ldr',{Cond_al,Rd,AM2_3_3}),
  t(OS,'ldr',{Cond_al,Rd,AM2_3_4}),
  t(OS,'ldr',{Cond_al,Rd,AM2_3_5}),
  t(OS,'ldr',{Cond_al,Rd,AM2_4_1}),
  t(OS,'ldr',{Cond_al,Rd,AM2_4_2}),
  t(OS,'ldr',{Cond_al,Rd,AM2_5_1}),
  t(OS,'ldr',{Cond_al,Rd,AM2_5_2}),
  t(OS,'ldr',{Cond_al,Rd,AM2_6_1}),
  t(OS,'ldr',{Cond_al,Rd,AM2_6_2}),
  t(OS,'ldr',{Cond_al,Rd,AM2_6_3}),
  t(OS,'ldr',{Cond_al,Rd,AM2_6_4}),
  t(OS,'ldr',{Cond_al,Rd,AM2_6_5}),
  t(OS,'ldr',{Cond_al,Rd,AM2_7_1}),
  t(OS,'ldr',{Cond_al,Rd,AM2_7_2}),
  t(OS,'ldr',{Cond_al,Rd,AM2_8_1}),
  t(OS,'ldr',{Cond_al,Rd,AM2_8_2}),
  t(OS,'ldr',{Cond_al,Rd,AM2_9_1}),
  t(OS,'ldr',{Cond_al,Rd,AM2_9_2}),
  t(OS,'ldr',{Cond_al,Rd,AM2_9_3}),
  t(OS,'ldr',{Cond_al,Rd,AM2_9_4}),
  t(OS,'ldr',{Cond_al,Rd,AM2_9_5}),
  t(OS,'ldrb',{Cond_al,Rd,AM2_1_1}),
  t(OS,'ldrd',{Cond_al,Rd,AM3_1_1}),
  t(OS,'ldrh',{Cond_al,Rd,AM3_1_1}),	% test all AM3 operands
  t(OS,'ldrh',{Cond_al,Rd,AM3_1_2}),
  t(OS,'ldrh',{Cond_al,Rd,AM3_2_1}),
  t(OS,'ldrh',{Cond_al,Rd,AM3_2_2}),
  t(OS,'ldrh',{Cond_al,Rd,AM3_3_1}),
  t(OS,'ldrh',{Cond_al,Rd,AM3_3_2}),
  t(OS,'ldrh',{Cond_al,Rd,AM3_4_1}),
  t(OS,'ldrh',{Cond_al,Rd,AM3_4_2}),
  t(OS,'ldrh',{Cond_al,Rd,AM3_5_1}),
  t(OS,'ldrh',{Cond_al,Rd,AM3_5_2}),
  t(OS,'ldrh',{Cond_al,Rd,AM3_6_1}),
  t(OS,'ldrh',{Cond_al,Rd,AM3_6_2}),
  t(OS,'ldrsb',{Cond_al,Rd,AM3_1_1}),
  t(OS,'ldrsh',{Cond_al,Rd,AM3_1_1}),
  t(OS,'mcr',{Cond_al,CpNum,CpOp3,Rd,CRn,CRm,CpOp3}),
  t(OS,'mcr2',{CpNum,CpOp3,Rd,CRn,CRm,CpOp3}),
  t(OS,'mcrr',{Cond_al,CpNum,CpOp4,Rd,Rn,CRm}),
  t(OS,'mla',{Cond_al,S0,Rd,Rm,Rs,Rn}),
  t(OS,'mov',{Cond_al,S0,Rd,AM1_1}),
  t(OS,'mrc',{Cond_al,CpNum,CpOp3,Rd,CRn,CRm,CpOp3}),
  t(OS,'mrc2',{CpNum,CpOp3,Rd,CRn,CRm,CpOp3}),
  t(OS,'mrrc',{Cond_al,CpNum,CpOp4,Rd,Rn,CRm}),
  t(OS,'mrs',{Cond_al,Rd,'cpsr'}),
  t(OS,'msr',{Cond_al,'cpsr',FieldMask4,Imm8,Imm4}),
  t(OS,'msr',{Cond_al,'cpsr',FieldMask4,Rm}),
  t(OS,'mul',{Cond_al,S0,Rd,Rm,Rs}),
  t(OS,'mvn',{Cond_al,S1,Rd,AM1_1}),
  t(OS,'orr',{Cond_al,S0,Rd,Rn,AM1_1}),
  t(OS,'pld',{AM2_1_1}),
  t(OS,'qadd',{Cond_al,Rd,Rm,Rn}),
  t(OS,'qdadd',{Cond_al,Rd,Rm,Rn}),
  t(OS,'qdsub',{Cond_al,Rd,Rm,Rn}),
  t(OS,'qsub',{Cond_al,Rd,Rm,Rn}),
  t(OS,'rsb',{Cond_al,S0,Rd,Rn,AM1_1}),
  t(OS,'rsc',{Cond_al,S0,Rd,Rn,AM1_1}),
  t(OS,'sbc',{Cond_al,S0,Rd,Rn,AM1_1}),
  t(OS,'smla',{BT0,BT0,Cond_al,Rd,Rm,Rs,Rn}),
  t(OS,'smla',{BT0,BT1,Cond_al,Rd,Rm,Rs,Rn}),
  t(OS,'smla',{BT1,BT0,Cond_al,Rd,Rm,Rs,Rn}),
  t(OS,'smla',{BT1,BT1,Cond_al,Rd,Rm,Rs,Rn}),
  t(OS,'smlal',{Cond_al,S0,RdLo,RdHi,Rm,Rs}),
  t(OS,'smlal',{BT0,BT1,Cond_al,RdLo,RdHi,Rm,Rs}),
  t(OS,'smlaw',{BT1,Cond_al,Rd,Rm,Rs,Rn}),
  t(OS,'smull',{Cond_al,S0,RdLo,RdHi,Rm,Rs}),
  t(OS,'smul',{BT1,BT0,Cond_al,Rd,Rm,Rs}),
  t(OS,'smulw',{BT1,Cond_al,Rd,Rm,Rs}),
  t(OS,'stc',{Cond_al,L0,CpNum,CRd,AM5_1_1}),
  t(OS,'stc2',{L0,CpNum,CRd,AM5_1_1}),
  t(OS,'stm',{Cond_al,AM4_1,Rn,Registers}),
  t(OS,'str',{Cond_al,Rd,AM2_1_1}),
  t(OS,'strb',{Cond_al,Rd,AM2_1_1}),
  t(OS,'strd',{Cond_al,Rd,AM3_1_1}),
  t(OS,'strh',{Cond_al,Rd,AM3_1_1}),
  t(OS,'sub',{Cond_al,S0,Rd,Rn,AM1_1}),
  t(OS,'swi',{Cond_al,Imm24}),
  t(OS,'swp',{Cond_al,Rd,Rm,Rn}),
  t(OS,'swpb',{Cond_al,Rd,Rm,Rn}),
  t(OS,'teq',{Cond_al,Rn,AM1_1}),
  t(OS,'tst',{Cond_al,Rn,AM1_1}),
  t(OS,'umlal',{Cond_al,S0,RdLo,RdHi,Rm,Rs}),
  t(OS,'umull',{Cond_al,S0,RdLo,RdHi,Rm,Rs}),
  [].

dotest() -> dotest1(group_leader()).

dotest(File) ->
  {ok,OS} = file:open(File, [write]),
  dotest1(OS),
  file:close(OS).

-endif.
