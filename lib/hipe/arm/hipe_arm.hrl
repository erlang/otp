%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

%%%--------------------------------------------------------------------
%%% Basic Values:
%%%
%%% temp	::= #arm_temp{reg, type, allocatable}
%%% reg		::= <token from hipe_arm_registers>
%%% type	::= tagged | untagged
%%% allocatable	::= true | false
%%%
%%% sdesc	::= #arm_sdesc{exnlab, fsize, arity, live}
%%% exnlab	::= [] | label
%%% fsize	::= int32		(frame size in words)
%%% live	::= <tuple of int32>	(word offsets)
%%% arity	::= uint8
%%%
%%% mfa		::= #arm_mfa{atom, atom, arity}
%%% prim	::= #arm_prim{atom}

-record(arm_mfa, {m::atom(), f::atom(), a::arity()}).
-record(arm_prim, {prim}).
-record(arm_sdesc, {exnlab, fsize, arity::arity(), live}).
-record(arm_temp, {reg, type, allocatable}).

%%% Instruction Operands:
%%%
%%% aluop	::= adc | add | and | bic | eor | orr | rsb | rsc | sbc | sub
%%% cmpop	::= cmn | cmp | tst | teq	(alu with s flag and no dst)
%%% cond	::= eq | ne | hs | lo | mi | pl | vs | vc | hi | ls | ge | lt | gt | le | al
%%% ldop	::= ldr | ldrb			(am2)
%%% movop	::= mov | mvn			(alu with no src)
%%% stop	::= str | strb			(am2)
%%%
%%% dst		::= temp
%%% src		::= temp
%%%
%%% s		::= true | false
%%%
%%% imm<N>	::= <an N-bit non-negative integer>
%%%
%%% Note: am1 represents all 11 variants of "Adressing Mode 1".
%%%
%%% am1		::= {imm8,imm4}		imm8 rotated right 2*imm4 bits
%%%		  | src
%%%		  | {src,rrx}
%%%		  | {src,shiftop,imm5}
%%%		  | {src,shiftop,src}
%%% shiftop	::= lsl | lsr | asr | ror
%%%
%%% Note: am2 can represent the first 3 variants of "Addressing Mode 2",
%%% i.e., not the pre- or post-indexed variants.
%%%
%%% am2		::= #am2{src, sign, am2offset}
%%% am2offset	::= imm12 | src | {src,rrx} | {src,shiftop,imm5}
%%% sign	::= + | -
%%%
%%% Note: am3 can represent the first 2 variants of "Addressing Mode 3",
%%% i.e., not the pre- or post-indexed variants.
%%%
%%% am3		::= #am3{src, sign, am3offset}
%%% am3offset	::= imm8 | src
%%%
%%% fun		::= mfa | prim
%%% funv	::= mfa | prim | temp
%%%
%%% immediate	::= int32 | atom | {label,label_type}
%%% label_type	::= constant | closure | c_const

-record(am2, {src, sign, offset}).
-record(am3, {src, sign, offset}).

%%% Instructions:

-record(alu, {aluop, s, dst, src, am1}).% cond not included
-record(b_fun, {'fun', linkage}).	% known tailcall; cond not included
-record(b_label, {'cond', label}).	% local jump
-record(bl, {'fun', sdesc, linkage}).	% known recursive call; cond not included
-record(blx, {src, sdesc}).		% computed recursive call; cond not included
-record(cmp, {cmpop, src, am1}).	% cond not included
-record(comment, {term}).
-record(label, {label}).
-record(load, {ldop, dst, am2}).	% cond not included; ldrh/ldrsh not included
-record(ldrsb, {dst, am3}).		% cond not included
-record(move, {movop, s, dst, am1}).	% cond not included
-record(pseudo_bc, {'cond', true_label, false_label, pred}).
-record(pseudo_blr, {}).		% alias for "mov pc,lr" to help cfg
-record(pseudo_bx, {src}).		% alias for "mov pc,src" to help cfg
-record(pseudo_call, {funv, sdesc, contlab, linkage}).
-record(pseudo_call_prepare, {nrstkargs}).
-record(pseudo_li, {dst, imm, label}).	% pre-generated label for use by the assembler
-record(pseudo_move, {dst, src}).
-record(pseudo_switch, {jtab, index, labels}).
-record(pseudo_tailcall, {funv, arity, stkargs, linkage}).
-record(pseudo_tailcall_prepare, {}).
-record(smull, {dstlo, dsthi, src1, src2}). % cond not included, s not included
-record(store, {stop, src, am2}).	% cond not included; strh not included

%%% Function definitions.

-include("../misc/hipe_consttab.hrl").

-record(defun, {mfa :: mfa(), formals, code,
	       	data	  :: hipe_consttab(),
	        isclosure :: boolean(),
		isleaf    :: boolean(),
		var_range, label_range}).
