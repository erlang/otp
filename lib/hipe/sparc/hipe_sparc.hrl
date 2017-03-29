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

%%%--------------------------------------------------------------------
%%% Basic Values:
%%%
%%% temp	::= #sparc_temp{reg, type, allocatable}
%%% reg		::= <token from hipe_sparc_registers>
%%% type	::= tagged | untagged | double
%%% allocatable	::= true | false
%%%
%%% sdesc	::= #sparc_sdesc{exnlab, fsize, arity, live}
%%% exnlab	::= [] | label
%%% fsize	::= int32		(frame size in words)
%%% live	::= <tuple of int32>	(word offsets)
%%% arity	::= uint8
%%%
%%% mfa		::= #sparc_mfa{atom, atom, arity}
%%% prim	::= #sparc_prim{atom}

-record(sparc_mfa, {m::atom(), f::atom(), a::arity()}).
-record(sparc_prim, {prim}).
-record(sparc_sdesc, {exnlab, fsize, arity::arity(), live}).
-record(sparc_temp, {reg, type, allocatable}).
-record(sparc_simm13, {value}).
-record(sparc_uimm5, {value}).
-record(sparc_uimm6, {value}).	% shift counts in 64-bit mode
-record(sparc_uimm22, {value}).

%%% Instruction Operands:
%%%
%%% aluop	::= add | addcc | and | andcc | or | orcc
%%%		  | xor | xorcc | sub | subcc | mulx | smul
%%%		  | sll | srl | sra | sllx | srlx | srax
%%%		  | ldsb | ldsh | ldsw | ldub | lduh | lduw | ldx
%%%		  (HW has andn{,cc}, orn{,cc}, xnor{,cc}, addc{,cc},
%%%		   and subc{,cc}, but we don't use them)
%%% cond	::= n | e | le | l | leu | lu | neg | vs |
%%%		  | a | ne | g | ge | gu | geu | pos | vc
%%% rcond	::= z | lez | lz | nz | gz | gez
%%% stop	::= stb | stw | stx	(HW has sth, but we don't use it)
%%%
%%% immediate	::= int32 | atom | {label, label_type}
%%% label_type	::= constant | closure | c_const
%%%
%%% dst		::= temp
%%% src		::= temp
%%% src1	::= temp
%%% src2	::= temp
%%%		  | simm13 	(only in alu.src2, jmp.src2, jmpl.src2)
%%% base	::= src1
%%% disp	::= src2
%%%
%%% fun		::= mfa | prim
%%% funv	::= fun | temp
%%%
%%% fp_binop	::= faddd | fdivd | fmuld | fsubd
%%% fp_unop	::= fitod | fmovd | fnegd

%%% Instructions:

-record(alu, {aluop, src1, src2, dst}).
-record(bp, {'cond', label, pred}).	% local jump on %icc
-ifdef(notdef).	% XXX: only for sparc64, alas
-record(br, {rcond, src, label, pred}).	% local jump on register
-endif.
-record(call_rec, {'fun', sdesc, linkage}).	% known recursive call
-record(call_tail, {'fun', linkage}).	% known tailcall
-record(comment, {term}).
-record(jmp, {src1, src2, labels}).	% return, switch, or computed tailcall
-record(jmpl, {src, sdesc}).		% computed recursive call (jmpl [src+0],%o7)
-record(label, {label}).
-record(pseudo_bp, {'cond', true_label, false_label, pred}).
%%-record(pseudo_br, {rcond, src, true_label, false_label, pred}).
-record(pseudo_call, {funv, sdesc, contlab, linkage}).
-record(pseudo_call_prepare, {nrstkargs}).
-record(pseudo_move, {src, dst}).
-record(pseudo_ret, {}).
-record(pseudo_set, {imm, dst}).
-record(pseudo_spill_fmove, {src, temp, dst}).
-record(pseudo_spill_move, {src, temp, dst}).
-record(pseudo_tailcall, {funv, arity, stkargs, linkage}).
-record(pseudo_tailcall_prepare, {}).
-record(rdy, {dst}).
-record(sethi, {uimm22, dst}).
-record(store, {stop, src, base, disp}).
-record(fp_binary, {fp_binop, src1, src2, dst}).
-record(fp_unary, {fp_unop, src, dst}).
-record(pseudo_fload, {base, disp, dst, is_single}).
-record(pseudo_fmove, {src, dst}).
-record(pseudo_fstore, {src, base, disp}).

%%% Function definitions.

-include("../misc/hipe_consttab.hrl").

-record(defun, {mfa :: mfa(), formals, code,
	       	data	  :: hipe_consttab(),
	        isclosure :: boolean(),
		isleaf    :: boolean(),
		var_range, label_range}).
