%%% -*- erlang-indent-level: 4 -*-
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
%%% Encode symbolic PowerPC instructions to binary form.
%%% Copyright (C) 2003-2005, 2009  Mikael Pettersson
%%%
%%% Notes:
%%% - PowerPC manuals use reversed bit numbering. In a 32-bit word,
%%%   the most significant bit has number 0, and the least significant
%%%   bit has number 31.
%%% - PowerPC manuals list opcodes in decimal, not hex.
%%% - This module does not support AltiVec instructions.
%%%
%%% Instruction Operands:
%%%
%%% {li,LI}		long branch offset/address (24 bits, signed)
%%% {bo,BO}		branch control operand (5 bits, restricted)
%%% {bi,BI}		branch CR field and bits operand (5 bits)
%%% {bd,BD}		branch offset (14 bits, signed)
%%% {to,TO}		trap condition (5 bits)
%%% {nb,NB}		number of bytes to copy (5 bits)
%%% {sh,SH}		shift count (5 bits)
%%% {mb,MB}		mask begin bit number (5 bits)
%%% {mb6,MB6}		mask begin bit number (6 bits) (64-bit)
%%% {me,ME}		mask end bit number (5 bits)
%%% {me6,ME6}		mask end bit number (6 bits) (64-bit)
%%% {sr,SR}		segment register (4 bits)
%%% {crimm,IMM}		FPSCR CR image (4 bits)
%%% {simm,SIMM}		immediate operand (16 bits, signed)
%%% {uimm,UIMM}		immediate operand (16 bits, unsigned)
%%% {d,Disp}		load/store byte displacement (16 bits, signed)
%%% {ds,DS}		load/store word displacement (14 bits, signed) (64-bit)
%%% {r,R}		integer register (5 bits)
%%% {fr,FR}		floating-point register (5 bits)
%%% {crf,CRF}		CR field number (3 bits)
%%% {crb,CRB}		CR bit number (5 bits)
%%% {tbr,TBR}		TBR number (10 bits, 268 or 269)
%%% {spr,SPR}		SPR number (10 bits)
%%% {crm,CRM}		CR fields set (8 bits)
%%% {fm,FM}		FPSCR fields set (8 bits)

-module(hipe_ppc_encode).

-export([insn_encode/2]).

%-define(TESTING,1).
-ifdef(TESTING).
-export([dotest/0, dotest/1]).
-endif.

-define(ASSERT(G), if G -> [] ; true -> exit({assertion_failed,?MODULE,?LINE,??G}) end).

-define(BF(LB,RB,V), bf(LB,RB,V)).

bf(LeftBit, RightBit, Value) ->
    ?ASSERT(LeftBit >= 0),
    ?ASSERT(LeftBit =< RightBit),
    ?ASSERT(RightBit < 32),
    ?ASSERT(Value >= 0),
    ?ASSERT(Value < (1 bsl ((RightBit - LeftBit) + 1))),
    Value bsl (31 - RightBit).

-define(BIT(Pos,Val), ?BF(Pos,Pos,Val)).
-define(BITS(N,Val), ?BF(32-N,31,Val)).

%%% I-Form Instructions
%%% b, ba, bl, bla

b_AA_LK({{li,LI}}, AA, LK) ->
    ?BF(0,5,10#18) bor ?BF(6,29,LI) bor ?BIT(30,AA) bor ?BIT(31,LK).

%%% B-Form Instructions
%%% bc, bca, bcl, bcla

bc_AA_LK({{bo,BO}, {bi,BI}, {bd,BD}}, AA, LK) ->
    ?BF(0,5,10#16) bor ?BF(6,10,BO) bor ?BF(11,15,BI) bor ?BF(16,29,BD) bor ?BIT(30,AA) bor ?BIT(31,LK).

%%% SC-Form Instructions
%%% sc

sc({}) ->
    ?BF(0,5,10#17) bor ?BIT(30,1).

%%% D-Form Instructions
%%% addi, addic, addic., addis, mulli, subfic
%%% andi., andis., ori, oris, xori, xoris
%%% lbz, lbzu, lha, lhau, lhz, lhzu, lwz, lwzu, lfd, lfdu, lfs, lfsu, lmw
%%% stb, stbu, sth, sthu, stw, stwu, stfd, stfdu, stfs, stfsu, stmw
%%% cmpi, cmpli, twi
%%% tdi (64-bit)

d_form(OPCD, D, A, IMM) ->
    ?BF(0,5,OPCD) bor ?BF(6,10,D) bor ?BF(11,15,A) bor ?BF(16,31,IMM).

d_form_D_A_SIMM(OPCD, {{r,D}, {r,A}, {simm,SIMM}}) ->
    d_form(OPCD, D, A, SIMM).

addi(Opnds) -> d_form_D_A_SIMM(10#14, Opnds).
addic(Opnds) -> d_form_D_A_SIMM(10#12, Opnds).
addic_dot(Opnds) -> d_form_D_A_SIMM(10#13, Opnds).
addis(Opnds) -> d_form_D_A_SIMM(10#15, Opnds).
mulli(Opnds) -> d_form_D_A_SIMM(10#07, Opnds).
subfic(Opnds) -> d_form_D_A_SIMM(10#08, Opnds).

d_form_S_A_UIMM(OPCD, {{r,A}, {r,S}, {uimm,UIMM}}) ->
    d_form(OPCD, S, A, UIMM).

andi_dot(Opnds) -> d_form_S_A_UIMM(10#28, Opnds).
andis_dot(Opnds) -> d_form_S_A_UIMM(10#29, Opnds).
ori(Opnds) -> d_form_S_A_UIMM(10#24, Opnds).
oris(Opnds) -> d_form_S_A_UIMM(10#25, Opnds).
xori(Opnds) -> d_form_S_A_UIMM(10#26, Opnds).
xoris(Opnds) -> d_form_S_A_UIMM(10#27, Opnds).

d_form_D_A_d_simple(OPCD, {{r,D}, {d,Disp}, {r,A}}) ->
    d_form(OPCD, D, A, Disp).

d_form_D_A_d_update(OPCD, {{r,D}, {d,Disp}, {r,A}}) ->
    ?ASSERT(A =/= 0),
    ?ASSERT(A =/= D),
    d_form(OPCD, D, A, Disp).

lbz(Opnds) -> d_form_D_A_d_simple(10#34, Opnds).
lbzu(Opnds) -> d_form_D_A_d_update(10#35, Opnds).
lha(Opnds) -> d_form_D_A_d_simple(10#42, Opnds).
lhau(Opnds) -> d_form_D_A_d_update(10#43, Opnds).
lhz(Opnds) -> d_form_D_A_d_simple(10#40, Opnds).
lhzu(Opnds) -> d_form_D_A_d_update(10#41, Opnds).
lwz(Opnds) -> d_form_D_A_d_simple(10#32, Opnds).
lwzu(Opnds) -> d_form_D_A_d_update(10#33, Opnds).

d_form_frD_A_d_simple(OPCD, {{fr,D}, {d,Disp}, {r,A}}) ->
    d_form(OPCD, D, A, Disp).

d_form_frD_A_d_update(OPCD, {{fr,D}, {d,Disp}, {r,A}}) ->
    ?ASSERT(A =/= 0),
    d_form(OPCD, D, A, Disp).

lfd(Opnds) -> d_form_frD_A_d_simple(10#50, Opnds).
lfdu(Opnds) -> d_form_frD_A_d_update(10#51, Opnds).
lfs(Opnds) -> d_form_frD_A_d_simple(10#48, Opnds).
lfsu(Opnds) -> d_form_frD_A_d_update(10#49, Opnds).

lmw({{r,D}, {d,Disp}, {r,A}}) ->
    ?ASSERT(A < D),
    d_form(10#46, D, A, Disp).

d_form_S_A_d_simple(OPCD, {{r,S}, {d,Disp}, {r,A}}) ->
    d_form(OPCD, S, A, Disp).

d_form_S_A_d_update(OPCD, {{r,S}, {d,Disp}, {r,A}}) ->
    ?ASSERT(A =/= 0),
    d_form(OPCD, S, A, Disp).

stb(Opnds) -> d_form_S_A_d_simple(10#38, Opnds).
stbu(Opnds) -> d_form_S_A_d_update(10#39, Opnds).
sth(Opnds) -> d_form_S_A_d_simple(10#44, Opnds).
sthu(Opnds) -> d_form_S_A_d_update(10#45, Opnds).
stmw(Opnds) -> d_form_S_A_d_simple(10#47, Opnds).
stw(Opnds) -> d_form_S_A_d_simple(10#36, Opnds).
stwu(Opnds) -> d_form_S_A_d_update(10#37, Opnds).

d_form_frS_A_d_simple(OPCD, {{fr,S}, {d,Disp}, {r,A}}) ->
    d_form(OPCD, S, A, Disp).

d_form_frS_A_d_update(OPCD, {{fr,S}, {d,Disp}, {r,A}}) ->
    ?ASSERT(A =/= 0),
    d_form(OPCD, S, A, Disp).

stfd(Opnds) -> d_form_frS_A_d_simple(10#54, Opnds).
stfdu(Opnds) -> d_form_frS_A_d_update(10#55, Opnds).
stfs(Opnds) -> d_form_frS_A_d_simple(10#52, Opnds).
stfsu(Opnds) -> d_form_frS_A_d_update(10#53, Opnds).

cmpi({{crf,CRFD}, L, {r,A}, {simm,SIMM}}) ->
    %% ?ASSERT(L == 0),	% L must be zero in 32-bit code
    d_form(10#11, (CRFD bsl 2) bor L, A, SIMM).

cmpli({{crf,CRFD}, L, {r,A}, {uimm,UIMM}}) ->
    %% ?ASSERT(L == 0),	% L must be zero in 32-bit code
    d_form(10#10, (CRFD bsl 2) bor L, A, UIMM).

d_form_OPCD_TO_A_SIMM(OPCD, {{to,TO}, {r,A}, {simm,SIMM}}) ->
    d_form(OPCD, TO, A, SIMM).

tdi(Opnds) -> d_form_OPCD_TO_A_SIMM(10#02, Opnds). % 64-bit
twi(Opnds) -> d_form_OPCD_TO_A_SIMM(10#03, Opnds).

%%% DS-Form Instructions
%%% ld, ldu, lwa, std, stdu (64-bit)

ds_form(OPCD, D, A, DS, XO) ->
    ?BF(0,5,OPCD) bor ?BF(6,10,D) bor ?BF(11,15,A) bor ?BF(16,29,DS) bor ?BF(30,31,XO).

ds_form_D_A_DS_XO_simple(OPCD, {{r,D}, {ds,DS}, {r,A}}, XO) ->
    ds_form(OPCD, D, A, DS, XO).

ds_form_D_A_DS_XO_update(OPCD, {{r,D}, {ds,DS}, {r,A}}, XO) ->
    ?ASSERT(A =/= 0),
    ?ASSERT(A =/= D),
    ds_form(OPCD, D, A, DS, XO).

ld(Opnds) -> ds_form_D_A_DS_XO_simple(10#58, Opnds, 10#0). % 64-bit
ldu(Opnds) -> ds_form_D_A_DS_XO_update(10#58, Opnds, 10#1). % 64-bit
lwa(Opnds) -> ds_form_D_A_DS_XO_simple(10#58, Opnds, 10#2). % 64-bit
std(Opnds) -> ds_form_D_A_DS_XO_simple(10#62, Opnds, 10#0). % 64-bit
stdu(Opnds) -> ds_form_D_A_DS_XO_update(10#62, Opnds, 10#1). % 64-bit

%%% X-Form Instructions
%%% ecixw, lbzux, lbzx, lhaux, lhax, lhbrx, lhzux, lhzx, lwarx, lwbrx, lwzux, lwzx, lswx
%%% lwaux, lwax (64-bit)
%%% lfdux, lfdx, lfsux, lfsx
%%% lswi
%%% fabs, fctiw, fctiwz, fmr, fnabs, fneg, frsp
%%% fcfid, fctid, fctidz (64-bit)
%%% mfsrin
%%% mffs
%%% mfcr, mfmsr
%%% mfsr
%%% and, andc, eqv, nand, nor, or, orc, slw, sraw, srw, xor
%%% sld, srad, srd (64-bit)
%%% stwcx.
%%% stdcx. (64-bit)
%%% ecowx, stbx, stbux, sthbrx, sthx, sthux, stswx, stwbrx, stwx, stwux
%%% stdux, stdx (64-bit)
%%% stfdx, stfdux, stfiwx, stfsx, stfsux
%%% stswi
%%% cntlzw, extsb, extsh
%%% cntlzd, extsw (64-bit)
%%% mtmsr
%%% mtmsrd (64-bit)
%%% mtsr, mtsrin
%%% mtsrd, mtsrdin (64-bit)
%%% srawi
%%% sradi (64-bit)
%%% cmp, cmpl
%%% fcmpo, fcmpu
%%% mcrfs
%%% mcrxr (obsolete)
%%% mtfsfi
%%% tw
%%% td (64-bit)
%%% mtfsb0, mtfsb1
%%% dcba, dcbf, dcbi, dcbst, dcbt, dcbtst, dcbz, icbi
%%% tlbie
%%% eieio, sync, tlbia, tlbsync

x_form(OPCD, D, A, B, XO, Rc) ->
    ?BF(0,5,OPCD) bor ?BF(6,10,D) bor ?BF(11,15,A) bor ?BF(16,20,B) bor ?BF(21,30,XO) bor ?BIT(31,Rc).

x_form_D_A_B_XO_simple({{r,D}, {r,A}, {r,B}}, XO) ->
    x_form(10#31, D, A, B, XO, 0).

x_form_D_A_B_XO_update({{r,D}, {r,A}, {r,B}}, XO) ->
    ?ASSERT(A =/= 0),
    ?ASSERT(A =/= D),
    x_form(10#31, D, A, B, XO, 0).

eciwx(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#310). % optional
lbzux(Opnds) -> x_form_D_A_B_XO_update(Opnds, 10#119).
lbzx(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#87).
ldarx(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#84). % 64-bit
ldux(Opnds) -> x_form_D_A_B_XO_update(Opnds, 10#53). % 64-bit
ldx(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#21). % 64-bit
lhaux(Opnds) -> x_form_D_A_B_XO_update(Opnds, 10#375).
lhax(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#343).
lhbrx(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#790).
lhzux(Opnds) -> x_form_D_A_B_XO_update(Opnds, 10#311).
lhzx(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#279).
lswx(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#533). % XXX: incomplete checks
lwarx(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#20).
lwaux(Opnds) -> x_form_D_A_B_XO_update(Opnds, 10#373). % 64-bit
lwax(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#341). % 64-bit
lwbrx(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#534).
lwzux(Opnds) -> x_form_D_A_B_XO_update(Opnds, 10#55).
lwzx(Opnds) -> x_form_D_A_B_XO_simple(Opnds, 10#23).

x_form_frD_A_B_XO_simple({{fr,D}, {r,A}, {r,B}}, XO) ->
    x_form(10#31, D, A, B, XO, 0).

x_form_frD_A_B_XO_update({{fr,D}, {r,A}, {r,B}}, XO) ->
    ?ASSERT(A =/= 0),
    x_form(10#31, D, A, B, XO, 0).

lfdux(Opnds) -> x_form_frD_A_B_XO_update(Opnds, 10#631).
lfdx(Opnds) -> x_form_frD_A_B_XO_simple(Opnds, 10#599).
lfsux(Opnds) -> x_form_frD_A_B_XO_update(Opnds, 10#567).
lfsx(Opnds) -> x_form_frD_A_B_XO_simple(Opnds, 10#535).

lswi({{r,D}, {r,A}, {nb,NB}}) ->	% XXX: incomplete checks
    x_form(10#31, D, A, NB, 10#597, 0).

x_form_D_B_XO_Rc({{fr,D}, {fr,B}}, XO, Rc) ->
    x_form(10#63, D, 0, B, XO, Rc).

fabs_Rc(Opnds, Rc) -> x_form_D_B_XO_Rc(Opnds, 10#264, Rc).
fcfid_Rc(Opnds, Rc) -> x_form_D_B_XO_Rc(Opnds, 10#846, Rc). % 64-bit
fctid_Rc(Opnds, Rc) -> x_form_D_B_XO_Rc(Opnds, 10#814, Rc). % 64-bit
fctidz_Rc(Opnds, Rc) -> x_form_D_B_XO_Rc(Opnds, 10#815, Rc). % 64-bit
fctiw_Rc(Opnds, Rc) -> x_form_D_B_XO_Rc(Opnds, 10#14, Rc).
fctiwz_Rc(Opnds, Rc) -> x_form_D_B_XO_Rc(Opnds, 10#15, Rc).
fmr_Rc(Opnds, Rc) -> x_form_D_B_XO_Rc(Opnds, 10#72, Rc).
fnabs_Rc(Opnds, Rc) -> x_form_D_B_XO_Rc(Opnds, 10#136, Rc).
fneg_Rc(Opnds, Rc) -> x_form_D_B_XO_Rc(Opnds, 10#40, Rc).
frsp_Rc(Opnds, Rc) -> x_form_D_B_XO_Rc(Opnds, 10#12, Rc).

mfsrin({{r,D}, {r,B}}) -> % supervisor
    x_form(10#31, D, 0, B, 10#659, 0).

mffs_Rc({{fr,D}}, Rc) ->
    x_form(10#63, D, 0, 0, 10#583, Rc).

x_form_D_XO({{r,D}}, XO) ->
    x_form(10#31, D, 0, 0, XO, 0).

mfcr(Opnds) -> x_form_D_XO(Opnds, 10#19).
mfmsr(Opnds) -> x_form_D_XO(Opnds, 10#83). % supervisor

mfsr({{r,D}, {sr,SR}}) -> % supervisor
    x_form(10#31, D, ?BITS(4,SR), 0, 10#595, 0).

x_form_S_A_B_XO_Rc({{r,A}, {r,S}, {r,B}}, XO, Rc) ->
    x_form(10#31, S, A, B, XO, Rc).

and_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#28, Rc).
andc_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#60, Rc).
eqv_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#284, Rc).
nand_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#476, Rc).
nor_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#124, Rc).
or_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#444, Rc).
orc_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#412, Rc).
sld_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#27, Rc). % 64-bit
slw_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#24, Rc).
srad_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#794, Rc). % 64-bit
sraw_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#792, Rc).
srd_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#539, Rc). % 64-bit
srw_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#536, Rc).
xor_Rc(Opnds, Rc) -> x_form_S_A_B_XO_Rc(Opnds, 10#316, Rc).

xform_S_A_B_XO_1({{r,S}, {r,A}, {r,B}}, XO) ->
    x_form(10#31, S, A, B, XO, 1).

stdcx_dot(Opnds) -> xform_S_A_B_XO_1(Opnds, 10#214). % 64-bit
stwcx_dot(Opnds) -> xform_S_A_B_XO_1(Opnds, 10#150).

x_form_S_A_B_XO_simple({{r,S}, {r,A}, {r,B}}, XO) ->
    x_form(10#31, S, A, B, XO, 0).

x_form_S_A_B_XO_update({{r,S}, {r,A}, {r,B}}, XO) ->
    ?ASSERT(A =/= 0),
    x_form(10#31, S, A, B, XO, 0).

ecowx(Opnds) -> x_form_S_A_B_XO_simple(Opnds, 10#438). % optional
stbx(Opnds) -> x_form_S_A_B_XO_simple(Opnds, 10#215).
stbux(Opnds) -> x_form_S_A_B_XO_update(Opnds, 10#247).
sthbrx(Opnds) -> x_form_S_A_B_XO_simple(Opnds, 10#918).
stdx(Opnds) -> x_form_S_A_B_XO_simple(Opnds, 10#149). % 64-bit
stdux(Opnds) -> x_form_S_A_B_XO_update(Opnds, 10#181). % 64-bit
sthx(Opnds) -> x_form_S_A_B_XO_simple(Opnds, 10#407).
sthux(Opnds) -> x_form_S_A_B_XO_update(Opnds, 10#439).
stswx(Opnds) -> x_form_S_A_B_XO_simple(Opnds, 10#661).
stwbrx(Opnds) -> x_form_S_A_B_XO_simple(Opnds, 10#662).
stwx(Opnds) -> x_form_S_A_B_XO_simple(Opnds, 10#151).
stwux(Opnds) -> x_form_S_A_B_XO_update(Opnds, 10#183).

x_form_frS_A_B_XO_simple({{fr,S}, {r,A}, {r,B}}, XO) ->
    x_form(10#31, S, A, B, XO, 0).

x_form_frS_A_B_XO_update({{fr,S}, {r,A}, {r,B}}, XO) ->
    ?ASSERT(A =/= 0),
    x_form(10#31, S, A, B, XO, 0).

stfdx(Opnds) -> x_form_frS_A_B_XO_simple(Opnds, 10#727).
stfdux(Opnds) -> x_form_frS_A_B_XO_update(Opnds, 10#759).
stfiwx(Opnds) -> x_form_frS_A_B_XO_simple(Opnds, 10#983). % optional
stfsx(Opnds) -> x_form_frS_A_B_XO_simple(Opnds, 10#663).
stfsux(Opnds) -> x_form_frS_A_B_XO_update(Opnds, 10#695).

stswi({{r,S}, {r,A}, {nb,NB}}) ->
    x_form(10#31, S, A, NB, 10#725, 0).

x_form_S_A_XO_Rc({{r,A}, {r,S}}, XO, Rc) ->
    x_form(10#31, S, A, 0, XO, Rc).

cntlzd_Rc(Opnds, Rc) -> x_form_S_A_XO_Rc(Opnds, 10#58, Rc). % 64-bit
cntlzw_Rc(Opnds, Rc) -> x_form_S_A_XO_Rc(Opnds, 10#26, Rc).
extsb_Rc(Opnds, Rc) -> x_form_S_A_XO_Rc(Opnds, 10#954, Rc).
extsh_Rc(Opnds, Rc) -> x_form_S_A_XO_Rc(Opnds, 10#922, Rc).
extsw_Rc(Opnds, Rc) -> x_form_S_A_XO_Rc(Opnds, 10#986, Rc). % 64-bit

mtmsr({{r,S}}) -> % supervisor
    x_form(10#31, S, 0, 0, 10#146, 0).

mtmsrd({{r,S}}) -> % supervisor, 64-bit
    x_form(10#31, S, 0, 0, 10#178, 0).

mtsr({{sr,SR}, {r,S}}) -> % supervisor
    x_form(10#31, S, ?BITS(4,SR), 0, 10#210, 0).

mtsrd({{sr,SR}, {r,S}}) -> % supervisor, 64-bit
    x_form(10#31, S, ?BITS(4,SR), 0, 10#82, 0).

mtsrdin({{r,S}, {r,B}}) -> % supervisor, 64-bit
    x_form(10#31, S, 0, B, 10#114, 0).

mtsrin({{r,S}, {r,B}}) -> % supervisor, 32-bit
    x_form(10#31, S, 0, B, 10#242, 0).

slbia({}) -> % supervisor, 64-bit
    x_form(10#31, 0, 0, 0, 10#498, 0).

slbie({{r,B}}) -> % supervisor, 64-bit
    x_form(10#31, 0, 0, B, 10#434, 0).

srawi_Rc({{r,A}, {r,S}, {sh,SH}}, Rc) ->
    x_form(10#31, S, A, SH, 10#824, Rc).

x_form_crfD_L_A_B_XO({{crf,CRFD}, L, {r,A}, {r,B}}, XO) ->
    %% ?ASSERT(L == 0),	% L should be zero in 32-bit code
    x_form(10#31, (CRFD bsl 2) bor L, A, B, XO, 0).

cmp(Opnds) -> x_form_crfD_L_A_B_XO(Opnds, 0).
cmpl(Opnds) -> x_form_crfD_L_A_B_XO(Opnds, 10#32).

x_form_crfD_A_B_XO({{crf,CRFD}, {fr,A}, {fr,B}}, XO) ->
    x_form(10#63, CRFD bsl 2, A, B, XO, 0).

fcmpo(Opnds) -> x_form_crfD_A_B_XO(Opnds, 10#32).
fcmpu(Opnds) -> x_form_crfD_A_B_XO(Opnds, 0).

mcrfs({{crf,CRFD}, {crf,CRFS}}) ->
    x_form(10#63, CRFD bsl 2, CRFS bsl 2, 0, 10#64, 0).

%% mcrxr({{crf,CRFD}}) ->
%%     x_form(10#31, CRFD bsl 2, 0, 0, 10#512, 0).

mtfsfi_Rc({{crf,CRFD}, {crimm,IMM}}, Rc) ->
    x_form(10#63, CRFD bsl 2, 0, IMM bsl 1, 10#134, Rc).

x_form_TO_A_B_XO({{to,TO}, {r,A}, {r,B}}, XO) ->
    x_form(10#31, TO, A, B, XO, 0).

td(Opnds) -> x_form_TO_A_B_XO(Opnds, 10#68). % 64-bit
tw(Opnds) -> x_form_TO_A_B_XO(Opnds, 10#4).

x_form_crbD_XO_Rc({{crb,CRBD}}, XO, Rc) ->
    x_form(10#63, CRBD, 0, 0, XO, Rc).

mtfsb0_Rc(Opnds, Rc) -> x_form_crbD_XO_Rc(Opnds, 10#70, Rc).
mtfsb1_Rc(Opnds, Rc) -> x_form_crbD_XO_Rc(Opnds, 10#38, Rc).

x_form_A_B_XO({{r,A}, {r,B}}, XO) ->
    x_form(10#31, 0, A, B, XO, 0).

dcba(Opnds) -> x_form_A_B_XO(Opnds, 10#758). % optional
dcbf(Opnds) -> x_form_A_B_XO(Opnds, 10#86).
dcbi(Opnds) -> x_form_A_B_XO(Opnds, 10#470). % supervisor
dcbst(Opnds) -> x_form_A_B_XO(Opnds, 10#54).
dcbt(Opnds) -> x_form_A_B_XO(Opnds, 10#278).
dcbtst(Opnds) -> x_form_A_B_XO(Opnds, 10#246).
dcbz(Opnds) -> x_form_A_B_XO(Opnds, 10#1014).
icbi(Opnds) -> x_form_A_B_XO(Opnds, 10#982).

x_form_B_XO({{r,B}}, XO) ->
    x_form(10#31, 0, 0, B, XO, 0).

tlbie(Opnds) -> x_form_B_XO(Opnds, 10#306).	% supervisor, optional
tlbld(Opnds) -> x_form_B_XO(Opnds, 10#978).	% supervisor, optional
tlbli(Opnds) -> x_form_B_XO(Opnds, 10#1010).	% supervisor, optional

x_form_XO({}, XO) ->
    x_form(10#31, 0, 0, 0, XO, 0).

eieio(Opnds) -> x_form_XO(Opnds, 10#854).
sync(Opnds) -> x_form_XO(Opnds, 10#598).
tlbia(Opnds) -> x_form_XO(Opnds, 10#370). % supervisor, optional
tlbsync(Opnds) -> x_form_XO(Opnds, 10#566). % supervisor, optional

%%% XL-Form Instructions
%%% bcctr, bclr
%%% crand, crandc, creqv, crnand, crnor, cror, crorc, crxor
%%% mcrf
%%% isync, rfi
%%% rfid (64-bit)

xl_form(A, B, C, XO, LK) ->
    ?BF(0,5,10#19) bor ?BF(6,10,A) bor ?BF(11,15,B) bor ?BF(16,20,C) bor ?BF(21,30,XO) bor ?BIT(31,LK).

xl_form_BO_BI_XO_LK({{bo,BO}, {bi,BI}}, XO, LK) ->
    xl_form(BO, BI, 0, XO, LK).

bcctr_lk(Opnds, LK) -> xl_form_BO_BI_XO_LK(Opnds, 10#528, LK).
bclr_lk(Opnds, LK) -> xl_form_BO_BI_XO_LK(Opnds, 10#16, LK).

xl_form_crbD_crbA_crbB_XO({{crb,CRBD}, {crb,CRBA}, {crb,CRBB}}, XO) ->
    xl_form(CRBD, CRBA, CRBB, XO, 0).

crand(Opnds) -> xl_form_crbD_crbA_crbB_XO(Opnds, 10#257).
crandc(Opnds) -> xl_form_crbD_crbA_crbB_XO(Opnds, 10#129).
creqv(Opnds) -> xl_form_crbD_crbA_crbB_XO(Opnds, 10#289).
crnand(Opnds) -> xl_form_crbD_crbA_crbB_XO(Opnds, 10#225).
crnor(Opnds) -> xl_form_crbD_crbA_crbB_XO(Opnds, 10#33).
cror(Opnds) -> xl_form_crbD_crbA_crbB_XO(Opnds, 10#449).
crorc(Opnds) -> xl_form_crbD_crbA_crbB_XO(Opnds, 10#417).
crxor(Opnds) -> xl_form_crbD_crbA_crbB_XO(Opnds, 10#193).

mcrf({{crf,CRFD}, {crf,CRFS}}) ->
    xl_form(CRFD bsl 2, CRFS bsl 2, 0, 0, 0).

xl_form_XO({}, XO) ->
    xl_form(0, 0, 0, XO, 0).

isync(Opnds) -> xl_form_XO(Opnds, 10#150).
rfi(Opnds) -> xl_form_XO(Opnds, 10#50). % supervisor
rfid(Opnds) -> xl_form_XO(Opnds, 10#18). % supervisor, 64-bit

%%% XFX-Form Instructions
%%% mfspr, mtspr, mftb, mtcrf

xfx_form(A, B, XO) ->
    ?BF(0,5,10#31) bor ?BF(6,10,A) bor ?BF(11,20,B) bor ?BF(21,30,XO).

xfx_form_R_SPR_XO(R, SPR, XO) ->
    SPR04 = SPR band 16#1F,
    SPR59 = (SPR bsr 5) band 16#1F,
    xfx_form(R, (SPR04 bsl 5) bor SPR59, XO).

mfspr({{r,D}, {spr,SPR}}) -> xfx_form_R_SPR_XO(D, SPR, 10#339).
mtspr({{spr,SPR}, {r,S}}) -> xfx_form_R_SPR_XO(S, SPR, 10#467).
mftb({{r,D}, {tbr,TBR}}) -> xfx_form_R_SPR_XO(D, TBR, 10#371).

mtcrf({{crm,CRM}, {r,S}}) -> xfx_form(S, ?BITS(8,CRM) bsl 1, 10#144).

%%% XFL-Form Instructions
%%% mtfsf

xfl_form(FM, B, Rc) ->
    ?BF(0,5,10#63) bor ?BF(7,14,FM) bor ?BF(16,20,B) bor ?BF(21,30,10#711) bor ?BIT(31,Rc).

mtfsf_Rc({{fm,FM}, {fr,B}}, Rc) -> xfl_form(FM, B, Rc).

%%% XS-Form Instructions
%%% sradi (64-bit)

xs_form(S, A, SH1, XO, SH2, Rc) ->
    ?BF(0,5,10#31) bor ?BF(6,10,S) bor ?BF(11,15,A) bor ?BF(16,20,SH1) bor ?BF(21,29,XO) bor ?BIT(30,SH2) bor ?BIT(31,Rc).

sradi_Rc({{r,A}, {r,S}, {sh6,SH6}}, Rc) -> % 64-bit
    xs_form(S, A, sh6_bits0to4(SH6), 10#413, sh6_bit5(SH6), Rc).

%%% XO-Form Instructions
%%% add, addc, adde, divw, divwu, mullw, subf, subfc, subfe
%%% divd, divdu, mulld (64-bit)
%%% mulhw, mulhwu
%%% mulhd, mulhdu (64-bit)
%%% addme, addze, neg, subfme, subfze

xo_form(D, A, B, OE, XO, Rc) ->
    ?BF(0,5,10#31) bor ?BF(6,10,D) bor ?BF(11,15,A) bor ?BF(16,20,B) bor ?BIT(21,OE) bor ?BF(22,30,XO) bor ?BIT(31,Rc).

xo_form_D_A_B_OE_XO_Rc({{r,D}, {r,A}, {r,B}}, OE, XO, Rc) ->
    xo_form(D, A, B, OE, XO, Rc).

add_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#266, Rc).
addc_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#10, Rc).
adde_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#138, Rc).
divd_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#489, Rc). % 64-bit
divdu_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#457, Rc). % 64-bit
divw_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#491, Rc).
divwu_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#459, Rc).
mulld_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#233, Rc). % 64-bit
mullw_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#235, Rc).
subf_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#40, Rc).
subfc_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#8, Rc).
subfe_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, OE, 10#136, Rc).

mulhd_Rc(Opnds, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, 0, 10#73, Rc). % 64-bit
mulhdu_Rc(Opnds, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, 0, 10#9, Rc). % 64-bit
mulhw_Rc(Opnds, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, 0, 10#75, Rc).
mulhwu_Rc(Opnds, Rc) -> xo_form_D_A_B_OE_XO_Rc(Opnds, 0, 10#11, Rc).

xo_form_D_A_OE_XO_Rc({{r,D}, {r,A}}, OE, XO, Rc) ->
    xo_form(D, A, 0, OE, XO, Rc).

addme_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_OE_XO_Rc(Opnds, OE, 10#234, Rc).
addze_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_OE_XO_Rc(Opnds, OE, 10#202, Rc).
neg_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_OE_XO_Rc(Opnds, OE, 10#104, Rc).
subfme_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_OE_XO_Rc(Opnds, OE, 10#232, Rc).
subfze_OE_Rc(Opnds, OE, Rc) -> xo_form_D_A_OE_XO_Rc(Opnds, OE, 10#200, Rc).

%%% A-Form Instructions
%%% fadd, fadds, fdiv, fdivs, fsub, fsubs
%%% fmadd, fmadds, fmsub, fmsubs, fnmadd, fnmadds, fnmsub, fnmsubs, fsel
%%% fmul, fmuls
%%% fres, fsqrte, fsqrt, fsqrts

a_form(OPCD, D, A, B, C, XO, Rc) ->
    ?BF(0,5,OPCD) bor ?BF(6,10,D) bor ?BF(11,15,A) bor ?BF(16,20,B) bor ?BF(21,25,C) bor ?BF(26,30,XO) bor ?BIT(31,Rc).

a_form_D_A_B_XO_Rc(OPCD, {{fr,D}, {fr,A}, {fr,B}}, XO, Rc) ->
    a_form(OPCD, D, A, B, 0, XO, Rc).

fadd_OPCD_Rc(OPCD, Opnds, Rc) -> a_form_D_A_B_XO_Rc(OPCD, Opnds, 10#21, Rc).
fadd_Rc(Opnds, Rc) -> fadd_OPCD_Rc(10#63, Opnds, Rc).
fadds_Rc(Opnds, Rc) -> fadd_OPCD_Rc(10#59, Opnds, Rc).

fdiv_OPCD_Rc(OPCD, Opnds, Rc) -> a_form_D_A_B_XO_Rc(OPCD, Opnds, 10#18, Rc).
fdiv_Rc(Opnds, Rc) -> fdiv_OPCD_Rc(10#63, Opnds, Rc).
fdivs_Rc(Opnds, Rc) -> fdiv_OPCD_Rc(10#59, Opnds, Rc).

fsub_OPCD_Rc(OPCD, Opnds, Rc) -> a_form_D_A_B_XO_Rc(OPCD, Opnds, 10#20, Rc).
fsub_Rc(Opnds, Rc) -> fsub_OPCD_Rc(10#63, Opnds, Rc).
fsubs_Rc(Opnds, Rc) -> fsub_OPCD_Rc(10#59, Opnds, Rc).

a_form_D_A_B_C_XO_Rc(OPCD, {{fr,D}, {fr,A}, {fr,C}, {fr,B}}, XO, Rc) ->
    a_form(OPCD, D, A, B, C, XO, Rc).

fmadd_OPCD_Rc(OPCD, Opnds, Rc) -> a_form_D_A_B_C_XO_Rc(OPCD, Opnds, 10#29, Rc).
fmadd_Rc(Opnds, Rc) -> fmadd_OPCD_Rc(10#63, Opnds, Rc).
fmadds_Rc(Opnds, Rc) -> fmadd_OPCD_Rc(10#59, Opnds, Rc).

fmsub_OPCD_Rc(OPCD, Opnds, Rc) -> a_form_D_A_B_C_XO_Rc(OPCD, Opnds, 10#28, Rc).
fmsub_Rc(Opnds, Rc) -> fmsub_OPCD_Rc(10#63, Opnds, Rc).
fmsubs_Rc(Opnds, Rc) -> fmsub_OPCD_Rc(10#59, Opnds, Rc).

fnmadd_OPCD_Rc(OPCD, Opnds, Rc) -> a_form_D_A_B_C_XO_Rc(OPCD, Opnds, 10#31, Rc).
fnmadd_Rc(Opnds, Rc) -> fnmadd_OPCD_Rc(10#63, Opnds, Rc).
fnmadds_Rc(Opnds, Rc) -> fnmadd_OPCD_Rc(10#59, Opnds, Rc).

fnmsub_OPCD_Rc(OPCD, Opnds, Rc) -> a_form_D_A_B_C_XO_Rc(OPCD, Opnds, 10#30, Rc).
fnmsub_Rc(Opnds, Rc) -> fnmsub_OPCD_Rc(10#63, Opnds, Rc).
fnmsubs_Rc(Opnds, Rc) -> fnmsub_OPCD_Rc(10#59, Opnds, Rc).

fsel_Rc(Opnds, Rc) -> a_form_D_A_B_C_XO_Rc(10#63, Opnds, 10#23, Rc). % optional

fmul_OPCD_Rc(OPCD, {{fr,D}, {fr,A}, {fr,C}}, Rc) ->
    a_form(OPCD, D, A, 0, C, 10#25, Rc).

fmul_Rc(Opnds, Rc) -> fmul_OPCD_Rc(10#63, Opnds, Rc).
fmuls_Rc(Opnds, Rc) -> fmul_OPCD_Rc(10#59, Opnds, Rc).

a_form_D_B_XO_Rc(OPCD, {{fr,D}, {fr,B}}, XO, Rc) ->
    a_form(OPCD, D, 0, B, 0, XO, Rc).

fres_Rc(Opnds, Rc) -> a_form_D_B_XO_Rc(10#59, Opnds, 10#24, Rc). % optional
frsqrte_Rc(Opnds, Rc) -> a_form_D_B_XO_Rc(10#63, Opnds, 10#26, Rc). % optional

fsqrt_OPCD_Rc(OPCD, Opnds, Rc) -> a_form_D_B_XO_Rc(OPCD, Opnds, 10#22, Rc). % optional
fsqrt_Rc(Opnds, Rc) -> fsqrt_OPCD_Rc(10#63, Opnds, Rc). % optional
fsqrts_Rc(Opnds, Rc) -> fsqrt_OPCD_Rc(10#59, Opnds, Rc). % optional

%%% M-Form Instructions
%%% rlwimi, rlwinm
%%% rlwnm

m_form(OPCD, S, A, SH, MB, ME, Rc) ->
    ?BF(0,5,OPCD) bor ?BF(6,10,S) bor ?BF(11,15,A) bor ?BF(16,20,SH) bor ?BF(21,25,MB) bor ?BF(26,30,ME) bor ?BIT(31,Rc).

m_form_S_A_SH_MB_ME_Rc(OPCD, {{r,A}, {r,S}, {sh,SH}, {mb,MB}, {me,ME}}, Rc) ->
    m_form(OPCD, S, A, SH, MB, ME, Rc).

rlwimi_Rc(Opnds, Rc) -> m_form_S_A_SH_MB_ME_Rc(10#20, Opnds, Rc).
rlwinm_Rc(Opnds, Rc) -> m_form_S_A_SH_MB_ME_Rc(10#21, Opnds, Rc).

rlwnm_Rc({{r,A}, {r,S}, {r,B}, {mb,MB}, {me,ME}}, Rc) ->
    m_form(10#23, S, A, B, MB, ME, Rc).

%%% MD-Form Instructions
%%% rldic, rldicl, rldicr, rldimi (64-bit)

md_form(S, A, SH1, MB, XO, SH2, Rc) ->
    ?BF(0,5,10#30) bor ?BF(6,10,S) bor ?BF(11,15,A) bor ?BF(16,20,SH1) bor ?BF(21,26,MB) bor ?BF(27,29,XO) bor ?BIT(30,SH2) bor ?BIT(31,Rc).

mb6_reformat(MB6) ->
    ((MB6 band 16#1F) bsl 1) bor ((MB6 bsr 5) band 1).

sh6_bits0to4(SH6) ->
    SH6 band 16#1F.

sh6_bit5(SH6) ->
    (SH6 bsr 5) band 1.

md_form_S_A_SH6_MB6_XO_Rc({{r,A}, {r,S}, {sh6,SH6}, {mb6,MB6}}, XO, Rc) ->
    md_form(S, A, sh6_bits0to4(SH6), mb6_reformat(MB6), XO, sh6_bit5(SH6), Rc).

rldic_Rc(Opnds, Rc) -> md_form_S_A_SH6_MB6_XO_Rc(Opnds, 10#2, Rc). % 64-bit
rldicl_Rc(Opnds, Rc) -> md_form_S_A_SH6_MB6_XO_Rc(Opnds, 10#0, Rc). % 64-bit
rldimi_Rc(Opnds, Rc) -> md_form_S_A_SH6_MB6_XO_Rc(Opnds, 10#3, Rc). % 64-bit

rldicr_Rc({{r,A}, {r,S}, {sh6,SH6}, {me6,ME6}}, Rc) -> % 64-bit
    md_form(S, A, sh6_bits0to4(SH6), mb6_reformat(ME6), 10#1, sh6_bit5(SH6), Rc).

%%% MDS-Form Instructions
%%% rldcl, rldcr (64-bit)

mds_form(S, A, B, MB, XO, Rc) ->
    ?BF(0,5,10#30) bor ?BF(6,10,S) bor ?BF(11,15,A) bor ?BF(16,20,B) bor ?BF(21,26,MB) bor ?BF(27,30,XO) bor ?BIT(31,Rc).

rldcl({{r,A}, {r,S}, {r,B}, {mb6,MB6}}, Rc) -> % 64-bit
    mds_form(S, A, B, mb6_reformat(MB6), 10#8, Rc).

rldcr({{r,A}, {r,S}, {r,B}, {me6,ME6}}, Rc) -> % 64-bit
    mds_form(S, A, B, mb6_reformat(ME6), 10#9, Rc).

%%% main encode dispatch

insn_encode(Op, Opnds) ->
    case Op of
	%% I-Form
	'b' -> b_AA_LK(Opnds, 0, 0);
	'ba' -> b_AA_LK(Opnds, 1, 0);
	'bl' -> b_AA_LK(Opnds, 0, 1);
	'bla' -> b_AA_LK(Opnds, 1, 1);
	%% B-Form
	'bc' -> bc_AA_LK(Opnds, 0, 0);
	'bca' -> bc_AA_LK(Opnds, 1, 0);
	'bcl' -> bc_AA_LK(Opnds, 0, 1);
	'bcla' -> bc_AA_LK(Opnds, 1, 1);
	%% SC-Form
	'sc' -> sc(Opnds);
	%% D-Form
	'addi' -> addi(Opnds);
	'addic' -> addic(Opnds);
	'addic.' -> addic_dot(Opnds);
	'addis' -> addis(Opnds);
	'andi.' -> andi_dot(Opnds);
	'andis.' -> andis_dot(Opnds);
	'cmpi' -> cmpi(Opnds);
	'cmpli' -> cmpli(Opnds);
	'lbz' -> lbz(Opnds);
	'lbzu' -> lbzu(Opnds);
	'lfd' -> lfd(Opnds);
	'lfdu' -> lfdu(Opnds);
	'lfs' -> lfs(Opnds);
	'lfsu' -> lfsu(Opnds);
	'lha' -> lha(Opnds);
	'lhau' -> lhau(Opnds);
	'lhz' -> lhz(Opnds);
	'lhzu' -> lhzu(Opnds);
	'lmw' -> lmw(Opnds);
	'lwz' -> lwz(Opnds);
	'lwzu' -> lwzu(Opnds);
	'mulli' -> mulli(Opnds);
	'ori' -> ori(Opnds);
	'oris' -> oris(Opnds);
	'stb' -> stb(Opnds);
	'stbu' -> stbu(Opnds);
	'stfd' -> stfd(Opnds);
	'stfdu' -> stfdu(Opnds);
	'stfs' -> stfs(Opnds);
	'stfsu' -> stfsu(Opnds);
	'sth' -> sth(Opnds);
	'sthu' -> sthu(Opnds);
	'stmw' -> stmw(Opnds);
	'stw' -> stw(Opnds);
	'stwu' -> stwu(Opnds);
	'subfic' -> subfic(Opnds);
	'tdi' -> tdi(Opnds);
	'twi' -> twi(Opnds);
	'xori' -> xori(Opnds);
	'xoris' -> xoris(Opnds);
	%% DS-Form
	'ld' -> ld(Opnds);
	'ldu' -> ldu(Opnds);
	'lwa' -> lwa(Opnds);
	'std' -> std(Opnds);
	'stdu' -> stdu(Opnds);
	%% X-Form
	'and' -> and_Rc(Opnds, 0);
	'and.' -> and_Rc(Opnds, 1);
	'andc' -> andc_Rc(Opnds, 0);
	'andc.' -> andc_Rc(Opnds, 1);
	'cmp' -> cmp(Opnds);
	'cmpl' -> cmpl(Opnds);
	'cntlzd' -> cntlzd_Rc(Opnds, 0);
	'cntlzd.' -> cntlzd_Rc(Opnds, 1);
	'cntlzw' -> cntlzw_Rc(Opnds, 0);
	'cntlzw.' -> cntlzw_Rc(Opnds, 1);
	'dcba' -> dcba(Opnds);
	'dcbf' -> dcbf(Opnds);
	'dcbi' -> dcbi(Opnds);
	'dcbst' -> dcbst(Opnds);
	'dcbt' -> dcbt(Opnds);
	'dcbtst' -> dcbtst(Opnds);
	'dcbz' -> dcbz(Opnds);
	'eciwx' -> eciwx(Opnds);
	'ecowx' -> ecowx(Opnds);
	'eieio' -> eieio(Opnds);
	'eqv' -> eqv_Rc(Opnds, 0);
	'eqv.' -> eqv_Rc(Opnds, 1);
	'extsb' -> extsb_Rc(Opnds, 0);
	'extsb.' -> extsb_Rc(Opnds, 1);
	'extsh' -> extsh_Rc(Opnds, 0);
	'extsh.' -> extsh_Rc(Opnds, 1);
	'extsw' -> extsw_Rc(Opnds, 0);
	'extsw.' -> extsw_Rc(Opnds, 1);
	'fabs' -> fabs_Rc(Opnds, 0);
	'fabs.' -> fabs_Rc(Opnds, 1);
	'fcfid' -> fcfid_Rc(Opnds, 0);
	'fcfid.' -> fcfid_Rc(Opnds, 1);
	'fcmpo' -> fcmpo(Opnds);
	'fcmpu' -> fcmpu(Opnds);
	'fctid' -> fctid_Rc(Opnds, 0);
	'fctid.' -> fctid_Rc(Opnds, 1);
	'fctidz' -> fctidz_Rc(Opnds, 0);
	'fctidz.' -> fctidz_Rc(Opnds, 1);
	'fctiw' -> fctiw_Rc(Opnds, 0);
	'fctiw.' -> fctiw_Rc(Opnds, 1);
	'fctiwz' -> fctiwz_Rc(Opnds, 0);
	'fctiwz.' -> fctiwz_Rc(Opnds, 1);
	'fmr' -> fmr_Rc(Opnds, 0);
	'fmr.' -> fmr_Rc(Opnds, 1);
	'fnabs' -> fnabs_Rc(Opnds, 0);
	'fnabs.' -> fnabs_Rc(Opnds, 1);
	'fneg' -> fneg_Rc(Opnds, 0);
	'fneg.' -> fneg_Rc(Opnds, 1);
	'frsp' -> frsp_Rc(Opnds, 0);
	'frsp.' -> frsp_Rc(Opnds, 1);
	'icbi' -> icbi(Opnds);
	'lbzux' -> lbzux(Opnds);
	'lbzx' -> lbzx(Opnds);
	'ldarx' -> ldarx(Opnds);
	'ldux' -> ldux(Opnds);
	'ldx' -> ldx(Opnds);
	'lfdux' -> lfdux(Opnds);
	'lfdx' -> lfdx(Opnds);
	'lfsux' -> lfsux(Opnds);
	'lfsx' -> lfsx(Opnds);
	'lhaux' -> lhaux(Opnds);
	'lhax' -> lhax(Opnds);
	'lhbrx' -> lhbrx(Opnds);
	'lhzux' -> lhzux(Opnds);
	'lhzx' -> lhzx(Opnds);
	'lswi' -> lswi(Opnds);
	'lswx' -> lswx(Opnds);
	'lwarx' -> lwarx(Opnds);
	'lwaux' -> lwaux(Opnds);
	'lwax' -> lwax(Opnds);
	'lwbrx' -> lwbrx(Opnds);
	'lwzux' -> lwzux(Opnds);
	'lwzx' -> lwzx(Opnds);
	'mcrfs' -> mcrfs(Opnds);
	%% 'mcrxr' -> mcrxr(Opnds);
	'mfcr' -> mfcr(Opnds);
	'mffs' -> mffs_Rc(Opnds, 0);
	'mffs.' -> mffs_Rc(Opnds, 1);
	'mfmsr' -> mfmsr(Opnds);
	'mfsr' -> mfsr(Opnds);
	'mfsrin' -> mfsrin(Opnds);
	'mtfsb0' -> mtfsb0_Rc(Opnds, 0);
	'mtfsb0.' -> mtfsb0_Rc(Opnds, 1);
	'mtfsb1' -> mtfsb1_Rc(Opnds, 0);
	'mtfsb1.' -> mtfsb1_Rc(Opnds, 1);
	'mtfsfi' -> mtfsfi_Rc(Opnds, 0);
	'mtfsfi.' -> mtfsfi_Rc(Opnds, 1);
	'mtmsr' -> mtmsr(Opnds);
	'mtmsrd' -> mtmsrd(Opnds);
	'mtsr' -> mtsr(Opnds);
	'mtsrd' -> mtsrd(Opnds);
	'mtsrdin' -> mtsrdin(Opnds);
	'mtsrin' -> mtsrin(Opnds);
	'nand' -> nand_Rc(Opnds, 0);
	'nand.' -> nand_Rc(Opnds, 1);
	'nor' -> nor_Rc(Opnds, 0);
	'nor.' -> nor_Rc(Opnds, 1);
	'or' -> or_Rc(Opnds, 0);
	'or.' -> or_Rc(Opnds, 1);
	'orc' -> orc_Rc(Opnds, 0);
	'orc.' -> orc_Rc(Opnds, 1);
	'slbia' -> slbia(Opnds);
	'slbie' -> slbie(Opnds);
	'sld' -> sld_Rc(Opnds, 0);
	'sld.' -> sld_Rc(Opnds, 1);
	'slw' -> slw_Rc(Opnds, 0);
	'slw.' -> slw_Rc(Opnds, 1);
	'srad' -> srad_Rc(Opnds, 0);
	'srad.' -> srad_Rc(Opnds, 1);
	'sraw' -> sraw_Rc(Opnds, 0);
	'sraw.' -> sraw_Rc(Opnds, 1);
	'srawi' -> srawi_Rc(Opnds, 0);
	'srawi.' -> srawi_Rc(Opnds, 1);
	'srd' -> srd_Rc(Opnds, 0);
	'srd.' -> srd_Rc(Opnds, 1);
	'srw' -> srw_Rc(Opnds, 0);
	'srw.' -> srw_Rc(Opnds, 1);
	'stbux' -> stbux(Opnds);
	'stbx' -> stbx(Opnds);
	'stdcx.' -> stdcx_dot(Opnds);
	'stdux' -> stdux(Opnds);
	'stdx' -> stdx(Opnds);	
	'stfdux' -> stfdux(Opnds);
	'stfdx' -> stfdx(Opnds);
	'stfiwx' -> stfiwx(Opnds);
	'stfsux' -> stfsux(Opnds);
	'stfsx' -> stfsx(Opnds);
	'sthbrx' -> sthbrx(Opnds);
	'sthux' -> sthux(Opnds);
	'sthx' -> sthx(Opnds);
	'stswi' -> stswi(Opnds);
	'stswx' -> stswx(Opnds);
	'stwbrx' -> stwbrx(Opnds);
	'stwcx.' -> stwcx_dot(Opnds);
	'stwux' -> stwux(Opnds);
	'stwx' -> stwx(Opnds);
	'sync' -> sync(Opnds);
	'td' -> td(Opnds);
	'tlbia' -> tlbia(Opnds);	% not implemented in MPC603e or MPC7450
	'tlbie' -> tlbie(Opnds);
	'tlbld' -> tlbld(Opnds);
	'tlbli' -> tlbli(Opnds);
	'tlbsync' -> tlbsync(Opnds);
	'tw' -> tw(Opnds);
	'xor' -> xor_Rc(Opnds, 0);
	'xor.' -> xor_Rc(Opnds, 1);
	%% XL-Form
	'bcctr' -> bcctr_lk(Opnds, 0);
	'bcctrl' -> bcctr_lk(Opnds, 1);
	'bclr' -> bclr_lk(Opnds, 0);
	'bclrl' -> bclr_lk(Opnds, 1);
	'crand' -> crand(Opnds);
	'crandc' -> crandc(Opnds);
	'creqv' -> creqv(Opnds);
	'crnand' -> crnand(Opnds);
	'crnor' -> crnor(Opnds);
	'cror' -> cror(Opnds);
	'crorc' -> crorc(Opnds);
	'crxor' -> crxor(Opnds);
	'isync' -> isync(Opnds);
	'mcrf' -> mcrf(Opnds);
	'rfi' -> rfi(Opnds);
	'rfid' -> rfid(Opnds);
	%% XFX-Form
	'mfspr' -> mfspr(Opnds);
	'mftb' -> mftb(Opnds);
	'mtcrf' -> mtcrf(Opnds);
	'mtspr' -> mtspr(Opnds);
	%% XFL-Form
	'mtfsf' -> mtfsf_Rc(Opnds, 0);
	'mtfsf.' -> mtfsf_Rc(Opnds, 1);
	%% XS-Form
	'sradi' -> sradi_Rc(Opnds, 0);
	'sradi.' -> sradi_Rc(Opnds, 1);
	%% XO-Form
	'add' -> add_OE_Rc(Opnds, 0, 0);
	'add.' -> add_OE_Rc(Opnds, 0, 1);
	'addo' -> add_OE_Rc(Opnds, 1, 0);
	'addo.' -> add_OE_Rc(Opnds, 1, 1);
	'addc' -> addc_OE_Rc(Opnds, 0, 0);
	'addc.' -> addc_OE_Rc(Opnds, 0, 1);
	'addco' -> addc_OE_Rc(Opnds, 1, 0);
	'addco.' -> addc_OE_Rc(Opnds, 1, 1);
	'adde' -> adde_OE_Rc(Opnds, 0, 0);
	'adde.' -> adde_OE_Rc(Opnds, 0, 1);
	'addeo' -> adde_OE_Rc(Opnds, 1, 0);
	'addeo.' -> adde_OE_Rc(Opnds, 1, 1);
	'addme' -> addme_OE_Rc(Opnds, 0, 0);
	'addme.' -> addme_OE_Rc(Opnds, 0, 1);
	'addmeo' -> addme_OE_Rc(Opnds, 1, 0);
	'addmeo.' -> addme_OE_Rc(Opnds, 1, 1);
	'addze' -> addze_OE_Rc(Opnds, 0, 0);
	'addze.' -> addze_OE_Rc(Opnds, 0, 1);
	'addzeo' -> addze_OE_Rc(Opnds, 1, 0);
	'addzeo.' -> addze_OE_Rc(Opnds, 1, 1);
	'divd' -> divd_OE_Rc(Opnds, 0, 0);
	'divd.' -> divd_OE_Rc(Opnds, 0, 1);
	'divdo' -> divd_OE_Rc(Opnds, 1, 0);
	'divdo.' -> divd_OE_Rc(Opnds, 1, 1);
	'divdu' -> divdu_OE_Rc(Opnds, 0, 0);
	'divdu.' -> divdu_OE_Rc(Opnds, 0, 1);
	'divduo' -> divdu_OE_Rc(Opnds, 1, 0);
	'divduo.' -> divdu_OE_Rc(Opnds, 1, 1);
	'divw' -> divw_OE_Rc(Opnds, 0, 0);
	'divw.' -> divw_OE_Rc(Opnds, 0, 1);
	'divwo' -> divw_OE_Rc(Opnds, 1, 0);
	'divwo.' -> divw_OE_Rc(Opnds, 1, 1);
	'divwu' -> divwu_OE_Rc(Opnds, 0, 0);
	'divwu.' -> divwu_OE_Rc(Opnds, 0, 1);
	'divwuo' -> divwu_OE_Rc(Opnds, 1, 0);
	'divwuo.' -> divwu_OE_Rc(Opnds, 1, 1);
	'mulhd' -> mulhd_Rc(Opnds, 0);
	'mulhd.' -> mulhd_Rc(Opnds, 1);
	'mulhdu' -> mulhdu_Rc(Opnds, 0);
	'mulhdu.' -> mulhdu_Rc(Opnds, 1);
	'mulhw' -> mulhw_Rc(Opnds, 0);
	'mulhw.' -> mulhw_Rc(Opnds, 1);
	'mulhwu' -> mulhwu_Rc(Opnds, 0);
	'mulhwu.' -> mulhwu_Rc(Opnds, 1);
	'mulld' -> mulld_OE_Rc(Opnds, 0, 0);
	'mulld.' -> mulld_OE_Rc(Opnds, 0, 1);
	'mulldo' -> mulld_OE_Rc(Opnds, 1, 0);
	'mulldo.' -> mulld_OE_Rc(Opnds, 1, 1);
	'mullw' -> mullw_OE_Rc(Opnds, 0, 0);
	'mullw.' -> mullw_OE_Rc(Opnds, 0, 1);
	'mullwo' -> mullw_OE_Rc(Opnds, 1, 0);
	'mullwo.' -> mullw_OE_Rc(Opnds, 1, 1);
	'neg' -> neg_OE_Rc(Opnds, 0, 0);
	'neg.' -> neg_OE_Rc(Opnds, 0, 1);
	'nego' -> neg_OE_Rc(Opnds, 1, 0);
	'nego.' -> neg_OE_Rc(Opnds, 1, 1);
	'subf' -> subf_OE_Rc(Opnds, 0, 0);
	'subf.' -> subf_OE_Rc(Opnds, 0, 1);
	'subfo' -> subf_OE_Rc(Opnds, 1, 0);
	'subfo.' -> subf_OE_Rc(Opnds, 1, 1);
	'subfc' -> subfc_OE_Rc(Opnds, 0, 0);
	'subfc.' -> subfc_OE_Rc(Opnds, 0, 1);
	'subfco' -> subfc_OE_Rc(Opnds, 1, 0);
	'subfco.' -> subfc_OE_Rc(Opnds, 1, 1);
	'subfe' -> subfe_OE_Rc(Opnds, 0, 0);
	'subfe.' -> subfe_OE_Rc(Opnds, 0, 1);
	'subfeo' -> subfe_OE_Rc(Opnds, 1, 0);
	'subfeo.' -> subfe_OE_Rc(Opnds, 1, 1);
	'subfme' -> subfme_OE_Rc(Opnds, 0, 0);
	'subfme.' -> subfme_OE_Rc(Opnds, 0, 1);
	'subfmeo' -> subfme_OE_Rc(Opnds, 1, 0);
	'subfmeo.' -> subfme_OE_Rc(Opnds, 1, 1);
	'subfze' -> subfze_OE_Rc(Opnds, 0, 0);
	'subfze.' -> subfze_OE_Rc(Opnds, 0, 1);
	'subfzeo' -> subfze_OE_Rc(Opnds, 1, 0);
	'subfzeo.' -> subfze_OE_Rc(Opnds, 1, 1);
	%% A-Form
	'fadd' -> fadd_Rc(Opnds, 0);
	'fadd.' -> fadd_Rc(Opnds, 1);
	'fadds' -> fadds_Rc(Opnds, 0);
	'fadds.' -> fadds_Rc(Opnds, 1);
	'fdiv' -> fdiv_Rc(Opnds, 0);
	'fdiv.' -> fdiv_Rc(Opnds, 1);
	'fdivs' -> fdivs_Rc(Opnds, 0);
	'fdivs.' -> fdivs_Rc(Opnds, 1);
	'fmadd' -> fmadd_Rc(Opnds, 0);
	'fmadd.' -> fmadd_Rc(Opnds, 1);
	'fmadds' -> fmadds_Rc(Opnds, 0);
	'fmadds.' -> fmadds_Rc(Opnds, 1);
	'fmsub' -> fmsub_Rc(Opnds, 0);
	'fmsub.' -> fmsub_Rc(Opnds, 1);
	'fmsubs' -> fmsubs_Rc(Opnds, 0);
	'fmsubs.' -> fmsubs_Rc(Opnds, 1);
	'fmul' -> fmul_Rc(Opnds, 0);
	'fmul.' -> fmul_Rc(Opnds, 1);
	'fmuls' -> fmuls_Rc(Opnds, 0);
	'fmuls.' -> fmuls_Rc(Opnds, 1);
	'fnmadd' -> fnmadd_Rc(Opnds, 0);
	'fnmadd.' -> fnmadd_Rc(Opnds, 1);
	'fnmadds' -> fnmadds_Rc(Opnds, 0);
	'fnmadds.' -> fnmadds_Rc(Opnds, 1);
	'fnmsub' -> fnmsub_Rc(Opnds, 0);
	'fnmsub.' -> fnmsub_Rc(Opnds, 1);
	'fnmsubs' -> fnmsubs_Rc(Opnds, 0);
	'fnmsubs.' -> fnmsubs_Rc(Opnds, 1);
	'fres' -> fres_Rc(Opnds, 0);
	'fres.' -> fres_Rc(Opnds, 1);
	'frsqrte' -> frsqrte_Rc(Opnds, 0);
	'frsqrte.' -> frsqrte_Rc(Opnds, 1);
	'fsel' -> fsel_Rc(Opnds, 0);
	'fsel.' -> fsel_Rc(Opnds, 1);
	'fsqrt' -> fsqrt_Rc(Opnds, 0);		% not implemented in MPC603e or MPC7450
	'fsqrt.' -> fsqrt_Rc(Opnds, 1);		% not implemented in MPC603e or MPC7450
	'fsqrts' -> fsqrts_Rc(Opnds, 0);	% not implemented in MPC603e or MPC7450
	'fsqrts.' -> fsqrts_Rc(Opnds, 1);	% not implemented in MPC603e or MPC7450
	'fsub' -> fsub_Rc(Opnds, 0);
	'fsub.' -> fsub_Rc(Opnds, 1);
	'fsubs' -> fsubs_Rc(Opnds, 0);
	'fsubs.' -> fsubs_Rc(Opnds, 1);
	%% M-Form
	'rlwimi' -> rlwimi_Rc(Opnds, 0);
	'rlwimi.' -> rlwimi_Rc(Opnds, 1);
	'rlwinm' -> rlwinm_Rc(Opnds, 0);
	'rlwinm.' -> rlwinm_Rc(Opnds, 1);
	'rlwnm' -> rlwnm_Rc(Opnds, 0);
	'rlwnm.' -> rlwnm_Rc(Opnds, 1);
	%% MD-Form
	'rldic' -> rldic_Rc(Opnds, 0);
	'rldic.' -> rldic_Rc(Opnds, 1);
	'rldicl' -> rldicl_Rc(Opnds, 0);
	'rldicl.' -> rldicl_Rc(Opnds, 1);
	'rldicr' -> rldicr_Rc(Opnds, 0);
	'rldicr.' -> rldicr_Rc(Opnds, 1);
	'rldimi' -> rldimi_Rc(Opnds, 0);
	'rldimi.' -> rldimi_Rc(Opnds, 1);
	%% MDS-Form
	'rldcl' -> rldcl(Opnds, 0);
	'rldcl.' -> rldcl(Opnds, 1);
	'rldcr' -> rldcr(Opnds, 0);
	'rldcr.' -> rldcr(Opnds, 1);
	_ -> exit({?MODULE,insn_encode,Op})
    end.

%%% testing interface

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
    R14 = {r,14},
    R10 = {r,10},
    R11 = {r,11},
    F2 = {fr,2},
    F4 = {fr,4},
    F6 = {fr,6},
    F8 = {fr,8},
    DispM3 = {d,16#FFFD},
    DS = {ds,16#FFFD bsr 2},
    SIMM99 = {simm,10#99},
    UIMM4711 = {uimm,10#4711},
    TO_LLE = {to, 2#00110},	% =, <U
    CR7 = {crf,7},
    CR5 = {crf,5},
    CRB_CR0_LT = {crb,0},
    CRB_CR7_SO = {crb,31},
    CRB_CR1_GT = {crb,5},
    CRM192 = {crm,192},
    FM255 = {fm,16#FF},		% all fields
    CRIMM15 = {crimm,16#F},
    TBR268 = {tbr, 10#268},	% TBL
    SPR9 = {spr, 10#9},		% CTR
    SR9 = {sr,9},
    NB7 = {nb,7},
    SH16 = {sh,16},
    SH45 = {sh6,45},
    MB10 = {mb,10},
    MB40 = {mb6,40},
    ME20 = {me,20},
    ME50 = {me6,50},
    LI = {li,16#ffffff},
    BD = {bd,16#3ff},
    BO_NZ_PLUS = {bo,2#01101},	% branch if cond true, predict taken
    BI_CR0_EQ = {bi,2#00010},	% CR0[2], Zero
    %% I-Form
    t(OS,'b',{LI}),
    t(OS,'ba',{LI}),
    t(OS,'bl',{LI}),
    t(OS,'bla',{LI}),
    %% B-Form
    t(OS,'bc',{BO_NZ_PLUS,BI_CR0_EQ,BD}),
    t(OS,'bca',{BO_NZ_PLUS,BI_CR0_EQ,BD}),
    t(OS,'bcl',{BO_NZ_PLUS,BI_CR0_EQ,BD}),
    t(OS,'bcla',{BO_NZ_PLUS,BI_CR0_EQ,BD}),
    %% SC-Form
    t(OS,'sc',{}),
    %% D-Form
    t(OS,'addi',{R14,R10,SIMM99}),
    t(OS,'addic',{R14,R10,SIMM99}),
    t(OS,'addic.',{R14,R10,SIMM99}),
    t(OS,'addis',{R14,R10,SIMM99}),
    t(OS,'andi.',{R14,R10,UIMM4711}),
    t(OS,'andis.',{R14,R10,UIMM4711}),
    t(OS,'cmpi',{CR7,0,R10,SIMM99}),
    t(OS,'cmpi',{CR7,1,R10,SIMM99}),
    t(OS,'cmpli',{CR7,0,R10,UIMM4711}),
    t(OS,'cmpli',{CR7,1,R10,UIMM4711}),
    t(OS,'lbz',{R14,DispM3,R10}),
    t(OS,'lbzu',{R14,DispM3,R10}),
    t(OS,'lfd',{F2,DispM3,R10}),
    t(OS,'lfdu',{F2,DispM3,R10}),
    t(OS,'lfs',{F2,DispM3,R10}),
    t(OS,'lfsu',{F2,DispM3,R10}),
    t(OS,'lha',{R14,DispM3,R10}),
    t(OS,'lhau',{R14,DispM3,R10}),
    t(OS,'lhz',{R14,DispM3,R10}),
    t(OS,'lhzu',{R14,DispM3,R10}),
    t(OS,'lmw',{R14,DispM3,R10}),
    t(OS,'lwz',{R14,DispM3,R10}),
    t(OS,'lwzu',{R14,DispM3,R10}),
    t(OS,'mulli',{R14,R10,SIMM99}),
    t(OS,'ori',{R14,R10,UIMM4711}),
    t(OS,'oris',{R14,R10,UIMM4711}),
    t(OS,'stb',{R14,DispM3,R10}),
    t(OS,'stbu',{R14,DispM3,R10}),
    t(OS,'stfd',{F2,DispM3,R10}),
    t(OS,'stfdu',{F2,DispM3,R10}),
    t(OS,'stfs',{F2,DispM3,R10}),
    t(OS,'stfsu',{F2,DispM3,R10}),
    t(OS,'sth',{R14,DispM3,R10}),
    t(OS,'sthu',{R14,DispM3,R10}),
    t(OS,'stmw',{R14,DispM3,R10}),
    t(OS,'stw',{R14,DispM3,R10}),
    t(OS,'stwu',{R14,DispM3,R10}),
    t(OS,'subfic',{R14,R10,SIMM99}),
    t(OS,'tdi',{TO_LLE,R10,SIMM99}),
    t(OS,'twi',{TO_LLE,R10,SIMM99}),
    t(OS,'xori',{R14,R10,UIMM4711}),
    t(OS,'xoris',{R14,R10,UIMM4711}),
    %% DS-Form
    t(OS,'ld',{R14,DS,R10}),
    t(OS,'ldu',{R14,DS,R10}),
    t(OS,'lwa',{R14,DS,R10}),
    t(OS,'std',{R14,DS,R10}),
    t(OS,'stdu',{R14,DS,R10}),
    %% X-Form
    t(OS,'and',{R14,R10,R11}),
    t(OS,'and.',{R14,R10,R11}),
    t(OS,'andc',{R14,R10,R11}),
    t(OS,'andc.',{R14,R10,R11}),
    t(OS,'cmp',{CR7,0,R10,R11}),
    t(OS,'cmp',{CR7,1,R10,R11}),
    t(OS,'cmpl',{CR7,0,R10,R11}),
    t(OS,'cmpl',{CR7,1,R10,R11}),
    t(OS,'cntlzd',{R14,R10}),
    t(OS,'cntlzd.',{R14,R10}),
    t(OS,'cntlzw',{R14,R10}),
    t(OS,'cntlzw.',{R14,R10}),
    t(OS,'dcba',{R10,R11}),
    t(OS,'dcbf',{R10,R11}),
    t(OS,'dcbi',{R10,R11}),
    t(OS,'dcbst',{R10,R11}),
    t(OS,'dcbt',{R10,R11}),
    t(OS,'dcbtst',{R10,R11}),
    t(OS,'dcbz',{R10,R11}),
    t(OS,'eciwx',{R14,R10,R11}),
    t(OS,'ecowx',{R14,R10,R11}),
    t(OS,'eieio',{}),
    t(OS,'eqv',{R14,R10,R11}),
    t(OS,'eqv.',{R14,R10,R11}),
    t(OS,'extsb',{R14,R10}),
    t(OS,'extsb.',{R14,R10}),
    t(OS,'extsh',{R14,R10}),
    t(OS,'extsh.',{R14,R10}),
    t(OS,'extsw',{R14,R10}),
    t(OS,'extsw.',{R14,R10}),
    t(OS,'fabs',{F2,F8}),
    t(OS,'fabs.',{F2,F8}),
    t(OS,'fcfid',{F2,F8}),
    t(OS,'fcfid.',{F2,F8}),
    t(OS,'fcmpo',{CR7,F4,F8}),
    t(OS,'fcmpu',{CR7,F4,F8}),
    t(OS,'fctid',{F2,F8}),
    t(OS,'fctid.',{F2,F8}),
    t(OS,'fctidz',{F2,F8}),
    t(OS,'fctidz.',{F2,F8}),
    t(OS,'fctiw',{F2,F8}),
    t(OS,'fctiw.',{F2,F8}),
    t(OS,'fctiwz',{F2,F8}),
    t(OS,'fctiwz.',{F2,F8}),
    t(OS,'fmr',{F2,F8}),
    t(OS,'fmr.',{F2,F8}),
    t(OS,'fnabs',{F2,F8}),
    t(OS,'fnabs.',{F2,F8}),
    t(OS,'fneg',{F2,F8}),
    t(OS,'fneg.',{F2,F8}),
    t(OS,'frsp',{F2,F8}),
    t(OS,'frsp.',{F2,F8}),
    t(OS,'icbi',{R10,R11}),
    t(OS,'lbzux',{R14,R10,R11}),
    t(OS,'lbzx',{R14,R10,R11}),
    t(OS,'ldarx',{R14,R10,R11}),
    t(OS,'ldux',{R14,R10,R11}),
    t(OS,'ldx',{R14,R10,R11}),
    t(OS,'lfdux',{F2,R10,R11}),
    t(OS,'lfdx',{F2,R10,R11}),
    t(OS,'lfsux',{F2,R10,R11}),
    t(OS,'lfsx',{F2,R10,R11}),
    t(OS,'lhaux',{R14,R10,R11}),
    t(OS,'lhax',{R14,R10,R11}),
    t(OS,'lhbrx',{R14,R10,R11}),
    t(OS,'lhzux',{R14,R10,R11}),
    t(OS,'lhzx',{R14,R10,R11}),
    t(OS,'lswi',{R14,R10,NB7}),
    t(OS,'lswx',{R14,R10,R11}),
    t(OS,'lwarx',{R14,R10,R11}),
    t(OS,'lwaux',{R14,R10,R11}),
    t(OS,'lwax',{R14,R10,R11}),
    t(OS,'lwbrx',{R14,R10,R11}),
    t(OS,'lwzux',{R14,R10,R11}),
    t(OS,'lwzx',{R14,R10,R11}),
    t(OS,'mcrfs',{CR7,CR5}),
    %% t(OS,'mcrxr',{CR7}),
    t(OS,'mfcr',{R14}),
    t(OS,'mffs',{F2}),
    t(OS,'mffs.',{F2}),
    t(OS,'mfmsr',{R14}),
    t(OS,'mfsr',{R14,SR9}),
    t(OS,'mfsrin',{R14,R11}),
    t(OS,'mtfsb0',{CRB_CR0_LT}),
    t(OS,'mtfsb0.',{CRB_CR0_LT}),
    t(OS,'mtfsb1',{CRB_CR0_LT}),
    t(OS,'mtfsb1.',{CRB_CR0_LT}),
    t(OS,'mtfsfi',{CR7,CRIMM15}),
    t(OS,'mtfsfi.',{CR7,CRIMM15}),
    t(OS,'mtmsr',{R14}),
    t(OS,'mtmsrd',{R14}),
    t(OS,'mtsr',{SR9,R14}),
    t(OS,'mtsrd',{SR9,R14}),
    t(OS,'mtsrdin',{R14,R11}),
    t(OS,'mtsrin',{R14,R11}),
    t(OS,'nand',{R14,R10,R11}),
    t(OS,'nand.',{R14,R10,R11}),
    t(OS,'nor',{R14,R10,R11}),
    t(OS,'nor.',{R14,R10,R11}),
    t(OS,'or',{R14,R10,R11}),
    t(OS,'or.',{R14,R10,R11}),
    t(OS,'orc',{R14,R10,R11}),
    t(OS,'orc.',{R14,R10,R11}),
    t(OS,'slbia',{}),
    t(OS,'slbie',{R11}),
    t(OS,'sld',{R14,R10,R11}),
    t(OS,'sld.',{R14,R10,R11}),
    t(OS,'slw',{R14,R10,R11}),
    t(OS,'slw.',{R14,R10,R11}),
    t(OS,'srad',{R14,R10,R11}),
    t(OS,'srad.',{R14,R10,R11}),
    t(OS,'sraw',{R14,R10,R11}),
    t(OS,'sraw.',{R14,R10,R11}),
    t(OS,'srawi',{R14,R10,SH16}),
    t(OS,'srawi.',{R14,R10,SH16}),
    t(OS,'srd',{R14,R10,R11}),
    t(OS,'srd.',{R14,R10,R11}),
    t(OS,'srw',{R14,R10,R11}),
    t(OS,'srw.',{R14,R10,R11}),
    t(OS,'stbux',{R14,R10,R11}),
    t(OS,'stbx',{R14,R10,R11}),
    t(OS,'stdcx.',{R14,R10,R11}),
    t(OS,'stdux',{R14,R10,R11}),
    t(OS,'stdx',{R14,R10,R11}),
    t(OS,'stfdux',{F2,R10,R11}),
    t(OS,'stfdx',{F2,R10,R11}),
    t(OS,'stfiwx',{F2,R10,R11}),
    t(OS,'stfsux',{F2,R10,R11}),
    t(OS,'stfsx',{F2,R10,R11}),
    t(OS,'sthbrx',{R14,R10,R11}),
    t(OS,'sthux',{R14,R10,R11}),
    t(OS,'sthx',{R14,R10,R11}),
    t(OS,'stswi',{R14,R10,NB7}),
    t(OS,'stswx',{R14,R10,R11}),
    t(OS,'stwbrx',{R14,R10,R11}),
    t(OS,'stwcx.',{R14,R10,R11}),
    t(OS,'stwux',{R14,R10,R11}),
    t(OS,'stwx',{R14,R10,R11}),
    t(OS,'sync',{}),
    t(OS,'td',{TO_LLE,R10,R11}),
    t(OS,'tlbia',{}),
    t(OS,'tlbie',{R11}),
    t(OS,'tlbld',{R11}),
    t(OS,'tlbli',{R11}),
    t(OS,'tlbsync',{}),
    t(OS,'tw',{TO_LLE,R10,R11}),
    t(OS,'xor',{R14,R10,R11}),
    t(OS,'xor.',{R14,R10,R11}),
    %% XL-Form
    t(OS,'bcctr',{BO_NZ_PLUS,BI_CR0_EQ}),
    t(OS,'bcctrl',{BO_NZ_PLUS,BI_CR0_EQ}),
    t(OS,'bclr',{BO_NZ_PLUS,BI_CR0_EQ}),
    t(OS,'bclrl',{BO_NZ_PLUS,BI_CR0_EQ}),
    t(OS,'crand',{CRB_CR0_LT,CRB_CR7_SO,CRB_CR1_GT}),
    t(OS,'crandc',{CRB_CR0_LT,CRB_CR7_SO,CRB_CR1_GT}),
    t(OS,'creqv',{CRB_CR0_LT,CRB_CR7_SO,CRB_CR1_GT}),
    t(OS,'crnand',{CRB_CR0_LT,CRB_CR7_SO,CRB_CR1_GT}),
    t(OS,'crnor',{CRB_CR0_LT,CRB_CR7_SO,CRB_CR1_GT}),
    t(OS,'cror',{CRB_CR0_LT,CRB_CR7_SO,CRB_CR1_GT}),
    t(OS,'crorc',{CRB_CR0_LT,CRB_CR7_SO,CRB_CR1_GT}),
    t(OS,'crxor',{CRB_CR0_LT,CRB_CR7_SO,CRB_CR1_GT}),
    t(OS,'isync',{}),
    t(OS,'mcrf',{CR7,CR5}),
    t(OS,'rfi',{}),
    t(OS,'rfid',{}),
    %% XFX-Form
    t(OS,'mfspr',{R14,SPR9}),
    t(OS,'mftb',{R14,TBR268}),
    t(OS,'mtcrf',{CRM192,R14}),
    t(OS,'mtspr',{SPR9,R14}),
    %% XFL-Form
    t(OS,'mtfsf',{FM255,F8}),
    t(OS,'mtfsf.',{FM255,F8}),
    %% XS-Form
    t(OS,'sradi',{R14,R10,SH45}),
    t(OS,'sradi.',{R14,R10,SH45}),
    %% XO-Form
    t(OS,'add',{R14,R10,R11}),
    t(OS,'add.',{R14,R10,R11}),
    t(OS,'addo',{R14,R10,R11}),
    t(OS,'addo.',{R14,R10,R11}),
    t(OS,'addc',{R14,R10,R11}),
    t(OS,'addc.',{R14,R10,R11}),
    t(OS,'addco',{R14,R10,R11}),
    t(OS,'addco.',{R14,R10,R11}),
    t(OS,'adde',{R14,R10,R11}),
    t(OS,'adde.',{R14,R10,R11}),
    t(OS,'addeo',{R14,R10,R11}),
    t(OS,'addeo.',{R14,R10,R11}),
    t(OS,'addme',{R14,R10}),
    t(OS,'addme.',{R14,R10}),
    t(OS,'addmeo',{R14,R10}),
    t(OS,'addmeo.',{R14,R10}),
    t(OS,'addze',{R14,R10}),
    t(OS,'addze.',{R14,R10}),
    t(OS,'addzeo',{R14,R10}),
    t(OS,'addzeo.',{R14,R10}),
    t(OS,'divd',{R14,R10,R11}),
    t(OS,'divd.',{R14,R10,R11}),
    t(OS,'divdo',{R14,R10,R11}),
    t(OS,'divdo.',{R14,R10,R11}),
    t(OS,'divdu',{R14,R10,R11}),
    t(OS,'divdu.',{R14,R10,R11}),
    t(OS,'divduo',{R14,R10,R11}),
    t(OS,'divduo.',{R14,R10,R11}),
    t(OS,'divw',{R14,R10,R11}),
    t(OS,'divw.',{R14,R10,R11}),
    t(OS,'divwo',{R14,R10,R11}),
    t(OS,'divwo.',{R14,R10,R11}),
    t(OS,'divwu',{R14,R10,R11}),
    t(OS,'divwu.',{R14,R10,R11}),
    t(OS,'divwuo',{R14,R10,R11}),
    t(OS,'divwuo.',{R14,R10,R11}),
    t(OS,'mulhd',{R14,R10,R11}),
    t(OS,'mulhd.',{R14,R10,R11}),
    t(OS,'mulhdu',{R14,R10,R11}),
    t(OS,'mulhdu.',{R14,R10,R11}),
    t(OS,'mulhw',{R14,R10,R11}),
    t(OS,'mulhw.',{R14,R10,R11}),
    t(OS,'mulhwu',{R14,R10,R11}),
    t(OS,'mulhwu.',{R14,R10,R11}),
    t(OS,'mulld',{R14,R10,R11}),
    t(OS,'mulld.',{R14,R10,R11}),
    t(OS,'mulldo',{R14,R10,R11}),
    t(OS,'mulldo.',{R14,R10,R11}),
    t(OS,'mullw',{R14,R10,R11}),
    t(OS,'mullw.',{R14,R10,R11}),
    t(OS,'mullwo',{R14,R10,R11}),
    t(OS,'mullwo.',{R14,R10,R11}),
    t(OS,'neg',{R14,R10}),
    t(OS,'neg.',{R14,R10}),
    t(OS,'nego',{R14,R10}),
    t(OS,'nego.',{R14,R10}),
    t(OS,'subf',{R14,R10,R11}),
    t(OS,'subf.',{R14,R10,R11}),
    t(OS,'subfo',{R14,R10,R11}),
    t(OS,'subfo.',{R14,R10,R11}),
    t(OS,'subfc',{R14,R10,R11}),
    t(OS,'subfc.',{R14,R10,R11}),
    t(OS,'subfco',{R14,R10,R11}),
    t(OS,'subfco.',{R14,R10,R11}),
    t(OS,'subfe',{R14,R10,R11}),
    t(OS,'subfe.',{R14,R10,R11}),
    t(OS,'subfeo',{R14,R10,R11}),
    t(OS,'subfeo.',{R14,R10,R11}),
    t(OS,'subfme',{R14,R10}),
    t(OS,'subfme.',{R14,R10}),
    t(OS,'subfmeo',{R14,R10}),
    t(OS,'subfmeo.',{R14,R10}),
    t(OS,'subfze',{R14,R10}),
    t(OS,'subfze.',{R14,R10}),
    t(OS,'subfzeo',{R14,R10}),
    t(OS,'subfzeo.',{R14,R10}),
    %% A-Form
    t(OS,'fadd',{F2,F4,F8}),
    t(OS,'fadd.',{F2,F4,F8}),
    t(OS,'fadds',{F2,F4,F8}),
    t(OS,'fadds.',{F2,F4,F8}),
    t(OS,'fdiv',{F2,F4,F8}),
    t(OS,'fdiv.',{F2,F4,F8}),
    t(OS,'fdivs',{F2,F4,F8}),
    t(OS,'fdivs.',{F2,F4,F8}),
    t(OS,'fmadd',{F2,F4,F6,F8}),
    t(OS,'fmadd.',{F2,F4,F6,F8}),
    t(OS,'fmadds',{F2,F4,F6,F8}),
    t(OS,'fmadds.',{F2,F4,F6,F8}),
    t(OS,'fmsub',{F2,F4,F6,F8}),
    t(OS,'fmsub.',{F2,F4,F6,F8}),
    t(OS,'fmsubs',{F2,F4,F6,F8}),
    t(OS,'fmsubs.',{F2,F4,F6,F8}),
    t(OS,'fmul',{F2,F4,F6}),
    t(OS,'fmul.',{F2,F4,F6}),
    t(OS,'fmuls',{F2,F4,F6}),
    t(OS,'fmuls.',{F2,F4,F6}),
    t(OS,'fnmadd',{F2,F4,F6,F8}),
    t(OS,'fnmadd.',{F2,F4,F6,F8}),
    t(OS,'fnmadds',{F2,F4,F6,F8}),
    t(OS,'fnmadds.',{F2,F4,F6,F8}),
    t(OS,'fnmsub',{F2,F4,F6,F8}),
    t(OS,'fnmsub.',{F2,F4,F6,F8}),
    t(OS,'fnmsubs',{F2,F4,F6,F8}),
    t(OS,'fnmsubs.',{F2,F4,F6,F8}),
    t(OS,'fres',{F2,F8}),
    t(OS,'fres.',{F2,F8}),
    t(OS,'frsqrte',{F2,F8}),
    t(OS,'frsqrte.',{F2,F8}),
    t(OS,'fsel',{F2,F4,F6,F8}),
    t(OS,'fsel.',{F2,F4,F6,F8}),
    t(OS,'fsqrt',{F2,F8}),
    t(OS,'fsqrt.',{F2,F8}),
    t(OS,'fsqrts',{F2,F8}),
    t(OS,'fsqrts.',{F2,F8}),
    t(OS,'fsub',{F2,F4,F8}),
    t(OS,'fsub.',{F2,F4,F8}),
    t(OS,'fsubs',{F2,F4,F8}),
    t(OS,'fsubs.',{F2,F4,F8}),
    %% M-Form
    t(OS,'rlwimi',{R14,R10,SH16,MB10,ME20}),
    t(OS,'rlwimi.',{R14,R10,SH16,MB10,ME20}),
    t(OS,'rlwinm',{R14,R10,SH16,MB10,ME20}),
    t(OS,'rlwinm.',{R14,R10,SH16,MB10,ME20}),
    t(OS,'rlwnm',{R14,R10,R11,MB10,ME20}),
    t(OS,'rlwnm.',{R14,R10,R11,MB10,ME20}),
    %% MD-Form
    t(OS,'rldic',{R14,R10,SH45,MB40}),
    t(OS,'rldic.',{R14,R10,SH45,MB40}),
    t(OS,'rldicl',{R14,R10,SH45,MB40}),
    t(OS,'rldicl.',{R14,R10,SH45,MB40}),
    t(OS,'rldicr',{R14,R10,SH45,ME50}),
    t(OS,'rldicr.',{R14,R10,SH45,ME50}),
    t(OS,'rldimi',{R14,R10,SH45,MB40}),
    t(OS,'rldimi.',{R14,R10,SH45,MB40}),
    %% MDS-Form
    t(OS,'rldcl',{R14,R10,R11,MB40}),
    t(OS,'rldcl.',{R14,R10,R11,MB40}),
    t(OS,'rldcr',{R14,R10,R11,ME50}),
    t(OS,'rldcr.',{R14,R10,R11,ME50}),
    [].

dotest() -> dotest1(group_leader()).

dotest(File) ->
    {ok,OS} = file:open(File, [write]),
    dotest1(OS),
    file:close(OS).

-endif.
