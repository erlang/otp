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
%%% Copyright (C) 2000-2004 Mikael Pettersson
%%% Copyright (C) 2004 Daniel Luna
%%%
%%% This is the syntax of amd64 r/m operands:
%%%
%%% opnd  ::= reg			mod == 11
%%%	    | MEM[ea]			mod != 11
%%%
%%% ea    ::= disp32(reg)		mod == 10, r/m != ESP
%%%  	    | disp32 sib12		mod == 10, r/m == 100
%%%	    | disp8(reg)		mod == 01, r/m != ESP
%%%	    | disp8 sib12		mod == 01, r/m == 100
%%%	    | (reg)			mod == 00, r/m != ESP and EBP
%%%	    | sib0			mod == 00, r/m == 100
%%%	    | disp32(%rip)		mod == 00, r/m == 101
%%%
%%% // sib0: mod == 00
%%% sib0  ::= disp32(,index,scale)	base == EBP, index != ESP
%%%	    | disp32			base == EBP, index == 100
%%%	    | (base,index,scale)	base != EBP, index != ESP
%%%	    | (base)			base != EBP, index == 100
%%%
%%% // sib12: mod == 01 or 10
%%% sib12  ::= (base,index,scale)	index != ESP
%%%	    | (base)			index == 100
%%%
%%% scale ::= 00 | 01 | 10 | 11		index << scale
%%%
%%% Notes:
%%%
%%% 1. ESP cannot be used as index register.
%%% 2. Use of ESP as base register requires a SIB byte.
%%% 3. disp(reg), when reg != ESP, can be represented without
%%%    [r/m == reg] or with [r/m == 100, base == reg] a SIB byte.
%%% 4. disp32 can be represented without [mod == 00, r/m == 101]
%%%    or with [mod == 00, r/m == 100, base == 101, index == 100]
%%%    a SIB byte.
%%% 5. AMD64 and x86 interpret mod==00b r/m==101b EAs differently:
%%%    on x86 the disp32 is an absolute address, but on AMD64 the
%%%    disp32 is relative to the %rip of the next instruction.

-module(hipe_amd64_encode).

-export([% condition codes
	 cc/1,
	 % 8-bit registers
	 %% al/0, cl/0, dl/0, bl/0,
	 % 32-bit registers
	 %% eax/0, ecx/0, edx/0, ebx/0, esp/0, ebp/0, esi/0, edi/0,
	 % operands
	 sindex/2, sib/1, sib/2,
	 ea_disp32_base/2, ea_disp32_sib/2,
	 ea_disp8_base/2, ea_disp8_sib/2,
	 ea_base/1,
	 ea_disp32_sindex/1, %%ea_disp32_sindex/2,
	 ea_sib/1, %ea_disp32_rip/1,
	 rm_reg/1, rm_mem/1,
	 % instructions
	 insn_encode/3, insn_sizeof/2]).

%%-define(DO_HIPE_AMD64_ENCODE_TEST,true).
-ifdef(DO_HIPE_AMD64_ENCODE_TEST).
-export([dotest/0, dotest/1]).	% for testing, don't use
-endif.

-define(ASSERT(F,G), if G -> [] ; true -> exit({?MODULE,F}) end).
%-define(ASSERT(F,G), []).

%%% condition codes

-define(CC_O,  2#0000).	% overflow
-define(CC_NO, 2#0001).	% no overflow
-define(CC_B,  2#0010).	% below, <u
-define(CC_AE, 2#0011).	% above or equal, >=u
-define(CC_E,  2#0100).	% equal
-define(CC_NE, 2#0101).	% not equal
-define(CC_BE, 2#0110).	% below or equal, <=u
-define(CC_A,  2#0111).	% above, >u
-define(CC_S,  2#1000).	% sign, +
-define(CC_NS, 2#1001).	% not sign, -
-define(CC_PE, 2#1010).	% parity even
-define(CC_PO, 2#1011).	% parity odd
-define(CC_L,  2#1100).	% less than, <s
-define(CC_GE, 2#1101).	% greater or equal, >=s
-define(CC_LE, 2#1110).	% less or equal, <=s
-define(CC_G,  2#1111).	% greater than, >s

cc(o) -> ?CC_O;
cc(no) -> ?CC_NO;
cc(b) -> ?CC_B;
cc(ae) -> ?CC_AE;
cc(e) -> ?CC_E;
cc(ne) -> ?CC_NE;
cc(be) -> ?CC_BE;
cc(a) -> ?CC_A;
cc(s) -> ?CC_S;
cc(ns) -> ?CC_NS;
cc(pe) -> ?CC_PE;
cc(po) -> ?CC_PO;
cc(l) -> ?CC_L;
cc(ge) -> ?CC_GE;
cc(le) -> ?CC_LE;
cc(g) -> ?CC_G.

%%% 8-bit registers

-define(AL, 2#000).
-define(CL, 2#001).
-define(DL, 2#010).
-define(BL, 2#011).
-define(SPL, 2#100).
-define(BPL, 2#101).
-define(SIL, 2#110).
-define(DIL, 2#111).

%% al() -> ?AL.
%% cl() -> ?CL.
%% dl() -> ?DL.
%% bl() -> ?BL.

%%% 32-bit registers

-define(EAX, 2#000).
-define(ECX, 2#001).
-define(EDX, 2#010).
-define(EBX, 2#011).
-define(ESP, 2#100).
-define(EBP, 2#101).
-define(ESI, 2#110).
-define(EDI, 2#111).

%% eax() -> ?EAX.
%% ecx() -> ?ECX.
%% edx() -> ?EDX.
%% ebx() -> ?EBX.
%% esp() -> ?ESP.
%% ebp() -> ?EBP.
%% esi() -> ?ESI.
%% edi() -> ?EDI.

%%% r/m operands

sindex(Scale, Index) when is_integer(Scale), is_integer(Index) ->
    ?ASSERT(sindex, Scale >= 0),
    ?ASSERT(sindex, Scale =< 3),
    ?ASSERT(sindex, Index =/= ?ESP),
    {sindex, Scale, Index}.

-record(sib, {sindex_opt, base :: integer()}).
sib(Base) when is_integer(Base) -> #sib{sindex_opt=none, base=Base}.
sib(Base, Sindex) when is_integer(Base) -> #sib{sindex_opt=Sindex, base=Base}.

ea_disp32_base(Disp32, Base) when is_integer(Base) ->
    ?ASSERT(ea_disp32_base, Base =/= ?ESP),
    {ea_disp32_base, Disp32, Base}.
ea_disp32_sib(Disp32, SIB) -> {ea_disp32_sib, Disp32, SIB}.
ea_disp8_base(Disp8, Base) when is_integer(Base) ->
    ?ASSERT(ea_disp8_base, Base =/= ?ESP),
    {ea_disp8_base, Disp8, Base}.
ea_disp8_sib(Disp8, SIB) -> {ea_disp8_sib, Disp8, SIB}.
ea_base(Base) when is_integer(Base) ->
    ?ASSERT(ea_base, Base =/= ?ESP),
    ?ASSERT(ea_base, Base =/= ?EBP),
    {ea_base, Base}.
ea_disp32_sindex(Disp32) -> {ea_disp32_sindex, Disp32, none}.
%% ea_disp32_sindex(Disp32, Sindex) -> {ea_disp32_sindex, Disp32, Sindex}.
ea_sib(SIB) ->
    ?ASSERT(ea_sib, SIB#sib.base =/= ?EBP),
    {ea_sib, SIB}.
%ea_disp32_rip(Disp32) -> {ea_disp32_rip, Disp32}.

rm_reg(Reg) -> {rm_reg, Reg}.
rm_mem(EA) -> {rm_mem, EA}.

mk_modrm(Mod, RO, RM) ->
    {rex([{r,RO}, {b,RM}]),
     (Mod bsl 6) bor ((RO band 2#111) bsl 3) bor (RM band 2#111)}.

mk_sib(Scale, Index, Base) ->
    {rex([{x,Index}, {b,Base}]),
     (Scale bsl 6) bor ((Index band 2#111) bsl 3) bor (Base band 2#111)}.

rex(REXs) -> {rex, rex_(REXs)}.
rex_([]) -> 0;
rex_([{r8, Reg8}| Rest]) ->             % 8 bit registers
    case Reg8 of
	{rm_mem, _} -> rex_(Rest);
	{rm_reg, R} -> rex_([{r8, R} | Rest]);
	4 -> (1 bsl 8) bor rex_(Rest);
	5 -> (1 bsl 8) bor rex_(Rest);
	6 -> (1 bsl 8) bor rex_(Rest);
	7 -> (1 bsl 8) bor rex_(Rest);
	X when is_integer(X) -> rex_(Rest)
    end;
rex_([{w, REXW}| Rest]) ->              % 64-bit mode
    (REXW bsl 3) bor rex_(Rest);
rex_([{r, ModRM_regRegister}| Rest]) when is_integer(ModRM_regRegister) ->
    REXR = if (ModRM_regRegister > 7) -> 1;
              true -> 0
           end,
    (REXR bsl 2) bor rex_(Rest);
rex_([{x, SIB_indexRegister}| Rest]) when is_integer(SIB_indexRegister) ->
    REXX = if (SIB_indexRegister > 7) -> 1;
              true -> 0
           end,
    (REXX bsl 1) bor rex_(Rest);
rex_([{b, OtherRegister}| Rest]) when is_integer(OtherRegister) ->
    %% ModRM r/m, SIB base or opcode reg
    REXB = if (OtherRegister > 7) -> 1;
              true -> 0
           end,
    REXB bor rex_(Rest).

le16(Word, Tail) ->
    [Word band 16#FF, (Word bsr 8) band 16#FF | Tail].

le32(Word, Tail) when is_integer(Word) ->
    [Word band 16#FF, (Word bsr 8) band 16#FF,
     (Word bsr 16) band 16#FF, (Word bsr 24) band 16#FF | Tail];
le32({Tag,Val}, Tail) ->	% a relocatable datum
    [{le32,Tag,Val} | Tail].

le64(Word, Tail) when is_integer(Word) ->
     [ Word         band 16#FF, (Word bsr  8) band 16#FF,
      (Word bsr 16) band 16#FF, (Word bsr 24) band 16#FF,
      (Word bsr 32) band 16#FF, (Word bsr 40) band 16#FF,
      (Word bsr 48) band 16#FF, (Word bsr 56) band 16#FF | Tail];
le64({Tag,Val}, Tail) ->
    [{le64,Tag,Val} | Tail].

enc_sindex_opt({sindex,Scale,Index}) -> {Scale, Index};
enc_sindex_opt(none) -> {2#00, 2#100}.

enc_sib(#sib{sindex_opt=SindexOpt, base=Base}) ->
    {Scale, Index} = enc_sindex_opt(SindexOpt),
    mk_sib(Scale, Index, Base).

enc_ea(EA, RO, Tail) ->
    case EA of
	{ea_disp32_base, Disp32, Base} ->
	    [mk_modrm(2#10, RO, Base) | le32(Disp32, Tail)];
	{ea_disp32_sib, Disp32, SIB} ->
	    [mk_modrm(2#10, RO, 2#100), enc_sib(SIB) | le32(Disp32, Tail)];
	{ea_disp8_base, Disp8, Base} ->
	    [mk_modrm(2#01, RO, Base), Disp8 | Tail];
	{ea_disp8_sib, Disp8, SIB} ->
	    [mk_modrm(2#01, RO, 2#100), enc_sib(SIB), Disp8 | Tail];
	{ea_base, Base} ->
	    [mk_modrm(2#00, RO, Base) | Tail];
	{ea_disp32_sindex, Disp32, SindexOpt} ->
	    {Scale, Index} = enc_sindex_opt(SindexOpt),
	    SIB = mk_sib(Scale, Index, 2#101),
	    MODRM = mk_modrm(2#00, RO, 2#100),
	    [MODRM, SIB | le32(Disp32, Tail)];
	{ea_sib, SIB} ->
	    [mk_modrm(2#00, RO, 2#100), enc_sib(SIB) | Tail];
	{ea_disp32_rip, Disp32} ->
	    [mk_modrm(2#00, RO, 2#101) | le32(Disp32, Tail)]
    end.

encode_rm(RM, RO, Tail) ->
    case RM of
	{rm_reg, Reg} -> [mk_modrm(2#11, RO, Reg) | Tail];
	{rm_mem, EA} -> enc_ea(EA, RO, Tail)
    end.

%% sizeof_ea(EA) ->
%%     case element(1, EA) of
%% 	ea_disp32_base -> 5;
%% 	ea_disp32_sib -> 6;
%% 	ea_disp8_base -> 2;
%% 	ea_disp8_sib -> 3;
%% 	ea_base -> 1;
%% 	ea_disp32_sindex -> 6;
%% 	ea_sib -> 2;
%% 	ea_disp32_rip -> 5
%%     end.

%% sizeof_rm(RM) ->
%%    case RM of
%%	{rm_reg, _} -> 1;
%%	{rm_mem, EA} -> sizeof_ea(EA)
%%    end.

%%% x87 stack postitions

-define(ST0, 2#000).
-define(ST1, 2#001).
-define(ST2, 2#010).
-define(ST3, 2#011).
-define(ST4, 2#100).
-define(ST5, 2#101).
-define(ST6, 2#110).
-define(ST7, 2#111).

st(0) -> ?ST0;
st(1) -> ?ST1;
st(2) -> ?ST2;
st(3) -> ?ST3;
st(4) -> ?ST4;
st(5) -> ?ST5;
st(6) -> ?ST6;
st(7) -> ?ST7.


%%% Instructions
%%%
%%% Insn	::= {Op,Opnds}
%%% Opnds	::= {Opnd1,...,Opndn}	(n >= 0)
%%% Opnd	::= eax | ax | al | 1 | cl
%%%		  | {imm32,Imm32} | {imm16,Imm16} | {imm8,Imm8}
%%%		  | {rm32,RM32} | {rm16,RM16} | {rm8,RM8}
%%%		  | {rel32,Rel32} | {rel8,Rel8}
%%%		  | {moffs32,Moffs32} | {moffs16,Moffs16} | {moffs8,Moffs8}
%%%		  | {cc,CC}
%%%		  | {reg32,Reg32} | {reg16,Reg16} | {reg8,Reg8}
%%%		  | {ea,EA}

-define(PFX_OPND_16BITS, 16#66).

arith_binop_encode(SubOpcode, Opnds) ->
    %% add, or, adc, sbb, and, sub, xor, cmp
     case Opnds of
         {eax, {imm32,Imm32}} ->
             [16#05 bor (SubOpcode bsl 3) | le32(Imm32, [])];
         {{rm32,RM32}, {imm32,Imm32}} ->
             [16#81 | encode_rm(RM32, SubOpcode, le32(Imm32, []))];
         {{rm32,RM32}, {imm8,Imm8}} ->
             [16#83 | encode_rm(RM32, SubOpcode, [Imm8])];
         {{rm32,RM32}, {reg32,Reg32}} ->
             [16#01 bor (SubOpcode bsl 3) | encode_rm(RM32, Reg32, [])];
         {{reg32,Reg32}, {rm32,RM32}} ->
             [16#03 bor (SubOpcode bsl 3) | encode_rm(RM32, Reg32, [])];
         %% Below starts amd64 stuff with rex prefix
         {rax, {imm32,Imm32}} ->
             [rex([{w,1}]), 16#05 bor (SubOpcode bsl 3) | le32(Imm32, [])];
         {{rm64,RM64}, {imm32,Imm32}} ->
             [rex([{w,1}]), 16#81 
              | encode_rm(RM64, SubOpcode, le32(Imm32, []))];
         {{rm64,RM64}, {imm8,Imm8}} ->
             [rex([{w,1}]), 16#83 | encode_rm(RM64, SubOpcode, [Imm8])];
         {{rm64,RM64}, {reg64,Reg64}} ->
             [rex([{w,1}]), 16#01 bor (SubOpcode bsl 3) 
              | encode_rm(RM64, Reg64, [])];
         {{reg64,Reg64}, {rm64,RM64}} ->
             [rex([{w,1}]), 16#03 bor (SubOpcode bsl 3)
              | encode_rm(RM64, Reg64, [])]
    end.

sse2_arith_binop_encode(Prefix, Opcode, {{xmm, XMM64}, {rm64fp, RM64}}) ->
    %% addpd, cmpsd, divsd, maxsd, minsd, mulsd, sqrtsd, subsd
    [Prefix, 16#0F, Opcode | encode_rm(RM64, XMM64, [])].

sse2_cvtsi2sd_encode({{xmm,XMM64}, {rm64,RM64}}) ->
    [rex([{w, 1}]), 16#F2, 16#0F, 16#2A | encode_rm(RM64, XMM64, [])].

sse2_mov_encode(Opnds) ->
    case Opnds of
        {{xmm, XMM64}, {rm64fp, RM64}} -> % movsd
            [16#F2, 16#0F, 16#10 | encode_rm(RM64, XMM64, [])];
        {{rm64fp, RM64}, {xmm, XMM64}} -> % movsd
            [16#F2, 16#0F, 16#11 | encode_rm(RM64, XMM64, [])]
%        {{xmm, XMM64}, {rm64, RM64}} -> % cvtsi2sd
%            [rex([{w, 1}]), 16#F2, 16#0F, 16#2A | encode_rm(RM64, XMM64, [])]
    end.

%% arith_binop_sizeof(Opnds) ->
%%     %% add, or, adc, sbb, and, sub, xor, cmp
%%    case Opnds of
%%	{eax, {imm32,_}} ->
%%	    1 + 4;
%%	{{rm32,RM32}, {imm32,_}} ->
%%	    1 + sizeof_rm(RM32) + 4;
%%	{{rm32,RM32}, {imm8,_}} ->
%%	    1 + sizeof_rm(RM32) + 1;
%%	{{rm32,RM32}, {reg32,_}} ->
%%	    1 + sizeof_rm(RM32);
%%	{{reg32,_}, {rm32,RM32}} ->
%%	    1 + sizeof_rm(RM32)
%%    end.

bs_op_encode(Opcode, {{reg32,Reg32}, {rm32,RM32}}) ->	% bsf, bsr
    [16#0F, Opcode | encode_rm(RM32, Reg32, [])].

%% bs_op_sizeof({{reg32,_}, {rm32,RM32}}) ->		% bsf, bsr
%%    2 + sizeof_rm(RM32).

bswap_encode(Opnds) ->
    case Opnds of
	{{reg32,Reg32}} ->
	    [rex([{b, Reg32}]), 16#0F, 16#C8 bor (Reg32 band 2#111)];
	{{reg64,Reg64}} ->
	    [rex([{w, 1}, {b, Reg64}]), 16#0F, 16#C8 bor (Reg64 band 2#111)]
    end.	

%% bswap_sizeof({{reg32,_}}) ->
%%    2.

bt_op_encode(SubOpcode, Opnds) ->	% bt, btc, btr, bts
    case Opnds of
	{{rm32,RM32}, {reg32,Reg32}} ->
	    [16#0F, 16#A3 bor (SubOpcode bsl 3) | encode_rm(RM32, Reg32, [])];
	{{rm32,RM32}, {imm8,Imm8}} ->
	    [16#0F, 16#BA | encode_rm(RM32, SubOpcode, [Imm8])]
    end.

%% bt_op_sizeof(Opnds) ->			% bt, btc, btr, bts
%%    case Opnds of
%%	{{rm32,RM32}, {reg32,_}} ->
%%	    2 + sizeof_rm(RM32);
%%	{{rm32,RM32}, {imm8,_}} ->
%%	    2 + sizeof_rm(RM32) + 1
%%    end.

call_encode(Opnds) ->
    case Opnds of
	{{rel32,Rel32}} ->
	    [16#E8 | le32(Rel32, [])];
%%% 	{{rm32,RM32}} ->
%%% 	    [16#FF | encode_rm(RM32, 2#010, [])];
	{{rm64,RM64}} -> % Defaults to 64 bits on amd64
	    [16#FF | encode_rm(RM64, 2#010, [])]
    end.

%% call_sizeof(Opnds) ->
%%    case Opnds of
%%	{{rel32,_}} ->
%%	    1 + 4;
%%	{{rm32,RM32}} ->
%%	    1 + sizeof_rm(RM32)
%%    end.

cbw_encode({}) ->
    [?PFX_OPND_16BITS, 16#98].

cbw_sizeof({}) ->
    2.

nullary_op_encode(Opcode, {}) ->
    %% cdq, clc, cld, cmc, cwde, into, leave, nop, prefix_fs, stc, std
    [Opcode].

nullary_op_sizeof({}) ->
    %% cdq, clc, cld, cmc, cwde, into, leave, nop, prefix_fs, stc, std
    1.

cmovcc_encode({{cc,CC}, {reg32,Reg32}, {rm32,RM32}}) ->
    [16#0F, 16#40 bor CC | encode_rm(RM32, Reg32, [])].

%% cmovcc_sizeof({{cc,_}, {reg32,_}, {rm32,RM32}}) ->
%%    2 + sizeof_rm(RM32).

incdec_encode(SubOpcode, Opnds) ->	% SubOpcode is either 0 or 1
    case Opnds of
	{{rm32,RM32}} ->
	    [16#FF | encode_rm(RM32, SubOpcode, [])];
	{{rm64,RM64}} ->
	    [rex([{w, 1}]), 16#FF | encode_rm(RM64, SubOpcode, [])]
    end.

%% incdec_sizeof(Opnds) ->
%%    case Opnds of
%%	{{rm32,RM32}} ->
%%	    1 + sizeof_rm(RM32);
%%	{{reg32,_}} ->
%%	    1
%%    end.

arith_unop_encode(Opcode, Opnds) ->  % div, idiv, mul, neg, not
    case Opnds of
	{{rm32,RM32}} ->
	    [16#F7 | encode_rm(RM32, Opcode, [])];
	{{rm64,RM64}} ->
	    [rex([{w,1}]), 16#F7 | encode_rm(RM64, Opcode, [])]
    end.

%% arith_unop_sizeof({{rm32,RM32}}) ->	% div, idiv, mul, neg, not
%%    1 + sizeof_rm(RM32).

enter_encode({{imm16,Imm16}, {imm8,Imm8}}) ->
    [16#C8 | le16(Imm16, [Imm8])].

enter_sizeof({{imm16,_}, {imm8,_}}) ->
    1 + 2 + 1.

imul_encode(Opnds) ->
    case Opnds of
	{{rm32,RM32}} ->				% <edx,eax> *= rm32
	    [16#F7 | encode_rm(RM32, 2#101, [])];
	{{rm64,RM64}} ->
	    [rex([{w,1}]), 16#F7 | encode_rm(RM64, 2#101, [])];
	{{reg32,Reg32}, {rm32,RM32}} ->			% reg *= rm32
	    [16#0F, 16#AF | encode_rm(RM32, Reg32, [])];
	{{reg64,Reg64}, {rm64,RM64}} ->
	    [rex([{w,1}]), 16#0F, 16#AF | encode_rm(RM64, Reg64, [])];
	{{reg32,Reg32}, {rm32,RM32}, {imm8,Imm8}} ->	% reg := rm32 * sext(imm8)
	    [16#6B | encode_rm(RM32, Reg32, [Imm8])];
	{{reg64,Reg64}, {rm64,RM64}, {imm8,Imm8}} ->
	    [rex([{w,1}]), 16#6B | encode_rm(RM64, Reg64, [Imm8])];
	{{reg32,Reg32}, {rm32,RM32}, {imm32,Imm32}} ->	% reg := rm32 * imm32
	    [16#69 | encode_rm(RM32, Reg32, le32(Imm32, []))];
	{{reg64,Reg64}, {rm64,RM64}, {imm32,Imm32}} ->
	    [rex([{w,1}]), 16#69 | encode_rm(RM64, Reg64, le32(Imm32, []))]
    end.

%% imul_sizeof(Opnds) ->
%%    case Opnds of
%%	{{rm32,RM32}} ->
%%	    1 + sizeof_rm(RM32);
%%	{{reg32,_}, {rm32,RM32}} ->
%%	    2 + sizeof_rm(RM32);
%%	{{reg32,_}, {rm32,RM32}, {imm8,_}} ->
%%	    1 + sizeof_rm(RM32) + 1;
%%	{{reg32,_}, {rm32,RM32}, {imm32,_}} ->
%%	    1 + sizeof_rm(RM32) + 4
%%    end.

jcc_encode(Opnds) ->
    case Opnds of
	{{cc,CC}, {rel8,Rel8}} ->
	    [16#70 bor CC, Rel8];
	{{cc,CC}, {rel32,Rel32}} ->
	    [16#0F, 16#80 bor CC | le32(Rel32, [])]
    end.

jcc_sizeof(Opnds) ->
    case Opnds of
       {{cc,_}, {rel8,_}} ->
	   2;
       {{cc,_}, {rel32,_}} ->
	   2 + 4
   end.

jmp8_op_encode(Opcode, {{rel8,Rel8}}) ->	% jecxz, loop, loope, loopne
    [Opcode, Rel8].

jmp8_op_sizeof({{rel8,_}}) ->			% jecxz, loop, loope, loopne
    2.

jmp_encode(Opnds) ->
    case Opnds of
	{{rel8,Rel8}} ->
	    [16#EB, Rel8];
	{{rel32,Rel32}} ->
	    [16#E9 | le32(Rel32, [])];
%%% 	{{rm32,RM32}} ->
%%% 	    [16#FF | encode_rm(RM32, 2#100, [])]
	{{rm64,RM64}} ->
	    [16#FF | encode_rm(RM64, 2#100, [])]
    end.

%% jmp_sizeof(Opnds) ->
%%    case Opnds of
%%	{{rel8,_}} ->
%%	    2;
%%	{{rel32,_}} ->
%%	    1 + 4;
%%	{{rm32,RM32}} ->
%%	    1 + sizeof_rm(RM32)
%%    end.

lea_encode({{reg32,Reg32}, {ea,EA}}) ->
    [16#8D | enc_ea(EA, Reg32, [])];
lea_encode({{reg64,Reg64}, {ea,EA}}) ->
    [rex([{w, 1}]), 16#8D | enc_ea(EA, Reg64, [])].

%% lea_sizeof({{reg32,_}, {ea,EA}}) ->
%%    1 + sizeof_ea(EA).

mov_encode(Opnds) ->
    case Opnds of
	{{rm8,RM8}, {reg8,Reg8}} ->
	    [rex([{r8, RM8}, {r8, Reg8}]), 16#88 | encode_rm(RM8, Reg8, [])];
	{{rm16,RM16}, {reg16,Reg16}} ->
	    [?PFX_OPND_16BITS, 16#89 | encode_rm(RM16, Reg16, [])];
	{{rm32,RM32}, {reg32,Reg32}} ->
	    [16#89 | encode_rm(RM32, Reg32, [])];
	{{rm64,RM64}, {reg64,Reg64}} ->
	    [rex([{w, 1}]), 16#89 | encode_rm(RM64, Reg64, [])];
	{{reg8,Reg8}, {rm8,RM8}} ->
	    [rex([{r8, RM8}, {r8, Reg8}]), 16#8A |
	     encode_rm(RM8, Reg8, [])];
	{{reg16,Reg16}, {rm16,RM16}} ->
	    [?PFX_OPND_16BITS, 16#8B | encode_rm(RM16, Reg16, [])];
	{{reg32,Reg32}, {rm32,RM32}} ->
	    [16#8B | encode_rm(RM32, Reg32, [])];
	{{reg64,Reg64}, {rm64,RM64}} ->
	    [rex([{w, 1}]), 16#8B | encode_rm(RM64, Reg64, [])];
	{al, {moffs8,Moffs8}} ->
	    [16#A0 | le32(Moffs8, [])];
	{ax, {moffs16,Moffs16}} ->
	    [?PFX_OPND_16BITS, 16#A1 | le32(Moffs16, [])];
	{eax, {moffs32,Moffs32}} ->
	    [16#A1 | le32(Moffs32, [])];
	{rax, {moffs32,Moffs32}} ->
	    [rex([{w, 1}]), 16#A1 | le32(Moffs32, [])];
	{{moffs8,Moffs8}, al} ->
	    [16#A2 | le32(Moffs8, [])];
	{{moffs16,Moffs16}, ax} ->
	    [?PFX_OPND_16BITS, 16#A3 | le32(Moffs16, [])];
	{{moffs32,Moffs32}, eax} ->
	    [16#A3 | le32(Moffs32, [])];
	{{moffs32,Moffs32}, rax} ->
	    [rex([{w, 1}]), 16#A3 | le32(Moffs32, [])];
	{{reg8,Reg8}, {imm8,Imm8}} ->
	    [rex([{b, Reg8}, {r8, Reg8}]), 16#B0 bor (Reg8 band 2#111), Imm8];
	{{reg16,Reg16}, {imm16,Imm16}} ->
	    [?PFX_OPND_16BITS, rex([{b, Reg16}]), 16#B8 bor (Reg16 band 2#111) 
             | le16(Imm16, [])];
	{{reg32,Reg32}, {imm32,Imm32}} ->
	    [rex([{b, Reg32}]), 16#B8 bor (Reg32 band 2#111)
             | le32(Imm32, [])];
	{{reg64,Reg64}, {imm64,Imm64}} ->
	    [rex([{w, 1}, {b, Reg64}]), 16#B8 bor (Reg64 band 2#111)
             | le64(Imm64, [])];
	{{rm8,RM8}, {imm8,Imm8}} ->
	    [rex([{r8, RM8}]), 16#C6 | encode_rm(RM8, 2#000, [Imm8])];
	{{rm16,RM16}, {imm16,Imm16}} ->
	    [?PFX_OPND_16BITS, 16#C7 |
             encode_rm(RM16, 2#000, le16(Imm16, []))];
	{{rm32,RM32}, {imm32,Imm32}} ->
	    [16#C7 | encode_rm(RM32, 2#000, le32(Imm32, []))];
	{{rm64,RM64}, {imm32,Imm32}} ->
	    [rex([{w, 1}]), 16#C7 | encode_rm(RM64, 2#000, le32(Imm32, []))]
    end.

%% mov_sizeof(Opnds) ->
%%     case Opnds of
%% 	{{rm8,RM8}, {reg8,_}} ->
%% 	    1 + sizeof_rm(RM8);
%% 	{{rm16,RM16}, {reg16,_}} ->
%% 	    2 + sizeof_rm(RM16);
%% 	{{rm32,RM32}, {reg32,_}} ->
%% 	    1 + sizeof_rm(RM32);
%% 	{{reg8,_}, {rm8,RM8}} ->
%% 	    1 + sizeof_rm(RM8);
%% 	{{reg16,_}, {rm16,RM16}} ->
%% 	    2 + sizeof_rm(RM16);
%% 	{{reg32,_}, {rm32,RM32}} ->
%% 	    1 + sizeof_rm(RM32);
%% 	{al, {moffs8,_}} ->
%% 	    1 + 4;
%% 	{ax, {moffs16,_}} ->
%% 	    2 + 4;
%% 	{eax, {moffs32,_}} ->
%% 	    1 + 4;
%% 	{{moffs8,_}, al} ->
%% 	    1 + 4;
%% 	{{moffs16,_}, ax} ->
%% 	    2 + 4;
%% 	{{moffs32,_}, eax} ->
%% 	    1 + 4;
%% 	{{reg8,_}, {imm8,_}} ->
%% 	    2;
%% 	{{reg16,_}, {imm16,_}} ->
%% 	    2 + 2;
%% 	{{reg32,_}, {imm32,_}} ->
%% 	    1 + 4;
%% 	{{rm8,RM8}, {imm8,_}} ->
%% 	    1 + sizeof_rm(RM8) + 1;
%% 	{{rm16,RM16}, {imm16,_}} ->
%% 	    2 + sizeof_rm(RM16) + 2;
%% 	{{rm32,RM32}, {imm32,_}} ->
%% 	    1 + sizeof_rm(RM32) + 4
%%     end.

movx_op_encode(Opcode, Opnds) ->	% movsx, movzx
    case Opnds of
	{{reg16,Reg16}, {rm8,RM8}} ->
	    [?PFX_OPND_16BITS, rex([{r8, RM8}]), 16#0F, Opcode |
	     encode_rm(RM8, Reg16, [])];
	{{reg32,Reg32}, {rm8,RM8}} ->
	    [rex([{r8, RM8}]), 16#0F, Opcode | encode_rm(RM8, Reg32, [])];
	{{reg32,Reg32}, {rm16,RM16}} ->
	    [16#0F, Opcode bor 1 | encode_rm(RM16, Reg32, [])];
	{{reg64,Reg64}, {rm8,RM8}} ->
	    [rex([{w,1}]), 16#0F, Opcode | encode_rm(RM8, Reg64, [])];
	{{reg64,Reg64}, {rm16,RM16}} ->
	    [rex([{w,1}]), 16#0F, Opcode bor 1 | encode_rm(RM16, Reg64, [])];
	{{reg64,Reg64}, {rm32,RM32}} ->
            %% This is magic... /Luna
	    [rex([{w,(1 band (Opcode bsr 3))}]), 16#63 |
             encode_rm(RM32, Reg64, [])]
    end.

%% movx_op_sizeof(Opnds) ->
%%    case Opnds of
%%	{{reg16,_}, {rm8,RM8}} ->
%%	    3 + sizeof_rm(RM8);
%%	{{reg32,_}, {rm8,RM8}} ->
%%	    1 + 2 + sizeof_rm(RM8);
%%	{{reg32,_}, {rm16,RM16}} ->
%%	    1 + 2 + sizeof_rm(RM16)
%%    end.

pop_encode(Opnds) ->
    case Opnds of
	{{rm64,RM64}} ->
	    [16#8F | encode_rm(RM64, 2#000, [])];
	{{reg64,Reg64}} ->
	    [rex([{b,Reg64}]),16#58 bor (Reg64 band 2#111)]
    end.

%% pop_sizeof(Opnds) ->
%%    case Opnds of
%%	{{rm32,RM32}} ->
%%	    1 + sizeof_rm(RM32);
%%	{{reg32,_}} ->
%%	    1
%%    end.

push_encode(Opnds) ->
    case Opnds of
%%% 	{{rm32,RM32}} ->
%%% 	    [16#FF | encode_rm(RM32, 2#110, [])];
	{{rm64,RM64}} ->
	    [16#FF | encode_rm(RM64, 2#110, [])];
%%% 	{{reg32,Reg32}} ->
%%% 	    [rex([{b, 1}]), 16#50 bor (Reg32 band 2#111)];
	{{reg64,Reg64}} ->
	    [rex([{b, Reg64}]), 16#50 bor (Reg64 band 2#111)];        
	{{imm8,Imm8}} ->	% sign-extended
	    [16#6A, Imm8];
	{{imm32,Imm32}} -> % Sign extended to 64 bits
	    [16#68 | le32(Imm32, [])]
    end.

%% push_sizeof(Opnds) ->
%%    case Opnds of
%%	{{rm32,RM32}} ->
%%	    1 + sizeof_rm(RM32);
%%	{{reg32,_}} ->
%%	    1;
%%	{{imm8,_}} ->
%%	    2;
%%	{{imm32,_}} ->
%%	    1 + 4
%%    end.

shift_op_encode(SubOpcode, Opnds) ->	% rol, ror, rcl, rcr, shl, shr, sar
     case Opnds of
         {{rm32,RM32}, 1} ->
             [16#D1 | encode_rm(RM32, SubOpcode, [])];
         {{rm32,RM32}, cl} ->
             [16#D3 | encode_rm(RM32, SubOpcode, [])];
         {{rm32,RM32}, {imm8,Imm8}} ->
             [16#C1 | encode_rm(RM32, SubOpcode, [Imm8])];
         {{rm64,RM64}, 1} ->
             [rex([{w,1}]), 16#D1 | encode_rm(RM64, SubOpcode, [])];
         {{rm64,RM64}, cl} ->
             [rex([{w,1}]), 16#D3 | encode_rm(RM64, SubOpcode, [])];
         {{rm64,RM64}, {imm8,Imm8}} ->
             [rex([{w,1}]), 16#C1 | encode_rm(RM64, SubOpcode, [Imm8])]
     end.

%% shift_op_sizeof(Opnds) ->		% rcl, rcr, rol, ror, sar, shl, shr
%%     case Opnds of
%% 	{{rm32,RM32}, 1} ->
%% 	    1 + sizeof_rm(RM32);
%% 	{{rm32,RM32}, cl} ->
%% 	    1 + sizeof_rm(RM32);
%% 	{{rm32,RM32}, {imm8,_Imm8}} ->
%% 	    1 + sizeof_rm(RM32) + 1
%%     end.

ret_encode(Opnds) ->
    case Opnds of
	{} ->
	    [16#C3];
	{{imm16,Imm16}} ->
	    [16#C2 | le16(Imm16, [])]
    end.

ret_sizeof(Opnds) ->
    case Opnds of
	{} ->
	    1;
	{{imm16,_}} ->
	    1 + 2
    end.

setcc_encode({{cc,CC}, {rm8,RM8}}) ->
    [rex([{r8, RM8}]), 16#0F, 16#90 bor CC | encode_rm(RM8, 2#000, [])].

%% setcc_sizeof({{cc,_}, {rm8,RM8}}) ->
%%    2 + sizeof_rm(RM8).

shd_op_encode(Opcode, Opnds) ->
    case Opnds of
	{{rm32,RM32}, {reg32,Reg32}, {imm8,Imm8}} ->
	    [16#0F, Opcode | encode_rm(RM32, Reg32, [Imm8])];
	{{rm32,RM32}, {reg32,Reg32}, cl} ->
	    [16#0F, Opcode bor 1 | encode_rm(RM32, Reg32, [])]
    end.

%% shd_op_sizeof(Opnds) ->
%%    case Opnds of
%%	{{rm32,RM32}, {reg32,_}, {imm8,_}} ->
%%	    2 + sizeof_rm(RM32) + 1;
%%	{{rm32,RM32}, {reg32,_}, cl} ->
%%	    2 + sizeof_rm(RM32)
%%    end.

test_encode(Opnds) ->
    case Opnds of
	{al, {imm8,Imm8}} ->
	    [16#A8, Imm8];
	{ax, {imm16,Imm16}} ->
	    [?PFX_OPND_16BITS, 16#A9 | le16(Imm16, [])];
	{eax, {imm32,Imm32}} ->
	    [16#A9 | le32(Imm32, [])];
	{rax, {imm32,Imm32}} ->
	    [rex([{w,1}]), 16#A9 | le32(Imm32, [])];
	{{rm8,RM8}, {imm8,Imm8}} ->
	    [rex([{r8,RM8}]), 16#F6 | encode_rm(RM8, 2#000, [Imm8])];
	{{rm16,RM16}, {imm16,Imm16}} ->
	    [?PFX_OPND_16BITS, 16#F7 | encode_rm(RM16, 2#000, le16(Imm16, []))];
	{{rm32,RM32}, {imm32,Imm32}} ->
	    [16#F7 | encode_rm(RM32, 2#000, le32(Imm32, []))];
	{{rm64,RM64}, {imm32,Imm32}} ->
	    [rex([{w,1}]), 16#F7 | encode_rm(RM64, 2#000, le32(Imm32, []))];
	{{rm32,RM32}, {reg32,Reg32}} ->
	    [16#85 | encode_rm(RM32, Reg32, [])];
	{{rm64,RM64}, {reg64,Reg64}} ->
	    [rex([{w,1}]), 16#85 | encode_rm(RM64, Reg64, [])]
    end.

%% test_sizeof(Opnds) ->
%%     case Opnds of
%% 	{eax, {imm32,_}} ->
%% 	    1 + 4;
%% 	{{rm32,RM32}, {imm32,_}} ->
%% 	    1 + sizeof_rm(RM32) + 4;
%% 	{{rm32,RM32}, {reg32,_}} ->
%% 	    1 + sizeof_rm(RM32)
%%     end.

fild_encode(Opnds) ->
    %% The operand cannot be a register!
    {{rm64, RM64}} = Opnds,
    [16#DB | encode_rm(RM64, 2#000, [])].

%% fild_sizeof(Opnds) ->
%%    {{rm32, RM32}} = Opnds,
%%    1 + sizeof_rm(RM32).

fld_encode(Opnds) ->
    case Opnds of
	{{rm64fp, RM64fp}} ->
	    [16#DD | encode_rm(RM64fp, 2#000, [])];
	{{fpst, St}} ->
	    [16#D9, 16#C0 bor st(St)]
    end.

%% fld_sizeof(Opnds) ->
%%    case Opnds of
%%	{{rm64fp, RM64fp}} ->
%%	    1 + sizeof_rm(RM64fp);
%%	{{fpst, _}} ->
%%	    2
%%    end.

x87_comm_arith_encode(OpCode, Opnds) ->
    %% fadd, fmul
    case Opnds of
	{{rm64fp, RM64fp}} ->
	    [16#DC | encode_rm(RM64fp, OpCode, [])];
	{{fpst,0}, {fpst,St}} ->
	    [16#D8, (16#C0 bor (OpCode bsl 3)) bor st(St)];
	{{fpst,St}, {fpst,0}} ->	
	    [16#DC, (16#C0 bor (OpCode bsl 3)) bor st(St)]
    end.
	    
x87_comm_arith_pop_encode(OpCode, Opnds) ->
    %% faddp, fmulp
    case Opnds of
	[] ->
	    [16#DE, 16#C0 bor (OpCode bsl 3) bor st(1)];
	{{fpst,St},{fpst,0}} ->
	    [16#DE, 16#C0 bor (OpCode bsl 3) bor st(St)]
    end.

x87_arith_encode(OpCode, Opnds) ->
    %% fdiv, fsub
    case Opnds of
	{{rm64fp, RM64fp}} ->
	    [16#DC | encode_rm(RM64fp, OpCode, [])];
	{{fpst,0}, {fpst,St}} ->
	    OpCode0 = OpCode band 2#110,
	    [16#D8, 16#C0 bor (OpCode0 bsl 3) bor st(St)];
	{{fpst,St}, {fpst,0}} ->
	    OpCode0 = OpCode bor 1,
	    [16#DC, 16#C0 bor (OpCode0 bsl 3) bor st(St)]
    end.
	    
x87_arith_pop_encode(OpCode, Opnds) ->
    %% fdivp, fsubp
    OpCode0 = OpCode bor 1,
    case Opnds of
	[] ->
	    [16#DE, 16#C8 bor (OpCode0 bsl 3) bor st(1)];
	{{fpst,St}, {fpst,0}} ->
	    [16#DE, 16#C8 bor (OpCode0 bsl 3) bor st(St)]
    end.

x87_arith_rev_encode(OpCode, Opnds) ->
    %% fdivr, fsubr
    case Opnds of
	{{rm64fp, RM64fp}} ->
	    [16#DC | encode_rm(RM64fp, OpCode, [])];
	{{fpst,0}, {fpst,St}} ->
	    OpCode0 = OpCode bor 1,
	    [16#D8, 16#C0 bor (OpCode0 bsl 3) bor st(St)];
	{{fpst,St}, {fpst,0}} ->
	    OpCode0 = OpCode band 2#110,
	    [16#DC, 16#C0 bor (OpCode0 bsl 3) bor st(St)]
    end.
	    
x87_arith_rev_pop_encode(OpCode, Opnds) ->
    %% fdivrp, fsubrp
    OpCode0 = OpCode band 2#110,
    case Opnds of
	[] ->
	    [16#DE, 16#C0 bor (OpCode0 bsl 3) bor st(1)];
	{{fpst,St}, {fpst, 0}} ->
	    [16#DE, 16#C0 bor (OpCode0 bsl 3) bor st(St)]
    end.

%% x87_arith_sizeof(Opnds) ->
%%    case Opnds of
%%	{{rm64fp, RM64fp}} ->
%%	    1 + sizeof_rm(RM64fp);
%%	{{fpst,0}, {fpst,_}} ->
%%	    2;
%%	{{fpst,_}, {fpst,0}} ->
%%	    2
%%    end.

fst_encode(OpCode, Opnds) ->
    case Opnds of
	{{rm64fp, RM64fp}} ->
	    [16#DD | encode_rm(RM64fp, OpCode, [])];
	{{fpst, St}} ->
	    [16#DD, 16#C0 bor (OpCode bsl 3) bor st(St)]
    end.

%% fst_sizeof(Opnds) ->
%%     case Opnds of
%% 	{{rm64fp, RM64fp}} ->
%% 	    1 + sizeof_rm(RM64fp);
%% 	{{fpst, _}} ->
%% 	    2
%%     end.
    
fchs_encode() ->
    [16#D9, 16#E0].

fchs_sizeof() ->
    2.

ffree_encode({{fpst, St}})->
    [16#DD, 16#C0 bor st(St)].

ffree_sizeof() ->
    2.

fwait_encode() ->
    [16#9B].

fwait_sizeof() ->
    1.

fxch_encode(Opnds) ->
    case Opnds of
	[] ->
	    [16#D9, 16#C8 bor st(1)];
	{{fpst, St}} ->
	    [16#D9, 16#C8 bor st(St)]
    end.

fxch_sizeof() ->
    2.

insn_encode(Op, Opnds, Offset) ->
    Bytes_and_REX = insn_encode_internal(Op, Opnds),
    Bytes         = fix_rex(Bytes_and_REX),
    case has_relocs(Bytes) of
	false ->	% the common case
	    {Bytes, []};
	_ ->
	    fix_relocs(Bytes, Offset, [], [])
    end.

fix_rex(Bytes) ->
    fix_rex(Bytes, 2#0100 bsl 4, []).

fix_rex([{rex, REX} | Rest], REXAcc, Bytes) ->
    fix_rex(Rest, REXAcc bor REX, Bytes);
fix_rex([{{rex, REX}, Byte} | Rest], REXAcc, Bytes) ->
    fix_rex(Rest, REXAcc bor REX, [Byte | Bytes]);
fix_rex([Byte | Rest], REXAcc, Bytes) ->
    fix_rex(Rest, REXAcc, [Byte | Bytes]);
fix_rex([], 2#01000000, Bytes) ->              % no rex prefix
    lists:reverse(Bytes);
fix_rex([], REX0, Bytes) ->                    % rex prefix...
    REX = REX0 band 16#FF, % for 8 bit registers
    [Head|Tail] = lists:reverse(Bytes),
    case Head of
        16#66 ->                               % ...and 16 bit/sse2 prefix
            [16#66, REX | Tail];
	16#F2 ->                               % ...and sse2 prefix
	    [16#F2, REX | Tail];
        _ ->                                   % ...only
           [REX, Head | Tail]
    end.

has_relocs([{le32,_,_}|_]) -> true;
has_relocs([{le64,_,_}|_]) -> true;
has_relocs([_|Bytes]) -> has_relocs(Bytes);
has_relocs([]) -> false.

fix_relocs([{le32,Tag,Val}|Bytes], Offset, Code, Relocs) ->
    fix_relocs(Bytes, Offset+4,
	       [16#00, 16#00, 16#00, 16#00 | Code],
	       [{Tag,Offset,Val}|Relocs]);
fix_relocs([{le64,Tag,Val}|Bytes], Offset, Code, Relocs) ->
    fix_relocs(Bytes, Offset+8,
	       [16#00, 16#00, 16#00, 16#00,
                16#00, 16#00, 16#00, 16#00 | Code],
	       [{Tag,Offset,Val}|Relocs]);
fix_relocs([Byte|Bytes], Offset, Code, Relocs) ->
    fix_relocs(Bytes, Offset+1, [Byte|Code], Relocs);
fix_relocs([], _Offset, Code, Relocs) ->
    {lists:reverse(Code), lists:reverse(Relocs)}.

insn_encode_internal(Op, Opnds) ->
    case Op of
	'adc' -> arith_binop_encode(2#010, Opnds);
	'add' -> arith_binop_encode(2#000, Opnds);
	'and' -> arith_binop_encode(2#100, Opnds);
	'bsf' -> bs_op_encode(16#BC, Opnds);
	'bsr' -> bs_op_encode(16#BD, Opnds);
	'bswap' -> bswap_encode(Opnds);
	'bt' -> bt_op_encode(2#100, Opnds);
	'btc' -> bt_op_encode(2#111, Opnds);
	'btr' -> bt_op_encode(2#110, Opnds);
	'bts' -> bt_op_encode(2#101, Opnds);
	'call' -> call_encode(Opnds);
	'cbw' -> cbw_encode(Opnds);
	'cdq' -> nullary_op_encode(16#99, Opnds);
	'clc' -> nullary_op_encode(16#F8, Opnds);
	'cld' -> nullary_op_encode(16#FC, Opnds);
	'cmc' -> nullary_op_encode(16#F5, Opnds);
	'cmovcc' -> cmovcc_encode(Opnds);
	'cmp' -> arith_binop_encode(2#111, Opnds);
	'cwde' -> nullary_op_encode(16#98, Opnds);
	'dec' -> incdec_encode(2#001, Opnds);
	'div' -> arith_unop_encode(2#110, Opnds);
	'enter' -> enter_encode(Opnds);
	'idiv' -> arith_unop_encode(2#111, Opnds);
	'imul' -> imul_encode(Opnds);
	'inc' -> incdec_encode(2#000, Opnds);
	'into' -> case get(hipe_target_arch) of
                      x86   -> nullary_op_encode(16#CE, Opnds);
                      amd64 -> exit({invalid_amd64_opcode,
                                     hipe_amd64_encode__erl})
                  end;
	'jcc' -> jcc_encode(Opnds);
	'jecxz' -> jmp8_op_encode(16#E3, Opnds);
	'jmp' -> jmp_encode(Opnds);
	'lea' -> lea_encode(Opnds);
	'leave' -> nullary_op_encode(16#C9, Opnds);
	'loop' -> jmp8_op_encode(16#E2, Opnds);
	'loope' -> jmp8_op_encode(16#E1, Opnds);
	'loopne' -> jmp8_op_encode(16#E0, Opnds);
	'mov' -> mov_encode(Opnds);
	'movsx' -> movx_op_encode(16#BE, Opnds);
	'movzx' -> movx_op_encode(16#B6, Opnds);
	'mul' -> arith_unop_encode(2#100, Opnds);
	'neg' -> arith_unop_encode(2#011, Opnds);
	'nop' -> nullary_op_encode(16#90, Opnds);
	'not' -> arith_unop_encode(2#010, Opnds);
	'or' -> arith_binop_encode(2#001, Opnds);
	'pop' -> pop_encode(Opnds);
	'prefix_fs' -> nullary_op_encode(16#64, Opnds);
	'push' -> push_encode(Opnds);
	'rcl' -> shift_op_encode(2#010, Opnds);
	'rcr' -> shift_op_encode(2#011, Opnds);
	'ret' -> ret_encode(Opnds);
	'rol' -> shift_op_encode(2#000, Opnds);
	'ror' -> shift_op_encode(2#001, Opnds);
	'sar' -> shift_op_encode(2#111, Opnds);
	'sbb' -> arith_binop_encode(2#011, Opnds);
	'setcc' -> setcc_encode(Opnds);
	'shl' -> shift_op_encode(2#100, Opnds);
	'shld' -> shd_op_encode(16#A4, Opnds);
	'shr' -> shift_op_encode(2#101, Opnds);
	'shrd' -> shd_op_encode(16#AC, Opnds);
	'stc' -> nullary_op_encode(16#F9, Opnds);
	'std' -> nullary_op_encode(16#FD, Opnds);
	'sub' -> arith_binop_encode(2#101, Opnds);
	'test' -> test_encode(Opnds);
	'xor' -> arith_binop_encode(2#110, Opnds);

        %% sse2
        'addsd'   -> sse2_arith_binop_encode(16#F2, 16#58, Opnds);
        'cmpsd'   -> sse2_arith_binop_encode(16#F2, 16#C2, Opnds);
        'comisd'  -> sse2_arith_binop_encode(16#66, 16#2F, Opnds);
	'cvtsi2sd' -> sse2_cvtsi2sd_encode(Opnds);
        'divsd'   -> sse2_arith_binop_encode(16#F2, 16#5E, Opnds);
        'maxsd'   -> sse2_arith_binop_encode(16#F2, 16#5F, Opnds);
        'minsd'   -> sse2_arith_binop_encode(16#F2, 16#5D, Opnds);
        'movsd'   -> sse2_mov_encode(Opnds);
        'mulsd'   -> sse2_arith_binop_encode(16#F2, 16#59, Opnds);
        'sqrtsd'  -> sse2_arith_binop_encode(16#F2, 16#51, Opnds);
        'subsd'   -> sse2_arith_binop_encode(16#F2, 16#5C, Opnds);
        'ucomisd' -> sse2_arith_binop_encode(16#66, 16#2E, Opnds);
	'xorpd'   -> sse2_arith_binop_encode(16#66, 16#57, Opnds);
        %% End of sse2

	%% x87
	'fadd'   -> x87_comm_arith_encode(2#000, Opnds);
	'faddp'  -> x87_comm_arith_pop_encode(2#000, Opnds);
	'fchs'   -> fchs_encode();
	'fdiv'   -> x87_arith_encode(2#110, Opnds);
	'fdivp'  -> x87_arith_pop_encode(2#110, Opnds);
	'fdivr'  -> x87_arith_rev_encode(2#111, Opnds);
	'fdivrp' -> x87_arith_rev_pop_encode(2#111, Opnds);
	'ffree'  -> ffree_encode(Opnds);
	'fild'   -> fild_encode(Opnds);
	'fld'    -> fld_encode(Opnds);
	'fmul'   -> x87_comm_arith_encode(2#001, Opnds);
	'fmulp'  -> x87_comm_arith_pop_encode(2#001, Opnds);
	'fst'    -> fst_encode(2#010, Opnds);
	'fstp'   -> fst_encode(2#011, Opnds);
	'fsub'   -> x87_arith_encode(2#100, Opnds);
	'fsubp'  -> x87_arith_pop_encode(2#100, Opnds);
	'fsubr'  -> x87_arith_rev_encode(2#101, Opnds);
	'fsubrp' -> x87_arith_rev_pop_encode(2#101, Opnds);
	'fwait'  -> fwait_encode();
	'fxch'   -> fxch_encode(Opnds);
	%% End of x87

	_ -> exit({?MODULE,insn_encode,Op})
    end.

insn_sizeof(Op, Opnds) ->
    case Op of
	'cbw' -> cbw_sizeof(Opnds);
  	'cdq' -> nullary_op_sizeof(Opnds);
  	'clc' -> nullary_op_sizeof(Opnds);
  	'cld' -> nullary_op_sizeof(Opnds);
  	'cmc' -> nullary_op_sizeof(Opnds);
  	'cwde' -> nullary_op_sizeof(Opnds);
  	'enter' -> enter_sizeof(Opnds);
  	'into' -> nullary_op_sizeof(Opnds);
  	'jcc' -> jcc_sizeof(Opnds);
  	'jecxz' -> jmp8_op_sizeof(Opnds);
  	'leave' -> nullary_op_sizeof(Opnds);
  	'loop' -> jmp8_op_sizeof(Opnds);
  	'loope' -> jmp8_op_sizeof(Opnds);
  	'loopne' -> jmp8_op_sizeof(Opnds);
  	'nop' -> nullary_op_sizeof(Opnds);
  	'prefix_fs' -> nullary_op_sizeof(Opnds);
  	'ret' -> ret_sizeof(Opnds);
  	'stc' -> nullary_op_sizeof(Opnds);
  	'std' -> nullary_op_sizeof(Opnds);

%% 	%% x87
%% 	'fadd'   -> x87_arith_sizeof(Opnds);
%% 	'faddp'  -> x87_arith_sizeof(Opnds);
 	'fchs'   -> fchs_sizeof();
%% 	'fdiv'   -> x87_arith_sizeof(Opnds);
%% 	'fdivp'  -> x87_arith_sizeof(Opnds);
%% 	'fdivr'  -> x87_arith_sizeof(Opnds);
%% 	'fdivrp' -> x87_arith_sizeof(Opnds);
 	'ffree'  -> ffree_sizeof();
%% 	'fild'   -> fild_sizeof(Opnds);
%% 	'fld'    -> fld_sizeof(Opnds);
%% 	'fmul'   -> x87_arith_sizeof(Opnds);
%% 	'fmulp'  -> x87_arith_sizeof(Opnds);
%% 	'fst'    -> fst_sizeof(Opnds);
%% 	'fstp'   -> fst_sizeof(Opnds);
%% 	'fsub'   -> x87_arith_sizeof(Opnds);
%% 	'fsubp'  -> x87_arith_sizeof(Opnds);
%% 	'fsubr'  -> x87_arith_sizeof(Opnds);
%% 	'fsubrp' -> x87_arith_sizeof(Opnds);
 	'fwait'  -> fwait_sizeof();
 	'fxch'   -> fxch_sizeof();	
%% 	%% End of x87
	_ -> %% Hack that is to be removed some day... Maybe...
            {Bytes, _} = insn_encode(Op, Opnds, 0),
            length(Bytes)
%%	'adc' -> arith_binop_sizeof(Opnds);
%%  	'add' -> arith_binop_sizeof(Opnds);
%%  	'and' -> arith_binop_sizeof(Opnds);
%%  	'bsf' -> bs_op_sizeof(Opnds);
%%  	'bsr' -> bs_op_sizeof(Opnds);
%%  	'bswap' -> bswap_sizeof(Opnds);
%%  	'bt' -> bt_op_sizeof(Opnds);
%%  	'btc' -> bt_op_sizeof(Opnds);
%%  	'btr' -> bt_op_sizeof(Opnds);
%%  	'bts' -> bt_op_sizeof(Opnds);
%%  	'call' -> call_sizeof(Opnds);
%%  	'cmovcc' -> cmovcc_sizeof(Opnds);
%%  	'cmp' -> arith_binop_sizeof(Opnds);
%%  	'dec' -> incdec_sizeof(Opnds);
%%  	'div' -> arith_unop_sizeof(Opnds);
%%  	'idiv' -> arith_unop_sizeof(Opnds);
%%  	'imul' -> imul_sizeof(Opnds);
%%  	'inc' -> incdec_sizeof(Opnds);
%%  	'jmp' -> jmp_sizeof(Opnds);
%%  	'lea' -> lea_sizeof(Opnds);
%%  	'mov' -> mov_sizeof(Opnds);
%%  	'movsx' -> movx_op_sizeof(Opnds);
%%  	'movzx' -> movx_op_sizeof(Opnds);
%%  	'mul' -> arith_unop_sizeof(Opnds);
%%  	'neg' -> arith_unop_sizeof(Opnds);
%%  	'not' -> arith_unop_sizeof(Opnds);
%%  	'or' -> arith_binop_sizeof(Opnds);
%%  	'pop' -> pop_sizeof(Opnds);
%%  	'push' -> push_sizeof(Opnds);
%%  	'rcl' -> shift_op_sizeof(Opnds);
%%  	'rcr' -> shift_op_sizeof(Opnds);
%%  	'rol' -> shift_op_sizeof(Opnds);
%%  	'ror' -> shift_op_sizeof(Opnds);
%%  	'sar' -> shift_op_sizeof(Opnds);
%%  	'sbb' -> arith_binop_sizeof(Opnds);
%%  	'setcc' -> setcc_sizeof(Opnds);
%%  	'shl' -> shift_op_sizeof(Opnds);
%%  	'shld' -> shd_op_sizeof(Opnds);
%%  	'shr' -> shift_op_sizeof(Opnds);
%%  	'shrd' -> shd_op_sizeof(Opnds);
%% 	'sub' -> arith_binop_sizeof(Opnds);
%%  	'test' -> test_sizeof(Opnds);
%%  	'xor' -> arith_binop_sizeof(Opnds);
%%	_ -> exit({?MODULE,insn_sizeof,Op})
    end.

%%=====================================================================
%% testing interface
%%=====================================================================

-ifdef(DO_HIPE_AMD64_ENCODE_TEST).

say(OS, Str) ->
    file:write(OS, Str).

digit16(Dig0) ->
    Dig = Dig0 band 16#F,
    if Dig >= 16#A -> $A + (Dig - 16#A);
       true -> $0 + Dig
    end.

say_byte(OS, Byte) ->
    say(OS, "0x"),
    say(OS, [digit16(Byte bsr 4)]),
    say(OS, [digit16(Byte)]).

init(OS) ->
    say(OS, "\t.text\n").

say_bytes(OS, Byte0, Bytes0) ->
    say_byte(OS, Byte0),
    case Bytes0 of
	[] ->
	    say(OS, "\n");
	[Byte1|Bytes1] ->
	    say(OS, ","),
	    say_bytes(OS, Byte1, Bytes1)
    end.

t(OS, Op, Opnds) ->
    insn_sizeof(Op, Opnds),
    {[Byte|Bytes],[]} = insn_encode(Op, Opnds, 0),
    say(OS, "\t.byte "),
    say_bytes(OS, Byte, Bytes).

dotest1(OS) ->
    init(OS),
    % exercise all rm32 types
    t(OS,lea,{{reg32,?EAX},{ea,ea_disp32_rip(16#87654321)}}),
    t(OS,lea,{{reg32,?EAX},{ea,ea_sib(sib(?ECX))}}),
    t(OS,lea,{{reg32,?EAX},{ea,ea_sib(sib(?ECX,sindex(2#10,?EDI)))}}),
    t(OS,lea,{{reg32,?EAX},{ea,ea_disp32_sindex(16#87654321)}}),
    t(OS,lea,{{reg32,?EAX},{ea,ea_disp32_sindex(16#87654321,sindex(2#10,?EDI))}}),
    t(OS,lea,{{reg32,?EAX},{ea,ea_base(?ECX)}}),
    t(OS,lea,{{reg32,?EAX},{ea,ea_disp8_sib(16#03,sib(?ECX))}}),
    t(OS,lea,{{reg32,?EAX},{ea,ea_disp8_sib(16#03,sib(?ECX,sindex(2#10,?EDI)))}}),
    t(OS,lea,{{reg32,?EAX},{ea,ea_disp8_base(16#3,?ECX)}}),
    t(OS,lea,{{reg32,?EAX},{ea,ea_disp32_sib(16#87654321,sib(?ECX))}}),
    t(OS,lea,{{reg32,?EAX},{ea,ea_disp32_sib(16#87654321,sib(?ECX,sindex(2#10,?EDI)))}}),
    t(OS,lea,{{reg32,?EAX},{ea,ea_disp32_base(16#87654321,?EBP)}}),
    t(OS,call,{{rm32,rm_reg(?EAX)}}),
    t(OS,call,{{rm32,rm_mem(ea_disp32_sindex(16#87654321,sindex(2#10,?EDI)))}}),
    t(OS,call,{{rel32,-5}}),
    % default parameters for the tests below
    Word32 = 16#87654321,
    Word16 = 16#F00F,
    Word8 = 16#80,
    Imm32 = {imm32,Word32},
    Imm16 = {imm16,Word16},
    Imm8 = {imm8,Word8},
    RM64 = {rm64,rm_reg(?EDX)},
    RM32 = {rm32,rm_reg(?EDX)},
    RM16 = {rm16,rm_reg(?EDX)},
    RM16REX = {rm16,rm_reg(?R13)},
    RM8 = {rm8,rm_reg(?EDX)},
    RM8REX = {rm8,rm_reg(?SIL)},
    Rel32 = {rel32,Word32},
    Rel8 = {rel8,Word8},
    Moffs32 = {moffs32,Word32},
    Moffs16 = {moffs16,Word32},
    Moffs8 = {moffs8,Word32},
    CC = {cc,?CC_G},
    Reg64 = {reg64,?EAX},
    Reg32 = {reg32,?EAX},
    Reg16 = {reg16,?EAX},
    Reg8 = {reg8,?SPL},
    EA = {ea,ea_base(?ECX)},
    % exercise each instruction definition
    t(OS,'adc',{eax,Imm32}),
    t(OS,'adc',{RM32,Imm32}),
    t(OS,'adc',{RM32,Imm8}),
    t(OS,'adc',{RM32,Reg32}),
    t(OS,'adc',{Reg32,RM32}),
    t(OS,'add',{eax,Imm32}),
    t(OS,'add',{RM32,Imm32}),
    t(OS,'add',{RM32,Imm8}),
    t(OS,'add',{RM32,Reg32}),
    t(OS,'add',{Reg32,RM32}),
    t(OS,'and',{eax,Imm32}),
    t(OS,'and',{RM32,Imm32}),
    t(OS,'and',{RM32,Imm8}),
    t(OS,'and',{RM32,Reg32}),
    t(OS,'and',{Reg32,RM32}),
    t(OS,'bsf',{Reg32,RM32}),
    t(OS,'bsr',{Reg32,RM32}),
    t(OS,'bswap',{Reg32}),
    t(OS,'bt',{RM32,Reg32}),
    t(OS,'bt',{RM32,Imm8}),
    t(OS,'btc',{RM32,Reg32}),
    t(OS,'btc',{RM32,Imm8}),
    t(OS,'btr',{RM32,Reg32}),
    t(OS,'btr',{RM32,Imm8}),
    t(OS,'bts',{RM32,Reg32}),
    t(OS,'bts',{RM32,Imm8}),
    t(OS,'call',{Rel32}),
    t(OS,'call',{RM32}),
    t(OS,'cbw',{}),
    t(OS,'cdq',{}),
    t(OS,'clc',{}),
    t(OS,'cld',{}),
    t(OS,'cmc',{}),
    t(OS,'cmovcc',{CC,Reg32,RM32}),
    t(OS,'cmp',{eax,Imm32}),
    t(OS,'cmp',{RM32,Imm32}),
    t(OS,'cmp',{RM32,Imm8}),
    t(OS,'cmp',{RM32,Reg32}),
    t(OS,'cmp',{Reg32,RM32}),
    t(OS,'cwde',{}),
    t(OS,'dec',{RM32}),
    t(OS,'dec',{Reg32}),
    t(OS,'div',{RM32}),
    t(OS,'enter',{Imm16,{imm8,3}}),
    t(OS,'idiv',{RM32}),
    t(OS,'imul',{RM32}),
    t(OS,'imul',{Reg32,RM32}),
    t(OS,'imul',{Reg32,RM32,Imm8}),
    t(OS,'imul',{Reg32,RM32,Imm32}),
    t(OS,'inc',{RM32}),
    t(OS,'inc',{Reg32}),
    t(OS,'into',{}),
    t(OS,'jcc',{CC,Rel8}),
    t(OS,'jcc',{CC,Rel32}),
    t(OS,'jecxz',{Rel8}),
    t(OS,'jmp',{Rel8}),
    t(OS,'jmp',{Rel32}),
    t(OS,'jmp',{RM32}),
    t(OS,'lea',{Reg32,EA}),
    t(OS,'leave',{}),
    t(OS,'loop',{Rel8}),
    t(OS,'loope',{Rel8}),
    t(OS,'loopne',{Rel8}),
    t(OS,'mov',{RM8,Reg8}),
    t(OS,'mov',{RM16,Reg16}),
    t(OS,'mov',{RM32,Reg32}),
    t(OS,'mov',{Reg8,RM8}),
    t(OS,'mov',{Reg16,RM16}),
    t(OS,'mov',{Reg32,RM32}),
    t(OS,'mov',{al,Moffs8}),
    t(OS,'mov',{ax,Moffs16}),
    t(OS,'mov',{eax,Moffs32}),
    t(OS,'mov',{Moffs8,al}),
    t(OS,'mov',{Moffs16,ax}),
    t(OS,'mov',{Moffs32,eax}),
    t(OS,'mov',{Reg8,Imm8}),
    t(OS,'mov',{Reg16,Imm16}),
    t(OS,'mov',{Reg32,Imm32}),
    t(OS,'mov',{RM8,Imm8}),
    t(OS,'mov',{RM16,Imm16}),
    t(OS,'mov',{RM32,Imm32}),
    t(OS,'movsx',{Reg16,RM8}),
    t(OS,'movsx',{Reg32,RM8}),
    t(OS,'movsx',{Reg32,RM16}),
    t(OS,'movzx',{Reg16,RM8}),
    t(OS,'movzx',{Reg32,RM8}),
    t(OS,'movzx',{Reg32,RM16}),
    t(OS,'mul',{RM32}),
    t(OS,'neg',{RM32}),
    t(OS,'nop',{}),
    t(OS,'not',{RM32}),
    t(OS,'or',{eax,Imm32}),
    t(OS,'or',{RM32,Imm32}),
    t(OS,'or',{RM32,Imm8}),
    t(OS,'or',{RM32,Reg32}),
    t(OS,'or',{Reg32,RM32}),
    t(OS,'pop',{RM32}),
    t(OS,'pop',{Reg32}),
    t(OS,'push',{RM32}),
    t(OS,'push',{Reg32}),
    t(OS,'push',{Imm8}),
    t(OS,'push',{Imm32}),
    t(OS,'rcl',{RM32,1}),
    t(OS,'rcl',{RM32,cl}),
    t(OS,'rcl',{RM32,Imm8}),
    t(OS,'rcr',{RM32,1}),
    t(OS,'rcr',{RM32,cl}),
    t(OS,'rcr',{RM32,Imm8}),
    t(OS,'ret',{}),
    t(OS,'ret',{Imm16}),
    t(OS,'rol',{RM32,1}),
    t(OS,'rol',{RM32,cl}),
    t(OS,'rol',{RM32,Imm8}),
    t(OS,'ror',{RM32,1}),
    t(OS,'ror',{RM32,cl}),
    t(OS,'ror',{RM32,Imm8}),
    t(OS,'sar',{RM32,1}),
    t(OS,'sar',{RM32,cl}),
    t(OS,'sar',{RM32,Imm8}),
    t(OS,'sbb',{eax,Imm32}),
    t(OS,'sbb',{RM32,Imm32}),
    t(OS,'sbb',{RM32,Imm8}),
    t(OS,'sbb',{RM32,Reg32}),
    t(OS,'sbb',{Reg32,RM32}),
    t(OS,'setcc',{CC,RM8}),
    t(OS,'shl',{RM32,1}),
    t(OS,'shl',{RM32,cl}),
    t(OS,'shl',{RM32,Imm8}),
    t(OS,'shld',{RM32,Reg32,Imm8}),
    t(OS,'shld',{RM32,Reg32,cl}),
    t(OS,'shr',{RM32,1}),
    t(OS,'shr',{RM32,cl}),
    t(OS,'shr',{RM32,Imm8}),
    t(OS,'shrd',{RM32,Reg32,Imm8}),
    t(OS,'shrd',{RM32,Reg32,cl}),
    t(OS,'stc',{}),
    t(OS,'std',{}),
    t(OS,'sub',{eax,Imm32}),
    t(OS,'sub',{RM32,Imm32}),
    t(OS,'sub',{RM32,Imm8}),
    t(OS,'sub',{RM32,Reg32}),
    t(OS,'sub',{Reg32,RM32}),
    t(OS,'test',{al,Imm8}),
    t(OS,'test',{ax,Imm16}),
    t(OS,'test',{eax,Imm32}),
    t(OS,'test',{rax,Imm32}),
    t(OS,'test',{RM8,Imm8}),
    t(OS,'test',{RM8REX,Imm8}),
    t(OS,'test',{RM16,Imm16}),
    t(OS,'test',{RM16REX,Imm16}),
    t(OS,'test',{RM32,Imm32}),
    t(OS,'test',{RM64,Imm32}),
    t(OS,'test',{RM32,Reg32}),
    t(OS,'test',{RM64,Reg64}),
    t(OS,'xor',{eax,Imm32}),
    t(OS,'xor',{RM32,Imm32}),
    t(OS,'xor',{RM32,Imm8}),
    t(OS,'xor',{RM32,Reg32}),
    t(OS,'xor',{Reg32,RM32}),
    t(OS,'prefix_fs',{}), t(OS,'add',{{reg32,?EAX},{rm32,rm_mem(ea_disp32_rip(16#20))}}),
    [].

dotest() -> dotest1(group_leader()).	% stdout == group_leader

dotest(File) ->
    {ok,OS} = file:open(File, [write]),
    dotest1(OS),
    file:close(OS).
-endif.
