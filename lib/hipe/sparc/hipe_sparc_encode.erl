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
%%% Encode symbolic SPARC instructions to binary form.
%%% Copyright (C) 2007-2008  Mikael Pettersson

-module(hipe_sparc_encode).

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
%%% Instruction Formats
%%%

format1(Disp30) ->
  ?BIT(30,1) bor ?BF(29,0,Disp30).

format2a(Rd, Op2, Imm22) ->
  ?BF(29,25,Rd) bor ?BF(24,22,Op2) bor ?BF(21,0,Imm22).

format2b(A, Cond, Op2, Disp22) ->
  ?BIT(29,A) bor ?BF(28,25,Cond) bor ?BF(24,22,Op2) bor ?BF(21,0,Disp22).

format2c(A, Cond, Op2, CC1, CC0, P, Disp19) ->
  ?BIT(29,A) bor ?BF(28,25,Cond) bor ?BF(24,22,Op2) bor ?BIT(21,CC1)
  bor ?BIT(20,CC0) bor ?BIT(19,P) bor ?BF(18,0,Disp19).

format2d(A, RCond, Op2, P, Rs1, Disp16) ->
  D16Hi = Disp16 bsr 14,
  D16Lo = Disp16 band 16#3FFF,
  ?BIT(29,A) bor ?BF(27,25,RCond) bor ?BF(24,22,Op2) bor ?BF(21,20,D16Hi)
  bor ?BIT(19,P) bor ?BF(18,14,Rs1) bor ?BF(13,0,D16Lo).

format3common(Op, Rd, Op3, Rs1) ->	% format 3, bits 31..14
  ?BF(31,30,Op) bor ?BF(29,25,Rd) bor ?BF(24,19,Op3) bor ?BF(18,14,Rs1).

format3a(Op, Rd, Op3, Rs1, Rs2) ->
  format3common(Op, Rd, Op3, Rs1) bor ?BF(4,0,Rs2).

format3ax(Op, Rd, Op3, Rs1, Rs2) ->
  format3a(Op, Rd, Op3, Rs1, Rs2) bor ?BIT(12,1).

format3b(Op, Rd, Op3, Rs1, Simm13) ->
  format3common(Op, Rd, Op3, Rs1) bor ?BIT(13,1) bor ?BF(12,0,Simm13).

format3b32(Op, Rd, Op3, Rs1, Shcnt32) ->
  format3a(Op, Rd, Op3, Rs1, Shcnt32) bor ?BIT(13,1).

format3b64(Op, Rd, Op3, Rs1, Shcnt64) ->
  format3common(Op, Rd, Op3, Rs1) bor ?BIT(13,1) bor ?BF(5,0,Shcnt64).

format3ab(Op, {r,Rd}, Op3, {r,Rs1}, Src2) ->
  case Src2 of
    {r,Rs2} ->
      format3a(Op, Rd, Op3, Rs1, Rs2);
    {simm13,Simm13} ->
      format3b(Op, Rd, Op3, Rs1, Simm13)
  end.

format3ab({Rs1,Src2,Rd}, Op3, Op) -> format3ab(Op, Rd, Op3, Rs1, Src2).

-ifdef(notdef).
format3c(Op, Rd, Op3, Rs1, Opf, Rs2) ->
  format3h(Op, Rd, Op3, Rs1) bor (Opf bsl 5) bor Rs2.

format3d(Op, Rd, Op3, Rs1, I, Rs2) ->
  format3h(Op, Rd, Op3, Rs1) bor (I bsl 13) bor Rs2.
-endif.

%%%
%%% Instruction Operands
%%%

'cond'(Cond) ->
  case Cond of
    'n'		-> 2#0000;
    'e'		-> 2#0001;
    'le'	-> 2#0010;
    'l'		-> 2#0011;
    'leu'	-> 2#0100;
    'lu'	-> 2#0101;	% a.k.a. 'cs'
    'neg'	-> 2#0110;
    'vs'	-> 2#0111;
    'a'		-> 2#1000;
    'ne'	-> 2#1001;
    'g'		-> 2#1010;
    'ge'	-> 2#1011;
    'gu'	-> 2#1100;
    'geu'	-> 2#1101;	% a.k.a. 'cc'
    'pos'	-> 2#1110;
    'vc'	-> 2#1111
  end.

rcond(RCond) ->
  case RCond of
    'z'		-> 2#001;
    'lez'	-> 2#010;
    'lz'	-> 2#011;
    'nz'	-> 2#101;
    'gz'	-> 2#110;
    'gez'	-> 2#111
  end.

pred(Pred) ->
  case Pred of
    'pt'	-> 1;
    'pn'	-> 0
  end.

%%%
%%% Branch Instructions
%%%

call({disp30,Disp30}) ->
  format1(Disp30).

ba({disp22,Disp22}) ->	% V7 Bicc, only used for unconditional branches
  format2b(0, 'cond'('a'), 2#010, Disp22).

bp({{'cond',Cond},{pred,Pred},{disp19,Disp19}}) ->
  %% XXX: sparc64 will need CC1=1 here
  format2c(0, 'cond'(Cond), 2#001, 0, 0, pred(Pred), Disp19).

br({{rcond,RCond},{pred,Pred},{r,Rs1},{disp16,Disp16}}) ->
  format2d(0, rcond(RCond), 2#011, pred(Pred), Rs1, Disp16).

%%%
%%% Integer Arithmetic Instructions
%%%

alu(Opnds, Op3) -> format3ab(Opnds, Op3, 2#10).

add(Opnds)	-> alu(Opnds, 2#000000).
addcc(Opnds)	-> alu(Opnds, 2#010000).
%%addc(Opnds)	-> alu(Opnds, 2#001000).
%%addccc(Opnds)	-> alu(Opnds, 2#011000).

sub(Opnds)	-> alu(Opnds, 2#000100).
subcc(Opnds)	-> alu(Opnds, 2#010100).
%%subc(Opnds)	-> alu(Opnds, 2#001100). % XXX: hipe_sparc_op has bug here
%%subccc(Opnds)	-> alu(Opnds, 2#011100). % XXX: hipe_sparc_op has bug here

%%taddcc(Opnds)	-> alu(Opnds, 2#100000).
%%taddcctv(Opnds)	-> alu(Opnds, 2#100010).

%%tsubcc(Opnds)	-> alu(Opnds, 2#100001).
%%tsubcctv(Opnds)	-> alu(Opnds, 2#100011).

mulx(Opnds)	-> alu(Opnds, 2#001001).
%%sdivx(Opnds)	-> alu(Opnds, 2#101101).
%%udivx(Opnds)	-> alu(Opnds, 2#001101).

%%umul(Opnds)	-> alu(Opnds, 2#001010).
smul(Opnds)	-> alu(Opnds, 2#001011).
%%umulcc(Opnds)	-> alu(Opnds, 2#011010).
%%smulcc(Opnds)	-> alu(Opnds, 2#011011).

'and'(Opnds)	-> alu(Opnds, 2#000001).
andcc(Opnds)	-> alu(Opnds, 2#010001).
%%andn(Opnds)	-> alu(Opnds, 2#000101).
%%andncc(Opnds)	-> alu(Opnds, 2#010101).

'or'(Opnds)	-> alu(Opnds, 2#000010).
orcc(Opnds)	-> alu(Opnds, 2#010010).
%%orn(Opnds)	-> alu(Opnds, 2#000110).
%%orncc(Opnds)	-> alu(Opnds, 2#010110).

'xor'(Opnds)	-> alu(Opnds, 2#000011).
xorcc(Opnds)	-> alu(Opnds, 2#010011).
%%xnor(Opnds)	-> alu(Opnds, 2#000111).
%%xnorcc(Opnds)	-> alu(Opnds, 2#010111).

shift32({{r,Rs1},Src2,{r,Rd}}, Op3) ->
  case Src2 of
    {r,Rs2} ->
      format3a(2#10, Rd, Op3, Rs1, Rs2);
    {uimm5,Shcnt32} ->
      format3b32(2#10, Rd, Op3, Rs1, Shcnt32)
  end.

shift64({{r,Rs1},Src2,{r,Rd}}, Op3) ->
  case Src2 of
    {r,Rs2} ->
      format3ax(2#10, Rd, Op3, Rs1, Rs2);
    {uimm6,Shcnt64} ->
      format3b64(2#10, Rd, Op3, Rs1, Shcnt64)
  end.

sll(Opnds)	-> shift32(Opnds, 2#100101).
sllx(Opnds)	-> shift64(Opnds, 2#100101).
srl(Opnds)	-> shift32(Opnds, 2#100110).
srlx(Opnds)	-> shift64(Opnds, 2#100110).
sra(Opnds)	-> shift32(Opnds, 2#100111).
srax(Opnds)	-> shift64(Opnds, 2#100111).

jmpl(Opnds)	-> alu(Opnds, 2#111000).

rd({y,{r,Rd}})	-> format3a(2#10, Rd, 2#101000, 0, 0).

sethi({{uimm22,UImm22},{r,Rd}}) -> format2a(Rd, 2#100, UImm22).

ld(Opnds, Op3) -> format3ab(Opnds, Op3, 2#11).

ldsb(Opnds)	-> ld(Opnds, 2#001001).
ldsh(Opnds)	-> ld(Opnds, 2#001010).
ldsw(Opnds)	-> ld(Opnds, 2#001000).
ldub(Opnds)	-> ld(Opnds, 2#000001).
lduh(Opnds)	-> ld(Opnds, 2#000010).
lduw(Opnds)	-> ld(Opnds, 2#000000).
ldx(Opnds)	-> ld(Opnds, 2#001011).
%%ldd(Opnds)	-> ld(Opnds, 2#000011).

st({Rd,Rs1,Src2}, Op3) -> format3ab(2#11, Rd, Op3, Rs1, Src2).

stb(Opnds)	-> st(Opnds, 2#000101).
%%sth(Opnds)	-> st(Opnds, 2#000110).
stw(Opnds)	-> st(Opnds, 2#000100).
stx(Opnds)	-> st(Opnds, 2#001110).
%%std(Opnds)	-> st(Opnds, 2#000111).

%%%
%%% Floating-Point Instructions
%%%

format3f(Rd, Rs1, Opf, Rs2) ->
  format3a(2#10, Rd, 2#110100, Rs1, Rs2) bor ?BF(13,5,Opf).

fpop1binary(Opf, {{fr,Rs1},{fr,Rs2},{fr,Rd}}) ->
  format3f(Rd, Rs1, Opf, Rs2).

faddd(Opnds) ->		fpop1binary(2#001000010, Opnds).
fdivd(Opnds) ->		fpop1binary(2#001001110, Opnds).
fmuld(Opnds) ->		fpop1binary(2#001001010, Opnds).
fsubd(Opnds) ->		fpop1binary(2#001000110, Opnds).

fpop1unary(Opf, {{fr,Rs2},{fr,Rd}}) ->
  format3f(Rd, 0, Opf, Rs2).

fitod(Opnds) ->		fpop1unary(2#011001000, Opnds).
fmovd(Opnds) ->		fpop1unary(2#000000010, Opnds).
fnegd(Opnds) ->		fpop1unary(2#000000110, Opnds).

ldf({{r,Rs1},{simm13,Simm13},{fr,Rd}}) ->
  format3b(2#11, Rd, 2#100000, Rs1, Simm13).

stf({{fr,Rd},{r,Rs1},{simm13,Simm13}}) ->
  format3b(2#11, Rd, 2#100100, Rs1, Simm13).

-ifdef(notdef).
fpop1(Rs1,Opf,Rs2,Rd) ->        format3a(2#10, Rd, 2#110100, Rs1, Opf, Rs2).
%% fpop2(Rs1,Opf,Rs2,Rd) ->        format3a(2#10, Rd, 2#110101, Rs1, Opf, Rs2).

%% fxtos(Rs2, Rd) ->               fpop1(0,2#010000100,Rs2,Rd).
%% fxtod(Rs2, Rd) ->               fpop1(0,2#010001000,Rs2,Rd).
%% fxtoq(Rs2, Rd) ->               fpop1(0,2#010001100,Rs2,Rd).
fitos(Rs2, Rd) ->               fpop1(0,2#011000100,Rs2,Rd).
fitoq(Rs2, Rd) ->               fpop1(0,2#011001100,Rs2,Rd).

%% fstox(Rs2, Rd) ->               fpop1(0,2#010000001,Rs2,Rd).
%% fdtox(Rs2, Rd) ->               fpop1(0,2#010000010,Rs2,Rd).
%% fqtox(Rs2, Rd) ->               fpop1(0,2#010000011,Rs2,Rd).
%% fstoi(Rs2, Rd) ->               fpop1(0,2#011010001,Rs2,Rd).
%% fdtoi(Rs2, Rd) ->               fpop1(0,2#011010010,Rs2,Rd).
%% fqtoi(Rs2, Rd) ->               fpop1(0,2#011010011,Rs2,Rd).
  
%% fstod(Rs2, Rd) ->               fpop1(0,2#011001001,Rs2,Rd).
%% fstoq(Rs2, Rd) ->               fpop1(0,2#011001101,Rs2,Rd).
%% fdtos(Rs2, Rd) ->               fpop1(0,2#011000110,Rs2,Rd).
%% fdtoq(Rs2, Rd) ->               fpop1(0,2#011001110,Rs2,Rd).
%% fqtos(Rs2, Rd) ->               fpop1(0,2#011000111,Rs2,Rd).
%% fqtod(Rs2, Rd) ->               fpop1(0,2#011001011,Rs2,Rd).
  
fmovs(Rs2, Rd) ->               fpop1(0,2#000000001,Rs2,Rd).
fnegs(Rs2, Rd) ->               fpop1(0,2#000000101,Rs2,Rd).
fabss(Rs2, Rd) ->               fpop1(0,2#000001001,Rs2,Rd).
fabsd(Rs2, Rd) ->               fpop1(0,2#000001010,Rs2,Rd).
fmovq(Rs2, Rd) ->               fpop1(0,2#000000011,Rs2,Rd).
fnegq(Rs2, Rd) ->               fpop1(0,2#000000111,Rs2,Rd).
fabsq(Rs2, Rd) ->               fpop1(0,2#000001011,Rs2,Rd).

%% fsqrts(Rs2, Rd) ->              fpop1(0,2#000101001,Rs2,Rd).
%% fsqrtd(Rs2, Rd) ->              fpop1(0,2#000101010,Rs2,Rd).
%% fsqrtq(Rs2, Rd) ->              fpop1(0,2#000101011,Rs2,Rd).

fadds(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001000001,Rs2,Rd).
faddq(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001000011,Rs2,Rd).
fsubs(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001000101,Rs2,Rd).
fsubq(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001000111,Rs2,Rd).

fmuls(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001001001,Rs2,Rd).
fmulq(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001001011,Rs2,Rd).
%% fsmuld(Rs1, Rs2, Rd) ->         fpop1(Rs1,2#001101001,Rs2,Rd).
%% fdmulq(Rs1, Rs2, Rd) ->         fpop1(Rs1,2#001101110,Rs2,Rd).
fdivs(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001001101,Rs2,Rd).
fdivq(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001001111,Rs2,Rd).

%% Uses fcc0
%% fcmps(Rs1, Rs2) ->              fpop2(Rs1,2#001010001,Rs2,0).
%% fcmpd(Rs1, Rs2) ->              fpop2(Rs1,2#001010010,Rs2,0).
%% fcmpq(Rs1, Rs2) ->              fpop2(Rs1,2#001010011,Rs2,0).
%% fcmpes(Rs1, Rs2) ->             fpop2(Rs1,2#001010101,Rs2,0).
%% fcmped(Rs1, Rs2) ->             fpop2(Rs1,2#001010110,Rs2,0).
%% fcmpeq(Rs1, Rs2) ->             fpop2(Rs1,2#001010111,Rs2,0).

%% fcmps(N, Rs1, Rs2) ->           fpcn(N,2#001010001,Rs1,Rs2).
%% fcmpd(N, Rs1, Rs2) ->           fpcn(N,2#001010010,Rs1,Rs2).
%% fcmpq(N, Rs1, Rs2) ->           fpcn(N,2#001010011,Rs1,Rs2).
%% fcmpes(N, Rs1, Rs2) ->          fpcn(N,2#001010101,Rs1,Rs2).
%% fcmped(N, Rs1, Rs2) ->          fpcn(N,2#001010110,Rs1,Rs2).
%% fcmpeq(N, Rs1, Rs2) ->          fpcn(N,2#001010111,Rs1,Rs2).

stfi(Rd, Rs1, Offset) ->        format3b(2#11, Rd, 2#100100, Rs1, Offset).
stdf(Rd, Rs1, Rs2) ->           format3a(2#11, Rd, 2#100111, Rs1, 0, Rs2).
stdfi(Rd, Rs1, Offset) ->       format3b(2#11, Rd, 2#100111, Rs1, Offset).
stqf(Rd, Rs1, Rs2) ->           format3a(2#11, Rd, 2#100110, Rs1, 0, Rs2).
stqfi(Rd, Rs1, Offset) ->       format3b(2#11, Rd, 2#100110, Rs1, Offset).
%% stfsr(Rd, Rs1, Rs2) ->          format3a(2#11, Rd, 2#100101, Rs1, 0, Rs2).
%% stfsri(Rd, Rs1, Offset) ->      format3b(2#11, Rd, 2#100101, Rs1, Offset).

ldfi(Rd, Rs1, Offset) ->        format3b(2#11, Rd, 2#100000, Rs1, Offset).
lddf(Rd, Rs1, Rs2) ->           format3a(2#11, Rd, 2#100011, Rs1, 0, Rs2).
lddfi(Rd, Rs1, Offset) ->       format3b(2#11, Rd, 2#100011, Rs1, Offset).
ldqf(Rd, Rs1, Rs2) ->           format3a(2#11, Rd, 2#100010, Rs1, 0, Rs2).
ldqfi(Rd, Rs1, Offset) ->       format3b(2#11, Rd, 2#100010, Rs1, Offset).
%% ldxfsr(Rs1, Rs2) ->             format3a(2#11,  1, 2#100001, Rs1, 0, Rs2).
%% ldxfsri(Rs1, Offset) ->         format3b(2#11,  1, 2#100001, Rs1, Offset).

%% fpcn(N, Opf, Rs1, Rs2) -> 
%%   case N of
%%     0 -> fpc0(Opf, Rs1, Rs2);
%%     1 -> fpc1(Opf, Rs1, Rs2);
%%     2 -> fpc2(Opf, Rs1, Rs2);
%%     3 -> fpc3(Opf, Rs1, Rs2)
%%   end.
      
%% fpc0(Opf, Rs1, Rs2) ->          format3c(2#10, 2#00000, 2#110101, Rs1, Opf, Rs2).
%% fpc1(Opf, Rs1, Rs2) ->          format3c(2#10, 2#00001, 2#110101, Rs1, Opf, Rs2).
%% fpc2(Opf, Rs1, Rs2) ->          format3c(2#10, 2#00010, 2#110101, Rs1, Opf, Rs2).
%% fpc3(Opf, Rs1, Rs2) ->          format3c(2#10, 2#00011, 2#110101, Rs1, Opf, Rs2).
-endif.	% FP insns

%%%
%%% Main Encode Dispatch
%%%

insn_encode(Op, Opnds) ->
  case Op of
    'add' -> add(Opnds);
    'addcc' -> addcc(Opnds);
    'and' -> 'and'(Opnds);
    'andcc' -> andcc(Opnds);
    'ba' -> ba(Opnds);
    'bp' -> bp(Opnds);
    'br' -> br(Opnds);
    'call' -> call(Opnds);
    'jmpl' -> jmpl(Opnds);
    'ldsb' -> ldsb(Opnds);
    'ldsh' -> ldsh(Opnds);
    'ldsw' -> ldsw(Opnds);
    'ldub' -> ldub(Opnds);
    'lduh' -> lduh(Opnds);
    'lduw' -> lduw(Opnds);
    'ldx' -> ldx(Opnds);
    'mulx' -> mulx(Opnds);
    'or' -> 'or'(Opnds);
    'orcc' -> orcc(Opnds);
    'rd' -> rd(Opnds);
    'sethi' -> sethi(Opnds);
    'sll' -> sll(Opnds);
    'sllx' -> sllx(Opnds);
    'smul' -> smul(Opnds);
    'sra' -> sra(Opnds);
    'srax' -> srax(Opnds);
    'srl' -> srl(Opnds);
    'srlx' -> srlx(Opnds);
    'stb' -> stb(Opnds);
    'stw' -> stw(Opnds);
    'stx' -> stx(Opnds);
    'sub' -> sub(Opnds);
    'subcc' -> subcc(Opnds);
    'xor' -> 'xor'(Opnds);
    'xorcc' -> xorcc(Opnds);
    'faddd' -> faddd(Opnds);
    'fdivd' -> fdivd(Opnds);
    'fmuld' -> fmuld(Opnds);
    'fsubd' -> fsubd(Opnds);
    'fitod' -> fitod(Opnds);
    'fmovd' -> fmovd(Opnds);
    'fnegd' -> fnegd(Opnds);
    'ldf' -> ldf(Opnds);
    'stf' -> stf(Opnds);
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
  [].

dotest() -> dotest1(group_leader()).

dotest(File) ->
  {ok,OS} = file:open(File, [write]),
  dotest1(OS),
  file:close(OS).

-endif.
