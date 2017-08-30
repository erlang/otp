%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : bs_des.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Purpose : An implementation of the DES Encryption/Descryption
%%%	      algorithm using Erlang binaries.
%%%
%%% Created : 14 Apr 2004
%%%-------------------------------------------------------------------
-module(bs_des).

-export([encode/2, decode/2, test/0]).

-define(ITERATIONS, 42).	%% for benchmarking use a higher number

test() ->
  Bin = <<1:64>>,
  Size= byte_size(Bin),
  Key = <<4704650607608769871263876:64>>,
  Jumbled = run_encode(?ITERATIONS, Bin, Key),
  Unjumbled = run_decode(?ITERATIONS, Jumbled, Key),
  <<Bin:Size/binary,_/binary>> = Unjumbled,
  ok.

run_encode(1, Bin, Key) ->
  encode(Bin, Key);
run_encode(N, Bin, Key) ->
  encode(Bin, Key),
  run_encode(N-1, Bin, Key).

run_decode(1, Bin, Key) ->
  decode(Bin, Key);
run_decode(N, Bin, Key) ->
  decode(Bin, Key),
  run_decode(N-1, Bin, Key).

encode(Data, Key) ->
  Keys = schedule(Key),
  list_to_binary(encode_data(Data, Keys)).

decode(Data, Key) ->
  Keys = lists:reverse(schedule(Key)),
  list_to_binary(decode_data(Data, Keys)).

encode_data(<<Data:8/binary, Rest/binary>>, Keys) ->
  [ipinv(des_core(ip(Data), Keys))|encode_data(Rest, Keys)];
encode_data(<<Rest/binary>>, Keys) ->
  case byte_size(Rest) of
    0 -> [];
    X ->
      Y = 8 - X,
      Data = <<Rest/binary, 0:Y/integer-unit:8>>,
      [ipinv(des_core(ip(Data), Keys))]
  end.

decode_data(<<Data:8/binary, Rest/binary>>, Keys) ->
  [ipinv(dechiper(ip(Data), Keys))|decode_data(Rest, Keys)];
decode_data(_, _Keys) ->
  [].

schedule(Key) ->
  NewKey = pc1(Key),
  subkeys(NewKey, 1).

subkeys(_Key, 17) ->
  [];
subkeys(Key, N) ->
  TmpKey =
    case rotate(N) of
      1 ->
	<<X1:1, L:27, X2:1, R:27>> = Key,
	<<L:27, X1:1, R:27, X2:1>>;
      2 ->
	<<X1:2, L:26, X2:2, R:26>> = Key,
	<<L:26, X1:2, R:26, X2:2>>
    end,
  [pc2(TmpKey)|subkeys(TmpKey, N+1)].

pc2(<<I1:1, I2:1, I3:1, I4:1, I5:1, I6:1, I7:1, I8:1,
      _I9:1, I10:1, I11:1, I12:1, I13:1, I14:1, I15:1, I16:1,
      I17:1, _I18:1, I19:1, I20:1, I21:1, _I22:1, I23:1, I24:1,
      _I25:1, I26:1, I27:1, I28:1, I29:1, I30:1, I31:1, I32:1,
      I33:1, I34:1, _I35:1, I36:1, I37:1, _I38:1, I39:1, I40:1,
      I41:1, I42:1, _I43:1, I44:1, I45:1, I46:1, I47:1, I48:1,
      I49:1, I50:1, I51:1, I52:1, I53:1, _I54:1, I55:1, I56:1>>) ->
  <<I14:1, I17:1, I11:1, I24:1, I1:1, I5:1, I3:1, I28:1,
    I15:1, I6:1, I21:1, I10:1, I23:1, I19:1, I12:1, I4:1,
    I26:1, I8:1, I16:1, I7:1, I27:1, I20:1, I13:1, I2:1,
    I41:1, I52:1, I31:1, I37:1, I47:1, I55:1, I30:1, I40:1,
    I51:1, I45:1, I33:1, I48:1, I44:1, I49:1, I39:1, I56:1,
    I34:1, I53:1, I46:1, I42:1, I50:1, I36:1, I29:1, I32:1>>.

pc1(<<I1:1, I2:1, I3:1, I4:1, I5:1, I6:1, I7:1, _:1,
      I9:1, I10:1, I11:1, I12:1, I13:1, I14:1, I15:1, _:1,
      I17:1, I18:1, I19:1, I20:1, I21:1, I22:1, I23:1, _:1,
      I25:1, I26:1, I27:1, I28:1, I29:1, I30:1, I31:1, _:1,
      I33:1, I34:1, I35:1, I36:1, I37:1, I38:1, I39:1, _:1,
      I41:1, I42:1, I43:1, I44:1, I45:1, I46:1, I47:1, _:1,
      I49:1, I50:1, I51:1, I52:1, I53:1, I54:1, I55:1, _:1,
      I57:1, I58:1, I59:1, I60:1, I61:1, I62:1, I63:1, _:1>>) ->
  <<I57:1, I49:1, I41:1, I33:1, I25:1, I17:1, I9:1, I1:1,
    I58:1, I50:1, I42:1, I34:1, I26:1, I18:1, I10:1, I2:1,
    I59:1, I51:1, I43:1, I35:1, I27:1, I19:1, I11:1, I3:1,
    I60:1, I52:1, I44:1, I36:1, I63:1, I55:1, I47:1, I39:1,
    I31:1, I23:1, I15:1, I7:1, I62:1, I54:1, I46:1, I38:1,
    I30:1, I22:1, I14:1, I6:1, I61:1, I53:1, I45:1, I37:1,
    I29:1, I21:1, I13:1, I5:1, I28:1, I20:1, I12:1, I4:1>>.

ip(<<I1:1, I2:1, I3:1, I4:1, I5:1, I6:1, I7:1, I8:1,
     I9:1, I10:1, I11:1, I12:1, I13:1, I14:1, I15:1, I16:1,
     I17:1, I18:1, I19:1, I20:1, I21:1, I22:1, I23:1, I24:1,
     I25:1, I26:1, I27:1, I28:1, I29:1, I30:1, I31:1, I32:1,
     I33:1, I34:1, I35:1, I36:1, I37:1, I38:1, I39:1, I40:1,
     I41:1, I42:1, I43:1, I44:1, I45:1, I46:1, I47:1, I48:1,
     I49:1, I50:1, I51:1, I52:1, I53:1, I54:1, I55:1, I56:1,
     I57:1, I58:1, I59:1, I60:1, I61:1, I62:1, I63:1, I64:1>>) ->
  <<I58:1, I50:1, I42:1, I34:1, I26:1, I18:1, I10:1, I2:1,
    I60:1, I52:1, I44:1, I36:1, I28:1, I20:1, I12:1, I4:1,
    I62:1, I54:1, I46:1, I38:1, I30:1, I22:1, I14:1, I6:1,
    I64:1, I56:1, I48:1, I40:1, I32:1, I24:1, I16:1, I8:1,
    I57:1, I49:1, I41:1, I33:1, I25:1, I17:1, I9:1, I1:1,
    I59:1, I51:1, I43:1, I35:1, I27:1, I19:1, I11:1, I3:1,
    I61:1, I53:1, I45:1, I37:1, I29:1, I21:1, I13:1, I5:1,
    I63:1, I55:1, I47:1, I39:1, I31:1, I23:1, I15:1, I7:1>>.

ipinv(<<I58:1, I50:1, I42:1, I34:1, I26:1, I18:1, I10:1, I2:1,
	I60:1, I52:1, I44:1, I36:1, I28:1, I20:1, I12:1, I4:1,
	I62:1, I54:1, I46:1, I38:1, I30:1, I22:1, I14:1, I6:1,
	I64:1, I56:1, I48:1, I40:1, I32:1, I24:1, I16:1, I8:1,
	I57:1, I49:1, I41:1, I33:1, I25:1, I17:1, I9:1, I1:1,
	I59:1, I51:1, I43:1, I35:1, I27:1, I19:1, I11:1, I3:1,
	I61:1, I53:1, I45:1, I37:1, I29:1, I21:1, I13:1, I5:1,
       I63:1, I55:1, I47:1, I39:1, I31:1, I23:1, I15:1, I7:1>>) ->
  <<I1:1, I2:1, I3:1, I4:1, I5:1, I6:1, I7:1, I8:1,
    I9:1, I10:1, I11:1, I12:1, I13:1, I14:1, I15:1, I16:1,
    I17:1, I18:1, I19:1, I20:1, I21:1, I22:1, I23:1, I24:1,
    I25:1, I26:1, I27:1, I28:1, I29:1, I30:1, I31:1, I32:1,
    I33:1, I34:1, I35:1, I36:1, I37:1, I38:1, I39:1, I40:1,
    I41:1, I42:1, I43:1, I44:1, I45:1, I46:1, I47:1, I48:1,
    I49:1, I50:1, I51:1, I52:1, I53:1, I54:1, I55:1, I56:1,
    I57:1, I58:1, I59:1, I60:1, I61:1, I62:1, I63:1, I64:1>>.

dechiper(<<L:4/binary, R:4/binary>>, Keys) ->
  dechiper(L, R, Keys, 16).

dechiper(L, R, [], 0) ->
  <<L:4/binary, R:4/binary>>;
dechiper(L, R, [Key|Rest], I) ->
  NewL = ebit(L),
  XorL = xor48(NewL, Key),
  Sboxed = sboxing(XorL),
  Ped = p(Sboxed),
  EndL = xor32(Ped, R),
  dechiper(EndL, L, Rest, I-1).

des_core(<<L:4/binary, R:4/binary>>, Keys) ->
  des_core(L, R, Keys, 0).

des_core(L, R, [], 16) ->
  <<L:4/binary, R:4/binary>>;
des_core(L, R, [Key|Rest], I) when I<16 ->
  NewR = ebit(R),
  XorR = xor48(NewR, Key),
  Sboxed = sboxing(XorR),
  Ped = p(Sboxed),
  EndR = xor32(Ped, L),
  des_core(R, EndR, Rest, I+1).

ebit(<<I1:1, I2:2, I3:2,I4:2,I5:2,I6:2,
       I7:2,I8:2,I9:2,I10:2,I11:2,I12:2,
       I13:2,I14:2,I15:2,I16:2,I17:1>>) ->
  <<I17:1, I1:1, I2:2, I3:2, I3:2,
    I4:2, I5:2, I5:2, I6:2,
    I7:2, I7:2, I8:2, I9:2,
    I9:2, I10:2, I11:2, I11:2,
    I12:2, I13:2, I13:2, I14:2,
    I15:2, I15:2, I16:2, I17:1, I1:1>>.

p(<<I1:1, I2:1, I3:1, I4:1, I5:1, I6:1, I7:1, I8:1,
    I9:1, I10:1, I11:1, I12:1, I13:1, I14:1, I15:1, I16:1,
    I17:1, I18:1, I19:1, I20:1, I21:1, I22:1, I23:1, I24:1,
    I25:1, I26:1, I27:1, I28:1, I29:1, I30:1, I31:1, I32:1>>) ->
  <<I16:1, I7:1, I20:1, I21:1, I29:1, I12:1, I28:1, I17:1,
    I1:1, I15:1, I23:1, I26:1, I5:1, I18:1, I31:1, I10:1,
    I2:1, I8:1, I24:1, I14:1, I32:1, I27:1, I3:1, I9:1,
    I19:1, I13:1, I30:1, I6:1, I22:1, I11:1, I4:1, I25:1>>.

rotate(1) -> 1;
rotate(2) -> 1;
rotate(9) -> 1;
rotate(16) -> 1;
rotate(N) when N>0, N<17 -> 2.

%% xor64(<<I1:16, I2:16, I3:16, I4:16>>,<<J1:16, J2:16, J3:16, J4:16>>) ->
%%   K1 = I1 bxor J1,
%%   K2 = I2 bxor J2,
%%   K3 = I3 bxor J3,
%%   K4 = I4 bxor J4,
%%   <<K1:16, K2:16, K3:16, K4:16>>.

xor48(<<I1:16, I2:16, I3:16>>,<<J1:16, J2:16, J3:16>>) ->
  K1 = I1 bxor J1,
  K2 = I2 bxor J2,
  K3 = I3 bxor J3,
  <<K1:16, K2:16, K3:16>>.

xor32(<<I1:16, I2:16>>,<<J1:16, J2:16>>) ->
  K1 = I1 bxor J1,
  K2 = I2 bxor J2,
  <<K1:16, K2:16>>.

sboxing(<<A1:6, A2:6, A3:6, A4:6, A5:6, A6:6, A7:6, A8:6>>) ->
  S1 = sbox(A1, 1),
  S2 = sbox(A2, 2),
  S3 = sbox(A3, 3),
  S4 = sbox(A4, 4),
  S5 = sbox(A5, 5),
  S6 = sbox(A6, 6),
  S7 = sbox(A7, 7),
  S8 = sbox(A8, 8),
  <<S1:4,S2:4,S3:4,S4:4,S5:4,S6:4,S7:4,S8:4>>.

sbox(0,1) -> 14;
sbox(1,1) -> 0;
sbox(2,1) -> 4;
sbox(3,1) -> 15;
sbox(4,1) -> 13;
sbox(5,1) -> 7;
sbox(6,1) -> 1;
sbox(7,1) -> 4;
sbox(8,1) -> 2;
sbox(9,1) -> 14;
sbox(10,1) -> 15;
sbox(11,1) -> 2;
sbox(12,1) -> 11;
sbox(13,1) -> 13;
sbox(14,1) -> 8;
sbox(15,1) -> 1;
sbox(16,1) -> 3;
sbox(17,1) -> 10;
sbox(18,1) -> 10;
sbox(19,1) -> 6;
sbox(20,1) -> 6;
sbox(21,1) -> 12;
sbox(22,1) -> 12;
sbox(23,1) -> 11;
sbox(24,1) -> 5;
sbox(25,1) -> 9;
sbox(26,1) -> 9;
sbox(27,1) -> 5;
sbox(28,1) -> 0;
sbox(29,1) -> 3;
sbox(30,1) -> 7;
sbox(31,1) -> 8;
sbox(32,1) -> 4;
sbox(33,1) -> 15;
sbox(34,1) -> 1;
sbox(35,1) -> 12;
sbox(36,1) -> 14;
sbox(37,1) -> 8;
sbox(38,1) -> 8;
sbox(39,1) -> 2;
sbox(40,1) -> 13;
sbox(41,1) -> 4;
sbox(42,1) -> 6;
sbox(43,1) -> 9;
sbox(44,1) -> 2;
sbox(45,1) -> 1;
sbox(46,1) -> 11;
sbox(47,1) -> 7;
sbox(48,1) -> 15;
sbox(49,1) -> 5;
sbox(50,1) -> 12;
sbox(51,1) -> 11;
sbox(52,1) -> 9;
sbox(53,1) -> 3;
sbox(54,1) -> 7;
sbox(55,1) -> 14;
sbox(56,1) -> 3;
sbox(57,1) -> 10;
sbox(58,1) -> 10;
sbox(59,1) -> 0;
sbox(60,1) -> 5;
sbox(61,1) -> 6;
sbox(62,1) -> 0;
sbox(63,1) -> 13;
sbox(0,2) -> 15;
sbox(1,2) -> 3;
sbox(2,2) -> 1;
sbox(3,2) -> 13;
sbox(4,2) -> 8;
sbox(5,2) -> 4;
sbox(6,2) -> 14;
sbox(7,2) -> 7;
sbox(8,2) -> 6;
sbox(9,2) -> 15;
sbox(10,2) -> 11;
sbox(11,2) -> 2;
sbox(12,2) -> 3;
sbox(13,2) -> 8;
sbox(14,2) -> 4;
sbox(15,2) -> 14;
sbox(16,2) -> 9;
sbox(17,2) -> 12;
sbox(18,2) -> 7;
sbox(19,2) -> 0;
sbox(20,2) -> 2;
sbox(21,2) -> 1;
sbox(22,2) -> 13;
sbox(23,2) -> 10;
sbox(24,2) -> 12;
sbox(25,2) -> 6;
sbox(26,2) -> 0;
sbox(27,2) -> 9;
sbox(28,2) -> 5;
sbox(29,2) -> 11;
sbox(30,2) -> 10;
sbox(31,2) -> 5;
sbox(32,2) -> 0;
sbox(33,2) -> 13;
sbox(34,2) -> 14;
sbox(35,2) -> 8;
sbox(36,2) -> 7;
sbox(37,2) -> 10;
sbox(38,2) -> 11;
sbox(39,2) -> 1;
sbox(40,2) -> 10;
sbox(41,2) -> 3;
sbox(42,2) -> 4;
sbox(43,2) -> 15;
sbox(44,2) -> 13;
sbox(45,2) -> 4;
sbox(46,2) -> 1;
sbox(47,2) -> 2;
sbox(48,2) -> 5;
sbox(49,2) -> 11;
sbox(50,2) -> 8;
sbox(51,2) -> 6;
sbox(52,2) -> 12;
sbox(53,2) -> 7;
sbox(54,2) -> 6;
sbox(55,2) -> 12;
sbox(56,2) -> 9;
sbox(57,2) -> 0;
sbox(58,2) -> 3;
sbox(59,2) -> 5;
sbox(60,2) -> 2;
sbox(61,2) -> 14;
sbox(62,2) -> 15;
sbox(63,2) -> 9;
sbox(0,3) -> 10;
sbox(1,3) -> 13;
sbox(2,3) -> 0;
sbox(3,3) -> 7;
sbox(4,3) -> 9;
sbox(5,3) -> 0;
sbox(6,3) -> 14;
sbox(7,3) -> 9;
sbox(8,3) -> 6;
sbox(9,3) -> 3;
sbox(10,3) -> 3;
sbox(11,3) -> 4;
sbox(12,3) -> 15;
sbox(13,3) -> 6;
sbox(14,3) -> 5;
sbox(15,3) -> 10;
sbox(16,3) -> 1;
sbox(17,3) -> 2;
sbox(18,3) -> 13;
sbox(19,3) -> 8;
sbox(20,3) -> 12;
sbox(21,3) -> 5;
sbox(22,3) -> 7;
sbox(23,3) -> 14;
sbox(24,3) -> 11;
sbox(25,3) -> 12;
sbox(26,3) -> 4;
sbox(27,3) -> 11;
sbox(28,3) -> 2;
sbox(29,3) -> 15;
sbox(30,3) -> 8;
sbox(31,3) -> 1;
sbox(32,3) -> 13;
sbox(33,3) -> 1;
sbox(34,3) -> 6;
sbox(35,3) -> 10;
sbox(36,3) -> 4;
sbox(37,3) -> 13;
sbox(38,3) -> 9;
sbox(39,3) -> 0;
sbox(40,3) -> 8;
sbox(41,3) -> 6;
sbox(42,3) -> 15;
sbox(43,3) -> 9;
sbox(44,3) -> 3;
sbox(45,3) -> 8;
sbox(46,3) -> 0;
sbox(47,3) -> 7;
sbox(48,3) -> 11;
sbox(49,3) -> 4;
sbox(50,3) -> 1;
sbox(51,3) -> 15;
sbox(52,3) -> 2;
sbox(53,3) -> 14;
sbox(54,3) -> 12;
sbox(55,3) -> 3;
sbox(56,3) -> 5;
sbox(57,3) -> 11;
sbox(58,3) -> 10;
sbox(59,3) -> 5;
sbox(60,3) -> 14;
sbox(61,3) -> 2;
sbox(62,3) -> 7;
sbox(63,3) -> 12;
sbox(0,4) -> 7;
sbox(1,4) -> 13;
sbox(2,4) -> 13;
sbox(3,4) -> 8;
sbox(4,4) -> 14;
sbox(5,4) -> 11;
sbox(6,4) -> 3;
sbox(7,4) -> 5;
sbox(8,4) -> 0;
sbox(9,4) -> 6;
sbox(10,4) -> 6;
sbox(11,4) -> 15;
sbox(12,4) -> 9;
sbox(13,4) -> 0;
sbox(14,4) -> 10;
sbox(15,4) -> 3;
sbox(16,4) -> 1;
sbox(17,4) -> 4;
sbox(18,4) -> 2;
sbox(19,4) -> 7;
sbox(20,4) -> 8;
sbox(21,4) -> 2;
sbox(22,4) -> 5;
sbox(23,4) -> 12;
sbox(24,4) -> 11;
sbox(25,4) -> 1;
sbox(26,4) -> 12;
sbox(27,4) -> 10;
sbox(28,4) -> 4;
sbox(29,4) -> 14;
sbox(30,4) -> 15;
sbox(31,4) -> 9;
sbox(32,4) -> 10;
sbox(33,4) -> 3;
sbox(34,4) -> 6;
sbox(35,4) -> 15;
sbox(36,4) -> 9;
sbox(37,4) -> 0;
sbox(38,4) -> 0;
sbox(39,4) -> 6;
sbox(40,4) -> 12;
sbox(41,4) -> 10;
sbox(42,4) -> 11;
sbox(43,4) -> 1;
sbox(44,4) -> 7;
sbox(45,4) -> 13;
sbox(46,4) -> 13;
sbox(47,4) -> 8;
sbox(48,4) -> 15;
sbox(49,4) -> 9;
sbox(50,4) -> 1;
sbox(51,4) -> 4;
sbox(52,4) -> 3;
sbox(53,4) -> 5;
sbox(54,4) -> 14;
sbox(55,4) -> 11;
sbox(56,4) -> 5;
sbox(57,4) -> 12;
sbox(58,4) -> 2;
sbox(59,4) -> 7;
sbox(60,4) -> 8;
sbox(61,4) -> 2;
sbox(62,4) -> 4;
sbox(63,4) -> 14;
sbox(0,5) -> 2;
sbox(1,5) -> 14;
sbox(2,5) -> 12;
sbox(3,5) -> 11;
sbox(4,5) -> 4;
sbox(5,5) -> 2;
sbox(6,5) -> 1;
sbox(7,5) -> 12;
sbox(8,5) -> 7;
sbox(9,5) -> 4;
sbox(10,5) -> 10;
sbox(11,5) -> 7;
sbox(12,5) -> 11;
sbox(13,5) -> 13;
sbox(14,5) -> 6;
sbox(15,5) -> 1;
sbox(16,5) -> 8;
sbox(17,5) -> 5;
sbox(18,5) -> 5;
sbox(19,5) -> 0;
sbox(20,5) -> 3;
sbox(21,5) -> 15;
sbox(22,5) -> 15;
sbox(23,5) -> 10;
sbox(24,5) -> 13;
sbox(25,5) -> 3;
sbox(26,5) -> 0;
sbox(27,5) -> 9;
sbox(28,5) -> 14;
sbox(29,5) -> 8;
sbox(30,5) -> 9;
sbox(31,5) -> 6;
sbox(32,5) -> 4;
sbox(33,5) -> 11;
sbox(34,5) -> 2;
sbox(35,5) -> 8;
sbox(36,5) -> 1;
sbox(37,5) -> 12;
sbox(38,5) -> 11;
sbox(39,5) -> 7;
sbox(40,5) -> 10;
sbox(41,5) -> 1;
sbox(42,5) -> 13;
sbox(43,5) -> 14;
sbox(44,5) -> 7;
sbox(45,5) -> 2;
sbox(46,5) -> 8;
sbox(47,5) -> 13;
sbox(48,5) -> 15;
sbox(49,5) -> 6;
sbox(50,5) -> 9;
sbox(51,5) -> 15;
sbox(52,5) -> 12;
sbox(53,5) -> 0;
sbox(54,5) -> 5;
sbox(55,5) -> 9;
sbox(56,5) -> 6;
sbox(57,5) -> 10;
sbox(58,5) -> 3;
sbox(59,5) -> 4;
sbox(60,5) -> 0;
sbox(61,5) -> 5;
sbox(62,5) -> 14;
sbox(63,5) -> 3;
sbox(0,6) -> 12;
sbox(1,6) -> 10;
sbox(2,6) -> 1;
sbox(3,6) -> 15;
sbox(4,6) -> 10;
sbox(5,6) -> 4;
sbox(6,6) -> 15;
sbox(7,6) -> 2;
sbox(8,6) -> 9;
sbox(9,6) -> 7;
sbox(10,6) -> 2;
sbox(11,6) -> 12;
sbox(12,6) -> 6;
sbox(13,6) -> 9;
sbox(14,6) -> 8;
sbox(15,6) -> 5;
sbox(16,6) -> 0;
sbox(17,6) -> 6;
sbox(18,6) -> 13;
sbox(19,6) -> 1;
sbox(20,6) -> 3;
sbox(21,6) -> 13;
sbox(22,6) -> 4;
sbox(23,6) -> 14;
sbox(24,6) -> 14;
sbox(25,6) -> 0;
sbox(26,6) -> 7;
sbox(27,6) -> 11;
sbox(28,6) -> 5;
sbox(29,6) -> 3;
sbox(30,6) -> 11;
sbox(31,6) -> 8;
sbox(32,6) -> 9;
sbox(33,6) -> 4;
sbox(34,6) -> 14;
sbox(35,6) -> 3;
sbox(36,6) -> 15;
sbox(37,6) -> 2;
sbox(38,6) -> 5;
sbox(39,6) -> 12;
sbox(40,6) -> 2;
sbox(41,6) -> 9;
sbox(42,6) -> 8;
sbox(43,6) -> 5;
sbox(44,6) -> 12;
sbox(45,6) -> 15;
sbox(46,6) -> 3;
sbox(47,6) -> 10;
sbox(48,6) -> 7;
sbox(49,6) -> 11;
sbox(50,6) -> 0;
sbox(51,6) -> 14;
sbox(52,6) -> 4;
sbox(53,6) -> 1;
sbox(54,6) -> 10;
sbox(55,6) -> 7;
sbox(56,6) -> 1;
sbox(57,6) -> 6;
sbox(58,6) -> 13;
sbox(59,6) -> 0;
sbox(60,6) -> 11;
sbox(61,6) -> 8;
sbox(62,6) -> 6;
sbox(63,6) -> 13;
sbox(0,7) -> 4;
sbox(1,7) -> 13;
sbox(2,7) -> 11;
sbox(3,7) -> 0;
sbox(4,7) -> 2;
sbox(5,7) -> 11;
sbox(6,7) -> 14;
sbox(7,7) -> 7;
sbox(8,7) -> 15;
sbox(9,7) -> 4;
sbox(10,7) -> 0;
sbox(11,7) -> 9;
sbox(12,7) -> 8;
sbox(13,7) -> 1;
sbox(14,7) -> 13;
sbox(15,7) -> 10;
sbox(16,7) -> 3;
sbox(17,7) -> 14;
sbox(18,7) -> 12;
sbox(19,7) -> 3;
sbox(20,7) -> 9;
sbox(21,7) -> 5;
sbox(22,7) -> 7;
sbox(23,7) -> 12;
sbox(24,7) -> 5;
sbox(25,7) -> 2;
sbox(26,7) -> 10;
sbox(27,7) -> 15;
sbox(28,7) -> 6;
sbox(29,7) -> 8;
sbox(30,7) -> 1;
sbox(31,7) -> 6;
sbox(32,7) -> 1;
sbox(33,7) -> 6;
sbox(34,7) -> 4;
sbox(35,7) -> 11;
sbox(36,7) -> 11;
sbox(37,7) -> 13;
sbox(38,7) -> 13;
sbox(39,7) -> 8;
sbox(40,7) -> 12;
sbox(41,7) -> 1;
sbox(42,7) -> 3;
sbox(43,7) -> 4;
sbox(44,7) -> 7;
sbox(45,7) -> 10;
sbox(46,7) -> 14;
sbox(47,7) -> 7;
sbox(48,7) -> 10;
sbox(49,7) -> 9;
sbox(50,7) -> 15;
sbox(51,7) -> 5;
sbox(52,7) -> 6;
sbox(53,7) -> 0;
sbox(54,7) -> 8;
sbox(55,7) -> 15;
sbox(56,7) -> 0;
sbox(57,7) -> 14;
sbox(58,7) -> 5;
sbox(59,7) -> 2;
sbox(60,7) -> 9;
sbox(61,7) -> 3;
sbox(62,7) -> 2;
sbox(63,7) -> 12;
sbox(0,8) -> 13;
sbox(1,8) -> 1;
sbox(2,8) -> 2;
sbox(3,8) -> 15;
sbox(4,8) -> 8;
sbox(5,8) -> 13;
sbox(6,8) -> 4;
sbox(7,8) -> 8;
sbox(8,8) -> 6;
sbox(9,8) -> 10;
sbox(10,8) -> 15;
sbox(11,8) -> 3;
sbox(12,8) -> 11;
sbox(13,8) -> 7;
sbox(14,8) -> 1;
sbox(15,8) -> 4;
sbox(16,8) -> 10;
sbox(17,8) -> 12;
sbox(18,8) -> 9;
sbox(19,8) -> 5;
sbox(20,8) -> 3;
sbox(21,8) -> 6;
sbox(22,8) -> 14;
sbox(23,8) -> 11;
sbox(24,8) -> 5;
sbox(25,8) -> 0;
sbox(26,8) -> 0;
sbox(27,8) -> 14;
sbox(28,8) -> 12;
sbox(29,8) -> 9;
sbox(30,8) -> 7;
sbox(31,8) -> 2;
sbox(32,8) -> 7;
sbox(33,8) -> 2;
sbox(34,8) -> 11;
sbox(35,8) -> 1;
sbox(36,8) -> 4;
sbox(37,8) -> 14;
sbox(38,8) -> 1;
sbox(39,8) -> 7;
sbox(40,8) -> 9;
sbox(41,8) -> 4;
sbox(42,8) -> 12;
sbox(43,8) -> 10;
sbox(44,8) -> 14;
sbox(45,8) -> 8;
sbox(46,8) -> 2;
sbox(47,8) -> 13;
sbox(48,8) -> 0;
sbox(49,8) -> 15;
sbox(50,8) -> 6;
sbox(51,8) -> 12;
sbox(52,8) -> 10;
sbox(53,8) -> 9;
sbox(54,8) -> 13;
sbox(55,8) -> 0;
sbox(56,8) -> 15;
sbox(57,8) -> 3;
sbox(58,8) -> 3;
sbox(59,8) -> 5;
sbox(60,8) -> 5;
sbox(61,8) -> 6;
sbox(62,8) -> 8;
sbox(63,8) -> 11.
