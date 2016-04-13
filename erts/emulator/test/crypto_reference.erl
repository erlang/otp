%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%%
%% Reference implementations of crc32, adler32 and md5 in erlang. Used
%% by crypto_SUITE.
%%

-module(crypto_reference).

-export([adler32/1, crc32/1, md5_init/0, md5_update/2, md5_final/1]).
-export([crc32_table/0, reflect8_table/0]).

-define(BASE, 65521).
-define(NMAX, 5552).

-define(AINIT,<<0,0,0,1>>).

adler32(Bin) when is_binary(Bin) ->
    B2 = adler32(Bin,0,1),
    <<Sum:32/big>> = B2,
    Sum;
    
adler32(IoList) ->
    adler32(erlang:iolist_to_binary(IoList)).
    
adler32(<<>>,B,A) ->
    <<B:16/big,A:16/big>>;

adler32(<<CH:8,T/binary>>,B,A) ->
    NewA = (A+CH) rem ?BASE,
    NewB = (B+NewA) rem ?BASE,
    adler32(T,NewB,NewA).

-define(FINAL_XOR_VALUE,16#FFFFFFFF).
-define(INITIAL_REMAINDER,16#FFFFFFFF).

reflect_bin(<<>>,Res) ->
    Res;
reflect_bin(<<X:1,Rest/bitstring>>,BS) ->
    reflect_bin(Rest,<<X:1,BS/bitstring>>).
reflect(Data,8) ->
    reflect8(Data);
reflect(Data,32) ->
    <<A:8,B:8,C:8,D:8>> = <<Data:32>>,
    ND = reflect8(D),
    NC = reflect8(C),
    NB = reflect8(B),
    NA = reflect8(A),
    <<Result:32>> = <<ND:8,NC:8,NB:8,NA:8>>,
    Result;
reflect(Data,Size) ->
    <<NewData:Size>> = reflect_bin(<<Data:Size>>,<<>>),
    NewData.
crc32(<<>>,Remainder) ->
    reflect(Remainder,32) bxor ?FINAL_XOR_VALUE;
crc32(<<CH:8,T/binary>>,Remainder) ->
    Data = reflect(CH,8) bxor (Remainder bsr 24),
    NewRem = crcTab32(Data) bxor ((Remainder bsl 8) band 16#FFFFFFFF),
%    io:format("CH = ~p (~p)~n",[CH,reflect(CH,8)]),
%    io:format("No reflect = ~p~n",[(CH bxor (Remainder bsr 24))]),
%    io:format("Data = ~p, NewRem = ~p~n",[Data,NewRem]),
    crc32(T,NewRem).

crc32(Bin) when is_binary(Bin) ->
    crc32(Bin,?INITIAL_REMAINDER);
crc32(L) ->
    crc32(erlang:iolist_to_binary(L)).

bitmod2(0,Remainder,_Topbit,_Polynomial,_Mask) ->
    %io:format("~p ",[Remainder]),
    Remainder;
bitmod2(N,Remainder,Topbit,Polynomial,Mask) ->
    %io:format("~p ",[Remainder]),
    case (Remainder band Topbit) of
	0 ->
	    bitmod2(N-1,(Remainder bsl 1) band Mask,Topbit,Polynomial,Mask);	 
	_ ->
	    bitmod2(N-1,((Remainder bsl 1) bxor Polynomial) band Mask,Topbit,Polynomial,Mask)
    end.

mask(CrcSize) ->
    16#FFFFFFFF bsr (32 - CrcSize).

calc_crc_table(256,_CrcSize,_Polynomial) ->
    ok;
calc_crc_table(Dividend,CrcSize,Polynomial) ->
    Mask = mask(CrcSize),
    Remainder = (Dividend bsl (CrcSize - 8)) band Mask,
    TopBit = 1 bsl (CrcSize - 1),
    Rem2 = bitmod2(8,Remainder,TopBit,Polynomial,Mask),
    io:format("crcTab~p(~p) -> ~p;~n",[CrcSize,Dividend,Rem2]),
    calc_crc_table(Dividend+1,CrcSize,Polynomial).

crc32_table() ->
   calc_crc_table(0,32,16#04C11DB7).

reflect8_table(256) ->
    ok;
reflect8_table(N) ->
    X = reflect(N,8),
    io:format("reflect8(~p) -> ~p;~n",[N,X]), 
    reflect8_table(N+1).
reflect8_table() ->
    reflect8_table(0).
    
%CRC32 table calculated with crc32_table above
crcTab32(0) -> 0;
crcTab32(1) -> 79764919;
crcTab32(2) -> 159529838;
crcTab32(3) -> 222504665;
crcTab32(4) -> 319059676;
crcTab32(5) -> 398814059;
crcTab32(6) -> 445009330;
crcTab32(7) -> 507990021;
crcTab32(8) -> 638119352;
crcTab32(9) -> 583659535;
crcTab32(10) -> 797628118;
crcTab32(11) -> 726387553;
crcTab32(12) -> 890018660;
crcTab32(13) -> 835552979;
crcTab32(14) -> 1015980042;
crcTab32(15) -> 944750013;
crcTab32(16) -> 1276238704;
crcTab32(17) -> 1221641927;
crcTab32(18) -> 1167319070;
crcTab32(19) -> 1095957929;
crcTab32(20) -> 1595256236;
crcTab32(21) -> 1540665371;
crcTab32(22) -> 1452775106;
crcTab32(23) -> 1381403509;
crcTab32(24) -> 1780037320;
crcTab32(25) -> 1859660671;
crcTab32(26) -> 1671105958;
crcTab32(27) -> 1733955601;
crcTab32(28) -> 2031960084;
crcTab32(29) -> 2111593891;
crcTab32(30) -> 1889500026;
crcTab32(31) -> 1952343757;
crcTab32(32) -> 2552477408;
crcTab32(33) -> 2632100695;
crcTab32(34) -> 2443283854;
crcTab32(35) -> 2506133561;
crcTab32(36) -> 2334638140;
crcTab32(37) -> 2414271883;
crcTab32(38) -> 2191915858;
crcTab32(39) -> 2254759653;
crcTab32(40) -> 3190512472;
crcTab32(41) -> 3135915759;
crcTab32(42) -> 3081330742;
crcTab32(43) -> 3009969537;
crcTab32(44) -> 2905550212;
crcTab32(45) -> 2850959411;
crcTab32(46) -> 2762807018;
crcTab32(47) -> 2691435357;
crcTab32(48) -> 3560074640;
crcTab32(49) -> 3505614887;
crcTab32(50) -> 3719321342;
crcTab32(51) -> 3648080713;
crcTab32(52) -> 3342211916;
crcTab32(53) -> 3287746299;
crcTab32(54) -> 3467911202;
crcTab32(55) -> 3396681109;
crcTab32(56) -> 4063920168;
crcTab32(57) -> 4143685023;
crcTab32(58) -> 4223187782;
crcTab32(59) -> 4286162673;
crcTab32(60) -> 3779000052;
crcTab32(61) -> 3858754371;
crcTab32(62) -> 3904687514;
crcTab32(63) -> 3967668269;
crcTab32(64) -> 881225847;
crcTab32(65) -> 809987520;
crcTab32(66) -> 1023691545;
crcTab32(67) -> 969234094;
crcTab32(68) -> 662832811;
crcTab32(69) -> 591600412;
crcTab32(70) -> 771767749;
crcTab32(71) -> 717299826;
crcTab32(72) -> 311336399;
crcTab32(73) -> 374308984;
crcTab32(74) -> 453813921;
crcTab32(75) -> 533576470;
crcTab32(76) -> 25881363;
crcTab32(77) -> 88864420;
crcTab32(78) -> 134795389;
crcTab32(79) -> 214552010;
crcTab32(80) -> 2023205639;
crcTab32(81) -> 2086057648;
crcTab32(82) -> 1897238633;
crcTab32(83) -> 1976864222;
crcTab32(84) -> 1804852699;
crcTab32(85) -> 1867694188;
crcTab32(86) -> 1645340341;
crcTab32(87) -> 1724971778;
crcTab32(88) -> 1587496639;
crcTab32(89) -> 1516133128;
crcTab32(90) -> 1461550545;
crcTab32(91) -> 1406951526;
crcTab32(92) -> 1302016099;
crcTab32(93) -> 1230646740;
crcTab32(94) -> 1142491917;
crcTab32(95) -> 1087903418;
crcTab32(96) -> 2896545431;
crcTab32(97) -> 2825181984;
crcTab32(98) -> 2770861561;
crcTab32(99) -> 2716262478;
crcTab32(100) -> 3215044683;
crcTab32(101) -> 3143675388;
crcTab32(102) -> 3055782693;
crcTab32(103) -> 3001194130;
crcTab32(104) -> 2326604591;
crcTab32(105) -> 2389456536;
crcTab32(106) -> 2200899649;
crcTab32(107) -> 2280525302;
crcTab32(108) -> 2578013683;
crcTab32(109) -> 2640855108;
crcTab32(110) -> 2418763421;
crcTab32(111) -> 2498394922;
crcTab32(112) -> 3769900519;
crcTab32(113) -> 3832873040;
crcTab32(114) -> 3912640137;
crcTab32(115) -> 3992402750;
crcTab32(116) -> 4088425275;
crcTab32(117) -> 4151408268;
crcTab32(118) -> 4197601365;
crcTab32(119) -> 4277358050;
crcTab32(120) -> 3334271071;
crcTab32(121) -> 3263032808;
crcTab32(122) -> 3476998961;
crcTab32(123) -> 3422541446;
crcTab32(124) -> 3585640067;
crcTab32(125) -> 3514407732;
crcTab32(126) -> 3694837229;
crcTab32(127) -> 3640369242;
crcTab32(128) -> 1762451694;
crcTab32(129) -> 1842216281;
crcTab32(130) -> 1619975040;
crcTab32(131) -> 1682949687;
crcTab32(132) -> 2047383090;
crcTab32(133) -> 2127137669;
crcTab32(134) -> 1938468188;
crcTab32(135) -> 2001449195;
crcTab32(136) -> 1325665622;
crcTab32(137) -> 1271206113;
crcTab32(138) -> 1183200824;
crcTab32(139) -> 1111960463;
crcTab32(140) -> 1543535498;
crcTab32(141) -> 1489069629;
crcTab32(142) -> 1434599652;
crcTab32(143) -> 1363369299;
crcTab32(144) -> 622672798;
crcTab32(145) -> 568075817;
crcTab32(146) -> 748617968;
crcTab32(147) -> 677256519;
crcTab32(148) -> 907627842;
crcTab32(149) -> 853037301;
crcTab32(150) -> 1067152940;
crcTab32(151) -> 995781531;
crcTab32(152) -> 51762726;
crcTab32(153) -> 131386257;
crcTab32(154) -> 177728840;
crcTab32(155) -> 240578815;
crcTab32(156) -> 269590778;
crcTab32(157) -> 349224269;
crcTab32(158) -> 429104020;
crcTab32(159) -> 491947555;
crcTab32(160) -> 4046411278;
crcTab32(161) -> 4126034873;
crcTab32(162) -> 4172115296;
crcTab32(163) -> 4234965207;
crcTab32(164) -> 3794477266;
crcTab32(165) -> 3874110821;
crcTab32(166) -> 3953728444;
crcTab32(167) -> 4016571915;
crcTab32(168) -> 3609705398;
crcTab32(169) -> 3555108353;
crcTab32(170) -> 3735388376;
crcTab32(171) -> 3664026991;
crcTab32(172) -> 3290680682;
crcTab32(173) -> 3236090077;
crcTab32(174) -> 3449943556;
crcTab32(175) -> 3378572211;
crcTab32(176) -> 3174993278;
crcTab32(177) -> 3120533705;
crcTab32(178) -> 3032266256;
crcTab32(179) -> 2961025959;
crcTab32(180) -> 2923101090;
crcTab32(181) -> 2868635157;
crcTab32(182) -> 2813903052;
crcTab32(183) -> 2742672763;
crcTab32(184) -> 2604032198;
crcTab32(185) -> 2683796849;
crcTab32(186) -> 2461293480;
crcTab32(187) -> 2524268063;
crcTab32(188) -> 2284983834;
crcTab32(189) -> 2364738477;
crcTab32(190) -> 2175806836;
crcTab32(191) -> 2238787779;
crcTab32(192) -> 1569362073;
crcTab32(193) -> 1498123566;
crcTab32(194) -> 1409854455;
crcTab32(195) -> 1355396672;
crcTab32(196) -> 1317987909;
crcTab32(197) -> 1246755826;
crcTab32(198) -> 1192025387;
crcTab32(199) -> 1137557660;
crcTab32(200) -> 2072149281;
crcTab32(201) -> 2135122070;
crcTab32(202) -> 1912620623;
crcTab32(203) -> 1992383480;
crcTab32(204) -> 1753615357;
crcTab32(205) -> 1816598090;
crcTab32(206) -> 1627664531;
crcTab32(207) -> 1707420964;
crcTab32(208) -> 295390185;
crcTab32(209) -> 358241886;
crcTab32(210) -> 404320391;
crcTab32(211) -> 483945776;
crcTab32(212) -> 43990325;
crcTab32(213) -> 106832002;
crcTab32(214) -> 186451547;
crcTab32(215) -> 266083308;
crcTab32(216) -> 932423249;
crcTab32(217) -> 861060070;
crcTab32(218) -> 1041341759;
crcTab32(219) -> 986742920;
crcTab32(220) -> 613929101;
crcTab32(221) -> 542559546;
crcTab32(222) -> 756411363;
crcTab32(223) -> 701822548;
crcTab32(224) -> 3316196985;
crcTab32(225) -> 3244833742;
crcTab32(226) -> 3425377559;
crcTab32(227) -> 3370778784;
crcTab32(228) -> 3601682597;
crcTab32(229) -> 3530312978;
crcTab32(230) -> 3744426955;
crcTab32(231) -> 3689838204;
crcTab32(232) -> 3819031489;
crcTab32(233) -> 3881883254;
crcTab32(234) -> 3928223919;
crcTab32(235) -> 4007849240;
crcTab32(236) -> 4037393693;
crcTab32(237) -> 4100235434;
crcTab32(238) -> 4180117107;
crcTab32(239) -> 4259748804;
crcTab32(240) -> 2310601993;
crcTab32(241) -> 2373574846;
crcTab32(242) -> 2151335527;
crcTab32(243) -> 2231098320;
crcTab32(244) -> 2596047829;
crcTab32(245) -> 2659030626;
crcTab32(246) -> 2470359227;
crcTab32(247) -> 2550115596;
crcTab32(248) -> 2947551409;
crcTab32(249) -> 2876312838;
crcTab32(250) -> 2788305887;
crcTab32(251) -> 2733848168;
crcTab32(252) -> 3165939309;
crcTab32(253) -> 3094707162;
crcTab32(254) -> 3040238851;
crcTab32(255) -> 2985771188;
crcTab32(_) -> exit(not_a_byte).

%%
%% Reflect8 table generated with code above crcTab32
%%

reflect8(0) -> 0;
reflect8(1) -> 128;
reflect8(2) -> 64;
reflect8(3) -> 192;
reflect8(4) -> 32;
reflect8(5) -> 160;
reflect8(6) -> 96;
reflect8(7) -> 224;
reflect8(8) -> 16;
reflect8(9) -> 144;
reflect8(10) -> 80;
reflect8(11) -> 208;
reflect8(12) -> 48;
reflect8(13) -> 176;
reflect8(14) -> 112;
reflect8(15) -> 240;
reflect8(16) -> 8;
reflect8(17) -> 136;
reflect8(18) -> 72;
reflect8(19) -> 200;
reflect8(20) -> 40;
reflect8(21) -> 168;
reflect8(22) -> 104;
reflect8(23) -> 232;
reflect8(24) -> 24;
reflect8(25) -> 152;
reflect8(26) -> 88;
reflect8(27) -> 216;
reflect8(28) -> 56;
reflect8(29) -> 184;
reflect8(30) -> 120;
reflect8(31) -> 248;
reflect8(32) -> 4;
reflect8(33) -> 132;
reflect8(34) -> 68;
reflect8(35) -> 196;
reflect8(36) -> 36;
reflect8(37) -> 164;
reflect8(38) -> 100;
reflect8(39) -> 228;
reflect8(40) -> 20;
reflect8(41) -> 148;
reflect8(42) -> 84;
reflect8(43) -> 212;
reflect8(44) -> 52;
reflect8(45) -> 180;
reflect8(46) -> 116;
reflect8(47) -> 244;
reflect8(48) -> 12;
reflect8(49) -> 140;
reflect8(50) -> 76;
reflect8(51) -> 204;
reflect8(52) -> 44;
reflect8(53) -> 172;
reflect8(54) -> 108;
reflect8(55) -> 236;
reflect8(56) -> 28;
reflect8(57) -> 156;
reflect8(58) -> 92;
reflect8(59) -> 220;
reflect8(60) -> 60;
reflect8(61) -> 188;
reflect8(62) -> 124;
reflect8(63) -> 252;
reflect8(64) -> 2;
reflect8(65) -> 130;
reflect8(66) -> 66;
reflect8(67) -> 194;
reflect8(68) -> 34;
reflect8(69) -> 162;
reflect8(70) -> 98;
reflect8(71) -> 226;
reflect8(72) -> 18;
reflect8(73) -> 146;
reflect8(74) -> 82;
reflect8(75) -> 210;
reflect8(76) -> 50;
reflect8(77) -> 178;
reflect8(78) -> 114;
reflect8(79) -> 242;
reflect8(80) -> 10;
reflect8(81) -> 138;
reflect8(82) -> 74;
reflect8(83) -> 202;
reflect8(84) -> 42;
reflect8(85) -> 170;
reflect8(86) -> 106;
reflect8(87) -> 234;
reflect8(88) -> 26;
reflect8(89) -> 154;
reflect8(90) -> 90;
reflect8(91) -> 218;
reflect8(92) -> 58;
reflect8(93) -> 186;
reflect8(94) -> 122;
reflect8(95) -> 250;
reflect8(96) -> 6;
reflect8(97) -> 134;
reflect8(98) -> 70;
reflect8(99) -> 198;
reflect8(100) -> 38;
reflect8(101) -> 166;
reflect8(102) -> 102;
reflect8(103) -> 230;
reflect8(104) -> 22;
reflect8(105) -> 150;
reflect8(106) -> 86;
reflect8(107) -> 214;
reflect8(108) -> 54;
reflect8(109) -> 182;
reflect8(110) -> 118;
reflect8(111) -> 246;
reflect8(112) -> 14;
reflect8(113) -> 142;
reflect8(114) -> 78;
reflect8(115) -> 206;
reflect8(116) -> 46;
reflect8(117) -> 174;
reflect8(118) -> 110;
reflect8(119) -> 238;
reflect8(120) -> 30;
reflect8(121) -> 158;
reflect8(122) -> 94;
reflect8(123) -> 222;
reflect8(124) -> 62;
reflect8(125) -> 190;
reflect8(126) -> 126;
reflect8(127) -> 254;
reflect8(128) -> 1;
reflect8(129) -> 129;
reflect8(130) -> 65;
reflect8(131) -> 193;
reflect8(132) -> 33;
reflect8(133) -> 161;
reflect8(134) -> 97;
reflect8(135) -> 225;
reflect8(136) -> 17;
reflect8(137) -> 145;
reflect8(138) -> 81;
reflect8(139) -> 209;
reflect8(140) -> 49;
reflect8(141) -> 177;
reflect8(142) -> 113;
reflect8(143) -> 241;
reflect8(144) -> 9;
reflect8(145) -> 137;
reflect8(146) -> 73;
reflect8(147) -> 201;
reflect8(148) -> 41;
reflect8(149) -> 169;
reflect8(150) -> 105;
reflect8(151) -> 233;
reflect8(152) -> 25;
reflect8(153) -> 153;
reflect8(154) -> 89;
reflect8(155) -> 217;
reflect8(156) -> 57;
reflect8(157) -> 185;
reflect8(158) -> 121;
reflect8(159) -> 249;
reflect8(160) -> 5;
reflect8(161) -> 133;
reflect8(162) -> 69;
reflect8(163) -> 197;
reflect8(164) -> 37;
reflect8(165) -> 165;
reflect8(166) -> 101;
reflect8(167) -> 229;
reflect8(168) -> 21;
reflect8(169) -> 149;
reflect8(170) -> 85;
reflect8(171) -> 213;
reflect8(172) -> 53;
reflect8(173) -> 181;
reflect8(174) -> 117;
reflect8(175) -> 245;
reflect8(176) -> 13;
reflect8(177) -> 141;
reflect8(178) -> 77;
reflect8(179) -> 205;
reflect8(180) -> 45;
reflect8(181) -> 173;
reflect8(182) -> 109;
reflect8(183) -> 237;
reflect8(184) -> 29;
reflect8(185) -> 157;
reflect8(186) -> 93;
reflect8(187) -> 221;
reflect8(188) -> 61;
reflect8(189) -> 189;
reflect8(190) -> 125;
reflect8(191) -> 253;
reflect8(192) -> 3;
reflect8(193) -> 131;
reflect8(194) -> 67;
reflect8(195) -> 195;
reflect8(196) -> 35;
reflect8(197) -> 163;
reflect8(198) -> 99;
reflect8(199) -> 227;
reflect8(200) -> 19;
reflect8(201) -> 147;
reflect8(202) -> 83;
reflect8(203) -> 211;
reflect8(204) -> 51;
reflect8(205) -> 179;
reflect8(206) -> 115;
reflect8(207) -> 243;
reflect8(208) -> 11;
reflect8(209) -> 139;
reflect8(210) -> 75;
reflect8(211) -> 203;
reflect8(212) -> 43;
reflect8(213) -> 171;
reflect8(214) -> 107;
reflect8(215) -> 235;
reflect8(216) -> 27;
reflect8(217) -> 155;
reflect8(218) -> 91;
reflect8(219) -> 219;
reflect8(220) -> 59;
reflect8(221) -> 187;
reflect8(222) -> 123;
reflect8(223) -> 251;
reflect8(224) -> 7;
reflect8(225) -> 135;
reflect8(226) -> 71;
reflect8(227) -> 199;
reflect8(228) -> 39;
reflect8(229) -> 167;
reflect8(230) -> 103;
reflect8(231) -> 231;
reflect8(232) -> 23;
reflect8(233) -> 151;
reflect8(234) -> 87;
reflect8(235) -> 215;
reflect8(236) -> 55;
reflect8(237) -> 183;
reflect8(238) -> 119;
reflect8(239) -> 247;
reflect8(240) -> 15;
reflect8(241) -> 143;
reflect8(242) -> 79;
reflect8(243) -> 207;
reflect8(244) -> 47;
reflect8(245) -> 175;
reflect8(246) -> 111;
reflect8(247) -> 239;
reflect8(248) -> 31;
reflect8(249) -> 159;
reflect8(250) -> 95;
reflect8(251) -> 223;
reflect8(252) -> 63;
reflect8(253) -> 191;
reflect8(254) -> 127;
reflect8(255) -> 255;
reflect8(_) -> exit(not_a_byte).
    
%%%
%%% Old MD5 implementation by Tony, modified to fit testing
%%%

-record(md5_ctx, 
	{
	 state = { 16#67452301, 16#efcdab89, 16#98badcfe, 16#10325476 },
	 count = 0,    %% number of bits (64 bit)
	 buffer = <<>>    %% input buffer (16 bytes)
	}).

-define(S11, 7).
-define(S12, 12).
-define(S13, 17).
-define(S14, 22).
-define(S21, 5).
-define(S22, 9).
-define(S23, 14).
-define(S24, 20).
-define(S31, 4).
-define(S32, 11).
-define(S33, 16).
-define(S34, 23).
-define(S41, 6).
-define(S42, 10).
-define(S43, 15).
-define(S44, 21).

%% F, G, H and I are basic MD5 functions.

-define(F(X, Y, Z), (((X) band (Y)) bor ((bnot (X)) band (Z)))).
-define(G(X, Y, Z), (((X) band (Z)) bor ((Y) band (bnot (Z))))).
-define(H(X, Y, Z), ((X) bxor (Y) bxor (Z))).
-define(I(X, Y, Z), ((Y) bxor ((X) bor (bnot (Z))))).

-define(U32(X), ((X) band 16#ffffffff)).

-define(ROTATE_LEFT(X,N), rotate_left(X,N)).

%% FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
%% Rotation is separate from addition to prevent recomputation.
%%
-define(FF(A, B, C, D, X, S, AC),
	?ROTATE_LEFT(A + ?F((B), (C), (D)) + (X) + (AC),(S)) + (B)).

-define(GG(A, B, C, D, X, S, AC), 
	?ROTATE_LEFT(A + ?G((B), (C), (D)) + (X) + (AC),(S)) + (B)).

-define( HH(A, B, C, D, X, S, AC), 
	?ROTATE_LEFT(A + ?H((B), (C), (D)) + (X) + (AC),(S)) + (B)).

-define(II(A, B, C, D, X, S, AC),
	?ROTATE_LEFT(A +  ?I((B), (C), (D)) + (X) + (AC),(S)) + (B)).

md5_init() ->
    #md5_ctx {}.

md5_update(CTX, Input) when is_list(Input) ->
    md5_update(CTX,iolist_to_binary(Input));
md5_update(CTX, Input) when is_binary(Input) ->
    Buffer = CTX#md5_ctx.buffer,
    LenI = size(Input),
    Len = LenI + size(Buffer),
    md5_update(<<Buffer/binary,Input/binary>>, Len,CTX#md5_ctx.state,
	   CTX#md5_ctx.count+(LenI bsl 3)).

%%
%% update state, count reflects number of bytes 
%% including bytes in buffer
%%
md5_update(Buf0, Len0, State0, Count) when Len0 >= 64 ->
    {Xs,Buf1} = decode(Buf0, 64),
    State1 = transform(State0, Xs),
    md5_update(Buf1, Len0 - 64, State1, Count);
md5_update(Buf0, _Len0, State0, Count) ->
    #md5_ctx { state = State0, count = Count, buffer = Buf0 }.

%% produce a digest
md5_final(CTX) ->
    %% pad out to a length 56 (we later add a count that makes 64)
    Count = CTX#md5_ctx.count,      %% number of bits
    Index =  (Count bsr 3) rem 64,  %% number of bytes
    PadLen = if Index < 56 ->
		     56 - Index;
		true -> 120 - Index
	     end,
    CTX1 = md5_update(CTX, list_to_binary(padding(PadLen,[]))),
    CTX2 = md5_update(CTX1, list_to_binary(encode([?U32(Count), ?U32(Count bsr 32)]))),
    list_to_binary(encode(tuple_to_list(CTX2#md5_ctx.state))).

%% generate padding info to final    
padding(0,Acc) -> Acc;
padding(1,Acc) -> [16#80 | Acc];
padding(N,Acc) -> padding(N-1, [0 | Acc]).

%% rotate X as 32-bit unsigned left N bits
rotate_left(X, N) ->
    ?U32(X bsl N) bor (?U32(X) bsr (32 - N)).

%%
%% decodes Len number of bytes into 32 bit integers
%% returns {Xs, Tail}
%%
decode(Buf, Len) ->
    decode(Buf, Len, []).

decode(Buf, 0, Acc) -> 
    {lists:reverse(Acc), Buf};
decode(<<A:32/little,Buf/binary>>, N, Acc) ->
    decode(Buf, N-4, [ A | Acc]).

%%
%% Encodes input 32-bit ints into byte buffer output. 
%%
encode(Xs) -> encode(Xs, []).

encode([X | Xs], Acc) ->
    encode(Xs, [(X bsr 24) band 16#ff,
		(X bsr 16) band 16#ff,
		(X bsr 8) band 16#ff,
		X  band 16#ff | Acc]);
encode([], Acc) -> lists:reverse(Acc).

    
transform({A0,B0,C0,D0}, Xs) ->
    [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15] = Xs,

    %% Round 1 
    A1 = ?FF (A0, B0, C0, D0, X0, ?S11, 16#d76aa478),
    D1 = ?FF (D0, A1, B0, C0, X1, ?S12, 16#e8c7b756),
    C1 = ?FF (C0, D1, A1, B0, X2, ?S13, 16#242070db),
    B1 = ?FF (B0, C1, D1, A1, X3, ?S14, 16#c1bdceee),

    A2 = ?FF (A1, B1, C1, D1, X4, ?S11, 16#f57c0faf),
    D2 = ?FF (D1, A2, B1, C1, X5, ?S12, 16#4787c62a),
    C2 = ?FF (C1, D2, A2, B1, X6, ?S13, 16#a8304613),
    B2 = ?FF (B1, C2, D2, A2, X7, ?S14, 16#fd469501),

    A3 = ?FF (A2, B2, C2, D2, X8, ?S11, 16#698098d8),
    D3 = ?FF (D2, A3, B2, C2, X9, ?S12, 16#8b44f7af),
    C3 = ?FF (C2, D3, A3, B2, X10, ?S13, 16#ffff5bb1),
    B3 = ?FF (B2, C3, D3, A3, X11, ?S14, 16#895cd7be),

    A4 = ?FF (A3, B3, C3, D3, X12, ?S11, 16#6b901122),
    D4 = ?FF (D3, A4, B3, C3, X13, ?S12, 16#fd987193),
    C4 = ?FF (C3, D4, A4, B3, X14, ?S13, 16#a679438e),
    B4 = ?FF (B3, C4, D4, A4, X15, ?S14, 16#49b40821),

    %% Round 2 
    A5 = ?GG (A4, B4, C4, D4, X1, ?S21, 16#f61e2562),
    D5 = ?GG (D4, A5, B4, C4, X6, ?S22, 16#c040b340),
    C5 = ?GG (C4, D5, A5, B4, X11, ?S23, 16#265e5a51),
    B5 = ?GG (B4, C5, D5, A5, X0, ?S24, 16#e9b6c7aa),

    A6 = ?GG (A5, B5, C5, D5, X5, ?S21, 16#d62f105d),
    D6 = ?GG (D5, A6, B5, C5, X10, ?S22,  16#2441453),
    C6 = ?GG (C5, D6, A6, B5, X15, ?S23, 16#d8a1e681),
    B6 = ?GG (B5, C6, D6, A6, X4, ?S24, 16#e7d3fbc8),

    A7 = ?GG (A6, B6, C6, D6, X9, ?S21, 16#21e1cde6),
    D7 = ?GG (D6, A7, B6, C6, X14, ?S22, 16#c33707d6),
    C7 = ?GG (C6, D7, A7, B6, X3, ?S23, 16#f4d50d87),
    B7 = ?GG (B6, C7, D7, A7, X8, ?S24, 16#455a14ed),

    A8 = ?GG (A7, B7, C7, D7, X13, ?S21, 16#a9e3e905),
    D8 = ?GG (D7, A8, B7, C7, X2, ?S22, 16#fcefa3f8),
    C8 = ?GG (C7, D8, A8, B7, X7, ?S23, 16#676f02d9),
    B8 = ?GG (B7, C8, D8, A8, X12, ?S24, 16#8d2a4c8a),

 %% Round 3
    A9 = ?HH (A8, B8, C8, D8, X5, ?S31, 16#fffa3942),
    D9 = ?HH (D8, A9, B8, C8, X8, ?S32, 16#8771f681),
    C9 = ?HH (C8, D9, A9, B8, X11, ?S33, 16#6d9d6122),
    B9 = ?HH (B8, C9, D9, A9, X14, ?S34, 16#fde5380c),

    A10 = ?HH (A9, B9, C9, D9, X1, ?S31, 16#a4beea44),
    D10 = ?HH (D9, A10, B9, C9, X4, ?S32, 16#4bdecfa9),
    C10 = ?HH (C9, D10, A10, B9, X7, ?S33, 16#f6bb4b60),
    B10 = ?HH (B9, C10, D10, A10, X10, ?S34, 16#bebfbc70),

    A11 = ?HH (A10, B10, C10, D10, X13, ?S31, 16#289b7ec6),
    D11 = ?HH (D10, A11, B10, C10, X0, ?S32, 16#eaa127fa),
    C11 = ?HH (C10, D11, A11, B10, X3, ?S33, 16#d4ef3085),
    B11 = ?HH (B10, C11, D11, A11, X6, ?S34,  16#4881d05),

    A12 = ?HH (A11, B11, C11, D11, X9, ?S31, 16#d9d4d039),
    D12 = ?HH (D11, A12, B11, C11, X12, ?S32, 16#e6db99e5),
    C12 = ?HH (C11, D12, A12, B11, X15, ?S33, 16#1fa27cf8),
    B12 = ?HH (B11, C12, D12, A12, X2, ?S34, 16#c4ac5665),

 %% Round 4
    A13 = ?II (A12, B12, C12, D12, X0, ?S41, 16#f4292244),
    D13 = ?II (D12, A13, B12, C12, X7, ?S42, 16#432aff97),
    C13 = ?II (C12, D13, A13, B12, X14, ?S43, 16#ab9423a7),
    B13 = ?II (B12, C13, D13, A13, X5, ?S44, 16#fc93a039),

    A14 = ?II (A13, B13, C13, D13, X12, ?S41, 16#655b59c3),
    D14 = ?II (D13, A14, B13, C13, X3, ?S42, 16#8f0ccc92),
    C14 = ?II (C13, D14, A14, B13, X10, ?S43, 16#ffeff47d),
    B14 = ?II (B13, C14, D14, A14, X1, ?S44, 16#85845dd1),

    A15 = ?II (A14, B14, C14, D14, X8, ?S41, 16#6fa87e4f),
    D15 = ?II (D14, A15, B14, C14, X15, ?S42, 16#fe2ce6e0),
    C15 = ?II (C14, D15, A15, B14, X6, ?S43, 16#a3014314),
    B15 = ?II (B14, C15, D15, A15, X13, ?S44, 16#4e0811a1),

    A16 = ?II (A15, B15, C15, D15, X4, ?S41, 16#f7537e82),
    D16 = ?II (D15, A16, B15, C15, X11, ?S42, 16#bd3af235),
    C16 = ?II (C15, D16, A16, B15, X2, ?S43, 16#2ad7d2bb),
    B16 = ?II (B15, C16, D16, A16, X9, ?S44, 16#eb86d391),
    
    {?U32(A0+A16), ?U32(B0+B16), ?U32(C0+C16), ?U32(D0+D16)}.

