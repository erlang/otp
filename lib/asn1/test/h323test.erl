%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
-module(h323test).

-compile(export_all).
-export([compile/3,run/1]).
-include_lib("test_server/include/test_server.hrl").

compile(Config,Rules,Options) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "H235-SECURITY-MESSAGES",[Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++ "H323-MESSAGES",[Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++ "MULTIMEDIA-SYSTEM-CONTROL",[Rules,{outdir,OutDir}]++Options).
    
run(per_bin) ->
    run();
run(per) ->
    run();
run(_Rules) -> 
    ok.

run() ->
    ?line alerting(),
    ?line connect(),
    ok.

arq() ->
    _AdmissionRequest = "27900007086000340036003300320038003700370101805337010180533600AC1F38C60693000D000445367AE75C5740120300AC1F38C6415004E0200100110000D7D22EA88D511C0200AC1F38C6C0580100".


t0() ->
    Setup = "00B8060008914A0001010180533622C000000000074572696373736F6E0356302E3100010180533700AC1F38C206B80045367AE75C5740120300AC1F38C6415000411C110000D7D22EA88D511C0200AC1F3806C0583802150000080E1403001E80800A04000100AC1F38C661A820400000060401004E1403001E80801114000100AC1F38C72EE000AC1F38C72EE00100010063AA34AB"
,
    ByteList = hexstr2bytes(Setup),
    asn1_wrapper:decode('H323-MESSAGES','H323-UU-PDU',ByteList).

t1() ->
    AdmissionRequest = "27900007086000340036003300320038003700370101805337010180533600AC1F38C60693000D000445367AE75C5740120300AC1F38C6415004E0200100110000D7D22EA88D511C0200AC1F38C6C0580100",
    ByteList = hexstr2bytes(AdmissionRequest),
    asn1_wrapper:decode('H323-MESSAGES','RasMessage',ByteList).

t2() ->
    Cs = "080200040504038090A56C059132303033700591323030347E00930500B8060008914A0001010180533622C000000000074572696373736F6E0356302E3100010180533700AC1F38C206B80045367AE75C5740120300AC1F38C6415000411C110000D7D22EA88D511C0200AC1F3806C0583802150000080E1403001E80800A04000100AC1F38C661A820400000060401004E1403001E80801114000100AC1F38C72EE000AC1F38C72EE00100010063AA34AB",
    ByteList = hexstr2bytes(Cs),
    asn1_wrapper:decode('H323-MESSAGES','H323-UU-PDU',ByteList).

t3() ->
    Cs = "10b8060008914a0002044003004d0067006f006e018085cc22c0b500534c164d6963726f736f6674ae204e65744d656574696e67ae0003332e3000000101808c990088e1293a06b8001689edc5bf23d3118c2d00c04f4b1cd0000c07000a00000204dc40b500534c3c0200000028000000000000001b0000008138427484ccd211b4e300a0c90d0660100000001289edc5bf23d3118c2d00c04f4b1cd00000000000000000a615d9ee",
    ByteList = hexstr2bytes(Cs),
    asn1_wrapper:decode('H323-MESSAGES','H323-UU-PDU',ByteList).
    
dec_alerting() ->
    Cs = "0380060008914a0002020120110000000000000000000000000000000000",
    _Slask="E83AE983",
    ByteList = hexstr2bytes(Cs),
    asn1_wrapper:decode('H323-MESSAGES','H323-UserInformation',ByteList).

enc_alerting(V) ->
    asn1_wrapper:encode('H323-MESSAGES','H323-UserInformation',V).

alerting() ->
    ?line {ok,V} = dec_alerting(),
    ?line {ok,B} = enc_alerting(V),
    ?line ByteList = lists:flatten(B),
    ?line {ok,V} = asn1_wrapper:decode('H323-MESSAGES','H323-UserInformation',ByteList).


dec_connect() ->
    Cs = "02c0060008914a00020088e1293a04a322c0b500534c164d6963726f736f6674ae204e65744d656474696e67ae0003332e3000001689edc5bf23d3118c2d00c04f4b1cd00900110000000000000000000000000000000000",
    _Slask="2f530a3f",
    ByteList = hexstr2bytes(Cs),
    asn1_wrapper:decode('H323-MESSAGES','H323-UserInformation',ByteList).
    
enc_connect(V) ->
    asn1_wrapper:encode('H323-MESSAGES','H323-UserInformation',V).

connect() ->
    ?line {ok,V} = dec_connect(),
    ?line {ok,B} = enc_connect(V),
    ?line ByteList = lists:flatten(B),
    ?line {ok,V} = asn1_wrapper:decode('H323-MESSAGES','H323-UserInformation',ByteList).

dec_h245_TCS() ->
    Cs ="02700106000881750003" 
	"800d00003c000100000100000100000e"
	"807fff04b5428080010180000e483060"
	"0100800c96a88000002020b500534c48"
	"020000000000f4010000f40101000400"
	"0000000002000100401f000000100000"
	"000104002000f4010700000100000002"
	"00ff00000000c0004000f0000000cc01"
	"30ff880118ff00008000012040b38000"
	"0220c0b38000032020b500534c280200"
	"00000000a0000000a000040010000000"
	"000070000100401f0000580200000c00"
	"1000000000008000042020b500534c28"
	"020000000000a0000000a00004001000"
	"0000000071000100401f00003a070000"
	"25001000000000008000052020b50053"
	"4c280200000000008000000080000500"
	"14000000000072000100401f00000809"
	"000025001000000000008000062020b5"
	"00534c28020000000000800000008000"
	"050014000000000073000100401f0000"
	"7f0a00002b0010000000000080000722"
	"000b40000909a00120390c000a099001"
	"20390c000b09880120390c000c08a220"
	"3940000d089220390004800602070007"
	"00060004000500020001000000030000"
	"0a00000e800702070007000600040005"
	"000200010000000300000900000e8008"
	"02070007000600040005000200010000"
	"000300000c00000e8009020700070006"
	"00040005000200010000000300000b00"
	"000e800a020700070006000400050002"
	"00010000000300000d00000e0300000b"
	"01003280299d93369631bc",
    ByteList = hexstr2bytes(Cs),
    asn1_wrapper:decode('MULTIMEDIA-SYSTEM-CONTROL',
		  'MultimediaSystemControlMessage',ByteList).

	hexstr2bytes([D1,D2|T]) ->
    [dig2num(D1)*16+dig2num(D2)|hexstr2bytes(T)];
hexstr2bytes([]) ->
    [].

dig2num(D) when D >= $0, D =< $9 ->
    D - $0;
dig2num(D) when D >= $a, D =< $f ->
    10 + D - $a;
dig2num(D) when D >= $A, D =< $F ->
    10 + D - $A.

bytes2hexstr(Bytes) ->
    bytes2hexstr(Bytes,[]).

bytes2hexstr([B|Bytes],Acc) ->
    D1 = num2dig(B bsr 4),
    D2 = num2dig(B band 15),
    bytes2hexstr(Bytes,[D2,D1|Acc]);
bytes2hexstr([],Acc) ->
    lists:reverse(Acc).

num2dig(Num) when Num =< 9 ->
    $0 + Num;
num2dig(Num) ->
    $a + Num - 10.
		  





