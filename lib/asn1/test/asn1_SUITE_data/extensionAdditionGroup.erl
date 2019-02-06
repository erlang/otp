%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

%%%-------------------------------------------------------------------
-module(extensionAdditionGroup).
-include("Extension-Addition-Group.hrl").
-export([run/1]).

-compile(export_all).

run(Erule) ->
    Val = #'Ax'{a=253,b=true,c={e,true},g="123",h=true},
    Enc = hex_to_binary(encoded_ax(Erule)),
    roundtrip('Ax', Val, Enc),

    Val2 = #'Ax3'{a=253,b=true,s=#'Ax3_s'{sa=11,sb=true,sextaddgroup=17}},
    roundtrip('Ax3', Val2),

    run3(),
    run3(Erule),

    roundtrip('InlinedSeq', #'InlinedSeq'{s=#'InlinedSeq_s'{a=42,b=true}}),
    roundtrip('ExtAddGroup1', #'ExtAddGroup1'{x=42,y=1023}),

    ok.

%% From X.691 (07/2002) A.4.
encoded_ax(per) ->  "9E000180 010291A4";
encoded_ax(uper) -> "9E000600 040A4690";
encoded_ax(ber) ->  none.

hex_to_binary(none) ->
    none;
hex_to_binary(L) ->
    << <<(hex_digit_to_binary(D)):4>> || D <- L, D =/= $\s >>.

hex_digit_to_binary(D) ->
    if
	$0 =< D, D =< $9 -> D - $0;
	$A =< D, D =< $F -> D - ($A-10)
    end.
run3(Erule) ->
    Val = 
{'RRC-DL-DCCH-Message',
 {c1,
  {rrcConnectionReconfiguration,
   {'RRC-RRCConnectionReconfiguration',0,
    {c1,
     {'rrcConnectionReconfiguration-r8',
      {'RRC-RRCConnectionReconfiguration-r8-IEs',
       {'RRC-MeasConfig',asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
        asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
        asn1_NOVALUE,asn1_NOVALUE},
       asn1_NOVALUE,
       [<<80,66,0,5,10,0,5,0,24,11,7,84,54,33,0,1,1,0,0,0,1,39,5,66,127,0,0,1>>,
        <<>>],
       {'RRC-RadioResourceConfigDedicated',
        [{'RRC-SRB-ToAddMod',1,
          {explicitValue,
           {am,
            {'RRC-RLC-Config_am',
             {'RRC-UL-AM-RLC',ms45,pInfinity,kBinfinity,t4},
             {'RRC-DL-AM-RLC',ms35,ms0}}}},
          {explicitValue,
           {'RRC-LogicalChannelConfig',
            {'RRC-LogicalChannelConfig_ul-SpecificParameters',3,infinity,
             ms50,0},
            asn1_NOVALUE}}}],
        [{'RRC-DRB-ToAddMod',3,3,
          {'RRC-PDCP-Config',infinity,
           {'RRC-PDCP-Config_rlc-AM',false},
           asn1_NOVALUE,
           {notUsed,'NULL'}},
          {am,
           {'RRC-RLC-Config_am',
            {'RRC-UL-AM-RLC',ms70,p256,kBinfinity,t4},
            {'RRC-DL-AM-RLC',ms35,ms40}}},
          3,
          {'RRC-LogicalChannelConfig',
           {'RRC-LogicalChannelConfig_ul-SpecificParameters',5,infinity,ms50,
            1},
           asn1_NOVALUE}},
         {'RRC-DRB-ToAddMod',4,4,
          {'RRC-PDCP-Config',infinity,
           {'RRC-PDCP-Config_rlc-AM',false},
           asn1_NOVALUE,
           {notUsed,'NULL'}},
          {am,
           {'RRC-RLC-Config_am',
            {'RRC-UL-AM-RLC',ms70,p256,kBinfinity,t4},
            {'RRC-DL-AM-RLC',ms35,ms40}}},
          4,
          {'RRC-LogicalChannelConfig',
           {'RRC-LogicalChannelConfig_ul-SpecificParameters',5,infinity,ms50,
            1},
           asn1_NOVALUE}}],
        asn1_NOVALUE,
        {explicitValue,
         {'RRC-MAC-MainConfig',
          {'RRC-MAC-MainConfig_ul-SCH-Config',n4,sf10,sf10240,false},
          asn1_NOVALUE,sf500,
          {setup,{'RRC-MAC-MainConfig_phr-Config_setup',sf200,sf200,dB3}},
          asn1_NOVALUE}},
        asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE},
       asn1_NOVALUE,asn1_NOVALUE}}}}}}},
        io:format("~p:~p~n",[Erule,Val]),
    {ok,List}= 'EUTRA-RRC-Definitions':encode('DL-DCCH-Message',Val),
    Enc = iolist_to_binary(List),
    io:format("Result from encode:~n~p~n",[Enc]),
    {ok,Val2} = 'EUTRA-RRC-Definitions':decode('DL-DCCH-Message', Enc),
    io:format("Result from decode:~n~p~n",[Val2]),
    case Val2 of
	Val -> ok;
	_ -> exit({expected,Val, got, Val2})
    end.

run3() ->
    SI = #'SystemInformationBlockType2'{
            timeAlignmentTimerCommon = sf500,
            lateNonCriticalExtension = asn1_NOVALUE,
            'ssac-BarringForMMTEL-Voice-r9' = asn1_NOVALUE,
            'ssac-BarringForMMTEL-Video-r9' = asn1_NOVALUE,
	    'ac-BarringForCSFB-r10' = asn1_NOVALUE},
    Barring = #'AC-BarringConfig'{
                 'ac-BarringFactor' = p00,
                 'ac-BarringTime' = s4,
                 'ac-BarringForSpecialAC' = <<0:5>>},
    T = 'SystemInformationBlockType2',
    roundtrip(T, SI),
    roundtrip(T, SI#'SystemInformationBlockType2'{
		   'ssac-BarringForMMTEL-Voice-r9'=Barring}),
    roundtrip(T, SI#'SystemInformationBlockType2'{
		'ssac-BarringForMMTEL-Video-r9'=Barring}),
    roundtrip(T, SI#'SystemInformationBlockType2'{
		   'ac-BarringForCSFB-r10'=Barring}).

roundtrip(T, V) ->
    roundtrip(T, V, none).

roundtrip(T, V, Expected) ->
    Mod = 'Extension-Addition-Group',
    {ok,E} = Mod:encode(T, V),
    {ok,V} = Mod:decode(T, E),
    case Expected of
	none -> ok;
	E -> ok
    end.
