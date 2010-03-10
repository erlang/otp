%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
-module(test_selective_decode).

-export([test/2]).

-include_lib("test_server/include/test_server.hrl").


test(ber_bin_v2,_Config) ->
    FMsg = msg('F'),
    ?line {ok,Bytes} = asn1_wrapper:encode('PartialDecSeq','F',FMsg),
    ?line {ok,3} =
	'PartialDecSeq':selected_decode_F1(list_to_binary(Bytes)),
    ?line {ok,[{'D',3,true},{'D',4,false},{'D',5,true},{'D',6,true},{'D',7,false},{'D',8,true},{'D',9,true},{'D',10,false},{'D',11,true},{'D',12,true},{'D',13,false},{'D',14,true}]} = 'PartialDecSeq':selected_decode_F2(list_to_binary(Bytes)),
    ?line {ok,{'D',3,true}} = 'PartialDecSeq':selected_decode_F3(list_to_binary(Bytes)),

    ?line {ok,17} = 'PartialDecSeq':selected_decode_F4(list_to_binary(Bytes)),
    
    EMsg = msg('E'),
    ?line {ok,Bytes2} = asn1_wrapper:encode('PartialDecSeq','E',EMsg),
    ?line {ok,14} = 'PartialDecSeq':selected_decode_E1(list_to_binary(Bytes2)),
    MGCMsg = msg('M-G-C'),
    ?line {ok,Bytes3} = asn1_wrapper:encode('MEDIA-GATEWAY-CONTROL',
					    'MegacoMessage',MGCMsg),
    ?line {ok,1} = 'MEDIA-GATEWAY-CONTROL':decode_MegacoMessage_selective(list_to_binary(Bytes3)),

    PRecMsg = {'PersonnelRecord',{'Name',"Sven","S","Svensson"},
	       "manager",123,"20000202",{'Name',"Inga","K","Svensson"},
	       asn1_DEFAULT},
    ?line {ok,Bytes4} = asn1_wrapper:encode('P-Record','PersonnelRecord',
					    PRecMsg),
    ?line {ok,_} = 'P-Record':sel_dec(list_to_binary(Bytes4)),

    ok;
test(Erule,_) ->
    {skip,lists:concat(["not implemented yet for version: ",Erule])}.


    
msg('F') ->
    {'F',{fb,{'E',35,[{'D',3,true},{'D',4,false},{'D',5,true},{'D',6,true},{'D',7,false},{'D',8,true},{'D',9,true},{'D',10,false},{'D',11,true},{'D',12,true},{'D',13,false},{'D',14,true}],false,{da,[{'A',16,{'D',17,true}}]}}}};

msg('E') ->
    {'E',10,[{'D',11,true},{'D',12,false}],false,{dc,{'E_d_dc',13,true,{'E_d_dc_dcc',14,15}}}};

msg('M-G-C') ->
    {'MegacoMessage',asn1_NOVALUE,{'Message',1,{ip4Address,{'IP4Address',[125,125,125,111],55555}},{transactions,[{transactionReply,{'TransactionReply',50007,asn1_NOVALUE,{actionReplies,[{'ActionReply',0,asn1_NOVALUE,asn1_NOVALUE,[{auditValueReply,{auditResult,{'AuditResult',{'TerminationID',[],[255,255,255]},[{mediaDescriptor,{'MediaDescriptor',asn1_NOVALUE,{multiStream,[{'StreamDescriptor',1,{'StreamParms',{'LocalControlDescriptor',sendRecv,asn1_NOVALUE,asn1_NOVALUE,[{'PropertyParm',[0,11,0,7],[[52,48]],asn1_NOVALUE}]},{'LocalRemoteDescriptor',[[{'PropertyParm',[0,0,176,1],[[48]],asn1_NOVALUE},{'PropertyParm',[0,0,176,8],[[73,78,32,73,80,52,32,49,50,53,46,49,50,53,46,49,50,53,46,49,49,49]],asn1_NOVALUE},{'PropertyParm',[0,0,176,15],[[97,117,100,105,111,32,49,49,49,49,32,82,84,80,47,65,86,80,32,32,52]],asn1_NOVALUE},{'PropertyParm',[0,0,176,12],[[112,116,105,109,101,58,51,48]],asn1_NOVALUE}]]},{'LocalRemoteDescriptor',[[{'PropertyParm',[0,0,176,1],[[48]],asn1_NOVALUE},{'PropertyParm',[0,0,176,8],[[73,78,32,73,80,52,32,49,50,52,46,49,50,52,46,49,50,52,46,50,50,50]],asn1_NOVALUE},{'PropertyParm',[0,0,176,15],[[97,117,100,105,111,32,50,50,50,50,32,82,84,80,47,65,86,80,32,32,52]],asn1_NOVALUE},{'PropertyParm',[0,0,176,12],[[112,116,105,109,101,58,51,48]],asn1_NOVALUE}]]}}}]}}},{packagesDescriptor,[{'PackagesItem',[0,11],1},{'PackagesItem',[0,11],1}]},{statisticsDescriptor,[{'StatisticsParameter',[0,12,0,4],[[49,50,48,48]]},{'StatisticsParameter',[0,11,0,2],[[54,50,51,48,48]]},{'StatisticsParameter',[0,12,0,5],[[55,48,48]]},{'StatisticsParameter',[0,11,0,3],[[52,53,49,48,48]]},{'StatisticsParameter',[0,12,0,6],[[48,46,50]]},{'StatisticsParameter',[0,12,0,7],[[50,48]]},{'StatisticsParameter',[0,12,0,8],[[52,48]]}]}]}}}]}]}}}]}}}.
