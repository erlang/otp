%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
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

-module(testMergeCompile).

-export([compile/3,main/1,mvrasn/1]).

-include_lib("test_server/include/test_server.hrl").

-record('InitiatingMessage',{procedureCode,criticality,value}).
-record('Iu-ReleaseCommand',{protocolIEs,protocolExtensions}).

compile(Config,Erule,Options) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),

    ?line ok = asn1ct:compile(DataDir ++ 
			      "MS.set.asn",[Erule,{outdir,OutDir}]++Options),

    ?line ok = asn1ct:compile(DataDir ++ 
			      "RANAPSET.set.asn1",[Erule,{outdir,OutDir}]++Options),
    
    ?line ok = asn1ct:compile(filename:join([DataDir,"Mvrasn4.set.asn"]),
			      [Erule,{outdir,OutDir}]++Options),

    ?line ok = asn1ct:compile(filename:join([DataDir,"Mvrasn6.set.asn"]),
			      [Erule,{outdir,OutDir}]++Options).


main(Erule) ->
    %% test of module MS.set.asn that tests OTP-4492: different tagdefault in 
    %% modules and types  with same name in modules
    ?line MSVal = {'Type4M2',8,true,three,"OCTET STRING"},
    ?line {ok,MSBytes} = asn1_wrapper:encode('MS','Type4M2',MSVal),
    ?line {ok,MSVal} = asn1_wrapper:decode('MS','Type4M2',MSBytes),
    

    %% test of RANAP.set.asn1
    ?line _PIEVal = [{'ProtocolIE-Field',4,ignore,{'Cause',{radioNetwork,{'CauseRadioNetwork','rab-pre-empted'}}}}],
    ?line PIEVal2 = [{'ProtocolIE-Field',4,ignore,{'Cause',{radioNetwork,'rab-pre-empted'}}}],
    ?line _PEVal = [{'ProtocolExtensionField',[0]}],
%%    ?line EncVal = asn1rt_per_v1:encode_integer([],100),
    ?line EncVal = 
	case Erule of
	    per -> 
		[1,100];
	    per_bin -> 
		<<1,100>>;
	    uper_bin ->
		<<1,100>>;
	    ber ->
		[2,1,1];
	    ber_bin ->
		<<2,1,1>>;
	    ber_bin_v2 ->
		<<2,1,1>>
	end,
    ?line PEVal2 = [{dummy,1,ignore,EncVal},{dummy,2,reject,EncVal}],
    ?line Val2 = 
	#'InitiatingMessage'{procedureCode=1,
			     criticality=ignore,
			     value=#'Iu-ReleaseCommand'{protocolIEs=PIEVal2,
							protocolExtensions=asn1_NOVALUE}},
    
    ?line {ok,Bytes2} = asn1_wrapper:encode('RANAPSET','InitiatingMessage',Val2),
    ?line {ok,_Ret2} = asn1_wrapper:decode('RANAPSET','InitiatingMessage',Bytes2),
    
    ?line Val3 = 
	#'InitiatingMessage'{procedureCode=1,
			     criticality=ignore,
			     value=#'Iu-ReleaseCommand'{protocolIEs=PIEVal2,
							protocolExtensions=PEVal2}},
    ?line {ok,Bytes3} = asn1_wrapper:encode('RANAPSET','InitiatingMessage',Val3),
    ?line {ok,_Ret3} = asn1_wrapper:decode('RANAPSET','InitiatingMessage',Bytes3).


mvrasn(Erule) ->
    case Erule of
	Ber when Ber == ber;Ber == ber_bin ->
	    ?line ok = test(isd),
	    ?line ok = test(isd2),
	    ?line ok = test(dsd),
	    ?line ok = test(ul_res),
	    ?line ok = test(seqofseq),
	    ?line ok = test('InsertSubscriberDataArg');
	_ ->
	    ok
    end,
    ?line ok = test(mvrasn6,'InsertSubscriberDataArg').

test(isd)->
    EncPdu = [48,128,129,7,145,148,113,50,1,0,241,131,1,0,176,128,5,0,161,128,48,22,2,1,1,144,2,241,33,145,4,0,1,2,3,146,3,36,131,16,148,2,1,42,48,35,2,1,2,144,2,241,33,145,4,255,255,255,255,146,3,37,147,18,147,0,148,13,7,67,79,77,80,65,78,89,4,67,79,77,53,48,28,2,1,3,144,2,241,33,146,3,26,98,31,148,14,9,67,79,77,80,65,78,89,49,50,3,67,79,77,0,0,0,0,152,1,2,0,0],
    
    ?line {ok,_} = asn1_wrapper:decode('Mvrasn4',
				'InsertSubscriberDataArg',
				EncPdu),
    ok;

% 
% Problems with indefinite length encoding !!!
% 
test(isd2)->
    EncPdu = [48, 128, 128, 8, 98, 2, 50, 1, 0, 0, 0, 241, 176, 128, 161, 128, 48, 128, 2, 1, 1, 144, 2, 241, 33, 145, 4, 255, 23, 12, 1, 146, 3, 9, 17, 1, 147, 0, 148, 13, 7, 67, 79, 77, 80, 65, 78, 89, 4, 67, 79, 77, 53, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    
    ?line {ok,_DecPdu} = asn1_wrapper:decode('Mvrasn4',
				'InsertSubscriberDataArg',
				EncPdu),

    ok;

% 
% Is doing fine, although there is indefinite encoding used... !!!
% 
test(dsd)->
    EncPdu = [48, 128, 128, 8, 98, 2, 50, 1, 0, 0, 0, 241, 170, 2, 5, 0, 0, 0, 0, 0],
    
    ?line {ok,_DecPdu} = asn1_wrapper:decode('Mvrasn4',
				'DeleteSubscriberDataArg',
				EncPdu),

    ok;

%
% Is doing fine !!!
% 
test(ul_res)->
    EncPdu = [48, 9, 4, 7, 145, 148, 113, 66, 16, 17, 241],
    
    ?line {ok,_DecPdu} = asn1_wrapper:decode('Mvrasn4',
				'UpdateGprsLocationRes',
				EncPdu),

    ok;

test(seqofseq) ->
    {ok,_V} = asn1_wrapper:decode('Mvrasn4',
				 'SentParameters',
				 [48,129,190,161,128,4,16,176,197,182,68,41,243,188,205,123,13,9,145,206,200,144,102,4,4,176,197,182,68,4,8,41,243,188,205,123,13,9,145,0,0,161,128,4,16,39,0,3,117,35,189,130,21,42,104,49,194,212,24,151,234,4,4,39,0,3,117,4,8,35,189,130,21,42,104,49,194,0,0,161,128,4,16,62,207,166,59,71,29,37,97,120,25,132,80,144,251,161,123,4,4,62,207,166,59,4,8,71,29,37,97,120,25,132,80,0,0,161,128,4,16,95,183,173,151,17,76,148,146,248,102,127,215,102,224,39,60,4,4,95,183,173,151,4,8,17,76,148,146,248,102,127,215,0,0,161,128,4,16,41,198,247,157,117,190,203,170,91,146,88,91,223,220,188,16,4,4,41,198,247,157,4,8,117,190,203,170,91,146,88,91,0,0]),
    ok;

test('InsertSubscriberDataArg') ->
    {ok,_V} =
	asn1_wrapper:decode('Mvrasn4','InsertSubscriberDataArg',
			    [16#30,16#80,16#81,16#07,16#91,16#94,
			     16#71,16#92,16#00,16#35,16#80,16#83,
			     16#01,16#00,16#A6,16#06,16#04,16#01,
			     16#21,16#04,16#01,16#22,16#B0,16#80,
			     16#05,16#00,16#A1,16#80,16#30,16#1A,
			     16#02,16#01,16#01,16#90,16#02,16#F1,
			     16#21,16#92,16#03,16#0D,16#92,16#1F,
			     16#94,16#0C,16#03,16#53,16#49,16#4D,
			     16#03,16#47,16#53,16#4E,16#03,16#4C,
			     16#4B,16#50,16#00,16#00,16#00,16#00,
			     16#98,16#01,16#00,16#00,16#00]),
    ok.

test(mvrasn6,'InsertSubscriberDataArg') ->
    Val = {'InsertSubscriberDataArg',"IMSI","Address","C",serviceGranted,["abc","cde"],["tele","serv","ice"],asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,{'NAEA-PreferredCI',"NCC",asn1_NOVALUE},{'GPRSSubscriptionData','NULL',[{'PDP-Context',49,"PT","PDP-Address","QoS",'NULL',"APN",asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE}],asn1_NOVALUE},'NULL',onlyMSC,{'LSAInformation','NULL',accessOutsideLSAsAllowed,[{'LSAData',"LSA","L",'NULL',asn1_NOVALUE},{'LSAData',"LSA","L",'NULL',asn1_NOVALUE}],asn1_NOVALUE},'NULL',{'LCSInformation',["Addr","ess","string"],[{'LCS-PrivacyClass',"S","ExtSS",notifyLocationAllowed,[{'ExternalClient',{'LCSClientExternalID',"Addr",asn1_NOVALUE},asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE}],[broadcastService,anonymousLocation,targetMSsubscribedService],asn1_NOVALUE}],asn1_NOVALUE},100,"age",{'MC-SS-Info',"S","ExtSS",5,4,asn1_NOVALUE},"C",{'SGSN-CAMEL-SubscriptionInfo',{'GPRS-CSI',[{'GPRS-CamelTDPData',attach,13,"Addr",continueTransaction,asn1_NOVALUE}],11,asn1_NOVALUE,'NULL','NULL'},{'SMS-CSI',[{'SMS-CAMEL-TDP-DataList','sms-CollectedInfo',13,"Addr",continueTransaction,asn1_NOVALUE}],11,asn1_NOVALUE,'NULL','NULL'},asn1_NOVALUE},"ON"},

    {ok,Bytes}=
	asn1_wrapper:encode('Mvrasn6','InsertSubscriberDataArg',Val),
    
    {ok,_Res} =
	asn1_wrapper:decode('Mvrasn6','InsertSubscriberDataArg',Bytes),
    
    ok.
