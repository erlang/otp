%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
-module(testNBAPsystem).

-export([compile/3,test/2,cell_setup_req_msg/0]).

-include_lib("test_server/include/test_server.hrl").

-record('InitiatingMessage',{
procedureID, criticality, messageDiscriminator, transactionID, value}).
-record('AuditRequest',{
protocolIEs, protocolExtensions = asn1_NOVALUE}).
-record('ProtocolIE-Field',{
id, criticality, value}).
-record('ProcedureID',{
procedureCode ,ddMode}).
-record('CellSetupRequestFDD',{
protocolIEs,protocolExtensions=asn1_NOVALUE}).
-record('CellSetupRequestFDD_protocolIEs_SEQOF',{
id, criticality, value}).
-record('Synchronisation-Configuration-Cell-SetupRqst',{
'n-INSYNC-IND', 'n-OUTSYNC-IND', 't-RLFAILURE', 'iE-Extensions' = asn1_NOVALUE}).
-record('PrimarySCH-Information-Cell-SetupRqstFDD',{
commonPhysicalChannelID, 'primarySCH-Power', 'tSTD-Indicator', 'iE-Extensions' = asn1_NOVALUE}).
-record('SecondarySCH-Information-Cell-SetupRqstFDD',{
commonPhysicalChannelID, 'secondarySCH-Power', 'tSTD-Indicator', 'iE-Extensions' = asn1_NOVALUE}).
-record('PrimaryCCPCH-Information-Cell-SetupRqstFDD',{
commonPhysicalChannelID, 'bCH-information', 'sTTD-Indicator', 'iE-Extensions' = asn1_NOVALUE}).
-record('PrimaryCPICH-Information-Cell-SetupRqstFDD',{
commonPhysicalChannelID, 'primaryCPICH-Power', transmitDiversityIndicator, 'iE-Extensions' = asn1_NOVALUE}).
-record('BCH-Information-Cell-SetupRqstFDD',{
commonTransportChannelID, 'bCH-Power', 'iE-Extensions' = asn1_NOVALUE}).
-record('Limited-power-increase-information-Cell-SetupRqstFDD',{
powerRaiseLimit, dLPowerAveragingWindowSize, 'iE-Extensions' = asn1_NOVALUE}).

-record('ResourceStatusIndication',{
	  protocolIEs, protocolExtensions = asn1_NOVALUE}).
%-record('ResourceStatusIndication_protocolIEs_SEQOF',{
%	  id, criticality, value}).
-record('No-Failure-ResourceStatusInd',{
'local-Cell-InformationList', 'local-Cell-Group-InformationList' = asn1_NOVALUE, 'iE-Extensions' = asn1_NOVALUE}). % with extension mark
%-record('Local-Cell-InformationList-ResourceStatusInd_SEQOF',{
%	  id, criticality, value}).
-record('Local-Cell-InformationItem-ResourceStatusInd',{
	  'local-CellID', addorDeleteIndicator, 
	  'dl-or-global-capacityCredit' = asn1_NOVALUE,
	  'ul-capacityCredit' = asn1_NOVALUE,
	  commonChannelsCapacityConsumptionLaw = asn1_NOVALUE, 
	  dedicatedChannelsCapacityConsumptionLaw = asn1_NOVALUE, 
	  'maximumDL-PowerCapability' = asn1_NOVALUE,
	  minSpreadingFactor = asn1_NOVALUE,
	  'minimumDL-PowerCapability' = asn1_NOVALUE,
	  'local-Cell-Group-ID' = asn1_NOVALUE,
	  'iE-Extensions' = asn1_NOVALUE}). % with extension mark
-record('CommonChannelsCapacityConsumptionLaw_SEQOF',{
	  'dl-Cost', 'ul-Cost', 'iE-Extensions' = asn1_NOVALUE}). % with extension mark
-record('DedicatedChannelsCapacityConsumptionLaw_SEQOF',{
	  'dl-Cost-1', 'dl-Cost-2', 'ul-Cost-1', 'ul-Cost-2',
	  'iE-Extensions' = asn1_NOVALUE}). % with extension mark
-record('Local-Cell-InformationItem-ResourceStatusInd_iE-Extensions_SEQOF',{
	  id, criticality, extensionValue}).


compile(Config,Rules,Opt) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line DataDir2 = filename:join([DataDir,nbapsystem]),

    ?line ok = asn1ct:compile(filename:join([DataDir2,"NBAP-CommonDataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
    ?line ok = asn1ct:compile(filename:join([DataDir2,"NBAP-IEs.asn"]),[Rules,{outdir,OutDir}]++Opt),
    ?line ok = asn1ct:compile(filename:join([DataDir2,"NBAP-PDU-Contents.asn"]),[Rules,{outdir,OutDir}]++Opt),
    ?line ok = asn1ct:compile(filename:join([DataDir2,"NBAP-PDU-Discriptions.asn"]),[Rules,{outdir,OutDir}]++Opt),
    ?line ok = asn1ct:compile(filename:join([DataDir2,"NBAP-Constants.asn"]),[Rules,{outdir,OutDir}]++Opt),
    ?line ok = asn1ct:compile(filename:join([DataDir2,"NBAP-Containers.asn"]),[Rules,{outdir,OutDir}]++Opt).


test(_Erule,Config) ->
    ?line ok = enc_audit_req_msg(),
    ?line ok = cell_setup_req_msg_test(),
    ticket_5812(Config).

ticket_5812(Config) ->
    ?line Msg = v_5812(),
    ?line {ok,B2} = asn1_wrapper:encode('NBAP-PDU-Discriptions',
				  'NBAP-PDU',
				  Msg),
    V = <<0,28,74,0,3,48,0,0,1,0,123,64,41,0,0,0,126,64,35,95,208,2,89,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,145,0,1,205,0,0,0,0,2,98,64,1,128>>,
    ?line ok = compare(V,B2),
    ?line {ok,Msg2} = asn1_wrapper:decode('NBAP-PDU-Discriptions',
				  'NBAP-PDU',B2),
    ?line ok = check_record_names(Msg2,Config).

enc_audit_req_msg() ->
    Msg = {initiatingMessage, audit_req_msg()},
    ?line {ok,B}=asn1_wrapper:encode('NBAP-PDU-Discriptions',
				     'NBAP-PDU',
				     Msg),
    ?line {ok,_Msg}=asn1_wrapper:decode('NBAP-PDU-Discriptions',
				     'NBAP-PDU',
				     B),
    ?line {initiatingMessage,
	   #'InitiatingMessage'{value=#'AuditRequest'{protocolIEs=[{_,114,ignore,_}],
						      protocolExtensions = asn1_NOVALUE}}} = _Msg,
    io:format("Msg: ~n~P~n~n_Msg:~n~P~n",[Msg,15,_Msg,15]),
    ok.
    
cell_setup_req_msg_test() ->
    Msg = {initiatingMessage, cell_setup_req_msg()},
    ?line {ok,B}=asn1_wrapper:encode('NBAP-PDU-Discriptions',
				     'NBAP-PDU',
				     Msg),
    ?line {ok,_Msg}=asn1_wrapper:decode('NBAP-PDU-Discriptions',
				     'NBAP-PDU',
				     B),
    io:format("Msg: ~P~n~n_Msg: ~P~n",[Msg,15,_Msg,15]),
    ok.
    
audit_req_msg() ->
    #'InitiatingMessage'{procedureID={'ProcedureID',0,common},
			 criticality=reject,
			 messageDiscriminator=common,
			 transactionID={'longTransActionId',0},
			 value=audit_req()}.

audit_req() ->
    #'AuditRequest'{
	   protocolIEs = 
	   [#'ProtocolIE-Field'{id=114,
				criticality=ignore, 
				value={'Start-Of-Audit-Sequence-Indicator',
				       'start-of-audit-sequence' } 
			       }
	   ]  
	  }.

cell_setup_req_msg() ->
    #'InitiatingMessage'{
		    procedureID = procedureID(),
		    criticality = reject,
		    messageDiscriminator = common,
		    transactionID = {longTransActionId,0},
		    value = 'CellSetupRequestFDD'()}.



procedureID() ->
    #'ProcedureID'{procedureCode = 5,ddMode = fdd}.

'CellSetupRequestFDD'() ->
    #'CellSetupRequestFDD'{
	 protocolIEs = 
	   ['CellSetupRequestFDD_protocolIEs_SEQOF'(124,reject,601),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(25,reject,601),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(43,reject,10),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(276,reject,v1),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(282,reject,9750),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(281,reject,10700),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(131,reject,380),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(181,reject,0),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(394,reject,
	         'Synchronisation-Configuration-Cell-SetupRqst'(100,100,100)),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(358,reject,5),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(180,reject,
                 'PrimarySCH-Information-Cell-SetupRqstFDD'(11,-18,inactive)),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(265,reject,
                 'SecondarySCH-Information-Cell-SetupRqstFDD'(12,-35,inactive)),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(178,reject,
                 'PrimaryCPICH-Information-Cell-SetupRqstFDD'(13,330,inactive)),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(176,reject,'PrimaryCCPCH-Information-Cell-SetupRqstFDD'(15,'BCH-Information-Cell-SetupRqstFDD'(5,-31),inactive)),
	    'CellSetupRequestFDD_protocolIEs_SEQOF'(369,reject,'Limited-power-increase-information-Cell-SetupRqstFDD'())]}.


'CellSetupRequestFDD_protocolIEs_SEQOF'(Id,Criticality,Value) ->
    #'CellSetupRequestFDD_protocolIEs_SEQOF'{
	 id = Id,
         criticality = Criticality,
         value = Value}.

'Limited-power-increase-information-Cell-SetupRqstFDD'() ->
    #'Limited-power-increase-information-Cell-SetupRqstFDD'{
	  powerRaiseLimit = 5,
	  dLPowerAveragingWindowSize = 30}.

'PrimaryCPICH-Information-Cell-SetupRqstFDD'(Common,Primary,Transmit) ->
#'PrimaryCPICH-Information-Cell-SetupRqstFDD'{
                                       commonPhysicalChannelID = Common,
                                       'primaryCPICH-Power' = Primary,
                                       transmitDiversityIndicator = Transmit}.

'BCH-Information-Cell-SetupRqstFDD'(CommonTCID,BCHPower) ->
    #'BCH-Information-Cell-SetupRqstFDD'{
       commonTransportChannelID = CommonTCID,
       'bCH-Power' = BCHPower}.

'PrimaryCCPCH-Information-Cell-SetupRqstFDD'(CommonPCID,BCHInfo,
					     STTDInd) ->
    #'PrimaryCCPCH-Information-Cell-SetupRqstFDD'{
	  commonPhysicalChannelID = CommonPCID,
          'bCH-information' = BCHInfo,
          'sTTD-Indicator' = STTDInd}.

'SecondarySCH-Information-Cell-SetupRqstFDD'(Common,Secondary,TSTDInd) ->
    #'SecondarySCH-Information-Cell-SetupRqstFDD'{
           commonPhysicalChannelID = Common,
           'secondarySCH-Power' = Secondary,
           'tSTD-Indicator' = TSTDInd}.

'PrimarySCH-Information-Cell-SetupRqstFDD'(Common,Primary,TSTDInd) ->
    #'PrimarySCH-Information-Cell-SetupRqstFDD'{
         commonPhysicalChannelID = Common,
         'primarySCH-Power' = Primary,
         'tSTD-Indicator' = TSTDInd}.

'Synchronisation-Configuration-Cell-SetupRqst'(INSYNC,OUTSYNC,RLFAILURE) ->
    #'Synchronisation-Configuration-Cell-SetupRqst'{
          'n-INSYNC-IND' = INSYNC,
          'n-OUTSYNC-IND' = OUTSYNC,
          't-RLFAILURE' = RLFAILURE}.

v_5812() ->
    {initiatingMessage,initiatingMessage_051107()}.
initiatingMessage_051107() ->
    #'InitiatingMessage'{procedureID = procedureID_051107(),
			 criticality = criticality_051107(),
			 messageDiscriminator = messageDiscriminator_051107(),
			 transactionID = transactionID_051107(),
			 value = value_051107()}.
procedureID_051107() ->
    #'ProcedureID'{procedureCode=28,
		   ddMode=common}.
criticality_051107() -> ignore.
messageDiscriminator_051107() -> common.
transactionID_051107() -> {longTransActionId,3}.
value_051107() ->
    #'ResourceStatusIndication'{protocolIEs = protocolIEs_051107()}.
protocolIEs_051107() ->
    [#'ProtocolIE-Field'{id = 123,
			 criticality = ignore,
			 value = 'IndicationType-ResourceStatusInd_051107'('no-Failure')}].

'IndicationType-ResourceStatusInd_051107'('no-Failure') ->
    {'no-Failure',#'No-Failure-ResourceStatusInd'{'local-Cell-InformationList' =[#'ProtocolIE-Field'{id = 126,
       		      criticality = ignore,
		      value = 'Local-Cell-InformationItem-ResourceStatusInd'()}]}}.
'Local-Cell-InformationItem-ResourceStatusInd'() ->
    #'Local-Cell-InformationItem-ResourceStatusInd'{
          'local-CellID' = 601,
          addorDeleteIndicator = add,
          'dl-or-global-capacityCredit' = 0,
          commonChannelsCapacityConsumptionLaw = ['CommonChannelsCapacityConsumptionLaw_051107'()],
          dedicatedChannelsCapacityConsumptionLaw = ['DedicatedChannelsCapacityConsumptionLaw_SEQOF'()],
          'maximumDL-PowerCapability' = 401,
          minSpreadingFactor = v4,
          'minimumDL-PowerCapability' = 461,
          'local-Cell-Group-ID' = 0,
          'iE-Extensions' = ['Local-Cell-InformationItem-ResourceStatusInd_iE-Extensions_SEQOF'()]}.

'CommonChannelsCapacityConsumptionLaw_051107'() ->
    #'CommonChannelsCapacityConsumptionLaw_SEQOF'{
	 'dl-Cost' = 0,
	 'ul-Cost' = 0}.
'DedicatedChannelsCapacityConsumptionLaw_SEQOF'() ->
    #'DedicatedChannelsCapacityConsumptionLaw_SEQOF'{
         'dl-Cost-1' = 0,
         'dl-Cost-2' = 0,
         'ul-Cost-1' = 0,
         'ul-Cost-2' = 0}.

'Local-Cell-InformationItem-ResourceStatusInd_iE-Extensions_SEQOF'() ->
    #'Local-Cell-InformationItem-ResourceStatusInd_iE-Extensions_SEQOF'{
          id = 610,
          criticality = ignore,
          extensionValue = 'hsdpa-non-capable'}.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare(V,V) ->  
    ok;
compare(V,L) when list(L) ->
    compare(V,list_to_binary(L));
compare(_,_) ->
    false.

check_record_names(Msg,Config) ->
    DataDir = ?config(data_dir,Config),
    OutDir = ?config(priv_dir,Config),
    io:format("check_record_names: compiling ~p~ninclude directory: ~p~n",
	      [filename:join([DataDir,"test_records"]),OutDir]),
    ?line {ok,test_records} = compile:file(filename:join([DataDir,"test_records"]),
			    [{i,OutDir}]),
    io:format("check_record_names: calling test_records:'check_record_names_OTP-5812'/1~n",[]),
    ?line ok = test_records:'check_record_names_OTP-5812'(Msg).
    
% check_record_names({initiatingMessage,
% 		    #'InitiatingMessage'{procedureID = ProcedureID,
% 					 criticality = _Criticality,
% 					 messageDiscriminator = _MessageDisc,
% 					 transactionID = _TransactionID,
% 					 value = Value}}) ->
    
%     ?line ok = check_record_ProcedureID(ProcedureID),
%     ?line ok = check_record_Value(Value).

% check_record_ProcedureID(#'ProcedureID'{}) ->
%     ok;
% check_record_ProcedureID(_) -> false.

% check_record_Value(#'ResourceStatusIndication'{protocolIEs = ProtocolIEs}) ->
%     ?line ok = check_record_ProtocolIEs(ProtocolIEs);
% check_record_Value(_) -> false.

% check_record_ProtocolIEs(#'ProtocolIE-Field'{value =IndicationType}) ->
%     ?line ok = check_record_NFResourceStatusInd(IndicationType);
% check_record_ProtocolIEs(_) -> false.

% check_record_NFResourceStatusInd({'no-Failure',#'No-Failure-ResourceStatusInd'{'local-Cell-InformationList'=[LCI]}}) ->
%     ?line ok = check_record_LCInfoResourceStatusInd(LCI);
% check_record_NFResourceStatusInd(_) -> false.

% check_record_LCInfoResourceStatusInd(#'Local-Cell-InformationItem-ResourceStatusInd'{commonChannelsCapacityConsumptionLaw=[CCCCL],dedicatedChannelsCapacityConsumptionLaw=[DCCCL],'iE-Extensions' = [LCIRE]}) ->
%     ?line ok = check_record_CCCCL(CCCCL),
%     ?line ok = check_record_DCCCL(DCCCL),
%     ?line ok = check_record_LCIRE(LCIRE).

% check_record_CCCCL(#'CommonChannelsCapacityConsumptionLaw_SEQOF'{}) -> 
%     ok;
% check_record_CCCCL(_) -> false.

% check_record_DCCCL(#'DedicatedChannelsCapacityConsumptionLaw_SEQOF'{}) ->
%     ok;
% check_record_DCCCL(_) -> false.
% check_record_LCIRE(#'ProtocolExtensionField'{}) ->
%     ok;
% check_record_LCIRE(_) -> false.
