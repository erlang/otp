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
%%
%%
-module(testobj).

-include("RANAP.hrl").

-compile(export_all).

-define(ranap, 'RANAP').


%% These are possible Reason-values for sccp_disconnect_req in RANPM
%%
%% 0,0,0,0,0,1,0,0,  % Message-type
%% 0,0,0,0,0,0,0,0,  % Dest local ref (dummy) - 3 octets
%% 0,0,0,0,0,0,0,0,
%% 0,0,0,0,0,0,0,1,
%% 0,0,0,0,0,0,0,0,  % Source local ref (dummy) - 3 octets
%% 0,0,0,0,0,0,0,0,
%% 0,0,0,0,0,0,1,0,

run() ->
    ok = run_com_id(),
    ok = run_dir_tsf_2cn(),
    ok = run_dir_tsf_2rnc(),
    ok = run_init_ue(),
    ok = run_iu_rel_cmd(),
    ok = run_iu_rel_cmp(),            
    ok = run_rab_ass_rsp_delete(),
    ok = run_rab_ass_rsp_setup(),
    ok = run_rab_create(),            
    ok = run_rab_rel(),
    ok = run_reset(),
    ok = run_reset_res(),             
    ok = run_sm_cmd(),
    ok = run_sm_cmp(),
    ok = run_sm_rej().   


ranap_pdu_contents(Enc,Type) ->
  {initiatingMessage, #'InitiatingMessage'{
    procedureCode = Type,
    criticality = ignore, %{'Criticality', ignore}, %XXX
    value = Enc
  }}.

ranap_pdu_contents_outcome(Enc,Type) ->
  {outcome, #'Outcome'{
    procedureCode = Type,
    criticality = ignore,%XXX
    value = Enc
  }}.

ranap_pdu_contents_suc(Enc,Type) ->
  {successfulOutcome, #'SuccessfulOutcome'{
    procedureCode = Type,
    criticality = ignore,%XXX
    value = Enc
  }}.

ranap_pdu_contents_unsuc(Enc,Type) ->
  {unsuccessfulOutcome, #'UnsuccessfulOutcome'{
    procedureCode = Type,
    criticality = ignore,%XXX
    value = Enc
  }}.



run_rab_rel() ->
    RAS = rab_release_request(),
    io:format("~w~n~n", [RAS]),
    RanapRAS = ranap_pdu_contents(RAS,0),               % 0=Rab Assignment Procedure
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapRAS),
    EncRanapRAS = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapRAS]),
    {ok,{initiatingMessage, 
	 #'InitiatingMessage'{procedureCode=ProcedureCode,
			      criticality=Criticality,
			      value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapRAS),
    io:format("~w~n~n", [DecGeneral]),
    case DecGeneral of
	{'RAB-AssignmentRequest',[{_ProtIEField,_Code,_Crit,DecRel}],_ASN1novalue} ->
	    io:format("~w~n~n", [DecRel])
    end,
    case DecRel of
	[[{_ProtIEField1,_Code1,_Crit1,DecRelList}]] ->
	    io:format("~w~n~n", [DecRelList])
    end,
    ok.

run_rab_create() ->
    RabID = [0,1,0,1,0,1,0,1],
    Teid = [0,13,83,211],
    SgsnIP = [0,0,0,0,1,1,0,1,0,0,0,0,1,1,0,0,0,0,0,0,1,0,1,1,0,0,0,0,1,0,1,0],
    RAS = rab_create_request(RabID, Teid, SgsnIP),
    io:format("~w~n~n", [RAS]),
    RanapRAS = ranap_pdu_contents(RAS,0),                     % 0=Rab Assignment Procedure
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapRAS),
    EncRanapRAS = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapRAS]),
    {ok,{initiatingMessage, 
	 #'InitiatingMessage'{procedureCode=ProcedureCode,
			      criticality=Criticality,
			      value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapRAS),
    io:format("~w~n~n", [DecGeneral]),
    case DecGeneral of
	{'RAB-AssignmentRequest',[{_ProtIEField,_Code,_Crit,DecSetOrMod}],_ASN1novalue} ->
	    io:format("~w~n~n", [DecSetOrMod])
    end,
    case DecSetOrMod of
	[[{'ProtocolIE-FieldPair',_Code1,_Crit1,DecSetOrModFirst,_Crit2,DecSetOrModSecond}]] ->
	    io:format("~w~n~n", [DecSetOrModFirst]),
	    io:format("~w~n~n", [DecSetOrModSecond])
    end,
    ok.

decode_initiating_ras(ProcedureCode,Crit,Val) ->
    case ProcedureCode of
        0 -> % RAB-Assignment
	    wrapper_decode(?ranap, 'RAB-AssignmentRequest',Val)
    end.


run_rab_ass_rsp_setup() ->
    RAR = rab_assignment_response_setup(),
    io:format("~w~n~n", [RAR]),
    RanapRAR = ranap_pdu_contents_outcome(RAR,0),                     % 0=Rab Assignment Procedure 
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapRAR),
    EncRanapRAR = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapRAR]),
	{ok,{outcome, 
	  #'Outcome'{procedureCode=ProcedureCode,
		     criticality=Criticality,
		     value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapRAR),
    io:format("~w~n~n", [DecGeneral]),
    case DecGeneral of
	{'RAB-AssignmentResponse',[{_ProtIEField,52,_Crit,DecSetOrMod}], _ASN1novalue} ->
	    io:format("Setup or Modified: ~w~n~n", [DecSetOrMod])
    end,
    case DecSetOrMod of
	[[{_ProtIEField1,51,_Crit1,DecSetOrModFirst}]] ->
	    io:format("Setup or Modified: ~w~n~n", [DecSetOrModFirst])
    end,
    ok.

run_rab_ass_rsp_delete() ->
    RAR = rab_assignment_response_delete(),
    io:format("~w~n~n", [RAR]),
    RanapRAR = ranap_pdu_contents_outcome(RAR,0),                     % 0=Rab Assignment Procedure 
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapRAR),
    EncRanapRAR = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapRAR]),
	{ok,{outcome, 
	  #'Outcome'{procedureCode=ProcedureCode,
		     criticality=Criticality,
		     value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapRAR),
    io:format("~w~n~n", [DecGeneral]),
    case DecGeneral of
	{'RAB-AssignmentResponse',[{_ProtIEField2,43,_Crit2,DecRelsd}], _ASN1novalue} ->
	    io:format("Released: ~w~n~n", [DecRelsd])
    end,
    case DecRelsd of
	[[{_ProtIEField3,_Code3,_Crit3,DecRelsdItem}]] ->
	    io:format("Released: ~w~n~n", [DecRelsdItem])
    end,
    ok.

decode_initiating_rar(ProcedureCode,Crit,Val) ->
    case ProcedureCode of
        0 -> % RAB-Assignment
	    wrapper_decode(?ranap, 'RAB-AssignmentResponse',Val)
    end.



run_init_ue() ->
    INI = initial_ue(),
    io:format("~w~n~n", [INI]),
    RanapINI = ranap_pdu_contents(INI, 19),                     % 19 = InitialUE-Message
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapINI),
    EncRanapINI = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapINI]),

    {ok,{initiatingMessage, 
	 #'InitiatingMessage'{procedureCode=ProcedureCode,
			      criticality=Criticality,
			      value=DecGeneral}}} = 
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapINI),
    io:format("~w~n~n", [DecGeneral]),
    case DecGeneral of
	{'InitialUE-Message',[{_ProtIEField,Code,_Crit,DecCN},
			      {_ProtIEField1,Code1,_Crit1,
			       {_LAI,LaiPlmnid,LaiLac,_ASN1novalue}},
			      {_ProtIEField2,Code2,_Crit2,DecRAC},
			      {_ProtIEField3,Code3,_Crit3,
			       {_SAI,SaiPlmnid,SaiLac,SaiSac,_ASN1novalue}},
			      {_ProtIEField4,Code4,_Crit4,DecNASPDU},
			      {_ProtIEField5,Code5,_Crit5,DecIUSCID},
			      {_ProtIEField6,Code6,_Crit6,
			       {_GRI,GlPlmnid,GlRNCID}}],_ASN1novalue} ->
	    io:format("CN Dom Ind: ~w~n", [DecCN]),
	    io:format("Code: ~w~n~n", [Code]),
	    io:format("LaiPlmnid: ~w~n", [LaiPlmnid]),
	    io:format("LaiLac: ~w~n", [LaiLac]),
	    io:format("Code: ~w~n~n", [Code1]),
	    io:format("RAC: ~w~n", [DecRAC]),
	    io:format("Code: ~w~n~n", [Code2]),
	    io:format("SaiPlmnid: ~w~n", [SaiPlmnid]),
	    io:format("SaiLac: ~w~n", [SaiLac]),
	    io:format("SaiSac: ~w~n", [SaiSac]),
	    io:format("Code: ~w~n~n", [Code3]),
	    io:format("NAS-PDU: ~w~n", [DecNASPDU]),
	    io:format("Code: ~w~n~n", [Code4]),
	    io:format("Iu Sign Con Id: ~w~n", [DecIUSCID]),
	    io:format("Code: ~w~n~n", [Code5]),
	    io:format("GlPlmnid: ~w~n", [GlPlmnid]),
	    io:format("GlRNCID: ~w~n", [GlRNCID]),
	    io:format("Code: ~w~n~n", [Code6])
    end,
    ok.

%  NasPdu = extract_ie({init_ue},'InitUE-MessageIEsNAS-PDU',ListsinLists),
%  io:format("Tebax~n~w~n~n", [NasPdu]), 
%  ok.

decode_initiating_ini(ProcedureCode,Crit,Val) ->
    case ProcedureCode of
        19 -> % InitialUE-Message
	    wrapper_decode(?ranap, 'InitialUE-Message',Val)
    end.


run_dir_tsf_2cn() ->
    DIR = direct_transfer_cn(),
    io:format("~w~n~n", [DIR]),
    RanapDIR = ranap_pdu_contents(DIR, 20),                     % 20 = DirectTransfer
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapDIR),
    EncRanapDIR = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapDIR]),
    {ok,{initiatingMessage, 
	 #'InitiatingMessage'{procedureCode=ProcedureCode,
			      criticality=Criticality,
			      value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapDIR),
    io:format("~w~n~n", [DecGeneral]),
    case DecGeneral of
	{'DirectTransfer',
	 [{_ProtIEField1,_Code1,_Crit1,DecNASPDU},
	  {_ProtIEField2,_Code2,_Crit2,DecLAI},
	  {_ProtIEField3,_Code3,_Crit3,DecRAC}],
	 _ASN1novalue} ->
	    io:format("NAS-PDU: ~w~n~n", [DecNASPDU]),
	    io:format("LAI:     ~w~n~n", [DecLAI]),
	    io:format("RAC:     ~w~n~n", [DecRAC])
    end,
    ok.

run_dir_tsf_2rnc() ->
    DIR = direct_transfer_rnc(),
    io:format("~w~n~n", [DIR]),
    RanapDIR = ranap_pdu_contents(DIR, 20),                     % 20 = DirectTransfer
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapDIR),
    EncRanapDIR = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapDIR]),
    {ok,{initiatingMessage, 
	 #'InitiatingMessage'{procedureCode=ProcedureCode,
			      criticality=Criticality,
			      value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapDIR),
    io:format("~w~n~n", [DecGeneral]),
    case DecGeneral of
	{'DirectTransfer',
	 [{_ProtIEField1,_Code1,_Crit1,DecNASPDU},
	  {_ProtIEField2,_Code2,_Crit2,DecSAPI}],
	 _ASN1novalue} ->
	    io:format("NAS-PDU: ~w~n~n", [DecNASPDU]),
	    io:format("SAPI:    m~w~n~n", [DecSAPI])
    end,
    ok.

decode_initiating_dir(ProcedureCode,Crit,Val) ->
    io:format("ProcedureCode: ~w~n~n", [ProcedureCode]),
    case ProcedureCode of
        20 -> % DirectTransfer
	    wrapper_decode(?ranap, 'DirectTransfer',Val)
    end.

%  List = tuple2list(Dec),
%  io:format("~w~n~n", [List]),
%  NasPdu = extract_ie({dir_trans},'DirTransIEs-NAS-PDU',List),
%  io:format("~w~n~n", [NasPdu]), 
%  ok.


run_iu_rel_cmd() ->
    IUR = iu_release_command(),
    io:format("~w~n~n", [IUR]),
    RanapIUR = ranap_pdu_contents(IUR, 1),                 % 1 = Iu-Release
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapIUR),
    EncRanapIUR = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapIUR]),
    {ok,{initiatingMessage, 
	 #'InitiatingMessage'{procedureCode=ProcedureCode,
			      criticality=Criticality,
			      value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapIUR),
    io:format("General: ~w~n~n", [DecGeneral]),
    case DecGeneral of
	{'Iu-ReleaseCommand',[{_ProtIEField,_Code,_Crit,DecCause}],_ASN1novalue} ->
	    io:format("Cause: ~w~n~n", [DecCause])
    end.

run_iu_rel_cmp() ->
    IUP = iu_release_complete(),
    io:format("~w~n~n", [IUP]),
    RanapIUP = ranap_pdu_contents_suc(IUP, 1),                % 1 = Iu-Release
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapIUP),
    EncRanapIUP = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapIUP]),
    {ok,{successfulOutcome, 
	 #'SuccessfulOutcome'{procedureCode=ProcedureCode,
			      criticality=Criticality,
			      value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapIUP),
    io:format("~w~n~n", [DecGeneral]).

% run_iu_rel_req() ->
%     IUP = iu_release_request(),
%     io:format("~w~n~n", [IUP]),
%     {ok, Tmp} = wrapper_encode(?ranap, 'Iu-ReleaseRequest', IUP),
%     EncIUP = lists:flatten(Tmp),
%     RanapIUP = ranap_pdu_contents(EncIUP, 1),                % 1 = Iu-Release
%     {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapIUP),
%     EncRanapIUP = lists:flatten(Tmp2),
%     io:format("~w~n~n", [EncRanapIUP]),
%     case wrapper_decode(?ranap, 'RANAP-PDU', EncRanapIUP) of
% 	{ok,{initiatingMessage, 
% 	  #'InitiatingMessage'{procedureCode=ProcedureCode,
% 		     criticality=Criticality,
% 		     value=Value}}} ->
% 	    DecGeneral = decode_initiating_iu(ProcedureCode,Criticality,Value)
%     end,
%     io:format("~w~n~n", [DecGeneral]).

decode_initiating_iu(ProcedureCode,Crit,Val) ->
    case ProcedureCode of
        1 -> % Iu-Release
	    wrapper_decode(?ranap, 'Iu-ReleaseComplete',Val)
    end.





run_com_id() ->
    COM = common_id(),
    io:format("~w~n~n", [COM]),
    RanapCOM = ranap_pdu_contents(COM, 15),                     % 15 = CommonID
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapCOM),
    EncRanapCOM = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapCOM]),
	{ok,{initiatingMessage,
	     #'InitiatingMessage'{procedureCode=ProcedureCode,
				  criticality=Criticality,
				  value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapCOM),
    io:format("DecGeneral: ~w~n~n", [DecGeneral]),
    case DecGeneral of
	{'CommonID',[{_ProtIEField,23,_Crit,DecCause}],_ASN1novalue} ->
	    io:format("Cause: ~w~n~n", [DecCause]);
	Error ->
	    io:format("Error: ~w~n~n", [Error])
    end.
    

decode_common_id(ProcedureCode,Crit,Val) ->
    case ProcedureCode of
        15 -> % Common ID
	    wrapper_decode(?ranap, 'CommonID',Val);
	Error ->
	    io:format("Error: ~w~n~n", [Error])
    end.




run_sm_cmd() ->
    DIR = security_mode_cmd(),
    io:format("~w~n~n", [DIR]),
    RanapDIR = ranap_pdu_contents(DIR, 6),                  % 6 = Sec Mode
    io:format("~w~n~n", [RanapDIR]),
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapDIR),
    EncRanapDIR = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapDIR]),
    {ok,{initiatingMessage, 
	 #'InitiatingMessage'{procedureCode=ProcedureCode,
			      criticality=Criticality,
			      value=DecGeneral}}} = 
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapDIR),
    io:format("~w~n~n", [DecGeneral]),
    case DecGeneral of
	{'SecurityModeCommand',
	 [{_ProtIEField1,12,_Crit1,DecIPI},
	  {_ProtIEField2,11,_Crit2,DecEI},
	  {_ProtIEField3,75,_Crit3,DecKS}],
	 _ASN1novalue} ->
	    io:format("Integrity Protection Algoritm: ~w~n~n", [DecIPI]),
	    io:format("Encryption Information: ~w~n~n", [DecEI]),
	    io:format("Key Status: ~w~n~n", [DecKS])
    end,
    ok.

run_sm_cmp() ->
    DIR = security_mode_cmp(),
    io:format("~w~n~n", [DIR]),
    RanapDIR = ranap_pdu_contents_suc(DIR, 6),                     % 6 = Sec Mode
    io:format("~w~n~n", [RanapDIR]),
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapDIR),
    EncRanapDIR = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapDIR]),
    {ok,{successfulOutcome,
	 #'SuccessfulOutcome'{procedureCode=ProcedureCode,
			      criticality=Criticality,
			      value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapDIR),
    io:format("~w~n~n", [DecGeneral]),
    case DecGeneral of
	{'SecurityModeComplete',
	 [{_ProtIEField1,6,_Crit1,DecIPA},
	  {_ProtIEField2,5,_Crit2,DecEI}],
	 _ASN1novalue} ->
	    io:format("Integrity Protection Algoritm: ~w~n~n", [DecIPA]),
	    io:format("Encryption Information: ~w~n~n", [DecEI])
    end,
    ok.

run_sm_rej() ->
    DIR = security_mode_rej(),
    io:format("~w~n~n", [DIR]),
    RanapDIR = ranap_pdu_contents_unsuc(DIR, 6),                     % 6 = Sec Mode
    io:format("~w~n~n", [RanapDIR]),
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapDIR),
    EncRanapDIR = lists:flatten(Tmp2),
    io:format("~w~n~n", [EncRanapDIR]),
    {ok,{unsuccessfulOutcome,
	 #'UnsuccessfulOutcome'{procedureCode=ProcedureCode,
				criticality=Criticality,
				value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapDIR),
    io:format("~w~n~n", [DecGeneral]),
%     case DecGeneral of
% 	{ok,{'SecurityModeReject',
% 	      [{_ProtIEField1,6,_Crit1,Value1},
% 	       {_ProtIEField2,5,_Crit2,Value2}],
% 	      _ASN1novalue}} ->
% 	    io:format("Value1: ~w~n~n", [Value1]),
% 	    io:format("Value2: ~w~n~n", [Value2]),
% 	    {ok,DecIPA} = wrapper_decode(?ranap, 'IntegrityProtectionAlgorithm', Value1),
% 	    {ok,DecEI} = wrapper_decode(?ranap, 'EncryptionAlgorithm', Value2),
% 	    io:format("Integrity Protection Algoritm: ~w~n~n", [DecIPA]),
% 	    io:format("Encryption Information: ~w~n~n", [DecEI])
%     end,
    ok.

decode_initiating_sm(ProcedureCode,Crit,Val) ->
    io:format("ProcedureCode: ~w~n~n", [ProcedureCode]),
    case ProcedureCode of
        6 -> % Sec Mode
	    wrapper_decode(?ranap, 'SecurityModeCommand',Val)
    end.

decode_suc_sm(ProcedureCode,Criticality,Value) ->
    io:format("ProcedureCode: ~w~n~n", [ProcedureCode]),
    case ProcedureCode of
        6 -> % Sec Mode
	    wrapper_decode(?ranap, 'SecurityModeComplete',Value)
    end.

decode_unsuc_sm(ProcedureCode,Criticality,Value) ->
    io:format("ProcedureCode: ~w~n~n", [ProcedureCode]),
    case ProcedureCode of
        6 -> % Sec Mode
	    wrapper_decode(?ranap, 'SecurityModeReject',Value)
    end.







run_reset() ->
    IUP = reset(),
    io:format("Reset: ~w~n~n", [IUP]),
    RanapIUP = ranap_pdu_contents(IUP, 9),                % 9 = Reset
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapIUP),
    EncRanapIUP = lists:flatten(Tmp2),
    io:format("Coded Reset: ~w~n~n", [EncRanapIUP]),
    {ok,{initiatingMessage, 
	 #'InitiatingMessage'{procedureCode=ProcedureCode,
		    criticality=Criticality,
		    value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapIUP), 
    io:format("Decoded Reset: ~w~n~n", [DecGeneral]),

    IUP1 = reset_ack(),
    io:format("Reset Ack:~w~n~n", [IUP1]),
    RanapIUP1 = ranap_pdu_contents_suc(IUP1, 9),          % 9 = Reset
    {ok, Tmp21} = wrapper_encode(?ranap, 'RANAP-PDU', RanapIUP1),
    EncRanapIUP1 = lists:flatten(Tmp21),
    io:format("Coded Reset Ack: ~w~n~n", [EncRanapIUP1]),
    {ok,{successfulOutcome, 
	 #'SuccessfulOutcome'{procedureCode=ProcedureCode1,
		    criticality=Criticality1,
		    value=DecGeneral1}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapIUP1),
    io:format("Decoded Reset Ack: ~w~n~n", [DecGeneral1]).

% decode_init_reset(ProcedureCode,Crit,Val) ->
%     io:format("ProcedureCode: ~w~n~n", [ProcedureCode]),
%     case ProcedureCode of
%         9 -> % reset
% 	    wrapper_decode(?ranap, 'Reset',Val)
%     end.

% decode_init_reset_ack(ProcedureCode,Crit,Val) ->
%     io:format("ProcedureCode: ~w~n~n", [ProcedureCode]),
%     case ProcedureCode of
%         9 -> % reset
% 	    wrapper_decode(?ranap, 'ResetAcknowledge',Val)
%     end.









run_reset_res() ->
    IUP = reset_res([12,13,14,15,16,17,18,19,20]),
    io:format("Reset Rsource: ~w~n~n", [IUP]),
    RanapIUP = ranap_pdu_contents(IUP, 27),            % 27 = Reset Res
    {ok, Tmp2} = wrapper_encode(?ranap, 'RANAP-PDU', RanapIUP),
    EncRanapIUP = lists:flatten(Tmp2),
    io:format("Coded Reset Resource: ~w~n~n", [EncRanapIUP]),
    {ok,{initiatingMessage, 
	 #'InitiatingMessage'{procedureCode=ProcedureCode,
			      criticality=Criticality,
			      value=DecGeneral}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapIUP),
    io:format("Decoded Reset Resource: ~w~n~n", [DecGeneral]),
%     case DecGeneral of
% 	{ok,{'ResetResource',
% 	     [{'ProtocolIE-Field',3,ignore,Value1},
% 	      {'ProtocolIE-Field',4,ignore,Value2},
% 	      {'ProtocolIE-Field',77,ignore,Value3},
% 	      {'ProtocolIE-Field',86,ignore,Value4}],
% 	     asn1_NOVALUE}} ->
% 	    io:format("Value1: ~w~n~n", [Value1]),
% 	    io:format("Value2: ~w~n~n", [Value2]),
% 	    io:format("Value3: ~w~n~n", [Value3]),
% 	    io:format("Value4: ~w~n~n", [Value4]),
% 	    {ok,DecIPA} = wrapper_decode(?ranap, 'CN-DomainIndicator', Value1),
% 	    {ok,{_Radio,DecEI}} = wrapper_decode(?ranap, 'Cause', Value2),
% 	    {ok,DecRR} = wrapper_decode(?ranap, 'ResetResourceList', Value3),
% 	    DecRRI = case DecRR of
% 			 [[{'ProtocolIE-Field',78,ignore,Value5}]] ->
% 			     {ok,{_apa,DecRI,_asn1novalue}} = 
% 				 wrapper_decode(?ranap, 'ResetResourceItem', Value5),
% 			     DecRI
% 		     end,
% 	    {ok,{_Gl_id,PLMN_ID,RNC_ID}} = wrapper_decode(?ranap, 'GlobalRNC-ID', Value4),
% 	    io:format("CN-DomainIndicator: ~w~n~n", [DecIPA]),
% 	    io:format("Cause: ~w~n~n", [DecEI]),
% 	    io:format("ResetResourceList: ~w~n~n", [DecRR]),
% 	    io:format("  ResetResourceItem: ~w~n~n", [DecRRI]),
% 	    io:format("GlobalRNC-ID: PLMN_ID: ~w, RNC_ID: ~w~n~n", [PLMN_ID,RNC_ID])
%     end,

    RSA = reset_res_ack([12,13,14,15,16,17,18,19,20]),
    io:format("~n~nReset Resource Ack:~w~n~n", [RSA]),
    RanapRSA = ranap_pdu_contents_suc(RSA, 27),             % 27 = Reset Res
    {ok, Tmp12} = wrapper_encode(?ranap, 'RANAP-PDU', RanapRSA),
    EncRanapRSA = lists:flatten(Tmp12),
    io:format("Coded Reset Resource Ack: ~w~n~n", [EncRanapRSA]),
    {ok,{successfulOutcome,
	 #'SuccessfulOutcome'{procedureCode=ProcedureCode1,
			      criticality=Criticality1,
			      value=DecGeneral1}}} =
	wrapper_decode(?ranap, 'RANAP-PDU', EncRanapRSA),
    io:format("Decoded Reset Resource Ack: ~w~n~n", [DecGeneral1]).
%     case DecGeneral1 of
% 	{ok,{'ResetResourceAcknowledge',
% 	     [{'ProtocolIE-Field',3,ignore,Value12},
% 	      {'ProtocolIE-Field',77,ignore,Value32}],
% 	     asn1_NOVALUE}} ->
% 	    io:format("Value1: ~w~n~n", [Value12]),
% 	    io:format("Value3: ~w~n~n", [Value32]),
% 	    {ok,DecIPA2} = wrapper_decode(?ranap, 'CN-DomainIndicator', Value12),
% 	    {ok,DecRR2} = wrapper_decode(?ranap, 'ResetResourceList', Value32),
% 	    DecRRI2 = case DecRR2 of
% 			 [[{'ProtocolIE-Field',78,ignore,Value52}]] ->
% 			     {ok,{_apa2,DecRI2,_asn1novalue2}} = 
% 				 wrapper_decode(?ranap, 'ResetResourceItem', Value52),
% 			     DecRI2
% 		     end,
% 	    io:format("CN-DomainIndicator: ~w~n~n", [DecIPA2]),
% 	    io:format("ResetResourceList: ~w~n~n", [DecRR2]),
% 	    io:format("  ResetResourceItem: ~w~n~n", [DecRRI2])
%     end.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Code for constructing RANAP messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rab_release_request() ->
     #'RAB-AssignmentRequest'{
       protocolIEs = rab_assign_request_release_ies()
    }.

rab_assign_request_release_ies() ->
    [rab_assign_release_rab_release_list()].

rab_assign_release_rab_release_list() ->
    #'ProtocolIE-Field'{
      id = 41,               % 41 = id-RAB-ReleaseList
      criticality = ignore,%XXX
      value = rab_release_list()
    }.

rab_release_list() ->
    [release_lists()].

release_lists() ->
    [rab_release_item_ies()].

rab_release_item_ies() ->
    #'ProtocolIE-Field'{
        id = 40,                                % 40 = id-RAB-ReleaseItem
	criticality = ignore,%XXX
	value = rab_release_item()}.
  
rab_release_item() ->
	   #'RAB-ReleaseItem'{'rAB-ID' = rab_id(),
			      cause = cause_nas()}.  

cause_nas() ->
    {nAS, 'normal-release'}.






rab_create_request(Rabid, Teid, SgsnIP) ->
    #'RAB-AssignmentRequest'{
		    protocolIEs = 
		    rab_assign_create_request_ies(Rabid, Teid, SgsnIP)
		   }.

rab_assign_create_request_ies(Rabid, Teid, SgsnIP) ->
    [rab_assign_setup_or_modify_list(Rabid, Teid, SgsnIP)].

rab_assign_setup_or_modify_list(Rabid, Teid, SgsnIP) ->
    #'ProtocolIE-Field'{
       id = 54,                                  %id-RAB-SetupOrModifyList
       criticality = ignore,%XXX
       value = rab_setup_or_modify_list(Rabid, Teid, SgsnIP)
       }.

rab_setup_or_modify_list(Rabid, Teid, SgsnIP) ->
    [setup_or_modify_lists(Rabid, Teid, SgsnIP)].
 
setup_or_modify_lists(Rabid, Teid, SgsnIP) ->
      [rab_setup_or_modify_item_ies(Rabid, Teid, SgsnIP)].

rab_setup_or_modify_item_ies(Rabid, Teid, SgsnIP) ->
      #'ProtocolIE-FieldPair'{
      id = 53, 
      firstCriticality = reject, %{'Criticality',reject}, 
      firstValue = rab_setup_or_modify_item_first(Rabid, Teid, SgsnIP), 
      secondCriticality = ignore, %{'Criticality', ignore} 
      secondValue = rab_setup_or_modify_item_second()
     }.

rab_setup_or_modify_item_first(Rabid, Teid, SgsnIP) ->
    #'RAB-SetupOrModifyItemFirst'{
				'rAB-ID' = Rabid, %ras_rab_id(Rabid), 
				'rAB-Parameters' = rab_parameters(),
				userPlaneInformation = user_plane_information(), 
				transportLayerAddress = SgsnIP,
				iuTransportAssociation = ras_iu_transport_association(Teid)  
			       }.

ras_iu_transport_association(Teid) ->
    {'gTP-TEI', Teid}.

rab_id() ->
    [0,1,0,1,0,1,0,1].    

rab_parameters() ->
  #'RAB-Parameters'{
        trafficClass = background,
	'rAB-AsymmetryIndicator' = 'symmetric-bidirectional',
	maxBitrate = [200000],
	deliveryOrder = 'delivery-order-not-requested', 
	'maxSDU-Size' = 11, 
	'sDU-Parameters' = sdu_parameters(),
	trafficHandlingPriority = 14 %{'TrafficHandlingPriority', 14} %14=lowest
   }.

user_plane_information() ->
  #'UserPlaneInformation'{
    userPlaneMode = 'support-mode-for-predefined-SDU-sizes', 
    'uP-ModeVersions' = 2#1010101010101010 
  }.

transport_layer_address() ->
  [1,1,0,0,1,1,0,0,1,1,1,1,0,0,0,0,1,0,1,0,1,0,1,0,0,0,0,0,1,1,1,1].
%  1               2               3             4

iu_transport_association() ->
    {'gTP-TEI', [31,32,33,34]}.

sdu_parameters() ->
    [#'SDU-Parameters_SEQOF'{
       'sDU-ErrorRatio' = sdu_error_ratio(),
       residualBitErrorRatio = residual_bit_error_ratio(),
       deliveryOfErroneousSDU = no
    }].

sdu_error_ratio() ->
   #'SDU-ErrorRatio'{
      mantissa = 9,
      exponent = 4
   }.

residual_bit_error_ratio() -> 
    #'ResidualBitErrorRatio'{
	mantissa = 9,
	exponent = 5
    }.

%allocationOrRetentionPriority() ->
%  #'AllocationOrRetentionPriority'{
%    priorityLevel = lowest, 
%    'pre-emptionCapability' = {'Pre-emptionCapability','can-trigger-pre-emption'}, 
%    'pre-emptionVulnerability' = {'Pre-emptionVulnerability','not-vulnerable-to-pre-emption'}, 
%    queuingAllowed = {'QueuingAllowed','queueing-allowed'}
%  }.

rab_setup_or_modify_item_second() -> % returns OpenType value
    #'RAB-SetupOrModifyItemSecond'{
				 'pDP-TypeInformation' = ['ipv4'],
				 dataVolumeReportingIndication = 'do-not-report', 
				 'dl-GTP-PDU-SequenceNumber' = 1, 
				 'ul-GTP-PDU-SequenceNumber' = 2, 
				 'dl-N-PDU-SequenceNumber' = 0, 
				 'ul-N-PDU-SequenceNumber' = 0 
				}.




rab_assignment_response_setup() ->
     #'RAB-AssignmentResponse'{
       protocolIEs = rab_assignement_response_ies_setup()
    }.

rab_assignement_response_ies_setup() ->
    [rab_ass_rsp_setup_or_modified_list()].

rab_ass_rsp_setup_or_modified_list() ->
  #'ProtocolIE-Field'{
    id = 52,                                  % 52=RAB-AssignRABSetupOrModifiedList
    criticality = ignore,%XXX
    value = rab_setup_or_modified_list()
  }.

rab_setup_or_modified_list() ->
    [rab_setup_or_modified_item_list()].

rab_setup_or_modified_item_list() ->
    [rab_setup_or_modified_item_ies()].

rab_setup_or_modified_item_ies() ->
    #'ProtocolIE-Field'{
       id = 51,                                    % 51 = RAB-SetupOrModifiedItemIEs
       criticality = reject, %{'Criticality', reject},
       value = rab_setup_or_modified_item()
    }.

rab_setup_or_modified_item() ->
    #'RAB-SetupOrModifiedItem'{
			    'rAB-ID' = rab_id(),
			    transportLayerAddress = transport_layer_address(),
			    iuTransportAssociation = iu_transport_association()
			   }.






rab_assignment_response_delete() ->
     #'RAB-AssignmentResponse'{
       protocolIEs = rab_assignement_response_ies_delete()
    }.

rab_assignement_response_ies_delete() ->
    [rab_ass_rsp_rab_release_list()].

rab_ass_rsp_rab_release_list() ->
    #'ProtocolIE-Field'{
      id = 43,                                  % 41 = RAB-AssignRABReleasedList
      criticality = ignore,%XXX
      value = rab_released_list()
  }.

rab_released_list() ->
    [released_lists()].

released_lists() ->
    [rab_released_item_ies()].      % 'ReleasedLists'

rab_released_item_ies() ->
    #'ProtocolIE-Field'{
      id = 42,                                  % 42 = RAB-ReleaseItemIEs
      criticality = ignore,%XXX
      value = rab_released_item()
    }.
  
rab_released_item() ->
    #'RAB-ReleasedItem'{
		   'rAB-ID' = rab_id()
		  }.



initial_ue() ->
    #'InitialUE-Message'{
	    protocolIEs = initial_ue_ies()
	   }.

initial_ue_ies() ->
    [cn_domain_ind(), 
     init_lai(), 
     init_rac(), 
     init_sai(), 
     init_nas_pdu(),
     init_iu_sign_con_id(),
     init_global_rnc_id_initial_ue()].


cn_domain_indicator() ->
    'ps-domain'.
    
init_lai() ->
  #'ProtocolIE-Field'{
    id = 15,                                 % 15 = LAI
    criticality = ignore,%XXX
    value = lai()
  }.

lai() ->
    #'LAI'{'pLMN-ID' = [25,26,27],
	   lAC = [25,26]}.

init_rac() ->
  #'ProtocolIE-Field'{
    id = 55,                                % 55 = RAC 
    criticality = ignore,%XXX
    value = rac()
  }.

rac() ->
    [25].

init_sai() ->
  #'ProtocolIE-Field'{
    id = 58,                                 % 58 = SAI
    criticality = ignore,%XXX
    value = sai()
  }.

sai() ->
    #'SAI'{'pLMN-ID' = [28,29,30],                  %KOLLA!!!!
	   lAC = [30,31],
	   sAC = [32,33]}.

init_nas_pdu() ->
  #'ProtocolIE-Field'{
    id = 16,                              % 16 = NAS-PDU
    criticality = ignore,%XXX      
    value = pdu_ar()
  }.

pdu_ac()->
    [16#08,
     16#03].

pdu_ar() ->
    [16#08,		% Skip indicator | Protocol discriminator 
     16#01,		% Attach Request
     16#01,16#b7,	% MsNetworkCapability
     16#21,      	% Cksn | AttachType
     16#55,16#06,	% DrxParameter
     16#05,16#61,16#86,16#14,16#09,16#f7, % MsId
     16#21,16#63,16#54,16#ac,16#dc,16#d5, % OldRai
     16#0a,16#f0,16#f1,16#f2,16#f3,16#f4, % MsRaCap
     16#f5,16#f6,16#f7,16#f8,16#f9        % MsRaCap continues
     %%16#19,16#ab,16#cd,16#ef,             % P-TMSI signature
     %%16#17,16#5b,                         % Requested READY timer 
     %%16#91                                % TMSI status
    ].

pdu_pdp() ->
    [2#01001010,  % Transaction_id | Protocol_disc
     2#01000001,  % Message_type
     2#00001001,  % Nsapi
     1,           % Llc_sapi (dummie_value)
     1,1,1,1,     % QoS dummie_value=1 (19 octets)
     1,1,1,1,1,
     1,1,1,1,1,
     1,1,1,1,1,
     7,           % PDP_Address (Ltot=7),
     2#00000001,  % ietf/etsi organisation
     2#00100001,  % IPv4 
     198,         % IP address MSD
     124,
     56,
     124,              % IP address LSD
     1,                % APN optional (octet1=IEI)
     25,               % APN total length
     4,                % length lable1
     116,101,115,116,  % lable1 = test
     4,                % length lable2
     116,101,115,116,  % lable2 = test
     4,                % lenght lable3
     116,101,115,116,  % lable3 = test
     4,                % length lable3
     116,101,115,116,  % lable4 = test
     4,                % length label5
     103,112,114,115
    ].

pdu_auth_rsp() ->
    [8,16#13,0,1,2,3,16#21,2,0,1].

pdu_auth_fail() ->
    [8,16#1C,1,16#22,2,0,1].

init_iu_sign_con_id() ->
  #'ProtocolIE-Field'{
    id = 79,                              % 79 = id-IuSigConId 
    criticality = ignore,%XXX      
    value = iu_sign_con_id()
  }.

iu_sign_con_id() ->
    53245.
% 		       [1,0,1,0,1,0,1,0,
% 			1,0,1,0,1,0,1,0,
% 			1,0,1,0,1,0,1,1]

init_global_rnc_id_initial_ue() ->
  #'ProtocolIE-Field'{
    id = 86,                              % 86 = id-GlobalRNC-ID 
    criticality = ignore,%XXX      
    value = global_rnc_id()
  }.
    
global_rnc_id() ->
    #'GlobalRNC-ID'{'pLMN-ID' = [10,11,12], 'rNC-ID' = 2048}.

direct_transfer_cn() ->
    #'DirectTransfer'{
		    protocolIEs = direct_transfer_cn_ies()
		   }.

direct_transfer_cn_ies() ->
    [dir_cn_nas_pdu(),
     dir_lai(),
     dir_rac()].

dir_cn_nas_pdu() ->
    #'ProtocolIE-Field'{
      id = 16,                                % 16 = id-NAS-PDU
      criticality = ignore,%XXX
      value = pdu_auth_fail()
    }.

dir_lai() ->
    #'ProtocolIE-Field'{
      id = 15,                                % 15 = id-LAI
      criticality = ignore,%XXX
      value = lai()
    }.

dir_rac() ->
    #'ProtocolIE-Field'{
      id = 55,                                % 55 = id-RAC
      criticality = ignore,%XXX
      value = rac()
    }.






direct_transfer_rnc() ->
    #'DirectTransfer'{
		     protocolIEs = direct_transfer_rnc_ies()
		    }.

direct_transfer_rnc_ies() ->
    [dir_rnc_nas_pdu(),
     dir_sapi()].

dir_rnc_nas_pdu() ->
    #'ProtocolIE-Field'{
      id = 16,                                % 16 = id-NAS-PDU
      criticality = ignore,%XXX
      value = pdu_auth_rsp()
    }.

dir_sapi() ->
    #'ProtocolIE-Field'{
      id = 59,                                % 59 = id-SAPI
      criticality = ignore,%XXX
      value = sapi()
    }.

sapi() ->
    'normal-priority'.





iu_release_command() ->
    #'Iu-ReleaseCommand'{
		    protocolIEs = iu_rel_command_ies()
		   }.

iu_rel_command_ies() ->
    [iu_relcomcause_nas()].

iu_relcomcause_nas() ->
    #'ProtocolIE-Field'{
        id = 4,                                   % 4 = Id-Cause
        criticality = ignore,%XXX      
	value = cause()
    }.

cause() ->
    cause_nas().
    
% cause() ->
%     {ok,Bytes} = 
%  	wrapper_encode(?ranap,'CauseNAS', 
% 		      {'CauseNAS', 'normal-release'}),   
%     Bytes.
    





iu_release_complete() ->
    #'Iu-ReleaseComplete'{
		     protocolIEs = iu_rel_comp_ies()
		    }.

iu_rel_comp_ies() ->
  [].
%    'iu-RelCompRABDataVolumeReportList' = asn1_NOVALUE,
%    'iu-RelCompRABReleasedListIuRelComp' = asn1_NOVALUE,
%    'iu-RelCompCriticalityDiagnostics' = asn1_NOVALUE
%  }.








common_id() ->
    #'CommonID'{
	   protocolIEs = common_id_ies()
	  }.

common_id_ies() ->
    [common_id_imsi()].

common_id_imsi() ->
    #'ProtocolIE-Field'{
        id = 23,                            % 23 = Perm. NAS ID (IMSI)
        criticality = ignore, %ignore,%XXX      %XXX
	value = imsi()
    }.

imsi() -> 
    {iMSI, [1,2,3,4,5,6,7,8]}.   










security_mode_cmd() ->
    #'SecurityModeCommand'{
		   protocolIEs = security_mode_cmd_ies()
		  }.

security_mode_cmd_ies() ->
     [security_mode_cmd_integ(),
      security_mode_cmd_encr(),
      security_mode_cmd_keystat()
     ].

security_mode_cmd_integ() ->
    #'ProtocolIE-Field'{
        id = 12,                            % 12 = Integ info
        criticality = ignore,%{'Criticality', ignore},      
	value = integ_info()
    }.

integ_info() ->
    #'IntegrityProtectionInformation'{
	    permittedAlgorithms = perm_integ_algs(),
	    key = key() }.

perm_integ_algs() ->
    [integ_prot_alg()].    

integ_prot_alg() ->
    'standard-UMTS-integrity-algorithm-UIA1'.

key() ->
    [1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
     1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
     1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
     1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
     1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
     1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
     1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
     1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0].



security_mode_cmd_encr() ->
    #'ProtocolIE-Field'{
        id = 11,                            % 11 = Encr info
        criticality = ignore,%{'Criticality', ignore},      
	value = encr_info()
    }.

encr_info() ->
    #'EncryptionInformation'{
	   permittedAlgorithms = perm_encr_algs(),
	   key = key() }.

perm_encr_algs() ->
    [encr_prot_alg()].    

encr_prot_alg() ->
    'standard-UMTS-encryption-algorith-UEA1'.

security_mode_cmd_keystat() ->
    #'ProtocolIE-Field'{
        id = 75,                            % 75 = id-KeyStatus
        criticality = ignore,%{'Criticality', ignore},      
	value = key_status()
    }.

key_status() ->
    new.





security_mode_cmp() ->
    #'SecurityModeComplete'{
		   protocolIEs = security_mode_cmp_ies()
		  }.

security_mode_cmp_ies() ->
    [security_mode_cmp_ch_integ_prot_alg(),
     security_mode_cmp_ch_encr_alg()
    ].

security_mode_cmp_ch_integ_prot_alg() ->
    #'ProtocolIE-Field'{
        id = 6,                            % 6 = Chosen Integ prot alg
        criticality = ignore,%XXX      
	value = ch_integ_prot_alg()
    }.

ch_integ_prot_alg() ->
    'standard-UMTS-integrity-algorithm-UIA1'.


security_mode_cmp_ch_encr_alg() ->    
    #'ProtocolIE-Field'{
        id = 5,                            % 5 = Chosen Encr alg
        criticality = ignore,%XXX      
	value = ch_encr_alg()
    }.

ch_encr_alg() ->
    'standard-UMTS-encryption-algorith-UEA1'.




security_mode_rej() ->
    #'SecurityModeReject'{
		   protocolIEs = security_mode_rej_ies()
		  }.

security_mode_rej_ies() ->
    [security_mode_rej_cause()].


security_mode_rej_cause() ->
    #'ProtocolIE-Field'{
        id = 4,                                % 4 = Id-Cause
        criticality = ignore,%XXX      
	value = cause_radio()                   % Se Reset  
    }.








reset() ->
    #'Reset'{
       protocolIEs = reset_ies()
      }.
reset_ies() ->
    [reset_cause(),
     cn_domain_ind(),     % Se initial Ue
     init_global_rnc_id() %  ---- " ----
    ].

init_global_rnc_id() ->
  #'ProtocolIE-Field'{
    id = 86,                              % 86 = id-GlobalRNC-ID 
    criticality = ignore,%XXX      
    value = global_rnc_id()
  }.

reset_cause() ->
    #'ProtocolIE-Field'{
        id = 4,                            % 4 = id-Cause
        criticality = ignore,%XXX      
	value = cause_radio()
    }.
%cause_open() ->
%    {ok,Bytes} = 
%	wrapper_encode(?ranap,'Cause', cause_radio()),
%    Bytes.
cause_radio() ->
    {radioNetwork, 'release-due-to-utran-generated-reason'}.


cn_domain_ind(IEs) when atom(IEs)->
    setelement(1,cn_domain_ind(),IEs).

cn_domain_ind() ->
  #'ProtocolIE-Field'{
    id = 3,                                    % 3 = InitUE-MessageIEsCN-DomainIndicator
    criticality = ignore,%XXX
    value = cn_domain_indicator()
  }.




reset_ack() ->
    #'ResetAcknowledge'{
	   protocolIEs = reset_ack_ies()
	  }.
reset_ack_ies() ->
    [cn_domain_ind()].    % Se initial Ue





reset_res(IuSCId) ->
    #'ResetResource'{
	   protocolIEs = reset_res_ies(IuSCId)
	  }.

reset_res_ies(IuSCId) ->
    [
     cn_domain_ind()       % Se initial Ue
     ,reset_cause()        % Se reset
     ,reset_res_list(IuSCId)
     ,init_global_rnc_id_reset_res() %  ---- " ----
    ].

init_global_rnc_id_reset_res() ->
  #'ProtocolIE-Field'{
    id = 86,                              % 86 = id-GlobalRNC-ID 
    criticality = ignore,%XXX      
    value = global_rnc_id()
  }.

reset_res_list(IuSCId) ->
    #'ProtocolIE-Field'{
      id = 77,               % 77 = id-IuSigConIdList
      criticality = ignore,%XXX
      value = res_list(IuSCId)
    }.

res_list(IuSCId) ->
    iu_Sig_Con_Id_list(IuSCId,[]).

iu_Sig_Con_Id_list([],List) ->
    List;

iu_Sig_Con_Id_list([IuSCId|T],List) ->
    Ie = [iu_Sig_Con_Id_ie(IuSCId)],
    iu_Sig_Con_Id_list(T,[Ie|List]).

iu_Sig_Con_Id_ie(IuSCId) ->
    #'ProtocolIE-Field'{
        id = 78,              % 78 = id-IuSigConIdItem
	criticality = ignore,%XXX
	value = iu_Sig_Con_Id_item(IuSCId)}.
  
iu_Sig_Con_Id_item(IuSCId) ->
	   #'ResetResourceItem'{
        	iuSigConId = IuSCId
% 53432
% 			     [1,0,1,0,1,0,1,0,
% 			      1,0,1,0,1,0,1,0,
% 			      1,0,1,0,1,0,1,0]
			      }.  


reset_res_ack(IuSCId) ->
     #'ResetResourceAcknowledge'{
	       protocolIEs = reset_res_ack_ies(IuSCId)
	      }.
reset_res_ack_ies(IuSCId) ->
    [
     cn_domain_ind()       % Se initial Ue
     ,reset_res_list(IuSCId)     % Se Reset Ressource
    ].


int2bin(Int) ->
    EmptyList = [],
    BitList_b1 = [Int band 2#1 | EmptyList],
    BitList_b2 = [(Int bsr 1) band 2#1 | BitList_b1],
    BitList_b3 = [(Int bsr 2) band 2#1 | BitList_b2],
    BitList_b4 = [(Int bsr 3) band 2#1 | BitList_b3],
    BitList_b5 = [(Int bsr 4) band 2#1 | BitList_b4],
    BitList_b6 = [(Int bsr 5) band 2#1 | BitList_b5],
    BitList_b7 = [(Int bsr 6) band 2#1 | BitList_b6],
    BitList = [(Int bsr 7) band 2#1 | BitList_b7],
    io:format("~n~w~n", [BitList]).


%%%%%%%%%%%%%%%%% wrappers %%%%%%%%%%%%%%%%%%%%%%%%

wrapper_encode(Module,Type,Value) ->
    case asn1rt:encode(Module,Type,Value) of
	{ok,X} when binary(X) ->
	    {ok, binary_to_list(X)};
	{ok,X} ->
	    {ok, binary_to_list(list_to_binary(X))};
	Error ->
	    Error
    end.

wrapper_decode(Module, Type, Bytes) when is_binary(Bytes) ->
    asn1rt:decode(Module, Type, Bytes);
wrapper_decode(Module, Type, Bytes) when is_list(Bytes) ->
    asn1rt:decode(Module, Type, list_to_binary(Bytes)).
