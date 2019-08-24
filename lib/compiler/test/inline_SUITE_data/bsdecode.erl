%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
-module(bsdecode).
-export([?MODULE/0]).

-record(protocolErrors, {invalidManIE = false,
                         outOfSequence = false,
                         incorrectOptIE = false}).

-record(mvsT_msisdn, {value}).


-record(mvsgT_pdpAddressType, {pdpTypeNbr,
                               address}).

-record(mvsgT_ipAddress, {version,
                          a1,
                          a2,
                          a3,
                          a4,
                          a5,
                          a6,
                          a7,
                          a8}).

-record(mvsgT_imsi, {value}).

-record(mvsgT_tid, {imsi,
                    nsapi}).

-record(sesT_qualityOfServiceV0, {delayClass,
                                  reliabilityClass,
                                  peakThroughput,
                                  precedenceClass,
                                  meanThroughput}).

-record(sesT_deleteReqV0, {tid}).

-record(sesT_deleteResV0, {tid,
                           cause}).

-record(sesT_createReqV0, {tid,
                           tidRaw,
                           qos,
                           recovery,
                           selMode,
                           flowLblData,
                           flowLblSig,
                           endUserAdd,
                           accPointName,
                           protConOpt,
                           sgsnAddSig,
                           sgsnAddUser,
                           msisdn}).

-record(sesT_updateReqV0, {tid,
                           tidRaw,
                           qos,
                           recovery,
                           flowLblData,
                           flowLblSig,
                           sgsnAddSig,
                           sgsnAddUser}).

-record(masT_ipcpData, {type,
                        ipAddress,
                        rawMessage}).

-record(masT_ipcp, {exists,
                    code,
                    id,
                    ipcpList}).

-record(masT_pap, {exists,
                   code,
                   id,
                   username,
                   password}).

-record(masT_chap, {code,
                    id,
                    value,
                    name}).

-record(masT_protocolConfigOptions, {chap,
                                     pap,
                                     ipcp}).

?MODULE() ->
    Res = test(),
    {Res,Res =:=
     {ok,{sesT_createReqV0,{mvsgT_tid,{mvsgT_imsi,<<81,67,101,7,0,0,0,240>>},6},
	  [81,67,101,7,0,0,0,96],
	  {sesT_qualityOfServiceV0,1,4,9,2,18},0,
	  subscribed,0,0,{mvsgT_pdpAddressType,ietf_ipv4,[]},
	  [<<97,112,110,48,49,51,97>>,<<101,114,105,99,115,115,111,110>>,<<115,101>>],
	  {masT_protocolConfigOptions,[],
	   {masT_pap,true,1,5,[117,115,101,114,53],[112,97,115,115,53]},[]},
	  {mvsgT_ipAddress,ipv4,172,28,12,1,0,0,0,0},
	  {mvsgT_ipAddress,ipv4,172,28,12,3,0,0,0,0},
	  {mvsT_msisdn,<<145,148,113,129,0,0,0,0>>}},1}}.

test() ->
    Pdu = <<30,
            16,
            0,
            90,
            0,
            1,
            0,
            0,
            255,
            255,
            255,
            255,
            81,
            67,
            101,
            7,
            0,
            0,
            0,
            96,
            6,
            12,
            146,
            18,
            14,
            0,
            15,
            252,
            16,
            0,
            0,
            17,
            0,
            0,
            128,
            0,
            2,
            241,
            33,
            131,
            0,
            20,
            7,
            97,
            112,
            110,
            48,
            49,
            51,
            97,
            8,
            101,
            114,
            105,
            99,
            115,
            115,
            111,
            110,
            2,
            115,
            101,
            132,
            0,
            20,
            128,
            192,
            35,
            16,
            1,
            5,
            0,
            16,
            5,
            117,
            115,
            101,
            114,
            53,
            5,
            112,
            97,
            115,
            115,
            53,
            133,
            0,
            4,
            172,
            28,
            12,
            1,
            133,
            0,
            4,
            172,
            28,
            12,
            3,
            134,
            0,
            8,
            145,
            148,
            113,
            129,
            0,
            0,
            0,
            0>>,
    decode_v0_opt(10,Pdu).

decode_v0_opt(0,Pdu) ->
    decode_gtpc_msg(Pdu);
decode_v0_opt(N,Pdu) ->
    decode_gtpc_msg(Pdu),
    decode_v0_opt(N - 1,Pdu).

decode_gtpc_msg(<<0:3,
                  _:4,
                  0:1,
                  16:8,
                  _Length:16,
                  SequenceNumber:16,
                  _FlowLabel:16,
                  _SNDCP_N_PDU_Number:8,
                  _:3/binary-unit:8,
                  TID:8/binary-unit:8,
                  InformationElements/binary>>) ->
    Errors = #protocolErrors{},
    {ok,TID2} = tid_internal_storage(TID,[]),
    EmptyCreateReq = #sesT_createReqV0{tid = TID2,
                                       tidRaw = binary_to_list(TID)},
    case catch decode_ie_create(InformationElements,0,Errors,EmptyCreateReq) of
        {ok,CreateReq} ->
            {ok,CreateReq,SequenceNumber};
        {fault,Cause,CreateReq} ->
            {fault,Cause,CreateReq,SequenceNumber};
        {'EXIT',_Reason} ->
            {fault,193,EmptyCreateReq,SequenceNumber}
    end;
decode_gtpc_msg(<<0:3,
                  _:4,
                  0:1,
                  18:8,
                  _Length:16,
                  SequenceNumber:16,
                  _FlowLabel:16,
                  _SNDCP_N_PDU_Number:8,
                  _:3/binary-unit:8,
                  TID:8/binary-unit:8,
                  InformationElements/binary>>) ->
    io:format("hej",[]),
    Errors = #protocolErrors{},
    {ok,TID2} = tid_internal_storage(TID,[]),
    EmptyUpdateReq = #sesT_updateReqV0{tid = TID2,
                                       tidRaw = binary_to_list(TID)},
    case catch decode_ie_update(InformationElements,0,Errors,EmptyUpdateReq) of
        {ok,UpdateReq} ->
            {ok,UpdateReq,SequenceNumber};
        {fault,Cause,UpdateReq} ->
            {fault,Cause,UpdateReq,SequenceNumber};
        {'EXIT',Reason} ->
            io:format("hej",[]),
            {fault,193,EmptyUpdateReq,SequenceNumber,Reason}
    end;
decode_gtpc_msg(<<0:3,
                  _:4,
                  0:1,
                  20:8,
                  _Length:16,
                  SequenceNumber:16,
                  _FlowLabel:16,
                  _SNDCP_N_PDU_Number:8,
                  _:3/binary-unit:8,
                  TID:8/binary-unit:8,
                  _InformationElements/binary>>) ->
    {ok,TID2} = tid_internal_storage(TID,[]),
    DeleteReq = #sesT_deleteReqV0{tid = TID2},
    {ok,DeleteReq,SequenceNumber};
decode_gtpc_msg(<<0:3,
                  _:4,
                  0:1,
                  21:8,
                  _Length:16,
                  SequenceNumber:16,
                  _FlowLabel:16,
                  _SNDCP_N_PDU_Number:8,
                  _:3/binary-unit:8,
                  TID:8/binary-unit:8,
                  InformationElements/binary>>) ->
    Errors = #protocolErrors{},
    {ok,TID2} = tid_internal_storage(TID,[]),
    EmptyDeleteRes = #sesT_deleteResV0{tid = TID2},
    case catch decode_ie_delete_res(InformationElements,0,Errors,EmptyDeleteRes) of
        {ok,DeleteRes} ->
            {ok,DeleteRes,SequenceNumber};
        {fault,Cause,DeleteRes} ->
            {fault,Cause,DeleteRes,SequenceNumber};
        {'EXIT',_Reason} ->
            {fault,193,EmptyDeleteRes,SequenceNumber}
    end;
decode_gtpc_msg(_GTP_C_Message) ->
    {fault}.

decode_ie_create(<<>>,PresentIEs,Errors,CreateReq) ->
    if
        PresentIEs band 1917 /= 1917 ->
            {fault,202,CreateReq};
        true ->
            case Errors of
                #protocolErrors{invalidManIE = true} ->
                    {fault,201,CreateReq};
                #protocolErrors{outOfSequence = true} ->
                    {fault,193,CreateReq};
                #protocolErrors{incorrectOptIE = true} ->
                    {fault,203,CreateReq};
                _ ->
                    {ok,CreateReq}
            end
    end;
decode_ie_create(<<6:8,
                   QoSElement:3/binary-unit:8,
                   Rest/binary>>,PresentIEs,Errors,CreateReq) ->
    if
        PresentIEs band 1 == 1 ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 1 ->
            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
            <<_:2,
              DelayClass:3,
              ReliabilityClass:3,
              PeakThroughput:4,
              _:1,
              PrecedenceClass:3,
              _:3,
              MeanThroughput:5>> = QoSElement,
            QoS = #sesT_qualityOfServiceV0{delayClass = DelayClass,
                                           reliabilityClass = ReliabilityClass,
                                           peakThroughput = PeakThroughput,
                                           precedenceClass = PrecedenceClass,
                                           meanThroughput = MeanThroughput},
            UpdatedCreateReq = CreateReq#sesT_createReqV0{qos = QoS},
            decode_ie_create(Rest,PresentIEs bor 1,UpdatedErrors,UpdatedCreateReq);
        true ->
            <<_:2,
              DelayClass:3,
              ReliabilityClass:3,
              PeakThroughput:4,
              _:1,
              PrecedenceClass:3,
              _:3,
              MeanThroughput:5>> = QoSElement,
            QoS = #sesT_qualityOfServiceV0{delayClass = DelayClass,
                                           reliabilityClass = ReliabilityClass,
                                           peakThroughput = PeakThroughput,
                                           precedenceClass = PrecedenceClass,
                                           meanThroughput = MeanThroughput},
            UpdatedCreateReq = CreateReq#sesT_createReqV0{qos = QoS},
            decode_ie_create(Rest,PresentIEs bor 1,Errors,UpdatedCreateReq)
    end;
decode_ie_create(<<14:8,
                   Recovery:8,
                   Rest/binary>>,PresentIEs,Errors,CreateReq) ->
    if
        PresentIEs band 2 == 2 ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 2 ->
            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
            UpdatedCreateReq = CreateReq#sesT_createReqV0{recovery = Recovery},
            decode_ie_create(Rest,PresentIEs bor 2,UpdatedErrors,UpdatedCreateReq);
        true ->
            UpdatedCreateReq = CreateReq#sesT_createReqV0{recovery = Recovery},
            decode_ie_create(Rest,PresentIEs bor 2,Errors,UpdatedCreateReq)
    end;
decode_ie_create(<<15:8,
                   _:6,
                   SelectionMode:2,
                   Rest/binary>>,PresentIEs,Errors,CreateReq) ->
    if
        PresentIEs band 4 == 4 ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 4 ->
            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
            UpdatedCreateReq = CreateReq#sesT_createReqV0{selMode = selection_mode_internal_storage(SelectionMode)},
            decode_ie_create(Rest,PresentIEs bor 4,UpdatedErrors,UpdatedCreateReq);
        true ->
            UpdatedCreateReq = CreateReq#sesT_createReqV0{selMode = selection_mode_internal_storage(SelectionMode)},
            decode_ie_create(Rest,PresentIEs bor 4,Errors,UpdatedCreateReq)
    end;
decode_ie_create(<<16:8,
                   FlowLabel:16,
                   Rest/binary>>,PresentIEs,Errors,CreateReq) ->
    if
        PresentIEs band 8 == 8 ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 8 ->
            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
            UpdatedCreateReq = CreateReq#sesT_createReqV0{flowLblData = FlowLabel},
            decode_ie_create(Rest,PresentIEs bor 8,UpdatedErrors,UpdatedCreateReq);
        true ->
            UpdatedCreateReq = CreateReq#sesT_createReqV0{flowLblData = FlowLabel},
            decode_ie_create(Rest,PresentIEs bor 8,Errors,UpdatedCreateReq)
    end;
decode_ie_create(<<17:8,
                   FlowLabel:16,
                   Rest/binary>>,PresentIEs,Errors,CreateReq) ->
    if
        PresentIEs band 16 == 16 ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 16 ->
            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
            UpdatedCreateReq = CreateReq#sesT_createReqV0{flowLblSig = FlowLabel},
            decode_ie_create(Rest,PresentIEs bor 16,UpdatedErrors,UpdatedCreateReq);
        true ->
            UpdatedCreateReq = CreateReq#sesT_createReqV0{flowLblSig = FlowLabel},
            decode_ie_create(Rest,PresentIEs bor 16,Errors,UpdatedCreateReq)
    end;
decode_ie_create(<<128:8,
                   Length:16,
                   More/binary>>,PresentIEs,Errors,CreateReq) ->
    <<PDPElement:Length/binary-unit:8,
      Rest/binary>> = More,
    if
        PresentIEs band 32 == 32 ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 32 ->
            case pdp_addr_internal_storage(PDPElement) of
                {ok,PDPAddress} ->
                    UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
                    UpdatedCreateReq = CreateReq#sesT_createReqV0{endUserAdd = PDPAddress},
                    decode_ie_create(Rest,PresentIEs bor 32,UpdatedErrors,UpdatedCreateReq);
                {fault} ->
                    UpdatedErrors = Errors#protocolErrors{invalidManIE = true,
                                                          outOfSequence = true},
                    decode_ie_create(Rest,PresentIEs bor 32,UpdatedErrors,CreateReq)
            end;
        true ->
            case pdp_addr_internal_storage(PDPElement) of
                {ok,PDPAddress} ->
                    UpdatedCreateReq = CreateReq#sesT_createReqV0{endUserAdd = PDPAddress},
                    decode_ie_create(Rest,PresentIEs bor 32,Errors,UpdatedCreateReq);
                {fault} ->
                    UpdatedErrors = Errors#protocolErrors{invalidManIE = true},
                    decode_ie_create(Rest,PresentIEs bor 32,UpdatedErrors,CreateReq)
            end
    end;
decode_ie_create(<<131:8,
                   Length:16,
                   More/binary>>,PresentIEs,Errors,CreateReq) ->
    <<APNElement:Length/binary-unit:8,
      Rest/binary>> = More,
    if
        PresentIEs band 64 == 64 ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 64 ->
            case catch apn_internal_storage(APNElement,[]) of
                {ok,APN} ->
                    UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
                    UpdatedCreateReq = CreateReq#sesT_createReqV0{accPointName = APN},
                    decode_ie_create(Rest,PresentIEs bor 64,UpdatedErrors,UpdatedCreateReq);
                _ ->
                    UpdatedErrors = Errors#protocolErrors{outOfSequence = true,
                                                          invalidManIE = true},
                    decode_ie_create(Rest,PresentIEs bor 64,UpdatedErrors,CreateReq)
            end;
        true ->
            case catch apn_internal_storage(APNElement,[]) of
                {ok,APN} ->
                    UpdatedCreateReq = CreateReq#sesT_createReqV0{accPointName = APN},
                    decode_ie_create(Rest,PresentIEs bor 64,Errors,UpdatedCreateReq);
                _ ->
                    UpdatedErrors = Errors#protocolErrors{invalidManIE = true},
                    decode_ie_create(Rest,PresentIEs bor 64,UpdatedErrors,CreateReq)
            end
    end;
decode_ie_create(<<132:8,
                   Length:16,
                   More/binary>>,PresentIEs,Errors,CreateReq) ->
    <<ConfigurationElement:Length/binary-unit:8,
      Rest/binary>> = More,
    if
        PresentIEs band 128 == 128 ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 128 ->
            case catch pco_internal_storage(ConfigurationElement) of
                {ok,PCO} ->
                    UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
                    UpdatedCreateReq = CreateReq#sesT_createReqV0{protConOpt = PCO},
                    decode_ie_create(Rest,PresentIEs bor 128,UpdatedErrors,UpdatedCreateReq);
                _ ->
                    UpdatedErrors = Errors#protocolErrors{outOfSequence = true,
                                                          incorrectOptIE = true},
                    decode_ie_create(Rest,PresentIEs bor 128,UpdatedErrors,CreateReq)
            end;
        true ->
            case catch pco_internal_storage(ConfigurationElement) of
                {ok,PCO} ->
                    UpdatedCreateReq = CreateReq#sesT_createReqV0{protConOpt = PCO},
                    decode_ie_create(Rest,PresentIEs bor 128,Errors,UpdatedCreateReq);
                _ ->
                    UpdatedErrors = Errors#protocolErrors{incorrectOptIE = true},
                    decode_ie_create(Rest,PresentIEs bor 128,UpdatedErrors,CreateReq)
            end
    end;
decode_ie_create(<<133:8,
                   Length:16,
                   More/binary>>,PresentIEs,Errors,CreateReq) ->
    <<AddressElement:Length/binary-unit:8,
      Rest/binary>> = More,
    if
        PresentIEs band 768 == 768 ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 512 ->
            if
                PresentIEs band 256 == 0 ->
                    case gsn_addr_internal_storage(AddressElement) of
                        {ok,GSNAddr} ->
                            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
                            UpdatedCreateReq = CreateReq#sesT_createReqV0{sgsnAddSig = GSNAddr},
                            decode_ie_create(Rest,PresentIEs bor 256,UpdatedErrors,UpdatedCreateReq);
                        {fault} ->
                            UpdatedErrors = Errors#protocolErrors{invalidManIE = true,
                                                                  outOfSequence = true},
                            decode_ie_create(Rest,PresentIEs bor 256,UpdatedErrors,CreateReq)
                    end;
                true ->
                    case gsn_addr_internal_storage(AddressElement) of
                        {ok,GSNAddr} ->
                            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
                            UpdatedCreateReq = CreateReq#sesT_createReqV0{sgsnAddUser = GSNAddr},
                            decode_ie_create(Rest,PresentIEs bor 512,UpdatedErrors,UpdatedCreateReq);
                        {fault} ->
                            UpdatedErrors = Errors#protocolErrors{invalidManIE = true,
                                                                  outOfSequence = true},
                            decode_ie_create(Rest,PresentIEs bor 512,UpdatedErrors,CreateReq)
                    end
            end;
        PresentIEs < 256 ->
            case gsn_addr_internal_storage(AddressElement) of
                {ok,GSNAddr} ->
                    UpdatedCreateReq = CreateReq#sesT_createReqV0{sgsnAddSig = GSNAddr},
                    decode_ie_create(Rest,PresentIEs bor 256,Errors,UpdatedCreateReq);
                {fault} ->
                    UpdatedErrors = Errors#protocolErrors{invalidManIE = true},
                    decode_ie_create(Rest,PresentIEs bor 256,UpdatedErrors,CreateReq)
            end;
        true ->
            case gsn_addr_internal_storage(AddressElement) of
                {ok,GSNAddr} ->
                    UpdatedCreateReq = CreateReq#sesT_createReqV0{sgsnAddUser = GSNAddr},
                    decode_ie_create(Rest,PresentIEs bor 512,Errors,UpdatedCreateReq);
                {fault} ->
                    UpdatedErrors = Errors#protocolErrors{invalidManIE = true},
                    decode_ie_create(Rest,PresentIEs bor 512,UpdatedErrors,CreateReq)
            end
    end;
decode_ie_create(<<134:8,
                   Length:16,
                   More/binary>>,PresentIEs,Errors,CreateReq) ->
    <<MSISDNElement:Length/binary-unit:8,
      Rest/binary>> = More,
    if
        PresentIEs band 1024 == 1024 ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 1024 ->
            case msisdn_internal_storage(MSISDNElement,[]) of
                {ok,MSISDN} ->
                    UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
                    UpdatedCreateReq = CreateReq#sesT_createReqV0{msisdn = MSISDN},
                    decode_ie_create(Rest,PresentIEs bor 1024,UpdatedErrors,UpdatedCreateReq);
                {fault} ->
                    UpdatedErrors = Errors#protocolErrors{outOfSequence = true,
                                                          invalidManIE = true},
                    decode_ie_create(Rest,PresentIEs bor 1024,UpdatedErrors,CreateReq)
            end;
        true ->
            UpdatedCreateReq = CreateReq#sesT_createReqV0{msisdn = #mvsT_msisdn{value = MSISDNElement}},
            decode_ie_create(Rest,PresentIEs bor 1024,Errors,UpdatedCreateReq)
    end;
decode_ie_create(UnexpectedIE,PresentIEs,Errors,CreateReq) ->
    case check_ie(UnexpectedIE) of
        {defined_ie,Rest} ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        {handled_ie,Rest} ->
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        {unhandled_ie} ->
            {fault,193,CreateReq}
    end.

decode_ie_update(<<>>,PresentIEs,Errors,UpdateReq) ->
    if
        PresentIEs band 61 /= 61 ->
            {fault,202,UpdateReq};
        true ->
            case Errors of
                #protocolErrors{invalidManIE = true} ->
                    {fault,201,UpdateReq};
                #protocolErrors{outOfSequence = true} ->
                    {fault,193,UpdateReq};
                #protocolErrors{incorrectOptIE = true} ->
                    {fault,203,UpdateReq};
                _ ->
                    {ok,UpdateReq}
            end
    end;
decode_ie_update(<<6:8,
                   QoSElement:3/binary-unit:8,
                   Rest/binary>>,PresentIEs,Errors,UpdateReq) ->
    if
        PresentIEs band 1 == 1 ->
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        PresentIEs > 1 ->
            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
            <<_:2,
              DelayClass:3,
              ReliabilityClass:3,
              PeakThroughput:4,
              _:1,
              PrecedenceClass:3,
              _:3,
              MeanThroughput:5>> = QoSElement,
            QoS = #sesT_qualityOfServiceV0{delayClass = DelayClass,
                                           reliabilityClass = ReliabilityClass,
                                           peakThroughput = PeakThroughput,
                                           precedenceClass = PrecedenceClass,
                                           meanThroughput = MeanThroughput},
            UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{qos = QoS},
            decode_ie_update(Rest,PresentIEs bor 1,UpdatedErrors,UpdatedUpdateReq);
        true ->
            <<_:2,
              DelayClass:3,
              ReliabilityClass:3,
              PeakThroughput:4,
              _:1,
              PrecedenceClass:3,
              _:3,
              MeanThroughput:5>> = QoSElement,
            QoS = #sesT_qualityOfServiceV0{delayClass = DelayClass,
                                           reliabilityClass = ReliabilityClass,
                                           peakThroughput = PeakThroughput,
                                           precedenceClass = PrecedenceClass,
                                           meanThroughput = MeanThroughput},
            UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{qos = QoS},
            decode_ie_update(Rest,PresentIEs bor 1,Errors,UpdatedUpdateReq)
    end;
decode_ie_update(<<14:8,
                   Recovery:8,
                   Rest/binary>>,PresentIEs,Errors,UpdateReq) ->
    if
        PresentIEs band 2 == 2 ->
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        PresentIEs > 2 ->
            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
            UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{recovery = Recovery},
            decode_ie_update(Rest,PresentIEs bor 2,UpdatedErrors,UpdatedUpdateReq);
        true ->
            UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{recovery = Recovery},
            decode_ie_update(Rest,PresentIEs bor 2,Errors,UpdatedUpdateReq)
    end;
decode_ie_update(<<16:8,
                   FlowLabel:16,
                   Rest/binary>>,PresentIEs,Errors,UpdateReq) ->
    if
        PresentIEs band 4 == 4 ->
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        PresentIEs > 4 ->
            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
            UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{flowLblData = FlowLabel},
            decode_ie_update(Rest,PresentIEs bor 4,UpdatedErrors,UpdatedUpdateReq);
        true ->
            UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{flowLblData = FlowLabel},
            decode_ie_update(Rest,PresentIEs bor 4,Errors,UpdatedUpdateReq)
    end;
decode_ie_update(<<17:8,
                   FlowLabel:16,
                   Rest/binary>>,PresentIEs,Errors,UpdateReq) ->
    if
        PresentIEs band 8 == 8 ->
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        PresentIEs > 8 ->
            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
            UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{flowLblSig = FlowLabel},
            decode_ie_update(Rest,PresentIEs bor 8,UpdatedErrors,UpdatedUpdateReq);
        true ->
            UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{flowLblSig = FlowLabel},
            decode_ie_update(Rest,PresentIEs bor 8,Errors,UpdatedUpdateReq)
    end;
decode_ie_update(<<133:8,
                   Length:16,
                   More/binary>>,PresentIEs,Errors,UpdateReq) ->
    <<AddressElement:Length/binary-unit:8,
      Rest/binary>> = More,
    if
        PresentIEs band 48 == 48 ->
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        PresentIEs > 32 ->
            if
                PresentIEs band 16 == 0 ->
                    case gsn_addr_internal_storage(AddressElement) of
                        {ok,GSNAddr} ->
                            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
                            UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{sgsnAddSig = GSNAddr},
                            decode_ie_update(Rest,PresentIEs bor 16,UpdatedErrors,UpdatedUpdateReq);
                        {fault} ->
                            UpdatedErrors = Errors#protocolErrors{invalidManIE = true,
                                                                  outOfSequence = true},
                            decode_ie_update(Rest,PresentIEs bor 16,UpdatedErrors,UpdateReq)
                    end;
                true ->
                    case gsn_addr_internal_storage(AddressElement) of
                        {ok,GSNAddr} ->
                            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
                            UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{sgsnAddUser = GSNAddr},
                            decode_ie_update(Rest,PresentIEs bor 32,UpdatedErrors,UpdatedUpdateReq);
                        {fault} ->
                            UpdatedErrors = Errors#protocolErrors{invalidManIE = true,
                                                                  outOfSequence = true},
                            decode_ie_update(Rest,PresentIEs bor 32,UpdatedErrors,UpdateReq)
                    end
            end;
        PresentIEs < 16 ->
            case gsn_addr_internal_storage(AddressElement) of
                {ok,GSNAddr} ->
                    UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{sgsnAddSig = GSNAddr},
                    decode_ie_update(Rest,PresentIEs bor 16,Errors,UpdatedUpdateReq);
                {fault} ->
                    UpdatedErrors = Errors#protocolErrors{invalidManIE = true},
                    decode_ie_update(Rest,PresentIEs bor 16,UpdatedErrors,UpdateReq)
            end;
        true ->
            case gsn_addr_internal_storage(AddressElement) of
                {ok,GSNAddr} ->
                    UpdatedUpdateReq = UpdateReq#sesT_updateReqV0{sgsnAddUser = GSNAddr},
                    decode_ie_update(Rest,PresentIEs bor 32,Errors,UpdatedUpdateReq);
                {fault} ->
                    UpdatedErrors = Errors#protocolErrors{invalidManIE = true},
                    decode_ie_update(Rest,PresentIEs bor 32,UpdatedErrors,UpdateReq)
            end
    end;
decode_ie_update(UnexpectedIE,PresentIEs,Errors,UpdateReq) ->
    case check_ie(UnexpectedIE) of
        {defined_ie,Rest} ->
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        {handled_ie,Rest} ->
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        {unhandled_ie} ->
            {fault,193,UpdateReq}
    end.

decode_ie_delete_res(<<>>,PresentIEs,Errors,DeleteRes) ->
    if
        PresentIEs band 1 /= 1 ->
            {fault,202,DeleteRes};
        true ->
            case Errors of
                #protocolErrors{invalidManIE = true} ->
                    {fault,201,DeleteRes};
                #protocolErrors{outOfSequence = true} ->
                    {fault,193,DeleteRes};
                #protocolErrors{incorrectOptIE = true} ->
                    {fault,203,DeleteRes};
                _ ->
                    {ok,DeleteRes}
            end
    end;
decode_ie_delete_res(<<1:8,
                       Cause:8,
                       Rest/binary>>,PresentIEs,Errors,DeleteRes) ->
    if
        PresentIEs band 1 == 1 ->
            decode_ie_delete_res(Rest,PresentIEs,Errors,DeleteRes);
        PresentIEs > 1 ->
            UpdatedErrors = Errors#protocolErrors{outOfSequence = true},
            UpdatedDeleteRes = DeleteRes#sesT_deleteResV0{cause = Cause},
            decode_ie_delete_res(Rest,PresentIEs bor 1,UpdatedErrors,UpdatedDeleteRes);
        true ->
            UpdatedDeleteRes = DeleteRes#sesT_deleteResV0{cause = Cause},
            decode_ie_delete_res(Rest,PresentIEs bor 1,Errors,UpdatedDeleteRes)
    end;
decode_ie_delete_res(UnexpectedIE,PresentIEs,Errors,DeleteRes) ->
    case check_ie(UnexpectedIE) of
        {defined_ie,Rest} ->
            decode_ie_delete_res(Rest,PresentIEs,Errors,DeleteRes);
        {handled_ie,Rest} ->
            decode_ie_delete_res(Rest,PresentIEs,Errors,DeleteRes);
        {unhandled_ie} ->
            {fault,193,DeleteRes}
    end.

check_ie(<<1:8,
           _:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<2:8,
           _:8/binary-unit:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<3:8,
           _:6/binary-unit:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<4:8,
           _:4/binary-unit:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<5:8,
           _:4/binary-unit:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<6:8,
           _:3/binary-unit:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<8:8,
           _:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<9:8,
           _:28/binary-unit:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<11:8,
           _:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<12:8,
           _:3/binary-unit:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<13:8,
           _:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<14:8,
           _:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<15:8,
           _:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<16:8,
           _:16,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<17:8,
           _:16,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<18:8,
           _:32,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<19:8,
           _:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<127:8,
           _:4/binary-unit:8,
           Rest/binary>>) ->
    {defined_ie,Rest};
check_ie(<<1:1,
           _:7,
           Length:16,
           More/binary>>) ->
    if
        Length > size(More) ->
            {unhandled_ie};
        true ->
            <<_:Length/binary-unit:8,
              Rest/binary>> = More,
            {handled_ie,Rest}
    end;
check_ie(_UnhandledIE) ->
    {unhandled_ie}.

tid_internal_storage(Bin,_) ->
    Size = size(Bin) - 1,
    <<Front:Size/binary,
      NSAPI:4,
      DigitN:4>> = Bin,
    Result = case DigitN of
                 15 ->
                     #mvsgT_tid{imsi = #mvsgT_imsi{value = Front},
                                nsapi = NSAPI};
                 _ ->
                     #mvsgT_tid{imsi = #mvsgT_imsi{value = <<Front/binary,
                                                             15:4,
                                                             DigitN:4>>},
                                nsapi = NSAPI}
             end,
    {ok,Result}.

selection_mode_internal_storage(0) ->
    subscribed;
selection_mode_internal_storage(1) ->
    msRequested;
selection_mode_internal_storage(2) ->
    sgsnSelected;
selection_mode_internal_storage(3) ->
    sgsnSelected.

pdp_addr_internal_storage(<<_:4,
                            0:4,
                            1:8>>) ->
    {ok,#mvsgT_pdpAddressType{pdpTypeNbr = etsi_ppp,
                           address = []}};
pdp_addr_internal_storage(<<_:4,
                            0:4,
                            2:8>>) ->
    {ok,#mvsgT_pdpAddressType{pdpTypeNbr = etsi_osp_ihoss,
                           address = []}};
pdp_addr_internal_storage(<<_:4,
                            1:4,
                            33:8>>) ->
    {ok,#mvsgT_pdpAddressType{pdpTypeNbr = ietf_ipv4,
                           address = []}};
pdp_addr_internal_storage(<<_:4,
                            1:4,
                            33:8,
                            IP_A:8,
                            IP_B:8,
                            IP_C:8,
                            IP_D:8>>) ->
    {ok,#mvsgT_pdpAddressType{pdpTypeNbr = ietf_ipv4,
                           address = [IP_A,IP_B,IP_C,IP_D]}};
pdp_addr_internal_storage(<<_:4,
                            1:4,
                            87:8,
                            IP_A:16,
                            IP_B:16,
                            IP_C:16,
                            IP_D:16,
                            IP_E:16,
                            IP_F:16,
                            IP_G:16,
                            IP_H:16>>) ->
    {ok,#mvsgT_pdpAddressType{pdpTypeNbr = ietf_ipv6,
                           address = [IP_A,IP_B,IP_C,IP_D,IP_E,IP_F,IP_G,IP_H]}};
pdp_addr_internal_storage(_PDP_ADDR) ->
    {fault}.

apn_internal_storage(<<>>,APN) ->
    {ok,lists:reverse(APN)};
apn_internal_storage(<<Length:8,
                       Rest/binary>>,APN) ->
    <<Label:Length/binary-unit:8,
      MoreAPNLabels/binary>> = Rest,
    apn_internal_storage(MoreAPNLabels,[Label|APN]).

pco_internal_storage(<<1:1,
                       _:4,
                       0:3,
                       PPPConfigurationOptions/binary>>) ->
    case ppp_configuration_options(PPPConfigurationOptions,#masT_pap{exists = false},[],[]) of
        {ok,PAP,CHAP,IPCP} ->
            {ok,#masT_protocolConfigOptions{pap = PAP,
                                         chap = CHAP,
                                         ipcp = IPCP}};
        {fault} ->
            {fault}
    end;
pco_internal_storage(<<1:1,
                       _:4,
                       1:3,
                       _OSP_IHOSSConfigurationOptions/binary>>) ->
    {ok,osp_ihoss};
pco_internal_storage(_UnknownConfigurationOptions) ->
    {fault}.

ppp_configuration_options(<<>>,PAP,CHAP,IPCP) ->
    {ok,PAP,CHAP,IPCP};
ppp_configuration_options(<<49185:16,
                            Length:8,
                            More/binary>>,PAP,CHAP,IPCP) ->
    <<_LCP:Length/binary-unit:8,
      Rest/binary>> = More,
    ppp_configuration_options(Rest,PAP,CHAP,IPCP);
ppp_configuration_options(<<49187:16,
                            _Length:8,
                            1:8,
                            Identifier:8,
                            DataLength:16,
                            More/binary>>,_PAP,CHAP,IPCP) ->
    ActualDataLength = DataLength - 4,
    <<Data:ActualDataLength/binary-unit:8,
      Rest/binary>> = More,
    <<PeerIDLength:8,
      PeerData/binary>> = Data,
    <<PeerID:PeerIDLength/binary-unit:8,
      PasswdLength:8,
      PasswordData/binary>> = PeerData,
    <<Password:PasswdLength/binary,
      _Padding/binary>> = PasswordData,
    ppp_configuration_options(Rest,#masT_pap{exists = true,
                                        code = 1,
                                        id = Identifier,
                                        username = binary_to_list(PeerID),
                                        password = binary_to_list(Password)},CHAP,IPCP);
ppp_configuration_options(<<49187:16,
                            Length:8,
                            More/binary>>,PAP,CHAP,IPCP) ->
    <<PAP:Length/binary-unit:8,
      Rest/binary>> = More,
    ppp_configuration_options(Rest,PAP,CHAP,IPCP);
ppp_configuration_options(<<49699:16,
                            _Length:8,
                            1:8,
                            Identifier:8,
                            DataLength:16,
                            More/binary>>,PAP,CHAP,IPCP) ->
    ActualDataLength = DataLength - 4,
    <<Data:ActualDataLength/binary-unit:8,
      Rest/binary>> = More,
    <<ValueSize:8,
      ValueAndName/binary>> = Data,
    <<Value:ValueSize/binary-unit:8,
      Name/binary>> = ValueAndName,
    ppp_configuration_options(Rest,PAP,[#masT_chap{code = 1,
                                          id = Identifier,
                                          value = binary_to_list(Value),
                                          name = binary_to_list(Name)}|CHAP],IPCP);
ppp_configuration_options(<<49699:16,
                            _Length:8,
                            2:8,
                            Identifier:8,
                            DataLength:16,
                            More/binary>>,PAP,CHAP,IPCP) ->
    ActualDataLength = DataLength - 4,
    <<Data:ActualDataLength/binary-unit:8,
      Rest/binary>> = More,
    <<ValueSize:8,
      ValueAndName/binary>> = Data,
    <<Value:ValueSize/binary-unit:8,
      Name/binary>> = ValueAndName,
    ppp_configuration_options(Rest,PAP,[#masT_chap{code = 2,
                                          id = Identifier,
                                          value = binary_to_list(Value),
                                          name = binary_to_list(Name)}|CHAP],IPCP);
ppp_configuration_options(<<49699:16,
                            Length:8,
                            More/binary>>,PAP,CHAP,IPCP) ->
    <<CHAP:Length/binary-unit:8,
      Rest/binary>> = More,
    ppp_configuration_options(Rest,PAP,CHAP,IPCP);
ppp_configuration_options(<<32801:16,
                            _Length:8,
                            1:8,
                            Identifier:8,
                            OptionsLength:16,
                            More/binary>>,PAP,CHAP,IPCP) ->
    ActualOptionsLength = OptionsLength - 4,
    <<Options:ActualOptionsLength/binary-unit:8,
      Rest/binary>> = More,
    case Options of
        <<3:8,
          6:8,
          A1:8,
          A2:8,
          A3:8,
          A4:8>> ->
            ppp_configuration_options(Rest,PAP,CHAP,[#masT_ipcp{exists = true,
                                                  code = 1,
                                                  id = Identifier,
                                                  ipcpList = [#masT_ipcpData{type = 3,
                                                                             ipAddress = #mvsgT_ipAddress{version = ipv4,
                                                                                                          a1 = A1,
                                                                                                          a2 = A2,
                                                                                                          a3 = A3,
                                                                                                          a4 = A4,
                                                                                                          a5 = 0,
                                                                                                          a6 = 0,
                                                                                                          a7 = 0,
                                                                                                          a8 = 0},
                                                                             rawMessage = binary_to_list(Options)}]}|IPCP]);
        <<129:8,
          6:8,
          B1:8,
          B2:8,
          B3:8,
          B4:8>> ->
            ppp_configuration_options(Rest,PAP,CHAP,[#masT_ipcp{exists = true,
                                                  code = 1,
                                                  id = Identifier,
                                                  ipcpList = [#masT_ipcpData{type = 129,
                                                                             ipAddress = #mvsgT_ipAddress{version = ipv4,
                                                                                                          a1 = B1,
                                                                                                          a2 = B2,
                                                                                                          a3 = B3,
                                                                                                          a4 = B4},
                                                                             rawMessage = binary_to_list(Options)}]}|IPCP]);
        <<131:8,
          6:8,
          C1:8,
          C2:8,
          C3:8,
          C4:8>> ->
            ppp_configuration_options(Rest,PAP,CHAP,[#masT_ipcp{exists = true,
                                                  code = 1,
                                                  id = Identifier,
                                                  ipcpList = [#masT_ipcpData{type = 131,
                                                                             ipAddress = #mvsgT_ipAddress{version = ipv4,
                                                                                                          a1 = C1,
                                                                                                          a2 = C2,
                                                                                                          a3 = C3,
                                                                                                          a4 = C4},
                                                                             rawMessage = binary_to_list(Options)}]}|IPCP]);
        _ ->
            ppp_configuration_options(Rest,PAP,CHAP,IPCP)
    end;
ppp_configuration_options(<<_UnknownProtocolID:16,
                            Length:8,
                            More/binary>>,PAP,CHAP,IPCP) ->
    <<_Skipped:Length/binary-unit:8,
      Rest/binary>> = More,
    ppp_configuration_options(Rest,PAP,CHAP,IPCP);
ppp_configuration_options(_Unhandled,_PAP,_CHAP,_IPCP) ->
    {fault}.

gsn_addr_internal_storage(<<IP_A:8,
                            IP_B:8,
                            IP_C:8,
                            IP_D:8>>) ->
    {ok,#mvsgT_ipAddress{version = ipv4,
                      a1 = IP_A,
                      a2 = IP_B,
                      a3 = IP_C,
                      a4 = IP_D,
                      a5 = 0,
                      a6 = 0,
                      a7 = 0,
                      a8 = 0}};
gsn_addr_internal_storage(<<IP_A:16,
                            IP_B:16,
                            IP_C:16,
                            IP_D:16,
                            IP_E:16,
                            IP_F:16,
                            IP_G:16,
                            IP_H:16>>) ->
    {ok,#mvsgT_ipAddress{version = ipv6,
                      a1 = IP_A,
                      a2 = IP_B,
                      a3 = IP_C,
                      a4 = IP_D,
                      a5 = IP_E,
                      a6 = IP_F,
                      a7 = IP_G,
                      a8 = IP_H}};
gsn_addr_internal_storage(_GSN_ADDR) ->
    {fault}.

msisdn_internal_storage(<<>>,MSISDN) ->
    {ok,#mvsT_msisdn{value = lists:reverse(MSISDN)}};
msisdn_internal_storage(<<255:8,
                          _Rest/binary>>,MSISDN) ->
    {ok,#mvsT_msisdn{value = lists:reverse(MSISDN)}};
msisdn_internal_storage(<<15:4,
                          DigitN:4,
                          _Rest/binary>>,MSISDN) when DigitN < 10 ->
    {ok,#mvsT_msisdn{value = lists:reverse([DigitN bor 240|MSISDN])}};
msisdn_internal_storage(<<DigitNplus1:4,
                          DigitN:4,
                          Rest/binary>>,MSISDN) when DigitNplus1 < 10, DigitN < 10 ->
    NewMSISDN = [DigitNplus1 bsl 4 bor DigitN|MSISDN],
    msisdn_internal_storage(Rest,NewMSISDN);
msisdn_internal_storage(_Rest,_MSISDN) ->
    {fault}.
