%% -*- erlang-indent-level: 2 -*-

-module(bs_decode).

-export([test/0]).

-include("bs_decode_extract.hrl").

-define(PDU, <<30,16,0,90,0,1,0,0,255,255,255,255,81,67,101,7,0,0,0,96,
	       6,12,146,18,14,0,15,252,16,0,0,17,0,0,128,0,2,241,33,131,
	       0,20,7,97,112,110,48,49,51,97,8,101,114,105,99,115,115,
	       111,110,2,115,101,132,0,20,128,192,35,16,1,5,0,16,5,117,
	       115,101,114,53,5,112,97,115,115,53,133,0,4,172,28,12,1,
	       133,0,4,172,28,12,3,134,0,8,145,148,113,129,0,0,0,0>>).

-define(RES, {ok,{sesT_createReqV0,
		  {mvsgT_tid,{mvsgT_imsi,<<81,67,101,7,0,0,0,240>>},6},
		  [81,67,101,7,0,0,0,96],
		  {sesT_qualityOfServiceV0,1,4,9,2,18},
		  0,subscribed,0,0,
		  {mvsgT_pdpAddressType,ietf_ipv4,[]},
		  [<<"apn013a">>,<<"ericsson">>,<<"se">>],
		  {masT_protocolConfigOptions,[],
		   {masT_pap,true,1,5,"user5","pass5"},
		   []},
		  {mvsgT_ipAddress,ipv4,172,28,12,1,0,0,0,0},
		  {mvsgT_ipAddress,ipv4,172,28,12,3,0,0,0,0},
		  {mvsT_msisdn,<<145,148,113,129,0,0,0,0>>}},
	      1}).

test() ->
    ?RES = decode_v0_opt(42, ?PDU),
    ok.

decode_v0_opt(0, Pdu) ->
    decode_gtpc_msg(Pdu);
decode_v0_opt(N, Pdu) ->
    decode_gtpc_msg(Pdu),
    decode_v0_opt(N-1, Pdu).

%%% --------------------------------------------------------------
%%% #3.1.2 DECODE GTP-C MESSAGE
%%% --------------------------------------------------------------

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% Function   : decode_gtpc_msg(GTP_C_Message)->
%%%              {ok,Request,ControlDataUs} |
%%%              {fault,Cause,Request,ControlDataUs}
%%%
%%% Types      : GTP_C_Message = binary(), GTP-C message from SGSN
%%%              Request = record(), Containing decoded request
%%%              ControlDataUS = record(), Containing header info
%%%              Cause = integer(), Error code
%%%
%%% Description: This function decodes a binary GTP-C message and
%%%              stores it in a record. Different records are used
%%%              for different message types.
%%%
%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%%% Create PDP Context Request
%%% GTP97, SNN=0
%%% (No SNDCP N-PDU number)
decode_gtpc_msg(<<0:3,_:4,0:1,16:8,_Length:16,SequenceNumber:16,
		  _FlowLabel:16,_SNDCP_N_PDU_Number:8,_:3/binary-unit:8,
		  TID:8/binary-unit:8,InformationElements/binary>>) ->
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

%%% Update PDP Context Request
%%% GTP97, SNN=0
%%% (No SNDCP N-PDU number)
decode_gtpc_msg(<<0:3,_:4,0:1,18:8,_Length:16,SequenceNumber:16,
		  _FlowLabel:16,_SNDCP_N_PDU_Number:8,_:3/binary-unit:8,
		  TID:8/binary-unit:8,InformationElements/binary>>) ->
    io:format("hej", []),
    Errors = #protocolErrors{},
    {ok,TID2}=tid_internal_storage(TID,[]),
    EmptyUpdateReq=#sesT_updateReqV0{tid=TID2,
				     tidRaw=binary_to_list(TID)},
    case catch decode_ie_update(InformationElements,0,Errors,
				EmptyUpdateReq) of
	{ok,UpdateReq} ->
	    {ok,UpdateReq,SequenceNumber};
	{fault,Cause,UpdateReq} ->
	    {fault,Cause,UpdateReq,SequenceNumber};
	{'EXIT',Reason} ->
	    io:format("hej", []),
	    {fault,193,EmptyUpdateReq,SequenceNumber, Reason}
    end;

%%% Delete PDP Context Request
%%% GTP97, SNN=0
%%% (No SNDCP N-PDU number)
decode_gtpc_msg(<<0:3,_:4,0:1,20:8,_Length:16,SequenceNumber:16,
		_FlowLabel:16,_SNDCP_N_PDU_Number:8,_:3/binary-unit:8,
		TID:8/binary-unit:8,_InformationElements/binary>>) ->
    {ok,TID2} = tid_internal_storage(TID,[]),
    DeleteReq = #sesT_deleteReqV0{tid=TID2},
    {ok,DeleteReq,SequenceNumber};

%%% Delete PDP Context Response
%%% GTP97, SNN=0
%%% (No SNDCP N-PDU number)
decode_gtpc_msg(<<0:3,_:4,0:1,21:8,_Length:16,SequenceNumber:16,
		_FlowLabel:16,_SNDCP_N_PDU_Number:8,_:3/binary-unit:8,
		TID:8/binary-unit:8,InformationElements/binary>>) ->
    {ok,TID2} = tid_internal_storage(TID,[]),
    EmptyDeleteRes = #sesT_deleteResV0{tid=TID2},
    case catch decode_ie_delete_res(InformationElements,0,EmptyDeleteRes) of
	{ok, DeleteRes} ->
	    {ok,DeleteRes,SequenceNumber};
	{fault,Cause,DeleteRes} ->
	    {fault,Cause,DeleteRes,SequenceNumber};
	{'EXIT',_Reason} ->
	    {fault,193,EmptyDeleteRes,SequenceNumber}
    end;

%%% Error handling
decode_gtpc_msg(_GTP_C_Message) ->
    {fault}.

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% decode_ie_create/4
%%% Decode information elements for Create PDP Context Request

%%% All elements decoded
decode_ie_create(<<>>,PresentIEs,Errors,CreateReq) ->
    %% Check mandatory IE's
    if
	(PresentIEs band 16#77D) =/= 16#77D ->
	    {fault,202,CreateReq}; %Mandatory IE missing
	true -> %OK
	    %% Check errors during decoding
	    case Errors of
		#protocolErrors{invalidManIE=true} -> %Invalid mandatory IE
		    {fault,201,CreateReq}; %Mandatory IE incorrect
		#protocolErrors{outOfSequence=true} -> %Out of sequence
		    {fault,193,CreateReq}; %Invalid message format
		#protocolErrors{incorrectOptIE=true} -> %Incorrect optional IE
		    {fault,203,CreateReq}; %Optional IE incorrect
		_ -> %OK
		    {ok,CreateReq}
	    end
    end;

%%% Quality of Service Profile, Mandatory
decode_ie_create(<<6:8,QoSElement:3/binary-unit:8,Rest/binary>>,PresentIEs,
		 Errors,CreateReq) ->
    if
        (PresentIEs band 16#00000001) =:= 16#00000001 -> %Repeated IE's, ignore
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 16#00000001 -> %Out of sequence
            UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
            <<_:2,DelayClass:3,ReliabilityClass:3,
	      PeakThroughput:4,_:1,PrecedenceClass:3,
	      _:3,MeanThroughput:5>> = QoSElement,
            QoS=#sesT_qualityOfServiceV0{delayClass=DelayClass,
					 reliabilityClass=ReliabilityClass,
					 peakThroughput=PeakThroughput,
					 precedenceClass=PrecedenceClass,
					 meanThroughput=MeanThroughput},
            UpdatedCreateReq=CreateReq#sesT_createReqV0{qos=QoS},
            decode_ie_create(Rest,(PresentIEs bor 16#00000001),
			     UpdatedErrors,UpdatedCreateReq);
        true -> %OK
            <<_:2,DelayClass:3,ReliabilityClass:3,
	      PeakThroughput:4,_:1,PrecedenceClass:3,
	      _:3,MeanThroughput:5>> = QoSElement,
            QoS=#sesT_qualityOfServiceV0{delayClass=DelayClass,
					 reliabilityClass=ReliabilityClass,
					 peakThroughput=PeakThroughput,
					 precedenceClass=PrecedenceClass,
					 meanThroughput=MeanThroughput},
            UpdatedCreateReq=CreateReq#sesT_createReqV0{qos=QoS},
            decode_ie_create(Rest,(PresentIEs bor 16#00000001),
			     Errors,UpdatedCreateReq)
    end;

%%% Recovery, Optional
decode_ie_create(<<14:8,Recovery:8,Rest/binary>>,
		 PresentIEs,Errors,CreateReq) ->
    if
        (PresentIEs band 16#00000002) =:= 16#00000002 -> %Repeated IE, ignored
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 16#00000002 -> %Out of sequence
            UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
            UpdatedCreateReq=CreateReq#sesT_createReqV0{recovery=Recovery},
            decode_ie_create(Rest,(PresentIEs bor 16#00000002),
                UpdatedErrors,UpdatedCreateReq);
        true -> %OK
            UpdatedCreateReq=CreateReq#sesT_createReqV0{recovery=Recovery},
            decode_ie_create(Rest,(PresentIEs bor 16#00000002),Errors,
			     UpdatedCreateReq)
    end;

%%% Selection mode, Mandatory
decode_ie_create(<<15:8,_:6,SelectionMode:2,Rest/binary>>,PresentIEs,
		 Errors,CreateReq) ->
    if
        (PresentIEs band 16#00000004) =:= 16#00000004 -> %Repeated IE, ignored
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 16#00000004 -> %Out of sequence
            UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
            UpdatedCreateReq=CreateReq#sesT_createReqV0{
			       selMode=selection_mode_internal_storage(SelectionMode)},
            decode_ie_create(Rest,(PresentIEs bor 16#00000004),
			     UpdatedErrors,UpdatedCreateReq);
        true -> %OK
            UpdatedCreateReq=CreateReq#sesT_createReqV0{
			       selMode=selection_mode_internal_storage(SelectionMode)},
            decode_ie_create(Rest,(PresentIEs bor 16#00000004),Errors,
                UpdatedCreateReq)
    end;

%%% Flow Label Data I, Mandatory
decode_ie_create(<<16:8,FlowLabel:16,Rest/binary>>,PresentIEs,Errors,CreateReq) ->
    if
        (PresentIEs band 16#00000008) =:= 16#00000008 -> %Repeated IE, ignored
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 16#00000008 -> %Out of sequence
            UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
            UpdatedCreateReq=CreateReq#sesT_createReqV0{flowLblData=FlowLabel},
            decode_ie_create(Rest,(PresentIEs bor 16#00000008),
			     UpdatedErrors,UpdatedCreateReq);
        true -> %OK
            UpdatedCreateReq=CreateReq#sesT_createReqV0{flowLblData=FlowLabel},
            decode_ie_create(Rest,(PresentIEs bor 16#00000008),Errors,
			     UpdatedCreateReq)
    end;

%%% Flow Label Signalling, Mandatory
decode_ie_create(<<17:8,FlowLabel:16,Rest/binary>>,PresentIEs,Errors,CreateReq) ->
    if
        (PresentIEs band 16#00000010) =:= 16#00000010 -> %Repeated IE, ignored
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 16#00000010 -> %Out of sequence
            UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
            UpdatedCreateReq=CreateReq#sesT_createReqV0{flowLblSig=FlowLabel},
            decode_ie_create(Rest,(PresentIEs bor 16#00000010),
			     UpdatedErrors,UpdatedCreateReq);
        true -> %OK
            UpdatedCreateReq=CreateReq#sesT_createReqV0{flowLblSig=FlowLabel},
            decode_ie_create(Rest,(PresentIEs bor 16#00000010),Errors,
			     UpdatedCreateReq)
    end;

%%% End User Address, Mandatory
decode_ie_create(<<128:8,Length:16,More/binary>>,PresentIEs,
		 Errors,CreateReq) ->
    <<PDPElement:Length/binary-unit:8,Rest/binary>> = More,
    if
        (PresentIEs band 16#00000020) =:= 16#00000020 -> %Repeated IE, ignore
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 16#00000020 -> %Out of sequence
            case pdp_addr_internal_storage(PDPElement) of
		{ok,PDPAddress} ->
		    UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
		    UpdatedCreateReq=CreateReq#sesT_createReqV0{endUserAdd=PDPAddress},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000020),
				     UpdatedErrors,UpdatedCreateReq);
		{fault} ->
		    UpdatedErrors=Errors#protocolErrors{invalidManIE=true,
							outOfSequence=true},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000020),
				     UpdatedErrors,CreateReq)
	    end;
        true -> %OK
	    case pdp_addr_internal_storage(PDPElement) of
		{ok,PDPAddress} ->
		    UpdatedCreateReq=CreateReq#sesT_createReqV0{endUserAdd=PDPAddress},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000020),
				     Errors,UpdatedCreateReq);
		{fault} ->
		    UpdatedErrors=Errors#protocolErrors{invalidManIE=true},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000020),
				     UpdatedErrors,CreateReq)
	    end
    end;

%%% Access Point Name, Mandatory
decode_ie_create(<<131:8,Length:16,More/binary>>,PresentIEs,
		 Errors,CreateReq) ->
    <<APNElement:Length/binary-unit:8,Rest/binary>> = More,
    if
        (PresentIEs band 16#00000040) =:= 16#00000040 -> %Repeated IE, ignore
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 16#00000040 -> %Out of sequence
            case catch apn_internal_storage(APNElement,[]) of
		{ok,APN} ->
		    UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
		    UpdatedCreateReq=CreateReq#sesT_createReqV0{accPointName=APN},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000040),
				     UpdatedErrors,UpdatedCreateReq);
		_ ->
		    UpdatedErrors=Errors#protocolErrors{outOfSequence=true,
							invalidManIE=true},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000040),
				     UpdatedErrors,CreateReq)
	    end;
        true -> %OK
            case catch apn_internal_storage(APNElement,[]) of
		{ok,APN} ->
		    UpdatedCreateReq=CreateReq#sesT_createReqV0{accPointName=APN},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000040),
				     Errors,UpdatedCreateReq);
		_ ->
		    UpdatedErrors=Errors#protocolErrors{invalidManIE=true},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000040),
				     UpdatedErrors,CreateReq)
	    end
    end;

%%% Protocol Configuration Options, Optional
decode_ie_create(<<132:8,Length:16,More/binary>>,PresentIEs,Errors,CreateReq) ->
    <<ConfigurationElement:Length/binary-unit:8,Rest/binary>> = More,
    if
        (PresentIEs band 16#00000080) =:= 16#00000080 -> %Repeated IE, ignore
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 16#00000080 -> %Out of sequence
            case catch pco_internal_storage(ConfigurationElement) of
		{ok,PCO} ->
		    UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
		    UpdatedCreateReq=CreateReq#sesT_createReqV0{protConOpt=PCO},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000080),
				     UpdatedErrors,UpdatedCreateReq);
		_ ->
		    UpdatedErrors=Errors#protocolErrors{outOfSequence=true,
							incorrectOptIE=true},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000080),
				     UpdatedErrors,CreateReq)
	    end;
        true -> %OK
            case catch pco_internal_storage(ConfigurationElement) of
		{ok,PCO} ->
		    UpdatedCreateReq=CreateReq#sesT_createReqV0{protConOpt=PCO},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000080),
				     Errors,UpdatedCreateReq);
		_ ->
		    UpdatedErrors=Errors#protocolErrors{incorrectOptIE=true},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000080),
				     UpdatedErrors,CreateReq)
	    end
    end;

%%% SGSN Address for signalling, Mandatory OR SGSN Address for user traffic, Mandatory
decode_ie_create(<<133:8,Length:16,More/binary>>,PresentIEs,
		 Errors,CreateReq) ->
    <<AddressElement:Length/binary-unit:8,Rest/binary>> = More,
    if
        (PresentIEs band 16#00000300) =:= 16#00000300 -> %Repeated IE, ignore
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 16#00000200 -> %Out of sequence
            if
                (PresentIEs band 16#00000100) =:= 16#00000000 -> %Signalling
                    case gsn_addr_internal_storage(AddressElement) of
			{ok,GSNAddr} ->
			    UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
			    UpdatedCreateReq=CreateReq#sesT_createReqV0{sgsnAddSig=GSNAddr},
			    decode_ie_create(Rest,(PresentIEs bor 16#00000100),
					     UpdatedErrors,UpdatedCreateReq);
			{fault} ->
			    UpdatedErrors=Errors#protocolErrors{invalidManIE=true,
								outOfSequence=true},
			    decode_ie_create(Rest,(PresentIEs bor 16#00000100),
					     UpdatedErrors,CreateReq)
		    end;
                true -> % User traffic
                    case gsn_addr_internal_storage(AddressElement) of
			{ok,GSNAddr} ->
			    UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
			    UpdatedCreateReq=CreateReq#sesT_createReqV0{sgsnAddUser=GSNAddr},
			    decode_ie_create(Rest,(PresentIEs bor 16#00000200),
					     UpdatedErrors,UpdatedCreateReq);
			{fault} ->
			    UpdatedErrors=Errors#protocolErrors{invalidManIE=true,
								outOfSequence=true},
			    decode_ie_create(Rest,(PresentIEs bor 16#00000200),
					     UpdatedErrors,CreateReq)
		    end
            end;
        PresentIEs < 16#00000100 -> %OK, SGSN Address for signalling
            case gsn_addr_internal_storage(AddressElement) of
		{ok,GSNAddr} ->
		    UpdatedCreateReq=CreateReq#sesT_createReqV0{sgsnAddSig=GSNAddr},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000100),
				     Errors,UpdatedCreateReq);
		{fault} ->
		    UpdatedErrors=Errors#protocolErrors{invalidManIE=true},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000100),
				     UpdatedErrors,CreateReq)
	    end;
        true -> %OK, SGSN Address for user traffic
            case gsn_addr_internal_storage(AddressElement) of
		{ok,GSNAddr} ->
		    UpdatedCreateReq=CreateReq#sesT_createReqV0{sgsnAddUser=GSNAddr},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000200),
				     Errors,UpdatedCreateReq);
		{fault} ->
		    UpdatedErrors=Errors#protocolErrors{invalidManIE=true},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000200),
				     UpdatedErrors,CreateReq)
	    end
    end;

%%% MSISDN, Mandatory
decode_ie_create(<<134:8,Length:16,More/binary>>,PresentIEs,
    Errors,CreateReq) ->
    <<MSISDNElement:Length/binary-unit:8,Rest/binary>> = More,
    if
        (PresentIEs band 16#00000400) =:= 16#00000400 -> %Repeated IE, ignore
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        PresentIEs > 16#00000400 -> %Out of sequence
            case msisdn_internal_storage(MSISDNElement,[]) of
		{ok,MSISDN} ->
		    UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
		    UpdatedCreateReq=CreateReq#sesT_createReqV0{msisdn=MSISDN},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000400),
				     UpdatedErrors,UpdatedCreateReq);
		{fault} ->
		    UpdatedErrors=Errors#protocolErrors{outOfSequence=true,invalidManIE=true},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000400),
				     UpdatedErrors,CreateReq)
	    end;
        true -> %OK
		    UpdatedCreateReq=CreateReq#sesT_createReqV0{msisdn=#mvsT_msisdn{value=MSISDNElement}},
		    decode_ie_create(Rest,(PresentIEs bor 16#00000400),
				     Errors,UpdatedCreateReq)

    end;

%%% Private Extension, Optional
%%% Not implemented

%%% Error handling, Unexpected or unknown IE
decode_ie_create(UnexpectedIE,PresentIEs,Errors,CreateReq) ->
    case check_ie(UnexpectedIE) of
        {defined_ie,Rest} -> %OK, ignored
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        {handled_ie,Rest} -> %OK, ignored
            decode_ie_create(Rest,PresentIEs,Errors,CreateReq);
        {unhandled_ie} -> %Error, abort decoding
            {fault,193,CreateReq} %Invalid message format
    end.

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% decode_ie_update/4
%%% Decode information elements for Update PDP Context Request

%%% All elements decoded
decode_ie_update(<<>>,PresentIEs,Errors,UpdateReq) ->
    %% Check mandatory IE's
    if
	(PresentIEs band 16#3D) =/= 16#3D ->
	    {fault,202,UpdateReq}; %Mandatory IE missing
	true -> %OK
	    %% Check errors during decoding
	    case Errors of
		#protocolErrors{invalidManIE=true} -> %Invalid mandatory IE
		    {fault,201,UpdateReq}; %Mandatory IE incorrect
		#protocolErrors{outOfSequence=true} -> %Out of sequence
		    {fault,193,UpdateReq}; %Invalid message format
		_ -> %OK
		    {ok,UpdateReq}
	    end
    end;

%%% Quality of Service Profile, Mandatory
decode_ie_update(<<6:8,QoSElement:3/binary-unit:8,Rest/binary>>,PresentIEs,
		 Errors,UpdateReq) ->
    if
        (PresentIEs band 16#00000001) =:= 16#00000001 -> %Repeated IE's, ignore
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        PresentIEs > 16#00000001 -> %Out of sequence
            UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
            <<_:2,DelayClass:3,ReliabilityClass:3,
	    PeakThroughput:4,_:1,PrecedenceClass:3,
	    _:3,MeanThroughput:5>> = QoSElement,
            QoS=#sesT_qualityOfServiceV0{delayClass=DelayClass,
					 reliabilityClass=ReliabilityClass,
					 peakThroughput=PeakThroughput,
					 precedenceClass=PrecedenceClass,
					 meanThroughput=MeanThroughput},
            UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{qos=QoS},
            decode_ie_update(Rest,(PresentIEs bor 16#00000001),
			     UpdatedErrors,UpdatedUpdateReq);
        true -> %OK
            <<_:2,DelayClass:3,ReliabilityClass:3,
	    PeakThroughput:4,_:1,PrecedenceClass:3,
	    _:3,MeanThroughput:5>> = QoSElement,
            QoS=#sesT_qualityOfServiceV0{delayClass=DelayClass,
					 reliabilityClass=ReliabilityClass,
					 peakThroughput=PeakThroughput,
					 precedenceClass=PrecedenceClass,
					 meanThroughput=MeanThroughput},
            UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{qos=QoS},
            decode_ie_update(Rest,(PresentIEs bor 16#00000001),
			     Errors,UpdatedUpdateReq)
    end;

%%% Recovery, Optional
decode_ie_update(<<14:8,Recovery:8,Rest/binary>>,PresentIEs,Errors,UpdateReq) ->
    if
        (PresentIEs band 16#00000002) =:= 16#00000002 -> %Repeated IE, ignored
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        PresentIEs > 16#00000002 -> %Out of sequence
            UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
            UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{recovery=Recovery},
            decode_ie_update(Rest,(PresentIEs bor 16#00000002),
                UpdatedErrors,UpdatedUpdateReq);
        true -> %OK
            UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{recovery=Recovery},
            decode_ie_update(Rest,(PresentIEs bor 16#00000002),Errors,
                UpdatedUpdateReq)
    end;

%%% Flow Label Data I, Mandatory
decode_ie_update(<<16:8,FlowLabel:16,Rest/binary>>,PresentIEs,Errors,UpdateReq) ->
    if
        (PresentIEs band 16#00000004) =:= 16#00000004 -> %Repeated IE, ignored
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        PresentIEs > 16#00000004 -> %Out of sequence
            UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
            UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{flowLblData=FlowLabel},
            decode_ie_update(Rest,(PresentIEs bor 16#00000004),
                UpdatedErrors,UpdatedUpdateReq);
        true -> %OK
            UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{flowLblData=FlowLabel},
            decode_ie_update(Rest,(PresentIEs bor 16#00000004),Errors,
			     UpdatedUpdateReq)
    end;

%%% Flow Label Signalling, Mandatory
decode_ie_update(<<17:8,FlowLabel:16,Rest/binary>>,PresentIEs,Errors,UpdateReq) ->
    if
        (PresentIEs band 16#00000008) =:= 16#00000008 -> %Repeated IE, ignored
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        PresentIEs > 16#00000008 -> %Out of sequence
            UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
            UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{flowLblSig=FlowLabel},
            decode_ie_update(Rest,(PresentIEs bor 16#00000008),
                UpdatedErrors,UpdatedUpdateReq);
        true -> %OK
            UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{flowLblSig=FlowLabel},
            decode_ie_update(Rest,(PresentIEs bor 16#00000008),Errors,
			     UpdatedUpdateReq)
    end;

%%% SGSN Address for signalling, Mandatory OR SGSN Address for user traffic, Mandatory
decode_ie_update(<<133:8,Length:16,More/binary>>,PresentIEs,
    Errors,UpdateReq) ->
    <<AddressElement:Length/binary-unit:8,Rest/binary>> = More,
    if
        (PresentIEs band 16#00000030) =:= 16#00000030 -> %Repeated IE, ignore
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        PresentIEs > 16#00000020 -> %Out of sequence
            if
                (PresentIEs band 16#00000010) =:= 16#00000000 -> %Signalling
                    case gsn_addr_internal_storage(AddressElement) of
			{ok,GSNAddr} ->
			    UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
			    UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{sgsnAddSig=GSNAddr},
			    decode_ie_update(Rest,(PresentIEs bor 16#00000010),
					     UpdatedErrors,UpdatedUpdateReq);
			{fault} ->
			    UpdatedErrors=Errors#protocolErrors{invalidManIE=true,
								outOfSequence=true},
			    decode_ie_update(Rest,(PresentIEs bor 16#00000010),
					     UpdatedErrors,UpdateReq)
		    end;
                true -> % User traffic
                    case gsn_addr_internal_storage(AddressElement) of
			{ok,GSNAddr} ->
			    UpdatedErrors=Errors#protocolErrors{outOfSequence=true},
			    UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{sgsnAddUser=GSNAddr},
			    decode_ie_update(Rest,(PresentIEs bor 16#00000020),
					     UpdatedErrors,UpdatedUpdateReq);
			{fault} ->
			    UpdatedErrors=Errors#protocolErrors{invalidManIE=true,
								outOfSequence=true},
			    decode_ie_update(Rest,(PresentIEs bor 16#00000020),
					     UpdatedErrors,UpdateReq)
		    end
            end;
        PresentIEs < 16#00000010 -> %OK, SGSN Address for signalling
            case gsn_addr_internal_storage(AddressElement) of
		{ok,GSNAddr} ->
		    UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{sgsnAddSig=GSNAddr},
		    decode_ie_update(Rest,(PresentIEs bor 16#00000010),
				     Errors,UpdatedUpdateReq);
		{fault} ->
		    UpdatedErrors=Errors#protocolErrors{invalidManIE=true},
		    decode_ie_update(Rest,(PresentIEs bor 16#00000010),
				     UpdatedErrors,UpdateReq)
	    end;
        true -> %OK, SGSN Address for user traffic
            case gsn_addr_internal_storage(AddressElement) of
		{ok,GSNAddr} ->
		    UpdatedUpdateReq=UpdateReq#sesT_updateReqV0{sgsnAddUser=GSNAddr},
		    decode_ie_update(Rest,(PresentIEs bor 16#00000020),
				     Errors,UpdatedUpdateReq);
		{fault} ->
		    UpdatedErrors=Errors#protocolErrors{invalidManIE=true},
		    decode_ie_update(Rest,(PresentIEs bor 16#00000020),
				     UpdatedErrors,UpdateReq)
	    end
    end;

%%% Private Extension, Optional
%%% Not implemented

%%% Error handling, Unexpected or unknown IE
decode_ie_update(UnexpectedIE,PresentIEs,Errors,UpdateReq) ->
    case check_ie(UnexpectedIE) of
        {defined_ie,Rest} -> %OK, ignored
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        {handled_ie,Rest} -> %OK, ignored
            decode_ie_update(Rest,PresentIEs,Errors,UpdateReq);
        {unhandled_ie} -> %Error, abort decoding
            {fault,193,UpdateReq} %Invalid message format
    end.


%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% decode_ie_delete_req/4
%%% Decode information elements for Delete PDP Context Request

%%% Private Extension, Optional
%%% Not implemented


%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% decode_ie_delete_res/4
%%% Decode information elements for Delete PDP Context Response

%%% All elements decoded
decode_ie_delete_res(<<>>,PresentIEs,DeleteRes) ->
    %% Check mandatory IE's
    if
	(PresentIEs band 16#0001) =/= 16#0001 ->
	    {fault,202,DeleteRes}; %Mandatory IE missing
	true -> %OK
	    {ok,DeleteRes}
    end;

%%% Cause, Mandatory
decode_ie_delete_res(<<1:8,Cause:8,Rest/binary>>,PresentIEs,DeleteRes) ->
    if
        (PresentIEs band 16#00000001) =:= 16#00000001 -> %Repeated IE, ignored
            decode_ie_delete_res(Rest,PresentIEs,DeleteRes);
        true -> %OK
            UpdatedDeleteRes=DeleteRes#sesT_deleteResV0{cause=Cause},
            decode_ie_delete_res(Rest,(PresentIEs bor 16#00000001),
		UpdatedDeleteRes)
     end;

%%% Private Extension, Optional
%%% Not implemented

%%% Error handling, Unexpected or unknown IE
decode_ie_delete_res(UnexpectedIE,PresentIEs,DeleteRes) ->
    case check_ie(UnexpectedIE) of
        {defined_ie,Rest} -> %OK, ignored
            decode_ie_delete_res(Rest,PresentIEs,DeleteRes);
        {handled_ie,Rest} -> %OK, ignored
            decode_ie_delete_res(Rest,PresentIEs,DeleteRes);
        {unhandled_ie} -> %Error, abort decoding
            {fault,193,DeleteRes} %Invalid message format
    end.

%%% --------------------------------------------------------------
%%% #3.2 COMMON INTERNAL FUNCTIONS
%%% --------------------------------------------------------------

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% check_ie/1
%%% Check Information Element, Unexpected or Unknown
check_ie(<<1:8,_:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% IMSI
check_ie(<<2:8,_:8/binary-unit:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% RAI
check_ie(<<3:8,_:6/binary-unit:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% TTLI
check_ie(<<4:8,_:4/binary-unit:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% P-TMSI
check_ie(<<5:8,_:4/binary-unit:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% Quality of Service Profile
check_ie(<<6:8,_:3/binary-unit:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% Reordering Required
check_ie(<<8:8,_:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% Authentication Triplet
check_ie(<<9:8,_:28/binary-unit:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% MAP Cause
check_ie(<<11:8,_:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% P-TMSI Signature
check_ie(<<12:8,_:3/binary-unit:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% MS Validated
check_ie(<<13:8,_:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% Recovery
check_ie(<<14:8,_:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% Selection Mode
check_ie(<<15:8,_:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% Flow Label Data I
check_ie(<<16:8,_:16,Rest/binary>>) ->
    {defined_ie,Rest};
%%% Flow Label Signalling
check_ie(<<17:8,_:16,Rest/binary>>) ->
    {defined_ie,Rest};
%%% Flow Label Data II
check_ie(<<18:8,_:32,Rest/binary>>) ->
    {defined_ie,Rest};
%%% MS Not Reachable Reason
check_ie(<<19:8,_:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% Charging ID
check_ie(<<127:8,_:4/binary-unit:8,Rest/binary>>) ->
    {defined_ie,Rest};
%%% TLV element, skipped using Length
check_ie(<<1:1,_:7,Length:16,More/binary>>) ->
    if
        Length > byte_size(More) ->
            {unhandled_ie};
        true ->
            <<_:Length/binary-unit:8,Rest/binary>> = More,
            {handled_ie,Rest}
    end;
%%% TV element, unknown size. Can not be handled.
check_ie(_UnhandledIE) ->
    {unhandled_ie}.

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% tid_internal_storage/3
%%% Convert TID binary to internal datatype
tid_internal_storage(Bin,_) ->
    Size = byte_size(Bin) - 1,
    <<Front:Size/binary,NSAPI:4,DigitN:4>> = Bin,
    Result =
	case DigitN of
	    2#1111 ->
		#mvsgT_tid{imsi = #mvsgT_imsi{value = Front}, nsapi = NSAPI};
	    _ ->
		Value = <<Front/binary,2#1111:4,DigitN:4>>,
		#mvsgT_tid{imsi = #mvsgT_imsi{value = Value}, nsapi = NSAPI}
	end,
    {ok,Result}.
%% tid_internal_storage(<<NSAPI:4,2#1111:4>>,IMSI) ->
%%      {ok,#mvsgT_tid{imsi=#mvsgT_imsi{value=lists:reverse(IMSI)},
%%  		  nsapi=NSAPI}};
%% tid_internal_storage(<<NSAPI:4,DigitN:4>>,IMSI) when
%%    DigitN < 10 ->
%%      {ok,#mvsgT_tid{imsi=#mvsgT_imsi{value=lists:reverse([(DigitN bor 2#11110000)|IMSI])},
%%  		  nsapi=NSAPI}};
%% tid_internal_storage(<<2#11111111:8,Rest/binary>>,IMSI) ->
%%      tid_internal_storage(Rest,IMSI);
%% tid_internal_storage(<<2#1111:4,DigitN:4,Rest/binary>>,IMSI) when
%%    DigitN < 10 ->
%%      tid_internal_storage(Rest,[(DigitN bor 2#11110000)|IMSI]);
%% tid_internal_storage(<<DigitNplus1:4,DigitN:4,Rest/binary>>,IMSI) when
%%    DigitNplus1 < 10,
%%    DigitN < 10 ->
%%      tid_internal_storage(Rest,[((DigitNplus1 bsl 4) bor DigitN)|IMSI]);
%% tid_internal_storage(_Rest,_IMSI) ->
%%      {fault}. %% Mandatory IE incorrect

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% selection_mode_internal_storage/1
%%% Convert Selection Mode integer to internal datatype (enum)
selection_mode_internal_storage(0) ->
    subscribed;
selection_mode_internal_storage(1) ->
    msRequested;
selection_mode_internal_storage(2) ->
    sgsnSelected;
selection_mode_internal_storage(3) ->
    sgsnSelected.

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% pdp_addr_internal_storage/1
%%% Convert PDP address to internal datatype (record containing
%%% addresstype and value)
pdp_addr_internal_storage(<<_:4,0:4,1:8>>) ->
    {ok,#mvsgT_pdpAddressType{pdpTypeNbr=etsi_ppp,address=[]}};
pdp_addr_internal_storage(<<_:4,0:4,2:8>>) ->
    {ok,#mvsgT_pdpAddressType{pdpTypeNbr=etsi_osp_ihoss,address=[]}};
pdp_addr_internal_storage(<<_:4,1:4,16#21:8>>) ->
    {ok,#mvsgT_pdpAddressType{pdpTypeNbr=ietf_ipv4,address=[]}};
pdp_addr_internal_storage(<<_:4,1:4,16#21:8,IP_A:8,IP_B:8,IP_C:8,IP_D:8>>) ->
    {ok,#mvsgT_pdpAddressType{pdpTypeNbr=ietf_ipv4,
        address=[IP_A,IP_B,IP_C,IP_D]}};
pdp_addr_internal_storage(<<_:4,1:4,16#57:8,IP_A:16,IP_B:16,IP_C:16,IP_D:16,
    IP_E:16,IP_F:16,IP_G:16,IP_H:16>>) ->
    {ok,#mvsgT_pdpAddressType{pdpTypeNbr=ietf_ipv6,
        address=[IP_A,IP_B,IP_C,IP_D,IP_E,IP_F,IP_G,IP_H]}};
pdp_addr_internal_storage(_PDP_ADDR) ->
    {fault}.

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% apn_internal_storage/2
%%% Convert APN to internal datatype (List containing APN labels)
apn_internal_storage(<<>>,APN) ->
    {ok,lists:reverse(APN)};
apn_internal_storage(<<Length:8,Rest/binary>>,APN) ->
    <<Label:Length/binary-unit:8,MoreAPNLabels/binary>> = Rest,
    apn_internal_storage(MoreAPNLabels,[Label|APN]).

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% pco_internal_storage/1
%%% Convert Protocol Configuration Options to internal datatype.
%%% Implemented configuration options:
%%% For PPP:
%%% LCP - Not implemented
%%% PAP - Authenticate request
%%% CHAP - Challenge
%%%      - Response
%%% IPCP - IP-Address
%%% For OSP:IHOSS
%%% Nothing implemented
pco_internal_storage(<<1:1,_:4,0:3,PPPConfigurationOptions/binary>>) ->
    case ppp_configuration_options(PPPConfigurationOptions,
				   #masT_pap{exists=false},[],[]) of
	{ok,PAP,CHAP,IPCP} ->
	    {ok,#masT_protocolConfigOptions{pap=PAP,chap=CHAP,ipcp=IPCP}};
	{fault} ->
	    {fault}
    end;
pco_internal_storage(<<1:1,_:4,1:3,_OSP_IHOSSConfigurationOptions/binary>>) ->
    {ok,osp_ihoss};
pco_internal_storage(_UnknownConfigurationOptions) ->
    {fault}. %% Optional IE incorrect

ppp_configuration_options(<<>>,PAP,CHAP,IPCP) ->
    {ok,PAP,CHAP,IPCP};
ppp_configuration_options(<<16#C021:16,Length:8,More/binary>>,PAP,CHAP,IPCP) ->
    %% LCP - Not implemented
    <<_LCP:Length/binary-unit:8,Rest/binary>> = More,
    ppp_configuration_options(Rest,PAP,CHAP,IPCP);
ppp_configuration_options(<<16#C023:16,_Length:8,1:8,Identifier:8,DataLength:16,
			  More/binary>>,_PAP,CHAP,IPCP) ->
    %% PAP - Authenticate request
    ActualDataLength=DataLength-4, %% DataLength includes Code, Identifier and itself
    <<Data:ActualDataLength/binary-unit:8,Rest/binary>> = More,
    <<PeerIDLength:8,PeerData/binary>> = Data,
    <<PeerID:PeerIDLength/binary-unit:8,PasswdLength:8,PasswordData/binary>> = PeerData,
    <<Password:PasswdLength/binary,_Padding/binary>> = PasswordData,
    ppp_configuration_options(Rest,#masT_pap{exists=true,code=1,id=Identifier,
					     username=binary_to_list(PeerID),
					     password=binary_to_list(Password)},CHAP,IPCP);

ppp_configuration_options(<<16#C023:16,Length:8,More/binary>>,PAP,CHAP,IPCP) ->
    %% PAP - Other, not implemented
    <<_PAP:Length/binary-unit:8,Rest/binary>> = More,
    ppp_configuration_options(Rest,PAP,CHAP,IPCP);
ppp_configuration_options(<<16#C223:16,_Length:8,1:8,Identifier:8,DataLength:16,
			 More/binary>>,PAP,CHAP,IPCP) ->
    %% CHAP - Challenge
    ActualDataLength=DataLength-4, %% DataLength includes Code, Identifier and itself
    <<Data:ActualDataLength/binary-unit:8,Rest/binary>> = More,
    <<ValueSize:8,ValueAndName/binary>> = Data,
    <<Value:ValueSize/binary-unit:8,Name/binary>> = ValueAndName,
    ppp_configuration_options(Rest,PAP,[#masT_chap{code=1,id=Identifier,
						   value=binary_to_list(Value),
						   name=binary_to_list(Name)}|CHAP],
			      IPCP);
ppp_configuration_options(<<16#C223:16,_Length:8,2:8,Identifier:8,DataLength:16,
			 More/binary>>,PAP,CHAP,IPCP) ->
    %% CHAP - Response
    ActualDataLength=DataLength-4, %% DataLength includes Code, Identifier and itself
    <<Data:ActualDataLength/binary-unit:8,Rest/binary>> = More,
    <<ValueSize:8,ValueAndName/binary>> = Data,
    <<Value:ValueSize/binary-unit:8,Name/binary>> = ValueAndName,
    ppp_configuration_options(Rest,PAP,[#masT_chap{code=2,id=Identifier,
						   value=binary_to_list(Value),
						   name=binary_to_list(Name)}|CHAP],
			      IPCP);
ppp_configuration_options(<<16#C223:16,Length:8,More/binary>>,PAP,CHAP,IPCP) ->
    %% CHAP - Other, not implemented
    <<_CHAP:Length/binary-unit:8,Rest/binary>> = More,
    ppp_configuration_options(Rest,PAP,CHAP,IPCP);
ppp_configuration_options(<<16#8021:16,_Length:8,1:8,Identifier:8,OptionsLength:16,
			  More/binary>>,PAP,CHAP,IPCP) ->
    %% IPCP - Configure request
    ActualOptionsLength=OptionsLength-4, %% OptionsLength includes Code, Identifier and itself
    <<Options:ActualOptionsLength/binary-unit:8,Rest/binary>> = More,
    case Options of
	<<3:8,6:8,A1:8,A2:8,A3:8,A4:8>> ->
	    %% IP Address, version 4
	    ppp_configuration_options(Rest,PAP,CHAP,
				      [#masT_ipcp{exists=true,code=1,
						 id=Identifier,
						 ipcpList=[#masT_ipcpData{type=3,ipAddress=
									  #mvsgT_ipAddress{version=ipv4,
											   a1=A1,a2=A2,
											   a3=A3,a4=A4,
											   a5=0,a6=0,
											   a7=0,a8=0},
									  rawMessage=binary_to_list(Options)}]}|IPCP]);
	<<129:8,6:8,B1:8,B2:8,B3:8,B4:8>> ->
	     %% IP Address, version 4
	     ppp_configuration_options(Rest,PAP,CHAP,
				      [#masT_ipcp{exists=true,code=1,
						 id=Identifier,
						 ipcpList=[#masT_ipcpData{type=129,ipAddress=
									  #mvsgT_ipAddress{version=ipv4,
											   a1=B1,a2=B2,
											   a3=B3,a4=B4},
									  rawMessage=binary_to_list(Options)}]}|IPCP]);

	<<131:8,6:8,C1:8,C2:8,C3:8,C4:8>> ->
	    %% IP Address, version 4
	    ppp_configuration_options(Rest,PAP,CHAP,
				      [#masT_ipcp{exists=true,code=1,
						 id=Identifier,
						 ipcpList=[#masT_ipcpData{type=131,ipAddress=
									  #mvsgT_ipAddress{version=ipv4,
											   a1=C1,a2=C2,
											   a3=C3,a4=C4},
									  rawMessage=binary_to_list(Options)}]}|IPCP]);
	_ ->
	    ppp_configuration_options(Rest,PAP,CHAP,IPCP)
    end;
ppp_configuration_options(<<_UnknownProtocolID:16,Length:8,More/binary>>,
			  PAP,CHAP,IPCP) ->
    <<_Skipped:Length/binary-unit:8,Rest/binary>> = More,
    ppp_configuration_options(Rest,PAP,CHAP,IPCP);
ppp_configuration_options(_Unhandled,_PAP,_CHAP,_IPCP) ->
    {fault}.

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% gsn_addr_internal_storage/1
%%% Convert GSN Address to internal datatype
gsn_addr_internal_storage(<<IP_A:8,IP_B:8,IP_C:8,IP_D:8>>) ->
    {ok,#mvsgT_ipAddress{version=ipv4,a1=IP_A,a2=IP_B,a3=IP_C,a4=IP_D,a5=0,a6=0,a7=0,a8=0}};
gsn_addr_internal_storage(<<IP_A:16,IP_B:16,IP_C:16,IP_D:16,
    IP_E:16,IP_F:16,IP_G:16,IP_H:16>>) ->
    {ok,#mvsgT_ipAddress{version=ipv6,a1=IP_A,a2=IP_B,a3=IP_C,a4=IP_D,
        a5=IP_E,a6=IP_F,a7=IP_G,a8=IP_H}};
gsn_addr_internal_storage(_GSN_ADDR) ->
    {fault}.

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% msisdn_internal_storage/3
%%% Convert MSISDN binary to internal datatype (TBCD-octet list)

msisdn_internal_storage(<<>>,MSISDN) ->
    {ok,#mvsT_msisdn{value=lists:reverse(MSISDN)}};
msisdn_internal_storage(<<2#11111111:8,_Rest/binary>>,MSISDN) ->
    {ok,#mvsT_msisdn{value=lists:reverse(MSISDN)}};
msisdn_internal_storage(<<2#1111:4,DigitN:4,_Rest/binary>>,MSISDN) when
      DigitN < 10 ->
    {ok,#mvsT_msisdn{value=lists:reverse([(DigitN bor 2#11110000)|MSISDN])}};
msisdn_internal_storage(<<DigitNplus1:4,DigitN:4,Rest/binary>>,MSISDN) when
      DigitNplus1 < 10,
      DigitN < 10 ->
    NewMSISDN=[((DigitNplus1 bsl 4) bor DigitN)|MSISDN],
    msisdn_internal_storage(Rest,NewMSISDN);
msisdn_internal_storage(_Rest,_MSISDN) ->
    {fault}. %% Mandatory IE incorrect
