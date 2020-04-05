%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2019. All Rights Reserved.
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
-module(snmpa_mpd).

-export([init/1, reset/0, inc/1, counters/0, 
	 discarded_pdu/1,
	 process_packet/5, process_packet/6, process_packet/7,
	 generate_response_msg/5, generate_response_msg/6, 
	 generate_msg/5, generate_msg/6, 
	 generate_discovery_msg/4, 
	 process_taddrs/1, 
	 generate_req_id/0]).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-include("SNMP-MPD-MIB.hrl").
-include("SNMPv2-TM.hrl").
-include("SNMP-FRAMEWORK-MIB.hrl").
-include("TRANSPORT-ADDRESS-MIB.hrl").

-define(VMODULE,"MPD").
-include("snmp_verbosity.hrl").
-include("snmpa_internal.hrl").

-define(empty_msg_size, 24).

-record(state, {v1 = false, v2c = false, v3 = false}).
-record(note, {sec_engine_id, 
	       sec_model, 
	       sec_name, 
	       sec_level, 
	       ctx_engine_id, 
	       ctx_name, 
	       disco = false, 
	       req_id}).

					
%%%-----------------------------------------------------------------
%%% This module implemets the Message Processing and Dispatch part of
%%% the multi-lingual SNMP agent.
%%%
%%% The MPD is responsible for:
%%%   *) call the security module (auth/priv).
%%%   *) decoding the message into a PDU.
%%%   *) decide a suitable Access Control Model, and provide it with
%%%      the data it needs.
%%%   *) maintaining SNMP counters.
%%%
%%% In order to take care of the different versions of counters, it
%%% implements and maintains the union of all SNMP counters (i.e. from
%%% rfc1213 and from rfc1907).  It is up to the administrator of the
%%% agent to load the correct MIB.  Note that this module implements
%%% the counters only, it does not provide instrumentation functions
%%% for the counters.
%%%
%%% With the terms defined in rfc2271, this module implememts part
%%% of the Dispatcher and the Message Processing functionality.
%%%-----------------------------------------------------------------
init(Vsns) ->
    ?vlog("init -> entry with"
	"~n   Vsns: ~p", [Vsns]),
    ?SNMP_RAND_SEED(),
    ets:insert(snmp_agent_table, {msg_id, rand:uniform(2147483647)}),
    ets:insert(snmp_agent_table, {req_id, rand:uniform(2147483647)}),
    init_counters(),
    init_versions(Vsns, #state{}).


reset() ->
    reset_counters(),
    ok.


%%-----------------------------------------------------------------
%% Purpose: We must calculate the length of a
%%          message with an empty Pdu, and zero-length community
%%          string.  This length is used to calculate the max
%%          pdu size allowed for each request. This size is 
%%          dependent on two dynamic fields, the community string
%%          and the pdu (varbinds actually). It is calculated
%%          as EmptySize + length(CommunityString) + 4.
%%          We assume that the length of the CommunityString is
%%          less than 128 (thus requiring just one octet for the
%%          length field (the same as the zero-length community
%%          string)). 4 comes from the fact that the maximum pdu
%%          size needs 31 bits which needs 5 * 7 bits to be
%%          expressed. One 7bit octet is already present in the
%%          empty msg, leaving 4 more 7bit octets.
%% Actually, this function is not used, we use a constant instead.
%%-----------------------------------------------------------------
%% Ret: 24
%empty_msg() ->
%    M = #message{version = 'version-1', community = "", data = 
%		 #pdu{type = 'get-response', request_id = 1,
%		      error_status = noError, error_index = 0, varbinds = []}},
%    length(snmp_pdus:enc_message(M)) + 4.

%%-----------------------------------------------------------------
%% Func: process_packet(Packet, Domain, Address, State, Log) ->
%%       {ok, SnmpVsn, Pdu, PduMS, ACMData} | {discarded, Reason}
%% Types: Packet = binary()
%%        Domain = snmpUDPDomain | transportDomain()
%%        Address = {Ip, Udp} (*but* depends on Domain)
%%        State = #state
%% Purpose: This is the main Message Dispatching function. (see
%%          section 4.2.1 in rfc2272)
%%-----------------------------------------------------------------
process_packet(Packet, From, State, NoteStore, Log) ->
    LocalEngineID = ?DEFAULT_LOCAL_ENGINE_ID, 
    process_packet(Packet, From, LocalEngineID, State, NoteStore, Log).

process_packet(
  Packet, Domain, Address, LocalEngineID, State, NoteStore, Log) ->
    From = {Domain, Address},
    process_packet(Packet, From, LocalEngineID, State, NoteStore, Log).

process_packet(Packet, Domain, Address, State, NoteStore, Log)
  when is_atom(Domain) ->
    LocalEngineID = ?DEFAULT_LOCAL_ENGINE_ID,
    From = {Domain, Address},
    process_packet(Packet, From, LocalEngineID, State, NoteStore, Log);
process_packet(Packet, From, LocalEngineID, State, NoteStore, Log) ->
    inc(snmpInPkts),
    case catch snmp_pdus:dec_message_only(binary_to_list(Packet)) of

	#message{version = 'version-1', vsn_hdr = Community, data = Data} 
	  when State#state.v1 =:= true ->
	    ?vlog("v1, community: ~s", [Community]),
	    HS = ?empty_msg_size + length(Community),
	    v1_v2c_proc(
	      'version-1', NoteStore, Community, From,
	      LocalEngineID, Data, HS, Log, Packet);

	#message{version = 'version-2', vsn_hdr = Community, data = Data}
	  when State#state.v2c =:= true ->
	    ?vlog("v2c, community: ~s", [Community]),
	    HS = ?empty_msg_size + length(Community),
	    v1_v2c_proc(
	      'version-2', NoteStore, Community, From,
	      LocalEngineID, Data, HS, Log, Packet);

	#message{version = 'version-3', vsn_hdr = V3Hdr, data = Data}
	  when State#state.v3 =:= true ->
	    ?vlog("v3, msgID: ~p, msgFlags: ~p, msgSecModel: ~p",
		  [V3Hdr#v3_hdr.msgID,
		   V3Hdr#v3_hdr.msgFlags,
		   V3Hdr#v3_hdr.msgSecurityModel]),
	    v3_proc(
	      NoteStore, Packet, From,
	      LocalEngineID, V3Hdr, Data, Log);

	{'EXIT', {bad_version, Vsn}} ->
	    ?vtrace("exit: bad version: ~p",[Vsn]),
	    inc(snmpInBadVersions),
	    {discarded, snmpInBadVersions};

	{'EXIT', Reason} ->
	    ?vtrace("exit: ~p", [Reason]),
	    inc(snmpInASNParseErrs),
	    {discarded, Reason};

	UnknownMessage ->
	    ?vtrace("Unknown message: ~n   ~p"
		"~nwhen"
		"~n   State: ~p", [UnknownMessage, State]),
	    inc(snmpInBadVersions),
	    {discarded, snmpInBadVersions}
    end.

discarded_pdu(false) -> ok;
discarded_pdu(Variable) -> inc(Variable).


%%-----------------------------------------------------------------
%% Handles a Community based message (v1 or v2c).
%%-----------------------------------------------------------------
v1_v2c_proc(
  Vsn, NoteStore, Community, From,
  LocalEngineID, Data, HS, Log, Packet) ->
    try
	case From of
	    {D, A} when is_atom(D) ->
		{snmp_conf:mk_tdomain(D),
		 snmp_conf:mk_taddress(D, A)};
	    {_, P} = A when is_integer(P) ->
		{snmp_conf:mk_tdomain(),
		 snmp_conf:mk_taddress(A)}
	end
    of
	{TDomain, TAddress} ->
	    v1_v2c_proc_dec(
	      Vsn, NoteStore, Community, TDomain, TAddress,
	      LocalEngineID, Data, HS, Log, Packet)
    catch
	_ ->
	    {discarded, {badarg, From}}
    end.


v1_v2c_proc_dec(
  Vsn, NoteStore, Community, TDomain, TAddress,
  LocalEngineID, Data, HS, Log, Packet) ->
    AgentMS  = get_engine_max_message_size(LocalEngineID),
    MgrMS    = snmp_community_mib:get_target_addr_ext_mms(TDomain, TAddress),
    PduMS    = case MgrMS of
		   {ok, MMS} when MMS < AgentMS -> MMS - HS;
		   _ -> AgentMS - HS
	       end,
    case (catch snmp_pdus:dec_pdu(Data)) of
	Pdu when is_record(Pdu, pdu) ->
	    Log(Pdu#pdu.type, Packet),
	    inc_snmp_in_vars(Pdu),
	    #pdu{request_id = ReqId} = Pdu,
	    
	    %% <TDomain>
	    %% We have added TDomain, what are the consequences?
	    ACMData = 
		{community, sec_model(Vsn), Community, TDomain, TAddress}, 
	    OkRes = {ok, Vsn, Pdu, PduMS, ACMData},
	    %% </TDomain>

	    %% Make sure that we don't process duplicate SET request
	    %% twice.  We don't know what could happen in that case.
	    %% The mgr does, so he has to generate a new SET request.
	    ?vdebug("PDU type: ~p", [Pdu#pdu.type]),
	    case Pdu#pdu.type of
		'set-request' ->
		    %% Check if this message has already been processed
		    Key = {agent, {TDomain, TAddress}, ReqId},
		    case snmp_note_store:get_note(NoteStore, Key) of
			undefined -> 
			    %% Set the processed note _after_ pdu processing. 
			    %% This makes duplicated requests be ignored even 
			    %% if pdu processing took long time.
			    snmp_note_store:set_note(NoteStore, 
						     100, Key, true),
			    %% Uses ACMData that snmpa_acm knows of.
			    OkRes;
			true ->
			    {discarded, duplicate_pdu}
		    end;
		_ ->
		    OkRes
	    end;
	{'EXIT', Reason} ->
	    ?vtrace("PDU decode exit: ~p",[Reason]),
	    inc(snmpInASNParseErrs),
	    {discarded, Reason};
	_TrapPdu ->
	    {discarded, trap_pdu}
    end.

sec_model('version-1') -> ?SEC_V1;
sec_model('version-2') -> ?SEC_V2C.


%%-----------------------------------------------------------------
%% Handles a SNMPv3 Message, following the procedures in rfc2272,
%% section 4.2 and 7.2
%%-----------------------------------------------------------------
v3_proc(NoteStore, Packet, _From, LocalEngineID, V3Hdr, Data, Log) ->
    case (catch v3_proc(NoteStore, Packet, LocalEngineID, V3Hdr, Data, Log)) of
	{'EXIT', Reason} ->
	    exit(Reason);
	Result ->
	    Result
    end.

v3_proc(NoteStore, Packet, LocalEngineID, V3Hdr, Data, Log) ->
    ?vtrace("v3_proc -> entry with"
	    "~n   LocalEngineID: ~p",
	    [LocalEngineID]),
    %% 7.2.3
    #v3_hdr{msgID                 = MsgID, 
	    msgMaxSize            = MMS, 
	    msgFlags              = MsgFlags,
	    msgSecurityModel      = MsgSecurityModel,
	    msgSecurityParameters = SecParams, 
	    hdr_size              = HdrSize} = V3Hdr,
    ?vdebug("v3_proc -> version 3 message header [7.2.3]:"
	    "~n   msgID                 = ~p"
	    "~n   msgMaxSize            = ~p"
	    "~n   msgFlags              = ~p"
	    "~n   msgSecurityModel      = ~p"
	    "~n   msgSecurityParameters = ~w",
	    [MsgID, MMS, MsgFlags, MsgSecurityModel, SecParams]),
    %% 7.2.4
    SecModule    = get_security_module(MsgSecurityModel),
    %% 7.2.5
    SecLevel     = check_sec_level(MsgFlags),
    IsReportable = snmp_misc:is_reportable(MsgFlags),
    %% 7.2.6
    ?vtrace("v3_proc -> [7.2.4-7.2.6]"
	    "~n   SecModule    = ~p"
	    "~n   SecLevel     = ~p"
	    "~n   IsReportable = ~p",
	    [SecModule, SecLevel, IsReportable]),
    SecRes = (catch SecModule:process_incoming_msg(Packet, Data,
						   SecParams, SecLevel, 
						   LocalEngineID)),
    ?vtrace("v3_proc -> message processing result: "
	    "~n   SecRes: ~p", [SecRes]),
    {SecEngineID, SecName, ScopedPDUBytes, SecData, DiscoOrPlain} =
	check_sec_module_result(SecRes, V3Hdr, Data, 
				LocalEngineID, IsReportable, Log),
    ?vtrace("v3_proc -> "
	    "~n   DiscoOrPlain: ~w"
	    "~n   SecEngineID:  ~w"
	    "~n   SecName:      ~p", [DiscoOrPlain, SecEngineID, SecName]),
    %% 7.2.7
    #scopedPdu{contextEngineID = ContextEngineID,
	       contextName     = ContextName,
	       data            = PDU} =
	case (catch snmp_pdus:dec_scoped_pdu(ScopedPDUBytes)) of
	    ScopedPDU when is_record(ScopedPDU, scopedPdu) -> 
		?vtrace("v3_proc -> message processing result: "
			"~n   ScopedPDU: ~p", [ScopedPDU]),
		ScopedPDU;
	    {'EXIT', Reason} ->
		inc(snmpInASNParseErrs),
		throw({discarded, Reason})
	end,
    %% We'll have to take care of the unlikely case that we receive an
    %% v1 trappdu in a v3 message explicitly...
    if
	is_record(PDU, trappdu) ->
	    inc(snmpUnknownPDUHandlers),
	    throw({discarded, received_v1_trap});
	true ->
	    ok
    end,
    ?vlog("7.2.7 result: "
	  "~n   contextEngineID: ~w"
	  "~n   ContextName:     \"~s\"", [ContextEngineID, ContextName]),
    if
	SecLevel =:= ?'SnmpSecurityLevel_authPriv' -> 
	    %% encrypted message - log decrypted pdu
	    Log(PDU#pdu.type, {V3Hdr, ScopedPDUBytes});
	true -> % otherwise, log binary
	    Log(PDU#pdu.type, Packet)
    end,
    %% Make sure a get_bulk doesn't get too big.
    AgentMS = get_engine_max_message_size(LocalEngineID),
    %% PduMMS is supposed to be the maximum total length of the response
    %% PDU we can send.  From the MMS, we need to subtract everything before
    %% the PDU, i.e. Message and ScopedPDU.
    %%   Message: [48, TotalLen, Vsn, [Tag, LH, Hdr], [Tag, LM, MsgSec], Data]
    %%             1              3   <----------- HdrSize ----------->
    %%   HdrSize = everything up to and including msgSecurityParameters.
    %% ScopedPduData follows.  This is
    %%   [Tag, Len, [Tag, L1, CtxName], [Tag, L2, CtxEID]]
    %%   i.e. 6 + length(CtxName) + length(CtxEID)
    %% 
    %% Total: 1 + TotalLenOctets + 3 + ScopedPduDataLen
    TotMMS = if AgentMS > MMS -> MMS;
		true -> AgentMS
	     end,
    TotalLenOctets = snmp_pdus:get_encoded_length(TotMMS - 1),
    PduMMS = TotMMS - TotalLenOctets - 10 - HdrSize - 
	length(ContextName) - length(ContextEngineID),
    ?vdebug("v3_proc -> PDU type: ~p", [PDU#pdu.type]),
    case PDU#pdu.type of
	report when DiscoOrPlain =:= discovery ->
	    %% Discovery stage 1 response
	    Key  = {agent, MsgID}, 
	    Note = snmp_note_store:get_note(NoteStore, Key), 
	    case Note of
                #note{sec_engine_id = "",
                      sec_model     = _MsgSecModel,
                      sec_name      = "",
                      sec_level     = _SecLevel,
                      ctx_engine_id = _CtxEngineID,
                      ctx_name      = _CtxName,
                      disco         = true,
                      req_id        = _ReqId} ->
                    %% This is part of the discovery process initiated by us.
                    %% Response to the discovery stage 1 request
                    ?vdebug("v3_proc -> discovery stage 1 response", []),
                    {ok, 'version-3', PDU, PduMMS, {discovery, SecEngineID}};
                #note{sec_engine_id = SecEngineID,
                      sec_model     = _MsgSecModel,
                      sec_name      = SecName,
                      sec_level     = _SecLevel,   % OTP-16207
                      ctx_engine_id = _CtxEngineID,
                      ctx_name      = _CtxName,
                      disco         = true,
                      req_id        = _ReqId} ->
                    %% This is part of the discovery process initiated by us.
                    %% Response to the discovery stage 2 request
                    ?vdebug("v3_proc -> discovery stage 2 response", []),
                    {ok, 'version-3', PDU, PduMMS, discovery};
		_ ->
		    %% 7.2.11
		    DiscardReason = {bad_disco_note, Key, Note}, 
		    throw({discarded, DiscardReason})
	    end;
	report ->
	    %% 7.2.11
	    throw({discarded, report});
	'get-response' -> %% As a result of a sent inform-request?
	    %% 7.2.12
	    Key  = {agent, MsgID}, 
	    Note = snmp_note_store:get_note(NoteStore, Key), 
	    case Note of
                #note{sec_engine_id = "",
                      sec_model     = _MsgSecModel,
                      sec_name      = "",
                      sec_level     = _SecLevel,
                      ctx_engine_id = _CtxEngineID,
                      ctx_name      = _CtxName,
                      disco         = true,
                      req_id        = _ReqId} ->
                    %% This is part of the discovery process initiated by us.
                    %% Response to the discovery stage 1 request
                    ?vdebug("v3_proc -> discovery stage 1 response", []),
                    {ok, 'version-3', PDU, PduMMS, {discovery, SecEngineID}};
                #note{sec_engine_id = SecEngineID,
                      sec_model     = _MsgSecModel,
                      sec_name      = SecName,
                      sec_level     = SecLevel,
                      ctx_engine_id = _CtxEngineID,
                      ctx_name      = _CtxName,
                      disco         = true,
                      req_id        = _ReqId} ->
                    %% This is part of the discovery process initiated by us.
                    %% Response to the discovery stage 2 request
                    ?vdebug("v3_proc -> discovery stage 2 response", []),
                    {ok, 'version-3', PDU, PduMMS, discovery};
		#note{sec_engine_id = SecEngineID, 
		      sec_model     = MsgSecurityModel, 
		      sec_name      = SecName, 
		      sec_level     = SecLevel,
		      ctx_engine_id = ContextEngineID, 
		      ctx_name      = ContextName,
		      disco         = false,
		      req_id        = _ReqId} ->
		    {ok, 'version-3', PDU, PduMMS, undefined};
		_ ->
		    inc(snmpUnknownPDUHandlers),
		    throw({discarded, {no_outstanding_req, MsgID}})
	    end;
	'snmpv2-trap' ->
	    inc(snmpUnknownPDUHandlers),
	    throw({discarded, received_v2_trap});
	Type ->
	    %% 7.2.13
	    SnmpEngineID = LocalEngineID, 
	    ?vtrace("v3_proc -> 7.2.13", []),
	    case SecEngineID of
		SnmpEngineID when (DiscoOrPlain =:= discovery) ->
		    %% This is a discovery step 2 message!
		    ?vtrace("v3_proc -> discovery stage 2", []),
		    generate_discovery2_report_msg(MsgID, 
						   MsgSecurityModel, 
						   SecName, 
						   SecLevel, 
						   ContextEngineID, 
						   ContextName,
						   SecData, 
						   PDU, 
						   LocalEngineID, 
						   Log);

		SnmpEngineID when (DiscoOrPlain =:= plain) ->
		    %% 4.2.2.1.1 - we don't handle proxys yet => we only 
		    %% handle ContextEngineID to ourselves
		    case ContextEngineID of
			SnmpEngineID ->
			    %% Uses ACMData that snmpa_acm knows of.
			    {ok, 'version-3', PDU, PduMMS, 
			     {v3, MsgID, MsgSecurityModel, SecName, SecLevel,
			      ContextEngineID, ContextName, SecData}};
			_ ->
			    %% 4.2.2.1.2
			    NIsReportable = snmp_misc:is_reportable_pdu(Type),
			    ErrorInfo = 
				snmpUnknownPDUHandlers_ei(SecName, SecLevel, 
							  ContextEngineID, 
							  ContextName),
			    case generate_v3_report_msg(MsgID, 
							MsgSecurityModel,
							Data, LocalEngineID, 
							ErrorInfo, Log) of
				{ok, Report} when NIsReportable =:= true ->
				    {discarded, snmpUnknownPDUHandlers, Report};
				_ ->
				    {discarded, snmpUnknownPDUHandlers}
			    end
		    end;

		"" ->
		    %% This is a discovery step 1 message!!
		    ?vtrace("v3_proc -> discovery step 1", []),
		    generate_discovery1_report_msg(MsgID, 
						   MsgSecurityModel, 
						   SecName, 
						   SecLevel, 
						   ContextEngineID, 
						   ContextName,
						   SecData, 
						   PDU, 
						   LocalEngineID, 
						   Log);
		    
		_ ->
		    {discarded, {badSecurityEngineID, SecEngineID}}
	    end
    end.

make_error_info(Variable, Oid, SecName, Opts) ->
    Val = inc(Variable),
    VB  = #varbind{oid          = Oid, 
		   variabletype = 'Counter32',
		   value        = Val},
    {VB, SecName, Opts}.

snmpUnknownPDUHandlers_ei(SecName, SecLevel, 
			  ContextEngineID, ContextName) ->
    Opts = [{securityLevel,   SecLevel},
	    {contextEngineID, ContextEngineID},
	    {contextName,     ContextName}], 
    make_error_info(snmpUnknownPDUHandlers, 
		    ?snmpUnknownPDUHandlers_instance,
		    SecName, Opts).

get_security_module(?SEC_USM) ->
    snmpa_usm;
get_security_module(_) ->
    inc(snmpUnknownSecurityModels),
    throw({discarded, snmpUnknownSecurityModels}).
    
check_sec_level([MsgFlag]) ->
    SecLevel = MsgFlag band 3,
    if 
	SecLevel == 2 -> 
	    inc(snmpInvalidMsgs),
	    throw({discarded, snmpInvalidMsgs});
	true ->
	    SecLevel
    end;
check_sec_level(Unknown) ->
    ?vlog("invalid msgFlags: ~p",[Unknown]), 
    inc(snmpInvalidMsgs),
    throw({discarded, snmpInvalidMsgs}).

check_sec_module_result(Res, V3Hdr, Data, LocalEngineID, IsReportable, Log) ->
    case Res of
	{ok, X} -> 
	    X;
	{error, Reason, []} ->         % case 7.2.6 b
	    ?vdebug("security module result [7.2.6-b]:"
		    "~n   Reason: ~p", [Reason]),
	    throw({discarded, {securityError, Reason}});
	{error, Reason, ErrorInfo} when IsReportable =:= true -> % case 7.2.6 a
	    ?vdebug("security module result when reportable [7.2.6-a]:"
		    "~n   Reason:    ~p"
		    "~n   ErrorInfo: ~p", [Reason, ErrorInfo]),
	    #v3_hdr{msgID = MsgID, msgSecurityModel = MsgSecModel} = V3Hdr,
	    Pdu = get_scoped_pdu(Data),
	    case generate_v3_report_msg(MsgID, MsgSecModel, Pdu,
					LocalEngineID, ErrorInfo, Log) of
		{ok, Report} ->
		    throw({discarded, {securityError, Reason}, Report});
		{discarded, _SomeOtherReason} ->
		    throw({discarded, {securityError, Reason}})
	    end;
	{error, Reason, ErrorInfo} ->
	    ?vdebug("security module result when not reportable:"
		    "~n   Reason:    ~p"
		    "~n   ErrorInfo: ~p", [Reason, ErrorInfo]),
	    throw({discarded, {securityError, Reason}});
	Else ->
	    ?vdebug("security module result:"
		    "~n   Else: ~p", [Else]),
	    throw({discarded, {securityError, Else}})
    end.

get_scoped_pdu(D) when is_list(D) ->
    (catch snmp_pdus:dec_scoped_pdu(D));
get_scoped_pdu(D) ->
    D.


%%-----------------------------------------------------------------
%% Executed when a response or report message is generated.
%%-----------------------------------------------------------------
generate_response_msg(Vsn, RePdu, Type, ACMData, Log) ->
    generate_response_msg(Vsn, RePdu, Type, ACMData, Log, 1).

generate_response_msg(Vsn, RePdu, Type, ACMData, Log, N) when is_integer(N) ->
    LocalEngineID = ?DEFAULT_LOCAL_ENGINE_ID, 
    generate_response_msg(Vsn, RePdu, Type, ACMData, LocalEngineID, Log, N);
generate_response_msg(Vsn, RePdu, Type, ACMData, LocalEngineID, Log) ->
    generate_response_msg(Vsn, RePdu, Type, ACMData, LocalEngineID, Log, 1).

generate_response_msg(Vsn, RePdu, Type, 
		      {community, _SecModel, Community, _TDomain, _TAddress},
		      LocalEngineID, 
		      Log, _) ->
	case catch snmp_pdus:enc_pdu(RePdu) of
	    {'EXIT', Reason} ->
		user_err("failed encoding pdu: "
			 "(pdu: ~w, community: ~w): ~n~w",
			 [RePdu, Community, Reason]),
		{discarded, Reason};
	    PduBytes ->
		Message = #message{version = Vsn, 
				   vsn_hdr = Community, 
				   data    = PduBytes},
		case catch list_to_binary(
			     snmp_pdus:enc_message_only(Message)) of
		    {'EXIT', Reason} ->
			user_err("failed encoding message only "
				 "(pdu: ~w, community: ~w): ~n~w",
				 [RePdu, Community, Reason]),
			{discarded, Reason};
		    Packet ->
			MMS = get_engine_max_message_size(LocalEngineID),
			case size(Packet) of
			    Len when Len =< MMS ->
				Log(Type, Packet),
				inc_snmp_cnt_vars(Type, RePdu),
				inc_snmp_out_vars(RePdu),
				{ok, Packet};
			    Len ->
				?vlog("pdu to big:"
				      "~n   Max message size:     ~p"
				      "~n   Encoded message size: ~p",
				      [MMS,Len]),
				too_big(Vsn, RePdu, Community, Log, MMS, Len)
			end
		end
	end;
generate_response_msg(Vsn, RePdu, Type, 
		      {v3, MsgID, MsgSecurityModel, SecName, SecLevel,
		       ContextEngineID, ContextName, SecData},
		      LocalEngineID, 
		      Log, N) ->
    %% rfc2272: 7.1 steps 6-8
    ScopedPDU = #scopedPdu{contextEngineID = ContextEngineID,
			   contextName     = ContextName,
			   data            = RePdu},
    case catch snmp_pdus:enc_scoped_pdu(ScopedPDU) of
	{'EXIT', Reason} ->
	    user_err("failed encoded scoped pdu "
		     "(pdu: ~w, contextName: ~w): ~n~w",
		     [RePdu, ContextName, Reason]),
	    {discarded, Reason};
	ScopedPDUBytes -> 
	    AgentMS = get_engine_max_message_size(LocalEngineID),
	    V3Hdr = #v3_hdr{msgID      = MsgID,
			    msgMaxSize = AgentMS,
			    msgFlags = snmp_misc:mk_msg_flags(Type, SecLevel),
			    msgSecurityModel = MsgSecurityModel},
	    Message = #message{version = Vsn, 
			       vsn_hdr = V3Hdr, 
			       data    = ScopedPDUBytes},
	    %% We know that the security model is valid when we
	    %% generate a response.
	    SecModule = 
		case MsgSecurityModel of
		    ?SEC_USM ->
			snmpa_usm
		end,
	    SecEngineID = LocalEngineID, % 3.1.1a
	    ?vtrace("generate_response_msg -> SecEngineID: ~w", [SecEngineID]),
	    case (catch SecModule:generate_outgoing_msg(Message, 
							SecEngineID,
							SecName, 
							SecData, 
							SecLevel,
							LocalEngineID)) of
		{'EXIT', Reason} ->
		    config_err("~p (message: ~p)", [Reason, Message]),
		    {discarded, Reason};
		{error, Reason} ->
		    config_err("~p (message: ~p)", [Reason, Message]),
		    {discarded, Reason};
		OutMsg when is_list(OutMsg) ->
		    %% Check the packet size.  Send the msg even
		    %% if it's larger than the mgr can handle - it
		    %% will be dropped.  Just check against the
		    %% internal size.  For GET-BULk responses: we
		    %% *know* that we're within the right limits,
		    %% because of the calculation we do when we
		    %% receive the bulk-request.
		    Packet = list_to_binary(OutMsg),
		    case size(Packet) of
			Len when Len =< AgentMS ->
			    if
				SecLevel =:= 3 -> 
				    %% encrypted - log decrypted pdu
				    Log(Type, {V3Hdr, ScopedPDUBytes});
				true -> 
				    %% otherwise log the entire msg
				    Log(Type, Packet)
			    end,
			    inc_snmp_cnt_vars(Type, RePdu),
			    inc_snmp_out_vars(RePdu),
			    {ok, Packet};
			Len when N =:= 2 ->
			    ?vlog("packet max size exceeded: "
				  "~n   Max: ~p"
				  "~n   Len: ~p",
				  [AgentMS,Len]),
			    inc(snmpSilentDrops),
			    {discarded, tooBig};
			Len ->
			    ?vlog("packet max size exceeded: "
				  "~n   N:   ~p"
				  "~n   Max: ~p"
				  "~n   Len: ~p",
				  [N, AgentMS, Len]),
			    TooBigPdu = RePdu#pdu{error_status = tooBig,
						  error_index  = 0, 
						  varbinds     = []},
			    generate_response_msg(Vsn, TooBigPdu, Type, 
						  {v3, MsgID, 
						   MsgSecurityModel,
						   SecName, SecLevel,
						   ContextEngineID, 
						   ContextName,
						   SecData}, 
						  LocalEngineID, Log, N+1)
		    end
	    end
    end.

generate_v3_report_msg(MsgID, MsgSecurityModel, Data, LocalEngineID, 
		       ErrorInfo, Log) ->
    {Varbind, SecName, Opts} = ErrorInfo,
    ReqId =
	if 
	    is_record(Data, scopedPdu) -> 
		(Data#scopedPdu.data)#pdu.request_id;
	   true -> 
		0 %% RFC2572, 7.1.3.c.4
	end,
    ?vtrace("Report ReqId: ~p",[ReqId]),
    Pdu = #pdu{type         = report, 
	       request_id   = ReqId,
	       error_status = noError, 
	       error_index  = 0,
	       varbinds     = [Varbind]},
    SecLevel        = snmp_misc:get_option(securityLevel, Opts, 0),
    SnmpEngineID    = LocalEngineID, 
    ContextEngineID = 
	snmp_misc:get_option(contextEngineID, Opts, SnmpEngineID),
    ContextName     = snmp_misc:get_option(contextName, Opts, ""),
    SecData         = snmp_misc:get_option(sec_data,    Opts, []),

    generate_response_msg('version-3', Pdu, report,
			  {v3, MsgID, MsgSecurityModel, SecName, SecLevel,
			   ContextEngineID, ContextName, SecData}, 
			  LocalEngineID, Log).


%% Response to stage 1 discovery message (terminating, i.e. from the manager)
generate_discovery1_report_msg(MsgID, MsgSecurityModel, 
			       SecName, SecLevel, 
			       ContextEngineID, ContextName,
			       {SecData, Oid, Value}, 
			       #pdu{request_id = ReqId}, 
			       LocalEngineID, Log) ->
    ?vtrace("generate_discovery1_report_msg -> entry with"
	    "~n   ReqId: ~p"
	    "~n   Value: ~p", [ReqId, Value]),
    Varbind = #varbind{oid          = Oid, 
		       variabletype = 'Counter32',
		       value        = Value, 
		       org_index    = 1}, 
    PduOut = #pdu{type         = report, 
		  request_id   = ReqId,
		  error_status = noError, 
		  error_index  = 0,
		  varbinds     = [Varbind]},
    case generate_response_msg('version-3', PduOut, report,
			       {v3, MsgID, MsgSecurityModel, SecName, SecLevel,
				ContextEngineID, ContextName, SecData}, 
			       LocalEngineID, Log) of
	{ok, Packet} ->
	    {discovery, Packet};
	Error ->
	    Error
    end.

%% Response to stage 2 discovery message (terminating, i.e. from the manager)
generate_discovery2_report_msg(MsgID, MsgSecurityModel, 
			       SecName, SecLevel, 
			       ContextEngineID, ContextName,
			       SecData, #pdu{request_id = ReqId}, 
			       LocalEngineID, Log) ->
    ?vtrace("generate_discovery2_report_msg -> entry with"
	    "~n   ReqId: ~p", [ReqId]),
    SecModule = get_security_module(MsgSecurityModel), 
    Vb = SecModule:current_statsNotInTimeWindows_vb(), 
    PduOut = #pdu{type         = report, 
		  request_id   = ReqId,
		  error_status = noError, 
		  error_index  = 0,
		  varbinds     = [Vb]},
    case generate_response_msg('version-3', PduOut, report,
			       {v3, MsgID, MsgSecurityModel, SecName, SecLevel,
				ContextEngineID, ContextName, SecData}, 
			       LocalEngineID, Log) of
	{ok, Packet} ->
	    {discovery, Packet};
	Error ->
	    Error
    end.


too_big(Vsn, Pdu, Community, Log, _MMS, _Len) 
  when Pdu#pdu.type =:= 'get-response' ->
    ErrPdu =
	if 
	    Vsn =:= 'version-1' ->
		%% In v1, the varbinds should be identical to the incoming
		%% request.  It isn't identical now!
		%% Make acceptable (?) approximation.
		V = set_vb_null(Pdu#pdu.varbinds),
		Pdu#pdu{error_status = tooBig, error_index = 0, varbinds = V};
	    true ->
		%% In v2, varbinds should be empty (reasonable!)
		Pdu#pdu{error_status = tooBig, error_index = 0, varbinds = []}
	end,

    case catch snmp_pdus:enc_pdu(ErrPdu) of
	{'EXIT', Reason} ->
	    user_err("failed encoding pdu (pdu: ~w, community: ~w): ~n~w", 
		     [ErrPdu, Community, Reason]),
	    {discarded, Reason};
	PduBytes -> 
	    Message = #message{version = Vsn, vsn_hdr = Community, 
			       data = PduBytes},
	    case catch snmp_pdus:enc_message_only(Message) of
		{'EXIT', Reason} ->
		    user_err("failed encoding message only"
			     "(pdu: ~w, community: ~w): ~n~w", 
			     [ErrPdu, Community, Reason]),
		    {discarded, Reason};
		Packet -> 
		    Bin = list_to_binary(Packet),
		    Log(Pdu#pdu.type, Bin),
		    inc_snmp_out_vars(ErrPdu),
		    {ok, Bin}
	    end
    end;
too_big(_Vsn, Pdu, _Community, _Log, MMS, Len) ->
    user_err("encoded pdu, ~p bytes, exceeded "
	     "max message size of ~p bytes. Pdu: ~n~w", 
	     [Len, MMS, Pdu]),
    {discarded, tooBig}.

set_vb_null([Vb | Vbs]) ->
    [Vb#varbind{variabletype = 'NULL', value = 'NULL'} | set_vb_null(Vbs)];
set_vb_null([]) ->
    [].

%%-----------------------------------------------------------------
%% Executed when a message that isn't a response is generated, i.e.
%% a trap or an inform.
%%-----------------------------------------------------------------
generate_msg(Vsn, NoteStore, Pdu, ACMData, To) ->
    LocalEngineID = ?DEFAULT_LOCAL_ENGINE_ID, 
    generate_msg(Vsn, NoteStore, Pdu, ACMData, LocalEngineID, To).

generate_msg(Vsn, _NoteStore, Pdu, {community, Community}, LocalEngineID, To) ->
    Message = #message{version = Vsn, vsn_hdr = Community, data = Pdu},
    case catch list_to_binary(snmp_pdus:enc_message(Message)) of
	{'EXIT', Reason} ->
	    user_err("failed encoding message "
		     "(pdu: ~w, community: ~w): ~n~w",
		     [Pdu, Community, Reason]),
	    {discarded, Reason};
	Packet ->
	    AgentMax = get_engine_max_message_size(LocalEngineID),
	    case size(Packet) of
		Len when Len =< AgentMax ->
		    {ok, mk_v1_v2_packet_list(To, Packet, Len, Pdu)};
		Len ->
		    ?vlog("packet max size exceeded: "
			  "~n   Max: ~p"
			  "~n   Len: ~p",
			  [AgentMax, Len]),
		    {discarded, tooBig}
	    end
    end;
generate_msg('version-3', NoteStore, Pdu, 
	     {v3, ContextEngineID, ContextName}, LocalEngineID, To) ->
    %% rfc2272: 7.1 step 6
    ScopedPDU = #scopedPdu{contextEngineID = LocalEngineID, 
			   contextName = ContextName,
			   data = Pdu},
    case (catch snmp_pdus:enc_scoped_pdu(ScopedPDU)) of
	{'EXIT', Reason} ->
	    user_err("failed encoding scoped pdu "
		     "(pdu: ~w, contextName: ~w): ~n~w",
		     [Pdu, ContextName, Reason]),
	    {discarded, Reason};
	ScopedPDUBytes -> 
	    {ok, mk_v3_packet_list(NoteStore, To, ScopedPDUBytes, Pdu, 
				   ContextEngineID, ContextName, 
				   LocalEngineID)}
    end.


generate_discovery_msg(NoteStore, Pdu, MsgData, To) ->
    Timeout = 1500, 
    generate_discovery_msg(NoteStore, Pdu, MsgData, Timeout, To).

generate_discovery_msg(NoteStore, Pdu, MsgData, Timeout, To) ->
    {SecData, ContextEngineID, ContextName}       = MsgData,
    {SecModel, SecName, SecLevelFlag, TargetName} = SecData,
    {ManagerEngineId, InitialUserName} = 
	case get_target_engine_id(TargetName) of
	    {ok, discovery} ->  
		{"", ""};             % Discovery stage 1
	    {ok, {discovery, IUN}} ->  
		{"", IUN};            % Discovery stage 1
	    {ok, TargetEngineId} ->
		{TargetEngineId, ""}  % Discovery stage 2
	end,
    generate_discovery_msg(NoteStore, Pdu, 
			   ContextEngineID, ContextName, 
			   SecModel, SecName, SecLevelFlag, 
			   ManagerEngineId, 
			   InitialUserName, 
			   Timeout, To).

generate_discovery_msg(NoteStore, Pdu, 
		       ContextEngineID, ContextName, 
		       SecModel, _SecName, _SecLevelFlag, 
		       "" = ManagerEngineID, 
		       InitialUserName,
		       Timeout, To) ->
    %% Discovery step 1 uses SecLevel = noAuthNoPriv
    SecName      = "", 
    SecLevelFlag = 0, % ?'SnmpSecurityLevel_noAuthNoPriv', 
    generate_discovery_msg2(NoteStore, Pdu, 
			    ContextEngineID, ManagerEngineID, 
			    SecModel, SecName, SecLevelFlag,
			    InitialUserName, 
			    ContextName, Timeout, To);
generate_discovery_msg(NoteStore, Pdu, 
		       ContextEngineID, ContextName, 
		       SecModel, SecName, SecLevelFlag, 
		       ManagerEngineID, 
		       InitialUserName,
		       Timeout, To) ->
    %% SecLevelFlag = 1, % ?'SnmpSecurityLevel_authNoPriv', 
    generate_discovery_msg2(NoteStore, Pdu, 
			    ContextEngineID, ManagerEngineID, 
			    SecModel, SecName, SecLevelFlag,
			    InitialUserName, 
			    ContextName, Timeout, To).

generate_discovery_msg2(NoteStore, Pdu, 
			ContextEngineID, ManagerEngineID, 
			SecModel, SecName, SecLevelFlag,
			InitialUserName, 
			ContextName, Timeout, To) ->
    %% rfc2272: 7.1.6
    ScopedPDU = #scopedPdu{contextEngineID = ContextEngineID,
			   contextName     = ContextName,
			   data            = Pdu},
    case (catch snmp_pdus:enc_scoped_pdu(ScopedPDU)) of
	{'EXIT', Reason} ->
	    user_err("failed encoding scoped pdu "
		     "(pdu: ~w, contextName: ~w): ~n~w",
		     [Pdu, ContextName, Reason]),
	    {discarded, Reason};
	ScopedPDUBytes -> 
	    {ok, generate_discovery_msg(NoteStore, To, 
					Pdu, ScopedPDUBytes, 
					ContextEngineID, ManagerEngineID,
					SecModel, SecName, SecLevelFlag,
					InitialUserName, 
					ContextName, Timeout)}
    end.

%% Timeout is in msec but note timeout is in 1/10 seconds
discovery_note_timeout(Timeout) ->
    (Timeout div 100) + 1.
    
generate_discovery_msg(NoteStore, {TDomain, TAddress}, 
		       Pdu, ScopedPduBytes, 
		       ContextEngineID, ManagerEngineID, 
		       SecModel, SecName, SecLevelFlag, 
		       InitialUserName, 
		       ContextName, Timeout) ->

    {ok, {Domain, Address}} = transform_taddr(TDomain, TAddress),

    %% 7.1.7
    ?vdebug("generate_discovery_msg -> 7.1.7 (~w)", [ManagerEngineID]),
    MsgID     = generate_msg_id(),
    PduType   = Pdu#pdu.type,
    MsgFlags  = mk_msg_flags(PduType, SecLevelFlag), 
    V3Hdr     = #v3_hdr{msgID            = MsgID,
			msgMaxSize       = get_max_message_size(),
			msgFlags         = MsgFlags,
			msgSecurityModel = SecModel},
    Message   = #message{version = 'version-3', 
			 vsn_hdr = V3Hdr,
			 data    = ScopedPduBytes},
    SecModule = sec_module(SecModel),

    %% 7.1.9b
    ?vdebug("generate_discovery_msg -> 7.1.9b", []),
    case generate_sec_discovery_msg(Message, SecModule, 
				    ManagerEngineID, 
				    SecName, SecLevelFlag,
				    InitialUserName) of
	{ok, Packet} ->
	    %% 7.1.9c
	    %% Store in cache for Timeout msec.
	    NoteTimeout = discovery_note_timeout(Timeout),
	    ?vdebug("generate_discovery_msg -> 7.1.9c [~w]", [NoteTimeout]),
	    %% The request id is just in case when we receive a 
	    %% report with incorrect securityModel and/or securityLevel
	    Key  = {agent, MsgID}, 
	    Note = #note{sec_engine_id = ManagerEngineID, 
			 sec_model     = SecModel, 
			 sec_name      = SecName, 
			 sec_level     = SecLevelFlag,
			 ctx_engine_id = ContextEngineID, 
			 ctx_name      = ContextName, 
			 disco         = true, 
			 req_id        = Pdu#pdu.request_id},
	    snmp_note_store:set_note(NoteStore, Timeout, Key, Note),
	    %% Log(Packet),
	    inc_snmp_out_vars(Pdu),
	    ?vdebug("generate_discovery_msg -> done", []),
	    {Domain, Address, Packet};

	Error ->
	    throw(Error)
    end.

generate_sec_discovery_msg(Message, SecModule, 
			   SecEngineID, SecName, SecLevelFlag, 
			   InitialUserName) ->
    case (catch SecModule:generate_discovery_msg(Message, SecEngineID, 
						 SecName, SecLevelFlag,
						 InitialUserName)) of
	{'EXIT', Reason} ->
	    config_err("~p (message: ~p)", [Reason, Message]),
	    {discarded, Reason};
	{error, Reason} ->
	    config_err("~p (message: ~p)", [Reason, Message]),
	    {discarded, Reason};
	Bin when is_binary(Bin) ->
	    {ok, Bin};
	OutMsg when is_list(OutMsg) ->
	    case (catch list_to_binary(OutMsg)) of
		Bin when is_binary(Bin) ->
		    {ok, Bin};
		{'EXIT', Reason} ->
		    {error, Reason}
	    end
    end.
	
    
transform_taddr(?snmpUDPDomain, TAddress) ->
    transform_taddr(?transportDomainUdpIpv4, TAddress);
transform_taddr(?transportDomainUdpIpv4, [A, B, C, D, P1, P2]) ->
    Domain  = transportDomainUdpIpv4, 
    Addr    = {A,B,C,D}, 
    Port    = P1 bsl 8 + P2, 
    Address = {Addr, Port},
    {ok, {Domain, Address}};
transform_taddr(?transportDomainUdpIpv4, BadAddr) ->
    {error, {bad_transportDomainUdpIpv4_address, BadAddr}};
transform_taddr(
  ?transportDomainUdpIpv6,
  [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16,
   P1, P2]) ->
    Domain = transportDomainUdpIpv6,
    Addr =
	{(A1 bsl 8) bor A2, (A3 bsl 8) bor A4,
	 (A5 bsl 8) bor A6, (A7 bsl 8) bor A8,
	 (A9 bsl 8) bor A10, (A11 bsl 8) bor A12,
	 (A13 bsl 8) bor A14, (A15 bsl 8) bor A16},
    Port = P1 bsl 8 + P2,
    Address = {Addr, Port},
    {ok, {Domain, Address}};
transform_taddr(
  ?transportDomainUdpIpv6,
  [A1, A2, A3, A4, A5, A6, A7, A8, P1, P2]) ->
    Domain  = transportDomainUdpIpv6, 
    Addr    = {A1, A2, A3, A4, A5, A6, A7, A8},
    Port    = P1 bsl 8 + P2,
    Address = {Addr, Port},
    {ok, {Domain, Address}};
transform_taddr(?transportDomainUdpIpv6, BadAddr) ->
    {error, {bad_transportDomainUdpIpv6_address, BadAddr}};
transform_taddr(BadTDomain, TAddress) ->
    case lists:member(BadTDomain, snmp_conf:all_tdomains()) of
	true ->
	    {error, {unsupported_tdomain, BadTDomain, TAddress}};
	false ->
	    {error, {unknown_tdomain, BadTDomain, TAddress}}
    end.


process_taddrs(Dests) ->
    ?vtrace("process_taddrs -> entry with"
	    "~n   Dests: ~p", [Dests]),
    process_taddrs(Dests, []).

process_taddrs([], Acc) ->
    ?vtrace("process_taddrs -> entry when done with"
	    "~n   Acc: ~p", [Acc]),
    lists:reverse(Acc);
    
%% v3
process_taddrs([{{TDomain, TAddress}, SecData} | T], Acc) ->
    ?vtrace("process_taddrs -> entry when v3 with"
	    "~n   TDomain:  ~p"
	    "~n   TAddress: ~p"
	    "~n   SecData:  ~p", [TDomain, TAddress, SecData]),
    case transform_taddr(TDomain, TAddress) of
	{ok, DestAddr} ->
	    ?vtrace("process_taddrs -> transformed: "
		    "~n   DestAddr: ~p", [DestAddr]),
	    Entry = {DestAddr, SecData}, 
	    process_taddrs(T, [Entry | Acc]);
	{error, Reason} ->
	    ?vinfo("Failed transforming v3 domain and address"
		   "~n   Reason:  ~p", [Reason]),
	    user_err("Bad TDomain/TAddress: ~w/~w", [TDomain, TAddress]),
	    process_taddrs(T, Acc)
    end;
%% v1 & v2
process_taddrs([{TDomain, TAddress} | T], Acc) ->
    ?vtrace("process_taddrs -> entry when v1/v2 with"
	    "~n   TDomain:  ~p"
	    "~n   TAddress: ~p", [TDomain, TAddress]),
    case transform_taddr(TDomain, TAddress) of
	{ok, DestAddr} ->
	    ?vtrace("process_taddrs -> transformed: "
		    "~n   DestAddr: ~p", [DestAddr]),
	    Entry = DestAddr, 
	    process_taddrs(T, [Entry | Acc]);
	{error, Reason} ->
	    ?vinfo("Failed transforming v1/v2 domain and address: "
		   "~n   Reason:  ~p", [Reason]),
	    user_err("Bad TDomain/TAddress: ~w/~w", [TDomain, TAddress]),
	    process_taddrs(T, Acc)
    end;
process_taddrs(Crap, Acc) ->
    throw({error, {bad_taddrs, Crap, Acc}}).


mk_v1_v2_packet_list(To, Packet, Len, Pdu) ->
    mk_v1_v2_packet_list(To, Packet, Len, Pdu, []).

mk_v1_v2_packet_list([], _Packet, _Len, _Pdu, Acc) ->
    lists:reverse(Acc);

%% This (old) clause is for backward compatibillity reasons
%% If this is called, then the filter function is not used
mk_v1_v2_packet_list([{?snmpUDPDomain, [A,B,C,D,U1,U2]} | T],
		     Packet, Len, Pdu, Acc) ->
    %% Sending from default UDP port
    inc_snmp_out_vars(Pdu),
    Entry = {snmpUDPDomain, {{A,B,C,D}, U1 bsl 8 + U2}, Packet},
    mk_v1_v2_packet_list(T, Packet, Len, Pdu, [Entry | Acc]);

%% This is the new clause
%% This is only called if the actual target was accepted
%% (by the filter module)
mk_v1_v2_packet_list([{Domain, Addr} | T], 
		     Packet, Len, Pdu, Acc) ->
    %% Sending from default UDP port
    inc_snmp_out_vars(Pdu),
    Entry = {Domain, Addr, Packet},
    %% It would be cleaner to return {To, Packet} to not
    %% break the abstraction for an address on the
    %% {Domain, Address} format.
    mk_v1_v2_packet_list(T, Packet, Len, Pdu, [Entry | Acc]).


get_max_message_size() ->
    snmp_framework_mib:get_engine_max_message_size().

mk_msg_flags(PduType, SecLevel) ->
    snmp_misc:mk_msg_flags(PduType, SecLevel).

mk_v3_packet_entry(NoteStore, Domain, Addr, 
		   {SecModel, SecName, SecLevel, TargetAddrName},
		   ScopedPDUBytes, Pdu, _ContextEngineID, ContextName,
		   LocalEngineID) ->
    %% rfc2272 7.1 step 77
    ?vtrace("mk_v3_packet_entry -> entry - RFC2272-7.1:7", []),
    MsgVersion  = 'version-3',                     % 7.1:7a
    MsgID       = generate_msg_id(),               % 7.1:7b
    MaxMsgSz    = get_max_message_size(),          % 7.1:7c
    PduType     = Pdu#pdu.type,      
    MsgFlags    = mk_msg_flags(PduType, SecLevel), % 7.1:7d
    MsgSecModel = SecModel,                        % 7.1:7e
    V3Hdr     = #v3_hdr{msgID            = MsgID,
			msgMaxSize       = MaxMsgSz, 
			msgFlags         = MsgFlags, 
			msgSecurityModel = MsgSecModel},
    Message   = #message{version = MsgVersion, 
			 vsn_hdr = V3Hdr,
			 data    = ScopedPDUBytes},
    SecModule = 
	case SecModel of
	    ?SEC_USM ->
		snmpa_usm
	end,

    %% 
    %% 7.1:8 - If the PDU is from the Response Class or the Internal Class
    %%         securityEngineID = snmpEngineID (local/source)
    %% 7.1:9 - If the PDU is from the Unconfirmed Class
    %%         securityEngineID = snmpEngineID (local/source)
    %%         else
    %%         securityEngineID = targetEngineID (remote/destination)
    %% 

    %% 7.1.9a
    ?vtrace("mk_v3_packet_entry -> sec engine id - 7.1.9a", []),
    SecEngineID =
	case PduType of
	    'snmpv2-trap' ->
		LocalEngineID; 
	    _ ->
		%% This is the implementation dependent target engine id
		%% procedure.
		case get_target_engine_id(TargetAddrName) of
		    {ok, discovery} ->  
			config_err("Discovery has not yet been performed for "
				   "snmpTargetAddrName ~p~n", 
				   [TargetAddrName]),
			throw({discarded, {discovery, TargetAddrName}});
		    {ok, TargetEngineId} ->
			?vtrace("TargetEngineId: ~p", [TargetEngineId]),
			TargetEngineId;
		    undefined ->
			config_err("Can't find engineID for "
				   "snmpTargetAddrName ~p~n",
				   [TargetAddrName]),
			"" % this will trigger error in secmodule
		end
	end,

    ?vdebug("mk_v3_packet_entry -> secEngineID: ~p", [SecEngineID]),
    %% 7.1.9b
    case (catch SecModule:generate_outgoing_msg(Message, SecEngineID,
						SecName, [], SecLevel,
						LocalEngineID)) of
	{'EXIT', Reason} ->
	    config_err("~p (message: ~p)", [Reason, Message]),
	    skip;
	{error, Reason} ->
	    ?vlog("~n   ~w error ~p\n", [SecModule, Reason]),
	    skip;
	OutMsg when is_list(OutMsg) ->
	    %% 7.1.9c
	    %% Store in cache for 150 sec.
	    Packet = list_to_binary(OutMsg),
	    ?vdebug("mk_v3_packet_entry -> generated: ~w bytes", 
		    [size(Packet)]),
	    Data = 
		if
		    SecLevel =:= 3 -> 
			%% encrypted - log decrypted pdu
			{Packet, {V3Hdr, ScopedPDUBytes}};
		    true -> 
			%% otherwise log the entire msg
			Packet
		end,
	    CacheKey = {agent, MsgID}, 
	    CacheVal = #note{sec_engine_id = SecEngineID, 
			     sec_model     = SecModel, 
			     sec_name      = SecName, 
			     sec_level     = SecLevel, 
			     ctx_engine_id = LocalEngineID, 
			     ctx_name      = ContextName,
			     disco         = false,
			     req_id        = Pdu#pdu.request_id},
	    snmp_note_store:set_note(NoteStore, 1500, CacheKey, CacheVal),
	    inc_snmp_out_vars(Pdu),
	    %% It would be cleaner to return {To, Packet} to not
	    %% break the abstraction for an address on the
	    %% {Domain, Address} format.
	    {ok, {Domain, Addr, Data}}
    end.


mk_v3_packet_list(NoteStore, To, 
		  ScopedPDUBytes, Pdu, ContextEngineID, ContextName, 
		  LocalEngineID) ->
    mk_v3_packet_list(NoteStore, To, 
		      ScopedPDUBytes, Pdu, 
		      ContextEngineID, ContextName, LocalEngineID, []).

mk_v3_packet_list(_, [], 
		  _ScopedPDUBytes, _Pdu, 
		  _ContextEngineID, _ContextName, 
		  _LocalEngineID, Acc) ->
    lists:reverse(Acc);

%% This clause is for backward compatibillity reasons
%% If this is called the filter function is not used
mk_v3_packet_list(NoteStore, 
		  [{{?snmpUDPDomain, [A,B,C,D,U1,U2]}, SecData} | T], 
		  ScopedPDUBytes, Pdu, ContextEngineID, ContextName, 
		  LocalEngineID, Acc) ->
    case mk_v3_packet_entry(NoteStore, 
			    snmpUDPDomain, {{A,B,C,D}, U1 bsl 8 + U2}, SecData,
			    ScopedPDUBytes, Pdu, 
			    ContextEngineID, ContextName, LocalEngineID) of
	skip ->
	    mk_v3_packet_list(NoteStore, T, 
			      ScopedPDUBytes, Pdu, 
			      ContextEngineID, ContextName, LocalEngineID, 
			      Acc);
	{ok, Entry} ->
	    mk_v3_packet_list(NoteStore, T, 
			      ScopedPDUBytes, Pdu, 
			      ContextEngineID, ContextName, LocalEngineID, 
			      [Entry | Acc])
    end;

%% This is the new clause
%% This is only called if the actual target was accepted
%% (by the filter module)
mk_v3_packet_list(NoteStore, 
		  [{{Domain, Addr}, SecData} | T], 
		  ScopedPDUBytes, Pdu, ContextEngineID, ContextName,
		  LocalEngineID, Acc) ->
    case mk_v3_packet_entry(NoteStore, 
			    Domain, Addr, SecData,
			    ScopedPDUBytes, Pdu, 
			    ContextEngineID, ContextName, LocalEngineID) of
	skip ->
	    mk_v3_packet_list(NoteStore, T, 
			      ScopedPDUBytes, Pdu, 
			      ContextEngineID, ContextName, Acc);
	{ok, Entry} ->
	    mk_v3_packet_list(NoteStore, T, 
			      ScopedPDUBytes, Pdu, 
			      ContextEngineID, ContextName, 
			      LocalEngineID, [Entry | Acc])
    end.


generate_msg_id() ->
    gen(msg_id).

generate_req_id() ->
    gen(req_id).

gen(Id) ->
    case ets:update_counter(snmp_agent_table, Id, 1) of
	N when N =< 2147483647 ->
	    N;
	_N ->
	    ets:insert(snmp_agent_table, {Id, 0}),
	    0
    end.


get_target_engine_id(TargetAddrName) ->
    snmp_target_mib:get_target_engine_id(TargetAddrName).

get_engine_max_message_size(_LocalEngineID) ->
    snmp_framework_mib:get_engine_max_message_size().

sec_module(?SEC_USM) ->
    snmpa_usm.


%%-----------------------------------------------------------------
%% Version(s) functions
%%-----------------------------------------------------------------
init_versions([], S) ->
    S;
init_versions([v1|Vsns], S) ->
    init_versions(Vsns, S#state{v1 = true});
init_versions([v2|Vsns], S) ->
    init_versions(Vsns, S#state{v2c = true});
init_versions([v3|Vsns], S) ->
    init_versions(Vsns, S#state{v3 = true}).


%%-----------------------------------------------------------------
%% Counter functions
%%-----------------------------------------------------------------
init_counters() -> 
    F = fun(Counter) -> maybe_create_counter(Counter) end,
    lists:map(F, counters()).

reset_counters() -> 
    F = fun(Counter) -> init_counter(Counter) end,
    lists:map(F, counters()).

maybe_create_counter(Counter) ->
    case ets:lookup(snmp_agent_table, Counter) of
	[_] -> ok;
	_ -> init_counter(Counter)
    end.

init_counter(Counter) -> 
    ets:insert(snmp_agent_table, {Counter, 0}).

counters() ->
    [
     snmpInPkts,
     snmpOutPkts,
     snmpInBadVersions,
     snmpInBadCommunityNames,
     snmpInBadCommunityUses,
     snmpInASNParseErrs,
     snmpInTooBigs,
     snmpInNoSuchNames,
     snmpInBadValues,
     snmpInReadOnlys,
     snmpInGenErrs,
     snmpInTotalReqVars,
     snmpInTotalSetVars,
     snmpInGetRequests,
     snmpInGetNexts,
     snmpInSetRequests,
     snmpInGetResponses,
     snmpInTraps,
     snmpOutTooBigs,
     snmpOutNoSuchNames,
     snmpOutBadValues,
     snmpOutGenErrs,
     snmpOutGetRequests,
     snmpOutGetNexts,
     snmpOutSetRequests,
     snmpOutGetResponses,
     snmpOutTraps,
     snmpSilentDrops,
     snmpProxyDrops,
     %% From SNMP-MPD-MIB
     snmpUnknownSecurityModels,
     snmpInvalidMsgs,
     snmpUnknownPDUHandlers
    ].
    


%%-----------------------------------------------------------------
%%  inc(VariableName) increments the variable (Counter) in
%%  the local mib. (e.g. snmpInPkts)
%%-----------------------------------------------------------------
inc(Name)    -> ets:update_counter(snmp_agent_table, Name, 1).
inc(Name, N) -> ets:update_counter(snmp_agent_table, Name, N).

inc_snmp_in_vars(#pdu{type = Type}) ->
    inc_in_type(Type).

inc_snmp_cnt_vars(_, #pdu{error_status = ErrStat}) when ErrStat =/= noError ->
    ok;
inc_snmp_cnt_vars('get-request', #pdu{varbinds = Vbs}) ->
    inc(snmpInTotalReqVars, length(Vbs));
inc_snmp_cnt_vars('get-next-request', #pdu{varbinds = Vbs}) ->
    inc(snmpInTotalReqVars, length(Vbs));
inc_snmp_cnt_vars('set-request', #pdu{varbinds = Vbs}) ->
    inc(snmpInTotalSetVars, length(Vbs));
inc_snmp_cnt_vars(_, _) ->
    ok.

inc_snmp_out_vars(#pdu{type         = Type, 
		       error_status = ErrorStatus}) ->
    inc(snmpOutPkts),
    inc_out_err(ErrorStatus),
    inc_out_vars_2(Type);
inc_snmp_out_vars(TrapPdu) when is_record(TrapPdu, trappdu) ->
    inc(snmpOutPkts),
    inc(snmpOutTraps).

inc_out_vars_2('get-response')     -> inc(snmpOutGetResponses);
inc_out_vars_2('get-request')      -> inc(snmpOutGetRequests);
inc_out_vars_2('get-next-request') -> inc(snmpOutGetNexts);
inc_out_vars_2('set-request')      -> inc(snmpOutSetRequests);
inc_out_vars_2(_)                  -> ok.

inc_out_err(genErr)     -> inc(snmpOutGenErrs);
inc_out_err(tooBig)     -> inc(snmpOutTooBigs);
inc_out_err(noSuchName) -> inc(snmpOutNoSuchNames);
inc_out_err(badValue)   -> inc(snmpOutBadValues);
% snmpOutReadOnlys is not used any more (rfc1213)
%inc_out_err(readOnly) -> inc(snmpOutReadOnlys);
inc_out_err(_)          -> ok.

inc_in_type('get-request')      -> inc(snmpInGetRequests);
inc_in_type('get-next-request') -> inc(snmpInGetNexts);
inc_in_type('set-request')      -> inc(snmpInSetRequests);
inc_in_type(_)                  -> ok.


user_err(F, A) ->
    snmpa_error:user_err(F, A).

config_err(F, A) ->
    snmpa_error:config_err(F, A).
