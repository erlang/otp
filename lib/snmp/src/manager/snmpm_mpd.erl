%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2019. All Rights Reserved.
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

-module(snmpm_mpd).

-export([init/1, 

	 process_msg/7, process_msg/6,
	 generate_msg/5, generate_response_msg/4,

	 next_msg_id/0, 
	 next_req_id/0, 

	 reset/1, 
	 inc/1]).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-include("snmpm_internal.hrl").
-include("SNMP-MPD-MIB.hrl").
-include("SNMPv2-TM.hrl").

-define(VMODULE,"MPD").
-include("snmp_verbosity.hrl").

-define(empty_msg_size, 24).

-record(state, {v1 = false, v2c = false, v3 = false}).

					
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
    ?vdebug("init -> entry with ~p", [Vsns]),
    ?SNMP_RAND_SEED(),
    %% rand:seed(exrop,
    %%           {erlang:phash2([node()]),
    %%            erlang:monotonic_time(),
    %%            erlang:unique_integer()}),
    snmpm_config:cre_counter(msg_id, rand:uniform(2147483647)),
    snmpm_config:cre_counter(req_id, rand:uniform(2147483647)),
    init_counters(),
    State = init_versions(Vsns, #state{}),
    init_usm(State#state.v3),
    ?vtrace("init -> done when ~p", [State]),
    State.

reset(#state{v3 = V3}) ->
    reset_counters(),
    reset_usm(V3).


%%-----------------------------------------------------------------
%% Func: process_msg(Packet, TDomain, TAddress, State) ->
%%       {ok, SnmpVsn, Pdu, PduMS, ACMData} | {discarded, Reason}
%% Types: Packet = binary()
%%        TDomain = snmpUDPDomain | atom()
%%        TAddress = {Ip, Udp}
%%        State = #state
%% Purpose: This is the main Message Dispatching function. (see
%%          section 4.2.1 in rfc2272)
%%-----------------------------------------------------------------
process_msg(Msg, Domain, Ip, Port, State, NoteStore, Logger) ->
    process_msg(Msg, Domain, {Ip, Port}, State, NoteStore, Logger).

process_msg(Msg, Domain, Addr, State, NoteStore, Logger) ->
    inc(snmpInPkts),

    case (catch snmp_pdus:dec_message_only(binary_to_list(Msg))) of

	%% Version 1
	#message{version = 'version-1', vsn_hdr = Community, data = Data} 
	  when State#state.v1 =:= true ->
	    HS = ?empty_msg_size + length(Community),
	    process_v1_v2c_msg(
	      'version-1', NoteStore, Msg, Domain, Addr,
	      Community, Data, HS, Logger);

	%% Version 2
	#message{version = 'version-2', vsn_hdr = Community, data = Data}
	  when State#state.v2c =:= true ->
	    HS = ?empty_msg_size + length(Community),
	    process_v1_v2c_msg(
	      'version-2', NoteStore, Msg, Domain, Addr,
	      Community, Data, HS, Logger);
	     
	%% Version 3
	#message{version = 'version-3', vsn_hdr = H, data = Data}
	  when State#state.v3 =:= true ->
	    ?vlog("v3:"
		"~n   msgID:       ~p"
		"~n   msgFlags:    ~p"
		"~n   msgSecModel: ~p",
		[H#v3_hdr.msgID,H#v3_hdr.msgFlags,H#v3_hdr.msgSecurityModel]),
	    process_v3_msg(NoteStore, Msg, H, Data, Addr, Logger);

	%% Crap
	{'EXIT', {bad_version, Vsn}} ->
	    ?vinfo("exit: bad version: ~p",[Vsn]),
	    inc(snmpInBadVersions),
	    {discarded, snmpInBadVersions};

	%% More crap
	{'EXIT', Reason} ->
	    ?vinfo("exit: ~p",[Reason]),
	    inc(snmpInASNParseErrs),
	    {discarded, Reason};

	%% Really crap
	Crap ->
	    ?vinfo("unknown message: "
		   "~n   ~p",[Crap]),
	    inc(snmpInBadVersions),
	    {discarded, snmpInBadVersions}
    end.


%%-----------------------------------------------------------------
%% Handles a Community based message (v1 or v2c).
%%-----------------------------------------------------------------
process_v1_v2c_msg(
  Vsn, _NoteStore, Msg, Domain, Addr, Community, Data, HS, Log) ->

    ?vdebug("process_v1_v2c_msg -> entry with"
	    "~n   Vsn:       ~p"
	    "~n   Domain:    ~p"
	    "~n   Addr:      ~p"
	    "~n   Community: ~p"
	    "~n   HS:        ~p", [Vsn, Domain, Addr, Community, HS]),

    {TDomain, TAddress} = 
	try 
	    {snmp_conf:mk_tdomain(Domain),
	     snmp_conf:mk_taddress(Domain, Addr)}
	catch 
	    throw:{error, TReason} ->
		throw({discarded, {badarg, Domain, TReason}})
	end,

    Max      = get_max_message_size(),
    AgentMax = get_agent_max_message_size(Domain, Addr),
    PduMS    = pdu_ms(Max, AgentMax, HS),

    ?vtrace("process_v1_v2c_msg -> PduMS: ~p", [PduMS]),
    
    case (catch snmp_pdus:dec_pdu(Data)) of
	Pdu when is_record(Pdu, pdu) ->
	    ?vtrace("process_v1_v2c_msg -> was a pdu", []),
	    Log(Msg),
	    inc_snmp_in(Pdu),
	    MsgData = {Community, sec_model(Vsn), TDomain, TAddress},
	    {ok, Vsn, Pdu, PduMS, MsgData};

	Trap when is_record(Trap, trappdu) ->
	    ?vtrace("process_v1_v2c_msg -> was a trap", []),
	    Log(Msg),
	    inc_snmp_in(Trap),
	    MsgData = {Community, sec_model(Vsn), TDomain, TAddress},
	    {ok, Vsn, Trap, PduMS, MsgData};

	{'EXIT', Reason} ->
	    ?vlog("process_v1_v2c_msg -> failed decoding PDU: "
		  "~n   Reason: ~p", [Reason]),
	    inc(snmpInASNParseErrs),
	    {discarded, Reason}
    end.

pdu_ms(MgrMMS, AgentMMS, HS) when AgentMMS < MgrMMS ->
    AgentMMS - HS;
pdu_ms(MgrMMS, _AgentMMS, HS) ->
    MgrMMS - HS.

sec_model('version-1') -> ?SEC_V1;
sec_model('version-2') -> ?SEC_V2C.


%%-----------------------------------------------------------------
%% Handles a SNMPv3 Message, following the procedures in rfc2272,
%% section 4.2 and 7.2
%%-----------------------------------------------------------------
process_v3_msg(NoteStore, Msg, Hdr, Data, Address, Log) ->
    ?vdebug(
       "process_v3_msg -> entry with~n"
       "   Hdr:     ~p~n"
       "   Address: ~p", [Hdr, Address]),

    %% 7.2.3
    #v3_hdr{msgID                 = MsgID, 
	    msgMaxSize            = MMS, 
	    msgFlags              = MsgFlags,
	    msgSecurityModel      = MsgSecModel,
	    msgSecurityParameters = SecParams, 
	    hdr_size              = HdrSize} = Hdr,

    %% 7.2.4
    SecModule    = get_security_module(MsgSecModel),
    ?vtrace("process_v3_msg -> 7.2.4: "
	    "~n   SecModule: ~p", [SecModule]),

    %% 7.2.5
    SecLevel     = check_sec_level(MsgFlags),
    IsReportable = is_reportable(MsgFlags),
    ?vtrace("process_v3_msg -> 7.2.5: "
	    "~n   SecLevel:     ~p"
	    "~n   IsReportable: ~p", [SecLevel, IsReportable]),

    %% 7.2.6
    SecRes = (catch SecModule:process_incoming_msg(Msg, Data,
						   SecParams, SecLevel)),
    ?vtrace("process_v3_msg -> 7.2.6 - message processing result: "
	    "~n   ~p",[SecRes]),
    {SecEngineID, SecName, ScopedPDUBytes, SecData} =
	check_sec_module_result(SecRes, Hdr, Data, IsReportable, Log),
    ?vtrace("process_v3_msg -> 7.2.6 - checked module result: "
	    "~n   SecEngineID: ~p"
	    "~n   SecName:     ~p",[SecEngineID, SecName]),

    %% 7.2.7
    #scopedPdu{contextEngineID = CtxEngineID,
	       contextName     = CtxName,
	       data            = PDU} =
	case (catch snmp_pdus:dec_scoped_pdu(ScopedPDUBytes)) of
	    ScopedPDU when is_record(ScopedPDU, scopedPdu) -> 
		ScopedPDU;
	    {'EXIT', Reason} ->
		?vlog("failed decoding scoped pdu: "
		      "~n   ~p",[Reason]),
		inc(snmpInASNParseErrs),
		discard(Reason)
	end,

    ?vlog("7.2.7"
	  "~n   ContextEngineID: ~p "
	  "~n   context:         \"~s\" ",
	  [CtxEngineID, CtxName]),
    if
	SecLevel =:= 3 -> % encrypted message - log decrypted pdu
	    Log({Hdr, ScopedPDUBytes});
	true -> % otherwise, log binary
	    Log(Msg)
    end,

    %% Make sure a get_bulk doesn't get too big.
    MgrMMS = get_max_message_size(),
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
    TotMMS = tot_mms(MgrMMS, MMS),
    TotalLenOctets = snmp_pdus:get_encoded_length(TotMMS - 1),
    PduMMS = TotMMS - TotalLenOctets - 10 - HdrSize - 
	length(CtxName) - length(CtxEngineID),
    ?vtrace("process_v3_msg -> PduMMS = ~p", [PduMMS]),
    Type = PDU#pdu.type,
    ?vdebug("process_v3_msg -> PDU type: ~p",[Type]),
    case Type of
	report ->
	    %% 7.2.10 & 11
	    %% BMK BMK BMK: discovery?
	    Note = snmp_note_store:get_note(NoteStore, MsgID), 
	    case Note of
		{SecEngineID, MsgSecModel, SecName, SecLevel,
		 CtxEngineID, CtxName, _ReqId} ->
		    ?vtrace("process_v3_msg -> 7.2.11b: ok", []),
		    %% BMK BMK: Should we discard the cached info
		    %% BMK BMK: or do we let the gc deal with it?
 		    {ok, 'version-3', PDU, PduMMS, ok};
 		_ when is_tuple(Note) ->
 		    ?vlog("process_v3_msg -> 7.2.11b: error"
 			  "~n   Note: ~p", [Note]),
		    Recv  = {SecEngineID, MsgSecModel, SecName, SecLevel,
			     CtxEngineID, CtxName, PDU#pdu.request_id}, 
		    Err   = sec_error(Note, Recv), 
 		    ACM   = {invalid_sec_info, Err}, 
		    ReqId = element(size(Note), Note), 
 		    {ok, 'version-3', PDU, PduMMS, {error, ReqId, ACM}};
		_NoFound ->
		    ?vtrace("process_v3_msg -> _NoFound: "
			    "~p", [_NoFound]),
		    inc(snmpUnknownPDUHandlers),
		    discard({no_outstanding_req, MsgID})
	    end;

	'get-response' -> 
	    %% 7.2.10 & 12 
	    case snmp_note_store:get_note(NoteStore, MsgID) of
		{SecEngineID, MsgSecModel, SecName, SecLevel,
		 CtxEngineID, CtxName, _} ->
		    %% 7.2.12.d
		    {ok, 'version-3', PDU, PduMMS, undefined};
		_ ->
		    %% 7.2.12.b
		    %% BMK BMK: Should we not discard the cached info??
		    inc(snmpUnknownPDUHandlers),
		    discard({no_outstanding_req, MsgID})
	    end;

	'snmpv2-trap' ->
	    %% 7.2.14
	    {ok, 'version-3', PDU, PduMMS, undefined};

	'inform-request' ->
	    %% 7.2.13 
	    SnmpEngineID = get_engine_id(),
	    case SecEngineID of
		SnmpEngineID -> % 7.2.13.b
		    ?vtrace("7.2.13d - valid securityEngineID: ~p", 
			    [SecEngineID]),
		    %% 4.2.2.1.1 - we don't handle proxys yet => we only 
		    %% handle CtxEngineID to ourselves
		    %% Check that we actually know of an agent with this
		    %% CtxEngineID and Address
		    case is_known_engine_id(CtxEngineID, Address) of
			true ->
			    ?vtrace("and the agent EngineID (~p) "
				    "is know to us", [CtxEngineID]),
			    %% Uses ACMData that snmpm_acm knows of.
			    %% BUGBUG BUGBUG
			    ACMData = 
				{MsgID, MsgSecModel, SecName, SecLevel,
				 CtxEngineID, CtxName, SecData},
			    {ok, 'version-3', PDU, PduMMS, ACMData};
			UnknownEngineID ->
			    ?vtrace("4.2.2.1.2 - UnknownEngineId: ~p", 
				    [UnknownEngineID]),
			    %% 4.2.2.1.2
			    NIsReportable = snmp_misc:is_reportable_pdu(Type),
			    Val = inc(snmpUnknownPDUHandlers),
			    ErrorInfo = 
				{#varbind{oid = ?snmpUnknownPDUHandlers,
					  variabletype = 'Counter32',
					  value = Val},
				 SecName,
				 [{securityLevel,   SecLevel},
				  {contextEngineID, CtxEngineID},
				  {contextName,     CtxName}]},
			    case generate_v3_report_msg(MsgID, 
							MsgSecModel,
							Data, 
							ErrorInfo, 
							Log) of
				{ok, Report} when NIsReportable =:= true ->
				    discard(snmpUnknownPDUHandlers, Report);
				_ ->
				    discard(snmpUnknownPDUHandlers)
			    end
		    end;
		_ -> % 7.2.13.a
		    ?vinfo("7.2.13a - invalid securityEngineID: ~p", 
			   [SecEngineID]),
		    discard({badSecurityEngineID, SecEngineID})
	    end;

	_ ->
	    %% 7.2.13 - This would be the requests which we should not 
	    %%          receive since we are a manager, barring possible
	    %%          proxy...
	    discard(Type)
    end.


sec_error(T1, T2) 
  when is_tuple(T1) andalso is_tuple(T2) andalso (size(T1) =:= size(T2)) ->
    Tags = {sec_engine_id, msg_sec_model, sec_name, sec_level, 
	    ctx_engine_id, ctx_name, request_id}, 
    sec_error(size(T1), T1, T2, Tags, []);
sec_error(T1, T2) ->
    [{internal_error, T1, T2}].

sec_error(0, _T1, _T2, _Tags, Acc) ->
    Acc;
sec_error(Idx, T1, T2, Tags, Acc) ->
    case element(Idx, T1) =:= element(Idx, T2) of
	true ->
	    sec_error(Idx - 1, T1, T2, Tags, Acc);
	false ->
	    Elem = {element(Idx, Tags), element(Idx, T1), element(Idx, T2)},
	    sec_error(Idx - 1, T1, T2, Tags, [Elem|Acc])
    end.
    
tot_mms(MgrMMS, AgentMMS) when MgrMMS > AgentMMS -> AgentMMS;
tot_mms(MgrMMS, _AgentMMS) -> MgrMMS.
    
get_security_module(?SEC_USM) ->
    snmpm_usm;
get_security_module(_) ->
    inc(snmpUnknownSecurityModels),
    discard(snmpUnknownSecurityModels).
    
check_sec_level([MsgFlag]) ->
    SecLevel = MsgFlag band 3,
    if 
	SecLevel == 2 -> 
	    inc(snmpInvalidMsgs),
	    discard(snmpInvalidMsgs);
	true ->
	    SecLevel
    end;
check_sec_level(_Unknown) ->
    inc(snmpInvalidMsgs),
    discard(snmpInvalidMsgs).

is_reportable([MsgFlag]) ->
    4 == (MsgFlag band 4).


check_sec_module_result({ok, X}, _, _, _, _) ->
    X;
check_sec_module_result({error, Reason, Info}, _, _, _, _) 
  when is_list(Info) ->
    %% case 7.2.6 b
    discard({securityError, Reason, Info});
check_sec_module_result({error, Reason, ErrorInfo}, V3Hdr, Data, true, Log) ->
    %% case 7.2.6 a
    ?vtrace("security module result:"
	    "~n   Reason:    ~p"
	    "~n   ErrorInfo: ~p", [Reason, ErrorInfo]),
    #v3_hdr{msgID = MsgID, msgSecurityModel = MsgSecModel} = V3Hdr,
    Pdu = get_scoped_pdu(Data),
    case generate_v3_report_msg(MsgID, MsgSecModel, Pdu, ErrorInfo, Log) of
	{ok, Report} ->
	    discard({securityError, Reason}, Report);
	{discarded, _SomeOtherReason} ->
	    discard({securityError, Reason})
    end;
check_sec_module_result({error, Reason, _ErrorInfo}, _, _, _, _) ->
    ?vtrace("security module result:"
	    "~n   Reason:     ~p"
	    "~n   _ErrorInfo: ~p", [Reason, _ErrorInfo]),
    discard({securityError, Reason});
check_sec_module_result(Res, _, _, _, _) ->
    ?vtrace("security module result:"
	    "~n   Res: ~p", [Res]),
    discard({securityError, Res}).

get_scoped_pdu(D) when is_list(D) ->
    (catch snmp_pdus:dec_scoped_pdu(D));
get_scoped_pdu(D) ->
    D.


%%-----------------------------------------------------------------
%% Generate a message
%%-----------------------------------------------------------------
generate_msg('version-3', NoteStore, Pdu, 
	     {SecModel, SecName, SecLevel, CtxEngineID, CtxName, 
	      TargetName}, Log) ->
    generate_v3_msg(NoteStore, Pdu, 
		    SecModel, SecName, SecLevel, CtxEngineID, CtxName, 
		    TargetName, Log);
generate_msg(Vsn, _NoteStore, Pdu, {Comm, _SecModel}, Log) ->
    generate_v1_v2c_msg(Vsn, Pdu, Comm, Log).


generate_v3_msg(NoteStore, Pdu, 
		SecModel, SecName, SecLevel, CtxEngineID, CtxName, 
		TargetName, Log) ->
    %% rfc2272: 7.1.6
    ?vdebug("generate_v3_msg -> 7.1.6", []),
    ScopedPDU = #scopedPdu{contextEngineID = CtxEngineID,
			   contextName     = CtxName,
			   data            = Pdu},
    case (catch snmp_pdus:enc_scoped_pdu(ScopedPDU)) of
	{'EXIT', Reason} ->
	    user_err("failed encoding scoped pdu "
		     "~n   pdu: ~w"
		     "~n   contextName: ~w"
		     "~n   reason: ~w", [Pdu, CtxName, Reason]),
	    {discarded, Reason};
	ScopedPDUBytes -> 
	    {ok, generate_v3_msg(NoteStore, Pdu, ScopedPDUBytes, 
				 SecModel, SecName, SecLevel, 
				 CtxEngineID, CtxName, TargetName, Log)}
    end.    

generate_v3_msg(NoteStore, 
		#pdu{type = Type} = Pdu, ScopedPduBytes, 
		SecModel, SecName, SecLevel, CtxEngineID, CtxName, 
		TargetName, Log) ->
    %% 7.1.7
    ?vdebug("generate_v3_msg -> 7.1.7", []),
    MsgID     = next_msg_id(),
    MsgFlags  = snmp_misc:mk_msg_flags(Type, SecLevel),
    V3Hdr     = #v3_hdr{msgID            = MsgID,
			msgMaxSize       = get_max_message_size(),
			msgFlags         = MsgFlags,
			msgSecurityModel = SecModel},
    Message   = #message{version = 'version-3', 
			 vsn_hdr = V3Hdr,
			 data    = ScopedPduBytes},
    SecModule = sec_module(SecModel),

    %% 7.1.9a
    ?vdebug("generate_v3_msg -> 7.1.9a", []),
    SecEngineID = sec_engine_id(TargetName),
    ?vtrace("SecEngineID: ~p", [SecEngineID]),
    %% 7.1.9b
    ?vdebug("generate_v3_msg -> 7.1.9b", []),
    case generate_v3_outgoing_msg(Message, SecModule, SecEngineID,
				  SecName, [], SecLevel) of
	{ok, Packet} ->
	    %% 7.1.9c
	    %% Store in cache for 150 sec.
	    ?vdebug("generate_v3_msg -> 7.1.9c", []),
	    %% The request id is just in the case when we receive a 
	    %% report with incorrect securityModel and/or securityLevel
	    CacheVal = {SecEngineID, SecModel, SecName, SecLevel,
			CtxEngineID, CtxName, Pdu#pdu.request_id},
	    snmp_note_store:set_note(NoteStore, 1500, MsgID, CacheVal),
	    Log(Packet),
	    inc_snmp_out(Pdu),
	    ?vdebug("generate_v3_msg -> done", []),
	    Packet;

	Error ->
	    throw(Error)
    end.


sec_module(?SEC_USM) ->
    snmpm_usm.

%% 9) If the PDU is a GetRequest-PDU, GetNextRequest-PDU,
%%    GetBulkRequest-PDU, SetRequest-PDU, InformRequest-PDU, or or
%%    SNMPv2-Trap-PDU, then
%%
%%    a) If the PDU is an SNMPv2-Trap-PDU, then securityEngineID is set
%%       to the value of this entity's snmpEngineID.
%%
%%       Otherwise, the snmpEngineID of the target entity is determined,
%%       in an implementation-dependent manner, possibly using
%%       transportDomain and transportAddress.  The value of
%%       securityEngineID is set to the value of the target entity's
%%       snmpEngineID.
%% 
%% As we never send traps, the SecEngineID is allways the 
%% snmpEngineID of the target entity!
sec_engine_id(TargetName) ->
    case get_agent_engine_id(TargetName) of
	{ok, EngineId} ->
	    EngineId;
	_ ->
	    config_err("Can't find engineID for "
		       "snmpTargetAddrName ~p", [TargetName]),
	    %% this will trigger error in secmodule
	    "" 
	end.


%% BMK BMK BMK
%% This one looks very similar to lik generate_v1_v2c_response_msg!
%% Common/shared? Should there be differences?
%% 
generate_v1_v2c_msg(Vsn, Pdu, Community, Log) ->
    ?vdebug("generate_v1_v2c_msg -> encode pdu", []),
    case (catch snmp_pdus:enc_pdu(Pdu)) of
	{'EXIT', Reason} ->
	    user_err("failed encoding pdu: "
		     "(pdu: ~w, community: ~w): ~n~w",
		     [Pdu, Community, Reason]),
	    {discarded, Reason};
	PduBytes ->
	    MMS     = get_max_message_size(),
	    Message = #message{version = Vsn, 
			       vsn_hdr = Community, 
			       data    = PduBytes},
	    case generate_v1_v2c_outgoing_msg(Message) of
		{error, Reason} ->
		    user_err("failed encoding message "
			     "(pdu: ~w, community: ~w): ~n~w",
			     [Pdu, Community, Reason]),
		    {discarded, Reason};
		{ok, Packet} when size(Packet) =< MMS ->
		    Log(Packet),
		    inc_snmp_out(Pdu),
		    {ok, Packet};
		{ok, Packet} ->
		    ?vlog("packet max size exceeded: "
			  "~n   MMS: ~p"
			  "~n   Len: ~p",
			  [MMS, size(Packet)]),
		    {discarded, tooBig}
	    end
    end.



%% -----------------------------------------------------------------------

generate_response_msg('version-3', Pdu,
		      {MsgID, SecModel, SecName, SecLevel, 
		       CtxEngineID, CtxName, SecData}, Log) ->
    generate_v3_response_msg(Pdu, MsgID, SecModel, SecName, SecLevel, 
			     CtxEngineID, CtxName, SecData, Log);
generate_response_msg(Vsn, Pdu, {Comm, _SecModel}, Log) ->
    generate_v1_v2c_response_msg(Vsn, Pdu, Comm, Log);
generate_response_msg(Vsn, Pdu, {Comm, _SecModel, _TDomain, _TAddress}, Log) ->
    generate_v1_v2c_response_msg(Vsn, Pdu, Comm, Log).


generate_v3_response_msg(#pdu{type = Type} = Pdu, MsgID, 
			 SecModel, SecName, SecLevel, 
			 CtxEngineID, CtxName, SecData, Log) ->
    %% rfc2272: 7.1 steps 6-8
    ScopedPdu = #scopedPdu{contextEngineID = CtxEngineID,
			   contextName     = CtxName,
			   data            = Pdu},
    case (catch snmp_pdus:enc_scoped_pdu(ScopedPdu)) of
	{'EXIT', Reason} ->
	    user_err("failed encoded scoped pdu "
		     "(pdu: ~w, contextName: ~w): ~n~w",
		     [Pdu, CtxName, Reason]),
	    {discarded, Reason};
	ScopedPduBytes -> 
	    MMS      = get_max_message_size(),
	    MsgFlags = snmp_misc:mk_msg_flags(Type, SecLevel),
	    V3Hdr    = #v3_hdr{msgID            = MsgID,
			       msgMaxSize       = MMS,
			       msgFlags         = MsgFlags,
			       msgSecurityModel = SecModel},
	    Message  = #message{version = 'version-3', 
				vsn_hdr = V3Hdr, 
				data    = ScopedPduBytes},
	    %% We know that the security model is valid when we
	    %% generate a response.
	    SecModule = sec_module(SecModel),
	    SecEngineID = get_engine_id(),
	    case generate_v3_outgoing_msg(Message, SecModule, SecEngineID, 
					  SecName, SecData, SecLevel) of
		%% Check the packet size. Send the msg even
		%% if it's larger than the agent can handle - 
		%% it will be dropped. Just check against the
		%% internal size.  
		{ok, Packet} when size(Packet) =< MMS ->
		    if
			SecLevel == 3 -> 
			    %% encrypted - log decrypted pdu
			    Log({V3Hdr, ScopedPduBytes});
			true -> 
			    %% otherwise log the entire msg
			    Log(Packet)
		    end,
		    inc_snmp_out(Pdu),
		    {ok, Packet};
		
		{ok, _Packet} when Pdu#pdu.error_status =:= tooBig ->
		    ?vlog("packet max size exceeded (tooBog): "
			  "~n   MMS: ~p", [MMS]),
		    inc(snmpSilentDrops),
		    {discarded, tooBig};
		{ok, _Packet} ->
		    ?vlog("packet max size exceeded: "
			  "~n   MMS: ~p", [MMS]),
		    TooBigPdu = Pdu#pdu{error_status = tooBig,
					error_index  = 0, 
					varbinds     = []},
		    generate_v3_response_msg(TooBigPdu, MsgID, 
					     SecModel, SecName, SecLevel,
					     CtxEngineID, 
					     CtxName,
					     SecData, Log);
		Error ->
		    Error
	    end
    end.


generate_v3_outgoing_msg(Message, 
			 SecModule, SecEngineID, SecName, SecData, SecLevel) ->
    case (catch SecModule:generate_outgoing_msg(Message, 
						SecEngineID,
						SecName, SecData, 
						SecLevel)) of
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


generate_v1_v2c_response_msg(Vsn, Pdu, Comm, Log) ->
    case (catch snmp_pdus:enc_pdu(Pdu)) of
	{'EXIT', Reason} ->
	    user_err("failed encoding pdu: "
		     "(pdu: ~w, community: ~w): ~n~w",
		     [Pdu, Comm, Reason]),
	    {discarded, Reason};
	PduBytes ->
	    MMS = get_max_message_size(),	    
	    Message = #message{version = Vsn, 
			       vsn_hdr = Comm, 
			       data    = PduBytes},
	    case generate_v1_v2c_outgoing_msg(Message) of
		{error, Reason} ->
		    user_err("failed encoding message only "
			     "(pdu: ~w, community: ~w): ~n~w",
			     [Pdu, Comm, Reason]),
		    {discarded, Reason};
		
		{ok, Packet} when size(Packet) =< MMS ->
		    Log(Packet),
		    inc_snmp_out(Pdu),
		    {ok, Packet};

		{ok, Packet} ->  %% Too big
		    too_big(Vsn, Pdu, Comm, MMS, size(Packet), Log)
	    end
    end.
		    

too_big('version-1' = Vsn, #pdu{type = 'get-response'} = Pdu, 
	Comm, _MMS, _Len, Log) ->
    %% In v1, the varbinds should be identical to the incoming
    %% request. It isn't identical now! Make acceptable (?) 
    %% approximation.
    V = set_vb_null(Pdu#pdu.varbinds),
    TooBigPdu = Pdu#pdu{error_status = tooBig, error_index = 0, varbinds = V},
    too_big(Vsn, TooBigPdu, Comm, Log);
too_big('version-2' = Vsn, #pdu{type = 'get-response'} = Pdu, 
	Comm, _MMS, _Len, Log) ->
    %% In v2, varbinds should be empty (reasonable!)
    TooBigPdu = Pdu#pdu{error_status = tooBig, error_index = 0, varbinds = []},
    too_big(Vsn, TooBigPdu, Comm, Log);
too_big(_Vsn, Pdu, _Comm, _Log, MMS, Len) ->
    user_err("encoded pdu, ~p bytes, exceeded "
	     "max message size of ~p bytes. Pdu: ~n~w", 
	     [Len, MMS, Pdu]),
    {discarded, tooBig}.
   

too_big(Vsn, Pdu, Comm, Log) ->
    case (catch snmp_pdus:enc_pdu(Pdu)) of
	{'EXIT', Reason} ->
	    user_err("failed encoding pdu "
		     "(pdu: ~w, community: ~w): ~n~w", 
		     [Pdu, Comm, Reason]),
	    {discarded, Reason};
	PduBytes -> 
	    Message = #message{version = Vsn, 
			       vsn_hdr = Comm, 
			       data    = PduBytes},
	    case generate_v1_v2c_outgoing_msg(Message) of 
		{error, Reason} ->
		    user_err("failed encoding message only"
			     "(pdu: ~w, community: ~w): ~n~w", 
			     [Pdu, Comm, Reason]),
		    {discarded, Reason};
		{ok, Bin} ->
		    Log(Bin),
		    inc_snmp_out(Pdu),
		    {ok, Bin}
	    end
    end.
	
set_vb_null(Vbs) ->
    [Vb#varbind{variabletype = 'NULL', value = 'NULL'} || Vb <- Vbs].


generate_v1_v2c_outgoing_msg(Message) ->
    ?vdebug("generate_v1_v2c_outgoing_msg -> encode message", []),
    case (catch snmp_pdus:enc_message_only(Message)) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Bin when is_binary(Bin) ->
	    {ok, Bin};
	Packet when is_list(Packet) ->
	    case (catch list_to_binary(Packet)) of
		Bin when is_binary(Bin) ->
		    {ok, Bin};
		{'EXIT', Reason} ->
		    {error, Reason}
	    end
    end.



generate_v3_report_msg(MsgID, SecModel, ScopedPdu, ErrInfo, Log) 
  when is_record(ScopedPdu, scopedPdu) ->
    ReqID = (ScopedPdu#scopedPdu.data)#pdu.request_id,
    generate_v3_report_msg2(MsgID, ReqID, SecModel, ErrInfo, Log);
generate_v3_report_msg(MsgID, SecModel, _, ErrInfo, Log) ->
    %% RFC2572, 7.1.3.c.4
    generate_v3_report_msg2(MsgID, 0, SecModel, ErrInfo, Log).


generate_v3_report_msg2(MsgID, ReqID, SecModel, ErrInfo, Log) ->
    {Varbind, SecName, Opts} = ErrInfo,
    Pdu = #pdu{type         = report, 
	       request_id   = ReqID,
	       error_status = noError, 
	       error_index  = 0,
	       varbinds     = [Varbind]},
    SecLevel    = snmp_misc:get_option(securityLevel, Opts, 0),
    CtxEngineID = snmp_misc:get_option(contextEngineID, Opts, get_engine_id()),
    CtxName     = snmp_misc:get_option(contextName, Opts, ""),
    SecData     = snmp_misc:get_option(sec_data, Opts, []),
    generate_v3_response_msg(Pdu, 
			     MsgID, SecModel, SecName, SecLevel,
			     CtxEngineID, CtxName, SecData, Log).

    
%%-----------------------------------------------------------------

%% Get "our" (manager) MMS
get_max_message_size() ->
    case snmpm_config:get_engine_max_message_size() of
	{ok, MMS} ->
	    MMS;
	E ->
	    user_err("failed retreiving engine max message size: ~w", [E]),
	    484
    end.

%% The the MMS of the agent
get_agent_max_message_size(Domain, Addr) ->
    case snmpm_config:get_agent_engine_max_message_size(Domain, Addr) of
	{ok, MMS} ->
	    MMS;
	_Error ->
            TAddr = fun(TN) ->
                            case snmpm_config:agent_info(TN, taddress) of
                                {ok, TA} ->
                                    TA;
                                {error, _} ->
                                    undefined
                            end
                    end,
            KnownAgents =
                [{TargetName, TAddr(TargetName)} ||
                    TargetName <- snmpm_config:which_agents()],
	    ?vlog("[agent engine max msg size lookup] unknown agent: ~s"
                  "~n      Known Agents: ~p",
		  [snmp_conf:mk_addr_string({Domain, Addr}), KnownAgents]),
	    get_max_message_size()
    end.
%% get_agent_max_message_size(Addr, Port) ->
%%     case snmpm_config:get_agent_engine_max_message_size(Addr, Port) of
%% 	{ok, MMS} ->
%% 	    MMS;
%% 	_Error ->
%% 	    ?vlog("unknown agent: ~w:~w", [Addr, Port]),
%% 	    get_max_message_size()
%%     end.

%% Get "our" (manager) engine id
get_engine_id() ->
    case snmpm_config:get_engine_id() of
	{ok, Id} ->
	    Id;
	_Error ->
	    ""
    end.

%% The engine id of the agent
get_agent_engine_id(Name) ->
    snmpm_config:get_agent_engine_id(Name).

is_known_engine_id(EngineID, {Addr, Port}) ->
    snmpm_config:is_known_engine_id(EngineID, Addr, Port).


%%-----------------------------------------------------------------
%% Sequence number (msg-id & req-id) functions
%%-----------------------------------------------------------------
next_msg_id() ->
    next_id(msg_id).

next_req_id() ->
    next_id(req_id).

next_id(Id) ->
    snmpm_config:incr_counter(Id, 1).


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

init_usm(true) ->
    snmpm_usm:init();
init_usm(_) ->
    ok.


%%-----------------------------------------------------------------
%% Counter functions
%%-----------------------------------------------------------------
init_counters() -> 
    F = fun(Counter) -> maybe_create_counter(Counter) end,
    lists:map(F, counters()).

reset_counters() -> 
    F = fun(Counter) -> snmpm_config:reset_stats_counter(Counter) end,
    lists:map(F, counters()).

reset_usm(true) ->
    snmpm_usm:reset();
reset_usm(_) ->
    ok.

maybe_create_counter(Counter) ->
    snmpm_config:maybe_cre_stats_counter(Counter, 0).

counters() ->
    [snmpInPkts,
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
inc(Name)    -> inc(Name, 1).
inc(Name, N) -> snmpm_config:incr_stats_counter(Name, N).

inc_snmp_in(#pdu{type = Type}) ->
    inc_in_type(Type);
inc_snmp_in(TrapPdu) when is_record(TrapPdu, trappdu) ->
    inc(snmpInPkts),
    inc(snmpInTraps).

inc_snmp_out(#pdu{type         = Type, 
		  error_status = ErrorStatus}) ->
    inc(snmpOutPkts),
    inc_out_err(ErrorStatus),
    inc_out_type(Type).

inc_out_type('get-request')      -> inc(snmpOutGetRequests);
inc_out_type('get-next-request') -> inc(snmpOutGetNexts);
inc_out_type('set-request')      -> inc(snmpOutSetRequests);
inc_out_type(_) -> ok.

inc_out_err(genErr)     -> inc(snmpOutGenErrs);
inc_out_err(tooBig)     -> inc(snmpOutTooBigs);
inc_out_err(noSuchName) -> inc(snmpOutNoSuchNames);
inc_out_err(badValue)   -> inc(snmpOutBadValues);
inc_out_err(_)          -> ok.

inc_in_type('get-response') -> inc(snmpInGetResponses);
inc_in_type(_)              -> ok.


%%-----------------------------------------------------------------

discard(Reason) ->
    throw({discarded, Reason}).

discard(Reason, Report) ->
    throw({discarded, Reason, Report}).

user_err(F, A) ->
    error_msg("USER ERROR: " ++ F ++ "~n", A).

config_err(F, A) ->
    error_msg("CONFIG ERROR: " ++ F ++ "~n", A).

error_msg(F, A) ->
    ?snmpm_error("MPD: " ++ F, A).
