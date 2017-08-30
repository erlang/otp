%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%% ts:run(snmp, snmp_agent_test, [batch]).
%% 
-module(snmp_test_mgr_misc).

%% API
-export([start_link_packet/8, start_link_packet/9, start_link_packet/10,
	 stop/1, 
	 send_discovery_pdu/2, 
	 send_pdu/2, send_msg/4, send_bytes/2,
	 error/2,
	 get_pdu/1, set_pdu/2, format_hdr/1]).

%% internal exports
-export([init_packet/11]).

-compile({no_auto_import, [error/2]}).

-define(SNMP_USE_V3, true).
-include_lib("snmp/include/snmp_types.hrl").


%%----------------------------------------------------------------------
%% The InHandler process will receive messages on the form {snmp_pdu, Pdu}.
%%----------------------------------------------------------------------
start_link_packet(
  InHandler, AgentIp, UdpPort, TrapUdp, VsnHdr, Version, Dir, BufSz) ->
    start_link_packet(
      InHandler, AgentIp, UdpPort, TrapUdp, VsnHdr, Version, Dir, BufSz,
      false).

start_link_packet(
  InHandler, AgentIp, UdpPort, TrapUdp, VsnHdr, Version, Dir, BufSz,
  Dbg) ->
    start_link_packet(
      InHandler, AgentIp, UdpPort, TrapUdp, VsnHdr, Version, Dir, BufSz,
      Dbg, inet).

start_link_packet(
  InHandler, AgentIp, UdpPort, TrapUdp, VsnHdr, Version, Dir, BufSz,
  Dbg, IpFamily) when is_integer(UdpPort) ->
    Args =
	[self(),
	 InHandler, AgentIp, UdpPort, TrapUdp,  VsnHdr, Version, Dir, BufSz,
	 Dbg, IpFamily],
    proc_lib:start_link(?MODULE, init_packet, Args).

stop(Pid) ->
    Pid ! {stop, self()},
    receive
	{Pid, stopped} -> ok
    end.
	

send_discovery_pdu(Pdu, PacketPid) when is_record(Pdu, pdu) ->
    PacketPid ! {send_discovery_pdu, self(), Pdu},
    await_discovery_response_pdu().

await_discovery_response_pdu() ->
    receive
	{discovery_response, Reply} ->
	    Reply
    end.
    

send_pdu(Pdu, PacketPid) when is_record(Pdu, pdu) ->
    PacketPid ! {send_pdu, Pdu}.

send_msg(Msg, PacketPid, Ip, Udp) when is_record(Msg, message) ->
    PacketPid ! {send_msg, Msg, Ip, Udp}.

send_bytes(Bytes, PacketPid) ->
    PacketPid ! {send_bytes, Bytes}.

%%--------------------------------------------------
%% The SNMP encode/decode process
%%--------------------------------------------------
init_packet(
  Parent,
  SnmpMgr, AgentIp, UdpPort, TrapUdp, VsnHdr, Version, Dir, BufSz,
  DbgOptions, IpFamily) ->
    put(sname, mgr_misc),
    init_debug(DbgOptions),
    {ok, UdpId} =
	gen_udp:open(TrapUdp, [{recbuf,BufSz}, {reuseaddr, true}, IpFamily]),
    put(msg_id, 1),
    proc_lib:init_ack(Parent, self()),
    init_usm(Version, Dir),
    packet_loop(SnmpMgr, UdpId, AgentIp, UdpPort, VsnHdr, Version, []).

init_debug(Dbg) when is_atom(Dbg) ->
    put(debug,Dbg),
    %% put(verbosity, silence);
    put(verbosity, trace);
init_debug(DbgOptions) when is_list(DbgOptions) ->
    case lists:keysearch(debug, 1, DbgOptions) of
	{value, {_, Dbg}} when is_atom(Dbg) ->
	    put(debug, Dbg);
	_ ->
	    put(debug, false)
    end,
    case lists:keysearch(verbosity, 1, DbgOptions) of
	{value, {_, Ver}} when is_atom(Ver) ->
	    put(verbosity, Ver);
	_ ->
	    put(verbosity, silence)
    end,
    ok.


packet_loop(SnmpMgr, UdpId, AgentIp, UdpPort, VsnHdr, Version, MsgData) ->
    receive
	{send_discovery_pdu, From, Pdu} ->
	    d("packet_loop -> received send_discovery_pdu with"
	      "~n   From: ~p"
	      "~n   Pdu:  ~p", [From, Pdu]),
	    case mk_discovery_msg(Version, Pdu, VsnHdr, "") of
		error ->
		    ok;
		{M, B} when is_list(B) -> 
		    put(discovery, {M, From}),
		    display_outgoing_message(M),
		    udp_send(UdpId, AgentIp, UdpPort, B)
	    end,
	    packet_loop(SnmpMgr, UdpId, AgentIp, UdpPort, VsnHdr, Version, []);

	{send_pdu, Pdu} ->
	    d("packet_loop -> received send_pdu with"
	      "~n   Pdu:  ~p", [Pdu]),
	    case mk_msg(Version, Pdu, VsnHdr, MsgData) of
		error ->
		    ok;
		B when is_list(B) -> 
		    udp_send(UdpId, AgentIp, UdpPort, B)
	    end,
	    packet_loop(SnmpMgr,UdpId,AgentIp,UdpPort,VsnHdr,Version,[]);

	{send_msg, Msg, Ip, Udp} ->
	    d("packet_loop -> received send_msg with"
	      "~n   Msg:  ~p"
	      "~n   Ip:   ~p"
	      "~n   Udp:  ~p", [Msg,Ip,Udp]),
	    case catch snmp_pdus:enc_message(Msg) of
		{'EXIT', Reason} ->
		    error("Encoding error:"
			  "~n   Msg:    ~w"
			  "~n   Reason: ~w",[Msg, Reason]);
		B when is_list(B) -> 
		    udp_send(UdpId, Ip, Udp, B)
	    end,
	    packet_loop(SnmpMgr,UdpId,AgentIp,UdpPort,VsnHdr,Version,[]);
	{udp, UdpId, Ip, UdpPort, Bytes} ->
	    d("packet_loop -> received udp with"
	      "~n   UdpId:     ~p"
	      "~n   Ip:        ~p"
	      "~n   UdpPort:   ~p"
	      "~n   sz(Bytes): ~p", [UdpId, Ip, UdpPort, sz(Bytes)]),	    
	    MsgData3 = handle_udp_packet(Version, erase(discovery),
					 UdpId, Ip, UdpPort, Bytes,
					 SnmpMgr, AgentIp),
	    packet_loop(SnmpMgr,UdpId,AgentIp,UdpPort,VsnHdr,Version,
			MsgData3);
	{send_bytes, B} ->
	    d("packet_loop -> received send_bytes with"
	      "~n   sz(B): ~p", [sz(B)]),	    
	    udp_send(UdpId, AgentIp, UdpPort, B),
	    packet_loop(SnmpMgr,UdpId,AgentIp,UdpPort,VsnHdr,Version,[]);
	{stop, Pid} ->
	    d("packet_loop -> received stop from ~p", [Pid]),	    
	    gen_udp:close(UdpId),
	    Pid ! {self(), stopped},
	    exit(normal);
	Other ->
	    d("packet_loop -> received unknown"
	      "~n   ~p", [Other]),	    
	    exit({snmp_packet_got_other, Other})
    end.


handle_udp_packet(_V, undefined, 
		  UdpId, Ip, UdpPort,
		  Bytes, SnmpMgr, AgentIp) ->
    M = (catch snmp_pdus:dec_message_only(Bytes)),
    MsgData3 =
	case M of
	    Message when Message#message.version =:= 'version-3' ->
		d("handle_udp_packet -> version 3"),
		case catch handle_v3_msg(Bytes, Message) of
		    {ok, NewData, MsgData2} ->
			Msg = Message#message{data = NewData},
			case SnmpMgr of
			    {pdu, Pid} ->
				Pdu = get_pdu(Msg), 
				d("packet_loop -> "
				  "send pdu to manager (~w): ~p", [Pid, Pdu]),
				Pid ! {snmp_pdu, Pdu};
			    {msg, Pid} ->
				d("packet_loop -> "
				  "send msg to manager (~w): ~p", [Pid, Msg]),
				Pid ! {snmp_msg, Msg, Ip, UdpPort}
			end,
			MsgData2;
		    {error, Reason, B} ->
			udp_send(UdpId, AgentIp, UdpPort, B),
			error("Decoding error. Auto-sending Report.\n"
			      "Reason: ~w "
			      "(UDPport: ~w, Ip: ~w)",
			      [Reason, UdpPort, Ip]),
			[];
		    {error, Reason} ->
			error("Decoding error. "
				      "Bytes: ~w ~n Reason: ~w "
			      "(UDPport: ~w, Ip: ~w)",
			      [Bytes, Reason, UdpPort, Ip]),
			[]
		end;
	    Message when is_record(Message, message) ->
		%% v1 or v2c
		d("handle_udp_packet -> version v1 or v2c"),
		case catch snmp_pdus:dec_pdu(Message#message.data) of
		    Pdu when is_record(Pdu, pdu) ->
			case SnmpMgr of
			    {pdu, Pid} ->
				d("handle_udp_packet -> "
				  "send pdu to manager (~w): ~p", 
				  [Pid, Pdu]),
				Pid ! {snmp_pdu, Pdu};
			    {msg, Pid} ->
				d("handle_udp_packet -> "
				  "send pdu-msg to manager (~w):  ~p", 
				  [Pid, Pdu]),
				Msg = Message#message{data = Pdu},
				Pid ! {snmp_msg, Msg, Ip, UdpPort}
			end;
		    Pdu when is_record(Pdu, trappdu) ->
			case SnmpMgr of
			    {pdu, Pid} ->
				d("handle_udp_packet -> "
				  "send trap to manager (~w): ~p", 
				  [Pid, Pdu]),
				Pid ! {snmp_pdu, Pdu};
			    {msg, Pid} ->
				d("handle_udp_packet -> "
				  "send trap-msg to manager (~w): ~p", 
				  [Pid, Pdu]),
				Msg = Message#message{data = Pdu},
				Pid ! {snmp_msg, Msg, Ip, UdpPort}
			end;
		    Reason ->
			error("Decoding error. "
			      "Bytes: ~w ~n Reason: ~w "
			      "(UDPport: ~w, Ip: ~w)",
			      [Bytes, Reason, UdpPort, Ip])
		end,
		[];
	    Reason ->
		error("Decoding error. Bytes: ~w ~n Reason: ~w "
		      "(UDPport: ~w, Ip: ~w)",
		      [Bytes, Reason, UdpPort, Ip]),
		[]
	end,
    MsgData3;
handle_udp_packet(V, {DiscoReqMsg, From}, _UdpId, _Ip, _UdpPort, 
		  Bytes, _, _AgentIp) ->
    DiscoRspMsg = (catch snmp_pdus:dec_message(Bytes)),
    display_incomming_message(DiscoRspMsg),
    _Reply = (catch check_discovery_result(V, DiscoReqMsg, DiscoRspMsg)),
    case (catch check_discovery_result(V, DiscoReqMsg, DiscoRspMsg)) of
	{ok, AgentEngineID} when is_list(AgentEngineID) ->
	    %% Ok, step 1 complete, now for step 2
	    %% Which we skip for now
	    OK = {ok, AgentEngineID}, 
	    From ! {discovery_response, OK},
	    [];
	Error ->
	    From ! {discovery_response, Error},
	    []
    end.


%% This function assumes that the agent and the manager (thats us) 
%% has the same version.
check_discovery_result('version-3', DiscoReqMsg, DiscoRspMsg) ->
    ReqMsgID = getMsgID(DiscoReqMsg),
    RspMsgID = getMsgID(DiscoRspMsg),
    check_msgID(ReqMsgID, RspMsgID),
    ReqRequestId = getRequestId('version-3', DiscoReqMsg),
    RspRequestId = getRequestId('version-3', DiscoRspMsg),
    check_requestId(ReqRequestId, RspRequestId),
    {ok, getMsgAuthEngineID(DiscoRspMsg)};
check_discovery_result(Version, DiscoReqMsg, DiscoRspMsg) ->
    ReqRequestId = getRequestId(Version, DiscoReqMsg),
    RspRequestId = getRequestId(Version, DiscoRspMsg),
    check_requestId(ReqRequestId, RspRequestId),
    {ok, getSysDescr(DiscoRspMsg)}.

check_msgID(ID, ID) ->
    ok;
check_msgID(ReqMsgID, RspMsgID) ->
    throw({error, {invalid_msgID, ReqMsgID, RspMsgID}}).

check_requestId(Id,Id) ->
    ok;
check_requestId(ReqRequestId, RspRequestId) ->
    throw({error, {invalid_requestId, ReqRequestId, RspRequestId}}).

getMsgID(M) when is_record(M, message) ->
    (M#message.vsn_hdr)#v3_hdr.msgID.

getRequestId('version-3',M) when is_record(M, message) ->
    ((M#message.data)#scopedPdu.data)#pdu.request_id;
getRequestId(_Version,M) when is_record(M, message) ->
    (M#message.data)#pdu.request_id;
getRequestId(Version,M) ->
    io:format("************* ERROR ****************"
	      "~n   Version: ~w"
	      "~n   M:       ~w~n", [Version,M]),
    throw({error, {unknown_request_id, Version, M}}).
    
getMsgAuthEngineID(M) when is_record(M, message) ->
    SecParams1 = (M#message.vsn_hdr)#v3_hdr.msgSecurityParameters,
    SecParams2 = snmp_pdus:dec_usm_security_parameters(SecParams1),
    SecParams2#usmSecurityParameters.msgAuthoritativeEngineID.
    
getSysDescr(M) when is_record(M, message) ->
    getSysDescr((M#message.data)#pdu.varbinds);
getSysDescr([]) ->
    not_found;
getSysDescr([#varbind{oid = [1,3,6,1,2,1,1,1], value = Value}|_]) ->
    Value;
getSysDescr([#varbind{oid = [1,3,6,1,2,1,1,1,0], value = Value}|_]) ->
    Value;
getSysDescr([_|T]) ->
    getSysDescr(T).
    
handle_v3_msg(Packet, #message{vsn_hdr = V3Hdr, data = Data}) ->
    d("handle_v3_msg -> entry"),
    %% Code copied from snmp_mpd.erl
    #v3_hdr{msgID = MsgId, msgFlags = MsgFlags,
	    msgSecurityModel = MsgSecurityModel,
	    msgSecurityParameters = SecParams} = V3Hdr,
    SecModule = get_security_module(MsgSecurityModel), 
    d("handle_v3_msg -> SecModule: ~p", [SecModule]),
    SecLevel = hd(MsgFlags) band 3,
    d("handle_v3_msg -> SecLevel: ~p", [SecLevel]),
    IsReportable = snmp_misc:is_reportable(MsgFlags),
    SecRes = (catch SecModule:process_incoming_msg(list_to_binary(Packet), 
						   Data,SecParams,SecLevel)),
    {_SecEngineID, SecName, ScopedPDUBytes, SecData, _} =
	check_sec_module_result(SecRes, V3Hdr, Data, IsReportable),
    case (catch snmp_pdus:dec_scoped_pdu(ScopedPDUBytes)) of
	ScopedPDU when is_record(ScopedPDU, scopedPdu) -> 
	    {ok, ScopedPDU, {MsgId, SecName, SecData}};
	{'EXIT', Reason} ->
	    throw({error, Reason});
	Error ->
	    throw({error, {scoped_pdu_decode_failed, Error}})
    end;
handle_v3_msg(_Packet, BadMessage) ->
    throw({error, bad_message, BadMessage}).

get_security_module(?SEC_USM) ->
    snmpa_usm;
get_security_module(SecModel) ->
    throw({error, {unknown_sec_model, SecModel}}).

check_sec_module_result(Res, V3Hdr, Data, IsReportable) ->
    d("check_sec_module_result -> entry with"
      "~n   Res: ~p", [Res]),
    case Res of
	{ok, X} -> 
	    X;
	{error, Reason, []} ->
	    throw({error, {securityError, Reason}});
	{error, Reason, ErrorInfo} when IsReportable == true ->
	    #v3_hdr{msgID = MsgID, msgSecurityModel = MsgSecModel} = V3Hdr,
	    case generate_v3_report_msg(MsgID, MsgSecModel, Data, ErrorInfo) of
		error ->
		    throw({error, {securityError, Reason}});
		Packet ->
		    throw({error, {securityError, Reason}, Packet})
	    end;
	{error, Reason, _} ->
	    throw({error, {securityError, Reason}});
	Else ->
	    throw({error, {securityError, Else}})
    end.

generate_v3_report_msg(_MsgID, _MsgSecurityModel, Data, ErrorInfo) ->
    d("generate_v3_report_msg -> entry with"
      "~n   ErrorInfo: ~p", [ErrorInfo]),
    {Varbind, SecName, Opts} = ErrorInfo,
    ReqId =
	if is_record(Data, scopedPdu) -> (Data#scopedPdu.data)#pdu.request_id;
	   true -> 0
	end,
    Pdu = #pdu{type = report, request_id = ReqId,
	       error_status = noError, error_index = 0,
	       varbinds = [Varbind]},
    SecLevel = snmp_misc:get_option(securityLevel, Opts, 0),
    SnmpEngineID = snmp_framework_mib:get_engine_id(),
    ContextEngineID = 
	snmp_misc:get_option(contextEngineID, Opts, SnmpEngineID),
    ContextName = snmp_misc:get_option(contextName, Opts, ""),
    mk_msg('version-3', Pdu, {ContextName, SecName, SnmpEngineID, 
			      ContextEngineID, SecLevel},
	   undefined).


error(Format, Data) ->
    io:format("*** Error ***~n"),
    ok = io:format(Format, Data),
    io:format("~n").


mk_discovery_msg('version-3', Pdu, _VsnHdr, UserName) ->
    ScopedPDU = #scopedPdu{contextEngineID = "",
			   contextName     = "",
			   data            = Pdu},
    Bytes = snmp_pdus:enc_scoped_pdu(ScopedPDU),
    MsgID = get(msg_id),
    put(msg_id, MsgID+1),
    UsmSecParams = 
	#usmSecurityParameters{msgAuthoritativeEngineID = "",
			       msgAuthoritativeEngineBoots = 0,
			       msgAuthoritativeEngineTime = 0,
			       msgUserName = UserName,
			       msgPrivacyParameters = "",
			       msgAuthenticationParameters = ""},
    SecBytes = snmp_pdus:enc_usm_security_parameters(UsmSecParams),
    PduType = Pdu#pdu.type,
    Hdr = #v3_hdr{msgID                 = MsgID, 
		  msgMaxSize            = 1000,
		  msgFlags              = snmp_misc:mk_msg_flags(PduType, 0),
		  msgSecurityModel      = ?SEC_USM,
		  msgSecurityParameters = SecBytes},
    Msg = #message{version = 'version-3', vsn_hdr = Hdr, data = Bytes},
    case (catch snmp_pdus:enc_message_only(Msg)) of
	{'EXIT', Reason} ->
	    error("Discovery encoding error: "
		  "~n   Pdu:    ~w"
		  "~n   Reason: ~w",[Pdu, Reason]),
	    error;
	L when is_list(L) ->
	    {Msg#message{data = ScopedPDU}, L}
    end;
mk_discovery_msg(Version, Pdu, {Com, _, _, _, _}, _UserName) ->
    Msg = #message{version = Version, vsn_hdr = Com, data = Pdu},
    case catch snmp_pdus:enc_message(Msg) of
	{'EXIT', Reason} ->
	    error("Discovery encoding error:"
		  "~n   Pdu:    ~w"
		  "~n   Reason: ~w",[Pdu, Reason]),
	    error;
	L when is_list(L) -> 
	    {Msg, L}
    end.


mk_msg('version-3', Pdu, {Context, User, EngineID, CtxEngineId, SecLevel}, 
       MsgData) ->
    d("mk_msg(version-3) -> entry with"
      "~n   Pdu:         ~p"
      "~n   Context:     ~p"
      "~n   User:        ~p"
      "~n   EngineID:    ~p"
      "~n   CtxEngineID: ~p"
      "~n   SecLevel:    ~p", 
      [Pdu, Context, User, EngineID, CtxEngineId, SecLevel]),
    %% Code copied from snmp_mpd.erl
    {MsgId, SecName, SecData} =
	if
	    is_tuple(MsgData) andalso (Pdu#pdu.type =:= 'get-response') ->
		MsgData;
	    true -> 
		Md = get(msg_id),
		put(msg_id, Md + 1),
		{Md, User, []}
	end,
    ScopedPDU = #scopedPdu{contextEngineID = CtxEngineId,
			   contextName = Context,
			   data = Pdu},
    ScopedPDUBytes = snmp_pdus:enc_scoped_pdu(ScopedPDU),

    PduType = Pdu#pdu.type,
    V3Hdr = #v3_hdr{msgID      = MsgId,
		    msgMaxSize = 1000,
		    msgFlags   = snmp_misc:mk_msg_flags(PduType, SecLevel),
		    msgSecurityModel = ?SEC_USM},
    Message = #message{version = 'version-3', vsn_hdr = V3Hdr,
		       data = ScopedPDUBytes},
    SecEngineID = case PduType of
		      'get-response' -> snmp_framework_mib:get_engine_id();
		      _ -> EngineID
		  end,
    case catch snmpa_usm:generate_outgoing_msg(Message, SecEngineID,
					       SecName, SecData, SecLevel) of
	{'EXIT', Reason} ->
	    error("version-3 message encoding exit"
		  "~n   Pdu:    ~w"
		  "~n   Reason: ~w",[Pdu, Reason]),
	    error;
	{error, Reason} ->
	    error("version-3 message encoding error"
		  "~n   Pdu:    ~w"
		  "~n   Reason: ~w",[Pdu, Reason]),
	    error;
	Packet ->
	    Packet
    end;
mk_msg(Version, Pdu, {Com, _User, _EngineID, _Ctx, _SecLevel}, _SecData) ->
    Msg = #message{version = Version, vsn_hdr = Com, data = Pdu},
    case catch snmp_pdus:enc_message(Msg) of
	{'EXIT', Reason} ->
	    error("~w encoding error"
		  "~n   Pdu:    ~w"
		  "~n   Reason: ~w",[Version, Pdu, Reason]),
	    error;
	B when is_list(B) -> 
	    B
    end.

format_hdr(#message{version = 'version-3', 
		    vsn_hdr = #v3_hdr{msgID = MsgId},
		    data = #scopedPdu{contextName = CName}}) ->
    io_lib:format("v3, ContextName = \"~s\"  Message ID = ~w\n",
		  [CName, MsgId]);
format_hdr(#message{version = Vsn, vsn_hdr = Com}) ->
    io_lib:format("~w, CommunityName = \"~s\"\n", [vsn(Vsn), Com]).

vsn('version-1') -> v1;
vsn('version-2') -> v2c.


udp_send(UdpId, AgentIp, UdpPort, B) ->
    case (catch gen_udp:send(UdpId, AgentIp, UdpPort, B)) of
	{error,ErrorReason} ->
	    error("failed (error) sending message to ~p:~p: "
		  "~n   ~p",[AgentIp, UdpPort, ErrorReason]);
	{'EXIT',ExitReason} ->
	    error("failed (exit) sending message to ~p:~p:"
		  "~n   ~p",[AgentIp, UdpPort, ExitReason]);
	_ ->
	    ok
    end.


get_pdu(#message{version = 'version-3', data = #scopedPdu{data = Pdu}}) ->
    Pdu;
get_pdu(#message{data = Pdu}) ->
    Pdu.

set_pdu(Msg, RePdu) when Msg#message.version == 'version-3' ->
    SP = (Msg#message.data)#scopedPdu{data = RePdu}, 
    Msg#message{data = SP};
set_pdu(Msg, RePdu) ->
    Msg#message{data = RePdu}.


init_usm('version-3', Dir) ->
    ets:new(snmp_agent_table, [set, public, named_table]),
    ets:insert(snmp_agent_table, {agent_mib_storage, persistent}),
    snmpa_local_db:start_link(normal, Dir, [{verbosity,trace}]),
    NameDb = snmpa_agent:db(snmpEngineID),
    R = snmp_generic:variable_set(NameDb, "mgrEngine"),
    io:format("~w:init_usm -> engine-id set result: ~p~n", [?MODULE,R]),
    snmp_framework_mib:set_engine_boots(1),
    snmp_framework_mib:set_engine_time(1),
    snmp_user_based_sm_mib:reconfigure(Dir);
init_usm(_Vsn, _Dir) ->
    ok.


display_incomming_message(M) ->
    display_message("Incomming",M).

display_outgoing_message(M) ->
    display_message("Outgoing", M).

display_message(Direction, M) when is_record(M, message) ->
    io:format("~s SNMP message:~n", [Direction]),
    V = M#message.version,
    display_version(V),
    display_hdr(V, M#message.vsn_hdr),
    display_msg_data(V, Direction, M#message.data);
display_message(Direction, M) ->
    io:format("~s message unknown: ~n~p", [Direction, M]).

display_version('version-3') ->
    display_prop("Version",'SNMPv3');
display_version(V) ->
    display_prop("Version",V).

display_hdr('version-3',H) ->
    display_msgID(H#v3_hdr.msgID),
    display_msgMaxSize(H#v3_hdr.msgMaxSize),
    display_msgFlags(H#v3_hdr.msgFlags),
    SecModel = H#v3_hdr.msgSecurityModel,
    display_msgSecurityModel(SecModel),
    display_msgSecurityParameters(SecModel,H#v3_hdr.msgSecurityParameters);
display_hdr(_V,Community) -> 
    display_community(Community).

display_community(Community) ->
    display_prop("Community",Community).

display_msgID(Id) ->
    display_prop("msgID",Id).

display_msgMaxSize(Size) ->
    display_prop("msgMaxSize",Size).

display_msgFlags([Flags]) ->
    display_prop("msgFlags",Flags);
display_msgFlags([]) ->
    display_prop("msgFlags",no_value_to_display);
display_msgFlags(Flags) ->
    display_prop("msgFlags",Flags).

display_msgSecurityModel(?SEC_USM) ->
    display_prop("msgSecurityModel",'USM');
display_msgSecurityModel(Model) ->
    display_prop("msgSecurityModel",Model).

display_msgSecurityParameters(?SEC_USM,Params) ->
    display_usmSecurityParameters(Params);
display_msgSecurityParameters(_Model,Params) ->
    display_prop("msgSecurityParameters",Params).

display_usmSecurityParameters(P) when is_list(P) ->
    P1 = lists:flatten(P),
    display_usmSecurityParameters(snmp_pdus:dec_usm_security_parameters(P1));
display_usmSecurityParameters(P) when is_record(P,usmSecurityParameters) ->
    ID = P#usmSecurityParameters.msgAuthoritativeEngineID,
    display_msgAuthoritativeEngineID(ID),
    Boots = P#usmSecurityParameters.msgAuthoritativeEngineBoots,
    display_msgAuthoritativeEngineBoots(Boots),
    Time = P#usmSecurityParameters.msgAuthoritativeEngineTime,
    display_msgAuthoritativeEngineTime(Time),
    Name = P#usmSecurityParameters.msgUserName,
    display_msgUserName(Name),
    SecParams = P#usmSecurityParameters.msgAuthenticationParameters,
    display_msgAuthenticationParameters(SecParams),
    PrivParams = P#usmSecurityParameters.msgPrivacyParameters,
    display_msgPrivacyParameters(PrivParams);
display_usmSecurityParameters(P) ->
    display_prop("unknown USM sec paraams",P).

display_msgAuthoritativeEngineID(ID) ->
    display_prop("msgAuthoritativeEngineID",ID).

display_msgAuthoritativeEngineBoots(V) ->
    display_prop("msgAuthoritativeEngineBoots",V).

display_msgAuthoritativeEngineTime(V) ->
    display_prop("msgAuthoritativeEngineTime",V).

display_msgUserName(V) ->
    display_prop("msgUserName",V).

display_msgAuthenticationParameters(V) ->
    display_prop("msgAuthenticationParameters",V).

display_msgPrivacyParameters(V) ->
    display_prop("msgPrivacyParameters",V).

display_msg_data('version-3',Direction,D) when is_record(D,scopedPdu) ->
    display_scoped_pdu(Direction,D);
display_msg_data(_Version,Direction,D) when is_record(D,pdu) ->
    display_pdu(Direction,D);
display_msg_data(_Version,_Direction,D) ->
    display_prop("Unknown message data",D).

display_scoped_pdu(Direction,P) ->
    display_contextEngineID(P#scopedPdu.contextEngineID),
    display_contextName(P#scopedPdu.contextName),
    display_scoped_pdu_data(Direction,P#scopedPdu.data).

display_contextEngineID(Id) ->
    display_prop("contextEngineID",Id).

display_contextName(Name) ->
    display_prop("contextName",Name).

display_scoped_pdu_data(Direction,D) when is_record(D,pdu) ->
    display_pdu(Direction,D);
display_scoped_pdu_data(Direction,D) when is_record(D,trappdu) ->
    display_trappdu(Direction,D);
display_scoped_pdu_data(_Direction,D) ->
    display_prop("Unknown scoped pdu data",D).

display_pdu(Direction, P) ->
    io:format("~s PDU:~n", [Direction]),
    display_type(P#pdu.type),
    display_request_id(P#pdu.request_id),
    display_error_status(P#pdu.error_status),
    display_error_index(P#pdu.error_index),
    display_varbinds(P#pdu.varbinds).

display_type(T) ->
    display_prop("Type",T).

display_request_id(Id) ->
    display_prop("Request id",Id).

display_error_status(S) ->
    display_prop("Error status",S).

display_error_index(I) ->
    display_prop("Error index",I).

display_varbinds([H|T]) ->
    display_prop_hdr("Varbinds"),
    display_varbind(H),
    display_varbinds(T);
display_varbinds([]) ->
    ok.

display_varbind(V) when is_record(V,varbind) ->
    display_oid(V#varbind.oid),
    display_vtype(V#varbind.variabletype),
    display_value(V#varbind.value),
    display_org_index(V#varbind.org_index);
display_varbind(V) ->
    display_prop("\tVarbind",V).

display_oid(V) ->
    display_prop("\tOid",V).
    
display_vtype(V) ->
    display_prop("\t\tVariable type",V).
    
display_value(V) ->
    display_prop("\t\tValue",V).
    
display_org_index(V) ->
    display_prop("\t\tOrg index",V).

display_trappdu(Direction,P) ->
    io:format("~s TRAP-PDU:~n",[Direction]),
    display_prop("TRAP-PDU",P).

display_prop(S,no_value_to_display) ->
    io:format("\t~s: ~n",[S]);
display_prop(S,V) ->
    io:format("\t~s: ~p~n",[S,V]).


display_prop_hdr(S) ->
    io:format("\t~s:~n",[S]).


%%----------------------------------------------------------------------
%% Debug
%%----------------------------------------------------------------------

sz(L) when is_list(L) ->
    length(lists:flatten(L));
sz(B) when is_binary(B) ->
    size(B);
sz(O) ->
    {unknown_size, O}.

d(F)   -> d(F, []).
d(F,A) -> d(get(debug),F,A).

d(true,F,A) ->
    io:format("*** [~s] MGR_PS_DBG *** " ++ F ++ "~n",
	      [formated_timestamp()|A]);
d(_,_F,_A) -> 
    ok.

formated_timestamp() ->
    snmp_test_lib:formated_timestamp().
