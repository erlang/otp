%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2019. All Rights Reserved.
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
%% AES: RFC 3826
%% 

-module(snmpa_usm).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
%% Avoid warning for local function error/2 clashing with autoimported BIF.
-compile({no_auto_import,[error/2]}).
-export([
	 process_incoming_msg/4, process_incoming_msg/5, 
	 generate_outgoing_msg/5, generate_outgoing_msg/6,
	 generate_discovery_msg/4, generate_discovery_msg/5,
	 current_statsNotInTimeWindows_vb/0
	]).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-include("SNMP-USER-BASED-SM-MIB.hrl").
-include("SNMP-USM-AES-MIB.hrl").
-include("SNMPv2-TC.hrl").

-define(VMODULE,"A-USM").
-include("snmp_verbosity.hrl").
-include("snmpa_internal.hrl").


%%-----------------------------------------------------------------
%% This module implements the User Based Security Model for SNMP,
%% as defined in rfc2274.
%%-----------------------------------------------------------------

%% Columns not accessible via SNMP
-define(usmUserAuthKey, 14).
-define(usmUserPrivKey, 15).

-define(i32(Int), (Int bsr 24) band 255, (Int bsr 16) band 255, (Int bsr 8) band 255, Int band 255).
-define(i64(Int), (Int bsr 56) band 255, (Int bsr 48) band 255, (Int bsr 40) band 255, (Int bsr 32) band 255, (Int bsr 24) band 255, (Int bsr 16) band 255, (Int bsr 8) band 255, Int band 255).


%%-----------------------------------------------------------------
%% Func: process_incoming_msg(Packet, Data, SecParams, SecLevel) ->
%%       {ok, {SecEngineID, SecName, ScopedPDUBytes, SecData}} |
%%       {error, Reason} | {error, Reason, ErrorInfo}
%%       Return value may be throwed.
%% Types: Reason -> term()
%% Purpose: 
%%-----------------------------------------------------------------

process_incoming_msg(Packet, Data, SecParams, SecLevel) ->
    LocalEngineID = ?DEFAULT_LOCAL_ENGINE_ID, 
    process_incoming_msg(Packet, Data, SecParams, SecLevel, LocalEngineID).

process_incoming_msg(Packet, Data, SecParams, SecLevel, LocalEngineID) ->
    TermDiscoEnabled    = is_terminating_discovery_enabled(), 
    TermTriggerUsername = terminating_trigger_username(), 
    %% 3.2.1
    ?vtrace("process_incoming_msg -> check security parms: 3.2.1",[]),
    UsmSecParams =
	case catch snmp_pdus:dec_usm_security_parameters(SecParams) of
	    {'EXIT', Reason} ->
		inc(snmpInASNParseErrs),
		error({parseError, Reason}, []);
	    Res ->
		Res
	end,
    case UsmSecParams of
	#usmSecurityParameters{msgAuthoritativeEngineID = MsgAuthEngineID,
			       msgUserName              = TermTriggerUsername} when TermDiscoEnabled =:= true ->
	    %% Step 1 discovery message
	    ?vtrace("process_incoming_msg -> [~p] discovery step 1", 
		    [TermTriggerUsername]),
	    process_discovery_msg(MsgAuthEngineID, Data, SecLevel);
	
	#usmSecurityParameters{msgAuthoritativeEngineID = MsgAuthEngineID,
			       msgUserName = MsgUserName} ->
	    ?vlog("process_incoming_msg -> USM security parms: "
		  "~n   msgAuthEngineID: ~w"
		  "~n   userName:        ~p", [MsgAuthEngineID, MsgUserName]),
	    %% 3.2.3
	    ?vtrace("process_incoming_msg -> check engine id: 3.2.3",[]),
	    case snmp_user_based_sm_mib:is_engine_id_known(MsgAuthEngineID) of
		true ->
		    ok;
		false ->
		    SecData1 = [MsgUserName],
		    error(usmStatsUnknownEngineIDs, 
			  ?usmStatsUnknownEngineIDs_instance, %% OTP-3542
			  undefined, [{sec_data, SecData1}])
	    end,
	    %% 3.2.4
	    ?vtrace("process_incoming_msg -> retrieve usm user: 3.2.4",[]),
	    UsmUser =
		case snmp_user_based_sm_mib:get_user(MsgAuthEngineID, 
						     MsgUserName) of
		    User when element(?usmUserStatus, User) =:= ?'RowStatus_active' ->
			User;
		    {_, Name,_,_,_,_,_,_,_,_,_,_,_, RowStatus,_,_} ->
			?vdebug("process_incoming_msg -> "
				"found user ~p with wrong row status: ~p", 
				[Name, RowStatus]),
			SecData2 = [MsgUserName],
			error(usmStatsUnknownUserNames, 
			      ?usmStatsUnknownUserNames_instance, %% OTP-3542
			      undefined, [{sec_data, SecData2}]);
		    _ -> % undefined or not active user
			SecData2 = [MsgUserName],
			error(usmStatsUnknownUserNames, 
			      ?usmStatsUnknownUserNames_instance, %% OTP-3542
			      undefined, [{sec_data, SecData2}])
		end,
	    SecName = element(?usmUserSecurityName, UsmUser),
	    ?vtrace("process_incoming_msg -> securityName: ~p",[SecName]),
	    %% 3.2.5 - implicit in following checks
	    %% 3.2.6 - 3.2.7
	    ?vtrace("process_incoming_msg -> "
		    "authenticate incoming: 3.2.5 - 3.2.7"
		    "~n   ~p",[UsmUser]),
	    DiscoOrPlain = authenticate_incoming(Packet, 
						 UsmSecParams, UsmUser, 
						 SecLevel, LocalEngineID), 
	    %% 3.2.8
	    ?vtrace("process_incoming_msg -> "
		    "decrypt scoped data: 3.2.8",[]),
	    ScopedPDUBytes = 
		decrypt(Data, UsmUser, UsmSecParams, SecLevel),
	    %% 3.2.9
	    %% Means that if AuthKey/PrivKey are changed; 
	    %% the old values will be used.
	    ?vtrace("process_incoming_msg -> "
		    "AuthKey/PrivKey are changed - "
		    "use old values: 3.2.9",[]),
	    CachedSecData = {MsgUserName,
			     element(?usmUserAuthProtocol, UsmUser),
			     element(?usmUserPrivProtocol, UsmUser),
			     element(?usmUserAuthKey, UsmUser),
			     element(?usmUserPrivKey, UsmUser)},
	    {ok, {MsgAuthEngineID, SecName, ScopedPDUBytes, 
		  CachedSecData, DiscoOrPlain}}
    end.
    
%% Process a step 1 discovery message
process_discovery_msg(MsgAuthEngineID, Data, SecLevel) ->
    ?vtrace("process_discovery_msg -> entry with"
	    "~n   Data:     ~p"
	    "~n   SecLevel: ~p", [Data, SecLevel]),
    case (not snmp_misc:is_priv(SecLevel)) of
	true -> % noAuthNoPriv
	    ?vtrace("process_discovery_msg -> noAuthNoPriv", []),
	    ScopedPDUBytes = Data,
	    SecData = {"", usmNoAuthProtocol, "", usmNoPrivProtocol, ""},
	    NewData = {SecData,
		       ?usmStatsUnknownEngineIDs_instance, 
		       get_counter(usmStatsUnknownEngineIDs)}, 
	    {ok, {MsgAuthEngineID, "", ScopedPDUBytes, NewData, discovery}};
	false ->
	    error(usmStatsUnknownEngineIDs, 
		  ?usmStatsUnknownEngineIDs_instance, 
		  undefined, [{sec_data, ""}])
    end.
	    

authenticate_incoming(Packet, UsmSecParams, UsmUser, SecLevel, 
		      LocalEngineID) ->
    %% 3.2.6
    ?vtrace("authenticate_incoming -> 3.2.6", []),
    AuthProtocol = element(?usmUserAuthProtocol, UsmUser),
    #usmSecurityParameters{msgAuthoritativeEngineID    = MsgAuthEngineID,
			   msgAuthoritativeEngineBoots = MsgAuthEngineBoots,
			   msgAuthoritativeEngineTime  = MsgAuthEngineTime,
			   msgAuthenticationParameters = MsgAuthParams} = 
	UsmSecParams,
    ?vtrace("authenticate_incoming -> Sec params: "
	    "~n   MsgAuthEngineID:    ~w"
	    "~n   MsgAuthEngineBoots: ~p"
	    "~n   MsgAuthEngineTime:  ~p",
	    [MsgAuthEngineID, MsgAuthEngineBoots, MsgAuthEngineTime]),
    case snmp_misc:is_auth(SecLevel) of
	true ->
	    SecName = element(?usmUserSecurityName, UsmUser),
	    case is_auth(AuthProtocol,
			 element(?usmUserAuthKey, UsmUser),
			 MsgAuthParams,
			 Packet,
			 SecName,
			 MsgAuthEngineID,
			 MsgAuthEngineBoots, 
			 MsgAuthEngineTime,
			 LocalEngineID) of
		discovery ->
		    discovery;
		true -> 
		    plain;
		false -> 
		    error(usmStatsWrongDigests,
			  ?usmStatsWrongDigests_instance, % OTP-5464
			  SecName) 
	    end;

	false ->  % noAuth
	    plain
    end.
	    
authoritative(SecName, MsgAuthEngineBoots, MsgAuthEngineTime, LocalEngineID) ->
    ?vtrace("authoritative -> entry with"
	    "~n   SecName:            ~p"
	    "~n   MsgAuthEngineBoots: ~p"
	    "~n   MsgAuthEngineTime:  ~p", 
	    [SecName, MsgAuthEngineBoots, MsgAuthEngineTime]),
    SnmpEngineBoots = get_local_engine_boots(LocalEngineID),
    ?vtrace("authoritative -> SnmpEngineBoots: ~p", [SnmpEngineBoots]),
    SnmpEngineTime = get_local_engine_time(LocalEngineID),
    ?vtrace("authoritative -> SnmpEngineTime: ~p", [SnmpEngineTime]),
    InTimeWindow =
	if
	    SnmpEngineBoots =:= 2147483647 -> false;
	    MsgAuthEngineBoots =/= SnmpEngineBoots -> false;
	    MsgAuthEngineTime + 150 < SnmpEngineTime -> false;
	    MsgAuthEngineTime - 150 > SnmpEngineTime -> false;
	    true -> true
	end,
    case InTimeWindow of
	true -> 
	    true;
	false -> 
	    %% OTP-4090 (OTP-3542)
	    ?vinfo("NOT in time window: "
		   "~n   SecName:            ~p"
		   "~n   SnmpEngineBoots:    ~p"
		   "~n   MsgAuthEngineBoots: ~p"
		   "~n   SnmpEngineTime:     ~p"
		   "~n   MsgAuthEngineTime:  ~p",
		   [SecName,
		    SnmpEngineBoots, MsgAuthEngineBoots,
		    SnmpEngineTime, MsgAuthEngineTime]),
	    error(usmStatsNotInTimeWindows,
		  ?usmStatsNotInTimeWindows_instance,
		  SecName,
		  [{securityLevel, 1}]) % authNoPriv
    end.

non_authoritative(SecName, 
		  MsgAuthEngineID, MsgAuthEngineBoots, MsgAuthEngineTime) ->
    ?vtrace("non_authoritative -> entry with"
	    "~n   SecName:            ~p"
	    "~n   MsgAuthEngineID:    ~p"
	    "~n   MsgAuthEngineBoots: ~p"
	    "~n   MsgAuthEngineTime:  ~p", 
	    [SecName, 
	     MsgAuthEngineID, MsgAuthEngineBoots, MsgAuthEngineTime]),
    SnmpEngineBoots = get_engine_boots(MsgAuthEngineID),
    SnmpEngineTime  = get_engine_time(MsgAuthEngineID),
    LatestRecvTime  = get_engine_latest_time(MsgAuthEngineID),
    ?vtrace("non_authoritative -> "
	    "~n   SnmpEngineBoots: ~p"
	    "~n   SnmpEngineTime:  ~p"
	    "~n   LatestRecvTime:  ~p", 
	    [SnmpEngineBoots, SnmpEngineTime, LatestRecvTime]),
    UpdateLCD =
	if
	    MsgAuthEngineBoots > SnmpEngineBoots -> true;
	    ((MsgAuthEngineBoots =:= SnmpEngineBoots) andalso 
	     (MsgAuthEngineTime > LatestRecvTime)) -> true;
	    true -> false
	end,
    case UpdateLCD of
	true -> %% 3.2.7b1
	    ?vtrace("non_authoritative -> "
		    "update msgAuthoritativeEngineID: 3.2.7b1",
		    []),
	    set_engine_boots(MsgAuthEngineID, MsgAuthEngineBoots),
	    set_engine_time(MsgAuthEngineID, MsgAuthEngineTime),
	    set_engine_latest_time(MsgAuthEngineID, MsgAuthEngineTime);
	false ->
	    ok
    end,
    %% 3.2.7.b2
    ?vtrace("non_authoritative -> "
	    "check if message is outside time window: 3.2.7b2", []),
    InTimeWindow =
	if
	    SnmpEngineBoots =:= 2147483647 ->
		false;
	    MsgAuthEngineBoots < SnmpEngineBoots ->
		false;
	    ((MsgAuthEngineBoots =:= SnmpEngineBoots) andalso 
	     (MsgAuthEngineTime < (SnmpEngineTime - 150))) ->
		false;
	    true -> true
	end,
    case InTimeWindow of
	false ->
	    ?vinfo("NOT in time window: "
		   "~n   SecName:            ~p"
		   "~n   SnmpEngineBoots:    ~p"
		   "~n   MsgAuthEngineBoots: ~p"
		   "~n   SnmpEngineTime:     ~p"
		   "~n   MsgAuthEngineTime:  ~p",
		   [SecName,
		    SnmpEngineBoots, MsgAuthEngineBoots,
		    SnmpEngineTime, MsgAuthEngineTime]),
	    error(notInTimeWindow, []);
	true ->
	    %% If the previous values where all zero's this is the 
	    %% second stage discovery message
	    if
		((SnmpEngineBoots =:= 0) andalso 
		 (SnmpEngineTime  =:= 0) andalso 
		 (LatestRecvTime  =:= 0)) ->
		    ?vtrace("non_authoritative -> "
			    "[maybe] originating discovery stage 2", []),
		    discovery;
		true ->
		    true
	    end
    end.

is_auth(?usmNoAuthProtocol, _, _, _, SecName, _, _, _, _) -> % 3.2.5
    error(usmStatsUnsupportedSecLevels,
	  ?usmStatsUnsupportedSecLevels_instance, SecName); % OTP-5464
is_auth(AuthProtocol, AuthKey, AuthParams, Packet, SecName,
	MsgAuthEngineID, MsgAuthEngineBoots, MsgAuthEngineTime, 
	LocalEngineID) ->
    TermDiscoEnabled = is_terminating_discovery_enabled(), 
    TermDiscoStage2  = terminating_discovery_stage2(), 
    IsAuth = auth_in(AuthProtocol, AuthKey, AuthParams, Packet),
    ?vtrace("is_auth -> IsAuth: ~p", [IsAuth]),
    case IsAuth of
	true ->
	    %% 3.2.7
	    ?vtrace("is_auth -> "
		    "retrieve EngineBoots and EngineTime: 3.2.7",[]),
	    SnmpEngineID = LocalEngineID,
	    ?vtrace("is_auth -> SnmpEngineID: ~p", [SnmpEngineID]),
	    case MsgAuthEngineID of
		SnmpEngineID when ((MsgAuthEngineBoots =:= 0) andalso 
				   (MsgAuthEngineTime =:= 0) andalso 
				   (TermDiscoEnabled =:= true) andalso 
				   (TermDiscoStage2 =:= discovery)) -> %% 3.2.7a
		    ?vtrace("is_auth -> terminating discovery stage 2 - discovery",[]),
		    discovery;
		SnmpEngineID when ((MsgAuthEngineBoots =:= 0) andalso 
				   (MsgAuthEngineTime =:= 0) andalso 
				   (TermDiscoEnabled =:= true) andalso 
				   (TermDiscoStage2 =:= plain)) -> %% 3.2.7a
		    ?vtrace("is_auth -> terminating discovery stage 2 - plain",[]),
		    %% This will *always* result in the manager *not* 
		    %% beeing in timewindow
		    authoritative(SecName, 
				  MsgAuthEngineBoots, MsgAuthEngineTime, 
				  LocalEngineID);

		SnmpEngineID -> %% 3.2.7a
		    ?vtrace("is_auth -> we are authoritative: 3.2.7a", []),
		    authoritative(SecName, 
				  MsgAuthEngineBoots, MsgAuthEngineTime, 
				  LocalEngineID);

		_ -> %% 3.2.7b - we're non-authoritative
		    ?vtrace("is_auth -> we are non-authoritative: 3.2.7b",[]),
		    non_authoritative(SecName, 
				      MsgAuthEngineID, 
				      MsgAuthEngineBoots, MsgAuthEngineTime)
	    end;

	false -> 
	    false
    end.
				
			    
decrypt(Data, UsmUser, UsmSecParams, SecLevel) ->
    case snmp_misc:is_priv(SecLevel) of
	true ->
	    do_decrypt(Data, UsmUser, UsmSecParams);
	false ->
	    Data
    end.

do_decrypt(Data, UsmUser, UsmSecParams) ->
	    EncryptedPDU = snmp_pdus:dec_scoped_pdu_data(Data),
	    SecName      = element(?usmUserSecurityName, UsmUser),
	    PrivP        = element(?usmUserPrivProtocol, UsmUser),
	    PrivKey      = element(?usmUserPrivKey,      UsmUser), 
    ?vtrace("do_decrypt -> try decrypt with: "
	    "~n   SecName: ~p"
	    "~n   PrivP:   ~p", [SecName, PrivP]),
    try_decrypt(PrivP, PrivKey, UsmSecParams, EncryptedPDU, SecName).

try_decrypt(?usmNoPrivProtocol, _, _, _, SecName) -> % 3.2.5
    error(usmStatsUnsupportedSecLevels, 
	  ?usmStatsUnsupportedSecLevels_instance, SecName); % OTP-5464
try_decrypt(?usmDESPrivProtocol, 
	    PrivKey, UsmSecParams, EncryptedPDU, SecName) ->
    case (catch des_decrypt(PrivKey, UsmSecParams, EncryptedPDU)) of
	{ok, DecryptedData} ->
	    DecryptedData;
	Error ->
	    ?vlog("try_decrypt -> failed DES decrypt"
		  "~n   Error: ~p", [Error]),
	    error(usmStatsDecryptionErrors, 
		  ?usmStatsDecryptionErrors_instance, % OTP-5464
		  SecName)
    end;
try_decrypt(?usmAesCfb128Protocol, 
	    PrivKey, UsmSecParams,  EncryptedPDU, SecName) ->
    case (catch aes_decrypt(PrivKey, UsmSecParams, EncryptedPDU)) of
	{ok, DecryptedData} ->
	    DecryptedData;
	Error ->
	    ?vlog("try_decrypt -> failed AES decrypt"
		  "~n   Error: ~p", [Error]),
	    error(usmStatsDecryptionErrors, 
		  ?usmStatsDecryptionErrors_instance, % OTP-5464
		  SecName)
    end.


generate_outgoing_msg(Message, SecEngineID, SecName, SecData, SecLevel) ->
    LocalEngineID = ?DEFAULT_LOCAL_ENGINE_ID, 
    generate_outgoing_msg(Message, SecEngineID, SecName, SecData, SecLevel, 
			  LocalEngineID).

generate_outgoing_msg(Message, SecEngineID, SecName, SecData, SecLevel, 
		      LocalEngineID) ->
    %% 3.1.1
    ?vtrace("generate_outgoing_msg -> [3.1.1] entry with"
	    "~n   SecEngineID:   ~p"
	    "~n   SecName:       ~p"
	    "~n   SecLevel:      ~w" 
	    "~n   LocalEngineID: ~p", 
	    [SecEngineID, SecName, SecLevel, LocalEngineID]),
    {UserName, AuthProtocol, PrivProtocol, AuthKey, PrivKey} =
	case SecData of
	    [] -> % 3.1.1b
		%% Not a response - read from LCD
		case snmp_user_based_sm_mib:get_user_from_security_name(
		       SecEngineID, SecName) of
		    User when element(?usmUserStatus, User) =:=
			      ?'RowStatus_active' ->
			{element(?usmUserName, User),
			 element(?usmUserAuthProtocol, User),
			 element(?usmUserPrivProtocol, User),
			 element(?usmUserAuthKey, User),
			 element(?usmUserPrivKey, User)};
		    {_, Name,_,_,_,_,_,_,_,_,_,_,_, RowStatus,_,_} ->
			?vdebug("generate_outgoing_msg -> "
				"found not active user ~p: ~p", 
				[Name, RowStatus]),
			error(unknownSecurityName);
		    _ ->
			error(unknownSecurityName)
		end;
	    [MsgUserName] ->
		%% This means the user at the engine is unknown
		{MsgUserName, ?usmNoAuthProtocol, ?usmNoPrivProtocol, "", ""};
	    _ -> % 3.1.1a
		SecData
	end,
    %% 3.1.4
    ?vtrace("generate_outgoing_msg -> [3.1.4]"
	    "~n   UserName:     ~p"
	    "~n   AuthProtocol: ~p"
	    "~n   PrivProtocol: ~p", 
	    [UserName, AuthProtocol, PrivProtocol]),
    ScopedPduBytes = Message#message.data,
    {ScopedPduData, MsgPrivParams} =
	encrypt(ScopedPduBytes, PrivProtocol, PrivKey, SecLevel),
    SnmpEngineID = LocalEngineID, 
    ?vtrace("generate_outgoing_msg -> SnmpEngineID: ~p [3.1.6]",
	    [SnmpEngineID]),
    %% 3.1.6
    {MsgAuthEngineBoots, MsgAuthEngineTime} =
	case snmp_misc:is_auth(SecLevel) of
	    false when SecData =:= [] -> % not a response
		{0, 0}; 
	    false when UserName =:= "" -> % reply (report) to discovery step 1
		{0, 0}; 
	    true when SecEngineID =/= SnmpEngineID ->
		{get_engine_boots(SecEngineID),
		 get_engine_time(SecEngineID)};
	    _ ->
		{get_local_engine_boots(SnmpEngineID),
		 get_local_engine_time(SnmpEngineID)}
	end,
    %% 3.1.5 - 3.1.7
    ?vtrace("generate_outgoing_msg -> [3.1.5 - 3.1.7]",[]),
    UsmSecParams =
	#usmSecurityParameters{msgAuthoritativeEngineID    = SecEngineID,
			       msgAuthoritativeEngineBoots = MsgAuthEngineBoots,
			       msgAuthoritativeEngineTime  = MsgAuthEngineTime,
			       msgUserName                 = UserName,
			       msgPrivacyParameters        = MsgPrivParams},
    Message2 = Message#message{data = ScopedPduData},
    %% 3.1.8
    ?vtrace("generate_outgoing_msg -> [3.1.8]",[]),
    authenticate_outgoing(Message2, UsmSecParams,
			  AuthKey, AuthProtocol, SecLevel).


generate_discovery_msg(Message, SecEngineID, SecName, SecLevel) ->
    generate_discovery_msg(Message, SecEngineID, SecName, SecLevel, "").

generate_discovery_msg(Message, 
		       SecEngineID, SecName, SecLevel, 
		       InitialUserName) ->
   ?vtrace("generate_discovery_msg -> entry with"
	    "~n   SecEngineID:     ~p"
	    "~n   SecName:         ~p"
	    "~n   SecLevel:        ~p"
	    "~n   InitialUserName: ~p", 
	    [SecEngineID, SecName, SecLevel, InitialUserName]),
    {UserName, AuthProtocol, AuthKey, PrivProtocol, PrivKey} = 
	case SecEngineID of
	    "" ->
		%% Discovery step 1
		%% Nothing except the user name will be used in this
		%% tuple in this step, but since we need some values,
		%% we fill in proper ones just in case
		%% {"initial", usmNoAuthProtocol, "", usmNoPrivProtocol, ""}; 
		%% {"", usmNoAuthProtocol, "", usmNoPrivProtocol, ""}; 
		{InitialUserName, 
		 usmNoAuthProtocol, "", usmNoPrivProtocol, ""}; 

	    _ ->
		%% Discovery step 2
		case snmp_user_based_sm_mib:get_user_from_security_name(
		       SecEngineID, SecName) of
		    User when element(?usmUserStatus, User) =:=
			      ?'RowStatus_active' ->
			{element(?usmUserName, User),
			 element(?usmUserAuthProtocol, User),
			 element(?usmUserAuthKey, User),
			 element(?usmUserPrivProtocol, User),
			 element(?usmUserPrivKey, User)};
		    {_, Name,_,_,_,_,_,_,_,_,_,_,_, RowStatus,_,_} ->
			?vdebug("generate_discovery_msg -> "
				"found user ~p with wrong row status: ~p", 
				[Name, RowStatus]),
			error(unknownSecurityName);
		    _ ->
			error(unknownSecurityName)
		end
	end,
    ScopedPduBytes = Message#message.data,
    {ScopedPduData, MsgPrivParams} =
	encrypt(ScopedPduBytes, PrivProtocol, PrivKey, SecLevel),
    UsmSecParams =
	#usmSecurityParameters{msgAuthoritativeEngineID    = SecEngineID, 
			       msgAuthoritativeEngineBoots = 0, % Boots, 
			       msgAuthoritativeEngineTime  = 0, % Time, 
			       msgUserName                 = UserName,
			       msgPrivacyParameters        = MsgPrivParams},
    Message2 = Message#message{data = ScopedPduData},
    authenticate_outgoing(Message2, UsmSecParams,
			  AuthKey, AuthProtocol, SecLevel).

    
%% Ret: {ScopedPDU, MsgPrivParams} - both are already encoded as OCTET STRINGs
encrypt(Data, PrivProtocol, PrivKey, SecLevel) ->
    case snmp_misc:is_priv(SecLevel) of
	false -> % 3.1.4b
	    ?vtrace("encrypt -> 3.1.4b",[]),
	    {Data, []};
	true -> % 3.1.4a
	    ?vtrace("encrypt -> 3.1.4a",[]),
	    case (catch try_encrypt(PrivProtocol, PrivKey, Data)) of
		{ok, ScopedPduData, MsgPrivParams} ->
		    ?vtrace("encrypt -> encrypted - now encode tag",[]),
		    {snmp_pdus:enc_oct_str_tag(ScopedPduData), MsgPrivParams};
                {error, Reason} ->
		    ?vtrace("encrypt -> error: "
			    "~n   Reason: ~p", [Reason]),
                    error(Reason);
 		Error ->
		    ?vtrace("encrypt -> other: "
			    "~n   Error: ~p", [Error]),
		    error(encryptionError)
	    end
    end.

try_encrypt(?usmNoPrivProtocol, _PrivKey, _Data) -> % 3.1.2
    error(unsupportedSecurityLevel);
try_encrypt(?usmDESPrivProtocol, PrivKey, Data) ->
    des_encrypt(PrivKey, Data);
try_encrypt(?usmAesCfb128Protocol, PrivKey, Data) ->
    aes_encrypt(PrivKey, Data).


authenticate_outgoing(Message, UsmSecParams, 
		      AuthKey, AuthProtocol, SecLevel) ->
    Message2 = 
	case snmp_misc:is_auth(SecLevel) of
	    true ->
		auth_out(AuthProtocol, AuthKey, Message, UsmSecParams);
	    false ->
		set_msg_auth_params(Message, UsmSecParams)
	end,
    ?vtrace("authenticate_outgoing -> encode message only",[]),
    snmp_pdus:enc_message_only(Message2).


%%-----------------------------------------------------------------
%% Auth and priv algorithms
%%-----------------------------------------------------------------
auth_in(AuthProtocol, AuthKey, AuthParams, Packet) ->
    snmp_usm:auth_in(AuthProtocol, AuthKey, AuthParams, Packet).

auth_out(AuthProtocol, AuthKey, Message, UsmSecParams) ->
    snmp_usm:auth_out(AuthProtocol, AuthKey, Message, UsmSecParams).

set_msg_auth_params(Message, UsmSecParams) ->
    snmp_usm:set_msg_auth_params(Message, UsmSecParams, []).

des_encrypt(PrivKey, Data) ->
    snmp_usm:des_encrypt(PrivKey, Data, fun get_des_salt/0).

des_decrypt(PrivKey, UsmSecParams, EncData) ->
    #usmSecurityParameters{msgPrivacyParameters = PrivParms} = UsmSecParams,
    snmp_usm:des_decrypt(PrivKey, PrivParms, EncData).

get_des_salt() ->
    SaltInt = 
	case catch ets:update_counter(snmp_agent_table, usm_des_salt, 1) of
	    N when N =< 4294967295 ->
		N;
	    N when is_integer(N) -> % wrap
		ets:insert(snmp_agent_table, {usm_des_salt, 0}),
		0;
	    _ -> % it doesn't exist, initialize
                ?SNMP_RAND_SEED(),
                %% rand:seed(exrop,
                %%           {erlang:phash2([node()]),
                %%            erlang:monotonic_time(),
                %%            erlang:unique_integer()}),
		R = rand:uniform(4294967295),
		ets:insert(snmp_agent_table, {usm_des_salt, R}),
		R
	end,
    EngineBoots = snmp_framework_mib:get_engine_boots(),
    [?i32(EngineBoots), ?i32(SaltInt)].

aes_encrypt(PrivKey, Data) ->
    EngineBoots = snmp_framework_mib:get_engine_boots(),
    EngineTime  = snmp_framework_mib:get_engine_time(),
    snmp_usm:aes_encrypt(PrivKey, Data, fun get_aes_salt/0, 
			 EngineBoots, EngineTime).

aes_decrypt(PrivKey, UsmSecParams, EncData) ->
    #usmSecurityParameters{msgPrivacyParameters        = PrivParams,
			   msgAuthoritativeEngineTime  = EngineTime,
			   msgAuthoritativeEngineBoots = EngineBoots} =
	UsmSecParams,
    snmp_usm:aes_decrypt(PrivKey, PrivParams, EncData, 
			 EngineBoots, EngineTime).

get_aes_salt() ->
    SaltInt = 
	case catch ets:update_counter(snmp_agent_table, usm_aes_salt, 1) of
	    N when N =< 36893488147419103231  ->
		N;
	    N when is_integer(N) -> % wrap
		ets:insert(snmp_agent_table, {usm_aes_salt, 0}),
		0;
	    _ -> % it doesn't exist, initialize
                ?SNMP_RAND_SEED(),
                %% rand:seed(exrop,
                %%           {erlang:phash2([node()]),
                %%            erlang:monotonic_time(),
                %%            erlang:unique_integer()}),
		R = rand:uniform(36893488147419103231),
		ets:insert(snmp_agent_table, {usm_aes_salt, R}),
		R
	end,
    [?i64(SaltInt)].



%%-----------------------------------------------------------------
%% Discovery wrapper functions
%%-----------------------------------------------------------------

is_terminating_discovery_enabled() ->
    snmpa_agent:is_terminating_discovery_enabled().

terminating_discovery_stage2() ->
    snmpa_agent:terminating_discovery_stage2().

terminating_trigger_username() ->
    snmpa_agent:terminating_trigger_username().

current_statsNotInTimeWindows_vb() ->
    #varbind{oid          = ?usmStatsNotInTimeWindows_instance,
	     variabletype = 'Counter32',
	     value        = get_counter(usmStatsNotInTimeWindows)}.



%%-----------------------------------------------------------------
%% Future profing...
%%-----------------------------------------------------------------

get_local_engine_boots(_LocalEngineID) ->
    snmp_framework_mib:get_engine_boots().

get_local_engine_time(_LocalEngineID) ->
    snmp_framework_mib:get_engine_time().



%%-----------------------------------------------------------------
%% We cache the local values of all non-auth engines we know.
%% Keep the values in the snmp_agent_table.
%% See section 2.3 of the RFC.
%%-----------------------------------------------------------------
get_engine_boots(SnmpEngineID) ->
    case ets:lookup(snmp_agent_table, {usm_eboots, SnmpEngineID}) of
	[{_Key, Boots}] -> Boots;
	_ -> 0
    end.

get_engine_time(SnmpEngineID) ->
    case ets:lookup(snmp_agent_table, {usm_etime, SnmpEngineID}) of
	[{_Key, Diff}] -> snmp_misc:now(sec) - Diff;
	_ -> 0
    end.
	    
get_engine_latest_time(SnmpEngineID) ->
    case ets:lookup(snmp_agent_table, {usm_eltime, SnmpEngineID}) of
	[{_Key, Time}] -> Time;
	_ -> 0
    end.
	    

set_engine_boots(SnmpEngineID, EngineBoots) ->
    ets:insert(snmp_agent_table, {{usm_eboots, SnmpEngineID}, EngineBoots}).

set_engine_time(SnmpEngineID, EngineTime) ->
    Diff = snmp_misc:now(sec) - EngineTime,
    ets:insert(snmp_agent_table, {{usm_etime, SnmpEngineID}, Diff}).

set_engine_latest_time(SnmpEngineID, EngineTime) ->
    ets:insert(snmp_agent_table, {{usm_eltime, SnmpEngineID}, EngineTime}).


%%-----------------------------------------------------------------
%% Utility functions
%%-----------------------------------------------------------------
-spec error(term()) -> no_return().
error(Reason) ->
    throw({error, Reason}).

-spec error(term(), term()) -> no_return().
error(Reason, ErrorInfo) ->
    throw({error, Reason, ErrorInfo}).

-spec error(term(), term(), term()) -> no_return().
error(Variable, Oid, SecName) ->
    error(Variable, Oid, SecName, []).

-spec error(term(), term(), term(), [term()]) -> no_return().
error(Variable, Oid, SecName, Opts) ->
    Val = inc(Variable),
    ErrorInfo = {#varbind{oid = Oid,
			  variabletype = 'Counter32',
			  value = Val},
		 SecName,
		 Opts},
    throw({error, Variable, ErrorInfo}).

inc(Name) -> ets:update_counter(snmp_agent_table, Name, 1).

get_counter(Name) ->
    case (catch ets:lookup(snmp_agent_table, Name)) of
	[{_, Val}] ->
	    Val;
	_ ->
	    0
    end.
