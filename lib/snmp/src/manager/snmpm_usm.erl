%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2024. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% This module implements the User Based Security Model for SNMP,
%% as defined in rfc2274.
%% 
%% AES: RFC 3826
%% 
%%-----------------------------------------------------------------

-module(snmpm_usm).
-moduledoc false.

%% Avoid warning for local functions error/1,2,3 clashing with
%% autoimported BIFs.
-compile({no_auto_import, [error/1, error/2, error/3]}).

-export([init/0, 
	 reset/0, 
	 process_incoming_msg/4, generate_outgoing_msg/5]).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-include("snmpm_usm.hrl").
-include("SNMP-USER-BASED-SM-MIB.hrl").
-include("SNMP-USM-AES-MIB.hrl").
% -include("SNMPv2-TC.hrl").

-define(VMODULE,"M-USM").
-include("snmp_verbosity.hrl").


%%-----------------------------------------------------------------

-define(i32(Int), (Int bsr 24) band 255, (Int bsr 16) band 255, (Int bsr 8) band 255, Int band 255).
-define(i64(Int), (Int bsr 56) band 255, (Int bsr 48) band 255, (Int bsr 40) band 255, (Int bsr 32) band 255, (Int bsr 24) band 255, (Int bsr 16) band 255, (Int bsr 8) band 255, Int band 255).


init() ->
    init_counters().


%%-----------------------------------------------------------------
%% Func: process_incoming_msg(Packet, Data, SecParams, SecLevel) ->
%%       {ok, {SecEngineID, SecName, ScopedPDUBytes, SecData}} |
%%       {error, Reason} | {error, Reason, ErrorInfo}
%%       Return value may be thrown.
%% Types: Reason -> term()
%% Purpose: 
%%-----------------------------------------------------------------
process_incoming_msg(Packet, Data, SecParams, SecLevel) ->
    %% 3.2.1
    ?vtrace("process_incoming_msg -> [3.2.1] check security parms",[]),
    UsmSecParams =
	case (catch snmp_pdus:dec_usm_security_parameters(SecParams)) of
	    {'EXIT', Reason} ->
                ?vlog("Failed decode USM security parameters: "
                      "~n      ~p", [Reason]),
		inc(snmpInASNParseErrs),
		error({parseError, Reason}, []);
	    Res ->
		Res
	end,

    %% Part of 3.2.2
    #usmSecurityParameters{msgAuthoritativeEngineID = MsgAuthEngineID,
			   msgUserName = MsgUserName} = UsmSecParams,
    ?vlog("process_incoming_msg -> [3.2.2]"
	  "~n      authEngineID: ~p"
	  "~n      userName:     ~p", [MsgAuthEngineID, MsgUserName]),

    %% 3.2.3 (b)
    ?vtrace("process_incoming_msg -> [3.2.3-b] check engine id",[]),
    case snmpm_config:is_usm_engine_id_known(MsgAuthEngineID) of
	true ->
	    ok;
	false ->
            ?vlog("Unknown USM engine id: "
                  "~n      ~p", [MsgAuthEngineID]),
	    SecData1 = [MsgUserName],
	    error(usmStatsUnknownEngineIDs, 
		  ?usmStatsUnknownEngineIDs_instance,
		  undefined, [{sec_data, SecData1}])
    end,

    %% 3.2.4
    ?vtrace("process_incoming_msg -> [3.2.4] retrieve usm user",[]),
    SecUser =
	case snmpm_config:get_usm_user(MsgAuthEngineID, MsgUserName) of
	    {ok, User} ->
		User;
	    _ -> % undefined user
                ?vlog("Unknown USM user: "
                      "~n      Auth Engine ID: ~p"
                      "~n      User Name:      ~p", [MsgAuthEngineID, MsgUserName]),
		SecData2 = [MsgUserName],
		error(usmStatsUnknownUserNames, 
		      ?usmStatsUnknownUserNames_instance, %% OTP-3542
		      undefined, [{sec_data, SecData2}])
	end,

    %% 3.2.5 - implicit in following checks
    %% 3.2.6 - 3.2.7
    ?vtrace("process_incoming_msg -> "
	    "[3.2.5 - 3.2.7] authenticate incoming",[]),
    authenticate_incoming(Packet, UsmSecParams, SecUser, SecLevel),

    %% 3.2.8
    ?vtrace("process_incoming_msg -> [3.2.8] decrypt scoped data",[]),
    ScopedPDUBytes = decrypt(Data, SecUser, UsmSecParams, SecLevel),

    %% 3.2.9
    %% Means that if AuthKey/PrivKey are changed; the old values
    %% will be used.
    CachedSecData = {MsgUserName,
		     SecUser#usm_user.auth,
		     SecUser#usm_user.auth_key,
		     SecUser#usm_user.priv,
		     SecUser#usm_user.priv_key},
    SecName = SecUser#usm_user.sec_name,
    {ok, {MsgAuthEngineID, SecName, ScopedPDUBytes, CachedSecData}}.
    

authenticate_incoming(Packet, UsmSecParams, UsmUser, SecLevel) ->
    %% 3.2.6
    ?vtrace("authenticate incoming -> 3.2.6"
            "~n      SecLevel: ~p", [SecLevel]),
    #usmSecurityParameters{msgAuthoritativeEngineID    = MsgAuthEngineID,
			   msgAuthoritativeEngineBoots = MsgAuthEngineBoots,
			   msgAuthoritativeEngineTime  = MsgAuthEngineTime,
			   msgAuthenticationParameters = MsgAuthParams} =
	UsmSecParams,
    ?vtrace("authenticate_incoming -> Sec params: "
	    "~n      MsgAuthEngineID:    ~w"
	    "~n      MsgAuthEngineBoots: ~p"
	    "~n      MsgAuthEngineTime:  ~p",
	    [MsgAuthEngineID, MsgAuthEngineBoots, MsgAuthEngineTime]),
    case snmp_misc:is_auth(SecLevel) of
	true ->
            ?vtrace("authenticate_incoming -> authenticate"),
	    SecName = UsmUser#usm_user.sec_name,
	    case is_auth(UsmUser#usm_user.auth, 
			 UsmUser#usm_user.auth_key,
			 MsgAuthParams,
			 Packet,
			 SecName,
			 MsgAuthEngineID,
			 MsgAuthEngineBoots, 
			 MsgAuthEngineTime) of
		true -> 
		    ok;
		false -> 
                    ?vlog("Not authenticated: "
                          "~n      Sec Name: ~p", [SecName]),
		    error(usmStatsWrongDigests,
			  ?usmStatsWrongDigests_instance, SecName)
	    end;
	false ->  % noAuth
            ?vtrace("authenticate_incoming -> don't authenticate"),
	    ok
    end.


	    
is_auth(usmNoAuthProtocol, _, _, _, SecName, _, _, _) -> % 3.2.5
    ?vlog("auth: Unsupported security levels: "
          "~n      Sec Name: ~p", [SecName]),
    error(usmStatsUnsupportedSecLevels,
	  ?usmStatsUnsupportedSecLevels_instance, SecName);
is_auth(AuthProtocol, AuthKey, AuthParams, Packet, SecName,
	MsgAuthEngineID, MsgAuthEngineBoots, MsgAuthEngineTime) ->
    case auth_in(AuthProtocol, AuthKey, AuthParams, Packet) of
	true ->
	    %% 3.2.7
	    ?vtrace("retrieve EngineBoots and EngineTime: 3.2.7",[]),
	    SnmpEngineID = get_engine_id(),
	    ?vtrace("SnmpEngineID: ~p",[SnmpEngineID]),
	    case MsgAuthEngineID of
		SnmpEngineID -> %% 3.2.7a
		    ?vtrace("we are authoritative: 3.2.7a",[]),
		    SnmpEngineBoots = get_engine_boots(),
		    ?vtrace("SnmpEngineBoots: ~p",[SnmpEngineBoots]),
		    SnmpEngineTime = get_engine_time(),
		    ?vtrace("SnmpEngineTime: ~p",[SnmpEngineTime]),
		    InTimeWindow =
			if
			    SnmpEngineBoots == 2147483647 -> false;
			    MsgAuthEngineBoots /= SnmpEngineBoots -> false;
			    MsgAuthEngineTime + 150 < SnmpEngineTime -> false;
			    MsgAuthEngineTime - 150 > SnmpEngineTime -> false;
			    true -> true
			end,
		    case InTimeWindow of
			true -> 
			    true;
			%% OTP-4090 (OTP-3542)
			false -> 
                            ?vlog("Not in time window: "
                                  "~n      Sec Name: ~p", [SecName]),
			    error(usmStatsNotInTimeWindows,
				  ?usmStatsNotInTimeWindows_instance,
				  SecName,
				  [{securityLevel, 1}]) % authNoPriv
		    end;
		_ -> %% 3.2.7b - we're non-authoritative
		    ?vtrace("is_auth -> we are non-authoritative: 3.2.7b"),
		    SnmpEngineBoots = get_engine_boots(MsgAuthEngineID),
		    ?vtrace("is_auth -> SnmpEngineBoots: ~p", [SnmpEngineBoots]),
		    SnmpEngineTime = get_engine_time(MsgAuthEngineID),
		    ?vtrace("is_auth -> SnmpEngineTime: ~p", [SnmpEngineTime]),
		    LatestRecvTime = get_engine_latest_time(MsgAuthEngineID),
		    ?vtrace("is_auth -> LatestRecvTime: ~p", [LatestRecvTime]),
		    UpdateLCD =
			if
			    MsgAuthEngineBoots > SnmpEngineBoots -> true;
			    MsgAuthEngineBoots == SnmpEngineBoots,
			    MsgAuthEngineTime > LatestRecvTime -> true;
			    true -> false
			end,
		    ?vtrace("is_auth -> UpdateLCD: ~p", [UpdateLCD]),
		    case UpdateLCD of
			true -> %% 3.2.7b1
			    ?vtrace("is_auth -> "
                                    "[3.2.7b1] update msgAuthoritativeEngineID"),
			    set_engine_boots(MsgAuthEngineID,
					     MsgAuthEngineBoots),
			    set_engine_time(MsgAuthEngineID,
					    MsgAuthEngineTime),
			    set_engine_latest_time(MsgAuthEngineID,
						   MsgAuthEngineTime);
			false ->
			    ok
		    end,
		    %% 3.2.7.b2
		    ?vtrace("is_auth -> [3.2.7b2] is message outside time window"),
		    InTimeWindow =
			if
			    SnmpEngineBoots == 2147483647 ->
				{false, [{engine, SnmpEngineID}, 
					 {boots,  at_max}]};
			    MsgAuthEngineBoots < SnmpEngineBoots ->
				{false, [{engine, MsgAuthEngineID},
					 {boots,  MsgAuthEngineBoots}]};
			    MsgAuthEngineBoots == SnmpEngineBoots,
			    MsgAuthEngineTime < (SnmpEngineTime - 150) ->
				{false, [{engine, MsgAuthEngineID},
					 {time,   MsgAuthEngineTime}]};
			    true -> true
			end,
		    ?vtrace("is_auth -> InTimeWindow: ~p", [InTimeWindow]),
		    case InTimeWindow of
			{false, Reason} ->
			    ?vinfo("not in time window[3.2.7b2]: ~p", 
				   [Reason]),
			    error(notInTimeWindow, Reason);
			true ->
			    ok
		    end,
		    true
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

do_decrypt(Data, #usm_user{sec_name = SecName,
			   priv     = PrivP,
			   priv_key = PrivKey}, 
	   UsmSecParams) ->
    EncryptedPDU = snmp_pdus:dec_scoped_pdu_data(Data),
    try_decrypt(PrivP, PrivKey, UsmSecParams, EncryptedPDU, SecName).

try_decrypt(usmNoPrivProtocol, _, _, _, SecName) -> % 3.2.5
    ?vlog("decrypt: Unsupported security levels: "
          "~n      Sec Name: ~p", [SecName]),
    error(usmStatsUnsupportedSecLevels, 
	  ?usmStatsUnsupportedSecLevels_instance, SecName);
try_decrypt(usmDESPrivProtocol, 
	    PrivKey, UsmSecParams, EncryptedPDU, SecName) ->
    #usmSecurityParameters{msgPrivacyParameters = MsgPrivParams} = UsmSecParams,
    case (catch des_decrypt(PrivKey, MsgPrivParams, EncryptedPDU)) of
	{ok, DecryptedData} ->
	    DecryptedData;
	_Error ->
            ?vlog("USM DES decrypt failed: "
                  "~n      Sec Name: ~p"
                  "~n      Error:    ~p", [SecName, _Error]),
	    error(usmStatsDecryptionErrors, 
		  ?usmStatsDecryptionErrors, SecName)
    end;
try_decrypt(usmAesCfb128Protocol, 
	    PrivKey, UsmSecParams,  EncryptedPDU, SecName) ->
    case (catch aes_decrypt(PrivKey, UsmSecParams, EncryptedPDU)) of
	{ok, DecryptedData} ->
	    DecryptedData;
	_Error ->
            ?vlog("USM AES-CFB-128 decrypt failed: "
                  "~n      Sec Name: ~p"
                  "~n      Error:    ~p", [SecName, _Error]),
	    error(usmStatsDecryptionErrors, 
		  ?usmStatsDecryptionErrors, SecName)
    end.
    

%%-----------------------------------------------------------------
%% Func: process_outgoing_msg(Message, SecEngineID, SecName, 
%%                            SecData, SecLevel) ->
%%       {ok, {SecEngineID, SecName, ScopedPDUBytes, SecData}} |
%%       {error, Reason} | {error, Reason, ErrorInfo}
%%       Return value may be thrown.
%% Types: Reason -> term()
%% Purpose: 
%%-----------------------------------------------------------------
generate_outgoing_msg(Message, SecEngineID, SecName, SecData, SecLevel) ->
    %% 3.1.1
    ?vtrace("generate_outgoing_msg -> entry (3.1.1) when"
            "~n      SecEngineID: ~p"
            "~n      SecName:     ~p"
            "~n      SecData:     ~p"
            "~n      SecLevel:    ~p", [SecEngineID, SecName, SecData, SecLevel]),
    {UserName, AuthProtocol, AuthKey, PrivProtocol, PrivKey} = 
	case SecData of
	    [] -> % 3.1.1b
                ?vdebug("generate_outgoing_msg -> "
                        "[(3.1.1b] not a response - read from LCD"),
		%% Not a response - read from LCD
		case snmpm_config:get_usm_user_from_sec_name(SecEngineID,
							     SecName) of
		    {ok, User} ->
 			{User#usm_user.name, 
 			 User#usm_user.auth,
			 User#usm_user.auth_key,
			 User#usm_user.priv,
 			 User#usm_user.priv_key};
		    _ ->
                        ?vlog("[outgoing] Failed get USM User from sec name: "
                              "~n      Sec Engine ID: ~p"
                              "~n      Sec Name:      ~p", [SecEngineID, SecName]),
			error(unknownSecurityName)
		end;
	    [MsgUserName] ->
		%% This means the user at the engine is unknown
		{MsgUserName, usmNoAuthProtocol, "", usmNoPrivProtocol, ""};
	    _ -> % 3.1.1a
		SecData
	end,
    SnmpEngineID = get_engine_id(),
    ?vtrace("generate_outgoing_msg -> [3.1.6] SnmpEngineID: ~p", [SnmpEngineID]),
    %% 3.1.6
    {MsgAuthEngineBoots, MsgAuthEngineTime} =
	case snmp_misc:is_auth(SecLevel) of
	    false when SecData == [] -> % not a response
		{0, 0}; 
	    true when SecEngineID /= SnmpEngineID ->
                ?vtrace("generate_outgoing_msg -> "
                        "~n      SecEngineID:  ~p"
                        "~n      SnmpEngineID: ~p",
                        [SecEngineID, SnmpEngineID]),
		{get_engine_boots(SecEngineID), get_engine_time(SecEngineID)};
	    _ ->
                ?vtrace("generate_outgoing_msg -> use local boots and time"),
		{get_engine_boots(), get_engine_time()}
	end,
    %% 3.1.4
    ?vtrace("generate_outgoing_msg -> [3.1.4]"),
    ScopedPduBytes = Message#message.data,
    {ScopedPduData, MsgPrivParams} = encrypt(ScopedPduBytes,
                                             PrivProtocol, PrivKey,
                                             SecLevel, MsgAuthEngineBoots,
        MsgAuthEngineTime),
    %% 3.1.5 - 3.1.7
    ?vtrace("generate_outgoing_msg -> [3.1.5 - 3.1.7]",[]),
    UsmSecParams =
	#usmSecurityParameters{msgAuthoritativeEngineID = SecEngineID,
			       msgAuthoritativeEngineBoots = MsgAuthEngineBoots,
			       msgAuthoritativeEngineTime = MsgAuthEngineTime,
			       msgUserName = UserName,
			       msgPrivacyParameters = MsgPrivParams},
    Message2 = Message#message{data = ScopedPduData},
    %% 3.1.8
    ?vtrace("generate_outgoing_msg -> (3.1.8)",[]),
    authenticate_outgoing(Message2, UsmSecParams,
			  AuthKey, AuthProtocol, SecLevel).


%% Ret: {ScopedPDU, MsgPrivParams} - both are already encoded as OCTET STRINGs
encrypt(Data, PrivProtocol, PrivKey, SecLevel, EngineBoots, EngineTime) ->
    case snmp_misc:is_priv(SecLevel) of
	false -> % 3.1.4b
            ?vtrace("encrypt -> [3.1.4b]"),
	    {Data, []};
	true -> % 3.1.4a
            ?vtrace("encrypt -> [3.1.4a]"),
	    case (catch try_encrypt(PrivProtocol, PrivKey, Data, EngineBoots, EngineTime)) of
		{ok, ScopedPduData, MsgPrivParams} ->
		    {snmp_pdus:enc_oct_str_tag(ScopedPduData), MsgPrivParams};
		{error, Reason} ->
                    ?vlog("try encrypt error: "
                          "~n      Protocol: ~p"
                          "~n      Reason:   ~p", [PrivProtocol, Reason]),
		    error(Reason);
		_Error ->
                    ?vlog("try encrypt unexpected failure: "
                          "~n      Protocol: ~p"
                          "~n      Error:    ~p", [PrivProtocol, _Error]),
		    error(encryptionError)
	    end
    end.

try_encrypt(usmNoPrivProtocol, _PrivKey, _Data, _EngineBoots, _EngineTime) -> % 3.1.2
    ?vlog("encrypt: Unsupported security levels: "
          "~n      Engine Boots: ~p"
          "~n      Engine Time:  ~p", [_EngineBoots, _EngineTime]),
    error(unsupportedSecurityLevel);
try_encrypt(usmDESPrivProtocol, PrivKey, Data, _EngineBoots, _EngineTime) ->
    des_encrypt(PrivKey, Data);
try_encrypt(usmAesCfb128Protocol, PrivKey, Data, EngineBoots, EngineTime) ->
    aes_encrypt(PrivKey, Data, EngineBoots, EngineTime).

authenticate_outgoing(Message, UsmSecParams, 
		      AuthKey, AuthProtocol, SecLevel) ->
    Message2 = 
	case snmp_misc:is_auth(SecLevel) of
	    true ->
		auth_out(AuthProtocol, AuthKey, Message, UsmSecParams);
	    false ->
		set_msg_auth_params(Message, UsmSecParams)
	end,
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

des_decrypt(PrivKey, MsgPrivParams, EncData) ->
    snmp_usm:des_decrypt(PrivKey, MsgPrivParams, EncData).

get_des_salt() ->
    SaltInt     = snmpm_config:incr_counter(usm_des_salt, 1),
    EngineBoots = get_engine_boots(),
    [?i32(EngineBoots), ?i32(SaltInt)].

aes_encrypt(PrivKey, Data, EngineBoots, EngineTime) ->
    snmp_usm:aes_encrypt(PrivKey, Data, fun get_aes_salt/0, EngineBoots, EngineTime).

aes_decrypt(PrivKey, UsmSecParams, EncData) ->
    #usmSecurityParameters{msgPrivacyParameters        = MsgPrivParams,
			   msgAuthoritativeEngineTime  = EngineTime,
			   msgAuthoritativeEngineBoots = EngineBoots} =
	UsmSecParams,
    snmp_usm:aes_decrypt(PrivKey, MsgPrivParams, EncData, 
			 EngineBoots, EngineTime).

get_aes_salt() ->
    SaltInt     = snmpm_config:incr_counter(usm_aes_salt, 1),
    [?i64(SaltInt)].

%%-----------------------------------------------------------------

get_engine_id() ->
    {ok, EngineID} = snmpm_config:get_engine_id(),
    EngineID.

get_engine_boots() ->
    {ok, Boots} = snmpm_config:get_engine_boots(),
    Boots.

get_engine_time() ->
    {ok, Diff} = snmpm_config:get_engine_time(),
    Diff.


%%-----------------------------------------------------------------
%% We cache the local values of all non-auth engines we know.
%% See section 2.3 (Time Synchronization) of the RFC.
%%-----------------------------------------------------------------
get_engine_boots(SnmpEngineID) ->
    {ok, Boots} = snmpm_config:get_usm_eboots(SnmpEngineID),
    Boots.

get_engine_time(SnmpEngineID) ->
    {ok, Diff} = snmpm_config:get_usm_etime(SnmpEngineID),
    Diff.
	    
get_engine_latest_time(SnmpEngineID) ->
    {ok, Time} = snmpm_config:get_usm_eltime(SnmpEngineID),
    Time.
	    

set_engine_boots(SnmpEngineID, EngineBoots) ->
    snmpm_config:set_usm_eboots(SnmpEngineID, EngineBoots).

set_engine_time(SnmpEngineID, EngineTime) ->
    Diff = snmp_misc:now(sec) - EngineTime,
    snmpm_config:set_usm_etime(SnmpEngineID, Diff).

set_engine_latest_time(SnmpEngineID, EngineTime) ->
    snmpm_config:set_usm_eltime(SnmpEngineID, EngineTime).


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
    ErrorInfo = {#varbind{oid          = Oid,
			  variabletype = 'Counter32',
			  value        = Val},
		 SecName,
		 Opts},
    throw({error, Variable, ErrorInfo}).


%%-----------------------------------------------------------------

init_counters() ->
    F = fun(Counter) -> snmpm_config:maybe_cre_stats_counter(Counter, 0) end,
    lists:map(F, counters()).

reset() ->
    F = fun(Counter) -> snmpm_config:reset_stats_counter(Counter) end,
    lists:map(F, counters()).

counters() ->
    [usmStatsUnsupportedSecLevels,
     usmStatsNotInTimeWindows,
     usmStatsUnknownUserNames,
     usmStatsUnknownEngineIDs,
     usmStatsWrongDigests,
     usmStatsDecryptionErrors].

inc(Name) -> snmpm_config:incr_stats_counter(Name, 1).

