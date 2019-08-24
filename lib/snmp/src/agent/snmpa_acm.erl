%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(snmpa_acm).

-behaviour(snmpa_authentication_service).

-export([init_check_access/2, get_root_mib_view/0,
	 error2status/1,
	 validate_mib_view/2, validate_all_mib_view/2,
	 is_definitely_not_in_mib_view/2,
         invalidate_ca_cache/0]).

-include("snmp_types.hrl").
-include("STANDARD-MIB.hrl").
-include("SNMP-FRAMEWORK-MIB.hrl").
-include("SNMPv2-TM.hrl").

-define(VMODULE,"ACM").
-include("snmp_verbosity.hrl").


%%%-----------------------------------------------------------------
%%% This module implements the Access Control Model part of the
%%% multi-lingual SNMP agent.  It contains generic function not
%%% tied to a specific model, but in this version it uses VACM.
%%%
%%% Note that we don't follow the isAccessAllowed Abstract Service
%%% Interface defined in rfc2271.  We implement an optimization
%%% of that ASI.  Since the mib view is the same for all variable
%%% bindings in a PDU, there is no need to recalculate the mib
%%% view for each variable.  Therefore, one function
%%% (init_check_access/2) is used to find the mib view, and then
%%% each variable is checked against this mib view.
%%%
%%% Access checking is done in several steps.  First, the version-
%%% specific MPD (see snmpa_mpd) creates data used by VACM.  This
%%% means that the format of this data is known by both the MPD and
%%% the ACM.  When the master agent wants to check the access to a
%%% Pdu, it first calls init_check_access/2, which returns a MibView
%%% that can be used to check access of individual variables.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: init_check_access(Pdu, ACMData) ->
%%       {ok, MibView, ContextName} |
%%       {error, Reason} |
%%       {discarded, Variable, Reason}
%% Types: Pdu = #pdu
%%        ACMData = acm_data() = 
%%             {community, SecModel, Community, TDomain, TAddress} |
%%             {v3, MsgID, SecModel, SecName, SecLevel,
%%                  ContextEngineID, ContextName, SecData}
%%        Community       = string()
%%        TDomain         = ?transportDomainUdpIpv4 | ?transportDomainUdpIpv6
%%        TAddress        = ip() ++ udp() (list)
%%        MsgID           = integer() <not used>
%%        SecModel        = ?SEC_*  (see snmp_types.hrl)
%%        SecName         = string()
%%        SecLevel        = ?'SnmpSecurityLevel_*' (see SNMP-FRAMEWORK-MIB.hrl)
%%        ContextEngineID = string() <not used>
%%        ContextName     = string()
%%        SecData         = <not used>
%%        Variable        = snmpInBadCommunityNames |
%%                          snmpInBadCommunityUses |
%%                          snmpInASNParseErrs
%%        Reason          = snmp_message_decoding |
%%                          {bad_community_name, Address, Community}} |
%%                          {invalid_access, Access, Op}
%% 
%% Purpose: Called once for each Pdu.  Returns a MibView
%%          which is later used for each variable in the pdu.
%%          The authenticationFailure trap is sent (maybe) when the auth.
%%          procedure evaluates to unauthentic,
%%
%% NOTE: This function is executed in the Master agents's context
%%-----------------------------------------------------------------
init_check_access(Pdu, ACMData) ->
    case init_ca(Pdu, ACMData) of
	{ok, MibView, ContextName} ->
	    {ok, MibView, ContextName};
	{discarded, Reason} ->
	    {error, Reason};
	{authentication_failure, Variable, Reason} ->
	    handle_authentication_failure(),
	    {discarded, Variable, Reason}
    end.

error2status(noSuchView) -> authorizationError;
error2status(noAccessEntry) -> authorizationError;
error2status(noGroupName) -> authorizationError;
error2status(_) -> genErr.
     
%%-----------------------------------------------------------------
%% Func: init_ca(Pdu, ACMData) ->
%%       {ok, MibView} |
%%       {discarded, Reason} |
%%       {authentication_failure, Variable, Reason}
%%
%% error: an error response will be sent
%% discarded: no error response is sent
%% authentication_failure: no error response is sent, a trap is generated
%%-----------------------------------------------------------------
init_ca(Pdu, {community, SecModel, Community, TAddress}) ->
    TDomain = snmp_conf:mk_tdomain(snmp_target_mib:default_domain()),
    init_ca(Pdu, {community, SecModel, Community, TDomain, TAddress});
init_ca(Pdu, {community, SecModel, Community, TDomain, TAddress}) ->
    %% This is a v1 or v2c request.   Use SNMP-COMMUNITY-MIB to
    %% map the community to vacm parameters.
    ?vtrace("check access for ~n"
	    "   Pdu:            ~p~n"
	    "   Security model: ~p~n"
	    "   Community:      ~s",[Pdu,SecModel,Community]),
    ViewType = case Pdu#pdu.type of
		   'set-request' -> write;
		   _ -> read
	       end,
    ?vtrace("View type: ~p", [ViewType]),
    CaCacheKey = {Community, SecModel, TDomain, TAddress, ViewType},
    case check_ca_cache(CaCacheKey) of
        false ->
            case snmp_community_mib:community2vacm(Community, 
						   {TDomain, TAddress}) of
                {SecName, _ContextEngineId, ContextName} ->
                    %% Maybe we should check that the contextEngineID 
		    %% matches the local engineID?  
		    %% It better, since we don't impl. proxy.
                    ?vtrace("get mib view"
                            "~n   Security name: ~p"
                            "~n   Context name:  ~p",[SecName, ContextName]),
                    case snmpa_vacm:get_mib_view(ViewType, SecModel, SecName,
                                                 ?'SnmpSecurityLevel_noAuthNoPriv',
                                                 ContextName) of
                        {ok, MibView} ->
                            Res = {ok, MibView, ContextName},
                            upd_ca_cache({CaCacheKey, Res}),
                            put(sec_model, SecModel),
                            put(sec_name, SecName),
                            Res;
                        {discarded, Reason} ->
                            snmpa_mpd:inc(snmpInBadCommunityUses),
                            {discarded, Reason}
                    end;
                undefined ->
                    {authentication_failure, snmpInBadCommunityNames,
                     {bad_community_name, TDomain, TAddress, Community}}
            end;
        Res ->
            Res
    end;

init_ca(Pdu, {v3, _MsgID, SecModel, SecName, SecLevel,
	      _ContextEngineID, ContextName, _SecData}) ->
    ?vtrace("check v3 access for ~n"
	    "   Pdu:            ~p~n"
	    "   Security model: ~p~n"
	    "   Security name:  ~p~n"
	    "   Security level: ~p",[Pdu,SecModel,SecName,SecLevel]),
    ViewType = case Pdu#pdu.type of
		   'set-request' -> write;
		   _ -> read
	       end,
    ?vtrace("View type: ~p",[ViewType]),
    %% Convert the msgflag value to a ?'SnmpSecurityLevel*'
    SL = case SecLevel of
	     0 -> ?'SnmpSecurityLevel_noAuthNoPriv';
	     1 -> ?'SnmpSecurityLevel_authNoPriv';
	     3 -> ?'SnmpSecurityLevel_authPriv'
	 end,
    put(sec_model, SecModel),
    put(sec_name, SecName),
    CaCacheKey = {ViewType, SecModel, SecName, SL, ContextName},
    case check_ca_cache(CaCacheKey) of
        false ->
            case snmpa_vacm:get_mib_view(ViewType, SecModel, SecName, 
                                         SL, ContextName) of
                {ok, MibView} ->
                    Res = {ok, MibView, ContextName},
                    upd_ca_cache({CaCacheKey, Res}),
                    Res;
                Else ->
                    Else
            end;
        Res ->
            Res
    end.

check_ca_cache(Key) ->
    case get(ca_cache) of
        undefined ->
            put(ca_cache, []),
            false;
        L ->
            check_ca_cache(L, Key)
    end.

check_ca_cache([{Key, Val} | _], Key) -> Val;
check_ca_cache([_ | T], Key) -> check_ca_cache(T, Key);
check_ca_cache([], _) -> false.

upd_ca_cache(KeyVal) ->
    case get(ca_cache) of
        [A,B,C,_] -> % cache is full
            put(ca_cache, [KeyVal,A,B,C]);
        L ->
            put(ca_cache, [KeyVal|L])
    end.

invalidate_ca_cache() ->
    erase(ca_cache).


%%-----------------------------------------------------------------
%% Func: check(Res) -> {ok, MibView} | {discarded, Variable, Reason}
%% Args: Res = {ok, AccessFunc} | 
%%             {authentication_failure, Variable, Reason}
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% NOTE: The do_get MUST be executed in the Master agents's 
%%       context. Therefor, force master-agent to do a GET to 
%%       retrieve the value for snmpEnableAuthenTraps.  
%%       A user may have another impl. than default for this 
%%       variable.
%%-----------------------------------------------------------------
handle_authentication_failure() ->
    case snmpa_agent:do_get(get_root_mib_view(),
			    [#varbind{oid = ?snmpEnableAuthenTraps_instance}],
			    true, true) of
	{noError, _, [#varbind{value = ?snmpEnableAuthenTraps_enabled}]} ->
	    ?vtrace("handle_authentication_failure -> enabled", []),
	    snmpa:send_notification(snmp_master_agent, 
				    authenticationFailure, 
				    no_receiver);
	_ ->
	    ok
    end.

%%%-----------------------------------------------------------------
%%% MIB View handling
%%%-----------------------------------------------------------------

get_root_mib_view() ->
    [{[1], [], ?view_included}].

%%-----------------------------------------------------------------
%% Returns true if Oid is in the MibView, false
%% otherwise.
%% Alg: (defined in SNMP-VIEW-BASED-ACM-MIB)
%% For each family (= {SubTree, Mask, Type}), check if Oid
%% belongs to that family. For each family that Oid belong to,
%% get the longest. If two or more are longest, get the
%% lexicografically greatest. Check the type of this family. If
%% included, then Oid belongs to the MibView, otherwise it
%% does not.
%% Optimisation: Do only one loop, and kepp the largest sofar.
%% When we find a family that Oid belongs to, check if it is
%% larger than the largest.
%%-----------------------------------------------------------------
validate_mib_view(Oid, MibView) ->
    case get_largest_family(MibView, Oid, undefined) of
	{_, _, ?view_included} -> true;
	_ -> false
    end.

get_largest_family([{SubTree, Mask, Type} | T], Oid, Res) ->
    case check_mask(Oid, SubTree, snmp_view_based_acm_mib:emask2imask(Mask)) of
	true -> get_largest_family(T, Oid, add_res(length(SubTree), SubTree,
						   Type, Res));
	false -> get_largest_family(T, Oid, Res)
    end;
get_largest_family([], _Oid, Res) -> Res.

%%-----------------------------------------------------------------
%% We keep only the largest (first longest SubTree, and then 
%% lexicografically greatest) SubTree.
%%-----------------------------------------------------------------
add_res(Len, SubTree, Type, undefined) ->
    {Len, SubTree, Type};
add_res(Len, SubTree, Type, {MaxLen, _MaxS, _MaxT}) when Len > MaxLen ->
    {Len, SubTree, Type};
add_res(Len, SubTree, Type, {MaxLen, MaxS, MaxT}) when Len == MaxLen ->
    if
	SubTree > MaxS -> {Len, SubTree, Type};
	true -> {MaxLen, MaxS, MaxT}
    end;
add_res(_Len, _SubTree, _Type, MaxRes) -> MaxRes.


%% 1 in mask is exact match, 0 is wildcard.
%% If mask is shorter than SubTree, its regarded
%% as being all ones.
check_mask(_Oid, [], _Mask) -> true;
check_mask([X | Xs], [X | Ys], [1 | Ms]) ->
    check_mask(Xs, Ys, Ms);
check_mask([X | Xs], [X | Ys], []) ->
    check_mask(Xs, Ys, []);
check_mask([_X | Xs], [_Y | Ys], [0 | Ms]) ->
    check_mask(Xs, Ys, Ms);
check_mask(_, _, _) -> false.

%%-----------------------------------------------------------------
%% Validates all oids in the Varbinds list towards the MibView.
%%-----------------------------------------------------------------
validate_all_mib_view([#varbind{oid = Oid, org_index = Index} | Varbinds],
		      MibView) ->
    ?vtrace("validate_all_mib_view -> entry with"
	    "~n   Oid:    ~p"
	    "~n   Index:  ~p", [Oid, Index]),
    case validate_mib_view(Oid, MibView) of
	true -> validate_all_mib_view(Varbinds, MibView);
	false -> {false, Index}
    end;
validate_all_mib_view([], _MibView) ->
    ?vtrace("validate_all_mib_view -> done", []),
    true.

%%-----------------------------------------------------------------
%% This function is used to optimize the next operation in
%% snmpa_mib_data. If we get to a node in the tree where we can
%% determine that we are guaranteed to be outside the mibview,
%% we don't have to continue the search in the that tree (Actually
%% we will, because we only check this at leafs. But we won't
%% go into tables or subagents, and that's the important
%% optimization.) For now, this function isn't that sophisticated;
%% it just checks that there is really no family in the mibview
%% that the Oid (or any other oids with Oid as prefix) may be
%% included in. Perhaps this function easily could be more
%% intelligent.
%%-----------------------------------------------------------------
is_definitely_not_in_mib_view(Oid, [{SubTree, Mask,?view_included}|T]) ->
    case check_maybe_mask(Oid, SubTree, snmp_view_based_acm_mib:emask2imask(Mask)) of
	true -> false;
	false -> is_definitely_not_in_mib_view(Oid, T)
    end;
is_definitely_not_in_mib_view(Oid, [{_SubTree, _Mask,?view_excluded}|T]) ->
    is_definitely_not_in_mib_view(Oid, T);
is_definitely_not_in_mib_view(_Oid, []) ->
    true.
    
%%-----------------------------------------------------------------
%% As check_mask, BUT if Oid < SubTree and sofar good, we
%% return true. As Oid get larger we may decide.
%%-----------------------------------------------------------------
check_maybe_mask(_Oid, [], _Mask) -> true;
check_maybe_mask([X | Xs], [X | Ys], [1 | Ms]) ->
    check_maybe_mask(Xs, Ys, Ms);
check_maybe_mask([X | Xs], [X | Ys], []) ->
    check_maybe_mask(Xs, Ys, []);
check_maybe_mask([_X | Xs], [_Y | Ys], [0 | Ms]) ->
    check_maybe_mask(Xs, Ys, Ms);
check_maybe_mask([_X | _Xs], [_Y | _Ys], _) ->
    false;
check_maybe_mask(_, _, _) -> 
    true.
