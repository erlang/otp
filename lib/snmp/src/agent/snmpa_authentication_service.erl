%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
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
-module(snmpa_authentication_service).

-export([behaviour_info/1]).
 
behaviour_info(callbacks) ->
    [{init_check_access, 2}];
behaviour_info(_) ->
    undefined.
 

%%-----------------------------------------------------------------
%% init_check_access(Pdu, ACMData)
%% Pdu = #pdu
%% ACMData = acm_data() = {community, SecModel, Community, TDomain, TAddress} |
%%                        {v3, MsgID, SecModel, SecName, SecLevel,
%%                             ContextEngineID, ContextName, SecData}
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
