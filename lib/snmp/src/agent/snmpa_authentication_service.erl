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
-module(snmpa_authentication_service).

-export_type([
              acm_data/0
             ]).

-type acm_data() :: {community,
                     SecModel  :: 0 | 1 | 2 | 3, % any | v1 | v2c | v3
                     Community :: string(),
                     %% Oids for either:
                     %%      transportDomainUdpIpv4 | transportDomainUdpIpv6
                     TDomain   :: snmp:oid(),
                     TAddress  :: [non_neg_integer()]} |
                    {v3,
                     MsgID           :: integer(),
                     SecModel        :: 0 | 1 | 2 | 3, % any | v1 | v2c | v3
                     SecName         :: string(),
                     %% noAuthNoPriv | authNoPriv | authPriv
                     SecLevel        :: 1 | 2 | 3,
                     ContextEngineID :: string(),
                     ContextName     :: string(),
                     SecData         :: term()}.


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
%%        Reason          = {bad_community_name, Address, Community}}
%%
%% Purpose: Called once for each Pdu.  Returns a MibView
%%          which is later used for each variable in the pdu.
%%          The authenticationFailure trap is sent (maybe) when the auth.
%%          procedure evaluates to unauthentic,
%%
%% NOTE: This function is executed in the Master agents's context
%%-----------------------------------------------------------------

-callback init_check_access(Pdu, ACMData) ->
    {ok, MibView, ContextName} |
    {error, Reason} |
    {discarded, Variable, Reason} when
      Pdu         :: snmp:pdu(),
      ACMData     :: acm_data(),
      MibView     :: snmp_view_based_acm_mib:mibview(),
      ContextName :: string(),
      Reason      :: term(),
      Variable    :: snmpInBadCommunityNames.
