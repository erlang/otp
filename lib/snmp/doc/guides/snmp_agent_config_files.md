<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2023-2025. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Definition of Agent Configuration Files

All configuration data must be included in configuration files that are located
in the configuration directory. The name of this directory is given in the
`config_dir` configuration parameter. These files are read at start-up, and are
used to initialize the SNMPv2-MIB or STANDARD-MIB, SNMP-FRAMEWORK-MIB,
SNMP-MPD-MIB, SNMP-VIEW-BASED-ACM-MIB, SNMP-COMMUNITY-MIB,
SNMP-USER-BASED-SM-MIB (adjusted according to SNMP-USM-HMAC-SHA2-MIB),
SNMP-TARGET-MIB and SNMP-NOTIFICATION-MIB (refer to the
[Management of the Agent](snmp_agent_funct_descr.md#management) for a
description of the MIBs).

The files are:

- `agent.conf`: see
  [Agent Information](snmp_agent_config_files.md#agent_information)
- `standard.conf`: see
  [System Information](snmp_agent_config_files.md#system_information)
- `context.conf`: see [Contexts](snmp_agent_config_files.md#context)
- `community.conf`: see [Communities](snmp_agent_config_files.md#community)
- `target_addr.conf`: see
  [Target Address Definitions](snmp_agent_config_files.md#target_addr)
- `target_params.conf`: see
  [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
- `vacm.conf`: see [MIB Views for VACM](snmp_agent_config_files.md#vacm)
- `usm.conf`: see [Security data for USM](snmp_agent_config_files.md#usm)
- `notify.conf`: see [Notify Definitions](snmp_agent_config_files.md#notify)

The directory where the configuration files are found is given as a parameter to
the agent.

The entry format in all files are Erlang terms, separated by a '_._' and a
_newline_. In the following sections, the formats of these terms are described.
Comments may be specified as ordinary Erlang comments.

Syntax errors in these files are discovered and reported with the function
`config_err/2` of the error report module at start-up.

[](){: #agent_information }

## Agent Information

The agent information should be stored in a file called `agent.conf`.

Each entry is a tuple of size two:

`{AgentVariable, Value}.`

- `AgentVariable` is one of the variables in SNMP-FRAMEWORK-MIB or one of the
  internal variables `intAgentUDPPort`, which defines which UDP port the agent
  listens to, or `intAgentTransports`, which defines the transport domains and
  addresses of the agent.
- `Value` is the value for the variable.

The following example shows an `agent.conf` file:

```erlang
{intAgentUDPPort, 4000}.
{intAgentTransports,
 [{transportDomainUdpIpv4, {141,213,11,24}},
  {transportDomainUdpIpv6, {0,0,0,0,0,0,0,1}}]}.
{snmpEngineID, "mbj's engine"}.
{snmpEngineMaxMessageSize, 484}.
```

And this is a code (snippet) example of how to generate this file in runtime:

```erlang
AgentDir    = "/tmp",
AgentPort   = 4000,
Transports  = [{transportDomainUdpIpv4, {141,213,11,24}},
               {transportDomainUdpIpv6, {0,0,0,0,0,0,0,1}}],
EngineID    = "mbj's engine",
MMS         = 484,
AgentConfig =
   [snmpa_conf:agent_entry(intAgentUDPPort,          AgentPort),
    snmpa_conf:agent_entry(intAgentTransports,       Transports),
    snmpa_conf:agent_entry(snmpEngineID,             EngineID),
    snmpa_conf:agent_entry(snmpEngineMaxMessageSize, MMS)],
snmpa_conf:write_agent_config(AgentDir, AgentConfig),
```


These are the supported entries and their value types:

```erlang
      {snmpEngine,               string()}.                     % Mandatory
      {snmpEngineMaxMessageSize, snmp_framework_mib:max_message_size()}.  % Mandatory
      {intAgentUDPPort,          inet:port_number()}.                      % Optional
      {intAgentTransports,       [snmpa_conf:intAgentTransport()]}.   % Mandatory
```

If a "traditional" transport is specified (without explicit `Kind`, handling
both requests and traps) for a transport domain, its _not_ possible to also
specify a transport (for that domain) with a specific `Kind`. This is for
example, _not_ allowed:

```erlang
 [{transportDomainUdpIpv4, {{141,213,11,24}, 4000}},
  {transportDomainUdpIpv4, {{141,213,11,24}, 4001}, trap_sender}].
```

Note that only one transport per kind for each transport domain can be
configured.

`PortInfo` `system` is used to indicate that the 'system' should choose (the way
port number '0' (zero) is normally used). Port info '0' (zero) cannot be used
for this, since it is (internally) used to represent the 'default' port number.

In the traditional transport entries, when the `Addr` value does not contain a
port number, the value of `intAgentUDPPort` is used.

Note that the (new) extended transport entries (including `Kind` and `Opts`)
_must_ specify port-info as they ignore any value specified by
`intAgentUDPPort`.

`Opts` is the same as for the [net-if](snmp_config.md#agent_ni_opts) process
_and_ takes precedence (for that transport) if present. The point is that each
transport can have its own socket options.

The value of `snmpEngineID` is a string, which for a deployed agent should have
a very specific structure. See RFC 2271/2571 for details.

> #### Note {: .info }
>
> The legacy and intermediate variables `intAgentIpAddress` and
> `intAgentTransportDomain` are still supported so old `agent.conf` files will
> work.
>
> But they _cannot_ be combined with intAgentTransports.

[](){: #context }

## Contexts

The context information should be stored in a file called `context.conf`. The
default context `""` need not be present.

Each row defines a context in the agent. This information is used in the table
`vacmContextTable` in the SNMP-VIEW-BASED-ACM-MIB.

Each entry is a term:

`ContextName.`

- `ContextName` is a string.

And this is a code (snippet) example of how to generate this file in runtime:

```erlang
AgentDir      = "/tmp",
ContextConfig =
   [snmpa_conf:context_entry("foo"),
    snmpa_conf:context_entry("bar")],
snmpa_conf:write_context_config(AgentDir, ContextConfig),
```


[](){: #system_information }

## System Information

The system information should be stored in a file called `standard.conf`.

Each entry is a tuple of size two:

`{SystemVariable, Value}.`

- `SystemVariable` is one of the variables in the system group, or
  `snmpEnableAuthenTraps`.
- `Value` is the value for the variable.

The following example shows a valid `standard.conf` file:

```erlang
{sysDescr, "Erlang SNMP agent"}.
{sysObjectID, [1,2,3]}.
{sysContact, "(mbj,eklas)@erlang.ericsson.se"}.
{sysName, "test"}.
{sysServices, 72}.
{snmpEnableAuthenTraps, enabled}.
```

And this is a code (snippet) example of how to generate this file in runtime:

```erlang
AgentDir  = "/tmp",
StdConfig =
   [snmpa_conf:standard_entry(sysDescr,    "Erlang SNMP agent"),
    snmpa_conf:standard_entry(sysObjectID, [1,2,3]),
    snmpa_conf:standard_entry(sysContact,  "(mbj,eklas)@erlang.ericsson.se"),
    snmpa_conf:standard_entry(sysName,     "test"),
    snmpa_conf:standard_entry(sysServices, 72),
    snmpa_conf:standard_entry(snmpEnableAuthenTraps, enabled)],
snmpa_conf:write_standard_config(AgentDir, StdConfig),
```

A value must be provided for all variables, which lack default values in the
MIB.


[](){: #community }

## Communities

The community information should be stored in a file called `community.conf`. It
must be present if the agent is configured for SNMPv1 or SNMPv2c.

An SNMP _community_ is a relationship between an SNMP agent and a set of SNMP
managers that defines authentication, access control and proxy characteristics.

The corresponding table is `snmpCommunityTable` in the SNMP-COMMUNITY-MIB.

Each entry is a term:

`{CommunityIndex, CommunityName, SecurityName, ContextName, TransportTag}.`

- `CommunityIndex` is a non-empty string.
- `CommunityName` is a string.
- `SecurityName` is a string.
- `ContextName` is a string.
- `TransportTag` is a string.

And this is a code (snippet) example of how to generate this file in runtime:

```erlang
AgentDir        = "/tmp",
CommunityConfig =
   [snmpa_conf:community_entry("public"),
    snmpa_conf:community_entry("all-rights"),
    snmpa_conf:community_entry("standard trap",
                               "standard trap", "initial", "", "")],
snmpa_conf:write_community_config(AgentDir, CommunityConfig),
```


[](){: #vacm }

## MIB Views for VACM

The information about MIB Views for VACM should be stored in a file called
`vacm.conf`.

The corresponding tables are `vacmSecurityToGroupTable`, `vacmAccessTable` and
`vacmViewTreeFamilyTable` in the SNMP-VIEW-BASED-ACM-MIB.

Each entry is one of the terms, one entry corresponds to one row in one of the
tables.

`{vacmSecurityToGroup, SecModel, SecName, GroupName}.`

`{vacmAccess, GroupName, Prefix, SecModel, SecLevel, Match, ReadView, WriteView, NotifyView}.`

`{vacmViewTreeFamily, ViewIndex, ViewSubtree, ViewStatus, ViewMask}.`

- `SecModel` is `any`, `v1`, `v2c`, or `usm`.
- `SecName` is a string.
- `GroupName` is a string.
- `Prefix` is a string.
- `SecLevel` is `noAuthNoPriv`, `authNoPriv`, or `authPriv`
- `Match` is `prefix` or `exact`.
- `ReadView` is a string.
- `WriteView` is a string.
- `NotifyView` is a string.
- `ViewIndex` is an integer.
- `ViewSubtree` is a list of integer.
- `ViewStatus` is either `included` or `excluded`
- `ViewMask` is either `null` or a list of ones and zeros. Ones nominate that an
  exact match is used for this sub-identifier. Zeros are wild-cards which match
  any sub-identifier. If the mask is shorter than the sub-tree, the tail is
  regarded as all ones. `null` is shorthand for a mask with all ones.

And this is a code (snippet) example of how to generate this file in runtime:

```erlang
AgentDir   = "/tmp",
SecName    = "plain",
VacmConfig =
   [%%                        SecModel, SecName, GroupName
    snmpa_conf:vacm_s2g_entry(usm, SecName, SecName),

    %%                        GroupName, Prefix, SecModel,
    snmpa_conf:vacm_acc_entry(SecName, "", any,
    %%                        SecLevel, Match, RV, WV, NV
                              noAuthNoPriv, exact, "all", "all", "all"),

    %%                        ViewName, ViewSubtree, ViewType, ViewMask
    snmpa_conf:vacm_vtf_entry("restricted", [1,3,6,1], included, null)],
snmpa_conf:write_vacm_config(AgentDir, VacmConfig),
```


[](){: #usm }

## Security data for USM

The information about Security data for USM should be stored in a file called
`usm.conf`, which must be present if the agent is configured for SNMPv3.

The corresponding table is `usmUserTable` in the SNMP-USER-BASED-SM-MIB
(adjusted according to SNMP-USM-HMAC-SHA2-MIB).

Each entry is a term:

`{EngineID, UserName, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC, PrivP, PrivKeyC, OwnPrivKeyC, Public, AuthKey, PrivKey}.`

- `EngineID` is a string.
- `UserName` is a string.
- `SecName` is a string.
- `Clone` is `zeroDotZero` or a list of integers.
- `AuthP` is a `usmNoAuthProtocol`, `usmHMACMD5AuthProtocol`,
  `usmHMACSHAAuthProtocol`, `usmHMAC128SHA224AuthProtocol`,
  `usmHMAC192SH256AuthProtocol`, `usmHMAC256SHA384AuthProtocol` or
  `usmHMAC384SHA512AuthProtocol`.
- `AuthKeyC` is a string.
- `OwnAuthKeyC` is a string.
- `PrivP` is a `usmNoPrivProtocol`, `usmDESPrivProtocol` or
  `usmAesCfb128Protocol`.
- `PrivKeyC` is a string.
- `OwnPrivKeyC` is a string.
- `Public` is a string.
- `AuthKey` is a list (of integer). This is the User's secret localized
  authentication key. It is not visible in the MIB. The length (number of
  octets) of this key needs to be:

  - 16 if `usmHMACMD5AuthProtocol`.
  - 20 if `usmHMACSHAAuthProtocol`.
  - 28 if `usmHMAC128SHA224AuthProtocol`.
  - 32 if `usmHMAC192SHA256AuthProtocol`.
  - 48 if `usmHMAC256SHA384AuthProtocol`.
  - 64 if `usmHMAC384SHA512AuthProtocol`.

- `PrivKey` is a list (of integer). This is the User's secret localized
  encryption key. It is not visible in the MIB. The length of this key needs to
  be 16 if `usmDESPrivProtocol` or `usmAesCfb128Protocol` is used.

And this is a code (snippet) example of how to generate this file in runtime:

```erlang
AgentDir  = "/tmp",
EngineID  = "plain engine"
Passwd    = "FooBar Hoopla", %% This should *obviously* be choosen better
Secret16  = snmp:passwd2localized_key(md5, Passwd, EngineID),
Secret20  = snmp:passwd2localized_key(sha, Passwd, EngineID),
UsmConfig =
   [snmpa_conf:usm_entry(EngineID, "initial", "initial", zeroDotZero,
                         usmHMACMD5AuthProtocol, "", "",
                         usmNoPrivProtocol, "", "",
                         "", Secret16, ""),

    snmpa_conf:usm_entry(EngineID, "templateMD5", "templateMD5", zeroDotZero,
                         usmHMACMD5AuthProtocol, "", "",
                         usmDESPrivProtocol, "", "",
                         "", Secret16, Secret16),

    snmpa_conf:usm_entry(EngineID, "templateSHA", "templateSHA", zeroDotZero,
                         usmHMACSHAAuthProtocol, "", "",
                         usmAesCfb128Protocol, "", "",
                         "", Secret20, Secret16)],
snmpa_conf:write_usm_config(AgentDir, UsmConfig),
```


[](){: #notify }

## Notify Definitions

The information about Notify Definitions should be stored in a file called
`notify.conf`.

The corresponding table is `snmpNotifyTable` in the SNMP-NOTIFICATION-MIB.

Each entry is a term:

`{NotifyName, Tag, Type}.`

- `NotifyName` is a unique non-empty string.
- `Tag` is a string.
- `Type` is `trap` or `inform`.

And this is a code (snippet) example of how to generate this file in runtime:

```erlang
AgentDir     = "/tmp",
NotifyConfig =
   [snmpa_conf:notify_entry("standard trap",   "std_trap",   trap),
    snmpa_conf:notify_entry("standard inform", "std_inform", inform)],
snmpa_conf:write_notify_config(AgentDir, NotifyConfig),
```


[](){: #target_addr }

## Target Address Definitions

The information about Target Address Definitions should be stored in a file
called `target_addr.conf`.

The corresponding tables are `snmpTargetAddrTable` in the SNMP-TARGET-MIB and
`snmpTargetAddrExtTable` in the SNMP-COMMUNITY-MIB.

Each entry is a term:

`{TargetName, Domain, Addr, Timeout, RetryCount, TagList, ParamsName, EngineId}.`  
or  
`{TargetName, Domain, Addr, Timeout, RetryCount, TagList, ParamsName, EngineId, TMask, MaxMessageSize}.`

- `TargetName` is a unique non-empty string.
- `Domain` is one of the atoms: `transportDomainUdpIpv4` |
  `transportDomainUdpIpv6`.
- `Addr` is either an `IpAddr` or an `{IpAddr, IpPort}` tuple. `IpAddr` is
  either a regular Erlang/OTP [`ip_address()`](`t:inet:ip_address/0`) or a
  traditional SNMP integer list, and `IpPort` is an integer.

  If `IpPort` is omitted `162` is used.

- `Timeout` is an integer.
- `RetryCount` is an integer.
- `TagList` is a string.
- `ParamsName` is a string.
- `EngineId` is a string or the atom `discovery`.
- `TMask` is specified just as `Addr` or as `[]`. Note in particular that using
  a list of 6 bytes for IPv4 or 8 words plus 2 bytes for IPv6 are still valid
  address formats so old configurations will work.
- `MaxMessageSize` is an integer (default: 2048).

The old tuple formats with `Ip` address and `Udp` port number found in old
configurations still work.

Note that if `EngineId` has the value `discovery`, the agent cannot send
`inform` messages to that manager until it has performed the _discovery_ process
with that manager.

And this is a code (snippet) example of how to generate this file in runtime:

```erlang
AgentDir         = "/tmp",
Addr1            = {{1,2,3,4},     162},
Addr2            = {{11,21,31,41}, 162},
Timeout          = 1500,
RetryCount       = 3,
TargetAddrConfig =
   [snmpa_conf:target_addr_entry("Target 1",
                                 transportDomainUdpIpv4, Addr1,
				 Timeout, RetryCount,
				 "std_trap, "target_1", "",
				 [], 2048),
    snmpa_conf:target_addr_entry("Target 2",
                                 transportDomainUdpIpv4, Addr2,
				 Timeout, RetryCount,
				 "std_inform, "target_2", "",
				 [], 2048)],
snmpa_conf:write_target_addr_config(AgentDir, TargetAddrConfig),
```


[](){: #target_params }

## Target Parameters Definitions

The information about Target Parameters Definitions should be stored in a file
called `target_params.conf`.

The corresponding table is `snmpTargetParamsTable` in the SNMP-TARGET-MIB.

Each entry is a term:

`{ParamsName, MPModel, SecurityModel, SecurityName, SecurityLevel}.`

- `ParamsName` is a unique non-empty string.
- `MPModel` is `v1`, `v2c` or `v3`
- `SecurityModel` is `v1`, `v2c`, or `usm`.
- `SecurityName` is a string.
- `SecurityLevel` is `noAuthNoPriv`, `authNoPriv` or `authPriv`.

And this is a code (snippet) example of how to generate this file in runtime:

```erlang
AgentDir         = "/tmp",
TargetAddrConfig =
   [snmpa_conf:target_params_entry("target_1", v1),
    snmpa_conf:target_params_entry("target_2", v2, "initial", noAthNoPriv],
snmpa_conf:write_target_params_config(AgentDir, TargetParamsConfig),
```
