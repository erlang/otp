<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

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
# Definition of Manager Configuration Files

Configuration data may be included in configuration files that is located in the
configuration directory. The name of this directory is given in the `config_dir`
configuration parameter. These files are read at start-up.

The directory where the configuration files are found is given as a parameter to
the manager.

The entry format in all files are Erlang terms, separated by a '_._' and a
_newline_. In the following sections, the formats of these terms are described.
Comments may be specified as ordinary Erlang comments.

If syntax errors are discovered in these files they are reported with the
function `config_err/2` of the [error report module](`m:snmpa_error_report`) at
start-up.

## Manager Information

The manager information should be stored in a file called `manager.conf`.

Each entry is a tuple of size two:

`{Variable, Value}.`

- `Variable` is one of the following:

  - `transports` \- which defines the transport domains and their addresses for
    the manager. _Mandatory_

    `Value` is a list of `{Domain, Addr}` tuples or `Domain` atoms.

    - `Domain` is one of `transportDomainUdpIpv4` or `transportDomainUdpIpv6`.
    - `Addr` is for the currently supported domains either an `IpAddr` or an
      `{IpAddr, IpPort}` tuple.`IpAddr` is either a regular Erlang/OTP
      [`ip_address()`](`t:inet:ip_address/0`) or a traditional SNMP integer list
      and `IpPort` is an integer.

      When `Addr` does not contain a port number, the value of `port` is used.

      When a `Addr` is not specified i.e by using only a `Domain` atom, the
      host's name is resolved to find the IP address, and the value of `port` is
      used.

  - `port` \- which defines which UDP port the manager uses for communicating
    with agents. _Mandatory_ if `transports` does not define a port number for
    every transport.
  - `engine_id` \- The `SnmpEngineID` as defined in SNMP-FRAMEWORK-MIB.
    _Mandatory_.
  - `max_message_size` \- The `snmpEngineMaxMessageSize` as defined in
    SNMP-FRAMEWORK-MIB. _Mandatory_.

- `Value` is the value for the variable.

The legacy and intermediate variables `address` and `domain` are still supported
so old configurations will work.

The following example shows a `manager.conf` file:

```erlang
{transports,       [{transportDomainUdpIpv4, {{141,213,11,24}, 5000}},
                    {transportDomainUdpIpv6, {{0,0,0,0,0,0,0,1}, 5000}}]}.
{engine_id,        "mgrEngine"}.
{max_message_size, 484}.
```

The value of `engine_id` is a string, which should have a very specific
structure. See RFC 2271/2571 for details.

## Users

For each _manager user_, the manager needs some information. This information is
either added in the `users.conf` config file or by calling the
[register_user](`snmpm:register_user/4`) function in run-time.

Each row defines a _manager user_ of the manager.

Each entry is a tuple of size four:

`{UserId, UserMod, UserData, DefaultAgentConfig}.`

- `UserId` is any term (used to uniquely identify the user).
- `UserMod` is the user callback module (atom).
- `UserData` is any term (passed on to the user when calling the `UserMod`.
- `DefaultAgentConfig` is a list of default agent config's. These values are
  used as default values when this user registers agents.

## Agents

The information needed to handle agents should be stored in a file called
`agents.conf`. It is also possible to add agents in run-time by calling the
[register_agent](`snmpm:register_agent/3`).

Each entry is a tuple:

`{UserId, TargetName, Comm, Domain, Addr, EngineID, Timeout, MaxMessageSize, Version, SecModel, SecName, SecLevel}.`

- `UserId` is the identity of the _manager user_ responsible for this agent
  (term).
- `TargetName` is a _unique_ _non-empty_ string.
- `Comm` is the community string (string).
- `Domain` is the transport domain, either `transportDomainUdpIpv4` or
  `transportDomainUdpIpv6`.
- `Addr` is the address in the transport domain, either an `{IpAddr, IpPort}`
  tuple or a traditional SNMP integer list containing port number. `IpAddr` is
  either a regular Erlang/OTP [`ip_address()`](`t:inet:ip_address/0`) or a
  traditional SNMP integer list not containing port number, and `IpPort` is an
  integer.
- `EngineID` is the engine-id of the agent (string).
- `Timeout` is re-transmission timeout (`infinity` | integer).
- `MaxMessageSize` is the max message size for outgoing messages to this agent
  (integer).
- `Version` is the version (v1 | v2 | v3).
- `SecModel` is the security model (any | v1 | v2c | usm).
- `SecName` is the security name (string).
- `SecLevel` is security level (noAuthNoPriv | authNoPriv | authPriv).

Legacy configurations using tuples without `Domain` element, as well as with all
`TDomain`, `Ip` and `Port` elements still work.

## Security data for USM

The information about Security data for USM should be stored in a file called
`usm.conf`, which must be present if the manager wishes to use SNMPv3 when
communicating with agents. It is also possible to add usm data in run-time by
calling the [register_usm_user](`snmpm:register_usm_user/3`).

The corresponding table is `usmUserTable` in the SNMP-USER-BASED-SM-MIB
(adjusted according to SNMP-USM-HMAC-SHA2-MIB).

Each entry is a term:

`{EngineID, UserName, AuthP, AuthKey, PrivP, PrivKey}.`  
`{EngineID, UserName, SecName, AuthP, AuthKey, PrivP, PrivKey}.`

The first case is when we have the identity-function (`SecName` = `UserName`).

- `EngineID` is a string.
- `UserName` is a string.
- `SecName` is a string.
- `AuthP` is a `usmNoAuthProtocol`, `usmHMACMD5AuthProtocol`,
  `usmHMACSHAAuthProtocol`, `usmHMAC128SHA224AuthProtocol`,
  `usmHMAC192SH256AuthProtocol`, `usmHMAC256SHA384AuthProtocol` or
  `usmHMAC384SHA512AuthProtocol`.
- `AuthKey` is a list (of integer). This is the User's secret localized
  authentication key. It is not visible in the MIB. The length (number of
  octets) of this key needs to be:

  - 16 if `usmHMACMD5AuthProtocol`.
  - 20 if `usmHMACSHAAuthProtocol`.
  - 28 if `usmHMAC128SHA224AuthProtocol`.
  - 32 if `usmHMAC192SHA256AuthProtocol`.
  - 48 if `usmHMAC256SHA384AuthProtocol`.
  - 64 if `usmHMAC384SHA512AuthProtocol`.

- `PrivP` is a `usmNoPrivProtocol`, `usmDESPrivProtocol` or
  `usmAesCfb128Protocol`.
- `PrivKey` is a list (of integer). This is the User's secret localized
  encryption key. It is not visible in the MIB. The length of this key needs to
  be 16 if `usmDESPrivProtocol` or `usmAesCfb128Protocol` is used.
