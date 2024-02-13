%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2024. All Rights Reserved.
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

-module(snmpa_conf).
-moduledoc """
Utility functions for handling the agent config files.

The module `snmpa_conf` contains various utility functions to used for
manipulating (write/append/read) the config files of the SNMP agent.

## DATA TYPES

```erlang
transportDomain() = transportDomainUdpIpv4 | transportDomainUdpIpv6

transportAddress() =
    transportAddressIPv4() | transportAddressIPv6()

transportAddressWithPort() =
    transportAddressIPv4WithPort() | transportAddressIPv6WithPort()

transportAddressWithoutPort() =
    transportAddressIPv4WithoutPort() | transportAddressIPv6WithoutPort()

transportAddressIPv4() =
    transportAddressIPv4WithPort() | transportAddressIPv4WithoutPort()
transportAddressIPv4WithPort =
    {transportAddressIPv4WithoutPort(), inet:port_number()} |
    [byte() x 4, byte() x 2]
transportAddressIPv4WithoutPort =
    inet:ip4_address() | [byte() x 4]

transportAddressIPv6() =
    transportAddressIPv6WithPort() | transportAddressIPv6WithoutPort()
transportAddressIPv6WithPort =
    {transportAddressIPv6WithoutPort(), inet:port_number()} |
    [word() x 8, inet:port_number()] |
    [word() x 8, byte() x 2] |
    {byte() x 16, byte() x 2]
transportAddressIPv6WithoutPort =
    inet:ip6_address() | [word() x 8] | [byte() x 16]

transportAddressMask() =
    [] | transportAddressWithPort()

byte() = 0..255
word() = 0..65535
```

For [`inet:ip4_address()`](`t:inet:ip4_address/0`),
[`inet:ip6_address()`](`t:inet:ip6_address/0`) and
[`inet:port_number()`](`t:inet:port_number/0`), see also `t:inet:ip_address/0`

[](){: #agent_entry }
""".

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([
	 %% agent.conf
	 agent_entry/2,
	 write_agent_config/2, write_agent_config/3, 
	 append_agent_config/2, 
	 read_agent_config/1,
	 
	 %% context.conf
	 context_entry/1, 
	 write_context_config/2, write_context_config/3, 
	 append_context_config/2, 
	 read_context_config/1, 

	 %% community.conf
	 community_entry/1, community_entry/5, 
	 write_community_config/2, write_community_config/3, 
	 append_community_config/2, 
	 read_community_config/1, 
	 
	 %% standard.conf
	 standard_entry/2,  
	 write_standard_config/2, write_standard_config/3,  
	 append_standard_config/2, 
	 read_standard_config/1, 

	 %% target_addr.conf
	 target_addr_entry/5, target_addr_entry/6,  target_addr_entry/7,
	 target_addr_entry/8, target_addr_entry/10, target_addr_entry/11, 
	 write_target_addr_config/2, write_target_addr_config/3, 
	 append_target_addr_config/2, 
	 read_target_addr_config/1, 

	 %% target_params.conf
	 target_params_entry/2, target_params_entry/4, target_params_entry/5, 
	 write_target_params_config/2, write_target_params_config/3, 
	 append_target_params_config/2, 
	 read_target_params_config/1, 

	 %% xyz.conf
	 notify_entry/3, 
	 write_notify_config/2, write_notify_config/3, 
	 append_notify_config/2, 
	 read_notify_config/1, 

	 %% xyz.conf
	 usm_entry/1, usm_entry/13, 
	 write_usm_config/2, write_usm_config/3, 
	 append_usm_config/2, 
	 read_usm_config/1, 

	 %% xyz.conf
	 vacm_s2g_entry/3, 
	 vacm_acc_entry/8, 
	 vacm_vtf_entry/2, vacm_vtf_entry/4, 
	 write_vacm_config/2, write_vacm_config/3, 
	 append_vacm_config/2, 
 	 read_vacm_config/1
	]).



-export_type([
              usm_entry/0,
              transportDomain/0,
              transportAddress/0,
              transportAddressWithPort/0,
              transportAddressWithoutPort/0,
              transportAddressMask/0
             ]).

-type transportDomain() :: snmp:tdomain().

-type transportAddress() ::
    transportAddressIPv4() | transportAddressIPv6().

-type transportAddressWithPort() ::
        transportAddressIPv4WithPort() | transportAddressIPv6WithPort().

-type transportAddressWithoutPort() ::
        transportAddressIPv4WithoutPort() | transportAddressIPv6WithoutPort().

-type transportAddressIPv4() ::
        transportAddressIPv4WithPort() | transportAddressIPv4WithoutPort().

-type transportAddressIPv4WithPort() ::
   {transportAddressIPv4WithoutPort(), inet:port_number()} |
   [IPA :: byte() | IPB :: byte() | IPC :: byte() | IPD :: byte() |
    PortA :: byte() |  PortB :: byte()].

-type transportAddressIPv4WithoutPort() ::
   inet:ip4_address() | [IPA :: byte() | IPB :: byte() | IPC :: byte() | IPD :: byte()].

-type transportAddressIPv6() ::
    transportAddressIPv6WithPort() | transportAddressIPv6WithoutPort().

-type transportAddressIPv6WithPort() ::
   {transportAddressIPv6WithoutPort(), inet:port_number()} |
   [IPA :: word() | IPB :: word() | IPC :: word() | IPD :: word() |
    IPE :: word() | IPF :: word() | IPG :: word() | IPH :: word() |
    inet:port_number()] |
   [IPA :: word() | IPB :: word() | IPC :: word() | IPD :: word() |
    IPE :: word() | IPF :: word() | IPG :: word() | IPH :: word() |
    PortA :: byte() |  PortB :: byte()] |
   {IPA :: byte(),IPB :: byte(),IPC :: byte(),IPD :: byte(),
    IPE :: byte(),IPF :: byte(),IPG :: byte(),IPH :: byte(),
    IPI :: byte(),IPJ :: byte(),IPK :: byte(),IPL :: byte(),
    IPM :: byte(),IPN :: byte(),IPO :: byte(),IPP :: byte(),
    PortA :: byte(), PortB :: byte()}.
-type transportAddressIPv6WithoutPort() ::
   inet:ip6_address() |
   [IPA :: word() | IPB :: word() | IPC :: word() | IPD :: word() |
    IPE :: word() | IPF :: word() | IPG :: word() | IPH :: word()] |
   [IPA :: byte() | IPB :: byte() | IPC :: byte() | IPD :: byte() |
    IPE :: byte() | IPF :: byte() | IPG :: byte() | IPH :: byte() |
    IPI :: byte() | IPJ :: byte() | IPK :: byte() | IPL :: byte() |
    IPM :: byte() | IPN :: byte() | IPO :: byte() | IPP :: byte()].

-type transportAddressMask() ::
    [] | transportAddressWithPort().

-type word() :: 0..65535.

-type usm_entry() :: {
                      EngineID    :: string(),
                      UserName    :: string(),
                      SecName     :: string(),
                      Clone       :: zeroDotZero | [non_neg_integer()],
                      AuthP       :: usmNoAuthProtocol |
                                     usmHMACMD5AuthProtocol |
                                     usmHMACSHAAuthProtocol |
                                     usmHMAC128SHA224AuthProtocol |
                                     usmHMAC192SHA256AuthProtocol |
                                     usmHMAC256SHA384AuthProtocol |
                                     usmHMAC384SHA512AuthProtocol,
                      AuthKeyC    :: string(),
                      OwnAuthKeyC :: string(),
                      PrivP       :: usmNoPrivProtocol |
                                     usmDESPrivProtocol |
                                     usmAesCfb128Protocol,
                      PrivKeyC    :: string(),
                      OwnPrivKeyC :: string(),
                      Public      :: string(),
                      %% Size 16 for usmHMACMD5AuthProtocol
                      %% Size 20 for usmHMACSHAAuthProtocol
                      %% Size 28 for usmHMAC128SHA224AuthProtocol
                      %% Size 32 for usmHMAC192SHA256AuthProtocol
                      %% Size 48 for usmHMAC256SHA384AuthProtocol
                      %% Size 64 for usmHMAC384SHA512AuthProtocol
                      AuthKey     :: [non_neg_integer()],
                      %% Size 16 for usmDESPrivProtocol | usmAesCfb128Protocol
                      PrivKey     :: [non_neg_integer()]
                     }.


-ifndef(version).
%% This crap is hopefully temporary!
%% It is because our current doc build
%% script (specs file generation) has
%% no way to pass this value in as the
%% normal compilation (erlc) does.
-define(version, "99.99").
-endif.


%%
%% ------ agent.conf ------
%%

-doc """
agent_entry(Tag, Val) -> agent_entry()

Create an entry for the agent config file, `agent.conf`.

The type of `Val` depends on the value of `Tag`, see
[Agent Information](snmp_agent_config_files.md#agent_information) for more info.

[](){: #write_agent_config }
""".
agent_entry(Tag, Val) ->
    {Tag, Val}.


-doc(#{equiv => write_agent_config/3}).
write_agent_config(Dir, Conf) ->
    Comment = 
"%% This file defines the Agent local configuration info\n"
"%% The data is inserted into the snmpEngine* variables defined\n"
"%% in SNMP-FRAMEWORK-MIB, and the intAgent* variables defined\n"
"%% in OTP-SNMPEA-MIB.\n"
"%% Each row is a 2-tuple:\n"
"%% {AgentVariable, Value}.\n"
"%% For example\n"
"%% {intAgentUDPPort, 4000}.\n"
"%% The ip address for the agent is sent as id in traps.\n"
"%% {intAgentIpAddress, [127,42,17,5]}.\n"
"%% {snmpEngineID, \"agentEngine\"}.\n"
"%% {snmpEngineMaxMessageSize, 484}.\n"
"%%\n\n",
    Hdr = header() ++ Comment, 
    write_agent_config(Dir, Hdr, Conf).

-doc """
write_agent_config(Dir, Hdr, Conf) -> ok

Write the agent config to the agent config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Agent Information](snmp_agent_config_files.md#agent_information) for more
info.

[](){: #append_agent_config }
""".
write_agent_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_framework_mib:order_agent/2,
    Check = fun snmp_framework_mib:check_agent/2,
    Write = fun (Fd, Entries) -> write_agent_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "agent.conf", Order, Check, Write, Conf).

-doc """
append_agent_config(Dir, Conf) -> ok

Append the config to the current agent config file.

`Dir` is the path to the directory where to store the config file.

See [Agent Information](snmp_agent_config_files.md#agent_information) for more
info.

[](){: #read_agent_config }
""".
append_agent_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_framework_mib:order_agent/2,
    Check = fun snmp_framework_mib:check_agent/2,
    Write = fun write_agent_conf/2,
    append_config_file(Dir, "agent.conf", Order, Check, Write, Conf).

-doc """
read_agent_config(Dir) -> Conf

Read the current agent config file.

`Dir` is the path to the directory where to store the config file.

See [Agent Information](snmp_agent_config_files.md#agent_information) for more
info.

[](){: #standard_entry }
""".
read_agent_config(Dir) ->
    Order = fun snmp_framework_mib:order_agent/2,
    Check = fun snmp_framework_mib:check_agent/2,
    read_config_file(Dir, "agent.conf", Order, Check).


write_agent_conf(Fd, "", Conf) ->
    write_agent_conf(Fd, Conf);
write_agent_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_agent_conf(Fd, Conf).

write_agent_conf(_Fd, []) ->
    ok;
write_agent_conf(Fd, [H|T]) ->
    do_write_agent_conf(Fd, H),
    write_agent_conf(Fd, T).

do_write_agent_conf(Fd, {intAgentTransports = Tag, Val}) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_agent_conf(Fd, {intAgentTransportDomain = Tag, Val}) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_agent_conf(Fd, {intAgentIpAddress = Tag, Val}) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_agent_conf(Fd, {intAgentUDPPort = Tag, Val} ) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_agent_conf(Fd, {intAgentMaxPacketSize = Tag, Val} ) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_agent_conf(Fd, {snmpEngineMaxMessageSize = Tag, Val} ) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_agent_conf(Fd, {snmpEngineID = Tag, Val} ) ->
    io:format(Fd, "{~w, ~p}.~n", [Tag, Val]);
do_write_agent_conf(_Fd, Crap) ->
    error({bad_agent_config, Crap}).


%%
%% ------ context.conf ------
%%

-doc """
context_entry(Context) -> context_entry()

Create an entry for the agent context config file, `context.conf`.

See [Contexts](snmp_agent_config_files.md#context) for more info.

[](){: #write_context_config }
""".
context_entry(Ctx) ->
    Ctx.


-doc(#{equiv => write_context_config/3}).
write_context_config(Dir, Conf) ->
    Comment =
"%% This file defines the contexts known to the agent.\n"
"%% The data is inserted into the vacmContextTable defined\n"
"%% in SNMP-VIEW-BASED-ACM-MIB.\n"
"%% Each row is a string:\n"
"%% ContextName.\n"
"%%\n"
"%% The empty string is the default context.\n"
"%% For example\n"
"%% \"bridge1\".\n"
"%% \"bridge2\".\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_context_config(Dir, Hdr, Conf).

-doc """
write_context_config(Dir, Hdr, Conf) -> ok

Write the agent context config to the agent context config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Contexts](snmp_agent_config_files.md#context) for more info.

[](){: #append_context_config }
""".
write_context_config(Dir, Hdr, Conf) 
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_context/2,
    Write = fun (Fd, Entries) -> write_context_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "context.conf", Order, Check, Write, Conf).

-doc """
append_context_config(Dir, Conf) -> ok

Append the context config to the current agent context config file.

`Dir` is the path to the directory where to store the config file.

See [Contexts](snmp_agent_config_files.md#context) for more info.

[](){: #read_context_config }
""".
append_context_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_context/2,
    Write = fun write_context_conf/2,
    append_config_file(Dir, "context.conf", Order, Check, Write, Conf).

-doc """
read_context_config(Dir) -> Conf

Read the current agent context config file.

`Dir` is the path to the directory where to store the config file.

See [Contexts](snmp_agent_config_files.md#context) for more info.

[](){: #community_entry }
""".
read_context_config(Dir) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_context/2,
    read_config_file(Dir, "context.conf", Order, Check).


check_context(Entry, State) ->
    {check_ok(snmp_framework_mib:check_context(Entry)),
     State}.

write_context_conf(Fd, "", Conf) ->
    write_context_conf(Fd, Conf);
write_context_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_context_conf(Fd, Conf).

write_context_conf(_Fd, []) ->
    ok;
write_context_conf(Fd, [H|T]) when is_list(H) ->
    io:format(Fd, "\"~s\".~n", [H]),
    write_context_conf(Fd, T);
write_context_conf(_Fd, X) ->
    error({invalid_context_config, X}).


%%
%% ------ community.conf ------
%%

-doc(#{equiv => community_entry/5}).
community_entry(CommIndex) when CommIndex == "public" ->
    CommName     = CommIndex,
    SecName      = "initial",
    CtxName      = "",
    TransportTag = "",
    community_entry(CommIndex, CommName, SecName, CtxName, TransportTag);
community_entry(CommIndex) when CommIndex == "all-rights" ->
    CommName     = CommIndex,
    SecName      = CommIndex,
    CtxName      = "",
    TransportTag = "",
    community_entry(CommIndex, CommName, SecName, CtxName, TransportTag).

-doc """
community_entry(CommunityIndex, CommunityName, SecName, ContextName,
TransportTag) -> community_entry()

Create an entry for the agent community config file, `community.conf`.

`CommunityIndex` must be a _non-empty_ string.

[`community_entry("public")`](`community_entry/1`) translates to the following
call:
[`community_entry(CommunityIndex, CommunityIndex, "initial", "", "")`](`community_entry/5`).

[`community_entry("all-rights")`](`community_entry/1`) translates to the
following call:
[`community_entry(CommunityIndex, CommunityIndex, CommunityIndex, "", "")`](`community_entry/5`).

See [Community](snmp_agent_config_files.md#community) for more info.

[](){: #write_community_config }
""".
community_entry(CommIndex, CommName, SecName, CtxName, TransportTag) ->
    {CommIndex, CommName, SecName, CtxName, TransportTag}.


-doc(#{equiv => write_community_config/3}).
write_community_config(Dir, Conf) ->
    Comment =
"%% This file defines the community info which maps to VACM parameters.\n"
"%% The data is inserted into the snmpCommunityTable defined\n"
"%% in SNMP-COMMUNITY-MIB.\n"
"%% Each row is a 5-tuple:\n"
"%% {CommunityIndex, CommunityName, SecurityName, ContextName, TransportTag}.\n"
"%% For example\n"
"%% {\"1\", \"public\", \"initial\", \"\", \"\"}.\n"
"%% {\"2\", \"secret\", \"secret_name\", \"\", \"tag\"}.\n"
"%% {\"3\", \"bridge1\", \"initial\", \"bridge1\", \"\"}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_community_config(Dir, Hdr, Conf).

-doc """
write_community_config(Dir, Hdr, Conf) -> ok

Write the agent community config to the agent community config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Community](snmp_agent_config_files.md#community) for more info.

[](){: #append_community_config }
""".
write_community_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_community/2,
    Write = fun (Fd, Entries) -> write_community_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "community.conf", Order, Check, Write, Conf).

-doc """
append_community_config(Dir, Conf) -> ok

Append the community config to the current agent community config file.

`Dir` is the path to the directory where to store the config file.

See [Community](snmp_agent_config_files.md#community) for more info.

[](){: #read_community_config }
""".
append_community_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_community/2,
    Write = fun write_community_conf/2,
    append_config_file(Dir, "community.conf", Order, Check, Write, Conf).

-doc """
read_community_config(Dir) -> Conf

Read the current agent community config file.

`Dir` is the path to the directory where to store the config file.

See [Communities](snmp_agent_config_files.md#community) for more info.

[](){: #target_addr_entry }
""".
read_community_config(Dir) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_community/2,
    read_config_file(Dir, "community.conf", Order, Check).


check_community(Entry, State) ->
    {check_ok(snmp_community_mib:check_community(Entry)),
     State}.

write_community_conf(Fd, "", Conf) ->
    write_community_conf(Fd, Conf);
write_community_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_community_conf(Fd, Conf).

write_community_conf(Fd, Conf) ->
    Fun =
	fun({Idx, Name, SecName, CtxName, TranspTag}) ->
		io:format(
		  Fd,
		  "{\"~s\", \"~s\", \"~s\", \"~s\", \"~s\"}.~n",
		  [Idx, Name, SecName, CtxName, TranspTag]);
	     (Crap) ->
		error({bad_community_config, Crap})
	end,
    lists:foreach(Fun, Conf).


%%
%% ------ standard.conf ------
%%

-doc """
standard_entry(Tag, Val) -> standard_entry()

Create an entry for the agent standard config file, `standard.conf`.

The type of `Val` depends on the value of `Tag`, see
[System Information](snmp_agent_config_files.md#system_information) for more
info.

[](){: #write_standard_config }
""".
standard_entry(Tag, Val) ->
    {Tag, Val}.


-doc(#{equiv => write_standard_config/3}).
write_standard_config(Dir, Conf) ->
    Comment =
"%% This file defines the STANDARD-MIB info.\n"
"%% Each row is a 2-tuple:\n"
"%% {StandardVariable, Value}.\n"
"%% For example\n"
"%% {sysDescr, \"Erlang SNMP agent\"}.\n"
"%% {sysObjectID, [1,2,3]}.\n"
"%% {sysContact, \"{mbj,eklas}@erlang.ericsson.se\"}.\n"
"%% {sysName, \"test\"}.\n"
"%% {sysLocation, \"erlang\"}.\n"
"%% {sysServices, 72}.\n"
"%% {snmpEnableAuthenTraps, enabled}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_standard_config(Dir, Hdr, Conf).

-doc """
write_standard_config(Dir, Hdr, Conf) -> ok

Write the agent standard config to the agent standard config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [System Information](snmp_agent_config_files.md#system_information) for more
info.

[](){: #append_standard_config }
""".
write_standard_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_standard/2,
    Write = fun (Fd, Entries) -> write_standard_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "standard.conf", Order, Check, Write, Conf).

-doc """
append_standard_config(Dir, Conf) -> ok

Append the standard config to the current agent standard config file.

`Dir` is the path to the directory where to store the config file.

See [System Information](snmp_agent_config_files.md#system_information) for more
info.

[](){: #read_standard_config }
""".
append_standard_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_standard/2,
    Write = fun write_standard_conf/2,
    append_config_file(Dir, "standard.conf", Order, Check, Write, Conf).

-doc """
read_standard_config(Dir) -> Conf

Read the current agent standard config file.

`Dir` is the path to the directory where to store the config file.

See [System Information](snmp_agent_config_files.md#system_information) for more
info.

[](){: #context_entry }
""".
read_standard_config(Dir) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_standard/2,
    read_config_file(Dir, "standard.conf", Order, Check).


check_standard(Entry, State) ->
    {check_ok(snmp_standard_mib:check_standard(Entry)),
     State}.

write_standard_conf(Fd, "", Conf) ->
    write_standard_conf(Fd, Conf);
write_standard_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_standard_conf(Fd, Conf).

write_standard_conf(Fd, Conf) -> 
    Fun = fun({Tag, Val}) -> do_write_standard_conf(Fd, Tag, Val) end,
    lists:foreach(Fun, Conf).
		
do_write_standard_conf(Fd, sysDescr = Tag, Val) ->
    io:format(Fd, "{~w, \"~s\"}.~n", [Tag, Val]);
do_write_standard_conf(Fd, sysObjectID = Tag, Val) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_standard_conf(Fd, sysContact = Tag,  Val) ->
    io:format(Fd, "{~w, \"~s\"}.~n", [Tag, Val]);
do_write_standard_conf(Fd, sysName = Tag,     Val) ->
    io:format(Fd, "{~w, \"~s\"}.~n", [Tag, Val]);
do_write_standard_conf(Fd, sysLocation = Tag, Val) ->
    io:format(Fd, "{~w, \"~s\"}.~n", [Tag, Val]);
do_write_standard_conf(Fd, sysServices = Tag, Val) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_standard_conf(Fd, snmpEnableAuthenTraps = Tag, Val) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_standard_conf(_Fd, Tag, Val) ->
    error({bad_standard_config, {Tag, Val}}).


%%
%% ------ target_addr.conf ------
%%

-doc false.
target_addr_entry(
  Name, Ip, TagList, ParamsName, EngineId) ->
    target_addr_entry(Name, Ip, TagList, ParamsName, EngineId, []).

-doc(#{equiv => target_addr_entry/10}).
-doc(#{since => <<"OTP 17.3">>}).
target_addr_entry(
  Name, Domain, Addr, TagList,
  ParamsName, EngineId) when is_atom(Domain) ->
    target_addr_entry(
      Name, Domain, Addr, TagList,
      ParamsName, EngineId, []);
target_addr_entry(
  Name, Ip, TagList, ParamsName,
  EngineId, TMask) ->
    target_addr_entry(
      Name, Ip, 162, TagList, ParamsName,
      EngineId, TMask, 2048).

-doc(#{equiv => target_addr_entry/10}).
-doc(#{since => <<"OTP 17.3">>}).
target_addr_entry(
  Name, Domain_or_Ip, Addr_or_Port, TagList,
  ParamsName, EngineId, TMask) ->
    target_addr_entry(
      Name, Domain_or_Ip, Addr_or_Port, TagList,
      ParamsName, EngineId, TMask, 2048).

-doc(#{equiv => target_addr_entry/10}).
-doc(#{since => <<"OTP 17.3">>}).
target_addr_entry(
  Name, Domain_or_Ip, Addr_or_Port, TagList,
  ParamsName, EngineId, TMask, MaxMessageSize) ->
    target_addr_entry(
      Name, Domain_or_Ip, Addr_or_Port, 1500, 3, TagList,
      ParamsName, EngineId, TMask, MaxMessageSize).

-doc """
target_addr_entry(Name, Domain, Addr, Timeout, RetryCount, TagList, ParamsName,
EngineId, TMask, MaxMessageSize) -> target_addr_entry()

Create an entry for the agent target_addr config file, `target_addr.conf`.

`Name` must be a _non-empty_ string.

[`target_addr_entry/6`](`target_addr_entry/6`) translates to the following call:
[`target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, [])`](`target_addr_entry/7`).

[`target_addr_entry/7`](`target_addr_entry/7`) translates to the following call:
[`target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, TMask, 2048)`](`target_addr_entry/8`).

[`target_addr_entry/8`](`target_addr_entry/8`) translates to the following call:
[`target_addr_entry(Name, Domain, Addr, 1500, 3, TagList, ParamsName, EngineId, TMask, MaxMessageSize)`](`target_addr_entry/10`).

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

[](){: #write_target_addr_config }
""".
-doc(#{since => <<"OTP 17.3">>}).
target_addr_entry(
  Name, Domain_or_Ip, Addr_or_Port, Timeout, RetryCount, TagList,
  ParamsName, EngineId, TMask, MaxMessageSize) ->
    {Name, Domain_or_Ip, Addr_or_Port, Timeout, RetryCount, TagList,
     ParamsName, EngineId, TMask, MaxMessageSize}.

-doc false.
target_addr_entry(
  Name, Domain, Ip, Udp, Timeout, RetryCount, TagList,
  ParamsName, EngineId,TMask, MaxMessageSize) ->
    {Name, Domain, Ip, Udp, Timeout, RetryCount, TagList,
     ParamsName, EngineId, TMask, MaxMessageSize}.


-doc(#{equiv => write_target_addr_config/3}).
write_target_addr_config(Dir, Conf) ->
    Comment = 
"%% This file defines the target address parameters.\n"
"%% The data is inserted into the snmpTargetAddrTable defined\n"
"%% in SNMP-TARGET-MIB, and in the snmpTargetAddrExtTable defined\n"
"%% in SNMP-COMMUNITY-MIB.\n"
"%% Each row is a 10 or 11-tuple (Domain is optional):\n"
"%% {Name, \n"
"%%  Domain, Ip, Port, \n"
"%%  Timeout, RetryCount, TagList, ParamsName, EngineId,\n"
"%%  TMask, MaxMessageSize}.\n"
"%% The value of Domain decide the format of the Ip and TMask values. \n"
"%% If not present, classic Ipv4 is assumed. \n"
"%% The EngineId value is only used if Inform-Requests are sent to this\n"
"%% target.  If Informs are not sent, this value is ignored, and can be\n"
"%% e.g. an empty string.  However, if Informs are sent, it is essential\n"
"%% that the value of EngineId matches the value of the target's\n"
"%% actual snmpEngineID.\n"
"%% For example\n"
"%% {\"1.2.3.4 v1\", [1,2,3,4], 162, \n"
"%%  1500, 3, \"std_inform\", \"otp_v2\", \"\",\n"
"%%  [127,0,0,0],  2048}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_target_addr_config(Dir, Hdr, Conf).

-doc """
write_target_addr_config(Dir, Hdr, Conf) -> ok

Write the agent target_addr config to the agent target_addr config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

[](){: #append_target_addr_config }
""".
write_target_addr_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_addr/2,
    Write = fun (Fd, Entries) -> write_target_addr_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "target_addr.conf", Order, Check, Write, Conf).

-doc """
append_target_addr_config(Dir, Conf) -> ok

Append the target_addr config to the current agent target_addr config file.

`Dir` is the path to the directory where to store the config file.

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

[](){: #read_target_addr_config }
""".
append_target_addr_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_addr/2,
    Write = fun write_target_addr_conf/2,
    append_config_file(Dir, "target_addr.conf", Order, Check, Write, Conf).

-doc """
read_target_addr_config(Dir) -> Conf

Read the current agent target_addr config file.

`Dir` is the path to the directory where to store the config file.

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

[](){: #target_params_entry }
""".
read_target_addr_config(Dir) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_addr/2,
    read_config_file(Dir, "target_addr.conf", Order, Check).


check_target_addr(Entry, State) ->
    {check_ok(snmp_target_mib:check_target_addr(Entry)),
     State}.

write_target_addr_conf(Fd, "", Conf) ->
    write_target_addr_conf(Fd, Conf);
write_target_addr_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_target_addr_conf(Fd, Conf).

write_target_addr_conf(Fd, Conf) -> 
    Fun = fun(Entry) -> do_write_target_addr_conf(Fd, Entry) end,
    lists:foreach(Fun, Conf),
    ok.

do_write_target_addr_conf(
  Fd,
  {Name, Domain, Address, Timeout, RetryCount, TagList,
   ParamsName, EngineId, TMask, MaxMessageSize})
  when is_atom(Domain) ->
    io:format(
      Fd,
      "{\"~s\", ~w, ~w, ~w, ~w, \"~s\", \"~s\", \"~s\", ~w, ~w}.~n",
      [Name, Domain, Address, Timeout, RetryCount, TagList,
       ParamsName, EngineId, TMask, MaxMessageSize]);
do_write_target_addr_conf(
  Fd,
  {Name, Ip, Udp, Timeout, RetryCount, TagList,
   ParamsName, EngineId, TMask, MaxMessageSize})
  when is_integer(Udp) ->
    Domain = snmp_target_mib:default_domain(),
    Address = {Ip, Udp},
    do_write_target_addr_conf(
      Fd,
      {Name, Domain, Address, Timeout, RetryCount, TagList,
       ParamsName, EngineId, TMask, MaxMessageSize});
do_write_target_addr_conf(
  _Fd,
  {_Name, Domain, Address, _Timeout, _RetryCount, _TagList,
   _ParamsName, _EngineId, _TMask, _MaxMessageSize}) ->
    error({bad_address, {Domain, Address}});
do_write_target_addr_conf(
  Fd,
  {Name, Domain, Ip, Udp, Timeout, RetryCount, TagList,
   ParamsName, EngineId, TMask, MaxMessageSize}) ->
    Address = {Ip, Udp},
    do_write_target_addr_conf(
      Fd,
      {Name, Domain, Address, Timeout, RetryCount, TagList,
       ParamsName, EngineId, TMask, MaxMessageSize});
do_write_target_addr_conf(_Fd, Crap) ->
    error({bad_target_addr_config, Crap}).


%%
%% ------ target_params.conf ------
%%

-doc(#{equiv => target_params_entry/5}).
target_params_entry(Name, Vsn) ->
    SecName  = "initial",
    SecLevel = noAuthNoPriv,
    target_params_entry(Name, Vsn, SecName, SecLevel).

-doc(#{equiv => target_params_entry/5}).
target_params_entry(Name, Vsn, SecName, SecLevel) ->
    MPModel = if Vsn =:= v1 -> v1;
		 Vsn =:= v2 -> v2c;
		 Vsn =:= v3 -> v3
	      end,
    SecModel = if Vsn =:= v1 -> v1;
		  Vsn =:= v2 -> v2c;
		  Vsn =:= v3 -> usm
	       end,
    target_params_entry(Name, MPModel, SecModel, SecName, SecLevel).

-doc """
target_params_entry(Name, MPModel, SecModel, SecName, SecLevel) ->
target_params_entry()

Create an entry for the agent target_params config file, `target_params.conf`.

`Name` must be a _non-empty_ string.

`Vsn` translates into `MPModel` and `SecModel` as follows:

```text
\011  Vsn = v1 => MPModel = v1,  SecModel = v1
\011  Vsn = v2 => MPModel = v2c, SecModel = v2c
\011  Vsn = v3 => MPModel = v3,  SecModel = usm
```

[`target_params_entry/2`](`target_params_entry/2`) translates to the following
call:
[`target_params_entry(Name, Vsn, "initial", noAuthNoPriv)`](`target_params_entry/4`).

[`target_params_entry/4`](`target_params_entry/4`) translates to the following
call:
[`target_params_entry(Name, MPModel, SecModel, SecName, SecLevel)`](`target_params_entry/5`)
where `MPModel` and `SecModel` is mapped from `Vsn`, see above.

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

[](){: #write_target_params_config }
""".
target_params_entry(Name, MPModel, SecModel, SecName, SecLevel) ->
    {Name, MPModel, SecModel, SecName, SecLevel}.
    

-doc(#{equiv => write_target_params_config/3}).
write_target_params_config(Dir, Conf) ->
    Comment =
"%% This file defines the target parameters.\n"
"%% The data is inserted into the snmpTargetParamsTable defined\n"
"%% in SNMP-TARGET-MIB.\n"
"%% Each row is a 5-tuple:\n"
"%% {Name, MPModel, SecurityModel, SecurityName, SecurityLevel}.\n"
"%% For example\n"
"%% {\"target_v3\", v3, usm, \"\", noAuthNoPriv}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_target_params_config(Dir, Hdr, Conf).

-doc """
write_target_params_config(Dir, Hdr, Conf) -> ok

Write the agent target_params config to the agent target_params config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

[](){: #append_target_params_config }
""".
write_target_params_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_params/2,
    Write = fun (Fd, Entries) -> write_target_params_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "target_params.conf", Order, Check, Write, Conf).

-doc """
append_target_params_config(Dir, Conf) -> ok

Append the target_params config to the current agent target_params config file.

`Dir` is the path to the directory where to store the config file.

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

[](){: #read_target_params_config }
""".
append_target_params_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_params/2,
    Write = fun write_target_params_conf/2,
    append_config_file(Dir, "target_params.conf", Order, Check, Write, Conf).

-doc """
read_target_params_config(Dir) -> Conf

Read the current agent target_params config file.

`Dir` is the path to the directory where to store the config file.

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

[](){: #vacm_entry }
""".
read_target_params_config(Dir) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_params/2,
    read_config_file(Dir, "target_params.conf", Order, Check).


check_target_params(Entry, State) ->
    {check_ok(snmp_target_mib:check_target_params(Entry)),
     State}.

write_target_params_conf(Fd, "", Conf) ->
    write_target_params_conf(Fd, Conf);
write_target_params_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_target_params_conf(Fd, Conf).

write_target_params_conf(Fd, Conf) ->
    Fun = fun(Entry) -> do_write_target_params_conf(Fd, Entry) end,
    lists:foreach(Fun, Conf).

do_write_target_params_conf(Fd, 
			    {Name, MpModel, SecModel, SecName, SecLevel}) ->
    io:format(Fd, "{\"~s\", ~w, ~w, \"~s\", ~w}.~n",
              [Name, MpModel, SecModel, SecName, SecLevel]);
do_write_target_params_conf(_Fd, Crap) ->
    error({bad_target_params_config, Crap}).


%%
%% ------ notify.conf ------
%%

-doc """
notify_entry(Name, Tag, Type) -> notify_entry()

Create an entry for the agent notify config file, `notify.conf`.

`Name` must be a _non-empty_ string.

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.

[](){: #write_notify_config }
""".
notify_entry(Name, Tag, Type) ->
    {Name, Tag, Type}.


-doc(#{equiv => write_notify_config/3}).
write_notify_config(Dir, Conf) ->
    Comment =
"%% This file defines the notification parameters.\n"
"%% The data is inserted into the snmpNotifyTable defined\n"
"%% in SNMP-NOTIFICATION-MIB.\n"
"%% The Name is used as CommunityString for v1 and v2c.\n"
"%% Each row is a 3-tuple:\n"
"%% {Name, Tag, Type}.\n"
"%% For example\n"
"%% {\"standard trap\", \"std_trap\", trap}.\n"
"%% {\"standard inform\", \"std_inform\", inform}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_notify_config(Dir, Hdr, Conf).

-doc """
write_notify_config(Dir, Hdr, Conf) -> ok

Write the agent notify config to the agent notify config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.

[](){: #append_notify_config }
""".
write_notify_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_notify/2,
    Write = fun (Fd, Entries) -> write_notify_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "notify.conf", Order, Check, Write, Conf).

-doc """
append_notify_config(Dir, Conf) -> ok

Append the notify config to the current agent notify config file.

`Dir` is the path to the directory where to store the config file.

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.

[](){: #read_notify_config }
""".
append_notify_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_notify/2,
    Write = fun write_notify_conf/2,
    append_config_file(Dir, "notify.conf", Order, Check, Write, Conf).

-doc """
read_notify_config(Dir) -> Conf

Read the current agent notify config file.

`Dir` is the path to the directory where to store the config file.

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.

[](){: #end }
""".
read_notify_config(Dir) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_notify/2,
    read_config_file(Dir, "notify.conf", Order, Check).


check_notify(Entry, State) ->
    {check_ok(snmp_notification_mib:check_notify(Entry)),
     State}.

write_notify_conf(Fd, "", Conf) ->
    write_notify_conf(Fd, Conf);
write_notify_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_notify_conf(Fd, Conf).

write_notify_conf(Fd, Conf) ->
    Fun = fun(Entry) -> do_write_notify_conf(Fd, Entry) end,
    lists:foreach(Fun, Conf).

do_write_notify_conf(Fd, {Name, Tag, Type}) ->
    io:format(Fd, "{\"~s\", \"~s\", ~w}.~n", [Name, Tag, Type]);
do_write_notify_conf(_Fd, Crap) ->
    error({bad_notify_config, Crap}).


%%
%% ------ usm.conf ------
%%

-doc(#{equiv => usm_entry/13}).
usm_entry(EngineID) ->
    UserName    = "initial", 
    SecName     = "initial", 
    Clone       = zeroDotZero, 
    AuthP       = usmNoAuthProtocol, 
    AuthKeyC    = "", 
    OwnAuthKeyC = "",
    PrivP       = usmNoPrivProtocol, 
    PrivKeyC    = "", 
    OwnPrivKeyC = "",
    Public      = "", 
    AuthKey     = "", 
    PrivKey     = "",
    usm_entry(EngineID, UserName, SecName, Clone, 
	      AuthP, AuthKeyC, OwnAuthKeyC,
              PrivP, PrivKeyC, OwnPrivKeyC,
              Public, AuthKey, PrivKey).

-doc """
usm_entry(EngineID, UserName, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC,
PrivP, PrivKeyC, OwnPrivKeyC, Public, AuthKey, PrivKey) -> usm_entry()

Create an entry for the agent vacm config file, `vacm.conf`.

[`usm_entry/1`](`usm_entry/1`) translates to the following call:
`usm_entry("initial", "initial", zeroDotZero, usmNoAuthProtocol, "", "", usmNoPrivProtocol, "", "", "", "", "")`.

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.

[](){: #write_usm_config }
""".
usm_entry(EngineID, UserName, SecName, Clone, 
	  AuthP, AuthKeyC, OwnAuthKeyC,
	  PrivP, PrivKeyC, OwnPrivKeyC,
	  Public, AuthKey, PrivKey) ->
    {EngineID, UserName, SecName, Clone, 
     AuthP, AuthKeyC, OwnAuthKeyC,
     PrivP, PrivKeyC, OwnPrivKeyC,
     Public, AuthKey, PrivKey}.
    

-doc(#{equiv => write_usm_config/3}).
write_usm_config(Dir, Conf) ->
    Comment =
"%% This file defines the security parameters for the user-based\n"
"%% security model.\n"
"%% The data is inserted into the usmUserTable defined\n"
"%% in SNMP-USER-BASED-SM-MIB.\n"
"%% Each row is a 13-tuple:\n"
"%% {EngineID, UserName, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC,\n"
"%%  PrivP, PrivKeyC, OwnPrivKeyC, Public, AuthKey, PrivKey}.\n"
"%% For example\n"
"%% {\"agentEngine\", \"initial\", \"initial\", zeroDotZero,\n"
"%%  usmNoAuthProtocol, \"\", \"\", usmNoPrivProtocol, \"\", \"\", \"\",\n"
"%%  \"\", \"\"}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_usm_config(Dir, Hdr, Conf).

-doc """
write_usm_config(Dir, Hdr, Conf) -> ok

Write the agent usm config to the agent usm config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.

[](){: #append_usm_config }
""".
write_usm_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_usm/2,
    Write = fun (Fd, Entries) -> write_usm_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "usm.conf", Order, Check, Write, Conf).

-doc """
append_usm_config(Dir, Conf) -> ok

Append the usm config to the current agent vacm config file.

`Dir` is the path to the directory where to store the config file.

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.

[](){: #read_usm_config }
""".
append_usm_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_usm/2,
    Write = fun write_usm_conf/2,
    append_config_file(Dir, "usm.conf", Order, Check, Write, Conf).

-doc """
read_usm_config(Dir) -> Conf

Read the current agent usm config file.

`Dir` is the path to the directory where to store the config file.

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.

[](){: #notify_entry }
""".
read_usm_config(Dir) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_usm/2,
    read_config_file(Dir, "usm.conf", Order, Check).


check_usm(Entry, State) ->
    {check_ok(snmp_user_based_sm_mib:check_usm(Entry)),
     State}.

write_usm_conf(Fd, "", Conf) ->
    write_usm_conf(Fd, Conf);
write_usm_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_usm_conf(Fd, Conf).

write_usm_conf(Fd, Conf) ->
    Fun = fun(Entry) -> do_write_usm_conf(Fd, Entry) end,
    lists:foreach(Fun, Conf).

do_write_usm_conf(
  Fd,
  {EngineID, UserName, SecName, Clone,
   AuthP, AuthKeyC, OwnAuthKeyC,
   PrivP, PrivKeyC, OwnPrivKeyC,
   Public, AuthKey, PrivKey}) ->
    io:format(Fd, "{", []),
    io:format(Fd, "~p, ", [EngineID]),
    io:format(Fd, "~p, ", [UserName]),
    io:format(Fd, "~p, ", [SecName]),
    io:format(Fd, "~w, ",     [Clone]),
    io:format(Fd, "~w, ",     [AuthP]),
    do_write_usm2(Fd, AuthKeyC, ", "),
    do_write_usm2(Fd, OwnAuthKeyC, ", "),
    io:format(Fd, "~w, ",     [PrivP]),
    do_write_usm2(Fd, PrivKeyC, ", "),
    do_write_usm2(Fd, OwnPrivKeyC, ", "),
    do_write_usm2(Fd, Public, ", "),
    do_write_usm2(Fd, AuthKey, ", "),
    do_write_usm2(Fd, PrivKey, ""),
    io:format(Fd, "}.~n", []);
do_write_usm_conf(_Fd, Crap) ->
    error({bad_usm_config, Crap}).

do_write_usm2(Fd, "", P) ->
    io:format(Fd, "\"\"~s", [P]);
do_write_usm2(Fd, X, P) ->
    io:format(Fd, "~w~s", [X, P]).


%%
%% ------ vacm.conf ------
%%

-doc(#{equiv => vacm_vtf_entry/4}).
vacm_s2g_entry(SecModel, SecName, GroupName) ->
    {vacmSecurityToGroup, SecModel, SecName, GroupName}.

-doc(#{equiv => vacm_vtf_entry/4}).
vacm_acc_entry(GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV) ->
    {vacmAccess, GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV}.

-doc(#{equiv => vacm_vtf_entry/4}).
vacm_vtf_entry(ViewIndex, ViewSubtree) ->
    vacm_vtf_entry(ViewIndex, ViewSubtree, included, null).
-doc """
vacm_vtf_entry(ViewIndex, ViewSubtree, ViewStatus, ViewMask) -> vacm_vtf_entry()

Create an entry for the agent vacm config file, `vacm.conf`.

[`vacm_vtf_entry/2`](`vacm_vtf_entry/2`) translates to the following call:
[`vacm_vtf_entry(ViewIndex, ViewSubtree, included, null)`](`vacm_vtf_entry/4`).

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

[](){: #write_vacm_config }
""".
vacm_vtf_entry(ViewIndex, ViewSubtree, ViewStatus, ViewMask) ->
    {vacmViewTreeFamily, ViewIndex, ViewSubtree, ViewStatus, ViewMask}.


-doc(#{equiv => write_vacm_config/3}).
write_vacm_config(Dir, Conf) ->
    Comment =
"%% This file defines the Mib Views.\n"
"%% The data is inserted into the vacm* tables defined\n"
"%% in SNMP-VIEW-BASED-ACM-MIB.\n"
"%% Each row is one of 3 tuples; one for each table in the MIB:\n"
"%% {vacmSecurityToGroup, SecModel, SecName, GroupName}.\n"
"%% {vacmAccess, GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV}.\n"
"%% {vacmViewTreeFamily, ViewIndex, ViewSubtree, ViewStatus, ViewMask}.\n"
"%% For example\n"
"%% {vacmSecurityToGroup, v2c, \"initial\", \"initial\"}.\n"
"%% {vacmSecurityToGroup, usm, \"initial\", \"initial\"}.\n"
"%%  read/notify access to system\n"
"%% {vacmAccess, \"initial\", \"\", any, noAuthNoPriv, exact,\n"
"%%              \"system\", \"\", \"system\"}.\n"
"%% {vacmViewTreeFamily, \"system\", [1,3,6,1,2,1,1], included, null}.\n"
"%% {vacmViewTreeFamily, \"exmib\", [1,3,6,1,3], included, null}."
" % for EX1-MIB\n"
"%% {vacmViewTreeFamily, \"internet\", [1,3,6,1], included, null}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_vacm_config(Dir, Hdr, Conf).

-doc """
write_vacm_config(Dir, Hdr, Conf) -> ok

Write the agent vacm config to the agent vacm config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

[](){: #append_vacm_config }
""".
write_vacm_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_vacm/2,
    Write = fun (Fd, Entries) -> write_vacm_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "vacm.conf", Order, Check, Write, Conf).

-doc """
append_vacm_config(Dir, Conf) -> ok

Append the vacm config to the current agent vacm config file.

`Dir` is the path to the directory where to store the config file.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

[](){: #read_vacm_config }
""".
append_vacm_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_vacm/2,
    Write = fun write_vacm_conf/2,
    append_config_file(Dir, "vacm.conf", Order, Check, Write, Conf).

-doc """
read_vacm_config(Dir) -> Conf

Read the current agent vacm config file.

`Dir` is the path to the directory where to store the config file.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

[](){: #usm_entry }
""".
read_vacm_config(Dir) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_vacm/2,
    read_config_file(Dir, "vacm.conf", Order, Check).


check_vacm(Entry, State) ->
    {check_ok(snmp_view_based_acm_mib:check_vacm(Entry)),
     State}.

write_vacm_conf(Fd, "", Conf) ->
    write_vacm_conf(Fd, Conf);
write_vacm_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_vacm_conf(Fd, Conf).

write_vacm_conf(Fd, Conf) ->
    Fun = fun(Entry) -> do_write_vacm_conf(Fd, Entry) end,
    lists:foreach(Fun, Conf).

do_write_vacm_conf(
  Fd,
  {vacmSecurityToGroup,
   SecModel, SecName, GroupName}) ->
    io:format(
      Fd, "{vacmSecurityToGroup, ~w, ~p, ~p}.~n",
      [SecModel, SecName, GroupName]);
do_write_vacm_conf(
  Fd,
  {vacmAccess,
   GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV}) ->
    io:format(
      Fd, "{vacmAccess, ~p, ~p, ~w, ~w, ~w, "
      "~p, ~p, ~p}.~n",
      [GroupName, Prefix, SecModel, SecLevel,
       Match, RV, WV, NV]);
do_write_vacm_conf(
  Fd,
  {vacmViewTreeFamily,
   ViewIndex, ViewSubtree, ViewStatus, ViewMask}) ->
    io:format(
      Fd, "{vacmViewTreeFamily, ~p, ~w, ~w, ~w}.~n",
      [ViewIndex, ViewSubtree, ViewStatus, ViewMask]);
do_write_vacm_conf(_Fd, Crap) ->
    error({bad_vacm_config, Crap}).


%% ---- config file wrapper functions ----

write_config_file(Dir, File, Order, Check, Write, Conf) ->
    snmp_config:write_config_file(Dir, File, Order, Check, Write, Conf).

append_config_file(Dir, File, Order, Check, Write, Conf) ->
    snmp_config:append_config_file(Dir, File, Order, Check, Write, Conf).

read_config_file(Dir, File, Order, Check) ->
    snmp_config:read_config_file(Dir, File, Order, Check).


%% ---- config file utility functions ----

check_ok(ok) ->
    ok;
check_ok({ok, _}) ->
    ok.

header() ->
    {Y,Mo,D} = date(),
    {H,Mi,S} = time(),
    io_lib:format(
      "%% This file was generated by "
      "~w (version-~s) ~w-~2.2.0w-~2.2.0w "
      "~2.2.0w:~2.2.0w:~2.2.0w\n",
      [?MODULE, ?version, Y, Mo, D, H, Mi, S]).

error(R) ->
    throw({error, R}).
