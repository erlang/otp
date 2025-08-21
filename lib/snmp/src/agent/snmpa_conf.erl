%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2006-2025. All Rights Reserved.
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

The module `snmpa_conf` contains various utility functions to use for
manipulating (write/read/append) the config files of the SNMP agent.

""".

-include("snmp_internal.hrl").


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

	 %% notify.conf
	 notify_entry/3, 
	 write_notify_config/2, write_notify_config/3, 
	 append_notify_config/2, 
	 read_notify_config/1, 

	 %% usm.conf
	 usm_entry/1, usm_entry/13, 
	 write_usm_config/2, write_usm_config/3, 
	 append_usm_config/2, 
	 read_usm_config/1, 

	 %% vacm.conf
	 vacm_s2g_entry/3, 
	 vacm_acc_entry/8, 
	 vacm_vtf_entry/2, vacm_vtf_entry/4, 
	 write_vacm_config/2, write_vacm_config/3, 
	 append_vacm_config/2, 
 	 read_vacm_config/1
	]).


-export_type([
              transportDomain/0,
              transportAddress/0,
              transportAddressWithPort/0,
              transportAddressWithoutPort/0,
              transportAddressMask/0,

              agent_entry/0,
              community_entry/0,
              context_entry/0,
              notify_entry/0,
              standard_entry/0,
              target_addr_entry/0,
              target_params_entry/0,
              usm_entry/0,
              vacm_entry/0,
              vacm_s2g_entry/0,
              vacm_acc_entry/0,
              vacm_vtf_entry/0,

              range/0,
              ranges/0,
              port_info/0,
              transport_address/0,
              extended_transport_address/0,
              intAgentTransport/0
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

-doc """
Because of limitations of the Erlang type language we cannot define this type in
detail. Instead, we describe it here.

The list variant, 4 bytes for address + 2 bytes for port:

`[byte() x 4, byte() x 2]`
""".
-type transportAddressIPv4WithPort() ::
   {transportAddressIPv4WithoutPort(), inet:port_number()} |
   [ byte() ].

-doc """
Because of limitations of the Erlang type language we cannot define this type in
detail. Instead, we describe it here.

The list variant, 4 bytes for address:

`[byte() x 4]`
""".
-type transportAddressIPv4WithoutPort() ::
   inet:ip4_address() |
   [ byte() ].

-type transportAddressIPv6() ::
    transportAddressIPv6WithPort() | transportAddressIPv6WithoutPort().

-doc """
Because of limitations of the Erlang type language we cannot define this type in
detail. Instead, we describe it here.

First list variant, 8 words for address + 1 word for port:

`[word() x 8, inet:port_number()]`

Second list variant, 8 words for address + 2 bytes for port:

`[word() x 8, byte() x 2]`

Third list variant, 16 bytes for address + 2 bytes for port:

`[byte() x 16, byte() x 2]`
""".
-type transportAddressIPv6WithPort() ::
   {transportAddressIPv6WithoutPort(), inet:port_number()} |
   [ word() | inet:port_number()] |
   [ word() | byte() ] |
   [ byte() ].

-doc """
Because of limitations of the Erlang type language we cannot define this type in
detail. Instead, we describe it here.

First list variant, 8 words for address:

`[word() x 8]`

Second list variant, 16 bytes for address:

`[byte() x 16]`
""".
-type transportAddressIPv6WithoutPort() ::
   inet:ip6_address() |
   [ word() ] |
   [ byte() ].

-type transportAddressMask() ::
    [] | transportAddressWithPort().

-type word() :: 0..65535.

%% -type agent_entry() :: term().
-doc "An opaque term that represents an entry in the 'agent' config.".
-opaque agent_entry() :: {Tag :: atom(), Value :: term()}.

%% -type community_entry() :: term().
-doc "An opaque term that represents an entry in the 'community' (agent) config.".
-opaque community_entry() ::
          {
           CommIndex    :: snmp_community_mib:index(),
           CommName     :: snmp_community_mib:name(),
           SecName      :: snmp_community_mib:security_name(),
           CtxName      :: snmp_community_mib:context_name(),
           TransportTag :: snmp_community_mib:transport_tag()
          }.

%% -type context_entry() :: term().
-doc "An opaque term that represents an entry in the 'context' (agent) config.".
-opaque context_entry() :: snmp_community_mib:context_name().

%% -type notify_entry() :: term().
-doc "An opaque term that represents an entry in the 'notify' (agent) config.".
-opaque notify_entry() ::
          {
           Name :: snmp_notification_mib:notify_name(),
           Tag  :: snmp_notification_mib:notify_tag(),
           Type :: snmp_notification_mib:notify_type()
          }.

%% -type standard_entry() :: term().
-doc "An opaque term that represents an entry in the 'standard' (agent) config.".
-opaque standard_entry() :: {Tag :: atom(), Value :: term()}.

%% -type target_addr_entry() :: term().
-doc "An opaque term that represents an entry in the 'target address' (agent) config.".
-opaque target_addr_entry() ::
          {
           Name       :: snmp_target_mib:name(),
           Domain     :: transportDomain(),
           Addr       :: transportAddress(),
           Timeout    :: snmp:time_interval(),
           RetryCount :: snmp_target_mib:retry_count(), 
           TagList    :: snmp_target_mib:tag_list(),
           ParamsName :: snmp_target_mib:params(),
           EngineId   :: snmp_framework_mib:engine_id(),
           TMask      :: snmp_target_mib:tmask(),
           MMS        :: snmp_target_mib:mms()
          }.

%% -type target_params_entry() :: term().
-doc """
An opaque term that represents an entry in the 'target parameters' (agent)
config.
""".
-opaque target_params_entry() ::
          {
           Name     :: snmp_target_mib:name(),
           MPModel  :: snmp_framework_mib:message_processing_model(),
           SecModel :: snmp_framework_mib:security_model(),
           SecName  :: snmp_framework_mib:admin_string(),
           SecLevel :: snmp_framework_mib:security_level()
          }.

%% -type usm_entry() :: term().
-doc "An opaque term that represents an entry in the 'user based sm' (agent) config.".
-opaque usm_entry() ::
          {
           EngineID    :: snmp_framework_mib:engine_id(),
           UserName    :: snmp_user_based_sm_mib:name(),
           SecName     :: snmp_framework_mib:admin_string(),
           Clone       :: snmp_user_based_sm_mib:clone_from(), 
           AuthP       :: snmp_user_based_sm_mib:auth_protocol(),
           AuthKeyC    :: snmp_user_based_sm_mib:key_change(),
           OwnAuthKeyC :: snmp_user_based_sm_mib:key_change(),
           PrivP       :: snmp_user_based_sm_mib:priv_protocol(),
           PrivKeyC    :: snmp_user_based_sm_mib:key_change(),
           OwnPrivKeyC :: snmp_user_based_sm_mib:key_change(),
           Public      :: snmp_user_based_sm_mib:public(),
           AuthKey     :: snmp_user_based_sm_mib:auth_key(),
           PrivKey     :: snmp_user_based_sm_mib:priv_key()
          }.

-doc """
An basically opaque term that represents an entry in the 'view based acm'
(agent) config.
""".
-type vacm_entry() :: vacm_s2g_entry() |
                      vacm_acc_entry() |
                      vacm_vtf_entry().
-doc """
An opaque term that represents an (access) entry in the 'vacm access' (agent)
config.
""".
-opaque vacm_acc_entry() ::
          {
           vacmAccess,
           GroupName :: snmp_framework_mib:admin_string(),
           Prefix    :: snmp_view_based_acm_mib:context_prefix(),
           SecModel  :: snmp_framework_mib:security_model(),
           SecLevel  :: snmp_framework_mib:security_level(),
           Match     :: snmp_view_based_acm_mib:context_match(),
           RV        :: snmp_framework_mib:admin_string(),
           WV        :: snmp_framework_mib:admin_string(),
           NV        :: snmp_framework_mib:admin_string()
          }.
-doc """
An opaque term that represents an (security to group) entry in the 'vacm
security to group' (agent) config.
""".
-opaque vacm_s2g_entry() ::
          {
           vacmSecurityToGroup,
           SecModel  :: snmp_framework_mib:security_model(),
           SecName   :: snmp_view_based_acm_mib:security_name(),
           GroupName :: snmp_framework_mib:admin_string()
          }.
-doc """
An opaque term that represents an (tree family) entry in the 'vacm tree family'
(agent) config.
""".
-opaque vacm_vtf_entry() ::
          {
           vacmViewTreeFamily,
           ViewIndex   :: integer(),
           ViewSubtree :: snmp:oid(),
           ViewStatus  :: snmp_view_based_acm_mib:view_type(),
           ViewMask    :: null | snmp_view_based_acm_mib:view_mask()
          }.

-doc "`Min < Max`".
-type range()     :: {Min :: inet:port_number(), Max :: inet:port_number()}.
-type ranges()    :: [inet:port_number() | range()].
-doc """
Port number `0` (zero) cannot be specified directly (it is used internally).
Instead the atom `'system'` should be used.
""".
-type port_info() :: inet:port_number() | 'system' | range() | ranges().

-type snmp_ip_address()            :: [non_neg_integer()].
-type ip_address()                 :: inet:ip_address() |
                                      snmp_ip_address().
-type transport_address()          :: {ip_address(), inet:port_number()} |
                                      ip_address().
-type extended_transport_address() :: {inet:ip_address(), port_info()}.
-type transport_opts()             :: list().

-type intAgentTransport() ::
        {transportDomain(), transport_address()} |
        {transportDomain(), extended_transport_address(), snmpa:transport_kind()} |
        {transportDomain(), extended_transport_address(), transport_opts()} |
        {transportDomain(), extended_transport_address(), snmpa:transport_kind(), transport_opts()}.


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
Create an entry for the agent config file, `agent.conf`.

The type of `Val` depends on the value of `Tag`:

- **`intAgentTransports: [`[`snmpa_conf:intAgentTransport()`](`t:intAgentTransport/0`) ] `<mandatory>`**{: #intAgentTransports }

- **`intAgentUDPPort: `[`inet:port_number()`](`t:inet:port_number/0`) `<optional>`**{: #intAgentUDPPort }

- **`snmpEngineMaxMessageSize: `[`snmp_framework_mib:max_message_size()`](`t:snmp_framework_mib:max_message_size/0`) `<mandatory>`**{: #snmpEngineMaxMessageSize }

- **`snmpEngineID: `[`snmp_framework_mib:engine_id()`](`t:snmp_framework_mib:engine_id/0`) `<mandatory>`**{: #snmpEngineID }

See [Agent Information](snmp_agent_config_files.md#agent_information) for more
info.
""".
-spec agent_entry(Tag, Val) -> AgentEntry when
      Tag        :: intAgentTransports |
                    intAgentUDPPort |
                    snmpEngineMaxMessageSize |
                    snmpEngineID,
      Val        :: term(),
      AgentEntry :: agent_entry().

agent_entry(Tag, Val) ->
    {Tag, Val}.


-doc(#{equiv => write_agent_config/3}).
-spec write_agent_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [agent_entry()].

write_agent_config(Dir, Conf) ->
    Comment = 
"%% This file defines the Agent local configuration info\n"
"%% The data is inserted into the snmpEngine* variables defined\n"
"%% in SNMP-FRAMEWORK-MIB, and the intAgent* variables defined\n"
"%% in OTP-SNMPEA-MIB.\n"
"%% Each row is a 2-tuple:\n"
"%% {AgentVariable, Value}.\n"
"%% For example\n"
"%% {intAgentUDPPort,          4000}.\n"
"%% {intAgentTransports,       [{transportDomainUdpIpv4, {127,42,17,5}}]}.\n"
"%% {snmpEngineID,             \"agentEngine\"}.\n"
"%% {snmpEngineMaxMessageSize, 484}.\n"
"%%\n\n",
    Hdr = header() ++ Comment, 
    write_agent_config(Dir, Hdr, Conf).

-doc """
Write the agent config to the agent config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Agent Information](snmp_agent_config_files.md#agent_information) for more
info.
""".
-spec write_agent_config(Dir, Hdr, Conf) -> ok when
      Dir  :: snmp:dir(),
      Hdr  :: string(),
      Conf :: [agent_entry()].

write_agent_config(Dir, Hdr, Conf)
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_framework_mib:order_agent/2,
    Check = fun snmp_framework_mib:check_agent/2,
    Write = fun (Fd, Entries) -> write_agent_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "agent.conf", Order, Check, Write, Conf).


-doc """
Append the config to the current agent config file.

`Dir` is the path to the directory where to store the config file.

See [Agent Information](snmp_agent_config_files.md#agent_information) for more
info.
""".
-spec append_agent_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [agent_entry()].

append_agent_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_framework_mib:order_agent/2,
    Check = fun snmp_framework_mib:check_agent/2,
    Write = fun write_agent_conf/2,
    append_config_file(Dir, "agent.conf", Order, Check, Write, Conf).


-doc """
Read the current agent config file.

`Dir` is the path to the directory where to store the config file.

See [Agent Information](snmp_agent_config_files.md#agent_information) for more
info.
""".
-spec read_agent_config(Dir) -> {ok, Conf} | {error, Reason} when
      Dir    :: snmp:dir(),
      Conf   :: [agent_entry()],
      Reason :: term().

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
Create an entry for the agent context config file, `context.conf`.

See [Contexts](snmp_agent_config_files.md#context) for more info.
""".
-spec context_entry(Ctx) -> ContextEntry when
      Ctx          :: snmp_community_mib:context_name(),
      ContextEntry :: context_entry().

context_entry(Ctx) ->
    Ctx.


-doc(#{equiv => write_context_config/3}).
-spec write_context_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [context_entry()].

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
Write the agent context config to the agent context config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Contexts](snmp_agent_config_files.md#context) for more info.
""".
-spec write_context_config(Dir, Hdr, Conf) -> ok when
      Dir  :: snmp:dir(),
      Hdr  :: string(),
      Conf :: [context_entry()].

write_context_config(Dir, Hdr, Conf) 
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_context/2,
    Write = fun (Fd, Entries) -> write_context_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "context.conf", Order, Check, Write, Conf).


-doc """
Append the context config to the current agent context config file.

`Dir` is the path to the directory where to store the config file.

See [Contexts](snmp_agent_config_files.md#context) for more info.
""".
-spec append_context_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [context_entry()].

append_context_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_context/2,
    Write = fun write_context_conf/2,
    append_config_file(Dir, "context.conf", Order, Check, Write, Conf).


-doc """
Read the current agent context config file.

`Dir` is the path to the directory where to store the config file.

See [Contexts](snmp_agent_config_files.md#context) for more info.
""".
-spec read_context_config(Dir) -> {ok, Conf} | {error, Reason} when
      Dir    :: snmp:dir(),
      Conf   :: [context_entry()],
      Reason :: term().

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

-doc """
Create an entry for the agent community config file, `community.conf`.

`CommunityIndex` must be a _non-empty_ string.

This function only accepts the following values of `CommIndex`:

- **`"public"`{: #community_index_public }** - Translates to the following call:

  [`community_entry(CommunityIndex, CommunityIndex, "initial", "", "")`](`community_entry/5`).

- **`"all-rights"`{: #community_index_all_rights }** - Translates to the
  following call:

  [`community_entry(CommunityIndex, CommunityIndex, CommunityIndex, "", "")`](`community_entry/5`).

See [Community](snmp_agent_config_files.md#community) for more info.
""".
-spec community_entry(CommIndex) -> CommunityEntry when
      CommIndex      :: snmp_framework_mib:admin_string(),
      CommunityEntry :: community_entry().

community_entry(CommIndex) when CommIndex =:= "public" ->
    CommName     = CommIndex,
    SecName      = "initial",
    CtxName      = "",
    TransportTag = "",
    community_entry(CommIndex, CommName, SecName, CtxName, TransportTag);
community_entry(CommIndex) when CommIndex =:= "all-rights" ->
    CommName     = CommIndex,
    SecName      = CommIndex,
    CtxName      = "",
    TransportTag = "",
    community_entry(CommIndex, CommName, SecName, CtxName, TransportTag).

-doc """
Create an entry for the agent community config file, `community.conf`.

`CommunityIndex` must be a _non-empty_ string.

See [Community](snmp_agent_config_files.md#community) for more info.
""".
-spec community_entry(CommIndex, CommName, SecName, CtxName, TransportTag) ->
          CommunityEntry when
      CommIndex      :: snmp_community_mib:index(),
      CommName       :: snmp_community_mib:name(),
      SecName        :: snmp_community_mib:security_name(),
      CtxName        :: snmp_community_mib:context_name(),
      TransportTag   :: snmp_community_mib:transport_tag(),
      CommunityEntry :: community_entry().

community_entry(CommIndex, CommName, SecName, CtxName, TransportTag) ->
    {CommIndex, CommName, SecName, CtxName, TransportTag}.


-doc(#{equiv => write_community_config/3}).
-spec write_community_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [community_entry()].

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
Write the agent community config to the agent community config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Community](snmp_agent_config_files.md#community) for more info.
""".
-spec write_community_config(Dir, Hdr, Conf) -> ok when
      Dir  :: snmp:dir(),
      Hdr  :: string(),
      Conf :: [community_entry()].

write_community_config(Dir, Hdr, Conf)
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_community/2,
    Write = fun (Fd, Entries) -> write_community_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "community.conf", Order, Check, Write, Conf).


-doc """
Append the community config to the current agent community config file.

`Dir` is the path to the directory where to store the config file.

See [Community](snmp_agent_config_files.md#community) for more info.
""".
-spec append_community_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [community_entry()].

append_community_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_community/2,
    Write = fun write_community_conf/2,
    append_config_file(Dir, "community.conf", Order, Check, Write, Conf).


-doc """
Read the current agent community config file.

`Dir` is the path to the directory where to store the config file.

See [Communities](snmp_agent_config_files.md#community) for more info.

""".
-spec read_community_config(Dir) -> {ok, Conf} | {error, Reason} when
      Dir    :: snmp:dir(),
      Conf   :: [community_entry()],
      Reason :: term().

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
Create an entry for the agent standard config file, `standard.conf`.

The type of `Val` depends on the value of `Tag`:

- **`sysDescr: `{: #sysDescr }[`string()`](`t:erlang:string/0`)
  `<mandatory>`** - `DisplayString (SIZE(0..255))`

- **`sysObjectID: `{: #sysObjectID }[`snmp:oid()`](`t:snmp:oid/0`) `<mandatory>`** - `OBJECT IDENTIFIER`

- **`sysContact: `{: #sysContact }[`string()`](`t:erlang:string/0`)
  `<mandatory>`** - `DisplayString (SIZE(0..255))`

- **`sysName: `{: #sysName }[`string()`](`t:erlang:string/0`) `<mandatory>`** -
  `DisplayString (SIZE(0..255))`

- **`sysLocation: `{: #sysLocation }[`string()`](`t:erlang:string/0`)
  `<mandatory>`** - `DisplayString (SIZE(0..255))`

- **`sysLocation: `[`non_neg_integer()`](`t:erlang:non_neg_integer/0`) `<mandatory>`** - "A
  value which indicates the set of services that this entity primarily offers."

  `INTEGER (0..127)`

- **`snmpEnableAuthenTraps: `{: #snmpEnableAuthenTraps }`enabled | disabled`
  `<mandatory>`** - `INTEGER { enabled(1), disabled(2) }`

See [System Information](snmp_agent_config_files.md#system_information) for more
info.
""".
-spec standard_entry(Tag, Val) -> StandardEntry when
      Tag           :: sysDescr    |
                       sysObjectID |
                       sysContact  |
                       sysName     |
                       sysLocation |
                       sysServices |
                       snmpEnableAuthenTraps,
      Val           :: term(),
      StandardEntry :: standard_entry().

standard_entry(Tag, Val) ->
    {Tag, Val}.


-doc(#{equiv => write_standard_config/3}).
-spec write_standard_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [standard_entry()].

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
Write the agent standard config to the agent standard config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [System Information](snmp_agent_config_files.md#system_information) for more
info.
""".
-spec write_standard_config(Dir, Hdr, Conf) -> ok when
      Dir  :: snmp:dir(),
      Hdr  :: string(),
      Conf :: [standard_entry()].

write_standard_config(Dir, Hdr, Conf)
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_standard/2,
    Write = fun (Fd, Entries) -> write_standard_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "standard.conf", Order, Check, Write, Conf).


-doc """
Append the standard config to the current agent standard config file.

`Dir` is the path to the directory where to store the config file.

See [System Information](snmp_agent_config_files.md#system_information) for more
info.
""".
-spec append_standard_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [standard_entry()].

append_standard_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_standard/2,
    Write = fun write_standard_conf/2,
    append_config_file(Dir, "standard.conf", Order, Check, Write, Conf).


-doc """
Read the current agent standard config file.

`Dir` is the path to the directory where to store the config file.

See [System Information](snmp_agent_config_files.md#system_information) for more
info.
""".
-spec read_standard_config(Dir) -> {ok, Conf} | {error, Reason} when
      Dir    :: snmp:dir(),
      Conf   :: [standard_entry()],
      Reason :: term().

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
-spec target_addr_entry(Name, IP, TagList, ParamsName, EngineId) ->
          TargetAddrEntry when
      Name            :: snmp_target_mib:name(),
      IP              :: inet:ip_address(),
      TagList         :: snmp_target_mib:tag_list(),
      ParamsName      :: snmp_target_mib:params(),
      EngineId        :: snmp_framework_mib:engine_id(),
      TargetAddrEntry :: target_addr_entry().

target_addr_entry(
  Name, IP, TagList, ParamsName, EngineId) ->
    target_addr_entry(Name, IP, TagList, ParamsName, EngineId, []).


-doc """
Create an entry for the agent target_addr config file, `target_addr.conf`.

`Name` must be a _non-empty_ string.

[`target_addr_entry/6` (1)](`target_addr_entry/6`) translates to the following call:
[`target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, [])`](`target_addr_entry/7`).

[`target_addr_entry/6` (2)](`target_addr_entry/6`) translates to the following call (with `Domain` and `Addr` built from `IP` and the default port number):
[`target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, TMask, 2048)`](`target_addr_entry/8`).

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.
""".
-spec target_addr_entry(Name, Domain, Addr, TagList,
                        ParamsName, EngineId) -> TargetAddrEntry when
      Name            :: snmp_target_mib:name(),
      Domain          :: transportDomain(),
      Addr            :: transportAddress(),
      TagList         :: snmp_target_mib:tag_list(),
      ParamsName      :: snmp_target_mib:params(),
      EngineId        :: snmp_framework_mib:engine_id(),
      TargetAddrEntry :: target_addr_entry();
                     (Name, IP, TagList, ParamsName,
                      EngineId, TMask) ->  TargetAddrEntry when
      Name            :: snmp_target_mib:name(),
      IP              :: inet:ip_address(),
      TagList         :: snmp_target_mib:tag_list(),
      ParamsName      :: snmp_target_mib:params(),
      EngineId        :: snmp_framework_mib:engine_id(),
      TMask           :: snmp_target_mib:tmask(),
      TargetAddrEntry :: target_addr_entry().
           
target_addr_entry(
  Name, Domain, Addr, TagList,
  ParamsName, EngineId) when is_atom(Domain) ->
    target_addr_entry(
      Name, Domain, Addr, TagList,
      ParamsName, EngineId, []);
target_addr_entry(
  Name, IP, TagList, ParamsName,
  EngineId, TMask) when (?ip4(IP) orelse ?ip6(IP)) ->
    {Domain, Addr} = ip_and_port_to_taddr(IP, 162),
    target_addr_entry(
      Name, Domain, Addr, TagList, ParamsName,
      EngineId, TMask, 2048).


-doc """
Create an entry for the agent target_addr config file, `target_addr.conf`.

`Name` must be a _non-empty_ string.

[`target_addr_entry/7` (1)](`target_addr_entry/7`) translates to the following call:
[`target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, TMask, 2048)`](`target_addr_entry/8`).

[`target_addr_entry/7` (2)](`target_addr_entry/7`) translates to the following call (with `Domain` and `Addr` built from `IP` and `Port`):
[`target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, TMask, 2048)`](`target_addr_entry/8`).

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.
""".
-doc(#{since => <<"OTP 17.3">>}).
-spec target_addr_entry(Name, Domain, Addr, TagList,
                        ParamsName, EngineId, TMask) -> TargetAddrEntry when
      Name            :: snmp_target_mib:name(),
      Domain          :: transportDomain(),
      Addr            :: transportAddress(),
      TagList         :: snmp_target_mib:tag_list(),
      ParamsName      :: snmp_target_mib:params(),
      EngineId        :: snmp_framework_mib:engine_id(),
      TMask           :: snmp_target_mib:tmask(),
      TargetAddrEntry :: target_addr_entry();
                     (Name, IP, Port, TagList, ParamsName,
                      EngineId, TMask) ->  TargetAddrEntry when
      Name            :: snmp_target_mib:name(),
      IP              :: inet:ip_address(),
      Port            :: inet:port_number(),
      TagList         :: snmp_target_mib:tag_list(),
      ParamsName      :: snmp_target_mib:params(),
      EngineId        :: snmp_framework_mib:engine_id(),
      TMask           :: snmp_target_mib:tmask(),
      TargetAddrEntry :: target_addr_entry().

target_addr_entry(
  Name, Domain, Addr, TagList,
  ParamsName, EngineId, TMask) when is_atom(Domain) ->
    target_addr_entry(
      Name, Domain, Addr, TagList,
      ParamsName, EngineId, TMask, 2048);
target_addr_entry(
  Name, IP, Port, TagList,
  ParamsName, EngineId, TMask)
  when (?ip4(IP) orelse ?ip6(IP)) andalso ?port(Port) ->
    {Domain, Addr} = ip_and_port_to_taddr(IP, Port),
    target_addr_entry(
      Name, Domain, Addr, TagList,
      ParamsName, EngineId, TMask, 2048).

-doc """
Create an entry for the agent target_addr config file, `target_addr.conf`.

`Name` must be a _non-empty_ string.

[`target_addr_entry/8` (1)](`target_addr_entry/8`) translates to the following call:
[`target_addr_entry(Name, Domain, Addr, 1500, 3, TagList, ParamsName, EngineId, TMask, MaxMessageSize)`](`target_addr_entry/10`).

[`target_addr_entry/8` (2)](`target_addr_entry/8`) translates to the following call (with `Domain` and `Addr` built from `IP` and `Port`):
[`target_addr_entry(Name, Domain, Addr, 1500, 3, TagList, ParamsName, EngineId, TMask, MaxMessageSize)`](`target_addr_entry/10`).

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.
""".
-spec target_addr_entry(Name, Domain, Addr, TagList,
                        ParamsName, EngineId, TMask, MaxMessageSize) ->
          TargetAddrEntry when
      Name            :: snmp_target_mib:name(),
      Domain          :: transportDomain(),
      Addr            :: transportAddress(),
      TagList         :: snmp_target_mib:tag_list(),
      ParamsName      :: snmp_target_mib:params(),
      EngineId        :: snmp_framework_mib:engine_id(),
      TMask           :: snmp_target_mib:tmask(),
      MaxMessageSize  :: snmp_target_mib:mms(),
      TargetAddrEntry :: target_addr_entry();
                       (Name, IP, Port, TagList,
                        ParamsName, EngineId, TMask, MaxMessageSize) -> 
          TargetAddrEntry when
      Name            :: snmp_target_mib:name(),
      IP              :: inet:ip_address(),
      Port            :: inet:port_number(),
      TagList         :: snmp_target_mib:tag_list(),
      ParamsName      :: snmp_target_mib:params(),
      EngineId        :: snmp_framework_mib:engine_id(),
      TMask           :: snmp_target_mib:tmask(),
      MaxMessageSize  :: snmp_target_mib:mms(),
      TargetAddrEntry :: target_addr_entry().

target_addr_entry(
  Name, Domain, Addr, TagList,
  ParamsName, EngineId, TMask, MaxMessageSize) when is_atom(Domain) ->
    target_addr_entry(
      Name, Domain, Addr, 1500, 3, TagList,
      ParamsName, EngineId, TMask, MaxMessageSize);
target_addr_entry(
  Name, IP, Port, TagList,
  ParamsName, EngineId, TMask, MaxMessageSize)
  when (?ip4(IP) orelse ?ip6(IP)) andalso ?port(Port) ->
    {Domain, Addr} = ip_and_port_to_taddr(IP, Port),
    target_addr_entry(
      Name, Domain, Addr, 1500, 3, TagList,
      ParamsName, EngineId, TMask, MaxMessageSize).

-doc """
Create an entry for the agent target_addr config file, `target_addr.conf`.

`Name` must be a _non-empty_ string.

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.
""".
-spec target_addr_entry(Name,
                        Domain, Addr,
                        Timeout, RetryCount, TagList,
                        ParamsName, EngineId, TMask, MaxMessageSize) ->
          TargetAddrEntry when
      Name            :: snmp_target_mib:name(),
      Domain          :: transportDomain(),
      Addr            :: transportAddress(),
      Timeout         :: snmp:time_interval(),
      RetryCount      :: snmp_target_mib:retry_count(),
      TagList         :: snmp_target_mib:tag_list(),
      ParamsName      :: snmp_framework_mib:admin_string(),
      EngineId        :: snmp_framework_mib:engine_id(),
      TMask           :: snmp_target_mib:tmask(),
      MaxMessageSize  :: snmp_target_mib:mms(),
      TargetAddrEntry :: target_addr_entry().

target_addr_entry(
  Name, Domain, Addr, Timeout, RetryCount, TagList,
  ParamsName, EngineId, TMask, MaxMessageSize) when is_atom(Domain) ->
    {Name, Domain, Addr, Timeout, RetryCount, TagList,
     ParamsName, EngineId, TMask, MaxMessageSize}.


-doc false.
target_addr_entry(
  Name, Domain, Ip, Udp, Timeout, RetryCount, TagList,
  ParamsName, EngineId, TMask, MaxMessageSize) ->
    {Name, Domain, Ip, Udp, Timeout, RetryCount, TagList,
     ParamsName, EngineId, TMask, MaxMessageSize}.


-spec ip_and_port_to_taddr(IP, Port) -> {TDomain, TAddr} when
      IP      :: inet:ip_address(),
      Port    :: inet:port_number(),
      TDomain :: transportDomain(),
      TAddr   :: transportAddress().

ip_and_port_to_taddr(IP, Port)
  when ?ip4(IP) andalso ?port(Port) ->
    TAddr   = {IP, Port},
    TDomain = transportDomainUdpIpv4,
    {TDomain, TAddr};
ip_and_port_to_taddr(IP, Port)
  when ?ip6(IP) andalso ?port(Port) ->
    TAddr   = {IP, Port},
    TDomain = transportDomainUdpIpv6,
    {TDomain, TAddr}.


-doc(#{equiv => write_target_addr_config/3}).
-spec write_target_addr_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [target_addr_entry()].

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
Write the agent target_addr config to the agent target_addr config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.
""".
-spec write_target_addr_config(Dir, Hdr, Conf) -> ok when
      Dir  :: snmp:dir(),
      Hdr  :: string(),
      Conf :: [target_addr_entry()].

write_target_addr_config(Dir, Hdr, Conf)
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_addr/2,
    Write = fun (Fd, Entries) -> write_target_addr_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "target_addr.conf", Order, Check, Write, Conf).


-doc """
Append the target_addr config to the current agent target_addr config file.

`Dir` is the path to the directory where to store the config file.

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.
""".
-spec append_target_addr_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [target_addr_entry()].

append_target_addr_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_addr/2,
    Write = fun write_target_addr_conf/2,
    append_config_file(Dir, "target_addr.conf", Order, Check, Write, Conf).


-doc """
Read the current agent target_addr config file.

`Dir` is the path to the directory where to store the config file.

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

""".
-spec read_target_addr_config(Dir) -> {ok, Conf} | {error, Reason} when
      Dir    :: snmp:dir(),
      Conf   :: [target_addr_entry()],
      Reason :: term().

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

-doc """
Create an entry for the agent target_params config file, `target_params.conf`.

`Name` must be a _non-empty_ string.

[`target_params_entry/2`](`target_params_entry/2`) translates to the following
call:

```text
	  target_params_entry(Name, Vsn, "initial", noAuthNoPriv)
```

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

""".
-spec target_params_entry(Name, Vsn) -> TargetParamsEntry when
      Name              :: snmp_target_mib:name(),
      Vsn               :: snmp:version(),
      TargetParamsEntry :: target_params_entry().

target_params_entry(Name, Vsn) ->
    SecName  = "initial",
    SecLevel = noAuthNoPriv,
    target_params_entry(Name, Vsn, SecName, SecLevel).

-doc """
Create an entry for the agent target_params config file, `target_params.conf`.

`Name` must be a _non-empty_ string.

`Vsn` translates into `MPModel` and `SecModel` as follows:

```text
	  Vsn = v1 => MPModel = v1,  SecModel = v1
	  Vsn = v2 => MPModel = v2c, SecModel = v2c
	  Vsn = v3 => MPModel = v3,  SecModel = usm
```

[`target_params_entry/4`](`target_params_entry/4`) translates to the following
call:

```text
	  target_params_entry(Name, MPModel, SecModel, SecName, SecLevel)
```

Where `MPModel` and `SecModel` is mapped from `Vsn`, see above.

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

""".
-spec target_params_entry(Name, Vsn, SecName, SecLevel) ->
          TargetParamsEntry when
      Name              :: snmp_target_mib:name(),
      Vsn               :: snmp:version(),
      SecName           :: snmp_framework_mib:admin_string(),
      SecLevel          :: snmp_framework_mib:security_level(),
      TargetParamsEntry :: target_params_entry().

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
Create an entry for the agent target_params config file, `target_params.conf`.

`Name` must be a _non-empty_ string.

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.
""".
-spec target_params_entry(Name, MPModel, SecModel, SecName, SecLevel) ->
          TargetParamsEntry when
      Name              :: snmp_target_mib:name(),
      MPModel           :: snmp_framework_mib:message_processing_model(),
      SecModel          :: snmp_framework_mib:security_model(),
      SecName           :: snmp_framework_mib:admin_string(),
      SecLevel          :: snmp_framework_mib:security_level(),
      TargetParamsEntry :: target_params_entry().

target_params_entry(Name, MPModel, SecModel, SecName, SecLevel) ->
    {Name, MPModel, SecModel, SecName, SecLevel}.
    

-doc(#{equiv => write_target_params_config/3}).
-spec write_target_params_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [target_params_entry()].

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
Write the agent target_params config to the agent target_params config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.
""".
-spec write_target_params_config(Dir, Hdr, Conf) -> ok when
      Dir  :: snmp:dir(),
      Hdr  :: string(),
      Conf :: [target_params_entry()].

write_target_params_config(Dir, Hdr, Conf)
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_params/2,
    Write = fun (Fd, Entries) -> write_target_params_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "target_params.conf", Order, Check, Write, Conf).


-doc """
Append the target_params config to the current agent target_params config file.

`Dir` is the path to the directory where to store the config file.

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.
""".
-spec append_target_params_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [target_params_entry()].

append_target_params_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_params/2,
    Write = fun write_target_params_conf/2,
    append_config_file(Dir, "target_params.conf", Order, Check, Write, Conf).


-doc """
Read the current agent target_params config file.

`Dir` is the path to the directory where to store the config file.

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

""".
-spec read_target_params_config(Dir) -> {ok, Conf} | {error, Reason} when
      Dir    :: snmp:dir(),
      Conf   :: [target_params_entry()],
      Reason :: term().

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
Create an entry for the agent notify config file, `notify.conf`.

`Name` must be a _non-empty_ string.

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.
""".
-spec notify_entry(Name, Tag, Type) -> NotifyEntry when
      Name        :: snmp_notification_mib:notify_name(),
      Tag         :: snmp_notification_mib:notify_tag(),
      Type        :: snmp_notification_mib:notify_type(),
      NotifyEntry :: notify_entry().

notify_entry(Name, Tag, Type) ->
    {Name, Tag, Type}.


-doc(#{equiv => write_notify_config/3}).
-spec write_notify_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [notify_entry()].

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
Write the agent notify config to the agent notify config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.
""".
-spec write_notify_config(Dir, Hdr, Conf) -> ok when
      Dir  :: snmp:dir(),
      Hdr  :: string(),
      Conf :: [notify_entry()].

write_notify_config(Dir, Hdr, Conf)
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_notify/2,
    Write = fun (Fd, Entries) -> write_notify_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "notify.conf", Order, Check, Write, Conf).


-doc """
Append the notify config to the current agent notify config file.

`Dir` is the path to the directory where to store the config file.

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.
""".
-spec append_notify_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [notify_entry()].

append_notify_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_notify/2,
    Write = fun write_notify_conf/2,
    append_config_file(Dir, "notify.conf", Order, Check, Write, Conf).


-doc """
Read the current agent notify config file.

`Dir` is the path to the directory where to store the config file.

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.
""".
-spec read_notify_config(Dir) -> {ok, Conf} | {error, Reason} when
      Dir    :: snmp:dir(),
      Conf   :: [notify_entry()],
      Reason :: term().

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

-doc """
Create an entry for the agent usm config file, `usm.conf`.

[`usm_entry/1`](`usm_entry/1`) translates to the following call:

```text
	  usm_entry(EngineID,
	            "initial", "initial", zeroDotZero,
		    usmNoAuthProtocol, "", "",
		    usmNoPrivProtocol, "", "",
		    "", "", "").
```

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.
""".
-spec usm_entry(EngineID) -> UsmEntry when
      EngineID :: snmp_framework_mib:engine_id(),
      UsmEntry :: usm_entry().

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
Create an entry for the agent usm config file, `usm.conf`.

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.
""".
-spec usm_entry(EngineID,
                UserName, SecName,
                Clone,
                AuthP, AuthKeyC, OwnAuthKeyC,
                PrivP, PrivKeyC, OwnPrivKeyC,
                Public, AuthKey, PrivKey) -> UsmEntry when
      EngineID    :: snmp_framework_mib:engine_id(),
      UserName    :: snmp_user_based_sm_mib:name(),
      SecName     :: snmp_framework_mib:admin_string(),
      Clone       :: snmp_user_based_sm_mib:clone_from(), 
      AuthP       :: snmp_user_based_sm_mib:auth_protocol(),
      AuthKeyC    :: snmp_user_based_sm_mib:key_change(),
      OwnAuthKeyC :: snmp_user_based_sm_mib:key_change(),
      PrivP       :: snmp_user_based_sm_mib:priv_protocol(),
      PrivKeyC    :: snmp_user_based_sm_mib:key_change(),
      OwnPrivKeyC :: snmp_user_based_sm_mib:key_change(),
      Public      :: snmp_user_based_sm_mib:public(),
      AuthKey     :: snmp_user_based_sm_mib:auth_key(),
      PrivKey     :: snmp_user_based_sm_mib:priv_key(),
      UsmEntry    :: usm_entry().

usm_entry(EngineID, UserName, SecName, Clone, 
	  AuthP, AuthKeyC, OwnAuthKeyC,
	  PrivP, PrivKeyC, OwnPrivKeyC,
	  Public, AuthKey, PrivKey) ->
    {EngineID, UserName, SecName, Clone, 
     AuthP, AuthKeyC, OwnAuthKeyC,
     PrivP, PrivKeyC, OwnPrivKeyC,
     Public, AuthKey, PrivKey}.
    

-doc(#{equiv => write_usm_config/3}).
-spec write_usm_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [usm_entry()].

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
Write the agent usm config to the agent usm config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.
""".
-spec write_usm_config(Dir, Hdr, Conf) -> ok when
      Dir  :: snmp:dir(),
      Hdr  :: string(),
      Conf :: [usm_entry()].

write_usm_config(Dir, Hdr, Conf)
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_usm/2,
    Write = fun (Fd, Entries) -> write_usm_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "usm.conf", Order, Check, Write, Conf).


-doc """
Append the usm config to the current agent usm config file.

`Dir` is the path to the directory where to store the config file.

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.
""".
-spec append_usm_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [usm_entry()].

append_usm_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_usm/2,
    Write = fun write_usm_conf/2,
    append_config_file(Dir, "usm.conf", Order, Check, Write, Conf).


-doc """
Read the current agent usm config file.

`Dir` is the path to the directory where to store the config file.

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.
""".
-spec read_usm_config(Dir) -> {ok, Conf} | {error, Reason} when
      Dir    :: snmp:dir(),
      Conf   :: [usm_entry()],
      Reason :: term().

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

-doc """
Create an (access) entry for the agent vacm config file, `vacm.conf`.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

""".
-spec vacm_acc_entry(GroupName, Prefix, SecModel, SecLevel, Match,
                     RV, WV, NV) -> VacmAccEntry when
      GroupName    :: snmp_framework_mib:admin_string(),
      Prefix       :: snmp_view_based_acm_mib:context_prefix(),
      SecModel     :: snmp_framework_mib:security_model(),
      SecLevel     :: snmp_framework_mib:security_level(),
      Match        :: snmp_view_based_acm_mib:context_match(),
      RV           :: snmp_framework_mib:admin_string(),
      WV           :: snmp_framework_mib:admin_string(),
      NV           :: snmp_framework_mib:admin_string(),
      VacmAccEntry :: vacm_acc_entry().
      
vacm_acc_entry(GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV) ->
    {vacmAccess, GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV}.

-doc """
Create an (security to group) entry for the agent vacm config file, `vacm.conf`.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.
""".
-spec vacm_s2g_entry(SecModel, SecName, GroupName) -> VacmS2GEntry when
      SecModel     :: snmp_framework_mib:security_model(),
      SecName      :: snmp_view_based_acm_mib:security_name(),
      GroupName    :: snmp_framework_mib:admin_string(),
      VacmS2GEntry :: vacm_s2g_entry().

vacm_s2g_entry(SecModel, SecName, GroupName) ->
    {vacmSecurityToGroup, SecModel, SecName, GroupName}.

-doc """
Create an (view tree family) entry for the agent vacm config file, `vacm.conf`.

[`vacm_vtf_entry/2`](`vacm_vtf_entry/2`) translates to the following call:

```text
	  vacm_vtf_entry(ViewIndex, ViewSubtree, included, null).
```

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.
""".
-spec vacm_vtf_entry(ViewName, ViewSubtree) -> VacmVtfEntry when
      ViewName     :: snmp_framework_mib:admin_string(),
      ViewSubtree  :: snmp:oid(),
      VacmVtfEntry :: VacmVtfEntry.

vacm_vtf_entry(ViewName, ViewSubtree) ->
    vacm_vtf_entry(ViewName, ViewSubtree, included, null).

-doc """
Create an (view tree family) entry for the agent vacm config file, `vacm.conf`.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.
""".
-spec vacm_vtf_entry(ViewName, ViewSubtree, ViewType, ViewMask) ->
          VacmVtfEntry when
      ViewName     :: snmp_framework_mib:admin_string(),
      ViewSubtree  :: snmp:oid(),
      ViewType     :: snmp_view_based_acm_mib:view_type(),
      ViewMask     :: null | snmp_view_based_acm_mib:view_mask(),
      VacmVtfEntry :: VacmVtfEntry.

vacm_vtf_entry(ViewName, ViewSubtree, ViewType, ViewMask) ->
    {vacmViewTreeFamily, ViewName, ViewSubtree, ViewType, ViewMask}.


-doc(#{equiv => write_vacm_config/3}).
-spec write_vacm_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [vacm_entry()].

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
Write the agent vacm config to the agent vacm config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.
""".
-spec write_vacm_config(Dir, Hdr, Conf) -> ok when
      Dir  :: snmp:dir(),
      Hdr  :: string(),
      Conf :: [vacm_entry()].

write_vacm_config(Dir, Hdr, Conf)
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_vacm/2,
    Write = fun (Fd, Entries) -> write_vacm_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "vacm.conf", Order, Check, Write, Conf).


-doc """
Append the vacm config to the current agent vacm config file.

`Dir` is the path to the directory where to store the config file.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.
""".
-spec append_vacm_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [vacm_entry()].

append_vacm_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_vacm/2,
    Write = fun write_vacm_conf/2,
    append_config_file(Dir, "vacm.conf", Order, Check, Write, Conf).


-doc """
Read the current agent vacm config file.

`Dir` is the path to the directory where to store the config file.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.
""".
-spec read_vacm_config(Dir) -> {ok, Conf} | {error, Reason} when
      Dir    :: snmp:dir(),
      Conf   :: [vacm_entry()],
      Reason :: term().

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
