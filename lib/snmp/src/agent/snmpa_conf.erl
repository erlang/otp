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

-type transportAddressIPv4WithPort() ::
   {transportAddressIPv4WithoutPort(), inet:port_number()} |
   [ byte() ].

-type transportAddressIPv4WithoutPort() ::
   inet:ip4_address() |
   [ byte() ].

-type transportAddressIPv6() ::
    transportAddressIPv6WithPort() | transportAddressIPv6WithoutPort().

-type transportAddressIPv6WithPort() ::
   {transportAddressIPv6WithoutPort(), inet:port_number()} |
   [ word() | inet:port_number()] |
   [ word() | byte() ] |
   [ byte() ].

-type transportAddressIPv6WithoutPort() ::
   inet:ip6_address() |
   [ word() ] |
   [ byte() ].

-type transportAddressMask() ::
    [] | transportAddressWithPort().

-type word() :: 0..65535.

%% -type agent_entry() :: term().
-opaque agent_entry() :: {Tag :: atom(), Value :: term()}.

%% -type community_entry() :: term().
-opaque community_entry() ::
          {
           CommIndex    :: snmp_community_mib:index(),
           CommName     :: snmp_community_mib:name(),
           SecName      :: snmp_community_mib:security_name(),
           CtxName      :: snmp_community_mib:context_name(),
           TransportTag :: snmp_community_mib:transport_tag()
          }.

%% -type context_entry() :: term().
-opaque context_entry() :: snmp_community_mib:context_name().

%% -type notify_entry() :: term().
-opaque notify_entry() ::
          {
           Name :: snmp_notification_mib:notify_name(),
           Tag  :: snmp_notification_mib:notify_tag(),
           Type :: snmp_notification_mib:notify_type()
          }.

%% -type standard_entry() :: term().
-opaque standard_entry() :: {Tag :: atom(), Value :: term()}.

%% -type target_addr_entry() :: term().
-opaque target_addr_entry() ::
          {
           Name       :: snmp_target_mib:name(),
           Domain     :: transportDomain(),
           Addr       :: transportAddress(),
           TagList    :: snmp_target_mib:tag_list(),
           ParamsName :: snmp_target_mib:params(),
           EngineId   :: snmp_framework_mib:engine_id(),
           TMask      :: snmp_target_mib:tmask(),
           MMS        :: snmp_target_mib:mms()
          }.

%% -type target_params_entry() :: term().
-opaque target_params_entry() ::
          {
           Name     :: snmp_target_mib:name(),
           MPModel  :: snmp_framework_mib:message_processing_model(),
           SecModel :: snmp_framework_mib:security_model(),
           SecName  :: snmp_framework_mib:admin_string(),
           SecLevel :: snmp_framework_mib:security_level()
          }.

%% -type usm_entry() :: term().
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

-type vacm_entry() :: vacm_s2g_entry() |
                      vacm_acc_entry() |
                      vacm_vtf_entry().
-opaque vacm_s2g_entry() ::
          {
           vacmSecurityToGroup,
           SecModel  :: snmp_framework_mib:security_model(),
           SecName   :: snmp_view_based_acm_mib:security_name(),
           GroupName :: snmp_framework_mib:admin_string()
          }.
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
-opaque vacm_vtf_entry() ::
          {
           vacmViewTreeFamily,
           ViewIndex   :: integer(),
           ViewSubtree :: snmp:oid(),
           ViewStatus  :: snmp_view_based_acm_mib:view_type(),
           ViewMask    :: null | snmp_view_based_acm_mib:view_mask()
          }.

-type range()     :: {Min :: inet:port_number(), Max :: inet:port_number()}.
-type ranges()    :: [inet:port_number() | range()].
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

-spec agent_entry(Tag, Val) -> AgentEntry when
      Tag        :: intAgentTransports |
                    intAgentUDPPort |
                    snmpEngineMaxMessageSize |
                    snmpEngineID,
      Val        :: term(),
      AgentEntry :: agent_entry().

agent_entry(Tag, Val) ->
    {Tag, Val}.


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

write_agent_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_framework_mib:order_agent/2,
    Check = fun snmp_framework_mib:check_agent/2,
    Write = fun (Fd, Entries) -> write_agent_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "agent.conf", Order, Check, Write, Conf).


-spec append_agent_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [agent_entry()].

append_agent_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_framework_mib:order_agent/2,
    Check = fun snmp_framework_mib:check_agent/2,
    Write = fun write_agent_conf/2,
    append_config_file(Dir, "agent.conf", Order, Check, Write, Conf).


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

-spec context_entry(Ctx) -> ContextEntry when
      Ctx          :: snmp_community_mib:context_name(),
      ContextEntry :: context_entry().

context_entry(Ctx) ->
    Ctx.


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

write_context_config(Dir, Hdr, Conf) 
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_context/2,
    Write = fun (Fd, Entries) -> write_context_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "context.conf", Order, Check, Write, Conf).


-spec append_context_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [context_entry()].

append_context_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_context/2,
    Write = fun write_context_conf/2,
    append_config_file(Dir, "context.conf", Order, Check, Write, Conf).


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

write_community_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_community/2,
    Write = fun (Fd, Entries) -> write_community_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "community.conf", Order, Check, Write, Conf).


-spec append_community_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [community_entry()].

append_community_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_community/2,
    Write = fun write_community_conf/2,
    append_config_file(Dir, "community.conf", Order, Check, Write, Conf).


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

standard_entry(Tag, Val) ->
    {Tag, Val}.


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

write_standard_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_standard/2,
    Write = fun (Fd, Entries) -> write_standard_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "standard.conf", Order, Check, Write, Conf).


-spec append_standard_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [standard_entry()].

append_standard_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_standard/2,
    Write = fun write_standard_conf/2,
    append_config_file(Dir, "standard.conf", Order, Check, Write, Conf).

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

target_addr_entry(
  Name, Ip, TagList, ParamsName, EngineId) ->
    target_addr_entry(Name, Ip, TagList, ParamsName, EngineId, []).

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

target_addr_entry(
  Name, Domain_or_Ip, Addr_or_Port, TagList,
  ParamsName, EngineId, TMask) ->
    target_addr_entry(
      Name, Domain_or_Ip, Addr_or_Port, TagList,
      ParamsName, EngineId, TMask, 2048).

target_addr_entry(
  Name, Domain_or_Ip, Addr_or_Port, TagList,
  ParamsName, EngineId, TMask, MaxMessageSize) ->
    target_addr_entry(
      Name, Domain_or_Ip, Addr_or_Port, 1500, 3, TagList,
      ParamsName, EngineId, TMask, MaxMessageSize).

target_addr_entry(
  Name, Domain_or_Ip, Addr_or_Port, Timeout, RetryCount, TagList,
  ParamsName, EngineId, TMask, MaxMessageSize) ->
    {Name, Domain_or_Ip, Addr_or_Port, Timeout, RetryCount, TagList,
     ParamsName, EngineId, TMask, MaxMessageSize}.

target_addr_entry(
  Name, Domain, Ip, Udp, Timeout, RetryCount, TagList,
  ParamsName, EngineId,TMask, MaxMessageSize) ->
    {Name, Domain, Ip, Udp, Timeout, RetryCount, TagList,
     ParamsName, EngineId, TMask, MaxMessageSize}.


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

write_target_addr_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_addr/2,
    Write = fun (Fd, Entries) -> write_target_addr_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "target_addr.conf", Order, Check, Write, Conf).


-spec append_target_addr_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [target_addr_entry()].

append_target_addr_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_addr/2,
    Write = fun write_target_addr_conf/2,
    append_config_file(Dir, "target_addr.conf", Order, Check, Write, Conf).

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

target_params_entry(Name, Vsn) ->
    SecName  = "initial",
    SecLevel = noAuthNoPriv,
    target_params_entry(Name, Vsn, SecName, SecLevel).

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

target_params_entry(Name, MPModel, SecModel, SecName, SecLevel) ->
    {Name, MPModel, SecModel, SecName, SecLevel}.
    

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

write_target_params_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_params/2,
    Write = fun (Fd, Entries) -> write_target_params_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "target_params.conf", Order, Check, Write, Conf).


-spec append_target_params_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [target_params_entry()].

append_target_params_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_target_params/2,
    Write = fun write_target_params_conf/2,
    append_config_file(Dir, "target_params.conf", Order, Check, Write, Conf).

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

-spec notify_entry(Name, Tag, Type) -> NotifyEntry when
      Name        :: snmp_notification_mib:notify_name(),
      Tag         :: snmp_notification_mib:notify_tag(),
      Type        :: snmp_notification_mib:notify_type(),
      NotifyEntry :: notify_entry().

notify_entry(Name, Tag, Type) ->
    {Name, Tag, Type}.


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

write_notify_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_notify/2,
    Write = fun (Fd, Entries) -> write_notify_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "notify.conf", Order, Check, Write, Conf).


-spec append_notify_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [notify_entry()].

append_notify_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_notify/2,
    Write = fun write_notify_conf/2,
    append_config_file(Dir, "notify.conf", Order, Check, Write, Conf).


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

usm_entry(EngineID, UserName, SecName, Clone, 
	  AuthP, AuthKeyC, OwnAuthKeyC,
	  PrivP, PrivKeyC, OwnPrivKeyC,
	  Public, AuthKey, PrivKey) ->
    {EngineID, UserName, SecName, Clone, 
     AuthP, AuthKeyC, OwnAuthKeyC,
     PrivP, PrivKeyC, OwnPrivKeyC,
     Public, AuthKey, PrivKey}.
    

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

write_usm_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_usm/2,
    Write = fun (Fd, Entries) -> write_usm_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "usm.conf", Order, Check, Write, Conf).


-spec append_usm_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [usm_entry()].

append_usm_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_usm/2,
    Write = fun write_usm_conf/2,
    append_config_file(Dir, "usm.conf", Order, Check, Write, Conf).

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

vacm_s2g_entry(SecModel, SecName, GroupName) ->
    {vacmSecurityToGroup, SecModel, SecName, GroupName}.

vacm_acc_entry(GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV) ->
    {vacmAccess, GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV}.

vacm_vtf_entry(ViewIndex, ViewSubtree) ->
    vacm_vtf_entry(ViewIndex, ViewSubtree, included, null).
vacm_vtf_entry(ViewIndex, ViewSubtree, ViewStatus, ViewMask) ->
    {vacmViewTreeFamily, ViewIndex, ViewSubtree, ViewStatus, ViewMask}.


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

write_vacm_config(Dir, Hdr, Conf)
  when is_list(Dir) and is_list(Hdr) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_vacm/2,
    Write = fun (Fd, Entries) -> write_vacm_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, "vacm.conf", Order, Check, Write, Conf).


-spec append_vacm_config(Dir, Conf) -> ok when
      Dir  :: snmp:dir(),
      Conf :: [vacm_entry()].

append_vacm_config(Dir, Conf)
  when is_list(Dir) and is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_vacm/2,
    Write = fun write_vacm_conf/2,
    append_config_file(Dir, "vacm.conf", Order, Check, Write, Conf).

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
