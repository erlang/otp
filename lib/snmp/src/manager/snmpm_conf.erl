%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2019. All Rights Reserved.
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

-module(snmpm_conf).

-include_lib("kernel/include/file.hrl").

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([
	 %% manager.conf
	 manager_entry/2, 
	 write_manager_config/2, write_manager_config/3, 
	 append_manager_config/2, 
	 read_manager_config/1, 

	 %% users.conf
	 users_entry/1, users_entry/2, users_entry/3,
	 write_users_config/2, write_users_config/3, 
	 append_users_config/2, 
	 read_users_config/1, 

	 %% agents.conf
	 agents_entry/12, 
	 write_agents_config/2, write_agents_config/3, 
	 append_agents_config/2, 
	 read_agents_config/1, 

	 %% usm.conf
	 usm_entry/6, usm_entry/7, 
	 write_usm_config/2, write_usm_config/3, 
	 append_usm_config/2,
	 read_usm_config/1
	]).



-define(MANAGER_CONF_FILE,   "manager.conf").
-define(USERS_CONF_FILE,     "users.conf").
-define(AGENTS_CONF_FILE,    "agents.conf").
-define(USM_USERS_CONF_FILE, "usm.conf").

%% 
%% ------ manager.conf ------
%% 

manager_entry(Tag, Val) ->
    {Tag, Val}.


write_manager_config(Dir, Conf) -> 
    Comment = 
"%% This file defines the Manager local configuration info\n"
"%% Each row is a 2-tuple:\n"
"%% {Variable, Value}.\n"
"%% For example\n"
"%% {port,             5000}.\n"
"%% {address,          [127,42,17,5]}.\n"
"%% {engine_id,        \"managerEngine\"}.\n"
"%% {max_message_size, 484}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_manager_config(Dir, Hdr, Conf).

write_manager_config(Dir, Hdr, Conf)
  when is_list(Dir), is_list(Hdr), is_list(Conf) ->
    Order = fun snmpm_config:order_manager_config/2,
    Check = fun snmpm_config:check_manager_config/2,
    Write = fun (Fd, Entries) -> write_manager_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, ?MANAGER_CONF_FILE, Order, Check, Write, Conf).

append_manager_config(Dir, Conf) 
  when is_list(Dir), is_list(Conf) ->
    Order = fun snmpm_config:order_manager_config/2,
    Check = fun snmpm_config:check_manager_config/2,
    Write = fun write_manager_conf/2,
    append_config_file(Dir, ?MANAGER_CONF_FILE, Order, Check, Write, Conf).

read_manager_config(Dir) when is_list(Dir) ->
    Order = fun snmpm_config:order_manager_config/2,
    Check = fun snmpm_config:check_manager_config/2,
    read_config_file(Dir, ?MANAGER_CONF_FILE, Order, Check).


write_manager_conf(Fd, "", Conf) ->
    write_manager_conf(Fd, Conf);
write_manager_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_manager_conf(Fd, Conf).
    
write_manager_conf(_Fd, []) ->
    ok;
write_manager_conf(Fd, [H|T]) ->
    do_write_manager_conf(Fd, H),
    write_manager_conf(Fd, T).

do_write_manager_conf(Fd, {Tag, Val})
  when Tag =:= domain;
       Tag =:= address;
       Tag =:= port;
       Tag =:= transports;
       Tag =:= max_message_size ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_manager_conf(Fd, {Tag, Val})
  when Tag =:= engine_id ->
    io:format(Fd, "{~w, \"~s\"}.~n", [Tag, Val]);
do_write_manager_conf(_Fd, Crap) ->
    error({bad_manager_config, Crap}).


%% 
%% ------ users.conf ------
%% 

users_entry(UserId) ->
    users_entry(UserId, snmpm_user_default).

users_entry(UserId, UserMod) ->
    users_entry(UserId, UserMod, undefined).

users_entry(UserId, UserMod, UserData) ->
    users_entry(UserId, UserMod, UserData, []).

users_entry(UserId, UserMod, UserData, DefaultAgentConfig) ->
    {UserId, UserMod, UserData, DefaultAgentConfig}.


write_users_config(Dir, Conf) ->
    Comment = 
"%% This file defines the users the manager handles\n"
"%% Each row is a 4-tuple:\n"
"%% {UserId, UserMod, UserData, DefaultAgentConfig}.\n"
"%% For example\n"
"%% {kalle, kalle_callback_user_mod, \"dummy\", []}.\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_users_config(Dir, Hdr, Conf).

write_users_config(Dir, Hdr, Conf)
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_user_config/2,
    Write = fun (Fd, Entries) -> write_users_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, ?USERS_CONF_FILE, Order, Check, Write, Conf).

append_users_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_user_config/2,
    Write = fun write_users_conf/2,
    append_config_file(Dir, ?USERS_CONF_FILE, Order, Check, Write, Conf).

read_users_config(Dir) when is_list(Dir) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_user_config/2,
    read_config_file(Dir, ?USERS_CONF_FILE, Order, Check).


check_user_config(Entry, State) ->
    {check_ok(snmpm_config:check_user_config(Entry)),
     State}.

write_users_conf(Fd, "", Conf) ->
    write_users_conf(Fd, Conf);
write_users_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_users_conf(Fd, Conf).
    
write_users_conf(_Fd, []) ->
    ok;
write_users_conf(Fd, [H|T]) ->
    do_write_users_conf(Fd, H),
    write_users_conf(Fd, T).

do_write_users_conf(Fd, {Id, Mod, Data}) ->
    do_write_users_conf(Fd, {Id, Mod, Data, []});
do_write_users_conf(Fd, {Id, Mod, Data, DefaultAgentConfig}) ->
    io:format(Fd, "{~w, ~w, ~w, ~w}.~n", [Id, Mod, Data, DefaultAgentConfig]);
do_write_users_conf(_Fd, Crap) ->
   error({bad_users_config, Crap}).


%% 
%% ------ agents.conf ------
%% 

agents_entry(
  UserId, TargetName, Comm, Domain_or_Ip, Addr_or_Port, EngineID, Timeout,
  MaxMessageSize, Version, SecModel, SecName, SecLevel) ->
    {UserId, TargetName, Comm, Domain_or_Ip, Addr_or_Port, EngineID, Timeout,
     MaxMessageSize, Version, SecModel, SecName, SecLevel}.


write_agents_config(Dir, Conf) ->
    Comment = 
"%% This file defines the agents the manager handles\n"
"%% Each row is a 12-tuple:\n"
"%% {UserId, \n"
"%%  TargetName, Comm, Ip, Port, EngineID, Timeout, \n"
"%%  MaxMessageSize, Version, SecModel, SecName, SecLevel}\n"
"%%\n\n",
    Hdr = header() ++ Comment, 
    write_agents_config(Dir, Hdr, Conf).

write_agents_config(Dir, Hdr, Conf)
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_agent_config/2,
    Write = fun (Fd, Entries) -> write_agents_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, ?AGENTS_CONF_FILE, Order, Check, Write, Conf).

append_agents_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_agent_config/2,
    Write = fun write_agents_conf/2,
    append_config_file(Dir, ?AGENTS_CONF_FILE, Order, Check, Write, Conf).

read_agents_config(Dir) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_agent_config/2,
    read_config_file(Dir, ?AGENTS_CONF_FILE, Order, Check).


check_agent_config(Entry, State) ->
    {check_ok(snmpm_config:check_agent_config(Entry)),
     State}.

write_agents_conf(Fd, "", Conf) ->
    write_agents_conf(Fd, Conf);
write_agents_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_agents_conf(Fd, Conf).
    
write_agents_conf(_Fd, []) ->
    ok;
write_agents_conf(Fd, [H|T]) ->
    do_write_agents_conf(Fd, H),
    write_agents_conf(Fd, T).

do_write_agents_conf(
  Fd,
  {UserId, TargetName, Comm, Ip, Port, EngineID,
   Timeout, MaxMessageSize, Version, SecModel, SecName, SecLevel} = _A) ->
    io:format(
      Fd,
      "{~w, \"~s\", \"~s\", ~w, ~w, \"~s\", ~w, ~w, ~w, ~w, \"~s\", ~w}.~n",
      [UserId, TargetName, Comm, Ip, Port, EngineID,
       Timeout, MaxMessageSize, Version, SecModel, SecName, SecLevel]);
do_write_agents_conf(_Fd, Crap) ->
    error({bad_agents_config, Crap}).


%% 
%% ------ usm.conf -----
%% 

usm_entry(EngineID, UserName, AuthP, AuthKey, PrivP, PrivKey) ->
    {EngineID, UserName, AuthP, AuthKey, PrivP, PrivKey}.

usm_entry(EngineID, UserName, SecName, AuthP, AuthKey, PrivP, PrivKey) ->
    {EngineID, UserName, SecName, AuthP, AuthKey, PrivP, PrivKey}.


write_usm_config(Dir, Conf) ->
    Comment = 
"%% This file defines the usm users the manager handles\n"
"%% Each row is a 6 or 7-tuple:\n"
"%% {EngineID, UserName, AuthP, AuthKey, PrivP, PrivKey}\n"
"%% {EngineID, UserName, SecName, AuthP, AuthKey, PrivP, PrivKey}\n"
"%%\n\n",
    Hdr = header() ++ Comment,
    write_usm_config(Dir, Hdr, Conf).

write_usm_config(Dir, Hdr, Conf)
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_usm_user_config/2,
    Write = fun (Fd, Entries) -> write_usm_conf(Fd, Hdr, Entries) end,
    write_config_file(Dir, ?USM_USERS_CONF_FILE, Order, Check, Write, Conf).

append_usm_config(Dir, Conf)
  when is_list(Dir) andalso is_list(Conf) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_usm_user_config/2,
    Write = fun write_usm_conf/2,
    append_config_file(Dir, ?USM_USERS_CONF_FILE, Order, Check, Write, Conf).

read_usm_config(Dir) 
  when is_list(Dir) ->
    Order = fun snmp_conf:no_order/2,
    Check = fun check_usm_user_config/2,
    read_config_file(Dir, ?USM_USERS_CONF_FILE, Order, Check).


check_usm_user_config(Entry, State) ->
    {check_ok(snmpm_config:check_usm_user_config(Entry)),
     State}.

write_usm_conf(Fd, "", Conf) ->
    write_usm_conf(Fd, Conf);
write_usm_conf(Fd, Hdr, Conf) ->
    io:format(Fd, "~s~n", [Hdr]),
    write_usm_conf(Fd, Conf).
    
write_usm_conf(_Fd, []) ->
    ok;
write_usm_conf(Fd, [H|T]) ->
    do_write_usm_conf(Fd, H),
    write_usm_conf(Fd, T).

do_write_usm_conf(
  Fd,
  {EngineID, UserName, AuthP, AuthKey, PrivP, PrivKey}) ->
    io:format(
      Fd, "{\"~s\", \"~s\", ~w, ~w, ~w, ~w}.~n",
      [EngineID, UserName, AuthP, AuthKey, PrivP, PrivKey]);
do_write_usm_conf(
  Fd,
  {EngineID, UserName, SecName,
   AuthP, AuthKey, PrivP, PrivKey}) ->
    io:format(
      Fd, "{\"~s\", \"~s\", \"~s\", í~w, ~w, ~w, ~w}.~n",
      [EngineID, UserName, SecName, AuthP, AuthKey, PrivP, PrivKey]);
do_write_usm_conf(_Fd, Crap) ->
    error({bad_usm_conf, Crap}).


%% ---- config file wrapper functions ----

write_config_file(Dir, File, Order, Check, Write, Conf) ->
    snmp_config:write_config_file(Dir, File, Order, Check, Write, Conf).

append_config_file(Dir, File, Order, Check, Write, Conf) ->
    snmp_config:append_config_file(Dir, File, Order, Check, Write, Conf).

read_config_file(Dir, File, Order, Check) ->
    snmp_config:read_config_file(Dir, File, Order, Check).

%% ---- config file utility functions ----

-dialyzer({nowarn_function, check_ok/1}). % Future compat
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
