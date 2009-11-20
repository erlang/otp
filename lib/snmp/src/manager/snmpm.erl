%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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

-module(snmpm).

%%----------------------------------------------------------------------
%% This module implements a simple SNMP manager for Erlang.
%%----------------------------------------------------------------------

%% User interface
-export([
	 %% 
	 %% Management API
	 start/0, start/1, 
	 start_link/0, start_link/1, 
	 stop/0, 

	 monitor/0, demonitor/1, 
	 notify_started/1, cancel_notify_started/1, 

	 backup/1, 

	 load_mib/1, unload_mib/1, 
	 which_mibs/0, 
	 name_to_oid/1, oid_to_name/1, oid_to_type/1, 

	 register_user/3, register_user/4, 
	 register_user_monitor/3, register_user_monitor/4, 
	 unregister_user/1, 
	 which_users/0, 

	 register_agent/2, register_agent/3, register_agent/4, 
	 unregister_agent/2, unregister_agent/3,
	 which_agents/0, which_agents/1, 
	 agent_info/2, update_agent_info/4, 
	 
	 register_usm_user/3, unregister_usm_user/2, 
	 which_usm_users/0, which_usm_users/1, 
	 usm_user_info/3, update_usm_user_info/4, 
	 
	 %% 
	 %% Basic SNMP API
	 sync_get/3, sync_get/4, sync_get/5, sync_get/6, 
	 async_get/3, async_get/4, async_get/5, async_get/6, 
	 sync_get_next/3, sync_get_next/4, sync_get_next/5, sync_get_next/6, 
	 async_get_next/3, async_get_next/4, async_get_next/5, async_get_next/6, 
	 sync_set/3, sync_set/4, sync_set/5, sync_set/6, 
	 async_set/3, async_set/4, async_set/5, async_set/6, 
	 sync_get_bulk/5, sync_get_bulk/6, sync_get_bulk/7, sync_get_bulk/8, 
	 async_get_bulk/5, async_get_bulk/6, async_get_bulk/7, async_get_bulk/8, 
	 cancel_async_request/2, 
	 
	 %% 
	 %% Extended SNMP API
	 %% discovery/2, discovery/3, discovery/4, discovery/5, discovery/6, 

	 %% 
	 %% Logging
	 log_to_txt/2, log_to_txt/3, log_to_txt/4,
	 log_to_txt/5, log_to_txt/6, log_to_txt/7, 
	 change_log_size/1,
	 get_log_type/0,
	 set_log_type/1,

	 reconfigure/0,

	 system_start_time/0,
	 sys_up_time/0,

	 info/0, 
	 verbosity/2 
	]).

-export([format_reason/1, format_reason/2]).

%% Backward compatibillity exports
-export([
	 agent_info/3, update_agent_info/5, 
	 g/3, g/4, g/5, g/6, g/7, 
	 ag/3, ag/4, ag/5, ag/6, ag/7, 
	 gn/3, gn/4, gn/5, gn/6, gn/7, 
	 agn/3, agn/4, agn/5, agn/6, agn/7, 
	 gb/5, gb/6, gb/7, gb/8, gb/9, 
	 agb/5, agb/6, agb/7, agb/8, agb/9, 
	 s/3, s/4, s/5, s/6, s/7, 
	 as/3, as/4, as/5, as/6, as/7
	]).

%% Application internal export
-export([start_link/3, snmpm_start_verify/2, snmpm_start_verify/3]).


-include("snmp_debug.hrl").
-include("snmpm_atl.hrl").
-include("snmp_types.hrl").

-define(DEFAULT_AGENT_PORT, 161).
-define(DEFAULT_CONTEXT,    "").


%% This function is called when the snmp application
%% starts. 
start_link(Opts, normal, []) ->
    start_link(Opts).


simple_conf() ->
    Vsns      = [v1, v2, v3],
    {ok, Cwd} = file:get_cwd(),
    %% Check if the manager config file exist, if not create it
    MgrConf = filename:join(Cwd, "manager.conf"),
    case file:read_file_info(MgrConf) of
	{ok, _} ->
	    ok;
	_ ->
	    ok = snmp_config:write_manager_config(Cwd, "", 
						  [{port, 5000},
						   {engine_id, "mgrEngine"},
						   {max_message_size, 484}])
    end,
    Conf = [{dir, Cwd}, {db_dir, Cwd}],
    [{versions, Vsns}, {config, Conf}].
    
%% Simple start. Start a manager with default values.
start_link() ->
    start_link(simple_conf()).

%% This function is normally not used. Instead the manager is
%% started as a consequence of a call to application:start(snmp)
%% when {snmp, [{manager, Options}]} is present in the
%% node config file.
start_link(Opts) ->
    %% This start the manager top supervisor, which in turn
    %% starts the other processes.
    {ok, _} = snmpm_supervisor:start_link(normal, Opts),
    ok.

%% Simple start. Start a manager with default values.
start() ->
    start(simple_conf()).
    
start(Opts) ->
    %% This start the manager top supervisor, which in turn
    %% starts the other processes.
    {ok, Pid} = snmpm_supervisor:start_link(normal, Opts),
    unlink(Pid),
    ok.

stop() ->
    snmpm_supervisor:stop().


monitor() ->
    erlang:monitor(process, snmpm_supervisor).

demonitor(Ref) ->
    erlang:demonitor(Ref).
	

-define(NOTIFY_START_TICK_TIME, 500).

notify_started(To) when is_integer(To) andalso (To > 0) ->
    spawn_link(?MODULE, snmpm_start_verify, [self(), To]).

cancel_notify_started(Pid) ->
    Pid ! {cancel, self()},
    ok.

snmpm_start_verify(Parent, To) ->
    ?d("starting", []),
    snmpm_start_verify(Parent, monitor(), To).

snmpm_start_verify(Parent, _Ref, To) when (To =< 0) ->
    ?d("timeout", []),
    unlink(Parent),
    Parent ! {snmpm_start_timeout, self()};
snmpm_start_verify(Parent, Ref, To) ->
    T0 = t(),
    receive
	{cancel, Parent} ->
	    ?d("cancel", []),
	    demonitor(Ref),
	    unlink(Parent),
	    exit(normal);
	{'EXIT', Parent, _} ->
	    exit(normal);
	{'DOWN', Ref, process, _Object, _Info} ->
	    ?d("down", []),
	    sleep(?NOTIFY_START_TICK_TIME),
	    ?MODULE:snmpm_start_verify(Parent, monitor(), t(T0, To))
    after ?NOTIFY_START_TICK_TIME ->
	    ?d("down timeout", []),
	    demonitor(Ref),
	    case snmpm_server:is_started() of
		true ->
		    unlink(Parent),
		    Parent ! {snmpm_started, self()};
		_ ->
		    ?MODULE:snmpm_start_verify(Parent, monitor(), t(T0, To))
	    end
    end.

t(T0, T)  -> T - (t() - T0).
t()       -> snmp_misc:now(ms).
sleep(To) -> snmp_misc:sleep(To).


%% -- Misc --

backup(BackupDir) ->
    snmpm_config:backup(BackupDir).


%% -- Mibs --

%% Load a mib into the manager
load_mib(MibFile) ->
    snmpm_server:load_mib(MibFile).

%% Unload a mib from the manager
unload_mib(Mib) ->
    snmpm_server:unload_mib(Mib).

%% Which mib's are loaded
which_mibs() ->
    snmpm_config:which_mibs().

%% Get all the possible oid's for the aliasname
name_to_oid(Name) ->
    snmpm_config:name_to_oid(Name).

%% Get the aliasname for an oid
oid_to_name(Oid) ->
    snmpm_config:oid_to_name(Oid).

%% Get the type for an oid
oid_to_type(Oid) ->
    snmpm_config:oid_to_type(Oid).


%% -- Info -- 

info() ->
    snmpm_server:info().


%% -- Verbosity -- 

%% Change the verbosity of a process in the manager
verbosity(config, V) ->
    snmpm_config:verbosity(V);
verbosity(server, V) ->
    snmpm_server:verbosity(V);
verbosity(net_if, V) ->
    snmpm_server:verbosity(net_if, V);
verbosity(note_store, V) ->
    snmpm_server:verbosity(note_store, V);
verbosity(all, V) ->
    snmpm_config:verbosity(V),
    snmpm_server:verbosity(V),
    snmpm_server:verbosity(net_if, V),
    snmpm_server:verbosity(note_store, V).


%% -- Users --

%% Register the 'user'. 
%% The manager entity responsible for a specific agent. 
%% Module is the callback module (snmpm_user behaviour) which 
%% will be called whenever something happens (detected 
%% agent, incomming reply or incomming trap/notification).
%% Note that this could have already been done as a 
%% consequence of the node config.
register_user(Id, Module, Data) ->
    register_user(Id, Module, Data, []).

%% Default config for agents registered by this user
register_user(Id, Module, Data, DefaultAgentConfig) ->
    snmpm_server:register_user(Id, Module, Data, DefaultAgentConfig).

register_user_monitor(Id, Module, Data) ->
    register_user_monitor(Id, Module, Data, []).

register_user_monitor(Id, Module, Data, DefaultAgentConfig) ->
    snmpm_server:register_user_monitor(Id, Module, Data, DefaultAgentConfig).

unregister_user(Id) ->
    snmpm_server:unregister_user(Id).

which_users() ->
    snmpm_config:which_users().


%% -- Agents --

%% Explicitly instruct the manager to handle this agent.
%% Called to instruct the manager that this agent 
%% shall be handled. These functions is used when
%% the user know's in advance which agents the
%% manager shall handle.
%% Note that there is an alternate way to do the same thing:
%% Add the agent to the manager config files.
%% 
%% UserId     -> Id of the user responsible for this agent: term()
%% TargetName -> Unique name for the agent: (string())
%% Config     -> Agent configuration: [config()]

do_register_agent(UserId, TargetName, Config) ->
    snmpm_config:register_agent(UserId, TargetName, Config).

register_agent(UserId, TargetName, Config) 
  when (is_list(TargetName) andalso 
	(length(TargetName) > 0) andalso 
	is_list(Config)) ->
    do_register_agent(UserId, TargetName, [{reg_type, target_name} | Config]);

%% Backward compatibillity 
%% Note that the agent engine id is a mandatory config option,
%% so this function *will* fail!
register_agent(UserId, Addr, Port) when is_integer(Port) ->
    register_agent(UserId, Addr, Port, []);

%% Backward compatibillity 
register_agent(UserId, Addr, Config) when is_list(Config) ->
    register_agent(UserId, Addr, ?DEFAULT_AGENT_PORT, Config).

%% Backward compatibillity 
%% Note that the agent engine id is a mandatory config option,
%% so this function *will* fail!
register_agent(UserId, Addr) ->
    register_agent(UserId, Addr, ?DEFAULT_AGENT_PORT, []).

%% Backward compatibillity 
register_agent(UserId, Addr, Port, Config0) ->
    case lists:keymember(target_name, 1, Config0) of
	false ->
	    TargetName = mk_target_name(Addr, Port, Config0), 
	    Config     = [{reg_type, addr_port}, 
			  {address, Addr}, {port, Port} | Config0], 
	    do_register_agent(UserId, TargetName, ensure_engine_id(Config));
	true ->
	    {value, {_, TargetName}} = 
		lists:keysearch(target_name, 1, Config0),
	    Config1 = lists:keydelete(target_name, 1, Config0),
	    Config2 = [{reg_type, addr_port}, 
		       {address, Addr}, {port, Port} | Config1], 
	    register_agent(UserId, TargetName, ensure_engine_id(Config2))
    end.

unregister_agent(UserId, TargetName) when is_list(TargetName) ->
    snmpm_config:unregister_agent(UserId, TargetName);

%% Backward compatibillity functions
unregister_agent(UserId, Addr) ->
    unregister_agent(UserId, Addr, ?DEFAULT_AGENT_PORT).

unregister_agent(UserId, Addr, Port) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    unregister_agent(UserId, TargetName);
	Error ->
	    Error
    end.

agent_info(TargetName, Item) ->
    snmpm_config:agent_info(TargetName, Item).

%% Backward compatibillity
agent_info(Addr, Port, Item) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    agent_info(TargetName, Item);
	Error ->
	    Error
    end.

update_agent_info(UserId, TargetName, Item, Val) ->
%%     p("update_agent_info -> entry with"
%%       "~n   UserId:     ~p"
%%       "~n   TargetName: ~p"
%%       "~n   Item:       ~p"
%%       "~n   Val:        ~p", [UserId, TargetName, Item, Val]),
    snmpm_config:update_agent_info(UserId, TargetName, Item, Val).

%% Backward compatibillity functions
update_agent_info(UserId, Addr, Port, Item, Val) ->
%%     p("update_agent_info -> entry with"
%%       "~n   UserId: ~p"
%%       "~n   Addr:   ~p"
%%       "~n   Port:   ~p"
%%       "~n   Item:   ~p"
%%       "~n   Val:    ~p", [UserId, Addr, Port, Item, Val]),
    case target_name(Addr, Port) of
	{ok, TargetName} ->
%% 	    p("update_agent_info -> TargetName: ~p", [TargetName]),
	    update_agent_info(UserId, TargetName, Item, Val);
	Error ->
	    Error
    end.

which_agents() ->
    snmpm_config:which_agents().

which_agents(UserId) ->
    snmpm_config:which_agents(UserId).


%% -- USM users --

register_usm_user(EngineID, UserName, Conf) 
  when is_list(EngineID) andalso is_list(UserName) andalso is_list(Conf) ->
    snmpm_config:register_usm_user(EngineID, UserName, Conf).

unregister_usm_user(EngineID, UserName) 
  when is_list(EngineID) andalso is_list(UserName) ->
    snmpm_config:unregister_usm_user(EngineID, UserName).

usm_user_info(EngineID, UserName, Item) 
  when is_list(EngineID) andalso is_list(UserName) andalso is_atom(Item) ->
    snmpm_config:usm_user_info(EngineID, UserName, Item).

update_usm_user_info(EngineID, UserName, Item, Val) 
  when is_list(EngineID) andalso is_list(UserName) andalso is_atom(Item) ->
    snmpm_config:update_usm_user_info(EngineID, UserName, Item, Val).

which_usm_users() ->
    snmpm_config:which_usm_users().

which_usm_users(EngineID) when is_list(EngineID) ->
    snmpm_config:which_usm_users(EngineID).


%% -- Discovery --

%% Start a discovery process
%% discovery(UserId, BAddr) ->
%%     snmpm_server:discovery(UserId, BAddr).

%% discovery(UserId, BAddr, ExpireOrConfig) ->
%%     snmpm_server:discovery(UserId, BAddr, ExpireOrConfig).

%% discovery(UserId, BAddr, Config, Expire) ->
%%     snmpm_server:discovery(UserId, BAddr, Config, Expire).

%% discovery(UserId, BAddr, Port, Config, Expire) ->
%%     snmpm_server:discovery(UserId, BAddr, Port, Config, Expire).

%% discovery(UserId, BAddr, Port, Config, Expire, ExtraInfo) ->
%%     snmpm_server:discovery(UserId, BAddr, Port, Config, Expire, ExtraInfo).


%% -- Requests --

%% --- synchroneous get-request ---
%% 

sync_get(UserId, TargetName, Oids) ->
%%     p("sync_get -> entry with"
%%       "~n   UserId:     ~p"
%%       "~n   TargetName: ~p"
%%       "~n   Oids:       ~p", [UserId, TargetName, Oids]),
    sync_get(UserId, TargetName, ?DEFAULT_CONTEXT, Oids).

sync_get(UserId, TargetName, Context, Oids) when is_list(Oids) ->
%%     p("sync_get -> entry with"
%%       "~n   UserId:     ~p"
%%       "~n   TargetName: ~p"
%%       "~n   Context:    ~p"
%%       "~n   Oids:       ~p", [UserId, TargetName, Context, Oids]),
    snmpm_server:sync_get(UserId, TargetName, Context, Oids);

sync_get(UserId, TargetName, Oids, Timeout) when is_integer(Timeout) ->
%%     p("sync_get -> entry with"
%%       "~n   UserId:     ~p"
%%       "~n   TargetName: ~p"
%%       "~n   Oids:       ~p"
%%       "~n   Timeout:    ~p", [UserId, TargetName, Oids, Timeout]),
    sync_get(UserId, TargetName, ?DEFAULT_CONTEXT, Oids, Timeout).

sync_get(UserId, TargetName, Context, Oids, Timeout) ->
%%     p("sync_get -> entry with"
%%       "~n   UserId:     ~p"
%%       "~n   TargetName: ~p"
%%       "~n   Context:    ~p"
%%       "~n   Oids:       ~p"
%%       "~n   Timeout:    ~p", [UserId, TargetName, Context, Oids, Timeout]),
    snmpm_server:sync_get(UserId, TargetName, Context, Oids, Timeout).

sync_get(UserId, TargetName, Context, Oids, Timeout, ExtraInfo) ->
%%     p("sync_get -> entry with"
%%       "~n   UserId:     ~p"
%%       "~n   TargetName: ~p"
%%       "~n   Context:    ~p"
%%       "~n   Oids:       ~p"
%%       "~n   Timeout:    ~p"
%%       "~n   ExtraInfo:  ~p", 
%%       [UserId, TargetName, Context, Oids, Timeout, ExtraInfo]),
    snmpm_server:sync_get(UserId, TargetName, Context, Oids, Timeout, 
			  ExtraInfo).


g(UserId, Addr, Oids) ->
%%     p("g -> entry with"
%%       "~n   UserId:  ~p"
%%       "~n   Addr:    ~p"
%%       "~n   Oids:    ~p", [UserId, Addr, Oids]),
    g(UserId, Addr, ?DEFAULT_AGENT_PORT, Oids).

g(UserId, Addr, CtxName, Oids) when is_list(CtxName) andalso is_list(Oids) ->
%%     p("g -> entry with"
%%       "~n   UserId:  ~p"
%%       "~n   Addr:    ~p"
%%       "~n   CtxName: ~p"
%%       "~n   Oids:    ~p", [UserId, Addr, CtxName, Oids]),
    g(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids);

g(UserId, Addr, Port, Oids) when is_integer(Port) andalso is_list(Oids) ->
%%     p("g -> entry with"
%%       "~n   UserId:  ~p"
%%       "~n   Addr:    ~p"
%%       "~n   Port:    ~p"
%%       "~n   Oids:    ~p", [UserId, Addr, Port, Oids]),
    g(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids);

g(UserId, Addr, Oids, Timeout) 
  when is_list(Oids) andalso is_integer(Timeout) ->
%%     p("g -> entry with"
%%       "~n   UserId:  ~p"
%%       "~n   Addr:    ~p"
%%       "~n   Oids:    ~p"
%%       "~n   Timeout: ~p", [UserId, Addr, Oids, Timeout]),
    g(UserId, Addr, ?DEFAULT_AGENT_PORT, Oids, Timeout).

g(UserId, Addr, Port, CtxName, Oids) 
  when is_integer(Port) andalso is_list(CtxName) andalso is_list(Oids) ->
%%     p("g -> entry with"
%%       "~n   UserId:  ~p"
%%       "~n   Addr:    ~p"
%%       "~n   Port:    ~p"
%%       "~n   Context: ~p"
%%       "~n   Oids:    ~p", [UserId, Addr, Port, CtxName, Oids]),
    case target_name(Addr, Port) of
	{ok, TargetName} ->
%% 	    p("g -> TargetName: ~p", [TargetName]),
	    sync_get(UserId, TargetName, CtxName, Oids);
	Error ->
	    Error
    end;    
    
g(UserId, Addr, Port, Oids, Timeout) 
  when is_integer(Port) andalso is_list(Oids) andalso is_integer(Timeout) ->
%%     p("g -> entry with"
%%       "~n   UserId:    ~p"
%%       "~n   Addr:      ~p"
%%       "~n   Oids:      ~p"
%%       "~n   Timeout:   ~p", 
%%       [UserId, Addr, Oids, Timeout]),
    g(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids, Timeout);

g(UserId, Addr, CtxName, Oids, Timeout) 
  when is_list(CtxName) andalso is_list(Oids) andalso is_integer(Timeout) ->
%%     p("g -> entry with"
%%       "~n   UserId:    ~p"
%%       "~n   Addr:      ~p"
%%       "~n   CtxName:   ~p"
%%       "~n   Oids:      ~p"
%%       "~n   Timeout:   ~p", 
%%       [UserId, Addr, CtxName, Oids, Timeout]),
    g(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids, Timeout).

g(UserId, Addr, Port, CtxName, Oids, Timeout) ->
%%     p("g -> entry with"
%%       "~n   UserId:    ~p"
%%       "~n   Addr:      ~p"
%%       "~n   Port:      ~p"
%%       "~n   CtxName:   ~p"
%%       "~n   Oids:      ~p"
%%       "~n   Timeout:   ~p", 
%%       [UserId, Addr, Port, CtxName, Oids, Timeout]),
    case target_name(Addr, Port) of
	{ok, TargetName} ->
%% 	    p("g -> TargetName: ~p", [TargetName]),
	    sync_get(UserId, TargetName, CtxName, Oids, Timeout);
	Error ->
	    Error
    end.

g(UserId, Addr, Port, CtxName, Oids, Timeout, ExtraInfo) ->
%%     p("g -> entry with"
%%       "~n   UserId:    ~p"
%%       "~n   Addr:      ~p"
%%       "~n   Port:      ~p"
%%       "~n   CtxName:   ~p"
%%       "~n   Oids:      ~p"
%%       "~n   Timeout:   ~p"
%%       "~n   ExtraInfo: ~p", 
%%       [UserId, Addr, Port, CtxName, Oids, Timeout, ExtraInfo]),
    case target_name(Addr, Port) of
	{ok, TargetName} ->
%% 	    p("g -> TargetName: ~p", [TargetName]),
	    sync_get(UserId, TargetName, CtxName, Oids, Timeout, ExtraInfo);
	Error ->
	    Error
    end.



%% --- asynchroneous get-request ---
%% 
%% The reply will be delivered to the user
%% through a call to handle_pdu/5
%% 

async_get(UserId, TargetName, Oids) ->
    async_get(UserId, TargetName, ?DEFAULT_CONTEXT, Oids).

async_get(UserId, TargetName, Context, Oids) when is_list(Oids) ->
    snmpm_server:async_get(UserId, TargetName, Context, Oids);

async_get(UserId, TargetName, Oids, Expire) when is_integer(Expire) ->
    async_get(UserId, TargetName, ?DEFAULT_CONTEXT, Oids, Expire).

async_get(UserId, TargetName, Context, Oids, Expire) ->
    snmpm_server:async_get(UserId, TargetName, Context, Oids, Expire).

async_get(UserId, TargetName, Context, Oids, Expire, ExtraInfo) ->
    snmpm_server:async_get(UserId, TargetName, Context, Oids, Expire, 
			   ExtraInfo).


ag(UserId, Addr, Oids) ->
    ag(UserId, Addr, ?DEFAULT_AGENT_PORT, Oids).

ag(UserId, Addr, Port, Oids) when is_integer(Port) andalso is_list(Oids) ->
    ag(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids);

ag(UserId, Addr, CtxName, Oids) when is_list(CtxName) andalso is_list(Oids) ->
    ag(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids);

ag(UserId, Addr, Oids, Expire) when is_list(Oids) andalso is_integer(Expire) ->
    ag(UserId, Addr, ?DEFAULT_AGENT_PORT, ?DEFAULT_CONTEXT, Oids, Expire).

ag(UserId, Addr, Port, CtxName, Oids) 
  when is_integer(Port) andalso is_list(CtxName) andalso is_list(Oids) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_get(UserId, TargetName, CtxName, Oids);
	Error ->
	    Error
    end;

ag(UserId, Addr, Port, Oids, Expire) 
  when is_integer(Port) andalso is_list(Oids) andalso is_integer(Expire) ->
    ag(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids, Expire);

ag(UserId, Addr, CtxName, Oids, Expire) 
  when is_list(CtxName) andalso is_list(Oids) andalso is_integer(Expire) ->
    ag(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids, Expire).

ag(UserId, Addr, Port, CtxName, Oids, Expire) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_get(UserId, TargetName, CtxName, Oids, Expire);
	Error ->
	    Error
    end.

ag(UserId, Addr, Port, CtxName, Oids, Expire, ExtraInfo) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_get(UserId, TargetName, CtxName, Oids, Expire, ExtraInfo);
	Error ->
	    Error
    end.



%% --- synchroneous get_next-request ---
%% 

sync_get_next(UserId, TargetName, Oids) ->
    sync_get_next(UserId, TargetName, ?DEFAULT_CONTEXT, Oids).

sync_get_next(UserId, TargetName, Context, Oids) 
  when is_list(Context) andalso is_list(Oids) ->
    snmpm_server:sync_get_next(UserId, TargetName, Context, Oids);

sync_get_next(UserId, TargetName, Oids, Timeout) 
  when is_list(Oids) andalso is_integer(Timeout) ->
    sync_get_next(UserId, TargetName, ?DEFAULT_CONTEXT, Oids, Timeout).

sync_get_next(UserId, TargetName, Context, Oids, Timeout) ->
    snmpm_server:sync_get_next(UserId, TargetName, Context, Oids, Timeout).

sync_get_next(UserId, TargetName, Context, Oids, Timeout, ExtraInfo) ->
    snmpm_server:sync_get_next(UserId, TargetName, Context, Oids, Timeout, 
			       ExtraInfo).


gn(UserId, Addr, Oids) ->
    gn(UserId, Addr, ?DEFAULT_AGENT_PORT, Oids).

gn(UserId, Addr, CtxName, Oids) when is_list(CtxName) andalso is_list(Oids) ->
    gn(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids);

gn(UserId, Addr, Port, Oids) when is_integer(Port) andalso is_list(Oids) ->
    gn(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids);

gn(UserId, Addr, Oids, Timeout) 
  when is_list(Oids) andalso is_integer(Timeout) ->
    gn(UserId, Addr, ?DEFAULT_AGENT_PORT, Oids, Timeout).

gn(UserId, Addr, Port, CtxName, Oids) 
  when is_integer(Port) andalso is_list(CtxName) andalso is_list(Oids) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    sync_get_next(UserId, TargetName, CtxName, Oids);
	Error ->
	    Error
    end;    
	
gn(UserId, Addr, Port, Oids, Timeout) 
  when is_integer(Port) andalso is_list(Oids) andalso is_integer(Timeout) ->
    gn(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids, Timeout);
gn(UserId, Addr, CtxName, Oids, Timeout) 
  when is_list(CtxName) andalso is_list(Oids) andalso is_integer(Timeout) ->
    gn(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids, Timeout).

gn(UserId, Addr, Port, CtxName, Oids, Timeout) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    sync_get_next(UserId, TargetName, CtxName, Oids, Timeout);
	Error ->
	    Error
    end.

gn(UserId, Addr, Port, CtxName, Oids, Timeout, ExtraInfo) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    sync_get_next(UserId, TargetName, CtxName, Oids, Timeout, ExtraInfo);
	Error ->
	    Error
    end.



%% --- asynchroneous get_next-request ---
%% 

async_get_next(UserId, TargetName, Oids) ->
    async_get_next(UserId, TargetName, ?DEFAULT_CONTEXT, Oids).

async_get_next(UserId, TargetName, Context, Oids) 
  when is_list(Context) andalso is_list(Oids) ->
    snmpm_server:async_get_next(UserId, TargetName, Context, Oids);

async_get_next(UserId, TargetName, Oids, Timeout) 
  when is_list(Oids) andalso is_integer(Timeout) ->
    async_get_next(UserId, TargetName, ?DEFAULT_CONTEXT, Oids, Timeout).

async_get_next(UserId, TargetName, Context, Oids, Timeout) ->
    snmpm_server:async_get_next(UserId, TargetName, Context, Oids, Timeout).

async_get_next(UserId, TargetName, Context, Oids, Timeout, ExtraInfo) ->
    snmpm_server:async_get_next(UserId, TargetName, Context, Oids, Timeout, 
				ExtraInfo).

agn(UserId, Addr, Oids) ->
    agn(UserId, Addr, ?DEFAULT_AGENT_PORT, Oids).

agn(UserId, Addr, CtxName, Oids) when is_list(CtxName) andalso is_list(Oids) ->
    agn(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids);

agn(UserId, Addr, Port, Oids) when is_integer(Port) andalso is_list(Oids) ->
    agn(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids);

agn(UserId, Addr, Oids, Expire) 
  when is_list(Oids) andalso is_integer(Expire) ->
    agn(UserId, Addr, ?DEFAULT_AGENT_PORT, Oids, Expire).

agn(UserId, Addr, Port, CtxName, Oids) 
  when is_integer(Port) andalso is_list(CtxName) andalso is_list(Oids) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_get_next(UserId, TargetName, CtxName, Oids);
	Error ->
	    Error
    end;    
	
agn(UserId, Addr, Port, Oids, Expire) 
  when is_integer(Port) andalso is_list(Oids) andalso is_integer(Expire) ->
    agn(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids, Expire);
agn(UserId, Addr, CtxName, Oids, Expire) 
  when is_list(CtxName) andalso is_list(CtxName) andalso is_integer(Expire) ->
    agn(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids, Expire).

agn(UserId, Addr, Port, CtxName, Oids, Expire) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_get_next(UserId, TargetName, CtxName, Oids, Expire);
	Error ->
	    Error
    end.

agn(UserId, Addr, Port, CtxName, Oids, Expire, ExtraInfo) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_get_next(UserId, TargetName, CtxName, Oids, Expire, 
			   ExtraInfo);
	Error ->
	    Error
    end.



%% --- synchroneous set-request ---
%% 

sync_set(UserId, TargetName, VarsAndVals) ->
    sync_set(UserId, TargetName, ?DEFAULT_CONTEXT, VarsAndVals).

sync_set(UserId, TargetName, Context, VarsAndVals) 
  when is_list(Context) andalso is_list(VarsAndVals) ->
    snmpm_server:sync_set(UserId, TargetName, Context, VarsAndVals);

sync_set(UserId, TargetName, VarsAndVals, Timeout) 
  when is_list(VarsAndVals) andalso is_integer(Timeout) ->
    sync_set(UserId, TargetName, ?DEFAULT_CONTEXT, VarsAndVals, Timeout).

sync_set(UserId, TargetName, Context, VarsAndVals, Timeout) ->
    snmpm_server:sync_set(UserId, TargetName, Context, VarsAndVals, Timeout).

sync_set(UserId, TargetName, Context, VarsAndVals, Timeout, ExtraInfo) ->
    snmpm_server:sync_set(UserId, TargetName, Context, VarsAndVals, Timeout, 
			  ExtraInfo).


s(UserId, Addr, VarsAndVals) ->
    s(UserId, Addr, ?DEFAULT_AGENT_PORT, VarsAndVals).

s(UserId, Addr, Port, VarsAndVals) 
  when is_integer(Port) andalso is_list(VarsAndVals) ->
    s(UserId, Addr, Port, ?DEFAULT_CONTEXT, VarsAndVals);

s(UserId, Addr, CtxName, VarsAndVals) 
  when is_list(CtxName) andalso is_list(VarsAndVals) ->
    s(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, VarsAndVals);

s(UserId, Addr, VarsAndVals, Timeout) 
  when is_list(VarsAndVals) andalso is_integer(Timeout) ->
    s(UserId, Addr, ?DEFAULT_AGENT_PORT, VarsAndVals, Timeout).

s(UserId, Addr, Port, CtxName, VarsAndVals) 
  when is_integer(Port) andalso 
       is_list(CtxName) andalso 
       is_list(VarsAndVals) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    sync_set(UserId, TargetName, CtxName, VarsAndVals);
	Error ->
	    Error
    end;

s(UserId, Addr, Port, VarsAndVals, Timeout) 
  when is_integer(Port) andalso 
       is_list(VarsAndVals) andalso 
       is_integer(Timeout) ->
    s(UserId, Addr, Port, ?DEFAULT_CONTEXT, VarsAndVals, Timeout);

s(UserId, Addr, CtxName, VarsAndVals, Timeout) 
  when is_list(CtxName) andalso 
       is_list(VarsAndVals) andalso 
       is_integer(Timeout) ->
    s(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, VarsAndVals, Timeout).

s(UserId, Addr, Port, CtxName, VarsAndVals, Timeout) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    sync_set(UserId, TargetName, CtxName, VarsAndVals, Timeout);
	Error ->
	    Error
    end.

s(UserId, Addr, Port, CtxName, VarsAndVals, Timeout, ExtraInfo) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    sync_set(UserId, TargetName, CtxName, VarsAndVals, Timeout, ExtraInfo);
	Error ->
	    Error
    end.



%% --- asynchroneous set-request --- 
%% 

async_set(UserId, TargetName, VarsAndVals) ->
    async_set(UserId, TargetName, ?DEFAULT_CONTEXT, VarsAndVals).

async_set(UserId, TargetName, Context, VarsAndVals) 
  when is_list(Context) andalso is_list(VarsAndVals) ->
    snmpm_server:async_set(UserId, TargetName, Context, VarsAndVals);

async_set(UserId, TargetName, VarsAndVals, Expire) 
  when is_list(VarsAndVals) andalso is_integer(Expire) ->
    async_set(UserId, TargetName, ?DEFAULT_CONTEXT, VarsAndVals, Expire).

async_set(UserId, TargetName, Context, VarsAndVals, Expire) ->
    snmpm_server:async_set(UserId, TargetName, Context, VarsAndVals, Expire).

async_set(UserId, TargetName, Context, VarsAndVals, Expire, ExtraInfo) ->
    snmpm_server:async_set(UserId, TargetName, Context, VarsAndVals, Expire, 
			   ExtraInfo).


as(UserId, Addr, VarsAndVals) ->
    as(UserId, Addr, ?DEFAULT_AGENT_PORT, VarsAndVals).

as(UserId, Addr, Port, VarsAndVals) 
  when is_integer(Port) andalso is_list(VarsAndVals) ->
    as(UserId, Addr, Port, ?DEFAULT_CONTEXT, VarsAndVals);

as(UserId, Addr, CtxName, VarsAndVals) 
  when is_list(CtxName) andalso is_list(VarsAndVals) ->
    as(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, VarsAndVals);

as(UserId, Addr, VarsAndVals, Expire) 
  when is_list(VarsAndVals) andalso is_integer(Expire) ->
    as(UserId, Addr, ?DEFAULT_AGENT_PORT, VarsAndVals, Expire).

as(UserId, Addr, Port, CtxName, VarsAndVals) 
  when is_integer(Port) andalso 
       is_list(CtxName) andalso 
       is_list(VarsAndVals) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_set(UserId, TargetName, CtxName, VarsAndVals);
	Error ->
	    Error
    end;
	
as(UserId, Addr, Port, VarsAndVals, Expire) 
  when is_integer(Port) andalso 
       is_list(VarsAndVals) andalso 
       is_integer(Expire) ->
    as(UserId, Addr, Port, ?DEFAULT_CONTEXT, VarsAndVals, Expire);

as(UserId, Addr, CtxName, VarsAndVals, Expire) 
  when is_list(CtxName) andalso 
       is_list(VarsAndVals) andalso 
       is_integer(Expire) ->
    as(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, VarsAndVals, Expire).

as(UserId, Addr, Port, CtxName, VarsAndVals, Expire) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_set(UserId, TargetName, CtxName, VarsAndVals, Expire);
	Error ->
	    Error
    end.

as(UserId, Addr, Port, CtxName, VarsAndVals, Expire, ExtraInfo) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_set(UserId, TargetName, CtxName, VarsAndVals, Expire, ExtraInfo);
	Error ->
	    Error
    end.




%% --- synchroneous get-bulk ---
%% 

sync_get_bulk(UserId, TargetName, NonRep, MaxRep, Oids) ->
    sync_get_bulk(UserId, TargetName, NonRep, MaxRep, ?DEFAULT_CONTEXT, Oids).

sync_get_bulk(UserId, TargetName, NonRep, MaxRep, Context, Oids) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Context) andalso 
       is_list(Oids) ->
    snmpm_server:sync_get_bulk(UserId, TargetName, 
			       NonRep, MaxRep, 
			       Context, Oids);

sync_get_bulk(UserId, TargetName, NonRep, MaxRep, Oids, Timeout) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) andalso 
       is_integer(Timeout) ->
    sync_get_bulk(UserId, TargetName, NonRep, MaxRep, 
		  ?DEFAULT_CONTEXT, Oids, Timeout).

sync_get_bulk(UserId, TargetName, NonRep, MaxRep, Context, Oids, Timeout) ->
    snmpm_server:sync_get_bulk(UserId, TargetName, NonRep, MaxRep, 
			       Context, Oids, Timeout).

sync_get_bulk(UserId, TargetName, NonRep, MaxRep, Context, Oids, Timeout, 
	      ExtraInfo) ->
    snmpm_server:sync_get_bulk(UserId, TargetName, NonRep, MaxRep, 
			       Context, Oids, Timeout, ExtraInfo).


gb(UserId, Addr, NonRep, MaxRep, Oids) ->
    gb(UserId, Addr, ?DEFAULT_AGENT_PORT, NonRep, MaxRep, Oids).

gb(UserId, Addr, Port, NonRep, MaxRep, Oids) 
  when is_integer(Port) andalso 
       is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) ->
    gb(UserId, Addr, Port, NonRep, MaxRep, ?DEFAULT_CONTEXT, Oids);

gb(UserId, Addr, NonRep, MaxRep, CtxName, Oids) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(CtxName) andalso 
       is_list(Oids) ->
    gb(UserId, Addr, ?DEFAULT_AGENT_PORT, NonRep, MaxRep, CtxName, Oids);

gb(UserId, Addr, NonRep, MaxRep, Oids, Timeout) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) andalso 
       is_integer(Timeout) ->
    gb(UserId, Addr, ?DEFAULT_AGENT_PORT, NonRep, MaxRep, Oids, Timeout).

gb(UserId, Addr, Port, NonRep, MaxRep, CtxName, Oids) 
  when is_integer(Port) andalso 
       is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(CtxName) andalso 
       is_list(Oids) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    sync_get_bulk(UserId, TargetName, NonRep, MaxRep, CtxName, Oids);
	Error ->
	    Error
    end;

gb(UserId, Addr, Port, NonRep, MaxRep, Oids, Timeout) 
  when is_integer(Port) andalso 
       is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) andalso 
       is_integer(Timeout) ->
    gb(UserId, Addr, Port, NonRep, MaxRep, ?DEFAULT_CONTEXT, Oids, Timeout);

gb(UserId, Addr, NonRep, MaxRep, CtxName, Oids, Timeout) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(CtxName) andalso 
       is_list(Oids) andalso 
       is_integer(Timeout) ->
    gb(UserId, Addr, ?DEFAULT_AGENT_PORT, NonRep, MaxRep, CtxName, Oids, 
       Timeout).

gb(UserId, Addr, Port, NonRep, MaxRep, CtxName, Oids, Timeout) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    sync_get_bulk(UserId, TargetName, 
			  NonRep, MaxRep, CtxName, Oids, Timeout);
	Error ->
	    Error
    end.

gb(UserId, Addr, Port, NonRep, MaxRep, CtxName, Oids, Timeout, ExtraInfo) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    sync_get_bulk(UserId, TargetName, 
			  NonRep, MaxRep, CtxName, Oids, Timeout, ExtraInfo);
	Error ->
	    Error
    end.



%% --- asynchroneous get-bulk ---
%% 

async_get_bulk(UserId, TargetName, NonRep, MaxRep, Oids) ->
    async_get_bulk(UserId, TargetName, NonRep, MaxRep, ?DEFAULT_CONTEXT, Oids).

async_get_bulk(UserId, TargetName, NonRep, MaxRep, Context, Oids) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Context) andalso 
       is_list(Oids) ->
    snmpm_server:async_get_bulk(UserId, TargetName, 
				NonRep, MaxRep, Context, Oids);

async_get_bulk(UserId, TargetName, NonRep, MaxRep, Oids, Expire) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) andalso 
       is_integer(Expire) ->
    async_get_bulk(UserId, TargetName, 
		   NonRep, MaxRep, ?DEFAULT_CONTEXT, Oids, Expire).

async_get_bulk(UserId, TargetName, NonRep, MaxRep, Context, Oids, Expire) ->
    snmpm_server:async_get_bulk(UserId, TargetName, 
				NonRep, MaxRep, Context, Oids, Expire).

async_get_bulk(UserId, TargetName, NonRep, MaxRep, Context, Oids, Expire, 
	       ExtraInfo) ->
    snmpm_server:async_get_bulk(UserId, TargetName, 
				NonRep, MaxRep, 
				Context, Oids, Expire, ExtraInfo).


agb(UserId, Addr, NonRep, MaxRep, Oids) ->
    agb(UserId, Addr, ?DEFAULT_AGENT_PORT, NonRep, MaxRep, Oids).

agb(UserId, Addr, Port, NonRep, MaxRep, Oids) 
  when is_integer(Port) andalso 
       is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) ->
    agb(UserId, Addr, Port, NonRep, MaxRep, ?DEFAULT_CONTEXT, Oids);

agb(UserId, Addr, NonRep, MaxRep, CtxName, Oids) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(CtxName) andalso 
       is_list(Oids) ->
    agb(UserId, Addr, ?DEFAULT_AGENT_PORT, NonRep, MaxRep, CtxName, Oids);

agb(UserId, Addr, NonRep, MaxRep, Oids, Expire) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) andalso 
       is_integer(Expire) ->
    agb(UserId, Addr, ?DEFAULT_AGENT_PORT, NonRep, MaxRep, Oids, Expire).

agb(UserId, Addr, Port, NonRep, MaxRep, CtxName, Oids) 
  when is_integer(Port) andalso 
       is_integer(NonRep) andalso 
       is_integer(MaxRep), 
       is_list(CtxName) andalso 
       is_list(Oids) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_get_bulk(UserId, TargetName, 
			   NonRep, MaxRep, CtxName, Oids);
	Error ->
	    Error
    end;

agb(UserId, Addr, Port, NonRep, MaxRep, Oids, Expire) 
  when is_integer(Port) andalso 
       is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) andalso 
       is_integer(Expire) ->
    agb(UserId, Addr, Port, NonRep, MaxRep, ?DEFAULT_CONTEXT, Oids, Expire);

agb(UserId, Addr, NonRep, MaxRep, CtxName, Oids, Expire) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(CtxName) andalso 
       is_list(Oids) andalso 
       is_integer(Expire) ->
    agb(UserId, Addr, ?DEFAULT_AGENT_PORT, NonRep, MaxRep, CtxName, Oids).

agb(UserId, Addr, Port, NonRep, MaxRep, CtxName, Oids, Expire) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_get_bulk(UserId, TargetName, 
			   NonRep, MaxRep, CtxName, Oids, Expire);
	Error ->
	    Error
    end.

agb(UserId, Addr, Port, NonRep, MaxRep, CtxName, Oids, Expire, ExtraInfo) ->
    case target_name(Addr, Port) of
	{ok, TargetName} ->
	    async_get_bulk(UserId, TargetName, 
			   NonRep, MaxRep, CtxName, Oids, Expire,
			   ExtraInfo);
	Error ->
	    Error
    end.


cancel_async_request(UserId, ReqId) ->
    snmpm_server:cancel_async_request(UserId, ReqId).


%%%-----------------------------------------------------------------
%%% Audit Trail Log functions (for backward compatibillity)
%%%-----------------------------------------------------------------
log_to_txt(LogDir, Mibs) ->
    OutFile = "snmpm_log.txt",       
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile).
log_to_txt(LogDir, Mibs, OutFile) ->
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile).
log_to_txt(LogDir, Mibs, OutFile, LogName) ->
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile).
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile).
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start).
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start, Stop) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start, Stop).


change_log_size(NewSize) ->
    LogName = ?audit_trail_log_name, 
    snmp:change_log_size(LogName, NewSize).


get_log_type() ->
    snmpm_server:get_log_type().

%% NewType -> atl_type()
set_log_type(NewType) ->
    snmpm_server:set_log_type(NewType).


reconfigure() ->
    snmpm_server:reconfigure().


%%%-----------------------------------------------------------------

system_start_time() ->
    {ok, Time} = snmpm_config:system_start_time(),
    Time.

sys_up_time() ->
    % time in 0.01 seconds.
    StartTime = system_start_time(),
    (snmp_misc:now(cs) - StartTime) rem (2 bsl 31).


%%%-----------------------------------------------------------------
%%% This is just some simple utility functions to create a pretty-
%%% printable string of the error reason received from either:
%%% 
%%%    * If any of the sync/async get/get-next/set/get-bulk
%%%      returnes {error, Reason} 
%%%    * The Reason parameter in the handle_error user callback 
%%%      function
%%% 
%%%-----------------------------------------------------------------

format_reason(Reason) ->
    format_reason("", Reason).

format_reason(Prefix, Reason) when is_integer(Prefix) and (Prefix >= 0) ->
    format_reason(lists:duplicate(Prefix, $ ), Reason);
format_reason(Prefix, Reason) when is_list(Prefix) ->
    case (catch do_format_reason(Prefix, Reason)) of
	FL when is_list(FL) ->
	    FL;
	_ ->
	    %% Crap, try it without any fancy formatting
	    case (catch io_lib:format("~sInternal manager error: ~n"
				      "~s   ~p~n", 
				      [Prefix, Prefix, Reason])) of
		L1 when is_list(L1) ->
		    lists:flatten(L1);
		_ ->
		    %% Really crap, try it without the prefix
		    case (catch io_lib:format("Internal manager error: ~n"
					      "   ~p~n", 
					      [Reason])) of
			L2 when is_list(L2) ->
			    lists:flatten(L2);
			_ ->
			    %% Ok, I give up
			    "Illegal input. Unable to format error reason"
		    end
	    end
    end.
		    

do_format_reason(Prefix, {failed_generating_response, {RePdu, Reason}}) ->
    FmtPdu = format_pdu(Prefix ++ "   ", RePdu),
    lists:flatten(io_lib:format("~sFailed generating response: ~n"
				"~s"
				"~s   ~p~n", 
				[Prefix, FmtPdu, Prefix, Reason]));
do_format_reason(Prefix, {failed_processing_message, Reason})  ->
    lists:flatten(io_lib:format("~sFailed processing message: ~n"
				"~s   ~p~n", 
				[Prefix, Prefix, Reason]));
do_format_reason(Prefix, {unexpected_pdu, SnmpInfo})  ->
    FmtSnmpInfo = format_snmp_info(Prefix ++ "   ", SnmpInfo),
    lists:flatten(io_lib:format("~sUnexpected PDU: ~n~s", 
				[Prefix, FmtSnmpInfo]));
do_format_reason(Prefix, {send_failed, ReqId, Reason})  ->
    lists:flatten(io_lib:format("~sSend failed: ~n"
				"~s   Request id: ~w~n"
				"~s   Reason:     ~p~n", 
				[Prefix, Prefix, ReqId, Prefix, Reason]));
do_format_reason(Prefix, {invalid_sec_info, SecInfo, SnmpInfo})  ->
    FmtSecInfo  = format_sec_info(Prefix ++ "   ", SecInfo),
    FmtSnmpInfo = format_snmp_info(Prefix ++ "   ", SnmpInfo),
    lists:flatten(io_lib:format("~sInvalid security info: ~n"
				"~s"
				"~s", 
				[Prefix, FmtSecInfo, FmtSnmpInfo]));
do_format_reason(Prefix, Reason)  ->
    lists:flatten(io_lib:format("~sInternal manager error: ~n"
				"~s   ~p~n", [Prefix, Prefix, Reason])).

format_pdu(Prefix, #pdu{type         = Type,
			request_id   = ReqId,
			error_status = ES,
			error_index  = EI,
			varbinds     = VBs}) ->
    FmtPdyType   = format_pdu_type(Type),
    FmtErrStatus = format_error_status(ES),
    FmtErrIdx    = format_error_index(EI),
    FmtVBs       = format_varbinds(Prefix ++ "   ", VBs),
    lists:flatten(io_lib:format("~s~s: ~n"
				"~s   Request-id:   ~w~n"
				"~s   Error-status: ~s~n"
				"~s   Error-index:  ~s~n"
				"~s",
				[Prefix, FmtPdyType,
				 Prefix, ReqId, 
				 Prefix, FmtErrStatus, 
				 Prefix, FmtErrIdx, 
				 FmtVBs]));
format_pdu(Prefix, #trappdu{enterprise    = E,
			    agent_addr    = AA,
			    generic_trap  = GT,
			    specific_trap = ST,
			    time_stamp    = TS,
			    varbinds      = VBs}) ->
    FmtVBs = format_varbinds(Prefix ++ "   ", VBs),
    lists:flatten(io_lib:format("~sTrap PDU: ~n"
				"~s   Enterprise:    ~p~n"
				"~s   Agent address: ~p~n"
				"~s   Generic trap:  ~p~n"
				"~s   Specific trap: ~p~n"
				"~s   Time stamp:    ~p~n"
				"~s",
				[Prefix, 
				 Prefix, E,
				 Prefix, AA, 
				 Prefix, GT, 
				 Prefix, ST, 
				 Prefix, TS, 
				 FmtVBs]));
format_pdu(Prefix, PDU) ->
    lists:flatten(io_lib:format("~s~p~n", [Prefix, PDU])).

format_pdu_type('get-request') ->
    "GetRequest-PDU";
format_pdu_type('get-next-request') ->
    "GetNextRequest-PDU";
format_pdu_type('get-response') ->
    "Response-PDU";
format_pdu_type('set-request') ->
    "SetRequest-PDU";
format_pdu_type('get-bulk-request') ->
    "GetBulkRequest-PDU";
format_pdu_type('inform-request') ->
    "InformRequest-PDU";
format_pdu_type('snmpv2-trap') ->
    "SNMPv2-Trap-PDU";
format_pdu_type(report) ->
    "Report-PDU";
format_pdu_type(T) ->
    lists:flatten(io_lib:format("~p", [T])).
    
format_snmp_info(Prefix, {ES, EI, VBs}) ->
    lists:flatten(io_lib:format("~sSNMP info: ~n"
				"~s   Error-status: ~s~n"
				"~s   Error-index:  ~s~n"
				"~s",
				[Prefix, 
				 Prefix, format_error_status(ES),
				 Prefix, format_error_index(EI),
				 format_varbinds(Prefix ++ "   ", VBs)]));
format_snmp_info(Prefix, JunkSnmpInfo) ->
    lists:flatten(io_lib:format("~sJunk SNMP info: ~n"
				"~s   ~p~n",
				[Prefix, Prefix, JunkSnmpInfo])).

format_error_status(ES) ->
    lists:flatten(io_lib:format("~p", [ES])).

format_error_index(EI) ->
    lists:flatten(io_lib:format("~p", [EI])).

format_sec_info(Prefix, Info) ->
    FmtSecInfo = do_format_sec_info(Prefix ++ "   ", Info),
    lists:flatten(io_lib:format("~sSecurity info: ~n~s", 
				[Prefix, FmtSecInfo])).

do_format_sec_info(_Prefix, []) ->
    "";
do_format_sec_info(Prefix, [{Tag, ExpVal, Val}|T]) ->
    format_sec_info(Prefix, Tag, ExpVal, Val) ++
	do_format_sec_info(Prefix, T).


format_sec_info(_Prefix, _Tag, Val, Val) ->
    "";
format_sec_info(Prefix, Tag, ExpVal, Val) ->
    lists:flatten(io_lib:format("~s~s:~n"
				"~s   Expected value: ~p~n"
				"~s   Actual value:   ~p~n",
				[Prefix, format_sec_info_tag(Tag),
				 Prefix, ExpVal,
				 Prefix, Val])).

format_sec_info_tag(sec_engine_id) ->
    "Sec engine id";
format_sec_info_tag(msg_sec_model) ->
    "Msg sec model";
format_sec_info_tag(sec_name) ->
    "Sec name";
format_sec_info_tag(sec_level) ->
    "Sec level";
format_sec_info_tag(ctx_engine_id) ->
    "Context engine id";
format_sec_info_tag(ctx_name) ->
    "Context name";
format_sec_info_tag(request_id) ->
    "Request id";
format_sec_info_tag(T) ->
    lists:flatten(io_lib:format("~p", [T])).

format_varbinds(Prefix, []) ->
    lists:flatten(io_lib:format("~sVarbinds:    []~n", [Prefix])); 
format_varbinds(Prefix, VBs) when is_list(VBs) ->
    lists:flatten(io_lib:format("~sVarbinds: ~n~s", 
				[Prefix, format_vbs(Prefix ++ "   ", VBs)]));
format_varbinds(Prefix, VBs) ->
    lists:flatten(io_lib:format("~sInvalid varbinds: ~n"
				"~s   ~p~n", 
				[Prefix, Prefix, VBs])).

format_vbs(_Prefix, []) ->
    "";
format_vbs(Prefix, [VB|VBs]) ->
    format_vb(Prefix, VB) ++ format_vbs(Prefix, VBs).
    
format_vb(Prefix, #varbind{oid          = Oid0,
			   variabletype = Type,
			   value        = Val,
			   org_index    = Idx}) ->
    Oid = 
	case snmpm:oid_to_name(Oid0) of
	    {ok, O} ->
		O;
	    _ ->
		Oid0
	end,
    FmtVT  = format_vb_variabletype(Prefix ++ "   ", Type),
    FmtVal = format_vb_value(Prefix ++ "   ", Type, Val),
    lists:flatten(io_lib:format("~s~w:~n"
				"~s"
				"~s"
				"~s   Org-index:     ~p~n", 
				[Prefix, Oid, 
				 FmtVT, 
				 FmtVal, 
				 Prefix, Idx]));
format_vb(Prefix, JunkVB) ->
    lists:flatten(io_lib:format("~sJunk varbind:~n"
				"~s   ~p~n", [Prefix, Prefix, JunkVB])).

format_vb_variabletype(Prefix, Type) when is_atom(Type) ->
    lists:flatten(io_lib:format("~sVariable-type: ~s~n", 
				[Prefix, atom_to_list(Type)]));
format_vb_variabletype(Prefix, Type) ->
    lists:flatten(io_lib:format("~sVariable-type: ~p~n", [Prefix, Type])).

format_vb_value(Prefix, _Type, Val) ->
    lists:flatten(io_lib:format("~sValue:         ~p~n", [Prefix, Val])).


%% ---------------------------------------------------------------------------
%% 
%% --- Internal utility functions ---
%% 

target_name(Addr, Port) ->
    snmpm_config:agent_info(Addr, Port, target_name).

mk_target_name(Addr, Port, Config) ->
    snmpm_config:mk_target_name(Addr, Port, Config).

ensure_engine_id(Config) ->
    case lists:keymember(engine_id, 1, Config) of
	true ->
	    Config;
	false ->
	    DefaultEngineId = "agentEngine-default", 
	    [{engine_id, DefaultEngineId} | Config]
    end.



%% p(F) ->
%%     p(F, []).

%% p(F, A) ->
%%     io:format("~w:" ++ F ++ "~n", [?MODULE | A]).
