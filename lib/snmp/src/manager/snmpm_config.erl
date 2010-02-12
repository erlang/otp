%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
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
%% -------------------------------------------------------------------------
%%
%% Some of the stuff stored here should really be persistent!!
%% (e.g. snmp-engine-boot)
%%
%% -------------------------------------------------------------------------

-module(snmpm_config).

-behaviour(gen_server).

%% External exports
-export([start_link/1, stop/0, is_started/0]).
-export([register_user/4, unregister_user/1, 
	 which_users/0, 
	 user_info/0, user_info/1, user_info/2, 

	 register_agent/3, unregister_agent/2, 
	 agent_info/0, agent_info/2, agent_info/3, update_agent_info/4, 
	 which_agents/0, which_agents/1, 

	 is_known_engine_id/2, 
	 get_agent_engine_id/1, 
	 get_agent_engine_max_message_size/1, 
	 get_agent_version/1, 
	 get_agent_mp_model/1, 
	 get_agent_user_id/1, get_agent_user_id/2, 
	 get_agent_user_info/2, 
	 
	 system_info/0, system_info/1, 
	 %% update_system_info/2, 
	 get_engine_id/0, get_engine_max_message_size/0,

	 register_usm_user/3, unregister_usm_user/2, 
	 which_usm_users/0, which_usm_users/1, 
	 usm_user_info/3, update_usm_user_info/4, 
	 get_usm_user/2, get_usm_user_from_sec_name/2, 
	 is_usm_engine_id_known/1,
	 get_engine_boots/0, get_engine_time/0, 
	 set_engine_boots/1, set_engine_time/1, 
	 get_usm_eboots/1, get_usm_etime/1, get_usm_eltime/1, 
	 set_usm_eboots/2, set_usm_etime/2, set_usm_eltime/2, 
	 reset_usm_cache/1, 
	 

	 cre_counter/2,
	 incr_counter/2,
	 increment_counter/3, increment_counter/4, 

	 cre_stats_counter/2,
	 maybe_cre_stats_counter/2,
	 incr_stats_counter/2,
	 reset_stats_counter/1,
	 get_stats_counters/0, get_stats_counter/1,

	 load_mib/1, unload_mib/1, which_mibs/0, 
	 make_mini_mib/0,
	 name_to_oid/1, oid_to_name/1, oid_to_type/1, 

	 system_start_time/0,

	 info/0, 
	 verbosity/1,

	 backup/1,

	 mk_target_name/3

	]).

%% Backward compatibillity exports
-export([
	 register_user/3,
	 unregister_agent/3, 
	 update_agent_info/5,
	 is_known_engine_id/3, 
	 get_agent_engine_id/2, 
	 get_agent_engine_max_message_size/2, 
	 get_agent_version/2, 
	 get_agent_mp_model/2
	]).

-export([check_manager_config/1, 
	 check_user_config/1, 
	 check_agent_config/1,
	 check_usm_user_config/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).


%% Includes:
-include_lib("kernel/include/file.hrl").
-include("snmp_types.hrl").
-include("snmpm_internal.hrl").
-include("snmpm_usm.hrl").
-include("snmp_debug.hrl").
-include("snmp_verbosity.hrl").


%% Types:
-record(user, {id, mod, data, default_agent_config}).

-record(state, {backup}).


%% Macros and Constants:
-define(SERVER,    ?MODULE).
-define(BACKUP_DB, snmpm_config_backup).
-define(CONFIG_DB, snmpm_config_db).

-define(DEFAULT_USER,       default_user).

-define(DEFAULT_AGENT_PORT, 161).

-define(IRB_DEFAULT, auto).
%% -define(IRB_DEFAULT, {user, timer:seconds(15)}).

-define(USER_MOD_DEFAULT,  snmpm_user_default).
-define(USER_DATA_DEFAULT, undefined).

%% -define(DEF_ADDR_TAG, default_addr_tag).
-define(DEFAULT_TARGETNAME, default_agent).
-define(DEF_PORT_TAG, default_port_tag).

-ifdef(snmp_debug).
-define(GS_START_LINK(Opts),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], 
			      [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Opts),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], [])).
-endif.



%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(Opts) -> 
    ?d("start_link -> entry with"
       "~n   Opts: ~p", [Opts]),
    ?GS_START_LINK(Opts).

stop() ->
    call(stop).

is_started() ->
    call(is_started, 1000).

backup(BackupDir) when is_list(BackupDir) ->
    call({backup, BackupDir}).

%% Backward compatibillity
register_user(UserId, UserMod, UserData) ->
    register_user(UserId, UserMod, UserData, []).

register_user(UserId, UserMod, UserData, DefaultAgentConfig) 
  when (UserId =/= ?DEFAULT_USER) andalso is_list(DefaultAgentConfig) ->
    case (catch verify_user_behaviour(UserMod)) of
	ok ->
	    Config = default_agent_config(DefaultAgentConfig),
	    call({register_user, UserId, UserMod, UserData, Config});
	Error ->
	    Error
    end;
register_user(UserId, _UserMod, _UserData, DefaultAgentConfig) 
  when (UserId =/= ?DEFAULT_USER) ->
    {error, {bad_default_agent_config, DefaultAgentConfig}};
register_user(UserId, _, _, _) ->
    {error, {bad_user_id, UserId}}.

default_agent_config(DefaultAgentConfig) ->
    {ok, SystemDefaultAgentConfig} = agent_info(),
    default_agent_config(SystemDefaultAgentConfig, DefaultAgentConfig).

default_agent_config([], DefaultAgentConfig) ->
    DefaultAgentConfig;
default_agent_config([{Key, _} = Entry|T], DefaultAgentConfig) ->
    case lists:keysearch(Key, 1, DefaultAgentConfig) of
	{value, _} ->
	    default_agent_config(T, DefaultAgentConfig);
	false ->
	    default_agent_config(T, [Entry|DefaultAgentConfig])
    end.


verify_user_behaviour(UserMod) ->
    case snmp_misc:verify_behaviour(snmpm_user, UserMod) of
	ok ->
	    ok;
	Error ->
	    %% This user may implement the old behaviour, check it
	    case snmp_misc:verify_behaviour(snmpm_user_old, UserMod) of
		ok ->
		    ok;
		_ ->
		    throw(Error)
	    end
    end.


unregister_user(UserId) when UserId =/= ?DEFAULT_USER ->
    call({unregister_user, UserId});
unregister_user(BadUserId) ->
    {error, {bad_user_id, BadUserId}}.


which_users() ->
    Pattern = #user{id = '$1', _ = '_'},
    Match   = ets:match(snmpm_user_table, Pattern),
    [UserId || [UserId] <- Match, UserId =/= ?DEFAULT_USER].


user_info() ->
    UserId = ?DEFAULT_USER,
    case user_info(UserId) of
	{ok, Mod, Data} ->
	    {ok, UserId, Mod, Data};
	Error ->
	    Error
    end.

user_info(UserId) ->
    case ets:lookup(snmpm_user_table, UserId) of
	[#user{mod = UserMod, data = UserData}] ->
	    {ok, UserMod, UserData};
	_ ->
	    {error, not_found}
    end.

user_info(UserId, Item) ->
    case (catch do_user_info(UserId, Item)) of
	{'EXIT', _} ->
	    {error, {not_found, Item}};
	Val ->
	    {ok, Val}
    end.

do_user_info(UserId, module) ->
    ets:lookup_element(snmpm_user_table, UserId, #user.mod);
do_user_info(UserId, data) ->
    ets:lookup_element(snmpm_user_table, UserId, #user.data);
do_user_info(UserId, default_agent_config) ->
    ets:lookup_element(snmpm_user_table, UserId, #user.default_agent_config);
do_user_info(_UserId, BadItem) ->
    error({not_found, BadItem}).


%% A target-name constructed in this way is a string with the following 
%% <IP-address>:<Port>-<Version>
%% 
mk_target_name(Addr0, Port, Config) when is_list(Config) ->
    Version = 
	case lists:keysearch(version, 1, Config) of
	    {value, {_, V}} ->
		V;
	    false ->
		select_lowest_supported_version()
	end,
%%     p("mk_target_name -> Version: ~p", [Version]),
    case normalize_address(Addr0) of
	{A, B, C, D} ->
	    lists:flatten(
	      io_lib:format("~w.~w.~w.~w:~w-~w", [A, B, C, D, Port, Version]));
	[A, B, C, D] ->
	    lists:flatten(
	      io_lib:format("~w.~w.~w.~w:~w-~w", [A, B, C, D, Port, Version]));
	_ -> 
	    lists:flatten(
	      io_lib:format("~p:~w-~w", [Addr0, Port, Version]))
    end.
	
select_lowest_supported_version() ->
    {ok, Versions} = system_info(versions),
    select_lowest_supported_version([v1, v2, v3], Versions).

select_lowest_supported_version([], Versions) ->
    error({bad_versions, Versions});
select_lowest_supported_version([H|T], Versions) ->
    case lists:member(H, Versions) of
	true ->
	    H;
	false ->
	    select_lowest_supported_version(T, Versions)
    end.


register_agent(UserId, _TargetName, _Config) when (UserId =:= user_id) ->
    {error, {bad_user_id, UserId}};
register_agent(UserId, TargetName, Config) 
  when (is_list(TargetName) andalso 
	(length(TargetName) > 0) andalso 
	is_list(Config)) ->

%%     p("register_agent -> entry with"
%%       "~n   UserId:     ~p"
%%       "~n   TargetName: ~p"
%%       "~n   Config:     ~p", [UserId, TargetName, Config]),

    %% Check: 
    %%   1) That the mandatory configs are present
    %%   2) That the illegal config user_id (used internally) is 
    %%      not present
    %%   3) Check that there are no invalid or erroneous configs
    %%   4) Chack that the manager is capable to use the selected version
    case verify_agent_config(Config) of
	ok ->
	    call({register_agent, UserId, TargetName, Config});
	Error ->
	    Error
    end.


verify_agent_config(Conf) ->
    ok = verify_mandatory(Conf, [engine_id, address, reg_type]),
    case verify_invalid(Conf, [user_id]) of
	ok ->
	    case verify_agent_config2(Conf) of
		ok ->
		    {ok, Vsns} = system_info(versions),
		    Vsn = 
			case lists:keysearch(version, 1, Conf) of
			    {value, {version, V}} ->
				V;
			    false ->
				v1
			end,
		    case lists:member(Vsn, Vsns) of
			true ->
			    ok;
			false ->
			    {error, {version_not_supported_by_manager, Vsn, Vsns}}
		    end
	    end;
	Error ->
	    Error
    end.

verify_agent_config2(Conf) ->
    verify_agent2(Conf).


unregister_agent(UserId, TargetName) ->
    call({unregister_agent, UserId, TargetName}).

unregister_agent(UserId, Addr0, Port) ->
    Addr = normalize_address(Addr0),
    case do_agent_info(Addr, Port, target_name) of
	{ok, TargetName} ->
	    unregister_agent(UserId, TargetName);
	Error ->
	    Error
    end.

agent_info() ->
    agent_info(?DEFAULT_TARGETNAME, all).

agent_info(TargetName, all) ->
    case ets:match_object(snmpm_agent_table, {{TargetName, '_'}, '_'}) of
	[] ->
	    {error, not_found};
	All ->
	    {ok, [{Item, Val} || {{_, Item}, Val} <- All]}
    end;
agent_info(TargetName, Item) ->
    case ets:lookup(snmpm_agent_table, {TargetName, Item}) of
	[{_, Val}] ->
	    {ok, Val};
	[] ->
	    {error, not_found}
    end.
    
agent_info(Addr0, Port, Item) ->
    Addr = normalize_address(Addr0),
    do_agent_info(Addr, Port, Item).

do_agent_info(Addr, Port, target_name = Item) ->
    case ets:lookup(snmpm_agent_table, {Addr, Port, Item}) of
	[{_, Val}] ->
	    {ok, Val};
	[] ->
	    {error, not_found}
    end;
do_agent_info(Addr, Port, Item) ->
    case do_agent_info(Addr, Port, target_name) of
	{ok, TargetName} ->
	    agent_info(TargetName, Item);
	Error ->
	    Error
    end.


which_agents() ->
    which_agents('_').

which_agents(UserId) ->
    Pat = {{'$1', user_id}, UserId},
    Agents = ets:match(snmpm_agent_table, Pat),
    [TargetName || [TargetName] <- Agents].

    
update_agent_info(UserId, TargetName, Item, Val0) 
  when (Item =/= user_id) ->
    case (catch verify_val(Item, Val0)) of
	{ok, Val} ->
	    call({update_agent_info, UserId, TargetName, Item, Val});
	Error ->
	    Error
    end.

%% Backward compatibillity
update_agent_info(UserId, Addr, Port, Item, Val)  ->
    case agent_info(Addr, Port, target_name) of
	{ok, TargetName} ->
	    update_agent_info(UserId, TargetName, Item, Val);
	Error ->
	    Error
    end.

is_known_engine_id(EngineID, TargetName) ->
    case agent_info(TargetName, engine_id) of
	{ok, EngineID} ->
	    true;
	{ok, _OtherEngineID} ->
	    false;
	_ ->
	    false
    end.
    
%% Backward compatibillity
is_known_engine_id(EngineID, Addr, Port) ->
    case agent_info(Addr, Port, target_name) of
	{ok, TargetName} ->
	    is_known_engine_id(EngineID, TargetName);
	_ ->
	    false
    end.

get_agent_engine_id(TargetName) ->
    agent_info(TargetName, engine_id).

%% Backward compatibillity
get_agent_engine_id(Addr, Port) ->
    agent_info(Addr, Port, engine_id).

get_agent_engine_max_message_size(TargetName) ->
    agent_info(TargetName, max_message_size).

%% Backward compatibillity
get_agent_engine_max_message_size(Addr, Port) ->
    agent_info(Addr, Port, max_message_size).

get_agent_version(TargetName) ->
    agent_info(TargetName, version). 

%% Backward compatibillity
get_agent_version(Addr, Port) ->
    agent_info(Addr, Port, version). 

get_agent_mp_model(TargetName) ->
    case agent_info(TargetName, version) of
	{ok, v2} ->
	    {ok, v2c};
	{ok, V} ->
	    {ok, V};
	Err ->
	    Err
    end.

%% Backward compatibillity
get_agent_mp_model(Addr, Port) ->
    case agent_info(Addr, Port, target_name) of
	{ok, TargetName} ->
	    get_agent_mp_model(TargetName);
	Error ->
	    Error
    end.

get_agent_user_id(TargetName) ->
    agent_info(TargetName, user_id).

get_agent_user_id(Addr, Port) ->
    agent_info(Addr, Port, user_id).

get_agent_user_info(Addr, Port) ->
    case agent_info(Addr, Port, target_name) of
	{ok, Target} ->
	    case agent_info(Target, reg_type) of
		{ok, RegType} ->
		    case agent_info(Target, user_id) of
			{ok, UserId} ->
			    {ok, UserId, Target, RegType};
			{error, not_found} ->
			    {error, {user_id_not_found, Target}}
		    end;
		{error, not_found} ->
		    {error, {reg_type_not_found, Target}}
	    end;
	{error, not_found} ->
	    {error, {target_name_not_found, Addr, Port}}
    end.
	    


system_info() ->
    system_info(all).

system_info(all) ->
    lists:sort(ets:tab2list(snmpm_config_table));
system_info(Key) when is_atom(Key) ->
    case ets:lookup(snmpm_config_table, Key) of
	[{_, Val}] ->
	    {ok, Val};
	_ ->
	    {error, not_found}
    end.

%% update_system_info(Key, Val) ->
%%     call({update_system_info, Key, Val}).

system_start_time() ->
    system_info(system_start_time).

get_engine_id() ->
    system_info(engine_id).

get_engine_max_message_size() ->
    system_info(max_message_size).

get_engine_boots() ->
    case dets:lookup(?CONFIG_DB, snmp_engine_boots) of
	[{_, Boots}] ->
	    {ok, Boots};
	_ ->
	    {error, not_found}
    end.

set_engine_boots(Boots) ->
    case (whereis(?SERVER) =:= self()) of
	false ->
	    call({set_engine_boots, Boots});
	true ->
	    dets:insert(?CONFIG_DB, {snmp_engine_boots, Boots}),
	    ok
    end.
    

get_engine_time() ->
    case system_info(snmp_engine_base) of
	{ok, EngineBase} ->
	    {ok, snmp_misc:now(sec) - EngineBase};
	Error ->
	    Error
    end.

get_usm_eboots(SnmpEngineID) ->
    Key = {eboots, SnmpEngineID},
    case get_usm_cache(Key) of
	{ok, Boots} ->
	    {ok, Boots};
	_ ->
	    {ok, 0}
    end.

get_usm_etime(SnmpEngineID) ->
    Key = {etime, SnmpEngineID},
    case get_usm_cache(Key) of
	{ok, Diff} ->
	    {ok, snmp_misc:now(sec) - Diff};
	_ ->
	    {ok, 0}
    end.

get_usm_eltime(SnmpEngineID) ->
    Key = {eltime, SnmpEngineID},
    case get_usm_cache(Key) of
	{ok, Time} ->
	    {ok, Time};
	_ ->
	    {ok, 0}
    end.

get_usm_cache(Key) ->
    case ets:lookup(snmpm_usm_table, {usm_cache, Key}) of
	[{_, Val}] ->
	    {ok, Val};
	_ ->
	    {error, not_found}
    end.
    
set_usm_eboots(SnmpEngineID, EngineBoots) ->
    set_usm_cache({eboots, SnmpEngineID}, EngineBoots).

set_usm_etime(SnmpEngineID, Diff) ->
    set_usm_cache({etime, SnmpEngineID}, Diff).

set_usm_eltime(SnmpEngineID, Time) ->
    set_usm_cache({eltime, SnmpEngineID}, Time).

set_usm_cache(Key, Val) ->
    call({set_usm_cache, Key, Val}).

reset_usm_cache(SnmpEngineID) ->
    case (whereis(?SERVER) =:= self()) of
	false ->
	    call({reset_usm_cache, SnmpEngineID});
	true ->
	    Pat = {{usm_cache, {'_', SnmpEngineID}}, '_'},
	    ets:match_delete(snmpm_usm_table, Pat),
	    ok
    end.

set_engine_time(Time) ->
    call({set_engine_time, Time}).

register_usm_user(EngineID, Name, Config) 
  when is_list(EngineID) andalso is_list(Name) ->
    case verify_usm_user_config(EngineID, Name, Config) of
	{ok, User} ->
	    call({register_usm_user, User});
	Error ->
	    Error
    end.

unregister_usm_user(EngineID, Name) 
  when is_list(EngineID) andalso is_list(Name) ->
    call({unregister_usm_user, EngineID, Name}).

verify_usm_user_config(EngineID, Name, Config) ->
    %%     case verify_mandatory(Config, []) of
    %% 	ok ->
    %% 	    case verify_invalid(Config, [engine_id, name]) of
    %% 		ok ->
    %% 		    verify_usm_user_config2(EngineID, Name, Config);
    %% 		Error ->
    %% 		    Error
    %% 	    end;
    %% 	Error ->
    %% 	    Error
    %%     end.
    ok = verify_mandatory(Config, []),
    case verify_invalid(Config, [engine_id, name]) of
	ok ->
	    verify_usm_user_config2(EngineID, Name, Config);
	Error ->
	    Error
    end.

verify_usm_user_config2(EngineID, Name, Config) ->
    SecName = verify_usm_user_get(sec_name, Name,              Config),
    Auth    = verify_usm_user_get(auth,     usmNoAuthProtocol, Config),
    AuthKey = verify_usm_user_get(auth_key, [],                Config),
    Priv    = verify_usm_user_get(priv,     usmNoPrivProtocol, Config),
    PrivKey = verify_usm_user_get(priv_key, [],                Config),
    User = {EngineID, Name, SecName, Auth, AuthKey, Priv, PrivKey},
    verify_usm_user(User).
	
verify_usm_user_get(Item, Default, Config) ->	
    case lists:keysearch(Item, 1, Config) of
	{value, {_, Val}} ->
	    Val;
	false ->
	    Default
    end.

which_usm_users() ->
    Pattern = {usm_key('$1', '$2'), '_'},
    Match   = ets:match(snmpm_usm_table, Pattern),
    [{EngineID, UserName} || [EngineID, UserName] <- Match].

which_usm_users(EngineID) ->
    Pattern = {usm_key(EngineID, '$1'), '_'},
    Match   = ets:match(snmpm_usm_table, Pattern),
    [UserName || [UserName] <- Match].

usm_user_info(EngineID, UserName, Item) ->
    case ets:lookup(snmpm_usm_table, usm_key(EngineID, UserName)) of
	[] ->
	    {error, not_found};
	[{_Key, UsmUser}] ->
	    do_usm_user_info(UsmUser, Item)
    end.

do_usm_user_info(#usm_user{sec_name = SecName}, sec_name) ->
    {ok, SecName};
do_usm_user_info(#usm_user{auth = AuthP}, auth) ->
    {ok, AuthP};
do_usm_user_info(#usm_user{auth_key = AuthKey}, auth_key) ->
    {ok, AuthKey};
do_usm_user_info(#usm_user{priv = PrivP}, priv) ->
    {ok, PrivP};
do_usm_user_info(#usm_user{priv_key = PrivKey}, priv_key) ->
    {ok, PrivKey};
do_usm_user_info(#usm_user{engine_id = EngineID}, engine_id) ->
    {ok, EngineID};
do_usm_user_info(#usm_user{name = Name}, name) ->
    {ok, Name};
do_usm_user_info(_, Item) ->
    {error, {bad_iten, Item}}.

update_usm_user_info(EngineID, UserName, Item, Val) 
  when (Item =/= engine_id) andalso (Item =/= name) ->
    call({update_usm_user_info, EngineID, UserName, Item, Val}).

get_usm_user(EngineID, UserName) ->
    Key = usm_key(EngineID, UserName),
    case ets:lookup(snmpm_usm_table, Key) of
	[{_, User}] ->
	    {ok, User};
	_ ->
	    {error, not_found}
    end.

is_usm_engine_id_known(EngineID) ->
    Pattern = {usm_key(EngineID, '$1'), '_'},
    case ets:match(snmpm_usm_table, Pattern) of
	[] ->
	    false;
	_ ->
	    true
    end.

get_usm_user_from_sec_name(EngineID, SecName) ->
    %% Since the normal mapping between UserName and SecName is the
    %% identity-function, we first try to use the SecName as UserName,
    %% and check the resulting row.  If it doesn't match, we'll have to
    %% loop through the entire table.
    Key = usm_key(EngineID, SecName),
    case ets:lookup(snmpm_usm_table, Key) of
	[{Key, #usm_user{sec_name = SecName} = User}] ->
	    {ok, User};
	_ ->
	    %% That did not work, so we have to search
	    Pattern = {usm_key(EngineID, '_'), 
		       #usm_user{sec_name = SecName, _ = '_'}},
	    case ets:match_object(snmpm_usm_table, Pattern) of
		[{_, User}|_] ->
		    {ok, User};
		_ ->
		    {error, not_found}
	    end
    end.


%% Wrap-counters (wrapping at 2147483647 or 4294967295)
cre_counter(Counter, Initial) ->
    case (whereis(?SERVER) =:= self()) of
	false ->
	    call({cre_counter, Counter, Initial});
	true ->
	    ets:insert(snmpm_counter_table, {Counter, Initial}),
	    Initial
    end.

incr_counter(usm_salt, Incr) ->        % Backward compatibillity (upgrade)
    incr_counter(usm_des_salt, Incr);  % Backward compatibillity (upgrade)
incr_counter(usm_des_salt, Incr) ->
    incr_counter(usm_des_salt, Incr, 4294967295);
incr_counter(usm_aes_salt, Incr) ->
    incr_counter(usm_aes_salt, Incr, 36893488147419103231);
incr_counter(Counter, Incr) ->
    incr_counter(Counter, Incr, 2147483647).

incr_counter(Counter, Incr, Wrap) ->
    case (catch ets:update_counter(snmpm_counter_table, Counter, Incr)) of
	{'EXIT', _} ->
	    cre_counter(Counter, Incr);
	NewVal when NewVal =< Wrap ->
	    NewVal;
	N ->
	    cre_counter(Counter, N - Wrap)
    end.


%% <ATL Sequence Number>
increment_counter(Counter, Initial, Max) ->
    Increment = 1, 
    increment_counter(Counter, Initial, Increment, Max).

increment_counter(Counter, Initial, Increment, Max) ->
    %% This is to make sure no one else increments our counter
    Key = {Counter, self()},

    %% Counter data
    Position  = 2,
    Threshold = Max,
    SetValue  = Initial,
    UpdateOp  = {Position, Increment, Threshold, SetValue},

    %% And now for the actual increment
    Tab = snmpm_counter_table, 
    case (catch ets:update_counter(Tab, Key, UpdateOp)) of
        {'EXIT', {badarg, _}} ->
            %% Oups, first time
            ets:insert(Tab, {Key, Initial}),
            Initial;
        Next when is_integer(Next) ->
            Next
    end.
%% </ATL Sequence Number>


maybe_cre_stats_counter(Counter, Initial) ->
    case ets:lookup(snmpm_stats_table, Counter) of
	[_] ->
	    ok;
	_ ->
	    cre_stats_counter(Counter, Initial)
    end.
			      
cre_stats_counter(Counter, Initial) ->
    case (whereis(?SERVER) =:= self()) of
	false ->
	    call({cre_stats_counter, Counter, Initial});
	true ->
	    ets:insert(snmpm_stats_table, {Counter, Initial}),
	    Initial
    end.

incr_stats_counter(Counter, Incr) ->
    case (catch ets:update_counter(snmpm_stats_table, Counter, Incr)) of
	{'EXIT', _} ->
	    cre_counter(Counter, Incr);
	NewVal ->
	    NewVal
    end.

reset_stats_counter(Counter) ->
    case (whereis(?SERVER) =:= self()) of
	false ->
	    call({reset_stats_counter, Counter});
	true ->
	    ets:insert(snmpm_stats_table, {Counter, 0})
    end,
    ok.
    
get_stats_counter(Counter) ->
    case ets:lookup(snmpm_stats_table, Counter) of
	[{Counter, Value}] ->
	    {ok, Value};
	_ ->
	    {error, not_found}
    end.
    
get_stats_counters() ->
    ets:tab2list(snmpm_stats_table).
    
load_mib(Mib) when is_list(Mib) ->
    call({load_mib, Mib}).

unload_mib(Mib) when is_list(Mib) ->
    call({unload_mib, Mib}).

which_mibs() ->
    Pattern = {{mib, '_'}, '$1', '$2'},
    Mibs = ets:match(snmpm_mib_table, Pattern),
    [list_to_tuple(X) || X <- Mibs].

name_to_oid(Name) ->
    Pat = {{mini_mib, '$1'}, Name, '_', '_'},
    case ets:match(snmpm_mib_table, Pat) of
	[] ->
	    {error, not_found};
	X ->
	    Oids = [Oid || [Oid] <- X],
	    {ok, Oids}
    end.

oid_to_name(Oid) ->
    case ets:lookup(snmpm_mib_table, {mini_mib, Oid}) of
	[{_, Name, _, _}] ->
	    {ok, Name};
	[] ->
	    {error, not_found}
    end.

oid_to_type(Oid) ->
    case ets:lookup(snmpm_mib_table, {mini_mib, Oid}) of
	[{_, _, Type, _}] ->
	    {ok, Type};
	[] ->
	    {error, not_found}
    end.

make_mini_mib() ->
    Pat = {{mini_mib, '$1'}, '$2', '$3', '_'},
    MiniElems = ets:match(snmpm_mib_table, Pat),
    lists:keysort(1, [list_to_tuple(MiniElem) || MiniElem <- MiniElems]).


info() ->
    call(info).

verbosity(Verbosity) ->
    case ?vvalidate(Verbosity) of
	Verbosity ->
	    call({verbosity, Verbosity});
	_ ->
	    {error, {invalid_verbosity, Verbosity}}
    end.


%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Opts]) ->
%     put(sname, mconf),
%     put(verbosity, trace),
    ?d("init -> entry with"
       "~n   Opts: ~p", [Opts]),
    case (catch do_init(Opts)) of
	ok ->
	    {ok, #state{}};
	{error, Reason} ->
	    error_msg("init error: ~p", [Reason]),
	    {stop, Reason};
	{'EXIT', Reason} ->
	    error_msg("init exit: ~p", [Reason]),
	    {stop, Reason};
	Error ->
	    error_msg("init failed: ~p", [Error]),
	    {stop, Error}
    end.

do_init(Opts) ->
    process_flag(trap_exit, true),
    %% Mandatory = [versions, {config, [dir]}],
    Mandatory = [{config, [dir, db_dir]}],
    verify_options(Opts, Mandatory),

    ets:new(snmpm_counter_table, [set, public,    named_table, {keypos, 1}]),
    ets:new(snmpm_stats_table,   [set, public,    named_table, {keypos, 1}]),
    ets:new(snmpm_mib_table,     [set, protected, named_table, {keypos, 1}]),
    ets:new(snmpm_config_table,  [set, protected, named_table, {keypos, 1}]),
    ets:new(snmpm_agent_table,   [set, protected, named_table, {keypos, 1}]),
    ets:new(snmpm_user_table,    [set, protected, named_table, {keypos, 2}]),
    ets:new(snmpm_usm_table,     [set, protected, named_table, {keypos, 1}]),

    %% -- System start time --
    ets:insert(snmpm_config_table, {system_start_time, snmp_misc:now(cs)}),
    
    %% --- Own options (dir and db_dir mandatory) ---
    ConfOpts      = get_opt(config,        Opts,     []),
    ConfVerb      = get_opt(verbosity,     ConfOpts, silence),
    ConfDir       = get_opt(dir,           ConfOpts),
    ConfDbDir     = get_opt(db_dir,        ConfOpts),
    ConfDbInitErr = get_opt(db_init_error, ConfOpts, terminate),
    ConfRep       = get_opt(repair,        ConfOpts, true),
    ConfAs        = get_opt(auto_save,     ConfOpts, 5000),
    ets:insert(snmpm_config_table, {config_verbosity,     ConfVerb}),
    ets:insert(snmpm_config_table, {config_dir,           ConfDir}),
    ets:insert(snmpm_config_table, {config_db_dir,        ConfDbDir}),
    ets:insert(snmpm_config_table, {config_db_init_error, ConfDbInitErr}),
    ets:insert(snmpm_config_table, {config_repair,        ConfRep}),
    ets:insert(snmpm_config_table, {config_auto_save,     ConfAs}),
    put(sname, mconf),
    put(verbosity, ConfVerb),
    ?vlog("starting", []),

    %% -- Create dets file used for storing persistent data --
    dets_open(ConfDbDir, ConfDbInitErr, ConfRep, ConfAs),
    
    %% -- Prio (optional) --
    Prio = get_opt(priority, Opts, normal),
    ets:insert(snmpm_config_table, {prio, Prio}),
    process_flag(priority, Prio),

    %% -- Server (optional) --
    ServerOpts = get_opt(server,         Opts,      []),
    ServerVerb = get_opt(verbosity,      ServerOpts, silence),
    ServerGct  = get_opt(timeout,        ServerOpts, 30000),
    ServerMt   = get_opt(multi_threaded, ServerOpts, true),
    ets:insert(snmpm_config_table, {server_verbosity,      ServerVerb}),
    ets:insert(snmpm_config_table, {server_timeout,        ServerGct}),
    ets:insert(snmpm_config_table, {server_multi_threaded, ServerMt}),
   
    %% -- Mibs (optional) --
    ?vdebug("initiate mini mib", []),
    Mibs = get_opt(mibs, Opts, []),
    ets:insert(snmpm_config_table, {mibs, Mibs}),
    init_mini_mib(Mibs),

    %% -- Net-if (optional) --
    ?vdebug("net_if options", []),
    NetIfIrb     = 
	case get_opt(inform_request_behaviour, Opts, ?IRB_DEFAULT) of
	    user ->
		{user, timer:seconds(15)};
	    Irb ->
		Irb
	end,
    NetIfOpts    = get_opt(net_if,                    Opts,      []),
    NetIfMod     = get_opt(module,                    NetIfOpts, snmpm_net_if),
    NetIfVerb    = get_opt(verbosity,                 NetIfOpts, silence),
    NetIfOptions = get_opt(options,                   NetIfOpts, []),
    ets:insert(snmpm_config_table, {net_if_module,    NetIfMod}),
    ets:insert(snmpm_config_table, {net_if_verbosity, NetIfVerb}),
    ets:insert(snmpm_config_table, {net_if_irb,       NetIfIrb}),
    ets:insert(snmpm_config_table, {net_if_options,   NetIfOptions}),

    %% -- Versions (optional) --
    %% -- Versions (mandatory) ???????????? --
    ?vdebug("versions", []),
    Vsns = get_opt(versions, Opts, [v1, v2, v3]),
    ets:insert(snmpm_config_table, {versions, Vsns}),

    %% -- Audit trail log (optional) --
    ?vdebug("audit trail log", []),
    case get_opt(audit_trail_log, Opts, []) of
	[] ->
	    ?vtrace("no ATL", []),
	    ets:insert(snmpm_config_table, {audit_trail_log, false});
	AuditTrailLogOpts ->
	    ?vtrace("ATL options: ~p", [AuditTrailLogOpts]),
	    ets:insert(snmpm_config_table, {audit_trail_log, true}),
	    LogDir   = get_atl_dir(AuditTrailLogOpts),
	    LogType  = get_atl_type(AuditTrailLogOpts),
	    LogSize  = get_atl_size(AuditTrailLogOpts),
	    LogRep   = get_atl_repair(AuditTrailLogOpts),
	    LogSeqNo = get_atl_seqno(AuditTrailLogOpts),
	    ets:insert(snmpm_config_table, {audit_trail_log_dir,    LogDir}),
	    ets:insert(snmpm_config_table, {audit_trail_log_type,   LogType}),
	    ets:insert(snmpm_config_table, {audit_trail_log_size,   LogSize}),
	    ets:insert(snmpm_config_table, {audit_trail_log_repair, LogRep}),
	    ets:insert(snmpm_config_table, {audit_trail_log_seqno,  LogSeqNo})
    end,

    %% -- System default agent config --
    ?vdebug("system default agent config", []),
    init_agent_default(),

    %% -- User (optional) --
    ?vdebug("default user", []),
    DefUserMod  = get_opt(def_user_mod,  Opts, ?USER_MOD_DEFAULT),
    DefUserData = get_opt(def_user_data, Opts, ?USER_DATA_DEFAULT),
    ets:insert(snmpm_config_table, {def_user_mod,  DefUserMod}),
    ets:insert(snmpm_config_table, {def_user_data, DefUserData}),
    
    {ok, SystemDefaultAgentConfig}       = agent_info(),
    DefUser = #user{id                   = ?DEFAULT_USER, 
		    mod                  = DefUserMod, 
		    data                 = DefUserData,
		    default_agent_config = SystemDefaultAgentConfig},
    ok = handle_register_user(DefUser),
    
    %% -- Note store --
    ?vdebug("note store", []),
    NoteStoreOpts    = get_opt(note_store, Opts, []),
    NoteStoreVerb    = get_opt(verbosity,  NoteStoreOpts, silence),
    NoteStoreTimeout = get_opt(timeout,    NoteStoreOpts, 30000),
    ets:insert(snmpm_config_table, {note_store_verbosity, NoteStoreVerb}),
    ets:insert(snmpm_config_table, {note_store_timeout,   NoteStoreTimeout}),
    
    %% -- Manager SNMP config --
    ?vdebug("manager snmp config", []),
    MgrConf = read_manager_config_file(ConfDir),
    init_manager_config(MgrConf),

    %% -- User config --
    ?vdebug("users config", []),
    Users = read_users_config_file(ConfDir),
    init_users_config(Users),

    %% -- Agents config --
    ?vdebug("agents config", []),
    Agents = read_agents_config_file(ConfDir),
    init_agents_config(Agents),

    %% -- USM config --
    UsmUsers = read_usm_config_file(ConfDir),
    init_usm_users_config(UsmUsers),

    %% -- snmp engine init --
    init_engine(),

    ?vlog("started", []),
    ok.


dets_open(Dir, DbInitError, Repair, AutoSave) ->
    Name     = ?CONFIG_DB, 
    Filename = dets_filename(Name, Dir),
    case file:read_file_info(Filename) of
	{ok, _} ->
	    %% File exists
	    case do_dets_open(Name, Filename, Repair, AutoSave) of
		{ok, _Dets} ->
		    ok;
		{error, Reason1} ->
                    info_msg("Corrupt local database: ~p", [Filename]),
		    case DbInitError of
			terminate ->
			    error({failed_reopen_dets, Filename, Reason1});
			_ ->
			    Saved = Filename ++ ".saved",
			    file:rename(Filename, Saved),
			    case do_dets_open(Name, Filename, 
					      Repair, AutoSave) of
				{ok, _Dets} ->
				    ok;
				{error, Reason2} ->
				    error({failed_open_dets, Filename, 
					   Reason1, Reason2})
			    end
		    end
	    end;
	_ ->
	    case do_dets_open(Name, Filename, Repair, AutoSave) of
		{ok, _Dets} ->
		    ok;
		{error, Reason} ->
		    error({failed_open_dets, Filename, Reason})
	    end
    end.
		
do_dets_open(Name, Filename, Repair, AutoSave) ->
    Opts = [{repair,    Repair}, 
	    {auto_save, AutoSave}, 
	    {file,      Filename}],
    dets:open_file(Name, Opts).


dets_filename(Name, Dir) when is_atom(Name) ->
    dets_filename(atom_to_list(Name), Dir);
dets_filename(Name, Dir) ->
    filename:join(dets_filename1(Dir), Name).

dets_filename1([])  -> ".";
dets_filename1(Dir) -> Dir.


%% ------------------------------------------------------------------------

init_engine() ->
    case get_engine_boots() of
	{ok, Val} when Val < 2147483647 ->
	    set_engine_boots(Val + 1);
	{ok, _} ->
	    ok;
	_ ->
	    set_engine_boots(1)
    end,
    reset_engine_base().

reset_engine_base() ->
    ets:insert(snmpm_config_table, {snmp_engine_base, snmp_misc:now(sec)}).


%% ------------------------------------------------------------------------

verify_options(Opts, Mandatory) ->
    ?d("verify_options -> entry with"
       "~n   Opts:      ~p"
       "~n   Mandatory: ~p", [Opts, Mandatory]),
    verify_mandatory_options(Opts, Mandatory),
    verify_options(Opts).

%% mandatory() -> [mand()]
%% mand() -> atom() | {atom, [atom()]}
verify_mandatory_options(_Opts, []) ->
    ok;
verify_mandatory_options(Opts, [Mand|Mands]) ->
    verify_mandatory_option(Opts, Mand),
    verify_mandatory_options(Opts, Mands).

verify_mandatory_option(Opts, {Mand, MandSubOpts}) ->
    ?d("verify_mandatory_option -> entry with"
       "~n   Mand:        ~p"
       "~n   MandSubObjs: ~p", [Mand, MandSubOpts]),
    case lists:keysearch(Mand, 1, Opts) of
	{value, {Mand, SubOpts}} ->
	    verify_mandatory_options(SubOpts, MandSubOpts);
	false ->
	    ?d("missing mandatory option: ~w [~p]", [Mand, MandSubOpts]),
	    error({missing_mandatory, Mand, MandSubOpts})
    end;
verify_mandatory_option(Opts, Mand) ->
    ?d("verify_mandatory_option -> entry with"
       "~n   Mand:        ~p", [Mand]),
    case lists:keymember(Mand, 1, Opts) of
	true ->
	    ok;
	false ->
	    ?d("missing mandatory option: ~w", [Mand]),
	    error({missing_mandatory, Mand})
    end.
	
verify_options([]) ->
    ?d("verify_options -> done", []),
    ok;
verify_options([Opt|Opts]) ->
    ?d("verify_options -> entry with"
       "~n   Opt: ~p", [Opt]),
    verify_option(Opt),
    verify_options(Opts).

verify_option({prio, Prio}) ->
    verify_prio(Prio);
verify_option({mibs, Mibs}) ->
    verify_mibs(Mibs);
verify_option({inform_request_behaviour, IRB}) ->
    verify_irb(IRB);
verify_option({net_if, NetIfOpts}) ->
    verify_net_if_opts(NetIfOpts);
verify_option({server, ServerOpts}) ->
    verify_server_opts(ServerOpts);
verify_option({note_store, NoteStoreOpts}) ->
    verify_note_store_opts(NoteStoreOpts);
verify_option({config, ConfOpts}) ->
    verify_config_opts(ConfOpts);
verify_option({versions, Vsns}) ->
    verify_versions(Vsns);
verify_option({audit_trail_log, LogOpts}) ->
    Mandatory = [dir, size],
    case (catch verify_mandatory_options(LogOpts, Mandatory)) of
	ok ->
	    verify_audit_trail_log_opts(LogOpts);
	{error, {missing_mandatory, LogOpt}} ->
	    error({missing_mandatory, audit_trail_log, LogOpt})
    end;
verify_option({def_user_mod, Mod}) ->
    verify_module(def_user_mod, Mod);
verify_option({def_user_data, _Data}) ->
    ok;
verify_option(Opt) ->
    {error, {invalid_option, Opt}}.

verify_prio(Prio) when is_atom(Prio) ->
    ok;
verify_prio(Prio) ->
    error({invalid_prio, Prio}).

verify_irb(auto) ->
    ok;
verify_irb(user) ->
    ok;
verify_irb({user, To}) when is_integer(To) andalso (To > 0) ->
    ok;
verify_irb(IRB) ->
    error({invalid_irb, IRB}).

verify_mibs([]) ->
    ok;
verify_mibs([Mib|Mibs]) when is_list(Mib) ->
    verify_mibs(Mibs);
verify_mibs(Mibs) ->
    error({invalid_mibs, Mibs}).

verify_config_opts([]) ->
    ok;
verify_config_opts([{verbosity, Verbosity}|Opts]) ->
    verify_verbosity(Verbosity),
    verify_config_opts(Opts);
verify_config_opts([{dir, Dir}|Opts]) ->
    verify_conf_dir(Dir),
    verify_config_opts(Opts);
verify_config_opts([{db_dir, Dir}|Opts]) ->
    verify_conf_db_dir(Dir),
    verify_config_opts(Opts);
verify_config_opts([{db_init_error, DbInitErr}|Opts]) ->
    verify_conf_db_init_error(DbInitErr),
    verify_config_opts(Opts);
verify_config_opts([{repair, Repair}|Opts]) ->
    verify_conf_repair(Repair),
    verify_config_opts(Opts);
verify_config_opts([{auto_save, AutoSave}|Opts]) ->
    verify_conf_auto_save(AutoSave),
    verify_config_opts(Opts);
verify_config_opts([Opt|_]) ->
    error({invalid_config_option, Opt}).

verify_server_opts([]) ->
    ok;
verify_server_opts([{verbosity, Verbosity}|Opts]) ->
    verify_verbosity(Verbosity),
    verify_server_opts(Opts);
verify_server_opts([{timeout, Timeout}|Opts]) ->
    verify_server_timeout(Timeout),
    verify_server_opts(Opts);
verify_server_opts([Opt|_]) ->
    error({invalid_server_option, Opt}).

verify_server_timeout(T) when is_integer(T) andalso (T > 0) ->
    ok;
verify_server_timeout(T) ->
    error({invalid_server_timeout, T}).

verify_net_if_opts([]) ->
    ok;
verify_net_if_opts([{module, Mod}|Opts]) ->
    verify_network_interface_behaviour(Mod),
    verify_net_if_opts(Opts);
verify_net_if_opts([{verbosity, Verbosity}|Opts]) ->
    verify_verbosity(Verbosity),
    verify_net_if_opts(Opts);
verify_net_if_opts([{options, Options}|Opts]) when is_list(Options) ->
    verify_net_if_opts(Opts);
verify_net_if_opts([Opt|_]) ->
    error({invalid_net_if_option, Opt}).

verify_network_interface_behaviour(Mod) ->
    case snmp_misc:verify_behaviour(snmpm_network_interface, Mod) of
	ok ->
	    ok;
	Error ->
	    throw(Error)
    end.
    
    
verify_note_store_opts([]) ->
    ok;
verify_note_store_opts([{verbosity, Verbosity}|Opts]) ->
    verify_verbosity(Verbosity),
    verify_note_store_opts(Opts);
verify_note_store_opts([{timeout, Timeout}|Opts]) ->
    verify_note_store_timeout(Timeout),
    verify_note_store_opts(Opts);
verify_note_store_opts([Opt|_]) ->
    error({invalid_note_store_option, Opt}).

verify_note_store_timeout(T) when is_integer(T) andalso (T > 0) ->
    ok;
verify_note_store_timeout(T) ->
    error({invalid_note_store_timeout, T}).

verify_conf_dir(Dir) ->
    case (catch verify_dir(Dir)) of
	ok ->
	    ok;
	{error, Reason} ->
	    error({invalid_conf_dir, Dir, Reason});
	_ ->
	    error({invalid_conf_dir, Dir})
    end.

verify_conf_db_dir(Dir) ->
    case (catch verify_dir(Dir)) of
	ok ->
	    ok;
	{error, Reason} ->
	    error({invalid_conf_db_dir, Dir, Reason});
	_ ->
	    error({invalid_conf_db_dir, Dir})
    end.


verify_conf_db_init_error(terminate) ->
    ok;
verify_conf_db_init_error(create) ->
    ok;
verify_conf_db_init_error(InvalidDbInitError) ->
    error({invalid_conf_db_init_error, InvalidDbInitError}).


verify_conf_repair(true) ->
    ok;
verify_conf_repair(false) ->
    ok;
verify_conf_repair(force) ->
    ok;
verify_conf_repair(InvalidRepair) ->
    error({invalid_conf_db_repair, InvalidRepair}).


verify_conf_auto_save(infinity) ->
    ok;
verify_conf_auto_save(AutoSave) 
  when is_integer(AutoSave) andalso (AutoSave > 0) ->
    ok;
verify_conf_auto_save(InvalidAutoSave) ->
    error({invalid_conf_db_auto_save, InvalidAutoSave}).


verify_versions([]) ->
    ok;
verify_versions([Vsn|Vsns]) ->
    verify_version(Vsn),
    verify_versions(Vsns).

verify_version(v1) ->
    ok;
verify_version(v2) ->
    ok;
verify_version(v3) ->
    ok;
verify_version(Vsn) ->
    error({invalid_version, Vsn}).

verify_audit_trail_log_opts([]) ->
    ok;
verify_audit_trail_log_opts([{dir, Dir}|Opts]) ->
    verify_log_dir(Dir),
    verify_audit_trail_log_opts(Opts);
verify_audit_trail_log_opts([{type, Type}|Opts]) ->
    verify_log_type(Type),
    verify_audit_trail_log_opts(Opts);
verify_audit_trail_log_opts([{size, Size}|Opts]) ->
    verify_log_size(Size),
    verify_audit_trail_log_opts(Opts);
verify_audit_trail_log_opts([{repair, Repair}|Opts]) ->
    verify_log_repair(Repair),
    verify_audit_trail_log_opts(Opts);
verify_audit_trail_log_opts([{seqno, SeqNo}|Opts]) ->
    verify_log_seqno(SeqNo),
    verify_audit_trail_log_opts(Opts);
verify_audit_trail_log_opts([Opt|_Opts]) ->
    error({invalid_audit_trail_log_option, Opt}).

verify_log_type(read) ->
    ok;
verify_log_type(write) ->
    ok;
verify_log_type(read_write) ->
    ok;
verify_log_type(Type) ->
    error({invalid_audit_trail_log_type, Type}).

verify_log_dir(Dir) ->
    case (catch verify_dir(Dir)) of
	ok ->
	    ok;
	{error, Reason} ->
	    error({invalid_audit_trail_log_dir, Dir, Reason});
	_ ->
	    error({invalid_audit_trail_log_dir, Dir})
    end.

verify_log_size(Sz) when is_integer(Sz) andalso (Sz > 0) ->
    ok;
verify_log_size(infinity) ->
    ok;
verify_log_size({MaxNoBytes, MaxNoFiles}) 
  when (is_integer(MaxNoBytes) andalso 
	(MaxNoBytes > 0) andalso 
	is_integer(MaxNoFiles) andalso 
	(MaxNoFiles > 0) andalso 
	(MaxNoFiles < 65000)) ->
    ok;
verify_log_size(Sz) ->
    error({invalid_audit_trail_log_size, Sz}).

verify_log_repair(true) -> ok;
verify_log_repair(false) -> ok;
verify_log_repair(truncate) -> ok;
verify_log_repair(Repair) ->
    error({invalid_audit_trail_log_repair, Repair}).

verify_log_seqno(true) -> ok;
verify_log_seqno(false) -> ok;
verify_log_seqno(SeqNo) ->
    error({invalid_audit_trail_log_seqno, SeqNo}).


verify_module(_, Mod) when is_atom(Mod) ->
    ok;
verify_module(ReasonTag, Mod) ->
    error({invalid_module, ReasonTag, Mod}).

% verify_bool(_, true) ->
%     ok;
% verify_bool(_, false) ->
%     ok;
% verify_bool(ReasonTag, Bool) ->
%     error({invalid_bool, ReasonTag, Bool}).

verify_dir(Dir) when is_list(Dir) ->
    case file:read_file_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    ok;
	{ok, _} ->
	    {error, not_directory};
	{error, _Reason} ->
	    {error, not_found}
    end;
verify_dir(Dir) ->
    {error, {invalid_log_dir, Dir}}.


verify_verbosity(Verbosity) ->
    case snmp_verbosity:validate(Verbosity) of
	Verbosity ->
	    ok;
	_ ->
	    error({invalid_verbosity, Verbosity})
    end.
    
%% ------------------------------------------------------------------------

init_manager_config([]) ->
    ok;
init_manager_config([{Key, Val}|Confs]) ->
    ets:insert(snmpm_config_table, {Key, Val}),
    init_manager_config(Confs).



init_agent_default() ->
    %% The purpose of the default_agent is only to have a place
    %% to store system wide default values related to agents.
    %% 

    %% Port
    init_agent_default(port, ?DEFAULT_AGENT_PORT),

    %% Timeout
    init_agent_default(timeout, 10000),

    %% Max message (packet) size
    init_agent_default(max_message_size, 484),

    %% MPModel
    init_agent_default(version, v2),

    %% SecModel
    init_agent_default(sec_model, v2c),

    %% SecName
    init_agent_default(sec_name, "initial"),

    %% SecLevel
    init_agent_default(sec_level, noAuthNoPriv),

    %% Community
    init_agent_default(community, "all-rights"),
    ok.


init_agent_default(Item, Val) when Item =/= user_id ->
    case do_update_agent_info(default_agent, Item, Val) of
	ok ->
	    ok;
	{error, Reason} ->
	    error(Reason)
    end.


read_agents_config_file(Dir) ->
    Check = fun(C) -> check_agent_config2(C) end,
    case read_file(Dir, "agents.conf", Check, []) of
	{ok, Conf} ->
	    Conf;
	Error ->
	    ?vlog("agent config error: ~p", [Error]),
	    throw(Error)
    end.

check_agent_config2(Agent) ->
    case (catch check_agent_config(Agent)) of
	{ok, {UserId, TargetName, Conf, Version}} ->
	    {ok, Vsns} = system_info(versions),
	    case lists:member(Version, Vsns) of
		true ->
		    {ok, {UserId, TargetName, Conf}};
		false ->
		    error({version_not_supported_by_manager, 
			   Version, Vsns})
	    end;
	Err ->
	    throw(Err)
    end.

check_agent_config({UserId, 
		    TargetName, 
		    Community, 
		    Ip, Port, 
		    EngineId, 
		    Timeout, MaxMessageSize, 
		    Version, SecModel, SecName, SecLevel}) ->
    ?vtrace("check_agent_config -> entry with"
	    "~n   UserId:         ~p"
	    "~n   TargetName:     ~p"
	    "~n   Community:      ~p"
	    "~n   Ip:             ~p"
	    "~n   Port:           ~p"
	    "~n   EngineId:       ~p"
	    "~n   Timeout:        ~p"
	    "~n   MaxMessageSize: ~p"
	    "~n   Version:        ~p"
	    "~n   SecModel:       ~p"
	    "~n   SecName:        ~p"
	    "~n   SecLevel:       ~p", 
	    [UserId, TargetName, Community, Ip, Port, 
	     EngineId, Timeout, MaxMessageSize, 
	     Version, SecModel, SecName, SecLevel]),
    Addr = normalize_address(Ip),
    ?vtrace("check_agent_config -> Addr: ~p", [Addr]),
    Agent = {UserId, 
	     TargetName, 
	     Community, 
	     Addr, Port, 
	     EngineId, 
	     Timeout, MaxMessageSize, 
	     Version, SecModel, SecName, SecLevel},
    {ok, verify_agent(Agent)};
check_agent_config(Agent) ->
    error({bad_agent_config, Agent}).


init_agents_config([]) ->
    ok;
init_agents_config([Agent|Agents]) ->
    init_agent_config(Agent),
    init_agents_config(Agents).

init_agent_config({_UserId, ?DEFAULT_TARGETNAME = TargetName, _Config}) ->
    throw({error, {invalid_target_name, TargetName}});
init_agent_config({UserId, TargetName, Config}) ->
    case handle_register_agent(UserId, TargetName, Config) of
	ok ->
	    ok;
	Error ->
	    throw(Error)
    end.


verify_agent({UserId, 
	      TargetName, 
	      Comm, 
	      Ip, Port, 
	      EngineId, 
	      Timeout, MMS, 
	      Version, SecModel, SecName, SecLevel}) ->
    ?vtrace("verify_agent -> entry with"
	    "~n   UserId:     ~p"
	    "~n   TargetName: ~p", [UserId, TargetName]),
    snmp_conf:check_string(TargetName, {gt, 0}),
    case verify_val(address, Ip) of
	{ok, Addr} ->
	    snmp_conf:check_integer(Port, {gt, 0}),
	    Conf = 
		[{reg_type,         target_name},
		 {address,          Addr},
		 {port,             Port},
		 {community,        Comm}, 
		 {engine_id,        EngineId},
		 {timeout,          Timeout},
		 {max_message_size, MMS},
		 {version,          Version},
		 {sec_model,        SecModel},
		 {sec_name,         SecName},
		 {sec_level,        SecLevel}
		],
	    case verify_agent2(Conf) of
		ok ->
		    {UserId, TargetName, Conf, Version};
		Err ->
		    throw(Err)
	    end;
	
	Error ->
	    ?vlog("verify_agent -> failed: ~n   ~p", [Error]),
	    throw(Error)
    end.

verify_agent2([]) ->
    ok;
verify_agent2([{Item, Val}|Items]) ->
    case verify_val(Item, Val) of
	{ok, _Val} ->
	    verify_agent2(Items);
	Err ->
	    Err
    end;
verify_agent2([Bad|_]) ->
    {error, {bad_agent_config, Bad}}.


read_users_config_file(Dir) ->
    Check = fun(C) -> check_user_config(C) end,
    case read_file(Dir, "users.conf", Check, []) of
	{ok, Conf} ->
	    Conf;
	Error ->
	    ?vlog("failure reading users config file: ~n   ~p", [Error]),
	    throw(Error)
    end.


check_user_config({Id, Mod, Data}) ->
    check_user_config({Id, Mod, Data, []});
check_user_config({Id, Mod, _Data, DefaultAgentConfig} = User) 
  when (Id =/= ?DEFAULT_USER) andalso is_list(DefaultAgentConfig) ->
    case (catch verify_user_behaviour(Mod)) of
	ok ->
	    case verify_user_agent_config(DefaultAgentConfig) of
		ok ->
	    	    {ok, User};
		{error, Reason} ->
		    error({bad_default_agent_config, Reason})
	    end;
	Error ->
	    throw(Error)
    end;
check_user_config({Id, _Mod, _Data, DefaultAgentConfig}) 
  when (Id =/= ?DEFAULT_USER) ->
    {error, {bad_default_agent_config, DefaultAgentConfig}};
check_user_config({Id, _Mod, _Data, _DefaultAgentConfig}) ->
    error({bad_user_id, Id});
check_user_config(User) ->
    error({bad_user_config, User}).

init_users_config([]) ->
    ok;
init_users_config([User|Users]) ->
    init_user_config(User),
    init_users_config(Users).

init_user_config(User) ->
    case (catch verify_user(User)) of
	{ok, UserRec} ->
	    case handle_register_user(UserRec) of
		ok ->
		    ok;
		{error, Reason} ->
		    error_msg("failed register user: "
			      "~n~w~n~w", [User, Reason])
	    end;
	{error, Reason} ->
	    error_msg("user config check failed: "
		      "~n~w~n~w", [User, Reason])
    end.
   
verify_user({Id, UserMod, UserData}) ->
    verify_user({Id, UserMod, UserData, []});
verify_user({Id, UserMod, UserData, DefaultAgentConfig}) 
  when (Id =/= ?DEFAULT_USER) andalso is_list(DefaultAgentConfig) ->
    ?d("verify_user -> entry with"
       "~n   Id:       ~p"
       "~n   UserMod:  ~p"
       "~n   UserData: ~p"
       "~n   DefaultAgentConfig: ~p", 
       [Id, UserMod, UserData, DefaultAgentConfig]),
    case (catch verify_user_behaviour(UserMod)) of
	ok ->
	    case verify_user_agent_config(DefaultAgentConfig) of
		ok ->
		    Config = default_agent_config(DefaultAgentConfig),
		    {ok, #user{id                   = Id, 
			       mod                  = UserMod, 
			       data                 = UserData, 
			       default_agent_config = Config}};
		{error, Reason} ->
		    error({bad_default_agent_config, Reason})
	    end;
	Error ->
	    throw(Error)
    end;
verify_user({Id, _UserMod, _UserData, DefaultAgentConfig}) 
  when (Id =/= ?DEFAULT_USER) ->
    {error, {bad_default_agent_config, DefaultAgentConfig}};
verify_user({Id, _, _, _}) ->
    {error, {bad_user_id, Id}}.

verify_user_agent_config(Conf) ->
    case verify_invalid(Conf, [user_id, engine_id, address]) of
	ok ->
	    verify_agent_config2(Conf);
	Error ->
	    Error
    end.

read_usm_config_file(Dir) ->
    Check = fun(C) -> check_usm_user_config(C) end,
    case read_file(Dir, "usm.conf", Check, []) of
	{ok, Conf} ->
	    Conf;
	Error ->
	    throw(Error)
    end.

%% Identity-function
check_usm_user_config({EngineId, Name, 
		       AuthP, AuthKey, 
		       PrivP, PrivKey}) ->
    User = {EngineId, Name, Name, AuthP, AuthKey, PrivP, PrivKey},
    verify_usm_user(User);
check_usm_user_config({_EngineId, _Name, _SecName, 
		       _AuthP, _AuthKey, 
		       _PrivP, _PrivKey} = User) ->
    verify_usm_user(User);
check_usm_user_config(User) ->
    error({bad_usm_config, User}).

init_usm_users_config([]) ->
    ok;
init_usm_users_config([User|Users]) ->
    init_usm_user_config(User),
    init_usm_users_config(Users).

init_usm_user_config(User) when is_record(User, usm_user) ->
    case handle_register_usm_user(User) of
	ok ->
	    ok;
	Error ->
	    throw(Error)
    end;
init_usm_user_config(BadUser) ->
    error({bad_usm_user, BadUser}).


verify_usm_user({EngineID, Name, SecName, AuthP, AuthKey, PrivP, PrivKey}) ->
    ?d("verify_usm_user -> entry with"
       "~n   EngineID: ~p"
       "~n   Name:     ~p"
       "~n   SecName:  ~p"
       "~n   AuthP:    ~p"
       "~n   AuthKey:  ~p"
       "~n   PrivP:    ~p"
       "~n   PrivKey:  ~p", 
       [EngineID, Name, SecName, AuthP, AuthKey, PrivP, PrivKey]),
    verify_usm_user_engine_id(EngineID),
    verify_usm_user_name(Name),
    verify_usm_user_sec_name(SecName),
    verify_usm_user(AuthP, AuthKey, PrivP, PrivKey),
    User = #usm_user{engine_id = EngineID,
		     name      = Name,
		     sec_name  = SecName,
		     auth      = AuthP, 
		     auth_key  = AuthKey,
		     priv      = PrivP, 
		     priv_key  = PrivKey},
    {ok, User}.

verify_usm_user_engine_id(EngineID) ->
    case (catch snmp_conf:check_string(EngineID, {gt, 0})) of
	ok ->
	    ok;
	_ ->
	    error({bad_usm_engine_id, EngineID})
    end.

verify_usm_user_name(Name) ->
    case (catch snmp_conf:check_string(Name, {gt, 0})) of
	ok ->
	    ok;
	_ ->
	    error({bad_usm_user_name, Name})
    end.

verify_usm_user_sec_name(Name) ->
    case (catch snmp_conf:check_string(Name, {gt, 0})) of
	ok ->
	    ok;
	_ ->
	    error({bad_usm_sec_name, Name})
    end.

verify_usm_user(AuthP, AuthKey, PrivP, PrivKey) ->
    verify_usm_user_auth(AuthP, AuthKey),
    verify_usm_user_priv(PrivP, PrivKey),
    ok.

verify_usm_user_auth(usmNoAuthProtocol, AuthKey) ->
    case (catch snmp_conf:check_string(AuthKey, any)) of
	ok ->
	    ok;
	_ ->
	    error({invalid_auth_key, usmNoAuthProtocol})
    end;
verify_usm_user_auth(usmHMACMD5AuthProtocol, AuthKey) 
  when is_list(AuthKey) andalso (length(AuthKey) =:= 16) ->
    case is_crypto_supported(md5_mac_96) of
	true -> 
	    case snmp_conf:all_integer(AuthKey) of
		true ->
		    ok;
		_ ->
		    error({invalid_auth_key, usmHMACMD5AuthProtocol})
	    end;
	false -> 
	    error({unsupported_crypto, md5_mac_96})
    end;    
verify_usm_user_auth(usmHMACMD5AuthProtocol, AuthKey) when is_list(AuthKey) ->
    Len = length(AuthKey),
    error({invalid_auth_key, usmHMACMD5AuthProtocol, Len});
verify_usm_user_auth(usmHMACMD5AuthProtocol, _AuthKey) ->
    error({invalid_auth_key, usmHMACMD5AuthProtocol});
verify_usm_user_auth(usmHMACSHAAuthProtocol, AuthKey) 
  when is_list(AuthKey) andalso (length(AuthKey) =:= 20) ->
    case is_crypto_supported(sha_mac_96) of
	true -> 
	    case snmp_conf:all_integer(AuthKey) of
		true ->
		    ok;
		_ ->
		    error({invalid_auth_key, usmHMACSHAAuthProtocol})
	    end;
	false -> 
	    error({unsupported_crypto, sha_mac_96})
    end;
verify_usm_user_auth(usmHMACSHAAuthProtocol, AuthKey) when is_list(AuthKey) ->
    Len = length(AuthKey),
    error({invalid_auth_key, usmHMACSHAAuthProtocol, Len});
verify_usm_user_auth(usmHMACSHAAuthProtocol, _AuthKey) ->
    error({invalid_auth_key, usmHMACSHAAuthProtocol});
verify_usm_user_auth(AuthP, _AuthKey) ->
    error({invalid_auth_protocol, AuthP}).
    
verify_usm_user_priv(usmNoPrivProtocol, PrivKey) ->
    case (catch snmp_conf:check_string(PrivKey, any)) of
	ok ->
	    ok;
	_ ->
	    error({invalid_priv_key, usmNoPrivProtocol})
    end;
verify_usm_user_priv(usmDESPrivProtocol, PrivKey) 
  when (length(PrivKey) =:= 16) ->
    case is_crypto_supported(des_cbc_decrypt) of
	true -> 
	    case snmp_conf:all_integer(PrivKey) of
		true ->
		    ok;
		_ ->
		    error({invalid_priv_key, usmDESPrivProtocol})
	    end;
	false -> 
	    error({unsupported_crypto, des_cbc_decrypt})
    end;
verify_usm_user_priv(usmDESPrivProtocol, PrivKey) when is_list(PrivKey) ->
    Len = length(PrivKey),
    error({invalid_priv_key, usmDESPrivProtocol, Len});
verify_usm_user_priv(usmDESPrivProtocol, _PrivKey) ->
    error({invalid_priv_key, usmDESPrivProtocol});
verify_usm_user_priv(usmAesCfb128Protocol, PrivKey) 
  when (length(PrivKey) =:= 16) ->
    case is_crypto_supported(aes_cfb_128_decrypt) of
	true -> 
	    case snmp_conf:all_integer(PrivKey) of
		true ->
		    ok;
		_ ->
		    error({invalid_priv_key, usmAesCfb128Protocol})
	    end;
	false -> 
	    error({unsupported_crypto, aes_cfb_128_decrypt})
    end;
verify_usm_user_priv(usmAesCfb128Protocol, PrivKey) when is_list(PrivKey) ->
    Len = length(PrivKey),
    error({invalid_priv_key, usmAesCfb128Protocol, Len});
verify_usm_user_priv(usmAesCfb128Protocol, _PrivKey) ->
    error({invalid_priv_key, usmAesCfb128Protocol});
verify_usm_user_priv(PrivP, _PrivKey) ->
    error({invalid_priv_protocol, PrivP}).
    
is_crypto_supported(Func) ->
    %% The 'catch' handles the case when 'crypto' is
    %% not present in the system (or not started).
    case (catch lists:member(Func, crypto:info())) of
        true -> true;
        _ -> false
    end.
 

read_manager_config_file(Dir) ->
    Check = fun(Conf) -> check_manager_config(Conf) end,
    case read_file(Dir, "manager.conf", Check) of
	{ok, Conf} ->
	    ?d("read_manager_config_file -> ok: "
	       "~n   Conf: ~p", [Conf]),
	    %% If the address is not specified, then we assume
	    %% it should be the local host.
	    %% If the address is not possible to determine
	    %% that way, then we give up...
	    check_mandatory_manager_config(Conf),
	    ensure_manager_config(Conf);
	Error ->
	    throw(Error)
    end.

default_manager_config() ->
    {ok, HostName} = inet:gethostname(),
    case inet:getaddr(HostName, inet) of
	{ok, A} ->
	    [{address, tuple_to_list(A)}];
	{error, _Reason} ->
	    ?d("default_manager_config -> failed getting address: "
	       "~n   _Reason: ~p", [_Reason]),
	    []
    end.
    
check_manager_config({address, Addr}) ->
    snmp_conf:check_ip(Addr);
check_manager_config({port, Port}) ->
    snmp_conf:check_integer(Port, {gt, 0});
check_manager_config({engine_id, EngineID}) ->
    snmp_conf:check_string(EngineID);
check_manager_config({max_message_size, Max}) ->
    snmp_conf:check_integer(Max, {gte, 484});
check_manager_config(Conf) ->
    {error, {unknown_config, Conf}}.


check_mandatory_manager_config(Conf) ->
    Mand  = [port, engine_id, max_message_size],
    check_mandatory_manager_config(Mand, Conf).

check_mandatory_manager_config([], _Conf) ->
    ok;
check_mandatory_manager_config([Item|Mand], Conf) ->
    case lists:keysearch(Item, 1, Conf) of
	false ->
	    error({missing_mandatory_manager_config, Item});
	_ ->
	    check_mandatory_manager_config(Mand, Conf)
    end.
    

ensure_manager_config(Confs) ->
    ensure_manager_config(Confs, default_manager_config()).

ensure_manager_config(Confs, []) ->
    Confs;
ensure_manager_config(Confs, [{Key,_} = DefKeyVal|Defs]) ->
    case lists:keysearch(Key, 1, Confs) of
	false ->
	    ensure_manager_config([DefKeyVal|Confs], Defs);
	{value, _Conf} ->
	    ensure_manager_config(Confs, Defs)
    end.

% ensure_manager_config([], Defs, Confs) ->
%     Confs ++ Defs;
% ensure_manager_config(Confs0, [{Key, DefVal}|Defs], Acc) ->
%     case lists:keysearch(Key, 1, Confs0) of
% 	false ->
% 	    ensure_manager_config(Confs0, Defs, [{Key, DefVal}|Acc]);
% 	{value, Conf} ->
% 	    Confs = lists:keydelete(Key, 1, Confs0),
% 	    ensure_manager_config(Confs, Defs, [Conf|Acc])
%     end.
			      


read_file(Dir, FileName, Check, Default) ->
    File = filename:join(Dir, FileName),
    case file:read_file_info(File) of
        {ok, _} ->
            case (catch do_read(File, Check)) of
		{ok, Conf} ->
		    {ok, Conf};
		Error ->
		    ?vtrace("read_file -> read failed:"
			    "~n   Error: ~p", [Error]),
		    Error
	    end;
        {error, Reason} ->
	    ?vlog("failed reading config from ~s: ~p", [FileName, Reason]),
	    {ok, Default}
    end.

read_file(Dir, FileName, Check) ->
    File = filename:join(Dir, FileName),
    case file:read_file_info(File) of
        {ok, _} ->
            case (catch do_read(File, Check)) of
		{ok, Conf} ->
		    ?vtrace("read_file -> read ok"
			    "~n   Conf: ~p", [Conf]),
		    {ok, Conf};
		Error ->
		    ?vtrace("read_file -> read failed:"
			    "~n   Error: ~p", [Error]),
		    Error
	    end;
        {error, Reason} ->
	    error_msg("failed reading config from ~s: ~p", [FileName, Reason]),
	    {error, {failed_reading, FileName, Reason}}
    end.

do_read(File, Check) ->
    {ok, snmp_conf:read(File, Check)}.


%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({register_user, UserId, UserMod, UserData, DefaultAgentConfig}, 
	    _From, State) ->
    ?vlog("received register_user request: "
	  "~n   UserId:             ~p"
	  "~n   UserMod:            ~p"
	  "~n   UserData:           ~p"
	  "~n   DefaultAgentConfig: ~p", 
	  [UserId, UserMod, UserData, DefaultAgentConfig]),
    User  = #user{id                   = UserId, 
		  mod                  = UserMod, 
		  data                 = UserData,
		  default_agent_config = DefaultAgentConfig},
    Reply = handle_register_user(User),
    {reply, Reply, State};

handle_call({unregister_user, UserId}, _From, State) ->
    ?vlog("received unregister_user request: "
	  "~n   UserId: ~p", [UserId]),
    Reply = handle_unregister_user(UserId),
    {reply, Reply, State};

handle_call({register_agent, UserId, TargetName, Config}, _From, State) ->
    ?vlog("received register_agent request: "
	  "~n   UserId:     ~p"
	  "~n   TargetName: ~p"
	  "~n   Config:     ~p", [UserId, TargetName, Config]),
    Reply = handle_register_agent(UserId, TargetName, Config),
    {reply, Reply, State};

handle_call({unregister_agent, UserId, TargetName}, _From, State) ->
    ?vlog("received unregister_agent request: "
	  "~n   UserId:     ~p"
	  "~n   TargetName: ~p", [UserId, TargetName]),
    Reply = handle_unregister_agent(UserId, TargetName),
    {reply, Reply, State};

handle_call({update_agent_info, UserId, TargetName, Item, Val}, 
	    _From, State) ->
    ?vlog("received update_agent_info request: "
	  "~n   UserId:     ~p"
	  "~n   TargetName: ~p"
	  "~n   Item:       ~p"
	  "~n   Val:        ~p", [UserId, TargetName, Item, Val]),
    Reply = handle_update_agent_info(UserId, TargetName, Item, Val),
    {reply, Reply, State};

handle_call({register_usm_user, User}, _From, State) ->
    ?vlog("received register_usm_user request: "
	  "~n   User: ~p", [User]),
    Reply = handle_register_usm_user(User),
    {reply, Reply, State};

handle_call({unregister_usm_user, EngineID, Name}, _From, State) ->
    ?vlog("received register_usm_user request: "
	  "~n   EngineID: ~p"
	  "~n   Name:     ~p", [EngineID, Name]),
    Reply = handle_unregister_usm_user(EngineID, Name),
    {reply, Reply, State};

handle_call({update_usm_user_info, EngineID, UserName, Item, Val}, 
	    _From, State) ->
    ?vlog("received update_usm_user_info request: "
	  "~n   EngineID: ~p"
	  "~n   UserName: ~p"
	  "~n   Item:     ~p"
	  "~n   Val:      ~p", [EngineID, UserName, Item, Val]),
    Reply = handle_update_usm_user_info(EngineID, UserName, Item, Val),
    {reply, Reply, State};

handle_call({cre_counter, Counter, Initial}, _From, State) ->
    ?vlog("received cre_counter ~p -> ~w", [Counter, Initial]),
    Reply = cre_counter(Counter, Initial),
    {reply, Reply, State};

handle_call({cre_stats_counter, Counter, Initial}, _From, State) ->
    ?vlog("received cre_stats_counter ~p -> ~w", [Counter, Initial]),
    Reply = cre_stats_counter(Counter, Initial),
    {reply, Reply, State};

handle_call({reset_stats_counter, Counter}, _From, State) ->
    ?vlog("received reset_stats_counter ~p", [Counter]),
    Reply = reset_stats_counter(Counter),
    {reply, Reply, State};

handle_call({load_mib, Mib}, _From, State) ->
    ?vlog("received load_mib ~p", [Mib]),
    case handle_load_mib(Mib) of
	ok ->
	    {reply, ok, State};
	Error ->
	    {reply, Error, State}
    end;


handle_call({unload_mib, Mib}, _From, State) ->
    ?vlog("received unload_mib ~p", [Mib]),
    case handle_unload_mib(Mib) of
	ok ->
	    {reply, ok, State};
	Error ->
	    {reply, Error, State}
    end;


handle_call({set_engine_boots, Boots}, _From, State) ->
    ?vlog("received set_engine_boots ~p", [Boots]),
    set_engine_boots(Boots),
    {reply, ok, State};

handle_call({set_engine_time, Time}, _From, State) ->
    ?vlog("received set_engine_time ~p", [Time]),
    Base = snmp_misc:now(sec) - Time,
    ets:insert(snmpm_config_table, {snmp_engine_base, Base}),
    {reply, ok, State};

handle_call({set_usm_cache, Key, Val}, _From, State) ->
    ?vlog("received set_usm_cache: ~w -> ~p", [Key, Val]),
    ets:insert(snmpm_usm_table, {{usm_cache, Key}, Val}),
    {reply, ok, State};

handle_call({reset_usm_cache, EngineID}, _From, State) ->
    ?vlog("received reset_usm_cache: ~p", [EngineID]),
    reset_usm_cache(EngineID),
    {reply, ok, State};

handle_call({verbosity, Verbosity}, _From, State) ->
    ?vlog("received verbosity request", []),
    put(verbosity, Verbosity),
    {reply, ok, State};

handle_call(info, _From, State) ->
    ?vlog("received info request", []),
    Reply = get_info(),
    {reply, Reply, State};

handle_call({backup, BackupDir}, From, State) ->
    ?vlog("backup to ~p", [BackupDir]),
    Pid = self(),
    V   = get(verbosity),
    case file:read_file_info(BackupDir) of
        {ok, #file_info{type = directory}} ->
            BackupServer =
                erlang:spawn_link(
                  fun() ->
                          put(sname, mcbs),
                          put(verbosity, V),
                          Dir   = filename:join([BackupDir]),
                          Reply = handle_backup(?CONFIG_DB, Dir),
                          Pid ! {backup_done, Reply},
                          unlink(Pid)
                  end),
            ?vtrace("backup server: ~p", [BackupServer]),
            {noreply, State#state{backup = {BackupServer, From}}};
        {ok, _} ->
            {reply, {error, not_a_directory}, State};
        Error ->
            {reply, Error, State}
    end;


%% handle_call({update_system_info, Key, Val}, _From, State) ->
%%     ?vlog("received update_system_info: ~p -> ~p", [Key, Val]),
%%     Reply = handle_update_system_info(Key, Val),
%%     {reply, Reply, State};


handle_call(is_started, _From, State) ->
    ?vlog("received is_started request", []),
    {reply, true, State};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(Req, _From, State) ->
    warning_msg("received unknown request: ~n~p", [Req]),
    {reply, {error, unknown_request}, State}.


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    warning_msg("received unknown message: ~n~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #state{backup = {Pid, From}} = S) ->
    ?vlog("backup server (~p) exited for reason ~n~p", [Pid, Reason]),
    gen_server:reply(From, {error, Reason}),
    {noreply, S#state{backup = undefined}};

handle_info({'EXIT', Pid, Reason}, S) ->
    %% The only other processes we should be linked to are
    %% either the server or our supervisor, so die...
    {stop, {received_exit, Pid, Reason}, S};

handle_info({backup_done, Reply}, #state{backup = {_, From}} = S) ->
    ?vlog("backup done:"
          "~n   Reply: ~p", [Reply]),
    gen_server:reply(From, Reply),
    {noreply, S#state{backup = undefined}};

handle_info(Info, State) ->
    warning_msg("received unknown info: ~n~p", [Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    ?vdebug("terminate: ~p",[Reason]),
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------

%% downgrade
%%
code_change({down, _Vsn}, S1, downgrade_to_pre_4_7) ->
    #state{backup = B} = S1,
    stop_backup_server(B),
    S2 = {state},
    {ok, S2};

%% upgrade
%%
code_change(_Vsn, _S1, upgrade_from_pre_4_7) ->
    %% {state} = S1,
    S2 = #state{},
    {ok, S2};

code_change(_Vsn, State, _Extra) ->
    {ok, State}.


stop_backup_server(undefined) ->
    ok;
stop_backup_server({Pid, _}) when is_pid(Pid) ->
    exit(Pid, kill).



%%----------------------------------------------------------
%% Update system info
%%----------------------------------------------------------

%% handle_update_system_info(audit_trail_log_type = Key, Val) ->
%%     case snmpm_config:system_info(audit_trail_log) of
%% 	{ok, true} ->
%% 	    Value = 
%% 		case Val of
%% 		    read ->
%% 			{ok, [read]};
%% 		    write ->
%% 			{ok, [write]};
%% 		    read_write ->
%% 			{ok, [read,write]};
%% 		    _ ->
%% 			{error, {bad_value, Key, Val}}
%% 		end,
%% 	    case Value of
%% 		{ok, NewValue} ->
%% 		    ets:insert(snmpm_config_table, {Key, NewValue}),
%% 		    ok;
%% 		false ->
%% 		    Value
%% 	    end;
%% 	_ ->
%% 	    {error, audit_trail_log_not_enabled}
%%     end;
%% handle_update_system_info(BadKey, Val) ->
%%     {error, {unsupported_update, BadKey, Val}}.


%%----------------------------------------------------------
%% Backup
%%----------------------------------------------------------

handle_backup(D, BackupDir) ->
    %% First check that we do not wrote to the corrent db-dir...
    ?vtrace("handle_backup -> entry with"
        "~n   D:         ~p"
        "~n   BackupDir: ~p", [D, BackupDir]),
    case dets:info(D, filename) of
        undefined ->
            ?vinfo("handle_backup -> no file to backup", []),
            {error, no_file};
        Filename ->
            ?vinfo("handle_backup -> file to backup: ~n   ~p", [Filename]),
            case filename:dirname(Filename) of
                BackupDir ->
                    ?vinfo("handle_backup -> backup dir and db dir the same",
                           []),
                    {error, db_dir};
                _ ->
                    case file:read_file_info(BackupDir) of
                        {ok, #file_info{type = directory}} ->
                            ?vdebug("handle_backup -> backup dir ok", []),
                            %% All well so far...
                            Type = dets:info(D, type),
                            KP   = dets:info(D, keypos),
                            dets_backup(D,
                                        filename:basename(Filename),
                                        BackupDir, Type, KP);
                        {ok, _} ->
                            ?vinfo("handle_backup -> backup dir not a dir",
                                   []),
                            {error, not_a_directory};
                        Error ->
                            ?vinfo("handle_backup -> Error: ~p", [Error]),
                            Error
                    end
            end
    end.

dets_backup(D, Filename, BackupDir, Type, KP) ->
    ?vtrace("dets_backup -> entry with"
            "~n   D:         ~p"
            "~n   Filename:  ~p"
            "~n   BackupDir: ~p", [D, Filename, BackupDir]),
    BackupFile = filename:join(BackupDir, Filename),
    ?vtrace("dets_backup -> "
            "~n   BackupFile: ~p", [BackupFile]),
    Opts = [{file, BackupFile}, {type, Type}, {keypos, KP}],
    case dets:open_file(?BACKUP_DB, Opts) of
        {ok, B} ->
            ?vtrace("dets_backup -> create fun", []),
            F = fun(Arg) ->
                        dets_backup(Arg, start, D, B)
                end,
            dets:safe_fixtable(D, true),
            Res = dets:init_table(?BACKUP_DB, F, [{format, bchunk}]),
            dets:safe_fixtable(D, false),
            ?vtrace("dets_backup -> Res: ~p", [Res]),
            Res;
        Error ->
            ?vinfo("dets_backup -> open_file failed: "
                   "~n   ~p", [Error]),
            Error
    end.


dets_backup(close, _Cont, _D, B) ->
    dets:close(B),
    ok;
dets_backup(read, Cont1, D, B) ->
    case dets:bchunk(D, Cont1) of
        {Cont2, Data} ->
            F = fun(Arg) ->
                        dets_backup(Arg, Cont2, D, B)
                end,
            {Data, F};
        '$end_of_table' ->
            dets:close(B),
            end_of_input;
        Error ->
            Error
    end.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

handle_register_user(#user{id = Id} = User) ->
    ?vdebug("handle_register_user -> entry with"
	    "~n   User: ~p", [User]),
    case ets:lookup(snmpm_user_table, Id) of
	[] ->
	    ets:insert(snmpm_user_table, User),
	    ok;
	_ ->
	    {error, {already_registered, User}}
    end.

handle_unregister_user(UserId) ->
    ?vdebug("handle_unregister_user -> entry with"
	    "~n   UserId: ~p", [UserId]),
    ets:delete(snmpm_user_table, UserId),
    ok.


handle_register_agent(UserId, TargetName, Config) ->
    ?vdebug("handle_register_agent -> entry with"
	    "~n   UserId:     ~p"
	    "~n   TargetName: ~p"
	    "~n   Config:     ~p", [UserId, TargetName, Config]),
    case (catch agent_info(TargetName, user_id)) of
	{error, _} ->
	    case ets:lookup(snmpm_user_table, UserId) of
		[#user{default_agent_config = DefConfig}] ->
		    do_handle_register_agent(TargetName, DefConfig),
		    do_handle_register_agent(TargetName, 
					     [{user_id, UserId}|Config]),
		    %% <DIRTY-BACKWARD-COMPATIBILLITY>
		    %% And now for some (backward compatibillity) 
		    %% dirty crossref stuff
		    {ok, Addr} = agent_info(TargetName, address),
		    {ok, Port} = agent_info(TargetName, port),
		    ets:insert(snmpm_agent_table, 
			       {{Addr, Port, target_name}, TargetName}),
		    %% </DIRTY-BACKWARD-COMPATIBILLITY>
		    ok;
		_ ->
		    {error, {not_found, UserId}}
	    end;
	{ok, UserId} ->
	    ?vinfo("[~w] Agent (~p) already registered"
		   "~nwhen"
		   "~n   Agents: ~p", 
		   [UserId, TargetName, which_agents()]),
	    {error, {already_registered, TargetName}};
	{ok, OtherUserId} ->
	    ?vinfo("[~w] Agent (~p) already registered to ~p"
		   "~nwhen"
		   "~n   Agents: ~p", 
		   [UserId, TargetName, OtherUserId, which_agents()]),
	    {error, {already_registered, TargetName, OtherUserId}}
    end.

do_handle_register_agent(_TargetName, []) ->
    ok;
do_handle_register_agent(TargetName, [{Item, Val}|Rest]) ->
    case (catch do_update_agent_info(TargetName, Item, Val)) of
	ok ->
	    do_handle_register_agent(TargetName, Rest);
	{error, Reason} ->
	    ets:match_delete(snmpm_agent_table, {TargetName, '_'}),
	    {error, Reason}
    end;
do_handle_register_agent(TargetName, BadConfig) ->
    error_msg("error during agent registration - bad config: ~n~p", 
	      [BadConfig]),
    ets:match_delete(snmpm_agent_table, {TargetName, '_'}),
    {error, {bad_agent_config, TargetName, BadConfig}}.


handle_unregister_agent(UserId, TargetName) ->
    ?vdebug("handle_unregister_agent -> entry with"
	    "~n   UserId:     ~p"
	    "~n   TargetName: ~p", [UserId, TargetName]),
    case (catch agent_info(TargetName, user_id)) of
	{ok, UserId} ->
	    {ok, EngineID} = agent_info(TargetName, engine_id),
	    reset_usm_cache(EngineID),
	    %% <DIRTY-BACKWARD-COMPATIBILLITY>
	    %% And now for some (backward compatibillity) 
	    %% dirty crossref stuff
	    {ok, Addr} = agent_info(TargetName, address),
	    {ok, Port} = agent_info(TargetName, port),
	    ets:delete(snmpm_agent_table, {Addr, Port, target_name}),
	    %% </DIRTY-BACKWARD-COMPATIBILLITY>
	    ets:match_delete(snmpm_agent_table, {{TargetName, '_'}, '_'}),
	    ok;
	{ok, OtherUserId} ->
	    {error, {not_owner, OtherUserId}};
	Error ->
	    Error
    end.


handle_update_agent_info(UserId, TargetName, Item, Val) ->
    ?vdebug("handle_update_agent_info -> entry with"
	    "~n   UserId:     ~p"
	    "~n   TargetName: ~p"
	    "~n   Item:       ~p"
	    "~n   Val:        ~p", [UserId, TargetName, Item, Val]),
    case (catch agent_info(TargetName, user_id)) of
	{ok, UserId} ->
	    do_update_agent_info(TargetName, Item, Val);
	{ok, OtherUserId} ->
	    {error, {not_owner, OtherUserId}};
	Error ->
	    Error
    end.

do_update_agent_info(TargetName, Item, Val0) ->
%%     p("do_update_agent_info -> entry with"
%%       "~n   TargetName: ~p"
%%       "~n   Item:       ~p"
%%       "~n   Val0:       ~p", [TargetName, Item, Val0]),
    case verify_val(Item, Val0) of
	{ok, Val} ->
%% 	    p("do_update_agent_info -> verified value"
%% 	      "~n   Val: ~p", [Val]),
	    ets:insert(snmpm_agent_table, {{TargetName, Item}, Val}),
	    ok;
	Error ->
	    ?vlog("do_update_agent_info -> verify value failed: "
		  "~n   TargetName: ~p"
		  "~n   Item:       ~p"
		  "~n   Val0:       ~p"
		  "~n   Error:      ~p", [TargetName, Item, Val0, Error]),
	    {error, {bad_agent_val, TargetName, Item, Val0}}
    end.


handle_register_usm_user(#usm_user{engine_id = EngineID, 
				   name      = Name} = User) ->
    ?vdebug("handle_register_usm_user -> entry with"
	    "~n   User: ~p", [User]),
    Key = usm_key(EngineID, Name),
    case ets:lookup(snmpm_usm_table, Key) of
	[] ->
	    do_update_usm_user_info(Key, User);
	_ ->
	    {error, {already_registered, EngineID, Name}}
    end;
handle_register_usm_user(BadUsmUser) ->
    {error, {bad_usm_user, BadUsmUser}}.
	
handle_unregister_usm_user(EngineID, Name) ->
    ?vdebug("handle_unregister_usm_user -> entry with"
	    "~n   EngineID: ~p"
	    "~n   Name:     ~p", [EngineID, Name]),
    Key = usm_key(EngineID, Name),
    ets:delete(snmpm_usm_table, Key),
    ok.
	

handle_update_usm_user_info(EngineID, Name, Item, Val) ->
    ?vdebug("handle_update_usm_user_info -> entry with"
	    "~n   EngineID: ~p"
	    "~n   Name:     ~p"
	    "~n   Item:     ~p"
	    "~n   Val:      ~p", [EngineID, Name, Item, Val]),
    Key = usm_key(EngineID, Name),
    case ets:lookup(snmpm_usm_table, Key) of
	[] ->
	    {error, not_found};
	[{_Key, User}] ->
	    do_update_usm_user_info(Key, User, Item, Val)
    end.

do_update_usm_user_info(Key, User, sec_name, Val) ->
    %% case verify_usm_user_sec_name(Val) of
    %%     ok ->
    %% 	do_update_usm_user_info(Key, User#usm_user{sec_name = Val});
    %%     _ ->
    %% 	{error, {invalid_usm_sec_name, Val}}
    %% end;
    ok = verify_usm_user_sec_name(Val),
    do_update_usm_user_info(Key, User#usm_user{sec_name = Val});
do_update_usm_user_info(Key, User, auth, Val) 
  when (Val =:= usmNoAuthProtocol) orelse 
       (Val =:= usmHMACMD5AuthProtocol) orelse
       (Val =:= usmHMACSHAAuthProtocol) ->
    do_update_usm_user_info(Key, User#usm_user{auth = Val});
do_update_usm_user_info(_Key, _User, auth, Val) ->
    {error, {invalid_auth_protocol, Val}};
do_update_usm_user_info(Key, 
			#usm_user{auth = usmNoAuthProtocol} = User, 
			auth_key, Val) ->
    case (catch snmp_conf:check_string(Val, any)) of
	ok ->
	    do_update_usm_user_info(Key, User#usm_user{auth_key = Val});
	_ ->
	    {error, {invalid_auth_key, Val}}
    end;
do_update_usm_user_info(Key, 
			#usm_user{auth = usmHMACMD5AuthProtocol} = User, 
			auth_key, Val) 
  when length(Val) =:= 16 ->
    case is_crypto_supported(md5_mac_96) of
	true -> 
	    do_update_usm_user_info(Key, User#usm_user{auth_key = Val});
	false -> 
	    {error, {unsupported_crypto, md5_mac_96}}
    end;    
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACMD5AuthProtocol}, 
			auth_key, Val) when is_list(Val) ->
    Len = length(Val),
    {error, {invalid_auth_key_length, usmHMACMD5AuthProtocol, Len}};
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACMD5AuthProtocol}, 
			auth_key, Val) ->
    {error, {invalid_auth_key, usmHMACMD5AuthProtocol, Val}};
do_update_usm_user_info(Key, 
			#usm_user{auth = usmHMACSHAAuthProtocol} = User, 
			auth_key, Val) 
  when length(Val) =:= 20 ->
    case is_crypto_supported(sha_mac_96) of
	true -> 
	    do_update_usm_user_info(Key, User#usm_user{auth_key = Val});
	false -> 
	    {error, {unsupported_crypto, sha_mac_96}}
    end;    
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACSHAAuthProtocol}, 
			auth_key, Val) when is_list(Val) ->
    Len = length(Val),
    {error, {invalid_auth_key_length, usmHMACSHAAuthProtocol, Len}};
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACSHAAuthProtocol}, 
			auth_key, Val) ->
    {error, {invalid_auth_key, usmHMACSHAAuthProtocol, Val}};
do_update_usm_user_info(Key, User, priv, Val) 
  when (Val =:= usmNoPrivProtocol) orelse 
       (Val =:= usmDESPrivProtocol) orelse
       (Val =:= usmAesCfb128Protocol) ->
    do_update_usm_user_info(Key, User#usm_user{priv = Val});
do_update_usm_user_info(_Key, _User, priv, Val) ->
    {error, {invalid_priv_protocol, Val}};
do_update_usm_user_info(Key, 
			#usm_user{priv = usmNoPrivProtocol} = User, 
			priv_key, Val) ->
    case (catch snmp_conf:check_string(Val, any)) of
	ok ->
	    do_update_usm_user_info(Key, User#usm_user{priv_key = Val});
	_ ->
	    {error, {invalid_priv_key, Val}}
    end;
do_update_usm_user_info(Key, 
			#usm_user{priv = usmDESPrivProtocol} = User, 
			priv_key, Val) 
  when length(Val) =:= 16 ->
    case is_crypto_supported(des_cbc_decrypt) of
	true -> 
	    do_update_usm_user_info(Key, User#usm_user{priv_key = Val});
	false -> 
	    {error, {unsupported_crypto, des_cbc_decrypt}}
    end;    
do_update_usm_user_info(Key, 
			#usm_user{priv = usmAesCfb128Protocoll} = User, 
			priv_key, Val) 
  when length(Val) =:= 16 ->
    case is_crypto_supported(aes_cfb_128_decrypt) of
	true -> 
	    do_update_usm_user_info(Key, User#usm_user{priv_key = Val});
	false -> 
	    {error, {unsupported_crypto, aes_cfb_128_decrypt}}
    end;    
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACSHAAuthProtocol}, 
			priv_key, Val) when is_list(Val) ->
    Len = length(Val),
    {error, {invalid_priv_key_length, usmHMACSHAAuthProtocol, Len}};
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACSHAAuthProtocol}, 
			priv_key, Val) ->
    {error, {invalid_priv_key, usmHMACSHAAuthProtocol, Val}};
do_update_usm_user_info(_Key, _User, Item, Val) ->
    {error, {bad_item, Item, Val}}.

do_update_usm_user_info(Key, User) ->
    ets:insert(snmpm_usm_table, {Key, User}),
    ok.


usm_key(EngineId, Name) ->
    {usmUserTable, EngineId, Name}.


%% ---------------------------------------------------------------------

verify_mandatory(_, []) ->
    ok;
verify_mandatory(Conf, [Mand|Mands]) ->
    case lists:keymember(Mand, 1, Conf) of
	true ->
	    verify_mandatory(Conf, Mands);
	false ->
	    {error, {missing_mandatory_config, Mand}}
    end.

verify_invalid(_, []) ->
    ok;
verify_invalid(Conf, [Inv|Invs]) ->
    case lists:member(Inv, Conf) of
	false ->
	    verify_invalid(Conf, Invs);
	true ->
	    {error, {illegal_config, Inv}}
    end.


verify_val(user_id, UserId) ->
    {ok, UserId};
verify_val(reg_type, RegType) 
  when (RegType =:= addr_port) orelse (RegType =:= target_name) ->
    {ok, RegType};
verify_val(address, Addr0) ->
    case normalize_address(Addr0) of
	{_A1, _A2, _A3, _A4} = Addr ->
	    {ok, Addr};
	_ when is_list(Addr0) ->
	    case (catch snmp_conf:check_ip(Addr0)) of
		ok ->
		    {ok, list_to_tuple(Addr0)};
		Err ->
		    Err
	    end;
	_ ->
	    error({bad_address, Addr0})
    end;
verify_val(port, Port) ->
    case (catch snmp_conf:check_integer(Port, {gt, 0})) of
	ok ->
	    {ok, Port};
	Err ->
	    Err
    end;
verify_val(community, Comm) ->
    case (catch snmp_conf:check_string(Comm)) of
	ok ->
	    {ok, Comm};
	Err ->
	    Err
    end;
verify_val(engine_id, discovery = EngineId) ->
    {ok, EngineId};
verify_val(engine_id, EngineId) ->
    case (catch snmp_conf:check_string(EngineId)) of
	ok ->
	    {ok, EngineId};
	Err ->
	    Err
    end;
verify_val(timeout, Timeout) ->
    (catch snmp_conf:check_timer(Timeout));
verify_val(max_message_size, MMS) ->
    case (catch snmp_conf:check_packet_size(MMS)) of
	ok ->
	    {ok, MMS};
	Err ->
	    Err
    end;
verify_val(version, V) 
  when (V =:= v1) orelse (V =:= v2) orelse (V =:= v3) ->
    {ok, V};
verify_val(version, BadVersion) ->
    error({bad_version, BadVersion});
verify_val(sec_model, Model) ->
    (catch snmp_conf:check_sec_model(Model));
verify_val(sec_name, Name) when is_list(Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    {ok, Name};
	Err ->
	    Err
    end;
verify_val(sec_name, BadName) ->
    error({bad_sec_name, BadName});
verify_val(sec_level, Level) ->
    (catch snmp_conf:check_sec_level(Level));
verify_val(Item, _) ->
    {error, {no_such_item, Item}}.


%%%-------------------------------------------------------------------
%%%
%%% Mini MIB stuff
%%%
%%%-------------------------------------------------------------------

init_mini_mib(MibFiles) ->
    MiniMibs = lists:flatten([do_load_mib(MibFile) || MibFile <- MibFiles]),
    MiniMIB  = remove_duplicates(lists:keysort(1, MiniMibs), []),
    init_mini_mib2(MiniMIB).

remove_duplicates([], Res) -> 
    Res;
remove_duplicates([X,X|T], Res) -> 
    remove_duplicates([X|T], Res);
remove_duplicates([{Oid, Name, Type, _} = X, {Oid, Name, Type, _}|T], Res) -> 
    remove_duplicates([X|T], Res);
remove_duplicates([X|T], Res) -> 
    remove_duplicates(T, [X|Res]).

init_mini_mib2([]) ->
    ok;
init_mini_mib2([{Oid, Name, Type, MibName}|MiniMib]) ->
    ?vtrace("init mini mib -> ~w: ~w [~w] from ~s", 
	    [Name, Oid, Type,MibName ]),    
    ets:insert(snmpm_mib_table, {{mini_mib, Oid}, Name, Type, MibName}),
    init_mini_mib2(MiniMib).


handle_load_mib(Mib) ->
    [{mibs, Mibs0}] = ets:lookup(snmpm_config_table, mibs),
    case lists:member(Mib, Mibs0) of
	true ->
	    {error, already_loaded};
	false ->
	    Mibs = [Mib|Mibs0],
	    case (catch do_load_mib(Mib)) of
		MiniElems when is_list(MiniElems) ->
		    ets:insert(snmpm_config_table, {mibs, Mibs}),
		    update_mini_mib(MiniElems),
		    ok;
		Error ->
		    Error
	    end
    end.

update_mini_mib([]) ->
    ok;
update_mini_mib([{Oid, Name, Type, MibName}|Elems]) ->
    Key = {mini_mib, Oid},
    case ets:lookup(snmpm_mib_table, Key) of
	[{Key, _Name, _Type, _AnotherMibName}] ->
	    %% Already loaded from another mib
	    update_mini_mib(Elems);
	[] ->
	    %% Not yet loaded
	    ?vtrace("update mini mib -> ~w: ~w [~w] from ~s", 
		    [Name, Oid, Type, MibName]),    
	    ets:insert(snmpm_mib_table, {Key, Name, Type, MibName}),
	    update_mini_mib(Elems)
    end.


handle_unload_mib(Mib) ->
    Key = {mib, Mib},
    case ets:lookup(snmpm_mib_table, Key) of
	[{Key, MibName, _MibFile}] ->
	    do_unload_mib(MibName),
	    [{mibs, Mibs0}] = ets:lookup(snmpm_config_table, mibs),
	    Mibs = lists:delete(Mib, Mibs0),
	    ets:insert(snmpm_config_table, {mibs, Mibs}),
	    ets:delete(snmpm_mib_table, Key),
	    ok;
	_ ->
	    {error, not_loaded}
    end.

do_unload_mib(MibName) ->
    Pat  = {{mini_mib, '$1'}, '_', '_', MibName},
    Oids = ets:match(snmpm_mib_table, Pat),
    F    = fun([Oid]) -> ets:delete(snmpm_mib_table, {mini_mib, Oid}) end,
    lists:foreach(F, Oids).
    

do_load_mib(MibFile) ->
    ?vtrace("load mib ~s", [MibFile]),
    F1 = snmp_misc:strip_extension_from_filename(MibFile, ".bin"),
    ActualFileName = lists:append(F1, ".bin"),
    case snmp_misc:read_mib(ActualFileName) of
        {ok, #mib{name = Name, mes = MEs, traps = Traps}} -> 
	    %% Check that the mib was not loaded or loaded
	    %% with a different filename: 
	    %% e.g. /tmp/MYMIB.bin and /tmp/mibs/MYMIB.bin
	    Name1   = mib_name(Name),
	    Pattern = {{mib, '_'}, Name1, '$1'},
	    case ets:match(snmpm_mib_table, Pattern) of
		[] ->
		    
		    Rec = {{mib, MibFile}, Name1, ActualFileName}, 
		    ets:insert(snmpm_mib_table, Rec),
		    init_mini_mib_elems(Name1, MEs++Traps, []);

		%% This means that the mib has already been loaded
		[[ActualFileName]] ->
		    [];

		%% This means that the mib was loaded before,
		%% but under another filename
		[[OtherMibFile]] ->
		    error({already_loaded, MibFile, OtherMibFile})
	    end;
		
        {error, Reason} -> 
	    error({failed_reading_mib, MibFile, Reason})
    end.

mib_name(N) when is_list(N) ->
    list_to_atom(N);
mib_name(N) ->
    N.

init_mini_mib_elems(_, [], Res) -> 
    Res;
init_mini_mib_elems(MibName, 
		    [#me{aliasname = N, 
			 oid       = Oid, 
			 entrytype = variable,
			 asn1_type = #asn1_type{bertype = Type}} | T], Res) ->
    init_mini_mib_elems(MibName, T, [{Oid, N, Type, MibName}|Res]);

init_mini_mib_elems(MibName, 
		    [#me{aliasname = N, 
			 oid       = Oid, 
			 entrytype = table_column,
			 asn1_type = #asn1_type{bertype = Type}}|T], Res) ->
    init_mini_mib_elems(MibName, T, [{Oid, N, Type, MibName}|Res]);

init_mini_mib_elems(MibName, 
		    [#me{aliasname = N, 
			 oid       = Oid,
			 asn1_type = undefined}|T], Res) ->
    init_mini_mib_elems(MibName, T, [{Oid, N, undefined, MibName}|Res]);

init_mini_mib_elems(MibName, 
		    [#notification{trapname = N, 
				   oid      = Oid}|T], Res) ->
    init_mini_mib_elems(MibName, T, [{Oid, N, undefined, MibName}|Res]);

init_mini_mib_elems(MibName, [_|T], Res) ->
    init_mini_mib_elems(MibName, T, Res).



%%----------------------------------------------------------------------

normalize_address(Addr) ->
    case inet:getaddr(Addr, inet) of
        {ok, Addr2} ->
            Addr2;
        _ when is_list(Addr) ->
	    case (catch snmp_conf:check_ip(Addr)) of
		ok ->
		    list_to_tuple(Addr);
		_ ->
		    Addr
	    end;
	_ ->
            Addr
    end.


%%----------------------------------------------------------------------

call(Req) ->
    call(Req, infinity).

call(Req, To) ->
    gen_server:call(?SERVER, Req, To).

% cast(Msg) ->
%     gen_server:cast(snmpm_server, Msg).


%%-------------------------------------------------------------------

get_atl_dir(Opts) ->
    get_opt(dir, Opts).

get_atl_type(Opts) ->
    case get_opt(type, Opts, read_write) of
	read_write ->
	    [read,write];
	read ->
	    [read];
	write ->
	    [write]
    end.

get_atl_size(Opts) ->
    get_opt(size, Opts).

get_atl_repair(Opts) ->
    get_opt(repair, Opts, truncate).

get_atl_seqno(Opts) ->
    get_opt(seqno, Opts, false).


%%----------------------------------------------------------------------

get_opt(Key, Opts) ->
    ?d("get option ~w from ~p", [Key, Opts]),
    snmp_misc:get_option(Key, Opts).

get_opt(Key, Opts, Def) ->
    ?d("get option ~w with default ~p from ~p", [Key, Def, Opts]),
    snmp_misc:get_option(Key, Opts, Def).


%%----------------------------------------------------------------------

get_info() ->
    ProcSize = proc_mem(self()),
    CntSz    = tab_size(snmpm_counter_table),
    StatsSz  = tab_size(snmpm_stats_table),
    MibSz    = tab_size(snmpm_mib_table),
    ConfSz   = tab_size(snmpm_config_table),
    AgentSz  = tab_size(snmpm_agent_table),
    UserSz   = tab_size(snmpm_user_table),
    UsmSz    = tab_size(snmpm_usm_table),
    [{process_memory, ProcSize},
     {db_memory, [{counter, CntSz}, 
		  {stats,   StatsSz},
		  {mib,     MibSz}, 
		  {config,  ConfSz},
		  {agent,   AgentSz}, 
		  {user,    UserSz}, 
		  {usm,     UsmSz}]}].

proc_mem(P) when is_pid(P) ->
    case (catch erlang:process_info(P, memory)) of
	{memory, Sz} when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end.
%% proc_mem(_) ->
%%     undefined.

tab_size(T) ->
    case (catch ets:info(T, memory)) of
	Sz when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end.


%%----------------------------------------------------------------------

error(Reason) ->
    throw({error, Reason}).


%%----------------------------------------------------------------------

info_msg(F, A) ->
    ?snmpm_info("Config server: " ++ F, A).

warning_msg(F, A) ->
    ?snmpm_warning("Config server: " ++ F, A).

error_msg(F, A) ->
    ?snmpm_error("Config server: " ++ F, A).

%% p(F) ->
%%     p(F, []).

%% p(F, A) ->
%%     io:format("~w:" ++ F ++ "~n", [?MODULE | A]).

