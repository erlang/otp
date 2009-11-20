%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
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

-module(snmpm_conf).

-include_lib("kernel/include/file.hrl").

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
  when is_list(Dir) andalso is_list(Hdr) andalso is_list(Conf) ->
    Verify = fun()    -> verify_manager_conf(Conf)           end,
    Write  = fun(Fid) -> write_manager_conf(Fid, Hdr, Conf)  end,
    write_config_file(Dir, ?MANAGER_CONF_FILE, Verify, Write).
    

append_manager_config(Dir, Conf) 
  when is_list(Dir) andalso is_list(Conf) ->
    Verify = fun()    -> verify_manager_conf(Conf)     end,
    Write  = fun(Fid) -> write_manager_conf(Fid, Conf) end,
    append_config_file(Dir, ?MANAGER_CONF_FILE, Verify, Write).
    

read_manager_config(Dir) ->
    Verify = fun(Entry) -> verify_manager_conf_entry(Entry) end,
    read_config_file(Dir, ?MANAGER_CONF_FILE, Verify).


verify_manager_conf([]) ->
    ok;
verify_manager_conf([H|T]) ->
    verify_manager_conf_entry(H),
    verify_manager_conf(T);
verify_manager_conf(X) ->
    error({bad_manager_config, X}).

verify_manager_conf_entry(Entry) ->
    case snmpm_config:check_manager_config(Entry) of
        ok ->
            ok;
%%         {ok, _} ->
%%             ok;
	Error ->
	    throw(Error)
    end.

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

do_write_manager_conf(Fd, {address = Tag, Val}) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_manager_conf(Fd, {port = Tag, Val} ) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
do_write_manager_conf(Fd, {engine_id = Tag, Val} ) ->
    io:format(Fd, "{~w, \"~s\"}.~n", [Tag, Val]);
do_write_manager_conf(Fd, {max_message_size = Tag, Val} ) ->
    io:format(Fd, "{~w, ~w}.~n", [Tag, Val]);
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
    Verify = fun()   -> verify_users_conf(Conf)         end,
    Write  = fun(Fd) -> write_users_conf(Fd, Hdr, Conf) end,
    write_config_file(Dir, ?USERS_CONF_FILE, Verify, Write).


append_users_config(Dir, Conf) 
  when is_list(Dir) andalso is_list(Conf) ->
    Verify = fun()   -> verify_users_conf(Conf)    end,
    Write  = fun(Fd) -> write_users_conf(Fd, Conf) end,
    append_config_file(Dir, ?USERS_CONF_FILE, Verify, Write).


read_users_config(Dir) when is_list(Dir) ->
    Verify = fun(Entry) -> verify_users_conf_entry(Entry) end,
    read_config_file(Dir, ?USERS_CONF_FILE, Verify).

    
verify_users_conf([]) ->
    ok;
verify_users_conf([H|T]) ->
    verify_users_conf_entry(H),
    verify_users_conf(T);
verify_users_conf(X) ->
    error({bad_users_conf, X}).

verify_users_conf_entry(Entry) ->
    {ok, _} = snmpm_config:check_user_config(Entry),
    ok.

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

agents_entry(UserId, TargetName, Comm, Ip, Port, EngineID, Timeout, 
	     MaxMessageSize, Version, SecModel, SecName, SecLevel) ->
    {UserId, TargetName, Comm, Ip, Port, EngineID, Timeout, 
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
    Verify = fun()   -> verify_agents_conf(Conf)         end,
    Write  = fun(Fd) -> write_agents_conf(Fd, Hdr, Conf) end,
    write_config_file(Dir, ?AGENTS_CONF_FILE, Verify, Write).


append_agents_config(Dir, Conf) 
  when is_list(Dir) andalso is_list(Conf) ->
    Verify = fun()   -> verify_agents_conf(Conf)    end,
    Write  = fun(Fd) -> write_agents_conf(Fd, Conf) end,
    append_config_file(Dir, ?AGENTS_CONF_FILE, Verify, Write).


read_agents_config(Dir) ->
    Verify = fun(Entry) -> verify_agents_conf_entry(Entry) end,
    read_config_file(Dir, ?AGENTS_CONF_FILE, Verify).


verify_agents_conf([]) ->
    ok;
verify_agents_conf([H|T]) ->
    verify_agents_conf_entry(H),
    verify_agents_conf(T);
verify_agents_conf(X) ->
    error({bad_agents_config, X}).

verify_agents_conf_entry(Entry) ->
    {ok, _} = snmpm_config:check_agent_config(Entry),
    ok.

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

do_write_agents_conf(Fd, 
		    {UserId, 
		     TargetName, Comm, Ip, Port, EngineID, 
		     Timeout, MaxMessageSize, Version, 
		     SecModel, SecName, SecLevel} = _A) ->
    io:format(Fd, 
	      "{~w, \"~s\", \"~s\", ~w, ~w, \"~s\", ~w, ~w, ~w, ~w, \"~s\", ~w}.~n", [UserId, TargetName, Comm, Ip, Port, EngineID, Timeout, MaxMessageSize, Version, SecModel, SecName, SecLevel]);
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
    Verify = fun()   -> verify_usm_conf(Conf)         end,
    Write  = fun(Fd) -> write_usm_conf(Fd, Hdr, Conf) end,
    write_config_file(Dir, ?USM_USERS_CONF_FILE, Verify, Write).


append_usm_config(Dir, Conf) 
  when is_list(Dir) andalso is_list(Conf) ->
    Verify = fun()   -> verify_usm_conf(Conf)    end,
    Write  = fun(Fd) -> write_usm_conf(Fd, Conf) end,
    append_config_file(Dir, ?USM_USERS_CONF_FILE, Verify, Write).


read_usm_config(Dir) 
  when is_list(Dir) ->
    Verify = fun(Entry) -> verify_usm_conf_entry(Entry) end,
    read_config_file(Dir, ?USM_USERS_CONF_FILE, Verify).


verify_usm_conf([]) ->
    ok;
verify_usm_conf([H|T]) ->
    verify_usm_conf_entry(H),
    verify_usm_conf(T);
verify_usm_conf(X) ->
    error({bad_usm_conf, X}).

verify_usm_conf_entry(Entry) ->
    {ok, _} = snmpm_config:check_usm_user_config(Entry),
    ok.

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

do_write_usm_conf(Fd, 
		  {EngineID, UserName, AuthP, AuthKey, PrivP, PrivKey}) ->
    io:format(Fd, "{\"~s\", \"~s\", ~w, ~w, ~w, ~w}.~n", 
	      [EngineID, UserName, AuthP, AuthKey, PrivP, PrivKey]);
do_write_usm_conf(Fd, 
		  {EngineID, UserName, SecName, 
		   AuthP, AuthKey, PrivP, PrivKey}) ->
    io:format(Fd, "{\"~s\", \"~s\", \"~s\", í~w, ~w, ~w, ~w}.~n", 
	      [EngineID, UserName, SecName, AuthP, AuthKey, PrivP, PrivKey]);
do_write_usm_conf(_Fd, Crap) ->
    error({bad_usm_conf, Crap}).


%% ---- config file wrapper functions ----

write_config_file(Dir, File, Verify, Write) ->
    snmp_config:write_config_file(Dir, File, Verify, Write).

append_config_file(Dir, File, Verify, Write) ->
    snmp_config:append_config_file(Dir, File, Verify, Write).

read_config_file(Dir, File, Verify) ->
    snmp_config:read_config_file(Dir, File, Verify).


%% ---- config file utility functions ----

header() ->
    {Y,Mo,D} = date(),
    {H,Mi,S} = time(),
    io_lib:format("%% This file was generated by "
		  "~w (version-~s) ~w-~2.2.0w-~2.2.0w "
		  "~2.2.0w:~2.2.0w:~2.2.0w\n",
		  [?MODULE, ?version, Y, Mo, D, H, Mi, S]).


error(R) ->
    throw({error, R}).
