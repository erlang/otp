%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2024. All Rights Reserved.
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
%%
-module(mod_security).
-moduledoc """
Security Audit and Trailing Functionality

Security Audit and Trailing Functionality
""".
-moduledoc(#{titles => [{callback,<<"SecurityCallbackModule">>}]}).

%% Security Audit Functionality

%% User API exports
-export([list_blocked_users/1, list_blocked_users/2, list_blocked_users/3, 
	 block_user/4, block_user/5, 
	 unblock_user/2, unblock_user/3, unblock_user/4,
	 list_auth_users/1, list_auth_users/2, list_auth_users/3]).

%% module API exports
-export([do/1, store/2, remove/1]).

-include("httpd.hrl").
-include("httpd_internal.hrl").

-define(VMODULE,"SEC").


%%====================================================================
%% Internal application API
%%====================================================================
-doc(#{title => <<"SecurityCallbackModule">>,
       equiv => {callback,event,5},
       since => <<"OTP 18.1">>}).
-callback event(What, Port, Dir, Data) -> term() when
      What :: auth_fail | user_block | user_unblock,
      Port :: integer(),
      Dir :: string(),
      Data :: [Info],
      Info :: {Name :: term(), Value :: term()}.
-doc """
[](){: #callback_module_event }

[`event/4`](`c:event/4`) or [`event/5`](`c:event/5`) is called whenever an event
occurs in the `mod_security` Erlang web server API module.
([`event/4`](`c:event/4`) is called if `Address` is undefined, otherwise
[`event/5`](`c:event/5`). Argument `What` specifies the type of event that has
occurred and is one of the following reasons:

- **`auth_fail`** - A failed user authentication.

- **`user_block`** - A user is being blocked from access.

- **`user_unblock`** - A user is being removed from the block list.

> #### Note {: .info }
>
> The event `user_unblock` is not triggered when a user is removed from the
> block list explicitly using the `unblock_user` function.
""".
-doc(#{title => <<"SecurityCallbackModule">>,since => <<"OTP 18.1">>}).
-callback event(What, Address, Port, Dir, Data) -> term() when
      What :: auth_fail | user_block | user_unblock,
      Port :: integer(),
      Address :: inet:ip4_address() | string(),
      Dir :: string(),
      Data :: [Info],
      Info :: {Name :: term(), Value :: term()}.

%%====================================================================
%% Internal application API
%%====================================================================
-doc false.
do(Info) ->
    %% Check and see if any user has been authorized.
    case proplists:get_value(remote_user, Info#mod.data,not_defined_user) of
	not_defined_user ->
	    %% No user has been authorized.
	    case proplists:get_value(response, Info#mod.data) of
		%% A status code has been generated!
		{401, _Response} ->
		    case proplists:get_value("authorization",
					     Info#mod.parsed_header) of
			undefined ->
			    %% Not an authorization attempt (server
			    %% just replied to challenge for
			    %% authentication)
			    {proceed, Info#mod.data};
			[$B,$a,$s,$i,$c,$ |EncodedString] ->
			    %% Someone tried to authenticate, and
			    %% obviously failed!
			    DecodedString =  
				case (catch 
					  base64:decode_to_string(
					    EncodedString)) of
				    %% Decode failed 
				    {'EXIT',{function_clause, _}} ->
					EncodedString;
				    String ->
					String
				end,
				 
			    report_failed(Info, DecodedString,
					  "Failed authentication"),
			    take_failed_action(Info, DecodedString),
			    {proceed, Info#mod.data}
		    end;
		_ ->
		    {proceed, Info#mod.data}
	    end;
	User ->
	    %% A user has been authenticated, now is he blocked ?
	    Path = mod_alias:path(Info#mod.data,
				  Info#mod.config_db,
				  Info#mod.request_uri),
	    {_Dir, SDirData} = secretp(Path, Info#mod.config_db),
	    Addr = httpd_util:lookup(Info#mod.config_db, bind_address),
	    Port = httpd_util:lookup(Info#mod.config_db, port),
	    Profile = httpd_util:lookup(Info#mod.config_db, profile, ?DEFAULT_PROFILE),
	    case mod_security_server:check_blocked_user(Info, User, 
							SDirData, 
							Addr, Port, Profile) of
		true ->
		    report_failed(Info, User ,"User Blocked"),
		    {proceed, [{status, {403, Info#mod.request_uri, ""}} |
			       Info#mod.data]};
		false ->
		    report_failed(Info, User,"Authentication Succeeded"),
		    mod_security_server:store_successful_auth(Addr, Port, Profile, 
							      User, 
							      SDirData),
		    {proceed, Info#mod.data}
	    end
    end.

-doc false.
store({security_directory, {Dir, DirData}}, ConfigList) 
  when is_list(Dir) andalso is_list(DirData) ->
    Addr = proplists:get_value(bind_address, ConfigList),
    Port = proplists:get_value(port, ConfigList),
    Profile = proplists:get_value(profile, ConfigList, ?DEFAULT_PROFILE),
    mod_security_server:start(Addr, Port, Profile),
    SR = proplists:get_value(server_root, ConfigList),
    case proplists:get_value(data_file, DirData, no_data_file) of
	no_data_file ->
	    {error, {missing_security_data_file, {security_directory, {Dir, DirData}}}};
	DataFile0 ->
	    DataFile = 
		case filename:pathtype(DataFile0) of
		    relative ->
			filename:join(SR, DataFile0);
		    _ ->
			DataFile0
		end,
	    case mod_security_server:new_table(Addr, Port, Profile, DataFile) of
		{ok, TwoTables} ->
		    NewDirData0 = lists:keyreplace(data_file, 1, DirData, 
						   {data_file, TwoTables}),
		    NewDirData1 = case Addr of
				      undefined ->
					  [{port,Port}|NewDirData0];
				      _ ->
					  [{port,Port},{bind_address,Addr}|
					   NewDirData0]
				  end,
		    {ok, {security_directory, {Dir, NewDirData1}}};
		{error, Err} ->
		    {error, {{open_data_file, DataFile}, Err}}
	    end
    end;
store({directory, {Directory, DirData}}, _) ->
    {error, {wrong_type, {security_directory, {Directory, DirData}}}}.

-doc false.
remove(ConfigDB) ->
    Addr = httpd_util:lookup(ConfigDB, bind_address, undefined),
    Port = httpd_util:lookup(ConfigDB, port),
    Profile = httpd_util:lookup(ConfigDB, profile, ?DEFAULT_PROFILE),
    mod_security_server:delete_tables(Addr, Port, Profile),
    mod_security_server:stop(Addr, Port, Profile).
    

-doc(#{equiv => list_blocked_users/3}).
list_blocked_users(Port) ->
    list_blocked_users(undefined, Port).

-doc(#{equiv => list_blocked_users/3}).
list_blocked_users(Port, Dir) when is_integer(Port) ->
    list_blocked_users(undefined,Port,Dir);
list_blocked_users(Addr, Port) when is_integer(Port) ->
    lists:map(fun({User, Addr0, Port0, ?DEFAULT_PROFILE, Dir0, Time}) ->
		      {User, Addr0, Port0, Dir0,Time}
	      end,
	      mod_security_server:list_blocked_users(Addr, Port)).

-doc """
list_blocked_users(Address, Port, Dir) -> Users | []

[`list_blocked_users/1`](`list_blocked_users/1`),
[`list_blocked_users/2`](`list_blocked_users/2`), and
[`list_blocked_users/3`](`list_blocked_users/3`) each returns a list of users
that are currently blocked from access.
""".
list_blocked_users(Addr, Port, Dir) ->
    lists:map(fun({User, Addr0, Port0, ?DEFAULT_PROFILE, Dir0, Time}) ->
		      {User, Addr0, Port0, Dir0,Time}
	      end,
	      mod_security_server:list_blocked_users(Addr, Port, Dir)).

-doc(#{equiv => block_user/5}).
block_user(User, Port, Dir, Time) ->
    block_user(User, undefined, Port, Dir, Time).
-doc """
block_user(User, Address, Port, Dir, Seconds) -> true | {error, Reason}

[`block_user/4`](`block_user/4`) and [`block_user/5`](`block_user/5`) each
blocks the user `User` from directory `Dir` for a specified amount of time.
""".
block_user(User, Addr, Port, Dir, Time) ->
    mod_security_server:block_user(User, Addr, Port, Dir, Time).

-doc(#{equiv => unblock_user/4}).
unblock_user(User, Port) ->
    unblock_user(User, undefined, Port).

-doc(#{equiv => unblock_user/4}).
unblock_user(User, Port, Dir) when is_integer(Port) ->
    unblock_user(User, undefined, Port, Dir);
unblock_user(User, Addr, Port) when is_integer(Port) ->
    mod_security_server:unblock_user(User, Addr, Port).

-doc """
unblock_user(User, Address, Port, Dir) -> true | {error, Reason}

[`unblock_user/2`](`unblock_user/2`), [`unblock_user/3`](`unblock_user/3`), and
[`unblock_user/4`](`unblock_user/4`) each removes the user `User` from the list
of blocked users for `Port` (and `Dir`).
""".
unblock_user(User, Addr, Port, Dir) ->
    mod_security_server:unblock_user(User, Addr, Port, Dir).

-doc(#{equiv => list_auth_users/3}).
list_auth_users(Port) ->
    list_auth_users(undefined,Port).

-doc(#{equiv => list_auth_users/3}).
list_auth_users(Port, Dir) when is_integer(Port) ->
    list_auth_users(undefined, Port, Dir);
list_auth_users(Addr, Port) when is_integer(Port) ->
    mod_security_server:list_auth_users(Addr, Port).

-doc """
list_auth_users(Address, Port, Dir) -> Users | []

[`list_auth_users/1`](`list_auth_users/1`),
[`list_auth_users/2`](`list_auth_users/2`), and
[`list_auth_users/3`](`list_auth_users/3`) each returns a list of users that are
currently authenticated. Authentications are stored for `SecurityAuthTimeout`
seconds, and then discarded.
""".
list_auth_users(Addr, Port, Dir) ->
    mod_security_server:list_auth_users(Addr, Port, Dir).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

report_failed(Info, Auth, Event) ->
    Request = Info#mod.request_line,
    {_PortNumber,RemoteHost}=(Info#mod.init_data)#init_data.peername,
    String = RemoteHost ++ " : " ++ Event ++ " : " ++ Request ++ 
	" : " ++ Auth,
    mod_disk_log:security_log(Info,String),
    mod_log:security_log(Info, String).

take_failed_action(Info, Auth) ->
    Path = mod_alias:path(Info#mod.data, Info#mod.config_db, 
			  Info#mod.request_uri),
    {_Dir, SDirData} = secretp(Path, Info#mod.config_db),
    Addr = httpd_util:lookup(Info#mod.config_db, bind_address),
    Port = httpd_util:lookup(Info#mod.config_db, port),
    Profile = httpd_util:lookup(Info#mod.config_db, profile, ?DEFAULT_PROFILE),
    mod_security_server:store_failed_auth(Info, Addr, Port, Profile, 
					  Auth, SDirData).

secretp(Path, ConfigDB) ->
    Directories = ets:match(ConfigDB,{directory,{'$1','_'}}),
    case secret_path(Path, Directories) of
	{yes, Directory} ->
	    SDirs0 = httpd_util:multi_lookup(ConfigDB, security_directory),
	    [SDir] = lists:filter(fun({Directory0, _}) 
				     when Directory0 == Directory ->
					  true;
				     (_) ->
					  false
				  end, SDirs0),
	    SDir;
	no ->
	    {[], []}
    end.

secret_path(Path,Directories) ->
    secret_path(Path, httpd_util:uniq(lists:sort(Directories)), to_be_found).

secret_path(_Path, [], to_be_found) ->
    no;
secret_path(_Path, [], Dir) ->
    {yes, Dir};
secret_path(Path, [[NewDir]|Rest], Dir) ->
    case re:run(Path, NewDir, [{capture, first}]) of
	{match, _} when Dir =:= to_be_found ->
	    secret_path(Path, Rest, NewDir);
	{match, [{_, Length}]} when Length > length(Dir) ->
	    secret_path(Path, Rest, NewDir);
	{match, _} ->
	    secret_path(Path, Rest, Dir);
	nomatch ->
	    secret_path(Path, Rest, Dir)
    end.


